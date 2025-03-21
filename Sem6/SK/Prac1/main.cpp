
#include <bits/stdc++.h>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <unistd.h>
#include <poll.h>

using namespace std;

#define DEBUG 0     // debug stage; 0 = none, verbose = 3, minimal >= 4
#define dprintf if(DEBUG) printf

constexpr int MAX_TTL = 30;
constexpr int WAIT_TIME_MS = 1000;
constexpr int PACKETS_PER_TTL = 3;


//* handle errors - write them and exit
void ERROR(const char* str)
{
    fprintf(stderr, "%s: %s\n", str, strerror(errno));
    exit(EXIT_FAILURE);
}

/* //? #region --- HELPER --- */
class Timer {
public:
    chrono::time_point<chrono::high_resolution_clock> start_time;
    Timer() : start_time(chrono::high_resolution_clock::now()) {}

    void reset() {
        start_time = chrono::high_resolution_clock::now();
    }

    int64_t elapsed() {
        return chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now() - start_time).count();
    }

    int64_t now() {
        return chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
    double now_precise() {
        return chrono::duration<double, milli>(chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
};

// hash for pair
namespace std {
    template <typename T, typename U>
    struct hash<pair<T, U>> {
        size_t operator()(const pair<T, U>& p) const {
            return hash<T>()(p.first) ^ hash<U>()(p.second);
        }
    };
}
/*//? #endregion */


class Debug {
    Debug() = delete;
public:
    static void print_as_bytes (unsigned char* buff, ssize_t length)
    {
        for (ssize_t i = 0; i < length; i++, buff++)
            dprintf("%.2x ", *buff);
    }
    static void print_icmp_header (unsigned char* header)
    {
        struct icmp* icmp_header = (struct icmp*) header;
        dprintf("Type: %d\n", icmp_header->icmp_type);
        dprintf("Code: %d\n", icmp_header->icmp_code);
        dprintf("Checksum: %d\n", icmp_header->icmp_cksum);
        dprintf("ID: %d\n", icmp_header->icmp_hun.ih_idseq.icd_id);
        dprintf("Sequence: %d\n", icmp_header->icmp_hun.ih_idseq.icd_seq);
    }
    static void print_ip_header (unsigned char* header, bool verbose=true)
    {
        struct ip* ip_header = (struct ip*) header;
        if(verbose) {
            dprintf("Version: %d\n", ip_header->ip_v);
            dprintf("Header length: %d\n", ip_header->ip_hl);
            dprintf("Type of service: %d\n", ip_header->ip_tos);
            dprintf("Total length: %d\n", ip_header->ip_len);
            dprintf("Identification: %d\n", ip_header->ip_id);
            dprintf("Fragment offset: %d\n", ip_header->ip_off);
            dprintf("Time to live: %d\n", ip_header->ip_ttl);
            dprintf("Protocol: %d\n", ip_header->ip_p);
            dprintf("Checksum: %d\n", ip_header->ip_sum);
        }
        dprintf("Source: %s\n", inet_ntoa(ip_header->ip_src));
        dprintf("Destination: %s\n", inet_ntoa(ip_header->ip_dst));
    }

    //* debug/sandbox playing with response
    static void decompose_response_timeout(
        unsigned char* ip_header_start,
        int p_ip_header=1,
        bool p_icmp_header=true,
        bool p_original_ip_header=true
    ) {
    struct ip* ip_header = (struct ip*) ip_header_start;
    ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);

    // received entire packet header, usually first 20 bytes
    if(p_ip_header) {
        dprintf("IP header:\n");
        Debug::print_ip_header(ip_header_start, p_ip_header - 1);
        dprintf("\n");
    }

    // received entire packet data, after the header
    // in our case, if its icmp header, there will be info about our ping (reply or ttl exceeded)
    unsigned char *ip_data_start = ip_header_start + ip_header_len;
    if(p_icmp_header) {
        dprintf("IP data / ICMP header:\n");
        Debug::print_icmp_header(ip_data_start);
        dprintf("\n");
    }

    /*
    since it is icmp response, there are 2 cases:
        either our original ping reached host -
        - in this case the icmp header contains the original ip header
        
        or it didnt and we got ttl exceeded - 
        - in this case the router sends us the original ip header 
        (or at least as much as managed to fit) after the icmp header
    */
    unsigned char *original_ip_header_start = ip_data_start + sizeof(struct icmp);
    if(p_original_ip_header) {
        dprintf("IP header from IP data / original ICMP header:\n");
        Debug::print_icmp_header(original_ip_header_start);
        dprintf("\n\n");
    }
}
};


class UidHelper {
    UidHelper() = delete;
public:
    static inline int gen_uid(int ttl, int seq) {
        return ttl * PACKETS_PER_TTL + seq;
        // return ttl;
    }
    static inline int get_ttl(int uid) {
        return uid / PACKETS_PER_TTL;
        // return uid;
    }
    static inline int get_ttl(struct icmp icmp_header) {
        return get_ttl(icmp_header.icmp_hun.ih_idseq.icd_seq);
    }
    static inline int get_seq(int uid) {
        return uid % PACKETS_PER_TTL;
        // return 0;
    }
    static inline int get_seq(struct icmp icmp_header) {
    return get_seq(icmp_header.icmp_hun.ih_idseq.icd_seq);
}
};

class IcmpIpHelper {
    IcmpIpHelper() = delete;
public:
    //* compute checksum for icmp packet
    static u_int16_t compute_icmp_checksum(const void *buff, int length)
    {
        const u_int16_t* ptr = reinterpret_cast<const uint16_t*>(buff);
        u_int32_t sum = 0;
        assert (length % 2 == 0);
        for (; length > 0; length -= 2)
            sum += *ptr++;
        sum = (sum >> 16U) + (sum & 0xffffU);
        return (u_int16_t)(~(sum + (sum >> 16U)));
    }
    //* get sent icmp struct from received ip header
    static inline struct icmp get_icmp_header(unsigned char* ip_header_start) {
        struct ip* ip_header = (struct ip*) ip_header_start;
        const ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);
        unsigned char *ip_data_start = ip_header_start + ip_header_len;
        return *(struct icmp*) ip_data_start;
    }
    //* get sent icmp struct from received ip header
    static inline struct icmp get_sent_icmp_header(unsigned char* ip_header_start) {
    struct ip* ip_header = (struct ip*) ip_header_start;
    const ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);
    unsigned char *ip_data_start = ip_header_start + ip_header_len;
    unsigned char *original_ip_header_start = ip_data_start + sizeof(struct icmp);
    return *(struct icmp*) original_ip_header_start;
}
};

class IcmpPackets {
private:
    IcmpPackets() = delete;
public:
    //* send icmp echo packet
    static void icmp_send(int sock_fd, struct sockaddr_in &recipient, int ttl, int seq) {
        // generate header
        struct icmp header;
        header.icmp_type = ICMP_ECHO;
        header.icmp_code = 0;
        header.icmp_hun.ih_idseq.icd_id = getpid();
        header.icmp_hun.ih_idseq.icd_seq = UidHelper::gen_uid(ttl, seq);
        header.icmp_cksum = 0;  // we have to set it to 0 in order to computer correct chksum
        header.icmp_cksum = IcmpIpHelper::compute_icmp_checksum ((u_int16_t*)&header, sizeof(header));

        // set ttl in socket
        if (setsockopt(sock_fd,
                    IPPROTO_IP,
                    IP_TTL,
                    &ttl,
                    sizeof(int)) < 0) {
            ERROR("setsockopt error");
        }

        // send packet
        if (sendto(sock_fd,
                &header,
                sizeof(header),
                0,
                (struct sockaddr*)&recipient,
                sizeof(recipient)) < 0) {
            ERROR("sendto error");
        }

        if(DEBUG == 1 || DEBUG == 3) {
            dprintf("Sent ICMP echo packet with TTL = %d\n", ttl);
            dprintf("ICMP header:\n");
            Debug::print_icmp_header((unsigned char*)&header);
            dprintf("\n");
        }
    }

    //* receive icmp echo packet, return if received
    static bool icmp_receive(int sock_fd, string &ip, int &ttl, int &seq) {
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    u_int8_t buffer[IP_MAXPACKET];

    //? https://pubs.opengroup.org/onlinepubs/7908799/xsh/poll.html
    struct pollfd fds = {sock_fd, POLLIN, 0};
    int status = poll(&fds, 1, WAIT_TIME_MS);
    if(status < 0)
        ERROR("poll error");
    if(status == 0) {
        dprintf("No response received\n");
        return false;
    }

    //? recvfrom - receive a message from a socket
    ssize_t packet_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
    if (packet_len < 0)
        ERROR("recvfrom error");

    // struct ip* ip_header = (struct ip*) buffer;
    struct icmp icmp_header = IcmpIpHelper::get_icmp_header(buffer);
    struct icmp sent_icmp_header = IcmpIpHelper::get_sent_icmp_header(buffer);

    // ensure type
    if(icmp_header.icmp_type != ICMP_ECHOREPLY && icmp_header.icmp_type != ICMP_TIME_EXCEEDED) {
        dprintf("Received packet with wrong type\n");
        return false;
    }

    // check if its my packet
    if(icmp_header.icmp_type == ICMP_TIME_EXCEEDED && 
       sent_icmp_header.icmp_hun.ih_idseq.icd_id != getpid()) {
        dprintf("Received packet not meant for me:\t%d != %d\n", sent_icmp_header.icmp_hun.ih_idseq.icd_id, getpid());
        return false;
    } else if(icmp_header.icmp_type == ICMP_ECHOREPLY && 
              icmp_header.icmp_hun.ih_idseq.icd_id != getpid()) {
        dprintf("Received packet not meant for me:\t%d != %d\n", icmp_header.icmp_hun.ih_idseq.icd_id, getpid());
        return false;
    }

    // check checksum
    struct icmp* icmp_head = (struct icmp*) buffer;
    u_int16_t checksum = icmp_head->icmp_cksum;
    icmp_head->icmp_cksum = 0;
    if(checksum != IcmpIpHelper::compute_icmp_checksum((u_int16_t*)icmp_head, packet_len)) {
        dprintf("Received packet with wrong checksum\n");
        return false;
    }

    // extract ip
    char sender_ip_str[20];
    //* inet_ntop - convert IPv4 and IPv6 addresses from binary to text form
    inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
    ip = sender_ip_str;

    // get ttl from received packet
    const char type = icmp_header.icmp_type;
    if(type == 11) {
        ttl = UidHelper::get_ttl(sent_icmp_header);
        seq = UidHelper::get_seq(sent_icmp_header);
    } else if(type == 0) {
        // ttl = UidHelper::get_ttl(IcmpIpHelper::get_icmp_header(buffer));
    } else {
        ERROR("Received packet with wrong type");
    }
    
    if(DEBUG == 1 || DEBUG == 2 || DEBUG == 3) {
        dprintf("Received IP packet with ICMP content from: %s\n", sender_ip_str);
        Debug::decompose_response_timeout(buffer, 1,0,1);
    }

    return true;
}
};

class Traceroute {
    Traceroute() = delete;
public:
    static void traceroute(char* destination_ip) {
        // variables
        Timer timer;
        unordered_map<pair<int, int>, double> sent_times;

        // generate destination ip and check if valid
        struct sockaddr_in recipient;
        memset (&recipient, 0, sizeof(recipient));
        recipient.sin_family = AF_INET;
        if (inet_pton(AF_INET, destination_ip, &recipient.sin_addr) != 1) {
            ERROR("Invalid IP address");
        }

        // create socket
        int sock_fd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
        if (sock_fd < 0)
            ERROR("socket error");

        // for each ttl send packets without waiting
        // wait WAIT_TIME for responses
        // if none received, print * for path and ??? for time
        // if received, print all ips and avg time
        for (int ttl = 1; ttl <= MAX_TTL; ttl++) {
            dprintf("====== TTL = %d ======\n", ttl);

            vector<string> ips; // received ips from this ttl
            double total_rtt = 0;
            int responses = 0;

            // send packets
            for (int seq = 0; seq < PACKETS_PER_TTL; seq++) {
                sent_times[{ttl, seq}] = timer.now_precise();
                IcmpPackets::icmp_send(sock_fd, recipient, ttl, seq);
            }

            // wait WAIT_TIME for responses, without stalling cpu
            timer.reset();
            while(timer.elapsed() < WAIT_TIME_MS && responses < PACKETS_PER_TTL) {
                string ip;
                int recv_ttl = ttl;
                int recv_seq = 0;
                if(IcmpPackets::icmp_receive(sock_fd, ip, recv_ttl, recv_seq)) {
                    if (recv_ttl == ttl) {  // drop old ttls
                        if(find(ips.begin(), ips.end(), ip) == ips.end())
                            ips.push_back(ip);

                        double rtt = timer.now_precise() - sent_times[{recv_ttl, recv_seq}];
                        total_rtt += rtt;
                        responses++;
                    } else {
                        if(DEBUG == 2) {
                            dprintf("Received packet from %s with wrong TTL: %d\n", ip.c_str(), recv_ttl);
                        }
                    }
                }
            }

            dprintf("========> ");

            // check if destination reached
            if(!ips.empty() && find(ips.begin(), ips.end(), destination_ip) != ips.end()) {
                const double avg_rtt = total_rtt/(double)PACKETS_PER_TTL;
                printf("%d:\t%s %.2fms\n", ttl, destination_ip, avg_rtt);
                break;
            }

            // print ips
            printf("%d:\t", ttl);
            if(ips.empty()) {
                printf("*\n");
            } else {
                // ips
                for(string ip : ips) {
                    printf("%s ", ip.c_str());
                }
                
                // rtt
                if(responses < PACKETS_PER_TTL) {
                    printf("???");
                } else {
                    const double avg_rtt = (double)total_rtt/(double)PACKETS_PER_TTL;
                    printf("%.2fms", avg_rtt);    
                }

                printf("\n");
            }
        }

        close(sock_fd);
    }
};

// =================================================================================================
int main(int argc, char* argv[])
{
    dprintf("PID: %d\n", getpid());

    // ensure ip argumennt
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <destination IP>" << endl;
        return EXIT_FAILURE;
    }

    Traceroute::traceroute(argv[1]);

    return EXIT_SUCCESS;
}
