#include <bits/stdc++.h>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace std;

#define dprintf if(1) printf

constexpr int MAX_HOPS = 30;
constexpr int WAIT_TIME = 1;
constexpr int PACKETS_PER_TTL = 2;


//? handle errors - write them and exit
void ERROR(const char* str)
{
    fprintf(stderr, "%s: %s\n", str, strerror(errno));
    exit(EXIT_FAILURE);
}

/*//* #region ---DEBUG--- */
void print_as_bytes (unsigned char* buff, ssize_t length)
{
    for (ssize_t i = 0; i < length; i++, buff++)
        dprintf("%.2x ", *buff);
}
void print_icmp_header (unsigned char* header, ssize_t header_length)
{
    struct icmp* icmp_header = (struct icmp*) header;
    dprintf("Type: %d\n", icmp_header->icmp_type);
    dprintf("Code: %d\n", icmp_header->icmp_code);
    dprintf("Checksum: %d\n", icmp_header->icmp_cksum);
    dprintf("ID: %d\n", icmp_header->icmp_hun.ih_idseq.icd_id);
    dprintf("Sequence: %d\n", icmp_header->icmp_hun.ih_idseq.icd_seq);
}
void print_ip_header (unsigned char* header, ssize_t header_length)
{
    struct ip* ip_header = (struct ip*) header;
    dprintf("Version: %d\n", ip_header->ip_v);
    dprintf("Header length: %d\n", ip_header->ip_hl);
    dprintf("Type of service: %d\n", ip_header->ip_tos);
    dprintf("Total length: %d\n", ip_header->ip_len);
    dprintf("Identification: %d\n", ip_header->ip_id);
    dprintf("Fragment offset: %d\n", ip_header->ip_off);
    dprintf("Time to live: %d\n", ip_header->ip_ttl);
    dprintf("Protocol: %d\n", ip_header->ip_p);
    dprintf("Checksum: %d\n", ip_header->ip_sum);
    dprintf("Source: %s\n", inet_ntoa(ip_header->ip_src));
    dprintf("Destination: %s\n", inet_ntoa(ip_header->ip_dst));
}

//? debug/sandbox playing with response
void decompose_response(
    unsigned char* ip_header_start, 
    ssize_t length, 
    bool p_ip_header=true, 
    bool p_icmp_header=true,
    bool p_original_ip_header=true
) {
    struct ip* ip_header = (struct ip*) ip_header_start;
    ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);

    // received entire packet header, usually first 20 bytes
    if(p_ip_header) {
        dprintf("IP header:\n");
        print_ip_header(ip_header_start, ip_header_len);
        dprintf("\n");
    }

    // received entire packet data, after the header
    // in our case, if its icmp header, there will be info about our ping (reply or ttl exceeded)
    unsigned char *ip_data_start = ip_header_start + ip_header_len;
    const ssize_t packet_len = length - ip_header_len;
    if(p_icmp_header) {
        dprintf("IP data / ICMP header:\n");
        print_icmp_header(ip_data_start, packet_len);
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
        print_icmp_header(original_ip_header_start, ip_header_len);
        dprintf("\n\n");
    }
}
/*//* #endregion */

//? compute checksum for icmp packet
u_int16_t compute_icmp_checksum(const void *buff, int length)
{
    const u_int16_t* ptr = reinterpret_cast<const uint16_t*>(buff);
    u_int32_t sum = 0;
    assert (length % 2 == 0);
    for (; length > 0; length -= 2)
        sum += *ptr++;
    sum = (sum >> 16U) + (sum & 0xffffU);
    return (u_int16_t)(~(sum + (sum >> 16U)));
}

inline int gen_uid(int ttl, int seq) {
    return ttl * PACKETS_PER_TTL + seq;
}
inline int get_ttl(int uid) {
    return uid / PACKETS_PER_TTL;
}
inline int get_seq(int uid) {
    return uid % PACKETS_PER_TTL;
}

int find_header_start(unsigned char* buffer, ssize_t length) {
    const int IP_HEADER_SIZE = 20;
    for (int i = 0; i < length - 1; i++) {
        if (buffer[i] == 0x45 && buffer[i + 1] == 0x00) {
            return i;
        }
    }
}

//? send icmp echo packet
void icmp_send(int sock_fd, struct sockaddr_in &recipient, int ttl, int seq) {
    // generate header
    struct icmp header;
    header.icmp_type = ICMP_ECHO;
    header.icmp_code = 0;
    header.icmp_hun.ih_idseq.icd_id = getpid();
    header.icmp_hun.ih_idseq.icd_seq = gen_uid(ttl, seq);
    header.icmp_cksum = 0;  // we have to set it to 0 in order to computer correct chksum
    header.icmp_cksum = compute_icmp_checksum ((u_int16_t*)&header, sizeof(header));

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

    dprintf("Sent ICMP echo packet with TTL = %d\n", ttl);
    dprintf("ICMP header:\n");
    print_icmp_header((unsigned char*)&header, sizeof(header));
    dprintf("\n");
}

// TODO fix stalling with poll
//? receive icmp echo packet 
void icmp_receive(int sock_fd, string &ip, int &ttl, long &rtt) {
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    u_int8_t buffer[IP_MAXPACKET];

    // stall here (i think)
    ssize_t packet_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
    if (packet_len < 0)
        ERROR("recvfrom error");

    char sender_ip_str[20];
    inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
    ip = sender_ip_str;
    dprintf("Received IP packet with ICMP content from: %s\n", sender_ip_str);

    decompose_response(buffer, packet_len, 1,0,1);
}



// ==============================

int main(int argc, char* argv[])
{
    // ensure ip argumennt
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <destination IP>" << endl;
        return EXIT_FAILURE;
    }

    // generate destination ip and check if valid
    struct sockaddr_in recipient;
    memset (&recipient, 0, sizeof(recipient));
    recipient.sin_family = AF_INET;
    if (inet_pton(AF_INET, argv[1], &recipient.sin_addr) != 1) {
        ERROR("Invalid IP address");
    }

    // create socket
    int sock_fd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sock_fd < 0)
        ERROR("socket error");

    // for each ttl send 3 packets without waiting
    // wait WAIT_TIME for responses
    // if none received, print * for path and ??? for time
    // if received, print all ips and avg time
    for (int ttl = 1; ttl <= MAX_HOPS; ttl++) {
        dprintf("====== TTL = %d ======\n", ttl);

        vector<string> ips; // received ips from this ttl

        // send packets
        for (int seq = 0; seq < PACKETS_PER_TTL; seq++) {
            dprintf("--- SEQ = %d ---\n", seq);
            icmp_send(sock_fd, recipient, ttl, seq);
        }

        // wait WAIT_TIME for responses, without stalling cpu
        for (int seq = 0; seq < PACKETS_PER_TTL; seq++) {
            dprintf("--- SEQ = %d ---\n", seq);
            
            string ip;
            int recv_ttl;
            long rtt;
            icmp_receive(sock_fd, ip, recv_ttl, rtt);
            if (recv_ttl == ttl) {
                ips.push_back(ip);
            }
        }
    }

    // ------------------------------
    // receive example

    // for (;;) {
    //     struct sockaddr_in sender;
    //     socklen_t sender_len = sizeof(sender);
    //     u_int8_t buffer[IP_MAXPACKET];

    //     ssize_t packet_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
    //     if (packet_len < 0)
    //         ERROR("recvfrom error");

    //     char sender_ip_str[20];
    //     inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
    //     printf("Received IP packet with ICMP content from: %s\n", sender_ip_str);

    //     struct ip* ip_header = (struct ip*) buffer;
    //     ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);

    //     printf("IP header: ");
    //     print_as_bytes(buffer, ip_header_len);
    //     printf("\n");

    //     printf("IP data:   ");
    //     print_as_bytes(buffer + ip_header_len, packet_len - ip_header_len);
    //     printf("\n\n");
    // }
}
