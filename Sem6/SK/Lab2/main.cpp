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
#include <chrono>
#include <thread>


using namespace std;

#define DEBUG 1     // debug stage; 0 = none, verbose = 3, minimal >= 4
#define dprintf if(DEBUG) printf


constexpr int PORT = 54321;
constexpr int TABLE_BROADCAST_INTERVAL = 15; // seconds
constexpr int PRINT_TABLE_INTERVAL = 5; // seconds
constexpr int RECEIVE_TABLES_INTERVAL = 500; // milliseconds
constexpr uint8_t INF = (1 << 8) - 1;

//* ------------- HELPER -----------------
void ERROR(const char* str) {
    fprintf(stderr, "%s: %s\n", str, strerror(errno));  // NOLINT(*-err33-c)
    exit(EXIT_FAILURE);
}

class Timer {
public:
    chrono::time_point<chrono::high_resolution_clock> start_time;
    Timer() : start_time(chrono::high_resolution_clock::now()) {
        reset();
    }

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


//* ------------- MAIN IMPL -----------------
struct Network {
    uint32_t ip;    //? network byte order
    uint8_t prefix_len;
    uint8_t dist;

    bool connected_directly = true;
    string next_hop = "";
};

unordered_map<string, Network> routing_table;
vector<string> interfaces;


//? listens for incoming routing tables
void receiveTables() {
    int sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        ERROR("socket error");
    }
    
    struct sockaddr_in server_address;
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(PORT);
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(sock_fd, (struct sockaddr*)&server_address, sizeof(server_address)) < 0) {
        ERROR("bind error");
    }

    struct pollfd fds = {sock_fd, POLLIN, 0};
    Timer time_left;
    int status = poll(&fds, 1, RECEIVE_TABLES_INTERVAL - time_left.elapsed());
    
    while (status > 0) {
        struct sockaddr_in sender;
        socklen_t sender_len = sizeof(sender);
        uint8_t buffer[IP_MAXPACKET+1];

        ssize_t datagram_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
        if (datagram_len < 0) {
            ERROR("recvfrom error");
        }

        if (datagram_len < 6) {
            dprintf("Invalid packet size: %zd\n", datagram_len);
            continue;
        }

        char sender_ip_str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
        dprintf("Received UDP packet from IP address: %s, port: %d\n", sender_ip_str, ntohs(sender.sin_port));
     
        buffer[datagram_len] = 0;
        dprintf("%ld-byte message: +%s+\n", datagram_len, (char*)buffer);
    
        uint32_t network_ip = *reinterpret_cast<uint32_t*>(buffer); // parse received in network byte order
        uint8_t prefix_len = buffer[4];
        uint8_t dist = buffer[5];
        
        string cidr_ip = to_string(buffer[0]) + "." + 
                         to_string(buffer[1]) + "." + 
                         to_string(buffer[2]) + "." + 
                         to_string(buffer[3]) + "/" + 
                         to_string(prefix_len);

        dprintf("IP: %u, prefix_len: %d, dist: %d\n", network_ip, prefix_len, (int)dist);
        dprintf("CIDR: %s\n", cidr_ip.c_str());


        // if cidr is not in table or is further away, update it
        if (routing_table.find(cidr_ip) == routing_table.end() || 
            (!routing_table[cidr_ip].connected_directly && 
             dist != INF &&
             routing_table[cidr_ip].dist > dist + 1)) {
            routing_table[cidr_ip] = {
                .ip = network_ip,
                .prefix_len = prefix_len,
                .dist = static_cast<uint8_t>(dist + 1),
                .connected_directly = false,
                .next_hop = sender_ip_str
            };
        }


        status = poll(&fds, 1, RECEIVE_TABLES_INTERVAL - time_left.elapsed());
    }

    if (status < 0) {
        ERROR("poll error");
    }

    close(sock_fd);
}


//? sends routing table on broadcast address
void broadcastTable() {
    int sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        ERROR("socket error");
    }

    // needed to access the broadcast address
    int broadcast_enable = 1;
    if (setsockopt(sock_fd, SOL_SOCKET, SO_BROADCAST, &broadcast_enable, sizeof(broadcast_enable)) < 0) {
        ERROR("setsockopt error");
    }

    // send to every interface connected
    for (const auto& iface : interfaces) {
        struct sockaddr_in dest_addr;
        memset(&dest_addr, 0, sizeof(dest_addr));
        dest_addr.sin_family = AF_INET;
        dest_addr.sin_port = htons(PORT);

        if(inet_pton(AF_INET, iface.c_str(), &dest_addr.sin_addr) != 1) {
            ERROR("Invalid broadcast IP address");
        }

        // send every entry with different packet
        for (const auto& [cidr, net] : routing_table) {
            if (net.dist == INF) continue;

            uint8_t packet[6];
            *reinterpret_cast<uint32_t*>(packet) = net.ip;  //? network byte order
            packet[4] = net.prefix_len;
            packet[5] = net.dist;

            dprintf("Sending packet to %s\n", iface.c_str());
            dprintf("Packet: %u.%u.%u.%u/%d %d\n", 
                    packet[0], packet[1], packet[2], packet[3], packet[4], packet[5]);
                
            if (sendto(sock_fd, packet, sizeof(packet), 0, (struct sockaddr*)&dest_addr, sizeof(dest_addr)) != sizeof(packet)) {
                ERROR("sendto error");
            }
        }
    }

    close(sock_fd);
}


//? prints routing table
void printTable(ostream& os) {
    os << "----- Routing table -----\n";
    for (const auto& [cidr, net] : routing_table) {

        char ip_str[INET_ADDRSTRLEN];
        in_addr addr = {net.ip};  //? network order to presentation
        inet_ntop(AF_INET, &addr, ip_str, INET_ADDRSTRLEN);

        os << ip_str << "/" << static_cast<int>(net.prefix_len)
           << " distance " << static_cast<int>(net.dist)
           << " via " << (net.connected_directly ? "direct" : net.next_hop)
           << endl;
    }
    os << "\n------------------------\n";
}


void runTasks() {
    Timer timer_print_table, timer_broadcast_table;

    while (true) {
        if (timer_broadcast_table.elapsed() > TABLE_BROADCAST_INTERVAL * 1000) {
            broadcastTable();
            timer_broadcast_table.reset();
        }
        
        if (timer_print_table.elapsed() > PRINT_TABLE_INTERVAL * 1000) {
            printTable(cout);
            timer_print_table.reset();
        }

        // sleep for 100ms to avoid busy waiting
        receiveTables();
        this_thread::sleep_for(chrono::milliseconds(100)); 
    }
}


//? reads config from stdin
void readStdinConfig() {
    int n; cin >> n;

    for(int i = 0; i < n; i++) {
        string cidr, trash;
        int idist;

        cin >> cidr >> trash >> idist;

        uint8_t dist = static_cast<uint8_t>(idist);

        // parse CIDR notation and add to routing table
        size_t slash_pos = cidr.find('/');
        if (slash_pos == string::npos) { // slash not found
            ERROR("Invalid CIDR format");
        }
        
        string ip_str = cidr.substr(0, slash_pos);
        uint8_t prefix_len = stoi(cidr.substr(slash_pos + 1));

        // convert to network byte order
        in_addr ip_net;
        if (inet_pton(AF_INET, ip_str.c_str(), &ip_net) != 1) {
            ERROR("Invalid IP address");
        }

        dprintf("IP: %u, prefix_len: %d, dist: %d\n", ip_net, prefix_len, (int)dist);

        routing_table[cidr] = {
            .ip = ip_net.s_addr,
            .prefix_len = prefix_len,
            .dist = dist,
            .connected_directly = true,
            .next_hop = ""
        };

        // generate broadcast address
        // generate address mask in network order - start from all 1s and shift left by prefix_len
        uint32_t mask = (prefix_len) ? htonl(~0U << (32 - prefix_len)) : 0;
        // broadcast is las addr in range - combine given ip and place 1 in all 0 of mask
        uint32_t broadcast = (ip_net.s_addr & mask) | ~mask;

        char broadcast_str[INET_ADDRSTRLEN];
        in_addr bc_addr = {broadcast};
        inet_ntop(AF_INET, &bc_addr, broadcast_str, INET_ADDRSTRLEN);
        interfaces.push_back(broadcast_str);
    }
}



int main() {
    readStdinConfig();
    runTasks();
}
