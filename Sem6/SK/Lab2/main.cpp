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
    
        uint32_t network_ip = 
                      (buffer[0] << 24) |
                      (buffer[1] << 16) |
                      (buffer[2] << 8)  |
                      (buffer[3]);
        uint32_t ip = ntohl(network_ip); // store in host order
        uint8_t prefix_len = buffer[4];
        uint8_t dist = buffer[5];
        
        string cidr_ip = to_string(buffer[0]) + "." + 
                         to_string(buffer[1]) + "." + 
                         to_string(buffer[2]) + "." + 
                         to_string(buffer[3]) + "/" + 
                         to_string(prefix_len);

        dprintf("IP: %u, prefix_len: %d, dist: %d\n", ip, prefix_len, (int)dist);
        dprintf("CIDR: %s\n", cidr_ip.c_str());


        // if cidr is not in table or is further away, update it
        if (routing_table.find(cidr_ip) == routing_table.end() || 
            routing_table[cidr_ip].dist > dist + 1) {
            routing_table[cidr_ip] = {
                .ip = ip,
                .prefix_len = prefix_len,
                .dist = static_cast<uint8_t>((dist == INF) ? INF : dist + 1),
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
        struct sockaddr_in server_address;
        memset(&server_address, 0, sizeof(server_address));
        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(PORT);

        if(inet_pton(AF_INET, iface.c_str(), &server_address.sin_addr) != 1) {
            ERROR("Invalid broadcast IP address");
        }

        // send every entry with different packet
        for (const auto& [key, net] : routing_table) {
            if (net.dist == INF && !net.connected_directly) continue;
            
            uint32_t network_ip = htonl(net.ip);
            uint8_t packet[6] = {
                static_cast<uint8_t>((network_ip >> 24) & 0xFF),
                static_cast<uint8_t>((network_ip >> 16) & 0xFF),
                static_cast<uint8_t>((network_ip >> 8) & 0xFF),
                static_cast<uint8_t>(network_ip & 0xFF),
                net.prefix_len,
                net.dist
            };

            dprintf("Sending packet to %s\n", iface.c_str());
            dprintf("Packet: %u.%u.%u.%u/%d %d\n", 
                    packet[0], packet[1], packet[2], packet[3], packet[4], packet[5]);
                
            if (sendto(sock_fd, packet, sizeof(packet), 0, (struct sockaddr*)&server_address, sizeof(server_address)) != sizeof(packet)) {
                ERROR("sendto error");
            }
        }
    }

    close(sock_fd);
}


//? prints routing table
void printTable(ostream& os) {
    os << "----- Routing table -----\n";
    for (const auto& [key, net] : routing_table) {
        struct in_addr ip_addr;
        ip_addr.s_addr = htonl(net.ip);
        char ip_str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &ip_addr, ip_str, sizeof(ip_str));

        os << ip_str << "/" << (int)net.prefix_len << " distance ";

        if (net.dist == INF)
            os << "unreachable";
        else
            os << (int)net.dist;

        if (net.connected_directly)
            os << " connected directly";
        else
            os << " via " << net.next_hop;
        
        os << "\n------------------------\n";
    }
}


void runTasksOnTime() {
    Timer timer_print_table, timer_broadcast_table, timer_receive_tables;

    while (true) {
        if (timer_broadcast_table.elapsed() > TABLE_BROADCAST_INTERVAL * 1000) {
            broadcastTable();
            timer_broadcast_table.reset();
        }
        
        if (timer_receive_tables.elapsed() > RECEIVE_TABLES_INTERVAL) {
            receiveTables();
            timer_receive_tables.reset();
        }

        if (timer_print_table.elapsed() > PRINT_TABLE_INTERVAL * 1000) {
            printTable(cout);
            timer_print_table.reset();
        }

        // sleep for 100ms to avoid busy waiting
        this_thread::sleep_for(chrono::milliseconds(100)); 
    }
}


//? reads config from stdin
void readStdinConfig() {
    int n; cin >> n;

    for(int i = 0; i < n; i++) {
        string ip_cidr, trash;
        int idist;

        cin >> ip_cidr >> trash >> idist;

        uint32_t dist = static_cast<uint32_t>(idist);

        // parse CIDR notation and add to routing table
        size_t pos = ip_cidr.find('/');
        if (pos == string::npos) { // slash not found
            ERROR("Invalid CIDR format");
        }
        
        string ip_str = ip_cidr.substr(0, pos);
        uint8_t prefix_len = stoi(ip_cidr.substr(pos + 1));

        // convert IP string to uint32_t and check validity
        uint32_t ip_net;
        if (inet_pton(AF_INET, ip_str.c_str(), &ip_net) != 1) {
            ERROR("Invalid IP address");
        }
        uint32_t ip = ntohl(ip_net); // store in host order

        dprintf("IP: %u, prefix_len: %d, dist: %d\n", ip, prefix_len, (int)dist);

        routing_table[ip_cidr] = {
            .ip = ip,
            .prefix_len = prefix_len,
            .dist = dist,
            .connected_directly = true,
            .next_hop = ""
        };

        // generate broadcast address
        // generate address mask - start from all 1s and shift left by prefix_len
        uint32_t mask = (prefix_len == 0) ? 0 : (~0U << (32 - prefix_len));
        // broadcast is las addr in range - combine given ip and place 1 in all 0 of mask
        uint32_t broadcast = (ntohl(ip) & mask) | ~mask;

        struct in_addr broadcast_addr;
        broadcast_addr.s_addr = broadcast;

        char broadcast_str[INET_ADDRSTRLEN];
        if (inet_ntop(AF_INET, &broadcast_addr, broadcast_str, sizeof(broadcast_str)) == nullptr) {
            ERROR("Failed to generate broadcast address");
        }

        interfaces.push_back(string(broadcast_str));
    }
}



int main() {
    readStdinConfig();
    runTasksOnTime();
}
