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

#define DEBUG 2     // debug stage
#define dprintf if(DEBUG) printf

//* ------------------ CONSTANTS ------------------
constexpr int PORT = 54321;
constexpr int TABLE_BROADCAST_INTERVAL = 15 * 1000; // milliseconds
constexpr int PRINT_TABLE_INTERVAL = 5 * 1000; // milliseconds
constexpr int RECEIVE_TABLES_INTERVAL = 500; // milliseconds
constexpr uint8_t MAX_DIST = (1 << 4) - 1;   //TODO use it (max distance)
constexpr uint8_t INF = (1 << 8) - 1;
constexpr int TIME_TO_DIE = 60 * 1000; // milliseconds, time to mark as unreachable without receiving any packets
constexpr int TIME_KEEP_UNREACHABLE = 30 * 1000; // milliseconds
constexpr int INF_BROADCAST_COUNT = 3; 

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

    int64_t elapsed() const {
        return chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now() - start_time).count();
    }

    int64_t now() const {
        return chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
    double now_precise() const {
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


//* =================== IMPLEMENTATION ===================
struct Network {
    uint32_t ip;    //? network byte order
    uint8_t prefix_len;
    uint8_t dist;

    bool connected_directly = true;
    string next_hop = "";

    Timer last_reachable = Timer(); //? time since last received/socket working/dist not INF
    Timer last_updated = Timer(); //? time since last received/socket working/dist not INF
};

struct Interface {
    uint8_t dist;
    string cidr;
};

//* -------------- GLOBALS  ---------------
unordered_map<string, Network> routing_table;
vector<pair<string, Interface>> net_broadcast_adddr; //? {broadcast address, interface distance}

int receive_sock = -1;
int broadcast_sock = -1;


//* -------------- HELPERS -----------------
uint32_t get_network_ip(uint32_t ip, uint8_t prefix_len) {
    uint32_t mask = (prefix_len == 0) ? 0 : htonl(~0U << (32 - prefix_len));
    return ip & mask;
}


//* -------------- MAIN FUNCTIONS -----------------
//? initializes sockets for receiving and broadcasting
void initSockets() {
    // receive
    receive_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (receive_sock < 0) {
        ERROR("socket error");
    }
    
    // port reuse
    int opt = 1;
    setsockopt(receive_sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    // bind to port
    struct sockaddr_in server_address;
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(PORT);
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(receive_sock, (struct sockaddr*)&server_address, sizeof(server_address)) < 0) {
        ERROR("bind error");
    }

    // broadcast
    broadcast_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (broadcast_sock < 0) {
        ERROR("socket error");
    }
    
    // enable broadcast
    int broadcast_enable = 1;
    if (setsockopt(broadcast_sock, SOL_SOCKET, SO_BROADCAST, 
                  &broadcast_enable, sizeof(broadcast_enable)) < 0) {
        ERROR("setsockopt error");
    }

    dprintf("Port %d bound to %s\n", PORT, inet_ntoa(server_address.sin_addr));
}


//? listens for incoming routing tables
void receiveTables() {
    struct pollfd fds = {receive_sock, POLLIN, 0};
    Timer time_left;
    int status = poll(&fds, 1, RECEIVE_TABLES_INTERVAL - time_left.elapsed());
    
    // TODO: ensure packet is ours and stuff
    while (status > 0) {
        struct sockaddr_in sender;
        socklen_t sender_len = sizeof(sender);
        uint8_t buffer[IP_MAXPACKET+1];

        ssize_t datagram_len = recvfrom(receive_sock, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
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
    
        uint32_t network_ip = *reinterpret_cast<uint32_t*>(buffer); // parse received in network byte order
        uint8_t prefix_len = buffer[4];
        uint8_t dist = buffer[5];
        
        string cidr_net_ip = to_string(buffer[0]) + "." + 
                             to_string(buffer[1]) + "." + 
                             to_string(buffer[2]) + "." + 
                             to_string(buffer[3]) + "/" + 
                             to_string(prefix_len);

        dprintf("CIDR: %s, dist: %d\n", cidr_net_ip.c_str(), (int)dist);


        // convert sender ip to network byte order
        in_addr sender_addr;
        inet_pton(AF_INET, sender_ip_str, &sender_addr);
        uint32_t sender_ip_net = sender_addr.s_addr;

        // find best matching route to sender ip
        uint8_t dist_from_sender = INF;
        size_t best_prefix = 0;
        for (const auto& [cidr, entry] : routing_table) {
            // check if ip matches network
            if (get_network_ip(sender_ip_net, entry.prefix_len) == 
                get_network_ip(entry.ip, entry.prefix_len)) {
                if (entry.prefix_len > best_prefix) {
                    best_prefix = entry.prefix_len;
                    dist_from_sender = entry.dist;
                }
            }
        }

        // if sender is from direct interface, compare it with its distance and ensure iface marked as alive
        for (const auto& [iface_broad, iface] : net_broadcast_adddr) {
            if (sender_ip_net == inet_addr(iface_broad.c_str())) {
                dist_from_sender = min(dist_from_sender, iface.dist);

                routing_table[iface.cidr].last_reachable.reset(); // mark iface as reachable

                if(routing_table[iface.cidr].dist == INF) {
                    routing_table[iface.cidr].dist = iface.dist; // update distance to iface
                    routing_table[iface.cidr].last_updated.reset(); // mark iface as reachable
                }

                break;
            }
        }

        // no route found - skip
        if (dist_from_sender == INF) {
            dprintf("No route to sender %s\n", sender_ip_str);
            continue;
        }

        // if cidr is not in table or is further away, update it
        if (routing_table.find(cidr_net_ip) == routing_table.end() || 
            (!routing_table[cidr_net_ip].connected_directly && 
                dist != INF &&
                routing_table[cidr_net_ip].dist > (int)dist + (int)dist_from_sender)) 
        {
            routing_table[cidr_net_ip] = {
                .ip = network_ip,
                .prefix_len = prefix_len,
                .dist = static_cast<uint8_t>(dist + dist_from_sender),
                .connected_directly = false,
                .next_hop = sender_ip_str,
                .last_reachable = Timer(),
                .last_updated = Timer(),
            };
        }

        status = poll(&fds, 1, RECEIVE_TABLES_INTERVAL - time_left.elapsed());
    }

    if (status < 0) {
        ERROR("poll error");
    }
}


//? sends routing table on broadcast address
void broadcastTable() {
    // send to every interface connected
    for (const auto& [iface, if_data] : net_broadcast_adddr) {
        struct sockaddr_in dest_addr;
        memset(&dest_addr, 0, sizeof(dest_addr));
        dest_addr.sin_family = AF_INET;
        dest_addr.sin_port = htons(PORT);

        if(inet_pton(AF_INET, iface.c_str(), &dest_addr.sin_addr) != 1) {
            ERROR("Invalid broadcast IP address");
        }

        // send every entry with different packet
        for (const auto& [cidr, net] : routing_table) {
            // skip expired unreachable routes
            if (net.dist == INF && net.last_updated.elapsed() > TIME_KEEP_UNREACHABLE) {
                continue;
            }

            // prepare packet
            uint8_t packet[6];
            *reinterpret_cast<uint32_t*>(packet) = net.ip;  //? network byte order
            packet[4] = net.prefix_len;
            packet[5] = net.dist;

            dprintf("Sending packet to %s\n", iface.c_str());
            dprintf("Packet: %u.%u.%u.%u/%d %d\n", 
                    packet[0], packet[1], packet[2], packet[3], packet[4], packet[5]);
                
            if (sendto(broadcast_sock, packet, sizeof(packet), 0, (struct sockaddr*)&dest_addr, sizeof(dest_addr)) != sizeof(packet)) {
                dprintf("Interface %s sendto failed: %s\n", iface.c_str(), strerror(errno));

                // mark all routes through this interface as unreachable
                for (auto& [key, route] : routing_table) {
                    if (route.next_hop == iface) {
                        route.dist = INF;
                        route.last_updated.reset();
                    }
                }

                continue;
            }

            // update direct route timestamps
            for (auto& [key, route] : routing_table) {
                if (route.connected_directly && route.ip == net.ip) {
                    route.last_reachable.reset();
                }
            }
        }
    }
}


//? removes old unreachable (excluding interfaces)
void removeUnreachable() {
    for (auto it = routing_table.begin(); it != routing_table.end();) {
        Network& net = it->second;
        
        // handle indirect routes
        if (!net.connected_directly) {
            // mark as INF (unreachable) if no updates received
            if (net.last_reachable.elapsed() > TIME_TO_DIE && net.dist != INF) {
                net.dist = INF;
                net.last_updated.reset();
                dprintf("Marked %s as unreachable (timeout)\n", it->first.c_str());
            }
            
            // remove if INF expired (unreachable for too long)
            if (net.dist == INF && net.last_updated.elapsed() > TIME_KEEP_UNREACHABLE) {
                dprintf("Removing expired route: %s\n", it->first.c_str());
                it = routing_table.erase(it);
                continue;
            }
        }
        

        // TODO should i keep original distance?
        // handle direct routes
        if (net.connected_directly) {
            // mark direct as INF if interface down, but keep in table
            if (net.last_reachable.elapsed() > TIME_TO_DIE && net.dist != INF) {
                net.dist = INF;
                net.last_updated.reset();
                dprintf("Marked direct route %s as down (timeout)\n", it->first.c_str());
            }
        }
        
        ++it;
    }
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
           << (net.connected_directly ? " connected directly" : " via " + net.next_hop)
           << '\n';
    }
    os << "------------------------\n";
}


void runTasks() {
    Timer timer_print_table, timer_broadcast_table;

    while (true) {
        if (timer_broadcast_table.elapsed() > TABLE_BROADCAST_INTERVAL) {
            broadcastTable();
            timer_broadcast_table.reset();
        }
        
        if (timer_print_table.elapsed() > PRINT_TABLE_INTERVAL) {
            printTable(cout);
            timer_print_table.reset();
        }

        receiveTables();
        removeUnreachable();
        // sleep for 100ms to avoid busy waiting
        this_thread::sleep_for(chrono::milliseconds(100)); 
    }
}

bool debugInteract() {
    char op; cin >> op;

    switch (op) {
    case 'p':
        printTable(cout);
        break;
    case 't':
        printTable(cout);
        break;
    case 'b':
        broadcastTable();
        break;
    case 'r':
        receiveTables();
        break;
    case 'c':   // run continuously 
        return false;
    case 'q':
        cout << "Exiting..." << endl;
        exit(0);
    }

    return true;
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
        in_addr host_ip;
        if (inet_pton(AF_INET, ip_str.c_str(), &host_ip) != 1) {
            ERROR("Invalid IP address");
        }

        dprintf("IP: %u, prefix_len: %d, dist: %d\n", host_ip.s_addr, prefix_len, (int)dist);

        // generate broadcast address
        // generate address mask in network order - start from all 1s and shift left by prefix_len
        uint32_t mask = (prefix_len) ? (~0U << (32 - prefix_len)) : 0;
        // broadcast is las addr in range - combine given ip and place 1 in all 0 of mask
        mask = htonl(mask); // convert to network byte order
        uint32_t network_addr = host_ip.s_addr & mask;
        uint32_t broadcast = network_addr | ~mask;


        char network_str[INET_ADDRSTRLEN];
        in_addr network_ip = {network_addr};
        inet_ntop(AF_INET, &network_ip, network_str, INET_ADDRSTRLEN);
        string network_cidr = string(network_str) + "/" + to_string(prefix_len);
        

        routing_table[network_cidr] = {
            .ip = network_addr,
            .prefix_len = prefix_len,
            .dist = dist,
            .connected_directly = true,
            .next_hop = "",
            // .reachable = true,
        };


        char broadcast_str[INET_ADDRSTRLEN];
        in_addr bc_addr = {broadcast};
        inet_ntop(AF_INET, &bc_addr, broadcast_str, INET_ADDRSTRLEN);
        net_broadcast_adddr.push_back({broadcast_str, Interface{
            .dist = dist,
            .cidr = network_cidr,
        }});
    }
}



int main() {
    initSockets();

    readStdinConfig();

    while(DEBUG && debugInteract()) {}

    runTasks();

    close(receive_sock);
    close(broadcast_sock);
}
