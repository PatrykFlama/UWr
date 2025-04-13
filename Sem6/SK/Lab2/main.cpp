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

#define DEBUG 0
#define dprintf if(DEBUG) printf

// TODO dist should be uint32_t
//* ------------------ CONSTANTS ------------------
constexpr int PORT = 54321;
constexpr int TABLE_BROADCAST_INTERVAL = 5 * 1000; // milliseconds
constexpr int PRINT_TABLE_INTERVAL = 2 * 1000; // milliseconds
constexpr int RECEIVE_TABLES_INTERVAL = 500; // milliseconds
constexpr uint32_t MAX_DIST = (1 << 4) - 1;
constexpr uint32_t INF = (1 << 8) - 1;
constexpr int TIME_TO_DIE = 10 * 1000; // milliseconds, time to mark as unreachable without receiving any packets
constexpr int TIME_KEEP_UNREACHABLE = 10 * 1000; // milliseconds
// constexpr int PORT = 54321;
// constexpr int TABLE_BROADCAST_INTERVAL = 15 * 1000; // milliseconds
// constexpr int PRINT_TABLE_INTERVAL = 5 * 1000; // milliseconds
// constexpr int RECEIVE_TABLES_INTERVAL = 500; // milliseconds
// constexpr uint32_t MAX_DIST = (1 << 4) - 1;
// constexpr uint32_t INF = (1 << 8) - 1;
// constexpr int TIME_TO_DIE = 60 * 1000; // milliseconds, time to mark as unreachable without receiving any packets
// constexpr int TIME_KEEP_UNREACHABLE = 30 * 1000; // milliseconds

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
    uint32_t dist = INF;

    bool connected_directly = false;
    string next_hop = "";
    string iface_bcast = "";

    Timer last_reachable = Timer(); //? time since last event on this route (receive/send/timeout/socket)
};

struct Interface {
    bool up = true;
    uint32_t dist = 0;
    Timer last_reachable = Timer();
    uint8_t prefix_len = 0;
    uint32_t iface_ip = 0;
    string iface_net = "";
};

//* -------------- GLOBALS  ---------------
unordered_map<string, Network> routing_table;
unordered_map<string, Interface> interfaces;    //? broadcast iface addr -> interface details
unordered_map<string, string> interface_to_bcast; //? interface addr -> broadcast addr

int receive_sock = -1;
int broadcast_sock = -1;


//* -------------- HELPERS -----------------
uint32_t get_network_ip(uint32_t ip, uint8_t prefix_len) {
    uint32_t mask = (prefix_len == 0) ? 0 : htonl(~0U << (32 - prefix_len));
    return ip & mask;
}

uint32_t get_broadcast_ip(uint32_t ip, uint8_t prefix_len) {
    uint32_t mask = (prefix_len == 0) ? 0 : htonl(~0U << (32 - prefix_len));
    return ip | ~mask;
}

string get_broadcast_ip_str(uint32_t ip, uint8_t prefix_len) {
    uint32_t broadcast = get_broadcast_ip(ip, prefix_len);
    in_addr addr = {broadcast};
    char buffer[INET_ADDRSTRLEN];
    inet_ntop(AF_INET, &addr, buffer, INET_ADDRSTRLEN);
    return string(buffer);
}


//? adds interface to routing table if not already present
void addIfaceToRoutingTable(const string& iface, Interface& iface_details, bool only_nonexistent = false) {
    if (routing_table.count(iface_details.iface_net) == 0) {
        routing_table[iface_details.iface_net] = {
            .ip = iface_details.iface_ip,
            .prefix_len = iface_details.prefix_len,
            .dist = (iface_details.up ? iface_details.dist : INF),
            .connected_directly = true,
            .next_hop = "",
            .iface_bcast = iface,
            .last_reachable = Timer(),
        };
    } else if (!only_nonexistent) {
        auto& route = routing_table[iface_details.iface_net];
        route.connected_directly = true;
        route.next_hop = "";
        route.iface_bcast = iface;
        route.prefix_len = iface_details.prefix_len;
        route.dist = (iface_details.up ? iface_details.dist : INF),
        route.last_reachable.reset();
    }
}


//? manages interface status based on send success
void manageIfaceStatus(const string& iface, Interface& iface_details, bool is_up) {
    if (!is_up && iface_details.up) {
        dprintf("Interface %s down\n", iface.c_str());

        // interface down
        iface_details.up = false;
        iface_details.last_reachable.reset();
        
        // mark directly connected routes as INF
        for (auto& [key, route] : routing_table) {
            if (route.connected_directly && route.iface_bcast == iface) {
                route.dist = INF;
                route.last_reachable.reset();
            }
        }
    } else if (is_up && !iface_details.up) {
        // interface recovered
        iface_details.up = true;

        addIfaceToRoutingTable(iface, iface_details);
    }
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
}


//? listens for incoming routing tables
void receiveTables() {
    struct pollfd fds = {receive_sock, POLLIN, 0};
    Timer time_left;
    
    while (true) {
        // ----- wait for packet ------
        int timeout = RECEIVE_TABLES_INTERVAL - time_left.elapsed();
        if (timeout <= 0) {
            break; // timeout
        }

        int status = poll(&fds, 1, timeout);
        if (status < 0) {
            ERROR("poll error");
        } else if (status == 0) {
            break; // timeout
        }


        // ------ receive packet ------
        struct sockaddr_in sender;
        socklen_t sender_len = sizeof(sender);
        uint8_t buffer[IP_MAXPACKET+1];

        // receive
        ssize_t datagram_len = recvfrom(receive_sock, buffer, IP_MAXPACKET, 
                                        0, (struct sockaddr*)&sender, &sender_len);
        if (datagram_len < 0) {
            ERROR("recvfrom error");
        }

        // ensure packet is valid
        if (datagram_len != 9) {
            dprintf("Invalid packet size: %zd\n", datagram_len);
            continue;
        }


        // ------ interpret packet -------
        // calculate sender address
        char sender_ip_str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));

        // drop self-originated packets
        if (interface_to_bcast.count(sender_ip_str)) {
            if (DEBUG == 2) dprintf("Dropping self-originated packet from %s\n", sender_ip_str);
            continue;
        }

        if (DEBUG == 1) {
            dprintf("Received UDP packet from IP address: %s, port: %d\n", sender_ip_str, ntohs(sender.sin_port));
        }
        
        // parse received packet
        uint32_t network_ip = *reinterpret_cast<uint32_t*>(buffer); // parse received in network byte order
        uint8_t prefix_len = buffer[4];
        uint32_t dist;
        memcpy(&dist, buffer + 5, sizeof(dist));
        dist = ntohl(dist); // Convert to host byte order

        // const string cidr_net_ip = string_format("%d.%d.%d.%d/%d",
        //             buffer[0], buffer[1], buffer[2], buffer[3], prefix_len);     
                    
        string cidr_net_ip = to_string(buffer[0]) + "." + 
                    to_string(buffer[1]) + "." + 
                    to_string(buffer[2]) + "." + 
                    to_string(buffer[3]) + "/" + 
                    to_string(prefix_len);


        if (DEBUG == 1) {
            dprintf("CIDR: %s, dist: %d\n", cidr_net_ip.c_str(), (int)dist);
        } else if (DEBUG >= 2 && DEBUG < 4) {
            dprintf("Received: %s:%d, dist: %d\n", cidr_net_ip.c_str(), ntohs(sender.sin_port), (int)dist);
        }

        // ------ update routing table ------
        // handle INF dist advertisement
        if (dist == INF) {
            // mark routes through this sender as INF
            auto it = routing_table.find(cidr_net_ip);
            if (it != routing_table.end() && 
                it->second.next_hop == sender_ip_str &&
                it->second.dist != INF) 
            {
                it->second.dist = INF;
                it->second.last_reachable.reset();
            }

            continue;
        }


        // find best path to sender
        uint8_t best_dist = INF;
        size_t best_prefix = 0;
        for (const auto& [route_cidr, entry] : routing_table) {
            if (entry.dist == INF) continue;
            
            if (get_network_ip(sender.sin_addr.s_addr, entry.prefix_len) ==
                get_network_ip(entry.ip, entry.prefix_len)) {
                if (entry.prefix_len > best_prefix) {
                    best_prefix = entry.prefix_len;
                    best_dist = entry.dist;
                }
            }
        }

        if (best_dist == INF) {
            dprintf("No valid route to sender %s\n", sender_ip_str);
            continue;
        }


        const uint8_t new_dist = (dist >= MAX_DIST - best_dist) 
                                ? INF 
                                : dist + best_dist;

        // update routing table if better route found
        auto& existing = routing_table[cidr_net_ip];
        // if (!existing.connected_directly && 
        //     (existing.dist > new_dist || existing.next_hop == sender_ip_str)) {
        //     existing.ip = network_ip;
        //     existing.prefix_len = prefix_len;
        //     existing.dist = new_dist;
        //     existing.next_hop = sender_ip_str;
        //     existing.last_reachable.reset();
        // }
        if (new_dist != INF && 
            (existing.dist > new_dist || existing.next_hop == sender_ip_str)) {
             // preserve direct route status if still valid
             bool keep_direct = existing.connected_directly && (existing.dist != INF);
             
             existing.connected_directly = keep_direct ? true : false;
             existing.ip = network_ip;
             existing.prefix_len = prefix_len;
             existing.dist = new_dist;
             existing.next_hop = keep_direct ? "" : sender_ip_str;
             existing.last_reachable.reset();
 
             if (DEBUG == 4) {
                 string status = keep_direct ? " (direct preserved)" : "";
                 dprintf("Updated %s via %s dist %d%s\n", 
                        cidr_net_ip.c_str(), sender_ip_str, new_dist, status.c_str());
             }
         }    
    }
}


//? sends routing table on broadcast address
void broadcastTable() {
    // send to every interface connected
    for (auto& [iface, iface_details] : interfaces) {
        struct sockaddr_in dest_addr;
        memset(&dest_addr, 0, sizeof(dest_addr));
        dest_addr.sin_family = AF_INET;
        dest_addr.sin_port = htons(PORT);

        if(inet_pton(AF_INET, iface.c_str(), &dest_addr.sin_addr) != 1) {
            ERROR("Invalid broadcast interface IP address");
        }

        bool send_success = true;
        // send every entry with different packet
        for (const auto& [cidr, net] : routing_table) {
            // dont send unreachable expired interfaces
            if (net.dist == INF && 
                net.connected_directly && 
                net.last_reachable.elapsed() > TIME_KEEP_UNREACHABLE) {
                continue;
            }

            // prepare packet
            uint8_t packet[9];
            *reinterpret_cast<uint32_t*>(packet) = net.ip;  //? network byte order
            packet[4] = net.prefix_len;
            uint32_t dist_network = htonl(net.dist);
            memcpy(packet + 5, &dist_network, 4);

            if (DEBUG == 1) {
                dprintf("Sending packet to %s\n", iface.c_str());
                dprintf("Packet: %u.%u.%u.%u/%d %d\n", 
                        packet[0], packet[1], packet[2], packet[3], packet[4], packet[5]);
            } else if (DEBUG >= 2 && DEBUG < 4) {
                dprintf("Sending to %s: %u.%u.%u.%u/%d %d\n", 
                        iface.c_str(), packet[0], packet[1], packet[2], packet[3], packet[4], packet[5]);
            }
                
            if (sendto(broadcast_sock, packet, sizeof(packet), 0, (struct sockaddr*)&dest_addr, sizeof(dest_addr)) != sizeof(packet)) {
                if (DEBUG == 1) dprintf("sendto error: %s\n", strerror(errno));
                send_success = false;
                break;
            }
        }

        manageIfaceStatus(iface, iface_details, send_success);
    }
}


//? removes old unreachable (excluding interfaces)
void removeUnreachable() {
    for (auto it = routing_table.begin(); it != routing_table.end();) {
        Network & net = it->second;

        if (!net.connected_directly) {
            // mark indirect routes as INF if unreachable
            if(net.dist != INF && net.last_reachable.elapsed() > TIME_TO_DIE) {
                dprintf("Marking unreachable route: %s\n", it->first.c_str());
                net.dist = INF;
                net.last_reachable.reset();
            }

            // remove unreachable routes after TIME_KEEP_UNREACHABLE
            if (net.dist == INF && net.last_reachable.elapsed() > TIME_KEEP_UNREACHABLE) {
                dprintf("Removing unreachable route: %s\n", it->first.c_str());
                it = routing_table.erase(it);
                continue;
            }
        } 

        ++it;
    }

    // add interfaces to routing table if not already present
    for (auto& [iface, iface_details] : interfaces) {
        addIfaceToRoutingTable(iface, iface_details, true);
    }
}


//? prints routing table
void printTable(ostream& os) {
    if (DEBUG) os << "----- Routing table -----\n";
    for (const auto& [cidr, net] : routing_table) {

        char ip_str[INET_ADDRSTRLEN];
        in_addr addr = {net.ip};  //? network order to presentation
        inet_ntop(AF_INET, &addr, ip_str, INET_ADDRSTRLEN);

        os << ip_str << "/" << static_cast<int>(net.prefix_len)
           << (net.dist == INF ? " unreachable" : " distance " + to_string(net.dist))
           << (net.connected_directly ? " connected directly" : " via " + net.next_hop)
           << '\n';
    }
    if (DEBUG) os << "-------------------------";
    os << '\n';
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
    case 'p':   // print routing table
        printTable(cout);
        break;
    case 't':   // print routing table
        printTable(cout);
        break;
    case 'b':   // broadcast routing table
        broadcastTable();
        break;
    case 'r':   // receive routing table
        receiveTables();
        break;
    case 'c':   // run continuously 
        return false;
    case 'q':   // quit
        cout << "Exiting..." << endl;
        exit(0);
        break;
    }

    return true;
}


//? reads config from stdin
void readStdinConfig() {
    int n; cin >> n;

    for(int i = 0; i < n; i++) {
        string cidr, trash;
        uint idist;

        cin >> cidr >> trash >> idist;

        uint32_t dist = static_cast<uint32_t>(idist);

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
        
        char broadcast_str[INET_ADDRSTRLEN];
        in_addr bc_addr = {broadcast};
        inet_ntop(AF_INET, &bc_addr, broadcast_str, INET_ADDRSTRLEN);


        routing_table[network_cidr] = {
            .ip = network_addr,
            .prefix_len = prefix_len,
            .dist = dist,
            .connected_directly = true,
            .next_hop = "",
            .iface_bcast = broadcast_str,
            .last_reachable = Timer(),
        };

        // interfaces[broadcast_str] = { true, dist, Timer(), prefix_len, network_cidr };
        interfaces[broadcast_str] = {
            .up = true,
            .dist = dist,
            .last_reachable = Timer(),
            .prefix_len = prefix_len,
            .iface_ip = network_addr,
            .iface_net = network_cidr,
        };

        interface_to_bcast[ip_str] = broadcast_str; // map interface addr to broadcast addr
    }
}


int main() {
    initSockets();

    readStdinConfig();

    // while(DEBUG && debugInteract()) {}

    runTasks();

    close(receive_sock);
    close(broadcast_sock);
}
