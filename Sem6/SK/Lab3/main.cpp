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

#define DEBUG 1
#define dprintf if(DEBUG) printf

//* ------------------ CONSTANTS ------------------
constexpr int MAX_CHUNK_SIZE = 1000;
constexpr int WINDOW_SIZE = 1000;
constexpr int TIMEOUT_MS = 2000;


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
//* -------------- GLOBALS  ---------------
ofstream out_file;
int sock_fd;
struct sockaddr_in server_address;

//* -------------- STRUCTURES ---------------
struct Chunk {
    bool sent;
    bool acked;
    vector<char> data;
    Timer last_sent;

    Chunk() : sent(false), acked(false), data(MAX_CHUNK_SIZE) {}
    Chunk(int size) : sent(false), acked(false), data(size) {}

    void write_to_file() {
        out_file.write(data.data(), data.size());
    }

    void reset() {
        acked = false;
        sent = false;
        last_sent.reset();
        data.clear();
    }
};

struct Window {
    const int n = WINDOW_SIZE;
    int ptr;
    int n_chunks;
    vector<Chunk> data;

    Window(int n_chunks) : ptr(0), n_chunks(n_chunks), data(WINDOW_SIZE) {}


    void send_request_chunk(int index) {
        const int ptr_relative = (ptr + index) % n;
        dprintf("request for %d sent \n", ptr_relative);

        long start = (ptr + index) * MAX_CHUNK_SIZE;
        int length = (ptr + index == n_chunks - 1) ? (n_chunks * MAX_CHUNK_SIZE - start) : MAX_CHUNK_SIZE;
        string request = "DATA " + to_string(start) + " " + to_string(length) + "\n";

        if (sendto(sock_fd, request.c_str(), request.size(), 0, (struct sockaddr*)&server_address, sizeof(server_address)) != (long int)request.size())
            ERROR("sendto error");

        data[ptr_relative].last_sent.reset();
        data[ptr_relative].sent = true;
    }

    void send_request_all() {
        for (int i = 0; i < n; ++i) {
            if (ptr + i >= n_chunks) break;

            const int ptr_relative = (ptr + i) % n;
            if (data[i].acked || (data[i].sent && data[i].last_sent.elapsed() <= TIMEOUT_MS)) continue;

            dprintf("Sending request for chunk %d ...", ptr_relative);
            send_request_chunk(i);
        }
    }


    void receive() {
        pollfd pfd = {sock_fd, POLLIN, 0};
        int ret = poll(&pfd, 1, TIMEOUT_MS);
        if (ret < 0) ERROR("poll error");
        if (ret == 0) return; // timeout

        char buffer[MAX_CHUNK_SIZE + 1024];
        sockaddr_in from;
        socklen_t from_len = sizeof(from);
        ssize_t len = recvfrom(sock_fd, buffer, sizeof(buffer), 0, (sockaddr*)&from, &from_len);
        if (len < 0) ERROR("recvfrom error");

        string header(buffer, strcspn(buffer, "\n"));
        size_t space1 = header.find(' ');
        size_t space2 = header.rfind(' ');
        if (space1 == string::npos || space2 == string::npos || space1 == space2) return;

        string cmd = header.substr(0, space1);
        int start = stoi(header.substr(space1 + 1, space2 - space1 - 1));
        int length = stoi(header.substr(space2 + 1));

        if (cmd != "DATA" || start < 0 || length <= 0 || start + length > n_chunks * MAX_CHUNK_SIZE)
            return;

        int chunk_index = start / MAX_CHUNK_SIZE;
        if (chunk_index < ptr || chunk_index >= ptr + n) return;

        const int ptr_relative = (ptr + chunk_index) % n;
        
        data[ptr_relative].data.resize(length);
        memcpy(data[ptr_relative].data.data(), buffer + header.size() + 1, length);
        
        data[ptr_relative].acked = true;
        
        dprintf("Received chunk %d\n", chunk_index);
        dprintf("Data: %s\n", string(data[ptr_relative].data.begin(), data[ptr_relative].data.end()).c_str());
    }

    
    void write_received() {
        int offset = 0;

        for (; offset < n; ++offset) {
            const int ptr_relative = (ptr + offset) % n;

            if (ptr >= n_chunks || !data[ptr_relative].acked)
                break;
            
            data[ptr_relative].write_to_file();
            data[ptr_relative].reset();
        }

        ptr = (ptr + offset) % n;
    }
};


//* -------------- HELPERS -----------------


//* -------------- MAIN FUNCTIONS -----------------


int main(int argc, char* argv[]) {
    if (argc != 5) {
        cerr << "Usage: " << argv[0] << " <IP> <port> <filename> <size>" << '\n';
        return EXIT_FAILURE;
    }

    string ip = argv[1];
    int port = stoi(argv[2]);
    string filename = argv[3];
    long file_size = stol(argv[4]);

    // open file for writing
    out_file.open(filename, ios::binary);
    if (!out_file) {
        cerr << "Failed to open file: " << filename << '\n';
        return EXIT_FAILURE;
    }

    // create socket
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0)
        ERROR("socket error");

    // set socket options
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    if (inet_pton(AF_INET, ip.c_str(), &server_address.sin_addr) <= 0)
        ERROR("inet_pton error");

    // create window
    int total_chunks = (file_size + MAX_CHUNK_SIZE - 1) / MAX_CHUNK_SIZE;
    Window window(total_chunks);

    
    while (window.ptr < total_chunks) {
        window.send_request_all();

        window.receive();

        window.write_received();
    }
}
