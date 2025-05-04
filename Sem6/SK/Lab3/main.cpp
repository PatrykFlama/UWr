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

//* ------------------ CONSTANTS ------------------
constexpr int MAX_CHUNK_SIZE = 1000;
constexpr int WINDOW_SIZE = 1000;
constexpr int TIMEOUT_MS = 100;


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
    bool sent = false;
    bool acked = false;
    vector<char> data;
    Timer last_sent;

    Chunk() : data(MAX_CHUNK_SIZE) {}
    Chunk(int size) : sent(false), acked(false), data(size) {}

    void reset() {
        acked = false;
        sent = false;
        last_sent.reset();
        data.clear();
    }
};

struct Window {
    int ptr;
    int total_chunks;
    int file_size;
    int received_chunks = 0;
    vector<Chunk> data;

    Window(int file_size) : ptr(0), file_size(file_size), data(WINDOW_SIZE) {
        total_chunks = (file_size + MAX_CHUNK_SIZE - 1) / MAX_CHUNK_SIZE;
    }


    void send_request_chunk(int chunk) {
        long start = chunk * MAX_CHUNK_SIZE;
        int length = min(MAX_CHUNK_SIZE, static_cast<int>(file_size - start));
        string request = "GET " + to_string(start) + " " + to_string(length) + "\n";

        // int data_index = (chunk - ptr) % WINDOW_SIZE;
        int data_index = chunk % WINDOW_SIZE;
        if (sendto(sock_fd, request.c_str(), request.size(), 0, 
                  (struct sockaddr*)&server_address, sizeof(server_address)) < 0)
            ERROR("sendto error");

        data[data_index].last_sent.reset();
        data[data_index].sent = true;
        dprintf("Sent request for chunk %d\n", chunk);
    }

    void send_request_all() {
        const int window_end = min(total_chunks, ptr + WINDOW_SIZE);
        for (int chunk = ptr; chunk < window_end; ++chunk) {
            int idx = chunk % WINDOW_SIZE;

            if (data[idx].acked) continue;
            if (data[idx].sent && 
                data[idx].last_sent.elapsed() < TIMEOUT_MS) continue;

            send_request_chunk(chunk);
        }
    }

    void receive() {
        pollfd pfd = {sock_fd, POLLIN, 0};
        if (poll(&pfd, 1, TIMEOUT_MS) <= 0) return;

        char buffer[MAX_CHUNK_SIZE + 1024];
        sockaddr_in from;
        socklen_t from_len = sizeof(from);
        ssize_t len = recvfrom(sock_fd, buffer, sizeof(buffer), 0, 
                             (sockaddr*)&from, &from_len);
        if (len <= 0) return;

        char* header_end = strstr(buffer, "\n");
        if (!header_end) return;
        string header(buffer, header_end - buffer);

        size_t space1 = header.find(' ');
        size_t space2 = header.rfind(' ');
        if (space1 == string::npos || space2 == string::npos || space1 == space2) 
            return;

        string cmd = header.substr(0, space1);
        int start = stoi(header.substr(space1+1, space2-space1-1));
        int length = stoi(header.substr(space2+1));

        if (cmd != "DATA" || start < 0 || length <= 0 || 
            start % MAX_CHUNK_SIZE != 0 ||
            start + length > file_size)
            return;

        int chunk_index = start / MAX_CHUNK_SIZE;
        if (chunk_index < ptr || chunk_index >= ptr + WINDOW_SIZE || 
            chunk_index >= total_chunks)
            return;

        int data_index = chunk_index % WINDOW_SIZE;
        size_t data_len = len - (header_end - buffer + 1);
        if (data_len < static_cast<size_t>(length)) return;

        if (data[data_index].acked) return;
        data[data_index].data.assign(header_end+1, header_end+1+length);
        data[data_index].acked = true;
        received_chunks++;
        dprintf("Received chunk %d\n", chunk_index);
    }

    void write_received() {
        dprintf("Writing received data:");

        while (ptr < total_chunks) {
            dprintf(" %d", ptr);

            int data_index = ptr % WINDOW_SIZE;
            if (!data[data_index].acked) {
                dprintf(" nah");
                break;
            }
            
            out_file.write(data[data_index].data.data(), 
                           data[data_index].data.size());
            data[data_index].reset();
            ptr++;
        }
        dprintf(" (%s, %d)\n", ptr >= total_chunks ? "done" : "not done", ptr);
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

    // main loop
    Window window(file_size);
    int percent = 0;
    const int percent_resolution = 5;
    
    while (window.ptr < window.total_chunks) {
        dprintf("Current base: %d\n", window.ptr);

        window.send_request_all();

        Timer timer;
        while (window.ptr < window.total_chunks && 
               timer.elapsed() < TIMEOUT_MS) {
            window.receive();
        }

        window.write_received();

        const int new_percent = ((100 * window.received_chunks) / window.total_chunks);
        if (new_percent/percent_resolution > percent/percent_resolution) {
            percent = new_percent;
            cout << "\rProgress: " << percent << "% " << endl;
        }
    }

    close(sock_fd);
    out_file.close();
}
