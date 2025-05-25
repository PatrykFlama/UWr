#include <bits/stdc++.h>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>
#include <poll.h>
#include <chrono>
#include <thread>
#include <dirent.h>


using namespace std;

constexpr int DEBUG = 4;
#define dprintf if(DEBUG) printf
#define ndprintf(...) if (([](){ int arr[] = {__VA_ARGS__}; for (int lvl : arr) if (DEBUG == lvl) return true; return false; })()) printf

//* ------------------ CONSTANTS ------------------
constexpr int CONNECTION_TIMEOUT = 500; // ms
constexpr int BUFFER_SIZE = 2048;
constexpr int MAX_CONNECTIONS = 100;

unordered_map<string, string> mime_types = {
    {"txt", "text/plain; charset=utf-8"},
    {"html", "text/html; charset=utf-8"},
    {"css", "text/css; charset=utf-8"},
    {"jpg", "image/jpeg"},
    {"jpeg", "image/jpeg"},
    {"png", "image/png"},
    {"pdf", "application/pdf"},
    {"default", "application/octet-stream"}
};

unordered_map<int, string> status_messages = {
    {200, "OK"},
    {301, "Moved Permanently"},
    {403, "Forbidden"},
    {404, "Not Found"},
    {500, "Internal Server Error"},
    {501, "Not Implemented"}
};


//* ------------- HELPER -----------------
void ERROR(const char* str) {
    fprintf(stderr, "%s: %s\n", str, strerror(errno));  // NOLINT(*-err33-c)
    exit(EXIT_FAILURE);
}
void WARN(const char* str) {
    fprintf(stderr, "%s: %s\n", str, strerror(errno));
}

// --- other helpers ---
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


//* ================ FUNCTIONS ===================
string url_to_text(const string& url) {
    string result;
    for (size_t i = 0; i < url.size(); ++i) {
        if (url[i] == '%' && i + 2 < url.size()) {
            int value;
            istringstream iss(url.substr(i + 1, 2));
            iss >> std::hex >> value;
            result += static_cast<char>(value);
            i += 2;
        } else if (url[i] == '+') {
            result += ' ';
        } else {
            result += url[i];
        }
    }
    return result;
}

string get_ext(const string& path) {
    size_t dot_pos = path.rfind('.');
    if (dot_pos == string::npos) return mime_types["default"];
    
    string ext = path.substr(dot_pos + 1);
    transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
    
    return mime_types.count(ext) ? mime_types[ext] : mime_types["default"];
}

string find_path(const string& base_dir, const string& host, const string& req_path) {
    string decoded_path = url_to_text(req_path);
    string full_path = base_dir + "/" + host + decoded_path;

    ndprintf(4)("Raw path: %s | Decoded path: %s\n", req_path.c_str(), decoded_path.c_str());
    
    // ensure there is no escape from base directory
    if (full_path.find("..") != string::npos || 
        full_path.find("~") != string::npos) {
        return "";
    }

    return full_path;
}


//* ================ MAIN FUNCTIONS ===================
void send_response(int client_fd, int code, 
                  const string& content_type = "", const string& body = "",
                  const map<string, string>& headers = {}) {
    const string& message = status_messages.count(code) ? status_messages[code] : "Unknown Error";

    ostringstream response;
    response << "HTTP/1.1 " << code << " " << message << "\r\n";
    response << "Content-Type: " << content_type << "\r\n";
    response << "Content-Length: " << body.size() << "\r\n";
    
    for (const auto& [key, value] : headers) {
        response << key << ": " << value << "\r\n";
    }
    
    response << "Connection: keep-alive\r\n\r\n";
    response << body;
    
    write(client_fd, response.str().c_str(), response.str().size());
}
void send_error_response(int client_fd, int code,
                         const map<string, string>& headers = {}) {

    const string& message = status_messages.count(code) ? status_messages[code] : "Unknown Error";
    send_response(client_fd, code, "text/html;charset=utf-8", 
                  "<html><body><h1>" + message + "</h1></body></html>",
                  headers);
}

//? true = keep alive, false = close connection
bool handle_request(int client_fd, const string& base_dir) {
    char buffer[BUFFER_SIZE];
    ssize_t bytes_read = read(client_fd, buffer, BUFFER_SIZE - 1);
    if (bytes_read <= 0) return false;

    buffer[bytes_read] = '\0';
    istringstream request(buffer);
    string method, path, version;
    request >> method >> path >> version;

    string line;
    getline(request, line);

    ndprintf(1)("%s", buffer);
    ndprintf(2)("Method: %s, Path: %s, Version: %s\n", method.c_str(), path.c_str(), version.c_str());

    // not implemented methods
    if (method != "GET") {
        send_error_response(client_fd, 501);
        return false;
    }

    // parse header
    string host;
    string connection = "keep-alive";
    while (getline(request, line)) {
        if (line.find("Host:") == 0) {
            host = line.substr(6);
            if (host.find(':') != string::npos) {
                host = host.substr(0, host.find(':'));
            }
        }
        else if (line.find("Connection:") == 0) connection = line.substr(12);
        else if (line == "\r" || line.empty()) break;
    }

    if (host.empty() || path.empty()) {
        ndprintf(4)("Invalid req: missing Host/Path\n");
        send_error_response(client_fd, 501);
        return connection == "keep-alive";
    }

    string full_path = find_path(base_dir, host, path);

    ndprintf(2, 3)("Base directory: %s, Host: %s\nResolved path: %s\n",
                base_dir.c_str(), host.c_str(), full_path.c_str());

    // if path was invalid, then full_path will be empty
    if (full_path.empty()) {
        ndprintf(4)("Invalid path: %s %s\n", full_path.c_str(), path.c_str());
        send_error_response(client_fd, 403);
        return connection == "keep-alive";
    }

    // check if file exists and is accessible
    struct stat st; // file info
    if (stat(full_path.c_str(), &st) != 0) {
        ndprintf(4)("File not found: %s\n", full_path.c_str());
        send_error_response(client_fd, 404);
        return connection == "keep-alive";
    }

    if (S_ISDIR(st.st_mode)) {
        ndprintf(4)("Path is directory: %s\n", full_path.c_str());
        // Redirect to index.html in the directory
        if (path.back() != '/') path += '/';
        path += "index.html";
        ndprintf(4)("Redirect to directory: %s\n", (path).c_str());
        send_error_response(client_fd, 301, {{"Location", path}});
        return connection == "keep-alive";
    }

    ifstream file(full_path, ios::binary);
    if (!file) {
        ndprintf(4)("Failed to open file: %s\n", full_path.c_str());
        send_error_response(client_fd, 500);
        return connection == "keep-alive";
    }

    string content((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    send_response(client_fd, 200, get_ext(full_path), content);

    return connection == "keep-alive";
}




int main(int argc, char* argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <port> <directory>\n";
        return EXIT_FAILURE;
    }

    string base_dir = argv[2];
    DIR* dir = opendir(base_dir.c_str());
    if (!dir) ERROR("Invalid dir");
    closedir(dir);

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) ERROR("socket");

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    sockaddr_in address{};
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(stoi(argv[1]));

    if (bind(server_fd, (sockaddr*)&address, sizeof(address)) < 0)
        ERROR("bind");

    if (listen(server_fd, MAX_CONNECTIONS) < 0) ERROR("listen");

    printf("Server started on port %s\n", argv[1]);

    while (true) {
        sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int client_fd = accept(server_fd, (sockaddr*)&client_addr, &client_len);
        if (client_fd < 0) {
            WARN("accept");
            continue;
        }

        // serve only one connection at a time (until closed)
        pollfd pfd = {client_fd, POLLIN, 0};
        while (true) {
            int ret = poll(&pfd, 1, CONNECTION_TIMEOUT);
            if (ret == 0) break;
            if (ret < 0) {
                WARN("poll");
                break;
            }

            const bool connection_action = handle_request(client_fd, base_dir);
            if (!connection_action) {
                ndprintf(3)("Connection force-sloced\n");
                break;
            }
        }

        close(client_fd);
    }

    close(server_fd);
}
