[(back)](../)

# Lab2
## Docs
#### `AF_INET` and `AF_INET6`
`AF_INET` is standing for IPv4, while `AF_INET6` is for IPv6


### Structs
#### `struct sockaddr`
- Base structure for all socket addresses
- Abstract, usually casted to specific types like sockaddr_in
- Required for functions like `bind()`, `recvfrom()`, `sendto()`

#### `struct sockaddr_in`
Structure for IPv4 socket addresses  

Contains:
```cpp
sa_family_t    sin_family; // Always set to AF_INET for IPv4
in_port_t      sin_port;   // Port number (in network byte order)
struct in_addr sin_addr;   // IP address
```

#### `struct in_addr`
Holds an IPv4 address:

```cpp
uint32_t s_addr; // IPv4 address (in network byte order)
```

#### `struct pollfd`
Used by poll() to monitor events (like readable data) on a file descriptor.

```cpp
int fd;       // File descriptor to monitor
short events; // Events to watch for (e.g. POLLIN for read)
short revents;// Events that occurred (set by poll())
```


### Functions
#### `inet_pton(AF_INET, "IP string", &addr)`
Convert IP string to binary (network byte order)

Example: `"192.168.0.1"` -> `addr.s_addr = 0xC0A80001`

`pton` = "presentation to numeric"


#### `inet_ntop(AF_INET, &addr, str_buf, size)`
Convert binary IP address to string

Example: `0xC0A80001` -> `"192.168.0.1"`

`ntop` = "numeric to presentation"

#### `htons(uint16_t)` / `htonl(uint32_t)`
Convert values from host byte order to network byte order

`s` = short (16-bit), `l` = long (32-bit)

Needed because network protocols use big-endian byte order

#### `ntohs(uint16_t)` / `ntohl(uint32_t)`
Opposite of above: network -> host byte order.

#### `sendto(socket_fd, buffer, len, flags, dest_addr, addr_len)`
Send a UDP packet to a specified address

Used with UDP (SOCK_DGRAM)

`dest_addr` is usually a `sockaddr_in*` casted to `(sockaddr*)`

#### `recvfrom(socket_fd, buffer, max_len, flags, src_addr, addr_len)`
Receive a UDP packet and also get the sender's address

Fills `src_addr` with sender info, useful for routing responses


#### `socket(domain, type, protocol)`
- Create a new socket
- `domain` = AF_INET (IPv4)
- `type` = SOCK_DGRAM (UDP) or SOCK_STREAM (TCP)
- `protocol` = 0 (default)

#### `bind(socket_fd, sockaddr*, size)`
Bind the socket to a specific IP address and port

Necessary for receiving data (eg listening on port 54321)

#### `setsockopt(socket_fd, level, option_name, value_ptr, value_len)`
Change socket settings (eg enable broadcasting via `SO_BROADCAST`)


#### `poll(pollfd[], nfds, timeout_ms)`
Wait for data/events on multiple file descriptors  
More efficient than `select()` for many connections  
`timeout` in ms, or `-1` to wait forever  




