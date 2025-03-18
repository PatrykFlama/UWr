#include <bits/stdc++.h>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <unistd.h>



u_int16_t compute_icmp_checksum(const void *buff, int length)
{
    const u_int16_t* ptr = buff;
    u_int32_t sum = 0;
    assert (length % 2 == 0);
    for (; length > 0; length -= 2)
        sum += *ptr++;
    sum = (sum >> 16U) + (sum & 0xffffU);
    return (u_int16_t)(~(sum + (sum >> 16U)));
}


void ERROR(const char* str)
{
    fprintf(stderr, "%s: %s\n", str, strerror(errno));
    exit(EXIT_FAILURE);
}


void print_as_bytes (unsigned char* buff, ssize_t length)
{
    for (ssize_t i = 0; i < length; i++, buff++)
        printf("%.2x ", *buff);
}


void icmp_send(int sock_fd, struct sockaddr_in &recipient, int ttl, int seq) {
    // generate header
    struct icmp header;
    header.icmp_type = ICMP_ECHO;
    header.icmp_code = 0;
    header.icmp_hun.ih_idseq.icd_id = getpid();
    header.icmp_hun.ih_idseq.icd_seq = seq;
    header.icmp_cksum = 0;
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
}


int main(int argc, char* argv[])
{
    // ensure ip argumennt
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <destination IP>" << std::endl;
        return EXIT_FAILURE;
    }

    // generate destination ip and check if valid
    struct sockaddr_in recipient{};
    memset (&recipient, 0, sizeof(recipient));
    recipient.sin_family = AF_INET;
    if (inet_pton(AF_INET, argv[1], &recipient.sin_addr) != 1) {
        ERROR("Invalid IP address");
    }

    // create socket
    int sock_fd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sock_fd < 0)
        ERROR("socket error");

    
    for (int ttl = 1; ttl <= 30; ttl++) {
        std::vector<std::pair<int, std::string>> res; // ttl, ip

        
    }



    for (;;) {
        struct sockaddr_in sender;
        socklen_t sender_len = sizeof(sender);
        u_int8_t buffer[IP_MAXPACKET];

        ssize_t packet_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
        if (packet_len < 0)
            ERROR("recvfrom error");

        char sender_ip_str[20];
        inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
        printf("Received IP packet with ICMP content from: %s\n", sender_ip_str);

        struct ip* ip_header = (struct ip*) buffer;
        ssize_t	ip_header_len = 4 * (ssize_t)(ip_header->ip_hl);

        printf("IP header: ");
        print_as_bytes(buffer, ip_header_len);
        printf("\n");

        printf("IP data:   ");
        print_as_bytes(buffer + ip_header_len, packet_len - ip_header_len);
        printf("\n\n");
    }
}
