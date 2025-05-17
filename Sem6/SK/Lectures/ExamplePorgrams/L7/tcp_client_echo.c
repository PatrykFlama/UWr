#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


void ERROR(const char* str)
{
    fprintf(stderr, "%s: %s\n", str, strerror(errno));  // NOLINT(*-err33-c)
    exit(EXIT_FAILURE);
}


int main (int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, "usage: %s number_of_bytes\n", argv[0]);  // NOLINT(*-err33-c)
        exit(EXIT_FAILURE);
    }

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd < 0)
        ERROR("socket error");

    struct sockaddr_in server_address = { 0 };
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(32345);
    inet_pton(AF_INET, "127.0.0.1", &server_address.sin_addr);
    if (connect (sock_fd, (struct sockaddr *) &server_address, sizeof(server_address)) < 0)
        ERROR("connect error");

    long n = strtol(argv[1], NULL, 10);
    u_int8_t sending_buffer[n];
    for (int i = 0; i < n - 1; i++)
        sending_buffer[i] = (u_int8_t)('0' + i % 10);
    sending_buffer[n - 1] = (u_int8_t)('X');
    ssize_t bytes_sent = send(sock_fd, sending_buffer, (size_t)n, 0);

    printf("%ld bytes sent\n", bytes_sent);
    if (bytes_sent < n)
        ERROR("send error");        // Czy to faktycznie jest bląd?

    enum { BUFFER_SIZE = 4096 };
    u_int8_t recv_buffer[BUFFER_SIZE+1];
    ssize_t bytes_read = recv(sock_fd, recv_buffer, BUFFER_SIZE, 0);
    if (bytes_read < 0)
        ERROR("recv error");

    recv_buffer[bytes_read] = 0;
    printf("server reply: %s\n", (const char*)recv_buffer);

    if (close (sock_fd) < 0)
        ERROR("close error");
}
