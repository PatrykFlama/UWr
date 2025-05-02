#include <arpa/inet.h>
#include <errno.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#define MAX_CHUNK_SIZE 1000
#define WINDOW_SIZE 1000
#define TIMEOUT_MS 2000
#define MAX_REQUEST_SIZE 128
#define MAX_UDP_PACKET_SIZE 65536

struct WindowChunk {
    bool sent;
    bool acked;
    unsigned char data[MAX_CHUNK_SIZE];
    struct timeval last_sent_time;
};

void ERROR(const char* str) {
    fprintf(stderr, "%s: %s\n", str, strerror(errno));  // NOLINT(*-err33-c)
    exit(EXIT_FAILURE);
}

long time_diff_ms(struct timeval *start, struct timeval *end) {
    long sec_diff = end->tv_sec - start->tv_sec;
    long usec_diff = end->tv_usec - start->tv_usec;
    return sec_diff * 1000 + usec_diff / 1000;
}

int main(int argc, char *argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Usage: %s <IP> <port> <filename> <size>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    char *ip = argv[1];
    int port = atoi(argv[2]);
    char *filename = argv[3];
    long size = atol(argv[4]);

    if (size <= 0) {
        fprintf(stderr, "Invalid size. Size must be a positive integer.\n");
        exit(EXIT_FAILURE);
    }

    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        ERROR("socket");
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    if (inet_pton(AF_INET, ip, &server_addr.sin_addr) <= 0) {
        ERROR("inet_pton");
    }

    FILE *output_file = fopen(filename, "wb");
    if (!output_file) {
        ERROR("fopen");
    }

    int num_chunks = (size + MAX_CHUNK_SIZE - 1) / MAX_CHUNK_SIZE;
    struct WindowChunk *window = (struct WindowChunk *)calloc(WINDOW_SIZE, sizeof(struct WindowChunk));
    if (!window) {
        ERROR("calloc");
    }

    int current_start = 0;

    while (current_start < num_chunks) {
        int window_end = current_start + WINDOW_SIZE;
        if (window_end > num_chunks) {
            window_end = num_chunks;
        }
        int chunks_in_window = window_end - current_start;

        struct timeval now;
        gettimeofday(&now, NULL);

        for (int i = 0; i < chunks_in_window; i++) {
            struct WindowChunk *chunk = &window[i];
            int global_index = current_start + i;
            if (global_index >= num_chunks) {
                break;
            }

            long start = (long)global_index * MAX_CHUNK_SIZE;
            int length = (global_index == num_chunks - 1) ? (int)(size - start) : MAX_CHUNK_SIZE;

            if (!chunk->sent || (!chunk->acked && time_diff_ms(&chunk->last_sent_time, &now) > TIMEOUT_MS)) {
                char request[MAX_REQUEST_SIZE];
                int req_len = snprintf(request, sizeof(request), "GET %ld %d\n", start, length);
                if (sendto(sockfd, request, req_len, 0, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
                    perror("sendto");
                }
                chunk->sent = true;
                gettimeofday(&chunk->last_sent_time, NULL);
            }
        }

        struct pollfd fds[1];
        fds[0].fd = sockfd;
        fds[0].events = POLLIN;
        int timeout = TIMEOUT_MS;
        int ret = poll(fds, 1, timeout);

        if (ret < 0) {
            ERROR("poll");
        }

        if (ret > 0) {
            unsigned char buffer[MAX_UDP_PACKET_SIZE];
            ssize_t recv_len = recvfrom(sockfd, buffer, sizeof(buffer), 0, NULL, NULL);
            if (recv_len < 0) {
                perror("recvfrom");
                continue;
            }

            char *header_end = (char *)memchr(buffer, '\n', recv_len);
            if (!header_end) {
                continue;
            }
            *header_end = '\0';

            long received_start;
            int received_length;
            if (sscanf((char *)buffer, "DATA %ld %d", &received_start, &received_length) != 2) {
                continue;
            }

            int global_index = (int)(received_start / MAX_CHUNK_SIZE);
            if (global_index < current_start || global_index >= current_start + chunks_in_window) {
                continue;
            }

            int expected_length;
            if (global_index == num_chunks - 1) {
                expected_length = (int)(size - (long)global_index * MAX_CHUNK_SIZE);
            } else {
                expected_length = MAX_CHUNK_SIZE;
            }
            if (expected_length != received_length) {
                continue;
            }

            int window_index = global_index - current_start;
            struct WindowChunk *chunk = &window[window_index];
            if (!chunk->acked) {
                char *data_part = (char *)(header_end + 1);
                size_t data_len = recv_len - (data_part - (char *)buffer);
                if ((int)data_len == received_length) {
                    memcpy(chunk->data, data_part, data_len);
                    chunk->acked = true;
                }
            }
        }

        while (current_start < num_chunks) {
            struct WindowChunk *chunk = &window[0];
            if (chunk->acked) {
                int global_index = current_start;
                long start = (long)global_index * MAX_CHUNK_SIZE;
                int length = (global_index == num_chunks - 1) ? (int)(size - start) : MAX_CHUNK_SIZE;
                if ((int)fwrite(chunk->data, 1, length, output_file) != length) {
                    ERROR("fwrite");
                }
                current_start++;

                memmove(&window[0], &window[1], (WINDOW_SIZE - 1) * sizeof(struct WindowChunk));
                if (current_start + WINDOW_SIZE - 1 < num_chunks) {
                    window[WINDOW_SIZE - 1].sent = false;
                    window[WINDOW_SIZE - 1].acked = false;
                    memset(window[WINDOW_SIZE - 1].data, 0, MAX_CHUNK_SIZE);
                }
            } else {
                break;
            }
        }
    }

    free(window);
    fclose(output_file);
    close(sockfd);
}