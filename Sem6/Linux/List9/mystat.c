#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <getopt.h>
#include <unistd.h>

typedef struct {
    double *data;
    size_t size;
    size_t capacity;
} Samples;

int get_cpu_times(unsigned long long *total, unsigned long long *idle) {
    FILE *fp = fopen("/proc/stat", "r");
    if (!fp) {
        perror("Failed to open /proc/stat");
        return -1;
    }

    char line[1024];
    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        fprintf(stderr, "Failed to read /proc/stat\n");
        return -1;
    }
    fclose(fp);

    if (strncmp(line, "cpu ", 4) != 0) {
        fprintf(stderr, "Unexpected format in /proc/stat\n");
        return -1;
    }

    char *token = strtok(line, " ");
    unsigned long long user = 0, nice = 0, system = 0, idle_val = 0, iowait = 0;
    int field = 0;
    *total = 0;

    while ((token = strtok(NULL, " ")) != NULL) {
        unsigned long long val = strtoull(token, NULL, 10);
        *total += val;

        switch (field) {
            case 0: user = val; break;
            case 1: nice = val; break;
            case 2: system = val; break;
            case 3: idle_val = val; break;
            case 4: iowait = val; break;
            default: break;
        }
        field++;
    }

    *idle = idle_val + iowait;
    return 0;
}

int main(int argc, char *argv[]) {
    int period_seconds = 1;
    int interval_seconds = 60;
    char *logfile = "/var/log/mystat.log";

    struct option long_options[] = {
        {"period", required_argument, 0, 'p'},
        {"interval", required_argument, 0, 'i'},
        {"logfile", required_argument, 0, 'f'},
        {0, 0, 0, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "p:i:f:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'p':
                period_seconds = atoi(optarg);
                if (period_seconds <= 0) {
                    fprintf(stderr, "Period must be a positive integer\n");
                    exit(EXIT_FAILURE);
                }
                break;
            case 'i':
                interval_seconds = atoi(optarg);
                if (interval_seconds <= 0) {
                    fprintf(stderr, "Interval must be a positive integer\n");
                    exit(EXIT_FAILURE);
                }
                break;
            case 'f':
                logfile = optarg;
                break;
            default:
                fprintf(stderr, "Usage: %s [-p period] [-i interval] [-f logfile]\n", argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    FILE *logfp = fopen(logfile, "a");
    if (!logfp) {
        perror("Failed to open log file");
        exit(EXIT_FAILURE);
    }

    time_t last_log_time = time(NULL);
    time_t last_sample_time = last_log_time;
    unsigned long long prev_total = 0, prev_idle = 0;
    Samples samples = {0};

    while (1) {
        time_t now = time(NULL);

        if (now - last_sample_time >= period_seconds) {
            unsigned long long current_total, current_idle;
            if (get_cpu_times(dt_total, dt_idle) == 0) {
                if (prev_total == 0) {
                    prev_total = current_total;
                    prev_idle = current_idle;
                } else {
                    unsigned long long diff_total = current_total - prev_total;
                    unsigned long long diff_idle = current_idle - prev_idle;
                    double usage = 0.0;

                    if (diff_total > 0) {
                        usage = ((double)(diff_total - diff_idle) / diff_total) * 100.0;
                    }

                    if (samples.size >= samples.capacity) {
                        size_t new_cap = samples.capacity == 0 ? 1 : samples.capacity * 2;
                        double *new_data = realloc(samples.data, new_cap * sizeof(double));
                        if (!new_data) {
                            perror("Failed to allocate memory for samples");
                            exit(EXIT_FAILURE);
                        }
                        samples.data = new_data;
                        samples.capacity = new_cap;
                    }
                    samples.data[samples.size++] = usage;
                    prev_total = current_total;
                    prev_idle = current_idle;
                }
                last_sample_time = now;
            } else {
                fprintf(stderr, "Error reading CPU times, skipping sample\n");
            }
        }

        if (now - last_log_time >= interval_seconds) {
            if (samples.size > 0) {
                double sum = 0.0, min = samples.data[0], max = samples.data[0];
                for (size_t i = 0; i < samples.size; i++) {
                    sum += samples.data[i];
                    if (samples.data[i] < min) min = samples.data[i];
                    if (samples.data[i] > max) max = samples.data[i];
                }
                double avg = sum / samples.size;

                time_t log_time = time(NULL);
                struct tm *tm_info = localtime(&log_time);
                char timestamp[20];
                strftime(timestamp, sizeof(timestamp), "%Y-%m-%d %H:%M:%S", tm_info);

                fprintf(logfp, "[%s] CPU Usage: Avg=%.2f%%, Min=%.2f%%, Max=%.2f%%\n",
                        timestamp, avg, min, max);
                fflush(logfp);

                free(samples.data);
                samples.data = NULL;
                samples.size = 0;
                samples.capacity = 0;
            }
            last_log_time = now;
        }

        time_t next_sample = last_sample_time + period_seconds;
        time_t next_log_check = last_log_time + interval_seconds;
        time_t next_event = (next_sample < next_log_check) ? next_sample : next_log_check;
        time_t now_again = time(NULL);

        if (now_again < next_event) {
            sleep(next_event - now_again);
        }
    }

    fclose(logfp);
    return 0;
}