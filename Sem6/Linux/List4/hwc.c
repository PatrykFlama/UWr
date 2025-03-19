#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

void print_help() {
    printf("usage: hwc [OPTIONS] [NAMES...]\n");
    printf("print greetings\n\n");
    printf("options:\n");
    printf("  -c, --capitalize          capitalize the name or 'world'\n");
    printf("  --color=MODE              set color mode (never, auto, always)\n");
    printf("  -g TEXT, --greeting=TEXT  set greeting text instead of 'Hello'\n");
    printf("  -w, --world               also print 'Hello, world!'\n");
    printf("  -h, --help                display this help message\n");
    printf("  -v, --version             display program version\n");
}

void print_version() {
    printf("hwc 1.0\n");
    printf("Copyright (C) 2025 Free Software Foundation, Inc.\
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.\
This is free software: you are free to change and redistribute it.\
There is NO WARRANTY, to the extent permitted by law.\
\
Written by me.");
}

int tty_output() {
    // https://stackoverflow.com/questions/5156675/how-to-use-isatty-on-cout-or-can-i-assume-that-cout-file-descriptor-1
    return isatty(fileno(stdout));
}

void colorize(const char *text, const char *color) {
    if (strcmp(color, "always") == 0 || (strcmp(color, "auto") == 0 && tty_output())) {
        printf("\033[1;34m%s\033[0m", text);
    } else {
        printf("%s", text);
    }
}

void greet(const char *greeting, const char *name, int capitalize, const char *color) {
    char modified_name[100];
    strcpy(modified_name, name);

    if (capitalize) {
        modified_name[0] = toupper(modified_name[0]);
        for (int i = 1; modified_name[i]; i++) {
            modified_name[i] = tolower(modified_name[i]);
        }
    }

    printf("%s, ", greeting);
    colorize(modified_name, color);
    printf("!\n");
}

int main(int argc, char *argv[]) {
    int capitalize = 0;
    int print_world = 0;
    char *greeting = "Hello";
    char *color = "auto";
    const char *version = "hwc 1.0";

    static struct option long_options[] = {
        {"capitalize", no_argument, NULL, 'c'},
        {"color", required_argument, NULL, 0},
        {"greeting", required_argument, NULL, 'g'},
        {"world", no_argument, NULL, 'w'},
        {"help", no_argument, NULL, 'h'},
        {"version", no_argument, NULL, 'v'},
        {0, 0, 0, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "cg:hvw", long_options, NULL)) != -1) {
        switch (opt) {
            case 'c':
                capitalize = 1;
                break;
            case 'g':
                greeting = optarg;
                break;
            case 'w':
                print_world = 1;
                break;
            case 'h':
                print_help();
                return 0;
            case 'v':
                print_version();
                return 0;
            case 0:
                if (strcmp(long_options[optind - 1].name, "color") == 0) {
                    color = optarg;
                }
                break;
            default:
                print_help();
                return 1;
        }
    }

    if (optind == argc && !print_world) {
        greet(greeting, "world", capitalize, color);
    }

    for (int i = optind; i < argc; i++) {
        greet(greeting, argv[i], capitalize, color);
    }

    if (print_world) {
        greet(greeting, "world", capitalize, color);
    }

    return 0;
}
