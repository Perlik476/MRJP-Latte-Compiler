#include <stdio.h>
#include <stdlib.h>

char* readline() {
    char* line = NULL;
    size_t len = 0;
    long read;
    read = getline(&line, &len, stdin);
    if (read != -1 && line[read - 1] == '\n')
        line[read - 1] = '\0';
    if (read == -1) {
        free(line);
        line = NULL;
        printf("Cannot read line\n");
        exit(1);
    }
    return line;
}