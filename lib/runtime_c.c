#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* _readString() {
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

void _printString(char* str) {
    if (str == NULL) {
        printf("\n");
    }
    else {
        printf("%s\n", str);
    }
}

int _compareStrings(char* str1, char* str2) {
    if (str1 == NULL || str2 == NULL) {
        return str1 == str2;
    }
    return strcmp(str1, str2);
}

char* _concatStrings(char* str1, char* str2) {
    if (str1 == NULL) {
        return str2;
    }
    else if (str2 == NULL) {
        return str1;
    }
    char* result = malloc(strlen(str1) + strlen(str2) + 1);
    strcpy(result, str1);
    strcat(result, str2);
    return result;
}