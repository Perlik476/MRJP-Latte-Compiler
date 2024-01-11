#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

char* _readString() {
    char* line = NULL;
    size_t len = 0;
    long read;
    errno = 0;
    read = getline(&line, &len, stdin);
    if (read != -1 && line[read - 1] == '\n')
        line[read - 1] = '\0';
    if (read == -1) {
        if (errno == 0) {
            return NULL;
        }
        else if (errno == EINVAL) {
            perror("_readString");
            exit(EXIT_FAILURE);
        }
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
    if (str1 == NULL) {
        return str2 == NULL || strlen(str2) == 0;
    }
    else if (str2 == NULL) {
        return strlen(str1) == 0;
    }
    return strcmp(str1, str2) == 0;
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

int _readInt() {
    int result;
    scanf("%d ", &result);
    return result;
}

void _decrementReferenceCounter(void* ptr) {
    printf("decrementing reference counter\n");
    // TODO null string
    if (ptr != NULL) {
        int* refCounter = (int*)ptr;
        *refCounter = *refCounter - 1;
        if (*refCounter == 0) {
            printf("getting vtable\n");
            // get the vtable pointer (it's the second element of the struct)
            void** vtable = (void**)((int*)ptr + 1);
            printf("getting destructor\n");
            // get the first method of the vtable which is the destructor (it's type is void(void*)))
            void (*destructor)(char*) = (void(*)(char*))vtable[0];
            printf("calling destructor\n");
            // call the destructor
            destructor((char*)ptr);
            // free the memory
            free(ptr);
        }
    }
}

void _incrementReferenceCounter(void* ptr) {
    printf("incrementing reference counter\n");
    // TODO null string
    if (ptr != NULL) {
        int* refCounter = (int*)ptr;
        *refCounter = *refCounter + 1;
    }
}