#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "input.h"

int a(char *str);
int b(char *str);

int main(int argc, char *argv[]) {

    if (argc != 3) 
        return 1;

    char *input = malloc(INPUT_SIZE);

    if (giff(input, argv[2]) == -1)
        fprintf(stderr, "Error getting input! Exiting...\n");

    if (strcmp(argv[1], "-a") == 0)
        printf("Sum: %d\n", a(input));

    if (strcmp(argv[1], "-b") == 0)
        printf("Sum: %d\n", b(input));

    return 0;
}

int a(char *str) {

    int len = strlen(str);

    int i, sum = 0;
    for (i = 0; i < len; i++) {
        if (str[i] == str[(i+1) % len])
            sum += (str[i] - '0');
    }

    return sum;
}

int b(char *str) {

    int len = strlen(str);

    int i, sum = 0;
    for (i = 0; i < len; i++) {
        if (str[i] == str[(i + len/2) % len])
            sum += (str[i] - '0');
    }

    return sum;

}
