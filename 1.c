#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int a(char *str, int len);
int b(char *str, int len);

int main(int argc, char *argv[]) {

    if (argc != 3) 
        return 1;

    char input[4096];
    FILE *fp = fopen(argv[2], "r");

    int len = 0;
    int c;

    while ((c = getc(fp)) != EOF)
        input[len++] = c;

    input[len--] = '\0';    // Ignore null terminator when quoting len

    free(fp);

    if (strcmp(argv[1], "-a") == 0)
        printf("Sum: %d\n", a(input, len));

    if (strcmp(argv[1], "-b") == 0)
        printf("Sum: %d\n", b(input, len));

    return 0;
}

int a(char *str, int len) {

    int i, sum = 0;
    for (i = 0; i < len; i++) {
        if (str[i] == str[(i+1) % len])
            sum += (str[i] - '0');
    }

    return sum;
}

int b(char *str, int len) {

    int i, sum = 0;
    for (i = 0; i < len; i++) {
        if (str[i] == str[(i + len/2) % len])
            sum += (str[i] - '0');
    }

    return sum;

}
