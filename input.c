#include "input.h"

#include <stdio.h>
#include <stdlib.h>

int giff(char *input, char *fname) {

    FILE *fp; 
    if ((fp = fopen(fname, "r")) == NULL)
        return -1;

    int len = 0;
    int c;

    while ((c = getc(fp)) != EOF)
        input[len++] = c;

    input[--len] = '\0';    // Ignore newline character at end of file

    free(fp);
    return 0;
}
