#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "input.h"

int a(int i, int j, int nums[i][j]);
int b(int i, int j, int n[i][j]);

int main(int argc, char *argv[]) {

    if (argc != 3)
        return 1;

    char *input = malloc(INPUT_SIZE);
    
    if (giff(input, argv[2]) == -1)
        fprintf(stderr, "Error getting input! Exiting...\n");

    int i = 0;
    int rows = 0, cols = 0, max_cols = 0;
    int len = strlen(input);

    while (i < len) {

        if (isspace(input[i])) {
            cols++;
            max_cols = (cols > max_cols) ? cols : max_cols;
        }

        if (input[i] == '\n' || i == len - 1) {
            rows++;
            cols = 0;
        }

        i++;
    }

    cols = max_cols;

    int nums[rows][cols];
    for (int a = 0; a < rows; a++)
        for (int b = 0; b < cols; b++)
            nums[a][b] = 0;

    int numsum = 0;

    int j, k;
    i = j = k = 0;

    while (i < len) {

        if (!isspace(input[i])) {

            numsum = numsum * 10 + input[i] - '0';

            if (i == len - 1)
                nums[j][k] = numsum;

        } else {

            nums[j][k] = numsum;
            numsum = 0;

            if (input[i] != '\n') {
                k++;
            } else {
                j++;
                k = 0;
            }

        }

        i++;

    }

    if (strcmp(argv[1], "-a") == 0)
        printf("Checksum: %d\n", a(rows, cols, nums));
    if (strcmp(argv[1], "-b") == 0)
        printf("Checksum: %d\n", b(rows, cols, nums));

    return 0;
}

int a(int i, int j, int nums[i][j]) {

    int min, max, checksum = 0;

    for (int a = 0; a < i; a++) {

        for (int b = 0; b < j; b++) {

            if (b == 0)
                min = max = nums[a][b];

            if (nums[a][b] < min && nums[a][b] > 0)
                min = nums[a][b];

            if (nums[a][b] > max)
                max = nums[a][b];

        }

        checksum += (max - min);
    }

    return checksum;
}

int b(int i, int j, int nums[i][j]) {

    int checksum = 0;
    for (int a = 0; a < i; a++) {

        for (int b = 0; b < j; b++) {

            for (int c = 0; c < j; c++) {

                if (nums[a][b] % nums[a][c] == 0 && b != c) {
                    checksum += (nums[a][b] / nums[a][c]);
                    continue;
                }

            }

        }

    }

    return checksum;
}
