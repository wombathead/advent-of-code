#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int iterators[] = {1,3,5,7};

int main(int argc, char *argv[]) {

    if (argc != 3)
        return -1;

    int n = atoi(argv[2]);

    int a = (int) ceil(sqrt(n));

    if (a % 2 == 0)
        a++;

    int l = a / 2;

    int dist_min, dist_curr;
    int i;

    for (i = 0; i < 4; i++) {
        
        dist_curr = abs(n - (a * a - iterators[i] * l));
        if (i == 0)
            dist_min = dist_curr;
        else
            dist_min = (dist_curr < dist_min) ? dist_curr : dist_min;

    }

    printf("Distance: %d\n", dist_min + l);
    return 0;
}
