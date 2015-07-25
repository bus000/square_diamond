#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i, j, next = 0;
    map_t m;
    size_t side_len =  5;
    double roughness = 0.5;

    map_init(&m, side_len, roughness);

    for (i = 0; i < m.side_len; i++) {
        for (j = 0; j < m.side_len; j++) {
            map_set_height(&m, i, j, next);
            printf("%d\n", map_get_height(&m, i, j));
            next += 1;
        }
    }

    exit(EXIT_SUCCESS);
}
