#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i, j;
    int map_height;
    map_t m;
    size_t side_len = 10;
    double roughness = 0.7;

    map_init(&m, side_len, roughness);
    map_square_diamond(&m);

    for (i = 0; i < m.side_len; i++) {
        for (j = 0; j < m.side_len; j++) {
            map_height = map_get_height(&m, i, j);
            if (map_height < 0 || map_height > m.max)
                printf("err on (%d, %d) = %d\n", i, j, map_height);
        }
    }

    m.roughness = 0.1;
    map_square_diamond(&m);

    for (i = 0; i < m.side_len; i++) {
        for (j = 0; j < m.side_len; j++) {
            map_height = map_get_height(&m, i, j);
            if (map_height < 0 || map_height > m.max)
                printf("err on (%d, %d) = %d\n", i, j, map_height);
        }
    }

    m.roughness = 0.5;
    map_square_diamond(&m);

    for (i = 0; i < m.side_len; i++) {
        for (j = 0; j < m.side_len; j++) {
            map_height = map_get_height(&m, i, j);
            if (map_height < 0 || map_height > m.max)
                printf("err on (%d, %d) = %d\n", i, j, map_height);
        }
    }

    return EXIT_SUCCESS;
}
