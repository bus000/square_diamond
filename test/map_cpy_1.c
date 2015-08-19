#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i, j;
    map_t map1, map2, map3;
    size_t size = 10;
    double roughness = 0.8;

    map_init(&map1, size, roughness);
    map_square_diamond(&map1);

    map2 = map_cpy(&map1);
    map3 = map_cpy(&map2);

    if (map_shallow_cmp(&map1, &map2))
        printf("maps equal\n");
    else
        printf("maps not equal\n");

    if (map_shallow_cmp(&map2, &map3))
        printf("maps equal\n");
    else
        printf("maps not equal\n");

    if (map_shallow_cmp(&map1, &map3))
        printf("maps equal\n");
    else
        printf("maps not equal\n");

    for (i = 0; i < map1.side_len; i++)
        for (j = 0; j < map3.side_len; j++)
            if (map_get_height(&map1, i, j) != map_get_height(&map2, i, j))
                printf("(%d, %d)\n", i, j);

    for (i = 0; i < map2.side_len; i++)
        for (j = 0; j < map1.side_len; j++)
            if (map_get_height(&map2, i, j) != map_get_height(&map3, i, j))
                printf("(%d, %d)\n", i, j);

    /* Change value of map in a point. */
    map_set_height(&map1, 100, 100, map_get_height(&map1, 100, 100) == 0 ? 1 : 0);

    for (i = 0; i < map1.side_len; i++)
        for (j = 0; j < map3.side_len; j++)
            if (map_get_height(&map1, i, j) != map_get_height(&map2, i, j))
                printf("(%d, %d)\n", i, j);

    for (i = 0; i < map2.side_len; i++)
        for (j = 0; j < map1.side_len; j++)
            if (map_get_height(&map2, i, j) != map_get_height(&map1, i, j))
                printf("(%d, %d)\n", i, j);

    return EXIT_SUCCESS;
}
