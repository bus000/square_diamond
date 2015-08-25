#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i, j;
    map_t map;
    int res;

    map_init(&map, 7, 0.9);

    res = map_set_water_height(&map, -2);
    printf("%d %d\n", res, map.water_height);

    res = map_set_water_height(&map, map.max + 1);
    printf("%d %d\n", res, map.water_height);

    res = map_set_water_height(&map, 40);
    printf("%d %d\n", res, map.water_height);

    for (i = 0; i < map.side_len; i++)
        for (j = 0; j < map.side_len; j++)
            if (!map_is_water(&map, i, j) && map_get_height(&map, i, j) <= 40)
                printf("err");

    return EXIT_SUCCESS;
}
