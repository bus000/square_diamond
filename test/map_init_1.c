#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i, j, tmp = 0;
    map_t m;
    size_t side_len =  5; /* Side len is 2^5 + 1 = 33. */
    double roughness = 0.5;

    map_init(&m, side_len, roughness);

    printf("side len %zu\n", m.side_len);
    printf("roughness %f\n", m.roughness);
    printf("water height %d\n", m.water_height);
    printf("max %zu\n", m.max);

    /* Loop through all of map to test for segfaults. */
    for (i = 0; i < m.side_len; i++)
        for (j = 0; j < m.side_len; j++)
            tmp += m.height[i][j];

    return tmp - tmp + EXIT_SUCCESS;
}
