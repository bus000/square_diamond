#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    map_t m1, m2, m3;
    size_t side_len =  5; /* Side len is 2^5 + 1 = 33. */
    double roughness = 0.5;

    map_init(&m1, side_len, roughness);
    map_init(&m2, side_len, roughness);
    map_init(&m3, side_len, roughness);

    if (map_shallow_cmp(&m1, &m2))
        printf("m1 and m2 equal\n");
    else
        printf("m1 and m2 not equal\n");

    m3.side_len = 1134;

    if (map_shallow_cmp(&m1, &m3))
        printf("m1 and m3 equal\n");
    else
        printf("m1 and m3 not equal\n");

    m3.side_len = m2.side_len;
    m3.water_height = 18181;

    if (map_shallow_cmp(&m2, &m3))
        printf("m2 and m3 equal\n");
    else
        printf("m2 and m3 not equal\n");

    m3.water_height = m2.water_height;
    m3.max = 9919;

    if (map_shallow_cmp(&m2, &m3))
        printf("m2 and m3 equal\n");
    else
        printf("m2 and m3 not equal\n");

    m3.max = m2.max;
    m3.roughness = 999.011;

    if (map_shallow_cmp(&m1, &m3))
        printf("m1 and m3 equal\n");
    else
        printf("m1 and m3 not equal\n");

    m3.roughness = m2.roughness;

    if (map_shallow_cmp(&m1, &m3))
        printf("m1 and m3 equal\n");
    else
        printf("m1 and m3 not equal\n");

    return EXIT_SUCCESS;
}
