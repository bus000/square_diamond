#include <stdio.h>
#include <stdlib.h>
#include "map.h"

int main(int argc, char const *argv[])
{
    int height = 10;
    int width = 10;
    int i, j;
    map m;

    map_init(&m, width, height, 20);
    map_square_diamond(&m);

    for (i = 0; i < width; i++)
        for (j = 0; j < height; j++)
            printf("(%d, %d, %d)\n", i, j, map_get_height(&m, i, j));

    return 0;
}
