#include <stdio.h>
#include "map.h"

int main(int argc, char const *argv[])
{
    map_t map;
    int length = 1000;
    int width = 1000;

    map_init(&map, length, width, 100);
    map_square_diamond(&map);
    save_as_png(&map, "out.png");

    return 0;
}
