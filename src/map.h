#ifndef MAP_H
#define MAP_H

#include <stdlib.h>
#include <stdio.h>

typedef struct {
    unsigned int **height; /* Array of the heights of points in the plane. */
    size_t side_len;       /* Internal width and height of map. */
    int water_height;      /* The height of any water on the map. */
    size_t max;            /* The maximum height on the map. */
    double roughness;      /* Between 0 and 1, change the slope. */
} map_t;

#define LOWER_LEFT(m) (m->height[0][0])
#define UPPER_LEFT(m) (m->height[0][m->side_len-1])
#define LOWER_RIGHT(m) (m->height[m->side_len-1][0])
#define UPPER_RIGHT(m) (m->height[m->side_len-1][m->side_len-1])

/* Initializes the map, the higher the random range, the more hilly the terrain
 * will be. */
int map_init(map_t *m, size_t size, double roughness);

/* Returns the height of the map in a specific point. */
int map_get_height(map_t const *m, int x, int y);

/* Set the height of the map in a specific point. */
void map_set_height(map_t *m, int x, int y, int value);

/* Set a height for water return non 0 on error. */
int map_set_water_height(map_t *m, int new_height);

/* Run the square diamond algorithm on the map to generate a terrain. */
void map_square_diamond(map_t *m);

/* Save the map as a png image. */
int map_save_as_png(map_t const *m, char const *filename, size_t height,
        size_t width);

/* Compare two map_t's by comparing all metadata about the maps. The function
 * does not actually compare all points on the map. */
int map_shallow_cmp(map_t const *m1, map_t const *m2);

/* Print map to the file pointer f. */
void map_print(map_t const *m, FILE *f);

/* Copy the map pointed to by m to the map that is returned by the function.
 * The function also copies the underlying array so all changing is safe. */
map_t map_cpy(map_t const *m);

/* Find out if point on a map is water or not. */
int map_is_water(map_t const *m, int x, int y);

#endif
