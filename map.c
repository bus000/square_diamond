#include "map.h"
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <stdarg.h>

/* Help functions. */
static void map_calculate_height(map_t *m, int lower_left_x, int lower_left_y,
        int upper_right_x, int upper_right_y, int random_range);
static int random_pm_range(int range);
static int average(int n_args, ...);

void map_init(map_t *m, int length, int width, int random_range)
{
    int i;

    /* calloc initializes all to 0. */
    m->height = calloc(width, sizeof(int *));

    assert(m->height != NULL);

    for (i = 0; i < width; i++) {
        m->height[i] = calloc(length, sizeof(int));
        assert(m->height[i] != NULL);
    }

    m->length = length;
    m->width = width;
    m->random_range = random_range;

    /* Set the waterlevel so no water is shown unless a user call
     * map_set_water_height. */
    m->water_height = -random_range - 1;

    /* Set the random number generator. */
    srand(time(NULL));
}

int map_get_height(map_t *m, int x, int y)
{
    return m->height[x][y];
}

void map_set_water_height(map_t *m, int new_height)
{
    m->water_height = new_height;
}

void map_square_diamond(map_t *m)
{
    /* Choose height of initial corners and call map_calculate_height on the
     * map. */
    m->height[0][0] = random_pm_range(m->random_range);
    m->height[m->width-1][0] = random_pm_range(m->random_range);
    m->height[m->width-1][m->length-1] = random_pm_range(m->random_range);
    m->height[0][m->length-1] = random_pm_range(m->random_range);

    map_calculate_height(m, 0, 0, m->width-1, m->length-1, m->random_range);
}

/* Function assumes the corner points given is already computed.  It then splits
 * the square to smaller squares, computes the corners and call the function
 * recursively. */
static void map_calculate_height(map_t *m, int lower_left_x, int lower_left_y,
        int upper_right_x, int upper_right_y, int random_range)
{
    int x_mid;
    int y_mid;

    x_mid = average(2, upper_right_x, lower_left_x);
    y_mid = average(2, upper_right_y, lower_left_y);

    /* Update midpoints as the average of surrounding points. */
    m->height[x_mid][lower_left_y] = average(2, m->height[lower_left_x]
            [lower_left_y], m->height[upper_right_x][lower_left_y]);

    m->height[lower_left_x][y_mid] = average(2, m->height[lower_left_x]
            [lower_left_y], m->height[lower_left_x][upper_right_y]);

    m->height[x_mid][upper_right_y] = average(2, m->height[lower_left_x]
            [upper_right_y], m->height[upper_right_x][upper_right_y]);

    m->height[upper_right_x][y_mid] = average(2, m->height[upper_right_x]
            [upper_right_y], m->height[upper_right_x][lower_left_y]);

    m->height[x_mid][y_mid] = average(4, m->height[lower_left_x][lower_left_y],
            m->height[lower_left_x][upper_right_y], m->height[upper_right_x]
            [upper_right_y], m->height[upper_right_x][lower_left_y]);

    /* Add error to midpoint height. */
    m->height[x_mid][y_mid] += random_pm_range(random_range);

    /* Recursive call on all subsquares with a size greater than 1. */
    if (lower_left_x + 1 < x_mid || lower_left_y + 1 < y_mid)
        map_calculate_height(m, lower_left_x, lower_left_y, x_mid, y_mid,
                random_range / 2);

    if (lower_left_x + 1 < x_mid || y_mid + 1 < upper_right_y)
        map_calculate_height(m, lower_left_x, y_mid, x_mid, upper_right_y,
                random_range / 2);

    if (x_mid + 1 < upper_right_x || y_mid + 1 < upper_right_y)
        map_calculate_height(m, x_mid, y_mid, upper_right_x, upper_right_y,
                random_range / 2);

    if (x_mid + 1 < upper_right_x || lower_left_y + 1 < y_mid)
        map_calculate_height(m, x_mid, lower_left_y, upper_right_x, y_mid,
                random_range / 2);
}

static int random_pm_range(int range)
{
    if (range == 0)
        return 0;

    return (rand() % (2 * range)) - range;
}

static int average(int n_args, ...)
{
    int i;
    int sum = 0;
    va_list ap;

    va_start(ap, n_args);

    for (i = 0; i < n_args; i++)
        sum += va_arg(ap, int);

    va_end(ap);

    return sum / n_args;
}
