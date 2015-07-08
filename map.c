#include "map.h"
#include <stdlib.h> /* malloc, calloc, free. */
#include <time.h> /* time. */
#include <stdarg.h> /* va_start, va_arg, va_end. */
#include <inttypes.h> /* uint_8. */
#include <stdio.h> /* FILE, fopen, fclose. */
#include <png.h> /* png_create_write_struct, png_set_IHDR, png_malloc */
#include <errno.h> /* ENOMEM, EINVAL. */

/* Help structs. */
typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} pixel_t;

typedef struct  {
    pixel_t *pixels;
    size_t width;
    size_t height;
} bitmap_t;

/* Help functions. */
static void map_calculate_height(map_t *m, int lower_left_x, int lower_left_y,
        int upper_right_x, int upper_right_y, int random_range);
static int random_pm_range(int range);
static int average(int n_args, ...);
static int pix(int value, int max);
static int save_png_to_file(bitmap_t *bitmap, char const *path);
static pixel_t * pixel_at(bitmap_t *bitmap, int x, int y);
static int ** alloc_height_arr(size_t size);
static void handle_partial_alloc(int **arr, int last_alloced);
static void calculate_means(map_t *m);

/* TODO: implement water. */
int map_init(map_t *m, size_t side_len, size_t random_range, size_t max_height)
{
    /* Error handling. */
    if (random_range >= max_height || side_len <= 0 || random_range <= 0)
        return EINVAL;

    if ((m->height = alloc_height_arr(side_len)) == NULL)
        return ENOMEM;

    m->side_len = side_len;
    m->random_range = random_range;
    m->max = max_height;

    /* Set the waterlevel so no water is shown unless a user call
     * map_set_water_height. */
    m->water_height = -random_range - 1;

    /* Set the random number generator. */
    srand(time(NULL));

    return 0;
}

int map_get_height(map_t const *m, point_t pos)
{
    return m->height[pos.x][pos.y];
}

void map_set_height(map_t *m, point_t pos, int value)
{
    m->height[pos.x][pos.y] = value;
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
    m->height[m->side_len-1][0] = random_pm_range(m->random_range);
    m->height[m->side_len-1][m->side_len-1] = random_pm_range(m->random_range);
    m->height[0][m->side_len-1] = random_pm_range(m->random_range);

    map_calculate_height(m, 0, 0, m->side_len-1, m->side_len-1, m->random_range);
}

int map_save_as_png(map_t const *m, char const *filename, size_t height,
        size_t width)
{
    bitmap_t bitmap;
    int x;
    int y;
    int tmp_height;
    pixel_t *pixel;

    if (height > m->side_len || width > m->side_len)
        return EINVAL;

    /* Create an image. */
    bitmap.width = width;
    bitmap.height = height;

    bitmap.pixels = calloc(sizeof(pixel_t), bitmap.width * bitmap.height);

    for (y = 0; y < bitmap.height; y++) {
        for (x = 0; x < bitmap.width; x++) {
            tmp_height = map_get_height(m, x, y) + m->random_range;

            pixel = pixel_at(&bitmap, x, y);
            pixel->red = pix(tmp_height, m->random_range * 2);
            pixel->green = pix(tmp_height, m->random_range * 2);
            pixel->blue = pix(tmp_height, m->random_range * 2);
        }
    }

    save_png_to_file(&bitmap, filename);

    return 0;
}

point_t map_get_pos(pos_t pos)
{
    switch (pos) {
        case TOP_LEFT:
            return {0, side_len-1};
        case TOP_RIGHT:
            return {side_len-1, side_len-1};
        case BOTTOM_LEFT:
            return {0, 0};
        case BOTTOM_RIGHT:
            return {0, side_len-1};
        case MIDDLE:
            return {side_len/2, side_len/2};
        case TOP_MIDDLE:
            return {side_len/2, side_len-1};
        case LEFT_MIDDLE:
            return {0, side_len/2};
        case RIGHT_MIDDLE:
            return {side_len-1, side_len/2};
        case BOTTOM_MIDDLE:
            return {side_len/2, 0};
        default:
            return {-1, -1};
    }
}

typedef struct {
    map_t *map;
    int lower_left_x;
    int lower_left_y;
    int upper_rigth_x;
    int upper_right_y;
    int random_range;
}

static void * map_calculate_height(void *map)
{
    map_t *m = (map_t *) map;
/*    point_t top_left = map_get_pos(TOP_LEFT);*/
    /*point_t top_right = map_get_pos(TOP_RIGHT);*/
    /*point_t bottom_left = map_get_pos(BOTTOM_LEFT);*/
    /*point_t bottom_right = map_get_pos(BOTTOM_RIGHT);*/
    /*point_t middle = map_get_pos(MIDDLE);*/
    /*point_t left_middle = map_get_pos(LEFT_MIDDLE);*/

    calculate_means(m);
}

/* Function assumes the corner points given is already computed.  It then splits
 * the square to smaller squares, computes the corners and call the function
 * recursively. */
static void map_calculate_height(map_t *m, int lower_left_x, int lower_left_y,
        int upper_right_x, int upper_right_y, int random_range)
{
    int x_mid;
    int y_mid;
    int lower_left, lower_right, upper_left, upper_right;

    x_mid = average(2, upper_right_x, lower_left_x);
    y_mid = average(2, upper_right_y, lower_left_y);

    lower_left = map_get_height(m, lower_left_x, lower_left_y);
    lower_right = map_get_height(m, upper_right_x, lower_left_y);
    upper_left = map_get_height(m, lower_left_x, upper_right_y);
    upper_right = map_get_height(m, upper_right_x, upper_right_y);

    /* Update midpoints as the average of surrounding points. */
    map_set_height(m, x_mid, lower_left_y, AVERAGE(lower_left, lower_right));
    map_set_height(m, lower_left_x, y_mid, AVERAGE(lower_left, upper_left));
    map_set_height(m, x_mid, upper_right_y, AVERAGE(upper_left, upper_right));
    map_set_height(m, upper_right_x, y_mid, AVERAGE(lower_right, upper_right));
    map_set_height(m, x_mid, y_mid, average(4, lower_left, upper_left,
                lower_right, upper_right));

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
    return (range == 0) ? 0 : (rand() % (2 * range)) - range;
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

/* http://www.lemoda.net/c/write-png/ */
/* TODO: Add better handling of errors. */
static int save_png_to_file(bitmap_t *bitmap, char const *path)
{
    FILE * fp;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    size_t x, y;
    png_byte ** row_pointers = NULL;
    int status = -1;
    int pixel_size = 3;
    int depth = 8;

    fp = fopen (path, "wb");
    if (!fp) {
        status = errno;
        goto fopen_failed;
    }

    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL)
        goto png_create_write_struct_failed;

    info_ptr = png_create_info_struct (png_ptr);
    if (info_ptr == NULL)
        goto png_create_info_struct_failed;

    if (setjmp (png_jmpbuf (png_ptr)))
        goto png_failure;

    png_set_IHDR (png_ptr, info_ptr, bitmap->width, bitmap->height, depth,
            PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
            PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

    row_pointers = png_malloc (png_ptr, bitmap->height * sizeof (png_byte *));
    for (y = 0; y < bitmap->height; ++y) {
        png_byte *row =
            png_malloc (png_ptr, sizeof (uint8_t) * bitmap->width * pixel_size);
        row_pointers[y] = row;
        for (x = 0; x < bitmap->width; ++x) {
            pixel_t * pixel = pixel_at (bitmap, x, y);
            *row++ = pixel->red;
            *row++ = pixel->green;
            *row++ = pixel->blue;
        }
    }

    png_init_io (png_ptr, fp);
    png_set_rows (png_ptr, info_ptr, row_pointers);
    png_write_png (png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

    status = 0;

    for (y = 0; y < bitmap->height; y++) {
            png_free (png_ptr, row_pointers[y]);
        }
    png_free (png_ptr, row_pointers);

 png_failure:
 png_create_info_struct_failed:
    png_destroy_write_struct(&png_ptr, &info_ptr);
 png_create_write_struct_failed:
    fclose(fp);
 fopen_failed:
    return status;
}

static int pix(int value, int max)
{
    if (value < 0)
        return 0;
    return (int) (256.0 *((double) (value)/(double) max));
}

static pixel_t * pixel_at(bitmap_t *bitmap, int x, int y)
{
    return bitmap->pixels + bitmap->width * y + x;
}

static int ** alloc_height_arr(size_t size)
{
    int i;
    int **arr = calloc(size, sizeof(int *));

    if (arr == NULL)
        NULL;

    for (i = 0; i < size; i++) {
        arr[i] = calloc(size, sizeof(int));
        if (arr[i] == NULL) {
            handle_partial_alloc(arr, i - 1);
            return NULL;
        }
    }

    return arr;
}

static void handle_partial_alloc(int **arr, int last_alloced)
{
    int i;

    if (last_alloced < 0)
        return;

    for (i = 0; i < last_alloced; i++)
        free(arr[i]);

    free(arr);
}

static void calculate_means(map_t *m)
{
    int value;
    point_t top_left = map_get_pos(TOP_LEFT);
    point_t top_right = map_get_pos(TOP_RIGHT);
    point_t bottom_left = map_get_pos(BOTTOM_LEFT);
    point_t bottom_right = map_get_pos(BOTTOM_RIGHT);
    point_t middle = map_get_pos(MIDDLE);

    point_t left_middle = map_get_pos(LEFT_MIDDLE);
    point_t right_middle = map_get_pos(RIGHT_MIDDLE);
    point_t top_middle = map_get_pos(TOP_MIDDLE);
    point_t bottom_middle = map_get_pos(BOTTOM_MIDDLE);
    point_t middle = map_get_pos(MIDDLE);

    value = average(2, map_get_height(m, bottom_left),
            map_get_height(m, top_left));
    map_set_height(m, left_middle, value);

    value = average(2, map_get_height(m, bottom_right),
            map_get_height(m, top_right));
    map_set_height(m, right_middle, value);
}
