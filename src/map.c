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
static void map_calculate_height(map_t *m);
static int random_pm_range(int range);
static int average(int n_args, ...);
static int pix(int value, int max);
static int save_png_to_file(bitmap_t *bitmap, char const *path);
static pixel_t * pixel_at(bitmap_t *bitmap, int x, int y);
static int ** alloc_height_arr(size_t size);
static void handle_partial_alloc(int **arr, int last_alloced);
static size_t nearest_greater_two_power(size_t n);
static void map_top_right(map_t *m);
static void map_top_left(map_t *m);
static void map_bottom_right(map_t *m);
static void map_bottom_left(map_t *m);
static void map_restore_top_right(map_t *m);
static void map_restore_top_left(map_t *m);
static void map_restore_bottom_right(map_t *m);
static void map_restore_bottom_left(map_t *m);
static void calculate_means(map_t *m);
static inline void add_error_mid(map_t *m);

int map_init(map_t *m, size_t side_len, size_t random_range, size_t max_height)
{
    size_t act_side_len;

    /* Error handling. */
    if (random_range >= max_height || side_len <= 0 || random_range <= 0)
        return EINVAL;

    act_side_len = nearest_greater_two_power(side_len);

    if ((m->height = alloc_height_arr(act_side_len)) == NULL)
        return ENOMEM;

    m->side_len = act_side_len;
    m->random_range = random_range;
    m->max = max_height;

    /* Set the waterlevel so no water is shown unless a user call
     * map_set_water_height. */
    m->water_height = -random_range - 1;

    /* Set the random number generator. */
    srand(time(NULL));

    return 0;
}

int map_get_height(map_t const *m, int x, int y)
{
    return m->height[x][y];
}

void map_set_height(map_t *m, int x, int y, int value)
{
    m->height[x][y] = value;
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

    map_calculate_height(m);
}

static void map_calculate_height(map_t *m)
{
    if (m->side_len <= 1 || m->random_range <= 1)
        return;

    calculate_means(m);
    add_error_mid(m);

    map_top_right(m);
    map_top_left(m);
    map_bottom_right(m);
    map_bottom_left(m);
}

static void calculate_means(map_t *m)
{
    int x_mid, y_mid, lower_left, lower_right, upper_left, upper_right;
    size_t side_len = m->side_len - 1;

    x_mid = m->side_len / 2;
    y_mid = m->side_len / 2;

    lower_left = map_get_height(m, 0, 0);
    lower_right = map_get_height(m, side_len, 0);
    upper_left = map_get_height(m, 0, side_len);
    upper_right = map_get_height(m, side_len, side_len);

    /* Update midpoints as the average of surrounding points. */
    map_set_height(m, x_mid, 0, AVERAGE(lower_left, lower_right));
    map_set_height(m, 0, y_mid, AVERAGE(lower_left, upper_left));
    map_set_height(m, x_mid, side_len, AVERAGE(upper_left, upper_right));
    map_set_height(m, side_len, y_mid, AVERAGE(lower_right, upper_right));
    map_set_height(m, x_mid, y_mid, average(4, lower_left, upper_left,
                lower_right, upper_right));
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

inline int map_shallow_cmp(map_t const *m1, map_t const *m2)
{
    return m1->side_len == m2->side_len ||
        m1->random_range == m2->random_range ||
        m1->water_height == m2->water_height ||
        m1->max == m2->max;
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
        return NULL;

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

    for (i = 0; i < last_alloced; i++) {
        free(arr[i]);
    }

    free(arr);
}

static size_t nearest_greater_two_power(size_t n)
{
    size_t a = 1;

    while (a < n)
        a *= 2;

    return a;
}

static void map_top_right(map_t *m)
{
    int i;

    m->random_range /= 2;
    m->side_len /= 2;
    m->height += m->side_len;

    for (i = 0; i < m->side_len; i++)
        m->height[i] += m->side_len;

    map_calculate_height(m);
    map_restore_top_right(m);
}

static void map_restore_top_right(map_t *m)
{
    int i;

    for (i = 0; i < m->side_len; i++)
        m->height[i] -= m->side_len;

    m->height -= m->side_len;
    m->random_range *= 2;
    m->side_len *= 2;
}

static void map_top_left(map_t *m)
{
    int i;

    m->random_range /= 2;
    m->side_len /= 2;

    for (i = 0; i < m->side_len; i ++)
        m->height[i] += m->side_len;

    map_calculate_height(m);
    map_restore_top_left(m);
}

static void map_restore_top_left(map_t *m)
{
    int i;

    for (i = 0; i < m->side_len; i++)
        m->height[i] -= m->side_len;

    m->random_range *= 2;
    m->side_len *= 2;
}

static void map_bottom_right(map_t *m)
{
    m->random_range /= 2;
    m->side_len /= 2;
    m->height += m->side_len;

    map_calculate_height(m);
    map_restore_bottom_right(m);
}

static void map_restore_bottom_right(map_t *m)
{
    m->height -= m->side_len;
    m->random_range *= 2;
    m->side_len *= 2;
}

static void map_bottom_left(map_t *m)
{
    m->random_range /= 2;
    m->side_len /= 2;

    map_calculate_height(m);
    map_restore_bottom_left(m);
}

static void map_restore_bottom_left(map_t *m)
{
    m->random_range *= 2;
    m->side_len *= 2;
}

static inline void add_error_mid(map_t *m)
{
    int half_side = m->side_len / 2;
    int new_height = map_get_height(m, half_side, half_side) +
        random_pm_range(m->random_range);

    map_set_height(m, half_side, half_side, new_height);
}
