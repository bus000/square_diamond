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
static int new_height(map_t *m, int size, int offset);
static int map_average(int n_args, ...);
static int pix(int value, int max);
static int save_png_to_file(bitmap_t *bitmap, char const *path);
static pixel_t * pixel_at(bitmap_t *bitmap, int x, int y);
static unsigned int ** alloc_height_arr(size_t size);
static void handle_partial_alloc(unsigned int **arr, int last_alloced);
static inline int pow_2(int n);
static void divide(map_t *m, int size);
static void square(map_t *m, int x, int y, int size);
static void diamond(map_t *m, int x, int y, int size);
static int cmp_double(double a, double b, double eps);

int map_init(map_t *m, size_t size, double roughness)
{
    /* Error handling. */
    if (size <= 0 || roughness > 1.0 || roughness < 0.0)
        return EINVAL;

    /* Set the random number generator. */
    srand(time(NULL));

    m->side_len = pow_2(size) + 1;
    m->max = m->side_len - 1;
    m->roughness = roughness;

    if ((m->height = alloc_height_arr(m->side_len)) == NULL)
        return ENOMEM;

    /* Set the waterlevel so no water is shown unless a user call
     * map_set_water_height. */
    m->water_height = -1;

    return 0;
}

int map_get_height(map_t const *m, int x, int y)
{
    if (x >= m->side_len || x < 0 || y >= m->side_len || y < 0)
        return -1;

    return m->height[x][y];
}

void map_set_height(map_t *m, int x, int y, int value)
{
    m->height[x][y] = value;
}

void map_set_water_height(map_t *m, int height)
{
    m->water_height = height;
}

/* Choose height of initial corners and call map_calculate_height on the map. */
void map_square_diamond(map_t *m)
{
    LOWER_LEFT(m) = m->max / 2;
    UPPER_LEFT(m) = m->max / 2;
    LOWER_RIGHT(m) = m->max / 2;
    UPPER_RIGHT(m) = m->max / 2;

    divide(m, m->side_len);
}

static void divide(map_t *m, int size)
{
    int i, j;
    int half_size = size / 2;

    if (half_size < 1)
        return;

    for (i = half_size; i < m->max; i += size)
        for (j = half_size; j < m->max; j += size)
            square(m, i, j, half_size);

    for (j = 0; j <= m->max; j += half_size)
        for (i = (j + half_size) % size; i <= m->max; i += size)
            diamond(m, i, j, half_size);

    divide(m, half_size);
}

static void square(map_t *m, int x, int y, int size)
{
    int ave = map_average(4, map_get_height(m, x + size, y + size),
            map_get_height(m, x + size, y - size),
            map_get_height(m, x - size, y + size),
            map_get_height(m, x - size, y - size));

    map_set_height(m, x, y, new_height(m, size, ave));
}

static void diamond(map_t *m, int x, int y, int size)
{
    int ave = map_average(4, map_get_height(m, x, y - size),
            map_get_height(m, x + size, y),
            map_get_height(m, x, y + size),
            map_get_height(m, x - size, y));

    map_set_height(m, x, y, new_height(m, size, ave));
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
            tmp_height = map_get_height(m, x, y);

            pixel = pixel_at(&bitmap, x, y);
            pixel->red = pix(tmp_height, m->max);
            pixel->green = pix(tmp_height, m->max);
            pixel->blue = pix(tmp_height, m->max);
        }
    }

    save_png_to_file(&bitmap, filename);

    return 0;
}

inline int map_shallow_cmp(map_t const *m1, map_t const *m2)
{
    return m1->side_len == m2->side_len &&
        m1->roughness == m2->roughness &&
        m1->water_height == m2->water_height &&
        m1->max == m2->max &&
        cmp_double(m1->roughness, m2->roughness, 0.001);
}

static int new_height(map_t *m, int size, int prevval)
{
    int max_change, new_val;

    /* Calculate the maximum amount the map value can change. */
    max_change = (int) ((double) size * m->roughness);

    if (max_change == 0)
        new_val = 0;
    else
        new_val = prevval - ((rand() % (max_change * 2)) - max_change);

    if (new_val < 0)
        return 0;
    else if (new_val > m->max)
        return m->max;
    else
        return new_val;
}

static int map_average(int n_args, ...)
{
    int i;
    int sum = 0;
    int cur;
    int num_valid = 0;
    va_list ap;

    va_start(ap, n_args);

    for (i = 0; i < n_args; i++) {
        cur = va_arg(ap, int);

        if (cur >= 0) {
            num_valid += 1;
            sum += cur;
        }
    }

    va_end(ap);

    return sum / num_valid;
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
    return (value < 0) ? 0 : (int) (256.0 *((double) (value)/(double) max));
}

static pixel_t * pixel_at(bitmap_t *bitmap, int x, int y)
{
    return bitmap->pixels + bitmap->width * y + x;
}

static unsigned int ** alloc_height_arr(size_t size)
{
    int i;
    unsigned int **arr = calloc(size, sizeof(int *));

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

static void handle_partial_alloc(unsigned int **arr, int last_alloced)
{
    int i;

    if (last_alloced < 0)
        return;

    for (i = 0; i < last_alloced; i++)
        free(arr[i]);

    free(arr);
}

static inline int pow_2(int n)
{
    return 1 << n;
}

static int cmp_double(double a, double b, double eps)
{
    return (a > b) ? (a - b) < eps : (b - a) < eps;
}
