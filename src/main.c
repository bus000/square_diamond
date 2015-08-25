#include "map.h"
#include "util.h"
#include <stdio.h> /* fprintf, stderr. */
#include <stdlib.h> /* exit, strtol. */
#include <string.h> /* strdup. */
#include <errno.h> /* EINVAL, ENOMEM, strerror. */

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define LENGTH       1000L
#define WIDTH        1000L
#define ROUGHNESS    0.5
#define FILENAME     NULL
#define WATER_HEIGHT -1

/* Help functions. */
static void handle_arguments(int argc, char const *argv[]);
static void test_input(char const *program_name);
static void print_err_exit(char *fun, int num);
static void usage(char const *program_name);
static void usage_with_return(char const *program_name, int return_no);

static long length = LENGTH;
static long width = WIDTH;
static double roughness = ROUGHNESS;
static char *filename = FILENAME;
static int water_height = WATER_HEIGHT;

int main(int argc, char const *argv[])
{
    map_t map;
    int side_len;
    int ret_val;

    handle_arguments(argc, argv);
    side_len = find_two_greater(MAX(length, width));

    if ((ret_val = map_init(&map, side_len, roughness)) != 0)
        print_err_exit("map_init", ret_val);
    map_set_water_height(&map, water_height);

    map_square_diamond(&map);
    if ((ret_val = map_save_as_png(&map, filename, length, width)) != 0)
        print_err_exit("map_save_as_png", ret_val);

    printf("Map written to the file %s\n", filename);

    return 0;
}

/* Set the internal variables as the values maybe given by the user. */
static void handle_arguments(int argc, char const *argv[])
{
    int next_arg = 0;

    while (++next_arg < argc) {
        if (*(argv[next_arg]) == '-') {
            switch (*(argv[next_arg]+1)) {
            case 'l':
                if (next_arg == argc-1) {
                    usage(argv[0]);
                } else if (!is_number(argv[next_arg+1])) {
                    usage(argv[0]);
                } else {
                    length = strtol(argv[next_arg+1], NULL, 10);
                    next_arg += 1;
                }

                break;
            case 'w':
                if (next_arg == argc-1) {
                    usage(argv[0]);
                } else if (!is_number(argv[next_arg+1])) {
                    usage(argv[0]);
                } else {
                    width = strtol(argv[next_arg+1], NULL, 10);
                    next_arg += 1;
                }

                break;
            case 'r':
                if (next_arg == argc-1) {
                    usage(argv[0]);
                } else if ((roughness = strtod(argv[next_arg+1], NULL)) == 0.0) {
                    if (errno != 0)
                        usage(argv[0]);
                }
                next_arg += 1;

                break;
            case 'h':
                usage_with_return(argv[0], EXIT_SUCCESS);
                break;
            case 'W':
                if (next_arg == argc-1) {
                    usage(argv[0]);
                } else if (!is_number(argv[next_arg+1])) {
                    usage(argv[0]);
                } else {
                    water_height = (int) strtol(argv[next_arg+1], NULL, 10);
                    next_arg += 1;
                }

                break;
            default:
                usage(argv[0]);
                break;
            }
        } else {
            if (next_arg+1 < argc) { /* Not last argument. */
                usage(argv[0]);
            } else { /* Last argument is filename. */
                filename = strdup(argv[next_arg]);
            }
        }
    }

    test_input(argv[0]);
}

static void test_input(char const *program_name)
{
    if (length <= 0)
        usage(program_name);
    else if (width <= 0)
        usage(program_name);
    else if (roughness < 0.0 || roughness > 1.0)
        usage(program_name);
    else if (filename == NULL)
        usage(program_name);
}

static void print_err_exit(char *fun, int num)
{
    fprintf(stderr, "%s failed with: %d and errno %d %s\n", fun, num, errno,
            strerror(errno));

    exit(EXIT_FAILURE);
}

static void usage(char const *program_name)
{
    usage_with_return(program_name, EXIT_FAILURE);
}

static void usage_with_return(char const *program_name, int return_no)
{
    fprintf(stderr, "usage: %s [-w width] [-l length] [-r roughness] "
            "[-W waterheight] outfilename\n"

            "  -w: width of the output map in pixels, greater than 0, default "
            "%ld\n"

            "  -l: length of the output map in pixels, greater than 0, default "
            "%ld\n"

            "  -r: roughness of the map between 0.0 and 1.0, default %f\n"

            "  -W: height of water on the map, default %d\n"

            "outfilename: name of output file\n",

            program_name, WIDTH, LENGTH, ROUGHNESS, WATER_HEIGHT);

    exit(return_no);
}
