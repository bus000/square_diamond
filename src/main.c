#include <stdio.h> /* fprintf, stderr. */
#include <stdlib.h> /* exit, strtol. */
#include <ctype.h> /* isspace. */
#include <string.h> /* strdup. */
#include <errno.h> /* EINVAL, ENOMEM, strerror. */
#include "map.h"

#define MAX(x, y) ((x) < (y) ? (y) : (x))

/* Help functions. */
static void handle_arguments(int argc, char const *argv[]);
static int is_number(char const *string);
static void test_input(long length, long width, double roughness);
static void print_err_exit(char *fun, int num);
static int find_two_greater(int n);

static long length = 1000;
static long width = 1000;
static double roughness = 0.5;
static char *filename = "out.png";

int main(int argc, char const *argv[])
{
    map_t map;
    int side_len;
    int ret_val;

    handle_arguments(argc, argv);
    side_len = find_two_greater(MAX(length, width));

    if ((ret_val = map_init(&map, side_len, roughness)) != 0)
        print_err_exit("map_init", ret_val);

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
                        fprintf(stderr, "Expecting number after -l\n");
                        exit(EXIT_SUCCESS);
                    } else if (!is_number(argv[next_arg+1])) {
                        fprintf(stderr, "Expecting number after -l\n");
                        exit(EXIT_SUCCESS);
                    } else {
                        length = strtol(argv[next_arg+1], NULL, 10);
                        next_arg += 1;
                    }

                    break;
                case 'w':
                    if (next_arg == argc-1) {
                        fprintf(stderr, "Expecting number after -w\n");
                        exit(EXIT_SUCCESS);
                    } else if (!is_number(argv[next_arg+1])) {
                        fprintf(stderr, "Expecting number after -w\n");
                        exit(EXIT_SUCCESS);
                    } else {
                        width = strtol(argv[next_arg+1], NULL, 10);
                        next_arg += 1;
                    }

                    break;
                case 'r':
                    if (next_arg == argc-1) {
                        fprintf(stderr, "Expecting floating point number "
                                "after -r\n");
                        exit(EXIT_SUCCESS);
                    } else if ((roughness = strtod(argv[next_arg+1], NULL)) == 0.0) {
                        if (errno != 0) {
                            fprintf(stderr, "Expecting floating point number "
                                "after -r\n");
                            exit(EXIT_SUCCESS);
                        }
                    }
                    next_arg += 1;

                    break;
                default:
                    fprintf(stderr, "Error, unknown switch %s\n",
                            argv[next_arg]);
                    exit(EXIT_SUCCESS);
            }
        } else {
            if (next_arg+1 < argc) { /* Not last argument. */
                fprintf(stderr, "Error, unknown argument %s\n", argv[next_arg]);
                exit(EXIT_SUCCESS);
            } else { /* Last argument is filename. */
                filename = strdup(argv[next_arg]);
            }
        }
    }

    test_input(length, width, roughness);
}

static int is_number(char const *string)
{
    /* Skip whitespace. */
    while (isspace(*string) && *string != '\0')
        string++;

    /* Handle single + or -. */
    if (!(isdigit(*string) || *string == '+' || *string == '-'))
        return 0;

    string++;
    while (*string != '\0') {
        if (!isdigit(*string))
            return 0;
        string++;
    }

    return 1;
}

static void test_input(long length, long width, double roughness)
{
    if (length <= 0) {
        fprintf(stderr, "Length should be greater than 0\n");
        exit(EXIT_SUCCESS);
    } else if (width <= 0) {
        fprintf(stderr, "Width should be greater than 0\n");
        exit(EXIT_SUCCESS);
    } else if (roughness < 0.0 || roughness > 1.0) {
        fprintf(stderr, "Roughness should be between 0.0 and 1.0.\n");
    }
}

static void print_err_exit(char *fun, int num)
{
    fprintf(stderr, "%s failed with: %d and errno %d %s\n", fun, num, errno,
            strerror(errno));

    exit(EXIT_SUCCESS);
}

static int find_two_greater(int n)
{
    int a = 1;
    int b = 1;

    while (a < n) {
        a <<= 1;
        b += 1;
    }

    return b;
}
