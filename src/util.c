#include "util.h"
#include <ctype.h> /* isspace, isdigit. */

int is_number(char const *string)
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

int find_two_greater(int n)
{
    int a = 1;
    int b = 1;

    while (a < n) {
        a <<= 1;
        b += 1;
    }

    return b - 1;
}

inline int pow_2(int n)
{
    return 1 << n;
}

inline int cmp_double(double a, double b, double eps)
{
    return (a > b) ? (a - b) < eps : (b - a) < eps;
}
