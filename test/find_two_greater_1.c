#include "../src/util.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[])
{
    int i;

    for (i = 0; i < 10; i++) {
        printf("%d: %d\n", i, find_two_greater(i));
    }

    return EXIT_SUCCESS;
}
