// test file to run c functions compiled via gcc
#include <stdio.h>
#include <math.h>
#include "cJSON.h"

int add(int a, int b) {
    return a + b;
}

int main(void) {
    printf("%i \n", add(1, 2));
    printf("%i \n", add(5, 3));
    printf("%f \n", ldexp(11.0, 2));

    return 0;
}
