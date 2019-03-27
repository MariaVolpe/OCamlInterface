// test file to run c functions compiled via gcc
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main(void) {
    printf("%d", add(1, 2));

    return 0;
}
