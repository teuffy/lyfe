/*
 * is-little-endian.c
 *
 *  Created on: Apr 27, 2016
 *      Author: navaro1
 */

#include <stdio.h>

char is_little_endian(void) {
	int x = 1;
	unsigned char *xp = (unsigned char*) &x;
	return *xp;
}

int main(void) {
	printf("%d\n", is_little_endian());
}
