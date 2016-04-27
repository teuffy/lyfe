/*
 * 2-59.c
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

int mask_bigendian(int x, int y) {
	return (x & 0xFF) | (y & ~0xFF);
}

int mask_smallendian(int x, int y) {
	return (x & ~0xFF) | (y & 0xFF);
}

int main(void) {
	int test_x = 0x89ABCDEF;
	int test_y = 0x76543210;
	if (is_little_endian() == 1) {
		printf("%X\n", mask_smallendian(test_x, test_y));
	} else {
		printf("%X\n", mask_bigendian(test_x, test_y));
	}
	return 0;
}
