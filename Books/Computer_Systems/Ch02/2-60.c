/*
 * 2-60.c
 *
 *  Created on: Apr 27, 2016
 *      Author: navaro1
 */

#include <stdio.h>

int replace_byte(unsigned x, int i, unsigned char b) {
	int bitsSwitch = i << 3;
	unsigned int mask = 0xFF << bitsSwitch;
	return (x & ~mask) | (b << bitsSwitch);
}

int main(void) {
	printf("%x\n", replace_byte(0x12345678, 0xAB, 2));
	printf("%x\n", replace_byte(0x12345678, 0xAB, 0));
	return 0;
}
