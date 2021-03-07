#include <stdio.h>
#include <stdlib.h>

short tab[] = {0x00, 0x80, 0x88, 0xa4, 0xaa, 0xda, 0xee};

int CountBits(int n, short *masks, short *m) {
    int i, mx = 0, c = 0;
    for (i=0; i<8; i++) {
	int j, b = 0;
	for (j=0; j<n; j++) {
	    if (masks[j] & (1<<i))
		b++;
	}
	if (c < b)
	    c = b;
	mx += b * b;
    }
    if (m)
	*m = c;
    return mx;
}


int main(int argc, char *argv[]) {
    short speed[7], mask[7], l;
    int n;

    for (n=1; n < argc; n++) {
	speed[n-1] = atol(argv[n]);
    }
    for (n=0; n < argc-1; n++) {
	int shift;
	short m, b, bs = 0;
	mask[n] = m = tab[speed[n]];
	b = CountBits(n+1, mask, &l);
	for (shift = 1; shift<8; shift++) {
	    short old = mask[n], new;
	    m = ((m<<1) | ((m>>7)&1)) & 0xff;
	    mask[n] = m;
	    new = CountBits(n+1, mask, &l);
	    if (b > new) {
		b = new;
		bs = shift;
	    } else {
		mask[n] = old;
	    }
	}
	CountBits(n+1, mask, &l);
	printf("%d: speed %d, shift %d, b %d, mask 0x%02x, len %d\n",
		n, speed[n], bs, b, mask[n], l);
    }
    return 0;
}
