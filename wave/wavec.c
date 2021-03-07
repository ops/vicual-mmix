#include <stdio.h>
#include <math.h>

#define SIZE 16
#define MAX_AMP 8.9
#define AMP_SIZE 8
int sini[2*AMP_SIZE][SIZE];

char dat[2*SIZE][2*SIZE+1] = {0};

char amps[AMP_SIZE] = {0, 12, 22, 29, 31, 29, 22, 12, 0};

int main(void) {
    int i, x, y, k;

    for (i=0;i<2*AMP_SIZE;i++) {
	for (x=0;x<SIZE;x++) {
	    sini[i][x] = MAX_AMP *
		sin(i*M_PI/AMP_SIZE) * sin(x*M_PI/SIZE);
	    if ((x & 15)==0)
		printf("\n\tdc.b ");
	    else
		printf(",");
	    printf("$%02x", (sini[i][x]*16) & 0xff);
	}
    }
    printf("\n");

    printf("\033[2J");
    for (i=0;i<40000;i++) {
	int modi = i & (2*AMP_SIZE-1);
	int modj = (i + AMP_SIZE/2) & (2*AMP_SIZE-1);

	printf("\033[0;0H");

	for (y=0; y<2*SIZE; y++) {
	    int y2;
	    if (y >= SIZE)
		y2 = -sini[modj][y-SIZE];
	    else
		y2 = sini[modj][y];

	    //	    printf("%d ", y2);
	    for (x=0;x<SIZE;x++) {
		int x2 = (y + sini[modi][x]);
		dat[y][x] = (x2 & (SIZE/2)) ? 1 : 0;
	    }
	    for (x=0;x<SIZE;x++) {
		//printf("%d", (y2 & (SIZE/2)) ? 1 : 0);
		dat[y][x] ^= (y2 & (SIZE/2)) ? 1 : 0;
		y2++;
	    }
	    //printf("\n");
	    for (x=0;x<SIZE;x++) {
		dat[y][x] = dat[y][x] ? '#' : ' ';
	    }
	}
	for (y=0; y<2*SIZE; y++) {
	    for (x=0;x<SIZE;x++) {
		dat[y][2*SIZE-x-1] = dat[2*SIZE-y-1][x];
	    }
	}
	for (y=0;y<64;y++) {
	    for (x=0;x<2;x++) {
		printf("%s", dat[y&(2*SIZE-1)]);
	    }
	    printf("\n");
	}
	usleep(100000);
    }
    return 0;
}
