#include <stdio.h>
#include <math.h>


int main() {
    int i, a;

    for (a=-4;a<9;a++) {
	for(i=0;i<32;i++) {
	    if(!(i&15))
		fprintf(stdout, "\n\tdc.b ");
	    else
		fprintf(stdout, ",");
	    fprintf(stdout, "$%02x",
		    (int)((double)abs(a)+(double)a*cos((double)i*M_PI/16.0)) & 0xff);
	}
	fprintf(stdout, "\n");
    }
    return 0;
}
