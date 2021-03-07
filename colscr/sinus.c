#include <stdio.h>
#include <math.h>

double leveys[256];
int tab[256] = {0};
double sinu[256];

int main() {
    int i, w, k;
    double sum = 0.;
    double acc = 0.;

    for(i=0;i<256;i++) {
	leveys[i] = 4.0+3.2*sin(i*M_PI/16.0);
    }
    k = 0;
#define MULT 1.99
#define OFFS (4.0-3.2)
    for(i=0;i<256;) {
	acc += leveys[k++];
	w = acc;
	if (w) {
	    acc -= w;
	    tab[i] = 1;
#if 1
	    {
		double kk = (MULT*(leveys[k]-OFFS) - MULT*(leveys[k-1]-OFFS)) / w ;
		int ii = 0;
		while (w--) {
		    sinu[i] = kk * ii++ + MULT*(leveys[k-1]-OFFS);
		    i++;
		}
	    }
#else
	    i += w;
#endif
	} else if (acc >= 0.5) {
	    acc -= 0.5;
	    tab[i] = 2;
	    sinu[i] = MULT/2.0*(leveys[k-1]-OFFS);
	    i++;
	}
    }
    {
	k = 0;
	for(i=0;i<256;i++) {
	    k += tab[i];
	    tab[i] = k;
	}
    }
    for(i=0;i<256;i++) {
	if (!(i & 15))
	    printf("\n\tdc.b ");
	else
	    printf(",");
	printf("$%02x", tab[i]-1);
    }
    fprintf(stdout, "\n");

    printf("sin:\n");
    for(i=0;i<256;i++) {
	if (!(i & 15))
	    printf("\n\tdc.b ");
	else
	    printf(",");
	printf("$%02x", (int)sinu[i]);
    }
    printf("\n");
    return 0;
}
