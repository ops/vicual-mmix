#include <stdio.h>
#include <math.h>


int main()
{
    int i;

    for(i=0;i<256;i++)
    {
	if(!(i&15))
	    fprintf(stdout, "\n\tdc.b ");
	else
	    fprintf(stdout, ",");
	fprintf(stdout, "$%02x",
#if 1
		(int)(20.0+19.9*cos((double)i*M_PI/128.0)) & 0xff);
#else
		(int)(48.0+
		      23.9*cos((double)i*M_PI/128.0)+
		      23.9*cos((double)i*M_PI/43.0)) & 0xff);
#endif
    }
    fprintf(stdout, "\n");
    return 0;
}
