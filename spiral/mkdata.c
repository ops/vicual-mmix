#include <stdio.h>

int main(int argc, char *argv[]) {
    int n;

    fwrite("\x00\x16", 2, 1, stdout);
    for (n=1;n<argc;n++) {
	FILE *fp = fopen(argv[n], "rb");
	unsigned char tmp[8*256];

	if (fp) {
	    int i;
	    fread(tmp, 2, 1, fp);
#if 0 /*not packed yet*/
	    i = fread(tmp, 6*256, 1, fp);
	    fprintf(stderr, "%d bytes\n", i);
	    for (i=0;i<4*256;i++) {
		tmp[i] |= (tmp[4*256+i]<<4);
	    }
	    fclose(fp);
	    fwrite(tmp, 3*256, 1, stdout);
#else /*packed already*/
	    i = fread(tmp, 1, 8*128, fp);
	    fprintf(stderr, "%d bytes\n", i);
	    fclose(fp);
	    fwrite(tmp, 1, 8*128, stdout);
#endif
	} else {
	    fprintf(stderr, "Could not open %s\n", argv[n]);
	}
    }
    return 0;
}
