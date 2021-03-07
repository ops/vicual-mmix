#include <stdio.h>
#include <math.h>

/*
   00  01  10  11  00  01  10  11  00  01  10  11  00  01  10  11
   00  00  00  00  01  01  01  01  10  10  10  10  11  11  11  11

              128     128     128     128     128     128 128 128
   32 124 126 +98 108 +97 127 123 123 127  97 108  98 126 124  32
 */

char logo[6][6] = {
#if 1
    "      ",
    "vv vv ",
    "vvvvv ",
    "vv vv ",
    " vvv  ",
    "      ",
#endif
#if 0
    "  v   ",
    " vvv  ",
    "vv vv ",
    "vv vv ",
    "vv vv ",
    "      ",
#endif
#if 0
    "  vv  ",
    "  vv  ",
    " vvvv ",
    " v  v ",
    " vvvv ",
    "  vv  ",
#endif
#if 0
    "  vv  ",
    "  vv  ",
    " vvvv ",
    " v  v ",
    " vvvv ",
    "      ",
#endif
#if 0
    "v     ",
    "vv    ",
    "vvv  v",
    " vvvvv",
    "  vvv ",
    "   v  "
#endif
};


#define MAXFRAMES 8
char pic[64][64], prev[64][64];
unsigned char anim[MAXFRAMES][128];

int main(int argc, char *argv[]) {
    const char *name = NULL;
    int type = 0, size = 512, spirals = 8, amp = 8, header = 1, tail = 0;
    int i, x, y, ntsc = 0;
    int frames = 8, parallax = 0;

    for (i=1;i<argc;i++) {
	if (!strcasecmp(argv[i], "-n")) {
	    ntsc = 1;
	} else if (!strcasecmp(argv[i], "-d")) {
	    header = 0;
	} else if (!strcasecmp(argv[i], "-t")) {
	    tail = 1;
	} else if (!strcasecmp(argv[i], "-p")) {
	    parallax = 1;
	} else if (!strcasecmp(argv[i], "-o") && i+1<argc) {
	    name = argv[++i];
	} else if (!strcasecmp(argv[i], "-f") && i+1<argc) {
	    /* -f frames */
	    frames = strtol(argv[++i], NULL, 0);
	} else if (!strcasecmp(argv[i], "-s") && i+2<argc) {
	    /* -s spirals size */
	    type = 1;
	    spirals = strtol(argv[++i], NULL, 0);
	    size = strtol(argv[++i], NULL, 0);
	    amp = 0;
	    if (i+1<argc)
	      amp = strtol(argv[++i], NULL, 0);
	} else if (!strcasecmp(argv[i], "-r") && i+1<argc) {
	    /* -r size */
	    type = 1;
	    spirals = 0;
	    size = strtol(argv[++i], NULL, 0);
	    amp = 0;
	} else if (!strcasecmp(argv[i], "-x") && i+3<argc) {
	    /* -x size */
	    type = 3;
	    spirals = strtol(argv[++i], NULL, 0);
	    size = strtol(argv[++i], NULL, 0);
	    amp = strtol(argv[++i], NULL, 0);
	} else if (!strcasecmp(argv[i], "-v") && i+3<argc) {
	    /* -v spirals size amp */
	    type = 4;
	    spirals = strtol(argv[++i], NULL, 0);
	    size = strtol(argv[++i], NULL, 0);
	    amp = strtol(argv[++i], NULL, 0);
	}
    }



    for (i=0; i < frames; i++) {
	for (y=0; y<32*4; y+=4) {
	    for (x=0; x<32*4; x+=4) {
		int r2;
		if (type == 1) {
		  if (parallax) {
		    r2 = pow((x-32*4+2)*(x-32*4+2)+(y-32*4+2)*(y-32*4+2), 0.25)*16.;
		  } else {
		    r2 =  sqrt((x-32*4+2)*(x-32*4+2)+(y-32*4+2)*(y-32*4+2));
		  }
		  r2 += (atan2(x-32*4+2,y-32*4+2))*spirals*size/M_PI +
		    i * size / (double)frames;
		  r2 = (r2 & size) ? 1 : 0;

		  if (amp) {
		    if ((int)(sqrt((x-32*4+2)*(x-32*4+2)+(y-32*4+2)*(y-32*4+2))*amp
			      //+ i * amp / (double)frames
			      ) & size)
		      r2 ^= 1;
		  }

		} else if (type == 4) {
		    double x2 = x-32*4+2;
		    double y2 = y-32*4+2;
		    int r3 = sqrt(x2*x2+y2*y2)/size + 6.*i/ (double)frames;
		    int a3 = //x2+y2;
		      atan2(x2,y2)*spirals/M_PI + amp*i/(double)frames;

		    //printf("%f %f %f\n", x2, y2, atan2(x2,y2));

		    if (parallax) {
		      r3 = sqrt(sqrt(x2*x2+y2*y2))*16./size + 6*i/ (double)frames;
		    }

		    //r2 = (a3 & 1) ? 1 : 0;
#if 0
		    printf("%d %d %d\n",
			   (6*32+r3)%6, (6*32+a3)%6,
			   logo[(6*32+r3)%6][(6*32+a3)%6]);
#endif
		    r2 = (logo[(6*32+r3)%6][(6*32+a3)%6] != ' ') ? 1 : 0;
		    if (!(((6*32+r3)/6)&1))	r2 ^= 1;

		    //r2 = a3 & 2;//(r3 & 4) ? 1 : 0;
		    //r2 = (a3 == 0) ? 1 : 0;

		} else if (type == 3) {
		    //r2 = (int)(x + i * size / 6.0) ^ (int)(y + i * size / 6.0);
#if 0
		    /* Not suitable for color scrolling.. */
		    r2 = sqrt((x-32*4+2)*(x-32*4+2)+(y-32*4+2)*(y-32*4+2)) +
			(sin(atan2(x-32*4+2,y-32*4+2)*spirals+M_PI*i/3.0))*
			amp/M_PI;
#endif
		    r2 = (int)(amp*sqrt((x-32*4+2)*(x-32*4+2)+(y-32*4+2)*(y-32*4+2)))
			^
			(int)((atan2(x-32*4+2,y-32*4+2))*spirals*size/M_PI +
			      i * size / (double)frames);
		    r2 = (r2 & size) ? 1 : 0;
		}
		pic[x/4][y/4] = r2;
	    }
	}
	/* packed -- only one quarter */
	for (y=0;y<8;y++) {
	    for (x=0;x<16;x++) {
	        int idx =
		  (pic[2*x+0][2*y+0]<<1) | pic[2*x+1][2*y+0] |
		  (pic[2*x+0][2*y+1]<<3) | (pic[2*x+1][2*y+1]<<2) |
		  (pic[2*x+0][2*y+0+16]<<5) | (pic[2*x+1][2*y+0+16]<<4) |
		  (pic[2*x+0][2*y+1+16]<<7) | (pic[2*x+1][2*y+1+16]<<6)
		  ;
		anim[i][y*16+x] = idx;

		/*
		  10
		  32
		  ..
		  ..
		  54
		  76
		  ..
		  ..
		 */
	    }
	}
    }


    if (!name) {
      memset(prev, 0, sizeof(prev));
	printf("\033[2J");
	for (i=0; i < 1000; i++) {
	  int frame   = i & 7;
	  int reverse = i & 8;
	  const char *on = "\033[7m  \033[m";
	  const char *off = "  ";
	  if (reverse) {
	    off = on;
	    on = "  ";
	  }
	  printf("\033[0;0H");
	  /* first quarter */
	  for (y=0;y<8;y++) {
	    for (x=0;x<16;x++) {
	      int cell = anim[frame][y*16+x];
	      pic[2*x+0][2*y+0] = (cell & 2) ? 1 : 0;
	      pic[2*x+1][2*y+0] = (cell & 1) ? 1 : 0;
	    }
	    for (x=0;x<16;x++) {
	      int cell = anim[frame][y*16+x];
	      pic[2*x+0][2*y+1] = (cell & 8) ? 1 : 0;
	      pic[2*x+1][2*y+1] = (cell & 4) ? 1 : 0;
	    }
	  }
	  for (y=0;y<8;y++) {
	    for (x=0;x<16;x++) {
	      int cell = anim[frame][y*16+x];
	      pic[2*x+0][2*y+0+16] = (cell & 32) ? 1 : 0;
	      pic[2*x+1][2*y+0+16] = (cell & 16) ? 1 : 0;
	    }
	    for (x=0;x<16;x++) {
	      int cell = anim[frame][y*16+x];
	      pic[2*x+0][2*y+1+16] = (cell & 128) ? 1 : 0;
	      pic[2*x+1][2*y+1+16] = (cell & 64) ? 1 : 0;
	    }
	  }
	  /* rotate second quarter */
	  for (y=0;y<32;y++) {
	    for (x=0;x<32;x++) {
	      pic[32+x][y] = pic[y][31-x];
	    }
	  }
	  /* mirror bottom half */
	  for (y=0;y<32;y++) {
	    for (x=0;x<64;x++) {
	      pic[x][32+y] = pic[63-x][31-y];
	    }
	  }

	  /* Actual display */
	  for (y=0;y<64;y++) {
	    printf("%02d", y);
	    for (x=0;x<64;x++) {
	      if (tail && (reverse ? 1-pic[x][y] : pic[x][y]) != prev[x][y]) {
		printf("XX");
	      } else {
		printf(pic[x][y] ? on : off);
	      }
	      prev[x][y] = reverse ? 1-pic[x][y] : pic[x][y];
	    }
	    printf("\n");
	  }

	  //printf(r2 ? "\033[7m  \033[m" : "  ");
	  printf("\n");
#if 0
	  fgetc(stdin);
#else
	  usleep(5000);
#endif
	  //	    usleep(20000);
	}
    } else {
        FILE *fp = fopen(name, "wb");
	if (!header) {
	    fwrite("\x00\x16", 2, 1, fp);
	}
	/* packed -- well, code expects them at different pages. */
	for (i=0;i<frames;i++) {
	    if (header) {
	      fprintf(fp,"\nscr%d:", i);
	    }
	    for (x=0;x<128;x++) {
	        if (ntsc && (x & 15) < 32/2-26/2)
		    continue; /* skip first 3 columns */
	        if (header) {
		  if ((x&15)==0)
		      fprintf(fp, "\n\tdc.b ");
		  else
		      fprintf(fp, ",");
		  fprintf(fp, "$%02x", anim[i][x]);
		} else {
		  fputc(anim[i][x], fp);
		}
	    }
	}
	if (header)
	  fprintf(fp, "\n");
	fclose(fp);
    }
    return 0;
}
