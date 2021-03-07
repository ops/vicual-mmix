#include <stdio.h>


char *font[] = {
#if 0
    "      "
    "      "
    "      "
    "      "
    "      "
    "      "
    "      "
    "      ",

    "      "
    " aaaa "
    "a    a"
    "a    a"
    "aaaaaa"
    "a    a"
    "a    a"
    "a    a",

    "      "
    "bbbbb "
    "b    b"
    "b    b"
    "bbbbb "
    "b    b"
    "b    b"
    "bbbbb ",

    "      "
    " cccc "
    "c    c"
    "c     "
    "c     "
    "c     "
    "c    c"
    " cccc ",

    "      "
    "dddd  "
    "d   d "
    "d    d"
    "d    d"
    "d    d"
    "d   d "
    "dddd  ",

    "      "
    "eeeeee"
    "e     "
    "e     "
    "eeeee "
    "e     "
    "e     "
    "eeeeee",

    "      "
    "ffffff"
    "f     "
    "f     "
    "fffff "
    "f     "
    "f     "
    "f     ",

    "      "
    " gggg "
    "g    g"
    "g     "
    "g  ggg"
    "g    g"
    "g    g"
    " gggg ",

    "      "
    "h    h"
    "h    h"
    "h    h"
    "hhhhhh"
    "h    h"
    "h    h"
    "h    h",

    "      "
    " iii  "
    "  i   "
    "  i   "
    "  i   "
    "  i   "
    "  i   "
    " iii  ",

    "      "
    "     j"
    "     j"
    "     j"
    "     j"
    "     j"
    "j    j"
    " jjjj ",

    "      "
    "k    k"
    "k    k"
    "k   k "
    "kkkk  "
    "k   k "
    "k    k"
    "k    k",

    "      "
    "l     "
    "l     "
    "l     "
    "l     "
    "l     "
    "l     "
    "llllll",

    "      "
    " mmmm "
    "m mm m"
    "m mm m"
    "m    m"
    "m    m"
    "m    m"
    "m    m",

    "      "
    "n    n"
    "nn   n"
    "n n  n"
    "n nn n"
    "n  n n"
    "n   nn"
    "n    n",

    "      "
    " oooo "
    "o    o"
    "o    o"
    "o    o"
    "o    o"
    "o    o"
    " oooo ",

    "      "
    "ppppp "
    "p    p"
    "p    p"
    "ppppp "
    "p     "
    "p     "
    "p     ",

    "      "
    " qqqq "
    "q    q"
    "q    q"
    "q q  q"
    "q  q q"
    "q   qq"
    " qqqq ",

    "      "
    "rrrrr "
    "r    r"
    "r    r"
    "rrrrr "
    "r  r  "
    "r   r "
    "r    r",

    "      "
    " ssss "
    "s    s"
    "s     "
    " ssss "
    "     s"
    "s    s"
    " ssss ",

    "      "
    "tttttt"
    "  t   "
    "  t   "
    "  t   "
    "  t   "
    "  t   "
    "  t   ",

    "      "
    "u    u"
    "u    u"
    "u    u"
    "u    u"
    "u    u"
    "u    u"
    " uuuu ",

    "      "
    "v    v"
    "v    v"
    "v    v"
    "v   v "
    "v  v  "
    "v v   "
    "vv    ",

    "      "
    "w    w"
    "w    w"
    "w    w"
    "w    w"
    "w ww w"
    "ww  ww"
    "w    w",

    "      "
    "x    x"
    "x    x"
    " x  x "
    "  xx  "
    " x  x "
    "x    x"
    "x    x",

    "      "
    "y    y"
    "y    y"
    " y   y"
    "  y y "
    "   y  "
    "  y   "
    " y    ",

    "      "
    "zzzzzz"
    "     z"
    "   zz "
    " zz   "
    "z     "
    "z     "
    "zzzzzz",

    "      "
    " 2222 "
    "2    2"
    "     2"
    " 2222 "
    "2     "
    "2     "
    "222222",

    "      "
    " 3333 "
    "3    3"
    "     3"
    " 3333 "
    "     3"
    "3    3"
    " 3333 ",

    "      "
    " 9999 "
    "9    9"
    "9    9"
    " 99999"
    "     9"
    "9    9"
    " 9999 ",

    "      "
    "      "
    "      "
    "      "
    "      "
    "      "
    "  ..  "
    "  ..  ",

    "      "
    " aaaa "
    "a    a"
    "a aa a"
    "a a aa"
    "a  aa "
    "a    a"
    " aaaa ",
#else
    "      "
    "      "
    "      "
    "      "
    "      "
    "      "
    "      "
    "      ",

    "      "
    "aaaaa "
    "  a aa"
    "  a  a"
    "  a  a"
    "  a  a"
    "  a aa"
    "aaaaa ",

    "      "
    "bbbbbb"
    "b  b b"
    "b  b b"
    "b  b b"
    "b  b b"
    "bb bb "
    " bbb  ",

    "      "
    " cccc "
    "cc  cc"
    "c    c"
    "c    c"
    "c    c"
    "cc  cc"
    " c  c ",

    "      "
    "dddddd"
    "d    d"
    "d    d"
    "d    d"
    "d    d"
    "dd  dd"
    " dddd ",

    "      "
    "eeeeee"
    "e  e e"
    "e  e e"
    "e  e e"
    "e  e e"
    "e  e e"
    "e    e",

    "      "
    "ffffff"
    "   f f"
    "   f f"
    "   f f"
    "   f f"
    "   f f"
    "     f",

    "      "
    " gggg "
    "gg  gg"
    "g    g"
    "g g  g"
    "g g  g"
    "ggg gg"
    " gg g ",

    "      "
    "hhhhhh"
    "   h  "
    "   h  "
    "   h  "
    "   h  "
    "   h  "
    "hhhhhh",

    "      "
    "      "
    "      "
    "i    i"
    "iiiiii"
    "i    i"
    "      "
    "      ",

    "      "
    " j    "
    "jj    "
    "j     "
    "j     "
    "j     "
    "jj    "
    " jjjjj",

    "      "
    "kkkkkk"
    "   k  "
    "   k  "
    "  kkk "
    " kk kk"
    "kk   k"
    "k     ",

    "      "
    "llllll"
    "l     "
    "l     "
    "l     "
    "l     "
    "l     "
    "l     ",

    "      "
    "mmmmm "
    "    mm"
    "     m"
    " mmmmm"
    "     m"
    "    mm"
    "mmmmm ",

    "      "
    "nnnnnn"
    "    nn"
    "   nn "
    "  nn  "
    " nn   "
    "nn    "
    "nnnnnn",

    "      "
    " oooo "
    "oo  oo"
    "o    o"
    "o    o"
    "o    o"
    "oo  oo"
    " oooo ",

    "      "
    "pppppp"
    "  p  p"
    "  p  p"
    "  p  p"
    "  p  p"
    "  pppp"
    "   pp ",

    "      "
    " qqqq "
    "qq  qq"
    "q    q"
    "q    q"
    "q q  q"
    "qq  qq"
    " qqqq ",

    "      "
    "rrrrrr"
    "  r  r"
    "  r  r"
    "  r  r"
    " rr  r"
    "rrrrrr"
    "r  rr ",

    "      "
    " s  s "
    "ss sss"
    "s  s s"
    "s  s s"
    "s  s s"
    "ssss s"
    " ss   ",

    "      "
    "     t"
    "     t"
    "     t"
    "tttttt"
    "     t"
    "     t"
    "     t",

    "      "
    " uuuuu"
    "uu    "
    "u     "
    "u     "
    "u     "
    "uu    "
    " uuuuu",

    "      "
    "     v"
    "   vvv"
    " vvv  "
    "vv    "
    " vvv  "
    "   vvv"
    "     v",

    "      "
    "wwwwww"
    "ww    "
    " ww   "
    "  ww  "
    " ww   "
    "ww    "
    "wwwwww",

    "      "
    "x    x"
    "xx  xx"
    " xxxx "
    "  xx  "
    " xxxx "
    "xx  xx"
    "x    x",

    "      "
    "     y"
    "    yy"
    "   yy "
    "yyyy  "
    "   yy "
    "    yy"
    "     y",

    "      "
    "zz   z"
    "zzz  z"
    "z z  z"
    "z zz z"
    "z  z z"
    "z  zzz"
    "z   zz",

    "      "
    " 22   "
    "2222 2"
    "2  2 2"
    "2  2 2"
    "2  2 2"
    "2  222"
    "2   2 ",

    "      "
    "3    3"
    "3  3 3"
    "3  3 3"
    "3  3 3"
    "3  3 3"
    "333333"
    " 33 3 ",

    "      "
    "   99 "
    "9 9999"
    "9 9  9"
    "9 9  9"
    "9 9  9"
    "999999"
    " 9999 ",

    "      "
    "      "
    "      "
    "..    "
    "..    "
    "      "
    "      "
    "      ",

    "      "
    " aaaa "
    "a    a"
    "a aa a"
    "a aa a"
    "a aa a"
    "a  a a"
    " a aa ",
#endif
};

/*
    12
    34
 */
int main(int argc, char *argv[]) {
    int i;

    for (i=0;i<32;i++) {
	char *c = font[i];
	int x, y;

	printf("\tdc.b ");
	for (y=0;y<4;y++) {
	    for (x=0;x<3;x++) {
		int p = 0;
		if (c[y*12+x*2+0] != ' ')
		    p += 1;
		if (c[y*12+x*2+1] != ' ')
		    p += 2;
		if (c[y*12+x*2+6] != ' ')
		    p += 4;
		if (c[y*12+x*2+7] != ' ')
		    p += 8;
		printf("$%x%s", p, (y==3 && x==2)?"":",");
	    }
	}
	printf("\n");
    }
    return 0;
}
