all:
	cd c ; gmake ppmtologo
	cd intro-hq ; gmake
	cd spiral ; gmake
	cd wave ; gmake
	cd 1check ; gmake
	cd twirl ; gmake
	cd colscr ; gmake
	cd 7up ; gmake
	cd credits ; gmake
	# last but actually first..
	cd loader ; gmake
# loader	1cb0..		1c51..2000 free (but music copied to 1cb0..)
# intro-hq	-""-		1ca2..2000 free (font can be packed more?)
# spiral	1cb0..		1b85..2000 free
# wave		1cb0..		1b8f..2000 free
# 1check	-""-		1b7d..2000 free
# twirl		-""-		1b1a..2000 free
# colsrc	1bc0..		1a00..2000 free (still some free)
# 7up		-""-		1c35..2000 free
# credits	-""-		1c71..2000 free

# loader copies video matrix, copies and starts first music (Hit the road Jack)
# intro-hq uses the music loaded, fades it when next part loaded
# spiral starts the second music
# rest of the parts use the same music
# TODO: fix the tempo jumps when changing parts

# loader intro spiral		folk.sng
# wave 1check		mmix.sng	//vicual.sng
# twirl colscr		cutie.sng
# 7up credits		voyage.sng

#
#~/work/src/fdc/fdwrite -f loader/disk.d81 -d /dev/fd0
#
