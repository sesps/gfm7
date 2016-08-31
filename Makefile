src/gfm7: src/gfm7.for src/rtcomm0.f src/rtcomm65.f
	gfortran -fno-align-commons -fno-automatic -finit-local-zero -ffixed-line-length-80 -g -o gfm7 src/gfm7.for
rt: rt.f rtcomm0.f rtcomm65.f
	gfortran -fno-align-commons -finit-local-zero -ffixed-line-length-80 -g -o rt rt.f
