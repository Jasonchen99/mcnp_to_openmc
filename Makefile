mcnp_to_openmc: 
	del *.o
	del *.mod
	gfortran -c constants.f95
	gfortran -c general.f95
	gfortran -c declrs.f95
	gfortran -c global.f95
	gfortran -c input.f95
	gfortran -c xmlparse.f90
	gfortran -c xmlwrite.f90
	gfortran constants.o general.o declrs.o global.o input.o xmlparse.o xmlwrite.o main.f90 -o mcnp_to_openmc
	del *.o
	del *.mod