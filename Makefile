FC =gfortran
CFLAGS=-Wall -Wconversion -fcheck=all -Jinclude
OBJS =autoderiv.o main.o
SRC =autoderiv.f95 main.f95
OUTPTNAME =main.exe

%.o: %.f95
	$(FC) -c $(CFLAGS) $<
$(OUTPTNAME): $(OBJS)
	$(FC) $(CFLAGS) $(SRC) -o $(OUTPTNAME)
.PHONY: clean
clean:
	@rm -f $(OUTPTNAME) *.o ./include/*.mod
