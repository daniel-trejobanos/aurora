F90  = pgf90
FLGS =  -byteswapio -Bstatic_pgi

###F90  = ifort
###FLGS =  -convert big_endian

###F90  = gfortran
###FLGS =  -fconvert=big-endian -frecord-marker=4

LIBS = 

TARGT1 =  asc2bin
TARGT2 =  bin2asc

OBJCTS1 = \
asc2bin.o
OBJCTS2 = \
bin2asc.o

$(TARGT1): $(OBJCTS1)
	$(F90) -o $(TARGT1) $(FLGS) $(OBJCTS1) $(LIBS)
	make bin2asc
.f.o	:
	$(F90) -c -o $@ $(FLGS) $<

$(TARGT2): $(OBJCTS2)
	$(F90) -o $(TARGT2) $(FLGS) $(OBJCTS2) $(LIBS)

clean:
	rm $(TARGT1) $(TARGT2) $(OBJCTS1) $(OBJCTS2)
