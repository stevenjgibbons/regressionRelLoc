#
ALLSOURCECODE=  \
	csargm.f \
	dtlnrd.f \
	eabslf.f \
	indlef.f \
	indlsf.f \
	irlsra.f \
	irwmps.f \
	isleap.f \
	md2doy.f \
	mdpvun.f \
	nevelr.f \
	nlspef.f \
	nspslr.f \
	osevps.f \
	osspps.f \
	sach2e.f \
	siwmps.f \
	vdcr2a.f
#
VALUABLE= \
        $(ALLSOURCECODE)
#
SOURCES= \
        $(ALLSOURCECODE)
#
OPTIM=	  -O3
LIBDIR=  ..
SUBSLIB= $(LIBDIR)/subslib.a
FORTRAN=  gfortran
.PRECIOUS: 	$(SUBSLIB)
#
$(SUBSLIB):	$(SOURCES)
	$(FORTRAN) $(OPTIM) $(STDLIBS) $? -c
	ar rv $(SUBSLIB) *.o
	ranlib $(SUBSLIB)
	chmod 644 $(SUBSLIB)
	\rm *.o
#
backup:
	cp -ip $(VALUABLE) ./BACKUP ; \
	cd ./BACKUP ; \
	\rm -f *.gz ; \
	gzip $(VALUABLE) ; \
	cd ../
#
