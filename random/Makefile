# FLAGS = -g -fPIC -Wall
# LIBDIR = -Llib
# LIBS = 
# CC = g++
# include 

# DYNAMIC = -dynamic
# SUPPRESS = -flat_namespace -undefined suppress
 



 SHELL=/bin/sh
USRLIBDIR=-Llibunix
#LIBS = -lobj -lcudd -lmtr -lst -lutil -lepd
CFLAGS=-shared -fPIC -ffast-math -Wall -O3
DEBUGOPTIONS=

#OBJDIR=svm-Dlight
#OBJECTS = rng.o
# OBJECTS = countainer.o\
# 	YapHelpers.o
# kfoil_main.o \
#	kfoil_yap_interface.o \
#	Task.o \
#	MulticlassKernelTargetAlignment.o \
#	utility.o \
#	$(OBJDIR)/svm_common.o \
#	$(OBJDIR)/plugin.o \
#	$(OBJDIR)/sparsevector.o \
#	$(OBJDIR)/svm_learn.o \
#	$(OBJDIR)/svm_hideo.o 

TARGETS=sampling.so 

all: sampling.so

sampling.so : sampling.o
	g++ -o sampling.so sampling.o -shared $(USRLIBDIR) -lorocos-bfl -L/usr/local/lib -lYap -lgsl -lgslcblas -lm -fPIC 

#	 g++ $(USRLIBDIR)  $(LIBS) -p -shared -o $@ $+  

#g++ -o sampling.so sampling.o -shared -Llib -lobj -lcudd -lmtr -lst -lutil -lepd

%.o:%.cpp
	g++ ${CFLAGS} $(DEBUGOPTIONS)  -g -O0 -c $< -o $@

%.o:%.c
	gcc ${CFLAGS} $(DEBUGOPTIONS) -g -O0 -c $< -o $@

clean:
	rm -f *.o *~ ${TARGETS}


test: sampling.so
	echo "load_foreign_files(['sampling'],[],init_my_predicates). poisson(40,N1),normal(1,1,N2), format('sampling ~q ~q~n',[N1,N2]),halt. halt(1)." |~/yap-cvs/yap

#install: all
#	 cp ${TARGETS} ~/COMMON/lib/
