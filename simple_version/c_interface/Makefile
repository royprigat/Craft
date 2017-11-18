CFLAGS = -Wall -g
LFLAGS = -L/usr/local/lib -lSDL2
OBJS   = world.o
PROG = world
CXX = clang


%.o: world.c
	$(CXX) $(CFLAGS) -c $<

$(PROG): $(OBJS)
	$(CXX) $(OBJS) -o $(PROG) $(LFLAGS)

#llvm:
#	clang -S -emit-llvm main.c

clean:
	rm $(PROG) *.o
