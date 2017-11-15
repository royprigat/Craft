CFLAGS = -Wall -g
LFLAGS = -L/usr/local/lib -lSDL2
OBJS   = main.o
PROG = hello_world
CXX = clang


%.o: main.c
	$(CXX) $(CFLAGS) -c $<

$(PROG): $(OBJS)
	$(CXX) $(OBJS) -o $(PROG) $(LFLAGS)

#llvm:
#	clang -S -emit-llvm main.c

clean:
	rm $(PROG) *.o
