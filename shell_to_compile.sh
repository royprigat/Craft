#Martin testing/ref only

#!/bin/sh

#MicroC just prints out the generated LLVM IR which can be seen when running it (Done in microc.ml)
#So we can just pipe it into a new file and then use that in the steps below

#when starting new terminal session this path is lost each time....
#export PATH=$PATH:/usr/local/opt/llvm/bin

#make a .ll file (the LLVM IR made by MicroC compiler)
#./craft.native helloworld_stupid.crf > helloworld_stupid.ll

#llc is the compiler for LLVM IR and makes a .s file
#llc helloworld_stupid.ll > helloworld_stupid.s

#use .s to make an executable with cc compiler. linking the pringbig function in this step
gcc -o helloworld_stupid.exe helloworld_stupid.s world.o -L/usr/local/lib -lSDL2

