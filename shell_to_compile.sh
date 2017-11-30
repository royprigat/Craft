#Martin testing/ref only

#!/bin/sh

# Export command for llc compiler errors
#export PATH=$PATH:/usr/local/opt/llvm/bin

#MicroC just prints out the generated LLVM IR which can be seen when running it (Done in microc.ml)
#So we can just pipe it into a new file and then use that in the steps below

#make a .ll file (the LLVM IR made by MicroC compiler)
./craft.native helloworld.crf > helloworld.ll

#llc is the compiler for LLVM IR and makes a .s file
llc helloworld.ll > helloworld.s

#use .s to make an executable with cc compiler. linking the pringbig function in this step
gcc -o helloworld.exe helloworld.s world.o -L/usr/local/lib -lSDL2
