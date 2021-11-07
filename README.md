
# BF

[BF interpreter/compiler](https://en.wikipedia.org/wiki/Brainfuck)

Change `compile` to `interpret` and vice versa to switch between compiling and
interpreting BF instructions.

Execute a program by running `stack run -- /path/to/program.bf`.

Note that the compiled variant generates an object file. The final executable
can be created by running `clang -o bf bf.o`
