Main: librust.a Main.hs loop.o
	stack --resolver lts-8.12 exec --package criterion -- ghc -O2 -o Main Main.hs librust.a loop.o -fllvm

librust.a: rust.rs
	rustc --crate-type staticlib rust.rs -C opt-level=2

loop.o: loop.c
	gcc -O2 -o loop.o -c loop.c
#	clang -O3 -o loop.o -c loop.c
