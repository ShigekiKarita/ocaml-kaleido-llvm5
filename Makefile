OCB_FLAGS := -use-ocamlfind
OCB := ocamlbuild $(OCB_FLAGS)
TARGET := toy

all:
	$(OCB) $(TARGET).native

clean:
	$(OCB) -clean

test: all
	$(OCB) -I test test.native
	./test.native
	./toy.native test.txt

.PHONY: all clean byte native profile debug sanity test

install-llvm:
	wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
	echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main" >> /etc/apt/sources.list
	sudo apt-get update
	sudo apt-get install -y llvm-5.0-dev

install-ocaml-bin:
	sudo apt-get install opam
	opam update
	opam switch 4.06.0+flambda

install-ocaml-packages:
	opam install batteries menhir merlin llvm.5.0.0 ctypes-foreign utop
