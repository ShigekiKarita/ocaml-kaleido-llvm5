OCB_FLAGS := -use-ocamlfind
OCB := ocamlbuild $(OCB_FLAGS)
TARGET := toy

all: native # byte profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) $(TARGET).native

byte:
	$(OCB) $(TARGET).byte

profile:
	$(OCB) -tag profile $(TARGET).native

debug:
	$(OCB) -tag debug $(TARGET).byte

test:
	$(OCB) -I test test.native
	./test.native

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
