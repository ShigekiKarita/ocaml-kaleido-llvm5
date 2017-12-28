# OCaml Kaleidoscope in LLVM5.0.0

wip

rewriting [the out-of-date OCaml tutorial in LLVM5.0.0](http://releases.llvm.org/5.0.0/docs/tutorial/index.html) with [the latest OCaml API](https://llvm.moe/ocaml/index.html)


## DONE
- chapter 4: adding JIT optimization
-- `ExcecutionEngine.run_function` is obsolete. use `get_function_address` in `toplevel.ml`. see http://releases.llvm.org/5.0.0/docs/tutorial/LangImpl04.html#adding-a-jit-compiler
-- `DataLayout.add` is gone?

## TODO
- chapter 5 (extending the language control flow)
- rewrite lexer/parser with menhir

``` console
$ opam install batteries menhir merlin llvm.5.0.0 ctypes-foreign utop camlp4
$ make
$ ./toy.native
ready> 1+2;
parsed a top-level expr

define double @_anonymous_func_1() {
entry:
  ret double 3.000000e+00
}
Evaluated to 3.000000
ready> def f(x) x * 2;
parsed a function definition.

define double @f(double %x) {
entry:
  %multmp = fmul double %x, 2.000000e+00
  ret double %multmp
}
ready> def g(x) x * x;
parsed a function definition.

define double @g(double %x) {
entry:
  %multmp = fmul double %x, %x
  ret double %multmp
}
ready> g(f(3));
parsed a top-level expr

define double @_anonymous_func_2() {
entry:
  %calltmp = call double @f(double 3.000000e+00)
  %calltmp1 = call double @g(double %calltmp)
  ret double %calltmp1
}
Evaluated to 36.000000
```

