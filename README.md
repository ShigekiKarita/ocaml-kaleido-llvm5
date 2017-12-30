# OCaml Kaleidoscope in LLVM5.0.0

wip

rewriting [the out-of-date OCaml tutorial in LLVM5.0.0](http://releases.llvm.org/5.0.0/docs/tutorial/index.html) with [the latest OCaml API](https://llvm.moe/ocaml/index.html)


## DONE
- chapter 4: adding JIT optimization
  - `ExcecutionEngine.run_function` is obsolete. use `get_function_address` in `toplevel.ml`. see http://releases.llvm.org/5.0.0/docs/tutorial/LangImpl04.html#adding-a-jit-compiler
  - `DataLayout.add` is gone?
- rewrite lexer/parser with menhir
- support file/stdin evaluations
- chapter 5: extending the language control flow
  - need to change `Llvm.build_add variable step_val "nextvar" builder` to `Llvm.build_fadd ...`
  - need to use `gcc` or `clang` instead of `g++` (bindings.c will be regarded as C++)

## TODO
- fix precedence of operators
- chapter 6: Extending the Language: User-defined Operators


### REPL

print AST and LLVM-IR to stderr

``` llvm
$ opam install batteries menhir merlin llvm.5.0.0 ctypes-foreign utop
$ make
$ ./toy.native
ready> def f(x, y) x - y;
AST: (Ast.Definition
   (Ast.Function ((Ast.Prototype ("f", [|"y"; "x"|])),
      (Ast.Binary ("-", (Ast.Variable "x"), (Ast.Variable "y"))))))
parsed a function definition.

define double @f(double %y, double %x) {
entry:
  %aritmp = fsub double %x, %y
  ret double %aritmp
}
ready> extern sin(x);
AST: (Ast.Extern (Ast.Prototype ("sin", [|"x"|])))
parsed an extern.

declare double @sin(double %x)
ready> extern cos(x); f(cos(1.0), sin(1.0));
AST: (Ast.Extern (Ast.Prototype ("cos", [|"x"|])))
parsed an extern.

declare double @cos(double %x)
ready> AST: (Ast.Toplevel
   (Ast.Call ("f",
      [|(Ast.Call ("sin", [|(Ast.Number 1.)|]));
        (Ast.Call ("cos", [|(Ast.Number 1.)|]))|]
      )))
parsed a top-level expr

define double @_anonymous_func_0() {
entry:
  %calltmp2 = call double @f(double 0x3FEAED548F090CEE, double 0x3FE14A280FB5068C)
  ret double %calltmp2
}
Evaluated to -0.301169
```

### error info

lexer support only

```
$ ./toy.native fail.txt
ready> Lexer.Error unknown token: '%' at (file: fail.txt, line: 1, col: 2)
ready> ; ModuleID = 'my cool jit'
source_filename = "my cool jit"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

$ ./toy.native
ready> 1 @ 3;
Lexer.Error unknown token: '@' at (file: <stdin>, line: 1, col: 74)
```
