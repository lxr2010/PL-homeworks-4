open Expr

// test
let fact = e => Letfn(
  "fact",
  list{"n"},
  If(
    Var("n"),
    Prim(Mul, list{Var("n"), Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)})})}),
    Cst(1),
  ),
  e,
)

let sum = e => Letfn(
  "sum",
  list{"n"},
  If(
    Var("n"),
    Prim(Add, list{Var("n"), Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)})})}),
    Cst(0),
  ),
  e,
)

let fib = e => Letfn(
  "fib",
  list{"n"},
  If(
    Var("n"),
    If(
      Prim(Add, list{Var("n"), Cst(-1)}),
      Prim(
        Add,
        list{
          Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)})}),
          Prim(Self, list{Prim(Add, list{Var("n"), Cst(-2)})}),
        },
      ),
      Cst(1),
    ),
    Cst(1),
  ),
  e,
)

let fact_tail = e => Letfn(
  "fact_tail",
  list{"n", "acc"},
  If(
    Var("n"),
    Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)}), Prim(Mul, list{Var("n"), Var("acc")})}),
    Var("acc"),
  ),
  Letfn("fact", list{"n"}, App("fact_tail", list{Var("n"), Cst(1)}), e),
)

let test_fact_tail = fact_tail(
  Let("a", Cst(4), Letfn("id", list{"x"}, Var("x"), App("id", list{App("fact", list{Var("a")})}))),
)

// expected result
let test_fact = fact(App("fact", list{Cst(5)}))
let test_sum = sum(App("sum", list{Cst(5)}))
let test_fib = fib(App("fib", list{Cst(7)}))

Js.log(compile_and_execute(test_fact) === 120)
Js.log(compile_and_execute(test_sum) === 15)
Js.log(compile_and_execute(test_fib) === 21)
Js.log(compile_and_execute(test_fact_tail) === 24)

Js.log(compile_encode_and_execute(test_fact) === 120)
Js.log(compile_encode_and_execute(test_sum) === 15)
Js.log(compile_encode_and_execute(test_fib) === 21)
Js.log(compile_encode_and_execute(test_fact_tail) === 24)

// print the instructions
let addr = ref(0)
let print = s => {
  Js.log(`${Belt.Int.toString(addr.contents)}: ${s}`)
  addr := addr.contents + 1
}

let addr_print = instrs => {
  let addr = ref(0)
  for i in 0 to Array.length(instrs) - 1 {
    let instr = instrs[i]
    Js.log(`${Belt.Int.toString(addr.contents)}: ${Stack.toString(instr)}`)
    addr := addr.contents + Stack.instr_length(instr)
  }
}

let test = Let(
  "a",
  Cst(2),
  Letfn(
    "cube",
    list{"x"},
    Letfn(
      "square",
      list{"x"},
      Prim(Mul, list{Var("x"), Var("x")}),
      Prim(Mul, list{App("square", list{Var("x")}), Var("x")}),
    ),
    App("cube", list{Var("a")}),
  ),
)

Js.log(compile_and_execute(test) === 8)
Js.log(compile_encode_and_execute(test) === 8)

let write: (Js.Typed_array.Int32Array.t, string) => unit = %raw(`
function (code_int32, path) {
  const fs = require("fs");
  const buf = new Uint8Array(code_int32.buffer);
  fs.writeFileSync(path, buf);
}
`)

// Js.log(compile_encode(test_fact))
// let _ = write(compile_encode(test_fact), "./code1")
// let code = compile(test_fact)
// List.iter(print, List.map(Stack.toString, code))
// let code_array = Array.of_list(code)
// addr_print(Stack.decode(Stack.encode(code_array)))

// let mem = fst(Stack.encode(code_array))
// let init_pc = Stack.getInitPc(code_array)

// let real_vm = Stack.RealVm.initVm(mem, init_pc)
// Js.log(Stack.RealVm.run(real_vm))
