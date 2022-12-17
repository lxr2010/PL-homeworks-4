type instr =
  | Cst(int)
  | Add
  | Mul
  | Var(int)
  | Pop
  | Swap
  | Label(string)
  | Call(string, int)
  | Ret(int) // num of args
  | IFZERO(string)
  | GOTO(string)
  | Exit // Halt the machine
type instrs = array<instr>
type operand = int
type stack = array<operand>

type vm = {
  code: array<instr>,
  stack: array<operand>,
  mutable pc: int,
  mutable sp: int,
}

let debug = false
// let debug = true

let getExn = Belt.Option.getExn

let getLabel = (code, label) => {
  getExn(Belt.Array.getIndexBy(code, x => x == Label(label)))
}

let initStack = length => {
  let stack = []
  for _ in 1 to length {
    let _ = Js.Array.push(0, stack)
  }
  stack
}

let getInitPc = (code: array<instr>) => {
  Js.Array.findIndex(i => {i == Call("main", 0)}, code)
}

let push = (vm: vm, x) => {
  vm.stack[vm.sp] = x
  vm.sp = vm.sp + 1
}

let pop = (vm: vm): operand => {
  vm.sp = vm.sp - 1
  let t = vm.stack[vm.sp]
  if debug {
    vm.stack[vm.sp] = 0
  }
  t
}

let initVm = code => {
  code,
  stack: initStack(40),
  pc: getInitPc(code),
  sp: 0,
}

let run = (vm: vm) => {
  let break = ref(false)

  while !break.contents {
    if debug {
      Js.log(Js_array.toString(vm.stack))
      Js.log(vm.pc)
    }

    let ins = vm.code[vm.pc]
    vm.pc = vm.pc + 1
    switch ins {
    | Cst(i) => push(vm, i)

    | Add => {
        let a = pop(vm)
        let b = pop(vm)
        push(vm, a + b)
      }

    | Mul => {
        let a = pop(vm)
        let b = pop(vm)
        push(vm, a * b)
      }

    | Var(i) => {
        let var = vm.stack[vm.sp - i - 1]
        push(vm, var)
      }

    | Pop => {
        let _ = pop(vm)
      }

    | Swap => {
        let a = pop(vm)
        let b = pop(vm)
        let _ = push(vm, a)
        let _ = push(vm, b)
      }

    | Label(_) => () // do nothing

    | Call(f, n) => {
        let next_pc = getLabel(vm.code, f)
        let _ = Js.Array.spliceInPlace(~pos=vm.sp - n, ~remove=0, ~add=[vm.pc], vm.stack)
        vm.sp = vm.sp + 1
        vm.pc = next_pc
      }

    | Ret(n) => {
        let res = pop(vm)
        vm.sp = vm.sp - n
        let next_pc = pop(vm)
        let _ = push(vm, res)
        vm.pc = next_pc
      }

    | IFZERO(to) => {
        let cond = pop(vm)
        if cond === 0 {
          vm.pc = getLabel(vm.code, to)
        }
      }

    | GOTO(to) => vm.pc = getLabel(vm.code, to)

    | Exit => break := true
    }
  }
  pop(vm)
}

let toString = instr => {
  switch instr {
  | Cst(i) => `Cst(${Belt.Int.toString(i)})`
  | Add => "Add"
  | Mul => "Mul"
  | Var(i) => `Var(${Belt.Int.toString(i)})`
  | Pop => "Pop"
  | Swap => "Swap"
  | Label(s) => `Label ${s}`
  | Call(s, n) => `Call ${s} ${Belt.Int.toString(n)}`
  | Ret(n) => `Ret ${Belt.Int.toString(n)}`
  | IFZERO(s) => `IFZERO ${s}`
  | GOTO(s) => `GOTO ${s}`
  | Exit => "Exit"
  }
}

let initArray = (length: int) => {
  let array = []
  for _ in 1 to length {
    let _ = Js.Array.push(0, array)
  }
  array
}

let fst = ((a, _)) => a
let snd = ((_, b)) => b
let max = (a, b) => a > b ? a : b
let min = (a, b) => a > b ? b : a

let instr_length = (instr: instr) => {
  switch instr {
  | Label(_) => 0
  | Add | Mul | Pop | Swap | Exit => 1
  | Cst(_) | Var(_) | Ret(_) | IFZERO(_) | GOTO(_) => 2
  | Call(_, _) => 3
  }
}

let encode = (instrs: instrs) => {
  let get_offset = (label_index: int, cur: int) => {
    let start = min(label_index, cur)
    let end_ = max(label_index, cur)
    let segment = Js.Array.slice(~start, ~end_, instrs)
    let offset = Array.fold_left((acc, instr) => {acc + instr_length(instr)}, 0, segment)
    label_index > cur ? offset : -offset
  }

  let make = Js.Typed_array.Int32Array.make
  let setArrayOffset = Js.Typed_array.Int32Array.setArrayOffset
  let code_seg = make(initArray(0xffff))
  let p = ref(0)
  for cur in 0 to Array.length(instrs) - 1 {
    let instr = instrs[cur]
    switch instr {
    | Cst(i) => {
        setArrayOffset([0, i], p.contents, code_seg)
        p := p.contents + 2
      }

    | Add => {
        setArrayOffset([1], p.contents, code_seg)
        p := p.contents + 1
      }

    | Mul => {
        setArrayOffset([2], p.contents, code_seg)
        p := p.contents + 1
      }

    | Var(i) => {
        setArrayOffset([3, i], p.contents, code_seg)
        p := p.contents + 2
      }

    | Pop => {
        setArrayOffset([4], p.contents, code_seg)
        p := p.contents + 1
      }

    | Swap => {
        setArrayOffset([5], p.contents, code_seg)
        p := p.contents + 1
      }

    | Label(_) => () // do nothing

    | Call(s, i) => {
        let label_index = getLabel(instrs, s)
        let offset = get_offset(label_index, cur)
        setArrayOffset([6, offset, i], p.contents, code_seg)
        p := p.contents + 3
      }

    | Ret(i) => {
        setArrayOffset([7, i], p.contents, code_seg)
        p := p.contents + 2
      }

    | IFZERO(s) => {
        let label_index = getLabel(instrs, s)
        let offset = get_offset(label_index, cur)
        setArrayOffset([8, offset], p.contents, code_seg)
        p := p.contents + 2
      }

    | GOTO(s) => {
        let label_index = getLabel(instrs, s)
        let offset = get_offset(label_index, cur)
        setArrayOffset([9, offset], p.contents, code_seg)
        p := p.contents + 2
      }

    | Exit => {
        setArrayOffset([10], p.contents, code_seg)
        p := p.contents + 1
      }
    }
  }
  (code_seg, p.contents)
}

let decode = ((mem, len)) => {
  let code = []
  let push = Js.Array.push
  let unsafe_get = Js.Typed_array.Int32Array.unsafe_get
  let p = ref(0)
  let break = ref(false)
  while !break.contents {
    let opcode = unsafe_get(mem, p.contents)
    switch opcode {
    | 0 => {
        let operand = unsafe_get(mem, p.contents + 1)
        let _ = push(Cst(operand), code)
        p := p.contents + 2
      }

    | 1 => {
        let _ = push(Add, code)
        p := p.contents + 1
      }

    | 2 => {
        let _ = push(Mul, code)
        p := p.contents + 1
      }

    | 3 => {
        let operand = unsafe_get(mem, p.contents + 1)
        let _ = push(Var(operand), code)
        p := p.contents + 2
      }

    | 4 => {
        let _ = push(Pop, code)
        p := p.contents + 1
      }

    | 5 => {
        let _ = push(Swap, code)
        p := p.contents + 1
      }

    | 6 => {
        let offset = unsafe_get(mem, p.contents + 1)
        let arity = unsafe_get(mem, p.contents + 2)
        let _ = push(Call(Belt.Int.toString(offset), arity), code)
        p := p.contents + 3
      }

    | 7 => {
        let arity = unsafe_get(mem, p.contents + 1)
        let _ = push(Ret(arity), code)
        p := p.contents + 2
      }

    | 8 => {
        let offset = unsafe_get(mem, p.contents + 1)
        let _ = push(IFZERO(Belt.Int.toString(offset)), code)
        p := p.contents + 2
      }

    | 9 => {
        let offset = unsafe_get(mem, p.contents + 1)
        let _ = push(GOTO(Belt.Int.toString(offset)), code)
        p := p.contents + 2
      }

    | 10 => {
        let _ = push(Exit, code)
        p := p.contents + 1
      }

    | _ => assert false
    }
    break := p.contents >= len
  }
  code
}

module RealVm = {
  type vm = {
    code: Js.Typed_array.Int32Array.t,
    stack: array<int>,
    mutable pc: int,
    mutable sp: int,
  }
  let initVm = (code, pc) => {
    code,
    stack: initStack(40),
    pc,
    sp: 0,
  }

  let push = (vm: vm, x: operand) => {
    vm.stack[vm.sp] = x
    vm.sp = vm.sp + 1
  }

  let pop = (vm: vm): operand => {
    vm.sp = vm.sp - 1
    vm.stack[vm.sp]
  }

  let run = (vm: vm) => {
    let unsafe_get = Js.Typed_array.Int32Array.unsafe_get

    let break = ref(false)
    while !break.contents {
      let opcode = unsafe_get(vm.code, vm.pc)
      // Js.log(`${Belt.Int.toString(vm.pc)}: ${Belt.Int.toString(opcode)}`)
      switch opcode {
      | 0 => {
          // Cst(i)
          let i = unsafe_get(vm.code, vm.pc + 1)
          let _ = push(vm, i)

          vm.pc = vm.pc + 2
        }

      | 1 => {
          // Add
          let a = pop(vm)
          let b = pop(vm)
          push(vm, a + b)

          vm.pc = vm.pc + 1
        }

      | 2 => {
          // Mul
          let a = pop(vm)
          let b = pop(vm)
          push(vm, a * b)

          vm.pc = vm.pc + 1
        }

      | 3 => {
          // Var(i)
          let i = unsafe_get(vm.code, vm.pc + 1)
          let var = vm.stack[vm.sp - i - 1]
          push(vm, var)

          vm.pc = vm.pc + 2
        }

      | 4 => {
          // Pop
          let _ = pop(vm)

          vm.pc = vm.pc + 1
        }

      | 5 => {
          // Swap
          let a = pop(vm)
          let b = pop(vm)
          let _ = push(vm, a)
          let _ = push(vm, b)
          vm.pc = vm.pc + 1
        }

      | 6 => {
          // Call(offset, arity)
          let offset = unsafe_get(vm.code, vm.pc + 1)
          let arity = unsafe_get(vm.code, vm.pc + 2)

          let next_pc = vm.pc + offset
          let _ = Js.Array.spliceInPlace(~pos=vm.sp - arity, ~remove=0, ~add=[vm.pc + 3], vm.stack)
          vm.sp = vm.sp + 1
          vm.pc = next_pc
        }

      | 7 => {
          // Ret(arity)
          let arity = unsafe_get(vm.code, vm.pc + 1)
          let res = pop(vm)
          vm.sp = vm.sp - arity
          let next_pc = pop(vm)
          let _ = push(vm, res)
          vm.pc = next_pc
        }

      | 8 => {
          // IFZERO(offset)
          let offset = unsafe_get(vm.code, vm.pc + 1)
          let to = vm.pc + offset
          let cond = pop(vm)
          if cond === 0 {
            vm.pc = to
          } else {
            vm.pc = vm.pc + 2
          }
        }

      | 9 => {
          // GOTO(offset)
          let offset = unsafe_get(vm.code, vm.pc + 1)
          vm.pc = vm.pc + offset
        }

      | 10 => break := true
      | _ => assert false
      }
      // Js.log(`After: ${Belt.Int.toString(vm.sp)}`)
      // Js.log(vm.stack)
    }
    pop(vm)
  }
}
