// Used for a safer implementation of GcBox.
#![feature(ptr_metadata)]
// Used to ensure GcBox follows the upcoming pointer provenance rules.
#![feature(strict_provenance)]
// To allow trait upcasting for VmObject. Should be mostly complete and stable soon.
// #![feature(trait_upcasting)]
// Enforces strict pointer provenenance for the above feature.
#![deny(fuzzy_provenance_casts)]
// Forces the use of unsafe blocks for unsafe operations, even when inside of an unsafe function.
#![deny(unsafe_op_in_unsafe_fn)]

use program::Program;
use std::time::Instant;
use value::Value;
use vm::ops::OpCode;
use vm::Vm;

pub mod object;
pub mod program;
pub mod serde_impl;
pub mod symbol;
pub mod utils;
pub mod value;
pub mod vm;

fn main() {
    let mut program = Program::new();

    let main_sym = program.define_symbol("main");
    let adder_sym = program.define_symbol("adder");

    let adder = program
        .define_function(adder_sym, [OpCode::Add, OpCode::Ret])
        .expect("failed to define `adder` function");

    program
        .define_function(
            main_sym,
            [
                // Initialize a counter to 50 million and store the counter in local variable 0
                OpCode::Push(Value::UInt(50_000_000)),
                OpCode::ReserveImm(1),
                // Load the counter from local variable 0, and if it's zero, jump to the end of
                // the program.
                OpCode::Load(0),
                OpCode::EqImm(Value::UInt(0)),
                OpCode::JumpCondImm(13),
                // else...
                // load the value from local variable 0, subtract 1, and store the new counter
                // back in the local variable 0.
                OpCode::Load(0),
                OpCode::SubImm(Value::UInt(1)),
                OpCode::Store(0),
                // Push two 2s onto the stack
                OpCode::Push(Value::UInt(2)),
                OpCode::Dup,
                // Call a function which pops them from the stack, adds them, then returns
                OpCode::CallImm(adder),
                // Remove the added value (it's not actually used)
                OpCode::Pop,
                // Jump back to the counter check above
                OpCode::JumpImm(2),
                // halt the virtual machine
                OpCode::Halt,
            ],
        )
        .expect("failed to define `main` function");

    let mut vm = Vm::new(program).expect("failed to create VM");

    let start = Instant::now();
    vm.run().expect("failed to run vm");
    let elapsed = start.elapsed();
    eprintln!("{elapsed:?}")
}
