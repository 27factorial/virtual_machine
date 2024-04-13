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
// Allows the define_builtins macro to work in crate::vm::builtin.
#![recursion_limit = "256"]

use module::Module;
use std::time::Instant;
use value::Value;
use vm::ops::OpCode;
use vm::Vm;

pub mod module;
pub mod object;
pub mod serde_impl;
pub mod symbol;
pub mod utils;
pub mod value;
pub mod vm;

const VALID_MAGIC: [u8; 4] = *b"PFVM";

fn main() {
    let mut module = Module::new();

    let main_sym = module.define_symbol("main::main");
    let adder_sym = module.define_symbol("main::adder");

    let adder = module
        .define_function(adder_sym, [OpCode::Add, OpCode::Ret])
        .expect("failed to define `adder` function");

    module
        .define_function(
            main_sym,
            [
                // Initialize a counter to 50 million and store the counter in local variable 0
                OpCode::Push(Value::Int(50_000_000)),
                OpCode::ReserveImm(1),
                // Load the counter from local variable 0, and if it's zero, jump to the end of
                // the program.
                OpCode::Load(0),
                OpCode::EqImm(Value::Int(0)),
                OpCode::JumpCondImm(9),
                // else...
                // load the value from local variable 0, subtract 1, and store the new counter
                // back in the local variable 0.
                OpCode::Load(0),
                OpCode::SubImm(Value::Int(1)),
                OpCode::Store(0),
                // Push two 2s onto the stack
                OpCode::Push(Value::Int(2)),
                OpCode::Dup,
                // Call a function which pops them from the stack, adds them, then returns
                // OpCode::Add,
                OpCode::CallImm(adder),
                // Remove the added value (it's not actually used)
                OpCode::Pop,
                // Jump back to the counter check above
                OpCode::JumpImm(-11),
                // halt the virtual machine
                OpCode::Halt,
            ],
        )
        .expect("failed to define `main` function");

    let mut vm = Vm::new(module);

    let start = Instant::now();
    vm.run().expect("failed to run vm");
    let elapsed = start.elapsed();
    println!("{elapsed:?}")
}
