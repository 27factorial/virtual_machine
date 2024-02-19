// Used for a safer implementation of GcBox.
#![feature(ptr_metadata)]
// Used to ensure GcBox follows the upcoming pointer provenance rules.
#![feature(strict_provenance)]
// To allow trait upcasting for VmObject. Should be mostly complete and stable soon.
#![feature(trait_upcasting)]
// Enforces strict pointer provenenance for the above feature.
#![deny(fuzzy_provenance_casts)]
// Forces #[must_use] return values to be used in *some* way.
#![deny(unused_must_use)]

use program::Program;
use vm::{ops::OpCode, Vm};

mod object;
mod program;
mod serde_impl;
mod string;
mod utils;
mod value;
mod vm;

// TODO: Local variables for functions inside CallFrame

fn main() {
    let mut program = Program::new();
    let main_func = program.define_symbol("main");

    program
        .define_function(main_func, [OpCode::Halt])
        .expect("Failed to define main function");

    let mut vm = Vm::new(program).expect("Failed to create vm");

    let _ = vm.run();
}
