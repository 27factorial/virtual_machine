#![deny(fuzzy_provenance_casts)]
#![deny(unused_must_use)]
#![allow(unused)]

// Used for a safer implementation of GcBox.
#![feature(ptr_metadata)]
// Used to ensure GcBox follows the upcoming pointer provenance rules.
#![feature(strict_provenance)]

mod object;
mod ops;
mod program;
mod serde_impl;
mod string;
mod utils;
mod value;
mod vm;

fn main() {}
