use std::{
    fmt::Debug,
    io::{self, BufReader, BufWriter, Read, Write},
};

use crate::{
    module::Module,
    object::{Type, VmObject},
    value::Value,
    vm::heap::Collector,
};

pub struct ByteBuffer(Vec<u8>);



pub struct Reader(Box<dyn Read + Send + Sync>);

impl Reader {
    pub fn new<R: Read + Send + Sync + 'static>(reader: R) -> Self {
        Self(Box::new(reader))
    }
}

impl Debug for Reader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Reader").finish_non_exhaustive()
    }
}

impl Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

impl VmObject for Reader {
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        todo!()
    }

    fn field(&self, name: &str) -> Option<&Value> {
        None
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        None
    }

    fn gc(&self, collector: Collector<'_>) {}
}

pub struct Writer<W: ?Sized>(W);

impl<W: Write + ?Sized> Write for Writer<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}
