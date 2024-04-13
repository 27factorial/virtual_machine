use std::io::{self, BufReader, BufWriter, Read, Write};

enum Reader<R: Read> {
    Unbuffered(R),
    Buffered(BufReader<R>),
}

impl<R: Read> Read for Reader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Self::Unbuffered(reader) => reader.read(buf),
            Self::Buffered(reader) => reader.read(buf),
        }
    }
}

enum Writer<W: Write> {
    Unbuffered(W),
    Buffered(BufWriter<W>),
}

impl<W: Write> Write for Writer<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Self::Unbuffered(writer) => writer.write(buf),
            Self::Buffered(writer) => writer.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::Unbuffered(writer) => writer.flush(),
            Self::Buffered(writer) => writer.flush(),
        }
    }
}

enum ReaderWriter<Rw: Read + Write> {
    Unbuffered(Rw),
    Buffered(BufReader<Rw>, BufWriter<Rw>),
}
