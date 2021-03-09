use std::fmt::Debug;

fn process_debugs<P: DebugProcessor>(p: P) {
    p.process("a");
    p.process(3);
}

trait DebugProcessor {
    fn process<D: Debug>(&self, d: D);
}

impl<F, D1: Debug> DebugProcessor for F
where
    F: Fn(D1),
{
    fn process<D: Debug>(&self, d: D) {
        self(d);
    }
}

fn main() {}
