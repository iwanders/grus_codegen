# Grus Codegen

A prototype backend / code generator, using [cranelift][cranelift] as a frontend. Mostly to get a
better understanding of the challenges solved by a compiler.


Trying to use the interfaces of [cranelift_codegen][cranelift_codegen] and [regalloc2][regalloc2]
such that it would be easier properly integrate into cranelift if `TargetIsa` becomes implementable.

Naming is hard, [Grus grus](https://en.wikipedia.org/wiki/Common_crane) is the common crane.

# License
License is [`Apache-2.0 WITH LLVM-exception`](./LICENSE), identical to cranelift.

[cranelift]: https://cranelift.dev/
[cranelift_codegen]: https://docs.rs/cranelift-codegen/latest/cranelift_codegen/
[regalloc2]: https://docs.rs/regalloc2/latest/regalloc2/
