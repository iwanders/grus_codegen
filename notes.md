# Notes

- [x] Assigning constant integers.
- [x] Moving between registers.
- [x] Adding integers.
- [x] Naive register allocator for single block, without stack.
- [ ] Handle spilling in register allocator.
- [ ] Handle multiple blocks (so for loops first, then jumptables?)
- [ ] Handle if statement.
    - How do we handle situations like `brif v0, block5(v1), block5(v2)` where we go into the same
      block with different values?
    - The transitions between blocks place constraints on the registers and departure and landing?
- Fold immediate values into the x86 instruction? How do we support this? Need to see that the value
  in the SSA is only used once, and then collapse it? This holds for (IAdd, ISub, IMul)
- Add an indirection between the regalloc Function and IrFunction?
- Visualiser for the register allocator?
- [x] Subtracting integers.
- Add stack frame setup for functions?
- [x] Multiplication of integers.


## Useful commands

### Compile to wasm.
Compile `z.rs` to wasm32;
```
rustc z.rs  --edition 2021 --crate-type  cdylib --target wasm32-unknown-unknown
```

### Wasmtime ir

With
```wast
(module
  (func (export "diverging_converging") (param i32 i32) (result i32)
    (if (result i32) (local.get 0)
      (then
        local.get 1
      )
      (else
        local.get 0
      )
    )
    i32.const 1
    i32.add
  )
)
```

```
wasmtime$ cargo r --release -- explore  <file_to_wasm/wast>
```


### Register plot
```
cargo r  --example tool -- reg-alloc filetests/0006b* --write-svg /tmp/registers.svg --allocator Winged --write-regalloc-serialize /tmp/foo.json
```
