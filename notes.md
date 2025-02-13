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

