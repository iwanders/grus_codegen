# Notes

- [x] Assigning constant integers.
- [x] Moving between registers.
- [x] Adding integers.
- [x] Naive register allocator for single block, without stack.
- [ ] Handle spilling in register allocator.
- [ ] Handle multiple blocks (so for loops first, then jumptables?)
- [ ] Handle if statement.
- Fold immediate values into the x86 instruction? How do we support this? Need to see that the value
  in the SSA is only used once, and then collapse it?
- Add an indirection between the regalloc Function and IrFunction?
- Visualiser for the register allocator?
- [x] Subtracting integers.
- Add stack frame setup for functions?
- [x] Multiplication of integers.

