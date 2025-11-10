# Instruction Set
| Mnemonic                             | Description                                | Flags       |
|--------------------------------------|--------------------------------------------|-------------|
| Load &amp; Store Instructions        |
| LDA                                  | load accumulator                           | NZ          |
| LDX                                  | load X index                               | NZ          |
| LDY                                  | load Y index                               | NZ          |
| STA                                  | store accumulator                          | -           |
| STX                                  | store X index                              | -           |
| STY                                  | store Y index                              | -           |
| STZ                                  | store zero                                 | -           |
| Stack Operations                     |
| PHA                                  | push accumulator                           | -           |
| PHX                                  | push X index                               | -           |
| PHY                                  | push Y index                               | -           |
| PHP                                  | push processor flags                       | -           |
| PLA                                  | pull (pop) accumulator                     | NZ          |
| PLX                                  | pull (pop) X index                         | NZ          |
| PLY                                  | pull (pop) Y index                         | NZ          |
| PLP                                  | pull (pop) processor flags                 | All         |
| TSX                                  | transfer stack pointer to X                | NZ          |
| TXS                                  | transfer stack pointer to X                | -           |
| Increment &amp; Decrement Operations |
| INA                                  | increment accumulator                      | NZ          |
| INX                                  | increment X index                          | NZ          |
| INY                                  | increment Y index                          | NZ          |
| DEA                                  | decrement accumulator                      | NZ          |
| DEX                                  | decrement X index                          | NZ          |
| DEY                                  | decrement Y index                          | NZ          |
| INC                                  | increment memory location                  | NZ          |
| DEC                                  | decrement memory location                  | NZ          |
| Shift Operations                     |
| ASL                                  | arithmetic shift left, high bit into carry | NZC         |
| LSR                                  | logical shift right, low bit into carry    | N=0 ZC      |
| ROL                                  | rotate left through carry                  | NZC         |
| ROR                                  | rotate right through carry                 | NZC         |
| Logical Operations                   |
| AND                                  | and accumulator                            | NZ          |
| ORA                                  | or accumulator                             | NZ          |
| EOR                                  | exclusive-or accumulator                   | NZ          |
| BIT                                  | test bits against accumulator (1)          | N=M7 V=M6 Z |
| CMP                                  | compare with accumulator                   | NZC         |
| CPX                                  | compare with X index                       | NZC         |
| CPY                                  | compare with Y index                       | NZC         |
| TRB                                  | test and reset bits                        | x           |
| TSB                                  | test and set bits                          | x           |
| RMB                                  | reset memory bit                           | x           |
| SMB                                  | reset memory bit                           | x           |
| Math Operations                      |
| ADC                                  | add accumulator, with carry                | NZCV        |
| SBC                                  | subtract accumulator, with borrow          | NZCV        |
| Flow Control Instructions            |
| JMP                                  | unconditional jump                         | -           |
| JSR                                  | jump Subroutine                            | -           |
| RTS                                  | return from Subroutine                     | -           |
| RTI                                  | return from Interrupt                      | From Stack  |
| BRA                                  | branch Always                              | -           |
| BEQ                                  | branch on equal (zero set)                 | -           |
| BNE                                  | branch on not equal (zero clear)           | -           |
| BCC                                  | branch on carry clear (2)                  | -           |
| BCS                                  | branch on carry set (2)                    | -           |
| BVC                                  | branch on overflow clear                   | -           |
| BVS                                  | branch on overflow set                     | -           |
| BMI                                  | branch on minus                            | -           |
| BPL                                  | branch on plus                             | -           |
| BBR                                  | branch on bit reset (zero)                 | -           |
| BBS                                  | branch on bit set (one)                    | -           |
| Processor Status Instructions        |
| CLC                                  | clear carry flag                           | C=0         |
| CLD                                  | clear decimal mode                         | D=0         |
| CLI                                  | clear interrupt disable bit                | I=0         |
| CLV                                  | clear overflow flag                        | V=0         |
| SEC                                  | set carry flag                             | C=1         |
| SED                                  | set decimal mode                           | D=1         |
| SEI                                  | set interrupt disable bit                  | I=1         |
| Transfer Instructions                |
| TAX                                  | transfer accumulator to X index            | NZ          |
| TAY                                  | transfer accumulator to Y index            | NZ          |
| TXA                                  | transfer X index to accumulator            | NZ          |
| TYA                                  | transfer Y index to accumulator            | NZ          |
| Misc Instructions                    |
| NOP                                  | no operation                               | -           |
| BRK                                  | force break                                | B=1         |

### Notes:
> The BIT instruction copies bit 6 to the V flag, and bit 7 to the N flag (except in immediate addressing mode where V & N are untouched.) The accumulator and the operand are ANDed and the Z flag is set appropriately.

> The BCC & BCS instructions instructions are sometimes known as BLT (branch less than) and BGE (branch greater or equal), respectively.
