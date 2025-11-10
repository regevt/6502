# Address Map:

| Address   | Use                 |
| --------- | ------------------- |
| C000-FFFF | ROM – 16 KiB        |
| A000-BFFF | Not decoded – 8 KiB |
| 8000-9FFF | I/O – 8 KiB         |
| 0000-7FFF | RAM                 |

---

| Name                       | address |
| -------------------------- | ------- |
| ACIA_COMMAND               | $8402   |
| ACIA_CONTROL               | $8403   |
| ACIA_RX                    | $8400   |
| ACIA_STATUS                | $8401   |
| ACIA_TX                    | $8400   |
| DEBUG_INTERRUPT_COUNT      | $01     |
| DEBUG_LAST_INTERRUPT_INDEX | $00     |
| IRQ_CONTROLLER             | $8c00   |
| SPEAKER                    | $8800   |
| USER_PROGRAM_START         | $0400   |
| USER_PROGRAM_WRITE_PTR     | $00     |
| VIA_ACR                    | $800b   |
| VIA_DDRA                   | $8003   |
| VIA_DDRB                   | $8002   |
| VIA_IER                    | $800e   |
| VIA_IFR                    | $800d   |
| VIA_PCR                    | $800c   |
| VIA_PORTA                  | $8001   |
| VIA_PORTA_2                | $800f   |
| VIA_PORTB                  | $8000   |
| VIA_SR                     | $800a   |
| VIA_T1C_H                  | $8005   |
| VIA_T1C_L                  | $8004   |
| VIA_T1L_H                  | $8007   |
| VIA_T1L_L                  | $8006   |
| VIA_T2C_H                  | $8009   |
| VIA_T2C_L                  | $8008   |
