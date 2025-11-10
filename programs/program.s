.org $0400

ACIA_RX = $8400
ACIA_TX = $8400
ACIA_DATA = $8400
ACIA_STATUS = $8401
ACIA_COMMAND = $8402
ACIA_CONTROL = $8403

main:
  ldx #0
@test_char:
  lda test_string, X
  beq @test_done
  jsr acia_print_char ; acia_print_char
  inx
  jmp @test_char
@test_done:
  lda #0
  jmp $c03a ; sys_exit


acia_print_char:
  pha                         ; save A
  lda ACIA_STATUS             ; Read ACIA status register
  pla                         ; ELSE, restore ACCUMULATOR from STACK
  sta ACIA_TX                 ; Write byte to ACIA transmit data register
  jsr acia_delay              ; Required delay - Comment out for working 6551/65C51!     
  rts                         ; Done COUT subroutine, RETURN

acia_delay:
  phy                         ; Save Y Reg
  phx                         ; Save X Reg
  ldy   #6                    ; Get delay value (clock rate in MHz 2 clock cycles)
@minidly:
  ldx   #$68                  ; Seed X reg
@delay_1:
  dex                         ; Decrement low index
  bne @delay_1                ; Loop back until done
  dey                         ; Decrease by one
  bne @minidly                ; Loop until done
  plx                         ; Restore X Reg
  ply                         ; Restore Y Reg
@delay_done:
  rts                         ; Delay done, return

test_string: .asciiz "Test program"