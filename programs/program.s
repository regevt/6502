.org $0400

; ACIA_RX = $8400
; ACIA_TX = $8400
; ACIA_DATA = $8400
; ACIA_STATUS = $8401
; ACIA_COMMAND = $8402
; ACIA_CONTROL = $8403

; main:
;   ldx #0
; @test_char:
;   lda test_string, X
;   beq @test_done
;   jsr acia_print_char ; acia_print_char
;   inx
;   jmp @test_char
; @test_done:
;   lda #0
;   jmp $c03a ; sys_exit


; acia_print_char:
;   pha                         ; save A
;   lda ACIA_STATUS             ; Read ACIA status register
;   pla                         ; ELSE, restore ACCUMULATOR from STACK
;   sta ACIA_TX                 ; Write byte to ACIA transmit data register
;   jsr acia_delay              ; Required delay - Comment out for working 6551/65C51!     
;   rts                         ; Done COUT subroutine, RETURN

; acia_delay:
;   phy                         ; Save Y Reg
;   phx                         ; Save X Reg
;   ldy   #6                    ; Get delay value (clock rate in MHz 2 clock cycles)
; @minidly:
;   ldx   #$68                  ; Seed X reg
; @delay_1:
;   dex                         ; Decrement low index
;   bne @delay_1                ; Loop back until done
;   dey                         ; Decrease by one
;   bne @minidly                ; Loop until done
;   plx                         ; Restore X Reg
;   ply                         ; Restore Y Reg
; @delay_done:
;   rts                         ; Delay done, return

; test_string: .asciiz "Test program"

; main:
;     LDA #$31
;     STA $8800       ; Freq lower byte
;     LDA #$0C
;     STA $8801       ; Freq higher byte
;     LDA #$00
;     STA $8805       ; Attack/Decay
;     LDA #$F0
;     STA $8806       ; Sustain/Release
;     LDA #$11
;     STA $8804       ; Triangle + Gate ON
;     LDA #$0F
;     STA $8818       ; Volume = 15
;     jmp $c03a       ; sys_exit

VIA_BASE = $8000    ; Base address of the 6522 VIA
; VIA Register Offsets:
IFR      = VIA_BASE + $D   ; Interrupt Flag Register
IER      = VIA_BASE + $E   ; Interrupt Enable Register
ACR      = VIA_BASE + $B   ; Auxiliary Control Register
T1C_L    = VIA_BASE + $4   ; Timer 1 Counter Low
T1C_H    = VIA_BASE + $5   ; Timer 1 Counter High
T1L_L    = VIA_BASE + $4   ; Timer 1 Latch Low (same as T1C_L)
T1L_H    = VIA_BASE + $5   ; Timer 1 Latch High (same as T1C_H)

Setup60HzTimer:		    ; 1Mhz system clock needs VIA T1 at ~8331
    SEI                 ; Disable interrupt
    LDA #$C0            ; Enable VIA T1 interrupt 
    STA IER             ; 
    LDA #$E0            ; Set ACR for T1 
    STA ACR             ; 
    LDA #$F6            ; Low byte for 8331
    STA T1L_L           ; 
    LDA #$3B            ; High byte for 8331
    STA T1L_H           ; Timer start
    CLI                 ; Turn on interrupt
    
loopforever:
    JMP loopforever     ; Loop forever