.setcpu "65C02"
        ; Boot ROM for 6502 computer. Provides shell which can access test programs.

.include "hardware/speaker.s"
.include "hardware/acia.s"
.include "hardware/via.s"
.include "hardware/keyboard.s"

        ; .include "hardware/vga.s"

.segment "BSS"
shell_cmd_id: .res 1
shell_cmd_tmp: .res 1
shell_buffer_used: .res 1
shell_buffer: .res 64

.segment "CODE"

XAML  = $24                            ; Last "opened" location Low
XAMH  = $25                            ; Last "opened" location High
STL   = $26                            ; Store address Low
STH   = $27                            ; Store address High
L     = $28                            ; Hex value parsing Low
H     = $29                            ; Hex value parsing High
YSAV  = $2A                            ; Used to see if hex value is given
MODE  = $2B                            ; $00=XAM, $7F=STOR, $AE=BLOCK XAM

IN    = $0200                          ; Input buffer

IS_FROM_IRQ = $7F01

VGA_OE      = %00000001
KB_OE       = %00000010
PB_DISABLE_ALL = KB_OE | VGA_OE         ; both OEs disabled (high)

PORT_INPUT  = %00000000
PORT_OUTPUT = %11111111

RESET:
                ; Configure VIA CA1 interrupt on positive edge
                LDA #%00000001   ; CA1 positive edge
                STA VIA_PCR

                ; enable interrupt
                LDA #%10000010
                ; lda #$c2                     ; IRQ ON, T1 ON, CA1 ON
                STA VIA_IER

                ; Port directions
                LDA #PORT_OUTPUT
                STA VIA_DDRB          ; Port B = outputs (control lines / OEs)
                LDA #PORT_OUTPUT
                STA VIA_DDRA          ; Default Port A = outputs (VGA data)

                ; Ensure both shared-bus devices are disabled initially (avoid contention)
                LDA #0
                STA VIA_PORTB
                STA VIA_PORTA

                ; lda #$C0 				   ; Timer 1 in continuous mode, PB7 output
                ; STA VIA_ACR
                ; lda #$0
                ; STA VIA_T1C_L
                ; lda #$3c
                ; STA VIA_T1C_H

                LDA #$00
                STA kb_flags
                STA kb_wptr
                STA kb_rptr

                ; globally enable interrupts on the CPU
                CLI

                LDA     #$1F            ; 8-N-1, 19200 baud.
                STA     ACIA_CONTROL
                LDA     #$0B            ; No parity, no echo, no interrupts.
                STA     ACIA_COMMAND
                LDA     #$1B            ; Begin with escape.

                JMP     NOTCR

KEYBOARD_INPUT:
                LDX     kb_rptr
                LDA     kb_buffer, X
                STA     IN, Y
                INC     kb_rptr
                JMP     NEXTCHAR_COTINUE
NOTCR:
                CMP     #$08           ; Backspace key?
                BEQ     BACKSPACE      ; Yes.
                CMP     #$1B           ; ESC?
                BEQ     ESCAPE         ; Yes.
                INY                    ; Advance text index.
                BPL     NEXTCHAR       ; Auto ESC if line longer than 127.

ESCAPE:
                LDA     #$5C           ; "\".
                JSR     ECHO           ; Output it.

GETLINE:
                LDA     #$0D           ; Send CR
                JSR     ECHO

                LDY     #$01           ; Initialize text index.
BACKSPACE:      DEY                    ; Back up text index.
                BMI     GETLINE        ; Beyond start of line, reinitialize.
                
NEXTCHAR:
                SEI
                LDA     kb_rptr
                CMP     kb_wptr
                CLI
                BNE     KEYBOARD_INPUT
                LDA     ACIA_STATUS    ; Check status.
                AND     #$08           ; Key ready?
                BEQ     NEXTCHAR       ; Loop until ready.
                LDA     ACIA_DATA      ; Load character. B7 will be '0'.
                STA     IN, Y
NEXTCHAR_COTINUE:
                JSR     ECHO           ; Display character.
                CMP     #$0D           ; CR?
                BNE     NOTCR          ; No.
                LDY     #$FF           ; Reset text index.
                LDA     #$00           ; For XAM mode.
                TAX                    ; X=0.
SETBLOCK:
                ASL
SETSTOR:
                ASL                    ; Leaves $7B if setting STOR mode.
                STA     MODE           ; $00 = XAM, $74 = STOR, $B8 = BLOK XAM.
BLSKIP:
                INY                    ; Advance text index.
NEXTITEM:
                LDA     IN,Y           ; Get character.
                CMP     #$0D           ; CR?
                BEQ     GETLINE        ; Yes, done this line.
                CMP     #$2E           ; "."?
                BCC     BLSKIP         ; Skip delimiter.
                BEQ     SETBLOCK       ; Set BLOCK XAM mode.
                CMP     #$3A           ; ":"?
                BEQ     SETSTOR        ; Yes, set STOR mode.
                CMP     #$52           ; "R"?
                BEQ     RUN            ; Yes, run user program.
                STX     L              ; $00 -> L.
                STX     H              ;    and H.
                STY     YSAV           ; Save Y for comparison

NEXTHEX:
                LDA     IN,Y           ; Get character for hex test.
                EOR     #$30           ; Map digits to $0-9.
                CMP     #$0A           ; Digit?
                BCC     DIG            ; Yes.
                ADC     #$88           ; Map letter "A"-"F" to $FA-FF.
                CMP     #$FA           ; Hex letter?
                BCC     NOTHEX         ; No, character not hex.
DIG:
                ASL
                ASL                    ; Hex digit to MSD of A.
                ASL
                ASL

                LDX     #$04           ; Shift count.
HEXSHIFT:
                ASL                    ; Hex digit left, MSB to carry.
                ROL     L              ; Rotate into LSD.
                ROL     H              ; Rotate into MSD's.
                DEX                    ; Done 4 shifts?
                BNE     HEXSHIFT       ; No, loop.
                INY                    ; Advance text index.
                BNE     NEXTHEX        ; Always taken. Check next character for hex.

NOTHEX:
                CPY     YSAV           ; Check if L, H empty (no hex digits).
                BEQ     ESCAPE         ; Yes, generate ESC sequence.

                BIT     MODE           ; Test MODE byte.
                BVC     NOTSTOR        ; B6=0 is STOR, 1 is XAM and BLOCK XAM.

                LDA     L              ; LSD's of hex data.
                STA     (STL,X)        ; Store current 'store index'.
                INC     STL            ; Increment store index.
                BNE     NEXTITEM       ; Get next item (no carry).
                INC     STH            ; Add carry to 'store index' high order.
TONEXTITEM:     JMP     NEXTITEM       ; Get next command item.

RUN:
                JMP     (XAML)         ; Run at current XAM index.

NOTSTOR:
                BMI     XAMNEXT        ; B7 = 0 for XAM, 1 for BLOCK XAM.

                LDX     #$02           ; Byte count.
SETADR:         LDA     L-1,X          ; Copy hex data to
                STA     STL-1,X        ;  'store index'.
                STA     XAML-1,X       ; And to 'XAM index'.
                DEX                    ; Next of 2 bytes.
                BNE     SETADR         ; Loop unless X = 0.

NXTPRNT:
                BNE     PRDATA         ; NE means no address to print.
                LDA     #$0D           ; CR.
                JSR     ECHO           ; Output it.
                LDA     XAMH           ; 'Examine index' high-order byte.
                JSR     PRBYTE         ; Output it in hex format.
                LDA     XAML           ; Low-order 'examine index' byte.
                JSR     PRBYTE         ; Output it in hex format.
                LDA     #$3A           ; ":".
                JSR     ECHO           ; Output it.

PRDATA:
                LDA     #$20           ; Blank.
                JSR     ECHO           ; Output it.
                LDA     (XAML,X)       ; Get data byte at 'examine index'.
                JSR     PRBYTE         ; Output it in hex format.
XAMNEXT:        STX     MODE           ; 0 -> MODE (XAM mode).
                LDA     XAML
                CMP     L              ; Compare 'examine index' to hex data.
                LDA     XAMH
                SBC     H
                BCS     TONEXTITEM     ; Not less, so no more data to output.

                INC     XAML
                BNE     MOD8CHK        ; Increment 'examine index'.
                INC     XAMH

MOD8CHK:
                LDA     XAML           ; Check low-order 'examine index' byte
                AND     #$07           ; For MOD 8 = 0
                BPL     NXTPRNT        ; Always taken.

PRBYTE:
                PHA                    ; Save A for LSD.
                LSR
                LSR
                LSR                    ; MSD to LSD position.
                LSR
                JSR     PRHEX          ; Output hex digit.
                PLA                    ; Restore A.

PRHEX:
                AND     #$0F           ; Mask LSD for hex print.
                ORA     #$30           ; Add "0".
                CMP     #$3A           ; Digit?
                BCC     ECHO           ; Yes, output it.
                ADC     #$06           ; Add offset for letter.

ECHO:
                PHA                    ; Save A.
                STA     ACIA_DATA      ; Output character.
                LDA     #$FF           ; Initialize delay loop.
TXDELAY:        DEC                    ; Decrement A.
                BNE     TXDELAY        ; Until A gets to 0.
                PLA                    ; Restore A.
                JMP PRINT_TO_SCREEN
                RTS                    ; Return.

PRINT_TO_SCREEN:
    PHA
    STA VIA_PORTA
    
    LDA #PORT_OUTPUT
    STA VIA_DDRA

    LDA #VGA_OE
    STA VIA_PORTB

    LDA #0
    STA VIA_PORTB

    PLA
    RTS

irq:
        pha
        txa
        pha
        tya
        pha

;         LDA VIA_IFR
;         and #$40                     ; Timer 1 interrupt flag (bit 6)
;         BEQ check_ca1
;         lda #%01000000               ; disable T1 interrupt in VIA_IER
;         STA VIA_IER                  ; clear CA1 interrupt enable

;         lda #$0
;         STA VIA_T1C_L
;         lda #$3c
;         STA VIA_T1C_H

; check_ca1:
;         LDA VIA_IFR
;         and #$02                     ; CA1 interrupt flag (bit 1)
;         BNE irq_done

    ; ----- disable CA1 interrupt in VIA_IER -----
    ; write IER with bit7 = 0 to clear the bit(s).
    ; CA1 bit is bit1 => %00000010
    LDA #%00000010
    STA VIA_IER       ; clear CA1 interrupt enable
  
    ; LDA #PORT_OUTPUT
    ; STA VIA_DDRA

    ; LDA #0
    ; STA VIA_PORTA     ; ensure PORTA is cleared before enabling KB_OE

    LDA #PORT_INPUT
    STA VIA_DDRA

    LDA #KB_OE
    STA VIA_PORTB

    JSR keyboard_interrupt

    LDA #0
    STA VIA_PORTB

; irq_done:
    ; ----- acknowledge/read IFR so VIA clears internal flags -----
    LDA VIA_IFR

	LDA #1
	STA IS_FROM_IRQ

    ; ; ----- re-enable CA1 interrupt -----
    ; LDA #$C2                     ; IRQ ON, T1 ON, CA1 ON
    LDA #%10000010
    STA VIA_IER       ; set CA1 interrupt enable (bit7=1 => set)

        ; restore registers
        pla
        tay
        pla
        tax
        pla
        RTI

nmi:
        RTI

; .include "hardware/keyboard.s"
    ; .include "../programs/SID_Test.s"
    ; .include "../programs/MontyOnTheRun.s"

.segment "VECTORS"
    .word   nmi             ; NMI vector
    .word   RESET           ; RESET vector
    .word   irq             ; IRQ vector
