; SID6581 Test Program
; Base address: $8800
;  .org $0400

        SID_BASE = $8800
        SID_FREQ_L = SID_BASE + $00
        SID_FREQ_H = SID_BASE + $01
        SID_PW_L = SID_BASE + $02
        SID_PW_H = SID_BASE + $03
        SID_CTRL = SID_BASE + $04
        SID_AD = SID_BASE + $05
        SID_SR = SID_BASE + $06
        SID_VOL = SID_BASE + $18

sid_test:
        ldx #$0           ; Start with waveform 0
waveform_loop:
        lda waveforms, x
        STA SID_CTRL       ; Set waveform

        lda #$10           ; Gate ON
        ORA SID_CTRL
        STA SID_CTRL

        lda #$44           ; Freq low
        STA SID_FREQ_L
        lda #$22           ; Freq high
        STA SID_FREQ_H

        lda #$0           ; Pulse width low
        STA SID_PW_L
        lda #$8           ; Pulse width high
        STA SID_PW_H

        lda #$f0           ; Attack/Decay
        STA SID_AD
        lda #$f0           ; Sustain/Release
        STA SID_SR

        JSR sweep_volume

        LDA SID_CTRL
        and #$ef           ; Gate OFF
        STA SID_CTRL

        JSR delay

        inx
        cpx #$4
        BCC waveform_loop

        jmp $c03a        ; sys_exit

waveforms:
        .byte $10   ; Triangle
        .byte $20   ; Sawtooth
        .byte $40   ; Pulse
        .byte $80   ; Noise

sweep_volume:
        ldy #$0
vol_up:
        tya
        STA SID_VOL
        JSR delay_short
        iny
        cpy #$10
        BCC vol_up

        ldy #$f
vol_down:
        tya
        STA SID_VOL
        JSR delay_short
        dey
        BPL vol_down
        rts

delay:
        ldy #$ff
d1: LDX #$ff
d2: DEX
        BNE d2
        dey
        BNE d1
        rts

delay_short:
        ldy #$20
ds1: DEY
        BNE ds1
        rts
