; CONSTANTS
;   APU
        APUFLAGS    = $4015 ;       Channel enable flags (bitfield [---D NT21] D -> DMC, N -> noise, T -> tri, 2/1 -> pulse)
        SQ1_ENV     = $4000 ;       Pulse 1 Duty, Envelope loop/Length counter halt, constant volume, volume/enveloppe
        SQ1_LO      = $4002 ;       Pulse 1 Period low  (8 bits)
        SQ1_HI      = $4003 ;       Pulse 1 Period high (4 bits)
;   PPU
        PPUCTRL     = $2000 ; W     Controller/settings (PPU flags) (bit0-1 = nametable, useful to snap to another bg after PPUSCROLLing one)
                            ;                                       (bit2 = vram increment (for PPUDATA))
                            ;                                       (bit3 = sprite pattern table address (ignored in 8x16 mode))
                            ;                                       (bit4 = background pattern table address)
                            ;                                       (bit5 = 0: 8x8 sprites, 1: 8x16 sprites (double height))
                            ;                                       (bit6 = ACTUALLY KILL AND FRY THE NES HOLY SHIT)
                            ;                                       (bit7 = emit NMI at start of frame)
        PPUMASK     = $2001 ; W     Mask (Controls frame rendering) (bit0 = 0: color, 1: grayscale)
                            ;                                       (bit1 = 1: show background in leftmost 8 pixels, 0: hide)
                            ;                                       (bit2 = 1: show sprites in leftmost 8 pixels, 0: hide)
                            ;                                       (bit3 = 1: show background, 0: hide)
                            ;                                       (bit4 = 1: show sprites, 0: hide) (bit3-4 = 0 -> safe PPU outside vblank)
                            ;                                       (bit5 = emphasize red (green on PAL/Dendy))
                            ;                                       (bit6 = emphasize green (red on PAL/Dendy))
                            ;                                       (bit7 = emphasize blue)
        PPUSTATUS   = $2002 ; R     Status (LSB/Sprite0 hit/Timing) (bit7 = vblank started, cleared after read) (can test bit7 as sign bit (BIT))
        OAMADDR     = $2003 ; W     OAM Address ($00 to use DMA)
        OAMDATA     = $2004 ; RW    OAM Register (W -> OAMADDR+1) (Points to next OAM field each write (or struct each 4 writes))
        PPUSCROLL   = $2005 ; W2    Pixel scroll (W1 -> X, W2 -> Y) (Must be set AFTER finishing VRAM writes)
        PPUADDR     = $2006 ; W2    VRAM Address (W1 ->HI, W2 ->LO) ($0000-$3FFF, higher are mirrored) (PPUSTATUS must have been read)
        PPUDATA     = $2007 ; RW    VRAM Register (R/W -> VRAM + (PPUCTRL & %000000100)? 32 : 1) (Access increments addr right or down)
        OAMDMA      = $4014 ; W     OAM Bulk upload W(XX -> $XX00-$XXFF -> OAM) (Uploads a complete buffered OAM update from page XX)
;       PPU internal memory (Not addressable by CPU, use PPU registers)
        VRAM_PT0    = $0000 ;       Pattern Table 0
        VRAM_PT1    = $1000 ;       Pattern Table 1
        VRAM_NT0    = $2000 ;       Nametable 0
        VRAM_AT0    = $23C0 ;       Nametable 0 Attributes
        VRAM_NT1    = $2400 ;       Nametable 1
        VRAM_AT1    = $27C0 ;       Nametable 1 Attributes
        VRAM_NT2    = $2800 ;       Nametable 2
        VRAM_AT2    = $2BC0 ;       Nametable 2 Attributes
        VRAM_NT3    = $2C00 ;       Nametable 3
        VRAM_AT3    = $2FC0 ;       Nametable 3 Attributes
        VRAM_NTM    = $3000 ;       Mirror of $2000-$2EFF
        VRAM_PAL    = $3F00 ;       Palette RAM indexes
        VRAM_PAM    = $3F20 ;       Mirror of Palette RAM indexes

; game character mapping
.macro GAME_TEXT    text
    .pushcharmap
    .include "charmap.inc"
    .byte text
    .popcharmap
.endmacro

; game character mapping with (non-ascii) nterm added
NULL_TERM = $2F
.macro GAME_NTEXT   text
    .pushcharmap
    .include "charmap.inc"
    .byte text
    .byte NULL_TERM
    .popcharmap
.endmacro

.segment "HEADER"

INES_MAPPER = 0
INES_MIRROR = 1 ; use nametable mirroring
INES_SRAM   = 1 ; PRG ram as save battery

.byte 'N', 'E', 'S', $1A
.byte $02   ; 16k PRG chunk count
.byte $01   ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, 'v', 'i', 't', 'a'


.segment "TILES"
.incbin "assets.chr"    ; sprites


.segment "RODATA"
pal: .incbin "assets.pal"
.res 13 ; padding, so tiles start at multiple of 16
mts:
; METATILE DEFINITIONS, 4 byte stride starting from metatiles
; NAME            TOP       BOTTOM
mt_null:    .byte $00, $00, $00, $00
mt_road:    .byte $4F, $4F, $4F, $4F
mt_lrside:  .byte $35, $36, $35, $36
mt_rrside:  .byte $57, $35, $57, $35
mt_rrvent:  .byte $39, $36, $35, $36
mt_rline:   .byte $57, $36, $57, $36
mt_cone:    .byte $41, $42, $43, $44
mt_tlwalk:  .byte $47, $46, $30, $31
mt_trwalk:  .byte $46, $45, $31, $34
mt_blwalk:  .byte $30, $31, $3A, $3B
mt_brwalk:  .byte $31, $34, $3B, $3C
mt_lwalk:   .byte $30, $31, $30, $31
mt_rwalk:   .byte $31, $34, $31, $34
mt_twalk:   .byte $46, $46, $31, $31
mt_bwalk:   .byte $31, $31, $3B, $3B
mt_light:   .byte $3D, $3D, $3E, $3E
mt_pole:    .byte $3F, $40, $3F, $40
mt_barrier: .byte $58, $58, $3F, $40

; STRIPS (horizontal level components) (name contains the horizontal extent, for width calculations) (PALETTE<<6+((mt_TILE-mts)>>2))
.define strip_road_5()      1<<6+((mt_lrside-mts)>>2), 1<<6+((mt_road-mts)>>2), 1<<6+((mt_rline-mts)>>2), 1<<6+((mt_road-mts)>>2), 1<<6+((mt_rrside-mts)>>2)
.define strip_walk_top_3()  (mt_tlwalk-mts)>>2, (mt_twalk-mts)>>2, (mt_trwalk-mts)>>2
.define strip_walk_mid_3()  (mt_lwalk-mts)>>2, (mt_null-mts)>>2, (mt_rwalk-mts)>>2
.define strip_walk_btm_3()  (mt_blwalk-mts)>>2, (mt_bwalk-mts)>>2, (mt_brwalk-mts)>>2
.define strip_park_3()      1<<6+(mt_rline-mts)>>2, 1<<6+(mt_rline-mts)>>2, 1<<6+(mt_rline-mts)>>2
.define strip_conc_3()      3<<6+(mt_road-mts)>>2, 3<<6+(mt_road-mts)>>2, 3<<6+(mt_road-mts)>>2

; LEVELS (using strips or mt tiles)
levels:
lv_test:
    .byte strip_walk_top_3, strip_road_5, strip_road_5, strip_walk_top_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_btm_3, strip_road_5, strip_road_5, strip_walk_btm_3
.byte strip_park_3, strip_road_5, strip_road_5, strip_park_3
.byte strip_conc_3, strip_road_5, strip_road_5, strip_conc_3
.byte strip_conc_3, strip_road_5, strip_road_5, strip_conc_3
    .byte strip_walk_top_3, strip_road_5, strip_road_5, strip_walk_top_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_mid_3, strip_road_5, strip_road_5, strip_walk_mid_3
    .byte strip_walk_btm_3, strip_road_5, strip_road_5, strip_walk_btm_3
.byte strip_conc_3, strip_road_5, strip_road_5, strip_conc_3
.byte strip_conc_3, strip_road_5, strip_road_5, strip_conc_3


.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA


.segment "VECTORS"
.word nmi
.word reset
.word irq


; CPU RAM variables
.segment "ZEROPAGE"
zp_nmi_lock:    .res 1
zp_periodlo:    .res 1  ; debug sound period (low)
zp_scroll:      .res 1
; used when uploading a background
zp_nt_offset:   .res 1  ; nametable tile offset
zp_nt_attrib:   .res 1  ; memory for current attrib

.segment "BSS"
; bss_level_addr:     .res 2


; IRQ interrupt
.segment "CODE"
irq:
    rti

upload_dma:
    lda #>oam
    sta OAMDMA
    rts

; NMI interrupt
.segment "CODE"
nmi:
    pha
    txa
    pha
    tya
    pha

    ; check nmi mutex availability
    lda zp_nmi_lock
    beq :+
        jmp @nmi_ret
    :
    inc zp_nmi_lock

    lda PPUSTATUS   ; late read (don't break the latch)
    lda #%10010000
    sta PPUCTRL     ; clear blank correctly

    ; write into background
    lda #>(VRAM_NT0+$42)
    sta PPUADDR
    lda #<(VRAM_NT0+$42)
    sta PPUADDR
    ldx #$0B
    :
        stx PPUDATA
        inx
        cpx #$25
        bne :-

    ; upload dma before scrolling
    jsr upload_dma

    ; scroll back into position
    ; lda #0
    ; sta PPUSCROLL   ; y scroll (0)
    ; lda zp_scroll
    ; sta PPUSCROLL
    ; inc zp_scroll
    lda #0
    sta PPUSCROLL
    sta PPUSCROLL

    ; PPU UPDATES DONE, can issue APU updates here
    
    ; ; play a simple pulse
    ; lda #$0F
    ; sta APUFLAGS    ; enable all channels but DPCM
    ; lda #$0F
    ; sta SQ1_ENV     ; full volume, use internal systems
    ; lda zp_periodlo
    ; sta SQ1_LO
    ; lda #$10
    ; sta SQ1_HI
    ; inc zp_periodlo ; decrease pitch
    ; inc zp_periodlo ; decrease pitch
    
@nmi_unlock:
    dec zp_nmi_lock

@nmi_ret:   ; end of nmi, restoring state
    pla
    tay
    pla
    tax
    pla
    rti

; Load level specified by bss_level_addr (low address byte + high address byte)
bss_level_addr = lv_test    ; only possible way to support indexed outside zp
upload_level:  ; CALL IN VBLANK OR WHEN BACKGROUND IS DISABLED
    ; we have a level with a grid of 16x15 bytes, each corresponding to a tile id and attribute palette index
    ; make sure we write lines
    lda PPUCTRL
    and #%11111011
    sta PPUCTRL
    ; start ppu write at start of nametable 0
    lda #>VRAM_NT0
    sta PPUADDR
    lda #<VRAM_NT0
    sta PPUADDR
    ldy #0
    sty zp_nt_offset  ; store that here too before starting
@nametable_line:
    ldy #0
@nametable_top:
        ; get grid byte (+ offset we have in ram)
        tya
        pha
        clc
        adc zp_nt_offset
        tay
        ldx bss_level_addr, Y
        pla
        tay
        txa
        ; dissect it (6-bit type, high 2-bit pal idx) tile format: 1<<6+((mt_lrside-mts)>>2)
        and #$3F        ; tile index mask
        asl A
        asl A           ; turn index into metatile list offset (*4)
        tax
        lda mts, X
        sta PPUDATA     ; write left part of tile
        lda mts+1, X
        sta PPUDATA     ; write right part of tile
        iny
        cpy #$10
        bne @nametable_top

    ldy #0
@nametable_btm:
        ; get grid byte (+ offset we have in ram)
        tya
        pha
        clc
        adc zp_nt_offset
        tay
        ldx bss_level_addr, Y
        pla
        tay
        txa
        ; dissect it (6-bit type, high 2-bit pal idx) tile format: 1<<6+((mt_lrside-mts)>>2)
        and #$3F        ; tile index mask
        asl A
        asl A           ; turn index into metatile list offset (*4)
        tax
        lda mts+2, X
        sta PPUDATA     ; write bottom left part of tile
        lda mts+3, X
        sta PPUDATA     ; write bottom right part of tile
        iny
        cpy #$10
        bne @nametable_btm

    lda #$10
    clc
    adc zp_nt_offset
    sta zp_nt_offset
    cmp #$F0            ; 240 metatiles, nametable is full.
    bne @nametable_line ; ...or complete an entire screen of tiles

    ; now to fill the attribute table
    ldx #0
    ldy #0
    @nametable_attribs: ; each loop completes a 32x32 block of attribs
        stx zp_nt_attrib    ; reset attrib memory
        lda bss_level_addr, Y       ; top left
        and #$C0
        clc
        rol A
        rol A
        rol A
        adc zp_nt_attrib    ; no carry, last rotated bit IS 0
        sta zp_nt_attrib    ; or+store attrib (top left)

        lda bss_level_addr+1, Y     ; top right
        and #$C0
        lsr A
        lsr A
        lsr A
        lsr A
        adc zp_nt_attrib    ; no carry, last shifted bit IS 0
        sta zp_nt_attrib    ; or+store attrib (top right)

        lda bss_level_addr+16, Y    ; bottom left
        and #$C0
        lsr A
        lsr A
        adc zp_nt_attrib    ; no carry, last shifted bit IS 0
        sta zp_nt_attrib    ; or+store attrib (top right)

        lda bss_level_addr+17, Y    ; bottom right
        ; get the hi pal address
        and #$C0            ; get pal bits (at bits 6-7)
        adc zp_nt_attrib    ; AND does not carry
        sta PPUDATA         ; store final attrib (bottom right)

        ; did we finish a 32x strip? (check low bits for multiple of 8)
        iny
        iny                 ; increment (we did 2 tiles)
        sty zp_nt_attrib    ; temp store our index
        tya                 ; y -> a + set zero flag
        and #%11111000      ; are we done with the line?
        cmp zp_nt_attrib    ; cannot compare a to another register...
        bne :+
            tya
            clc
            adc #$10
            tay
        :
        cpy #$F0            ; did we complete the attrib table? (at 4*60 tiles (240))
        bne @nametable_attribs

    rts

upload_pal:
    ; background palette
    lda #>VRAM_PAL
    sta PPUADDR
    lda #<VRAM_PAL
    sta PPUADDR
    ldx #0
    :
        lda pal, X
        sta PPUDATA
        inx
        cpx #$10
        bne :-
    rts

; reset callback
.segment "CODE"
reset:
    sei         ; disable interrupts
    cld         ; decimal mode unsupported
    
    lda #0
    sta PPUCTRL ; disable NMI
    sta PPUMASK ; disable render
    sta APUFLAGS; disable sound
    sta $4010   ; disable dmc IRQ
    lda #$40
    sta $4017   ; disable APU IRQ
    
    ldx #$FF    ; init SP
    txs

    ; wait for first vblank
    bit PPUSTATUS
    :
        bit PPUSTATUS
        bpl :-

    ; clear ram
    lda #0
    ldx #0
    :
        sta $0000, X
        sta $0100, X
        sta $0200, X
        sta $0300, X
        sta $0400, X
        sta $0500, X
        sta $0600, X
        sta $0700, X
        inx
        bne :-

    ; prepare DMA buffer section (sprites moved out)
    lda #$FF
    ldx #0
    :
        sta oam, X
        inx
        inx
        inx
        inx
        bne :-

    ; wait for second vblank
    :
        bit PPUSTATUS
        bpl :-
    
    ; ! ppu is correctly initialized

    ; upload background palette
    jsr upload_pal

    ; upload dma (avoid sprites flashing
    ; before first real vblank callback)
    jsr upload_dma

    ; upload background with our tile loader
    ; lda #<lv_test
    ; sta bss_level_addr
    ; lda #>lv_test
    ; sta bss_level_addr+1
    jsr upload_level

    lda #%10010000
    sta PPUCTRL     ; reenable NMI
    lda #%00011110
    sta PPUMASK     ; reenable display
    jmp main

; main loop
.segment "CODE"
main:
    ; ; play a simple pulse
    ; lda #$0F
    ; sta APUFLAGS    ; enable all channels but DPCM
    ; lda #$0F
    ; sta SQ1_ENV     ; full volume, use internal systems
    ; lda #$C9
    ; sta SQ1_LO
    ; lda #$A0
    ; sta SQ1_HI
@loop:
    jmp @loop