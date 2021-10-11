; Uppercase
.repeat $1A, i
    .charmap $41 + i, $0B + i
.endrep

; Lowercase
.repeat $1A, i
    .charmap $61 + i, $0B + i
.endrep

; Numbers
.repeat $0A, i
    .charmap $30 + i, $01 + i
.endrep

; Space
.charmap ' ', $00