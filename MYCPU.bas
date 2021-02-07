'Fictional CPU writen in BASIC because why not...
'Don't ask, i was bored...

SCREEN 2

'Some Features
' 64Kb RAM
' integrated io port
' 256 Entries Program Stack
' 256 Entries General Purpose Stack

DIM ram(0 TO 65535) AS _UNSIGNED INTEGER
DIM io AS _UNSIGNED INTEGER
DIM PStack(0 TO 255) AS _UNSIGNED _BYTE
DIM Stack(0 TO 255) AS _UNSIGNED _BYTE

'Some Registers 4x8bits, 4x16bits
DIM A AS _UNSIGNED _BYTE
DIM X AS _UNSIGNED _BYTE
DIM Y AS _UNSIGNED _BYTE
DIM Z AS _UNSIGNED _BYTE
DIM IA AS _UNSIGNED INTEGER
DIM IX AS _UNSIGNED INTEGER
DIM IY AS _UNSIGNED INTEGER
DIM IZ AS _UNSIGNED INTEGER
'Zeroing the RAM First

irq_vector = 65280
brk_vector = 65024
rst_vector = 0

GOSUB cold_restart


'For Debuging Purpose...
' sstep = 0 RUN FULL; sstep = 1 single step; sstep = 2 run slower
sstep = 0

'Prof of concept Hello World program for our CPU to execute
ram(0) = 4 ' lda $00
ram(1) = 0 ' -> high byte
ram(2) = 5 ' ldx $80
ram(3) = 128 ' -> low byte
ram(4) = 8 ' taxia -> a and x to IA as string pointer
ram(5) = 7 ' ldz $00
ram(6) = 0 ' -> z will be our counter
ram(7) = 2 ' out $02
ram(8) = 2 ' -> IO char out
ram(9) = 79 ' inia -> increment string pointer
ram(10) = 27 'inz -> increment ou counter
ram(11) = 44 ' cpz $0C
ram(12) = 13 ' -> compare z to 13
ram(13) = 72 ' bne $0007
ram(14) = 0 '  -> not equal we loop to $0007
ram(15) = 7 '  -> otherwise we are done
ram(16) = 68 ' jmp $0100
ram(17) = 1 '  -> jump somewhere after the string
ram(18) = 0 '  -> so we can let the CPU cycle over all the nop instruction in RAM before repeating our program
ram(128) = ASC("h") ' This is our string
ram(129) = ASC("e") ' This is our string
ram(130) = ASC("l") ' This is our string
ram(131) = ASC("l") ' This is our string
ram(132) = ASC("o") ' This is our string
ram(133) = ASC(" ") ' This is our string
ram(134) = ASC("w") ' This is our string
ram(135) = ASC("o") ' This is our string
ram(136) = ASC("r") ' This is our string
ram(137) = ASC("l") ' This is our string
ram(138) = ASC("d") ' This is our string
ram(139) = ASC("!") ' This is our string
ram(140) = ASC(CHR$(13)) ' This is our string

'This is our CPU main loop

WHILE PC <= 65535
    opcode = ram(PC)
    ' NOP, IN ia, OUT ia, HALT
    IF opcode = 0 THEN GOSUB nop
    IF opcode = 1 THEN GOSUB in
    IF opcode = 2 THEN GOSUB ou
    IF opcode = 3 THEN GOSUB halt
    ' LOAD immediate 8 bits
    IF opcode = 4 THEN GOSUB lda_i
    IF opcode = 5 THEN GOSUB ldx_i
    IF opcode = 6 THEN GOSUB ldy_i
    IF opcode = 7 THEN GOSUB ldz_i
    ' 8bits pair <=> 16bits
    IF opcode = 8 THEN GOSUB taxia
    IF opcode = 9 THEN GOSUB tyzix
    IF opcode = 10 THEN GOSUB taxiy
    IF opcode = 11 THEN GOSUB tyziz
    IF opcode = 12 THEN GOSUB iaxat
    IF opcode = 13 THEN GOSUB xizyt
    IF opcode = 14 THEN GOSUB yixat
    IF opcode = 15 THEN GOSUB zizyt
    ' Store 8 bits, addr = IA
    IF opcode = 16 THEN GOSUB sta
    IF opcode = 17 THEN GOSUB stx
    IF opcode = 18 THEN GOSUB sty
    IF opcode = 19 THEN GOSUB stz
    ' Store 16 bits, addr = IA
    IF opcode = 20 THEN GOSUB stix
    IF opcode = 21 THEN GOSUB stiy
    IF opcode = 22 THEN GOSUB stiz
    ' Store 16 bits IA, addr = IZ
    IF opcode = 23 THEN GOSUB stia
    ' increment 8bits
    IF opcode = 24 THEN GOSUB ina
    IF opcode = 25 THEN GOSUB inx
    IF opcode = 26 THEN GOSUB iny
    IF opcode = 27 THEN GOSUB inz
    ' decrement 8bits
    IF opcode = 28 THEN GOSUB dca
    IF opcode = 29 THEN GOSUB dcx
    IF opcode = 30 THEN GOSUB dcy
    IF opcode = 31 THEN GOSUB dcz
    ' add 8 bits add sub other reg with A
    IF opcode = 32 THEN GOSUB adx
    IF opcode = 33 THEN GOSUB sbx
    IF opcode = 34 THEN GOSUB ady
    IF opcode = 35 THEN GOSUB sby
    IF opcode = 36 THEN GOSUB adz
    IF opcode = 37 THEN GOSUB sbz
    '    ' mul and div
    IF opcode = 38 THEN GOSUB axmia
    IF opcode = 40 THEN GOSUB iada
    '    ' compare imediate
    IF opcode = 41 THEN GOSUB cpa
    IF opcode = 42 THEN GOSUB cpx
    IF opcode = 43 THEN GOSUB cpy
    IF opcode = 44 THEN GOSUB cpz
    ' compare absolute via ia
    IF opcode = 45 THEN GOSUB cpaia
    IF opcode = 46 THEN GOSUB cpxia
    IF opcode = 47 THEN GOSUB cpyia
    IF opcode = 48 THEN GOSUB cpzia
    '   ' compare immediate indexed via ia,a
    IF opcode = 49 THEN GOSUB cpiaa
    '    ' or immediate
    '    IF opcode = 50 THEN GOSUB ora
    '    IF opcode = 51 THEN GOSUB oria
    '    ' and immediate
    '    IF opcode = 52 THEN GOSUB ana
    '    IF opcode = 53 THEN GOSUB ania
    '    ' or reg pair
    '    IF opcode = 54 THEN GOSUB orax
    '    IF opcode = 55 THEN GOSUB oryz
    '    IF opcode = 56 THEN GOSUB oriaix
    '    ' and reg pair
    '    IF opcode = 57 THEN GOSUB anax
    '    IF opcode = 58 THEN GOSUB anyz
    '    IF opcode = 59 THEN GOSUB aniaix
    ' Stack push an pull
    IF opcode = 60 THEN GOSUB pha
    IF opcode = 61 THEN GOSUB phx
    IF opcode = 62 THEN GOSUB phy
    IF opcode = 63 THEN GOSUB phz
    IF opcode = 64 THEN GOSUB pla
    IF opcode = 65 THEN GOSUB plx
    IF opcode = 66 THEN GOSUB ply
    IF opcode = 67 THEN GOSUB plz
    'branching
    IF opcode = 68 THEN GOSUB jmp
    IF opcode = 69 THEN GOSUB jsr
    IF opcode = 70 THEN GOSUB rts
    IF opcode = 71 THEN GOSUB beq
    IF opcode = 72 THEN GOSUB bne
    IF opcode = 73 THEN GOSUB brz
    IF opcode = 74 THEN GOSUB bnz
    IF opcode = 75 THEN GOSUB brn
    IF opcode = 76 THEN GOSUB bnn
    IF opcode = 77 THEN GOSUB brk
    IF opcode = 78 THEN GOSUB irq
    ' Increment IA
    IF opcode = 79 THEN GOSUB inia
    ' Clear and Set Flags
    IF opcode = 80 THEN GOSUB cle
    IF opcode = 81 THEN GOSUB see
    IF opcode = 82 THEN GOSUB clz
    IF ocpode = 83 THEN GOSUB sez
    IF opcode = 84 THEN GOSUB cln
    IF opcode = 85 THEN GOSUB sen
    IF opcode = 86 THEN GOSUB clb
    IF opcode = 87 THEN GOSUB seb
    IF opcode = 88 THEN GOSUB cli
    IF opcode = 89 THEN GOSUB sei


    IF PC >= 65535 THEN PC = 0
    IF sstep = 1 THEN
        LOCATE 1, 1
        PRINT "PC= "; HEX$(PC); " A= "; HEX$(A); " X= "; HEX$(X); " Y= "; HEX$(Y); " Z= "; HEX$(Z)
        PRINT "IA= "; HEX$(IA); " IX= "; HEX$(IX); " IY= "; HEX$(IY); " IZ= "; HEX$(IZ)
        PRINT "PSP="; HEX$(PSP); " SP="; HEX$(SP); "PStack="; HEX$(PStack(PSP)); " FLAGS:"; CARRY; ZERO; EQUAL; INTERUPT; BRK; NEG
        K$ = "": WHILE K$ = "": K$ = INKEY$: WEND
    END IF
    IF sstep = 2 THEN
        '    LOCATE 1, 1
        '    PRINT "PC= "; HEX$(PC); " A= "; HEX$(A); " X= "; HEX$(X); " Y= "; HEX$(Y); " Z= "; HEX$(Z)
        '    PRINT "IA= "; HEX$(IA); " IX= "; HEX$(IX); " IY= "; HEX$(IY); " IZ= "; HEX$(IZ)
        '    PRINT "PSP="; HEX$(PSP); " SP="; HEX$(SP); "PStack="; HEX$(PStack(PSP)); " FLAGS:"; CARRY; ZERO; EQUAL; INTERUPT; BRK; NEG
        FOR i = 0 TO 5000: NEXT i
    END IF
WEND
END

'This is our CPU instruction Set

nop:
PC = PC + 1
RETURN

in:
io = ram(PC + 1)
IF io = 1 THEN
    K$ = "": WHILE K$ = "" AND K$ <> CHR$(13): K$ = INKEY$: WEND
    ram(IA) = ASC(K$)
    PRINT K$;
END IF
IF io = 2 THEN
    ram(IA) = 0 'Unimplemented
END IF
PC = PC + 2
io = 0
RETURN

ou:
io = ram(PC + 1)
IF io = 1 THEN
END IF
IF io = 2 THEN
    S = ram(IA): PRINT CHR$(S);
END IF
PC = PC + 2
io = 0
RETURN

halt:
END

lda_i:
A = ram(PC + 1)
PC = PC + 2
RETURN

ldx_i:
X = ram(PC + 1)
PC = PC + 2
RETURN

ldy_i:
Y = ram(PC + 1)
PC = PC + 2
RETURN

ldz_i:
Z = ram(PC + 1)
PC = PC + 2
RETURN

taxia:
IA = A * 256 + X
PC = PC + 1
RETURN

tyzix:
IX = Y * 256 + Z
PC = PC + 1
RETURN

taxiy:
IY = A * 256 + X
PC = PC + 1
RETURN

tyziz:
IZ = Y * 256 + Z
PC = PC + 1
RETURN

iaxat:
X = IA MOD 256
A = IA / 256
PC = PC + 1
RETURN

xizyt:
Z = IX MOD 256
Y = IX / 256
PC = PC + 1
RETURN

yixat:
X = IY MOD 256
A = IY / 256
PC = PC + 1
RETURN

zizyt:
Y = IZ MOD 256
Z = IZ / 256
PC = PC + 1
RETURN

sta:
ram(IA) = A
PC = PC + 1
RETURN

stx:
ram(IA) = X
PC = PC + 1
RETURN

sty:
ram(IA) = Y
PC = PC + 1
RETURN

stz:
ram(IA) = Z
PC = PC + 1
RETURN

stia:
ram(IZ) = IA / 256
ram(IZ + 1) = IA MOD 256
PC = PC + 1
RETURN

stix:
ram(IA) = IX / 256
ram(IA + 1) = IX MOD 256
PC = PC + 1
RETURN

stiy:
ram(IA) = IY / 256
ram(IA + 1) = IY MOD 256
PC = PC + 1
RETURN

stiz:
ram(IA) = IZ / 256
ram(IA + 1) = IZ MOD 256
PC = PC + 1
RETURN

ina:
A = A + 1
IF A >= 255 THEN A = A - 255: CARRY = 1
PC = PC + 1
RETURN

inx:
X = X + 1
IF X >= 255 THEN X = X - 255: CARRY = 1
PC = PC + 1
RETURN

iny:
Y = Y + 1
IF Y >= 255 THEN Y = Y - 255: CARRY = 1
PC = PC + 1
RETURN

inz:
Z = Z + 1
IF Z >= 255 THEN Z = Z - 255: CARRY = 1
PC = PC + 1
RETURN

dca:
A = A - 1
IF A <= 0 THEN A = A + 255: NEG = 1
PC = PC + 1
RETURN

dcx:
X = X - 1
IF X <= 0 THEN X = X + 255: NEG = 1
PC = PC + 1
RETURN

dcy:
Y = Y - 1
IF Y <= 0 THEN Y = Y + 255: NEG = 1
PC = PC + 1
RETURN

dcz:
Z = Z - 1
IF Z <= 0 THEN Z = Z + 255: NEG = 1
PC = PC + 1
RETURN

cpa:
op1 = ram(PC + 1)
IF op1 = A THEN EQUAL = 1
IF op1 <> A THEN EQUAL = 0
PC = PC + 2
RETURN

cpx:
op1 = ram(PC + 1)
IF op1 = X THEN EQUAL = 1
IF op1 <> X THEN EQUAL = 0
PC = PC + 2
RETURN

cpy:
op1 = ram(PC + 1)
IF op1 = Y THEN EQUAL = 1
IF op1 <> Y THEN EQUAL = 0
PC = PC + 2
RETURN

cpz:
op1 = ram(PC + 1)
IF op1 = Z THEN EQUAL = 1
IF op1 <> Z THEN EQUAL = 0
PC = PC + 2
RETURN

inia:
IA = IA + 1
IF IA >= 65535 THEN IA = 0
PC = PC + 1
RETURN

bne:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF EQUAL = 0 THEN
    PC = IZ
END IF
IF EQUAL = 1 THEN PC = PC + 3
RETURN

jmp:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
PC = op1 * 256 + op2
RETURN

adx:
X = X + A
IF X >= 256 THEN X = X - 256: CARRY = 1
PC = PC + 1
RETURN

sbx:
X = X - A
IF X <= -1 THEN X = X + 256: NEG = 1
PC = PC + 1
RETURN

ady:
Y = Y + A
IF Y >= 256 THEN Y = Y - 256: CARRY = 1
PC = PC + 1
RETURN

sby:
Y = Y - A
IF Y <= -1 THEN Y = Y + 256: NEG = 1
PC = PC + 1
RETURN

adz:
Z = Z + A
IF Z >= 256 THEN Z = Z - 256: CARRY = 1
PC = PC + 1
RETURN

sbz:
Z = Z - A
IF Z <= -1 THEN Z = Z + 256: NEG = 1
PC = PC + 1
RETURN

'mul A by x result in IA
axmia:
IA = A * X
PC = PC + 1
RETURN

'div IA by A result in IX
iada:
IX = IA / A
PC = PC + 1
RETURN

cpaia:
IF ram(IA) = A THEN EQUAL = 1
IF ram(IA) <> A THEN EQUAL = 0
PC = PC + 1
RETURN

cpxia:
IF ram(IA) = X THEN EQUAL = 1
IF ram(IA) <> X THEN EQUAL = 0
PC = PC + 1
RETURN

cpyia:
IF ram(IA) = Y THEN EQUAL = 1
IF ram(IA) <> Y THEN EQUAL = 0
PC = PC + 1
RETURN

cpzia:
IF ram(IA) = Z THEN EQUAL = 1
IF ram(IA) <> Z THEN EQUAL = 0
PC = PC + 1
RETURN

cpiaa:
op1 = ram(PC + 1)
IF ram(IA + A) = op1 THEN EQUAL = 1
IF ram(IA + A) <> op1 THEN EQUAL = 0
PC = PC + 2
RETURN

pha:
Stack(SP) = A
SP = SP + 1
PC = PC + 1
RETURN

phx:
Stack(SP) = X
SP = SP + 1
PC = PC + 1
RETURN

phy:
Stack(SP) = Y
SP = SP + 1
PC = PC + 1
RETURN

phz:
Stack(SP) = Z
SP = SP + 1
PC = PC + 1
RETURN

pla:
A = Stack(SP)
SP = SP - 1
PC = PC + 1
RETURN

plx:
X = Stack(SP)
SP = SP - 1
PC = PC + 1
RETURN

ply:
Y = Stack(SP)
SP = SP - 1
PC = PC + 1
RETURN

plz:
Z = Stack(SP)
SP = SP - 1
PC = PC + 1
RETURN

jsr:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
addr = op1 * 256 + op2
PC = PC + 3
PStack(PSP) = PC
PSP = PSP + 1
PC = addr
RETURN

rts:
PC = PStack(PSP)
PStack(PSP) = 0
PSP = PSP - 1
RETURN

beq:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF EQUAL = 1 THEN
    PC = IZ
END IF
IF EQUAL = 0 THEN PC = PC + 3
RETURN

brz:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF ZERO = 0 THEN
    PC = IZ
END IF
IF ZERO = 1 THEN PC = PC + 3
RETURN

bnz:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF ZERO = 1 THEN
    PC = IZ
END IF
IF ZERO = 0 THEN PC = PC + 3
RETURN

brn:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF NEG = 0 THEN
    PC = IZ
END IF
IF NEG = 1 THEN PC = PC + 3
RETURN

bnn:
op1 = ram(PC + 1)
op2 = ram(PC + 2)
IZ = op1 * 256 + op2
IF NEG = 1 THEN
    PC = IZ
END IF
IF NEG = 0 THEN PC = PC + 3
RETURN

brk:
PC = brk_vector
RETURN

irq:
PC = irq_vector
RETURN

'This Function initialize the RAM by Zeroing it.
clr_ram:
FOR C = 0 TO 65535
    ram(C) = 0
NEXT C
RETURN

clr_Pstack:
FOR C = 0 TO 255
    PStack(C) = 0
NEXT C
RETURN

clr_stack:
FOR C = 0 TO 255
    Stack(C) = 0
NEXT C
RETURN

cle:
EQUAL = 0
PC = PC + 1
RETURN

see:
EQUAL = 1
PC = PC + 1
RETURN

cli:
INTERUPT = 0
PC = PC + 1
RETURN

sei:
INTERUPT = 1
PC = PC + 1
RETURN

clz:
ZERO = 0
PC = PC + 1
RETURN

sez:
ZERO = 1
PC = PC + 1
RETURN

cln:
NEG = 0
PC = PC + 1
RETURN

sen:
NEG = 1
PC = PC + 1
RETURN

clb:
BRK = 0
PC = PC + 1

seb:
BRK = 1
PC = PC + 1
RETURN

'This clear the registers, and flags
clr_regf:
'Registers
' Accumulators  8bits, 16bits
A = 0
IA = 0
' 8 bits
X = 0
Y = 0
Z = 0
' 16 bits
IX = 0
IY = 0
IZ = 0
'Specials (Program counter, Program Stack Pointer, Stack Pointer)
PC = rst_vector
PSP = 0
SP = 0
'FLAGS
BRK = 0
NEG = 0
ZERO = 0
CARRY = 0
EQUAL = 0
INTERUPT = 0
RETURN

'This is a cold start or hard reset (clear the ram, registers, Program Stack and Stack)
cold_restart:
GOSUB clr_regf
GOSUB clr_Pstack
GOSUB clr_stack
GOSUB clr_ram
RETURN

'This is a warm start or soft reset (only clean registers and flags, Program Stack but not Stack)
warm_restart:
GOSUB clr_regf
GOSUB clr_Pstack
RETURN

