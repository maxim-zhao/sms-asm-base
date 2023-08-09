; WLA-DX banking setup
.memorymap
defaultslot 0
slotsize 16*1024 ; ROM
slot 0 $0000
slot 1 $4000
slot 2 $8000
slotsize 8*1024 ; RAM
slot 3 $c000
.endme

; You must specify your ROM size here. Mostly you only need two, but I put more here to demonstrate using paging.
.rombankmap
bankstotal 4
banksize 16*1024
banks 4
.endro

; We specify how text is encoded here. The numbers correspond to tile indices. 
; This lets us specify text in the source and have it adjusted to fit our font.
; We are using a font with lots of characters, starting with space at index 0.
.asciitable
map ' ' to '~' = 0
.enda

; We set some definitions for the way we're using VRAM.
.define SpriteSet 1 ; Use upper 256 tiles for sprites
; These are the standard locations and it's best not to use anything else unless you really need to.
.define TileMapAddress     $3800
.define SpriteTableAddress $3f00

; Here is where we specify our RAM usage. This also allows the debugging emulator to show names instead of addresses.
.enum $c000 export
Port3EValue                            db       ; We make sure this is at the start. The BIOS sets this value.
Palette                                dsb 32   ; Full palette
SpriteTable                            dsb 64*3 ; In the same order as in VRAM, but with the "gap" removed
Paused                                 db       ; A flag to signal when the game is paused
VBlankDone                             db       ; A flag to signal that a VBlank has just happened
VBlankRoutine                          dw       ; The VBlank routine
HBlankRoutine                          dw       ; The HBlank routine
CurrentlyPressedButtons                db       ; Buttons which are held down right now
JustPressedButtons                     db       ; Buttons which have just been pressed
CurrentFrameworkPhase                  db       ; Counter for the "phases" of the demo
.ende

; We define some macros to help with some common tasks

; FastOtir: this is code to do the equivalent of "otir" more quickly, using lots of "outi" instructions.
.define FastOtirBlockSize 256 ; How large a block of outi instructions we are willing to use
; Arguments:
;   count = number of bytes to output
; Registers:
;   c = port to output to
;   hl = address of block of data to output
.macro FastOtir args count
.ifgr count FastOtirBlockSize
  ; If the count is more than the block size, execute the whole block...
  call outiblock
  ; ...and repeat
  FastOtir count-FastOtirBlockSize
.else
  ; Call to the point far enough from the end of the block to emit the right number of bytes
  call outiblock+(FastOtirBlockSize-count)*2
.endif
.endm
.section "FastOtir outi block" free
outiblock:
.rept FastOtirBlockSize
  outi
.endr
  ret
.ends

; SetVDPAddress: set the VDP address to the given value.
; Changes register a.
; This is only useful for constant values.
.macro SetVDPAddress args addr
  ld a, <addr ; WLA DX syntax: low byte of addr
  out (VDPAddress),a
  ld a, >addr ; WLA DX syntax: high byte of addr
  out (VDPAddress), a
.endm

.bank 0 slot 0

; We include our external libraries here. This is assuming they are using WLA DX .sections.
.include "Definitions.inc"

; This sets the necessar metadata for the game to work on a real system, and also to let you get credit for your work.
.sdsctag 1.00, "SMS framework", SDSCComment, "Maxim"

.section "SDSC tag comment" free
SDSCComment:
.db "Base project for SMS development. "
.db "Don't forget to change this tag!"
.db 0
.ends

.org 0
; standard startup
.section "Startup" force
  di
  im 1
  ld sp, $dff0
  jp Initialise
.ends

.org $10
.section "rst $10" force
; We put this function here so we can do a fast "rst" invocation for it.
; This allows us to fake a "call hl" opcode.
  jp (hl)
; We put in an empty function so we can make these calls do nothing when that is wanted.
DoNothing:
  ret
; We also set up a macro to make it clearer when we invoke it.
.macro CallHL
  rst $10
.endm
.ends

.org $18
.section "rst $18" force
; Similar to rst $10 above, this is a commonly-used function so we optimise it a bit for space and speed by putting it here.
  ld a, e
  out (VDPAddress),a
  ld a, d
  out (VDPAddress), a
  ret
.macro SetVRAMAddressToDE
  rst $18
.endm
.ends

.org $38
.section "Interrupt handler" force
InterruptHandler:
  ; We swap to the "shadow registers" in the VBlank and don't push things to the stack. This is a design decision which means we can't use the shadow registers anywhere else.
  ex af, af'
  exx
    in a, (VDPStatus) ; satisfy interrupt and check interrupt type
    ; check for H-int
    or a
    jp m, _VBlank
_HBlank:
    ld hl, (HBlankRoutine)
    CallHL
    jr _InterruptEnd
_VBlank:
    ld hl, (VBlankRoutine)
    CallHL
_InterruptEnd:
  exx
  ex af, af'
  ei
  reti
.ends

.org $66
.section "Pause handler" force
PauseHandler:
  ; We just toggle a flag.
  ; This is a design decision. It requires the running code to check the flag periodically and either switch to a different state, or follow a different code path, when it changes.
  push af
    ld a, (Paused)
    xor 1
    ld (Paused), a
  pop af
  retn
.ends

.section "Initialisation" free
; This is the very start of the program. We reset everything so we don't depend on any existing state.
Initialise:
  ; Initialise the VDP
  call InitialiseVDPRegisters
  call ClearVRAM
  ; Clear RAM. We leave the first byte alone, as that is the memory control value, and the last 16 bytes, as we don't use them.
  ld hl, Port3EValue + 1 ; First byte to clear
  ld de, Port3EValue + 2 ; Second byte to clear
  ld bc, $dff0 - (Port3EValue + 2) ; Count
  ld (hl), 0; Set first byte to 0
  ldir ; ...and copy that forward
  
  ; Initialise paging
  xor a
  ld hl,Slot0Paging
  ld b,3
-:ld (hl),a
  inc a
  inc hl
  djnz -

  ; Blacken the palette
  call PaletteToCRAM

  ; Start the demo
  ; We set the index to -1 so we wil start at 0
  ld a, -1
  ld (CurrentFrameworkPhase), a  
  jp NextPhase
.ends

.section "Screen control" free
; Design decision: I'm never going to vary the features controlled by this register.
.define VDP_REG_1_VALUE_SCREEEN_OFF %10100000
.define VDP_REG_1_VALUE_SCREEEN_ON  %11100000
                                ;    |||||||`- Zoomed sprites -> 16x16 pixels
                                ;    ||||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
                                ;    |||||`--- Always 0
                                ;    ||||`---- 30 row/240 line mode
                                ;    |||`----- 28 row/224 line mode
                                ;    ||`------ Enable VBlank interrupts
                                ;    |`------- Enable display
                                ;    `-------- Always 1
TurnOffScreen:
  ld a, VDP_REG_1_VALUE_SCREEEN_OFF
  jr +
TurnOnScreen:
  ld a, VDP_REG_1_VALUE_SCREEEN_ON
+:out (VDPStatus), a
  ld a, VDPReg_1
  out (VDPStatus), a
  ret
.ends

.section "Initialise VDP registers" free
; Set all the VDP registers. We start with the screen turned off and VDP interrupts disabled.
; No arguments.
; Alters hl, bc
InitialiseVDPRegisters:
  ld hl,_Data
  ld b,_End-_Data
  ld c,VDPRegister
  otir
  ret
  
; Let's do some sanity checks on the definitions...
.if SpriteSet >> 1 != 0
.fail "SpriteSet must be either 0 or 1"
.endif
.if (TileMapAddress & %100011111111111) != 0
.fail "TileMapAddress must be a multiple of $800 between 0 and $3800 (usually $3800)"
.endif
.if SpriteTableAddress & %1100000011111111 != 0
.fail "SpriteTableAddress must be a multiple of $100 between 0 and $3f00 (usually $3f00)"
.endif
  
_Data:
  .db %00100110, VDPReg_0
  ;    |||||||`- Disable sync
  ;    ||||||`-- Enable extra height modes
  ;    |||||`--- SMS mode instead of SG
  ;    ||||`---- Shift sprites left 8 pixels
  ;    |||`----- Enable line interrupts
  ;    ||`------ Blank leftmost column for scrolling
  ;    |`------- Fix top 2 rows during horizontal scrolling
  ;    `-------- Fix right 8 columns during vertical scrolling
  .db VDP_REG_1_VALUE_SCREEEN_OFF, VDPReg_1 ; See above
  .db (TileMapAddress >> 10)    | %11110001, VDPRegTileMapAddress
  .db (SpriteTableAddress >> 7) | %10000001, VDPRegSpriteTableAddress
  .db (SpriteSet << 2)          | %11111011, VDPRegSpriteTileSet
  .db $0 | $f0, VDPRegBorderColour
  ;    `-------- Border palette colour (sprite palette)
  .db $00, VDPRegHScroll
  ;    ``------- Horizontal scroll
  .db $00, VDPRegVScroll
  ;    ``------- Vertical scroll
  .db $ff, VDPRegLineInt
  ;    ``------- Line interrupt spacing ($ff to disable)
_End:
.ends

.section "Clear VRAM" free
; Writes zero to all of VRAM.
; No arguments.
; Alters bc, af
ClearVRAM:
  SetVDPAddress 0|VRAMWrite
  ; Output 16KB of zeroes
  ld bc,16*1024    ; Counter
-:xor a            ; Value to write
  out (VDPData),a  ; Output it
  dec bc           ; Decrement counter
  ld a,b           ; Loop until it is zero
  or c
  jr nz,-
  ret
.ends

.section "Title screen" free
TitleScreen:
  call LoadFont

  ; Load a palette to RAM
  ld hl, BasicFontPalette
  ld de, Palette
  ld bc, 32 ; Palette size
  ldir
  
  ; Set the interrupt handlers
  ld hl, DoNothing
  ld (HBlankRoutine), hl
  ld hl, BasicVBlankHandler
  ld (VBlankRoutine), hl
    
  ; Draw some text to the screen
  ; Design decision: I'm assuming I always want to write text at certain X, Y positions and I build my data format around that.
  ld hl,TitleScreenText
  call DrawText
  
  ; Turn on the screen - and VBlank interrupts
  call TurnOnScreen
  ei
  
  ; Wait for a bit
  ld a,255
  call WaitForAFrames
  
  ; Get into the demo
  jp NextPhase
.ends

.section "Wait for a frames" free
WaitForAFrames:
; Uses b
; Changes a, f
; Assumes the VBlank handler will set VBlankDone
  ; Get the counter in b
  ld b, a
  ; Clear the flag
  xor a
  ld (VBlankDone), a
  ; Wait for it to be set
-:ld a, (VBlankDone)
  or a
  jr z, -
  ; Loop the given number of times
  djnz -
  ; Done
  ret
.ends

.section "Text drawing code" free
.define STRING_TERMINATOR $ff
  
; Text: encodes a location and some text
.macro Text args x, y, text
.dw ((x + y * 32) * 2 + TileMapAddress) | VRAMWrite
.asc \3
.db STRING_TERMINATOR
.endm

; Args: hl = pointer to data defined using Text macro
DrawText:
  ; Set the VRAM write address
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  SetVRAMAddressToDE
-:ld a, (hl)
  cp STRING_TERMINATOR
  ret z ; Exit when we get to the end
  ; Else write to the VDP
  out (VDPData), a
  ; And then a zero for the high byte
  xor a
  out (VDPData), a
  inc hl
  jr -
.ends

.section "Text" free
TitleScreenText:
  Text 7, 10, "SMS game framework"
.ends

.section "Basic VBlank handler"
BasicVBlankHandler:
  ; Do all the VDP access first
  ; Update the palette
  call PaletteToCRAM
  ; Update the sprite table
  call UpdateSpriteTable
  ; Then do things that happen every frame, but need not happen during the inactive display
  call GetInputs
  ; Finally, we signal the VBlank flag
  ld a, 1
  ld (VBlankDone), a
  ret
.ends

.section "Update sprite table" free
UpdateSpriteTable:
  SetVDPAddress SpriteTableAddress|VRAMWrite
  ld c,VDPData
  ld hl,SpriteTable
  FastOtir 64
  SetVDPAddress (SpriteTableAddress+128)|VRAMWrite
  FastOtir 128
  ret
.ends

.section "Copy palette from RAM to CRAM" free
PaletteToCRAM:
  SetVDPAddress 0|PaletteWrite
  ld hl, Palette
  ld c, VDPData
  FastOtir 32
  ret
.ends

.section "Load font" free
; Load the font at tile index 0
; Should be done while the screen is off
; The font uses colours 0 and 1
; Parameters: none
; Affects: paging, af, bc, hl
LoadFont:
  ld a, :Font
  ld (Slot2Paging), a
  ld hl, Font
  ld bc, FontEnd-Font
  SetVDPAddress 0|VRAMWrite
-:ld a, (hl)
  out (VDPData), a
  xor a ; Fill three zero bytes
  out (VDPData), a
  out (VDPData), a
  out (VDPData), a
  ; Loop over data
  inc hl
  dec bc
  ld a, b
  or c
  jr nz, -
  ret
.ends

.section "Basic font palette" free
BasicFontPalette:
.db $00, $3f ; Black, white
; The next 30 bytes may be anything, we won't use them so it doesn't matter
.ends

.section "Get Inputs" free
GetInputs:
  ; Get player 1 inputs
  in a, (IOPort1)
  ; Convert it so 1 = pressed
  cpl
  ; Mask to valid buttons
  and P1U|P1D|P1L|P1R|P11|P12
  ld b, a            ; b = all buttons pressed
  ld hl, CurrentlyPressedButtons
  xor (hl)           ; xor with what was pressed already
  ld (hl), b
  and b              ; a = all buttons pressed since last time
  ld (JustPressedButtons),a
  ; Check for reset
  in a, (IOPort2)
  cpl
  and ResetButton
  jp nz, 0 ; Simple reset
  ret
.ends

.section "Next phase" free
NextPhase:
  ; Turn off the screen
  call TurnOffScreen
  ; Get the number
  ld a, (CurrentFrameworkPhase)
  inc a
  ; Check for reaching the end
  cp (_FunctionsEnd - _Functions) / 2
  jr nz, +
  xor a ; wrap to zero
+:ld (CurrentFrameworkPhase), a

  ; Look it up in the function table
  ; We need to multiply it by 2 and add it to the table address
  add a, a
  ld e, a
  ld d, 0
  ld hl, _Functions
  add hl, de
  ; Read the value there into hl
  ld a, (hl)
  inc hl
  ld h, (hl)
  ld l, a
  ; Then jump to it
  jp (hl)

_Functions:
.dw TitleScreen
_FunctionsEnd:
  
.ends

; Let's put some data in paged ROM. We let the assembler decide where to put it, but we must page in the correct bank (using the :Label syntax to get the bank number, and the paging registers) before accessing it.
.slot 2
.section "Font data" superfree
Font:
.include "BBC Micro font.inc"
FontEnd:
.ends
