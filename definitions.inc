; I/O ports
.define VDPRegister     $bf ; w
.define VDPAddress      $bf ; w
.define VDPData         $be ; w
.define VDPStatus       $bf ; r
.define PSG             $7f ; w
.define FMAddress       $f0 ; w
.define FMData          $f1 ; w
.define FMDetect        $f2 ; r/w
.define MemoryControl   $3e ; w
.define IOControl       $3f ; w
.define IOPort1         $dc ; r/w
.define IOPort2         $dd ; r/w

; Paging
.define SRAMPaging      $fffc ; r/w
.define SRAMPagingOn    $08
.define SRAMPagingOff   $80
.define Slot0Paging     $fffd ; r/w
.define Slot1Paging     $fffe ; r/w
.define Slot2Paging     $ffff ; r/w

; VDP address masks
; OR an address with else to make it have the desired effect
.define VRAMRead        $0000 ; You need not do this one, of course
.define VRAMWrite       $4000
.define PaletteWrite    $c000

; VDP register names
.enum $80
VDPReg_0                 db ; Misc
VDPReg_1                 db ; Misc
VDPRegTileMapAddress     db
VDPReg_3                 db ; Unused
VDPReg_4                 db ; Unused
VDPRegSpriteTableAddress db
VDPRegSpriteTileSet      db
VDPRegBorderColour       db
VDPRegHScroll            db
VDPRegVScroll            db
VDPRegLineInt            db
.ende

; Controller inputs, as presented by the system
.define P1U %00000001
.define P1D %00000010
.define P1L %00000100
.define P1R %00001000
.define P11 %00010000
.define P12 %00100000
.define P2U %01000000
.define P2D %10000000
.define P2L %00000001
.define P2R %00000010
.define P21 %00000100
.define P22 %00001000
.define ResetButton %00010000
