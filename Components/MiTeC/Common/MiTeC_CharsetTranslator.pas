{*******************************************************}
{                MiTeC Common Routines                  }
{               Character Set Translator                }
{                                                       }
{          Copyright (c) 2009-2016 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_CharsetTranslator;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TXLATTable = record
    Name: string;
    Table: array [0..255] of Byte;
    ReverseTable: array [0..255] of Byte;
  end;

  TCharSetTranslator = class
  private
    class function GetXLATByID(const ATableID: string): TXLATTable;
    class function ToUtf8(Source: PWideChar; SourceLength: Integer;
      Dest: PChar; DestLength: Integer): Integer;
    class function FromUtf8(Source: PChar; SourceLength: Integer;
      Dest: PWideChar; DestLength: Integer): Integer;
  public
    class function TranslateToUtf8(const ASource: WideString): string;
    class function TranslateFromUtf8(const ASource: string): WideString;

    class function TranslateTo(const ACharSet, ASource: string): string; overload;
    class function TranslateFrom(const ACharSet, ASource: string): string; overload;

    class procedure TranslateTo(const ACharSet: string; ASource, ADestination: TStream); overload;
    class procedure TranslateFrom(const ACharSet: string; ASource, ADestination: TStream); overload;

    class procedure GetSupportedCharSets(ACharSets: TStrings);
  end;

var
  CharsetTranslator: TCharsetTranslator;

const
  CharSetTables: array [0..11] of TXLATTable = (
  (
  Name: 'ISO-8859-1';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff)
  ),
  (
  Name: 'ISO-8859-2';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a5,$a2,$a3,$a4,$a5,$8c,$a7
        ,$a8,$a9,$aa,$ab,$8f,$ad,$ae,$af,$b0,$b9,$b2,$b3,$b4,$b5,$9c,$b7,$b8,$b9,$ba,$bb,$9f,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$a6,$8d,$8e,$ac
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$b6,$9d,$9e,$bc,$a0,$a1,$a2,$a3,$a4,$a1,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b1,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff)
  ),
  (
  Name: 'Windows-1250';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff)
  ),
  (
  Name: 'ibm866';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14//21
        ,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29//42
        ,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e//63
        ,$3f,$40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53//84
        ,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63,$64,$65,$66,$67,$68//105
        ,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d//126
        ,$7e,$7f,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2//147
        ,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7//168
        ,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc//189
        ,$bd,$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1//210
        ,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$f0,$f1,$f2,$f3,$f4,$f5,$f6//231
        ,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff,$a8,$a3,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb//252
        ,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14//21
        ,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29//42
        ,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e//63
        ,$3f,$40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53//84
        ,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63,$64,$65,$66,$67,$68//105
        ,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d//126
        ,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90,$91,$92//147
        ,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$f1,$a4,$a5,$a6,$a7//168
        ,$f0,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$f0,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc//189
        ,$bd,$be,$bf,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90,$91//210
        ,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6//231
        ,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb//252
        ,$ec,$ed,$ee,$ef)
  ),
  (
  Name: 'ISO-8859-5';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7
        ,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
        ,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7
        ,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$90,$91,$92,$93,$94,$95,$96,$97
        ,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
        ,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
        ,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
        ,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef)
  ),
  (
  Name: 'koi8-r';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$b8,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$a8,$b4,$b5,$b6,$b7,$a3,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$fe,$e0,$e1,$f6,$e4,$e5,$f4,$e3,$f5,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$ff,$f0,$f1,$f2,$f3,$e6,$e2
        ,$fc,$fb,$e7,$f8,$fd,$f9,$f7,$fa,$de,$c0,$c1,$d6,$c4,$c5,$d4,$c3,$d5,$c8,$c9,$ca,$cb,$cc,$cd,$ce
        ,$cf,$df,$d0,$d1,$d2,$d3,$c6,$c2,$dc,$db,$c7,$d8,$dd,$d9,$d7,$da);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$b8,$a4,$a5,$a6,$a7
        ,$b3,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$a3,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$e1,$e2,$f7,$e7,$e4,$e5,$f6,$fa,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f2,$f3,$f4,$f5,$e6,$e8,$e3,$fe
        ,$fb,$fd,$ff,$f9,$f8,$fc,$e0,$f1,$c1,$c2,$d7,$c7,$c4,$c5,$d6,$da,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0
        ,$d2,$d3,$d4,$d5,$c6,$c8,$c3,$de,$db,$dd,$df,$d9,$d8,$dc,$c0,$d1)
  ),
  (
  Name: 'koi8-u';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$b8,$ba,$a5,$b3,$bf
        ,$a8,$a9,$aa,$ab,$ac,$b4,$ae,$af,$b0,$b1,$b2,$a8,$aa,$b5,$b2,$af,$b8,$b9,$ba,$bb,$bc,$a5,$be,$bf
        ,$fe,$e0,$e1,$f6,$e4,$e5,$f4,$e3,$f5,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$ff,$f0,$f1,$f2,$f3,$e6,$e2
        ,$fc,$fb,$e7,$f8,$fd,$f9,$f7,$fa,$de,$c0,$c1,$d6,$c4,$c5,$d4,$c3,$d5,$c8,$c9,$ca,$cb,$cc,$cd,$ce
        ,$cf,$df,$d0,$d1,$d2,$d3,$c6,$c2,$dc,$db,$c7,$d8,$dd,$d9,$d7,$da);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$bd,$a6,$a7
        ,$b3,$a9,$b4,$ab,$ac,$ad,$ae,$b7,$b0,$b1,$b6,$a6,$ad,$b5,$b6,$b7,$a3,$b9,$a4,$bb,$bc,$bd,$be,$a7
        ,$e1,$e2,$f7,$e7,$e4,$e5,$f6,$fa,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f2,$f3,$f4,$f5,$e6,$e8,$e3,$fe
        ,$fb,$fd,$ff,$f9,$f8,$fc,$e0,$f1,$c1,$c2,$d7,$c7,$c4,$c5,$d6,$da,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0
        ,$d2,$d3,$d4,$d5,$c6,$c8,$c3,$de,$db,$dd,$df,$d9,$d8,$dc,$c0,$d1)
  ),
  (
  Name: 'KOI8-WIN';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$da,$9b,$9c,$9d,$9e,$9f,$a0,$e7,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$d7,$de,$c0,$c6,$c4,$c5,$d2,$d6,$d3,$d5,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$d1,$cf,$df,$d0,$d1,$d4,$c1
        ,$dd,$d8,$c3,$dc,$d9,$db,$c2,$c7,$f7,$fe,$e0,$e6,$e4,$e5,$f2,$f6,$f3,$f5,$e8,$e9,$ea,$eb,$ec,$ed
        ,$ee,$fa,$ef,$ff,$f0,$f1,$f4,$e1,$fd,$f8,$e3,$fc,$f9,$fb,$e2,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c2,$d7,$de,$da,$c4,$c5,$c3,$df,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d2,$d4,$d5,$c6,$c8,$d6,$c9,$c7,$c0
        ,$d9,$dc,$9a,$dd,$db,$d8,$c1,$d3,$e2,$f7,$fe,$fa,$e4,$e5,$e3,$a1,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f2
        ,$f4,$f5,$e6,$e8,$f6,$e9,$e7,$e0,$f9,$fc,$f1,$fd,$fb,$f8,$e1,$f3)
  ),
  (
  Name: 'WIN-KOI8';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$ff,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$e1,$e2,$f7,$e7,$e4,$e5,$f6,$fa,$c8,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f2,$f3,$f4,$f5,$e6,$e8,$e3,$fe
        ,$fb,$fd,$da,$f9,$f8,$fc,$e0,$f1,$c1,$c2,$d7,$c7,$c4,$c5,$d6,$ff,$e8,$ca,$cb,$cc,$cd,$ce,$cf,$d0
        ,$d2,$d3,$d4,$d5,$c6,$c8,$c3,$de,$db,$dd,$df,$d9,$d8,$dc,$c0,$d1);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17 //23
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f//47
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47//71
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f//95
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77//119
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f//143
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7//167
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf//191
        ,$fe,$e0,$e1,$f6,$e4,$e5,$f4,$e3,$f5,$c9,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$ff,$f0,$f1,$f2,$f3,$e6,$e2//215
        ,$fc,$fb,$da,$f8,$fd,$f9,$f7,$fa,$de,$c0,$c1,$d6,$c4,$c5,$d4,$c3,$d5,$e9,$c9,$ca,$cb,$cc,$cd,$ce//239
        ,$cf,$df,$d0,$d1,$d2,$d3,$c6,$c2,$dc,$db,$c7,$d8,$dd,$d9,$d7,$e7)
  ),
  (
  Name: 'Windows-1251';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff)
  ),
  (
  Name: 'x-mac-cyrillic';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
        ,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$a8,$b8,$ff,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$dd,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$de,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90,$91,$92,$93,$94,$95,$96,$97
        ,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$df)
  ),
  (
  Name: 'Latin1';
  Table:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  ReverseTable:
        ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17
        ,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47
        ,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        ,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77
        ,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        ,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
        ,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        ,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
        ,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        ,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff)
  )
  );
  

implementation

{ TCharSetTranslator }

class procedure TCharSetTranslator.GetSupportedCharSets(ACharSets: TStrings);
var
  i: Integer;
begin
  ACharSets.Clear();
  for i:=Low(CharSetTables) to High(CharSetTables) do
  begin
    ACharSets.Add(CharSetTables[i].Name);
  end;
end;

class function TCharSetTranslator.GetXLATByID(const ATableID: string): TXLATTable;
var
  i: Integer;
begin
  Result.Name:='';
  for i:=Low(CharSetTables) to High(CharSetTables) do
  begin
    if CompareText(CharSetTables[i].Name, ATableID) = 0 then
    begin
      Result:=CharSetTables[i];
      Break;
    end;
  end;
end;

class procedure TCharSetTranslator.TranslateTo(const ACharSet: string;
  ASource, ADestination: TStream);
var
  i: Integer;
  Xlat: TXLATTable;
  Ch: Byte;
  PrevSourcePos, PrevDestPos: Integer;
begin
  Xlat:=GetXLATByID(ACharSet);
  PrevSourcePos:=ASource.Position;
  PrevDestPos:=ADestination.Position;
  ASource.Position:=0;
  ADestination.Position:=0;
  if (Xlat.Name <> '') then
  begin
    for i:=0 to ASource.Size - 1 do
    begin
      ASource.Read(Ch, 1);
      Ch:=Xlat.ReverseTable[Ch];
      ADestination.Write(Ch, 1);
    end;
  end else
  begin
    ADestination.CopyFrom(ASource, ASource.Size);
  end;
  ASource.Position:=PrevSourcePos;
  ADestination.Position:=PrevDestPos;
end;

class procedure TCharSetTranslator.TranslateFrom(const ACharSet: string;
  ASource, ADestination: TStream);
var
  i: Integer;
  Xlat: TXLATTable;
  Ch: Byte;
  PrevSourcePos, PrevDestPos: Integer;
begin
  Xlat:=GetXLATByID(ACharSet);
  PrevSourcePos:=ASource.Position;
  PrevDestPos:=ADestination.Position;
  ASource.Position:=0;
  ADestination.Position:=0;
  if (Xlat.Name <> '') then
  begin
    for i:=0 to ASource.Size - 1 do
    begin
      ASource.Read(Ch, 1);
      Ch:=Xlat.Table[Ch];
      ADestination.Write(Ch, 1);
    end;
  end else
  begin
    ADestination.CopyFrom(ASource, ASource.Size);
  end;
  ASource.Position:=PrevSourcePos;
  ADestination.Position:=PrevDestPos;
end;

class function TCharSetTranslator.TranslateTo(const ACharSet, ASource: string): string;
var
  Src, Dst: TStringStream;
begin
  Src:=TStringStream.Create(ASource);
  Dst:=TStringStream.Create('');
  try
    TranslateTo(ACharSet, Src, Dst);
    Result:=Dst.DataString;
  finally
    Dst.Free();
    Src.Free();
  end;
end;

class function TCharSetTranslator.TranslateFrom(const ACharSet, ASource: string): string;
var
  Src, Dst: TStringStream;
begin
  Src:=TStringStream.Create(ASource);
  Dst:=TStringStream.Create('');
  try
    TranslateFrom(ACharSet, Src, Dst);
    Result:=Dst.DataString;
  finally
    Dst.Free();
    Src.Free();
  end;
end;

class function TCharSetTranslator.ToUtf8(Source: PWideChar; SourceLength: Integer;
  Dest: PChar; DestLength: Integer): Integer;
var
  i, count: Integer;
  c: Cardinal;
begin
  count:=0;
  i:=0;
  while (i < SourceLength) and (count < DestLength) do
  begin
    c:=Cardinal(Source[i]);
    Inc(i);
    if c <= $7F then
    begin
      Dest[count]:=Char(c);
      Inc(count);
    end else
    if c > $7FF then
    begin
      if count + 3 > DestLength then Break;
      Dest[count]:=Char($E0 or (c shr 12));
      Dest[count+1]:=Char($80 or ((c shr 6) and $3F));
      Dest[count+2]:=Char($80 or (c and $3F));
      Inc(count,3);
    end else
    begin
      if count + 2 > DestLength then Break;
      Dest[count]:=Char($C0 or (c shr 6));
      Dest[count + 1]:=Char($80 or (c and $3F));
      Inc(count, 2);
    end;
  end;
  if count >= DestLength then
  begin
    count:=DestLength - 1;
  end;
  Dest[count]:=#0;
  Result:=count + 1;
end;

class function TCharSetTranslator.FromUtf8(Source: PChar;
  SourceLength: Integer; Dest: PWideChar; DestLength: Integer): Integer;
var
  i, count: Integer;
  c: Byte;
  wc: Cardinal;
begin
  Result:=-1;
  count:=0;
  i:=0;
  while (i < SourceLength) and (count < DestLength) do
  begin
    wc:=Cardinal(Source[i]);
    Inc(i);
    if (wc and $80) <> 0 then
    begin
      wc:=wc and $3F;
      if i > SourceLength then Exit;
      if (wc and $20) <> 0 then
      begin
        c:=Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
        begin
          if (c = $20) then
          begin
            Dest[count]:=#32;
          end else
          begin
            Dest[count]:=#128;
          end;
          Inc(count);
          continue;
        end;
        if i > SourceLength then Exit;
        wc:=(wc shl 6) or (c and $3F);
      end;
      c:=Byte(Source[i]);
      Inc(i);
      if (c and $C0) <> $80 then
      begin
        if (c = $20) then
        begin
          Dest[count]:=#32;
        end else
        begin
          Dest[count]:=#128;
        end;
        Inc(count);
        continue;
      end;

      Dest[count]:=WideChar((wc shl 6) or (c and $3F));
    end else
    begin
      Dest[count]:=WideChar(wc);
    end;
    Inc(count);
  end;
  if count >= DestLength then
  begin
    count:=DestLength - 1;
  end;
  Dest[count]:=#0;
  Result:=count + 1;
end;

class function TCharSetTranslator.TranslateFromUtf8(const ASource: string): WideString;
var
  len: Integer;
  ws: WideString;
begin
  Result:='';
  if (ASource = '') then Exit;

  SetLength(ws, Length(ASource));

  len:=FromUtf8(PChar(ASource), Length(ASource), PWideChar(ws), Length(ws) + 1);

  if (len > 0) then
  begin
    SetLength(ws, len - 1);
  end else
  begin
    ws:='';
  end;
  Result:=ws;
end;

class function TCharSetTranslator.TranslateToUtf8(const ASource: WideString): string;
var
  len: Integer;
  s: string;
begin
  Result:='';
  if (ASource = '') then Exit;

  SetLength(s, Length(ASource) * 3);

  len:=ToUtf8(PWideChar(ASource), Length(ASource), PChar(s), Length(s) + 1);
  if (len > 0) then
  begin
    SetLength(s, len - 1);
  end else
  begin
    s:='';
  end;
  Result:=s;
end;

initialization
  CharsetTranslator:=TCharsetTranslator.Create;
finalization
  CharsetTranslator.Free;
end.
