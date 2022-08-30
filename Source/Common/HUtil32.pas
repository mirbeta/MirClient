unit HUtil32;

interface
{.$WARN UNIT_PLATFORM OFF}
uses
  Classes, SysUtils, StrUtils, WinProcs, Graphics, ShlObj;

type
  Str4096 = array[0..4096] of Char;
  Str256 = array[0..256] of Char;
  TyNameTable = record
    Name: string;
    varl: Longint;
  end;

  TLRect = record
    Left, Top, Right, Bottom: Longint;
  end;

const
  MAXDEFCOLOR               = 16;
  ColorNames                : array[1..MAXDEFCOLOR] of TyNameTable = (
    (Name: 'BLACK'; varl: clBlack),
    (Name: 'BROWN'; varl: clMaroon),
    (Name: 'MARGENTA'; varl: clFuchsia),
    (Name: 'GREEN'; varl: clGreen),
    (Name: 'LTGREEN'; varl: clOlive),
    (Name: 'BLUE'; varl: clNavy),
    (Name: 'LTBLUE'; varl: clBlue),
    (Name: 'PURPLE'; varl: clPurple),
    (Name: 'CYAN'; varl: clTeal),
    (Name: 'LTCYAN'; varl: clAqua),
    (Name: 'GRAY'; varl: clGray),
    (Name: 'LTGRAY'; varl: clSilver),
    (Name: 'YELLOW'; varl: clYellow),
    (Name: 'LIME'; varl: clLime),
    (Name: 'WHITE'; varl: clWhite),
    (Name: 'RED'; varl: clRed));

  MAXLISTMARKER             = 3;
  LiMarkerNames             : array[1..MAXLISTMARKER] of TyNameTable =
    (
    (Name: 'DISC'; varl: 0),
    (Name: 'CIRCLE'; varl: 1),
    (Name: 'SQUARE'; varl: 2)
    );

  MAXPREDEFINE              = 3;
  PreDefineNames            : array[1..MAXPREDEFINE] of TyNameTable =
    (
    (Name: 'LEFT'; varl: 0),
    (Name: 'RIGHT'; varl: 1),
    (Name: 'CENTER'; varl: 2)
    );

procedure DisPoseAndNil(var Obj);
function CountGarbage(paper: TCanvas; Src: PChar; TargWidth: Longint): Integer;
function ArrestString(Source, SearchAfter, ArrestBefore: string; const DropTags: array of string; var RsltStr: string): string;
function ArrestStringEx(Source, SearchAfter, ArrestBefore: string; var ArrestStr: string): string;
function CaptureString(Source: string; var rdstr: string): string;
procedure ClearWindow(ACanvas: TCanvas; aLeft, aTop, aRight, aBottom: Longint; aColor: TColor);
function CombineDirFile(SrcDir, TargName: string): string;
function CompareLStr(Src, targ: string; compn: Integer): Boolean;
function CompareBackLStr(Src, targ: string; compn: Integer): Boolean;
function CompareBuffer(p1, p2: PByte; Len: Integer): Boolean;
function CreateMask(Src: PChar; TargPos: Integer): string;
procedure DrawTileImage(Canv: TCanvas; Rect: TRect; TileImage: TBitmap);
procedure DrawingGhost(rc: TRect);
function ExtractFileNameOnly(const FName: string): string;
function FloatToString(F: Real): string;
function FloatToStrFixFmt(fVal: Double; prec, digit: Integer): string;
function FileSize(const FName: string): Longint;
function FileCopy(Source, Dest: string): Boolean;
function FileCopyEx(Source, Dest: string): Boolean;
function GetSpaceCount(Str: string): Longint;
function RemoveSpace(Str: string): string;
function GetFirstWord(Str: string; var sWord: string; var FrontSpace: Longint): string;
function GetDefColorByName(Str: string): TColor;
function GetULMarkerType(Str: string): Longint;
function GetValidStr3(Str: string; var Dest: string; const Divider: array of Char): string;
function GetValidStr4(Str: string; var Dest: string; const Divider: array of Char): string;
function GetValidStrVal(Str: string; var Dest: string; const Divider: array of Char): string;
function GetValidStrCap(Str: string; var Dest: string; const Divider: array of Char): string;
function GetStrToCoords(Str: string): TRect;
function GetDefines(Str: string): Longint;
function GetValueFromMask(Src: PChar; Mask: string): string;
procedure GetDirList(Path: string; flList: TStringList);

function GetFileDate(FileName: string): Integer; //DOS format file date..
function GetItemFormatDate(): Integer;
function GetItemDateTimeInfo(nDateTimeInfo: Integer): TDateTime;
function HexToIntEx(shap_str: string): Longint;
function HexToInt(Str: string): Longint;
function IntToStrFill(num, Len: Integer; Fill: Char): string;
function IsInB(Src: string; Pos: Integer; targ: string): Boolean;
function IsInRect(X, Y: Integer; Rect: TRect): Boolean;
function IsEnglish(ch: Char): Boolean;
function IsChinese(Str: string): Boolean;
function IsEngNumeric(ch: Char): Boolean;
function IsFloatNumeric(Str: string): Boolean;
function IsUniformStr(Src: string; ch: Char): Boolean;
function IsStringNumber(Str: string): Boolean;
function KillFirstSpace(var Str: string): Longint;
procedure KillGabageSpace(var Str: string);
function LRect(L, t, r, b: Longint): TLRect;
procedure MemPCopy(Dest: PChar; Src: string);
procedure MemCpy(Dest, Src: PChar; count: Longint); {PChar type}
procedure memcpy2(TargAddr, SrcAddr: Longint; count: Integer); {Longint type}
procedure memset(Buffer: PChar; FillChar: Char; count: Integer);
procedure PCharSet(p: PChar; n: Integer; ch: Char);
function ReplaceChar(Src: string; srcchr, repchr: Char): string;
function Str_ToDate(Str: string): TDateTime;
function Str_ToTime(Str: string): TDateTime;
function Str_ToInt(Str: string; Def: Longint): Longint;
function Str_ToFloat(Str: string): Real;
function SkipStr(Src: string; const Skips: array of Char): string;
procedure ShlStr(Source: PChar; count: Integer);
procedure ShrStr(Source: PChar; count: Integer);
procedure Str256PCopy(Dest: PChar; const Src: string);
function _StrPas(Dest: PChar): string;
function Str_PCopy(Dest: PChar; Src: string): Integer;
function Str_PCopyEx(Dest: PChar; const Src: string; BufLen: Longint): Integer;
procedure SpliteBitmap(DC: hdc; X, Y: Integer; bitmap: TBitmap; transcolor: TColor);
procedure TiledImage(Canv: TCanvas; Rect: TLRect; TileImage: TBitmap);
function Trim_R(const Str: string): string;
function IsEqualFont(SrcFont, TarFont: TFont): Boolean;
function CutHalfCode(Str: string): string;
function ConvertToShortName(Canvas: TCanvas; Source: string; WantWidth: Integer): string;
{*}
function CatchString(Source: string; cap: Char; var catched: string): string;
function DivString(Source: string; cap: Char; var sel: string): string;
function DivTailString(Source: string; cap: Char; var sel: string): string;
function SPos(substr, Str: string): Integer;
function NumCopy(Str: string): Integer;
function GetMonDay: string;
function BoolToInt(boo: Boolean): Integer;
function BoolToStr(boo: Boolean): string;
function HBoolToStr(b: Boolean): string;
function StrToBool(Str: string): Boolean;
function IntToSex(Int: Integer): string;
function IntToJob(Int: Integer): string;
function IntToStr2(Int: Integer): string;
function BoolToCStr(boo: Boolean): string;
function BoolToIntStr(boo: Boolean): string;
function TagCount(Source: string; tag: Char): Integer;
function _MIN(N1, N2: Integer): Integer;
function _MAX(N1, N2: Integer): Integer;
function _MAX1(N1, N2: Integer): Integer;
function _LMIN(N1: DWORD; N2: UInt64): DWORD;

function CalcFileCRC(FileName: string): Integer;
function CalcBufferCRC(Buffer: PChar; nSize: Integer): Integer;
function IsIPaddr(IP: string): Boolean;
function GetDayCount(MaxDate, MinDate: TDateTime): Integer;
function GetCodeMsgSize(X: Double): Integer;
function NotGetCodeMsgSize(X: Double): Integer;
function pGetCodeMsgSize(X: Double): Integer;
function ConvertPercent(N1, N2: Integer): Integer;
//function DoRound(Value: Extended): Int64;
function JudgeOddEven(var bcs: Integer): Boolean;
function DecStr(SourStr: string; X, Y: Integer): string;
function GetSystemFolderDir(mFolder: Integer): string;
//procedure FillCharSafe(out Dest; count: Integer; Value: Char);
procedure aSort(var a: array of Integer);
procedure DeleteDir(sDirectory: string; RDir: Boolean = True); //删除目录和目录下得所有文件和文件夹
function AddToRAR(WorkPath, RarDir, ArchiveName, FileNames, ArchiveOptions: string): DWORD;
{
          c := AddToRAR(WorkPath, sRarpath, Format('%s.rar', [sid]), sid, '-r');
          if c = 0 then begin
            s := WorkPath + sid + '.rar';
            DeleteDir(WorkPath + sid);
          end;
}

function CRC8(const s: string): Byte;
function CRC16(s: PByteArray; iCount: Integer; OldCRC: Word = 0): Word;
function CRC32(s: PByteArray; iCount: Integer; OldCRC: Cardinal = 0): Cardinal;
function MyGetProcAddress(hModule: hModule; lpProcName: LPCSTR): FARPROC;
procedure SafeFillChar(out X; Count: Integer; Value: Char); overload;
procedure SafeFillChar(out X; Count: Integer; Value: Byte); overload;
procedure SafeFillChar(out X; Count: Integer; Value: Boolean); overload;

function AnsiIsInB(const Src: AnsiString; Pos: Integer; const targ: AnsiString): Boolean;
function AnsiArrestString(const Source, SearchAfter, ArrestBefore: AnsiString; const DropTags: array of AnsiString; var RsltStr: AnsiString): AnsiString;
function AnsiArrestStringEx(Source: AnsiString; const SearchAfter, ArrestBefore: AnsiString; var ArrestStr: AnsiString): AnsiString; inline;
function AnsiCaptureString(const Source: AnsiString; var rdstr: AnsiString): AnsiString;
function AnsiGetValidStr2(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function DBAnsiGetValidStr2(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function AnsiGetValidStr3(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function DBAnsiGetValidStr3(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function AnsiGetValidStr4(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function AnsiGetValidStrVal(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function AnsiGetValidStrCap(Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
function AnsiDivString(const Source: AnsiString; cap: AnsiChar; var sel: AnsiString): AnsiString;
function AnsiDivTailString(const Source: AnsiString; cap: AnsiChar; var sel: AnsiString): AnsiString;
function AnsiTagCount(const Source: AnsiString; Tag: AnsiChar): Integer;
function AnsiTextLength(const S: String): Integer;
function CopyAnisText(const S: string; index, Length: Integer): string;

implementation

uses
  DateUtils;

const
  TABLE_SIZE                = 256;
const
  CRC8Table                 : array[0..TABLE_SIZE - 1] of Byte = (
    $00, $07, $0E, $09, $1C, $1B, $12, $15,
    $38, $3F, $36, $31, $24, $23, $2A, $2D,
    $70, $77, $7E, $79, $6C, $6B, $62, $65,
    $48, $4F, $46, $41, $54, $53, $5A, $5D,
    $E0, $E7, $EE, $E9, $FC, $FB, $F2, $F5,
    $D8, $DF, $D6, $D1, $C4, $C3, $CA, $CD,
    $90, $97, $9E, $99, $8C, $8B, $82, $85,
    $A8, $AF, $A6, $A1, $B4, $B3, $BA, $BD,
    $C7, $C0, $C9, $CE, $DB, $DC, $D5, $D2,
    $FF, $F8, $F1, $F6, $E3, $E4, $ED, $EA,
    $B7, $B0, $B9, $BE, $AB, $AC, $A5, $A2,
    $8F, $88, $81, $86, $93, $94, $9D, $9A,
    $27, $20, $29, $2E, $3B, $3C, $35, $32,
    $1F, $18, $11, $16, $03, $04, $0D, $0A,
    $57, $50, $59, $5E, $4B, $4C, $45, $42,
    $6F, $68, $61, $66, $73, $74, $7D, $7A,
    $89, $8E, $87, $80, $95, $92, $9B, $9C,
    $B1, $B6, $BF, $B8, $AD, $AA, $A3, $A4,
    $F9, $FE, $F7, $F0, $E5, $E2, $EB, $EC,
    $C1, $C6, $CF, $C8, $DD, $DA, $D3, $D4,
    $69, $6E, $67, $60, $75, $72, $7B, $7C,
    $51, $56, $5F, $58, $4D, $4A, $43, $44,
    $19, $1E, $17, $10, $05, $02, $0B, $0C,
    $21, $26, $2F, $28, $3D, $3A, $33, $34,
    $4E, $49, $40, $47, $52, $55, $5C, $5B,
    $76, $71, $78, $7F, $6A, $6D, $64, $63,
    $3E, $39, $30, $37, $22, $25, $2C, $2B,
    $06, $01, $08, $0F, $1A, $1D, $14, $13,
    $AE, $A9, $A0, $A7, $B2, $B5, $BC, $BB,
    $96, $91, $98, $9F, $8A, $8D, $84, $83,
    $DE, $D9, $D0, $D7, $C2, $C5, $CC, $CB,
    $E6, $E1, $E8, $EF, $FA, $FD, $F4, $F3);

  CRC16Table                : array[0..TABLE_SIZE - 1] of Word = (
    $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0);

  Crc16Start                : Cardinal = $FFFF;
  Crc16Bytes                = 2;
  Crc16Bits                 = 16;

  Crc32Table                : array[0..255] of Cardinal = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61, $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039, $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1, $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,
    $34867077, $30476DC0, $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE, $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6, $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637, $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF, $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7, $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8, $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0, $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668, $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4);

  Crc32Start                : Cardinal = $FFFFFFFF;
  Crc32Bits                 = 32;
  Crc32Bytes                = 4;

function CRC8(const s: string): Byte;
var
  i, iLen, iStep            : Integer;
begin
  Result := 0;
  iLen := Length(s);
  if iLen < 32 then begin
    for i := 1 to iLen do begin
      Result := CRC8Table[Result xor Byte(s[i])];
    end;
  end
  else begin
    iStep := iLen div 32 + 1;
    i := 1;
    while i < iLen do begin
      Result := CRC8Table[Result xor Byte(s[i])];
      Inc(i, iStep);
    end;
  end;
end;

function CRC16(s: PByteArray; iCount: Integer; OldCRC: Word = 0): Word;
var
  i, Step, DecCount         : Integer;
begin
  Result := Crc16Start;
  if iCount < 32 then begin
    for i := 0 to iCount - 1 do begin
      Result := CRC16Table[Result shr (Crc16Bits - 8)] xor Word((Result shl 8)) xor s[i];
    end;
  end
  else begin
    Step := iCount div 32 + 1;
    i := 0;
    DecCount := iCount - 1;
    while i < DecCount do begin
      Result := CRC16Table[Result shr (Crc16Bits - 8)] xor Word((Result shl 8)) xor s[i];
      Inc(i, Step);
    end;
  end;
  for i := 0 to Crc16Bytes - 1 do begin
    Result := CRC16Table[Result shr (Crc16Bits - 8)] xor Word((Result shl 8)) xor (OldCRC shr (Crc16Bits - 8));
    OldCRC := Word(OldCRC shl 8);
  end;
end;

function CRC32(s: PByteArray; iCount: Integer; OldCRC: Cardinal = 0): Cardinal;
var
  i, Step, DecCount         : Integer;
begin
  Result := Crc32Start;
  if iCount < 32 then begin
    for i := 0 to iCount - 1 do begin
      Result := Crc32Table[Result shr (Crc32Bits - 8)] xor (Result shl 8) xor s[i];
    end;
  end
  else begin
    Step := iCount div 32 + 1;
    i := 0;
    DecCount := iCount - 1;
    while i < DecCount do begin
      Result := Crc32Table[Result shr (Crc32Bits - 8)] xor (Result shl 8) xor s[i];
      Inc(i, Step);
    end;
  end;
  for i := 0 to Crc32Bytes - 1 do begin
    Result := Crc32Table[Result shr (Crc32Bits - 8)] xor (Result shl 8) xor (OldCRC shr (Crc32Bits - 8));
    OldCRC := OldCRC shl 8;
  end;
end;

function AddToRAR(WorkPath, RarDir, ArchiveName, FileNames, ArchiveOptions: string): DWORD;
var
  PChTmp                    : PChar;
  ExitCodes                 : DWORD;
  si                        : STARTUPINFO;
  Pi                        : PROCESS_INFORMATION;
begin
  PChTmp := PChar('"' + RarDir + '\rar.exe" a ' + ArchiveName + ' ' + FileNames + ' ' + ArchiveOptions);

  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  GetStartupInfo(si);
  si.wShowWindow := SW_HIDE;
  si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  if not CreateProcess(
    nil,                                //lpApplicationName
    PChTmp,                             //lpCommandLine
    nil,                                //lpProcessAttributes
    nil,                                //lpThreadAttributes
    False,                              //bInheritHandles
    0,                                  //dwCreationFlags
    nil,                                //lpEnvironment
    PChar(WorkPath),                    //lpCurrentDirectory
    si,                                 //lpStartupInfo
    Pi                                  //lpProcessInformation
    ) then
    ExitCodes := 999
  else begin
    WaitForSingleObject(Pi.hProcess, INFINITE);
    GetExitCodeProcess(Pi.hProcess, ExitCodes);
  end;
  CloseHandle(Pi.hProcess);
  CloseHandle(Pi.hThread);
  Result := ExitCodes;
end;

procedure DeleteDir(sDirectory: string; RDir: Boolean); //删除目录和目录下得所有文件和文件夹
var
  sr                        : TSearchRec;
  sPath, sFile              : string;
begin
  //检查目录名后面是否有 '\'
  if Copy(sDirectory, Length(sDirectory), 1) <> '\' then
    sPath := sDirectory + '\'
  else
    sPath := sDirectory;

  if FindFirst(sPath + '*.*', faAnyFile, sr) = 0 then begin
    repeat
      sFile := Trim(sr.Name);
      if sFile = '.' then Continue;
      if sFile = '..' then Continue;
      sFile := sPath + sr.Name;
      if (sr.Attr and faDirectory) <> 0 then
        DeleteDir(sFile, True)
      else if (sr.Attr and faAnyFile) = sr.Attr then begin
        SysUtils.DeleteFile(sFile);     //删除文件
      end;
    until FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
  if RDir then RemoveDir(sPath);
end;

procedure aSort(var a: array of Integer);
var
  i, J, t                   : Integer;
begin
  for i := High(a) downto Low(a) do
    for J := Low(a) to High(a) - 1 do
      if a[J] > a[J + 1] then begin
        t := a[J];
        a[J] := a[J + 1];
        a[J + 1] := t;
      end;
end;

procedure SafeFillChar(out X; Count: Integer; Value: Char);
begin
  FillChar(X, Count, Value);
end;

procedure SafeFillChar(out X; Count: Integer; Value: Byte);
begin
  FillChar(X, Count, Value);
end;

procedure SafeFillChar(out X; Count: Integer; Value: Boolean);
begin
  FillChar(X, Count, Value);
end;

function IsIPaddr(IP: string): Boolean;
var
  Node                      : array[0..3] of Integer;
  tIP                       : string;
  tNode                     : string;
  tPos                      : Integer;
  tLen                      : Integer;
begin
  Result := False;
  tIP := IP;
  tLen := Length(tIP);
  tPos := Pos('.', tIP);
  tNode := MidStr(tIP, 1, tPos - 1);
  tIP := MidStr(tIP, tPos + 1, tLen - tPos);
  if not TryStrToInt(tNode, Node[0]) then
    Exit;

  tLen := Length(tIP);
  tPos := Pos('.', tIP);
  tNode := MidStr(tIP, 1, tPos - 1);
  tIP := MidStr(tIP, tPos + 1, tLen - tPos);
  if not TryStrToInt(tNode, Node[1]) then
    Exit;

  tLen := Length(tIP);
  tPos := Pos('.', tIP);
  tNode := MidStr(tIP, 1, tPos - 1);
  tIP := MidStr(tIP, tPos + 1, tLen - tPos);
  if not TryStrToInt(tNode, Node[2]) then
    Exit;

  if not TryStrToInt(tIP, Node[3]) then
    Exit;
  for tLen := Low(Node) to High(Node) do begin
    if (Node[tLen] < 0) or (Node[tLen] > 255) then
      Exit;
  end;
  Result := True;
end;

function CalcFileCRC(FileName: string): Integer;
var
  i                         : Integer;
  nFileHandle               : Integer;
  nFileSize, nBuffSize      : Integer;
  Buffer                    : PChar;
  Int                       : ^Integer;
  nCRC                      : Integer;
begin
  Result := 0;
  if not FileExists(FileName) then
    Exit;
  nFileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if nFileHandle = 0 then
    Exit;
  nFileSize := FileSeek(nFileHandle, 0, 2);
  nBuffSize := (nFileSize div 4) * 4;
  GetMem(Buffer, nBuffSize);
  FillChar(Buffer^, nBuffSize, 0);
  FileSeek(nFileHandle, 0, 0);
  FileRead(nFileHandle, Buffer^, nBuffSize);
  FileClose(nFileHandle);
  Int := Pointer(Buffer);
  nCRC := 0;
  //Exception.Create(IntToStr(SizeOf(Integer))); {Memory Leaked By Blue 2006.03.28}
  for i := 0 to nBuffSize div 4 - 1 do begin
    nCRC := nCRC xor Int^;
    Int := Pointer(Integer(Int) + 4);
  end;
  FreeMem(Buffer, nBuffSize);
  Result := nCRC;
end;

function CalcBufferCRC(Buffer: PChar; nSize: Integer): Integer;
var
  i                         : Integer;
  Int                       : ^Integer;
  nCRC                      : Integer;
begin
  Int := Pointer(Buffer);
  nCRC := 0;
  for i := 0 to nSize div 4 - 1 do begin
    nCRC := nCRC xor Int^;
    Int := Pointer(Integer(Int) + 4);
  end;
  Result := nCRC;
end;
{ capture "double quote streams" }

function CaptureString(Source: string; var rdstr: string): string;
var
  st, et, c, Len, i         : Integer;
begin
  if Source = '' then begin
    rdstr := '';
    Result := '';
    Exit;
  end;
  //et := 0;
  c := 1;
  Len := Length(Source);
  while Source[c] = ' ' do
    if c < Len then
      Inc(c)
    else
      Break;
  if ((Source[c] = '"') or (Source[c] = '(')) and (c < Len) then begin
    st := c + 1;
    et := Len;
    for i := c + 1 to Len do
      if (Source[i] = '"') or (Source[i] = ')') then begin
        et := i - 1;
        Break;
      end;
  end else begin
    st := c;
    et := Len;
    for i := c to Len do
      if Source[i] = ' ' then begin
        et := i - 1;
        Break;
      end;
  end;
  rdstr := Copy(Source, st, (et - st + 1));
  if Len >= (et + 2) then
    Result := Copy(Source, et + 2, Len - (et + 1))
  else
    Result := '';
end;

function CountUglyWhiteChar(sptr: PChar): Longint;
var
  cnt, Killw                : Longint;
begin
  Killw := 0;
  for cnt := (StrLen(sptr) - 1) downto 0 do begin
    if sptr[cnt] = ' ' then begin
      Inc(Killw);
      {sPtr[Cnt] := #0;}
    end
    else
      Break;
  end;
  Result := Killw;
end;

function CountGarbage(paper: TCanvas; Src: PChar; TargWidth: Longint): Integer;
{garbage}
var
  gab, destWidth            : Integer;
begin

  gab := CountUglyWhiteChar(Src);
  destWidth := paper.TextWidth(StrPas(Src)) - gab;
  Result := TargWidth - destWidth + (gab * paper.TextWidth(' '));

end;

function GetSpaceCount(Str: string): Longint;
var
  cnt, Len, SpaceCount      : Longint;
begin
  SpaceCount := 0;
  Len := Length(Str);
  for cnt := 1 to Len do
    if Str[cnt] = ' ' then
      SpaceCount := SpaceCount + 1;
  Result := SpaceCount;
end;

function RemoveSpace(Str: string): string;
var
  i                         : Integer;
begin
  Result := '';
  for i := 1 to Length(Str) do
    if Str[i] <> ' ' then
      Result := Result + Str[i];
end;

function KillFirstSpace(var Str: string): Longint;
var
  cnt, Len                  : Longint;
begin
  Result := 0;
  Len := Length(Str);
  for cnt := 1 to Len do
    if Str[cnt] <> ' ' then begin
      Str := Copy(Str, cnt, Len - cnt + 1);
      Result := cnt - 1;
      Break;
    end;
end;

procedure KillGabageSpace(var Str: string);
var
  cnt, Len                  : Longint;
begin
  Len := Length(Str);
  for cnt := Len downto 1 do
    if Str[cnt] <> ' ' then begin
      Str := Copy(Str, 1, cnt);
      KillFirstSpace(Str);
      Break;
    end;
end;

function GetFirstWord(Str: string; var sWord: string; var FrontSpace: Longint): string;
var
  cnt, Len, n               : Longint;
  DestBuf                   : Str4096;
begin
  Len := Length(Str);
  if Len <= 0 then
    Result := ''
  else begin
    FrontSpace := 0;
    for cnt := 1 to Len do begin
      if Str[cnt] = ' ' then
        Inc(FrontSpace)
      else
        Break;
    end;
    n := 0;
    for cnt := cnt to Len do begin
      if Str[cnt] <> ' ' then
        DestBuf[n] := Str[cnt]
      else begin
        DestBuf[n] := #0;
        sWord := StrPas(DestBuf);
        Result := Copy(Str, cnt, Len - cnt + 1);
        Exit;
      end;
      Inc(n);
    end;
    DestBuf[n] := #0;
    sWord := StrPas(DestBuf);
    Result := '';
  end;
end;

function HexToIntEx(shap_str: string): Longint;
begin
  Result := HexToInt(Copy(shap_str, 2, Length(shap_str) - 1));
end;

function HexToInt(Str: string): Longint;
var
  digit                     : Char;
  count, i                  : Integer;
  cur, Val                  : Longint;
begin
  Val := 0;
  count := Length(Str);
  for i := 1 to count do begin
    digit := Str[i];
    if (digit >= '0') and (digit <= '9') then
      cur := Ord(digit) - Ord('0')
    else if (digit >= 'A') and (digit <= 'F') then
      cur := Ord(digit) - Ord('A') + 10
    else if (digit >= 'a') and (digit <= 'f') then
      cur := Ord(digit) - Ord('a') + 10
    else
      cur := 0;
    Val := Val + (cur shl (4 * (count - i)));
  end;
  Result := Val;
  //   Result := (Val and $0000FF00) or ((Val shl 16) and $00FF0000) or ((Val shr 16) and $000000FF);
end;

{function Str_ToInt(Str: string; Def: LongInt): LongInt;
var
  i                         : Integer;
begin
  for i := 1 to Length(Str) do
    if not (((Word(Str[i]) >= Word('0')) and (Word(Str[i]) <= Word('9'))) or (Str[i] = '+') or (Str[i] = '-')) then begin
      setlength(Str, i - 1);
      Break;
    end;
  Result := Def;
  if Str <> '' then begin
    if ((Word(Str[1]) >= Word('0')) and (Word(Str[1]) <= Word('9'))) or (Str[1] = '+') or (Str[1] = '-') then try
      Result := StrToInt64(Str);
    except
    end;
  end;
  Result := Def;
  Val(Str, v, code);
  if code = 0 then
    Result := v;
end;}

function Str_ToInt(Str: string; Def: Longint): Longint;
begin
  Result := Def;
  if Str <> '' then begin
    if ((Word(Str[1]) >= Word('0')) and (Word(Str[1]) <= Word('9'))) or (Str[1] = '+') or (Str[1] = '-') then try
      Result := StrToInt64Def(Str, Def);
    except
      Result := Def;
    end;
  end;
end;

function Str_ToDate(Str: string): TDateTime;
begin
  if Trim(Str) = '' then
    Result := Date
  else
    Result := StrToDate(Str);
end;

function Str_ToTime(Str: string): TDateTime;
begin
  if Trim(Str) = '' then
    Result := Time
  else
    Result := StrToTime(Str);
end;

function Str_ToFloat(Str: string): Real;
begin
  if Str <> '' then try
    Result := StrToFloat(Str);
    Exit;
  except
  end;
  Result := 0;
end;

procedure DrawingGhost(rc: TRect);
var
  DC                        : hdc;
begin
  DC := GetDC(0);
  DrawFocusRect(DC, rc);
  ReleaseDC(0, DC);
end;

function ExtractFileNameOnly(const FName: string): string;
var
  extpos                    : Integer;
  ext, fn                   : string;
begin
  ext := ExtractFileExt(FName);
  fn := ExtractFileName(FName);
  if ext <> '' then begin
    extpos := Pos(ext, fn);
    Result := Copy(fn, 1, extpos - 1);
  end else
    Result := fn;
end;

function FloatToString(F: Real): string;
begin
  Result := FloatToStrFixFmt(F, 5, 2);
end;

function FloatToStrFixFmt(fVal: Double; prec, digit: Integer): string;
var
  cnt, Dest, Len, i, J      : Integer;
  fstr                      : string;
  buf                       : array[0..255] of Char;
label
  end_conv;
begin
  cnt := 0;
  Dest := 0;
  fstr := FloatToStrF(fVal, ffGeneral, 15, 3);
  Len := Length(fstr);
  for i := 1 to Len do begin
    if fstr[i] = '.' then begin
      buf[Dest] := '.';
      Inc(Dest);
      cnt := 0;
      for J := i + 1 to Len do begin
        if cnt < digit then begin
          buf[Dest] := fstr[J];
          Inc(Dest);
        end
        else begin
          goto end_conv;
        end;
        Inc(cnt);
      end;
      goto end_conv;
    end;
    if cnt < prec then begin
      buf[Dest] := fstr[i];
      Inc(Dest);
    end;
    Inc(cnt);
  end;
  end_conv:
  buf[Dest] := Char(0);
  Result := StrPas(buf);
end;

function FileSize(const FName: string): Longint;
var
  SearchRec                 : TSearchRec;
begin
  if FindFirst(ExpandFileName(FName), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else
    Result := -1;
  SysUtils.FindClose(SearchRec);
end;

function FileCopy(Source, Dest: string): Boolean;
var
  fSrc, fDst, Len           : Integer;
  Size                      : Longint;
  Buffer                    : packed array[0..2047] of Byte;
begin
  Result := False;                      { Assume that it WONT work }
  if Source <> Dest then begin
    fSrc := FileOpen(Source, fmOpenRead);
    if fSrc >= 0 then begin
      Size := FileSeek(fSrc, 0, 2);
      FileSeek(fSrc, 0, 0);
      fDst := FileCreate(Dest);
      if fDst >= 0 then begin
        while Size > 0 do begin
          Len := FileRead(fSrc, Buffer, SizeOf(Buffer));
          FileWrite(fDst, Buffer, Len);
          Size := Size - Len;
        end;
        FileSetDate(fDst, FileGetDate(fSrc));
        FileClose(fDst);
        FileSetAttr(Dest, FileGetAttr(Source));
        Result := True;
      end;
      FileClose(fSrc);
    end;
  end;
end;

function FileCopyEx(Source, Dest: string): Boolean;
var
  fSrc, fDst, Len           : Integer;
  Size                      : Longint;
  Buffer                    : array[0..512000] of Byte;
begin
  Result := False;                      { Assume that it WONT work }
  if Source <> Dest then begin
    fSrc := FileOpen(Source, fmOpenRead or fmShareDenyNone);
    if fSrc >= 0 then begin
      Size := FileSeek(fSrc, 0, 2);
      FileSeek(fSrc, 0, 0);
      fDst := FileCreate(Dest);
      if fDst >= 0 then begin
        while Size > 0 do begin
          Len := FileRead(fSrc, Buffer, SizeOf(Buffer));
          FileWrite(fDst, Buffer, Len);
          Size := Size - Len;
        end;
        FileSetDate(fDst, FileGetDate(fSrc));
        FileClose(fDst);
        FileSetAttr(Dest, FileGetAttr(Source));
        Result := True;
      end;
      FileClose(fSrc);
    end;
  end;
end;

function GetDefColorByName(Str: string): TColor;
var
  cnt                       : Integer;
  COmpStr                   : string;
begin
  COmpStr := UpperCase(Str);
  for cnt := 1 to MAXDEFCOLOR do begin
    if COmpStr = ColorNames[cnt].Name then begin
      Result := TColor(ColorNames[cnt].varl);
      Exit;
    end;
  end;
  Result := $0;
end;

function GetULMarkerType(Str: string): Longint;
var
  cnt                       : Integer;
  COmpStr                   : string;
begin
  COmpStr := UpperCase(Str);
  for cnt := 1 to MAXLISTMARKER do begin
    if COmpStr = LiMarkerNames[cnt].Name then begin
      Result := LiMarkerNames[cnt].varl;
      Exit;
    end;
  end;
  Result := 1;
end;

function GetDefines(Str: string): Longint;
var
  cnt                       : Integer;
  COmpStr                   : string;
begin
  COmpStr := UpperCase(Str);
  for cnt := 1 to MAXPREDEFINE do begin
    if COmpStr = PreDefineNames[cnt].Name then begin
      Result := PreDefineNames[cnt].varl;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure ClearWindow(ACanvas: TCanvas; aLeft, aTop, aRight, aBottom: Longint;
  aColor: TColor);
begin
  with ACanvas do begin
    brush.Color := aColor;
    Pen.Color := aColor;
    Rectangle(0, 0, aRight - aLeft, aBottom - aTop);
  end;
end;

procedure DrawTileImage(Canv: TCanvas; Rect: TRect; TileImage: TBitmap);
var
  i, J, ICnt, JCnt, BmWidth, BmHeight: Integer;
begin

  BmWidth := TileImage.Width;
  BmHeight := TileImage.Height;
  ICnt := ((Rect.Right - Rect.Left) + BmWidth - 1) div BmWidth;
  JCnt := ((Rect.Bottom - Rect.Top) + BmHeight - 1) div BmHeight;

  UnrealizeObject(Canv.Handle);
  SelectPalette(Canv.Handle, TileImage.Palette, False);
  RealizePalette(Canv.Handle);

  for J := 0 to JCnt do begin
    for i := 0 to ICnt do begin

      { if (I * BmWidth) < (Rect.Right-Rect.Left) then
         BmWidth := TileImage.Width else
          BmWidth := (Rect.Right - Rect.Left) - ((I-1) * BmWidth);

       if (
       BmWidth := TileImage.Width;
       BmHeight := TileImage.Height;  }

      BitBlt(Canv.Handle,
        Rect.Left + i * BmWidth,
        Rect.Top + (J * BmHeight),
        BmWidth,
        BmHeight,
        TileImage.Canvas.Handle,
        0,
        0,
        SRCCOPY);

    end;
  end;

end;

procedure TiledImage(Canv: TCanvas; Rect: TLRect; TileImage: TBitmap);
var
  i, J, ICnt, JCnt, BmWidth, BmHeight: Integer;
  Rleft, RTop, RWidth, RHeight, BLeft, btop: Longint;
begin

  if Assigned(TileImage) then
    if TileImage.Handle <> 0 then begin

      BmWidth := TileImage.Width;
      BmHeight := TileImage.Height;
      ICnt := (Rect.Right + BmWidth - 1) div BmWidth - (Rect.Left div BmWidth);
      JCnt := (Rect.Bottom + BmHeight - 1) div BmHeight - (Rect.Top div
        BmHeight);

      UnrealizeObject(Canv.Handle);
      SelectPalette(Canv.Handle, TileImage.Palette, False);
      RealizePalette(Canv.Handle);

      for J := 0 to JCnt do begin
        for i := 0 to ICnt do begin

          if i = 0 then begin
            BLeft := Rect.Left - ((Rect.Left div BmWidth) * BmWidth);
            Rleft := Rect.Left;
            RWidth := BmWidth;
          end
          else begin
            if i = ICnt then
              RWidth := Rect.Right - ((Rect.Right div BmWidth) * BmWidth)
            else
              RWidth := BmWidth;
            BLeft := 0;
            Rleft := (Rect.Left div BmWidth) + (i * BmWidth);
          end;

          if J = 0 then begin
            btop := Rect.Top - ((Rect.Top div BmHeight) * BmHeight);
            RTop := Rect.Top;
            RHeight := BmHeight;
          end
          else begin
            if J = JCnt then
              RHeight := Rect.Bottom - ((Rect.Bottom div BmHeight) * BmHeight)
            else
              RHeight := BmHeight;
            btop := 0;
            RTop := (Rect.Top div BmHeight) + (J * BmHeight);
          end;

          BitBlt(Canv.Handle,
            Rleft,
            RTop,
            RWidth,
            RHeight,
            TileImage.Canvas.Handle,
            BLeft,
            btop,
            SRCCOPY);

        end;
      end;

    end;
end;

function GetValidStr3(Str: string; var Dest: string; const Divider: array of Char): string;
const
  BUF_SIZE                  = 2 * 32 * 1024;
var
  buf                       : array[0..BUF_SIZE] of Char;
  i, BufCount, count        : Longint;
  SrcLen, ArrCount          : Longint;
  ch                        : Char;
label
  CATCH_DIV;
begin
  ch := #0;
  try
    SrcLen := Length(Str);
    BufCount := 0;
    count := 1;

    if SrcLen >= BUF_SIZE - 1 then begin //源字符串超长，直接退出
      Result := '';
      Dest := '';
      Exit;
    end;

    if Str = '' then begin //源字符串为空，直接退出
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := SizeOf(Divider) div SizeOf(Char); //字符数组长度

    while True do begin
      if count <= SrcLen then begin
        ch := Str[count];
        for i := 0 to ArrCount - 1 do begin
          if ch = Divider[i] then
            goto CATCH_DIV;
        end;
      end;

      if (count > SrcLen) then begin
        CATCH_DIV:
        if (BufCount > 0) then begin
          if BufCount < BUF_SIZE - 1 then begin
            buf[BufCount] := #0;
            Dest := string(buf);
            Result := Copy(Str, count + 1, SrcLen - count);
          end;
          Break;
        end
        else begin
          if (count > SrcLen) then begin
            Dest := '';
            Result := Copy(Str, count + 2, SrcLen - 1);
            Break;
          end;
        end;
      end
      else begin
        if BufCount < BUF_SIZE - 1 then begin
          buf[BufCount] := ch;
          Inc(BufCount);
        end;
      end;

      Inc(count);
    end;
  except
    Dest := '';
    Result := '';
  end;
end;

function GetValidStr4(Str: string; var Dest: string; const Divider: array of Char): string;
const
  BUF_SIZE                  = 2 * 32 * 1024; //= 18200;    //$7FFF;
var
  buf                       : array[0..BUF_SIZE] of Char;
  BufCount, count, SrcLen, i, ArrCount: Longint;
  ch                        : Char;
label
  CATCH_DIV;
begin
  ch := #0;                             //Jacky
  try
    //EnterCriticalSection (CSUtilLock);
    SrcLen := Length(Str);
    BufCount := 0;
    count := 1;

    if Str = '' then begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := SizeOf(Divider) div SizeOf(Char);

    while True do begin
      if count <= SrcLen then begin
        ch := Str[count];
        for i := 0 to ArrCount - 1 do
          if ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if (count > SrcLen) then begin
        CATCH_DIV:
        if (BufCount > 0) or (ch <> ' ') then begin
          if BufCount <= 0 then begin
            buf[0] := ch;
            buf[1] := #0;
            ch := ' ';
          end else
            buf[BufCount] := #0;
          Dest := string(buf);          //0629
          if ch <> ' ' then
            Result := Copy(Str, count, SrcLen - count + 1)
              //remain divider in rest-string,
          else
            Result := Copy(Str, count + 1, SrcLen - count); //exclude whitespace
          Break;
        end
        else begin
          if (count > SrcLen) then begin
            Dest := '';
            Result := Copy(Str, count + 2, SrcLen - 1);
            Break;
          end;
        end;
      end
      else begin
        if BufCount < BUF_SIZE - 1 then begin
          buf[BufCount] := ch;
          Inc(BufCount);
        end else
          //ShowMessage('BUF_SIZE overflow !');
      end;
      Inc(count);
    end;
  finally
    //LeaveCriticalSection (CSUtilLock);
  end;
end;

function GetValidStrVal(Str: string; var Dest: string; const Divider: array of Char): string;
//箭磊甫 盒府秦晨 ex) 12.30mV
const
  BUF_SIZE                  = 2 * 32 * 1024; //= 15600;
var
  buf                       : array[0..BUF_SIZE] of Char;
  BufCount, count, SrcLen, i, ArrCount: Longint;
  ch                        : Char;
  currentNumeric            : Boolean;
  hexmode                   : Boolean;
label
  CATCH_DIV;
begin
  ch := #0;                             //Jacky
  try
    //EnterCriticalSection (CSUtilLock);
    hexmode := False;
    SrcLen := Length(Str);
    BufCount := 0;
    count := 1;
    currentNumeric := False;

    if Str = '' then begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := SizeOf(Divider) div SizeOf(Char);

    while True do begin
      if count <= SrcLen then begin
        ch := Str[count];
        for i := 0 to ArrCount - 1 do
          if ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if not currentNumeric then begin
        if (count + 1) < SrcLen then begin
          if (Str[count] = '0') and (UpCase(Str[count + 1]) = 'X') then begin
            buf[BufCount] := Str[count];
            buf[BufCount + 1] := Str[count + 1];
            Inc(BufCount, 2);
            Inc(count, 2);
            hexmode := True;
            currentNumeric := True;
            Continue;
          end;
          if (ch = '-') and (Str[count + 1] >= '0') and (Str[count + 1] <= '9') then begin
            currentNumeric := True;
          end;
        end;
        if (ch >= '0') and (ch <= '9') then begin
          currentNumeric := True;
        end;
      end
      else begin
        if hexmode then begin
          if not (((ch >= '0') and (ch <= '9')) or
            ((ch >= 'A') and (ch <= 'F')) or
            ((ch >= 'a') and (ch <= 'f'))) then begin
            Dec(count);
            goto CATCH_DIV;
          end;
        end
        else if ((ch < '0') or (ch > '9')) and (ch <> '.') then begin
          Dec(count);
          goto CATCH_DIV;
        end;
      end;
      if (count > SrcLen) then begin
        CATCH_DIV:
        if (BufCount > 0) then begin
          buf[BufCount] := #0;
          Dest := string(buf);          //0629
          Result := Copy(Str, count + 1, SrcLen - count);
          Break;
        end
        else begin
          if (count > SrcLen) then begin
            Dest := '';
            Result := Copy(Str, count + 2, SrcLen - 1);
            Break;
          end;
        end;
      end
      else begin
        if BufCount < BUF_SIZE - 1 then begin
          buf[BufCount] := ch;
          Inc(BufCount);
        end else
          //ShowMessage('BUF_SIZE overflow !');
      end;
      Inc(count);
    end;
  finally
    //LeaveCriticalSection (CSUtilLock);
  end;
end;

{" " capture => CaptureString (source: string; var rdstr: string): string;
 ** 贸澜俊 " 绰 亲惑 盖 贸澜俊 乐促绊 啊沥}

function GetValidStrCap(Str: string; var Dest: string; const Divider: array of Char): string;
begin
  Str := TrimLeft(Str);
  if Str <> '' then begin
    if (Str[1] = '"') or (Str[1] = '(') then
      Result := CaptureString(Str, Dest)
    else begin
      Result := GetValidStr3(Str, Dest, Divider);
    end;
  end
  else begin
    Result := '';
    Dest := '';
  end;
end;

function IntToStrFill(num, Len: Integer; Fill: Char): string;
var
  i                         : Integer;
  Str                       : string;
begin
  Result := '';
  Str := IntToStr(num);
  for i := 1 to Len - Length(Str) do
    Result := Result + Fill;
  Result := Result + Str;
end;

function IntToStr2(Int: Integer): string;
begin
  if Int < 10 then begin
    Result := '0' + IntToStr(Int);
  end
  else begin
    Result := IntToStr(Int);
  end;
end;

function IsInB(Src: string; Pos: Integer; targ: string): Boolean;
var
  tLen, i                   : Integer;
begin
  Result := False;
  tLen := Length(targ);
  if Length(Src) < Pos + tLen then
    Exit;
  for i := 0 to tLen - 1 do
    if UpCase(Src[Pos + i]) <> UpCase(targ[i + 1]) then
      Exit;

  Result := True;
end;

function IsInRect(X, Y: Integer; Rect: TRect): Boolean;
begin
  if (X >= Rect.Left) and (X <= Rect.Right) and (Y >= Rect.Top) and (Y <=
    Rect.Bottom) then
    Result := True
  else
    Result := False;
end;

function IsStringNumber(Str: string): Boolean;
var
  i                         : Integer;
begin
  Result := True;
  for i := 1 to Length(Str) do
    if (Byte(Str[i]) < Byte('0')) or (Byte(Str[i]) > Byte('9')) then begin
      Result := False;
      Break;
    end;
end;

{Return : remain string}

function ArrestString(Source, SearchAfter, ArrestBefore: string; const DropTags: array of string; var RsltStr: string): string;
const
  BUF_SIZE                  = 2 * 32 * 1024; //= $7FFF;
var
  buf                       : array[0..BUF_SIZE] of Char;
  BufCount, SrcCount, SrcLen, {AfterLen, BeforeLen,} DropCount, i: Integer;
  ArrestNow                 : Boolean;
begin
  try
    //EnterCriticalSection (CSUtilLock);
    RsltStr := '';                      {result string}
    SrcLen := Length(Source);

    if SrcLen > BUF_SIZE then begin
      Result := '';
      Exit;
    end;

    BufCount := 0;
    SrcCount := 1;
    ArrestNow := False;
    DropCount := SizeOf(DropTags) div SizeOf(string);

    if (SearchAfter = '') then
      ArrestNow := True;

    //GetMem (Buf, BUF_SIZE);

    while True do begin
      if SrcCount > SrcLen then
        Break;

      if not ArrestNow then begin
        if IsInB(Source, SrcCount, SearchAfter) then
          ArrestNow := True;
      end
      else begin
        buf[BufCount] := Source[SrcCount];
        if IsInB(Source, SrcCount, ArrestBefore) or (BufCount >= BUF_SIZE - 2) then begin
          BufCount := BufCount - Length(ArrestBefore);
          buf[BufCount + 1] := #0;
          RsltStr := string(buf);       //0629
          BufCount := 0;
          Break;
        end;

        for i := 0 to DropCount - 1 do begin
          if IsInB(Source, SrcCount, DropTags[i]) then begin
            BufCount := BufCount - Length(DropTags[i]);
            Break;
          end;
        end;

        Inc(BufCount);
      end;
      Inc(SrcCount);
    end;

    if (ArrestNow) and (BufCount <> 0) then begin
      buf[BufCount] := #0;
      RsltStr := string(buf);           //0629
    end;

    Result := Copy(Source, SrcCount + 1, SrcLen - SrcCount);
    {result is remain string}
  finally
    //LeaveCriticalSection (CSUtilLock);
  end;
end;

(*function ArrestStringEx2(Source: string; SearchAfter, ArrestBefore: Char; var ArrestStr: string): string;
var
  srclen                    : Integer;
  GoodData                  : Boolean;
  i, n                      : Integer;
begin
  ArrestStr := '';                      {result string}
  if Source = '' then begin
    Result := '';
    Exit;
  end;
  try
    srclen := Length(Source);
    GoodData := False;
    if srclen >= 2 then
      if Source[1] = SearchAfter then begin
        Source := Copy(Source, 2, srclen - 1);
        srclen := Length(Source);
        GoodData := True;
      end else begin
        n := Pos(SearchAfter, Source);
        if n > 0 then begin
          Source := Copy(Source, n + 1, srclen - (n));
          srclen := Length(Source);
          GoodData := True;
        end;
      end;
    if GoodData then begin
      n := Pos(ArrestBefore, Source);
      if n > 0 then begin
        ArrestStr := Copy(Source, 1, n - 1);
        Result := Copy(Source, n + 1, srclen - n);
      end else
        Result := SearchAfter + Source;
    end else begin
      for i := 1 to srclen do begin
        if Source[i] = SearchAfter then begin
          Result := Copy(Source, i, srclen - i + 1);
          Break;
        end;
      end;
    end;
  except
    ArrestStr := '';
    Result := '';
  end;
end;*)

function ArrestStringEx(Source, SearchAfter, ArrestBefore: string; var ArrestStr: string): string;
var
  SrcLen                    : Integer;
  i, n                      : Integer;
begin
  ArrestStr := '';                      {result string}
  if Source = '' then begin
    Result := '';
    Exit;
  end;

  try
    SrcLen := Length(Source);
    //---------------算法优化开始------------------------
    i := Pos(SearchAfter, Source);
    if i > 0 then begin                 //有前缀
      n := posEx(ArrestBefore, Source, i + 1);
      if n > i then begin               //有后缀
        ArrestStr := Copy(Source, i + 1, n - i - 1); //取前后缀之间的字符串
        Result := Copy(Source, n + 1, SrcLen - n); //剩余数据取后缀之后的字符串
      end else begin                    //有前缀没后缀
        ArrestStr := Copy(Source, i + 1, SrcLen - i); //前缀之后的字符串都是ArrestStr
        Result := '';                   //剩余数据为空
      end;
    end else begin                      //没有前缀
      n := Pos(ArrestBefore, Source);
      if n > 0 then begin               //没前缀有后缀
        ArrestStr := Copy(Source, 1, n - 1); //取后缀之前的字符串
        Result := Copy(Source, n + 1, SrcLen - n) //剩余数据为后缀之后的字符串
      end else begin                    //没前缀也没后缀
        ArrestStr := Source;            //取全部数据
        Result := '';                   //剩余数据为空
      end;
    end;
    //---------------算法优化结束------------------------
  except
    ArrestStr := '';
    Result := '';
  end;
end;

//取出前后缀之间的字符串到ArrestStr,并返回后面剩余部分的字符串

(*function ArrestStringEx(Source, SearchAfter, ArrestBefore: string; var ArrestStr: string {; var startPos: Integer}): string;
var
  srclen, startPos          : Integer;
  i, n                      : Integer;
begin
  ArrestStr := '';                      {result string}
  if Source = '' then begin
    Result := '';
    Exit;
  end;
  try
    srclen := Length(Source);
    startPos := 1;
    //---------------算法优化开始------------------------
    i := posEx(SearchAfter, Source, startPos);
    if i > 0 then begin                 //有前缀
      n := posEx(ArrestBefore, Source, i + 1);
      if n > i then begin               //有后缀
        ArrestStr := Copy(Source, i + 1, n - i - 1); //取前后缀之间的字符串
        Result := Copy(Source, n + 1, srclen - n); //剩余数据取后缀之后的字符串
        startPos := i + 1;
      end else begin                    //有前缀没后缀
        ArrestStr := Copy(Source, i + 1, srclen - i); //前缀之后的字符串都是ArrestStr
        Result := '';                   //剩余数据为空
        startPos := srclen + 1;
      end;
    end else begin                      //没有前缀
      n := Pos(ArrestBefore, Source);
      if n > 0 then begin               //没前缀有后缀
        ArrestStr := Copy(Source, 1, n - 1); //取后缀之前的字符串
        Result := Copy(Source, n + 1, srclen - n); //剩余数据为后缀之后的字符串
        startPos := i + 1;
      end else begin                    //没前缀也没后缀
        ArrestStr := Source;            //取全部数据
        Result := '';                   //剩余数据为空
        startPos := srclen;
      end;
    end;
    //---------------算法优化结束------------------------
  except
    ArrestStr := '';
    Result := '';
  end;
end;*)

function SkipStr(Src: string; const Skips: array of Char): string;
var
  i, Len, c                 : Integer;
  NowSkip                   : Boolean;
begin
  Len := Length(Src);
  for i := 1 to Len do begin
    NowSkip := False;
    for c := Low(Skips) to High(Skips) do
      if Src[i] = Skips[c] then begin
        NowSkip := True;
        Break;
      end;
    if not NowSkip then
      Break;
  end;
  Result := Copy(Src, i, Len - i + 1);
end;

function GetStrToCoords(Str: string): TRect;
var
  temp                      : string;
begin
  Str := GetValidStr3(Str, temp, [',', ' ']);
  Result.Left := Str_ToInt(temp, 0);
  Str := GetValidStr3(Str, temp, [',', ' ']);
  Result.Top := Str_ToInt(temp, 0);
  Str := GetValidStr3(Str, temp, [',', ' ']);
  Result.Right := Str_ToInt(temp, 0);
  GetValidStr3(Str, temp, [',', ' ']);
  Result.Bottom := Str_ToInt(temp, 0);
end;

function CombineDirFile(SrcDir, TargName: string): string;
begin
  if (SrcDir = '') or (TargName = '') then begin
    Result := SrcDir + TargName;
    Exit;
  end;
  if SrcDir[Length(SrcDir)] = '\' then
    Result := SrcDir + TargName
  else
    Result := SrcDir + '\' + TargName;
end;

function CompareLStr(Src, targ: string; compn: Integer): Boolean;
var
  i                         : Integer;
begin
  Result := False;
  if (compn <= 0) or (Length(Src) < compn) or (Length(targ) < compn) then Exit;
  Result := True;
  for i := 1 to compn do
    if UpCase(Src[i]) <> UpCase(targ[i]) then begin
      Result := False;
      Break;
    end;
end;

function CompareBuffer(p1, p2: PByte; Len: Integer): Boolean;
var
  i                         : Integer;
begin
  Result := True;
  for i := 0 to Len - 1 do
    if PByte(Integer(p1) + i)^ <> PByte(Integer(p2) + i)^ then begin
      Result := False;
      Break;
    end;
end;

function CompareBackLStr(Src, targ: string; compn: Integer): Boolean;
var
  i, slen, tLen             : Integer;
begin
  Result := False;
  if compn <= 0 then
    Exit;
  if Length(Src) < compn then
    Exit;
  if Length(targ) < compn then
    Exit;
  slen := Length(Src);
  tLen := Length(targ);
  Result := True;
  for i := 0 to compn - 1 do
    if UpCase(Src[slen - i]) <> UpCase(targ[tLen - i]) then begin
      Result := False;
      Break;
    end;
end;

function IsEnglish(ch: Char): Boolean;
begin
  Result := False;
  if ((ch >= 'A') and (ch <= 'Z')) or ((ch >= 'a') and (ch <= 'z')) then
    Result := True;
end;

function IsEngNumeric(ch: Char): Boolean;
begin
  Result := False;
  if IsEnglish(ch) or ((ch >= '0') and (ch <= '9')) then
    Result := True;
end;

function IsFloatNumeric(Str: string): Boolean;
begin
  if Trim(Str) = '' then begin
    Result := False;
    Exit;
  end;
  try
    StrToFloat(Str);
    Result := True;
  except
    Result := False;
  end;
end;

procedure PCharSet(p: PChar; n: Integer; ch: Char);
var
  i                         : Integer;
begin
  for i := 0 to n - 1 do
    (p + i)^ := ch;
end;

function ReplaceChar(Src: string; srcchr, repchr: Char): string;
var
  i, Len                    : Integer;
begin
  if Src <> '' then begin
    Len := Length(Src);
    for i := 1 to Len do
      if Src[i] = srcchr then
        Src[i] := repchr;
  end;
  Result := Src;
end;

function IsUniformStr(Src: string; ch: Char): Boolean;
var
  i, Len                    : Integer;
begin
  Result := True;
  if Src <> '' then begin
    Len := Length(Src);
    for i := 1 to Len do
      if Src[i] = ch then begin
        Result := False;
        Break;
      end;
  end;
end;

function CreateMask(Src: PChar; TargPos: Integer): string;

  function IsNumber(Chr: Char): Boolean;
  begin
    if (Chr >= '0') and (Chr <= '9') then
      Result := True
    else
      Result := False;
  end;
var
  intFlag, loop             : Boolean;
  cnt, IntCnt, SrcLen       : Integer;
  ch, Ch2                   : Char;
begin
  intFlag := False;
  loop := True;
  cnt := 0;
  IntCnt := 0;
  SrcLen := StrLen(Src);

  while loop do begin
    ch := PChar(Longint(Src) + cnt)^;
    case ch of
      #0: begin
          Result := '';
          Break;
        end;
      ' ': begin
        end;
    else begin

        if not intFlag then begin       { Now Reading char }
          if IsNumber(ch) then begin
            intFlag := True;
            Inc(IntCnt);
          end;
        end
        else begin                      { If, now reading integer }
          if not IsNumber(ch) then begin { XXE+3 }
            case UpCase(ch) of
              'E': begin
                  if (cnt >= 1) and (cnt + 2 < SrcLen) then begin
                    ch := PChar(Longint(Src) + cnt - 1)^;
                    if IsNumber(ch) then begin
                      ch := PChar(Longint(Src) + cnt + 1)^;
                      Ch2 := PChar(Longint(Src) + cnt + 2)^;
                      if not ((ch = '+') and (IsNumber(Ch2))) then begin
                        intFlag := False;
                      end;
                    end;
                  end;
                end;
              '+': begin
                  if (cnt >= 1) and (cnt + 1 < SrcLen) then begin
                    ch := PChar(Longint(Src) + cnt - 1)^;
                    Ch2 := PChar(Longint(Src) + cnt + 1)^;
                    if not ((UpCase(ch) = 'E') and (IsNumber(Ch2))) then begin
                      intFlag := False;
                    end;
                  end;
                end;
              '.': begin
                  if (cnt >= 1) and (cnt + 1 < SrcLen) then begin
                    ch := PChar(Longint(Src) + cnt - 1)^;
                    Ch2 := PChar(Longint(Src) + cnt + 1)^;
                    if not ((IsNumber(ch)) and (IsNumber(Ch2))) then begin
                      intFlag := False;
                    end;
                  end;
                end;

            else
              intFlag := False;
            end;
          end;
        end;                            {end of case else}
      end;                              {end of Case}
    end;
    if (intFlag) and (cnt >= TargPos) then begin
      Result := '%' + format('%d', [IntCnt]);
      Exit;
    end;
    Inc(cnt);
  end;
end;

function GetValueFromMask(Src: PChar; Mask: string): string;

  function Positon(Str: string): Integer;
  var
    str2                    : string;
  begin
    str2 := Copy(Str, 2, Length(Str) - 1);
    Result := StrToIntDef(str2, 0);
    if Result <= 0 then
      Result := 1;
  end;

  function IsNumber(ch: Char): Boolean;
  begin
    case ch of
      '0'..'9': Result := True;
    else
      Result := False;
    end;
  end;
var
  intFlag, loop, Sign       : Boolean;
  buf                       : Str256;
  BufCount, Pos, LocCount, TargLoc, SrcLen: Integer;
  ch, Ch2                   : Char;
begin
  SrcLen := StrLen(Src);
  LocCount := 0;
  BufCount := 0;
  Pos := 0;
  intFlag := False;
  loop := True;
  Sign := False;

  if Mask = '' then
    Mask := '%1';
  TargLoc := Positon(Mask);

  while loop do begin
    if Pos >= SrcLen then
      Break;
    ch := PChar(Src + Pos)^;
    if not intFlag then begin           {now reading chars}
      if LocCount < TargLoc then begin
        if IsNumber(ch) then begin
          intFlag := True;
          BufCount := 0;
          Inc(LocCount);
        end
        else begin
          if not Sign then begin        {default '+'}
            if ch = '-' then
              Sign := True;
          end
          else begin
            if ch <> ' ' then
              Sign := False;
          end;
        end;
      end
      else begin
        Break;
      end;
    end;
    if intFlag then begin               {now reading numbers}
      buf[BufCount] := ch;
      Inc(BufCount);
      if not IsNumber(ch) then begin
        case ch of
          'E', 'e': begin
              if (Pos >= 1) and (Pos + 2 < SrcLen) then begin
                ch := PChar(Src + Pos - 1)^;
                if IsNumber(ch) then begin
                  ch := PChar(Src + Pos + 1)^;
                  Ch2 := PChar(Src + Pos + 2)^;
                  if not ((ch = '+') or (ch = '-') and (IsNumber(Ch2))) then begin
                    Dec(BufCount);
                    intFlag := False;
                  end;
                end;
              end;
            end;
          '+', '-': begin
              if (Pos >= 1) and (Pos + 1 < SrcLen) then begin
                ch := PChar(Src + Pos - 1)^;
                Ch2 := PChar(Src + Pos + 1)^;
                if not ((UpCase(ch) = 'E') and (IsNumber(Ch2))) then begin
                  Dec(BufCount);
                  intFlag := False;
                end;
              end;
            end;
          '.': begin
              if (Pos >= 1) and (Pos + 1 < SrcLen) then begin
                ch := PChar(Src + Pos - 1)^;
                Ch2 := PChar(Src + Pos + 1)^;
                if not ((IsNumber(ch)) and (IsNumber(Ch2))) then begin
                  Dec(BufCount);
                  intFlag := False;
                end;
              end;
            end;
        else begin
            intFlag := False;
            Dec(BufCount);
          end;
        end;
      end;
    end;
    Inc(Pos);
  end;
  if LocCount = TargLoc then begin
    buf[BufCount] := #0;
    if Sign then
      Result := '-' + StrPas(buf)
    else
      Result := StrPas(buf);
  end
  else
    Result := '';
end;

procedure GetDirList(Path: string; flList: TStringList);
var
  SearchRec                 : TSearchRec;
begin
  if FindFirst(Path, faAnyFile, SearchRec) = 0 then begin
    flList.AddObject(SearchRec.Name, TObject(SearchRec.Time));
    while True do begin
      if FindNext(SearchRec) = 0 then
        flList.AddObject(SearchRec.Name, TObject(SearchRec.Time))
      else begin
        SysUtils.FindClose(SearchRec);
        Break;
      end;
    end;
  end;
end;

function GetFileDate(FileName: string): Integer; //DOS format file date..
var
  SearchRec                 : TSearchRec;
begin
  Result := 0;                          //jacky
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then begin
    Result := SearchRec.Time;
    SysUtils.FindClose(SearchRec);
  end;
end;

function GetItemFormatDate(): Integer;
var
  t, d, DT, DTN             : TDateTime;
begin
  t := EncodeTime(8, 0, 0, 0);
  d := EncodeDate(1970, 1, 1);
  DT := d + t;
  t := Time;
  d := Date;
  DTN := d + t;
  Result := Round((DTN - DT) * (24 * 60 * 60));
end;

function GetItemDateTimeInfo(nDateTimeInfo: Integer): TDateTime;
begin
  Result := nDateTimeInfo / (24 * 60 * 60) + (EncodeDate(1970, 1, 1) + EncodeTime(7, 59, 59, 0));
end;

procedure ShlStr(Source: PChar; count: Integer);
var
  i, Len                    : Integer;
begin
  Len := StrLen(Source);
  while (count > 0) do begin
    for i := 0 to Len - 2 do
      Source[i] := Source[i + 1];
    Source[Len - 1] := #0;
    Dec(count);
  end;
end;

procedure ShrStr(Source: PChar; count: Integer);
var
  i, Len                    : Integer;
begin
  Len := StrLen(Source);
  while (count > 0) do begin
    for i := Len - 1 downto 0 do
      Source[i + 1] := Source[i];
    Source[Len + 1] := #0;
    Dec(count);
  end;
end;

function LRect(L, t, r, b: Longint): TLRect;
begin
  Result.Left := L;
  Result.Top := t;
  Result.Right := r;
  Result.Bottom := b;
end;

procedure MemPCopy(Dest: PChar; Src: string);
var
  i                         : Integer;
begin
  for i := 0 to Length(Src) - 1 do
    Dest[i] := Src[i + 1];
end;

procedure MemCpy(Dest, Src: PChar; count: Longint);
var
  i                         : Longint;
begin
  for i := 0 to count - 1 do begin
    PChar(Longint(Dest) + i)^ := PChar(Longint(Src) + i)^;
  end;
end;

procedure memcpy2(TargAddr, SrcAddr: Longint; count: Integer);
var
  i                         : Integer;
begin
  for i := 0 to count - 1 do
    PChar(TargAddr + i)^ := PChar(SrcAddr + i)^;
end;

procedure memset(Buffer: PChar; FillChar: Char; count: Integer);
var
  i                         : Integer;
begin
  for i := 0 to count - 1 do
    Buffer[i] := FillChar;
end;

procedure Str256PCopy(Dest: PChar; const Src: string);
begin
  StrPLCopy(Dest, Src, 255);
end;

function _StrPas(Dest: PChar): string;
var
  i                         : Integer;
begin
  Result := '';
  for i := 0 to Length(Dest) - 1 do
    if Dest[i] <> Chr(0) then
      Result := Result + Dest[i]
    else
      Break;
end;

function Str_PCopy(Dest: PChar; Src: string): Integer;
var
  Len, i                    : Integer;
begin
  Len := Length(Src);
  for i := 1 to Len do
    Dest[i - 1] := Src[i];
  Dest[Len] := #0;
  Result := Len;
end;

function Str_PCopyEx(Dest: PChar; const Src: string; BufLen: Longint): Integer;
var
  Len, i                    : Integer;
begin
  Len := _MIN(Length(Src), BufLen);
  for i := 1 to Len do
    Dest[i - 1] := Src[i];
  Dest[Len] := #0;
  Result := Len;
end;

function Str_Catch(Src, Dest: string; Len: Integer): string; //Result is rests..
begin

end;

function Trim_R(const Str: string): string;
var
  i, Len, tr                : Integer;
begin
  tr := 0;
  Len := Length(Str);
  for i := Len downto 1 do
    if Str[i] = ' ' then
      Inc(tr)
    else
      Break;
  Result := Copy(Str, 1, Len - tr);
end;

function IsEqualFont(SrcFont, TarFont: TFont): Boolean;
begin
  Result := True;
  if SrcFont.Name <> TarFont.Name then
    Result := False;
  if SrcFont.Color <> TarFont.Color then
    Result := False;
  if SrcFont.Style <> TarFont.Style then
    Result := False;
  if SrcFont.Size <> TarFont.Size then
    Result := False;
end;

function CutHalfCode(Str: string): string;
var
  Pos, Len                  : Integer;
begin
  Result := '';
  Pos := 1;
  Len := Length(Str);
  while True do begin
    if Pos > Len then
      Break;
    if (Str[Pos] > #127) then begin
      if ((Pos + 1) <= Len) and (Str[Pos + 1] > #127) then begin
        Result := Result + Str[Pos] + Str[Pos + 1];
        Inc(Pos);
      end;
    end
    else
      Result := Result + Str[Pos];
    Inc(Pos);
  end;
end;

function ConvertToShortName(Canvas: TCanvas; Source: string; WantWidth:
  Integer): string;
var
  i, Len                    : Integer;
  Str                       : string;
begin
  if Length(Source) > 3 then
    if Canvas.TextWidth(Source) > WantWidth then begin
      Len := Length(Source);
      for i := 1 to Len do begin
        Str := Copy(Source, 1, (Len - i));
        Str := Str + '..';
        if Canvas.TextWidth(Str) < (WantWidth - 4) then begin
          Result := CutHalfCode(Str);
          Exit;
        end;
      end;
      Result := CutHalfCode(Copy(Source, 1, 2)) + '..';
      Exit;
    end;
  Result := Source;
end;

function DuplicateBitmap(bitmap: TBitmap): HBitmap;
var
  hbmpOldSrc, hbmpOldDest, hbmpNew: HBitmap;
  hdcSrc, hdcDest           : hdc;

begin
  hdcSrc := CreateCompatibleDC(0);
  hdcDest := CreateCompatibleDC(hdcSrc);
  hbmpOldSrc := SelectObject(hdcSrc, bitmap.Handle);
  hbmpNew := CreateCompatibleBitmap(hdcSrc, bitmap.Width, bitmap.Height);
  hbmpOldDest := SelectObject(hdcDest, hbmpNew);
  BitBlt(hdcDest, 0, 0, bitmap.Width, bitmap.Height, hdcSrc, 0, 0, SRCCOPY);
  SelectObject(hdcDest, hbmpOldDest);
  SelectObject(hdcSrc, hbmpOldSrc);
  DeleteDC(hdcDest);
  DeleteDC(hdcSrc);
  Result := hbmpNew;
end;

procedure SpliteBitmap(DC: hdc; X, Y: Integer; bitmap: TBitmap; transcolor:
  TColor);
var
  hdcMixBuffer, hdcBackMask, hdcForeMask, hdcCopy: hdc;
  hOld, hbmCopy, hbmMixBuffer, hbmBackMask, hbmForeMask: HBitmap;
  OldColor                  : TColor;
begin
  {UnrealizeObject (DC);}
(* SelectPalette (DC, bitmap.Palette, FALSE);
  RealizePalette (DC);*)

  hbmCopy := DuplicateBitmap(bitmap);
  hdcCopy := CreateCompatibleDC(DC);
  hOld := SelectObject(hdcCopy, hbmCopy);

  hdcBackMask := CreateCompatibleDC(DC);
  hdcForeMask := CreateCompatibleDC(DC);
  hdcMixBuffer := CreateCompatibleDC(DC);

  hbmBackMask := CreateBitmap(bitmap.Width, bitmap.Height, 1, 1, nil);
  hbmForeMask := CreateBitmap(bitmap.Width, bitmap.Height, 1, 1, nil);
  hbmMixBuffer := CreateCompatibleBitmap(DC, bitmap.Width, bitmap.Height);

  SelectObject(hdcBackMask, hbmBackMask);
  SelectObject(hdcForeMask, hbmForeMask);
  SelectObject(hdcMixBuffer, hbmMixBuffer);

  OldColor := SetBkColor(hdcCopy, transcolor); //clWhite);

  BitBlt(hdcForeMask, 0, 0, bitmap.Width, bitmap.Height, hdcCopy, 0, 0, SRCCOPY);

  SetBkColor(hdcCopy, OldColor);

  BitBlt(hdcBackMask, 0, 0, bitmap.Width, bitmap.Height, hdcForeMask, 0, 0, NOTSRCCOPY);

  BitBlt(hdcMixBuffer, 0, 0, bitmap.Width, bitmap.Height, DC, X, Y, SRCCOPY);

  BitBlt(hdcMixBuffer, 0, 0, bitmap.Width, bitmap.Height, hdcForeMask, 0, 0, SRCAND);

  BitBlt(hdcCopy, 0, 0, bitmap.Width, bitmap.Height, hdcBackMask, 0, 0, SRCAND);

  BitBlt(hdcMixBuffer, 0, 0, bitmap.Width, bitmap.Height, hdcCopy, 0, 0, SRCPAINT);

  BitBlt(DC, X, Y, bitmap.Width, bitmap.Height, hdcMixBuffer, 0, 0, SRCCOPY);

  {DeleteObject (hbmCopy);}
  DeleteObject(SelectObject(hdcCopy, hOld));
  DeleteObject(SelectObject(hdcForeMask, hOld));
  DeleteObject(SelectObject(hdcBackMask, hOld));
  DeleteObject(SelectObject(hdcMixBuffer, hOld));

  DeleteDC(hdcCopy);
  DeleteDC(hdcForeMask);
  DeleteDC(hdcBackMask);
  DeleteDC(hdcMixBuffer);
end;

function TagCount(Source: string; tag: Char): Integer;
var
  i, tCount                 : Integer;
begin
  tCount := 0;
  for i := 1 to Length(Source) do
    if Source[i] = tag then
      Inc(tCount);
  Result := tCount;
end;

{ "xxxxxx" => xxxxxx }

function TakeOffTag(Src: string; tag: Char; var rstr: string): string;
var
  //  i,
  N2                        : Integer;
begin
  N2 := Pos(tag, Copy(Src, 2, Length(Src)));
  rstr := Copy(Src, 2, N2 - 1);
  Result := Copy(Src, N2 + 2, Length(Src) - N2);
end;

function CatchString(Source: string; cap: Char; var catched: string): string;
var
  n                         : Integer;
begin
  Result := '';
  catched := '';
  if Source = '' then
    Exit;
  if Length(Source) < 2 then begin
    Result := Source;
    Exit;
  end;
  if Source[1] = cap then begin
    if Source[2] = cap then             //##abc#
      Source := Copy(Source, 2, Length(Source));
    if TagCount(Source, cap) >= 2 then begin
      Result := TakeOffTag(Source, cap, catched);
    end
    else
      Result := Source;
  end
  else begin
    if TagCount(Source, cap) >= 2 then begin
      n := Pos(cap, Source);
      Source := Copy(Source, n, Length(Source));
      Result := TakeOffTag(Source, cap, catched);
    end
    else
      Result := Source;
  end;
end;

{ GetValidStr3客 崔府 侥喊磊啊 楷加栏肺 唱棵版快 贸府 救凳 }
{ 侥喊磊啊 绝阑 版快, nil 府畔.. }

function DivString(Source: string; cap: Char; var sel: string): string;
var
  n                         : Integer;
begin
  if Source = '' then begin
    sel := '';
    Result := '';
    Exit;
  end;
  n := Pos(cap, Source);
  if n > 0 then begin
    sel := Copy(Source, 1, n - 1);
    Result := Copy(Source, n + 1, Length(Source));
  end
  else begin
    sel := Source;
    Result := '';
  end;
end;

function DivTailString(Source: string; cap: Char; var sel: string): string;
var
  i, n                      : Integer;
begin
  if Source = '' then begin
    sel := '';
    Result := '';
    Exit;
  end;
  n := 0;
  for i := Length(Source) downto 1 do
    if Source[i] = cap then begin
      n := i;
      Break;
    end;
  if n > 0 then begin
    sel := Copy(Source, n + 1, Length(Source));
    Result := Copy(Source, 1, n - 1);
  end
  else begin
    sel := '';
    Result := Source;
  end;
end;

function SPos(substr, Str: string): Integer;
var
  i, J, Len, slen           : Integer;
  flag                      : Boolean;
begin
  Result := -1;
  Len := Length(Str);
  slen := Length(substr);
  for i := 0 to Len - slen do begin
    flag := True;
    for J := 1 to slen do begin
      if Byte(Str[i + J]) >= $B0 then begin
        if (J < slen) and (i + J < Len) then begin
          if substr[J] <> Str[i + J] then begin
            flag := False;
            Break;
          end;
          if substr[J + 1] <> Str[i + J + 1] then begin
            flag := False;
            Break;
          end;
        end
        else
          flag := False;
      end
      else if substr[J] <> Str[i + J] then begin
        flag := False;
        Break;
      end;
    end;
    if flag then begin
      Result := i + 1;
      Break;
    end;
  end;
end;

function NumCopy(Str: string): Integer;
var
  i                         : Integer;
  data                      : string;
begin
  data := '';
  for i := 1 to Length(Str) do begin
    if (Word('0') <= Word(Str[i])) and (Word('9') >= Word(Str[i])) then begin
      data := data + Str[i];
    end
    else
      Break;
  end;
  Result := Str_ToInt(data, 0);
end;

function GetMonDay: string;
var
  year, mon, day            : Word;
  Str                       : string;
begin
  DecodeDate(Date, year, mon, day);
  Str := IntToStr(year);
  if mon < 10 then
    Str := Str + '0' + IntToStr(mon)
  else
    Str := IntToStr(mon);
  if day < 10 then
    Str := Str + '0' + IntToStr(day)
  else
    Str := IntToStr(day);
  Result := Str;
end;
function BoolToInt(boo: Boolean): Integer;
begin
  if boo then Result := 1
  else Result := 0;
end;
function BoolToStr(boo: Boolean): string;
begin
  if boo then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function HBoolToStr(b: Boolean): string;
const
  cSimpleBoolStrs           : array[Boolean] of string = ('0', '1');
begin
  Result := cSimpleBoolStrs[b];
end;

function StrToBool(Str: string): Boolean;
begin
  Result := False;
  if CompareStr(Str, 'FALSE') = 0 then
    Result := False
  else
    if CompareStr(Str, 'TRUE') = 0 then
      Result := True;
end;

function BoolToCStr(boo: Boolean): string;
begin
  if boo then
    Result := '是'
  else
    Result := '否';
end;

function IntToSex(Int: Integer): string;
begin
  case Int of                           //
    0: Result := '男';
    1: Result := '女';
  else begin
      Result := '??';
    end;
  end;
end;

function IntToJob(Int: Integer): string;
begin
  case Int of                           //
    0: Result := '武士';
    1: Result := '法师';
    2: Result := '道士';
  else begin
      Result := '???';
    end;
  end;
end;

function BoolToIntStr(boo: Boolean): string;
begin
  if boo then
    Result := '1'
  else
    Result := '0';
end;

function _MIN(N1, N2: Integer): Integer;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function _MAX(N1, N2: Integer): Integer;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function _MAX1(N1, N2: Integer): Integer;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
  if Result > 65535 then
    Result := 65535;
end;

function _LMIN(N1: DWORD; N2: UInt64): DWORD;
begin
  if N1 < N2 then
    Result := N1
  else begin
    //if N2 > High(Integer) then
    //  Result := N1
    //else
    Result := N2;
  end;
end;

//取得二个日期之间相差天数

function GetDayCount(MaxDate, MinDate: TDateTime): Integer;
begin
  //Result := DaysBetween(MaxDate, MinDate);
  Result := Trunc(MaxDate - MinDate);
end;

function CalcDay(day: TDateTime; plus: Integer): TDateTime;
var
  ayear, amon, aday         : Word;
begin
  if plus > 0 then begin
    plus := plus - 1;
    DecodeDate(day, ayear, amon, aday);
    while True do begin
      if aday + plus > MonthDays[False][amon] then begin
        plus := aday + plus - MonthDays[False][amon] - 1;
        aday := 1;
        if amon <= 11 then Inc(amon)
        else begin
          amon := 1;
          if ayear = 99 then ayear := 2000
          else Inc(ayear);
        end;
      end else begin
        aday := aday + plus;
        Break;
      end;
    end;
    Result := EncodeDate(ayear, amon, aday);
  end else
    Result := day;
end;

function GetCodeMsgSize(X: Double): Integer;
begin
  if Int(X) < X then
    Result := Trunc(X) + 1
  else
    Result := Trunc(X)
end;

function NotGetCodeMsgSize(X: Double): Integer;
begin
  Result := Trunc(X);
  if Frac(X) > 0 then
    Result := Result + 1;
end;

function pGetCodeMsgSize(X: Double): Integer;
{$IFDEF CPU64BITS}
 begin
 //备注1
 //因M2没有用到这个函数 64位未添加
{$else}
asm
  push ecx
  fld qword ptr [ebp+$08]
  add esp, $FFFFFFF4
  fstp tbyte ptr [esp]
  wait
  call NotGetCodeMsgSize
  mov dword ptr [ebp-$04], eax
  mov eax, dword ptr [ebp-$04]
  pop ecx
{$endif}
end;

function ConvertPercent(N1, N2: Integer): Integer;
begin
  if N2 = 0 then begin
    Result := 0;
    Exit;
  end;
  if N1 > N2 then begin
    Result := 100;
    Exit;
  end;
  Result := Round((N1 / N2) * 100);
end;

{function DoRound(Value: Extended): Int64;

  procedure Set8087CW(NewCW: Word);
  asm
    MOV Default8087CW,AX
    FNCLEX
    FLDCW Default8087CW
  end;

const
  RoundUpCW                 = $1B32;
var
  OldCW                     : Word;
begin
  OldCW := Default8087CW;
  try
    Set8087CW(RoundUpCW);
    Result := Round(Value);
  finally
    Set8087CW(OldCW);
  end;
end;}

function JudgeOddEven(var bcs: Integer): Boolean; //  奇偶数判断
begin
  if (bcs <> 0) then begin
    if (bcs mod 2) = 0 then
      Result := True                    //偶数
    else
      Result := False;
  end else
    Result := True;
end;

function IsChinese(Str: string): Boolean;
var
  i                         : Integer;
begin
  Result := False;
  i := 1;
  while i <= Length(Str) do begin
    if Integer(Str[i]) > $A0 then begin
      Result := True;
      Inc(i);
    end;
    Inc(i);
  end;
end;

function DecStr(SourStr: string; X, Y: Integer): string;
var
  s1, s2                    : string;
begin
  Result := SourStr;
  if (X > 0) and (Y > 0) then begin
    s1 := Copy(SourStr, 1, X - 1);
    s2 := Copy(SourStr, Y + 1, Length(SourStr)); //放置剩余部分
    Result := s1 + s2;
  end;
end;

function GetSystemFolderDir(mFolder: Integer): string;
var
  vItemIDList               : PItemIDList;
  vBuffer                   : array[0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, mFolder, vItemIDList);
  SHGetPathFromIDList(vItemIDList, vBuffer); //转换成文件系统的路径
  Result := vBuffer;
end;

{procedure FillCharSafe(out Dest; count: Integer; Value: Char);
begin
  FillChar(Dest, count, Value);
end;}

procedure DisPoseAndNil(var Obj);
var
  temp                      : Pointer;
begin
  temp := Pointer(Obj);
  Pointer(Obj) := nil;
  Dispose(temp);
end;

function MyGetProcAddress(hModule: hModule; lpProcName: LPCSTR): FARPROC;
var
  DataDirectory             : TImageDataDirectory;
  p1                        : ^Cardinal;
  p2                        : ^Word;
  Base, NumberOfNames, AddressOfFunctions, AddressOfNames, AddressOfNameOrdinals, i, Ordinal: Cardinal;
  TempStr1, TempStr2        : string;
begin
  Result := nil;
  DataDirectory := PImageNtHeaders(Cardinal(hModule) + Cardinal(PImageDosHeader(hModule)^._lfanew))^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  p1 := Pointer(hModule + DataDirectory.VirtualAddress + 16);
  Base := p1^;                          //输出函数的起始序号。一般为1。
  p1 := Pointer(hModule + DataDirectory.VirtualAddress + 24);
  NumberOfNames := p1^;                 //输出函数名的指针的数组中的元素个数，也是输出函数名对应的序号的数组中的元素个数。
  p1 := Pointer(hModule + DataDirectory.VirtualAddress + 28);
  AddressOfFunctions := p1^;            //一个RVA，指向输出函数入口地址的数组。
  p1 := Pointer(hModule + DataDirectory.VirtualAddress + 32);
  AddressOfNames := p1^;                //一个RVA，指向输出函数名的指针的数组。
  p1 := Pointer(hModule + DataDirectory.VirtualAddress + 36);
  AddressOfNameOrdinals := p1^;         //一个RVA，指向输出函数名对应的序号的数组。
  Ordinal := 0;
  if Cardinal(lpProcName) > $0000FFFF then begin
    //lpProcName参数指向函数名
    TempStr1 := PChar(lpProcName);      //要找的函数名
    for i := 1 to NumberOfNames do begin
      //按顺序在输出函数名中找
      p1 := Pointer(hModule + AddressOfNames + (i - 1) * 4);
      TempStr2 := PChar(hModule + p1^); //当前输出函数名
      if TempStr1 = TempStr2 then begin
        //找到输出函数
        p2 := Pointer(hModule + AddressOfNameOrdinals + (i - 1) * 2);
        //获得序号，不必减Base
        Ordinal := p2^;
        Break;
      end;
    end;
  end
  else
    //lpProcName传过来的是序号，需减Base
    Ordinal := Cardinal(lpProcName) - Base;
  p1 := Pointer(hModule + AddressOfFunctions + Ordinal * 4); //P1^为函数入口RVA
  if (p1^ >= DataDirectory.VirtualAddress) and (p1^ <= DataDirectory.VirtualAddress + DataDirectory.Size) then begin
    //！！！入口RVA在输出表中，指向另一DLL的某函数，这一点很容易被忽视，很少有教程提到，也许是没仔细看！！！
    TempStr1 := PChar(hModule + p1^);   //DLL.函数
    TempStr2 := TempStr1;
    while Pos('.', TempStr2) > 0 do
      TempStr2 := Copy(TempStr2, Pos('.', TempStr2) + 1, Length(TempStr2) - Pos('.', TempStr2));
    TempStr1 := Copy(TempStr1, 1, Length(TempStr1) - Length(TempStr2) - 1); //TempStr1是DLL名，TempStr2是函数名
    //递归调用获取新的函数地址
    Base := GetModuleHandle(PChar(TempStr1));
    if Base = 0 then
      Base := LoadLibrary(PChar(TempStr1));
    if Base > 0 then
      Result := MyGetProcAddress(Base, PAnsiChar(TempStr2));
  end
  else
    //RVA+基址就是函数的真实入口了
    Result := Pointer(hModule + p1^);
  //结果Result := GetProcAddress(hModule, lpProcName);
end;

function AnsiCaptureString(const Source: AnsiString; var rdstr: AnsiString): AnsiString;
var
  st, et, C, Len, i: Integer;
begin
  if Source = '' then
  begin
    rdstr := '';
    Result := '';
    Exit;
  end;
  C := 1;
  // et := 0;
  Len := Length(Source);
  while Source[C] = ' ' do
    if C < Len then
      Inc(C)
    else
      Break;

  if (Source[C] = '"') and (C < Len) then
  begin

    st := C + 1;
    et := Len;
    for i := C + 1 to Len do
      if Source[i] = '"' then
      begin
        et := i - 1;
        Break;
      end;

  end
  else
  begin
    st := C;
    et := Len;
    for i := C to Len do
      if Source[i] = ' ' then
      begin
        et := i - 1;
        Break;
      end;

  end;

  rdstr := Copy(Source, st, (et - st + 1));
  if Len >= (et + 2) then
    Result := Copy(Source, et + 2, Len - (et + 1))
  else
    Result := '';

end;

function DBAnsiGetValidStr2(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 181920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
label
  CATCH_DIV;
begin
  Ch := #0;
  try
    Result  :=  '';
    Dest    :=  '';
    srclen := Length(Str);
    BufCount := 0;
    Count := 1;

    if srclen >= BUF_SIZE - 1 then
    begin
      Result := '';
      Dest := '';
      Exit;
    end;

    if Str = '' then
    begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := Length(Divider);

    while True do
    begin
      if Count <= srclen then
      begin
        Ch := Str[Count];
        for i := 0 to ArrCount - 1 do
          if Ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if (Count > srclen) then
      begin
      CATCH_DIV:
        if (BufCount > 0) then
        begin
          if BufCount < BUF_SIZE - 1 then
          begin
            Result := StrUtils.MidStr(Str, Count + 1, srclen - Count);
            buf[BufCount] := #0;
            Dest := StrPas(buf);
            //Result := Copy(Str, Count + 1, srclen - Count);
          end;
          Break;
        end
        else
        begin
          if (Count > srclen) then
          begin
            Dest := '';
            Result := Copy(Str, Count + 2, srclen - 1);
            Break;
          end;
        end;
      end
      else
      begin
        if BufCount < BUF_SIZE - 1 then
        begin
          buf[BufCount] := Ch;
          Inc(BufCount);
        end;
      end;
      Inc(Count);
    end;
    Finalize(buf);
  except
    Dest := '';
    Result := '';
  end;
end;

function AnsiGetValidStr2(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
label
  CATCH_DIV;
begin
  Ch := #0;
  try
    Result  :=  '';
    Dest    :=  '';
    srclen := Length(Str);
    BufCount := 0;
    Count := 1;

    if srclen >= BUF_SIZE - 1 then
    begin
      Result := '';
      Dest := '';
      Exit;
    end;

    if Str = '' then
    begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := Length(Divider);

    while True do
    begin
      if Count <= srclen then
      begin
        Ch := Str[Count];
        for i := 0 to ArrCount - 1 do
          if Ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if (Count > srclen) then
      begin
      CATCH_DIV:
        if (BufCount > 0) then
        begin
          if BufCount < BUF_SIZE - 1 then
          begin
            Result := StrUtils.MidStr(Str, Count + 1, srclen - Count);
            buf[BufCount] := #0;
            Dest := StrPas(buf);
            //Result := Copy(Str, Count + 1, srclen - Count);
          end;
          Break;
        end
        else
        begin
          if (Count > srclen) then
          begin
            Dest := '';
            Result := Copy(Str, Count + 2, srclen - 1);
            Break;
          end;
        end;
      end
      else
      begin
        if BufCount < BUF_SIZE - 1 then
        begin
          buf[BufCount] := Ch;
          Inc(BufCount);
        end;
      end;
      Inc(Count);
    end;
    Finalize(buf);
  except
    Dest := '';
    Result := '';
  end;
end;

function DBAnsiGetValidStr3(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
label
  CATCH_DIV;
begin
  Ch := #0;
  try
    Result := '';
    Dest := '';
    srclen := Length(Str);
    BufCount := 0;
    Count := 1;

    if srclen >= BUF_SIZE - 1 then
    begin
      Result := '';
      Dest := '';
      Exit;
    end;

    if Str = '' then
    begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := Length(Divider);

    while True do
    begin
      if Count <= srclen then
      begin
        Ch := Str[Count];
        for i := 0 to ArrCount - 1 do
          if Ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if (Count > srclen) then
      begin
      CATCH_DIV:
        if (BufCount > 0) then
        begin
          if BufCount < BUF_SIZE - 1 then
          begin
            Result := StrUtils.AnsiMidStr(Str, Count + 1, srclen - Count);
            buf[BufCount] := #0;
            Dest := StrPas(buf);
          end;
          Break;
        end
        else
        begin
          if (Count > srclen) then
          begin
            Dest := '';
            Result := Copy(Str, Count + 2, srclen - 1);
            Break;
          end;
        end;
      end
      else
      begin
        if BufCount < BUF_SIZE - 1 then
        begin
          buf[BufCount] := Ch;
          Inc(BufCount);
        end;
      end;
      Inc(Count);
    end;
    Finalize(buf);
  except
    Dest := '';
    Result := '';
  end;
end;

function AnsiGetValidStr3(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
label
  CATCH_DIV;
begin
  Ch := #0;
  try
    Result := '';
    Dest := '';
    srclen := Length(Str);
    BufCount := 0;
    Count := 1;

    if srclen >= BUF_SIZE - 1 then
    begin
      Result := '';
      Dest := '';
      Exit;
    end;

    if Str = '' then
    begin
      Dest := '';
      Result := Str;
      Exit;
    end;
    ArrCount := Length(Divider);

    while True do
    begin
      if Count <= srclen then
      begin
        Ch := Str[Count];
        for i := 0 to ArrCount - 1 do
          if Ch = Divider[i] then
            goto CATCH_DIV;
      end;
      if (Count > srclen) then
      begin
      CATCH_DIV:
        if (BufCount > 0) then
        begin
          if BufCount < BUF_SIZE - 1 then
          begin
            Result := StrUtils.AnsiMidStr(Str, Count + 1, srclen - Count);
            buf[BufCount] := #0;
            Dest := StrPas(buf);
          end;
          Break;
        end
        else
        begin
          if (Count > srclen) then
          begin
            Dest := '';
            Result := Copy(Str, Count + 2, srclen - 1);
            Break;
          end;
        end;
      end
      else
      begin
        if BufCount < BUF_SIZE - 1 then
        begin
          buf[BufCount] := Ch;
          Inc(BufCount);
        end;
      end;
      Inc(Count);
    end;
    Finalize(buf);
  except
    Dest := '';
    Result := '';
  end;
end;

function AnsiGetValidStr4(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
label
  CATCH_DIV;
begin
  Ch := #0;
  srclen := Length(Str);
  BufCount := 0;
  Count := 1;
  if Str = '' then
  begin
    Dest := '';
    Result := Str;
    Exit;
  end;
  ArrCount := SizeOf(Divider) div SizeOf(AnsiChar);

  while True do
  begin
    if Count <= srclen then
    begin
      Ch := Str[Count];
      for i := 0 to ArrCount - 1 do
        if Ch = Divider[i] then
          goto CATCH_DIV;
    end;
    if (Count > srclen) then
    begin
    CATCH_DIV:
      if (BufCount > 0) or (Ch <> ' ') then
      begin
        if BufCount <= 0 then
        begin
          buf[0] := Ch;
          buf[1] := #0;
          Ch := ' ';
        end
        else
          buf[BufCount] := #0;
        Dest := AnsiString(buf);
        if Ch <> ' ' then
          Result := Copy(Str, Count, srclen - Count + 1) // remain divider in rest-string,
        else
          Result := Copy(Str, Count + 1, srclen - Count); // exclude whitespace
        Break;
      end
      else
      begin
        if (Count > srclen) then
        begin
          Dest := '';
          Result := Copy(Str, Count + 2, srclen - 1);
          Break;
        end;
      end;
    end
    else
    begin
      if BufCount < BUF_SIZE - 1 then
      begin
        buf[BufCount] := Ch;
        Inc(BufCount);
      end
      else
        Raise Exception.Create('BUF_SIZE overflow !');
    end;
    Inc(Count);
  end;
end;

function AnsiGetValidStrVal(const Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, Count, srclen, i, ArrCount: LongInt;
  Ch: AnsiChar;
  currentNumeric: Boolean;
  hexmode: Boolean;
label
  CATCH_DIV;
begin
  Ch := #0;
  hexmode := FALSE;
  srclen := Length(Str);
  BufCount := 0;
  Count := 1;
  currentNumeric := FALSE;

  if Str = '' then
  begin
    Dest := '';
    Result := Str;
    Exit;
  end;
  ArrCount := SizeOf(Divider) div SizeOf(AnsiChar);

  while True do
  begin
    if Count <= srclen then
    begin
      Ch := Str[Count];
      for i := 0 to ArrCount - 1 do
        if Ch = Divider[i] then
          goto CATCH_DIV;
    end;
    if not currentNumeric then
    begin
      if (Count + 1) < srclen then
      begin
        if (Str[Count] = '0') and (UpCase(Str[Count + 1]) = 'X') then
        begin
          buf[BufCount] := Str[Count];
          buf[BufCount + 1] := Str[Count + 1];
          Inc(BufCount, 2);
          Inc(Count, 2);
          hexmode := True;
          currentNumeric := True;
          Continue;
        end;
        if (Ch = '-') and (Str[Count + 1] >= '0') and (Str[Count + 1] <= '9') then
        begin
          currentNumeric := True;
        end;
      end;
      if (Ch >= '0') and (Ch <= '9') then
      begin
        currentNumeric := True;
      end;
    end
    else
    begin
      if hexmode then
      begin
        if not(((Ch >= '0') and (Ch <= '9')) or ((Ch >= 'A') and (Ch <= 'F')) or ((Ch >= 'a') and (Ch <= 'f'))) then
        begin
          Dec(Count);
          goto CATCH_DIV;
        end;
      end
      else if ((Ch < '0') or (Ch > '9')) and (Ch <> '.') then
      begin
        Dec(Count);
        goto CATCH_DIV;
      end;
    end;
    if (Count > srclen) then
    begin
    CATCH_DIV:
      if (BufCount > 0) then
      begin
        buf[BufCount] := #0;
        Dest := AnsiString(buf);
        Result := Copy(Str, Count + 1, srclen - Count);
        Break;
      end
      else
      begin
        if (Count > srclen) then
        begin
          Dest := '';
          Result := Copy(Str, Count + 2, srclen - 1);
          Break;
        end;
      end;
    end
    else
    begin
      if BufCount < BUF_SIZE - 1 then
      begin
        buf[BufCount] := Ch;
        Inc(BufCount);
      end
      else
        Raise Exception.Create('BUF_SIZE overflow !');
    end;
    Inc(Count);
  end;
end;

function AnsiGetValidStrCap(Str: AnsiString; var Dest: AnsiString; const Divider: array of AnsiChar): AnsiString;
begin
  Str := TrimLeft(Str);
  if Str <> '' then
  begin
    if Str[1] = '"' then
      Result := AnsiCaptureString(Str, Dest)
    else
    begin
      Result := AnsiGetValidStr3(Str, Dest, Divider);
    end;
  end
  else
  begin
    Result := '';
    Dest := '';
  end;
end;

function AnsiIsInB(const Src: AnsiString; Pos: Integer; const targ: AnsiString): Boolean;
var
  tLen, i: Integer;
begin
  Result := FALSE;
  tLen := Length(targ);
  if Length(Src) < Pos + tLen then
    Exit;
  for i := 0 to tLen - 1 do
    if UpCase(Src[Pos + i]) <> UpCase(targ[i + 1]) then
      Exit;

  Result := True;
end;

function AnsiArrestString(const Source, SearchAfter, ArrestBefore: AnsiString; const DropTags: array of AnsiString; var RsltStr: AnsiString): AnsiString;
const
  BUF_SIZE = 81920;
var
  buf: array [0 .. BUF_SIZE] of AnsiChar;
  BufCount, SrcCount, srclen, DropCount, i: Integer;
  ArrestNow: Boolean;
begin
  try
    RsltStr := ''; { result AnsiString }
    srclen := Length(Source);

    if srclen > BUF_SIZE then
    begin
      Result := '';
      Exit;
    end;

    BufCount := 0;
    SrcCount := 1;
    ArrestNow := FALSE;
    DropCount := SizeOf(DropTags) div SizeOf(AnsiString);

    if (SearchAfter = '') then
      ArrestNow := True;

    // GetMem (Buf, BUF_SIZE);

    while True do
    begin
      if SrcCount > srclen then
        Break;

      if not ArrestNow then
      begin
        if AnsiIsInB(Source, SrcCount, SearchAfter) then
          ArrestNow := True;
      end
      else
      begin
        buf[BufCount] := Source[SrcCount];
        if AnsiIsInB(Source, SrcCount, ArrestBefore) or (BufCount >= BUF_SIZE - 2) then
        begin
          BufCount := BufCount - Length(ArrestBefore);
          buf[BufCount + 1] := #0;
          RsltStr := AnsiString(buf);
          BufCount := 0;
          Break;
        end;

        for i := 0 to DropCount - 1 do
        begin
          if AnsiIsInB(Source, SrcCount, DropTags[i]) then
          begin
            BufCount := BufCount - Length(DropTags[i]);
            Break;
          end;
        end;

        Inc(BufCount);
      end;
      Inc(SrcCount);
    end;

    if (ArrestNow) and (BufCount <> 0) then
    begin
      buf[BufCount] := #0;
      RsltStr := AnsiString(buf);
    end;

    Result := Copy(Source, SrcCount + 1, srclen - SrcCount); { result is remain AnsiString }
  finally
    // LeaveCriticalSection (CSUtilLock);
  end;
end;

// 截取字符串
// 例 AnsiArrestStringEx('[1234]','[',']',str)    str=1234
function AnsiArrestStringEx(Source: AnsiString; const SearchAfter, ArrestBefore: AnsiString; var ArrestStr: AnsiString): AnsiString;
var
  { BufCount, SrcCount, } srclen: Integer;
  GoodData { , fin } : Boolean;
  i, N: Integer;
begin
  ArrestStr := ''; { result AnsiString }
  if Source = '' then
  begin
    Result := '';
    Exit;
  end;

  try
    srclen := Length(Source);
    GoodData := FALSE;
    if srclen >= 2 then
      if Source[1] = SearchAfter then
      begin
        Source := Copy(Source, 2, srclen - 1);
        srclen := Length(Source);
        GoodData := True;
      end
      else
      begin
        N := Pos(SearchAfter, Source);
        if N > 0 then
        begin
          Source := Copy(Source, N + 1, srclen - (N));
          srclen := Length(Source);
          GoodData := True;
        end;
      end;
    // fin := False;
    if GoodData then
    begin
      N := Pos(ArrestBefore, Source);
      if N > 0 then
      begin
        ArrestStr := Copy(Source, 1, N - 1);
        Result := Copy(Source, N + 1, srclen - N);
      end
      else
      begin
        Result := SearchAfter + Source;
      end;
    end
    else
    begin
      for i := 1 to srclen do
      begin
        if Source[i] = SearchAfter then
        begin
          Result := Copy(Source, i, srclen - i + 1);
          Break;
        end;
      end;
    end;
  except
    ArrestStr := '';
    Result := '';
  end;
end;

function AnsiDivString(const Source: AnsiString; cap: AnsiChar; var sel: AnsiString): AnsiString;
var
  N: Integer;
begin
  if Source = '' then
  begin
    sel := '';
    Result := '';
    Exit;
  end;
  N := Pos(cap, Source);
  if N > 0 then
  begin
    sel := Copy(Source, 1, N - 1);
    Result := Copy(Source, N + 1, Length(Source));
  end
  else
  begin
    sel := Source;
    Result := '';
  end;
end;

function AnsiDivTailString(const Source: AnsiString; cap: AnsiChar; var sel: AnsiString): AnsiString;
var
  i, N: Integer;
begin
  if Source = '' then
  begin
    sel := '';
    Result := '';
    Exit;
  end;
  N := 0;
  for i := Length(Source) downto 1 do
    if Source[i] = cap then
    begin
      N := i;
      Break;
    end;
  if N > 0 then
  begin
    sel := Copy(Source, N + 1, Length(Source));
    Result := Copy(Source, 1, N - 1);
  end
  else
  begin
    sel := '';
    Result := Source;
  end;
end;

function AnsiTagCount(const Source: AnsiString; Tag: AnsiChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Source) do
    if Source[I] = Tag then
      Inc(Result);
end;

function AnsiTextLength(const S: String): Integer;
var
  AnsiText : AnsiString;
begin
  AnsiText := S;
  Result := Length(AnsiText);
end;

function CopyAnisText(const S: string; index, Length: Integer): string;
var
  AnsiText : AnsiString;
begin
  AnsiText := S;
  AnsiText := Copy(AnsiText , Index , Length);
  Result := AnsiText;
end;

end.

