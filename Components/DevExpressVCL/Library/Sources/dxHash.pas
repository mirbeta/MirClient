{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxHash;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, dxCrypto, dxCryptoAPI,
{$ENDIF}
  SysUtils, Classes, Generics.Defaults, Generics.Collections;

type
  TdxMD4Byte16 = array[0..15] of Byte;
  TdxMD5Byte16 = TdxMD4Byte16;
  TdxMD5Byte64 = array[0..63] of Byte;
  PdxMD5Byte64 = ^TdxMD5Byte64;

  TdxMD5Int2 = array[0..1] of Cardinal;
  TdxMD5Int4 = array[0..3] of Cardinal;
  TdxMD5Int16 = array[0..15] of Cardinal;
  PdxMD5Int16 = ^TdxMD5Int16;

  TdxMD5Context = record
    State: TdxMD5Int4;
    Count: TdxMD5Int2;
    Buffer: TdxMD5Byte64;
    BufferLen: Cardinal;
  end;

type

  { TdxHashAlgorithm }

  TdxHashAlgorithmClass = class of TdxHashAlgorithm;
  TdxHashAlgorithm = class abstract
  protected
    function GetHashSize: Integer; virtual; abstract;
  public
    constructor Create; virtual;
    procedure Add(const A: TBytes); overload;
    procedure Add(const A: TBytes; AIndex, ALength: Integer); overload;
    procedure Add(const P: Pointer; ASize: Integer); overload; virtual; abstract;
    procedure Add(const S: string; AEncoding: TEncoding = nil); overload;
    function GetHash: TBytes; virtual; abstract;
    procedure Reset; virtual; abstract;

    class function Calculate(const A: TBytes): TBytes; overload;
    class function Calculate(const A: TBytes; AIndex, ALength: Integer): TBytes; overload;
    class function Calculate(const P: Pointer; ASize: Integer): TBytes; overload;
    class function Calculate(const S: string): TBytes; overload;

    class function ToBigEndian(AValue: Cardinal): Cardinal; overload; static; inline;
    class function ToBigEndian(AValue: UInt64): UInt64; overload; static; inline;

    property HashSize: Integer read GetHashSize;
  end;

{$IFDEF MSWINDOWS}
  { TdxCustomCryptoHashAlgorithm }

  TdxCustomCryptoHashAlgorithm = class abstract(TdxHashAlgorithm)
  protected
    FHandle: HCRYPTHASH;
    FProvider: IdxCryptoProvider;

    procedure Initialize(AHashAlgorithm: Integer; AProvider: IdxCryptoProvider = nil); virtual;
    function GetHashSize: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const P: Pointer; ASize: Integer); override;
    function GetHash: TBytes; override;
    procedure Reset; override;
    //
    property Handle: HCRYPTHASH read FHandle;
    property Provider: IdxCryptoProvider read FProvider;
  end;

  { TdxCryptoHashAlgorithm }

  TdxCryptoHashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  public
    constructor Create(AHashAlgorithm: Integer; AProvider: IdxCryptoProvider = nil); reintroduce;
  end;

  { TdxHMACHashAlgorithm }

  TdxHMACHashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  strict private
    FHashAlgorithm: Integer;
    FKeyHandle: HCRYPTKEY;

    procedure CreateHashHandle;
  public
    constructor Create(AKey: TBytes; AHashAlgorithm: Integer; AProvider: IdxCryptoProvider = nil); reintroduce;
    destructor Destroy; override;
    procedure Reset; override;
  end;

  { TdxMD2HashAlgorithm }

  TdxMD2HashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  public
    constructor Create; override;
  end;

  { TdxMD4HashAlgorithm }

  TdxMD4HashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  public
    constructor Create; override;
  end;

  { TdxMD5HashAlgorithm }

  TdxMD5HashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  public
    constructor Create; override;
  end;

  { TdxSHA1HashAlgorithm }

  TdxSHA1HashAlgorithm = class(TdxCustomCryptoHashAlgorithm)
  public
    constructor Create; override;
  end;

  { TdxSHA2HashAlgorithm }

  TdxSHA2HashAlgorithm = class abstract(TdxHashAlgorithm)
  strict protected
    FBuffer: array [0..127] of Byte;
    FBufferIndex: Integer;
    FBitLength: UInt64;
    FFinalized: Boolean;

    procedure ClearVariables; inline;
    function GetBlockSize: Integer; virtual; abstract;
    function GetHashBytes: TBytes; virtual; abstract;
    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;
    procedure ProcessBlock; virtual; abstract;
  public
    constructor Create; override;
    procedure Add(const AData: Pointer; ASize: Integer); override;
    function GetHash: TBytes; override;
    procedure Reset; override;
  end;

  { TdxSHA2HashAlgorithm32Bit }

  TdxSHA2HashAlgorithm32Bit = class abstract (TdxSHA2HashAlgorithm)
  strict protected const
    BlockSize = 64;
  strict protected
    FHash: array[0..7] of Cardinal;

    function GetBlockSize: Integer; override;
    function GetHashBytes: TBytes; override;
    procedure Finalize; override;
    procedure ProcessBlock; override;
  end;

  { TdxSHA256HashAlgorithm }

  TdxSHA256HashAlgorithm = class(TdxSHA2HashAlgorithm32Bit)
  strict protected
    function GetHashSize: Integer; override;
    procedure Initialize; override;
  end;

  { TdxSHA2HashAlgorithm64Bit }

  TdxSHA2HashAlgorithm64Bit = class abstract (TdxSHA2HashAlgorithm)
  strict protected const
    BlockSize = 128;
  strict protected
    FHash: array[0..7] of UInt64;

    function GetBlockSize: Integer; override;
    function GetHashBytes: TBytes; override;
    procedure Finalize; override;
    procedure ProcessBlock; override;
  end;

  { TdxSHA384HashAlgorithm }

  TdxSHA384HashAlgorithm = class(TdxSHA2HashAlgorithm64Bit)
  strict protected
    function GetHashSize: Integer; override;
    procedure Initialize; override;
  end;

  { TdxSHA512HashAlgorithm }

  TdxSHA512HashAlgorithm = class(TdxSHA2HashAlgorithm64Bit)
  strict protected
    function GetHashSize: Integer; override;
    procedure Initialize; override;
  end;
{$ENDIF}
  { TdxStringHash }

  TdxStringHash = class
  public
    class function BobJenkins(const S: string): Cardinal; static;
    class function DotNet(const S: string): Cardinal; static;
{$IFDEF MSWINDOWS}
    class function Elf(const S: string): Cardinal; static;
{$ENDIF}
    class function Murmur2(const S: string): Cardinal; static;
    class function Murmur2A(const S: string): Cardinal; static;
  end;

function dxCRC32(AData: PByte; ACount: Integer): Cardinal; overload;
function dxCRC32(AData: PByte; ACount: Integer; ACurrentCRC32: Cardinal): Cardinal; overload;
function dxCRC32(AStream: TStream; const APosition: Int64; ACount: Int64): Cardinal; overload;

function dxBobJenkinsHash(const Data; Len, InitData: Integer): Integer; inline;
function dxDotNetHash(Data: PByte; Len: Cardinal): Integer;

function dxElfHash(const S: AnsiString; ALangID: Cardinal = CP_ACP): Integer; overload;
function dxElfHash(const S: string; ALangID: Cardinal = CP_ACP): Integer; overload;
function dxElfHash(P: PWideChar; ALength: Integer; ALangID: Cardinal = CP_ACP): Integer; overload;
function dxElfHash(P: PWideChar; ALength: Integer; AUpperCaseBuffer: PWideChar;
  AUpperCaseBufferLength: Integer; ALangID: Cardinal = CP_ACP): Integer; overload;

function dxMD5CalcStr(const S: string): string;
function dxMD5Compare(const ADigits1, ADigits2: TdxMD5Byte16): Boolean;
function dxMD5DigestToString(const ADigits: TdxMD5Byte16): string;
procedure dxMD5Calc(AInput: PByteArray; AInputLength: Integer; var ADigits: TdxMD5Byte16);
procedure dxMD5Final(var AContext: TdxMD5Context; var ADigits: TdxMD5Byte16);
procedure dxMD5Init(var AContext: TdxMD5Context);
procedure dxMD5Update(var AContext: TdxMD5Context; AInput: PByteArray; AInputLength: Integer); overload;
procedure dxMD5Update(var AContext: TdxMD5Context; const S: AnsiString); overload;

procedure dxMD4Calc(AInput: PByteArray; AInputLength: Integer; var ADigits: TdxMD4Byte16);
function dxMurmur2(const S: string; const ASeed: Cardinal = 0): Cardinal;
function dxMurmur2A(const S: string; const ASeed: Cardinal = 0): Cardinal;
function dxAdler32(AInitialValue: Cardinal; ABuffer: Pointer; ASize: Integer): Cardinal;

implementation

uses
{$IFNDEF MSWINDOWS}
  Character,
{$ENDIF}
  Math, dxCore;

const
  sErrorHashAlgorithmNotDefined = 'Hash algorithm was not defined';
  sErrorHashFinalized = 'Hash is already finalized';

const
  CRCTable: array[0..255] of Cardinal =
   ($00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

type
  TdxMDTransform = procedure(var AAccumulator: TdxMD5Int4; const ABuf: PdxMD5Int16);

const
  FFTable: array[0..15] of LongWord = (
    $d76aa478, $e8c7b756, $242070db, $c1bdceee,
    $f57c0faf, $4787c62a, $a8304613, $fd469501,
    $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
    $6b901122, $fd987193, $a679438e, $49b40821
  );

  GGTable: array[0..15] of LongWord = (
    $f61e2562, $c040b340, $265e5a51, $e9b6c7aa,
    $d62f105d, $02441453, $d8a1e681, $e7d3fbc8,
    $21e1cde6, $c33707d6, $f4d50d87, $455a14ed,
    $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a
  );

  HHTable: array[0..15] of LongWord = (
    $fffa3942, $8771f681, $6d9d6122, $fde5380c,
    $a4beea44, $4bdecfa9, $f6bb4b60, $bebfbc70,
    $289b7ec6, $eaa127fa, $d4ef3085, $04881d05,
    $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665
  );

  IITable: array[0..15] of LongWord = (
    $f4292244, $432aff97, $ab9423a7, $fc93a039,
    $655b59c3, $8f0ccc92, $ffeff47d, $85845dd1,
    $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
    $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391
  );

function ROL(X, N: LongWord): LongWord; inline;
begin
  Result := (X shl N) or (X shr (32 - N));
end;

function FF(A, B, C, D, X, S, AC: LongWord): LongWord; inline;
begin
  Result := ROL(A + X + AC + (B and C or not B and D), S) + B;
end;

function GG(A, B, C, D, X, S, AC: LongWord): LongWord; inline;
begin
  Result := ROL(A + X + AC + (B and D or C and not D), S) + B;
end;

function HH(A, B, C, D, X, S, AC: LongWord): LongWord; inline;
begin
  Result := ROL(A + X + AC + (B xor C xor D), S) + B;
end;

function II(A, B, C, D, X, S, AC: LongWord): LongWord; inline;
begin
  Result := ROL(A + X + AC + (C xor (B or not D)), S) + B;
end;

procedure dxMDInit(var AContext: TdxMD5Context);
begin
{$IFDEF MSWINDOWS}
  ZeroMemory(@AContext, SizeOf(AContext));
{$ELSE}
  FillChar(AContext, SizeOf(AContext), 0);
{$ENDIF}
  AContext.State[0] := $67452301;
  AContext.State[1] := $EFCDAB89;
  AContext.State[2] := $98BADCFE;
  AContext.State[3] := $10325476;
end;

procedure dxMDHashUpdate(var AContext: TdxMD5Context; AInput: PByteArray;
  AInputLength: Integer; ATransformProc: TdxMDTransform);
const
  SplitPartSize = 64;
var
  AIndex: Integer;
  ALeft: Integer;
begin
  AIndex := 0;
  if Integer(AInputLength shl 3) < 0 then
    Inc(AContext.Count[1]);

  Inc(AContext.Count[0], AInputLength shl 3);
  Inc(AContext.Count[1], AInputLength shr 29);

  if AContext.BufferLen > 0 then
  begin
    ALeft := Min(AInputLength, SplitPartSize - AContext.BufferLen);
    Move(AInput^[AIndex], AContext.Buffer[AContext.BufferLen], ALeft);
    Inc(AContext.BufferLen, ALeft);
    Inc(AIndex, ALeft);
    if AContext.BufferLen < SplitPartSize then
      Exit;
    ATransformProc(AContext.State, @AContext.Buffer[0]);
    AContext.BufferLen := 0;
  end;

  while AIndex + SplitPartSize <= AInputLength do
  begin
    ATransformProc(AContext.State, @AInput^[AIndex]);
    Inc(AIndex, SplitPartSize);
  end;

  if AInputLength > AIndex then
  begin
    AContext.BufferLen := AInputLength - AIndex;
    Move(AInput^[AIndex], AContext.Buffer[0], AContext.BufferLen);
  end;
end;

procedure dxMDHashFinal(var AContext: TdxMD5Context; var ADigits: TdxMD5Byte16; ATransformProc: TdxMDTransform);
var
  AWorkBuf: TdxMD5Byte64;
  AWorkLen: Cardinal;
begin
  ADigits := TdxMD5Byte16(AContext.State);
  Move(AContext.Buffer, AWorkBuf, AContext.BufferLen);
  AWorkBuf[AContext.BufferLen] := $80;
  AWorkLen := AContext.BufferLen + 1;
  if AWorkLen > 56 then
  begin
    FillChar(AWorkBuf[AWorkLen], 64 - AWorkLen, 0);
    ATransformProc(TdxMD5Int4(ADigits), @AWorkBuf[0]);
    AWorkLen := 0
  end;
  FillChar(AWorkBuf[AWorkLen], 56 - AWorkLen, 0);
  TdxMD5Int16(AWorkBuf)[14] := AContext.Count[0];
  TdxMD5Int16(AWorkBuf)[15] := AContext.Count[1];
  ATransformProc(TdxMD5Int4(ADigits), @AWorkBuf[0]);
  FillChar(AContext, SizeOf(AContext), 0);
end;

procedure dxMD5Init(var AContext: TdxMD5Context);
begin
  dxMDInit(AContext); // MD5 based on MD4 algorithm
end;

procedure dxMD5Transform(var AAccumulator: TdxMD5Int4; const ABuf: PdxMD5Int16);

  function CalcPos(ARow, AOffset: Integer): Integer;
  begin
    Result := ARow * 4 + AOffset;
    if Result < 0 then
      Inc(Result, 16);
    if Result >= 16 then
      Dec(Result, 16);
  end;

var
  A, B, C, D: LongWord;
  I: Integer;
begin
  A := AAccumulator[0];
  B := AAccumulator[1];
  C := AAccumulator[2];
  D := AAccumulator[3];

  for I := 0 to 3 do
  begin
    A := FF(A, B, C, D, ABuf^[CalcPos(I, 0)],  7, FFTable[4 * I + 0]);
    D := FF(D, A, B, C, ABuf^[CalcPos(I, 1)], 12, FFTable[4 * I + 1]);
    C := FF(C, D, A, B, ABuf^[CalcPos(I, 2)], 17, FFTable[4 * I + 2]);
    B := FF(B, C, D, A, ABuf^[CalcPos(I, 3)], 22, FFTable[4 * I + 3]);
  end;

  for I := 0 to 3 do
  begin
    A := GG(A, B, C, D, ABuf^[CalcPos(I, 1)],   5, GGTable[4 * I + 0]);
    D := GG(D, A, B, C, ABuf^[CalcPos(I, 6)],   9, GGTable[4 * I + 1]);
    C := GG(C, D, A, B, ABuf^[CalcPos(I, 11)], 14, GGTable[4 * I + 2]);
    B := GG(B, C, D, A, ABuf^[CalcPos(I, 0)],  20, GGTable[4 * I + 3]);
  end;

  for I := 0 to 3 do
  begin
    A := HH(A, B, C, D, ABuf^[CalcPos(-I, 5)],   4, HHTable[4 * I + 0]);
    D := HH(D, A, B, C, ABuf^[CalcPos(-I, 8)],  11, HHTable[4 * I + 1]);
    C := HH(C, D, A, B, ABuf^[CalcPos(-I, 11)], 16, HHTable[4 * I + 2]);
    B := HH(B, C, D, A, ABuf^[CalcPos(-I, 14)], 23, HHTable[4 * I + 3]);
  end;

  for I := 0 to 3 do
  begin
    A := II(A, B, C, D, ABuf^[CalcPos(-I, 0)],   6, IITable[4 * I + 0]);
    D := II(D, A, B, C, ABuf^[CalcPos(-I, 7)],  10, IITable[4 * I + 1]);
    C := II(C, D, A, B, ABuf^[CalcPos(-I, 14)], 15, IITable[4 * I + 2]);
    B := II(B, C, D, A, ABuf^[CalcPos(-I, 5)],  21, IITable[4 * I + 3]);
  end;

  Inc(AAccumulator[0], A);
  Inc(AAccumulator[1], B);
  Inc(AAccumulator[2], C);
  Inc(AAccumulator[3], D);
end;

procedure dxMD5Update(var AContext: TdxMD5Context; const S: AnsiString);
begin
  dxMD5Update(AContext, @S[1], Length(S));
end;

procedure dxMD5Update(var AContext: TdxMD5Context; AInput: PByteArray; AInputLength: Integer);
begin
  dxMDHashUpdate(AContext, AInput, AInputLength, dxMD5Transform);
end;

procedure dxMD5Final(var AContext: TdxMD5Context; var ADigits: TdxMD5Byte16);
begin
  dxMDHashFinal(AContext, ADigits, dxMD5Transform);
end;

procedure dxMD5Calc(AInput: PByteArray; AInputLength: Integer; var ADigits: TdxMD5Byte16);
var
  AContext: TdxMD5Context;
begin
  dxMD5Init(AContext);
  dxMD5Update(AContext, AInput, AInputLength);
  dxMD5Final(AContext, ADigits);
end;

function dxMD5CalcStr(const S: string): string;
var
  AAnsiString: AnsiString;
  ADigits: TdxMD5Byte16;
begin
{$IFDEF MSWINDOWS}
  AAnsiString := dxStringToAnsiString(S);
{$ELSE}
  AAnsiString := AnsiString(S);
{$ENDIF}
  dxMD5Calc(@AAnsiString[1], Length(AAnsiString), ADigits);
  Result := dxMD5DigestToString(ADigits);
end;

function dxMD5Compare(const ADigits1, ADigits2: TdxMD5Byte16): Boolean;
begin
  Result := CompareMem(@ADigits1[0], @ADigits2[0], SizeOf(TdxMD5Byte16));
end;

function dxMD5DigestToString(const ADigits: TdxMD5Byte16): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ADigits) do
    Result := Result + IntToHex(ADigits[I], 2);
end;

{  MD4 }

procedure dxMD4Transform(var AAccumulator: TdxMD5Int4; const ABuf: PdxMD5Int16);
var
  I: Integer;
  A, B, C, D: LongWord;
const
  R3Pos: array[0..3] of Byte = (0, 2, 1, 3);
begin
  A := AAccumulator[0];
  B := AAccumulator[1];
  C := AAccumulator[2];
  D := AAccumulator[3];

  // R1
  for i := 0 to 3 do
  begin
    A := ROL((((D xor C) and B) xor D) + A + ABuf^[0 + I * 4],  3);
    D := ROL((((C xor B) and A) xor C) + D + ABuf^[1 + I * 4],  7);
    C := ROL((((B xor A) and D) xor B) + C + ABuf^[2 + I * 4], 11);
    B := ROL((((A xor D) and C) xor A) + B + ABuf^[3 + I * 4], 19);
  end;

  // R2
  for i := 0 to 3 do
  begin
    A := ROL(((B and C) or (D and (B or C))) + A + ABuf^[0 * 4 + I] + $5A827999,  3);
    D := ROL(((A and B) or (C and (A or B))) + D + ABuf^[1 * 4 + I] + $5A827999,  5);
    C := ROL(((D and A) or (B and (D or A))) + C + ABuf^[2 * 4 + I] + $5A827999,  9);
    B := ROL(((C and D) or (A and (C or D))) + B + ABuf^[3 * 4 + I] + $5A827999, 13);
  end;

  // R3
  for i := 0 to 3 do
  begin
    A := ROL((B xor C xor D) + A + ABuf^[0 + R3Pos[I]] + $6ED9EBA1,  3);
    D := ROL((A xor B xor C) + D + ABuf^[8 + R3Pos[I]] + $6ED9EBA1,  9);
    C := ROL((D xor A xor B) + C + ABuf^[4 + R3Pos[I]] + $6ED9EBA1, 11);
    B := ROL((C xor D xor A) + B + ABuf^[12 + R3Pos[I]] + $6ED9EBA1, 15);
  end;

  Inc(AAccumulator[0], A);
  Inc(AAccumulator[1], B);
  Inc(AAccumulator[2], C);
  Inc(AAccumulator[3], D);
end;


procedure dxMD4Update(var AContext: TdxMD5Context; AInput: PByteArray; AInputLength: Integer);
begin
  dxMDHashUpdate(AContext, AInput, AInputLength, dxMD4Transform);
end;

procedure dxMD4Final(var AContext: TdxMD5Context; var ADigits: TdxMD4Byte16);
begin
  dxMDHashFinal(AContext, ADigits, dxMD4Transform);
end;

procedure dxMD4Calc(AInput: PByteArray; AInputLength: Integer; var ADigits: TdxMD4Byte16);
var
  AContext: TdxMD5Context;
begin
  dxMD5Init(AContext);
  dxMD4Update(AContext, AInput, AInputLength);
  dxMD4Final(AContext, ADigits);
end;

function dxCRC32(AData: PByte; ACount: Integer): Cardinal;
begin
  Result := dxCRC32(AData, ACount, $FFFFFFFF) xor $FFFFFFFF;
end;

function dxCRC32(AData: PByte; ACount: Integer; ACurrentCRC32: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := ACurrentCRC32;
  for I := 0 to ACount - 1 do
  begin
    Result := (Result shr 8) xor CRCTable[Byte(Result) xor AData^];
    Inc(AData);
  end;
end;

function dxCRC32(AStream: TStream; const APosition: Int64; ACount: Int64): Cardinal;
var
  ABuffer: PByte;
  ARead: Integer;
  ASavedPosition: Int64;
begin
  ASavedPosition := AStream.Position;
  try
    AStream.Position := APosition;
    ABuffer := AllocMem(MaxWord);
    try
      Result := $FFFFFFFF;
      repeat
        ARead := AStream.Read(ABuffer^, Min(MaxWord, ACount));
        Result := dxCRC32(ABuffer, ARead, Result);
        Dec(ACount, ARead);
      until ARead = 0;
      Result := Result xor $FFFFFFFF;
    finally
      FreeMem(ABuffer);
    end;
  finally
    AStream.Position := ASavedPosition;
  end;
end;

function dxBobJenkinsHash(const Data; Len, InitData: Integer): Integer; inline;
begin
{$IFDEF DELPHI22}
  Result := THashBobJenkins.GetHashValue(Data, Len, InitData);
{$ELSE}
  Result := BobJenkinsHash(Data, Len, InitData);
{$ENDIF}
end;

function dxDotNetHash(Data: PByte; Len: Cardinal): Integer;
var
  I: Cardinal;
begin
  Result := Len;
  for I := 1 to Len do
  begin
    Result := (Result shl 7) xor Data^;
    Inc(Data);
  end;

  Dec(Result, Result shr 17);
  Dec(Result, Result shr 11);
  Dec(Result, Result shr 5);
end;

function dxElfHash(const S: AnsiString; ALangID: Cardinal = CP_ACP): Integer;
begin
  Result := dxElfHash(dxAnsiStringToString(S, ALangID), ALangID);
end;

function dxElfHash(const S: string; ALangID: Cardinal = CP_ACP): Integer;
begin
  Result := dxElfHash(PWideChar(S), Length(S), ALangID);
end;

function dxElfHash(P: PWideChar; ALength: Integer; ALangID: Cardinal = CP_ACP): Integer;
var
  ATempBuffer: PWideChar;
begin
  ATempBuffer := AllocMem(ALength * SizeOf(WideChar));
  try
    Result := dxElfHash(P, ALength, ATempBuffer, ALength, ALangID);
  finally
    FreeMem(ATempBuffer);
  end;
end;

function dxElfHash(P: PWideChar; ALength: Integer; AUpperCaseBuffer: PWideChar;
  AUpperCaseBufferLength: Integer; ALangID: Cardinal = CP_ACP): Integer;
var
  AIndex, I: Integer;
{$IFNDEF MSWINDOWS}
  AUpperStr: string;
{$ENDIF}
begin
  Result := 0;
  if P <> nil then
  begin
    if ALength <= 0 then
      ALength := StrLen(P);

    if AUpperCaseBuffer <> nil then
    begin
{$IFDEF MSWINDOWS}
      ALength := LCMapStringW(ALangID, LCMAP_UPPERCASE, P, ALength, AUpperCaseBuffer, AUpperCaseBufferLength);
      P := AUpperCaseBuffer;
{$ELSE}
      AUpperStr := TCharacter.ToUpper(string(P));
      P := PWideChar(AUpperStr);
{$ENDIF}
    end;

    for I := 0 to ALength - 1 do
    begin
      Result := (Result shl 4) + Ord(P^);
      AIndex := Result and $F0000000;
      if AIndex <> 0 then
        Result := Result xor (AIndex shr 24);
      Result := Result and (not AIndex);
      Inc(P);
    end;
  end;
end;

const
  Murmur2_M = $5BD1E995;
  Murmur2_R = 24;

function dxMurmur2(const S: string; const ASeed: Cardinal = 0): Cardinal;
var
  AIndex, ALength, K: Cardinal;
  P: PByte;
begin
  ALength := Length(S);
  Result := ASeed xor ALength;

  ALength := ALength * SizeOf(Char);
  P := PByte(@S[1]);

  AIndex := 0;
  while ALength >= 4 do
  begin
    K := PCardinal(@P[AIndex])^;
    K := K * Murmur2_M;
    K := K xor (K shr Murmur2_R);
    K := K * Murmur2_M;
    Result := Result * Murmur2_M;
    Result := Result xor K;
    Inc(AIndex, 4);
    Dec(ALength, 4);
  end;

  if ALength = 3 then
    Result := Result xor (P[AIndex + 2] shl 16);
  if ALength >= 2 then
    Result := Result xor (P[AIndex + 1] shl 8);
  if ALength >= 1 then
  begin
    Result := Result xor P[AIndex];
    Result := Result * Murmur2_M;
  end;
  Result := Result xor (Result shr 13);
  Result := Result * Murmur2_M;
  Result := Result xor (Result shr 15);
end;

procedure mmix(var H, K: Cardinal); inline;
{ K *= M; K ^= K >> r; K *= M; H *= M; H ^= K; }
begin
  K := K * Murmur2_M;
  K := K xor (K shr Murmur2_R);
  K := K * Murmur2_M;
  H := H * Murmur2_M;
  H := H xor K;
end;

function dxMurmur2A(const S: string; const ASeed: Cardinal = 0): Cardinal;
var
  AIndex, ALength, L, K, T: Cardinal;
  P: PByte;
begin
  Result := ASeed;

  ALength := Length(S);
  L := ALength * SizeOf(Char);
  P := PByte(S);
  if P = nil then
    Exit;

  AIndex := 0;
  while L >= 4 do
  begin
    K := PCardinal(@P[AIndex])^;
    mmix(Result, K);
    K := K * Murmur2_M;
    K := K xor (K shr Murmur2_R);
    K := K * Murmur2_M;
    Inc(AIndex, 4);
    Dec(L, 4);
  end;

  if L = 3 then
    T := P[AIndex + 2] shl 16
  else
    T := 0;
  if L >= 2 then
    T := T xor (P[AIndex + 1] shl 8);
  if L >= 1 then
    T := T xor P[AIndex];

  mmix(Result, T);
  mmix(Result, ALength);

  Result := Result xor (Result shr 13);
  Result := Result * Murmur2_M;
  Result := Result xor (Result shr 15);
end;

{ TdxHashAlgorithm }

constructor TdxHashAlgorithm.Create;
begin
  // do nothing
end;

procedure TdxHashAlgorithm.Add(const A: TBytes; AIndex, ALength: Integer);
begin
  if (AIndex < 0) or (AIndex >= ALength) then
    raise ERangeError.Create('AIndex');
  if (ALength < 0) or (AIndex + ALength > Length(A)) then
    raise ERangeError.Create('ALength');
  Add(@A[AIndex], ALength);
end;

procedure TdxHashAlgorithm.Add(const A: TBytes);
var
  P: Pointer;
  L: Integer;
begin
  L := Length(A);
  if L = 0 then
    P := nil
  else
    P := Pointer(A);

  Add(P, L);
end;

procedure TdxHashAlgorithm.Add(const S: string; AEncoding: TEncoding = nil);
begin
  if AEncoding = nil then
    AEncoding := TEncoding.UTF8;
  Add(AEncoding.GetBytes(S));
end;

class function TdxHashAlgorithm.Calculate(const P: Pointer; ASize: Integer): TBytes;
begin
  with Create do
  try
    Add(P, ASize);
    Result := GetHash;
  finally
    Free;
  end;
end;

class function TdxHashAlgorithm.Calculate(const A: TBytes): TBytes;
var
  P: Pointer;
  L: Integer;
begin
  L := Length(A);
  if L = 0 then
    P := nil
  else
    P := Pointer(A);

  Result := Calculate(P, L);
end;

class function TdxHashAlgorithm.Calculate(const A: TBytes; AIndex, ALength: Integer): TBytes;
begin
  if (AIndex < 0) or (AIndex >= ALength) then
    raise ERangeError.Create('AIndex');
  if (ALength < 0) or (AIndex + ALength > Length(A)) then
    raise ERangeError.Create('ALength');
  Result := Calculate(@A[AIndex], ALength);
end;

class function TdxHashAlgorithm.Calculate(const S: string): TBytes;
begin
  Result := Calculate(TEncoding.UTF8.GetBytes(S));
end;

class function TdxHashAlgorithm.ToBigEndian(AValue: UInt64): UInt64;
begin
  Result := UInt64(ToBigEndian(Cardinal(AValue))) shl 32 or ToBigEndian(Cardinal(AValue shr 32));
end;

class function TdxHashAlgorithm.ToBigEndian(AValue: Cardinal): Cardinal;
begin
  Result := (AValue shr 24) or (AValue shl 24) or ((AValue shr 8) and $FF00) or ((AValue shl 8) and $FF0000);
end;

{$IFDEF MSWINDOWS}
{ TdxCustomCryptoHashAlgorithm }

constructor TdxCustomCryptoHashAlgorithm.Create;
begin
  raise Exception.Create(sErrorHashAlgorithmNotDefined);
end;

destructor TdxCustomCryptoHashAlgorithm.Destroy;
begin
  CryptCheck(CryptDestroyHash(FHandle));
  FProvider := nil;
  inherited Destroy;
end;

procedure TdxCustomCryptoHashAlgorithm.Add(const P: Pointer; ASize: Integer);
begin
  CryptCheck(CryptHashData(FHandle, P, ASize, 0));
end;

function TdxCustomCryptoHashAlgorithm.GetHash: TBytes;
var
  AValue, ALength: Cardinal;
begin
  ALength := SizeOf(AValue);
  CryptCheck(CryptGetHashParam(Handle, HP_HASHSIZE, @AValue, ALength, 0));

  ALength := AValue;
  SetLength(Result, ALength);
  CryptCheck(CryptGetHashParam(Handle, HP_HASHVAL, @Result[0], ALength, 0));
end;

procedure TdxCustomCryptoHashAlgorithm.Reset;
var
  AHashAlgorithm, ALength: Cardinal;
begin
  ALength := SizeOf(AHashAlgorithm);
  CryptCheck(CryptGetHashParam(Handle, HP_ALGID, @AHashAlgorithm, ALength, 0));
  CryptCheck(CryptDestroyHash(FHandle));
  CryptCheck(CryptCreateHash(FProvider.GetHandle, AHashAlgorithm, 0, 0, FHandle));
end;

procedure TdxCustomCryptoHashAlgorithm.Initialize(AHashAlgorithm: Integer; AProvider: IdxCryptoProvider);
begin
  if AProvider <> nil then
    FProvider := AProvider
  else
    FProvider := TdxCryptoProvider.Create(nil, PROV_RSA_FULL);

  CryptCheck(CryptCreateHash(FProvider.GetHandle, AHashAlgorithm, 0, 0, FHandle));
end;

function TdxCustomCryptoHashAlgorithm.GetHashSize: Integer;
var
  ASize: Cardinal;
begin
  ASize := SizeOf(Result);
  CryptCheck(CryptGetHashParam(Handle, HP_HASHSIZE, @Result, ASize, 0));
end;

{ TdxCryptoHashAlgorithm }

constructor TdxCryptoHashAlgorithm.Create(AHashAlgorithm: Integer; AProvider: IdxCryptoProvider = nil);
begin
  Initialize(AHashAlgorithm, AProvider);
end;

{ TdxHMACHashAlgorithm }

constructor TdxHMACHashAlgorithm.Create(AKey: TBytes; AHashAlgorithm: Integer; AProvider: IdxCryptoProvider = nil);
begin
  FHashAlgorithm := AHashAlgorithm;
  Initialize(AHashAlgorithm, AProvider);
  Add(AKey);
  CryptCheck(CryptDeriveKey(Provider.Handle, CALG_RC4, Handle, 0, FKeyHandle));
  CryptCheck(CryptDestroyHash(Handle));
  CreateHashHandle;
end;

destructor TdxHMACHashAlgorithm.Destroy;
begin
  CryptCheck(CryptDestroyKey(FKeyHandle));
  inherited Destroy;
end;

procedure TdxHMACHashAlgorithm.Reset;
begin
  CryptCheck(CryptDestroyHash(Handle));
  CreateHashHandle;
end;

procedure TdxHMACHashAlgorithm.CreateHashHandle;
var
  AInfo: TdxHMACInfo;
begin
  ZeroMemory(@AInfo, SizeOf(AInfo));
  AInfo.HashAlgid := FHashAlgorithm;
  CryptCheck(CryptCreateHash(Provider.Handle, CALG_HMAC, FKeyHandle, 0, FHandle));
  CryptCheck(CryptSetHashParam(Handle, HP_HMAC_INFO, @AInfo, 0));
end;

{ TdxMD2HashAlgorithm }

constructor TdxMD2HashAlgorithm.Create;
begin
  Initialize(CALG_MD2);
end;

{ TdxMD4HashAlgorithm }

constructor TdxMD4HashAlgorithm.Create;
begin
  Initialize(CALG_MD4);
end;

{ TdxMD5HashAlgorithm }

constructor TdxMD5HashAlgorithm.Create;
begin
  Initialize(CALG_MD5);
end;

{ TdxSHA1HashAlgorithm }

constructor TdxSHA1HashAlgorithm.Create;
begin
  Initialize(CALG_SHA1);
end;

{ TdxSHA2HashAlgorithm }

constructor TdxSHA2HashAlgorithm.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TdxSHA2HashAlgorithm.Add(const AData: Pointer; ASize: Integer);
var
  P: PByte;
  I, ACount, ABufferLength, ARemainderSize: Integer;
begin
  if FFinalized then
    raise Exception.Create(sErrorHashFinalized);

  P := AData;
  ABufferLength := GetBlockSize;
  Inc(FBitLength, ASize * 8);
  ACount := (ASize + FBufferIndex) div ABufferLength;
  if ACount > 0  then
  begin
    ARemainderSize := ABufferLength - FBufferIndex;
    Move(P^, FBuffer[FBufferIndex], ARemainderSize);
    Inc(P, ARemainderSize);
    Dec(ASize, ARemainderSize);
    ProcessBlock;
    for I := 1 to ACount - 1 do
    begin
      Move(P^, FBuffer[0], ABufferLength);
      Inc(P, ABufferLength);
      Dec(ASize, ABufferLength);
      ProcessBlock;
    end;
    FBufferIndex := 0;
  end;
  Move(P^, FBuffer[FBufferIndex], ASize);
  Inc(FBufferIndex, ASize);
end;

function TdxSHA2HashAlgorithm.GetHash: TBytes;
begin
  if not FFinalized then
  begin
    FFinalized := True;
    Finalize;
  end;
  Result := GetHashBytes;
end;

procedure TdxSHA2HashAlgorithm.Reset;
begin
  FFinalized := False;
  Initialize;
end;

procedure TdxSHA2HashAlgorithm.ClearVariables;
begin
  FBitLength := 0;
  FBufferIndex := 0;
  FillChar(FBuffer, SizeOf(FBuffer), 0);
end;

{ TdxSHA2HashAlgorithm32Bit }

procedure TdxSHA2HashAlgorithm32Bit.Finalize;
var
  I: Integer;
begin
  FBuffer[FBufferIndex] := $80;
  if FBufferIndex >= 56 then
  begin
    for I := FBufferIndex + 1 to BlockSize - 1 do
      FBuffer[I] := $00;
    ProcessBlock;
    FBufferIndex := 0;
  end
  else
    Inc(FBufferIndex);

  // No need to clean the bytes used for the BitLength
  FillChar(FBuffer[FBufferIndex], (BlockSize - 8) - FBufferIndex, 0);

  PCardinal(@FBuffer[56])^ := TdxHashAlgorithm.ToBigEndian(Cardinal(FBitLength shr 32));
  PCardinal(@FBuffer[60])^ := TdxHashAlgorithm.ToBigEndian(Cardinal(FBitLength));
  ProcessBlock;
  FHash[0] := TdxHashAlgorithm.ToBigEndian(FHash[0]);
  FHash[1] := TdxHashAlgorithm.ToBigEndian(FHash[1]);
  FHash[2] := TdxHashAlgorithm.ToBigEndian(FHash[2]);
  FHash[3] := TdxHashAlgorithm.ToBigEndian(FHash[3]);
  FHash[4] := TdxHashAlgorithm.ToBigEndian(FHash[4]);
  FHash[5] := TdxHashAlgorithm.ToBigEndian(FHash[5]);
  FHash[6] := TdxHashAlgorithm.ToBigEndian(FHash[6]);
  FHash[7] := TdxHashAlgorithm.ToBigEndian(FHash[7]);
end;

function TdxSHA2HashAlgorithm32Bit.GetBlockSize: Integer;
begin
  Result := BlockSize;
end;

function TdxSHA2HashAlgorithm32Bit.GetHashBytes: TBytes;
var
  AHashSize: Integer;
begin
  AHashSize := GetHashSize;
  SetLength(Result, AHashSize);
  Move(FHash, PByte(@Result[0])^, AHashSize);
end;

procedure TdxSHA2HashAlgorithm32Bit.ProcessBlock;
const
  K_256: array[0..63] of Cardinal = (
   $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
   $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
   $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
   $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
   $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
   $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
   $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
   $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);

  function Ch32(X, Y, Z: Cardinal): Cardinal; inline;
  begin
    Result := (X and Y) xor ((not X) and Z);
  end;

  function Maj32(X, Y, Z: Cardinal): Cardinal; inline;
  begin
    Result := (X and Y) xor (X and Z) xor (Y and Z);
  end;

  function ROR32(x: Cardinal; n: Byte): Cardinal; inline;
  begin
    Result := (x shl (32 - n) or (x shr n));
  end;

var
  I: Integer;
  S0, S1, T1, T2: Cardinal;
  W: array[0..63] of Cardinal;
  A, B, C, D, E, F, G, H: Cardinal;
begin
  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];
  E := FHash[4];
  F := FHash[5];
  G := FHash[6];
  H := FHash[7];

  Move(FBuffer, W, BlockSize);
  for I := 0 to 15 do
    W[I] := TdxHashAlgorithm.ToBigEndian(W[I]);

  for I := 16 to 63 do
  begin
    S0   := ROR32(W[I - 15],  7) xor ROR32(W[I - 15], 18) xor (W[I - 15] shr  3);
    S1   := ROR32(W[I -  2], 17) xor ROR32(W[I -  2], 19) xor (W[I -  2] shr 10);
    W[I] := W[I - 16] + S0 + W[I - 7] + S1;
  end;

  // Process the data to generate the change for the next intermediate hash
  for I := 0 to 63 do
  begin
    S0 := ROR32(A, 2) xor ROR32(A, 13) xor ROR32(A, 22);
    T2 := S0 + Maj32(A, B, C);
    S1 := ROR32(E, 6) xor ROR32(E, 11) xor ROR32(E, 25);

    T1 := H + S1 + Ch32(E, F, G) + K_256[I] + W[I];
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;
  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
  FHash[4] := FHash[4] + E;
  FHash[5] := FHash[5] + F;
  FHash[6] := FHash[6] + G;
  FHash[7] := FHash[7] + H;
end;


{ TdxSHA256HashAlgorithm }

function TdxSHA256HashAlgorithm.GetHashSize: Integer;
begin
  Result := 32;
end;

procedure TdxSHA256HashAlgorithm.Initialize;
begin
  ClearVariables;
  FHash[0]:= $6A09E667;
  FHash[1]:= $BB67AE85;
  FHash[2]:= $3C6EF372;
  FHash[3]:= $A54FF53A;
  FHash[4]:= $510E527F;
  FHash[5]:= $9B05688C;
  FHash[6]:= $1F83D9AB;
  FHash[7]:= $5BE0CD19;
end;

{ TdxSHA2HashAlgorithm64Bit }

procedure TdxSHA2HashAlgorithm64Bit.Finalize;
var
  I: Integer;
begin
  FBuffer[FBufferIndex] := $80;
  if FBufferIndex >= 112 then
  begin
    for I := FBufferIndex + 1 to BlockSize - 1 do
      FBuffer[I] := $00;
    ProcessBlock;
    FBufferIndex := 0;
  end
  else
    Inc(FBufferIndex);

  FillChar(FBuffer[FBufferIndex], (BlockSize - 16) - FBufferIndex, 0);

  PCardinal(@FBuffer[112])^ := 0;
  PCardinal(@FBuffer[116])^ := 0;
  PCardinal(@FBuffer[120])^ := TdxHashAlgorithm.ToBigEndian(Cardinal(FBitLength shr 32));
  PCardinal(@FBuffer[124])^ := TdxHashAlgorithm.ToBigEndian(Cardinal(FBitLength));
  ProcessBlock;
  FHash[0] := TdxHashAlgorithm.ToBigEndian(FHash[0]);
  FHash[1] := TdxHashAlgorithm.ToBigEndian(FHash[1]);
  FHash[2] := TdxHashAlgorithm.ToBigEndian(FHash[2]);
  FHash[3] := TdxHashAlgorithm.ToBigEndian(FHash[3]);
  FHash[4] := TdxHashAlgorithm.ToBigEndian(FHash[4]);
  FHash[5] := TdxHashAlgorithm.ToBigEndian(FHash[5]);
  FHash[6] := TdxHashAlgorithm.ToBigEndian(FHash[6]);
  FHash[7] := TdxHashAlgorithm.ToBigEndian(FHash[7]);
end;

function TdxSHA2HashAlgorithm64Bit.GetBlockSize: Integer;
begin
  Result := BlockSize;
end;

function TdxSHA2HashAlgorithm64Bit.GetHashBytes: TBytes;
var
  AHashSize: Integer;
begin
  AHashSize := GetHashSize;
  SetLength(Result, AHashSize);
  Move(FHash, PByte(@Result[0])^, AHashSize);
end;

procedure TdxSHA2HashAlgorithm64Bit.ProcessBlock;
const
  K_512: array[0..79] of UInt64 = (
    $428A2F98D728AE22, $7137449123EF65CD, $B5C0FBCFEC4D3B2F, $E9B5DBA58189DBBC,
    $3956C25BF348B538, $59F111F1B605D019, $923F82A4AF194F9B, $AB1C5ED5DA6D8118,
    $D807AA98A3030242, $12835B0145706FBE, $243185BE4EE4B28C, $550C7DC3D5FFB4E2,
    $72BE5D74F27B896F, $80DEB1FE3B1696B1, $9BDC06A725C71235, $C19BF174CF692694,
    $E49B69C19EF14AD2, $EFBE4786384F25E3, $0FC19DC68B8CD5B5, $240CA1CC77AC9C65,
    $2DE92C6F592B0275, $4A7484AA6EA6E483, $5CB0A9DCBD41FBD4, $76F988DA831153B5,
    $983E5152EE66DFAB, $A831C66D2DB43210, $B00327C898FB213F, $BF597FC7BEEF0EE4,
    $C6E00BF33DA88FC2, $D5A79147930AA725, $06CA6351E003826F, $142929670A0E6E70,
    $27B70A8546D22FFC, $2E1B21385C26C926, $4D2C6DFC5AC42AED, $53380D139D95B3DF,
    $650A73548BAF63DE, $766A0ABB3C77B2A8, $81C2C92E47EDAEE6, $92722C851482353B,
    $A2BFE8A14CF10364, $A81A664BBC423001, $C24B8B70D0F89791, $C76C51A30654BE30,
    $D192E819D6EF5218, $D69906245565A910, $F40E35855771202A, $106AA07032BBD1B8,
    $19A4C116B8D2D0C8, $1E376C085141AB53, $2748774CDF8EEB99, $34B0BCB5E19B48A8,
    $391C0CB3C5C95A63, $4ED8AA4AE3418ACB, $5B9CCA4F7763E373, $682E6FF3D6B2B8A3,
    $748F82EE5DEFB2FC, $78A5636F43172F60, $84C87814A1F0AB72, $8CC702081A6439EC,
    $90BEFFFA23631E28, $A4506CEBDE82BDE9, $BEF9A3F7B2C67915, $C67178F2E372532B,
    $CA273ECEEA26619C, $D186B8C721C0C207, $EADA7DD6CDE0EB1E, $F57D4F7FEE6ED178,
    $06F067AA72176FBA, $0A637DC5A2C898A6, $113F9804BEF90DAE, $1B710B35131C471B,
    $28DB77F523047D84, $32CAAB7B40C72493, $3C9EBE0A15C9BEBC, $431D67C49C100D4C,
    $4CC5D4BECB3E42B6, $597F299CFC657E2A, $5FCB6FAB3AD6FAEC, $6C44198C4A475817);

  function Ch64(X, Y, Z: UInt64): UInt64; inline;
  begin
    Result := (X and Y) xor ((not X) and Z);
  end;

  function Maj64(X, Y, Z: UInt64): UInt64; inline;
  begin
    Result := (X and Y) xor (X and Z) xor (Y and Z);
  end;

  function ROR64(X: UInt64; ACount: Byte): UInt64; inline;
  begin
    Result := (X shl (64 - ACount)) or (X shr ACount);
  end;

var
  I: Integer;
  W: array[0..79] of UInt64;
  A, B, C, D, E, F, G, H, S0, S1, T1, T2: UInt64;
begin
  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];
  E := FHash[4];
  F := FHash[5];
  G := FHash[6];
  H := FHash[7];

  Move(FBuffer, W, BlockSize);
  // Initialize message schedule
  for I := 0 to 15 do
    W[I] := TdxHashAlgorithm.ToBigEndian(W[I]);

  for I := 16 to 79 do
  begin
    S0   := ROR64(W[I - 15],  1) xor ROR64(W[I - 15],  8) xor (W[I - 15] shr 7);
    S1   := ROR64(W[I -  2], 19) xor ROR64(W[I -  2], 61) xor (W[I -  2] shr 6);
    W[I] := W[I - 16] + S0 + W[I - 7] + S1;
  end;

  // Process the data to generate the change for the next intermediate hash
  for I := 0 to 79 do
  begin
    S0 := ROR64(A, 28) xor ROR64(A, 34) xor ROR64(A, 39);
    T2 := S0 + Maj64(A, B, C);
    S1 := ROR64(E, 14) xor ROR64(E, 18) xor ROR64(E, 41);

    T1 := H + S1 + Ch64(E, F, G) + K_512[I] + W[I];
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;

  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
  FHash[4] := FHash[4] + E;
  FHash[5] := FHash[5] + F;
  FHash[6] := FHash[6] + G;
  FHash[7] := FHash[7] + H;
end;

{ TdxSHA384HashAlgorithm }

function TdxSHA384HashAlgorithm.GetHashSize: Integer;
begin
  Result := 48;
end;

procedure TdxSHA384HashAlgorithm.Initialize;
begin
  ClearVariables;
  FHash[0]:= $CBBB9D5DC1059ED8;
  FHash[1]:= $629A292A367CD507;
  FHash[2]:= $9159015A3070DD17;
  FHash[3]:= $152FECD8F70E5939;
  FHash[4]:= $67332667FFC00B31;
  FHash[5]:= $8EB44A8768581511;
  FHash[6]:= $DB0C2E0D64F98FA7;
  FHash[7]:= $47B5481DBEFA4FA4;
end;

{ TdxSHA512HashAlgorithm }

function TdxSHA512HashAlgorithm.GetHashSize: Integer;
begin
  Result := 64;
end;

procedure TdxSHA512HashAlgorithm.Initialize;
begin
  ClearVariables;
  FHash[0]:= $6A09E667F3BCC908;
  FHash[1]:= $BB67AE8584CAA73B;
  FHash[2]:= $3C6EF372FE94F82B;
  FHash[3]:= $A54FF53A5F1D36F1;
  FHash[4]:= $510E527FADE682D1;
  FHash[5]:= $9B05688C2B3E6C1F;
  FHash[6]:= $1F83D9ABFB41BD6B;
  FHash[7]:= $5BE0CD19137E2179;
end;
{$ENDIF}

{ TdxStringHash }

class function TdxStringHash.BobJenkins(const S: string): Cardinal;
begin
  Result := Cardinal(dxBobJenkinsHash(S[1], Length(S) * SizeOf(Char), 0));
end;

class function TdxStringHash.DotNet(const S: string): Cardinal;
begin
  Result := Cardinal(dxDotNetHash(@S[1], Length(S) * SizeOf(Char)));
end;

{$IFDEF MSWINDOWS}
class function TdxStringHash.Elf(const S: string): Cardinal;
begin
  Result := Cardinal(dxElfHash(PWideChar(S), Length(S), nil, 0));
end;
{$ENDIF}

class function TdxStringHash.Murmur2(const S: string): Cardinal;
begin
  Result := dxMurmur2(S, 0);
end;

class function TdxStringHash.Murmur2A(const S: string): Cardinal;
begin
  Result := dxMurmur2A(S, 0);
end;

function dxAdler32(AInitialValue: Cardinal; ABuffer: Pointer; ASize: Integer): Cardinal;
const
  BASE = 65521; // largest prime smaller than 65536
var
  P: PByte absolute ABuffer;
  L, H: Cardinal;
  I, ACount: integer;
begin
  L := LongRec(AInitialValue).Lo;
  H := LongRec(AInitialValue).Hi;
  while ASize > 0 do
  begin
    if ASize < 5552 then
      ACount := ASize
    else
      ACount := 5552;
    for I := 1 to ACount do
    begin
      Inc(L, P^);
      Inc(P, SizeOf(Cardinal));
      Inc(H,L);
    end;
    L := L mod BASE;
    H := H mod BASE;
    Dec(ASize, ACount);
  end;
  Result := Word(L) + Cardinal(Word(H)) shl 16;
end;

end.
