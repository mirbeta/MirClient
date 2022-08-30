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

unit dxCore;

{$I cxVer.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  {$IFDEF VCL}
    Graphics,
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Classes, SysUtils, Variants, Character, Contnrs, AnsiStrings,
  Generics.Defaults, Generics.Collections, Types;

const
  dxBuildNumber: Cardinal = 20190102;
  dxUnicodePrefix: Word = $FEFF;
  dxCRLF = #13#10;
  dxTAB  = #9;

  MinInt   = -2147483647 - 1;
  MinInt64 = -9223372036854775807 - 1;
  MaxInt64 = 9223372036854775807;

const
  dxUserNameUnknown            = 0;
  dxUserNameFullyQualifiedDN   = 1;
  dxUserNameSamCompatible      = 2;
  dxUserNameDisplay            = 3;
  dxUserNameUniqueId           = 6;
  dxUserNameCanonical          = 7;
  dxUserNameUserPrincipal      = 8;
  dxUserNameCanonicalEx        = 9;
  dxUserNameServicePrincipal   = 10;
  dxUserNameDnsDomain          = 12;
{$IFNDEF MSWINDOWS}
  MAXWORD = 65535;
{$ENDIF}

type
{$IFDEF DELPHIXE2}
  TColors = array of System.UITypes.TColor;
{$ELSE}
  {$IFDEF VCL}
  TColors = array of TColor;
  {$ENDIF}
{$ENDIF}
  TPoints = array of TPoint;
  TRects = array of TRect;
  PdxNativeInt = ^TdxNativeInt;
  TdxNativeInt = {$IFDEF DELPHI16}NativeInt{$ELSE}Longint{$ENDIF};
  PdxNativeUInt = ^TdxNativeUInt;
  TdxNativeUInt = {$IFDEF DELPHI16}NativeUInt{$ELSE}Cardinal{$ENDIF};

  TcxResourceStringID = Pointer;

  PRectArray = ^TRectArray;
  TRectArray = array [0..0] of TRect;

  TdxDefaultBoolean = (bFalse, bTrue, bDefault);
  TdxCorner = (coTopLeft, coTopRight, coBottomLeft, coBottomRight);
  TdxCorners = set of TdxCorner;
  TdxSortOrder = (soNone, soAscending, soDescending);

  TdxOrientation = (orHorizontal, orVertical);
  TdxDirectedOrientation = (doLeftToRight, doTopToBottom, doBottomToTop, doRightToLeft);
  TdxTextOrientation = (toLeftToRight, toTopToBottom, toBottomToTop);

  PdxPWideCharArray = ^TdxPWideCharArray;
  TdxPWideCharArray = array[0..0] of PWideChar;

  { IdxLocalizerListener }

  IdxLocalizerListener = interface
  ['{1DA7EAB4-4D89-43A3-98B4-FF63B9A10A0D}']
    procedure TranslationChanged;
  end;

  { IdxDebugVisualizer }

  IdxDebugVisualizer = interface
  ['{159F61D6-24F4-47B6-A635-9AD95A904780}']
    function GetDebugVisualizerData: string;
  end;

  TdxAnsiCharSet = set of AnsiChar;

  { TdxByteArray }

  TdxByteArray = class
  public
    class function Clone(const ABytes: TBytes): TBytes; overload;
    class function Clone(const ABytes: TBytes; AMaxLength: Integer): TBytes; overload;
    class function Compare(const AB1, AB2: TBytes): Boolean;
    class function Concatenate(ANum: Integer; const ABytes: TBytes): TBytes; overload;
    class function Concatenate(const AB1: TBytes; const AB2: TBytes): TBytes; overload;
    class function Concatenate(const ABytes: TBytes; ANum: Integer): TBytes; overload;
    class function Resize(const ABytes: TBytes; ATargetSize: Integer; AFillBy: Byte = 0): TBytes;
  end;

  { TdxStream }

  TdxStream = class(TStream)
  private
    FIsUnicode: Boolean;
    FStream: TStream;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AStream: TStream); virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property IsUnicode: Boolean read FIsUnicode;
    property Stream: TStream read FStream;
  end;

  { TdxProductResourceStrings }

  TdxProductResourceStrings = class;

  TdxAddResourceStringsProcedure = procedure(AProduct: TdxProductResourceStrings);

  TdxProductResourceStrings = class
  private
    FName: string;
    FInitializeProcedures: TList;
    FResStringNames: TStrings;

    function GetNames(AIndex: Integer): string;
    function GetResStringsCount: Integer;
    procedure SetTranslation(AIndex: Integer);
    function GetValues(AIndex: Integer): string;
  protected
    procedure AddProcedure(AProc: TdxAddResourceStringsProcedure);
    procedure RemoveProcedure(AProc: TdxAddResourceStringsProcedure);
    procedure Translate;

    property InitializeProcedures: TList read FInitializeProcedures;
  public
    constructor Create(const AName: string; AInitializeProc: TdxAddResourceStringsProcedure); virtual;
    destructor Destroy; override;
    procedure Add(const AResStringName: string; AResStringAddr: Pointer);
    procedure Clear;
    function GetIndexByName(const AName: string): Integer;

    property Name: string read FName;
    property Names[AIndex: Integer]: string read GetNames;
    property ResStringsCount: Integer read GetResStringsCount;
    property Values[AIndex: Integer]: string read GetValues;
  end;

  TdxLocalizationTranslateResStringEvent = procedure(const AResStringName: string; AResString: Pointer) of object;

  { TdxResourceStringsRepository }

  TdxResourceStringsRepository = class
  private
    FListeners: TList;
    FProducts: TObjectList;
    FOnTranslateResString: TdxLocalizationTranslateResStringEvent;

    function GetProducts(AIndex: Integer): TdxProductResourceStrings;
    function GetProductsCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddListener(AListener: IdxLocalizerListener);
    procedure RemoveListener(AListener: IdxLocalizerListener);
    procedure NotifyListeners;

    procedure RegisterProduct(const AProductName: string; AAddStringsProc: TdxAddResourceStringsProcedure);
    function GetProductIndexByName(const AName: string): Integer;
    function GetOriginalValue(const AName: string): string;
    procedure Translate;
    procedure UnRegisterProduct(const AProductName: string; AAddStringsProc: TdxAddResourceStringsProcedure = nil);

    property Products[Index: Integer]: TdxProductResourceStrings read GetProducts;
    property ProductsCount: Integer read GetProductsCount;
    property OnTranslateResString: TdxLocalizationTranslateResStringEvent read FOnTranslateResString write FOnTranslateResString;
  end;

  Safe<T: class> = class
  public
    class function Cast(AObject: TObject): T;
  end;

  { TdxUnitsLoader }

  TdxProc = procedure;

  TdxUnitsLoader = class
  protected
    FinalizeList: TList;
    InitializeList: TList;
    procedure CallProc(AProc: TdxProc);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddUnit(const AInitializeProc, AFinalizeProc: Pointer);
    procedure RemoveUnit(const AFinalizeProc: Pointer);
    procedure Finalize;
    procedure Initialize;
  end;

  EdxException = class(Exception);

  EdxTestException = class(EdxException);

procedure dxAbstractError;
procedure dxTestCheck(AValue: WordBool; const AMessage: string);
procedure dxCheckOrientation(var AValue: TdxOrientation; ADefaultOrientation: TdxOrientation);
procedure dxShowException(AException: TObject);

procedure dxCallNotify(ANotifyEvent: TNotifyEvent; ASender: TObject);
{$IFDEF MSWINDOWS}
function dxGetTickCount: Int64;
{$ENDIF}
function dxBooleanToDefaultBoolean(AValue: Boolean): TdxDefaultBoolean;
function dxDefaultBooleanToBoolean(AValue: TdxDefaultBoolean; ADefault: Boolean): Boolean;
function dxSameMethods(const Method1, Method2): Boolean; inline;

function dxMakeInt64(const A, B: Integer): Int64;

// math
function dxFMod(const ANumerator, ADenominator: Single): Single; overload;
function dxFMod(const ANumerator, ADenominator: Double): Double; overload;
function dxFMod(const ANumerator, ADenominator: Extended): Extended; overload;

// string functions
function dxBinToHex(const ABuffer: AnsiString): AnsiString; overload;
function dxBinToHex(const ABuffer: PAnsiChar; ABufSize: Integer): AnsiString; overload;
function dxHexToBin(const AText: AnsiString): AnsiString; overload;
function dxHexToBin(const AText: PAnsiChar): AnsiString; overload;
function dxHexToByte(const AHex: string): Byte;
function dxCharCount(const S: string): Integer; inline;
function dxCharInSet(C: Char; const ACharSet: TdxAnsiCharSet): Boolean; inline;
function dxStringSize(const S: string): Integer; inline;

{$IFDEF MSWINDOWS}
function dxAnsiIsAlpha(Ch: AnsiChar): Boolean;
function dxAnsiIsNumeric(Ch: AnsiChar): Boolean;
function dxCharIsAlpha(Ch: Char): Boolean;
function dxCharIsNumeric(Ch: Char): Boolean;
function dxWideIsAlpha(Ch: WideChar): Boolean;
function dxWideIsNumeric(Ch: WideChar): Boolean;
function dxWideIsSpace(Ch: WideChar): Boolean;
{$ENDIF}
function dxIsLowerCase(AChar: Char): Boolean; inline;
function dxIsWhiteSpace(AChar: Char): Boolean; inline;
function dxLowerCase(AChar: Char): Char; inline;

function dxStrComp(const Str1, Str2: PAnsiChar): Integer; inline;
function dxStrCopy(ADest: PAnsiChar; const ASource: PAnsiChar): PAnsiChar; inline;
function dxStrLCopy(ADest: PAnsiChar; const ASource: PAnsiChar; AMaxLen: Cardinal): PAnsiChar; inline;
function dxStrLen(const AStr: PAnsiChar): Cardinal; inline;

{$IFDEF MSWINDOWS}
function dxGetCodePageFromCharset(ACharset: Integer): Integer;
function dxGetStringTypeA(Locale: LCID; dwInfoType: DWORD; const lpSrcStr: PAnsiChar; cchSrc: Integer; var lpCharType): BOOL;
function dxGetStringTypeW(dwInfoType: DWORD; const lpSrcStr: PWideChar; cchSrc: Integer; var lpCharType): BOOL;
function dxGetAnsiCharCType1(Ch: AnsiChar): Word;
function dxGetWideCharCType1(Ch: WideChar): Word;
{$ENDIF}

// string conversions
function dxAnsiStringToWideString(const ASource: AnsiString; ACodePage: Cardinal = CP_ACP): WideString; inline;
function dxWideStringToAnsiString(const ASource: WideString; ACodePage: Cardinal = CP_ACP): AnsiString; inline;

function dxAnsiStringToString(const S: AnsiString; ACodePage: Integer = CP_ACP): string;
function dxStringToAnsiString(const S: string; ACodePage: Integer = CP_ACP): AnsiString;

function dxShortStringToString(const S: ShortString): string; inline;
function dxStringToShortString(const S: string): ShortString; inline;

function dxVariantToAnsiString(const V: Variant): AnsiString;
function dxVariantToString(const V: Variant): string;

function dxVarIsBlob(const V: Variant): Boolean;

function dxConcatenateStrings(const AStrings: array of PChar): string;
procedure dxStringToBytes(const S: string; var Buf);
function dxStrToFloat(const S: string; const ADecimalSeparator: Char = '.'): Extended;
function dxStrToFloatDef(const S: string; const ADecimalSeparator: Char = '.'; ADefaultValue: Extended = 0): Extended;
function dxFloatToStr(AValue: Extended; ADecimalSeparator: Char = '.'): string;
function RemoveAccelChars(const S: string; AAppendTerminatingUnderscore: Boolean = True): string;
{$IFNDEF DELPHIXE2}
function SplitString(const S, Delimiters: string): TStringDynArray;
{$ENDIF}

function dxAnsiStrToInt(const S: AnsiString): Integer;
function dxUTF8StringToAnsiString(const S: UTF8String): AnsiString;
function dxAnsiStringToUTF8String(const S: AnsiString): UTF8String;
function dxUTF8StringToString(const S: UTF8String): string;
function dxUTF8StringToWideString(const S: UTF8String): WideString;
function dxStringToUTF8String(const S: string): UTF8String;
function dxWideStringToUTF8String(const S: WideString): UTF8String;

function dxAnsiStringReplace(const AString, AOldPattern, ANewPattern: AnsiString; AFlags: TReplaceFlags): AnsiString;

// streaming
function dxIsUnicodeStream(AStream: TStream): Boolean;
procedure dxWriteStandardEncodingSignature(AStream: TStream);
procedure dxWriteStreamType(AStream: TStream);

function dxReadStr(Stream: TStream; AIsUnicode: Boolean): string;
procedure dxWriteStr(Stream: TStream; const S: string);

function dxStreamToVariant(AStream: TStream): Variant; overload;
function dxStreamToVariant(AStream: TStream; ASize: Integer): Variant; overload;

procedure dxCompressStream(ASourceStream, ADestStream: TStream; ACompressMethod: Byte; ASize: Integer = 0);
procedure dxDecompressStream(ASourceStream, ADestStream: TStream);

function dxResourceStringsRepository: TdxResourceStringsRepository;
function cxGetResourceString(AResString: TcxResourceStringID): string; overload;
procedure cxSetResourceString(AResString: TcxResourceStringID; const Value: string);
function cxGetResourceString(const AResString: string): string; overload; deprecated;
function cxGetResourceStringNet(const AResString: string): string; deprecated;
procedure cxSetResourceStringNet(const AResString, Value: string); deprecated;
procedure cxClearResourceStrings;

// file utils
{$IFDEF MSWINDOWS}
function dxCreateTempFile(const AExtension: string = ''): string;
{$ENDIF}
function dxExcludeTrailingPathDelimiter(const APath: string; const APathDelimiter: Char = PathDelim): string;
function dxExtractFileName(const APath: string; const APathDelimiter: Char = PathDelim): string;
function dxIncludeTrailingPathDelimiter(const APath: string; const APathDelimiter: Char = PathDelim): string;
function dxIsPathDelimiter(const S: string; Index: Integer; const APathDelimiter: Char = PathDelim): Boolean;
function dxReplacePathDelimiter(const APath: AnsiString; const AOldDelimiter, ANewDelimiter: AnsiChar): AnsiString; overload;
function dxReplacePathDelimiter(const APath: string; const AOldDelimiter, ANewDelimiter: Char): string; overload;

// memory functions
{$IFDEF MSWINDOWS}
procedure cxZeroMemory(ADestination: Pointer; ACount: Integer);
function cxAllocMem(Size: Cardinal): Pointer;
{$ENDIF}
procedure cxFreeMem(P: Pointer);
procedure dxFillChar(var ADest; Count: Integer; const APattern: Char);
procedure dxFreeAndNil(var AObject);

procedure cxCopyData(ASource, ADestination: Pointer; ACount: Integer); overload; inline;
procedure cxCopyData(ASource, ADestination: Pointer; ASourceOffSet, ADestinationOffSet, ACount: Integer); overload; inline;
function ReadBoolean(ASource: Pointer; AOffset: Integer = 0): WordBool;
function ReadByte(ASource: Pointer; AOffset: Integer = 0): Byte;
function ReadInteger(ASource: Pointer; AOffset: Integer = 0): Integer;
function ReadPointer(ASource: Pointer): Pointer;
function ReadWord(ASource: Pointer; AOffset: Integer = 0): Word;
procedure WriteBoolean(ADestination: Pointer; AValue: WordBool; AOffset: Integer = 0);
procedure WriteByte(ADestination: Pointer; AValue: Byte; AOffset: Integer = 0);
procedure WriteInteger(ADestination: Pointer; AValue: Integer; AOffset: Integer = 0);
procedure WritePointer(ADestination: Pointer; AValue: Pointer);
procedure WriteWord(ADestination: Pointer; AValue: Word; AOffset: Integer = 0);

function ReadBufferFromStream(AStream: TStream; ABuffer: Pointer; Count: Integer): Boolean;
function ReadStringFromStream(AStream: TStream; out AValue: AnsiString): Longint;
function WriteBufferToStream(AStream: TStream; ABuffer: Pointer; ACount: Longint): Longint;
function WriteCharToStream(AStream: TStream; AValue: AnsiChar): Longint;
function WriteDoubleToStream(AStream: TStream; AValue: Double): Longint;
function WriteIntegerToStream(AStream: TStream; AValue: Integer): Longint;
function WriteSmallIntToStream(AStream: TStream; AValue: SmallInt): Longint;
function WriteStringToStream(AStream: TStream; const AValue: AnsiString): Longint;

function dxSwap16(const AValue: Word): Word;
function dxSwap32(const AValue: DWORD): DWORD;
procedure ExchangeLongWords(var AValue1, AValue2);
procedure ExchangePointers(var AValue1, AValue2);
function ShiftPointer(P: Pointer; AOffset: Integer): Pointer; inline;
{$IFDEF MSWINDOWS}
function dxPointToLParam(P: TPoint): LPARAM;
function dxLParamToPoint(AParam: LPARAM): TPoint;
{$ENDIF}

// List functions
procedure dxAppendList(ASource, ADestination: TList);
procedure dxCopyList(ASource, ADestination: TList);

{$IFDEF MSWINDOWS}
// locale functions
function dxGetInvariantLocaleID: Integer;
procedure dxGetLocaleFormatSettings(ALocale: Integer; var AFormatSettings: TFormatSettings);
function dxGetLocaleInfo(ALocale, ALocaleType: Integer; const ADefaultValue: string = ''): string;
function dxGetSubLangID(ALangId: Word): Word;
function dxMakeLangID(APrimaryLang, ASubLang: Word): Word;
function dxMakeLCID(ALangId, ASortId: Word): DWORD;

function dxGetUserName: string;
function dxGetUserNameEx(AFormat: DWORD): string;
function dxShellExecute(AHandle: HWND; const AFileName: string; AShowCmd: Integer = SW_SHOWNORMAL): Boolean; overload;
function dxShellExecute(const AFileName: string; AShowCmd: Integer = SW_SHOWNORMAL): Boolean; overload;
{$ENDIF}

//compare functions
function dxCompareValues(A, B: Integer): Integer; overload; inline;
function dxCompareValues(A, B: Pointer): Integer; overload; inline;
function dxSameText(const A, B: string): Boolean; inline;

function dxGetBuildNumberAsString(ABuildNumber: Cardinal = 0): string;
function dxGetShortBuildNumberAsString(ABuildNumber: Cardinal = 0): string;
procedure dxFactorizeBuildNumber(ABuildNumber: Cardinal; out AMajor, AMinor, ABuild: Integer);

function dxGenerateGUID: string;
function dxGenerateID: string;
function dxIsDLL: Boolean;

function dxUnitsLoader: TdxUnitsLoader;
procedure dxInitialize; stdcall;
procedure dxFinalize; stdcall;


{$IFDEF MSWINDOWS}
var
// platform info
  IsWin9X: Boolean;
  IsWin95, IsWin98, IsWinMe: Boolean;

  IsWinNT: Boolean;
  IsWin2K, IsWin2KOrLater: Boolean;
  IsWinXP, IsWinXPOrLater: Boolean;
  IsWin2KOrXP: Boolean;

  IsWinVista, IsWinVistaOrLater: Boolean;
  IsWinSeven, IsWinSevenOrLater: Boolean;
  IsWin8, IsWin8OrLater: Boolean;

  IsWin10: Boolean;
  IsWin10v1809OrNewer: Boolean;
  IsWin10FallCreatorsUpdate: Boolean;
  IsWin10OrLater: Boolean;

  IsWin64Bit: Boolean;
{$ENDIF}
  dxInvariantFormatSettings: TFormatSettings;

var
  dxIsDesignTime: Boolean = False;


implementation


uses
{$IFDEF VCL}
  Menus, Forms,
{$ENDIF}
{$IFDEF MSWINDOWS}
  ShellAPI,
{$ENDIF}
  Math, StrUtils, SysConst, DateUtils;

const
  MaxUserNameSize = 256;

type
  TdxStreamHeader = array[0..5] of AnsiChar;

{$IFDEF MSWINDOWS}
type
  TGetUserNameExW = function (NameFormat: DWORD; lpNameBuffer: LPWSTR; var nSize: ULONG): BOOL; stdcall;
  TRtlGetVersion = function (var lpVersionInformation: TOSVersionInfoExW): Integer; stdcall;
{$ENDIF}

const
  StreamFormatANSI: TdxStreamHeader = 'DXAFMT';
  StreamFormatUNICODE: TdxStreamHeader = 'DXUFMT';

var
  FdxResourceStringsRepository: TdxResourceStringsRepository;
  IsInitialized: Boolean;
  UnitsLoader: TdxUnitsLoader;

{$IFDEF MSWINDOWS}
  FGetUserNameExChecked: Boolean = False;
  FGetUserNameExProc: TGetUserNameExW = nil;
{$ENDIF}


{$IFDEF MSWINDOWS}
function GetStringTypeA(ALocale: Cardinal; dwInfoType: DWORD; const lpSrcStr: PAnsiChar;
  cchSrc: Integer; var lpCharType): BOOL; stdcall; external kernel32 name 'GetStringTypeA';
function GetStringTypeW(dwInfoType: DWORD; const lpSrcStr: PWideChar;
  cchSrc: Integer; var lpCharType): BOOL; stdcall; external kernel32 name 'GetStringTypeW';
{$ENDIF}


function dxIsDLL: Boolean;
begin
  Result := ModuleIsLib and not ModuleIsPackage;
end;

function dxAnsiStringToString(const S: AnsiString; ACodePage: Integer = CP_ACP): string;
const
{$IFNDEF MSWINDOWS}
  MB_PRECOMPOSED = 1;
{$ENDIF}
  ConversionFlags: array[Boolean] of Integer = (MB_PRECOMPOSED, 0);
var
  ADestLength: Integer;
begin
  if S = '' then
    Exit('');
  if {$IFDEF MSWINDOWS} not IsWinNT and {$ENDIF}
    (ACodePage = CP_UTF8) then //CP_UTF8 not supported on Windows 95
    Exit(UTF8ToString(S));
{$IFDEF DELPHIXE}
  ADestLength := UnicodeFromLocaleChars(ACodePage, 0, PAnsiChar(S), Length(S), nil, 0);
  SetLength(Result, ADestLength);
  UnicodeFromLocaleChars(ACodePage, ConversionFlags[ACodePage = CP_UTF8], PAnsiChar(S), Length(S), PWideChar(Result), ADestLength);
{$ELSE}
  ADestLength := MultiByteToWideChar(ACodePage, 0, PAnsiChar(S), Length(S), nil, 0);
  SetLength(Result, ADestLength);
  MultiByteToWideChar(ACodePage, ConversionFlags[ACodePage = CP_UTF8], PAnsiChar(S), Length(S), PWideChar(Result), ADestLength);
{$ENDIF}
end;

function dxStringToAnsiString(const S: string; ACodePage: Integer = CP_ACP): AnsiString;
var
  ADestLength: Integer;
begin
  if S = '' then
    Exit('');
  if {$IFDEF MSWINDOWS} not IsWinNT and {$ENDIF}
    (ACodePage = CP_UTF8) then //CP_UTF8 not supported on Windows 95
    Exit(UTF8Encode(S));
{$IFDEF DELPHIXE}
  ADestLength := LocaleCharsFromUnicode(ACodePage, 0, PWideChar(S), Length(S), nil, 0, nil, nil);
  SetLength(Result, ADestLength);
  LocaleCharsFromUnicode(ACodePage, 0, PWideChar(S), Length(S), PAnsiChar(Result), ADestLength, nil, nil);
{$ELSE}
  ADestLength := WideCharToMultiByte(ACodePage, 0, PWideChar(S), Length(S), nil, 0, nil, nil);
  SetLength(Result, ADestLength);
  WideCharToMultiByte(ACodePage, 0, PWideChar(S), Length(S), PAnsiChar(Result), ADestLength, nil, nil);
{$ENDIF}
end;

function dxVariantToString(const V: Variant): string;
begin
  if VarIsStr(V) then
    Result := VarToStr(V)
  else
    Result := dxAnsiStringToString(dxVariantToAnsiString(V));
end;

function dxVariantToAnsiString(const V: Variant): AnsiString;
var
  ASize: Integer;
begin
  if VarIsArray(V) and (VarArrayDimCount(V) = 1) then
  begin
    ASize := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    SetLength(Result, ASize);
    Move(VarArrayLock(V)^, Result[1], ASize);
    VarArrayUnlock(V);
  end
  else
    if VarType(V) = varString then
      Result := AnsiString(TVarData(V).VString)
    else
      Result := dxStringToAnsiString(VarToStr(V))
end;

function dxVarIsBlob(const V: Variant): Boolean;
begin
  Result := VarIsStr(V) or (VarIsArray(V) and (VarArrayDimCount(V) = 1));
end;

function dxShortStringToString(const S: ShortString): string;
begin
  Result := UTF8ToString(S);
end;

function dxStringToShortString(const S: string): ShortString;
begin
  Result := UTF8EncodeToShortString(S);
end;

procedure dxWriteStandardEncodingSignature(AStream: TStream);
begin
  AStream.WriteBuffer(dxUnicodePrefix, SizeOf(dxUnicodePrefix));
end;

function dxIsUnicodeStream(AStream: TStream): Boolean;
var
  B: TdxStreamHeader;
begin
  Result := False;
  if (AStream.Size - AStream.Position) >= SizeOf(TdxStreamHeader) then
  begin
    AStream.ReadBuffer(B, SizeOf(TdxStreamHeader));
    Result := B = StreamFormatUNICODE;
    if not Result and (B <> StreamFormatANSI) then
      AStream.Position := AStream.Position - SizeOf(TdxStreamHeader);
  end;
end;

procedure dxWriteStreamType(AStream: TStream);
begin
{$IFNDEF STREAMANSIFORMAT}
   AStream.WriteBuffer(StreamFormatUNICODE, SizeOf(TdxStreamHeader));
{$ENDIF}
end;

procedure dxWriteStr(Stream: TStream; const S: string);
var
  L: Integer;
{$IFDEF STREAMANSIFORMAT}
  SA: AnsiString;
{$ENDIF}
begin
  L := Length(S);
  if L > $FFFF then L := $FFFF;
  Stream.WriteBuffer(L, SizeOf(Word));
  if L > 0 then
  begin
  {$IFDEF STREAMANSIFORMAT}
    SA := UTF8Encode(S);
    Stream.WriteBuffer(SA[1], L);
  {$ELSE}
    Stream.WriteBuffer(S[1], L * SizeOf(Char));
  {$ENDIF}
  end;
end;

function dxStreamToVariant(AStream: TStream): Variant;
var
  ASavePos: Integer;
begin
  Result := Null;
  ASavePos := AStream.Position;
  try
    AStream.Position := 0;
    Result := dxStreamToVariant(AStream, AStream.Size);
  finally
    AStream.Position := ASavePos;
  end;
end;

function dxStreamToVariant(AStream: TStream; ASize: Integer): Variant;
var
  ASavePos: Integer;
  ABinaryData: AnsiString;
begin
  Result := Null;
  if ASize > AStream.Size - AStream.Position then
    ASize := AStream.Size - AStream.Position;
  if ASize <= 0 then
    Exit;
  SetLength(ABinaryData, ASize);
  ASavePos := AStream.Position;
  AStream.ReadBuffer(ABinaryData[1], ASize);
  AStream.Position := ASavePos;
  Result := ABinaryData;
end;

function dxReadStr(Stream: TStream; AIsUnicode: Boolean): string;
var
  L: Word;
  SA: AnsiString;
  SW: WideString;
begin
  Stream.ReadBuffer(L, SizeOf(Word));
  if AIsUnicode then
  begin
    SetLength(SW, L);
    if L > 0 then Stream.ReadBuffer(SW[1], L * 2);
    Result := SW;
  end
  else
  begin
    SetLength(SA, L);
    if L > 0 then Stream.ReadBuffer(SA[1], L);
    Result := UTF8ToWideString(SA);
  end;
end;

function dxResourceStringsRepository: TdxResourceStringsRepository;
begin
  if FdxResourceStringsRepository = nil then
    FdxResourceStringsRepository := TdxResourceStringsRepository.Create;
  Result := FdxResourceStringsRepository;
end;

type
  TcxResourceStringsModificationMode = (rmmByResStringValue, rmmByResStringID, rmmUndefined);

  TcxResOriginalStrings = class(TStringList)
  public
    constructor Create;
  end;

var
  FResOriginalStrings: TcxResOriginalStrings;
  FResStrings: TStringList;
  FResStringsModificationMode: TcxResourceStringsModificationMode = rmmUndefined;

procedure DestroyResStringLists;
begin
  FResStringsModificationMode := rmmUndefined;
  FreeAndNil(FResOriginalStrings);
  FreeAndNil(FResStrings);
end;

constructor TcxResOriginalStrings.Create;
begin
  inherited Create;
  CaseSensitive := True;
end;

procedure CreateResStringLists(AResStringsModificationMode: TcxResourceStringsModificationMode);
begin
  if AResStringsModificationMode = rmmUndefined then
    raise EdxException.Create('');
  if (FResStringsModificationMode <> rmmUndefined) and
    (AResStringsModificationMode <> FResStringsModificationMode) then
      raise EdxException.Create('You cannot mix cxSetResourceString and cxSetResourceStringNet calls');

  if FResStringsModificationMode = rmmUndefined then
  begin
    FResStringsModificationMode := AResStringsModificationMode;
    FResOriginalStrings := TcxResOriginalStrings.Create;
    FResStrings := TStringList.Create;
  end;
end;

function GetResOriginalStringIndex(AResString: TcxResourceStringID): Integer;
begin
  case FResStringsModificationMode of
    rmmByResStringValue:
      Result := FResOriginalStrings.IndexOf(LoadResString(AResString));
    rmmByResStringID:
      Result := FResOriginalStrings.IndexOfObject(TObject(AResString));
  else
    Result := -1;
  end;
end;

function cxGetResourceString(AResString: TcxResourceStringID): string;
var
  AIndex: Integer;
begin
  AIndex := GetResOriginalStringIndex(AResString);
  if AIndex <> -1 then
    Result := FResStrings[AIndex]
  else
    Result := LoadResString(AResString);
end;

procedure cxSetResourceString(AResString: TcxResourceStringID; const Value: string);
var
  AIndex: Integer;
begin
  CreateResStringLists(rmmByResStringID);
  AIndex := GetResOriginalStringIndex(AResString);
  if AIndex <> -1 then
    FResStrings[AIndex] := Value
  else
  begin
    FResOriginalStrings.AddObject(LoadResString(AResString), TObject(AResString));
    FResStrings.Add(Value);
  end;
end;

function cxGetResourceString(const AResString: string): string; deprecated;
begin
  Result := cxGetResourceStringNet(AResString);
end;

function cxGetResourceStringNet(const AResString: string): string; deprecated;
var
  AIndex: Integer;
begin
  Result := AResString;
  if FResOriginalStrings <> nil then
  begin
    AIndex := FResOriginalStrings.IndexOf(AResString);
    if AIndex <> -1 then
      Result := FResStrings[AIndex];
  end
end;

procedure cxSetResourceStringNet(const AResString, Value: string); deprecated;
var
  AIndex: Integer;
begin
  CreateResStringLists(rmmByResStringValue);
  AIndex := FResOriginalStrings.IndexOf(AResString);
  if AIndex <> -1 then
    FResStrings[AIndex] := Value
  else
  begin
    FResOriginalStrings.Add(AResString);
    FResStrings.Add(Value);
  end;
end;

procedure cxClearResourceStrings;
begin
  if FResStrings <> nil then
    FResStrings.Clear;
  if FResOriginalStrings <> nil then
    FResOriginalStrings.Clear;
end;

{$IFDEF MSWINDOWS}
function dxCreateTempFile(const AExtension: string = ''): string;
var
  ABuffer: array[0..MAX_PATH] of Char;
begin
  repeat
    GetTempPath(MAX_PATH, ABuffer);
    GetTempFileName(ABuffer, '~DX', 0, ABuffer);
    if AExtension = '' then
      Exit(ABuffer);

    Result := ChangeFileExt(ABuffer, AExtension);
    if RenameFile(ABuffer, Result) then
      Exit;
    if GetLastError = ERROR_ALREADY_EXISTS then
      DeleteFile(ABuffer)
    else
      RaiseLastOSError;
  until False;
end;
{$ENDIF}

function dxIsPathDelimiter(const S: string; Index: Integer; const APathDelimiter: Char = PathDelim): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = APathDelimiter) and (ByteType(S, Index) = mbSingleByte);
end;

function dxExcludeTrailingPathDelimiter(const APath: string; const APathDelimiter: Char = PathDelim): string;
var
  AIndex: Integer;
begin
  Result := APath;
  AIndex := Length(Result);
  if dxIsPathDelimiter(Result, AIndex, APathDelimiter) then
    SetLength(Result, AIndex - 1);
end;

function dxExtractFileName(const APath: string; const APathDelimiter: Char = PathDelim): string;
var
  AIndex: Integer;
begin
  AIndex := LastDelimiter(APathDelimiter + DriveDelim, APath);
  Result := Copy(APath, AIndex + 1, MaxInt);
end;

function dxIncludeTrailingPathDelimiter(const APath: string; const APathDelimiter: Char = PathDelim): string;
begin
  if dxIsPathDelimiter(APath, Length(APath), APathDelimiter) then
    Result := APath
  else
    Result := APath + APathDelimiter;
end;

function dxReplacePathDelimiter(const APath: AnsiString; const AOldDelimiter, ANewDelimiter: AnsiChar): AnsiString;
begin
  if AOldDelimiter <> ANewDelimiter then
    Result := AnsiStrings.StringReplace(APath, AOldDelimiter, ANewDelimiter, [rfReplaceAll])
  else
    Result := APath;
end;

function dxReplacePathDelimiter(const APath: string; const AOldDelimiter, ANewDelimiter: Char): string;
begin
  if AOldDelimiter <> ANewDelimiter then
    Result := StringReplace(APath, AOldDelimiter, ANewDelimiter, [rfReplaceAll])
  else
    Result := APath;
end;

{$IFDEF MSWINDOWS}
function dxGetStringTypeA(Locale: LCID; dwInfoType: DWORD; const lpSrcStr: PAnsiChar;
  cchSrc: Integer; var lpCharType): BOOL;
begin
  Result := GetStringTypeA(Locale, dwInfoType, lpSrcStr, cchSrc, lpCharType);
end;

function dxGetStringTypeW(dwInfoType: DWORD; const lpSrcStr: PWideChar;
  cchSrc: Integer; var lpCharType): BOOL;
begin
  Result := GetStringTypeW(dwInfoType, lpSrcStr, cchSrc, lpCharType);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function dxGetAnsiCharCType1(Ch: AnsiChar): Word;
begin
  if not dxGetStringTypeA(GetThreadLocale, CT_CTYPE1, @Ch, 1, Result) then
    Result := 0;
end;

function dxGetWideCharCType1(Ch: WideChar): Word;
begin
  if not dxGetStringTypeW(CT_CTYPE1, @Ch, 1, Result) then
    Result := 0;
end;

procedure cxZeroMemory(ADestination: Pointer; ACount: Integer);
begin
  ZeroMemory(ADestination, ACount);
end;

function cxAllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  cxZeroMemory(Result, Size);
end;
{$ENDIF}

procedure cxFreeMem(P: Pointer);
begin
  FreeMem(P);
end;

procedure dxFillChar(var ADest; Count: Integer; const APattern: Char);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PWordArray(@ADest)^[I] := Word(APattern);
end;

procedure dxFreeAndNil(var AObject);
begin
  TObject(AObject).Free;
  Pointer(AObject) := nil;
end;

procedure dxAppendList(ASource, ADestination: TList);
var
  APrevDestinationCount: Integer;
begin
  APrevDestinationCount := ADestination.Count;
  ADestination.Count := ADestination.Count + ASource.Count;
  cxCopyData(ASource.List, ADestination.List, 0, APrevDestinationCount * SizeOf(Pointer),
    ASource.Count * SizeOf(Pointer));
end;

procedure dxCopyList(ASource, ADestination: TList);
begin
  ADestination.Count := ASource.Count;
  cxCopyData(ASource.List, ADestination.List, ASource.Count * SizeOf(Pointer));
end;

procedure cxCopyData(ASource, ADestination: Pointer; ACount: Integer);
begin
  Move(ASource^, ADestination^, ACount);
end;

procedure cxCopyData(ASource, ADestination: Pointer; ASourceOffSet, ADestinationOffSet, ACount: Integer);
begin
  if ASourceOffSet > 0 then
    ASource := ShiftPointer(ASource, ASourceOffSet);
  if ADestinationOffSet > 0 then
    ADestination := ShiftPointer(ADestination, ADestinationOffSet);
  cxCopyData(ASource, ADestination, ACount);
end;

function ReadBoolean(ASource: Pointer; AOffset: Integer = 0): WordBool;
begin
  cxCopyData(ASource, @Result, AOffset, 0, SizeOf(WordBool));
end;

function ReadByte(ASource: Pointer; AOffset: Integer = 0): Byte;
begin
  cxCopyData(ASource, @Result, AOffset, 0, SizeOf(Byte));
end;

function ReadInteger(ASource: Pointer; AOffset: Integer = 0): Integer;
begin
  cxCopyData(ASource, @Result, AOffset, 0, SizeOf(Integer));
end;

function ReadPointer(ASource: Pointer): Pointer;
begin
  Result := Pointer(ASource^);
end;

function ReadWord(ASource: Pointer; AOffset: Integer = 0): Word;
begin
  cxCopyData(ASource, @Result, AOffset, 0, SizeOf(Word));
end;

{$IFDEF MSWINDOWS}
function dxAnsiIsAlpha(Ch: AnsiChar): Boolean;
begin
  Result := dxGetAnsiCharCType1(Ch) and C1_ALPHA > 0;
end;

function dxCharIsAlpha(Ch: Char): Boolean;
begin
  Result := dxWideIsAlpha(Ch);
end;

function dxWideIsAlpha(Ch: WideChar): Boolean;
begin
  Result := dxGetWideCharCType1(Ch) and C1_ALPHA > 0;
end;

function dxAnsiIsNumeric(Ch: AnsiChar): Boolean;
begin
  Result := dxGetAnsiCharCType1(Ch) and C1_DIGIT > 0;
end;

function dxCharIsNumeric(Ch: Char): Boolean;
begin
  Result := dxWideIsNumeric(Ch);
end;

function dxWideIsNumeric(Ch: WideChar): Boolean;
begin
  Result := dxGetWideCharCType1(Ch) and C1_DIGIT > 0;
end;

function dxWideIsSpace(Ch: WideChar): Boolean;
begin
  Result := dxGetWideCharCType1(Ch) and C1_SPACE > 0;
end;
{$ENDIF}

function dxIsWhiteSpace(AChar: Char): Boolean; inline;
begin
{$IFDEF DELPHIXE4}
  Result := AChar.IsWhiteSpace;
{$ELSE}
  Result := TCharacter.IsWhiteSpace(AChar);
{$ENDIF}
end;

function dxIsLowerCase(AChar: Char): Boolean; inline;
begin
{$IFDEF DELPHIXE4}
  Result := AChar.IsLower;
{$ELSE}
  Result := TCharacter.IsLower(AChar);
{$ENDIF}
end;

function dxLowerCase(AChar: Char): Char; inline;
begin
{$IFDEF DELPHIXE4}
  Result := AChar.ToLower;
{$ELSE}
  Result := TCharacter.ToLower(AChar);
{$ENDIF}
end;

function dxStrComp(const Str1, Str2: PAnsiChar): Integer;
begin
  Result := {$IFDEF DELPHI18}AnsiStrings.{$ENDIF}StrComp(Str1, Str2);
end;

function dxStrCopy(ADest: PAnsiChar; const ASource: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DELPHI18}AnsiStrings.{$ENDIF}StrCopy(ADest, ASource)
end;

function dxStrLCopy(ADest: PAnsiChar; const ASource: PAnsiChar; AMaxLen: Cardinal): PAnsiChar;
begin
  Result := {$IFDEF DELPHI18}AnsiStrings.{$ENDIF}StrLCopy(ADest, ASource, AMaxLen)
end;

function dxStrLen(const AStr: PAnsiChar): Cardinal;
begin
  Result := {$IFDEF DELPHI18}AnsiStrings.{$ENDIF}StrLen(AStr);
end;

{$IFDEF MSWINDOWS}
function dxGetCodePageFromCharset(ACharset: Integer): Integer;
begin
  if (ACharset = DEFAULT_CHARSET) or (ACharset = ANSI_CHARSET) then //speedup
  begin
    Result := 0;
    Exit;
  end;
  case ACharset of
    THAI_CHARSET:
      Result := 874;
    SHIFTJIS_CHARSET:
      Result := 932;
    GB2312_CHARSET:
      Result := 936;
    HANGEUL_CHARSET, JOHAB_CHARSET:
      Result := 949;
    CHINESEBIG5_CHARSET:
      Result := 950;
    EASTEUROPE_CHARSET:
      Result := 1250;
    RUSSIAN_CHARSET:
      Result := 1251;
    GREEK_CHARSET:
      Result := 1253;
    TURKISH_CHARSET:
      Result := 1254;
    HEBREW_CHARSET:
      Result := 1255;
    ARABIC_CHARSET:
      Result := 1256;
    BALTIC_CHARSET:
      Result := 1257;
  else
    Result := 0;
  end;
end;

{$ENDIF}

function dxAnsiStringToWideString(const ASource: AnsiString; ACodePage: Cardinal = CP_ACP): WideString;
begin
  Result := dxAnsiStringToString(ASource, ACodePage);
end;

function dxWideStringToAnsiString(const ASource: WideString; ACodePage: Cardinal = CP_ACP): AnsiString;
begin
  Result := dxStringToAnsiString(ASource, ACodePage);
end;

function dxAnsiStrToInt(const S: AnsiString): Integer;
begin
  Result := StrToInt(dxAnsiStringToString(S));
end;

function dxConcatenateStrings(const AStrings: array of PChar): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AStrings) - 1 do
    Result := Result + AStrings[I];
end;

procedure dxStringToBytes(const S: string; var Buf);
begin
  if Length(S) > 0 then
    Move(S[1], Buf, dxStringSize(S));
end;

function dxStrToFloat(const S: string; const ADecimalSeparator: Char = '.'): Extended;
var
  AFormatSettings: TFormatSettings;
begin
  FillChar(AFormatSettings, SizeOf(AFormatSettings), 0);
  AFormatSettings.DecimalSeparator := ADecimalSeparator;
  Result := StrToFloat(S, AFormatSettings);
end;

function dxStrToFloatDef(const S: string; const ADecimalSeparator: Char = '.'; ADefaultValue: Extended = 0): Extended;
var
  AFormatSettings: TFormatSettings;
begin
  FillChar(AFormatSettings, SizeOf(AFormatSettings), 0);
  AFormatSettings.DecimalSeparator := ADecimalSeparator;
  Result := StrToFloatDef(S, ADefaultValue, AFormatSettings);
end;

function dxFloatToStr(AValue: Extended; ADecimalSeparator: Char = '.'): string;
var
  AFormatSettings: TFormatSettings;
begin
  FillChar(AFormatSettings, SizeOf(AFormatSettings), 0);
  AFormatSettings.DecimalSeparator := ADecimalSeparator;
  Result := FloatToStr(AValue, AFormatSettings);
end;

function RemoveAccelChars(const S: string; AAppendTerminatingUnderscore: Boolean = True): string;
var
  ALastIndex, I: Integer;
begin
  Result := S;
  ALastIndex := Length(Result);
  for I := 1 to ALastIndex do
    if Result[I] = cHotkeyPrefix then
    begin
      Delete(Result, I, 1);
      if (I < ALastIndex) or not AAppendTerminatingUnderscore then
        Dec(ALastIndex)
      else
        Result := Result + '_';
    end;
end;

{$IFNDEF DELPHIXE2}
function SplitString(const S, Delimiters: string): TStringDynArray;
var
  ADelimPos: Integer;
  AIndex: Integer;
  ASplitCount: Integer;
  AStartIndex: Integer;
  I: Integer;
begin
  Result := nil;

  if S <> '' then
  begin
    ASplitCount := 0;
    for I := 1 to Length(S) do
    begin
      if IsDelimiter(Delimiters, S, I) then
        Inc(ASplitCount);
    end;
    SetLength(Result, ASplitCount + 1);

    AStartIndex := 1;
    AIndex := 0;
    repeat
      ADelimPos := FindDelimiter(Delimiters, S, AStartIndex);
      if ADelimPos <> 0 then
      begin
        Result[AIndex] := Copy(S, AStartIndex, ADelimPos - AStartIndex);
        Inc(AIndex);
        AStartIndex := ADelimPos + 1;
      end;
    until AIndex = ASplitCount;
    Result[ASplitCount] := Copy(S, AStartIndex, Length(S) - AStartIndex + 1);
  end;
end;
{$ENDIF}

function dxUTF8StringToAnsiString(const S: UTF8String): AnsiString;
begin
  Result := dxStringToAnsiString(Utf8ToAnsi(S));
end;

function dxAnsiStringToUTF8String(const S: AnsiString): UTF8String;
begin
  Result := UTF8Encode(dxAnsiStringToString(S));
end;

function dxUTF8StringToString(const S: UTF8String): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function dxUTF8StringToWideString(const S: UTF8String): WideString;
begin
  Result := UTF8ToWideString(S);
end;

function dxStringToUTF8String(const S: string): UTF8String;
begin
  Result := UTF8Encode(S);
end;

function dxWideStringToUTF8String(const S: WideString): UTF8String;
begin
  Result := UTF8Encode(S);
end;

function dxAnsiStringReplace(const AString, AOldPattern, ANewPattern: AnsiString; AFlags: TReplaceFlags): AnsiString;
begin
  Result := AnsiStrings.StringReplace(AString, AOldPattern, ANewPattern, AFlags);
end;

procedure WriteBoolean(ADestination: Pointer; AValue: WordBool; AOffset: Integer = 0);
begin
  cxCopyData(@AValue, ADestination, 0, AOffset, SizeOf(WordBool));
end;

procedure WriteByte(ADestination: Pointer; AValue: Byte; AOffset: Integer = 0);
begin
  cxCopyData(@AValue, ADestination, 0, AOffset, SizeOf(Byte));
end;

procedure WriteInteger(ADestination: Pointer; AValue: Integer; AOffset: Integer = 0);
begin
  cxCopyData(@AValue, ADestination, 0, AOffset, SizeOf(Integer));
end;

procedure WritePointer(ADestination: Pointer; AValue: Pointer);
begin
  Pointer(ADestination^) := AValue;
end;

procedure WriteWord(ADestination: Pointer; AValue: Word; AOffset: Integer = 0);
begin
  cxCopyData(@AValue, ADestination, 0, AOffset, SizeOf(Word));
end;

function ReadBufferFromStream(AStream: TStream; ABuffer: Pointer; Count: Integer): Boolean;
begin
  Result := AStream.Read(ABuffer^, Count) = Count;
end;

function ReadStringFromStream(AStream: TStream; out AValue: AnsiString): Longint;
begin
  SetLength(AValue, AStream.Size);
  Result := AStream.Read(AValue[1], AStream.Size);
end;

function WriteBufferToStream(AStream: TStream; ABuffer: Pointer; ACount: Longint): Longint;
var
  AData: TBytes;
begin
  SetLength(AData, ACount);
  if ABuffer <> nil then
    cxCopyData(ABuffer, AData, ACount);

  Result := AStream.Write(AData[0], ACount);
end;

function WriteCharToStream(AStream: TStream; AValue: AnsiChar): Longint;
begin
  Result := AStream.Write(AValue, 1);
end;

function WriteDoubleToStream(AStream: TStream; AValue: Double): Longint;
begin
  Result := AStream.Write(AValue, SizeOf(Double));
end;

function WriteIntegerToStream(AStream: TStream; AValue: Integer): Longint;
begin
  Result := AStream.Write(AValue, SizeOf(Integer));
end;

function WriteSmallIntToStream(AStream: TStream; AValue: SmallInt): Longint;
begin
  Result := AStream.Write(AValue, SizeOf(SmallInt));
end;

function WriteStringToStream(AStream: TStream; const AValue: AnsiString): Longint;
begin
  Result := AStream.Write(PAnsiChar(AValue)^, Length(AValue));
end;


function dxSwap16(const AValue: Word): Word;
begin
  Result := MakeWord(HiByte(AValue), LoByte(AValue));
end;

function dxSwap32(const AValue: DWORD): DWORD;
var
  B: array [1..4] of Byte absolute AValue;
  I: Integer;
begin
  Result := 0;
  for I := 1 to 4 do
    Result := DWORD(Result shl 8) or DWORD(B[I]);
end;

procedure ExchangeLongWords(var AValue1, AValue2);
var
  ATempValue: LongWord;
begin
  ATempValue := LongWord(AValue1);
  LongWord(AValue1) := LongWord(AValue2);
  LongWord(AValue2) := ATempValue;
end;

procedure ExchangePointers(var AValue1, AValue2);
var
  ATempValue: Pointer;
begin
  ATempValue := Pointer(AValue1);
  Pointer(AValue1) := Pointer(AValue2);
  Pointer(AValue2) := ATempValue;
end;

function dxCharInSet(C: Char; const ACharSet: TdxAnsiCharSet): Boolean;
begin
  Result := CharInSet(C, ACharSet);
end;

function dxStringSize(const S: string): Integer;
begin
  Result := Length(S) * SizeOf(Char);
end;

function ShiftPointer(P: Pointer; AOffset: Integer): Pointer;
begin
  Result := Pointer(TdxNativeInt(P) + AOffset);
end;

{$IFDEF MSWINDOWS}
function dxPointToLParam(P: TPoint): LPARAM;
begin
  Result := PointToLParam(P);
end;

function dxLParamToPoint(AParam: LPARAM): TPoint;
begin
{$IFDEF CPUX64}
  Result := TSmallPoint(LongWord(AParam));
{$ELSE}
  Result := SmallPointToPoint(TSmallPoint(AParam));
{$ENDIF}
end;
{$ENDIF}

type
  TSeekMode = (smDup, smUnique);
const
  AModeMap: array[Boolean] of TSeekMode = (smDup, smUnique);
  AModeMask: array[TSeekMode] of Byte = (0, 128);

function ReadStreamByte(AStream: TStream; AMaxPos: Integer; var AByte: Byte): Boolean;
begin
  Result := AStream.Position < AMaxPos;
  if Result then
    AStream.Read(AByte, SizeOf(Byte));
end;

procedure WriteStreamByte(AStream: TStream; AByte: Byte);
begin
  AStream.Write(AByte, SizeOf(Byte));
end;

function CompareBlock(const ABlock1, ABlock2: TBytes): Boolean;
begin
  Result := (Length(ABlock1) = Length(ABlock2)) and CompareMem(ABlock1, ABlock2, Length(ABlock1));
end;

function ReadStreamBlock(AStream: TStream; AMaxPos: Integer; var ABlock: TBytes; ABlockSize: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to ABlockSize - 1 do
    Result := Result and ReadStreamByte(AStream, AMaxPos, ABlock[I]);
end;

procedure WriteStreamBlock(AStream: TStream; const ABlock: TBytes; ABlockSize: Integer);
var
  I: Integer;
begin
  for I := 0 to ABlockSize - 1 do
    WriteStreamByte(AStream, ABlock[I]);
end;

procedure dxCompressStream(ASourceStream, ADestStream: TStream; ACompressMethod: Byte; ASize: Integer);

  procedure CompressByBlock(ASourceStream, ADestStream: TStream; ASize, ABlockSize: Integer);

    function GetCounter(const ASeekBlock: TBytes; AMode: TSeekMode; AMaxPos: Integer): Integer;
    var
      ABlock: TBytes;
    begin
      Result := 1;
      SetLength(ABlock, ABlockSize);
      while (Result < 125) and ReadStreamBlock(ASourceStream, AMaxPos, ABlock, ABlockSize) do
      begin
        if (AMode = smDup) and CompareBlock(ABlock, ASeekBlock) or (AMode = smUnique) and not CompareBlock(ABlock, ASeekBlock) then
          Inc(Result)
        else
        begin
          if AMode = smUnique then
            Dec(Result);
          Break;
        end;
        cxCopyData(ABlock, ASeekBlock, ABlockSize);
      end;
    end;

  var
    AReadBlock1, AReadBlock2: TBytes;
    ACounter, AReadCount: Integer;
    AStreamPos, AMaxPos: Integer;
    AMode: TSeekMode;
  begin
    AMaxPos := ASourceStream.Position + ASize;

    SetLength(AReadBlock1, ABlockSize);
    SetLength(AReadBlock2, ABlockSize);

    while ReadStreamBlock(ASourceStream, AMaxPos, AReadBlock1, ABlockSize) do
    begin
      AReadCount := ABlockSize;
      AStreamPos := ASourceStream.Position - ABlockSize;
      if ReadStreamBlock(ASourceStream, AMaxPos, AReadBlock2, ABlockSize) then
      begin
        Inc(AReadCount, ABlockSize);
        AMode := AModeMap[(AReadCount = ABlockSize) or not CompareBlock(AReadBlock1, AReadBlock2)];
        ASourceStream.Position := ASourceStream.Position - (AReadCount - ABlockSize);
        ACounter := GetCounter(AReadBlock1, AMode, AMaxPos);
      end
      else
      begin
        AMode := smUnique;
        ACounter := 1;
      end;

      WriteStreamByte(ADestStream, ACounter or AModeMask[AMode]);
      case AMode of
        smUnique:
          begin
            ASourceStream.Position := AStreamPos;
            ADestStream.CopyFrom(ASourceStream, ACounter * ABlockSize);
          end;
        smDup:
          WriteStreamBlock(ADestStream, AReadBlock1, ABlockSize);
      end;
      ASourceStream.Position := AStreamPos + ACounter * ABlockSize;
    end;
  end;

var
  ABlockSize, AShift: Byte;
  APrevPosition: Integer;
begin
  if ASize = 0 then
    ASize := ASourceStream.Size - ASourceStream.Position;
  APrevPosition := ADestStream.Position;
  ADestStream.Position := ADestStream.Position + SizeOf(Integer);
  WriteStreamByte(ADestStream, ACompressMethod);
  if (ACompressMethod >= 1) and (ACompressMethod <= 4) then
  begin
    ABlockSize := ACompressMethod;
    AShift := ASize mod ABlockSize;
    WriteStreamByte(ADestStream, AShift);
    if AShift > 0 then
      ADestStream.CopyFrom(ASourceStream, AShift);
    CompressByBlock(ASourceStream, ADestStream, ASize - AShift, ABlockSize);
  end
  else
{    if ACompressMethod = 5 then
    begin

    end;
    else
    }
      ADestStream.CopyFrom(ASourceStream, ASize);
  ASize := ADestStream.Position - APrevPosition;
  ADestStream.Position := APrevPosition;
  ADestStream.Write(ASize, SizeOf(ASize));
  ADestStream.Position := APrevPosition + ASize;
end;

procedure dxDecompressStream(ASourceStream, ADestStream: TStream);

  procedure DecompressByBlock(ASourceStream, ADestStream: TStream; ASize, ABlockSize: Integer);
  var
    ACode: Byte;
    AReadBlob: TBytes;
    AMaxPos: Integer;
    I: Integer;
    ACounter: Integer;
  begin
    AMaxPos := ASourceStream.Position + ASize;

    SetLength(AReadBlob, ABlockSize);

    while ReadStreamByte(ASourceStream, AMaxPos, ACode) do
    begin
      ACounter := ACode and 127;
      if (ACode and AModeMask[smUnique]) <> 0 then
        ADestStream.CopyFrom(ASourceStream, ACounter * ABlockSize)
      else
      begin
        ReadStreamBlock(ASourceStream, AMaxPos, AReadBlob, ABlockSize);
        for I := 0 to ACounter - 1 do
          WriteStreamBlock(ADestStream, AReadBlob, ABlockSize);
      end;
    end;
  end;

var
  ASize: Integer;
  ACompressMethod, AShift: Byte;
begin
  ASourceStream.Read(ASize, SizeOf(ASize));
  ASourceStream.Read(ACompressMethod, SizeOf(ACompressMethod));
  if (ACompressMethod >= 1) and (ACompressMethod <= 4) then
  begin
    ASourceStream.Read(AShift, SizeOf(AShift));
    if AShift > 0 then
      ADestStream.CopyFrom(ASourceStream, AShift);
    DecompressByBlock(ASourceStream, ADestStream, ASize - 6 - AShift, ACompressMethod);
  end
  else
    ADestStream.CopyFrom(ASourceStream, ASize - 5);
end;


{ TdxByteArray }

class function TdxByteArray.Clone(const ABytes: TBytes): TBytes;
begin
  Result := Clone(ABytes, MaxInt);
end;

class function TdxByteArray.Clone(const ABytes: TBytes; AMaxLength: Integer): TBytes;
begin
  AMaxLength := Min(AMaxLength, Length(ABytes));
  if ABytes <> nil then
  begin
    SetLength(Result, AMaxLength);
    Move(ABytes[0], Result[0], AMaxLength);
  end
  else
    Result := nil;
end;

class function TdxByteArray.Compare(const AB1, AB2: TBytes): Boolean;
begin
  if Pointer(AB1) = Pointer(AB2) then
    Exit(True);
  if (AB1 = nil) or (AB2 = nil) then
    Exit(False);
  if Length(AB1) <> Length(AB2) then
    Exit(False);
  Result := CompareMem(@AB1[0], @AB2[0], Length(AB1));
end;

class function TdxByteArray.Concatenate(ANum: Integer; const ABytes: TBytes): TBytes;
var
  ACountBytes: TBytes;
  AArrayLength: Integer;
begin
  AArrayLength := Length(ABytes);
  SetLength(Result, AArrayLength + 4);
  SetLength(ACountBytes, 4);
  ACountBytes[3] := Byte((ANum and $FF000000) shr 24);
  ACountBytes[2] := Byte((ANum and $00FF0000) shr 16);
  ACountBytes[1] := Byte((ANum and $0000FF00) shr 8);
  ACountBytes[0] := Byte(ANum and $000000FF);
  Move(ACountBytes[0], Result[0], Length(ACountBytes));
  if AArrayLength > 0 then
    Move(ABytes[0], Result[Length(ACountBytes)], AArrayLength);
end;

class function TdxByteArray.Concatenate(const AB1, AB2: TBytes): TBytes;
begin
  if AB1 = nil then
    Exit(AB2);
  if AB2 = nil then
    Exit(AB1);
  SetLength(Result, Length(AB1) + Length(AB2));
  Move(AB1[0], Result[0], Length(AB1));
  Move(AB2[0], Result[Length(AB1)], Length(AB2));
end;

class function TdxByteArray.Concatenate(const ABytes: TBytes; ANum: Integer): TBytes;
var
  ACountBytes: TBytes;
  AArrayLength: Integer;
begin
  AArrayLength := Length(ABytes);
  SetLength(Result, AArrayLength + 4);
  SetLength(ACountBytes, 4);
  ACountBytes[3] := Byte((ANum and $FF000000) shr 24);
  ACountBytes[2] := Byte((ANum and $00FF0000) shr 16);
  ACountBytes[1] := Byte((ANum and $0000FF00) shr 8);
  ACountBytes[0] := Byte(ANum and $000000FF);
  if AArrayLength > 0 then
    Move(ABytes[0], Result[0], AArrayLength);
  Move(ACountBytes[0], Result[AArrayLength], Length(ACountBytes));
end;

class function TdxByteArray.Resize(const ABytes: TBytes; ATargetSize: Integer; AFillBy: Byte): TBytes;
begin
  if Length(ABytes) = ATargetSize then
    Exit(ABytes);

  SetLength(Result, ATargetSize);
  ATargetSize := Min(ATargetSize, Length(ABytes));
  Move(ABytes[0], Result[0], ATargetSize);
  FillChar(Result[ATargetSize], Length(Result) - ATargetSize, AFillBy);
end;

{ TdxStream }

constructor TdxStream.Create(AStream: TStream);
begin
  FIsUnicode := dxIsUnicodeStream(AStream);
  FStream := AStream;
end;

function TdxStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TdxStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TdxStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TdxStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;


procedure dxAbstractError;
begin
  dxTestCheck(False, SAbstractError);
end;

procedure dxTestCheck(AValue: WordBool; const AMessage: string);
begin
//do nothing
end;

procedure dxCheckOrientation(var AValue: TdxOrientation; ADefaultOrientation: TdxOrientation);
begin
  if Ord(AValue) = -1 then
    AValue := ADefaultOrientation;
end;

procedure dxShowException(AException: TObject);
begin
{$IFDEF VCL}
  if Assigned(Application) then
    Application.ShowException(AException as Exception)
  else
{$ENDIF}
   SysUtils.ShowException(AException, nil);
end;

{$IFDEF MSWINDOWS}
function dxGetInvariantLocaleID: Integer;
begin
  if IsWinXPOrLater then
    Result := LOCALE_INVARIANT
  else
    Result := dxMakeLCID(dxMakeLangID(LANG_ENGLISH, SUBLANG_ENGLISH_US), SORT_DEFAULT);
end;

procedure dxGetLocaleFormatSettings(ALocale: Integer; var AFormatSettings: TFormatSettings);

  procedure DoGetLocaleFormatSettings(var AFormatSettings: TFormatSettings);
  begin
  {$IFDEF DELPHI15}
    AFormatSettings := TFormatSettings.Create(ALocale)
  {$ELSE}
    GetLocaleFormatSettings(ALocale, AFormatSettings);
  {$ENDIF}
  end;

var
  APrevLocale: Integer;
begin
  if not IsValidLocale(ALocale, LCID_INSTALLED) and IsValidLocale(ALocale, LCID_SUPPORTED) then
  begin
    APrevLocale := GetThreadLocale;
    try
      SetThreadLocale(ALocale);
      DoGetLocaleFormatSettings(AFormatSettings);
    finally
      SetThreadLocale(APrevLocale);
    end;
  end
  else
    DoGetLocaleFormatSettings(AFormatSettings);
end;

function dxGetLocaleInfo(ALocale, ALocaleType: Integer; const ADefaultValue: string = ''): string;
var
  ABuffer: string;
  ABufferSize: Integer;
begin
  ABufferSize := GetLocaleInfo(ALocale, ALocaleType, nil, 0);
  if ABufferSize = 0 then
    Result := ADefaultValue
  else
  begin
    SetLength(ABuffer, ABufferSize);
    GetLocaleInfo(ALocale, ALocaleType, PChar(ABuffer), ABufferSize);
    Result := Copy(ABuffer, 1, ABufferSize - 1)
  end;
end;

function dxMakeLangID(APrimaryLang, ASubLang: Word): Word;
begin
  Result := MAKELANGID(APrimaryLang, ASubLang);
end;

function dxMakeLCID(ALangId, ASortId: Word): DWORD;
begin
  Result := MAKELCID(ALangId, ASortId)
end;

function dxGetSubLangID(ALangId: Word): Word;
begin
  Result := SUBLANGID(ALangId);
end;

function dxGetUserName: string;
var
  ABuffer:  array[0..MaxUserNameSize + 1] of Char;
  ABufferSize: Cardinal;
begin
  ABufferSize := MaxUserNameSize + 1;
  GetUserName(ABuffer, ABufferSize);
  SetString(Result, ABuffer, ABufferSize - 1);
end;

function dxGetUserNameEx(AFormat: DWORD): string;
var
  ABuffer:  array[0..MaxUserNameSize] of WideChar;
  ABufferSize: Cardinal;
begin
  if IsWin10 then
    Exit(dxGetUserName);

  if not FGetUserNameExChecked then
  begin
    FGetUserNameExChecked := True;
    FGetUserNameExProc := GetProcAddress(LoadLibrary('secur32.dll'), 'GetUserNameExW');
  end;

  if Assigned(FGetUserNameExProc) then
  begin
    ABufferSize := MaxUserNameSize;
    if FGetUserNameExProc(AFormat, @ABuffer[0], ABufferSize) then
      SetString(Result, ABuffer, ABufferSize)
    else
      Result := dxGetUserName;
  end
  else
    Result := dxGetUserName;
end;

function dxShellExecute(AHandle: HWND; const AFileName: string; AShowCmd: Integer = SW_SHOWNORMAL): Boolean;
begin
  Result := ShellExecute(AHandle, 'open', PChar(AFileName), nil, nil, AShowCmd) >= 32;
end;

function dxShellExecute(const AFileName: string; AShowCmd: Integer = SW_SHOWNORMAL): Boolean;
begin
  Result := dxShellExecute(0, AFileName, AShowCmd);
end;
{$ENDIF}

procedure dxCallNotify(ANotifyEvent: TNotifyEvent; ASender: TObject);
begin
  if Assigned(ANotifyEvent) then
    ANotifyEvent(ASender);
end;

{$IFDEF MSWINDOWS}
function dxGetTickCount: Int64;
begin
  if not QueryPerformanceCounter(Result) then
    Result := GetTickCount
  else
    Result := Result div 1000;
end;
{$ENDIF}

function dxSameMethods(const Method1, Method2): Boolean;
begin
  Result := (TMethod(Method1).Code = TMethod(Method2).Code) and
    (TMethod(Method1).Data = TMethod(Method2).Data);
end;

function dxBooleanToDefaultBoolean(AValue: Boolean): TdxDefaultBoolean;
begin
  if AValue then
    Result := bTrue
  else
    Result := bFalse;
end;

function dxDefaultBooleanToBoolean(AValue: TdxDefaultBoolean; ADefault: Boolean): Boolean;
begin
  case AValue of
    bFalse:
      Result := False;
    bTrue:
      Result := True;
  else
    Result := ADefault;
  end;
end;

function dxMakeInt64(const A, B: Integer): Int64;
begin
  Result := Int64(A) or (Int64(B) shl 32);
end;

// math
function dxFMod(const ANumerator, ADenominator: Single): Single;
begin
  Result := ANumerator - Trunc(ANumerator / ADenominator) * ADenominator;
end;

function dxFMod(const ANumerator, ADenominator: Double): Double;
begin
  Result := ANumerator - Trunc(ANumerator / ADenominator) * ADenominator;
end;

function dxFMod(const ANumerator, ADenominator: Extended): Extended;
begin
  Result := ANumerator - Trunc(ANumerator / ADenominator) * ADenominator;
end;

// string functions
function dxBinToHex(const ABuffer: AnsiString): AnsiString;
begin
  Result := dxBinToHex(PAnsiChar(ABuffer), Length(ABuffer));
end;

function dxBinToHex(const ABuffer: PAnsiChar; ABufSize: Integer): AnsiString;
begin
  SetLength(Result, ABufSize * 2);
  BinToHex(ABuffer, PAnsiChar(Result), ABufSize);
end;

function dxHexToBin(const AText: AnsiString): AnsiString;
begin
  Result := dxHexToBin(PAnsiChar(AText));
end;

function dxHexToBin(const AText: PAnsiChar): AnsiString;
begin
  SetLength(Result, Length(AText) div 2);
  HexToBin(AText, PAnsiChar(Result), Length(Result));
end;

function dxHexToByte(const AHex: string): Byte;
var
  S: string;
begin
  S := DupeString('0', 2 - Length(AHex)) + AHex;
  HexToBin(PChar(S), @Result, SizeOf(Result));
end;

function dxCharCount(const S: string): Integer;
begin
  Result := ElementToCharLen(S, Length(S));
end;

  { TdxProductResourceStrings }

constructor TdxProductResourceStrings.Create(const AName: string; AInitializeProc: TdxAddResourceStringsProcedure);
begin
  inherited Create;
  FName := AName;
  FResStringNames := TStringList.Create;
  TStringList(FResStringNames).Sorted := True;
  FInitializeProcedures := TList.Create;
  AddProcedure(AInitializeProc);
end;

destructor TdxProductResourceStrings.Destroy;
begin
  FreeAndNil(FInitializeProcedures);
  FreeAndNil(FResStringNames);
  inherited Destroy;
end;

procedure TdxProductResourceStrings.Add(const AResStringName: string; AResStringAddr: Pointer);
begin
  FResStringNames.AddObject(AResStringName, AResStringAddr);
end;

procedure TdxProductResourceStrings.Clear;
begin
  FResStringNames.Clear;
end;

function TdxProductResourceStrings.GetIndexByName(const AName: string): Integer;
begin
  if not TStringList(FResStringNames).Find(AName, Result) then
    Result := -1;
end;

procedure TdxProductResourceStrings.AddProcedure(AProc: TdxAddResourceStringsProcedure);
begin
  if Assigned(AProc) and (FInitializeProcedures.IndexOf(@AProc) = -1) then
  begin
    FInitializeProcedures.Add(@AProc);
    AProc(Self);
  end;
end;

procedure TdxProductResourceStrings.RemoveProcedure(AProc: TdxAddResourceStringsProcedure);
begin
  FInitializeProcedures.Remove(@AProc);
end;

procedure TdxProductResourceStrings.Translate;
var
  I: Integer;
begin
  for I := 0 to ResStringsCount - 1 do
    SetTranslation(I);
end;

function TdxProductResourceStrings.GetNames(AIndex: Integer): string;
begin
  Result := FResStringNames[AIndex];
end;

function TdxProductResourceStrings.GetResStringsCount: Integer;
begin
  Result := FResStringNames.Count;
end;

procedure TdxProductResourceStrings.SetTranslation(AIndex: Integer);
begin
  dxResourceStringsRepository.OnTranslateResString(Names[AIndex], FResStringNames.Objects[AIndex]);
end;

function TdxProductResourceStrings.GetValues(AIndex: Integer): string;
begin
  Result := LoadResString(PResStringRec(FResStringNames.Objects[AIndex]));
end;

 { TdxResourceStringsRepository }

constructor TdxResourceStringsRepository.Create;
begin
  FProducts := TObjectList.Create;
  FListeners := TList.Create;
end;

destructor TdxResourceStringsRepository.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FProducts);
  inherited;
end;

procedure TdxResourceStringsRepository.AddListener(AListener: IdxLocalizerListener);
begin
  if FListeners.IndexOf(Pointer(AListener)) = -1 then
    FListeners.Add(Pointer(AListener));
end;

procedure TdxResourceStringsRepository.RemoveListener(AListener: IdxLocalizerListener);
begin
  FListeners.Remove(Pointer(AListener));
end;

procedure TdxResourceStringsRepository.NotifyListeners;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    IdxLocalizerListener(FListeners[I]).TranslationChanged;
end;

procedure TdxResourceStringsRepository.RegisterProduct(const AProductName: string; AAddStringsProc: TdxAddResourceStringsProcedure);
var
  AIndex: Integer;
begin
  AIndex := GetProductIndexByName(AProductName);
  if AIndex >= 0 then
    Products[AIndex].AddProcedure(AAddStringsProc)
  else
    FProducts.Add(TdxProductResourceStrings.Create(AProductName, AAddStringsProc));
end;

function TdxResourceStringsRepository.GetProductIndexByName(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ProductsCount - 1 do
    if Products[I].Name = AName then
    begin
      Result := I;
      Break;
    end;
end;

function TdxResourceStringsRepository.GetOriginalValue(const AName: string): string;
var
  I, AIndex: Integer;
begin
  Result := '';
  for I := 0 to ProductsCount - 1 do
  begin
    AIndex := Products[I].GetIndexByName(AName);
    if AIndex <> -1 then
    begin
      Result := Products[I].Values[AIndex];
      Break;
    end;
  end;
end;

procedure TdxResourceStringsRepository.Translate;
var
  I: Integer;
begin
  if Assigned(FOnTranslateResString) then
  begin
    for I := 0 to ProductsCount - 1 do
      Products[I].Translate;
  end;
end;

procedure TdxResourceStringsRepository.UnRegisterProduct(const AProductName: string; AAddStringsProc: TdxAddResourceStringsProcedure = nil);
var
  AIndex: Integer;
  AProduct: TdxProductResourceStrings;
begin
  AIndex := GetProductIndexByName(AProductName);
  if AIndex <> -1 then
  begin
    if Assigned(AAddStringsProc) then
    begin
      AProduct := Products[AIndex];
      AProduct.RemoveProcedure(AAddStringsProc);
      if AProduct.InitializeProcedures.Count = 0 then
        FProducts.Delete(AIndex);
    end
    else
      FProducts.Delete(AIndex);
  end;
end;

function TdxResourceStringsRepository.GetProducts(AIndex: Integer): TdxProductResourceStrings;
begin
  Result := TdxProductResourceStrings(FProducts[AIndex]);
end;

function TdxResourceStringsRepository.GetProductsCount: Integer;
begin
  Result := FProducts.Count;
end;


{$IFDEF MSWINDOWS}
procedure InitPlatformInfo;
type
  PWKSTA_INFO_100 = ^WKSTA_INFO_100;
  WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computerName: LPWSTR;
    wki100_LanGroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
var
{$IFNDEF CPUX64}
  AIsWow64Process: function (AHandle: THandle; AWow64Process: PBOOL): BOOL; stdcall;
  AIsWow64: BOOL;
{$ENDIF CPUX64}
  ARtlHandle: THandle;
  ARtlGetVersion: TRtlGetVersion;
  AVersionInfo: TOSVersionInfoExW;
begin
  IsWin9X := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;

  IsWin95 := IsWin9X and (Win32MinorVersion = 0);
  IsWin98 := IsWin9X and (Win32MinorVersion = 10);
  IsWinMe := IsWin9X and (Win32MinorVersion = 90);

  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;

  IsWin2K := IsWinNT and (Win32MajorVersion = 5) and (Win32MinorVersion = 0);
  IsWin2KOrLater := IsWinNT and (Win32MajorVersion >= 5);
  IsWinXP := IsWinNT and (Win32MajorVersion = 5) and (Win32MinorVersion > 0);
  IsWinXPOrLater := IsWinNT and (Win32MajorVersion >= 5) and not IsWin2K;
  IsWin2KOrXP := IsWin2K or IsWinXP;

  IsWinVista := IsWinNT and (Win32MajorVersion = 6) and (Win32MinorVersion = 0);
  IsWinVistaOrLater := IsWinNT and (Win32MajorVersion >= 6);
  IsWinSeven := IsWinNT and (Win32MajorVersion = 6) and (Win32MinorVersion = 1);
  IsWinSevenOrLater := IsWinVistaOrLater and not IsWinVista;
  IsWin8 := IsWinNT and (Win32MajorVersion = 6) and (Win32MinorVersion = 2);
  IsWin8OrLater := IsWinSevenOrLater and not IsWinSeven;

  IsWin10 := False;
  IsWin10OrLater := False;
  if IsWin8OrLater then
  begin
    ARtlHandle := LoadLibrary('ntdll.dll');
    if ARtlHandle > 32 then
    try
      ARtlGetVersion := GetProcAddress(ARtlHandle, 'RtlGetVersion');
      if Assigned(ARtlGetVersion) then
      begin
        ZeroMemory(@AVersionInfo, SizeOf(AVersionInfo));
        AVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoExW);
        if ARtlGetVersion(AVersionInfo) = 0 then
        begin
          IsWin10 := (AVersionInfo.dwMajorVersion = 10) and (AVersionInfo.dwMinorVersion = 0);
          IsWin10OrLater := IsWin10 or (AVersionInfo.dwMajorVersion > 10);
          IsWin10FallCreatorsUpdate := IsWin10 and (AVersionInfo.dwBuildNumber = 16299);
          IsWin10v1809OrNewer := IsWin10 and (AVersionInfo.dwBuildNumber >= 17763);
        end;
      end;
    finally
      FreeLibrary(ARtlHandle);
    end;
  end;

  // IsWow64Process
{$IFDEF CPUX64}
  IsWin64Bit := True;
{$ELSE}
  AIsWow64Process := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process');
  if Assigned(AIsWow64Process) and AIsWow64Process(GetCurrentProcess, @AIsWow64) then
    IsWin64Bit := AIsWow64
  else
    IsWin64Bit := False;
{$ENDIF CPUX64}
end;
{$ENDIF}

function dxCompareValues(A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else
    if A > B then
      Result := 1
    else
      Result := 0;
end;

function dxCompareValues(A, B: Pointer): Integer;
begin
  if TdxNativeUInt(A) < TdxNativeUInt(B) then
    Result := -1
  else
    if TdxNativeUInt(A) > TdxNativeUInt(B) then
      Result := 1
    else
      Result := 0;
end;

function dxSameText(const A, B: string): Boolean;
begin
  Result := (Length(A) = Length(B)) and SameText(A, B);
end;

function dxGetBuildNumberAsString(ABuildNumber: Cardinal = 0): string;
var
  AMajor, AMinor, ABuild: Integer;
begin
  if ABuildNumber = 0 then
    ABuildNumber := dxBuildNumber;
  dxFactorizeBuildNumber(ABuildNumber, AMajor, AMinor, ABuild);
  Result := Format('%d.%d.%d', [AMajor mod 100, AMinor, ABuild]);
end;

function dxGetShortBuildNumberAsString(ABuildNumber: Cardinal = 0): string;
var
  AMajor, AMinor, ABuild: Integer;
begin
  if ABuildNumber = 0 then
    ABuildNumber := dxBuildNumber;
  dxFactorizeBuildNumber(ABuildNumber, AMajor, AMinor, ABuild);
  Result := Format('%d.%d', [AMajor mod 1000, AMinor]);
end;

procedure dxFactorizeBuildNumber(ABuildNumber: Cardinal; out AMajor, AMinor, ABuild: Integer);
var
  AMinorAndBuild: Integer;
begin
  if ABuildNumber = 0 then
    ABuildNumber := dxBuildNumber;

  AMajor := ABuildNumber div 10000;
  AMinorAndBuild := ABuildNumber mod 10000;
  AMinor := AMinorAndBuild div 100;
  ABuild := AMinorAndBuild mod 100;
end;

function dxGenerateGUID: string;
var
  AGuid: TGUID;
begin
  CreateGUID(AGuid);
  Result := GUIDToString(AGuid);
end;

function dxGenerateID: string;
begin
  Result := dxGenerateGUID;
  Result := Copy(Result, 2, Length(Result) - 2);
end;



{ Safe<T> }

class function Safe<T>.Cast(AObject: TObject): T;
begin
  if (AObject <> nil) and AObject.InheritsFrom(T) then
    Result := T(AObject)
  else
    Result := nil;
end;

{ TdxLoader }

constructor TdxUnitsLoader.Create;
begin
  inherited Create;
  FinalizeList := TList.Create;
  InitializeList := TList.Create;
end;

destructor TdxUnitsLoader.Destroy;
begin
  Finalize;
  FreeAndNil(InitializeList);
  FreeAndNil(FinalizeList);
  inherited Destroy;
end;

procedure TdxUnitsLoader.AddUnit(const AInitializeProc, AFinalizeProc: Pointer);
var
  AProc: TdxProc;
begin
  if AInitializeProc <> nil then
  begin
    AProc := AInitializeProc;
    if not dxIsDLL then
    begin
      IsInitialized := True;
      AProc;
    end
    else
      InitializeList.Add(AInitializeProc);
  end;
  if AFinalizeProc <> nil then
    FinalizeList.Add(AFinalizeProc);
end;

procedure TdxUnitsLoader.CallProc(AProc: TdxProc);
begin
  if Assigned(AProc) then AProc;
end;

procedure TdxUnitsLoader.RemoveUnit(const AFinalizeProc: Pointer);
var
  AProc: TdxProc;
begin
  AProc := AFinalizeProc;
  if FinalizeList.Remove(AFinalizeProc) >= 0 then
  begin
    if IsInitialized then
      CallProc(AProc);
  end;
end;

procedure TdxUnitsLoader.Finalize;
var
  I: Integer;
begin
  if IsInitialized then
  begin
    for I := FinalizeList.Count - 1 downto 0 do
      CallProc(TdxProc(FinalizeList[I]));
  end;
  IsInitialized := False;
  FinalizeList.Clear;
end;

procedure TdxUnitsLoader.Initialize;
var
  I: Integer;
begin
  for I := 0 to InitializeList.Count - 1 do
    CallProc(TdxProc(InitializeList[I]));
  InitializeList.Clear;
  IsInitialized := True;
end;

function dxUnitsLoader: TdxUnitsLoader;
begin
  if UnitsLoader = nil then
    UnitsLoader := TdxUnitsLoader.Create;
  Result := UnitsLoader;
end;


procedure dxInitialize;
begin
  dxUnitsLoader.Initialize;
end;

procedure dxFinalize;
begin
  dxUnitsLoader.Finalize;
end;

initialization
{$IFDEF MSWINDOWS}
  InitPlatformInfo;
{$ENDIF}
  dxGetLocaleFormatSettings(dxGetInvariantLocaleID, dxInvariantFormatSettings);

finalization
  FreeAndNil(FdxResourceStringsRepository);
  DestroyResStringLists;
  if dxIsDLL and (UnitsLoader.FinalizeList.Count > 0) then
    raise Exception.Create('Need call dxFinalize before free library!');
  FreeAndNil(UnitsLoader);
end.
