{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit cxVariants;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, dxCore, Variants, Generics.Defaults, Generics.Collections;

type
  LargeInt = Int64;
  TVariantArray = array of Variant;

  { TcxFiler }

  TcxFiler = class
  private
    FIsUnicode: Boolean;
    FStream: TStream;
    FVersion: Cardinal;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(AStream: TStream; AVersion: Cardinal); overload;

    property IsUnicode: Boolean read FIsUnicode;
    property Stream: TStream read FStream;
    property Version: Cardinal read FVersion write FVersion;
  end;

  { TcxReader }

  TcxReader = class(TcxFiler)
  public
    function ReadAnsiString: AnsiString;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadCardinal: Cardinal;
    function ReadChar: Char;
    function ReadCurrency: Currency;
    function ReadDateTime: TDateTime;
    function ReadFloat: Extended;
    function ReadInt64: Int64;
    function ReadInteger: Integer;
    function ReadLargeInt: LargeInt;
    procedure ReadMemoryStream(AStream: TMemoryStream);
    function ReadPoint: TPoint;
    function ReadRect: TRect;
    function ReadShortInt: ShortInt;
    function ReadSingle: Single;
    function ReadSmallInt: SmallInt;
    function ReadString_: string;
    function ReadVariant: Variant;
    function ReadWideString: WideString;
    function ReadWord: Word;
  end;

  { TcxWriter }

  TcxWriter = class(TcxFiler)
  public
    procedure BeginChunk(out APosition: Int64);
    procedure EndChunk(const APosition: Int64);

    procedure WriteAnsiString(const S: AnsiString);
    procedure WriteBoolean(AValue: Boolean);
    procedure WriteByte(AValue: Byte);
    procedure WriteCardinal(AValue: Cardinal);
    procedure WriteChar(AValue: Char);
    procedure WriteCurrency(AValue: Currency);
    procedure WriteDateTime(AValue: TDateTime);
    procedure WriteFloat(AValue: Extended);
    procedure WriteInt64(AValue: Int64);
    procedure WriteInteger(AValue: Integer);
    procedure WriteLargeInt(AValue: LargeInt);
    procedure WriteMemoryStream(AStream: TMemoryStream);
    procedure WritePoint(const AValue: TPoint);
    procedure WriteRect(const AValue: TRect);
    procedure WriteShortInt(AValue: ShortInt);
    procedure WriteSingle(AValue: Single);
    procedure WriteSmallInt(AValue: SmallInt);
    procedure WriteVariant(const AValue: Variant);
    procedure WriteWideString(const S: WideString);
    procedure WriteWord(AValue: Word);
  end;

  { TdxVariantList }

  TdxVariantList = class(TList<Variant>)
  public
    constructor Create;
  end;

  { TdxVariantListComparer }

  TdxVariantListComparer = class(TInterfacedObject, IComparer<Variant>)
  public
    function Compare(const Left, Right: Variant): Integer;
  end;

  { TdxVariantComparer }

  TdxVariantComparer = class(TEqualityComparer<Variant>)
  public
    function Equals(const Left, Right: Variant): Boolean; override;
    function GetHashCode(const Value: Variant): Integer; override;
  end;

  { TdxVariantToObjectDictionary }

  TdxVariantDictionary<T> = class(TDictionary<Variant, T>)
  public
    constructor Create(ACapacity: Integer = 0);
  end;

  { TdxVariantToObjectDictionary }

  TdxVariantToObjectDictionary<TValue: class> = class(TObjectDictionary<Variant, TValue>)
  public
    constructor Create(AOwnsValues: Boolean; ACapacity: Integer = 0);
  end;

function VarCompare(const V1, V2: Variant): Integer;
function VarEquals(const V1, V2: Variant): Boolean;
function VarEqualsExact(const V1, V2: Variant): Boolean;
function VarEqualsSoft(const V1, V2: Variant): Boolean;
function VarIndex(const AList: TVariantArray; const AValue: Variant): Integer;
function VarIsDate(const AValue: Variant): Boolean;
function VarIsNumericEx(const AValue: Variant): Boolean;
function VarIsSoftEmpty(const AValue: Variant): Boolean;
function VarIsSoftNull(const AValue: Variant): Boolean;
function VarToStrEx(const V: Variant): string;
function VarTypeIsCurrency(AVarType: TVarType): Boolean;
function VarBetweenArrayCreate(const AValue1, AValue2: Variant): Variant;
function VarListArrayCreate(const AValue: Variant): Variant;
procedure VarListArrayAddValue(var Value: Variant; const AValue: Variant);

function ReadAnsiStringFunc(AStream: TStream): AnsiString;
procedure ReadAnsiStringProc(AStream: TStream; var S: AnsiString);
procedure WriteAnsiStringProc(AStream: TStream; const S: AnsiString);

function ReadWideStringFunc(AStream: TStream): WideString;
procedure ReadWideStringProc(AStream: TStream; var S: WideString);
procedure WriteWideStringProc(AStream: TStream; const S: WideString);

function ReadVariantFunc(AStream: TStream): Variant;
procedure ReadVariantProc(AStream: TStream; var Value: Variant);
procedure WriteVariantProc(AStream: TStream; const AValue: Variant);

function ReadBooleanFunc(AStream: TStream): Boolean;
procedure ReadBooleanProc(AStream: TStream; var Value: Boolean);
procedure WriteBooleanProc(AStream: TStream; AValue: Boolean);

function ReadCharFunc(AStream: TStream): Char;
procedure ReadCharProc(AStream: TStream; var Value: Char);
procedure WriteCharProc(AStream: TStream; AValue: Char);

function ReadFloatFunc(AStream: TStream): Extended;
procedure ReadFloatProc(AStream: TStream; var Value: Extended);
procedure WriteFloatProc(AStream: TStream; AValue: Extended);

function ReadSingleFunc(AStream: TStream): Single;
procedure ReadSingleProc(AStream: TStream; var Value: Single);
procedure WriteSingleProc(AStream: TStream; AValue: Single);

function ReadCurrencyFunc(AStream: TStream): Currency;
procedure ReadCurrencyProc(AStream: TStream; var Value: Currency);
procedure WriteCurrencyProc(AStream: TStream; AValue: Currency);

function ReadDateTimeFunc(AStream: TStream): TDateTime;
procedure ReadDateTimeProc(AStream: TStream; var Value: TDateTime);
procedure WriteDateTimeProc(AStream: TStream; AValue: TDateTime);

function ReadIntegerFunc(AStream: TStream): Integer;
procedure ReadIntegerProc(AStream: TStream; var Value: Integer);
procedure WriteIntegerProc(AStream: TStream; AValue: Integer);

function ReadLargeIntFunc(AStream: TStream): LargeInt;
procedure ReadLargeIntProc(AStream: TStream; var Value: LargeInt);
procedure WriteLargeIntProc(AStream: TStream; AValue: LargeInt);

function ReadByteFunc(AStream: TStream): Byte;
procedure ReadByteProc(AStream: TStream; var Value: Byte);
procedure WriteByteProc(AStream: TStream; AValue: Byte);

function ReadSmallIntFunc(AStream: TStream): SmallInt;
procedure ReadSmallIntProc(AStream: TStream; var Value: SmallInt);
procedure WriteSmallIntProc(AStream: TStream; AValue: SmallInt);

function ReadCardinalFunc(AStream: TStream): Cardinal;
procedure ReadCardinalProc(AStream: TStream; var Value: Cardinal);
procedure WriteCardinalProc(AStream: TStream; AValue: Cardinal);

function ReadShortIntFunc(AStream: TStream): ShortInt;
procedure ReadShortIntProc(AStream: TStream; var Value: ShortInt);
procedure WriteShortIntProc(AStream: TStream; AValue: ShortInt);

function ReadWordFunc(AStream: TStream): Word;
procedure ReadWordProc(AStream: TStream; var Value: Word);
procedure WriteWordProc(AStream: TStream; AValue: Word);

procedure ReadInt64Proc(AStream: TStream; var Value: Int64);
procedure WriteInt64Proc(AStream: TStream; AValue: Int64);

function ReadBytesFunc(AStream: TStream; ACount: Integer): TBytes;
procedure WriteBytesProc(AStream: TStream; ABytes: TBytes);

function GetVariantHash(const V: Variant): Cardinal;

implementation

uses
{$IFNDEF NONDB}
  FMTBcd, SqlTimSt,
{$ENDIF}
  Windows, VarUtils, cxDataConsts, dxHash, dxHashUtils;

function VarArrayCompare(const V1, V2: Variant): Integer;
var
  I: Integer;
begin
  if VarIsArray(V1) and VarIsArray(V2) then
  begin
    Result := VarArrayHighBound(V1, 1) - VarArrayHighBound(V2, 1);
    if Result = 0 then
    begin
      for I := 0 to VarArrayHighBound(V1, 1) do
      begin
        Result := VarCompare(V1[I], V2[I]);
        if Result <> 0 then
          Break;
      end;
    end;
  end
  else
    if VarIsArray(V1) then
      Result := 1
    else
      if VarIsArray(V2) then
        Result := -1
      else
        Result := VarCompare(V1, V2);
end;

function VarCompare(const V1, V2: Variant): Integer;

  function CompareValues(const V1, V2: Variant): Integer;
  begin
    try
      if VarIsEmpty(V1) then
        if VarIsEmpty(V2) then
          Result := 0
        else
          Result := -1
      else
        if VarIsEmpty(V2) then
          Result := 1
        else
          if V1 = V2 then
            Result := 0
          else
            if VarIsNull(V1) then
              Result := -1
            else
              if VarIsNull(V2) then
                Result := 1
              else
                if V1 < V2 then
                  Result := -1
                else
                  Result := 1;
    except
      on EVariantError do
        Result := -1;
    end;
  end;

begin
  if VarIsArray(V1) or VarIsArray(V2) then
    Result := VarArrayCompare(V1, V2)
  else
    Result := CompareValues(V1, V2);
end;

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := VarCompare(V1, V2) = 0;
end;

function VarEqualsExact(const V1, V2: Variant): Boolean;
var
  AVarType1, AVarType2: Integer;
  AValue1, AValue2: Variant;
begin
  AVarType1 := VarType(V1);
  AVarType2 := VarType(V2);
  if (AVarType1 = varNull) or (AVarType2 = varNull) or
    ((AVarType1 <> varBoolean) and (AVarType2 <> varBoolean)) then
    Result := VarEquals(V1, V2)
  else
    try
      VarCast(AValue1, V1, varUString);
      VarCast(AValue2, V2, varUString);
      Result := AValue1 = AValue2;
    except
      on EVariantError do
        Result := False;
    end;
end;

function VarEqualsSoft(const V1, V2: Variant): Boolean;
begin
  Result := VarEquals(V1, V2) or (VarIsSoftNull(V1) and VarIsSoftNull(V2));
end;

function VarIndex(const AList: TVariantArray; const AValue: Variant): Integer;
begin
  for Result := 0 to Length(AList) - 1 do
    if VarEquals(AList[Result], AValue) then Exit;
  Result := -1;
end;

function VarIsDate(const AValue: Variant): Boolean;

  function VarTypeIsDate(const AVarType: TVarType): Boolean;
  begin
    Result := (AVarType = varDate)
      {$IFNDEF NONDB} or (AVarType = VarSQLTimeStamp){$ENDIF};
  end;

begin
  Result := VarTypeIsDate(FindVarData(AValue)^.VType);
end;

function VarIsNumericEx(const AValue: Variant): Boolean;
begin
  Result := VarIsNumeric(AValue)
    {$IFNDEF NONDB} or (FindVarData(AValue)^.VType = VarFMTBcd){$ENDIF};
end;

function VarIsSoftNull(const AValue: Variant): Boolean;
begin
  Result := VarIsNull(AValue) or
    ({(VarType(AValue) = varString)}VarIsStr(AValue) and (AValue = ''));
end;

function VarIsSoftEmpty(const AValue: Variant): Boolean;
begin
  Result := VarType(AValue) in [varNull, varEmpty];
end;

function VarToStrEx(const V: Variant): string;
begin
  Result := VarToStr(V);
end;

function VarTypeIsCurrency(AVarType: TVarType): Boolean;
begin
  Result := (AVarType = varCurrency)
    {$IFNDEF NONDB} or (AVarType = VarFMTBcd){$ENDIF};
end;

function VarBetweenArrayCreate(const AValue1, AValue2: Variant): Variant;
begin
  Result := VarArrayCreate([0, 1], varVariant);
  Result[0] := AValue1;
  Result[1] := AValue2;
end;

function VarListArrayCreate(const AValue: Variant): Variant;
begin
  Result := VarArrayCreate([0, 0], varVariant);
  Result[0] := AValue;
end;

procedure VarListArrayAddValue(var Value: Variant; const AValue: Variant);
var
  V: Variant;
  I, C: Integer;
begin
  C := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 2;
  V := VarArrayCreate([0, C - 1], varVariant);
  for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
    V[I] := Value[I];
  V[C - 1] := AValue;
  Value := V;
end;

// Stream routines

function ReadAnsiStringFunc(AStream: TStream): AnsiString;
begin
  ReadAnsiStringProc(AStream, Result);
end;

procedure ReadAnsiStringProc(AStream: TStream; var S: AnsiString);
var
  L: Integer;
begin
  AStream.ReadBuffer(L, SizeOf(L));
  SetLength(S, L);
  AStream.ReadBuffer(Pointer(S)^, L);
end;

procedure WriteAnsiStringProc(AStream: TStream; const S: AnsiString);
var
  L: Integer;
begin
  L := Length(S);
  AStream.WriteBuffer(L, SizeOf(L));
  AStream.WriteBuffer(S[1], L);
end;

function ReadWideStringFunc(AStream: TStream): WideString;
begin
  ReadWideStringProc(AStream, Result);
end;

procedure ReadWideStringProc(AStream: TStream; var S: WideString);
var
  L: Integer;
begin
  AStream.ReadBuffer(L, SizeOf(L));
  SetLength(S, L);
  AStream.ReadBuffer(Pointer(S)^, L * 2);
end;

procedure WriteWideStringProc(AStream: TStream; const S: WideString);
var
  L: Integer;
begin
  L := Length(S);
  AStream.WriteBuffer(L, SizeOf(L));
  AStream.WriteBuffer(Pointer(S)^, L * 2);
end;

function ReadVariantFunc(AStream: TStream): Variant;
begin
  ReadVariantProc(AStream, Result);
end;

procedure ReadVariantProc(AStream: TStream; var Value: Variant);
const
  ValToVarT: array[TValueType] of Integer = (varNull, varError,
    varShortInt, varSmallInt, varInteger, varDouble, varString, varError, varBoolean,
    varBoolean, varError, varError, varString, varEmpty, varError, varSingle,
    varCurrency, varDate, varOleStr, varInt64, varError, varDouble);
var
  ValType: TValueType;

  function ReadValue: TValueType;
  var
    B: Byte;
  begin
    AStream.ReadBuffer(B, SizeOf(Byte));
    Result := TValueType(B);
  end;

  function ReadInteger: LargeInt;
  var
    SH: Shortint;
    SM: Smallint;
    I: Integer;
  begin
    case ValType of
      vaInt8:
        begin
          AStream.ReadBuffer(SH, SizeOf(SH));
          Result := SH;
        end;
      vaInt16:
        begin
          AStream.ReadBuffer(SM, SizeOf(SM));
          Result := SM;
        end;
      vaInt32:
        begin
          AStream.ReadBuffer(I, SizeOf(I));
          Result := I;
        end
    else  // vaInt64
      AStream.ReadBuffer(Result, SizeOf(Result));
    end;
  end;

  function ReadFloat: Extended;
  begin
    ReadFloatProc(AStream, Result);
  end;

  function ReadSingle: Single;
  begin
    AStream.ReadBuffer(Result, SizeOf(Result));
  end;

  function ReadCurrency: Currency;
  begin
    ReadCurrencyProc(AStream, Result);
  end;

  function ReadDate: TDateTime;
  begin
    ReadDateTimeProc(AStream, Result);
  end;

  function ReadAnsiString: AnsiString;
  var
    L: Integer;
  begin
    L := 0;
    case ValType of
      vaString:
        AStream.ReadBuffer(L, SizeOf(Byte));
    else {vaLString}
      AStream.ReadBuffer(L, SizeOf(Integer));
    end;
    SetLength(Result, L);
    AStream.ReadBuffer(Pointer(Result)^, L);
  end;

  function ReadWideString: WideString;
  begin
    ReadWideStringProc(AStream, Result);
  end;

  procedure ReadArrayProc(var Value: Variant);
  var
    I, C: Integer;
    V: Variant;
  begin
    // read size
    ValType := ReadValue; // len
    C := ReadInteger;
    // read values
    Value := VarArrayCreate([0, C - 1], varVariant);
    for I := 0 to C - 1 do
    begin
      ReadVariantProc(AStream, V);
      Value[I] := V;
    end;
  end;

begin
  Value := Null; //todo: need finalize previous value
  ValType := ReadValue;
  if ValType = vaList then
  begin
    ReadArrayProc(Value);
    Exit;
  end;
  case ValType of
    vaNil:
      VarClear(Value);
    vaNull:
      Value := Null;
    vaInt8:
      TVarData(Value).VShortInt := ShortInt(ReadInteger);
    vaInt16:
      TVarData(Value).VSmallint := Smallint(ReadInteger);
    vaInt32:
      TVarData(Value).VInteger := ReadInteger;
    vaInt64:
      TVarData(Value).VInt64 := ReadInteger;
    vaExtended:
      TVarData(Value).VDouble := ReadFloat;
    vaString, vaLString:
      Value := ReadAnsiString;
    vaFalse, vaTrue:
      TVarData(Value).VBoolean := ValType = vaTrue;
    vaWString:
      Value := ReadWideString;
    vaSingle:
      TVarData(Value).VSingle := ReadSingle;
    vaCurrency:
      TVarData(Value).VCurrency := ReadCurrency;
    vaDate:
      TVarData(Value).VDate := ReadDate;
  else
    raise EReadError.Create(cxSDataReadError);
  end;
  TVarData(Value).VType := ValToVarT[ValType];
end;

procedure WriteVariantProc(AStream: TStream; const AValue: Variant);

  procedure WriteValue(Value: TValueType);
  begin
    AStream.WriteBuffer(Byte(Value), SizeOf(Byte));
  end;

  procedure WriteInteger(Value: LargeInt);
  var
    SH: Shortint;
    SM: Smallint;
    I: Integer;
  begin
    if (Value >= Low(ShortInt)) and (Value <= High(ShortInt)) then
    begin
      WriteValue(vaInt8);
      SH := Value;
      AStream.WriteBuffer(SH, SizeOf(SH));
    end
    else
      if (Value >= Low(SmallInt)) and (Value <= High(SmallInt)) then
      begin
        WriteValue(vaInt16);
        SM := Value;
        AStream.WriteBuffer(SM, SizeOf(SM));
      end
      else
        if (Value >= Low(Integer)) and (Value <= High(Integer)) then
        begin
          WriteValue(vaInt32);
          I := Value;
          AStream.WriteBuffer(I, SizeOf(I));
        end
        else
        begin
          WriteValue(vaInt64);
          AStream.WriteBuffer(Value, SizeOf(Value));
        end;
  end;

  procedure WriteAnsiString(const Value: AnsiString);
  var
    B: Byte;
    L: Integer;
  begin
    L := Length(Value);
    if L <= 255 then
    begin
      WriteValue(vaString);
      B := L;
      AStream.WriteBuffer(B, SizeOf(B));
    end
    else
    begin
      WriteValue(vaLString);
      AStream.WriteBuffer(L, SizeOf(L));
    end;
    AStream.WriteBuffer(Pointer(Value)^, L);
  end;

  procedure WriteFloat(const Value: Extended);
  begin
    WriteValue(vaExtended);
    WriteFloatProc(AStream, Value);
  end;

  procedure WriteSingle(const Value: Single);
  begin
    WriteValue(vaSingle);
    AStream.WriteBuffer(Value, SizeOf(Single));
  end;

  procedure WriteCurrency(const Value: Currency);
  begin
    WriteValue(vaCurrency);
    WriteCurrencyProc(AStream, Value);
  end;

  procedure WriteDate(const Value: TDateTime);
  begin
    WriteValue(vaDate);
    WriteDateTimeProc(AStream, Value);
  end;

  procedure WriteWideString(const Value: WideString);
  begin
    WriteValue(vaWString);
    WriteWideStringProc(AStream, Value);
  end;

  procedure WriteArrayProc(const Value: Variant);
  var
    I, L, H: Integer;
  begin
    if VarArrayDimCount(Value) <> 1 then
      raise EWriteError.Create(cxSDataWriteError);
    L := VarArrayLowBound(Value, 1);
    H := VarArrayHighBound(Value, 1);
    WriteValue(vaList);
    WriteInteger(H - L + 1);
    for I := L to H do
      WriteVariantProc(AStream, Value[I]);
  end;

var
  VType: Integer;
begin
  if VarIsArray(AValue) then
  begin
    WriteArrayProc(AValue);
    Exit;
  end;
  VType := VarType(AValue);
  case VType and varTypeMask of
    varEmpty:
      WriteValue(vaNil);
    varNull:
      WriteValue(vaNull);
    varString:
      WriteAnsiString(dxVariantToAnsiString(AValue));
    varShortInt, varWord, varLongWord, varInt64,
    varByte, varSmallInt, varInteger:
      WriteInteger(AValue);
    varDouble:
      WriteFloat(AValue);
    varBoolean:
      if AValue then
        WriteValue(vaTrue)
      else
        WriteValue(vaFalse);
    varUString, varOleStr:
      WriteWideString(AValue);
    varSingle:
      WriteSingle(AValue);
    varCurrency:
      WriteCurrency(AValue);
    varDate:
      WriteDate(AValue);
  else
  {$IFNDEF NONDB}
    if VType = VarSQLTimeStamp then
      WriteVariantProc(AStream, TDateTime(AValue))
    else
  {$ENDIF}
    try
      WriteAnsiString(dxVariantToAnsiString(AValue));
    except
      raise EWriteError.Create(cxSDataWriteError);
    end;
  end;
end;

function ReadBooleanFunc(AStream: TStream): Boolean;
begin
  ReadBooleanProc(AStream, Result);
end;

procedure ReadBooleanProc(AStream: TStream; var Value: Boolean);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteBooleanProc(AStream: TStream; AValue: Boolean);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadCharFunc(AStream: TStream): Char;
begin
  ReadCharProc(AStream, Result);
end;

procedure ReadCharProc(AStream: TStream; var Value: Char);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteCharProc(AStream: TStream; AValue: Char);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadFloatFunc(AStream: TStream): Extended;
begin
  ReadFloatProc(AStream, Result);
end;

procedure ReadFloatProc(AStream: TStream; var Value: Extended);
{$IFDEF CPUX64}
var
  AData: TExtended80Rec;
begin
  AStream.ReadBuffer(AData, SizeOf(AData));
  Value := Extended(AData);
{$ELSE}
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
{$ENDIF}
end;

procedure WriteFloatProc(AStream: TStream; AValue: Extended);
{$IFDEF CPUX64}
var
  AData: TExtended80Rec;
begin
  AData := TExtended80Rec(AValue);
  AStream.WriteBuffer(AData, SizeOf(AData));
{$ELSE}
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
{$ENDIF}
end;

function ReadSingleFunc(AStream: TStream): Single;
begin
  ReadSingleProc(AStream, Result);
end;

procedure ReadSingleProc(AStream: TStream; var Value: Single);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteSingleProc(AStream: TStream; AValue: Single);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadCurrencyFunc(AStream: TStream): Currency;
begin
  ReadCurrencyProc(AStream, Result);
end;

procedure ReadCurrencyProc(AStream: TStream; var Value: Currency);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteCurrencyProc(AStream: TStream; AValue: Currency);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadDateTimeFunc(AStream: TStream): TDateTime;
begin
  ReadDateTimeProc(AStream, Result);
end;

procedure ReadDateTimeProc(AStream: TStream; var Value: TDateTime);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteDateTimeProc(AStream: TStream; AValue: TDateTime);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadIntegerFunc(AStream: TStream): Integer;
begin
  ReadIntegerProc(AStream, Result);
end;

procedure ReadInt64Proc(AStream: TStream; var Value: Int64);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure ReadIntegerProc(AStream: TStream; var Value: Integer);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteInt64Proc(AStream: TStream; AValue: Int64);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure WriteIntegerProc(AStream: TStream; AValue: Integer);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadLargeIntFunc(AStream: TStream): LargeInt;
begin
  ReadLargeIntProc(AStream, Result);
end;

procedure ReadLargeIntProc(AStream: TStream; var Value: LargeInt);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteLargeIntProc(AStream: TStream; AValue: LargeInt);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadByteFunc(AStream: TStream): Byte;
begin
  ReadByteProc(AStream, Result);
end;

procedure ReadByteProc(AStream: TStream; var Value: Byte);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteByteProc(AStream: TStream; AValue: Byte);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadSmallIntFunc(AStream: TStream): SmallInt;
begin
  ReadSmallIntProc(AStream, Result);
end;

procedure ReadSmallIntProc(AStream: TStream; var Value: SmallInt);
begin
  AStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure WriteSmallIntProc(AStream: TStream; AValue: SmallInt);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

function ReadCardinalFunc(AStream: TStream): Cardinal;
begin
  ReadCardinalProc(AStream, Result);
end;

procedure ReadCardinalProc(AStream: TStream; var Value: Cardinal);
begin
  Value := ReadIntegerFunc(AStream);
end;

procedure WriteCardinalProc(AStream: TStream; AValue: Cardinal);
begin
  WriteIntegerProc(AStream, AValue);
end;

function ReadShortIntFunc(AStream: TStream): ShortInt;
begin
  ReadShortIntProc(AStream, Result);
end;

procedure ReadShortIntProc(AStream: TStream; var Value: ShortInt);
begin
  Value := ReadByteFunc(AStream);
end;

procedure WriteShortIntProc(AStream: TStream; AValue: ShortInt);
begin
  WriteByteProc(AStream, AValue);
end;

function ReadWordFunc(AStream: TStream): Word;
begin
  ReadWordProc(AStream, Result);
end;

procedure ReadWordProc(AStream: TStream; var Value: Word);
begin
  Value := ReadSmallIntFunc(AStream);
end;

procedure WriteWordProc(AStream: TStream; AValue: Word);
begin
  WriteSmallIntProc(AStream, AValue);
end;

function ReadBytesFunc(AStream: TStream; ACount: Integer): TBytes;
begin
  SetLength(Result, ACount);
  if ACount > 0 then
    AStream.ReadBuffer(Result[0], ACount);
end;

procedure WriteBytesProc(AStream: TStream; ABytes: TBytes);
var
  ACount: Integer;
begin
  ACount := Length(ABytes);
  if ACount > 0 then
    AStream.WriteBuffer(ABytes[0], ACount);
end;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
function GetVarDataArrayInfo(const AVarData: TVarData; out AVarType: TVarType;
  out AVarArray: PVarArray): Boolean;
begin
  if AVarData.VType = varByRef or varVariant then
    Result := GetVarDataArrayInfo(PVarData(AVarData.VPointer)^, AVarType, AVarArray)
  else
  begin
    AVarType := AVarData.VType;
    Result := (AVarType and varArray) <> 0;
    if Result then
      if (AVarType and varByRef) <> 0 then
        AVarArray := PVarArray(AVarData.VPointer^)
      else
        AVarArray := AVarData.VArray
    else
      AVarArray := nil;
  end;
end;

function AnsiElfHash(P: PAnsiChar): Cardinal;
var
  I: Cardinal;
begin
  Result := 0;
  if P = nil then
    Exit;
  while P^ <> #$00 do
  begin
    Result := (Result shl 4) + Ord(P^);
    I := Result and $F0000000;
    if (I <> 0) then
      Result := Result xor (I shr 24);
    Result := Result and (not I);
    Inc(P);
  end;
end;

function GetDateTimeHash(const ADateTime: TDateTime): Cardinal;
begin
  Result := $FFFFFFFF and Int64(Trunc(ADateTime * SecsPerDay));
end;

function GetBcdDigit(const AValue: TBcd; ADigit: Integer): Integer; inline;
begin
  if Odd(ADigit) then
    Result := (AValue.Fraction[ADigit shr 1]) and $F
  else
    Result := (AValue.Fraction[ADigit shr 1]) shr 4;
end;

function BcdToCardinal(const ASource: TBcd; out ADest: Cardinal): Boolean;
var
  T: Int64;
  I, ADecPos, ANibble: Integer;
begin
  if (ASource.SignSpecialPlaces and $40) <> 0 then
  begin
    ADest := 0;
    Result := True;
  end
  else
  begin
    Result := False;
    ADecPos := ASource.Precision - (ASource.SignSpecialPlaces and $3F);
    for I := ASource.Precision - 1 downto ADecPos do
      if GetBcdDigit(ASource, I) <> 0 then
        Exit;
    T := 0;
    for I := 0 to ADecPos - 1 do
    begin
      ANibble := GetBcdDigit(ASource, I);
      if (ANibble = 0) and (Int64Rec(T).Lo = 0) then
        Continue;
      T := T * 10 + ANibble;
      if Int64Rec(T).Hi <> 0 then
        Exit;
    end;
    ADest := Int64Rec(T).Lo;
    Result := True;
  end;
end;

function GetBcdHash(const Value: Variant): Cardinal;
var
  Bcd: TBcd;
begin
  Bcd := VarToBcd(Value);
  if not BcdToCardinal(Bcd, Result) then
    Result := AnsiElfHash(PAnsiChar(AnsiString(Value)));
end;

function GetVarDataHash(AVarType: TVarType; const AData: TVarData): Cardinal;
begin
  case AVarType of
    varInteger:
      Result := AData.VInteger;
    varString:
      Result := AnsiElfHash(PAnsiChar(AData.VString));
    varUString:
      Result := Cardinal(dxElfHash(PWideChar(AData.VUString), 0, nil, 0));
    varUInt64:
      Result := Int64Rec(AData.VUInt64).Lo xor Int64Rec(AData.VUInt64).Hi;
    varOleStr:
      Result := Cardinal(dxElfHash(AData.VOleStr, 0, nil, 0));
    varSingle:
      Move(AData.VSingle, Result, SizeOf(Single));
    varDouble:
      Result := Int64Rec(AData.VDouble).Lo xor Int64Rec(AData.VDouble).Hi;
    varCurrency:
      Result := Int64Rec(AData.VCurrency).Lo xor Int64Rec(AData.VCurrency).Hi;
    varDate:
      Result := GetDateTimeHash(AData.VDate);
    varBoolean:
      Result := Ord(AData.VBoolean);
    varShortInt:
      Result := AData.VShortInt;
    varByte:
      Result := AData.VByte;
    varWord:
      Result := AData.VWord;
    varLongWord:
      Result := AData.VLongWord;
    varInt64:
      Result := Int64Rec(AData.VInt64).Lo xor Int64Rec(AData.VInt64).Hi;
    varSmallInt:
      Result := AData.VSmallInt;
    varNull:
      Result := 0;
    varEmpty:
      Result := 1;
  else
    if (AVarType = VarSQLTimeStamp) or (AVarType = varSQLTimeStampOffset) then
    begin
      Result := GetDateTimeHash(Variant(AData));
      Exit;
    end
    else
      if AVarType = VarFMTBcd then
      begin
        Result := GetBcdHash(Variant(AData));
        Exit;
      end;
    raise Exception.Create('Wrong argument for hashing!');
    end;
end;

function GetVariantHash(const V: Variant): Cardinal;
const
  MagicNumber = $5bd1e995;
var
  I, AIndex: Integer;
  AHash: Cardinal;
  AVarType: TVarType;
  AVarArray: PVarArray;
  AData: TVarData;
  P: Pointer;
begin
  if GetVarDataArrayInfo(TVarData(V), AVarType, AVarArray) then
  begin
    Result := 0;
    if AVarArray.DimCount <> 1 then
      raise Exception.Create('Wrong variant array dimension for hashing!');
    for I := AVarArray.Bounds[0].LowBound to AVarArray.Bounds[0].ElementCount -1 do
    begin

      AVarType := AVarType and varTypeMask;
      AIndex := I;
      if AVarType = varVariant then
      begin
        VarResultCheck(SafeArrayPtrOfIndex(AVarArray, @AIndex, P));
        AHash := GetVarDataHash(PVarData(P).VType, PVarData(P)^);
      end
      else
      begin
        AData.VType := AVarType;
        FillChar(AData.VBytes, SizeOf(AData.VBytes), 0);
        VarResultCheck(SafeArrayGetElement(AVarArray, @AIndex, @AData.VPointer));
        AHash := GetVarDataHash(AVarType, AData);
      end;
      if AVarArray.Bounds[0].ElementCount = 1 then
        Result := AHash
      else
      begin
        AHash  := AHash xor ((AHash * MagicNumber) shr 24);
        AHash  := Cardinal(AHash * MagicNumber);
        Result := Cardinal(Result * MagicNumber) xor AHash;
      end;
    end;
  end
  else
    Result := GetVarDataHash(AVarType, TVarData(V));
end;

{ TcxFiler }

constructor TcxFiler.Create(AStream: TStream);
begin
  Create(AStream, 0);
end;

constructor TcxFiler.Create(AStream: TStream; AVersion: Cardinal);
begin
  inherited Create;
  FStream := AStream;
  FVersion := AVersion;
  FIsUnicode := dxIsUnicodeStream(AStream);
end;

{ TcxReader }

function TcxReader.ReadAnsiString: AnsiString;
begin
  ReadAnsiStringProc(Stream, Result);
end;

function TcxReader.ReadBoolean: Boolean;
begin
  ReadBooleanProc(Stream, Result);
end;

function TcxReader.ReadByte: Byte;
begin
  ReadByteProc(Stream, Result);
end;

function TcxReader.ReadCardinal: Cardinal;
begin
  ReadCardinalProc(Stream, Result);
end;

function TcxReader.ReadChar: Char;
begin
  ReadCharProc(Stream, Result);
end;

function TcxReader.ReadCurrency: Currency;
begin
  ReadCurrencyProc(Stream, Result);
end;

function TcxReader.ReadDateTime: TDateTime;
begin
  ReadDateTimeProc(Stream, Result);
end;

function TcxReader.ReadFloat: Extended;
begin
  ReadFloatProc(Stream, Result);
end;

function TcxReader.ReadInt64: Int64;
begin
  ReadInt64Proc(Stream, Result);
end;

function TcxReader.ReadInteger: Integer;
begin
  ReadIntegerProc(Stream, Result);
end;

function TcxReader.ReadLargeInt: LargeInt;
begin
  ReadLargeIntProc(Stream, Result);
end;

procedure TcxReader.ReadMemoryStream(AStream: TMemoryStream);
begin
  AStream.Size := ReadInteger;
  Stream.ReadBuffer(AStream.Memory^, AStream.Size);
end;

function TcxReader.ReadPoint: TPoint;
begin
  Result.X := ReadInteger;
  Result.Y := ReadInteger;
end;

function TcxReader.ReadRect: TRect;
begin
  Result.Left := ReadInteger;
  Result.Top := ReadInteger;
  Result.Right := ReadInteger;
  Result.Bottom := ReadInteger;
end;

function TcxReader.ReadShortInt: ShortInt;
begin
  ReadShortIntProc(Stream, Result);
end;

function TcxReader.ReadSingle: Single;
begin
  ReadSingleProc(Stream, Result);
end;

function TcxReader.ReadSmallInt: SmallInt;
begin
  ReadSmallIntProc(Stream, Result);
end;

function TcxReader.ReadString_: string;
begin
  Result := dxReadStr(Stream, IsUnicode);
end;

function TcxReader.ReadVariant: Variant;
begin
  ReadVariantProc(Stream, Result);
end;

function TcxReader.ReadWideString: WideString;
begin
  ReadWideStringProc(Stream, Result);
end;

function TcxReader.ReadWord: Word;
begin
  ReadWordProc(Stream, Result);
end;

{ TcxWriter }

procedure TcxWriter.BeginChunk(out APosition: Int64);
begin
  WriteCardinal(0);
  APosition := Stream.Position;
end;

procedure TcxWriter.EndChunk(const APosition: Int64);
var
  ASavedPos: Int64;
begin
  ASavedPos := Stream.Position;
  try
    Stream.Position := APosition - SizeOf(Cardinal);
    WriteCardinal(ASavedPos - APosition);
  finally
    Stream.Position := ASavedPos;
  end;
end;

procedure TcxWriter.WriteAnsiString(const S: AnsiString);
begin
  WriteAnsiStringProc(Stream, S);
end;

procedure TcxWriter.WriteBoolean(AValue: Boolean);
begin
  WriteBooleanProc(Stream, AValue);
end;

procedure TcxWriter.WriteByte(AValue: Byte);
begin
  WriteByteProc(Stream, AValue);
end;

procedure TcxWriter.WriteCardinal(AValue: Cardinal);
begin
  WriteCardinalProc(Stream, AValue);
end;

procedure TcxWriter.WriteChar(AValue: Char);
begin
  WriteCharProc(Stream, AValue);
end;

procedure TcxWriter.WriteCurrency(AValue: Currency);
begin
  WriteCurrencyProc(Stream, AValue);
end;

procedure TcxWriter.WriteDateTime(AValue: TDateTime);
begin
  WriteDateTimeProc(Stream, AValue);
end;

procedure TcxWriter.WriteFloat(AValue: Extended);
begin
  WriteFloatProc(Stream, AValue);
end;

procedure TcxWriter.WriteInt64(AValue: Int64);
begin
  WriteInt64Proc(Stream, AValue);
end;

procedure TcxWriter.WriteInteger(AValue: Integer);
begin
  WriteIntegerProc(Stream, AValue);
end;

procedure TcxWriter.WriteLargeInt(AValue: LargeInt);
begin
  WriteLargeIntProc(Stream, AValue);
end;

procedure TcxWriter.WriteMemoryStream(AStream: TMemoryStream);
begin
  WriteInteger(AStream.Size);
  Stream.WriteBuffer(AStream.Memory^, AStream.Size);
end;

procedure TcxWriter.WritePoint(const AValue: TPoint);
begin
  WriteInteger(AValue.X);
  WriteInteger(AValue.Y);
end;

procedure TcxWriter.WriteRect(const AValue: TRect);
begin
  WriteInteger(AValue.Left);
  WriteInteger(AValue.Top);
  WriteInteger(AValue.Right);
  WriteInteger(AValue.Bottom);
end;

procedure TcxWriter.WriteShortInt(AValue: ShortInt);
begin
  WriteShortIntProc(Stream, AValue);
end;

procedure TcxWriter.WriteSingle(AValue: Single);
begin
  WriteSingleProc(Stream, AValue);
end;

procedure TcxWriter.WriteSmallInt(AValue: SmallInt);
begin
  WriteSmallIntProc(Stream, AValue);
end;

procedure TcxWriter.WriteVariant(const AValue: Variant);
begin
  WriteVariantProc(Stream, AValue);
end;

procedure TcxWriter.WriteWideString(const S: WideString);
begin
  WriteWideStringProc(Stream, S);
end;

procedure TcxWriter.WriteWord(AValue: Word);
begin
  WriteWordProc(Stream, AValue);
end;

{ TdxVariantList }

constructor TdxVariantList.Create;
var
  AIntf: IComparer<Variant>;
begin
  AIntf := TdxVariantListComparer.Create;
  inherited Create(AIntf);
end;

{ TdxVariantListComparer }

function TdxVariantListComparer.Compare(const Left, Right: Variant): Integer;
begin
  if VarIsStr(Left) or VarIsStr(Right) then
    Result := CompareStr(Left, Right)
  else
    Result := VarCompare(Left, Right);
end;

{ TdxVariantComparer }

function TdxVariantComparer.Equals(const Left, Right: Variant): Boolean;
begin
  Result := Left = Right;
end;

function TdxVariantComparer.GetHashCode(const Value: Variant): Integer;
begin
  Result := Integer(GetVariantHash(Value));
end;

  { TdxVariantDictionary }

constructor TdxVariantDictionary<T>.Create(ACapacity: Integer = 0);
begin
  inherited Create(ACapacity, TdxVariantComparer.Create);
end;

{ TdxVariantToObjectDictionary }

constructor TdxVariantToObjectDictionary<TValue>.Create(AOwnsValues: Boolean; ACapacity: Integer = 0);
var
  AOwnerships: TDictionaryOwnerships;
begin
  if AOwnsValues then
    AOwnerships := [doOwnsValues]
  else
    AOwnerships := [];
  inherited Create(AOwnerships, ACapacity, TdxVariantComparer.Create);
end;

end.
