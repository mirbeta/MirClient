(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)

{$IFDEF INTERFACE}
type
  TCharSet = set of StdChar;

procedure FreeAndNil(var Obj);
function Min(const Value1, Value2: LongInt): LongInt; overload; inline;
function Min(const Value1, Value2: Int64): Int64; overload; inline;
function Min(const Value1, Value2: Single): Single; overload; inline;
function Min(const Value1, Value2: Double): Double; overload; inline;
function Max(const Value1, Value2: LongInt): LongInt; overload; inline;
function Max(const Value1, Value2: Int64): Int64; overload; inline;
function Max(const Value1, Value2: Single): Single; overload; inline;
function Max(const Value1, Value2: Double): Double; overload; inline;
function Hypot(const Value1, Value2: Extended): Extended; overload;
function ArcTan2(const Y, X: Extended): Extended;
function ArcSin(const Value: Extended): Extended;
function ArcCos(const Value: Extended): Extended;
procedure SinCos(const Angle: Single; out SinValue, CosValue: Single); overload;
procedure SinCos(const Angle: Double; out SinValue, CosValue: Double); overload;
function Cot(const Value: Extended): Extended;
function IntToStr(const Value: LongInt): StdString;
function IntToHex(Value, Digits: Integer): StdString;
function PosEx(const Ch: StdChar; const Text: StdString; const StartPos: Integer): Integer;
function StrToIntDef(const Text: StdString; const DefaultValue: LongInt): LongInt;

function StrPCopy(const Dest: PStdChar; const Source: StdString; const MaxLength: Integer = 0): PStdChar; overload;
{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function StrPCopy(const Dest: PUniChar; const Source: UniString; const MaxLength: Integer = 0): PUniChar; overload;
{$ENDIF}

function CompareText(const Text1, Text2: StdString): Integer; overload;
{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function CompareText(const Text1, Text2: UniString): Integer; overload;
{$ENDIF}

function SameText(const Text1, Text2: StdString): Boolean; overload; inline;
{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function SameText(const Text1, Text2: UniString): Boolean; overload; inline;
{$ENDIF}

{$IF SIZEOF(StdChar) = 1}
function CharInSet(const Ch: StdChar; const CharSet: TCharSet): Boolean; overload;
{$ENDIF}
function CharInSet(const Ch: UniChar; const CharSet: TCharSet): Boolean; overload;

function ExtractFileExt(const FileName: StdString): StdString; overload;
{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function ExtractFileExt(const FileName: UniString): UniString; overload;
{$ENDIF}

function CompareMem(const Data1, Data2: Pointer; DataLength: Integer): Boolean;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function Min(const Value1, Value2: LongInt): LongInt;
begin
  if Value1 <= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Min(const Value1, Value2: Int64): Int64;
begin
  if Value1 <= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Min(const Value1, Value2: Single): Single;
begin
  if Value1 <= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Min(const Value1, Value2: Double): Double;
begin
  if Value1 <= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Max(const Value1, Value2: LongInt): LongInt;
begin
  if Value1 >= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Max(const Value1, Value2: Int64): Int64;
begin
  if Value1 >= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Max(const Value1, Value2: Single): Single;
begin
  if Value1 >= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Max(const Value1, Value2: Double): Double;
begin
  if Value1 >= Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Hypot(const Value1, Value2: Extended): Extended;
begin
  Result := Sqrt(Value1 * Value1 + Value2 * Value2);
end;

function ArcTan2(const Y, X: Extended): Extended;
begin
  if X = 0.0 then
  begin
    if Y > 0.0 then
      Result := Pi * 0.5
    else if Y < 0.0 then
      Result := -Pi * 0.5
    else
      Result := 0.0;
  end
  else
    Result := ArcTan(Y / X);

  if X < 0.0 then
    Result := Result + Pi;

  if Result > Pi then
    Result := Result - Pi * 2.0;
end;

function ArcSin(const Value: Extended): Extended;
begin
  Result := ArcTan2(Value, Sqrt((1.0 - Value) * (1.0 + Value)));
end;

function ArcCos(const Value: Extended): Extended;
begin
  if Abs(Value) >= 1.0 then
    if Value < 0.0 then
      Result := Pi
    else
      Result := 0.0
  else
    Result := ArcTan2(Sqrt((1.0 - Value) * (1.0 + Value)), Value);
end;

procedure SinCos(const Angle: Single; out SinValue, CosValue: Single);
begin
  SinValue := Sin(Angle);
  CosValue := Cos(Angle);
end;

procedure SinCos(const Angle: Double; out SinValue, CosValue: Double);
begin
  SinValue := Sin(Angle);
  CosValue := Cos(Angle);
end;

function Cot(const Value: Extended): Extended;
begin
  Result := Cos(Value) / Sin(Value);
end;

function IntToStr(const Value: LongInt): StdString;
var
  TempValue, Digits, I: Integer;
begin
  Digits := 0;
  TempValue := Value;

  while TempValue > 0 do
  begin
    TempValue := TempValue div 10;
    Inc(Digits);
  end;

  SetLength(Result, Digits);
  TempValue := Value;

  for I := 0 to Digits - 1 do
  begin
    Result[Digits - I] := Chr(Ord('0') + (TempValue mod 10));
    TempValue := TempValue div 10;
  end;
end;

function IntToHex(Value, Digits: Integer): StdString;
const
  HexDigits: array[0..15] of StdChar = '0123456789ABCDEF';
var
  I: Integer;
begin
  if Digits <= 0 then
    Digits := 1;

  SetLength(Result, Digits);
  for I := 0 to Digits - 1 do
  begin
    Result[Digits - I] := HexDigits[Value and $0F];
    Value := Value shr 4;
  end;

  while Value <> 0 do
  begin
    Result := HexDigits[Value and $0F] + Result;
    Value := Value shr 4;
  end;
end;

function PosEx(const Ch: StdChar; const Text: StdString; const StartPos: Integer): Integer;
var
  CurPos: Integer;
begin
  CurPos := StartPos;

  while CurPos <= Length(Text) do
  begin
    if Text[CurPos] = Ch then
      Exit(CurPos);

    Inc(CurPos);
  end;

  Exit(-1);
end;

function StrToIntDef(const Text: StdString; const DefaultValue: LongInt): LongInt;
var
  ConvRes: Integer;
begin
  Val(Text, Result, ConvRes);
  if ConvRes <> 0 then
    Result := DefaultValue;
end;

function StrPCopy(const Dest: PStdChar; const Source: StdString; const MaxLength: Integer = 0): PStdChar;
var
  CopyChars: Integer;
begin
  Result := Dest;
  if Result <> nil then
  begin
    CopyChars := Length(Source);
    if (MaxLength > 0) and (CopyChars > MaxLength) then
      CopyChars := MaxLength;

    Move(Source[1], Dest^, SizeOf(StdChar) * CopyChars);
    PStdChar(PtrUInt(Dest) + Cardinal(CopyChars) * SizeOf(StdChar))^ := #0;
  end;
end;

{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function StrPCopy(const Dest: PUniChar; const Source: UniString; const MaxLength: Integer = 0): PUniChar;
var
  CopyChars: Integer;
begin
  Result := Dest;
  if Result <> nil then
  begin
    CopyChars := Length(Source);
    if (MaxLength > 0) and (CopyChars > MaxLength) then
      CopyChars := MaxLength;

    Move(Source[1], Dest^, SizeOf(UniChar) * CopyChars);
    PUniChar(PtrUInt(Dest) + Cardinal(CopyChars) * SizeOf(UniChar))^ := #0;
  end;
end;
{$ENDIF}

function CompareText(const Text1, Text2: StdString): Integer;
var
  Index, CommonLength, Length1, Length2: Integer;
  Value1, Value2: Byte;
  Char1, Char2: PStdChar;
begin
  Length1 := Length(Text1);
  Length2 := Length(Text2);

  CommonLength := Min(Length1, Length2);

  Value1 := 0;
  Value2 := 0;
  Index := 0;

  if CommonLength > 0 then
  begin
    Char1 := @Text1[1];
    Char2 := @Text2[1];

    while Index < CommonLength do
    begin
      Value1 := Ord(Char1^);
      Value2 := Ord(Char2^);

      if Value1 <> Value2 then
      begin
        if Value1 in [97..122] then
          Dec(Value1, 32);

        if Value2 in [97..122] then
          Dec(Value2, 32);

        if Value1 <> Value2 then
          Break;
      end;

      Inc(Char1);
      Inc(Char2);
      Inc(Index);
    end;
  end;

  if Index < CommonLength then
    Result := Value1 - Value2
  else
    Result := Length1 - Length2;
end;

{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function CompareText(const Text1, Text2: UniString): Integer;
var
  Index, CommonLength, Length1, Length2: Integer;
  Value1, Value2: Byte;
  Char1, Char2: PUniChar;
begin
  Length1 := Length(Text1);
  Length2 := Length(Text2);

  CommonLength := Min(Length1, Length2);

  Value1 := 0;
  Value2 := 0;
  Index := 0;

  if CommonLength > 0 then
  begin
    Char1 := @Text1[1];
    Char2 := @Text2[1];

    while Index < CommonLength do
    begin
      Value1 := Ord(Char1^);
      Value2 := Ord(Char2^);

      if Value1 <> Value2 then
      begin
        if Value1 in [97..122] then
          Dec(Value1, 32);

        if Value2 in [97..122] then
          Dec(Value2, 32);

        if Value1 <> Value2 then
          Break;
      end;

      Inc(Char1);
      Inc(Char2);
      Inc(Index);
    end;
  end;

  if Index < CommonLength then
    Result := Value1 - Value2
  else
    Result := Length1 - Length2;
end;
{$ENDIF}

function SameText(const Text1, Text2: StdString): Boolean;
begin
  Result := CompareText(Text1, Text2) = 0;
end;

{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function SameText(const Text1, Text2: UniString): Boolean;
begin
  Result := CompareText(Text1, Text2) = 0;
end;
{$ENDIF}

{$IF SIZEOF(StdChar) = 1}
function CharInSet(const Ch: StdChar; const CharSet: TCharSet): Boolean; overload;
begin
  Result := Ch in CharSet;
end;
{$ENDIF}

function CharInSet(const Ch: UniChar; const CharSet: TCharSet): Boolean; overload;
begin
  Result := (Ord(Ch) <= 255) and (StdChar(Ch) in CharSet);
end;

function ExtractFileExt(const FileName: StdString): StdString;
const
  PathChars = ['/', '\'];
  StopChars = PathChars + [':', '.'];
var
  CharPos: Integer;
begin
  Result := '';
  CharPos := Length(FileName);

  while (CharPos > 0) and (not CharInSet(FileName[CharPos], StopChars)) do
    Dec(CharPos);

  if (CharPos > 1) and (FileName[CharPos] = '.') and (not CharInSet(FileName[CharPos - 1], PathChars)) then
    Result := Copy(FileName, CharPos, Length(FileName));
end;

{$IF SIZEOF(StdChar) <> SIZEOF(UniChar)}
function ExtractFileExt(const FileName: UniString): UniString;
const
  PathChars = ['/', '\'];
  StopChars = PathChars + [':', '.'];
var
  CharPos: Integer;
begin
  Result := '';
  CharPos := Length(FileName);

  while (CharPos > 0) and (not CharInSet(FileName[CharPos], StopChars)) do
    Dec(CharPos);

  if (CharPos > 1) and (FileName[CharPos] = '.') and (not CharInSet(FileName[CharPos - 1], PathChars)) then
    Result := Copy(FileName, CharPos, Length(FileName));
end;
{$ENDIF}

function CompareMem(const Data1, Data2: Pointer; DataLength: Integer): Boolean;
var
  I: Integer;
  Value1, Value2: PByte;
begin
  if DataLength > 0 then
  begin
    Value1 := Data1;
    Value2 := Data2;

    for I := 0 to DataLength - 1 do
    begin
      if Value1^ <> Value2^ then
        Exit(False);

      Inc(Value1);
      Inc(Value2);
    end;
  end;

  Result := True;
end;

{$ENDIF}
