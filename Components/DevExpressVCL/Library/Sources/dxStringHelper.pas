{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCoreLibrary                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORELIBRARY AND ALL            }
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

unit dxStringHelper;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, StrUtils, dxCore;

type

  { TdxStringHelper }

  TdxStringHelper = record
  public type
    TStringSplitOptions = set of (RemoveEmptyEntries);
  private const
    ByteToHexMap: array[Byte] of string =
    ('00','01','02','03','04','05','06','07','08','09','0a','0b','0c','0d','0e','0f',
     '10','11','12','13','14','15','16','17','18','19','1a','1b','1c','1d','1e','1f',
     '20','21','22','23','24','25','26','27','28','29','2a','2b','2c','2d','2e','2f',
     '30','31','32','33','34','35','36','37','38','39','3a','3b','3c','3d','3e','3f',
     '40','41','42','43','44','45','46','47','48','49','4a','4b','4c','4d','4e','4f',
     '50','51','52','53','54','55','56','57','58','59','5a','5b','5c','5d','5e','5f',
     '60','61','62','63','64','65','66','67','68','69','6a','6b','6c','6d','6e','6f',
     '70','71','72','73','74','75','76','77','78','79','7a','7b','7c','7d','7e','7f',
     '80','81','82','83','84','85','86','87','88','89','8a','8b','8c','8d','8e','8f',
     '90','91','92','93','94','95','96','97','98','99','9a','9b','9c','9d','9e','9f',
     'a0','a1','a2','a3','a4','a5','a6','a7','a8','a9','aa','ab','ac','ad','ae','af',
     'b0','b1','b2','b3','b4','b5','b6','b7','b8','b9','ba','bb','bc','bd','be','bf',
     'c0','c1','c2','c3','c4','c5','c6','c7','c8','c9','ca','cb','cc','cd','ce','cf',
     'd0','d1','d2','d3','d4','d5','d6','d7','d8','d9','da','db','dc','dd','de','df',
     'e0','e1','e2','e3','e4','e5','e6','e7','e8','e9','ea','eb','ec','ed','ee','ef',
     'f0','f1','f2','f3','f4','f5','f6','f7','f8','f9','fa','fb','fc','fd','fe','ff');
    LastLowSpecial       = #$001F;
    FirstHighSpecial     = #$FFFF;
    MaxDocFontNameLength = 42;
  private
    class function ContainsSpecialSymbols(const AText: string): Boolean; static;
    class function CharInArray(const C: Char; const AChars: array of Char): Boolean; static;
    class procedure SetSinglePrecision(var AValue: Extended); static;
  public
    class function Create(const AValue: array of Char; AStartIndex, ALength: Integer): string; overload; static;
    class function Create(const AValue: TCharArray; AStartIndex, ALength: Integer): string; overload; static;
    class function EndsWith(const AText, APattern: string): Boolean; overload; static; inline;
    class function EndsWith(const AText, APattern: string; AIgnoreCase: Boolean): Boolean; overload; static;
    class function Contains(const AText, APattern: string): Boolean; static;
    class function IndexOf(const AText: string; APattern: Char; AOffset: Integer = 0): Integer; overload; static; inline;
    class function IndexOf(const AText, APattern: string; AOffset: Integer = 0): Integer; overload; static; inline;
    class function IndexOfAny(const AText: string; const AChars: array of Char; AOffset: Integer = 0): Integer; static;
    class function Join(const ASeparator: string; const AValues: TArray<string>): string; overload; static;
    class function Join(const Separator: string; const AValues: array of string; AStartIndex, ACount: Integer): string; overload; static;
    class function Remove(const AText: string; AStartIndex: Integer): string; overload; static;
    class function Remove(const AText: string; AStartIndex, ACount: Integer): string; overload; static;
    class function RemoveEnclosedPart(const AText: string; ALeft, ARight: Char): string; static;
    class function Replace(const AText: string; AOldChar: Char; ANewChar: Char): string; overload; static; inline;
    class function Replace(const AText: string; const OldValue: string; const NewValue: string): string; overload; static; inline;
    class function Split(const AText: string; const ADelimiters: array of string): TArray<string>; overload; static;
    class function Split(const AText: string; const ADelimiters: array of string; AOptions: TStringSplitOptions): TArray<string>; overload; static;
    class function StartsWithChar(const AText: string; APattern: Char; AIgnoreCase: Boolean = True): Boolean; overload; static; inline;
    class function StartsWith(const AText, APattern: string; AIgnoreCase: Boolean = True): Boolean; overload; static; inline;
    class function Substring(const AText: string; AStartIndex: Integer): string; overload; static;
    class function Substring(const AText: string; AStartIndex, ALength: Integer): string; overload; static;
    class function LastIndexOf(const AText: string; AChar: Char): Integer; overload; static; inline;
    class function LastIndexOf(const AText, AValue: string): Integer; overload; static; inline;
    class function LastIndexOf(const AText, AValue: string; AStartIndex: Integer): Integer; overload; static;
    class function LastIndexOf(const AText, AValue: string; AStartIndex: Integer; ACount: Integer): Integer; overload; static; inline;
    class function LastIndexOfAny(const AText: string; const AAnyOf: array of Char): Integer; overload; static;
    class function LastIndexOfAny(const AText: string; const AAnyOf: array of Char; AStartIndex: Integer): Integer; overload; static;
    class function LastIndexOfAny(const AText: string; const AAnyOf: array of Char; AStartIndex, ACount: Integer): Integer; overload; static;
    class function Trim(const AText: string; const AChars: array of Char): string; static;
    class function TrimEnd(const AText: string; const AChars: array of Char): string; static;
    class function TrimStart(const AText: string; const AChars: array of Char): string; static;
    class function ToHex(C: Char): string; overload; static;
    class function ToHex(C: Char; P: PChar): PChar; overload; static;
    class function ToHex(B: Byte): string; overload; static;
    class function ToHex(const ASource: TBytes; AOffset, ACount: Integer; var ADest: TCharArray): Integer; overload; static;
    class function ToInt32(const AText: string; AFromBase: Cardinal = 10): Integer; static;
    class function ToInt32Def(const AText: string; ADefault: Integer; AFromBase: Cardinal = 10): Integer; static;
    class function ToBinary(AValue: Cardinal; ADigits: Integer): string; static;
    class function Format(const AFormat: string; const Args: array of const): string; overload; static;
    class function Format(const AFormatSettings: TFormatSettings; const AFormat: string; const Args: array of const): string; overload; static;
    class function PrepareFontNameForDoc(const AOriginalFontName: string): string; static;
    class function RemoveSpecialSymbols(const AText: string): string; static;
    class function ReplaceParagraphMarksWithLineBreaks(const AText: string): string; static;
    class function ContainsParagraphMarksOrUnitSeparators(const AText: string): Boolean; static;
  end;

  { TdxStringSet }

  TdxStringSet = class
  private const
    TableSize = 257;
  private type
    PItem = ^TItem;
    TItem = record
      Hash: Cardinal;
      Value: string;
      Next: PItem;
    end;
    TTable = array[0..TableSize] of PItem;
  private
    FTable: TTable;
    procedure Clear;
    function Hash(const S: string): Cardinal;
    function NewItem(const S: string; AHash: Cardinal): PItem;
  public
    destructor Destroy; override;
    function Add(const S: string): string;
    function Contains(const S: string): Boolean;
  end;

  { TdxStringBuilderManager }

  TdxStringBuilderManager = class
  strict private const
    CacheSize = 4;
  strict private
    class var Cache: array[0..Pred(CacheSize)] of TStringBuilder;
  public
    class procedure Finalize;
    class function Get(ACapacity: Integer = 0): TStringBuilder;
    class procedure Release(var ABuilder: TStringBuilder);
  end;

implementation

uses
  Windows, Math, SysConst, dxCharacters;

  {$ASSERTIONS OFF}

{ TdxStringHelper }

class function TdxStringHelper.Create(const AValue: array of Char; AStartIndex, ALength: Integer): string;
begin
  Result := System.Copy(AValue, AStartIndex + 1, ALength);
end;

class function TdxStringHelper.CharInArray(const C: Char; const AChars: array of Char): Boolean;
var
  P: PChar;
  ALen: Integer;
begin
  //Result := False;
  ALen := Length(AChars);
  Assert(ALen > 0);
  P := @AChars[0];
  while True do
  begin
    if P^ = C then
      Exit(True);
    Dec(ALen);
    if ALen = 0 then
      Exit(False);
    Inc(P);
  end;
end;

class function TdxStringHelper.Contains(const AText, APattern: string): Boolean;
begin
  Result := Pos(APattern, AText) > 0;
end;

class function TdxStringHelper.ContainsSpecialSymbols(const AText: string): Boolean;
var
  P: PChar;
  I: Integer;
  ALength: Integer;
begin
  ALength := Length(AText);
  P := PChar(AText);
  for I := 0 to ALength - 1 do
  begin
    if (P^ <= LastLowSpecial) or (P^ >= FirstHighSpecial) then
      if (P^ <> TdxCharacters.TabMark) and (P^ <> #$000A) and (P^ <> TdxCharacters.ParagraphMark) then
        Exit(True);
    Inc(P);
  end;
  Result := False;
end;

class function TdxStringHelper.Create(const AValue: TCharArray; AStartIndex, ALength: Integer): string;
begin
  SetLength(Result, ALength);
  Move(AValue[0], Result[1], ALength * SizeOf(Char));
end;

class function TdxStringHelper.EndsWith(const AText, APattern: string): Boolean;
begin
  Result := EndsWith(AText, APattern, False);
end;

class function TdxStringHelper.EndsWith(const AText, APattern: string; AIgnoreCase: Boolean): Boolean;
const
  CompareOptions: array[Boolean] of Integer = (0, NORM_IGNORECASE);
var
  ATextLen, APatternLen, ASubTextLocation: Integer;
begin
  ATextLen := Length(AText);
  APatternLen := Length(APattern);
  ASubTextLocation := ATextLen - APatternLen + 1;
  if (ASubTextLocation > 0) and (APattern <> '') and (ByteType(AText, ASubTextLocation) <> mbTrailByte) then
    Result := CompareString(LOCALE_USER_DEFAULT, CompareOptions[AIgnoreCase],
      PChar(@AText[ASubTextLocation]), APatternLen, PChar(APattern), APatternLen) = 2
  else
   Result := False;
end;

class function TdxStringHelper.IndexOf(const AText: string; APattern: Char; AOffset: Integer = 0): Integer;
begin
  Result := PosEx(APattern, AText, AOffset + 1) - 1;
end;

class function TdxStringHelper.IndexOf(const AText, APattern: string; AOffset: Integer = 0): Integer;
begin
  Result := PosEx(APattern, AText, AOffset + 1) - 1;
end;

class function TdxStringHelper.IndexOfAny(const AText: string; const AChars: array of Char; AOffset: Integer = 0): Integer;
var
  P: PChar;
  ALength: Integer;
begin
  ALength := Length(AText);
  if (ALength = 0) or (AOffset >= ALength) then
    Exit(-1);

  P := PChar(AText) + AOffset;
  while P^ <> #0 do
  begin
    if CharInArray(P^, AChars) then
    begin
      Result := P - PChar(AText);
      Exit;
    end;
    Inc(P);
  end;
  Result := -1;
end;

class function TdxStringHelper.Join(const Separator: string; const AValues: array of string; AStartIndex,
  ACount: Integer): string;
var
  I: Integer;
  AMax: Integer;
  AResult: TStringBuilder;
begin
  if (ACount = 0) or ((System.Length(AValues) = 0) and (AStartIndex = 0)) then
    Result := ''
  else
  begin
    if (ACount < 0) or (AStartIndex >= Length(AValues)) then
      raise ERangeError.CreateRes(@SRangeError);

    if (AStartIndex + ACount) > Length(AValues) then
      AMax := System.Length(AValues)
    else
      AMax := AStartIndex + ACount;

    AResult := TdxStringBuilderManager.Get;
    try
      AResult.Append(AValues[AStartIndex]);
      for I:= AStartIndex + 1 to AMax - 1 do
        AResult.Append(Separator).Append(AValues[I]);
      Result := AResult.ToString;
    finally
      TdxStringBuilderManager.Release(AResult);
    end;
  end;
end;

class function TdxStringHelper.Join(const ASeparator: string; const AValues: TArray<string>): string;
begin
  Result := Join(ASeparator, AValues, 0, Length(AValues));
end;

class function TdxStringHelper.LastIndexOf(const AText, AValue: string): Integer;
begin
  Result := LastIndexOf(AText, AValue, Length(AText) - 1, Length(AText));
end;

class function TdxStringHelper.LastIndexOf(const AText, AValue: string; AStartIndex: Integer): Integer;
begin
  Result := LastIndexOf(AText, AValue, AStartIndex, Length(AText));
end;

class function TdxStringHelper.LastIndexOf(const AText: string; AChar: Char): Integer;
begin
  Result := LastIndexOf(AText, AChar, Length(AText) - 1, Length(AText));
end;

class function TdxStringHelper.LastIndexOf(const AText, AValue: string; AStartIndex, ACount: Integer): Integer;
var
  I, AMin, AValueLength: Integer;
  ACharText: PChar;
begin
  AValueLength := Length(AValue);
  if AValueLength = 0 then
    Exit(-1);
  if AStartIndex < Length(AText) then
    I := AStartIndex - AValueLength + 1
  else
    I := Length(AText) - AValueLength;
  if AStartIndex < ACount then
    AMin := 0
  else
    AMin := AStartIndex - ACount + 1;
  ACharText := PChar(AText);
  while I >= AMin do
  begin
    if StrLComp(@ACharText[I], PChar(AValue), AValueLength) = 0 then
      Exit(I);
    Dec(I);
  end;
  Result := -1;
end;

class function TdxStringHelper.LastIndexOfAny(const AText: string; const AAnyOf: array of Char): Integer;
begin
  Result := LastIndexOfAny(AText, AAnyOf, Length(AText) - 1, Length(AText));
end;

class function TdxStringHelper.LastIndexOfAny(const AText: string; const AAnyOf: array of Char;
  AStartIndex: Integer): Integer;
begin
  Result := LastIndexOfAny(AText, AAnyOf, AStartIndex, Length(AText));
end;

class function TdxStringHelper.LastIndexOfAny(const AText: string; const AAnyOf: array of Char; AStartIndex,
  ACount: Integer): Integer;
var
  I, AMin: Integer;
  C: Char;
begin
  if AStartIndex < Length(AText) then
    I := AStartIndex
  else
    I := Length(AText) - 1;
  if (AStartIndex - ACount) < 0 then
    AMin := 0
  else
    AMin := AStartIndex - ACount + 1;
  while I >= AMin do
  begin
    for C in AAnyOf do
      if AText[I + 1] = C then
        Exit(I);
    Dec(I);
  end;
  Result := -1;
end;

class function TdxStringHelper.Remove(const AText: string; AStartIndex: Integer): string;
begin
  Result := AText;
  Delete(Result, AStartIndex + 1, Length(Result));
end;

class function TdxStringHelper.Remove(const AText: string; AStartIndex, ACount: Integer): string;
begin
  Result := AText;
  Delete(Result, AStartIndex + 1, ACount);
end;

class function TdxStringHelper.RemoveEnclosedPart(const AText: string; ALeft, ARight: Char): string;
var
  AFirst, ALast: PChar;
begin
  Result := AText;
  if AText = '' then
    Exit;
  AFirst := PChar(AText);
  while (AFirst^ <> #0) and (AFirst^ <> ALeft) do
    Inc(AFirst);
  if AFirst^ = #0 then
    Exit;

  ALast := @AText[Length(AText)];
  while (ALast > AFirst) and (ALast^ <> ARight) do
    Dec(ALast);
  if AFirst = ALast then
    Exit;

  Delete(Result, AFirst - PChar(AText) + 1, ALast - AFirst + 1);
end;

class function TdxStringHelper.RemoveSpecialSymbols(const AText: string): string;
var
  P: PChar;
  ASb: TStringBuilder;
  I, ALength: Integer;
begin
  if not ContainsSpecialSymbols(AText) then
    Exit(AText);

  ALength := Length(AText);
  ASb := TdxStringBuilderManager.Get(ALength);
  try
    P := PChar(AText);
    for I := 0 to ALength - 1 do
    begin
      if (P^ > LastLowSpecial) and (P^ < FirstHighSpecial) then
        ASb.Append(P^)
      else
        if (P^ = TdxCharacters.TabMark) or (P^ = #$000A) or (P^ = TdxCharacters.ParagraphMark) then
          ASb.Append(P^);
      Inc(P);
    end;
    Result := ASb.ToString;
  finally
    TdxStringBuilderManager.Release(ASb);
  end;
end;

class function TdxStringHelper.ReplaceParagraphMarksWithLineBreaks(const AText: string): string;
var
  ASb: TStringBuilder;
  ACount, I: Integer;
  ACh, ANextCh: Char;
begin
  if not ContainsParagraphMarksOrUnitSeparators(AText) then
    Exit(AText);
  ACount := Length(AText);
  ASb := TdxStringBuilderManager.Get(ACount);
  try
    I := 1;
    while I <= ACount do
    begin
      ACh := AText[I];
      if (ACh <> #13) and (ACh <> #10) and (ACh <> #$1F) and (ACh <> #$1E) then
        ASb.Append(ACh)
      else
        if (ACh <> #$1F) and (ACh <> #$1E)  then
        begin
          ASb.Append(TdxCharacters.LineBreak);
          if I <> ACount then
          begin
            ANextCh := AText[I + 1];
            if ((ANextCh = #13) or (ANextCh = #10)) and (ANextCh <> ACh) then
              Inc(I);
          end;
        end
        else
          if ACh = #$1E then
            ASb.Append(TdxCharacters.Dash);
      Inc(I);
    end;
    Result := ASb.ToString;
  finally
    TdxStringBuilderManager.Release(ASb);
  end;
end;

class function TdxStringHelper.ContainsParagraphMarksOrUnitSeparators(const AText: string): Boolean;
var
  ACh: Char;
begin
  for ACh in AText do
    if (ACh = #13) or (ACh = #10) or (ACh = #31) or (ACh = #$1E) then
      Exit(True);
  Result := False;
end;

class function TdxStringHelper.Replace(const AText, OldValue, NewValue: string): string;
begin
  Result := SysUtils.StringReplace(AText, OldValue, NewValue, [rfReplaceAll]);
end;

class function TdxStringHelper.Replace(const AText: string; AOldChar, aNewChar: Char): string;
var
  L: Integer;
  P: PChar;
begin
  Result := AText;
  L := Length(AText);
  if L > 0 then
  begin
    UniqueString(Result);
    P := PChar(Result);
    repeat
      if P^ = AOldChar then
        P^ := ANewChar;
      Inc(P);
      Dec(L);
    until L = 0;
  end;
end;

class function TdxStringHelper.Split(const AText: string;
  const ADelimiters: array of string; AOptions: TStringSplitOptions): TArray<string>;

  function IndexOfAny(const AText: string; AOffset: Integer; const AValues: array of string;
    var AIndex: Integer): Integer; overload;
  var
    C, P, I: Integer;
  begin
    P := MaxInt;
    AIndex := -1;
    for C := 0 to Length(AValues) - 1 do
    begin
      I := TdxStringHelper.IndexOf(AText, AValues[C], AOffset);
      if (I >= 0) and (I < P) then
      begin
        P := I;
        AIndex := C;
      end;
    end;
    if P < MaxInt then
      Result := P
    else
      Result := -1;
  end;

var
  P: Integer;
  ATotal: Integer;
  AIndex: Integer;
  AToSplitPos: Integer;
  S: string;
begin
  if AText = '' then
    Exit(nil);
  ATotal := 0;
  AToSplitPos := 0;
  repeat
    P := IndexOfAny(AText, AToSplitPos, ADelimiters, AIndex);
    if (P < 0) and (AToSplitPos < Length(AText)) then
      P := Length(AText);
    S := TdxStringHelper.Substring(AText, AToSplitPos, P - AToSplitPos);
    if (S <> '') or ((S = '') and (AOptions = [])) then
    begin
      Inc(ATotal);
      SetLength(Result, ATotal);
      Result[ATotal - 1] := S;
    end;
    if AIndex < 0 then
      Break;
    AToSplitPos := P + Length(ADelimiters[AIndex]);
  until P < 0;
end;

class function TdxStringHelper.StartsWithChar(const AText: string; APattern: Char; AIgnoreCase: Boolean = True): Boolean;
var
  ALength: Integer;
begin
  ALength := Length(AText);
  if ALength = 0 then
    Result := False
  else
    if AIgnoreCase then
      Result := UpCase(AText[ALength]) = UpCase(APattern)
    else
      Result := AText[ALength] = APattern;
end;

class function TdxStringHelper.StartsWith(const AText, APattern: string; AIgnoreCase: Boolean = True): Boolean;
begin
  if APattern = '' then
    Result := False
  else
    if not AIgnoreCase then
      Result := StrLComp(PChar(AText), PChar(APattern), Length(APattern)) = 0
    else
      Result := StrLIComp(PChar(AText), PChar(APattern), Length(APattern)) = 0;
end;

class function TdxStringHelper.Substring(const AText: string; AStartIndex: Integer): string;
begin
  Result := Copy(AText, AStartIndex + 1, Length(AText));
end;

class function TdxStringHelper.Split(const AText: string; const ADelimiters: array of string): TArray<string>;
begin
  Result := TdxStringHelper.Split(AText, ADelimiters, []);
end;

class function TdxStringHelper.Substring(const AText: string; AStartIndex, ALength: Integer): string;
begin
  Result := Copy(AText, AStartIndex + 1, ALength);
end;

class function TdxStringHelper.ToInt32(const AText: string; AFromBase: Cardinal): Integer;

  function SBinToInt(const AValue: string): Integer;
  const
    Convert: array['0'..'1'] of byte = (0, 1);
  var
    I: Integer;
    ADigit: Word;
    B: Byte;
  begin
    Result := 0;
    ADigit := 0;
    for I := Length(AValue) downto 1 do
    begin
      if not ((AValue[I] = '0') or (AValue[I] = '1')) then
        raise Exception.Create('Additional non-parsable characters are at the end of the string');
      B := Convert[AnsiChar(AValue[I])];
      if (ADigit >= 32) and (B <> 0) then
        raise Exception.Create('Value was either too large or too small for a integer');
      Result := Result or (B shl ADigit);
      Inc(ADigit);
    end;
  end;

begin
  case AFromBase of
    2:
      Result := SBinToInt(AText);
    10:
      Result := StrToInt(AText);
    else
      raise Exception.Create('Invalid Base:' + IntToStr(AFromBase));
  end;
end;

class function TdxStringHelper.ToInt32Def(const AText: string; ADefault: Integer; AFromBase: Cardinal): Integer;

  function SBinToInt(const AValue: string; ADefault: Integer): Integer;
  const
    Convert: array['0'..'1'] of byte = (0, 1);
  var
    I: Integer;
    ADigit: Word;
    B: Byte;
  begin
    Result := 0;
    ADigit := 0;
    for I := Length(AValue) downto 1 do
    begin
      if not ((AValue[I] = '0') or (AValue[I] = '1')) then
        Exit(ADefault);
      B := Convert[AnsiChar(AValue[I])];
      if (ADigit >= 32) and (B <> 0) then
        Exit(ADefault);
      Result := Result or (B shl ADigit);
      Inc(ADigit);
    end;
  end;

begin
  case AFromBase of
    2:
      Result := SBinToInt(AText, ADefault);
    10:
      Result := StrToIntDef(AText, ADefault);
    else
      Result := ADefault;
  end;
end;

class function TdxStringHelper.ToBinary(AValue: Cardinal; ADigits: Integer): string;
var
  ABuffer: array[0..31] of Char;
  P: PChar;
begin
  P := @ABuffer[31];
  if ADigits > 32 then
    ADigits := 32;
  while (AValue > 0) or (ADigits > 0) do
  begin
    if AValue and 1 = 0 then
      P^ := '0'
    else
      P^ := '1';
    Dec(P);
    AValue := AValue shr 1;
    Dec(ADigits);
  end;
  Inc(P);
  SetString(Result, P, PChar(@ABuffer[31]) - P + 1);
end;

class function TdxStringHelper.Trim(const AText: string; const AChars: array of Char): string;
var
  I, L: Integer;
begin
  L := Length(AText);
  I := 1;
  if (L > 0) and not CharInArray(AText[I], AChars) and not CharInArray(AText[L], AChars) then
    Exit(AText);
  while (I <= L) and CharInArray(AText[I], AChars) do
    Inc(I);
  if I > L then
    Exit('');
  while CharInArray(AText[L], AChars) do
    Dec(L);
  Result := Copy(AText, I, L - I + 1);
end;

class function TdxStringHelper.TrimEnd(const AText: string; const AChars: array of Char): string;
var
  I: Integer;
begin
  I := Length(AText);
  if (I = 0) or not CharInArray(AText[I], AChars) then
    Exit(AText);
  while (I > 0) and CharInArray(AText[I], AChars) do
    Dec(I);
  Result := System.Copy(AText, 1, I);
end;

class function TdxStringHelper.TrimStart(const AText: string; const AChars: array of Char): string;
var
  I, ALength: Integer;
begin
  ALength := Length(AText);
  I := 0;
  while (I < ALength) and (CharInArray(AText[I + 1], AChars)) do
    Inc(I);
  if I > 0 then
    Result := SubString(AText, I)
  else
    Result := AText;
end;

class function TdxStringHelper.ToHex(B: Byte): string;
begin
  Result := ByteToHexMap[B];
end;

class function TdxStringHelper.ToHex(C: Char): string;
var
  ABuffer: array[0..1] of Cardinal;
begin
  ABuffer[1] := PCardinal(ByteToHexMap[Byte(Ord(C))])^;
  ABuffer[0] := PCardinal(ByteToHexMap[Ord(C) shr 8])^;
  SetString(Result, PChar(@ABuffer), 4);
end;

class function TdxStringHelper.ToHex(C: Char; P: PChar): PChar;
var
  ABuffer: PCardinal absolute P;
begin
  ABuffer^ := PCardinal(ByteToHexMap[Ord(C) shr 8])^;
  Inc(ABuffer);
  ABuffer^ := PCardinal(ByteToHexMap[Byte(Ord(C))])^;
  Inc(ABuffer);
  Result := P;
end;

class function TdxStringHelper.ToHex(const ASource: TBytes; AOffset, ACount: Integer; var ADest: TCharArray): Integer;
var
  AHex: PCardinal;
  P: PByte;
begin
  Assert(ASource <> nil);
  Assert(AOffset >= 0);
  Assert(ACount >= 0);
  Assert(ACount <= Length(ASource) - AOffset);
  Assert(Length(ADest) >= ACount * 2);
  Result := ACount shl 1;
  AHex := Pointer(ADest);
  P := Pointer(ASource);
  Inc(P, AOffset);
  repeat
    AHex^ := PCardinal(ByteToHexMap[P^])^;
    Inc(AHex);
    Inc(P);
    Dec(ACount);
  until ACount = 0;
end;

class procedure TdxStringHelper.SetSinglePrecision(var AValue: Extended);
const
  SinglePrecision = 7;
var
  ALog10: Integer;
begin
  if IsZero(AValue) then
    Exit;
  ALog10 := Round(Log10(Abs(AValue)));
  if ALog10 < SinglePrecision then
    AValue := SimpleRoundTo(AValue, -Max(0, SinglePrecision - ALog10));
end;

class function TdxStringHelper.Format(const AFormat: string; const Args: array of const): string;
var
  AFormatSettings: TFormatSettings;
begin
  dxGetLocaleFormatSettings(GetUserDefaultLCID, AFormatSettings);
  Result := Format(AFormatSettings, AFormat, Args);
end;

class function TdxStringHelper.Format(
  const AFormatSettings: TFormatSettings; const AFormat: string; const Args: array of const): string;
var
  I: Integer;
begin
  for I := Low(Args) to High(Args) do
    if Args[I].VType = vtExtended then
      SetSinglePrecision(Args[I].VExtended^);
  Result := SysUtils.Format(AFormat, Args, AFormatSettings);
end;

class function TdxStringHelper.PrepareFontNameForDoc(const AOriginalFontName: string): string;
var
  ATrimIndex: Integer;
begin
  Result := RemoveSpecialSymbols(AOriginalFontName);
  if Length(Result) <= MaxDocFontNameLength then
    Exit;

  ATrimIndex := IndexOf(Result, ',');
  if ATrimIndex > 0 then
    Result := Substring(Result, 0, ATrimIndex);
  if Length(Result) > MaxDocFontNameLength then
    Result := Substring(Result, 0, MaxDocFontNameLength);
end;

{ TdxStringSet }

destructor TdxStringSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TdxStringSet.Add(const S: string): string;
var
  AIndex, AHash: Cardinal;
  AEntry, ATemp: PItem;
begin
  AHash := Hash(S);
  AIndex := AHash mod TableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
  begin
    AEntry := NewItem(S, AHash);
    FTable[AIndex] := AEntry;
    Exit(AEntry.Value);
  end
  else
  begin
    repeat
      if (AHash = AEntry.Hash) and (S = AEntry.Value) then
        Exit(AEntry.Value);

      ATemp := AEntry.Next;
      if ATemp = nil then
      begin
        ATemp := NewItem(S, AHash);
        AEntry.Next := ATemp;
        Exit(ATemp.Value);
      end;
      AEntry := ATemp;
    until False;
  end;
end;

procedure TdxStringSet.Clear;
var
  I: Integer;
  AItem, ATemp: PItem;
begin
  for I := Low(FTable) to High(FTable) do
  begin
    AItem := FTable[I];
    while AItem <> nil do
    begin
      ATemp := AItem;
      AItem := AItem.Next;
      Dispose(ATemp);
    end;
    FTable[I] := nil;
  end;
end;

function TdxStringSet.Contains(const S: string): Boolean;
var
  AHash: Cardinal;
  AEntry, ATemp: PItem;
begin
  AHash := Hash(S);
  AEntry := FTable[AHash mod TableSize];
  if AEntry = nil then
    Exit(False)
  else
  begin
    repeat
      if (AHash = AEntry.Hash) and (S = AEntry.Value) then
        Exit(True);

      ATemp := AEntry.Next;
      if ATemp = nil then
        Exit(False);

      AEntry := ATemp;
    until False;
  end;
end;

function TdxStringSet.Hash(const S: string): Cardinal;
var
  P: PChar;
begin
  Result := 0;
  P := PChar(S);
  while P^ <> #0 do
  begin
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(P^);
    Inc(P);
  end;
end;

function TdxStringSet.NewItem(const S: string; AHash: Cardinal): PItem;
begin
  New(Result);
  Result.Hash := AHash;
  Result.Value := S;
  Result.Next := nil;
end;

{ TdxStringBuilderManager }

class procedure TdxStringBuilderManager.Finalize;
var
  I: Integer;
begin
  for I := 0 to CacheSize - 1 do
    FreeAndNil(Cache[I]);
end;

class function TdxStringBuilderManager.Get(ACapacity: Integer): TStringBuilder;
var
  AIndex: Integer;
begin
  AIndex := 0;
  Result := nil;
  while (Result = nil) and (AIndex < CacheSize) do
  begin
  {$IFDEF MSWINDOWS}
    Result := InterlockedExchangePointer(Pointer(Cache[AIndex]), nil);
  {$ELSE}
    Result := AtomicExchange(Pointer(Cache[AIndex]), nil);
  {$ENDIF}
    Inc(AIndex);
  end;

  if Result <> nil then
  begin
    Result.Capacity := Max(Result.Capacity, ACapacity);
    Result.Length := 0;
  end
  else
    Result := TStringBuilder.Create(ACapacity);
end;

class procedure TdxStringBuilderManager.Release(var ABuilder: TStringBuilder);
var
  AIndex: Integer;
begin
  AIndex := 0;
  while (ABuilder <> nil) and (AIndex < CacheSize) do
  begin
  {$IFDEF MSWINDOWS}
    ABuilder := InterlockedExchangePointer(Pointer(Cache[AIndex]), ABuilder);
  {$ELSE}
    ABuilder := AtomicExchange(Pointer(Cache[AIndex]), ABuilder);
  {$ENDIF}
    Inc(AIndex);
  end;
  FreeAndNil(ABuilder);
end;

initialization

finalization
  TdxStringBuilderManager.Finalize;
end.
