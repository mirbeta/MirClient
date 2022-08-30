{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
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

unit dxHunspellUtils;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Forms, dxHunspellTypes;

const

  mlPhon        = 'ph:';
  mlTagLength   = Length(mlPhon);

  dxDefaultFlags      = 65510;
  dxForbiddenWordFlag = 65510;
  dxOnlyUpcaseFlag    = 65511;

  dxNullFlag          = $00;

  dxDefaultHunspellCodePage: Word = 28591; //ISO 8859-1 Latin I;
  dxDefaultHunspellKeyboardString: PWideChar = 'qwertyuiop|asdfghjkl|zxcvbnm';

function ContainsFlag(AFlags: PdxAffixFlagsData; ALength: Integer; ARequiredFlag: Word): Boolean;
function CopyMorphology(ADestination: PWideChar; const AMorphologicalDescription,
  AMorphologicalField: PWideChar): PWideChar;
function GetLanguageID(ALanguage: PWideChar): Integer;
function GetSecondPartOfString(S: PWideChar; out ADest: PWideChar): Boolean;
procedure HunspellError(const AMessage: string; ALineIndex: Integer);
function IntStr(AValue: Longint): PWideChar;
function IsDigit(AChar: WideChar): Boolean;
function IsSpace(AChar: WideChar): Boolean;
function IsUpCase(AChar: WideChar): Boolean;
procedure MakeAllCapital(S: PWideChar; ALangID: LCID; ALength: Integer = 0);
procedure MakeAllSmall(S: PWideChar; ALangID: LCID; ALength: Integer = 0);
procedure MakeCapitalized(S: PWideChar; ALangID: LCID);
procedure MakeCharLower(S: PWideChar; AIndex: Integer; ALangID: LCID);
procedure RemoveCRLF(S: PWideChar);
procedure RemoveIgnoredChars(S: PWideChar; const AIgnoreChars: PWideChar);
procedure SortFlags(var AFlags: array of Word; ABegin, AEnd: Integer);
function StrCopyReverse(S: PWideChar): PWideChar;
procedure StrFreeAndNil(var S: PWideChar); inline;
function StrInt(S: PWideChar): Integer;
function StrReplace(S: PWideChar; APattern: PWideChar; AReplacement: PWideChar): PWideChar;
function StrReverse(S: PWideChar): PWideChar;
function StrSeparate(S: PPWideChar; const ADelimiter: WideChar): PWideChar;

var
  dxHunspellWarningFlag: Boolean;
  dxHunspellLastWarningMessage: string;
  dxHunspellLastWarningLineIndex: Integer;

implementation

uses
  StrUtils, Dialogs, dxCore, cxClasses, dxSpellCheckerUtils;


var
  HasWarning: Boolean;

function ContainsFlag(AFlags: PdxAffixFlagsData; ALength: Integer;
  ARequiredFlag: Word): Boolean;
var
  AMiddle, ALeft, ARight: Integer;
begin
  Result := False;
  ALeft := 0;
  ARight := ALength - 1;
  while ALeft <= ARight do
  begin
    AMiddle := (ALeft + ARight) div 2;
    if AFlags^[AMiddle] = ARequiredFlag then
    begin
      Result := True;
      Break;
    end;
    if ARequiredFlag < AFlags^[AMiddle] then
      ARight := AMiddle - 1
    else
      ALeft := AMiddle + 1;
  end;
end;

function CopyMorphology(ADestination: PWideChar; const AMorphologicalDescription,
  AMorphologicalField: PWideChar): PWideChar;
var
  APosition, ADestinationCursor: PWideChar;
begin
  Result := nil;
  if AMorphologicalDescription = nil then
    Exit;
  APosition := StrPos(AMorphologicalDescription, AMorphologicalField);
  if APosition <> nil then
  begin
    ADestinationCursor := ADestination;
    Inc(APosition, mlTagLength);
    while not ((APosition^ = #0) or (APosition^ = ' ') or
      (APosition^ = #9) or (APosition^ = #10)) do
    begin
      ADestinationCursor^ := APosition^;
      Inc(ADestinationCursor);
      Inc(APosition);
    end;
    ADestinationCursor^ := #0;
    Result := ADestination;
  end;
end;

function GetLanguageID(ALanguage: PWideChar): Integer;
begin
  Result := TdxLanguageID.GetLanguageID(ALanguage);
end;

function GetSecondPartOfString(S: PWideChar; out ADest: PWideChar): Boolean;
var
  P, APart: PWideChar;
  I, APartCount: Integer;
begin
  Result := False;
  P := S;
  I := 0;
  APartCount := 0;
  if ADest <> nil then
    Exit;
  APart := StrSeparate(@P, #0);
  while APart <> nil do
  begin
    if APart^ <> #0 then
    begin
      case I of
        0:
          Inc(APartCount);
        1:
          begin
            ADest := StrNew(APart);
            if ADest = nil then
              Exit;
            Inc(APartCount);
          end;
      end;
      Inc(I);
    end;
    APart := StrSeparate(@P, #0);
  end;
  Result := APartCount = 2;
end;

procedure HunspellError(const AMessage: string; ALineIndex: Integer);
begin
  dxHunspellLastWarningMessage := AMessage;
  dxHunspellLastWarningLineIndex := ALineIndex;
  dxHunspellWarningFlag := True;
  HasWarning := True;
end;

function IntStr(AValue: Longint): PWideChar;
var
  I, ASign: Integer;
  ABuffer: array [0..19] of WideChar;
begin
  ASign := AValue;
  AValue := Abs(AValue);
  I := 0;
  repeat
    ABuffer[I] := WideChar((AValue mod 10) + Ord('0'));
    Inc(I);
    AValue := AValue div 10;
  until AValue = 0;
  if ASign < 0 then
  begin
    ABuffer[I] := '-';
    Inc(I);
  end;
  ABuffer[I] := #0;
  Result := StrCopyReverse(PWideChar(@ABuffer));
end;

function IsDigit(AChar: WideChar): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function IsSpace(AChar: WideChar): Boolean;
begin
  Result := (#$09 <= AChar) and (AChar <= #$0D) or (AChar = #$20);
end;

function IsUpCase(AChar: WideChar): Boolean;
begin
  Result := dxGetWideCharCType1(AChar) and C1_UPPER > 0;
end;

procedure MakeAllCapital(S: PWideChar; ALangID: LCID; ALength: Integer = 0);
var
  L: Integer;
begin
  if S = nil then Exit;
  if ALength > 0 then
    L := ALength
  else
    L := StrLen(S);
  if L = 0 then Exit;
  LCMapStringW(ALangID, LCMAP_UPPERCASE + LCMAP_LINGUISTIC_CASING, S, L, S, L);
end;

procedure MakeAllSmall(S: PWideChar; ALangID: LCID; ALength: Integer = 0);
var
  L: Integer;
begin
  if S = nil then Exit;
  if ALength > 0 then
    L := ALength
  else
    L := StrLen(S);
  if L = 0 then Exit;
  LCMapStringW(ALangID, LCMAP_LOWERCASE + LCMAP_LINGUISTIC_CASING, S, L, S, L);
end;

procedure MakeCapitalized(S: PWideChar; ALangID: LCID);
var
  L: Integer;
begin
  if S = nil then Exit;
  L := StrLen(S);
  if L < 1 then Exit;
  LCMapStringW(ALangID, LCMAP_UPPERCASE + LCMAP_LINGUISTIC_CASING, S, 1, S, 1);
end;

procedure MakeCharLower(S: PWideChar; AIndex: Integer; ALangID: LCID);
var
  L: Integer;
begin
  if S = nil then Exit;
  L := StrLen(S);
  if AIndex >= L then Exit;
  LCMapStringW(ALangID, LCMAP_LOWERCASE + LCMAP_LINGUISTIC_CASING, S + AIndex, 1, S + AIndex, 1);
end;

procedure RemoveCRLF(S: PWideChar);
var
  L: Integer;
begin
  L := StrLen(S);
  if (L > 0) and (((S + L - 1)^ = #13) or ((S + L - 1)^ = #10)) then
    (S + L - 1)^ := #0;
  if (L > 1) and ((S + L - 2)^ = #13) then
    (S + L - 2)^ := #0;
end;

procedure RemoveIgnoredChars(S: PWideChar; const AIgnoreChars: PWideChar);
var
  P: PWideChar;
begin
  if AIgnoreChars = nil then
    Exit;
  P := S;
  while P^ <> #0 do
  begin
    if StrScan(AIgnoreChars, P^) = nil then
    begin
      S^ := P^;
      Inc(S);
    end;
    Inc(P);
  end;
  S^ := #0;
end;

procedure SortFlags(var AFlags: array of Word; ABegin, AEnd: Integer);
var
  ATemp, APivot: Word;
  ALeft, ARight: Integer;
begin
  if AEnd > ABegin then
  begin
    APivot := AFlags[ABegin];
    ALeft := ABegin + 1;
    ARight := AEnd;
    while ALeft < ARight do
    begin
      if AFlags[ALeft] <= APivot then
        Inc(ALeft)
      else
      begin
        Dec(ARight);
        ATemp := AFlags[ALeft];
        AFlags[ALeft] := AFlags[ARight];
        AFlags[ARight] := ATemp;
      end
    end;
    Dec(ALeft);
    ATemp := AFlags[ABegin];
    AFlags[ABegin] := AFlags[ALeft];
    AFlags[ALeft] := ATemp;

    SortFlags(AFlags, ABegin, ALeft);
    SortFlags(AFlags, ARight, AEnd);
  end;
end;

function StrCopyReverse(S: PWideChar): PWideChar;
var
  ALength: Integer;
  ALeft, ARight: PWideChar;
begin
  Result := nil;
  if S <> nil then
  begin
    ALength := StrLen(S);
    Result := StrAlloc(ALength + 1);
    ALeft := Result;
    ARight := S + ALength - 1;
    while ARight >= S do
    begin
      ALeft^ := ARight^;
      Inc(ALeft);
      Dec(ARight);
    end;
    ALeft^ := #0;
  end;
end;

procedure StrFreeAndNil(var S: PWideChar);
begin
  StrDispose(S);
  S := nil;
end;

function StrInt(S: PWideChar): Integer;
begin
  Result := 0;
  if S = nil then Exit;
  while (S^ >= '0') and (S^ <= '9') do
  begin
    Result := Result * 10 + Ord(S^) - Ord('0');
    Inc(S);
  end;
end;

function StrReplace(S: PWideChar; APattern: PWideChar; AReplacement: PWideChar): PWideChar;
var
  APos, AEnd, ANext, APrev: PWideChar;
  AReplacementLength, APatternLength: Integer;
begin
  APos := StrPos(S, APattern);
  if APos <> nil then
  begin
    AReplacementLength := StrLen(AReplacement);
    APatternLength := StrLen(APattern);
    if AReplacementLength < APatternLength then
    begin
      AEnd := S + StrLen(S);
      ANext := APos + AReplacementLength;
      APrev := APos + APatternLength;
      while APrev < AEnd do
      begin
        ANext^ := APrev^;
        Inc(APrev);
        Inc(ANext);
      end;
      ANext^ := #0;
    end
    else if AReplacementLength > APatternLength then
    begin
      AEnd := APos + APatternLength;
      ANext := S + StrLen(S) + AReplacementLength - APatternLength;
      APrev := ANext - AReplacementLength + APatternLength;
      while APrev >= AEnd do
      begin
        ANext^ := APrev^;
        Dec(APrev);
        Dec(ANext);
      end;
    end;
    StrLCopy(APos, AReplacement, AReplacementLength);
  end;
  Result := S;
end;

function StrReverse(S: PWideChar): PWideChar;
var
  C: WideChar;
  P: PWideChar;
begin
  Result := S;
  P := S + StrLen(S) - 1;
  while S < P do
  begin
    C := S^;
    S^ := P^;
    P^ := C;
    Inc(S);
    Dec(P);
  end;
end;

function StrSeparate(S: PPWideChar; const ADelimiter: WideChar): PWideChar;
var
  ALineStart, ADelimiterPos: PWideChar;
begin
  Result := nil;
  ALineStart := S^;
  if ALineStart^ <> #0 then
  begin
    if ADelimiter <> #0 then
      ADelimiterPos := StrScan(ALineStart, ADelimiter)
    else
    begin
      ADelimiterPos := ALineStart;
      while (ADelimiterPos^ <> #0) and (ADelimiterPos^ <> ' ') and (ADelimiterPos^ <> #9) do
        Inc(ADelimiterPos);
      if ADelimiterPos^ = #0 then
        ADelimiterPos := nil;
    end;
    if ADelimiterPos <> nil then
    begin
      S^ := ADelimiterPos + 1;
      ADelimiterPos^ := #0;
    end
    else
      S^ := ALineStart + StrLen(ALineStart);
    Result := ALineStart;
  end;
end;


end.
