{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFunctionsText;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, StrUtils, DateUtils, Variants, dxCore, cxClasses, cxVariants,
  //
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetFunctions,
  dxSpreadSheetNumberFormat,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnChar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnClean(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCode(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnConcatenate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDollar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnExact(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFind(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFixed(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLeft(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLen(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLower(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMid(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnProper(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnReplace(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRept(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRight(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSearch(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSubstitute(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnText(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTrim(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnUpper(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiChar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiClean(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCode(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiConcatenate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDollar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiExact(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFind(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFixed(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLeft(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLen(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLower(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMid(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiProper(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiReplace(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRept(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRight(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSearch(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSubstitute(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiText(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTrim(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiUpper(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

function dxMaskSearch(const AMask, AText: string; AStartSearch: Integer; const APartialMatch: Boolean = True): Integer;
function dxSearchValueInText(const AValue, AText: Variant; AStartSearch: Integer; const APartialMatch: Boolean = True): Integer;
implementation

uses
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetFunctionsStrs, dxSpreadSheetFunctionsMath, dxSpreadSheetCoreStrs;

type
  { TdxMaskToken }

  TdxMaskToken = class
  private
    FNum: Integer;
    FOwner: TcxObjectList;
    FToken: string;
    FWildChar: Char;
    function GetLength: Integer;
    function GetNextToken: TdxMaskToken;
  public
    constructor Create(AOwner: TcxObjectList; ANum: Integer; AToken: string; AWildChar: Char);
    function IsPinned: Boolean;

    property Length: Integer read GetLength;
    property NextToken: TdxMaskToken read GetNextToken;
    property Num: Integer read FNum;
    property Owner: TcxObjectList read FOwner;
    property Token: string read FToken;
    property WildChar: Char read FWildChar;
  end;

  constructor TdxMaskToken.Create(AOwner: TcxObjectList; ANum: Integer; AToken: string; AWildChar: Char);
  begin
    inherited Create;
    FOwner := AOwner;
    FNum := ANum;
    FWildChar := AWildChar;
    FToken := AToken;
  end;

  function TdxMaskToken.GetLength: Integer;
  begin
    Result := System.Length(FToken);
    if WildChar = '?' then
      Inc(Result);
  end;

  function TdxMaskToken.GetNextToken: TdxMaskToken;
  begin
    Result := nil;
    if Num < Owner.Count - 1 then
      Result := TdxMaskToken(Owner[Num + 1]);
  end;

  function TdxMaskToken.IsPinned: Boolean;
  begin
    Result := (Num > 0) and (TdxMaskToken(Owner[Num - 1]).WildChar <> '*');
  end;

function dxMaskSearch(const AMask, AText: string; AStartSearch: Integer; const APartialMatch: Boolean = True): Integer;

  function CleanToken(const AToken: string): string;
  const
    ACleanStr: array[0..2] of string = ('~*', '~?', '""');
  var
    I, P: Integer;
  begin
    Result := AToken;
    for I := Low(ACleanStr) to High(ACleanStr) do
    begin
      P := Pos(ACleanStr[I], Result);
      while P > 0 do
      begin
        Delete(Result, P, 1);
        P := Pos(ACleanStr[I], Result);
      end;
    end;
  end;

  procedure PopulateTokenList(ATokenList: TcxObjectList; const S: string);
  var
    ANum, I, L: Integer;
    ATokenIsReady: Boolean;
    AToken: string;
    AWildChar: Char;
  begin
    ANum := 0;
    AToken := '';
    AWildChar := #0;
    L := Length(S);
    for I := 1 to L do
    begin
      ATokenIsReady := CharInSet(S[I], ['*', '?']) and ((I = 1) or ((I > 1) and (S[I - 1] <> '~')));
      if ATokenIsReady then
        AWildChar := S[I]
      else
      begin
        AToken := AToken + S[I];
        ATokenIsReady := I = L;
      end;

      if ATokenIsReady then
      begin
        AToken := CleanToken(AToken);
        ATokenList.Add(TdxMaskToken.Create(ATokenList, ANum, AToken, AWildChar));
        Inc(ANum);
        AToken := '';
        AWildChar := #0;
      end;
    end;
  end;

  function ExecuteSearch(AToken: TdxMaskToken; const S: string; AStart: Integer): Integer;
  var
    ANextToken: TdxMaskToken;
  begin
    Result := 0;
    while AStart + AToken.Length - 1 <= Length(S) do
    begin
      if AToken.Token = '' then
        Result := AStart
      else
      begin
        Result := PosEx(AToken.Token, S, AStart);
        if ((Result > AStart) and AToken.IsPinned) or (Result + AToken.Length - 1 > Length(S)) then
          Result := 0;
      end;
      if Result > 0 then
      begin
        ANextToken := AToken.NextToken;
        if (ANextToken <> nil) and (ExecuteSearch(ANextToken, S, Result + AToken.Length) = 0) then
          Result := 0;
      end;
      if (Result = 0) and not AToken.IsPinned then
        Inc(AStart)
      else
        Break;
    end;
  end;

var
  ATokenList: TcxObjectList;
  ARealMask, ARealText: string;
begin
  Result := 0;
  if (AStartSearch < 1) or (AStartSearch > Length(AText)) then
    Exit;
  ATokenList := TcxObjectList.Create;
  try
    ARealMask := dxSpreadSheetUpperCase(AMask);
    ARealText := dxSpreadSheetUpperCase(AText);
    PopulateTokenList(ATokenList, ARealMask);
    if not APartialMatch and (ATokenList.Count = 1) and (TdxMaskToken(ATokenList[0]).WildChar = #0) then
    begin
      if TdxMaskToken(ATokenList[0]).Token = ARealText then
        Result := 1;
    end
    else
      Result := ExecuteSearch(TdxMaskToken(ATokenList[0]), ARealText, AStartSearch);
  finally
    ATokenList.Free;
  end;
end;

function dxSearchValueInText(const AValue, AText: Variant; AStartSearch: Integer; const APartialMatch: Boolean = True): Integer;
begin
  Result := 0;
  if VarIsStr(AValue) and VarIsStr(AText) then
    Result := dxMaskSearch(VarToStr(AValue), VarToStr(AText), AStartSearch, APartialMatch);
end;

function dxConvertFloatToString(const ANumber: Extended; const AFloatFormat: TFloatFormat; const ADigits: Integer): string;
const
  APrecision = 18;
begin
  Result := FloatToStrF(ANumber, AFloatFormat, APrecision, ADigits);
  if ADigits < 0 then
  {$IFDEF CPUX64}
    Delete(Result, Length(Result), 1)
  {$ELSE}
    Delete(Result, Length(Result) - APrecision, APrecision + 1);
  {$ENDIF}
end;

function dxGetAsCurrencyString(ANumber: Extended; ADigits: Integer; const AFormatSettings: TdxSpreadSheetFormatSettings): string;
begin
  Result := dxConvertFloatToString(Abs(ANumber), ffNumber, ADigits);
  if ANumber >= 0 then
    Result := TdxSpreadSheetCurrencyFormatHelper.GetPositiveFormat(AFormatSettings.CurrencyFormat, Result, AFormatSettings.Data.CurrencyFormat)
  else
    Result := TdxSpreadSheetCurrencyFormatHelper.GetNegativeFormat(AFormatSettings.CurrencyFormat, Result, AFormatSettings.Data.NegCurrFormat);
end;

function dxSubstitute(var AText: string; const AFromText, AToText: string; AReplaceAll: Boolean; AInstance: Integer): Boolean;
var
  AStart, ANum: Integer;
begin
  Result := AReplaceAll or (AInstance > 0);
  if not Result or (AFromText = '') then
    Exit;
  if AReplaceAll then
    AText := ReplaceStr(AText, AFromText, AToText)
  else
  begin
    AStart := 1;
    ANum := 1;
    while (AStart <> 0) and (AStart <= Length(AText)) do
    begin
      AStart := PosEx(AFromText, AText, AStart);
      if (AStart > 0) and (ANum = AInstance) then
      begin
        Delete(AText, AStart, Length(AFromText));
        Insert(AToText, AText, AStart);
      end
      else
      begin
        Inc(AStart, Integer(AStart > 0));
        Inc(ANum);
      end;
    end;
  end;
end;

procedure fnChar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
  begin
    AParameter := Trunc(AParameter);
    if (AParameter < 1) or (AParameter > 255) then
      Sender.SetError(ecValue)
    else
      Sender.AddValue(Chr(Byte(AParameter)));
  end;
end;

procedure fnClean(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
  st, AResult: string;
  I: Integer;
begin
  AParameter := Sender.ExtractStringParameter(AParams);
  st := AParameter;
  AResult := '';
  for I := 1 to Length(st) do
    if not CharInSet(st[I], [#0.. #31]) then
      AResult := AResult + st[I];
  Sender.AddValue(AResult);
end;

procedure fnCode(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  st: string;
begin
  st := VarToStr(Sender.ExtractStringParameter(AParams));
  if st = '' then
    Sender.SetError(ecValue)
  else
    Sender.AddValue(Ord(st[1]));
end;

procedure fnConcatenate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AItem: TdxSpreadSheetFormulaToken;
  AParameter: string;
  AResult: string;
begin
  AResult := '';
  AItem := AParams;
  while (AItem <> nil) and Sender.Validate do
  begin
    AParameter := Sender.ExtractStringParameter(AItem);
    if Sender.Validate then
      AResult := AResult + AParameter;
    AItem := AItem.Next;
  end;
  if Sender.Validate then
    Sender.AddValue(AResult);
end;

procedure fnDollar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADigits: Variant;
  AExtNumber: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(ADigits, 2, 0, AParams, 1) then
  begin
    ADigits := Trunc(ADigits);
    AExtNumber := ANumber;
    if dxSpreadSheetRound(Sender, AExtNumber, ADigits) then
      Sender.AddValue(dxGetAsCurrencyString(AExtNumber, ADigits, Sender.FormatSettings));
  end;
end;

procedure fnExact(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AText1, AText2: string;
begin
  AText1 := VarToStr(Sender.ExtractStringParameter(AParams));
  AText2 := VarToStr(Sender.ExtractStringParameter(AParams.Next));
  Sender.AddValue(AnsiCompareStr(AText1, AText2) = 0);
end;

procedure fnFind(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ASubStr, AStr: string;
  AStart: Variant;
  AIntStart, AResult: Integer;
begin
  ASubStr := VarToStr(Sender.ExtractStringParameter(AParams));
  if Sender.Validate then
  begin
    AStr := VarToStr(Sender.ExtractStringParameter(AParams.Next));
    if Sender.Validate and Sender.ExtractNumericParameterDef(AStart, 1, 0, AParams, 2) then
    begin
      AIntStart := Trunc(AStart);
      if (AIntStart < 1) or (AIntStart > Length(AStr)) then
        Sender.SetError(ecValue)
      else
        if ASubStr = '' then
          Sender.AddValue(AIntStart)
        else
        begin
          AResult := PosEx(ASubStr, AStr, AIntStart);
          if AResult > 0 then
            Sender.AddValue(AResult)
          else
            Sender.SetError(ecValue);
        end;
    end;
  end;
end;

procedure fnFixed(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
const
  AFormat: array[Boolean] of TFloatFormat = (ffFixed, ffNumber);
var
  ANumber, ADigits, ANoCommas: Variant;
  AExtNumber: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and
     Sender.ExtractNumericParameterDef(ADigits, 2, 2, AParams, 1) and
     Sender.ExtractNumericParameterDef(ANoCommas, 0, 0, AParams, 2) then
  begin
    ADigits := Trunc(ADigits);
    AExtNumber := Extended(ANumber);
    if dxSpreadSheetRound(Sender, AExtNumber, ADigits) then
      Sender.AddValue(dxConvertFloatToString(AExtNumber, AFormat[Trunc(ANoCommas) = 0], ADigits));
  end;
end;

procedure fnLeft(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AStr: string;
  ALength: Variant;
  AIntLength: Integer;
begin
  AStr := VarToStr(Sender.ExtractStringParameter(AParams));
  if Sender.ExtractNumericParameterDef(ALength, 1, 0, AParams, 1) then
    if ALength < 0 then
      Sender.SetError(ecValue)
    else
    begin
      AIntLength := Trunc(ALength);
      if AIntLength = 0 then
        Sender.AddValue('')
      else
        if AIntLength > Length(AStr) then
          Sender.AddValue(AStr)
        else
          Sender.AddValue(LeftStr(AStr, AIntLength));
    end;
end;

procedure fnLen(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(Length(Sender.ExtractStringParameter(AParams)));
end;

procedure fnLower(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(dxSpreadSheetLowerCase(Sender.ExtractStringParameter(AParams)));
end;

procedure fnMid(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AStr: string;
  AStart, ACount: Variant;
  AIntStart, AIntCount: Integer;
begin
  AStr := VarToStr(Sender.ExtractStringParameter(AParams));
  if Sender.ExtractNumericParameter(AStart, AParams, 1) and Sender.ExtractNumericParameter(ACount, AParams, 2) then
  begin
    AIntStart := Trunc(AStart);
    AIntCount := Trunc(ACount);
    if (AIntStart < 1) or (AIntCount < 0) then
      Sender.SetError(ecValue)
    else
      if (AIntStart > Length(AStr)) or (AIntCount = 0) then
        Sender.AddValue('')
      else
        Sender.AddValue(MidStr(AStr, AIntStart, AIntCount));
  end;
end;

procedure fnProper(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function IsPriorCharNonLetter(const ch: Char): Boolean;
  begin
    Result := (WideUpperCase(ch) = ch) and (WideLowerCase(ch) = ch);
  end;

var
  S, AResult: string;
  ALen, I: Integer;
begin
  S := dxSpreadSheetLowerCase(Sender.ExtractStringParameter(AParams));
  AResult := '';
  ALen := Length(S);
  if ALen > 0 then
    for I := 1 to ALen do
      if (I = 1) or ((I > 1) and IsPriorCharNonLetter(S[I - 1])) then
        AResult := AResult + WideUpperCase(S[I])
      else
        AResult := AResult + S[I];
  Sender.AddValue(AResult);
end;

procedure fnReplace(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ASource, ANew: string;
  AStart, ACount: Variant;
  AIntStart, AIntCount: Integer;
begin
  ASource := Sender.ExtractStringParameter(AParams);
  ANew := Sender.ExtractStringParameter(AParams, 3);
  if Sender.ExtractNumericParameter(AStart, AParams, 1) and Sender.ExtractNumericParameter(ACount, AParams, 2) then
  begin
    AIntStart := Trunc(AStart);
    AIntCount := Trunc(ACount);
    if (AIntStart < 1) or (AIntCount < 0) then
      Sender.SetError(ecValue)
    else
    begin
      Delete(ASource, AIntStart, AIntCount);
      Insert(ANew, ASource, AIntStart);
      Sender.AddValue(ASource);
    end;
  end;
end;

procedure fnRept(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AText, AResult: string;
  ACount: Variant;
  I: Integer;
begin
  AText := Sender.ExtractStringParameter(AParams);
  if Sender.ExtractNumericParameter(ACount, AParams, 1) then
  begin
    ACount := Trunc(ACount);
    if (ACount < 0) or (ACount * Length(AText) > 32767) then
      Sender.SetError(ecValue)
    else
    begin
      AResult := '';
      for I := 1 to ACount do
        AResult := AResult + AText;
      Sender.AddValue(AResult);
    end;
  end;
end;

procedure fnRight(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AStr: string;
  ACount: Variant;
  AIntCount: Integer;
begin
  AStr := VarToStr(Sender.ExtractStringParameter(AParams));
  if Sender.ExtractNumericParameterDef(ACount, 1, 0, AParams, 1) then
    if ACount < 0 then
      Sender.SetError(ecValue)
    else
    begin
      AIntCount := Trunc(ACount);
      if AIntCount = 0 then
        Sender.AddValue('')
      else
        if AIntCount > Length(AStr) then
          Sender.AddValue(AStr)
        else
          Sender.AddValue(RightStr(AStr, AIntCount));
    end;
end;

procedure fnSearch(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ASubStr, S: string;
  AStart: Variant;
  AResult: Integer;
begin
  ASubStr := VarToStr(Sender.ExtractStringParameter(AParams));
  S := VarToStr(Sender.ExtractStringParameter(AParams, 1));
  if Sender.ExtractNumericParameterDef(AStart, 1, AParams, 2) then
  begin
    AStart := Trunc(AStart);
    if (AStart < 1) or (AStart > Length(S)) then
      Sender.SetError(ecValue)
    else
      if ASubStr = '' then
        Sender.AddValue(AStart)
      else
      begin
        AResult := dxMaskSearch(ASubStr, S, AStart);
        if AResult > 0 then
          Sender.AddValue(AResult)
        else
          Sender.SetError(ecValue);
      end;
  end;
end;

procedure fnSubstitute(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AText, AOld, ANew: string;
  AInstance: Variant;
  AReplaceAll: Boolean;
begin
  AText := Sender.ExtractStringParameter(AParams);
  AOld := Sender.ExtractStringParameter(AParams, 1);
  ANew := Sender.ExtractStringParameter(AParams, 2);
  AReplaceAll := not Sender.ParameterExists(AParams, 3);
  AInstance := 0;
  if not AReplaceAll then
    if not Sender.ExtractNumericParameter(AInstance, AParams, 3) then
      Exit;
  if dxSubstitute(AText, AOld, ANew, AReplaceAll, Trunc(AInstance)) then
    Sender.AddValue(AText)
  else
    Sender.SetError(ecValue);
end;

procedure fnT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    if VarIsStr(AParameter) then
      Sender.AddValue(AParameter)
    else
      Sender.AddValue('');
end;

procedure fnText(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AFormatText: Variant;
  AParameter: Variant;
  AResult: TdxSpreadSheetNumberFormatResult;
begin
  if Sender.ExtractParameterDef(AParameter, 0, AParams) and Sender.ExtractParameterDef(AFormatText, '', AParams, 1) then
    if VarIsStr(AFormatText) then
    begin
      AFormatText := StringReplace(StringReplace(AFormatText, dxStringMarkChar + dxStringMarkChar, dxStringMarkChar, [rfReplaceAll]),
        dxStringMarkChar2 + dxStringMarkChar2, dxStringMarkChar2, [rfReplaceAll]);
      if (AFormatText = '') and dxIsNumberOrDateTime(AParameter) then
        Sender.AddValue('')
      else
      begin
        AResult := dxGetFormattedResult(AParameter, AFormatText, Sender.Owner.FormatSettings);
        if AResult.IsError then
          Sender.SetError(ecValue)
        else
          Sender.AddValue(AResult.Text);
      end;
    end
    else
      Sender.SetError(ecValue);
end;

procedure fnTrim(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  S: string;
  APos: Integer;
begin
  S := Sender.ExtractStringParameter(AParams);
  S := Trim(S);
  repeat
    APos := Pos('  ', S);
    if APos > 0 then
      Delete(S, APos, 1);
  until APos = 0;
  Sender.AddValue(S);
end;

procedure fnUpper(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(dxSpreadSheetUpperCase(Sender.ExtractStringParameter(AParams)));
end;

procedure fnValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function CanConvertToNumber(var AValue: Variant): Boolean;
  begin
    Result := dxTryStrToOrdinal(VarToStr(AValue), AValue, AParams.FormatSettings);
    if Result then
      Sender.SetError(ecNone);
  end;

var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameterWithoutBoolean(AParameter, AParams) or
     Sender.ExtractDateTimeParameterWithoutBoolean(AParameter, AParams) then
    Sender.AddValue(AParameter)
  else
  begin
    Sender.SetError(ecNone);
    if Sender.ExtractParameter(AParameter, AParams) and (VarIsStr(AParameter) and CanConvertToNumber(AParameter)) then
      Sender.AddValue(AParameter);
  end;
end;

procedure fpiChar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiClean(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCode(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiConcatenate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiDollar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiExact(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiFind(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiFixed(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiLeft(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiLen(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiLower(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiMid(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiProper(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiReplace(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiRept(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiRight(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiSearch(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiSubstitute(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
end;

procedure fpiT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiText(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiTrim(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiUpper(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnASC, nil, nil, frkValue, 214, ftText, @sfnASCDescription);
  ARepository.Add(@sfnBahtText, nil, nil, frkValue, 368, ftText, @sfnBahtTextDescription);
  ARepository.Add(@sfnChar, fnChar, fpiChar, frkValue, 111, ftText, @sfnCharDescription);
  ARepository.Add(@sfnClean, fnClean, fpiClean, frkValue, 162, ftText, @sfnCleanDescription);
  ARepository.Add(@sfnCode, fnCode, fpiCode, frkValue, 121, ftText, @sfnCodeDescription);
  ARepository.Add(@sfnConcatenate, fnConcatenate, fpiConcatenate, frkNonArrayValue, 336, ftText, @sfnConcatenateDescription);
  ARepository.Add(@sfnDBCS, nil, nil, frkValue, 215, ftText, @sfnDBCSDescription);
  ARepository.Add(@sfnDollar, fnDollar, fpiDollar, frkValue, 13, ftText, @sfnDollarDescription);
  ARepository.Add(@sfnExact, fnExact, fpiExact, frkValue, 117, ftText, @sfnExactDescription);
  ARepository.Add(@sfnFind, fnFind, fpiFind, frkValue, 124, ftText, @sfnFindDescription);
  ARepository.Add(@sfnFindB, nil, nil, frkValue, 205, ftText, @sfnFindBDescription);
  ARepository.Add(@sfnFixed, fnFixed, fpiFixed, frkValue, 14, ftText, @sfnFixedDescription);
  ARepository.Add(@sfnLeft, fnLeft, fpiLeft, frkValue, 115, ftText, @sfnLeftDescription);
  ARepository.Add(@sfnLeftB, nil, nil, frkValue, 208, ftText, @sfnLeftBDescription);
  ARepository.Add(@sfnLen, fnLen, fpiLen, frkValue, 32, ftText, @sfnLenDescription);
  ARepository.Add(@sfnLenB, nil, nil, frkValue, 211, ftText, @sfnLenBDescription);
  ARepository.Add(@sfnLower, fnLower, fpiLower, frkValue, 112, ftText, @sfnLowerDescription);
  ARepository.Add(@sfnMid, fnMid, fpiMid, frkValue, 31, ftText, @sfnMidDescription);
  ARepository.Add(@sfnMidB, nil, nil, frkValue, 210, ftText, @sfnMidBDescription);
  ARepository.Add(@sfnNumberValue, nil, nil, frkValue, 255, ftText, @sfnNumberValueDescription, 1);
  ARepository.Add(@sfnPhonetic, nil, nil, frkValue, 360, ftText, @sfnPhoneticDescription);
  ARepository.Add(@sfnProper, fnProper, fpiProper, frkValue, 114, ftText, @sfnProperDescription);
  ARepository.Add(@sfnReplace, fnReplace, fpiReplace, frkValue, 119, ftText, @sfnReplaceDescription);
  ARepository.Add(@sfnReplaceB, nil, nil, frkValue, 207, ftText, @sfnReplaceBDescription);
  ARepository.Add(@sfnRept, fnRept, fpiRept, frkValue, 30, ftText, @sfnReptDescription);
  ARepository.Add(@sfnRight, fnRight, fpiRight, frkValue, 116, ftText, @sfnRightDescription);
  ARepository.Add(@sfnRightB, nil, nil, frkValue, 209, ftText, @sfnRightBDescription);
  ARepository.Add(@sfnSearch, fnSearch, fpiSearch, frkValue, 82, ftText, @sfnSearchDescription);
  ARepository.Add(@sfnSearchB, nil, nil, frkValue, 206, ftText, @sfnSearchBDescription);
  ARepository.Add(@sfnSubstitute, fnSubstitute, fpiSubstitute, frkValue, 120, ftText, @sfnSubstituteDescription);
  ARepository.Add(@sfnT, fnT, fpiT, frkValue, 130, ftText, @sfnTDescription);
  ARepository.Add(@sfnText, fnText, fpiText, frkValue, 48, ftText, @sfnTextDescription);
  ARepository.Add(@sfnTrim, fnTrim, fpiTrim, frkValue, 118, ftText, @sfnTrimDescription);
  ARepository.Add(@sfnUniChar, nil, nil, frkValue, 255, ftText, @sfnUniCharDescription, 1);
  ARepository.Add(@sfnUniCode, nil, nil, frkValue, 255, ftText, @sfnUniCodeDescription, 1);
  ARepository.Add(@sfnUpper, fnUpper, fpiUpper, frkValue, 113, ftText, @sfnUpperDescription);
  ARepository.Add(@sfnValue, fnValue, fpiValue, frkValue, 33, ftText, @sfnValueDescription)
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
