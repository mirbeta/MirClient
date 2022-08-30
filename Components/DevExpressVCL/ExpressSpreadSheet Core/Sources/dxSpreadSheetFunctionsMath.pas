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

unit dxSpreadSheetFunctionsMath;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, DateUtils, Variants, Math, Classes, dxCore,
  // SpreadSheet
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnABS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnACOS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnACOSH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnACOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnACOTH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnASIN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnASINH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnATAN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnATAN2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnATANH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBase(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCeiling(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCOS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCOSH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCOTH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCSC(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCSCH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCeiling_Math(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCeiling_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCombin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCombinA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDecimal(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDegrees(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnExp(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnEven(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFact(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFactDouble(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFloor(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFloor_Math(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFloor_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnINT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIso_Ceiling(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLOG(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLOG10(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMMult(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMOD(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMRound(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnOdd(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPI(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPower(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnQuotient(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRadians(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRand(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRandBetween(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRound(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRoundDown(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRoundUp(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSec(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSech(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSign(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSinH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSQRT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSQRTPI(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumX2MY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumX2PY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumXMY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTan(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTanH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTrunc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiABS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiACOS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiACOSH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiACOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiACOTH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiASIN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiASINH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiATAN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiATAN2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiATANH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBase(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCeiling(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCeiling_Math(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCeiling_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCOS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCOSH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCOTH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCombin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCombinA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCSC(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCSCH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDecimal(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDegrees(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiExp(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiEven(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFact(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFactDouble(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFloor(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFloor_Math(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFloor_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiINT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIso_Ceiling(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLOG(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLOG10(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMMult(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMOD(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMRound(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiOdd(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPI(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPower(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiProduct(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiQuotient(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRadians(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRand(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRandBetween(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRound(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRoundDown(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRoundUp(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSec(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSech(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSign(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSinH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSQRT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSQRTPI(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumProduct(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumX2MY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumX2PY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSumXMY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTan(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTanH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTrunc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

function dxSpreadSheetRound(Sender: TdxSpreadSheetFormulaResult; var ANumber: Extended; const ADigits: Integer): Boolean; overload;
procedure dxSpreadSheetRound(var ANumber: Extended; const APrecision: Extended); overload;
implementation

uses
  cxVariants,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetFunctionsStatistical,
  dxSpreadSheetFunctionsStrs,
  dxSpreadSheetFunctions;

type
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);

const
  Resolution = 1E-15;
  AllowedChars: array[0..35] of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
     'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

function dxGetCharIndex(AChar: Char): Integer; inline;
var
  I: Byte;
begin
  Result := -1;
  for I := 0 to 35 do
    if AChar = AllowedChars[I] then
    begin
      Result := I;
      Break;
    end;
end;

function dxGetDoubleFactorial(AValue: Integer): Extended;
begin
  Result := 1;
  while AValue > 1 do
  begin
    Result := Int(Result * AValue);
    AValue := AValue - 2;
  end;
end;

function dxGetCombinA(N, K: Integer): Extended;
var
  I: Integer;
begin
  Result := 1;
  for I := N to N + K - 1 do
    Result := Int(Result * I);
  Result := Result / dxGetFactorial(K);
end;

function dxGetRoundPrecision(Sender: TdxSpreadSheetFormulaResult; const ADigits: Integer; var APrecision: Extended): Boolean;
var
  ABase: Extended;
begin
  Result := False;
  try
    ABase := 10;
    APrecision := IntPower(ABase, ADigits);
    Result := True;
  except
    on EOverflow do
      Sender.SetError(ecNUM)
  end;
end;

function dxExecRoundDown(Sender: TdxSpreadSheetFormulaResult; var ANumber: Extended; const ADigits: Integer): Boolean;
var
  APrecision, AProduct: Extended;
  AMultiplier: Int64;
begin
  Result := dxGetRoundPrecision(Sender, ADigits, APrecision);
  if Result then
  begin
    AProduct := Abs(ANumber * APrecision);
    AMultiplier := Trunc(AProduct);
    if Abs(Abs(AProduct - AMultiplier) - 1) <= 1E-10 then
      Inc(AMultiplier);
    ANumber := Sign(ANumber) * AMultiplier / APrecision;
  end;
end;

function dxSpreadSheetRound(Sender: TdxSpreadSheetFormulaResult; var ANumber: Extended; const ADigits: Integer): Boolean;
var
  APrecision: Extended;
begin
  Result := dxGetRoundPrecision(Sender, ADigits, APrecision);
  if Result then
    dxSpreadSheetRound(ANumber, APrecision);
end;

procedure dxSpreadSheetRound(var ANumber: Extended; const APrecision: Extended);
const
  LimitFracValue = 0.5;
var
  A, B: Extended;
  AEpsilon: Extended;
  AIntProduct: Extended;
  AProduct: Extended;
begin
  AProduct := ANumber * APrecision;
  AIntProduct := Int(AProduct);
  A := Abs(AProduct);
  B := Abs(AIntProduct + Sign(ANumber) * LimitFracValue);
  AEpsilon := Max(Min(A, B) * Resolution, Resolution);
  if CompareValue(A, B, AEpsilon) <> -1 then
    AIntProduct := AIntProduct + Sign(ANumber);
  ANumber := AIntProduct / APrecision;
end;

procedure fnABS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Abs(AParameter));
end;

procedure fnACOS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if Abs(AParameter) > 1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(ArcCos(AParameter));
end;

procedure fnACOSH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if Abs(AParameter) < 1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(ArcCosH(AParameter));
end;

procedure fnACOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(ArcCot(AParameter));
end;

procedure fnACOTH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if Abs(AParameter) <= 1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(ArcCotH(AParameter));
end;

procedure fnASIN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if Abs(AParameter) > 1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(ArcSin(AParameter));
end;

procedure fnASINH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(ArcSinH(AParameter));
end;

procedure fnATAN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(ArcTan(AParameter));
end;

procedure fnATAN2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameterX, AParameterY: Variant;
begin
  if Sender.ExtractNumericParameter(AParameterX, AParams) and Sender.ExtractNumericParameter(AParameterY, AParams.Next) then
    if (AParameterX = 0) and (AParameterY = 0) then
      Sender.SetError(ecDivByZero)
    else
      Sender.AddValue(ArcTan2(AParameterY, AParameterX));
end;

procedure fnATANH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if Abs(AParameter) >= 1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(ArcTanH(AParameter));
end;

procedure fnBase(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function GetBase(ANumber: Int64; const ARadix, AMinLength: Int64): string;
  var
    ARatio: Int64;
    I: Integer;
  begin
    Result := '';
    while ANumber >= ARadix do
    begin
      ARatio := ANumber div ARadix;
      Result := AllowedChars[ANumber - ARatio * ARadix] + Result;
      ANumber := ARatio;
    end;
    Result := AllowedChars[ANumber] + Result;
    for I := Length(Result) + 1 to AMinLength do
      Result := '0' + Result;
  end;

var
  ANumber, ARadix, AMinLength: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ARadix, AParams, 1) and
    Sender.ExtractNumericParameterDef(AMinLength, 0, 0, AParams, 2) then
  begin
    ARadix := Trunc(ARadix);
    if (ANumber < 0) or (ARadix < 2) or (ARadix > 36) or (AMinLength < 0) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(GetBase(Trunc(ANumber), ARadix, Trunc(AMinLength)));
  end;
end;

procedure fnCeiling(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ASignificance, AParams.Next) then
    if (ANumber > 0) and (ASignificance < 0) then
      Sender.SetError(ecNUM)
    else
      if (ANumber = 0) or (ASignificance = 0) then
        Sender.AddValue(0)
      else
      begin
        ARatio := Abs(ANumber / ASignificance);
        AMultiplier := Trunc(ARatio);
        if Frac(ARatio) > Resolution then
          Inc(AMultiplier, Integer(ANumber * ASignificance > 0));
        Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
      end;
end;

procedure fnCOS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Cos(AParameter));
end;

procedure fnCOSH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(CosH(AParameter));
end;

procedure fnCOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter = 0 then
      Sender.SetError(ecDivByZero)
    else
      Sender.AddValue(Cot(AParameter));
end;

procedure fnCOTH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter = 0 then
      Sender.SetError(ecDivByZero)
    else
      Sender.AddValue(CotH(AParameter));
end;

procedure fnCSC(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter = 0 then
      Sender.SetError(ecDivByZero)
    else
      Sender.AddValue(Cosecant(AParameter));
end;

procedure fnCSCH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter = 0 then
      Sender.SetError(ecDivByZero)
    else
      Sender.AddValue(CscH(AParameter));
end;

procedure fnCeiling_Math(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance, AMode: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(ASignificance, 1, 1, AParams, 1) and
     Sender.ExtractNumericParameterDef(AMode, 0, 0, AParams, 2) then
    if (ANumber = 0) or (ASignificance = 0) then
      Sender.AddValue(0)
    else
    begin
      ARatio := Abs(ANumber / ASignificance);
      AMultiplier := Trunc(ARatio);
      if ANumber > 0 then
        Inc(AMultiplier, Integer(Frac(ARatio) <> 0))
      else
        Inc(AMultiplier, Integer((Frac(ARatio) <> 0) and (AMode <> 0)));
      Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
    end;
end;

procedure fnCeiling_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(ASignificance, 1, 1, AParams, 1) then
    if (ANumber = 0) or (ASignificance = 0) then
      Sender.AddValue(0)
    else
    begin
      ARatio := Abs(ANumber / ASignificance);
      AMultiplier := Trunc(ARatio);
      Inc(AMultiplier, Integer((ANumber > 0) and (Frac(ARatio) <> 0)));
      Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
    end;
end;

procedure fnCombin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, AChosen: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(AChosen, AParams.Next) then
  begin
    ANumber := Trunc(ANumber);
    AChosen := Trunc(AChosen);
    if (ANumber < 0) or (AChosen < 0) or (ANumber < AChosen) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetCombin(ANumber, AChosen));
  end;
end;

procedure fnCombinA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, AChosen: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(AChosen, AParams.Next) then
  begin
    ANumber := Trunc(ANumber);
    AChosen := Trunc(AChosen);
    if (ANumber < 0) or (AChosen < 0) or ((ANumber = 0) and (ANumber < AChosen)) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetCombinA(ANumber, AChosen));
  end;
end;

procedure fnDecimal(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function GetDecimal(const AText: string; const ARadix: Int64): Int64;
  var
    I, P: Integer;
  begin
    Result := 0;
    for I := Length(AText) downto 1 do
    begin
      P := Length(AText) - I;
      Result := Result + dxGetCharIndex(AText[I]) * Trunc(IntPower(ARadix, P));
    end;
  end;

  function CheckArguments(const AText: string; const ARadix: Int64): Boolean;
  var
    I, AIndex: Integer;
  begin
    Result := ARadix in [2..36];
    if Result then
      for I := 1 to Length(AText) do
      begin
        AIndex := dxGetCharIndex(AText[I]);
        if (AIndex = -1) or (AIndex > ARadix - 1) then
        begin
          Result := False;
          Break;
        end;
      end;
  end;

var
  AText, ARadix: Variant;
begin
  AText := dxSpreadSheetUpperCase(Sender.ExtractStringParameter(AParams));
  if Sender.ExtractNumericParameter(ARadix, AParams.Next) then
  begin
    ARadix := Trunc(ARadix);
    if CheckArguments(AText, ARadix) then
      Sender.AddValue(GetDecimal(AText, ARadix))
    else
      Sender.SetError(ecNUM);
  end;
end;

procedure fnDegrees(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(RadToDeg(AParameter));
end;

procedure fnExp(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Exp(AParameter));
end;

procedure fnEven(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
  ANumber: Int64;
begin
  if not Sender.ExtractNumericParameter(AParameter, AParams) then
    Exit;
  ANumber:= Abs(Trunc(AParameter));
  if Odd(ANumber) then
    Inc(ANumber)
  else
    if Frac(AParameter) <> 0 then
      Inc(ANumber, 2);
  Sender.AddValue(Sign(Double(AParameter)) * ANumber);
end;

procedure fnFact(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if not Sender.ExtractNumericParameter(AParameter, AParams) then
    Exit
  else
    AParameter := Trunc(AParameter);
  if AParameter >= 0 then
    Sender.AddValue(dxGetFactorial(AParameter))
  else
    Sender.SetError(ecNUM);
end;

procedure fnFactDouble(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter < -1 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetDoubleFactorial(Trunc(AParameter)));
end;

procedure fnFloor(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ASignificance, AParams.Next) then
    if (ANumber > 0) and (ASignificance < 0) then
      Sender.SetError(ecNUM)
    else
      if ANumber = 0 then
        Sender.AddValue(0)
      else
        if ASignificance = 0 then
          Sender.SetError(ecDivByZero)
        else
        begin
          ARatio := Abs(ANumber / ASignificance);
          AMultiplier := Trunc(ARatio);
          if Frac(ARatio) > Resolution then
            Inc(AMultiplier, Integer((ANumber < 0) and (ASignificance > 0)));
          Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
        end;
end;

procedure fnFloor_Math(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance, AMode: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(ASignificance, 1, 1, AParams, 1) and
     Sender.ExtractNumericParameterDef(AMode, 0, 0, AParams, 2) then
    if (ANumber = 0) or (ASignificance = 0) then
      Sender.AddValue(0)
    else
    begin
      ARatio := Abs(ANumber / ASignificance);
      AMultiplier := Trunc(ARatio);
      if ANumber < 0 then
        Inc(AMultiplier, Integer((Frac(ARatio) <> 0) and (AMode = 0)));
      Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
    end;
end;

procedure fnFloor_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ASignificance: Variant;
  AMultiplier: Int64;
  ARatio: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(ASignificance, 1, 1, AParams, 1) then
    if (ANumber = 0) or (ASignificance = 0) then
      Sender.AddValue(0)
    else
    begin
      ARatio := Abs(ANumber / ASignificance);
      AMultiplier := Trunc(ARatio);
      Inc(AMultiplier, Integer((ANumber < 0) and (Frac(ARatio) <> 0)));
      Sender.AddValue(Sign(Double(ANumber)) * Abs(ASignificance) * AMultiplier);
    end;
end;

procedure fnINT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Floor(AParameter));
end;

procedure fnIso_Ceiling(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCeiling_Precise(Sender, AParams);
end;

procedure fnLN(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter <= 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(Ln(AParameter));
end;

procedure fnLOG(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ABase: Variant;
begin
  if Sender.ExtractNumericParameterDef(ANumber, 0, AParams) and Sender.ExtractNumericParameterDef(ABase, 10, 0, AParams, 1) then
    if ABase = 1 then
      Sender.SetError(ecDivByZero)
    else
      if (ANumber <= 0) or (ABase <= 0) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(LogN(ABase, ANumber));
end;

procedure fnLOG10(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter <= 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(Log10(AParameter));
end;

function CheckMMultDimensions(AMatrixToken1, AMatrixToken2: TdxSpreadSheetFormulaToken;
  var ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension): TdxSpreadSheetFormulaErrorCode;
begin
  Result := ecNone;
  ADimension1 := AMatrixToken1.GetDimension(Result);
  if Result = ecNone then
    ADimension2 := AMatrixToken2.GetDimension(Result);
  if (Result = ecNone) and (ADimension1.ColumnCount <> ADimension2.RowCount) then
    Result := ecValue;
end;

function GetMMultMatrix(AMatrixToken: TdxSpreadSheetFormulaToken; const ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
var
  ARow, AColumn, ARowCount, AColumnCount: Integer;
  AValue: Variant;
begin
  ARowCount := ADimension.RowCount;
  AColumnCount := ADimension.ColumnCount;
  Result := VarArrayCreate([0, ARowCount - 1, 0, AColumnCount - 1], varVariant);
  AErrorCode := ecNone;
  for ARow := 0 to ARowCount - 1 do
  begin
    for AColumn := 0 to AColumnCount - 1 do
    begin
      TTokenAccess(AMatrixToken).GetValueAsArrayItem(ARow, AColumn, AValue, AErrorCode);
      if (AErrorCode = ecNone) and not dxIsNumberOrDateTime(AValue) then
        AErrorCode := ecValue
      else
        Result[ARow, AColumn] := AValue;
      if AErrorCode <> ecNone then
        Break;
    end;
    if AErrorCode <> ecNone then
      Break;
  end;
  if AErrorCode <> ecNone then
    Result := Unassigned;
end;

function ExtractSourcesMatrix(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; var AMatrix1, AMatrix2: Variant): TdxSpreadSheetFormulaErrorCode;
var
  AMatrixToken1, AMatrixToken2: TdxSpreadSheetFormulaToken;
  ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension;
begin
  Result := ecNone;
  AMatrixToken2 := nil;
  AMatrixToken1 := AParams.FirstChild;
  dxSpreadSheetIsReferenceToken(AMatrixToken1, Sender);
  if (AMatrixToken1 <> nil) and (AParams.Next <> nil) then
    AMatrixToken2 := AParams.Next.FirstChild;
  dxSpreadSheetIsReferenceToken(AMatrixToken2, Sender);
  if (AMatrixToken1 = nil) or (AMatrixToken2 = nil) then
    Result := ecValue;

  if Result = ecNone then
    Result := CheckMMultDimensions(AMatrixToken1, AMatrixToken2, ADimension1, ADimension2);

  if Result = ecNone then
  begin
    AMatrix1 := GetMMultMatrix(AMatrixToken1, ADimension1, Result);
    if Result = ecNone then
      AMatrix2 := GetMMultMatrix(AMatrixToken2, ADimension2, Result);
  end;
  if Result <> ecNone then
  begin
    AMatrix1 := Unassigned;
    AMatrix2 := Unassigned;
  end;
end;

procedure fnMMult(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParentToken: TdxSpreadSheetFormulaToken;
  AMatrix1, AMatrix2: Variant;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AResult: TdxSpreadSheetFormulaArrayToken;
  ARow, AColumn, AIndex, AMatrix1RowCount, AMatrix1ColumnCount, AMatrix2ColumnCount: Integer;
  AValue: Variant;
begin
  AErrorCode := ExtractSourcesMatrix(Sender, AParams, AMatrix1, AMatrix2);
  if AErrorCode <> ecNone then
    Sender.SetError(AErrorCode)
  else
  begin
    AResult := TdxSpreadSheetFormulaArrayToken.Create;
    AMatrix1RowCount := VarArrayHighBound(AMatrix1, 1) + 1;
    AMatrix1ColumnCount := VarArrayHighBound(AMatrix1, 2) + 1;
    AMatrix2ColumnCount := VarArrayHighBound(AMatrix2, 2) + 1;
    for ARow := 0 to AMatrix1RowCount - 1 do
      for AColumn := 0 to AMatrix2ColumnCount - 1 do
      begin
        if (AColumn = 0) and (ARow > 0) then
          TdxSpreadSheetFormulaToken.AddChild(AResult, TdxSpreadSheetFormulaArrayRowSeparator.Create());

        AValue := 0;
        for AIndex := 0 to AMatrix1ColumnCount - 1 do
          AValue := AValue + AMatrix1[ARow, AIndex] * AMatrix2[AIndex, AColumn];

        AParentToken := TdxSpreadSheetFormulaToken.Create;
        TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaVariantToken.Create(AValue));
        TdxSpreadSheetFormulaToken.AddChild(AResult, AParentToken);
      end;
    Sender.AddTemporary(AResult);
  end;
end;

procedure fnMOD(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADivisor, AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ADivisor, AParams.Next) then
    if ADivisor = 0 then
      Sender.SetError(ecDivByZero)
    else
    begin
      AParameter := ANumber - ADivisor * Floor(ANumber / ADivisor);
      if ADivisor * AParameter < 0 then
        AParameter := -AParameter;
      Sender.AddValue(AParameter);
    end;
end;

procedure fnMRound(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, AMultiple: Variant;
  ARatio, AResult: Extended;
begin
  if dxSpreadSheetExtractedArgumentsAreNotNull(Sender, AParams, ANumber, AMultiple) then
    if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(AMultiple, AParams.Next) then
      if ANumber * AMultiple < 0 then
        Sender.SetError(ecNUM)
      else
        if (ANumber = 0) or (AMultiple = 0) then
          Sender.AddValue(0)
        else
        begin
          ARatio := ANumber / AMultiple;
          AResult := AMultiple * Trunc(ARatio);
          if Frac(ARatio) >= 0.5 then
            AResult := AMultiple * (Trunc(ARatio) + 1);
          Sender.AddValue(AResult);
        end;
end;

procedure fnOdd(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
  begin
    if (Abs(AParameter) - Abs(Trunc(AParameter))) <> 0 then
      AParameter := Trunc(AParameter + ValueIncr[AParameter >= 0]);
    if not Odd(Trunc(AParameter)) then
      AParameter := AParameter + ValueIncr[AParameter >= 0];
    Sender.AddValue(AParameter);
  end;
end;

procedure fnPI(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(Pi);
end;

procedure fnPower(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, APower: Variant;
begin
  if Sender.ExtractNumericParameterDef(ANumber, 0, AParams) and Sender.ExtractNumericParameterDef(APower, 0, 0, AParams.Next) then
  begin
    if (ANumber = 0) and (APower = 0) then
      Sender.SetError(ecNUM)
    else
      if (ANumber = 0) and (APower < 0) then
        Sender.SetError(ecDivByZero)
      else
        if (ANumber < 0) and (APower < 0) and (APower <> Trunc(APower)) then
          Sender.SetError(ecNUM)
        else
          Sender.AddValue(Power(ANumber, APower));
  end;
end;

procedure fnProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnProduct(Sender, AParams);
end;

procedure fnQuotient(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADivisor: Variant;
begin
  if dxSpreadSheetExtractedArgumentsAreNotNull(Sender, AParams, ANumber, ADivisor) then
    if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterWithoutBoolean(ADivisor, AParams.Next) then
      if ADivisor = 0 then
        Sender.SetError(ecDivByZero)
      else
        Sender.AddValue(Trunc(ANumber / ADivisor));
end;

procedure fnRadians(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(DegToRad(AParameter));
end;

procedure fnRand(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(Random);
end;

procedure fnRandBetween(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ABottom, ATop: Variant;
  AResult: Variant;
begin
  if dxSpreadSheetExtractedArgumentsAreNotNull(Sender, AParams, ABottom, ATop) and
    Sender.ExtractNumericParameterWithoutBoolean(ABottom, AParams) and
    Sender.ExtractNumericParameterWithoutBoolean(ATop, AParams.Next) then
      if ABottom > ATop then
        Sender.SetError(ecNUM)
      else
      begin
        AResult := Round(ABottom + Random(101) / 100 * (ATop - ABottom));
        if AResult < ABottom then
          AResult := Trunc(ABottom) + 1;
        if ATop - ABottom < 1 then
        begin
          if AResult > ATop then
            AResult := Min(AResult, Trunc(ATop) + 1);
        end
        else
          AResult := Min(AResult, Trunc(ATop));
        Sender.AddValue(AResult);
      end;
end;

procedure fnRound(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADigits: Variant;
  AResult: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ADigits, AParams.Next) then
  begin
    AResult := Extended(ANumber);
    if dxSpreadSheetRound(Sender, AResult, Trunc(ADigits)) then
      Sender.AddValue(AResult);
  end;
end;

procedure fnRoundDown(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADigits: Variant;
  AResult: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ADigits, AParams.Next) then
  begin
    AResult := Extended(ANumber);
    if dxExecRoundDown(Sender, AResult, Trunc(ADigits)) then
      Sender.AddValue(AResult);
  end;
end;

procedure fnRoundUp(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADigits: Variant;
  AMultiplier: Int64;
  APrecision, AProduct: Extended;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ADigits, AParams.Next) then
    if dxGetRoundPrecision(Sender, ADigits, APrecision) then
    begin
      ADigits := Trunc(ADigits);
      AProduct := Abs(ANumber * APrecision);
      AMultiplier := Trunc(AProduct);
      Inc(AMultiplier, Integer(Frac(AProduct) > Resolution));
      Sender.AddValue(Sign(Double(ANumber)) * AMultiplier / APrecision);
    end;
end;

procedure fnSec(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Secant(AParameter));
end;

procedure fnSech(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(SecH(AParameter));
end;

procedure fnSign(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Sign(Double(AParameter)));
end;

procedure fnSin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Sin(AParameter));
end;

procedure fnSinH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(SinH(AParameter));
end;

procedure fnSQRT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(SQRT(AParameter));
end;

procedure fnSQRTPI(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameterWithoutBoolean(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(SQRT(AParameter * Pi));
end;

procedure fnSum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSum(Sender, AParams);
end;

procedure fnSumIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumIF(Sender, AParams);
end;

procedure fnSumIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumIFS(Sender, AParams);
end;

procedure fnSumProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumProduct(Sender, AParams);
end;

procedure fnSumSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumSQ(Sender, AParams);
end;

procedure fnSumX2MY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumX2MY2(Sender, AParams);
end;

procedure fnSumX2PY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumX2PY2(Sender, AParams);
end;

procedure fnSumXMY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnSumXMY2(Sender, AParams);
end;

procedure fnTan(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(Tan(AParameter));
end;

procedure fnTanH(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameter(AParameter, AParams) then
    Sender.AddValue(TanH(AParameter));
end;

procedure fnTrunc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ADigits: Variant;
  AResult: Extended;
begin
  if Sender.ExtractNumericParameterDef(ANumber, 0, AParams) and Sender.ExtractNumericParameterDef(ADigits, 0, 0, AParams, 1) then
  begin
    AResult := Extended(ANumber);
    if dxExecRoundDown(Sender, AResult, Trunc(ADigits)) then
      Sender.AddValue(AResult);
  end;
end;

//
{
var
  Criteria: string;

function IfCompare(const Value: string): Boolean;
begin
  if (Length(Criteria) > 0) and dxCharInSet(Criteria[1], ['>', '<']) then
  begin
    if Criteria[1] = '>' then
    begin
      if (Length(Criteria) > 1) and (Criteria[2] = '=') then
        Result := AnsiCompareText(Value, Copy(Criteria, 3, Length(Criteria) - 2)) >= 0
      else
        Result := AnsiCompareText(Value, Copy(Criteria, 2, Length(Criteria) - 1)) > 0;
    end
    else
    begin
      if (Length(Criteria) > 1) and (Criteria[2] = '=') then
        Result := AnsiCompareText(Value, Copy(Criteria, 3, Length(Criteria) - 2)) <= 0
      else
        Result := AnsiCompareText(Value, Copy(Criteria, 2, Length(Criteria) - 1)) < 0;
    end;
  end
  else
    Result := AnsiCompareText(Value, Criteria) = 0;
end;
}

procedure fpiABS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiACOS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiACOSH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiACOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiACOTH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiASIN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiASINH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiATAN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiATAN2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiATANH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiBase(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiCeiling(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiCeiling_Math(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiCeiling_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiCOS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCOSH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCOTH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCombin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiCombinA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiCSC(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiCSCH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiDecimal(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiDegrees(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiExp(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiEven(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiFact(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiFactDouble(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiFloor(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiFloor_Math(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiFloor_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiINT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIso_Ceiling(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiLN(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiLOG(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiLOG10(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiMMult(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiMOD(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiMRound(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiOdd(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiPI(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiPower(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiProduct(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiQuotient(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiRadians(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiRand(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiRandBetween(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiRound(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiRoundDown(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiRoundUp(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiSec(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSech(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSign(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSinH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSQRT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSQRTPI(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiSum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSumIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredArray;
end;

procedure fpiSumIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredUnlimited;
end;

procedure fpiSumProduct(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSumSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSumX2MY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiSumX2PY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiSumXMY2(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiTan(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiTanH(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiTrunc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnAbs, fnABS, fpiABS, frkValue, 24, ftMath, @sfnAbsDescription);
  ARepository.Add(@sfnAcos, fnACOS, fpiACOS, frkValue, 99, ftMath, @sfnAcosDescription);
  ARepository.Add(@sfnAcosh, fnACOSH, fpiACOSH, frkValue, 233, ftMath, @sfnAcoshDescription);
  ARepository.Add(@sfnAcot, fnACOT, fpiACOT, frkValue, 255, ftMath, @sfnAcotDescription, 1);
  ARepository.Add(@sfnAcoth, fnACOTH, fpiACOTH, frkValue, 255, ftMath, @sfnAcothDescription, 1);
  ARepository.Add(@sfnAggregate, nil, nil, frkValue, 255, ftMath, @sfnAggregateDescription, 1);
  ARepository.Add(@sfnArabic, nil, nil, frkValue, 255, ftMath, @sfnArabicDescription);
  ARepository.Add(@sfnAsin, fnASIN, fpiASIN, frkValue, 98, ftMath, @sfnAsinDescription);
  ARepository.Add(@sfnAsinh, fnASINH, fpiASINH, frkValue, 232, ftMath, @sfnAsinhDescription);
  ARepository.Add(@sfnAtan, fnATAN, fpiATAN, frkValue, 18, ftMath, @sfnAtanDescription);
  ARepository.Add(@sfnAtan2, fnATAN2, fpiATAN2, frkValue, 97, ftMath, @sfnAtan2Description);
  ARepository.Add(@sfnAtanh, fnATANH, fpiATANH, frkValue, 234, ftMath, @sfnAtanhDescription);
  ARepository.Add(@sfnBase, fnBase, fpiBase, frkValue, 255, ftMath, @sfnBaseDescription, 1);
  ARepository.Add(@sfnCeiling, fnCeiling, fpiCeiling, frkValue, 288, ftMath, @sfnCeilingDescription);
  ARepository.Add(@sfnCeiling_Math, fnCeiling_Math, fpiCeiling_Math, frkValue, 255, ftMath, @sfnCeiling_MathDescription, 1);
  ARepository.Add(@sfnCeiling_Precise, fnCeiling_Precise, fpiCeiling_Precise, frkValue, 255, ftMath, @sfnCeiling_PreciseDescription, 1);
  ARepository.Add(@sfnCombin, fnCombin, fpiCombin, frkValue, 276, ftMath, @sfnCombinDescription);
  ARepository.Add(@sfnCombinA, fnCombinA, fpiCombinA, frkValue, 255, ftMath, @sfnCombinADescription, 1);
  ARepository.Add(@sfnCos, fnCOS, fpiCOS, frkValue, 16, ftMath, @sfnCosDescription);
  ARepository.Add(@sfnCosh, fnCOSH, fpiCOSH, frkValue, 230, ftMath, @sfnCoshDescription);
  ARepository.Add(@sfnCot, fnCOT, fpiCOT, frkValue, 255, ftMath, @sfnCotDescription, 1);
  ARepository.Add(@sfnCoth, fnCOTH, fpiCOTH, frkValue, 255, ftMath, @sfnCothDescription, 1);
  ARepository.Add(@sfnCsc, fnCSC, fpiCSC, frkValue, 255, ftMath, @sfnCscDescription, 1);
  ARepository.Add(@sfnCsch, fnCSCH, fpiCSCH, frkValue, 255, ftMath, @sfnCschDescription, 1);
  ARepository.Add(@sfnDecimal, fnDecimal, fpiDecimal, frkValue, 255, ftMath, @sfnDecimalDescription, 1);
  ARepository.Add(@sfnDegrees, fnDegrees, fpiDegrees, frkValue, 343, ftMath, @sfnDegreesDescription);
  ARepository.Add(@sfnExp, fnExp, fpiExp, frkValue, 21, ftMath, @sfnExpDescription);
  ARepository.Add(@sfnEven, fnEven, fpiEven, frkValue, 279, ftMath, @sfnEvenDescription);
  ARepository.Add(@sfnFact, fnFact, fpiFact, frkValue, 184, ftMath, @sfnFactDescription);
  ARepository.Add(@sfnFactDouble, fnFactDouble, fpiFactDouble, frkValue, 255, ftMath, @sfnFactDoubleDescription);
  ARepository.Add(@sfnFloor, fnFloor, fpiFloor, frkValue, 285, ftMath, @sfnFloorDescription);
  ARepository.Add(@sfnFloor_Math, fnFloor_Math, fpiFloor_Math, frkValue, 255, ftMath, @sfnFloor_MathDescription, 1);
  ARepository.Add(@sfnFloor_Precise, fnFloor_Precise, fpiFloor_Precise, frkValue, 255, ftMath, @sfnFloor_PreciseDescription, 1);
  ARepository.Add(@sfnGCD, nil, nil, frkValue, 255, ftMath, @sfnGCDDescription);
  ARepository.Add(@sfnInt, fnINT, fpiINT, frkValue, 25, ftMath, @sfnIntDescription);
  ARepository.Add(@sfnIso_Ceiling, fnIso_Ceiling, fpiIso_Ceiling, frkValue, 255, ftMath, @sfnIso_CeilingDescription);
  ARepository.Add(@sfnLCM, nil, nil, frkValue, 255, ftMath, @sfnLCMDescription);
  ARepository.Add(@sfnLn, fnLN, fpiLN, frkValue, 22, ftMath, @sfnLnDescription);
  ARepository.Add(@sfnLog, fnLOG, fpiLOG, frkValue, 109, ftMath, @sfnLogDescription);
  ARepository.Add(@sfnLog10, fnLOG10, fpiLOG10, frkValue, 23, ftMath, @sfnLog10Description);
  ARepository.Add(@sfnMDeterm, nil, nil, frkValue, 163, ftMath, @sfnMDetermDescription);
  ARepository.Add(@sfnMInverse, nil, nil, frkValue, 164, ftMath, @sfnMInverseDescription);
  ARepository.Add(@sfnMMult, fnMMult, fpiMMult, frkArray, 165, ftMath, @sfnMMultDescription);
  ARepository.Add(@sfnMod, fnMOD, fpiMOD, frkValue, 39, ftMath, @sfnModDescription);
  ARepository.Add(@sfnMRound, fnMRound, fpiMRound, frkValue, 255, ftMath, @sfnMRoundDescription);
  ARepository.Add(@sfnMultiNomial, nil, nil, frkValue, 255, ftMath, @sfnMultiNomialDescription);
  ARepository.Add(@sfnMUnit, nil, nil, frkValue, 255, ftMath, @sfnMUnitDescription, 1);
  ARepository.Add(@sfnOdd, fnOdd, fpiOdd, frkValue, 298, ftMath, @sfnOddDescription);
  ARepository.Add(@sfnPi, fnPI, fpiPI, frkValue, 19, ftMath, @sfnPiDescription);
  ARepository.Add(@sfnPower, fnPower, fpiPower, frkValue, 337, ftMath, @sfnPowerDescription);
  ARepository.Add(@sfnProduct, fnProduct, fpiProduct, frkNonArrayValue, 183, ftMath, @sfnProductDescription);
  ARepository.Add(@sfnQuotient, fnQuotient, fpiQuotient, frkValue, 255, ftMath, @sfnQuotientDescription);
  ARepository.Add(@sfnRadians, fnRadians, fpiRadians, frkValue, 342, ftMath, @sfnRadiansDescription);
  ARepository.Add(@sfnRand, fnRand, fpiRand, frkValue, 63, ftMath, @sfnRandDescription);
  ARepository.Add(@sfnRandBetween, fnRandBetween, fpiRandBetween, frkValue, 255, ftMath, @sfnRandBetweenDescription);
  ARepository.Add(@sfnRound, fnRound, fpiRound, frkValue, 27, ftMath, @sfnRoundDescription);
  ARepository.Add(@sfnRoundDown, fnRoundDown, fpiRoundDown, frkValue, 213, ftMath, @sfnRoundDownDescription);
  ARepository.Add(@sfnRoundUp, fnRoundUp, fpiRoundUp, frkValue, 212, ftMath, @sfnRoundUpDescription);
  ARepository.Add(@sfnRoman, nil, nil, frkValue, 354, ftMath, @sfnRomanDescription);
  ARepository.Add(@sfnSec, fnSec, fpiSec, frkValue, 255, ftMath, @sfnSecDescription, 1);
  ARepository.Add(@sfnSech, fnSech, fpiSech, frkValue, 255, ftMath, @sfnSechDescription, 1);
  ARepository.Add(@sfnSeriesSum, nil, nil, frkValue, 255, ftMath, @sfnSeriesSumDescription);
  ARepository.Add(@sfnSign, fnSign, fpiSign, frkValue, 26, ftMath, @sfnSignDescription);
  ARepository.Add(@sfnSin, fnSin, fpiSin, frkValue, 15, ftMath, @sfnSinDescription);
  ARepository.Add(@sfnSinh, fnSinH, fpiSinH, frkValue, 229, ftMath, @sfnSinhDescription);
  ARepository.Add(@sfnSqrt, fnSQRT, fpiSQRT, frkValue, 20, ftMath, @sfnSqrtDescription);
  ARepository.Add(@sfnSqrtPi, fnSQRTPI, fpiSQRTPI, frkValue, 255, ftMath, @sfnSqrtPiDescription);
  ARepository.Add(@sfnSubTotal, fnSubTotal, fpiSubTotal, frkValue, 344, ftMath, @sfnSubTotalDescription);
  ARepository.Add(@sfnSum, fnSum, fpiSum, frkNonArrayValue, 4, ftMath, @sfnSumDescription);
  ARepository.Add(@sfnSumIF, fnSumIF, fpiSumIF, frkNonArrayValue, 345, ftMath, @sfnSumIFDescription);
  ARepository.Add(@sfnSumIFS, fnSumIFS, fpiSumIFS, frkNonArrayValue, 255, ftMath, @sfnSumIFSDescription);
  ARepository.Add(@sfnSumProduct, fnSumProduct, fpiSumProduct, frkNonArrayValue, 228, ftMath, @sfnSumProductDescription);
  ARepository.Add(@sfnSumSQ, fnSumSQ, fpiSumSQ, frkNonArrayValue, 321, ftMath, @sfnSumSQDescription);
  ARepository.Add(@sfnSumX2MY2, fnSumX2MY2, fpiSumX2MY2, frkNonArrayValue, 304, ftMath, @sfnSumX2MY2Description);
  ARepository.Add(@sfnSumX2PY2, fnSumX2PY2, fpiSumX2PY2, frkNonArrayValue, 305, ftMath, @sfnSumX2PY2Description);
  ARepository.Add(@sfnSumXMY2, fnSumXMY2, fpiSumXMY2, frkNonArrayValue, 303, ftMath, @sfnSumXMY2Description);
  ARepository.Add(@sfnTan, fnTan, fpiTan, frkValue, 17, ftMath, @sfnTanDescription);
  ARepository.Add(@sfnTanh, fnTanH, fpiTanH, frkValue, 231, ftMath, @sfnTanhDescription);
  ARepository.Add(@sfnTrunc, fnTrunc, fpiTrunc, frkValue, 197, ftMath, @sfnTruncDescription);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
