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

unit dxSpreadSheetFunctionsFinancial;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Variants, Generics.Defaults, Generics.Collections,
  //
  dxSpreadSheetUtils,
  dxSpreadSheetTypes,
  dxSpreadSheetCoreFormulas;

procedure fnFV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNPer(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNPV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
{
procedure fnPMT(Sender: TdxSpreadSheetFormula);
}
procedure fpiFV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNPer(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNPV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
implementation

uses
  Math, cxVariants, Classes,
  //
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStrs;

function dxGetFutureValue(const ARate, APeriodCount, APayment, APresentValue, AType: Extended): Extended;
var
  AValue: Extended;
begin
  if ARate = 0 then
    Result := -APayment * APeriodCount - APresentValue
  else
  begin
    if (ARate > -1) and (ARate < -1) and (APeriodCount > -1) and (APeriodCount < 0) then
      AValue := 1 / -Exp(Log10(Abs(1 + ARate)) * Abs(APeriodCount))
    else
      AValue := Power(1 + ARate, APeriodCount);
    Result := -((AValue - 1) / ARate * APayment * (1 + ARate * Integer(AType <> 0)) + APresentValue * AValue);
  end;
end;

procedure fnFV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ARate, APeriodCount, APayment, APresentValue, AType: Variant;
begin
  if Sender.ExtractNumericParameter(ARate, AParams) and Sender.ExtractNumericParameter(APeriodCount, AParams, 1) and
     Sender.ExtractNumericParameter(APayment, AParams, 2) and
     Sender.ExtractNumericParameterDef(APresentValue, 0, 0, AParams, 3) and
     Sender.ExtractNumericParameterDef(AType, 0, 0, AParams, 4) then
   if (ARate = -1) and (APeriodCount <= 0) then
   begin
     if APeriodCount < 0 then
       Sender.SetError(ecDivByZero)
     else
       Sender.SetError(ecNUM);
   end
   else
     if (ARate < -1) and (Trunc(APeriodCount) <> APeriodCount) then
       Sender.SetError(ecNUM)
     else
       Sender.AddValue(dxGetFutureValue(ARate, APeriodCount, APayment, APresentValue, AType));
end;

function dxGetCustomPayment(ARate, APeriod, APeriodCount, APresentValue, AFutureValue, AType: Extended;
  AIsPeriodPayment: Boolean): Extended;
var
  ASumma, V, V1, V2: Extended;
begin
  if AType <> 0 then
    AType := 1;
  Result := 0;
  if (((APeriod = 1) and (AType = 1)) or (ARate = 0)) and not AIsPeriodPayment then
    Exit;
  ASumma := APresentValue + AFutureValue;
  if ARate = 0 then
    Result := -ASumma / APeriodCount
  else
  begin
    V := ARate / (1 + AType * ARate);
    V1 := 1 - Power(1 + ARate, APeriodCount);
    if (APeriod = 1) and (AType = 1) then
      Result := V * (ASumma / V1 - APresentValue)
    else
    begin
      V2 := Power(1 + ARate, APeriod - 1);
      if AIsPeriodPayment then
        Result := V * ASumma * V2 / V1
      else
        Result := V * (ASumma * (1 - V2) / V1 - APresentValue);
    end;
  end;
end;

procedure fnCustomPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AIsPeriodPayment: Boolean);
var
  ARate, APeriod, APeriodCount, APresentValue, AFutureValue, AType: Variant;
begin
  if Sender.ExtractNumericParameter(ARate, AParams) and Sender.ExtractNumericParameter(APeriod, AParams, 1) and
     Sender.ExtractNumericParameter(APeriodCount, AParams, 2) and
     Sender.ExtractNumericParameter(APresentValue, AParams, 3) and
     Sender.ExtractNumericParameterDef(AFutureValue, 0, 0, AParams, 4) and
     Sender.ExtractNumericParameterDef(AType, 0, 0, AParams, 5) then
    if (ARate <= -1.0) or (APeriod < 1) or (APeriodCount <= 0) then
      Sender.SetError(ecNum)
    else
      Sender.AddValue(dxGetCustomPayment(ARate, APeriod, APeriodCount, APresentValue,
        AFutureValue, AType, AIsPeriodPayment));
end;

procedure fnIPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomPMT(Sender, AParams, False);
end;

procedure fnNPer(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ARate, APayment, APresentValue, AFutureValue, AType: Variant;
  AResult, ASumma: Extended;
begin
  if Sender.ExtractNumericParameter(ARate, AParams) and Sender.ExtractNumericParameter(APayment, AParams, 1) and
     Sender.ExtractNumericParameter(APresentValue, AParams, 2) and
     Sender.ExtractNumericParameterDef(AFutureValue, 0, 0, AParams, 3) and
     Sender.ExtractNumericParameterDef(AType, 0, 0, AParams, 4) then
  begin
    if ARate <= -1 then
      Sender.SetError(ecNUM)
    else
      if (ARate = 0) and (APayment = 0) then
        Sender.SetError(ecDivByZero)
      else
      begin
        AType := Integer(AType <> 0);
        AResult := APayment * (1 + ARate * AType) + APresentValue * ARate;
        if AResult = 0 then
          Sender.SetError(ecNUM)
        else
        begin
          ASumma := APresentValue + AFutureValue;
          if ARate = 0 then
            AResult := -ASumma / APayment
          else
          begin
            AResult := (AResult - ASumma * ARate) / AResult;
            if AResult <= 0 then
            begin
              Sender.SetError(ecNUM);
              Exit;
            end;
            AResult := Log10(AResult) / Log10(1 + ARate);
          end;
          Sender.AddValue(AResult);
        end;
      end;
  end;
end;

procedure fnNPV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function GetNPV(ARate: Extended; ANumbers: TList<Double>): Extended;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to ANumbers.Count do
      Result := Result + ANumbers[I - 1] / Power(1 + ARate, I);
  end;

var
  ARate: Variant;
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  if not Sender.ExtractNumericParameter(ARate, AParams) then
    Exit;
  if ARate = -1 then
    Sender.SetError(ecDivByZero)
  else
  begin
    ADataInfo.Init(False, False, False, False);
    AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
    try
      if AData.PopulateNumericList(Sender, AParams.Next, True) <> ecNone then
        Sender.SetError(AData.ErrorCode)
      else
        Sender.AddValue(GetNPV(ARate, AData.NumericList));
    finally
      FreeAndNil(AData);
    end;
  end;
end;

function dxGetPayment(const ARate, APeriodCount, APresentValue, AFutureValue, AType: Extended): Extended;
var
  AValueSumma: Extended;
begin
  AValueSumma := APresentValue + AFutureValue;
  if ARate = 0 then
    Result :=  -AValueSumma / APeriodCount
  else
  begin
    Result := 1 - Power(ARate + 1, APeriodCount);
    Result := ARate * (AValueSumma / Result - APresentValue);
    if AType <> 0 then
      Result := Result / (1 + ARate);
  end;
end;

procedure fnPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ARate, APeriodCount, APresentValue, AFutureValue, AType: Variant;
begin
  if Sender.ExtractNumericParameter(ARate, AParams) and Sender.ExtractNumericParameter(APeriodCount, AParams, 1) and
     Sender.ExtractNumericParameter(APresentValue, AParams, 2) and
     Sender.ExtractNumericParameterDef(AFutureValue, 0, 0, AParams, 3) and
     Sender.ExtractNumericParameterDef(AType, 0, 0, AParams, 4) then
    if APeriodCount = 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetPayment(ARate, APeriodCount, APresentValue, AFutureValue, AType));
end;

procedure fnPPMT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomPMT(Sender, AParams, True);
end;

procedure fnPV(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ARate, APeriodCount, APayment, AFutureValue, AType: Variant;
  AHelper: Extended;
begin
  if not(Sender.ExtractNumericParameter(ARate, AParams) and Sender.ExtractNumericParameter(APeriodCount, AParams, 1) and
     Sender.ExtractNumericParameter(APayment, AParams, 2) and
     Sender.ExtractNumericParameterDef(AFutureValue, 0, 0, AParams, 3) and
     Sender.ExtractNumericParameterDef(AType, 0, 0, AParams, 4))then
    Exit;
  if ARate = 0 then
    Sender.AddValue(-AFutureValue - APayment * APeriodCount)
  else
    if ARate = -1 then
      Sender.SetError(ecDivByZero)
    else
    begin
      AHelper := Power(1 + Extended(ARate), Extended(APeriodCount));
      Sender.AddValue((-AFutureValue - APayment * (1 + ARate * AType) * ((AHelper - 1)/ARate)) / AHelper);
    end;
end;

procedure fpiFV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiIPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(6, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
  AParamKind[4] := fpkNonRequiredValue;
  AParamKind[5] := fpkNonRequiredValue;
end;

procedure fpiNPer(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiNPV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkUnlimited;
end;

procedure fpiPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiPPMT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(6, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
  AParamKind[4] := fpkNonRequiredValue;
  AParamKind[5] := fpkNonRequiredValue;
end;

procedure fpiPV(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnAccrInt, nil, nil, frkValue, 255, ftFinancial, @sfnAccrIntDescription);
  ARepository.Add(@sfnAccrIntM, nil, nil, frkValue, 255, ftFinancial, @sfnAccrIntMDescription);
  ARepository.Add(@sfnAmorDegr, nil, nil, frkValue, 255, ftFinancial, @sfnAmorDegrDescription);
  ARepository.Add(@sfnAmorLinc, nil, nil, frkValue, 255, ftFinancial, @sfnAmorLincDescription);
  ARepository.Add(@sfnCoupDayBS, nil, nil, frkValue, 255, ftFinancial, @sfnCoupDayBSDescription);
  ARepository.Add(@sfnCoupDays, nil, nil, frkValue, 255, ftFinancial, @sfnCoupDaysDescription);
  ARepository.Add(@sfnCoupDaysNC, nil, nil, frkValue, 255, ftFinancial, @sfnCoupDaysNCDescription);
  ARepository.Add(@sfnCoupNCD, nil, nil, frkValue, 255, ftFinancial, @sfnCoupNCDDescription);
  ARepository.Add(@sfnCoupNum, nil, nil, frkValue, 255, ftFinancial, @sfnCoupNumDescription);
  ARepository.Add(@sfnCoupPCD, nil, nil, frkValue, 255, ftFinancial, @sfnCoupPCDDescription);
  ARepository.Add(@sfnCoupIPMT, nil, nil, frkValue, 255, ftFinancial, @sfnCoupIPMTDescription);
  ARepository.Add(@sfnCoupRINC, nil, nil, frkValue, 255, ftFinancial, @sfnCoupRINCDescription);
  ARepository.Add(@sfnDB, nil, nil, frkValue, 247, ftFinancial, @sfnDBDescription);
  ARepository.Add(@sfnDDB, nil, nil, frkValue, 144, ftFinancial, @sfnDDBDescription);
  ARepository.Add(@sfnDisc, nil, nil, frkValue, 255, ftFinancial, @sfnDiscDescription);
  ARepository.Add(@sfnDollarDe, nil, nil, frkValue, 255, ftFinancial, @sfnDollarDeDescription);
  ARepository.Add(@sfnDollarFr, nil, nil, frkValue, 255, ftFinancial, @sfnDollarFrDescription);
  ARepository.Add(@sfnDuration, nil, nil, frkValue, 255, ftFinancial, @sfnDurationDescription);
  ARepository.Add(@sfnEffect, nil, nil, frkValue, 255, ftFinancial, @sfnEffectDescription);
  ARepository.Add(@sfnFV, fnFV, fpiFV, frkValue, 57, ftFinancial, @sfnFVDescription);
  ARepository.Add(@sfnFVSchedule, nil, nil, frkValue, 255, ftFinancial, @sfnFVScheduleDescription);
  ARepository.Add(@sfnIntRate, nil, nil, frkValue, 255, ftFinancial, @sfnIntRateDescription);
  ARepository.Add(@sfnIPMT, fnIPMT, fpiIPMT, frkValue, 167, ftFinancial, @sfnIPMTDescription);
  ARepository.Add(@sfnIRR, nil, nil, frkValue, 62, ftFinancial, @sfnIRRDescription);
  ARepository.Add(@sfnIsPMT, nil, nil, frkValue, 350, ftFinancial, @sfnIsPMTDescription);
  ARepository.Add(@sfnMDuration, nil, nil, frkValue, 255, ftFinancial, @sfnMDurationDescription);
  ARepository.Add(@sfnMIRR, nil, nil, frkValue, 61, ftFinancial, @sfnMIRRDescription);
  ARepository.Add(@sfnNominal, nil, nil, frkValue, 255, ftFinancial, @sfnNominalDescription);
  ARepository.Add(@sfnNPer, fnNPer, fpiNPer, frkValue, 58, ftFinancial, @sfnNPerDescription);
  ARepository.Add(@sfnNPV, fnNPV, fpiNPV, frkValue, 11, ftFinancial, @sfnNPVDescription);
  ARepository.Add(@sfnOddFPrice, nil, nil, frkValue, 255, ftFinancial, @sfnOddFPriceDescription);
  ARepository.Add(@sfnOddFYield, nil, nil, frkValue, 255, ftFinancial, @sfnOddFYieldDescription);
  ARepository.Add(@sfnOddLPrice, nil, nil, frkValue, 255, ftFinancial, @sfnOddLPriceDescription);
  ARepository.Add(@sfnOddLYield, nil, nil, frkValue, 255, ftFinancial, @sfnOddLYieldDescription);
  ARepository.Add(@sfnPDuration, nil, nil, frkValue, 255, ftFinancial, @sfnPDurationDescription, 1);
  ARepository.Add(@sfnPMT, fnPMT, fpiPMT, frkValue, 59, ftFinancial, @sfnPMTDescription);
  ARepository.Add(@sfnPPMT, fnPPMT, fpiPPMT, frkValue, 168, ftFinancial, @sfnPPMTDescription);
  ARepository.Add(@sfnPrice, nil, nil, frkValue, 255, ftFinancial, @sfnPriceDescription);
  ARepository.Add(@sfnPriceDisc, nil, nil, frkValue, 255, ftFinancial, @sfnPriceDiscDescription);
  ARepository.Add(@sfnPriceMat, nil, nil, frkValue, 255, ftFinancial, @sfnPriceMatDescription);
  ARepository.Add(@sfnPV, fnPV, fpiPV, frkValue, 56, ftFinancial, @sfnPVDescription);
  ARepository.Add(@sfnRate, nil, nil, frkValue, 60, ftFinancial, @sfnRateDescription);
  ARepository.Add(@sfnReceived, nil, nil, frkValue, 255, ftFinancial, @sfnReceivedDescription);
  ARepository.Add(@sfnRRI, nil, nil, frkValue, 255, ftFinancial, @sfnRRIDescription, 1);
  ARepository.Add(@sfnSLN, nil, nil, frkValue, 142, ftFinancial, @sfnSLNDescription);
  ARepository.Add(@sfnSYD, nil, nil, frkValue, 143, ftFinancial, @sfnSYDDescription);
  ARepository.Add(@sfnTBillEq, nil, nil, frkValue, 255, ftFinancial, @sfnTBillEqDescription);
  ARepository.Add(@sfnTBillPrice, nil, nil, frkValue, 255, ftFinancial, @sfnTBillPriceDescription);
  ARepository.Add(@sfnTBillYield, nil, nil, frkValue, 255, ftFinancial, @sfnTBillYieldDescription);
  ARepository.Add(@sfnVDB, nil, nil, frkValue, 222, ftFinancial, @sfnVDBDescription);
  ARepository.Add(@sfnXIRR, nil, nil, frkValue, 255, ftFinancial, @sfnXIRRDescription);
  ARepository.Add(@sfnXNPV, nil, nil, frkValue, 255, ftFinancial, @sfnXNPVDescription);
  ARepository.Add(@sfnYield, nil, nil, frkValue, 255, ftFinancial, @sfnYieldDescription);
  ARepository.Add(@sfnYieldDisc, nil, nil, frkValue, 255, ftFinancial, @sfnYieldDiscDescription);
  ARepository.Add(@sfnYieldMat, nil, nil, frkValue, 255, ftFinancial, @sfnYieldMatDescription);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
