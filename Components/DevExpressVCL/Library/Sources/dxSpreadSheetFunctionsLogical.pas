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

unit dxSpreadSheetFunctionsLogical;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Variants, Math, cxDateUtils,
  //
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnAND(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFalse(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIfError(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIfNA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnOR(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTrue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnXOR(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiAND(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFalse(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIfError(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIfNA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiOR(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTrue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiXOR(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

implementation

uses
  cxVariants, Classes,
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStrs;

type
  TdxAccumulationLogicalType = (altAND, altOR, altXOR);
  TdxErrorFunctionIdentity = (efiAnyError, efiNA);
  TdxSpreadSheetFunctionReference = reference to procedure (Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
  TdxSpreadSheetFormulaTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxSpreadSheetCustomFormulaAccess = class(TdxSpreadSheetCustomFormula);

function dxAccumulateLogicalResult(const AParameter: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData: TdxSpreadSheetEnumValues; AType: TdxAccumulationLogicalType): Boolean;
var
  ACheckedValue: Variant;
begin
  Result := AErrorCode = ecNone;
  if Result then
  begin
    if not AData.CheckValueOnLogical(AParameter, ACanConvertStrToNumber, ACheckedValue) then
      AData.SetErrorCode(ecValue)
    else
      if not VarIsNull(ACheckedValue) then
      case AType of
        altAND:
          AData.ResultValue := AData.ResultValue and ACheckedValue;
        altOR, altXOR:
          AData.ResultValue := AData.ResultValue or ACheckedValue;
      end;
    case AType of
      altAND: if not AData.ResultValue then
                AData.SetCalculated;
      altOR:  if AData.ResultValue then
                AData.SetCalculated;
    end;
  end
  else
    AData.SetErrorCode(AErrorCode);
end;

function cbAND(AValue: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: Pointer = nil): Boolean;
begin
  Result := dxAccumulateLogicalResult(AValue, ACanConvertStrToNumber, AErrorCode, AData, altAND);
end;

function cbOR(AValue: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: Pointer = nil): Boolean;
begin
  Result := dxAccumulateLogicalResult(AValue, ACanConvertStrToNumber, AErrorCode, AData, altOR);
end;

function cbXOR(AValue: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: Pointer = nil): Boolean;
begin
  Result := dxAccumulateLogicalResult(AValue, ACanConvertStrToNumber, AErrorCode, AData, altXOR);
end;

procedure fnANDORXOR(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AProc: TdxSpreadSheetForEachCallBack);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, @AProc = @cbAND, ADataInfo);
  try
    AData.SetErrorCode(Sender.ErrorCode);
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, AProc);
    if not AData.Validate or (AData.EmptyCount + AData.StringCount = AData.Count) then
      Sender.SetError(AData.ErrorCode)
    else
    begin
      if @AProc = @cbXOR then
        AData.ResultValue := AData.ResultValue and Odd(AData.TrueCount);
      Sender.AddValue(AData.ResultValue);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnAND(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnANDORXOR(Sender, AParams, @cbAND);
end;

procedure fnFalse(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(False);
end;

procedure dxExtractValueFromToken(AToken: TdxSpreadSheetFormulaToken; ATokenDimensionErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AErrorCode := ATokenDimensionErrorCode;
  if AErrorCode = ecNone then
    if AToken.Dimension.Count = 1 then
      AToken.GetValue(AValue, AErrorCode)
    else
      if (ARow <= AToken.Dimension.RowCount - 1) and (AColumn <= AToken.Dimension.ColumnCount - 1) then
        TdxSpreadSheetFormulaTokenAccess(AToken).GetValueAsArrayItem(ARow, AColumn, AValue, AErrorCode)
      else
        AErrorCode := ecNA;
end;

procedure dxCheckDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension;
  ATokenDimension: TdxSpreadSheetFormulaTokenDimension; ATokenDimensionErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  if ATokenDimensionErrorCode <> ecNone then
    Exit;
  ADimension.RowCount := Max(ADimension.RowCount, ATokenDimension.RowCount);
  ADimension.ColumnCount := Max(ADimension.ColumnCount, ATokenDimension.ColumnCount);
end;

procedure fnIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  procedure CalculateSingleValue;
  var
    ACondition: Variant;
  begin
    if Sender.ExtractNumericParameter(ACondition, AParams) then
      if ACondition <> 0 then
      begin
        if Sender.ParameterExists(AParams, 1) then
          dxSpreadSheetCalculateExpression(AParams.Next.FirstChild, Sender, True)
        else
          Sender.AddValue(True);
      end
      else
      begin
        if Sender.ParameterExists(AParams, 2) then
          dxSpreadSheetCalculateExpression(AParams.Next.Next.FirstChild, Sender, True)
        else
          Sender.AddValue(False);
      end;
  end;

var
  ATempResult: TdxSpreadSheetFormulaResult;
  ADimension, ADimensionOfTrue, ADimensionOfFalse: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode, ADimensionOfTrueErrorCode, ADimensionOfFalseErrorCode: TdxSpreadSheetFormulaErrorCode;
  AConditionToken, ATokenIfTrue, ATokenIfFalse, AParentToken: TdxSpreadSheetFormulaToken;
  AResult: TdxSpreadSheetFormulaArrayToken;
  ARow, AColumn: Integer;
  ACondition, AValue: Variant;
begin
  Sender.SetError(ecNone);
  if Sender.Owner.IsArrayFormula then
  begin
    ATempResult := TdxSpreadSheetCustomFormulaAccess(Sender.Owner).Calculate(AParams.FirstChild);
    try
      if not ATempResult.Validate then
        Sender.SetError(ATempResult.ErrorCode)
      else
      begin
        AConditionToken := ATempResult.LastItem;
        ADimension := AConditionToken.GetDimension(AErrorCode);
        if AErrorCode <> ecNone then
          Sender.SetError(AErrorCode)
        else
          if ADimension.Count > 1 then
          begin
            if Sender.ParameterExists(AParams, 1) then
              dxSpreadSheetCalculateExpression(AParams.Next.FirstChild, ATempResult, True)
            else
              ATempResult.AddValue(True);
            ATokenIfTrue := ATempResult.LastItem;
            ADimensionOfTrue := ATokenIfTrue.GetDimension(ADimensionOfTrueErrorCode);
            dxCheckDimension(ADimension, ADimensionOfTrue, ADimensionOfTrueErrorCode);

            if Sender.ParameterExists(AParams, 2) then
              dxSpreadSheetCalculateExpression(AParams.Next.Next.FirstChild, ATempResult, True)
            else
              ATempResult.AddValue(False);
            ATokenIfFalse := ATempResult.LastItem;
            ADimensionOfFalse := ATokenIfFalse.GetDimension(ADimensionOfFalseErrorCode);
            dxCheckDimension(ADimension, ADimensionOfFalse, ADimensionOfFalseErrorCode);

            AResult := TdxSpreadSheetFormulaArrayToken.Create;
            for ARow := 0 to ADimension.RowCount - 1 do
              for AColumn := 0 to ADimension.ColumnCount - 1 do
              begin
                if (AColumn = 0) and (ARow > 0) then
                  TdxSpreadSheetFormulaToken.AddChild(AResult, TdxSpreadSheetFormulaArrayRowSeparator.Create());

                AParentToken := TdxSpreadSheetFormulaToken.Create;
                AErrorCode := ecNone;
                TdxSpreadSheetFormulaTokenAccess(AConditionToken).GetValueAsArrayItem(ARow, AColumn, ACondition, AErrorCode);
                if AErrorCode <> ecNone then
                  TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode))
                else
                begin
                  if ACondition then
                    dxExtractValueFromToken(ATokenIfTrue, ADimensionOfTrueErrorCode, ARow, AColumn, AValue, AErrorCode)
                  else
                    dxExtractValueFromToken(ATokenIfFalse, ADimensionOfFalseErrorCode, ARow, AColumn, AValue, AErrorCode);
                  TdxSpreadSheetFormulaToken.AddChild(AParentToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
                end;
                TdxSpreadSheetFormulaToken.AddChild(AResult, AParentToken);
              end;
            Sender.AddTemporary(AResult);
          end
          else
            CalculateSingleValue;
      end;
    finally
      ATempResult.Free;
    end;
  end
  else
    CalculateSingleValue;
end;

procedure dxIfCustomError(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AIdentity: TdxErrorFunctionIdentity; ASingleValueCalculatingProc: TdxSpreadSheetFunctionReference);

  function IsError(AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
  begin
    if AIdentity = efiAnyError then
      Result := AErrorCode <> ecNone
    else
      Result := AErrorCode = ecNA;
  end;

var
  ATempResult: TdxSpreadSheetFormulaResult;
  ADimension, ADimensionOfValue, ADimensionOfTrue: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode, ADimensionOfTrueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AValueToken, ATokenIfTrue, AParentToken: TdxSpreadSheetFormulaToken;
  AResult: TdxSpreadSheetFormulaArrayToken;
  ARow, AColumn: Integer;
  AValue: Variant;
begin
  Sender.SetError(ecNone);
  if Sender.Owner.IsArrayFormula then
  begin
    ATempResult := TdxSpreadSheetCustomFormulaAccess(Sender.Owner).Calculate(AParams.FirstChild);
    try
      if not ATempResult.Validate then
        Sender.SetError(ATempResult.ErrorCode)
      else
      begin
        AValueToken := ATempResult.LastItem;
        ADimensionOfValue := AValueToken.GetDimension(AErrorCode);
        ADimension := ADimensionOfValue;
        if AErrorCode <> ecNone then
          Sender.SetError(AErrorCode)
        else
          if ADimension.Count > 1 then
          begin
            if Sender.ParameterExists(AParams, 1) then
              dxSpreadSheetCalculateExpression(AParams.Next.FirstChild, ATempResult, True)
            else
              ATempResult.AddValue(0);
            ATokenIfTrue := ATempResult.LastItem;
            ADimensionOfTrue := ATokenIfTrue.GetDimension(ADimensionOfTrueErrorCode);
            dxCheckDimension(ADimension, ADimensionOfTrue, ADimensionOfTrueErrorCode);

            AResult := TdxSpreadSheetFormulaArrayToken.Create;
            for ARow := 0 to ADimension.RowCount - 1 do
              for AColumn := 0 to ADimension.ColumnCount - 1 do
              begin
                if (AColumn = 0) and (ARow > 0) then
                  TdxSpreadSheetFormulaToken.AddChild(AResult, TdxSpreadSheetFormulaArrayRowSeparator.Create());

                AParentToken := TdxSpreadSheetFormulaToken.Create;
                AErrorCode := ecNone;
                if (ARow <= ADimensionOfValue.RowCount - 1) and (AColumn <= ADimensionOfValue.ColumnCount - 1) then
                  TdxSpreadSheetFormulaTokenAccess(AValueToken).GetValueAsArrayItem(ARow, AColumn, AValue, AErrorCode)
                else
                  AErrorCode := ecNA;
                if IsError(AErrorCode) then
                  dxExtractValueFromToken(ATokenIfTrue, ADimensionOfTrueErrorCode, ARow, AColumn, AValue, AErrorCode);
                if dxSpreadSheetIsNullValue(AValue) then
                  AValue := 0;
                TdxSpreadSheetFormulaToken.AddChild(AParentToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
                TdxSpreadSheetFormulaToken.AddChild(AResult, AParentToken);
              end;
            Sender.AddTemporary(AResult);
          end
          else
            ASingleValueCalculatingProc(Sender, AParams);
      end;
    finally
      ATempResult.Free;
    end;
  end
  else
    ASingleValueCalculatingProc(Sender, AParams);
end;

procedure fnIfError(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxIfCustomError(Sender, AParams, efiAnyError,
    procedure (Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken)
    var
      AParameter: Variant;
    begin
      if Sender.ExtractParameterDef(AParameter, 0, AParams) then
        Sender.AddValue(AParameter)
      else
        begin
          Sender.SetError(ecNone);
          if Sender.ExtractParameterDef(AParameter, 0, AParams, 1) then
            Sender.AddValue(AParameter);
        end;
    end);
end;

procedure fnIfNA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxIfCustomError(Sender, AParams, efiNA,
    procedure (Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken)
    var
      AParameter: Variant;
    begin
      Sender.ExtractParameterDef(AParameter, 0, AParams);
      if Sender.ErrorCode <> ecNA then
        Sender.AddValue(AParameter)
      else
        begin
          Sender.SetError(ecNone);
          if Sender.ExtractParameterDef(AParameter, 0, AParams, 1) then
            Sender.AddValue(AParameter);
        end;
    end);
end;

procedure fnNOT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    Sender.AddValue(not AParameter);
end;

procedure fnOR(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnANDORXOR(Sender, AParams, @cbOR);
end;

procedure fnTrue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(True);
end;

procedure fnXOR(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnANDORXOR(Sender, AParams, @cbXOR);
end;

procedure fpiAND(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiFalse(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiIfError(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiIfNA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiNOT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiOR(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiTrue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiXOR(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnAnd, fnAND, fpiAND, frkValue, 36, ftLogical, @sfnAndDescription);
  ARepository.Add(@sfnFalse, fnFalse, fpiFalse, frkValue, 35, ftLogical, @sfnFalseDescription);
  ARepository.Add(@sfnIF, fnIF, fpiIF, frkParamValue, 1, ftLogical, @sfnIFDescription);
  ARepository.Add(@sfnIfError, fnIfError, fpiIfError, frkParamValue, 255, ftLogical, @sfnIfErrorDescription);
  ARepository.Add(@sfnIfNA, fnIfNA, fpiIfNA, frkParamValue, 255, ftLogical, @sfnIfNADescription, 1);
  ARepository.Add(@sfnNot, fnNOT, fpiNOT, frkValue, 38, ftLogical, @sfnNotDescription);
  ARepository.Add(@sfnOr, fnOR, fpiOR, frkValue, 37, ftLogical, @sfnOrDescription);
  ARepository.Add(@sfnTrue, fnTrue, fpiTrue, frkValue, 34, ftLogical, @sfnTrueDescription);
  ARepository.Add(@sfnXor, fnXOR, fpiXOR, frkValue, 255, ftLogical, @sfnXorDescription, 1);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
