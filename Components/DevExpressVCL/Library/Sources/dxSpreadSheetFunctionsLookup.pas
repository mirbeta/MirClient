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

unit dxSpreadSheetFunctionsLookup;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, SysUtils, Classes, StrUtils, DateUtils, Variants, Math, cxVariants, dxCore, cxGeometry,
  //
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnAddress(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnAreas(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChoose(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnColumn(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnColumns(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnFormulaText(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnHLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIndex(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIndirect(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMatch(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnOffset(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRow(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRows(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTranspose(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiAddress(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiAreas(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChoose(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiColumn(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiColumns(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiFormulaText(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiHLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIndex(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIndirect(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMatch(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiOffset(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRow(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRows(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTranspose(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

implementation

uses
  dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStrs,
  dxSpreadSheetFunctionsText, dxCoreClasses, dxStringHelper;

type
  TdxSpreadSheetFormulaExtractVectorFromRange = function(const ARange: TdxSpreadSheetFormulaToken;
    AVectorIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector of object;

  TdxSpreadSheetXLookupComparer = function(const AValue1: Variant; AValue2: TdxSpreadSheetVectorValue): Integer;

  TControllerAccess = class(TdxSpreadSheetCustomFormulaController);
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TFormulaParser = class(TdxSpreadSheetCustomFormulaParser);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);

procedure fnAddress(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
const
  rtAbsolute        = 1;
  rtAbsRowRelColumn = 2;
  rtRelRowAbsColumn = 3;
  rtRelative        = 4;

  function CheckParams(var ARow, AColumn, AAbsNum: Variant): Boolean;
  begin
    ARow := Trunc(ARow);
    AColumn := Trunc(AColumn);
    AAbsNum := Trunc(AAbsNum);
    Result := InRange(ARow, 1, 65536) and InRange(AColumn, 1, 256) and
      (Trunc(AAbsNum) in [rtAbsolute, rtAbsRowRelColumn, rtRelRowAbsColumn, rtRelative]);
    if not Result then
      Sender.SetError(ecValue);
  end;

  function GetSheetAddressName(const ASheet: string): string;
  begin
    Result := ASheet;
    if Result = '' then
      Exit;
    if Pos(' ', Result) > 0 then
      Result := '''' + Result + '''';
    Result := Result + '!';
  end;

  procedure InitializeReference(var AReference: TdxSpreadSheetReference; AOffset, AReferenceType, AAbsReferenceType: Integer);
  begin
    AReference.Reset;
    AReference.Offset := AOffset;
    AReference.IsAbsolute := (AReferenceType = rtAbsolute) or (AReferenceType = AAbsReferenceType);
  end;

var
  ARow, AColumn, AAbsNum, AA1RefStyle, ASheet: Variant;
  ARowReference, AColumnReference: TdxSpreadSheetReference;
begin
  if not (Sender.ExtractNumericParameter(ARow, AParams) and Sender.ExtractNumericParameter(AColumn, AParams, 1) and
          Sender.ExtractNumericParameterDef(AAbsNum, 1, 1, AParams, 2) and
          Sender.ExtractNumericParameterDef(AA1RefStyle, 1, 1, AParams, 3) and
          CheckParams(ARow, AColumn, AAbsNum) and Sender.ExtractParameterDef(ASheet, '', '', AParams, 4)) then Exit;
  InitializeReference(ARowReference, ARow - 1, AAbsNum, rtAbsRowRelColumn);
  InitializeReference(AColumnReference, AColumn - 1, AAbsNum, rtRelRowAbsColumn);
  if AA1RefStyle = 0 then
  begin
    if not AColumnReference.IsAbsolute then
      AColumnReference.Offset := AColumnReference.Offset + 1;
    if not ARowReference.IsAbsolute then
      ARowReference.Offset := ARowReference.Offset + 1;
  end;
  if AA1RefStyle <> 0 then
    Sender.AddValue(GetSheetAddressName(VarToStr(ASheet)) + dxReferenceToString(0, 0, ARowReference, AColumnReference))
  else
    Sender.AddValue(GetSheetAddressName(VarToStr(ASheet)) + dxR1C1ReferenceToString(0, 0, ARowReference, AColumnReference));
end;

function dxExtractFirstChild(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
begin
  AErrorCode := ecNA;
  Result := AParams.FirstChild;
  if Result = nil then
    Exit;

  AErrorCode := ecNone;
  if (Result is TdxSpreadSheetFormulaParenthesesToken) or (Result is TdxSpreadSheetFormulaFunctionToken) then
    dxSpreadSheetIsReferenceToken(Result, Sender);

  if (Result is TdxSpreadSheetFormulaArrayToken) and (Result.Next <> nil) then
      AErrorCode := ecNA;
end;

function dxExtractArea(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; const AAreaNum: Integer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
var
  I: Integer;
begin
  Result := dxExtractFirstChild(Sender, AParams, AErrorCode);
  if (AErrorCode = ecNone) and (Result is TdxSpreadSheetFormulaArrayToken) and (AAreaNum > 1) then
    AErrorCode := ecRefErr;
  if (AErrorCode <> ecNone) or (Result is TdxSpreadSheetFormulaArrayToken)then
    Exit;

  if Result is TdxSpreadSheetListToken then
  begin
    Result := Result.FirstChild;
    for I := 2 to AAreaNum do
      if Result <> nil then
        Result := Result.Next
      else
        AErrorCode := ecRefErr;
    if AErrorCode = ecNone then
    begin
      Result := Result.FirstChild;
      dxSpreadSheetIsReferenceToken(Result, Sender);
    end;
  end
  else
    if AAreaNum > 1 then
      AErrorCode := ecRefErr;
end;

function dxGetAreasCount(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Integer;
const
  ErrorCodeMap: array[Boolean] of TdxSpreadSheetFormulaErrorCode = (ecNA, ecNone);
var
  AParam, AExpression: TdxSpreadSheetFormulaToken;
  AResult: TdxSpreadSheetFormulaResult;
  AIsList: Boolean;
begin
  Result := 0;
  AParam := dxExtractFirstChild(Sender, AParams, AErrorCode);
  if AErrorCode = ecNone then
    if (AParam is TdxSpreadSheetFormulaArrayToken) or (AParam is TdxSpreadSheetCustomDefinedNameToken) then
    begin
      if AParam.Next = nil then
        Result := 1
      else
        AErrorCode := ecNA;
    end;

  if (AErrorCode <> ecNone) or (AParam is TdxSpreadSheetFormulaArrayToken) or (AParam is TdxSpreadSheetCustomDefinedNameToken) then
    Exit;

  AIsList := AParam is TdxSpreadSheetListToken;
  if AIsList then
    AParam := AParam.FirstChild;

  while AParam <> nil do
  begin
    AResult := TdxSpreadSheetFormulaResult.Create(AParam.Owner);
    try
      AExpression := AParam;
      if AExpression.FirstChild <> nil then
        AExpression := AExpression.FirstChild;
      dxSpreadSheetCalculateExpression(AExpression, AResult);

      AErrorCode := AResult.ErrorCode;
      if AErrorCode = ecNone then
      begin
        AErrorCode := ErrorCodeMap[
          (AExpression is TdxSpreadSheetFormulaReference) or
          (AExpression is TdxSpreadSheetFormulaArrayToken) or
          (AExpression is TdxSpreadSheetCustomDefinedNameToken)];
      end;
    finally
      AResult.Free;
    end;

    if AErrorCode <> ecNone then
      Break;
    Inc(Result);
    AParam := AParam.Next;
    if not AIsList then
      Break;
  end;
end;

procedure fnAreas(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AResult: Integer;
begin
  AResult := dxGetAreasCount(Sender, AParams, AErrorCode);
  if AErrorCode <> ecNone then
    Sender.SetError(AErrorCode)
  else
    Sender.AddValue(AResult);
end;

procedure fnChoose(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParamCount: Integer;

  procedure CalculateSingleValue;
  var
    AIndex: Variant;
    AItem: TdxSpreadSheetFormulaToken;
  begin
    if not Sender.ExtractNumericParameter(AIndex, AParams) then
      Exit;
    AIndex := Int(AIndex);
    if (AIndex < 1) or (AIndex > 254) or (AIndex > AParamCount - 1) then
      Sender.SetError(ecValue)
    else
    begin
      AItem := AParams;
      while (AIndex > 0) and Sender.Validate do
      begin
        AIndex := AIndex - 1;
        AItem := AItem.Next;
        if AItem = nil then
          Break;
      end;
      if AItem = nil then
        Sender.SetError(ecValue)
      else
        dxSpreadSheetCalculateExpression(AItem.FirstChild, Sender, True);
    end;
  end;

  procedure InternalExtractValueFromToken(AToken: TdxSpreadSheetFormulaToken; const ATokenDimensionErrorCode: TdxSpreadSheetFormulaErrorCode;
    ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  var
    ADimension: TdxSpreadSheetFormulaTokenDimension;
  begin
    AErrorCode := ATokenDimensionErrorCode;
    if AErrorCode <> ecNone then Exit;
    ADimension := AToken.Dimension;
    if ADimension.Count = 1 then
      AToken.GetValue(AValue, AErrorCode)
    else
      if (((ADimension.RowCount > 1) and (ADimension.ColumnCount > 1)) and
          ((ARow > ADimension.RowCount - 1) or (AColumn > ADimension.ColumnCount - 1))) or
         ((ADimension.ColumnCount = 1) and (ARow > ADimension.RowCount - 1)) or
         ((ADimension.RowCount = 1) and (AColumn > ADimension.ColumnCount - 1)) then
        AErrorCode := ecNA
      else
      begin
        if ADimension.ColumnCount = 1 then
          AColumn := 0;
        if ADimension.RowCount = 1 then
          ARow := 0;
        TTokenAccess(AToken).GetValueAsArrayItem(ARow, AColumn, AValue, AErrorCode);
      end;
  end;

  procedure InternalCheckDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension;
    ATokenDimension: TdxSpreadSheetFormulaTokenDimension; ATokenDimensionErrorCode: TdxSpreadSheetFormulaErrorCode);
  begin
    if ATokenDimensionErrorCode <> ecNone then
      Exit;
    ADimension.RowCount := Max(ADimension.RowCount, ATokenDimension.RowCount);
    ADimension.ColumnCount := Max(ADimension.ColumnCount, ATokenDimension.ColumnCount);
  end;

var
  ATempResult: TdxSpreadSheetFormulaResult;
  AIndexDimension, ADimension: TdxSpreadSheetFormulaTokenDimension;
  AIndexDimensionErrorCode, AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AIndexToken, AValueToken, AParentToken, AResult: TdxSpreadSheetFormulaToken;
  I, ARow, AColumn, AIndexInt: Integer;
  AIndex, AValue: Variant;
  AValues: array of TdxSpreadSheetFormulaToken;
  AValuesDimension: array of TdxSpreadSheetFormulaTokenDimension;
  AValuesDimensionErrorCode: array of TdxSpreadSheetFormulaErrorCode;
begin
  Sender.SetError(ecNone);
  AParamCount := Sender.GetParamsCount(AParams);

  if Sender.Owner.IsArrayFormula then
  begin
    ATempResult := TFormulaAccess(Sender.Owner).Calculate(AParams.FirstChild);
    try
      if not ATempResult.Validate then
        Sender.SetError(ATempResult.ErrorCode)
      else
      begin
        AIndexToken := ATempResult.LastItem;
        AIndexDimension := AIndexToken.GetDimension(AIndexDimensionErrorCode);
        AErrorCode := AIndexDimensionErrorCode;
        ADimension := AIndexDimension;
        if AErrorCode <> ecNone then
          Sender.SetError(AErrorCode)
        else
          if ADimension.Count > 1 then
          begin
            SetLength(AValues, AParamCount - 1);
            SetLength(AValuesDimension, AParamCount - 1);
            SetLength(AValuesDimensionErrorCode, AParamCount - 1);
            AValueToken := AParams.Next;
            for I := 0 to AParamCount - 2 do
            begin
              dxSpreadSheetCalculateExpression(AValueToken.FirstChild, ATempResult, True);
              AValues[I] := ATempResult.LastItem;
              AValuesDimension[I] := AValues[I].GetDimension(AValuesDimensionErrorCode[I]);
              InternalCheckDimension(ADimension, AValuesDimension[I], AValuesDimensionErrorCode[I]);
              AValueToken := AValueToken.Next;
            end;

            AResult := TdxSpreadSheetFormulaArrayToken.Create;
            for ARow := 0 to ADimension.RowCount - 1 do
              for AColumn := 0 to ADimension.ColumnCount - 1 do
              begin
                if (AColumn = 0) and (ARow > 0) then
                  TdxSpreadSheetFormulaToken.AddChild(AResult, TdxSpreadSheetFormulaArrayRowSeparator.Create());

                AParentToken := TdxSpreadSheetFormulaToken.Create;
                InternalExtractValueFromToken(AIndexToken, AIndexDimensionErrorCode, ARow, AColumn, AIndex, AErrorCode);
                if AErrorCode = ecNone then
                begin
                  ATempResult.SetError(ecNone);
                  if not ATempResult.ConvertToNumeric(AIndex, False, False) then
                    TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaErrorValueToken.Create(ATempResult.ErrorCode))
                  else
                  begin
                    AIndexInt := Trunc(Int(AIndex));
                    if (AIndexInt < 1) or (AIndexInt > 254) or (AIndexInt > AParamCount - 1) then
                      TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaErrorValueToken.Create(ecValue))
                    else
                      if AValuesDimensionErrorCode[AIndexInt - 1] <> ecNone then
                        TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaErrorValueToken.Create(AValuesDimensionErrorCode[AIndexInt - 1]))
                      else
                      begin
                        InternalExtractValueFromToken(AValues[AIndexInt - 1], AValuesDimensionErrorCode[AIndexInt - 1], ARow, AColumn, AValue, AErrorCode);
                        if dxSpreadSheetIsNullValue(AValue) then
                          AValue := 0;
                        TdxSpreadSheetFormulaToken.AddChild(AParentToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
                      end
                  end;
                end
                else
                  TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode));
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

procedure fnColumn(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParam: TdxSpreadSheetFormulaToken;
begin
  if Sender.ParameterExists(AParams, 0) then
  begin
    AParam := AParams.FirstChild;
    if not dxSpreadSheetIsReferenceToken(AParam, Sender) then
      Sender.SetError(ecValue)
    else
      Sender.AddValue(TdxSpreadSheetFormulaReference(AParam).ActualColumn + 1);
  end
  else
    Sender.AddValue(Sender.Owner.AnchorColumn + 1);
end;

procedure fnColumns(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParam: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  if not Sender.ParameterExists(AParams, 0) then
    Sender.SetError(ecValue)
  else
  begin
    AParam := AParams.FirstChild;
    if dxSpreadSheetIsReferenceToken(AParam, Sender) or (AParam is TdxSpreadSheetFormulaArrayToken) then
    begin
      ADimension := AParam.GetDimension(AErrorCode);
      if AErrorCode = ecNone then
        Sender.AddValue(ADimension.ColumnCount)
      else
        Sender.SetError(AErrorCode);
    end
    else
    if (AParam is TdxSpreadSheetFormulaIntegerValueToken) or (AParam is TdxSpreadSheetFormulaFloatValueToken) then
      Sender.AddValue(1)
    else
      Sender.SetError(ecValue);
  end;
end;

procedure fnFormulaText(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ACellData: IdxSpreadSheetCellData;
  AFormula: TdxSpreadSheetCustomFormula;
  AParam: TdxSpreadSheetFormulaToken;
  AViewData: IdxSpreadSheetViewData;
begin
  if Sender.ParameterExists(AParams, 0) then
  begin
    AParam := AParams.FirstChild;
    if dxSpreadSheetIsReferenceToken(AParam, Sender) and Supports(AParam.View, IdxSpreadSheetViewData, AViewData) then
    begin
      ACellData := AViewData.GetCellData(
        TdxSpreadSheetFormulaReference(AParam).ActualRow,
        TdxSpreadSheetFormulaReference(AParam).ActualColumn);
      if ACellData <> nil then
        AFormula := TdxSpreadSheetCustomFormula(ACellData.AsFormula)
      else
        AFormula := nil;

      if AFormula <> nil then
      begin
        Sender.AddValue(AFormula.AsText);
        Exit;
      end;
    end;
  end;
  Sender.SetError(ecNA);
end;

function dxBooleanLookupComparer(const ABoolValue: Variant; AValue2: TdxSpreadSheetVectorValue): Integer; inline;
begin
  if AValue2.IsError or not dxIsLogical(AValue2.Value) then
    Result := -2
  else
    if (ABoolValue = False) and (AValue2.Value = True) then
      Result := -1
    else
      if (ABoolValue = True) and (AValue2.Value = False) then
        Result := 1
      else
        Result := 0;
end;

function dxNumericLookupComparer(const ANumericValue: Variant; AValue2: TdxSpreadSheetVectorValue): Integer; inline;
begin
  if AValue2.IsError or not dxIsNumberOrDateTime(AValue2.Value) then
    Result := -2
  else
    Result := Sign(Double(ANumericValue - AValue2.Value));
end;

function dxNullValueLookupComparer(const ANullValue: Variant; AValue2: TdxSpreadSheetVectorValue): Integer; inline;
begin
  if AValue2.IsError or not dxIsNumberOrDateTime(AValue2.Value) then
    Result := -2
  else
    Result := Sign(Double(0 - AValue2.Value));
end;

function dxStringLookupComparer(const AStrValue: Variant; AValue2: TdxSpreadSheetVectorValue): Integer; inline;
const
  AResult: array[Boolean] of Integer = (-1,1);
begin
  if AValue2.IsError or not VarIsStr(AValue2.Value) then
    Result := -2
  else
    Result := dxSpreadSheetVarCompare(dxSpreadSheetUpperCase(AStrValue), dxSpreadSheetUpperCase(AValue2.Value));
end;

function dxCheckLookupComparer(const ALookupValue: Variant): TdxSpreadSheetXLookupComparer;
begin
  @Result := nil;
  if VarIsNull(ALookupValue) then
    Result := dxNullValueLookupComparer
  else
    if dxIsLogical(ALookupValue) then
      Result := @dxBooleanLookupComparer
    else
      if dxIsNumberOrDateTime(ALookupValue) then
        Result := @dxNumericLookupComparer
      else
        if VarIsStr(ALookupValue) then
          Result := @dxStringLookupComparer;
end;

function dxGetApproximateXLookupIndexForAscending(const AValue: Variant; const AVector: TdxSpreadSheetVector;
  AComparer: TdxSpreadSheetXLookupComparer): Integer;

  function GetNextSameTypeValueIndex(AComparer: TdxSpreadSheetXLookupComparer;
    const ACheckedValue: Variant; const AEmptyValueIndex, AEndIndex: Integer): Integer;
  var
    AIndex: Integer;
  begin
    Result := AEmptyValueIndex;
    AIndex := Result + 1;
    while AIndex <= AEndIndex do
      if AComparer(ACheckedValue, AVector[AIndex]) = -2 then
        AIndex := AVector.GetNextItemIndex(AIndex, True)
      else
      begin
        Result := AIndex;
        Break;
      end;
  end;

  function ExecuteBinaryForwardSearch(AComparer: TdxSpreadSheetXLookupComparer; AStartIndex, AEndIndex: Integer): Integer;
  var
    AMedianIndex, ANextIndex: Integer;
  begin
    Result := -1;
    if AStartIndex > AEndIndex then
      Exit;
    AMedianIndex := (AStartIndex + AEndIndex) div 2;
    case AComparer(AValue, AVector[AMedianIndex]) of
      -1: Result := ExecuteBinaryForwardSearch(AComparer, AStartIndex, AMedianIndex - 1);
       0: Result := AMedianIndex;
       1: Result := Max(AMedianIndex, ExecuteBinaryForwardSearch(AComparer, AMedianIndex + 1, AEndIndex));
    else
      begin
        ANextIndex := GetNextSameTypeValueIndex(AComparer, AValue, AMedianIndex, AEndIndex);
        if ANextIndex > AMedianIndex then
          case AComparer(AValue, AVector[ANextIndex]) of
           -1: Result := ExecuteBinaryForwardSearch(AComparer, AStartIndex, AMedianIndex - 1);
            0: Result := ANextIndex;
            1: if AVector.IsAllItems then
                 Result := ANextIndex
               else
                 Result := Max(ANextIndex, ExecuteBinaryForwardSearch(AComparer, ANextIndex + 1, AEndIndex));
          end
        else
          Result := ExecuteBinaryForwardSearch(AComparer, AStartIndex, AMedianIndex - 1);
      end;
    end;
  end;

var
  AHighIndex: Integer;
begin
  AHighIndex := AVector.Length - 1;
  Result := ExecuteBinaryForwardSearch(AComparer, 0, AHighIndex);
  while (Result > -1) and (Result < AHighIndex) and (AComparer(AValue, AVector[Result + 1]) = 0) and
    (AComparer(AVector[Result].Value, AVector[Result + 1]) >= 0) do
    Inc(Result);
end;

function dxGetApproximateXLookupIndexForDescending(const AValue: Variant; const AVector: TdxSpreadSheetVector;
  AComparer: TdxSpreadSheetXLookupComparer): Integer;

  function GetPriorSameTypeValueIndex(AComparer: TdxSpreadSheetXLookupComparer;
    const ACheckedValue: Variant; const AEmptyValueIndex, AEndIndex: Integer): Integer;
  var
    AIndex: Integer;
  begin
    Result := AEmptyValueIndex;
    AIndex := Result - 1;
    while AIndex > 0 do
      if AComparer(ACheckedValue, AVector[AIndex]) = -2 then
        AIndex := AVector.GetNextItemIndex(AIndex, False)
      else
      begin
        Result := AIndex;
        Break;
      end;
  end;

  function ExecuteBinaryBackwardSearch(AComparer: TdxSpreadSheetXLookupComparer; AStartIndex, AEndIndex: Integer): Integer;
  var
    AMedianIndex, APriorIndex: Integer;
  begin
    Result := -1;
    if AStartIndex > AEndIndex then
      Exit;
    if AComparer(AValue, AVector[AStartIndex]) = 0 then
    begin
      Result := AStartIndex;
      Exit;
    end;
    AMedianIndex := (AStartIndex + AEndIndex) div 2;
    case AComparer(AValue, AVector[AMedianIndex]) of
      -1: Result := Max(AMedianIndex, ExecuteBinaryBackwardSearch(AComparer, AMedianIndex + 1, AEndIndex));
       0: Result := AMedianIndex;
       1: Result := ExecuteBinaryBackwardSearch(AComparer, AStartIndex, AMedianIndex - 1);
    else
      begin
        APriorIndex := GetPriorSameTypeValueIndex(AComparer, AValue, AMedianIndex, AEndIndex);
        if APriorIndex < AMedianIndex then
          case AComparer(AValue, AVector[APriorIndex]) of
           -1: if AVector.IsAllItems then
                 Result := APriorIndex
               else
                 Result := Max(AMedianIndex, ExecuteBinaryBackwardSearch(AComparer, AMedianIndex + 1, AEndIndex));
            0: Result := APriorIndex;
            1: Result := ExecuteBinaryBackwardSearch(AComparer, AStartIndex, APriorIndex - 1);
          end
        else
          Result := ExecuteBinaryBackwardSearch(AComparer, AMedianIndex + 1, AEndIndex);
      end;
    end;
  end;

begin
  Result := ExecuteBinaryBackwardSearch(AComparer, 0, AVector.Length - 1);
  while (Result > -1) and (Result > 0) and (AComparer(AValue, AVector[Result - 1]) = 0) and
    (AComparer(AVector[Result].Value, AVector[Result - 1]) <= 0) do
    Dec(Result);
end;

function dxGetApproximateXLookupIndex(const AValue: Variant; const AVector: TdxSpreadSheetVector; const AAscending: Boolean = True): Integer;
var
  AComparer: TdxSpreadSheetXLookupComparer;
begin
  Result := -1;
  AComparer := dxCheckLookupComparer(AValue);
  if @AComparer <> nil then
    if AAscending then
      Result := dxGetApproximateXLookupIndexForAscending(AValue, AVector, AComparer)
    else
      Result := dxGetApproximateXLookupIndexForDescending(AValue, AVector, AComparer);
end;

function dxGetExactXLookupIndex(const AValue: Variant; const AVector: TdxSpreadSheetVector): Integer;

  function HasMaskToken(const S, AToken: string; var AStartPos: Integer): Boolean;
  begin
    AStartPos := PosEx(AToken, S, AStartPos);
    Result := (AStartPos = 1) or ((AStartPos > 1) and (S[AStartPos - 1] <> '~'));
    if not Result and (AStartPos > 1) then
      Inc(AStartPos);
  end;

var
  ANeedMaskSearch: Boolean;
  APosQ, APosA: Integer;
  S: string;
begin
  ANeedMaskSearch := VarIsStr(AValue);
  if ANeedMaskSearch then
  begin
    S := VarToStr(AValue);
    APosQ := 1;
    APosA := 1;
    ANeedMaskSearch := False;
    while (APosQ > 0) or (APosA > 0) do
    begin
      ANeedMaskSearch := HasMaskToken(S, '?', APosQ) or HasMaskToken(S, '*', APosA);
      if ANeedMaskSearch then
        Break;
    end;
  end;
  if VarIsNull(AValue) then
    Result := AVector.GetFirstZeroValueIndex
  else
    Result := AVector.GetItemIndex(AValue, ANeedMaskSearch);
end;

procedure dxSetSenderResultAsVectorValue(Sender: TdxSpreadSheetFormulaResult;
  const AVector: TdxSpreadSheetVector; AValueIndex: Integer);
var
  AResult: Variant;
begin
  if AVector[AValueIndex].IsError then
    Sender.SetError(AVector[AValueIndex].ErrorCode)
  else
  begin
    AResult := AVector[AValueIndex].Value;
    if dxSpreadSheetIsNullValue(AResult) then
      AResult := 0;
    Sender.AddValue(AResult);
  end;
end;

procedure DoXLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AExtractVectorMethod: TdxSpreadSheetFormulaExtractVectorFromRange);

  function CheckDimension(ADimension: TdxSpreadSheetFormulaTokenDimension; ACheckedIndex: Integer): Boolean;
  var
    ASenderExtractColumnMethod: TdxSpreadSheetFormulaExtractVectorFromRange;
  begin
    ASenderExtractColumnMethod := Sender.ExtractColumnFromRange;
    if dxSameMethods(ASenderExtractColumnMethod, AExtractVectorMethod) then
      Result := ACheckedIndex < ADimension.ColumnCount
    else
      Result := ACheckedIndex < ADimension.RowCount;
  end;

var
  ALookupValue, AResultVectorIndex, AApproximateMatch: Variant;
  ARangeParam: TdxSpreadSheetFormulaToken;
  ARangeDimension: TdxSpreadSheetFormulaTokenDimension;
  AFirstVector, AResultVector: TdxSpreadSheetVector;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AFoundIndex: Integer;
begin
  if not (Sender.ExtractParameter(ALookupValue, AParams) and
          Sender.ExtractNumericParameterDef(AResultVectorIndex, 0, AParams, 2) and
          Sender.ExtractNumericParameterDef(AApproximateMatch, 1, 0, AParams, 3)) then
    Exit;

  if VarIsEmpty(ALookupValue) then
    ALookupValue := Null;

  AResultVectorIndex := Trunc(AResultVectorIndex) - 1;

  ARangeParam := AParams.Next.FirstChild;
  dxSpreadSheetIsReferenceToken(ARangeParam, Sender);
  ARangeDimension := ARangeParam.GetDimension(AErrorCode);
  if AErrorCode = ecNone then
  begin
    AFirstVector := AExtractVectorMethod(ARangeParam, 0, AErrorCode);
    try
      if AErrorCode = ecNone then
      begin
        if AApproximateMatch <> 0 then
          AFoundIndex := dxGetApproximateXLookupIndex(ALookupValue, AFirstVector)
        else
          AFoundIndex := dxGetExactXLookupIndex(ALookupValue, AFirstVector);
        if AFoundIndex = -1 then
          Sender.SetError(ecNA)
        else
          if AResultVectorIndex < 0 then
            Sender.SetError(ecValue)
          else
          begin
            AResultVector := AExtractVectorMethod(ARangeParam, AResultVectorIndex, AErrorCode);
            try
              if AErrorCode = ecNone then
                if not CheckDimension(ARangeDimension, AResultVectorIndex) then
                  Sender.SetError(ecRefErr)
                else
                  dxSetSenderResultAsVectorValue(Sender, AResultVector, AFoundIndex);
            finally
              FreeAndNil(AResultVector);
            end;
          end;
      end
    finally
      FreeAndNil(AFirstVector);
    end;
  end;
  if Sender.Validate and (AErrorCode <> ecNone) then
    Sender.SetError(AErrorCode);
end;

procedure fnHLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  DoXLookup(Sender, AParams, Sender.ExtractRowFromRange);
end;

procedure fnVLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  DoXLookup(Sender, AParams, Sender.ExtractColumnFromRange);
end;

procedure fnLookup(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

    function ExtractResultVector(ALookupRange: TdxSpreadSheetFormulaToken; const ALookupRangeDimension: TdxSpreadSheetFormulaTokenDimension;
      AExtractLookupVectorMethod: TdxSpreadSheetFormulaExtractVectorFromRange; const ALookupVectorLength: Integer): TdxSpreadSheetVector;
    var
      AResultRange: TdxSpreadSheetFormulaToken;
      AResultRangeDimension: TdxSpreadSheetFormulaTokenDimension;
      AErrorCode: TdxSpreadSheetFormulaErrorCode;
    begin
      Result := nil;
      if Sender.ParameterExists(AParams, 2) then
      begin
        AResultRange := AParams.Next.Next.FirstChild;
        dxSpreadSheetIsReferenceToken(AResultRange, Sender);
        AResultRangeDimension := AResultRange.GetDimension(AErrorCode);
        if (AErrorCode <> ecNone) or ((AResultRangeDimension.ColumnCount > 1) and (AResultRangeDimension.RowCount > 1)) then
        begin
          if AErrorCode <> ecNone then
            Sender.SetError(AErrorCode)
          else
            Sender.SetError(ecNA);
        end
        else
          if AResultRangeDimension.RowCount > 1 then
          begin
            if AResultRange is TdxSpreadSheetFormulaReference then
              Result := TdxSpreadSheetFormulaReference(AResultRange).ExtractVector(ALookupVectorLength, True)
            else
              Result := Sender.ExtractColumnFromRange(AResultRange, 0, AErrorCode)
          end
          else
          begin
            if AResultRange is TdxSpreadSheetFormulaReference then
              Result := TdxSpreadSheetFormulaReference(AResultRange).ExtractVector(ALookupVectorLength, False)
            else
              Result := Sender.ExtractRowFromRange(AResultRange, 0, AErrorCode);
          end;
      end
      else
        Result := AExtractLookupVectorMethod(ALookupRange,
          Min(ALookupRangeDimension.ColumnCount - 1, ALookupRangeDimension.RowCount - 1), AErrorCode);
    end;

var
  ALookupValue: Variant;
  ALookupRange: TdxSpreadSheetFormulaToken;
  ALookupRangeDimension: TdxSpreadSheetFormulaTokenDimension;
  AExtractLookupVectorMethod: TdxSpreadSheetFormulaExtractVectorFromRange;
  ALookupVector, AResultVector: TdxSpreadSheetVector;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AFoundIndex: Integer;
begin
  if not Sender.ExtractParameter(ALookupValue, AParams) then
    Exit;

  if VarIsEmpty(ALookupValue) then
    ALookupValue := Null;

  AErrorCode := ecNone;
  ALookupRange := AParams.Next.FirstChild;
  dxSpreadSheetIsReferenceToken(ALookupRange, Sender);
  ALookupRangeDimension := ALookupRange.GetDimension(AErrorCode);
  if ALookupRangeDimension.ColumnCount > ALookupRangeDimension.RowCount then
    AExtractLookupVectorMethod := Sender.ExtractRowFromRange
  else
    AExtractLookupVectorMethod := Sender.ExtractColumnFromRange;
  ALookupVector := AExtractLookupVectorMethod(ALookupRange, 0, AErrorCode);
  try
    if AErrorCode = ecNone then
    begin
      AResultVector := ExtractResultVector(ALookupRange, ALookupRangeDimension, AExtractLookupVectorMethod, ALookupVector.Length);
      try
        if Sender.Validate then
        begin
          AFoundIndex := dxGetApproximateXLookupIndex(ALookupValue, ALookupVector);
          if (AFoundIndex = -1) or (AFoundIndex > AResultVector.AnchorItemsIndex + AResultVector.Length - 1) then
            Sender.SetError(ecNA)
          else
            dxSetSenderResultAsVectorValue(Sender, AResultVector, AFoundIndex);
        end;
      finally
        FreeAndNil(AResultVector);
      end;
    end;
  finally
    FreeAndNil(ALookupVector);
  end;
end;

procedure fnIndex(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  procedure InternalAddResultForArrayToken(AArea: TdxSpreadSheetFormulaArrayToken;
    const ARowNum, AColumnNum: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  var
    AValue: Variant;
    AArray: TdxSpreadSheetFormulaArrayToken;
  begin
    if (ARowNum > 0) and (AColumnNum > 0) then
    begin
      TTokenAccess(AArea).GetValueAsArrayItem(ARowNum - 1, AColumnNum - 1, AValue, AErrorCode);
      if AErrorCode = ecNone then
        Sender.AddValue(AValue);
    end
    else
    begin
      if ARowNum = 0 then
        AArray := AArea.ExtractColumnAsArray(AColumnNum - 1, AErrorCode)
      else
        AArray := AArea.ExtractRowAsArray(ARowNum - 1, AErrorCode);

      if AErrorCode = ecNone then
        Sender.AddTemporary(AArray);
    end;
  end;

  procedure InternalAddResultForAreaReference(AArea: TdxSpreadSheetFormulaToken; const ARowNum, AColumnNum: Integer);
  var
    R: TRect;
  begin
    R.Left := TdxSpreadSheetFormulaReference(AArea).ActualColumn;
    R.Top := TdxSpreadSheetFormulaReference(AArea).ActualRow;
    if AArea is TdxSpreadSheetFormulaAreaReference then
    begin
      R.Right := TdxSpreadSheetFormulaAreaReference(AArea).ActualColumn2;
      R.Bottom := TdxSpreadSheetFormulaAreaReference(AArea).ActualRow2;
    end
    else
      R.BottomRight := R.TopLeft;

    if ARowNum = 0 then
      dxSpreadSheetAddReferenceToken(Sender, AArea.View, cxRect(R.Left + AColumnNum - 1, R.Top, R.Left + AColumnNum - 1, R.Bottom))
    else
      if AColumnNum = 0 then
        dxSpreadSheetAddReferenceToken(Sender, AArea.View, cxRect(R.Left, R.Top + ARowNum - 1, R.Right, R.Top + ARowNum - 1))
      else
        dxSpreadSheetAddReferenceToken(Sender, AArea.View, cxRect(R.Left + AColumnNum - 1, R.Top + ARowNum - 1, R.Left + AColumnNum - 1, R.Top + ARowNum - 1));
  end;

var
  AAreasCount: Integer;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARowNum, AColumnNum, AAreaNum: Variant;
  AArea: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
begin
  AAreasCount := dxGetAreasCount(Sender, AParams, AErrorCode);
  if AErrorCode <> ecNone then
    Sender.SetError(AErrorCode)
  else
  if Sender.ExtractParameterDef(ARowNum, 0, AParams, 1) and Sender.ExtractParameterDef(AColumnNum, 0, AParams, 2) and
     Sender.ExtractParameterDef(AAreaNum, 1, AParams, 3) then
  begin
    ARowNum := Int(ARowNum);
    AColumnNum := Int(AColumnNum);
    AAreaNum := Int(AAreaNum);
    if (ARowNum < 0) or (AColumnNum < 0) or (AAreaNum > AAreasCount) then
      Sender.SetError(ecRefErr)
    else
      if AAreaNum < 1 then
        Sender.SetError(ecValue)
      else
      begin
        AArea := dxExtractArea(Sender, AParams, AAreaNum, AErrorCode);
        if AErrorCode = ecNone then
          if (ARowNum = 0) and (AColumnNum = 0) then
            Sender.Add(AArea)
          else
          begin
            ADimension := AArea.GetDimension(AErrorCode);
            if (AErrorCode = ecNone) and ((ARowNum > ADimension.RowCount) or (AColumnNum > ADimension.ColumnCount)) then
              AErrorCode := ecRefErr;
            if AErrorCode = ecNone then
              if AArea is TdxSpreadSheetFormulaArrayToken then
                InternalAddResultForArrayToken(AArea as TdxSpreadSheetFormulaArrayToken, ARowNum, AColumnNum, AErrorCode)
              else
              begin
                dxSpreadSheetIsReferenceToken(AArea, Sender);
                InternalAddResultForAreaReference(AArea, ARowNum, AColumnNum);
              end;
          end;
        if AErrorCode <> ecNone then
          Sender.SetError(AErrorCode);
      end;
  end;
end;

procedure fnIndirect(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function CalculateExpression(const AExpression: string; AR1C1Reference: Boolean;
    out AValue: Variant; out AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
  var
    AParser: TFormulaParser;
    APosition: Integer;
    AToken: TdxSpreadSheetFormulaToken;
  begin
    AParser := TFormulaParser(TControllerAccess(Sender.Owner.Controller).CreateParser);
    try
      APosition := 1;
      AToken := nil;
      AParser.FormatSettings.R1C1Reference := AR1C1Reference;
      AParser.Initialize(AExpression, Sender.Owner.AnchorRow, Sender.Owner.AnchorColumn);
      if not AParser.IsReference(APosition, Length(AExpression), AToken) then
      begin
        if not AParser.IsDefinedName(APosition, Length(AExpression), AToken) then
          AToken := nil;
      end;
      Result := AToken <> nil;
      if Result then
      try
        AToken.Owner := Sender.Owner;
        AToken.GetValue(AValue, AErrorCode);
      finally
        TdxSpreadSheetFormulaToken.DestroyTokens(AToken);
      end;
    finally
      AParser.Free;
    end;
  end;

var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AIsA1Style: Variant;
  ARefText: Variant;
  AResult: Variant;
begin
  AErrorCode := ecNone;
  ARefText := VarToStr(Sender.ExtractStringParameter(AParams));
  if Sender.Validate then
  begin
    if ARefText = '' then
      Sender.SetError(ecRefErr)
    else
      if dxSpreadSheetIsErrorString(ARefText, AErrorCode) and (AErrorCode <> ecNone) then
        Sender.SetError(AErrorCode);
  end;

  if Sender.Validate then
  begin
    Sender.ExtractNumericParameterDef(AIsA1Style, 1, 0, AParams, 1);
    if Sender.Validate and CalculateExpression(ARefText, not Boolean(AIsA1Style), AResult, AErrorCode) then
    begin
      if AErrorCode <> ecNone then
        Sender.SetError(AErrorCode)
      else if dxSpreadSheetIsNullValue(AResult) then
        Sender.AddValue(0)
      else
        Sender.AddValue(AResult);
    end
    else
      Sender.SetError(ecRefErr);
  end;
end;

procedure fnMatch(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function ExtractLookupVector(var ALookupVector: TdxSpreadSheetVector): Boolean;
  var
    ADimension: TdxSpreadSheetFormulaTokenDimension;
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
    ALookupToken: TdxSpreadSheetFormulaToken;
  begin
    Result := (AParams.Next <> nil) and (AParams.Next.FirstChild <> nil);
    if not Result then
    begin
      Sender.SetError(ecNA);
      Exit;
    end;

    ALookupToken := AParams.Next.FirstChild;
    dxSpreadSheetIsReferenceToken(ALookupToken, Sender);
    ADimension := ALookupToken.GetDimension(AErrorCode);
    if (AErrorCode <> ecNone) or ((ADimension.ColumnCount > 1) and (ADimension.RowCount > 1)) then
    begin
      Result := False;
      if AErrorCode <> ecNone then
        Sender.SetError(AErrorCode)
      else
        Sender.SetError(ecNA);
      Exit;
    end;
    if ADimension.ColumnCount > ADimension.RowCount then
      ALookupVector := Sender.ExtractRowFromRange(ALookupToken, 0, AErrorCode)
    else
      ALookupVector := Sender.ExtractColumnFromRange(ALookupToken, 0, AErrorCode);
    Result := AErrorCode = ecNone;
    if not Result then
      Sender.SetError(AErrorCode);
  end;

var
  ALookupValue, AMatchType: Variant;
  ALookupVector: TdxSpreadSheetVector;
  AFoundIndex: Integer;
begin
  if not (Sender.ExtractParameterDef(ALookupValue, 0, AParams) and
          Sender.ExtractNumericParameterDef(AMatchType, 1, 0, AParams, 2) and
          ExtractLookupVector(ALookupVector)) then
  begin
    FreeAndNil(ALookupVector);
    Exit;
  end;

  try
    if AMatchType = 0 then
      AFoundIndex := dxGetExactXLookupIndex(ALookupValue, ALookupVector)
    else
      AFoundIndex := dxGetApproximateXLookupIndex(ALookupValue, ALookupVector, AMatchType > 0);
  finally
    FreeAndNil(ALookupVector);
  end;
  if AFoundIndex = -1 then
    Sender.SetError(ecNA)
  else
    Sender.AddValue(AFoundIndex + 1);
end;

procedure fnOffset(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function InternalCheckRect(const R: TRect): TRect;
  begin
    Result := R;
    if Result.Right < Result.Left then
      ExchangeLongWords(Result.Left, Result.Right);
    if Result.Bottom < Result.Top then
      ExchangeLongWords(Result.Top, Result.Bottom);
  end;

const
  ASign: array[Boolean] of Integer = (-1, 1);
var
  AArea: TdxSpreadSheetFormulaToken;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  ARows, AColumns, AHeight, AWidth: Variant;
  R: TRect;
begin
  AArea := dxExtractArea(Sender, AParams, 1, AErrorCode);
  if AErrorCode = ecNone then
    ADimension := AArea.GetDimension(AErrorCode);
  if AErrorCode <> ecNone then
    Sender.SetError(AErrorCode)
  else
  if Sender.ExtractNumericParameter(ARows, AParams, 1) and Sender.ExtractNumericParameter(AColumns, AParams, 2) and
     Sender.ExtractNumericParameterDef(AHeight, ADimension.RowCount, ADimension.RowCount, AParams, 3) and
     Sender.ExtractNumericParameterDef(AWidth, ADimension.ColumnCount, ADimension.ColumnCount, AParams, 4) then
  begin
    ARows := Int(ARows);
    AColumns := Int(AColumns);
    AHeight := Int(AHeight);
    AWidth := Int(AWidth);
    if (AHeight = 0) or (AWidth = 0) then
      Sender.SetError(ecRefErr)
    else
    begin
      R.Left := TdxSpreadSheetFormulaReference(AArea).ActualColumn + AColumns;
      R.Top := TdxSpreadSheetFormulaReference(AArea).ActualRow + ARows;
      R.Right := R.Left + AWidth - ASign[AWidth > 0];
      R.Bottom := R.Top + AHeight - ASign[AHeight > 0];
      dxSpreadSheetAddReferenceToken(Sender, AArea.View, InternalCheckRect(R));
    end;
  end;
end;

procedure fnRow(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParam: TdxSpreadSheetFormulaToken;
begin
  if Sender.ParameterExists(AParams, 0) then
  begin
    AParam := AParams.FirstChild;
    if not dxSpreadSheetIsReferenceToken(AParam, Sender) then
      Sender.SetError(ecValue)
    else
      Sender.AddValue(TdxSpreadSheetFormulaReference(AParam).ActualRow + 1);
  end
  else
    Sender.AddValue(Sender.Owner.AnchorRow + 1);
end;

procedure fnRows(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParam: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  if not Sender.ParameterExists(AParams, 0) then
    Sender.SetError(ecValue)
  else
  begin
    AParam := AParams.FirstChild;
    if dxSpreadSheetIsReferenceToken(AParam, Sender) or (AParam is TdxSpreadSheetFormulaArrayToken) then
      begin
        ADimension := AParam.GetDimension(AErrorCode);
        if AErrorCode = ecNone then
          Sender.AddValue(ADimension.RowCount)
        else
          Sender.SetError(AErrorCode);
      end
    else
      if (AParam is TdxSpreadSheetFormulaIntegerValueToken) or (AParam is TdxSpreadSheetFormulaFloatValueToken) then
        Sender.AddValue(1)
      else
        Sender.SetError(ecValue);
  end;
end;

procedure fnTranspose(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AOwner: TFormulaAccess;
  ASource: TTokenAccess;
  AParentToken: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AResult: TdxSpreadSheetFormulaArrayToken;
  ARow, AColumn: Integer;
  AValue: Variant;
begin
  AOwner := TFormulaAccess(Sender.Owner);
  if not AOwner.IsArrayFormula then
  begin
    Sender.SetError(ecValue);
    Exit;
  end;

  ASource := TTokenAccess(AParams.FirstChild);
  dxSpreadSheetIsReferenceToken(TdxSpreadSheetFormulaToken(ASource), Sender);
  ADimension := ASource.GetDimension(AErrorCode);
  if AErrorCode = ecNone then
  begin
    AResult := TdxSpreadSheetFormulaArrayToken.Create;
    for ARow := 0 to ADimension.ColumnCount - 1 do
      for AColumn := 0 to ADimension.RowCount - 1 do
      begin
        if (AColumn = 0) and (ARow > 0) then
          TdxSpreadSheetFormulaToken.AddChild(AResult, TdxSpreadSheetFormulaArrayRowSeparator.Create());

        ASource.GetValueAsArrayItem(AColumn, ARow, AValue, AErrorCode);
        if AValue = Unassigned then
          AValue := 0;
        AParentToken := TdxSpreadSheetFormulaToken.Create;
        TdxSpreadSheetFormulaToken.AddChild(AParentToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
        TdxSpreadSheetFormulaToken.AddChild(AResult, AParentToken);
        AErrorCode := ecNone;
      end;
    Sender.AddTemporary(AResult);
  end
  else
    Sender.SetError(AErrorCode);
end;

procedure fpiAddress(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiAreas(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
end;

procedure fpiChoose(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkUnlimited;
end;

procedure fpiColumn(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkNonRequiredValue;
end;

procedure fpiColumns(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
end;

procedure fpiFormulaText(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiHLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
end;

procedure fpiIndex(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
  AParamKind[3] := fpkNonRequiredValue;
end;

procedure fpiIndirect(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkNonRequiredArray;
end;

procedure fpiMatch(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiOffset(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiRow(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkNonRequiredValue;
end;

procedure fpiRows(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
end;

procedure fpiTranspose(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
end;

procedure fpiVLookup(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnAddress, fnAddress, fpiAddress, frkValue, 219, ftLookupAndReference, @sfnAddressDescription);
  ARepository.Add(@sfnAreas, fnAreas, fpiAreas, frkValue, 75, ftLookupAndReference, @sfnAreasDescription);
  ARepository.Add(@sfnChoose, fnChoose, fpiChoose, frkParamValue, 100, ftLookupAndReference, @sfnChooseDescription);
  ARepository.Add(@sfnColumn, fnColumn, fpiColumn, frkValue, 9, ftLookupAndReference, @sfnColumnDescription);
  ARepository.Add(@sfnColumns, fnColumns, fpiColumns, frkValue, 77, ftLookupAndReference, @sfnColumnsDescription);
  ARepository.Add(@sfnFormulaText, fnFormulaText, fpiFormulaText, frkValue, 255, ftLookupAndReference, @sfnFormulaTextDescription, 1);
  ARepository.Add(@sfnGetPivotData, nil, nil, frkValue, 358, ftLookupAndReference, @sfnGetPivotDataDescription);
  ARepository.Add(@sfnHLookup, fnHLookup, fpiHLookup, frkValue, 101, ftLookupAndReference, @sfnHLookupDescription);
  ARepository.Add(@sfnHyperlink, nil, nil, frkValue, 359, ftLookupAndReference, @sfnHyperlinkDescription);
  ARepository.Add(@sfnLookup, fnLookup, fpiLookup, frkValue, 28, ftLookupAndReference, @sfnLookupDescription);
  ARepository.Add(@sfnIndex, fnIndex, fpiIndex, frkValue, 29, ftLookupAndReference, @sfnIndexDescription);
  ARepository.Add(@sfnIndirect, fnIndirect, fpiIndirect, frkValue, 148, ftLookupAndReference, @sfnIndirectDescription);
  ARepository.Add(@sfnMatch, fnMatch, fpiMatch, frkValue, 64, ftLookupAndReference, @sfnMatchDescription);
  ARepository.Add(@sfnOffset, fnOffset, fpiOffset, frkArray, 78, ftLookupAndReference, @sfnOffsetDescription);
  ARepository.Add(@sfnRow, fnRow, fpiRow, frkValue, 8, ftLookupAndReference, @sfnRowDescription);
  ARepository.Add(@sfnRows, fnRows, fpiRows, frkValue, 76, ftLookupAndReference, @sfnRowsDescription);
  ARepository.Add(@sfnRTD, nil, nil, frkValue, 379, ftLookupAndReference, @sfnRTDDescription);
  ARepository.Add(@sfnTranspose, fnTranspose, fpiTranspose, frkArray, 83, ftLookupAndReference, @sfnTransposeDescription);
  ARepository.Add(@sfnVLookup, fnVLookup, fpiVLookup, frkValue, 102, ftLookupAndReference, @sfnVLookupDescription);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.


