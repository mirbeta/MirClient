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

unit dxSpreadSheetCoreFormulasHelpers;

{$I cxVer.Inc}

interface

uses
  Generics.Collections, Generics.Defaults,
  dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetCoreFormulas, Variants;

type

  { TdxSpreadSheetEnumValuesProcessingInfo }

  TdxSpreadSheetEnumValuesProcessingInfo = record
  public
    ErrorIfNonReferenceToken: Boolean;
    ForSubTotal: Boolean;
    IgnoreHiddenRows: Boolean;
    NumericListSortedInAscendingOrder: Boolean;
    PopulateNumericListWhenAccumulateResult: Boolean;

    procedure Init(const ANumericListSortedInAscendingOrder, APopulateNumericListWhenAccumulateResult,
      AErrorIfNonReferenceToken, AIgnoreHiddenRows: Boolean; const AForSubTotal: Boolean = False);
  end;

  { TdxSpreadSheetEnumValues }

  TdxSpreadSheetEnumValues = class
  strict private
    FIndex: Integer;
    FCount: Integer;
    FEmptyCount: Integer;
    FErrorCode: TdxSpreadSheetFormulaErrorCode;
    FFormatSettings: TdxSpreadSheetFormatSettings;
    FCalculated: Boolean;
    FNullCount: Integer;
    FNumericCount: Integer;
    FNumericList: TList<Double>;
    FProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo;
    FStringCount: Integer;
    FTrueCount: Integer;
    FResultValue: Variant;
    function ConvertNullToDefValue(const ADefValue: Variant; var AOutputValue: Variant; ACanConvertStrToNumeric: Boolean): Boolean;
    procedure Initialize(const AInitialResultValue: Variant);
  public
    constructor Create(const AFormatSettings: TdxSpreadSheetFormatSettings; const AProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo); overload;
    constructor Create(const AFormatSettings: TdxSpreadSheetFormatSettings; const AInitialResultValue: Variant; const AProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo); overload;
    destructor Destroy; override;
    function AddToNumericList(const AValue: Variant; const AValueIsChecked: Boolean): Boolean;
    function CheckValueOnLogical(const AValue: Variant; ACanConvertStrToNumber: Boolean; var AOutputValue: Variant): Boolean;
    function CheckValueOnNumeric(const AValue: Variant; ACanConvertStrToNumber: Boolean; AWithoutSimilarNumericByReference: Boolean; var AOutputValue: Variant): Boolean;
    function DeleteFromNumericList(const AIndex: Integer): Boolean;
    procedure IncrementPassedCellCount(const AIncrement: Integer);
    function InsertIntoNumericList(const AIndex: Integer; const ANumber: Double): Boolean;
    function IsFinalResult: Boolean;
    function PopulateNumericList(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
      const AProcessAllParams: Boolean = False): TdxSpreadSheetFormulaErrorCode;
    procedure SetErrorCode(AErrorCode: TdxSpreadSheetFormulaErrorCode);
    procedure SetCalculated;
    function Validate: Boolean;

    property Index: Integer read FIndex;
    property Count: Integer read FCount;
    property EmptyCount: Integer read FEmptyCount;
    property ErrorCode: TdxSpreadSheetFormulaErrorCode read FErrorCode;
    property FormatSettings: TdxSpreadSheetFormatSettings read FFormatSettings;
    property NullCount: Integer read FNullCount;
    property NumericCount: Integer read FNumericCount;
    property NumericList: TList<Double> read FNumericList;
    property ProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo read FProcessingInfo;
    property ResultValue: Variant read FResultValue write FResultValue;
    property StringCount: Integer read FStringCount;
    property TrueCount: Integer read FTrueCount;
  end;

  { TdxSpreadSheetAdditionalCondition }

  TdxSpreadSheetAdditionalCondition = class
  private
    FRange: TdxSpreadSheetFormulaToken;
    FValue: Variant;
    FOperation: TdxSpreadSheetFormulaOperation;
  public
    constructor Create(ARange: TdxSpreadSheetFormulaToken; const AValue: Variant; const AOperation: TdxSpreadSheetFormulaOperation);

    property Range: TdxSpreadSheetFormulaToken read FRange;
    property Value: Variant read FValue;
    property Operation: TdxSpreadSheetFormulaOperation read FOperation;
  end;

  { TdxSpreadSheetEnumValuesWithCondition }

  TdxSpreadSheetEnumValuesWithCondition = class
  private
    procedure DestroyAdditionalConditions;
  public
    AdditionalConditions: TList<TdxSpreadSheetAdditionalCondition>;
    ConditionValue: Variant;
    CountValues: Integer;
    Iteration: Integer;
    ErrorCode: TdxSpreadSheetFormulaErrorCode;
    Operation: TdxSpreadSheetFormulaOperation;
    ResultValue, Value1, Value2: Variant;
    ConditionRange, ResultRange: TdxSpreadSheetFormulaToken;

    constructor Create;
    destructor Destroy; override;
    procedure AddAdditionalCondition(AConditionRange: TdxSpreadSheetFormulaToken;
      const AConditionValue: Variant; const AOperation: TdxSpreadSheetFormulaOperation);
  end;

procedure dxSpreadSheetCalculateForEachParam(
  Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AData: TdxSpreadSheetEnumValues; ACalculateProc: TdxSpreadSheetForEachCallBack);
function dxSpreadSheetExtractedArgumentsAreNotNull(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken; var AArgument1, AArgument2: Variant): Boolean;
implementation

uses
  Math, SysUtils, dxSpreadSheetUtils, dxCore, cxDateUtils, dxSpreadSheetCoreStrs;

type
  { TdxDescendingComparerDoubleF }

  TdxDescendingComparerDoubleF = class(TComparer<Double>)
  public
    function Compare(const Left, Right: Double): Integer; override;
  end;

function dxConvertToNumberOrDateTime(const AValue: Variant): Variant;
begin
  Result := AValue;
  if dxIsDateTime(AValue) then
    Result := VarToDateTime(AValue)
  else
    if dxIsLogical(AValue) then
      Result := Integer(AValue = True);
end;

procedure dxSpreadSheetCalculateForEachParam(
  Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AData: TdxSpreadSheetEnumValues; ACalculateProc: TdxSpreadSheetForEachCallBack);
var
  ACurrentParams: TdxSpreadSheetFormulaToken;
begin
  if (AParams = nil) or not InRange(AParams.ChildCount, 1, 255) then
    AData.SetErrorCode(ecNA)
  else
  begin
    ACurrentParams := AParams;
    while not AData.IsFinalResult and (ACurrentParams <> nil) do
    begin
      Sender.ForEach(ACurrentParams, ACalculateProc, AData);
      ACurrentParams := ACurrentParams.Next;
    end
  end;
end;

function dxSpreadSheetExtractedArgumentsAreNotNull(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken; var AArgument1, AArgument2: Variant): Boolean;
begin
  Result := not(Sender.ParameterIsNull(AArgument1, AParams) or (Sender.ParameterIsNull(AArgument2, AParams.Next)));
  if not Result then
    Sender.SetError(ecNA);
end;

{ TdxDescendingComparerDoubleF }

function TdxDescendingComparerDoubleF.Compare(const Left, Right: Double): Integer;
begin
  Result := Sign(Right - Left);
end;

{ TdxSpreadSheetEnumValuesProcessingInfo }

procedure TdxSpreadSheetEnumValuesProcessingInfo.Init(const ANumericListSortedInAscendingOrder,
  APopulateNumericListWhenAccumulateResult, AErrorIfNonReferenceToken, AIgnoreHiddenRows: Boolean;
  const AForSubTotal: Boolean = False);
begin
  NumericListSortedInAscendingOrder := ANumericListSortedInAscendingOrder;
  PopulateNumericListWhenAccumulateResult := APopulateNumericListWhenAccumulateResult;
  ErrorIfNonReferenceToken := AErrorIfNonReferenceToken;
  ForSubTotal := AForSubTotal;
  IgnoreHiddenRows := AIgnoreHiddenRows;
end;

{ TdxSpreadSheetEnumValues }

constructor TdxSpreadSheetEnumValues.Create(const AFormatSettings: TdxSpreadSheetFormatSettings;
  const AProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo);
begin
  Create(AFormatSettings, 0, AProcessingInfo);
end;

constructor TdxSpreadSheetEnumValues.Create(const AFormatSettings: TdxSpreadSheetFormatSettings;
  const AInitialResultValue: Variant; const AProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo);
begin
  inherited Create;
  FFormatSettings := AFormatSettings;
  FProcessingInfo := AProcessingInfo;
  if FProcessingInfo.NumericListSortedInAscendingOrder then
    FNumericList := TList<Double>.Create
  else
    FNumericList := TList<Double>.Create(TdxDescendingComparerDoubleF.Create);
  Initialize(AInitialResultValue);
end;

destructor TdxSpreadSheetEnumValues.Destroy;
begin
  FreeAndNil(FNumericList);
  inherited Destroy;
end;

function TdxSpreadSheetEnumValues.AddToNumericList(const AValue: Variant; const AValueIsChecked: Boolean): Boolean;
begin
  Result := AValueIsChecked or dxIsNumberOrDateTime(AValue);
  if Result then
  begin
    NumericList.Add(Double(AValue));
    FNumericCount := NumericList.Count;
  end;
end;

function TdxSpreadSheetEnumValues.ConvertNullToDefValue(const ADefValue: Variant; var AOutputValue: Variant;
  ACanConvertStrToNumeric: Boolean): Boolean;
begin
  Result := ACanConvertStrToNumeric;
  if Result then
  begin
    Inc(FNumericCount);
    AOutputValue := ADefValue;
  end
  else
    Inc(FNullCount);
end;

function TdxSpreadSheetEnumValues.CheckValueOnLogical(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  var AOutputValue: Variant): Boolean;
var
  ABoolean: Boolean;
  ANumeric: Variant;
begin
  Result := False;
  AOutputValue := Null;
  Inc(FCount);
  if VarIsEmpty(AValue) then
  begin
    Inc(FEmptyCount);
    Result := True;
  end
  else
    if VarIsNull(AValue) then
      Result := ConvertNullToDefValue(False, AOutputValue, ACanConvertStrToNumber)
    else
      if dxIsText(AValue) and not ACanConvertStrToNumber then
      begin
        Inc(FStringCount);
        Result := True;
      end
      else
        if dxIsNumericOrDateTime(AValue) then
        begin
          Inc(FNumericCount);
          Result := True;
          ANumeric := dxConvertToNumberOrDateTime(AValue);
          AOutputValue := ANumeric <> 0;
        end
        else
          if dxIsText(AValue) then
          begin
            Result := (AValue <> '') and ACanConvertStrToNumber and dxTryStrToBool(AValue, ABoolean);
            if Result then
            begin
              Inc(FNumericCount);
              AOutputValue := ABoolean;
            end
            else
              Inc(FStringCount);
          end;
  if Result and not VarIsNull(AOutputValue) and AOutputValue then
    Inc(FTrueCount);
end;

function TdxSpreadSheetEnumValues.CheckValueOnNumeric(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  AWithoutSimilarNumericByReference: Boolean; var AOutputValue: Variant): Boolean;

  procedure CheckIsString(const S: Variant);
  begin
    if VarIsStr(S) then
      Inc(FStringCount);
  end;

var
  ANumeric: Variant;
  ADate: TDateTime;
begin
  Result := False;
  AOutputValue := AValue;
  Inc(FCount);
  if VarIsEmpty(AValue) then
    Inc(FEmptyCount)
  else
    if VarIsNull(AValue) then
      Result := ConvertNullToDefValue(0, AOutputValue, ACanConvertStrToNumber)
    else
      if not ACanConvertStrToNumber and AWithoutSimilarNumericByReference then
      begin
        if dxIsNumberOrDateTime(AValue) then
        begin
          Inc(FNumericCount);
          if dxIsDateTime(AValue) then
            AOutputValue := VarToDateTime(AValue);
          Result := True;
        end
        else
          CheckIsString(AValue);
      end
      else
        if dxIsNumericOrDateTime(AValue) then
        begin
          Inc(FNumericCount);
          AOutputValue := dxConvertToNumberOrDateTime(AValue);
          Result := True;
        end
        else
        begin
          ANumeric := Null;
          if dxIsText(AValue) and (AValue <> '') and ACanConvertStrToNumber and
            (dxTryStrToOrdinal(AValue, ANumeric, FormatSettings) or dxConvertToXLSDate(AValue, ADate)) then
          begin
            Inc(FNumericCount);
            if ANumeric <> Null then
              AOutputValue := ANumeric
            else
              AOutputValue := ADate;
            Result := True;
          end
          else
            CheckIsString(AValue);
        end;
end;

function TdxSpreadSheetEnumValues.DeleteFromNumericList(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= NumericList.Count - 1);
  if Result then
  begin
    NumericList.Delete(AIndex);
    FNumericCount := NumericList.Count;
  end;
end;

procedure TdxSpreadSheetEnumValues.Initialize(const AInitialResultValue: Variant);
begin
  FIndex := 0;
  FCount := 0;
  FNullCount := 0;
  FNumericCount := 0;
  FStringCount := 0;
  FEmptyCount := 0;
  FErrorCode := ecNone;
  FTrueCount := 0;
  FResultValue := AInitialResultValue;
end;

procedure TdxSpreadSheetEnumValues.IncrementPassedCellCount(const AIncrement: Integer);
begin
  Inc(FEmptyCount, AIncrement);
  Inc(FCount, AIncrement);
end;

function TdxSpreadSheetEnumValues.InsertIntoNumericList(const AIndex: Integer; const ANumber: Double): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < NumericList.Count - 1);
  if Result then
  begin
    NumericList.Insert(AIndex, ANumber);
    FNumericCount := NumericList.Count;
  end;
end;

function TdxSpreadSheetEnumValues.IsFinalResult: Boolean;
begin
  Result := FCalculated or (ErrorCode <> ecNone);
end;

function dxAddToNumericList(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData: TdxSpreadSheetEnumValues; AInfo: Pointer = nil): Boolean;
var
  ANumeric: Variant;
begin
  Result := AErrorCode = ecNone;
  if Result then
    if AData.CheckValueOnNumeric(AValue, ACanConvertStrToNumber, True, ANumeric) then
      AData.AddToNumericList(ANumeric, True)
    else
      if ACanConvertStrToNumber then
        AErrorCode := ecValue;
  AData.SetErrorCode(AErrorCode);
end;

function TdxSpreadSheetEnumValues.PopulateNumericList(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken; const AProcessAllParams: Boolean = False): TdxSpreadSheetFormulaErrorCode;
var
  ACurrentParams: TdxSpreadSheetFormulaToken;
begin
  begin
    ACurrentParams := AParams;
    while (ErrorCode = ecNone) and (ACurrentParams <> nil) do
    begin
      Sender.ForEach(ACurrentParams, @dxAddToNumericList, Self);
      if not AProcessAllParams then
        Break;
      ACurrentParams := ACurrentParams.Next;
    end
  end;
  Result := ErrorCode;
end;

procedure TdxSpreadSheetEnumValues.SetErrorCode(AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  FErrorCode := AErrorCode;
end;

procedure TdxSpreadSheetEnumValues.SetCalculated;
begin
  FCalculated := True;
end;

function TdxSpreadSheetEnumValues.Validate: Boolean;
begin
  Result := ErrorCode = ecNone;
end;

{ TdxSpreadSheetAdditionalCondition }

constructor TdxSpreadSheetAdditionalCondition.Create(ARange: TdxSpreadSheetFormulaToken;
  const AValue: Variant; const AOperation: TdxSpreadSheetFormulaOperation);
begin
  inherited Create;
  FRange := ARange;
  FValue := AValue;
  FOperation := AOperation;
end;

{ TdxSpreadSheetEnumValuesWithCondition }

constructor TdxSpreadSheetEnumValuesWithCondition.Create;
begin
  inherited Create;
  AdditionalConditions := TList<TdxSpreadSheetAdditionalCondition>.Create;
end;

destructor TdxSpreadSheetEnumValuesWithCondition.Destroy;
begin
  DestroyAdditionalConditions;
  inherited Destroy;
end;

procedure TdxSpreadSheetEnumValuesWithCondition.DestroyAdditionalConditions;
var
  I: Integer;
begin
  for I := AdditionalConditions.Count - 1 downto 0 do
    AdditionalConditions[I].Free;
  FreeAndNil(AdditionalConditions);
end;

procedure TdxSpreadSheetEnumValuesWithCondition.AddAdditionalCondition(AConditionRange: TdxSpreadSheetFormulaToken;
  const AConditionValue: Variant; const AOperation: TdxSpreadSheetFormulaOperation);
begin
  AdditionalConditions.Add(TdxSpreadSheetAdditionalCondition.Create(AConditionRange, AConditionValue, AOperation));
end;

end.
