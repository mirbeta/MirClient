{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.Fields.SequenceField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type

  { TdxSequenceField }

  TdxSequenceField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'SEQ';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FId: string;
    FIncrementCounter: Boolean;
    FHideResult: Boolean;
    FNewCounterValue: Integer;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
  public
    constructor Create; override;

    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property Id: string read FId;
    property IncrementCounter: Boolean read FIncrementCounter;
    property NewCounterValue: Integer read FNewCounterValue;
    property HideResult: Boolean read FHideResult;
  end;

  { TdxSequentialCounterItem }

  TdxSequentialCounterItem = record
  strict private
    FField: TdxField;
    FValue: Integer;
  public
    constructor Create(AField: TdxField; AValue: Integer);

    property Field: TdxField read FField;
    property Value: Integer read FValue;
  end;

  { TdxSequentialFieldCounter }

  TdxSequentialFieldCounter = class
  strict private
    FId: string;
    FValues: TList<TdxSequentialCounterItem>;
  protected
    function CalculateValue(AField: TdxField; APieceTable: TdxSimplePieceTable): Integer; virtual;
    function UpdateCounter(AField: TdxField; APieceTable: TdxSimplePieceTable): Integer; virtual;
    function UpdateCounterCore(AField: TdxField; AValue: Integer; APieceTable: TdxSimplePieceTable): Integer; virtual;
  public
    constructor Create(const AId: string);
    destructor Destroy; override;

    property Id: string read FId;
  end;

  { TdxSequentialFieldManager }

  TdxSequentialFieldManager = class(TdxSequentialFieldCustomManager)
  strict private
    FPieceTable: TdxSimplePieceTable;
    FCounterTable: TdxNamedObjectDictionary<TdxSequentialFieldCounter>;
  public
    constructor Create(APieceTable: TdxSimplePieceTable);
    destructor Destroy; override;
    function CalculateCounterValue(AField: TdxCalculatedFieldBase; ADocumentField: TdxField): Integer; override;
    procedure ClearCounters; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Utils.Token,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FieldCalculatorService;

type
  { TdxSequentialCounterItemAndFieldComparable }

  TdxSequentialCounterItemAndFieldComparable = class(TcxIUnknownObject,
    IdxComparable<TdxSequentialCounterItem>)
  strict private
    FField: TdxField;
  public
    constructor Create(AField: TdxField);
    function CompareTo(const AOther: TdxSequentialCounterItem): Integer;
  end;

{ TdxSequenceField }

class constructor TdxSequenceField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('r', 's'));
end;

class destructor TdxSequenceField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

constructor TdxSequenceField.Create;
begin
  inherited Create;
  FNewCounterValue := MinInt;
end;

class function TdxSequenceField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxSequenceField.Create;
end;

function TdxSequenceField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxSequenceField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxSequenceField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
var
  ANewCounterValueString: TdxNullableValue<string>;
begin
  inherited Initialize(APieceTable, AInstructions);

  FId := AInstructions.GetArgumentAsString(0);
  FIncrementCounter := not AInstructions.GetBool('c');
  FHideResult := AInstructions.GetBool('h');

  ANewCounterValueString := AInstructions.GetString('r');
  if ANewCounterValueString.IsNull then
    FNewCounterValue := MinInt
  else
    if not TryStrToInt(ANewCounterValueString.Value, FNewCounterValue) then
      FNewCounterValue := MinInt;
end;

function TdxSequenceField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.NonMailMerge;
end;

function TdxSequenceField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATable: TdxPieceTable absolute ASourcePieceTable;
  AValue: Integer;
begin
  AValue := ATable.Fields.SequentialFieldManager.CalculateCounterValue(Self, ADocumentField);
  if FHideResult and (GeneralFormatting = nil) then
    Result := TdxCalculatedFieldValue.Create('')
  else
    Result := TdxCalculatedFieldValue.Create(AValue);
end;

{ TdxSequentialCounterItem }

constructor TdxSequentialCounterItem.Create(AField: TdxField; AValue: Integer);
begin
  FField := AField;
  FValue := AValue;
end;

{ TdxSequentialFieldCounter }

constructor TdxSequentialFieldCounter.Create(const AId: string);
begin
  inherited Create;
  FValues := TList<TdxSequentialCounterItem>.Create;
  FId := AId;
end;

destructor TdxSequentialFieldCounter.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;

function TdxSequentialFieldCounter.CalculateValue(AField: TdxField; APieceTable: TdxSimplePieceTable): Integer;
var
  AIndex: Integer;
  AComparable: TdxSequentialCounterItemAndFieldComparable;
begin
  AComparable := TdxSequentialCounterItemAndFieldComparable.Create(AField);
  try
    if TdxAlgorithms<TdxSequentialCounterItem>.BinarySearch(FValues, AComparable, AIndex) then
      Exit(FValues[AIndex].Value);
  finally
    AComparable.Free;
  end;
  Result := UpdateCounter(AField, APieceTable);
end;

function TdxSequentialFieldCounter.UpdateCounter(AField: TdxField; APieceTable: TdxSimplePieceTable): Integer;
var
  AFirstFieldIndex, AValue, ALastFieldIndex, I: Integer;
begin
  if FValues.Count > 0 then
  begin
    AFirstFieldIndex := FValues[FValues.Count - 1].Field.Index;
    AValue := FValues[FValues.Count - 1].Value;
  end
  else
  begin
    AFirstFieldIndex := -1;
    AValue := 0;
  end;
  ALastFieldIndex := AField.Index;
  for I := AFirstFieldIndex + 1 to ALastFieldIndex do
    AValue := UpdateCounterCore(APieceTable.Fields[I], AValue, APieceTable);
  Result := AValue;
end;

function TdxSequentialFieldCounter.UpdateCounterCore(AField: TdxField; AValue: Integer;
  APieceTable: TdxSimplePieceTable): Integer;
var
  AIterator: TdxDocumentFieldIterator;
  AScanner: TdxFieldScanner;
  AToken: IdxToken;
  ACalculatedField: TdxSequenceField;
  AInstructions: TdxInstructionCollection;
  ATempField: TdxCalculatedFieldBase;
  ACounterItem: TdxSequentialCounterItem;
begin
  AIterator := TdxDocumentFieldIterator.Create(APieceTable, AField);
  try
    AScanner := TdxFieldScanner.Create(AIterator, APieceTable.DocumentModel.MaxFieldSwitchLength, APieceTable.DocumentModel.EnableFieldNames, APieceTable.SupportFieldCommonStringFormat);
    try
      AToken := AScanner.Scan;
      if AToken.ActualKind in [TdxTokenKind.OpEQ, TdxTokenKind.Eq] then
        Exit(AValue);
      ATempField := TdxFieldCalculatorService.CreateField(AToken.Value);
      try
        ACalculatedField := Safe<TdxSequenceField>.Cast(ATempField);
        if ACalculatedField = nil then
          Exit(AValue);
        AInstructions := TdxFieldCalculatorService.ParseInstructions(AScanner, ACalculatedField);
        ACalculatedField.Initialize(APieceTable, AInstructions);
        if ACalculatedField.Id <> Id then
          Exit(AValue);

        if ACalculatedField.NewCounterValue <> MinInt then
          AValue := ACalculatedField.NewCounterValue
        else
          if ACalculatedField.IncrementCounter then
            Inc(AValue);

        ACounterItem := TdxSequentialCounterItem.Create(AField, AValue);
        FValues.Add(ACounterItem);
        Result := AValue;
      finally
        ATempField.Free;
      end;
    finally
      AScanner.Free;
    end;
  finally
    AIterator.Free;
  end;
end;

{ TdxSequentialCounterItemAndFieldComparable }

constructor TdxSequentialCounterItemAndFieldComparable.Create(AField: TdxField);
begin
  inherited Create;
  FField := AField;
end;

function TdxSequentialCounterItemAndFieldComparable.CompareTo(const AOther: TdxSequentialCounterItem): Integer;
var
  AOtherStart, AFieldStart: TdxRunIndex;
begin
  AOtherStart := AOther.Field.Code.Start;
  AFieldStart := FField.Code.Start;
  if AOtherStart = AFieldStart then
    Exit(0);
  if AOtherStart < AFieldStart then
    Exit(-1);
  Result := 1;
end;

{ TdxSequentialFieldManager }

constructor TdxSequentialFieldManager.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FCounterTable := TdxNamedObjectDictionary<TdxSequentialFieldCounter>.Create(True);
end;

destructor TdxSequentialFieldManager.Destroy;
begin
  FreeAndNil(FCounterTable);
  inherited Destroy;
end;

function TdxSequentialFieldManager.CalculateCounterValue(AField: TdxCalculatedFieldBase; ADocumentField: TdxField): Integer;
var
  ACounter: TdxSequentialFieldCounter;
  ASequenceField: TdxSequenceField absolute AField;
begin
  if ASequenceField.Id = '' then
    Exit(MinInt);

  if not FCounterTable.TryGetValue(ASequenceField.Id, ACounter) then
  begin
    ACounter := TdxSequentialFieldCounter.Create(ASequenceField.Id);
    FCounterTable.Add(ASequenceField.Id, ACounter);
  end;

  Result := ACounter.CalculateValue(ADocumentField, FPieceTable);
end;

procedure TdxSequentialFieldManager.ClearCounters;
begin
  FCounterTable.Clear;
end;

end.
