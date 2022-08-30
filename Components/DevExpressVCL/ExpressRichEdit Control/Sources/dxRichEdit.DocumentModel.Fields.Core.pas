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

unit dxRichEdit.DocumentModel.Fields.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, Contnrs, Rtti,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Token,
  dxGenerics,
  dxRichEdit.Options.Simple,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core;

type
  TdxField = class;

  TdxUpdateFieldOperationType = (
    Load,
    Copy,
    Normal,
    PasteFromIE,
    CreateModelForExport);
  TdxUpdateFieldOperationTypes = set of TdxUpdateFieldOperationType;

  TdxMailMergeDataMode = (
    None,
    ViewMergedData,
    FinalMerging);

  TdxFieldResultOption = (
    MergeFormat,
    CharFormat,
    MailMergeField,
    KeepOldResult,
    HyperlinkField,
    DoNotApplyFieldCodeFormatting,
    SuppressUpdateInnerCodeFields,
    SuppressMergeUseFirstParagraphStyle);
  TdxFieldResultOptions = set of TdxFieldResultOption;

  { TdxFieldRunInterval }

  TdxFieldRunInterval = class(TdxCloneable)
  strict private
    FStart: TdxRunIndex;
    FEnd: TdxRunIndex;
  public
    constructor Create(AStart: TdxRunIndex = -1; AEnd: TdxRunIndex = -1); reintroduce;
    function Contains(ARunIndex: TdxRunIndex): Boolean; overload; virtual;
    function GetText(APieceTable: TdxCustomPieceTable): string; virtual;
    function Clone: TdxFieldRunInterval; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    //for internal use
    function Contains(AField: TdxField): Boolean; overload; virtual;
    procedure SetInterval(AStart: TdxRunIndex; AEnd: TdxRunIndex); virtual;
    procedure ShiftEndRunIndex(AOffset: Integer); virtual;
    procedure ShiftRunIndex(AOffset: Integer); virtual;

    property Start: TdxRunIndex read FStart;
    property &End: TdxRunIndex read FEnd;
  end;

  { TdxCalculatedFieldValue }

  TdxCalculatedFieldValue = class
  strict private
    class var FNullValue: TdxCalculatedFieldValue;
    class var FInvalidValue: TdxCalculatedFieldValue;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FConvertedToString: Boolean;
    FDocumentModel: TdxCustomDocumentModel;
    FOptions: TdxFieldResultOptions;
    FRawValue: TValue;
    FText: string;
  public
    constructor Create(const ARawValue: TValue); overload;
    constructor Create(const ARawValue: TValue; AOptions: TdxFieldResultOptions); overload;
    destructor Destroy; override;

    function AddOptions(AOptions: TdxFieldResultOptions): TdxCalculatedFieldValue;
    procedure SetRawValue(const AValue: TValue);
    function IsDateTimeValue: Boolean;
    function IsDocumentModelValue: Boolean;
    procedure SetValueConvertedToString(const AStringValue: string);

    function IsNull: Boolean;
    function IsInvalid: Boolean;

    class property NullValue: TdxCalculatedFieldValue read FNullValue;
    class property InvalidValue: TdxCalculatedFieldValue read FInvalidValue;
    property Options: TdxFieldResultOptions read FOptions;
    property ConvertedToString: Boolean read FConvertedToString write FConvertedToString;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property Text: string read FText;
    property RawValue: TValue read FRawValue;
  end;

  { TdxCustomCalculatedField }

  TdxCustomCalculatedField = class abstract(TcxIUnknownObject)
  public
    function Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode;
      ADocumentField: TdxField): TdxCalculatedFieldValue; virtual; abstract;

    // for internal use
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; virtual;
  end;

  { TdxField }

  TdxField = class(TdxCloneable)
  private const
    CodeViewMask = $01;
    LockedMask = $02;
  strict private
    FCode: TdxFieldRunInterval;
    FResult: TdxFieldRunInterval;
    FPieceTable: TdxCustomPieceTable;
    FBitProperties: Byte;
    FParent: TdxField;
    FIndex: Integer;
    FDisableUpdate: Boolean;
    FPreparedCalculatedField: TdxCustomCalculatedField;
    FHideByParent: Boolean;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetFirstRunIndex: TdxRunIndex;
    function GetIsCodeView: Boolean;
    function GetLocked: Boolean;
    function GetLastRunIndex: TdxRunIndex;
    procedure SetIsCodeView(const AValue: Boolean);
    procedure SetLocked(const AValue: Boolean);
    procedure InternalSetDisableUpdate(const AValue: Boolean);
  protected
    function ContainsRun(ARunIndex: TdxRunIndex): Boolean;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AIndex: Integer = -1); reintroduce;
    destructor Destroy; override;
    procedure SetDisableUpdate;
    function ContainsField(AField: TdxField): Boolean; virtual;
    function Clone: TdxCloneable; override;
    function CloneToNewPieceTable(APieceTable: TdxCustomPieceTable): TdxField;
    procedure CopyFrom(Source: TdxCloneable); override;
    function GetResultLength(APieceTable: TdxCustomPieceTable): Integer; virtual;
    function GetTopLevelParent: TdxField;
    //IdxSupportHierarchy
    function GetParent: TdxField;
    function Inside(const AField: TdxField): Boolean;

    procedure ToggleDisableUpdate;

    property Code: TdxFieldRunInterval read FCode;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property IsCodeView: Boolean read GetIsCodeView write SetIsCodeView;
    property Locked: Boolean read GetLocked write SetLocked;
    property DisableUpdate: Boolean read FDisableUpdate write InternalSetDisableUpdate;
    property &Result: TdxFieldRunInterval read FResult;
    property FirstRunIndex: TdxRunIndex read GetFirstRunIndex;
    property LastRunIndex: TdxRunIndex read GetLastRunIndex;
    property Parent: TdxField read FParent write FParent;
    property Index: Integer read FIndex write FIndex;
    property PreparedCalculatedField: TdxCustomCalculatedField read FPreparedCalculatedField write FPreparedCalculatedField;
    property HideByParent: Boolean read FHideByParent write FHideByParent;
  end;

  TdxFieldList = class(TdxList<TdxField>);

  { TdxFieldCollectionBase }

  TdxFieldCollectionBase = class
  strict private
    FInnerList: TdxFieldList;
    function GetFirst: TdxField;
    function GetLast: TdxField;
    function GetCount: Integer;
  private
    function GetItem(Index: Integer): TdxField;
    procedure SetItem(Index: Integer; const Value: TdxField);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;
    destructor Destroy; override;

    procedure Add(AField: TdxField);
    procedure Clear;
    procedure CopyTo(var AArray: TArray<TdxField>; AArrayIndex: Integer);
    function Contains(AItem: TdxField): Boolean;
    function Extract(AItem: TdxField): TdxField;
    procedure ForEach(const AAction: TdxAction<TdxField>);
    function IndexOf(AItem: TdxField): Integer;
    procedure Insert(AIndex: Integer; AItem: TdxField);
    function LookupParentFieldIndex(AIndex: Integer): Integer;
    function Remove(AItem: TdxField): Boolean;
    procedure RemoveAt(AIndex: Integer);
    procedure ExtractAt(AIndex: Integer);

    //for internal use
    procedure ClearCounters; virtual;

    property First: TdxField read GetFirst;
    property Last: TdxField read GetLast;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxField read GetItem write SetItem; default;
    property InnerList: TdxFieldList read FInnerList;
  end;

  { TdxFieldRunIndexComparable }

  TdxFieldRunIndexComparable = class(TcxIUnknownObject, IdxComparable<TdxField>)
  strict private
    FRunIndex: TdxRunIndex;
  public
    constructor Create(ARunIndex: TdxRunIndex);
    //IdxComparable
    function CompareTo(const AField: TdxField): Integer;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxFieldRunInterval }

constructor TdxFieldRunInterval.Create(AStart: TdxRunIndex = -1; AEnd: TdxRunIndex = -1);
begin
  inherited Create;
  FStart := AStart;
  FEnd := AEnd;
end;

procedure TdxFieldRunInterval.SetInterval(AStart: TdxRunIndex; AEnd: TdxRunIndex);
begin
  if AStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('start', AStart);
  if (AEnd < 0) or (AEnd < AStart) then
    TdxRichEditExceptions.ThrowArgumentException('end', AEnd);
  FStart := AStart;
  FEnd := AEnd;
end;

function TdxFieldRunInterval.Clone: TdxFieldRunInterval;
begin
  Result := TdxFieldRunInterval(inherited Clone);
end;

function TdxFieldRunInterval.Contains(AField: TdxField): Boolean;
begin
  Result := (AField.FirstRunIndex >= Start) and (AField.LastRunIndex <= &End);
end;

function TdxFieldRunInterval.Contains(ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (ARunIndex >= Start) and (ARunIndex <= &End);
end;

function TdxFieldRunInterval.GetText(APieceTable: TdxCustomPieceTable): string;
var
  ABuilder: TStringBuilder;
  ARunIndex: TdxRunIndex;
begin
  ABuilder := TStringBuilder.Create;
  try
    for ARunIndex := Start to &End do
      ABuilder.Append(TdxSimplePieceTable(APieceTable).GetRunPlainText(ARunIndex));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

procedure TdxFieldRunInterval.ShiftRunIndex(AOffset: Integer);
var
  ANewStart: TdxRunIndex;
begin
  ANewStart := FStart + AOffset;
  if ANewStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('offset', AOffset);
  FStart := ANewStart;
  FEnd := FEnd + AOffset;
end;

procedure TdxFieldRunInterval.ShiftEndRunIndex(AOffset: Integer);
var
  ANewEnd: TdxRunIndex;
begin
  ANewEnd := FEnd + AOffset;
  if ANewEnd < FStart then
    TdxRichEditExceptions.ThrowArgumentException('offset', AOffset);
  FEnd := ANewEnd;
end;

procedure TdxFieldRunInterval.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxFieldRunInterval absolute Source;
begin
  FStart := AValue.Start;
  FEnd := AValue.&End;
end;

{ TdxCalculatedFieldValue }

constructor TdxCalculatedFieldValue.Create(const ARawValue: TValue);
begin
  inherited Create;
  SetRawValue(ARawValue);
end;

constructor TdxCalculatedFieldValue.Create(const ARawValue: TValue; AOptions: TdxFieldResultOptions);
begin
  Create(ARawValue);
  AddOptions(AOptions);
end;

destructor TdxCalculatedFieldValue.Destroy;
begin
  if FRawValue.IsObject then
    FRawValue.AsObject.Free;
  inherited Destroy;
end;

class constructor TdxCalculatedFieldValue.Initialize;
begin
  FNullValue := TdxCalculatedFieldValue.Create(nil);
  FInvalidValue := TdxCalculatedFieldValue.Create(nil);
end;

class destructor TdxCalculatedFieldValue.Finalize;
begin
  FreeAndNil(FNullValue);
  FreeAndNil(FInvalidValue);
end;

function TdxCalculatedFieldValue.AddOptions(AOptions: TdxFieldResultOptions): TdxCalculatedFieldValue;
begin
  FOptions := FOptions + AOptions;
  Result := Self;
end;

procedure TdxCalculatedFieldValue.SetRawValue(const AValue: TValue);
begin
  FRawValue := AValue;
  if not FRawValue.IsEmpty and FRawValue.IsString then
    FText := AValue.AsString
  else
    FText := '';
  if not FRawValue.IsEmpty and FRawValue.IsType<TdxCustomDocumentModel> then
    FDocumentModel := Safe<TdxCustomDocumentModel>.Cast(AValue.AsObject)
  else
    FDocumentModel := nil;
  if FText <> '' then
    ConvertedToString := True;
end;

function TdxCalculatedFieldValue.IsDateTimeValue: Boolean;
begin
  Result := not FRawValue.IsEmpty and FRawValue.IsDateTime;
end;

function TdxCalculatedFieldValue.IsDocumentModelValue: Boolean;
begin
  Result := not FRawValue.IsEmpty and FRawValue.IsObject and FRawValue.IsType<TdxCustomDocumentModel>;
end;

procedure TdxCalculatedFieldValue.SetValueConvertedToString(const AStringValue: string);
begin
  FreeAndNil(FDocumentModel);
  SetRawValue(AStringValue);
  ConvertedToString := True;
end;

function TdxCalculatedFieldValue.IsNull: Boolean;
begin
  Result := Self = TdxCalculatedFieldValue.NullValue;
end;

function TdxCalculatedFieldValue.IsInvalid: Boolean;
begin
  Result := Self = TdxCalculatedFieldValue.InvalidValue;
end;

{ TdxCustomCalculatedField }

function TdxCustomCalculatedField.GetAllowedUpdateFieldTypes(
  const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Normal];
end;

{ TdxField }

constructor TdxField.Create(APieceTable: TdxCustomPieceTable; AIndex: Integer = -1);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FIndex := AIndex;
  FCode := TdxFieldRunInterval.Create;
  FResult := TdxFieldRunInterval.Create;
end;

destructor TdxField.Destroy;
begin
  FreeAndNil(FCode);
  FreeAndNil(FResult);
  FreeAndNil(FPreparedCalculatedField);
  inherited Destroy;
end;

function TdxField.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxField.GetIsCodeView: Boolean;
begin
  Result := (FBitProperties and CodeViewMask) <> 0;
end;

procedure TdxField.SetIsCodeView(const AValue: Boolean);
begin
  if AValue then
    FBitProperties := FBitProperties or CodeViewMask
  else
    FBitProperties := FBitProperties and not CodeViewMask;
end;

function TdxField.GetLocked: Boolean;
begin
  Result := (FBitProperties and LockedMask) <> 0;
end;

procedure TdxField.SetLocked(const AValue: Boolean);
begin
  if AValue then
    FBitProperties := FBitProperties or LockedMask
  else
    FBitProperties := FBitProperties and not LockedMask;
end;

procedure TdxField.InternalSetDisableUpdate(const AValue: Boolean);
begin
  if AValue <> FDisableUpdate then
    SetDisableUpdate;
end;

function TdxField.GetFirstRunIndex: TdxRunIndex;
begin
  Result := Code.Start;
end;

function TdxField.GetLastRunIndex: TdxRunIndex;
begin
  Result := Self.&Result.&End;
end;

procedure TdxField.SetDisableUpdate;
var
  AItem: TdxDisableUpdateChangedHistoryItem;
begin
  if Index < 0 then
    ToggleDisableUpdate
  else
  begin
    DocumentModel.BeginUpdate;
    try
      AItem := TdxDisableUpdateChangedHistoryItem.Create(PieceTable, Index);
      DocumentModel.History.Add(AItem);
      AItem.Execute;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

procedure TdxField.ToggleDisableUpdate;
begin
  FDisableUpdate := not FDisableUpdate;
end;

function TdxField.ContainsRun(ARunIndex: TdxRunIndex): Boolean;
begin
  Result := Code.Contains(ARunIndex) or Self.Result.Contains(ARunIndex);
end;

function TdxField.ContainsField(AField: TdxField): Boolean;
begin
  Result := (FirstRunIndex < AField.FirstRunIndex) and (AField.LastRunIndex < LastRunIndex);
end;

function TdxField.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxField.Create(PieceTable);
  Result.CopyFrom(Self);
end;

function TdxField.CloneToNewPieceTable(APieceTable: TdxCustomPieceTable): TdxField;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxField.Create(APieceTable);
  Result.CopyFrom(Self);
end;

procedure TdxField.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxField absolute Source;
begin
  Code.CopyFrom(AValue.Code);
  &Result.CopyFrom(AValue.&Result);
  FBitProperties := AValue.FBitProperties;
  FDisableUpdate := AValue.DisableUpdate;
  HideByParent := AValue.HideByParent;
end;

function TdxField.GetParent: TdxField;
begin
  Result := FParent;
end;

function TdxField.Inside(const AField: TdxField): Boolean;
begin
  Result := AField.ContainsField(Self);
end;

function TdxField.GetResultLength(APieceTable: TdxCustomPieceTable): Integer;
var
  ARunIndex: TdxRunIndex;
begin
  Result := 0;
  for ARunIndex := Self.&Result.Start to Self.&Result.&End - 1 do
    Inc(Result, APieceTable.Runs[ARunIndex].Length);
end;

function TdxField.GetTopLevelParent: TdxField;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

{ TdxFieldCollectionBase }

constructor TdxFieldCollectionBase.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FInnerList := TdxFieldList.Create(True);
end;

destructor TdxFieldCollectionBase.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxFieldCollectionBase.GetFirst: TdxField;
begin
  if Count > 0 then
    Result := Self[0]
  else
    Result := nil;
end;

function TdxFieldCollectionBase.GetLast: TdxField;
begin
  if Count > 0 then
    Result := Self[Count - 1]
  else
    Result := nil;
end;

function TdxFieldCollectionBase.GetCount: Integer;
begin
  Result := FInnerList.Count;
end;

procedure TdxFieldCollectionBase.Add(AField: TdxField);
begin
  ClearCounters;
  FInnerList.Add(AField);
end;

function TdxFieldCollectionBase.LookupParentFieldIndex(AIndex: Integer): Integer;
begin
  while True do
  begin
    if Self[AIndex].Parent = nil then
      Exit(AIndex);
    Inc(AIndex);
  end;
end;

procedure TdxFieldCollectionBase.ForEach(const AAction: TdxAction<TdxField>);
var
  I, ACount: Integer;
begin
  ACount := FInnerList.Count;
  for I := 0 to ACount - 1 do
    AAction(FInnerList[I]);
end;

procedure TdxFieldCollectionBase.ClearCounters;
begin
end;

function TdxFieldCollectionBase.IndexOf(AItem: TdxField): Integer;
begin
  Result := FInnerList.IndexOf(AItem);
end;

procedure TdxFieldCollectionBase.Insert(AIndex: Integer; AItem: TdxField);
begin
  ClearCounters;
  FInnerList.Insert(AIndex, AItem);
end;

procedure TdxFieldCollectionBase.RemoveAt(AIndex: Integer);
begin
  ClearCounters;
  FInnerList.Delete(AIndex);
end;

procedure TdxFieldCollectionBase.ExtractAt(AIndex: Integer);
begin
  ClearCounters;
  FInnerList.Extract(FInnerList[AIndex]);
end;

procedure TdxFieldCollectionBase.SetItem(Index: Integer; const Value: TdxField);
begin
  ClearCounters;
  FInnerList[Index] := Value;
end;

procedure TdxFieldCollectionBase.Clear;
begin
  ClearCounters;
  FInnerList.Clear;
end;

function TdxFieldCollectionBase.Contains(AItem: TdxField): Boolean;
begin
  Result := FInnerList.Contains(AItem);
end;

function TdxFieldCollectionBase.Extract(AItem: TdxField): TdxField;
begin
  Result := FInnerList.Extract(AItem);
end;

procedure TdxFieldCollectionBase.CopyTo(var AArray: TArray<TdxField>; AArrayIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FInnerList.Count - 1 do
    AArray[AArrayIndex + I] := FInnerList[I];
end;

function TdxFieldCollectionBase.GetItem(Index: Integer): TdxField;
begin
  Result := FInnerList[Index];
end;

function TdxFieldCollectionBase.Remove(AItem: TdxField): Boolean;
begin
  ClearCounters;
  Result := FInnerList.Remove(AItem) >= 0;
end;

{ TdxFieldRunIndexComparable }

constructor TdxFieldRunIndexComparable.Create(ARunIndex: TdxRunIndex);
begin
  inherited Create;
  FRunIndex := ARunIndex;
end;

function TdxFieldRunIndexComparable.CompareTo(const AField: TdxField): Integer;
begin
  if AField.LastRunIndex < FRunIndex then
    Result := -1
  else
    if AField.LastRunIndex > FRunIndex then
      Result := 1
    else
      Result := 0;
end;

end.
