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

unit dxRichEdit.DocumentModel.History.FieldHistory;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.SectionRange;

type
  { TdxRichEditFieldHistoryItem }

  TdxRichEditFieldHistoryItem = class abstract(TdxRichEditHistoryItem)
  strict private
    FChildFieldIndices: TArray<Integer>;
  protected
    procedure SetAsParentForAllChildren(AField: TdxField);
    procedure RemoveParentFromAllChildren(ARemovedField: TdxField);
  public
    property ChildFieldIndices: TArray<Integer> read FChildFieldIndices;
  end;

  { TdxInsertFieldHistoryItem }

  TdxInsertFieldHistoryItem = class(TdxRichEditFieldHistoryItem)
  strict private
    FInsertedField: TdxField;
    FInsertedFieldIndex: Integer;
    FDisableUpdate: Boolean;
    procedure SetInsertedField(const Value: TdxField);
  protected
    FNeedDestroyField: Boolean;
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;
    procedure Execute; override;

    property InsertedFieldIndex: Integer read FInsertedFieldIndex write FInsertedFieldIndex;
    property InsertedField: TdxField read FInsertedField write SetInsertedField;
  end;

  { TdxAddFieldHistoryItem }

  TdxAddFieldHistoryItem = class(TdxRichEditFieldHistoryItem)
  private
    FCodeStartRunIndex: TdxRunIndex;
    FResultEndRunIndex: TdxRunIndex;
    FCodeEndRunIndex: TdxRunIndex;
    FInsertedFieldIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property CodeStartRunIndex: TdxRunIndex read FCodeStartRunIndex write FCodeStartRunIndex;
    property CodeEndRunIndex: TdxRunIndex read FCodeEndRunIndex write FCodeEndRunIndex;
    property ResultEndRunIndex: TdxRunIndex read FResultEndRunIndex write FResultEndRunIndex;
    property InsertedFieldIndex: Integer read FInsertedFieldIndex;
  end;

  { TdxToggleFieldCodesHistoryItem }

  TdxToggleFieldCodesHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FFieldIndex: Integer;
  protected
    function GetChangeModified: Boolean; override;
    procedure UndoCore; override;
    procedure RedoCore; override;
    procedure ToggleFieldCodes;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable; AFieldIndex: Integer); reintroduce;

    property FieldIndex: Integer read FFieldIndex;
  end;

  { TdxToggleFieldLockedHistoryItem }

  TdxToggleFieldLockedHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FFieldIndex: Integer;
  protected
    function GetChangeModified: Boolean; override;
    procedure UndoCore; override;
    procedure RedoCore; override;
    procedure ToggleFieldLocked;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable; AFieldIndex: Integer); reintroduce;

    property FieldIndex: Integer read FFieldIndex;
  end;

  { TdxRemoveFieldHistoryItem }

  TdxRemoveFieldHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FDeletedField: TdxField;
    FRemovedFieldIndex: Integer;
    FChildFields: TdxFieldList;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;

    property DeletedField: TdxField read FDeletedField;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    property RemovedFieldIndex: Integer read FRemovedFieldIndex write FRemovedFieldIndex;
  end;

  { TdxDeleteHyperlinkInfoHistoryItem }

  TdxDeleteHyperlinkInfoHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FDeletedHyperlinkInfo: TdxHyperlinkInfo;
    FFieldIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    property FieldIndex: Integer read FFieldIndex write FFieldIndex;
  end;

  { TdxReplaceHyperlinkInfoHistoryItem }

  TdxReplaceHyperlinkInfoHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FFieldIndex: Integer;
    FOldInfo: TdxHyperlinkInfo;
    FNewInfo: TdxHyperlinkInfo;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    property FieldIndex: Integer read FFieldIndex write FFieldIndex;
    property NewInfo: TdxHyperlinkInfo read FNewInfo write FNewInfo;
  end;

  { TdxDisableUpdateChangedHistoryItem }

  TdxDisableUpdateChangedHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FFieldIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
    procedure ToggleDisableUpdate;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable; AIndex: Integer); reintroduce;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Intervals.Core;

{ TdxRichEditFieldHistoryItem }

procedure TdxRichEditFieldHistoryItem.SetAsParentForAllChildren(AField: TdxField);
var
  AFields: TdxFieldCollectionBase;
  AChildIndex: Integer;
begin
  AFields := PieceTable.Fields;
  FChildFieldIndices := PieceTable.GetChildFieldIndexes(AField);
  if Length(FChildFieldIndices) > 0 then
  begin
    for AChildIndex in FChildFieldIndices do
    begin
      if AFields[AChildIndex].Parent = AField.Parent then
        AFields[AChildIndex].Parent := AField;
    end;
  end;
end;

procedure TdxRichEditFieldHistoryItem.RemoveParentFromAllChildren(ARemovedField: TdxField);
var
  AFields: TdxFieldCollectionBase;
  AChildIndex: Integer;
begin
  if FChildFieldIndices = nil then
    Exit;
  AFields := PieceTable.Fields;
  for AChildIndex in FChildFieldIndices do
  begin
    if AFields[AChildIndex].Parent = ARemovedField then
      AFields[AChildIndex].Parent := ARemovedField.Parent;
  end;
end;

{ TdxInsertFieldHistoryItem }

constructor TdxInsertFieldHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FInsertedFieldIndex := -1;
end;

destructor TdxInsertFieldHistoryItem.Destroy;
begin
  if FNeedDestroyField then
    FreeAndNil(FInsertedField);
  inherited Destroy;
end;

procedure TdxInsertFieldHistoryItem.Execute;
begin
  Assert(FInsertedField.PieceTable = PieceTable);
  PieceTable.Fields.Insert(FInsertedFieldIndex, FInsertedField);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(PieceTable, PieceTable, FInsertedFieldIndex);
end;

procedure TdxInsertFieldHistoryItem.RedoCore;
var
  AParentField: TdxField;
begin
  AParentField := PieceTable.FindFieldByRunIndex(FInsertedField.FirstRunIndex);
  FInsertedField.Parent := AParentField;
  SetAsParentForAllChildren(FInsertedField);
  Execute;
  FInsertedField.DisableUpdate := FDisableUpdate;
  FNeedDestroyField := False;
end;

procedure TdxInsertFieldHistoryItem.UndoCore;
begin
  FInsertedField.Parent := nil;
  FDisableUpdate := FInsertedField.DisableUpdate;
  PieceTable.Fields.ExtractAt(FInsertedFieldIndex);
  RemoveParentFromAllChildren(FInsertedField);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(PieceTable, PieceTable, FInsertedFieldIndex);
  FNeedDestroyField := True;
end;

procedure TdxInsertFieldHistoryItem.SetInsertedField(const Value: TdxField);
begin
  FInsertedField := Value;
  Assert(FInsertedField.PieceTable = PieceTable);
end;

{ TdxAddFieldHistoryItem }

constructor TdxAddFieldHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FInsertedFieldIndex := -1;
end;

procedure TdxAddFieldHistoryItem.RedoCore;
var
  AField, AParentField: TdxField;
  AFields: TdxFieldCollectionBase;
begin
  AField := TdxField.Create(PieceTable);
  AField.IsCodeView := True;
  AField.Code.SetInterval(FCodeStartRunIndex, FCodeEndRunIndex);
  AField.Result.SetInterval(FCodeEndRunIndex + 1, FResultEndRunIndex);
  FInsertedFieldIndex := PieceTable.GetInsertIndex(AField);
  AFields := PieceTable.Fields;
  if FInsertedFieldIndex >= 0 then
    AField.Parent := AFields[FInsertedFieldIndex]
  else
  begin
    FInsertedFieldIndex := not FInsertedFieldIndex;
    if FInsertedFieldIndex < AFields.Count then
    begin
      AParentField := AFields[FInsertedFieldIndex];
      while AParentField <> nil do
      begin
        if (AParentField.FirstRunIndex < AField.FirstRunIndex) and
            (AParentField.LastRunIndex > AField.LastRunIndex) then
          Break;
          AParentField := AParentField.Parent;
      end;
      AField.Parent := AParentField;
    end;
  end;
  SetAsParentForAllChildren(AField);
  if Length(ChildFieldIndices) > 0 then
    FInsertedFieldIndex := ChildFieldIndices[Length(ChildFieldIndices) - 1] + 1;
  AField.Index := FInsertedFieldIndex;
  AFields.Insert(FInsertedFieldIndex, AField);
  DocumentModel.ApplyChanges(PieceTable, TdxDocumentModelChangeType.Fields, AField.FirstRunIndex, AField.LastRunIndex);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(PieceTable, PieceTable, FInsertedFieldIndex);
end;

procedure TdxAddFieldHistoryItem.UndoCore;
var
  AFields: TdxFieldCollectionBase;
  ARemovedField: TdxField;
begin
  AFields := PieceTable.Fields;
  ARemovedField := AFields[FInsertedFieldIndex];
  try
    AFields.ExtractAt(FInsertedFieldIndex);
    RemoveParentFromAllChildren(ARemovedField);
    DocumentModel.ApplyChanges(PieceTable, TdxDocumentModelChangeType.Fields, 0, 0);
    TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(PieceTable, PieceTable, FInsertedFieldIndex);
  finally
    ARemovedField.Free;
  end;
end;

{ TdxToggleFieldCodesHistoryItem }

constructor TdxToggleFieldCodesHistoryItem.Create(const ADocumentModelPart: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  inherited Create(ADocumentModelPart);
  FFieldIndex := AFieldIndex;
end;

function TdxToggleFieldCodesHistoryItem.GetChangeModified: Boolean;
begin
  Result := False;
end;

procedure TdxToggleFieldCodesHistoryItem.UndoCore;
begin
  ToggleFieldCodes;
end;

procedure TdxToggleFieldCodesHistoryItem.RedoCore;
begin
  ToggleFieldCodes;
end;

procedure TdxToggleFieldCodesHistoryItem.ToggleFieldCodes;
var
  AField: TdxField;
begin
  AField := PieceTable.Fields[FieldIndex];
  AField.IsCodeView := not AField.IsCodeView;
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.ToggleFieldCodes, AField.FirstRunIndex, AField.LastRunIndex);
end;

{ TdxToggleFieldLockedHistoryItem }

constructor TdxToggleFieldLockedHistoryItem.Create(const ADocumentModelPart: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  inherited Create(ADocumentModelPart);
  FFieldIndex := AFieldIndex;
end;

function TdxToggleFieldLockedHistoryItem.GetChangeModified: Boolean;
begin
  Result := False;
end;

procedure TdxToggleFieldLockedHistoryItem.UndoCore;
begin
  ToggleFieldLocked;
end;

procedure TdxToggleFieldLockedHistoryItem.RedoCore;
begin
  ToggleFieldLocked;
end;

procedure TdxToggleFieldLockedHistoryItem.ToggleFieldLocked;
var
  AField: TdxField;
begin
  AField := PieceTable.Fields[FieldIndex];
  AField.Locked := not AField.Locked;
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.ToggleFieldLocked,
    AField.FirstRunIndex, AField.LastRunIndex);
end;

{ TdxRemoveFieldHistoryItem }

constructor TdxRemoveFieldHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FRemovedFieldIndex := -1;
  FChildFields := TdxFieldList.Create;
end;

destructor TdxRemoveFieldHistoryItem.Destroy;
begin
  FreeAndNil(FChildFields);
  FreeAndNil(FDeletedField);
  inherited Destroy;
end;

procedure TdxRemoveFieldHistoryItem.RedoCore;
var
  AFields: TdxFieldCollectionBase;
  I: Integer;
  AField: TdxField;
begin
  AFields := PieceTable.Fields;
  FDeletedField := AFields[FRemovedFieldIndex];
  AFields.Extract(FDeletedField);
  for I := FRemovedFieldIndex - 1 downto 0 do
  begin
    AField := AFields[I];
    if AField.Parent = FDeletedField then
    begin
      FChildFields.Add(AField);
      AField.Parent := FDeletedField.Parent;
    end;
  end;
  DocumentModel.ApplyChanges(PieceTable, TdxDocumentModelChangeType.Fields, 0, 0);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(PieceTable, PieceTable, FRemovedFieldIndex);
end;

procedure TdxRemoveFieldHistoryItem.UndoCore;
var
  ACount, I: Integer;
begin
  PieceTable.Fields.Insert(FRemovedFieldIndex, FDeletedField);
  ACount := FChildFields.Count;
  for I := 0 to ACount - 1 do
    FChildFields[I].Parent := FDeletedField;
  DocumentModel.ApplyChanges(PieceTable, TdxDocumentModelChangeType.Fields, FDeletedField.FirstRunIndex, FDeletedField.LastRunIndex);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(PieceTable, PieceTable, FRemovedFieldIndex);
  FDeletedField := nil;
  FChildFields.Clear;
end;

{ TdxDeleteHyperlinkInfoHistoryItem }

procedure TdxDeleteHyperlinkInfoHistoryItem.RedoCore;
begin
  FDeletedHyperlinkInfo := PieceTable.HyperlinkInfos[FFieldIndex];
  DocumentModel.RaiseHyperlinkInfoDeleted(PieceTable, FFieldIndex);
end;

procedure TdxDeleteHyperlinkInfoHistoryItem.UndoCore;
begin
  PieceTable.HyperlinkInfos[FFieldIndex] := FDeletedHyperlinkInfo;
  DocumentModel.RaiseHyperlinkInfoInserted(PieceTable, FFieldIndex);
end;

{ TdxReplaceHyperlinkInfoHistoryItem }

procedure TdxReplaceHyperlinkInfoHistoryItem.UndoCore;
begin
  PieceTable.HyperlinkInfos[FFieldIndex] := FOldInfo;
end;

procedure TdxReplaceHyperlinkInfoHistoryItem.RedoCore;
begin
  FOldInfo := PieceTable.HyperlinkInfos[FFieldIndex];
  PieceTable.HyperlinkInfos[FFieldIndex] := FNewInfo;
end;

{ TdxDisableUpdateChangedHistoryItem }

constructor TdxDisableUpdateChangedHistoryItem.Create(const ADocumentModelPart: TdxCustomPieceTable; AIndex: Integer);
begin
  inherited Create(ADocumentModelPart);
  FFieldIndex := AIndex;
end;

procedure TdxDisableUpdateChangedHistoryItem.RedoCore;
begin
  ToggleDisableUpdate;
end;

procedure TdxDisableUpdateChangedHistoryItem.UndoCore;
begin
  ToggleDisableUpdate;
end;

procedure TdxDisableUpdateChangedHistoryItem.ToggleDisableUpdate;
var
  AField: TdxField;
begin
  AField := PieceTable.Fields[FFieldIndex];
  AField.ToggleDisableUpdate;
end;

end.
