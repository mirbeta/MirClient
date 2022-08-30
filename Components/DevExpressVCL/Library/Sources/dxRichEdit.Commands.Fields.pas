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

unit dxRichEdit.Commands.Fields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Selection,
  dxGenerics,
  dxRichEdit.Utils.Types;

type

  { TdxShowAllFieldResultsCommand }

  TdxShowAllFieldResultsCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowAllFieldCodesCommand }

  TdxShowAllFieldCodesCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxFieldBasedRichEditMenuItemSimpleCommand }

  TdxFieldBasedRichEditMenuItemSimpleCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    FField: TdxField;
  protected
    function ValidateField: Boolean; virtual;

    property Field: TdxField read FField write FField;
  public
    constructor Create(const AControl: IdxRichEditControl; AField: TdxField); reintroduce; overload;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxToggleFieldCodesCommand }

  TdxToggleFieldCodesCommand = class(TdxFieldBasedRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxCreateFieldCommand }

  TdxCreateFieldCommand = class(TdxRichEditMenuItemSimpleCommand)
  private const
    NewEmptyFieldText = '  ';
  protected
    procedure ExecuteCore; override;
    function CreateField(AForceVisible: Boolean; ASelection: TdxSelection; ASelectionStart: TdxDocumentLogPosition): TdxField; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxUpdateFieldCommand }

  TdxUpdateFieldCommand = class(TdxFieldBasedRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxUpdateFieldsCommand }

  TdxUpdateFieldsCommand = class(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    function FindFirstField(ARunIndex: TdxRunIndex): TdxField;
    function FindLastField(ARunIndex: TdxRunIndex): TdxField;
    function FindLastFieldIndex(ARunIndex: TdxRunIndex): Integer;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertFieldCoreCommand }

  TdxInsertFieldCoreCommand = class(TdxInsertTextCoreBaseCommand)
  strict private
    FFieldCode: string;
  protected
    function AllowAutoCorrect: Boolean; override;
    procedure ModifyModel; override;
    procedure UpdateField(AField: TdxField); virtual;
    function GetInsertedText: string; override;
  public
    constructor Create(const AControl: IdxRichEditControl; const AFieldCode: string); reintroduce; overload;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property FieldCode: string read FFieldCode write FFieldCode;
  end;

  { TdxInsertFieldCommand }

  TdxInsertFieldCommand = class(TdxTransactedInsertObjectCommand)
  strict private
    function GetFieldCode: string;
    procedure SetFieldCode(const AValue: string);
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    property FieldCode: string read GetFieldCode write SetFieldCode;
  end;

  { TdxInsertMergeFieldCommand }

  TdxInsertMergeFieldCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FFieldName: string;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const AFieldArgument: string); reintroduce; overload;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    property FieldArgument: string read FFieldName write FFieldName;
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertPageCountFieldCommand }

  TdxInsertPageCountFieldCommand = class(TdxInsertFieldCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertPageNumberFieldCommand }

  TdxInsertPageNumberFieldCommand = class(TdxInsertFieldCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowInsertMergeFieldFormCommand }

  TdxShowInsertMergeFieldFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleViewMergedDataCommand }

  TdxToggleViewMergedDataCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Contnrs, Math, StrUtils, dxCore,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.Types;

{ TdxShowAllFieldResultsCommand }

procedure TdxShowAllFieldResultsCommand.ExecuteCore;
begin
  DocumentModel.ToggleAllFieldCodes(False);
end;

class function TdxShowAllFieldResultsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowAllFieldResultsDescription);
end;

class function TdxShowAllFieldResultsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowAllFieldResultsMenuCaption);
end;

class function TdxShowAllFieldResultsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowAllFieldResults;
end;

class function TdxShowAllFieldResultsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowAllFieldResults;
end;

procedure TdxShowAllFieldResultsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
  AState.Checked := False;
end;

{ TdxShowAllFieldCodesCommand }

procedure TdxShowAllFieldCodesCommand.ExecuteCore;
begin
  DocumentModel.ToggleAllFieldCodes(True);
end;

class function TdxShowAllFieldCodesCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowAllFieldCodesDescription);
end;

class function TdxShowAllFieldCodesCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowAllFieldCodesMenuCaption);
end;

class function TdxShowAllFieldCodesCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowAllFieldCodes;
end;

class function TdxShowAllFieldCodesCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowAllFieldCodes;
end;

procedure TdxShowAllFieldCodesCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False
  else
  begin
    AState.Enabled := True;
    AState.Visible := True;
    AState.Checked := False;
  end;
end;

{ TdxFieldBasedRichEditMenuItemSimpleCommand }

constructor TdxFieldBasedRichEditMenuItemSimpleCommand.Create(const AControl: IdxRichEditControl; AField: TdxField);
begin
  inherited Create(AControl);
  FField := AField;
end;

procedure TdxFieldBasedRichEditMenuItemSimpleCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  if ValidateField then
    inherited ForceExecute(AState);
end;

function TdxFieldBasedRichEditMenuItemSimpleCommand.ValidateField: Boolean;
var
  ACaretPos: TdxDocumentLayoutPosition;
  APieceTable: TdxPieceTable;
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  ARunIndex: TdxRunIndex;
begin
  if FField <> nil then
    Exit(True);

  ACaretPos := ActiveView.CaretPosition.LayoutPosition;
  APieceTable := ActivePieceTable;
  AParagraphIndex := APieceTable.FindParagraphIndex(ACaretPos.LogPosition);
  AParagraph := APieceTable.Paragraphs[AParagraphIndex];
  APieceTable.FindRunStartLogPosition(AParagraph, ACaretPos.LogPosition, ARunIndex);
  FField := APieceTable.FindFieldByRunIndex(ARunIndex);
  Result := FField <> nil;
end;

{ TdxToggleFieldCodesCommand }

procedure TdxToggleFieldCodesCommand.ExecuteCore;
begin
  ActivePieceTable.ToggleFieldCodesFromCommandOrApi(Field);
end;

class function TdxToggleFieldCodesCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFieldCodesDescription);
end;

class function TdxToggleFieldCodesCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFieldCodesMenuCaption);
end;

class function TdxToggleFieldCodesCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFieldCodes;
end;

class function TdxToggleFieldCodesCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFieldCodes;
end;

procedure TdxToggleFieldCodesCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxCreateFieldCommand }

procedure TdxCreateFieldCommand.ExecuteCore;
var
  AForceVisible, ANewEmptyField: Boolean;
  ASelection: TdxSelection;
  ASelectionStart: TdxDocumentLogPosition;
begin
  DocumentModel.BeginUpdate;
  try
    AForceVisible := GetForceVisible;
    ASelection := DocumentModel.Selection;
    ASelectionStart := ASelection.NormalizedStart;
    ANewEmptyField := False;
    if DocumentModel.Selection.Length = 0 then
    begin
      Assert(ASelectionStart = ASelection.&End);
      ActivePieceTable.InsertText(ASelectionStart, NewEmptyFieldText, AForceVisible);
      ASelection.Start := ASelectionStart;
      ASelection.&End := ASelectionStart + Length(NewEmptyFieldText);
      ANewEmptyField := True;
    end;
    CreateField(AForceVisible, ASelection, ASelectionStart);
    if ANewEmptyField then
      ASelection.Start := ASelectionStart + 2
    else
      ASelection.Start := ASelectionStart + 1;
    ASelection.&End := ASelection.Start;
  finally
    DocumentModel.EndUpdate;
  end;
end;

class function TdxCreateFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateFieldDescription);
end;

class function TdxCreateFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateFieldMenuCaption);
end;

class function TdxCreateFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CreateField;
end;

function TdxCreateFieldCommand.CreateField(AForceVisible: Boolean; ASelection: TdxSelection; ASelectionStart: TdxDocumentLogPosition): TdxField;
begin
  Result := ActivePieceTable.CreateField(ASelectionStart, ASelection.Length, AForceVisible);
end;

procedure TdxCreateFieldCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxUpdateFieldCommand }

procedure TdxUpdateFieldCommand.ExecuteCore;
begin
  ActivePieceTable.FieldUpdater.UpdateFieldAndNestedFields(Field);
end;

class function TdxUpdateFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateFieldDescription);
end;

class function TdxUpdateFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateFieldMenuCaption);
end;

class function TdxUpdateFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.UpdateField;
end;

class function TdxUpdateFieldCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.UpdateField;
end;

procedure TdxUpdateFieldCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxUpdateFieldsCommand }

function TdxUpdateFieldsCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  Result := [];
end;

procedure TdxUpdateFieldsCommand.ModifyDocumentModelCore(const AState: IdxCommandUIState);
var
  ARunInfo: TdxRunInfo;
  AFirstField, ALastField: TdxField;
  AFieldsToUpdate: TdxFieldList;
  I: Integer;
begin
  ARunInfo := ActivePieceTable.FindRunInfo(DocumentModel.Selection.NormalizedStart, Max(1, DocumentModel.Selection.Length));
  AFirstField := FindFirstField(ARunInfo.Start.RunIndex);
  if AFirstField = nil then
    Exit;
  ALastField := FindLastField(ARunInfo.&End.RunIndex);
  if ALastField = nil then
    ALastField := AFirstField;
  AFieldsToUpdate := TdxFieldList.Create;
  try
    for I := AFirstField.Index to ALastField.Index do
      if ActivePieceTable.Fields[I].Parent = nil then
        AFieldsToUpdate.Add(ActivePieceTable.Fields[I]);
    ActivePieceTable.FieldUpdater.UpdateFields(AFieldsToUpdate, TdxUpdateFieldOperationType.Normal);
  finally
    AFieldsToUpdate.Free;
  end;
end;

function TdxUpdateFieldsCommand.FindFirstField(ARunIndex: TdxRunIndex): TdxField;
var
  AIndex: Integer;
begin
  AIndex := ActivePieceTable.FindFieldIndexByRunIndexCore(ARunIndex);
  if AIndex < 0 then
    Result := nil
  else
    Result := ActivePieceTable.Fields[AIndex].GetTopLevelParent;
end;

function TdxUpdateFieldsCommand.FindLastField(ARunIndex: TdxRunIndex): TdxField;
var
  AIndex: Integer;
begin
  AIndex := FindLastFieldIndex(ARunIndex);
  if AIndex < 0 then
    Result := nil
  else
    Result := ActivePieceTable.Fields[AIndex].GetTopLevelParent;
end;

function TdxUpdateFieldsCommand.FindLastFieldIndex(ARunIndex: TdxRunIndex): Integer;
var
  AComparator: TdxFieldRunIndexComparable;
begin
  AComparator := TdxFieldRunIndexComparable.Create(ARunIndex);
  try
    if not TdxAlgorithms1<TdxField>.BinarySearch(ActivePieceTable.Fields.InnerList, AComparator, Result) then
      Dec(Result);
  finally
    AComparator.Free;
  end;
end;

class function TdxUpdateFieldsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateFieldsDescription);
end;

class function TdxUpdateFieldsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateFieldsMenuCaption);
end;

class function TdxUpdateFieldsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.UpdateFields;
end;

procedure TdxUpdateFieldsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxInsertFieldCoreCommand }

constructor TdxInsertFieldCoreCommand.Create(const AControl: IdxRichEditControl; const AFieldCode: string);
begin
  inherited Create(AControl);
  FFieldCode := AFieldCode;
end;

function TdxInsertFieldCoreCommand.AllowAutoCorrect: Boolean;
begin
  Result := False;
end;

class function TdxInsertFieldCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFieldDescription);
end;

procedure TdxInsertFieldCoreCommand.ModifyModel;
var
  AStartPosition: TdxDocumentLogPosition;
  AField: TdxField;
begin
  AStartPosition := DocumentModel.Selection.&End;
  inherited ModifyModel;
  AField := ActivePieceTable.CreateField(AStartPosition, Length(GetInsertedText), GetForceVisible);
  UpdateField(AField);
end;

procedure TdxInsertFieldCoreCommand.UpdateField(AField: TdxField);
var
  AUpdateCommand: TdxUpdateFieldCommand;
begin
  AUpdateCommand := TdxUpdateFieldCommand.Create(RichEditControl, AField);
  try
    AUpdateCommand.Execute;
  finally
    AUpdateCommand.Free;
  end;
end;

function TdxInsertFieldCoreCommand.GetInsertedText: string;
begin
  Result := Format(' %s ', [FieldCode]);
end;

class function TdxInsertFieldCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFieldMenuCaption);
end;

{ TdxInsertFieldCommand }

function TdxInsertFieldCommand.GetFieldCode: string;
var
  ACommand: TdxInsertFieldCoreCommand;
begin
  ACommand := TdxInsertFieldCoreCommand(InsertObjectCommand);
  Result := ACommand.FieldCode;
end;

class function TdxInsertFieldCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertFieldCoreCommand;
end;

procedure TdxInsertFieldCommand.SetFieldCode(const AValue: string);
var
  ACommand: TdxInsertFieldCoreCommand;
begin
  ACommand := TdxInsertFieldCoreCommand(InsertObjectCommand);
  ACommand.FieldCode := AValue;
end;

{ TdxInsertMergeFieldCommand }

constructor TdxInsertMergeFieldCommand.Create(const AControl: IdxRichEditControl; const AFieldArgument: string);
begin
  inherited Create(AControl);
  FFieldName := AFieldArgument;
end;

procedure TdxInsertMergeFieldCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<string>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<string>;
  if AValueBasedState <> nil then
  begin
    if AValueBasedState.Value <> '' then
      FFieldName := AValueBasedState.Value;
  end;

  inherited ForceExecute(AState);
end;

class function TdxInsertMergeFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMergeFieldDescription);
end;

class function TdxInsertMergeFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMergeFieldMenuCaption);
end;

class function TdxInsertMergeFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertMailMergeField;
end;

procedure TdxInsertMergeFieldCommand.ExecuteCore;
var
  ACommand: TdxInsertFieldCommand;
begin
  ACommand := TdxInsertFieldCommand.Create(RichEditControl);
  try
    if (FFieldName <> '') or not ContainsStr(FFieldName, ' ')  then
      ACommand.FieldCode := Format('MERGEFIELD %s', [FFieldName])
    else
      ACommand.FieldCode := Format('MERGEFIELD "%s"', [FFieldName]);
    ACommand.CommandSourceType := CommandSourceType;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInsertMergeFieldCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

function TdxInsertMergeFieldCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxDefaultValueBasedCommandUIState<string>;
begin
  AState := TdxDefaultValueBasedCommandUIState<string>.Create;
  AState.Value := FFieldName;
  Result := AState;
end;

{ TdxInsertPageCountFieldCommand }

constructor TdxInsertPageCountFieldCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FieldCode := 'NUMPAGES';
end;

class function TdxInsertPageCountFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageCountFieldDescription);
end;

class function TdxInsertPageCountFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageCountFieldMenuCaption);
end;

class function TdxInsertPageCountFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertPageCountField;
end;

class function TdxInsertPageCountFieldCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PageCount;
end;

procedure TdxInsertPageCountFieldCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsHeaderFooter and (ActiveViewType = TdxRichEditViewType.PrintLayout);
end;

{ TdxInsertPageNumberFieldCommand }

constructor TdxInsertPageNumberFieldCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FieldCode := 'PAGE';
end;

class function TdxInsertPageNumberFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageNumberFieldDescription);
end;

class function TdxInsertPageNumberFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageNumberFieldMenuCaption);
end;

class function TdxInsertPageNumberFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertPageNumberField;
end;

class function TdxInsertPageNumberFieldCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PageNumber;
end;

procedure TdxInsertPageNumberFieldCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsHeaderFooter and (ActiveViewType = TdxRichEditViewType.PrintLayout);
end;

{ TdxShowInsertMergeFieldFormCommand }

procedure TdxShowInsertMergeFieldFormCommand.ExecuteCore;
begin
  RichEditControl.ShowInsertMergeFieldForm;
end;

class function TdxShowInsertMergeFieldFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowInsertMergeFieldFormDescription);
end;

class function TdxShowInsertMergeFieldFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowInsertMergeFieldFormMenuCaption);
end;

class function TdxShowInsertMergeFieldFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowInsertMergeFieldForm;
end;

class function TdxShowInsertMergeFieldFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertDataField;
end;

procedure TdxShowInsertMergeFieldFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable and DocumentModel.MailMergeDataController.IsReady;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxToggleViewMergedDataCommand }

procedure TdxToggleViewMergedDataCommand.ExecuteCore;
begin
  Options.MailMerge.ViewMergedData := not Options.MailMerge.ViewMergedData;
end;

class function TdxToggleViewMergedDataCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ViewMergedData;
end;

class function TdxToggleViewMergedDataCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleViewMergedDataDescription);
end;

class function TdxToggleViewMergedDataCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleViewMergedDataMenuCaption);
end;

class function TdxToggleViewMergedDataCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleViewMergedData;
end;

procedure TdxToggleViewMergedDataCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.MailMergeDataController.IsReady;
  AState.Visible := True;
  AState.Checked := Options.MailMerge.ViewMergedData;
end;

end.
