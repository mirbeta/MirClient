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

unit dxRichEdit.Commands.Hyperlink;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Fields,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types;

type

  { TdxShowHyperlinkFormCommand }

  TdxShowHyperlinkFormCommand = class(TdxMultiCommand)
  protected
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    function GetShowsModalDialog: Boolean; override;
    procedure CreateCommands; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxEditHyperlinkCommand }

  TdxEditHyperlinkCommand = class(TdxShowHyperlinkFormCommand)
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxCreateHyperlinkContextMenuItemCommand }

  TdxCreateHyperlinkContextMenuItemCommand = class(TdxShowHyperlinkFormCommand)
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSelectTextForHyperlinkCommand }

  TdxSelectTextForHyperlinkCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition); override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowHyperlinkFormCoreCommand }

  TdxShowHyperlinkFormCoreCommand = class(TdxRichEditSelectionCommand)
  strict private
    FHyperlinkField: TdxField;
    FShouldChangeSelection: Boolean;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure ShowInsertHyperlinkForm; virtual;
    procedure ShowEditHyperlinkForm; virtual;
    function GetSelectedHyperlinkFields: TdxFieldList;
    procedure CreateHyperlink(AHyperlinkInfo: TdxHyperlinkInfo; ASource: TdxTextToDisplaySource; ARunInfo: TdxRunInfo; const AText: string);
    procedure ChangeHyperlink(AHyperlinkInfo: TdxHyperlinkInfo; ASource: TdxTextToDisplaySource; ARunInfo: TdxRunInfo; const AText: string);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ApplyCommandRestriction(const AState: IdxCommandUIState);
    function IsTableCorrectlySelected: Boolean; virtual;
    function PerformChangeSelection: Boolean; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetFieldEndPosition(AField: TdxField): TdxDocumentLogPosition;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ExecuteCore; override;
  end;

  { TdxCreateHyperlinkCommand }

  TdxCreateHyperlinkCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FHyperlinkInfo: TdxHyperlinkInfo;
    FLength: Integer;
    FLogPosition: TdxDocumentLogPosition;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function SplitTable(const AStart, AEnd: TdxDocumentLogPosition): Boolean; virtual;
  public
    constructor Create(const AControl: IdxRichEditControl; AHyperlinkInfo: TdxHyperlinkInfo;
      ALogPosition: TdxDocumentLogPosition; ALength: Integer); reintroduce; overload;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property HyperlinkInfo: TdxHyperlinkInfo read FHyperlinkInfo write FHyperlinkInfo;
    property Length: Integer read FLength write FLength;
    property LogPosition: TdxDocumentLogPosition read FLogPosition write FLogPosition;
  end;

  { TdxInsertHyperlinkCommand }

  TdxInsertHyperlinkCommand = class(TdxTransactedMultiCommand)
  strict private
    function GetCreateHyperlinkCommand: TdxCreateHyperlinkCommand;
    function GetHyperlinkInfo: TdxHyperlinkInfo;
    function GetInsertTextCommand: TdxInsertTextCommand;
    function GetText: string;
    procedure SetHyperlinkInfo(const Value: TdxHyperlinkInfo);
    procedure SetText(const Value: string);
  protected
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    procedure CreateCommands; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    property InsertTextCommand: TdxInsertTextCommand read GetInsertTextCommand;
    property CreateHyperlinkCommand: TdxCreateHyperlinkCommand read GetCreateHyperlinkCommand;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property HyperlinkInfo: TdxHyperlinkInfo read GetHyperlinkInfo write SetHyperlinkInfo;
    property Text: string read GetText write SetText;
  end;

  { TdxHyperlinkFieldCommandBase }

  TdxHyperlinkFieldCommandBase = class abstract(TdxFieldBasedRichEditMenuItemSimpleCommand)
  protected
    procedure EnsureFieldIsHyperlink(AField: TdxField); virtual;
    function GetSelectedHyperlinkFields: TdxFieldList;
    function IsEnabled: Boolean;
    function IsTableCorrectlySelected: Boolean;
    function ValidateField: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxModifyHyperlinkCommandBase }

  TdxModifyHyperlinkCommandBase = class abstract(TdxHyperlinkFieldCommandBase)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxFollowHyperlinkCommand }

  TdxFollowHyperlinkCommand = class(TdxHyperlinkFieldCommandBase)
  protected
    procedure ExecuteCore; override;
    procedure NavigateToExternalUri(AHyperlinkInfo: TdxHyperlinkInfo); virtual;
    procedure NavigateToBookmark(const AAnchor: string); overload; virtual;
    procedure NavigateToBookmark(ABookmark: TdxBookmark); overload;
    procedure ChangeActivePieceTable(ABookmark: TdxBookmark); virtual;
    procedure SelectBookmark(ABookmark: TdxBookmark); virtual;
    procedure MakeHeaderFooterActive(APieceTable: TdxPieceTable); virtual;
    procedure MoveToPieceTableStart; virtual;
    function FindBookmarkByName(const AName: string): TdxBookmark; virtual;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxOpenHyperlinkCommand }

  TdxOpenHyperlinkCommand = class(TdxHyperlinkFieldCommandBase)
  protected
    procedure ExecuteCore; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxRemoveHyperlinkFieldCommand }

  TdxRemoveHyperlinkFieldCommand = class(TdxModifyHyperlinkCommandBase)
  protected
    procedure ExecuteCore; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Contnrs, ShellAPI, Windows, dxCore,
  dxRichEdit.Commands.Images,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.Commands.Bookmarks,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Commands.HeaderFooter;

{ TdxShowHyperlinkFormCommand }

class function TdxShowHyperlinkFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowHyperlinkFormDescription);
end;

class function TdxShowHyperlinkFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowHyperlinkFormMenuCaption);
end;

class function TdxShowHyperlinkFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowHyperlinkForm;
end;

class function TdxShowHyperlinkFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowHyperlinkForm;
end;

function TdxShowHyperlinkFormCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

function TdxShowHyperlinkFormCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable;
end;

function TdxShowHyperlinkFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxShowHyperlinkFormCommand.CreateCommands;
begin
  Commands.Add(TdxSelectTextForHyperlinkCommand.Create(RichEditControl));
  Commands.Add(TdxShowHyperlinkFormCoreCommand.Create(RichEditControl));
end;

{ TdxEditHyperlinkCommand }

class function TdxEditHyperlinkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditHyperlinkDescription);
end;

class function TdxEditHyperlinkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditHyperlinkMenuCaption);
end;

class function TdxEditHyperlinkCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EditHyperlink;
end;

{ TdxCreateHyperlinkContextMenuItemCommand }

class function TdxCreateHyperlinkContextMenuItemCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CreateHyperlink;
end;

class function TdxCreateHyperlinkContextMenuItemCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandHyperlinkDescription);
end;

class function TdxCreateHyperlinkContextMenuItemCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandHyperlinkMenuCaption);
end;

{ TdxSelectTextForHyperlinkCommand }

class function TdxSelectTextForHyperlinkCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxSelectTextForHyperlinkCommand.GetMenuCaption: string;
begin
  Result := '';
end;

function TdxSelectTextForHyperlinkCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectTextForHyperlinkCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectTextForHyperlinkCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxSelectTextForHyperlinkCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxSelectTextForHyperlinkCommand.ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition);
var
  AIterator: TdxSpellCheckerWordIterator;
  APos: TdxDocumentModelPosition;
begin
  inherited ChangeSelectionStart(ASelection, ALogPosition);
  AIterator := TdxSpellCheckerWordIterator.Create(ActivePieceTable);
  try
    APos := ASelection.Interval.NormalizedStart^;
    ASelection.Start := AIterator.MoveToWordStart(APos, False).LogPosition;
    ASelection.Interval.NormalizedStart.CopyFrom(APos);
  finally
    AIterator.Free;
  end;
end;

function TdxSelectTextForHyperlinkCommand.PerformChangeSelection: Boolean;
var
  AField: TdxField;
begin
  if DocumentModel.Selection.Length > 0 then
    Exit(False);
  AField := ActivePieceTable.FindFieldByRunIndex(DocumentModel.Selection.Interval.Start.RunIndex);
  if (AField <> nil) and ActivePieceTable.IsHyperlinkField(AField) then
    Exit(False);
  Result := inherited PerformChangeSelection;
end;

function TdxSelectTextForHyperlinkCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectTextForHyperlinkCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  AIterator: TdxSpellCheckerWordIterator;
  APosition: TdxDocumentModelPosition;
begin
  AIterator := TdxSpellCheckerWordIterator.Create(ActivePieceTable);
  try
    APosition := APos;
    AIterator.MoveToWordEndCore(APosition);
    Result := APosition.LogPosition;
  finally
    AIterator.Free;
  end;
end;

{ TdxShowHyperlinkFormCoreCommand }

class function TdxShowHyperlinkFormCoreCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxShowHyperlinkFormCoreCommand.GetMenuCaption: string;
begin
  Result := '';
end;

function TdxShowHyperlinkFormCoreCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxShowHyperlinkFormCoreCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxShowHyperlinkFormCoreCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxShowHyperlinkFormCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxShowHyperlinkFormCoreCommand.ExecuteCore;
var
  AFields: TdxFieldList;
begin
  AFields := GetSelectedHyperlinkFields;
  try
    if AFields.Count = 0 then
      ShowInsertHyperlinkForm
    else
    begin
      FHyperlinkField := AFields[0];
      ShowEditHyperlinkForm;
    end;
    inherited ExecuteCore;
  finally
    AFields.Free;
  end;
end;

procedure TdxShowHyperlinkFormCoreCommand.ShowInsertHyperlinkForm;
var
  ASelection: TdxSelection;
  ATitle: string;
  AHyperlinkInfo: TdxHyperlinkInfo;
  ARunInfo: TdxRunInfo;
begin
  ASelection := DocumentModel.Selection;
  ATitle := cxGetResourceString(@sdxRichEditInsertHyperlinkTitle);
  AHyperlinkInfo := TdxHyperlinkInfo.Create;
  ARunInfo := ASelection.Interval.Clone;
  try
    RichEditControl.ShowHyperlinkForm(AHyperlinkInfo, ARunInfo, ATitle, CreateHyperlink);
  finally
    AHyperlinkInfo.Free;
    ARunInfo.Free;
  end;
end;

procedure TdxShowHyperlinkFormCoreCommand.ShowEditHyperlinkForm;
var
  AHyperlinkInfo: TdxHyperlinkInfo;
  ARunInfo: TdxRunInfo;
  ATitle: string;
  APos: TdxDocumentModelPosition;
begin
  AHyperlinkInfo := ActivePieceTable.HyperlinkInfos[FHyperlinkField.Index].Clone;
  try
    ARunInfo := TdxRunInfo.Create(ActivePieceTable);
    try
      APos := ARunInfo.Start;
      TdxDocumentModelPosition.SetRunStart(APos, FHyperlinkField.Result.Start);
      ARunInfo.Start.CopyFrom(APos);
      APos := ARunInfo.&End;
      TdxDocumentModelPosition.SetRunStart(APos, FHyperlinkField.Result.&End);
      ARunInfo.&End.CopyFrom(APos);
      ATitle := cxGetResourceString(@sdxRichEditEditHyperlinkTitle);
      if FHyperlinkField.IsCodeView then
        ActivePieceTable.ToggleFieldCodes(FHyperlinkField);
      RichEditControl.ShowHyperlinkForm(AHyperlinkInfo, ARunInfo, ATitle, ChangeHyperlink);
    finally
      ARunInfo.Free;
    end;
  finally
    AHyperlinkInfo.Free;
  end;
end;

function TdxShowHyperlinkFormCoreCommand.GetSelectedHyperlinkFields: TdxFieldList;
var
  AInterval: TdxRunInfo;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AInterval := DocumentModel.Selection.Interval;
  AStart := AInterval.NormalizedStart^;
  AEnd := AInterval.NormalizedEnd^;
  if AEnd > AStart then
    AEnd := TdxDocumentModelPosition.MoveBackward(AEnd);
  Result := ActivePieceTable.GetHyperlinkFields(AStart.RunIndex, AEnd.RunIndex);
end;

procedure TdxShowHyperlinkFormCoreCommand.CreateHyperlink(AHyperlinkInfo: TdxHyperlinkInfo; ASource: TdxTextToDisplaySource;
  ARunInfo: TdxRunInfo; const AText: string);
var
  AStart: TdxDocumentLogPosition;
  ALength: Integer;
  ACreateCommand: TdxCreateHyperlinkCommand;
  AInsertCommand: TdxInsertHyperlinkCommand;
begin
  if ASource = TdxTextToDisplaySource.ExistingText then
  begin
    AStart := ARunInfo.NormalizedStart.LogPosition;
    ALength := ARunInfo.NormalizedEnd.LogPosition - AStart;
    ACreateCommand := TdxCreateHyperlinkCommand.Create(RichEditControl, AHyperlinkInfo, AStart, ALength);
    try
      ACreateCommand.Execute;
    finally
      ACreateCommand.Free;
    end;
  end
  else
  begin
    AInsertCommand := TdxInsertHyperlinkCommand.Create(RichEditControl);
    try
      AInsertCommand.Text := AText;
      AInsertCommand.HyperlinkInfo := AHyperlinkInfo;
      AInsertCommand.Execute;
    finally
      AInsertCommand.Free;
    end;
  end;
  FShouldChangeSelection := True;
end;

procedure TdxShowHyperlinkFormCoreCommand.ChangeHyperlink(AHyperlinkInfo: TdxHyperlinkInfo; ASource: TdxTextToDisplaySource; ARunInfo: TdxRunInfo; const AText: string);
begin
  if ASource = TdxTextToDisplaySource.NewText then
    ActivePieceTable.ModifyHyperlinkResult(FHyperlinkField, AText);
  ActivePieceTable.ModifyHyperlinkCode(FHyperlinkField, AHyperlinkInfo);
  FShouldChangeSelection := True;
end;

procedure TdxShowHyperlinkFormCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AHyperlinks: TdxFieldList;
  ACanShowHyperlinkForm: Boolean;
begin
  inherited UpdateUIStateCore(AState);

  ApplyCommandRestriction(AState);
  AHyperlinks := GetSelectedHyperlinkFields;
  try
    ACanShowHyperlinkForm := (AHyperlinks.Count = 0) or ((AHyperlinks.Count = 1) and (not AHyperlinks[0].HideByParent) or (AHyperlinks[0].Parent = nil));
    AState.Enabled := ((AState.Enabled and not DocumentModel.Selection.IsMultiSelection) and IsTableCorrectlySelected) and ACanShowHyperlinkForm;
  finally
    AHyperlinks.Free;
  end;
end;

procedure TdxShowHyperlinkFormCoreCommand.ApplyCommandRestriction(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Hyperlinks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

function TdxShowHyperlinkFormCoreCommand.IsTableCorrectlySelected: Boolean;
var
  ASelection: TdxSelection;
  AStartCell, AEndCell: TdxTableCell;
  AIsNotSelectedCellMark: Boolean;
begin
  ASelection := DocumentModel.Selection;
  AStartCell := ActivePieceTable.FindParagraph(ASelection.NormalizedStart).GetCell;
  AEndCell := ActivePieceTable.FindParagraph(ASelection.NormalizedVirtualEnd).GetCell;

  if (AStartCell = nil) and (AEndCell = nil) then
    Exit(True);

  if AEndCell = nil then
    AIsNotSelectedCellMark := False
  else
    AIsNotSelectedCellMark := ActivePieceTable.Paragraphs[AEndCell.EndParagraphIndex].EndLogPosition + 1 <> ASelection.NormalizedEnd;
  if (AStartCell = AEndCell) and AIsNotSelectedCellMark then
    Exit(True);

  if (AStartCell <> nil) and (AEndCell = nil) then
    Exit(True);

  if (((AStartCell <> nil) and (AEndCell <> nil)) and (AStartCell.Table.NestedLevel = AEndCell.Table.NestedLevel + 1)) and AIsNotSelectedCellMark then
    Exit(True);

  Result := False;
end;

function TdxShowHyperlinkFormCoreCommand.PerformChangeSelection: Boolean;
begin
  if FShouldChangeSelection then
    Result := inherited PerformChangeSelection
  else
    Result := False;
end;

function TdxShowHyperlinkFormCoreCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxShowHyperlinkFormCoreCommand.GetFieldEndPosition(AField: TdxField): TdxDocumentLogPosition;
begin
  Result := TdxDocumentModelPosition.FromRunEnd(ActivePieceTable, AField.Result.&End).LogPosition;
end;

function TdxShowHyperlinkFormCoreCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  AField: TdxField;
begin
  if FHyperlinkField <> nil then
    Exit(GetFieldEndPosition(FHyperlinkField))
  else
  begin
    AField := ActivePieceTable.FindFieldByRunIndex(APos.RunIndex);
    if AField <> nil then
      Exit(GetFieldEndPosition(AField));
  end;
  Result := APos.LogPosition;
end;

{ TdxCreateHyperlinkCommand }

constructor TdxCreateHyperlinkCommand.Create(
  const AControl: IdxRichEditControl; AHyperlinkInfo: TdxHyperlinkInfo;
  ALogPosition: TdxDocumentLogPosition; ALength: Integer);
begin
  inherited Create(AControl);
  FHyperlinkInfo := AHyperlinkInfo;
  FLogPosition := ALogPosition;
  FLength := ALength;
end;

procedure TdxCreateHyperlinkCommand.ExecuteCore;
var
  AIsSplit: Boolean;
begin
  DocumentModel.BeginUpdate;
  try
    AIsSplit := SplitTable(LogPosition, LogPosition + Length);
    if AIsSplit then
      Inc(FLength);
    ActivePieceTable.CreateHyperlink(LogPosition, Length, HyperlinkInfo, GetForceVisible);
    DocumentModel.Selection.Start := LogPosition;
    DocumentModel.Selection.&End := LogPosition;
  finally
    DocumentModel.EndUpdate;
  end;
end;

class function TdxCreateHyperlinkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateHyperlinkDescription);
end;

class function TdxCreateHyperlinkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateHyperlinkMenuCaption);
end;

function TdxCreateHyperlinkCommand.SplitTable(const AStart,
  AEnd: TdxDocumentLogPosition): Boolean;
var
  AStartCell, AEndCell: TdxTableCell;
begin
  AStartCell := ActivePieceTable.FindParagraph(AStart).GetCell;
  AEndCell := ActivePieceTable.FindParagraph(AEnd).GetCell;
  if (AStartCell = nil) or (AStartCell = AEndCell) then
    Exit(False);

  ActivePieceTable.SplitTable(AStartCell.Table.Index, AStartCell.Row.IndexInTable, GetForceVisible);
  Result := True;
end;

procedure TdxCreateHyperlinkCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Hyperlinks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxInsertHyperlinkCommand }

procedure TdxInsertHyperlinkCommand.CreateCommands;
begin
  Commands.Add(TdxInsertTextCommand.Create(RichEditControl));
  Commands.Add(TdxCreateHyperlinkCommand.Create(RichEditControl));
end;

procedure TdxInsertHyperlinkCommand.ForceExecuteCore(
  const AState: IdxCommandUIState);
begin
  CreateHyperlinkCommand.LogPosition := DocumentModel.Selection.NormalizedStart;
  CreateHyperlinkCommand.Length := Length(Text);
  inherited ForceExecuteCore(AState);
end;

function TdxInsertHyperlinkCommand.GetCreateHyperlinkCommand: TdxCreateHyperlinkCommand;
begin
  Result := TdxCreateHyperlinkCommand(Commands[1]);
end;

class function TdxInsertHyperlinkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertHyperlinkDescription);
end;

function TdxInsertHyperlinkCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

function TdxInsertHyperlinkCommand.GetHyperlinkInfo: TdxHyperlinkInfo;
begin
  Result := CreateHyperlinkCommand.HyperlinkInfo;
end;

function TdxInsertHyperlinkCommand.GetInsertTextCommand: TdxInsertTextCommand;
begin
  Result := TdxInsertTextCommand(Commands[0]);
end;

class function TdxInsertHyperlinkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertHyperlinkMenuCaption);
end;

function TdxInsertHyperlinkCommand.GetText: string;
begin
  Result := InsertTextCommand.Text;
end;

function TdxInsertHyperlinkCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable;
end;

procedure TdxInsertHyperlinkCommand.SetHyperlinkInfo(
  const Value: TdxHyperlinkInfo);
begin
  CreateHyperlinkCommand.HyperlinkInfo := Value;
end;

procedure TdxInsertHyperlinkCommand.SetText(const Value: string);
begin
  InsertTextCommand.Text := Value;
end;

{ TdxHyperlinkFieldCommandBase }

procedure TdxHyperlinkFieldCommandBase.EnsureFieldIsHyperlink(AField: TdxField);
begin
  if not ActivePieceTable.HyperlinkInfos.IsHyperlink(AField.Index) then
    TdxRichEditExceptions.ThrowArgumentException('field', AField);
end;

function TdxHyperlinkFieldCommandBase.ValidateField: Boolean;
var
  ASelectedHyperlink: TdxFieldList;
begin
  if not inherited ValidateField then
  begin
    ASelectedHyperlink := GetSelectedHyperlinkFields;
    try
      if ASelectedHyperlink.Count = 1 then
      begin
        Result := True;
        Field := ASelectedHyperlink[0];
      end
      else
        Result := False;
    finally
      ASelectedHyperlink.Free;
    end;
  end
  else
    Result := ActivePieceTable.HyperlinkInfos.IsHyperlink(Field.Index);
end;

function TdxHyperlinkFieldCommandBase.GetSelectedHyperlinkFields: TdxFieldList;
var
  AInterval: TdxRunInfo;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AInterval := DocumentModel.Selection.Interval;
  AStart := AInterval.NormalizedStart^;
  AEnd := AInterval.NormalizedEnd^;
  if AEnd > AStart then
    AEnd := TdxDocumentModelPosition.MoveBackward(AEnd);
  Result := ActivePieceTable.GetHyperlinkFields(AStart.RunIndex, AEnd.RunIndex);
end;

function TdxHyperlinkFieldCommandBase.IsEnabled: Boolean;
var
  AHyperlinks: TdxFieldList;
begin
  AHyperlinks := GetSelectedHyperlinkFields;
  try
    Result := (AHyperlinks.Count = 1) and ((not AHyperlinks[0].HideByParent) or (AHyperlinks[0].Parent = nil)) and
      not DocumentModel.Selection.IsMultiSelection and IsTableCorrectlySelected;
  finally
    AHyperlinks.Free;
  end;
end;

function TdxHyperlinkFieldCommandBase.IsTableCorrectlySelected: Boolean;
var
  ASelection: TdxSelection;
  AStartCell, AEndCell: TdxTableCell;
  AIsNotSelectedCellMark: Boolean;
begin
  ASelection := DocumentModel.Selection;
  AStartCell := ActivePieceTable.FindParagraph(ASelection.NormalizedStart).GetCell;
  AEndCell := ActivePieceTable.FindParagraph(ASelection.NormalizedVirtualEnd).GetCell;

  if (AStartCell = nil) and (AEndCell = nil) then
    Exit(True);

  if AEndCell = nil then
    AIsNotSelectedCellMark := False
  else
    AIsNotSelectedCellMark := ActivePieceTable.Paragraphs[AEndCell.EndParagraphIndex].EndLogPosition + 1 <> ASelection.NormalizedEnd;
  if (AStartCell = AEndCell) and AIsNotSelectedCellMark then
    Exit(True);

  if (AStartCell <> nil) and (AEndCell = nil) then
    Exit(True);

  if (((AStartCell <> nil) and (AEndCell <> nil)) and (AStartCell.Table.NestedLevel = AEndCell.Table.NestedLevel + 1)) and AIsNotSelectedCellMark then
    Exit(True);

  Result := False;
end;

procedure TdxHyperlinkFieldCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.DocumentCapabilities.Hyperlinks);

  AState.Enabled := AState.Enabled and IsEnabled;
end;

{ TdxModifyHyperlinkCommandBase }

procedure TdxModifyHyperlinkCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Hyperlinks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
  AState.Enabled := AState.Enabled and IsEnabled;
end;

{ TdxFollowHyperlinkCommand }

procedure TdxFollowHyperlinkCommand.ExecuteCore;
var
  AHyperlinkInfo: TdxHyperlinkInfo;
begin
  AHyperlinkInfo := ActivePieceTable.HyperlinkInfos[Field.Index];
  if AHyperlinkInfo.NavigateUri <> '' then
    NavigateToExternalUri(AHyperlinkInfo)
  else
    if AHyperlinkInfo.Anchor <> '' then
      NavigateToBookmark(AHyperlinkInfo.Anchor)
    else
      Exit;

  AHyperlinkInfo.Visited := True;
end;

procedure TdxFollowHyperlinkCommand.NavigateToExternalUri(AHyperlinkInfo: TdxHyperlinkInfo);
var
  AUri: string;
begin
  AUri := AHyperlinkInfo.CreateUrl;
  if TdxHyperlinkUriHelper.IsRelativePath(AUri) then
    AUri := TdxHyperlinkUriHelper.ConvertRelativePathToAbsolute(AHyperlinkInfo.NavigateUri,
      RichEditControl.InnerControl.Options.DocumentSaveOptions.CurrentFileName);
  dxShellExecute(AUri);
end;

procedure TdxFollowHyperlinkCommand.NavigateToBookmark(const AAnchor: string);
var
  ABookmark: TdxBookmark;
begin
  ABookmark := FindBookmarkByName(AAnchor);
  NavigateToBookmark(ABookmark);
end;

procedure TdxFollowHyperlinkCommand.NavigateToBookmark(ABookmark: TdxBookmark);
begin
  if ABookmark <> nil then
  begin
    if ABookmark.PieceTable <> DocumentModel.ActivePieceTable then
      ChangeActivePieceTable(ABookmark);
    SelectBookmark(ABookmark);
  end
  else
    MoveToPieceTableStart;
end;

procedure TdxFollowHyperlinkCommand.ChangeActivePieceTable(ABookmark: TdxBookmark);
var
  APieceTable: TdxPieceTable;
  APos: TdxDocumentLayoutPosition;
  ACommand: TdxChangeActivePieceTableCommand;
begin
  APieceTable := TdxPieceTable(ABookmark.PieceTable);
  if APieceTable.IsMain then
  begin
    APos := ActiveView.CaretPosition.LayoutPosition;
    if APos.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    begin
      ACommand := TdxChangeActivePieceTableCommand.Create(RichEditControl, APieceTable, nil, APos.Page.PageIndex);
      try
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
  end
  else
    MakeHeaderFooterActive(APieceTable);
end;

procedure TdxFollowHyperlinkCommand.SelectBookmark(ABookmark: TdxBookmark);
var
  ACommand: TdxSelectBookmarkCommand;
begin
  if ABookmark.PieceTable <> DocumentModel.ActivePieceTable then
    Exit;

  ACommand := TdxSelectBookmarkCommand.Create(RichEditControl, ABookmark);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxFollowHyperlinkCommand.MakeHeaderFooterActive(APieceTable: TdxPieceTable);
var
  ASectionHeader: TdxSectionHeader;
  ASectionFooter: TdxSectionFooter;
  AHeaderCommand: TdxMakeNearestHeaderActiveCommand;
  AFooterCommand: TdxMakeNearestFooterActiveCommand;
begin
  ASectionHeader := Safe<TdxSectionHeader>.Cast(APieceTable.ContentType);
  if ASectionHeader <> nil then
  begin
    AHeaderCommand := TdxMakeNearestHeaderActiveCommand.Create(RichEditControl, ASectionHeader);
    try
      AHeaderCommand.Execute;
    finally
      AHeaderCommand.Free;
    end;
  end
  else
  begin
    ASectionFooter := Safe<TdxSectionFooter>.Cast(APieceTable.ContentType);
    if ASectionFooter <> nil then
    begin
      AFooterCommand := TdxMakeNearestFooterActiveCommand.Create(RichEditControl, ASectionFooter);
      try
        AFooterCommand.Execute;
      finally
        AFooterCommand.Free;
      end;
    end;
  end;
end;

procedure TdxFollowHyperlinkCommand.MoveToPieceTableStart;
var
  ACommand: TdxStartOfDocumentCommand;
begin
  ACommand := TdxStartOfDocumentCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxFollowHyperlinkCommand.FindBookmarkByName(const AName: string): TdxBookmark;
var
  ABookmarks: TdxBookmarkList;
  ABookmark: TdxBookmark;
  I: Integer;
begin
  ABookmarks := DocumentModel.GetBookmarks;
  try
    for I := 0 to ABookmarks.Count - 1 do
    begin
      ABookmark := ABookmarks[I];
      if AnsiSameText(ABookmark.Name, AName) then
        Exit(ABookmark);
    end;
    Result := nil;
  finally
    ABookmarks.Free;
  end;
end;

class function TdxFollowHyperlinkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkDescription);
end;

class function TdxFollowHyperlinkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkMenuCaption);
end;

{ TdxOpenHyperlinkCommand }

procedure TdxOpenHyperlinkCommand.ExecuteCore;
begin
  RichEditControl.InnerControl.OnHyperlinkClick(Field, False);
end;

class function TdxOpenHyperlinkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkDescription);
end;

class function TdxOpenHyperlinkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkMenuCaption);
end;

class function TdxOpenHyperlinkCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.OpenHyperlink;
end;

{ TdxRemoveHyperlinkFieldCommand }

procedure TdxRemoveHyperlinkFieldCommand.ExecuteCore;
begin
  ActivePieceTable.DeleteHyperlink(Field);
end;

class function TdxRemoveHyperlinkFieldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandRemoveHyperlinkDescription);
end;

class function TdxRemoveHyperlinkFieldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandRemoveHyperlinkMenuCaption);
end;

class function TdxRemoveHyperlinkFieldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.RemoveHyperlink;
end;

class function TdxRemoveHyperlinkFieldCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DeleteHyperlink;
end;

end.
