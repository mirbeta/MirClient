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

unit dxRichEdit.Commands.FindAndReplace;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, RegularExpressions,
  dxCoreClasses, cxEdit,
  dxGenerics,
  dxRichEdit.Api.FindAndReplace,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.View.Core;

type
  TdxEndSearchCoreCommand = class;
  TdxFindAndSelectCoreCommandBase = class;
  TdxReplaceAllCoreCommand = class;

  { TdxSearchCommandBase }

  TdxSearchCommandBase = class abstract(TdxMultiCommand)
  strict private
    function GetSearchContext: TdxSearchContext;
  protected
    function GetAction: TdxSearchAction; virtual; abstract;
    function GetDirection: TdxDirection; virtual; abstract;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    procedure CreateCommands; override;
    function CreateEndSearchCommand: TdxEndSearchCoreCommand; virtual;
    procedure PopulateCommands(ACommands: TdxCommandCollection); virtual; abstract;

    property SearchContext: TdxSearchContext read GetSearchContext;
    property Action: TdxSearchAction read GetAction;
    property Direction: TdxDirection read GetDirection;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxFindNextAndSelectCommandBase }

  TdxFindNextAndSelectCommandBase = class abstract(TdxSearchCommandBase)
  protected
    function GetAction: TdxSearchAction; override;
    procedure PopulateCommands(ACommands: TdxCommandCollection); override;
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; virtual; abstract;
  end;

  { TdxFindAndSelectForwardCommand }

  TdxFindAndSelectForwardCommand = class(TdxFindNextAndSelectCommandBase)
  protected
    function GetDirection: TdxDirection; override;
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxFindAndSelectForwardCommand }

  TdxFindAndSelectBackwardCommand = class(TdxFindNextAndSelectCommandBase)
  protected
    function GetDirection: TdxDirection; override;
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxEndSearchCoreCommand }

  TdxEndSearchCoreCommand = class(TdxRichEditSelectionCommand)
  strict private
    function GetSearchContext: TdxSearchContext;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property SearchContext: TdxSearchContext read GetSearchContext;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxFindAndSelectCoreCommandBase }

  TdxFindAndSelectCoreCommandBase = class abstract(TdxRichEditSelectionCommand)
  strict private
    function GetSearchParameters: TdxSearchParameters;
    function GetSearchContext: TdxSearchContext;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function FindNextPosition: TdxRunInfo; virtual;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function CreateTextSearchProvider: TdxTextSearchProvider; virtual; abstract;

    property SearchParameters: TdxSearchParameters read GetSearchParameters;
    property SearchContext: TdxSearchContext read GetSearchContext;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxFindAndSelectForwardCoreCommand }

  TdxFindAndSelectForwardCoreCommand = class(TdxFindAndSelectCoreCommandBase)
  protected
    function CreateTextSearchProvider: TdxTextSearchProvider; override;
  end;

  { TdxFindAndSelectBackwardCoreCommand }

  TdxFindAndSelectBackwardCoreCommand = class(TdxFindAndSelectCoreCommandBase)
  protected
    function CreateTextSearchProvider: TdxTextSearchProvider; override;
  end;

  { TdxReplaceCommandBase }

  TdxReplaceCommandBase = class abstract(TdxSearchCommandBase)
  protected
    function GetAction: TdxSearchAction; override;
    procedure PopulateCommands(ACommands: TdxCommandCollection); override;
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; virtual; abstract;
  end;

  { TdxReplaceForwardCommand }

  TdxReplaceForwardCommand = class(TdxReplaceCommandBase)
  protected
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; override;
    function GetDirection: TdxDirection; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxReplaceBackwardCommand }

  TdxReplaceBackwardCommand = class(TdxReplaceCommandBase)
  protected
    function CreateFindCommand: TdxFindAndSelectCoreCommandBase; override;
    function GetDirection: TdxDirection; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxReplaceCoreCommand }

  TdxReplaceCoreCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    function GetSearchParameters: TdxSearchParameters;
    function GetSearchContext: TdxSearchContext;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ExecuteCore; override;
    function CanReplaceSelectedText: Boolean; virtual;
    function CompareStringWithSelectedText(const AText: string): Boolean;
    procedure ReplaceText(AStartPos: TdxDocumentLogPosition; ALength: Integer); virtual;

    property SearchParameters: TdxSearchParameters read GetSearchParameters;
    property SearchContext: TdxSearchContext read GetSearchContext;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxReplaceAllCommandBase }

  TdxReplaceAllCommandBase = class abstract(TdxSearchCommandBase)
  protected
    function GetAction: TdxSearchAction; override;
    procedure PopulateCommands(ACommands: TdxCommandCollection); override;
    function CreateReplaceAllCommand: TdxReplaceAllCoreCommand; virtual; abstract;
  end;

  { TdxReplaceAllForwardCommand }

  TdxReplaceAllForwardCommand = class(TdxReplaceAllCommandBase)
  protected
    function GetDirection: TdxDirection; override;
    function CreateReplaceAllCommand: TdxReplaceAllCoreCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxReplaceAllBackwardCommand }

  TdxReplaceAllBackwardCommand = class(TdxReplaceAllCommandBase)
  protected
    function GetDirection: TdxDirection; override;
    function CreateReplaceAllCommand: TdxReplaceAllCoreCommand; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxBeginReplaceAllCoreCommand }

  TdxBeginReplaceAllCoreCommand = class(TdxRichEditSelectionCommand)
  strict private
    function GetSearchContext: TdxSearchContext;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property SearchContext: TdxSearchContext read GetSearchContext;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxReplaceAllCoreCommand }

  TdxReplaceAllCoreCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    FInnerReplaceCommand: TdxReplaceCoreCommand;
    function GetSearchParameters: TdxSearchParameters;
    function GetSearchContext: TdxSearchContext;
  protected
    function CanEdit(AStart: TdxDocumentLogPosition; ALength: Integer): Boolean; virtual;
    function CreateReplaceCommand: TdxReplaceCoreCommand; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ExecuteCore; override;
    procedure OnAfterExecute; virtual;
    procedure DoReplaceAll; virtual;
    function CreateTextSearchProvider: TdxTextSearchProvider; virtual; abstract;

    property SearchParameters: TdxSearchParameters read GetSearchParameters;
    property SearchContext: TdxSearchContext read GetSearchContext;
    property InnerReplaceCommand: TdxReplaceCoreCommand read FInnerReplaceCommand;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;
  end;

  { TdxReplaceAllForwardCoreCommand }

  TdxReplaceAllForwardCoreCommand = class(TdxReplaceAllCoreCommand)
  protected
    function CreateTextSearchProvider: TdxTextSearchProvider; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxReplaceAllBackwardCoreCommand }

  TdxReplaceAllBackwardCoreCommand = class(TdxReplaceAllCoreCommand)
  protected
    function CreateTextSearchProvider: TdxTextSearchProvider; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxFindPrevNextCommand }

  TdxFindPrevNextCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    function GetSearchParameters: TdxSearchParameters;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    property SearchParameters: TdxSearchParameters read GetSearchParameters;
  end;

  { TdxFindNextCommand }

  TdxFindNextCommand = class(TdxFindPrevNextCommand)
  protected
    procedure ExecuteCore; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxFindPrevCommand }

  TdxFindPrevCommand = class(TdxFindPrevNextCommand)
  protected
    procedure ExecuteCore; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  RTLConsts, Contnrs, RegularExpressionsCore, Math, Character, Dialogs,
  dxCore,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.TextRange,
  dxCharacters,
  dxRichEdit.Dialogs.SearchText,
  dxRichEdit.Dialogs.FindAndReplaceFormHelpers,
  dxStringHelper;

{ TdxSearchCommandBase }

function TdxSearchCommandBase.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

function TdxSearchCommandBase.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

function TdxSearchCommandBase.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

procedure TdxSearchCommandBase.ForceExecute(const AState: IdxCommandUIState);
begin
  SearchContext.BeginSearch(Action, Direction);
  try
    try
      inherited ForceExecute(AState);
    except
      on E: EIncorrectRegularExpressionException do
        ShowMessage(E.Message);
    end;
  finally
    SearchContext.EndSearch;
  end;
end;

procedure TdxSearchCommandBase.CreateCommands;
begin
  PopulateCommands(Commands);
  Commands.Add(CreateEndSearchCommand);
end;

function TdxSearchCommandBase.CreateEndSearchCommand: TdxEndSearchCoreCommand;
begin
  Result := TdxEndSearchCoreCommand.Create(RichEditControl);
end;

{ TdxFindNextAndSelectCommandBase }

function TdxFindNextAndSelectCommandBase.GetAction: TdxSearchAction;
begin
  Result := TdxSearchAction.Find;
end;

procedure TdxFindNextAndSelectCommandBase.PopulateCommands(ACommands: TdxCommandCollection);
begin
  ACommands.Add(CreateFindCommand);
end;

{ TdxFindAndSelectForwardCommand }

class function TdxFindAndSelectForwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindAndSelectForwardMenuCaption);
end;

class function TdxFindAndSelectForwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindAndSelectForwardDescription);
end;

class function TdxFindAndSelectForwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FindForward;
end;

function TdxFindAndSelectForwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Forward;
end;

function TdxFindAndSelectForwardCommand.CreateFindCommand: TdxFindAndSelectCoreCommandBase;
begin
  Result := TdxFindAndSelectForwardCoreCommand.Create(RichEditControl);
end;

{ TdxFindAndSelectBackwardCommand }

function TdxFindAndSelectBackwardCommand.CreateFindCommand: TdxFindAndSelectCoreCommandBase;
begin
  Result := TdxFindAndSelectBackwardCoreCommand.Create(RichEditControl);
end;

class function TdxFindAndSelectBackwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindAndSelectBackwardDescription);
end;

function TdxFindAndSelectBackwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Backward;
end;

class function TdxFindAndSelectBackwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindAndSelectBackwardMenuCaption);
end;

class function TdxFindAndSelectBackwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FindBackward;
end;

{ TdxEndSearchCoreCommand }

function TdxEndSearchCoreCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxEndSearchCoreCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxEndSearchCoreCommand.ChangeSelection(ASelection: TdxSelection);
begin
  ASelection.Start := SearchContext.StartSelectionAnchor.Position.LogPosition;
  ASelection.&End := SearchContext.EndSelectionAnchor.Position.LogPosition;
  inherited ChangeSelection(ASelection);
end;

class function TdxEndSearchCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxEndSearchCoreCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxEndSearchCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

function TdxEndSearchCoreCommand.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

function TdxEndSearchCoreCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxEndSearchCoreCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxEndSearchCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxEndSearchCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := SearchContext.EndOfSearch;
  AState.Visible := True;
end;

{ TdxFindAndSelectCoreCommandBase }

function TdxFindAndSelectCoreCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxFindAndSelectCoreCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

class function TdxFindAndSelectCoreCommandBase.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxFindAndSelectCoreCommandBase.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxFindAndSelectCoreCommandBase.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

function TdxFindAndSelectCoreCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxFindAndSelectCoreCommandBase.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

function TdxFindAndSelectCoreCommandBase.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

procedure TdxFindAndSelectCoreCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Checked := False;
  AState.Enabled := DocumentModel.SearchParameters.SearchString <> '';
  AState.Visible := True;
end;

procedure TdxFindAndSelectCoreCommandBase.ChangeSelection(ASelection: TdxSelection);
var
  AResult: TdxRunInfo;
  AEvent: TdxSearchCompleteEventArgs;
begin
  if SearchParameters.SearchString = '' then
    Exit;

  while True do
  begin
    AResult := FindNextPosition;
    if AResult <> nil then
    try
      ASelection.Start := AResult.Start.LogPosition;
      ASelection.&End := AResult.&End.LogPosition;
      Break;
    finally
      AResult.Free
    end
    else
    begin
      AEvent := SearchContext.CreateEventArgs(SearchParameters.SearchString, SearchParameters.ReplaceString);
      try
        InnerControl.DoSearchComplete(AEvent);
        if not AEvent.EntireDocument and AEvent.Continue then
          Continue
        else
          Break;
      finally
        AEvent.Free;
      end;
    end;
  end;
  inherited ChangeSelection(ASelection);
end;

function TdxFindAndSelectCoreCommandBase.FindNextPosition: TdxRunInfo;
var
  ASearchProvider: TdxTextSearchProvider;
begin
  ASearchProvider := CreateTextSearchProvider;
  try
    Result := ASearchProvider.FindNextPosition;
  finally
    ASearchProvider.Free;
  end;
end;

function TdxFindAndSelectCoreCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

function TdxFindAndSelectCoreCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

{ TdxFindAndSelectForwardCoreCommand }

function TdxFindAndSelectForwardCoreCommand.CreateTextSearchProvider: TdxTextSearchProvider;
begin
  Result := TdxTextSearchForwardProvider.Create(ActivePieceTable, SearchParameters, SearchContext);
end;

{ TdxFindAndSelectBackwardCoreCommand }

function TdxFindAndSelectBackwardCoreCommand.CreateTextSearchProvider: TdxTextSearchProvider;
begin
  Result := TdxTextSearchBackwardProvider.Create(ActivePieceTable, SearchParameters, SearchContext);
end;

{ TdxReplaceCommandBase }

function TdxReplaceCommandBase.GetAction: TdxSearchAction;
begin
  Result := TdxSearchAction.Replace;
end;

procedure TdxReplaceCommandBase.PopulateCommands(ACommands: TdxCommandCollection);
begin
  ACommands.Add(TdxReplaceCoreCommand.Create(RichEditControl));
  ACommands.Add(CreateFindCommand);
end;

{ TdxReplaceForwardCommand }

function TdxReplaceForwardCommand.CreateFindCommand: TdxFindAndSelectCoreCommandBase;
begin
  Result := TdxFindAndSelectForwardCoreCommand.Create(RichEditControl);
end;

class function TdxReplaceForwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceForwardDescription);
end;

function TdxReplaceForwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Forward;
end;

class function TdxReplaceForwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceForwardMenuCaption);
end;

class function TdxReplaceForwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ReplaceForward;
end;

{ TdxReplaceBackwardCommand }

function TdxReplaceBackwardCommand.CreateFindCommand: TdxFindAndSelectCoreCommandBase;
begin
  Result := TdxFindAndSelectBackwardCoreCommand.Create(RichEditControl);
end;

class function TdxReplaceBackwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceBackwardDescription);
end;

function TdxReplaceBackwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Backward;
end;

class function TdxReplaceBackwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceBackwardMenuCaption);
end;

{ TdxReplaceCoreCommand }

function TdxReplaceCoreCommand.CanReplaceSelectedText: Boolean;
begin
  if (DocumentModel.Selection.Length = 0) or (SearchParameters.SearchString = '') then
    Exit(False);

  if not CanEditSelection then
    Exit(False);

  if not SearchContext.StartOfSearch then
    Result := True
  else
    Result := (not SearchParameters.UseRegularExpression and CompareStringWithSelectedText(SearchParameters.SearchString));
end;

function TdxReplaceCoreCommand.CompareStringWithSelectedText(const AText: string): Boolean;
var
  ASelection: TdxRunInfo;
  ALength, AOffset: Integer;
  AIterator: TdxBufferedDocumentCharacterIteratorForward;
begin
  ASelection := DocumentModel.Selection.Interval;
  ALength := Length(AText);
  AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(ASelection.NormalizedStart^);
  try
    AIterator.AppendBuffer(ALength);
    if (not AIterator.IsCharacterExist(ALength - 1)) or (AIterator.GetPosition(ALength) <> ASelection.NormalizedEnd^) then
      Exit(False);

    for AOffset := 0 to ALength - 1 do
      if not AIterator.Compare(AOffset, AText[AOffset + 1], True) then
        Exit(False);
    Result := True;
  finally
    AIterator.Free;
  end;
end;

procedure TdxReplaceCoreCommand.ExecuteCore;
var
  ASelection: TdxSelection;
begin
  CheckExecutedAtUIThread;

  ASelection := DocumentModel.Selection;
  ReplaceText(ASelection.NormalizedStart, ASelection.Length);
  ActiveView.EnsureCaretVisible;
end;

class function TdxReplaceCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxReplaceCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

function TdxReplaceCoreCommand.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

function TdxReplaceCoreCommand.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

procedure TdxReplaceCoreCommand.ReplaceText(AStartPos: TdxDocumentLogPosition; ALength: Integer);
var
  AReplacement: string;
begin
  AReplacement := SearchParameters.ReplaceString;
  if SearchParameters.UseRegularExpression and (SearchContext.MatchInfo <> nil) then
    AReplacement := SearchContext.MatchInfo.Match.Result(AReplacement);
  ActivePieceTable.ReplaceText(AStartPos, ALength, AReplacement);
  SearchContext.ReplaceCount := SearchContext.ReplaceCount + 1;
end;

procedure TdxReplaceCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable and CanReplaceSelectedText;
  AState.Visible := True;
end;

{ TdxReplaceAllCommandBase }

function TdxReplaceAllCommandBase.GetAction: TdxSearchAction;
begin
  Result := TdxSearchAction.ReplaceAll;
end;

procedure TdxReplaceAllCommandBase.PopulateCommands(ACommands: TdxCommandCollection);
begin
  ACommands.Add(TdxBeginReplaceAllCoreCommand.Create(RichEditControl));
  ACommands.Add(CreateReplaceAllCommand);
end;

{ TdxReplaceAllForwardCommand }

function TdxReplaceAllForwardCommand.CreateReplaceAllCommand: TdxReplaceAllCoreCommand;
begin
  Result := TdxReplaceAllForwardCoreCommand.Create(RichEditControl);
end;

class function TdxReplaceAllForwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceAllForwardDescription);
end;

function TdxReplaceAllForwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Forward;
end;

class function TdxReplaceAllForwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceAllForwardDescription);
end;

class function TdxReplaceAllForwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ReplaceAllForward;
end;

{ TdxReplaceAllBackwardCommand }

function TdxReplaceAllBackwardCommand.CreateReplaceAllCommand: TdxReplaceAllCoreCommand;
begin
  Result := TdxReplaceAllBackwardCoreCommand.Create(RichEditControl);
end;

class function TdxReplaceAllBackwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceAllBackwardDescription);
end;

function TdxReplaceAllBackwardCommand.GetDirection: TdxDirection;
begin
  Result := TdxDirection.Backward;
end;

class function TdxReplaceAllBackwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceAllBackwardMenuCaption);
end;

{ TdxBeginReplaceAllCoreCommand }

function TdxBeginReplaceAllCoreCommand.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

class function TdxBeginReplaceAllCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxBeginReplaceAllCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxBeginReplaceAllCoreCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := True;
end;

function TdxBeginReplaceAllCoreCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxBeginReplaceAllCoreCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxBeginReplaceAllCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxBeginReplaceAllCoreCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxBeginReplaceAllCoreCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxBeginReplaceAllCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := SearchContext.StartOfSearch;
  AState.Visible := True;
end;

procedure TdxBeginReplaceAllCoreCommand.ChangeSelection(ASelection: TdxSelection);
begin
  ASelection.Start := ASelection.&End;
  inherited ChangeSelection(ASelection);
end;

{ TdxReplaceAllCoreCommand }

constructor TdxReplaceAllCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInnerReplaceCommand := CreateReplaceCommand;
end;

destructor TdxReplaceAllCoreCommand.Destroy;
begin
  FreeAndNil(FInnerReplaceCommand);
  inherited Destroy;
end;

function TdxReplaceAllCoreCommand.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

function TdxReplaceAllCoreCommand.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

function TdxReplaceAllCoreCommand.CanEdit(AStart: TdxDocumentLogPosition; ALength: Integer): Boolean;
begin
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit(True);

  Result := ActivePieceTable.CanEditRangeLength(AStart, ALength);
end;

function TdxReplaceAllCoreCommand.CreateReplaceCommand: TdxReplaceCoreCommand;
begin
  Result := TdxReplaceCoreCommand.Create(RichEditControl);
end;

procedure TdxReplaceAllCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

procedure TdxReplaceAllCoreCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  AEvent: TdxSearchCompleteEventArgs;
begin
  if SearchParameters.SearchString = '' then
    Exit;

  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    while True do
    begin
      DoReplaceAll;
      AEvent := SearchContext.CreateEventArgs(SearchParameters.SearchString, SearchParameters.ReplaceString);
      try
        InnerControl.DoSearchComplete(AEvent);
        if (not AEvent.Continue) or (AEvent.EntireDocument) then
          Break;
      finally
        AEvent.Free;
      end;
    end;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxReplaceAllCoreCommand.OnAfterExecute;
var
  AEvent: TdxSearchCompleteEventArgs;
begin
  AEvent := SearchContext.CreateEventArgs(SearchParameters.SearchString, SearchParameters.ReplaceString);
  try
    InnerControl.DoSearchComplete(AEvent);
  finally
    AEvent.Free;
  end;
end;

procedure TdxReplaceAllCoreCommand.DoReplaceAll;
var
  ASearchProvider: TdxTextSearchProvider;
  AResult: TdxRunInfo;
  AStart: TdxDocumentLogPosition;
  ALength: Integer;
begin
  RichEditControl.BeginUpdate;
  try
    DocumentModel.BeginUpdate;
    try
      ASearchProvider := CreateTextSearchProvider;
      try
        while True do
        begin
          AResult := ASearchProvider.FindNextPosition;
          if AResult = nil then
            Break;
          try
            if AResult.Start = AResult.&End then
              Continue;
            AStart := AResult.Start.LogPosition;
            ALength := AResult.&End.LogPosition - AStart;
            if CanEdit(AStart, ALength) then
              InnerReplaceCommand.ReplaceText(AStart, ALength);
          finally
            AResult.Free;
          end;
        end;
      finally
        ASearchProvider.Free;
      end;
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

{ TdxReplaceAllForwardCoreCommand }

function TdxReplaceAllForwardCoreCommand.CreateTextSearchProvider: TdxTextSearchProvider;
begin
  Result := TdxTextSearchForwardProvider.Create(ActivePieceTable, SearchParameters, SearchContext);
end;

class function TdxReplaceAllForwardCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxReplaceAllForwardCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxReplaceAllForwardCoreCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ReplaceAllForward;
end;

{ TdxReplaceAllBackwardCoreCommand }

function TdxReplaceAllBackwardCoreCommand.CreateTextSearchProvider: TdxTextSearchProvider;
begin
  Result := TdxTextSearchBackwardProvider.Create(ActivePieceTable, SearchParameters, SearchContext);
end;

class function TdxReplaceAllBackwardCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxReplaceAllBackwardCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

{ TdxFindPrevNextCommand }

function TdxFindPrevNextCommand.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

procedure TdxFindPrevNextCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := SearchParameters.SearchString <> '';
  AState.Visible := True;
end;

{ TdxFindNextCommand }

procedure TdxFindNextCommand.ExecuteCore;
var
  ASearchHelper: TdxSearchHelper;
begin
  ASearchHelper := TdxSearchHelper.Create(RichEditControl);
  try
    ASearchHelper.Direction := TdxTextSearchDirection.Down;
    ASearchHelper.ExecuteFindCommand;
  finally
    ASearchHelper.Free;
  end;
end;

class function TdxFindNextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindNextDescription);
end;

class function TdxFindNextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindNextMenuCaption);
end;

class function TdxFindNextCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FindNext;
end;

{ TdxFindPrevCommand }

procedure TdxFindPrevCommand.ExecuteCore;
var
  ASearchHelper: TdxSearchHelper;
begin
  ASearchHelper := TdxSearchHelper.Create(RichEditControl);
  try
    ASearchHelper.Direction := TdxTextSearchDirection.Down;
    ASearchHelper.ExecuteFindCommand;
  finally
    ASearchHelper.Free;
  end;
end;

class function TdxFindPrevCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindPrevDescription);
end;

class function TdxFindPrevCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindPrevMenuCaption);
end;

class function TdxFindPrevCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FindPrev;
end;

end.
