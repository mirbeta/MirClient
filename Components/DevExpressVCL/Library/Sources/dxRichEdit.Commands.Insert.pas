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

unit dxRichEdit.Commands.Insert;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.View.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.ChangeProperties;

type
  { TdxTransactedInsertObjectCommand }

  TdxTransactedInsertObjectCommand = class abstract(TdxTransactedMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;

    function CreateDeleteCommand: TdxCommand; virtual;
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; virtual;
    function CreateInsertObjectCommand: TdxRichEditCommand; virtual;
    procedure CreateInsertObjectCommands; virtual;
    function GetInsertObjectCommand: TdxRichEditCommand; virtual;
    function GetKeepLastParagraphMarkInSelection: Boolean; virtual;

    property InsertObjectCommand: TdxRichEditCommand read GetInsertObjectCommand;
    property KeepLastParagraphMarkInSelection: Boolean read GetKeepLastParagraphMarkInSelection;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    function GetObjectDescription: string; override;
    function GetObjectMenuCaption: string; override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertParagraphCommand }

  TdxInsertParagraphCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTextCommand }

  TdxInsertTextCommand = class(TdxTransactedInsertObjectCommand)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
    property Text: string read GetText write SetText;
  end;

  { TdxInsertObjectCommandBase }

  TdxInsertObjectCommandBase = class abstract(TdxRichEditSelectionCommand)
  protected
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    procedure ModifyModel; virtual; abstract;
  public
    procedure PerformModifyModel; override;
  end;

  { TdxInsertParagraphCoreCommand }

  TdxInsertParagraphCoreCommand = class(TdxInsertObjectCommandBase)
  strict private
    FOldInputPosition: TdxInputPosition;
  private
    function CalculateTableCellToAdjustIndex(AParagraph: TdxParagraph; APos: TdxDocumentLogPosition): TdxTableCell;
  protected
    procedure AfterUpdate; override;
    procedure BeforeUpdate; override;
    function GetAllowAutoCorrect: Boolean; virtual;
    procedure ModifyModel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property AllowAutoCorrect: Boolean read GetAllowAutoCorrect;
  public
    destructor Destroy; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertParagraphIntoNonEmptyParagraphCoreCommand }

  TdxInsertParagraphIntoNonEmptyParagraphCoreCommand = class(TdxInsertParagraphCoreCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

    { TdxInsertTextCoreBaseCommand }

  TdxInsertTextCoreBaseCommand = class abstract(TdxInsertObjectCommandBase)
  protected
    procedure ModifyModel; override;
    function AllowAutoCorrect: Boolean; virtual;
    function GetInsertedText: string; virtual; abstract;
    procedure InsertTextCore; virtual;
    function ResetMerging: Boolean; virtual;
    procedure OnTextInserted(APos: TdxDocumentLogPosition; ALength: Integer); virtual;
  end;

  { TdxInsertTextCoreCommand }

  TdxInsertTextCoreCommand = class(TdxInsertTextCoreBaseCommand)
  private
    FText: string;
  protected
    function GetInsertedText: string; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const AText: string); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Text: string read FText write FText;
  end;

  { TdxInsertLineBreakCommand }

  TdxInsertLineBreakCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertBreakCommand }

  TdxInsertBreakCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertSectionBreakCoreCommand }

  TdxInsertSectionBreakCoreCommand = class abstract(TdxInsertParagraphCoreCommand)
  protected
    function GetStartType: TdxSectionStartType; virtual; abstract;
    procedure ModifyModel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property StartType: TdxSectionStartType read GetStartType;
  end;

  { TdxInsertSectionBreakNextPageCoreCommand }

  TdxInsertSectionBreakNextPageCoreCommand = class(TdxInsertSectionBreakCoreCommand)
  protected
    function GetStartType: TdxSectionStartType; override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertSectionBreakNextPageCommand }

  TdxInsertSectionBreakNextPageCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertSectionBreakOddPageCoreCommand }

  TdxInsertSectionBreakOddPageCoreCommand = class(TdxInsertSectionBreakCoreCommand)
  protected
    function GetStartType: TdxSectionStartType; override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertSectionBreakOddPageCommand }

  TdxInsertSectionBreakOddPageCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertSectionBreakEvenPageCoreCommand }

  TdxInsertSectionBreakEvenPageCoreCommand = class(TdxInsertSectionBreakCoreCommand)
  protected
    function GetStartType: TdxSectionStartType; override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertSectionBreakEvenPageCommand }

  TdxInsertSectionBreakEvenPageCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertSectionBreakContinuousCoreCommand }

  TdxInsertSectionBreakContinuousCoreCommand = class(TdxInsertSectionBreakCoreCommand)
  protected
    function GetStartType: TdxSectionStartType; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertSectionBreakContinuousCommand }

  TdxInsertSectionBreakContinuousCommand = class(TdxTransactedInsertObjectCommand)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertSpecialCharacterCommandBase }

  TdxInsertSpecialCharacterCommandBase = class abstract(TdxInsertTextCoreBaseCommand)
  protected
    function GetInsertedText: string; override;
    function GetCharacter: Char; virtual; abstract;
    property Character: Char read GetCharacter;
  end;

  { TdxInsertLineBreakCoreCommand }

  TdxInsertLineBreakCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertPageBreakCommand }

  TdxInsertPageBreakCommand = class(TdxTransactedInsertObjectCommand)
  protected
    procedure CreateInsertObjectCommands; override;
    function GetInsertObjectCommand: TdxRichEditCommand; override;
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertPageBreakCoreCommand }

  TdxInsertPageBreakCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
    function ResetMerging: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertNonBreakingSpaceCoreCommand }

  TdxInsertNonBreakingSpaceCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertNonBreakingSpaceCommand }

  TdxInsertNonBreakingSpaceCommand = class(TdxTransactedInsertObjectCommand)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertEnDashCoreCommand }

  TdxInsertEnDashCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertEnDashCommand }

  TdxInsertEnDashCommand = class(TdxTransactedInsertObjectCommand)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertEmDashCoreCommand }

  TdxInsertEmDashCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertEmDashCommand }

  TdxInsertEmDashCommand = class(TdxTransactedInsertObjectCommand)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertColumnBreakCoreCommand }

  TdxInsertColumnBreakCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
    function ResetMerging: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertColumnBreakCommand }

  TdxInsertColumnBreakCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function GetInsertObjectCommand: TdxRichEditCommand; override;
    procedure CreateInsertObjectCommands; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxOvertypeTextCommand }

  TdxOvertypeTextCommand = class(TdxInsertTextCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxOvertypeTextCoreCommand }

  TdxOvertypeTextCoreCommand = class(TdxInsertTextCoreCommand)
  private
    function CanDeleteRun(ARun: TdxTextRunBase): Boolean;
    function CalculateDeletedLength(AFirstDeletePos: TdxDocumentLogPosition; ALength: Integer): Integer;
  protected
    function CanEditSelection: Boolean; override;
    procedure OnTextInserted(APos: Integer; ALength: Integer); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTabToParagraphCommand }

  TdxInsertTabToParagraphCommand = class(TdxSelectionBasedPropertyChangeCommandBase)
  strict private
    FTab: TdxTabInfo;
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    function CalculateStartPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition; override;
    function CalculateEndPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const ATab: TdxTabInfo); reintroduce;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertDialogCommand }
  TdxInsertDialogCommand = class(TdxTransactedInsertObjectCommand)
  protected
    function ExecuteDialog(const AState: IdxCommandUIState): Boolean; virtual; abstract;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertPictureCommand }

  TdxInsertPictureCommand = class(TdxInsertDialogCommand)
  strict private
    FImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>;
  protected
    function ExecuteDialog(const AState: IdxCommandUIState): Boolean; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;

    property ImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference> read FImportSource
      write FImportSource;
  public
    class function Id: TdxRichEditCommandId; override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertPictureCoreCommand }

  TdxInsertPictureCoreCommand = class(TdxInsertObjectCommandBase)
  strict private
    FImage: TdxOfficeImageReference;
    FImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>;
    procedure SetImportSource(const Value: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>);
  protected
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure ModifyModel; override;

    function CalculateImageScale(AImage: TdxOfficeImageReference): Integer; overload;
    function EnsureImageLoaded: Boolean; virtual;

    property ImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference> read FImportSource write SetImportSource;
  public
    destructor Destroy; override;
    class function CalculateImageScale(AImage: TdxOfficeImageReference; const APos: TdxDocumentLayoutPosition): Integer; overload; static;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { IdxInsertSymbolCommandUIState }

  IdxInsertSymbolCommandUIState = interface(IdxValueBasedCommandUIState<TdxSymbolProperties>)
  ['{1FD259EE-E981-4B66-93AA-D03A4BE384F0}']
  end;

  { TdxInsertSymbolCommandUIState }

  TdxInsertSymbolCommandUIState = class(TdxDefaultCommandUIState, IdxInsertSymbolCommandUIState, IdxCommandUIState)
  private
    FValue: TdxSymbolProperties;
    //IdxInsertSymbolCommandUIState
    function GetValue: TdxSymbolProperties;
    procedure SetValue(const Value: TdxSymbolProperties);
  public
    property Value: TdxSymbolProperties read FValue write FValue;
  end;

  { TdxInsertSymbolCoreCommand }

  TdxInsertSymbolCoreCommand = class(TdxInsertTextCoreBaseCommand)
  strict private
    FSymbolInfo: TdxSymbolProperties;
  protected
    function AllowAutoCorrect: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetInsertedText: string; override;
    procedure ModifyModel; override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertSymbolCommand }

  TdxInsertSymbolCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
  end;


implementation

uses
  Classes, Contnrs, RTLConsts, dxCore, dxCoreClasses,
  Math,
  dxTypeHelpers,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Import.DocumentImportHelper,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.View.Simple,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxCharacters,
  dxRichEdit.Utils.Exceptions.Strs;

type
  TdxChangeFontNameCommandAccess = class(TdxChangeFontNameCommand);

{ TdxTransactedInsertObjectCommand }

class function TdxTransactedInsertObjectCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  raise Exception.Create('for C++Builder');
end;

procedure TdxTransactedInsertObjectCommand.CreateCommands;
begin
  Commands.Add(CreateDeleteCommand);
  CreateInsertObjectCommands;
end;

function TdxTransactedInsertObjectCommand.CreateDeleteCommand: TdxCommand;
var
  ACommand: TdxDeleteNonEmptySelectionCommand;
begin
  if GetKeepLastParagraphMarkInSelection then
    ACommand := TdxDeleteSelectionKeepLastParagraphCommand.Create(RichEditControl)
  else
    ACommand := TdxDeleteNonEmptySelectionCommand.Create(RichEditControl);
  ACommand.RestoreInputPositionFormatting := True;
  Result := ACommand;
end;

function TdxTransactedInsertObjectCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := GetInsertObjectCommandClass.Create(RichEditControl);
end;

procedure TdxTransactedInsertObjectCommand.CreateInsertObjectCommands;
begin
  Commands.Add(CreateInsertObjectCommand);
end;

function TdxTransactedInsertObjectCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

class function TdxTransactedInsertObjectCommand.GetImageName: string;
begin
  Result := GetInsertObjectCommandClass.GetImageName;
end;

class function TdxTransactedInsertObjectCommand.GetDescription: string;
begin
  Result := GetInsertObjectCommandClass.GetDescription;
end;

class function TdxTransactedInsertObjectCommand.GetMenuCaption: string;
begin
  Result := GetInsertObjectCommandClass.GetMenuCaption;
end;

function TdxTransactedInsertObjectCommand.GetObjectDescription: string;
begin
  Result := GetInsertObjectCommand.GetObjectDescription;
end;

function TdxTransactedInsertObjectCommand.GetObjectMenuCaption: string;
begin
  Result := GetInsertObjectCommand.GetObjectMenuCaption;
end;

function TdxTransactedInsertObjectCommand.GetInsertObjectCommand: TdxRichEditCommand;
begin
  if Commands.Count > 1 then
    Result := TdxRichEditCommand(Commands[1])
  else
    Result := nil;
end;

function TdxTransactedInsertObjectCommand.GetKeepLastParagraphMarkInSelection: Boolean;
begin
  Result := True;
end;

procedure TdxTransactedInsertObjectCommand.UpdateUIState(const AState: IdxCommandUIState);
var
  ACommandState: IdxCommandUIState;
begin
  if GetInsertObjectCommand <> nil then
  begin
    ACommandState := GetInsertObjectCommand.CreateDefaultCommandUIState;
    GetInsertObjectCommand.UpdateUIState(ACommandState);
    AState.Enabled := ACommandState.Enabled;
    AState.Visible := ACommandState.Visible;
  end
  else
    inherited UpdateUIState(AState);
end;

function TdxTransactedInsertObjectCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxInsertTextCommand }

class function TdxInsertTextCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertText;
end;

function TdxInsertTextCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertTextCoreCommand.Create(RichEditControl, '');
end;

class function TdxInsertTextCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTextCoreCommand;
end;

function TdxInsertTextCommand.GetText: string;
var
  ACommand: TdxInsertTextCoreCommand;
begin
  ACommand := TdxInsertTextCoreCommand(GetInsertObjectCommand);
  Result := ACommand.Text;
end;

procedure TdxInsertTextCommand.SetText(const Value: string);
var
  ACommand: TdxInsertTextCoreCommand;
begin
  ACommand := TdxInsertTextCoreCommand(GetInsertObjectCommand);
  ACommand.Text := Value;
end;

{ TdxInsertParagraphCommand }

class function TdxInsertParagraphCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertParagraphCoreCommand;
end;

class function TdxInsertParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertParagraph;
end;

{ TdxInsertParagraphCoreCommand }

destructor TdxInsertParagraphCoreCommand.Destroy;
begin
  FreeAndNil(FOldInputPosition);
  inherited Destroy;
end;

class function TdxInsertParagraphCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertParagraphDescription);
end;

class function TdxInsertParagraphCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertParagraphMenuCaption);
end;

procedure TdxInsertParagraphCoreCommand.ModifyModel;
var
  APos, APrevPos: TdxDocumentLogPosition;
  AFilter: IdxVisibleTextFilter;
  AInputPosition: TdxInputPosition;
  AParagraph, ANewParagraph: TdxSimpleParagraph;
  AParagraphStyle, ANextParagraphStyle: TdxParagraphStyle;
  ACell: TdxTableCell;
begin
  if AllowAutoCorrect then
    ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.ApplyAutoCorrect], -1, -1);

  APos := DocumentModel.Selection.&End;
  AFilter := DocumentModel.ActivePieceTable.VisibleTextFilter;
  if (APos <> 1) and not AFilter.IsRunVisible(0) then
  begin
    APrevPos := AFilter.GetPrevVisibleLogPosition(APos, False);
    if APrevPos = 0 then
      APos := APrevPos;
  end;

  if CommandSourceType = TdxCommandSourceType.Keyboard then
    AInputPosition := CaretPosition.GetInputPosition
  else
    AInputPosition := CaretPosition.TryGetInputPosition;
  if (AInputPosition <> nil) and (DocumentModel.Selection.Length = 0) then
    AParagraph := ActivePieceTable.InsertParagraph(AInputPosition, APos, GetForceVisible)
  else
    AParagraph := ActivePieceTable.InsertParagraph(APos, GetForceVisible);

  ANewParagraph := ActivePieceTable.Paragraphs[AParagraph.Index + 1];
  if ANewParagraph.Length <= 1 then
  begin
    AParagraphStyle := AParagraph.ParagraphStyle;
    ANextParagraphStyle := AParagraphStyle.NextParagraphStyle;
    if (ANextParagraphStyle <> nil) and (ANextParagraphStyle <> AParagraphStyle) then
    begin
      ANewParagraph.ParagraphProperties.ResetAllUse;
      ANewParagraph.ParagraphStyleIndex := DocumentModel.ParagraphStyles.IndexOf(ANextParagraphStyle);
    end;
  end;

  ACell := CalculateTableCellToAdjustIndex(TdxParagraph(AParagraph), APos);
  if ACell <> nil then
  begin
    DocumentModel.Selection.ActiveSelection.Start := DocumentModel.Selection.ActiveSelection.Start - 1;
    DocumentModel.Selection.ActiveSelection.&End  := DocumentModel.Selection.ActiveSelection.&End - 1;
    TdxParagraph(AParagraph).PieceTable.ChangeCellStartParagraphIndex(ACell, AParagraph.Index + 1);
    if AParagraph.IsInList then
      AParagraph.PieceTable.RemoveNumberingFromParagraph(AParagraph);
  end;

end;

procedure TdxInsertParagraphCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Paragraphs, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

procedure TdxInsertParagraphCoreCommand.AfterUpdate;
var
  ANewInputPosition: TdxInputPosition;
begin
  inherited AfterUpdate;
  UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Character);
  ANewInputPosition := CaretPosition.GetInputPosition;
  ANewInputPosition.CopyFormattingFrom(FOldInputPosition);
end;

function TdxInsertParagraphCoreCommand.GetAllowAutoCorrect: Boolean;
begin
  Result := CommandSourceType <> TdxCommandSourceType.Unknown;
end;

procedure TdxInsertParagraphCoreCommand.BeforeUpdate;
begin
  inherited BeforeUpdate;
  FOldInputPosition := CaretPosition.GetInputPosition.Clone;
end;

function TdxInsertParagraphCoreCommand.CalculateTableCellToAdjustIndex(
  AParagraph: TdxParagraph; APos: TdxDocumentLogPosition): TdxTableCell;
var
  AParentCell, APrevParagraphCell: TdxTableCell;
  APrevParagraph: TdxParagraph;
begin
  if APos <> AParagraph.LogPosition then
    Exit(nil);

  if AParagraph.Index = 0 then
    Exit(AParagraph.GetCell);

  Result := AParagraph.GetCell;
  if Result = nil then
    Exit;

  AParentCell := Result.Table.ParentCell;
  if (AParentCell <> nil) and (AParentCell.StartParagraphIndex = Result.StartParagraphIndex) then
    Exit;

  APrevParagraph := ActivePieceTable.Paragraphs[AParagraph.Index - 1];
  APrevParagraphCell := APrevParagraph.GetCell;
  if APrevParagraphCell = nil then
    Exit(nil);

  if (APrevParagraphCell.Table <> Result.Table) and (APrevParagraphCell.Table.NestedLevel = Result.Table.NestedLevel) then
    Exit;

  Result := nil;
end;

{ TdxInsertLineBreakCommand }

class function TdxInsertLineBreakCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertLineBreakCoreCommand;
end;

class function TdxInsertLineBreakCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertLineBreak;
end;

{ TdxInsertBreakCommand }

procedure TdxInsertBreakCommand.ExecuteCore;
begin
end;

class function TdxInsertBreakCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBreakDescription);
end;

class function TdxInsertBreakCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBreakMenuCaption);
end;

class function TdxInsertBreakCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertBreak;
end;

procedure TdxInsertBreakCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Sections);
  ApplyDocumentProtectionToSelectedSections(AState);
end;

{ TdxInsertSectionBreakCoreCommand }

procedure TdxInsertSectionBreakCoreCommand.ModifyModel;
var
  ASplitTable: TdxSplitTableCommand;
  APos: TdxDocumentLogPosition;
  AParagraph: TdxParagraph;
  ACell: TdxTableCell;
  ASectionIndex: TdxSectionIndex;
begin
  ASplitTable := TdxSplitTableCommand.Create(RichEditControl);
  try
    if ASplitTable.CanExecute then
      ASplitTable.PerformTableSplitBySelectionStart;

    APos := DocumentModel.Selection.&End;
    AParagraph := ActivePieceTable.FindParagraph(APos);
    if AParagraph = nil then
      Exit;

    ACell := AParagraph.GetCell;
    if ACell <> nil then
      Exit;

    DocumentModel.InsertSection(APos);
    ASectionIndex := DocumentModel.FindSectionIndex(APos + 1);
    if ASectionIndex >= 0 then
      DocumentModel.Sections[ASectionIndex].GeneralSettings.StartType := StartType;
  finally
    ASplitTable.Free;
  end;
end;

procedure TdxInsertSectionBreakCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsMain and not IsSelectionEndInTableCell;

  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Sections, AState.Enabled);
  ApplyDocumentProtectionToSelectedSections(AState);
end;

{ TdxInsertSectionBreakNextPageCoreCommand }

class function TdxInsertSectionBreakNextPageCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertSectionBreakNextPage;
end;

class function TdxInsertSectionBreakNextPageCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakNextPageDescription);
end;

class function TdxInsertSectionBreakNextPageCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakNextPageMenuCaption);
end;

function TdxInsertSectionBreakNextPageCoreCommand.GetStartType: TdxSectionStartType;
begin
  Result := TdxSectionStartType.NextPage;
end;

{ TdxInsertSectionBreakNextPageCommand }

class function TdxInsertSectionBreakNextPageCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertSectionBreakNextPageCoreCommand;
end;

function TdxInsertSectionBreakNextPageCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertSectionBreakNextPageCoreCommand.Create(RichEditControl);
end;

class function TdxInsertSectionBreakNextPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertSectionBreakNextPage;
end;

{ TdxInsertSectionBreakOddPageCoreCommand }

class function TdxInsertSectionBreakOddPageCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertSectionBreakOddPage;
end;

class function TdxInsertSectionBreakOddPageCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakOddPageDescription);
end;

class function TdxInsertSectionBreakOddPageCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakOddPageMenuCaption);
end;

function TdxInsertSectionBreakOddPageCoreCommand.GetStartType: TdxSectionStartType;
begin
  Result := TdxSectionStartType.OddPage;
end;

{ TdxInsertSectionBreakOddPageCommand }

class function TdxInsertSectionBreakOddPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertSectionBreakOddPage;
end;

class function TdxInsertSectionBreakOddPageCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertSectionBreakOddPageCoreCommand;
end;

function TdxInsertSectionBreakOddPageCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertSectionBreakOddPageCoreCommand.Create(RichEditControl);
end;

{ TdxInsertSectionBreakEvenPageCoreCommand }

class function TdxInsertSectionBreakEvenPageCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertSectionBreakEvenPage;
end;

class function TdxInsertSectionBreakEvenPageCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakEvenPageDescription);
end;

class function TdxInsertSectionBreakEvenPageCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakEvenPageMenuCaption);
end;

function TdxInsertSectionBreakEvenPageCoreCommand.GetStartType: TdxSectionStartType;
begin
  Result := TdxSectionStartType.EvenPage;
end;

{ TdxInsertSectionBreakEvenPageCommand }

class function TdxInsertSectionBreakEvenPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertSectionBreakEvenPage;
end;

class function TdxInsertSectionBreakEvenPageCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertSectionBreakEvenPageCoreCommand;
end;

function TdxInsertSectionBreakEvenPageCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertSectionBreakEvenPageCoreCommand.Create(RichEditControl);
end;

{ TdxInsertSectionBreakContinuousCoreCommand }

class function TdxInsertSectionBreakContinuousCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakContinuousDescription);
end;

class function TdxInsertSectionBreakContinuousCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSectionBreakContinuousMenuCaption);
end;

function TdxInsertSectionBreakContinuousCoreCommand.GetStartType: TdxSectionStartType;
begin
  Result := TdxSectionStartType.Continuous;
end;

{ TdxInsertSectionBreakContinuousCommand }

class function TdxInsertSectionBreakContinuousCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertSectionBreakContinuous;
end;

function TdxInsertSectionBreakContinuousCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertSectionBreakContinuousCoreCommand.Create(RichEditControl);
end;

{ TdxInsertSpecialCharacterCommandBase }

function TdxInsertSpecialCharacterCommandBase.GetInsertedText: string;
begin
  Result := Character;
end;

{ TdxInsertLineBreakCoreCommand }

function TdxInsertLineBreakCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.LineBreak;
end;

class function TdxInsertLineBreakCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertLineBreakDescription);
end;

class function TdxInsertLineBreakCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertLineBreakMenuCaption);
end;

{ TdxInsertPageBreakCommand }

procedure TdxInsertPageBreakCommand.CreateInsertObjectCommands;
begin
  Commands.Add(TdxInsertParagraphIntoNonEmptyParagraphCoreCommand.Create(RichEditControl));
  inherited CreateInsertObjectCommands;
end;

class function TdxInsertPageBreakCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertPageBreakCoreCommand;
end;

class function TdxInsertPageBreakCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertPageBreak;
end;

function TdxInsertPageBreakCommand.GetInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxRichEditCommand(Commands[2]);
end;

procedure TdxInsertPageBreakCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    GetInsertObjectCommand.UpdateUIState(AState);
end;

{ TdxInsertPageBreakCoreCommand }

function TdxInsertPageBreakCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.PageBreak;
end;

class function TdxInsertPageBreakCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertPageBreak;
end;

class function TdxInsertPageBreakCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageBreakDescription);
end;

class function TdxInsertPageBreakCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPageBreakMenuCaption);
end;

function TdxInsertPageBreakCoreCommand.ResetMerging: Boolean;
begin
  Result := True;
end;

procedure TdxInsertPageBreakCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsMain and not IsSelectionEndInTableCell;
end;

{ TdxInsertNonBreakingSpaceCoreCommand }

class function TdxInsertNonBreakingSpaceCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertNonBreakingSpaceMenuCaption);
end;

class function TdxInsertNonBreakingSpaceCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertNonBreakingSpaceDescription);
end;

function TdxInsertNonBreakingSpaceCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.NonBreakingSpace;
end;

{ TdxInsertNonBreakingSpaceCommand }

class function TdxInsertNonBreakingSpaceCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertNonBreakingSpace;
end;

function TdxInsertNonBreakingSpaceCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertNonBreakingSpaceCoreCommand.Create(RichEditControl);
end;

{ TdxInsertEnDashCoreCommand }

function TdxInsertEnDashCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.EnDash;
end;

class function TdxInsertEnDashCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEnDashDescription);
end;

class function TdxInsertEnDashCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEnDashMenuCaption);
end;

{ TdxInsertEnDashCommand }

function TdxInsertEnDashCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertEnDashCoreCommand.Create(RichEditControl);
end;

class function TdxInsertEnDashCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertEnDash;
end;

{ TdxInsertEmDashCoreCommand }

function TdxInsertEmDashCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.EmDash;
end;

class function TdxInsertEmDashCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEmDashDescription);
end;

class function TdxInsertEmDashCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEmDashMenuCaption);
end;

{ TdxInsertEmDashCommand }

function TdxInsertEmDashCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertEmDashCoreCommand.Create(RichEditControl);
end;

class function TdxInsertEmDashCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertEmDash;
end;

{ TdxInsertColumnBreakCoreCommand }

function TdxInsertColumnBreakCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.ColumnBreak;
end;

class function TdxInsertColumnBreakCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertColumnBreak;
end;

class function TdxInsertColumnBreakCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertColumnBreakDescription);
end;

class function TdxInsertColumnBreakCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertColumnBreakMenuCaption);
end;

function TdxInsertColumnBreakCoreCommand.ResetMerging: Boolean;
begin
  Result := True;
end;

procedure TdxInsertColumnBreakCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsMain and not IsSelectionEndInTableCell;
end;

{ TdxInsertColumnBreakCommand }

class function TdxInsertColumnBreakCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertColumnBreak;
end;

class function TdxInsertColumnBreakCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertColumnBreakCoreCommand;
end;

function TdxInsertColumnBreakCommand.GetInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxRichEditCommand(Commands[2]);
end;

procedure TdxInsertColumnBreakCommand.CreateInsertObjectCommands;
begin
  Commands.Add(TdxInsertParagraphIntoNonEmptyParagraphCoreCommand.Create(RichEditControl));
  inherited CreateInsertObjectCommands;
end;

function TdxInsertColumnBreakCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxInsertColumnBreakCoreCommand.Create(RichEditControl);
end;

procedure TdxInsertColumnBreakCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    GetInsertObjectCommand.UpdateUIState(AState);
end;

{ TdxInsertParagraphIntoNonEmptyParagraphCoreCommand }

procedure TdxInsertParagraphIntoNonEmptyParagraphCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AParagraphIndex: TdxParagraphIndex;
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
  begin
    AParagraphIndex := CaretPosition.GetInputPosition.ParagraphIndex;
    AState.Enabled := ActivePieceTable.Paragraphs[AParagraphIndex].Length > 1;
  end;
end;

{ TdxInsertObjectCommandBase }

function TdxInsertObjectCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := IsContentEditable and CanEditSelection;
end;

function TdxInsertObjectCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

function TdxInsertObjectCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxInsertObjectCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxInsertObjectCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxInsertObjectCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxInsertObjectCommandBase.PerformModifyModel;
begin
  DocumentModel.BeginUpdate;
  try
    ModifyModel;
  finally
    DocumentModel.EndUpdate;
  end;
  ActiveView.EnforceFormattingCompleteForVisibleArea;
end;

{ TdxInsertTextCoreBaseCommand }

function TdxInsertTextCoreBaseCommand.AllowAutoCorrect: Boolean;
begin
  Result := CommandSourceType <> TdxCommandSourceType.Unknown;
end;

procedure TdxInsertTextCoreBaseCommand.InsertTextCore;
var
  AInsertedText: string;
  AForceVisible: Boolean;
  APos: TdxInputPosition;
  AInsertPosition: TdxDocumentLogPosition;
begin
  AInsertedText := GetInsertedText;
  if AInsertedText = '' then
    Exit;
  AForceVisible := GetForceVisible;
  if ResetMerging then
    DocumentModel.ResetMerging;
  if DocumentModel.Selection.Length > 0 then
  begin
    AInsertPosition := DocumentModel.Selection.&End;
    ActivePieceTable.InsertText(DocumentModel.Selection.&End, AInsertedText, AForceVisible);
    OnTextInserted(AInsertPosition, Length(AInsertedText));
    Exit;
  end;
  if CommandSourceType = TdxCommandSourceType.Keyboard then
    APos := CaretPosition.GetInputPosition
  else
    APos := CaretPosition.TryGetInputPosition;
  if APos <> nil then
  begin
    AInsertPosition := APos.LogPosition;
    ActivePieceTable.InsertText(APos, AInsertedText, AForceVisible);
    OnTextInserted(AInsertPosition, Length(AInsertedText));
  end
  else
  begin
    AInsertPosition := DocumentModel.Selection.&End;
    ActivePieceTable.InsertText(DocumentModel.Selection.&End, AInsertedText, AForceVisible);
    OnTextInserted(AInsertPosition, Length(AInsertedText));
  end;
  if ResetMerging then
    DocumentModel.ResetMerging;
end;

procedure TdxInsertTextCoreBaseCommand.ModifyModel;
begin
  if not ActivePieceTable.Runs[DocumentModel.Selection.Interval.&End.RunIndex].CanPlaceCaretBefore then
    Exit;
  if AllowAutoCorrect then
    ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.ApplyAutoCorrect], -1, -1);
  InsertTextCore;
end;

function TdxInsertTextCoreBaseCommand.ResetMerging: Boolean;
begin
  Result := False;
end;

procedure TdxInsertTextCoreBaseCommand.OnTextInserted(APos: TdxDocumentLogPosition; ALength: Integer);
begin
end;

{ TdxInsertTextCoreCommand }

constructor TdxInsertTextCoreCommand.Create(const AControl: IdxRichEditControl; const AText: string);
begin
  inherited Create(AControl);
  FText := AText;
end;

class function TdxInsertTextCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTextDescription);
end;

function TdxInsertTextCoreCommand.GetInsertedText: string;
begin
  Result := Text;
end;

class function TdxInsertTextCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTextMenuCaption);
end;

procedure TdxInsertTextCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := IsContentEditable and (Text <> '');
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxOvertypeTextCommand }

class function TdxOvertypeTextCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.OvertypeText;
end;

function TdxOvertypeTextCommand.CreateInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxOvertypeTextCoreCommand.Create(RichEditControl, '');
end;

class function TdxOvertypeTextCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxOvertypeTextCoreCommand;
end;

{ TdxOvertypeTextCoreCommand }

function TdxOvertypeTextCoreCommand.CalculateDeletedLength(
  AFirstDeletePos: TdxDocumentLogPosition; ALength: Integer): Integer;
var
  AStartPos: TdxDocumentModelPosition;
  ATotalLength: Integer;
  ARunIndex, AMaxRunIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  AStartPos := TdxPositionConverter.ToDocumentModelPosition(ActivePieceTable, AFirstDeletePos);
  ATotalLength := -AStartPos.RunOffset;
  ARunIndex := AStartPos.RunIndex;
  AMaxRunIndex := ActivePieceTable.Runs.Count - 1;
  while (ARunIndex <= AMaxRunIndex) and (ATotalLength < ALength) do
  begin
    ARun := ActivePieceTable.Runs[ARunIndex];
    if not CanDeleteRun(ARun) then
      Exit(ATotalLength);
    ATotalLength := ATotalLength + ARun.Length;
    Inc(ARunIndex);
  end;
  Result := Min(ATotalLength, ALength);
end;

function TdxOvertypeTextCoreCommand.CanDeleteRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := (ARun is TdxInlineObjectRun) or (ARun is TdxTextRun);
end;

function TdxOvertypeTextCoreCommand.CanEditSelection: Boolean;
var
  AStart: TdxDocumentLogPosition;
  ALength, ADeletedLength: Integer;
begin
  if not inherited CanEditSelection then
    Exit(False);
  AStart := DocumentModel.Selection.&End;
  ALength := Length(GetInsertedText);
  ADeletedLength := CalculateDeletedLength(AStart, ALength);
  Result := DocumentModel.ActivePieceTable.CanEditRangeLength(AStart, ADeletedLength);
end;

class function TdxOvertypeTextCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOvertypeTextDescription);
end;

class function TdxOvertypeTextCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOvertypeTextMenuCaption);
end;

procedure TdxOvertypeTextCoreCommand.OnTextInserted(APos, ALength: Integer);
var
  ADeleteStart: TdxDocumentLogPosition;
  ADeleteLength: Integer;
begin
  inherited OnTextInserted(APos, ALength);
  ADeleteStart := APos + ALength;
  ADeleteLength := CalculateDeletedLength(ADeleteStart, ALength);
  if ADeleteLength > 0 then
    ActivePieceTable.DeleteContent(APos + ALength, ADeleteLength, False, False, False, True, False);
end;

{ TdxInsertTabToParagraphCommand }

constructor TdxInsertTabToParagraphCommand.Create(const AControl: IdxRichEditControl; const ATab: TdxTabInfo);
begin
  inherited Create(AControl);
  FTab := ATab;
end;

class function TdxInsertTabToParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabToParagraphMenuCaption);
end;

class function TdxInsertTabToParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabToParagraphDescription);
end;

function TdxInsertTabToParagraphCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AParagraphs: TdxParagraphCollection;
  AFrom, ATo, I: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  ATabs: TdxTabFormattingInfo;
begin
  AParagraphs := ActivePieceTable.Paragraphs;
  AFrom := AStart.ParagraphIndex;
  ATo := AEnd.ParagraphIndex;
  for I := AFrom to ATo do
  begin
    AParagraph := AParagraphs[I];
    ATabs := AParagraph.Tabs.GetTabs;
    try
      ATabs.Add(FTab);
      AParagraph.Tabs.SetTabs(ATabs);
    finally
      ATabs.Free;
    end;
  end;
  Result := [];
end;

function TdxInsertTabToParagraphCommand.CalculateStartPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.FromParagraphStart(AItem.PieceTable, AItem.GetStartParagraphIndex);
end;

function TdxInsertTabToParagraphCommand.CalculateEndPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.FromParagraphEnd(AItem.PieceTable, AItem.GetEndParagraphIndex);
end;

procedure TdxInsertTabToParagraphCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphTabs);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxInsertDialogCommand }

procedure TdxInsertDialogCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  if ExecuteDialog(AState) then
    inherited ForceExecute(AState);
end;

{ TdxInsertPictureCommand }

class function TdxInsertPictureCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertPictureCoreCommand;
end;

function TdxInsertPictureCommand.ExecuteDialog(
  const AState: IdxCommandUIState): Boolean;
var
  AImportManagerService: IdxImportManagerService<TdxOfficeImageFormat, TdxOfficeImageReference>;
  AImportHelper: TdxRichEditImageImportHelper;
begin
  AImportManagerService := TdxPictureFormatsManagerService.Create;
  AImportHelper := TdxRichEditImageImportHelper.Create(DocumentModel);
  try
    FImportSource := AImportHelper.InvokeImportDialog(Control, AImportManagerService);
    Result := FImportSource <> nil;
  finally
    AImportHelper.Free;
  end;
end;

procedure TdxInsertPictureCommand.ForceExecuteCore(
  const AState: IdxCommandUIState);
var
  AInsertCommand: TdxInsertPictureCoreCommand;
begin
  AInsertCommand := TdxInsertPictureCoreCommand(Commands[1]);
  AInsertCommand.ImportSource := FImportSource;
  try
    if AInsertCommand.EnsureImageLoaded then
      inherited ForceExecuteCore(AState)
    else
      RichEditControl.ShowErrorMessage(cxGetResourceString(@sdxRichEditExceptionInvalidImageFile));
  finally
    FImportSource := nil;
  end;
end;

class function TdxInsertPictureCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertPicture;
end;

procedure TdxInsertPictureCommand.UpdateUIState(
  const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.InlinePictures, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxInsertPictureCoreCommand }

destructor TdxInsertPictureCoreCommand.Destroy;
begin
  ImportSource := nil;
  FreeAndNiL(FImage);
  inherited Destroy;
end;

function TdxInsertPictureCoreCommand.CalculateImageScale(
  AImage: TdxOfficeImageReference): Integer;
begin
  if ActiveView is TdxSimpleView then
    Result := 100
  else
    Result := CalculateImageScale(AImage, CaretPosition.LayoutPosition);
end;

class function TdxInsertPictureCoreCommand.CalculateImageScale(
  AImage: TdxOfficeImageReference;
  const APos: TdxDocumentLayoutPosition): Integer;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AColumnBounds: TRect;
  AImageSize: TSize;
  AScale, AScaleX, AScaleY: Integer;
begin
  if not APos.IsValid(TdxDocumentLayoutDetailsLevel.Column) then
    Exit(100);
  AUnitConverter := APos.DocumentModel.ToDocumentLayoutUnitConverter;
  AColumnBounds := APos.Column.Bounds;
  if not APos.PieceTable.IsMain then
    AColumnBounds.Height := Max(AColumnBounds.Height, APos.Page.ClientBounds.Height div 3);
  AImageSize := AImage.Image.SizeInTwips;
  AScaleX := 100 * AUnitConverter.ToModelUnits(AColumnBounds.Width) div Max(1, AImageSize.cx);
  AScaleY := 100 * AUnitConverter.ToModelUnits(AColumnBounds.Height) div Max(1, AImageSize.cy);
  AScale := Min(AScaleX, AScaleY);
  Result := Max(1, Min(AScale, 100));
end;

class function TdxInsertPictureCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertPicture;
end;

function TdxInsertPictureCoreCommand.EnsureImageLoaded: Boolean;
var
  AStream: TStream;
  AOptions: TObject;
  AImporterOptions: IdxImporterOptions;
begin
  if FImage <> nil then
    Exit(True);
  try
    AOptions := ImportSource.Importer.SetupLoading;
    try
      Supports(AOptions, IdxImporterOptions, AImporterOptions);
      AStream := ImportSource.GetStream;
      try
        FImage := ImportSource.Importer.LoadDocument(DocumentModel, AStream, AImporterOptions);
        FImage.Uri := ImportSource.FileName;
      finally
        AStream.Free;
      end;
    finally
      AImporterOptions := nil;
      FreeAndNil(AOptions);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

class function TdxInsertPictureCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPictureDescription);
end;

class function TdxInsertPictureCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertPictureMenuCaption);
end;

function TdxInsertPictureCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Column;
end;

procedure TdxInsertPictureCoreCommand.ModifyModel;
var
  AScale: Integer;
begin
  if FImportSource = nil then
    Exit;
  if not EnsureImageLoaded then
    Exit;
  AScale := CalculateImageScale(FImage);
  ActivePieceTable.InsertInlinePicture(DocumentModel.Selection.&End, FImage, AScale, AScale, GetForceVisible);
end;

procedure TdxInsertPictureCoreCommand.SetImportSource(
  const Value: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>);
begin
  FreeAndNil(FImportSource);
  FImportSource := Value;
end;

{ TdxInsertSymbolCoreCommand }

class function TdxInsertSymbolCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSymbolDescription);
end;

class function TdxInsertSymbolCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSymbolMenuCaption);
end;

function TdxInsertSymbolCoreCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxInsertSymbolCommandUIState.Create;
end;

procedure TdxInsertSymbolCoreCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueState: TdxInsertSymbolCommandUIState;
begin
  AValueState := AState as TdxInsertSymbolCommandUIState;
  if AValueState = nil then
    Exit;

  FSymbolInfo := AValueState.Value;
  inherited ForceExecute(AState);
end;

function TdxInsertSymbolCoreCommand.AllowAutoCorrect: Boolean;
begin
  Result := False;
end;

function TdxInsertSymbolCoreCommand.GetInsertedText: string;
begin
  Result := FSymbolInfo.UnicodeChar;
end;

procedure TdxInsertSymbolCoreCommand.ModifyModel;
var
  ASelection: TdxSelection;
  ASelectionEnd: TdxDocumentLogPosition;
  AUsePreviousBoxBounds: Boolean;
  ACommand: TdxChangeFontNameCommand;
  AState: TdxDefaultValueBasedCommandUIState<string>;
begin
  ASelection := DocumentModel.Selection;
  ASelectionEnd := ASelection.&End;

  inherited ModifyModel;

  if ASelectionEnd = ASelection.&End then
    Exit;

  if FSymbolInfo.FontName = '' then
    Exit;

  ASelectionEnd := ASelection.&End;
  AUsePreviousBoxBounds := ASelection.UsePreviousBoxBounds;
  ASelection.Start := ASelectionEnd - 1;

  ACommand := TdxChangeFontNameCommand.Create(RichEditControl);
  try
    if ACommand.CanExecute then
    begin
      AState := TdxDefaultValueBasedCommandUIState<string>.Create;
      AState.Value := FSymbolInfo.FontName;
      TdxChangeFontNameCommandAccess(ACommand).ModifyDocumentModelCore(AState);
    end;
  finally
    ACommand.Free;
  end;

  ASelection.BeginUpdate;
  try
    ASelection.Start := ASelectionEnd;
    ASelection.&End := ASelectionEnd;
    ASelection.UsePreviousBoxBounds := AUsePreviousBoxBounds;
  finally
    ASelection.EndUpdate;
  end;
end;

procedure TdxInsertSymbolCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueState: TdxInsertSymbolCommandUIState;
begin
  AValueState := AState as TdxInsertSymbolCommandUIState;
  if AValueState = nil then
    AState.Enabled := False
  else
    inherited UpdateUIStateCore(AState);
end;

{ TdxInsertSymbolCommand }

class function TdxInsertSymbolCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertSymbolCoreCommand;
end;

function TdxInsertSymbolCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxInsertSymbolCommandUIState.Create;
end;

{ TdxInsertSymbolCommandUIState }

function TdxInsertSymbolCommandUIState.GetValue: TdxSymbolProperties;
begin
  Result := FValue;
end;

procedure TdxInsertSymbolCommandUIState.SetValue(const Value: TdxSymbolProperties);
begin
  FValue := Value;
end;

end.
