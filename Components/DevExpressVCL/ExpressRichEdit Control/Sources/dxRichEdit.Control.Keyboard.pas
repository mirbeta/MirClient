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

unit dxRichEdit.Control.Keyboard;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, Controls,
  dxCoreClasses,

  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.View.Core,
  dxRichEdit.InnerControl,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert;

type
  TdxRichEditKeyboardController = class;

  { TdxRichEditKeyboardCustomHandler }

  TdxRichEditKeyboardCustomHandler = class(TInterfacedObject, IdxKeyboardHandlerService)
  strict private
    FController: TdxRichEditKeyboardController;
    FKeyHandlerIdTable: TDictionary<TShortCut, TdxRichEditCommandId>;
  private
    function GetControl: IdxRichEditControl;
    function GetInnerControl: IdxInnerControl;
  protected
    procedure PopulateCommandTable; virtual;

    procedure RegisterKeyCommand(AShortCut: TShortCut; ACommandClass: TdxRichEditCommandClass); overload;
    procedure RegisterKeyCommand(AKey: Word; const AShiftState: TShiftState; ACommandClass: TdxRichEditCommandClass); overload;
    procedure RegisterKeyCommand(AKey: Char; const AShiftState: TShiftState; ACommandClass: TdxRichEditCommandClass); overload;
    procedure UnregisterKeyCommand(AKey: Word; const AShiftState: TShiftState); overload;
    procedure UnregisterKeyCommand(AShortCut: TShortCut); overload;

    function CreateHandlerCommand(AHandlerId: TdxRichEditCommandId): TdxRichEditCommand; virtual;
    function ExecuteCommand(ACommand: TdxRichEditCommand; AShortCut: TShortCut): Boolean; virtual;
    procedure ExecuteCommandCore(ACommand: TdxRichEditCommand; AShortCut: TShortCut); virtual;
    function GetKeyHandler(const Args: TdxKeyEventArgs): TdxRichEditCommand; overload; virtual;
    function GetKeyHandlerCore(AShortCut: TShortCut): TdxRichEditCommand; virtual;
    function GetKeyHandlerId(AShortCut: TShortCut): TdxRichEditCommandId; virtual;

    function CanHandleKeyShortCut(ACommand: TdxRichEditCommandId; AShortCut: TShortCut; out AHandled: Boolean): Boolean;
    function HandleKeyShortCut(AShortCut: TShortCut): Boolean;

    function IsValidChar(const AChar: Char): Boolean;
    function HandleKeyPress(AChar: Char; const AShiftState: TShiftState): Boolean; overload; virtual;

    property Control: IdxRichEditControl read GetControl;
    property InnerControl: IdxInnerControl read GetInnerControl;
    property KeyHandlerIdTable: TDictionary<TShortCut, TdxRichEditCommandId> read FKeyHandlerIdTable;
  public
    constructor Create(AController: TdxRichEditKeyboardController); virtual;
    destructor Destroy; override;

    function CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean; virtual;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean; virtual;
    function HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean; overload;

    property Controller: TdxRichEditKeyboardController read FController;
  end;

  { TdxRichEditKeyboardDefaultHandler }

  TdxRichEditKeyboardDefaultHandler = class(TdxRichEditKeyboardCustomHandler)
  public const
    PendingTextFlushMilliseconds = 100;
  strict private
    FBeginProcessMultipleKeyPressCount: Integer;
    FLastFlushTime: Cardinal;
    FPreviousCursor: TdxNullableValue<TCursor>;
    FText: string;
    function CreateFlushPendingTextCommand: TdxInsertTextCommand;
    procedure FlushPendingTextInputIfNeeded;
    procedure PerformFlushPendingTextInput(const S: string);
    procedure FlushPendingTextInputCore(const S: string);
  protected
    procedure ExecuteCommandCore(ACommand: TdxRichEditCommand; AShortCut: TShortCut); override;
    procedure FlushPendingTextInputByParts(const APendingInput: string); virtual;
    function GetCursor: TCursor;
    function GetKeyHandler(const Args: TdxKeyEventArgs): TdxRichEditCommand; override;
    function IsSeparator(ACh: Char): Boolean; virtual;
    procedure PopulateNavigationCommandTable; virtual;
    procedure PopulateDialogsCommandTable; virtual;
    procedure PopulatePagesCommandTable; virtual;
    procedure PopulateCommandTable; override;
    procedure RestorePreviousCursor; virtual;
    procedure TryChangeCursor; virtual;
    procedure SetCursor(ACursor: TCursor);
    function ShouldChangeCursor(ACursor: TCursor): Boolean; virtual;
  public
    constructor Create(AController: TdxRichEditKeyboardController); override;

    procedure BeginProcessMultipleKeyPress; virtual;
    procedure EndProcessMultipleKeyPress; virtual;

    procedure FlushPendingTextInput; virtual;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean; override;
    function HandleKeyPress(AChar: Char; const AShiftState: TShiftState): Boolean; override;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean; override;
  end;

  { TdxRichEditKeyboardController }

  TdxRichEditKeyboardController = class(TdxCustomKeyboardController)
  strict private
    FInnerControl: TdxInnerRichEditControl;
    FDefaultHandler: IdxKeyboardHandlerService;
  private
    function GetActiveView: TdxRichEditView;
    function GetDocumentModel: TdxDocumentModel;
    function GetInnerControl: IdxInnerControl;
    function GetRichControl: IdxRichEditControl;
  protected
    function CreateDefaultHandler: IdxKeyboardHandlerService; virtual;

    procedure DoShortCut(Args: TdxRichEditShortCutEventArgs);

    property ActiveView: TdxRichEditView read GetActiveView;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InnerControl: IdxInnerControl read GetInnerControl;
    property RichControl: IdxRichEditControl read GetRichControl;
  public
    constructor Create(AInnerControl: TdxInnerRichEditControl); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TdxKeyboardHandlerServiceWrapper }

  TdxKeyboardHandlerServiceWrapper = class(TInterfacedObject, IdxKeyboardHandlerService)
  strict private
    FService: IdxKeyboardHandlerService;
  public
    constructor Create(const AService: IdxKeyboardHandlerService);
    destructor Destroy; override;
    //IdxKeyboardHandlerService
    function CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean; virtual;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean; virtual;
    function HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean; virtual;
    property Service: IdxKeyboardHandlerService read FService;
  end;

implementation

uses
  Forms,
  Menus,
  Character,
  dxCore,

  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Keyboard,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.Hyperlink,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.FileOperations,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.Commands.Tab,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.Commands.Fields;

{ TdxRichEditKeyboardDefaultHandler }

function TdxRichEditKeyboardCustomHandler.CanKeyDownModifyEdit(
  const Args: TdxKeyEventArgs): Boolean;
var
  AId: TdxRichEditCommandId;
begin
  AId := GetKeyHandlerId(TdxKeyboardHelper.ArgsToShortCut(Args));
  Result := AId <> TdxRichEditCommandId.None;
  if Result then
  begin
    case AId of
      TdxRichEditCommandId.InsertParagraph,
      TdxRichEditCommandId.InsertLineBreak,
      TdxRichEditCommandId.InsertPageBreak,
      TdxRichEditCommandId.InsertColumnBreak,
      TdxRichEditCommandId.InsertNonBreakingSpace,
      TdxRichEditCommandId.InsertEnDash,
      TdxRichEditCommandId.InsertEmDash,
      TdxRichEditCommandId.InsertCopyrightSymbol,
      TdxRichEditCommandId.InsertRegisteredTrademarkSymbol,
      TdxRichEditCommandId.InsertTrademarkSymbol,
      TdxRichEditCommandId.InsertEllipsis,
      TdxRichEditCommandId.InsertPicture,
      TdxRichEditCommandId.InsertTab,
      TdxRichEditCommandId.InsertPageNumberField,
      TdxRichEditCommandId.InsertPageCountField,
      TdxRichEditCommandId.InsertMailMergeField,
      TdxRichEditCommandId.InsertTableOfContents,
      TdxRichEditCommandId.InsertTableOfEquations,
      TdxRichEditCommandId.InsertTableOfFigures,
      TdxRichEditCommandId.InsertTableOfTables,
      TdxRichEditCommandId.InsertEquationCaption,
      TdxRichEditCommandId.InsertFigureCaption,
      TdxRichEditCommandId.InsertTableCaption,
      TdxRichEditCommandId.InsertText,
      TdxRichEditCommandId.Delete,
      TdxRichEditCommandId.BackSpaceKey,
      TdxRichEditCommandId.DeleteWord,
      TdxRichEditCommandId.DeleteWordBack,
      TdxRichEditCommandId.PasteSelection,
      TdxRichEditCommandId.CutSelection,
      TdxRichEditCommandId.Replace,
      TdxRichEditCommandId.TabKey,
      TdxRichEditCommandId.ShiftTabKey,
      TdxRichEditCommandId.FileNew,
      TdxRichEditCommandId.IncreaseIndent,
      TdxRichEditCommandId.DecreaseIndent,
      TdxRichEditCommandId.ClearFormatting,
      TdxRichEditCommandId.CreateField,
      TdxRichEditCommandId.UpdateField,
      TdxRichEditCommandId.EnterKey,
      TdxRichEditCommandId.ProtectDocument,
      TdxRichEditCommandId.UnprotectDocument,
      TdxRichEditCommandId.InsertBreak,
      TdxRichEditCommandId.CreateHyperlink,
      TdxRichEditCommandId.CreateBookmark,
      TdxRichEditCommandId.UpdateTableOfContents,
      TdxRichEditCommandId.ReplaceForward,
      TdxRichEditCommandId.ReplaceAllForward,
      TdxRichEditCommandId.UpdateFields,
      TdxRichEditCommandId.ChangePageColor,
      TdxRichEditCommandId.UpdateTableOfFigures,
      TdxRichEditCommandId.ReplaceMisspelling,
      TdxRichEditCommandId.DeleteRepeatedWord,
      TdxRichEditCommandId.AutoCorrectMisspelling,
      TdxRichEditCommandId.ToggleHeaderFooterLinkToPrevious,
      TdxRichEditCommandId.ToggleDifferentFirstPage,
      TdxRichEditCommandId.ToggleDifferentOddAndEvenPages,
      TdxRichEditCommandId.SetPortraitPageOrientation,
      TdxRichEditCommandId.SetLandscapePageOrientation,
      TdxRichEditCommandId.ChangeSectionPageOrientation,
      TdxRichEditCommandId.ChangeSectionPageMargins,
      TdxRichEditCommandId.SetNormalSectionPageMargins,
      TdxRichEditCommandId.SetNarrowSectionPageMargins,
      TdxRichEditCommandId.SetModerateSectionPageMargins,
      TdxRichEditCommandId.SetWideSectionPageMargins,
      TdxRichEditCommandId.SetSectionOneColumn,
      TdxRichEditCommandId.SetSectionTwoColumns,
      TdxRichEditCommandId.SetSectionThreeColumns,
      TdxRichEditCommandId.ChangeSectionPaperKind,
      TdxRichEditCommandId.InsertSectionBreakNextPage,
      TdxRichEditCommandId.InsertSectionBreakOddPage,
      TdxRichEditCommandId.InsertSectionBreakEvenPage,
      TdxRichEditCommandId.InsertSectionBreakContinuous,
      TdxRichEditCommandId.ChangeSectionLineNumbering,
      TdxRichEditCommandId.SetSectionLineNumberingNone,
      TdxRichEditCommandId.SetSectionLineNumberingContinuous,
      TdxRichEditCommandId.SetSectionLineNumberingRestartNewPage,
      TdxRichEditCommandId.SetSectionLineNumberingRestartNewSection,
      TdxRichEditCommandId.ToggleShowWhitespace,
      TdxRichEditCommandId.ToggleFontBold,
      TdxRichEditCommandId.ToggleFontItalic,
      TdxRichEditCommandId.ToggleFontUnderline,
      TdxRichEditCommandId.ToggleFontDoubleUnderline,
      TdxRichEditCommandId.IncreaseFontSize,
      TdxRichEditCommandId.DecreaseFontSize,
      TdxRichEditCommandId.IncrementFontSize,
      TdxRichEditCommandId.DecrementFontSize,
      TdxRichEditCommandId.ToggleFontSuperscript,
      TdxRichEditCommandId.ToggleFontSubscript,
      TdxRichEditCommandId.ToggleFontStrikeout,
      TdxRichEditCommandId.ToggleFontDoubleStrikeout,
      TdxRichEditCommandId.ChangeFontName,
      TdxRichEditCommandId.ChangeFontSize,
      TdxRichEditCommandId.ChangeFontStyle,
      TdxRichEditCommandId.ChangeFontForeColor,
      TdxRichEditCommandId.ChangeFontBackColor,
      TdxRichEditCommandId.MakeTextUpperCase,
      TdxRichEditCommandId.MakeTextLowerCase,
      TdxRichEditCommandId.ToggleTextCase,
      TdxRichEditCommandId.ChangeCase,
      TdxRichEditCommandId.ToggleParagraphAlignmentLeft,
      TdxRichEditCommandId.ToggleParagraphAlignmentCenter,
      TdxRichEditCommandId.ToggleParagraphAlignmentRight,
      TdxRichEditCommandId.ToggleParagraphAlignmentJustify,
      TdxRichEditCommandId.SetSingleParagraphSpacing,
      TdxRichEditCommandId.SetDoubleParagraphSpacing,
      TdxRichEditCommandId.SetSesquialteralParagraphSpacing,
      TdxRichEditCommandId.IncrementNumerationFromParagraph,
      TdxRichEditCommandId.DecrementNumerationFromParagraph,
      TdxRichEditCommandId.ToggleNumberingListItem,
      TdxRichEditCommandId.ToggleMultilevelListItem,
      TdxRichEditCommandId.ToggleBulletedListItem,
      TdxRichEditCommandId.ChangeParagraphLineSpacing,
      TdxRichEditCommandId.AddSpacingBeforeParagraph,
      TdxRichEditCommandId.AddSpacingAfterParagraph,
      TdxRichEditCommandId.RemoveSpacingBeforeParagraph,
      TdxRichEditCommandId.RemoveSpacingAfterParagraph,
      TdxRichEditCommandId.ToggleParagraphSuppressLineNumbers,
      TdxRichEditCommandId.ToggleParagraphSuppressHyphenation,
      TdxRichEditCommandId.IncrementParagraphOutlineLevel,
      TdxRichEditCommandId.DecrementParagraphOutlineLevel,
      TdxRichEditCommandId.SetParagraphBodyTextLevel,
      TdxRichEditCommandId.SetParagraphHeading1Level,
      TdxRichEditCommandId.SetParagraphHeading2Level,
      TdxRichEditCommandId.SetParagraphHeading3Level,
      TdxRichEditCommandId.SetParagraphHeading4Level,
      TdxRichEditCommandId.SetParagraphHeading5Level,
      TdxRichEditCommandId.SetParagraphHeading6Level,
      TdxRichEditCommandId.SetParagraphHeading7Level,
      TdxRichEditCommandId.SetParagraphHeading8Level,
      TdxRichEditCommandId.SetParagraphHeading9Level,
      TdxRichEditCommandId.AddParagraphsToTableOfContents,
      TdxRichEditCommandId.ChangeParagraphBackColor,
      TdxRichEditCommandId.InsertTable,
      TdxRichEditCommandId.InsertTableRowBelow,
      TdxRichEditCommandId.InsertTableRowAbove,
      TdxRichEditCommandId.MergeTableElement,
      TdxRichEditCommandId.MergeTableCells,
      TdxRichEditCommandId.SplitTable,
      TdxRichEditCommandId.ChangeTableBordersPlaceholder,
      TdxRichEditCommandId.ToggleTableCellsAllBorders,
      TdxRichEditCommandId.ResetTableCellsAllBorders,
      TdxRichEditCommandId.ToggleTableCellsOutsideBorder,
      TdxRichEditCommandId.ToggleTableCellsInsideBorder,
      TdxRichEditCommandId.ToggleTableCellsLeftBorder,
      TdxRichEditCommandId.ToggleTableCellsRightBorder,
      TdxRichEditCommandId.ToggleTableCellsTopBorder,
      TdxRichEditCommandId.ToggleTableCellsBottomBorder,
      TdxRichEditCommandId.ToggleTableCellsInsideHorizontalBorder,
      TdxRichEditCommandId.ToggleTableCellsInsideVerticalBorder,
      TdxRichEditCommandId.InsertTableColumnToTheLeft,
      TdxRichEditCommandId.InsertTableColumnToTheRight,
      TdxRichEditCommandId.DeleteTable,
      TdxRichEditCommandId.ChangeTableCellsBorderColor,
      TdxRichEditCommandId.ChangeTableCellsShading,
      TdxRichEditCommandId.DeleteTableColumns,
      TdxRichEditCommandId.DeleteTableRows,
      TdxRichEditCommandId.ToggleTableCellsTopLeftAlignment,
      TdxRichEditCommandId.ToggleTableCellsTopCenterAlignment,
      TdxRichEditCommandId.ToggleTableCellsTopRightAlignment,
      TdxRichEditCommandId.ToggleTableCellsMiddleLeftAlignment,
      TdxRichEditCommandId.ToggleTableCellsMiddleCenterAlignment,
      TdxRichEditCommandId.ToggleTableCellsMiddleRightAlignment,
      TdxRichEditCommandId.ToggleTableCellsBottomLeftAlignment,
      TdxRichEditCommandId.ToggleTableCellsBottomCenterAlignment,
      TdxRichEditCommandId.ToggleTableCellsBottomRightAlignment,
      TdxRichEditCommandId.ChangeTableCellsBorderLineStyle,
      TdxRichEditCommandId.ToggleTableAutoFitPlaceholder,
      TdxRichEditCommandId.ToggleTableAutoFitContents,
      TdxRichEditCommandId.ToggleTableAutoFitWindow,
      TdxRichEditCommandId.ToggleTableFixedColumnWidth,
      TdxRichEditCommandId.ChangeTableCellsBorderLineWeight,
      TdxRichEditCommandId.ChangeTableStyle,
      TdxRichEditCommandId.SetFloatingObjectSquareTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectBehindTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectInFrontOfTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectThroughTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectTightTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectTopAndBottomTextWrapType,
      TdxRichEditCommandId.SetFloatingObjectTopLeftAlignment,
      TdxRichEditCommandId.SetFloatingObjectTopCenterAlignment,
      TdxRichEditCommandId.SetFloatingObjectTopRightAlignment,
      TdxRichEditCommandId.SetFloatingObjectMiddleLeftAlignment,
      TdxRichEditCommandId.SetFloatingObjectMiddleCenterAlignment,
      TdxRichEditCommandId.SetFloatingObjectMiddleRightAlignment,
      TdxRichEditCommandId.SetFloatingObjectBottomLeftAlignment,
      TdxRichEditCommandId.SetFloatingObjectBottomCenterAlignment,
      TdxRichEditCommandId.SetFloatingObjectBottomRightAlignment,
      TdxRichEditCommandId.ChangeFloatingObjectTextWrapType,
      TdxRichEditCommandId.ChangeFloatingObjectAlignment,
      TdxRichEditCommandId.FloatingObjectBringForward,
      TdxRichEditCommandId.FloatingObjectBringToFront,
      TdxRichEditCommandId.FloatingObjectBringInFrontOfText,
      TdxRichEditCommandId.FloatingObjectSendBackward,
      TdxRichEditCommandId.FloatingObjectSendToBack,
      TdxRichEditCommandId.FloatingObjectSendBehindText,
      TdxRichEditCommandId.FloatingObjectSendBackwardPlaceholder,
      TdxRichEditCommandId.SelectUpperLevelObject,
      TdxRichEditCommandId.ChangeFloatingObjectFillColor,
      TdxRichEditCommandId.ChangeFloatingObjectOutlineColor,
      TdxRichEditCommandId.ChangeFloatingObjectOutlineWeight,
      TdxRichEditCommandId.InsertTextBox,
      TdxRichEditCommandId.InsertFloatingPicture: Result := True;
    else
      Result := False;
    end;
  end;
end;

constructor TdxRichEditKeyboardCustomHandler.Create(AController: TdxRichEditKeyboardController);
begin
  inherited Create;
  FController := AController;
  FKeyHandlerIdTable := TDictionary<TShortCut, TdxRichEditCommandId>.Create;
  PopulateCommandTable;
end;

destructor TdxRichEditKeyboardCustomHandler.Destroy;
begin
  FreeAndNil(FKeyHandlerIdTable);
  inherited Destroy;
end;

function TdxRichEditKeyboardCustomHandler.ExecuteCommand(
  ACommand: TdxRichEditCommand; AShortCut: TShortCut): Boolean;
begin
  Result := ACommand <> nil;
  if Result then
    ExecuteCommandCore(ACommand, AShortCut);
end;

procedure TdxRichEditKeyboardCustomHandler.ExecuteCommandCore(
  ACommand: TdxRichEditCommand; AShortCut: TShortCut);
begin
  ACommand.CommandSourceType := TdxCommandSourceType.Keyboard;
  ACommand.Execute;
end;

function TdxRichEditKeyboardCustomHandler.CanHandleKeyShortCut(ACommand: TdxRichEditCommandId;
  AShortCut: TShortCut; out AHandled: Boolean): Boolean;
var
  Args: TdxRichEditShortCutEventArgs;
begin
  Args := TdxRichEditShortCutEventArgs.Create(ACommand, TdxKeyboardHelper.GetKeyData(AShortCut),
    TdxKeyboardHelper.IsAltGrPressed, TdxKeyboardHelper.GetShiftState(AShortCut));
  try
    FController.DoShortCut(Args);
    AHandled := Args.Handled;
    Result := Args.Allow;
  finally
    Args.Free;
  end;
end;

function TdxRichEditKeyboardCustomHandler.HandleKeyShortCut(AShortCut: TShortCut): Boolean;
var
  ACommand: TdxRichEditCommand;
  AHandled: Boolean;
begin
  Result := False;
  ACommand := GetKeyHandlerCore(AShortCut);
  if ACommand <> nil then
  try
    Result := CanHandleKeyShortCut(ACommand.Id, AShortCut, AHandled);
    if Result and not AHandled then
      Result := ExecuteCommand(ACommand, AShortCut);
  finally
    ACommand.Free;
  end;
end;

function TdxRichEditKeyboardCustomHandler.HandleKeyDown(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Result := HandleKeyShortCut(TdxKeyboardHelper.ArgsToShortCut(Args));
end;

function TdxRichEditKeyboardCustomHandler.HandleKeyPress(AChar: Char;
  const AShiftState: TShiftState): Boolean;
var
  AKey: Word;
begin
  Result := TdxKeyboardHelper.IsValidShortCutChar(AChar, AKey) and HandleKeyShortCut(TdxKeyboardHelper.KeysToShortCut(AKey, AShiftState));
end;

function TdxRichEditKeyboardCustomHandler.HandleKeyPress(
  const Args: TdxKeyPressEventArgs): Boolean;
begin
  Result := HandleKeyPress(Args.Key, Args.Shift);
end;

Function TdxRichEditKeyboardCustomHandler.HandleKeyUp(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Result := False;
end;

procedure TdxRichEditKeyboardCustomHandler.PopulateCommandTable;
begin
end;

procedure TdxRichEditKeyboardCustomHandler.RegisterKeyCommand(
  AShortCut: TShortCut; ACommandClass: TdxRichEditCommandClass);
begin
  KeyHandlerIdTable.Add(AShortCut, ACommandClass.Id);
  InnerControl.RegisterCommand(ACommandClass);
end;

procedure TdxRichEditKeyboardCustomHandler.RegisterKeyCommand(AKey: Word;
  const AShiftState: TShiftState; ACommandClass: TdxRichEditCommandClass);
begin
  RegisterKeyCommand(TdxKeyboardHelper.KeysToShortCut(AKey, AShiftState), ACommandClass);
end;

procedure TdxRichEditKeyboardCustomHandler.RegisterKeyCommand(AKey: Char;
  const AShiftState: TShiftState; ACommandClass: TdxRichEditCommandClass);
begin
  RegisterKeyCommand(TdxKeyboardHelper.CharToShortCut(AKey, AShiftState), ACommandClass);
end;

procedure TdxRichEditKeyboardCustomHandler.UnregisterKeyCommand(AKey: Word;
  const AShiftState: TShiftState);
begin
  UnregisterKeyCommand(TdxKeyboardHelper.KeysToShortCut(AKey, AShiftState));
end;

function TdxRichEditKeyboardCustomHandler.CreateHandlerCommand(AHandlerId: TdxRichEditCommandId): TdxRichEditCommand;
begin
  Result := InnerControl.CreateCommand(AHandlerId);
end;

function TdxRichEditKeyboardCustomHandler.GetControl: IdxRichEditControl;
begin
  Result := Controller.RichControl;
end;

function TdxRichEditKeyboardCustomHandler.GetInnerControl: IdxInnerControl;
begin
  Result := Controller.InnerControl;
end;

function TdxRichEditKeyboardCustomHandler.IsValidChar(const AChar: Char): Boolean;
begin
  Result := Ord(AChar) >= 32;
end;

function TdxRichEditKeyboardCustomHandler.GetKeyHandler(const Args: TdxKeyEventArgs): TdxRichEditCommand;
begin
  Result := GetKeyHandlerCore(TdxKeyboardHelper.ArgsToShortCut(Args));
end;

function TdxRichEditKeyboardCustomHandler.GetKeyHandlerCore(AShortCut: TShortCut): TdxRichEditCommand;
var
  AHandlerId: TdxRichEditCommandId;
begin
  AHandlerId := GetKeyHandlerId(AShortCut);
  if AHandlerId <> TdxRichEditCommandId.None then
    Result := CreateHandlerCommand(AHandlerId)
  else
    Result := nil;
end;

function TdxRichEditKeyboardCustomHandler.GetKeyHandlerId(AShortCut: TShortCut): TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.None;
  if KeyHandlerIdTable.ContainsKey(AShortCut) then
    Result := KeyHandlerIdTable[AShortCut];
end;

{ TdxRichEditKeyboardDefaultHandler }

constructor TdxRichEditKeyboardDefaultHandler.Create(AController: TdxRichEditKeyboardController);
begin
  inherited Create(AController);
  FLastFlushTime := GetTickCount;
end;

procedure TdxRichEditKeyboardDefaultHandler.BeginProcessMultipleKeyPress;
begin
  Inc(FBeginProcessMultipleKeyPressCount);
end;

procedure TdxRichEditKeyboardDefaultHandler.EndProcessMultipleKeyPress;
begin
  if FBeginProcessMultipleKeyPressCount > 0 then
    Dec(FBeginProcessMultipleKeyPressCount);
  if FBeginProcessMultipleKeyPressCount = 0 then
    FlushPendingTextInputIfNeeded;
end;

procedure TdxRichEditKeyboardDefaultHandler.FlushPendingTextInput;
var
  APendingInput: string;
begin
  FLastFlushTime := GetTickCount;
  if Length(FText) = 0 then
    Exit;
  APendingInput := FText;
  FText := '';
  PerformFlushPendingTextInput(APendingInput);
  FLastFlushTime := GetTickCount;
end;

procedure TdxRichEditKeyboardDefaultHandler.ExecuteCommandCore(ACommand: TdxRichEditCommand; AShortCut: TShortCut);
begin
  FlushPendingTextInput;
  inherited ExecuteCommandCore(ACommand, AShortCut);
end;

procedure TdxRichEditKeyboardDefaultHandler.FlushPendingTextInputByParts(const APendingInput: string);
var
  AStartIndex, ACount, I: Integer;
  APart: string;
begin
  AStartIndex := 1;
  ACount := Length(APendingInput);
  for I := 1 to ACount do
  begin
    if IsSeparator(APendingInput[I]) then
    begin
      APart := Copy(APendingInput, AStartIndex, I - AStartIndex + 1);
      AStartIndex := I + 1;
      FlushPendingTextInputCore(APart);
    end;
  end;
  if AStartIndex <= ACount then
    FlushPendingTextInputCore(Copy(APendingInput, AStartIndex, MaxInt));
end;

procedure TdxRichEditKeyboardDefaultHandler.FlushPendingTextInputCore(const S: string);
var
  ACommand: TdxInsertTextCommand;
begin
  ACommand := CreateFlushPendingTextCommand;
  try
    ACommand.Text := S;
    ACommand.CommandSourceType := TdxCommandSourceType.Keyboard;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxRichEditKeyboardDefaultHandler.CreateFlushPendingTextCommand: TdxInsertTextCommand;
const
  AIDMap: array[Boolean] of TdxRichEditCommandId = (TdxRichEditCommandId.InsertText, TdxRichEditCommandId.OvertypeText);
begin
  Result := TdxInsertTextCommand(Controller.InnerControl.CreateCommand(AIDMap[Control.Overtype]));
end;

function TdxRichEditKeyboardDefaultHandler.GetKeyHandler(
  const Args: TdxKeyEventArgs): TdxRichEditCommand;
begin
  Result := inherited GetKeyHandler(Args);
  if Result <> nil then
    FlushPendingTextInput;
end;

function TdxRichEditKeyboardDefaultHandler.HandleKeyDown(const Args: TdxKeyEventArgs): Boolean;
begin
  if inherited HandleKeyDown(Args) then
    Exit(True);
  TryChangeCursor;
  Result := False;
end;

function TdxRichEditKeyboardDefaultHandler.HandleKeyUp(const Args: TdxKeyEventArgs): Boolean;
begin
  Result := inherited HandleKeyUp(Args);
  RestorePreviousCursor;
end;

function TdxRichEditKeyboardDefaultHandler.HandleKeyPress(AChar: Char; const AShiftState: TShiftState): Boolean;
begin
  Result := inherited HandleKeyPress(AChar, AShiftState);
  if not Result and IsValidChar(AChar) then
  begin
    FText := FText + AChar;
    FlushPendingTextInputIfNeeded;
    Result := True;
  end;
end;

procedure TdxRichEditKeyboardDefaultHandler.FlushPendingTextInputIfNeeded;
var
  ASpan: Cardinal;
begin
  ASpan := GetTickCount - FLastFlushTime;
  if (ASpan > PendingTextFlushMilliseconds) and (FBeginProcessMultipleKeyPressCount = 0) then
    FlushPendingTextInput;
end;

function TdxRichEditKeyboardDefaultHandler.IsSeparator(ACh: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := ACh.IsWhiteSpace or ACh.IsSeparator or ACh.IsPunctuation;
{$ELSE}
  Result := TCharacter.IsWhiteSpace(ACh) or TCharacter.IsSeparator(ACh) or TCharacter.IsPunctuation(ACh);
{$ENDIF}
end;

procedure TdxRichEditKeyboardDefaultHandler.PerformFlushPendingTextInput(const S: string);
begin
  if Length(S) = 1 then
    FlushPendingTextInputCore(S)
  else
    FlushPendingTextInputByParts(S);
end;

procedure TdxRichEditKeyboardDefaultHandler.PopulateNavigationCommandTable;
begin
  RegisterKeyCommand(VK_LEFT, [], TdxPreviousCharacterCommand);
  RegisterKeyCommand(VK_RIGHT, [], TdxNextCharacterCommand);
  RegisterKeyCommand(VK_LEFT, [ssShift], TdxExtendPreviousCharacterCommand);
  RegisterKeyCommand(VK_RIGHT, [ssShift], TdxExtendNextCharacterCommand);
  RegisterKeyCommand(VK_LEFT, [ssCtrl], TdxPreviousWordCommand);
  RegisterKeyCommand(VK_RIGHT, [ssCtrl], TdxNextWordCommand);
  RegisterKeyCommand(VK_LEFT, [ssCtrl, ssShift], TdxExtendPreviousWordCommand);
  RegisterKeyCommand(VK_RIGHT, [ssShift, ssCtrl], TdxExtendNextWordCommand);
  RegisterKeyCommand(VK_HOME, [], TdxStartOfLineCommand);
  RegisterKeyCommand(VK_END, [], TdxEndOfLineCommand);
  RegisterKeyCommand(VK_HOME, [ssShift], TdxExtendStartOfLineCommand);
  RegisterKeyCommand(VK_END, [ssShift], TdxExtendEndOfLineCommand);
  RegisterKeyCommand(VK_UP, [], TdxPreviousLineCommand);
  RegisterKeyCommand(VK_DOWN, [], TdxNextLineCommand);
  RegisterKeyCommand(VK_UP, [ssShift], TdxExtendPreviousLineCommand);
  RegisterKeyCommand(VK_DOWN, [ssShift], TdxExtendNextLineCommand);
  RegisterKeyCommand(VK_UP, [ssCtrl], TdxPreviousParagraphCommand);
  RegisterKeyCommand(VK_DOWN, [ssCtrl], TdxNextParagraphCommand);
  RegisterKeyCommand(VK_UP, [ssCtrl, ssShift], TdxExtendPreviousParagraphCommand);
  RegisterKeyCommand(VK_DOWN, [ssCtrl, ssShift], TdxExtendNextParagraphCommand);
  RegisterKeyCommand(VK_PRIOR, [ssCtrl], TdxPreviousPageCommand);
  RegisterKeyCommand(VK_NEXT, [ssCtrl], TdxNextPageCommand);
  RegisterKeyCommand(VK_PRIOR, [ssCtrl, ssShift], TdxExtendPreviousPageCommand);
  RegisterKeyCommand(VK_NEXT, [ssCtrl, ssShift], TdxExtendNextPageCommand);
  RegisterKeyCommand(VK_PRIOR, [], TdxPreviousScreenCommand);
  RegisterKeyCommand(VK_NEXT, [], TdxNextScreenCommand);
  RegisterKeyCommand(VK_PRIOR, [ssShift], TdxExtendPreviousScreenCommand);
  RegisterKeyCommand(VK_NEXT, [ssShift], TdxExtendNextScreenCommand);
  RegisterKeyCommand(VK_HOME, [ssCtrl], TdxStartOfDocumentCommand);
  RegisterKeyCommand(VK_END, [ssCtrl], TdxEndOfDocumentCommand);
  RegisterKeyCommand(VK_HOME, [ssCtrl, ssShift], TdxExtendStartOfDocumentCommand);
  RegisterKeyCommand(VK_END, [ssCtrl, ssShift], TdxExtendEndOfDocumentCommand);
  RegisterKeyCommand('A', [ssCtrl], TdxSelectAllCommand);
  RegisterKeyCommand(VK_NUMPAD5, [ssCtrl], TdxSelectAllCommand);
  RegisterKeyCommand(VK_CLEAR, [ssCtrl], TdxSelectAllCommand);
end;

procedure TdxRichEditKeyboardDefaultHandler.PopulateDialogsCommandTable;
begin
end;

procedure TdxRichEditKeyboardDefaultHandler.PopulatePagesCommandTable;
begin
  RegisterKeyCommand(VK_RETURN, [ssCtrl], TdxInsertPageBreakCommand);
end;

procedure TdxRichEditKeyboardDefaultHandler.PopulateCommandTable;
begin
  inherited PopulateCommandTable;
  PopulateNavigationCommandTable;
  PopulateDialogsCommandTable;
  PopulatePagesCommandTable;
  RegisterKeyCommand('8', [ssShift, ssCtrl], TdxToggleShowWhitespaceCommand);
  RegisterKeyCommand('Z', [ssCtrl], TdxUndoCommand);
  RegisterKeyCommand('Y', [ssCtrl], TdxRedoCommand);
  RegisterKeyCommand(VK_BACK, [ssAlt], TdxUndoCommand);
  RegisterKeyCommand(VK_BACK, [ssAlt, ssShift], TdxRedoCommand);
  RegisterKeyCommand(VK_RETURN, [], TdxEnterKeyCommand);
  RegisterKeyCommand(VK_RETURN, [ssShift], TdxInsertLineBreakCommand);
  RegisterKeyCommand(VK_INSERT, [], TdxToggleOvertypeCommand);
  RegisterKeyCommand(VK_TAB, [], TdxTabKeyCommand);
  RegisterKeyCommand(VK_TAB, [ssShift], TdxShiftTabKeyCommand);
  RegisterKeyCommand(VK_TAB, [ssCtrl], TdxInsertTabCommand);
  RegisterKeyCommand(VK_SPACE, [ssShift, ssCtrl], TdxInsertNonBreakingSpaceCommand);
  RegisterKeyCommand(VK_RETURN, [ssCtrl, ssShift], TdxInsertColumnBreakCommand);
  RegisterKeyCommand(VK_SUBTRACT, [ssCtrl], TdxInsertEnDashCommand);
  RegisterKeyCommand(VK_SUBTRACT, [ssCtrl, ssAlt], TdxInsertEmDashCommand);
  RegisterKeyCommand('C', [ssCtrl, ssAlt], TdxInsertCopyrightSymbolCommand);
  RegisterKeyCommand('R', [ssCtrl, ssAlt], TdxInsertRegisteredTrademarkSymbolCommand);
  RegisterKeyCommand('T', [ssCtrl, ssAlt], TdxInsertTrademarkSymbolCommand);
  RegisterKeyCommand(VK_OEM_PERIOD, [ssCtrl, ssAlt], TdxInsertEllipsisCommand);
  RegisterKeyCommand('B', [ssCtrl], TdxToggleFontBoldCommand);
  RegisterKeyCommand('I', [ssCtrl], TdxToggleFontItalicCommand);
  RegisterKeyCommand('U', [ssCtrl], TdxToggleFontUnderlineCommand);
  RegisterKeyCommand('D', [ssCtrl, ssShift], TdxToggleFontDoubleUnderlineCommand);
  RegisterKeyCommand(VK_OEM_PERIOD, [ssCtrl, ssShift], TdxIncreaseFontSizeCommand);
  RegisterKeyCommand(VK_OEM_COMMA, [ssCtrl, ssShift], TdxDecreaseFontSizeCommand);
  RegisterKeyCommand(VK_OEM_6, [ssCtrl], TdxIncrementFontSizeCommand);
  RegisterKeyCommand(VK_OEM_4, [ssCtrl], TdxDecrementFontSizeCommand);
  RegisterKeyCommand(VK_OEM_PLUS, [ssCtrl, ssShift], TdxToggleFontSuperscriptCommand);
  RegisterKeyCommand(VK_OEM_PLUS, [ssCtrl], TdxToggleFontSubscriptCommand);

  RegisterKeyCommand('L', [ssCtrl], TdxToggleParagraphAlignmentLeftCommand);
  RegisterKeyCommand('E', [ssCtrl], TdxToggleParagraphAlignmentCenterCommand);
  RegisterKeyCommand('R', [ssCtrl], TdxToggleParagraphAlignmentRightCommand);
  RegisterKeyCommand('J', [ssCtrl], TdxToggleParagraphAlignmentJustifyCommand);

  RegisterKeyCommand('1', [ssCtrl], TdxSetSingleParagraphSpacingCommand);
  RegisterKeyCommand('2', [ssCtrl], TdxSetDoubleParagraphSpacingCommand);
  RegisterKeyCommand('3', [ssCtrl], TdxSetSesquialteralParagraphSpacingCommand);
  RegisterKeyCommand(VK_DELETE, [], TdxDeleteCommand);
  RegisterKeyCommand(VK_BACK, [], TdxBackSpaceKeyCommand);
  RegisterKeyCommand(VK_BACK, [ssShift], TdxBackSpaceKeyCommand);
  RegisterKeyCommand(VK_DELETE, [ssCtrl], TdxDeleteWordCommand);
  RegisterKeyCommand(VK_BACK, [ssCtrl], TdxDeleteWordBackCommand);
  RegisterKeyCommand('C', [ssCtrl], TdxCopySelectionCommand);
  RegisterKeyCommand(VK_INSERT, [ssCtrl], TdxCopySelectionCommand);
  RegisterKeyCommand('V', [ssCtrl], TdxPasteSelectionCommand);
  RegisterKeyCommand(VK_INSERT, [ssShift], TdxPasteSelectionCommand);
  RegisterKeyCommand('X', [ssCtrl], TdxCutSelectionCommand);
  RegisterKeyCommand(VK_DELETE, [ssShift], TdxCutSelectionCommand);

  RegisterKeyCommand('I', [ssAlt], TdxIncrementNumerationFromParagraphCommand);
  RegisterKeyCommand('I', [ssAlt, ssCtrl], TdxDecrementNumerationFromParagraphCommand);
  RegisterKeyCommand(VK_F9, [], TdxUpdateFieldsCommand);
  RegisterKeyCommand(VK_F9, [ssCtrl], TdxCreateFieldCommand);
  RegisterKeyCommand(VK_ESCAPE, [], TdxSelectUpperLevelObjectCommand);
end;

function TdxRichEditKeyboardDefaultHandler.GetCursor: TCursor;
begin
  Result := Controller.RichControl.Control.Cursor;
end;

function TdxRichEditKeyboardDefaultHandler.ShouldChangeCursor(ACursor: TCursor): Boolean;
begin
  if GetCursor = ACursor then
    Exit(False);
  Result := FPreviousCursor.IsNull or (ACursor <> FPreviousCursor.Value);
end;

procedure TdxRichEditKeyboardDefaultHandler.RestorePreviousCursor;
begin
  if not FPreviousCursor.IsNull then
  begin
    SetCursor(FPreviousCursor.Value);
    FPreviousCursor.Reset;
  end;
end;

procedure TdxRichEditKeyboardDefaultHandler.TryChangeCursor;
begin
  if not (ShouldChangeCursor(TdxRichEditCursors.Hand) and Control.IsHyperlinkActive) then
		Exit;
  FPreviousCursor := GetCursor;
  SetCursor(TdxRichEditCursors.Hand);
end;

procedure TdxRichEditKeyboardDefaultHandler.SetCursor(ACursor: TCursor);
begin
  Controller.RichControl.Control.Cursor := ACursor
end;

{ TdxRichEditKeyboardController }

constructor TdxRichEditKeyboardController.Create(AInnerControl: TdxInnerRichEditControl);
begin
  inherited Create;
  FInnerControl := AInnerControl;
  FDefaultHandler := CreateDefaultHandler;
  Handlers.Add(FDefaultHandler);
end;

function TdxRichEditKeyboardController.CreateDefaultHandler: IdxKeyboardHandlerService;
begin
  Result := TdxRichEditKeyboardDefaultHandler.Create(Self);
end;

procedure TdxRichEditKeyboardController.DoShortCut(Args: TdxRichEditShortCutEventArgs);
begin
  FInnerControl.DoShortCut(Args);
end;

destructor TdxRichEditKeyboardController.Destroy;
begin
  Handlers.Remove(FDefaultHandler);
  FDefaultHandler := nil;
  inherited Destroy;
end;

function TdxRichEditKeyboardController.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

function TdxRichEditKeyboardController.GetDocumentModel: TdxDocumentModel;
begin
  Result := InnerControl.DocumentModel;
end;

function TdxRichEditKeyboardController.GetInnerControl: IdxInnerControl;
begin
  Result := FInnerControl;
end;

function TdxRichEditKeyboardController.GetRichControl: IdxRichEditControl;
begin
  Result := FInnerControl.Owner.RichEditControl;
end;

procedure TdxRichEditKeyboardCustomHandler.UnregisterKeyCommand(
  AShortCut: TShortCut);
begin
  if KeyHandlerIdTable.ContainsKey(AShortCut) then
    KeyHandlerIdTable.Remove(AShortCut);
end;

{ TdxKeyboardHandlerServiceWrapper }

constructor TdxKeyboardHandlerServiceWrapper.Create(const AService: IdxKeyboardHandlerService);
begin
  inherited Create;
  FService := AService;
end;

destructor TdxKeyboardHandlerServiceWrapper.Destroy;
begin
  FService := nil;
  inherited Destroy;
end;

function TdxKeyboardHandlerServiceWrapper.CanKeyDownModifyEdit(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Result := Service.CanKeyDownModifyEdit(Args);
end;

function TdxKeyboardHandlerServiceWrapper.HandleKeyDown(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Service.HandleKeyDown(Args);
  Result := Args.Handled;
end;

function TdxKeyboardHandlerServiceWrapper.HandleKeyPress(
  const Args: TdxKeyPressEventArgs): Boolean;
begin
  Service.HandleKeyPress(Args);
  Result := Args.Handled;
end;

function TdxKeyboardHandlerServiceWrapper.HandleKeyUp(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Service.HandleKeyUp(Args);
  Result := Args.Handled;
end;

end.
