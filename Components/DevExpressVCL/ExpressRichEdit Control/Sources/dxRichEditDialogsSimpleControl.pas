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

unit dxRichEditDialogsSimpleControl;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  cxControls, cxGraphics, cxContainer, cxEdit,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.InnerControl,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Control,
  dxRichEdit.InnerControl.SpellCheckerController;

type
  { TdxCustomSimpleRichEditControl }

  TdxCustomSimpleRichEditControl = class(TdxCustomRichEditControl)
  private
    FAbstractList: TdxAbstractNumberingList;
    FEnableSelection: Boolean;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
    function CreateInnerControl: TdxInnerRichEditControl; override;
    procedure DoSelectionChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetText(const AText: string);
    property AbstractList: TdxAbstractNumberingList read FAbstractList write FAbstractList;
    property Color;
    property EnableSelection: Boolean read FEnableSelection write FEnableSelection default True;
  end;

  { TdxSimpleInnerRichEditControl }

  TdxSimpleInnerRichEditControl = class(TdxInnerRichEditControl)
  protected
    function CreateKeyboardController: TdxCustomKeyboardController; override;
    function CreateDocumentModelCore: TdxDocumentModel; override;
    function CreateMouseController: TdxRichEditCustomMouseController; override;
    function CreateSpellCheckerController: TdxSpellCheckerCustomController; override;
    function CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager; override;
  end;

  { TdxSimpleRichEditParagraphFormattingInfoCache }

  TdxSimpleRichEditParagraphFormattingInfoCache = class(TdxParagraphFormattingInfoCache)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo; override;
  end;

  { TdxSimpleRichEditParagraphFormattingCache }

  TdxSimpleRichEditParagraphFormattingCache = class(TdxParagraphFormattingCache)
  protected
    function CreateDefaultParagraphFormattingInfo(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo; override;
  end;

  { TdxSimpleRichEditDocumentCache }

  TdxSimpleRichEditDocumentCache = class(TdxDocumentCache)
  protected
    function CreateParagraphFormattingCache(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache; override;
    function CreateParagraphFormattingInfoCache(AUnitConverter: TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache; override;
  end;

  { TdxSimpleRichEditDocumentModel }

  TdxSimpleRichEditDocumentModel = class(TdxDocumentModel)
  protected
    function CreateDocumentCache: TdxCustomDocumentCache; override;
  end;

  { TdxSimpleRichEditKeyboardHandler }

  TdxSimpleRichEditKeyboardHandler = class(TdxRichEditKeyboardDefaultHandler)
  protected
    procedure PopulateCommandTable; override;
  end;

  { TdxSimpleRichEditKeyboardController }

  TdxSimpleRichEditKeyboardController = class(TdxRichEditKeyboardController)
  protected
    function CreateDefaultHandler: IdxKeyboardHandlerService; override;
  end;

  { TdxSimpleRichEditControl }

  TdxSimpleRichEditControl = class(TdxCustomSimpleRichEditControl)
  published
    property Align;
    property BorderStyle;
    property Font;
    property LookAndFeel;
    property TabOrder;
    property Visible;
    property OnKeyDown;
  end;

implementation

uses
  dxTypeHelpers,
  dxRichEdit.Commands,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.Keyboard,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.View.Core,
  dxRichEdit.Actions,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Options,
  dxRichEdit.Types;

type
  TdxRichEditControlAccess = class(TdxCustomRichEditControl);

{ TdxCustomSimleRichEditControl }

procedure TdxCustomSimpleRichEditControl.BeginUpdate;
begin
  InnerControl.BeginUpdate;
end;

constructor TdxCustomSimpleRichEditControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActiveViewType := TdxRichEditViewType.Simple;
  Options.HorizontalRuler.Visibility := TdxRichEditRulerVisibility.Hidden;
  Options.VerticalRuler.Visibility := TdxRichEditRulerVisibility.Hidden;
  Options.HorizontalScrollbar.Visibility := TdxRichEditScrollbarVisibility.Hidden;
  Options.VerticalScrollbar.Visibility := TdxRichEditScrollbarVisibility.Hidden;
  Views.Simple.Padding.All := 0;
  BorderStyle := cxcbsNone;
  Enabled := False;
  FEnableSelection := True;
  DocumentModel.Sections.First.Page.Width := MaxInt;
end;

function TdxCustomSimpleRichEditControl.CreateInnerControl: TdxInnerRichEditControl;
begin
  Result := TdxSimpleInnerRichEditControl.Create(Self);
end;

procedure TdxCustomSimpleRichEditControl.DoSelectionChanged(Sender: TObject);
begin
end;

procedure TdxCustomSimpleRichEditControl.EndUpdate;
begin
  InnerControl.EndUpdate;
end;

procedure TdxCustomSimpleRichEditControl.SetText(const AText: string);
var
  ACommand: TdxInsertTextCommand;
begin
  ACommand := TdxInsertTextCommand.Create(TdxRichEditControlAccess(InnerControl));
  try
    ACommand.CommandSourceType := TdxCommandSourceType.Menu;
    ACommand.Text := AText;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxCustomSimpleRichEditControl.WMMouseMove(var Message: TWMMouseMove);
begin
  if not FEnableSelection then
    Exit;
  inherited;
end;

{ TdxSimpleInnerRichEditControl }

function TdxSimpleInnerRichEditControl.CreateDocumentModelCore: TdxDocumentModel;
begin
  Result := TdxSimpleRichEditDocumentModel.Create;
end;

function TdxSimpleInnerRichEditControl.CreateMouseController: TdxRichEditCustomMouseController;
begin
  Result := TdxRichEditMouseController.Create(Owner.RichEditControl);
end;

function TdxSimpleInnerRichEditControl.CreateKeyboardController: TdxCustomKeyboardController;
begin
  Result := TdxSimpleRichEditKeyboardController.Create(Self);
end;

function TdxSimpleInnerRichEditControl.CreateSpellCheckerController: TdxSpellCheckerCustomController;
begin
  Result := TdxEmptySpellCheckerController.Create;
end;

function TdxSimpleInnerRichEditControl.CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager;
begin
  Result := TdxEmptySpellCheckerManager.Create(APieceTable);
end;

{ TdxSimpleRichEditDocumentModel }

function TdxSimpleRichEditDocumentModel.CreateDocumentCache: TdxCustomDocumentCache;
begin
  Result := TdxSimpleRichEditDocumentCache.Create;
end;

{ TdxSimpleRichEditKeyboardHandler }

procedure TdxSimpleRichEditKeyboardHandler.PopulateCommandTable;
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

  RegisterKeyCommand(VK_HOME, [ssCtrl], TdxStartOfDocumentCommand);
  RegisterKeyCommand(VK_END, [ssCtrl], TdxEndOfDocumentCommand);
  RegisterKeyCommand(VK_HOME, [ssCtrl, ssShift], TdxExtendStartOfDocumentCommand);
  RegisterKeyCommand(VK_END, [ssCtrl, ssShift], TdxExtendEndOfDocumentCommand);

  RegisterKeyCommand(VK_TAB, [ssCtrl], TdxInsertTabCommand);

  RegisterKeyCommand('V', [ssCtrl], TdxPasteSelectionCommand);
  RegisterKeyCommand(VK_INSERT, [ssShift], TdxPasteSelectionCommand);

  RegisterKeyCommand('C', [ssCtrl], TdxCopySelectionCommand);
  RegisterKeyCommand(VK_INSERT, [ssCtrl], TdxCopySelectionCommand);

  RegisterKeyCommand('X', [ssCtrl], TdxCutSelectionCommand);
  RegisterKeyCommand(VK_DELETE, [ssShift], TdxCutSelectionCommand);

  RegisterKeyCommand(VK_DELETE, [], TdxDeleteCommand);
  RegisterKeyCommand(VK_BACK, [], TdxBackSpaceKeyCommand);
  RegisterKeyCommand(VK_BACK, [ssShift], TdxBackSpaceKeyCommand);
  RegisterKeyCommand(VK_DELETE, [ssCtrl], TdxDeleteWordCommand);
  RegisterKeyCommand(VK_BACK, [ssCtrl], TdxDeleteWordBackCommand);

  RegisterKeyCommand('A', [ssCtrl], TdxSelectAllCommand);
  RegisterKeyCommand(VK_NUMPAD5, [ssCtrl], TdxSelectAllCommand);
  RegisterKeyCommand(VK_CLEAR, [ssCtrl], TdxSelectAllCommand);
  RegisterKeyCommand('Z', [ssCtrl], TdxUndoCommand);
  RegisterKeyCommand('Y', [ssCtrl], TdxRedoCommand);
  RegisterKeyCommand(VK_BACK, [ssAlt], TdxUndoCommand);
  RegisterKeyCommand(VK_BACK, [ssAlt, ssShift], TdxRedoCommand);

  RegisterKeyCommand('C', [ssCtrl, ssAlt], TdxInsertCopyrightSymbolCommand);
  RegisterKeyCommand('R', [ssCtrl, ssAlt], TdxInsertRegisteredTrademarkSymbolCommand);
  RegisterKeyCommand('T', [ssCtrl, ssAlt], TdxInsertTrademarkSymbolCommand);
end;

{ TdxSimpleRichEditKeyboardController }

function TdxSimpleRichEditKeyboardController.CreateDefaultHandler: IdxKeyboardHandlerService;
begin
  Result := TdxSimpleRichEditKeyboardHandler.Create(Self);
end;

{ TdxSimpleRichEditDocumentCache }

function TdxSimpleRichEditDocumentCache.CreateParagraphFormattingCache(
  ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache;
begin
  Result := TdxSimpleRichEditParagraphFormattingCache.Create(ADocumentModel);
end;

function TdxSimpleRichEditDocumentCache.CreateParagraphFormattingInfoCache(
  AUnitConverter: TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache;
begin
  Result := TdxSimpleRichEditParagraphFormattingInfoCache.Create(AUnitConverter);
end;

{ TdxSimpleRichEditParagraphFormattingInfoCache }

function TdxSimpleRichEditParagraphFormattingInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
begin
  Result := CreateDefaultItemMSO2003(AUnitConverter);
end;

{ TdxSimpleRichEditParagraphFormattingCache }

function TdxSimpleRichEditParagraphFormattingCache.CreateDefaultParagraphFormattingInfo(
  ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo;
begin
  Result := TdxSimpleRichEditParagraphFormattingInfoCache.CreateDefaultItemMSO2003(ADocumentModel.UnitConverter);
end;

end.
