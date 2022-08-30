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

unit dxRichEdit.Dialogs.EventArgs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Controls,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.View.Core,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.ParagraphFormController,
  dxRichEdit.Dialogs.NumberingFormController,
  dxRichEdit.Dialogs.SymbolFormController,
  dxRichEdit.Dialogs.FontsFormController,
  dxRichEdit.Dialogs.TablePropertiesFormController,
  dxRichEdit.Dialogs.TableOptionsController,
  dxRichEdit.Dialogs.TabsFormController,
  dxRichEdit.Dialogs.InsertTableFormController,
  dxRichEdit.Dialogs.SplitTableCellsFormController,
  dxRichEdit.Dialogs.HyperlinkFormController,
  dxRichEdit.Dialogs.InsertDeleteTableCellsFormController,
  dxRichEdit.Dialogs.ColumnsSetupFormController,
  dxRichEdit.Dialogs.EditStyleController,
  dxRichEdit.Dialogs.FloatingObjectLayoutFormController,
  dxRichEdit.Dialogs.FindAndReplaceFormHelpers,
  dxRichEdit.Dialogs.LineNumberingController,
  dxRichEdit.Dialogs.PageSetupController,
  dxRichEdit.Dialogs.RangeEditingPermissionsFormController,
  dxRichEdit.Dialogs.BookmarkFormController,
  dxRichEdit.Dialogs.MergeOptionsController,
  dxRichEdit.Dialogs.TableStyleFormController,
  dxRichEdit.Dialogs.FormControllers;

type

  { TdxShowFormEventArgs }

  TdxShowFormEventArgs = class(TdxEventArgs)
  private
    FHandled: Boolean;
    FDialogResult: TModalResult;
    FParent: TCustomControl;
  public
    property DialogResult: TModalResult read FDialogResult write FDialogResult;
    property Parent: TCustomControl read FParent write FParent;
    property Handled: Boolean read FHandled write FHandled;
  end;

  { TdxFormShowingEventArgs }

  TdxFormShowingEventArgs = class abstract(TdxShowFormEventArgs)
  public
    constructor Create(AControllerParameters: TdxFormControllerParameters);
  end;

  TdxBaseFormShowingEventArgs<T: class> = class(TdxFormShowingEventArgs)
  private
    FControllerParameters: T;
  public
    constructor Create(AControllerParameters: T);
    property ControllerParameters: T read FControllerParameters;
  end;

  { TdxParagraphFormShowingEventArgs }

  TdxParagraphFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxParagraphFormControllerParameters>);
  TdxParagraphFormShowingEvent = procedure(Sender: TObject; const Args: TdxParagraphFormShowingEventArgs) of object;

  { TdxNumberingListFormShowingEventArgs }

  TdxNumberingListFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxNumberingListFormControllerParameters>);
  TdxNumberingListFormShowingEvent = procedure(Sender: TObject; const Args: TdxNumberingListFormShowingEventArgs) of object;

  { TdxSymbolFormShowingEventArgs }

  TdxSymbolFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxRichEditInsertSymbolControllerParameters>);
  TdxSymbolFormShowingEvent = procedure(Sender: TObject; const Args: TdxSymbolFormShowingEventArgs) of object;

  { TdxFontFormShowingEventArgs }

  TdxFontFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxFontFormControllerParameters>);
  TdxFontFormShowingEvent = procedure(Sender: TObject; const Args: TdxFontFormShowingEventArgs) of object;

  { TdxTablePropertiesFormShowingEventArgs }

  TdxTablePropertiesFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxTablePropertiesFormControllerParameters>);
  TdxTablePropertiesFormShowingEvent = procedure(Sender: TObject; const Args: TdxTablePropertiesFormShowingEventArgs) of object;

  { TdxTableOptionsFormShowingEventArgs }

  TdxTableOptionsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxTableOptionsFormControllerParameters>);
  TdxTableOptionsFormShowingEvent = procedure(Sender: TObject; const Args: TdxTableOptionsFormShowingEventArgs) of object;

  { TdxTabsFormShowingEventArgs }

  TdxTabsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxTabsFormControllerParameters>);
  TdxTabsFormShowingEvent = procedure(Sender: TObject; const Args: TdxTabsFormShowingEventArgs) of object;

  { TdxTabsFormShowingEventArgs }

  TdxInsertTableFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxInsertTableFormControllerParameters>);
  TdxInsertTableFormShowingEvent = procedure(Sender: TObject; const Args: TdxInsertTableFormShowingEventArgs) of object;

  { TdxSplitTableCellsFormShowingEventArgs }

  TdxSplitTableCellsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxSplitTableCellsFormControllerParameters>);
  TdxSplitTableCellsFormShowingEvent = procedure(Sender: TObject; const Args: TdxSplitTableCellsFormShowingEventArgs) of object;

  { TdxHyperlinkFormShowingEventArgs }

  TdxHyperlinkFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxHyperlinkFormControllerParameters>);
  TdxHyperlinkFormShowingEvent = procedure(Sender: TObject; const Args: TdxHyperlinkFormShowingEventArgs) of object;

  { TdxDeleteTableCellsFormShowingEventArgs }

  TdxDeleteTableCellsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxDeleteTableCellsFormControllerParameters>);
  TdxDeleteTableCellsFormShowingEvent = procedure(Sender: TObject; const Args: TdxDeleteTableCellsFormShowingEventArgs) of object;

  { TdxInsertTableCellsFormShowingEventArgs }

  TdxInsertTableCellsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxInsertTableCellsFormControllerParameters>);
  TdxInsertTableCellsFormShowingEvent = procedure(Sender: TObject; const Args: TdxInsertTableCellsFormShowingEventArgs) of object;

  { TdxInsertTableCellsFormShowingEventArgs }

  TdxColumnsSetupFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxColumnsSetupFormControllerParameters>);
  TdxColumnsSetupFormShowingEvent = procedure(Sender: TObject; const Args: TdxColumnsSetupFormShowingEventArgs) of object;

  { TdxEditStyleFormShowingEventArgs }

  TdxEditStyleFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxEditStyleFormControllerParameters>);
  TdxEditStyleFormShowingEvent = procedure(Sender: TObject; const Args: TdxEditStyleFormShowingEventArgs) of object;

  { TdxFloatingObjectLayoutOptionsFormShowingEventArgs }

  TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxFloatingInlineObjectLayoutOptionsFormControllerParameters>);
  TdxFloatingInlineObjectLayoutOptionsFormShowingEvent = procedure(Sender: TObject; const Args: TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs) of object;

  { TdxSearchFormShowingEventArgs }

  TdxSearchFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxFormControllerParameters>);
  TdxSearchFormShowingEvent = procedure(Sender: TObject; const Args: TdxSearchFormShowingEventArgs) of object;

  { TdxLineNumberingFormShowingEventArgs }

  TdxLineNumberingFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxLineNumberingFormControllerParameters>);
  TdxLineNumberingFormShowingEvent = procedure(Sender: TObject; const Args: TdxLineNumberingFormShowingEventArgs) of object;

  { TdxPageSetupFormShowingEventArgs }

  TdxPageSetupFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxPageSetupFormControllerParameters>);
  TdxPageSetupFormShowingEvent = procedure(Sender: TObject; const Args: TdxPageSetupFormShowingEventArgs) of object;

  { TdxInsertMergeFieldFormShowingEventArgs }

  TdxInsertMergeFieldFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxInsertMergeFieldFormControllerParameters>);
  TdxInsertMergeFieldFormShowingEvent = procedure(Sender: TObject; const Args: TdxInsertMergeFieldFormShowingEventArgs) of object;

  { TdxBookmarkFormShowingEventArgs }

  TdxBookmarkFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxBookmarkFormControllerParameters>);
  TdxBookmarkFormShowingEvent = procedure(Sender: TObject; const Args: TdxBookmarkFormShowingEventArgs) of object;

  { TdxMergeDatabaseRecordsFormShowingEventArgs }

  TdxMergeDatabaseRecordsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxMergeOptionsFormControllerParameters>);
  TdxMergeDatabaseRecordsFormShowingEvent = procedure(Sender: TObject; const Args: TdxMergeDatabaseRecordsFormShowingEventArgs) of object;

  { TdxTableStyleFormShowingEventArgs }

  TdxTableStyleFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxTableStyleFormControllerParameters>);
  TdxTableStyleFormShowingEvent = procedure(Sender: TObject; const Args: TdxTableStyleFormShowingEventArgs) of object;

  { TdxDocumentProtectionQueryNewPasswordFormShowingEventArgs }

  TdxDocumentProtectionSetPasswordFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxDocumentProtectionSetPasswordFormControllerParameters>);
  TdxDocumentProtectionSetPasswordFormShowingEvent = procedure(Sender: TObject; const Args: TdxDocumentProtectionSetPasswordFormShowingEventArgs) of object;

  { TdxDocumentProtectionQueryPasswordFormShowingEventArgs }

  TdxDocumentProtectionGetPasswordFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxDocumentProtectionGetPasswordFormControllerParameters>);
  TdxDocumentProtectionGetPasswordFormShowingEvent = procedure(Sender: TObject; const Args: TdxDocumentProtectionGetPasswordFormShowingEventArgs) of object;

  { TdxRangeEditingPermissionsFormShowingEventArgs }

  TdxRangeEditingPermissionsFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxRangeEditingPermissionsFormControllerParameters>);
  TdxRangeEditingPermissionsFormShowingEvent = procedure(Sender: TObject; const Args: TdxRangeEditingPermissionsFormShowingEventArgs) of object;

  { TdxDocumentProtectionQueryNewPasswordFormShowingEventArgs }

  TdxDocumentEncryptSetPasswordFormShowingEventArgs = class(TdxBaseFormShowingEventArgs<TdxDocumentEncryptQueryPasswordFormControllerParameters>);
  TdxDocumentEncryptSetPasswordFormShowingEvent = procedure(Sender: TObject; const Args: TdxDocumentEncryptSetPasswordFormShowingEventArgs) of object;

implementation

{ TdxFormShowingEventArgs }

constructor TdxFormShowingEventArgs.Create(AControllerParameters: TdxFormControllerParameters);
begin
  inherited Create;
  Assert(AControllerParameters <> nil);
  Parent := AControllerParameters.Control.Control as TCustomControl;
end;

{ TdxBaseFormShowingEventArgs<T> }

constructor TdxBaseFormShowingEventArgs<T>.Create(AControllerParameters: T);
begin
  inherited Create(AControllerParameters as TdxFormControllerParameters);
  FControllerParameters := AControllerParameters;
end;

end.
