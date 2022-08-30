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

unit dxRichEdit.Dialogs.TableOfContents;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, StdCtrls, Controls, Forms, Dialogs,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer,
  dxLayoutControl, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit, cxCheckBox,
  dxLayoutControlAdapters, cxButtons,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Control,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Dialogs.TableOfContentsController;

type
  TdxRichEditTableOfContentsForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbRightAlignPageNumbers: TcxCheckBox;
    cbShowPageNumbers: TcxCheckBox;
    cbUseHyperlinks: TcxCheckBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    liEditShowLevels: TdxLayoutItem;
    liPrintPreview: TdxLayoutLabeledItem;
    PreviewRichEditControl: TdxSimpleRichEditControl;
    spEditShowLevels: TcxSpinEdit;
    procedure cbShowPageNumbersPropertiesChange(Sender: TObject);
    procedure cbRightAlignPageNumbersPropertiesChange(Sender: TObject);
    procedure cbUseHyperlinksPropertiesChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure spEditShowLevelsPropertiesChange(Sender: TObject);
  private
    function GetController: TdxTableOfContentsFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure FillPreviewControl;
    procedure InitializeForm; override;
  public
    property Controller: TdxTableOfContentsFormController read GetController;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Options.Core,
  dxRichEdit.Options,
  dxRichEdit.Types,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.Utils.Types,
  dxCharacters,
  dxMeasurementUnits,
  dxRichEdit.Commands.Selection;

{$R *.dfm}

{ TdxRichEditTableOfContentsForm }

procedure TdxRichEditTableOfContentsForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditTableOfContentsForm);
  cbShowPageNumbers.Caption := cxGetResourceString(@sdxRichEditTableOfContentsShowPageNumbers);
  cbRightAlignPageNumbers.Caption := cxGetResourceString(@sdxRichEditTableOfContentsRightAlignPageNumbers);
  cbUseHyperlinks.Caption := cxGetResourceString(@sdxRichEditTableOfContentsUseHyperlinks);
  liPrintPreview.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableOfContentsPrintPreview);
  liEditShowLevels.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableOfContentsEditShowLevels);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
end;

function TdxRichEditTableOfContentsForm.GetController: TdxTableOfContentsFormController;
begin
  Result := TdxTableOfContentsFormController(inherited Controller);
end;

procedure TdxRichEditTableOfContentsForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

procedure TdxRichEditTableOfContentsForm.cbRightAlignPageNumbersPropertiesChange(Sender: TObject);
begin
  Controller.RightAlignPageNumbers := cbRightAlignPageNumbers.Checked;
  FillPreviewControl;
end;

procedure TdxRichEditTableOfContentsForm.cbShowPageNumbersPropertiesChange(Sender: TObject);
begin
  Controller.ShowPageNumbers := cbShowPageNumbers.Checked;
  cbRightAlignPageNumbers.Enabled := Controller.ShowPageNumbers;
  FillPreviewControl;
end;

procedure TdxRichEditTableOfContentsForm.cbUseHyperlinksPropertiesChange(Sender: TObject);
begin
  Controller.UseHyperlinks := cbUseHyperlinks.Checked;
  FillPreviewControl;
end;

function TdxRichEditTableOfContentsForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTableOfContentsFormController.Create(AControllerParameters as TdxTOCFormControllerParameters);
end;

procedure TdxRichEditTableOfContentsForm.FillPreviewControl;
const
  DpiX = 96;
var
  ADocumentModel: TdxDocumentModel;
  APieceTable: TdxPieceTable;
  I: Integer;
  AContent, AListParagraphContent: string;
  ATocStyleIndex: Integer;
  AParagraph: TdxParagraph;
  ATabs: TdxTabFormattingInfo;
  ATabInfo: TdxTabInfo;
  ATabPosition: Integer;
  ALeftIndent: Integer;
  AStartOfDocumentCommand: TdxStartOfDocumentCommand;
begin
  PreviewRichEditControl.CreateNewDocument;
  ADocumentModel := PreviewRichEditControl.DocumentModel;
  APieceTable := ADocumentModel.MainPieceTable;
  AListParagraphContent := cxGetResourceString(@sdxRichEditTableOfContentsListParagraphContent);
  ATabPosition := ADocumentModel.UnitConverter.PixelsToModelUnits(PreviewRichEditControl.Width - 50, DpiX);
  ALeftIndent := ADocumentModel.UnitConverter.PixelsToModelUnits(20, DpiX);
  for I := 0 to Controller.ShowLevels - 1 do
  begin
    ADocumentModel.ParagraphStyles.CreateTocStyle(I + 1);
    AContent := Format('%s%d' + TdxCharacters.TabMark, [AListParagraphContent, I + 1]);
    if I > 0 then
      APieceTable.InsertParagraph(APieceTable.DocumentEndLogPosition);

    APieceTable.InsertPlainText(APieceTable.DocumentEndLogPosition, AContent);
    if Controller.ShowPageNumbers then
    begin
      if Controller.RightAlignPageNumbers then
        APieceTable.InsertPlainText(APieceTable.DocumentEndLogPosition, #9);
      AContent := IntToStr(I * 2 + 1);
      APieceTable.InsertPlainText(APieceTable.DocumentEndLogPosition, AContent);
    end;
  end;

  for I := 0 to Controller.ShowLevels - 1 do
  begin
    ATocStyleIndex := ADocumentModel.ParagraphStyles.GetTocStyle(I + 1);
    AParagraph := APieceTable.Paragraphs[I];
    AParagraph.ParagraphStyleIndex := ATocStyleIndex;
    AParagraph.LeftIndent := AParagraph.LeftIndent + ALeftIndent;
    if Controller.ShowPageNumbers and Controller.RightAlignPageNumbers then
    begin
      ATabs := AParagraph.GetOwnTabs;
      try
        ATabInfo := TdxTabInfo.Create(ATabPosition, TdxTabAlignmentType.Right, TdxTabLeaderType.Dots, False, False);
        ATabs.Add(ATabInfo);
        AParagraph.SetOwnTabs(ATabs);
      finally
        ATabs.Free;
      end;
    end;
  end;

  AStartOfDocumentCommand := TdxStartOfDocumentCommand.Create(PreviewRichEditControl);
  try
    AStartOfDocumentCommand.Execute;
  finally
    AStartOfDocumentCommand.Free;
  end;
  PreviewRichEditControl.ReadOnly := True;
end;

procedure TdxRichEditTableOfContentsForm.InitializeForm;
begin
  cbRightAlignPageNumbers.Checked := Controller.RightAlignPageNumbers;
  cbShowPageNumbers.Checked := Controller.ShowPageNumbers;
  cbUseHyperlinks.Checked := Controller.UseHyperlinks;
  spEditShowLevels.Value := Controller.ShowLevels;
  PreviewRichEditControl.WantTabs := False;
  PreviewRichEditControl.ShowCaretInReadOnly := False;
  PreviewRichEditControl.Options.VerticalScrollbar.Visibility := TdxRichEditScrollbarVisibility.Auto;
  PreviewRichEditControl.Options.Behavior.Drag := TdxDocumentCapability.Disabled;
  PreviewRichEditControl.Options.Behavior.Drop := TdxDocumentCapability.Disabled;
  PreviewRichEditControl.Options.Behavior.ShowPopupMenu := TdxDocumentCapability.Disabled;
  PreviewRichEditControl.EnableSelection := False;
  FillPreviewControl;
end;

procedure TdxRichEditTableOfContentsForm.spEditShowLevelsPropertiesChange(Sender: TObject);
begin
  Controller.ShowLevels := spEditShowLevels.Value;
  FillPreviewControl;
end;

end.
