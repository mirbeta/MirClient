{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPDFExportDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, dxPSPDFExport, dxPSCore, dxPSEngn, dxPSGlbl, dxPrnDlg, cxTextEdit,
  StdCtrls, cxRadioGroup, cxControls, cxContainer, cxEdit, cxLabel, cxClasses,
  cxLookAndFeelPainters, cxGroupBox, cxPC, cxCheckBox, cxTrackBar, Menus,
  cxButtons, dxPSRes, dxPSForm, dxPSPDFExportCore, cxGraphics, cxMaskEdit,
  cxDropDownEdit, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, dxLayoutControl;

type

  { TdxPSPDFExportDialogForm }

  TdxPSPDFExportDialogForm = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbCompressed: TcxCheckBox;
    cbEmbedFonts: TcxCheckBox;
    cbJpgCompress: TcxCheckBox;
    cbOpenAfterExport: TcxCheckBox;
    edPageRanges: TcxTextEdit;
    rbtnAllPages: TcxRadioButton;
    rbtnCurrentPage: TcxRadioButton;
    rbtnPageRanges: TcxRadioButton;
    tbJpgCompression: TcxTrackBar;
    teAuthor: TcxTextEdit;
    teCreator: TcxTextEdit;
    teKeywords: TcxTextEdit;
    teSubject: TcxTextEdit;
    teTitle: TcxTextEdit;
    cbUseCIDFonts: TcxCheckBox;
    cbAllowChanging: TcxCheckBox;
    cbAllowPrinting: TcxCheckBox;
    cbAllowComments: TcxCheckBox;
    cbAllowContentCopying: TcxCheckBox;
    cbSecurityEnable: TcxCheckBox;
    edUserPassword: TcxTextEdit;
    edOwnerPassword: TcxTextEdit;
    cbAllowDocumentAssembly: TcxCheckBox;
    cbAllowPrintingHiResolution: TcxCheckBox;
    cbxMethod: TcxComboBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup2: TdxLayoutGroup;
    tbsExport: TdxLayoutGroup;
    gbExportSettings: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    tbsPageRange: TdxLayoutGroup;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutGroup11: TdxLayoutGroup;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutGroup13: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    lbDescription: TdxLayoutLabeledItem;
    tbsDocInfo: TdxLayoutGroup;
    dxLayoutGroup16: TdxLayoutGroup;
    lbTitle: TdxLayoutItem;
    lbAuthor: TdxLayoutItem;
    lbSubject: TdxLayoutItem;
    lbKeywords: TdxLayoutItem;
    lbCreator: TdxLayoutItem;
    tbsSecurity: TdxLayoutGroup;
    dxLayoutItem18: TdxLayoutItem;
    gbSecuritySettings: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutGroup17: TdxLayoutGroup;
    lbUserPassword: TdxLayoutItem;
    lbOwnerPassword: TdxLayoutItem;
    lbMethod: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    licbAllowDocumentAssembly: TdxLayoutItem;
    licbAllowPrintingHiResolution: TdxLayoutItem;
    dxLayoutGroup18: TdxLayoutGroup;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lbMaxQuality: TdxLayoutLabeledItem;
    lbMaxCompression: TdxLayoutLabeledItem;
    dxLayoutGroup1: TdxLayoutGroup;
    procedure cbJpgCompressClick(Sender: TObject);
    procedure rbtnPageRangesClick(Sender: TObject);
    procedure edPageRangesKeyPress(Sender: TObject; var Key: Char);
    procedure cbSecurityEnableClick(Sender: TObject);
    procedure cbxMethodPropertiesChange(Sender: TObject);
  protected
    function GetPageRanges: TdxPageRanges;
    procedure LoadSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
    procedure SaveSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
    procedure SetPageRanges(AValue: TdxPageRanges);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadOptions(AOptions: TdxPSPDFReportExportOptions);
    procedure LoadTranslation;
    procedure SaveOptions(AOptions: TdxPSPDFReportExportOptions);
    //
    property PageRanges: TdxPageRanges read GetPageRanges write SetPageRanges;
  end;

function dxPSShowPDFSettingsDialog(AOptions: TdxPSPDFReportExportOptions;
  AIsDesignTime: Boolean = False): Boolean;
implementation

{$R *.dfm}

function dxPSShowPDFSettingsDialog(AOptions: TdxPSPDFReportExportOptions;
  AIsDesignTime: Boolean = False): Boolean;
begin
  with TdxPSPDFExportDialogForm.Create(nil) do
  try
    tbsPageRange.Visible := not AIsDesignTime;
    LoadOptions(AOptions);
    Result := ShowModal = mrOk;
    if Result then
      SaveOptions(AOptions);
  finally
    Free;
  end;
end;

{ TdxPSPDFExportDialogForm }

constructor TdxPSPDFExportDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  cbSecurityEnableClick(nil);
  LoadTranslation;
end;

procedure TdxPSPDFExportDialogForm.cbSecurityEnableClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to gbSecuritySettings.Count - 1 do
    gbSecuritySettings.Items[I].Enabled := cbSecurityEnable.Checked;
end;

procedure TdxPSPDFExportDialogForm.cbxMethodPropertiesChange(Sender: TObject);
begin
  licbAllowDocumentAssembly.Visible := cbxMethod.ItemIndex = 1;
  licbAllowPrintingHiResolution.Visible := cbxMethod.ItemIndex = 1;
end;

procedure TdxPSPDFExportDialogForm.LoadTranslation;
begin
  Caption := cxGetResourceString(@sdxPDFDialogCaption);
  gbExportSettings.Caption := cxGetResourceString(@sdxPDFDialogExportSettings);
  gbSecuritySettings.Caption := cxGetResourceString(@sdxPDFDialogSecuritySettings);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnOk.Caption := cxGetResourceString(@sdxBtnOK);

  tbsExport.Caption := cxGetResourceString(@sdxPDFDialogTabExport);
  tbsDocInfo.Caption := cxGetResourceString(@sdxPDFDialogTabDocInfo);
  tbsPageRange.Caption := cxGetResourceString(@sdxPDFDialogTabPages);
  tbsSecurity.Caption := cxGetResourceString(@sdxPDFDialogTabSecurity);

  cbCompressed.Caption := cxGetResourceString(@sdxPDFDialogCompressed);
  cbEmbedFonts.Caption := cxGetResourceString(@sdxPDFDialogEmbedFonts);
  cbJpgCompress.Caption := cxGetResourceString(@sdxPDFDialogUseJPEGCompression);
  cbOpenAfterExport.Caption := cxGetResourceString(@sdxPDFDialogOpenAfterExport);
  cbUseCIDFonts.Caption := cxGetResourceString(@sdxPDFDialogUseCIDFonts);

  lbAuthor.Caption := cxGetResourceString(@sdxPDFDialogAuthor);
  lbCreator.Caption := cxGetResourceString(@sdxPDFDialogCreator);
  lbKeywords.Caption := cxGetResourceString(@sdxPDFDialogKeywords);
  lbMaxCompression.Caption := cxGetResourceString(@sdxPDFDialogMaxCompression);
  lbMaxQuality.Caption := cxGetResourceString(@sdxPDFDialogMaxQuality);
  lbSubject.Caption := cxGetResourceString(@sdxPDFDialogSubject);
  lbTitle.Caption := cxGetResourceString(@sdxPDFDialogTitle);

  lbDescription.Caption := cxGetResourceString(@sdxPrintDialogRangeLegend);
  rbtnAllPages.Caption := cxGetResourceString(@sdxPrintDialogAll);
  rbtnCurrentPage.Caption := cxGetResourceString(@sdxPrintDialogCurrentPage);
  rbtnPageRanges.Caption := cxGetResourceString(@sdxPrintDialogPages);

  cbSecurityEnable.Caption := cxGetResourceString(@sdxPDFDialogSecurityEnabled);
  lbUserPassword.Caption := cxGetResourceString(@sdxPDFDialogSecurityUserPassword);
  lbOwnerPassword.Caption := cxGetResourceString(@sdxPDFDialogSecurityOwnerPassword);
  lbMethod.Caption := cxGetResourceString(@sdxPDFDialogSecurityMethod);
  cbAllowPrinting.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowPrint);
  cbAllowChanging.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowChanging);
  cbAllowComments.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowComments);
  cbAllowContentCopying.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowCopy);
  cbAllowDocumentAssembly.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowDocumentAssemble);
  cbAllowPrintingHiResolution.Caption := cxGetResourceString(@sdxPDFDialogSecurityAllowPrintHiResolution);
end;

procedure TdxPSPDFExportDialogForm.LoadOptions(AOptions: TdxPSPDFReportExportOptions);
begin
  teTitle.Text := AOptions.Title;
  teAuthor.Text := AOptions.Author;
  teSubject.Text := AOptions.Subject;
  teCreator.Text := AOptions.Creator;
  teKeywords.Text := AOptions.Keywords;

  PageRanges := AOptions.PageRangeInfo.PageRanges;
  edPageRanges.Text := AOptions.PageRangeInfo.PageIndexesAsString;
  cbOpenAfterExport.Checked := AOptions.OpenDocumentAfterExport;
  cbCompressed.Checked := AOptions.CompressStreams;
  cbCompressed.Enabled :=  dxPDFCanCompressStreams;
  cbEmbedFonts.Checked := AOptions.EmbedFonts;
  tbJpgCompression.Position := AOptions.JPEGQuality;
  cbJpgCompress.Checked := AOptions.UseJPEGCompression;
  cbJpgCompress.Enabled := dxPDFCanUseJPEGCompression;
  cbUseCIDFonts.Checked := AOptions.UseCIDFonts;

  LoadSecurityOptions(AOptions.SecurityOptions);
  cbJpgCompressClick(nil);
  rbtnPageRangesClick(nil);
end;

procedure TdxPSPDFExportDialogForm.SaveOptions(AOptions: TdxPSPDFReportExportOptions);
begin
  AOptions.Title := teTitle.Text;
  AOptions.Author := teAuthor.Text;
  AOptions.Subject := teSubject.Text;
  AOptions.Creator := teCreator.Text;
  AOptions.Keywords := teKeywords.Text;

  AOptions.EmbedFonts := cbEmbedFonts.Checked;
  AOptions.UseCIDFonts := cbUseCIDFonts.Checked;
  AOptions.CompressStreams := cbCompressed.Checked;
  AOptions.UseJPEGCompression := cbJpgCompress.Checked;
  AOptions.OpenDocumentAfterExport := cbOpenAfterExport.Checked;
  AOptions.JPEGQuality := tbJpgCompression.Position;

  AOptions.PageRangeInfo.PageRanges := PageRanges;
  AOptions.PageRangeInfo.PageIndexesAsString := edPageRanges.Text;

  SaveSecurityOptions(AOptions.SecurityOptions);
end;

procedure TdxPSPDFExportDialogForm.LoadSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
begin
  cbSecurityEnable.Checked := AOptions.Enabled;
  cbxMethod.ItemIndex := Ord(AOptions.KeyLength);
  cbAllowPrinting.Checked := pdaPrint in AOptions.AllowActions;
  cbAllowComments.Checked := pdaComment in AOptions.AllowActions;
  cbAllowChanging.Checked := pdaContentEdit in AOptions.AllowActions;
  cbAllowContentCopying.Checked := pdaContentCopy in AOptions.AllowActions;
  cbAllowDocumentAssembly.Checked := pdaDocumentAssemble in AOptions.AllowActions;
  cbAllowPrintingHiResolution.Checked := pdaPrintHighResolution in AOptions.AllowActions;
  edOwnerPassword.Text := AOptions.OwnerPassword;
  edUserPassword.Text := AOptions.UserPassword;
  cbSecurityEnableClick(nil);
end;

procedure TdxPSPDFExportDialogForm.SaveSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
var
  AAllowActions: TdxPSPDFDocumentActions;
begin
  AAllowActions := [];
  AOptions.Enabled := cbSecurityEnable.Checked;
  AOptions.KeyLength := TdxPSPDFEncryptKeyLength(cbxMethod.ItemIndex);
  if cbAllowPrinting.Checked then
    Include(AAllowActions, pdaPrint);
  if cbAllowComments.Checked then
    Include(AAllowActions, pdaComment);
  if cbAllowChanging.Checked then
    Include(AAllowActions, pdaContentEdit);
  if cbAllowContentCopying.Checked then
    Include(AAllowActions, pdaContentCopy);
  if cbAllowDocumentAssembly.Checked then
    Include(AAllowActions, pdaDocumentAssemble);
  if cbAllowPrintingHiResolution.Checked then
    Include(AAllowActions, pdaPrintHighResolution);
  AOptions.AllowActions := AAllowActions;
  AOptions.OwnerPassword := edOwnerPassword.Text;
  AOptions.UserPassword := edUserPassword.Text;
end;

function TdxPSPDFExportDialogForm.GetPageRanges: TdxPageRanges;
begin
  Result := prAll;
  if rbtnPageRanges.Checked then Result := prRange;
  if rbtnCurrentPage.Checked then Result := prCurrent;
end;

procedure TdxPSPDFExportDialogForm.SetPageRanges(AValue: TdxPageRanges);
begin
  rbtnAllPages.Checked := AValue = prAll;
  rbtnPageRanges.Checked := AValue = prRange;
  rbtnCurrentPage.Checked := AValue = prCurrent;
end;

procedure TdxPSPDFExportDialogForm.cbJpgCompressClick(Sender: TObject);
begin
  lbMaxQuality.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
  lbMaxCompression.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
  tbJpgCompression.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
end;

procedure TdxPSPDFExportDialogForm.rbtnPageRangesClick(Sender: TObject);
begin
  edPageRanges.Enabled := rbtnPageRanges.Checked;
end;

procedure TdxPSPDFExportDialogForm.edPageRangesKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) <> VK_BACK then
  begin
    case Key of
      '0'..'9', cPageRangeSeparator, cPageSeparator: ;
      else
        Key := #0;
    end;
  end;
end;

end.
