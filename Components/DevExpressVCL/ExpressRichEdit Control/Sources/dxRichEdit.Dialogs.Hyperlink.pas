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

unit dxRichEdit.Dialogs.Hyperlink;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Menus,
  Controls, Forms, Dialogs, Generics.Defaults, Generics.Collections, cxGraphics, cxRadioGroup,
  dxCore, cxControls, cxLookAndFeels, cxButtonEdit, cxLookAndFeelPainters, dxLayoutLookAndFeels,
  cxClasses, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutControlAdapters, cxLabel, cxButtons,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.HyperlinkFormController;

type
  TdxRichEditHyperlinkDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cmblBookmark: TcxComboBox;
    cmbTargetFrame: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1SeparatorItem1: TdxLayoutSeparatorItem;
    edtEditAddress: TcxButtonEdit;
    edtText: TcxTextEdit;
    edtTooltip: TcxTextEdit;
    liAddress: TdxLayoutItem;
    lilBookmark: TdxLayoutItem;
    lilLinkTo: TdxLayoutItem;
    liTarget: TdxLayoutItem;
    liText: TdxLayoutItem;
    liTooltip: TdxLayoutItem;
    rbLinkToDocument: TcxRadioButton;
    rbLinkToWebPage: TcxRadioButton;
    procedure btnOkClick(Sender: TObject);
    procedure edtEditAddressPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  strict private
    class var
      FTargetFrames: TStrings;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateTargetFrameDescriptions: TStrings; static;
    class function CreateTargetFrames: TStrings; static;
  private
    FTargetFrameDescriptions: TStrings;
    function GetController: TdxHyperlinkFormController;
    function GetIsAddressActive: Boolean;
    function GetLinkToSelectedIndex: Integer;
    procedure SetLinkToSelectedIndex(const Value: Integer);

    procedure PopulateBookmarkList;
    procedure PopulateTargetFrameList;

    procedure BookmarkPropertiesChange(Sender: TObject);
    procedure EditAddressPropertiesChange(Sender: TObject);
    procedure LinkToSelectedIndexChanged(Sender: TObject);
    procedure TargetFramePropertiesChange(Sender: TObject);
    procedure TextPropertiesChange(Sender: TObject);
    procedure TooltipPropertiesChange(Sender: TObject);
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    procedure ApplyBookmarkName;
    procedure ApplyNavigationUri;
    procedure ChangePanelsVisibility(AIsAddressActive: Boolean);
    function ShouldEnableOKButton: Boolean;
    procedure SwitchToAddress;
    procedure SwitchToBookmark;
    procedure UpdateAddress;
    procedure UpdateAddressPanelVisibility;
    procedure UpdateOKButtonEnabling;
    procedure UpdateTargetFrame;
    procedure UpdateTextToDisplay;
    procedure UpdateTooltip;
    procedure UpdateFormCore; override;

    property IsAddressActive: Boolean read GetIsAddressActive;
    property LinkToSelectedIndex: Integer read GetLinkToSelectedIndex write SetLinkToSelectedIndex;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    destructor Destroy; override;
    property Controller: TdxHyperlinkFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Strs,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.Dialogs.BookmarkFormController;

{$R *.dfm}

{ TdxRichEditHyperlinkDialogForm }

destructor TdxRichEditHyperlinkDialogForm.Destroy;
begin
  FreeAndNil(FTargetFrameDescriptions);
  inherited Destroy;
end;

function TdxRichEditHyperlinkDialogForm.GetController: TdxHyperlinkFormController;
begin
  Result := TdxHyperlinkFormController(inherited Controller);
end;

function TdxRichEditHyperlinkDialogForm.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.InnerControl.DocumentModel;
end;

procedure TdxRichEditHyperlinkDialogForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

procedure TdxRichEditHyperlinkDialogForm.BookmarkPropertiesChange(Sender: TObject);
begin
  ApplyBookmarkName;
  UpdateOKButtonEnabling;
end;

function TdxRichEditHyperlinkDialogForm.GetIsAddressActive: Boolean;
begin
  Result := LinkToSelectedIndex = 0;
end;

function TdxRichEditHyperlinkDialogForm.GetLinkToSelectedIndex: Integer;
begin
  if rbLinkToWebPage.Checked then
    Result := 0
  else
    if rbLinkToDocument.Checked then
      Result := 1
    else
      Result := -1;
end;

procedure TdxRichEditHyperlinkDialogForm.SetLinkToSelectedIndex(const Value: Integer);
begin
  rbLinkToWebPage.Checked := Value = 0;
  rbLinkToDocument.Checked := Value = 1;
end;

procedure TdxRichEditHyperlinkDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditHyperlinkDialogForm);
  rbLinkToWebPage.Caption := cxGetResourceString(@sdxRichEditHyperlinkDialogLinkToWebPage);
  rbLinkToDocument.Caption := cxGetResourceString(@sdxRichEditHyperlinkDialogLinkToDocument);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  liAddress.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogAddress);
  liText.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogText);
  liTooltip.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogTooltip);
  liTarget.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogTarget);
  lilLinkTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogLinkTo);
  lilBookmark.CaptionOptions.Text := cxGetResourceString(@sdxRichEditHyperlinkDialogBookmark);
end;

function TdxRichEditHyperlinkDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxHyperlinkFormController.Create(AControllerParameters as TdxHyperlinkFormControllerParameters);
end;

procedure TdxRichEditHyperlinkDialogForm.InitializeForm;
begin
  FTargetFrameDescriptions := CreateTargetFrameDescriptions;
  PopulateBookmarkList;
  PopulateTargetFrameList;
end;

procedure TdxRichEditHyperlinkDialogForm.SubscribeControlsEvents;
begin
  edtText.Properties.OnChange := TextPropertiesChange;
  edtTooltip.Properties.OnChange := TooltipPropertiesChange;
  cmbTargetFrame.Properties.OnChange := TargetFramePropertiesChange;
  rbLinkToWebPage.OnClick := LinkToSelectedIndexChanged;
  rbLinkToDocument.OnClick := LinkToSelectedIndexChanged;
  cmblBookmark.Properties.OnChange := BookmarkPropertiesChange;
  edtEditAddress.Properties.OnChange := EditAddressPropertiesChange;
end;

procedure TdxRichEditHyperlinkDialogForm.UnsubscribeControlsEvents;
begin
  edtText.Properties.OnChange := nil;
  edtTooltip.Properties.OnChange := nil;
  cmbTargetFrame.Properties.OnChange := nil;
  rbLinkToWebPage.OnClick := nil;
  rbLinkToDocument.OnClick := nil;
  cmblBookmark.Properties.OnChange := nil;
  edtEditAddress.Properties.OnChange := nil;
end;

procedure TdxRichEditHyperlinkDialogForm.ApplyBookmarkName;
begin
  if cmblBookmark.ItemIndex = 0 then
    Controller.Anchor := ''
  else
    Controller.Anchor := cmblBookmark.Text;
end;

procedure TdxRichEditHyperlinkDialogForm.ApplyNavigationUri;
begin
  Controller.NavigateUri := edtEditAddress.Text;
end;

procedure TdxRichEditHyperlinkDialogForm.ChangePanelsVisibility(AIsAddressActive: Boolean);
begin
  dxLayoutControl1.BeginUpdate;
  try
    lilBookmark.Visible := not AIsAddressActive;
    liAddress.Visible := AIsAddressActive;
  finally
    dxLayoutControl1.EndUpdate;
  end;
end;

procedure TdxRichEditHyperlinkDialogForm.TargetFramePropertiesChange(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := FTargetFrameDescriptions.IndexOf(cmbTargetFrame.Text);
  if AIndex = -1 then
    Controller.Target := cmbTargetFrame.Text
  else
    Controller.Target := FTargetFrames[AIndex];
end;

class function TdxRichEditHyperlinkDialogForm.CreateTargetFrameDescriptions: TStrings;
begin
  Result := TStringList.Create;
  Result.Add(cxGetResourceString(@sdxRichEditHyperlinkDialogTargetFrameDescription_Blank));
  Result.Add(cxGetResourceString(@sdxRichEditHyperlinkDialogTargetFrameDescription_Parent));
  Result.Add(cxGetResourceString(@sdxRichEditHyperlinkDialogTargetFrameDescription_Self));
  Result.Add(cxGetResourceString(@sdxRichEditHyperlinkDialogTargetFrameDescription_Top));
end;

class function TdxRichEditHyperlinkDialogForm.CreateTargetFrames: TStrings;
begin
  Result := TStringList.Create;
  Result.Add('_blank');
  Result.Add('_parent');
  Result.Add('_self');
  Result.Add('_top');
end;

procedure TdxRichEditHyperlinkDialogForm.EditAddressPropertiesChange(Sender: TObject);
begin
  ApplyNavigationUri;
  UpdateOKButtonEnabling;
end;

procedure TdxRichEditHyperlinkDialogForm.edtEditAddressPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  AFileDialog: TdxOpenFileDialog;
  ADescription: string;
begin
  AFileDialog := TdxOpenFileDialog.Create(Self);
  try
    ADescription := cxGetResourceString(@sdxRichEditFileFilterDescription_AllFiles);
    AFileDialog.Filter := Format('%s|*.*', [ADescription]);
    if AFileDialog.Execute then
      edtEditAddress.Text := AFileDialog.FileName;
  finally
    AFileDialog.Free;
  end;
end;

procedure TdxRichEditHyperlinkDialogForm.TooltipPropertiesChange(Sender: TObject);
begin
  Controller.ToolTip := edtTooltip.Text;
end;

procedure TdxRichEditHyperlinkDialogForm.TextPropertiesChange(Sender: TObject);
begin
  Controller.TextToDisplay := edtText.Text;
end;

class destructor TdxRichEditHyperlinkDialogForm.Finalize;
begin
  FreeAndNil(FTargetFrames);
end;

class constructor TdxRichEditHyperlinkDialogForm.Initialize;
begin
  FTargetFrames := CreateTargetFrames;
end;

procedure TdxRichEditHyperlinkDialogForm.PopulateBookmarkList;
var
  ABookmarks: TdxBookmarkList;
  AComparer: TdxBookmarkNameComparer;
  I: Integer;
begin
  cmblBookmark.Properties.Items.Add(cxGetResourceString(@sdxRichEditHyperlinkSelectedBookmarkNone));

  ABookmarks := DocumentModel.GetBookmarks;
  try
    AComparer := TdxBookmarkNameComparer.Create;
    try
      ABookmarks.Sort(AComparer);
    finally
      AComparer.Free;
    end;
    for I := 0 to ABookmarks.Count - 1 do
      cmblBookmark.Properties.Items.Add(ABookmarks[I].Name);
  finally
    ABookmarks.Free;
  end;
  cmblBookmark.ItemIndex := 0;
end;

procedure TdxRichEditHyperlinkDialogForm.PopulateTargetFrameList;
begin
  Populate(cmbTargetFrame, procedure(ACombobox: TcxCustomComboBox)
    var
      ADescription: string;
    begin
      for ADescription in FTargetFrameDescriptions do
        ACombobox.Properties.Items.Add(ADescription);
    end);
end;

procedure TdxRichEditHyperlinkDialogForm.LinkToSelectedIndexChanged(Sender: TObject);
begin
  if IsAddressActive then
    Controller.UriType := TdxHyperlinkUriType.Url
  else
    Controller.UriType := TdxHyperlinkUriType.Anchor;

  UpdateAddressPanelVisibility;
  UpdateOKButtonEnabling;
end;

function TdxRichEditHyperlinkDialogForm.ShouldEnableOKButton: Boolean;
begin
  if IsAddressActive then
    Result := edtEditAddress.Text <> ''
  else
    Result := cmblBookmark.ItemIndex > 0;
end;

procedure TdxRichEditHyperlinkDialogForm.SwitchToAddress;
begin
  LinkToSelectedIndex := 0;
  ChangePanelsVisibility(True);
end;

procedure TdxRichEditHyperlinkDialogForm.SwitchToBookmark;
begin
  LinkToSelectedIndex := 1;
  ChangePanelsVisibility(False);
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateAddress;
var
  AIndex: Integer;
begin
  if Controller.UriType = TdxHyperlinkUriType.Anchor then
  begin
    AIndex := cmblBookmark.Properties.Items.IndexOf(Controller.Anchor);

    SwitchToBookmark;
    cmblBookmark.ItemIndex := AIndex;
  end
  else
  begin
    SwitchToAddress;
    edtEditAddress.Text := Controller.NavigateUri;
  end;
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateAddressPanelVisibility;
var
  AVisible: Boolean;
begin
  AVisible := Controller.UriType = TdxHyperlinkUriType.Url;
  ChangePanelsVisibility(AVisible);
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateOKButtonEnabling;
begin
  btnOk.Enabled := ShouldEnableOKButton;
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateTargetFrame;
var
  AIndex: Integer;
begin
  AIndex := FTargetFrames.IndexOf(Controller.Target);
  if AIndex = -1 then
    cmbTargetFrame.Text := Controller.Target
  else
    cmbTargetFrame.Text := FTargetFrameDescriptions[AIndex];
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateTextToDisplay;
var
  AText: string;
begin
  if Controller.CanChangeDisplayText then
    edtText.Text := Controller.TextToDisplay
  else
  begin
    AText := cxGetResourceString(@sdxRichEditHyperlinkDialogSelectionInDocument);
    edtText.Enabled := False;
    edtText.Text := AText;
  end;
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateTooltip;
begin
  edtTooltip.Text := Controller.ToolTip;
end;

procedure TdxRichEditHyperlinkDialogForm.UpdateFormCore;
begin
  UpdateTooltip;
  UpdateAddress;
  UpdateTextToDisplay;

  UpdateTargetFrame;
end;

end.
