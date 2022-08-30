{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridWizardTableViewOptionsInplaceEditFormPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxRadioGroup, cxLabel, cxCheckBox, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridTableView, cxGridCustomTableView, dxLayoutLookAndFeels, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxGridInplaceEditForm;

type
  { TcxGridWizardTableViewOptionsInplaceEditFormPageFrame }

  TcxGridWizardTableViewOptionsInplaceEditFormPageFrame = class(TcxGridWizardCustomPageFrame)
    chbUseDefaultLayout: TcxCheckBox;
    lbMasterRowDblClickAction: TcxLabel;
    lbStretch: TcxLabel;
    lciClient: TdxLayoutItem;
    lciDefaultColumnCount: TdxLayoutItem;
    lciHorizontal: TdxLayoutItem;
    lciMasterRowDblClickAction: TdxLayoutItem;
    lciNone: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciShowEditForm: TdxLayoutItem;
    lciStretch: TdxLayoutItem;
    lciSwitchExpandedState: TdxLayoutItem;
    lciUseDefaultLayout: TdxLayoutItem;
    lciVertical: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup6: TdxLayoutGroup;
    lcMainGroup7: TdxLayoutGroup;
    lcSeparatorItem1: TdxLayoutSeparatorItem;
    lcSeparatorItem2: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    rbClient: TcxRadioButton;
    rbHorizontal: TcxRadioButton;
    rbNone: TcxRadioButton;
    rbShowEditForm: TcxRadioButton;
    rbSwitchExpandedState: TcxRadioButton;
    rbVertical: TcxRadioButton;
    seDefaultColumnCount: TcxSpinEdit;
    procedure RefreshPreviewGrid(Sender: TObject);
    procedure chbUseDefaultLayoutPropertiesChange(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure GridViewChanged; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardTableViewOptionsInplaceEditFormPageFrame }

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.ApplyLocalization;
begin
  lbStretch.Caption := cxGetResourceString(@scxgwInplaceEditFormPageDefaultStretch);

  rbClient.Caption := cxGetResourceString(@scxgwInplaceEditFormPageClient);
  rbClient.Hint := cxGetResourceString(@scxgwInplaceEditFormPageClientHint);

  rbHorizontal.Caption := cxGetResourceString(@scxgwInplaceEditFormPageHorizontal);
  rbHorizontal.Hint := cxGetResourceString(@scxgwInplaceEditFormPageHorizontalHint);

  rbNone.Caption := cxGetResourceString(@scxgwInplaceEditFormPageNone);
  rbNone.Hint := cxGetResourceString(@scxgwInplaceEditFormPageNoneHint);

  rbVertical.Caption := cxGetResourceString(@scxgwInplaceEditFormPageVertical);
  rbVertical.Hint := cxGetResourceString(@scxgwInplaceEditFormPageVerticalHint);

  lbMasterRowDblClickAction.Caption := cxGetResourceString(@scxgwInplaceEditFormPageMasterRowDblClickAction);

  rbShowEditForm.Caption := cxGetResourceString(@scxgwInplaceEditFormPageShowEditForm);
  rbShowEditForm.Hint := cxGetResourceString(@scxgwInplaceEditFormPageShowEditFormHint);

  rbSwitchExpandedState.Caption := cxGetResourceString(@scxgwInplaceEditFormPageSwitchExpandedState);
  rbSwitchExpandedState.Hint := cxGetResourceString(@scxgwInplaceEditFormPageSwitchExpandedStateHint);

  chbUseDefaultLayout.Caption := cxGetResourceString(@scxgwInplaceEditFormPageUseDefaultLayout);
  chbUseDefaultLayout.Hint := cxGetResourceString(@scxgwInplaceEditFormPageUseDefaultLayoutHint);

  lciDefaultColumnCount.Caption := cxGetResourceString(@scxgwInplaceEditFormPageDefaultColumnCount);
  seDefaultColumnCount.Hint := cxGetResourceString(@scxgwInplaceEditFormPageDefaultColumnCountHint);
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.ApplySettings;
var
  AView: TcxGridTableView;
begin
  Helper.RestoreAfterCustomization;
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridTableView(Helper.GridView);
  if rbClient.Checked then
    AView.EditForm.DefaultStretch := fsClient
  else
    if rbHorizontal.Checked then
      AView.EditForm.DefaultStretch := fsHorizontal
    else
      if rbNone.Checked then
        AView.EditForm.DefaultStretch := fsNone
      else
        AView.EditForm.DefaultStretch := fsVertical;
  if rbShowEditForm.Checked then
    AView.EditForm.MasterRowDblClickAction := dcaShowEditForm
  else
    AView.EditForm.MasterRowDblClickAction := dcaSwitchExpandedState;
  AView.EditForm.UseDefaultLayout := chbUseDefaultLayout.Checked;
  AView.EditForm.DefaultColumnCount := Integer(seDefaultColumnCount.Value);
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.GridViewChanged;
begin
  Visible := TcxGridTableView(Helper.GridView).OptionsBehavior.IsInplaceEditFormMode;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;
  Helper.PrepareForCustomization;

  AView := TcxGridTableView(Helper.GridView);
  rbClient.Checked := AView.EditForm.DefaultStretch = fsClient;
  rbHorizontal.Checked := AView.EditForm.DefaultStretch = fsHorizontal;
  rbNone.Checked := AView.EditForm.DefaultStretch = fsNone;
  rbVertical.Checked := AView.EditForm.DefaultStretch = fsVertical;
  rbShowEditForm.Checked := AView.EditForm.MasterRowDblClickAction = dcaShowEditForm;
  rbSwitchExpandedState.Checked := AView.EditForm.MasterRowDblClickAction = dcaSwitchExpandedState;
  chbUseDefaultLayout.Checked := AView.EditForm.UseDefaultLayout;
  seDefaultColumnCount.Value := AView.EditForm.DefaultColumnCount;
  seDefaultColumnCount.Enabled := chbUseDefaultLayout.Checked;
end;

function TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwInplaceEditFormPageDescription);
end;

function TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwInplaceEditFormPageTitle);
end;

{ Events }

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormPageFrame.chbUseDefaultLayoutPropertiesChange(Sender: TObject);
begin
  RefreshPreviewGridContent;
  seDefaultColumnCount.Enabled := chbUseDefaultLayout.Checked;
end;

end.
