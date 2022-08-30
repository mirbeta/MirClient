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

unit cxGridWizardLayoutViewOptionsViewSettingsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, dxLayoutLookAndFeels, ExtCtrls, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridLayoutView, cxLabel, StdCtrls, cxRadioGroup, cxGridCustomLayoutView,
  cxGridCustomTableView;

type
  { TcxGridWizardLayoutViewOptionsViewSettingsPageFrame }

  TcxGridWizardLayoutViewOptionsViewSettingsPageFrame = class(TcxGridWizardCustomPageFrame)
    chbCenterRecords: TcxCheckBox;
    lbSingleRecordStretch: TcxLabel;
    lbViewMode: TcxLabel;
    lciCarousel: TdxLayoutItem;
    lciCenterRecords: TdxLayoutItem;
    lciClient: TdxLayoutItem;
    lciHorizontal: TdxLayoutItem;
    lciMultiColumn: TdxLayoutItem;
    lciMultiRow: TdxLayoutItem;
    lciNone: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciSingleColumn: TdxLayoutItem;
    lciSingleRecord: TdxLayoutItem;
    lciSingleRecordStretch: TdxLayoutItem;
    lciSingleRow: TdxLayoutItem;
    lciVertical: TdxLayoutItem;
    lciViewMode: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcViewSettingsPageGroup1: TdxLayoutGroup;
    lcViewSettingsPageGroup2: TdxLayoutGroup;
    lcViewSettingsPageGroup3: TdxLayoutGroup;
    lcViewSettingsPageGroup4: TdxLayoutGroup;
    lcViewSettingsPageGroup5: TdxLayoutGroup;
    lcViewSettingsPageGroup7: TdxLayoutGroup;
    lcViewSettingsPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcViewSettingsPageSeparatorItem2: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    rbCarousel: TcxRadioButton;
    rbClient: TcxRadioButton;
    rbHorizontal: TcxRadioButton;
    rbMultiColumn: TcxRadioButton;
    rbMultiRow: TcxRadioButton;
    rbNone: TcxRadioButton;
    rbSingleColumn: TcxRadioButton;
    rbSingleRecord: TcxRadioButton;
    rbSingleRow: TcxRadioButton;
    rbVertical: TcxRadioButton;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    function GetSingleRecordStretch: TcxGridLayoutViewSingleRecordStretch;
    function GetViewMode: TcxGridLayoutViewViewMode;
    procedure UpdateControlsState;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardLayoutViewOptionsViewSettingsPageFrame }

procedure TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.ApplyLocalization;
begin
  lbSingleRecordStretch.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretch);
  lbSingleRecordStretch.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchHint);

  rbClient.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchClient);
  rbClient.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchClientHint);

  rbHorizontal.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchHorizontal);
  rbHorizontal.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchHorizontalHint);

  rbNone.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchNone);
  rbNone.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchNoneHint);

  rbVertical.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchVertical);
  rbVertical.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageSingleRecordStretchVerticalHint);

  chbCenterRecords.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageCenterRecords);
  chbCenterRecords.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageCenterRecordsHint);

  lbViewMode.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewMode);
  lbViewMode.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeHint);

  rbCarousel.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeCarousel);
  rbCarousel.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeCarouselHint);

  rbMultiColumn.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeMultiColumn);
  rbMultiColumn.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeMultiColumnHint);

  rbMultiRow.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeMultiRow);
  rbMultiRow.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeMultiRowHint);

  rbSingleColumn.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleColumn);
  rbSingleColumn.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleColumnHint);

  rbSingleRecord.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleRecord);
  rbSingleRecord.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleRecordHint);

  rbSingleRow.Caption := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleRow);
  rbSingleRow.Hint := cxGetResourceString(@scxgwLayoutViewOptionsViewPageViewModeSingleRowHint);
end;

procedure TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.ApplySettings;
var
  AView: TcxGridLayoutView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridLayoutView(Helper.GridView);
  AView.OptionsView.SingleRecordStretch := GetSingleRecordStretch;
  AView.OptionsView.CenterRecords := chbCenterRecords.Checked;
  AView.OptionsView.ViewMode := GetViewMode;
end;

procedure TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.LoadSettings;
var
  AView: TcxGridLayoutView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := False;

  AView := TcxGridLayoutView(Helper.GridView);
  rbNone.Checked := AView.OptionsView.SingleRecordStretch = srsNone;
  rbHorizontal.Checked := AView.OptionsView.SingleRecordStretch = srsHorizontal;
  rbVertical.Checked := AView.OptionsView.SingleRecordStretch = srsVertical;
  rbClient.Checked := AView.OptionsView.SingleRecordStretch = srsClient;
  chbCenterRecords.Checked := AView.OptionsView.CenterRecords;
  rbSingleRecord.Checked := AView.OptionsView.ViewMode = lvvmSingleRecord;
  rbSingleRow.Checked := AView.OptionsView.ViewMode = lvvmSingleRow;
  rbMultiRow.Checked := AView.OptionsView.ViewMode = lvvmMultiRow;
  rbSingleColumn.Checked := AView.OptionsView.ViewMode = lvvmSingleColumn;
  rbMultiColumn.Checked := AView.OptionsView.ViewMode = lvvmMultiColumn;
  rbCarousel.Checked := AView.OptionsView.ViewMode = lvvmCarousel;
  UpdateControlsState;
end;

function TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewOptionsViewPageDescription);
end;

function TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewOptionsViewPageTitle);
end;

function TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.GetSingleRecordStretch: TcxGridLayoutViewSingleRecordStretch;
begin
  Result := srsNone;
  if rbHorizontal.Checked then
    Result := srsHorizontal;
  if rbVertical.Checked then
    Result := srsVertical;
  if rbClient.Checked then
    Result := srsClient;
end;

function TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.GetViewMode: TcxGridLayoutViewViewMode;
begin
  Result := lvvmSingleRecord;
  if rbSingleRow.Checked then
    Result := lvvmSingleRow;
  if rbMultiRow.Checked then
    Result := lvvmMultiRow;
  if rbSingleColumn.Checked then
    Result := lvvmSingleColumn;
  if rbMultiColumn.Checked then
    Result := lvvmMultiColumn;
  if rbCarousel.Checked then
    Result := lvvmCarousel;
end;

procedure TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.UpdateControlsState;
begin
  lcViewSettingsPageGroup2.Visible := TcxGridLayoutView(Helper.GridView).OptionsView.ViewMode = lvvmSingleRecord;
  lcViewSettingsPageGroup4.Visible := TcxGridLayoutView(Helper.GridView).OptionsView.ViewMode = lvvmSingleRecord;
end;

{ Events }

procedure TcxGridWizardLayoutViewOptionsViewSettingsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
  UpdateControlsState;
end;

end.
