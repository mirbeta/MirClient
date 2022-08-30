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

unit cxGridWizardDBViewsDataLoadingSettingsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, cxGraphics, cxControls, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutContainer, ExtCtrls, cxTextEdit, cxMaskEdit, cxSpinEdit, cxCheckBox, dxLayoutControl, cxDBData,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGroupBox, cxRadioGroup, cxLabel, dxCore, cxDropDownEdit,
  dxLayoutLookAndFeels;

type
  { TcxGridWizardDBViewsDataLoadingSettingsPageFrame }

  TcxGridWizardDBViewsDataLoadingSettingsPageFrame = class(TcxGridWizardCustomPageFrame)
    cbMultiThreadedOptionsFiltering: TcxComboBox;
    cbMultiThreadedOptionsSorting: TcxComboBox;
    chbGridMode: TcxCheckBox;
    chbSmartRefresh: TcxCheckBox;
    chbSynchronization: TcxCheckBox;
    chbSyncMode: TcxCheckBox;
    lciGridMode: TdxLayoutItem;
    lciGridModeBufferCount: TdxLayoutItem;
    lciMultiThreadedOptionsFiltering: TdxLayoutItem;
    lciMultiThreadedOptionsSorting: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciSmartRefresh: TdxLayoutItem;
    lciSynchronization: TdxLayoutItem;
    lciSyncMode: TdxLayoutItem;
    lcLoadSettingsPageGroup1: TdxLayoutGroup;
    pnPreviewGrid: TPanel;
    seGridModeBufferCount: TcxSpinEdit;
    procedure RefreshPreviewGrid(Sender: TObject);
    procedure chbGridModePropertiesChange(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridDBDataDefinitions, cxGridWizardCustomHelper;

{$R *.dfm}

{ TcxGridWizardDBViewsDataLoadingSettingsPageFrame }

procedure TcxGridWizardDBViewsDataLoadingSettingsPageFrame.ApplyLocalization;
var
  I: Integer;
begin
  chbGridMode.Caption := cxGetResourceString(@scxgwDataSettingsPageGridMode);
  lciGridModeBufferCount.Caption := cxGetResourceString(@scxgwDataSettingsPageGridModeBufferCount);

  chbSyncMode.Caption := cxGetResourceString(@scxgwDataSettingsPageSyncMode);
  chbSyncMode.Hint := cxGetResourceString(@scxgwDataSettingsPageSyncModeHint);

  chbSynchronization.Caption := cxGetResourceString(@scxgwDataSettingsPageSynchronization);
  chbSynchronization.Hint := cxGetResourceString(@scxgwDataSettingsPageSynchronizationHint);

  chbSmartRefresh.Caption := cxGetResourceString(@scxgwDataSettingsPageSmartRefresh);
  chbSmartRefresh.Hint := cxGetResourceString(@scxgwDataSettingsPageSmartRefreshHint);

  lciMultiThreadedOptionsFiltering.Caption := cxGetResourceString(@scxgwDataSettingsPageMultiThreadedOptionsFiltering);
  lciMultiThreadedOptionsSorting.Caption := cxGetResourceString(@scxgwDataSettingsPageMultiThreadedOptionsSorting);
  for I := 0 to Length(scxGridWizardDefaultBooleanCaptions) - 1 do
  begin
    cbMultiThreadedOptionsFiltering.Properties.Items.Strings[I] := scxGridWizardDefaultBooleanCaptions[I];
    cbMultiThreadedOptionsSorting.Properties.Items.Strings[I] := scxGridWizardDefaultBooleanCaptions[I];
  end;
end;

procedure TcxGridWizardDBViewsDataLoadingSettingsPageFrame.ApplySettings;
const
  MultiThreadedOptionsValuesMap: array [0..2] of TdxDefaultBoolean = (bFalse, bTrue, bDefault);
var
  ADataController: TcxDBDataController;
begin
  ADataController := (Helper as IcxGridWizardHelperDBDataControllerSupport).GetDataController;

  ADataController.DataModeController.SyncMode := chbSyncMode.Checked;
  ADataController.MultiThreadedOptions.Filtering := MultiThreadedOptionsValuesMap[cbMultiThreadedOptionsFiltering.ItemIndex];
  ADataController.MultiThreadedOptions.Sorting := MultiThreadedOptionsValuesMap[cbMultiThreadedOptionsSorting.ItemIndex];

  Helper.GridView.Synchronization := chbSynchronization.Checked;
end;

procedure TcxGridWizardDBViewsDataLoadingSettingsPageFrame.LoadSettings;
const
  MultiThreadedOptionsValuesMap: array [TdxDefaultBoolean] of Integer = (0, 1, 2);
var
  ADataController: TcxDBDataController;
begin
  ADataController := (Helper as IcxGridWizardHelperDBDataControllerSupport).GetDataController;

  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := False;


  lciSynchronization.Visible := IsMultiLevelStructure;
  chbSynchronization.Checked := Helper.GridView.Synchronization;

  chbSyncMode.Checked := ADataController.DataModeController.SyncMode;
  cbMultiThreadedOptionsFiltering.ItemIndex := MultiThreadedOptionsValuesMap[ADataController.MultiThreadedOptions.Filtering];
  cbMultiThreadedOptionsSorting.ItemIndex := MultiThreadedOptionsValuesMap[ADataController.MultiThreadedOptions.Sorting];
  seGridModeBufferCount.Enabled := chbGridMode.Checked;
end;

function TcxGridWizardDBViewsDataLoadingSettingsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwDataSettingsPageDescription);
end;

function TcxGridWizardDBViewsDataLoadingSettingsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwDataSettingsPageTitle);
end;

{ Events }

procedure TcxGridWizardDBViewsDataLoadingSettingsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

procedure TcxGridWizardDBViewsDataLoadingSettingsPageFrame.chbGridModePropertiesChange(Sender: TObject);
begin
  RefreshPreviewGridContent;
  seGridModeBufferCount.Enabled := chbGridMode.Checked;
end;

end.
