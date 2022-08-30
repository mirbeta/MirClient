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

unit cxGridWizardChartViewOptionsViewSettingsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, ExtCtrls, dxLayoutControl, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridChartView, dxLayoutLookAndFeels, cxLabel;

type
  { TcxGridWizardChartViewOptionsViewSettingsPageFrame }

  TcxGridWizardChartViewOptionsViewSettingsPageFrame = class(TcxGridWizardCustomPageFrame)
    chbAntialiasing: TcxCheckBox;
    chbCustomizeButton: TcxCheckBox;
    chbDataDrillDown: TcxCheckBox;
    chbDataGroupHiding: TcxCheckBox;
    chbDataGroupMoving: TcxCheckBox;
    chbOptionsCustomization: TcxCheckBox;
    chbSeriesCustomization: TcxCheckBox;
    chbTransparentCaptions: TcxCheckBox;
    lbCustomizationOptions: TcxLabel;
    lbDataGroupOptions: TcxLabel;
    lbOther: TcxLabel;
    lciAntialiasing: TdxLayoutItem;
    lciCustomizationOptions: TdxLayoutItem;
    lciCustomizeButton: TdxLayoutItem;
    lciDataDrillDown: TdxLayoutItem;
    lciDataGroupHiding: TdxLayoutItem;
    lciDataGroupMoving: TdxLayoutItem;
    lciDataGroupOptions: TdxLayoutItem;
    lciOptionsCustomization: TdxLayoutItem;
    lciOther: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciSeriesCustomization: TdxLayoutItem;
    lciTransparentCaptions: TdxLayoutItem;
    lcViewSettingsPageGroup1: TdxLayoutGroup;
    lcViewSettingsPageGroup10: TdxLayoutGroup;
    lcViewSettingsPageGroup2: TdxLayoutGroup;
    lcViewSettingsPageGroup3: TdxLayoutGroup;
    lcViewSettingsPageGroup4: TdxLayoutGroup;
    lcViewSettingsPageGroup5: TdxLayoutGroup;
    lcViewSettingsPageGroup6: TdxLayoutGroup;
    lcViewSettingsPageGroup7: TdxLayoutGroup;
    lcViewSettingsPageGroup8: TdxLayoutGroup;
    lcViewSettingsPageGroup9: TdxLayoutGroup;
    lcViewSettingsPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcViewSettingsPageSeparatorItem2: TdxLayoutSeparatorItem;
    lcViewSettingsPageSeparatorItem3: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardChartViewOptionsViewSettingsPageFrame }

procedure TcxGridWizardChartViewOptionsViewSettingsPageFrame.ApplyLocalization;
begin
  lbDataGroupOptions.Caption := cxGetResourceString(@scxGridWizardOptionCaptionDataGroupOptions);
  chbDataGroupHiding.Caption := cxGetResourceString(@scxGridWizardOptionCaptionDataGroupHiding);
  chbDataGroupMoving.Caption := cxGetResourceString(@scxGridWizardOptionCaptionDataGroupMoving);
  lbCustomizationOptions.Caption := cxGetResourceString(@scxGridWizardOptionCaptionCustomizationOptions);
  chbOptionsCustomization.Caption := cxGetResourceString(@scxGridWizardOptionCaptionOptionsCustomization);
  chbSeriesCustomization.Caption := cxGetResourceString(@scxGridWizardOptionCaptionSeriesCustomization);
  chbCustomizeButton.Caption := cxGetResourceString(@scxGridWizardOptionCaptionToolBoxCustomizeButton);
  lbOther.Caption := cxGetResourceString(@scxgwCommonGroupCaptionOthers);
  chbDataDrillDown.Caption := cxGetResourceString(@scxGridWizardOptionCaptionDataDrillDown);
  chbAntialiasing.Caption := cxGetResourceString(@scxGridWizardOptionCaptionAntialiasing);
  chbTransparentCaptions.Caption := cxGetResourceString(@scxGridWizardOptionCaptionTransparentCaption);
end;

procedure TcxGridWizardChartViewOptionsViewSettingsPageFrame.ApplySettings;
var
  AView: TcxGridChartView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridChartView(Helper.GridView);
  AView.OptionsCustomize.DataGroupHiding := chbDataGroupHiding.Checked;
  AView.OptionsCustomize.DataGroupMoving := chbDataGroupMoving.Checked;
  AView.OptionsCustomize.OptionsCustomization := chbOptionsCustomization.Checked;
  AView.OptionsCustomize.SeriesCustomization := chbSeriesCustomization.Checked;
  AView.ToolBox.CustomizeButton := chbCustomizeButton.Checked;
  AView.OptionsCustomize.DataDrillDown := chbDataDrillDown.Checked;
  AView.OptionsView.Antialiasing := chbAntialiasing.Checked;
  AView.OptionsView.TransparentCaptions := chbTransparentCaptions.Checked;
end;

procedure TcxGridWizardChartViewOptionsViewSettingsPageFrame.LoadSettings;
var
  AView: TcxGridChartView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := False;

  AView := TcxGridChartView(Helper.GridView);
  chbDataGroupHiding.Checked := AView.OptionsCustomize.DataGroupHiding;
  chbDataGroupMoving.Checked := AView.OptionsCustomize.DataGroupMoving;
  chbOptionsCustomization.Checked := AView.OptionsCustomize.OptionsCustomization;
  chbSeriesCustomization.Checked := AView.OptionsCustomize.SeriesCustomization;
  chbCustomizeButton.Checked := AView.ToolBox.CustomizeButton;
  chbDataDrillDown.Checked := AView.OptionsCustomize.DataDrillDown;
  chbAntialiasing.Checked := AView.OptionsView.Antialiasing;
  chbTransparentCaptions.Checked := AView.OptionsView.TransparentCaptions;
end;

function TcxGridWizardChartViewOptionsViewSettingsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxGridWizardChartViewOptionsViewSettingsPageDescription);
end;

function TcxGridWizardChartViewOptionsViewSettingsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxGridWizardChartViewOptionsViewSettingsPageTitle);
end;

{ Events }

procedure TcxGridWizardChartViewOptionsViewSettingsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
