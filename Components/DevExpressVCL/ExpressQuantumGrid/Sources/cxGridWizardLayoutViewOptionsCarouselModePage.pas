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

unit cxGridWizardLayoutViewOptionsCarouselModePage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutContainer, ExtCtrls, dxLayoutControl,
  cxGridLayoutView, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxTrackBar, cxLabel, cxTextEdit, cxMaskEdit,
  cxSpinEdit, cxCheckBox, cxClasses, cxGridWizardCustomPage, cxGridWizardStrs, dxLayoutLookAndFeels,
  cxGridCustomLayoutView, cxGridCustomTableView;

type
  { TcxGridWizardLayoutViewOptionsCarouselModePageFrame }

  TcxGridWizardLayoutViewOptionsCarouselModePageFrame = class(TcxGridWizardCustomPageFrame)
    chbAutoPitchAngle: TcxCheckBox;
    lbAngleOptions: TcxLabel;
    lbBackgroundRecordOptions: TcxLabel;
    lbOther: TcxLabel;
    lcCarouselModeSettingsPageGroup1: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup10: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup2: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup3: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup4: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup5: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup6: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup7: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup8: TdxLayoutGroup;
    lcCarouselModeSettingsPageGroup9: TdxLayoutGroup;
    lcCarouselModeSettingsPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcCarouselModeSettingsPageSeparatorItem2: TdxLayoutSeparatorItem;
    lcCarouselModeSettingsPageSeparatorItem3: TdxLayoutSeparatorItem;
    lciAngleOptions: TdxLayoutItem;
    lciAnimationInterval: TdxLayoutItem;
    lciAutoPitchAngle: TdxLayoutItem;
    lciBackgroundRecordAlphaLevel: TdxLayoutItem;
    lciBackgroundRecordEndScale: TdxLayoutItem;
    lciBackgroundRecordOptions: TdxLayoutItem;
    lciBackgroundRecordStartScale: TdxLayoutItem;
    lciOther: TdxLayoutItem;
    lciPitchAngle: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRadius: TdxLayoutItem;
    lciRecordCount: TdxLayoutItem;
    lciRollAngle: TdxLayoutItem;
    pnPreviewGrid: TPanel;
    seAnimationInterval: TcxSpinEdit;
    seRadius: TcxSpinEdit;
    seRecordCount: TcxSpinEdit;
    trbBackgroundRecordAlphaLevel: TcxTrackBar;
    trbBackgroundRecordEndScale: TcxTrackBar;
    trbBackgroundRecordStartScale: TcxTrackBar;
    trbPitchAngle: TcxTrackBar;
    trbRollAngle: TcxTrackBar;
    procedure RefreshPreviewGrid(Sender: TObject);
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

uses
  cxGridWizardCustomHelper;

{$R *.dfm}

{ TcxGridWizardLayoutViewOptionsCarouselModePageFrame }

procedure TcxGridWizardLayoutViewOptionsCarouselModePageFrame.ApplyLocalization;
begin
  lbBackgroundRecordOptions.Caption := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordOptions);

  lciBackgroundRecordAlphaLevel.Caption := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordAlphaLevel);
  trbBackgroundRecordAlphaLevel.Hint := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordAlphaLevelHint);

  lciBackgroundRecordStartScale.Caption := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordStartScale);
  trbBackgroundRecordStartScale.Hint := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordStartScaleHint);

  lciBackgroundRecordEndScale.Caption := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordEndScale);
  trbBackgroundRecordEndScale.Hint := cxGetResourceString(@scxgwLayoutViewCarouselBackgroundRecordEndScaleHint);

  lbAngleOptions.Caption := cxGetResourceString(@scxgwLayoutViewCarouselAngleOptions);

  lciRollAngle.Caption := cxGetResourceString(@scxgwLayoutViewCarouselRollAngle);
  trbRollAngle.Hint := cxGetResourceString(@scxgwLayoutViewCarouselRollAngleHint);

  lciPitchAngle.Caption := cxGetResourceString(@scxgwLayoutViewCarouselPitchAngle);
  trbPitchAngle.Hint := cxGetResourceString(@scxgwLayoutViewCarouselPitchAngleHint);
  chbAutoPitchAngle.Caption := cxGetResourceString(@scxgwLayoutViewCarouselAutoPitchAngle);
  chbAutoPitchAngle.Hint := cxGetResourceString(@scxgwLayoutViewCarouselAutoPitchAngleHint);

  lbOther.Caption := cxGetResourceString(@scxgwCommonGroupCaptionOthers);

  lciAnimationInterval.Caption := cxGetResourceString(@scxgwLayoutViewCarouselAnimationInterval);
  seAnimationInterval.Hint := cxGetResourceString(@scxgwLayoutViewCarouselAnimationIntervalHint);

  lciRadius.Caption := cxGetResourceString(@scxgwLayoutViewCarouselRadius);
  seRadius.Hint := cxGetResourceString(@scxgwLayoutViewCarouselRadiusHint);

  lciRecordCount.Caption := cxGetResourceString(@scxgwLayoutViewCarouselRecordCount);
  seRecordCount.Hint := cxGetResourceString(@scxgwLayoutViewCarouselRecordCountHint);
end;

procedure TcxGridWizardLayoutViewOptionsCarouselModePageFrame.ApplySettings;
var
  AOptions: TcxGridLayoutViewCarouselMode;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AOptions := TcxGridLayoutView(Helper.GridView).OptionsView.CarouselMode;
  AOptions.BackgroundRecordAlphaLevel := trbBackgroundRecordAlphaLevel.Position;
  AOptions.BackgroundRecordStartScale := trbBackgroundRecordStartScale.Position;
  AOptions.BackgroundRecordEndScale := trbBackgroundRecordEndScale.Position;
  AOptions.RollAngle := trbRollAngle.Position;
  AOptions.PitchAngle := trbPitchAngle.Position;
  AOptions.AutoPitchAngle := chbAutoPitchAngle.Checked;
  AOptions.AnimationInterval := seAnimationInterval.Value;
  AOptions.Radius := seRadius.Value;
  AOptions.RecordCount := seRecordCount.Value;
end;

procedure TcxGridWizardLayoutViewOptionsCarouselModePageFrame.GridViewChanged;
begin
  Visible := TcxGridLayoutView(Helper.GridView).OptionsView.ViewMode = lvvmCarousel;
end;

procedure TcxGridWizardLayoutViewOptionsCarouselModePageFrame.LoadSettings;
var
  AOptions: TcxGridLayoutViewCarouselMode;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AOptions := TcxGridLayoutView(Helper.GridView).OptionsView.CarouselMode;
  trbBackgroundRecordAlphaLevel.Position := AOptions.BackgroundRecordAlphaLevel;
  trbBackgroundRecordStartScale.Position := AOptions.BackgroundRecordStartScale;
  trbBackgroundRecordEndScale.Position := AOptions.BackgroundRecordEndScale;
  trbRollAngle.Position := Round(AOptions.RollAngle);
  trbPitchAngle.Position := Round(AOptions.PitchAngle);
  chbAutoPitchAngle.Checked := AOptions.AutoPitchAngle;
  seAnimationInterval.Value := AOptions.AnimationInterval;
  seRadius.Value := AOptions.Radius;
  seRecordCount.Value := AOptions.RecordCount;
end;

function TcxGridWizardLayoutViewOptionsCarouselModePageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewCarouselPageDescription);
end;

function TcxGridWizardLayoutViewOptionsCarouselModePageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewCarouselPageTitle);
end;

{ Events }

procedure TcxGridWizardLayoutViewOptionsCarouselModePageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
