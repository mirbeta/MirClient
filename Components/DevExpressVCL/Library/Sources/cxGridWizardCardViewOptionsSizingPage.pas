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

unit cxGridWizardCardViewOptionsSizingPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, ExtCtrls, dxLayoutControl, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridCardView, dxLayoutLookAndFeels, cxLabel;

type
  { TcxGridWizardCardViewOptionsSizingPageFrame }

  TcxGridWizardCardViewOptionsSizingPageFrame = class(TcxGridWizardCustomPageFrame)
    chbCardAutoWidth: TcxCheckBox;
    chbCardSizing: TcxCheckBox;
    chbCellAutoHeight: TcxCheckBox;
    chbCellEndEllipsis: TcxCheckBox;
    chbRowCaptionAutoHeight: TcxCheckBox;
    chbRowCaptionEndEllipsis: TcxCheckBox;
    lbAutoSizingOptions: TcxLabel;
    lbManualSizingOptions: TcxLabel;
    lcCellEndEllipsis: TdxLayoutItem;
    lciCardAutoWidth: TdxLayoutItem;
    lciCardSizing: TdxLayoutItem;
    lciCellAutoHeight: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRowCaptionAutoHeight: TdxLayoutItem;
    lciSizingPageGroup1: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup6: TdxLayoutGroup;
    lcMainGroup7: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainSeparatorItem2: TdxLayoutSeparatorItem;
    lcRowCaptionEndEllipsis: TdxLayoutItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    procedure UpdateControlsState;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardCardViewOptionsSizingPageFrame }

procedure TcxGridWizardCardViewOptionsSizingPageFrame.ApplyLocalization;
begin
  lbAutoSizingOptions.Caption := cxGetResourceString(@scxgwSizingPageGroupAutoSizingOptions);
  lbManualSizingOptions.Caption := cxGetResourceString(@scxgwSizingPageGroupManualSizingOptions);

  chbCellAutoHeight.Caption := cxGetResourceString(@scxgwSizingPageCellAutoHeight);
  chbCellAutoHeight.Hint := cxGetResourceString(@scxgwSizingPageCellAutoHeightHint);

  chbCellEndEllipsis.Caption := cxGetResourceString(@scxgwSizingPageCellEndEllipsis);
  chbCellEndEllipsis.Hint := cxGetResourceString(@scxgwSizingPageCellEndEllipsisHint);

  chbCardAutoWidth.Caption := cxGetResourceString(@scxgwSizingPageCardAutoWidth);
  chbCardAutoWidth.Hint := cxGetResourceString(@scxgwSizingPageCardAutoWidthHint);

  chbCardSizing.Caption := cxGetResourceString(@scxgwSizingPageCardSizing);
  chbCardSizing.Hint := cxGetResourceString(@scxgwSizingPageCardSizingHint);

  chbRowCaptionAutoHeight.Caption := cxGetResourceString(@scxgwSizingPageRowCaptionAutoHeight);
  chbRowCaptionAutoHeight.Hint := cxGetResourceString(@scxgwSizingPageRowCaptionAutoHeightHint);

  chbRowCaptionEndEllipsis.Caption := cxGetResourceString(@scxgwSizingPageRowCaptionEndEllipsis);
  chbRowCaptionEndEllipsis.Hint := cxGetResourceString(@scxgwSizingPageRowCaptionEndEllipsisHint);
end;

procedure TcxGridWizardCardViewOptionsSizingPageFrame.ApplySettings;
var
  AView: TcxGridCardView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridCardView(Helper.GridView);
  AView.OptionsView.CardAutoWidth := chbCardAutoWidth.Checked;
  AView.OptionsView.CellAutoHeight := chbCellAutoHeight.Checked;
  AView.OptionsView.RowCaptionAutoHeight := chbRowCaptionAutoHeight.Checked;
  AView.OptionsCustomize.CardSizing := chbCardSizing.Checked;
  AView.OptionsView.CellEndEllipsis := chbCellEndEllipsis.Checked;
  AView.OptionsView.RowCaptionEndEllipsis := chbRowCaptionEndEllipsis.Checked;
end;

procedure TcxGridWizardCardViewOptionsSizingPageFrame.LoadSettings;
var
  AView: TcxGridCardView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridCardView(Helper.GridView);
  chbCardAutoWidth.Checked := AView.OptionsView.CardAutoWidth;
  chbCellAutoHeight.Checked := AView.OptionsView.CellAutoHeight;
  chbRowCaptionAutoHeight.Checked := AView.OptionsView.RowCaptionAutoHeight;
  chbCardSizing.Checked := AView.OptionsCustomize.CardSizing;
  chbRowCaptionEndEllipsis.Checked := AView.OptionsView.RowCaptionEndEllipsis;
  chbCellEndEllipsis.Checked := AView.OptionsView.CellEndEllipsis;

  UpdateControlsState;
end;

function TcxGridWizardCardViewOptionsSizingPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSizingPageDescription);
end;

function TcxGridWizardCardViewOptionsSizingPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSizingPageTitle);
end;

procedure TcxGridWizardCardViewOptionsSizingPageFrame.UpdateControlsState;
begin
  lcCellEndEllipsis.Enabled := not chbCellAutoHeight.Checked;
  lcRowCaptionEndEllipsis.Enabled := not chbRowCaptionAutoHeight.Checked;
end;

{ Events }

procedure TcxGridWizardCardViewOptionsSizingPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
  UpdateControlsState;
end;

end.
