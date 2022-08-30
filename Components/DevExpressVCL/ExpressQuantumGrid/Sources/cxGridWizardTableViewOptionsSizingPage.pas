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

unit cxGridWizardTableViewOptionsSizingPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,ExtCtrls, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControl, cxClasses, cxCheckBox, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridTableView, dxLayoutLookAndFeels, cxLabel;

type
  { TcxGridWizardTableViewOptionsSizingPageFrame }

  TcxGridWizardTableViewOptionsSizingPageFrame = class(TcxGridWizardCustomPageFrame)
    chbCellAutoheight: TcxCheckBox;
    chbCellEndEllipsis: TcxCheckBox;
    chbColumnAutowidth: TcxCheckBox;
    chbDataRowSizing: TcxCheckBox;
    chbFooterAutoheight: TcxCheckBox;
    chbGroupRowSizing: TcxCheckBox;
    chbHeaderAutoheight: TcxCheckBox;
    lbAutoSizingOptions: TcxLabel;
    lbManualSizingOptions: TcxLabel;
    lciAutoSizingOptions: TdxLayoutItem;
    lciCellAutoheight: TdxLayoutItem;
    lciCellEndEllipsis: TdxLayoutItem;
    lciColumnAutowidth: TdxLayoutItem;
    lciDataRowSizing: TdxLayoutItem;
    lciFooterAutoheight: TdxLayoutItem;
    lciGroupRowSizing: TdxLayoutItem;
    lciHeaderAutoheight: TdxLayoutItem;
    lciManualSizingOptions: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lcSizingPageGroup1: TdxLayoutGroup;
    lcSizingPageGroup2: TdxLayoutGroup;
    lcSizingPageGroup3: TdxLayoutGroup;
    lcSizingPageGroup4: TdxLayoutGroup;
    lcSizingPageGroup5: TdxLayoutGroup;
    lcSizingPageGroup6: TdxLayoutGroup;
    lcSizingPageGroup7: TdxLayoutGroup;
    lcSizingPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcSizingPageSeparatorItem2: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
    procedure chbCellAutoheightClick(Sender: TObject);
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

{ TcxGridWizardTableViewOptionsSizingPageFrame }

procedure TcxGridWizardTableViewOptionsSizingPageFrame.ApplyLocalization;
begin
  lbAutoSizingOptions.Caption := cxGetResourceString(@scxgwSizingPageGroupAutoSizingOptions);

  chbCellAutoheight.Caption := cxGetResourceString(@scxgwSizingPageCellAutoHeight);
  chbCellAutoheight.Hint := cxGetResourceString(@scxgwSizingPageCellAutoHeightHint);

  chbColumnAutowidth.Caption := cxGetResourceString(@scxgwSizingPageColumnAutoWidth);
  chbColumnAutowidth.Hint := cxGetResourceString(@scxgwSizingPageColumnAutoWidthHint);

  chbFooterAutoheight.Caption := cxGetResourceString(@scxgwSizingPageFooterAutoHeight);
  chbFooterAutoheight.Hint := cxGetResourceString(@scxgwSizingPageFooterAutoHeightHint);

  chbHeaderAutoheight.Caption := cxGetResourceString(@scxgwSizingPageHeaderAutoHeight);
  chbHeaderAutoheight.Hint := cxGetResourceString(@scxgwSizingPageHeaderAutoHeightHint);

  chbGroupRowSizing.Caption := cxGetResourceString(@scxgwSizingPageGroupRowSizing);
  chbGroupRowSizing.Hint := cxGetResourceString(@scxgwSizingPageGroupRowSizingHint);

  lbManualSizingOptions.Caption := cxGetResourceString(@scxgwSizingPageGroupManualSizingOptions);

  chbDataRowSizing.Caption := cxGetResourceString(@scxgwSizingPageDataRowSizing);
  chbDataRowSizing.Hint := cxGetResourceString(@scxgwSizingPageDataRowSizingHint);

  chbCellEndEllipsis.Caption := cxGetResourceString(@scxgwSizingPageCellEndEllipsis);
  chbCellEndEllipsis.Hint := cxGetResourceString(@scxgwSizingPageCellEndEllipsis);
end;

procedure TcxGridWizardTableViewOptionsSizingPageFrame.ApplySettings;
var
  AView: TcxGridTableView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridTableView(Helper.GridView);
  AView.OptionsView.CellAutoHeight := chbCellAutoheight.Checked;
  AView.OptionsView.ColumnAutoWidth := chbColumnAutowidth.Checked;
  AView.OptionsView.FooterAutoHeight := chbFooterAutoheight.Checked;
  AView.OptionsView.HeaderAutoHeight := chbHeaderAutoheight.Checked;
  AView.OptionsView.CellEndEllipsis := chbCellEndEllipsis.Checked;
  AView.OptionsCustomize.DataRowSizing := chbDataRowSizing.Checked;
  AView.OptionsCustomize.GroupRowSizing := chbGroupRowSizing.Checked;
  AView.OptionsView.DataRowHeight := 0;
  AView.OptionsView.GroupRowHeight := 0;
end;

procedure TcxGridWizardTableViewOptionsSizingPageFrame.chbCellAutoheightClick(Sender: TObject);
begin
  chbCellEndEllipsis.Enabled := not chbCellAutoheight.Checked;
  RefreshPreviewGrid(Self);
end;

procedure TcxGridWizardTableViewOptionsSizingPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridTableView(Helper.GridView);
  chbCellAutoheight.Checked := AView.OptionsView.CellAutoHeight;
  chbColumnAutowidth.Checked := AView.OptionsView.ColumnAutoWidth;
  chbFooterAutoheight.Checked := AView.OptionsView.FooterAutoHeight;
  chbHeaderAutoheight.Checked := AView.OptionsView.HeaderAutoHeight;
  chbCellEndEllipsis.Checked := AView.OptionsView.CellEndEllipsis;
  chbDataRowSizing.Checked := AView.OptionsCustomize.DataRowSizing;
  chbGroupRowSizing.Checked := AView.OptionsCustomize.GroupRowSizing;

  lcSizingPageGroup3.Visible := AView.OptionsView.Indicator;
  lcSizingPageGroup6.Visible := AView.OptionsView.Indicator;
end;

function TcxGridWizardTableViewOptionsSizingPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSizingPageDescription);
end;

function TcxGridWizardTableViewOptionsSizingPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSizingPageTitle);
end;

{ Events }

procedure TcxGridWizardTableViewOptionsSizingPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
