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

unit cxGridWizardTableViewOptionsSummaryPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxCustomData, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxGridCustomPopupMenu, cxGridPopupMenu, dxLayoutContainer, cxCheckBox, ExtCtrls,
  dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs, cxGridTableView, dxCustomWizardControl,
  dxLayoutLookAndFeels, StdCtrls, cxRadioGroup;

type
  { TcxGridWizardTableViewOptionsSummaryPageFrame }

  TcxGridWizardTableViewOptionsSummaryPageFrame = class(TcxGridWizardCustomPageFrame)
    chbMultipleSelectedRecords: TcxRadioButton;
    chbNullIgnore: TcxCheckBox;
    chbSelectedRecords: TcxRadioButton;
    lciMultipleSelectedRecords: TdxLayoutItem;
    lciNullIgnore: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciSelectedRecords: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcSummaryCustomizationPageGroup1: TdxLayoutGroup;
    pnPreviewGrid: TPanel;
    pupmSummaryCustomization: TcxGridPopupMenu;
    rbAllRecords: TcxRadioButton;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    procedure ActivatePopupMenu;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure GridViewChanged; override;
    procedure LoadSettings; override;
    procedure PageDeactivating; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardTableViewOptionsSummaryPageFrame }

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.ApplyLocalization;
begin
  chbNullIgnore.Caption := cxGetResourceString(@scxgwSummaryPageNullIgnore);
  chbNullIgnore.Hint:= cxGetResourceString(@scxgwSummaryPageNullIgnoreHint);

  rbAllRecords.Caption := cxGetResourceString(@scxgwSummaryPageAllRecords);
  rbAllRecords.Hint := cxGetResourceString(@scxgwSummaryPageAllRecordsHint);

  chbSelectedRecords.Caption := cxGetResourceString(@scxgwSummaryPageSelectedRecords);
  chbSelectedRecords.Hint := cxGetResourceString(@scxgwSummaryPageSelectedRecordsHint);

  chbMultipleSelectedRecords.Caption := cxGetResourceString(@scxgwSummaryPageMultipleSelectedRecords);
  chbMultipleSelectedRecords.Hint := cxGetResourceString(@scxgwSummaryPageMultipleSelectedRecordsHint);
end;

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.ApplySettings;
var
  AView: TcxGridTableView;
begin
  AView := TcxGridTableView(Helper.GridView);
  AView.DataController.Summary.Assign(PreviewGrid.ActiveView.DataController.Summary);
  AView.DataController.Summary.Options := [];
  if chbNullIgnore.Checked then
    AView.DataController.Summary.Options := AView.DataController.Summary.Options + [soNullIgnore];
  if chbSelectedRecords.Checked then
    AView.DataController.Summary.Options := AView.DataController.Summary.Options + [soSelectedRecords];
  if chbMultipleSelectedRecords.Checked then
    AView.DataController.Summary.Options := AView.DataController.Summary.Options + [soMultipleSelectedRecords];
end;

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.GridViewChanged;
begin
  Visible := TcxGridTableView(Helper.GridView).OptionsView.Footer or
    (TcxGridTableView(Helper.GridView).OptionsView.GroupFooters <> gfInvisible);
end;

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridTableView(Helper.GridView);
  PreviewGrid.ActiveView.DataController.Summary.Assign(AView.DataController.Summary);
  chbNullIgnore.Checked := soNullIgnore in AView.DataController.Summary.Options;
  chbSelectedRecords.Checked := soSelectedRecords in AView.DataController.Summary.Options;
  chbMultipleSelectedRecords.Checked := soMultipleSelectedRecords in AView.DataController.Summary.Options;
  rbAllRecords.Checked := not (chbSelectedRecords.Checked or chbMultipleSelectedRecords.Checked);
  ActivatePopupMenu;
end;

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.PageDeactivating;
begin
  pupmSummaryCustomization.Grid := nil;
  pupmSummaryCustomization.PopupMenus.Items[0].GridView := nil;
end;

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.ActivatePopupMenu;
begin
  pupmSummaryCustomization.Grid := PreviewGrid;
  pupmSummaryCustomization.PopupMenus.Items[0].GridView := PreviewGrid.ActiveView;
end;

function TcxGridWizardTableViewOptionsSummaryPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSummaryPageDescription);
end;

function TcxGridWizardTableViewOptionsSummaryPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSummaryPageTitle);
end;

{ Events }

procedure TcxGridWizardTableViewOptionsSummaryPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
