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

unit cxGridWizardCommonCustomizeItemsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxLayoutControlAdapters, dxLayoutContainer, cxSplitter, cxGridLevel,
  cxGrid, ExtCtrls, dxLayoutControl, cxGridCustomView, cxGridCardView, cxGridDBCardView, cxGridChartView,
  cxGridDBChartView, cxGridTableView, cxGridDBTableView, cxGridBandedTableView, cxGridDBBandedTableView,
  cxGridWizardCustomPage, cxGridWizardStrs, dxCustomWizardControl, cxGridWizardCustomHelper, dxLayoutLookAndFeels;

type
  TcxCustomGridControllerAccess = class(TcxCustomGridController);

  { TcxGridWizardCommonCustomizeItemsPageFrame }

  TcxGridWizardCommonCustomizeItemsPageFrame = class(TcxGridWizardCustomPageFrame)
    lciCustomizationForm: TdxLayoutItem;
    lciSplitterCustomizeItemsPage: TdxLayoutItem;
    lcPreviewGrid: TdxLayoutItem;
    pnCustomizationForm: TPanel;
    pnPreviewGrid: TPanel;
    SplitterCustomizeItemsPage: TcxSplitter;
    procedure SplitterCustomizeItemsPageMoved(Sender: TObject);
  private
    function GetGridViewController: TcxCustomGridControllerAccess;
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplySettings; override;
    procedure LoadSettings; override;
    procedure PageActivating; override;
    procedure ShowCustomizationForm;

    property GridViewController: TcxCustomGridControllerAccess read GetGridViewController;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardCommonCustomizeItemsPageFrame }

procedure TcxGridWizardCommonCustomizeItemsPageFrame.ApplySettings;
begin
  GridViewController.Customization := False;
  Helper.Assign(PreviewGrid.ActiveView);
  Helper.RestoreAfterCustomization;
end;

procedure TcxGridWizardCommonCustomizeItemsPageFrame.LoadSettings;
var
  ACustomizationForm: TForm;
begin
  Helper.PrepareForCustomization;

  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  GridViewController.DoCreateCustomizationForm;
  ACustomizationForm := GridViewController.CustomizationForm;
  if ACustomizationForm <> nil then
  begin
    Helper.CorrectCustomizationFormContent(ACustomizationForm);
    ACustomizationForm.BorderStyle := bsNone;
    ACustomizationForm.Parent := pnCustomizationForm;
    ACustomizationForm.Align := alClient;
  end;
end;

procedure TcxGridWizardCommonCustomizeItemsPageFrame.PageActivating;
begin
  ShowCustomizationForm;
end;

procedure TcxGridWizardCommonCustomizeItemsPageFrame.ShowCustomizationForm;
begin
  GridViewController.Customization := True;
end;

function TcxGridWizardCommonCustomizeItemsPageFrame.GetPageDescription: string;
begin
  if Helper.GetGridViewType = gvtUnbound then
    Result := cxGetResourceString(@scxgwCustomizeItemsPageDescriptionUnbound)
  else
    Result := cxGetResourceString(@scxgwCustomizeItemsPageDescriptionDB);
end;

function TcxGridWizardCommonCustomizeItemsPageFrame.GetPageTitle: string;
begin
  if Helper.GetGridViewType = gvtUnbound then
    Result := cxGetResourceString(@scxgwCustomizeItemsPageTitleUnbound)
  else
    Result := cxGetResourceString(@scxgwCustomizeItemsPageTitleDB);
end;

function TcxGridWizardCommonCustomizeItemsPageFrame.GetGridViewController: TcxCustomGridControllerAccess;
begin
  Result := TcxCustomGridControllerAccess(PreviewGrid.ActiveView.Controller);
end;

{ Events }

procedure TcxGridWizardCommonCustomizeItemsPageFrame.SplitterCustomizeItemsPageMoved(Sender: TObject);
begin
  ApplySettings;
  LoadSettings;
  ShowCustomizationForm;
end;

end.
