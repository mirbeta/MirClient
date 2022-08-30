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

unit cxGridWizardLayoutViewOptionsCustomizeItemsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxClasses, ExtCtrls,
  cxGridCustomView, cxGridLayoutViewCustomizationForm, cxGridWizardCustomPage, cxGridWizardStrs, dxCustomWizardControl,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutLookAndFeels, dxLayoutControl, cxGridCustomLayoutView;

type
  TcxCustomGridControllerAccess = class(TcxCustomGridController);

  { TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame }

  TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame = class(TcxGridWizardCustomPageFrame)
    lcMainItem1: TdxLayoutItem;
    pnPreviewGrid: TPanel;
  private
    function GetCustomizationForm: TcxGridLayoutViewCustomizationForm;
    function GetGridViewController: TcxCustomGridControllerAccess;
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplySettings; override;
    procedure BeforeDestruction; override;
    procedure LoadSettings; override;
    procedure PageActivating; override;
    procedure ShowCustomizationForm;

    property CustomizationForm: TcxGridLayoutViewCustomizationForm read GetCustomizationForm;
    property GridViewController: TcxCustomGridControllerAccess read GetGridViewController;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame }

procedure TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.ApplySettings;
begin
  if Assigned(CustomizationForm) then
  begin
    CustomizationForm.ApplyChanges;
    GridViewController.HideCustomizationForm;
    GridViewController.DoCustomization;
  end;
  Helper.Assign(PreviewGrid.ActiveView);
  Helper.RestoreAfterCustomization;
end;

procedure TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(CustomizationForm) then
  begin
    GridViewController.HideCustomizationForm;
    GridViewController.DoCustomization;
  end;
end;

procedure TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.LoadSettings;
begin
  Helper.PrepareForCustomization;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  GridViewController.DoCreateCustomizationForm;
  if Assigned(CustomizationForm) then
  begin
    Helper.CorrectCustomizationFormContent(CustomizationForm);
    GridViewController.DoCustomization;
    CustomizationForm.BorderStyle := bsNone;
    CustomizationForm.liOk.Visible := False;
    CustomizationForm.liCancel.Visible := False;
    CustomizationForm.Parent := pnPreviewGrid;
    CustomizationForm.Align := alClient;
  end;
end;

procedure TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.PageActivating;
begin
  ShowCustomizationForm;
end;

procedure TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.ShowCustomizationForm;
begin
  CustomizationForm.Show;
end;

function TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewCustomizeItemsPageDescription);
end;

function TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwLayoutViewCustomizeItemsPageTitle);
end;

function TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.GetCustomizationForm: TcxGridLayoutViewCustomizationForm;
begin
  if (PreviewGrid <> nil) and (PreviewGrid.ActiveLevel <> nil) and (PreviewGrid.ActiveView <> nil) then
    Result := TcxGridLayoutViewCustomizationForm(GridViewController.CustomizationForm)
  else
    Result := nil;
end;

function TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame.GetGridViewController: TcxCustomGridControllerAccess;
begin
  Result := TcxCustomGridControllerAccess(PreviewGrid.ActiveView.Controller)
end;

end.
