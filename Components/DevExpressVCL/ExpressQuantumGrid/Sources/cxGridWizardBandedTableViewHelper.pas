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

unit cxGridWizardBandedTableViewHelper;

{$I cxVer.inc}

interface

uses
  Forms, SysUtils,
  dxCore, cxClasses, cxGraphics,
  cxGridWizardCustomHelper, cxGridCustomView, cxGridTableView, cxGridDBTableView, cxGridBandedTableView,
  cxGridDBBandedTableView, cxGridLevel, dxCustomWizardControl, dxWizardControl,
  cxGridWizardStrs, cxGridWizardTableViewHelper;

type
  { IcxGridWizardBandedTableView }

  IcxGridWizardBandedTableView = interface
  ['{D8D1666E-6281-4C21-949B-ABB1656D5E87}']
    function GetBands: TcxGridBands;
    function GetBandsQuickCustomization: Boolean;
    procedure SetBandsQuickCustomization(const AValue: Boolean);
  end;

  { TcxGridWizardBandedTableViewHelper }

  TcxGridWizardBandedTableViewHelper = class(TcxGridWizardTableViewHelper, IcxGridWizardBandedTableView)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    // IcxGridWizardBandedTableView
    function GetBands: TcxGridBands;
    function GetBandsQuickCustomization: Boolean;
    procedure SetBandsQuickCustomization(const AValue: Boolean);
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    //
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

  { TcxGridWizardDBBandedTableViewHelper }

  TcxGridWizardDBBandedTableViewHelper = class(TcxGridWizardDBTableViewHelper, IcxGridWizardBandedTableView)
  private
    function GetGridView: TcxGridDBBandedTableView;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    // IcxGridWizardBandedTableView
    function GetBands: TcxGridBands;
    function GetBandsQuickCustomization: Boolean;
    procedure SetBandsQuickCustomization(const AValue: Boolean);
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    //
    property GridView: TcxGridDBBandedTableView read GetGridView;
  end;

implementation

{$R cxGridWizardBandedTableViewHelper.res}

uses
  cxGridWizardCustomPage, cxGridWizardDBViewsDataSourcePage, cxGridWizardDBViewsSelectItemsForDisplayPage,
  cxGridWizardBandedTableViewOptionsBandsPage, cxGridWizardCommonCustomizeItemsPage, cxGridWizardDBViewsDataLoadingSettingsPage,
  cxGridWizardTableViewOptionsInterfaceElementsPage, cxGridWizardTableViewOptionsInplaceEditFormPage,
  cxGridWizardTableViewOptionsInplaceEditFormLayoutPage, cxGridWizardTableViewOptionsSummaryPage,
  cxGridWizardTableViewOptionsFilteringSortingPage, cxGridWizardTableViewOptionsBehaviorPage,
  cxGridWizardTableViewOptionsSizingPage, cxGridWizardUnboundViewsSelectItemsForDisplayPage, cxGridCommon;

const
  cxGridWizardBandedTableViewPages: array [0..9] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardBandedTableViewOptionsBandsPageFrame, TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardTableViewOptionsInterfaceElementsPageFrame, TcxGridWizardTableViewOptionsInplaceEditFormPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame, TcxGridWizardTableViewOptionsSummaryPageFrame,
    TcxGridWizardTableViewOptionsFilteringSortingPageFrame, TcxGridWizardTableViewOptionsBehaviorPageFrame,
    TcxGridWizardTableViewOptionsSizingPageFrame);

  cxGridWizardDBBandedTableViewPages: array [0..11] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardDBViewsDataSourcePageFrame, TcxGridWizardBandedTableViewOptionsBandsPageFrame,
    TcxGridWizardDBViewsSelectItemsForDisplayPageFrame, TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardDBViewsDataLoadingSettingsPageFrame,
    TcxGridWizardTableViewOptionsInterfaceElementsPageFrame, TcxGridWizardTableViewOptionsInplaceEditFormPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame, TcxGridWizardTableViewOptionsSummaryPageFrame,
    TcxGridWizardTableViewOptionsFilteringSortingPageFrame, TcxGridWizardTableViewOptionsBehaviorPageFrame,
    TcxGridWizardTableViewOptionsSizingPageFrame);

{ TcxGridWizardBandedTableViewHelper }

procedure TcxGridWizardBandedTableViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
var
  AForm: TcxGridBandedTableCustomizationForm;
begin
  AForm := ACustomizationForm as TcxGridBandedTableCustomizationForm;
  AForm.BandsPage.Caption := cxGetResourceString(@scxgwCustomizationFormBandsTab);
  AForm.ColumnsPage.Caption := cxGetResourceString(@scxgwCustomizationFormColumnsTab);
end;

class function TcxGridWizardBandedTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

function TcxGridWizardBandedTableViewHelper.GetBands: TcxGridBands;
begin
  Result := GridView.Bands;
end;

function TcxGridWizardBandedTableViewHelper.GetBandsQuickCustomization: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomization;
end;

procedure TcxGridWizardBandedTableViewHelper.SetBandsQuickCustomization(const AValue: Boolean);
begin
  GridView.OptionsCustomize.BandsQuickCustomization := AValue;
end;

function TcxGridWizardBandedTableViewHelper.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GetGridView);
end;

function TcxGridWizardBandedTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardBandedTableViewPages);
end;

function TcxGridWizardBandedTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardBandedTableViewPages[Index];
end;

{ TcxGridWizardDBBandedTableViewHelper }

procedure TcxGridWizardDBBandedTableViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
var
  AForm: TcxGridBandedTableCustomizationForm;
begin
  AForm := ACustomizationForm as TcxGridBandedTableCustomizationForm;
  AForm.BandsPage.Caption := cxGetResourceString(@scxgwCustomizationFormBandsTab);
  AForm.ColumnsPage.Caption := cxGetResourceString(@scxgwCustomizationFormColumnsTab);
end;

class function TcxGridWizardDBBandedTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBBandedTableView;
end;

function TcxGridWizardDBBandedTableViewHelper.GetBands: TcxGridBands;
begin
  Result := GridView.Bands;
end;

function TcxGridWizardDBBandedTableViewHelper.GetBandsQuickCustomization: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomization;
end;

procedure TcxGridWizardDBBandedTableViewHelper.SetBandsQuickCustomization(const AValue: Boolean);
begin
  GridView.OptionsCustomize.BandsQuickCustomization := AValue;
end;

function TcxGridWizardDBBandedTableViewHelper.GetGridView: TcxGridDBBandedTableView;
begin
  Result := TcxGridDBBandedTableView(inherited GetGridView);
end;

procedure TcxGridWizardDBBandedTableViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  ATableView: TcxGridDBBandedTableView;
  I: Integer;
begin
  ATableView := AView as TcxGridDBBandedTableView;
  for I := 0 to ATableView.ColumnCount - 1 do
    ATableView.Columns[I].Name := CreateUniqueName(GetParentForm(ATableView.Control),
      ATableView, ATableView.Columns[I], ScxGridPrefixName, ATableView.Columns[I].DataBinding.FieldName);
end;

function TcxGridWizardDBBandedTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardDBBandedTableViewPages);
end;

function TcxGridWizardDBBandedTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardDBBandedTableViewPages[Index];
end;

initialization
  cxGridWizardHelperInfoList.Add(TcxGridWizardBandedTableViewHelper, HInstance);
  cxGridWizardHelperInfoList.Add(TcxGridWizardDBBandedTableViewHelper, HInstance);
end.
