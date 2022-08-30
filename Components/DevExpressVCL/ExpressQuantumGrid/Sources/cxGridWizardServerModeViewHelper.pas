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

unit cxGridWizardServerModeViewHelper;

{$I cxVer.inc}

interface

uses
  SysUtils, Forms, DB, Classes,
  dxCore, cxClasses, cxGraphics, dxCustomWizardControl, dxWizardControl, cxGridWizardStrs, cxGridCustomView,
  cxGridServerModeTableView, cxGridServerModeBandedTableView, cxGridLevel, cxGridWizardCustomHelper, dxServerModeData,
  cxGridServerModeDataDefinitions, cxGridWizardTableViewHelper, cxGridWizardBandedTableViewHelper,
  cxGridBandedTableView, cxEdit;


type
  { IcxGridWizardServerModeViewHelper }

  IcxGridWizardServerModeViewHelper = interface
  ['{C15CE8F1-A901-4EAD-8F08-E76043FD3F78}']
    function GetSelectedDataSource: TdxServerModeCustomDataSource;
    procedure SelectDataSource(ADataSource: TdxServerModeCustomDataSource);
  end;

  { TcxGridWizardServerModeTableViewHelper }

  TcxGridWizardServerModeTableViewHelper = class(TcxGridWizardCustomTableViewHelper,
    IcxGridWizardServerModeViewHelper, IcxGridWizardHelperDataSetFieldsSupport)
  private
    FDataSource: TdxServerModeCustomDataSource;
    FDataSourceActiveChecked: Boolean;

    function GetGridView: TcxGridServerModeTableView;
    function GetItem(Index: Integer): TcxGridServerModeColumn;
  protected
    procedure AssignDataSource(AGridView: TcxCustomGridView); virtual;
    procedure CheckDataSourceActivated;

    function GetDataController: TcxGridServerModeDataController; virtual;
    function GetItemCaption(Index: Integer): string; override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    procedure SetItemFieldName(Index: Integer; const AValue: string); override;
    // IcxGridWizardServerModeViewHelper
    function GetSelectedDataSource: TdxServerModeCustomDataSource;
    procedure SelectDataSource(ADataSource: TdxServerModeCustomDataSource);
    // IcxGridWizardHelperDataSetFieldsSupport
    function GetDataSetFields: TFields;

    procedure SaveGridViewData(AView: TcxCustomGridView); override;
  public
    procedure DeleteItem(const AFieldName: string); overload; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; override;
    procedure PreparePreview(APreviewView: TcxCustomGridView); override;

    property Items[Index: Integer]: TcxGridServerModeColumn read GetItem;
    property GridView: TcxGridServerModeTableView read GetGridView;
  end;

  { TcxGridWizardServerModeBandedTableViewHelper }

  TcxGridWizardServerModeBandedTableViewHelper = class(TcxGridWizardServerModeTableViewHelper, IcxGridWizardBandedTableView)
  private
    function GetGridView: TcxGridServerModeBandedTableView;
  protected
    procedure AssignDataSource(AGridView: TcxCustomGridView); override;
    function GetDataController: TcxGridServerModeDataController; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    // IcxGridWizardBandedTableView
    function GetBands: TcxGridBands;
    function GetBandsQuickCustomization: Boolean;
    function GetDefaultBandCaption: string;
    procedure SetBandsQuickCustomization(const AValue: Boolean);
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    //
    property GridView: TcxGridServerModeBandedTableView read GetGridView;
  end;

implementation

{$R cxGridWizardServerModeViewHelper.res}

uses
  cxGridWizardCustomPage, cxGridWizardCommonCustomizeItemsPage, cxGridWizardTableViewOptionsInterfaceElementsPage,
  cxGridWizardTableViewOptionsSummaryPage, cxGridWizardTableViewOptionsFilteringSortingPage,
  cxGridWizardTableViewOptionsBehaviorPage, cxGridWizardTableViewOptionsSizingPage,
  cxGridWizardBandedTableViewOptionsBandsPage, cxGridWizardServerModeViewsDataSourcePage,
  cxGridWizardDBViewsSelectItemsForDisplayPage;

const
  cxGridWizardServerModeTableViewPages: array [0..7] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardServerModeViewsDataSourcePageFrame, TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame, TcxGridWizardTableViewOptionsInterfaceElementsPageFrame,
    TcxGridWizardTableViewOptionsSummaryPageFrame, TcxGridWizardTableViewOptionsFilteringSortingPageFrame,
    TcxGridWizardTableViewOptionsBehaviorPageFrame, TcxGridWizardTableViewOptionsSizingPageFrame);

  cxGridWizardServerModeBandedTableViewPages: array [0..8] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardServerModeViewsDataSourcePageFrame, TcxGridWizardBandedTableViewOptionsBandsPageFrame,
    TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame, TcxGridWizardTableViewOptionsInterfaceElementsPageFrame,
    TcxGridWizardTableViewOptionsSummaryPageFrame, TcxGridWizardTableViewOptionsFilteringSortingPageFrame,
    TcxGridWizardTableViewOptionsBehaviorPageFrame, TcxGridWizardTableViewOptionsSizingPageFrame);

{ TcxGridWizardServerModeTableViewHelper }

procedure TcxGridWizardServerModeTableViewHelper.DeleteItem(const AFieldName: string);
begin
  GridView.GetColumnByFieldName(AFieldName).Free;
end;

class function TcxGridWizardServerModeTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeTableView;
end;

function TcxGridWizardServerModeTableViewHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
var
  AColumn: TcxGridServerModeColumn;
begin
  AColumn := GridView.GetColumnByFieldName(AFieldName);
  if AColumn <> nil then
    Result := AColumn.Index
  else
    Result := -1;
end;

procedure TcxGridWizardServerModeTableViewHelper.PreparePreview(APreviewView: TcxCustomGridView);
begin
  inherited PreparePreview(APreviewView);
  AssignDataSource(APreviewView);
  CheckDataSourceActivated;
end;

procedure TcxGridWizardServerModeTableViewHelper.AssignDataSource(AGridView: TcxCustomGridView);
begin
  TcxGridServerModeTableView(AGridView).DataController.DataSource := FDataSource;
end;

procedure TcxGridWizardServerModeTableViewHelper.CheckDataSourceActivated;
begin
  if not FDataSourceActiveChecked then
  try
    FDataSourceActiveChecked := True;
    if FDataSource <> nil then
      FDataSource.Active := True;
  except
    // do nothing;
  end;
end;

function TcxGridWizardServerModeTableViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := inherited GetItemCaption(Index);
  if Result = '' then
    Result := ItemFieldName[Index];
end;

function TcxGridWizardServerModeTableViewHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := Items[Index].DataBinding.FieldName;
end;

function TcxGridWizardServerModeTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardServerModeTableViewPages);
end;

function TcxGridWizardServerModeTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardServerModeTableViewPages[Index];
end;

procedure TcxGridWizardServerModeTableViewHelper.SetItemFieldName(Index: Integer; const AValue: string);
begin
  Items[Index].DataBinding.FieldName := AValue;
end;

function TcxGridWizardServerModeTableViewHelper.GetDataController: TcxGridServerModeDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardServerModeTableViewHelper.GetSelectedDataSource: TdxServerModeCustomDataSource;
begin
  Result := FDataSource;
end;

procedure TcxGridWizardServerModeTableViewHelper.SelectDataSource(ADataSource: TdxServerModeCustomDataSource);
begin
  FDataSourceActiveChecked := False;
  FDataSource := ADataSource;
end;

function TcxGridWizardServerModeTableViewHelper.GetDataSetFields: TFields;
begin
  if FDataSource <> nil then
    Result := FDataSource.Fields
  else
    raise Exception.Create(ClassName + '.GetDataSetFields failed');
end;

procedure TcxGridWizardServerModeTableViewHelper.SaveGridViewData(AView: TcxCustomGridView);
begin
  AssignDataSource(AView);
  inherited SaveGridViewData(AView);
end;

function TcxGridWizardServerModeTableViewHelper.GetGridView: TcxGridServerModeTableView;
begin
  Result := TcxGridServerModeTableView(inherited GridView);
end;

function TcxGridWizardServerModeTableViewHelper.GetItem(Index: Integer): TcxGridServerModeColumn;
begin
  Result := GridView.Columns[Index];
end;

{ TcxGridWizardServerModeBandedTableViewHelper }

procedure TcxGridWizardServerModeBandedTableViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
var
  AForm: TcxGridBandedTableCustomizationForm;
begin
  AForm := ACustomizationForm as TcxGridBandedTableCustomizationForm;
  AForm.BandsPage.Caption := cxGetResourceString(@scxgwCustomizationFormBandsTab);
  AForm.ColumnsPage.Caption := cxGetResourceString(@scxgwCustomizationFormColumnsTab);
end;

class function TcxGridWizardServerModeBandedTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeBandedTableView;
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardServerModeBandedTableViewPages);
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardServerModeBandedTableViewPages[Index];
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetBands: TcxGridBands;
begin
  Result := GridView.Bands;
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetBandsQuickCustomization: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomization;
end;

procedure TcxGridWizardServerModeBandedTableViewHelper.AssignDataSource(AGridView: TcxCustomGridView);
begin
  TcxGridServerModeBandedTableView(AGridView).DataController.DataSource := FDataSource;
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetDataController: TcxGridServerModeDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetDefaultBandCaption: string;
begin
  Result := GridView.Bands.ItemClass.ClassName;
end;

procedure TcxGridWizardServerModeBandedTableViewHelper.SetBandsQuickCustomization(const AValue: Boolean);
begin
  GridView.OptionsCustomize.BandsQuickCustomization := AValue;
end;

function TcxGridWizardServerModeBandedTableViewHelper.GetGridView: TcxGridServerModeBandedTableView;
begin
  Result := TcxGridServerModeBandedTableView(inherited GridView);
end;

initialization
  cxGridWizardHelperInfoList.Add(TcxGridWizardServerModeBandedTableViewHelper, HInstance);
  cxGridWizardHelperInfoList.Add(TcxGridWizardServerModeTableViewHelper, HInstance);
end.
