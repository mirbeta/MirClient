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

unit cxGridWizardTableViewHelper;

{$I cxVer.inc}

interface

uses
  SysUtils, Forms,
  dxCore, cxGraphics, dxCustomWizardControl, dxWizardControl, cxClasses, cxGridWizardStrs, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridLevel, cxGridWizardCustomHelper,
  cxGridDBDataDefinitions, cxDBData, cxEdit;

type

  { TcxGridWizardCustomTableViewHelper }

  TcxGridWizardCustomTableViewHelper = class(TcxGridWizardCustomTableLikeViewHelper)
  private
    function GetGridView: TcxGridTableView;
    function GetItem(Index: Integer): TcxGridColumn;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
    function GetDefaultItemCaption: string; override;

    procedure RestoreGridViewOptionsData; override;
    procedure SaveGridViewOptionsData; override;

    property Items[Index: Integer]: TcxGridColumn read GetItem;
    property GridView: TcxGridTableView read GetGridView;
  end;

  { TcxGridWizardTableViewHelper }

  TcxGridWizardTableViewHelper = class(TcxGridWizardCustomTableViewHelper)
  private
    FSavedNavigatorVisibleOptionValue: Boolean;
  protected
    function CanSaveData: Boolean; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
  public
    class function GetGridViewClass: TcxCustomGridViewClass; override;

    procedure PrepareForCustomization; override;
    procedure RestoreAfterCustomization; override;
  end;

  { TcxGridWizardDBTableViewHelper }

  TcxGridWizardDBTableViewHelper = class(TcxGridWizardCustomTableViewHelper, IcxGridWizardHelperDBDataControllerSupport)
  private
    function GetGridView: TcxGridDBTableView;
    function GetItem(Index: Integer): TcxGridDBColumn;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
    function GetItemCaption(Index: Integer): string; override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    procedure SetItemFieldName(Index: Integer; const AValue: string); override;
    // IcxGridWizardHelperDBDataControllerSupport
    function GetDataController: TcxDBDataController;
  public
    procedure DeleteItem(const AFieldName: string); overload; override;
    class function CanBeMasterView: Boolean; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; override;

    property Items[Index: Integer]: TcxGridDBColumn read GetItem;
    property GridView: TcxGridDBTableView read GetGridView;
  end;

implementation

{$R cxGridWizardTableViewHelper.res}

uses
  cxGridWizardCustomPage, cxGridWizardDBViewsDataSourcePage, cxGridWizardDBViewsSelectItemsForDisplayPage,
  cxGridWizardCommonCustomizeItemsPage, cxGridWizardDBViewsDataLoadingSettingsPage,
  cxGridWizardTableViewOptionsInterfaceElementsPage, cxGridWizardTableViewOptionsInplaceEditFormPage,
  cxGridWizardTableViewOptionsInplaceEditFormLayoutPage, cxGridWizardTableViewOptionsSummaryPage,
  cxGridWizardTableViewOptionsFilteringSortingPage, cxGridWizardTableViewOptionsBehaviorPage,
  cxGridWizardTableViewOptionsSizingPage, cxGridWizardUnboundViewsSelectItemsForDisplayPage, cxGridCommon;

const
  cxGridWizardTableViewPages: array [0..8] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame, TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardTableViewOptionsInterfaceElementsPageFrame, TcxGridWizardTableViewOptionsInplaceEditFormPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame, TcxGridWizardTableViewOptionsSummaryPageFrame,
    TcxGridWizardTableViewOptionsFilteringSortingPageFrame, TcxGridWizardTableViewOptionsBehaviorPageFrame,
    TcxGridWizardTableViewOptionsSizingPageFrame);

  cxGridWizardDBTableViewPages: array [0..10] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardDBViewsDataSourcePageFrame, TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame, TcxGridWizardDBViewsDataLoadingSettingsPageFrame,
    TcxGridWizardTableViewOptionsInterfaceElementsPageFrame, TcxGridWizardTableViewOptionsInplaceEditFormPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame, TcxGridWizardTableViewOptionsSummaryPageFrame,
    TcxGridWizardTableViewOptionsFilteringSortingPageFrame, TcxGridWizardTableViewOptionsBehaviorPageFrame,
    TcxGridWizardTableViewOptionsSizingPageFrame);

{ TcxGridWizardCustomTableViewHelper }

procedure TcxGridWizardCustomTableViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
begin
  (ACustomizationForm as TcxGridTableCustomizationForm).ColumnsPage.Caption := cxGetResourceString(@scxgwCustomizationFormColumnsTab);
end;

function TcxGridWizardCustomTableViewHelper.GetDefaultItemCaption: string;
begin
  Result := 'Column';
end;

procedure TcxGridWizardCustomTableViewHelper.RestoreGridViewOptionsData;
begin
  GridView.OptionsData.Appending := FOriginalGridViewOptionsData.Appending;
  GridView.OptionsData.Deleting := FOriginalGridViewOptionsData.Deleting;
  GridView.OptionsData.Editing := FOriginalGridViewOptionsData.Editing;
  GridView.OptionsData.Inserting := FOriginalGridViewOptionsData.Inserting;
end;

procedure TcxGridWizardCustomTableViewHelper.SaveGridViewOptionsData;
begin
  FOriginalGridViewOptionsData.Appending := GridView.OptionsData.Appending;
  FOriginalGridViewOptionsData.Deleting := GridView.OptionsData.Deleting;
  FOriginalGridViewOptionsData.Editing := GridView.OptionsData.Editing;
  FOriginalGridViewOptionsData.Inserting := GridView.OptionsData.Inserting;
  GridView.OptionsData.Appending := False;
  GridView.OptionsData.Deleting := False;
  GridView.OptionsData.Editing := False;
  GridView.OptionsData.Inserting := False;
end;

procedure TcxGridWizardCustomTableViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  ATableView: TcxGridTableView;
  I: Integer;
begin
  ATableView := AView as TcxGridTableView;
  for I := 0 to ATableView.ColumnCount - 1 do
    ATableView.Columns[I].Name := CreateUniqueName(GetParentForm(ATableView.Control),
      ATableView, ATableView.Columns[I], ScxGridPrefixName, '');
end;

function TcxGridWizardCustomTableViewHelper.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridWizardCustomTableViewHelper.GetItem(Index: Integer): TcxGridColumn;
begin
  Result := GridView.Columns[Index];
end;

{ TcxGridWizardTableViewHelper }

class function TcxGridWizardTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridTableView;
end;

procedure TcxGridWizardTableViewHelper.PrepareForCustomization;
begin
  inherited PrepareForCustomization;
  FSavedNavigatorVisibleOptionValue := GridView.Navigator.Visible;
  GridView.Navigator.Visible := True;
  GridView.OptionsData.Appending := True;
  GridView.OptionsData.Deleting := True;
  GridView.OptionsData.Editing := True;
  GridView.OptionsData.Inserting := True;
end;

procedure TcxGridWizardTableViewHelper.RestoreAfterCustomization;
begin
  inherited RestoreAfterCustomization;
  GridView.Navigator.Visible := FSavedNavigatorVisibleOptionValue;
  GridView.OptionsData.Appending := False;
  GridView.OptionsData.Deleting := False;
  GridView.OptionsData.Editing := False;
  GridView.OptionsData.Inserting := False;
end;

function TcxGridWizardTableViewHelper.CanSaveData: Boolean;
begin
  Result := True;
end;

function TcxGridWizardTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardTableViewPages);
end;

function TcxGridWizardTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardTableViewPages[Index]
end;

{ TcxGridWizardDBTableViewHelper }

procedure TcxGridWizardDBTableViewHelper.DeleteItem(const AFieldName: string);
begin
  GridView.GetColumnByFieldName(AFieldName).Free;
end;

class function TcxGridWizardDBTableViewHelper.CanBeMasterView: Boolean;
begin
  Result := True;
end;

class function TcxGridWizardDBTableViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBTableView;
end;

function TcxGridWizardDBTableViewHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
var
  AColumn: TcxGridDBColumn;
begin
  AColumn := GridView.GetColumnByFieldName(AFieldName);
  if AColumn <> nil then
    Result := AColumn.Index
  else
    Result := -1;
end;

procedure TcxGridWizardDBTableViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  ATableView: TcxGridDBTableView;
  I: Integer;
begin
  ATableView := AView as TcxGridDBTableView;
  for I := 0 to ATableView.ColumnCount - 1 do
    ATableView.Columns[I].Name := CreateUniqueName(GetParentForm(ATableView.Control),
      ATableView, ATableView.Columns[I], ScxGridPrefixName, ATableView.Columns[I].DataBinding.FieldName);
end;

function TcxGridWizardDBTableViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := inherited GetItemCaption(Index);
  if Result = '' then
    Result := ItemFieldName[Index];
end;

function TcxGridWizardDBTableViewHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := Items[Index].DataBinding.FieldName;
end;

function TcxGridWizardDBTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardDBTableViewPages);
end;

function TcxGridWizardDBTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardDBTableViewPages[Index];
end;

procedure TcxGridWizardDBTableViewHelper.SetItemFieldName(Index: Integer; const AValue: string);
begin
  Items[Index].DataBinding.FieldName := AValue;
end;

function TcxGridWizardDBTableViewHelper.GetDataController: TcxDBDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardDBTableViewHelper.GetGridView: TcxGridDBTableView;
begin
  Result := TcxGridDBTableView(inherited GetGridView);
end;

function TcxGridWizardDBTableViewHelper.GetItem(Index: Integer): TcxGridDBColumn;
begin
  Result := GridView.Columns[Index];
end;

initialization
  cxGridWizardHelperInfoList.Add(TcxGridWizardTableViewHelper, HInstance);
  cxGridWizardHelperInfoList.Add(TcxGridWizardDBTableViewHelper, HInstance);
end.
