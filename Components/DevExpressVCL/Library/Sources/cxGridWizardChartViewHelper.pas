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

unit cxGridWizardChartViewHelper;

{$I cxVer.inc}

interface

uses
  cxGridWizardCustomHelper, cxGridCustomView, cxGridChartView, cxGridDBChartView, cxGridLevel, SysUtils, cxGraphics,
  dxCustomWizardControl, dxWizardControl, cxGridDBDataDefinitions, cxDBData, cxEdit;

type
  { TcxGridWizardCustomChartViewHelper }

  TcxGridWizardCustomChartViewHelper = class(TcxGridWizardCustomHelper)
  private
    FSavedCustomizeButtonOptionValue: Boolean;
    FSavedOptionsCustomizationOptionValue: Boolean;
    FSavedSeriesCustomizationOptionValue: Boolean;

    function GetGridView: TcxGridChartView;
    function GetItem(Index: Integer): TcxGridChartSeries;
  protected
    function GetItemCaption(Index: Integer): string; override;
    function GetItemsCount: Integer; override;
    function GetItemsVisibleCount: Integer; override;
    function GetItemVisible(Index: Integer): Boolean; override;
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
    procedure SetItemCaption(Index: Integer; const AValue: string); override;
    procedure SetItemVisible(Index: Integer; const AValue: Boolean); override;
  public
    procedure ChangeItemIndex(const AOldIndex, ANewIndex: Integer); override;
    procedure DeleteItem(const AItemIndex: Integer); overload; override;
    function GetDefaultItemCaption: string; override;

    procedure PrepareForCustomization; override;
    procedure RestoreAfterCustomization; override;
    procedure RestoreGridViewOptionsData; override;
    procedure SaveGridViewOptionsData; override;

    property Item[Index: Integer]: TcxGridChartSeries read GetItem;
    property GridView: TcxGridChartView read GetGridView;
  end;

  { TcxGridWizardChartViewHelper }

  TcxGridWizardChartViewHelper = class(TcxGridWizardCustomChartViewHelper)
  protected
    function CanSaveData: Boolean; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
  public
    class function GetGridViewClass: TcxCustomGridViewClass; override;
  end;

  { TcxGridWizardDBChartViewHelper }

  TcxGridWizardDBChartViewHelper = class(TcxGridWizardCustomChartViewHelper, IcxGridWizardHelperDBDataControllerSupport)
  private
    function GetGridView: TcxGridDBChartView;
    function GetItem(Index: Integer): TcxGridDBChartSeries;
  protected
    function GetItemCaption(Index: Integer): string; override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    // IcxGridWizardHelperDBDataControllerSupport
    function GetDataController: TcxDBDataController;
  public
    procedure DeleteItem(const AFieldName: string); overload; override;
    function GetDefaultItemCaption: string; override;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;

    property Item[Index: Integer]: TcxGridDBChartSeries read GetItem;
    property GridView: TcxGridDBChartView read GetGridView;
  end;

implementation

uses
  cxClasses, Forms, cxGridWizardCustomPage, cxGridWizardDBViewsDataSourcePage, cxGridWizardDBViewsSelectItemsForDisplayPage,
  cxGridWizardCommonCustomizeItemsPage, cxGridWizardChartViewOptionsViewSettingsPage, cxGridWizardUnboundViewsSelectItemsForDisplayPage,
  cxGridCommon;

const
  cxGridWizardChartViewPages: array [0..2] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame, TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardChartViewOptionsViewSettingsPageFrame);
  cxGridWizardDBChartViewPages: array [0..3] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardDBViewsDataSourcePageFrame, TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardChartViewOptionsViewSettingsPageFrame);

{$R cxGridWizardChartViewHelper.res}

{ TcxGridWizardCustomChartViewHelper }

procedure TcxGridWizardCustomChartViewHelper.ChangeItemIndex(const AOldIndex, ANewIndex: Integer);
begin
  Item[AOldIndex].Index := ANewIndex;
end;

procedure TcxGridWizardCustomChartViewHelper.DeleteItem(const AItemIndex: Integer);
begin
  Item[AItemIndex].Free;
end;

function TcxGridWizardCustomChartViewHelper.GetDefaultItemCaption: string;
begin
  Result := TcxGridChartSeries.ClassName;
end;

procedure TcxGridWizardCustomChartViewHelper.PrepareForCustomization;
begin
  FSavedCustomizeButtonOptionValue := GridView.ToolBox.CustomizeButton;
  FSavedOptionsCustomizationOptionValue := GridView.OptionsCustomize.OptionsCustomization;
  FSavedSeriesCustomizationOptionValue := GridView.OptionsCustomize.SeriesCustomization;
  GridView.ToolBox.CustomizeButton := False;
  GridView.OptionsCustomize.OptionsCustomization := True;
  GridView.OptionsCustomize.SeriesCustomization := True;
end;

procedure TcxGridWizardCustomChartViewHelper.RestoreAfterCustomization;
begin
  GridView.ToolBox.CustomizeButton := FSavedCustomizeButtonOptionValue;
  GridView.OptionsCustomize.OptionsCustomization := FSavedOptionsCustomizationOptionValue;
  GridView.OptionsCustomize.SeriesCustomization := FSavedSeriesCustomizationOptionValue;
end;

procedure TcxGridWizardCustomChartViewHelper.RestoreGridViewOptionsData;
begin
  // do nothing
end;

procedure TcxGridWizardCustomChartViewHelper.SaveGridViewOptionsData;
begin
  // do nothing
end;

procedure TcxGridWizardCustomChartViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  AChartView: TcxGridChartView;
  I: Integer;
begin
  AChartView := AView as TcxGridChartView;
  for I := 0 to AChartView.SeriesCount - 1 do
    AChartView.Series[I].Name := CreateUniqueName(GetParentForm(AChartView.Control),
      AChartView, AChartView.Series[I], ScxGridPrefixName, '');
end;

function TcxGridWizardCustomChartViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := Item[Index].DisplayText;
end;

function TcxGridWizardCustomChartViewHelper.GetItemsCount: Integer;
begin
  Result := GridView.SeriesCount;
end;

function TcxGridWizardCustomChartViewHelper.GetItemsVisibleCount: Integer;
begin
  Result := GridView.VisibleSeriesCount;
end;

function TcxGridWizardCustomChartViewHelper.GetItemVisible(Index: Integer): Boolean;
begin
  Result := Item[Index].Visible;
end;

procedure TcxGridWizardCustomChartViewHelper.SetItemCaption(Index: Integer; const AValue: string);
begin
  Item[Index].DisplayText := AValue;
end;

procedure TcxGridWizardCustomChartViewHelper.SetItemVisible(Index: Integer; const AValue: Boolean);
begin
  Item[Index].Visible := AValue;
end;

function TcxGridWizardCustomChartViewHelper.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridWizardCustomChartViewHelper.GetItem(Index: Integer): TcxGridChartSeries;
begin
  Result := GridView.Series[Index];
end;

{ TcxGridWizardChartViewHelper }

class function TcxGridWizardChartViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridChartView;
end;

function TcxGridWizardChartViewHelper.CanSaveData: Boolean;
begin
  Result := True;
end;

function TcxGridWizardChartViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardChartViewPages);
end;

function TcxGridWizardChartViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardChartViewPages[Index];
end;

{ TcxGridWizardDBChartViewHelper }

procedure TcxGridWizardDBChartViewHelper.DeleteItem(const AFieldName: string);
begin
  GridView.FindSeriesByFieldName(AFieldName).Free;
end;

function TcxGridWizardDBChartViewHelper.GetDefaultItemCaption: string;
begin
  Result := TcxGridDBChartSeries.ClassName;
end;

function TcxGridWizardDBChartViewHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
var
  ASeries: TcxGridDBChartSeries;
begin
  ASeries := GridView.FindSeriesByFieldName(AFieldName);
  if ASeries <> nil then
    Result := ASeries.Index
  else
    Result := -1;
end;

class function TcxGridWizardDBChartViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBChartView;
end;

function TcxGridWizardDBChartViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := inherited GetItemCaption(Index);
  if (Result = '') and (ItemFieldName[Index] <> '') then
    Result := ItemFieldName[Index];
end;

function TcxGridWizardDBChartViewHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := Item[Index].DataBinding.FieldName;
end;

function TcxGridWizardDBChartViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardDBChartViewPages);
end;

function TcxGridWizardDBChartViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardDBChartViewPages[Index];
end;

function TcxGridWizardDBChartViewHelper.GetDataController: TcxDBDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardDBChartViewHelper.GetGridView: TcxGridDBChartView;
begin
  Result := TcxGridDBChartView(inherited GetGridView);
end;

function TcxGridWizardDBChartViewHelper.GetItem(Index: Integer): TcxGridDBChartSeries;
begin
  Result := GridView.Series[Index];
end;

//initialization
//  cxGridWizardHelperInfoList.Add(TcxGridWizardChartViewHelper, HInstance);
//  cxGridWizardHelperInfoList.Add(TcxGridWizardDBChartViewHelper, HInstance);
end.

