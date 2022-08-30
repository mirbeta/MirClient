{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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

unit cxGridHeaderPopupMenuItems;

{$I cxVer.inc}

interface

uses
  Classes, dxCore, cxGridMenuOperations, cxFindPanel;

type
  TcxGridHeaderPopupMenuOperation = class(TcxGridTableColumnMenuOperation);

  TcxGridSortingMenuOperation = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetVisible: Boolean; override;
    function IsSorted: Boolean;
  end;

  TcxGridSortColumn = class(TcxGridSortingMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetEnabled: Boolean; override;
  end;

  TcxGridSortColumnAsc = class(TcxGridSortColumn)
  protected
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridSortColumnDesc = class(TcxGridSortColumn)
  protected
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridClearSorting = class(TcxGridSortingMenuOperation)
  protected
    function GetEnabled: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridGroupByThisField = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetCaption: string; override;
    function GetEnabled: Boolean; override;
    function GetImageResourceName: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridGroupByBox = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetImageResourceName: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridAlignmentSubMenu = class(TcxGridHeaderPopupMenuOperation)
  protected
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridAlign = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; Override;
    function GetEnabled: Boolean; override;
  public
    class function GetParentOperationClass: TcxGridPopupMenuOperationClass; override;
  end;

  TcxGridAlignLeft = class(TcxGridAlign)
  protected
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridAlignRight = class(TcxGridAlign)
  protected
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridAlignCenter = class(TcxGridAlign)
  protected
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridBestFit = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetEnabled: Boolean; override;
    function GetImageResourceName: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridBestFitAllColumns = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridRemoveColumn = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridFieldChooser = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridShowFooter = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; Override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  TcxGridShowGroupFooter = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetEnabled: Boolean; override;
    function GetImageResourceName: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridShowFindPanel = class(TcxGridHeaderPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetImageResourceName: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridHeaderPopupMenuOperations = class(TcxGridPopupMenuOperations)
  protected
    procedure AddItems; override;
  end;

implementation

uses
  cxCustomData, cxEdit, cxGridCustomTableView, cxGridTableView, cxGridPopupMenuConsts;

const
  AImageName = 'HdrImg';

{ TcxGridSortingMenuOperation }

procedure TcxGridSortingMenuOperation.Execute(Sender: TObject);
begin
  if IsSorted then
    HitGridView.DataController.ClearSorting(True);
end;

function TcxGridSortingMenuOperation.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.ColumnSorting;
end;

function TcxGridSortingMenuOperation.IsSorted: Boolean;
var
  I: Integer;
begin
  Result := False;
  with HitGridView do
    if SortedItemCount > 0 then
      for I := 0 to SortedItemCount - 1 do
        if TcxCustomGridTableItemAccess.GetGroupIndex(SortedItems[I]) = -1 then
        begin
          Result := True;
          Break;
        end;
end;

{ TcxGridSortColumn }

procedure TcxGridSortColumn.Execute(Sender: TObject);
var
  ASortOrder: TcxDataSortOrder;
begin
  HitGridView.BeginSortingUpdate;
  try
    inherited;
    case Tag of
      0: ASortOrder := soAscending;
      1: ASortOrder := soDescending;
    else
      ASortOrder := soNone;
    end;
    HitColumn.SortOrder := ASortOrder;
  finally
    HitGridView.EndSortingUpdate;
  end;
end;

function TcxGridSortColumn.GetDown: Boolean;
begin
  if Tag = 0 then
    Result := HitColumn.SortOrder = soAscending
  else
    Result := HitColumn.SortOrder = soDescending;
end;

function TcxGridSortColumn.GetEnabled: Boolean;
begin
  Result := TcxCustomGridTableItemAccess.CanSort(HitColumn);
end;

{ TcxGridSortColumnAsc }

constructor TcxGridSortColumnAsc.Create;
begin
  inherited;
  ResCaption := @cxSGridSortColumnAsc;
  Tag := 0;
end;

function TcxGridSortColumnAsc.GetImageResourceName: string;
begin
  Result := AImageName + '1';
end;

{ TcxGridSortColumnDesc }

constructor TcxGridSortColumnDesc.Create;
begin
  inherited;
  ResCaption := @cxSGridSortColumnDesc;
  Tag := 1;
end;

function TcxGridSortColumnDesc.GetImageResourceName: string;
begin
  Result := AImageName + '2';
end;

{ TcxGridClearSorting }

constructor TcxGridClearSorting.Create;
begin
  inherited;
  ResCaption := @cxSGridClearSorting;
end;

function TcxGridClearSorting.GetEnabled: Boolean;
begin
  Result := IsSorted;
end;

{ TcxGridGroupBy }

constructor TcxGridGroupByThisField.Create;
begin
  inherited;
  ResCaption := @cxSGridGroupByThisField;
end;

procedure TcxGridGroupByThisField.Execute(Sender: TObject);
var
  AGroupIndex: Integer;
begin
  HitGridView.BeginGroupingUpdate;
  try
    HitGridView.OptionsView.GroupByBox := True;
    if HitColumn.GroupIndex <> -1 then
      AGroupIndex := -1
    else
      AGroupIndex := HitGridView.GroupedColumnCount;
    HitColumn.GroupBy(AGroupIndex);
  finally
    HitGridView.EndGroupingUpdate;
  end;
  TcxGridTableViewAccess.DoColumnPosChanged(HitGridView, HitColumn);
end;

function TcxGridGroupByThisField.GetCaption: string;
begin
  if HitColumn.GroupIndex <> -1 then
    ResCaption := @cxSGridRemoveThisGroupItem
  else
    ResCaption := @cxSGridGroupByThisField;
  Result := inherited GetCaption;
end;

function TcxGridGroupByThisField.GetEnabled: Boolean;
begin
  Result := TcxCustomGridTableItemAccess.CanGroup(HitColumn);
end;

function TcxGridGroupByThisField.GetImageResourceName: string;
begin
  Result := AImageName + '3';
end;

function TcxGridGroupByThisField.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.ColumnGrouping;
end;

{ TcxGridGroupByBox }

constructor TcxGridGroupByBox.Create;
begin
  inherited;
  ResCaption := @cxSGridGroupByBox;
end;

procedure TcxGridGroupByBox.Execute(Sender: TObject);
begin
  GridOperationHelper.DoShowGroupingPanel(not GridOperationHelper.IsGroupingPanelShowing);
end;

function TcxGridGroupByBox.GetDown: Boolean;
begin
  Result := GridOperationHelper.IsGroupingPanelShowing;
end;

function TcxGridGroupByBox.GetImageResourceName: string;
begin
  Result := AImageName + '7';
end;

function TcxGridGroupByBox.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.ColumnGrouping;
end;

{ TcxGridAlignmentSubMenu }

constructor TcxGridAlignmentSubMenu.Create;
begin
  inherited;
  ResCaption := @cxSGridAlignmentSubMenu;
end;

function TcxGridAlignmentSubMenu.GetVisible: Boolean;
begin
  Result := HitColumn.Properties <> nil;
end;

{ TcxGridAlignLeft }

procedure TcxGridAlign.Execute(Sender: TObject);
begin
  case Tag of
    0: HitColumn.GetProperties.Alignment.Horz := taLeftJustify;
    1: HitColumn.GetProperties.Alignment.Horz := taRightJustify;
    2: HitColumn.GetProperties.Alignment.Horz := taCenter;
  end;
end;

function TcxGridAlign.GetDown: Boolean;
var
  AProperties: TcxCustomEditProperties;
begin
  AProperties := HitColumn.GetProperties;
  case Tag of
    0: Result := AProperties.Alignment.Horz = taLeftJustify;
    1: Result := AProperties.Alignment.Horz = taRightJustify;
  else
    Result := AProperties.Alignment.Horz = taCenter;
  end;
end;

function TcxGridAlign.GetEnabled: Boolean;
begin
  Result := esoHorzAlignment in HitColumn.GetProperties.GetSupportedOperations;
end;

class function TcxGridAlign.GetParentOperationClass: TcxGridPopupMenuOperationClass;
begin
  Result := TcxGridAlignmentSubMenu;
end;

{ TcxGridAlignLeft }

constructor TcxGridAlignLeft.Create;
begin
  inherited;
  ResCaption := @cxSGridAlignLeft;
  Tag := 0;
end;

function TcxGridAlignLeft.GetImageResourceName: string;
begin
  Result := AImageName + '4';
end;

{ TcxGridAlignRight }

constructor TcxGridAlignRight.Create;
begin
  inherited;
  ResCaption := @cxSGridAlignRight;
  Tag := 1;
end;

function TcxGridAlignRight.GetImageResourceName: string;
begin
  Result := AImageName + '5';
end;

{ TcxGridAlignCenter }

constructor TcxGridAlignCenter.Create;
begin
  inherited;
  ResCaption := @cxSGridAlignCenter;
  Tag := 2;
end;

function TcxGridAlignCenter.GetImageResourceName: string;
begin
  Result := AImageName + '6';
end;

{ TcxGridBestFit }

constructor TcxGridBestFit.Create;
begin
  inherited;
  ResCaption := @cxSGridBestFit;
end;

procedure TcxGridBestFit.Execute(Sender: TObject);
begin
  HitColumn.ApplyBestFit(True, True);
end;

function TcxGridBestFit.GetEnabled: Boolean;
begin
  Result := TcxCustomGridTableItemAccess.CanHorzSize(HitColumn);
end;

function TcxGridBestFit.GetImageResourceName: string;
begin
  Result := AImageName + '9';
end;

function TcxGridBestFit.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.ColumnHorzSizing;
end;

{ TcxGridBestFitAllColumns }

constructor TcxGridBestFitAllColumns.Create;
begin
  inherited;
  ResCaption := @cxSGridBestFitAllColumns;
end;

procedure TcxGridBestFitAllColumns.Execute(Sender: TObject);
begin
  TcxCustomGridTableView(HitGridView).ApplyBestFit(nil, True, True);
end;

function TcxGridBestFitAllColumns.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.ColumnHorzSizing;
end;

{ TcxGridCustColumns }

constructor TcxGridRemoveColumn.Create;
begin
  inherited;
  ResCaption := @cxSGridRemoveColumn;
end;

procedure TcxGridRemoveColumn.Execute(Sender: TObject);
begin
  HitColumn.Visible := False;
  TcxGridTableViewAccess.DoColumnPosChanged(HitGridView, HitColumn);
end;

function TcxGridRemoveColumn.GetVisible: Boolean;
begin
  Result := HitColumn.Visible and
    HitColumn.VisibleForCustomization and TcxCustomGridTableItemAccess.CanHide(HitColumn);
end;

{ TcxGridFieldChooser }

constructor TcxGridFieldChooser.Create;
begin
  inherited;
  ResCaption := @cxSGridFieldChooser;
end;

procedure TcxGridFieldChooser.Execute(Sender: TObject);
begin
  GridOperationHelper.DoShowColumnCustomizing(
    not GridOperationHelper.IsColumnsCustomizingShowing);
end;

function TcxGridFieldChooser.GetDown: Boolean;
begin
  Result := GridOperationHelper.IsColumnsCustomizingShowing;
end;

function TcxGridFieldChooser.GetImageResourceName: string;
begin
  Result := AImageName + '8';
end;

{ TcxGridShowFooter }

constructor TcxGridShowFooter.Create;
begin
  inherited;
  ResCaption := @cxSGridShowFooter;
end;

procedure TcxGridShowFooter.Execute(Sender: TObject);
begin
  GridOperationHelper.DoShowSummaryFooter(
    not GridOperationHelper.IsSummaryFooterShowing);
end;

function TcxGridShowFooter.GetDown: Boolean;
begin
  Result := GridOperationHelper.IsSummaryFooterShowing;
end;

function TcxGridShowFooter.GetImageResourceName: string;
begin
  Result := AImageName + '10';
end;

{ TcxGridShowGroupFooter }

constructor TcxGridShowGroupFooter.Create;
begin
  inherited;
  ResCaption := @cxSGridShowGroupFooter;
end;

procedure TcxGridShowGroupFooter.Execute(Sender: TObject);
begin
  with HitGridView.OptionsView do
    if GroupFooters = gfInvisible then
      GroupFooters := PrevGroupFooters
    else
      GroupFooters := gfInvisible;
end;

function TcxGridShowGroupFooter.GetDown: Boolean;
begin
  Result := HitGridView.OptionsView.GroupFooters <> gfInvisible;
end;

function TcxGridShowGroupFooter.GetEnabled: Boolean;
begin
  Result := HitGridView.GroupedColumnCount <> 0;
end;

function TcxGridShowGroupFooter.GetImageResourceName: string;
begin
  Result := AImageName + '11';
end;

function TcxGridShowGroupFooter.GetVisible: Boolean;
begin
  with HitGridView do
    Result := (GroupedColumnCount <> 0) or OptionsCustomize.ColumnGrouping;
end;

{ TcxGridShowFindPanel }

constructor TcxGridShowFindPanel.Create;
begin
  inherited Create;
  ResCaption := @cxSGridShowFindPanel;
end;

procedure TcxGridShowFindPanel.Execute(Sender: TObject);
var
  AController: TcxGridTableController;
begin
  AController := HitGridView.Controller;
  if AController.IsFindPanelVisible then
    AController.HideFindPanel
  else
    AController.ShowFindPanel;
end;

function TcxGridShowFindPanel.GetDown: Boolean;
begin
  Result := HitGridView.Controller.IsFindPanelVisible;
end;

function TcxGridShowFindPanel.GetImageResourceName: string;
begin
  Result := AImageName + '12';
end;

function TcxGridShowFindPanel.GetVisible: Boolean;
begin
  Result := HitGridView.FindPanel.DisplayMode = fpdmManual;
end;

{ TcxGridHeaderPopupMenuOperations }

procedure TcxGridHeaderPopupMenuOperations.AddItems;
begin
  AddItem(TcxGridSortColumnAsc);
  AddItem(TcxGridSortColumnDesc);
  AddItem(TcxGridClearSorting);
  AddItem(TcxGridGroupByThisField).BeginGroup := True;
  AddItem(TcxGridGroupByBox);
  AddItem(TcxGridShowFooter).BeginGroup := True;
  AddItem(TcxGridShowGroupFooter);
  AddItem(TcxGridRemoveColumn).BeginGroup := True;
  AddItem(TcxGridFieldChooser);
  AddItem(TcxGridAlignmentSubMenu).BeginGroup := True;
  AddItem(TcxGridAlignLeft);
  AddItem(TcxGridAlignRight);
  AddItem(TcxGridAlignCenter);
  AddItem(TcxGridBestFit);
  AddItem(TcxGridBestFitAllColumns).BeginGroup := True;
  AddItem(TcxGridShowFindPanel).BeginGroup := True;
end;

end.
