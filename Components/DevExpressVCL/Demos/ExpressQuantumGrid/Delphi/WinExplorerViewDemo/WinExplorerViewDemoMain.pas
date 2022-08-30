unit WinExplorerViewDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseForm, cxLookAndFeels, cxGridCardView, cxStyles, cxGridTableView,
  cxClasses, Menus, ComCtrls, StdCtrls, cxGraphics, cxControls, dxCore,
  cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxNavigator, cxGridCustomView, cxGridCustomTableView, cxGridWinExplorerView,
  cxGridDBWinExplorerView, cxGridLevel, cxGrid, CarsDataForGrid, DB, cxDBData,
  cxGridDBTableView, cxImage, cxContainer, cxGroupBox, dxGalleryControl,
  dxGallery, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCheckBox;

type
  TfrmMain = class(TfmBaseForm)
    Level: TcxGridLevel;
    Grid: TcxGrid;
    WinExplorerView: TcxGridDBWinExplorerView;
    WinExplorerViewTrademark: TcxGridDBWinExplorerViewItem;
    WinExplorerViewName: TcxGridDBWinExplorerViewItem;
    WinExplorerViewCategory: TcxGridDBWinExplorerViewItem;
    WinExplorerViewBodyStyle: TcxGridDBWinExplorerViewItem;
    WinExplorerViewTransmissionTypeName: TcxGridDBWinExplorerViewItem;
    WinExplorerViewDescription: TcxGridDBWinExplorerViewItem;
    WinExplorerViewImage: TcxGridDBWinExplorerViewItem;
    WinExplorerViewPhoto: TcxGridDBWinExplorerViewItem;
    WinExplorerViewInStock: TcxGridDBWinExplorerViewItem;
    GroupBox: TcxGroupBox;
    gcDisplayModes: TdxGalleryControl;
    gcgGroup: TdxGalleryControlGroup;
    gciExtraLargeIcons: TdxGalleryControlItem;
    gciLargeIcons: TdxGalleryControlItem;
    gciMediumIcons: TdxGalleryControlItem;
    gciSmallIcons: TdxGalleryControlItem;
    gciList: TdxGalleryControlItem;
    gciTiles: TdxGalleryControlItem;
    gciContent: TdxGalleryControlItem;
    cbSortBy: TcxComboBox;
    lbSortBy: TcxLabel;
    lbGroupBy: TcxLabel;
    cbGroupBy: TcxComboBox;
    cbHotTrack: TcxCheckBox;
    cbMultiSelect: TcxCheckBox;
    cbShowCheckBoxes: TcxCheckBox;
    cbShowExpandButtons: TcxCheckBox;
    procedure gcDisplayModesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
    procedure cbSortByPropertiesEditValueChanged(Sender: TObject);
    procedure cbGroupByPropertiesEditValueChanged(Sender: TObject);
    procedure cbHotTrackPropertiesEditValueChanged(Sender: TObject);
    procedure cbMultiSelectPropertiesEditValueChanged(Sender: TObject);
    procedure cbShowCheckBoxesPropertiesEditValueChanged(Sender: TObject);
    procedure cbShowExpandButtonPropertiesEditValueChanged(Sender: TObject);
  private
    function GetGroupItemByTag(AValue: Integer): TcxGridWinExplorerViewItem;
    function GetSortOrderByText(AValue: string): TdxSortOrder;
    function GetDisplayModeByTag(AValue: Integer): TcxGridWinExplorerViewDisplayMode;
    procedure SetGroupItem(AValue: TcxGridWinExplorerViewItem);
    procedure SetGroupItemSortOrder(ASortOrder: TdxSortOrder);
    procedure SetTextItemSortOrder(ASortOrder: TdxSortOrder);
    procedure SetDisplayMode(AValue: TcxGridWinExplorerViewDisplayMode);
  public
    procedure ShowCheckBoxes(AValue: Boolean);
    procedure ShowExpandButtons(AValue: Boolean);
    procedure UpdateGroup;
    procedure UpdateSortOrder;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.cbGroupByPropertiesEditValueChanged(Sender: TObject);
begin
  UpdateGroup;
end;

procedure TfrmMain.cbHotTrackPropertiesEditValueChanged(Sender: TObject);
begin
  WinExplorerView.OptionsBehavior.HotTrack := cbHotTrack.Checked;
end;

procedure TfrmMain.cbMultiSelectPropertiesEditValueChanged(Sender: TObject);
begin
  WinExplorerView.OptionsSelection.MultiSelect := cbMultiSelect.Checked;
end;

procedure TfrmMain.cbShowCheckBoxesPropertiesEditValueChanged(Sender: TObject);
begin
  ShowCheckBoxes(cbShowCheckBoxes.Checked);
end;

procedure TfrmMain.cbShowExpandButtonPropertiesEditValueChanged(Sender: TObject);
begin
  ShowExpandButtons(cbShowExpandButtons.Checked);
end;

procedure TfrmMain.cbSortByPropertiesEditValueChanged(Sender: TObject);
begin
  UpdateSortOrder;
end;

procedure TfrmMain.gcDisplayModesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
var
  ADisplayMode: TcxGridWinExplorerViewDisplayMode;
begin
  ADisplayMode := GetDisplayModeByTag(AItem.Tag);
  SetDisplayMode(ADisplayMode);
end;

procedure TfrmMain.ShowCheckBoxes(AValue: Boolean);
begin
  WinExplorerView.OptionsView.ShowItemCheckBoxes := AValue;
end;

procedure TfrmMain.ShowExpandButtons(AValue: Boolean);
begin
  WinExplorerView.OptionsView.ShowExpandButtons := AValue;
end;

procedure TfrmMain.UpdateGroup;
var
  AGroupItemTag: Integer;
  AGroupItem: TcxGridWinExplorerViewItem;
begin
  WinExplorerView.BeginGroupingUpdate;
  try
    SetGroupItemSortOrder(soNone);
    AGroupItemTag := cbGroupBy.Properties.Items.IndexOf(cbGroupBy.Text);
    AGroupItem := GetGroupItemByTag(AGroupItemTag);
    SetGroupItem(AGroupItem);
    UpdateSortOrder;
    WinExplorerView.Controller.FocusNextRecord(cxRecordIndexNone, True, False, False, False);
  finally
    WinExplorerView.EndGroupingUpdate;
  end;
end;

procedure TfrmMain.UpdateSortOrder;
var
  ASortOrder: TdxSortOrder;
begin
  WinExplorerView.BeginSortingUpdate;
  try
    ASortOrder := GetSortOrderByText(cbSortBy.Text);
    SetGroupItemSortOrder(ASortOrder);
    SetTextItemSortOrder(ASortOrder);
  finally
    WinExplorerView.EndSortingUpdate;
  end;
end;

function TfrmMain.GetGroupItemByTag(AValue: Integer): TcxGridWinExplorerViewItem;
var
  AItem: TcxCustomGridTableItem;
begin
  AItem := WinExplorerView.FindItemByTag(AValue);
  Result := TcxGridWinExplorerViewItem(AItem);
end;

function TfrmMain.GetSortOrderByText(AValue: string): TdxSortOrder;
begin
  if AValue = 'Ascending' then
    Result := soAscending
  else
    if AValue = 'Descending' then
      Result := soDescending
    else
      Result := soNone;
end;

function TfrmMain.GetDisplayModeByTag(AValue: Integer): TcxGridWinExplorerViewDisplayMode;
begin
  Result := TcxGridWinExplorerViewDisplayMode(AValue);
end;

procedure TfrmMain.SetGroupItem(AValue: TcxGridWinExplorerViewItem);
begin
  WinExplorerView.ItemSet.GroupItem := AValue;
end;

procedure TfrmMain.SetGroupItemSortOrder(ASortOrder: TdxSortOrder);
begin
  if WinExplorerView.ItemSet.GroupItem <> nil then
    WinExplorerView.ItemSet.GroupItem.SortOrder := ASortOrder;
end;

procedure TfrmMain.SetTextItemSortOrder(ASortOrder: TdxSortOrder);
begin
  if WinExplorerView.ItemSet.TextItem <> nil then
    WinExplorerView.ItemSet.TextItem.SortOrder := ASortOrder;
end;

procedure TfrmMain.SetDisplayMode(AValue: TcxGridWinExplorerViewDisplayMode);
begin
  WinExplorerView.ActiveDisplayMode := AValue;
end;

end.
