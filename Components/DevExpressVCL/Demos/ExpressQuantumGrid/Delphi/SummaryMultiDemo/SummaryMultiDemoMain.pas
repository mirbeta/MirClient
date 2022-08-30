unit SummaryMultiDemoMain;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, ToolWin, cxStyles, cxGraphics, cxFilter, cxData, cxEdit,
  DB, cxDBData, cxClasses, cxDataStorage, cxDBLookupComboBox, cxCalendar,
  cxImageComboBox, cxCalc, cxBlobEdit, cxSpinEdit, cxLookAndFeels,
  cxLookAndFeelPainters, BaseForm, cxGridCardView, cxNavigator, CarsDataForGrid;

type
  TSummaryMultiDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    miSummaries: TMenuItem;
    Grid: TcxGrid;
    tvOrders: TcxGridDBTableView;
    tvOrdersCustomerID: TcxGridDBColumn;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    miSelectedRecordsOnly: TMenuItem;
    miIgnoreNullValues: TMenuItem;
    N1: TMenuItem;
    miGroupFooters: TMenuItem;
    miMultiSelect: TMenuItem;
    miGroupFooterAlwaysShow: TMenuItem;
    miGroupFooterNeverShow: TMenuItem;
    miGroupFooterShowWhenExpand: TMenuItem;
    cxGridPopupMenu1: TcxGridPopupMenu;
    lvOrders: TcxGridLevel;
    tvOrdersProductID: TcxGridDBColumn;
    miUseOnAfterSummaryEvent: TMenuItem;
    miMultipleSummariesInFooter: TMenuItem;
    miMultipleSummariesInGroupFooters: TMenuItem;
    N2: TMenuItem;
    procedure miSelectedRecordsOnlyClick(Sender: TObject);
    procedure miIgnoreNullValuesClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure miGroupFootersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvOrdersDataControllerSummaryAfterSummary(ASender: TcxDataSummary);
    procedure miUseOnAfterSummaryEventClick(Sender: TObject);
    procedure tvOrdersDataControllerSummaryDefaultGroupSummaryItemsSummary(
      ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments;
      var OutArguments: TcxSummaryEventOutArguments);
    procedure tvOrdersStylesGetFooterSummaryStyle(AView: TcxGridTableView;
      ARow: TcxCustomGridRow; AColumn: TcxGridColumn; AFooterGroupLevel: Integer;
      ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle);
    procedure tvOrdersStylesGetGroupSummaryStyle(Sender: TcxGridTableView;
      ARow: TcxGridGroupRow; AColumn: TcxGridColumn;
      ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle);
    procedure miMultipleSummariesInFooterClick(Sender: TObject);
    procedure miMultipleSummariesInGroupFootersClick(Sender: TObject);
  protected
    procedure UpdateMenu;
  end;

var
  SummaryMultiDemoMainForm: TSummaryMultiDemoMainForm;

implementation

{$R *.dfm}

uses
  SummaryMultiDemoData, AboutDemoForm;

procedure TSummaryMultiDemoMainForm.UpdateMenu;
begin
  MenuItemSetChecked('miSelectedRecordsOnly', soSelectedRecords in tvOrders.DataController.Summary.Options);
  MenuItemSetChecked('miIgnoreNullValues', soNullIgnore in tvOrders.DataController.Summary.Options);
  MenuItemSetChecked('miUseOnAfterSummaryEvent', Assigned(tvOrders.DataController.Summary.OnAfterSummary));
  MenuItemSetChecked('miMultipleSummariesInFooter', tvOrders.OptionsView.FooterMultiSummaries);
  MenuItemSetChecked('miMultipleSummariesInGroupFooters', tvOrders.OptionsView.GroupFooterMultiSummaries);
  MenuItemSetChecked('miMultiSelect', tvOrders.OptionsSelection.MultiSelect);
  MenuItemCheckSubItemWithTag('miGroupFooters', Ord(tvOrders.OptionsView.GroupFooters));
end;

procedure TSummaryMultiDemoMainForm.miSelectedRecordsOnlyClick(Sender: TObject);
begin
  with tvOrders.DataController.Summary do
    if soSelectedRecords in Options then
      Options := Options - [soSelectedRecords]
    else
      Options := Options + [soSelectedRecords];
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miIgnoreNullValuesClick(Sender: TObject);
begin
  with tvOrders.DataController.Summary do
    if soNullIgnore in Options then
      Options := Options - [soNullIgnore]
    else
      Options := Options + [soNullIgnore];
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miMultipleSummariesInFooterClick(Sender: TObject);
begin
  with tvOrders.OptionsView do
    FooterMultiSummaries := not FooterMultiSummaries;
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miMultipleSummariesInGroupFootersClick(Sender: TObject);
begin
  with tvOrders.OptionsView do
    GroupFooterMultiSummaries := not GroupFooterMultiSummaries;
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miMultiSelectClick(Sender: TObject);
begin
  with tvOrders.OptionsSelection do
    MultiSelect := not MultiSelect;
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miGroupFootersClick(Sender: TObject);
begin
  tvOrders.OptionsView.GroupFooters := TcxGridGroupFootersMode(TComponent(Sender).Tag);
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.tvOrdersStylesGetFooterSummaryStyle(
  AView: TcxGridTableView; ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
  AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle);
begin
  case ASummaryItem.Kind of
    skMin:
      AStyle := SummaryMultiDemoDataDM.styleRed;
    skMax:
      AStyle := SummaryMultiDemoDataDM.styleBlue;
  end;
end;

procedure TSummaryMultiDemoMainForm.tvOrdersStylesGetGroupSummaryStyle(
  Sender: TcxGridTableView; ARow: TcxGridGroupRow; AColumn: TcxGridColumn;
  ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle);
begin
  if ASummaryItem <> nil then
    tvOrdersStylesGetFooterSummaryStyle(Sender, ARow, AColumn, ARow.Level, ASummaryItem, AStyle);
end;

procedure TSummaryMultiDemoMainForm.FormShow(Sender: TObject);
begin
  tvOrders.DataController.Groups.ChangeExpanding(0, True, False);
  tvOrders.DataController.GotoFirst;
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.miUseOnAfterSummaryEventClick(Sender: TObject);
var
  ASummary: TcxDataSummary;
begin
  ASummary := tvOrders.DataController.Summary;
  ASummary.BeginUpdate;
  try
    if Assigned(ASummary.OnAfterSummary) then
      ASummary.OnAfterSummary := nil
    else
      ASummary.OnAfterSummary := tvOrdersDataControllerSummaryAfterSummary;
  finally
    ASummary.EndUpdate;
  end;
  UpdateMenu;
end;

procedure TSummaryMultiDemoMainForm.tvOrdersDataControllerSummaryAfterSummary(
  ASender: TcxDataSummary);

  function CalculateFooterSummaryValue(AIndex: Integer; AKind: TcxSummaryKind): Variant;
  var
    AGroups: TcxDataControllerGroups;
    I: Integer;
    AValue: Variant;
  begin
    AGroups := tvOrders.DataController.Groups;
    if AKind = skCount then
      Result := AGroups.ChildCount[-1]
    else
    begin
      Result := Null;
      for I := 0 to AGroups.ChildCount[-1] - 1 do
      begin;
        AValue := ASender.GroupSummaryValues[AGroups.ChildDataGroupIndex[-1, I], AIndex];
        if not VarIsNull(AValue) then
          if VarIsNull(Result) then
            Result := AValue
          else
            case AKind of
              skMin:
                if AValue < Result then
                  Result := AValue;
              skMax:
                if AValue > Result then
                  Result := AValue;
              skSum, skAverage:
                Result := Result + AValue;
            end;
      end;
      if (AKind = skAverage) and not VarIsNull(Result) and (AGroups.ChildCount[-1] <> 0) then
        Result := Result / AGroups.ChildCount[-1];
    end;
  end;

var
  AFooterSummaryItems: TcxDataFooterSummaryItems;
  AGroupSummaryItems: TcxDataGroupSummaryItems;
  I: Integer;
  AGroupSummaryItem: TcxDataSummaryItem;
  AValue: Variant;
begin  // calculate footer summaries using the group footer summary values, not actual data values
  if tvOrders.GroupedColumnCount = 0 then Exit;
  AFooterSummaryItems := ASender.FooterSummaryItems;
  AGroupSummaryItems := ASender.GroupSummaryItems[0];
  for I := 0 to AFooterSummaryItems.Count - 1 do
  begin
    AGroupSummaryItem := AGroupSummaryItems.GetDataItem(
      (AFooterSummaryItems[I].ItemLink as TcxGridColumn).Index, spFooter);
    if AGroupSummaryItem <> nil then
    begin
      AValue := CalculateFooterSummaryValue(AGroupSummaryItem.Index, AFooterSummaryItems[I].Kind);
      if not VarIsNull(AValue) then
        ASender.FooterSummaryValues[I] := VarAsType(AValue, VarType(ASender.FooterSummaryValues[I]));
    end;
  end;
end;

procedure TSummaryMultiDemoMainForm.tvOrdersDataControllerSummaryDefaultGroupSummaryItemsSummary(
  ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments;
  var OutArguments: TcxSummaryEventOutArguments);
var
  AItem: TcxDataSummaryItem;
  AValue: Variant;
begin  // calculate the number of orders with the PaymentAmount > $300,000
  AItem := Arguments.SummaryItem;
  if (AItem.ItemLink = tvOrdersProductID) and
    (AItem.Kind = skCount) and (AItem.Position = spGroup) then
  begin
    AValue := tvOrders.DataController.Values[Arguments.RecordIndex, tvOrdersPaymentAmount.Index];
    if not VarIsNull(AValue) and (VarAsType(AValue, varInteger) <= 300000) then
      Dec(OutArguments.CountValue);
  end;
end;

end.
