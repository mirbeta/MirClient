unit HybridAppFrameProducts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit, cxButtonEdit,
  dxLayoutControl, dxLayoutControlAdapters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, DB,
  cxDBData, cxCurrencyEdit, dxDBSparkline, Menus, StdCtrls, cxButtons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridCustomView, cxGrid, dxCustomTileControl, dxTileControl, ImgList,
  HybridAppDM, cxGroupBox, cxImageList, dxDateRanges;

type
  TfrmProducts = class(TfrmBase)
    ilProducts: TcxImageList;
    dxLayoutItem2: TdxLayoutItem;
    tcProducts: TdxTileControl;
    dxTileControl1Group1: TdxTileControlGroup;
    tiAll: TdxTileControlItem;
    tiVideoPlayers: TdxTileControlItem;
    tiAutomation: TdxTileControlItem;
    tiMonitors: TdxTileControlItem;
    tiProjectors: TdxTileControlItem;
    tiTelevisions: TdxTileControlItem;
    dxLayoutItem3: TdxLayoutItem;
    cxGridProduct: TcxGrid;
    gvProduct: TcxGridDBTableView;
    gvProduct_Name: TcxGridDBColumn;
    gvProduct_Cost: TcxGridDBColumn;
    gvProduct_Sale_Price: TcxGridDBColumn;
    gvProduct_RetailPrice: TcxGridDBColumn;
    gvProduct_Inventory: TcxGridDBColumn;
    gvProduct_Backorder: TcxGridDBColumn;
    gvProductMontlySales2015: TcxGridDBColumn;
    cxGridProductLevel1: TcxGridLevel;
    dxLayoutItem4: TdxLayoutItem;
    cxbEdit1: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    cxbNew1: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnNew: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    btnEdit: TcxButton;
    procedure tcProductsItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem,
      ANewFocusedItem: TdxTileControlItem);
    procedure gvProductTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem;
      const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure gvProductTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems1GetText(Sender: TcxDataSummaryItem;
      const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure btnEditClick(Sender: TObject);
    procedure gvProduct_NameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure gvProductCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure TranslateFilters;
  protected
    procedure DoAfterActivate; override;
    procedure Translate; override;
    procedure RefreshInfo; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

procedure TfrmProducts.btnEditClick(Sender: TObject);
begin
  ShowEditPage(MainForm.tbiProductEdit);
end;

procedure TfrmProducts.DoAfterActivate;
begin
  inherited DoAfterActivate;
  if tcProducts.Controller.FocusedItem = nil then
    tcProducts.Controller.FocusedItem := tiAll;
end;

procedure TfrmProducts.Translate;
begin
  inherited Translate;
  TranslateFilters;

  gvProduct_Name.Caption := cxGetResourceString(@sName);
  gvProduct_Cost.Caption := cxGetResourceString(@sCostColumn);
  gvProduct_Sale_Price.Caption := cxGetResourceString(@sSalePriceColumn);
  gvProduct_RetailPrice.Caption := cxGetResourceString(@sRetailPriceColumn);
  gvProduct_Inventory.Caption := cxGetResourceString(@sInventoryColumn);
  gvProduct_Backorder.Caption := cxGetResourceString(@sBackorderColumn);
  gvProductMontlySales2015.Caption := cxGetResourceString(@sMonthlySalesColumn);

  btnNew.Caption := cxGetResourceString(@sNewButton);
  btnEdit.Caption := cxGetResourceString(@sEditButton);
end;

procedure TfrmProducts.gvProductCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  btnEdit.Click;
end;

procedure TfrmProducts.TranslateFilters;
var
  I: Integer;
  AText: string;
begin
  tcProducts.Title.Text := cxGetResourceString(@sProductCategoriesFilterCaption);
  for I := 0 to tcProducts.Items.Count - 1 do
  begin
    AText := DM.GetProductCategoryName(TdxProductCategory(tcProducts.Items[I].Tag));
    if AText = '' then
      case tcProducts.Items[I].Tag of
        cAllCountTag:
          AText := cxGetResourceString(@sFilterAllCaption);
        cHighPriorityTaskCountTag:
          AText := DM.GetTaskPriorityTypeName(tpHigh);
        cUrgentPriorityTaskCountTag:
          AText := DM.GetTaskPriorityTypeName(tpUrgent);
      else
        AText := tcProducts.Items[I].Text3.Value;
      end;
    tcProducts.Items[I].Text3.Value := AText;
  end;
end;

procedure TfrmProducts.gvProductTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(
  Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  AText := cxGetResourceString(@sTotalInventoryLabel) + #13 + AText;
end;

procedure TfrmProducts.gvProductTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems1GetText(
  Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  AText := cxGetResourceString(@sTotalSalesLabel) + #13 + AText;
end;

procedure TfrmProducts.gvProduct_NameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AssignGridFilterBoxFont(Sender, AValueList);
end;

procedure TfrmProducts.tcProductsItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem,
  ANewFocusedItem: TdxTileControlItem);
begin
  if ANewFocusedItem <> nil then
    DM.ApplyProductFilter(ANewFocusedItem.Tag);
end;

procedure TfrmProducts.RefreshInfo;
begin
  inherited RefreshInfo;

  DM.RecalculateProductCategoriesCount;

  tiAll.Tag := cAllCountTag;
  tiAll.Text2.Value := IntToStr(DM.ProductCategoryCount[tiAll.Tag]);

  tiTelevisions.Tag := Integer(pcTelevisions);
  tiTelevisions.Text2.Value := IntToStr(DM.ProductCategoryCount[tiTelevisions.Tag]);

  tiMonitors.Tag := Integer(pcMonitors);
  tiMonitors.Text2.Value := IntToStr(DM.ProductCategoryCount[tiMonitors.Tag]);

  tiVideoPlayers.Tag := Integer(pcVideoPlayers);
  tiVideoPlayers.Text2.Value := IntToStr(DM.ProductCategoryCount[tiVideoPlayers.Tag]);

  tiProjectors.Tag := Integer(pcProjectors);
  tiProjectors.Text2.Value := IntToStr(DM.ProductCategoryCount[tiProjectors.Tag]);

  tiAutomation.Tag := Integer(pcAutomation);
  tiAutomation.Text2.Value := IntToStr(DM.ProductCategoryCount[tiAutomation.Tag]);
end;

initialization
  RegisterFrame(IDProducts, TfrmProducts);

end.

