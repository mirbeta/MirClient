unit HybridAppFrameSaleView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, dxLayoutControl, DB,
  dxCustomTileControl, HybridAppDM, cxDBEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCalendar, cxLabel, cxDBLabel,
  cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, cxDBData, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid, cxCurrencyEdit, dxMapControlTypes,
  dxMapControlOpenStreetMapImageryDataProvider, dxMapItem, dxCustomMapItemLayer, dxMapItemLayer, dxMapLayer,
  dxMapImageTileLayer, dxMapControl, cxGroupBox,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxDateRanges;

type
  TfrmSaleView = class(TfrmBase)
    lgOrder: TdxLayoutGroup;
    lbInvoice: TcxLabel;
    liSale: TdxLayoutItem;
    liOrderDate: TdxLayoutItem;
    edOrderDate: TcxDBDateEdit;
    liStore: TdxLayoutItem;
    edStore: TcxTextEdit;
    liCompany: TdxLayoutItem;
    edCompany: TcxTextEdit;
    liPurchaseOrder: TdxLayoutItem;
    edPONumber: TcxDBTextEdit;
    liAddress: TdxLayoutItem;
    edAddress: TcxTextEdit;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    dxLayoutItem8: TdxLayoutItem;
    cxGrid1DBTableView1ProductUnits: TcxGridDBColumn;
    cxGrid1DBTableView1ProductPrice: TcxGridDBColumn;
    cxGrid1DBTableView1Discount: TcxGridDBColumn;
    cxGrid1DBTableView1Name: TcxGridDBColumn;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    liShipping: TdxLayoutItem;
    edShipping: TcxDBCurrencyEdit;
    liGrandTotal: TdxLayoutItem;
    edGrandTotal: TcxDBCurrencyEdit;
    dxLayoutItem11: TdxLayoutItem;
    dxMapControl1: TdxMapControl;
    dxMapControl1ImageTileLayer1: TdxMapImageTileLayer;
    mcItemLayer: TdxMapItemLayer;
    mcItemLayerDot1: TdxMapDot;
    dxLayoutItem12: TdxLayoutItem;
    btnClose: TcxButton;
    procedure btnCloseClick(Sender: TObject);
  protected
    function GetParentFrameTileItem: TdxTileControlItem; override;
    procedure DoAfterActivate; override;
    procedure DoOnBackButtonClick; override;
    procedure Translate; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

function TfrmSaleView.GetParentFrameTileItem: TdxTileControlItem;
begin
  Result := MainForm.tbiSales;
end;

procedure TfrmSaleView.btnCloseClick(Sender: TObject);
begin
  ReturnToParentFrame;
end;

procedure TfrmSaleView.DoAfterActivate;
var
  ALongitude, ALatitude: Double;
  ADot: TdxMapItem;
  AHint: string;
begin
  inherited DoAfterActivate;

  lbInvoice.Caption := '#' + DM.clOrders.FieldByName('InvoiceNumber').AsString;
  if DM.clCustomersHelper.FindKey([DM.clOrders.FieldByName('CustomerId').AsInteger]) then
    edCompany.Text := DM.clCustomersHelper.FieldByName('Name').AsString;
  ALongitude := 0;
  ALatitude := 0;
  AHint := '';
  if DM.clCustomerEmployeesHelper.Locate('Id', DM.clOrders.FieldByName('EmployeeId').AsInteger, []) then
    AHint := Format('%s, %s, %s', [DM.clCustomerEmployeesHelper.FieldByName('FullName').AsString,
      DM.clCustomerEmployeesHelper.FieldByName('MobilePhone').AsString, DM.clCustomerEmployeesHelper.FieldByName('Email').AsString]);
  if DM.clCustomerStoresHelper.FindKey([DM.clOrders.FieldByName('StoreId').AsInteger]) then
  begin
    edStore.Text := DM.clCustomerStoresHelper.FieldByName('Location').AsString;
    edAddress.Text := DM.clCustomerStoresHelper.FieldByName('Address_Full').AsString;
    ALongitude := DM.clCustomerStoresHelper.FieldByName('Address_Longitude').AsFloat;
    ALatitude := DM.clCustomerStoresHelper.FieldByName('Address_Latitude').AsFloat;
    AHint := Format('%s; %s', [DM.clCustomerStoresHelper.FieldByName('Address_Full').AsString, AHint]);
  end;

  dxMapControl1.CenterPoint.Longitude := ALongitude;
  dxMapControl1.CenterPoint.Latitude := ALatitude;

  mcItemLayer.MapItems.Clear;
  ADot := mcItemLayer.MapItems.Add(TdxMapDot);
  ADot.Hint := AHint;
  TdxMapDot(ADot).Location.Longitude := ALongitude;
  TdxMapDot(ADot).Location.Latitude := ALatitude;
  TdxMapDot(ADot).Size := 20;
end;

procedure TfrmSaleView.DoOnBackButtonClick;
begin
  btnClose.Click;
end;

procedure TfrmSaleView.Translate;
begin
  inherited Translate;

  liSale.Caption := cxGetResourceString(@sSaleLabel);
  liOrderDate.Caption := cxGetResourceString(@sOrderDateLabel);
  liStore.Caption := cxGetResourceString(@sStoreLabel);
  liCompany.Caption := cxGetResourceString(@sCompanyLabel);
  liAddress.Caption := cxGetResourceString(@sAddressLabel);
  liPurchaseOrder.Caption := cxGetResourceString(@sPurchaseDateLabel);
  liShipping.Caption := cxGetResourceString(@sShippingLabel);
  liGrandTotal.Caption := cxGetResourceString(@sGrandTotalLabel);

  cxGrid1DBTableView1ProductPrice.Caption := cxGetResourceString(@sUnitPriceColumn);
  cxGrid1DBTableView1ProductUnits.Caption := cxGetResourceString(@sQuantityColumn);
  cxGrid1DBTableView1Discount.Caption := cxGetResourceString(@sDiscountColumn);
  cxGrid1DBTableView1Name.Caption := cxGetResourceString(@sName);

  btnClose.Caption := cxGetResourceString(@sCloseButton);
end;

initialization
  RegisterFrame(IDSaleView, TfrmSaleView);

end.
