unit MasterDetailChartDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData,
  cxDataStorage, cxEdit, DB, cxDBData, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, StdCtrls, cxLookAndFeels, Menus,
  cxGridChartView, cxGridDBChartView, cxLookAndFeelPainters, BaseForm,
  cxGridCardView, ComCtrls, DBClient, MidasLib;

type
  TfrmMain = class(TfmBaseForm)
    dsOrders: TDataSource;
    tvCustomers: TcxGridDBTableView;
    grMainLevel1: TcxGridLevel;
    grMain: TcxGrid;
    grMainLevel2: TcxGridLevel;
    chvOrders: TcxGridDBChartView;
    dsCustomers: TDataSource;
    tvCustomersID: TcxGridDBColumn;
    tvCustomersFirstName: TcxGridDBColumn;
    tvCustomersLastName: TcxGridDBColumn;
    tvCustomersCompany: TcxGridDBColumn;
    tvCustomersAddress: TcxGridDBColumn;
    tvCustomersCity: TcxGridDBColumn;
    tvCustomersState: TcxGridDBColumn;
    tvCustomersZipCode: TcxGridDBColumn;
    tvCustomersEmail: TcxGridDBColumn;
    chvOrdersPaymentAmountSeries: TcxGridDBChartSeries;
    chvOrdersProductIDSeries: TcxGridDBChartSeries;
    chvOrdersQuantitySeries: TcxGridDBChartSeries;
    grMainLevel3: TcxGridLevel;
    chvProducts: TcxGridDBChartView;
    dsProducts: TDataSource;
    chvProductsCopiesSeries: TcxGridDBChartSeries;
    cdsCustomers: TClientDataSet;
    cdsOrders: TClientDataSet;
    cdsCustomersID: TAutoIncField;
    cdsCustomersFirstName: TStringField;
    cdsCustomersLastName: TStringField;
    cdsCustomersCompany: TStringField;
    cdsCustomersPrefix: TStringField;
    cdsCustomersTitle: TStringField;
    cdsCustomersAddress: TStringField;
    cdsCustomersCity: TStringField;
    cdsCustomersState: TStringField;
    cdsCustomersZipCode: TStringField;
    cdsCustomersSource: TStringField;
    cdsCustomersCustomer: TStringField;
    cdsCustomersHomePhone: TStringField;
    cdsCustomersFaxPhone: TStringField;
    cdsCustomersSpouse: TStringField;
    cdsCustomersOccupation: TStringField;
    cdsCustomersDescription: TMemoField;
    cdsOrdersID: TAutoIncField;
    cdsOrdersCustomerID: TIntegerField;
    cdsOrdersProductID: TIntegerField;
    cdsOrdersPurchaseDate: TDateTimeField;
    cdsOrdersTime: TDateTimeField;
    cdsOrdersPaymentType: TStringField;
    cdsOrdersDescription: TMemoField;
    cdsOrdersQuantity: TIntegerField;
    cdsOrdersPaymentAmount: TCurrencyField;
    cdsProductSumOfQuantity: TClientDataSet;
    cdsProductSumOfQuantityCustomerID: TIntegerField;
    cdsProductSumOfQuantityName: TStringField;
    cdsProductSumOfQuantitySUMOFQuantity: TFloatField;
    cdsProducts: TClientDataSet;
    cdsProductsID: TAutoIncField;
    cdsProductsName: TStringField;
    cdsProductsDescription: TMemoField;
    cdsProductsPlatform: TStringField;
    cdsProductsLogo: TBlobField;
    cdsProductsLink: TMemoField;
    procedure FormCreate(Sender: TObject);
    procedure chvOrdersGetValueHint(Sender: TcxGridChartView;
      ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: String);
    procedure chvOrdersCategoriesGetValueDisplayText(Sender: TObject;
      const AValue: Variant; var ADisplayText: string);
  protected
    function GetProductName(AID: Integer): string;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  AboutDemoForm;

function TfrmMain.GetProductName(AID: Integer): string;
begin
  if cdsProducts.FindKey([AID]) then
    Result := cdsProducts.FieldValues['Name']
  else
    Result := '';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  cdsOrders.Open;
  cdsProducts.Open;
  cdsProductSumOfQuantity.Open;
  tvCustomers.ViewData.Rows[0].Expanded := True;
  tvCustomers.ViewData.Rows[1].Expanded := True;
  (tvCustomers.ViewData.Rows[1] as TcxGridMasterDataRow).ActiveDetailIndex := 1;
end;

procedure TfrmMain.chvOrdersCategoriesGetValueDisplayText(Sender: TObject;
  const AValue: Variant; var ADisplayText: string);
begin
  ADisplayText := '#' + ADisplayText;
end;

procedure TfrmMain.chvOrdersGetValueHint(Sender: TcxGridChartView;
  ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: String);
begin
  AHint := Format('Order %s (%d copies of %s) for %s',
    [Sender.Categories.VisibleDisplayTexts[AValueIndex],
     Integer(Sender.FindSeriesByID(chvOrdersQuantitySeries.ID).VisibleValues[AValueIndex]),
     GetProductName(Integer(Sender.FindSeriesByID(chvOrdersProductIDSeries.ID).VisibleValues[AValueIndex])),
     ASeries.VisibleDisplayTexts[AValueIndex]]);
end;

end.
