unit CustomDrawTableViewDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridTableView, Forms, DBClient, MidasLib;

type
  TCustomDrawTableViewDemoMainDM = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    dsCustomers: TDataSource;
    dsOrders: TDataSource;
    GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
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
    cdsCustomersEmail: TStringField;
    cdsOrdersID: TAutoIncField;
    cdsOrdersCustomerID: TIntegerField;
    cdsOrdersProductID: TIntegerField;
    cdsOrdersPurchaseDate: TDateTimeField;
    cdsOrdersTime: TDateTimeField;
    cdsOrdersPaymentType: TStringField;
    cdsOrdersDescription: TMemoField;
    cdsOrdersQuantity: TIntegerField;
    cdsOrdersPaymentAmount: TCurrencyField;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  CustomDrawTableViewDemoMainDM: TCustomDrawTableViewDemoMainDM;

implementation

{$R *.dfm}

procedure TCustomDrawTableViewDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  cdsCustomers.LoadFromFile('..\..\Data\CustomDrawTableViewDemo_Customers.xml');
  cdsCustomers.Open;
  cdsOrders.LoadFromFile('..\..\Data\CustomDrawTableViewDemo_Orders.xml');
  cdsOrders.Open;
end;

end.
