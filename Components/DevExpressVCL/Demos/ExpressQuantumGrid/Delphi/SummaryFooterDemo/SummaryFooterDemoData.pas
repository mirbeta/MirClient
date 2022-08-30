unit SummaryFooterDemoData;

interface

uses
  Forms,
  SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxClasses,
  cxGridTableView, DemoUtils, DBClient, MidasLib;

type
  TSummaryFooterDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    tblOrders: TClientDataSet;
    tblCustomers: TClientDataSet;
    tblCustomersID: TIntegerField;
    tblCustomersFirstName: TStringField;
    tblCustomersLastName: TStringField;
    tblCustomersCompany: TStringField;
    tblCustomersPrefix: TStringField;
    tblCustomersTitle: TStringField;
    tblCustomersAddress: TStringField;
    tblCustomersCity: TStringField;
    tblCustomersState: TStringField;
    tblCustomersZipCode: TStringField;
    tblCustomersSource: TStringField;
    tblCustomersCustomer: TStringField;
    tblCustomersHomePhone: TStringField;
    tblCustomersFaxPhone: TStringField;
    tblCustomersSpouse: TStringField;
    tblCustomersOccupation: TStringField;
    tblCustomersDescription: TMemoField;
    tblCustomersName: TStringField;
    PaymentTypeImages: TImageList;
    tblOrdersID: TAutoIncField;
    tblOrdersCustomerID: TIntegerField;
    tblOrdersProductID: TIntegerField;
    tblOrdersPurchaseDate: TDateTimeField;
    tblOrdersTime: TDateTimeField;
    tblOrdersPaymentType: TStringField;
    tblOrdersDescription: TMemoField;
    tblOrdersQuantity: TIntegerField;
    tblOrdersPaymentAmount: TCurrencyField;
    procedure tblCustomersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  SummaryFooterDemoDataDM: TSummaryFooterDemoDataDM;

implementation

{$R *.dfm}

procedure TSummaryFooterDemoDataDM.tblCustomersCalcFields(DataSet: TDataSet);
begin
  SetStringFieldValue(tblCustomersName, tblCustomersFirstName.Value + ' ' + tblCustomersLastName.Value);
end;

procedure TSummaryFooterDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblOrders.LoadFromFile(APath + 'Orders.xml');
end;

end.
