unit ViewNestedBandsDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxGridTableView,
  cxGridBandedTableView, cxClasses, Forms, DemoUtils, DBClient, MidasLib;

type
  TViewNestedBandsDemoDataDM = class(TDataModule)
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
    StyleRepository: TcxStyleRepository;
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
    tblCustomersName: TStringField;
    cxStyle31: TcxStyle;
    cxStyle35: TcxStyle;
    procedure tblCustomersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  ViewNestedBandsDemoDataDM: TViewNestedBandsDemoDataDM;

implementation

{$R *.dfm}

procedure TViewNestedBandsDemoDataDM.tblCustomersCalcFields(DataSet: TDataSet);
begin
  SetStringFieldValue(tblCustomersName, tblCustomersFirstName.Value + ' ' + tblCustomersLastName.Value);
end;

procedure TViewNestedBandsDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblOrders.LoadFromFile(APath + 'Orders.xml');
end;

end.
