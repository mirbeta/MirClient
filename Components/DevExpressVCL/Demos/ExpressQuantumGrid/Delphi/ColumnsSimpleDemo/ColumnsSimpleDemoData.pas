unit ColumnsSimpleDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxGridTableView,
  cxGridBandedTableView, cxClasses, Forms, DBClient, MidasLib;

type
  TColumnsSimpleDemoDataDM = class(TDataModule)
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
    dsCities: TDataSource;
    tblCities: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColumnsSimpleDemoDataDM: TColumnsSimpleDemoDataDM;

implementation

{$R *.dfm}

uses
  DemoUtils;

procedure TColumnsSimpleDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblOrders.LoadFromFile(APath + 'Orders.xml');
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblCities.LoadFromFile(APath + 'Cities.xml');
end;

end.
