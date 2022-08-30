unit FilterControlDemoData;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, Forms, DB, cxStyles, ImgList, Controls, cxClasses,
  cxGridTableView, DBClient, CarsDataForGrid, MidasLib;

type
  TFilterControlDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    PaymentTypeImages: TImageList;
    dsOrdersStd: TDataSource;
    tblOrders: TClientDataSet;
    tblCustomers: TClientDataSet;
    tblOrdersStd: TClientDataSet;
    tblOrdersID: TAutoIncField;
    tblOrdersCustomerID: TIntegerField;
    tblOrdersProductID: TIntegerField;
    tblOrdersPurchaseDate: TDateTimeField;
    tblOrdersTime: TDateTimeField;
    tblOrdersPaymentType: TStringField;
    tblOrdersPaymentAmount: TCurrencyField;
    tblOrdersDescription: TMemoField;
    tblOrdersQuantity: TIntegerField;
    tblOrdersStdCustomerID: TIntegerField;
    tblOrdersStdProductID: TIntegerField;
    tblOrdersStdPurchaseDate: TDateTimeField;
    tblOrdersStdTime: TDateTimeField;
    tblOrdersStdPaymentType: TStringField;
    tblOrdersStdPaymentAmount: TCurrencyField;
    tblOrdersStdDescription: TMemoField;
    tblOrdersStdQuantity: TIntegerField;
    tblCustomersID: TAutoIncField;
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
    tblOrdersStdCompany: TStringField;
    tblOrdersStdCar: TStringField;
    procedure tblCustomersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
    procedure tblOrdersStdCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilterControlDemoDataDM: TFilterControlDemoDataDM;

implementation

{$R *.dfm}

procedure TFilterControlDemoDataDM.tblCustomersCalcFields(DataSet: TDataSet);
begin
  tblCustomersName.AsString := tblCustomersFirstName.AsString + ' ' + tblCustomersLastName.AsString;
end;

procedure TFilterControlDemoDataDM.tblOrdersStdCalcFields(DataSet: TDataSet);
begin
  dmGridCars.mdModels.Locate(dmGridCars.mdModelsID.FieldName, tblOrdersStdProductID.Value, []);
  tblOrdersStdCar.Value := AnsiString(dmGridCars.mdModelsFullName.Value);
end;

procedure TFilterControlDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblOrders.LoadFromFile(APath + 'Orders.xml');
  tblOrdersStd.LoadFromFile(APath + 'Orders.xml');
end;

end.
