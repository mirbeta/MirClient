unit RTTIInspectorDemoData;

interface

uses
  SysUtils, Classes, ImgList, Controls, cxClasses, cxStyles, cxVGrid, DB, Forms,
  dxmdaset, CarsData;

type
  TRTTIInspectorDemoMainDM = class(TDataModule)
    dsCustomers: TDataSource;
    dsOrders: TDataSource;
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    PaymentTypeImages: TImageList;
    cxVerticalGridStyleSheetUserFormat3: TcxVerticalGridStyleSheet;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    mdOrders: TdxMemData;
    mdCustomers: TdxMemData;
    mdOrdersID: TAutoIncField;
    mdOrdersCustomerID: TIntegerField;
    mdOrdersProductID: TIntegerField;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersTime: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TFloatField;
    mdOrdersDescription: TMemoField;
    mdOrdersQuantity: TIntegerField;
    mdCustomersID: TAutoIncField;
    mdCustomersFirstName: TStringField;
    mdCustomersLastName: TStringField;
    mdCustomersCompany: TStringField;
    mdCustomersPrefix: TStringField;
    mdCustomersTitle: TStringField;
    mdCustomersAddress: TStringField;
    mdCustomersCity: TStringField;
    mdCustomersState: TStringField;
    mdCustomersZipCode: TStringField;
    mdCustomersSource: TStringField;
    mdCustomersCustomer: TStringField;
    mdCustomersHomePhone: TStringField;
    mdCustomersFaxPhone: TStringField;
    mdCustomersSpouse: TStringField;
    mdCustomersOccupation: TStringField;
    mdCustomersDescription: TMemoField;
    mdCustomersEmail: TStringField;
    mdOrdersCustomerEmail: TStringField;
    mdOrdersCarsTrademark: TStringField;
    mdOrdersCarsModel: TStringField;
    mdOrdersCar: TStringField;
    procedure mdOrdersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RTTIInspectorDemoMainDM: TRTTIInspectorDemoMainDM;

implementation

{$R *.dfm}

procedure TRTTIInspectorDemoMainDM.mdOrdersCalcFields(DataSet: TDataSet);
begin
  mdOrdersCar.AsString := mdOrdersCarsTrademark.AsString + ' ' + mdOrdersCarsModel.AsString;
end;

procedure TRTTIInspectorDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  mdCustomers.LoadFromBinaryFile('..\..\Data\Customers.dat');
  mdOrders.LoadFromBinaryFile('..\..\Data\Orders.dat');
  mdCustomers.Open;
  mdOrders.Open;
end;

end.
