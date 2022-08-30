unit DemoBasicDM;

{$I cxVer.inc}

interface

uses
  Forms, SysUtils, Classes, DB, ImgList, Controls, cxStyles, DBClient, cxClasses, MidasLib;

type
  TdmOrders = class(TDataModule)
    dsCustomers: TDataSource;
    dsOrders: TDataSource;
    PaymentTypeImages: TImageList;
    cxStyleRepository1: TcxStyleRepository;
    stBoldBlueFont: TcxStyle;
    stBoldRedFont: TcxStyle;
    stBoldBlackFont: TcxStyle;
    cdsOrders: TClientDataSet;
    cdsCustomers: TClientDataSet;
    cdsOrdersID: TAutoIncField;
    cdsOrdersCustomerID: TIntegerField;
    cdsOrdersProductID: TIntegerField;
    cdsOrdersPurchaseDate: TDateTimeField;
    cdsOrdersPaymentType: TStringField;
    cdsOrdersQuantity: TIntegerField;
    cdsOrdersCarName: TStringField;
    cdsOrdersUnitPrice: TCurrencyField;
    cdsOrdersCompanyName: TStringField;
    cdsOrdersPaymentAmount: TCurrencyField;
    cdsCustomersID: TIntegerField;
    cdsCustomersCompany: TStringField;
    dsCars: TDataSource;
    cdsCars: TClientDataSet;
    cdsCarsID: TAutoIncField;
    cdsCarsTrademark: TWideStringField;
    cdsCarsModel: TWideStringField;
    cdsCarsPrice: TBCDField;
    cdsCarsCarName: TWideStringField;
    procedure cdsCarsCalcFields(DataSet: TDataSet);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmOrders: TdmOrders;

implementation

{$R *.dfm}

procedure TdmOrders.cdsCarsCalcFields(DataSet: TDataSet);
begin
  cdsCarsCarName.Value := cdsCarsTrademark.Value + ': ' + cdsCarsModel.Value;
end;

procedure TdmOrders.cdsOrdersCalcFields(DataSet: TDataSet);
begin
  cdsOrdersPaymentAmount.Value := cdsOrdersQuantity.Value * cdsOrdersUnitPrice.Value;
end;

end.
