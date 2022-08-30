unit DemoDM;

interface

uses
  SysUtils, Classes, DB, dxLayoutLookAndFeels, cxLookAndFeels, dxmdaset, cxClasses;

type
  TdmDemo = class(TDataModule)
    dsOrders: TDataSource;
    llcfMain: TdxLayoutLookAndFeelList;
    dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel;
    dxLayoutOfficeLookAndFeel1: TdxLayoutOfficeLookAndFeel;
    dxLayoutWebLookAndFeel1: TdxLayoutWebLookAndFeel;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    mdOrders: TdxMemData;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersOrders_Time: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TFloatField;
    mdOrdersQuantity: TIntegerField;
    mdOrdersFirstName: TStringField;
    mdOrdersLastName: TStringField;
    mdOrdersCompany: TStringField;
    mdOrdersPrefix: TStringField;
    mdOrdersTitle: TStringField;
    mdOrdersAddress: TStringField;
    mdOrdersCity: TStringField;
    mdOrdersState: TStringField;
    mdOrdersZipCode: TStringField;
    mdOrdersSource: TStringField;
    mdOrdersCustomer: TStringField;
    mdOrdersHomePhone: TStringField;
    mdOrdersFaxPhone: TStringField;
    mdOrdersSpouse: TStringField;
    mdOrdersOccupation: TStringField;
    mdOrdersEmail: TStringField;
    mdOrdersTrademark: TStringField;
    mdOrdersModel: TStringField;
    mdOrdersHP: TSmallintField;
    mdOrdersLiter: TFloatField;
    mdOrdersCyl: TSmallintField;
    mdOrdersTransmissSpeedCount: TSmallintField;
    mdOrdersTransmissAutomatic: TStringField;
    mdOrdersMPG_City: TSmallintField;
    mdOrdersMPG_Highway: TSmallintField;
    mdOrdersCategory: TStringField;
    mdOrdersCars_Description: TMemoField;
    mdOrdersHyperlink: TStringField;
    mdOrdersPicture: TBlobField;
    mdOrdersPrice: TFloatField;
    mdOrdersCustomers_ID: TIntegerField;
    mdOrdersOrders_ID: TIntegerField;
    mdOrdersCars_ID: TIntegerField;
    mdCars: TdxMemData;
    mdCarsTrademark: TStringField;
    mdCarsModel: TStringField;
    mdCarsHP: TSmallintField;
    mdCarsLiter: TFloatField;
    mdCarsCyl: TSmallintField;
    mdCarsTransmissSpeedCount: TSmallintField;
    mdCarsTransmissAutomatic: TStringField;
    mdCarsMPG_City: TSmallintField;
    mdCarsMPG_Highway: TSmallintField;
    mdCarsCategory: TStringField;
    mdCarsCars_Description: TMemoField;
    mdCarsHyperlink: TStringField;
    mdCarsPicture: TBlobField;
    mdCarsPrice: TFloatField;
    mdCarsID: TIntegerField;
    dsCars: TDataSource;
    mdOrdersCustomerPhoto: TBlobField;
    mdCustomers: TdxMemData;
    dsCustomers: TDataSource;
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
    mdCustomersPhoto: TBlobField;
    mdCustomersHomePhone: TStringField;
    mdCustomersFaxPhone: TStringField;
    mdCustomersSpouse: TStringField;
    mdCustomersOccupation: TStringField;
    mdCustomersEmail: TStringField;
    mdCustomersID: TIntegerField;
    mdCustomersDescription: TMemoField;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  dmDemo: TdmDemo;

implementation

uses
  Forms;

{$R *.dfm}

procedure TdmDemo.DataModuleCreate(Sender: TObject);
var
  I: Integer;
begin
  mdCars.LoadFromBinaryFile(ExtractFilePath(Application.ExeName) + '..\..\Data\Cars.dat');
  mdCustomers.LoadFromBinaryFile(ExtractFilePath(Application.ExeName) + '..\..\Data\Customers.dat');
  mdOrders.LoadFromBinaryFile(ExtractFilePath(Application.ExeName) + '..\..\Data\OrdersCarsCustomers.dat');

  mdCars.DisableControls;
  mdCustomers.DisableControls;
  mdOrders.DisableControls;
  try
    for I := 1 to mdOrders.RecordCount do
    begin
      mdOrders.RecNo := I;
      mdCars.Locate('ID', mdOrdersCars_ID.Value, []);
      mdCustomers.Locate('ID', mdOrdersCustomers_ID.Value, []);
      mdOrders.Edit;
      mdOrdersPicture.Value := mdCarsPicture.Value;
      mdOrdersCustomerPhoto.Value := mdCustomersPhoto.Value;
      mdOrders.Post;
    end;

    mdOrders.First;
  finally
    mdOrders.EnableControls;
    mdCustomers.EnableControls;
    mdCars.EnableControls;
  end;
end;

end.
