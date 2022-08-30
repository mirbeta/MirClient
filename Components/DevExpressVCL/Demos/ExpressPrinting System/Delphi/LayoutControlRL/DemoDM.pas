unit DemoDM;

interface

uses
  SysUtils, Classes, DB, dxLayoutLookAndFeels, cxLookAndFeels, dxmdaset;

type
  TdmDemo = class(TDataModule)
    dsOrders: TDataSource;
    mdLayoutControl: TdxMemData;
    mdLayoutControlPurchaseDate: TDateTimeField;
    mdLayoutControlOrders_Time: TDateTimeField;
    mdLayoutControlPaymentType: TStringField;
    mdLayoutControlPaymentAmount: TFloatField;
    mdLayoutControlQuantity: TIntegerField;
    mdLayoutControlFirstName: TStringField;
    mdLayoutControlLastName: TStringField;
    mdLayoutControlCompany: TStringField;
    mdLayoutControlPrefix: TStringField;
    mdLayoutControlTitle: TStringField;
    mdLayoutControlAddress: TStringField;
    mdLayoutControlCity: TStringField;
    mdLayoutControlState: TStringField;
    mdLayoutControlZipCode: TStringField;
    mdLayoutControlSource: TStringField;
    mdLayoutControlCustomer: TStringField;
    mdLayoutControlHomePhone: TStringField;
    mdLayoutControlFaxPhone: TStringField;
    mdLayoutControlSpouse: TStringField;
    mdLayoutControlOccupation: TStringField;
    mdLayoutControlEmail: TStringField;
    mdLayoutControlTrademark: TStringField;
    mdLayoutControlModel: TStringField;
    mdLayoutControlHP: TSmallintField;
    mdLayoutControlLiter: TFloatField;
    mdLayoutControlCyl: TSmallintField;
    mdLayoutControlTransmissSpeedCount: TSmallintField;
    mdLayoutControlTransmissAutomatic: TStringField;
    mdLayoutControlMPG_City: TSmallintField;
    mdLayoutControlMPG_Highway: TSmallintField;
    mdLayoutControlCategory: TStringField;
    mdLayoutControlCars_Description: TMemoField;
    mdLayoutControlHyperlink: TStringField;
    mdLayoutControlPicture: TBlobField;
    mdLayoutControlPrice: TFloatField;
    mdLayoutControlCustomers_ID: TIntegerField;
    mdLayoutControlOrders_ID: TIntegerField;
    mdLayoutControlCars_ID: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDemo: TdmDemo;

implementation

{$R *.dfm}

procedure TdmDemo.DataModuleCreate(Sender: TObject);
begin
  mdLayoutControl.LoadFromBinaryFile('..\..\Data\LayoutControl.dat');
  mdLayoutControl.Open;
end;

end.
