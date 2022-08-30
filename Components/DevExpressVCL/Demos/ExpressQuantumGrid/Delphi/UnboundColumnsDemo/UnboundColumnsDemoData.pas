unit UnboundColumnsDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxEdit,
  cxEditRepositoryItems, cxDBEditRepository, cxClasses, cxGridCardView, DBClient, MidasLib;

type
  TUnboundColumnsDemoDataDM = class(TDataModule)
    tblCustomers: TClientDataSet;
    dsCustomers: TDataSource;
    tblCustomersID: TAutoIncField;
    tblCustomersFIRSTNAME: TStringField;
    tblCustomersLASTNAME: TStringField;
    tblCustomersCOMPANYNAME: TStringField;
    tblCustomersPAYMENTTYPE: TIntegerField;
    tblCustomersPRODUCTID: TIntegerField;
    tblCustomersCUSTOMER: TBooleanField;
    tblCustomersPURCHASEDATE: TDateTimeField;
    tblCustomersPAYMENTAMOUNT: TCurrencyField;
    tblCustomersCOPIES: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UnboundColumnsDemoDataDM: TUnboundColumnsDemoDataDM;

implementation

{$R *.dfm}

procedure TUnboundColumnsDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  tblCustomers.LoadFromFile(ExtractFilePath(Application.ExeName) + '..\..\Data\Customer.xml');
end;

end.

