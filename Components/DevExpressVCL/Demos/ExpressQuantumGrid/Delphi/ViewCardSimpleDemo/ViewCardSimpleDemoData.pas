unit ViewCardSimpleDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, cxGridCardView, cxClasses,
  cxGridTableView, Forms, DBClient, MidasLib;

type
  TViewCardSimpleDemoMainDM = class(TDataModule)
    tlbDEPARTMENTS: TClientDataSet;
    dsDEPARTMENTS: TDataSource;
    dsUSERS: TDataSource;
    tlbUSERS: TClientDataSet;
    tlbDEPARTMENTSID: TAutoIncField;
    tlbDEPARTMENTSNAME: TStringField;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  ViewCardSimpleDemoMainDM: TViewCardSimpleDemoMainDM;

implementation

{$R *.dfm}

procedure TViewCardSimpleDemoMainDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tlbDEPARTMENTS.LoadFromFile(APath + 'Departments.xml');
  tlbUSERS.LoadFromFile(APath + 'Users.xml');
end;

end.
