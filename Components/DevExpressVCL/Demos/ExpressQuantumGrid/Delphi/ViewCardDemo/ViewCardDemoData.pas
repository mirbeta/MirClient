unit ViewCardDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList,
  Controls, cxEdit, cxEditRepositoryItems, cxDBEditRepository, cxClasses,
  cxGridCardView, DBClient, MidasLib;

type
  TViewCardDemoDataDM = class(TDataModule)
    tblPersons: TClientDataSet;
    dsPersons: TDataSource;
    tblPersonsID: TAutoIncField;
    tblPersonsFIRSTNAME: TStringField;
    tblPersonsSECONDNAME: TStringField;
    tblPersonsGENDER: TBooleanField;
    tblPersonsBIRTHNAME: TStringField;
    tblPersonsDATEOFBIRTH: TDateTimeField;
    tblPersonsBIRTHCOUNTRY: TIntegerField;
    tblPersonsLOCATIONOFBIRTH: TStringField;
    tblPersonsBIOGRAPHY: TMemoField;
    tblPersonsNICKNAME: TStringField;
    tblPersonsFullName: TStringField;
    ilPics: TImageList;
    tblPersonsHOMEPAGE: TStringField;
    tblCountries: TClientDataSet;
    dsCountries: TDataSource;
    procedure tblPersonsCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewCardDemoDataDM: TViewCardDemoDataDM;

implementation

{$R *.dfm}

procedure TViewCardDemoDataDM.tblPersonsCalcFields(DataSet: TDataSet);
begin
  with DataSet do
    tblPersonsFullName.AsString := Format('%s %s (%d)',[tblPersonsFirstName.AsString,
      tblPersonsSecondName.AsString, tblPersonsID.AsInteger]);
end;

procedure TViewCardDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblPersons.LoadFromFile(APath + 'Persons.xml');
  tblCountries.LoadFromFile(APath + 'Countries.xml');
end;

end.

