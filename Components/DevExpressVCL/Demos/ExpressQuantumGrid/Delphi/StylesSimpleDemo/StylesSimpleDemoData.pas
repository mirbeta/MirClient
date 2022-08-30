unit StylesSimpleDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls,
  cxGridTableView, cxClasses, DBClient, MidasLib;

type
  TStylesSimpleDemoMainDM = class(TDataModule)
    dsPersons: TDataSource;
    dsCountries: TDataSource;
    StyleRepository: TcxStyleRepository;
    Sunny: TcxStyle;
    Dark: TcxStyle;
    Golden: TcxStyle;
    Summer: TcxStyle;
    Autumn: TcxStyle;
    Bright: TcxStyle;
    Cold: TcxStyle;
    Spring: TcxStyle;
    Light: TcxStyle;
    Winter: TcxStyle;
    Depth: TcxStyle;
    UserStyleSheet: TcxGridTableViewStyleSheet;
    cdsPersons: TClientDataSet;
    cdsCountries: TClientDataSet;
    cdsPersonsID: TAutoIncField;
    cdsPersonsFIRSTNAME: TStringField;
    cdsPersonsSECONDNAME: TStringField;
    cdsPersonsGENDER: TBooleanField;
    cdsPersonsBIRTHNAME: TStringField;
    cdsPersonsDATEOFBIRTH: TDateTimeField;
    cdsPersonsBIRTHCOUNTRY: TIntegerField;
    cdsPersonsLOCATIONOFBIRTH: TStringField;
    cdsPersonsBIOGRAPHY: TMemoField;
    cdsPersonsNICKNAME: TStringField;
    cdsPersonsHOMEPAGE: TStringField;
    cdsCountriesID: TAutoIncField;
    cdsCountriesNAME: TStringField;
    cdsCountriesACRONYM: TStringField;
    cdsCountriesNATIONALFLAG: TBlobField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StylesSimpleDemoMainDM: TStylesSimpleDemoMainDM;

implementation

{$R *.dfm}

procedure TStylesSimpleDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  cdsPersons.LoadFromFile('..\..\Data\Persons.xml');
  cdsPersons.Open;
  cdsCountries.LoadFromFile('..\..\Data\Countries.xml');
  cdsCountries.Open;
end;

end.

