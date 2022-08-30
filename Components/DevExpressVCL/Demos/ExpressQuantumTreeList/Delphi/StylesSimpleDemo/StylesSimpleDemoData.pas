unit StylesSimpleDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls,
  cxClasses, cxTL, dxmdaset;

type
  TStylesSimpleDemoDataDM = class(TDataModule)
    dsDEPARTMENTS: TDataSource;
    dsPERSONS: TDataSource;
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
    UserStyleSheet: TcxTreeListStyleSheet;
    mdDEPARTMENTS: TdxMemData;
    mdPERSONS: TdxMemData;
    mdDEPARTMENTSID: TAutoIncField;
    mdDEPARTMENTSPARENTID: TIntegerField;
    mdDEPARTMENTSMANAGERID: TIntegerField;
    mdDEPARTMENTSNAME: TStringField;
    mdDEPARTMENTSBUDGET: TFloatField;
    mdDEPARTMENTSLOCATION: TStringField;
    mdDEPARTMENTSPHONE: TStringField;
    mdDEPARTMENTSFAX: TStringField;
    mdDEPARTMENTSEMAIL: TStringField;
    mdDEPARTMENTSVACANCY: TBooleanField;
    mdPERSONSID: TAutoIncField;
    mdPERSONSName: TStringField;
    mdPERSONSCountry: TStringField;
    mdPERSONSPostalCode: TStringField;
    mdPERSONSCity: TStringField;
    mdPERSONSAddress: TStringField;
    mdPERSONSPhone: TStringField;
    mdPERSONSFax: TStringField;
    mdPERSONSEMAIL: TStringField;
    mdPERSONSHOMEPAGE: TStringField;
    mdPERSONSDepartmentID: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetParentValue(AValue: Variant);
  end;

var
  StylesSimpleDemoDataDM: TStylesSimpleDemoDataDM;

implementation

{$R *.dfm}

procedure TStylesSimpleDemoDataDM.SetParentValue(AValue: Variant);
begin
  if mdDepartments.State in [dsEdit, dsInsert] then
    mdDepartments.FindField('ParentID').Value := AValue;
end;

procedure TStylesSimpleDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdPERSONS.LoadFromBinaryFile(DataPath + 'Persons.dat');
  mdDEPARTMENTS.LoadFromBinaryFile(DataPath + 'Departments.dat');
  mdDEPARTMENTS.Open;
  mdPERSONS.Open;
end;

end.

