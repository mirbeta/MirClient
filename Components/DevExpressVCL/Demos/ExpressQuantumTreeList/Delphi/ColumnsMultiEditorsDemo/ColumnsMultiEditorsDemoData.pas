unit ColumnsMultiEditorsDemoData;

interface

uses
  SysUtils, Classes, DB, Forms, cxClasses, cxStyles, cxTL, dxmdaset;

type
  TColumnsMultiEditorsDemoDataDM = class(TDataModule)
    dsPersons: TDataSource;
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    mdPersons: TdxMemData;
    mdPersonsID: TAutoIncField;
    mdPersonsName: TStringField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColumnsMultiEditorsDemoDataDM: TColumnsMultiEditorsDemoDataDM;

implementation

{$R *.dfm}

procedure TColumnsMultiEditorsDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdPersons.LoadFromBinaryFile(DataPath + 'Persons.dat');
  mdPersons.Open;
end;

end.
