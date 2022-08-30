unit EditorsMaskDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, Forms, cxClasses, DBClient, MidasLib;

type
  TEditorsMaskDemoMainDM = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    stBlueDark: TcxStyle;
    stGold: TcxStyle;
    stBlueLight: TcxStyle;
    stBlueBright: TcxStyle;
    stYellowLight: TcxStyle;
    stGreyLight: TcxStyle;
    stBlueSky: TcxStyle;
    DataSourceUSERS: TDataSource;
    DataSourceDEPARTMENTS: TDataSource;
    cdsUsers: TClientDataSet;
    cdsDepartments: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditorsMaskDemoMainDM: TEditorsMaskDemoMainDM;

implementation

{$R *.dfm}

procedure TEditorsMaskDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  cdsDepartments.LoadFromFile('..\..\Data\Departments.xml');
  cdsUsers.LoadFromFile('..\..\Data\Users.xml');
end;

end.
