unit FrameAnimationDemoDM;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, DB, DBClient, MidasLib;

type
  TDM = class(TDataModule)
    dsHomePhotos: TDataSource;
    clHomePhotos: TClientDataSet;
    dsHomesAndAgents: TDataSource;
    clHomesAndAgents: TClientDataSet;
    dsHomesAndHomes: TDataSource;
    clHomesAndHomes: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
var
  ADataPath: string;
begin
  ADataPath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  clHomePhotos.LoadFromFile(ADataPath + 'HomePhotos.cds');
  clHomesAndAgents.LoadFromFile(ADataPath + 'HomesAndAgents.cds');
  clHomesAndHomes.LoadFromFile(ADataPath + 'HomesAndHomes.cds');
end;

end.
