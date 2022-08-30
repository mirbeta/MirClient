unit EMFCreateConnection;

interface

uses
  StdCtrls, Classes, Controls, ExtCtrls, DB, DBClient, Menus, cxGraphics, cxClasses, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxUIAdorners,
  dxLayoutLookAndFeels, cxLabel, cxButtons, cxRichEdit, dxLayoutContainer, cxTextEdit, cxMemo, dxLayoutControl,
  dxEMF.Core, dxEMF.DataProvider.FireDAC, EMFCustomTutorial;

type
  { TfrmEMFCreateConnection }

  TfrmEMFCreateConnection = class(TfrmEMFCustomTutorial)
    btnStep1: TcxButton;
    liShowData: TdxLayoutItem;
    btnStep2: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnStep3: TcxButton;
    btnStep4: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    procedure btnStep1Click(Sender: TObject);
    procedure btnStep2Click(Sender: TObject);
    procedure btnStep3Click(Sender: TObject);
    procedure btnStep4Click(Sender: TObject);
  strict private
    FCurrentStep: Integer;
  protected
    function GetDescription: string; override;
    procedure GenerateContent; override;
    procedure Initialize; override;
    procedure RegisterActions; override;
    procedure UnregisterActions; override;
    procedure UpdateControls; override;
  public
    class function GetCaption: string; override;
    class function GetID: Integer; override;
    class function GetSortIndex: Integer; override;
  end;

implementation

uses
  SysUtils, dxCore, dxEMF.Types, dxEMF.MetaData, EMFDemoClasses;

{$R *.dfm}

{ TfrmEMFCreateConnection }

class function TfrmEMFCreateConnection.GetCaption: string;
begin
  Result := 'Connecting to a Database';
end;

class function TfrmEMFCreateConnection.GetID: Integer;
begin
  Result := 4;
end;

class function TfrmEMFCreateConnection.GetSortIndex: Integer;
begin
  Result := 0;
end;

procedure TfrmEMFCreateConnection.btnStep1Click(Sender: TObject);
begin
  CreateDatabaseConnection;
  mResults.Clear;
  mResults.Lines.Add('Completed Step 1: Create a FireDAC connection to an SQLite database.');
  Inc(FCurrentStep);
  UpdateControls;
end;

function TfrmEMFCreateConnection.GetDescription: string;
begin
  Result := 'This tutorial shows how to connect a session component to a database and retrieve data from it.'
end;

procedure TfrmEMFCreateConnection.GenerateContent;
begin
  GenerateFilms;
end;

procedure TfrmEMFCreateConnection.Initialize;
begin
  FCurrentStep := 1;
  inherited Initialize;
end;

procedure TfrmEMFCreateConnection.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnStep1, 'This code creates a standard FireDAC connection and adjusts its settings to connect to an SQLite database. The connection string is database-specific - it can be modified to access any supported database.', ['create database connection']);
  RegisterTutorialAction(btnStep2, 'This code creates a FireDAC data provider shipped with the ExpressEntityMapping Framework and links it to the FireDAC connection.', ['create data provider']);
  RegisterTutorialAction(btnStep3, 'This code creates an ExpressEntityMapping Framework session component and links it to the FireDAC data provider.', ['create session']);
  RegisterTutorialAction(btnStep4, 'This code retrieves all TFilm entity objects from the database and lists their data.', ['show data', 'TFilm|TFilm']);
end;

procedure TfrmEMFCreateConnection.UnregisterActions;
begin
  UnregisterTutorialAction(btnStep4);
  UnregisterTutorialAction(btnStep3);
  UnregisterTutorialAction(btnStep2);
  UnregisterTutorialAction(btnStep1);
  inherited UnregisterActions;
end;

procedure TfrmEMFCreateConnection.UpdateControls;

  procedure CheckEnabled(AButton: TcxButton);
  begin
    AButton.Enabled := FCurrentStep = AButton.Tag;
  end;

begin
  inherited UpdateControls;
  CheckEnabled(btnStep1);
  CheckEnabled(btnStep2);
  CheckEnabled(btnStep3);
  CheckEnabled(btnStep4);
end;

//<create data provider
procedure TfrmEMFCreateConnection.btnStep2Click(Sender: TObject);
begin
  FreeAndNil(FEMFDataProvider); //<tutorial>
  FEMFDataProvider := TdxEMFFireDACDataProvider.Create(Self);
  FEMFDataProvider.Connection := FConnection;
  mResults.Lines.Add('Completed Step 2: Create a FireDAC data provider and link it to the connection.'); //<tutorial>
  Inc(FCurrentStep); //<tutorial>
  UpdateControls; //<tutorial>
end;
//>create data provider

//<create session
procedure TfrmEMFCreateConnection.btnStep3Click(Sender: TObject);
begin
  FreeAndNil(FEMFSession); //<tutorial>
  FEMFSession := TdxEMFSession.Create(Self);
  FEMFSession.DataProvider := FEMFDataProvider;
  mResults.Lines.Add('Completed Step 3: Create a session component and link it to the data provider.'); //<tutorial>
  Inc(FCurrentStep); //<tutorial>
  UpdateControls; //<tutorial>
end;
//>create session

//<show data
procedure TfrmEMFCreateConnection.btnStep4Click(Sender: TObject);
var
  AFilmCollection: IdxEMFCollection<TFilm>;
begin
  AFilmCollection := EMFSession.GetObjects<TFilm>;
  ShowQueryResults<TFilm>(AFilmCollection, 'TFilm entity objects stored in the database:', False);
  ShowText(dxCRLF + 'Completed Step 4: Retrieve TFilm entity objects from the database and list their data.'); //<tutorial>
  FCurrentStep := 1; //<tutorial>
  UpdateControls; //<tutorial>
end;
//>show data

initialization
  TfrmEMFCreateConnection.Register;

end.
