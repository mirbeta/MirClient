unit EMFUsingDataset;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, Grids, DBGrids, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, dxEMF.DataSet, cxNavigator, cxDBNavigator, EMFCustomTutorial;

type
  { TfrmEMFUsingDataset }

  TfrmEMFUsingDataset = class(TfrmEMFCustomTutorial)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    dxEMFDataset1: TdxEMFDataset;
    btnRefresh: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    cxDBNavigator1: TcxDBNavigator;
    dxLayoutItem3: TdxLayoutItem;
    procedure btnRefreshClick(Sender: TObject);
  protected
    procedure GenerateContent; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    procedure RegisterActions; override;
    procedure UnregisterActions; override;
  public
    class function GetCaption: string; override;
    class function GetID: Integer; override;
    class function GetSortIndex: Integer; override;
  end;

implementation

{$R *.dfm}

uses
  uMain, dxEMF.Types, dxEMF.DB.Criteria, dxEMF.Linq, dxEMF.Linq.Expressions, EMFDemoClasses;

{ TfrmEMFUsingDataset }

class function TfrmEMFUsingDataset.GetCaption: string;
begin
  Result := 'Binding a Data-Aware Control to a Database Table';
end;

function TfrmEMFUsingDataset.GetDescription: string;
begin
  Result := 'This tutorial shows how to use a dataset component (TdxEMFDataSet)' +
    ' shipped with the ExpressEntityMapping Framework to bind a standard VCL ' +
    'TDBGrid control to a database table. Click the data navigator''s buttons to' +
    ' navigate grid records and manipulate their data.'
end;

class function TfrmEMFUsingDataset.GetID: Integer;
begin
  Result := 20;
end;

class function TfrmEMFUsingDataset.GetSortIndex: Integer;
begin
  Result := 5;
end;

procedure TfrmEMFUsingDataset.GenerateContent;
begin
  inherited GenerateContent;
  GenerateMails;
end;

procedure TfrmEMFUsingDataset.Initialize;
begin
  inherited Initialize;
  dxEMFDataset1.Session := EMFSession;
  btnRefreshClick(Self);
end;

procedure TfrmEMFUsingDataset.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnRefresh, 'This button repopulates the dataset with TFilm entity objects retrieved from the Film database table using the session component.', ['bind using dataset refresh data', 'TMail|TMail']);
end;

procedure TfrmEMFUsingDataset.UnregisterActions;
begin
  UnregisterTutorialAction(btnRefresh);
  inherited UnregisterActions;
end;

//<bind using dataset refresh data
procedure TfrmEMFUsingDataset.btnRefreshClick(Sender: TObject);
begin
  dxEMFDataset1.Close;
  dxEMFDataset1.AssignData<TFilm>(EMFSession.GetObjects<TFilm>);
  dxEMFDataset1.Open;
end;
//>bind using dataset refresh data

initialization
  TfrmEMFUsingDataset.Register;

end.
