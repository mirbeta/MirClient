unit EMFDeleteEntity;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFDeleteEntity }

  TfrmEMFDeleteEntity = class(TfrmEMFCustomTutorial)
    btnDeleteLocalEntity: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnDeleteDBEntity: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    procedure btnDeleteLocalEntityClick(Sender: TObject);
    procedure btnDeleteDBEntityClick(Sender: TObject);
  protected
    function GetActionButtonWidth: Integer; override;
    function GetDescription: string; override;
    procedure GenerateContent; override;
    procedure RegisterActions; override;
    procedure UnregisterActions; override;
  public
    class function GetCaption: string; override;
    class function GetGroupID: Integer; override;
    class function GetID: Integer; override;
  end;

implementation

{$R *.dfm}

uses
  uMain, dxCore, dxEMF.Core, dxEMF.Utils, EMFDemoClasses;

{ TfrmEMFDeleteEntity }

class function TfrmEMFDeleteEntity.GetCaption: string;
begin
  Result := 'Deleting Entity Objects';
end;

class function TfrmEMFDeleteEntity.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityDeletingGroupID;
end;

class function TfrmEMFDeleteEntity.GetID: Integer;
begin
  Result := 8;
end;

function TfrmEMFDeleteEntity.GetActionButtonWidth: Integer;
begin
  Result := 190;
end;

function TfrmEMFDeleteEntity.GetDescription: string;
begin
  Result := 'This tutorial shows how deferred object deletion works when a se' +
    'ssion component marks entity objects as deleted rather than immediately ' +
    'deleting them in a database.';
end;

procedure TfrmEMFDeleteEntity.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
end;

procedure TfrmEMFDeleteEntity.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnDeleteLocalEntity, 'This code marks a TFilm entity object as deleted without calling the session component''s FlushChanges method to save changes to the database.', ['delete object locally', 'TFilm|TFilm']);
  RegisterTutorialAction(btnDeleteDBEntity, 'This code marks a TFilm entity object as deleted and calls the session component''s FlushChanges method to save changes to the database and delete the corresponding record in the Film table.', ['delete object in database', 'TFilm|TFilm']);
end;

procedure TfrmEMFDeleteEntity.UnregisterActions;
begin
  UnregisterTutorialAction(btnDeleteDBEntity);
  UnregisterTutorialAction(btnDeleteLocalEntity);
  inherited UnregisterActions;
end;

//<delete object locally
procedure TfrmEMFDeleteEntity.btnDeleteLocalEntityClick(Sender: TObject);
var
  AFilm: TFilm;
  AFilmID: Integer;
begin
  AFilm := EMFSession.GetObjects<TFilm>.Last;
  AFilmID := AFilm.ID;
  ShowText('The session component started tracking entity object changes.');
  EMFSession.BeginTrackingChanges;
  try
    EMFSession.Delete(AFilm);
    ShowText(FilmMarkedAsDeletedText);
    ShowResults<TFilm>(AFilm, False);
  finally
    EMFSession.DropChanges;
    ShowText(dxCRLF + 'The session component discarded pending changes to the database and finished tracking entity object changes.');
  end;

  ShowLocateEntityText<TFilm>(AFilmID);
end;
//>delete object locally

//<delete object in database
procedure TfrmEMFDeleteEntity.btnDeleteDBEntityClick(Sender: TObject);
var
  AFilm: TFilm;
  AFilmID: Integer;
begin
  AFilm := EMFSession.GetObjects<TFilm>.Last;
  AFilmID := AFilm.ID;
  ShowText('The session component started tracking entity object changes.');
  EMFSession.BeginTrackingChanges;
  try
    EMFSession.Delete(AFilm);
    ShowText(FilmMarkedAsDeletedText, True);
    ShowResults<TFilm>(AFilm, False);
    EMFSession.FlushChanges;
    ShowText(dxCRLF + 'The session component applied the changes to the database and finished tracking entity object changes.');
  except
    EMFSession.DropChanges;
    ShowText(dxCRLF + 'An error occurred. No changes were applied to the database.');
    ShowText('The session component finished tracking entity object changes.');
  end;

  ShowLocateEntityText<TFilm>(AFilmID);
end;
//>delete object in database

initialization
  TfrmEMFDeleteEntity.Register;

end.
