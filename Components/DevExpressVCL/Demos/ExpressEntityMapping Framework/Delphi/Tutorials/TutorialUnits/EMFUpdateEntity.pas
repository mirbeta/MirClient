unit EMFUpdateEntity;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, dxEMF.Types, EMFCustomTutorial, EMFDemoClasses;

type
  { TfrmEMFUpdateEntity }

  TfrmEMFUpdateEntity = class(TfrmEMFCustomTutorial)
    btnModifyLocalEntity: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnModifyDBEntity: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    procedure btnModifyLocalEntityClick(Sender: TObject);
    procedure btnModifyDBEntityClick(Sender: TObject);
  protected
    procedure GenerateContent; override;
    function GetDescription: string; override;
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
  uMain, dxCore, dxEMF.Core, dxEMF.Utils;

{ TfrmEMFModifyEntity }

class function TfrmEMFUpdateEntity.GetCaption: string;
begin
  Result := 'Updating Entity Objects';
end;

function TfrmEMFUpdateEntity.GetDescription: string;
begin
  Result := 'This tutorial shows how to update entity objects locally and save ' +
    'the changes to the database.'
end;

class function TfrmEMFUpdateEntity.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityModificationGroupID;
end;

class function TfrmEMFUpdateEntity.GetID: Integer;
begin
  Result := 7;
end;

procedure TfrmEMFUpdateEntity.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
end;

procedure TfrmEMFUpdateEntity.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnModifyLocalEntity, 'This code modifies a TFilm entity object''s Views property value without saving changes to the database.', ['update object locally', 'TFilm|TFilm', 'TTeam|TTeam']);
  RegisterTutorialAction(btnModifyDBEntity, 'This code modifies a TFilm entity object''s Views property value and saves changes to the database by calling the session component''s Save method.', ['update object in database', 'TFilm|TFilm']);
end;

procedure TfrmEMFUpdateEntity.UnregisterActions;
begin
  UnregisterTutorialAction(btnModifyDBEntity);
  UnregisterTutorialAction(btnModifyLocalEntity);
  inherited UnregisterActions;
end;

//<update object locally
procedure TfrmEMFUpdateEntity.btnModifyLocalEntityClick(Sender: TObject);
var
  AFilm: TFilm;
begin
  AFilm := EMFSession.GetObjects<TFilm>.Last;

  ShowText('The following TFilm entity object is going to be updated:', True);
  ShowResults<TFilm>(AFilm, False);

  AFilm.Views := AFilm.Views + 1;
  ShowText(dxCRLF + 'The object''s Views property value was increased:');
  ShowResults<TFilm>(EMFSession.Find<TFilm>(AFilm.ID), False);

  EMFSession.Reload(AFilm);
  ShowText(dxCRLF + 'The object is reloaded from the database and all local changes to it are discarded:');
  ShowResults<TFilm>(EMFSession.Find<TFilm>(AFilm.ID), False);
end;
//>update object locally

//<update object in database
procedure TfrmEMFUpdateEntity.btnModifyDBEntityClick(Sender: TObject);
var
  AFilm: TFilm;
begin
  AFilm := EMFSession.GetObjects<TFilm>.Last;

  ShowText('The following TFilm entity object is going to be updated:', True);
  ShowResults<TFilm>(AFilm, False);

  AFilm.Views := AFilm.Views + 1;
  ShowText(dxCRLF + 'The object''s Views property value was increased:');
  ShowResults<TFilm>(EMFSession.Find<TFilm>(AFilm.ID), False);

  EMFSession.Save(AFilm);
  EMFSession.Reload(AFilm);
  ShowText(dxCRLF + 'All the changes to the entity object are stored in the database:');
  ShowResults<TFilm>(EMFSession.Find<TFilm>(AFilm.ID), False);
end;
//>update object in database

initialization
  TfrmEMFUpdateEntity.Register;

end.
