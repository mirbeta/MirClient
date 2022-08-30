unit EMFCreateEntity;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
 { TfrmEMFCreateEntity }

  TfrmEMFCreateEntity = class(TfrmEMFCustomTutorial)
    btnAddFilm: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnAddMail: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    btnAddToCollection: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    btnRemoveFromCollection: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    procedure btnAddFilmClick(Sender: TObject);
    procedure btnAddMailClick(Sender: TObject);
    procedure btnAddToCollectionClick(Sender: TObject);
    procedure btnRemoveFromCollectionClick(Sender: TObject);
  protected
    function GetDescription: string; override;
    procedure GenerateContent; override;
    procedure Clear; override;
    procedure RegisterActions; override;
    procedure UnregisterActions; override;

    procedure ShowText(AEntityClass: TClass); overload;
  public
    class function GetCaption: string; override;
    class function GetGroupID: Integer; override;
    class function GetID: Integer; override;
  end;

implementation

uses
  uMain, dxCore, dxEMF.DB.Criteria, dxEMF.Types, EMFDemoClasses;

{$R *.dfm}

{ TfrmEMFCreateEntity }

class function TfrmEMFCreateEntity.GetCaption: string;
begin
  Result := 'Creating Entity Objects';
end;

class function TfrmEMFCreateEntity.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityCreationGroupID;
end;

class function TfrmEMFCreateEntity.GetID: Integer;
begin
  Result := 6;
end;

function TfrmEMFCreateEntity.GetDescription: string;
begin
  Result := 'This tutorial shows how to persist entities with auto-generated ' +
    '(Film) and manually generated (Mail) keys, and manage entity obj' +
    'ects in a collection.'
end;

procedure TfrmEMFCreateEntity.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
  GenerateMails;
end;

procedure TfrmEMFCreateEntity.Clear;
begin
  inherited Clear;
  mResults.Clear;
end;

procedure TfrmEMFCreateEntity.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnAddFilm, 'This code creates a TFilm entity object, specifies its properties, and saves the object to a database using the session component. Persisting an entity requires a primary key,' + ' but since the TFilm entity is marked with the Automapping attribute and has a read-only ID property linked to an Integer FID field, this property is considered as an auto-generated primary key. You don''t need to generate its values.', ['add film', 'TFilm|TFilm']);
  RegisterTutorialAction(btnAddMail, 'This code creates a TMail entity object, generates a primary key value, adjusts the object''s properties, and saves the object to a database using the session component. Unlike the TFilm entity, TMail' + ' doesn''t have an auto-generated primary key because the ID property has a setter. You need to assign a unique key value to this property before saving a TMail object.', ['add mail', 'TMail|TMail']);
  RegisterTutorialAction(btnAddToCollection, 'This code creates a TFilm entity object, adjusts its properties, and adds it to a collection of TFilm entity objects retrieved from a database.' + ' Then, the code calls the Find function to retrieve the object from the database by an automatically generated ID key value.' + ' Note that adding the object to the collection automatically saves it to a database, because no BeginTrackingChanges method call is made to enable the session component to accumulate object changes.' + ' ', ['add film to collection', 'TFilm|TFilm']);
  RegisterTutorialAction(btnRemoveFromCollection, 'This code retrieves a collection of TFilm entity objects from a database, accesses the collection''s last object and stores its ID key value. Then,' + ' the code enforces permanent object removals using the collection''s DeleteObjectOnRemove property, removes the' + ' object from the collection, and deletes the corresponding record from the database table. As a result, the Find function cannot locate any TFilm entity object whose ID matches the stored key value.', ['remove film from collection', 'TFilm|TFilm']);
end;

procedure TfrmEMFCreateEntity.UnregisterActions;
begin
  UnregisterTutorialAction(btnRemoveFromCollection);
  UnregisterTutorialAction(btnAddToCollection);
  UnregisterTutorialAction(btnAddMail);
  UnregisterTutorialAction(btnAddFilm);
  inherited UnregisterActions;
end;

procedure TfrmEMFCreateEntity.ShowText(AEntityClass: TClass);
begin
  ShowText(Format('The following %s entity object was created:', [AEntityClass.ClassName]), True);
end;

//<add film
procedure TfrmEMFCreateEntity.btnAddFilmClick(Sender: TObject);
var
  AFilm: TFilm;
begin
  AFilm := TFilm.Create;
  AFilm.Title := GetRandomTitle;
  AFilm.Runtime := GetRandomRuntime;
  AFilm.Year := GetRandomYear;
  AFilm.Tagline := 'Tagline for ' + AFilm.Title;
  AFilm.Plotoutline := 'Plot outline for ' + AFilm.Title;
  AFilm.Website := 'http://www.website' + IntToStr(Random(1000)) + '.com';
  AFilm.Color := True;

  EMFSession.Save(AFilm);

  ShowText(TFilm);
  ShowResults<TFilm>(AFilm, False);
end;
//>add film

//<add mail
procedure TfrmEMFCreateEntity.btnAddMailClick(Sender: TObject);
var
  AMostRecentMail, AMail: TMail;
  AMaxPrimaryKey: Integer;
  ASortBy: TdxSortByExpressions;
begin
  ASortBy := TdxSortByExpressions.Create;
  ASortBy.Add(TdxSortByExpression.Create(TdxOperandProperty.Create('ID')));
  AMostRecentMail := EMFSession.GetObjects<TMail>(nil, ASortBy).Last;
  if AMostRecentMail <> nil then
    AMaxPrimaryKey := AMostRecentMail.ID
  else
    AMaxPrimaryKey := 0;

  AMail := TMail.Create;
  AMail.ID := AMaxPrimaryKey + 1;
  AMail.Subject := 'Subject' + GetRandomIntegerAsString;
  AMail.From := 'User' + GetRandomIntegerAsString;
  AMail.Sent := GetRandomInteger;
  AMail.Size := GetRandomInteger;
  AMail.HasAttachment := GetRandomBoolean;
  AMail.Priority := Random(3);

  EMFSession.Save(AMail);

  ShowText(TMail);
  ShowResults<TMail>(AMail, False);
end;
//>add mail

//<add film to collection
procedure TfrmEMFCreateEntity.btnAddToCollectionClick(Sender: TObject);
var
  AFilm: TFilm;
  AFilmCollection: IdxEMFCollection<TFilm>;
begin
  AFilm := TFilm.Create;
  AFilm.Title := GetRandomTitle;
  AFilm.Runtime := GetRandomRuntime;
  AFilm.Year := GetRandomYear;
  AFilm.Views := GetRandomViews;

  AFilmCollection := EMFSession.GetObjects<TFilm>;
  if AFilmCollection <> nil then
    AFilmCollection.Add(AFilm);

  ShowText('The following TFilm entity object was added to the collection:', True);
  ShowResults(EMFSession.Find<TFilm>(AFilm.ID), False);
end;
//>add film to collection

//<remove film from collection
procedure TfrmEMFCreateEntity.btnRemoveFromCollectionClick(Sender: TObject);
var
  AID: Integer;
  AFilm: TFilm;
  AFilmCollection: IdxEMFCollection<TFilm>;
begin
  AID := -1;
  AFilmCollection := EMFSession.GetObjects<TFilm>;
  if AFilmCollection <> nil then
  begin
    AFilmCollection.DeleteObjectOnRemove := True;
    AFilm := AFilmCollection.Last;
    AID := AFilm.ID;
    ShowText('The following TFilm entity object was removed from the collection:', True);
    ShowResults<TFilm>(AFilm, False);
    AFilmCollection.Remove(AFilm);
  end;

  ShowLocateEntityText(TFilm, AID);
  AFilm := EMFSession.Find<TFilm>(AID);
  if AFilm = nil then
    ShowResults(EntityObjectNotFound)
  else
    ShowResults<TFilm>(AFilm, False);
end;
//>remove film from collection

initialization
  TfrmEMFCreateEntity.Register;

end.
