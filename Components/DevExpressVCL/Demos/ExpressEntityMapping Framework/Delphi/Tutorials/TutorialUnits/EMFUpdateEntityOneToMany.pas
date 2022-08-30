unit EMFUpdateEntityOneToMany;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, dxEMF.Types, EMFCustomTutorial, EMFDemoClasses;

type
  { TfrmEMFUpdateEntityOneToMany }

  TfrmEMFUpdateEntityOneToMany = class(TfrmEMFCustomTutorial)
    btnModifyCollection: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    procedure btnModifyCollectionClick(Sender: TObject);
  protected
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
  uMain, dxCore, dxEMF.Core, dxEMF.Utils;

{ TfrmEMFUpdateEntityOneToMany }

class function TfrmEMFUpdateEntityOneToMany.GetCaption: string;
begin
  Result := 'Updating Entity Objects in a One-to-Many Relationship';
end;

function TfrmEMFUpdateEntityOneToMany.GetDescription: string;
begin
  Result := 'This tutorial shows how the cascade update operation works for entities in a ' +
    'One-to-Many relationship. A TTeam entity at the association''s "one" end relates to ' +
    'TMember entities at the "many" end. Marking the Members collection property''s field' +
    ' with the Aggregated attribute (in addition to the Association attribute) in the ' +
    'TTeam entity enables the field''s cascade option.';
end;

class function TfrmEMFUpdateEntityOneToMany.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityModificationGroupID;
end;

class function TfrmEMFUpdateEntityOneToMany.GetID: Integer;
begin
  Result := 9;
end;

procedure TfrmEMFUpdateEntityOneToMany.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
end;

procedure TfrmEMFUpdateEntityOneToMany.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnModifyCollection, 'This code retrieves all TTeam entity objects from the database as a collection and calls the session component''s BeginTrackingChanges method to enable accumulating entity object changes. ' + 'Then, the code traverses the collection, modifies the TTeam and TMember entity objects, and calls the session component''s Save method for each modified object to store its changes in memory. ' + 'After that, the FlushChanges method call applies the accumulated changes to the database. If an error occurs when updating entity objects or saving/applying changes, ' + 'the DropChanges method discards pending changes to the database while retaining changes in object instances.', ['update objects in one-to-many relationship', 'TTeam|TTeam', 'TMember|TMember']);
end;

procedure TfrmEMFUpdateEntityOneToMany.UnregisterActions;
begin
  UnregisterTutorialAction(btnModifyCollection);
  inherited UnregisterActions;
end;

//<update objects in one-to-many relationship
procedure TfrmEMFUpdateEntityOneToMany.btnModifyCollectionClick(Sender: TObject);
var
  AMember: TMember;
  ATeam: TTeam;
  ATeams: IdxEMFCollection<TTeam>;
begin
  GenerateTeams;//<tutorial
  ATeams := EMFSession.GetObjects<TTeam>;
  ShowText('The following TTeam and TMember entity objects are going to be updated:', True);
  ShowTeams(ATeams);

  ShowText(dxCRLF + 'The session component started tracking entity object changes.');
  EMFSession.BeginTrackingChanges;
  try
    for ATeam in ATeams do
    begin
      ATeam.Name := ATeam.Name + '_' + GetRandomIntegerAsString;
      for AMember in ATeam.Members do
        AMember.EMail := ATeam.Name + '-' + AMember.EMail;
      EMFSession.Save(ATeam);
    end;
    ShowText('The team names and member emails were modified:');
    ShowTeams(ATeams);

    EMFSession.FlushChanges;
    ShowText(dxCRLF + 'The session component applied the changes to the database and finished tracking entity object changes.');

    for ATeam in ATeams do
    begin
      for AMember in ATeam.Members do
        EMFSession.Reload(AMember);
      EMFSession.Reload(ATeam);
    end;
    ShowText('The database now contains the following TTeam and TMember entity objects:');
    ShowTeams(ATeams);
  except
    EMFSession.DropChanges;
    ShowText(dxCRLF + 'An error occurred. No changes were applied to the database.');
    ShowText('The session component finished tracking entity object changes.');
  end;
end;
//>update objects in one-to-many relationship

initialization
  TfrmEMFUpdateEntityOneToMany.Register;

end.
