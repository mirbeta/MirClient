unit EMFCreateEntityOneToMany;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFCreateEntityOneToMany }

  TfrmEMFCreateEntityOneToMany = class(TfrmEMFCustomTutorial)
    btnOneToMany: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    procedure btnOneToManyClick(Sender: TObject);
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
  uMain, dxCore, dxEMF.Types, EMFDemoClasses;

{ TfrmEMFCreateEntityOneToMany }

class function TfrmEMFCreateEntityOneToMany.GetCaption: string;
begin
  Result := 'Creating Entity Objects in a One-to-Many Relationship';
end;

class function TfrmEMFCreateEntityOneToMany.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityCreationGroupID;
end;

class function TfrmEMFCreateEntityOneToMany.GetID: Integer;
begin
  Result := 13;
end;

function TfrmEMFCreateEntityOneToMany.GetDescription: string;
begin
  Result := 'This tutorial shows how to use an association to create objects ' +
    'for entities in a One-to-Many relationship. A TTeam entity at the' +
    ' association''s "one" end relates to TMember entities at the "many" end.'
end;

procedure TfrmEMFCreateEntityOneToMany.GenerateContent;
begin
  inherited GenerateContent;
  GenerateTeams;
end;

procedure TfrmEMFCreateEntityOneToMany.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnOneToMany, 'This code creates a TTeam entity object (the "one" end), adds ten new TMember entity objects (the "many" end) to the object''s Members collection, and saves these entity objects to a database by applying changes to the session component.' +
    ' Then, the code reloads the created TTeam and TMember entity objects from the database and lists their data. Object reloading is done for demonstration purposes only as the objects already contain up-to-date information.' +
    ' Note that the BeginTrackingChanges method call enables the session component to accumulate object changes until the FlushChanges method is called to apply them to the database.', ['create objects one-to-many relationship', 'TTeam|TTeam', 'TMember|TMember']);
end;

procedure TfrmEMFCreateEntityOneToMany.UnregisterActions;
begin
  UnregisterTutorialAction(btnOneToMany);
  inherited UnregisterActions;
end;

//<create objects one-to-many relationship
procedure TfrmEMFCreateEntityOneToMany.btnOneToManyClick(Sender: TObject);
var
  I: Integer;
  AMember: TMember;
  ATeam: TTeam;
begin
  ShowText('The session component started tracking entity object changes.', True);
  EMFSession.BeginTrackingChanges;
  try
    ATeam := TTeam.Create;
    ATeam.Name := 'New Team';
    for I := 0 to 9 do
    begin
      AMember := TMember.Create;
      AMember.Name := GetRandomUser;
      AMember.EMail := GetDefaultEMail(AMember.Name);
      ATeam.Members.Add(AMember);
    end;
    EMFSession.Save(ATeam);
    ShowText('A TTeam entity object was created and populated with ten new TMember entity objects.');

    EMFSession.FlushChanges;
    ShowText('The session component applied the changes to the database and finished tracking entity object changes.');

    for AMember in ATeam.Members do
      EMFSession.Reload(AMember);
    EMFSession.Reload(ATeam);
    ShowText(Format('The database now contains a new TTeam entity object (Team ID: %d) with the following TMember entity objects:', [ATeam.ID]));
    ShowTeamMembers(ATeam.Members, True);
  except
    EMFSession.DropChanges;
    ShowText('An error occurred. No changes were applied to the database.');
    ShowText('The session component finished tracking entity object changes.');
  end;
end;
//>create objects one-to-many relationship

initialization
  TfrmEMFCreateEntityOneToMany.Register;

end.
