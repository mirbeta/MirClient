unit EMFDeleteEntityOneToMany;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer, cxSplitter,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFDeleteEntityOneToMany }

  TfrmEMFDeleteEntityOneToMany = class(TfrmEMFCustomTutorial)
    btnDeleteCascade: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    procedure btnDeleteCascadeClick(Sender: TObject);
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
  uMain, dxCore, dxEMF.Core, dxEMF.Utils, dxEMF.Types, EMFDemoClasses;

{ TfrmEMFDeleteEntityOneToMany }

class function TfrmEMFDeleteEntityOneToMany.GetCaption: string;
begin
  Result := 'Deleting Entity Objects in a One-to-Many Relationship';
end;

function TfrmEMFDeleteEntityOneToMany.GetDescription: string;
begin
  Result := 'This tutorial shows how the cascade delete operation works for entities in a ' +
    'One-to-Many relationship. A TTeam entity at the association''s "one" end relates to ' +
    'TMember entities at the "many" end. Marking the Members collection property''s field' +
    ' with the Aggregated attribute (in addition to the Association attribute) in the ' +
    'TTeam entity enables the field''s cascade option.';
end;

class function TfrmEMFDeleteEntityOneToMany.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityDeletingGroupID;
end;

class function TfrmEMFDeleteEntityOneToMany.GetID: Integer;
begin
  Result := 11;
end;

procedure TfrmEMFDeleteEntityOneToMany.GenerateContent;
begin
  inherited GenerateContent;
  GenerateTeams;
end;

procedure TfrmEMFDeleteEntityOneToMany.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnDeleteCascade, 'This code retrieves a TTeam entity object, stores this and associated TMember entity objects'' identity values, and calls the session component''s Delete method to apply the changes to the database. ' + 'Then, the database is queried for objects using the stored identity values.', ['delete objects in one-to-many relationship', 'TTeam|TTeam', 'TMember|TMember']);
end;

procedure TfrmEMFDeleteEntityOneToMany.UnregisterActions;
begin
  UnregisterTutorialAction(btnDeleteCascade);
  inherited UnregisterActions;
end;

//<delete objects in one-to-many relationship
procedure TfrmEMFDeleteEntityOneToMany.btnDeleteCascadeClick(Sender: TObject);
var
  I, AID, AMemberCount: Integer;
  AMemberIDs: TArray<Integer>;
  ATeam: TTeam;
  ATeams: IdxEMFCollection<TTeam>;
begin
  GenerateTeams;//<tutorial
  ATeams := EMFSession.GetObjects<TTeam>;
  ShowText('The database contains the following TTeam and TMember entity objects:', True);
  ShowTeams(ATeams);

  ATeam := ATeams.First;
  ShowText(dxCRLF + Format('The first team (Team ID: %d) and its members are going to be deleted in the database...', [ATeam.ID]));
  AMemberCount := ATeam.Members.Count;
  SetLength(AMemberIDs, AMemberCount);
  for I := 0 to AMemberCount - 1 do
    AMemberIDs[I] := ATeam.Members.Items[I].ID;

  AID := ATeam.ID;
  EMFSession.Delete(ATeam);

  ShowLocateEntityText(TTeam, AID);
  ShowTeam(EMFSession.Find<TTeam>(AID));

  for AID in AMemberIDs do
  begin
    ShowLocateEntityText(TMember, AID);
    ShowTeamMember(EMFSession.Find<TMember>(AID), True);
  end;
end;
//>delete objects in one-to-many relationship

initialization
  TfrmEMFDeleteEntityOneToMany.Register;

end.
