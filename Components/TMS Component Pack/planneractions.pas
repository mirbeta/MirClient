unit PlannerActions;

interface

uses
  Classes, Planner, PlannerMonthView, ActnList, Clipbrd, Windows, Controls;

type
  TPlannerAction = class(TAction)
  private
    FControl: TCustomControl;
    procedure SetControl(Value: TCustomControl);
  protected
    function GetControl(Target: TObject): TCustomControl; virtual;
    function GetControlItems(Target: TObject): TPlannerItems; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    property Control: TCustomControl read FControl write SetControl;

  end;

  TPlannerCut = class(TPlannerAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPlannerCopy = class(TPlannerAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPlannerPaste = class(TPlannerAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPlannerDelete = class(TPlannerAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPlannerInsert = class(TPlannerAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPlannerEdit = class(TPlannerAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;


implementation

{ TPlannerAction }

destructor TPlannerAction.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);
  inherited;
end;

function TPlannerAction.GetControl(Target: TObject): TCustomControl;
begin
{ We could hard cast Target as a TCustomEdit since HandlesTarget "should" be
called before ExecuteTarget and UpdateTarget, however, we're being safe. }
  Result := Target as TCustomControl;
end;

function TPlannerAction.HandlesTarget(Target: TObject): Boolean;
var
  res: Boolean;
begin
  Result := false;

  res := ((Control <> nil) and (Target = Control) or (Control = nil) and
          ((Target is TCustomPlanner) or (Target is TPlannerMonthView)));

  if res then
  begin
    if (Target is TCustomPlanner) then
      Result := (TCustomPlanner(Target).GridControl.Focused or TCustomPlanner(Target).Focused);
    if (Target is TPlannerMonthView) then
      Result := TPlannerMonthView(Target).Focused;
  end;
end;

function TPlannerAction.GetControlItems(Target: TObject): TPlannerItems;
begin
  Result := nil;

  if (Target is TCustomPlanner) then
    Result := TCustomPlanner(Target).Items;
    
  if (Target is TPlannerMonthView) then
    Result := TPlannerMonthView(Target).Items;
    
end;

procedure TPlannerAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TPlannerAction.UpdateTarget(Target: TObject);
begin
  if (Self is TPlannerCut) or (Self is TPlannerCopy) or (Self is TPlannerDelete) or (Self is TPlannerEdit) then
    Enabled := Assigned(GetControlItems(Target).Selected);
end;

procedure TPlannerAction.SetControl(Value: TCustomControl);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

{ TPlannerCopy }

procedure TPlannerCopy.ExecuteTarget(Target: TObject);
begin
  GetControlItems(Target).CopyToClipboard;
end;

{ TPlannerCut }

procedure TPlannerCut.ExecuteTarget(Target: TObject);
begin
  GetControlItems(Target).CutToClipboard;
end;

{ TPlannerPaste }

procedure TPlannerPaste.ExecuteTarget(Target: TObject);
begin
  GetControlItems(Target).PasteFromClipboardAtPos;
end;

procedure TPlannerPaste.UpdateTarget(Target: TObject);
begin
  Enabled := Clipboard.HasFormat(CF_PLANNERITEM);
end;

{ TPlannerDelete }

constructor TPlannerDelete.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := VK_DELETE;
end;

procedure TPlannerDelete.ExecuteTarget(Target: TObject);
begin
  if (Target is TCustomPlanner) then
    TCustomPlanner(Target).FreeItem(GetControlItems(Target).Selected);

  if (Target is TPlannerMonthView) then
    TPlannerMonthView(Target).FreeItem(GetControlItems(Target).Selected);
end;

{ TPlannerInsert }

constructor TPlannerInsert.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := VK_INSERT;
end;

procedure TPlannerInsert.ExecuteTarget(Target: TObject);
begin
  if (Target is TCustomPlanner) then
    TCustomPlanner(Target).CreateItemAtSelection;

  if (Target is TPlannerMonthView) then
    TPlannerMonthView(Target).CreateItemAtSelection;
end;

{ TPlannerEdit }

procedure TPlannerEdit.ExecuteTarget(Target: TObject);
begin
  GetControlItems(Target).Selected.Edit;
end;

end.
 
