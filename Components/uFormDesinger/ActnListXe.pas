{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit ActnListXE;

{$T-,H+,X+}

interface

uses
{$IF DEFINED(CLR)}
  System.ComponentModel.Design.Serialization,
{$IFEND}
  System.Classes, Messages, ImgList, Contnrs;

type

{ TContainedAction }

  TCustomActionList = class;

  TContainedAction = class(TBasicAction)
  private
    FCategory: string;
    FActionList: TCustomActionList;
    function GetIndex: Integer;
    function IsCategoryStored: Boolean;
    procedure SetCategory(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetActionList(AActionList: TCustomActionList);
{$IF DEFINED(CLR)}
    class constructor Create;
{$IFEND}
  protected
    procedure ReadState(Reader: TReader); override;
{$IF DEFINED(CLR)}
    procedure Change; override;
{$IFEND}
  public
    destructor Destroy; override;
    function Execute: Boolean; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetParentComponent(AParent: TComponent); override;
    function Update: Boolean; override;
    property ActionList: TCustomActionList read FActionList write SetActionList;
    property Index: Integer read GetIndex write SetIndex stored False;
  published
    property Category: string read FCategory write SetCategory stored IsCategoryStored;
  end;

  TContainedActionClass = class of TContainedAction;

{ TActionListEnumerator }

  TActionListEnumerator = class
  private
    FIndex: Integer;
    FActionList: TCustomActionList;
  public
    constructor Create(AActionList: TCustomActionList);
    function GetCurrent: TContainedAction;
    function MoveNext: Boolean;
    property Current: TContainedAction read GetCurrent;
  end;

{ TCustomActionList }

  TActionEvent = procedure (Action: TBasicAction; var Handled: Boolean) of object;
  TActionListState = (asNormal, asSuspended, asSuspendedEnabled);

  [RootDesignerSerializerAttribute('', '', False)]
  TCustomActionList = class(TComponent)
  private
    FActions: TList;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FOnChange: TNotifyEvent;
    FOnExecute: TActionEvent;
    FOnUpdate: TActionEvent;
    FState: TActionListState;
    FOnStateChange: TNotifyEvent;
    function GetAction(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    procedure SetAction(Index: Integer; Value: TContainedAction);
    procedure ImageListChange(Sender: TObject);
{$IF DEFINED(CLR)}
    class constructor Create;
{$IFEND}
  protected
    procedure AddAction(Action: TContainedAction);
    procedure RemoveAction(Action: TContainedAction);
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetImages(Value: TCustomImageList); virtual;
    procedure SetState(const Value: TActionListState); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExecute: TActionEvent read FOnExecute write FOnExecute;
    property OnUpdate: TActionEvent read FOnUpdate write FOnUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetEnumerator: TActionListEnumerator;
    function IsShortCut(var Message: TWMKey): Boolean;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Actions[Index: Integer]: TContainedAction read GetAction write SetAction; default;
    property ActionCount: Integer read GetActionCount;
    property Images: TCustomImageList read FImages write SetImages;
    property State: TActionListState read FState write SetState default asNormal;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

{ TActionList }

  TActionList = class(TCustomActionList)
  published
    property Images;
    property State;
    property OnChange;
    property OnExecute;
    property OnStateChange;
    property OnUpdate;
  end;

{ TShortCutList }

  TShortCutList = class(TStringList)
  private
    function GetShortCuts(Index: Integer): TShortCut;
  public
    function Add(const S: String): Integer; override;
    function IndexOfShortCut(const Shortcut: TShortCut): Integer;
    property ShortCuts[Index: Integer]: TShortCut read GetShortCuts;
  end;

{ TCustomAction }

  THintEvent = procedure (var HintStr: string; var CanShow: Boolean) of object;

  TCustomAction = class(TContainedAction)
  private
    FDisableIfNoHandler: Boolean;
    FCaption: string;
    FChecking: Boolean;
    FChecked: Boolean;
    FEnabled: Boolean;
    FGroupIndex: Integer;
    FHelpType: THelpType;
    FHelpContext: THelpContext;
    FHelpKeyword: string;
    FHint: string;
    FImageIndex: TImageIndex;
    FShortCut: TShortCut;
    FVisible: Boolean;
    FOnHint: THintEvent;
    FSecondaryShortCuts: TShortCutList;
    FSavedEnabledState: Boolean;
    FAutoCheck: Boolean;
    procedure SetAutoCheck(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetHelpType(Value: THelpType);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetShortCut(Value: TShortCut);
    procedure SetVisible(Value: Boolean);
    function GetSecondaryShortCuts: TShortCutList;
    procedure SetSecondaryShortCuts(const Value: TShortCutList);
    function IsSecondaryShortCutsStored: Boolean;
  protected
    FImage: TObject;
    FMask: TObject;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    function HandleShortCut: Boolean; virtual;
    property SavedEnabledState: Boolean read FSavedEnabledState write FSavedEnabledState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DoHint(var HintStr: string): Boolean; dynamic;
    function Execute: Boolean; override;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default False;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property DisableIfNoHandler: Boolean read FDisableIfNoHandler write FDisableIfNoHandler default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property HelpKeyword: string read FHelpKeyword write SetHelpKeyword;
    property HelpType: THelpType read FHelpType write SetHelpType default htKeyword;
    property Hint: string read FHint write SetHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    property SecondaryShortCuts: TShortCutList read GetSecondaryShortCuts
      write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnHint: THintEvent read FOnHint write FOnHint;
    { Property access for design time support in .NET }
{$IF DEFINED(CLR)}
    property Image: TObject read FImage write FImage;
    property Mask: TObject read FMask write FMask;
{$IFEND}
  end;

  TAction = class(TCustomAction)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

{ TActionLink }

  TActionLink = class(TBasicActionLink)
  protected
    function IsCaptionLinked: Boolean; virtual;
    function IsCheckedLinked: Boolean; virtual;
    function IsEnabledLinked: Boolean; virtual;
    function IsGroupIndexLinked: Boolean; virtual;
    function IsHelpContextLinked: Boolean; virtual;
    function IsHelpLinked: Boolean; virtual;
    function IsHintLinked: Boolean; virtual;
    function IsImageIndexLinked: Boolean; virtual;
    function IsShortCutLinked: Boolean; virtual;
    function IsVisibleLinked: Boolean; virtual;
    procedure SetAutoCheck(Value: Boolean); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetGroupIndex(Value: Integer); virtual;
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    procedure SetHelpType(Value: THelpType); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetImageIndex(Value: Integer); virtual;
    procedure SetShortCut(Value: TShortCut); virtual;
    procedure SetVisible(Value: Boolean); virtual;
  end;

  TActionLinkClass = class of TActionLink;

{ Action registration }

{$IF DEFINED(CLR)}
  TEnumActionProcInfo = TObject;
{$ELSE}
  TEnumActionProcInfo = Pointer;
{$IFEND}

  TEnumActionProc = procedure (const Category: string; ActionClass: TBasicActionClass;
    Info: TEnumActionProcInfo) of object;

procedure RegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction;
procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: TEnumActionProcInfo);



{$IF DEFINED(CLR)}
var
  RegisterActionsProc: procedure (const CategoryName: string;
    const AClasses: array of TBasicActionClass; Resource: TComponentClass);
  UnRegisterActionsProc: procedure (const AClasses: array of TBasicActionClass);
  EnumRegisteredActionsProc: procedure (Proc: TEnumActionProc; Info: TObject);
  CreateActionProc: function (AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction;
{$ELSE}
const
  RegisterActionsProc: procedure (const CategoryName: string;
    const AClasses: array of TBasicActionClass; Resource: TComponentClass) = nil;
  UnRegisterActionsProc: procedure (const AClasses: array of TBasicActionClass) = nil;
  EnumRegisteredActionsProc: procedure (Proc: TEnumActionProc; Info: Pointer) = nil;
  CreateActionProc: function (AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction = nil;
{$IFEND}

implementation

uses
{$IF DEFINED(CLR)}
  System.Runtime.InteropServices, System.Security.Permissions,
{$IFEND}
  SysUtils, Windows, Forms, Menus, Consts, Graphics, Controls;


resourcestring
  SInvalidActionRegistration = 'Invalid action registration';
  SInvalidActionUnregistration = 'Invalid action unregistration';
  SInvalidActionEnumeration = 'Invalid action enumeration';
  SInvalidActionCreation = 'Invalid action creation';



procedure RegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
begin
  if Assigned(RegisterActionsProc) then
    RegisterActionsProc(CategoryName, AClasses, Resource) else
    raise Exception.CreateRes({$IFNDEF CLR}@{$ENDIF}SInvalidActionRegistration);
end;

procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
begin
  if Assigned(UnRegisterActionsProc) then
    UnRegisterActionsProc(AClasses) else
    raise Exception.CreateRes({$IFNDEF CLR}@{$ENDIF}SInvalidActionUnregistration);
end;

procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: TEnumActionProcInfo);
begin
  if Assigned(EnumRegisteredActionsProc) then
    EnumRegisteredActionsProc(Proc, Info) else
    raise Exception.CreateRes({$IFNDEF CLR}@{$ENDIF}SInvalidActionEnumeration);
end;

function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction;
begin
  if Assigned(CreateActionProc) then
    Result := CreateActionProc(AOwner, ActionClass) else
    raise Exception.CreateRes({$IFNDEF CLR}@{$ENDIF}SInvalidActionCreation);
end;

{ TContainedAction }

{$IF DEFINED(CLR)}
class constructor TContainedAction.Create;
begin
  GroupDescendentsWith(TContainedAction, TControl);
end;
{$IFEND}

destructor TContainedAction.Destroy;
begin
  if ActionList <> nil then ActionList.RemoveAction(Self);
  inherited Destroy;
end;

function TContainedAction.GetIndex: Integer;
begin
  if ActionList <> nil then
    Result := ActionList.FActions.IndexOf(Self) else
    Result := -1;
end;

function TContainedAction.IsCategoryStored: Boolean;
begin
  Result := True;//GetParentComponent <> ActionList;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TContainedAction.GetParentComponent: TComponent;
begin
  if ActionList <> nil then
    Result := ActionList else
    Result := inherited GetParentComponent;
end;

function TContainedAction.HasParent: Boolean;
begin
  if ActionList <> nil then
    Result := True else
    Result := inherited HasParent;
end;

{$IF DEFINED(CLR)}
procedure TContainedAction.Change;
begin
  inherited Change;
end;
{$IFEND}

procedure TContainedAction.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TCustomActionList then
    ActionList := TCustomActionList(Reader.Parent);
end;

procedure TContainedAction.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := ActionList.FActions.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      ActionList.FActions.Delete(CurIndex);
      ActionList.FActions.Insert(Value, Self);
    end;
  end;
end;

procedure TContainedAction.SetCategory(const Value: string);
begin
  if Value <> Category then
  begin
    FCategory := Value;
    if ActionList <> nil then
      ActionList.Change;
  end;
end;

procedure TContainedAction.SetActionList(AActionList: TCustomActionList);
begin
  if AActionList <> ActionList then
  begin
    if ActionList <> nil then ActionList.RemoveAction(Self);
    if AActionList <> nil then AActionList.AddAction(Self);
  end;
end;

procedure TContainedAction.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) and (AParent is TCustomActionList) then
    ActionList := TCustomActionList(AParent);
end;

function TContainedAction.Execute: Boolean;
begin
{$IF DEFINED(CLR)}
  Result := (ActionList <> nil) and ActionList.ExecuteAction(Self) or
    Application.ExecuteAction(Self) or inherited Execute;
  if not Result then
    if Assigned(Application) then
      Result := Application.DispatchAction(True, self, False);
{$ELSE}
  Result := (ActionList <> nil) and ActionList.ExecuteAction(Self) or
    Application.ExecuteAction(Self) or inherited Execute or
    (SendAppMessage(CM_ACTIONEXECUTE, 0, Longint(Self)) = 1);
{$IFEND}
end;

function TContainedAction.Update: Boolean;
begin
{$IF DEFINED(CLR)}
  Result := (ActionList <> nil) and ActionList.UpdateAction(Self) or
    Application.UpdateAction(Self) or inherited Update;
   if not Result then
     if Assigned(Application) then
       Result := Application.DispatchAction(False, self, False);
{$ELSE}
  Result := (ActionList <> nil) and ActionList.UpdateAction(Self) or
    Application.UpdateAction(Self) or inherited Update or
    (SendAppMessage(CM_ACTIONUPDATE, 0, Longint(Self)) = 1);
{$IFEND}
end;

{ TActionListEnumerator }

constructor TActionListEnumerator.Create(AActionList: TCustomActionList);
begin
  inherited Create;
  FIndex := -1;
  FActionList := AActionList;
end;

function TActionListEnumerator.GetCurrent: TContainedAction;
begin
  Result := FActionList[FIndex];
end;

function TActionListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FActionList.ActionCount - 1;
  if Result then
    Inc(FIndex);
end;

{ TCustomActionList }

{$IF DEFINED(CLR)}
class constructor TCustomActionList.Create;
begin
  GroupDescendentsWith(TCustomActionList, TControl);
end;
{$IFEND}

constructor TCustomActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActions := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FState := asNormal;
end;

destructor TCustomActionList.Destroy;
begin
  FImageChangeLink.Free;
  while FActions.Count > 0 do TContainedAction(FActions.Last).Free;
  FreeAndNil(FActions);
  inherited Destroy;
end;

procedure TCustomActionList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Action: TCustomAction;
begin
  for I := 0 to FActions.Count - 1 do
  begin
    Action := TCustomAction(FActions.List[I]);
    if Action.Owner = Root then Proc(Action);
  end;
end;

procedure TCustomActionList.SetChildOrder(Component: TComponent; Order: Integer);
begin
  if FActions.IndexOf(Component) >= 0 then
    (Component as TContainedAction).Index := Order;
end;

function TCustomActionList.GetAction(Index: Integer): TContainedAction;
begin
  Result := TContainedAction(FActions[Index]);
end;

function TCustomActionList.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

function TCustomActionList.GetEnumerator: TActionListEnumerator;
begin
  Result := TActionListEnumerator.Create(Self);
end;

procedure TCustomActionList.SetAction(Index: Integer; Value: TContainedAction);
begin
  TContainedAction(FActions[Index]).Assign(Value);
end;

procedure TCustomActionList.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
end;

procedure TCustomActionList.ImageListChange(Sender: TObject);
begin
  if Sender = Images then Change;
end;

procedure TCustomActionList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
    else if (FActions <> nil) and (AComponent is TContainedAction) then
      RemoveAction(TContainedAction(AComponent));
end;

procedure TCustomActionList.AddAction(Action: TContainedAction);
begin
  FActions.Add(Action);
  Action.FActionList := Self;
  Action.FreeNotification(Self);
end;

procedure TCustomActionList.RemoveAction(Action: TContainedAction);
begin
  if FActions.Remove(Action) >= 0 then
  begin
    Action.RemoveFreeNotification(Self);
    Action.FActionList := nil;
  end;
end;

procedure TCustomActionList.Change;
var
  I: Integer;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  for I := 0 to FActions.Count - 1 do
    TContainedAction(FActions.List[I]).Change;
  if csDesigning in ComponentState then
  begin
    if (Owner is TForm) and (TForm(Owner).Designer <> nil) then
      TForm(Owner).Designer.Modified;
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TCustomActionList.IsShortCut(var Message: TWMKey): Boolean;
var
  I: Integer;
  ShortCut: TShortCut;
  ShiftState: TShiftState;
  Action: TCustomAction;
begin
{$IF NOT DEFINED(CLR)}
  Result := False;
  if Menus.IsAltGRPressed then Exit;
{$IFEND}
  ShiftState := KeyDataToShiftState(Message.KeyData);
  ShortCut := Menus.ShortCut(Message.CharCode, ShiftState);
  if ShortCut <> scNone then
    for I := 0 to FActions.Count - 1 do
    begin
      Action := TCustomAction(FActions.List[I]);
      if (TObject(Action) is TCustomAction) then
        if (Action.ShortCut = ShortCut) or (Assigned(Action.FSecondaryShortCuts) and
           (Action.SecondaryShortCuts.IndexOfShortCut(ShortCut) <> -1)) then
        begin
          Result := Action.HandleShortCut;
          Exit;
        end;
    end;
{$IF DEFINED(CLR)}
  Result := False;
{$IFEND}
end;

function TCustomActionList.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnExecute) then FOnExecute(Action, Result);
end;

function TCustomActionList.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnUpdate) then FOnUpdate(Action, Result);
end;

procedure TCustomActionList.SetState(const Value: TActionListState);
var
  I: Integer;
  Action: TCustomAction;
  OldState: TActionListState;
begin
  if FState <> Value then
  begin
    OldState := FState;
    FState := Value;
    if State = asSuspended then exit;
    for I := 0 to FActions.Count - 1 do
    begin
      Action := TCustomAction(FActions.List[I]);
      case Value of
        asNormal:
          begin
            if Action is TCustomAction then
              if OldState = asSuspendedEnabled then
                with Action as TCustomAction do
                  Enabled := SavedEnabledState;
            Action.Update;
          end;
        asSuspendedEnabled:
          if Action is TCustomAction then
            if Value = asSuspendedEnabled then
              with Action as TCustomAction do
              begin
                SavedEnabledState := Enabled;
                Enabled := True;
              end;
      end;
    end;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self);
  end;
end;

{ TActionLink }

function TActionLink.IsCaptionLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsCheckedLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsEnabledLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHelpContextLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHelpLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHintLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsImageIndexLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsShortCutLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsVisibleLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

procedure TActionLink.SetAutoCheck(Value: Boolean);
begin
end;

procedure TActionLink.SetCaption(const Value: string);
begin
end;

procedure TActionLink.SetChecked(Value: Boolean);
begin
end;

procedure TActionLink.SetEnabled(Value: Boolean);
begin
end;

procedure TActionLink.SetGroupIndex(Value: Integer);
begin
end;

procedure TActionLink.SetHelpContext(Value: THelpContext);
begin
end;

procedure TActionLink.SetHelpKeyword(const Value: string);
begin
end;

procedure TActionLink.SetHelpType(Value: THelpType);
begin
end;

procedure TActionLink.SetHint(const Value: string);
begin
end;

procedure TActionLink.SetImageIndex(Value: Integer);
begin
end;

procedure TActionLink.SetShortCut(Value: TShortCut);
begin
end;

procedure TActionLink.SetVisible(Value: Boolean);
begin
end;

{ TCustomAction }

constructor TCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FImageIndex := -1;
  FVisible := True;
  FSecondaryShortCuts := nil;
end;

destructor TCustomAction.Destroy;
begin
  FImage.Free;
  FMask.Free;
  if Assigned(FSecondaryShortCuts) then
    FreeAndNil(FSecondaryShortCuts);
  inherited Destroy;
end;

procedure TCustomAction.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      AutoCheck := Self.AutoCheck;
      Caption := Self.Caption;
      Checked := Self.Checked;
      Enabled := Self.Enabled;
      GroupIndex := Self.GroupIndex;
      HelpContext := Self.HelpContext;
      HelpKeyword := Self.HelpKeyword;
      HelpType := Self.HelpType;
      Hint := Self.Hint;
      ImageIndex := Self.ImageIndex;
      SecondaryShortCuts := Self.SecondaryShortCuts;
      ShortCut := Self.ShortCut;
      Visible := Self.Visible;
      Tag := Self.Tag;
      OnExecute := Self.OnExecute;
      OnHint := Self.OnHint;
      OnUpdate := Self.OnUpdate;
      OnChange := Self.OnChange;
    end
  else
    inherited AssignTo(Dest);
end;

procedure TCustomAction.SetAutoCheck(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FAutoCheck then
  begin
    for I := 0 to ClientCount - 1 do
      if TBasicActionLink(Clients[I]) is TActionLink then
        TActionLink(Clients[I]).SetAutoCheck(Value);
    FAutoCheck := Value;
    Change;
  end;
end;

procedure TCustomAction.SetCaption(const Value: string);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FCaption then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetCaption(Value);
    end;
    FCaption := Value;
    Change;
  end;
end;

procedure TCustomAction.SetChecked(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
  Action: TContainedAction;
begin
  if FChecking then exit;
  FChecking := True;
  try
    if Value <> FChecked then
    begin
      for I := 0 to ClientCount - 1 do
      begin
        Link := TObject(Clients[I]) as TActionLink;
        if Assigned(Link) then
          Link.SetChecked(Value);
      end;
      FChecked := Value;
      if (FGroupIndex > 0) and FChecked then
        for I := 0 to ActionList.ActionCount - 1 do
        begin
          Action := ActionList.Actions[I];
          if (Action <> Self) and
             (TObject(Action) is TCustomAction) and
             (TCustomAction(Action).FGroupIndex = FGroupIndex) then
            TCustomAction(Action).Checked := False;
        end;
      Change;
    end;
  finally
    FChecking := False;
  end;
end;

procedure TCustomAction.SetEnabled(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FEnabled then
  begin
    if Assigned(ActionList) then
      if ActionList.State = asSuspended then
      begin
        FEnabled := Value;
        Exit;
      end
      else
        if (ActionList.State = asSuspendedEnabled) then
          Value := True;
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        TActionLink(Link).SetEnabled(Value);
    end;
    FEnabled := Value;
    Change;
  end;
end;

procedure TCustomAction.SetGroupIndex(const Value: Integer);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FGroupIndex then
  begin
    FGroupIndex := Value;
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetGroupIndex(Value);
    end;
    Change;
  end;
end;

procedure TCustomAction.SetHelpType(Value: THelpType);
var
  I: Integer;
begin
  if Value <> FHelpType then
  begin
    for I := 0 to ClientCount -1 do
     if TBasicActionLink(Clients[I]) is TActionLink then
       TActionLink(Clients[I]).SetHelpType(Value);
    FHelpType := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHelpKeyword(const Value: string);
var
  I: Integer;
begin
  if Value <> FHelpKeyword then
  begin
    for I := 0 to ClientCount -1 do
     if TBasicActionLink(Clients[I]) is TActionLink then
       TActionLink(Clients[I]).SetHelpKeyword(Value);
    FHelpKeyword := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHelpContext(Value: THelpContext);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FHelpContext then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetHelpContext(Value);
    end;
    FHelpContext := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHint(const Value: string);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FHint then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetHint(Value);
    end;
    FHint := Value;
    Change;
  end;
end;

procedure TCustomAction.SetImageIndex(Value: TImageIndex);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FImageIndex then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetImageIndex(Value);
    end;
    FImageIndex := Value;
    Change;
  end;
end;

procedure TCustomAction.SetShortCut(Value: TShortCut);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FShortCut then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetShortCut(Value);
    end;
    FShortCut := Value;
    Change;
  end;
end;

procedure TCustomAction.SetVisible(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FVisible then
  begin
    for I := 0 to ClientCount - 1 do
    begin
      Link := TObject(Clients[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetVisible(Value);
    end;
    FVisible := Value;
    Change;
  end;
end;

procedure TCustomAction.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (Name = Caption) and ((Owner = nil) or
    not (csLoading in Owner.ComponentState));
  inherited SetName(Value);
  { Don't update caption to name if we've got clients connected. }
  if ChangeText and (ClientCount = 0) then Caption := Value;
end;

function TCustomAction.DoHint(var HintStr: string): Boolean;
begin
  Result := True;
  if Assigned(FOnHint) then FOnHint(HintStr, Result);
end;

function TCustomAction.Execute: Boolean;
begin
  Result := False;
  if Assigned(ActionList) and (ActionList.State <> asNormal) then Exit;
  Update;
  if Enabled and FAutoCheck then
    if not Checked or Checked and (GroupIndex = 0) then
      Checked := not Checked;
  Result := Enabled and inherited Execute;
end;

function TCustomAction.GetSecondaryShortCuts: TShortCutList;
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  Result := FSecondaryShortCuts;
end;

procedure TCustomAction.SetSecondaryShortCuts(const Value: TShortCutList);
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  FSecondaryShortCuts.Assign(Value);
end;

function TCustomAction.IsSecondaryShortCutsStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
end;

function TCustomAction.HandleShortCut: Boolean;
begin
  Result := Execute;
end;

{ TShortCutList }

function TShortCutList.Add(const S: String): Integer;
begin
  Result := inherited Add(S);
  Objects[Result] := TObject(TextToShortCut(S));
end;

function TShortCutList.GetShortCuts(Index: Integer): TShortCut;
begin
  Result := TShortCut(Objects[Index]);
end;

{ TAction }

constructor TAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := True;
end;

function TShortCutList.IndexOfShortCut(const Shortcut: TShortCut): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if TShortCut(Objects[I]) = ShortCut then
    begin
      Result := I;
      break;
    end;
end;

{$IF NOT DEFINED(CLR)}
initialization
  GroupDescendentsWith(TCustomActionList, TControl);
  GroupDescendentsWith(TContainedAction, TControl);
{$IFEND}

end.
