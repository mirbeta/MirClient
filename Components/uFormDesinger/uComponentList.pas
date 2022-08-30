unit uComponentList;

interface
  uses Classes, Sysutils, Windows, StdCtrls, Controls, uDesignIntf, uDesigner, Forms;

  type
  TuCustomComponentList  = class(TCustomComboBox, IComponentsMonitor)
  private
    FDesigner: IDesigner;
    FChanged: boolean;
    procedure SetDesigner(const Value: IDesigner);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Select; override;
    {IComponentsMonitor}
    procedure AddComponent(Component: TComponent);
    procedure DeleteComponent(Component: TComponent);
    procedure ActiveComponent(Component: TComponent);

    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Designer: IDesigner read FDesigner write SetDesigner;
  public
    procedure ClearComponents;
    procedure LoadComponents;
  end;

  TuComponentList  = class(TuCustomComponentList)
  published
    property Designer;

    property Align;
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
//    property OnChange;
//    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation
  uses Graphics;

{ TuCustomComponentList }

procedure TuCustomComponentList.ActiveComponent(Component: TComponent);
var
  I: Integer;
begin
  if Component=nil then
  begin
    FChanged  :=  True;
    ItemIndex :=  -1;
    FChanged  :=  False;
    Exit;
  end;

  for I := 0 to Items.Count - 1 do
    if Component=Items.Objects[I] then
    begin
      FChanged  :=  True;
      ItemIndex :=  I;
      FChanged  :=  False;
      Exit;
    end;
end;

procedure TuCustomComponentList.AddComponent(Component: TComponent);
begin
  Items.AddObject(Component.Name, Component);
end;

procedure TuCustomComponentList.ClearComponents;
begin
  Clear;
end;

constructor TuCustomComponentList.Create(AOwner: TComponent);
begin
  inherited;
  Style :=  csOwnerDrawVariable;
end;

procedure TuCustomComponentList.DeleteComponent(Component: TComponent);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Component=Items.Objects[I] then
    begin
      Items.Delete(I);
      Break;
    end;
end;

destructor TuCustomComponentList.Destroy;
begin
  if FDesigner<>nil then
    TCustomDesigner(FDesigner).UnRegisterComponentsMonitor(Self);
  inherited;
end;

procedure TuCustomComponentList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  _name, _cls: String;
  nameW: Integer;
  ActiveItem: boolean;
begin
  if Index=-1 then Exit;
  Canvas.FillRect(Rect);
  ActiveItem  :=  ItemIndex = Index;
  _name       :=  TComponent(Items.Objects[Index]).Name;
  _cls        :=  TComponent(Items.Objects[Index]).ClassName;
  Canvas.Font.Style :=  [fsBold];
  nameW :=  Canvas.TextWidth(_name);
  Canvas.TextOut(Rect.Left+2, Rect.Top, _name);
  if not ActiveItem then
    Canvas.Font.Color :=  clGray;
  Canvas.Font.Style :=  [];
  Canvas.TextOut(Rect.Left+12+nameW, Rect.Top, _cls);
end;

procedure TuCustomComponentList.LoadComponents;
var
  I: Integer;
  hook: IDesignerHook;
begin
  if FDesigner<>nil then
    with TCustomDesigner(FDesigner) do
      if Supports(FDesigner, IDesignerHook, hook) and (hook.GetRoot<>nil) then
      begin
        AddComponent(hook.GetRoot);
        for I := 0 to hook.GetRoot.ComponentCount - 1 do
          AddComponent(hook.GetRoot.Components[I]);
      end;
end;

procedure TuCustomComponentList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and AComponent.IsImplementorOf(FDesigner) then
  begin
    Clear;
    FDesigner  :=  nil;
  end;
end;

procedure TuCustomComponentList.Select;
begin
  if not FChanged and (FDesigner<>nil) then
    FDesigner.SelectComponent(PersistentToDesignObject(TComponent(Items.Objects[ItemIndex])));
end;

procedure TuCustomComponentList.SetDesigner(const Value: IDesigner);
begin
  if Value<>Self.FDesigner then
  begin
    ReferenceInterface(FDesigner, opRemove);
    FDesigner := Value;
    Clear;
    if FDesigner<>nil then
    begin
      LoadComponents;
      TCustomDesigner(FDesigner).RegisterComponentsMonitor(Self);
    end;
    ReferenceInterface(FDesigner, opInsert);;
  end;
end;

end.
