{*************************************************************************}
{ TFullDropDownItem Class                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPFullDropDownItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Forms, Graphics,
  GDIPCustomItem, GDIPTextItem, GDIPBase, AdvPolyList, CustomItemsContainer, AdvGDIP,
  ExtCtrls, SysUtils, GDIPFill, Controls,
  AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TFormPosition = (fpLeft, fpTop, fpRight, fpBottom);

  TFormPositionSet = set of TFormPosition;

  TFullDropDownItem = class;

  TDropDownForm = class(TForm)
  private
    FContainer: TAdvPolyList;
    FPosition: TFormPositionSet;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
  public
    procedure Init;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Container: TAdvPolyList read FContainer write FContainer;
    property Position: TFormPositionSet read FPosition write FPosition;
  end;

  TDropDownAppearance = class(TPersistent)
  private
    FOwner: TAdvPolyList;
    FDropDownWidth: Integer;
    FDropDownHeight: Integer;
    function GetFill: TGDIPFill;
    function GetListMargins: TMargins;
    function GetReadOnly: Boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetListMargins(const Value: TMargins);
    procedure SetReadOnly(const Value: Boolean);
    function GetHorizontalSpacing: Integer;
    function GetVerticalSpacing: Integer;
    procedure SetHorizontalSpacing(const Value: Integer);
    procedure SetVerticalSpacing(const Value: Integer);
    function GetAutoSizeMode: TAutoSizeMode;
    function GetAutoSizeType: TAutoSizeType;
    function GetBorderMode: TListBorderMode;
    function GetBorderTypes: TListBorderTypes;
    function GetColumns: Integer;
    function GetRows: Integer;
    function GetShowFocus: Boolean;
    procedure SetAutoSizeMode(const Value: TAutoSizeMode);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetBorderMode(const Value: TListBorderMode);
    procedure SetBorderTypes(const Value: TListBorderTypes);
    procedure SetColumns(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetDropDownWidth(const Value: Integer);
    procedure SetDropDownHeight(const Value: Integer);
  public
    constructor Create(AOwner: TAdvPolyList);
    destructor Destroy; override;
  published
    property AutoSizeMode: TAutoSizeMode read GetAutoSizeMode write SetAutoSizeMode default asmItems;
    property AutoSizeType: TAutoSizeType read GetAutoSizeType write SetAutoSizeType default astWidth;
    property Fill: TGDIPFill read GetFill write SetFill;
    property HorizontalSpacing: Integer read GetHorizontalSpacing write SetHorizontalSpacing default 5;
    property VerticalSpacing: Integer read GetVerticalSpacing write SetVerticalSpacing default 5;
    property ListMargins: TMargins read GetListMargins write SetListMargins;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
    property Columns: Integer read GetColumns write SetColumns default 1;
    property Rows: Integer read GetRows write SetRows default 0;
    property ShowFocus: Boolean read GetShowFocus write SetShowFocus default true;
    property BorderMode: TListBorderMode read GetBorderMode write SetBorderMode default bmNormal;
    property BorderTypes: TListBorderTypes read GetBorderTypes write SetBorderTypes default [btNormalLeft, btNormalTop, btNormalRight, btNormalBottom];
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 150;
    property DropDownHeight: Integer read FDropDownHeight write SetDropDownHeight default 200;
  end;

  TFullDropDownItem = class(TTextItem, IGDIPBase, ITMSStyle, ITMSTones, IPictureContainerSupport)
  private
    FBlockUpdate: Boolean;
    FAllowSelect: Boolean;
    time: cardinal;
    FContainer: TAdvPolyList;
    FDeactivating: Boolean;
    ContainerParent: TDropDownForm;
    FOnContainerItemSelect: TItemSelectEvent;
    FDropDownAppearance: TDropDownAppearance;
    FItemIndex: integer;
    FItem: TCustomItem;
    FAutoSetItemSize: Boolean;
    procedure HideParent;
    procedure SetItems(const Value: TCustomBaseList);
    function GetItems: TCustomBaseList;
    procedure SetDropDownAppearance(const Value: TDropDownAppearance);
    procedure SetItemIndex(const Value: integer);
    procedure SetAutoSetItemSize(const Value: Boolean);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure IndexChanged(Sender: TObject; Item: TCustomItem; OldIndex, NewIndex: integer);
    procedure ContainerParentDeactivate(Sender: TObject);
    { Protected declarations }
    procedure DoInternalChange(Sender: TObject); override;
    procedure DoInternalItemClick(Sender: TObject; Item: TCustomItem); override;
    procedure ListItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure ContainerChanged(Sender: TObject);
    procedure ContainerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ContainerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ReadItemState(Reader: TReader; Item: TCustomItem);
    procedure SetItemParentComponent(AParent: TComponent; Item: TCustomItem);
    function GetFormOwner(AOwner: TComponent; var frm: TCustomForm; var ControlFlag: Boolean): TWinControl;
    function GetItemPos(AOwner: TCustomItem): TPoint;
    procedure HideAllBelow;
    function IsContainerVisible: Boolean;
    function GetVersionNr: Integer; override;
    procedure SelectNewItem(Item: TCustomItem);
    procedure RefreshItem(Sender: TObject);
    procedure DrawArrow(g: TGPGraphics; R: TGPRectF);
    procedure FillPictureNames(Proc: TGetStrProc);
    procedure GetPictures(APictureList: TPictureContainerList);
    procedure DoDropDown; virtual;
  public
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetColorTones(ATones: TColorTones);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    property Container: TAdvPolyList read FContainer write FContainer;
    function AddItem(AClass: TCustomItemClass): TCustomItem;
    procedure RemoveItem(Index: integer);
    procedure ClearItems;
    procedure SelectItem(Index: integer);
    procedure VisualizeItem(Index: integer; AllowScrollItem: Boolean = true;
      AllowSelectItem: Boolean = true);
    procedure ScrollToItem(Index: integer);
    function InsertItem(Index: integer; AClass: TCustomItemClass): TCustomItem;

    procedure DoCMMouseLeave(var Message: TMessage); override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoCMHintShow(var Message: TMessage; Interaction: TItemInteraction); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  published
    { Published declarations }
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property OnContainerItemSelect : TItemSelectEvent read FOnContainerItemSelect write FOnContainerItemSelect;
    property Items: TCustomBaseList read GetItems write SetItems;
    property DropDownAppearance : TDropDownAppearance read FDropDownAppearance write SetDropDownAppearance;
    property AutoSetItemSize: Boolean read FAutoSetItemSize write SetAutoSetItemSize default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TFullDropDownItem);
end;

{ TFullDropDownItem }

function TFullDropDownItem.AddItem(AClass: TCustomItemClass): TCustomItem;
begin
  Result := FContainer.AddItem(AClass)
end;

procedure TFullDropDownItem.ClearItems;
begin
  FContainer.ClearItems;
end;

procedure TFullDropDownItem.ContainerChanged(Sender: TObject);
begin
  if Assigned(ContainerParent) then
    ContainerParent.Invalidate;

  Changed;
end;

procedure TFullDropDownItem.ContainerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sel: TCustomItem;
begin
  case Key of
    VK_ESCAPE, VK_F4: HideParent;
    VK_RETURN, VK_SPACE:
    begin
      FAllowSelect := true;
      sel := Container.List.SelectedItem;
      if Assigned(sel) then
      begin
        Container.List.SelectItem(sel.Index);
      end;
    end;
  end;
end;

procedure TFullDropDownItem.ContainerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowSelect := true;
end;

procedure TFullDropDownItem.ContainerParentDeactivate(Sender: TObject);
begin
  time := GetTickCount;
  FDeactivating := true;
  HideAllBelow;
end;

constructor TFullDropDownItem.Create(AOwner: TComponent);
begin
  inherited;
  FContainer := TAdvPolyList.Create(Self);
  FContainer.Height := 200;
  FContainer.Ctl3D := false;
  FContainer.BorderStyle := bsNone;
  FContainer.AutoSizeMode := asmItems;
  FContainer.AutoSizeType := astWidth;
  FContainer.List.SetOwnerComponent(Self);
  FContainer.OnChange := ContainerChanged;
  FContainer.OnKeyDown := ContainerKeyDown;
  FContainer.OnMouseDown := ContainerMouseDown;
  FContainer.OnItemSelect := listitemselect;
  FContainer.Visible := false;

  FDropDownAppearance := TDropDownAppearance.Create(FContainer);
  FItemIndex := -1;
  FAutoSetItemSize := True;
end;

function TFullDropDownItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TFullDropDownItem.Create(AOwner);
end;

class function TFullDropDownItem.CustomClassName: String;
begin
  Result := 'Normal Full Drop Down Item';
end;

destructor TFullDropDownItem.Destroy;
begin
  FDropDownAppearance.Free;
  FContainer.Free;
  if Assigned(ContainerParent) then
    ContainerParent.Free;
  inherited;
end;

procedure TFullDropDownItem.DoCMHintShow(var Message: TMessage;
  Interaction: TItemInteraction);
type
  PHintInfo = ^THintInfo;
var
  hi: PHintInfo;
  int: TItemInteraction;
begin
  if Interaction.InteractionItem = Self then
  begin
    if Assigned(FItem) then
    begin
      hi := PHintInfo(Message.LParam);
      int.InteractionItem := FItem;
      int.InteractionType := FItem.GetItemInteraction(hi^.cursorPos.x,hi^.cursorpos.y);
      FItem.DoCMHintShow(Message, int);
    end
    else
      inherited;
  end
end;

procedure TFullDropDownItem.DoCMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FItem) then
    FItem.DoCMMouseLeave(Message);
end;

procedure TFullDropDownItem.DropDown;
begin
  DoInternalItemClick(Self, Self);
end;

procedure TFullDropDownItem.DoInternalChange(Sender: TObject);
begin
  inherited;
  if FBlockUpdate then
    Exit;

  FBlockUpdate := True;

  if Assigned(FItem) then
  begin
    FItem.Y := Self.Y;
    FItem.X := Self.X;
    FItem.Width := Self.Width;
    FItem.Height := Self.Height;
  end;

  FBlockUpdate := False;
end;

procedure TFullDropDownItem.DoInternalItemClick(Sender: TObject;
  Item: TCustomItem);
var
  co: TWinControl;
  parentf: TCustomForm;
  f: Boolean;
begin
  inherited;

  if FDeactivating then
  begin
    FDeactivating := false;
    if (GetTickCount - time < 200) then
    begin
      Exit;
    end;
  end;

  if not Assigned(ContainerParent) then
  begin
    parentf := nil;
    co := GetFormOwner(Self.ItemOwner, parentf, f);
    if f then
    begin
      if Assigned(co) then
        parentf := GetParentForm(co);
    end;

    if Assigned(parentf) then
    begin
      ContainerParent := TDropDownForm.CreateNew(parentf);
      ContainerParent.Container := FContainer;
      ContainerParent.OnDeactivate := ContainerParentDeactivate;
      ContainerParent.Init;
    end;
  end;

  if not Assigned(ContainerParent) then
    Exit;

  if Assigned(ContainerParent) then
  begin
    if ContainerParent.Visible then
    begin
      HideAllBelow;
      Exit;
    end
    else
      DoDropDown;
  end
  else
    DoDropDown;
end;

procedure TFullDropDownItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if not (ssAlt in Shift) then
  begin
    case Key of
      VK_F4: DoInternalItemClick(Self, Self);
    end;
  end;
end;

procedure TFullDropDownItem.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
var
  int: TItemInteraction;
begin
  if Interaction.InteractionItem = Self then
  begin
    if Assigned(FItem) then
    begin
      int.InteractionItem := FItem;
      int.InteractionType := FItem.GetItemInteraction(pX, pY);
      FItem.DoMouseDown(Sender, Button, Shift, pX, pY, int, ItemAppearance);
    end
    else
      inherited;
  end;
end;

procedure TFullDropDownItem.DoMouseMove(Sender: TObject; Shift: TShiftState; pX,
  pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
var
  int: TItemInteraction;
begin
  if Interaction.InteractionItem = Self then
  begin
    inherited;
    if Assigned(FItem) then
    begin
      int.InteractionItem := FItem;
      int.InteractionType := FItem.GetItemInteraction(pX, pY);
      FItem.DoMouseMove(Sender, Shift, pX, pY, int, ItemAppearance);
    end
    else
      inherited;
  end
  else
  begin
    ClearItemState;
    if Assigned(FItem) then
      FItem.ClearItemState;
  end;
end;

procedure TFullDropDownItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
var
  int: TItemInteraction;
begin
  if Interaction.InteractionItem = Self then
  begin
    if Assigned(FItem) then
    begin
      int.InteractionItem := FItem;
      int.InteractionType := FItem.GetItemInteraction(pX, pY);
      FItem.DoMouseUp(Sender, Button, Shift, pX, pY, int, ItemAppearance);
    end
    else
      inherited;
  end
  else if (Interaction.InteractionItem <> nil) and (Interaction.InteractionType = itDefault) then
  begin
    if (State = isSelected) and Interaction.InteractionItem.Selectable then
      State := isNormal;
  end;
end;

procedure TFullDropDownItem.DrawArrow(g: TGPGraphics; R: TGPRectF);
var
  path: TGPGraphicsPath;
  b: TGPSolidBrush;
  pts: array[0..3] of TGPPointF;
  s: integer;
  p: TGPPen;
  smt: TSmoothingMode;
begin
  s := 7;
  path := TGPGraphicsPath.Create;

  pts[0].X := r.X + r.Width - s - 5;
  pts[0].Y := R.Y + (r.Height - s / 3 * 2) / 2;

  pts[1].X := r.X + r.Width - 5;
  pts[1].Y := R.Y + (r.Height - s / 3 * 2) / 2;

  pts[2].X := r.X + r.Width - 5 - s / 2;
  pts[2].Y := R.Y + (r.Height + s / 2) / 2;
  path.AddPolygon(PGPPointF(@pts), 3);

  smt := g.GetSmoothingMode;
  g.SetSmoothingMode(SmoothingModeDefault);

  b := TGPSolidBrush.Create(MakeColor(255, clBlack));
  g.FillPath(b, path);
  p := TGPPEN.Create(MakeColor(255, clBlack));
  g.DrawPath(p, path);

  g.SetSmoothingMode(smt);

  p.Free;
  b.Free;
  path.Free;
end;

procedure TFullDropDownItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    if Assigned(FItem) then
      FItem.DrawInRect(g, ItemAppearance, R)
    else
      inherited;

    DrawArrow(g, R);
    if ItemAppearance.Focus and (ItemAppearance.FocusedItem = Index) then
      DrawFocus(g, R, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TFullDropDownItem.DoDropDown;
var
  ContainerPos: TPoint;
  r: TRect;
  pos, t: TPoint;
  co: TWinControl;
  I: Integer;
  selx, sely, selw, selh: integer;

  function Min(a, b: Integer): Integer;
  begin
    if (a > b) then
      Result := b
    else
      Result := a;
  end;

begin
  Container.Width := DropDownAppearance.DropDownWidth;
  Container.Height := DropDownAppearance.DropDownHeight;
  co := GetCustomControlOwner(Self);
  if Assigned(co) then
  begin
    pos := GetItemPos(Self);
    t := TCustomControl(co).ClientToScreen(pos);
    ContainerPos.X := t.X;
    ContainerPos.Y := t.Y;

    if co is TCustomItemsContainer then
    begin
      Container.PictureContainer := (co as TCustomItemsContainer).PictureContainer;
      Container.ImageList := (co as TCustomItemsContainer).ImageList;
    end;
  end;

  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0); // account for taskbar...
  ContainerParent.Position := [];
  if (ContainerPos.Y + FContainer.Height > r.Bottom) then
  begin
    ContainerPos.Y := ContainerPos.Y - FContainer.Height - Height + 3;
    ContainerParent.Position := ContainerParent.Position + [fpTop];
  end
  else
    ContainerParent.Position := ContainerParent.Position + [fpBottom];

  if (ContainerPos.X + FContainer.Width > r.right) then
  begin
    ContainerPos.X := ContainerPos.X - (FContainer.Width - Width);
    ContainerParent.Position := ContainerParent.Position + [fpLeft];
  end
  else
    ContainerParent.Position := ContainerParent.Position + [fpRight];

  SetWindowPos(ContainerParent.Handle, 0, ContainerPos.X, ContainerPos.Y, Container.Width, Container.Height, 0);
  Container.BeginUpdate;
  for I := 0 to Items.Items.Count - 1 do
  begin
    if Items.Items[I] is TCustomItem then
    begin
      if (ItemIndex = I) and Assigned(FItem) then
      begin
        selx := TCustomItem(Items.Items[I]).X;
        sely := TCustomItem(Items.Items[I]).Y;
        selw := TCustomItem(Items.Items[I]).Width;
        selh := TCustomItem(Items.Items[I]).Height;
        TCustomItem(Items.Items[I]).Assign(FItem);
        TCustomItem(Items.Items[I]).X := selx;
        TCustomItem(Items.Items[I]).Y := sely;
        TCustomItem(Items.Items[I]).Width := selw;
        TCustomItem(Items.Items[I]).Height := selh;
      end;

      if Assigned(TCustomItem(Items.Items[I]).Control) then
      begin
        TCustomItem(Items.Items[I]).Control.Parent := Container;
        TCustomItem(Items.Items[I]).Control.Visible := true;
      end;
    end;
  end;
  Container.EndUpdate;
  ContainerParent.Width := Container.Width;
  ContainerParent.Height := Container.Height;
  Container.Parent := ContainerParent;
  ContainerParent.Visible := true;
  Container.Visible := true;
  Container.SetFocus;
end;

procedure TFullDropDownItem.FillPictureNames(Proc: TGetStrProc);
var
  intf: IPictureContainerSupport;
begin
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, intf) then
      intf.FillPictureNames(Proc);
  end;
end;

procedure TFullDropDownItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Items.Items.Count - 1 do
    Proc(TCustomItem(Items.Items[i]));
end;

function TFullDropDownItem.GetClassType: TComponentClass;
begin
  Result := TFullDropDownItem;
end;

function TFullDropDownItem.GetFormOwner(AOwner: TComponent; var frm: TCustomForm; var ControlFlag: Boolean): TWinControl;
begin
  Result := nil;
  ControlFlag := true;
  if AOwner is TWinControl then
  begin
    ControlFlag := true;
    Result := TWinControl(AOwner);
  end
  else if Assigned(AOwner) then
  begin
    if (AOwner is TFullDropDownItem) then
    begin
      ControlFlag := false;
      frm := TFullDropDownItem(AOwner).ContainerParent;
    end
  end
  else if AOwner = nil then
  begin
    Result := nil;
  end;
end;

function TFullDropDownItem.GetItemPos(AOwner: TCustomItem): TPoint;
var
  p: TPoint;
begin
  p := Point(AOwner.X, AOwner.Y + AOwner.Height);
  Result.X := p.X;
  Result.Y := p.Y;
  if (AOwner.ItemOwner is TCustomItem) then
  begin
    p := GetItemPos(TCustomItem(AOwner.ItemOwner));
    Result.X := Result.X + p.X;
    Result.Y := Result.Y + p.Y;
  end;
end;

function TFullDropDownItem.GetItems: TCustomBaseList;
begin
  Result := Container.List;
end;

procedure TFullDropDownItem.GetPictures(APictureList: TPictureContainerList);
var
  intf: IPictureContainerSupport;
begin
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, intf) then
      intf.GetPictures(APictureList);
  end;
end;

function TFullDropDownItem.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TFullDropDownItem.HideAllBelow;
var
  I: Integer;
begin
  for I := 0 to Items.Items.Count - 1 do
  begin
    if Items.Items[I] is TFullDropDownItem then
    begin
      TFullDropDownItem(Items.Items[I]).HideAllBelow;
    end;
  end;
  HideParent;
end;

procedure TFullDropDownItem.HideParent;
begin
  if not Assigned(ContainerParent) then
    Exit;

  if (csDesigning in ComponentState) then
    Exit;

  if ContainerParent.HandleAllocated then
  begin
    ContainerParent.Hide;
    if Assigned(ItemOwner) then
    begin
      if (ItemOwner is TWinControl) then
        (ItemOwner as TWinControl).SetFocus;
    end;
  end;
end;

procedure TFullDropDownItem.IndexChanged(Sender: TObject; Item: TCustomItem; OldIndex, NewIndex: integer);
begin

end;

function TFullDropDownItem.InsertItem(Index: integer;
  AClass: TCustomItemClass): TCustomItem;
begin
  Result := FContainer.InsertItem(Index, AClass);
end;

function TFullDropDownItem.IsContainerVisible: Boolean;
begin
  Result := false;
  if Assigned(ContainerParent) then
    Result := ContainerParent.Visible;
end;

procedure TFullDropDownItem.ListItemSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if FAllowSelect then
  begin
    FAllowSelect := false;
    if Assigned(OnContainerItemSelect) then
      OnContainerItemSelect(Sender, Item, Allow);

    if Allow then
    begin
      ItemIndex := Item.Index;
      HideParent;
    end;
  end;
end;

procedure TFullDropDownItem.Loaded;
begin
  inherited;
  ItemIndex := ItemIndex;
end;

procedure TFullDropDownItem.ReadItemState(Reader: TReader;
  Item: TCustomItem);
begin
  if Reader.Parent = Self then
  begin
    if Items.Items.IndexOf(Item) = -1 then
    begin
      Item.ItemOwner := Self;
      Items.AssignEvents(Item);
      Items.Items.Add(Item);
    end;
  end;
end;

procedure TFullDropDownItem.RefreshItem(Sender: TObject);
begin
  Self.RefreshObject;
end;

procedure TFullDropDownItem.RemoveItem(Index: integer);
begin
  FContainer.RemoveItem(Index);
end;

procedure TFullDropDownItem.ScrollToItem(Index: integer);
begin
  FContainer.ScrollToItem(Index);
end;

procedure TFullDropDownItem.SelectItem(Index: integer);
begin
  FContainer.SelectItem(Index);
end;

procedure TFullDropDownItem.SelectNewItem(Item: TCustomItem);
begin
  if Assigned(FItem) then
    FItem.Free;

  if Assigned(Item) then
  begin
    FItem := Item.CreateNewItem(Self);
    FItem.Assign(Item);
    if AutoSetItemSize then
    begin
      Height := Item.Height;
      Width := Item.Width;
    end;
    FItem.X := X;
    FItem.Y := Y;
    FItem.Width := Width;
    FItem.Height := Height;
    FItem.AssignEvents(Item);
    FItem.Index := Item.Index;
    FItem.OnInternalRefresh := OnInternalRefresh;
    FItem.OnInternalItemClick := DoInternalItemClick;
  end
  else
    FItem := nil;
end;

procedure TFullDropDownItem.SetAutoSetItemSize(const Value: Boolean);
begin
  if FAutoSetItemSize <> Value then
  begin
    FAutoSetItemSize := Value;
    Changed;
  end;
end;

procedure TFullDropDownItem.SetColorTones(ATones: TColorTones);
begin
  if Assigned(Container) then
    Container.SetColorTones(ATones);
end;

procedure TFullDropDownItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  if Assigned(Container) then
    Container.SetComponentStyle(AStyle);
end;

procedure TFullDropDownItem.SetDropDownAppearance
  (const Value: TDropDownAppearance);
begin
  if FDropDownAppearance <> Value then
  begin
    FDropDownAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TFullDropDownItem.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  if Assigned(Container) then
  begin
    if (FItemIndex >= 0) and (FItemIndex <= Container.List.Items.Count - 1) then
      SelectNewItem(Container.Items[FItemIndex])
    else
      SelectNewItem(nil);

    FBlockUpdate := True;
    Changed;
    FBlockUpdate := False;
  end;
end;

procedure TFullDropDownItem.SetItemParentComponent(AParent: TComponent;
  Item: TCustomItem);
begin
  if AParent = Self then
  begin
    if Items.Items.IndexOf(Item) = -1 then
    begin
      Item.ItemOwner := Self;
      Items.AssignEvents(Item);
      Items.Items.Add(Item);
    end;
  end;
end;

procedure TFullDropDownItem.SetItems(const Value: TCustomBaseList);
begin
  Container.List.Assign(Value);
end;

procedure TFullDropDownItem.VisualizeItem(Index: integer; AllowScrollItem,
  AllowSelectItem: Boolean);
begin
  FContainer.VisualizeItem(Index, AllowScrollItem, AllowSelectItem);
end;

procedure TDropDownForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TDropDownForm.Init;
begin
  DoubleBuffered := true;
  Visible := false;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  Color := clWhite;
  FormStyle := fsStayOnTop;
end;

procedure TDropDownForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TDropDownForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ TDropDownAppearance }

constructor TDropDownAppearance.Create(AOwner: TAdvPolyList);
begin
  FOwner := AOwner;
  FDropDownWidth := 150;
  FDropDownHeight := 200;
  if Assigned(FOwner) then
  begin
    FOwner.Fill.Color := clWhite;
    FOwner.Fill.GradientType := gtSolid;
    FOwner.Fill.BorderColor := clSilver;
  end;
end;

destructor TDropDownAppearance.Destroy;
begin
  inherited;
end;

function TDropDownAppearance.GetAutoSizeMode: TAutoSizeMode;
begin
  Result := FOwner.AutoSizeMode;
end;

function TDropDownAppearance.GetAutoSizeType: TAutoSizeType;
begin
  Result := FOwner.AutoSizeType;
end;

function TDropDownAppearance.GetBorderMode: TListBorderMode;
begin
  Result := FOwner.BorderMode;
end;

function TDropDownAppearance.GetBorderTypes: TListBorderTypes;
begin
  Result := FOwner.BorderTypes;
end;

function TDropDownAppearance.GetColumns: Integer;
begin
  Result := FOwner.Columns;
end;

function TDropDownAppearance.GetFill: TGDIPFill;
begin
  Result := FOwner.Fill;
end;

function TDropDownAppearance.GetHorizontalSpacing: Integer;
begin
  Result := FOwner.HorizontalSpacing;
end;

function TDropDownAppearance.GetListMargins: TMargins;
begin
  Result := FOwner.ListMargins;
end;

function TDropDownAppearance.GetReadOnly: Boolean;
begin
  Result := FOwner.ReadOnly;
end;

function TDropDownAppearance.GetRows: Integer;
begin
  Result := FOwner.Rows;
end;

function TDropDownAppearance.GetShowFocus: Boolean;
begin
  Result := FOwner.ShowFocus;
end;

function TDropDownAppearance.GetVerticalSpacing: Integer;
begin
  Result := FOwner.VerticalSpacing;
end;

procedure TDropDownAppearance.SetAutoSizeMode(const Value: TAutoSizeMode);
begin
  FOwner.AutoSizeMode := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FOwner.AutoSizeType := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetBorderMode(const Value: TListBorderMode);
begin
  FOwner.BorderMode := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetBorderTypes(const Value: TListBorderTypes);
begin
  FOwner.BorderTypes := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetColumns(const Value: Integer);
begin
  FOwner.Columns := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetDropDownHeight(const Value: Integer);
begin
  FDropDownHeight := Value;
  FOwner.Invalidate;
end;

procedure TDropDownAppearance.SetDropDownWidth(const Value: Integer);
begin
  FDropDownWidth := Value;
  FOwner.Invalidate;
end;

procedure TDropDownAppearance.SetFill(const Value: TGDIPFill);
begin
  FOwner.Fill.Assign(Value);
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetHorizontalSpacing(const Value: Integer);
begin
  FOwner.HorizontalSpacing := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetListMargins(const Value: TMargins);
begin
  FOwner.ListMargins.Assign(Value);
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetReadOnly(const Value: Boolean);
begin
  FOwner.ReadOnly := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetRows(const Value: Integer);
begin
  FOwner.Rows := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetShowFocus(const Value: Boolean);
begin
  FOwner.ShowFocus := Value;
  FOwner.List.Changed;
end;

procedure TDropDownAppearance.SetVerticalSpacing(const Value: Integer);
begin
  FOwner.VerticalSpacing := Value;
  FOwner.List.Changed;
end;

end.
