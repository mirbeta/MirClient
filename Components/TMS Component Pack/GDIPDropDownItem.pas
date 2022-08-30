{*************************************************************************}
{ TDropDownItem Class                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2013                                      }
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

unit GDIPDropDownItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics,
  GDIPCustomItem, GDIPBase, AdvPolyList, CustomItemsContainer,  AdvGDIP,
  GDIPImageTextButtonItem, ExtCtrls, SysUtils, GDIPFill,
  AdvStyleIF, Types, Contnrs;

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

  TDropDownItem = class;

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
    FDropDownHeight: Integer;
    FDropDownWidth: Integer;
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
    procedure SetDropDownHeight(const Value: Integer);
    procedure SetDropDownWidth(const Value: Integer);
  public
    constructor Create(AOwner: TAdvPolyList);
    destructor Destroy; override;
  published
    property DropDownHeight: Integer read FDropDownHeight write SetDropDownHeight default 350;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 250;
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
  end;

  TDropDownItem = class(TImageTextButtonItem, IGDIPBase, ITMSStyle, ITMSTones, IPictureContainerSupport)
  private
    time: cardinal;
    FContainer: TAdvPolyList;
    FDeactivating: Boolean;
    ContainerParent: TDropDownForm;
    FOnContainerItemSelect: TItemSelectEvent;
    FDropDownAppearance: TDropDownAppearance;
    procedure HideParent;
    procedure SetItems(const Value: TCustomBaseList);
    function GetItems: TCustomBaseList;
    procedure SetDropDownAppearance(const Value: TDropDownAppearance);
    { Private declarations }
  protected
    procedure ContainerParentDeactivate(Sender: TObject);
    { Protected declarations }
    procedure DoInternalItemButtonClick(Sender: TObject; Item: TCustomItem); override;
    procedure ListItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure ContainerChanged(Sender: TObject);
    procedure ContainerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    property Container: TAdvPolyList read FContainer write FContainer;
    procedure ReadItemState(Reader: TReader; Item: TCustomItem);
    procedure SetItemParentComponent(AParent: TComponent; Item: TCustomItem);
    function GetFormOwner(AOwner: TComponent; var frm: TCustomForm; var ControlFlag: Boolean): TWinControl;
    function GetItemPos(AOwner: TCustomItem): TPoint;
    procedure HideAllBelow;
    function IsContainerVisible: Boolean;
    function GetVersionNr: Integer; override;
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
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);

    function AddItem(AClass: TCustomItemClass): TCustomItem;
    procedure RemoveItem(Index: integer);
    procedure ClearItems;
    procedure SelectItem(Index: integer);
    procedure VisualizeItem(Index: integer; AllowScrollItem: Boolean = true;
      AllowSelectItem: Boolean = true);
    procedure ScrollToItem(Index: integer);
    function InsertItem(Index: integer; AClass: TCustomItemClass): TCustomItem;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  published
    { Published declarations }
    property OnContainerItemSelect : TItemSelectEvent read FOnContainerItemSelect write FOnContainerItemSelect;
    property Items: TCustomBaseList read GetItems write SetItems;
    property DropDownAppearance : TDropDownAppearance read FDropDownAppearance write SetDropDownAppearance;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TDropDownItem);
end;

{ TDropDownItem }

function TDropDownItem.AddItem(AClass: TCustomItemClass): TCustomItem;
begin
  Result := FContainer.AddItem(AClass);
end;

procedure TDropDownItem.ClearItems;
begin
  FContainer.ClearItems;
end;

procedure TDropDownItem.ContainerChanged(Sender: TObject);
begin
  if Assigned(ContainerParent) then
    ContainerParent.Invalidate;
end;

procedure TDropDownItem.ContainerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE, VK_F4: HideParent;
  end;
end;

procedure TDropDownItem.ContainerParentDeactivate(Sender: TObject);
begin
  time := GetTickCount;
  FDeactivating := true;
  Down := false;
  HideAllBelow;
end;

constructor TDropDownItem.Create(AOwner: TComponent);
begin
  inherited;
  FContainer := TAdvPolyList.Create(Self);
  FContainer.List.SetOwnerComponent(Self);
  FContainer.OnChange := ContainerChanged;
  FContainer.OnKeyDown := ContainerKeyDown;
  FContainer.OnItemSelect := ListItemSelect;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    FContainer.Ctl3D := false;
    FContainer.Align := alClient;
    FContainer.BorderStyle := bsNone;
    FContainer.AutoSizeMode := asmItems;
    FContainer.AutoSizeType := astWidth;
  end;

  FDropDownAppearance := TDropDownAppearance.Create(FContainer);
end;

function TDropDownItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TDropDownItem.Create(AOwner);
end;

class function TDropDownItem.CustomClassName: String;
begin
  Result := 'Normal Drop Down Item';
end;

destructor TDropDownItem.Destroy;
begin
  FDropDownAppearance.Free;
  FContainer.Free;
  if Assigned(ContainerParent) then
    ContainerParent.Free;
  inherited;
end;

procedure TDropDownItem.DropDown;
begin
  DoInternalItemButtonClick(Self, Self);
end;

procedure TDropDownItem.DoInternalItemButtonClick(Sender: TObject;
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
      Down := false;
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

procedure TDropDownItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_F4: DoInternalItemClick(Self, Self);
  end;
end;

procedure TDropDownItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);
    inherited;
    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TDropDownItem.DoDropDown;
var
  ContainerPos: TPoint;
  r: TRect;
  pos, t: TPoint;
  co: TWinControl;
  I: Integer;

  function Min(a, b: Integer): Integer;
  begin
    if (a > b) then
      Result := b
    else
      Result := a;
  end;

begin
  FContainer.Width := DropDownAppearance.DropDownWidth;
  FContainer.Height := DropDownAppearance.DropDownHeight;

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
  Container.SetFocus;
end;

procedure TDropDownItem.FillPictureNames(Proc: TGetStrProc);
var
  intf: IPictureContainerSupport;
begin
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, intf) then
      intf.FillPictureNames(Proc);
  end;
end;

procedure TDropDownItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Items.Items.Count - 1 do
    Proc(TCustomItem(Items.Items[i]));
end;

function TDropDownItem.GetClassType: TComponentClass;
begin
  Result := TDropDownItem;
end;

function TDropDownItem.GetFormOwner(AOwner: TComponent; var frm: TCustomForm; var ControlFlag: Boolean): TWinControl;
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
    if (AOwner is TDropDownItem) then
    begin
      ControlFlag := false;
      frm := TDropDownItem(AOwner).ContainerParent;
    end
  end
  else if AOwner = nil then
  begin
    Result := nil;
  end;
end;

function TDropDownItem.GetItemPos(AOwner: TCustomItem): TPoint;
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

function TDropDownItem.GetItems: TCustomBaseList;
begin
  Result := Container.List;
end;

procedure TDropDownItem.GetPictures(APictureList: TPictureContainerList);
var
  intf: IPictureContainerSupport;
begin
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, intf) then
      intf.GetPictures(APictureList);
  end;
end;

function TDropDownItem.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TDropDownItem.HideAllBelow;
var
  I: Integer;
begin
  for I := 0 to Items.Items.Count - 1 do
  begin
    if Items.Items[I] is TDropDownItem then
    begin
      TDropDownItem(Items.Items[I]).HideAllBelow;
    end;
  end;
  HideParent;
end;

procedure TDropDownItem.HideParent;
begin
  if not Assigned(ContainerParent) then
    Exit;

  if (csDesigning in ComponentState) then
    Exit;

  if ContainerParent.HandleAllocated then
    ContainerParent.Hide;
end;

function TDropDownItem.InsertItem(Index: integer;
  AClass: TCustomItemClass): TCustomItem;
begin
  Result := FContainer.InsertItem(Index, AClass);
end;

function TDropDownItem.IsContainerVisible: Boolean;
begin
  Result := false;
  if Assigned(ContainerParent) then
    Result := ContainerParent.Visible;
end;

procedure TDropDownItem.ListItemSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnContainerItemSelect) then
    OnContainerItemSelect(Sender, Item, Allow);
end;

procedure TDropDownItem.ReadItemState(Reader: TReader;
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

procedure TDropDownItem.RemoveItem(Index: integer);
begin
  FContainer.RemoveItem(Index);
end;

procedure TDropDownItem.ScrollToItem(Index: integer);
begin
  FContainer.ScrollToItem(Index);
end;

procedure TDropDownItem.SelectItem(Index: integer);
begin
  FContainer.SelectItem(Index);
end;

procedure TDropDownItem.SetColorTones(ATones: TColorTones);
begin
  if Assigned(Container) then
    Container.SetColorTones(ATones);
end;

procedure TDropDownItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  if Assigned(Container) then
    Container.SetComponentStyle(AStyle);
end;

procedure TDropDownItem.SetDropDownAppearance
  (const Value: TDropDownAppearance);
begin
  if FDropDownAppearance <> Value then
  begin
    FDropDownAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TDropDownItem.SetItemParentComponent(AParent: TComponent;
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

procedure TDropDownItem.SetItems(const Value: TCustomBaseList);
begin
  Container.List.Assign(Value);
end;

procedure TDropDownItem.VisualizeItem(Index: integer; AllowScrollItem,
  AllowSelectItem: Boolean);
begin
  Fcontainer.VisualizeItem(Index, AllowScrollItem, AllowSelectItem);
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
  FDropDownHeight := 350;
  FDropDownWidth := 250;
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
  if FDropDownHeight <> Value then
    FDropDownHeight := Value;
end;

procedure TDropDownAppearance.SetDropDownWidth(const Value: Integer);
begin
  if FDropDownWidth <> Value then
    FDropDownWidth := Value;
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
