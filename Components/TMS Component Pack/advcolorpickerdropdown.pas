{***************************************************************************}
{ TAdvColorPickerDropDown components                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvColorPickerDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ExtCtrls, SysUtils, Forms, Math, AdvDropDown,
  AdvStyleIF, Types
  {$IFDEF DELPHI2006_LVL}
  , GraphUtil
  {$ENDIF}
  ;

type
  TColorSelectionStyle = (csList, csDiscrete, csColorCube, csSpectrum);
  TItemPos = (ipStandAlone, ipTop, ipMiddle, ipBottom);

  TColorItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FColor: TColor;
    FEnabled: Boolean;
    FRect: TRect;
    FHint: string;
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
  protected
    procedure Changed;
    property Rect: TRect read FRect write FRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clNone;
    property Hint: string read FHint write FHint;
  end;

  TColorItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TColorItem;
    procedure SetItem(Index: Integer; const Value: TColorItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TColorItem read GetItem write SetItem; default;
    function Add: TColorItem;
    function Insert(Index: Integer): TColorItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvCustomColorSelectorPanel = class(TCustomControl)
  private
    FOnShouldHide: TNotifyEvent;
  protected
    property OnShouldHide: TNotifyEvent read FOnShouldHide write FOnShouldHide;
  end;

  TColorCubeCell = record
    CenterPos: TPoint;
    Color: TColor;
  end;

  TAdvColorCubePanel = class(TAdvCustomColorSelectorPanel)
  private
    FCubeCells: array[1..127] of TColorCubeCell;
    FCubeSize: TPoint;
    FSelectedColor: TColor;
    FSelectedIndex: integer;
    FHotIndex: integer;
    FOnSelect: TNotifyEvent;
    FShowRGBHint: Boolean;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawColorCube(Index: integer);
    procedure DrawAllColorCube;
    procedure DrawSelectedBorder;
    procedure DrawHotBorder;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedIndexAndColor(clr: TColor; index: integer = -1);
    procedure SetShowRGBHint(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function PtInCell(Index: integer; P: TPoint): Boolean;
    function IndexOfCellAt(X, Y: integer): integer;
    procedure DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer);
    procedure Initialize;
    procedure SetItemsPosition;
    procedure SetPanelSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property ShowRGBHint: Boolean read FShowRGBHint write SetShowRGBHint;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TAdvColorSpectrumPanel = class(TAdvCustomColorSelectorPanel)
  private
    FSpectrumImage: TImage;
    FHotColor: TColor;
    FSelectedColor: TColor;
    FHotRect: TRect;
    FSelectedRect: TRect;
    FOnSelect: TNotifyEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SpectrumImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SpectrumImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpectrumImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawHotRect;
    procedure DrawSelectedRect;
    procedure DrawFocusPoint;
    procedure SetSelectedColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemsPosition;
    procedure SetPanelSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TColorBoxAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FOuterBorderColorHot: TColor;
    FInnerBorderColorHot: TColor;
    FOuterBorderColorSelected: TColor;
    FInnerBorderColorSelected: TColor;
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write FBorderColor default clSilver;
    property OuterBorderColorHot: TColor read FOuterBorderColorHot write FOuterBorderColorHot;
    property InnerBorderColorHot: TColor read FInnerBorderColorHot write FInnerBorderColorHot;
    property OuterBorderColorSelected: TColor read FOuterBorderColorSelected write FOuterBorderColorSelected;
    property InnerBorderColorSelected: TColor read FInnerBorderColorSelected write FInnerBorderColorSelected;
  end;

  TColorBoxItem = class(TCollectionItem)
  private
    FColor: TColor;
    FEnabled: Boolean;
    FRect: TRect;
    FColorBoxPos: TItemPos;
    procedure SetColor(const Value: TColor);
    procedure SetColorBoxPos(const Value: TItemPos);
  protected
    procedure Changed;
    property Rect: TRect read FRect write FRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clNone;
    property ColorBoxPos: TItemPos read FColorBoxPos write SetColorBoxPos default ipStandAlone;
  end;

  TColorBoxItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TColorBoxItem;
    procedure SetItem(Index: Integer; const Value: TColorBoxItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TColorBoxItem read GetItem write SetItem; default;
    function Add: TColorBoxItem;
    function Insert(Index: Integer): TColorBoxItem;
    function GetOwner: TPersistent; override;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomColorSelector = class(TAdvCustomColorSelectorPanel)
  private
    FItemIndex: Integer;
    FItemHot: Integer;
    FOffSetX: Integer;  // Control's top/bottom offset
    FOffSetY: Integer;  // Control's Left/Right offset
    FOnItemSelect: TNotifyEvent;
    FColumns: Integer;
    FColumnGap: Integer;
    FColorBoxHeight: Integer;
    FColorBoxWidth: Integer;
    FColorBoxAppearance: TColorBoxAppearance;
    FItems: TColorBoxItems;
    FItemColorStyle: TSelectionColorStyle;
    FSelectedColor: TColor;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemHot(const Value: Integer);

    procedure SetColorBoxAppearance(const Value: TColorBoxAppearance);
    procedure SetColorBoxHeight(const Value: Integer);
    procedure SetColorBoxWidth(const Value: Integer);
    procedure SetColumnGap(const Value: Integer);
    procedure SetColumns(const Value: Integer);
    procedure SetItemColorStyle(const Value: TSelectionColorStyle);
    procedure SetItems(const Value: TColorBoxItems);
    procedure SetSelectedColor(const Value: TColor);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Paint; override;

    procedure DrawItems(aCanvas: TCanvas);
    procedure DrawItem(Index: Integer; aCanvas: TCanvas); virtual;
    function GetInnerRect: TRect; virtual;
    function GetItemSize: TSize; virtual;
    procedure UpdateRectAndSize; virtual;
    procedure InvalidateItem(Index: Integer); virtual;
    function ItemAtPos(X, Y: Integer): Integer;
    function GetItemRect(Index: Integer): TRect;

    procedure CreateDefaultColors;
    procedure HandleKey(Key: Word; EditorEnabled, DroppedDown: Boolean);
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectPrevious;
    procedure SelectNext;
    procedure HotPrevious;
    procedure HotNext;

    property ItemHot: Integer read FItemHot write SetItemHot default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ColorBoxAppearance: TColorBoxAppearance read FColorBoxAppearance write SetColorBoxAppearance;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;

    property Items: TColorBoxItems read FItems write SetItems;
    property ColumnGap: Integer read FColumnGap write SetColumnGap;
    property Columns: Integer read FColumns write SetColumns;
    property ItemColorStyle: TSelectionColorStyle read FItemColorStyle write SetItemColorStyle;
    property ColorBoxHeight: Integer read FColorBoxHeight write SetColorBoxHeight;
    property ColorBoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth;

    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
  end;

  TColorValueText = (cvtNone, cvtHex, cvtWebName, cvtRGB, cvtHTML);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvColorPickerDropDown = class(TAdvCustomDropDown)
  private
    FUpdateCount: integer;
    FItemSelector: TAdvCustomItemSelector;   // List item selector
    FColorCubePanel: TAdvColorCubePanel;
    FSpectrumPanel: TAdvColorSpectrumPanel;
    FColorSelector: TCustomColorSelector;
    FColumns: Integer;
    FLayout: TItemLayout;
    FColors: TColorItems;
    FItemIndex: Integer;
    FOnSelect: TNotifyEvent;
    FColorBoxWidth: Integer;
    FColorBoxHeight: Integer;
    FColorSelectionStyle: TColorSelectionStyle;
    FSelectedColor: TColor;
    FShowText: Boolean;
    FOnDrawSelectedColor: TOnDrawSelectedItem;
    FItemAppearance: TItemAppearance;
    FInternalCall: Boolean;
    FKeyTimer: TTimer;
    FCurSearch: string;
    FOldItemIndex: Integer;
    FOldSelectedColor: TColor;
    FOnDrawColor: TDrawItemEvent;
    FDesignTime: Boolean;
    FColorValueText: TColorValueText;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure DrawSelectedItem;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnSelectorItemSelect(Sender: TObject);
    procedure CubePanelOnSelect(Sender: TObject);
    procedure SpectrumPanelOnSelect(Sender: TObject);
    procedure ColorSelectorOnItemSelect(Sender: TObject);
    procedure OnItemSelectorDrawItem(Sender: TObject; Canvas: TCanvas; R: TRect; Index: Integer);
    procedure OnKeyTimerTime(Sender: TObject);
    procedure SetColumns(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetLayout(const Value: TItemLayout);
    procedure SetColors(const Value: TColorItems);
    procedure AssignedItemsToItemSelector;
    procedure SetSelectorProperties;
    procedure SetColorBoxHeight(const Value: Integer);
    procedure SetColorBoxWidth(const Value: Integer);
    procedure SetColorSelectionStyle(const Value: TColorSelectionStyle);
    procedure SetSelectedColor(const Value: TColor);
    procedure CreateItemListPanel;
    procedure CreateColorCubePanel;
    procedure CreateSpectrumPanel;
    procedure CreateColorSelectorPanel;
    procedure SetShowText(const Value: Boolean);
    procedure SetItemAppearance(const Value: TItemAppearance);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure OnDestroyDropDownForm; override;
    procedure SetEditRect; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure SetCenterControl; override;
    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;
    procedure DrawBackGround; override;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    function GetSelectedColorText: string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure AddDefaultColors;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clNone;
    property SelectedColorText: string read GetSelectedColorText;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Columns: Integer read FColumns write SetColumns default 1;
    property Colors: TColorItems read FColors write SetColors;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property ColorBoxHeight: Integer read FColorBoxHeight write SetColorBoxHeight default DD_COLORBOXHEIGHT;
    property ColorBoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth default DD_COLORBOXWIDTH;
    property Layout: TItemLayout read FLayout write SetLayout default ilCaptionRight;
    property ItemAppearance: TItemAppearance read FItemAppearance write SetItemAppearance;

    property ColorValueText: TColorValueText read FColorValueText write FColorValueText default cvtNone;
    property ColorSelectionStyle: TColorSelectionStyle read FColorSelectionStyle write SetColorSelectionStyle default csList;
    property ShowText: Boolean read FShowText write SetShowText default True;  // works only with csList

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property Cursor default crArrow;
    property BorderColor;
    property DisabledBorder;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight default 200;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownButtonGlyph;
    property DropDownSizeable;
    property Enabled;
    property Font;
    property Images;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnDrawSelectedColor: TOnDrawSelectedItem read FOnDrawSelectedColor write FOnDrawSelectedColor;
    property OnDrawColor: TDrawItemEvent read FOnDrawColor write FOnDrawColor;
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
  end;

implementation

uses StdCtrls;
{$R AdvColorPickerDropDown.RES}

const
  crTMSCur1 = 54;

type
  TInternalItemSelector = class(TAdvCustomItemSelector);
  TInternalItemAppearance = class(TItemAppearance);

//------------------------------------------------------------------------------

{ TColorItem }

procedure TColorItem.Assign(Source: TPersistent);
begin
  if (Source is TColorItem) then
  begin
    Caption := (Source as TColorItem).Caption;
    Color := (Source as TColorItem).Color;
    Enabled := (Source as TColorItem).Enabled;
    Hint := (Source as TColorItem).Hint;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TColorItem.Changed;
begin
  TColorItems(Collection).Change;
end;

//------------------------------------------------------------------------------

constructor TColorItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FColor := clNone;
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TColorItem.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TColorItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TColorItems }

function TColorItems.Add: TColorItem;
begin
  Result := TColorItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TColorItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TColorItems.Create(AOwner: TPersistent);
begin
  inherited Create(TColorItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TColorItems.GetItem(Index: Integer): TColorItem;
begin
  Result := TColorItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TColorItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TColorItems.Insert(Index: Integer): TColorItem;
begin
  Result := TColorItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TColorItems.SetItem(Index: Integer;
  const Value: TColorItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvColorCubePanel }

procedure TAdvColorCubePanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvColorCubePanel.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedColor := clNone;
  FHotIndex := -1;
  FSelectedIndex := -1;
  Initialize;
  ShowRGBHint := true;
end;

//------------------------------------------------------------------------------

destructor TAdvColorCubePanel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawColorCube(Index: integer);
begin
  if (Index >= 1) and (Index <= High(FCubeCells)) then
  begin
    Canvas.Pen.Color := FCubeCells[Index].Color;
    Canvas.Brush.Color := FCubeCells[Index].Color;
    DrawHexagon(Canvas, FCubeCells[Index].CenterPos, FCubeSize.X, FCubeSize.Y);
    if FHotIndex = Index then
      DrawHotBorder;
    if FSelectedIndex = Index then
      DrawSelectedBorder;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawAllColorCube;
var
  i: integer;
begin
  for i := 1 to high(FCubeCells) do
  begin
    DrawColorCube(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer);
begin
  P.X := P.X - X;
  P.Y := P.Y - y * 2;
  aCanvas.Polygon([Point(P.X, P.Y + Y), Point(P.X + X, P.Y),
    Point(P.X + X * 2, P.Y + Y), Point(P.X + X * 2, P.Y + Y * 3), Point(P.X + X, P.Y + Y * 4), Point(P.X, P.Y + Y * 3)]);
end;

//------------------------------------------------------------------------------

function TAdvColorCubePanel.IndexOfCellAt(X, Y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 1 to high(FCubeCells) do
  begin
    if PtInCell(i, Point(X, Y)) then
    begin
      Result := i;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.Initialize;
var
  c, r, i, j, cl: integer;
begin
  FCubeSize.X := 6;
  FCubeSize.Y := 3;

  FCubeCells[1].Color := $00663300; FCubeCells[2].Color := $00996633;
  FCubeCells[3].Color := $00CC6633; FCubeCells[4].Color := $00993300;
  FCubeCells[5].Color := $00990000; FCubeCells[6].Color := $00CC0000;
  FCubeCells[7].Color := $00660000; FCubeCells[8].Color := $00666600;
  FCubeCells[9].Color := $00996600; FCubeCells[10].Color := $00CC9900;
  FCubeCells[11].Color := $00CC6600; FCubeCells[12].Color := $00CC3300;
  FCubeCells[13].Color := clBlue; FCubeCells[14].Color := $00FF3333;
  FCubeCells[15].Color := $00993333; FCubeCells[16].Color := clTeal;
  FCubeCells[17].Color := $00999900; FCubeCells[18].Color := $00CCCC33;
  FCubeCells[19].Color := $00FFCC00; FCubeCells[20].Color := $00FF9900;
  FCubeCells[21].Color := $00FF6600; FCubeCells[22].Color := $00FF6633;
  FCubeCells[23].Color := $00CC3333; FCubeCells[24].Color := $00996666;
  FCubeCells[25].Color := $00669933; FCubeCells[26].Color := $0099CC00;
  FCubeCells[27].Color := $00CCFF00; FCubeCells[28].Color := clAqua;
  FCubeCells[29].Color := $00FFCC33; FCubeCells[30].Color := $00FF9933;
  FCubeCells[31].Color := $00FF9966; FCubeCells[32].Color := $00FF6666;
  FCubeCells[33].Color := $00FF0066; FCubeCells[34].Color := $00CC0066;
  FCubeCells[35].Color := $00339933; FCubeCells[36].Color := $0066CC00;
  FCubeCells[37].Color := $0099FF00; FCubeCells[38].Color := $00CCFF66;
  FCubeCells[39].Color := $00FFFF66; FCubeCells[40].Color := $00FFCC66;
  FCubeCells[41].Color := $00FFCC99; FCubeCells[42].Color := $00FF9999;
  FCubeCells[43].Color := $00FF6699; FCubeCells[44].Color := $00FF3399;
  FCubeCells[45].Color := $00FF0099; FCubeCells[46].Color := $00006600;
  FCubeCells[47].Color := $0000CC00; FCubeCells[48].Color := clLime;
  FCubeCells[49].Color := $0099FF66; FCubeCells[50].Color := $00CCFF99;
  FCubeCells[51].Color := $00FFFFCC; FCubeCells[52].Color := $00FFECCC;
  FCubeCells[53].Color := $00FFCCCC; FCubeCells[54].Color := $00FF99CC;
  FCubeCells[55].Color := $00FF66CC; FCubeCells[56].Color := $00FF00CC;
  FCubeCells[57].Color := $00CC0099; FCubeCells[58].Color := $00003300;
  FCubeCells[59].Color := clGreen; FCubeCells[60].Color := $0033CC33;
  FCubeCells[61].Color := $0066FF66; FCubeCells[62].Color := $0099FF99;
  FCubeCells[63].Color := $00CCFFCC; FCubeCells[64].Color := clWhite;
  FCubeCells[65].Color := $00FFCCFF; FCubeCells[66].Color := $00FF99FF;
  FCubeCells[67].Color := $00FF66FF; FCubeCells[68].Color := clFuchsia;
  FCubeCells[69].Color := $00CC00CC; FCubeCells[70].Color := $00660066;
  FCubeCells[71].Color := $00006633; FCubeCells[72].Color := $00009900;
  FCubeCells[73].Color := $0033FF66; FCubeCells[74].Color := $0066FF99;
  FCubeCells[75].Color := $0099FFCC; FCubeCells[76].Color := $00CCFFFF;
  FCubeCells[77].Color := $00CCCCFF; FCubeCells[78].Color := $00CC99FF;
  FCubeCells[79].Color := $00CC66FF; FCubeCells[80].Color := $00CC33FF;
  FCubeCells[81].Color := $009900CC; FCubeCells[82].Color := clPurple;
  FCubeCells[83].Color := $00003333; FCubeCells[84].Color := $00009966;
  FCubeCells[85].Color := $0033FF99; FCubeCells[86].Color := $0066FFCC;
  FCubeCells[87].Color := $0099FFFF; FCubeCells[88].Color := $0099CCFF;
  FCubeCells[89].Color := $009999FF; FCubeCells[90].Color := $009966FF;
  FCubeCells[91].Color := $009933FF; FCubeCells[92].Color := $009933CC;
  FCubeCells[93].Color := $00990099; FCubeCells[94].Color := $00336666;
  FCubeCells[95].Color := $0000CC99; FCubeCells[96].Color := $0033FFCC;
  FCubeCells[97].Color := $0066FFFF; FCubeCells[98].Color := $0066CCFF;
  FCubeCells[99].Color := $006699FF; FCubeCells[100].Color := $00807CFF;
  FCubeCells[101].Color := $006600FF; FCubeCells[102].Color := $009300D6;
  FCubeCells[103].Color := $00663399; FCubeCells[104].Color := clOlive;
  FCubeCells[105].Color := $0000CCCC; FCubeCells[106].Color := clYellow;
  FCubeCells[107].Color := $0000CCFF; FCubeCells[108].Color := $003399FF;
  FCubeCells[109].Color := $000066FF; FCubeCells[110].Color := $005050FF;
  FCubeCells[111].Color := $006600CC; FCubeCells[112].Color := $00330066;
  FCubeCells[113].Color := $00336699; FCubeCells[114].Color := $000099CC;
  FCubeCells[115].Color := $000099FF; FCubeCells[116].Color := $000066CC;
  FCubeCells[117].Color := $000033FF; FCubeCells[118].Color := clRed;
  FCubeCells[119].Color := $000000CC; FCubeCells[120].Color := $00330099;
  FCubeCells[121].Color := $00003366; FCubeCells[122].Color := $00006699;
  FCubeCells[123].Color := $000033CC; FCubeCells[124].Color := $00003399;
  FCubeCells[125].Color := $00000099; FCubeCells[126].Color := clMaroon;
  FCubeCells[127].Color := $002100A5;

  c := 55; // Left
  r := 20; // Top
  cl := 1;

  // Set Cells Position
  for i := 1 to 13 do
  begin
    if i < 8 then
    begin
      for j := 1 to 6 + i do
      begin
        //DrawHexagon(Canvas, Point(C,R), 6, 3);
        FCubeCells[cl].CenterPos := Point(C, R);
        c := c + FCubeSize.x * 2; //12;
        inc(cl);
      end;
      r := r + 10;
      c := 55 - (FCubeSize.x * i);
    end
    else
    begin
      c := 19 + (FCubeSize.x * (i - 7));
      for j := 1 to 13 - (i - 7) do
      begin
        //DrawHexagon(Canvas, Point(C,R), 6, 3);
        FCubeCells[cl].CenterPos := Point(C, R);
        c := c + FCubeSize.x * 2; //12;
        inc(cl);
      end;
      r := r + 10;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  inherited;
  i := IndexOfCellAt(X, Y);
  if i >= 1 then
  begin
    //SelectedColor:= FCubeCells[i].Color;
    if FSelectedIndex >= 1 then
    begin
      j := FSelectedIndex;
      FSelectedIndex := -1;
      DrawColorCube(j);
    end;

    SetSelectedIndexAndColor(FCubeCells[i].Color, i);

    DrawColorCube(i);

    if Assigned(FOnSelect) then
      FOnSelect(self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  inherited;
  i := IndexOfCellAt(X, Y);
  if (i >= 1) then
  begin
    if (FHotIndex <> i) then
    begin
      if FHotIndex >= 1 then
      begin
        j := FHotIndex;
        FHotIndex := -1;
        DrawColorCube(j);
      end;
      FHotIndex := i;
      DrawColorCube(FHotIndex);
      if ShowRGBHint then
      begin
        Hint := Format('Red: %d,  Green: %d,  Blue: %d', [GetRValue(FCubeCells[i].Color), GetGValue(FCubeCells[i].Color), GetBValue(FCubeCells[i].Color)]);
        Application.CancelHint;
      end;
    end;
  end
  else
  begin
    if ShowRGBHint then
    begin
      Hint := '';
      Application.CancelHint;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.Paint;
begin
  inherited;
  DrawAllColorCube;
end;

//------------------------------------------------------------------------------

function TAdvColorCubePanel.PtInCell(Index: integer; P: TPoint): Boolean;
begin
  Result := sqr(FCubeCells[Index].CenterPos.X - P.X) + sqr(FCubeCells[Index].CenterPos.Y - P.Y) <= (FCubeSize.X * FCubeSize.X);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetItemsPosition;
begin
  FHotIndex := -1;
  SetPanelSize;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetPanelSize;
var
  h: Integer;
begin
  h := 160;

  Height := h;
  Width := 182;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetSelectedColor(const Value: TColor);
begin
  SetSelectedIndexAndColor(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawSelectedBorder;
begin
  if (FSelectedIndex >= 1) and (FSelectedIndex <= High(FCubeCells)) then
  begin
    Canvas.Brush.style := bsClear;
    Canvas.Pen.Color := clBlack;
    DrawHexagon(Canvas, FCubeCells[FSelectedIndex].centerPos, FCubeSize.X - 1, FCubeSize.Y);
    Canvas.Pen.Color := clWhite;
    DrawHexagon(Canvas, FCubeCells[FSelectedIndex].centerPos, FCubeSize.X - 2, FCubeSize.Y - 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawHotBorder;
begin
  if (FHotIndex >= 1) and (FHotIndex <= High(FCubeCells)) then
  begin
    Canvas.Brush.style := bsClear;
    Canvas.Pen.Color := clRed;
    DrawHexagon(Canvas, FCubeCells[FHotIndex].centerPos, FCubeSize.X - 1, FCubeSize.Y);
    Canvas.Pen.Color := clWhite;
    DrawHexagon(Canvas, FCubeCells[FHotIndex].centerPos, FCubeSize.X - 2, FCubeSize.Y - 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetSelectedIndexAndColor(clr: TColor; index: integer);
var
  i: integer;
begin
  FSelectedColor := clr;


  if (Index >= 1) and (Index <= High(FCubeCells)) then
  begin
    FSelectedIndex := Index;
  end
  else
  begin
    FSelectedIndex := -1;
    for i := 1 to high(FCubeCells) do
      if FCubeCells[i].Color = FSelectedColor then
      begin
        FSelectedIndex := i;
        break;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetShowRGBHint(const Value: Boolean);
begin
  FShowRGBHint := Value;
  ShowHint := Value;
end;

//------------------------------------------------------------------------------

{ TAdvColorSpectrumPanel }

procedure TAdvColorSpectrumPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvColorSpectrumPanel.Create(AOwner: TComponent);
var
  tbmp: TBitmap;
begin
  inherited;

  FSpectrumImage := TImage.Create(self);
  FSpectrumImage.Parent := self;
  FSpectrumImage.Height := 128;
  FSpectrumImage.Width := 128;

  tbmp := TBitmap.Create;
  try
    tbmp.LoadFromResourceName(HInstance, 'TMS_SPECTRUM');
    FSpectrumImage.Picture.Assign(tbmp);
  finally
    tbmp.Free;
  end;

  FSpectrumImage.OnMouseMove := SpectrumImageMouseMove;
  FSpectrumImage.OnMouseDown := SpectrumImageMouseDown;
  FSpectrumImage.OnMouseUp := SpectrumImageMouseUp;
  FHotColor := clNone;
  FSelectedColor := clWhite;

  Screen.Cursors[crTMSCur1] := LoadCursor(HInstance, 'TMS_POINTER');
  FSpectrumImage.Cursor := crTMSCur1;
end;

//------------------------------------------------------------------------------

destructor TAdvColorSpectrumPanel.Destroy;
begin
  FSpectrumImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawFocusPoint;
begin
  {
  FSpectrumImage.Canvas.Pen.Color:= clWhite;
  FSpectrumImage.Canvas.MoveTo(50, 50);
  FSpectrumImage.Canvas.LineTo(100, 100);
  }
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawHotRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FHotRect.Left - 1, FHotRect.Top - 1, FHotRect.Right + 1, FHotRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FHotColor;
  Canvas.Rectangle(FHotRect.Left, FHotRect.Top, FHotRect.Right, FHotRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawSelectedRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FSelectedRect.Left - 1, FSelectedRect.Top - 1, FSelectedRect.Right + 1, FSelectedRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FSelectedColor;
  Canvas.Rectangle(FSelectedRect.Left, FSelectedRect.Top, FSelectedRect.Right, FSelectedRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.Paint;
begin
  inherited;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clwhite;
  Canvas.Rectangle(FSpectrumImage.Left - 1, FSpectrumImage.Top - 1, FSpectrumImage.Left + FSpectrumImage.Width + 1, FSpectrumImage.Top + FSpectrumImage.Height + 1);
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(FSpectrumImage.Left - 2, FSpectrumImage.Top - 2, FSpectrumImage.Left + FSpectrumImage.Width + 2, FSpectrumImage.Top + FSpectrumImage.Height + 2);

  DrawSelectedRect;
  DrawHotRect;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SetItemsPosition;
var
  y: Integer;
begin
  SetPanelSize;
  y := 10;

  FSpectrumImage.Left := (Width - FSpectrumImage.Width) div 2;
  FSpectrumImage.Top := y;
  FSelectedRect := Rect(FSpectrumImage.Left - 1, FSpectrumImage.Top + FSpectrumImage.Height + 8, FSpectrumImage.Left + (FSpectrumImage.Width div 2) - 2, FSpectrumImage.Top + FSpectrumImage.Height + 8 + 20);
  FHotRect := Rect(FSelectedRect.Right + 4, FSelectedRect.Top, FSpectrumImage.Left + FSpectrumImage.Width + 1, FSelectedRect.Bottom);
  FHotColor := clNone;

  DrawFocusPoint;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SetPanelSize;
var
  h: Integer;
begin
  h := 174;

  Height := h;
  Width := 150
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SetSelectedColor(const Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelectedColor := FHotColor;
  DrawSelectedRect;
  if Assigned(OnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FHotColor := Canvas.Pixels[X + FSpectrumImage.Left, Y + FSpectrumImage.Top];
  DrawHotRect;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

{ TColorBoxItem }

procedure TColorBoxItem.Assign(Source: TPersistent);
begin
  if (Source is TColorBoxItem) then
  begin
    Color := (Source as TColorBoxItem).Color;
    Enabled := (Source as TColorBoxItem).Enabled;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TColorBoxItem.Changed;
begin
  TColorBoxItems(Collection).Change;
end;

//------------------------------------------------------------------------------

constructor TColorBoxItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clNone;
  FColorBoxPos := ipStandAlone;
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TColorBoxItem.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TColorBoxItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorBoxItem.SetColorBoxPos(const Value: TItemPos);
begin
  FColorBoxPos := Value;
end;

//------------------------------------------------------------------------------

{ TColorBoxItems }

function TColorBoxItems.Add: TColorBoxItem;
begin
  Result := TColorBoxItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TColorBoxItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TColorBoxItems.Create(AOwner: TPersistent);
begin
  inherited Create(TColorBoxItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TColorBoxItems.GetItem(Index: Integer): TColorBoxItem;
begin
  Result := TColorBoxItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TColorBoxItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TColorBoxItems.Insert(Index: Integer): TColorBoxItem;
begin
  Result := TColorBoxItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TColorBoxItems.SetItem(Index: Integer;
  const Value: TColorBoxItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TCustomColorSelector }

constructor TCustomColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  FItemIndex := -1;
  FItemHot := -1;
  FOffSetX := 4;
  FOffSetY := 4;
  FColumnGap := 4;
  FColumns := 10;
  FColorBoxHeight := 12;
  FColorBoxWidth := 12;
  FColorBoxAppearance := TColorBoxAppearance.Create;
  FSelectedColor := clNone;
  FItems := TColorBoxItems.Create(Self);
  ItemColorStyle := scOffice2007;
  ParentColor := True;
end;

//------------------------------------------------------------------------------

destructor TCustomColorSelector.Destroy;
begin
  FColorBoxAppearance.Free;
  FItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetItemIndex(const Value: Integer);
var
  i: Integer;
begin
  //if (FItemIndex <> Value) then
  begin
    if (Value >= 0) and (Value < Items.Count) then
      if not Items[Value].Enabled then
        Exit;

    if (FItemIndex >= 0) and (FItemIndex <> Value) then  // refresh old Selected item
    begin
      i := FItemIndex;
      FItemIndex := -1;
      InvalidateItem(i);
    end;

    FItemIndex := Value;
    if (FItemIndex >= 0) then
    begin
      FSelectedColor := Items[FItemIndex].Color;

      DrawItem(FItemIndex, Canvas);
      if Assigned(OnItemSelect) then
        FOnItemSelect(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetItemHot(const Value: Integer);
var
  i: Integer;
begin
  if (FItemHot <> Value) then
  begin
    if (Value >= 0) and (Value < Items.Count) then
      if not Items[Value].Enabled then
        Exit;

    if (FItemHot >= 0) then  // refresh old hot item
    begin
      i := FItemHot;
      FItemHot := -1;
      InvalidateItem(i);
    end;

    FItemHot := Value;
    if (FItemHot >= 0) then
      DrawItem(FItemHot, Canvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ItemAtPos(X, Y);
  if (i >= 0) {and (i <> FItemIndex)} then
  begin
    if Items[i].Enabled then
    begin
      ItemIndex := i;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ItemAtPos(X, Y);
  if (i >= 0) then
  begin
    if (i <> FItemHot) then
    begin
      if Items[i].Enabled then
        ItemHot := i
      else
        ItemHot := -1;
    end;
  end
  else
  begin
    if (FItemHot >= 0) then
    begin
      ItemHot := -1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.Paint;
begin
  inherited;
  DrawItems(Canvas);
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.DrawItem(Index: Integer; aCanvas: TCanvas);
var
  OutBrClr, InBrClr, Clr: TColor;
  R: TRect;
  HotOrSelected: Boolean;
begin
  if (Index < 0) or (Index >= Items.Count) or not Assigned(aCanvas) then
    Exit;

  R := GetItemRect(Index);
  if (R.Left < 0) and (R.Right < 0) then
    Exit;

  OutBrClr := ColorBoxAppearance.BorderColor;
  InBrClr := clNone;
  Clr := Items[Index].Color;
  HotOrSelected := False;

  if (Index = FItemHot) then
  begin
    OutBrClr := ColorBoxAppearance.OuterBorderColorHot;
    InBrClr := ColorBoxAppearance.InnerBorderColorHot;
    HotOrSelected := True;
  end;

  if (Index = ItemIndex) then
  begin
    OutBrClr := ColorBoxAppearance.OuterBorderColorSelected;
    InBrClr := ColorBoxAppearance.InnerBorderColorSelected;
    HotOrSelected := True;
  end;

  if (Clr <> clNone) then
  begin
    Canvas.Pen.Color := Clr;
    Canvas.Brush.Color := Clr;
    Canvas.Rectangle(R);
  end;

  if HotOrSelected then
  begin
    if (OutBrClr <> clNone) then
    begin
      Canvas.Pen.Color := OutBrClr;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R);
      InflateRect(R, -1, -1);
    end;
    if (InBrClr <> clNone) then
    begin
      Canvas.Pen.Color := InBrClr;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R);
    end;
  end
  else
  begin
    Canvas.Pen.Color := OutBrClr;
    case Items[Index].ColorBoxPos of
      ipStandAlone:
      begin
        Canvas.MoveTo(R.Left, R.Bottom);
        Canvas.LineTo(R.Left, R.Top);
        Canvas.LineTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
        Canvas.LineTo(R.Left, R.Bottom);
      end;
      ipTop:
      begin
        Canvas.MoveTo(R.Left, R.Bottom);
        Canvas.LineTo(R.Left, R.Top);
        Canvas.LineTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
      end;
      ipMiddle:
      begin
        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Left, R.Bottom);
        Canvas.MoveTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
      end;
      ipBottom:
      begin
        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Left, R.Bottom);
        Canvas.LineTo(R.Right, R.Bottom);
        Canvas.MoveTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.UpdateRectAndSize;
var
  h, w, i, x, y, c: Integer;
  sz: TSize;
  R: TRect;
  saItem: Boolean;
begin
  if (Items.Count = 0) then
  begin
    Height := 50;
    Width := 100;
    Exit;
  end;

  FItemHot := -1;
  
  sz := GetItemSize;
  R := GetInnerRect; // consider left/top only, Height and width will be set here
  X := R.Left;
  Y := R.Top;
  c := 0;
  w := 0;
  saItem := False;
  for i := 0 to Items.Count -1 do
  begin
    Items[i].Rect := Rect(X, Y, X + sz.cx, Y + sz.cy);
    Inc(c);
    if (i < Items.Count - 1) then // not last item
    begin
      if (Items[i].ColorBoxPos = ipStandAlone) then
        saItem := True;

      if (c >= Columns) then
      begin
        w := max(w, X);
        X := R.Left;
        if saItem then
          Y := Y + sz.cy + ColumnGap
        else
          Y := Y + sz.cy;
        c := 0;
        saItem := False;
      end
      else
      begin
        X := X + sz.cx + ColumnGap;
      end;
    end;
  end;

  w := max(w, X);
  
  h := Y + sz.cy + R.Top;
  w := w + sz.cx + R.Left + 1;

  Height := h;
  Width := w;
end;

//------------------------------------------------------------------------------

function TCustomColorSelector.GetItemSize: TSize;
begin
  Result.cx := ColorBoxWidth;
  Result.cy := ColorBoxHeight;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.HandleKey(Key: Word; EditorEnabled, DroppedDown: Boolean);
var
  i: Integer;
begin
  if Enabled then
  begin
    case Key of
      VK_UP:
      begin
        if DroppedDown then
        begin
          if (ItemIndex < 0) then
            i := 0
          else
            i := ItemIndex - Columns;
          if (i >= 0) and (i < Items.Count) then
            ItemIndex := i;
        end
        else
          SelectPrevious;
      end;
      VK_DOWN:
      begin
        if DroppedDown then
        begin
          if (ItemIndex < 0) then
            i := 0
          else
            i := ItemIndex + Columns;
          if (i >= 0) and (i < Items.Count) then
            ItemIndex := i;
        end
        else
          SelectNext;
      end;
      VK_LEFT:
      begin
        if not EditorEnabled or DroppedDown then
          SelectPrevious;
      end;
      VK_RIGHT:
      begin
        if not EditorEnabled or DroppedDown then        
          SelectNext;
      end;
      VK_HOME, VK_PRIOR: SelectFirst;
      VK_END, VK_NEXT: SelectLast;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomColorSelector.GetItemRect(Index: Integer): TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].Rect;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  if (Index >= 0) and (Index < Items.Count) then
  begin
    R := GetItemRect(Index);
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

function TCustomColorSelector.ItemAtPos(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    P := Point(X, Y);
    if PtInRect(Items[i].Rect, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.DrawItems(aCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    DrawItem(i, aCanvas);
end;

//------------------------------------------------------------------------------

function TCustomColorSelector.GetInnerRect: TRect;
begin
  Result := ClientRect;
  Result := Rect(Result.Left + FOffSetX, Result.Top + FOffSetY, Result.Right - FOffSetX, Result.Bottom - FOffSetY);
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetColumnGap(const Value: Integer);
begin
  if (FColumnGap <> Value) then
  begin
    FColumnGap := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (ItemHot >= 0) then
    ItemHot := -1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SelectFirst;
begin
  if (Items.Count > 0) then
    ItemIndex := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SelectLast;
begin
  if (Items.Count > 0) then
    ItemIndex := Items.Count - 1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SelectNext;
begin
  if (ItemIndex + 1 < Items.Count) then
    ItemIndex := ItemIndex + 1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SelectPrevious;
begin
  if (ItemIndex - 1 >= 0) then
    ItemIndex := ItemIndex - 1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.HotNext;
begin
  if (ItemHot + 1 < Items.Count) then
    ItemHot := ItemHot + 1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.HotPrevious;
begin
  if (ItemHot - 1 >= 0) then
    ItemHot := ItemHot - 1;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetColorBoxAppearance(
  const Value: TColorBoxAppearance);
begin
  FColorBoxAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetColorBoxHeight(const Value: Integer);
begin
  if (FColorBoxHeight <> Value) then
  begin
    FColorBoxHeight := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetColorBoxWidth(const Value: Integer);
begin
  if (FColorBoxWidth <> Value) then
  begin
    FColorBoxWidth := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetColumns(const Value: Integer);
begin
  if (FColumns <> Value) then
  begin
    FColumns := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetItemColorStyle(
  const Value: TSelectionColorStyle);
begin
  if (FItemColorStyle <> Value) then
  begin
    FItemColorStyle := Value;
    case FItemColorStyle of
      scOffice2007:
      begin
        ColorBoxAppearance.OuterBorderColorSelected := clRed;
        ColorBoxAppearance.InnerBorderColorSelected := RGB(255, 226, 148);
        ColorBoxAppearance.OuterBorderColorHot := RGB(242, 148, 54);
        ColorBoxAppearance.InnerBorderColorHot := RGB(255, 226, 148);
        ColorBoxAppearance.BorderColor := RGB(197, 197, 197);
      end;
      scWindowsVista:
      begin
        ColorBoxAppearance.OuterBorderColorSelected := clRed;
        ColorBoxAppearance.InnerBorderColorSelected := RGB(255, 226, 148);
        ColorBoxAppearance.OuterBorderColorHot := RGB(242, 148, 54);
        ColorBoxAppearance.InnerBorderColorHot := RGB(255, 226, 148);
        ColorBoxAppearance.BorderColor := RGB(197, 197, 197);
      end;
      scWindows7:
      begin
        ColorBoxAppearance.OuterBorderColorSelected := clRed;
        ColorBoxAppearance.InnerBorderColorSelected := RGB(255, 226, 148);
        ColorBoxAppearance.OuterBorderColorHot := RGB(242, 148, 54);
        ColorBoxAppearance.InnerBorderColorHot := RGB(255, 226, 148);
        ColorBoxAppearance.BorderColor := RGB(197, 197, 197);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetItems(const Value: TColorBoxItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.CreateDefaultColors;
begin
  FItems.Clear;

  //--- Top 
  with Items.Add do
  begin
    Color := clWhite;
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := clBlack;
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := $00E1ECEE;
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(31, 73, 125);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(79, 129, 189);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(192, 80, 77);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(155, 187, 89);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(128, 100, 162);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(75, 172, 198);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(247, 150, 70);
    ColorBoxPos := ipStandAlone;
  end;
  with Items.Add do
  begin
    Color := RGB(242, 242, 242);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(127, 127, 127);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(221, 217, 195);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(194, 206, 218); // RGB(198, 217, 140); 
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(219, 229, 241);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(242, 220, 219);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(235, 241, 221);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(229, 224, 236);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(219, 238, 243);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(253, 234, 218);
    ColorBoxPos := ipTop;
  end;
  with Items.Add do
  begin
    Color := RGB(216, 216, 216);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(89, 89, 89);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(196, 189, 151);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(141, 179, 226);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(184, 204, 228);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(229, 185, 183);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(215, 227, 188);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(204, 193, 217);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(183, 221, 232);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(251, 213, 181);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(191, 191, 191);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(63, 63, 63);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(147, 137, 83);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(84, 141, 212);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(149, 179, 215);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(217, 150, 148);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(195, 214, 155);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(178, 162, 199);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(146, 205, 220);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(250, 192, 143);
    ColorBoxPos := ipMiddle;
  end;

  with Items.Add do
  begin
    Color := RGB(165, 165, 165);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(38, 38, 38);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(73, 68, 41);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(23, 54, 93);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(54, 96, 146);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(149, 55, 52);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(118, 146, 60);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(95, 73, 122);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(49, 133, 155);
    ColorBoxPos := ipMiddle;
  end;
  with Items.Add do
  begin
    Color := RGB(227, 108, 9);
    ColorBoxPos := ipMiddle;
  end;


  //--- Bottom
  with Items.Add do
  begin
    Color := RGB(127, 127, 127);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(12, 12, 12);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(29, 27, 16);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(15, 36, 62);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(36, 64, 97);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(99, 36, 35);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(79, 97, 40);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(63, 49, 81);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(32, 88, 103);
    ColorBoxPos := ipBottom;
  end;
  with Items.Add do
  begin
    Color := RGB(151, 72, 6);
    ColorBoxPos := ipBottom;
  end;

end;

//------------------------------------------------------------------------------

procedure TCustomColorSelector.SetSelectedColor(const Value: TColor);
var
  i: Integer;
  Found: Boolean;
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    if (FSelectedColor = clNone) then
      ItemIndex := -1;

    Found := False;
    for i := 0 to Items.Count - 1 do
    begin
      if (Items[i].Color = FSelectedColor) then
      begin
        ItemIndex := i;
        Found := True;
        Break;
      end;
    end;

    if not Found then
      ItemIndex := -1;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvColorPickerDropDown }

constructor TAdvColorPickerDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FColorCubePanel := nil;
  FSpectrumPanel := nil;
  FColorSelector := nil;
  FColorSelectionStyle := csList;
  FColorBoxWidth := DD_COLORBOXHEIGHT;
  FColorBoxHeight := DD_COLORBOXWIDTH;
  FColors := TColorItems.Create(Self);
  FColors.OnChange := OnItemsChanged;
  FItemAppearance := TItemAppearance.Create(Self);
  FColumns := 1;
  FItemIndex := -1;
  FOldItemIndex := FItemIndex;
  FLayout := ilCaptionRight;
  AutoSize := False;
  EditorEnabled := False;
  DropDownEnabled := True;
  FSelectedColor := clNone;
  FOldSelectedColor := FSelectedColor;
  FShowText := True;
  FCurSearch := '';
  FKeyTimer := TTimer.Create(Self);
  FKeyTimer.Enabled := False;
  FKeyTimer.Interval := 500;
  FKeyTimer.OnTimer := OnKeyTimerTime;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  DropDownHeight := 200;
  DropDownHeader.Visible := False;
  DropDownFooter.Visible := False;               
  Cursor := crArrow;               
end;

//------------------------------------------------------------------------------

destructor TAdvColorPickerDropDown.Destroy;
begin
  FKeyTimer.Enabled := False;
  FKeyTimer.Free;
  FColors.Free;
  FItemAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateDropDownForm;
begin
  inherited;
  case ColorSelectionStyle of
    csList:
    begin
      CreateItemListPanel;
    end;
    csDiscrete:
    begin
      CreateColorSelectorPanel;
    end;
    csColorCube:
    begin
      CreateColorCubePanel;
    end;
    csSpectrum:
    begin
      CreateSpectrumPanel;
    end;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetSelectorProperties;
begin
  if Assigned(FItemSelector) then
  begin
    FItemSelector.OnItemSelect := nil;
    FItemSelector.Columns := FColumns;
    FItemSelector.Images := Images;
    FItemSelector.ItemLayout := FLayout;
    FItemSelector.Color := DropDownColor;
    AssignedItemsToItemSelector;
    FItemSelector.ColorBoxHeight := ColorBoxHeight;
    FItemSelector.ColorBoxWidth := ColorBoxWidth;
    //FItemSelector.ItemAppearance.ColorStyle := SelectionColorStyle;
    FItemSelector.ItemAppearance.Assign(FItemAppearance);
    TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
    FItemSelector.ItemIndex := FItemIndex;
    FItemSelector.OnItemSelect := OnSelectorItemSelect;
    FItemSelector.ShowHint := ShowHint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.BeforeDropDown;
begin
  inherited;
  case ColorSelectionStyle of
    csList:
    begin
      CreateItemListPanel;
      if Assigned(FItemSelector) then
      begin
        if Assigned(FColorCubePanel) then
          FColorCubePanel.Visible:= false;
        if Assigned(FSpectrumPanel) then
          FSpectrumPanel.Visible:= false;
        if Assigned(FColorSelector) then
          FColorSelector.Visible := False;

        SetSelectorProperties;
        if Assigned(OnDrawColor) then
          FItemSelector.OnDrawItem := OnItemSelectorDrawItem
        else
          FItemSelector.OnDrawItem := nil;
        FOldItemIndex := ItemIndex;
      end;
    end;
    csDiscrete:
    begin
      CreateColorSelectorPanel;
      if Assigned(FColorSelector) then
      begin
        if Assigned(FItemSelector) then
          FItemSelector.Visible:= false;
        if assigned(FSpectrumPanel) then
          FSpectrumPanel.Visible:= false;
        if Assigned(FColorCubePanel) then
          FColorCubePanel.Visible := False;

        FColorSelector.SelectedColor := FSelectedColor;
        FColorSelector.UpdateRectAndSize;
        FColorSelector.OnItemSelect := ColorSelectorOnItemSelect;
        FColorSelector.ItemColorStyle := SelectionColorStyle;
        if not FColorSelector.Visible then
          FColorSelector.Visible:= true;
      end;
    end;
    csColorCube:
    begin
      CreateColorCubePanel;
      if Assigned(FColorCubePanel) then
      begin
        if Assigned(FItemSelector) then
          FItemSelector.Visible:= false;
        if assigned(FSpectrumPanel) then
          FSpectrumPanel.Visible:= false;
        if Assigned(FColorSelector) then
          FColorSelector.Visible := False;

        FColorCubePanel.SelectedColor := FSelectedColor;
        FColorCubePanel.Initialize;
        FColorCubePanel.SetItemsPosition;
        FColorCubePanel.OnSelect := CubePanelOnSelect;
        if not FColorCubePanel.Visible then
          FColorCubePanel.Visible:= true;
      end;
    end;
    csSpectrum:
    begin
      CreateSpectrumPanel;
      if Assigned(FSpectrumPanel) then
      begin
        if Assigned(FItemSelector) then
          FItemSelector.Visible:= false;
        if assigned(FColorCubePanel) then
          FColorCubePanel.Visible:= False;
        if Assigned(FColorSelector) then
          FColorSelector.Visible := False;

        FSpectrumPanel.SelectedColor := FSelectedColor;
        FSpectrumPanel.SetItemsPosition;
        FSpectrumPanel.OnSelect := SpectrumPanelOnSelect;
        if not FSpectrumPanel.Visible then
          FSpectrumPanel.Visible:= true;
      end;
    end;
  end;

  FOldSelectedColor := SelectedColor;
end;

procedure TAdvColorPickerDropDown.BeginUpdate;
begin
  inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.AddDefaultColors;
begin
  Colors.Clear;
  with Colors.Add do
  begin
    Caption := 'Black';
    Color := clBlack;
  end;
  with Colors.Add do
  begin
    Caption := 'Maroon';
    Color := clMaroon;
  end;
  with Colors.Add do
  begin
    Caption := 'Green';
    Color := clGreen;
  end;
  with Colors.Add do
  begin
    Caption := 'Olive';
    Color := clOlive;
  end;
  with Colors.Add do
  begin
    Caption := 'Navy';
    Color := clNavy;
  end;
  with Colors.Add do
  begin
    Caption := 'Purple';
    Color := clPurple;
  end;
  with Colors.Add do
  begin
    Caption := 'Teal';
    Color := clTeal;
  end;
  with Colors.Add do
  begin
    Caption := 'Gray';
    Color := clGray;
  end;
  with Colors.Add do
  begin
    Caption := 'Silver';
    Color := clSilver;
  end;
  with Colors.Add do
  begin
    Caption := 'Red';
    Color := clRed;
  end;
  with Colors.Add do
  begin
    Caption := 'Lime';
    Color := clLime;
  end;
  with Colors.Add do
  begin
    Caption := 'Yellow';
    Color := clYellow;
  end;
  with Colors.Add do
  begin
    Caption := 'Blue';
    Color := clBlue;
  end;
  with Colors.Add do
  begin
    Caption := 'Fuchsia';
    Color := clFuchsia;
  end;
  with Colors.Add do
  begin
    Caption := 'Aqua';
    Color := clAqua;
  end;
  with Colors.Add do
  begin
    Caption := 'White';
    Color := clWhite;
  end;
  {
  with Colors.Add do
  begin
    Caption := 'MoneyGreen';
    Color := clMoneyGreen;
  end;
  with Colors.Add do
  begin
    Caption := 'SkyBlue';
    Color := clSkyBlue;
  end;
  with Colors.Add do
  begin
    Caption := 'Cream';
    Color := clCream;
  end;
  with Colors.Add do
  begin
    Caption := 'MedGray';
    Color := clMedGray;
  end;
  }
end;

procedure TAdvColorPickerDropDown.AssignedItemsToItemSelector;
var
  i: Integer;
begin
  if not Assigned(FItemSelector) then
    Exit;

  FItemSelector.Items.Clear;

  for i := 0 to Colors.Count - 1 do
  begin
    with FItemSelector.Items.Add do
    begin
      Caption := Colors[i].Caption;
      Color := Colors[i].Color;
      Hint := Colors[i].Hint;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.UpdateDropDownSize;
var
  sz: TSize;
  h: Integer;
begin
  case ColorSelectionStyle of
    csList:
    begin
      if not Assigned(FItemSelector) then
      begin
        inherited;
        Exit;
      end;

      FItemSelector.Align := alNone;
      sz := TInternalItemSelector(FItemSelector).GetItemPanelSize;
      if (DropDownWidth <= 0) then
        FItemSelector.Width := sz.cx;

      if (DropDownHeight <= 0) then
        FItemSelector.Height := sz.cy;

      inherited;

      if FItemSelector.VertScrollBar.IsScrollBarVisible then
      begin
        FDropDownForm.Width := FDropDownForm.Width + GetSystemMetrics(SM_CXVSCROLL);
      end;
    end;
    csDiscrete, csColorCube, csSpectrum:
    begin
      if Assigned(Control) and Control.Visible then
        FDropDownForm.Width := Control.Width + DropDownBorderWidth * 2
      else
        FDropDownForm.Width := Self.Width;

      h := DropDownBorderWidth * 2;
      if DropDownHeader.Visible then
        h := h + DropDownHeader.Height;
      if DropDownFooter.Visible then
        h := h+ DropDownFooter.Height;
      if Assigned(Control) and Control.Visible then
        h := h + Control.Height;
      FDropDownForm.Height := h;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;
  if Canceled then
  begin
    if Assigned(FItemSelector) and (ColorSelectionStyle = csList) then
      ItemIndex := FOldItemIndex;
    if Assigned(FColorSelector) and (ColorSelectionStyle = csDiscrete) then
      FColorSelector.SelectedColor := FOldSelectedColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnHideDropDown;
begin
  inherited;
  //if Assigned(FItemSelector) then
    //FItemSelector.OnItemSelect := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnKeyTimerTime(Sender: TObject);
begin
  FKeyTimer.Enabled := False;
  FCurSearch := '';
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.WMKeyDown(var Msg: TWMKeyDown);
var
  IsAlt, IsCtrl, NewSel: Boolean;
  i: Integer;
begin
  NewSel := False;

  if Enabled and DroppedDown then
  begin
    if (Msg.CharCode = VK_RETURN) then
    begin
      if Assigned(FItemSelector) and (ColorSelectionStyle = csList) then
      begin
        if (FItemSelector.ItemHot >= 0) then
        begin
          FItemSelector.ItemIndex := FItemSelector.ItemHot;
          NewSel := True;
        end;
      end;

      if Enabled and Assigned(FColorSelector) and (ColorSelectionStyle = csDiscrete) then
      begin
        if (FColorSelector.ItemHot >= 0) then
        begin
          FColorSelector.ItemIndex := FColorSelector.ItemHot;
          NewSel := True;
        end;
      end;
    end;
  end;

  inherited;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);
  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;
  if Enabled and not IsAlt and Assigned(FItemSelector) and (ColorSelectionStyle = csList) then
  begin
    case Msg.CharCode of
      VK_UP:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          if (FItemSelector.ItemIndex < 0) then
            i := 0
          else
            i := FItemSelector.ItemIndex - Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
        end
        else
          SelectPrevious;
      end;
      VK_DOWN:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          if (FItemSelector.ItemIndex < 0) then
            i := 0
          else
            i := FItemSelector.ItemIndex + Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
        end
        else
          SelectNext;
      end;
      VK_LEFT:
      begin
        if not EditorEnabled and DroppedDown then
        begin
          FInternalCall := True;
          FItemSelector.SelectPrevious;
          FInternalCall := False;
        end;
      end;
      VK_RIGHT:
      begin
        if not EditorEnabled and DroppedDown then
        begin
          FInternalCall := True;
          FItemSelector.SelectNext;
          FInternalCall := False;
        end;
      end;
      VK_HOME:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          FInternalCall := True;
          FItemSelector.SelectFirst;
          FInternalCall := False;
        end
        else
          SelectFirst;
      end;
      VK_END:
      begin
        if (DroppedDown) and Assigned(FItemSelector) then
        begin
          FInternalCall := True;
          FItemSelector.SelectLast;
          FInternalCall := False;
        end
        else
          SelectLast;
      end;
      VK_PRIOR:
      begin
        if Assigned(FItemSelector) then
        begin
          if (FItemSelector.Items.Count <> FColors.Count) then
            SetSelectorProperties;
          i := FItemSelector.GetVisibleItemCount;
          if (i > 0) then
          begin
            i := Max(FItemSelector.ItemIndex - i, 0);
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
          end;        
        end;
      end;
      VK_NEXT:
      begin
        if Assigned(FItemSelector) then
        begin         
          if (FItemSelector.Items.Count <> FColors.Count) then          
            SetSelectorProperties;
          i := FItemSelector.GetVisibleItemCount;
          if (i > 0) then
          begin
            i := Min(FItemSelector.ItemIndex + i, FColors.Count - 1);
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
          end;         
        end;
      end;
      VK_RETURN:
      begin
        if not NewSel and Assigned(FOnSelect) then
          FOnSelect(Self);
      end;
      else
      begin
        if not IsAlt and not IsCtrl and Assigned(FItemSelector) then
        begin
          FCurSearch := FCurSearch + Char(Msg.CharCode);
          FInternalCall := True;
          FItemSelector.LookupItem(FCurSearch);
          ItemIndex := FItemSelector.ItemIndex;
          FInternalCall := False;
          FKeyTimer.Enabled := True;
        end;
      end;
    end;
  end;

  if Enabled and not IsAlt and Assigned(FColorSelector) and (ColorSelectionStyle = csDiscrete) then
  begin
    FInternalCall := True;
    FColorSelector.HandleKey(Msg.CharCode, EditorEnabled, DroppedDown);

    FInternalCall := False;
    if (Msg.CharCode = VK_RETURN) then
    begin
      if not NewSel and Assigned(FOnSelect) then
        FOnSelect(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.WMPaint(var Message: TWMPaint);
begin
  inherited;
  DrawSelectedItem;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.DrawBackGround;
begin
  inherited;
  DrawSelectedItem;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.DrawSelectedItem;
var
  DC: HDC;
  Canvas: TCanvas;
  R, R1: TRect;
  //ts: TSize;
  s: string;
begin
  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    R := Rect(1,1,Width - 2 - DropDownButtonWidth ,Height - 2);

    if Assigned(OnDrawSelectedColor) then
      OnDrawSelectedColor(Self, Canvas, R)
    else if (FSelectedColor <> clNone) then
    begin
      if not EditorEnabled and Focused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Pen.Color := clHighlight;
        R1 := R;
        InflateRect(R1,-2,-2);
        Canvas.Rectangle(R1);
      end;

      R1.Left := R.Left + 4;
      R1.Top := R.Top + (Height - ColorBoxHeight) div 2;
      R1.Right := R1.Left + ColorBoxWidth;
      R1.Bottom := R1.Top + ColorBoxHeight;
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := FSelectedColor;
      Canvas.Rectangle(R1);

      if ShowText then
      begin
        Canvas.Font.Assign(Self.Font);
        if not EditorEnabled and Focused then
          Canvas.Font.Color := clHighlightText;

        s := SelectedColorText;
        R1 := Rect(R1.Right + 4, R.Top, R.Right, R.Bottom);

        if Focused then
          Canvas.Brush.Color := clHighLight
        else
          Canvas.Brush.Color := Color;
        DrawText(Canvas.Handle, PChar(s), length(s), R1, DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS);

        (*
        if (s <> '') then
        begin
          R1 := Rect(R1.Right + 4, R.Top, R.Right, R.Bottom);
          ts := DrawHTMLEX(Canvas, s, R1, nil, False, Handle);

          R1.Top := R1.Top + (R1.Bottom - R1.Top - ts.cy) div 2;
          ts := DrawHTMLEX(Canvas, s, R1, nil, True, Handle);
        end;
        *)

      end;
    end;

    Canvas.Free;
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TAdvColorPickerDropDown.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);

    if FUpdateCount = 0 then
      AssignedItemsToItemSelector;
  end;
end;

function TAdvColorPickerDropDown.GetSelectedColorText: string;
var
  rgb: DWORD;
  cr,cb,cg: byte;

begin
  if (ColorSelectionStyle = csList) and (ItemIndex >= 0) then
    Result := Colors[ItemIndex].Caption
  else
  begin
    case FColorValueText of
    cvtNone: Result := '';
    cvtHex: Result := IntToHex(FSelectedColor,6);
    cvtHTML:
      begin
        rgb := ColorToRGB(FSelectedColor);
        cr := rgb and $0000FF;
        cg := (rgb and $00FF00) shr 8;
        cb := (rgb and $FF0000) shr 16;
        Result := '#'+inttohex(cr,2)+inttohex(cg,2)+inttohex(cb,2);
      end;
    {$IFDEF DELPHI2006_LVL}
    cvtWebName: Result := ColorToWebColorName(FselectedColor);
    {$ENDIF}
    {$IFNDEF DELPHI2006_LVL}
    cvtWebName: Result := '#' + inttohex(FSelectedColor,6);
    {$ENDIF}
    cvtRGB:
      begin
        rgb := ColorToRGB(FSelectedColor);
        cr := rgb and $0000FF;
        cg := (rgb and $00FF00) shr 8;
        cb := (rgb and $FF0000) shr 16;
        Result := 'Red:'+inttostr(cr)+',Green:'+inttostr(cg)+',Blue:'+inttostr(cb);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.HandleMouseWheelDown;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if Assigned(FItemSelector) and (ColorSelectionStyle = csList) then
    begin
      if DroppedDown then
        FItemSelector.HotNext
      else
      begin
        FInternalCall := True;
        FItemSelector.SelectNext;
        FInternalCall := False;
      end;
    end
    else if Assigned(FColorSelector) and (ColorSelectionStyle = csDiscrete) then
    begin
      if DroppedDown then
        FColorSelector.HotNext
      else
      begin
        FInternalCall := True;
        FColorSelector.SelectNext;
        FInternalCall := False;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.HandleMouseWheelUp;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if Assigned(FItemSelector) and (ColorSelectionStyle = csList) then
    begin
      if DroppedDown then
        FItemSelector.HotPrevious
      else
      begin
        FInternalCall := True;
        FItemSelector.SelectPrevious;
        FInternalCall := False;
      end;
    end
    else if Assigned(FColorSelector) and (ColorSelectionStyle = csDiscrete) then
    begin
      if DroppedDown then
        FColorSelector.HotPrevious
      else
      begin
        FInternalCall := True;
        FColorSelector.SelectPrevious;
        FInternalCall := False;
      end;
    end;
  end;
end;

procedure TAdvColorPickerDropDown.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TAdvColorPickerDropDown.Loaded;
begin
  inherited;
  if FDesignTime then
    AddDefaultColors;
  AssignedItemsToItemSelector;
  SetSelectorProperties;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetEditRect;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetSelectionColorStyle(
  const Value: TSelectionColorStyle);
begin
  inherited;

  if Assigned(FItemSelector) then
    TInternalItemAppearance(FItemSelector.ItemAppearance).ColorStyle := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetColumns(const Value: Integer);
begin
  FColumns := Value;
end;

procedure TAdvColorPickerDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;

  SetAppearanceStyle(ItemAppearance, AStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetItemIndex(const Value: Integer);
begin
  if {(FItemIndex <> Value) and} (Value < FColors.Count) then
  begin
    FItemIndex := Value;
    if (ItemIndex >= 0) then
      FSelectedColor := Colors[ItemIndex].Color;

    Invalidate;

    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetLayout(const Value: TItemLayout);
begin
  FLayout := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetColors(const Value: TColorItems);
begin
  FColors.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnItemsChanged(Sender: TObject);
begin
//  AssignedItemsToItemSelector;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnItemSelectorDrawItem(Sender: TObject;
  Canvas: TCanvas; R: TRect; Index: Integer);
begin
  if Assigned(OnDrawColor) then
    FOnDrawColor(Self, Canvas, R, Index);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnSelectorItemSelect(Sender: TObject);
begin
  if Assigned(FItemSelector) then
  begin
    ItemIndex := FItemSelector.ItemIndex;
    if not FInternalCall then    
      DoHideDropDown(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SelectFirst;
begin
  if (FColors.Count <= 0) then
    Exit;

  ItemIndex := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SelectLast;
begin
  if (FColors.Count <= 0) then
    Exit;

  ItemIndex := FColors.Count - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SelectNext;
begin
  if (FColors.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 1) < FColors.Count) then
      ItemIndex := ItemIndex + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SelectPrevious;
begin
  if (FColors.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 1) >= 0) then
      ItemIndex := ItemIndex - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetCenterControl;
begin
  inherited;

  if Assigned(FDropDownForm) and (ColorSelectionStyle in [csDiscrete, csColorCube, csSpectrum]) then
    FDropDownForm.Sizeable := False;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetColorBoxHeight(const Value: Integer);
begin
  FColorBoxHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetColorBoxWidth(const Value: Integer);
begin
  FColorBoxWidth := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetColorSelectionStyle(
  const Value: TColorSelectionStyle);
var
  sz: TSize;
  h: Integer;
begin
  if (FColorSelectionStyle <> Value) then
  begin
    FColorSelectionStyle := Value;
    ResetDropSize;

    if not (csDesigning in ComponentState) then
    begin
      case ColorSelectionStyle of
        csList:
        begin
          CreateItemListPanel;

          if Assigned(FItemSelector) then
          begin
            if Assigned(FColorCubePanel) then
              FColorCubePanel.Visible:= false;

            if Assigned(FSpectrumPanel) then
              FSpectrumPanel.Visible:= false;

            if Assigned(FColorSelector) then
              FColorSelector.Visible := False;

            FItemSelector.Visible := True;


            SetSelectorProperties;

            if Assigned(OnDrawColor) then
              FItemSelector.OnDrawItem := OnItemSelectorDrawItem
            else
              FItemSelector.OnDrawItem := nil;

            FOldItemIndex := ItemIndex;

            FItemSelector.Align := alNone;
            sz := TInternalItemSelector(FItemSelector).GetItemPanelSize;
            if (DropDownWidth <= 0) then
              FItemSelector.Width := sz.cx;

            if (DropDownHeight <= 0) then
              FItemSelector.Height := sz.cy;

            inherited UpdateDropDownSize;

            if FItemSelector.VertScrollBar.IsScrollBarVisible then
              FDropDownForm.Width := FDropDownForm.Width + GetSystemMetrics(SM_CXVSCROLL);

            FItemSelector.Align := alClient;
          end;
        end;
        csDiscrete:
        begin
          CreateColorSelectorPanel;
          if Assigned(FColorSelector) then
          begin
            FColorSelector.Align := alNone;
            if Assigned(FItemSelector) then
              FItemSelector.Visible:= false;
            if assigned(FSpectrumPanel) then
              FSpectrumPanel.Visible:= false;
            if Assigned(FColorCubePanel) then
              FColorCubePanel.Visible := False;

            FColorSelector.SelectedColor := FSelectedColor;
            FColorSelector.UpdateRectAndSize;
            FColorSelector.OnItemSelect := ColorSelectorOnItemSelect;
            FColorSelector.ItemColorStyle := SelectionColorStyle;
            if not FColorSelector.Visible then
              FColorSelector.Visible:= true;
          end;
        end;
        csColorCube:
        begin
          CreateColorCubePanel;
          if Assigned(FColorCubePanel) then
          begin
            FColorCubePanel.Align := alNone;
            if Assigned(FItemSelector) then
              FItemSelector.Visible:= false;
            if assigned(FSpectrumPanel) then
              FSpectrumPanel.Visible:= false;
            if Assigned(FColorSelector) then
              FColorSelector.Visible := False;

            FColorCubePanel.SelectedColor := FSelectedColor;
            FColorCubePanel.Initialize;
            FColorCubePanel.SetItemsPosition;
            FColorCubePanel.OnSelect := CubePanelOnSelect;
            if not FColorCubePanel.Visible then
              FColorCubePanel.Visible:= true;
          end;
        end;
        csSpectrum:
        begin
          CreateSpectrumPanel;
          if Assigned(FSpectrumPanel) then
          begin
            FSpectrumPanel.Align := alNone;
            if Assigned(FItemSelector) then
              FItemSelector.Visible:= false;
            if assigned(FColorCubePanel) then
              FColorCubePanel.Visible:= False;
            if Assigned(FColorSelector) then
              FColorSelector.Visible := False;

            FSpectrumPanel.SelectedColor := FSelectedColor;
            FSpectrumPanel.SetItemsPosition;
            FSpectrumPanel.OnSelect := SpectrumPanelOnSelect;
            if not FSpectrumPanel.Visible then
              FSpectrumPanel.Visible:= true;
          end;
        end;
      end;

      FOldSelectedColor := SelectedColor;

      if (ColorSelectionStyle in [csDiscrete, csColorCube, csSpectrum]) then
      begin
        Control.Align := alNone;
        if Assigned(Control) and Control.Visible then
          FDropDownForm.Width := Control.Width + DropDownBorderWidth * 2
        else
          FDropDownForm.Width := Self.Width;

        h := DropDownBorderWidth * 2;
        if DropDownHeader.Visible then
          h := h + DropDownHeader.Height;
        if DropDownFooter.Visible then
          h := h+ DropDownFooter.Height;
        if Assigned(Control) and Control.Visible then
          h := h + Control.Height;
        FDropDownForm.Height := h;

        Control.Align := alClient;
      end;

      FDropDownForm.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CubePanelOnSelect(Sender: TObject);
begin
  if Assigned(FColorCubePanel) then
    SelectedColor := FColorCubePanel.SelectedColor;
  ItemIndex := -1;
  DoHideDropDown(False);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SpectrumPanelOnSelect(Sender: TObject);
begin
  if Assigned(FSpectrumPanel) then
    SelectedColor := FSpectrumPanel.SelectedColor;
  ItemIndex := -1;
  DoHideDropDown(False);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetSelectedColor(const Value: TColor);
var
  i: integer;
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := ColorToRGB(Value);

    for I := 0 to Colors.Count - 1 do
    begin
      if FSelectedColor = ColorToRGB(Colors[I].Color) then
      begin
        ItemIndex := i;
        break;
      end;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.OnDestroyDropDownForm;
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    FItemSelector := nil;
    FColorCubePanel := nil;
    FSpectrumPanel := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateColorCubePanel;
begin
  if not Assigned(FColorCubePanel) then
  begin
    FColorCubePanel := TAdvColorCubePanel.Create(Self);
    FColorCubePanel.Parent := FDropDownForm;
  end;
  FColorCubePanel.Left := 0;
  FColorCubePanel.Top := 0;
  FColorCubePanel.Color := DropDownColor;
  Control := FColorCubePanel;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateItemListPanel;
begin
  if not Assigned(FItemSelector) then
  begin
    FItemSelector := TAdvCustomItemSelector.Create(Self);
    FItemSelector.Parent := FDropDownForm;
    //FItemSelector.AdvDropDown := Self;
  end;
  FItemSelector.SelectorType := stColor;
  FItemSelector.Left := 0;
  FItemSelector.Top := 0;
  FItemSelector.Height := 150;
  FItemSelector.Color := DropDownColor;
  TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
  Control := FItemSelector;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateSpectrumPanel;
begin
  if not Assigned(FSpectrumPanel) then
  begin
    FSpectrumPanel := TAdvColorSpectrumPanel.Create(Self);
    FSpectrumPanel.Parent := FDropDownForm;
  end;
  FSpectrumPanel.Left := 0;
  FSpectrumPanel.Top := 0;
  FSpectrumPanel.Color := DropDownColor;
  Control := FSpectrumPanel;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateWnd;
begin
  inherited;

  if FDesignTime and (Colors.Count = 0) then
  begin
    FDesignTime := False;
    AddDefaultColors;
    AssignedItemsToItemSelector;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.ColorSelectorOnItemSelect(
  Sender: TObject);
begin
  if Assigned(FColorSelector) then
    SelectedColor := FColorSelector.SelectedColor;
  ItemIndex := -1;
  if not FInternalCall then
    DoHideDropDown(False);
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.CreateColorSelectorPanel;
begin
  if not Assigned(FColorSelector) then
  begin
    FColorSelector := TCustomColorSelector.Create(Self);
    FColorSelector.Parent := FDropDownForm;
    FColorSelector.CreateDefaultColors;
  end;
  FColorSelector.Left := 0;
  FColorSelector.Top := 0;
  FColorSelector.Color := DropDownColor;
  FColorSelector.UpdateRectAndSize;
  Control := FColorSelector;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetShowText(const Value: Boolean);
begin
  if (FShowText <> Value) then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorPickerDropDown.SetItemAppearance(
  const Value: TItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TColorBoxAppearance }

procedure TColorBoxAppearance.Assign(Source: TPersistent);
begin
  if (Source is TColorBoxAppearance) then
  begin
    FBorderColor := (Source as TColorBoxAppearance).BorderColor;
    FOuterBorderColorHot := (Source as TColorBoxAppearance).OuterBorderColorHot;
    FInnerBorderColorHot := (Source as TColorBoxAppearance).InnerBorderColorHot;
    FOuterBorderColorSelected := (Source as TColorBoxAppearance).OuterBorderColorSelected;
    FInnerBorderColorSelected := (Source as TColorBoxAppearance).InnerBorderColorSelected;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TColorBoxAppearance.Create;
begin
  inherited;
  FBorderColor := RGB(197, 197, 197);
  FOuterBorderColorHot := RGB(242, 148, 54);
  FInnerBorderColorHot := RGB(255, 226, 148);
  FOuterBorderColorSelected := clRed;
  FInnerBorderColorSelected := RGB(255, 226, 148);
end;

//------------------------------------------------------------------------------

destructor TColorBoxAppearance.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TColorBoxAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

end.
