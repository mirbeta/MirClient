{*************************************************************************}
{ TAdvSmoothCapacityBar component                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2013                                              }
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

unit AdvSmoothCapacityBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, GDIPFill, AdvGDIP
  ;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 6; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  //v1.0.0.0 : First Release
  //v1.0.0.1 : Fixed : Issue with GetFreeSpace declaration in C++Builder
  //v1.0.1.0 : New: Appearance.CapacityTextShadowColor and Appearance.LegenTextShadowColor added
  //         : Improved : appearance of free space legend indication made consistent with taken space legend indication
  //v1.0.1.1 : Fixed : Issue with textrendering
  //         : Fixed : Issue with shadow colors = clNone
  //         : Fixed : Issue with text width
  //v1.0.1.2 : Fixed : Issue with rendering divisions
  //v1.0.1.3 : Fixed : System out of Resources error with height = 0;
  //v1.0.1.4 : Fixed : Issue with drawing when bar size gets 0
  //v1.0.2.0 : New : LegendPos lpNone added
  //         : New : When CapacityDescription is an empty string, no description will be drawn
  //         : Improved : Drawing precision
  //         : Improved : Property initialization
  //v1.0.3.0 : New : AutoFormatValues property to automatically format the added values in bytes to bytes, Kb, Mb or Gb
  //v1.0.5.0 : New : ShowLegend and ShowTotal properties to optionally show/hide the legend and total values
  //v1.0.6.0 : Improved : ShowFree property to show / hide the free space indication


type
  TLegendFormatEvent = procedure (Sender: TObject; Index: Integer; var Format: string; var Value: Double) of object;
  TCapacityItemEvent = procedure (Sender: TObject; Index: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothCapacityBar = class(TGraphicControl)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FItems: TCapacityItems;
    FAppearance: TGDIPCapacityBar;
    FTotalCapacity: Double;
    FCapacityDescription: string;
    FFreeDescription: string;
    FOnGetLegendFormat: TLegendFormatEvent;
    FHintItem: TCapacityItem;
    FOnItemMouseLeave: TCapacityItemEvent;
    FOnItemClick: TCapacityItemEvent;
    FOnItemMouseEnter: TCapacityItemEvent;
    FAntiAlias: TAntiAlias;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnDeleteItem(Sender: TObject; Index: integer);
    procedure OnCreateItem(Sender: TObject; Index: integer);
    procedure OnAppearanceChanged(Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAppearance(const Value: TGDIPCapacityBar);

    function MyClientRect: TRect;
    procedure SetItems(const Value: TCapacityItems);
    procedure SetTotalCapacity(const Value: Double);
    procedure SetCapacityDescription(const Value: string);
    procedure SetFreeDescription(const Value: string);
    procedure GetLegendFormatEvent(Sender: TObject; Item: TCapacityItem; var Format: string; var Value: Double);
    function GetFreeSpaceValue: Double;
    procedure UpdateReflection;
    procedure SetAntiAlias(const Value: TAntiAlias);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function PtOnItem(X, Y: Integer): TCapacityItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetVersionNr: Integer;
    property FreeSpaceValue: Double read GetFreeSpaceValue;
  published
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Version: string read GetVersion write SetVersion stored false;
    property Appearance: TGDIPCapacityBar read FAppearance write SetAppearance;
    property Items: TCapacityItems read FItems write SetItems;
    property CapacityDescription: string read FCapacityDescription write SetCapacityDescription;
    property FreeDescription: string read FFreeDescription write SetFreeDescription;
    property TotalCapacity: Double read FTotalCapacity write SetTotalCapacity;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnItemClick: TCapacityItemEvent read FOnItemClick write FOnItemClick;
    property OnItemMouseEnter: TCapacityItemEvent read FOnItemMouseEnter write FOnItemMouseEnter;
    property OnItemMouseLeave: TCapacityItemEvent read FOnItemMouseLeave write FOnItemMouseLeave;
    property OnGetLegendFormat: TLegendFormatEvent read FOnGetLegendFormat write FOnGetLegendFormat;
  end;


implementation

{$IFDEF DELPHI9_LVL}
uses
  Types;
{$ENDIF}

//------------------------------------------------------------------------------

type
  TAccessCapacityItems = class(TCapacityItems);
  TAccessAppearance = class(TGDIPCapacityBar);

{ TAdvSmoothCapacityBar }

constructor TAdvSmoothCapacityBar.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  FAppearance := TGDIPCapacityBar.Create;
  FAppearance.OnChange := OnAppearanceChanged;
  TAccessAppearance(FAppearance).OnGetLegendFormat := GetLegendFormatEvent;
  FItems := TCapacityItems.Create(Self);
  FItems.OnChange := OnItemsChanged;
  FItems.OnDeleteItem := OnDeleteItem;
  FItems.OnCreateItem := OnCreateItem;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FCapacityDescription := 'Capacity';
    FFreeDescription := 'Free';
  end;
  FHintItem := nil;
  FTotalCapacity := 100;
  Height := 80;
  Width := 641;
  FAntiAlias := aaClearType;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothCapacityBar.Destroy;
begin
  FAppearance.Free;
  FItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

function TAdvSmoothCapacityBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCapacityBar.PtOnItem(X, Y: Integer): TCapacityItem;
begin
  Result := TAccessAppearance(Appearance).PtOnItem(Point(X, Y), Items);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  TAccessCapacityItems(Items).DownItem := PtOnItem(X, Y);
  if Assigned(TAccessCapacityItems(Items).DownItem) then
  begin
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: TCapacityItem;
begin
  inherited;

  if not (csDesigning in ComponentState) and Enabled then
  begin
    Item := PtOnItem(X, Y);
    if (FHintItem <> Item) then
    begin
      if Assigned(FOnItemMouseLeave) and Assigned(FHintItem) then
        FOnItemMouseLeave(Self, FHintItem.Index);

      if Assigned(FOnItemMouseEnter) and Assigned(Item) then
        FOnItemMouseEnter(Self, Item.Index);
    end;
    
    if ShowHint then
    begin
      if FHintItem <> Item then
      begin
        FHintItem := Item;
        Application.CancelHint;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TCapacityItem;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  Item := PtOnItem(X, Y);  
  if Assigned(TAccessCapacityItems(Items).DownItem) then
  begin
    if (Item = TAccessCapacityItems(Items).DownItem) and Assigned(FOnItemClick) then
      FOnItemClick(Self, TAccessCapacityItems(Items).DownItem.Index);
    TAccessCapacityItems(Items).DownItem := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.CMHintShow(var Msg: TCMHintShow);
var
  hi: PHintInfo;
begin
  hi := Msg.HintInfo;

  if Assigned(FHintItem) then
  begin
    hi.HintStr := FHintItem.Hint;

    if (hi.HintStr = '') then
      hi.HintStr := Hint;
  end
  else
  begin
    hi.HintStr := Hint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.OnItemsChanged(Sender: TObject);
begin
  UpdateReflection;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.Paint;
var
  g: TGPGraphics;
  R: TRect;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  case AntiAlias of
    aaNone: ;
    aaClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
  end;
  R := MyClientRect;
  Appearance.Draw(g, R, TotalCapacity, Items, CapacityDescription, Appearance.CapacityFormat, FreeDescription, Appearance.FreeFormat, AntiAlias);
  g.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  UpdateReflection;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetCapacityDescription(const Value: string);
begin
  if (FCapacityDescription <> Value) then
  begin
    FCapacityDescription := Value;
    UpdateReflection;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetFreeDescription(const Value: string);
begin
  if (FFreeDescription <> Value) then
  begin
    FFreeDescription := Value;
    UpdateReflection;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetItems(const Value: TCapacityItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.OnAppearanceChanged(Sender: TObject);
begin
  UpdateReflection;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.OnCreateItem(Sender: TObject; Index: integer);
begin
  UpdateReflection;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.OnDeleteItem(Sender: TObject; Index: integer);
begin
  TAccessCapacityItems(Items).DownItem := nil;
  FHintItem := nil;
  if not (csDestroying in ComponentState) then
  begin
    UpdateReflection;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetTotalCapacity(const Value: Double);
begin
  if (FTotalCapacity <> Value) then
  begin
    FTotalCapacity := Value;
    UpdateReflection;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCapacityBar.MyClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------

function TAdvSmoothCapacityBar.GetFreeSpaceValue: Double;
begin
  Result := TAccessAppearance(FAppearance).FreeSpace;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.GetLegendFormatEvent(Sender: TObject;
  Item: TCapacityItem; var Format: string; var Value: Double);
begin
  if Assigned(Item) and Assigned(FOnGetLegendFormat) then
    FOnGetLegendFormat(Self, Item.Index, Format, Value);
end;

//------------------------------------------------------------------------------

function TAdvSmoothCapacityBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.UpdateReflection;
begin
  TAccessAppearance(FAppearance).UpdateReflection;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.SetAntiAlias(const Value: TAntiAlias);
begin
  if FAntiAlias <> Value then
  begin
    FAntiAlias := Value;
    invalidate;
  end;
end;

procedure TAdvSmoothCapacityBar.SetAppearance(const Value: TGDIPCapacityBar);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCapacityBar.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------

end.
