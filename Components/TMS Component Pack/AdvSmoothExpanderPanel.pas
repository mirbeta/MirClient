{**************************************************************************}
{ TAdvSmoothExpanderPanel component                                        }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2009 - 2015                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothExpanderPanel;

{$I TMSDEFS.inc}

interface

uses
  Windows, Messages, ExtCtrls, SysUtils, Classes, Graphics, Controls, StdCtrls, forms,
  Comobj, Activex, Math, AdvStyleIF, ImgList,
  GDIPPictureContainer, GDIPFill, AdvSmoothPanel, AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with panel starting collapsed
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.2.1 : Fixed : issue with expand / collapse after component loading
  // v1.0.2.2 : Fixed : issue with HTML when collapsing
  // v1.0.2.3 : Fixed : issue with expanding when collapsed ot creation
  // v1.0.2.4 : Fixed : issue with starting panel height when collapsed
  // v1.0.2.5 : Improved : Added readonly property ExpandedHeight
  // v1.0.2.6 : Fixed : Issue with minimumpanelheight when loading form
  // v1.0.2.7 : Fixed : Line visible when line is disabled
  // v1.0.2.8 : Fixed : Issue with height when setting Minimumpanelheight
  // v1.0.2.9 : Fixed : Issue with height in designtime when collapsed
  // v1.0.3.0 : Improved : Virtual method DrawExpander
  // v1.0.4.0 : New : OnStartExpandPanel, OnStartCollapsePanel, OnEndExpandPanel, OnEndCollapsePanel events
  // v1.0.4.1 : Fixed : Handling expand in scenarios with aligning, anchoring
  // v1.0.4.2 : Fixed : Issue with use of CanMove and expander button
  // v1.1.0.0 : New : Windows 10, Office 2016 styles added
  // v1.1.1.0 : New : OnBeforeDrawExpander and OnAfterDrawExpander events

type
  TAdvSmoothExpanderPanel = class;

  TAdvSmoothExpanderPanelShape = (esRounded, esRoundRect, esRect);

  TAdvSmoothExpanderPanelBeforeDrawExpander = procedure(Sender: TObject; AGraphics: TGPGraphics; ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvSmoothExpanderPanelAfterDrawExpander = procedure(Sender: TObject; AGraphics: TGPGraphics; ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothExpanderPanel = class(TAdvSmoothPanel, ITMSStyle)
  private
    FFlatStyle: Boolean;
    DisableLine: Boolean;
    FEnableResize, FDoAnimate: Boolean;
    FOldHeight, FExpHeight, FExpHeightTo: Double;
    FExpTimer: TTimer;
    FDesignTime, FHover, FOldHover, FDown: Boolean;
    FExpanderColor: TColor;
    FExpanded: Boolean;
    FExpanderShape: TAdvSmoothExpanderPanelShape;
    FExpanderHoverColor: TColor;
    FExpanderDownColor: TColor;
    FExpanderSize: integer;
    FExpanderBorderColor: TColor;
    FExpanderLocation: TAdvSmoothPanelLocation;
    FExpanderTop: integer;
    FExpanderLeft: integer;
    FAnimationFactor: integer;
    FMinimumPanelHeight: integer;
    FShowExpander: Boolean;
    FOnExpandPanel: TNotifyEvent;
    FOnStartExpandPanel: TNotifyEvent;
    FOnEndExpandPanel: TNotifyEvent;
    FOnStartCollapsePanel: TNotifyEvent;
    FOnEndCollapsePanel: TNotifyEvent;
    FOnBeforeDrawExpander: TAdvSmoothExpanderPanelBeforeDrawExpander;
    FOnAfterDrawExpander: TAdvSmoothExpanderPanelAfterDrawExpander;
    procedure SetExpanderColor(const Value: TColor);
    procedure SetExpanderDownColor(const Value: TColor);
    procedure SetExpanderHoverColor(const Value: TColor);
    procedure SetExpanderShape(const Value: TAdvSmoothExpanderPanelShape);
    procedure SetExpanderSize(const Value: integer);
    procedure SetExpanderBorderColor(const Value: TColor);
    procedure SetExpanderLocation(const Value: TAdvSmoothPanelLocation);
    procedure SetExpanderLeft(const Value: integer);
    procedure SetExpanderTop(const Value: integer);
    procedure SetExpanded(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetAnimationFactor(const Value: integer);
    procedure SetMinimumPanelHeight(const Value: integer);
    procedure SetShowExpander(const Value: Boolean);
  protected
    procedure Changed;
    procedure DrawExpander(g: TGPGraphics; r: TGPRectF; Down, Hover: Boolean); virtual;
    function InsideRect: TRect;
    function GetExpanderRect: TGPRectF;
    function GetVersionNr: integer; override;
    procedure GetPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; location: TAdvSmoothPanelLocation);
    procedure DoBeforeDrawExpander(AGraphics: TGPGraphics; ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawExpander(AGraphics: TGPGraphics; ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure AnimateExpand(Sender: TObject);
    procedure GDIPPaint(g: TGPGraphics); override;
    function IsResizeEnabled: Boolean;
    procedure SetResizeEnabled(Resize: boolean);
    function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadCustomProperty(Reader: TReader);
    procedure WriteCustomProperty(Writer: TWriter);
    function InMoveArea(pt: TPoint): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure Resize; override;
    procedure Loaded; override;
    procedure Collapse;
    procedure Expand;
    property ExpandedHeight: Double read FOldHeight write FOldHeight;
  published
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 4;
    property ShowExpander: Boolean read FShowExpander write SetShowExpander default true;
    property ExpanderBorderColor: TColor read FExpanderBorderColor write SetExpanderBorderColor default clBlack;
    property ExpanderLocation: TAdvSmoothPanelLocation read FExpanderLocation write SetExpanderLocation default plTopRight;
    property ExpanderLeft: integer read FExpanderLeft write SetExpanderLeft default 0;
    property ExpanderTop: integer read FExpanderTop write SetExpanderTop default 0;
    property ExpanderSize: integer read FExpanderSize write SetExpanderSize default 16;
    property Expanded: Boolean read FExpanded write SetExpanded default true;
    property ExpanderColor: TColor read FExpanderColor write SetExpanderColor default clWhite;
    property ExpanderDownColor: TColor read FExpanderDownColor write SetExpanderDownColor default clWhite;
    property ExpanderHoverColor: TColor read FExpanderHoverColor write SetExpanderHoverColor default clWhite;
    property ExpanderShape: TAdvSmoothExpanderPanelShape read FExpanderShape write SetExpanderShape default esRoundRect;
    property MinimumPanelHeight: integer read FMinimumPanelHeight write SetMinimumPanelHeight default 32;
    property OnExpandPanel: TNotifyEvent read FOnExpandPanel write FOnExpandPanel;
    property OnStartExpandPanel: TNotifyEvent read FOnStartExpandPanel write FOnStartExpandPanel;
    property OnEndExpandPanel: TNotifyEvent read FOnEndExpandPanel write FOnEndExpandPanel;
    property OnStartCollapsePanel: TNotifyEvent read FOnStartCollapsePanel write FOnStartCollapsePanel;
    property OnEndCollapsePanel: TNotifyEvent read FOnEndCollapsePanel write FOnEndCollapsePanel;
    property OnBeforeDrawExpander: TAdvSmoothExpanderPanelBeforeDrawExpander read FOnBeforeDrawExpander write FOnBeforeDrawExpander;
    property OnAfterDrawExpander: TAdvSmoothExpanderPanelAfterDrawExpander read FOnAfterDrawExpander write FOnAfterDrawExpander;
  end;

implementation

uses
  CommCtrl, ShellApi;

{$IFNDEF DELPHI7_LVL}

const
  CS_DROPSHADOW = $00020000;

{$ENDIF}
{$i GDIPHTMLEngine.pas}

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

function Lighter(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r + muldiv(255 - r, Percent, 100); //Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(r, g, b);
end;

procedure TAdvSmoothExpanderPanel.AnimateExpand(Sender: TObject);
var
  hto: Single;
  d: Double;
  a: Boolean;
  newheight: integer;
begin
  if FDoAnimate then
  begin
    d := Abs(FExpHeightTo - FExpHeight) / AnimationFactor;
    hto := FExpHeight;
    a := AnimateDouble(hto, FExpHeightTo, d, 0.1);
    if a then
    begin
      FEnableResize := false;
      newheight := Round(hto);
      Height := newheight;
      Changed;
      FExpHeight := Height;
      FEnableResize := true;

      if (Height <> newheight) then
      begin
        FDoAnimate := false;
        FExpTimer.Enabled := false;
      end;
    end
    else
    begin
      if Assigned(FOnExpandPanel) then
        FOnExpandPanel(Self);

      if not DisableLine then
        Caption.Line := Expanded;

      FDoAnimate := false;
      FExpTimer.Enabled := false;

      if Expanded then
      begin
        if Assigned(OnEndExpandPanel) then
          OnEndExpandPanel(Self);
      end
      else
      begin
        if Assigned(OnEndCollapsePanel) then
          OnEndCollapsePanel(Self);
      end;
    end;
  end;
end;

procedure TAdvSmoothExpanderPanel.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothExpanderPanel) then
  begin
    FExpanderShape := (Source as TAdvSmoothExpanderPanel).ExpanderShape;
    FExpanderColor := (Source as TAdvSmoothExpanderPanel).ExpanderColor;
    FExpanderHoverColor := (Source as TAdvSmoothExpanderPanel).ExpanderHoverColor;
    FExpanderDownColor := (Source as TAdvSmoothExpanderPanel).ExpanderDownColor;
    FExpanderSize := (Source as TAdvSmoothExpanderPanel).ExpanderSize;
    FExpanderBorderColor := (Source as TAdvSmoothExpanderPanel).ExpanderBorderColor;
    FExpanded := (Source as TAdvSmoothExpanderPanel).Expanded;
    FExpanderLeft := (Source as TAdvSmoothExpanderPanel).ExpanderLeft;
    FExpanderTop := (Source as TAdvSmoothExpanderPanel).ExpanderTop;
    FMinimumPanelHeight := (Source as TAdvSmoothExpanderPanel).MinimumPanelHeight;
    FShowExpander := (Source as TAdvSmoothExpanderPanel).ShowExpander;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.Changed;
begin
  inherited;
  Invalidate;
end;

procedure TAdvSmoothExpanderPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHover or FDown then
  begin
    FDown := false;
    FHover := false;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.Collapse;
begin
  if Expanded then
    Expanded := false;
end;

constructor TAdvSmoothExpanderPanel.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  Cursor := crDefault;
  FMinimumPanelHeight := 32;
  FExpHeight := Height;
  FOldHeight := Height;
  FExpanderColor := clWhite;
  FExpanderShape := esRoundRect;
  FExpanderColor := clGray;
  FExpanderHoverColor := clBlue;
  FExpanderDownColor := clRed;
  FExpanderSize := 16;
  FExpanderBorderColor := clBlack;
  FExpanderLocation := plTopRight;
  FExpanderLeft := 0;
  FExpanderTop := 0;
  FExpanded := true;
  FEnableResize := true;
  FExpTimer := TTimer.Create(Self);
  FExpTimer.Interval := 10;
  FExpTimer.OnTimer := AnimateExpand;
  FExpTimer.Enabled := false;
  FAnimationFactor := 4;
  FShowExpander := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  Width := 276;
  Height := 128;

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

procedure TAdvSmoothExpanderPanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('OldHeight', ReadCustomProperty, WriteCustomProperty, True);
end;

destructor TAdvSmoothExpanderPanel.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothExpanderPanel.DoAfterDrawExpander(AGraphics: TGPGraphics;
  ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean);
begin
  if Assigned(OnAfterDrawExpander) then
    OnAfterDrawExpander(Self, AGraphics, ARect, AExpanded, ADown, AHover);
end;

procedure TAdvSmoothExpanderPanel.DoBeforeDrawExpander(AGraphics: TGPGraphics;
  ARect: TGPRectF; AExpanded: Boolean; ADown: Boolean; AHover: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawExpander) then
    OnBeforeDrawExpander(Self, AGraphics, ARect, AExpanded, ADown, AHover, ADefaultDraw);
end;

procedure TAdvSmoothExpanderPanel.DrawExpander(g: TGPGraphics; r: TGPRectF;
  Down, Hover: Boolean);
var
  ct: TColor;
  fl: TGDIPFill;
  p: TGPPen;
  b: Boolean;
begin
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  b := True;
  DoBeforeDrawExpander(g, r, FExpanded, Down, Hover, b);
  if b then
  begin
    fl := TGDIPFill.Create;
    fl.GradientType := gtVertical;
    fl.GradientMirrorType := gtVertical;
    if not FFlatStyle then
    begin
      case ExpanderShape of
        esRounded:
        begin
          fl.RoundingType := rtBoth;
          fl.Rounding := Round(ExpanderSize / 2);
        end;
        esRoundRect:
        begin
          fl.RoundingType := rtBoth;
          fl.Rounding := Round(ExpanderSize / 4);
        end;
      end;
    end
    else
    begin
      fl.Rounding := 0;
    end;

    fl.BorderColor := ExpanderBorderColor;

    if Down then
    begin
      ct := ExpanderDownColor;
      fl.Color := Lighter(ct, 60);
      fl.ColorTo := ct;
    end
    else if Hover then
    begin
      ct := ExpanderHoverColor;
      fl.Color := Lighter(ct, 60);
      fl.ColorTo := ct;
    end
    else
    begin
      ct := ExpanderColor;
      fl.Color := Lighter(ct, 60);
      fl.ColorTo := ct;
    end;

    if ct <> clNone then
      fl.Fill(g, r);

    //DRAW SIGN
    p := TGPPen.Create(MakeColor(255, clBlack), 1);
    if not FExpanded then
    begin
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 2), r.X + (r.Width / 3), r.Y + (r.Height / 4));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 2), r.X + (r.Width / 3 * 2), r.Y + (r.Height / 4));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 3), r.X + (r.Width / 3), r.Y + (r.Height / 4 * 2));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 3), r.X + (r.Width / 3 * 2), r.Y + (r.Height / 4 * 2));
    end
    else
    begin
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4), r.X + (r.Width / 3), r.Y + (r.Height / 4 * 2));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4), r.X + (r.Width / 3 * 2), r.Y + (r.Height / 4 * 2));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 2), r.X + (r.Width / 3), r.Y + (r.Height / 4 * 3));
      g.DrawLine(p, r.X + (r.Width / 2), r.Y + (r.Height / 4 * 2), r.X + (r.Width / 3 * 2), r.Y + (r.Height / 4 * 3));
    end;

    p.Free;
    fl.Free;
    DoAfterDrawExpander(g, r, FExpanded, Down, Hover);
  end;

  g.SetSmoothingMode(SmoothingModeDefault);
end;

procedure TAdvSmoothExpanderPanel.Expand;
begin
  if not Expanded then
    Expanded := true;
end;

function Choose(A,B: integer; choice: boolean): integer;
begin
  if choice then
    Result := B
  else
    Result := A;
end;

procedure TAdvSmoothExpanderPanel.ReadCustomProperty(Reader: TReader);
begin
  FOldHeight := Reader.ReadFloat;
end;

procedure TAdvSmoothExpanderPanel.Resize;
begin
  inherited;
  if (csDesigning in ComponentState) and FEnableResize and Expanded then
    FOldHeight := Height;
end;

procedure TAdvSmoothExpanderPanel.GDIPPaint(g: TGPGraphics);
begin
  inherited;
  if ShowExpander then
    DrawExpander(g, GetExpanderRect, FDown, FHover);
end;

function TAdvSmoothExpanderPanel.GetExpanderRect: TGPRectF;
var
  x, y: double;
  r: TRect;
begin
  r := InsideRect;
  if ExpanderLocation <> plCustom then
    GetPosition(x, y, MakeRect(r.Left, r.Top, r.Right, r.Bottom), ExpanderSize, ExpanderSize, ExpanderLocation)
  else
  begin
    x := ExpanderLeft;
    y := ExpanderTop;
  end;

  Result := MakeRect(X, y, ExpanderSize, ExpanderSize);
end;

procedure TAdvSmoothExpanderPanel.GetPosition(var x, y: Double;
  rectangle: TGPRectF; objectwidth, objectheight: Double;
  location: TAdvSmoothPanelLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;

  case location of
    plTopLeft:
    begin
      x := rectangle.X;
      y := rectangle.Y;
    end;
    plTopRight:
    begin
      x := Max(rectangle.X, w - tw);
      y := rectangle.Y;
    end;
    plBottomLeft:
    begin
      x := rectangle.X;
      y := h - th;
    end;
    plBottomRight:
    begin
      x := Max(rectangle.X, w - tw);
      y := h - th;
    end;
    plTopCenter:
    begin
      x := Max(rectangle.X, w - tw) / 2;
      y := rectangle.Y;
    end;
    plBottomCenter:
    begin
      x := Max(rectangle.X, w - tw) / 2;
      y := h - th;
    end;
    plCenterCenter:
    begin
      x := Max(rectangle.X, w - tw) / 2;
      y := (h - th) / 2;
    end;
    plCenterLeft:
    begin
      x := rectangle.x;
      y := (h - th) / 2;
    end;
    plCenterRight:
    begin
      x := Max(rectangle.X, w - tw);;
      y := (h - th) / 2;
    end;
  end;
end;

function TAdvSmoothExpanderPanel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothExpanderPanel.InMoveArea(pt: TPoint): boolean;
var
  r: TRect;
begin
  r := ClientRect;
  Result := (pt.y < 20) and (pt.x < r.right - 20 - Fill.ShadowOffset) and (pt.X > r.Left);
end;

function TAdvSmoothExpanderPanel.InsideRect: TRect;
var
  bw: integer;
begin
  Result := ClientRect;
  // adapt width & height for GDI+ drawing rect
  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (Fill.BorderColor <> clNone) then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;

  if (Fill.ShadowOffset <> 0) and (Fill.ShadowColor <> clNone) then
  begin
    Result.Right := Result.Right -Fill.ShadowOffset;
    Result.Bottom := Result.Bottom -Fill.ShadowOffset;
  end;

  Result.Left := Result.Left + 2;
  Result.Right := Result.Right - 2;
  Result.Top := Result.Top + 2;
  Result.Bottom := Result.Bottom - 2;
end;

function TAdvSmoothExpanderPanel.IsResizeEnabled: Boolean;
begin
  Result := FEnableResize;
end;

procedure TAdvSmoothExpanderPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F4 then
    Expanded := not Expanded;
end;

procedure TAdvSmoothExpanderPanel.Loaded;
begin
  inherited;
  if not Caption.Line then
    DisableLine := true;

//  if FEnableResize and Expanded then
//    FOldHeight := Height;
end;

procedure TAdvSmoothExpanderPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if ShowExpander then
  begin
    FDown := PtInGPRect(GetExpanderRect, Point(X, Y));
    if FDown then
      Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ShowExpander then
  begin
    FHover := PtInGPRect(GetExpanderRect, Point(X, Y));
    if FHover then
    begin
      if not FOldHover then
      begin
        FOldHover := true;
        Changed;
      end;
    end
    else
    begin
      if FOldHover then
      begin
        FOldHover := false;
        Changed;
      end;
    end;
  end;
end;

procedure TAdvSmoothExpanderPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ShowExpander then
  begin
    FDown := false;
    if PtInGPRect(GetExpanderRect, Point(X, Y)) then
      Expanded := not Expanded;

    Changed;
  end;
end;

function TAdvSmoothExpanderPanel.PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TAdvSmoothExpanderPanel.SetAnimationFactor(const Value: integer);
begin
  if FAnimationFactor <> value then
  begin
    FAnimationFactor := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  FFlatStyle := False;
  ExpanderBorderColor := clSilver;
  case AStyle of
    tsWindows8, tsWindows10:
    begin
      FFlatStyle := True;
      ExpanderColor := $F7F6F5;
      ExpanderHoverColor := $F7EFE8;
      ExpanderDownColor := $F7E0C9;
    end;
    tsOffice2013White:
    begin
      FFlatStyle := True;
      ExpanderColor := clWhite;
      ExpanderHoverColor := $FCF0E4;
      ExpanderDownColor := $FCE2C8;
    end;
    tsOffice2013LightGray:
    begin
      FFlatStyle := True;
      ExpanderColor := clWhite;
      ExpanderHoverColor := $FCF0E4;
      ExpanderDownColor := $FCE2C8;
    end;
    tsOffice2013Gray:
    begin
      FFlatStyle := True;
      ExpanderColor := $E5E5E5;
      ExpanderHoverColor := $FCF0E4;
      ExpanderDownColor := $FCE2C8;
    end;
    tsOffice2016White:
    begin
      FFlatStyle := True;
      ExpanderColor := clWhite;
      ExpanderHoverColor := $F2E1D5;
      ExpanderDownColor := $E3BDA3;
      ExpanderBorderColor := $D4D4D4;
    end;
    tsOffice2016Gray:
    begin
      FFlatStyle := True;
      ExpanderColor := $B2B2B2;
      ExpanderHoverColor := $F2E1D5;
      ExpanderDownColor := $E3BDA3;
      ExpanderBorderColor := $444444;
    end;
    tsOffice2016Black:
    begin
      FFlatStyle := True;
      ExpanderColor := $363636;
      ExpanderHoverColor := $6A6A6A;
      ExpanderDownColor := $444444;
      ExpanderBorderColor := $444444;
    end;
  else
  begin
    ExpanderBorderColor := clBlack;
    ExpanderColor := Fill.Color;
    ExpanderDownColor := Fill.ColorTo;
    ExpanderHoverColor := $AAD9FF;
  end;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanded(const Value: Boolean);
begin
  FExpanded := Value;
  if not DisableLine then
    Caption.Line := Expanded;
  if (csDesigning in ComponentState) then
  begin
    FEnableResize := false;
    if FExpanded then
      Height := Round(FOldHeight)
    else
      Height := FMinimumPanelHeight;

    FExpHeight := Height;
    FExpHeightTo := Height;
    FEnableResize := true;
    if Assigned(FOnExpandPanel) then
      FOnExpandPanel(Self);
  end
  else
  begin
    if (csLoading in ComponentState) then
    begin
      if not FExpanded then
      begin
        FExpHeight := FMinimumPanelHeight;
        FExpHeightTo := FMinimumPanelHeight;
      end
      else
      begin
        FExpHeight := Height;
        FExpHeightTo := Height;
      end;
    end
    else
    begin
      FDoAnimate := true;
      FExpTimer.Enabled := true;
      if FExpanded then
      begin
        FExpHeightTo := Round(FOldHeight);
        if Assigned(OnStartExpandPanel) then
          OnStartExpandPanel(Self);
      end
      else
      begin
        FExpHeightTo := FMinimumPanelHeight;
        if Assigned(OnStartCollapsePanel) then
          OnStartCollapsePanel(Self);
      end;
    end;
  end;

  Changed;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderBorderColor(const Value: TColor);
begin
  if FExpanderBorderColor <> value then
  begin
    FExpanderBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderColor(const Value: TColor);
begin
  if FExpanderColor <> value then
  begin
    FExpanderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderDownColor(const Value: TColor);
begin
  if FExpanderDownColor <> value then
  begin
    FExpanderDownColor := Value;
    changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderHoverColor(const Value: TColor);
begin
  if FExpanderHoverColor <> Value then
  begin
    FExpanderHoverColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderLeft(const Value: integer);
begin
  if FExpanderLeft <> value then
  begin
    FExpanderLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderLocation(
  const Value: TAdvSmoothPanelLocation);
begin
  if FExpanderLocation <> value then
  begin
    FExpanderLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderShape(
  const Value: TAdvSmoothExpanderPanelShape);
begin
  if FExpanderShape <> value then
  begin
    FExpanderShape := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderSize(const Value: integer);
begin
  if FExpanderSize <> value then
  begin
    FExpanderSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetExpanderTop(const Value: integer);
begin
  if FExpanderTop <> value then
  begin
    FExpanderTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetMinimumPanelHeight(const Value: integer);
begin
  if FMinimumPanelHeight <> value then
  begin
    FMinimumPanelHeight := Value;
    if not Expanded then
      Height := Value;
//    SetExpanded(Expanded);
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.SetResizeEnabled(Resize: boolean);
begin
  FEnableResize := Resize;
end;

procedure TAdvSmoothExpanderPanel.SetShowExpander(const Value: Boolean);
begin
  if FShowExpander <> value then
  begin
    FShowExpander := Value;
    Changed;
  end;
end;

procedure TAdvSmoothExpanderPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothExpanderPanel.WMWindowPosChanged(
  var Message: TWMWindowPosChanged);
begin
//  if (csDesigning in ComponentState) or FDoAnimate then
//    invalidate
//  else if (Fill.ShadowColor <> clNone) and (Fill.ShadowOffset > 0) then
    Invalidate;

  inherited;
end;

procedure TAdvSmoothExpanderPanel.WriteCustomProperty(Writer: TWriter);
begin
  Writer.WriteFloat(FOldHeight);
end;

end.
