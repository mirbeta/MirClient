{***************************************************************************}
{ AeroWizardButton component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               }
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

unit AeroWizardButton;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Controls, Forms, Graphics, Messages, StdCtrls, SysUtils, Menus, Types;

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TButtonType = (btBack, btForward, btBackForward);
  TButtonPart = (bpNone, bpBack, bpForward, bpDropDown);
  TButtonState = (bsNormal, bsHot, bsDown, bsDisabled);
  TWizButton = (wbBack, wbForward);

  TOnAeroButtonEvent = procedure (Sender: TObject; Button: TWizButton) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAeroWizardButton = class(TCustomControl)
  private
    FMouseInControl: Boolean;
    FDropDownButton: Boolean;
    FButtonType: TButtonType;
    FAutoHeight: Boolean;
    FIsAeroVista: Boolean;
    FUpdatingSize: Boolean;
    FHotButton: TButtonPart;
    FDownButton: TButtonPart;
    FMouseDown: Boolean;
    FDropDownMenu: TPopupMenu;
    FOnDropDown: TNotifyEvent;
    FForwardEnabled: Boolean;
    FBackEnabled: Boolean;
    FForwardHint: string;
    FDropDownButtonHint: string;
    FBackHint: string;
    FFocusedPart: TButtonPart;
    FOnClick: TOnAeroButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetButtonType(const Value: TButtonType);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetBackEnabled(const Value: Boolean);
    procedure SetForwardEnabled(const Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function IsOnGlass: Boolean;
    procedure UpdateSize; virtual;
    procedure AdjustSize; override;
    procedure Loaded; override;
    function GetButtonRect(ButtonPart: TButtonPart): TRect;
    function PtOnButton(P: TPoint): TButtonPart;
    procedure InvalidateButton(ButtonPart: TButtonPart);
    function IsDropDownEnabled: Boolean;
    procedure DoDropDown; virtual;
    procedure DoButtonClick(Button: TButtonPart); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Anchors;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property BackEnabled: Boolean read FBackEnabled write SetBackEnabled default true;
    property BackHint: string read FBackHint write FBackHint;
    property BiDiMode;
    property ButtonType: TButtonType read FButtonType write SetButtonType default btBackForward;
    property Constraints;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default true;
    property DropDownButtonHint: string read FDropDownButtonHint write FDropDownButtonHint;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property Enabled;
    property ForwardEnabled: Boolean read FForwardEnabled write SetForwardEnabled default true;
    property ForwardHint: string read FForwardHint write FForwardHint;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnClick: TOnAeroButtonEvent read FOnClick write FOnClick;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  Themes, DWMApi, uxTheme, Math, AdvGDIP;


{$IFDEF DELPHIXE2_LVL}
function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

function ThemeServicesThemesEnabled: boolean;
begin
  Result := StyleServices.Enabled;
end;
{$ENDIF}

{$IFNDEF DELPHIXE2_LVL}
function ThemeServicesThemesEnabled: boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;
{$ENDIF}

//------------------------------

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

//------------------------------------------------------------------------------

function IsComCtl6: Boolean;
var
  i: Integer;
begin
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

function AeroIsEnabled: boolean;
var
  enabled: bool;
begin
  Result := False;
  //if (DWMlibrary = 0) then
  begin
    if (@DwmIsCompositionEnabled <> nil) then
    begin
      DwmIsCompositionEnabled(enabled);
      Result := enabled;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAeroWizardButton }

constructor TAeroWizardButton.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
{$IFNDEF DELPHI6_LVL}
  FIsAeroVista := False;
{$ELSE}
  FIsAeroVista := IsComCtl6 and IsVista and ThemeServicesThemesEnabled and AeroIsEnabled and not (csDesigning in ComponentState);
{$ENDIF}

  inherited Create(AOwner);

  if (csDesigning in ComponentState) then
    FIsAeroVista := False;

  FDropDownButton := True;
  FHotButton := bpNone;
  FDownButton := bpNone;
  FBackEnabled := True;
  FForwardEnabled := True;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FBackHint := 'Back';
    FForwardHint := 'Forward';
    FDropDownButtonHint := 'Recent Pages';
  end;

  FButtonType := btBackForward;
  FFocusedPart := bpBack;
  FAutoHeight := True;

  Width := 76;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

destructor TAeroWizardButton.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.DoButtonClick(Button: TButtonPart);
var
  wb: TWizButton;
begin
  if Assigned(FOnClick) and Enabled and (((Button = bpBack) and BackEnabled) or ((Button = bpForward) and ForwardEnabled)) then
  begin
    if (Button = bpBack) then
      wb := wbBack
    else
      wb := wbForward;
    FOnClick(Self, wb);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.DoDropDown;
var
  pt: TPoint;
begin
  if Assigned(Parent) and Assigned(FDropDownMenu) then
  begin
    if Assigned(FOnDropDown) then
      FOnDropDown(self);

    pt := Point(Left, Top + Height);

    pt := Parent.ClientToScreen(pt);
    FDropDownMenu.Popup(pt.X,pt.Y);

    FDownButton := bpNone;
    InvalidateButton(bpDropDown);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.InvalidateButton(ButtonPart: TButtonPart);
var
  R: TRect;
begin
  if (ButtonPArt <> bpNone) then
  begin
    R := GetButtonRect(ButtonPart);
    if (ButtonPart = bpDropDown) then
    begin
      InflateRect(R, 2, 2);
      R.Left := R.Left - (R.Right - R.Left) div 3;
    end;
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

function TAeroWizardButton.IsDropDownEnabled: Boolean;
begin
  Result := Enabled and DropDownButton and ((BackEnabled and (ButtonType in [btBack, btBackForward])) or (ForwardEnabled and (ButtonType in [btForward, btBackForward])));
end;

//------------------------------------------------------------------------------

function TAeroWizardButton.IsOnGlass: Boolean;
var
  PForm: TCustomForm;
begin
  Result := not (csDesigning in ComponentState) and ThemeServicesThemesEnabled and
    DwmCompositionEnabled and FIsAeroVista;

  if Result then
  begin
    PForm := GetParentForm(Self);
    Result := (PForm <> nil) and PForm.GlassFrame.FrameExtended and
      PForm.GlassFrame.IntersectsControl(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.Loaded;
begin
  inherited;
  UpdateSize;
end;

//------------------------------------------------------------------------------

function TAeroWizardButton.PtOnButton(P: TPoint): TButtonPart;
var
  R: TRect;
begin
  Result := bpNone;
  case ButtonType of
    btBack, btForward:
    begin
      if ButtonType = btBack then
        R := GetButtonRect(bpBack)
      else
        R := GetButtonRect(bpForward);
      if (PtInRect(R, P)) then
      begin
        if ButtonType = btBack then
          Result := bpBack
        else
          Result := bpForward;
        Exit;
      end;

      if DropDownButton then
      begin
        R := GetButtonRect(bpDropDown);
        if (PtInRect(R, P)) then
        begin
          Result := bpDropDown;
        end;
      end;
    end;
    btBackForward:
    begin
      R := GetButtonRect(bpBack);
      if (PtInRect(R, P)) then
      begin
        Result := bpBack;
        Exit;
      end;

      R := GetButtonRect(bpForward);
      if (PtInRect(R, P)) then
      begin
        Result := bpForward;
        Exit;
      end;

      if DropDownButton then
      begin
        R := GetButtonRect(bpDropDown);
        if (PtInRect(R, P)) then
        begin
          Result := bpDropDown;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  BP: TButtonPart;
begin
  inherited;

  FMouseDown := True;

  if (csDesigning in ComponentState) then
    Exit;

  bp := PtOnButton(Point(X, Y));
  if (bp <> bpNone) then
  begin
    FDownButton := bp;
    if (FDownButton <> bpNone) then
    begin
      InvalidateButton(FDownButton);
      if (FDownButton = bpDropDown) and DropDownButton and IsDropDownEnabled then
        DoDropDown
      else
      begin
        if Focused and (FDownButton in [bpBack, bpForward]) and (FFocusedPart <> FDownButton) then
        begin
          FFocusedPart := FDownButton;
          Invalidate;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  bp, OldBP: TButtonPart;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  bp := PtOnButton(Point(X, Y));
  if (bp <> FHotButton) then
  begin
    Application.CancelHint;

    OldBP := FHotButton;
    FHotButton := bp;
    if (OldBP <> bpNone) then
      InvalidateButton(OldBP);
    if (FHotButton <> bpNone) then
      InvalidateButton(FHotButton);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  BP: TButtonPart;
begin
  inherited;
  FMouseDown := False;

  bp := PtOnButton(Point(X, Y));
  if (bp = FDownButton) then
  begin
    FDownButton := bpNone;
    InvalidateButton(bp);
    if (bp in [bpBack, bpForward]) then
    begin
      DoButtonClick(bp);
    end;
  end;

  if (FDownButton <> bpNone) then
  begin
    bp := FDownButton;
    FDownButton := bpNone;
    InvalidateButton(bp);
  end;
end;

//------------------------------------------------------------------------------

function TAeroWizardButton.GetButtonRect(ButtonPart: TButtonPart): TRect;
var
  bs, sp, dbs, w, h, l, t, i: Integer;  // button size, space, dropdownbutton size
  BR, FR, DBR: TRect;        // button rect, dropdown button rect
begin
  Result := Rect(-1, -1, -1, -1);
  BR := Result;
  FR := Result;
  DBR := Result;
  sp := 1;
  w := Width;
  if AutoHeight then
    h := w
  else
    h := Height - 1;
  l := 0;
  t := 0;
  case ButtonType of
    btBack, btForward:
    begin
      if DropDownButton then
      begin
        bs := Round((w * 57) / 100);
        bs := Min(h, bs);
        dbs := Min (w - bs - sp-1, Round((bs * 72) / 100));
        i := bs div 12;
        BR := Rect(l, t, l + bs, t + bs);
        DBR := Rect(BR.Right, BR.Top + i, BR.Right + dbs, BR.Bottom - i-1);
      end
      else
      begin
        bs := Min(h, w);
        BR := Rect(l, t, l + bs, t + bs);
      end;
      FR := BR;
      if (ButtonType = btBack) then
        FR := Result
      else if (ButtonType = btForward) then
        BR := Result;
    end;
    btBackForward:
    begin
      if DropDownButton then
      begin
        bs := Round((w * 36) / 100);
        bs := Min(h, bs);
        dbs := Min (w - (bs*2 + sp*2), Round((bs * 72) / 100));
        i := bs div 12;
        sp := Max(sp, Round(bs / 20));
        BR := Rect(l, t, l + bs, t + bs);
        FR := Rect(BR.Right + sp, BR.Top, BR.Right + sp + bs, BR.Bottom);
        DBR := Rect(FR.Right, FR.Top + i, FR.Right + dbs, FR.Bottom - i-1);
      end
      else
      begin
        bs := Round((w * 49) / 100);
        bs := Min(h, bs);
        sp := Min (w - (bs * 2), Max(sp, Round(bs / 20)));
        bs := Round(((w - sp) * 50) / 100);
        bs := Min(h, bs);
        BR := Rect(l, t, l + bs, t + bs);
        FR := Rect(BR.Right + sp, BR.Top, BR.Right + sp + bs, BR.Bottom);
      end;
    end;
  end;

  case ButtonPart of
    bpBack: Result := BR;
    bpForward: Result := FR;
    bpDropDown: Result := DBR;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.SetAutoHeight(const Value: Boolean);
begin
  if (FAutoHeight <> Value) then
  begin
    FAutoHeight := Value;
    UpdateSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.SetButtonType(const Value: TButtonType);
begin
  if (FButtonType <> Value) then
  begin
    FButtonType := Value;
    UpdateSize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.SetDropDownButton(const Value: Boolean);
begin
  if (FDropDownButton <> Value) then
  begin
    FDropDownButton := Value;
    UpdateSize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.SetBackEnabled(const Value: Boolean);
begin
  if (FBackEnabled <> Value) then
  begin
    FBackEnabled := Value;
    if DropDownButton then
      Invalidate
    else
      InvalidateButton(bpBack);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.SetForwardEnabled(const Value: Boolean);
begin
  if (FForwardEnabled <> Value) then
  begin
    FForwardEnabled := Value;
    if DropDownButton then
      Invalidate
    else
      InvalidateButton(bpForward);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.UpdateSize;
var
  h: Integer;
begin
  if AutoHeight and not FUpdatingSize and not (csLoading in ComponentState) then
  begin
    FUpdatingSize := True;
    try
      h := Max(GetButtonRect(bpBack).Bottom, GetButtonRect(bpForward).Bottom);
      Height := h + 1;
    finally
      FUpdatingSize := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.AdjustSize;
begin
  inherited;
  UpdateSize;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CreateHandle;
begin
  inherited CreateHandle;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CMHintShow(var Message: TMessage);
var
  PHI: PHintInfo;
  bp: TButtonPart;
begin
  PHI := TCMHintShow(Message).HintInfo;

  bp := PtOnButton(Point(PHI.CursorPos.X, PHI.CursorPos.Y));
  case bp of
    bpBack: PHI^.HintStr := BackHint;
    bpForward: PHI^.HintStr := ForwardHint;
    bpDropDown: PHI^.HintStr := DropDownButtonHint;
    else
      PHI^.HintStr := Hint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, MakeLParam(Message.Pos.x, Message.Pos.Y));
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------

procedure DrawAeroButtonBackground(g: TGPGraphics; BR, FR: TRect);
var
  Path: TGPGraphicsPath;
  h, h2, w2, C, i: Integer;
  P: array[0..2] of TGPPoint;
  Pen: TGPPen;
  gb: TGPLinearGradientBrush;
begin
  if not Assigned(g) then
    Exit;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  FR.Right := FR.Right - 1;
  FR.Bottom := FR.Bottom - 1;
  BR.Bottom := BR.Bottom - 1;
  h2 := (BR.Bottom - BR.Top) div 2;
  w2 := (BR.Right - BR.Left) div 2;
  h := (BR.Bottom - BR.Top) + 1;
  C := (FR.Left - BR.Right) div 2;
  i := Max(1, h div 8);

  //--- Top part
  gb := TGPLinearGradientBrush.Create(MakeRect(BR.Left,BR.Top, FR.Right - BR.Left, h2+1), MakeColor(50, 0, 0, 0), MakeColor(0, 0, 0, 0), LinearGradientModeVertical);
  Pen := TGPPEn.Create(MakeColor(0, 0, 0, 0), 1.5);
  Pen.SetBrush(gb);

  Path := TGPGraphicsPath.Create();
  Path.AddArc(BR.Left, BR.Top, h, h, 180, 90);

  p[0] := MakePoint(BR.Right - w2, BR.Top);
  p[1] := MakePoint(BR.Right + C, BR.Top + i-1);
  p[2] := MakePoint(FR.Left + w2, FR.Top);
  Path.AddCurve(PGPPoint(@p), 3, 0.8);

  Path.AddArc(FR.Left, FR.Top, h, h, -90, 90);
  g.DrawPath(Pen, Path);

  gb.Free;
  //Fill
  gb := TGPLinearGradientBrush.Create(MakeRect(BR.Left,BR.Top, FR.Right - BR.Left, h2+1), MakeColor(50, 0, 0, 0), MakeColor(10, 0, 0, 0), LinearGradientModeVertical);
  Path.CloseFigure;
  g.FillPath(gb, Path);
  gb.Free;

  Pen.Free;
  Path.Free;
  //---


  //--- Bottom part
  gb := TGPLinearGradientBrush.Create(MakeRect(BR.Left,BR.Top + h2, FR.Right - BR.Left, h2 + 2), MakeColor(0, 255, 255, 255), MakeColor(120, 255, 255, 255), LinearGradientModeVertical);
  Pen := TGPPEn.Create(MakeColor(0, 0, 0, 0), 1.6);
  Pen.SetBrush(gb);

  Path := TGPGraphicsPath.Create();
  //Path.AddArc(BR.Left, BR.Top, h, h, 180, 90);
  Path.AddArc(FR.Left, FR.Top, h, h, 0, 90);

  //g.DrawPath(Pen, Path);
  //Path.Reset;

  p[0] := MakePoint(FR.Left + w2, FR.Bottom + 1);
  p[1] := MakePoint(BR.Right + C, BR.Bottom - i + 1);
  p[2] := MakePoint(BR.Right - w2, BR.Bottom + 1);
  Path.AddCurve(PGPPoint(@p), 3, 0.6);
  //g.DrawPath(Pen, Path);

  //Path.Reset;
  //Path.AddArc(FR.Left, FR.Top, h, h, -90, 90);
  Path.AddArc(BR.Left, BR.Top, h, h, 90, 90);
  g.DrawPath(Pen, Path);

  gb.Free;
  //Fill
  gb := TGPLinearGradientBrush.Create(MakeRect(BR.Left,BR.Top + h2, FR.Right - BR.Left, h2), MakeColor(10, 255, 255, 255), MakeColor(100, 255, 255, 255), LinearGradientModeVertical);
  Path.CloseFigure;
  g.FillPath(gb, Path);
  gb.Free;

  Pen.Free;
  Path.Free;
  //---
end;

procedure DrawAeroButton(g: TGPGraphics; R: TRect; BackDir: Boolean; State: TButtonState; Focused: Boolean);
  procedure DrawArrow;
  var
    Pen: TGPPen;
    gb: TGPLinearGradientBrush;
    w, l: Integer;
    pw: Single;
    P1, P2: TGPPoint;
  begin
    w := (R.Right - R.Left);
    pw := w / 8;
    l := w div 2 + 1;
    if BackDir then
    begin
      if (State =  bsDisabled) then
      begin
        P1 := MakePoint(R.Left + (R.Right - R.Left - l) div 2, R.Top + (R.Bottom - R.Top) div 2);

        Pen := TGPPen.Create(MakeColor(160, 255, 255, 255), pw);
        Pen.SetEndCap(LineCapRound);
        P2 := MakePoint(P1.X + (l div 2), P1.Y - l div 2 +1);
        g.DrawLine(Pen, P1.X, P1.Y + 1, P2.X, P2.Y);   //  /
        Pen.Free;

        Pen := TGPPen.Create(MakeColor(180, 200, 200, 200), pw);
        Pen.SetEndCap(LineCapRound);
        g.DrawLine(Pen, P1.X, P1.Y, P1.X + l, P1.Y);   //  --

        P2 := MakePoint(P1.X + l div 2, P1.Y + l div 2 - 1);
        g.DrawLine(Pen, P1.X, P1.Y- pw / 4, P2.X, P2.Y);     //  \

        Pen.Free;
      end
      else
      begin
        Pen := TGPPen.Create(MakeColor(255, 255, 255, 255), pw);
        Pen.SetEndCap(LineCapRound);
        P1 := MakePoint(R.Left + (R.Right - R.Left - l) div 2, R.Top + (R.Bottom - R.Top) div 2);
        g.DrawLine(Pen, P1.X, P1.Y, P1.X + l, P1.Y); //  --

        P2 := MakePoint(P1.X + (l div 2), P1.Y - l div 2 +1);
        g.DrawLine(Pen, P1.X, P1.Y + pw / 4, P2.X, P2.Y); //  /

        gb := TGPLinearGradientBrush.Create(MakeRect(P1.X, P1.Y - pw, l, l), MakeColor(255, 255, 255, 255), MakeColor(200, 200, 200, 200), 30);
        Pen.SetBrush(gb);
        P2 := MakePoint(P1.X + l div 2, P1.Y + l div 2 - 1);
        g.DrawLine(Pen, P1.X, P1.Y- pw / 4, P2.X, P2.Y);   //  \
        gb.Free;

        Pen.Free;
      end;
    end
    else // Forward
    begin
      if (State =  bsDisabled) then
      begin
        P1 := MakePoint(R.Left + (R.Right - R.Left - l) div 2, R.Top + (R.Bottom - R.Top) div 2);

        Pen := TGPPen.Create(MakeColor(160, 255, 255, 255), pw);
        Pen.SetEndCap(LineCapRound);
        P2 := MakePoint(P1.X + l - (l div 2), P1.Y - l div 2 +1);
        g.DrawLine(Pen, P1.X + l, P1.Y + 1, P2.X, P2.Y);   //  /
        Pen.Free;

        Pen := TGPPen.Create(MakeColor(180, 200, 200, 200), pw);
        Pen.SetEndCap(LineCapRound);
        g.DrawLine(Pen, P1.X, P1.Y, P1.X + l, P1.Y);   //  --

        P2 := MakePoint(P1.X + l - (l div 2), P1.Y + l div 2 - 1);
        g.DrawLine(Pen, P1.X + l, P1.Y- pw / 4, P2.X, P2.Y);     //  \

        Pen.Free;
      end
      else
      begin
        Pen := TGPPen.Create(MakeColor(255, 255, 255, 255), pw);
        Pen.SetEndCap(LineCapRound);
        P1 := MakePoint(R.Left + (R.Right - R.Left - l) div 2, R.Top + (R.Bottom - R.Top) div 2);
        g.DrawLine(Pen, P1.X + l, P1.Y, P1.X, P1.Y); //  --

        P2 := MakePoint(P1.X + l - (l div 2), P1.Y - l div 2 +1);
        g.DrawLine(Pen, P1.X + l, P1.Y + pw / 4, P2.X, P2.Y); //  /

        gb := TGPLinearGradientBrush.Create(MakeRect(P1.X, P1.Y - pw, l, l), MakeColor(255, 255, 255, 255), MakeColor(200, 200, 200, 200), 120);
        Pen.SetBrush(gb);
        P2 := MakePoint(P1.X + l - (l div 2), P1.Y + l div 2 - 1);
        g.DrawLine(Pen, P1.X + l, P1.Y- pw / 4, P2.X, P2.Y);   //  \
        gb.Free;

        Pen.Free;
      end;
    end;
  end;
var
  Path, Path2: TGPGraphicsPath;
  Pen: TGPPen;
  sb: TGPSolidBrush;
  gb: TGPLinearGradientBrush;
  h, w, h2: Integer;
  TopBackClr, TopGClrFrom, TopGClrTo, BotmBackClr, BotmGClrFrom, BotmGClrTo: Cardinal;
  TopOutBrClr, TopInBrClrFrom, TopInBrClrTo, BotmOutBrClr, BotmInBrClrFrom, BotmInBrClrTo: Cardinal;
begin
  if not Assigned(g) then
    Exit;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  InflateRect(R, -2, -2);

  h2 := (R.Bottom - R.Top) div 2;
  h := (R.Bottom - R.Top);
  w := (R.Right - R.Left);
  case State of
    bsNormal:
    begin
      BotmBackClr := MakeColor(200, 0, 22, 92);
      BotmGClrFrom := MakeColor(0, 0, 22, 92);
      BotmGClrTo := MakeColor(255, 86, 218, 255);

      TopBackClr := MakeColor(200, 0, 22, 92);
      TopGClrFrom := MakeColor(255, 255, 255, 255);
      TopGClrTo := MakeColor(0, 255, 255, 255);

      TopOutBrClr := MakeColor(130, 0, 0, 0);
      TopInBrClrFrom := MakeColor(255, 255, 255, 255);
      TopInBrClrTo := MakeColor(0, 145, 255, 255);
      BotmOutBrClr := MakeColor(200, 0, 0, 0);
      BotmInBrClrFrom := MakeColor(0, 145, 255, 255);
      BotmInBrClrTo := MakeColor(255, 145, 255, 255);
    end;
    bsHot:
    begin
      TopBackClr := MakeColor(200, 0, 59, 150);
      TopGClrFrom := MakeColor(255, 255, 255, 255);
      TopGClrTo := MakeColor(60, 255, 255, 255);

      BotmBackClr := MakeColor(200, 0, 59, 150);
      BotmGClrFrom := MakeColor(0, 0, 55, 230);
      BotmGClrTo := MakeColor(255, 138, 247, 255);

      TopOutBrClr := MakeColor(190, 54, 124, 200);
      TopInBrClrFrom := MakeColor(255, 255, 255, 255);
      TopInBrClrTo := MakeColor(40, 145, 255, 255);
      BotmOutBrClr := MakeColor(200, 28, 60, 140);
      BotmInBrClrFrom := MakeColor(0, 0, 59, 150);
      BotmInBrClrTo := MakeColor(255, 133, 247, 255);
    end;
    bsDown:
    begin
      TopBackClr := MakeColor(200, 0, 19, 74);
      TopGClrFrom := MakeColor(140, 255, 255, 255);
      TopGClrTo := MakeColor(10, 255, 255, 255);

      BotmBackClr := MakeColor(200, 0, 19, 74);
      BotmGClrFrom := MakeColor(0, 0, 19, 74);
      BotmGClrTo := MakeColor(255, 86, 218, 255);

      TopOutBrClr := MakeColor(170, 0, 0, 0);
      TopInBrClrFrom := MakeColor(140, 255, 255, 255);
      TopInBrClrTo := MakeColor(10, 145, 255, 255);
      BotmOutBrClr := MakeColor(200, 0, 0, 0);
      BotmInBrClrFrom := MakeColor(0, 145, 255, 255);
      BotmInBrClrTo := MakeColor(255, 145, 255, 255);
    end;
    else // bsNormal
    begin
      TopBackClr := MakeColor(200, 0, 22, 92);
      TopGClrFrom := MakeColor(230, 255, 255, 255);
      TopGClrTo := MakeColor(0, 255, 255, 255);

      BotmBackClr := MakeColor(200, 0, 22, 92);
      BotmGClrFrom := MakeColor(0, 0, 22, 92);
      BotmGClrTo := MakeColor(255, 86, 218, 255);

      TopOutBrClr := MakeColor(130, 0, 0, 0);
      TopInBrClrFrom := MakeColor(230, 255, 255, 255);
      TopInBrClrTo := MakeColor(0, 145, 255, 255);
      BotmOutBrClr := MakeColor(200, 0, 0, 0);
      BotmInBrClrFrom := MakeColor(0, 145, 255, 255);
      BotmInBrClrTo := MakeColor(255, 145, 255, 255);
    end;
  end;


  case State of
    bsNormal, bsHot, bsDown:
    begin
      //- Bottom Dark fill
      Path := TGPGraphicsPath.Create();
      sb := TGPSolidBrush.Create(BotmBackClr);
      Path.AddEllipse(R.Left, R.Top, h, h);
      Path.CloseFigure;
      g.FillPath(sb, Path);
      sb.Free;
      Path.Free;

      // Bottom Glow shade
      Path := TGPGraphicsPath.Create();
      Path.AddArc(R.Left, R.Top, h, h, 5, 170);
      Path.CloseFigure;
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top + h2+1, w, h2-1), BotmGClrFrom, BotmGClrTo, LinearGradientModeVertical);
      g.FillPath(gb, Path);
      gb.Free;
      Path.Free;
      //-

      //- Top Dark fill
      Path := TGPGraphicsPath.Create();
      sb := TGPSolidBrush.Create(TopBackClr);
      Path.AddArc(R.Left, R.Top, h, h, 180, 180);
      Path.CloseFigure;
      g.FillPath(sb, Path);
      sb.Free;
      Path.Free;

      // Top Glow shade
      Path := TGPGraphicsPath.Create();
      Path.AddArc(R.Left, R.Top, h, h, 180, 180);
      Path.CloseFigure;
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top, w, h2+1), TopGClrFrom, TopGClrTo, LinearGradientModeVertical);
      g.FillPath(gb, Path);
      gb.Free;
      Path.Free;
      //-

      //Bottom inner border
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top + h2, h, h2+1), BotmInBrClrFrom, BotmInBrClrTo, LinearGradientModeVertical);
      Pen := TGPPen.Create(MakeColor(0, 0, 22, 92), 1.6);
      Pen.SetBrush(gb);
      Path2 := TGPGraphicsPath.Create();
      Path2.AddArc(R.Left+1, R.Top+1, h-2, h-2, 0, 180);
      g.DrawPath(Pen, Path2);
      Pen.Free;
      gb.Free;
      Path2.Free;

      //Top inner border
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top, h, h2+2), TopInBrClrFrom, TopInBrClrTo, LinearGradientModeVertical);
      Pen := TGPPen.Create(MakeColor(0, 0, 22, 92), 1.5);
      Pen.SetBrush(gb);
      Path2 := TGPGraphicsPath.Create();
      Path2.AddArc(R.Left+1, R.Top+1, h-2, h-2, 180, 180);
      g.DrawPath(Pen, Path2);
      Pen.Free;
      gb.Free;
      Path2.Free;

      Path := TGPGraphicsPath.Create();
      // Bottom outer border
      Pen := TGPPen.Create(BotmOutBrClr, 1.5);
      Path.AddArc(R.Left, R.Top, h, h, 0, 180);
      g.DrawPath(Pen, Path);
      Pen.Free;

      // Top outer border
      Pen := TGPPen.Create(TopOutBrClr, 1.5);
      Path.AddArc(R.Left, R.Top, h, h, 180, 180);
      g.DrawPath(Pen, Path);
      Pen.Free;
      Path.Free;
      //
    end;
    bsDisabled:
    begin
      //- Bottom Dark fill
      Path := TGPGraphicsPath.Create();
      sb := TGPSolidBrush.Create(MakeColor(40, 200, 200, 200));
      Path.AddEllipse(R.Left, R.Top, h, h);
      Path.CloseFigure;
      g.FillPath(sb, Path);
      sb.Free;
      Path.Free;
      //-

      // Top Glow shade
      Path := TGPGraphicsPath.Create();
      Path.AddArc(R.Left, R.Top, h, h, 180, 180);
      Path.CloseFigure;
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top, w, h2+1), MakeColor(190, 255, 255, 255), MakeColor(80, 255, 255, 255), LinearGradientModeVertical);
      g.FillPath(gb, Path);
      gb.Free;
      Path.Free;
      //-

      //Bottom inner border
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top + h2, h, h2+1), MakeColor(0, 255, 255, 255), MakeColor(100, 255, 255, 255), LinearGradientModeVertical);
      Pen := TGPPen.Create(MakeColor(0, 255, 255, 255), 1.6);
      Pen.SetBrush(gb);
      Path2 := TGPGraphicsPath.Create();
      Path2.AddArc(R.Left+1, R.Top+1, h-2, h-2, 0, 180);
      g.DrawPath(Pen, Path2);
      Pen.Free;
      gb.Free;
      Path2.Free;

      //Top inner border
      gb := TGPLinearGradientBrush.Create(MakeRect(R.Left, R.Top, h, h2+2), MakeColor(180, 255, 255, 255), MakeColor(0, 255, 255, 255), LinearGradientModeVertical);
      Pen := TGPPen.Create(MakeColor(0, 255, 255, 255), 1.5);
      Pen.SetBrush(gb);
      Path2 := TGPGraphicsPath.Create();
      Path2.AddArc(R.Left+1, R.Top+1, h-2, h-2, 180, 180);
      g.DrawPath(Pen, Path2);
      Pen.Free;
      Path2.Free;
      gb.Free;

      Path := TGPGraphicsPath.Create();
      // Bottom outer border
      Pen := TGPPen.Create(MakeColor(130, 0, 0, 0), 1.5);
      Path.AddArc(R.Left, R.Top, h, h, 0, 180);
      g.DrawPath(Pen, Path);
      Pen.Free;

      // Top outer border
      Pen := TGPPen.Create(MakeColor(100, 0, 0, 0), 1.5);
      Path.AddArc(R.Left, R.Top, h, h, 180, 180);
      g.DrawPath(Pen, Path);
      Pen.Free;
      Path.Free;
      //
    end;
  end;

  DrawArrow;

  //--- Focus Rect
  if Focused and (State <> bsDisabled) then
  begin
    //DrawFocusRect(g.GetHDC, Rect(R.Left + 4, R.Top + 4, R.Right - 4, R.Bottom - 4)); // disrupting GDIP drawing
    Path := TGPGraphicsPath.Create();
    Path.AddEllipse(R.Left + 3, R.Top + 3, (w - 6), (h - 6));
    Pen := TGPPen.Create(MakeColor(0, 0, 0), 1.0);
    Pen.SetDashStyle(DashStyleDot);
    g.DrawPath(Pen , Path);
    Pen.Free;
    Path.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawAeroDropDownButton(g: TGPGraphics; R: TRect; State: TButtonState);
var
  path: TGPGraphicsPath;
  Pen: TGPPen;
  radius, i: Integer;
  sb: TGPSolidBrush;
  gb: TGPLinearGradientBrush;
  R2: TRect;
  P: array[0..2] of TGPPoint;
  Br1, Br2, Br3, FromClr, ToClr: Cardinal;
begin
  if not Assigned(g) then
    Exit;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  radius := 2;

  R2 := R;
  R2.Left := R2.Left - (R.Bottom - R.Top) div 5;

  if (State in [bsHot, bsDown]) then
  begin
    if (State = bsHot) then
    begin
      Br1 := MakeColor(60, 255, 255, 255);
      Br2 := MakeColor(150, 100, 100, 100);
      Br3 := MakeColor(130, 255, 255, 255);

      FromClr := MakeColor(40, 255, 255, 255);
      ToClr := MakeColor(150, 255, 255, 255);
    end
    else //if (State = bsDown)
    begin
      Br1 := MakeColor(60, 255, 255, 255);
      Br2 := MakeColor(255, 91, 107, 114);
      Br3 := MakeColor(220, 68, 85, 85);

      FromClr := MakeColor(40, 100, 124, 124);
      ToClr := MakeColor(150, 100, 124, 124);
    end;

    // outer white border
    path := TGPGraphicsPath.Create;
    path.AddLine(R2.Left-1, R2.Top, R2.Right - (radius*2), R2.Top);
    path.AddArc(R2.Right - (radius*2), R2.Top, radius*2, radius*2, 270, 90);
    path.AddLine(R2.Right, R2.Top + radius, R2.Right, R2.Bottom - (radius*2));
    path.AddArc(R2.Right - (radius*2), R2.Bottom - (radius*2), radius*2, radius*2,0,90);
    path.AddLine(R2.Right - (radius*2), R2.Bottom, R2.Left, R2.Bottom);

    pen := TGPPen.Create(Br1);
    g.DrawPath(pen, path);
    Pen.Free;
    path.Free;

    // dark border
    InflateRect(R2, -1, -1);
    path := TGPGraphicsPath.Create;
    path.AddLine(R2.Left-1, R2.Top, R2.Right - (radius*2), R2.Top);
    path.AddArc(R2.Right - (radius*2), R2.Top, radius*2, radius*2, 270, 90);
    path.AddLine(R2.Right, R2.Top + radius, R2.Right, R2.Bottom - (radius*2));
    path.AddArc(R2.Right - (radius*2), R2.Bottom - (radius*2), radius*2, radius*2,0,90);
    path.AddLine(R2.Right - (radius*2), R2.Bottom, R2.Left, R2.Bottom);

    pen := TGPPen.Create(Br2);
    g.DrawPath(pen, path);
    Pen.Free;
    path.Free;

    // Inner border
    InflateRect(R2, -1, -1);
    path := TGPGraphicsPath.Create;

    path.AddLine(R2.Left - 1, R2.Top, R2.Right - (radius*2), R2.Top);
    path.AddArc(R2.Right - (radius*2), R2.Top, radius*2, radius*2, 270, 90);
    path.AddLine(R2.Right, R2.Top + radius, R2.Right, R2.Bottom - (radius*2));
    path.AddArc(R2.Right - (radius*2), R2.Bottom - (radius*2), radius*2, radius*2,0,90);
    path.AddLine(R2.Right - (radius*2), R2.Bottom, R2.Left, R2.Bottom);

    p[0] := MakePoint(R2.Left, R2.Bottom);
    p[1] := MakePoint(R.Left, R2.Top + (R2.Bottom - R2.Top) div 2);
    p[2] := MakePoint(R2.Left, R2.Top);
    Path.AddCurve(PGPPoint(@p), 3, 0.8);

    pen := TGPPen.Create(Br3);
    g.DrawPath(pen, path);
    Pen.Free;

    gb := TGPLinearGradientBrush.Create(MakeRect(R2.Left, R2.Top, R2.Left + (R2.Right - R2.Left), R2.Top + (R2.Bottom - R2.Top)), FromClr, ToClr, LinearGradientModeVertical);
    g.FillPath(gb, Path);
    gb.Free;
    path.Free;
  end;

  //--- Arrow
  if (State = bsDisabled) then
  begin
    i := Round((R.Bottom - R.Top) / 4);
    P[0] := MakePoint(R2.Left + (R2.Right - R2.Left) div 2, R2.Top + i + (R2.Bottom - R2.Top - i) div 2);
    P[1] := MakePoint(P[0].X - i + 1, P[0].Y - i);
    P[2] := MakePoint(P[0].X + i -1, P[0].Y - i);

    path := TGPGraphicsPath.Create;
    Path.AddLines(@P, 3);
    Path.CloseFigure;
    sb := TGPSolidBrush.Create(MakeColor(120, 255, 255, 255));
    g.FillPath(sb, Path);
    sb.Free;
    pen := TGPPen.Create(MakeColor(140, 147, 146), 1.5);
    g.DrawLine(Pen, P[1].X, P[1].Y+1, P[0].X, P[0].Y);
    g.DrawLine(Pen, P[0].X, P[0].Y, P[2].X, P[2].Y+1);
    g.DrawLine(Pen, P[1].X+1, P[1].Y, P[2].X-1, P[2].Y);
    Pen.Free;
    Path.Free;
  end
  else
  begin
    i := Round((R.Bottom - R.Top) / 4);
    P[0] := MakePoint(R2.Left + (R2.Right - R2.Left) div 2, R2.Top + i + (R2.Bottom - R2.Top - i) div 2);
    P[1] := MakePoint(P[0].X - i + 1, P[0].Y - i);
    P[2] := MakePoint(P[0].X + i -1, P[0].Y - i);

    path := TGPGraphicsPath.Create;
    Path.AddLines(@P, 3);
    Path.CloseFigure;
    gb := TGPLinearGradientBrush.Create(MakeRect(P[1].X-1, P[1].Y-1, R2.Right - R2.Left, i), MakeColor(50, 181, 218), MakeColor(56, 116, 187), LinearGradientModeVertical);
    g.FillPath(gb, Path);
    gb.Free;
    pen := TGPPen.Create(MakeColor(4, 40, 88), 1.5);
    g.DrawLine(Pen, P[1].X, P[1].Y+1, P[0].X, P[0].Y);
    g.DrawLine(Pen, P[0].X, P[0].Y, P[2].X, P[2].Y+1);
    g.DrawLine(Pen, P[1].X+1, P[1].Y, P[2].X-1, P[2].Y);
    Pen.Free;
    Path.Free;
  end;
  //---
end;

procedure TAeroWizardButton.Paint;
var
  Rgn1: HRGN;
  R: TRect;
  i: Integer;
  p: TPoint;
  g: TGPGraphics;
  bs: TButtonState;
  fp: TButtonPart;
begin
  R := ClientRect;
  if True then
  begin
    // TRANSPARENCY CODE
    rgn1 :=  CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    SelectClipRgn(Canvas.Handle, rgn1);

    i := SaveDC(Canvas.Handle);
    p := ClientOrigin;
    Windows.ScreenToClient(Parent.Handle, p);
    p.x := -p.x;
    p.y := -p.y;
    MoveWindowOrg(Canvas.Handle, p.x, p.y);

    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    // transparency ?
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);

    if (Parent is TWinCtrl) then
     (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);

    RestoreDC(Canvas.Handle, i);

    SelectClipRgn(Canvas.Handle, 0);
    DeleteObject(rgn1);
  end;

  g := TGPGraphics.Create(Canvas.Handle);

  fp := bpNone;
  if (ButtonType = btBackForward) then
  begin
    DrawAeroButtonBackground(g, GetButtonRect(bpBack), GetButtonRect(bpForward));
    if not (csDesigning in ComponentState) and TabStop and Focused then
      fp := FFocusedPart;
  end;

  if (ButtonType in [btBack, btBackForward]) then
  begin
    if not Enabled or not BackEnabled then
      bs := bsDisabled
    else
    begin
      bs := bsNormal;
      if (FHotButton = bpBack) then
        bs := bsHot;
      if (FDownButton = bpBack) then
        bs := bsDown;
    end;

    DrawAeroButton(g, GetButtonRect(bpBack), True, bs, (fp = bpBack));
  end;

  if (ButtonType in [btForward, btBackForward]) then
  begin
    if not Enabled or not ForwardEnabled then
      bs := bsDisabled
    else
    begin
      bs := bsNormal;
      if (FHotButton = bpForward) then
        bs := bsHot;
      if (FDownButton = bpForward) then
        bs := bsDown;
    end;

    DrawAeroButton(g, GetButtonRect(bpForward), False, bs, (fp = bpForward));
  end;

  if DropDownButton then
  begin
    if not IsDropDownEnabled then
      bs := bsDisabled
    else
    begin
      bs := bsNormal;
      if (FHotButton = bpDropDown) then
        bs := bsHot;
      if (FDownButton = bpDropDown) then
        bs := bsDown;
    end;

    DrawAeroDropDownButton(g, GetButtonRect(bpDropDown), bs);
  end;

  g.Free;

  {  TODO: del it
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(ClientRect);

  Canvas.Pen.Color := clYellow;
  Canvas.Rectangle(GetButtonRect(bpBack));

  Canvas.Pen.Color := clLime;
  Canvas.Rectangle(GetButtonRect(bpForward));

  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(GetButtonRect(bpDropDown));
  }
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMKeyDown(var Message: TWMKeyDown);
begin
  if not(csDesigning in ComponentState) and TabStop and Focused then
  begin
    case Message.CharCode of
      VK_LEFT, VK_UP:
      begin
        if (ButtonType = btBackForward) and (FFocusedPart = bpForward) then
        begin
          FFocusedPart := bpBack;
          Invalidate;
        end;
      end;
      VK_RIGHT, VK_DOWN:
      begin
        if (ButtonType = btBackForward) and (FFocusedPart = bpBack) then
        begin
          FFocusedPart := bpForward;
          Invalidate;
        end;
      end;
      VK_SPACE, VK_RETURN:
      begin
        if (FFocusedPart = bpBack) and (ButtonType in [btBack, btBackForward]) then
          DoButtonClick(bpBack)
        else if (FFocusedPart = bpForward) and (ButtonType in [btForward, btBackForward]) then
          DoButtonClick(bpForward);
      end;
      VK_F4:
      begin
        if DropDownButton and IsDropDownEnabled then
        begin
          FDownButton := bpDropDown;
          InvalidateButton(bpDropDown);
          DoDropDown;
          FDownButton := bpNone;
          InvalidateButton(bpDropDown);
        end;
      end;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateSize;
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := True;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAeroWizardButton.CMMouseLeave(var Message: TMessage);
var
  bp: TButtonPart;
begin
  inherited;

  FMouseInControl := False;

  if (FHotButton <> bpNone) then
  begin
    bp := FHotButton;
    FHotButton := bpNone;
    InvalidateButton(bp);
  end;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

end.
