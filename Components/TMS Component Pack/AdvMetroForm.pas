{*************************************************************************}
{ TMS Metro Form                                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2012 - 2015                                       }
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

unit AdvMetroForm;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, AdvStyleIF,
  ImgList, ExtCtrls, AdvMetroRes, Types;

const
  DEFAULT_CAPTIONHEIGHT = 26;
  DEFAULT_BTNSIZE = 20;
  DEFAULT_SIZEGRIPSIZE = 12;

var
  MetroFormTones: TColorTones;

type
  TCaptionStyle = (csMetro, csPlain);
  TSysIcon = (siNone, siMin, siMax, siClose);

  TFormAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FCaptionFont: TFont;
    FSystemIconColorDown: TColor;
    FSizeGripColor: TColor;
    FColor: TColor;
    FSystemIconColor: TColor;
    FSystemIconColorHot: TColor;
    FCaptionColor: TColor;
    FCaptionStyle: TCaptionStyle;
    FSystemIconColorDisabled: TColor;
    FCaptionActiveColor: TColor;
    FFont: TFont;
    FTextAlign: TAlignment;
    FProgressColor: TColor;
    FTextColor: TColor;
    FShowAppIcon: boolean;
    procedure Change;
    procedure CaptionFontChanged(Sender: TObject);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetSizeGripColor(const Value: TColor);
    procedure SetSystemIconColor(const Value: TColor);
    procedure SetCaptionStyle(const Value: TCaptionStyle);
    procedure SetCaptionActiveColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetTextAlign(const Value: TAlignment);
    procedure SetProgressColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetShowAppIcon(const Value: boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SystemIconColor: TColor read FSystemIconColor write SetSystemIconColor default $A0A0A0;
    property SystemIconColorHot: TColor read FSystemIconColorHot write FSystemIconColorHot default clSilver;
    property SystemIconColorDown: TColor read FSystemIconColorDown write FSystemIconColorDown default clGray;
    property SystemIconColorDisabled: TColor read FSystemIconColorDisabled write FSystemIconColorDisabled;
    property ShowAppIcon: boolean read FShowAppIcon write SetShowAppIcon default true;
    property Color: TColor read FColor write SetColor default clWhite;
    property SizeGripColor: TColor read FSizeGripColor write SetSizeGripColor default $00F2BC00;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default $00F2BC00;
    property CaptionStyle: TCaptionStyle read FCaptionStyle write SetCaptionStyle default csMetro;
    property CaptionActiveColor: TColor read FCaptionActiveColor write SetCaptionActiveColor default $00B0A374;
    property Font: TFont read FFont write SetFont;
    property TextAlign: TAlignment read FTextAlign write SetTextAlign default taRightJustify;
    property TextColor: TColor read FTextColor write SetTextColor default clGray;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clWhite;
  end;

  TAdvMetroForm = class(TForm, ITMSTones, ITMSMetro)
  private
    { Private declarations }
    FImages: TCustomImageList;
    FNoDropShadow: boolean;
    FSizeGrip: Boolean;
    FAppearance: TFormAppearance;
    FMouseOn: TSysIcon;
    FMouseDown: TSysIcon;
    FText: string;
    FShowProgress: boolean;
    FTimer: TTimer;
    FProgressPos: integer;
    FImageIndex: integer;
    FNoResize: boolean;
    FSysShadowHandle: THandle;
    procedure WMActivate(var Msg: TMessage); message WM_ACTIVATE;
    procedure WMNCActivate(var Msg: TMessage); message WM_NCACTIVATE;
    procedure WMGetMinMaxInfo(var Msg: TMessage); message WM_GETMINMAXINFO;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure OnAppearanceChanged(Sender: TObject);
    procedure SetSizeGrip(const Value: Boolean);
    procedure SetAppearance(const Value: TFormAppearance);
    procedure SetText(const Value: string);
    procedure SetShowProgress(const Value: boolean);
    procedure TimerProc(Sender: TObject);
    procedure Initialize;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: integer);
  protected
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Activate; override;
    procedure Loaded; override;
    {$IFDEF DELPHI_UNICODE}
    procedure InitializeNewForm; override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCaptionHeight: Integer;
    function GetCaptionRect: TRect;
    function GetSysIconRect(SysIcon: TSysIcon): TRect;
    function GetSysRectWidth: Integer;
    function PtOnIcon(pt: TPoint): TSysIcon;
    procedure DrawSysIcons(ACanvas: TCanvas; SysIcon: TSysIcon = siNone);  // siNone = All icons
    procedure DrawCaption;
    procedure DrawSizeGrip;
    procedure InvalidateIcon(SysIcon: TSysIcon);
    procedure SysIconClick(SysIcon: TSysIcon);           // siNone = None
    function IsFullMaximized: Boolean;
    procedure FixSysShadowOrder;
    function HasMinimize: boolean;
    function HasMaximize: boolean;
    function IsMetro: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetColorTones(ATones: TColorTones);
    property NoResize: boolean read FNoResize write FNoResize;
  {$IFNDEF DELPHI_UNICODE}
    property Appearance: TFormAppearance read FAppearance write SetAppearance;
  {$ENDIF}
  published
  {$IFDEF DELPHI_UNICODE}
    property Appearance: TFormAppearance read FAppearance write SetAppearance;
  {$ENDIF}
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default false;
    property NoDropShadow: boolean read FNoDropShadow write FNoDropShadow default False;

    property Action;
    property ActiveControl;
    property Align;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property TransparentColor default False;
    property TransparentColorValue default 0;
    property Constraints;
    property UseDockManager;
    property DefaultMonitor;
    property DockSite;
    property DoubleBuffered default False;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    property FormStyle;
    {$IFDEF DELPHI_UNICODE}
    property GlassFrame;
    {$ENDIF}
    property Height;
    property HelpFile;
    property HorzScrollBar;
    property Icon;
    property KeyPreview;
    {$IFDEF DELPHI2007_LVL}
    property Padding;
    {$ENDIF}
    property Menu;
{$IF NOT DEFINED(CLR)}
    property OldCreateOrder;
{$IFEND}
    property ObjectMenuItem;
    property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    {$IFDEF DELPHI2007_LVL}
    property PopupMode;
    property PopupParent;
    {$ENDIF}
    property Position;
    property PrintScale;
    property Scaled;
    property ScreenSnap default False;
    property ShowHint;
    property ShowProgress: boolean read FShowProgress write SetShowProgress default false;
    property SnapBuffer default 10;
    property Text: string read FText write SetText;
    property VertScrollBar;
    property Visible;
    property Width;
    property WindowState;
    property WindowMenu;
    property OnActivate;
    {$IFDEF DELPHI2007_LVL}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2007_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2007_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
  end;

  TAdvMetroFormClass = class of TAdvMetroForm;



implementation

uses
  MultiMon, ShellAPI;

//------------------------------------------------------------------------------

function IsTaskbarAutoHide: boolean;
var
  ApBrData : TAppBarData;
begin
  ApBrData.cbSize := sizeof(ApBrData);
  Result := (ShAppBarMessage(ABM_GETSTATE, ApBrData) and ABS_AUTOHIDE) > 0;
end;

//------------------------------------------------------------------------------

{ TAdvMetroForm }

constructor TAdvMetroForm.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF DELPHI_UNICODE}
  Initialize;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

constructor TAdvMetroForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  {$IFNDEF DELPHI_UNICODE}
  Initialize;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.Initialize;
begin
  if not Assigned(FAppearance) then
    FAppearance := TFormAppearance.Create;

  FAppearance.OnChange := OnAppearanceChanged;

  Caption := 'AdvMetroForm';

  FImages := nil;
  FImageIndex := -1;

  BorderWidth := 0;

  Ctl3D := false;

  FMouseOn := siNone;
  FMouseDown := siNone;

  Constraints.MinHeight := DEFAULT_CAPTIONHEIGHT;
  Constraints.MinWidth := 100;

  if not IsClearTones(MetroFormTones) then
    SetColorTones(MetroFormTones);

  FTimer := nil;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI_UNICODE}
procedure TAdvMetroForm.InitializeNewForm;
begin
  inherited;
  Initialize;
end;
{$ENDIF}

//------------------------------------------------------------------------------

destructor TAdvMetroForm.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;

begin
  inherited CreateParams(Params);

  if not (csDesigning in ComponentState) and not FNoDropShadow then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
       ((Win32MajorVersion > 5) or
        ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
      Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.Activate;
begin
  inherited;
  FixSysShadowOrder;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);

  Rect.Top := Rect.Top + GetCaptionHeight;

  if SizeGrip then
    Rect.Bottom := Rect.Bottom - DEFAULT_SIZEGRIPSIZE - 2;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;
  ARect.Top := ARect.Top + GetCaptionHeight;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.Loaded;
var
  i, ch: Integer;
begin
  inherited;

  {$IFDEF DELPHI2007_LVL}
  GlassFrame.Enabled := False;
  {$ENDIF}

  if not (csDesigning in ComponentState) then
  begin
    ch := GetCaptionHeight;
    for I := 0 to ControlCount - 1 do
    begin
      if Controls[i].Parent = Self then
        Controls[i].Top := Controls[i].Top + ch;
    end;
  end;

  FNoResize := (BorderStyle in [bsDialog, bsSingle, bsNone]);

  if not (csDesigning in ComponentState) then
  begin
    BorderStyle := bsNone;
  end;

  Width := Width - 2 * (GetSystemMetrics(SM_CXDLGFRAME) + GetSystemMetrics(SM_CXBORDER)) - 10;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.GetCaptionHeight: Integer;
begin
  //if (csDesigning in ComponentState) then
    Result := DEFAULT_CAPTIONHEIGHT
  //else
    //Result := DEFAULT_CAPTIONHEIGHT;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.GetCaptionRect: TRect;
begin
  Result := ClientRect;
  Result.Bottom := Result.Top + GetCaptionHeight;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.GetSysRectWidth: Integer;
begin
  Result := 0;

  if HasMinimize or HasMaximize then
  begin
    Result := Result + 2 * DEFAULT_BTNSIZE;
  end;

  if (biSystemMenu in BorderIcons) then
    Result := Result + DEFAULT_BTNSIZE;
end;

function TAdvMetroForm.HasMaximize: boolean;
begin
  Result := (biMaximize in BorderIcons) and not FNoResize;
end;

function TAdvMetroForm.HasMinimize: boolean;
begin
  Result := (biMinimize in BorderIcons) and not FNoResize;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.GetSysIconRect(SysIcon: TSysIcon): TRect;
begin
  Result := Rect(-1, -1, -1, -1);

  if not (biSystemMenu in BorderIcons) then
    Exit;

  case SysIcon of
    siMin:
    begin
      if HasMaximize or HasMinimize then
      begin
        Result := GetSysIconRect(siMax);
        Result.Right := Result.Left - 1;
        Result.Left := Result.Right - DEFAULT_BTNSIZE;
      end;
    end;
    siMax:
    begin
      if HasMaximize or HasMinimize then
      begin
        Result := GetSysIconRect(siClose);
        Result.Right := Result.Left - 1;
        Result.Left := Result.Right - DEFAULT_BTNSIZE;
      end;
    end;
    siClose:
    begin
      Result := GetCaptionRect;
      if (Appearance.CaptionStyle = csMetro) then
        Result.Top := Result.Top + 2 + (Result.Bottom - Result.Top - DEFAULT_BTNSIZE) div 2
      else
        Result.Top := Result.Top + (Result.Bottom - Result.Top - DEFAULT_BTNSIZE) div 2;

      Result.Right := Result.Right - 6;
      Result.Left := Result.Right - DEFAULT_BTNSIZE;
      Result.Bottom := Result.Top + DEFAULT_BTNSIZE;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.PtOnIcon(pt: TPoint): TSysIcon;
var
  R: TRect;
begin
  Result := siNone;

  if not (biSystemMenu in BorderIcons) then
    Exit;

  R := GetSysIconRect(siClose);
  if PtInRect(R, pt) then
  begin
    Result := siClose;
    Exit;
  end;

  if HasMaximize or HasMinimize then
  begin
    R := GetSysIconRect(siMax);
    if PtInRect(R, pt) then
    begin
      Result := siMax;
      Exit;
    end;

    R := GetSysIconRect(siMin);
    if PtInRect(R, pt) then
    begin
      Result := siMin;
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FMouseDown := PtOnIcon(Point(X, Y));
  if (FMouseDown <> siNone) then
    InvalidateIcon(FMouseDown);
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  si, Oldsi: TSysIcon;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  si := PtOnIcon(Point(X, Y));

  if (FMouseOn <> si) then
  begin
    if (FMouseOn <> siNone) then
    begin
      Oldsi := FMouseOn;
      FMouseOn := siNone;
      InvalidateIcon(Oldsi);
    end;

    FMouseOn := si;
    if (FMouseOn <> siNone) then
      InvalidateIcon(FMouseOn);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  si, Oldsi: TSysIcon;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  si := PtOnIcon(Point(X, Y));
  if (FMouseDown <> siNone) then
  begin
    Oldsi := FMouseDown;
    FMouseDown := siNone;
    InvalidateIcon(Oldsi);

    if (si = Oldsi) then
      SysIconClick(Oldsi);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.CMMouseEnter(var Msg: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.CMMouseLeave(var Message: TMessage);
var
  si: TSysIcon;
begin
  inherited;
  if (FMouseOn <> siNone) then
  begin
    si := FMouseOn;
    FMouseOn := siNone;
    InvalidateIcon(si);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  DrawCaption;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SysIconClick(SysIcon: TSysIcon);
begin
  if not (biSystemMenu in BorderIcons) or (csDesigning in ComponentState) then
    Exit;

  //if ((biMaximize in BorderIcons) or (biMinimize in BorderIcons)) then
  if (BorderIcons <> []) then
  begin
    case SysIcon of
      siMin:
      begin
        if HasMinimize then
          WindowState := wsMinimized;
      end;
      siMax:
      begin
        if HasMaximize then
        begin
          if (WindowState = wsMaximized) then
            WindowState := wsNormal
          else
            WindowState := wsMaximized;
        end;
        Invalidate;
      end;
      siClose: Close;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.TimerProc(Sender: TObject);
begin
  FProgressPos := FProgressPos + 5;
  if FProgressPos > ClientRect.Right then
    FProgressPos := 0;
  DrawCaption;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.Paint;
var
  R: TRect;
begin
  inherited;

//  if (csDesigning in ComponentState) then
//    Exit;

  R := ClientRect;
  //--- background
  Canvas.Brush.Color := Appearance.Color;
  Canvas.Pen.Color := Appearance.Color;
  Canvas.FillRect(R);

  DrawCaption;
  DrawSizeGrip;
end;

//------------------------------------------------------------------------------

function VCenter(R: TRect; H:integer): integer;
begin
  if H < R.Bottom - R.Top then
    Result := R.Top + (R.Bottom - R.Top - H) div 2
  else
    Result := R.Top;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.DrawCaption;
var
  R, TxtR: TRect;
  offsetx, w, tw: Integer;
  clr: TColor;
  DTSTYLE: DWORD;
  bmp: TBitmap;
begin
  if (csDesigning in ComponentState) then
    Exit;

  R := GetCaptionRect;

  if (r.Right = r.Left) or (r.Bottom = r.Top) then
    Exit;

  if (GetForegroundWindow = Handle) then
  begin
    clr := Appearance.CaptionActiveColor;
  end
  else
  begin
    clr := Appearance.CaptionColor;
  end;

  bmp := TBitmap.Create;
  try

    bmp.Width := r.Right - r.Left;
    bmp.Height := r.Bottom - r.Top;

    bmp.Canvas.Brush.Color := Appearance.Color;
    bmp.Canvas.Pen.Color := Appearance.Color;

    if (Appearance.CaptionStyle = csPlain) then
    begin
      bmp.Canvas.Brush.Color := clr;
      bmp.Canvas.Pen.Color := clr;
    end;

    // background
    bmp.Canvas.FillRect(R);

    offsetX := 4;
    if Assigned(FImages) and (FImageIndex >= 0) then
      offsetX := FImages.Width + 8
    else
      if not Application.Icon.Empty and Appearance.ShowAppIcon then
        offsetX := 24;

    bmp.Canvas.Font.Assign(Appearance.CaptionFont);

    w := 0;

    if (Appearance.CaptionStyle = csMetro) and (clr <> clNone) then
    begin
      bmp.Canvas.Brush.Color := clr;
      bmp.Canvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 3));

      w := bmp.Canvas.TextWidth(Caption) + offsetX + 8;

      bmp.Canvas.FillRect(Rect(R.Left, R.Top, R.Left + w, R.Bottom));
    end;

    if (Caption <> '') then
    begin
      if Assigned(FImages) and (FImageIndex >= 0) then
      begin
        FImages.Draw(bmp.Canvas, R.Left + 4, VCenter(R, FImages.Height), FImageIndex);
      end
      else
      begin
        if not Application.Icon.Empty and Appearance.ShowAppIcon then
        begin
          {
          bmpic := TBitmap.Create;
          try
            bmpic.Width := Application.Icon.Width;
            bmpic.Height := Application.Icon.Height;
            bmpic.Canvas.Brush.Color := clFuchsia;
            bmpic.Canvas.Pen.Color := clFuchsia;
            bmpic.Canvas.FillRect(Rect(0,0,bmpic.Width, bmpic.Height));
            bmpic.Transparent := true;
            bmpic.TransparentMode := tmAuto;
            bmpic.Canvas.Draw(0,0,Application.Icon);

            bmp.Canvas.StretchDraw(Rect(4,4,20,20), bmpic);
          finally
            bmpic.Free;
          end;
          }
          DrawIconEx(bmp.Canvas.Handle, 4, 4, Application.Icon.Handle, 16,16, 0,0, DI_NORMAL);
        end;
      end;

      TxtR := Rect(R.Left + offsetX, R.Top, R.Right, R.Bottom);
      DrawText(bmp.Canvas.Handle, PChar(Caption), -1, TxtR, DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_NOPREFIX);
    end;

    if Assigned(FTimer) then
    begin
      bmp.Canvas.Brush.Color := Appearance.ProgressColor;
      bmp.Canvas.FillRect(Rect(R.Left + FProgressPos, R.Top, R.Left + FProgressPos + 100, R.Top + 3));
      bmp.Canvas.Brush.Color := clr;
    end;

    if (Text <> '') then
    begin
      tw := GetSysRectWidth;

      TxtR := Rect(R.Left + w + offsetx, R.Top, R.Right - tw - offsetx, R.Bottom);

      bmp.Canvas.Brush.Color := clNone;
      bmp.Canvas.Brush.Style := bsClear;
      bmp.Canvas.Font.Color := Appearance.TextColor;

      case Appearance.TextAlign of
      taCenter: DTSTYLE := DT_CENTER;
      taRightJustify: DTSTYLE := DT_RIGHT;
      else
        DTSTYLE := DT_LEFT;
      end;

      DrawText(bmp.Canvas.Handle, PChar(Text), -1, TxtR, DT_SINGLELINE or DT_VCENTER or DTSTYLE or DT_NOPREFIX);
    end;

    DrawSysIcons(bmp.Canvas, siNone);

    Canvas.Draw(r.Left, r.Top, bmp);

  finally
    bmp.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.DrawSizeGrip;
var
  R, R1: TRect;
  i, j, x, y: Integer;
begin
  if (csDesigning in ComponentState) or not SizeGrip or (WindowState = wsMaximized) then
    Exit;

  R := ClientRect;
  R.Bottom := R.Bottom - 2;
  R.Right := R.Right - 2;

  Canvas.Brush.Color := Appearance.SizeGripColor;
  y := R.Bottom;
  for I := 4 downto 1 do
  begin
    x := R.Right;
    for j := 1 to I do
    begin
      R1 := Rect(x - 2, y - 2, x, y);
      Canvas.FillRect(R1);
      x := x - 3;
    end;
    y := y - 3;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.DrawSysIcons(ACanvas: TCanvas; SysIcon: TSysIcon);

  procedure DrawCross(R: TRect; Clr: TColor);
  begin
    with ACanvas do
    begin
      Pen.Color := Clr;
                     {/}
      MoveTo(R.Left, R.Top + 7);
      LineTo(R.Left + 8, R.Top);
      MoveTo(R.Left + 1, R.Top + 7);
      LineTo(R.Left + 7, R.Top);
      MoveTo(R.Left + 2, R.Top + 7);
      LineTo(R.Left + 9, R.Top);
                     {\}
      MoveTo(R.Left, R.Top + 1);
      LineTo(R.Left + 8, R.Top + 8);
      MoveTo(R.Left + 1, R.Top + 1);
      LineTo(R.Left + 7, R.Top + 8);
      MoveTo(R.Left + 2, R.Top + 1);
      LineTo(R.Left + 9, R.Top + 8);
    end;
  end;

  procedure DrawNormalIcon(R: TRect; Clr: TColor);
  var
    x, y: Integer;
  begin
    with ACanvas do
    begin
      x := 0; //(R.Right - R.Left - 8) div 3;
      y := 0; //(R.Bottom - R.Top - 8) div 3;

      Pen.Color := Clr;

      MoveTo(R.Left+x, R.Top+y);
      LineTo(R.Left + 9+x, R.Top+y);
      MoveTo(R.Left+x, R.Top+1+y);
      LineTo(R.Left + 9+x, R.Top+1+y);
      MoveTo(R.Left+x, R.Top + 2+y);
      LineTo(R.Left + 9+x, R.Top + 2+y);
      MoveTo(R.Left+x, R.Top + 2+y);
      LineTo(R.Left+x, R.Top + 8+y);
      LineTo(R.Left + 8+x, R.Top + 8+y);
      LineTo(R.Left + 8+x, R.Top + 2+y);
    end;
  end;

  procedure DrawMaximize(R: TRect; Clr: TColor);
  begin
    with ACanvas do
    begin
      Pen.Color := Clr;

      MoveTo(R.Left + 2, R.Top);
      LineTo(R.Left + 8, R.Top);
      MoveTo(R.Left + 2, R.Top + 1);
      LineTo(R.Left + 8, R.Top + 1);
      MoveTo(R.Left + 8, R.Top);
      LineTo(R.Left + 8, R.Top + 6);

      MoveTo(R.Left, R.Top + 3);
      LineTo(R.Left + 7, R.Top + 3);
      MoveTo(R.Left, R.Top + 4);
      LineTo(R.Left + 7, R.Top + 4);
      MoveTo(R.Left, R.Top + 4);
      LineTo(R.Left, R.Top + 8);
      LineTo(R.Left + 6, R.Top + 8);
      LineTo(R.Left + 6, R.Top + 4);
    end;

  end;

  procedure DrawMinimize(R: TRect; Clr: TColor);
  begin
    with ACanvas do
    begin
      Pen.Color := Clr;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 8, R.Top);
      MoveTo(R.Left , R.Top + 1);
      LineTo(R.Left + 8, R.Top + 1);
      MoveTo(R.Left , R.Top + 2);
      LineTo(R.Left + 8, R.Top + 2);
    end;
  end;

var
  clr: TColor;
  R: TRect;
begin
  if not (biSystemMenu in BorderIcons) or (csDesigning in ComponentState) then
    Exit;

  //-- Draw Min
  if (HasMaximize or HasMinimize) and ((SysIcon = siNone) or (SysIcon = siMin)) then
  begin
    if not (HasMinimize) then
      Clr := Appearance.SystemIconColorDisabled
    else if (FMouseDown = siMin) then
      Clr := Appearance.SystemIconColorDown
    else if (FMouseOn = siMin) then
      Clr := Appearance.SystemIconColorHot
    else
      Clr := Appearance.SystemIconColor;
    R := GetSysIconRect(siMin);

    R.Top := R.Bottom - (R.Bottom - R.Top) div 2;
    R.Left := R.Left + (R.Right - R.Left - 8) div 2;
    DrawMinimize(R, Clr);
  end;

  //-- Draw Max
  if (HasMaximize or HasMinimize) and ((SysIcon = siNone) or (SysIcon = siMax)) then
  begin
    if not (HasMaximize) then
      Clr := Appearance.SystemIconColorDisabled
    else if (FMouseDown = siMax) then
      Clr := Appearance.SystemIconColorDown
    else if (FMouseOn = siMax) then
      Clr := Appearance.SystemIconColorHot
    else
      Clr := Appearance.SystemIconColor;
    R := GetSysIconRect(siMax);

    R.Top := R.Top + (R.Bottom - R.Top - 8) div 2;
    R.Left := R.Left + (R.Right - R.Left - 8) div 2;
    if (WindowState = wsMaximized) then
      DrawMaximize(R, Clr)
    else
      DrawNormalIcon(R, Clr);
  end;

  //-- Draw close
  if (SysIcon = siNone) or (SysIcon = siClose) then
  begin
    if (FMouseDown = siClose) then
      Clr := Appearance.SystemIconColorDown
    else if (FMouseOn = siClose) then
      Clr := Appearance.SystemIconColorHot
    else
      Clr := Appearance.SystemIconColor;

    R := GetSysIconRect(siClose);

    R.Top := R.Top + (R.Bottom - R.Top - 8) div 2;
    R.Left := R.Left + (R.Right - R.Left - 8) div 2;
    DrawCross(R, Clr);
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.InvalidateIcon(SysIcon: TSysIcon);
var
  R: TRect;
begin
  if not (biSystemMenu in BorderIcons) or (SysIcon = siNone) then
    Exit;

  R := GetSysIconRect(SysIcon);
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetAppearance(const Value: TFormAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetColorTones(ATones: TColorTones);
begin
  MetroFormTones := ATones;

  if not Assigned(FAppearance) then
    FAppearance := TFormAppearance.Create;

  Appearance.Color := ATones.Background.BrushColor;
  Appearance.CaptionColor := ATones.Foreground.BrushColor;
  Appearance.CaptionActiveColor := ATones.Selected.BrushColor;
  Appearance.CaptionFont.Color := ATones.Selected.TextColor;
  Appearance.CaptionFont.Name := GetMetroFont;
  Appearance.ProgressColor := ATones.Hover.BrushColor;
  Appearance.TextColor := ATones.Background.TextColor;

  Appearance.SystemIconColor := ATones.Background.TextColor;
  Appearance.SystemIconColorHot := ATones.Hover.BrushColor;
  Appearance.SystemIconColorDown := ATones.Selected.BrushColor;
  Appearance.SizeGripColor := ATones.Selected.BrushColor;

  Font.Color := ATones.Background.TextColor;
  Font.Name := GetMetroFont;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetShowProgress(const Value: boolean);
begin
  if (FShowProgress <> Value) then
  begin
    FShowProgress := Value;

    if not FShowProgress and Assigned(FTimer) then
      FreeAndNil(FTimer);

    if FShowProgress and not Assigned(FTimer) then
    begin
      FProgressPos := 0;
      FTimer := TTimer.Create(Self);
      FTimer.Interval := 10;
      FTimer.OnTimer := TimerProc;
      FTimer.Enabled := true;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetSizeGrip(const Value: Boolean);
begin
  if (FSizeGrip <> Value) then
  begin
    FSizeGrip := Value;
    Invalidate;
    Width := Width - 1;
    Width := Width + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMActivate(var Msg: TMessage);
begin
  inherited;
  FixSysShadowOrder;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMGetMinMaxInfo(var Msg: TMessage);
var
  info: ^TMinMaxInfo;
  rc: TRect;
  mi: TMonitorInfo;
  dx, dy: integer;
  DoNotSetWidth: Boolean;
  mon: TMonitor;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FillChar(mi, SizeOf(mi), 0); mi.cbSize := SizeOf(mi);
    if GetMonitorInfo(MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST), @mi) then
    begin
      dx := 0;
      dy := 0;
      DoNotSetWidth := False;
      info := pointer(Msg.LParam);

      // compensate for clipregion
      if not IsVista then
      begin
        dx := 3;
        dy := 3;
      end;

      mon := Screen.MonitorFromWindow(Handle);
      if Assigned(mon) then
      begin
        rc := mon.WorkAreaRect;

        if not mon.Primary and (WindowState = wsMaximized) and not IsVista and (info^.ptMaxSize.X < (abs(rc.Right - rc.Left) + 2 * dx)) then // FF: Diff sec Mon resolution size iss
        begin
          DoNotSetWidth := True;
        end;
      end
      else
        rc := mi.rcWork;

      info^.ptMaxPosition.X := mi.rcWork.Left - mi.rcMonitor.Left - dx;
      info^.ptMaxPosition.Y := mi.rcWork.Top - mi.rcMonitor.Top - dy;

      if not DoNotSetWidth then
      begin
        if not mon.Primary and (WindowState = wsMaximized) and not IsVista then
          info^.ptMaxSize.X := rc.Right - rc.Left
        else
          info^.ptMaxSize.X := rc.Right - rc.Left + 2 * dx;
      end;
      info^.ptMaxSize.Y := rc.Bottom - rc.Top + 2 * dy;

      if (WindowState = wsMaximized) {and not IsWin7FormMaxState} and (BorderStyle = bsNone) then
        info^.ptMaxSize.Y := info^.ptMaxSize.Y - 1;

      if (WindowState = wsMaximized) {and not IsWin7FormMaxState} and IsTaskbarAutoHide then
      begin
        info^.ptMaxSize.Y := info^.ptMaxSize.Y - 4;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;

  if (csDesigning in ComponentState) or not (biSystemMenu in BorderIcons) then
    Exit;

  if (biMaximize in BorderIcons) and (PtInRect(GetCaptionRect, Point(Message.XPos, Message.YPos))) then
  begin
    if (WindowState = wsNormal) then
      WindowState := wsMaximized
    else
      WindowState := wsNormal;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  FixSysShadowOrder;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMWindowPosChanging(var Message: TWMWindowPosChanging);
const
  SWP_STATECHANGED = $8000;
begin
  inherited;

  if (Message.WindowPos.flags and SWP_STATECHANGED) = SWP_STATECHANGED then  // Window state changed
  begin
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMNCActivate(var Msg: TMessage);
begin
  inherited;
  FixSysShadowOrder;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
  inherited;
  if not (csDesigning in ComponentState) and (WindowState = wsNormal) then
  begin
    pt := ScreenToClient(Point(message.Xpos, message.YPos));

    if not FNoResize then
    begin
      if (pt.Y < 3) then
        Message.Result := HTTOP;

      if (pt.X < 3) then
        Message.Result := HTLEFT;

      if (pt.X > Width - 5) then
        Message.Result := HTRIGHT;

      if (pt.Y > Height - 3) then
        Message.Result := HTBOTTOM;

      if (pt.X > Width - 12) and (pt.Y > Height - 12)  then
        Message.Result := HTBOTTOMRIGHT;

      if (pt.X < 10) and (pt.Y < 10)  then
        Message.Result := HTTOPLEFT;

      if (pt.X > Width - 5) and (pt.Y < 5)  then
        Message.Result := HTTOPRIGHT;

      if (pt.X < 5) and (pt.Y > Height - 5)  then
        Message.Result := HTBOTTOMLEFT;
    end;

    if (pt.Y > 3) and (pt.Y < GetCaptionHeight) and (pt.X > 20) and (pt.X < Width - 8 - GetSysRectWidth) then
    begin
      Message.Result := HTCAPTION;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.IsFullMaximized: Boolean;
var
  mon: TMonitor;
  P: TPoint;
  R: TRect;
begin
  Result := (WindowState = wsMaximized);

  if Result and ((Constraints.MaxHeight > 0) or (Constraints.MaxWidth > 0)) then
  begin
    mon := nil;
    P := Point(0, 0);
    if Assigned(Parent) then
    begin
      P := Parent.ClientToScreen(P);
      mon := Screen.MonitorFromPoint(P);
    end;
    if Assigned(mon) then
      R := mon.WorkAreaRect
    else
    begin
      SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
    end;

    Result := (Self.Width >= R.Right - R.Left - 10) and ((Self.Height >= R.Bottom - R.Top - 30));
  end;
end;

//------------------------------------------------------------------------------

function TAdvMetroForm.IsMetro: boolean;
begin
  Result := true;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
  FixSysShadowOrder;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroForm.FixSysShadowOrder;

  function FindSysShadowOrderProc(WindowHandle: HWND; // handle to window
    Form: TAdvMetroForm // application-defined value, 32-bit
    ): BOOL; stdcall;
  var
    Buffer: array [0 .. 255] of char;
    Rect: TRect;
  begin
    Result := True;
    if IsWindowVisible(WindowHandle) then
    begin
      // this code  search for SysShadow window created for this window.
      GetClassName(WindowHandle, Buffer, 255);
      if 0 <> AnsiStrComp(Buffer, PChar('SysShadow')) then
        Exit;

      GetWindowRect(WindowHandle, Rect);
      if (Rect.Left <> Form.Left) or (Rect.Top <> Form.Top) then
        Exit;

      Form.FSysShadowHandle := WindowHandle;
      // stop enumeration
      Result := False;
    end;
  end;

begin
  if not(csDesigning in ComponentState) and
    ((GetClassLong(Handle, GCL_STYLE) and CS_DROPSHADOW) = CS_DROPSHADOW)
    and IsWindowVisible(Handle) then
  begin
    // for speed, proper SysShadow handle is cached
    if FSysShadowHandle = 0 then
      EnumThreadWindows(GetCurrentThreadID(), @FindSysShadowOrderProc,
        lParam(Self));
    // if SysShadow exists, change its z-order, and place it directly below this window
    if FSysShadowHandle <> 0 then
    begin
      SetWindowPos(FSysShadowHandle, Handle, 0, 0, 0, 0,
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOSIZE);
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TFormAppearance }

constructor TFormAppearance.Create;
var
  fnt: string;
begin
  inherited;
  FColor := clWhite;
  FShowAppIcon := true;
  FCaptionStyle := csMetro;
  FSizeGripColor := $00F2BC00;
  FSystemIconColor := $00A0A0A0;
  FSystemIconColorHot := clSilver;
  FSystemIconColorDown := clGray;
  FSystemIconColorDisabled := $005A5A5A;
  FCaptionColor := $00B0A374;
  FCaptionActiveColor := $00F2BC00;
  FProgressColor := clWhite;

  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := CaptionFontChanged;
  FFont := TFont.Create;
  FFont.OnChange := CaptionFontChanged;
  FFont.Color := clWhite;
  FCaptionFont.Color := clWhite;
  FTextAlign := taRightJustify;

  if IsVista then
    fnt := 'Segoe UI'
  else
    fnt := 'Tahoma';

  FCaptionFont.Name := fnt;
  FFont.Name := fnt;
end;


//------------------------------------------------------------------------------

destructor TFormAppearance.Destroy;
begin
  FCaptionFont.Free;
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.Assign(Source: TPersistent);
begin
  if (Source is TFormAppearance) then
  begin
    FColor := (Source as TFormAppearance).Color;
    FCaptionStyle := (Source as TFormAppearance).CaptionStyle;
    FSizeGripColor := (Source as TFormAppearance).SizeGripColor;
    FSystemIconColor := (Source as TFormAppearance).SystemIconColor;
    FSystemIconColorHot := (Source as TFormAppearance).SystemIconColorHot;
    FSystemIconColorDown := (Source as TFormAppearance).SystemIconColorDown;
    FSystemIconColorDisabled := (Source as TFormAppearance).SystemIconColorDisabled;
    FCaptionColor := (Source as TFormAppearance).CaptionColor;
    FCaptionActiveColor := (Source as TFormAppearance).CaptionActiveColor;
    FCaptionFont.Assign((Source as TFormAppearance).CaptionFont);
    FFont.Assign((Source as TFormAppearance).Font);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.CaptionFontChanged(Sender: TObject);
begin
  Change;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetCaptionActiveColor(const Value: TColor);
begin
  if (FCaptionActiveColor <> Value) then
  begin
    FCaptionActiveColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetCaptionColor(const Value: TColor);
begin
  if (FCaptionColor <> Value) then
  begin
    FCaptionColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetCaptionStyle(const Value: TCaptionStyle);
begin
  if (FCaptionStyle <> Value) then
  begin
    FCaptionStyle := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetProgressColor(const Value: TColor);
begin
  if (FProgressColor <> Value) then
  begin
    FProgressColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetShowAppIcon(const Value: boolean);
begin
  if (FShowAppIcon <> Value) then
  begin
    FShowAppIcon := Value;
    Change;
  end;
end;

procedure TFormAppearance.SetSizeGripColor(const Value: TColor);
begin
  if (FSizeGripColor <> Value) then
  begin
    FSizeGripColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetSystemIconColor(const Value: TColor);
begin
  if (FSystemIconColor <> Value) then
  begin
    FSystemIconColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetTextAlign(const Value: TAlignment);
begin
  if (FTextAlign <> Value) then
  begin
    FTextAlign := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

initialization
  MetroFormTones := ClearTones;

end.
