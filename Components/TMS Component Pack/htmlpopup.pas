{**************************************************************************}
{ THTMLPopup component                                                     }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2001-2013                                                    }
{   TMS Software                                                           }
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

unit HTMLPopup;

interface

{$I TMSDEFS.INC}
{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}

uses
  Classes, Windows, Graphics, Messages, Controls, Forms, SysUtils,
  PictureContainer, ImgList, ExtCtrls, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.2.0.0 : New property AutoSizeType added

  // 1.3.0.0 : New : Event OnClick event added
  //         : New : Property HideType & TimeOut added
  //         : Improved : DFM property storage

  // 1.3.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.4.0.0 : New : Support for PNG images via images in associated PictureContainer

  

type
  TAnchorClickEvent = procedure(Sender: TObject; Anchor: string) of object;

  TShadeDirection = (sdHorizontal, sdVertical);
  TAutoSizeType = (asdHeight, asdWidth, asdBoth);

  THideType = (htMouseLeave, htTimeOut);

  { THTMLPopup }
  THTMLPopupWindow = class(THintWindow)
  private
    FImageCache: THTMLPictureCache;
    FContainer: TPictureContainer;
    FHoverLink: Integer;
    FHover: Boolean;
    FHoverRect: TRect;
    FOnAnchorClick: TAnchorClickEvent;
    FOnClick: TNotifyEvent;
    FShadeEnable: Boolean;
    FShadeSteps: Integer;
    FShadeStartColor: TColor;
    FShadeEndColor: TColor;
    FColor: TColor;
    FShadeDirection: TShadeDirection;
    FBorderSize: Integer;
    FImages: TImageList;
    FAutoHide: Boolean;
    FAutoSize: Boolean;
    FAutoSizeType: TAutoSizeType;
    FAlwaysOnTop: Boolean;
    FMarginY: Integer;
    FMarginX: Integer;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetHover(const Value: Boolean);
    procedure SetAutoSizeEx(const Value: Boolean);
    procedure SetAlwaysOnTop(const Value: Boolean);
  protected
    procedure PaintShading(FromColor,ToColor: TColor; Steps: Integer; Direction: Boolean);
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop default false;
    property AutoHide: Boolean read FAutoHide write FAutoHide default false;
    property AutoSize: Boolean read FAutoSize write SetAutoSizeEx default false;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write FAutoSizeType default asdHeight;
    property BorderSize: Integer read FBorderSize write FBorderSize default 0;
    property Color: TColor read FColor write FColor default clInfoBk;
    property Images: TImageList read FImages write FImages;
    property MarginX: Integer read FMarginX write FMarginX default 4;
    property MarginY: Integer read FMarginY write FMarginY default 4;
    property ShadeEnable: Boolean read FShadeEnable write FShadeEnable default false;
    property ShadeStartColor: TColor read FShadeStartColor write FShadeStartColor default clSilver;
    property ShadeEndColor: TColor read FShadeEndColor write FShadeEndColor default clGray;
    property ShadeSteps: Integer read FShadeSteps write FShadeSteps default 40;
    property ShadeDirection: TShadeDirection read FShadeDirection write FShadeDirection default sdHorizontal;
    property Hover: Boolean read FHover write SetHover default false;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property OnAnchorClick: TAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLPopup = class(TComponent)
  private
    FOwner: TWinControl;
    FHTMLPopupWindow: THTMLPopupWindow;
    FHeight: Integer;
    FWidth: Integer;
    FContainer: TPictureContainer;
    FText: TStringList;
    FLeft: Integer;
    FTop: Integer;
    FHover: Boolean;
    FOnAnchorClick: TAnchorClickEvent;
    FOnClick: TNotifyEvent;
    FShadeEnable: Boolean;
    FShadeSteps: Integer;
    FShadeStartColor: TColor;
    FShadeEndColor: TColor;
    FColor: TColor;
    FFont: TFont;
    FShadeDirection: TShadeDirection;
    FBorderSize: Integer;
    FImages: TImageList;
    FAutoHide: Boolean;
    FAutoSize: Boolean;
    FAutoSizeType: TAutoSizeType;
    FAlwaysOnTop: Boolean;
    FMarginY: Integer;
    FMarginX: Integer;
    FRollUpSpace: Integer;
    FTimer: TTimer;
    FTimeOut: integer;
    FHideType: THideType;
    procedure SetText(const Value: TStringList);
    procedure SetImages(const Value: TImageList);
    procedure SetFont(const Value: TFont);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure WindowAnchorClick(Sender: TObject; Anchor:string);
    procedure WindowClick(Sender: TObject);
    procedure WindowTimer(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreatePopup;
    procedure TextChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure RollUp;
    procedure RollDown;
    procedure Hide;
    property PopupWindow: THTMLPopupWindow read FHTMLPopupWindow;
  published
    property AlwaysOnTop: Boolean read FAlwaysOnTop write FAlwaysOnTop default false;
    property AutoHide: Boolean read FAutoHide write FAutoHide default false;
    property AutoSize: Boolean read FAutoSize write FAutoSize default false;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write FAutoSizeType default asdHeight;
    property BorderSize: Integer read FBorderSize write FBorderSize default 0;
    property Color: TColor read FColor write FColor default clInfoBk;
    property Font: TFont read FFont write SetFont;
    property Hover: Boolean read FHover write FHover default false;
    property HideType: THideType read FHideType write FHideType default htMouseLeave;
    property Images: TImageList read FImages write SetImages;
    property MarginX: Integer read FMarginX write FMarginX default 4;
    property MarginY: Integer read FMarginY write FMarginY default 4;
    property PopupLeft: Integer read FLeft write FLeft default 0;
    property PopupTop: Integer read FTop write FTop default 0;
    property RollUpSpace: Integer read FRollUpSpace write FRollUpSpace default 0;
    property ShadeEnable: Boolean read FShadeEnable write FShadeEnable default false;
    property ShadeStartColor: TColor read FShadeStartColor write FShadeStartColor default clSilver;
    property ShadeEndColor: TColor read FShadeEndColor write FShadeEndColor default clGray;
    property ShadeSteps: Integer read FShadeSteps write FShadeSteps default 40;
    property ShadeDirection: TShadeDirection read FShadeDirection write FShadeDirection default sdHorizontal;
    property Text: TStringList read FText write SetText;
    property TimeOut: integer read FTimeOut write FTimeOut default 2000;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupWidth: Integer read FWidth write FWidth default 200;
    property PopupHeight: Integer read FHeight write FHeight default 200;
    property OnAnchorClick: TAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  Commctrl, ShellApi;

{$I HTMLENGO.PAS}

{ THTMLPopupWindow }

constructor THTMLPopupWindow.Create(AOwner: TComponent);
begin
  inherited;
  FImageCache := THTMLPictureCache.Create;
  FHoverLink := -1;
  FHover := True;
  FImages := nil;
  FAutoSizeType := asdBoth;
end;

destructor THTMLPopupWindow.Destroy;
begin
  FImageCache.Free;
  inherited;
end;

procedure THTMLPopupWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure THTMLPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style - WS_BORDER;
  if FAlwaysOnTop then
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure THTMLPopupWindow.PaintShading(FromColor,ToColor: TColor; Steps: Integer; Direction: Boolean);
var
  diffr,startr,endr:longint;
  diffg,startg,endg:longint;
  diffb,startb,endb:longint;
  rstepr:real;
  rstepg:real;
  rstepb:real;
  rstepw:real;
  i,stepw:word;

begin
  if Steps = 0 then
    Steps := 1;

  startr := (FromColor and $0000ff);
  startg := (FromColor and $00ff00) shr 8;
  startb := (FromColor and $ff0000) shr 16;
  endr := (ToColor and $0000ff);
  endg := (ToColor and $00ff00) shr 8;
  endb := (ToColor and $ff0000) shr 16;

  diffr := endr-startr;
  diffg := endg-startg;
  diffb := endb-startb;

  rstepr := diffr/steps;
  rstepg := diffg/steps;
  rstepb := diffb/steps;

  if Direction then
    rstepw := width/Steps
  else
    rstepw := height/Steps;

  with Canvas do
  begin
    for i := 0 to steps-1 do
    begin
      endr := startr+round(rstepr*i);
      endg := startg+round(rstepg*i);
      endb := startb+round(rstepb*i);
      stepw := round(i*rstepw);

      pen.Color := endr + (endg shl 8) + (endb shl 16);

      brush.Color := pen.Color;
      if Direction then
        Rectangle(stepw,0,stepw+round(rstepw)+1,height)
      else
        Rectangle(0,stepw,width,stepw+round(rstepw)+1);
    end;
  end;
end;

procedure THTMLPopupWindow.SetAutoSizeEx(const Value: Boolean);
var
  R, rd, hr : TRect;
  Anchor,Stripped,FocusAnchor:string;
  XSize,YSize,HyperLinks,MouseLink:integer;
begin
  FAutoSize := Value;

  if not FAutoSize then
    Exit;

  R := ClientRect;
  RD := ClientRect;

  InflateRect(R,-FBorderSize,-FBorderSize);

  RD.Left := R.Left + 6;
  RD.Top := R.Top + 2;
//  RD.Bottom := R.bottom - 8;
//  RD.Right := R.Right - 4;

  //RD.Right := RD.Left + 4096;
  //RD.Bottom := RD.Top + 4096;
  
  if FAutoSizeType in [asdWidth, asdBoth] then
    RD.Right := RD.Left + 4096
  else
    RD.Right := RD.Left;
    
  if FAutoSizeType in [asdHeight, asdBoth] then
    RD.Bottom := RD.Top + 4096
  else
    RD.Bottom := RD.Top;

  Canvas.Font.Assign(Self.Font);

  HTMLDrawEx(Canvas,Caption,rd,FImages,0,0,-1,FHoverLink,1,False,False,False,False,True,FHover,True,
             1.0,clBlue,clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,XSize,YSize,
             HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

  Width := XSize + 16 + BorderSize *2;
  Height := YSize + 16 + BorderSize * 2;
end;

procedure THTMLPopupWindow.Paint;
var
  DC: HDC;
  R, rd, hr : TRect;
  Brush, SaveBrush: HBRUSH;
  Anchor,Stripped,FocusAnchor:string;
  XSize,YSize,HyperLinks,MouseLink:integer;

  procedure DCFrame3D(var R: TRect; const TopLeftColor, BottomRightColor: TColor);
  var
    Pen, SavePen: HPEN;
    P: array[0..2] of TPoint;
  begin
    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(TopLeftColor));
    SavePen := SelectObject(DC, Pen);
    P[0] := Point(R.Left, R.Bottom-2);
    P[1] := Point(R.Left, R.Top);
    P[2] := Point(R.Right-1, R.Top);
    PolyLine(DC, P, 3);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);

    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(BottomRightColor));
    SavePen := SelectObject(DC, Pen);
    P[0] := Point(R.Left, R.Bottom-1);
    P[1] := Point(R.Right-1, R.Bottom-1);
    P[2] := Point(R.Right-1, R.Top-1);
    PolyLine(DC, P, 3);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
  end;

begin
  DC := Canvas.Handle;
  R := ClientRect;
  RD := ClientRect;

  // Background
  if FShadeEnable then
    PaintShading(FShadeStartColor,FShadeEndColor,FShadeSteps,FShadeDirection = sdHorizontal)
  else
  begin
    Brush := CreateSolidBrush(ColorToRGB(Color));

    SaveBrush := SelectObject(DC, Brush);
    FillRect(DC, R, Brush);
    SelectObject(DC, SaveBrush);
    DeleteObject(Brush);
  end;

  // Border
  if FBorderSize = 0 then
   DCFrame3D(R, cl3DLight, cl3DDkShadow)
  else
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := clBtnFace;

    Canvas.Rectangle(0,0,R.Right, FBorderSize);
    Canvas.Rectangle(R.Right - FBorderSize,0,R.Right, R.Bottom);
    Canvas.Rectangle(0,R.Bottom - FBorderSize,R.Right, R.Bottom);
    Canvas.Rectangle(0,0,FBorderSize,R.Bottom);

    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(0,R.Bottom);
    Canvas.LineTo(0,0);
    Canvas.LineTo(R.Right - 1,0);
    Canvas.Pen.Color := clGray;
    Canvas.LineTo(R.Right - 1,R.Bottom - 1);
    Canvas.LineTo(0,R.Bottom - 1);

    Canvas.MoveTo(FBorderSize,R.Bottom - FBorderSize);
    Canvas.LineTo(FBorderSize,FBorderSize);
    Canvas.LineTo(R.Right - FBorderSize,FBorderSize);
    Canvas.Pen.Color := clWhite;

    Canvas.LineTo(R.Right - FBorderSize,R.Bottom - FBorderSize);
    Canvas.LineTo(FBorderSize,R.Bottom - FBorderSize);

    InflateRect(R,-FBorderSize,-FBorderSize);
  end;

  RD.Left := R.Left + MarginX;
  RD.Top := R.Top + MarginY;
  RD.Bottom := R.bottom - MarginY;
  RD.Right := R.Right - MarginX;

  Canvas.Font.Assign(Self.Font);

  HTMLDrawEx(Canvas,Caption,rd,FImages,0,0,-1,FHoverLink,1,False,False,False,False,True,FHover,True,
             1.0,clBlue,clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,XSize,YSize,
             HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
end;

constructor THTMLPopup.Create(AOwner: TComponent);
begin
  inherited;
  FText := TStringList.Create;
  FText.OnChange := TextChanged;
  FOwner := TWinControl(AOwner);
  FHTMLPopupWindow := nil;
  FWidth := 200;
  FHeight := 200;
  FColor := clInfoBk;
  FShadeStartColor := clSilver;
  FShadeEndColor := clGray;
  FShadeSteps := 40;
  FImages := nil;
  FMarginX := 4;
  FMarginY := 4;
  FFont := TFont.Create;
  FTimer := nil;
  FTimeOut := 2000;  
end;

procedure THTMLPopup.Hide;
begin
  if Assigned(FHTMLPopupWindow) then
    FHTMLPopupWindow.Visible := False;
end;

procedure THTMLPopup.TextChanged(Sender: TObject);
var
  s: string;
  i: Integer;
begin

  if Assigned(FHTMLPopupWindow) then
  begin
    s := '';
    for i := 1 to FText.Count do
      s := s + FText.Strings[i - 1];
    FHTMLPopupWindow.Caption := s;
  end;
end;

procedure THTMLPopup.CreatePopup;
var
  i: Integer;
  s: string;
begin
  if not Assigned(FHTMLPopupWindow) then
    FHTMLPopupWindow := THTMLPopupWindow.Create(FOwner);

  FHTMLPopupWindow.Visible := False;
  FHTMLPopupWindow.Parent := nil;
  FHTMLPopupWindow.ParentWindow := FOwner.Handle;
  s := '';
  for i := 1 to FText.Count do
    s := s + FText.Strings[i - 1];

  FHTMLPopupWindow.Caption := s;

  FHTMLPopupWindow.AlwaysOnTop := FAlwaysOnTop;
  FHTMLPopupWindow.Width := FWidth;
  FHTMLPopupWindow.Height := FHeight;
  FHTMLPopupWindow.Left := FLeft;
  FHTMLPopupWindow.Top := FTop;
  FHTMLPopupWindow.PictureContainer := FContainer;
  FHTMLPopupWindow.Hover := FHover;
  FHTMLPopupWindow.Images := FImages;
  FHTMLPopupWindow.ShadeEnable := FShadeEnable;
  FHTMLPopupWindow.ShadeStartColor := FShadeStartColor;
  FHTMLPopupWindow.ShadeEndColor := FShadeEndColor;
  FHTMLPopupWindow.ShadeDirection := FShadeDirection;
  FHTMLPopupWindow.ShadeSteps := FShadeSteps;
  FHTMLPopupWindow.MarginX := MarginX;
  FHTMLPopupWindow.MarginY := MarginY;
  FHTMLPopupWindow.Font.Assign(FFont);

  FHTMLPopupWindow.AutoHide := FAutoHide and (FHideType = htMouseLeave);
  FHTMLPopupWindow.BorderSize := FBorderSize;
  FHTMLPopupWindow.Color := Color;

  FHTMLPopupWindow.OnAnchorClick := WindowAnchorClick;
  FHTMLPopupWindow.OnClick := WindowClick;

  FHTMLPopupWindow.AutoSizeType := FAutoSizeType;
  FHTMLPopupWindow.AutoSize := FAutoSize;
end;

procedure THTMLPopup.Show;
begin
  CreatePopup;
  FHTMLPopupWindow.Visible := True;

  if FAutoHide and (FHideType = htTimeOut) then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(self);

    FTimer.Interval := FTimeout;
    FTimer.OnTimer := WindowTimer;
    FTimer.Enabled := true;
  end;
end;

procedure THTMLPopup.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
end;

destructor THTMLPopup.Destroy;
begin
  FText.Free;
  FFont.Free;
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  inherited;
end;

procedure THTMLPopupWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
  RD,hr,r: TRect;
  hl,ml: Integer;
  XSize, YSize: Integer;
  a,s,fa: string;
  Anchor: string;
begin
  pt := ScreenToClient(Point(message.Xpos,message.YPos));

  r := ClientRect;
  RD.Left := R.Left + MarginX;
  RD.Top := R.Top + MarginY;
  RD.Bottom := r.bottom - MarginY;
  RD.Right := R.Right - MarginX;

  InflateRect(RD,-FBorderSize,-FBorderSize);

  Anchor := '';

  Canvas.Font.Assign(Self.Font);  

  if HTMLDrawEx(Canvas,Caption,rd,FImages,pt.X,pt.Y,-1,-1,1,True,False,False,False,False,False,True,
             1.0,clBlue,clNone,clNone,clGray,a,s,fa,XSize,YSize,
             hl,ml,hr,FImageCache,FContainer,0) then
  begin
    Anchor := a;
  end;

  if (Anchor <> '') then
  begin
    Cursor := crHandPoint;
    if (FHoverLink <> hl) and FHover then
      InvalidateRect(Handle,@hr,True);
    FHoverLink := hl;
    FHoverRect := hr;
  end
  else
  begin
    Cursor := crDefault;
    if (FHoverLink <> -1) and (FHover) then
      InvalidateRect(Handle,@FHoverRect,True);;
    FHoverLink := -1;
  end;

  Message.Result := HTCLIENT;
end;


procedure THTMLPopupWindow.WMSetFocus(var Msg: TWMSetFocus);
var
  pt: TPoint;
  RD,hr,r: TRect;
  hl,ml: Integer;
  XSize, YSize: Integer;
  a,s,fa: string;
  Anchor: string;
begin
  msg.Result := 0;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  r := ClientRect;

  RD.Left := R.Left + MarginX;
  RD.Top := R.Top + MarginY;
  RD.Bottom := R.bottom - MarginY;
  RD.Right := R.Right - MarginX;

  InflateRect(RD,-FBorderSize,-FBorderSize);

  Anchor := '';

  Canvas.Font.Assign(Self.Font);

  if HTMLDrawEx(Canvas,Caption,rd,FImages,pt.X,pt.Y,-1,-1,1,True,False,False,False,False,False,True,
             1.0,clBlue,clNone,clNone,clGray,a,s,fa,XSize,YSize,
             hl,ml,hr,FImageCache,FContainer,0) then
  begin
    Anchor := a;
    if Assigned(FOnAnchorClick) then
      FOnAnchorClick(Self,a);
  end;

  Windows.SetFocus(ParentWindow);

  if (FHoverLink <> -1) and (FHover) then
    InvalidateRect(Handle,@FHoverRect,True);;
end;

procedure THTMLPopup.WindowAnchorClick(Sender: TObject; Anchor: string);
begin
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(Self,Anchor);
end;

procedure THTMLPopup.WindowClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure THTMLPopup.WindowTimer(Sender: TObject);
begin
  Hide;
  FTimer.Enabled := false;
end;

procedure THTMLPopup.RollDown;
var
  t: DWORD;
  r: TRect;
begin
  if not Assigned(FHTMLPopupWindow) then
    Exit;

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0);

  while FHTMLPopupWindow.Top < r.Bottom do
  begin
    t := GetTickCount;
    while GetTickCount - t < 50 do
      Application.ProcessMessages;

    FHTMLPopupWindow.Top := FHTMLPopupWindow.Top + 20;
  end;
  Hide;
end;

procedure THTMLPopup.RollUp;
var
  t: DWORD;
  r: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0);

  FTop := r.Bottom;

  CreatePopup;
  FLeft := r.Right - FHTMLPopupWindow.Width - 20;
  Show;

  while FHTMLPopupWindow.Top + FHTMLPopupWindow.Height > r.Bottom do
  begin
    t := GetTickCount;
    while GetTickCount - t < 50 do
      Application.ProcessMessages;

    if FHTMLPopupWindow.Top - 20 < r.Bottom - FHTMLPopupWindow.Height - FRollUpSpace then
      FHTMLPopupWindow.Top := r.Bottom - FHTMLPopupWindow.Height - FRollUpSpace
    else
      FHTMLPopupWindow.Top := FHTMLPopupWindow.Top - 20;

  end;

  FHTMLPopupWindow.Top := r.Bottom - FHTMLPopupWindow.Height - FRollUpSpace;
end;

procedure THTMLPopupWindow.SetHover(const Value: Boolean);
begin
  FHover := Value;
  FHoverLink := -1;
end;

procedure THTMLPopup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  inherited;
end;

procedure THTMLPopup.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure THTMLPopup.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure THTMLPopupWindow.CMMouseLeave(var Message: TMessage);
begin
  if FAutoHide then
    Self.Visible := False;
end;

procedure THTMLPopupWindow.SetAlwaysOnTop(const Value: Boolean);
begin
  FAlwaysOnTop := Value;
  RecreateWnd;
end;

function THTMLPopup.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLPopup.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLPopup.SetVersion(const Value: string);
begin

end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
