{***************************************************************************}
{ TAdvMetroTile component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012                                               }
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

unit AdvMetroTile;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, ActnList, GDIPicture, AdvGDIP, AdvStyleIF, AdvMetroRes, Types,
  PictureContainer, ImgList
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
  // 1.0.0.0 : First release
  // 1.1.0.0 : New : Tile can be used as container for other controls
  // 1.1.1.0 : New : Support for setting the disabled color added

type
  TAdvCustomMetroTile = class;
  TAdvToolButtonStyle = (tasButton, tasCheck);
  TAdvButtonState = (absUp, absDisabled, absDown, absDropDown, absExclusive);

  TTileLayout = (tlPicLeft, tlPicRight, tlPicTop, tlPicBottom, tlPicBackground);

  TMetroTileAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FHoverBorderColor: TColor;
    FHoverTextColor: TColor;
    FDownBorderColor: TColor;
    FHoverColor: TColor;
    FDownTextColor: TColor;
    FDownColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FColor: TColor;
    FDisabledColor: TColor;
    FDisabledBorderColor: TColor;
    FDisabledTextColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;

    property HoverColor: TColor read FHoverColor write FHoverColor;
    property HoverBorderColor: TColor read FHoverBorderColor write FHoverBorderColor;
    property HoverTextColor: TColor read FHoverTextColor write FHoverTextColor;

    property DownColor: TColor read FDownColor write FDownColor;
    property DownBorderColor: TColor read FDownBorderColor write FDownBorderColor;
    property DownTextColor: TColor read FDownTextColor write FDownTextColor;

    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property DisabledBorderColor: TColor read FDisabledBorderColor write FDisabledBorderColor;
    property DisabledTextColor: TColor read FDisabledTextColor write FDisabledTextColor;
  end;

{$IFDEF DELPHI6_LVL}
  TAdvMetroTileActionLink = class(TControlActionLink)
  protected
    FClient: TAdvCustomMetroTile;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  end;
{$ENDIF}

  TAdvCustomMetroTile = class(TCustomControl, ITMSTones)
  private
    FGroupIndex: Integer;
    FDown: Boolean;
    FAllowAllUp: Boolean;
    FMouseInControl: Boolean;
    FHot: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FStyle: TAdvToolButtonStyle;
    FState: TAdvButtonState;
    FMouseDownInControl: Boolean;
    FGrouped: Boolean;
    FDragging: Boolean;
    FPropHot: Boolean;
    FUnHotTimer: TTimer;
    FInitialDown: Boolean;
    FIPicture: TGDIPPicture;
    FShowFocusRect: Boolean;
    FActive: Boolean;
    FAppearance: TMetroTileAppearance;
    FShowCircle: Boolean;
    FDefault: boolean;
    FCancel: Boolean;
    FModalResult: TModalResult;
    FCaption: TCaption;
    FPictureAutoColor: boolean;
    FPictureContainer: TPictureContainer;
    FImageList: TCustomImageList;
    FPictureAutoSize: boolean;
    FPictureMarginHorz: integer;
    FPictureMarginVert: integer;
    FLayout: TTileLayout;
    FTextMarginVert: integer;
    FTextMarginHorz: integer;
    FZoomOnHover: integer;
    FBorderStyle: TBorderStyle;
    FPictureAutoBrighten: boolean;
    procedure UnHotTimerOnTime(Sender: TObject);
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure ButtonDown;
    procedure OnPictureChanged(Sender: TObject);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetStyle(const Value: TAdvToolButtonStyle);
    procedure SetState(const Value: TAdvButtonState);
    procedure SetGrouped(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetHot: Boolean;
    procedure SetHot(const Value: Boolean);
    procedure SetIPicture(const Value: TGDIPPicture);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure OnAppearanceChanged(Sender: TObject);
    procedure SetAppearance(const Value: TMetroTileAppearance);
    procedure SetShowCircle(const Value: Boolean);
    procedure SetDefault(const Value: boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetPictureAutoColor(const Value: boolean);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureAutoSize(const Value: boolean);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetLayout(const Value: TTileLayout);
    procedure SetPictureMarginHorz(const Value: integer);
    procedure SetPictureMarginVert(const Value: integer);
    procedure SetTextMarginHorz(const Value: integer);
    procedure SetTextMarginVert(const Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetZoomOnHover(const Value: integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
{$IFDEF DELPHI6_LVL}
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
{$ENDIF}
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawButton;
    procedure DrawButtonContent(ACanvas: TCanvas; R: TRect); virtual;
    procedure DrawMetroPicture(g: TGPGraphics; Pic: TGDIPPicture; PicClr: TGPColor; DR: TRect; ColorAdapt: boolean; AutoSize: boolean);
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure RefreshContent;

    property MouseInControl: Boolean read FMouseInControl;
    property State: TAdvButtonState read FState write SetState;

    // published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;

    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default false;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Caption: TCaption read FCaption write SetCaption;
    property Default: boolean read FDefault write SetDefault default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;

    property Constraints;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Enabled;
    property Font;
    property Hot: Boolean read GetHot write SetHot default false;
    property Appearance: TMetroTileAppearance read FAppearance write SetAppearance;

    property Images: TCustomImageList read FImageList write SetImageList;
    property Layout: TTileLayout read FLayout write SetLayout default tlPicTop;
    property Picture: TGDIPPicture read FIPicture write SetIPicture;
    property PictureAutoBrighten: boolean read FPictureAutoBrighten write FPictureAutoBrighten default false;
    property PictureAutoColor: boolean read FPictureAutoColor write SetPictureAutoColor default true;
    property PictureAutoSize: boolean read FPictureAutoSize write SetPictureAutoSize default true;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    property PictureMarginVert: integer read FPictureMarginVert write SetPictureMarginVert default 2;
    property PictureMarginHorz: integer read FPictureMarginHorz write SetPictureMarginHorz default 2;


    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property ShowCircle: Boolean read FShowCircle write SetShowCircle default False;
    property Style: TAdvToolButtonStyle read FStyle write SetStyle default tasButton;

    property TextMarginHorz: integer read FTextMarginHorz write SetTextMarginHorz default 2;
    property TextMarginVert: integer read FTextMarginVert write SetTextMarginVert default 2;

    property Version: string read GetVersion write SetVersion;
    property Visible;

    property ZoomOnHover: integer read FZoomOnHover write SetZoomOnHover default 0;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetColorTones(ATones: TColorTones);
    function GetVersionNr: Integer; virtual;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMetroTile = class(TAdvCustomMetroTile)
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property Appearance;
    property BorderStyle;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property Down;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property Images;
    property Layout;
    property ModalResult;
    property Picture;
    property PictureAutoBrighten;
    property PictureAutoColor;
    property PictureAutoSize;
    property PictureContainer;
    property PictureMarginHorz;
    property PictureMarginVert;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowFocusRect;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop default False;
    property TextMarginHorz;
    property TextMarginVert;
    property Version;
    property Visible;
    property ZoomOnHover;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;


  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

implementation

uses
  ActiveX, ShellAPI, ComObj, CommCtrl;

{$I HTMLENGO.PAS}

//------------------------------------------------------------------------------

procedure DrawDottedRoundRect(graphic: TGPGraphics; R: TRect; Radius: Integer; Clr: TColor);
var
  path: TGPGraphicsPath;
  l, t, w, h, d: Integer;
  gppen: TGPPen;
begin
  if not Assigned(graphic) then
    Exit;
  path := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;
  d := Radius shl 1;
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddLine(l + radius, t, l + w - radius, t); // top
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.AddLine(l, t + h - radius, l, t + radius); // left
  path.CloseFigure();
  gppen := TGPPen.Create(ColorToARGB(Clr));
  gppen.SetDashStyle(DashStyleDot);
  graphic.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI6_LVL}

{ TAdvMetroTileActionLink }

procedure TAdvMetroTileActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvCustomMetroTile;
end;

//------------------------------------------------------------------------------

function TAdvMetroTileActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked {and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp} and (FClient.Down = (Action as TCustomAction).Checked);
end;

//------------------------------------------------------------------------------

function TAdvMetroTileActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvCustomMetroTile) and
    (TAdvCustomMetroTile(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvMetroTileActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TAdvCustomMetroTile(FClient).Down := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroTileActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TAdvCustomMetroTile(FClient).GroupIndex := Value;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{ TAdvCustomMetroTile }

constructor TAdvCustomMetroTile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FAppearance := TMetroTileAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;
  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := OnPictureChanged;
  FPictureAutoSize := true;
  FPictureAutoBrighten := false;
  FPictureMarginHorz := 2;
  FPictureMarginVert := 2;
  FTextMarginHorz := 2;
  FTextMarginVert := 2;
  FShowCircle := False;
  FZoomOnHover := 0;
  FLayout := tlPicTop;
  FBorderStyle := bsSingle;

  SetBounds(0, 0, 128, 90);
  //ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  FStyle := tasButton;
  FGroupIndex := 0;
  FGrouped := true;

  FUnHotTimer := TTimer.Create(self);
  FUnHotTimer.Interval := 1;
  FUnHotTimer.Enabled := false;
  FUnHotTimer.OnTimer := UnHotTimerOnTime;

  ShowHint := False;

  FShowFocusRect := False;
  FPictureAutoColor := true;
  TabStop := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CreateWnd;
begin
  inherited;
  FActive := FDefault;
  FDoubleBuffered := not (ControlCount > 0);
end;

//------------------------------------------------------------------------------

destructor TAdvCustomMetroTile.Destroy;
begin
  FIPicture.Free;
  FUnHotTimer.Enabled := False;
  FUnHotTimer.Free;
  FAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := true;
  FUnHotTimer.Enabled := True;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FUnHotTimer.Enabled := False;
  FMouseInControl := false;
  FHot := false;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.Loaded;
begin
  inherited;

  if (Down <> FInitialDown) then
    Down := FInitialDown;

  if not assigned(OnDblClick) then
    ControlStyle := ControlStyle - [csDoubleClicks];
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button <> mbLeft) or not Enabled or (csDesigning in ComponentState) then
    Exit;

  FMouseDownInControl := true;

  ButtonDown;

  if not FDown then
  begin
    FState := absDown;
    //Invalidate;
    RefreshContent;
  end;

  if Style = tasCheck then
  begin
    FState := absDown;
    Invalidate;
  end;

  if TabStop then
    SetFocus;

  FDragging := True;
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  NewState: TAdvButtonState;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if FDragging then
  begin
    if (not FDown) then NewState := absUp
    else NewState := absExclusive;

    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := absExclusive else NewState := absDown;

    if (Style = tasCheck) and FDown then
    begin
      NewState := absDown;
    end;

    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;

procedure TAdvCustomMetroTile.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

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

procedure TAdvCustomMetroTile.RefreshContent;
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.Paint;
begin
  DrawButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.DrawButton;
var
  R: TRect;
  clr,clrb: TColor;
begin
  R := ClientRect;
  if not Enabled then
  begin
    FState := absDisabled;
    FDragging := False;
  end
  else
  begin
    if (FState = absDisabled) then
      if FDown and (GroupIndex <> 0) then
        FState := absExclusive
      else
        FState := absUp;
  end;

  if (Style = tasCheck) and (Down) then
    FState := absDown;

  clr := Appearance.Color;
  clrb := Appearance.BorderColor;

  if not Enabled then
  begin
    clr := Appearance.DisabledColor;
    clrb := Appearance.DisabledBorderColor;
  end
  else if not (csDesigning in ComponentState) then
  begin
    if ((FMouseDownInControl and FMouseInControl) or FDown) then
    begin
      clr := Appearance.DownColor;
      clrb := Appearance.DownBorderColor;
    end
    else if FMouseInControl then
    begin
      clr := Appearance.HoverColor;
      clrb := Appearance.HoverBorderColor;
    end;
  end;

  if not FMouseInControl then
    InflateRect(R, - FZoomOnHover, -FZoomOnHover);

  Canvas.Brush.Color := clr;
  Canvas.Brush.Style := bsSolid;

  if BorderStyle = bsSingle then
    Canvas.Pen.Color := clrb
  else
    Canvas.Pen.Color := clr;

  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;

  Canvas.Rectangle(R.Left,R.Top, R.Left + R.Right - R.Left,R.Top + R.Bottom - R.Top);
  DrawButtonContent(Canvas, R);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.DrawMetroPicture(g: TGPGraphics; Pic: TGDIPPicture; PicClr: TGPColor; DR: TRect; ColorAdapt: boolean; AutoSize: boolean);
var
  Attr: TGPImageAttributes;
  ColorMatrix: TColorMatrix;
  Pen: TGPPen;
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  RF, RF2: TGPRectF;
  CR: TRect;
  rc, gc, bc: Double;
  factr: Double;
  hr: HResult;
begin
  if not Assigned(Pic) or Pic.Empty then
    Exit;

  CR := DR;

  if ShowCircle then
    InflateRect(CR, -3, -3)
  else
    InflateRect(CR, -2, -2);

  RF := MakeRect(DR.Left, DR.Top, DR.Right  - DR.Left, DR.Bottom - DR.Top);

  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  try
    // Create IStream* from global memory
    hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

    if hr = S_OK then
    begin
      pstm.Write(ms.Memory, ms.Size,@pcbWrite);

      if (ms.Size = pcbWrite) then
      begin
        Img := TGPImage.Create(pstm);

        Attr := nil;

        if FPictureAutoBrighten and FMouseInControl then
        begin
          if FMouseDownInControl then
            factr := -0.075
          else
            factr := 0.075;

          FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);
          // transformed image color
          ColorMatrix[0,0] := 1;
          ColorMatrix[1,1] := 1;
          ColorMatrix[2,2] := 1;
          ColorMatrix[3,3] := 1;

          ColorMatrix[4,0] := factr;
          ColorMatrix[4,1] := factr;
          ColorMatrix[4,2] := factr;
          ColorMatrix[4,3] := 1;
          ColorMatrix[4,4] := 1;

          Attr := TGPImageAttributes.Create;
          Attr.SetColorMatrix(ColorMatrix);
        end
        else
          if ColorAdapt then
          begin
            rc := ADVGDIP.GetRed(PicClr) / 255;
            gc := ADVGDIP.GetGreen(PicClr)/ 255;
            bc := ADVGDIP.GetBlue(PicClr)/ 255;

            FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);
            // transformed image color
            ColorMatrix[0,0] := 0;
            ColorMatrix[1,1] := 0;
            ColorMatrix[2,2] := 0;
            ColorMatrix[3,3] := 0;

            ColorMatrix[3,0] := 0;
            ColorMatrix[3,1] := 0;
            ColorMatrix[3,2] := 0;
            ColorMatrix[3,3] := 1; // <- original A

            ColorMatrix[4,0] := rc; // <- desired R
            ColorMatrix[4,1] := gc; // <- desired G
            ColorMatrix[4,2] := bc; // <- desired B
            ColorMatrix[4,3] := 0;

            Attr := TGPImageAttributes.Create;
            Attr.SetColorMatrix(ColorMatrix);
          end;

        g.DrawImage(img, RF, 0, 0, Img.Width, Img.Height, UnitPixel, Attr);

        Img.Free;
        if Assigned(Attr) then
          attr.Free;
      end;
      pstm := nil;
    end
    else
      GlobalFree(hGlobal);
  finally
    ms.Free;
  end;

  if ShowCircle then
  begin
    if Width < 26 then
    begin
      Pen := TGPPen.Create(PicClr,1)
    end
    else
    if Width <= 40 then
      Pen := TGPPen.Create(PicClr,2)
    else
      Pen := TGPPen.Create(PicClr,3);

    g.DrawEllipse(Pen,RF2);
    Pen.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.DrawButtonContent(ACanvas: TCanvas; R: TRect);
var
  Pic: TGDIPPicture;
  graphic: TGPGraphics;
  DrawFocused: Boolean;
  PicClr: TColor;
  RF: TGPRectF;
  DR,IR: TRect;
  sa,sv,fa: string;
  xs,ys,hl,ml: Integer;
  hr: TRect;
  ImgS: TSize;
  Rw, Rh: double;
  DTATTR: DWORD;
begin
  RF := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Pic := Picture;

  PicClr := Appearance.TextColor;

  if not Enabled then
  begin
    PicClr := Appearance.DisabledTextColor;
  end
  else if not (csDesigning in ComponentState) then
  begin
    if ((FMouseDownInControl and FMouseInControl) or FDown) then
      PicClr := Appearance.DownTextColor
    else if FMouseInControl then
      PicClr := Appearance.HoverTextColor;
  end;

  DR := R;

  if BorderStyle = bsSingle then
    InflateRect(DR, -1, -1);
  IR := DR;

  ImgS.cx := 0;
  ImgS.cy := 0;

  if Assigned(Pic) and not Pic.Empty then
  begin
    pic.GetImageSizes;
    ImgS.cx := pic.Width;
    ImgS.cy := pic.Height;

    if PictureAutoSize and (pic.Width > 0) and (pic.Height > 0) then
    begin
      Rw := (IR.Right - IR.Left - PictureMarginHorz * 2) / pic.Width;
      Rh := (IR.Bottom - IR.Top - PictureMarginVert * 2) / pic.Height;
      if (Rw < Rh) then
      begin
        ImgS.cx := Round(Rw * ImgS.cx);
        ImgS.cy := Round(Rw * ImgS.cy);
      end
      else
      begin
        ImgS.cx := Round(Rh * ImgS.cx);
        ImgS.cy := Round(Rh * ImgS.cy);
      end;
    end;
  end;

  DTATTR := DT_VCENTER or DT_CENTER;

  case Layout of
  tlPicLeft:
    begin
      IR.Left := DR.Left + PictureMarginHorz;
      IR.Right := IR.Left + ImgS.cx;
      IR.Top := PictureMarginVert + ((DR.Bottom - DR.Top - 2 * PictureMarginVert) - Imgs.cy) div 2;
      IR.Bottom := IR.Top + ImgS.cy;

      DR.Left := IR.Right;
      DTATTR := DT_VCENTER or DT_LEFT;
    end;
  tlPicRight:
    begin
      IR.Right := DR.Right - PictureMarginHorz;
      IR.Left := IR.Right - ImgS.cx;
      IR.Top := PictureMarginVert + ((DR.Bottom - DR.Top - 2 * PictureMarginVert) - Imgs.cy) div 2;
      IR.Bottom := IR.Top + ImgS.cy;

      DR.Right := IR.Left;
      DTATTR := DT_VCENTER or DT_RIGHT;
    end;
  tlPicTop:
    begin
      IR.Top := DR.Top + PictureMarginVert;
      IR.Bottom := IR.Top + ImgS.cy;
      IR.Left := PictureMarginHorz + ((DR.Right - DR.Left - 2 * PictureMarginHorz) - Imgs.cx) div 2;
      IR.Right := IR.Left + ImgS.cx;

      DR.Top := IR.Bottom;
      DTATTR := DT_CENTER;
    end;
  tlPicBottom:
    begin
      IR.Bottom := DR.Bottom - PictureMarginVert;
      IR.Top := IR.Bottom - ImgS.cy;
      IR.Left := PictureMarginHorz + ((DR.Right - DR.Left - 2 * PictureMarginHorz) - Imgs.cx) div 2;
      IR.Right := IR.Left + ImgS.cx;

      DR.Bottom := IR.Top;
      DTATTR := DT_BOTTOM or DT_CENTER;
    end;
  end;

  graphic := TGPGraphics.Create(ACanvas.Handle);
  graphic.SetSmoothingMode(SmoothingModeAntiAlias);

  DrawFocused := (GetFocus = self.Handle) and ShowFocusRect;

  DrawMetroPicture(graphic, Pic, MakeColor(255, PicClr), IR, PictureAutoColor, PictureAutoSize);

  if DrawFocused then
    DrawDottedRoundRect(graphic, Rect(R.Left + 2, r.Top + 2, r.Right - 5, r.Bottom - 5), 0, clGray);

  graphic.Free;

  if (Caption <> '') then
  begin
    ACanvas.Font.Assign(Font);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := PicClr;

    InflateRect(DR, -TextMarginHorz, -TextMarginVert);

    if pos('</', Caption) > 0 then
    begin
      HTMLDrawEx(Canvas, Caption, DR, FImageList, -1,-1,-1,-1,2,false,false,false,false,false,false,true,1.0,clBlue,
      clNone,clNone, clGray, sa, sv, fa, xs, ys, hl, ml, hr, nil, FPictureContainer, 2);
    end
    else
    begin
      DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), DR, DT_SINGLELINE or DTATTR or DT_END_ELLIPSIS);
      DR.Bottom := DR.Bottom - ACanvas.TextHeight('gh') - 2;
    end;
  end;

  if (csDesigning in ComponentState) and (Pic.Empty) then
  begin
    ACanvas.Pen.Color := Appearance.HoverBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(R);
    Exit;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LParam(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;


//------------------------------------------------------------------------------


procedure TAdvCustomMetroTile.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetAppearance(
  const Value: TMetroTileAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetDown(Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FInitialDown := Value;

  if (FGroupIndex = 0) and (Style = tasButton) then
    Value := False;

  if (Style = tasCheck) then
  begin
    FDown := Value;
    if FDown then
      FState := absDown
    else
      FState := absUp;
    Invalidate;
    Exit;
  end;

  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = absUp then
        Invalidate;
      FState := absExclusive
    end
    else
    begin
      FState := absUp;
      Invalidate;
    end;
    if Value then UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetState(const Value: TAdvButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetStyle(const Value: TAdvToolButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetTextMarginHorz(const Value: integer);
begin
  if (FTextMarginHorz <> Value) then
  begin
    FTextMarginHorz := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetTextMarginVert(const Value: integer);
begin
  if (FTextMarginVert <> Value) then
  begin
    FTextMarginVert := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetColorTones(ATones: TColorTones);
begin
  Appearance.Color := ATones.Background.BrushColor;
  Appearance.HoverColor := ATones.Hover.BrushColor;
  Appearance.DownColor := ATones.Selected.BrushColor;

  Appearance.TextColor := ATones.Background.TextColor;
  Appearance.HoverTextColor := ATones.Hover.TextColor;
  Appearance.DownTextColor := ATones.Selected.TextColor;

  Appearance.BorderColor := ATones.Background.BorderColor;
  Appearance.HoverBorderColor := ATones.Hover.BorderColor;
  Appearance.DownBorderColor := ATones.Selected.BorderColor;

  Appearance.DisabledColor := ATones.Disabled.BrushColor;
  Appearance.DisabledBorderColor := ATones.Disabled.BorderColor;
  Appearance.DisabledTextColor := ATones.Disabled.TextColor;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetGrouped(const Value: Boolean);
begin
  FGrouped := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.ButtonDown;
begin
end;

//------------------------------------------------------------------------------


procedure TAdvCustomMetroTile.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvCustomMetroTile;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvCustomMetroTile(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := absUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI6_LVL}

procedure TAdvCustomMetroTile.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      //Self.ImageIndex := ImageIndex;
    end;
end;

//------------------------------------------------------------------------------

function TAdvCustomMetroTile.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvMetroTileActionLink;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TAdvCustomMetroTile.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvCustomMetroTile.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetVersion(const Value: string);
begin

end;

procedure TAdvCustomMetroTile.SetZoomOnHover(const Value: integer);
begin
  if (FZoomOnHover <> Value) and (Value >= 0) then
  begin
    FZoomOnHover := Value;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomMetroTile.GetHot: Boolean;
begin
  Result := FPropHot;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetHot(const Value: Boolean);
var
  OldV: Boolean;
begin
  OldV := FPropHot;
  FPropHot := Value;
  if (State <> absUp) then
    FPropHot := false;

  FPropHot := false;
  if OldV <> FPropHot then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.UnHotTimerOnTime(Sender: TObject);
var
  CurP: TPoint;
begin
  GetCursorPos(CurP);
  CurP := ScreenToClient(CurP);
  if (not PtInRect(ClientRect, CurP)) then
  begin
    FUnHotTimer.Enabled := False;
    FMouseInControl := false;
    FHot := false;

    if Enabled then
      Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetParent(AParent: TWinControl);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetPictureAutoColor(const Value: boolean);
begin
  if (FPictureAutoColor <> Value) then
  begin
    FPictureAutoColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetPictureAutoSize(const Value: boolean);
begin
  if (FPictureAutoSize <> Value) then
  begin
    FPictureAutoSize := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetPictureMarginHorz(const Value: integer);
begin
  if (FPictureMarginHorz <> Value) then
  begin
    FPictureMarginHorz := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetPictureMarginVert(const Value: integer);
begin
  if (FPictureMarginVert <> Value) then
  begin
    FPictureMarginVert := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
  Invalidate;
end;

procedure TAdvCustomMetroTile.SetIPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomMetroTile.SetLayout(const Value: TTileLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.OnPictureChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.Click;
var
  Form: TCustomForm;
begin

  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMHintShow(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetShowCircle(const Value: Boolean);
begin
  if (FShowCircle <> Value) then
  begin
    FShowCircle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetDefault(const Value: boolean);
begin
  FDefault := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.SetShowFocusRect(const Value: Boolean);
begin
  if (FShowFocusRect <> Value) then
  begin
    FShowFocusRect := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TAdvCustomMetroTile then
      FActive := Sender = Self
    else
      FActive := FDefault;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if
      (((CharCode = VK_RETURN) and FActive) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  inherited;

  if (Key = #32) or (Key = #13) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;

    Click;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.DoEnter;
begin
  inherited;
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.DoExit;
begin
  inherited;
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomMetroTile.WMLButtonUp(var Msg: TWMLButtonDown);
var
  DoClick: Boolean;
begin
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  FMouseDownInControl := false;
  //Invalidate;

  if FDragging then
  begin
    FDragging := False;
    DoClick := (Msg.XPos >= 0) and (Msg.XPos < ClientWidth) and (Msg.YPos >= 0) and (Msg.YPos <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := absUp;
      //FMouseInControl := False;
      FHot := false;

      if Style = tasCheck then
      begin
        SetDown(not FDown);
        FState := absUp;
      end;

      if DoClick and not (FState in [absExclusive, absDown]) then
        RefreshContent;// Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Invalidate;
      end
      else
      begin
        if FDown then
          FState := absExclusive;
        Invalidate;
      end;
    //if DoClick then Click;
    UpdateTracking;
  end;

  inherited;
  //Invalidate;
end;

//------------------------------------------------------------------------------

{ TMetroTileAppearance }

constructor TMetroTileAppearance.Create;
begin
  inherited;
  FColor := clWhite;
  FTextColor := clBlack;
  FBorderColor := clHighlight;
  FHoverColor := $00F2BC00;
  FHoverBorderColor := $00F2BC00;
  FHoverTextColor := clWhite;
  FDownColor := $00B0A374;
  FDownBorderColor := $00B0A374;
  FDownTextColor := clWhite;
  FDisabledColor := clSilver;
  FDisabledBorderColor := clSilver;
  FDisabledTextColor := clGray;
end;

//------------------------------------------------------------------------------

procedure TMetroTileAppearance.Assign(Source: TPersistent);
begin
  if (Source is TMetroTileAppearance) then
  begin
    FColor := (Source as TMetroTileAppearance).Color;
    FHoverColor := (Source as TMetroTileAppearance).HoverColor;
    FDownColor := (Source as TMetroTileAppearance).DownColor;

    FDisabledColor := (Source as TMetroTileAppearance).DisabledColor;
    FDisabledBorderColor := (Source as TMetroTileAppearance).DisabledBorderColor;
    FDisabledTextColor := (Source as TMetroTileAppearance).DisabledTextColor;

    FDownColor := (Source as TMetroTileAppearance).DownColor;
    FDownBorderColor := (Source as TMetroTileAppearance).DownBorderColor;
    FDownTextColor := (Source as TMetroTileAppearance).DownTextColor;

    FHoverColor := (Source as TMetroTileAppearance).HoverColor;
    FHoverBorderColor := (Source as TMetroTileAppearance).HoverBorderColor;
    FHoverTextColor := (Source as TMetroTileAppearance).HoverTextColor;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TMetroTileAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TMetroTileAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TMetroTileAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TMetroTileAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{TWinCtrl}

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

end.
