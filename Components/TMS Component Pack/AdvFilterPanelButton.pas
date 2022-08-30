{***************************************************************************}
{ TAdvFilterPanelButton component                                           }
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

unit AdvFilterPanelButton;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, ActnList, GDIPicture, AdvGDIP, AdvStyleIF {, AdvMetroRes};

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : First release

type
  TAdvCustomFilterPanelButton = class;
  TAdvToolButtonStyle = (tasButton, tasCheck);
  TAdvButtonState = (absUp, absDisabled, absDown, absDropDown, absExclusive);

  TFilterButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPictureColor: TColor;
    FPictureColorHover: TColor;
    FPictureColorDown: TColor;
    FMonochrome: boolean;
    procedure SetPictureColor(const Value: TColor);
    procedure SetMonochrome(const Value: boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Monochrome: boolean read FMonochrome write SetMonochrome default true;
    property PictureColor: TColor read FPictureColor write SetPictureColor default clBlack;
    property PictureColorHover: TColor read FPictureColorHover write FPictureColorHover default $00F2BC00;
    property PictureColorDown: TColor read FPictureColorDown write FPictureColorDown default $00B0A374;
  end;

  TAdvFilterPanelButtonActionLink = class(TControlActionLink)
  protected
    FClient: TAdvCustomFilterPanelButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    function IsCaptionLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
  end;

  TAdvCustomFilterPanelButton = class(TCustomControl, ITMSTones)
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
    FAppearance: TFilterButtonAppearance;
    FDefault: boolean;
    FCancel: Boolean;
    FModalResult: TModalResult;
    FCaption: TCaption;
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
    procedure SetAppearance(const Value: TFilterButtonAppearance);
    procedure SetDefault(const Value: boolean);
    procedure SetCaption(const Value: TCaption);
  protected
    procedure SetParent(AParent: TWinControl); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DrawButton;
    procedure DrawButtonContent(ACanvas: TCanvas; R: TRect); virtual;
    procedure DrawMetroPicture(g: TGPGraphics; Pic: TGDIPPicture; PicClr: TGPColor; DR: TRect; DownState: boolean);
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

    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default true;
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

    property Appearance: TFilterButtonAppearance read FAppearance write SetAppearance;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property Picture: TGDIPPicture read FIPicture write SetIPicture;
    property PopupMenu;
    property ShowHint;
    property Style: TAdvToolButtonStyle read FStyle write SetStyle default tasButton;
    property Version: string read GetVersion write SetVersion;
    property Visible;

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
  TAdvFilterPanelButton = class(TAdvCustomFilterPanelButton)
  published
    property Action;
    property AllowAllUp;
    property Anchors;
    property Appearance;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property Down;
    property Enabled;
    property Font;
    property GroupIndex;
    property ModalResult;
    property Picture;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowFocusRect;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop default True;
    property Version;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

implementation

uses
  ActiveX;

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


{ TAdvFilterPanelButtonActionLink }

procedure TAdvFilterPanelButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvCustomFilterPanelButton;
end;

//------------------------------------------------------------------------------

function TAdvFilterPanelButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked;
end;

//------------------------------------------------------------------------------


function TAdvFilterPanelButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked {and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp} and (FClient.Down = (Action as TCustomAction).Checked);
end;

//------------------------------------------------------------------------------

function TAdvFilterPanelButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvCustomFilterPanelButton) and
    (TAdvCustomFilterPanelButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvFilterPanelButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    TAdvCustomFilterPanelButton(FClient).Caption := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvFilterPanelButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TAdvCustomFilterPanelButton(FClient).Down := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvFilterPanelButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TAdvCustomFilterPanelButton(FClient).GroupIndex := Value;
end;


//------------------------------------------------------------------------------

{ TAdvCustomFilterPanelButton }

constructor TAdvCustomFilterPanelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  FAppearance := TFilterButtonAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;
  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := OnPictureChanged;

  SetBounds(0, 0, 33, 33);
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

  FShowFocusRect := true;
  TabStop := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CreateWnd;
begin
  inherited;
  FActive := FDefault;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomFilterPanelButton.Destroy;
begin
  FIPicture.Free;
  FUnHotTimer.Enabled := False;
  FUnHotTimer.Free;
  FAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMDialogChar(var Message: TCMDialogChar);
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

procedure TAdvCustomFilterPanelButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMMouseEnter(var Message: TMessage);
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

procedure TAdvCustomFilterPanelButton.CMMouseLeave(var Message: TMessage);
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

procedure TAdvCustomFilterPanelButton.Loaded;
begin
  inherited;

  if (Down <> FInitialDown) then
    Down := FInitialDown;

  if not assigned(OnDblClick) then
    ControlStyle := ControlStyle - [csDoubleClicks];
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.MouseDown(Button: TMouseButton;
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

procedure TAdvCustomFilterPanelButton.MouseMove(Shift: TShiftState; X,
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

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.WMPaint(var Message: TWMPaint);
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

procedure TAdvCustomFilterPanelButton.RefreshContent;
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.Paint;
begin
  DrawButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.DrawButton;
var
  R: TRect;
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

  DrawButtonContent(Canvas, R);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.DrawMetroPicture(g: TGPGraphics; Pic: TGDIPPicture; PicClr: TGPColor; DR: TRect; DownState: boolean);
var
  Attr: TGPImageAttributes;
  ColorMatrix: TColorMatrix;
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  RF: TGPRectF;
  CR: TRect;
  rc, gc, bc: Double;
  hr: HResult;
  cx,cy: integer;

begin
  CR := DR;

  InflateRect(CR, -1, -1);

  if not Assigned(Pic) or Pic.Empty then
    Exit;

  pic.GetImageSizes;

  cx := CR.Left + (CR.Right - CR.Left - pic.Width) div 2;
  cy := CR.Top + (CR.Bottom - CR.Top - pic.Height) div 2;

  RF := MakeRect(cx,cy, pic.Width, pic.Height);

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

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size, @pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Appearance.Monochrome then
      begin
        rc := ADVGDIP.GetRed(PicClr) / 255;
        gc := ADVGDIP.GetGreen(PicClr) / 255;
        bc := ADVGDIP.GetBlue(PicClr) / 255;

        FillChar(ColorMatrix, sizeof(ColorMatrix), 0);

        // transformed image color
        ColorMatrix[3,3] := 1; // <- original A

        ColorMatrix[4,0] := rc; // <- desired R
        ColorMatrix[4,1] := gc; // <- desired G
        ColorMatrix[4,2] := bc; // <- desired B

        Attr := TGPImageAttributes.Create;
        Attr.SetColorMatrix(ColorMatrix);

        g.DrawImage(img, RF, 0, 0, pic.Width, pic.Height, UnitPixel, Attr);

        attr.Free;
      end
      else
      begin
        if DownState then
        begin
          RF.x := RF.X + 1;
          RF.Y := RF.Y + 1;
        end;
        g.DrawImage(Img, Round(RF.x), Round(RF.y));
      end;

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.DrawButtonContent(ACanvas: TCanvas; R: TRect);
var
  Pic: TGDIPPicture;
  graphic: TGPGraphics;
  DrawFocused: Boolean;
  PicClr: TColor;
  RF: TGPRectF;
  DR: TRect;
  VC: DWORD;
  isDown: boolean;
begin
  RF := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Pic := Picture;
  PicClr := Appearance.PictureColor;

  isDown := false;

  if not Enabled then
    PicClr := clGray
  else if not (csDesigning in ComponentState) then
  begin
    if ((FMouseDownInControl and FMouseInControl) or FDown) then
    begin
      PicClr := Appearance.PictureColorDown;
      isDown := true;
    end
    else if FMouseInControl then
      PicClr := Appearance.PictureColorHover;
  end;

  DR := ClientRect;

  if Caption <> '' then
  begin
    ACanvas.Font.Assign(Font);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := PicClr;
    if Pic.Empty then
      VC := DT_VCENTER
    else
      VC := DT_BOTTOM;

    DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), DR, DT_SINGLELINE or DT_CENTER or DT_END_ELLIPSIS or VC);
    DR.Bottom := DR.Bottom - ACanvas.TextHeight('gh') - 2;
  end;

  if (csDesigning in ComponentState) and (Pic.Empty) then
  begin
    ACanvas.Pen.Color := Appearance.PictureColorHover;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(R);
    Exit;
  end;

  graphic := TGPGraphics.Create(ACanvas.Handle);
  graphic.SetSmoothingMode(SmoothingModeAntiAlias);

  DrawFocused := (GetFocus = self.Handle) and ShowFocusRect;

  DrawMetroPicture(graphic, Pic, MakeColor(255, PicClr), DR, isDown);

  if DrawFocused then
    DrawDottedRoundRect(graphic, Rect(R.Left, r.Top, r.Right - 1, r.Bottom - 1), 0, clGray);

  graphic.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.UpdateExclusive;
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


//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.UpdateTracking;
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

procedure TAdvCustomFilterPanelButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetAppearance(
  const Value: TFilterButtonAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetDown(Value: Boolean);
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

procedure TAdvCustomFilterPanelButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetState(const Value: TAdvButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetStyle(const Value: TAdvToolButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetColorTones(ATones: TColorTones);
begin
  Appearance.PictureColor := ATones.Background.TextColor;
  Appearance.PictureColorHover := ATones.Hover.BrushColor;
  Appearance.PictureColorDown := ATones.Selected.BrushColor;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetGrouped(const Value: Boolean);
begin
  FGrouped := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.ButtonDown;
begin
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvCustomFilterPanelButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvCustomFilterPanelButton(Message.LParam);
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

procedure TAdvCustomFilterPanelButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);

  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      //Self.ImageIndex := ImageIndex;
      Self.Caption := Caption;
    end;
end;

//------------------------------------------------------------------------------

function TAdvCustomFilterPanelButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvFilterPanelButtonActionLink;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TAdvCustomFilterPanelButton.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvCustomFilterPanelButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvCustomFilterPanelButton.GetHot: Boolean;
begin
  Result := FPropHot;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetHot(const Value: Boolean);
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

procedure TAdvCustomFilterPanelButton.UnHotTimerOnTime(Sender: TObject);
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

procedure TAdvCustomFilterPanelButton.SetParent(AParent: TWinControl);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetIPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.OnPictureChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.Click;
var
  Form: TCustomForm;
begin

  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMHintShow(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------


procedure TAdvCustomFilterPanelButton.SetDefault(const Value: boolean);
begin
  FDefault := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.SetShowFocusRect(const Value: Boolean);
begin
  if (FShowFocusRect <> Value) then
  begin
    FShowFocusRect := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TAdvCustomFilterPanelButton then
      FActive := Sender = Self
    else
      FActive := FDefault;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.CMDialogKey(var Message: TCMDialogKey);
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

procedure TAdvCustomFilterPanelButton.KeyPress(var Key: Char);
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

procedure TAdvCustomFilterPanelButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.DoEnter;
begin
  inherited;
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.DoExit;
begin
  inherited;
  RefreshContent;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomFilterPanelButton.WMLButtonUp(var Msg: TWMLButtonDown);
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

{ TFilterButtonAppearance }

constructor TFilterButtonAppearance.Create;
begin
  inherited;
  FPictureColor := clBlack;
  FPictureColorHover := $00F2BC00;
  FPictureColorDown := $00B0A374;
  FMonoChrome := true;
end;

//------------------------------------------------------------------------------

procedure TFilterButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TFilterButtonAppearance) then
  begin
    FPictureColor := (Source as TFilterButtonAppearance).PictureColor;
    FPictureColorHover := (Source as TFilterButtonAppearance).PictureColorHover;
    FPictureColorDown := (Source as TFilterButtonAppearance).PictureColorDown;
    FMonochrome := (Source as TFilterButtonAppearance).Monochrome;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TFilterButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TFilterButtonAppearance.SetMonochrome(const Value: boolean);
begin
  if (FMonochrome <> Value) then
  begin
    FMonochrome := Value;
    Changed;
  end;
end;

procedure TFilterButtonAppearance.SetPictureColor(const Value: TColor);
begin
  if (FPictureColor <> Value) then
  begin
    FPictureColor := Value;
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
