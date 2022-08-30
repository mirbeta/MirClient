{*********************************************************************}
{ TEditBtn &  TAsgUnitEditBtn component for TAdvStringGrid            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1998-2014                                    }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit ASGEdit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Graphics, Buttons, Forms, Mask, AdvXPVS, ImgList
  {$IFDEF DELPHI7_LVL}
  , Types
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type

  TNumGlyphs = Buttons.TNumGlyphs;

  TAdvSpeedButton = class(TSpeedButton)
  private
    FIsWinXP: Boolean;
    FFlat: Boolean;
    FHot: Boolean;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
  protected
    procedure Paint; override;
  public
    property Hot: Boolean read FHot write FHot;
  published
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
    property Flat: Boolean read FFlat write FFlat;
  end;

  TAsgSpeedButton = class(TSpeedButton)
  private
    FPicture: TPicture;
    FImages: TCustomImageList;
    FImageIndex: integer;
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPicture(const Value: TPicture);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Picture: TPicture read FPicture write SetPicture;
  end;


{ TAsgEditButton }

  TAsgEditButton = class (TWinControl)
  private
    FButton: TAdvSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    FFlat: Boolean;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    function CreateButton: TAdvSpeedButton;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure SetFlat(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property Button: TAdvSpeedButton read FButton;
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Ctl3D;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;
    property ButtonCaption:string read GetCaption write SetCaption;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property Flat: Boolean read FFlat write SetFlat;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

{ TAsgEditBtn }

  TAsgEditBtn = class(TCustomMaskEdit)
  private
    FUnitSize: Integer;
    FRightAlign: Boolean;
    FButton: TAsgEditButton;
    FEditorEnabled: Boolean;
    FOnClickBtn: TNotifyEvent;
    FButtonWidth: Integer;
    FIsWinXP: Boolean;
    function GetMinHeight: Integer;
    procedure SetEditRect;
    procedure SetGlyph(value:tBitmap);
    function GetGlyph:TBitmap;
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure SetRightAlign(value : boolean);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure SetButtonWidth(const Value: Integer);
    procedure SetIsWinXP(const Value: Boolean);
  protected
    procedure BtnClick(Sender: TObject); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TAsgEditButton read FButton;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property IsWinXP: Boolean read FIsWinXP write SetIsWinXP;
    property ButtonCaption:string read GetCaption write SetCaption;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightAlign:boolean read FRightAlign write SetRightAlign;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property Height;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI2005_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
  end;

  TUnitChangeEvent = procedure(Sender: TObject; NewUnit:string) of object;

  TAsgUnitEditBtn = class(TAsgEditBtn)
  private
    FUnitID:string;
    FUnits:TStringList;
    FUnitChanged: TUnitChangeEvent;
    procedure SetUnitSize(value: Integer);
    function GetUnitSize: Integer;
    procedure SetUnits(value:tstringlist);
    procedure SetUnitID(value:string);
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    function GetTextAndUnit: string;
    procedure SetTextAndUnit(const Value: string);
  protected
    procedure BtnClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TextAndUnit: string read GetTextAndUnit write SetTextAndUnit;
  published
    property Units: TStringList read FUnits write SetUnits;
    property UnitID: string read FUnitID write SetUnitID;
    property UnitSpace: Integer read GetUnitSize write SetUnitSize;
    property OnUnitChanged: TUnitChangeEvent read FUnitChanged write FUnitChanged;
  end;

implementation


{ TAsgEditButton }         

constructor TAsgEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButton := CreateButton;
  Glyph := nil;
  Width := 20;
  Height := 25;
  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;
end;

function TAsgEditButton.CreateButton: TAdvSpeedButton;
begin
  Result := TAdvSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.Parent := Self;
  Result.Caption := '';
end;

procedure TAsgEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TAsgEditButton.AdjustSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or (csLoading in ComponentState) then
    Exit;
//  if W < 15 then W := 15;
  FButton.SetBounds (0, 0, W, H);
end;

procedure TAsgEditButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TAsgEditButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TAsgEditButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if (Sender = FButton) then FOnClick(Self);

    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TAsgEditButton.BtnClick(Sender: TObject);
begin
end;

procedure TAsgEditButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TAsgEditButton.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TAsgEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TAsgEditButton.SetCaption(value:string);
begin
  FButton.Caption := Value;
end;

function TAsgEditButton.GetCaption:string;
begin
  Result := FButton.Caption;
end;

function TAsgEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TAsgEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
 FButton.NumGlyphs := Value;
end;

{ TAsgEditBtn }

constructor TAsgEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TAsgEditButton.Create (Self);
  FButton.Width := 18;
  FButton.Height := 18;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnClick := BtnClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FRightAlign := False;
  FUnitSize := 0;
end;

destructor TAsgEditBtn.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TAsgEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if FRightAlign then
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or ES_RIGHT
  else
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TAsgEditBtn.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TAsgEditBtn.SetGlyph(value:TBitmap);
begin
  FButton.Glyph := Value;
end;

function TAsgEditBtn.GetGlyph:TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TAsgEditBtn.SetCaption(value:string);
begin
  FButton.ButtonCaption := Value;
end;

function TAsgEditBtn.GetCaption:string;
begin
  Result := FButton.ButtonCaption;
end;

procedure TAsgEditBtn.SetEditRect;
var
  Loc: TRect;
begin
  if not HandleAllocated then
    Exit;

  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 5 - FUnitsize;

  if FUnitSize > 0 then
    Loc.Right := Loc.Right - 3;

  if self.BorderStyle = bsNone then
  begin
    Loc.Top := 3;
    Loc.Left := 3;
  end
  else
  begin
    Loc.Top := 1;
    Loc.Left := 1;
  end;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TAsgEditBtn.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist:integer;
begin
  inherited;
  if BorderStyle = bsNone then
    Dist := 1
  else
    Dist := 5;

  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - Dist, 0, FButton.Width, Height - Dist)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TAsgEditBtn.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  {Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 +2;}
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

procedure TAsgEditBtn.BtnClick (Sender: TObject);
begin
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Sender);
end;

procedure TAsgEditBtn.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAsgEditBtn.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAsgEditBtn.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TAsgEditBtn.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TAsgEditBtn.SetRightAlign(value: boolean);
begin
  if FRightAlign <> Value then
  begin
    FRightAlign := Value;
    Recreatewnd;
  end;
end;

procedure TAsgEditBtn.WMPaint(var Msg: TWMPAINT);
var
  hdc: THandle;
begin
  inherited;
  hdc := GetDC(Handle);
  Releasedc(Handle,hdc);
end;

procedure TAsgEditBtn.KeyPress(var Key: Char);
begin
  inherited;
end;


procedure TAsgEditBtn.WMChar(var Message: TWMChar);
begin
  if not FEditorEnabled then
    Exit;
  Inherited;
end;

procedure TAsgEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F4) and
     (GetKeyState(vk_control) and $8000 = 0) and
     (GetKeyState(vk_lmenu) and $8000 = 0) and
     (GetKeyState(vk_rmenu) and $8000 = 0) then
   begin
     BtnClick(self);
   end;
end;

procedure TAsgEditBtn.SetIsWinXP(const Value: Boolean);
begin
  FIsWinXP := Value;
  FButton.FButton.IsWinXP := Value;
end;

procedure TAsgEditBtn.SetButtonWidth(const Value: Integer);
begin
  FButtonWidth := Value;
  FButton.Width := Value;
end;


procedure TAsgEditButton.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  FButton.Flat := FFlat;
end;

{  TAsgUnitEditBtn }
procedure  TAsgUnitEditBtn.BtnClick(Sender: TObject);
var
  popmenu: THandle;
  pt: TPoint;
  i: Integer;
begin
  if Assigned(OnClickBtn) then
    OnClickBtn(Self);

  pt := ClientToScreen(point(0,0));
  popmenu := CreatePopupMenu;

  for i := 1 to FUnits.Count do
    InsertMenu(popmenu,$FFFFFFFF,MF_BYPOSITION ,i,pchar(fUnits.Strings[i-1]));

  TrackPopupMenu(popmenu,TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x+ClientWidth-15,pt.y+ClientHeight,0,self.handle,nil);
  DestroyMenu(popmenu);
end;

constructor TAsgUnitEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FUnitSize := 20;
  FUnits := TStringList.Create;
  FRightAlign := True;
end;

destructor TAsgUnitEditBtn.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

procedure TAsgUnitEditBtn.SetTextAndUnit(const Value: string);
var
  i, vali: integer;

begin
  for i := 0 to Units.Count - 1 do
  begin
    vali := pos(Units[i], Value);

    if (vali = length(Value) - length(Units[i]) + 1) and (vali > 0) then
    begin
      Text := Copy(Value, 1, vali - 1);
      UnitID := Copy(Value, vali, length(Value));
      break;
    end;
  end;
end;

procedure  TAsgUnitEditBtn.SetUnitID(Value: string);
begin
  FUnitID := Value;
  Repaint;
end;

procedure TAsgUnitEditBtn.SetUnits(Value: TStringList);
begin
  if Assigned(Value) then
    FUnits.Assign(Value);
end;

function TAsgUnitEditBtn.GetTextAndUnit: string;
begin
  Result := Text + UnitID;
end;

function TAsgUnitEditBtn.GetUnitSize: Integer;
begin
  Result := FUnitSize;
end;

procedure TAsgUnitEditBtn.SetUnitSize(value: integer);
begin
  FUnitSize := Value;
  SetEditRect;
  Repaint;
end;

procedure  TAsgUnitEditBtn.WMCommand(var Message: TWMCommand);
var
  chg: boolean;
begin
  if (Message.NotifyCode <> EN_CHANGE) then
  begin
    if (message.ItemID <= FUnits.Count) then
    begin
      chg := UnitID <> FUnits.Strings[message.itemID - 1];
      UnitID := FUnits.Strings[message.itemID - 1];

      if Assigned(OnUnitChanged) and chg then
        OnUnitChanged(Self,UnitID);
    end;
  end;
end;

procedure  TAsgUnitEditBtn.WMPaint(var Msg: TWMPAINT);
var
  hdc: THandle;
  oldfont: THandle;
  r: TRect;
begin
  inherited;
  hdc := GetDC(Handle);
  r.Left := ClientWidth - FButton.Width - 3 - fUnitsize;
  r.Right := r.left + FUnitSize;
  r.Top := 2;
  r.Bottom := ClientHeight;
  oldfont := SelectObject(HDC,Font.Handle);

  DrawText(hdc,pchar(FUnitID),Length(FUnitID),r,DT_LEFT or DT_EDITCONTROL);

  SelectObject(hdc,oldfont);
  ReleaseDC(Handle,hdc);
end;

{ TAdvSpeedButton }

procedure TAdvSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  Hot := True;
  Invalidate;
end;


procedure TAdvSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Hot := False;
  Invalidate;
end;


procedure TAdvSpeedButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  PaintRect,ARect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  HTheme: THandle;

begin
  Canvas.Font := Self.Font;
  PaintRect := Rect(0, 0, Width, Height);

  if (TAsgEditButton(Owner).ButtonColor <> clNone) then
  begin
    FFlat := true;
    Canvas.Brush.Color := TAsgEditButton(Owner).ButtonColor;

    if FHot then
      Canvas.Brush.Color := TAsgEditButton(Owner).ButtonColorHot;

    if (FState in [bsDown, bsExclusive]) then
      Canvas.Brush.Color := TAsgEditButton(Owner).ButtonColorDown;

    Canvas.Pen.Color := Canvas.Brush.Color;
    ARect := ClientRect;
    ARect.Left := ARect.Left + 2;
    Canvas.FillRect(ClientRect);
    ARect.Left := ARect.Left - 2;

    Canvas.Brush.Color := TAsgEditButton(Owner).ButtonTextColor;

    if FHot then
    begin
      Canvas.Brush.Color := TAsgEditButton(Owner).ButtonTextColorHot;
    end;

    if (FState in [bsDown, bsExclusive]) then
    begin
      Canvas.Brush.Color := TAsgEditButton(Owner).ButtonTextColorDown;
    end;

    Canvas.Pen.Color := Canvas.Brush.Color;
  end
  else
  begin
    if FIsWinXP and IsThemeActive then
    begin
      HTheme := OpenThemeData(Parent.Handle,'button');

      if FState in [bsDown, bsExclusive] then
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@PaintRect,nil)
      else
        if Hot then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@PaintRect,nil)
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@PaintRect,nil);

      CloseThemeData(HTheme);
    end
    else
    begin
      if not FFlat then
      begin
        DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
        if FState in [bsDown, bsExclusive] then
          DrawFlags := DrawFlags or DFCS_PUSHED;
        DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
      end
      else
      begin
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          BF_MIDDLE or BF_RECT);
        InflateRect(PaintRect, -1, -1);
      end;
    end;
  end;

  if not (FState in [bsDown, bsExclusive]) then
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  if Assigned(Glyph) then
  begin
    if not Glyph.Empty then
    begin
      Glyph.Transparent := True;
      Offset.X := 0;
      Offset.Y := 0;
      if Glyph.Width < Width then
        Offset.X := (Width - Glyph.Width) shr 1;
      if Glyph.Height < Height then
        Offset.Y := (Height - Glyph.Height) shr 1;

      if FState = bsDown then
        Canvas.Draw(Offset.X + 1 ,Offset.Y + 1,Glyph)
      else
        Canvas.Draw(Offset.X ,Offset.Y,Glyph)
    end;
  end;

  Canvas.Font.Color := clBlack;
  SetBkMode(Canvas.Handle,Windows.TRANSPARENT);

  if FState = bsDown then
    OffsetRect(PaintRect,1,1);

  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), PaintRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
end;


{ TAsgSpeedButton }

constructor TAsgSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
end;

destructor TAsgSpeedButton.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TAsgSpeedButton.Paint;
var
  x,y: integer;
begin
  inherited;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    x := (Width - Images.Width) div 2;
    y := (Height - Images.Height) div 2;
    Images.Draw(Canvas,x,y,ImageIndex);
  end;

  if Assigned(Picture) and Assigned(Picture.Graphic) and not Picture.Graphic.Empty then
  begin
    x := (Width - Picture.Width) div 2;
    y := (Height - Picture.Height) div 2;
    Canvas.Draw(x,y,Picture.Graphic);
  end;
end;

procedure TAsgSpeedButton.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TAsgSpeedButton.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TAsgSpeedButton.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  Invalidate;
end;

end.

