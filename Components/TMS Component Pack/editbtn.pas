{********************************************************************}
{ TEDITBTN component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998-2013                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit EditBtn;

{$I TMSDEFS.INC}

interface

uses Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Dialogs, Menus, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.5.1.0 : New : Event OnUnitChanged added in TUnitEditBtn
  // 1.5.2.0 : Improved : Disabled drawing of attached button

type
  TNumGlyphs = Buttons.TNumGlyphs;

  { TAdvSpeedButton }

  TAdvSpeedButton = class(TSpeedButton)
  private
    FEtched: Boolean;
    FFocused: Boolean;
    procedure SetEtched(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
  protected
    procedure Paint; override;
  public
  published
    property Etched: Boolean read FEtched write SetEtched;
    property Focused: Boolean read FFocused write SetFocused;
  end;

  { TEditButton }

  TEditButton = class (TWinControl)
  private
    FButton: TAdvSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    function CreateButton: TAdvSpeedButton;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseUp (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AdjustWinSize (var W: Integer; var H: Integer);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Ctl3D;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ButtonCaption:string read GetCaption write SetCaption;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
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

{ TEditBtn }

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TEditBtn = class(TCustomEdit)
  private
    FUnitSize : integer;
    FRightAlign:boolean;
    FButton: TEditButton;
    FEditorEnabled: Boolean;
    FOnClickBtn:TNotifyEvent;
    FFlat: boolean;
    FEtched: boolean;
    FFocusBorder: boolean;
    FMouseInControl: boolean;
    FReturnIsTab:boolean;
    function GetMinHeight: Integer;
    procedure SetEditRect;
    procedure SetGlyph(value:tBitmap);
    function GetGlyph:TBitmap;
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure SetRightAlign(const Value : boolean);
    procedure SetFlat(const Value : boolean);
    procedure SetEtched(const Value : boolean);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawButtonBorder;
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure BtnClick (Sender: TObject); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key:char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TEditButton read FButton;
  published
    property Align;
    property Constraints;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property ReturnIsTab:boolean read fReturnIsTab write fReturnIsTab;
    property Enabled;
    property Font;
    property Flat:boolean read fFlat write SetFlat;
    property Etched:boolean read fEtched write SetEtched;
    property FocusBorder:boolean read fFocusBorder write fFocusBorder;
    property Glyph:TBitmap read GetGlyph write SetGlyph;
    property ButtonCaption:string read GetCaption write SetCaption;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightAlign:boolean read fRightAlign write SetRightAlign;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property Height;
    property Width;
    property Version: string read GetVersion write SetVersion;
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
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
  end;

  TUnitChangeEvent = procedure(Sender: TObject; NewUnit:string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TUnitEditBtn = class(TEditBtn)
  private
    FUnitChanged: TUnitChangeEvent;
    FUnitID: string;
    FUnits: TStringlist;
    function GetUnitSize: Integer;
    procedure SetUnitSize(value: Integer);
    procedure SetUnits(value: TStringList);
    procedure SetUnitID(value:string);
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
  protected
    procedure BtnClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Units: TStringlist read FUnits write SetUnits;
    property UnitID: string read FUnitID write SetUnitID;
    property UnitSpace: Integer read GetUnitSize write SetUnitSize;
    property OnUnitChanged: TUnitChangeEvent read FUnitChanged write FUnitChanged;
  end;

procedure DrawBitmapTransp(canvas:tcanvas;bmp:tbitmap;bkcolor:tcolor;r:trect);
  

implementation


procedure DrawBitmapTransp(canvas:tcanvas;bmp:tbitmap;bkcolor:tcolor;r:trect);
var
  tmpbmp: TBitmap;
  srcColor: TColor;
  tgtrect: TRect;
begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Height := bmp.Height;
    TmpBmp.Width := bmp.Width;
    tgtrect.left := 0;
    tgtrect.top := 0;
    tgtrect.right := r.right-r.left;
    tgtrect.bottom := r.bottom-r.Top;
    TmpBmp.Canvas.Brush.Color:=bkcolor;
    srcColor := bmp.canvas.pixels[0,0];
    TmpBmp.Canvas.BrushCopy(tgtrect,bmp,tgtrect,srcColor);
    Canvas.CopyRect(r, TmpBmp.Canvas, tgtrect);
  finally
    TmpBmp.Free;
  end;
end;

{ TAdvSpeedButton }
procedure TAdvSpeedButton.SetEtched(const Value:boolean);
begin
  if value <> FEtched then
  begin
    FEtched := Value;
    Invalidate;
  end;
end;

procedure TAdvSpeedButton.SetFocused(const Value:boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
  end;
end;

procedure TAdvSpeedButton.Paint;
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  r: TRect;
  BtnFaceBrush: HBRUSH;

begin
  if not Flat then
    inherited Paint
  else
  begin
    r := ClientRect;
    FillRect(canvas.handle,ClientRect,canvas.Brush.Handle);

    BtnFaceBrush:=CreateSolidBrush(GetSysColor(COLOR_BTNFACE));

    Inflaterect(r,-1,-1);

    FillRect(canvas.handle, r, BtnFaceBrush);

    DeleteObject(BtnFaceBrush);

    if fFocused then
    begin
      r.Bottom := r.Bottom + 1;
      r.Right := r.Right + 1;
      DrawEdge(canvas.handle, r, Edge[fEtched], BF_RECT or flags[fState=bsDown]);
    end;

    r := ClientRect;

    if Assigned(Glyph) then
    begin
      if not glyph.empty then
      begin
        inflaterect(r,-3,-3);
        if fstate=bsdown then
          offsetrect(r,1,1);
        DrawBitmapTransp(canvas,glyph,ColorToRGB(clBtnFace),r);
      end;
    end;

    if (Caption<>'') then
    begin
      inflaterect(r,-3,-1);
      if fstate=bsdown then
        offsetrect(r,1,1);
      windows.SetBKMode(Canvas.Handle,windows.TRANSPARENT);
      DrawText(Canvas.Handle,pchar(Caption),length(Caption),r,DT_CENTER);
    end;
  end;
end;

{ TEditButton }
procedure TEditButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

constructor TEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  FButton := CreateButton;
  Glyph := nil;
  Width := 16;
  Height := 25;
end;

function TEditButton.CreateButton: TAdvSpeedButton;
begin
  Result := TAdvSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseUp;

  Result.Visible := True;
  Result.Enabled := True;
  Result.Parent := Self;
  Result.Caption:='';
end;

procedure TEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TEditButton.AdjustWinSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 16 then W := 16;
  FButton.SetBounds (0, 0, W, H);
end;

procedure TEditButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustWinSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TEditButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustWinSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TEditButton.BtnMouseUp (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if (Sender = FButton) then
      FOnClick(Self);

    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;


procedure TEditButton.BtnClick(Sender: TObject);
begin
 {
 if (Sender=FButton) then FOnClick(Self);
 }
end;

procedure TEditButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustWinSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TEditButton.GetGlyph: TBitmap;
begin
 Result:=FButton.Glyph;
end;

procedure TEditButton.SetGlyph(Value: TBitmap);
begin
 FButton.Glyph:=Value;
end;

procedure TEditButton.SetCaption(value:string);
begin
 FButton.Caption:=value;
end;

function TEditButton.GetCaption:string;
begin
 result:=FButton.Caption;
end;

function TEditButton.GetNumGlyphs: TNumGlyphs;
begin
 result:=FButton.NumGlyphs;
end;

procedure TEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
 FButton.NumGlyphs := Value;
end;

{ TSpinEdit }

constructor TEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TEditButton.Create (Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnClick := BtnClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FRightAlign := False;
  FUnitSize:=0;
end;

destructor TEditBtn.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  if fRightAlign then
   Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or ES_RIGHT
  else
   Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TEditBtn.CreateWnd;
begin
 inherited CreateWnd;
 SetEditRect;
end;

procedure TEditBtn.SetGlyph(value:TBitmap);
begin
 FButton.Glyph:=Value;
end;

function TEditBtn.GetGlyph:TBitmap;
begin
 Result:=FButton.Glyph;
end;

procedure TEditBtn.SetCaption(value:string);
begin
 FButton.ButtonCaption:=value;
end;

function TEditBtn.GetCaption:string;
begin
 result:=FButton.ButtonCaption;
end;

procedure TEditBtn.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3 - FUnitsize;
  if self.BorderStyle=bsNone then
   begin
    Loc.Top := 2;
    Loc.Left := 2;
   end
  else
   begin
    Loc.Top := 1;
    Loc.Left := 1;
   end;
  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TEditBtn.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist,FlatCorr:integer;
begin
  inherited;
  if (BorderStyle=bsNone) then Dist:=2 else Dist:=5;
  if fFlat then Dist:=3;
  if fFlat then FlatCorr:=1 else FlatCorr:=0;

  MinHeight := GetMinHeight;

 { text edit bug: if size to less than minheight, then edit ctrl does
                  not display the text }

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - Dist,1+ FlatCorr, FButton.Width+FlatCorr, Height - Dist-FlatCorr)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TEditBtn.GetMinHeight: Integer;
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
  ReleaseDC(Self.Handle, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  {Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 +2;}
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

procedure TEditBtn.BtnClick (Sender: TObject);
begin
 if Assigned(FOnClickBtn) then
   FOnClickBtn(self);
end;

procedure TEditBtn.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TEditBtn.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TEditBtn.CMExit(var Message: TCMExit);
begin
  inherited;
  DrawBorders;
end;


procedure TEditBtn.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Enabled := self.Enabled and not ReadOnly;
end;

procedure TEditBtn.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
  DrawBorders;
end;

procedure TEditBtn.CMMouseEnter(var Message: TMessage);
begin
 inherited;
 if not FMouseInControl and Enabled then
   begin
    FMouseInControl := True;
    DrawBorders;
   end;
end;

procedure TEditBtn.CMMouseLeave(var Message: TMessage);
begin
 inherited;
 if FMouseInControl and Enabled then
   begin
    FMouseInControl := False;
    DrawBorders;
   end;
end;

procedure TEditBtn.SetRightAlign(const Value: boolean);
begin
 if fRightAlign<>value then
  begin
   fRightAlign:=value;
   recreatewnd;
  end;
end;

procedure TEditBtn.SetFlat(const Value: boolean);
begin
 if fFlat<>value then
  begin
   fFlat:=value;
   fButton.fButton.flat:=fFlat;
   if fFlat then BorderStyle:=bsNone;
   Invalidate;
   if not fFlat then recreatewnd;
  end;
end;

procedure TEditBtn.SetEtched(const Value: boolean);
begin
 if fEtched<>value then
  begin
   fEtched:=value;
   fButton.fButton.Etched:=value;
   Invalidate;
  end;
end;


function TEditBtn.Is3DBorderControl: Boolean;
begin
 if csDesigning in ComponentState then
   Result:=false
 else
   Result:=FMouseInControl or (Screen.ActiveControl = Self);
 result:=result and fFocusBorder;
end;

function TEditBtn.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result:=Enabled
  else
    Result:=FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TEditBtn.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  if Is3DBorderControl then
   BtnFaceBrush:=CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
   BtnFaceBrush:=CreateSolidBrush(ColorToRGB((parent as TWinControl).brush.color));

  WindowBrush:=CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
      begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
//      FrameRect(DC, ARect, WindowBrush);
      end
    else
      begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
//      FrameRect(DC, ARect, WindowBrush);
      end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

procedure TEditBtn.DrawButtonBorder;
begin
  FButton.FButton.Focused := Is3DBorderButton;
end;

procedure TEditBtn.DrawBorders;
var
  DC: HDC;
begin
  if not fFlat then exit;
  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    DrawButtonBorder;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TEditBtn.WMPaint(var Msg: TWMPAINT);
begin
 inherited;
 DrawBorders;
end;

procedure TEditBtn.WMNCPaint(var Message: TMessage);
begin
 inherited;
 DrawBorders;
end;

procedure TEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key=vk_return) and FReturnIsTab then
  begin
    postmessage(self.handle,wm_keydown,VK_TAB,0);
    key:=0;
  end;

  if (Key = VK_F4) and
     (GetKeyState(vk_control) and $8000 = 0) and
     (GetKeyState(vk_lmenu) and $8000 = 0) and
     (GetKeyState(vk_rmenu) and $8000 = 0) then

  begin
    if Assigned(OnClickBtn) then
      OnClickBtn(Self);
  end;

  inherited;
end;

procedure TEditBtn.KeyPress(var Key: char);
begin
  if Key = #13 then
    Key := #0
  else
    inherited Keypress(Key);
end;

function TEditBtn.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TEditBtn.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TEditBtn.SetVersion(const Value: string);
begin

end;

{ TUnitEditBtn }
procedure TUnitEditBtn.BtnClick(Sender: TObject);
var
  popmenu: THandle;
  pt: TPoint;
  i: integer;
begin
  pt := ClientToScreen(point(0,0));
  popmenu := CreatePopupMenu;

  for i := 1 to FUnits.Count do
    InsertMenu(popmenu,$FFFFFFFF,MF_BYPOSITION ,i,pchar(fUnits.Strings[i-1]));
  Trackpopupmenu(popmenu,TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x+ClientWidth-15,pt.y+ClientHeight,0,self.handle,nil);

  DestroyMenu(popmenu);

  if Assigned(FOnClickBtn) then
    FOnClickBtn(Sender);
end;

constructor TUnitEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FUnitSize := 20;
  FUnits := TStringList.Create;
  FRightAlign := true;
end;

destructor TUnitEditBtn.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

procedure TUnitEditBtn.SetUnitID(value: string);
begin
  FUnitID := Value;
  Repaint;
end;

procedure TUnitEditBtn.SetUnits(value: TStringList);
begin
  if Assigned(value) then
    FUnits.Assign(value);
end;

function TUnitEditBtn.GetUnitSize: Integer;
begin
  Result := FUnitSize;
end;

procedure TUnitEditBtn.SetUnitSize(value: integer);
begin
  FUnitSize := Value;
  SetEditRect;
  Repaint;
end;

procedure TUnitEditBtn.WMCommand(var Message: TWMCommand);
begin
  UnitID := FUnits.Strings[message.itemID - 1];

  if Assigned(OnUnitChanged) then
    OnUnitChanged(Self,UnitID);
end;

procedure TUnitEditBtn.WMPaint(var Msg: TWMPAINT);
var
  hdc: THandle;
  oldfont: THandle;
  r: TRect;
begin
  inherited;
  hdc := GetDC(Self.Handle);
  r.Left := ClientWidth - FButton.Width - 3 - fUnitsize;
  r.Right := r.left + FUnitSize;
  r.Top := 2;
  r.Bottom := ClientHeight;
  oldfont := SelectObject(hdc,Font.handle);
  Drawtext(hdc,PChar(fUnitID),length(fUnitID),r,DT_LEFT or DT_EDITCONTROL);
  SelectObject(hdc,oldfont);
  ReleaseDC(self.Handle,hdc);
end;




end.

