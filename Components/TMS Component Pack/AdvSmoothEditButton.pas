{*********************************************************************}
{ TAdvSmoothEditBtn component                                         }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 2000 - 2012                                  }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit AdvSmoothEditButton;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Dialogs, Menus, AdvSmoothEdit, AdvSmoothXPVS, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 8; // Build nr.

  // version history
  // v1.3.0.2 : fixed issue with EditorEnabled = false
  // v1.3.1.0 : Improved : disabled painting
  // v1.3.2.0 : New : F4 key support for unit edit control popupmenu
  // v1.3.2.1 : Fixed : issue with button enabled state for readonly edit control
  // v1.3.2.2 : Fixed : issue with F4 key for EditorEnabled = false
  // v1.3.2.3 : Fixed : clipboard issue with DBAdvEditBtn for readonly datasets
  // v1.3.2.4 : Fixed : issue with PaintTo() method
  // v1.3.2.5 : Fixed : issue with setting edit rectangle when control resizes
  // v1.3.2.6 : Fixed : issue with Delete button & EditorEnabled = false
  // v1.3.2.7 : Improved : painting of borderless control
  // v1.3.2.8 : Improved : behaviour with DefaultHandling = false when EditorEnabled = false


type
  TNumGlyphs = Buttons.TNumGlyphs;

  TButtonStyle = (bsButton, bsDropDown);

  { TAdvSmoothSpeedButton }

  TAdvSmoothSpeedButton = class(TSpeedButton)
  private
    FEtched: Boolean;
    FFocused: Boolean;
    FHot: Boolean;
    FUp: Boolean;
    FIsWinXP: Boolean;
    procedure SetEtched(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure PaintDropDown;
    procedure PaintButton;
  protected
    procedure Paint; override;
    function DoVisualStyles: Boolean;
  public
    procedure SetUp;
    constructor Create(AOwner: TComponent); override;
  published
    property Etched: boolean read FEtched write SetEtched;
    property Focused: boolean read FFocused write SetFocused;
  end;

  { TSmoothEditButton }

  TSmoothEditButton = class (TWinControl)
  private
    FButton: TAdvSmoothSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    FBWidth: Integer;
    function CreateButton: TAdvSmoothSpeedButton;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AdjustWinSize (var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property BWidth: Integer read fBWidth write fBWidth;
    procedure Setup;
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

{ TAdvSmoothEditBtn }

  TAdvSmoothEditBtn = class(TAdvSmoothEdit)
  private
    FUnitSize : integer;
    FButton: TSmoothEditButton;
    FEditorEnabled: Boolean;
    FOnClickBtn:TNotifyEvent;
    FFlat: boolean;
    FEtched: boolean;
    // FFocusBorder: boolean;
    FMouseInControl: boolean;
    FButtonHint: string;
    FButtonStyle: TButtonStyle;
    function GetMinHeight: Integer;
    procedure SetGlyph(value:tBitmap);
    function GetGlyph:TBitmap;
    procedure SetCaption(value:string);
    function GetCaption:string;
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
    procedure WMCopy(var Message: TWMCopy);   message WM_COPY;
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetButtonWidth: integer;
    procedure SetButtonWidth(const Value: integer);
    procedure ResizeControl;
    procedure SetButtonHint(const Value: string);
    procedure SetButtonStyle(const Value: TButtonStyle);
    function GetReadOnlyEx: boolean;
    procedure SetReadOnlyEx(const Value:boolean);
  protected
    function GetVersionNr: Integer; override;
    procedure BtnClick (Sender: TObject); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
  public
    procedure SetEditRect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TSmoothEditButton read FButton;
    procedure PaintTo(DC: HDC; X: Integer; Y: Integer); overload;
    procedure PaintTo(Canvas: TCanvas; X: Integer; Y: Integer); overload;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonStyle: TButtonStyle read FButtonStyle write SetButtonStyle;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 17;
    property ButtonHint: string read FButtonHint write SetButtonHint;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Flat: boolean read FFlat write SetFlat;
    property Font;
    property Etched: Boolean read FEtched write SetEtched;
    //  property FocusBorder: Boolean read FFocusBorder write FFocusBorder;
    property FocusBorder;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ButtonCaption:string read GetCaption write SetCaption;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: boolean read GetReadOnlyEx write SetReadOnlyEx;
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
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property EditMask;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property PasswordChar;
    {$IFDEF DELPHI2010_LVL}
    property TextHint;
    {$ENDIF}
    property OnEndDock;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    property Touch;
    {$ENDIF}
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDock;
  end;

  TUnitChangeEvent = procedure(Sender: TObject; NewUnit:string) of object;

  TUnitAdvSmoothEditBtn = class(TAdvSmoothEditBtn)
  private
    FUnitID: string;
    FUnits: TStringList;
    FUnitChanged: TUnitChangeEvent;
    function GetUnitSize: Integer;
    procedure SetUnitSize(value: Integer);
    procedure SetUnits(value:tstringlist);
    procedure SetUnitID(value:string);
    procedure WMPaint(var Msg: TWMPAINT); message WM_PAINT;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
  protected
    procedure BtnClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Units: TStringList read FUnits write SetUnits;
    property UnitID:string read FUnitID write SetUnitID;
    property UnitSpace: Integer read GetUnitSize write SetUnitSize;
    property OnUnitChanged: TUnitChangeEvent read FUnitChanged write FUnitChanged;
  end;



implementation

{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}


procedure DrawBitmapTransp(Canvas: TCanvas;bmp:TBitmap;bkcolor:TColor;r:TRect);
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
    tgtrect.right := r.right - r.left;
    tgtrect.bottom := r.bottom - r.Top;

    TmpBmp.Canvas.Brush.Color := bkcolor;
    srcColor := bmp.Canvas.Pixels[0,0];
    TmpBmp.Canvas.BrushCopy(tgtrect,bmp,tgtrect,srcColor);
    Canvas.CopyRect(r, TmpBmp.Canvas, tgtrect);
  finally
    TmpBmp.Free;
  end;
end;

{ TAdvSmoothSpeedButton }
procedure TAdvSmoothSpeedButton.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := value;
    Invalidate;
  end;
end;

procedure TAdvSmoothSpeedButton.SetFocused(const Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
end;

procedure TAdvSmoothSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
end;

procedure TAdvSmoothSpeedButton.Paint;
begin
  case TAdvSmoothEditBtn(Owner.Owner).ButtonStyle of
  bsButton: PaintButton;
  bsDropDown: PaintDropDown;
  end;
end;

procedure TAdvSmoothSpeedButton.PaintDropDown;
var
  htheme: THandle;
  ARect: TRect;

begin
  if not (DoVisualStyles and IsThemeActive) then
  begin
    Enabled := TAdvSmoothEditBtn(Owner.Owner).Enabled and not TAdvSmoothEditBtn(Owner.Owner).ReadOnly;

    inherited Paint;

    Canvas.Pen.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Width-2,0);
    Canvas.LineTo(0,0);
    Canvas.LineTo(0,Height - 1);

    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Width-3,1);
    Canvas.LineTo(1,1);
    Canvas.LineTo(1,Height - 2);
  end
  else
  begin
    htheme := OpenThemeData(Parent.Handle,'combobox');
    ARect := ClientRect;
    InflateRect(ARect,1,1);
    ARect.Left := ARect.Left + 2;

    if not TAdvSmoothEditBtn(Owner.Owner).Enabled or TAdvSmoothEditBtn(Owner.Owner).ReadOnly then
    begin
      DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) and not FUp then
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
      else
      begin
        if FHot then
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
      end;
    end;
    CloseThemeData(htheme);
  end;
end;


procedure TAdvSmoothSpeedButton.PaintButton;
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  r: TRect;
  BtnFaceBrush: HBRUSH;
  HTheme: THandle;
begin
  Canvas.Font.Assign(TAdvSmoothEdit(Owner.Owner).Font);

  if DoVisualStyles then
  begin
    r := BoundsRect;
    FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

    r := Rect(0, 0, Width + 1, Height + 1);

    HTheme := OpenThemeData(Parent.Handle,'button');

    if not TAdvSmoothEditBtn(Owner.Owner).Enabled or TAdvSmoothEditBtn(Owner.Owner).ReadOnly then
      DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_DISABLED,@r,nil)
    else
    begin
      if (FState in [bsDown, bsExclusive]) and not FUp then
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@r,nil)
      else
        if FHot then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@r,nil)
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@r,nil);
     end;

    CloseThemeData(HTheme);

    r := ClientRect;

    if Assigned(Glyph) then
    begin
      if not Glyph.Empty then
      begin
        InflateRect(r,-2,-2);

        if (Caption = '') then
        begin
          if Glyph.Width < r.Right - r.Left then
            r.Left := r.Left + (r.Right - r.Left - Glyph.Width) shr 1;
        end
        else
          r.Left := r.Left + 2;

        if Glyph.Height < r.Bottom - r.Top then
          r.Top := r.Top + (r.Bottom - r.Top - Glyph.Height) shr 1;

        if FState = bsdown then OffsetRect(r,1,1);

        Glyph.TransparentMode := tmAuto;
        Glyph.Transparent := true;
        Canvas.Draw(r.Left,r.Top, Glyph);
      end;
    end;


    if (Caption <> '') then
    begin
      Windows.setbkmode(canvas.handle,windows.TRANSPARENT);
      if not Glyph.Empty then
      begin
        r.Left := r.Left + Glyph.Width + 2;
        r.Top := r.Top -1;
        DrawText(canvas.handle,pchar(Caption),length(Caption),r,DT_LEFT);
      end
      else
      begin
        Inflaterect(r,-3,-1);
        if FState = bsdown then Offsetrect(r,1,1);
        DrawText(canvas.handle,pchar(Caption),length(Caption),r,DT_CENTER);
      end;
    end;

  end
  else
  begin
    Enabled := TAdvSmoothEditBtn(Owner.Owner).Enabled and not TAdvSmoothEditBtn(Owner.Owner).ReadOnly;

    if not Flat then
      inherited Paint else
    begin

      r := BoundsRect;
      FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

      BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));

      FillRect(Canvas.Handle, r, BtnFaceBrush);

      DeleteObject(BtnFaceBrush);

      r.Bottom := r.Bottom + 1;
      r.Right := r.Right + 1;
      DrawEdge(Canvas.Handle, r, Edge[fEtched], BF_RECT or flags[fState=bsDown]);

      r := ClientRect;

      if Assigned(Glyph) then
      begin
        if not Glyph.Empty then
        begin
          InflateRect(r,-3,-3);
          if fstate = bsdown then offsetrect(r,1,1);
          DrawBitmapTransp(canvas,glyph,ColorToRGB(clBtnFace),r);
        end;
      end;

      if not TAdvSmoothEditBtn(Owner.Owner).Enabled or TAdvSmoothEditBtn(Owner.Owner).ReadOnly then
        Canvas.Font.Color := clGray;

      if (Caption <> '') then
      begin
        Inflaterect(r,-3,-1);
        if FState = bsdown then Offsetrect(r,1,1);
        Windows.SetBKMode(canvas.handle,windows.TRANSPARENT);
        DrawText(Canvas.handle,pchar(Caption),length(Caption),r,DT_CENTER);
      end;
    end;
  end;
end;

function TAdvSmoothSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

constructor TAdvSmoothSpeedButton.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);

  FUp := False;
end;

procedure TAdvSmoothSpeedButton.SetUp;
begin
  FUp := true;
end;

{ TSmoothEditButton }
constructor TSmoothEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButton := CreateButton;
  Glyph := nil;
  Width := 16;
  Height := 25;
  FBWidth := 16;
end;

function TSmoothEditButton.CreateButton: TAdvSmoothSpeedButton;
begin
  Result := TAdvSmoothSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := Enabled;
  Result.Parent := Self;
  Result.Caption := '';
end;

procedure TSmoothEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TSmoothEditButton.AdjustWinSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or (csLoading in ComponentState) then
    Exit;
  W := FBWidth;
  FButton.SetBounds (0, 0, W, H);
end;

procedure TSmoothEditButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustWinSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TSmoothEditButton.WMSize(var Message: TWMSize);
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

procedure TSmoothEditButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
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

procedure TSmoothEditButton.BtnClick(Sender: TObject);
begin
 {
 if (Sender=FButton) then FOnClick(Self);
 }
end;

procedure TSmoothEditButton.Loaded;
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

function TSmoothEditButton.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TSmoothEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TSmoothEditButton.SetCaption(value:string);
begin
  FButton.Caption := Value;
end;

function TSmoothEditButton.GetCaption:string;
begin
  Result := FButton.Caption;
end;

function TSmoothEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TSmoothEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

{ TSpinEdit }

constructor TAdvSmoothEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TSmoothEditButton.Create(Self);
  FButton.Width := 17;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnClick := BtnClick;

  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FUnitSize := 0;
  IndentR := 18;
  IndentL := 0;
end;

destructor TAdvSmoothEditBtn.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TAdvSmoothEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (EditType = etPassword) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;
end;

procedure TAdvSmoothEditBtn.CreateWnd;
begin
  inherited CreateWnd;
  Width := Width - 1;
  Width := Width + 1;
  SetEditRect;
  ResizeControl;  
end;

procedure TAdvSmoothEditBtn.Loaded;
begin
  inherited Loaded;
  SetEditRect;
  ResizeControl;
  FButton.Enabled := Enabled;
end;

procedure TAdvSmoothEditBtn.SetGlyph(value:TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TAdvSmoothEditBtn.SetReadOnlyEx(const Value: boolean);
begin
  inherited ReadOnly := Value;
  FButton.Enabled := Enabled and not Value;
end;

function TAdvSmoothEditBtn.GetGlyph:TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TAdvSmoothEditBtn.SetCaption(value:string);
begin
  FButton.ButtonCaption := value;
end;

function TAdvSmoothEditBtn.GetCaption:string;
begin
  Result := FButton.ButtonCaption;
end;

procedure TAdvSmoothEditBtn.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3 - FUnitSize;

  if (BorderStyle = bsNone) then
  begin
    Loc.Top := 4;
    Loc.Left := 2;
  end
  else
  begin
    Loc.Top := 1;
    Loc.Left := 1;
  end;

  if not Ctl3D then
    Loc.Left := 2;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
 end;

procedure TAdvSmoothEditBtn.ResizeControl; 
var
  MinHeight: Integer;
  Dist,FlatCorr: Integer;
  Offs: Integer;
begin
  if (BorderStyle = bsNone) then
    Dist := 1
  else
    Dist := 4;

  if FFlat then
    Dist := 3;

  if FFlat then
    FlatCorr := 1
  else
    FlatCorr := -1;

  MinHeight := GetMinHeight;

  if not Ctl3d then
    Offs := 2
  else
    Offs := 0;

  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }

  if (Height < MinHeight) then
    Height := MinHeight
  else
  if (FButton <> nil) then
   begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.FBWidth - Dist - Offs,1 + FlatCorr, FButton.FBWidth,Height - Dist)
    else
      FButton.SetBounds (Width - FButton.FBWidth - Offs, 1, FButton.FBWidth,Height - 2);

    SetEditRect;
   end;

  Invalidate;
end;


procedure TAdvSmoothEditBtn.WMSize(var Message: TWMSize);
begin
  inherited;
  ResizeControl;
end;

procedure TAdvSmoothEditBtn.WMKeyDown(var Msg:TWMKeydown);
begin
  if not EditorEnabled and (Msg.CharCode = VK_DELETE) then
  begin
    Msg.CharCode := 0;
    Msg.Result := 1;
    Exit;
  end;

  inherited;

  if (Msg.CharCode = VK_RETURN) and (GetKeyState(VK_CONTROL) and $8000 = $8000) then
  begin
    PostMessage(Handle,WM_KEYDOWN,VK_UP,0);
  end;
end;

procedure TAdvSmoothEditBtn.PaintTo(DC: HDC; X: Integer; Y: Integer);
begin
  inherited;
  Button.PaintTo(DC, X + Width - Button.Width - 2 - BorderWidth, Y + BorderWidth + 2 );
end;

procedure TAdvSmoothEditBtn.PaintTo(Canvas: TCanvas; X: Integer; Y: Integer);
begin
  {$IFDEF DELPHI6_LVL}
  inherited;
  Button.PaintTo(Canvas, X + Width - Button.Width - 2 - BorderWidth, Y + BorderWidth + 2 );
  {$ENDIF}
  {$IFNDEF DELPHI6_LVL}
  inherited PaintTo(Canvas.Handle, X,Y);
  Button.PaintTo(Canvas.Handle, X + Width - Button.Width - 2 - BorderWidth, Y + BorderWidth + 2 );
  {$ENDIF}
end;


function TAdvSmoothEditBtn.GetMinHeight: Integer;
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

function TAdvSmoothEditBtn.GetReadOnlyEx: boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TAdvSmoothEditBtn.BtnClick (Sender: TObject);
begin
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Self);
end;

procedure TAdvSmoothEditBtn.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvSmoothEditBtn.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvSmoothEditBtn.CMExit(var Message: TCMExit);
begin
  inherited;
  DrawBorders;
end;

procedure TAdvSmoothEditBtn.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Enabled := self.Enabled and not ReadOnly;
end;

procedure TAdvSmoothEditBtn.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
  DrawBorders;
end;

procedure TAdvSmoothEditBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    DrawBorders;
  end;
end;

procedure TAdvSmoothEditBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl:=False;
    DrawBorders;
  end;
end;

procedure TAdvSmoothEditBtn.SetFlat(const Value: boolean);
begin
  if (FFlat <> value) then
  begin
    FFlat := Value;
    FButton.FButton.Flat := FFlat;
    inherited Flat := Value;
    // cause a proper repaint of full control
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TAdvSmoothEditBtn.SetEtched(const Value: boolean);
begin
  if FEtched <> value then
  begin
    FEtched := Value;
    FButton.FButton.Etched:=value;
    Invalidate;
  end;
end;

function TAdvSmoothEditBtn.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
//  Result := Result and FFocusBorder;
end;

function TAdvSmoothEditBtn.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TAdvSmoothEditBtn.DoEnter;
begin
  inherited;
  SetEditRect;
end;


procedure TAdvSmoothEditBtn.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((parent as TWinControl).brush.color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;


procedure TAdvSmoothEditBtn.DrawButtonBorder;
begin
  FButton.FButton.Focused := Is3DBorderButton;
end;

procedure TAdvSmoothEditBtn.DrawBorders;
var
  DC: HDC;
begin
  if not FFlat then Exit;

  DC := GetWindowDC(Handle);
  try
    if (1<0) then DrawControlBorder(DC);
    DrawButtonBorder;
  finally
    ReleaseDC(DC, Handle);
  end;
end;

procedure TAdvSmoothEditBtn.WMPaint(var Msg: TWMPAINT);
begin
  inherited;
//  DrawBorders;
end;

procedure TAdvSmoothEditBtn.WMNCPaint(var Message: TMessage);
begin
  inherited;
//  DrawBorders;
end;


procedure TAdvSmoothEditBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetEditRect;
end;

procedure TAdvSmoothEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
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

function TAdvSmoothEditBtn.GetButtonWidth: integer;
begin
 if Assigned(FButton) then
   Result := FButton.FBWidth
 else
   Result := 17;
end;

procedure TAdvSmoothEditBtn.SetButtonWidth(const Value: integer);
begin
  if Assigned(FButton) then
  begin
    FButton.FBWidth := Value;
    IndentR := Value + 3;
    if FButton.HandleAllocated then
      ResizeControl;
  end;
end;

procedure TAdvSmoothEditBtn.SetButtonHint(const Value: string);
begin
  if FButtonHint <> Value then
  begin
    FButtonHint := Value;
    FButton.Hint := Value;
    FButton.ShowHint := Value <> '';
  end;
end;

function TAdvSmoothEditBtn.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TSmoothEditButton.Setup;
begin
  FButton.Setup;
end;

{ TUniTAdvSmoothEditBtn }
procedure TUniTAdvSmoothEditBtn.BtnClick(Sender: TObject);
var
  popmenu: THandle;
  pt: TPoint;
  i: Integer;
begin
  pt := ClientToScreen(point(0,0));
  popmenu := CreatePopupMenu;

  for i := 1 to FUnits.Count do
    InsertMenu(popmenu,$FFFFFFFF,MF_BYPOSITION ,i,PChar(FUnits.Strings[i-1]));

  TrackPopupMenu(popmenu,TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x+ClientWidth-15,pt.y+ClientHeight,0,self.handle,nil);

  Destroymenu(popmenu);
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Sender);
end;

constructor TUniTAdvSmoothEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FUnitSize := 20;
  FUnits := TStringList.Create;
end;

destructor TUniTAdvSmoothEditBtn.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

procedure TUniTAdvSmoothEditBtn.SetUnitID(value: string);
begin
  FUnitID := value;
  Repaint;
end;

procedure TUniTAdvSmoothEditBtn.SetUnits(value: TStringList);
begin
  if Assigned(Value) then
    FUnits.Assign(Value);
end;

function TUniTAdvSmoothEditBtn.GetUnitSize: Integer;
begin
  Result := FUnitSize;
end;

procedure TUniTAdvSmoothEditBtn.SetUnitSize(value: Integer);
begin
  FUnitSize := Value;
  SetEditRect;
  Repaint;
end;

procedure TUniTAdvSmoothEditBtn.WMCommand(var Message: TWMCommand);
begin
  UnitID:=fUnits.Strings[message.itemID-1];
  if Assigned(OnUnitChanged) then
    OnUnitChanged(Self,UnitID);
end;

procedure TUniTAdvSmoothEditBtn.WMPaint(var Msg: TWMPAINT);
var
  hdc:THandle;
  oldfont:THandle;
  r:trect;
begin
  inherited;
  hdc := GetDC(Handle);
  if hdc <> 0 then
  begin
    r.left := ClientWidth - FButton.Width - 3 - FUnitsize;
    r.right := r.left + FUnitSize;
    r.top := 2;
    r.bottom := ClientHeight;
    oldfont := selectobject(hdc,self.Font.handle);

    Windows.SetBkMode(hdc,Windows.TRANSPARENT);

    if not Enabled then
    begin
      SetTextColor(hdc,ColorToRGB(clGrayText));
    end;

    DrawText(HDC,PChar(FUnitID),Length(FUnitID),r,DT_LEFT or DT_EDITCONTROL);
    SelectObject(hdc,oldfont);
    ReleaseDC(self.handle,hdc);
  end;
end;

procedure TAdvSmoothEditBtn.WMCopy(var Message: TWMCopy);
begin
  inherited;
end;

procedure TAdvSmoothEditBtn.SetButtonStyle(const Value: TButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

procedure TAdvSmoothEditBtn.WMChar(var Msg: TWMKey);
begin
  if not EditorEnabled then
  begin
    if (EditType <> etPassword) then
    begin
      if (Msg.CharCode = 3) then  // allow Ctrl-C
      begin
        inherited;
        Exit;
      end;
    end;
    if (Msg.CharCode <> 13) and (Msg.CharCode <> 27) then
      Exit;
  end;

  inherited;
end;

end.

