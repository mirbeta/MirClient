{*********************************************************************}
{ TADVEDITBTN component                                               }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 2000 - 2013                                  }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit AdvEdBtn;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Dialogs, Menus, AdvEdit, AEBXPVS, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.3.0.2 : Fixed : issue with EditorEnabled = false
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
  // v1.3.3.0 : New : BorderColor property added
  // v1.3.3.1 : Improved : Text positioning made more consistent with TAdvEdit
  // v1.3.3.2 : Improved : Text rendering in button
  // v1.3.3.3 : Improved : Handling of case where Button.Visible = false
  // v1.3.3.4 : Fixed : Issue with EditAlign = eaRight
  // v1.3.4.0 : Improved : Handling of FocusFontColor
  // v1.3.4.1 : Fixed : Issue with persistence when used in frames
  // v1.3.4.2 : Improved : Handling of label when using alignment
  // v1.3.5.0 : New : ShowFieldName added in TDBAdvEditBtn
  // v1.3.5.1 : Fixed : Display issue on container ctrl with Ctl3D = false

type
  TNumGlyphs = Buttons.TNumGlyphs;

  TButtonStyle = (bsButton, bsDropDown);

  { TAdvSpeedButton }

  TAdvSpeedButton = class(TSpeedButton)
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

  { TEditButton }

  TEditButton = class (TWinControl)
  private
    FButton: TAdvSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    FBWidth: Integer;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonTextColorDown: TColor;
    function CreateButton: TAdvSpeedButton;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AdjustWinSize (var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure BtnClick(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property BWidth: Integer read fBWidth write fBWidth;
    procedure Setup;
  public
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
    property OnStartDrag;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

{ TAdvEditBtn }

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEditBtn = class(TAdvEdit)
  private
    FUnitSize : integer;
    FButton: TEditButton;
    FEditorEnabled: Boolean;
    FOnClickBtn:TNotifyEvent;
    FFlat: boolean;
    FEtched: boolean;
    // FFocusBorder: boolean;
    FMouseInControl: boolean;
    FButtonHint: string;
    FButtonStyle: TButtonStyle;
    function GetMinHeight: Integer;
    procedure SetGlyph(value: TBitmap);
    function GetGlyph: TBitmap;
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure SetFlat(const Value : boolean);
    procedure SetEtched(const Value : boolean);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawButtonBorder;
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
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
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
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
    property Button: TEditButton read FButton;
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
  end;

  TUnitChangeEvent = procedure(Sender: TObject; NewUnit:string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TUnitAdvEditBtn = class(TAdvEditBtn)
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
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}


{
procedure DrawBitmapTransp(Canvas: TCanvas;  bmp:TBitmap; bkcolor:TColor; r:TRect);
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
}

{ TAdvSpeedButton }
procedure TAdvSpeedButton.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := value;
    Invalidate;
  end;
end;

procedure TAdvSpeedButton.SetFocused(const Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
  end;
end;

procedure TAdvSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
end;

procedure TAdvSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
end;

procedure TAdvSpeedButton.Paint;
begin
  case TAdvEditBtn(Owner.Owner).ButtonStyle of
  bsButton: PaintButton;
  bsDropDown: PaintDropDown;
  end;
end;

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

procedure TAdvSpeedButton.PaintDropDown;
var
  htheme: THandle;
  ARect: TRect;
  pt: array of TPoint;

begin
  if (TEditButton(Owner).ButtonColor <> clNone) then
  begin
    Canvas.Brush.Color := TEditButton(Owner).ButtonColor;

    if FHot then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorDown;

    Canvas.Pen.Color := Canvas.Brush.Color;
    ARect := ClientRect;
    ARect.Left := ARect.Left + 2;
    Canvas.FillRect(ClientRect);
    ARect.Left := ARect.Left - 2;

    Canvas.Pen.Color := TAdvEditBtn(Owner.Owner).BorderColor;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom);

    Canvas.Brush.Color := TEditButton(Owner).ButtonTextColor;

    if FHot then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorHot;
    end;

    if ((FState in [bsDown, bsExclusive]) and not FUp) then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorDown;
    end;

    Canvas.Pen.Color := Canvas.Brush.Color;

    SetLength(pt, 3);

    pt[0].X := 5;
    pt[0].y := 7;

    pt[1].X := 11;
    pt[1].y := 7;

    pt[2].X := 8;
    pt[2].y := 10;

    Canvas.Polygon(pt);
    Exit;
  end;

  if not (DoVisualStyles and IsThemeActive)  then
  begin
    Enabled := TAdvEditBtn(Owner.Owner).Enabled and not TAdvEditBtn(Owner.Owner).ReadOnly;

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

    if Glyph.Empty then
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := Canvas.Brush.Color;

      SetLength(pt, 3);

      pt[0].X := 5;
      pt[0].y := 7;

      pt[1].X := 11;
      pt[1].y := 7;

      pt[2].X := 8;
      pt[2].y := 10;

      Canvas.Polygon(pt);
    end;
  end
  else
  begin
    htheme := OpenThemeData(Parent.Handle,'combobox');
    ARect := ClientRect;
    if not IsVista then
    begin
      InflateRect(ARect,1,1);
      ARect.Left := ARect.Left + 2;
    end
    else
      Arect.Left := Arect.Left + 1;

    if not TAdvEditBtn(Owner.Owner).Enabled or TAdvEditBtn(Owner.Owner).ReadOnly then
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


procedure TAdvSpeedButton.PaintButton;
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  r, ARect: TRect;
  BtnFaceBrush: HBRUSH;
  HTheme: THandle;
  FFlat: boolean;

begin
  FFlat := false;

  if (TEditButton(Owner).ButtonColor <> clNone) then
  begin
    FFlat := true;
    Canvas.Brush.Color := TEditButton(Owner).ButtonColor;

    if FHot then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp then
      Canvas.Brush.Color := TEditButton(Owner).ButtonColorDown;

    Canvas.Pen.Color := Canvas.Brush.Color;
    ARect := ClientRect;
    ARect.Left := ARect.Left + 2;
    Canvas.FillRect(ClientRect);
    ARect.Left := ARect.Left - 2;

    Canvas.Pen.Color := TAdvEditBtn(Owner.Owner).BorderColor;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom);

    Canvas.Brush.Color := TEditButton(Owner).ButtonTextColor;

    if FHot then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorHot;
    end;

    if ((FState in [bsDown, bsExclusive]) and not FUp) then
    begin
      Canvas.Brush.Color := TEditButton(Owner).ButtonTextColorDown;
    end;

    Canvas.Pen.Color := Canvas.Brush.Color;
  end
  else
  begin
    if DoVisualStyles then
    begin
      r := ClientRect;
      FillRect(Canvas.Handle,r,Canvas.Brush.Handle);
      InflateRect(r,1,1);

      HTheme := OpenThemeData(Parent.Handle,'button');

      if not TAdvEditBtn(Owner.Owner).Enabled or TAdvEditBtn(Owner.Owner).ReadOnly then
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
    end
    else
    begin
      Enabled := TAdvEditBtn(Owner.Owner).Enabled and not TAdvEditBtn(Owner.Owner).ReadOnly;

      if not Flat then
        inherited Paint else
      begin
        r := BoundsRect;

        FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

        BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
        try
          FillRect(Canvas.Handle, r, BtnFaceBrush);
        finally
          DeleteObject(BtnFaceBrush);
        end;

        r.Bottom := r.Bottom + 1;
        r.Right := r.Right + 1;
        DrawEdge(Canvas.Handle, r, Edge[fEtched], BF_RECT or flags[fState=bsDown]);
      end;
    end;
  end;

  r := ClientRect;

  if Assigned(Glyph) then
  begin
    if not Glyph.Empty and (Glyph.Height > 0) and (Glyph.Width > 0) then
    begin
      OffsetRect(r,1,1);

      if (Caption = '') then
      begin
        if Glyph.Width < r.Right - r.Left then
          r.Left := r.Left + (r.Right - r.Left - Glyph.Width) shr 1;
      end
      else
        r.Left := r.Left + 2;

      if Glyph.Height < r.Bottom - r.Top then
        r.Top := r.Top + (r.Bottom - r.Top - Glyph.Height) shr 1;

      if (fState = bsdown) then
        Offsetrect(r,1,1);

      Glyph.Transparent := true;
      Glyph.TransparentMode := tmAuto;
      Canvas.Draw(r.Left, r.Top, Glyph);
    end;
  end;

  if (Caption <> '') then
  begin
    Canvas.Font.Assign(TAdvEdit(Owner.Owner).Font);

    if TEditButton(Owner).ButtonTextColor <> clNone then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColor;

    if not TAdvEditBtn(Owner.Owner).Enabled or TAdvEditBtn(Owner.Owner).ReadOnly then
      Canvas.Font.Color := clGray;

    if FHot and (TEditButton(Owner).ButtonTextColorHot <> clNone) then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp and (TEditButton(Owner).ButtonTextColorDown <> clNone) then
      Canvas.Font.Color := TEditButton(Owner).ButtonTextColorDown;

    r := ClientRect;

    if not FFlat then
    begin
      if not Assigned(Glyph) then
        InflateRect(r,-3,-1)
    end
    else
      r.Left := r.Left + 2;

    Windows.SetBKMode(Canvas.Handle,Windows.TRANSPARENT);

    if not Glyph.Empty then
    begin
      r.Left := r.Left + Glyph.Width + 2;
      r.Top := r.Top -1;
    end
    else
    begin
      if not FFlat then
        Inflaterect(r,-3,-1);
      if FState = bsdown then Offsetrect(r,1,1);
    end;

    DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r,DT_CENTER);
  end;
end;

function TAdvSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

constructor TAdvSpeedButton.Create(AOwner: TComponent);
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

procedure TAdvSpeedButton.SetUp;
begin
  FUp := true;
end;

{ TEditButton }

constructor TEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButton := CreateButton;
  Glyph := nil;
  Width := 16;
  Height := 25;
  FBWidth := 16;
  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;
end;

function TEditButton.CreateButton: TAdvSpeedButton;
begin
  Result := TAdvSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := Enabled;
  Result.Parent := Self;
  Result.Caption := '';
end;

procedure TEditButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TEditButton.AdjustWinSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or (csLoading in ComponentState) then
    Exit;
  W := FBWidth;
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

procedure TEditButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
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
  Result := FButton.Glyph;
end;

procedure TEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TEditButton.SetCaption(value:string);
begin
  FButton.Caption := Value;
end;

function TEditButton.GetCaption:string;
begin
  Result := FButton.Caption;
end;

function TEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

{ TSpinEdit }

constructor TAdvEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TEditButton.Create(Self);
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

destructor TAdvEditBtn.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TAdvEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (EditType = etPassword) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;
end;

procedure TAdvEditBtn.CreateWnd;
begin
  inherited CreateWnd;
  Width := Width - 1;
  Width := Width + 1;
  SetEditRect;
  ResizeControl;
end;

procedure TAdvEditBtn.Loaded;
begin
  inherited Loaded;
  SetEditRect;
  ResizeControl;
  FButton.Enabled := Enabled and not ReadOnly;
  if not Glyph.Empty then
  begin
    Glyph.TransparentMode := tmAuto;
    //Glyph.Transparent := true;
  end;
end;

procedure TAdvEditBtn.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TAdvEditBtn.SetReadOnlyEx(const Value: boolean);
begin
  inherited ReadOnly := Value;
  FButton.Enabled := Enabled and not Value;
end;

function TAdvEditBtn.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TAdvEditBtn.SetCaption(value:string);
begin
  FButton.ButtonCaption := value;
end;

function TAdvEditBtn.GetCaption:string;
begin
  Result := FButton.ButtonCaption;
end;

procedure TAdvEditBtn.SetEditRect;
var
  Loc: TRect;
  bw: integer;

begin
  if (csDestroying in ComponentState) then
    Exit;

  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  //+1 is workaround for windows paint bug

  bw := 0;
  if FButton.Visible then
    bw := FButton.Width + 2;

  Loc.Right := ClientWidth - bw - FUnitSize - 1;

  // Force the same as a TAdvEdit instead of centering vertically
  Loc.Top := 0;
  Loc.Left := 0;

  {
  if (BorderStyle = bsNone) then
  begin
    Loc.Top := 4;
    Loc.Left := 0;
  end
  else
  begin
    Loc.Top := 1;
    Loc.Left := 1;
  end;
  }

  if not Ctl3D then
    Loc.Left := 2;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
end;

procedure TAdvEditBtn.ResizeControl;
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
       FButton.SetBounds(Width - FButton.FBWidth - Dist - Offs,1 + FlatCorr, FButton.FBWidth, Height - Dist)
     else
       FButton.SetBounds (Width - FButton.FBWidth - Offs, 1, FButton.FBWidth, Height - 2);

     SetEditRect;
   end;

  Invalidate;
end;


procedure TAdvEditBtn.WMKeyDown(var Msg:TWMKeydown);
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

procedure TAdvEditBtn.PaintTo(DC: HDC; X: Integer; Y: Integer);
begin
  inherited;
  Button.PaintTo(DC, X + Width - Button.Width - 2 - BorderWidth, Y + BorderWidth + 2 );
end;

procedure TAdvEditBtn.PaintTo(Canvas: TCanvas; X: Integer; Y: Integer);
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

function TAdvEditBtn.GetMinHeight: Integer;
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

function TAdvEditBtn.GetReadOnlyEx: boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TAdvEditBtn.BtnClick (Sender: TObject);
begin
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Self);
end;

procedure TAdvEditBtn.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvEditBtn.WMSize(var Msg: TWMSize);
begin
  inherited;
  ResizeControl;
end;

procedure TAdvEditBtn.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvEditBtn.CMExit(var Message: TCMExit);
begin
  inherited;
  DrawBorders;
end;

procedure TAdvEditBtn.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Enabled := self.Enabled and not ReadOnly;
end;

procedure TAdvEditBtn.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
  DrawBorders;
end;

procedure TAdvEditBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    DrawBorders;
  end;
end;

procedure TAdvEditBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl:=False;
    DrawBorders;
  end;
end;

procedure TAdvEditBtn.SetFlat(const Value: boolean);
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

procedure TAdvEditBtn.SetEtched(const Value: boolean);
begin
  if FEtched <> value then
  begin
    FEtched := Value;
    FButton.FButton.Etched:=value;
    Invalidate;
  end;
end;

function TAdvEditBtn.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
//  Result := Result and FFocusBorder;
end;

function TAdvEditBtn.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TAdvEditBtn.DoEnter;
begin
  inherited;
  SetEditRect;
end;


procedure TAdvEditBtn.DrawControlBorder(DC: HDC);
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


procedure TAdvEditBtn.DrawButtonBorder;
begin
  FButton.FButton.Focused := Is3DBorderButton;
end;

procedure TAdvEditBtn.DrawBorders;
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

procedure TAdvEditBtn.WMPaint(var Msg: TWMPAINT);
begin
  inherited;
//  DrawBorders;
end;

procedure TAdvEditBtn.WMNCPaint(var Message: TMessage);
begin
  inherited;
//  DrawBorders;
end;

procedure TAdvEditBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetEditRect;
end;

procedure TAdvEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (Key = VK_F4) and
     (GetKeyState(VK_Control) and $8000 = 0) and
     (GetKeyState(VK_LMenu) and $8000 = 0) and
     (GetKeyState(VK_RMenu) and $8000 = 0) then
   begin
     BtnClick(self);
   end;
end;

function TAdvEditBtn.GetButtonWidth: integer;
begin
 if Assigned(FButton) then
   Result := FButton.FBWidth
 else
   Result := 17;
end;

procedure TAdvEditBtn.SetButtonWidth(const Value: integer);
begin
  if Assigned(FButton) then
  begin
    FButton.FBWidth := Value;
    IndentR := Value + 3;
    if FButton.HandleAllocated then
      ResizeControl;
  end;
end;

procedure TAdvEditBtn.SetButtonHint(const Value: string);
begin
  if FButtonHint <> Value then
  begin
    FButtonHint := Value;
    FButton.Hint := Value;
    FButton.ShowHint := Value <> '';
  end;
end;

function TAdvEditBtn.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TEditButton.Setup;
begin
  FButton.Setup;
end;

procedure TAdvEditBtn.SetButtonStyle(const Value: TButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

procedure TAdvEditBtn.WMChar(var Msg: TWMKey);
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

procedure TAdvEditBtn.WMCopy(var Message: TWMCopy);
begin
  inherited;
end;

{ TUnitAdvEditBtn }
procedure TUnitAdvEditBtn.BtnClick(Sender: TObject);
var
  popmenu: THandle;
  pt: TPoint;
  i: Integer;
begin
  inherited;
  pt := ClientToScreen(point(0,0));
  popmenu := CreatePopupMenu;

  for i := 1 to FUnits.Count do
    InsertMenu(popmenu,$FFFFFFFF,MF_BYPOSITION ,i,PChar(FUnits.Strings[i-1]));

  TrackPopupMenu(popmenu,TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x+ClientWidth-15,pt.y+ClientHeight,0,self.handle,nil);

  Destroymenu(popmenu);
end;

constructor TUnitAdvEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FUnitSize := 20;
  FUnits := TStringList.Create;
end;

destructor TUnitAdvEditBtn.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

procedure TUnitAdvEditBtn.SetUnitID(value: string);
begin
  FUnitID := value;
  Repaint;
end;

procedure TUnitAdvEditBtn.SetUnits(value: TStringList);
begin
  if Assigned(Value) then
    FUnits.Assign(Value);
end;

function TUnitAdvEditBtn.GetUnitSize: Integer;
begin
  Result := FUnitSize;
end;

procedure TUnitAdvEditBtn.SetUnitSize(value: Integer);
begin
  FUnitSize := Value;
  SetEditRect;
  Repaint;
end;

procedure TUnitAdvEditBtn.WMCommand(var Message: TWMCommand);
begin
  UnitID := FUnits.Strings[message.itemID - 1];
  
  if Assigned(OnUnitChanged) then
    OnUnitChanged(Self,UnitID);
end;

procedure TUnitAdvEditBtn.WMPaint(var Msg: TWMPAINT);
var
  hdc:THandle;
  oldfont:THandle;
  r:trect;
begin
  inherited;
  hdc := GetDC(Handle);
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

initialization
  {$IFDEF ISDELPHI}
  try
    Classes.RegisterClass(TEditButton);
  except
  end;
  {$ENDIF}

end.

