{***************************************************************************}
{ TAdvGroupBox component                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2015                                        }
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

unit AdvGroupBox;

{$I TMSDEFS.INC}
{$R AdvGroupBox.res}


interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, StdCtrls, ImgList;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : fixed issue for XP theming
  // v1.0.0.2 : fixed issue with persisting Ctl3D property setting
  // v1.1.0.0 : New : CheckBox property added
  // v1.1.0.1 : Fixed : Issue with checkbox hint
  // v1.1.0.2 : Fixed : Issue with initial checkbox state for caDisable
  // v1.1.0.3 : Improved : Caption will be painted in gray when control is disabled
  // v1.1.0.4 : Improved : Caption rectangle calculation
  // v1.1.0.5 : Fixed : Issue with border drawing when caption is empty
  // v1.1.0.6 : Improved : Handling of AdvGroupBox within AdvGroupBox
  // v1.1.1.0 : New : Checkbox action caControlsDisable added
  // v1.1.2.0 : Improved : Appearance matches Windows control class GROUPBOX better

type
  TAdvCustomGroupBox = class;

  TCaptionPosition = (cpTopLeft, cpTopRight, cpTopCenter, cpBottomLeft, cpBottomRight, cpBottomCenter);
  TBorderStyle = (bsNone, bsSingle, bsDouble);
  TCheckBoxPos = (cpLeft, cpRight);
  TCheckBoxAction = (caNone, caDisable, caCheckAll, caControlsDisable);

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TGroupBoxCheck = class(TPersistent)
  private
    FHint: string;
    FState: TCheckBoxState;
    FThemed: Boolean;
    FAllowGrayed: Boolean;
    FOnChange: TNotifyEvent;
    FAction: TCheckBoxAction;
    FPosition: TCheckBoxPos;
    FVisible: Boolean;
    FHot: Boolean;
    FDown: Boolean;
    FGroupBox: TAdvCustomGroupBox;
    procedure SetAction(const Value: TCheckBoxAction);
    procedure SetAllowGrayed(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetPosition(const Value: TCheckBoxPos);
    procedure SetState(const Value: TCheckBoxState);
    procedure SetThemed(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetHot(const Value: Boolean);
    function GetChecked: Boolean;
    procedure SetHint(const Value: string);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Down: Boolean read FDown write SetDown default False;
    property Hot: Boolean read FHot write SetHot default False;
    property GroupBox: TAdvCustomGroupBox read FGroupBox;
  public
    constructor Create(AOwner: TAdvCustomGroupBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Checked: Boolean read GetChecked write SetChecked default False;
    property Position: TCheckBoxPos read FPosition write SetPosition default cpLeft; //(left or right from caption)
    property Action: TCheckBoxAction read FAction write SetAction default caDisable;
    property Hint: string read FHint write SetHint;
    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property Themed: Boolean read FThemed write SetThemed default True;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;


  TAdvCustomGroupBox = class(TCustomGroupBox)
  private
    FTransparent: Boolean;
    FBorderColor: TColor;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FBorderStyle: TBorderStyle;
    FCaptionPosition: TCaptionPosition;
    FRoundEdges: Boolean;
    FCheckBox: TGroupBoxCheck;
    FOnCheckBoxClick: TNotifyEvent;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetTransparent(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetCaptionPosition(const Value: TCaptionPosition);
    procedure SetRoundEdges(const Value: Boolean);
    procedure SetCheckBox(const Value: TGroupBoxCheck);
    procedure OnCheckBoxChanged(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure PaintTransparency; virtual;
    procedure DrawCheck;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    function GetCaptionHeight: Integer;
    function GetCaptionRect: TRect;
    function GetBorderWidth: Integer;
    function HasCaption: boolean;
    function GetBorderRect: TRect;
    function CalculateRect(var CheckBoxR, ImgTextR: TRect): TRect;
    function GetCheckBoxRect: TRect;
    procedure ToggleCheck;
    function PtOnCaption(P: TPoint): Boolean;
    procedure PerformCheckBoxAction; virtual;
    procedure DoGroupCheckClick; virtual;
    property CaptionPosition: TCaptionPosition read FCaptionPosition write SetCaptionPosition default cpTopLeft;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Version: string read GetVersion write SetVersion stored false;
    property RoundEdges: Boolean read FRoundEdges write SetRoundEdges default False;
    property CheckBox: TGroupBoxCheck read FCheckBox write SetCheckBox;
    property OnCheckBoxClick: TNotifyEvent read FOnCheckBoxClick write FOnCheckBoxClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGroupBox = class(TAdvCustomGroupBox)
  published
    property BorderColor;
    property BorderStyle;
    property CaptionPosition;
    property CheckBox;
    property Images;
    property ImageIndex;
    property Transparent;
    property RoundEdges;
    property Version;

    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D default False;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnCheckBoxClick;
  end;

implementation

uses
  Math, AOBXPVS, Types;

function IsWin8: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 6) OR
    ((verinfo.dwMajorVersion = 6) AND (verinfo.dwMinorVersion >= 2));
end;

//------------------------------------------------------------------------------

{TWinCtrl}

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

function GetFileVersion(filename: string): Integer;
const
  MAX_TEXTSIZE = 1024;
var
  fileHandle: dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..MAX_TEXTSIZE - 1] of char;
  buf: pchar;
begin
  Result := -1;
  StrpCopy(querybuf, filename);
  l := GetFileVersionInfoSize(querybuf, fileHandle);
  if (l > 0) then
  begin
    GetMem(buf, l);
    GetFileVersionInfo(querybuf, fileHandle, l, buf);
    if verqueryvalue(buf, '\', pointer(pvs), lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;

//------------------------------------------------------------------------------

function IsThemedApp: Boolean;
var
  i: Integer;
begin
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
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

{ TAdvCustomGroupBox }

constructor TAdvCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csOpaque];
  FCheckBox := TGroupBoxCheck.Create(Self);
  FCheckBox.OnChange := OnCheckBoxChanged;
  FTransparent := True;
  FImages := nil;
  FImageIndex := -1;
  FBorderStyle := bsSingle;
  FCaptionPosition := cpTopLeft;
  FRoundEdges := false;
  Ctl3D := false;
  ParentCtl3D := false;
  FBorderColor := clSilver;
end;

procedure TAdvCustomGroupBox.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) and CheckBox.Visible and (CheckBox.Action <> caNone) then
    PerformCheckBoxAction
end;

//------------------------------------------------------------------------------

destructor TAdvCustomGroupBox.Destroy;
begin
  FCheckBox.Free;
  inherited;
end;

procedure TAdvCustomGroupBox.DoGroupCheckClick;
begin
  if Assigned(OnCheckBoxClick) then
    OnCheckBoxClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and CheckBox.Visible and (CheckBox.Action <> caNone) then
    PerformCheckBoxAction;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if Enabled and CheckBox.Visible then
  begin
//    if MouseCapture then
    begin
      CheckBox.Down := PtOnCaption(Point(X, Y));
      MouseCapture := CheckBox.Down;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if Enabled and CheckBox.Visible then
  begin
    CheckBox.Hot := PtOnCaption(Point(X, Y));
    if MouseCapture then
    begin
      CheckBox.Down := CheckBox.Hot;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if Enabled and CheckBox.Visible then
  begin
    if CheckBox.Down then
    begin
      MouseCapture := False;
      CheckBox.Down := False;
      if PtOnCaption(Point(X, Y)) then
        ToggleCheck;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.CMEnabledChanged(var Message: TMessage);
var
  i: integer;
begin
  inherited;

  Invalidate;

  if (CheckBox.Action = caDisable) then
  begin
    CheckBox.Checked := Enabled;

    for I := 0 to ControlCount - 1 do
    begin
      if (CheckBox.Action = caDisable) then
      begin
        Controls[I].Enabled := Enabled;
      end
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  r: TRect;

begin
  hi := PHintInfo(Msg.LParam);

  r := GetCheckBoxRect;

  if PtInRect(r, hi^.CursorPos) then
  begin
    if CheckBox.Hint <> '' then
      hi^.HintStr := CheckBox.Hint;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if Enabled and CheckBox.Visible then
  begin
    CheckBox.Hot := False;
    CheckBox.Down := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = Images) then
    begin
      Images := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.AdjustClientRect(var Rect: TRect);
var
  R: TRect;
begin
  R := Rect;
  inherited  AdjustClientRect(Rect);
  Rect := R;
  if CaptionPosition in [cpTopLeft, cpTopCenter, cpTopRight] then
  begin
    Inc(Rect.Top, Max(GetBorderWidth,GetCaptionHeight));
    Rect := Classes.Rect(Rect.Left + GetBorderWidth, Rect.Top, Rect.Right -GetBorderWidth, Rect.Bottom-GetBorderWidth);
  end
  else if CaptionPosition in [cpBottomLeft, cpBottomCenter, cpBottomRight] then
  begin
    Dec(Rect.Bottom, Max(GetBorderWidth,GetCaptionHeight));
    Rect := Classes.Rect(Rect.Left + GetBorderWidth, Rect.Top + GetBorderWidth, Rect.Right -GetBorderWidth, Rect.Bottom);
  end;

  InflateRect(Rect, -1, -1);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.DrawCheck;
var
  bmp: TBitmap;
  HTheme: THandle;
  r: TRect;
  DrawThemed: boolean;
begin
  if not CheckBox.Visible then
    Exit;

  DrawThemed := IsVista and CheckBox.Themed and IsThemeActive and IsThemedApp;

  r := GetCheckBoxRect;

  if DrawThemed then
  begin
    HTheme := OpenThemeData(Self.Handle,'button');

    if HTheme <> 0 then
    begin
      case CheckBox.State of
      cbChecked:
        begin
          if not Enabled then
             DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil)
          else if CheckBox.Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL or CBS_PUSHED,@r,nil)
          else if CheckBox.Hot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT or CBS_HOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil);
        end;
      cbUnChecked:
        begin
          if not Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil)
          else if CheckBox.Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL or CBS_PUSHED,@r,nil)
          else if CheckBox.Hot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_HOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil);
        end;
      cbGrayed:
        begin

          if not Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,@r,nil)
          else if CheckBox.Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDPRESSED,@r,nil)
          else if CheckBox.Hot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDHOT or CBS_HOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,@r,nil);
         end;
      end;
      CloseThemeData(HTheme);
    end
    else
      DrawThemed := false;
  end;

  if not DrawThemed then
  begin
    bmp := TBitmap.Create;

    if CheckBox.State = cbGrayed then
    begin
      if CheckBox.Down then
        bmp.LoadFromResourceName(hinstance,'TMSGRPGD')
      else if CheckBox.Hot then
        bmp.LoadFromResourceName(hinstance,'TMSGRPGH')
      else
        bmp.LoadFromResourceName(hinstance,'TMSGRPG');

    end
    else
    if (CheckBox.State = cbChecked) then
    begin
      if CheckBox.Down then
        bmp.LoadFromResourceName(hinstance,'TMSGRPCD')
      else if CheckBox.Hot then
        bmp.LoadFromResourceName(hinstance,'TMSGRPCH')
      else
        bmp.LoadFromResourceName(hinstance,'TMSGRPC');

    end
    else
    begin
      if CheckBox.Down then
        bmp.LoadFromResourceName(hinstance,'TMSGRPUD')
      else if CheckBox.Hot then
        bmp.LoadFromResourceName(hinstance,'TMSGRPUH')
      else
        bmp.LoadFromResourceName(hinstance,'TMSGRPU');
    end;

    bmp.Transparent := true;
    bmp.TransparentMode := tmAuto;

    Canvas.Draw(R.Left, R.Top, bmp);
    bmp.free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.PaintTransparency;
var
  i: Integer;
  P: TPoint;

begin
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
  begin
    (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);
  end;

  RestoreDC(Canvas.Handle, i);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.Paint;
var
  R, CapR, FCR: TRect;
  i, rt: Integer;
  bmp: TBitmap;
begin
  if Transparent then
    PaintTransparency;

  FCR := CalculateRect(R, CapR);
  R := ClientRect;
  CapR := GetCaptionRect;

  bmp := TBitmap.Create;
  bmp.Height := (FCR.Bottom - FCR.Top);
  bmp.Width := (FCR.Right - FCR.Left) + 2;
  i := CapR.Left;
  rt := 6;

  //--- Draw Check
  DrawCheck;

  //--- Draw Image
  if Assigned(Images) and (ImageIndex >= 0) then
  begin
    Images.Draw(Canvas, CapR.Left, CapR.Top, ImageIndex, Enabled);
    i := CapR.Left + Images.Width + 3;
  end;

  Canvas.Brush.Style := bsClear;
  //--- Draw Caption
  if (Caption <> '') then
  begin
    Canvas.Font.Assign(Self.Font);

    if not Enabled then
      Canvas.Font.Color := clGray;

    R := Rect(i, CapR.Top, CapR.Right, CapR.Bottom);
    DrawText(Canvas.Handle,PChar(Caption),Length(Caption), R, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
  end;

  bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), Canvas, Rect(FCR.Left-1, FCR.Top, FCR.Right+1, FCR.Bottom));
  R := GetBorderRect;
  //--- Draw Borders
  case BorderStyle of
    bsSingle:
    begin
      (*
      if Ctl3D then
      begin

        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := clWhite;
        R.Left := R.Left + 1;
        R.Top := R.Top + 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);

        Canvas.Pen.Color := clGray;
        R.Bottom := R.Bottom -1;
        R.Right := R.Right - 1;
        R.Left := R.Left - 1;
        R.Top := R.Top - 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);
      end
      else
      *)
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := BorderColor;

        if Ctl3D and not IsWin8 then
        begin
          Canvas.Pen.Color := clWhite;
          Canvas.RoundRect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom, rt, rt);
          R.Bottom := R.Bottom - 1;

          if BorderColor = clSilver then // default color
            Canvas.Pen.Color := $E5DFD5;
        end;

        if FRoundEdges then
        begin
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt);
        end
        else
        begin
          Canvas.Rectangle(R);
        end;
      end;
    end;
    bsDouble:
    begin
      if Ctl3D then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := clWhite;

        R.Left := R.Left + 1;
        R.Top := R.Top + 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);

        Canvas.Pen.Color := clGray;
        R.Bottom := R.Bottom -1;
        R.Right := R.Right - 1;
        R.Left := R.Left - 1;
        R.Top := R.Top - 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);

        R.Bottom := R.Bottom +1;
        R.Right := R.Right + 1;

        R := Rect(R.Left+2, R.Top+2, R.Right-2, R.Bottom-2);

        Canvas.Pen.Color := clWhite;
        R.Left := R.Left + 1;
        R.Top := R.Top + 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);
        Canvas.Pen.Color := clGray;
        R.Bottom := R.Bottom -1;
        R.Right := R.Right - 1;
        R.Left := R.Left - 1;
        R.Top := R.Top - 1;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);
      end
      else
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := BorderColor;
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);
        R := Rect(R.Left+2, R.Top+2, R.Right-2, R.Bottom-2);
        if FRoundEdges then
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, rt, rt)
        else
          Canvas.Rectangle(R);
      end;
    end;
  end;

  if HasCaption then
  begin
    Canvas.CopyRect(Rect(FCR.Left - 1, FCR.Top, FCR.Right+1, FCR.Bottom), bmp.Canvas, Rect(0, 0, bmp.Width, bmp.Height));
  end;

  bmp.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
    Realign;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    if not Assigned(FImages) then
    begin
      ImageIndex := -1;
    end;
    Invalidate;
    Realign;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetTransparent(const Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.HasCaption: boolean;
begin
  Result := (Caption <> '') or (CheckBox.Visible) or ((ImageIndex >= 0) and Assigned(Images));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetCaptionHeight: Integer;
var
  R, R1, R2: TRect;
begin
  R := CalculateRect(R1, R2); //GetCaptionRect;
  Result := Max(GetBorderWidth, R.Bottom - R.Top);
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetBorderWidth: Integer;
begin
  Result := 0;
  case BorderStyle of
    bsNone: Result := 1;
    bsSingle:
    begin
      Result := 1;
      if Ctl3D then
        Result := Result + 1;
    end;
    bsDouble:
    begin
      Result := 2;
      if Ctl3D then
        Result := Result + 2;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetBorderRect: TRect;
begin
  Result := ClientRect;
  if HasCaption and (CaptionPosition in [cpTopLeft, cpTopCenter, cpTopRight]) then
  begin
    Result.Top := Result.Top + (GetCaptionHeight div 2);
  end
  else if CaptionPosition in [cpBottomLeft, cpBottomCenter, cpBottomRight] then
  begin
    if HasCaption then
    begin
      Result.Bottom := Result.Bottom - (GetCaptionHeight div 2);
      if (BorderStyle = bsDouble) then
        Result.Bottom := Result.Bottom + 1;
    end;  
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetCaptionRect: TRect;
var
  //ImgH, ImgW, CapH, CapW, sp, st, w, h: Integer;
  R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  CalculateRect(R, Result);
  {
  ImgH := 0;
  ImgW := 0;
  CapH := 0;
  CapW := 0;
  st := 8;
  sp := 0;
  if (Caption <> '') then
  begin
    Canvas.Font.Assign(Self.Font);
    R := Rect(0, 0, 1000, 100);
    DrawText(Canvas.Handle,PChar(Caption),Length(Caption), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    CapH := R.Bottom - R.Top;
    CapW := R.Right - R.Left;
  end;

  if Assigned(Images) and (ImageIndex >= 0) then
  begin
    ImgH := Images.Height;
    ImgW := Images.Width;
  end;

  if (CapW > 0) and (ImgW > 0) then
  begin
    sp := 3;
  end;

  w := ImgW + sp + CapW;
  h := Max(ImgH, CapH) + 2;
  case CaptionPosition of
    cpTopLeft:
    begin
      Result.Left := st;
      Result.Right := Result.Left + w;
      Result.Bottom := Result.Top + h;
    end;
    cpTopRight:
    begin
      Result.Right := Width - st;
      Result.Left := Result.Right - w;
      Result.Bottom := Result.Top + h;
    end;
    cpTopCenter:
    begin
      Result.Left := (Width - w) div 2;
      Result.Right := Result.Left + w;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomLeft:
    begin
      Result.Left := st;
      Result.Right := Result.Left + w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomRight:
    begin
      Result.Right := Width - st;
      Result.Left := Result.Right - w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomCenter:
    begin
      Result.Left := (Width - w) div 2;
      Result.Right := Result.Left + w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
  end;
 }
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.CalculateRect(var CheckBoxR, ImgTextR: TRect): TRect;
var
  ImgH, ImgW, CapH, CapW, sp, st, w, h, cksz, cksp: Integer;
  R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  CheckBoxR := Rect(-1, -1, -1, -1);
  ImgTextR := Result;

  ImgH := 0;
  ImgW := 0;

  st := 8;
  sp := 0;
  cksz := 0;
  cksp := 0;
  if (Caption <> '') then
  begin
    Canvas.Font.Assign(Self.Font);
    R := Rect(0, 0, 1000, 100);
    DrawText(Canvas.Handle,PChar(Caption),Length(Caption), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    CapH := R.Bottom - R.Top;
    CapW := R.Right - R.Left;
  end
  else
  begin
    {
    Canvas.Font.Assign(Self.Font);
    R := Rect(0, 0, 1000, 100);
    DrawText(Canvas.Handle,PChar('AA'),Length('AA'), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    CapH := R.Bottom - R.Top;
    CapW := R.Right - R.Left;
    DrawText(Canvas.Handle,PChar(''),Length(''), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    }
    CapH := 0;
    CapW := 0;
  end;

  if Assigned(Images) and (ImageIndex >= 0) then
  begin
    ImgH := Images.Height;
    ImgW := Images.Width;
  end;

  if (CapW > 0) and (ImgW > 0) then
  begin
    sp := 3;
  end;

  if (CheckBox.Visible) then
  begin
    if CheckBox.Themed then
      cksz := 16
    else
      cksz := 14;
    if (CapW > 0) or (ImgW > 0) then
      cksp := 3;
  end;

  w := cksz + cksp + ImgW + sp + CapW;
  h := Max(cksz, Max(ImgH, CapH)) + 2;
  case CaptionPosition of
    cpTopLeft:
    begin
      Result.Left := st;
      Result.Right := Result.Left + w;
      Result.Bottom := Result.Top + h;
    end;
    cpTopRight:
    begin
      Result.Right := Width - st;
      Result.Left := Result.Right - w;
      Result.Bottom := Result.Top + h;
    end;
    cpTopCenter:
    begin
      Result.Left := (Width - w) div 2;
      Result.Right := Result.Left + w;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomLeft:
    begin
      Result.Left := st;
      Result.Right := Result.Left + w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomRight:
    begin
      Result.Right := Width - st;
      Result.Left := Result.Right - w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
    cpBottomCenter:
    begin
      Result.Left := (Width - w) div 2;
      Result.Right := Result.Left + w;
      Result.Top := Height - h;
      Result.Bottom := Result.Top + h;
    end;
  end;

  ImgTextR := Result;
  if (CheckBox.Visible) then
  begin
    if (CheckBox.Position = cpLeft) then
    begin
      CheckBoxR := Rect(Result.Left, Result.Top + ((h - cksz) div 2), Result.Left + cksz, Result.Top + ((h - cksz) div 2) + cksz);
      ImgTextR.Left := CheckBoxR.Right + cksp;
    end
    else //if (CheckBox.Position = cpRight) then
    begin
      CheckBoxR := Rect(Result.Right - cksz, Result.Top + ((h - cksz) div 2), Result.Right, Result.Top + ((h - cksz) div 2) + cksz);
      ImgTextR.Right := CheckBoxR.Left - cksp;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.ToggleCheck;
begin
  if CheckBox.AllowGrayed then
  begin
    case CheckBox.State of
    cbUnchecked: CheckBox.State := cbGrayed;
    cbChecked: CheckBox.State := cbUnchecked;
    cbGrayed: CheckBox.State := cbChecked;
    end;
  end
  else
    CheckBox.Checked := not CheckBox.Checked;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.PerformCheckBoxAction;
var
  i: Integer;
begin
  if not (csDesigning in ComponentState) and Enabled and CheckBox.Visible and
    (CheckBox.State in [cbChecked, cbUnChecked]) and (CheckBox.Action <> caNone) then
  begin
    for I := 0 to ControlCount - 1 do
    begin
      if (CheckBox.Action in [caDisable, caControlsDisable]) then
      begin
        Controls[I].Enabled := (CheckBox.State = cbChecked);
      end
      else if (CheckBox.Action = caCheckAll) then
      begin
        if (Controls[I] is TCheckBox) then
        begin
          TCheckBox(Controls[I]).Checked := (CheckBox.State = cbChecked);
        end
        else if (Controls[I] is TWinControl) and (UpperCase(Controls[I].ClassName) = ('TADVOFFICECHECKBOX')) then
        begin
          SendMessage(TWinControl(Controls[I]).Handle, BM_SETCHECK, Integer(CheckBox.State), 0);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.PtOnCaption(P: TPoint): Boolean;
var
  R, R1, R2: TRect;
begin
  R := CalculateRect(R1, R2);
  Result := PtInRect(R, p);
end;

//------------------------------------------------------------------------------

function TAdvCustomGroupBox.GetCheckBoxRect: TRect;
var
  R: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  CalculateRect(Result, R);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetCheckBox(const Value: TGroupBoxCheck);
begin
  FCheckBox.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.OnCheckBoxChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
    Realign;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetCaptionPosition(
  const Value: TCaptionPosition);
begin
  if (FCaptionPosition <> Value) then
  begin
    FCaptionPosition := Value;
    Invalidate;
    Realign;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGroupBox.SetRoundEdges(const Value: Boolean);
begin
  if (FRoundEdges <> Value) then
  begin
    FRoundEdges := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

{ TGroupBoxCheck }

procedure TGroupBoxCheck.Assign(Source: TPersistent);
begin
  if (Source is TGroupBoxCheck) then
  begin
    FHint := (Source as TGroupBoxCheck).Hint;
    State := (Source as TGroupBoxCheck).State;
    FThemed := (Source as TGroupBoxCheck).Themed;
    FAllowGrayed := (Source as TGroupBoxCheck).AllowGrayed;
    FAction := (Source as TGroupBoxCheck).Action;
    Checked := (Source as TGroupBoxCheck).Checked;
    Position := (Source as TGroupBoxCheck).Position;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TGroupBoxCheck.Create(AOwner: TAdvCustomGroupBox);
begin
  inherited Create;
  FGroupBox := AOwner;
  FHint := '';
  FState := cbUnChecked;
  FThemed := True;
  FAllowGrayed := False;
  FAction := caDisable;
  FPosition := cpLeft;
end;

//------------------------------------------------------------------------------

destructor TGroupBoxCheck.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TGroupBoxCheck.GetChecked: Boolean;
begin
  Result := (State = cbChecked);
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetAction(const Value: TCheckBoxAction);
begin
  if (FAction <> Value) then
  begin
    FAction := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetAllowGrayed(const Value: Boolean);
begin
  if (FAllowGrayed <> Value) then
  begin
    FAllowGrayed := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetChecked(const Value: Boolean);
begin
  if (Value <> (State = cbChecked)) then
  begin
    if Value then
      State := cbChecked
    else
      State := cbUnchecked;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetDown(const Value: Boolean);
begin
  if (FDown <> Value) then
  begin
    FDown := Value;
    FGroupBox.DrawCheck;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetHint(const Value: string);
begin
  if (FHint <> Value) then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TGroupBoxCheck.SetHot(const Value: Boolean);
begin
  if (FHot <> Value) then
  begin
    FHot := Value;
    FGroupBox.DrawCheck;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetPosition(const Value: TCheckBoxPos);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetState(const Value: TCheckBoxState);
begin
  if (FState <> Value) then
  begin
    FState := Value;
    FGroupBox.DrawCheck;
    FGroupBox.PerformCheckBoxAction;

    FGroupBox.DoGroupCheckClick;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetThemed(const Value: Boolean);
begin
  if (FThemed <> Value) then
  begin
    FThemed := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGroupBoxCheck.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}



end.
