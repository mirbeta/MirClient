{*************************************************************************}
{ TAdvSmoothSpinEDIT component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 1996-2012                                         }
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

unit AdvSmoothSpin;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Mask
  {$IFDEF TMSPACKGDIP}
  ,AdvXPVS
  {$ELSE}
  ,AdvSmoothXPVS
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : first release 


{$R ADVSMOOTHSPIN.RES}

type
  TNumGlyphs = Buttons.TNumGlyphs;

  TAdvSmoothTimerSpeedButton = class;

  TWinCtrl = class(TWinControl);

{ TAdvSmoothSpinButton }

  TSpinDirection = (spVertical,spHorizontal);

  TLabelPosition = (lpLeftTop,lpLeftCenter,lpLeftBottom,lpTopLeft,lpBottomLeft,
                    lpLeftTopLeft,lpLeftCenterLeft,lpLeftBottomLeft,lpTopCenter,
                    lpBottomCenter);

  TEditAlign = (eaLeft,eaRight,eaCenter);

  TAdvSmoothSpinButton = class (TWinControl)
  private
    FUpButton: TAdvSmoothTimerSpeedButton;
    FDownButton: TAdvSmoothTimerSpeedButton;
    FFocusedButton: TAdvSmoothTimerSpeedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    FDirection: TSpinDirection;
    function CreateButton: TAdvSmoothTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TAdvSmoothTimerSpeedButton);
    procedure SetDirection(const Value: TSpinDirection);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMEraseBkGnd(var Message: TWMEraseBkgnd);  message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property DragKind;
    property Constraints;
    property OnStartDock;
    property OnEndDock;
    property Ctl3D;
    property Direction: TSpinDirection read fDirection write SetDirection;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
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
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

{ TAdvSmoothSpinEdit }

  TAdvSmoothSpinType = (sptNormal,sptFloat,sptDate,sptTime,sptHex);

  TAdvSmoothSpinEdit = class(TCustomMaskEdit)
  private
    FFlat: Boolean;
    FFlatLineColor: TColor;
    FLabel: TLabel;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabelMargin: Integer;
    FLabelTransparent: boolean;
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FMinFloatValue: Double;
    FMaxFloatValue: Double;
    FMinDateValue: TDateTime;
    FMaxDateValue: TDateTime;
    FDateValue: TDateTime;
    FTimeValue: TDateTime;
    FHexValue: Longint;
    FIncrement: LongInt;
    FIncrementFloat : Double;
    FIncrementPage: Longint;
    FIncrementFloatPage: Double;
    FIncrementSmart: boolean;
    FButton: TAdvSmoothSpinButton;
    FEditorEnabled: Boolean;
    FDirection: TSpinDirection;
    FSpinType:TAdvSmoothSpinType;
    FPrecision: integer;
    FReturnIsTab: boolean;
    FNormalColor: TColor;
    FUSDates: Boolean;
    FSpinFlat : Boolean;
    FSpinTransparent : Boolean;
    FTransparent: Boolean;
    FAutoFocus: boolean;
    FIncrementHours: Integer;
    FIncrementMinutes: Integer;
    FShowSeconds: Boolean;
    FSigned: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FEditAlign: TEditAlign;
    FFocusBorderColor: TColor;
    FFocusColor: TColor;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    FAllowNullValue: boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    function GetFloatValue: double;
    function CheckFloatValue (NewValue: double): double;
    function CheckDateValue (NewValue: TDateTime): TDateTime;
    procedure SetFloatValue (NewValue: double);
    procedure SetEditRect;
    procedure SetEditorEnabled(NewValue:boolean);
    procedure SetDirection(const Value: TSpinDirection);
    procedure SetPrecision(const Value: integer);
    procedure SetSpinType(const Value: TAdvSmoothSpinType);
    function GetTimeValue: TDateTime;
    procedure SetTimeValue(const Value: TDateTime);
    function GetDateValue: TDateTime;
    procedure SetDateValue(const Value: TDateTime);
    function GetHexValue: integer;
    procedure SetHexValue(const Value: integer);
    procedure SetSpinFlat(const Value : boolean);
    procedure SetSpinTransparent(const value : boolean);

    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;    
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMChar(var Msg:TWMKey); message WM_CHAR;
    procedure WMPaint(var Msg:TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;    
    function GetLabelCaption: string;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelMargin(const Value: integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: boolean);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure LabelFontChange(Sender: TObject);
    procedure SetLabelFont(const Value: TFont);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetFlatRect(const Value: Boolean);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetShowSeconds(const Value: Boolean);
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    procedure SetEditAign(const Value: TEditAlign);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure DrawBorder;
    procedure DrawControlBorder(DC: HDC);
  protected
    function GetVersionNr: Integer; virtual;
    function IsValidChar(var Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure UpdateLabel;
    function CreateLabel: TLabel;
    procedure PaintEdit;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TAdvSmoothSpinButton read FButton;
    procedure IncSmart;
    procedure DecSmart;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Init;
    property SpinLabel: TLabel read FLabel;
  published
    property AllowNullValue: boolean read FAllowNullValue write FAllowNullValue default True;
    property AutoFocus: boolean read FAutoFocus write FAutoFocus default False;
    property Direction : TSpinDirection read FDirection write SetDirection default spVertical;
    property ReturnIsTab: boolean read FReturnIsTab write FReturnIsTab default False;
    property Precision: integer read FPrecision write SetPrecision default 0;
    property SpinType: TAdvSmoothSpinType read FSpinType write SetSpinType default sptNormal;
    property Value: LongInt read GetValue write SetValue;
    property FloatValue: double read GetFloatValue write SetFloatValue;
    property TimeValue: TDateTime read GetTimeValue write SetTimeValue;
    property DateValue: TDateTime read GetDateValue write SetDateValue;
    property HexValue: Integer read GetHexValue write SetHexValue;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor default clBlack;
    property SpinFlat : boolean read FSpinFlat write SetSpinFlat default False;
    property SpinTransparent : boolean read FSpinTransparent write SetSpinTransparent default False;
    property Anchors;
    property Constraints;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditAlign: TEditAlign read FEditAlign write SetEditAign default eaLeft;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clWindow;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property IncrementFloat: double read FIncrementFloat write FIncrementFloat;
    property IncrementPage: Longint read FIncrementPage write FIncrementPage default 10;
    property IncrementFloatPage: double read FIncrementFloatPage write FIncrementFloatPage;
    property IncrementSmart: boolean read FIncrementSmart write FIncrementSmart default False;
    property IncrementMinutes: Integer read FIncrementMinutes write FIncrementMinutes default 1;
    property IncrementHours: Integer read FIncrementHours write FIncrementHours default 1;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;  
    property LabelCaption:string read GetLabelCaption write SetLabelCaption;
    property LabelPosition:TLabelPosition read fLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read fLabelMargin write SetLabelMargin default 4;
    property LabelTransparent:boolean read fLabelTransparent write SetLabelTransparent default False;
    property LabelFont:TFont read fLabelFont write SetLabelFont;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue default 0;
    property MinValue: LongInt read FMinValue write FMinValue default 0;
    property MinFloatValue: double read fMinFloatValue write fMinFloatValue;
    property MaxFloatValue: double read fMaxFloatValue write fMaxFloatValue;
    property MinDateValue: TDateTime read fMinDateValue write fMinDateValue;
    property MaxDateValue: TDateTime read fMaxDateValue write fMaxDateValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Signed: Boolean read FSigned write FSigned default False;
    property ShowHint;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds default True;
    property TabOrder;
    property TabStop;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible:boolean read GetVisible write SetVisible;
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
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
  end;

  TAdvSmoothMaskSpinEdit = class(TAdvSmoothSpinEdit)
  published
    property EditMask;
  end;


{ TAdvSmoothTimerSpeedButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  TButtonDirection = (bdLeft,bdRight,bdUp,bdDown);

  TAdvSmoothTimerSpeedButton = class(TSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    FButtonDirection:TButtonDirection;
    FIsWinXP: Boolean;
    FHasMouse: Boolean;
    procedure TimerExpired(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected

    function DoVisualStyles: Boolean;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    property IsWinXP: Boolean read FIsWinXP;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
    property ButtonDirection: TButtonDirection read FButtonDirection write FButtonDirection;

  end;

implementation

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

{$I DELPHIXE.INC}

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;

function PosFrom(sub,s:string;n: Integer): Integer;
begin
  Delete(s,1,n);
  Result := Pos(sub,s);
  if Result > 0 then Result := Result + n;
end;

function IncYear(d: TDateTime;n: Integer): TDateTime;
var
  da,mo,ye:word;
begin
  DecodeDate(d,ye,mo,da);
  ye := ye + n;
  Result := Encodedate(ye,mo,da);
end;

{returns value from 0..255 of 2char hex string}
function HexVal(s:string): Integer;
var
  i,j: Integer;
begin
  if length(s)=1 then
   begin
    i:=ord(upcase(s[1]))-ord('0');
    if (i>10) then i:=10+i-(ord('A')-ord('0'));
    result:=i;
   end
  else
   begin
    i:=ord(upcase(s[1]))-ord('0');
    if (i>10) then i:=10+i-(ord('A')-ord('0'));
    j:=ord(upcase(s[2]))-ord('0');
    if (j>10) then j:=10+j-(ord('A')-ord('0'));
    result:=i*16+j;
   end;
end;


{ TAdvSmoothSpinButton }

constructor TAdvSmoothSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];

  FUpButton := CreateButton;
  FDownButton := CreateButton;
  UpGlyph := nil;
  DownGlyph := nil;

  FUpButton.ButtonDirection := bdUp;
  FDownButton.ButtonDirection := bdDown;

  Width := 15;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TAdvSmoothSpinButton.CreateButton: TAdvSmoothTimerSpeedButton;
begin
  Result := TAdvSmoothTimerSpeedButton.Create (Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
end;

procedure TAdvSmoothSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TAdvSmoothSpinButton.AdjustSize (var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if fDirection = spVertical then
   begin
    if W < 15 then W := 15;
    FUpButton.SetBounds (0, 0, W, H div 2);
    FDownButton.SetBounds (0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
   end
  else
   begin
    if W < 20 then W := 20;
    FDownButton.SetBounds (0, 0, W div 2, H );
    FUpButton.SetBounds ((W div 2)+1, 0 , W div 2, H );
   end;
end;

procedure TAdvSmoothSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TAdvSmoothSpinButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothSpinButton.WMSize(var Message: TWMSize);
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

procedure TAdvSmoothSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TAdvSmoothSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TAdvSmoothSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TAdvSmoothSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TAdvSmoothTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TAdvSmoothSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TAdvSmoothSpinButton.SetDirection(const Value:TSpinDirection);
begin
 if (value<>fDirection) then
  begin
   fDirection:=value;
   recreatewnd;
   if fdirection=spVertical then
     begin
       width:=15;
       fUpButton.FButtonDirection:=bdUp;
       fDownButton.FButtonDirection:=bdDown;
     end
   else
     begin
      width:=20;
      fUpButton.FButtonDirection:=bdRight;
      fDownButton.FButtonDirection:=bdLeft;
     end;
  end;
end;

procedure TAdvSmoothSpinButton.SetFocusBtn (Btn: TAdvSmoothTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TAdvSmoothSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TAdvSmoothSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds (Left, Top, W, H);
end;

function TAdvSmoothSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TAdvSmoothSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'AC_AdvSpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TAdvSmoothSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TAdvSmoothSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TAdvSmoothSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TAdvSmoothSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'AC_AdvSpinDown');
    FUpButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TAdvSmoothSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TAdvSmoothSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;


{ TAdvSmoothSpinEdit }

constructor TAdvSmoothSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TAdvSmoothSpinButton.Create (Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FIncrementPage := 10;
  FIncrementFloat := 0.1;
  FIncrementFloatPage := 1.0;
  FIncrementMinutes := 1;
  FIncrementHours := 1;
  FEditorEnabled := True;
  FMinFloatValue := 0;
  FMinValue := 0;
//  FMaxFloatValue := 100;
//  FMaxValue := 100;
  FShowSeconds := True;
  FFocusBorderColor := clNone;
  FFocusColor := clWindow;
  FAllowNullValue := true;

  FLabel := nil;
  FLabelMargin := 4;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
end;

destructor TAdvSmoothSpinEdit.Destroy;
begin
  FButton := nil;
  if FLabel <> nil then
  begin
    FLabel.Free;
    FLabel := nil;
  end;
  FLabelFont.Free;
  inherited Destroy;
end;

procedure TAdvSmoothSpinEdit.Loaded;
begin
  inherited;

//  if not (csDesigning in ComponentState) then
    Init;

  case FSpinType of
  sptDate:self.Text := DateToStr(FDateValue);
  sptTime:
    begin
      if FShowSeconds then
        self.Text := FormatDateTime('h'+TimeSeparator+'nn'+TimeSeparator+'ss',FTimeValue)
      else
        self.Text := FormatDateTime('h'+TimeSeparator+'nn',FTimeValue)
    end;
  sptHex:self.Text := '0x'+inttohex(FHexValue,0);
  end;

  SetSpinType(fSpinType);

  FFlat := not FFlat;
  SetFlat(not fFlat);

//  Height := FLoadedHeight;
  SetBounds(Left,Top,Width,Height);


  FUSDates := Pos('M',Uppercase(ShortDateFormat)) < Pos('D',Uppercase(ShortDateFormat));

  if Assigned(FLabel) and not Enabled then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := False;

  if not ctl3D then
  begin
    // force an editrect update
    width := width + 1;
    width := width - 1;
  end;

end;

procedure TAdvSmoothSpinEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if FFocusBorderColor <> clNone then
    Invalidate;

  if FFocusColor <> clNone then
  begin
    inherited Color := FFocusColor;
    if FButton.FUpButton.IsWinXP then
    begin
      Width := Width + 1;
      Width := Width - 1;
    end;
  end;
   
end;

procedure TAdvSmoothSpinEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
    Color := FNormalColor;

  if not AllowNullValue then
  begin
    case SpinType of
      sptNormal, sptHex:
        begin
          if (MinValue <> MaxValue) and (Text = '') then
            Value := MinValue;
        end;
      sptFloat:
        begin
          if (MinFloatValue <> MaxFloatValue) and (Text = '') then
            FloatValue := MinFloatValue;
        end;
      sptDate, sptTime:
        begin
          if (MinDateValue <> MaxDateValue) and (Text = '') then
            DateValue := MinDateValue;
        end;
    end;
  end;

  if FButton.FUpButton.IsWinXP then
  begin
    Width := Width + 1;
    Width := Width - 1;
  end
  else
    Invalidate;
end;


procedure TAdvSmoothSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TAdvSmoothSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  ss,sl: Integer;
  oldval: String;
begin
  inherited KeyDown(Key, Shift);

  ss := SelStart;
  sl := SelLength;

  oldval := self.Text;

  case key of
  vk_up:if not (FIncrementSmart and (sl=0) and (fSpinType in [sptFloat,sptNormal])) then UpClick (Self) else IncSmart;
  vk_down:if not (FIncrementSmart and (sl=0) and (fSpinType in [sptFloat,sptNormal])) then DownClick(self) else DecSmart;
  vk_next:if not (FIncrementSmart and (sl=0)) then
           case fSpinType of
           sptNormal: Value := Value - FIncrementPage;
           sptFloat:  FloatValue := FloatValue - FIncrementFloatPage;
           end
         else
           DecSmart;

  vk_prior:if not (FIncrementSmart and (sl=0)) then
           case fSpinType of
           sptNormal: Value := Value + FIncrementPage;
           sptFloat:  FloatValue := FloatValue + FIncrementFloatPage;
           end
          else IncSmart;

  vk_delete:if not fEditorEnabled or ((fSpinType=sptHex) and (selstart<2)) then key:=0;
  vk_return:
    begin
      if FReturnIsTab then
            begin
             key:=vk_tab;
             postmessage(self.handle,wm_keydown,VK_TAB,0);
            end;

     end;
  end;

  if key in [vk_up,vk_down,vk_next,vk_prior,vk_delete,vk_return] then
  begin
    SelStart := ss;
    SelLength := sl;
  end;

  if oldval <> self.Text then
    Modified := True;
end;

procedure TAdvSmoothSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

function CheckSignedNum(ch:char): boolean;
begin
  Result := IsNumChar(ch) or (ch = '-') or (ch = '+');
end;

function CheckHex(ch:char): boolean;
begin
  Result := IsNumChar(ch) or ((ch >= 'A') and (ch <= 'F')) or ((ch >= 'a') and (ch <= 'f'));
end;

function TAdvSmoothSpinEdit.IsValidChar(var Key: Char): Boolean;
var
  dp: Integer;
begin
  Result := (Key = DecimalSeparator) or (Key = ThousandSeparator) or (Key = TimeSeparator) or (Key = DateSeparator)
    or (CheckSignedNum(Key)) or ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (FSpinType = sptNormal) and ((Key = DecimalSeparator) or (Key = ThousandSeparator) or (Key = TimeSeparator) or (Key = DateSeparator)) then
    Result := False;

  if ((key = TimeSeparator) and (FSpinType <> sptTime) and
    not (((TimeSeparator = DecimalSeparator) or (TimeSeparator = ThousandSeparator) or (TimeSeparator = '-') or (TimeSeparator = '+')) and (FSpinType in [sptNormal, sptFloat]))) then
    Result := False;

  if ((key = DateSeparator) and (FSpinType <> sptDate) and
    not (((DateSeparator = DecimalSeparator) or (TimeSeparator = ThousandSeparator) or (TimeSeparator = '-') or (TimeSeparator = '+')) and (FSpinType in [sptNormal, sptFloat]))) then
    Result := False;

  if (FSpinType = sptFloat) and not ( (key = chr(VK_ESCAPE)) or (key = chr(VK_RETURN)) or (key = chr(VK_BACK))) then
  begin
    if (Key = ThousandSeparator) then
      Key := DecimalSeparator;

    if (Key = DecimalSeparator) and (SelLength = Length(self.Text)) then
      Result := true
    else
      if (Key = DecimalSeparator) and ((Pos(DecimalSeparator,self.Text)>0) or (FPrecision = 0)) then
        Result := False;

    dp := Pos(DecimalSeparator,self.Text);

    if (FPrecision > 0) and (dp > 0) and (selstart >= dp) and (sellength = 0) then
    begin
      if (Length(self.Text) >= dp + FPrecision) then
        Result := False;
    end;
  end;

  if FSpinType = sptTime then
  begin
     if (Key <> TimeSeparator) and ((Key = ',') or (key = '.')) then
       Key := TimeSeparator;

    if (Key = TimeSeparator) and (Pos(TimeSeparator,self.Text) > 0) then
      Result := False;
  end;

  if FSpinType = sptHex then
  begin
     Result := (CheckHex(Key) or
               ((Key < #32) and (Key <> Chr(VK_RETURN)))) and (SelStart >= 2);
     if result and (key=char(vk_back)) and (selstart=2) then result:=false;
  end;

  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TAdvSmoothSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;

  case FEditAlign of
  eaRight:
    begin
      Params.Style := Params.Style or ES_MULTILINE;
      Params.Style := Params.Style AND NOT (ES_LEFT) AND NOT (ES_CENTER);
      Params.Style := Params.Style OR (ES_RIGHT);
    end;
  eaCenter:
    begin
      Params.Style := Params.Style or ES_MULTILINE;
      Params.Style := Params.Style AND NOT (ES_LEFT) AND NOT (ES_RIGHT);
      Params.Style := Params.Style OR (ES_CENTER);
    end;
  end;
  
end;

procedure TAdvSmoothSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TAdvSmoothSpinEdit.SetDirection(const value: TSpinDirection);
begin
  if Value <> FDirection then
  begin
    FDirection := Value;
    FButton.Direction := Value;
    // force organisation update
    self.Width := self.Width + 1;
    self.Width := self.Width - 1;
  end;
end;

procedure TAdvSmoothSpinEdit.SetEditorEnabled(NewValue:boolean);
begin
  FEditorEnabled := NewValue;
end;

procedure TAdvSmoothSpinEdit.SetEditRect;
var
  Loc: TRect;
  Dist : integer;
begin

  if BorderStyle = bsNone then
    Dist := 2
  else
    Dist := 0;
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2 - Dist;
  Loc.Top := Dist;
  Loc.Left := Dist;

  if not Ctl3D then
    loc.Left := loc.Left + 2;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TAdvSmoothSpinEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
  if FLabel <> nil then UpdateLabel;
end;


procedure TAdvSmoothSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist: Integer;

begin
  inherited;

  if BorderStyle = bsNone then Dist:=1 else Dist:=5;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - dist, 0, FButton.Width, Height - dist)
    else
      FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TAdvSmoothSpinEdit.GetMinHeight: Integer;
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
  if BorderStyle = bsSingle then
   Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2
  else
   Result := Metrics.tmHeight + (I div 4) +  2;
end;

procedure TAdvSmoothSpinEdit.UpClick(Sender: TObject);
var
  ss,sl: Integer;
  oldval: string;
begin
  ss := SelStart;
  sl := SelLength;

  FUSDates := Pos('M',Uppercase(ShortDateFormat)) < Pos('D',Uppercase(ShortDateFormat));

  oldval := self.Text;

  if ReadOnly then MessageBeep(0)
  else
    case FSpinType of
    sptNormal:
      begin
        if FIncrementSmart then
          IncSmart
        else
          Value := Value + FIncrement;
      end;
    sptFloat:
      if FIncrementSmart then
        IncSmart
      else
        FloatValue := FloatValue + FIncrementFloat;
    sptTime:
      begin
        if SelStart >= Pos(TimeSeparator,Text) then
        begin
          if (selstart >= PosFrom(TimeSeparator,text,Pos(TimeSeparator,text))) and ShowSeconds then
            TimeValue := TimeValue + EncodeTime(0,0,1,0)
          else
            TimeValue := TimeValue + EncodeTime(0,FIncrementMinutes,0,0);
        end
        else
        begin
          if IncrementHours = 0 then
            TimeValue := TimeValue + EncodeTime(0,FIncrementMinutes,0,0)
          else
            TimeValue := TimeValue + EncodeTime(FIncrementHours,0,0,0);
        end;
      end;
    sptDate:
      begin
        if FUSDates then
        begin
          if SelStart >= Pos(DateSeparator,Text) then
          begin
            if (SelStart >= PosFrom(DateSeparator,text,pos(DateSeparator,text))) then
              DateValue := IncYear(DateValue,1)
            else
              DateValue := DateValue + 1
          end
          else
            DateValue := IncMonth(DateValue,1);
        end
        else
        begin
          if SelStart >= Pos(DateSeparator,Text) then
          begin
            if SelStart >= PosFrom(DateSeparator,text,pos(DateSeparator,text)) then
              DateValue := IncYear(DateValue,1)
            else
              DateValue := IncMonth(DateValue,1);
          end
          else
            DateValue := DateValue + 1;
        end;
      end;
    sptHex:
      begin
        HexValue := HexValue + FIncrement;
      end;
    end;


  if (pos('-',oldval) > 0) and (pos('-',self.Text) = 0) then
  begin
    dec(ss);
  end;
  if (pos('-',oldval) = 0) and (pos('-',self.Text) > 0) then
  begin
    inc(ss);
  end;

  SelStart := ss;
  SelLength := sl;

  if oldval <> self.Text then
    Modified := True;

  if Assigned(FOnUpClick) then
    FOnUpClick(self);
end;

procedure TAdvSmoothSpinEdit.DownClick (Sender: TObject);
var
  ss,sl: Integer;
  oldval: string;
begin
  ss := SelStart;
  sl := SelLength;

  oldval := self.Text;

  FUSDates := Pos('M',Uppercase(ShortDateFormat)) < Pos('D',Uppercase(ShortDateFormat));

  if ReadOnly then MessageBeep(0)
  else
    case FSpinType of
    sptNormal:
      if FIncrementSmart then
        DecSmart
      else
        Value := Value - FIncrement;
    sptFloat:
      if FIncrementSmart then
        DecSmart
      else
        FloatValue := FloatValue - FIncrementFloat;
    sptTime:
      begin
        if SelStart >= Pos(TimeSeparator,text) then
        begin
          if (selstart >= PosFrom(TimeSeparator,text,pos(TimeSeparator,text))) and ShowSeconds then
          begin
            if TimeValue >= EncodeTime(0,0,1,0) then
              TimeValue := TimeValue - EncodeTime(0,0,1,0)
          end
          else
          begin
            if TimeValue >= EncodeTime(0,FIncrementMinutes,0,0) then
              TimeValue := TimeValue - EncodeTime(0,FIncrementMinutes,0,0);
          end;
        end
        else
        begin
          if FIncrementHours = 0 then
          begin
            if TimeValue >= EncodeTime(0,FIncrementMinutes,0,0) then
              TimeValue := TimeValue - encodetime(0,FIncrementMinutes,0,0)
          end
          else
          begin
            if TimeValue >= EncodeTime(FIncrementHours,0,0,0) then
              TimeValue := TimeValue - encodetime(FIncrementHours,0,0,0);
          end;
        end;
      end;
    sptDate:
      begin
        if FUSDates then
        begin
          if Selstart >= Pos(DateSeparator,Text) then
          begin
            if SelStart >= PosFrom(DateSeparator,text,pos(DateSeparator,text)) then
              DateValue := IncYear(DateValue, - 1)
            else
              DateValue := DateValue - 1;
          end
          else
            DateValue := IncMonth(DateValue,-1);
        end
        else
        begin
          if Selstart >= Pos(DateSeparator,Text) then
          begin
            if SelStart >= PosFrom(DateSeparator,text,pos(DateSeparator,text)) then
              DateValue := IncYear(DateValue,-1)
            else
              DateValue := IncMonth(DateValue,-1);
          end
          else
            DateValue := DateValue - 1;
        end;
      end;
    sptHex:
      begin
        HexValue := HexValue - FIncrement;
      end;

    end;


  if (pos('-',oldval) > 0) and (pos('-',self.Text) = 0) then
  begin
    dec(ss);
  end;
  if (pos('-',oldval) = 0) and (pos('-',self.Text) > 0) then
  begin
    inc(ss);
  end;


  SelStart := ss;
  SelLength := sl;

  if oldval <> self.Text then
    Modified := True;

  if Assigned(FOnDownClick) then
    FOnDownClick(self);
end;

procedure TAdvSmoothSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvSmoothSpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TAdvSmoothSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  case fSpinType of
  sptNormal:if CheckValue(Value) <> Value then
    SetValue(Value);
  sptFloat: SetFloatValue(CheckFloatValue(FloatValue));
//  sptFloat:if CheckFloatValue(FloatValue) <> FloatValue then SetFloatValue(FloatValue);
  sptTime:if CheckDateValue (TimeValue) <> TimeValue then SetTimeValue(TimeValue);
  sptDate:if CheckDateValue (DateValue) <> DateValue then SetDateValue(DateValue);
  sptHex:HexValue:= CheckValue(HexValue);
  end;
end;

function TAdvSmoothSpinEdit.GetValue: LongInt;
begin
  try
    Result := StrToInt (Text);
  except
    Result := FMinValue;
  end;
end;

function TAdvSmoothSpinEdit.CheckFloatValue (NewValue: Double): Double;
begin
  Result := NewValue;
  if not (csLoading in ComponentState) then
  begin
    if (FMaxFloatValue <> FMinFloatValue) then
    begin
      if NewValue < FMinFloatValue then
        Result := FMinFloatValue
      else if NewValue > FMaxFloatValue then
        Result := FMaxFloatValue;
    end;
  end;
end;

function TAdvSmoothSpinEdit.GetFloatValue: Double;
begin
  try
    Result := StrToFloat(Text);
  except
    Result := FMinValue;
  end;
end;

procedure TAdvSmoothSpinEdit.SetFloatValue (NewValue: Double);
begin
  if FPrecision < 0 then
    Text := FloatToStrF (CheckFloatValue (NewValue), ffGeneral,4,4)
  else
    if FPrecision = 0 then
      Text := FloatToStr (CheckFloatValue (NewValue))
    else
      Text := FloatToStrF (CheckFloatValue (NewValue),ffFixed,15,FPrecision);
end;

procedure TAdvSmoothSpinEdit.SetValue(NewValue: LongInt);
begin
  Text := IntToStr(CheckValue(NewValue));
  if not FEditorEnabled and AutoSelect then SelectAll;
end;

function TAdvSmoothSpinEdit.CheckValue(NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if not (csLoading in ComponentState) then
  begin
    if (FMaxValue <> 0) or (FMinValue <> 0) then
    begin
      if NewValue < FMinValue then
        Result := FMinValue
      else if NewValue > FMaxValue then
        Result := FMaxValue;
    end;
  end;
end;

function TAdvSmoothSpinEdit.CheckDateValue (NewValue: TDateTime): TDateTime;
begin
  Result := NewValue;
  if not (csLoading in ComponentState) then
  begin
    if (FMaxDateValue <> FMinDateValue) then
    begin
      if NewValue < FMinDateValue then
        Result := FMinDateValue
      else if NewValue > FMaxDateValue then
        Result := FMaxDateValue;
    end;
  end;
end;


procedure TAdvSmoothSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and (not (csLButtonDown in ControlState) or not FeditorEnabled) then
    SelectAll;
  inherited;
end;

procedure TAdvSmoothSpinEdit.SetSpinFlat(const value: boolean);
begin
  FButton.FUpButton.Flat := Value;
  FButton.FDownButton.Flat := Value;
  FSpinFlat := value;
end;

procedure TAdvSmoothSpinEdit.SetSpinTransparent(const value: boolean);
begin
  FButton.FUpButton.Transparent := Value;
  FButton.FDownButton.Transparent := Value;
  Fspintransparent:=value;
  Width := Width + 1;
  Width := Width - 1;
end;

procedure TAdvSmoothSpinEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if FLabel <> nil then
    FLabel.Parent := AParent;
end;

procedure TAdvSmoothSpinEdit.SetPrecision(const Value: integer);
begin
  FPrecision := Value;
  if FSpinType = sptFloat then
    FloatValue := GetFloatValue;
end;

procedure TAdvSmoothSpinEdit.SetSpinType(const Value: TAdvSmoothSpinType);
begin
  if FSpinType <> value then
    FSpinType := Value;
    
  if not (csLoading in ComponentState) then
  case fSpinType of
  sptFloat:floatvalue := GetFloatValue;
  sptNormal:self.value := GetValue;
  sptTime:self.TimeValue := GetTimeValue;
  sptDate:self.DateValue := GetDateValue;
  sptHex:self.HexValue := GetHexValue;
  end;
end;

function TAdvSmoothSpinEdit.GetTimeValue: TDateTime;
begin

  try
    if not FShowSeconds then
      Result := Int(FTimeValue) + StrToTime(Text+TimeSeparator+'00')
    else
      Result := Int(FTimeValue) + StrToTime(Text);
  except
    Result := 0;
  end;
end;

procedure TAdvSmoothSpinEdit.SetTimeValue(const Value: TDateTime);
var
  ss: Integer;
begin
  FTimeValue := Value;
  if (csLoading in ComponentState) then
    Exit;

  ss := SelStart;

  if FShowSeconds then
    Text := FormatDateTime('h'+TimeSeparator+'nn'+TimeSeparator+'ss',Value)
  else
    Text := FormatDateTime('h'+TimeSeparator+'nn',Value);

  Selstart := ss;
end;

function TAdvSmoothSpinEdit.GetDateValue: TDateTime;
begin
  if (Text = '0') or (Text = '') then
    Result := Now
  else
   try
     Result := Frac(FDateValue) + StrToDate(Text);
   except
     Result := FMinDateValue;
   end;
end;

procedure TAdvSmoothSpinEdit.SetDateValue(const Value: TDateTime);
var
  ss: Integer;
begin
  FDateValue := Value;
  if (csLoading in ComponentState) then
    Exit;
  FDateValue := CheckDateValue(value);
  ss := SelStart;
  text := DateToStr(FDateValue);
  SelStart := ss;
end;

function TAdvSmoothSpinEdit.GetHexValue: integer;
var
 s:string;
 r: Integer;
begin
 s:=self.text;
 delete(s,1,2);
 r:=0;
 while (length(s)>=2) do
  begin
   r:=r*256+HexVal(copy(s,1,2));
   delete(s,1,2);
  end;
 if (length(s)=1) then
  begin
   r:=r*16+HexVal(s);
  end;
 result:=r;
end;

procedure TAdvSmoothSpinEdit.SetHexValue(const Value: integer);
begin
 fHexValue:=value;
 if (csLoading in ComponentState) then exit;
 text:='0x'+IntToHex(value,0);
end;

function Exp10(n: Integer): Integer;
var
 i: Integer;
begin
 result:=1;
 for i:=1 to n do result:=result*10;
end;

function Div10(n: Integer):double;
var
 i: Integer;
begin
 result:=1;
 for i:=1 to n do result:=result/10;
end;

procedure TAdvSmoothSpinEdit.DecSmart;
var
 ss,sp: Integer;
 incd: Integer;
 incf:double;
begin
 ss:=Length(Text)-SelStart;
 sp:=Length(Text)-Pos(decimalseparator,self.Text);

 if (SelStart=0) or ((pos('-',Text)>0) and (SelStart=1)) then
   begin
    case fSpinType of
    sptNormal: Value := Value - Increment;
    sptFloat:  FloatValue := FloatValue - IncrementFloat;
    end;
    Exit;
   end;

 if (sp=Length(Text)) then sp:=-1;

 if (sp<=ss) then
  begin
   incd:=Exp10(ss-sp-1);
   case fSpinType of
   sptNormal: Value := Value - incd;
   sptFloat:  FloatValue := FloatValue - incd;
   end;
 end
 else
  begin
   incf:=Div10(sp-ss);
   case fSpinType of
   sptFloat:  FloatValue := FloatValue - incf;
   end;
  end;
end;

procedure TAdvSmoothSpinEdit.IncSmart;
var
  ss,sp,dc: Integer;
  incd: Integer;
  incf: double;
begin
  ss := Length(Text) - SelStart;
  dc := Pos(DecimalSeparator,self.Text);
  sp := Length(Text) - dc;

  if (SelStart = 0) or ((pos('-',Text) > 0) and (SelStart = 1)) then
  begin
    case fSpinType of
    sptNormal: Value := Value + Increment;
    sptFloat:  FloatValue := FloatValue + IncrementFloat;
    end;
    Exit;
  end;

  if (sp = Length(Text)) then sp := -1;

  if (sp <= ss)  then
  begin
    incd := Exp10(ss-sp-1);
    case fSpinType of
    sptNormal: Value := Value + incd;
    sptFloat:  FloatValue := FloatValue + incd;
  end;
  end
  else
  begin
    incf := Div10(sp-ss);
    case fSpinType of
    sptFloat:  FloatValue := FloatValue + incf;
    end;
  end;
end;

procedure TAdvSmoothSpinEdit.UpdateLabel;
begin
 fLabel.transparent:=flabeltransparent;
 case fLabelPosition of
 lpLeftTop:begin
            flabel.top:=self.top;
            flabel.left:=self.left-flabel.canvas.textwidth(flabel.caption)-fLabelMargin;
           end;
 lpLeftCenter:begin
               flabel.top:=self.top+((self.height-flabel.height) shr 1);
               flabel.left:=self.left-flabel.canvas.textwidth(flabel.caption)-flabelMargin;
              end;
 lpLeftBottom:begin
               flabel.top:=self.top+self.height-flabel.height;
               flabel.left:=self.left-flabel.canvas.textwidth(flabel.caption)-fLabelMargin;
              end;
 lpTopLeft:begin
             flabel.top:=self.top-flabel.height-fLabelMargin;
             flabel.left:=self.left;
           end;
 lpTopCenter:begin
               FLabel.Top := self.top-FLabel.height-FLabelMargin;

               if self.Width - FLabel.Width > 0 then
                 FLabeL.Left := self.Left + ((self.Width - FLabel.Width) shr 1)
               else
                FLabeL.Left := self.Left - ((FLabel.Width - self.Width) shr 1)
             end;

 lpBottomLeft:begin
               flabel.top:=self.top+self.height+fLabelMargin;
               flabel.left:=self.left;
              end;
 lpBottomCenter:begin
                 FLabel.top := self.top+self.height+FLabelMargin;
                 if self.Width - FLabel.Width > 0 then
                   FLabeL.Left := self.Left + ((self.Width - FLabel.width) shr 1)
                 else
                   FLabeL.Left := self.Left - ((FLabel.Width - self.width) shr 1)
               end;

 lpLeftTopLeft:begin
                flabel.top:=self.top;
                flabel.left:=self.left-fLabelMargin;
               end;
 lpLeftCenterLeft:begin
                  flabel.top:=self.top+((self.height-flabel.height) shr 1);
                  flabel.left:=self.left-flabelMargin;
                  end;
 lpLeftBottomLeft:begin
                   flabel.top:=self.top+self.height-flabel.height;
                   flabel.left:=self.left-fLabelMargin;
                  end;
 end;
 fLabel.Font.Assign(fLabelFont);
end;

function TAdvSmoothSpinEdit.CreateLabel: TLabel;
begin
  Result := Tlabel.Create(self);
  Result.Parent := Parent;
  Result.FocusControl := Self;
  Result.Font.Assign(Font);
end;

function TAdvSmoothSpinEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAdvSmoothSpinEdit.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;

  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvSmoothSpinEdit.SetLabelMargin(const Value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvSmoothSpinEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvSmoothSpinEdit.SetLabelTransparent(const Value: boolean);
begin
  FLabeltransparent := Value;
  if FLabel <> nil then
    UpdateLabel;
end;

function TAdvSmoothSpinEdit.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvSmoothSpinEdit.SetVisible(const Value: boolean);
begin
  inherited Visible := Value;
  if (FLabel <> nil) then
    FLabel.Visible := value;
end;

procedure TAdvSmoothSpinEdit.LabelFontChange(Sender: TObject);
begin
  if (FLabel <> nil) then
    UpdateLabel;
end;

procedure TAdvSmoothSpinEdit.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
end;


procedure TAdvSmoothSpinEdit.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;

procedure TAdvSmoothSpinEdit.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;

procedure TAdvSmoothSpinEdit.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) then
    if FTransparent then Invalidate;
  inherited;
end;

procedure TAdvSmoothSpinEdit.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if FLabel<>nil then
      FLabel.Font.Assign(self.font);
  inherited;
  SetFlatRect(fFlat);
end;


procedure TAdvSmoothSpinEdit.WMPaint(var Msg:TWMPaint);
begin
  inherited;
  PaintEdit;
  if FFocusBorderColor <> clNone then
    DrawBorder;
end;

procedure TAdvSmoothSpinEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if FFocusBorderColor <> clNone then
    DrawBorder;
end;


procedure TAdvSmoothSpinEdit.DrawBorder;
var
  DC: HDC;
begin
  if not (FFocusBorderColor <> clNone) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TAdvSmoothSpinEdit.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
  BtnFaceBrush: HBRUSH;
begin
  if (FFocusBorderColor <> clNone) then
  begin
    if (GetFocus = self.Handle) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;
end;

procedure TAdvSmoothSpinEdit.PaintEdit;
var
 DC: HDC;
 oldpen: HPen;
 loc: TRect;
 voffset: Integer;
begin
  if FFlat then
  begin
    DC := GetDC(Handle);
    voffset := 1;

    oldpen := SelectObject(dc,CreatePen( PS_SOLID,1,ColorToRGB(FFlatLineColor)));

    SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

    MovetoEx(dc,loc.left,self.height-vOffset,nil);
    LineTo(dc,loc.right,self.height-vOffset);
    DeleteObject(selectobject(dc,oldpen));

    ReleaseDC(Handle,DC);
  end;
end;


procedure TAdvSmoothSpinEdit.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
  DC: HDC;
  i: integer;
  p: TPoint;
begin
  if FTransparent then
  begin
    if Assigned(Parent) then
    begin
      DC := Message.DC;
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, LParam(DC), 0);
      TWinCtrl(Parent).PaintControls(DC, nil);
      RestoreDC(DC, i);
    end;
  end
  else
    inherited;
end;


procedure TAdvSmoothSpinEdit.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothSpinEdit.SetFlatRect(const Value: Boolean);
var
  loc:TRect;
begin
  if value then
  begin
    loc.left := 2;
    loc.top := 4;
    loc.right := ClientRect.Right - 2 - FButton.Width - 2;
    loc.bottom := ClientRect.Bottom - 4;
  end
  else
  begin
    loc.left := 0;
    loc.top := 0;
    loc.right := Clientrect.Right - FButton.Width - 2;
    loc.bottom := Clientrect.Bottom;
  end;

  SendMessage(self.Handle,EM_SETRECTNP,0,LParam(@loc));

end;

procedure TAdvSmoothSpinEdit.SetFlat(const value: boolean);
//var
// OldColor:TColor;
begin
  if (csLoading in ComponentState) then
  begin
    FFlat := Value;
    Exit;
  end;

  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
    begin
      FNormalColor := Color;
      Borderstyle := bsNone;
      SetFlatRect(True);
    end
    else
    begin
      if (FNormalColor <> clNone) then
        Color := FNormalColor;
      Borderstyle := bsSingle;
      SetFlatRect(false);
    end;
    Invalidate;
  end;
end;


procedure TAdvSmoothSpinEdit.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvSmoothSpinEdit.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if (FLabel <> nil) then FLabel.Visible := Visible;

end;

procedure TAdvSmoothSpinEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if FAutoFocus then
    SetFocus;
end;

procedure TAdvSmoothSpinEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

procedure TAdvSmoothSpinEdit.WMKeyDown(var Msg:TWMKeydown);
begin
  inherited;
  if (Msg.CharCode = VK_RETURN) and (GetKeyState(VK_CONTROL) and $8000 = $8000) then
  begin
    PostMessage(Handle,WM_KEYDOWN,VK_UP,0);
  end;
end;

procedure TAdvSmoothSpinEdit.WMChar(var Msg: TWMKey);
var
  key: Char;
  s:string;
  OldSS,OldSL: integer;
begin
  if (Msg.CharCode = Ord('+')) then
    Msg.CharCode := 0;

  if not ((SpinType = sptDate) and (DateSeparator = '-')) then
  begin
    if (Msg.CharCode = Ord('-')) then
    begin
      Msg.CharCode := 0;
      s := Text;

      OldSS := SelStart;
      OldSL := SelLength;

      if Signed then
      begin
        if pos('-',s)=1 then
        begin
          Delete(s,1,1);
          Dec(OldSS);
        end
        else
        begin
          s := '-'  + s;
          inc(OldSS);
        end;
      end;
      Text := s;

      SelStart := OldSS;
      SelLength := OldSL;
    end;
  end;


  if Msg.CharCode = VK_RETURN then
  begin
    key := #13;
    if Assigned(OnKeyPress) then
      OnKeyPress(Self,key);
    Msg.CharCode := 0;
    Exit;
  end;
  inherited;
end;

procedure TAdvSmoothSpinEdit.SetShowSeconds(const Value: Boolean);
var
  TV: TDateTime;
begin
  if SpinType = sptTime then
  begin
    TV := FTimeValue;
    FShowSeconds := Value;
    SetTimeValue(TV);
  end;
end;

procedure TAdvSmoothSpinEdit.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then
    if Value then
      FLabel.Enabled := True;
  Invalidate;
end;

function TAdvSmoothSpinEdit.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvSmoothSpinEdit.SetEnabledEx(const Value: Boolean);
var
  OldValue: Boolean;
begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
     (csDesigning in ComponentState) then
    Exit;

  if OldValue <> Value then
  begin
    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
        FLabel.Enabled := Value;
  end;
end;

procedure TAdvSmoothSpinEdit.SetEditAign(const Value: TEditAlign);
begin
  FEditAlign := Value;
  ReCreateWnd;
end;

function TAdvSmoothSpinEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothSpinEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothSpinEdit.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothSpinEdit.Init;
begin
  FNormalColor := Color;
end;

{TAdvSmoothTimerSpeedButton}

destructor TAdvSmoothTimerSpeedButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TAdvSmoothTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;

  InvalidateRect(parent.handle,nil,true);
end;

procedure TAdvSmoothTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);

  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;

  InvalidateRect(parent.handle,nil,true);
end;

procedure TAdvSmoothTimerSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TAdvSmoothTimerSpeedButton.Paint;
const
  Flags: array[Boolean] of Integer = (0, DFCS_PUSHED);
  Flats: array[Boolean] of Integer = (0, DFCS_FLAT);
var
  R: TRect;
  HTheme: THandle;
begin
  R := GetClientRect;

  if DoVisualStyles then
  begin
    htheme := OpenThemeData((Owner as TWinControl).Handle,'spin');

    case FButtonDirection of
    bdLeft:
      begin
        if FState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWNHORZ,DNHZS_PRESSED,@r,nil)
        else
        begin
          if FHasMouse then
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWNHORZ,DNHZS_HOT,@r,nil)
          else
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWNHORZ,DNHZS_NORMAL,@r,nil);
        end;
      end;
    bdRight:
      begin
        if fState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UPHORZ,UPHZS_PRESSED,@r,nil)
        else
        begin
          if FHasMouse then
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_UPHORZ,UPHZS_HOT,@r,nil)
          else
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_UPHORZ,UPHZS_NORMAL,@r,nil);
        end;
      end;
    bdUp:
      begin
        if fState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,UPS_PRESSED,@r,nil)
        else
        begin
          if FHasMouse then
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,UPS_HOT,@r,nil)
          else
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,UPS_NORMAL,@r,nil);
        end;
      end;

    bdDown:
      begin
        if fState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DNS_PRESSED,@r,nil)
        else
        begin
          if FHasMouse then
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DNS_HOT,@r,nil)
          else
            DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DNS_NORMAL,@r,nil);
        end;
      end;
    end;

    CloseThemeData(htheme);
  end
  else
  begin
    case FButtonDirection of
    bdLeft:DrawFrameControl(Canvas.Handle,r,DFC_SCROLL,DFCS_SCROLLLEFT or flags[fState=bsDown] or flats[flat]);
    bdRight:DrawFrameControl(Canvas.Handle,r,DFC_SCROLL,DFCS_SCROLLRIGHT or flags[fState=bsDown] or flats[flat]);
    bdUp,bdDown:inherited Paint;
    end;
  end;
end;

constructor TAdvSmoothTimerSpeedButton.Create(AOwner: TComponent);
var
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;
begin
  inherited;

  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));

  FIsWinXP := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  FHasMouse := False;
end;

function TAdvSmoothTimerSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

procedure TAdvSmoothTimerSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHasMouse := True;
  Invalidate;
end;

procedure TAdvSmoothTimerSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHasMouse := False;
  Invalidate;
end;

end.
