{*********************************************************************}
{ TInspectorBar inplace edit controls                                 }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2001 - 2014                                            }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit InspEdits;

{$I TMSDEFS.INC}

interface

{$R InspEdits.Res}

uses
  Windows, Messages, Classes, Forms, Controls, Graphics, StdCtrls, SysUtils,
  InspXPVS, Buttons, ExtCtrls, Mask, ComCtrls, Dialogs
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TWinCtrl = class(TWinControl);

  TAdvSpeedButton = class(TSpeedButton)
  private
    FIsWinXP: Boolean;
    FFlat: Boolean;
    FHot: Boolean;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    function DoVisualStyles: Boolean;
  protected
    procedure Paint; override;
  public
    property Hot: Boolean read FHot write FHot;
  published
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
    property Flat: Boolean read FFlat write FFlat;
  end;


  TInspCustomCombo = class(TCustomComboBox)
  private
    FAutoFocus: boolean;
    FFlat: Boolean;
    FEtched: Boolean;
    FOldColor: TColor;
    FOldParentColor: Boolean;
    FButtonWidth: Integer;
    FFocusBorder: Boolean;
    FMouseInControl: Boolean;
    FDropWidth: integer;
    FIsWinXP: Boolean;
    FBkColor: TColor;
    procedure SetEtched(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetButtonWidth(const Value: Integer);
    procedure DrawButtonBorder(DC:HDC);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand (var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetDropWidth(const Value: integer);
    function DoVisualStyles: Boolean;
  protected
    property BkColor: TColor read FBkColor write FBkColor;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    property Flat: Boolean read FFlat write SetFlat;
    property Etched: Boolean read FEtched write SetEtched;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus;
    property DropWidth: integer read fDropWidth write SetDropWidth;
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TInspComboBox = class(TInspCustomCombo)
  published
    property AutoFocus;
    property ButtonWidth;
    property Style;
    property Flat;
    property Etched;
    property FocusBorder;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property DropWidth;
    property Enabled;
    property Font;
    {$IFNDEF DELPHI2_LVL}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    {$IFDEF DELPHI6_LVL}
    property OnCloseUp;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  TNumGlyphs = Buttons.TNumGlyphs;

  TAdvTimerSpeedButton = class;

{ TInspSpinButton }

  TSpinDirection = (spVertical,spHorizontal);

  TInspSpinButton = class (TWinControl)
  private
    FUpButton: TAdvTimerSpeedButton;
    FDownButton: TAdvTimerSpeedButton;
    FFocusedButton: TAdvTimerSpeedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    FDirection: TSpinDirection;
    FIsWinXP: Boolean;
    //FHasUpRes,FHasDownRes:boolean;
    function CreateButton: TAdvTimerSpeedButton;
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
    procedure SetFocusBtn (Btn: TAdvTimerSpeedButton);
    procedure SetDirection(const Value: TSpinDirection);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetIsWinXP(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property IsWinXP: Boolean read FIsWinXP write SetIsWinXP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property Direction: TSpinDirection read fDirection write SetDirection;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property DragCursor;
    property DragKind;
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
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

{ TInspSpinEdit }

  TInspSpinType = (sptNormal,sptFloat,sptDate,sptTime);

  TInspSpinEdit = class(TCustomMaskEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FMinFloatValue: double;
    FMaxFloatValue: double;
    FMinDateValue: TDatetime;
    FMaxDateValue: TDatetime;
    FDateValue: TDatetime;
    FTimeValue: TDatetime;
    FIncrement: LongInt;
    FIncrementFloat : double;
    FButton: TInspSpinButton;
    FEditorEnabled: Boolean;
    FDirection: TSpinDirection;
    FSpinType:TInspSpinType;
    FPrecision: integer;
    FReturnIsTab: boolean;
    FIsWinXP: Boolean;
    FOnSpinUp: TNotifyEvent;
    FOnSpinDown: TNotifyEvent;
    FOnSpinChange: TNotifyEvent;
    FSpinFlat : Boolean;
    FSpinTransparent : Boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    function GetFloatValue: double;
    function CheckFloatValue (NewValue: double): double;
    function CheckDateValue (NewValue: TDatetime): TDatetime;
    procedure SetFloatValue (NewValue: double);
    procedure SetEditorEnabled(NewValue:boolean);
    procedure SetDirection(const Value: TSpinDirection);
    procedure SetPrecision(const Value: integer);
    procedure SetSpinType(const Value: TInspSpinType);
    function GetTimeValue: TDatetime;
    procedure SetTimeValue(const Value: TDatetime);
    function GetDateValue: tdatetime;
    procedure SetDateValue(const Value: TDatetime);
    procedure SetSpinFlat(const Value : boolean);
    procedure SetSpinTransparent(const value : boolean);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure SetIsWinXP(const Value: Boolean);
  protected
    function IsValidChar(var Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    property IsWinXP: Boolean read FIsWinXP write SetIsWinXP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TInspSpinButton read FButton;
    procedure SetEditRect;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property Direction : TSpinDirection read FDirection write SetDirection;
    property ReturnIsTab: boolean read FReturnIsTab write fReturnIsTab;
    property Precision: integer read FPrecision write SetPrecision;
    property SpinType: TInspSpinType read FSpinType write SetSpinType;
    property Value: LongInt read GetValue write SetValue;
    property FloatValue: double read GetFloatValue write SetFloatValue;
    property TimeValue: TDatetime read GetTimeValue write SetTimeValue;
    property DateValue: TDatetime read GetDateValue write SetDateValue;
    property SpinFlat : boolean read FSpinFlat write SetSpinFlat;
    property SpinTransparent : boolean read FSpinTransparent write SetSpinTransparent;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property IncrementFloat: double read FIncrementFloat write FIncrementFloat;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property MinFloatValue: double read fMinFloatValue write fMinFloatValue;
    property MaxFloatValue: double read fMaxFloatValue write fMaxFloatValue;
    property MinDateValue: TDatetime read fMinDateValue write fMinDateValue;
    property MaxDateValue: TDatetime read fMaxDateValue write fMaxDateValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnSpinUp: TNotifyEvent read FOnSpinUp write FOnSpinUp;
    property OnSpinDown: TNotifyEvent read FOnSpinDown write FOnSpinDown;
    property OnSpinChange: TNotifyEvent read FOnSpinChange write FOnSpinChange;
  end;

  TAdvMaskSpinEdit = class(TInspSpinEdit)
  published
    property EditMask;
  end;


{ TAdvTimerSpeedButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  TButtonDirection = (bdLeft,bdRight,bdUp,bdDown);

  TAdvTimerSpeedButton = class(TSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    FButtonDirection:TButtonDirection;
    FIsWinXP: Boolean;
    procedure TimerExpired(Sender: TObject);
    function DoVisualStyles: Boolean;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
  public
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
    property ButtonDirection: TButtonDirection read FButtonDirection write FButtonDirection;
  end;

  TInspEditButton = class (TWinControl)
  private
    FButton: TAdvSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    FFlat: Boolean;
    FIsWinXP: Boolean;
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
    procedure SetIsWinXP(const Value: Boolean);
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
    property Flat: Boolean read FFlat write SetFlat;
    property IsWinXP: Boolean read FIsWinXP write SetIsWinXP;
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

{ TInspEditBtn }

  TInspEditBtn = class(TCustomEdit)
  private
    FUnitSize : integer;
    FRightAlign: Boolean;
    FButton: TInspEditButton;
    FEditorEnabled: Boolean;
    FOnClickBtn:TNotifyEvent;
    FButtonWidth: Integer;
    FIsWinXP: Boolean;
    //FGlyph: TBitmap;
    function GetMinHeight: Integer;
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
    procedure BtnExit(Sender: TObject); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure ResizeControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TInspEditButton read FButton;
    procedure SetEditRect;
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
    property RightAlign:boolean read fRightAlign write SetRightAlign;
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

  TInspDateTimePicker = class(TDateTimePicker)
  private
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
  protected
  published
  public
  end;

  TInspColorComboBox = class(TInspComboBox)
  private
    FCustomColor: TColor;
    function GetColorValue: TColor;
    procedure SetColorValue(const Value: TColor);
  protected
     procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
  published
    property IsWinXP;
    property BkColor;
    property ColorValue: TColor read GetColorValue write SetColorValue;
    property CustomColor: TColor read FCustomColor write FCustomColor;
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

{ TInspCustomCombo }

constructor TInspCustomCombo.Create(AOwner: TComponent);
begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FOldColor := inherited Color;
  FOldParentColor := inherited ParentColor;
  FFlat := True;
  FMouseInControl := False;
end;

procedure TInspCustomCombo.SetButtonWidth(const Value: integer);
begin
  if (value < 14) or (value > 32) then Exit;
  FButtonWidth := Value;
  Invalidate;
end;

procedure TInspCustomCombo.SetFlat(const Value: Boolean);
begin
  if Value<>FFlat then
  begin
    FFlat := Value;
    Ctl3D := not Value;
    Invalidate;
  end;
end;

procedure TInspCustomCombo.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := Value;
    Invalidate;
  end;
end;

procedure TInspCustomCombo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DrawBorders;
end;

procedure TInspCustomCombo.CMExit(var Message: TCMExit);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DrawBorders;
end;

procedure TInspCustomCombo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    DrawBorders;
  end;
  if FAutoFocus then
    Self.SetFocus;
end;

procedure TInspCustomCombo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    DrawBorders;
  end;
end;

procedure TInspCustomCombo.CMEnabledChanged(var Msg: TMessage);
begin
  if FFlat then
  begin
    if Enabled then
    begin
      inherited ParentColor := FOldParentColor;
      inherited Color := FOldColor;
    end
    else
    begin
      FOldParentColor := inherited Parentcolor;
      FOldColor := inherited Color;
      inherited ParentColor := True;
    end;
  end;
  inherited;
end;

procedure TInspCustomCombo.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if FFlat then
    DrawBorders;
end;


function IsMouseButtonDown:Boolean;
{
  Returns a "True" if a Mouse button happens to be down.
}
begin

  {Note: Key state is read twice because the first time you read it, you
learn
   if the bittpm has been pressed ever.  The second time you read it you
learn if
   the button is currently pressed.}
  if ((GetAsyncKeyState(VK_RBUTTON)and $8000)=0) and
     ((GetAsyncKeyState(VK_LBUTTON)and $8000)=0) then
  begin
    {Mouse buttons are up}
    Result:=False;
  end
  else
  begin
    {Mouse buttons are up}
    Result:=True;
  end;

end;


procedure TInspCustomCombo.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;

  procedure DrawButton;
  var
    ARect: TRect;
    htheme: THandle;
  begin
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Inc(ARect.Left, ClientWidth - FButtonWidth);
    InflateRect(ARect, -1, -1);

    ARect.Bottom := ARect.Top + 17;

    if DoVisualStyles then
    begin
      htheme := OpenThemeData(Handle,'combobox');
      if IsMouseButtonDown then
        DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
      else
        DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);

      CloseThemeData(htheme);
    end
    else
      DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT );

    ExcludeClipRect(DC, ClientWidth - FButtonWidth - 4 , 0, ClientWidth + 2, ClientHeight);
  end;

begin
  if not FFlat then
  begin
    inherited;
    Exit;
  end;

  if Message.DC = 0 then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;

  try
    if (Style <> csSimple) and not DoVisualStyles then
    begin
      FillRect(DC, ClientRect, Brush.Handle);
      DrawButton;
    end;
    PaintWindow(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;

  if DoVisualStyles then
    inherited;

  DrawBorders;
end;

function TInspCustomCombo.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

function TInspCustomCombo.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TInspCustomCombo.DrawButtonBorder(DC: HDC);
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);
var
  ARect: TRect;
  BtnFaceBrush: HBRUSH;
begin
  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4, ClientWidth - 4, ClientHeight - 4);

  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  if Is3DBorderButton then
    DrawEdge(DC, ARect, Edge[Etched], BF_RECT or Flags[DroppedDown])
  else
  begin
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    InflateRect(ARect, 0, -1);
    ARect.Right := ARect.Right - 1;
    FillRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
  end;


  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TInspCustomCombo.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;

begin
  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBkColor));

  WindowBrush := CreateSolidBrush(ColorToRGB(Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

procedure TInspCustomCombo.DrawBorders;
var
  DC: HDC;
begin
  if not FFlat then Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    if (Style <> csSimple) and not DoVisualStyles then
      DrawButtonBorder(DC);
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TInspCustomCombo.CNCommand(var Message: TWMCommand);
var
  r:TRect;
begin
  inherited;
  if Message.NotifyCode in [CBN_CLOSEUP,CBN_DROPDOWN] then
  begin
    r := GetClientRect;
    r.left := r.Right - FButtonWidth;
    InvalidateRect(Handle,@r,FALSE);
  end;
end;


procedure TInspCustomCombo.SetDropWidth(const Value: integer);
begin
  FDropWidth := Value;
  if value > 0 then
    SendMessage(self.Handle,CB_SETDROPPEDWIDTH,FDropWidth,0);
end;






function PosFrom(sub,s:string;n:integer):integer;
begin
  Delete(s,1,n);
  Result := Pos(sub,s);
  if Result > 0 then
    Result := Result + n;
end;

function IncYear(d:tdatetime;n:integer):tdatetime;
var
  da,mo,ye:word;
begin
  DecodeDate(d,ye,mo,da);
  ye := ye + n;
  Result := EncodeDate(ye,mo,da);
end;


function TInspCustomCombo.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;  
end;

{ TInspSpinButton }

constructor TInspSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];

  FUpButton := CreateButton;
  FDownButton := CreateButton;
  UpGlyph := nil;
  DownGlyph := nil;

  FUpButton.ButtonDirection:=bdUp;
  FDownButton.ButtonDirection:=bdDown;

  Width := 15;
  Height := 25;
  FFocusedButton := FUpButton;
end;

destructor TInspSpinButton.Destroy;
begin
  inherited;
end;

function TInspSpinButton.CreateButton: TAdvTimerSpeedButton;
begin
  Result := TAdvTimerSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
end;

procedure TInspSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TInspSpinButton.AdjustSize (var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if FDirection = spVertical then
   begin
    if W < 15 then W := 15;
    FUpButton.SetBounds (0, 0, W, H div 2);
    FDownButton.SetBounds (0, FUpButton.Height , W, H - FUpButton.Height -1);
   end
  else
   begin
    if W < 20 then W := 20;
    FDownButton.SetBounds (0, 0, W div 2, H );
    FUpButton.SetBounds ((W div 2)+1, 0 , W div 2, H );
   end;
end;

procedure TInspSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TInspSpinButton.WMSize(var Message: TWMSize);
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

procedure TInspSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TInspSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TInspSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
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

procedure TInspSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TAdvTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TInspSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TInspSpinButton.SetDirection(const Value:TSpinDirection);
begin
  if value <> FDirection then
  begin
    FDirection := Value;
    RecreateWnd;
    if fdirection = spVertical then
    begin
      Width := 15;
      FUpButton.FButtonDirection := bdUp;
      FDownButton.FButtonDirection := bdDown;
    end
    else
    begin
      Width := 20;
      FUpButton.FButtonDirection:=bdRight;
      FDownButton.FButtonDirection:=bdLeft;
    end;
  end;
end;

procedure TInspSpinButton.SetFocusBtn (Btn: TAdvTimerSpeedButton);
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

procedure TInspSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TInspSpinButton.Loaded;
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

function TInspSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TInspSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
  begin
    FUpButton.Glyph := Value
  end
  else
  begin
    FUpButton.Glyph.LoadFromResourceName(hinstance,'InspSpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TInspSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TInspSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TInspSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TInspSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.LoadFromResourceName(HInstance, 'InspSpinDown');
    FUpButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TInspSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TInspSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

procedure TInspSpinButton.SetIsWinXP(const Value: Boolean);
begin
  FIsWinXP := Value;
  FDownButton.IsWinXP := FIsWinXP;
  FFocusedButton.IsWinXP := FIsWinXP;
end;

{ TInspSpinEdit }

constructor TInspSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TInspSpinButton.Create (Self);
  FButton.Width := 17;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
  FMinFloatValue:=0;
  FMinValue:=0;
  FMaxFloatValue:=100;
  FMaxValue:=100;
end;

destructor TInspSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TInspSpinEdit.Loaded;
begin
  inherited;
  case fSpinType of
  sptDate:self.Text := DateToStr(FDateValue);
  sptTime:self.Text := TimeToStr(FTimeValue);
  end;
  SetSpinType(fSpinType);
end;

procedure TInspSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TInspSpinEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  case Message.CharCode of
  vk_up:
    begin
      UpClick (Self);
      Message.Result := 0;
      Exit;
    end;
  vk_down:
    begin
      DownClick(Self);
      Message.Result := 0;
      Exit;
    end;
  end;
  inherited;
end;

procedure TInspSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case key of
  VK_DELETE: if not FEditorEnabled then Key := 0;
  VK_RETURN:
    if FReturnIsTab then
    begin
      Key := vk_tab;
      PostMessage(self.Handle,wm_keydown,VK_TAB,0);
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TInspSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

function TInspSpinEdit.IsValidChar(var Key: Char): Boolean;
var
  dp:integer;
  s:string;
begin
  Result := (IsNumChar(Key) or (Key = DecimalSeparator) or (Key = ThousandSeparator) or (Key = TimeSeparator) or (Key = DateSeparator) or (Key = '+') or (Key = '-')) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (key = TimeSeparator) and (fSpinType <> sptTime) then Result := False;
  if (key = DateSeparator) and (fSpinType <> sptDate) then Result := False;

  if (FSpinType = sptFloat) and not ( (key = chr(VK_ESCAPE)) or (key = chr(VK_RETURN)) or (key = chr(VK_BACK))) then
  begin
    if key = ThousandSeparator then Key := DecimalSeparator;

    if (key=DecimalSeparator) and (pos(decimalseparator,self.text)>0) then result:=false;
    dp:=pos(decimalseparator,self.text);
    if (FPrecision>0) and (dp>0) and (selstart>=dp) and (sellength=0) then
    begin
      if (length(self.text)>=dp+fPrecision) then result:=false;
    end;
  end;

  if FSpinType = sptTime then
  begin
    s := Text;

    if (key = TimeSeparator) and (Pos(TimeSeparator,s) > 0) then
    begin
      Delete(s,Pos(TimeSeparator,s),1);
      if Pos(TimeSeparator,s) > 0 then
        Result := False;
    end;
  end;

  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TInspSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TInspSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TInspSpinEdit.SetDirection(const value: TSpinDirection);
begin
  if value <> FDirection then
  begin
    FDirection := Value;
    FButton.Direction := Value;
    self.Width := self.Width + 1;
    self.Width := self.Width - 1;
  end;
end;

procedure TInspSpinEdit.SetEditorEnabled(NewValue:boolean);
begin
  FEditorEnabled := NewValue;
end;

procedure TInspSpinEdit.SetEditRect;
var
  Loc: TRect;
  Dist : integer;
begin
  if BorderStyle = bsNone then
    Dist := 1
  else
    Dist := 0;

  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;
  Loc.Right := ClientWidth - FButton.Width - 2 - Dist;
  Loc.Top := Dist;
  Loc.Left := Dist;
  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TInspSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist:integer;

begin
  inherited;
  if BorderStyle=bsNone then Dist:=1 else Dist:=5;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - Dist, 1, FButton.Width, Height - Dist)
    else
      FButton.SetBounds (Width - FButton.Width, 0, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TInspSpinEdit.GetMinHeight: Integer;
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

procedure TInspSpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else
  begin
    case fSpinType of
    sptNormal: Value := Value + FIncrement;
    sptFloat:  FloatValue := FloatValue + FIncrementFloat;
    sptTime: begin
              if selstart>=pos(TimeSeparator,text) then
                begin
                 if selstart>=posfrom(TimeSeparator,text,pos(TimeSeparator,text)) then
                  TimeValue := TimeValue + encodetime(0,0,1,0)
                 else
                  TimeValue := TimeValue + encodetime(0,1,0,0);
                end
              else
               TimeValue := TimeValue + encodetime(1,0,0,0)
             end;
    sptDate: begin
              if selstart>=pos(DateSeparator,text) then
                begin
                 if selstart>=posfrom(DateSeparator,text,pos(DateSeparator,text)) then
                  DateValue := IncYear(DateValue,1)
                 else
                  DateValue := IncMonth(DateValue,1);
                end
              else
               DateValue := DateValue + 1;
             end;
    end;
    if Assigned(FOnSpinUp) then
      FOnSpinUp(Self);
    if Assigned(FOnSpinChange) then
      FOnSpinChange(Self);  
  end;

end;

procedure TInspSpinEdit.DownClick (Sender: TObject);
var
  dt: TDateTime;

begin
  if ReadOnly then MessageBeep(0)
  else
  begin
    case fSpinType of
    sptNormal: Value := Value - FIncrement;
    sptFloat:  FloatValue := FloatValue - FIncrementFloat;
    sptTime:
      begin
        dt := TimeValue;
        dt := dt + 1;
        if SelStart >= Pos(TimeSeparator,text) then
        begin
          if SelStart >= PosFrom(TimeSeparator,Text,Pos(TimeSeparator,Text)) then
            dt := dt - EncodeTime(0,0,1,0)
          else
            dt := dt - EncodeTime(0,1,0,0);
        end
        else
          dt := dt - EncodeTime(1,0,0,0);

        if dt > 1 then
          dt := dt - 1;
        TimeValue := dt;
      end;
    sptDate:
      begin
        if SelStart >= Pos(DateSeparator,text) then
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
    
    if Assigned(FOnSpinDown) then
      FOnSpinDown(Self);
    if Assigned(FOnSpinChange) then
      FOnSpinChange(Self);

  end;
end;

procedure TInspSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TInspSpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TInspSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  case fSpinType of
  sptNormal:if CheckValue (Value) <> Value then SetValue (Value);
  sptFloat:if CheckFloatValue (FloatValue) <> FloatValue then SetFloatValue (FloatValue);
  sptTime:if CheckDateValue (TimeValue) <> TimeValue then SetTimeValue (TimeValue);
  sptDate:if CheckDateValue (DateValue) <> DateValue then SetDateValue (DateValue);
  end;
end;

function TInspSpinEdit.GetValue: LongInt;
begin
  try
    Result := StrToInt (Text);
  except
    Result := FMinValue;
  end;
end;

function TInspSpinEdit.CheckFloatValue (NewValue: Double): Double;
begin
  Result := NewValue;
  if (FMaxFloatValue <> FMinFloatValue) then
  begin
    if NewValue < FMinFloatValue then
      Result := FMinFloatValue
    else if NewValue > FMaxFloatValue then
      Result := FMaxFloatValue;
  end;
end;

function TInspSpinEdit.GetFloatValue: Double;
begin
  try
    Result := StrToFloat (Text);
  except
    Result := FMinValue;
  end;
end;

procedure TInspSpinEdit.SetFloatValue (NewValue: Double);
begin
  if fPrecision=0 then
   Text := FloatToStr (CheckFloatValue (NewValue))
  else
   Text := FloatToStrF (CheckFloatValue (NewValue),ffFixed,15,fPrecision);
end;

procedure TInspSpinEdit.SetValue (NewValue: LongInt);
begin
  Text := IntToStr (CheckValue (NewValue));
  if not FEditorEnabled then SelectAll;
end;

function TInspSpinEdit.CheckValue (NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

function TInspSpinEdit.CheckDateValue (NewValue: tDatetime): tdatetime;
begin
  Result := NewValue;
  if (FMaxDateValue <> FMinDateValue) then
  begin
    if NewValue < FMinDateValue then
      Result := FMinDateValue
    else if NewValue > FMaxDateValue then
      Result := FMaxDateValue;
  end;
end;


procedure TInspSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and (not (csLButtonDown in ControlState) or not FeditorEnabled) then
    SelectAll;
  inherited;
end;

procedure TInspSpinEdit.SetSpinFlat(const value: boolean);
begin
  fButton.fUpButton.flat:=value;
  fButton.fDownButton.flat:=value;
  fspinflat:=value;
end;

procedure TInspSpinEdit.SetSpinTransparent(const value: boolean);
begin
  FButton.FUpButton.Transparent := Value;
  FButton.FDownButton.Transparent := Value;
  FSpinTransparent := Value;
  self.Width:=self.Width + 1;
  self.Width:=self.Width - 1;
end;

procedure TInspSpinEdit.SetPrecision(const Value: integer);
begin
  FPrecision := Value;
  if fSpinType = sptFloat then
    Floatvalue := GetFloatValue;
end;

procedure TInspSpinEdit.SetSpinType(const Value: TInspSpinType);
begin
  if FSpinType <> value then
    FSpinType := Value;

  case FSpinType of
  sptFloat:Floatvalue := GetFloatValue;
  sptNormal:self.Value := GetValue;
  sptTime:self.TimeValue := GetTimeValue;
  sptDate:self.DateValue := GetDateValue;
  end;
end;

function TInspSpinEdit.GetTimeValue: tdatetime;
begin
 try
   Result := StrToTime(Text);
 except
   Result := 0;
 end;
end;

procedure TInspSpinEdit.SetTimeValue(const Value: tdatetime);
var
  ss: Integer;
begin
  fTimeValue := Value;
  if (csLoading in ComponentState) then
    Exit;
  ss := SelStart;
  Text := TimeToStr(value);
  SelStart := ss;
end;

function TInspSpinEdit.GetDateValue: tdatetime;
begin
 if (Text = '0') or (Text = '') then Result := Now
 else
   try
     Result := StrToDate(Text);
   except
     Result := FMinDateValue;
   end;
end;

procedure TInspSpinEdit.SetDateValue(const Value: tdatetime);
var
  ss: Integer;
begin
  FDateValue := Value;
  if (csLoading in ComponentState) then
    Exit;
  FDateValue := CheckDateValue(value);
  ss := SelStart;
  Text := DateToStr(FDateValue);
  SelStart := ss;
end;

procedure TInspSpinEdit.SetIsWinXP(const Value: Boolean);
begin
  FIsWinXP := Value;
  FButton.IsWinXP := Value;
end;

{TAdvTimerSpeedButton}

destructor TAdvTimerSpeedButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TAdvTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
  invalidaterect(parent.handle,nil,true);
end;

procedure TAdvTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
  InvalidateRect(Parent.handle,nil,True);
end;

procedure TAdvTimerSpeedButton.TimerExpired(Sender: TObject);
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

procedure TAdvTimerSpeedButton.Paint;
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
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWNHORZ,DNHZS_NORMAL,@r,nil);
      end;
    bdRight:
      begin
        if FState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UPHORZ,UPHZS_PRESSED,@r,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UPHORZ,UPHZS_NORMAL,@r,nil);
      end;
    bdUp:
      begin
        if FState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,UPS_PRESSED,@r,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,UPS_NORMAL,@r,nil);
      end;

    bdDown:
      begin
        if FState = bsDown then
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DNS_PRESSED,@r,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DNS_NORMAL,@r,nil);
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


function TAdvTimerSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;  
end;

{ TInspEditButton }

constructor TInspEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  FButton := CreateButton;
  Glyph := nil;
  Width := 20;
  Height := 25;
  Font.Name := 'Arial';
  Font.Style := [];
  Font.Size := 10;
  FButton.Caption := '..';
end;

function TInspEditButton.CreateButton: TAdvSpeedButton;
begin
  Result := TAdvSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.Parent := Self;
  Result.Caption := '';
end;

procedure TInspEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TInspEditButton.AdjustSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  FButton.SetBounds (0, 0, W, H);
end;

procedure TInspEditButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TInspEditButton.WMSize(var Message: TWMSize);
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

procedure TInspEditButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
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

procedure TInspEditButton.BtnClick(Sender: TObject);
begin
end;

procedure TInspEditButton.Loaded;
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

function TInspEditButton.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TInspEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TInspEditButton.SetCaption(Value:string);
begin
  FButton.Caption := Value;
end;

function TInspEditButton.GetCaption:string;
begin
  Result := FButton.Caption;
end;

function TInspEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TInspEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

procedure TInspEditButton.SetIsWinXP(const Value: Boolean);
begin
  FIsWinXP := Value;
  FButton.IsWinXP := Value;
end;


{ TSpinEdit }

constructor TInspEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TInspEditButton.Create (Self);
  FButton.Width := 18;
  FButton.Height := 16;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnClick := BtnClick;
  FButton.OnExit := BtnExit;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FRightAlign := False;
  FUnitSize := 0;
end;

destructor TInspEditBtn.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TInspEditBtn.DoEnter;
begin
  inherited;
  SetEditRect;
end;

procedure TInspEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FRightAlign then
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or ES_RIGHT
  else
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TInspEditBtn.CreateWnd;
begin
  inherited CreateWnd;
  Width := Width - 1;
  Width := Width + 1;
  SetEditRect;
  ResizeControl;
end;

procedure TInspEditBtn.SetGlyph(value:TBitmap);
begin
  FButton.Glyph := Value;
end;

function TInspEditBtn.GetGlyph:TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TInspEditBtn.SetCaption(value:string);
begin
  FButton.ButtonCaption := Value;
end;

function TInspEditBtn.GetCaption:string;
begin
  Result := FButton.ButtonCaption;
end;

procedure TInspEditBtn.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}

  Loc.Right := Loc.Left + ClientWidth - FButton.Width - 4;

  if BorderStyle = bsNone then
  begin
    Loc.Top := 2;
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

procedure TInspEditBtn.ResizeControl;
var
  MinHeight: Integer;
  Dist:integer;
begin
  if BorderStyle = bsNone then
    Dist := 2
  else
    Dist := 5;

  MinHeight := GetMinHeight;

  // text edit bug: if size to less than minheight, then edit ctrl does
  // not display the text

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - Dist  + 2, 0, FButton.Width, Height - Dist)
    else
      FButton.SetBounds (Width - FButton.Width + 2, 1, FButton.Width, Height - 3);

    SetEditRect;
  end;
  Invalidate;
end;

procedure TInspEditBtn.WMSize(var Message: TWMSize);
begin
  inherited;
  ResizeControl;
end;

function TInspEditBtn.GetMinHeight: Integer;
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
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

procedure TInspEditBtn.BtnClick (Sender: TObject);
begin
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Sender);
end;

procedure TInspEditBtn.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TInspEditBtn.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TInspEditBtn.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TInspEditBtn.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TInspEditBtn.SetRightAlign(value: boolean);
begin
  if FRightAlign <> Value then
  begin
    FRightAlign := Value;
    ReCreatewnd;
  end;
end;

procedure TInspEditBtn.WMPaint(var Msg: TWMPAINT);
begin
  inherited;
end;

procedure TInspEditBtn.KeyPress(var Key: Char);
begin
  inherited;
end;

procedure TInspEditBtn.WMChar(var Message: TWMChar);
begin
  if not FEditorEnabled then
    Exit;
  Inherited;
end;

procedure TInspEditBtn.BtnExit(Sender: TObject);
begin
  if not EditorEnabled then
    DoExit;
end;

procedure TInspEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F4 then BtnClick(self);
end;

procedure TInspEditButton.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  FButton.Flat := FFlat;
end;

procedure TInspEditBtn.SetButtonWidth(const Value: Integer);
begin
  FButtonWidth := Value;
  FButton.Width := Value;
end;

procedure TInspEditBtn.SetIsWinXP(const Value: Boolean);
begin
  FIsWinXP := Value;
  FButton.IsWinXP := Value;
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

function TAdvSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

procedure TAdvSpeedButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  HTheme: THandle;

begin
  Canvas.Font := Self.Font;

  PaintRect := Rect(0, 0, Width, Height);

  if DoVisualStyles then
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

  if not (FState in [bsDown, bsExclusive]) then
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  if Assigned(Glyph) then
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

  SetBkMode(Canvas.Handle,Windows.TRANSPARENT);
  if FState = bsDown then
    Canvas.TextOut(7,2,Caption)
  else
    Canvas.TextOut(6,1,Caption)
end;

{ TInspDateTimePicker }

procedure TInspDateTimePicker.WMNCPaint (var Message: TMessage);
var
  DC: HDC;
  ARect: TRect;
  WindowBrush: HBrush;
begin
  inherited;
  DC := GetWindowDC(Handle);
  WindowBrush := 0;
  try
    WindowBrush := CreateSolidBrush(ColorToRGB(clwindow));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    FrameRect(DC, ARect, WindowBrush);
    InflateRect(arect,-1,-1);
    FrameRect(DC, ARect, WindowBrush);
  finally
    DeleteObject(WindowBrush);
    ReleaseDC(Handle,DC);
  end;
end;

procedure TInspColorComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  BC : TColor;
  Nm : string;
begin
  {get selected color and text to display}
  case Index of
     0: begin BC := clBlack;   Nm := 'Black';     end;
     1: begin BC := clMaroon;  Nm := 'Maroon';    end;
     2: begin BC := clGreen;   Nm := 'Green';     end;
     3: begin BC := clOlive;   Nm := 'Olive';     end;
     4: begin BC := clNavy;    Nm := 'Navy';      end;
     5: begin BC := clPurple;  Nm := 'Purple';    end;
     6: begin BC := clTeal;    Nm := 'Teal';      end;
     7: begin BC := clGray;    Nm := 'Gray';      end;
     8: begin BC := clSilver;  Nm := 'Silver';    end;
     9: begin BC := clRed;     Nm := 'Red';       end;
    10: begin BC := clLime;    Nm := 'Lime';      end;
    11: begin BC := clYellow;  Nm := 'Yellow';    end;
    12: begin BC := clBlue;    Nm := 'Blue';      end;
    13: begin BC := clFuchsia; Nm := 'Fuchsia';   end;
    14: begin BC := clAqua;    Nm := 'Aqua';      end;
    15: begin BC := clWhite;   Nm := 'White';     end;
    16: begin BC := clBackGround; Nm := 'Background'; end;
    17: begin BC := clActiveCaption; Nm := 'ActiveCaption'; end;
    18: begin BC := clInActiveCaption; Nm := 'InactiveCaption'; end;
    19: begin BC := clMenu; Nm := 'Menu'; end;
    20: begin BC := clWindow; Nm := 'Window'; end;
    21: begin BC := clWindowFrame; Nm := 'WindowFrame'; end;
    22: begin BC := clMenuText; Nm := 'MenuText'; end;
    23: begin BC := clWindowText; Nm := 'WindowText'; end;
    24: begin BC := clCaptionText; Nm := 'CaptionText'; end;
    25: begin BC := clActiveBorder; Nm := 'ActiveBorder'; end;
    26: begin BC := clInactiveBorder; Nm := 'InactiveBorder'; end;
    27: begin BC := clAppWorkSpace; Nm := 'AppWorkspace'; end;
    28: begin BC := clHighLight; Nm := 'Highlight'; end;
    29: begin BC := clHighLightText; Nm := 'HighlightText'; end;
    30: begin BC := clBtnFace; Nm := 'BtnFace'; end;
    31: begin BC := clBtnShadow; Nm := 'BtnShadow'; end;
    32: begin BC := clGrayText; Nm := 'GrayText'; end;
    33: begin BC := clBtnText; Nm := 'BtnText'; end;
    34: begin BC := clInactiveCaptionText; Nm := 'InactiveCaptionText'; end;
    35: begin BC := clBtnHighLight; Nm := 'BtnHighlight'; end;
    36: begin BC := cl3DDkShadow; Nm := '3ddkShadow'; end;
    37: begin BC := cl3DLight; Nm := '3dLight'; end;
    38: begin BC := clInfoText; Nm := 'InfoText'; end;
    39: begin BC := clInfoBk; Nm := 'Infobk'; end;
    40: begin BC := clNone; Nm := 'None'; end;
    41: begin BC := FCustomColor; Nm := 'Custom Color ...'; end;
  else
    begin BC := clWhite;   Nm := '???';       end;
  end;

  if (State * [odSelected, odFocused] <> []) then
  begin
    Canvas.Font.Color := clHighLightText;
    Canvas.Brush.Color := clHighLight;
  end
  else
  begin
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
  end;

  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := BC;
  Canvas.Rectangle(Rect.Left + 1, Rect.Top + 1, Rect.Left + 19, Rect.Bottom - 1);
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clWhite;

  Rect.Left := Rect.Left + 22;
  SetBkMode(Canvas.Handle,TRANSPARENT);
  DrawText(Canvas.Handle,@Nm[1],Length(Nm),Rect,DT_LEFT or DT_VCENTER or DT_SINGLELINE);

end;


function TInspColorComboBox.GetColorValue: TColor;
begin
  Result := clBlack;
  case ItemIndex of
     0: Result := clBlack;
     1: Result := clMaroon;
     2: Result := clGreen;
     3: Result := clOlive;
     4: Result := clNavy;
     5: Result := clPurple;
     6: Result := clTeal;
     7: Result := clGray;
     8: Result := clSilver;
     9: Result := clRed;
    10: Result := clLime;
    11: Result := clYellow;
    12: Result := clBlue;
    13: Result := clFuchsia;
    14: Result := clAqua;
    15: Result := clWhite;
    16: Result := clBackGround;
    17: Result := clActiveCaption;
    18: Result := clInActiveCaption;
    19: Result := clMenu;
    20: Result := clWindow;
    21: Result := clWindowFrame;
    22: Result := clMenuText;
    23: Result := clWindowText;
    24: Result := clCaptionText;
    25: Result := clActiveBorder;
    26: Result := clInactiveBorder;
    27: Result := clAppWorkSpace;
    28: Result := clHighLight;
    29: Result := clHighLightText;
    30: Result := clBtnFace;
    31: Result := clBtnShadow;
    32: Result := clGrayText;
    33: Result := clBtnText;
    34: Result := clInactiveCaptionText;
    35: Result := clBtnHighLight;
    36: Result := cl3DDkShadow;
    37: Result := cl3DLight;
    38: Result := clInfoText;
    39: Result := clInfoBk;
    40: Result := clNone;
    41: Result := FCustomColor;
  end;
end;

procedure TInspColorComboBox.SetColorValue(const Value: TColor);
begin
  case Value of
    clBlack: ItemIndex := 0;
    clMaroon: ItemIndex := 1;
    clGreen: ItemIndex := 2;
    clOlive: ItemIndex := 3;
    clNavy: ItemIndex := 4;
    clPurple: ItemIndex := 5;
    clTeal: ItemIndex := 6;
    clGray: ItemIndex := 7;
    clSilver: ItemIndex := 8;
    clRed: ItemIndex := 9;
    clLime: ItemIndex := 10;
    clYellow: ItemIndex := 11;
    clBlue: ItemIndex := 12;
    clFuchsia: ItemIndex := 13;
    clAqua: ItemIndex := 14;
    clWhite: ItemIndex := 15;
    clBackGround: ItemIndex := 16;
    clActiveCaption: ItemIndex := 17;
    clInActiveCaption: ItemIndex := 18;
    clMenu: ItemIndex := 19;
    clWindow: ItemIndex := 20;
    clWindowFrame: ItemIndex := 21;
    clMenuText: ItemIndex := 22;
    clWindowText: ItemIndex := 23;
    clCaptionText: ItemIndex := 24;
    clActiveBorder: ItemIndex := 25;
    clInactiveBorder: ItemIndex := 26;
    clAppWorkSpace: ItemIndex := 27;
    clHighLight: ItemIndex := 28;
    clHighLightText: ItemIndex := 29;
    clBtnFace: ItemIndex := 30;
    clBtnShadow: ItemIndex := 31;
    clGrayText: ItemIndex := 32;
    clBtnText: ItemIndex := 33;
    clInactiveCaptionText: ItemIndex := 34;
    clBtnHighLight: ItemIndex := 35;
    cl3DDkShadow: ItemIndex := 36;
    cl3DLight: ItemIndex := 37;
    clInfoText: ItemIndex := 38;
    clInfoBk: ItemIndex := 39;
    clNone: ItemIndex := 40;
  else
    begin
      ItemIndex := 41;
      CustomColor := Value;
    end;
  end;
end;


end.

