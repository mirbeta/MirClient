{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxCalc;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Clipbrd,
  cxClasses, cxControls, cxContainer, cxGraphics, cxDataStorage, cxDataUtils,
  cxButtons, cxEdit, cxDropDownEdit, cxEditConsts, cxFormats, cxLookAndFeelPainters,
  cxTextEdit, cxFilterControlUtils, dxFading;

const
  cxMaxCalcPrecision     = cxEditDefaultPrecision;
  cxDefCalcPrecision     = cxMaxCalcPrecision;
  // Size
  cxMinCalcFontSize      = 8;
  cxCalcMinBoldFontSize  = 10;
  cxMinCalcBtnWidth      = 28;
  cxMinCalcBtnHeight     = 22;
  cxMinCalcLargeBtnWidth = Integer(Trunc(1.7*cxMinCalcBtnWidth));
  cxMinCalcXOfs          = 3;
  cxMinCalcYOfs          = 3;
  cxMinCalcWidth         = (cxMinCalcXOfs+cxMinCalcBtnWidth)*6+cxMinCalcXOfs*3+3;
  cxMinCalcHeight        = (cxMinCalcYOfs+cxMinCalcBtnHeight)*5+cxMinCalcYOfs+3;

type
  TcxCalcState = (csFirst, csValid, csError);

const
  BtnCaptions : array [cbBack..cbEqual] of string = ('Back', 'CE', 'C',
    'MC', 'MR', 'MS', 'M+',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '+/-', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=');

type
  TcxCustomCalculator = class;
  TcxCalcEditButtonFadingHelper = class;

  { TcxCalcEditButtonViewInfo }

  TcxCalcEditButtonViewInfo = class(TObject)
  private
    FDown: Boolean;
    FOwner: TcxCustomCalculator;
    FState: TcxButtonState;
    FGrayed: Boolean;
    function GetIsDefault: Boolean;
    procedure SetDown(AValue: Boolean);
    procedure SetState(const Value: TcxButtonState);
    procedure SetGrayed(const Value: Boolean);
  protected
    Bounds: TRect;
    FadingHelper: TcxCalcEditButtonFadingHelper;
    Kind: TcxCalcButtonKind;
    Text: string;
    function CalculateState: TcxButtonState;
  public
    constructor Create(AOwner: TcxCustomCalculator);
    destructor Destroy; override;
    procedure Invalidate;
    procedure UpdateState;
    //
    property Down: Boolean read FDown write SetDown;
    property Grayed: Boolean read FGrayed write SetGrayed;
    property IsDefault: Boolean read GetIsDefault;
    property Owner: TcxCustomCalculator read FOwner;
    property State: TcxButtonState read FState write SetState;
  end;

  { TcxCalcEditButtonFadingHelper }

  TcxCalcEditButtonFadingHelper = class(TdxFadingObjectHelper)
  private
    FViewInfo: TcxCalcEditButtonViewInfo;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    function CanFade: Boolean; override;
    procedure DrawFadeImage; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
  public
    constructor Create(AViewInfo: TcxCalcEditButtonViewInfo); virtual;
    //
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ViewInfo: TcxCalcEditButtonViewInfo read FViewInfo;
  end;

  TcxCalcEditButtonsViewInfo = array [TcxCalcButtonKind] of TcxCalcEditButtonViewInfo;

  { TcxCustomCalculator }

  TcxCalcButtonClick = procedure(Sender: TObject; var ButtonKind : TcxCalcButtonKind) of object;
  TcxCalcGetEditValue = procedure(Sender: TObject; var Value : String) of object;
  TcxCalcSetEditValue = procedure(Sender: TObject; const Value : String) of object;

  TcxCustomCalculator = class(TcxControl)
  private
    {calc style}
    FAutoFontSize : Boolean;
    FBeepOnError: Boolean;
    FBorderStyle : TBorderStyle;
    FFocusRectVisible : Boolean;
    {calc size}
    FCalcFontSize      : Integer;
    FCalcBtnWidth      : Integer;
    FCalcBtnHeight     : Integer;
    FCalcLargeBtnWidth : Integer;
    FCalcXOffset       : Integer;
    FCalcYOffset       : Integer;
    FCalcWidth         : Integer;
    FCalcHeight        : Integer;
    {math}
    FMemory : Extended;
    FOperator: TcxCalcButtonKind;
    FOperand: Extended;
    FPrecision: Byte;
    FStatus: TcxCalcState;
    {control}
    FActiveButton: TcxCalcButtonKind;
    FButtons: TcxCalcEditButtonsViewInfo;
    FDownButton: TcxCalcButtonKind;
    FPressedButton: TcxCalcButtonKind;
    FTracking: Boolean;
    // events
    FOnDisplayChange: TNotifyEvent;
    FOnButtonClick: TcxCalcButtonClick;
    FOnResult: TNotifyEvent;
    FOnHidePopup: TcxEditClosePopupEvent;

    function GetDisplay: Extended;
    function GetMemory: Extended;
    procedure SetDisplay(Value: Extended);

    procedure SetActiveButton(AValue: TcxCalcButtonKind);
    procedure SetAutoFontSize(Value : Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetFocusRectVisible(Value : Boolean);

    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure DoButtonDown(ButtonKind : TcxCalcButtonKind);
    procedure DoButtonUp(ButtonKind : TcxCalcButtonKind);
    procedure Error;
    procedure CheckFirst;
    procedure Clear;
    procedure CalcSize(AWidth, AHeight : Integer);
    procedure UpdateMemoryButtons;
    procedure InvalidateMemoryButtons;
    procedure ResetOperands;
  protected
    IsPopupControl : Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter; virtual;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FontChanged; override;
    procedure FocusChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure SetEnabled( Value: Boolean); override;
    procedure CreateLayout;
    procedure ButtonClick(ButtonKind : TcxCalcButtonKind);
    procedure UpdateButtonsState;

    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawButton(ACanvas: TcxCanvas; AButtonInfo: TcxCalcEditButtonViewInfo); virtual;
    procedure DrawButtons(ACanvas: TcxCanvas); virtual;
    // for link with EditControl
    function GetEditorValue: String; virtual;
    procedure SetEditorValue(const Value: String); virtual;
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); virtual;
    procedure LockChanges(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True); virtual;

    property Color default clBtnFace;
    property ParentColor default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;

    property ActiveButton: TcxCalcButtonKind read FActiveButton write SetActiveButton;
    property AutoFontSize: Boolean read FAutoFontSize write SetAutoFontSize default True;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property ShowFocusRect: Boolean read FFocusRectVisible write SetFocusRectVisible default True;

    property Precision: Byte read FPrecision write FPrecision default cxDefCalcPrecision;
    property EditorValue : string read GetEditorValue write SetEditorValue;

    property OnHidePopup: TcxEditClosePopupEvent read FOnHidePopup write FOnHidePopup;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
    property OnButtonClick: TcxCalcButtonClick read FOnButtonClick write FOnButtonClick;
    property OnResult: TNotifyEvent read FOnResult write FOnResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetButtonKindAt(X, Y : Integer) : TcxCalcButtonKind;
    function GetButtonKindChar(Ch : Char) : TcxCalcButtonKind;
    function GetButtonKindKey(Key: Word; Shift: TShiftState) : TcxCalcButtonKind;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;

    property Memory: Extended read GetMemory;
    property Value: Extended read GetDisplay write SetDisplay;
  published
    property TabStop default True;
  end;

  { TcxPopupCalculator }

  TcxCustomCalcEdit = class;

  TcxPopupCalculator = class(TcxCustomCalculator)
  private
    FEdit: TcxCustomCalcEdit;
  protected
    function GetEditorValue: string; override;
    function GetPainter: TcxCustomLookAndFeelPainter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetEditorValue(const Value: string); override;
    procedure LockChanges(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True); override;
    property Edit: TcxCustomCalcEdit read FEdit write FEdit;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; virtual;
  end;

  { TcxCalcEditPropertiesValues }

  TcxCalcEditPropertiesValues = class(TcxTextEditPropertiesValues)
  private
    FPrecision: Boolean;
    procedure SetPrecision(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property Precision: Boolean read FPrecision write SetPrecision stored False;
  end;

  { TcxCustomCalcEditProperties }

  TcxCustomCalcEditProperties = class(TcxCustomPopupEditProperties)
  private
    FBeepOnError: Boolean;
    FPrecision: Byte;
    FQuickClose: Boolean;
    FScientificFormat: Boolean;
    FUseThousandSeparator: Boolean;
    function GetAssignedValues: TcxCalcEditPropertiesValues;
    function GetPrecision: Byte;
    function IsPrecisionStored: Boolean;
    procedure SetAssignedValues(Value: TcxCalcEditPropertiesValues);
    procedure SetBeepOnError(Value: Boolean);
    procedure SetPrecision(Value: Byte);
    procedure SetQuickClose(Value: Boolean);
    procedure SetScientificFormat(Value: Boolean);
    procedure SetUseThousandSeparator(Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetAlwaysPostEditValue: Boolean; override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    function HasDigitGrouping(AIsDisplayValueSynchronizing: Boolean): Boolean; override;
    function PopupWindowAcceptsAnySize: Boolean; override;

    property AssignedValues: TcxCalcEditPropertiesValues read GetAssignedValues
      write SetAssignedValues;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function IsDisplayValueValid(var DisplayValue: TcxEditValue;
      AEditFocused: Boolean): Boolean; override;
    function IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue;
      var ADisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    // !!!
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError
      default True;
    property ImmediateDropDownWhenKeyPressed default False;
    property Precision: Byte read GetPrecision write SetPrecision
      stored IsPrecisionStored;
    property QuickClose: Boolean read FQuickClose write SetQuickClose
      default False;
    property ScientificFormat: Boolean read FScientificFormat
      write SetScientificFormat default False;
    property UseThousandSeparator: Boolean read FUseThousandSeparator
      write SetUseThousandSeparator default False;
  end;

  { TcxCalcEditProperties }

  TcxCalcEditProperties = class(TcxCustomCalcEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property ButtonGlyph;
    property ClearKey;
    property DisplayFormat;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property Nullstring;
    property Precision;
    property ReadOnly;
    property QuickClose;
    property ScientificFormat;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property UseThousandSeparator;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;
  end;

  { TcxCustomCalcEdit }

  TcxCustomCalcEdit = class(TcxCustomPopupEdit)
  private
    FCalculator: TcxPopupCalculator;
    function GetProperties: TcxCustomCalcEditProperties;
    function GetActiveProperties: TcxCustomCalcEditProperties;
    function GetValue: Double;
    procedure SetProperties(Value: TcxCustomCalcEditProperties);
    procedure SetValue(const Value: Double);
  protected
    // IcxFormatControllerListener
    procedure FormatChanged; override;

    function CanDropDown: Boolean; override;
    procedure CreatePopupWindow; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoInitPopup; override;
    procedure Initialize; override;
    procedure InitializePopupWindow; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PopupWindowShowed(Sender: TObject); override;
    procedure PropertiesChanged(Sender: TObject); override;
    function InternalPrepareEditValue(const ADisplayValue: string; out EditValue: TcxEditValue): Boolean;
    property Calculator: TcxPopupCalculator read FCalculator;
  public
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomCalcEditProperties read GetActiveProperties;
    property Properties: TcxCustomCalcEditProperties read GetProperties write SetProperties;
    property Value: Double read GetValue write SetValue stored False;
  end;

  { TcxCalcEdit }

  TcxCalcEdit = class(TcxCustomCalcEdit)
  private
    function GetActiveProperties: TcxCalcEditProperties;
    function GetProperties: TcxCalcEditProperties;
    procedure SetProperties(Value: TcxCalcEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCalcEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCalcEditProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
    property TextHint;
    property Value;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
    property BiDiMode;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterCalcEditHelper }

  TcxFilterCalcEditHelper = class(TcxFilterDropDownEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

implementation

uses
  Variants, Math, cxLookAndFeels, cxVariants, dxThemeManager, dxUxTheme, dxCore, cxGeometry;

const
  ResultButtons    = [cbEqual, cbPercent];
  RepeatButtons    = [cbBack];
  OperationButtons = [cbAdd, cbSub, cbMul, cbDiv];
  BorderWidth = 4;

{ TcxCustomCalculator }

constructor TcxCustomCalculator.Create(AOwner: TComponent);
var
  AKind: TcxCalcButtonKind;
begin
  inherited Create(AOwner);
  for AKind := Low(AKind) to High(AKind) do
    FButtons[AKind] := TcxCalcEditButtonViewInfo.Create(Self);
  {init size variables}
  FCalcFontSize      := cxMinCalcFontSize;
  FCalcBtnWidth      := cxMinCalcBtnWidth;
  FCalcBtnHeight     := cxMinCalcBtnHeight;
  FCalcLargeBtnWidth := cxMinCalcLargeBtnWidth;
  FCalcXOffset       := cxMinCalcXOfs;
  FCalcYOffset       := cxMinCalcYOfs;
  FCalcWidth         := cxMinCalcWidth;
  FCalcHeight        := cxMinCalcHeight;
  {default size}
  Width := FCalcWidth;
  Height := FCalcHeight;
  {style}
  ControlStyle := [csCaptureMouse, csOpaque];
  Color := clBtnFace;
  ParentColor := False;
  TabStop := True;
  FAutoFontSize := True;
  FBorderStyle := bsNone;
  FBeepOnError := True;
  FDownButton := cbNone;
  FActiveButton := cbNone;
  FPressedButton := cbNone;
  FFocusRectVisible := True;
  FOperator := cbEqual;
  FPrecision := cxDefCalcPrecision;
  Keys := [kAll, kArrows, kChars, kTab];
  CreateLayout;
end;

destructor TcxCustomCalculator.Destroy;
var
  AKind: TcxCalcButtonKind;
begin
  for AKind := Low(AKind) to High(AKind) do
    FreeAndNil(FButtons[AKind]);
  inherited Destroy;
end;

function TcxCustomCalculator.GetButtonKindAt(X, Y : Integer) : TcxCalcButtonKind;
var
  I: TcxCalcButtonKind;
begin
  Result := cbNone;
  for I := cbBack to cbEqual do
    if PtInRect(FButtons[I].Bounds, Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomCalculator.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
end;

procedure TcxCustomCalculator.Paint;
begin
  if HandleAllocated then
  begin
    DrawBackground(Canvas);
    DrawButtons(Canvas);
  end;
end;

procedure TcxCustomCalculator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP or WS_CLIPCHILDREN;
    WindowClass.Style := WindowClass.Style and not CS_DBLCLKS;
    if IsPopupControl then
      Style := Style and not WS_BORDER
    else
      if FBorderStyle = bsSingle then
        if NewStyleControls and Ctl3D then
        begin
          Style := Style and not WS_BORDER;
          ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        end
        else
          Style := Style or WS_BORDER;
  end;
end;

procedure TcxCustomCalculator.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var ButtonKind : TcxCalcButtonKind;
begin
  if not (csDesigning in ComponentState) and
      (CanFocus or (GetParentForm(Self) = nil)) and not IsPopupControl then
    SetFocus;

  ButtonKind := GetButtonKindAt(X, Y);
  if (Button = mbLeft) and (ButtonKind <> cbNone) and not FButtons[ButtonKind].Grayed then
  begin
    MouseCapture := True;
    FTracking := True;
    FDownButton := ButtonKind;
    TrackButton(X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomCalculator.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
    TrackButton(X, Y)
  else
    if GetPainter.IsButtonHotTrack and Enabled and not Dragging then
      ActiveButton := GetButtonKindAt(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TcxCustomCalculator.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := (FDownButton <> cbNone) and FButtons[FDownButton].Down;
  StopTracking;
  if (Button = mbLeft) and WasPressed then
    ButtonClick(FDownButton);
  FDownButton := cbNone;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomCalculator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewButton, OldButton : TcxCalcButtonKind;
begin
  inherited KeyDown(Key, Shift);
  OldButton := FPressedButton;
  NewButton := GetButtonKindKey(Key, Shift);
  if (NewButton <> cbNone) and (OldButton <> NewButton) then
  begin
    DoButtonUp(OldButton);
    FPressedButton := NewButton;
    DoButtonDown(FPressedButton);
  end;
end;

procedure TcxCustomCalculator.KeyPress(var Key: Char);
var
  NewButton, OldButton : TcxCalcButtonKind;
begin
  inherited KeyPress(Key);
  if (Key = ^V) then
    PasteFromClipboard
  else
    if (Key = ^C) then CopyToClipboard;

  OldButton := FPressedButton;
  NewButton := GetButtonKindChar(Key);
  if (NewButton <> cbNone) and (OldButton <> NewButton) then
  begin
    DoButtonUp(OldButton);
    FPressedButton := NewButton;
    DoButtonDown(FPressedButton);
  end;
  if FPressedButton in RepeatButtons {cbBack} then
    ButtonClick(FPressedButton);
end;

procedure TcxCustomCalculator.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  DoButtonUp(FPressedButton);
end;

procedure TcxCustomCalculator.Resize;
begin
  CalcSize(ClientWidth, ClientHeight);
  ClientWidth := FCalcWidth;
  ClientHeight := FCalcHeight;
  inherited;
end;

procedure TcxCustomCalculator.DoButtonDown(ButtonKind : TcxCalcButtonKind);
begin
  if ButtonKind <> cbNone then
  begin
    FButtons[ButtonKind].Down := True;
    Update;
    if not (ButtonKind in RepeatButtons) {cbBack} then
      ButtonClick(ButtonKind);
  end;
end;

procedure TcxCustomCalculator.DoButtonUp(ButtonKind : TcxCalcButtonKind);
begin
  if ButtonKind <> cbNone then
  begin
    FPressedButton := cbNone;
    FButtons[ButtonKind].Down := False;
    Update;
  end;
end;

procedure TcxCustomCalculator.DrawBackground(ACanvas: TcxCanvas);
begin
  ACanvas.Brush.Color := Color;
  if IsPopupControl then
    GetPainter.DrawWindowContent(ACanvas, ClientRect)
  else
    GetPainter.DrawWindowContent(ACanvas, BoundsRect);
end;

procedure TcxCustomCalculator.DrawButton(ACanvas: TcxCanvas; AButtonInfo: TcxCalcEditButtonViewInfo);
var
  ATextRect: TRect;
begin
  if ACanvas.RectVisible(AButtonInfo.Bounds) then
  begin
    ACanvas.SaveState;
    try
      if not AButtonInfo.FadingHelper.DrawImage(ACanvas.Handle, AButtonInfo.Bounds) then
        GetPainter.DrawScaledButton(ACanvas, AButtonInfo.Bounds, '', AButtonInfo.State, ScaleFactor);
      ACanvas.Font.Color := GetPainter.CalcEditButtonTextColor(AButtonInfo.Kind);
      ACanvas.Brush.Style := bsClear;

      ATextRect := AButtonInfo.Bounds;
      if AButtonInfo.State = cxbsPressed then
        OffsetRect(ATextRect, GetPainter.ScaledButtonTextShift(ScaleFactor), GetPainter.ScaledButtonTextShift(ScaleFactor));
      ACanvas.DrawText(AButtonInfo.Text, ATextRect,
        cxAlignHCenter or cxAlignVCenter or cxSingleLine or cxShowPrefix,
        AButtonInfo.State <> cxbsDisabled);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

procedure TcxCustomCalculator.DrawButtons(ACanvas: TcxCanvas);
var
  ABitmap: TcxBitmap;
  AButton: TcxCalcEditButtonViewInfo;
  I: TcxCalcButtonKind;
begin
  ACanvas.Font := Font;
  if AutoFontSize then
  begin
    ACanvas.Font.Size := FCalcFontSize;
    if Canvas.Font.Size >= cxCalcMinBoldFontSize then
      ACanvas.Font.Style := [fsBold]
    else
      ACanvas.Font.Style := [];
  end;
  ACanvas.Brush.Color := Color;
  for I := cbBack to cbEqual do
  begin
    AButton := FButtons[I];
    if FFocusRectVisible and AButton.IsDefault then
    begin
     ABitmap := TcxBitmap.CreateSize(AButton.Bounds);
      try
        ABitmap.Canvas.Font := Canvas.Font;
        cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, AButton.Bounds.TopLeft, SRCCOPY);
        ABitmap.cxCanvas.WindowOrg := AButton.Bounds.TopLeft;
        DrawButton(ABitmap.cxCanvas, AButton);
        ABitmap.cxCanvas.DrawFocusRect(GetPainter.ScaledButtonFocusRect(ABitmap.cxCanvas, AButton.Bounds, ScaleFactor));
        ABitmap.cxCanvas.WindowOrg := cxNullPoint;
        ACanvas.Draw(AButton.Bounds.Left, AButton.Bounds.Top, ABitmap);
      finally
        ABitmap.Free;
      end;
    end
    else
      DrawButton(ACanvas, AButton);
  end;
end;

function TcxCustomCalculator.GetEditorValue: String;
begin
  Result := '';
end;

procedure TcxCustomCalculator.SetEditorValue(const Value: String);
begin
end;

procedure TcxCustomCalculator.CreateLayout;
var
  I: TcxCalcButtonKind;
  X: Integer;
begin
  for I := cbBack to cbEqual do
  begin
    FButtons[I].Kind := I;
    FButtons[I].Text := BtnCaptions[I];
    FButtons[I].Bounds := cxEmptyRect;
    FButtons[I].Down := False;
    FButtons[I].Grayed := False;
    if I = cbDecimal then
      FButtons[I].Text := dxFormatSettings.DecimalSeparator
    else
      FButtons[I].Text := BtnCaptions[i];
  end;
  {coord buttons}
  FButtons[cbMC].Bounds := Rect(FCalcXOffset,
                                 (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                 FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*2);
  FButtons[cbMR].Bounds := Rect(FCalcXOffset,
                                 (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                 FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*3);
  FButtons[cbMS].Bounds := Rect(FCalcXOffset,
                                 (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                 FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*4);
  FButtons[cbMP].Bounds := Rect(FCalcXOffset,
                                 (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                 FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  X := FCalcXOffset+FCalcBtnWidth + FCalcXOffset + 4;
  {7, 8, 9, /, sqrt}
  FButtons[cbNum7].Bounds := Rect(X+FCalcXOffset,
                                   (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                   X+FCalcXOffset+FCalcBtnWidth,
                                   (FCalcYOffset+FCalcBtnHeight)*2);
  FButtons[cbNum8].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth),
                                   (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*2,
                                   (FCalcYOffset+FCalcBtnHeight)*2);
  FButtons[cbNum9].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*2,
                                   (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*3,
                                   (FCalcYOffset+FCalcBtnHeight)*2);
  FButtons[cbDiv].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*3,
                                   (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*4,
                                   (FCalcYOffset+FCalcBtnHeight)*2);
  FButtons[cbSqrt].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*4,
                                   (FCalcYOffset+FCalcBtnHeight)+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*5,
                                   (FCalcYOffset+FCalcBtnHeight)*2);

  {4, 5, 6, *, %}
  FButtons[cbNum4].Bounds := Rect(X+FCalcXOffset,
                                  (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                   X+FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*3);
  FButtons[cbNum5].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth),
                                  (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*2,
                                 (FCalcYOffset+FCalcBtnHeight)*3);
  FButtons[cbNum6].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*2,
                                  (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*3,
                                 (FCalcYOffset+FCalcBtnHeight)*3);
  FButtons[cbMul].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*3,
                                  (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*4,
                                 (FCalcYOffset+FCalcBtnHeight)*3);
  FButtons[cbPercent].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*4,
                                  (FCalcYOffset+FCalcBtnHeight)*2+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*5,
                                 (FCalcYOffset+FCalcBtnHeight)*3);

  {1, 2, 3, -, 1/x}
  FButtons[cbNum1].Bounds := Rect(X+FCalcXOffset,
                                  (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                   X+FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*4);
  FButtons[cbNum2].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth),
                                  (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*2,
                                 (FCalcYOffset+FCalcBtnHeight)*4);
  FButtons[cbNum3].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*2,
                                  (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*3,
                                 (FCalcYOffset+FCalcBtnHeight)*4);
  FButtons[cbSub].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*3,
                                  (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*4,
                                 (FCalcYOffset+FCalcBtnHeight)*4);
  FButtons[cbRev].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*4,
                                  (FCalcYOffset+FCalcBtnHeight)*3+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*5,
                                 (FCalcYOffset+FCalcBtnHeight)*4);

  {0, +/-, ., +, =}
  FButtons[cbNum0].Bounds := Rect(X+FCalcXOffset,
                                  (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                   X+FCalcXOffset+FCalcBtnWidth,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  FButtons[cbSign].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth),
                                  (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*2,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  FButtons[cbDecimal].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*2,
                                  (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*3,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  FButtons[cbAdd].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*3,
                                  (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*4,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  FButtons[cbEqual].Bounds := Rect(X+FCalcXOffset+(FCalcXOffset+FCalcBtnWidth)*4,
                                  (FCalcYOffset+FCalcBtnHeight)*4+FCalcYOffset,
                                   X+(FCalcXOffset+FCalcBtnWidth)*5,
                                 (FCalcYOffset+FCalcBtnHeight)*5);
  {C}
  FButtons[cbClear].Bounds := FButtons[cbEqual].Bounds;
  FButtons[cbClear].Bounds.Left := FButtons[cbClear].Bounds.Right - FCalcLargeBtnWidth;
  FButtons[cbClear].Bounds.Top := FCalcYOffset;
  FButtons[cbClear].Bounds.Bottom := FCalcYOffset + FCalcBtnHeight;
  {CE}
  FButtons[cbCancel].Bounds := FButtons[cbClear].Bounds;
  FButtons[cbCancel].Bounds.Right := FButtons[cbClear].Bounds.Left - FCalcYOffset;
  FButtons[cbCancel].Bounds.Left := FButtons[cbCancel].Bounds.Right - FCalcLargeBtnWidth;
  {Back}
  FButtons[cbBack].Bounds := FButtons[cbCancel].Bounds;
  FButtons[cbBack].Bounds.Right := FButtons[cbBack].Bounds.Left - FCalcYOffset;
  FButtons[cbBack].Bounds.Left := FButtons[cbBack].Bounds.Right - FCalcLargeBtnWidth;
  // ResetOperands;
  ResetOperands;
  // Update Memory display
  UpdateMemoryButtons;
end;

procedure TcxCustomCalculator.ResetOperands;
begin
  FOperator := cbEqual;
  FStatus := csFirst;
  FMemory := 0.0;
end;

procedure TcxCustomCalculator.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TcxCustomCalculator.SetFocusRectVisible(Value : Boolean);
begin
  if FFocusRectVisible <> Value then
  begin
    FFocusRectVisible := Value;
    Invalidate;
  end;
end;

procedure TcxCustomCalculator.CalcSize(AWidth, AHeight : Integer);
var
  h, NearHeight, d, dMin : Integer;

  function CalcHeight(ABtnHeight:Integer):Integer;
  var FYOfs : Integer;
  begin
    FYOfs := MulDiv(ABtnHeight, cxMinCalcYOfs, cxMinCalcBtnHeight);
    Result := (FYOfs + ABtnHeight) * 5 + FYOfs;
  end;

begin
  if AutoFontSize then
  begin
    h := MulDiv(AWidth, cxMinCalcHeight, cxMinCalcWidth);
    if AHeight > h then AHeight := h;
    {Calculate nearest FCalcHeight }
    h := cxMinCalcBtnHeight;
    NearHeight := h;
    dMin := AHeight;
    while True do
    begin
      d := abs(CalcHeight(h) - AHeight);
      if d < dMin then
      begin
        dMin := d;
        NearHeight := h;
      end
      else
        Break;
      inc(h);
    end;
  end
  else
    NearHeight := Canvas.FontHeight(Font) * 2;
  FCalcBtnHeight     := NearHeight;
  dxAdjustToTouchableSize(FCalcBtnHeight, ScaleFactor);
  FCalcBtnWidth      := MulDiv(FCalcBtnHeight, cxMinCalcBtnWidth, cxMinCalcBtnHeight);
  FCalcYOffset          := MulDiv(FCalcBtnHeight, cxMinCalcYOfs, cxMinCalcBtnHeight);
  FCalcXOffset          := FCalcYOffset;
  FCalcLargeBtnWidth := MulDiv(FCalcBtnWidth, 17, 10);
  FCalcFontSize      := MulDiv(FCalcBtnHeight, cxMinCalcFontSize, cxMinCalcBtnHeight);
  FCalcHeight        := (FCalcYOffset+FCalcBtnHeight)*5+FCalcYOffset;
  FCalcWidth         := (FCalcXOffset+FCalcBtnWidth)*6+FCalcXOffset*2+4;
  // reCalc rect buttons
  CreateLayout;
end;

procedure TcxCustomCalculator.FontChanged;
begin
  if not (csLoading in ComponentState) then ParentFont := False;
  inherited FontChanged;
end;

procedure TcxCustomCalculator.FocusChanged;
begin
  inherited FocusChanged;
  FButtons[cbEqual].UpdateState;
  FButtons[cbEqual].Invalidate;
end;

procedure TcxCustomCalculator.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  UpdateButtonsState;
end;

procedure TcxCustomCalculator.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
    if FDownButton <> cbNone then
      FButtons[FDownButton].Down := False;
  end;
end;

procedure TcxCustomCalculator.TrackButton(X,Y: Integer);
var
  ANeedRepaint : Boolean;
begin
  if FDownButton <> cbNone then
  begin
    ANeedRepaint := (GetButtonKindAt(X, Y) = FDownButton) <> FButtons[FDownButton].Down;
    FButtons[FDownButton].Down := (GetButtonKindAt(X, Y) = FDownButton);
    if ANeedRepaint then
      FButtons[FDownButton].Invalidate;
  end;
end;

procedure TcxCustomCalculator.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if GetPainter.IsButtonHotTrack and Enabled and not Dragging then
    ActiveButton := cbNone;
end;

function TcxCustomCalculator.GetButtonKindChar(Ch : Char) : TcxCalcButtonKind;
begin
  case Ch of
    '0'..'9' : Result := TcxCalcButtonKind(Ord(cbNum0)+Ord(Ch)-Ord('0'));
    '+' : Result := cbAdd;
    '-' : Result := cbSub;
    '*' : Result := cbMul;
    '/' : Result := cbDiv;
    '%' : Result := cbPercent;
    '=' : Result := cbEqual;
    #8 : Result := cbBack;
    '@' : Result := cbSqrt;
    '.', ',': Result := cbDecimal;
  else
    Result := cbNone;
  end;
end;

function TcxCustomCalculator.GetButtonKindKey(Key: Word; Shift: TShiftState) : TcxCalcButtonKind;
begin
  Result := cbNone;
  case Key of
    VK_RETURN : Result := cbEqual;
    VK_ESCAPE : Result := cbClear;
    VK_F9 : Result := cbSign;
    VK_DELETE : Result := cbCancel;
    Ord('C'){VK_C} : if not (ssCtrl in Shift) then Result := cbClear;
    Ord('P'){VK_P} : if ssCtrl in Shift then Result := cbMP;
    Ord('L'){VK_L} : if ssCtrl in Shift then Result := cbMC;
    Ord('R'){VK_R} : if ssCtrl in Shift then Result := cbMR
                     else Result := cbRev;
    Ord('M'){VK_M} : if ssCtrl in Shift then Result := cbMS;
  end;
end;

procedure TcxCustomCalculator.CopyToClipboard;
begin
  Clipboard.AsText := GetEditorValue;
end;

procedure TcxCustomCalculator.PasteFromClipboard;
var
  S, S1 : String;
  i : Integer;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    try
      S := Clipboard.AsText;
      S1 := '';
      repeat
        i := Pos(dxFormatSettings.CurrencyString, S);
        if i > 0 then
        begin
          S1 := S1 + Copy(S, 1, i - 1);
          S := Copy(S, i + Length(dxFormatSettings.CurrencyString), MaxInt);
        end
        else
          S1 := S1 + S;
      until i <= 0;
      RemoveThousandSeparator(S1);
      SetDisplay(StrToFloat(Trim(S1)));
      FStatus := csValid;
    except
      SetDisplay(0.0);
    end;
end;

procedure TcxCustomCalculator.SetActiveButton(AValue: TcxCalcButtonKind);
begin
  if AValue <> ActiveButton then
  begin
    FActiveButton := AValue;
    UpdateButtonsState;
  end;
end;

procedure TcxCustomCalculator.SetAutoFontSize(Value : Boolean);
begin
  if AutoFontSize <> Value then
  begin
    FAutoFontSize := Value;
    Font.OnChange(nil);
  end;
end;

// math routines
procedure TcxCustomCalculator.Error;
begin
  FStatus := csError;
  SetEditorValue(cxGetResourceString(@scxSCalcError));
  if FBeepOnError then MessageBeep(0);
//  if Assigned(FOnError) then FOnError(Self);
end;

procedure TcxCustomCalculator.CheckFirst;
begin
  if FStatus = csFirst then
  begin
    FStatus := csValid;
    SetEditorValue('0');
  end;
end;

procedure TcxCustomCalculator.Clear;
begin
  FStatus := csFirst;
  SetDisplay(0.0);
  FOperator := cbEqual;
end;

procedure TcxCustomCalculator.ButtonClick(ButtonKind : TcxCalcButtonKind);
var
  AValue : Extended;
  AOldEditorValue: string;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self, ButtonKind);
  if (FStatus = csError) and not (ButtonKind in [cbClear, cbCancel]) then
  begin
    Error;
    Exit;
  end;
  AOldEditorValue := EditorValue;
  LockChanges(True);
  try
    case ButtonKind of
      cbDecimal:
        begin
          CheckFirst;
          if Pos(dxFormatSettings.DecimalSeparator, EditorValue) = 0 then
            SetEditorValue(EditorValue + dxFormatSettings.DecimalSeparator);
        end;
      cbRev:
        if FStatus in [csValid, csFirst] then
        begin
          FStatus := csFirst;
          if FOperator in OperationButtons then
            FStatus := csValid;
          if GetDisplay = 0 then Error else SetDisplay(1.0 / GetDisplay);
        end;
      cbSqrt:
        if FStatus in [csValid, csFirst] then
        begin
          FStatus := csFirst;
          if FOperator in OperationButtons then
            FStatus := csValid;
          if GetDisplay < 0 then Error else SetDisplay(Sqrt(GetDisplay));
        end;
      cbNum0..cbNum9:
        begin
          CheckFirst;
          if EditorValue = '0' then SetEditorValue('');
          if Length(EditorValue) < Max(2, FPrecision) + Ord(Boolean(Pos('-', EditorValue))) then
            SetEditorValue(EditorValue + Char(Ord('0')+Byte(ButtonKind)-Byte(cbNum0)))
          else
            if FBeepOnError then MessageBeep(0);
        end;
      cbBack:
        begin
          CheckFirst;
          if (Length(EditorValue) = 1) or ((Length(EditorValue) = 2) and (EditorValue[1] = '-')) then
            SetEditorValue('0')
          else
            SetEditorValue(Copy(EditorValue, 1, Length(EditorValue) - 1));
        end;
      cbSign: SetDisplay(-GetDisplay);
      cbAdd, cbSub, cbMul, cbDiv, cbEqual, cbPercent :
        begin
          if FStatus = csValid then
          begin
            FStatus := csFirst;
            AValue := GetDisplay;
            if ButtonKind = cbPercent then
              case FOperator of
                cbAdd, cbSub : AValue := FOperand * AValue / 100.0;
                cbMul, cbDiv : AValue := AValue / 100.0;
              end;
            case FOperator of
              cbAdd : SetDisplay(FOperand + AValue);
              cbSub : SetDisplay(FOperand - AValue);
              cbMul : SetDisplay(FOperand * AValue);
              cbDiv : if AValue = 0 then Error else SetDisplay(FOperand / AValue);
            end;
          end;
          FOperator := ButtonKind;
          FOperand := GetDisplay;
          if (ButtonKind in ResultButtons) and Assigned(FOnResult) then FOnResult(Self);
        end;
      cbClear, cbCancel: Clear;
      cbMP:
        if FStatus in [csValid, csFirst] then
        begin
          FStatus := csFirst;
          FMemory := FMemory + GetDisplay;
          UpdateMemoryButtons;
          InvalidateMemoryButtons;
        end;
      cbMS:
        if FStatus in [csValid, csFirst] then
        begin
          FStatus := csFirst;
          FMemory := GetDisplay;
          UpdateMemoryButtons;
          InvalidateMemoryButtons;
        end;
      cbMR:
        if FStatus in [csValid, csFirst] then
        begin
          FStatus := csFirst;
          CheckFirst;
          SetDisplay(FMemory);
        end;
      cbMC:
        begin
          FMemory := 0.0;
          UpdateMemoryButtons;
          InvalidateMemoryButtons;
        end;
    end;
  finally
    LockChanges(False, AOldEditorValue <> EditorValue);
  end;
end;

procedure TcxCustomCalculator.UpdateButtonsState;
var
  I: TcxCalcButtonKind;
begin
  for I := Low(I) to High(I) do
    FButtons[I].UpdateState;
end;

procedure TcxCustomCalculator.UpdateMemoryButtons;
begin
  // Disable buttons
  if FMemory <> 0.0 then
  begin
    FButtons[cbMC].Grayed := False;
    FButtons[cbMR].Grayed := False;
  end
  else
  begin
    FButtons[cbMC].Grayed := True;
    FButtons[cbMR].Grayed := True;
  end;
end;

procedure TcxCustomCalculator.InvalidateMemoryButtons;
begin
  FButtons[cbMC].Invalidate;
  FButtons[cbMR].Invalidate;
end;

function TcxCustomCalculator.GetDisplay: Extended;
var
  S: string;
begin
  if FStatus = csError then
    Result := 0.0
  else
  begin
    S := Trim(GetEditorValue);
    if S = '' then S := '0';
    RemoveThousandSeparator(S);
    Result := StrToFloat(S);
  end;
end;

procedure TcxCustomCalculator.SetDisplay(Value: Extended);
var
  S: string;
begin
  S := FloatToStrF(Value, ffGeneral, Max(2, FPrecision), 0);
  if GetEditorValue <> S then
  begin
    SetEditorValue(S);
    if Assigned(FOnDisplayChange) then FOnDisplayChange(Self);
  end;
end;

function TcxCustomCalculator.GetMemory: Extended;
begin
  Result := FMemory;
end;

procedure TcxCustomCalculator.HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  if Assigned(FOnHidePopup) then FOnHidePopup(Self, AReason);
end;

procedure TcxCustomCalculator.LockChanges(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
begin
end;

{ TcxPopupCalculator }

constructor TcxPopupCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsPopupControl := True;
end;

procedure TcxPopupCalculator.Init;
begin
  FPressedButton := cbNone;
end;

function TcxPopupCalculator.GetEditorValue: string;
begin
  Result := Edit.Text;
end;

function TcxPopupCalculator.GetPainter: TcxCustomLookAndFeelPainter;
begin
  if Edit.ViewInfo.UseSkins then
    Result := Edit.ViewInfo.Painter
  else
    Result := GetButtonPainter(Edit.PopupControlsLookAndFeel);
end;

procedure TcxPopupCalculator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_ESCAPE: HidePopup(Self, crCancel);
    VK_INSERT:
      if (Shift = [ssShift]) then
        PasteFromClipboard
      else
        if (Shift = [ssCtrl]) then
          CopyToClipboard;
    VK_F4:
      if not (ssAlt in Shift) then
        HidePopup(Self, crClose);
    VK_UP, VK_DOWN:
      if Shift = [ssAlt] then
        HidePopup(Self, crClose);
    VK_TAB:
      Edit.DoEditKeyDown(Key, Shift);
    VK_RETURN:
      HidePopup(Self, crEnter);
  end;
end;

procedure TcxPopupCalculator.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = '=') and FEdit.ActiveProperties.QuickClose then
    HidePopup(Self, crEnter);
end;

procedure TcxPopupCalculator.SetEditorValue(const Value: string);
begin
  if Edit.DoEditing then
  begin
    Edit.InnerEdit.EditValue := Value;
    Edit.ModifiedAfterEnter := True;
  end;
end;

procedure TcxPopupCalculator.LockChanges(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
begin
  inherited;
  Edit.LockChangeEvents(ALock, AInvokeChangedOnUnlock);
end;

{ TcxCalcEditPropertiesValues }

procedure TcxCalcEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxCalcEditPropertiesValues then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      Precision := TcxCalcEditPropertiesValues(Source).Precision;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCalcEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    Precision := False;
  finally
    EndUpdate;
  end;
end;

procedure TcxCalcEditPropertiesValues.SetPrecision(Value: Boolean);
begin
  if Value <> FPrecision then
  begin
    FPrecision := Value;
    Changed;
  end;
end;

{ TcxCustomCalcEditProperties }

constructor TcxCustomCalcEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBeepOnError := True;
  FPrecision := cxDefCalcPrecision;
//  MaxLength := cxDefCalcPrecision + 2;
  FQuickClose := False;
  PopupSizeable := False;
  ImmediateDropDownWhenKeyPressed := False;
end;

function TcxCustomCalcEditProperties.GetAssignedValues: TcxCalcEditPropertiesValues;
begin
  Result := TcxCalcEditPropertiesValues(FAssignedValues);
end;

function TcxCustomCalcEditProperties.GetPrecision: Byte;
begin
  if AssignedValues.Precision then
    Result := FPrecision
  else
    if IDefaultValuesProvider <> nil then
      Result := IDefaultValuesProvider.DefaultPrecision
    else
      Result := cxDefCalcPrecision;
  if Result > cxMaxCalcPrecision then
    Result := cxMaxCalcPrecision;
end;

function TcxCustomCalcEditProperties.IsPrecisionStored: Boolean;
begin
  Result := AssignedValues.Precision;
end;

procedure TcxCustomCalcEditProperties.SetAssignedValues(
  Value: TcxCalcEditPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomCalcEditProperties.SetBeepOnError(Value: Boolean);
begin
  if Value <> FBeepOnError then
  begin
    FBeepOnError := Value;
    Changed;
  end;
end;

procedure TcxCustomCalcEditProperties.SetPrecision(Value: Byte);
begin
  if AssignedValues.Precision and (Value = FPrecision) then
    Exit;

  AssignedValues.FPrecision := True;
  FPrecision := Value;
  Changed;
end;

procedure TcxCustomCalcEditProperties.SetQuickClose(Value: Boolean);
begin
  if Value <> FQuickClose then
  begin
    FQuickClose := Value;
    Changed;
  end;
end;

procedure TcxCustomCalcEditProperties.SetScientificFormat(Value: Boolean);
begin
  if Value <> FScientificFormat then
  begin
    FScientificFormat := Value;
    Changed;
  end;
end;

procedure TcxCustomCalcEditProperties.SetUseThousandSeparator(Value: Boolean);
begin
  if Value <> FUseThousandSeparator then
  begin
    FUseThousandSeparator := Value;
    Changed;
  end;
end;

function TcxCustomCalcEditProperties.GetAlwaysPostEditValue: Boolean;
begin
  Result := True;
end;

procedure TcxCustomCalcEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomCalcEditProperties then
    with TcxCustomCalcEditProperties(AProperties) do
    begin
      Self.BeepOnError := BeepOnError;

      Self.AssignedValues.Precision := False;
      if AssignedValues.Precision then
        Self.Precision := Precision;

      Self.QuickClose := QuickClose;
      Self.ScientificFormat := ScientificFormat;
      Self.UseThousandSeparator := UseThousandSeparator;
    end;
end;

class function TcxCustomCalcEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxCalcEditPropertiesValues;
end;

function TcxCustomCalcEditProperties.HasDigitGrouping(
  AIsDisplayValueSynchronizing: Boolean): Boolean;
begin
  Result := not ScientificFormat and UseThousandSeparator;
end;

function TcxCustomCalcEditProperties.PopupWindowAcceptsAnySize: Boolean;
begin
  Result := False;
end;

class function TcxCustomCalcEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCalcEdit;
end;

function TcxCustomCalcEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if not AEditFocused and not AssignedValues.DisplayFormat and
    (IDefaultValuesProvider <> nil) and
    IDefaultValuesProvider.IsDisplayFormatDefined(True) then
      Result := evsText
  else
    Result := evsValue;
end;

function TcxCustomCalcEditProperties.IsDisplayValueValid(
  var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
//  if AEditFocused

  Result := True;
end;

function TcxCustomCalcEditProperties.IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
var
  AValue: Extended;
begin
  Result := VarIsNumericEx(AEditValue) or VarIsSoftNull(AEditValue);
  if not Result then
    Result := VarIsStr(AEditValue) and TextToFloat(PChar(VarToStr(AEditValue)), AValue, fvExtended);
end;

procedure TcxCustomCalcEditProperties.DoPrepareDisplayValue(const AEditValue: TcxEditValue;
  var ADisplayValue: TcxEditValue; AEditFocused: Boolean);

  procedure RemoveInsignificantZeros(var S: string);
  var
    AExponentialPart: string;
    I: Integer;
  begin
    if Pos(dxFormatSettings.DecimalSeparator, S) = 0 then
      Exit;
    AExponentialPart := RemoveExponentialPart(S);
    I := Length(S);
    while S[I] = '0' do
      Dec(I);
    Delete(S, I + 1, Length(S) - I);
    if S[Length(S)] = dxFormatSettings.DecimalSeparator then
      Delete(S, Length(S), 1);
    S := S + AExponentialPart;
  end;

var
  AFormat: TFloatFormat;
  APrecision: Byte;
  S: string;
begin
  if VarIsSoftNull(AEditValue) then
    ADisplayValue := ''
  else
    if not AEditFocused and (DisplayFormat <> '') then
      ADisplayValue := FormatFloat(DisplayFormat, AEditValue)
    else
    begin
      if ScientificFormat then
        AFormat := ffExponent
      else
        AFormat := ffGeneral;
      APrecision := Precision;
      if APrecision = 0 then
        APrecision := cxDefCalcPrecision;

      S := FloatToStrF(AEditValue, AFormat, APrecision, 0);
      if UseThousandSeparator and not ScientificFormat then
        InsertThousandSeparator(S);
      if ScientificFormat then
        RemoveInsignificantZeros(S);
      ADisplayValue := S;
    end;
end;

{ TcxCustomCalcEdit }

destructor TcxCustomCalcEdit.Destroy;
begin
  FreeAndNil(FCalculator);
  inherited Destroy;
end;

function TcxCustomCalcEdit.GetProperties: TcxCustomCalcEditProperties;
begin
  Result := TcxCustomCalcEditProperties(inherited Properties);
end;

function TcxCustomCalcEdit.GetActiveProperties: TcxCustomCalcEditProperties;
begin
  Result := TcxCustomCalcEditProperties(InternalGetActiveProperties);
end;

function TcxCustomCalcEdit.GetValue: Double;
begin
  if VarIsNull(EditValue) or (VarIsStr(EditValue) and (EditValue = '')) then
    Result := 0
  else
    Result := EditValue;
end;

procedure TcxCustomCalcEdit.SetProperties(Value: TcxCustomCalcEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomCalcEdit.SetValue(const Value: Double);
begin
  InternalEditValue := Value;
end;

procedure TcxCustomCalcEdit.FormatChanged;
begin
  DataBinding.UpdateDisplayValue;
end;

function TcxCustomCalcEdit.CanDropDown: Boolean;
begin
  Result := (not ActiveProperties.ReadOnly) and DataBinding.IsDataAvailable;
end;

procedure TcxCustomCalcEdit.CreatePopupWindow;
begin
  inherited CreatePopupWindow;
  PopupWindow.ModalMode := False;
end;

procedure TcxCustomCalcEdit.DoEnter;
begin
  SynchronizeDisplayValue;
  inherited;
end;

procedure TcxCustomCalcEdit.DoExit;
begin
  inherited;
  DataBinding.UpdateDisplayValue;
end;

procedure TcxCustomCalcEdit.DoInitPopup;
begin
  inherited DoInitPopup;
  ActiveProperties.PopupControl := FCalculator;
end;

procedure TcxCustomCalcEdit.Initialize;
begin
  inherited Initialize;
  Value := 0;
  FCalculator := TcxPopupCalculator.Create(Self);
  FCalculator.Parent := PopupWindow;
  FCalculator.Edit := Self;
  FCalculator.AutoFontSize := False;
  FCalculator.OnHidePopup := HidePopup;
  ActiveProperties.PopupControl := FCalculator;
end;

procedure TcxCustomCalcEdit.InitializePopupWindow;
begin
  inherited InitializePopupWindow;
  with Calculator do
  begin
    HandleNeeded;
    Font.Assign(Self.VisibleFont);
    FontChanged;
    Resize;
  end;
end;

function TcxCustomCalcEdit.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Text, Result, True);
end;

function TcxCustomCalcEdit.IsValidChar(Key: Char): Boolean;

  function NumDigits(const S: string): Byte;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
      if (S[I] = 'e') or (S[I] = 'E') then
        Break
      else
        if dxCharInSet(S[I], ['0'..'9']) then
          Inc(Result);
  end;

var
  S: string;
  V: Double;
  StartPos, StopPos: Integer;
begin
  Result := False;
  if not IsNumericChar(Key, ntExponent) then
    Exit;
  S := Text;
  StartPos := SelStart;
  StopPos := SelStart + SelLength;
  Delete(S, SelStart + 1, StopPos - StartPos);
  if (Key = '-') and (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Insert(Key, S, StartPos + 1);
  Result := StrToFloatEx(S, V);
end;

procedure TcxCustomCalcEdit.KeyPress(var Key: Char);
begin
  if (Key = '.') or (Key = ',') then
    Key := dxFormatSettings.DecimalSeparator;
  if IsTextChar(Key) and not IsValidChar(Key) then
  begin
    Key := #0;
    if ActiveProperties.BeepOnError then Beep;
  end;
  inherited KeyPress(Key);
end;

procedure TcxCustomCalcEdit.PopupWindowClosed(Sender: TObject);
begin
  if Text = cxGetResourceString(@scxSCalcError) then InternalEditValue := 0;
  if ActiveProperties.AutoSelect then SelectAll else SelStart := Length(Text);
  inherited PopupWindowClosed(Sender);
end;

procedure TcxCustomCalcEdit.PopupWindowShowed(Sender: TObject);
begin
  inherited PopupWindowShowed(Sender);
  FCalculator.Init;
end;

procedure TcxCustomCalcEdit.PropertiesChanged(Sender: TObject);
begin
  if (Sender <> nil) and ActiveProperties.FormatChanging then
    Exit;
  inherited PropertiesChanged(Sender);
  if not PropertiesChangeLocked then
  begin
    FCalculator.BeepOnError := ActiveProperties.BeepOnError;
    FCalculator.Precision := ActiveProperties.Precision;
    ActiveProperties.FChangedLocked := True;
    ActiveProperties.PopupControl := FCalculator;
    ActiveProperties.FChangedLocked := False;
  end;
end;

function TcxCustomCalcEdit.InternalPrepareEditValue(const ADisplayValue: string;
  out EditValue: TcxEditValue): Boolean;
var
  AValue: Extended;
  S: string;
begin
  Result := True;
  S := VarToStr(ADisplayValue);
  if not ActiveProperties.ScientificFormat and ActiveProperties.UseThousandSeparator then
    RemoveThousandSeparator(S);
  if Trim(S) = '' then
    EditValue := Null
  else
  begin
    Result := TextToFloat(PChar(S), AValue, fvExtended);
    if Result then
      EditValue := AValue
    else
      EditValue := Null;
  end;
end;

class function TcxCustomCalcEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCalcEditProperties;
end;

procedure TcxCustomCalcEdit.PasteFromClipboard;
begin
  if DoEditing then
    Calculator.PasteFromClipboard;
end;

procedure TcxCustomCalcEdit.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  InternalPrepareEditValue(ADisplayValue, EditValue);
end;

{ TcxCalcEdit }

class function TcxCalcEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCalcEditProperties;
end;

function TcxCalcEdit.GetActiveProperties: TcxCalcEditProperties;
begin
  Result := TcxCalcEditProperties(InternalGetActiveProperties);
end;

function TcxCalcEdit.GetProperties: TcxCalcEditProperties;
begin
  Result := TcxCalcEditProperties(inherited Properties);
end;

procedure TcxCalcEdit.SetProperties(Value: TcxCalcEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterCalcEditHelper }

class function TcxFilterCalcEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxCalcEdit;
end;

class function TcxFilterCalcEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoLess, fcoLessEqual, fcoGreater,
    fcoGreaterEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoBetween, fcoNotBetween, fcoInList, fcoNotInList];
end;

class procedure TcxFilterCalcEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  TcxCustomCalcEditProperties(AProperties).QuickClose := True;
end;

{ TcxCalcEditButtonViewInfo }

constructor TcxCalcEditButtonViewInfo.Create(AOwner: TcxCustomCalculator);
begin
  inherited Create;
  FOwner := AOwner;
  FState := cxbsNormal;
  FadingHelper := TcxCalcEditButtonFadingHelper.Create(Self);
end;

destructor TcxCalcEditButtonViewInfo.Destroy;
begin
  FreeAndNil(FadingHelper);
  inherited Destroy;
end;

function TcxCalcEditButtonViewInfo.CalculateState: TcxButtonState;
begin
  if Grayed or not Owner.Enabled then
    Result := cxbsDisabled
  else
    if Down then
      Result := cxbsPressed
    else
      if (Owner.FActiveButton = Kind) and (Owner.FDownButton <> Kind) then
        Result := cxbsHot
      else
        if IsDefault then
          Result := cxbsDefault
        else
          Result := cxbsNormal;
end;

procedure TcxCalcEditButtonViewInfo.Invalidate;
begin
  if FOwner.HandleAllocated then
    cxInvalidateRect(FOwner.Handle, Bounds, False);
end;

function TcxCalcEditButtonViewInfo.GetIsDefault: Boolean;
begin
  Result := Owner.IsFocused and (Kind = cbEqual);
end;

procedure TcxCalcEditButtonViewInfo.SetDown(AValue: Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    UpdateState;
  end;
end;

procedure TcxCalcEditButtonViewInfo.SetGrayed(const Value: Boolean);
begin
  if Value <> FGrayed then
  begin
    FGrayed := Value;
    UpdateState;
  end;
end;

procedure TcxCalcEditButtonViewInfo.SetState(const Value: TcxButtonState);
begin
  if Value <> FState then
  begin
    FadingHelper.CheckStartFading(State, Value);
    FState := Value;
    Invalidate;
  end;
end;

procedure TcxCalcEditButtonViewInfo.UpdateState;
begin
  State := CalculateState;
end;

{ TcxCalcEditButtonFadingHelper }

constructor TcxCalcEditButtonFadingHelper.Create(AViewInfo: TcxCalcEditButtonViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

function TcxCalcEditButtonFadingHelper.CanFade: Boolean;
begin
  Result := Painter.LookAndFeelStyle in [lfsNative, lfsSkin];
end;

procedure TcxCalcEditButtonFadingHelper.DrawFadeImage;
begin
  ViewInfo.Invalidate;
end;

procedure TcxCalcEditButtonFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareFadingImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(ViewInfo.Bounds, True);
    Painter.DrawScaledButton(Result.cxCanvas, Result.ClientRect, '', AState, ViewInfo.Owner.ScaleFactor);
  end;

const
  DefaultStateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsDefault);
begin
  AFadeInImage := PrepareFadingImage(cxbsHot);
  AFadeOutImage := PrepareFadingImage(DefaultStateMap[ViewInfo.IsDefault]);
end;

function TcxCalcEditButtonFadingHelper.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ViewInfo.Owner.GetPainter;
end;

initialization
  GetRegisteredEditProperties.Register(TcxCalcEditProperties, scxSEditRepositoryCalcItem);
  FilterEditsController.Register(TcxCalcEditProperties, TcxFilterCalcEditHelper);

finalization
  FilterEditsController.Unregister(TcxCalcEditProperties, TcxFilterCalcEditHelper);

end.
