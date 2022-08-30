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

unit cxRadioGroup;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Variants, Classes, Controls, Forms, Graphics,
  ImgList, Menus, StdCtrls, SysUtils,
  dxCore, dxUxTheme, cxClasses, cxContainer, cxControls, cxDataStorage, cxGraphics, cxLookAndFeels,
  cxEdit, cxTextEdit, cxGroupBox, cxDropDownEdit, cxFilterControlUtils, dxFading, cxGeometry;

type
  TcxRadioButtonState = (rbsDisabled, rbsHot, rbsNormal, rbsPressed);
  TcxRadioGroupState = (rgsActive, rgsDisabled, rgsHot, rgsNormal);

  TcxCustomRadioGroup = class;
  TcxCustomRadioGroupProperties = class;
  TcxRadioButton = class;

  { TcxRadioButtonActionLink }

  TcxRadioButtonActionLink = class(TButtonActionLink)
  protected
    FClient: TcxRadioButton;
    procedure AssignClient(AClient: TObject); override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
  end;

  { TcxRadioButton }

  TcxRadioButton = class(TRadioButton,
    IdxFadingObject,
    IcxMouseTrackingCaller,
    IcxLookAndFeelContainer,
    IdxSkinSupport,
    IdxScaleFactor)
  private
    FAutoSize: Boolean;
    FCanvas: TcxCanvas;
    FChecked: Boolean;
    FColumn: Integer;
    FControlCanvas: TControlCanvas;
    FGroupIndex: Integer;
    FLookAndFeel: TcxLookAndFeel;
    FPopupMenu: TComponent;
    FRow: Integer;
    FScaleFactor: TdxScaleFactor;
    FShowEndEllipsis: Boolean;
    FState: TcxRadioButtonState;
    FTransparent: Boolean;

    procedure AdjustCanvasFontSettings(ACanvas: TcxCanvas);
    procedure CalculateNewSize(var AWidth, AHeight: Integer);
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    function GetBiDiModeDependentAlignment: TLeftRight;
    function GetButtonRect: TRect;
    function GetRadioButtonRect(const ARadioButtonSize: TSize; ANativeStyle: Boolean): TRect;
    function GetScaleFactor: TdxScaleFactor;
    function GetTextColor: TColor;
    function GetTextRect: TRect;
    function GetTextRectCorrection(ANativeStyle: Boolean): TRect;
    function IsDisabledTextColorAssigned: Boolean;
    procedure SetRadioButtonAutoSize(AValue: Boolean);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetPopupMenu(Value: TComponent);
    procedure SetShowEndEllipsis(Value: Boolean);
    procedure SetState(Value: TcxRadioButtonState);
    procedure SetTransparent(Value: Boolean);
    // Messages
    procedure BMSetCheck(var Message: TMessage); message BM_SETCHECK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNSysKeyDown(var Message: TWMSysKeyDown); message CN_SYSKEYDOWN;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure CheckStartFading(APrevState, AState: TcxRadioButtonState); virtual;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoEnter; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetChecked: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetParent(AValue: TWinControl); override;

    procedure CorrectTextRect(var R: TRect; ANativeStyle: Boolean); virtual;
    function DoShowPopupMenu(APopupMenu: TComponent; X, Y: Integer): Boolean; virtual;
    procedure EnabledChanged; dynamic;
    procedure InternalPolyLine(const APoints: array of TPoint);
    procedure InvalidateRadioButton;
    function IsInplace: Boolean; virtual;
    function IsNativeBackground: Boolean; virtual;
    function IsNativeStyle: Boolean; virtual;
    function IsTransparent: Boolean; virtual;
    function IsTransparentBackground: Boolean; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure ShortUpdateState;
    procedure UpdateState(Button: TcxMouseButton; Shift: TShiftState; const P: TPoint); virtual;

    // Drawing
    function NeedDoubleBuffered: Boolean; virtual;
    procedure Draw(ACanvas: TcxCanvas; ADrawOnlyFocusedState: Boolean); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawCaption(ACanvas: TcxCanvas; ANativeStyle: Boolean); virtual;
    procedure DrawRadioButton(ACanvas: TcxCanvas; AState: TcxRadioButtonState); virtual;
    procedure Paint(ADrawOnlyFocusedState: Boolean); virtual;
    procedure PrepareBackground(ACanvas: TcxCanvas); virtual;

    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingCallerMouseLeave;
    procedure MouseTrackingCallerMouseLeave;

    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel;

    // IdxFadingObject
    function CanFade: Boolean;
    procedure IdxFadingObject.DrawFadeImage = InvalidateRadioButton;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

    property ButtonRect: TRect read GetButtonRect;
    property Canvas: TcxCanvas read FCanvas;
    property Column: Integer read FColumn;
    property Row: Integer read FRow;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxRadioButtonState read FState write SetState;
    property TextRect: TRect read GetTextRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Focused: Boolean; override;
    procedure Invalidate; override;
  published
    property AutoSize: Boolean read FAutoSize write SetRadioButtonAutoSize default False;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
    property ParentBackground default True;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  {  TcxRadioGroupButtonViewInfo  }

  TcxRadioGroupButtonViewInfo = class(TcxButtonGroupButtonViewInfo)
  public
    function GetGlyphRect(const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect; override;
  end;

  { TcxCustomRadioGroupViewInfo }

  TcxCustomRadioGroupViewInfo = class(TcxButtonGroupViewInfo)
  private
    function GetEdit: TcxCustomRadioGroup;
    function ThemeHandle: TdxTheme;
  protected
    procedure DrawButtonCaption(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; override;
    function IsButtonGlyphTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean; override;
  public
    ItemIndex: Integer;
    constructor Create; override;
    property Edit: TcxCustomRadioGroup read GetEdit;
  end;

  { TcxCustomRadioGroupViewData }

  TcxCustomRadioGroupViewData = class(TcxButtonGroupViewData)
  private
    function GetProperties: TcxCustomRadioGroupProperties;
  protected
    procedure CalculateButtonNativePartInfo(ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo); override;
    function GetButtonNativeTheme(AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme; override;
    procedure GetEditMetrics(AAutoHeight: Boolean; ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics); override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    property Properties: TcxCustomRadioGroupProperties read GetProperties;
  end;

  { TcxRadioGroupItem }

  TcxRadioGroupItem = class(TcxButtonGroupItem)
  private
    FValue: TcxEditValue;
    function IsValueStored: Boolean;
    procedure SetValue(const Value: TcxEditValue);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption;
    property Value: TcxEditValue read FValue write SetValue stored IsValueStored;
    property Tag;
  end;

  { TcxRadioGroupItems }

  TcxRadioGroupItems = class(TcxButtonGroupItems)
  private
    function GetItem(Index: Integer): TcxRadioGroupItem;
    procedure SetItem(Index: Integer; Value: TcxRadioGroupItem);
  public
    function Add: TcxRadioGroupItem;
    property Items[Index: Integer]: TcxRadioGroupItem read GetItem write SetItem; default;
  end;

  { TcxCustomRadioGroupProperties }

  TcxCustomRadioGroupProperties = class(TcxCustomButtonGroupProperties)
  private
    FDefaultCaption: WideString;
    FDefaultValue: TcxEditValue;

    function GetButtonValue(AIndex: Integer): TcxEditValue;
    function GetItems: TcxRadioGroupItems;
    function IsDefaultCaptionStored: Boolean;
    function IsDefaultValueStored: Boolean;
    procedure SetDefaultValue(const Value: TcxEditValue);
    procedure SetItems(Value: TcxRadioGroupItems);
  protected
    function CreateItems: TcxButtonGroupItems; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetColumnCount: Integer; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    function CanCompareEditValue: Boolean; override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetRadioGroupItemIndex(const AEditValue: TcxEditValue): Integer;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsResetEditClass: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    // !!!
    property DefaultCaption: WideString read FDefaultCaption write FDefaultCaption stored IsDefaultCaptionStored;
    property DefaultValue: TcxEditValue read FDefaultValue write SetDefaultValue stored IsDefaultValueStored;
    property Items: TcxRadioGroupItems read GetItems write SetItems;
  end;

  { TcxRadioGroupProperties }

  TcxRadioGroupProperties = class(TcxCustomRadioGroupProperties)
  published
    property AssignedValues;
    property ClearKey;
    property Columns;
    property DefaultCaption;
    property DefaultValue;
    property ImmediatePost;
    property Items;
    property ReadOnly;
    property ShowEndEllipsis;
    property WordWrap;
    property OnChange;
    property OnEditValueChanged;
  end;

  { TcxCustomRadioGroupButton }

  TcxCustomRadioGroupButton = class(TcxRadioButton, IcxContainerInnerControl)
  private
    FFocusingByMouse: Boolean;
    FInternalSettingChecked: Boolean;
    FIsClickLocked: Boolean;
    function GetRadioGroup: TcxCustomRadioGroup;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure CorrectTextRect(var R: TRect; ANativeStyle: Boolean); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure PrepareBackground(ACanvas: TcxCanvas); override;
    function IsInplace: Boolean; override;
    function IsNativeBackground: Boolean; override;
    function IsNativeStyle: Boolean; override;
    function IsTransparent: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure SetChecked(Value: Boolean); override;
    procedure WndProc(var Message: TMessage); override;

    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    procedure InternalSetChecked(AValue: Boolean);

    property RadioGroup: TcxCustomRadioGroup read GetRadioGroup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CanFocus: Boolean; override;
    procedure DefaultHandler(var Message); override;
  end;

  TcxCustomRadioGroupButtonClass = class of TcxCustomRadioGroupButton;

  { TcxCustomRadioGroup }

  TcxCustomRadioGroup = class(TcxCustomButtonGroup)
  private
    FLoadedItemIndex: Integer;
    function GetCheckedIndex: Integer;
    function GetButton(Index: Integer): TcxCustomRadioGroupButton;
    function GetProperties: TcxCustomRadioGroupProperties;
    function GetActiveProperties: TcxCustomRadioGroupProperties;
    function GetItemIndex: Integer;
    function GetViewInfo: TcxCustomRadioGroupViewInfo;
    procedure SetItemIndex(Value: Integer);
    procedure SetProperties(Value: TcxCustomRadioGroupProperties);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure ButtonClickHandler(Sender: TObject);
  protected
    procedure CursorChanged; override;
    procedure DoSetFocusWhenActivate; override;
    function GetButtonDC(AButtonIndex: Integer): THandle; override;
    procedure Initialize; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    function IsContainerFocused: Boolean; override;
    function IsInternalControl(AControl: TControl): Boolean; override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure SynchronizeButtonsStyle; override;
    procedure ParentBackgroundChanged; override;
    procedure Resize; override;
    procedure SetDragKind(Value: TDragKind); override;
    procedure ArrangeButtons; override;
    function GetButtonInstance: TWinControl; override;
    procedure InternalUpdateButtons; override;

    procedure SetInternalValues(const AEditValue: TcxEditValue; AValidateEditValue, AFromButtonChecked: Boolean);
    procedure UpdateValues;
    function IsLoading: Boolean;
    procedure Loaded; override;
    procedure Updated; override;

    procedure ButtonChecked(AButton: TcxCustomRadioGroupButton);

    property ViewInfo: TcxCustomRadioGroupViewInfo read GetViewInfo;
  public
    procedure Clear; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure GetTabOrderList(List: TList); override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    procedure SetFocus; override;

    property ActiveProperties: TcxCustomRadioGroupProperties read GetActiveProperties;
    property Buttons[Index: Integer]: TcxCustomRadioGroupButton read GetButton;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property Properties: TcxCustomRadioGroupProperties read GetProperties write SetProperties;
    property Transparent;
  end;

  { TcxRadioGroup }

  TcxRadioGroup = class(TcxCustomRadioGroup)
  private
    function GetActiveProperties: TcxRadioGroupProperties;
    function GetProperties: TcxRadioGroupProperties;
    procedure SetProperties(Value: TcxRadioGroupProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxRadioGroupProperties read GetActiveProperties;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxRadioGroupProperties read GetProperties write SetProperties;
    property ItemIndex;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxFilterRadioGroupHelper }

  TcxFilterRadioGroupHelper = class(TcxFilterComboBoxHelper)
  public
    class procedure GetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption); override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
    class procedure SetFilterValue(AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
      AValue: Variant); override;
    class function UseDisplayValue: Boolean; override;
  end;

implementation

uses
  Math, ActnList, dxThemeConsts, dxThemeManager, cxVariants, cxLookAndFeelPainters,
  cxEditConsts, cxEditPaintUtils, cxEditUtils, dxDPIAwareUtils;

type
  TCanvasAccess = class(TCanvas);

{ TcxRadioButtonActionLink }

procedure TcxRadioButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited;
  FClient := AClient as TcxRadioButton;
end;

function TcxRadioButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := FClient.GroupIndex = (Action as TCustomAction).GroupIndex;
end;

procedure TcxRadioButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    FClient.GroupIndex := Value;
end;

{ TcxRadioButton }

constructor TcxRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := TdxScaleFactor.Create;
  FControlCanvas := TControlCanvas.Create;
  FControlCanvas.Control := Self;
  FCanvas := TcxCanvas.Create(TCanvas(FControlCanvas));
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FState := rbsNormal;
  ParentBackground := True;
  PrepareRadioButtonImageList(ScaleFactor);
  ControlStyle := ControlStyle + [{$IFDEF DELPHI16} csOverrideStylePaint,{$ENDIF} csDoubleClicks];
end;

destructor TcxRadioButton.Destroy;
begin
  dxFader.Remove(Self);
  EndMouseTracking(Self);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FCanvas);
  FreeAndNil(FControlCanvas);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

function TcxRadioButton.Focused: Boolean;
begin
  Result := not (csDesigning in ComponentState) and inherited Focused;
end;

procedure TcxRadioButton.Invalidate;
begin
  cxInvalidateRect(Self, GetControlRect(Self), False);
end;

procedure TcxRadioButton.InvalidateRadioButton;
begin
  cxInvalidateRect(Self, cxRectInflate(ButtonRect, 1, 1), False);
end;

procedure TcxRadioButton.DoEnter;
begin
  inherited DoEnter;
  if not Checked and not ClicksDisabled then
  begin
    ClicksDisabled := True;
    try
      Checked := True;
    finally
      ClicksDisabled := False;
      if not (csLoading in ComponentState) then
        Click;
    end;
  end;
end;

function TcxRadioButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TcxRadioButtonActionLink;
end;

function TcxRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TcxRadioButton.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareFadingImage(AHotTracked: Boolean): TcxBitmap32;
  const
    StateMap: array[Boolean] of TcxRadioButtonState = (rbsNormal, rbsHot);
  var
    AButtonRect: TRect;
  begin
    AButtonRect := ButtonRect;
    Result := TcxBitmap32.CreateSize(AButtonRect, True);
    Result.cxCanvas.WindowOrg := AButtonRect.TopLeft;
    DrawRadioButton(Result.cxCanvas, StateMap[AHotTracked]);
    Result.cxCanvas.WindowOrg := cxNullPoint;
  end;

begin
  AFadeInImage := PrepareFadingImage(True);
  AFadeOutImage := PrepareFadingImage(False);
end;

procedure TcxRadioButton.DrawCaption(ACanvas: TcxCanvas; ANativeStyle: Boolean);

  function GetDrawTextFlags: Integer;
  begin
    if UseRightToLeftAlignment then
      Result := cxAlignRight
    else
      Result := cxAlignLeft;
    if UseRightToLeftReading then
      Result := Result or cxRtlReading;
    Result := Result or cxAlignVCenter or cxShowPrefix;
    if WordWrap then
      Result := Result or cxDontClip or cxWordBreak
    else
    begin
      Result := Result or cxSingleLine;
      if ShowEndEllipsis then
        Result := Result or cxShowEndEllipsis;
    end;
  end;

  procedure CheckFocusRect(var R: TRect);
  begin
    if IsInplace then
    begin
      R.Top := Max(R.Top, 1);
      R.Bottom := Min(R.Bottom, Height - 1);
      R.Right := Min(R.Right, Width);
    end
    else
    begin
      R.Left := Min(R.Left, 0);
      R.Top := Min(R.Top, 0);
      R.Right := Min(R.Right, Width);
      R.Bottom := Min(R.Bottom, Height);
      if (GetBiDiModeDependentAlignment = taLeftJustify) then
        R.Right := Min(ButtonRect.Left, R.Right);
    end;
  end;

var
  AFlags: Integer;
  R: TRect;
begin
  AdjustCanvasFontSettings(ACanvas);

  R := TextRect;
  ACanvas.Brush.Style := bsClear;
  AFlags := GetDrawTextFlags;
  if IsNativeBackground then
    ACanvas.Canvas.Refresh;
  ACanvas.DrawText(Caption, R, AFlags, IsDisabledTextColorAssigned or
    Supports(Self, IcxContainerInnerControl) or ANativeStyle or Enabled);
  ACanvas.Brush.Style := bsSolid;
  if Focused and (Caption <> '') then
  begin
    ACanvas.TextExtent(Caption, R, AFlags);
    InflateRect(R, 1, 1);
    Inc(R.Bottom);
    if IsInplace then
      CheckFocusRect(R);
    ACanvas.Brush.Color := Color;
    ACanvas.Font.Color := Font.Color;
    TCanvasAccess(ACanvas.Canvas).RequiredState([csFontValid]);
    ACanvas.Canvas.DrawFocusRect(R);
  end;
end;

procedure TcxRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  UpdateState(ButtonTocxButton(Button), Shift, Point(X, Y));
end;

procedure TcxRadioButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  UpdateState(cxmbNone, Shift, Point(X, Y));
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  UpdateState(ButtonTocxButton(Button), Shift, Point(X, Y));
end;

function TcxRadioButton.NeedDoubleBuffered: Boolean;
begin
  Result := (LookAndFeel.SkinPainter <> nil) or dxFader.Contains(Self) or IsWinSevenOrLater;
end;

procedure TcxRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

procedure TcxRadioButton.SetChecked(Value: Boolean);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    ASibling: TControl;
    AAction: TCustomAction;
  begin
    if Parent <> nil then
      for I := 0 to Parent.ControlCount - 1 do
      begin
        ASibling := Parent.Controls[I];
        if (ASibling <> Self) and (ASibling is TRadioButton) then
        begin
          if not (ASibling is TcxRadioButton) or (TcxRadioButton(ASibling).GroupIndex = GroupIndex) then
          begin
            if Assigned(TRadioButton(ASibling).Action) and (TRadioButton(ASibling).Action is TCustomAction) then
            begin
              AAction := TCustomAction(TRadioButton(ASibling).Action);
              if AAction.AutoCheck then
                AAction.Checked := False;
            end;
            TRadioButton(ASibling).Checked := False;
          end;
        end;
      end;
  end;

begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, WPARAM(Checked), 0);
    if Value then
    begin
      TurnSiblingsOff;
      Changed;
      if not ClicksDisabled then
        Click;
    end;
  end;
end;

procedure TcxRadioButton.SetParent(AValue: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AValue) then
  begin
    inherited SetParent(AValue);
    TcxControlHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

procedure TcxRadioButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
    if CheckDefaults or (Self.GroupIndex = 0) then
      GroupIndex := TCustomAction(Sender).GroupIndex;
end;

function TcxRadioButton.CanFade: Boolean;
begin
  Result := IsNativeStyle or (LookAndFeel.SkinPainter <> nil);
end;

{$IFDEF DELPHIBERLIN}
procedure TcxRadioButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TcxRadioButton.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  ScaleFactor.Change(M, D);
  inherited;
  LookAndFeel.Refresh;
end;

function TcxRadioButton.CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  CalculateNewSize(NewWidth, NewHeight);
end;

procedure TcxRadioButton.CheckStartFading(APrevState, AState: TcxRadioButtonState);
begin
  if APrevState <> AState then
  begin
    if (APrevState = rbsNormal) and (AState = rbsHot) then
      dxFader.FadeIn(Self)
    else
      if (APrevState = rbsHot) and (AState = rbsNormal) then
        dxFader.FadeOut(Self)
      else
        dxFader.Remove(Self, False);
  end;
end;

procedure TcxRadioButton.CreateHandle;
begin
  inherited CreateHandle;
  ShortUpdateState;
end;

procedure TcxRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not BS_RADIOBUTTON or BS_OWNERDRAW;
end;

procedure TcxRadioButton.CreateWnd;
begin
  inherited;
  SendMessage(Handle, BM_SETCHECK, WPARAM(Checked), 0);
  if AutoSize then
    AdjustSize;
end;

procedure TcxRadioButton.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;
begin
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
  begin
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      P := ClientToScreen(Point(0, 0))
    else
      P := ClientToScreen(MousePos);
    Handled := DoShowPopupMenu(PopupMenu, P.X, P.Y);
  end;
end;

procedure TcxRadioButton.CorrectTextRect(var R: TRect; ANativeStyle: Boolean);
var
  AOffsets: TRect;
begin
  AOffsets := GetTextRectCorrection(ANativeStyle);
  if UseRightToLeftAlignment then
    AOffsets := TdxRightToLeftLayoutConverter.ConvertOffsets(AOffsets);
  R := cxRectContent(R, ScaleFactor.Apply(AOffsets));
end;

function TcxRadioButton.DoShowPopupMenu(APopupMenu: TComponent; X, Y: Integer): Boolean;
begin
  Result := ShowPopupMenu(Self, APopupMenu, X, Y);
end;

procedure TcxRadioButton.Draw(ACanvas: TcxCanvas; ADrawOnlyFocusedState: Boolean);
begin
  if not ADrawOnlyFocusedState then
  begin
    PrepareBackground(ACanvas);
    DrawBackground(ACanvas);
  end;
  if not dxFader.DrawFadeImage(Self, ACanvas.Handle, ButtonRect) then
    DrawRadioButton(ACanvas, State);
  DrawCaption(ACanvas, IsNativeStyle);
end;

procedure TcxRadioButton.DrawBackground(ACanvas: TcxCanvas);
begin
  if not IsTransparentBackground then
    cxEditFillRect(ACanvas, GetControlRect(Self), Color);
end;

procedure TcxRadioButton.EnabledChanged;
begin
  ShortUpdateState;
  Invalidate;
end;

procedure TcxRadioButton.AdjustCanvasFontSettings(ACanvas: TcxCanvas);
var
  AInnerControl: IcxContainerInnerControl;
  ABackgroundColor, ATextColor: TColor;
begin
  if Supports(Self, IcxContainerInnerControl, AInnerControl) then
  begin
    ACanvas.Font.Assign((AInnerControl.ControlContainer as TcxCustomRadioGroup).FCaptionFont);
    (AInnerControl.ControlContainer as TcxCustomRadioGroup).GetColorSettingsByPainter(ABackgroundColor, ATextColor);
    if ATextColor <> clDefault then
      ACanvas.Font.Color := ATextColor;
  end
  else
  begin
    ACanvas.Font.Assign(Font);
    ACanvas.Font.Color := GetTextColor;
  end;
end;

procedure TcxRadioButton.CalculateNewSize(var AWidth, AHeight: Integer);

  function GetTextFlags: Integer;
  begin
    Result := DT_NOPREFIX or DT_CALCRECT;
    if not WordWrap then
      Result := Result or DT_SINGLELINE
    else
      Result := Result or DT_WORDBREAK;

    if ShowEndEllipsis then
      Result := Result or DT_END_ELLIPSIS;
  end;

var
  ARect: TRect;
  AOffsets: TRect;
  AButtonEdge: Integer;
begin
  if AutoSize and not (Align in [alClient]) then
  begin
    Canvas.SaveState;
    try
      if UseRightToLeftAlignment then
        AButtonEdge := ButtonRect.Right
      else
        AButtonEdge := ButtonRect.Left;
      Canvas.Font := Font;
      if Text <> '' then
      begin
        AOffsets := ScaleFactor.Apply(GetTextRectCorrection(IsNativeStyle));
        ARect := cxRectSetWidth(cxNullRect, Max(AWidth - cxRectWidth(ButtonRect) - AButtonEdge - AOffsets.Left - AOffsets.Right, 1));
        cxDrawText(Canvas.Handle, Text, ARect, GetTextFlags);
        ARect := cxRectInflate(ARect, AOffsets);
        ARect := cxRectInflate(ARect, 1, 0);
        if not (Align in [alTop, alBottom]) then
          AWidth := cxRectWidth(ARect) + cxRectWidth(ButtonRect) + AButtonEdge;
        if not (Align in [alLeft, alRight]) then
          AHeight := Max(cxRectHeight(ARect), cxRectHeight(ButtonRect)) + ScaleFactor.Apply(4);
      end
      else
      begin
        if not (Align in [alTop, alBottom]) then
          AWidth := cxRectWidth(ButtonRect) + 2 * AButtonEdge;
        if not (Align in [alLeft, alRight]) then
          AHeight := cxRectHeight(ButtonRect) + ScaleFactor.Apply(4);
      end;
    finally
      Canvas.RestoreState;
    end;
  end;
end;

function TcxRadioButton.GetTextColor: TColor;
var
  AIntf: IdxCustomSkinnedContainer;
begin
  Result := clDefault;
  if Font.Color = clWindowText then
  begin
    Result := LookAndFeel.Painter.DefaultEditorTextColor(not Enabled);
    if Supports(Parent, IdxCustomSkinnedContainer, AIntf) then
      Result := cxGetActualColor(AIntf.GetDefaultTextColor(Enabled), Result);
  end;
  if Result = clDefault then
  begin
    if Enabled then
      Result := Font.Color
    else
      Result := clBtnShadow;
  end;
end;

function TcxRadioButton.GetTextRect: TRect;
begin
  Result := GetControlRect(Self);
  if GetBiDiModeDependentAlignment = taRightJustify then
    Result.Left := ButtonRect.Right
  else
    Result.Right := ButtonRect.Left;
  CorrectTextRect(Result, IsNativeStyle);
end;

function TcxRadioButton.GetTextRectCorrection(ANativeStyle: Boolean): TRect;
const
  AInplaceTextRectCorrectionA: array [Boolean] of TRect = (
    (Left: 5; Top: 0; Right: 1; Bottom: 0),
    (Left: 3; Top: 0; Right: 0; Bottom: 0)
  );
  ATextRectCorrectionA: array [Boolean, TLeftRight] of TRect = (
   ((Left: 2; Top: -1; Right: 1; Bottom: 0),
    (Left: 5; Top: -1; Right: 0; Bottom: 0)),
   ((Left: 2; Top: -1; Right: 6; Bottom: 0),
    (Left: 5; Top: -1; Right: 2; Bottom: 0))
    );
  ANativeStyleTextRectCorrectionA: array [Boolean, TLeftRight] of TRect = (
   ((Left: 0; Top: -1; Right: 1; Bottom: 0),
    (Left: 3; Top: -1; Right: 0; Bottom: 0)),
   ((Left: 0; Top: -1; Right: 3; Bottom: 0),
    (Left: 3; Top: -1; Right: 0; Bottom: 0))
  );
begin
  if IsInplace then
    Result := AInplaceTextRectCorrectionA[ANativeStyle]
  else
    if ANativeStyle then
    begin
      Result := ANativeStyleTextRectCorrectionA[WordWrap, Alignment];
      if EmulateStandardControlDrawing then
      begin
        Result.Top := 0;
        Result.Bottom := 0;
      end;
    end
    else
      Result := ATextRectCorrectionA[WordWrap, Alignment];
end;

procedure TcxRadioButton.InternalPolyLine(const APoints: array of TPoint);
begin
  Canvas.Polyline(APoints);
  with APoints[High(APoints)] do
    Canvas.Pixels[X, Y] := Canvas.Pen.Color;
end;

function TcxRadioButton.IsDisabledTextColorAssigned: Boolean;
begin
  Result := LookAndFeel.Painter.DefaultEditorTextColor(True) <> clDefault;
end;

function TcxRadioButton.IsInplace: Boolean;
begin
  Result := False;
end;

function TcxRadioButton.IsNativeBackground: Boolean;
begin
  Result := IsNativeStyle and ParentBackground and not IsInplace and
    not Transparent;
end;

function TcxRadioButton.IsNativeStyle: Boolean;
begin
  Result := AreVisualStylesMustBeUsed(LookAndFeel.NativeStyle, totButton);
end;

function TcxRadioButton.IsTransparent: Boolean;
begin
  Result := Transparent and not IsInplace;
end;

function TcxRadioButton.IsTransparentBackground: Boolean;
begin
  Result := IsNativeBackground or IsTransparent;
end;

procedure TcxRadioButton.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TcxRadioButton.MouseEnter(AControl: TControl);
begin
  ShortUpdateState;
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxRadioButton.MouseLeave(AControl: TControl);
begin
  UpdateState(cxmbNone, [], Point(-1, -1));
  EndMouseTracking(Self);
end;

procedure TcxRadioButton.Paint(ADrawOnlyFocusedState: Boolean);
var
  ABitmap: TcxBitmap;
begin
  if not DoubleBuffered and NeedDoubleBuffered then
  begin
    ABitmap := TcxBitmap.CreateSize(ClientRect, pf32bit);
    try
      ABitmap.Canvas.Lock;
      Draw(ABitmap.cxCanvas, False);
      ABitmap.Canvas.Unlock;
      Canvas.Draw(0, 0, ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
    Draw(Canvas, ADrawOnlyFocusedState);
end;

procedure TcxRadioButton.PrepareBackground(ACanvas: TcxCanvas);
begin
  if IsTransparent then
    cxDrawTransparentControlBackground(Self, ACanvas, GetControlRect(Self))
  else
    if IsNativeBackground then
      cxDrawThemeParentBackground(Self, ACanvas, GetControlRect(Self));
end;

procedure TcxRadioButton.ShortUpdateState;
begin
  if HandleAllocated then
    UpdateState(cxmbNone, KeyboardStateToShiftState, ScreenToClient(GetMouseCursorPos));
end;

procedure TcxRadioButton.UpdateState(Button: TcxMouseButton; Shift: TShiftState;
  const P: TPoint);
begin
  if not Enabled then
    State := rbsDisabled
  else
    if (csDesigning in ComponentState) then
      State := rbsNormal
    else
      if GetCaptureControl = Self then // VCL only
        if PtInRect(GetControlRect(Self), P) then
          State := rbsPressed
        else
          State := rbsHot
      else
        if PtInRect(GetControlRect(Self), P) then
        begin
            if cxShiftStateMoveOnly(Shift) then
              State := rbsHot
            else
              State := rbsNormal
        end
        else
          State := rbsNormal;
end;

// IcxMouseTrackingCaller
procedure TcxRadioButton.MouseTrackingCallerMouseLeave;
begin
  MouseLeave(nil);
end;

// IcxLookAndFeelContainer
function TcxRadioButton.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TcxRadioButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
begin
  FCanvas.Lock;
  FCanvas.Canvas.Handle := DrawItemStruct.hDC;
  Paint(DrawItemStruct.itemAction = ODA_FOCUS); // SC bug B19151
  FCanvas.Canvas.Handle := 0;
  FCanvas.Unlock;
end;

function TcxRadioButton.GetBiDiModeDependentAlignment: TLeftRight;
begin
  Result := Alignment;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertAlignment(Result);
end;

procedure TcxRadioButton.DrawRadioButton(ACanvas: TcxCanvas; AState: TcxRadioButtonState);

  function GetBackgroundColor: TColor;
  begin
    if LookAndFeel.SkinPainter <> nil then
      Result := clNone
    else
      if IsTransparentBackground then
        Result := clDefault
      else
        Result := Color;
  end;

const
  ButtonStateMap: array [TcxRadioButtonState] of TcxButtonState = (
    cxbsDisabled, cxbsHot, cxbsNormal, cxbsPressed
  );
var
  APainter: TcxCustomLookAndFeelPainter;
  AButtonRect: TRect;
begin
  APainter := LookAndFeel.GetAvailablePainter(totButton);
  AButtonRect := ButtonRect;
  APainter.DrawScaledRadioButton(ACanvas, AButtonRect.Left, AButtonRect.Top,
    ButtonStateMap[AState], Checked, Focused, GetBackgroundColor, ScaleFactor,
    csDesigning in ComponentState);
end;

function TcxRadioButton.GetButtonRect: TRect;
begin
  Result := GetRadioButtonRect(LookAndFeel.GetAvailablePainter(totButton).ScaledRadioButtonSize(ScaleFactor), IsNativeStyle);
end;

function TcxRadioButton.GetRadioButtonRect(
  const ARadioButtonSize: TSize; ANativeStyle: Boolean): TRect;
begin
  Result.Top := (Height - ARadioButtonSize.cy) div 2;
  Result.Bottom := Result.Top + ARadioButtonSize.cy;
  if GetBiDiModeDependentAlignment = taRightJustify then
  begin
    if ANativeStyle or IsInplace then
      Result.Left := 0
    else
      Result.Left := 1;

    Result.Right := Result.Left + ARadioButtonSize.cx;
  end
  else
  begin
    Result.Right := Width;
    Result.Left := Result.Right - ARadioButtonSize.cx;
  end;
end;

function TcxRadioButton.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

procedure TcxRadioButton.SetRadioButtonAutoSize(AValue: Boolean);
begin
  if FAutoSize <> AValue then
  begin
    FAutoSize := AValue;
    if AValue then
      AdjustSize;
  end;
end;

procedure TcxRadioButton.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxRadioButton.SetPopupMenu(Value: TComponent);
begin
  if not IsPopupMenu(Value) then
    Value := nil;
  if FPopupMenu <> Value then
  begin
    cxRemoveFreeNotification(Self, FPopupMenu);
    FPopupMenu := Value;
    cxAddFreeNotification(Self, FPopupMenu);
  end;
end;

procedure TcxRadioButton.SetShowEndEllipsis(Value: Boolean);
begin
  if FShowEndEllipsis <> Value then
  begin
    FShowEndEllipsis := Value;
    Invalidate;
  end;
end;

procedure TcxRadioButton.SetState(Value: TcxRadioButtonState);
var
  APrevState: TcxRadioButtonState;
begin
  if Value <> FState then
  begin
    APrevState := State;
    FState := Value;
    CheckStartFading(APrevState, State);
    InvalidateRadioButton;
  end;
end;

procedure TcxRadioButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TcxRadioButton.WMContextMenu(var Message: TWMContextMenu);
var
  AHandled: Boolean;
  P, P1: TPoint;
begin
  if Message.Result <> 0 then
    Exit;
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  P := SmallPointToPoint(Message.Pos);
  if (P.X = -1) and (P.Y = -1) then
    P1 := P
  else
  begin
    P1 := ScreenToClient(P);
    if not PtInRect(ClientRect, P1) then
    begin
      inherited;
      Exit;
    end;
  end;

  AHandled := False;
  DoContextPopup(P1, AHandled);
  Message.Result := Ord(AHandled);
  if not AHandled then
    inherited;
end;

procedure TcxRadioButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TcxRadioButton.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not (csDestroying in ComponentState) and IsTransparentBackground then
    Invalidate;
end;

procedure TcxRadioButton.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  ASize: TSize;
begin
  if not (csDestroying in ComponentState) then
  begin
    ASize := cxSize(Message.WindowPos.cx, Message.WindowPos.cy);
    inherited;
    if (Align = alRight) and (ASize.cx <> Message.WindowPos.cx) then
      Message.WindowPos.x := Message.WindowPos.x + ASize.cx - Message.WindowPos.cx;
    if (Align = alBottom) and (ASize.cy <> Message.WindowPos.cy) then
      Message.WindowPos.y := Message.WindowPos.y + ASize.cy - Message.WindowPos.cy;
  end;
end;

procedure TcxRadioButton.BMSetCheck(var Message: TMessage);
begin
  inherited;
  InvalidateRadioButton;
end;

procedure TcxRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  EnabledChanged;
end;

procedure TcxRadioButton.CMFontChanged(var Message: TMessage);
begin
  if AutoSize then
    AdjustSize;
  inherited;
end;

procedure TcxRadioButton.CMHintShow(var Message: TCMHintShow);
begin
  Message.HintInfo^.CursorRect := ClientRect;
end;

procedure TcxRadioButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxRadioButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxRadioButton.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TcxRadioButton.CMTextChanged(var Message: TMessage);
begin
  if AutoSize then
    AdjustSize;
  inherited;
end;

procedure TcxRadioButton.CNDrawItem(var Message: TWMDrawItem);
begin
  if not (csDestroying in ComponentState) then
    DrawItem(Message.DrawItemStruct^);
end;

procedure TcxRadioButton.CNKeyDown(var Message: TWMKeyDown);
begin
  if IsPopupMenuShortCut(PopupMenu, Message) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxRadioButton.CNMeasureItem(var Message: TWMMeasureItem);
var
  ATempVar: TMeasureItemStruct;
begin
  ATempVar := Message.MeasureItemStruct^;
  ATempVar.itemWidth := Width;
  ATempVar.itemHeight := Height;
  Message.MeasureItemStruct^ := ATempVar;
end;

procedure TcxRadioButton.CNSysKeyDown(var Message: TWMSysKeyDown);
begin
  if IsPopupMenuShortCut(PopupMenu, Message) then
    Message.Result := 1
  else
    inherited;
end;

{  TcxRadioGroupButtonViewInfo  }

function TcxRadioGroupButtonViewInfo.GetGlyphRect(
  const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect;

  procedure CorrectRadioRect(var ACheckRect: TRect);
  begin
    if AIsPaintCopy and not Data.NativeStyle then
      OffsetRect(ACheckRect, 1, 0);
  end;

begin
  Result := inherited GetGlyphRect(AGlyphSize, AAlignment, AIsPaintCopy);
  CorrectRadioRect(Result);
end;

{ TcxCustomRadioGroupViewInfo }

constructor TcxCustomRadioGroupViewInfo.Create;
begin
  inherited Create;
  PrepareRadioButtonImageList(ScaleFactor);
end;

procedure TcxCustomRadioGroupViewInfo.DrawButtonCaption(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);

  procedure CorrectTextRect(var R: TRect);
  const
    ANativeStyleTextRectCorrection: TRect = (Left: 3; Top: -1; Right: 0; Bottom: -1);
    ATextRectCorrection: TRect = (Left: 5; Top: 0; Right: 1; Bottom: 0);
  var
    ATextRectRealCorrection: TRect;
  begin
    if AButtonViewInfo.Data.NativeStyle then
      ATextRectRealCorrection := ANativeStyleTextRectCorrection
    else
      ATextRectRealCorrection := ATextRectCorrection;
    if UseRightToLeftAlignment then
      ATextRectRealCorrection := TdxRightToLeftLayoutConverter.ConvertOffsets(ATextRectRealCorrection);
    R := cxRectContent(R, ScaleFactor.Apply(ATextRectRealCorrection));
    if (Edit <> nil) and Edit.IsDBEditPaintCopyDrawing then
      OffsetRect(R, 0, -1);
  end;

var
  R: TRect;
begin
  ACanvas.Font := Font;
  ACanvas.Font.Color := TextColor;
  PrepareCanvasFont(ACanvas.Canvas);
  R := AButtonViewInfo.Bounds;
  if Alignment = taRightJustify then
    R.Left := AGlyphRect.Right
  else
    R.Right := AGlyphRect.Left;
  ACanvas.Brush.Style := bsClear;
  CorrectTextRect(R);
  ACanvas.DrawText(AButtonViewInfo.Caption, R, DrawTextFlags);
  ACanvas.Brush.Style := bsSolid;
  if not IsInplace and Focused then
  begin
    ACanvas.TextExtent(AButtonViewInfo.Caption, R, DrawTextFlags);
    InflateRect(R, 1, 1);
    Inc(R.Bottom);
    ACanvas.Brush.Color := BackgroundColor;
    TCanvasAccess(ACanvas.Canvas).RequiredState([csFontValid]);
    ACanvas.Canvas.DrawFocusRect(R);
  end;
end;

procedure TcxCustomRadioGroupViewInfo.DrawButtonGlyph(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);

  function GetBackgroundColor: TColor;
  begin
    if UseSkins and not IsInplace then
      Result := clNone
    else
      if IsBackgroundTransparent then
        Result := clDefault
      else
        Result := BackgroundColor;
  end;

const
  LookAndFeelKindMap: array [TcxEditButtonStyle] of TcxLookAndFeelKind =
    (lfStandard, lfStandard, lfFlat, lfStandard, lfStandard, lfUltraFlat, lfOffice11);
  ButtonStateMap: array [TcxEditButtonState] of TcxButtonState =
    (cxbsDisabled, cxbsNormal, cxbsPressed, cxbsHot);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(AButtonViewInfo.Bounds);
    if UseSkins then
      APainter := Painter
    else
      APainter := GetPainterClass(AButtonViewInfo.Data.NativeStyle, LookAndFeelKindMap[AButtonViewInfo.Data.Style]);

    APainter.DrawScaledRadioButton(ACanvas, AGlyphRect.Left, AGlyphRect.Top, ButtonStateMap[AButtonViewInfo.Data.State],
      ItemIndex = AButtonViewInfo.ButtonIndex, False, GetBackgroundColor, ScaleFactor, IsDesigning);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxCustomRadioGroupViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxRadioGroupButtonViewInfo;
end;

function TcxCustomRadioGroupViewInfo.IsButtonGlyphTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean;
begin
  Result := IsBackgroundTransparent or
    AButtonViewInfo.Data.NativeStyle and
      IsThemeBackgroundPartiallyTransparent(ThemeHandle, BP_RADIOBUTTON, AButtonViewInfo.Data.NativeState);
end;

function TcxCustomRadioGroupViewInfo.GetEdit: TcxCustomRadioGroup;
begin
  Result := TcxCustomRadioGroup(FEdit);
end;

function TcxCustomRadioGroupViewInfo.ThemeHandle: TdxTheme;
begin
  Result := OpenTheme(totButton);
end;

{ TcxCustomRadioGroupViewData }

procedure TcxCustomRadioGroupViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
begin
  if UseRightToLeftAlignment then
    TcxCustomRadioGroupViewInfo(AViewInfo).Alignment := taLeftJustify
  else
    TcxCustomRadioGroupViewInfo(AViewInfo).Alignment := taRightJustify;
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TcxCustomRadioGroupViewInfo(AViewInfo).GlyphSize :=
    Style.LookAndFeel.GetAvailablePainter(totButton).ScaledRadioButtonSize(ScaleFactor);
  AViewInfo.BackgroundColor := Style.Color;
end;

procedure TcxCustomRadioGroupViewData.CalculateButtonNativePartInfo(
  ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo);
const
  ButtonStateMap: array [Boolean, TcxEditButtonState] of Integer = (
    (RBS_UNCHECKEDDISABLED, RBS_UNCHECKEDNORMAL, RBS_UNCHECKEDPRESSED, RBS_UNCHECKEDHOT),
    (RBS_CHECKEDDISABLED, RBS_CHECKEDNORMAL, RBS_CHECKEDPRESSED, RBS_CHECKEDHOT)
  );
begin
  if not AButtonViewInfo.Data.NativeStyle then
    Exit;
  AButtonViewInfo.Data.NativePart := BP_RADIOBUTTON;
  AButtonViewInfo.Data.NativeState := ButtonStateMap[
    AButtonViewInfo.ButtonIndex = TcxCustomRadioGroupViewInfo(AButtonViewInfo.EditViewInfo).ItemIndex,
    AButtonViewInfo.Data.State];
end;

procedure TcxCustomRadioGroupViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  if PreviewMode then
    TcxCustomRadioGroupViewInfo(AViewInfo).ItemIndex := 0
  else
    TcxCustomRadioGroupViewInfo(AViewInfo).ItemIndex :=
      Properties.GetRadioGroupItemIndex(AEditValue);
  if epoAutoHeight in PaintOptions then
    Include(AViewInfo.PaintOptions, epoAutoHeight);
end;

function TcxCustomRadioGroupViewData.GetButtonNativeTheme(
  AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme;
begin
  Result := OpenTheme(totButton);
end;

procedure TcxCustomRadioGroupViewData.GetEditMetrics(AAutoHeight: Boolean;
  ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics);
const
  AColumnWidthCorrectionA: array [Boolean] of Integer = (7, 5);
  AAutoHeightColumnWidthCorrectionA: array [Boolean] of Integer = (3, 1);
var
  ANativeStyle: Boolean;
begin
  AMetrics.ClientLeftBoundCorrection := ScaleFactor.Apply(6 - 5 * Integer(IsInplace));
  AMetrics.ClientWidthCorrection := ScaleFactor.Apply(4 * Integer(IsInplace) - 6);
  AMetrics.ColumnOffset := 0;
  if ACanvas = nil then
    Exit;

  ANativeStyle := IsButtonNativeStyle(Style.LookAndFeel);
  AMetrics.ButtonSize := Style.LookAndFeel.GetAvailablePainter(totButton).ScaledRadioButtonSize(ScaleFactor);
  AMetrics.ColumnWidthCorrection := ScaleFactor.Apply(AColumnWidthCorrectionA[ANativeStyle]);
  AMetrics.AutoHeightColumnWidthCorrection :=
    ScaleFactor.Apply(AAutoHeightColumnWidthCorrectionA[ANativeStyle]);
  AMetrics.WidthCorrection := ScaleFactor.Apply(6 - 5 * Integer(IsInplace));
  AMetrics.AutoHeightWidthCorrection := ScaleFactor.Apply(Integer(IsInplace) - 6);
end;

function TcxCustomRadioGroupViewData.GetProperties: TcxCustomRadioGroupProperties;
begin
  Result := TcxCustomRadioGroupProperties(FProperties);
end;

{ TcxRadioGroupItem }

constructor TcxRadioGroupItem.Create(Collection: TCollection);
begin
  FValue := Null;
  inherited Create(Collection);
end;

procedure TcxRadioGroupItem.Assign(Source: TPersistent);
begin
  if Source is TcxRadioGroupItem then
    Value := TcxRadioGroupItem(Source).Value;
  inherited Assign(Source);
end;

function TcxRadioGroupItem.IsValueStored: Boolean;
begin
  Result := not VarIsNull(FValue);
end;

procedure TcxRadioGroupItem.SetValue(const Value: TcxEditValue);
begin
  if not InternalVarEqualsExact(Value, FValue) then
  begin
    FValue := Value;
    if Assigned(Collection) then
      TcxRadioGroupItems(Collection).InternalNotify(Self, -1, copChanged);
  end;
end;

{ TcxRadioGroupItems }

function TcxRadioGroupItems.Add: TcxRadioGroupItem;
begin
  Result := TcxRadioGroupItem(inherited Add);
end;

function TcxRadioGroupItems.GetItem(Index: Integer): TcxRadioGroupItem;
begin
  Result := TcxRadioGroupItem(inherited Items[Index]);
end;

procedure TcxRadioGroupItems.SetItem(Index: Integer; Value: TcxRadioGroupItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxCustomRadioGroupProperties }

constructor TcxCustomRadioGroupProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDefaultCaption := cxGetResourceString(@cxSRadioGroupDefaultCaption);
  FDefaultValue := Null;
end;

function TcxCustomRadioGroupProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

function TcxCustomRadioGroupProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
begin
  Result := GetRadioGroupItemIndex(AEditValue1) = GetRadioGroupItemIndex(AEditValue2);
end;

class function TcxCustomRadioGroupProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxRadioGroup;
end;

function TcxCustomRadioGroupProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  AItemIndex: Integer;
begin
  AItemIndex := GetRadioGroupItemIndex(AEditValue);
  if AItemIndex = -1 then
    Result := FDefaultCaption
  else
    Result := Items[AItemIndex].Caption;
end;

function TcxCustomRadioGroupProperties.GetRadioGroupItemIndex(
  const AEditValue: TcxEditValue): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if VarEquals(AEditValue, GetButtonValue(I)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TcxCustomRadioGroupProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

function TcxCustomRadioGroupProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations + [esoHorzAlignment, esoSortingByDisplayText];
end;

class function TcxCustomRadioGroupProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomRadioGroupViewInfo;
end;

function TcxCustomRadioGroupProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

procedure TcxCustomRadioGroupProperties.PrepareDisplayValue(const AEditValue:
  TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  DisplayValue := GetRadioGroupItemIndex(AEditValue);
end;

function TcxCustomRadioGroupProperties.CreateItems: TcxButtonGroupItems;
begin
  Result := TcxRadioGroupItems.Create(Self, TcxRadioGroupItem);
end;

procedure TcxCustomRadioGroupProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomRadioGroupProperties then
    with TcxCustomRadioGroupProperties(AProperties) do
    begin
      Self.DefaultCaption := DefaultCaption;
      Self.DefaultValue := DefaultValue;
    end;
end;

function TcxCustomRadioGroupProperties.GetColumnCount: Integer;
begin
  Result := Columns;
  if Result > Items.Count then
    Result := Items.Count;
  if Result = 0 then
    Result := 1;
end;

class function TcxCustomRadioGroupProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomRadioGroupViewData;
end;

function TcxCustomRadioGroupProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

function TcxCustomRadioGroupProperties.GetButtonValue(AIndex: Integer): TcxEditValue;
var
  AItem: TcxRadioGroupItem;
begin
  if AIndex = -1 then
    Result := DefaultValue
  else
  begin
    AItem := Items[AIndex];
    Result := AItem.Value;
    if VarIsNull(Result) then
      if AItem.Caption <> '' then
        Result := Items[AIndex].Caption
      else
        Result := AIndex;
  end;
end;

function TcxCustomRadioGroupProperties.GetItems: TcxRadioGroupItems;
begin
  Result := TcxRadioGroupItems(inherited Items);
end;

function TcxCustomRadioGroupProperties.IsDefaultCaptionStored: Boolean;
begin
  Result := not InternalCompareString(FDefaultCaption,
    cxGetResourceString(@cxSRadioGroupDefaultCaption), True);
end;

function TcxCustomRadioGroupProperties.IsDefaultValueStored: Boolean;
begin
  Result := not VarIsNull(FDefaultValue);
end;

procedure TcxCustomRadioGroupProperties.SetDefaultValue(const Value: TcxEditValue);
begin
  if not InternalVarEqualsExact(Value, FDefaultValue) then
  begin
    FDefaultValue := Value;
    Changed;
  end;
end;

procedure TcxCustomRadioGroupProperties.SetItems(Value: TcxRadioGroupItems);
begin
  inherited Items.Assign(Value);
end;

{ TcxCustomRadioGroupButton }

constructor TcxCustomRadioGroupButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with TcxCustomRadioGroup(AOwner) do
  begin
    InternalButtons.Add(Self);
    Self.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
  end;
end;

destructor TcxCustomRadioGroupButton.Destroy;
begin
  RadioGroup.InternalButtons.Remove(Self);
  inherited Destroy;
end;

function TcxCustomRadioGroupButton.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    RadioGroup.DataBinding.ExecuteAction(Action);
end;

function TcxCustomRadioGroupButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    RadioGroup.DataBinding.UpdateAction(Action);
end;

function TcxCustomRadioGroupButton.CanFocus: Boolean;
begin
  Result := RadioGroup.CanFocus;
end;

procedure TcxCustomRadioGroupButton.DefaultHandler(var Message);
begin
  if not RadioGroup.InnerControlDefaultHandler(TMessage(Message)) then
    inherited DefaultHandler(Message);
end;

procedure TcxCustomRadioGroupButton.CorrectTextRect(var R: TRect; ANativeStyle: Boolean);
begin
  inherited CorrectTextRect(R, ANativeStyle);
end;

procedure TcxCustomRadioGroupButton.DoEnter;
begin

  with RadioGroup do
  begin
    ShortRefreshContainer(False);
    if not Checked and not IsInplace and not FFocusingByMouse and DoEditing and not ClicksDisabled then
      Checked := True;
  end;
end;

procedure TcxCustomRadioGroupButton.DoExit;
begin
  inherited DoExit;
  RadioGroup.ShortRefreshContainer(False);
end;

procedure TcxCustomRadioGroupButton.DrawBackground(ACanvas: TcxCanvas);
var
  APrevWindowOrg: TPoint;
begin
  if RadioGroup.ViewInfo.IsCustomBackground then
  begin
    OffsetWindowOrgEx(ACanvas.Handle, Left, Top, APrevWindowOrg);
    try
      RadioGroup.ViewInfo.DrawBackground(ACanvas);
    finally
      SetWindowOrgEx(ACanvas.Handle, APrevWindowOrg.X, APrevWindowOrg.Y, nil);
    end;
  end
  else
    inherited DrawBackground(ACanvas);
end;

function TcxCustomRadioGroupButton.IsInplace: Boolean;
begin
  Result := RadioGroup.IsInplace;
end;

function TcxCustomRadioGroupButton.IsNativeBackground: Boolean;
begin
  Result := RadioGroup.IsNativeBackground;
end;

function TcxCustomRadioGroupButton.IsNativeStyle: Boolean;
begin
  Result := RadioGroup.IsButtonNativeStyle;
end;

function TcxCustomRadioGroupButton.IsTransparent: Boolean;
begin
  Result := RadioGroup.IsTransparent;
end;

function TcxCustomRadioGroupButton.IsTransparentBackground: Boolean;
begin
  Result := inherited IsTransparentBackground or
    RadioGroup.ViewInfo.IsCustomBackground or (LookAndFeel.SkinPainter <> nil);
end;

procedure TcxCustomRadioGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ((Key = #8) or (Key = ' ')) and not RadioGroup.CanModify then
    Key := #0;
end;

procedure TcxCustomRadioGroupButton.PrepareBackground(ACanvas: TcxCanvas);
begin
  if LookAndFeel.SkinPainter <> nil then
    cxDrawTransparentControlBackground(Self, ACanvas, GetControlRect(Self), False)
  else
    inherited PrepareBackground(ACanvas);
end;

procedure TcxCustomRadioGroupButton.SetChecked(Value: Boolean);
begin
  if Value = Checked then
    Exit;
  ClicksDisabled := True;
  try
    inherited SetChecked(Value);
  finally
    ClicksDisabled := False;
  end;
  if Value and not FInternalSettingChecked then
    RadioGroup.ButtonChecked(Self);
end;

procedure TcxCustomRadioGroupButton.WndProc(var Message: TMessage);
begin
  if RadioGroup.InnerControlMenuHandler(Message) then
    Exit;
  if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
    (RadioGroup.DragMode = dmAutomatic) and not RadioGroup.IsDesigning then
      RadioGroup.BeginAutoDrag
  else
  begin
    if Message.Msg = WM_LBUTTONDOWN then
      FFocusingByMouse := True;
    inherited WndProc(Message);
    if Message.Msg = WM_LBUTTONDOWN then
      FFocusingByMouse := False;
  end;
end;

// IcxContainerInnerControl
function TcxCustomRadioGroupButton.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomRadioGroupButton.GetControlContainer: TcxContainer;
begin
  Result := RadioGroup;
end;

procedure TcxCustomRadioGroupButton.InternalSetChecked(AValue: Boolean);
begin
  if FInternalSettingChecked then
    Exit;
  FInternalSettingChecked := True;
  try
    Checked := AValue;
  finally
    FInternalSettingChecked := False;
  end;
end;

function TcxCustomRadioGroupButton.GetRadioGroup: TcxCustomRadioGroup;
begin
  Result := TcxCustomRadioGroup(Owner);
end;

procedure TcxCustomRadioGroupButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if RadioGroup.TabsNeeded and (GetKeyState(VK_CONTROL) >= 0) then
    Message.Result := Message.Result or DLGC_WANTTAB;
  if RadioGroup.IsInplace then
    Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TcxCustomRadioGroupButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDestroying in ComponentState) and (Message.FocusedWnd <> RadioGroup.Handle) then
    RadioGroup.FocusChanged;
end;

procedure TcxCustomRadioGroupButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDestroying in ComponentState) and (Message.FocusedWnd <> RadioGroup.Handle) then
    RadioGroup.FocusChanged;
end;

procedure TcxCustomRadioGroupButton.CNCommand(var Message: TWMCommand);
begin
  if FIsClickLocked then
    Exit;
  FIsClickLocked := True;
  try
    try
      with RadioGroup do
        if ((Message.NotifyCode = BN_CLICKED) or (Message.NotifyCode = BN_DOUBLECLICKED)) and
          (Checked or CanModify and DoEditing) then
            inherited;
    except
      Application.HandleException(Self);
    end;
  finally
    FIsClickLocked := False;
  end;
end;

{ TcxCustomRadioGroup }

procedure TcxCustomRadioGroup.Clear;
begin
  ItemIndex := -1;
end;

procedure TcxCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
end;

class function TcxCustomRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomRadioGroupProperties;
end;

procedure TcxCustomRadioGroup.GetTabOrderList(List: TList);
var
  I: Integer;
begin
  inherited GetTabOrderList(List);
  List.Remove(Self);
  if (TabStop or Focused) and (ItemIndex <> -1) then
    for I := 0 to InternalButtons.Count - 1 do
      if Buttons[I].Enabled then
        List.Add(Buttons[I]);
end;

procedure TcxCustomRadioGroup.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  EditValue := ActiveProperties.GetButtonValue(ADisplayValue);
end;

procedure TcxCustomRadioGroup.SetFocus;
var
  ACheckedIndex: Integer;
begin
  ACheckedIndex := GetCheckedIndex;
  if (ACheckedIndex <> -1) and Buttons[ACheckedIndex].CanFocus then
    Buttons[ACheckedIndex].SetFocus
  else
    inherited SetFocus;
end;

procedure TcxCustomRadioGroup.CursorChanged;
var
  I: Integer;
begin
  inherited CursorChanged;
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].Cursor := Cursor;
end;

procedure TcxCustomRadioGroup.DoSetFocusWhenActivate;
var
  ACheckedButtonIndex: Integer;
begin
  if InternalButtons.Count = 0 then
    Exit;
  ACheckedButtonIndex := ItemIndex;
  if ACheckedButtonIndex = -1 then
    ACheckedButtonIndex := 0;
  if Buttons[ACheckedButtonIndex].CanFocus then
    Buttons[ACheckedButtonIndex].SetFocus;
end;

function TcxCustomRadioGroup.GetButtonDC(AButtonIndex: Integer): THandle;
begin
  Result := Buttons[AButtonIndex].Canvas.Handle;
end;

procedure TcxCustomRadioGroup.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle + [csParentBackground, csCaptureMouse, csClickEvents, csSetCaption,
    csDoubleClicks, csReplicatable];
  FLoadedItemIndex := -1;
  Width := 185;
  Height := 105;
end;

procedure TcxCustomRadioGroup.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
begin
  SetInternalValues(Value, AValidateEditValue, False);
end;

function TcxCustomRadioGroup.IsContainerFocused: Boolean;
var
  AIsButtonFocused: Boolean;
  I: Integer;
begin
  AIsButtonFocused := False;
  for I := 0 to ActiveProperties.Items.Count - 1 do
    if Buttons[I].Focused then
    begin
      AIsButtonFocused := True;
      Break;
    end;
  if AIsButtonFocused then
    Result := False
  else
    Result := inherited Focused;
end;

function TcxCustomRadioGroup.IsInternalControl(AControl: TControl): Boolean;
var
  I: Integer;
begin
  Result := AControl <> nil;
  if Result then
  begin
    Result := inherited IsInternalControl(AControl);
    if not Result then
      for I := 0 to InternalButtons.Count - 1 do
        if AControl = InternalButtons[I] then
        begin
          Result := True;
          Exit;
        end;
  end;
end;

procedure TcxCustomRadioGroup.SetDragMode(Value: TDragMode);
var
  I: Integer;
begin
  inherited SetDragMode(Value);
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].DragMode := Value;
end;

procedure TcxCustomRadioGroup.SetInternalValues(const AEditValue: TcxEditValue;
  AValidateEditValue, AFromButtonChecked: Boolean);

  procedure FocusButton(AIndex: Integer);
  begin
    if Focused and (GetFocus <> Handle) then
      Buttons[AIndex].SetFocus;
  end;

  procedure SetButtonCheck(AItemIndex: Integer);
  begin
    if AFromButtonChecked then
      FocusButton(AItemIndex)
    else
    begin
      if AItemIndex < 0 then
        Buttons[ItemIndex].InternalSetChecked(False)
      else
      begin
        Buttons[AItemIndex].InternalSetChecked(True);
        FocusButton(AItemIndex);
      end;
    end;
  end;

var
  AItemIndex: Integer;
begin
  if IsLoading then
    inherited InternalSetEditValue(AEditValue, AValidateEditValue)
  else
  begin
    LockChangeEvents(True);
    try
      inherited InternalSetEditValue(AEditValue, AValidateEditValue);
      AItemIndex := ActiveProperties.GetRadioGroupItemIndex(AEditValue);
      if AFromButtonChecked or (GetCheckedIndex <> AItemIndex) then
      begin
        SetButtonCheck(AItemIndex);
        DoClick;
        DoChange;
      end;
      if not IsUserAction then
        EditModified := False;
    finally
      LockChangeEvents(False);
    end;
    ShortRefreshContainer(False);
  end;
end;

procedure TcxCustomRadioGroup.SynchronizeButtonsStyle;
const
  AButtonLookAndFeelKinds: array [TcxEditButtonStyle] of TcxLookAndFeelKind =
    (lfStandard, lfStandard, lfFlat, lfStandard, lfStandard,
    lfUltraFlat, lfOffice11);
var
  I: Integer;
begin
  inherited SynchronizeButtonsStyle;
  if Length(ViewInfo.ButtonsInfo) > 0 then
    for I := 0 to InternalButtons.Count - 1 do
    begin
      Buttons[I].LookAndFeel.Kind := AButtonLookAndFeelKinds[ViewInfo.ButtonsInfo[0].Data.Style];
      Buttons[I].Transparent := Transparent; // Repaint buttons
    end;
end;

procedure TcxCustomRadioGroup.Resize;
begin
  inherited Resize;
  if IsDesigning and IsNativeBackground then
    InvalidateRect(GetControlRect(Self), True);
end;

procedure TcxCustomRadioGroup.ParentBackgroundChanged;
var
  I: Integer;
begin
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].ParentBackground := ParentBackground;
end;

procedure TcxCustomRadioGroup.SetDragKind(Value: TDragKind);
var
  I: Integer;
begin
  inherited SetDragKind(Value);
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].DragKind := Value;
end;

procedure TcxCustomRadioGroup.ArrangeButtons;
var
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  I: Integer;
begin
  inherited ArrangeButtons;
  for I := 0 to InternalButtons.Count - 1 do
  begin
    AButtonViewInfo := TcxGroupBoxButtonViewInfo(ViewInfo.ButtonsInfo[I]);
    Buttons[I].FColumn := AButtonViewInfo.Column;
    Buttons[I].FRow := AButtonViewInfo.Row;
  end;
end;

function TcxCustomRadioGroup.GetButtonInstance: TWinControl;
begin
  Result := TcxCustomRadioGroupButton.Create(Self);
end;

procedure TcxCustomRadioGroup.InternalUpdateButtons;
var
  I: Integer;
  AItemIndex: Integer;
begin
  AItemIndex := ItemIndex;
  inherited;
  if GetCheckedIndex <> AItemIndex then
    ItemIndex := AItemIndex;

  for I := 0 to InternalButtons.Count - 1 do
  begin
    Buttons[I].Caption := ActiveProperties.Items[I].Caption;
    Buttons[I].ShowEndEllipsis := ActiveProperties.ShowEndEllipsis;
    Buttons[I].WordWrap := ActiveProperties.WordWrap;
    Buttons[I].OnClick := ButtonClickHandler;
    InvalidateControl(Buttons[I], True, False);
  end;
end;

procedure TcxCustomRadioGroup.UpdateValues;
begin
  UpdateButtons;
  LockChangeEvents(True);
  LockClick(True);
  try
    if not IsDBEdit then
    begin
      FEditValue := ActiveProperties.DefaultValue;
      ItemIndex := FLoadedItemIndex;
    end
    else
      InternalSetEditValue(EditValue, False);
  finally
    LockClick(False);
    LockChangeEvents(False, False);
  end;
end;

function TcxCustomRadioGroup.IsLoading: Boolean;
begin
  Result := [csReading, csLoading, csUpdating] * ComponentState <> [];
end;

procedure TcxCustomRadioGroup.Loaded;
begin
  inherited;
  UpdateValues;
end;

procedure TcxCustomRadioGroup.Updated;
begin
  inherited;
  UpdateValues;
end;

procedure TcxCustomRadioGroup.ButtonChecked(AButton: TcxCustomRadioGroupButton);
var
  AEditValue: TcxEditValue;
begin
  LockChangeEvents(True);
  try
    BeginUserAction;
    try
      PrepareEditValue(InternalButtons.IndexOf(AButton), AEditValue, InternalFocused);
      SetInternalValues(AEditValue, True, True);
    finally
      EndUserAction;
    end;
    if Focused and ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      InternalPostEditValue;
  finally
    LockChangeEvents(False);
  end;
end;

function TcxCustomRadioGroup.GetCheckedIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to InternalButtons.Count - 1 do
    if Buttons[I].Checked then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomRadioGroup.GetButton(Index: Integer): TcxCustomRadioGroupButton;
begin
  Result := TcxCustomRadioGroupButton(InternalButtons[Index]);
end;

function TcxCustomRadioGroup.GetProperties: TcxCustomRadioGroupProperties;
begin
  Result := TcxCustomRadioGroupProperties(inherited Properties);
end;

function TcxCustomRadioGroup.GetActiveProperties: TcxCustomRadioGroupProperties;
begin
  Result := TcxCustomRadioGroupProperties(InternalGetActiveProperties);
end;

function TcxCustomRadioGroup.GetItemIndex: Integer;
begin
  if IsLoading then
    Result := FLoadedItemIndex
  else
    Result := GetCheckedIndex;
end;

function TcxCustomRadioGroup.GetViewInfo: TcxCustomRadioGroupViewInfo;
begin
  Result := TcxCustomRadioGroupViewInfo(FViewInfo);
end;

procedure TcxCustomRadioGroup.SetItemIndex(Value: Integer);

  procedure InternalUpdateValues;
  var
    AEditValue: TcxEditValue;
  begin
    if Value < -1 then
      Value := -1
    else
      if Value >= InternalButtons.Count then
        Value := InternalButtons.Count - 1;

    PrepareEditValue(Value, AEditValue, InternalFocused);
    SetInternalValues(AEditValue, True, False);
  end;

begin
  if IsLoading then
    FLoadedItemIndex := Value
  else
    InternalUpdateValues;
end;

procedure TcxCustomRadioGroup.SetProperties(Value: TcxCustomRadioGroupProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomRadioGroup.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SelectFirst;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TcxCustomRadioGroup.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  ShortRefreshContainer(False);
end;

procedure TcxCustomRadioGroup.ButtonClickHandler(Sender: TObject);
begin
  Click;
end;

{ TcxRadioGroup }

class function TcxRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRadioGroupProperties;
end;

function TcxRadioGroup.GetActiveProperties: TcxRadioGroupProperties;
begin
  Result := TcxRadioGroupProperties(InternalGetActiveProperties);
end;

function TcxRadioGroup.GetProperties: TcxRadioGroupProperties;
begin
  Result := TcxRadioGroupProperties(inherited Properties);
end;

procedure TcxRadioGroup.SetProperties(Value: TcxRadioGroupProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterRadioGroupHelper }

class procedure TcxFilterRadioGroupHelper.GetFilterValue(
  AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
  var V: Variant; var S: TCaption);
var
  AItemIndex: Integer;
begin
  AItemIndex := TcxComboBox(AEdit).ItemIndex;
  with TcxCustomRadioGroupProperties(AEditProperties) do
  begin
    if AItemIndex = -1 then
    begin
      V := DefaultValue;
      S := DefaultCaption;
    end
    else
    begin
      V := Items[AItemIndex].Value;
      S := Items[AItemIndex].Caption;
    end;
  end;
end;

class function TcxFilterRadioGroupHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then Result := Result + [fcoInList, fcoNotInList];
end;

class procedure TcxFilterRadioGroupHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
var
  ARadioGroupProperties: TcxCustomRadioGroupProperties;
  I: Integer;
begin
  ARadioGroupProperties := TcxCustomRadioGroupProperties(AEditProperties);
  with TcxComboBoxProperties(AProperties).Items do
  begin
    Clear;
    for I := 0 to ARadioGroupProperties.Items.Count - 1 do
      Add(ARadioGroupProperties.Items[I].Caption);
  end;
  TcxComboBoxProperties(AProperties).DropDownListStyle := lsFixedList;
  TcxComboBoxProperties(AProperties).IDefaultValuesProvider := nil;
  ClearPropertiesEvents(AProperties);
end;

class procedure TcxFilterRadioGroupHelper.SetFilterValue(
  AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
  AValue: Variant);
var
  V: TcxEditValue;
begin
  AEditProperties.PrepareDisplayValue(AValue, V, AEdit.Focused);
  TcxComboBox(AEdit).ItemIndex := V;
end;

class function TcxFilterRadioGroupHelper.UseDisplayValue: Boolean;
begin
  Result := True;
end;

initialization
  GetRegisteredEditProperties.Register(TcxRadioGroupProperties, scxSEditRepositoryRadioGroupItem);
  FilterEditsController.Register(TcxRadioGroupProperties, TcxFilterRadioGroupHelper);

finalization
  FilterEditsController.Unregister(TcxRadioGroupProperties, TcxFilterRadioGroupHelper);

end.
