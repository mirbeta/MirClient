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

unit cxSpinEdit;

{$I cxVer.inc}

interface

uses
  Types, Windows, Variants, Classes, Controls, Forms, Graphics, SysUtils, cxClasses,
  cxContainer, cxControls, cxDataStorage, cxDataUtils, cxEdit, cxGraphics, cxMaskEdit,
  cxTextEdit, cxVariants, cxLookAndFeelPainters, cxFilterControlUtils, dxUxTheme;

const
  cxSpinBackwardButtonIndex = 0;
  cxSpinForwardButtonIndex = 1;
  cxSpinFastBackwardButtonIndex = 2;
  cxSpinFastForwardButtonIndex = 3;

type
  TcxSpinBoundsCheckingKind = (bckDoNotExceed, bckExtendToBound, bckCircular);
  TcxSpinEditButtonsPosition = (sbpHorzLeftRight, sbpHorzRight, sbpVert);
  TcxSpinEditButton = (sebBackward, sebForward, sebFastBackward, sebFastForward);

  { TcxSpinEditViewInfo }

  TcxSpinEditViewInfo = class(TcxCustomTextEditViewInfo)
  protected
    procedure DrawHotFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); override;
    procedure DrawNativeButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); override;
    procedure DrawUltraFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      AIsOffice11Style: Boolean; var ARect: TRect; var AContentRect: TRect; out APenColor, ABrushColor: TColor); override;

    procedure DrawNativeButtonBackground(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect); override;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    ArrowSize: Integer;
    ButtonsPosition: TcxSpinEditButtonsPosition;
    DelimiterLine: array[0..1] of TPoint;

    procedure DrawButtonContent(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo;
      const AContentRect: TRect; APenColor, ABrushColor: TColor; ANeedOffsetContent: Boolean); override;
  end;

  { TcxSpinEditViewData }

  TcxSpinEditPressedState = (epsNone, epsDown, epsUp, epsFastDown, epsFastUp);
  TcxCustomSpinEditProperties = class;

  TcxSpinEditViewData = class(TcxCustomTextEditViewData)
  private
    function GetProperties: TcxCustomSpinEditProperties;
  protected
    function CanPressButton(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean; override;
    procedure CalculateButtonNativePartInfo(ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo); override;
    procedure CalculateButtonsBounds(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo;
      var ButtonsRect: TRect); virtual;
    function GetButtonNativeTheme(AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme; override;
    function IsButtonPressed(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean; override;
  public
    PressedState: TcxSpinEditPressedState;
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure CalculateButtonViewInfo(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo;
      AButtonVisibleIndex: Integer; var ButtonsRect: TRect); override;
    procedure CheckButtonsOnly(AViewInfo: TcxCustomEditViewInfo;
      APrevButtonsWidth, AButtonsWidth: Integer); override;
    procedure DoCalculateButtonViewInfos(ACanvas: TcxCanvas;
      var AButtonsRect: TRect; AViewInfo: TcxCustomEditViewInfo); override;
    function IgnoreButtonWhileStretching(
      AButtonVisibleIndex: Integer): Boolean; override;
    function IsButtonLeftAligned(AViewInfo: TcxCustomEditViewInfo;
      AButtonVisibleIndex: Integer): Boolean; override;
    property Properties: TcxCustomSpinEditProperties read GetProperties;
  end;

  { TcxSpinEditButtons }

  TcxSpinEditButtons = class(TPersistent)
  private
    FOwner: TPersistent;
    FPosition: TcxSpinEditButtonsPosition;
    FShowFastButtons: Boolean;
    FVisible: Boolean;
    function GetProperties: TcxCustomSpinEditProperties;
    procedure SetPosition(Value: TcxSpinEditButtonsPosition);
    procedure SetShowFastButtons(Value: Boolean);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Changed;
    property Properties: TcxCustomSpinEditProperties read GetProperties;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Position: TcxSpinEditButtonsPosition read FPosition
      write SetPosition default sbpVert;
    property ShowFastButtons: Boolean read FShowFastButtons
      write SetShowFastButtons default False;
    property Visible: Boolean read FVisible
      write SetVisible default True;
  end;

  { TcxSpinEditPropertiesValues }

  TcxSpinEditPropertiesValues = class(TcxTextEditPropertiesValues)
  private
    FValueType: Boolean;
    procedure SetValueType(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property ValueType: Boolean read FValueType write SetValueType stored False;
  end;

  { TcxCustomSpinEditProperties }

  TcxSpinEditValueType = (vtInt, vtFloat);

  TcxCustomSpinEdit = class;

  TcxSpinEditGetValueEvent = procedure(Sender: TObject; const AText: TCaption;
    out AValue: Variant; var ErrorText: TCaption; var Error: Boolean) of object;

  TcxCustomSpinEditProperties = class(TcxCustomMaskEditProperties)
  private
    FSpinButtons: TcxSpinEditButtons;
    FCanEdit: Boolean;
    FCircular: Boolean;
    FIncrement, FLargeIncrement: Double;
    FExceptionOnInvalidInput: Boolean;
    FUseCtrlIncrement: Boolean;
    FValueType: TcxSpinEditValueType;
    FOnGetValue: TcxSpinEditGetValueEvent;
    function DoubleAsValueType(AValue: Double;
      AValueType: TcxSpinEditValueType): Double;
    function GetAssignedValues: TcxSpinEditPropertiesValues;
    function GetValueType: TcxSpinEditValueType;
    function InternalTryTextToValue(S: string; out AValue: TcxEditValue): Boolean;
    function IsIncrementStored: Boolean;
    function IsLargeIncrementStored: Boolean;
    function IsValueTypeStored: Boolean;
    procedure ReadZeroIncrement(Reader: TReader);
    procedure ReadZeroLargeIncrement(Reader: TReader);
    procedure SetAssignedValues(AValue: TcxSpinEditPropertiesValues);
    procedure SetCircular(AValue: Boolean);
    procedure SetSpinButtons(AValue: TcxSpinEditButtons);
    procedure SetValueType(AValue: TcxSpinEditValueType);
    function TryExtractDisplayFormat(const S: string): string;
    function VarToCurrentValueType(AValue: TcxEditValue): TcxEditValue;
    procedure WriteZeroIncrement(Writer: TWriter);
    procedure WriteZeroLargeIncrement(Writer: TWriter);
  protected
    function DefaultFocusedDisplayValue: TcxEditValue; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure DoChanged; override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function IsEditValueNumeric: Boolean; override;

    function CheckValueBounds(const AValue: Variant): Variant; virtual;
    function ExtendValueUpToBound: Boolean; virtual;
    function GetBoundsCheckingKind: TcxSpinBoundsCheckingKind; virtual;
    function GetIncrement(AButton: TcxSpinEditButton): Double;
    function GetMaxMinValueForCurrentValueType(AMinValue: Boolean = True): TcxEditValue;
    function InternalMaxValue: TcxEditValue; virtual;
    function InternalMinValue: TcxEditValue; virtual;
    function IsDisplayValueNumeric: Boolean; virtual;
    function IsValueBoundsValid(AValue: Extended): Boolean;
    function PrepareValue(const AValue: TcxEditValue): Variant; virtual;
    function PreserveSelection: Boolean; virtual;
    function SetVariantType(const AValue: TcxEditValue): TcxEditValue;
    function TryTextToValue(S: string; out AValue: TcxEditValue): Boolean;

    property AssignedValues: TcxSpinEditPropertiesValues read GetAssignedValues write SetAssignedValues;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CreateViewData(AStyle: TcxCustomEditStyle;
      AIsInplace: Boolean; APreviewMode: Boolean = False): TcxCustomEditViewData; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsDisplayValueValid(var ADisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean;
      AEdit: TcxCustomEdit); override;
    // !!!
    property CanEdit: Boolean read FCanEdit write FCanEdit default True;
    property Circular: Boolean read FCircular write SetCircular default False;
    property ExceptionOnInvalidInput: Boolean read FExceptionOnInvalidInput write FExceptionOnInvalidInput default False;
    property Increment: Double read FIncrement write FIncrement stored IsIncrementStored;
    property LargeIncrement: Double read FLargeIncrement write FLargeIncrement stored IsLargeIncrementStored;
    property SpinButtons: TcxSpinEditButtons read FSpinButtons write SetSpinButtons;
    property UseCtrlIncrement: Boolean read FUseCtrlIncrement write FUseCtrlIncrement default False;
    property ValueType: TcxSpinEditValueType read GetValueType write SetValueType stored IsValueTypeStored;
    property OnGetValue: TcxSpinEditGetValueEvent read FOnGetValue write FOnGetValue;
  end;

  { TcxSpinEditProperties }

  TcxSpinEditProperties = class(TcxCustomSpinEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property CanEdit;
    property ClearKey;
    property DisplayFormat;
    property EchoMode;
    property EditFormat;
    property ExceptionOnInvalidInput;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediatePost;
    property Increment;
    property LargeIncrement;
    property MaxValue;
    property MinValue;
    property Nullstring;
    property PasswordChar;
    property ReadOnly;
    property SpinButtons;
    property UseCtrlIncrement;
    property UseDisplayFormatWhenEditing;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property ValueType;
    property OnChange;
    property OnEditValueChanged;
    property OnGetValue;
    property OnValidate;
  end;

  { TcxCustomSpinEdit }

  TcxCustomSpinEdit = class(TcxCustomMaskEdit)
  private
    FInternalValue: TcxEditValue;
    FIsCustomText: Boolean;
    FIsCustomTextAction: Boolean;
    FPressedState: TcxSpinEditPressedState;
    FTimer: TcxTimer;
    function GetProperties: TcxCustomSpinEditProperties;
    function GetActiveProperties: TcxCustomSpinEditProperties;
    procedure HandleTimer(Sender: TObject);
    function IsValueStored: Boolean;
    procedure SetPressedState(Value: TcxSpinEditPressedState);
    procedure SetProperties(Value: TcxCustomSpinEditProperties);
    procedure StopTracking;
  protected
    procedure ChangeHandler(Sender: TObject); override;
    procedure CheckEditorValueBounds; override;
    procedure DoButtonDown(AButtonVisibleIndex: Integer); override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeyPress(var Key: Char); override;
    procedure DoEditKeyUp(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    function GetNewValue(AButton: TcxSpinEditButton; out AValue: Variant): Boolean; virtual;
    procedure FocusChanged; override;
    procedure Initialize; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function InternalGetNotPublishedStyleValues: TcxEditStyleValues; override;
    function InternalIncrement(var AValue: Variant; AIncrement: Variant): Boolean; virtual;
    procedure InternalStoreEditValue(const Value: TcxEditValue); override;
    function IsEditValueStored: Boolean; override;
    function IsValidChar(AChar: Char): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PropertiesChanged(Sender: TObject); override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AIsMouseEvent: Boolean): Boolean; override;
    procedure SetInternalValue(AValue: TcxEditValue);
    procedure SynchronizeDisplayValue; override;

    procedure DoOnGetValue(const AText: TCaption; out AValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    function GetIncrement(AButton: TcxSpinEditButton): Double; virtual;
    function GetValue: Variant; virtual;
    function IncrementValueToStr(const AValue: TcxEditValue): string; virtual;
    function InternalPrepareEditValue(const ADisplayValue: TcxEditValue;
      ARaiseException: Boolean; out AEditValue: TcxEditValue; out AErrorText: TCaption): Boolean;
    function IsOnGetValueEventAssigned: Boolean;
    procedure SetValue(const Value: Variant); virtual;

    property InternalValue: TcxEditValue read FInternalValue;
    property PressedState: TcxSpinEditPressedState read FPressedState write SetPressedState;
    property Value: Variant read GetValue write SetValue stored IsValueStored;
  public
    destructor Destroy; override;
    procedure ClearSelection; override;
    procedure CutToClipboard; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function Increment(AButton: TcxSpinEditButton): Boolean; virtual;
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out AEditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomSpinEditProperties read GetActiveProperties;
    property Properties: TcxCustomSpinEditProperties read GetProperties write SetProperties;
  end;

  { TcxSpinEdit }

  TcxSpinEdit = class(TcxCustomSpinEdit)
  private
    function GetActiveProperties: TcxSpinEditProperties;
    function GetProperties: TcxSpinEditProperties;
    procedure SetProperties(Value: TcxSpinEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxSpinEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxSpinEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Value;
    property Visible;
    property DragCursor;
    property DragKind;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterSpinEditHelper }

  TcxFilterSpinEditHelper = class(TcxFilterMaskEditHelper)
  public
    class function EditPropertiesHasButtons: Boolean; override;
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType; override;
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
  Math, dxCore, cxGeometry, dxOffice11, dxThemeConsts, dxThemeManager,
  cxEditConsts, cxEditPaintUtils, cxEditUtils, cxFormats, cxDWMApi;

const
  cxSpinEditTimerInitialInterval = 400;
  cxSpinEditTimerInterval = 100;
  MaxIntValue = High(Int64);
  MinIntValue = Low(Int64);

type
  TcxSpinEditUltraFlatButtonPosition = (sufbpLeftMost, sufbpMiddle, sufbpRightMost,
    sufbpMiddleTop, sufbpMiddleBottom, sufbpRightTop, sufbpRightBottom, sufbpLeftBottom, sufbpLeftTop);
  TcxSpinEditFlatButtonPosition = (sfbpLeftSide, sfbpLeftSideRightMost,
    sfbpRightSide, sfbpRightSideLeftMost, sfbpMiddleTop, sfbpMiddleBottom,
    sfbpRightTop, sfbpRightBottom, sfbpLeftTop, sfbpLeftBottom, sfbpLeftMiddleBottom, sfbpLeftMiddleTop);

procedure DrawSpinEdit(ACanvas: TcxCanvas; AViewInfo: TcxSpinEditViewInfo);
var
  APrevClipRgn: TcxRegion;
begin
  DrawTextEdit(ACanvas, AViewInfo);
  with AViewInfo do
    if (Length(ButtonsInfo) > 0) and (ButtonsInfo[0].Data.NativePart <> TC_NONE) and
      (ButtonsPosition <> sbpHorzLeftRight) and not IsCustomDrawButton then
    begin
      APrevClipRgn := ACanvas.GetClipRegion;
      try
        ACanvas.IntersectClipRect(AViewInfo.BorderRect);
        InternalPolyLine(ACanvas, [DelimiterLine[0], DelimiterLine[1]], BackgroundColor, True);
      finally
        ACanvas.SetClipRegion(APrevClipRgn, roSet);
      end;
    end;
end;

procedure DrawSpinEditFlatButtonBorder(ACanvas: TcxCanvas;
  AButtonPosition: TcxSpinEditFlatButtonPosition; var R: TRect; AContentColor, ABorderColor: TColor);
begin
  with R do
    case AButtonPosition of
      sfbpLeftSide:
        begin
          DrawButtonBorder(ACanvas, R, [bBottom, bLeft, bTop], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bRight], AContentColor);
        end;
      sfbpLeftSideRightMost, sfbpRightSideLeftMost, sfbpRightTop:
        begin
          ACanvas.FrameRect(R, ABorderColor);
          InflateRect(R, -1, -1);
        end;
      sfbpRightSide, sfbpMiddleTop:
        begin
          DrawButtonBorder(ACanvas, R, [bTop, bRight, bBottom], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bLeft], AContentColor);
        end;
      sfbpMiddleBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bBottom, bRight], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bLeft, bTop], AContentColor);
        end;
      sfbpRightBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bBottom, bRight], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bTop], AContentColor);
        end;
      sfbpLeftBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bBottom, bRight], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bTop], AContentColor);
        end;
      sfbpLeftTop:
        begin
          ACanvas.FrameRect(R, ABorderColor);
          InflateRect(R, -1, -1);
        end;
      sfbpLeftMiddleBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bBottom], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bRight, bTop], AContentColor);
        end;
      sfbpLeftMiddleTop:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bTop, bBottom], ABorderColor);
          DrawButtonBorder(ACanvas, R, [bRight], AContentColor);
        end;
  end;
end;

procedure DrawSpinEditUltraFlatButtonBorder(ACanvas: TcxCanvas; R: TRect;
  AButtonPosition: TcxSpinEditUltraFlatButtonPosition; ABrushColor: TColor;
  AButtonViewInfo: TcxEditButtonViewInfo; const ABounds: TRect;
  AIsEditBackgroundTransparent, AHasExternalBorder: Boolean;
  AEditBorderStyle: TcxEditBorderStyle; out ABackgroundRect, AContentRect: TRect);
var
  AHighlightColor: TColor;
begin
  AHighlightColor := GetEditBorderHighlightColor(
    AButtonViewInfo.Data.Style = btsOffice11);

  if (AButtonViewInfo.Data.State in [ebsDisabled, ebsNormal]) or
    not AButtonViewInfo.Data.IsInplace and (AEditBorderStyle = ebsNone) or
    AButtonViewInfo.Data.IsInplace and not AHasExternalBorder then
  begin
    if not(AButtonViewInfo.Data.State in [ebsDisabled, ebsNormal]) then
      ACanvas.FrameRect(R, AHighlightColor)
    else
      if not AIsEditBackgroundTransparent then
        ACanvas.FrameRect(R, AButtonViewInfo.Data.BackgroundColor);
    InflateRect(R, -1, -1);
    ABackgroundRect := R;
  end
  else
  begin
    ABackgroundRect := R;
    case AButtonPosition of
      sufbpLeftMost:
        begin
          DrawButtonBorder(ACanvas, R, [bRight], AHighlightColor);
          Dec(ABackgroundRect.Right);
          R := cxRectContent(R, Rect(1, 1, 0, 1));
        end;
      sufbpMiddle:
        if R.Left = ABounds.Left then
        begin
          DrawButtonBorder(ACanvas, R, [bRight], AHighlightColor);
          Dec(ABackgroundRect.Right);
          R := cxRectContent(R, Rect(1, 1, 0, 1));
        end
        else
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bRight], AHighlightColor);
          ABackgroundRect := cxRectContent(ABackgroundRect, Rect(1, 0, 1, 0));
          R := cxRectContent(R, Rect(0, 1, 0, 1));
        end;
      sufbpRightMost:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft], AHighlightColor);
          Inc(ABackgroundRect.Left);
          R := cxRectContent(R, Rect(0, 1, 1, 1));
        end;
      sufbpRightTop:
        begin
          DrawButtonBorder(ACanvas, R, [bBottom], AHighlightColor);
          Dec(ABackgroundRect.Bottom);
          if R.Left = ABounds.Left then
            Inc(R.Left)
          else
          begin
            DrawButtonBorder(ACanvas, R, [bLeft], AHighlightColor);
            Inc(ABackgroundRect.Left);
          end;
          R := cxRectContent(R, Rect(0, 1, 1, 0));
        end;
      sufbpRightBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bTop], AHighlightColor);
          Inc(ABackgroundRect.Top);
          if R.Left = ABounds.Left then
            Inc(R.Left)
          else
          begin
            DrawButtonBorder(ACanvas, R, [bLeft], AHighlightColor);
            Inc(ABackgroundRect.Left);
          end;
          R := cxRectContent(R, Rect(0, 0, 1, 1));
        end;
      sufbpMiddleTop:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bBottom, bRight], AHighlightColor);
          ABackgroundRect := cxRectContent(ABackgroundRect, Rect(1, 0, 1, 1));
          Inc(R.Top);
        end;
      sufbpMiddleBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bLeft, bTop, bRight], AHighlightColor);
          ABackgroundRect := cxRectContent(ABackgroundRect, Rect(1, 1, 1, 0));
          Dec(R.Bottom);
        end;
      sufbpLeftTop:
        begin
          DrawButtonBorder(ACanvas, R, [bBottom], AHighlightColor);
          Dec(ABackgroundRect.Bottom);
          if R.Right = ABounds.Right then
            Dec(R.Right)
          else
          begin
            DrawButtonBorder(ACanvas, R, [bRight], AHighlightColor);
            Dec(ABackgroundRect.Right);
          end;
          R := cxRectContent(R, Rect(1, 1, 0, 0));
        end;
      sufbpLeftBottom:
        begin
          DrawButtonBorder(ACanvas, R, [bTop], AHighlightColor);
          Inc(ABackgroundRect.Top);
          if R.Right = ABounds.Right then
            Dec(R.Right)
          else
          begin
            DrawButtonBorder(ACanvas, R, [bRight], AHighlightColor);
            Dec(ABackgroundRect.Right);
          end;
          R := cxRectContent(R, Rect(1, 0, 0, 1));
        end;
    end;
  end;
  AContentRect := R;
end;

{ TcxSpinEditViewInfo }

procedure TcxSpinEditViewInfo.DrawButtonContent(ACanvas: TcxCanvas;
  AViewInfo: TcxEditButtonViewInfo; const AContentRect: TRect;
  APenColor, ABrushColor: TColor; ANeedOffsetContent: Boolean);

  procedure InternalDrawArrow(const R: TRect; AColor: TColor);
  const
    ArrowDirectionMap: array[Boolean, 0..3] of TcxArrowDirection = (
      (adLeft, adRight, adLeft, adRight),
      (adDown, adUp, adLeft, adRight)
    );
  begin
    DrawArrow(ACanvas, ArrowSize, R,
      ArrowDirectionMap[ButtonsPosition = sbpVert, AViewInfo.ButtonVisibleIndex],
      AViewInfo.ButtonVisibleIndex > 1, ANeedOffsetContent, AColor);
  end;

const
  PainterArrowMap: array[Boolean, 0..3] of TcxEditBtnKind = (
    (cxbkSpinLeftBtn, cxbkSpinRightBtn, cxbkSpinLeftBtn, cxbkSpinRightBtn),
    (cxbkSpinDownBtn, cxbkSpinUpBtn, cxbkSpinDownBtn, cxbkSpinUpBtn));
begin
  if UseSkins then
  begin
    Painter.DrawScaledEditorButtonGlyph(ACanvas, AContentRect,
      PainterArrowMap[ButtonsPosition = sbpVert, AViewInfo.ButtonVisibleIndex],
      EditBtnStateToButtonState[AViewInfo.Data.State], AViewInfo.ScaleFactor,
      EditBtnPositionMap[AViewInfo.Data.LeftAlignment]);
  end
  else
    if AViewInfo.Data.State <> ebsDisabled then
    begin
      if Assigned(OnGetButtonContentColor) then
        OnGetButtonContentColor(AViewInfo, APenColor);
      InternalDrawArrow(AContentRect, APenColor);
    end
    else
    begin
      InternalDrawArrow(cxRectOffset(AContentRect, 1, 1), clBtnHighlight);
      InternalDrawArrow(AContentRect, clBtnShadow);
    end;
end;

procedure TcxSpinEditViewInfo.DrawHotFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);

  function GetButtonPosition: TcxSpinEditFlatButtonPosition;
  const
    AButtonPositionA: array[Boolean, Boolean, TcxSpinEditButtonsPosition, sebBackward..sebForward] of TcxSpinEditFlatButtonPosition =
      (((
           (sfbpLeftSideRightMost, sfbpRightSideLeftMost),
           (sfbpRightSideLeftMost, sfbpRightSide),
           (sfbpRightBottom, sfbpRightTop)
        ),
        (
           (sfbpLeftSideRightMost, sfbpRightSideLeftMost),
           (sfbpRightSide, sfbpRightSide),
           (sfbpMiddleBottom, sfbpMiddleTop)
        )
       ),
       ((
           (sfbpLeftSideRightMost, sfbpRightSideLeftMost),
           (sfbpLeftSide, sfbpLeftSideRightMost),
           (sfbpLeftBottom, sfbpLeftTop)
         ),
         (
           (sfbpLeftSideRightMost, sfbpRightSideLeftMost),
           (sfbpLeftSide, sfbpLeftSide),
           (sfbpLeftMiddleBottom, sfbpLeftMiddleTop)
         )
    ));
    AFastButtonPositionA: array[Boolean, TcxSpinEditButtonsPosition, sebFastBackward..sebFastForward] of TcxSpinEditFlatButtonPosition = (
      ((sfbpLeftSide, sfbpRightSide),
      (sfbpRightSideLeftMost, sfbpRightSide),
      (sfbpRightSideLeftMost, sfbpRightSide)),

      ((sfbpLeftSide, sfbpRightSide),
      (sfbpLeftSide, sfbpLeftSideRightMost),
      (sfbpLeftSide, sfbpLeftSideRightMost))
    );
  begin
    if AButtonViewInfo.ButtonIndex > 1 then
      Result := AFastButtonPositionA[UseRightToLeftAlignment, ButtonsPosition, TcxSpinEditButton(AButtonViewInfo.ButtonIndex)]
    else
      Result := AButtonPositionA[UseRightToLeftAlignment, Length(ButtonsInfo) > 2, ButtonsPosition,
        TcxSpinEditButton(AButtonViewInfo.ButtonIndex)];
  end;

const
  ABrushColorA: array [TcxEditButtonState] of TColor = (
    clBtnFace, clBtnFace, clBtnText, clBtnShadow
  );
begin
  with AButtonViewInfo do
  begin
    ABrushColor := ABrushColorA[Data.State];
    if Data.Transparent then
      ABrushColor := Data.BackgroundColor;

    if Data.State in [ebsPressed, ebsSelected] then
      if Data.Transparent and (Data.State = ebsSelected) then
        APenColor := clBtnShadow
      else
        APenColor := clBtnHighlight
    else
      APenColor := clBtnText;
    DrawSpinEditFlatButtonBorder(ACanvas, GetButtonPosition, ARect, ABrushColor, clBtnShadow);

    AContentRect := ARect;
  end;
end;

procedure TcxSpinEditViewInfo.DrawNativeButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
begin
  if IsCustomDrawButton then
    AContentRect := ARect
  else
    AContentRect := cxEmptyRect;
end;

procedure TcxSpinEditViewInfo.DrawUltraFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  AIsOffice11Style: Boolean; var ARect: TRect; var AContentRect: TRect; out APenColor, ABrushColor: TColor);
var
  AButtonPosition: TcxSpinEditUltraFlatButtonPosition;
begin
  with ACanvas, AButtonViewInfo do
  begin
    if AButtonViewInfo.Data.Transparent then
      ABrushColor := Data.BackgroundColor
    else
      if AButtonViewInfo.Data.State = ebsDisabled then
        ABrushColor := clBtnFace
      else
        if AButtonViewInfo.Data.State = ebsNormal then
          if AIsOffice11Style then
            ABrushColor := dxOffice11DockColor1
          else
            ABrushColor := clBtnFace
        else
          ABrushColor := GetEditButtonHighlightColor(
            AButtonViewInfo.Data.State = ebsPressed, AIsOffice11Style);
    case AButtonViewInfo.ButtonIndex  of
      cxSpinFastBackwardButtonIndex:
        if ButtonsPosition = sbpHorzLeftRight then
          AButtonPosition := sufbpLeftMost
        else
          AButtonPosition := sufbpMiddle;
      cxSpinFastForwardButtonIndex:
        if UseRightToLeftAlignment and (ButtonsPosition <> sbpHorzLeftRight) then
          AButtonPosition := sufbpMiddle
        else
          AButtonPosition := sufbpRightMost;
      cxSpinBackwardButtonIndex:
        if ButtonsPosition = sbpVert then
          if Length(ButtonsInfo) > 2 then
            AButtonPosition := sufbpMiddleBottom
          else
            if UseRightToLeftAlignment then
              AButtonPosition := sufbpLeftBottom
            else
              AButtonPosition := sufbpRightBottom
        else
          if (Length(ButtonsInfo) = 2) and ((ButtonsPosition = sbpHorzLeftRight) or UseRightToLeftAlignment) then
            AButtonPosition := sufbpLeftMost
          else
            AButtonPosition := sufbpMiddle;
      else // cxSpinForwardButtonIndex
        if ButtonsPosition = sbpVert then
          if Length(ButtonsInfo) > 2 then
            AButtonPosition := sufbpMiddleTop
          else
            if UseRightToLeftAlignment then
              AButtonPosition := sufbpLeftTop
            else
              AButtonPosition := sufbpRightTop
        else
          if (Length(ButtonsInfo) > 2) or UseRightToLeftAlignment and (ButtonsPosition = sbpHorzRight) then
            AButtonPosition := sufbpMiddle
          else
            AButtonPosition := sufbpRightMost;
    end;
    DrawSpinEditUltraFlatButtonBorder(ACanvas, ARect,
      AButtonPosition, ABrushColor, AButtonViewInfo, BorderRect,
      Transparent, epoHasExternalBorder in PaintOptions, Self.BorderStyle,
      ARect, AContentRect);
  end;
end;

procedure TcxSpinEditViewInfo.DrawNativeButtonBackground(
  ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect);
begin
  DrawThemeBackground(OpenTheme(totSpin), ACanvas.Handle,
    AButtonViewInfo.Data.NativePart, AButtonViewInfo.Data.NativeState, ARect);
end;

procedure TcxSpinEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  DrawSpinEdit(ACanvas, Self);
end;

{ TcxSpinEditViewData }

procedure TcxSpinEditViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, ViewInfo, AIsMouseEvent);
  ViewInfo.UseRightToLeftAlignment := UseRightToLeftAlignment;
end;

{ TcxSpinEditViewData }

procedure TcxSpinEditViewData.CalculateButtonNativePartInfo(
  ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo);
const
  ButtonNativeStateMap: array [TcxEditButtonState] of Integer = (
    UPS_DISABLED, UPS_NORMAL, UPS_PRESSED, UPS_HOT
  );
begin
  if ATheme <> 0 then
    AButtonViewInfo.Data.NativeState := ButtonNativeStateMap[AButtonViewInfo.Data.State]
  else
    AButtonViewInfo.Data.NativeState := TC_NONE;
end;

procedure TcxSpinEditViewData.CalculateButtonsBounds(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo; var ButtonsRect: TRect);
var
  AButtonWidth, ADefaultButtonWidth, AFastButtonWidth: Integer;
  ANativeStyle: Boolean;
  ASpinButtonsPosition: TcxSpinEditButtonsPosition;
  Y: Integer;

  function GetDefaultButtonWidth(AButtonVisibleIndex: Integer): Integer;
  begin
    Result := CalculateEditDefaultButtonWidth(ACanvas, AViewInfo.ButtonsInfo[AButtonVisibleIndex]);
  end;

  procedure CalculateButtonMetrics;
  var
    AClientExtent: TRect;
    AEditButtonsExtent: TRect;
  begin
    ACanvas.Font := Style.GetVisibleFont;
    AButtonWidth := GetDefaultButtonWidth(Properties.Buttons.VisibleCount - 1);

    AClientExtent := GetClientExtent(ACanvas, AViewInfo);
    AEditButtonsExtent := GetButtonsExtent(ACanvas);
    TcxSpinEditViewInfo(AViewInfo).ArrowSize := GetArrowSize(Size(AButtonWidth + 2,
      (ButtonsRect.Bottom - ButtonsRect.Top) div 2 + 2), adUp).cy;
    if ANativeStyle then
    begin
      ADefaultButtonWidth := ACanvas.TextHeight('Zg') +
        Self.GetEditContentSizeCorrection.cy + (AClientExtent.Top + AClientExtent.Bottom) -
        (AEditButtonsExtent.Top + AEditButtonsExtent.Bottom) - 1;
      ADefaultButtonWidth := ADefaultButtonWidth div 2 + Integer(Odd(ADefaultButtonWidth));
    end
    else
      if Style.LookAndFeel.SkinPainter <> nil then
      begin
        with Style.LookAndFeel.SkinPainter.EditButtonSize do
        begin
          ADefaultButtonWidth := cy div 2 + TcxSpinEditViewInfo(AViewInfo).ArrowSize;
          AFastButtonWidth := ADefaultButtonWidth * 3 div 2 + Integer(Odd(cy));
          Exit;
        end;
      end
      else
        ADefaultButtonWidth := (TcxSpinEditViewInfo(AViewInfo).ArrowSize * 2 - 1) * 2 + 1 - 2;
      dxAdjustToTouchableSize(ADefaultButtonWidth, ScaleFactor);
      AFastButtonWidth := MulDiv(ADefaultButtonWidth, 3, 2) + Integer(Odd(ADefaultButtonWidth));
      dxAdjustToTouchableSize(AFastButtonWidth, ScaleFactor);
  end;

  procedure UpdateButtonsNativePart;

    procedure SetButtonsNativePart;
    begin
      if ASpinButtonsPosition = sbpVert then
      begin
        AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Data.NativePart := SPNP_DOWN;
        AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex].Data.NativePart := SPNP_UP;
      end
      else
      begin
        AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Data.NativePart := SPNP_DOWNHORZ;
        AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex].Data.NativePart := SPNP_UPHORZ;
      end;
      if Length(AViewInfo.ButtonsInfo) > 2 then
      begin
        AViewInfo.ButtonsInfo[cxSpinFastBackwardButtonIndex].Data.NativePart := SPNP_DOWNHORZ;
        AViewInfo.ButtonsInfo[cxSpinFastForwardButtonIndex].Data.NativePart := SPNP_UPHORZ;
      end;
    end;

    procedure ResetButtonsNativePart;
    var
      I: Integer;
    begin
      for I := Low(AViewInfo.ButtonsInfo) to High(AViewInfo.ButtonsInfo) do
        AViewInfo.ButtonsInfo[I].Data.NativePart := TC_NONE;
    end;

  begin
    if NativeStyle then
    begin
      SetButtonsNativePart;
    end
    else
      ResetButtonsNativePart;
  end;

  function GetDelimiterSize: Integer;
  begin
    if ANativeStyle and not AViewInfo.IsCustomDrawButton then
      Result := 1
    else
      Result := 0;
  end;

  function GetBottomCorrection: Integer;
  begin
    if ANativeStyle and IsWinVistaOrLater and not AViewInfo.IsCustomDrawButton then
      Result := 1
    else
      Result := 0;
  end;

var
  APrevButtonsRect: TRect;
  AShowFastButtons: Boolean;
  I: Integer;
  R: TRect;
begin
  APrevButtonsRect := ButtonsRect;
  AShowFastButtons := Properties.SpinButtons.ShowFastButtons;
  ASpinButtonsPosition := Properties.SpinButtons.Position;
  ANativeStyle := AreVisualStylesMustBeUsed(AViewInfo.NativeStyle, totSpin);
  TcxSpinEditViewInfo(AViewInfo).ButtonsPosition := Properties.SpinButtons.Position;

  CalculateButtonMetrics;
  if AShowFastButtons then
    with AViewInfo.ButtonsInfo[cxSpinFastForwardButtonIndex] do
    begin
      Bounds := Rect(ButtonsRect.Right - AFastButtonWidth, ButtonsRect.Top,
        ButtonsRect.Right, ButtonsRect.Bottom - GetBottomCorrection);
      ButtonsRect.Right := Bounds.Left;
    end;

  if ASpinButtonsPosition = sbpVert then
  begin
    with AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex] do
    begin
      Bounds := Rect(ButtonsRect.Right - AButtonWidth, ButtonsRect.Top,
        ButtonsRect.Right, ButtonsRect.Top + (cxRectHeight(ButtonsRect) - GetDelimiterSize) div 2);
      Y := Bounds.Bottom + GetDelimiterSize;
    end;
    with AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex] do
    begin
      Bounds := Rect(ButtonsRect.Right - AButtonWidth, Y,
        ButtonsRect.Right, ButtonsRect.Bottom);
      ButtonsRect.Right := Bounds.Left;
    end;
  end
  else
  begin
    with AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex] do
    begin
      Bounds := Rect(ButtonsRect.Right - ADefaultButtonWidth, ButtonsRect.Top,
        ButtonsRect.Right, ButtonsRect.Bottom - GetBottomCorrection);
      ButtonsRect.Right := Bounds.Left;
      Dec(ButtonsRect.Right, GetDelimiterSize);
    end;
  end;

  if ASpinButtonsPosition = sbpHorzLeftRight then
  begin
    if AShowFastButtons then
      with AViewInfo.ButtonsInfo[cxSpinFastBackwardButtonIndex] do
      begin
        Bounds := Rect(ButtonsRect.Left, ButtonsRect.Top,
          ButtonsRect.Left + AFastButtonWidth, ButtonsRect.Bottom - GetBottomCorrection);
        ButtonsRect.Left := Bounds.Right;
      end;
    with AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex] do
    begin
      Bounds := Rect(ButtonsRect.Left, ButtonsRect.Top,
        ButtonsRect.Left + ADefaultButtonWidth, ButtonsRect.Bottom - GetBottomCorrection);
      ButtonsRect.Left := Bounds.Right;
    end;
  end
  else
  begin
    if ASpinButtonsPosition = sbpHorzRight then
    begin
      with AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex] do
      begin
        Bounds := Rect(ButtonsRect.Right - ADefaultButtonWidth, ButtonsRect.Top,
          ButtonsRect.Right, ButtonsRect.Bottom - GetBottomCorrection);
        ButtonsRect.Right := Bounds.Left;
      end;
    end;
    if AShowFastButtons then
      with AViewInfo.ButtonsInfo[cxSpinFastBackwardButtonIndex] do
      begin
        Bounds := Rect(ButtonsRect.Right - AFastButtonWidth, ButtonsRect.Top,
          ButtonsRect.Right, ButtonsRect.Bottom - GetBottomCorrection);
        ButtonsRect.Right := Bounds.Left;
      end;
  end;
  if UseRightToLeftAlignment and (Properties.SpinButtons.Position <> sbpHorzLeftRight) then
  begin
    ButtonsRect := TdxRightToLeftLayoutConverter.ConvertRect(ButtonsRect, APrevButtonsRect);
    for I := 0 to High(AViewInfo.ButtonsInfo) do
      AViewInfo.ButtonsInfo[I].Bounds := TdxRightToLeftLayoutConverter.ConvertRect(AViewInfo.ButtonsInfo[I].Bounds, APrevButtonsRect);
    R := APrevButtonsRect;
    if Properties.SpinButtons.ShowFastButtons then
      R.Right := AViewInfo.ButtonsInfo[cxSpinFastBackwardButtonIndex].Bounds.Right
    else
      R.Right := AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Bounds.Right;
    for I := 0 to High(AViewInfo.ButtonsInfo) do
      AViewInfo.ButtonsInfo[I].Bounds := TdxRightToLeftLayoutConverter.ConvertRect(AViewInfo.ButtonsInfo[I].Bounds, R);
  end;
  UpdateButtonsNativePart;
end;

procedure TcxSpinEditViewData.CalculateButtonViewInfo(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer;
  var ButtonsRect: TRect);
begin
  InitializeButtonInfo(AViewInfo, AButtonVisibleIndex);
end;

procedure TcxSpinEditViewData.CheckButtonsOnly(AViewInfo: TcxCustomEditViewInfo;
  APrevButtonsWidth, AButtonsWidth: Integer);

  procedure SetButtonsVisibleBounds;
  var
    I: Integer;
  begin
    for I := 0 to Length(AViewInfo.ButtonsInfo) - 1 do
      with AViewInfo.ButtonsInfo[I] do
        IntersectRect(VisibleBounds, Bounds, AViewInfo.BorderRect);
  end;

var
  ASpinEditViewInfo: TcxSpinEditViewInfo;
begin
  inherited CheckButtonsOnly(AViewInfo, APrevButtonsWidth, AButtonsWidth);
  if Properties.SpinButtons.Position = sbpVert then
    with AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex].Bounds do
    begin
      AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Bounds.Left := Left;
      AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Bounds.Right := Right;
    end;
  SetButtonsVisibleBounds;
  if AreVisualStylesMustBeUsed(AViewInfo.NativeStyle, totSpin) then
  begin
    ASpinEditViewInfo := TcxSpinEditViewInfo(AViewInfo);
    case Properties.SpinButtons.Position of
      sbpHorzLeftRight:
        with ASpinEditViewInfo do
          FillChar(DelimiterLine, SizeOf(DelimiterLine), 0);
      sbpHorzRight:
        with AViewInfo.ButtonsInfo[cxSpinBackwardButtonIndex].Bounds do
        begin
          ASpinEditViewInfo.DelimiterLine[0] := Point(Right, Top);
          ASpinEditViewInfo.DelimiterLine[1] := Point(Right, Bottom - 1);
        end;
      sbpVert:
        with AViewInfo.ButtonsInfo[cxSpinForwardButtonIndex].Bounds do
        begin
          ASpinEditViewInfo.DelimiterLine[0] := Point(Left, Bottom);
          ASpinEditViewInfo.DelimiterLine[1] := Point(Right - 1, Bottom);
        end;
    end;
  end;
end;

procedure TcxSpinEditViewData.DoCalculateButtonViewInfos(ACanvas: TcxCanvas;
  var AButtonsRect: TRect; AViewInfo: TcxCustomEditViewInfo);
begin
  inherited DoCalculateButtonViewInfos(ACanvas, AButtonsRect, AViewInfo);
  CalculateButtonsBounds(ACanvas, AViewInfo, AButtonsRect);
end;

function TcxSpinEditViewData.IgnoreButtonWhileStretching(
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := (Properties.SpinButtons.Position = sbpVert) and
    (AButtonVisibleIndex = cxSpinBackwardButtonIndex);
end;

function TcxSpinEditViewData.IsButtonLeftAligned(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  if UseRightToLeftAlignment and (Properties.SpinButtons.Position = sbpHorzLeftRight) and
    ((AButtonVisibleIndex = cxSpinForwardButtonIndex) or (AButtonVisibleIndex = cxSpinFastForwardButtonIndex)) then
    Result := False
  else
    Result := (Properties.SpinButtons.Position = sbpHorzLeftRight) and
      ((AButtonVisibleIndex = cxSpinBackwardButtonIndex) or (AButtonVisibleIndex = cxSpinFastBackwardButtonIndex)) or
      inherited IsButtonLeftAligned(AViewInfo, AButtonVisibleIndex);
end;

function TcxSpinEditViewData.CanPressButton(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  if Edit <> nil then
    Result := TcxCustomSpinEdit(Edit).PressedState = epsNone
  else
    Result := True;
end;

function TcxSpinEditViewData.IsButtonPressed(
  AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean;
var
  APressedState: TcxSpinEditPressedState;
begin
  if Edit = nil then
    APressedState := epsNone
  else
    APressedState := TcxCustomSpinEdit(Edit).PressedState;
  Result := (AButtonVisibleIndex + 1) = Integer(APressedState)
end;

function TcxSpinEditViewData.GetButtonNativeTheme(
  AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme;
begin
  Result := OpenTheme(totSpin);
end;

function TcxSpinEditViewData.GetProperties: TcxCustomSpinEditProperties;
begin
  Result := TcxCustomSpinEditProperties(FProperties);
end;

{ TcxSpinEditButtons }

constructor TcxSpinEditButtons.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FPosition := sbpVert;
  FVisible := True;
end;

procedure TcxSpinEditButtons.Assign(Source: TPersistent);
begin
  if Source is TcxSpinEditButtons then
  begin
    Properties.BeginUpdate;
    try
      with TcxSpinEditButtons(Source) do
      begin
        Self.Position := Position;
        Self.ShowFastButtons := ShowFastButtons;
        Self.Visible := Visible;
      end;
    finally
      Properties.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSpinEditButtons.Changed;
begin
  Properties.Changed;
end;

function TcxSpinEditButtons.GetProperties: TcxCustomSpinEditProperties;
begin
  Result := TcxCustomSpinEditProperties(FOwner);
end;

procedure TcxSpinEditButtons.SetPosition(Value: TcxSpinEditButtonsPosition);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TcxSpinEditButtons.SetShowFastButtons(Value: Boolean);
begin
  if Value <> FShowFastButtons then
  begin
    FShowFastButtons := Value;
    Changed;
  end;
end;

procedure TcxSpinEditButtons.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TcxSpinEditPropertiesValues }

procedure TcxSpinEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxSpinEditPropertiesValues then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      ValueType := TcxSpinEditPropertiesValues(Source).ValueType;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSpinEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    ValueType := False;
  finally
    EndUpdate;
  end;
end;

procedure TcxSpinEditPropertiesValues.SetValueType(Value: Boolean);
begin
  if Value <> FValueType then
  begin
    FValueType := Value;
    Changed;
  end;
end;

{ TcxCustomSpinEditProperties }

constructor TcxCustomSpinEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Buttons.Add;
  Buttons.Add;
  FSpinButtons := TcxSpinEditButtons.Create(Self);
  FCanEdit := True;
  FIncrement := 1.0;
  FLargeIncrement := 10.0;
end;

destructor TcxCustomSpinEditProperties.Destroy;
begin
  FreeAndNil(FSpinButtons);
  inherited Destroy;
end;

function TcxCustomSpinEditProperties.CreateViewData(AStyle: TcxCustomEditStyle;
  AIsInplace: Boolean; APreviewMode: Boolean = False): TcxCustomEditViewData;
begin
  Result := inherited CreateViewData(AStyle, AIsInplace, APreviewMode);
  with TcxSpinEditViewData(Result) do
    if Edit <> nil then
      PressedState := TcxCustomSpinEdit(Edit).FPressedState
    else
      PressedState := epsNone;
end;

class function TcxCustomSpinEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxSpinEdit;
end;

class function TcxCustomSpinEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxSpinEditViewInfo;
end;

function TcxCustomSpinEditProperties.IsDisplayValueValid(var ADisplayValue: TcxEditValue; AEditFocused: Boolean):
  Boolean;
var
  AValue: TcxEditValue;
begin
  Result := inherited IsDisplayValueValid(ADisplayValue, AEditFocused);
  if not Result or not IsDisplayValueNumeric then
    Exit;
  try
    if ADisplayValue <> '' then
    begin
      Result := TryTextToValue(Trim(VarToStr(ADisplayValue)), AValue);
      if not Result then
        Exit;
      if (AValue < InternalMinValue) or (AValue > InternalMaxValue) then
        ADisplayValue := VarToStr(AValue);
    end;
    Result := True;
  except
    on EConvertError do
      Result := False;
  end;
end;

function TcxCustomSpinEditProperties.IsEditValueValid(var AEditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
var
  AValue: TcxEditValue;
begin
  if VarIsStr(AEditValue) or VarIsNumericEx(AEditValue) then
  begin
    if VarIsNumericEx(AEditValue) then
      Result := TryTextToValue(FloatToStr(AEditValue), AValue)
    else
      Result := TryTextToValue(VarToStr(AEditValue), AValue);
    if Result then
      AEditValue := AValue;
  end
  else
    Result :=  {VarIsNumericEx(EditValue) or} VarIsSoftNull(AEditValue) or
      VarIsDate(AEditValue);
  if not Result then
    Exit;
  try
    AEditValue := PrepareValue(AEditValue);
//    EditValue := CheckValueBounds(EditValue);
    AEditValue := SetVariantType(AEditValue);
  except
    Result := False;
  end;
end;

procedure TcxCustomSpinEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit);
var
  AEditValue: TcxEditValue;
  AIsUserErrorDisplayValue: Boolean;
begin
  if TcxCustomSpinEdit(AEdit).FIsCustomText then
    AError := not TcxCustomSpinEdit(AEdit).InternalPrepareEditValue(ADisplayValue,
      False, AEditValue, AErrorText)
  else
    AEditValue := TcxCustomSpinEdit(AEdit).FInternalValue;
  if not AError then
  begin
    if VarIsNull(AEditValue) then
    begin
      AEditValue := CheckValueBounds(0);
      AEditValue := SetVariantType(AEditValue);
    end;
    if not ExceptionOnInvalidInput then
      if AEditValue < InternalMinValue then
        AEditValue := InternalMinValue
      else
        if AEditValue > InternalMaxValue then
          AEditValue := InternalMaxValue;
    ADisplayValue := VarToStr(AEditValue);
    if ExceptionOnInvalidInput then
    begin
      AError := (AEditValue < InternalMinValue) or (AEditValue > InternalMaxValue);
      if AError then
        AErrorText := cxGetResourceString(@cxSEditValueOutOfBounds);
    end;
  end;

  DoValidate(ADisplayValue, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
end;

function TcxCustomSpinEditProperties.DefaultFocusedDisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(CheckValueBounds(0), Result, True)
end;

procedure TcxCustomSpinEditProperties.DefineProperties(Filer: TFiler);

  function HasZeroIncrement: Boolean;
  begin
    Result := (Increment = 0) and ((Filer.Ancestor = nil) or
      (TcxCustomSpinEditProperties(Filer.Ancestor).Increment <> Increment));
  end;

  function HasZeroLargeIncrement: Boolean;
  begin
    Result := (LargeIncrement = 0) and ((Filer.Ancestor = nil) or
      (TcxCustomSpinEditProperties(Filer.Ancestor).LargeIncrement <> LargeIncrement));
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ZeroIncrement', ReadZeroIncrement, WriteZeroIncrement,
    HasZeroIncrement);
  Filer.DefineProperty('ZeroLargeIncrement', ReadZeroLargeIncrement,
    WriteZeroLargeIncrement, HasZeroLargeIncrement);
end;

procedure TcxCustomSpinEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  if AProperties is TcxCustomSpinEditProperties then
    with TcxCustomSpinEditProperties(AProperties) do
    begin
      Self.FIDefaultValuesProvider := FIDefaultValuesProvider;
      Self.AssignedValues.ValueType := False;
      if AssignedValues.ValueType then
        Self.ValueType := ValueType;

      inherited;

      Self.CanEdit := CanEdit;
      Self.Circular := Circular;
      Self.ExceptionOnInvalidInput := ExceptionOnInvalidInput;
      Self.Increment := Increment;
      Self.LargeIncrement := LargeIncrement;
      Self.SpinButtons := SpinButtons;
      Self.UseCtrlIncrement := UseCtrlIncrement;
      Self.OnGetValue := OnGetValue;
    end
  else
    inherited;
end;

procedure TcxCustomSpinEditProperties.DoChanged;

  function GetButtonCount: Integer;
  begin
    Result := 0;
    if SpinButtons.Visible then
    begin
      Inc(Result, 2);
      if SpinButtons.ShowFastButtons then
        Inc(Result, 2);
    end;
  end;

var
  AButtonCount: Integer;
begin
  if FSpinButtons <> nil then
  begin
    BeginUpdate;
    try
      AButtonCount := GetButtonCount;
      while Buttons.Count < AButtonCount do
        Buttons.Add;
      while Buttons.Count > AButtonCount do
        Buttons[0].Free;
    finally
      EndUpdate(False);
    end;
  end;
  inherited DoChanged;
end;

class function TcxCustomSpinEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxSpinEditPropertiesValues;
end;

function TcxCustomSpinEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if not AEditFocused and not AssignedValues.DisplayFormat and (IDefaultValuesProvider <> nil) and
    IDefaultValuesProvider.IsDisplayFormatDefined(True) then
      Result := evsText
  else
    Result := evsValue;
end;

class function TcxCustomSpinEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxSpinEditViewData;
end;

function TcxCustomSpinEditProperties.IsEditValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinEditProperties.CheckValueBounds(const AValue: Variant): Variant;
begin
  Result := AValue;
  if VarIsNumericEx(Result) then
    if IsValueBoundDefined(evbMin) and (Result < MinValue) then
      Result := MinValue
    else
      if IsValueBoundDefined(evbMax) and (Result > MaxValue) then
        Result := MaxValue;
end;

function TcxCustomSpinEditProperties.ExtendValueUpToBound: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinEditProperties.GetBoundsCheckingKind: TcxSpinBoundsCheckingKind;
begin
  if FCircular then
    Result := bckCircular
  else
    if ExtendValueUpToBound then
      Result := bckExtendToBound
    else
      Result := bckDoNotExceed;
end;

function TcxCustomSpinEditProperties.GetIncrement(AButton: TcxSpinEditButton): Double;
begin
  case AButton of
    sebBackward: Result := - Increment;
    sebForward: Result := Increment;
    sebFastBackward: Result := - LargeIncrement;
    sebFastForward: Result := LargeIncrement;
  else
    raise EdxException.Create('TcxCustomSpinEditProperties.GetIncrement fails');
  end;
end;

function TcxCustomSpinEditProperties.GetMaxMinValueForCurrentValueType(AMinValue: Boolean = True): TcxEditValue;
begin
  if ValueType = vtInt then
    if AMinValue then
      Result := MinIntValue
    else
      Result := MaxIntValue
  else
    if AMinValue then
      Result := -MaxDouble
    else
      Result := MaxDouble;
end;

function TcxCustomSpinEditProperties.InternalMaxValue: TcxEditValue;
begin
  if IsValueBoundDefined(evbMax) then
    Result := DoubleAsValueType(MaxValue, ValueType)
  else
    Result := GetMaxMinValueForCurrentValueType(False);
  Result := VarToCurrentValueType(Result);
end;

function TcxCustomSpinEditProperties.InternalMinValue: TcxEditValue;
begin
  if IsValueBoundDefined(evbMin) then
    Result := DoubleAsValueType(MinValue, ValueType)
  else
    Result := GetMaxMinValueForCurrentValueType;
  Result := VarToCurrentValueType(Result);
end;

function TcxCustomSpinEditProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinEditProperties.IsValueBoundsValid(AValue: Extended): Boolean;
begin
  if ValueType = vtInt then
    Result := (AValue >= MinIntValue) and (AValue <= MaxIntValue)
  else
    Result := (AValue >= -MaxDouble) and (AValue <= MaxDouble);
end;

function TcxCustomSpinEditProperties.PrepareValue(const AValue: TcxEditValue): Variant;
begin
  if VarIsSoftNull(AValue) then
    if UseNullString and VarIsNull(AValue) then
      Result := Null
    else
     Result := VarToCurrentValueType(0)
  else
    Result := VarToCurrentValueType(AValue);
end;

function TcxCustomSpinEditProperties.PreserveSelection: Boolean;
begin
  Result := False;
end;

function TcxCustomSpinEditProperties.SetVariantType(const AValue: TcxEditValue): TcxEditValue;
begin
  if VarIsNumericEx(AValue) then
    Result := VarToCurrentValueType(AValue)
  else
    Result := AValue;
end;

function TcxCustomSpinEditProperties.DoubleAsValueType(AValue: Double;
  AValueType: TcxSpinEditValueType): Double;
begin
  if AValueType = vtInt then
    Result := Round(AValue)
  else
    Result := AValue;
end;

function TcxCustomSpinEditProperties.GetAssignedValues: TcxSpinEditPropertiesValues;
begin
  Result := TcxSpinEditPropertiesValues(FAssignedValues);
end;

function TcxCustomSpinEditProperties.GetValueType: TcxSpinEditValueType;
const
  ASpinEditValueTypeMap: array [Boolean] of TcxSpinEditValueType = (vtInt, vtFloat);
begin
  if AssignedValues.ValueType then
    Result := FValueType
  else
    if IDefaultValuesProvider <> nil then
      Result := ASpinEditValueTypeMap[IDefaultValuesProvider.DefaultIsFloatValue]
    else
      Result := vtInt;
end;

function TcxCustomSpinEditProperties.InternalTryTextToValue(S: string; out AValue: TcxEditValue): Boolean;
var
  AExtendedValue: Extended;
  AIntegerValue: Int64;
begin
  AValue := Null;
  AIntegerValue := 0;
  if ValueType = vtInt then
    Result := TryStrToInt64(S, AIntegerValue)
  else
  begin
    Result := TextToFloat(PChar(S), AExtendedValue, fvExtended);
    Result := Result and IsValueBoundsValid(AExtendedValue);
  end;
  if Result then
  begin
    if ValueType = vtInt then
      AValue := AIntegerValue
    else
      AValue := AExtendedValue;
  end;
end;

function TcxCustomSpinEditProperties.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TcxCustomSpinEditProperties.IsLargeIncrementStored: Boolean;
begin
  Result := FLargeIncrement <> 10.0;
end;

function TcxCustomSpinEditProperties.IsValueTypeStored: Boolean;
begin
  Result := AssignedValues.ValueType;
end;

procedure TcxCustomSpinEditProperties.ReadZeroIncrement(Reader: TReader);
begin
  Reader.ReadBoolean;
  Increment := 0;
end;

procedure TcxCustomSpinEditProperties.ReadZeroLargeIncrement(Reader: TReader);
begin
  Reader.ReadBoolean;
  LargeIncrement := 0;
end;

procedure TcxCustomSpinEditProperties.SetAssignedValues(AValue: TcxSpinEditPropertiesValues);
begin
  FAssignedValues.Assign(AValue);
end;

procedure TcxCustomSpinEditProperties.SetCircular(AValue: Boolean);
begin
  if AValue <> FCircular then
  begin
    FCircular := AValue;
    Changed;
  end;
end;

procedure TcxCustomSpinEditProperties.SetSpinButtons(AValue: TcxSpinEditButtons);
begin
  FSpinButtons.Assign(AValue);
end;

function TcxCustomSpinEditProperties.TryExtractDisplayFormat(const S: string): string;
var
  I: Integer;
  AExtraChars: string;
begin
  Result := S;
  AExtraChars := 'E+-' + dxFormatSettings.DecimalSeparator;
  I := 1;
  while I <= Length(Result) do
  begin
    if Pos(dxFormatSettings.CurrencyString, Result) = I then
      Inc(I, Length(dxFormatSettings.CurrencyString))
    else
      if (Pos(UpCase(Result[I]), AExtraChars) <> 0) or dxCharIsNumeric(Result[I]) then
        Inc(I)
      else
        if Pos(UpCase(Result[I]), DisplayFormat) <> 0 then
          Delete(Result, I, 1)
        else
          Inc(I);
  end;
end;

function TcxCustomSpinEditProperties.TryTextToValue(S: string;
  out AValue: TcxEditValue): Boolean;
begin
  Result := InternalTryTextToValue(S, AValue);
  if not Result and UseDisplayFormatWhenEditing then
    Result := InternalTryTextToValue(TryExtractDisplayFormat(S), AValue);
end;

procedure TcxCustomSpinEditProperties.SetValueType(AValue: TcxSpinEditValueType);
begin
  if AssignedValues.ValueType and (AValue = FValueType) then
    Exit;
  AssignedValues.FValueType := True;
  FValueType := AValue;
  Changed;
end;

function TcxCustomSpinEditProperties.VarToCurrentValueType(AValue: TcxEditValue): TcxEditValue;
begin
  if ValueType = vtFloat then
    Result := VarAsType(AValue, varDouble)
  else
    Result := VarAsType(AValue, varInt64);
end;

procedure TcxCustomSpinEditProperties.WriteZeroIncrement(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

procedure TcxCustomSpinEditProperties.WriteZeroLargeIncrement(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

{ TcxCustomSpinEdit }

destructor TcxCustomSpinEdit.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TcxCustomSpinEdit.ClearSelection;
begin
  if not (Focused and not ActiveProperties.CanEdit) then
  begin
    FIsCustomTextAction := True;
    try
      inherited ClearSelection;
    finally
      FIsCustomTextAction := False;
    end;
  end;
end;

procedure TcxCustomSpinEdit.CutToClipboard;
begin
  if not (Focused and not ActiveProperties.CanEdit) then
  begin
    FIsCustomTextAction := True;
    try
      inherited CutToClipboard;
    finally
      FIsCustomTextAction := False;
    end;
  end;
end;

class function TcxCustomSpinEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomSpinEditProperties;
end;

function TcxCustomSpinEdit.Increment(AButton: TcxSpinEditButton): Boolean;
var
  ADisplayValue: TcxEditValue;
  APrevText: string;
  AValue: Variant;
  APrevSelStart: Integer;
  APrevSelLength: Integer;
begin
  LockChangeEvents(True);
  try
    Result := False;
    if not DoEditing then
      Exit;
    if not GetNewValue(AButton, AValue) then
      Exit;

    APrevText := Text;

    ADisplayValue := IncrementValueToStr(AValue);
    APrevSelStart := SelStart;
    APrevSelLength := SelLength;
    SetInternalValue(AValue);
    SetInternalDisplayValue(ADisplayValue);

    Result := not InternalCompareString(APrevText, Text, True);
    if Result then
    begin
      ModifiedAfterEnter := True;
      if ActiveProperties.PreserveSelection then
        SetSelection(APrevSelStart, APrevSelLength)
      else
        SelStart := Length(Text);
      if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      begin
        InternalPostEditValue;
        SynchronizeDisplayValue;
      end;
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomSpinEdit.PasteFromClipboard;
begin
  if not (Focused and not ActiveProperties.CanEdit) then
  begin
    FIsCustomTextAction := True;
    try
      inherited PasteFromClipboard;
    finally
      FIsCustomTextAction := False;
    end;
  end;
end;

procedure TcxCustomSpinEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out AEditValue: TcxEditValue; AEditFocused: Boolean);
var
  AErrorText: TCaption;
begin
  InternalPrepareEditValue(ADisplayValue, True, AEditValue, AErrorText);
end;

procedure TcxCustomSpinEdit.ChangeHandler(Sender: TObject);
begin
  FIsCustomText := FIsCustomTextAction;
  inherited ChangeHandler(Sender);
  FIsCustomTextAction := False;
end;

procedure TcxCustomSpinEdit.CheckEditorValueBounds;
begin
  BeginUserAction;
  try
    with ActiveProperties do
      if Value < InternalMinValue then
        Value := InternalMinValue
      else
        if Value > InternalMaxValue then
          Value := InternalMaxValue;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomSpinEdit.DoButtonDown(AButtonVisibleIndex: Integer);

  procedure CreateTimer;
  begin
    if ActiveProperties.ReadOnly or not DataBinding.IsDataAvailable then
      Exit;
    FTimer.Free;
    FTimer := cxCreateTimer(HandleTimer, cxSpinEditTimerInitialInterval);
  end;

begin
  inherited DoButtonDown(AButtonVisibleIndex);
  if GetCaptureControl <> Self then
    ShortRefreshContainer(False);
  BeginUserAction;
  try
    if FPressedState = epsNone then
      if ViewInfo.PressedButton <> -1 then
      begin
        Increment(TcxSpinEditButton(ViewInfo.PressedButton));
        CreateTimer;
      end;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomSpinEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);
const
  APressedStateMap: array[TcxSpinEditButton] of TcxSpinEditPressedState =
    (epsDown, epsUp, epsFastDown, epsFastUp);
var
  AButton: TcxSpinEditButton;
begin
  FIsCustomTextAction := False;
  if ((Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_NEXT) or (Key = VK_PRIOR)) and
    not (ActiveProperties.UseCtrlIncrement and not (ssCtrl in Shift)) then
  begin
    BeginUserAction;
    try
      if not DataBinding.Modified and not DoEditing then
        Exit;
      case Key of
        VK_UP:
          AButton := sebForward;
        VK_DOWN:
          AButton := sebBackward;
        VK_PRIOR:
          AButton := sebFastForward;
        else
          AButton := sebFastBackward;
      end;
      PressedState := APressedStateMap[AButton];

      if HasNativeHandle(Self, GetCapture) then
        SetCaptureControl(nil);
      Increment(AButton);
    finally
      EndUserAction;
    end;
    Key := 0;
  end
  else
  begin
    StopTracking;
      if Key <> VK_ESCAPE then
        FIsCustomTextAction := True;
    if not ActiveProperties.CanEdit and CanKeyDownModifyEdit(Key, Shift) then
    begin
      DoAfterKeyDown(Key, Shift);
      Key := 0;
    end;
  end;

  if Key <> 0 then
    inherited DoEditKeyDown(Key, Shift);
end;

procedure TcxCustomSpinEdit.DoEditKeyPress(var Key: Char);
begin
  if (Key = '.') or (Key = ',') then
    Key := dxFormatSettings.DecimalSeparator;
  if IsTextChar(Key) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)) then
    if not IsValidChar(Key) then
    begin
      Key := #0;
      Beep;
    end
    else
      FIsCustomTextAction := True;
  if Key <> #0 then
    inherited DoEditKeyPress(Key);
end;

procedure TcxCustomSpinEdit.DoEditKeyUp(var Key: Word; Shift: TShiftState);
begin
  FIsCustomTextAction := False;
  inherited DoEditKeyUp(Key, Shift);
  if Key = 0 then
    Exit;
  if (Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_NEXT) or (Key = VK_PRIOR) then
    StopTracking;
end;

function TcxCustomSpinEdit.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := HandleMouseWheel(Shift);
  if Result then
    Increment(sebBackward);
end;

function TcxCustomSpinEdit.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := HandleMouseWheel(Shift);
  if Result then
    Increment(sebForward);
end;

function TcxCustomSpinEdit.GetNewValue(AButton: TcxSpinEditButton; out AValue: Variant): Boolean;
var
  ABoundsCheckingKind: TcxSpinBoundsCheckingKind;
  AActualButton: TcxSpinEditButton;
begin
  Result := False;

  AActualButton := AButton;
  if UseRightToLeftAlignment then
    case AActualButton of
      sebBackward    :
        if ActiveProperties.SpinButtons.Position <> sbpVert then
          AActualButton := sebForward;
      sebForward     :
        if ActiveProperties.SpinButtons.Position <> sbpVert then
          AActualButton := sebBackward;
      sebFastBackward:
        AActualButton := sebFastForward;
      sebFastForward :
        AActualButton := sebFastBackward;
    end;

  with ActiveProperties do
  begin
    ABoundsCheckingKind := ActiveProperties.GetBoundsCheckingKind;
    AValue := Value;
    if AValue < InternalMinValue then
      if AActualButton in [sebForward, sebFastForward] then
        AValue := InternalMinValue
      else
        if ABoundsCheckingKind = bckCircular then
          AValue := InternalMaxValue
        else
          Exit
    else
      if AValue > InternalMaxValue then
        if AActualButton in [sebBackward, sebFastBackward] then
          AValue := InternalMaxValue
        else
          if ABoundsCheckingKind = bckCircular then
            AValue := InternalMinValue
          else
            Exit
      else
      begin
        if not InternalIncrement(AValue, Self.GetIncrement(AActualButton)) then
          Exit;
      end;
  end;
  Result := True;
  AValue := ActiveProperties.SetVariantType(AValue);
end;

procedure TcxCustomSpinEdit.FocusChanged;
begin
  StopTracking;
  inherited FocusChanged;
end;

procedure TcxCustomSpinEdit.Initialize;
begin
  inherited Initialize;
  InternalEditValue := 0;
end;

function TcxCustomSpinEdit.InternalGetEditingValue: TcxEditValue;
begin
  Result := Value;
end;

function TcxCustomSpinEdit.InternalGetNotPublishedStyleValues: TcxEditStyleValues;
begin
  Result := inherited InternalGetNotPublishedStyleValues -
    [svButtonStyle, svButtonTransparency, svGradientButtons];
end;

function TcxCustomSpinEdit.InternalIncrement(var AValue: Variant; AIncrement: Variant): Boolean;

  function GetMinMaxValueDelta(const AValue: Variant; AIsMin: Boolean): Variant;
  var
    ADelta, AMaxRange, ARange: Variant;
  begin
    if AIsMin then
    begin
      AMaxRange := ActiveProperties.GetMaxMinValueForCurrentValueType;
      ARange := ActiveProperties.InternalMinValue;
    end
    else
    begin
      AMaxRange := ActiveProperties.GetMaxMinValueForCurrentValueType(False);
      ARange := ActiveProperties.InternalMaxValue;
    end;

    if (ARange > 0) and (AValue > 0) or (ARange < 0) and (AValue < 0) then
        Result := ARange - AValue
    else
    begin
      ADelta := AMaxRange - ARange;
      ADelta := ADelta + AValue;
      if (ADelta < 0) and (AValue < 0) or (ADelta > 0) and (AValue > 0) then
        Result := AMaxRange
      else
        Result := ARange - AValue;
    end;
  end;

var
  AMaxValueDelta, AMinValueDelta: TcxEditValue;
  AInternalValueString: string;
  AInternalValue, AInternalIncrement: Double;
begin
  Result := True;
  AIncrement := ActiveProperties.VarToCurrentValueType(AIncrement);
  AMinValueDelta := GetMinMaxValueDelta(AValue, True);
  AMaxValueDelta := GetMinMaxValueDelta(AValue, False);
  if (AIncrement < AMinValueDelta) or (AIncrement > AMaxValueDelta) then
    case ActiveProperties.GetBoundsCheckingKind of
      bckDoNotExceed:
        begin
          Result := False;
          Exit;
        end;
      bckExtendToBound:
        if AIncrement < AMinValueDelta then
          AValue := ActiveProperties.InternalMinValue
        else
          AValue := ActiveProperties.InternalMaxValue;
      bckCircular:
        if AIncrement < AMinValueDelta then
          AValue := ActiveProperties.InternalMaxValue + AIncrement  + 1 - AMinValueDelta
        else
          AValue := ActiveProperties.InternalMinValue + AIncrement - 1 - AMaxValueDelta;
    end
  else
  begin
    if ActiveProperties.ValueType = vtFloat then
    begin
      AInternalValueString := FloatToStr(AValue);
      StrToFloatEx(AInternalValueString, AInternalValue);
      AInternalIncrement := AIncrement;
      AInternalValue := AInternalValue + AInternalIncrement;
      AValue := AInternalValue;
    end
    else
      AValue := AValue + AIncrement;
  end;
end;

procedure TcxCustomSpinEdit.InternalStoreEditValue(const Value: TcxEditValue);
begin
  inherited;
  SetInternalValue(FEditValue);
end;

function TcxCustomSpinEdit.IsEditValueStored: Boolean;
begin
  Result := EditValue <> 0;
end;

function TcxCustomSpinEdit.IsValidChar(AChar: Char): Boolean;
begin
  with ActiveProperties do
    if CanEdit and IsOnGetValueEventAssigned then
    begin
      Result := True;
      Exit;
    end;
  if ActiveProperties.ValueType = vtFloat then
    Result := IsNumericChar(AChar, ntExponent)
  else
    Result := IsNumericChar(AChar, ntInteger) or (AChar = 'e') or (AChar = 'E');
  Result := Result or (AChar < #32) or (AnsiChar(AChar) in ActiveProperties.ValidChars);
  if not ActiveProperties.CanEdit and Result and ((AChar >= #32) or
      (AChar = Char(#8)) or (AChar = Char(VK_DELETE))) then
    Result := False;
end;

procedure TcxCustomSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ButtonVisibleIndexAt(Point(X, Y)) = -1 then
    StopTracking;
end;

procedure TcxCustomSpinEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomSpinEdit.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  if not DataBinding.CanCheckEditorValue and ModifiedAfterEnter then
    CheckEditorValueBounds;
end;

function TcxCustomSpinEdit.DoRefreshContainer(const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  with ViewInfo do
    if (Length(ButtonsInfo) > 0) and (PressedButton = -1) and (CaptureButtonVisibleIndex = -1) then
      StopTracking;
end;

procedure TcxCustomSpinEdit.SynchronizeDisplayValue;
var
  AValue: TcxEditValue;
begin
  AValue := EditValue;
  if ActiveProperties.IsEditValueValid(AValue, Focused) then
    SetInternalValue(ActiveProperties.PrepareValue(AValue))
  else
    SetInternalValue(ActiveProperties.CheckValueBounds(0));
  inherited SynchronizeDisplayValue;
end;

procedure TcxCustomSpinEdit.DoOnGetValue(const AText: TCaption;
  out AValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  with Properties do
    if Assigned(OnGetValue) then
      OnGetValue(Self, AText, AValue, ErrorText, Error);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnGetValue) then
        OnGetValue(Self, AText, AValue, ErrorText, Error);
end;

function TcxCustomSpinEdit.GetIncrement(AButton: TcxSpinEditButton): Double;
begin
  Result := ActiveProperties.GetIncrement(AButton);
  if ActiveProperties.ValueType = vtInt then
    Result := Round(Result);
end;

function TcxCustomSpinEdit.GetProperties: TcxCustomSpinEditProperties;
begin
  Result := TcxCustomSpinEditProperties(inherited Properties);
end;

function TcxCustomSpinEdit.GetActiveProperties: TcxCustomSpinEditProperties;
begin
  Result := TcxCustomSpinEditProperties(InternalGetActiveProperties);
end;

function TcxCustomSpinEdit.GetValue: Variant;
begin
  if Focused then
  begin
    if FIsCustomText then
      PrepareEditValue(Text, Result, Focused)
    else
      Result := FInternalValue;
    if not VarIsNumericEx(Result) then // Null
      Result := ActiveProperties.CheckValueBounds(0);
  end
  else
  begin
    if VarIsNumericEx(EditValue) then
      Result := EditValue
    else
      if VarIsStr(EditValue) then
      begin
        if EditValue = '' then
          Result := ActiveProperties.CheckValueBounds(0)
        else
          if not ActiveProperties.TryTextToValue(EditValue, Result) then
            Result := ActiveProperties.CheckValueBounds(0);
      end
      else
        Result := ActiveProperties.CheckValueBounds(0);
  end;
  Result := ActiveProperties.SetVariantType(Result);
end;

function TcxCustomSpinEdit.IncrementValueToStr(const AValue: TcxEditValue): string;
var
  ADisplayValue: TcxEditValue;
begin
  ActiveProperties.PrepareDisplayValue(AValue, ADisplayValue, InternalFocused);
  Result := ADisplayValue;
end;

function TcxCustomSpinEdit.InternalPrepareEditValue(
  const ADisplayValue: TcxEditValue; ARaiseException: Boolean;
  out AEditValue: TcxEditValue; out AErrorText: TCaption): Boolean;
var
  AError: Boolean;
  AValue: Variant;
begin
  Result := True;
  if ADisplayValue = '' then
    if (ActiveProperties.IDefaultValuesProvider <> nil) and ActiveProperties.IDefaultValuesProvider.DefaultRequired then
      AEditValue := ActiveProperties.CheckValueBounds(0)
    else
      AEditValue := Null
  else
  begin
    AError := not ActiveProperties.TryTextToValue(VarToStr(ADisplayValue), AValue);
    if AError then
    begin
      AErrorText := '';
      if IsOnGetValueEventAssigned then
        try
          DoOnGetValue(ADisplayValue, AValue, AErrorText, AError);
        except
          on E: Exception do
            if ARaiseException then
              raise
            else
            begin
              Result := False;
              AErrorText := E.Message;
              Exit;
            end;
        end;
    end;

    if AError then
    begin
      if AErrorText = '' then
        AErrorText := cxGetResourceString(@cxSSpinEditInvalidNumericValue);
      if not ActiveProperties.ExceptionOnInvalidInput then
        AErrorText := '';
      if ARaiseException and ActiveProperties.ExceptionOnInvalidInput then
        raise EcxEditError.Create(AErrorText);
      AValue := FInternalValue;
    end;
    AEditValue := AValue;
    Result := not AError;
  end;
//  ActiveProperties.CheckValueBounds(AEditValue);
  AEditValue := ActiveProperties.SetVariantType(AEditValue);
end;

function TcxCustomSpinEdit.IsOnGetValueEventAssigned: Boolean;
begin
  Result := Assigned(Properties.OnGetValue) or
    Assigned(ActiveProperties.OnGetValue);
end;

procedure TcxCustomSpinEdit.HandleTimer(Sender: TObject);
begin
  if FTimer.Interval = cxSpinEditTimerInitialInterval then
    FTimer.Interval := cxSpinEditTimerInterval;
  if ViewInfo.PressedButton <> -1 then
    Increment(TcxSpinEditButton(ViewInfo.PressedButton));
end;

function TcxCustomSpinEdit.IsValueStored: Boolean;
begin
  Result := not VarEqualsExact(Value, 0);
end;

procedure TcxCustomSpinEdit.SetInternalValue(AValue: TcxEditValue);
begin
  FInternalValue := ActiveProperties.SetVariantType(AValue);
end;

procedure TcxCustomSpinEdit.SetPressedState(Value: TcxSpinEditPressedState);
const
  ASpinButtonMap: array[TcxSpinEditPressedState] of TcxSpinEditButton =
    (sebBackward, sebBackward, sebForward, sebFastBackward, sebFastForward);
var
  I: Integer;
begin
  if (Length(ViewInfo.ButtonsInfo) > 0) and (Value <> FPressedState) then
  begin
    FPressedState := Value;
    CalculateViewInfo(False);
    for I := 0 to Length(ViewInfo.ButtonsInfo) - 1 do
      InvalidateRect(ViewInfo.ButtonsInfo[I].Bounds, False);
  end;
end;

procedure TcxCustomSpinEdit.SetProperties(Value: TcxCustomSpinEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomSpinEdit.SetValue(const Value: Variant);
var
  AValue: TcxEditValue;
begin
  if VarIsNumericEx(Value) then
  begin
    AValue := Value;
    if ActiveProperties.IsEditValueValid(AValue, Focused) then
      InternalEditValue := AValue;
  end;
end;

procedure TcxCustomSpinEdit.StopTracking;
var
  I: Integer;
begin
  FreeAndNil(FTimer);
  CaptureButtonVisibleIndex := -1;
  if Length(ViewInfo.ButtonsInfo) > 0 then
    for I := 0 to 1 do
      if ViewInfo.ButtonsInfo[I].Data.State = ebsPressed then
        ViewInfo.ButtonsInfo[I].Data.State := ebsNormal;
  PressedState := epsNone;

  if GetCaptureControl = Self then
    SetCaptureControl(nil);
end;

{ TcxSpinEdit }

class function TcxSpinEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties;
end;

function TcxSpinEdit.GetActiveProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(InternalGetActiveProperties);
end;

function TcxSpinEdit.GetProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(inherited Properties);
end;

procedure TcxSpinEdit.SetProperties(Value: TcxSpinEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterSpinEditHelper }

class function TcxFilterSpinEditHelper.EditPropertiesHasButtons: Boolean;
begin
  Result := True;
end;

class function TcxFilterSpinEditHelper.GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtNumeric;
end;

class function TcxFilterSpinEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxSpinEdit;
end;

class function TcxFilterSpinEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual..fcoGreaterEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoBetween..fcoNotInList]
end;

class procedure TcxFilterSpinEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomSpinEditProperties(AProperties) do
    CanEdit := True;
end;

initialization
  GetRegisteredEditProperties.Register(TcxSpinEditProperties, scxSEditRepositorySpinItem);
  FilterEditsController.Register(TcxSpinEditProperties, TcxFilterSpinEditHelper);

finalization
  FilterEditsController.Unregister(TcxSpinEditProperties, TcxFilterSpinEditHelper);

end.
