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

unit cxTrackBar;

{$I cxVer.inc}

interface

uses
  Types, Variants,
  Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls,
  cxEdit, cxExtEditConsts, cxLookAndFeelPainters,
  cxFilterControlUtils, cxGraphics, cxLookAndFeels, cxTextEdit, cxVariants, Math, dxCustomHint;

type
  TcxTrackBarOrientation = (tboHorizontal, tboVertical);
  TcxTrackBarTextOrientation = (tbtoHorizontal, tbtoVertical);
  TcxTrackBarTickMarks = (cxtmBoth, cxtmTopLeft, cxtmBottomRight);
  TcxTrackBarTickType = (tbttTicks, tbttNumbers, tbttValueNumber, tbttTicksAndNumbers);
  TcxTrackBarMouseState = (tbmpInControl, tbmpUnderThumb, tbmpSliding);
  TcxTrackBarMouseStates = set of TcxTrackBarMouseState;
  TcxTrackBarSlideState = (tbksNormal, tbksIncludeSelection);
  TcxTrackBarThumbType = (cxttNone, cxttRegular, cxttCustom);
  TcxTrackBarThumbStep = (cxtsNormal, cxtsJump);

  TcxTrackBarGetCustomTextEvent = procedure(Sender: TObject; const APosition: Integer; var AText: string) of object;
  TcxTrackBarGetCustomHintEvent = procedure(Sender: TObject; const APosition: Integer; var AHintText: string; var ACanShow, AIsHintMultiLine: Boolean) of object;
  TcxGetThumbRectEvent = procedure(Sender: TObject; var ARect: TRect) of object;
  TcxDrawThumbEvent = procedure(Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect) of object;

  TcxCustomTrackBar = class;
  TcxTrackBarPositionHintHelper = class;
  TcxCustomTrackBarProperties = class;
  TcxCustomTrackBarViewInfo = class;

  TdxDouble = record
    Numerator: Integer;
    Denominator: Integer;
  end;

  IcxTrackBar = interface
    function GetActiveProperties: TcxCustomTrackBarProperties;
    function IsInplace: Boolean;
    function GetMouseDownPos: TPoint;
    function GetPosition: Integer;
    function GetProperties: TcxCustomTrackBarProperties;
    function GetViewInfo: TcxCustomTrackBarViewInfo;
    procedure SetPosition(AValue: Integer);
  end;

  { TcxTrackBarStyle }

  TcxTrackBarStyle = class(TcxEditStyle)
  protected
    function DefaultBorderStyle: TcxContainerBorderStyle; override;
    function DefaultHotTrack: Boolean; override;
  end;

  { TcxCustomTrackBarFadingHelper }

  TcxCustomTrackBarFadingHelper = class(TcxCustomEditFadingHelper)
  private
    FViewInfo: TcxCustomTrackBarViewInfo;
  protected
    function GetEditViewInfo: TcxCustomEditViewInfo; override;
  public
    constructor Create(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    //
    property ViewInfo: TcxCustomTrackBarViewInfo read FViewInfo;
  end;

  { TcxTrackBarCustomButtonFadingHelper }

  TcxTrackBarCustomButtonFadingHelper = class(TcxCustomTrackBarFadingHelper)
  protected
    function CanFade: Boolean; override;
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual; abstract;
    function GetButtonBounds: TRect; virtual; abstract;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
  public
    procedure Invalidate; override;
    //
    property ButtonBounds: TRect read GetButtonBounds;
  end;

  { TcxTrackBarLeftButtonFadingHelper }

  TcxTrackBarLeftButtonFadingHelper = class(TcxTrackBarCustomButtonFadingHelper)
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    function GetButtonBounds: TRect; override;
  end;

  { TcxTrackBarRightButtonFadingHelper }

  TcxTrackBarRightButtonFadingHelper = class(TcxTrackBarCustomButtonFadingHelper)
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    function GetButtonBounds: TRect; override;
  end;

  { TcxTrackBarThumbFadingHelper }

  TcxTrackBarThumbFadingHelper = class(TcxCustomTrackBarFadingHelper)
  protected
    procedure DrawThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer); virtual;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
    function GetThumbRect: TRect; virtual;
  public
    procedure Invalidate; override;
  end;

  { TcxHorizontalTrackBarViewInfoHelper }

  TcxHorizontalTrackBarViewInfoHelper = class(TObject)
  private
    FViewInfo: TcxCustomTrackBarViewInfo;
  protected
    function GetCurrentMousePos(X, Y: Integer): Integer; virtual;
    function GetMouseDownPos(AMouseDownCoordinate: TPoint): Integer; virtual;

    procedure CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer); virtual;
    procedure DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer); virtual;
    procedure DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer); virtual;
    function GetClientRect: TRect; virtual;
    function GetLeftButtonRect: TRect; virtual;
    function GetPositionHintTextRect(ACanvas: TcxCanvas; const AHintText: string; AIsHintMultiLine: Boolean): TRect; virtual;
    function GetPositionHintSize(ACanvas: TcxCanvas; const AHintText: string): Integer; virtual;
    function GetRightButtonRect: TRect; virtual;
    function GetSelectionRect: TRect; virtual;
    function GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect; virtual;
    function GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect; virtual;
    function GetRealRect(const R: TRect): TRect; virtual;
    function GetThumbRect: TRect; virtual;
    function GetTrackBarRect: TRect; virtual;
    function GetTrackBounds: TRect; virtual;
    function GetTrackZoneRect: TRect; virtual;
    function GetTrackRect: TRect; virtual;
    function GetVisibleThumbRect: TRect; virtual;
    procedure InflateTrackRect(var ARect: TRect); virtual;

    property ViewInfo: TcxCustomTrackBarViewInfo read FViewInfo write FViewInfo;
  public
    constructor Create(AViewInfo: TcxCustomTrackBarViewInfo);
  end;

  { TcxVerticalTrackBarViewInfoHelper }

  TcxVerticalTrackBarViewInfoHelper = class(TcxHorizontalTrackBarViewInfoHelper)
  protected
    function GetCurrentMousePos(X, Y: Integer): Integer; override;
    function GetMouseDownPos(AMouseDownPos: TPoint): Integer; override;

    procedure CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer); override;
    procedure DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer); override;
    procedure DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer); override;
    function GetPositionHintSize(ACanvas: TcxCanvas; const AHintText: string): Integer; override;
    function GetRealRect(const R: TRect): TRect; override;
    function GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect; override;
    function GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect; override;
    function GetTrackBounds: TRect; override;
    procedure InflateTrackRect(var ARect: TRect); override;

    function GetRotatedRect(ARect: Trect): TRect;
  end;

  { TcxTrackBarThumbViewInfo }

  TcxTrackBarThumbViewInfo = class
  private
    FBounds: TRect;
    FFadingHelper: TcxTrackBarThumbFadingHelper;
    FFullRect: TRect;
    FPosition: Integer;
    FState: Integer;
    FViewInfo: TcxCustomTrackBarViewInfo;
    procedure SetState(AValue: Integer);
  protected
    function CreateFadingHelper: TcxTrackBarThumbFadingHelper; virtual;
    property ViewInfo: TcxCustomTrackBarViewInfo read FViewInfo;
  public
    constructor Create(AViewInfo: TcxCustomTrackBarViewInfo);
    destructor Destroy; override;
    property Bounds: TRect read FBounds write FBounds;
    property Position: Integer read FPosition write FPosition;
    property State: Integer read FState write SetState;
    property FullRect: TRect read FFullRect write FFullRect;

    property FadingHelper: TcxTrackBarThumbFadingHelper read FFadingHelper;
  end;

  { TcxCustomTrackBarViewInfo }

  TcxCustomTrackBarViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FButtonLeftFadingHelper: TcxTrackBarLeftButtonFadingHelper;
    FButtonLeftState: TcxButtonState;
    FButtonRightFadingHelper: TcxTrackBarRightButtonFadingHelper;
    FButtonRightState: TcxButtonState;
    FLookAndFeel: TcxLookAndFeel;
    FOrientationHelper: TcxHorizontalTrackBarViewInfoHelper;
    FPositionHintHelper: TcxTrackBarPositionHintHelper;
    FMajorTicks: TList;
    FMinorTicks: TList;
    FPropTransparent: Boolean;
    FSelectionEnd: Integer;
    FSelectionStart: Integer;
    FShowSelection: Boolean;
    FTBBitmap: TcxBitmap;
    FThumbHeight: Integer;
    FThumbWidth: Integer;
    FThumbs: TList<TcxTrackBarThumbViewInfo>;
    FTrackSize: Integer;
    FHotThumb: TcxTrackBarThumbViewInfo;
    FPressedThumb: TcxTrackBarThumbViewInfo;

    function GetLeftButtonSize: TSize;
    function GetPosition: Integer;
    function GetRightButtonSize: TSize;
    function GetThumbFadingHelper: TcxTrackBarThumbFadingHelper;
    function GetThumb(Index: Integer): TcxTrackBarThumbViewInfo;
    function GetTrackBarProperties: TcxCustomTrackBarProperties;
    function GetTrackBarState: Integer;
    function IsMajorTick(AIndex: Integer): Boolean;
    procedure SetButtonLeftState(Value: TcxButtonState);
    procedure SetButtonRightState(Value: TcxButtonState);
    procedure SetChangeButtonsState(Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetTrackBarState(AValue: Integer);
  protected
    LeftButtonRect: TRect;
    RealTrackBarRect: TRect;
    RightButtonRect: TRect;
    SelectionRect: TRect;
    ThumbRect: TRect;
    TrackBarRect: TRect;
    TrackRect: TRect;
    TrackZoneRect: TRect;

    procedure AddMinorTicks(AMin, AMax, AFrequency: Integer);
    procedure PopulateMajorTicks; virtual;
    procedure PopulateMinorTicks; virtual;
    procedure DoGetPositionHint(var AHintText: string; var ACanShow: Boolean; var AIsHintMultiLine: Boolean); virtual;

    procedure DrawButtons(ACanvas: TcxCanvas);
    procedure DrawButtonLeft(ACanvas: TcxCanvas); overload;
    procedure DrawButtonLeft(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); overload;
    procedure DrawButtonRight(ACanvas: TcxCanvas); overload;
    procedure DrawButtonRight(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); overload;
    procedure DrawCustomThumb(ACanvas: TcxCanvas); virtual;
    procedure DrawTrack(ACanvas: TcxCanvas); virtual;
    procedure DrawSelection(ACanvas: TcxCanvas); virtual;
    procedure DrawTickAsLine(ACanvas: TcxCanvas; AIndex, ATickPosition: Integer);
    procedure DrawTickAsText(ACanvas: TcxCanvas; AIndex, ATickPosition: Integer);
    procedure DrawTicks(ACanvas: TcxCanvas); virtual;
    procedure DrawThumb(ACanvas: TcxCanvas); overload; virtual;
    procedure DrawThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer); overload; virtual;
    function DrawingThumbRectToRealThumbRect(ACanvas: TcxCanvas; const AThumbRect: TRect): TRect; overload; virtual;
    function DrawingThumbRectToRealThumbRect(ACanvas: TcxCanvas): TRect; overload; virtual;
    function GetCoordinate(APosition: Integer): Integer; virtual;
    function GetOffset(APosition: Integer): Integer; virtual;
    function GetThumbRealColor(AState: Integer): TColor;
    function IsTickText(ATickValue: Integer): Boolean; virtual;
    procedure UpdateTrackBarState; virtual;
    procedure UpdateValue(AValue: TcxEditValue); virtual;

    function IsPtToTheRightFromThumb(AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean; virtual;
    function IsPtToTheLeftFromThumb(AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean; virtual;
    function GetCalculatedPosition(APosition: Integer; AIsInc: Boolean;
      AIsLineSize: Boolean; ANeedRound: Boolean = False): Integer; virtual;
    function GetDefaultPositionHintText: string; virtual;
    function GetMouseDelta(X, Y: Integer; AMouseDownPos: TPoint): Integer;
    function GetNewPosition(X, Y: Integer): Integer; overload;
    function GetPositionAfterJump(X, Y: Integer): Integer; virtual;
    function GetPositionHintText(var ACanShow: Boolean; var AIsHintMultiLine: Boolean): string;

    // position hint
    procedure HidePositionHint;
    procedure MouseMove(X, Y: Integer);
    procedure ProcessPositionHint;

    // orientation helper
    procedure CreateOrientationHelper;
    function GetCurrentMousePos(X, Y: Integer): Integer;
    function GetMouseDownPos(AMouseDownPos: TPoint): Integer;
    procedure CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer);
    procedure DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
    procedure DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
    function GetClientRect: TRect;
    function GetLeftButtonRect: TRect;
    function GetPositionHintTextRect(ACanvas: TcxCanvas; const AHintText: string; AIsHintMultiLine: Boolean): TRect;
    function GetRightButtonRect: TRect;
    function GetSelectionRect: TRect;
    function GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
    function GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
    function GetRealRect(const R: TRect): TRect;
    function GetThumbRect: TRect;
    function GetTrackBarRect: TRect;
    function GetTrackBounds: TRect;
    function GetTrackRect: TRect;
    function GetVisibleThumbRect: TRect;

    procedure AddThumb(AThumb: TcxTrackBarThumbViewInfo);
    procedure AddThumbs; virtual;
    procedure CreateThumbs; virtual;
    procedure DestroyThumbs; virtual;
    function CreateThumbViewInfo: TcxTrackBarThumbViewInfo; virtual;
    function GetNearestThumb(X, Y: Integer): TcxTrackBarThumbViewInfo; virtual;
    function GetThumbFromPos(const APoint: TPoint): TcxTrackBarThumbViewInfo; virtual;

    procedure PaintTrackBar(ACanvas: TcxCanvas); virtual;
    //
    property ShowSelection: Boolean read FShowSelection write FShowSelection;
    property ThumbFadingHelper: TcxTrackBarThumbFadingHelper read GetThumbFadingHelper;
    property Thumb[Index: Integer]: TcxTrackBarThumbViewInfo read GetThumb;
    property MajorTicks: TList read FMajorTicks;
    property MinorTicks: TList read FMinorTicks;
  public
    FocusRect: TRect;
    ReverseDirection: Boolean;
    ShowChangeButtons: Boolean;
    VisibleThumbRect: TRect;

    HasForegroundImage: Boolean;
    MouseStates: TcxTrackBarMouseStates;

    Min: Integer;
    Max: Integer;
    Frequency: Integer;

    ThumbLargeSize: Integer;
    ThumbSize: Integer;

    TickColor: TColor;
    TickMarks: TcxTrackBarTickMarks;
    PixelsPerPositionStep: TdxDouble;
    TickType: TcxTrackBarTickType;
    TickSize: Integer;

    TrackRectDelta: Integer;
    TrackBarBorderWidth: Integer;
    TrackHeight: Integer;
    TrackWidth: Integer;

    Orientation: TcxTrackBarOrientation;
    TextOrientation: TcxTrackBarTextOrientation;

    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;
    procedure DrawText(ACanvas: TcxCanvas); override;
    function IsHotTrack: Boolean; overload; override;
    function IsHotTrack(P: TPoint): Boolean; overload; override;
    function NeedShowHint(ACanvas: TcxCanvas; const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Offset(DX, DY: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write FLookAndFeel;

    property ButtonLeftFadingHelper: TcxTrackBarLeftButtonFadingHelper read FButtonLeftFadingHelper;
    property ButtonLeftState: TcxButtonState read FButtonLeftState write SetButtonLeftState;
    property ButtonRightFadingHelper: TcxTrackBarRightButtonFadingHelper read FButtonRightFadingHelper;
    property ButtonRightState: TcxButtonState read FButtonRightState write SetButtonRightState;
    property LeftButtonSize: TSize read GetLeftButtonSize;
    property OrientationHelper: TcxHorizontalTrackBarViewInfoHelper read FOrientationHelper;
    property Position: Integer read GetPosition write SetPosition;
    property PositionHintHelper: TcxTrackBarPositionHintHelper read FPositionHintHelper;
    property RightButtonSize: TSize read GetRightButtonSize;
    property SelectionEnd: Integer read FSelectionEnd write FSelectionEnd;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart;
    property ThumbHeight: Integer read FThumbHeight write FThumbHeight;
    property ThumbWidth: Integer read FThumbWidth write FThumbWidth;
    property TrackBarProperties: TcxCustomTrackBarProperties read GetTrackBarProperties;
    property TrackBarState: Integer read GetTrackBarState write SetTrackBarState;
    property TrackSize: Integer read FTrackSize write FTrackSize;

    property HotThumb: TcxTrackBarThumbViewInfo read FHotThumb write FHotThumb;
    property PressedThumb: TcxTrackBarThumbViewInfo read FPressedThumb write FPressedThumb;
  end;

  { TcxCustomTrackBarViewData }

  TcxCustomTrackBarViewData = class(TcxCustomEditViewData)
  private
    procedure GetCustomRealTrackZoneSize(AViewInfo: TcxCustomTrackBarViewInfo; var ARealTrackZoneSize: Integer);
    procedure GetOnGetThumbRect(out AValue: TcxGetThumbRectEvent);
    function GetProperties: TcxCustomTrackBarProperties;
  protected
    procedure CalculateCustomTrackBarRects(ACanvas: TcxCanvas;
      AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function CalculateTickSize(ACanvas: TcxCanvas;
      AViewInfo: TcxCustomTrackBarViewInfo; ALeftTop: Boolean): Integer; virtual;
    procedure CalculateTrackBarViewInfoProperties(AViewInfo: TcxCustomEditViewInfo); virtual;
    function ThumbHalfSize(AViewInfo: TcxCustomTrackBarViewInfo): Integer;
    procedure CalculateButtonLeftRect(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateButtonRightRect(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculatePixelsPerPositionStep(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateSelectionRect(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateThumbSize(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateThumbRect(ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateTrackBarRect(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateTrackRect(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure CalculateTrackSize(AViewInfo: TcxCustomTrackBarViewInfo; ARealTrackZoneSize: Integer); virtual;
    procedure CalculateTrackZoneRect(ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo; out ARealTrackZoneSize: Integer); virtual;
    procedure DoOnGetThumbRect(var ARect: TRect);
    function IsOnGetThumbRectEventAssigned: Boolean;
    function IsReverseDirection: Boolean;
    procedure UpdateSelection(AViewInfo: TcxCustomTrackBarViewInfo); virtual;
    procedure UpdateValue(AViewInfo: TcxCustomTrackBarViewInfo; AValue: TcxEditValue); virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    property Properties: TcxCustomTrackBarProperties read GetProperties;
  end;

  { TcxCustomTrackBarProperties }

  TcxCustomTrackBarProperties = class(TcxCustomEditProperties)
  private
    FAutoSize: Boolean;
    FBorderWidth: Integer;
    FReverseDirection: Boolean;
    FFrequency: Integer;
    FLineSize: TcxNaturalNumber;
    FOrientation: TcxTrackBarOrientation;
    FTextOrientation: TcxTrackBarTextOrientation;
    FPageSize: TcxNaturalNumber;
    FSelectionStart: Integer;
    FSelectionEnd: Integer;
    FSelectionColor: TColor;
    FShowPositionHint: Boolean;
    FShowTrack: Boolean;
    FThumbType: TcxTrackBarThumbType;
    FTickColor: TColor;
    FTickType: TcxTrackBarTickType;
    FTickMarks: TcxTrackBarTickMarks;
    FTickSize: TcxNaturalNumber;
    FTrackColor: TColor;
    FThumbColor: TColor;
    FThumbHighlightColor: TColor;
    FThumbStep: TcxTrackBarThumbStep;

    FOnGetPositionHint: TcxTrackBarGetCustomHintEvent;
    FOnGetTickLabel: TcxTrackBarGetCustomTextEvent;
    FOnGetThumbRect: TcxGetThumbRectEvent;
    FOnDrawThumb: TcxDrawThumbEvent;

    procedure SetAutoSize(Value: Boolean);
    procedure SetBorderWidth(Value: Integer);
    procedure SetReverseDirection(Value: Boolean);
    procedure SetFrequency(Value: Integer);
    procedure SetLineSize(Value: TcxNaturalNumber);
    procedure SetOrientation(Value: TcxTrackBarOrientation);
    procedure SetTextOrientation(Value: TcxTrackBarTextOrientation);
    procedure SetPageSize(Value: TcxNaturalNumber);
    procedure SetSelectionStart(Value: Integer);
    procedure SetSelectionEnd(Value: Integer);
    procedure SetSelectionColor(Value: TColor);
    procedure SetShowTicks(Value: Boolean);
    procedure SetThumbType(Value: TcxTrackBarThumbType);
    procedure SetShowChangeButtons(Value: Boolean);
    procedure SetShowPositionHint(Value: Boolean);
    procedure SetShowTrack(Value: Boolean);
    procedure SetTickColor(Value: TColor);
    procedure SetTickType(Value: TcxTrackBarTickType);
    procedure SetTickMarks(Value: TcxTrackBarTickMarks);
    procedure SetTickSize(Value: TcxNaturalNumber);
    procedure SetTrackColor(Value: TColor);
    procedure SetTrackSize(Value: Integer);
    procedure SetThumbHeight(Value: Integer);
    procedure SetThumbWidth(Value: Integer);
    procedure SetThumbColor(Value: TColor);
    procedure SetThumbHighlightColor(Value: TColor);
    procedure DoDrawThumb(Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
  protected
    FMin: Integer;
    FMax: Integer;
    FShowChangeButtons: Boolean;
    FShowTicks: Boolean;
    FThumbHeight: Integer;
    FThumbWidth: Integer;
    FTrackSize: Integer;

    function CanValidate: Boolean; override;
    function GetValidateErrorText(AErrorKind: TcxEditErrorKind): string; override;

    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;
    function FixPosition(const APosition: Integer): Integer; virtual;
    function EditValueToPosition(const AEditValue: TcxEditValue): Integer;
    procedure SetMax(Value: Integer); virtual;
    procedure SetMin(Value: Integer); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property ReverseDirection: Boolean read FReverseDirection write SetReverseDirection default False;
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property LineSize: TcxNaturalNumber read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TcxTrackBarOrientation read FOrientation write SetOrientation default tboHorizontal;
    property PageSize: TcxNaturalNumber read FPageSize write SetPageSize default 2;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property SelectionEnd: Integer read FSelectionEnd write SetSelectionEnd default 0;
    property SelectionStart: Integer read FSelectionStart write SetSelectionStart default 0;
    property ShowChangeButtons: Boolean read FShowChangeButtons write SetShowChangeButtons default False;
    property ShowPositionHint: Boolean read FShowPositionHint write SetShowPositionHint default False;
    property ShowTicks: Boolean read FShowTicks write SetShowTicks default True;
    property ShowTrack: Boolean read FShowTrack write SetShowTrack default True;
    property TextOrientation: TcxTrackBarTextOrientation read FTextOrientation write SetTextOrientation default tbtoHorizontal;
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clBtnFace;
    property ThumbHeight: Integer read FThumbHeight write SetThumbHeight default 12;
    property ThumbHighlightColor: TColor read FThumbHighlightColor write SetThumbHighlightColor default clSilver;
    property ThumbStep: TcxTrackBarThumbStep read FThumbStep write FThumbStep default cxtsNormal;
    property ThumbType: TcxTrackBarThumbType read FThumbType write SetThumbType default cxttRegular;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth default 7;
    property TickColor: TColor read FTickColor write SetTickColor default clWindowText;
    property TickMarks: TcxTrackBarTickMarks read FTickMarks write SetTickMarks default cxtmBottomRight;
    property TickSize: TcxNaturalNumber read FTickSize write SetTickSize default 3;
    property TickType: TcxTrackBarTickType read FTickType write SetTickType default tbttTicks;
    property TrackColor: TColor read FTrackColor write SetTrackColor default clWindow;
    property TrackSize: Integer read FTrackSize write SetTrackSize default 5;

    property OnGetPositionHint: TcxTrackBarGetCustomHintEvent read FOnGetPositionHint write FOnGetPositionHint;
    property OnGetTickLabel: TcxTrackBarGetCustomTextEvent read FOnGetTickLabel write FOnGetTickLabel;
    property OnDrawThumb: TcxDrawThumbEvent read FOnDrawThumb write FOnDrawThumb;
    property OnGetThumbRect: TcxGetThumbRectEvent read FOnGetThumbRect write FOnGetThumbRect;
  end;

  { TcxTrackBarProperties }

  TcxTrackBarProperties = class(TcxCustomTrackBarProperties)
  published
    property AutoSize;
    property BorderWidth;
    property ClearKey;
    property Frequency;
    property LineSize;
    property Max;
    property Min;
    property Orientation;
    property PageSize;
    property ReverseDirection;
    property SelectionColor;
    property SelectionEnd;
    property SelectionStart;
    property ShowChangeButtons;
    property ShowPositionHint;
    property ShowTicks;
    property ShowTrack;
    property TextOrientation;
    property ThumbColor;
    property ThumbHeight;
    property ThumbHighlightColor;
    property ThumbStep;
    property ThumbType;
    property ThumbWidth;
    property TickColor;
    property TickMarks;
    property TickSize;
    property TickType;
    property TrackColor;
    property TrackSize;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnGetPositionHint;
    property OnDrawThumb;
    property OnGetTickLabel;
    property OnGetThumbRect;
    property OnValidate;
  end;

  { TcxTrackBarPositionHintHelper }

  TcxTrackBarPositionHintHelper = class(TcxControlHintHelper)
  private
    FPositionHintHidingTimer: TcxTimer;
    FTrackBarViewInfo: TcxCustomTrackBarViewInfo;
    procedure PositionHintHidingTimerHandler(Sender: TObject);
  protected
    function CanShowHint: Boolean; override;
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerControl: TcxControl; override;
    procedure MouseLeave; override;
  public
    constructor Create(AViewInfo: TcxCustomTrackBarViewInfo);
    destructor Destroy; override;
    procedure DisableTimer;
    procedure HidePositionHint;
    procedure SetDelay(AValue: Integer);
  end;

  { TcxTrackBarController }

  TcxTrackBarController = class
  private
    FIsMouseDownOnLeftButton: Boolean;
    FIsMouseDownOnRightButton: Boolean;
    FSlideState: TcxTrackBarSlideState;
    FStartSelectionPosition: Integer;
    FStoredPosition: Integer;
    FTrackBar: IcxTrackBar;
    FTimer: TcxTimer;

    procedure CreateTimer;
    procedure DestroyTimer;
    function GetActiveProperties: TcxCustomTrackBarProperties;
    function GetPosition: Integer;
    function GetProperties: TcxCustomTrackBarProperties;
    function GetViewInfo: TcxCustomTrackBarViewInfo;
    procedure HandleTimer(Sender: TObject);
    function IsInplace: Boolean;
    function PressButton(AIsInc: Boolean): Integer;
    procedure ProcessButtons(var ANewPosition: Integer; X, Y: Integer);
    procedure SetNewSelectionPosition(const ANewPosition: Integer);
    procedure SetPosition(AValue: Integer);
  protected
    function GetMouseDownPos: TPoint;
    function GetPositionAfterKeyDown(AThumb: TcxTrackBarThumbViewInfo;
      var Key: Word; Shift: TShiftState): Integer; virtual;
    function GetPositionAfterSliding(X, Y: Integer): Integer; virtual;
    procedure UpdatePos(AValue: Integer; AThumb: TcxTrackBarThumbViewInfo); virtual;
    property TrackBar: IcxTrackBar read FTrackBar;
  public
    constructor Create(ATrackBar: IcxTrackBar); virtual;
    destructor Destroy; override;

    procedure FocusChanged; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property ActiveProperties: TcxCustomTrackBarProperties read GetActiveProperties;
    property Position: Integer read GetPosition write SetPosition;
    property Properties: TcxCustomTrackBarProperties read GetProperties;
    property StoredPosition: Integer read FStoredPosition write FStoredPosition;
    property ViewInfo: TcxCustomTrackBarViewInfo read GetViewInfo;
  end;

  { TcxCustomTrackBar }

  TcxCustomTrackBar = class(TcxCustomEdit, IcxTrackBar)
  private
    FController: TcxTrackBarController;
    function GetStyle: TcxTrackBarStyle;
    procedure SetStyle(Value: TcxTrackBarStyle);
    function GetProperties: TcxCustomTrackBarProperties;
    function GetActiveProperties: TcxCustomTrackBarProperties;
    procedure GetOnGetThumbRect(out AValue: TcxGetThumbRectEvent);
    function GetViewInfo: TcxCustomTrackBarViewInfo;
    procedure SetProperties(Value: TcxCustomTrackBarProperties);
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    function CreateController: TcxTrackBarController; virtual;
    function DefaultParentColor: Boolean; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean); override;
    function GetDisplayText: string; override;
    function GetPosition: Integer; virtual;
    procedure FocusChanged; override;
    procedure Initialize; override;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); override;
    procedure SetInternalDisplayValue(Value: TcxEditValue); override;
    procedure SetPosition(Value: Integer); virtual;
    procedure SynchronizeDisplayValue; override;
    function WantNavigationKeys: Boolean; override;

    procedure InternalSetPosition(Value: Integer); virtual;

  // IcxTrackBar
    function IcxTrackBar.GetActiveProperties = GetActiveProperties;
    function GetMouseDownPos: TPoint;
    function IcxTrackBar.GetPosition = GetPosition;
    function IcxTrackBar.GetProperties = GetProperties;
    function IcxTrackBar.GetViewInfo = GetViewInfo;
    function IcxTrackBar.IsInplace = IsInplace;
    procedure IcxTrackBar.SetPosition = InternalSetPosition;

    property ViewInfo: TcxCustomTrackBarViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TcxCustomTrackBarProperties
      read GetActiveProperties;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Properties: TcxCustomTrackBarProperties read GetProperties
      write SetProperties;
    property Style: TcxTrackBarStyle read GetStyle write SetStyle;
    property Transparent;
  end;

  { TcxTrackBar }

  TcxTrackBar = class(TcxCustomTrackBar)
  private
    function GetActiveProperties: TcxTrackBarProperties;
    function GetProperties: TcxTrackBarProperties;
    procedure SetProperties(Value: TcxTrackBarProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxTrackBarProperties read GetActiveProperties;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property Properties: TcxTrackBarProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
  end;

const
  TrackBarTicksToTicksAlign: array[TcxTrackBarTickMarks] of TcxTrackBarTicksAlign = (
    tbtaBoth, tbtaUp, tbtaDown
  );
  TrackBarStateToButtonStates: array[1..5] of TcxButtonState = (
    cxbsNormal, cxbsHot, cxbsPressed, cxbsHot, cxbsDisabled
  );

implementation

uses
  cxGeometry, dxThemeConsts, dxThemeManager, dxUxTheme, dxCoreGraphics,
  cxEditConsts, cxEditPaintUtils, cxEditUtils, cxExtEditUtils, cxSpinEdit;

const
  TrackAndTickIndent = 1;
  TickAndTextIndent = 2;
  FromBorderIndent = 7;
  ButtonIndent = 5;
  PositionHintIndent = 10;
  MaxRealTrackZoneSize = 21;
  TrackBarTimerInitialInterval = 400;
  TrackBarTimerInterval = 60;

type
  { TcxFilterTrackBarHelper }

  TcxFilterTrackBarHelper = class(TcxFilterSpinEditHelper)
  public
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

procedure CalculateCustomTrackBarViewInfo(ACanvas: TcxCanvas; AViewData: TcxCustomTrackBarViewData;
  AViewInfo: TcxCustomTrackBarViewInfo);

  procedure CheckFocusRectBounds;
  begin
    with AViewInfo do
    begin
      if FocusRect.Left < TextRect.Left - 1 then
        FocusRect.Left := TextRect.Left - 1;
      if FocusRect.Top < TextRect.Top - 1 then
        FocusRect.Top := TextRect.Top - 1;
      if FocusRect.Right > TextRect.Right + 1 then
        FocusRect.Right := TextRect.Right + 1;
      if FocusRect.Bottom > TextRect.Bottom + 1 then
        FocusRect.Bottom := TextRect.Bottom + 1;
    end;
  end;

begin
  with AViewInfo do
  begin
    BackgroundColor := AViewData.Style.Color;
    if not IsInplace and Focused then
      if Length(Text) = 0 then
        FocusRect := cxEmptyRect
      else
      begin
        FocusRect := TextRect;
        InflateRect(FocusRect, 1, 1);
        CheckFocusRectBounds;
      end;
  end;
end;

{ TcxTrackBarStyle }

function TcxTrackBarStyle.DefaultBorderStyle: TcxContainerBorderStyle;
begin
  if IsBaseStyle then
    Result := cbsNone
  else
    Result := inherited DefaultBorderStyle;
end;

function TcxTrackBarStyle.DefaultHotTrack: Boolean;
begin
  Result := False;
end;

{ TcxHorizontalTrackBarViewInfoHelper }

constructor TcxHorizontalTrackBarViewInfoHelper.Create(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

function TcxHorizontalTrackBarViewInfoHelper.GetCurrentMousePos(X, Y: Integer): Integer;
begin
  Result := X;
end;

function TcxHorizontalTrackBarViewInfoHelper.GetRealRect(const R: TRect): TRect;
begin
  Result := R;
end;

function TcxHorizontalTrackBarViewInfoHelper.GetMouseDownPos(AMouseDownCoordinate: TPoint): Integer;
begin
  Result := AMouseDownCoordinate.X;
end;

procedure TcxHorizontalTrackBarViewInfoHelper.CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer);
begin
  AThumbSize := cxRectWidth(ACustomRect);
  AThumbLargeSize := cxRectHeight(ACustomRect);
end;

procedure TcxHorizontalTrackBarViewInfoHelper.DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  ACanvas.Line(
    ATickPosition, GetThumbRect.Bottom + TrackAndTickIndent + 1,
    ATickPosition, GetThumbRect.Bottom + TrackAndTickIndent + ATickSize + 1);
end;

procedure TcxHorizontalTrackBarViewInfoHelper.DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  ACanvas.Line(
    ATickPosition, GetThumbRect.Top - TrackAndTickIndent - ATickSize,
    ATickPosition, GetThumbRect.Top - TrackAndTickIndent);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetClientRect: TRect;
begin
  Result := GetRealRect(cxRectOffset(ViewInfo.ClientRect, -ViewInfo.Bounds.Left, -ViewInfo.Bounds.Top));
end;

function TcxHorizontalTrackBarViewInfoHelper.GetLeftButtonRect: TRect;
begin
  Result := GetRealRect(ViewInfo.LeftButtonRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetPositionHintTextRect(ACanvas: TcxCanvas;
  const AHintText: string; AIsHintMultiLine: Boolean): TRect;
var
  R: TRect;
begin
  if ViewInfo.HotThumb <> nil then
    R := ViewInfo.HotThumb.FullRect
  else
    if ViewInfo.PressedThumb <> nil then
      R := ViewInfo.PressedThumb.FullRect
    else
      R := ViewInfo.ThumbRect;
  Result.Left := R.Left;
  if ViewInfo.TickMarks = cxtmTopLeft then
    Result.Top := R.Bottom + PositionHintIndent
  else
    Result.Top := R.Top - PositionHintIndent - GetPositionHintSize(ACanvas, AHintText);

  if AIsHintMultiLine then
    Result.Right := Result.Left + cxRectWidth(R) div 2
  else
    Result.Right := Result.Left + ACanvas.TextWidth(AHintText);
  Result.Bottom := Result.Top + ACanvas.TextHeight(AHintText);
  Result := GetRealRect(Result);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetPositionHintSize(ACanvas: TcxCanvas; const AHintText: string): Integer;
begin
  Result := ACanvas.TextHeight(AHintText);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetRightButtonRect: TRect;
begin
  Result := GetRealRect(ViewInfo.RightButtonRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetSelectionRect: TRect;
begin
  Result := GetRealRect(ViewInfo.SelectionRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := cxRectBounds(ATickPosition - ATextWidth div 2, GetThumbRect.Top - ADelta - TrackAndTickIndent - ATextHeight, ATextWidth, ATextHeight);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := cxRectBounds(ATickPosition - ATextWidth div 2, ViewInfo.ThumbRect.Bottom + TrackAndTickIndent + ADelta, ATextWidth, ATextHeight);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetThumbRect: TRect;
begin
  Result := GetRealRect(ViewInfo.ThumbRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTrackBarRect: TRect;
begin
  Result := GetRealRect(ViewInfo.TrackBarRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTrackBounds: TRect;
begin
  Result := ViewInfo.ClientRect;
  if ViewInfo.ShowChangeButtons then
  begin
    Result.Left := GetTrackRect.Left;
    Result.Right := GetTrackRect.Right;
  end;
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTrackZoneRect: TRect;
begin
  Result := GetRealRect(ViewInfo.TrackZoneRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetTrackRect: TRect;
begin
  Result := GetRealRect(ViewInfo.TrackRect);
end;

function TcxHorizontalTrackBarViewInfoHelper.GetVisibleThumbRect: TRect;
begin
  Result := GetRealRect(ViewInfo.VisibleThumbRect);
end;

procedure TcxHorizontalTrackBarViewInfoHelper.InflateTrackRect(var ARect: TRect);
begin
  Inc(ARect.Right);
end;

{ TcxVerticalTrackBarViewInfoHelper }

function TcxVerticalTrackBarViewInfoHelper.GetCurrentMousePos(X, Y: Integer): Integer;
begin
  Result := Y;
end;

function TcxVerticalTrackBarViewInfoHelper.GetMouseDownPos(AMouseDownPos: TPoint): Integer;
begin
  Result := AMouseDownPos.Y;
end;

procedure TcxVerticalTrackBarViewInfoHelper.CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer);
begin
  AThumbSize := cxRectHeight(ACustomRect);
  AThumbLargeSize := cxRectWidth(ACustomRect);
end;

procedure TcxVerticalTrackBarViewInfoHelper.DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  ACanvas.Line(
    GetThumbRect.Right + TrackAndTickIndent + 1, ATickPosition,
    GetThumbRect.Right + TrackAndTickIndent + ATickSize + 1, ATickPosition);
end;

procedure TcxVerticalTrackBarViewInfoHelper.DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  ACanvas.Line(
    GetThumbRect.Left - (TrackAndTickIndent + ATickSize), ATickPosition,
    GetThumbRect.Left - TrackAndTickIndent, ATickPosition);
end;

function TcxVerticalTrackBarViewInfoHelper.GetPositionHintSize(ACanvas: TcxCanvas; const AHintText: string): Integer;
begin
  Result := ACanvas.TextWidth(AHintText);
end;

function TcxVerticalTrackBarViewInfoHelper.GetRealRect(const R: TRect): TRect;
begin
  Result := GetRotatedRect(R);
end;

function TcxVerticalTrackBarViewInfoHelper.GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := cxRectBounds(GetThumbRect.Left - TrackAndTickIndent - ATextWidth - ADelta, ATickPosition - ATextHeight div 2, ATextWidth, ATextHeight);
end;

function TcxVerticalTrackBarViewInfoHelper.GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := cxRectBounds(GetThumbRect.Right + ADelta + TrackAndTickIndent, ATickPosition - ATextHeight div 2, ATextWidth, ATextHeight);
end;

function TcxVerticalTrackBarViewInfoHelper.GetTrackBounds: TRect;
begin
  Result := ViewInfo.Edit.ClientBounds;
  if ViewInfo.ShowChangeButtons then
  begin
    Result.Top := GetTrackRect.Top;
    Result.Bottom := GetTrackRect.Bottom;
  end
end;

procedure TcxVerticalTrackBarViewInfoHelper.InflateTrackRect(var ARect: TRect);
begin
  Inc(ARect.Bottom);
end;

function TcxVerticalTrackBarViewInfoHelper.GetRotatedRect(ARect: Trect): TRect;
begin
  Result.Left := ARect.Top;
  Result.Top := ARect.Left;
  Result.Right := ARect.Bottom;
  Result.Bottom := ARect.Right;
end;

{ TcxCustomTrackBarViewInfo }

constructor TcxCustomTrackBarViewInfo.Create;
begin
  inherited Create;
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FButtonLeftFadingHelper := TcxTrackBarLeftButtonFadingHelper.Create(Self);
  FButtonRightFadingHelper := TcxTrackBarRightButtonFadingHelper.Create(Self);
  FButtonRightState := cxbsNormal;
  FButtonLeftState := cxbsNormal;
  FPositionHintHelper := TcxTrackBarPositionHintHelper.Create(Self);
  FTBBitmap := TcxBitmap.Create;
  MouseStates := [];
  FMajorTicks := TList.Create;
  FMinorTicks := TList.Create;
  FShowSelection := True;
  CreateThumbs;
end;

destructor TcxCustomTrackBarViewInfo.Destroy;
begin
  DestroyThumbs;
  FreeAndNil(FOrientationHelper);
  FreeAndNil(FButtonRightFadingHelper);
  FreeAndNil(FButtonLeftFadingHelper);
  FreeAndNil(FMinorTicks);
  FreeAndNil(FMajorTicks);
  FreeAndNil(FPositionHintHelper);
  FreeAndNil(FTBBitmap);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TcxCustomTrackBarViewInfo.Assign(Source: TObject);
begin
  inherited Assign(Source);
  if Source is TcxCustomTrackBarViewInfo then
    with TcxCustomTrackBarViewInfo(Source) do
    begin
      Self.LookAndFeel.Assign(LookAndFeel);
      Self.Position := Position;
      Self.TrackSize := TrackSize;
      Self.ThumbWidth := ThumbWidth;
      Self.ThumbHeight := ThumbHeight;
      Self.TrackBarBorderWidth := TrackBarBorderWidth;
      Self.TrackBarState := TrackBarState;
      Self.SelectionStart := SelectionStart;
      Self.SelectionEnd := SelectionEnd;
      Self.ShowSelection := ShowSelection;
    end;
end;

function TcxCustomTrackBarViewInfo.IsHotTrack: Boolean;
begin
  Result := True;
end;

function TcxCustomTrackBarViewInfo.IsHotTrack(P: TPoint): Boolean;
begin
  Result := True;
end;

function TcxCustomTrackBarViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
begin
  Result := False;
end;

procedure TcxCustomTrackBarViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  {Dummy}
end;

procedure TcxCustomTrackBarViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(RealTrackBarRect, DX, DY);
end;

procedure TcxCustomTrackBarViewInfo.Paint(ACanvas: TcxCanvas);

  procedure FillBackground;
  var
    AIsCustomBackground: Boolean;
  begin
    if FPropTransparent then
    begin
      if IsInplace or (csPaintCopy in Edit.ControlState) then
        cxBitBlt(FTBBitmap.cxCanvas.Handle, ACanvas.Handle, FTBBitmap.ClientRect, Bounds.TopLeft, SRCCOPY)
      else
        cxDrawTransparentControlBackground(Edit, FTBBitmap.cxCanvas, Bounds);
    end
    else
      if IsInplace then
      begin
        FTBBitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
        try
          AIsCustomBackground := DrawBackground(FTBBitmap.cxCanvas);
        finally
          FTBBitmap.cxCanvas.WindowOrg := cxNullPoint;
        end;
        if not AIsCustomBackground then
          FTBBitmap.cxCanvas.FillRect(FTBBitmap.ClientRect, BackgroundColor);
      end
      else
        if (NativeStyle or UseSkins) and not IsDesigning then
          cxDrawTransparentControlBackground(Edit, FTBBitmap.cxCanvas, Bounds)
        else
          DrawCustomEdit(FTBBitmap.cxCanvas, True, False);
  end;

begin
  FTBBitmap.SetSize(Bounds);

  FillBackground;

  FTBBitmap.cxCanvas.SaveClipRegion;
  try
    FTBBitmap.cxCanvas.TextFlags := ACanvas.TextFlags;
    FTBBitmap.cxCanvas.SetClipRegion(TcxRegion.Create(GetTrackBarRect), roSet);
    PaintTrackBar(FTBBitmap.cxCanvas);
  finally
    FTBBitmap.cxCanvas.RestoreClipRegion;
  end;
  cxBitBlt(ACanvas.Handle, FTBBitmap.cxCanvas.Handle, Bounds, cxNullPoint, SRCCOPY);
  DrawCustomEditValidationMark(ACanvas);
end;

procedure TcxCustomTrackBarViewInfo.PaintTrackBar(ACanvas: TcxCanvas);
begin
  if ShowChangeButtons then
    DrawButtons(ACanvas);
  if TrackBarProperties.ShowTrack then
  begin
    DrawTrack(ACanvas);
    if FShowSelection then
      DrawSelection(ACanvas);
  end;
  if TrackBarProperties.ShowTicks then
    DrawTicks(ACanvas);
  case TrackBarProperties.ThumbType of
    cxttRegular:
      DrawThumb(ACanvas);
    cxttCustom:
      DrawCustomThumb(ACanvas);
  end;
end;

procedure TcxCustomTrackBarViewInfo.AddMinorTicks(AMin, AMax, AFrequency: Integer);
var
  I: Integer;
begin
  if AFrequency > 0 then
  begin
    MinorTicks.Capacity := MinorTicks.Capacity + (AMax - AMin) div AFrequency + 1;

    I := AMin + AFrequency;
    repeat
      MinorTicks.Add(Pointer(I));
      I := I + AFrequency;
    until I >= AMax;
  end;
end;

procedure TcxCustomTrackBarViewInfo.PopulateMajorTicks;
begin
  MajorTicks.Clear;
  MajorTicks.Add(Pointer(Min));
  MajorTicks.Add(Pointer(Max));
end;

procedure TcxCustomTrackBarViewInfo.PopulateMinorTicks;
begin
  MinorTicks.Clear;
  AddMinorTicks(Min, Max, Frequency);
end;

procedure TcxCustomTrackBarViewInfo.DoGetPositionHint(var AHintText: string; var ACanShow: Boolean; var AIsHintMultiLine: Boolean);
begin
  if Assigned(TrackBarProperties.OnGetPositionHint) then
    TrackBarProperties.OnGetPositionHint(Edit, Position, AHintText, ACanShow, AIsHintMultiLine);
end;

procedure TcxCustomTrackBarViewInfo.DrawButtonLeft(ACanvas: TcxCanvas);
begin
  if not ButtonLeftFadingHelper.DrawImage(ACanvas.Handle, GetLeftButtonRect) then
    DrawButtonLeft(ACanvas, GetLeftButtonRect, ButtonLeftState);
end;

procedure TcxCustomTrackBarViewInfo.DrawButtonLeft(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  if ReverseDirection then
    LookAndFeel.Painter.DrawScaledZoomInButton(ACanvas, R, AState, ScaleFactor)
  else
    LookAndFeel.Painter.DrawScaledZoomOutButton(ACanvas, R, AState, ScaleFactor);
end;

procedure TcxCustomTrackBarViewInfo.DrawButtonRight(ACanvas: TcxCanvas);
begin
  if not ButtonRightFadingHelper.DrawImage(ACanvas.Handle, GetRightButtonRect) then
    DrawButtonRight(ACanvas, GetRightButtonRect, ButtonRightState);
end;

procedure TcxCustomTrackBarViewInfo.DrawButtonRight(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  if ReverseDirection then
    LookAndFeel.Painter.DrawScaledZoomOutButton(ACanvas, R, AState, ScaleFactor)
  else
    LookAndFeel.Painter.DrawScaledZoomInButton(ACanvas, R, AState, ScaleFactor);
end;

procedure TcxCustomTrackBarViewInfo.DrawButtons(ACanvas: TcxCanvas);
begin
  DrawButtonLeft(ACanvas);
  DrawButtonRight(ACanvas);
end;

procedure TcxCustomTrackBarViewInfo.DrawCustomThumb(ACanvas: TcxCanvas);
begin
  TrackBarProperties.DoDrawThumb(Self, ACanvas, GetThumbRect);
end;

procedure TcxCustomTrackBarViewInfo.DrawTrack(ACanvas: TcxCanvas);
var
  ARect: TRect;
begin
  ARect := GetTrackRect;
  if not UseSkins then
    OrientationHelper.InflateTrackRect(ARect);
  LookAndFeel.Painter.DrawTrackBarScaledTrack(ACanvas, ARect, GetSelectionRect, FShowSelection,
    Enabled, Orientation = tboHorizontal, TrackBarProperties.TrackColor, ScaleFactor);
end;

procedure TcxCustomTrackBarViewInfo.DrawSelection(ACanvas: TcxCanvas);
begin
  if not UseSkins then
    cxEditFillRect(ACanvas, GetSelectionRect, TrackBarProperties.SelectionColor);
end;

procedure TcxCustomTrackBarViewInfo.DrawTickAsLine(ACanvas: TcxCanvas; AIndex, ATickPosition: Integer);
var
  ATickSize: Integer;
begin
  ATickSize := TickSize;
  if (TickType = tbttTicks) and IsMajorTick(AIndex) then
    ATickSize := ATickSize + (TickSize div 2);
  if TickMarks in [cxtmBottomRight, cxtmBoth] then
    DrawTickAsLineBottomRight(ACanvas, ATickPosition, ATickSize);
  if TickMarks in [cxtmTopLeft, cxtmBoth] then
    DrawTickAsLineTopLeft(ACanvas, ATickPosition, ATickSize);
end;

procedure TcxCustomTrackBarViewInfo.DrawTickAsText(ACanvas: TcxCanvas; AIndex, ATickPosition: Integer);

  procedure InternalDrawTickText(const ARect: TRect; const ACaption: string);
  const
    ARotationAngle: array [TcxTrackBarTextOrientation] of TcxRotationAngle = (ra0, raPlus90);
  begin
    if ACanvas.RectFullyVisible(ARect) then
    begin
      cxDrawText(ACanvas, ACaption, ARect, 0, clDefault, ARotationAngle[TextOrientation]);
      ACanvas.ExcludeClipRect(ARect);
    end;
  end;

const
  AOrientation: array [TcxTrackBarTextOrientation] of TcxTrackBarOrientation = (tboHorizontal, tboVertical);
var
  ATextSize: TSize;
  ADelta: Integer;
  ATextWidth, ATextHeight: Integer;
  ACaption: string;
begin
  ACaption := IntToStr(AIndex);
  if Assigned(TrackBarProperties.OnGetTickLabel) then
    TrackBarProperties.OnGetTickLabel(Edit, AIndex, ACaption);
  ATextSize := ACanvas.TextExtent(ACaption);

  if TextOrientation = tbtoVertical then
  begin
    ATextWidth := ATextSize.cy;
    ATextHeight := ATextSize.cx;
  end
  else
  begin
    ATextWidth := ATextSize.cx;
    ATextHeight := ATextSize.cy;
  end;

  if AOrientation[TextOrientation] <> Orientation then
    ADelta := 1
  else
    ADelta := 0;

  if TickType = tbttTicksAndNumbers then
    ADelta := ADelta + TickAndTextIndent + TickSize;

  if TickMarks in [cxtmTopLeft, cxtmBoth] then
    InternalDrawTickText(GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta), ACaption);
  if TickMarks in [cxtmBottomRight, cxtmBoth] then
    InternalDrawTickText(GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta), ACaption);
end;

procedure TcxCustomTrackBarViewInfo.DrawTicks(ACanvas: TcxCanvas);

  procedure DrawTick(ACanvas: TcxCanvas; AIndex, ATickPosition: Integer; ADrawTextTicks, ADrawLineTicks: Boolean);

    function NeedDrawTickAsLine(ATickValue: Integer): Boolean;
    begin
      Result := (TickType = tbttTicks) or (TickType = tbttTicksAndNumbers) or
        ((TickType = tbttValueNumber) and not IsTickText(ATickValue));
    end;

    function NeedDrawTickAsText(ATickValue: Integer): Boolean;
    begin
      Result := (TickType = tbttNumbers) or (TickType = tbttTicksAndNumbers) or
        ((TickType = tbttValueNumber) and IsTickText(ATickValue));
    end;

  begin
    if NeedDrawTickAsLine(AIndex) and ADrawLineTicks then
      DrawTickAsLine(ACanvas, AIndex, ATickPosition);
    if NeedDrawTickAsText(AIndex) and ADrawTextTicks then
      DrawTickAsText(ACanvas, AIndex, ATickPosition);
  end;

  procedure DrawMajorTicks;
  var
    I: Integer;
  begin
    for I := 0 to MajorTicks.Count - 1 do
      DrawTick(ACanvas, Integer(MajorTicks[I]), GetCoordinate(Integer(MajorTicks[I])), True, True);
  end;

  procedure DrawMinorTicks(ADrawTextTicks, ADrawLineTicks: Boolean);
  var
    I, AIndex, ATickPosition, APrevTickPosition: Integer;
  begin
    APrevTickPosition := GetCoordinate(Min);
    for I := 0 to MinorTicks.Count - 1 do
    begin
      AIndex := Integer(MinorTicks[I]);
      ATickPosition := GetCoordinate(AIndex);
      if (ATickPosition > APrevTickPosition + 1) or (ATickPosition < APrevTickPosition - 1) then
      begin
        DrawTick(ACanvas, AIndex, ATickPosition, ADrawTextTicks, ADrawLineTicks);
        APrevTickPosition := ATickPosition;
      end;
    end;
  end;

begin
  ACanvas.Font.Assign(Font);
  ACanvas.Brush.Color := clBtnFace;
  if UseSkins then
  begin
    ACanvas.Font.Color := Painter.TrackBarTicksColor(True);
    ACanvas.Pen.Color := Painter.TrackBarTicksColor(False);
  end
  else
  begin
    ACanvas.Font.Color := TextColor;
    ACanvas.Pen.Color := TickColor;
  end;

  DrawMajorTicks;
  DrawMinorTicks(True, False);
  DrawMinorTicks(False, True);
end;

procedure TcxCustomTrackBarViewInfo.DrawThumb(ACanvas: TcxCanvas);
begin
  if not ThumbFadingHelper.DrawImage(ACanvas.Handle, GetThumbRect) then
    DrawThumb(ACanvas, GetThumbRect, TrackBarState);
end;

procedure TcxCustomTrackBarViewInfo.DrawThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer);
begin
  LookAndFeel.Painter.DrawTrackBarScaledThumb(ACanvas, R,
    TrackBarStateToButtonStates[AState], Orientation = tboHorizontal,
    TrackBarTicksToTicksAlign[TickMarks], GetThumbRealColor(AState), ScaleFactor);
end;

function TcxCustomTrackBarViewInfo.DrawingThumbRectToRealThumbRect(ACanvas: TcxCanvas; const AThumbRect: TRect): TRect;
begin
  Result := AThumbRect;
  LookAndFeel.Painter.CorrectThumbRect(ACanvas, Result,
    Orientation = tboHorizontal, TrackBarTicksToTicksAlign[TickMarks]);
end;

function TcxCustomTrackBarViewInfo.DrawingThumbRectToRealThumbRect(ACanvas: TcxCanvas): TRect;
begin
  Result := DrawingThumbRectToRealThumbRect(ACanvas, ThumbRect);
end;

function TcxCustomTrackBarViewInfo.GetOffset(APosition: Integer): Integer;
begin
  Result := MulDiv(APosition - Min, PixelsPerPositionStep.Numerator, PixelsPerPositionStep.Denominator) + ThumbSize div 2;
end;

function TcxCustomTrackBarViewInfo.GetThumbRealColor(AState: Integer): TColor;
begin
  case AState of
    TUS_DISABLED:
      Result := clBtnShadow;
    TUS_PRESSED, TUS_HOT:
      if LookAndFeel.Kind in [lfUltraFlat, lfOffice11] then
        Result := GetEditButtonHighlightColor(AState = TUS_PRESSED, LookAndFeel.Kind = lfOffice11)
      else
        Result := TrackBarProperties.ThumbHighLightColor;
    else
      Result := TrackBarProperties.ThumbColor;
  end;
end;

function TcxCustomTrackBarViewInfo.GetCoordinate(APosition: Integer): Integer;
begin
  if ReverseDirection then
    Result := TrackRect.Right - GetOffset(APosition)
  else
    Result := TrackRect.Left + GetOffset(APosition);
end;

function TcxCustomTrackBarViewInfo.IsTickText(ATickValue: Integer): Boolean;
begin
  Result := (ATickValue = Min) or (ATickValue = Max) or (ATickValue = Position);
end;

procedure TcxCustomTrackBarViewInfo.UpdateTrackBarState;
begin
  if not Enabled then
  begin
    TrackBarState := TUS_DISABLED;
    SetChangeButtonsState(TUS_DISABLED);
  end
  else
  begin
    if tbmpSliding in MouseStates then
      TrackBarState := TUS_PRESSED
    else
      if tbmpUnderThumb in MouseStates then
        TrackBarState := TUS_HOT
      else
        TrackBarState := TUS_NORMAL;
    SetChangeButtonsState(TUS_NORMAL);
  end;
end;

procedure TcxCustomTrackBarViewInfo.UpdateValue(AValue: TcxEditValue);
begin
  Position := AValue;
end;

function TcxCustomTrackBarViewInfo.IsPtToTheLeftFromThumb(
  AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean;
begin
  Result := GetCurrentMousePos(X, Y) < VisibleThumbRect.Left;
end;

function TcxCustomTrackBarViewInfo.IsPtToTheRightFromThumb(
  AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean;
begin
  Result := GetCurrentMousePos(X, Y) > VisibleThumbRect.Right;
end;

function TcxCustomTrackBarViewInfo.GetCalculatedPosition(APosition: Integer; AIsInc: Boolean;
  AIsLineSize: Boolean; ANeedRound: Boolean = False): Integer;
var
  AStep: TcxNaturalNumber;
begin
  if ReverseDirection then
    AIsInc := not AIsInc;
  if AIsLineSize then
    AStep := TrackBarProperties.LineSize
  else
    AStep := TrackBarProperties.PageSize;
  if AIsInc then
    Result := APosition + AStep
  else
    Result := APosition - AStep;
  if ANeedRound then
    if AIsInc then
      Result := Floor((Result - Min) / AStep) * AStep + Min
    else
      Result := Ceil((Result - Min) / AStep) * AStep + Min;
end;

function TcxCustomTrackBarViewInfo.GetDefaultPositionHintText: string;
begin
  Result := IntToStr(Position);
end;

function TcxCustomTrackBarViewInfo.GetMouseDelta(X, Y: Integer; AMouseDownPos: TPoint): Integer;
begin
  Result := GetCurrentMousePos(X, Y) - GetMouseDownPos(AMouseDownPos);
  if ReverseDirection then
    Result := - Result;
end;

function TcxCustomTrackBarViewInfo.GetNewPosition(X, Y: Integer): Integer;
var
  AThumb: TcxTrackBarThumbViewInfo;
begin
  AThumb := GetNearestThumb(X, Y);
  if TrackBarProperties.ThumbStep = cxtsNormal then
  begin
    Result := AThumb.Position;
    if IsPtToTheRightFromThumb(AThumb, X, Y) then
      Result := GetCalculatedPosition(Result, True, False)
    else
      if IsPtToTheLeftFromThumb(AThumb, X, Y) then
        Result := GetCalculatedPosition(Result, False, False)
  end
  else
    Result := GetPositionAfterJump(X, Y);
  Result := TrackBarProperties.FixPosition(Result);
end;

function TcxCustomTrackBarViewInfo.GetPositionAfterJump(X, Y: Integer): Integer;

  function GetMouseOffset: Integer;
  begin
    Result := GetCurrentMousePos(X, Y) - TrackRect.Left - cxRectWidth(VisibleThumbRect) div 2
  end;

var
  ADeltaPosition: Integer;
begin
  ADeltaPosition := MulDiv(GetMouseOffset, PixelsPerPositionStep.Denominator, PixelsPerPositionStep.Numerator);
  if ReverseDirection then
    Result := Max - ADeltaPosition
  else
    Result := ADeltaPosition + Min;
end;

function TcxCustomTrackBarViewInfo.GetPositionHintText(var ACanShow: Boolean; var AIsHintMultiLine: Boolean): string;
begin
  Result := GetDefaultPositionHintText;
  ACanShow := True;
  AIsHintMultiLine := False;
  DoGetPositionHint(Result, ACanShow, AIsHintMultiLine);
end;

procedure TcxCustomTrackBarViewInfo.HidePositionHint;
begin
  PositionHintHelper.HidePositionHint;
end;

procedure TcxCustomTrackBarViewInfo.MouseMove(X, Y: Integer);
begin
  if FHotThumb <> nil then
  begin
    ProcessPositionHint;
    Include(MouseStates, tbmpUnderThumb);
  end
  else
  begin
    if TcxCustomTrackBarProperties(EditProperties).ShowPositionHint then
      HidePositionHint;
    Exclude(MouseStates, tbmpUnderThumb);
  end;
end;

procedure TcxCustomTrackBarViewInfo.ProcessPositionHint;
var
  AHintText: string;
  ACanShow, AIsMultiLine: Boolean;
begin
  if TcxCustomTrackBarProperties(EditProperties).ShowPositionHint then
  begin
    PositionHintHelper.DisableTimer;
    AHintText := GetPositionHintText(ACanShow, AIsMultiLine);
    if ACanShow then
      PositionHintHelper.ShowHint(TrackBarRect,
        GetPositionHintTextRect(Edit.Canvas, AHintText, AIsMultiLine), AHintText, AIsMultiline, Self, Font)
    else
      PositionHintHelper.CancelHint;
  end
  else
    PositionHintHelper.CancelHint;
end;

procedure TcxCustomTrackBarViewInfo.CreateOrientationHelper;
begin
  if Orientation = tboHorizontal then
  begin
    if (OrientationHelper = nil) or (OrientationHelper is TcxVerticalTrackBarViewInfoHelper) then
    begin
      FreeAndNil(FOrientationHelper);
      FOrientationHelper := TcxHorizontalTrackBarViewInfoHelper.Create(Self);
    end;
  end
  else
  begin
    if (OrientationHelper = nil) or not (OrientationHelper is TcxVerticalTrackBarViewInfoHelper) then
    begin
      FreeAndNil(FOrientationHelper);
      FOrientationHelper := TcxVerticalTrackBarViewInfoHelper.Create(Self);
    end;
  end;
end;

function TcxCustomTrackBarViewInfo.GetCurrentMousePos(X, Y: Integer): Integer;
begin
  Result := OrientationHelper.GetCurrentMousePos(X, Y);
end;

function TcxCustomTrackBarViewInfo.GetMouseDownPos(AMouseDownPos: TPoint): Integer;
begin
  Result := OrientationHelper.GetMouseDownPos(AMouseDownPos);
end;

procedure TcxCustomTrackBarViewInfo.CalculateCustomThumbSize(ACustomRect: TRect; out AThumbSize, AThumbLargeSize: Integer);
begin
  OrientationHelper.CalculateCustomThumbSize(ACustomRect, AThumbSize, AThumbLargeSize);
end;

procedure TcxCustomTrackBarViewInfo.DrawTickAsLineBottomRight(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  OrientationHelper.DrawTickAsLineBottomRight(ACanvas, ATickPosition, ATickSize);
end;

procedure TcxCustomTrackBarViewInfo.DrawTickAsLineTopLeft(ACanvas: TcxCanvas; ATickPosition, ATickSize: Integer);
begin
  OrientationHelper.DrawTickAsLineTopLeft(ACanvas, ATickPosition, ATickSize);
end;

function TcxCustomTrackBarViewInfo.GetClientRect: TRect;
begin
  Result := OrientationHelper.GetClientRect;
end;

function TcxCustomTrackBarViewInfo.GetLeftButtonRect: TRect;
begin
  Result := OrientationHelper.GetLeftButtonRect;
end;

function TcxCustomTrackBarViewInfo.GetPositionHintTextRect(ACanvas: TcxCanvas; const AHintText: string;
  AIsHintMultiLine: Boolean): TRect;
begin
  Result := OrientationHelper.GetPositionHintTextRect(ACanvas, AHintText, AIsHintMultiLine);
end;

function TcxCustomTrackBarViewInfo.GetRightButtonRect: TRect;
begin
  Result := OrientationHelper.GetRightButtonRect;
end;

function TcxCustomTrackBarViewInfo.GetSelectionRect: TRect;
begin
  Result := OrientationHelper.GetSelectionRect;
end;

function TcxCustomTrackBarViewInfo.GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := OrientationHelper.GetTextTopRect(ATickPosition, ATextWidth, ATextHeight, ADelta);
end;

function TcxCustomTrackBarViewInfo.GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta: Integer): TRect;
begin
  Result := OrientationHelper.GetTextBottomRect(ATickPosition, ATextWidth, ATextHeight, ADelta);
end;

function TcxCustomTrackBarViewInfo.GetRealRect(const R: TRect): TRect;
begin
  Result := OrientationHelper.GetRealRect(R);
end;

function TcxCustomTrackBarViewInfo.GetThumbRect: TRect;
begin
  Result := OrientationHelper.GetThumbRect;
end;

function TcxCustomTrackBarViewInfo.GetTrackBarRect: TRect;
begin
  Result := OrientationHelper.GetTrackBarRect;
end;

function TcxCustomTrackBarViewInfo.GetTrackBounds: TRect;
begin
  Result := OrientationHelper.GetTrackBounds;
end;

function TcxCustomTrackBarViewInfo.GetTrackRect: TRect;
begin
  Result := OrientationHelper.GetTrackRect;
end;

function TcxCustomTrackBarViewInfo.GetVisibleThumbRect: TRect;
begin
  Result := OrientationHelper.GetVisibleThumbRect;
end;

procedure TcxCustomTrackBarViewInfo.AddThumb(AThumb: TcxTrackBarThumbViewInfo);
begin
  FThumbs.Add(AThumb);
end;

procedure TcxCustomTrackBarViewInfo.AddThumbs;
begin
  FThumbs.Add(CreateThumbViewInfo);
end;

procedure TcxCustomTrackBarViewInfo.CreateThumbs;
begin
  FThumbs := TObjectList<TcxTrackBarThumbViewInfo>.Create;
  AddThumbs;
end;

function TcxCustomTrackBarViewInfo.CreateThumbViewInfo: TcxTrackBarThumbViewInfo;
begin
  Result := TcxTrackBarThumbViewInfo.Create(Self);
end;

procedure TcxCustomTrackBarViewInfo.DestroyThumbs;
begin
  FreeAndNil(FThumbs);
end;

function TcxCustomTrackBarViewInfo.GetNearestThumb(X,
  Y: Integer): TcxTrackBarThumbViewInfo;
var
  AThumb: TcxTrackBarThumbViewInfo;
  AMinDistance, ADistance: Integer;
  AThumbCenter: TPoint;
begin
  Result := nil;
  AMinDistance := MaxInt;
  for AThumb in FThumbs do
  begin
    AThumbCenter := cxRectCenter(GetRealRect(AThumb.Bounds));
    ADistance := Sqr(X - AThumbCenter.X) + Sqr(Y - AThumbCenter.Y);
    if ADistance < AMinDistance then
    begin
      Result := AThumb;
      AMinDistance := ADistance;
    end;
  end;
end;

function TcxCustomTrackBarViewInfo.GetThumbFromPos(
  const APoint: TPoint): TcxTrackBarThumbViewInfo;
var
  AThumb: TcxTrackBarThumbViewInfo;
begin
  Result := nil;
  for AThumb in FThumbs do
    if PtInRect(GetRealRect(AThumb.Bounds), APoint) then
    begin
      Result := AThumb;
      Break;
    end;
end;

function TcxCustomTrackBarViewInfo.GetTrackBarProperties: TcxCustomTrackBarProperties;
begin
  Result := EditProperties as TcxCustomTrackBarProperties;
end;

function TcxCustomTrackBarViewInfo.GetTrackBarState: Integer;
begin
  Result := FThumbs[0].State;
end;

function TcxCustomTrackBarViewInfo.IsMajorTick(AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to MajorTicks.Count - 1 do
  begin
    Result := Integer(MajorTicks[I]) = AIndex;
    if Result then Break;
  end;
end;

function TcxCustomTrackBarViewInfo.GetRightButtonSize: TSize;
begin
  Result := Painter.GetScaledZoomInButtonSize(ScaleFactor);
end;

function TcxCustomTrackBarViewInfo.GetThumbFadingHelper: TcxTrackBarThumbFadingHelper;
begin
  Result := FThumbs[0].FFadingHelper;
end;

function TcxCustomTrackBarViewInfo.GetThumb(Index: Integer): TcxTrackBarThumbViewInfo;
begin
  Result := FThumbs[Index];
end;

function TcxCustomTrackBarViewInfo.GetLeftButtonSize: TSize;
begin
  Result := Painter.GetScaledZoomOutButtonSize(ScaleFactor);
end;

function TcxCustomTrackBarViewInfo.GetPosition: Integer;
begin
  Result := FThumbs[0].Position;
end;

procedure TcxCustomTrackBarViewInfo.SetButtonLeftState(Value: TcxButtonState);
begin
  if FButtonLeftState <> Value then
  begin
    ButtonLeftFadingHelper.CheckStartFading(ButtonLeftState, Value);
    FButtonLeftState := Value;
  end;
end;

procedure TcxCustomTrackBarViewInfo.SetButtonRightState(Value: TcxButtonState);
begin
  if FButtonRightState <> Value then
  begin
    ButtonRightFadingHelper.CheckStartFading(ButtonRightState, Value);
    FButtonRightState := Value;
  end;
end;

procedure TcxCustomTrackBarViewInfo.SetChangeButtonsState(Value: Integer);
var
  AState: TcxButtonState;
begin
  AState := TrackBarStateToButtonStates[Value];
  if (AState = cxbsDisabled) or (FButtonLeftState = cxbsDisabled) and
    (FButtonRightState = cxbsDisabled) and (AState = cxbsNormal) then
  begin
    FButtonLeftState := AState;
    FButtonRightState := AState;
  end;
end;

procedure TcxCustomTrackBarViewInfo.SetPosition(const Value: Integer);
begin
  FThumbs[0].Position := Value;
end;

procedure TcxCustomTrackBarViewInfo.SetTrackBarState(AValue: Integer);
begin
  FThumbs[0].State := AValue;
end;

{ TcxCustomTrackBarViewData }

procedure TcxCustomTrackBarViewData.CalculateTrackBarViewInfoProperties(AViewInfo: TcxCustomEditViewInfo);
var
  ATrackBarViewInfo: TcxCustomTrackBarViewInfo;
begin
  ATrackBarViewInfo := AViewInfo as TcxCustomTrackBarViewInfo;
  with ATrackBarViewInfo do
  begin
    Frequency := Properties.Frequency;
    Min := Properties.Min;
    Max := Properties.Max;
    ReverseDirection := IsReverseDirection;
    ShowChangeButtons := Properties.ShowChangeButtons;
    TrackBarBorderWidth := Properties.BorderWidth;
    TrackSize := ScaleFactor.Apply(Properties.TrackSize, Properties.ScaleFactor);
    if Enabled then
      TickColor := Properties.TickColor
    else
      TickColor := clBtnShadow;

    TickType := Properties.TickType;
    TickMarks := Properties.TickMarks;
    TickSize := ScaleFactor.Apply(Properties.TickSize, Properties.ScaleFactor);
    ThumbHeight := ScaleFactor.Apply(Properties.ThumbHeight, Properties.ScaleFactor);
    ThumbWidth := ScaleFactor.Apply(Properties.ThumbWidth, Properties.ScaleFactor);

    Orientation := Properties.Orientation;
    TextOrientation := Properties.TextOrientation;

    if IsInplace then
      FPropTransparent := Transparent
    else
      FPropTransparent := TcxCustomTrackBar(Edit).Transparent;

    CreateOrientationHelper;
    PopulateMajorTicks;
    PopulateMinorTicks;
  end;
  UpdateSelection(ATrackBarViewInfo);
end;

function TcxCustomTrackBarViewData.ThumbHalfSize(AViewInfo: TcxCustomTrackBarViewInfo): Integer;
begin
  Result := AViewInfo.ThumbSize div 2;
end;

procedure TcxCustomTrackBarViewData.CalculateButtonLeftRect(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  AViewInfo.LeftButtonRect.Left := AViewInfo.TrackZoneRect.Left;
  AViewInfo.LeftButtonRect.Right := AViewInfo.TrackZoneRect.Left + AViewInfo.GetLeftButtonSize.cx;
  AViewInfo.LeftButtonRect.Top := AViewInfo.TrackZoneRect.Top +
    (cxRectHeight(AViewInfo.TrackZoneRect) - AViewInfo.GetLeftButtonSize.cy) div 2;
  AViewInfo.LeftButtonRect.Bottom := AViewInfo.LeftButtonRect.Top + AViewInfo.GetLeftButtonSize.cy;
end;

procedure TcxCustomTrackBarViewData.CalculateButtonRightRect(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  AViewInfo.RightButtonRect.Left := AViewInfo.TrackZoneRect.Right - AViewInfo.GetRightButtonSize.cx;
  if not AViewInfo.UseSkins then
    Inc(AViewInfo.RightButtonRect.Left);
  AViewInfo.RightButtonRect.Right := AViewInfo.RightButtonRect.Left + AViewInfo.GetRightButtonSize.cx;
  AViewInfo.RightButtonRect.Top := AViewInfo.TrackZoneRect.Top +
    (cxRectHeight(AViewInfo.TrackZoneRect) - AViewInfo.GetRightButtonSize.cy) div 2;
  AViewInfo.RightButtonRect.Bottom := AViewInfo.RightButtonRect.Top + AViewInfo.GetRightButtonSize.cy;
end;

procedure TcxCustomTrackBarViewData.CalculatePixelsPerPositionStep(AViewInfo: TcxCustomTrackBarViewInfo);

  function GetMinMaxDiff: Integer;
  begin
    Result := Max(Properties.Max - Properties.Min, 1);
  end;

begin
  AViewInfo.PixelsPerPositionStep.Numerator := cxRectWidth(AViewInfo.TrackRect) - AViewInfo.ThumbSize;
  AViewInfo.PixelsPerPositionStep.Denominator := GetMinMaxDiff;
end;

function TcxCustomTrackBarViewData.CalculateTickSize(
  ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo;
  ALeftTop: Boolean): Integer;

  procedure CorrectTickSize(ATextHeight: Integer);
  var
    ATrackBarHeight: Integer;
    AMultiplier: Integer;
  begin
    AMultiplier := 1;
    if Properties.TickMarks = cxtmBoth then
      AMultiplier := 2;
    ATrackBarHeight := cxRectHeight(AViewInfo.TrackBarRect);
    if ATrackBarHeight - AMultiplier * Result < MaxRealTrackZoneSize then
    begin
      AViewInfo.TickSize := Max((ATrackBarHeight - MaxRealTrackZoneSize -
        AMultiplier * (ATextHeight + TrackAndTickIndent + TickAndTextIndent)) div AMultiplier, 0);
      Result := AViewInfo.TickSize + TrackAndTickIndent + TickAndTextIndent + ATextHeight;
    end;
  end;

const
  Orientation: array [TcxTrackBarTextOrientation] of TcxTrackBarOrientation = (tboHorizontal, tboVertical);
var
  ACalcNumValue: string;
  AIsCorrectSide: Boolean;
  ATextHeight: Integer;
begin
  Result := 0;
  ATextHeight := 0;
  AIsCorrectSide :=
    ((Properties.TickMarks <> cxtmBottomRight) and ALeftTop) or
    (Properties.TickMarks <> cxtmTopLeft) and not ALeftTop;

  if AIsCorrectSide then
  begin
    Result := AViewInfo.TickSize + TrackAndTickIndent;
    if Properties.TickType in [tbttNumbers, tbttValueNumber, tbttTicksAndNumbers] then
    begin
      ACalcNumValue := IntToStr(Properties.Min);
      if Orientation[AViewInfo.TextOrientation] = AViewInfo.Orientation then
        ATextHeight := ACanvas.TextHeight(ACalcNumValue)
      else
      begin
        if Length(ACalcNumValue) < Length(IntToStr(Properties.Max)) then
          ACalcNumValue := IntToStr(Properties.Max);
        ATextHeight := ACanvas.TextWidth(ACalcNumValue);
      end;
      if Properties.TickType = tbttTicksAndNumbers then
        Result := Result + TickAndTextIndent + ATextHeight
      else
        Result := ATextHeight;
    end;
  end;
  if AViewInfo.TrackBarBorderWidth = 0 then
    Inc(Result, 1);

  if AIsCorrectSide and (Properties.TickType = tbttTicksAndNumbers) then
    CorrectTickSize(ATextHeight);
end;

procedure TcxCustomTrackBarViewData.CalculateSelectionRect(AViewInfo: TcxCustomTrackBarViewInfo);

  procedure CalculateSelectionBounds(out ASelectionStartCoordinate, ASelectionEndCoordinate: Integer);

    procedure AdjustSelectionStartCoordinate;
    begin
      ASelectionStartCoordinate := AViewInfo.TrackRect.Left;
    end;

    procedure AdjustSelectionEndCoordinate;
    begin
      ASelectionEndCoordinate := AViewInfo.TrackRect.Right;
    end;

    procedure PreparationForFillRect;
    begin
      Inc(ASelectionEndCoordinate);
    end;

  begin
    ASelectionStartCoordinate := AViewInfo.GetCoordinate(AViewInfo.SelectionStart);
    ASelectionEndCoordinate := AViewInfo.GetCoordinate(AViewInfo.SelectionEnd);
    if AViewInfo.ReverseDirection then
      SwapIntegers(ASelectionStartCoordinate, ASelectionEndCoordinate);
    PreparationForFillRect;
    if AViewInfo.UseSkins then
    begin
      if AViewInfo.SelectionStart = Properties.Min then
        if AViewInfo.ReverseDirection then
          AdjustSelectionEndCoordinate
        else
          AdjustSelectionStartCoordinate;
      if AViewInfo.SelectionEnd = Properties.Max then
        if AViewInfo.ReverseDirection then
          AdjustSelectionStartCoordinate
        else
          AdjustSelectionEndCoordinate;
    end;
  end;

begin
  if AViewInfo.FShowSelection then
  begin
    CalculateSelectionBounds(AViewInfo.SelectionRect.Left, AViewInfo.SelectionRect.Right);
    AViewInfo.SelectionRect.Bottom := AViewInfo.TrackRect.Bottom;
    AViewInfo.SelectionRect.Top := AViewInfo.TrackRect.Top;
    if not AViewInfo.UseSkins then
    begin
      Inc(AViewInfo.SelectionRect.Top, 2);
      Dec(AViewInfo.SelectionRect.Bottom);
    end;
  end;
end;

procedure TcxCustomTrackBarViewData.CalculateThumbSize(AViewInfo: TcxCustomTrackBarViewInfo);
var
  AThumbSize, AThumbLargeSize: Integer;
  ACustomRect: TRect;
  APainterThumbSize: TSize;
begin
  if (Properties.ThumbType = cxttCustom) and IsOnGetThumbRectEventAssigned then
  begin
    DoOnGetThumbRect(ACustomRect);
    AViewInfo.CalculateCustomThumbSize(ACustomRect, AThumbSize, AThumbLargeSize);
    AViewInfo.ThumbWidth := AThumbSize;
    AViewInfo.ThumbHeight := AThumbLargeSize;
  end
  else
    if AViewInfo.UseSkins then
    begin
      APainterThumbSize := AViewInfo.Painter.TrackBarScaledThumbSize(True, ScaleFactor);
      AThumbSize := APainterThumbSize.cx;
      AThumbLargeSize := Min(APainterThumbSize.cy, cxRectHeight(AViewInfo.TrackZoneRect));
    end
    else
      if Properties.AutoSize then
      begin
        AThumbSize := AViewInfo.TrackSize;
        AThumbLargeSize := AThumbSize * 2;
        Inc(AThumbSize, AThumbSize mod 2);
      end
      else
      begin
        AThumbSize := AViewInfo.ThumbWidth;
        AThumbLargeSize := AViewInfo.ThumbHeight;
      end;

  AViewInfo.ThumbSize := AThumbSize;
  AViewInfo.ThumbLargeSize := AThumbLargeSize;
end;

procedure TcxCustomTrackBarViewData.CalculateThumbRect(
  ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo);
begin
  AViewInfo.ThumbRect.Left := AViewInfo.GetCoordinate(AViewInfo.Position) - ThumbHalfSize(AViewInfo);
  AViewInfo.ThumbRect.Right := AViewInfo.ThumbRect.Left + AViewInfo.ThumbSize;
  AViewInfo.ThumbRect.Top := AViewInfo.TrackRect.Top +
    (AViewInfo.TrackSize - AViewInfo.ThumbLargeSize) div 2;
  AViewInfo.ThumbRect.Bottom := AViewInfo.ThumbRect.Top + AViewInfo.ThumbLargeSize;
  AViewInfo.VisibleThumbRect := AViewInfo.DrawingThumbRectToRealThumbRect(ACanvas);

  AViewInfo.Thumb[0].Bounds := AViewInfo.VisibleThumbRect;
  AViewInfo.Thumb[0].FullRect := AViewInfo.ThumbRect;
end;

procedure TcxCustomTrackBarViewData.CalculateTrackBarRect(
  AViewInfo: TcxCustomTrackBarViewInfo);
begin
  AViewInfo.RealTrackBarRect := AViewInfo.GetClientRect;
  AViewInfo.TrackBarRect := AViewInfo.GetClientRect;
  AViewInfo.TrackBarBorderWidth := Min(AViewInfo.TrackBarBorderWidth, cxRectWidth(AViewInfo.TrackBarRect) div 2);
  AViewInfo.TrackBarBorderWidth := Min(AViewInfo.TrackBarBorderWidth, cxRectHeight(AViewInfo.TrackBarRect) div 2);
end;

procedure TcxCustomTrackBarViewData.CalculateTrackRect(AViewInfo: TcxCustomTrackBarViewInfo);

  procedure AdjustTrackRect;
  begin
    AViewInfo.TrackRect.Left := AViewInfo.TrackRect.Left + cxRectWidth(AViewInfo.LeftButtonRect) + ScaleFactor.Apply(ButtonIndent);
    AViewInfo.TrackRect.Right := AViewInfo.TrackRect.Right - cxRectWidth(AViewInfo.RightButtonRect) - ScaleFactor.Apply(ButtonIndent);
  end;

begin
  AViewInfo.TrackRect.Left := AViewInfo.TrackZoneRect.Left;
  AViewInfo.TrackRect.Right := AViewInfo.TrackZoneRect.Right;
  AViewInfo.TrackRect.Top := AViewInfo.TrackZoneRect.Top + (cxRectHeight(AViewInfo.TrackZoneRect) - AViewInfo.TrackSize) div 2;
  AViewInfo.TrackRect.Bottom := AViewInfo.TrackRect.Top + AViewInfo.TrackSize;
  if Properties.ShowChangeButtons then
    AdjustTrackRect;
end;

procedure TcxCustomTrackBarViewData.CalculateTrackSize(AViewInfo: TcxCustomTrackBarViewInfo; ARealTrackZoneSize: Integer);
begin
  if AViewInfo.UseSkins then
    AViewInfo.TrackSize := AViewInfo.Painter.TrackBarScaledTrackSize(ScaleFactor)
  else
    if Properties.AutoSize then
      AViewInfo.TrackSize := ARealTrackZoneSize div 2;
end;

procedure TcxCustomTrackBarViewData.CalculateTrackZoneRect(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomTrackBarViewInfo; out ARealTrackZoneSize: Integer);
var
  ATopLeftIndent, ABottomRightIndent: Integer;
  ATrackZoneSize: Integer;
begin
  ATopLeftIndent := CalculateTickSize(ACanvas, AViewInfo, True);
  ABottomRightIndent := CalculateTickSize(ACanvas, AViewInfo, False);
  ATrackZoneSize := cxRectHeight(AViewInfo.TrackBarRect) - ATopLeftIndent - ABottomRightIndent;
  ARealTrackZoneSize := Min(Max(ATrackZoneSize, 10), ScaleFactor.Apply(MaxRealTrackZoneSize));

  GetCustomRealTrackZoneSize(AViewInfo as TcxCustomTrackBarViewInfo, ARealTrackZoneSize);

  AViewInfo.TrackZoneRect.Top := AViewInfo.TrackBarRect.Top +
    ((ATrackZoneSize - ARealTrackZoneSize) div 2) + ATopLeftIndent;
  AViewInfo.TrackZoneRect.Bottom := AViewInfo.TrackZoneRect.Top + ARealTrackZoneSize;
  AViewInfo.TrackZoneRect.Left := AViewInfo.TrackBarRect.Left +
    AViewInfo.TrackBarBorderWidth + ScaleFactor.Apply(FromBorderIndent);
  AViewInfo.TrackZoneRect.Right := AViewInfo.TrackBarRect.Right -
    AViewInfo.TrackBarBorderWidth - ScaleFactor.Apply(FromBorderIndent);
end;

procedure TcxCustomTrackBarViewData.DoOnGetThumbRect(var ARect: TRect);
var
  AOnGetThumbRect: TcxGetThumbRectEvent;
begin
  GetOnGetThumbRect(AOnGetThumbRect);
  AOnGetThumbRect(Properties, ARect);
end;

function TcxCustomTrackBarViewData.IsOnGetThumbRectEventAssigned: Boolean;
var
  AOnGetThumbRect: TcxGetThumbRectEvent;
begin
  GetOnGetThumbRect(AOnGetThumbRect);
  Result := Assigned(AOnGetThumbRect);
end;

function TcxCustomTrackBarViewData.IsReverseDirection: Boolean;
begin
  if UseRightToLeftAlignment and (Properties.Orientation = tboHorizontal) then
    Result := not Properties.ReverseDirection
  else
    Result := Properties.ReverseDirection;
end;

procedure TcxCustomTrackBarViewData.UpdateSelection(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  AViewInfo.SelectionStart := Properties.FixPosition(Properties.SelectionStart);
  AViewInfo.SelectionEnd := Properties.FixPosition(Properties.SelectionEnd);
  AViewInfo.ShowSelection := AViewInfo.SelectionStart < AViewInfo.SelectionEnd;
end;

procedure TcxCustomTrackBarViewData.UpdateValue(
  AViewInfo: TcxCustomTrackBarViewInfo; AValue: TcxEditValue);
begin
  AViewInfo.UpdateValue(AValue);
end;

procedure TcxCustomTrackBarViewData.CalculateCustomTrackBarRects(ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo);
var
  ARealTrackZoneSize: Integer;
begin
  ACanvas.Font.Assign(AViewInfo.Font);
  CalculateTrackBarRect(AViewInfo);
  CalculateTrackZoneRect(ACanvas, AViewInfo, ARealTrackZoneSize);
  CalculateTrackSize(AViewInfo, ARealTrackZoneSize);
  CalculateButtonLeftRect(AViewInfo);
  CalculateButtonRightRect(AViewInfo);
  CalculateTrackRect(AViewInfo);
  CalculateThumbSize(AViewInfo);
  CalculatePixelsPerPositionStep(AViewInfo);
  CalculateThumbRect(ACanvas, AViewInfo);
  CalculateSelectionRect(AViewInfo);
end;

procedure TcxCustomTrackBarViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
var
  ATrackBarViewInfo : TcxCustomTrackBarViewInfo;
  ADisplayValue: TcxEditValue;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);

  ATrackBarViewInfo := AViewInfo as TcxCustomTrackBarViewInfo;

  {Standart properties}
  ATrackBarViewInfo.LookAndFeel.Assign(Style.LookAndFeel);
  ATrackBarViewInfo.IsEditClass := False;
  ATrackBarViewInfo.DrawSelectionBar := False;
  ATrackBarViewInfo.HasPopupWindow := False;
  ATrackBarViewInfo.DrawTextFlags := 0;
  ATrackBarViewInfo.DrawSelectionBar := False;
  ATrackBarViewInfo.UpdateTrackBarState;
  {TrackBar properties}
  if Assigned(Edit) and not ATrackBarViewInfo.IsDBEditPaintCopyDrawing then
  begin
    Properties.PrepareDisplayValue(Edit.EditValue, ADisplayValue, Focused);
    UpdateValue(ATrackBarViewInfo, ADisplayValue);
  end;
  CalculateTrackBarViewInfoProperties(AViewInfo);
  CalculateCustomTrackBarViewInfo(ACanvas, Self, ATrackBarViewInfo);
  CalculateCustomTrackBarRects(ACanvas, ATrackBarViewInfo);
end;

procedure TcxCustomTrackBarViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  ADisplayValue: TcxEditValue;
begin
  Properties.PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused);
  UpdateValue(AViewInfo as TcxCustomTrackBarViewInfo, ADisplayValue);
end;

function TcxCustomTrackBarViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
  AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  ASize1: TSize;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace,
    AEditSizeProperties, MinContentSize, AViewInfo);

  ASize1.cx := cxRectWidth(TcxCustomTrackBarViewInfo(AViewInfo).GetThumbRect);
  ASize1.cy := GetTextEditContentSize(ACanvas, Self, dxMeasurePattern, 0,
    AEditSizeProperties, 0, False).cy;
  Result.cx := Result.cx + ASize1.cx;
  Result.cy := Result.cy + ASize1.cy;
end;

procedure TcxCustomTrackBarViewData.GetCustomRealTrackZoneSize(AViewInfo: TcxCustomTrackBarViewInfo;
  var ARealTrackZoneSize: Integer);
var
  ACustomRect: TRect;
begin
  if (Properties.ThumbType = cxttCustom) and
    IsOnGetThumbRectEventAssigned then
  begin
    DoOnGetThumbRect(ACustomRect);
    ACustomRect := AViewInfo.OrientationHelper.GetRealRect(ACustomRect);
    if cxRectHeight(ACustomRect) > ARealTrackZoneSize then
      ARealTrackZoneSize := cxRectHeight(ACustomRect);
  end;
end;

procedure TcxCustomTrackBarViewData.GetOnGetThumbRect(out AValue: TcxGetThumbRectEvent);
begin
  if Edit = nil then
    AValue := Properties.OnGetThumbRect
  else
    TcxCustomTrackBar(Edit).GetOnGetThumbRect(AValue);
end;

function TcxCustomTrackBarViewData.GetProperties: TcxCustomTrackBarProperties;
begin
  Result := TcxCustomTrackBarProperties(FProperties);
end;

{ TcxCustomTrackBarProperties }

constructor TcxCustomTrackBarProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
  FBorderWidth := 0;
  FFrequency := 1;
  FLineSize := 1;
  FMin := 0;
  FMax := 10;
  FOrientation := tboHorizontal;
  FTextOrientation := tbtoHorizontal;
  FPageSize := 2;
  FTrackColor := clWindow;
  FTrackSize := 5;
  FTickColor := clWindowText;
  FSelectionStart := 0;
  FSelectionEnd := 0;
  FSelectionColor := clHighlight;
  FShowTicks := True;
  FThumbType := cxttRegular;
  FShowTrack := True;
  FTickType := tbttTicks;
  FTickMarks := cxtmBottomRight;
  FTickSize := 3;
  FThumbHeight := 12;
  FThumbWidth := 7;
  FThumbColor := clBtnFace;
  FThumbHighlightColor := clSilver;
  FThumbStep := cxtsNormal;
end;

function TcxCustomTrackBarProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TcxCustomTrackBarProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxTrackBar;
end;

function TcxCustomTrackBarProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := ADisplayValue;
end;

class function TcxCustomTrackBarProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TcxTrackBarStyle;
end;

function TcxCustomTrackBarProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoEditing, esoFiltering, esoSorting,
    esoTransparency];
end;

class function TcxCustomTrackBarProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomTrackBarViewInfo;
end;

function TcxCustomTrackBarProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
  Result := inherited IsEditValueValid(EditValue, AEditFocused);
end;

procedure TcxCustomTrackBarProperties.PrepareDisplayValue(const AEditValue:
  TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  LockUpdate(True);
  try
    DisplayValue := FixPosition(EditValueToPosition(AEditValue));
  finally
    LockUpdate(False);
  end;
end;

function TcxCustomTrackBarProperties.EditValueToPosition(
  const AEditValue: TcxEditValue): Integer;
begin
  if not IsVarEmpty(AEditValue) and (VarIsNumeric(AEditValue) or VarIsStr(AEditValue)) then
    Result := VarAsType(AEditValue, varInteger)
  else
    Result := FMin;
end;

procedure TcxCustomTrackBarProperties.SetMax(Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    FMin := Math.Min(FMax, FMin);
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetMin(Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    FMax := Math.Max(FMax, FMin);
    Changed;
  end;
end;

function TcxCustomTrackBarProperties.CanValidate: Boolean;
begin
  Result := True;
end;

function TcxCustomTrackBarProperties.GetValidateErrorText(AErrorKind: TcxEditErrorKind): string;
begin
  Result := cxGetResourceString(@cxSEditValueOutOfBounds);
end;

class function TcxCustomTrackBarProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomTrackBarViewData;
end;

procedure TcxCustomTrackBarProperties.ChangeScale(M: Integer; D: Integer);
begin
  inherited;
  ThumbHeight := MulDiv(ThumbHeight, M, D);
  ThumbWidth := MulDiv(ThumbWidth, M, D);
  TrackSize := MulDiv(TrackSize, M, D);
  TickSize := MulDiv(TickSize, M, D);
end;

procedure TcxCustomTrackBarProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomTrackBarProperties then
    with TcxCustomTrackBarProperties(AProperties) do
    begin
      Self.AutoSize := AutoSize;
      Self.BorderWidth := BorderWidth;
      Self.ReverseDirection := ReverseDirection;
      Self.Frequency := Frequency;
      Self.LineSize := LineSize;
      Self.Min := Min;
      Self.Max := Max;
      Self.Orientation := Orientation;
      Self.TextOrientation := TextOrientation;
      Self.PageSize := PageSize;
      Self.SelectionStart := SelectionStart;
      Self.SelectionEnd := SelectionEnd;
      Self.SelectionColor := SelectionColor;
      Self.ShowChangeButtons := ShowChangeButtons;
      Self.ShowTicks := ShowTicks;
      Self.ThumbStep := ThumbStep;
      Self.ThumbType := ThumbType;
      Self.ShowPositionHint := ShowPositionHint;
      Self.ShowTrack := ShowTrack;
      Self.TickColor := TickColor;
      Self.TickType := TickType;
      Self.TickMarks := TickMarks;
      Self.TrackColor := TrackColor;
      Self.TickSize := Self.ScaleFactor.Apply(TickSize, ScaleFactor);
      Self.TrackSize := Self.ScaleFactor.Apply(TrackSize, ScaleFactor);
      Self.ThumbHeight := Self.ScaleFactor.Apply(ThumbHeight, ScaleFactor);
      Self.ThumbWidth := Self.ScaleFactor.Apply(ThumbWidth, ScaleFactor);
      Self.ThumbColor := ThumbColor;
      Self.ThumbHighlightColor := ThumbHighlightColor;
      Self.OnGetTickLabel := OnGetTickLabel;
      Self.OnGetPositionHint := OnGetPositionHint;
      Self.OnGetThumbRect := OnGetThumbRect;
      Self.OnDrawThumb := OnDrawThumb;
    end;
end;

function TcxCustomTrackBarProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTrackBarProperties.SetLineSize(Value: TcxNaturalNumber);
begin
  Value := Math.Max(Value, 1);
  if Value <> FLineSize then
  begin
    FLineSize := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTrackColor(Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetPageSize(Value: TcxNaturalNumber);
begin
  Value := Math.Max(Value, 1);
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetSelectionStart(Value: Integer);
begin
  if FSelectionStart <> Value then
  begin
    FSelectionStart := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetSelectionEnd(Value: Integer);
begin
  if FSelectionEnd <> Value then
  begin
    FSelectionEnd := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetBorderWidth(Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    if Value < 0 then
      FBorderWidth := 0
    else
      FBorderWidth := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetReverseDirection(Value: Boolean);
begin
  if FReverseDirection <> Value then
  begin
    FReverseDirection := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetFrequency(Value: Integer);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Math.Max(0, Value);
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetOrientation(Value: TcxTrackBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTextOrientation(Value: TcxTrackBarTextOrientation);
begin
  if FTextOrientation <> Value then
  begin
    FTextOrientation := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetShowTrack(Value: Boolean);
begin
  if FShowTrack <> Value then
  begin
    FShowTrack := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetShowTicks(Value: Boolean);
begin
  if FShowTicks <> Value then
  begin
    FShowTicks := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetThumbType(Value: TcxTrackBarThumbType);
begin
  if FThumbType <> Value then
  begin
    FThumbType := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTickColor(Value: TColor);
begin
  if FTickColor <> Value then
  begin
    FTickColor := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTickType(Value: TcxTrackBarTickType);
begin
  if FTickType <> Value then
  begin
    FTickType := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTickMarks(Value: TcxTrackBarTickMarks);
begin
  if FTickMarks <> Value then
  begin
    FTickMarks := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTickSize(Value: TcxNaturalNumber);
begin
  Value := Math.Max(Value, 1);
  if FTickSize <> Value then
  begin
    FTickSize := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetTrackSize(Value: Integer);
begin
  Value := Math.Max(Value, 0);
  if FTrackSize <> Value then
  begin
    FTrackSize := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetThumbHeight(Value: Integer);
begin
  Value := Math.Max(Value, 0);
  if FThumbHeight <> Value then
  begin
    FThumbHeight := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetThumbWidth(Value: Integer);
begin
  Value := Math.Max(Value, 0);
  if FThumbWidth <> Value then
  begin
    FThumbWidth := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetThumbHighlightColor(Value: TColor);
begin
  if FThumbHighlightColor <> Value then
  begin
    FThumbHighlightColor := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetShowChangeButtons(Value: Boolean);
begin
  if FShowChangeButtons <> Value then
  begin
    FShowChangeButtons := Value;
    Changed;
  end;
end;

procedure TcxCustomTrackBarProperties.SetShowPositionHint(Value: Boolean);
begin
  if FShowPositionHint <> Value then
  begin
    FShowPositionHint := Value;
    Changed;
  end;
end;

function TcxCustomTrackBarProperties.FixPosition(const APosition: Integer): Integer;
begin
  Result := Math.Max(Math.Min(APosition, Max), Min);
end;

procedure TcxCustomTrackBarProperties.DoDrawThumb(Sender: TObject; ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  if Assigned(OnDrawThumb) then
    OnDrawThumb(Self, ACanvas, ARect);
end;

{ TcxTrackBarPositionHintHelper }

constructor TcxTrackBarPositionHintHelper.Create(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  inherited Create;
  FTrackBarViewInfo := AViewInfo;
  FPositionHintHidingTimer := cxCreateTimer(PositionHintHidingTimerHandler, 1000, False);
end;

destructor TcxTrackBarPositionHintHelper.Destroy;
begin
  FreeAndNil(FPositionHintHidingTimer);
  inherited Destroy;
end;

procedure TcxTrackBarPositionHintHelper.DisableTimer;
begin
  FPositionHintHidingTimer.Enabled := False;
end;

procedure TcxTrackBarPositionHintHelper.HidePositionHint;
begin
  FPositionHintHidingTimer.Enabled := True;
end;

procedure TcxTrackBarPositionHintHelper.SetDelay(AValue: Integer);
begin
  FPositionHintHidingTimer.Interval := AValue;
end;

function TcxTrackBarPositionHintHelper.CanShowHint: Boolean;
begin
  Result := True;
end;

procedure TcxTrackBarPositionHintHelper.CorrectHintWindowRect(var ARect: TRect);
var
  ATopLeft: TPoint;
  AOwnerControl: TcxCustomTrackBar;
begin
  inherited CorrectHintWindowRect(ARect);
  AOwnerControl := TcxCustomTrackBar(GetOwnerControl);
  if AOwnerControl.ActiveProperties.TickMarks in [cxtmBottomRight, cxtmBoth] then
  begin
    ATopLeft := AOwnerControl.ClientToScreen(AOwnerControl.ViewInfo.GetThumbRect.TopLeft);
    if AOwnerControl.ActiveProperties.Orientation = tboHorizontal then
      ARect := cxRectSetTop(ARect, ATopLeft.Y - cxRectHeight(ARect) - PositionHintIndent)
    else
      ARect := cxRectSetLeft(ARect, ATopLeft.X - cxRectWidth(ARect) - PositionHintIndent);
  end;
end;

function TcxTrackBarPositionHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FTrackBarViewInfo.FEdit;
end;

procedure TcxTrackBarPositionHintHelper.MouseLeave;
begin
  if [tbmpUnderThumb, tbmpSliding] * FTrackBarViewInfo.MouseStates = [] then
    HidePositionHint;
end;

procedure TcxTrackBarPositionHintHelper.PositionHintHidingTimerHandler(
  Sender: TObject);
begin
  FPositionHintHidingTimer.Enabled := False;
  CancelHint;
end;

{ TcxTrackBarController }

constructor TcxTrackBarController.Create(ATrackBar: IcxTrackBar);
begin
  inherited Create;
  FTrackBar := ATrackBar;
  FSlideState := tbksNormal;
end;

destructor TcxTrackBarController.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

procedure TcxTrackBarController.FocusChanged;
begin
  DestroyTimer;
end;

procedure TcxTrackBarController.KeyDown(var Key: Word; Shift: TShiftState);
var
  ANewPosition: Integer;
begin
  if Key = VK_SHIFT then
  begin
    if not IsInplace and (FSlideState <> tbksIncludeSelection) then
    begin
      FSlideState := tbksIncludeSelection;
      FStartSelectionPosition := Position;
    end;
    Exit;
  end;

  ANewPosition := GetPositionAfterKeyDown(ViewInfo.FThumbs[0], Key, Shift);
  if ANewPosition <> Position then
  begin
    if FSlideState = tbksIncludeSelection then
      SetNewSelectionPosition(ANewPosition);

    Position := ANewPosition;
    if Properties.ShowPositionHint then
      ViewInfo.HidePositionHint;
  end;
end;

procedure TcxTrackBarController.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not IsInplace and (Key = VK_SHIFT) then
    FSlideState := tbksNormal;
end;

procedure TcxTrackBarController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure StartSliding(APosition: Integer);
  begin
    Include(ViewInfo.MouseStates, tbmpSliding);
    StoredPosition := APosition;
  end;

var
  ANewPosition: Integer;
begin
  ViewInfo.FHotThumb := ViewInfo.GetThumbFromPos(cxPoint(X, Y));
  ViewInfo.FPressedThumb := ViewInfo.FHotThumb;
  if Button = mbLeft then
  begin
    ANewPosition := Position;
    if PtInRect(ViewInfo.GetTrackBounds, Point(X, Y)) then
      if PtInRect(ViewInfo.GetVisibleThumbRect, Point(X, Y)) then
      begin
        StartSliding(Position);
        if ssCtrl in Shift then
        begin
          FSlideState := tbksNormal;
          ActiveProperties.FSelectionStart := 0;
          ActiveProperties.FSelectionEnd := 0;
        end;
      end
      else
      begin
        ANewPosition := ViewInfo.GetNewPosition(X, Y);
        if ActiveProperties.ThumbStep = cxtsJump then
          StartSliding(ANewPosition);
      end;
    if ActiveProperties.ShowChangeButtons then
      ProcessButtons(ANewPosition, X, Y);

    Position := ANewPosition;
  end;
end;

procedure TcxTrackBarController.MouseEnter;
begin
  inherited;
  Include(ViewInfo.MouseStates, tbmpInControl);
  if (tbmpSliding in ViewInfo.MouseStates) and not (ssLeft in KeyboardStateToShiftState) then
  begin
    Exclude(ViewInfo.MouseStates, tbmpSliding);
    Exclude(ViewInfo.MouseStates, tbmpUnderThumb);
  end;
end;

procedure TcxTrackBarController.MouseLeave;
begin
  ViewInfo.FHotThumb := nil;
  Exclude(ViewInfo.MouseStates, tbmpInControl);
  Exclude(ViewInfo.MouseStates, tbmpUnderThumb);
  ViewInfo.ButtonRightState := cxbsNormal;
  ViewInfo.ButtonLeftState := cxbsNormal;
  ViewInfo.PositionHintHelper.MouseLeave;
end;

procedure TcxTrackBarController.MouseMove(Shift: TShiftState; X, Y: Integer);

  function GetButtonState(AIsPressed: Boolean; const ABounds: TRect): TcxButtonState;
  begin
    Result := cxbsNormal;
    if PtInRect(ABounds, Point(X, Y)) then
    begin
      if ssLeft in Shift then
      begin
        if AIsPressed then
          Result := cxbsPressed;
      end
      else
        Result := cxbsHot
    end
  end;

  procedure ProcessButtons;
  begin
    ViewInfo.ButtonLeftState := GetButtonState(FIsMouseDownOnLeftButton, ViewInfo.GetLeftButtonRect);
    ViewInfo.ButtonRightState := GetButtonState(FIsMouseDownOnRightButton, ViewInfo.GetRightButtonRect);
  end;

begin
  ViewInfo.HotThumb := ViewInfo.GetThumbFromPos(cxPoint(X, Y));
  if (tbmpSliding in ViewInfo.MouseStates) and (ssLeft in KeyboardStateToShiftState) then
    UpdatePos(GetPositionAfterSliding(X, Y), ViewInfo.PressedThumb)
  else
    ViewInfo.MouseMove(X, Y);
  if ActiveProperties.ShowChangeButtons then
    ProcessButtons;
end;

procedure TcxTrackBarController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure InternalProcessButtons;
  begin
    FIsMouseDownOnLeftButton := False;
    FIsMouseDownOnRightButton := False;
    if PtInRect(ViewInfo.GetLeftButtonRect, Point(X, Y)) then
      ViewInfo.ButtonLeftState := cxbsHot
    else
      if PtInRect(ViewInfo.GetRightButtonRect, Point(X, Y)) then
        ViewInfo.ButtonRightState := cxbsHot;
  end;

begin
  DestroyTimer;
  if Button = mbLeft then
  begin
    Exclude(ViewInfo.MouseStates, tbmpSliding);
    if PtInRect(ViewInfo.GetVisibleThumbRect, Point(X, Y)) then
      Include(ViewInfo.MouseStates, tbmpUnderThumb)
    else
    begin
      if Properties.ShowPositionHint then
        ViewInfo.HidePositionHint;
      Exclude(ViewInfo.MouseStates, tbmpUnderThumb);
    end;
    if Properties.ShowChangeButtons then
      InternalProcessButtons;
    SetCaptureControl(nil);
  end;
end;

function TcxTrackBarController.GetPositionAfterSliding(X, Y: Integer): Integer;
begin
  Result := ActiveProperties.FixPosition(StoredPosition +
    MulDiv(ViewInfo.GetMouseDelta(X, Y, GetMouseDownPos),
     ViewInfo.PixelsPerPositionStep.Denominator, ViewInfo.PixelsPerPositionStep.Numerator));
end;

procedure TcxTrackBarController.UpdatePos(AValue: Integer; AThumb: TcxTrackBarThumbViewInfo);
begin
  if AValue <> Position then
  begin
    if (ssShift in KeyboardStateToShiftState) and (FSlideState = tbksIncludeSelection) then
      SetNewSelectionPosition(AValue);
    Position := AValue;
  end;
end;

procedure TcxTrackBarController.CreateTimer;
begin
  if ActiveProperties.ReadOnly {or not FTrackBar.DataBinding.IsDataAvailable} then
    Exit;

  FTimer.Free;
  FTimer := cxCreateTimer(HandleTimer, TrackBarTimerInitialInterval);
end;

procedure TcxTrackBarController.DestroyTimer;
begin
  FreeAndNil(FTimer);
end;

function TcxTrackBarController.GetActiveProperties: TcxCustomTrackBarProperties;
begin
  Result := FTrackBar.GetActiveProperties;
end;

function TcxTrackBarController.GetMouseDownPos: TPoint;
begin
  Result := FTrackBar.GetMouseDownPos;
end;

function TcxTrackBarController.GetPositionAfterKeyDown(
  AThumb: TcxTrackBarThumbViewInfo; var Key: Word; Shift: TShiftState): Integer;
begin
  case Key of
    VK_PRIOR:
      Result := ActiveProperties.FixPosition(ViewInfo.GetCalculatedPosition(AThumb.Position, False, False));
    VK_NEXT:
      Result := ActiveProperties.FixPosition(ViewInfo.GetCalculatedPosition(AThumb.Position, True, False));
    VK_END:
      begin
        if ActiveProperties.ReverseDirection then
          Result := ActiveProperties.Min
        else
          Result := ActiveProperties.Max;
      end;
    VK_HOME:
      begin
        if ActiveProperties.ReverseDirection then
          Result := ActiveProperties.Max
        else
          Result := ActiveProperties.Min;
      end;
    VK_LEFT, VK_UP:
      Result := ActiveProperties.FixPosition(ViewInfo.GetCalculatedPosition(AThumb.Position, False, True));
    VK_RIGHT, VK_DOWN:
      Result := ActiveProperties.FixPosition(ViewInfo.GetCalculatedPosition(AThumb.Position, True, True));
    else
      Result := AThumb.Position;
  end;
end;

function TcxTrackBarController.GetPosition: Integer;
begin
  Result := FTrackBar.GetPosition;
end;

function TcxTrackBarController.GetProperties: TcxCustomTrackBarProperties;
begin
  Result := FTrackBar.GetProperties;
end;

function TcxTrackBarController.GetViewInfo: TcxCustomTrackBarViewInfo;
begin
  Result := FTrackBar.GetViewInfo;
end;

procedure TcxTrackBarController.HandleTimer(Sender: TObject);
begin
  if FTimer.Interval = TrackBarTimerInitialInterval then
    FTimer.Interval := TrackBarTimerInterval;
  if FIsMouseDownOnLeftButton and (ViewInfo.ButtonLeftState = cxbsPressed) then
    Position := PressButton(False)
  else
    if FIsMouseDownOnRightButton and (ViewInfo.ButtonRightState = cxbsPressed) then
      Position := PressButton(True);
end;

function TcxTrackBarController.IsInplace: Boolean;
begin
  Result := FTrackBar.IsInplace;
end;

function TcxTrackBarController.PressButton(AIsInc: Boolean): Integer;
begin
  Result := ViewInfo.GetCalculatedPosition(Position, AIsInc, True, True);
end;

procedure TcxTrackBarController.ProcessButtons(var ANewPosition: Integer; X, Y: Integer);
begin
  if PtInRect(ViewInfo.GetLeftButtonRect, Point(X, Y)) then
  begin
    ViewInfo.ButtonLeftState := cxbsPressed;
    ANewPosition := PressButton(False);
    FIsMouseDownOnLeftButton := True;
  end
  else
    if PtInRect(ViewInfo.GetRightButtonRect, Point(X, Y)) then
    begin
      ViewInfo.ButtonRightState := cxbsPressed;
      ANewPosition := PressButton(True);
      FIsMouseDownOnRightButton := True;
    end;
  if FIsMouseDownOnLeftButton or FIsMouseDownOnRightButton then
    CreateTimer;
end;

procedure TcxTrackBarController.SetNewSelectionPosition(const ANewPosition: Integer);
begin
  if ANewPosition < FStartSelectionPosition then
  begin
    ActiveProperties.FSelectionStart := ANewPosition;
    ActiveProperties.FSelectionEnd := FStartSelectionPosition;
  end
  else
  begin
    ActiveProperties.FSelectionStart := FStartSelectionPosition;
    ActiveProperties.FSelectionEnd := ANewPosition;
  end;
end;

procedure TcxTrackBarController.SetPosition(AValue: Integer);
begin
  FTrackBar.SetPosition(AValue);
end;

{ TcxCustomTrackBar }

constructor TcxCustomTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FController := CreateController;
end;

destructor TcxCustomTrackBar.Destroy;
begin
  FreeAndNil(FController);
  inherited Destroy;
end;

class function TcxCustomTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomTrackBarProperties;
end;

function TcxCustomTrackBar.GetStyle: TcxTrackBarStyle;
begin
  Result := TcxTrackBarStyle(FStyles.Style);
end;

procedure TcxCustomTrackBar.SetStyle(Value: TcxTrackBarStyle);
begin
  FStyles.Style := Value;
end;

function TcxCustomTrackBar.CreateController: TcxTrackBarController;
begin
  Result := TcxTrackBarController.Create(Self);
end;

function TcxCustomTrackBar.DefaultParentColor: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTrackBar.DoValidateDisplayValue(var ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean);
begin
  inherited DoValidateDisplayValue(ADisplayValue, AErrorText, AError);
  if AError then
    ViewInfo.PositionHintHelper.CancelHint;
end;

function TcxCustomTrackBar.GetDisplayText: string;
begin
  Result := IntToStr(Position);
end;

procedure TcxCustomTrackBar.FocusChanged;
begin
  FController.FocusChanged;
  inherited FocusChanged;
end;

procedure TcxCustomTrackBar.Initialize;
begin
  inherited Initialize;
  FEditValue := 0;
  AutoSize := False;
  ControlStyle := ControlStyle - [csDoubleClicks, csClickEvents];
  Width := 196;
  Height := 76;
end;

procedure TcxCustomTrackBar.InternalSetDisplayValue(const Value: TcxEditValue);
begin
  FEditValue := Value;
  EditModified := False;
  SynchronizeDisplayValue;
end;

procedure TcxCustomTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FController.KeyDown(Key, Shift);
end;

procedure TcxCustomTrackBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  FController.KeyUp(Key, Shift);
end;

procedure TcxCustomTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AStoredMouseDownPos: TPoint;
begin
  AStoredMouseDownPos := MouseDownPos;
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FController.MouseDown(Button, Shift, X, Y);
    ShortRefreshContainer(False);
  end
  else
    MouseDownPos := AStoredMouseDownPos;
end;

procedure TcxCustomTrackBar.MouseEnter(AControl: TControl);
begin
  inherited;
  FController.MouseEnter;
end;

procedure TcxCustomTrackBar.MouseLeave(AControl: TControl);
begin
  FController.MouseLeave;
  inherited MouseLeave(AControl);
  ShortRefreshContainer(False);
end;

procedure TcxCustomTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AOldMouseStates: TcxTrackBarMouseStates;
  AOldButtonLeftState: TcxButtonState;
  AOldButtonRightState: TcxButtonState;
  AThumb: TcxTrackBarThumbViewInfo;
begin
  AOldButtonLeftState := ViewInfo.ButtonLeftState;
  AOldButtonRightState := ViewInfo.ButtonRightState;
  AOldMouseStates := ViewInfo.MouseStates;
  AThumb := ViewInfo.HotThumb;
  inherited MouseMove(Shift, X, Y);
  FController.MouseMove(Shift, X, Y);
  if (ViewInfo.MouseStates <> AOldMouseStates) or
    (ViewInfo.ButtonLeftState <> AOldButtonLeftState) or
    (ViewInfo.ButtonRightState <> AOldButtonRightState) or
    (AThumb <> ViewInfo.HotThumb) then
      ShortRefreshContainer(False);
end;

procedure TcxCustomTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FController.MouseUp(Button, Shift, X, Y);
  ShortRefreshContainer(False);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomTrackBar.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  AEditSizeProperties := cxDefaultEditSizeProperties;
  AEditSizeProperties.MaxLineCount := 1;
  AEditSizeProperties.Width := ViewInfo.TextRect.Right - ViewInfo.TextRect.Left;
end;

procedure TcxCustomTrackBar.SetInternalDisplayValue(Value: TcxEditValue);
begin
  EditValue := Value;
end;

procedure TcxCustomTrackBar.SynchronizeDisplayValue;
begin
  ShortRefreshContainer(False);
end;

function TcxCustomTrackBar.WantNavigationKeys: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTrackBar.InternalSetPosition(Value: Integer);
begin
  BeginUserAction;
  try
    if (Value <> Position) and DoEditing then
    begin
      InternalEditValue := Value;
      ActiveProperties.Changed;
      ModifiedAfterEnter := True;
      if ValidateEdit then
        InternalPostEditValue;
      ViewInfo.ProcessPositionHint;
    end;
  finally
    EndUserAction;
  end;
end;

function TcxCustomTrackBar.GetMouseDownPos: TPoint;
begin
  Result := MouseDownPos;
end;

function TcxCustomTrackBar.GetProperties: TcxCustomTrackBarProperties;
begin
  Result := TcxCustomTrackBarProperties(inherited Properties);
end;

function TcxCustomTrackBar.GetActiveProperties: TcxCustomTrackBarProperties;
begin
  Result := TcxCustomTrackBarProperties(InternalGetActiveProperties);
end;

procedure TcxCustomTrackBar.GetOnGetThumbRect(out AValue: TcxGetThumbRectEvent);
begin
  AValue := Properties.OnGetThumbRect;
  if not Assigned(AValue) then
    AValue := ActiveProperties.OnGetThumbRect;
end;

function TcxCustomTrackBar.GetViewInfo: TcxCustomTrackBarViewInfo;
begin
  Result := TcxCustomTrackBarViewInfo(FViewInfo);
end;

procedure TcxCustomTrackBar.SetProperties(Value: TcxCustomTrackBarProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomTrackBar.WMLButtonUp(var Message: TWMLButtonUp);
begin
  ControlState := ControlState - [csClicked];
  inherited;
end;

procedure TcxCustomTrackBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
  if IsInplace then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

function TcxCustomTrackBar.GetPosition: Integer;
begin
  Result := ActiveProperties.FixPosition(
    ActiveProperties.EditValueToPosition(FEditValue));
end;

procedure TcxCustomTrackBar.SetPosition(Value: Integer);
begin
  if not IsLoading then
    Value := ActiveProperties.FixPosition(Value);
  if Value <> Position then
    InternalEditValue := Value;
end;

{ TcxTrackBar }

class function TcxTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTrackBarProperties;
end;

function TcxTrackBar.GetActiveProperties: TcxTrackBarProperties;
begin
  Result := TcxTrackBarProperties(InternalGetActiveProperties);
end;

function TcxTrackBar.GetProperties: TcxTrackBarProperties;
begin
  Result := TcxTrackBarProperties(inherited Properties);
end;

procedure TcxTrackBar.SetProperties(Value: TcxTrackBarProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterTrackBarHelper }

class procedure TcxFilterTrackBarHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomSpinEditProperties(AProperties) do
  begin
    Buttons.Add;
    Buttons.Add;
    MinValue := TcxCustomTrackBarProperties(AEditProperties).Min;
    MaxValue := TcxCustomTrackBarProperties(AEditProperties).Max;
    LargeIncrement := TcxCustomTrackBarProperties(AEditProperties).PageSize;
  end;
end;

{ TcxCustomTrackBarFadingHelper }

constructor TcxCustomTrackBarFadingHelper.Create(AViewInfo: TcxCustomTrackBarViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

function TcxCustomTrackBarFadingHelper.GetEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := ViewInfo;
end;

{ TcxTrackBarThumbFadingHelper }

procedure TcxTrackBarThumbFadingHelper.Invalidate;
begin
  Invalidate(GetThumbRect, False);
end;

procedure TcxTrackBarThumbFadingHelper.DrawThumb(ACanvas: TcxCanvas;
  const R: TRect; AState: Integer);
begin
  ViewInfo.DrawThumb(ACanvas, R, AState);
end;

procedure TcxTrackBarThumbFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: Integer): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(GetThumbRect, True);
    DrawThumb(Result.cxCanvas, Result.ClientRect, AState);
  end;

begin
  AFadeInImage := PrepareImage(TUS_HOT);
  AFadeOutImage := PrepareImage(TUS_NORMAL);
end;

function TcxTrackBarThumbFadingHelper.GetThumbRect: TRect;
begin
  Result := ViewInfo.GetThumbRect;
end;

{ TcxTrackBarCustomButtonFadingHelper }

function TcxTrackBarCustomButtonFadingHelper.CanFade: Boolean;
begin
  Result := inherited CanFade and not IsRectEmpty(ButtonBounds);
end;

procedure TcxTrackBarCustomButtonFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(ButtonBounds, True);
    DrawButton(Result.cxCanvas, Result.ClientRect, AState);
  end;

begin
  AFadeOutImage := PrepareImage(cxbsNormal);
  AFadeInImage := PrepareImage(cxbsHot);
end;

procedure TcxTrackBarCustomButtonFadingHelper.Invalidate;
begin
  Invalidate(ButtonBounds, False);
end;

{ TcxTrackBarLeftButtonFadingHelper }

procedure TcxTrackBarLeftButtonFadingHelper.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  ViewInfo.DrawButtonLeft(ACanvas, R, AState);
end;

function TcxTrackBarLeftButtonFadingHelper.GetButtonBounds: TRect;
begin
  Result := ViewInfo.GetLeftButtonRect;
end;

{ TcxTrackBarRightButtonFadingHelper }

procedure TcxTrackBarRightButtonFadingHelper.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  ViewInfo.DrawButtonRight(ACanvas, R, AState);
end;

function TcxTrackBarRightButtonFadingHelper.GetButtonBounds: TRect;
begin
  Result := ViewInfo.GetRightButtonRect;
end;

{ TcxTrackBarThumbViewInfo }

constructor TcxTrackBarThumbViewInfo.Create(
  AViewInfo: TcxCustomTrackBarViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FFadingHelper := CreateFadingHelper;
end;

destructor TcxTrackBarThumbViewInfo.Destroy;
begin
  FreeAndNil(FFadingHelper);
  inherited;
end;

function TcxTrackBarThumbViewInfo.CreateFadingHelper: TcxTrackBarThumbFadingHelper;
begin
  Result := TcxTrackBarThumbFadingHelper.Create(FViewInfo);
end;

procedure TcxTrackBarThumbViewInfo.SetState(AValue: Integer);
begin
  if FState <> AValue then
  begin
    FFadingHelper.CheckStartFading(
      TrackBarStateToButtonStates[FState],
      TrackBarStateToButtonStates[AValue]);
    FState := AValue;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxTrackBarProperties, scxSEditRepositoryTrackBarItem);
  FilterEditsController.Register(TcxTrackBarProperties, TcxFilterTrackBarHelper);

finalization
  FilterEditsController.Unregister(TcxTrackBarProperties, TcxFilterTrackBarHelper);
  GetRegisteredEditProperties.Unregister(TcxTrackBarProperties);

end.
