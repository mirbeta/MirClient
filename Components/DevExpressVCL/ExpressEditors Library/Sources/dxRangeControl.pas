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

unit dxRangeControl;

{$I cxVer.inc}

interface

uses
  Types, Variants,
  Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils, StrUtils,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls, dxGdiPlusClasses, dxCoreGraphics,
  cxEdit, cxExtEditConsts, cxExtEditUtils, cxLookAndFeelPainters, cxGeometry, dxThemeConsts,
  cxFilterControlUtils, cxGraphics, cxLookAndFeels, cxTextEdit, cxVariants, Math, dxCustomHint, dxTypeHelpers,
  dxAnimation, Generics.Collections, Generics.Defaults;

const
  // hit test constants
  rchtNone                              = 0;
  rchtZoomScrollBar                     = 1;
  rchtZoomScrollBarSelectionScrollThumb = 2;
  rchtZoomScrollBarViewportScrollThumb  = 3;
  rchtZoomScrollBarMinZoomGrip          = 4;
  rchtZoomScrollBarMaxZoomGrip          = 5;
  rchtClientArea                        = 6;
  rchtClientAreaSelectedRange           = 7;
  rchtClientAreaMinSelectionThumb       = 8;
  rchtClientAreaMaxSelectionThumb       = 9;
  rchtRuler                             = 10;
  rchtRulerElement                      = 11;

type
  TdxCustomRangeControl = class;
  TdxRangeControlCustomClientProperties = class;
  TdxRangeControlCustomNumericClientProperties = class;
  TdxRangeControlCustomDateTimeClientProperties = class;
  TdxRangeControlCustomDateTimeHeaderClientProperties = class;
  TdxCustomRangeControlViewInfo = class;
  TdxCustomRangeControlHitTest = class;
  TdxCustomRangeControlStyle = class;
  TdxRangeControlDateTimeScale = class;
  TdxRangeControlDateTimeScales = class;
  TdxRangeControlDateTimeAutoFormatHelper = class;
  TdxRangeControlClientPropertiesClass = class of TdxRangeControlCustomClientProperties;
  TdxRangeControlDateTimeAutoFormatHelperClass = class of TdxRangeControlDateTimeAutoFormatHelper;

  IdxRangeControlClient = interface
    ['{4C5CCFF4-F330-4FC8-AF03-34299DDCBDE3}']
    procedure AttachRangeControl(ARangeControl: TdxCustomRangeControl);
    procedure DetachRangeControl(ARangeControl: TdxCustomRangeControl);
    function GetPropertiesClass: TdxRangeControlClientPropertiesClass;
    procedure SelectedRangeChanged;
  end;

  TdxRangeEditValue = Variant;

  TdxRangeControlElementViewInfo = class
  strict private
    FBounds: TRect;
    FViewInfo: TdxCustomRangeControlViewInfo;
    FVisible: Boolean;

    function GetControl: TdxCustomRangeControl;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
  protected
    function GetHitTestIndex: Integer; virtual;
    function IsHotTrackSupported: Boolean; virtual;
    procedure SetHitTest(AHitTest: TdxCustomRangeControlHitTest); virtual;
    property Visible: Boolean read FVisible write FVisible;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); virtual;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); virtual;
    function GetHitTest(AHitTest: TdxCustomRangeControlHitTest): Boolean;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    function PtInElement(const APoint: TPoint): Boolean; virtual;
    //
    property Bounds: TRect read FBounds;
    property Control: TdxCustomRangeControl read GetControl;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TdxCustomRangeControlViewInfo read FViewInfo;
  end;

  TdxRangeControlScaleElementViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FMinValue, FMaxValue: Variant;
    FText: string;
    FTextBounds: TRect;
  protected
    function GetContentOffsets: TRect;
    function GetHitTestIndex: Integer; override;
    procedure Calculate;
    function IsHotTrackSupported: Boolean; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    property MinValue: Variant read FMinValue write FMinValue;
    property MaxValue: Variant read FMaxValue write FMaxValue;
    property Text: string read FText write FText;
  end;

  TdxRangeControlDateTimeScaleUnit = (rcduSecond, rcduMinute, rcduHour, rcduDay, rcduWeek, rcduMonth, rcduQuarter, rcduYear);
  TdxRangeControlDateTimeScaleUnits = set of TdxRangeControlDateTimeScaleUnit;

  TdxRangeControlRulerScaleViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FElements: TObjectList<TdxRangeControlScaleElementViewInfo>;
    FScale: TdxRangeControlDateTimeScale;
  protected
    function GetHitTestIndex: Integer; override;
    procedure SetHitTest(AHitTest: TdxCustomRangeControlHitTest); override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    function AddElement: TdxRangeControlScaleElementViewInfo;
    procedure Calculate;
    procedure Paint(ACanvas: TcxCanvas); override;
    property Elements: TObjectList<TdxRangeControlScaleElementViewInfo> read FElements;
    property Scale: TdxRangeControlDateTimeScale read FScale write FScale;
  end;

  TdxRangeControlRulerViewInfo = class(TdxRangeControlElementViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  end;

  TdxRangeControlDateTimeHeaderClientRulerViewInfo = class(TdxRangeControlRulerViewInfo)
  strict private
    FScaleInfos: TObjectList<TdxRangeControlRulerScaleViewInfo>;
  protected
    procedure SetHitTest(AHitTest: TdxCustomRangeControlHitTest); override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    function AddScale(AScale: TdxRangeControlDateTimeScale): TdxRangeControlRulerScaleViewInfo;
    procedure Calculate;
    procedure Paint(ACanvas: TcxCanvas); override;
    property ScaleInfos: TObjectList<TdxRangeControlRulerScaleViewInfo> read FScaleInfos;
  end;

  TdxRangeControlContentViewInfo = class(TdxRangeControlElementViewInfo);

  TdxRangeControlErrorInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FText: string;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    property Text: string read FText write FText;
  end;

  TdxRangeElementType = (retMin, retMax);
  TdxRangeElementTypes = set of TdxRangeElementType;

  TdxRangeControlZoomScrollBarZoomGripViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FGlyphBounds: TRect;
    FType: TdxRangeElementType;
  protected
    function GetHitTestIndex: Integer; override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo;
      AType: TdxRangeElementType); reintroduce; virtual;
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo = class(TdxRangeControlElementViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo = class(TdxRangeControlElementViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    function PtInElement(const APoint: TPoint): Boolean; override;
  end;

  TdxRangeControlZoomScrollBarViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FSelectionScrollThumb: TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo;
    FViewportScrollThumb: TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo;
    FMinZoomGrip: TdxRangeControlZoomScrollBarZoomGripViewInfo;
    FMaxZoomGrip: TdxRangeControlZoomScrollBarZoomGripViewInfo;
  protected
    function GetHitTestIndex: Integer; override;
    procedure SetHitTest(AHitTest: TdxCustomRangeControlHitTest); override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property MaxZoomGrip: TdxRangeControlZoomScrollBarZoomGripViewInfo read FMaxZoomGrip;
    property MinZoomGrip: TdxRangeControlZoomScrollBarZoomGripViewInfo read FMinZoomGrip;
    property SelectionScrollThumb: TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo read FSelectionScrollThumb;
    property ViewportScrollThumb: TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo read FViewportScrollThumb;
  end;

  TdxRangeControlClientAreaSelectedRangeTargetViewInfo = class(TdxRangeControlElementViewInfo)
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxRangeControlClientAreaSelectedRangeViewInfo = class(TdxRangeControlElementViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxRangeControlClientAreaSelectionThumbViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FType: TdxRangeElementType;
  protected
    function GetHitTestIndex: Integer; override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo;
      AType: TdxRangeElementType); reintroduce; virtual;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxRangeControlClientAreaViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FMinSelectionThumb: TdxRangeControlClientAreaSelectionThumbViewInfo;
    FMaxSelectionThumb: TdxRangeControlClientAreaSelectionThumbViewInfo;
    FSelectedRange: TdxRangeControlClientAreaSelectedRangeViewInfo;
    FSelectedRangeTarget: TdxRangeControlClientAreaSelectedRangeTargetViewInfo;
  protected
    function GetHitTestIndex: Integer; override;
    procedure SetHitTest(AHitTest: TdxCustomRangeControlHitTest); override;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property MaxSelectionThumb: TdxRangeControlClientAreaSelectionThumbViewInfo read FMaxSelectionThumb;
    property MinSelectionThumb: TdxRangeControlClientAreaSelectionThumbViewInfo read FMinSelectionThumb;
    property SelectedRange: TdxRangeControlClientAreaSelectedRangeViewInfo read FSelectedRange;
    property SelectedRangeTarget: TdxRangeControlClientAreaSelectedRangeTargetViewInfo read FSelectedRangeTarget;
  end;

  TdxRangeControlCustomClientStyle = class(TcxOwnedPersistent)
  strict private
    function GetProperties: TdxRangeControlCustomClientProperties;
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    property Properties: TdxRangeControlCustomClientProperties read GetProperties;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TdxRangeControlCustomClientViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FContent: TdxRangeControlContentViewInfo;
    FRuler: TdxRangeControlRulerViewInfo;
    FNormalizedFirstTickMarkValue: Double;
    FScaleInterval: Variant;
    function GetProperties: TdxRangeControlCustomClientProperties;
    procedure SetScaleInterval(AValue: Variant);
  protected
    function CreateContentInfo: TdxRangeControlContentViewInfo; virtual;
    function CreateRulerInfo: TdxRangeControlRulerViewInfo; virtual;
    procedure DrawGridLines(ACanvas: TcxCanvas);
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure DrawScale(ACanvas: TcxCanvas); virtual;
    function GetGridLineColor: TdxAlphaColor; virtual;
    function GetScaleHeight: Integer; virtual;
    property NormalizedFirstTickMarkValue: Double read FNormalizedFirstTickMarkValue write FNormalizedFirstTickMarkValue;
    property ScaleInterval: Variant read FScaleInterval write SetScaleInterval;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); override;
    property Properties: TdxRangeControlCustomClientProperties read GetProperties;
    property Content: TdxRangeControlContentViewInfo read FContent;
    property Ruler: TdxRangeControlRulerViewInfo read FRuler;
  end;

  TdxRangeControlCustomClientProperties = class(TcxOwnedInterfacedPersistent)
  strict private
    FLockCount: Integer;
    FScaleInterval: Variant;
    FStyle: TdxRangeControlCustomClientStyle;
    function GetMaxValue: Variant;
    function GetMinValue: Variant;
    function GetRangeControl: TdxCustomRangeControl;
    function GetScaleInterval: Variant;
    procedure SetScaleIntervalMinWidth(AValue: Integer);
    procedure SetScaleInterval(Value: Variant);
    procedure SetStyle(Value: TdxRangeControlCustomClientStyle);
  private
    FMaxValue: Variant;
    FMinValue: Variant;
    FScaleIntervalMinWidth: Integer;
  protected
    procedure BeginUpdate;
    procedure Changed;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckMaxValue; virtual;
    procedure CheckMinPossibleRangeInterval(var AValue: Double); virtual;
    procedure CheckMinValue; virtual;
    function CreateStyle: TdxRangeControlCustomClientStyle; virtual;
    function CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo; virtual;
    procedure DoAssign(Source: TPersistent); override;
    function DoGetNextNormalizedAcceptableValue(const AValue: Double; AInterval: Variant; out AIsOverflow: Boolean): Double; virtual;
    function DoGetNextValue(const AOriginal, AInterval: Variant): Variant; virtual;
    function DoGetNormalizedValue(const AValue: Variant): Double; virtual;
    procedure EndUpdate;
    function GetLabelText(const Value: Variant): string; virtual;
    function GetNextValue(const AOriginal, AInterval: Variant; out AIsOverflow: Boolean): Variant; virtual;
    function GetNormalizedVisibleRangeMaxInterval: Double; virtual;
    function GetOriginalValue(const AValue: Double): Variant; overload; virtual;
    function GetOriginalValue(const AValue: Double; AType: Word): Variant; overload; virtual;
    function GetRangeDelta: Variant; virtual;
    function GetRangeValueType: Word; virtual;
    function GetScaleIntervalValueType: Word; virtual;
    function GetVisibleRangeValueType: Word; virtual;
    function IsAutoZoomSupported: Boolean; virtual;
    procedure ScaleIntervalMinWidthChanged; virtual;
    procedure SetMaxValue(Value: Variant); virtual;
    procedure SetMinValue(Value: Variant); virtual;
    property RangeControl: TdxCustomRangeControl read GetRangeControl;
    property ScaleInterval: Variant read GetScaleInterval write SetScaleInterval;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetInterval(const AMin, AMax: Double): Variant; virtual;
    function GetMinSelectedInterval: Variant; virtual;
    function GetNearestNormalizedAcceptableValue(const AValue: Double): Double; virtual;
    function GetNextNormalizedAcceptableValue(const AValue: Double; const AInterval: Variant; out AIsOverflow: Boolean): Double; virtual;
    function GetNormalizedValue(const AValue: Variant): Double;
    function IsRangeEmpty: Boolean; virtual;
    property MaxValue: TdxRangeEditValue read GetMaxValue write SetMaxValue;
    property ScaleIntervalMinWidth: Integer read FScaleIntervalMinWidth write SetScaleIntervalMinWidth;
    property MinValue: TdxRangeEditValue read GetMinValue write SetMinValue;
    property Style: TdxRangeControlCustomClientStyle read FStyle write SetStyle;
  end;

  TdxRangeControlSimpleRulerViewInfo = class(TdxRangeControlRulerViewInfo)
  strict private
    function GetProperties: TdxRangeControlCustomClientProperties;
    procedure CalculateLabelPosition(ACanvas: TcxCanvas; const AText: string; ATickMarkPos: Integer; out ARect: TRect);
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    property Properties: TdxRangeControlCustomClientProperties read GetProperties;
  end;

  TdxRangeControlNumericClientViewInfo = class(TdxRangeControlCustomClientViewInfo)
  strict private
    function GetProperties: TdxRangeControlCustomNumericClientProperties;
  protected
    function CreateRulerInfo: TdxRangeControlRulerViewInfo; override;
    procedure DrawScale(ACanvas: TcxCanvas); override;
    function GetGridLineColor: TdxAlphaColor; override;
  public
    procedure CalculateBounds(const ABounds: TRect); override;

    property Properties: TdxRangeControlCustomNumericClientProperties read GetProperties;
  end;

  TdxRangeControlClientStyle = class(TdxRangeControlCustomClientStyle)
  strict private
    FGridLineColor: TdxAlphaColor;
    FRulerColor: TdxAlphaColor;
    FRulerTextColor: TColor;
    procedure SetGridLineColor(Value: TdxAlphaColor);
    procedure SetRulerColor(Value: TdxAlphaColor);
    procedure SetRulerTextColor(Value: TColor);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property GridLineColor: TdxAlphaColor read FGridLineColor write SetGridLineColor default dxacDefault;
    property RulerColor: TdxAlphaColor read FRulerColor write SetRulerColor default dxacDefault;
    property RulerTextColor: TColor read FRulerTextColor write SetRulerTextColor default clDefault;
  end;

  TdxRangeControlCustomNumericClientProperties = class(TdxRangeControlCustomClientProperties)
  strict private
    FDisplayFormat: string;
    FRangeValueType: Word;
    function Convert(const AValue: Variant; AType: Word): Variant;
    function IsDisplayFormatStored: Boolean;
    procedure RangeValueTypeChanged;
    procedure SetDisplayFormat(const Value: string);
    procedure SetRangeValueType(Value: Word);
  protected
    procedure CheckMaxValue; override;
    procedure CheckMinPossibleRangeInterval(var AValue: Double); override;
    procedure CheckMinValue; override;
    function CreateStyle: TdxRangeControlCustomClientStyle; override;
    function CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo; override;
    procedure DoAssign(Source: TPersistent); override;
    function GetDefaultDisplayFormat: string; virtual;
    function GetLabelPattern: string; virtual;
    function GetLabelText(const Value: Variant): string; override;
    function GetMaxPossibleRangeInterval: Variant; overload; virtual;
    function GetMaxPossibleRangeInterval(AType: Word): Variant; overload; virtual;
    function GetMinPossibleRangeInterval(AType: Word): Variant; virtual;
    function GetNextValue(const AOriginal, AInterval: Variant; out AIsOverflow: Boolean): Variant; override;
    function GetOriginalValue(const AValue: Double; AType: Word): Variant; overload; override;
    function GetRangeValueType: Word; override;
    function GetScaleIntervalValueType: Word; override;
    function GetVisibleRangeValueType: Word; override;
  public
    constructor Create(AOwner: TPersistent); override;
    function GetInterval(const AMin, AMax: Double): Variant; override;
    function GetNearestNormalizedAcceptableValue(const AValue: Double): Double; override;
    function IsRangeValueTypeSupported(AValue: Word): Boolean;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat stored IsDisplayFormatStored;
    property RangeValueType: Word read FRangeValueType write SetRangeValueType default varInteger;
  end;

  TdxRangeControlNumericClientProperties = class(TdxRangeControlCustomNumericClientProperties)
  published
    property DisplayFormat;
    property RangeValueType;
    property MaxValue;
    property ScaleIntervalMinWidth default 5;
    property MinValue;
    property ScaleInterval;
    property Style;
  end;

  TdxRangeControlDateTimeClientHelper = class
  strict private
    class function DayOfTheQuarter(const ADate: TDateTime): Integer;
    class function DaysInQuarter(const ADate: TDateTime): Double;
    class function StartOfTheQuarter(const ADate: TDateTime): TDateTime;
  protected
    class function IncQuarter(const AValue: TDateTime; ANumberOfQuarters: Integer = 1): TDateTime;
    class function QuarterSpan(const AMin: TDateTime; AMax: TDateTime): Double;
  public
    class function CeilDate(const ADate: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
    class function GetDateInterval(const AMin, AMax: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): Double;
    class function IncDate(const AValue: Double; AScaleUnit: TdxRangeControlDateTimeScaleUnit; AIncrementValue: Int64 = 1): Double;
    class function RoundDate(const ADate: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
    class function TruncDate(const ADate: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
  end;

  TdxRangeControlDateTimeClientViewInfo = class(TdxRangeControlCustomClientViewInfo)
  strict private
    function GetProperties: TdxRangeControlCustomDateTimeClientProperties;
  protected
    function CreateRulerInfo: TdxRangeControlRulerViewInfo; override;
    procedure DrawScale(ACanvas: TcxCanvas); override;
    function GetGridLineColor: TdxAlphaColor; override;
  public
    procedure CalculateBounds(const ABounds: TRect); override;
    property Properties: TdxRangeControlCustomDateTimeClientProperties read GetProperties;
  end;

  TdxRangeControlDateTimeScale = class(TcxOwnedPersistent)
  strict private
    FVisible: Boolean;
    function GetActive: Boolean;
    function GetAutoFormatHelper: TdxRangeControlDateTimeAutoFormatHelperClass;
    function GetScales: TdxRangeControlDateTimeScales;
    procedure SetActive(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure DoAssign(Source: TPersistent); override;
    function GetFormatCount: Integer; virtual;
    function GetScaleUnit: TdxRangeControlDateTimeScaleUnit; virtual; abstract;

    property AutoFormatHelper: TdxRangeControlDateTimeAutoFormatHelperClass read GetAutoFormatHelper;
    property Scales: TdxRangeControlDateTimeScales read GetScales;
  public
    function CeilDate(const ADate: TDateTime): TDateTime; virtual;
    function GetDateInterval(const AMin, AMax: TDateTime): Double; virtual;
    function IncDate(const AValue: Double; AIncrementValue: Int64 = 1): Double; virtual;
    function RoundDate(const ADate: TDateTime): TDateTime; virtual;
    function TruncDate(const ADate: TDateTime): TDateTime; virtual;
  public
    function GetDisplayText(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    function GetDefaultDisplayText(const ADate: TDateTime): string; virtual;
    property Active: Boolean read GetActive write SetActive;
    property ScaleUnit: TdxRangeControlDateTimeScaleUnit read GetScaleUnit;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  TdxRangeControlDateTimeScaleClass = class of TdxRangeControlDateTimeScale;

  TdxRangeControlPredefinedDateTimeScale = class(TdxRangeControlDateTimeScale)
  strict private
    FDisplayFormat: string;
    FScaleUnit: TdxRangeControlDateTimeScaleUnit;
    procedure SetDisplayFormat(const Value: string);
  protected
    procedure DoAssign(Source: TPersistent); override;
    function GetDefaultDisplayFormat: string; virtual;
    function GetFormatCount: Integer; override;
    function GetScaleUnit: TdxRangeControlDateTimeScaleUnit; override;
  public
    constructor Create(AOwner: TPersistent; AScaleUnit: TdxRangeControlDateTimeScaleUnit); reintroduce; virtual;
    function GetDefaultDisplayText(const ADate: TDateTime): string; override;
  published
    property Active default False;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
  end;

  TdxRangeControlPredefinedDateHeaderScale = class(TdxRangeControlPredefinedDateTimeScale)
  strict private
    function IsDisplayFormatStored: Boolean;
  protected
    function GetDefaultDisplayFormat: string; override;
  public
    function GetDisplayText(const ADate: TDateTime; AFormatId: Integer): string; override;
    function GetDefaultDisplayText(const ADate: TDateTime): string; override;
  published
    property DisplayFormat stored IsDisplayFormatStored;
    property Visible;
  end;

  TdxRangeControlDateTimeScaleList = class(TList<TdxRangeControlDateTimeScale>);

  TdxRangeControlDateTimeScales = class(TcxOwnedPersistent)
  strict private
    FPrimaryScale: TdxRangeControlDateTimeScale;
    FCustomScales: TdxRangeControlDateTimeScaleList;
    FDay: TdxRangeControlDateTimeScale;
    FHour: TdxRangeControlDateTimeScale;
    FMinute: TdxRangeControlDateTimeScale;
    FMonth: TdxRangeControlDateTimeScale;
    FQuarter:  TdxRangeControlDateTimeScale;
    FWeek: TdxRangeControlDateTimeScale;
    FYear: TdxRangeControlDateTimeScale;
    function GetPrimaryScale: TdxRangeControlDateTimeScale;
    procedure SetPrimaryScale(Value: TdxRangeControlDateTimeScale);
    procedure SetDay(Value: TdxRangeControlDateTimeScale);
    procedure SetHour(Value: TdxRangeControlDateTimeScale);
    procedure SetMinute(Value: TdxRangeControlDateTimeScale);
    procedure SetMonth(Value: TdxRangeControlDateTimeScale);
    procedure SetQuarter(AValue:  TdxRangeControlDateTimeScale);
    procedure SetWeek(Value: TdxRangeControlDateTimeScale);
    procedure SetYear(Value: TdxRangeControlDateTimeScale);
  protected
    procedure Changed;
    function CreatePredefinedDateTimeScale(AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale; virtual;
    procedure DoAssign(Source: TPersistent); override;
    property CustomScales: TdxRangeControlDateTimeScaleList read FCustomScales;
    property PrimaryScale: TdxRangeControlDateTimeScale read GetPrimaryScale write SetPrimaryScale;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetScale(AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale;
    function RegisterScale(AClass: TdxRangeControlDateTimeScaleClass): TdxRangeControlDateTimeScale;
    procedure UnRegisterScale(AScale: TdxRangeControlDateTimeScale);
  published
    property Day: TdxRangeControlDateTimeScale read FDay write SetDay;
    property Hour: TdxRangeControlDateTimeScale read FHour write SetHour;
    property Minute: TdxRangeControlDateTimeScale read FMinute write SetMinute;
    property Month: TdxRangeControlDateTimeScale read FMonth write SetMonth;
    property Quarter:  TdxRangeControlDateTimeScale read FQuarter write SetQuarter;
    property Week: TdxRangeControlDateTimeScale read FWeek write SetWeek;
    property Year: TdxRangeControlDateTimeScale read FYear write SetYear;
  end;

  TdxRangeControlDateTimeClientGetLabelTextEvent = procedure(Sender: TObject; const AValue: TDateTime;
    var ADisplayText: string) of object;

  TdxRangeControlCustomDateTimeClientProperties = class(TdxRangeControlCustomClientProperties)
  strict private
    FOnGetLabelText: TdxRangeControlDateTimeClientGetLabelTextEvent;
    FScales: TdxRangeControlDateTimeScales;
    function GetPrimaryScale: TdxRangeControlDateTimeScale;
    procedure SetPrimaryScale(Value: TdxRangeControlDateTimeScale);
    procedure SetScales(AValue: TdxRangeControlDateTimeScales);
  protected
    function CeilDate(const ADate: TDateTime): TDateTime; virtual;
    function CreatePredefinedDateTimeScale(AScales: TdxRangeControlDateTimeScales;
       AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale; virtual;
    function CreateScales: TdxRangeControlDateTimeScales; virtual;
    function CreateStyle: TdxRangeControlCustomClientStyle; override;
    function CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoGetLabelText(const AValue: TDateTime; var ADisplayText: string);
    function DoGetNextValue(const AOriginal, AInterval: Variant): Variant; override;
    function GetActualPrimaryScale: TdxRangeControlDateTimeScale; virtual;
    function GetDateInterval(const AMin, AMax: TDateTime): Double; virtual;
    function GetLabelText(const AValue: Variant): string; override;
    function GetRangeValueType: Word; override;
    function GetVisibleRangeValueType: Word; override;
    function IncDate(const AValue: Double; AIncrementValue: Int64 = 1): Double; virtual;
    function RoundDate(const ADate: TDateTime): TDateTime; virtual;
    procedure PrimaryScaleChanged; virtual;
    procedure ScalesChanged; virtual;
    procedure SetMaxValue(Value: Variant); override;
    procedure SetMinValue(Value: Variant); override;
    function TruncDate(const ADate: TDateTime): TDateTime; virtual;

    property ActualPrimaryScale: TdxRangeControlDateTimeScale read GetActualPrimaryScale;
    property PrimaryScale: TdxRangeControlDateTimeScale read GetPrimaryScale write SetPrimaryScale;
    property OnGetLabelText: TdxRangeControlDateTimeClientGetLabelTextEvent read FOnGetLabelText write FOnGetLabelText;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetInterval(const AMin, AMax: Double): Variant; override;
    function GetNearestNormalizedAcceptableValue(const AValue: Double): Double; override;
    property Scales: TdxRangeControlDateTimeScales read FScales write SetScales;
  end;

  TdxRangeControlDateTimeClientProperties = class(TdxRangeControlCustomDateTimeClientProperties)
  strict private
    function GetScale: TdxRangeControlDateTimeScale;
    procedure SetScale(const Value: TdxRangeControlDateTimeScale);
  published
    property MaxValue;
    property ScaleIntervalMinWidth default 5;
    property MinValue;
    property ScaleInterval;
    property Scale: TdxRangeControlDateTimeScale read GetScale write SetScale stored False;
    property Scales;
    property Style;
    property OnGetLabelText;
  end;

  TdxRangeControlDateTimeAutoFormatHelper = class
  strict private
    class var
      FDayFormats: TStringList;
  strict private
    class constructor Initialize;
{$IFDEF DELPHIXE}
    class destructor Finalize;
{$ELSE}
  {$HINTS OFF} // #Ch http://qc.embarcadero.com/wc/qcmain.aspx?d=80785
    class destructor Finalize;
  {$HINTS ON}
{$ENDIF}
    class function FindBeginOfSequence(const AStr: string; AStartPosition: Integer;
      const ASoughtForChars: string; AForward: Boolean): Integer;
    class function FindEndOfSequence(const AStr: string; AStartPosition: Integer; const ASoughtForChar: Char): Integer;
  protected
    class function ChooseFormat(const ADate: TDateTime; AFont: TFont; AWidth: Integer; AScale: TdxRangeControlDateTimeScale): Integer; virtual;
    class procedure InitializeDayFormats; virtual;
    class function Strip(const APattern: string; AFormatChar: Char; ALength: Integer): string;
    class function StripDay(const APattern: string): string;
    class function StripDayOfWeek(const APattern: string): string;
    class function StripMonth(const APattern: string): string;
    class function StripYear(const APattern: string): string;
  public
    class function FormatDay(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatHour(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatMonth(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatMinute(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatQuarter(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatSecond(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatWeek(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function FormatYear(const ADate: TDateTime; AFormatId: Integer): string; virtual;
    class function GetFormatCount(AScaleUnit: TdxRangeControlDateTimeScaleUnit): Integer; virtual;
  end;

  TdxRangeControlDateTimeHeaderClientContentElementViewInfo = class(TdxRangeControlElementViewInfo)
  strict private
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
  public
    property MinDate: TDateTime read FMinDate write FMinDate;
    property MaxDate: TDateTime read FMaxDate write FMaxDate;
  end;

  TdxRangeControlDateTimeHeaderClientContentViewInfo = class(TdxRangeControlContentViewInfo)
  strict private
    FElements: TList<TdxRangeControlDateTimeHeaderClientContentElementViewInfo>;
  protected
    function AddElement: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
    function CreateContentElementViewInfo: TdxRangeControlDateTimeHeaderClientContentElementViewInfo; virtual;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property Elements: TList<TdxRangeControlDateTimeHeaderClientContentElementViewInfo> read FElements;
  end;

  TdxRangeControlDateTimeHeaderClientViewInfo = class(TdxRangeControlCustomClientViewInfo)
  strict private
    function GetContent: TdxRangeControlDateTimeHeaderClientContentViewInfo;
    function GetProperties: TdxRangeControlCustomDateTimeHeaderClientProperties;
    function GetRuler: TdxRangeControlDateTimeHeaderClientRulerViewInfo;
  protected
    function CreateContentInfo: TdxRangeControlContentViewInfo; override;
    function CreateRulerInfo: TdxRangeControlRulerViewInfo; override;
    function GetScaleHeight: Integer; override;
  public
    procedure CalculateBounds(const ABounds: TRect); override;
    property Content: TdxRangeControlDateTimeHeaderClientContentViewInfo read GetContent;
    property Properties: TdxRangeControlCustomDateTimeHeaderClientProperties read GetProperties;
    property Ruler: TdxRangeControlDateTimeHeaderClientRulerViewInfo read GetRuler;
  end;

  TdxRangeControlDateTimeHeaderClientStyle = class(TdxRangeControlCustomClientStyle)
  strict private
    FRulerHeaderColor: TdxAlphaColor;
    FRulerHeaderHotColor: TdxAlphaColor;
    FRulerHeaderTextColor: TColor;
    procedure SetRulerHeaderColor(Value: TdxAlphaColor);
    procedure SetRulerHeaderHotColor(Value: TdxAlphaColor);
    procedure SetRulerHeaderTextColor(Value: TColor);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property RulerHeaderColor: TdxAlphaColor read FRulerHeaderColor write SetRulerHeaderColor default dxacDefault;
    property RulerHeaderHotColor: TdxAlphaColor read FRulerHeaderHotColor write SetRulerHeaderHotColor default dxacDefault;
    property RulerHeaderTextColor: TColor read FRulerHeaderTextColor write SetRulerHeaderTextColor default clDefault;
  end;

  TdxRangeControlCustomDateTimeHeaderClientProperties = class(TdxRangeControlCustomDateTimeClientProperties)
  strict private
    FAutoFormatHelper: TdxRangeControlDateTimeAutoFormatHelperClass;
    FAutoFormatScaleCaptions: Boolean;
    FVisibleScales: TdxRangeControlDateTimeScaleList;
    procedure CheckVisibleRangeMaxInterval;
    procedure SetAutoFormatHelper(Value: TdxRangeControlDateTimeAutoFormatHelperClass);
    procedure SetAutoFormatScaleCaptions(Value: Boolean);
  private
    FMaxIntervalCount: Double;
  protected
    function CreatePredefinedDateTimeScale(AScales: TdxRangeControlDateTimeScales;
      AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale; override;
    function CreateStyle: TdxRangeControlCustomClientStyle; override;
    function CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoUpdateVisibleScales(AValues: TdxRangeControlDateTimeScaleList); virtual;
    function GetNormalizedVisibleRangeMaxInterval: Double; override;
    function IsAutoZoomSupported: Boolean; override;
    procedure PrimaryScaleChanged; override;
    procedure ScaleIntervalMinWidthChanged; override;
    procedure ScalesChanged; override;
    procedure SetMaxValue(Value: Variant); override;
    procedure SetMinValue(Value: Variant); override;
    procedure UpdateVisibleScales;
    property VisibleScales: TdxRangeControlDateTimeScaleList read FVisibleScales;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetMinSelectedInterval: Variant; override;
    property AutoFormatHelper: TdxRangeControlDateTimeAutoFormatHelperClass read FAutoFormatHelper write SetAutoFormatHelper;
    property AutoFormatScaleCaptions: Boolean read FAutoFormatScaleCaptions write SetAutoFormatScaleCaptions default True;
  end;

  TdxRangeControlDateTimeHeaderClientProperties = class(TdxRangeControlCustomDateTimeHeaderClientProperties)
  published
    property AutoFormatScaleCaptions;
    property MaxValue;
    property MinValue;
    property PrimaryScale stored False;
    property ScaleIntervalMinWidth default 30;
    property Scales;
    property Style;
  end;

  TdxCustomRangeControlHitTest = class
  strict private
    FFlags: Int64;
    FHitPoint: TPoint;
    FHitObject: TdxRangeControlElementViewInfo;
    function GetBitState(AIndex: Integer): Boolean;
  private
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
  public
    procedure Clear;
    property HitAtZoomScrollBar: Boolean index rchtZoomScrollBar read GetBitState;
    property HitAtZoomScrollBarSelectionScrollThumb: Boolean index rchtZoomScrollBarSelectionScrollThumb read GetBitState;
    property HitAtZoomScrollBarViewportScrollThumb: Boolean index rchtZoomScrollBarViewportScrollThumb read GetBitState;
    property HitAtZoomScrollBarMinZoomGrip: Boolean index rchtZoomScrollBarMinZoomGrip read GetBitState;
    property HitAtZoomScrollBarMaxZoomGrip: Boolean index rchtZoomScrollBarMaxZoomGrip read GetBitState;
    property HitAtClientArea: Boolean index rchtClientArea read GetBitState;
    property HitAtClientAreaSelectedRange: Boolean index rchtClientAreaSelectedRange read GetBitState;
    property HitAtClientAreaMinSelectionThumb: Boolean index rchtClientAreaMinSelectionThumb read GetBitState;
    property HitAtClientAreaMaxSelectionThumb: Boolean index rchtClientAreaMaxSelectionThumb read GetBitState;
    property HitAtRulerElement: Boolean index rchtRulerElement read GetBitState;
    property HitObject: TdxRangeControlElementViewInfo read FHitObject write FHitObject;
    property HitPoint: TPoint read FHitPoint write FHitPoint;
  end;

  TdxCustomRangeControlViewInfo = class
  strict private
    FBounds: TRect;
    FClientAreaNormalFactor: Double;
    FContentImage: TdxSmartImage;
    FContentMaskImage: TdxSmartImage;
    FRangeControl: TdxCustomRangeControl;
    FDragging: Boolean;
    FError: Boolean;
    FErrorInfo: TdxRangeControlErrorInfo;
    FHitTest: TdxCustomRangeControlHitTest;
    FClientArea: TdxRangeControlClientAreaViewInfo;
    FClientInfo: TdxRangeControlCustomClientViewInfo;
    FHotInfo: TdxRangeControlElementViewInfo;
    FPressedInfo: TdxRangeControlElementViewInfo;
    FZoomScrollBar: TdxRangeControlZoomScrollBarViewInfo;
    function GetClientProperties: TdxRangeControlCustomClientProperties;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetRuler: TdxRangeControlRulerViewInfo;
    function GetStyle: TdxCustomRangeControlStyle;
  private
    FCheckZoomNeeded: Boolean;
    FIsClientInfoCacheValid: Boolean;
    FIsClientInfoDirty: Boolean;
  protected
    FMinZoomScrollBarSelectedPos, FMaxZoomScrollBarSelectedPos: Integer;
    FMinZoomScrollBarVisiblePos, FMaxZoomScrollBarVisiblePos: Integer;
    FMinClientSelectedPos, FMaxClientSelectedPos: Integer;
    FMinClientSelectedTargetPos, FMaxClientSelectedTargetPos: Integer;
    procedure CheckCursorAtPoint(const APoint: TPoint; var ACursor: TCursor); virtual;
    function CreateHitTest: TdxCustomRangeControlHitTest; virtual;
    procedure CreateInfos;
    function GetBackgroundColor: TColor; virtual;
    function GetClientInfoTextFont: TFont; virtual;
    function GetElementBorderColor: TdxAlphaColor; virtual;
    function GetPositionInClientAreaFromNormalValue(const AValue: Double): Integer;
    function GetRulerLabelFont: TFont; virtual;
    function GetSelectedRegionBackgroundColor: TdxAlphaColor; virtual;
    function GetSelectedRegionBorderColor: TdxAlphaColor; virtual;
    function GetSelectedRangeThumbColor: TColor; virtual;
    procedure RecreateClientInfo;
    property Dragging: Boolean read FDragging write FDragging;
    property Error: Boolean read FError write FError;
    property HotInfo: TdxRangeControlElementViewInfo read FHotInfo write FHotInfo;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property PressedInfo: TdxRangeControlElementViewInfo read FPressedInfo write FPressedInfo;
    property RangeControl: TdxCustomRangeControl read FRangeControl;
    property Style: TdxCustomRangeControlStyle read GetStyle;
  public
    constructor Create(ARangeControl: TdxCustomRangeControl);
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect);
    procedure CalculateHitTest(const APoint: TPoint; AShift: TShiftState);
    procedure Paint(ACanvas: TcxCanvas);
    function GetPositionFromValue(const AValue: Variant): Integer;

    property ClientArea: TdxRangeControlClientAreaViewInfo read FClientArea;
    property ClientAreaNormalFactor: Double read FClientAreaNormalFactor;
    property ClientInfo: TdxRangeControlCustomClientViewInfo read FClientInfo;
    property ErrorInfo: TdxRangeControlErrorInfo read FErrorInfo;
    property Ruler: TdxRangeControlRulerViewInfo read GetRuler;
    property ZoomScrollBar: TdxRangeControlZoomScrollBarViewInfo read FZoomScrollBar;

    property HitTest: TdxCustomRangeControlHitTest read FHitTest;
    property ClientProperties: TdxRangeControlCustomClientProperties read GetClientProperties;
  end;

  TdxRangeControlBaseStyle = class(TcxOwnedPersistent)
  strict private
    function GetRangeControl: TdxCustomRangeControl;
  protected
    procedure Changed; virtual;
    property RangeControl: TdxCustomRangeControl read GetRangeControl;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TdxCustomRangeControlStyle = class(TdxRangeControlBaseStyle)
  strict private
    FBorderColor: TColor;
    FColor: TColor;
    FDisabledColor: TdxAlphaColor;
    FElementBorderColor: TdxAlphaColor;
    FOutOfRangeColor: TdxAlphaColor;
    FSelectedRangeThumbColor: TColor;
    FSelectionBorderColor: TdxAlphaColor;
    FSelectionColor: TdxAlphaColor;
    FZoomScrollBarColor: TColor;
    FZoomScrollBarSelectedRangeThumbColor: TColor;
    FZoomScrollBarVisibleRangeThumbColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetDisabledColor(Value: TdxAlphaColor);
    procedure SetElementBorderColor(Value: TdxAlphaColor);
    procedure SetOutOfRangeColor(Value: TdxAlphaColor);
    procedure SetSelectedRangeThumbColor(Value: TColor);
    procedure SetSelectionBorderColor(Value: TdxAlphaColor);
    procedure SetSelectionColor(Value: TdxAlphaColor);
    procedure SetZoomScrollBarColor(Value: TColor);
    procedure SetZoomScrollBarSelectedRangeThumbColor(Value: TColor);
    procedure SetZoomScrollBarVisibleRangeThumbColor(Value: TColor);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDefault;
    property Color: TColor read FColor write SetColor default clDefault;
    property DisabledColor: TdxAlphaColor read FDisabledColor write SetDisabledColor default $A000000;
    property ElementBorderColor: TdxAlphaColor read FElementBorderColor write SetElementBorderColor default dxacDefault;
    property OutOfRangeColor: TdxAlphaColor read FOutOfRangeColor write SetOutOfRangeColor default dxacDefault;
    property SelectedRangeThumbColor: TColor read FSelectedRangeThumbColor write SetSelectedRangeThumbColor default clDefault;
    property SelectionBorderColor: TdxAlphaColor read FSelectionBorderColor write SetSelectionBorderColor default dxacDefault;
    property SelectionColor: TdxAlphaColor read FSelectionColor write SetSelectionColor default dxacDefault;
    property ZoomScrollBarColor: TColor read FZoomScrollBarColor write SetZoomScrollBarColor default clDefault;
    property ZoomScrollBarSelectedRangeThumbColor: TColor read FZoomScrollBarSelectedRangeThumbColor write SetZoomScrollBarSelectedRangeThumbColor default clDefault;
    property ZoomScrollBarVisibleRangeThumbColor: TColor read FZoomScrollBarVisibleRangeThumbColor write SetZoomScrollBarVisibleRangeThumbColor default clDefault;
  end;

  TdxRangeControlStyle = class(TdxCustomRangeControlStyle)
  published
    property BorderColor;
    property Color;
    property DisabledColor;
    property ElementBorderColor;
    property OutOfRangeColor;
    property SelectedRangeThumbColor;
    property SelectionBorderColor;
    property SelectionColor;
    property ZoomScrollBarColor;
    property ZoomScrollBarSelectedRangeThumbColor;
    property ZoomScrollBarVisibleRangeThumbColor;
  end;

  TdxRangeControlClientDrawEvent = procedure (Sender: TdxCustomRangeControl;
    ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean) of object;

  TdxCustomRangeControl = class(TcxControl, IdxSkinSupport)
  strict private const
    AnimationTime = 450;
    AnimationTransitionEffect: TdxAnimationTransitionEffect = ateAccelerateDecelerate;
  strict private
    FSelectedRangeAnimationActive: Boolean;
    FSelectedRangeAnimationController: TdxAnimationTransition;
    FSelectedRangeAnimationLength: Integer;
    FSelectedRangeAnimationNormalizedMaxStartValue: Double;
    FSelectedRangeAnimationNormalizedMinStartValue: Double;

    FVisibleRangeAnimationActive: Boolean;
    FVisibleRangeAnimationController: TdxAnimationTransition;
    FVisibleRangeAnimationLength: Integer;
    FVisibleRangeAnimationNormalizedMinStartValue: Double;
    FVisibleRangeAnimationNormalizedMinFinishValue: Double;

    FAnimation: Boolean;
    FClient: TComponent;
    FClientProperties: TdxRangeControlCustomClientProperties;
    FClientPropertiesClass: TdxRangeControlClientPropertiesClass;
    FIsViewInfoDirty: Boolean;
    FLockCount: Integer;
    FLockNotifierCount: Integer;
    FStyle: TdxCustomRangeControlStyle;
    FViewInfo: TdxCustomRangeControlViewInfo;

    FDragPointOffset: Integer;
    FStoreSelectedRangeInterval: Boolean;

    FNormalizedSelectedRangeMaxValue: Double;
    FNormalizedSelectedRangeMinValue: Double;
    FNormalizedVisibleRangeMaxValue: Double;
    FNormalizedVisibleRangeMinValue: Double;
    FNormalizedVisibleRangeMaxInterval: Double;
    FNormalizedVisibleRangeMinInterval: Double;
    FNormalizedVisibleRangeStoredInterval: Double;

    FShowRuler: Boolean;
    FShowZoomScrollBar: Boolean;
    FOnDrawContent: TdxRangeControlClientDrawEvent;
    FOnDrawScale: TdxRangeControlClientDrawEvent;
    FOnSelectedRangeChanged: TNotifyEvent;
    FClientPropertiesEvents:  TNotifyEvent;

    procedure DoInternalCancelMode;
    procedure SetNormalizedSelectedRangeByMaxValue(AValue: Double);
    procedure SetNormalizedSelectedRangeByMinValue(AValue: Double);
    procedure DoSelectedRangeAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure DoSelectedRangeAnimationTerminate(Sender: TObject);
    procedure DoVisibleRangeAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure DoVisibleRangeAnimationTerminate(Sender: TObject);
    function GetClientPropertiesClassName: string;
    function GetHitTest: TdxCustomRangeControlHitTest;
    procedure GetNormalizedIntervalFromPosInClientArea(APos: Integer; out AMin, AMax: Double);
    function GetNormalizedValueFromPosition(APos: Integer;
      AScaleAreaViewInfo: TdxRangeControlElementViewInfo; const AMin, AMax: Double): Double;
    function GetNormalizedValueFromPositionInClientArea(APos: Integer): Double;
    function GetNormalizedValueFromPositionInZoomScrollBarArea(APos: Integer): Double;
    function GetSelectedRangeMaxValue: Variant;
    function GetSelectedRangeMinValue: Variant;
    function GetVisibleRangeMaxValue: Variant;
    function GetVisibleRangeMinValue: Variant;
    function GetVisibleRangeMaxScaleFactor: Double;
    function IsRangeValueStored: Boolean;
    procedure OffsetVisibleRange(const AValue: Double; AAnimated: Boolean);
    procedure RecreateClientProperties(Value: TdxRangeControlClientPropertiesClass);
    procedure SetClient(Value: TComponent);
    procedure SetClientProperties(Value: TdxRangeControlCustomClientProperties);
    procedure SetClientPropertiesClass(Value: TdxRangeControlClientPropertiesClass);
    procedure SetClientPropertiesClassName(const AValue: string);
    procedure SetNormalizedVisibleRangeMaxValue(AValue: Double);
    procedure SetNormalizedVisibleRangeMinValue(AValue: Double);
    procedure SetShowRuler(Value: Boolean);
    procedure SetShowZoomScrollBar(Value: Boolean);
    procedure SetStyle(Value: TdxCustomRangeControlStyle);
    procedure SetViewportPosition(APos: Integer; AAnimated: Boolean);
    procedure SetViewportStartValue(const AValue: Double; AAnimated: Boolean);
    procedure SetVisibleRangeMaxValue(Value: TdxRangeEditValue);
    procedure SetVisibleRangeMinValue(Value: TdxRangeEditValue);
    procedure SetVisibleRangeMaxScaleFactor(Value: Double);
    procedure StoreVisibleRangeInterval;
  private
    FNormalizedSelectedEditingMinValue: Double;
    FNormalizedSelectedEditingMaxValue: Double;

    FStartSelectionTargetIntervalMinValue: Double;
    FStartSelectionTargetIntervalMaxValue: Double;
    FEndSelectionTargetIntervalMinValue: Double;
    FEndSelectionTargetIntervalMaxValue: Double;
  protected
    function AllowAnimation: Boolean;
    procedure BoundsChanged; override;
    procedure SetNormalizedSelectedRange(const AMin, AMax: Double; AChangeType: TdxRangeElementTypes; AAnimated: Boolean);
    procedure CheckChanges;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;
    procedure DoPaint; override;
    procedure FontChanged; override;
    procedure InitControl; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;

    procedure Changed; virtual;
    procedure ClientPropertiesChanged; virtual;
    function CreateStyle: TdxRangeControlStyle; virtual;
    procedure CreateSubclasses;
    function CreateViewInfo: TdxCustomRangeControlViewInfo; virtual;
    procedure DestroySubclasses;
    procedure DoCancelMode; override;
    function DoOnDrawContent(ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo): Boolean;
    function DoOnDrawScale(ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo): Boolean;
    procedure DrawBorder(ACanvas: TcxCanvas); override;
    function GetBorderSize: Integer; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    procedure InitializeRanges; virtual;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsCancelModeNeeded: Boolean; virtual;
    function IsLocked: Boolean;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LockClientNotification;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure NotifyClient;
    procedure ResetRanges; virtual;
    procedure SetSelectedRangeMaxValue(Value: TdxRangeEditValue); virtual;
    procedure SetSelectedRangeMinValue(Value: TdxRangeEditValue); virtual;
    procedure StopSelectedRangeAnimation;
    procedure StopVisibleRangeAnimation;
    procedure SynchronizeSelectedRangeEditingValue(AAnimated: Boolean);
    procedure UnlockClientNotification;
    procedure UpdateVisibleRangeMaxInterval(AValue: Double);
    procedure VisibleRangeMaxScaleChanged;
    property NormalizedSelectedRangeMaxValue: Double read FNormalizedSelectedRangeMaxValue write FNormalizedSelectedRangeMaxValue;
    property NormalizedSelectedRangeMinValue: Double read FNormalizedSelectedRangeMinValue write FNormalizedSelectedRangeMinValue;
    property NormalizedVisibleRangeMaxValue: Double read FNormalizedVisibleRangeMaxValue write SetNormalizedVisibleRangeMaxValue;
    property NormalizedVisibleRangeMinValue: Double read FNormalizedVisibleRangeMinValue write SetNormalizedVisibleRangeMinValue;
    property ViewInfo: TdxCustomRangeControlViewInfo read FViewInfo;
    property OnDrawContent: TdxRangeControlClientDrawEvent read FOnDrawContent write FOnDrawContent;
    property OnDrawScale: TdxRangeControlClientDrawEvent read FOnDrawScale write FOnDrawScale;
    property OnSelectedRangeChanged: TNotifyEvent read FOnSelectedRangeChanged write FOnSelectedRangeChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure ContentChanged;
    procedure EndUpdate(AForceUpdate: Boolean = True);
    function GetClient: IdxRangeControlClient;
    function GetPositionFromValue(const AValue: Variant): Integer;
    procedure MakeSelectedRangeVisible(AAnimated: Boolean);

    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property Animation: Boolean read FAnimation write FAnimation default True;
    property Client: TComponent read FClient write SetClient;
    property ClientPropertiesClassName: string read GetClientPropertiesClassName write SetClientPropertiesClassName;
    property ClientProperties: TdxRangeControlCustomClientProperties read FClientProperties write SetClientProperties;
    property ClientPropertiesClass: TdxRangeControlClientPropertiesClass read FClientPropertiesClass write SetClientPropertiesClass;
    property HitTest: TdxCustomRangeControlHitTest read GetHitTest;
    property SelectedRangeMaxValue: TdxRangeEditValue read GetSelectedRangeMaxValue write SetSelectedRangeMaxValue stored IsRangeValueStored;
    property SelectedRangeMinValue: TdxRangeEditValue read GetSelectedRangeMinValue write SetSelectedRangeMinValue stored IsRangeValueStored;
    property ShowRuler: Boolean read FShowRuler write SetShowRuler default True;
    property ShowZoomScrollBar: Boolean read FShowZoomScrollBar write SetShowZoomScrollBar default True;
    property Style: TdxCustomRangeControlStyle read FStyle write SetStyle;
    property VisibleRangeMaxScaleFactor: Double read GetVisibleRangeMaxScaleFactor write SetVisibleRangeMaxScaleFactor;
    property VisibleRangeMaxValue: TdxRangeEditValue read GetVisibleRangeMaxValue write SetVisibleRangeMaxValue stored IsRangeValueStored;
    property VisibleRangeMinValue: TdxRangeEditValue read GetVisibleRangeMinValue write SetVisibleRangeMinValue stored IsRangeValueStored;
    property ClientPropertiesEvents: TNotifyEvent read FClientPropertiesEvents write FClientPropertiesEvents;
  end;

  TdxRangeControl = class(TdxCustomRangeControl)
  published
    property Align;
    property Anchors;
    property Animation;
    property Client;
    property ClientPropertiesClassName;
    property ClientProperties;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property LookAndFeel;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelectedRangeMaxValue;
    property SelectedRangeMinValue;
    property ShowHint;
    property ShowRuler;
    property ShowZoomScrollBar;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleRangeMaxScaleFactor;
    property VisibleRangeMaxValue;
    property VisibleRangeMinValue;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawContent;
    property OnDrawScale;
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
    property OnSelectedRangeChanged;
    property OnStartDock;
    property OnStartDrag;
    // nested events
    property ClientPropertiesEvents;
  end;

function dxRangeControlClients: TcxRegisteredClasses;

implementation

uses
  DateUtils, cxEditConsts, cxDateUtils, cxFormats, dxGdiPlusApi;

{$IFNDEF DELPHIXE2}
const
  MinSingle   =  1.4012984643248170709e-45;
  MaxSingle   =  340282346638528859811704183484516925440.0;
  MinDouble   =  4.9406564584124654418e-324;
  MaxDouble   =  1.7976931348623157081e+308;
{$ENDIF}
{$IFDEF DELPHI102TOKYO}
const
  MinSingle   =  MinSingleDenormal;
  MinDouble   =  MinDoubleDenormal;
{$ENDIF}

var
  FRangeControlClients: TcxRegisteredClasses;

function dxRangeControlClients: TcxRegisteredClasses;
begin
  if FRangeControlClients = nil then
    FRangeControlClients := TcxRegisteredClasses.Create;
  Result := FRangeControlClients;
end;

{ TdxRangeControlElementViewInfo }

constructor TdxRangeControlElementViewInfo.Create(
  AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

destructor TdxRangeControlElementViewInfo.Destroy;
begin
  if ViewInfo.HotInfo = Self then
    ViewInfo.HotInfo := nil;
  if ViewInfo.PressedInfo = Self then
    ViewInfo.PressedInfo := nil;
  if ViewInfo.HitTest.HitObject = Self then
    ViewInfo.HitTest.Clear;
  inherited;
end;

procedure TdxRangeControlElementViewInfo.CalculateBounds(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

function TdxRangeControlElementViewInfo.GetControl: TdxCustomRangeControl;
begin
  Result := ViewInfo.RangeControl;
end;

function TdxRangeControlElementViewInfo.GetHitTest(
  AHitTest: TdxCustomRangeControlHitTest): Boolean;
begin
  Result := (GetHitTestIndex <> rchtNone) and PtInElement(AHitTest.HitPoint);
  if Result then
    SetHitTest(AHitTest);
end;

procedure TdxRangeControlElementViewInfo.Paint(ACanvas: TcxCanvas);
begin
end;

function TdxRangeControlElementViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, APoint);
end;

function TdxRangeControlElementViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtNone;
end;

function TdxRangeControlElementViewInfo.IsHotTrackSupported: Boolean;
begin
  Result := False;
end;

procedure TdxRangeControlElementViewInfo.SetHitTest(AHitTest: TdxCustomRangeControlHitTest);
begin
  AHitTest.SetBitState(GetHitTestIndex, True);
  AHitTest.HitObject := Self;
end;

function TdxRangeControlElementViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ViewInfo.Painter;
end;

function TdxRangeControlElementViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.ScaleFactor;
end;

{ TdxRangeControlScaleElementViewInfo }

procedure TdxRangeControlScaleElementViewInfo.Calculate;
var
  AContentBounds: TRect;
begin
  AContentBounds := cxRectContent(Bounds, GetContentOffsets);
  FTextBounds := cxRectCenter(AContentBounds, cxTextSize(ViewInfo.GetRulerLabelFont, FText));
  if FTextBounds.Width > AContentBounds.Width then
  begin
    FTextBounds := cxRectOffsetHorz(FTextBounds, AContentBounds.Left - FTextBounds.Left);
    if FTextBounds.Right > AContentBounds.Right then
      FTextBounds.Right := AContentBounds.Right;
  end;
end;

function TdxRangeControlScaleElementViewInfo.GetContentOffsets: TRect;
var
  ATextOffset: Integer;
begin
  ATextOffset := ScaleFactor.Apply(cxTextOffset);
  Result := cxRect(ATextOffset, ATextOffset, ATextOffset, ATextOffset);
end;

function TdxRangeControlScaleElementViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtRulerElement;
end;

function TdxRangeControlScaleElementViewInfo.IsHotTrackSupported: Boolean;
begin
  Result := True;
end;

procedure TdxRangeControlScaleElementViewInfo.Paint(ACanvas: TcxCanvas);
var
  AIsHot: Boolean;
  AColor: TdxAlphaColor;
  ATextColor: TColor;
  AStyle: TdxRangeControlDateTimeHeaderClientStyle;
begin
  AIsHot := (ViewInfo.PressedInfo = Self) or (ViewInfo.HotInfo = Self) and (ViewInfo.PressedInfo = nil);
  AStyle := ViewInfo.ClientProperties.Style as TdxRangeControlDateTimeHeaderClientStyle;
  if AIsHot then
    AColor := AStyle.RulerHeaderHotColor
  else
    AColor := AStyle.RulerHeaderColor;

  Painter.DrawRangeControlScaledRulerHeader(ACanvas, Bounds, AIsHot, AColor, ViewInfo.GetElementBorderColor, ScaleFactor);
  ACanvas.Font := ViewInfo.GetRulerLabelFont;
  ATextColor := AStyle.RulerHeaderTextColor;
  if ATextColor = clDefault then
    ATextColor := Painter.GetRangeControlLabelColor;
  ACanvas.Font.Color := ATextColor;
  ACanvas.DrawTexT(FText, FTextBounds, cxShowEndEllipsis);
end;

{ TdxRangeControlRulerScaleViewInfo }

constructor TdxRangeControlRulerScaleViewInfo.Create(AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited;
  FElements := TObjectList<TdxRangeControlScaleElementViewInfo>.Create;
end;

destructor TdxRangeControlRulerScaleViewInfo.Destroy;
begin
  FreeAndNil(FElements);
  inherited;
end;

function TdxRangeControlRulerScaleViewInfo.AddElement: TdxRangeControlScaleElementViewInfo;
begin
  Result := TdxRangeControlScaleElementViewInfo.Create(ViewInfo);
  FElements.Add(Result);
end;

procedure TdxRangeControlRulerScaleViewInfo.Calculate;
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    FElements[I].Calculate;
end;

function TdxRangeControlRulerScaleViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtRuler;
end;

procedure TdxRangeControlRulerScaleViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    FElements[I].Paint(ACanvas);
end;

procedure TdxRangeControlRulerScaleViewInfo.SetHitTest(
  AHitTest: TdxCustomRangeControlHitTest);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FElements.Count - 1 do
    if FElements[I].GetHitTest(AHitTest) then
      Break;
end;

{ TdxRangeControlRulerViewInfo }

function TdxRangeControlRulerViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtRuler;
end;

{ TdxRangeControlDateTimeHeaderClientRulerViewInfo }

constructor TdxRangeControlDateTimeHeaderClientRulerViewInfo.Create(
  AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited;
  FScaleInfos := TObjectList<TdxRangeControlRulerScaleViewInfo>.Create;
end;

destructor TdxRangeControlDateTimeHeaderClientRulerViewInfo.Destroy;
begin
  FreeAndNil(FScaleInfos);
  inherited;
end;

function TdxRangeControlDateTimeHeaderClientRulerViewInfo.AddScale(AScale: TdxRangeControlDateTimeScale): TdxRangeControlRulerScaleViewInfo;
begin
  Result := TdxRangeControlRulerScaleViewInfo.Create(ViewInfo);
  ScaleInfos.Add(Result);
  Result.Scale := AScale;
end;

procedure TdxRangeControlDateTimeHeaderClientRulerViewInfo.Calculate;
var
  I: Integer;
begin
  for I := 0 to ScaleInfos.Count - 1 do
    FScaleInfos[I].Calculate;
end;

procedure TdxRangeControlDateTimeHeaderClientRulerViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to ScaleInfos.Count - 1 do
    FScaleInfos[I].Paint(ACanvas);
end;

procedure TdxRangeControlDateTimeHeaderClientRulerViewInfo.SetHitTest(
  AHitTest: TdxCustomRangeControlHitTest);
var
  I: Integer;
begin
  inherited SetHitTest(AHitTest);
  for I := 0 to FScaleInfos.Count - 1 do
    if FScaleInfos[I].GetHitTest(AHitTest) then
      Break;
end;

{ TdxRangeControlErrorInfo }

procedure TdxRangeControlErrorInfo.Paint(ACanvas: TcxCanvas);
begin
  ACanvas.Font := ViewInfo.GetClientInfoTextFont;
  ACanvas.Font.Color := Painter.GetRangeControlLabelColor;
  ACanvas.DrawTexT(FText, Bounds, cxAlignVCenter or cxAlignHCenter);
end;

{ TdxRangeControlZoomAndScrollBarZoomGripViewInfo }

procedure TdxRangeControlZoomScrollBarZoomGripViewInfo.CalculateBounds(const ABounds: TRect);
var
  R: TRect;
begin
  inherited CalculateBounds(ABounds);
  R := cxRectCenter(Bounds, Painter.GetRangeControlScaledSizingGlyphSize(ScaleFactor));
  if FType = retMin then
    R := cxRectOffsetHorz(R, ScaleFactor.Apply(1));
  FGlyphBounds := R;
end;

constructor TdxRangeControlZoomScrollBarZoomGripViewInfo.Create(
  AViewInfo: TdxCustomRangeControlViewInfo; AType: TdxRangeElementType);
begin
  inherited Create(AViewInfo);
  FType := AType;
end;

function TdxRangeControlZoomScrollBarZoomGripViewInfo.GetHitTestIndex: Integer;
begin
  if FType = retMin then
    Result := rchtZoomScrollBarMinZoomGrip
  else
    Result := rchtZoomScrollBarMaxZoomGrip;
end;

procedure TdxRangeControlZoomScrollBarZoomGripViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawRangeControlScaledSizingGlyph(ACanvas, FGlyphBounds, ViewInfo.GetElementBorderColor, ScaleFactor);
end;

{ TdxRangeControlZoomAndScrollBarViewportScrollThumbViewInfo }

function TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtZoomScrollBarViewportScrollThumb;
end;

procedure TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo.Paint(ACanvas: TcxCanvas);
var
  AViewPortPreviewColor: TColor;
begin
  if ViewInfo.Style.ZoomScrollBarVisibleRangeThumbColor <> clDefault then
    AViewPortPreviewColor := ViewInfo.Style.ZoomScrollBarVisibleRangeThumbColor
  else
    AViewPortPreviewColor := Painter.GetRangeControlViewPortPreviewColor;

  ACanvas.FillRect(Bounds, AViewPortPreviewColor);
end;

{ TdxRangeControlZoomAndScrollBarSelectionScrollThumbViewInfo }

function TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtZoomScrollBarSelectionScrollThumb;
end;

procedure TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo.Paint(
  ACanvas: TcxCanvas);
var
  ARangePreviewColor: TColor;
begin
  if ViewInfo.Style.ZoomScrollBarSelectedRangeThumbColor <> clDefault then
    ARangePreviewColor := ViewInfo.Style.ZoomScrollBarSelectedRangeThumbColor
  else
    ARangePreviewColor := Painter.GetRangeControlRangePreviewColor;
  ACanvas.FillRect(Bounds, ARangePreviewColor);
end;

function TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo.PtInElement(
  const APoint: TPoint): Boolean;
begin
  if cxRectIsEmpty(Bounds) then
    Result := cxRectPtIn(cxRectInflate(Bounds, ScaleFactor.Apply(2), 0), APoint)
  else
    Result := inherited PtInElement(APoint);
end;

{ TdxRangeControlZoomAndScrollBarViewInfo }

constructor TdxRangeControlZoomScrollBarViewInfo.Create(AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create(AViewInfo);
  FSelectionScrollThumb := TdxRangeControlZoomScrollBarSelectionScrollThumbViewInfo.Create(AViewInfo);
  FViewportScrollThumb := TdxRangeControlZoomScrollBarViewportScrollThumbViewInfo.Create(AViewInfo);
  FMinZoomGrip := TdxRangeControlZoomScrollBarZoomGripViewInfo.Create(AViewInfo, retMin);
  FMaxZoomGrip := TdxRangeControlZoomScrollBarZoomGripViewInfo.Create(AViewInfo, retMax);
end;

destructor TdxRangeControlZoomScrollBarViewInfo.Destroy;
begin
  FreeAndNil(FMaxZoomGrip);
  FreeAndNil(FMinZoomGrip);
  FreeAndNil(FViewportScrollThumb);
  FreeAndNil(FSelectionScrollThumb);
  inherited Destroy;
end;

procedure TdxRangeControlZoomScrollBarViewInfo.CalculateBounds(const ABounds: TRect);
var
  R: TRect;
  ASizingGlyphSize: TSize;
begin
  inherited CalculateBounds(ABounds);
  R := ABounds;
  R.Left := ViewInfo.FMinZoomScrollBarSelectedPos;
  R.Right := ViewInfo.FMaxZoomScrollBarSelectedPos;
  FSelectionScrollThumb.CalculateBounds(R);
  R.Left := ViewInfo.FMinZoomScrollBarVisiblePos;
  R.Right := ViewInfo.FMaxZoomScrollBarVisiblePos;
  FViewportScrollThumb.CalculateBounds(R);

  ASizingGlyphSize := Painter.GetRangeControlScaledSizingGlyphSize(ScaleFactor);
  R.Left := FViewportScrollThumb.Bounds.Left;
  R.Right := R.Left + ASizingGlyphSize.cx;
  FMinZoomGrip.CalculateBounds(R);
  R.Right := FViewportScrollThumb.Bounds.Right;
  R.Left := R.Right - ASizingGlyphSize.cx;
  FMaxZoomGrip.CalculateBounds(R);
end;

procedure TdxRangeControlZoomScrollBarViewInfo.Paint(ACanvas: TcxCanvas);
var
  AScrollAreaColor: TColor;
  ABorderColor: TdxAlphaColor;
  R: TRect;
  APenStyle: TPenStyle;
begin
  inherited;
  if ViewInfo.Style.ZoomScrollBarColor <> clDefault then
    AScrollAreaColor := ViewInfo.Style.ZoomScrollBarColor
  else
    AScrollAreaColor := Painter.GetRangeControlScrollAreaColor;
  ACanvas.FillRect(Bounds, AScrollAreaColor);
  FViewportScrollThumb.Paint(ACanvas);
  FMinZoomGrip.Paint(ACanvas);
  FMaxZoomGrip.Paint(ACanvas);
  FSelectionScrollThumb.Paint(ACanvas);

  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
  try
    ABorderColor := ViewInfo.GetElementBorderColor;
    R := FViewportScrollThumb.Bounds;
    if FSelectionScrollThumb.PtInElement(R.TopLeft) then
      APenStyle := psDot
    else
      APenStyle := psSolid;
    dxGPPaintCanvas.Line(R.Left, R.Top, R.Left, R.Bottom, ABorderColor, 1, APenStyle);

    if FSelectionScrollThumb.PtInElement(cxPointOffset(R.BottomRight, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1))) then
      APenStyle := psDot
    else
      APenStyle := psSolid;
    dxGPPaintCanvas.Line(R.Right, R.Top, R.Right, R.Bottom, ABorderColor, 1, APenStyle);

    R := FSelectionScrollThumb.Bounds;
    dxGPPaintCanvas.Line(R.Left, R.Top, R.Left, R.Bottom, ABorderColor);
    dxGPPaintCanvas.Line(R.Right, R.Top, R.Right, R.Bottom, ABorderColor);

    R := Bounds;
    dxGPPaintCanvas.Line(R.Left, R.Top, R.Right, R.Top, ABorderColor);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxRangeControlZoomScrollBarViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtZoomScrollBar;
end;

procedure TdxRangeControlZoomScrollBarViewInfo.SetHitTest(
  AHitTest: TdxCustomRangeControlHitTest);
begin
  inherited SetHitTest(AHitTest);
  if not FMinZoomGrip.GetHitTest(AHitTest) and
    not FMaxZoomGrip.GetHitTest(AHitTest) and
    not FSelectionScrollThumb.GetHitTest(AHitTest) then
    FViewportScrollThumb.GetHitTest(AHitTest);
end;

{ TdxRangeControlClientAreaSelectedRangeTargetViewInfo }

procedure TdxRangeControlClientAreaSelectedRangeTargetViewInfo.Paint(
  ACanvas: TcxCanvas);
var
  R, AContent: TRect;
  ABackgroundColor, ABorderColor: TdxAlphaColor;
begin
  R := Bounds;
  AContent := R;
  Inc(AContent.Left);
  Dec(R.Bottom);
  ABackgroundColor := ViewInfo.GetSelectedRegionBackgroundColor;
  ABorderColor := ViewInfo.GetSelectedRegionBorderColor;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    if cxRectIsEmpty(R) then
      dxGPPaintCanvas.Line(R.Left, R.Top, R.Right, R.Bottom, ABorderColor)
    else
    begin
      dxGPPaintCanvas.Line(R.Left, R.Top, R.Left, R.Bottom, ABorderColor);
      dxGPPaintCanvas.FillRectangle(AContent, ABackgroundColor);
      dxGPPaintCanvas.Line(R.Right, R.Top, R.Right, R.Bottom, ABorderColor);
    end;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

{ TdxRangeControlClientAreaSelectedRangeViewInfo }

function TdxRangeControlClientAreaSelectedRangeViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtClientAreaSelectedRange;
end;

procedure TdxRangeControlClientAreaSelectedRangeViewInfo.Paint(ACanvas: TcxCanvas);
var
  R: TRect;
  ABorderColor: TdxAlphaColor;
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
  try
    R := Bounds;
    ABorderColor := ViewInfo.GetElementBorderColor;
    dxGPPaintCanvas.Line(R.Left, R.Top, R.Left, R.Bottom, ABorderColor);
    dxGPPaintCanvas.Line(R.Right, R.Top, R.Right, R.Bottom, ABorderColor);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

{ TdxRangeControlClientAreaSelectionThumbViewInfo }

constructor TdxRangeControlClientAreaSelectionThumbViewInfo.Create(
  AViewInfo: TdxCustomRangeControlViewInfo; AType: TdxRangeElementType);
begin
  inherited Create(AViewInfo);
  FType := AType;
end;

function TdxRangeControlClientAreaSelectionThumbViewInfo.GetHitTestIndex: Integer;
begin
  if FType = retMin then
    Result := rchtClientAreaMinSelectionThumb
  else
    Result := rchtClientAreaMaxSelectionThumb;
end;

procedure TdxRangeControlClientAreaSelectionThumbViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if FType = retMin then
    Painter.DrawRangeControlScaledLeftThumb(ACanvas, Bounds,
      ViewInfo.GetSelectedRangeThumbColor, ViewInfo.GetElementBorderColor, ScaleFactor)
  else
    Painter.DrawRangeControlScaledRightThumb(ACanvas, Bounds,
      ViewInfo.GetSelectedRangeThumbColor, ViewInfo.GetElementBorderColor, ScaleFactor);
end;

{ TdxRangeControlClientAreaViewInfo }

constructor TdxRangeControlClientAreaViewInfo.Create(
  AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create(AViewInfo);
  FSelectedRange := TdxRangeControlClientAreaSelectedRangeViewInfo.Create(AViewInfo);
  FMinSelectionThumb := TdxRangeControlClientAreaSelectionThumbViewInfo.Create(AViewInfo, retMin);
  FMaxSelectionThumb := TdxRangeControlClientAreaSelectionThumbViewInfo.Create(AViewInfo, retMax);
  FSelectedRangeTarget := TdxRangeControlClientAreaSelectedRangeTargetViewInfo.Create(AViewInfo);
end;

destructor TdxRangeControlClientAreaViewInfo.Destroy;
begin
  FreeAndNil(FSelectedRangeTarget);
  FreeAndNil(FMaxSelectionThumb);
  FreeAndNil(FMinSelectionThumb);
  FreeAndNil(FSelectedRange);
  inherited;
end;

procedure TdxRangeControlClientAreaViewInfo.CalculateBounds(
  const ABounds: TRect);
var
  R, R1: TRect;
  ASelectionThumbSize: TSize;
begin
  inherited CalculateBounds(ABounds);
  R := Bounds;
  R.Left := ViewInfo.FMinClientSelectedPos;
  R.Right := ViewInfo.FMaxClientSelectedPos;
  FSelectedRange.CalculateBounds(R);

  ASelectionThumbSize := Painter.GetRangeControlScaledThumbSize(ScaleFactor);
  R1 := cxRectCenterVertically(Bounds, ASelectionThumbSize.cy);
  R1.Left := R.Left - cxHalfCoordinate(ASelectionThumbSize.cx);
  R1.Right := R1.Left + ASelectionThumbSize.cx;
  FMinSelectionThumb.CalculateBounds(R1);
  R1.Left := R.Right - cxHalfCoordinate(ASelectionThumbSize.cx);
  R1.Right := R1.Left + ASelectionThumbSize.cx;
  FMaxSelectionThumb.CalculateBounds(R1);

  R.Left := ViewInfo.FMinClientSelectedTargetPos;
  R.Right := ViewInfo.FMaxClientSelectedTargetPos;
  FSelectedRangeTarget.CalculateBounds(R);
end;

function TdxRangeControlClientAreaViewInfo.GetHitTestIndex: Integer;
begin
  Result := rchtClientArea;
end;

procedure TdxRangeControlClientAreaViewInfo.Paint(ACanvas: TcxCanvas);
var
  R: TRect;
  AOutOfRangeColor: TdxAlphaColor;
begin
  inherited;
  FSelectedRange.Paint(ACanvas);
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
  try
    if ViewInfo.Style.OutOfRangeColor <> dxacDefault then
      AOutOfRangeColor := ViewInfo.Style.OutOfRangeColor
    else
      AOutOfRangeColor := Painter.GetRangeControlOutOfRangeColor;
    R := Rect(Bounds.Left + 1, Bounds.Top,
      FSelectedRange.Bounds.Left, Bounds.Bottom);
    if R.Width > 0 then
    begin
      cxRectIntersect(R, R, Bounds);
      dxGPPaintCanvas.FillRectangle(R, AOutOfRangeColor);
    end;
    R := Rect(FSelectedRange.Bounds.Right + 1, Bounds.Top,
      Bounds.Right - 1, Bounds.Bottom);
    if R.Width > 0 then
      dxGPPaintCanvas.FillRectangle(R, AOutOfRangeColor);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
  if ViewInfo.PressedInfo = Self then
    FSelectedRangeTarget.Paint(ACanvas);
  FMinSelectionThumb.Paint(ACanvas);
  FMaxSelectionThumb.Paint(ACanvas);
end;

procedure TdxRangeControlClientAreaViewInfo.SetHitTest(
  AHitTest: TdxCustomRangeControlHitTest);
begin
  inherited SetHitTest(AHitTest);
  if not FMinSelectionThumb.GetHitTest(AHitTest) and
    not FMaxSelectionThumb.GetHitTest(AHitTest) then
    FSelectedRange.GetHitTest(AHitTest);
end;

function TdxRangeControlBaseStyle.GetRangeControl: TdxCustomRangeControl;
begin
  Result := Owner as TdxCustomRangeControl;
end;

{ TdxRangeControlCustomClientStyle }

procedure TdxRangeControlCustomClientStyle.Assign(Source: TPersistent);
begin
  Properties.BeginUpdate;
  try
    inherited Assign(Source);
  finally
    Properties.EndUpdate;
  end;
end;

procedure TdxRangeControlCustomClientStyle.Changed;
begin
  Properties.Changed;
end;

procedure TdxRangeControlCustomClientStyle.ChangeScale(M, D: Integer);
begin
//do nothing
end;

function TdxRangeControlCustomClientStyle.GetProperties: TdxRangeControlCustomClientProperties;
begin
  Result := Owner as TdxRangeControlCustomClientProperties;
end;

{ TdxRangeControlCustomClientViewInfo }

constructor TdxRangeControlCustomClientViewInfo.Create(AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create(AViewInfo);
  FContent := CreateContentInfo;
  FRuler := CreateRulerInfo;
end;

destructor TdxRangeControlCustomClientViewInfo.Destroy;
begin
  FreeAndNil(FRuler);
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TdxRangeControlCustomClientViewInfo.CalculateBounds(const ABounds: TRect);
var
  ARect: TRect;
begin
  inherited CalculateBounds(ABounds);
  ARect := ABounds;
  if ViewInfo.RangeControl.ShowRuler then
    ARect.Top := ARect.Bottom - GetScaleHeight
  else
    ARect.Top := ARect.Bottom;
  Ruler.CalculateBounds(ARect);
  ARect := ABounds;
  ARect.Bottom := Ruler.Bounds.Top;
  Content.CalculateBounds(ARect);
end;

function TdxRangeControlCustomClientViewInfo.CreateContentInfo: TdxRangeControlContentViewInfo;
begin
  Result := TdxRangeControlContentViewInfo.Create(ViewInfo);
end;

function TdxRangeControlCustomClientViewInfo.CreateRulerInfo: TdxRangeControlRulerViewInfo;
begin
  Result := nil;
end;

procedure TdxRangeControlCustomClientViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  Content.Paint(ACanvas);
end;

procedure TdxRangeControlCustomClientViewInfo.DrawGridLines(ACanvas: TcxCanvas);
var
  P1: TPoint;
  P2: TPoint;
  ATickMarkPos: Integer;
  AValue: Double;
  AIsOverflow: Boolean;
  AGridLineColor: TdxAlphaColor;
begin
  P1.Y := Content.Bounds.Top;
  P2.Y := Content.Bounds.Bottom;
  AValue := NormalizedFirstTickMarkValue;
  AGridLineColor := GetGridLineColor;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Content.Bounds);
  try
    AIsOverflow := False;
    while (AValue <= Control.NormalizedVisibleRangeMaxValue) and not AIsOverflow do
    begin
      ATickMarkPos := ViewInfo.GetPositionInClientAreaFromNormalValue(AValue);
      P1.X := ATickMarkPos;
      P2.X := P1.X;
      dxGPPaintCanvas.Line(P1.X, P1.Y - 1, P2.X, P2.Y - 1, AGridLineColor);
      AValue := Properties.GetNextNormalizedAcceptableValue(AValue, ScaleInterval, AIsOverflow);
    end;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxRangeControlCustomClientViewInfo.DrawScale(ACanvas: TcxCanvas);
begin
  Ruler.Paint(ACanvas);
end;

function TdxRangeControlCustomClientViewInfo.GetGridLineColor: TdxAlphaColor;
begin
  Result := Painter.GetRangeControlRulerColor;
end;

function TdxRangeControlCustomClientViewInfo.GetProperties: TdxRangeControlCustomClientProperties;
begin
  Result := Control.ClientProperties as TdxRangeControlCustomClientProperties;
end;

function TdxRangeControlCustomClientViewInfo.GetScaleHeight: Integer;
begin
  Result := cxTextHeight(ViewInfo.GetRulerLabelFont) + ScaleFactor.Apply(7);
end;

procedure TdxRangeControlCustomClientViewInfo.SetScaleInterval(AValue: Variant);
begin
  if AValue <= 0 then
    AValue := 1;
  FScaleInterval := AValue;
end;

{ TdxCustomRangeControlClient }

constructor TdxRangeControlCustomClientProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FStyle := CreateStyle;
  FScaleInterval := VarAsType(1, varInteger);
  FScaleIntervalMinWidth := 5;
end;

destructor TdxRangeControlCustomClientProperties.Destroy;
begin
  FreeAndNil(FStyle);
  inherited;
end;

procedure TdxRangeControlCustomClientProperties.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxRangeControlCustomClientProperties.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxRangeControlCustomClientProperties.Changed;
begin
  if FLockCount = 0 then
    RangeControl.ClientPropertiesChanged;
end;

procedure TdxRangeControlCustomClientProperties.ChangeScale(M, D: Integer);
begin
  Style.ChangeScale(M, D);
end;

procedure TdxRangeControlCustomClientProperties.CheckMaxValue;
begin
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
end;

procedure TdxRangeControlCustomClientProperties.CheckMinPossibleRangeInterval(var AValue: Double);
begin
end;

procedure TdxRangeControlCustomClientProperties.CheckMinValue;
begin
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
end;

function TdxRangeControlCustomClientProperties.CreateStyle: TdxRangeControlCustomClientStyle;
begin
  Result := nil;
end;

function TdxRangeControlCustomClientProperties.CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo):
    TdxRangeControlCustomClientViewInfo;
begin
  Result := TdxRangeControlCustomClientViewInfo.Create(AViewInfo);
end;

procedure TdxRangeControlCustomClientProperties.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlCustomClientProperties;
begin
  inherited DoAssign(Source);
  if Source is TdxRangeControlCustomClientProperties then
  begin
    ASource := TdxRangeControlCustomClientProperties(Source);
    MaxValue := ASource.MaxValue;
    ScaleIntervalMinWidth := ASource.ScaleIntervalMinWidth;
    MinValue := ASource.MinValue;
    ScaleInterval := ASource.ScaleInterval;
  end;
end;

function TdxRangeControlCustomClientProperties.DoGetNextNormalizedAcceptableValue(const AValue: Double; AInterval: Variant; out AIsOverflow: Boolean): Double;
var
  AOriginal: Variant;
  ANextValue: Variant;
begin
  AOriginal := GetOriginalValue(AValue);
  ANextValue := GetNextValue(AOriginal, AInterval, AIsOverflow);
  if AIsOverflow and (AInterval > 0) then
    Result := 1
  else
    if AIsOverflow and (AInterval < 0) then
      Result := 0
    else
      Result := GetNormalizedValue(ANextValue);
end;

function TdxRangeControlCustomClientProperties.DoGetNextValue(const AOriginal, AInterval: Variant): Variant;
begin
  Result := AOriginal + AInterval;
end;

function TdxRangeControlCustomClientProperties.DoGetNormalizedValue(const AValue: Variant): Double;
var
  ARangeDelta: Variant;
begin
  ARangeDelta := GetRangeDelta;
  if (ARangeDelta = 0) or (AValue <= FMinValue) then
    Result := 0
  else
    if AValue >= FMaxValue then
      Result := 1
    else
      Result := (AValue - FMinValue)/ARangeDelta;
end;

procedure TdxRangeControlCustomClientProperties.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

function TdxRangeControlCustomClientProperties.GetInterval(const AMin, AMax: Double): Variant;
begin
  Result := 0;
end;

function TdxRangeControlCustomClientProperties.GetLabelText(const Value: Variant): string;
begin
  Result := '';
end;

function TdxRangeControlCustomClientProperties.GetMaxValue: Variant;
begin
  Result := VarAsType(FMaxValue, GetRangeValueType);
end;

function TdxRangeControlCustomClientProperties.GetMinSelectedInterval: Variant;
begin
  Result := 0;
end;

function TdxRangeControlCustomClientProperties.GetMinValue: Variant;
begin
  Result := VarAsType(FMinValue, GetRangeValueType);
end;

function TdxRangeControlCustomClientProperties.GetRangeControl: TdxCustomRangeControl;
begin
  Result := Owner as TdxCustomRangeControl;
end;

function TdxRangeControlCustomClientProperties.GetNearestNormalizedAcceptableValue(const AValue: Double): Double;
begin
  Result := AValue;
end;

function TdxRangeControlCustomClientProperties.GetNextNormalizedAcceptableValue(const AValue: Double; const AInterval: Variant; out AIsOverflow: Boolean): Double;
begin
  AIsOverflow := False;
  if AInterval = 0 then
    Result := GetNearestNormalizedAcceptableValue(AValue)
  else
    Result := DoGetNextNormalizedAcceptableValue(AValue, AInterval, AIsOverflow);
end;

function TdxRangeControlCustomClientProperties.GetNextValue(const AOriginal, AInterval: Variant; out AIsOverflow: Boolean): Variant;
begin
  Result := DoGetNextValue(AOriginal, AInterval);
  AIsOverflow := (MaxValue < Result) or (Result < MinValue);
end;

function TdxRangeControlCustomClientProperties.GetNormalizedValue(const AValue: Variant): Double;
begin
  Result := EnsureRange(DoGetNormalizedValue(AValue), 0, 1);
end;

function TdxRangeControlCustomClientProperties.GetNormalizedVisibleRangeMaxInterval: Double;
begin
  Result := 1;
end;

function TdxRangeControlCustomClientProperties.GetOriginalValue(const AValue: Double): Variant;
begin
  Result := GetOriginalValue(AValue, GetRangeValueType);
end;

function TdxRangeControlCustomClientProperties.GetOriginalValue(const AValue: Double; AType: Word): Variant;
begin
  Result := VarAsType(FMinValue + AValue * GetRangeDelta, AType);
end;

function TdxRangeControlCustomClientProperties.GetRangeDelta: Variant;
begin
  Result := FMaxValue - FMinValue;
end;

function TdxRangeControlCustomClientProperties.GetRangeValueType: Word;
begin
  Result := varUnknown;
end;

function TdxRangeControlCustomClientProperties.GetScaleInterval: Variant;
begin
  Result := VarAsType(FScaleInterval, GetScaleIntervalValueType);
end;

function TdxRangeControlCustomClientProperties.GetScaleIntervalValueType: Word;
begin
  Result := varInteger;
end;

function TdxRangeControlCustomClientProperties.GetVisibleRangeValueType: Word;
begin
  Result := varDouble;
end;

function TdxRangeControlCustomClientProperties.IsAutoZoomSupported: Boolean;
begin
  Result := False;
end;

function TdxRangeControlCustomClientProperties.IsRangeEmpty: Boolean;
begin
  Result := GetRangeDelta = 0;
end;

procedure TdxRangeControlCustomClientProperties.ScaleIntervalMinWidthChanged;
begin
  Changed;
end;

procedure TdxRangeControlCustomClientProperties.SetMaxValue(Value: Variant);
var
  ASelectedRangeMinValue, ASelectedRangeMaxValue: Variant;
begin
  try
    if not VarIsType(Value, GetRangeValueType) then
      VarCast(Value, Value, GetRangeValueType);
    if FMaxValue <> Value then
    begin
      BeginUpdate;
      try
        ASelectedRangeMinValue := RangeControl.SelectedRangeMinValue;
        ASelectedRangeMaxValue := RangeControl.SelectedRangeMaxValue;
        FMaxValue := Value;
        CheckMinValue;
        RangeControl.LockClientNotification;
        try
          RangeControl.VisibleRangeMaxScaleFactor := RangeControl.VisibleRangeMaxScaleFactor;
          RangeControl.SelectedRangeMinValue := ASelectedRangeMinValue;
          RangeControl.SelectedRangeMaxValue := ASelectedRangeMaxValue;
        finally
          RangeControl.UnlockClientNotification;
        end;
      finally
        EndUpdate;
      end;
    end;
  except
    on EVariantError do ;
    else
      raise;
  end;
end;

procedure TdxRangeControlCustomClientProperties.SetScaleIntervalMinWidth(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FScaleIntervalMinWidth <> AValue then
  begin
    FScaleIntervalMinWidth := AValue;
    ScaleIntervalMinWidthChanged;
  end;
end;

procedure TdxRangeControlCustomClientProperties.SetMinValue(Value: Variant);
var
  ASelectedRangeMinValue, ASelectedRangeMaxValue: Variant;
begin
  try
    if not VarIsType(Value, GetRangeValueType) then
      VarCast(Value, Value, GetRangeValueType);
    if FMinValue <> Value then
    begin
      BeginUpdate;
      try
        ASelectedRangeMinValue := RangeControl.SelectedRangeMinValue;
        ASelectedRangeMaxValue := RangeControl.SelectedRangeMaxValue;
        FMinValue := Value;
        CheckMaxValue;
        RangeControl.LockClientNotification;
        try
          RangeControl.VisibleRangeMaxScaleFactor := RangeControl.VisibleRangeMaxScaleFactor;
          RangeControl.SelectedRangeMinValue := ASelectedRangeMinValue;
          RangeControl.SelectedRangeMaxValue := ASelectedRangeMaxValue;
        finally
          RangeControl.UnlockClientNotification;
        end;
      finally
        EndUpdate;
      end;
    end;
  except
    on EVariantError do ;
    else
      raise;
  end;
end;

procedure TdxRangeControlCustomClientProperties.SetScaleInterval(Value: Variant);
begin
  Value := VarAsType(Value, GetScaleIntervalValueType);
  if Value <= 0 then
    Value := 1;
  if FScaleInterval <> Value then
  begin
    FScaleInterval := Value;
    Changed;
  end;
end;

procedure TdxRangeControlCustomClientProperties.SetStyle(Value: TdxRangeControlCustomClientStyle);
begin
  FStyle.Assign(Value);
end;

{ TdxRangeControlSimpleRulerViewInfo }

procedure TdxRangeControlSimpleRulerViewInfo.Paint(ACanvas: TcxCanvas);

  procedure DrawLabel(ACanvas: TcxCanvas; const AText: string; const ARect: TRect);
  var
    ATextColor: TColor;
    AStyle: TdxRangeControlClientStyle;
  begin
    AStyle := Properties.Style as TdxRangeControlClientStyle;
    ATextColor := AStyle.RulerTextColor;
    if ATextColor = clDefault then
      ATextColor := Painter.GetRangeControlLabelColor;
    ACanvas.Font.Color := ATextColor;
    ACanvas.DrawTexT(AText, ARect, cxShowEndEllipsis or cxAlignCenter);
  end;

var
  AText: string;
  ARect: TRect;
  ATickMarkPos: Integer;
  APrevRect: TRect;
  AValue: Double;
  AIsOverflow: Boolean;
  ARulerColor: TdxAlphaColor;
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
  try
    ARulerColor := (Properties.Style as TdxRangeControlClientStyle).RulerColor;
    if ARulerColor <> dxacDefault then
      dxGPPaintCanvas.FillRectangle(Bounds, ARulerColor);
    dxGPPaintCanvas.Line(Bounds.Left, Bounds.Top,
      Bounds.Right, Bounds.Top, ViewInfo.GetElementBorderColor);
  finally
    dxGPPaintCanvas.EndPaint;
  end;

  AValue := ViewInfo.ClientInfo.NormalizedFirstTickMarkValue;
  APrevRect := Bounds;
  APrevRect.Right := APrevRect.Left;
  AIsOverflow := False;
  while AValue <= Control.NormalizedVisibleRangeMaxValue do
  begin
    ATickMarkPos := ViewInfo.GetPositionInClientAreaFromNormalValue(AValue);
    AText := Properties.GetLabelText(Properties.GetOriginalValue(AValue));
    ACanvas.Font := ViewInfo.GetRulerLabelFont;
    CalculateLabelPosition(ACanvas, AText, Round(ATickMarkPos), ARect);
    if not cxRectIntersect(APrevRect, ARect) then
    begin
      DrawLabel(ACanvas, AText, ARect);
      APrevRect := ARect;
    end;
    if AIsOverflow then
      Break;
    AValue := Properties.GetNextNormalizedAcceptableValue(AValue, ViewInfo.ClientInfo.ScaleInterval, AIsOverflow);
  end;
end;

function TdxRangeControlSimpleRulerViewInfo.GetProperties: TdxRangeControlCustomClientProperties;
begin
  Result := ViewInfo.ClientInfo.Properties;
end;

procedure TdxRangeControlSimpleRulerViewInfo.CalculateLabelPosition(ACanvas: TcxCanvas;
  const AText: string; ATickMarkPos: Integer; out ARect: TRect);
var
  ATextWidth, ATextWidthHalf: Integer;
  ASize: TSize;
begin
  ASize := ACanvas.TextExtent(AText);
  Inc(ASize.cx, ScaleFactor.Apply(cxTextOffset) * 2);
  Inc(ASize.cy, ScaleFactor.Apply(cxTextOffset) * 2);
  ARect := cxRectCenterVertically(Bounds, ASize.cy);
  ATextWidth := ASize.cx;
  ATextWidthHalf := cxHalfCoordinate(ATextWidth);
  if ATickMarkPos - ATextWidthHalf < Bounds.Left then
  begin
    ARect.Left := Bounds.Left;
    ARect.Right := ARect.Left + ATextWidth;
  end
  else
    if ATickMarkPos + ATextWidthHalf > Bounds.Right then
    begin
      ARect.Right := Bounds.Right;
      ARect.Left := ARect.Right - ATextWidth;
    end
    else
    begin
      ARect.Left := ATickMarkPos - ATextWidthHalf;
      ARect.Right := ARect.Left + ATextWidth;
    end;
end;

{ TdxRangeControlNumericClientViewInfo }

procedure TdxRangeControlNumericClientViewInfo.CalculateBounds(const ABounds: TRect);
var
  ATickMarkDistance: Double;
  ANormalScaleInterval: Double;
  AScaleFactor: Double;
  AIsOverflow: Boolean;
begin
  inherited CalculateBounds(ABounds);
  ScaleInterval := Properties.ScaleInterval;
  AScaleFactor := ViewInfo.ClientAreaNormalFactor;
  ANormalScaleInterval := Properties.GetNextNormalizedAcceptableValue(0, ScaleInterval, AIsOverflow);
  ATickMarkDistance := ANormalScaleInterval * AScaleFactor;
  if ATickMarkDistance < Properties.ScaleIntervalMinWidth then
  begin
    ANormalScaleInterval := Properties.ScaleIntervalMinWidth / AScaleFactor;
    ScaleInterval := VarAsType(Properties.GetOriginalValue(ANormalScaleInterval) - Properties.MinValue, Properties.GetScaleIntervalValueType);
  end;
  NormalizedFirstTickMarkValue := Ceil(Control.NormalizedVisibleRangeMinValue / ANormalScaleInterval) * ANormalScaleInterval;
end;

function TdxRangeControlNumericClientViewInfo.CreateRulerInfo: TdxRangeControlRulerViewInfo;
begin
  Result := TdxRangeControlSimpleRulerViewInfo.Create(ViewInfo);
end;

procedure TdxRangeControlNumericClientViewInfo.DrawScale(ACanvas: TcxCanvas);
begin
  DrawGridLines(ACanvas);
  inherited DrawScale(ACanvas);
end;

function TdxRangeControlNumericClientViewInfo.GetGridLineColor: TdxAlphaColor;
var
  AColor: TdxAlphaColor;
begin
  AColor := (Properties.Style as TdxRangeControlClientStyle).GridLineColor;
  if AColor <> dxacDefault then
    Result := AColor
  else
    Result := inherited GetGridLineColor;
end;

function TdxRangeControlNumericClientViewInfo.GetProperties: TdxRangeControlCustomNumericClientProperties;
begin
  Result := Control.ClientProperties as TdxRangeControlCustomNumericClientProperties;
end;

{ TdxRangeControlClientStyle }

constructor TdxRangeControlClientStyle.Create(AOwner: TPersistent);
begin
  inherited;
  FRulerColor := dxacDefault;
  FRulerTextColor := clDefault;
  FGridLineColor := dxacDefault;
end;

procedure TdxRangeControlClientStyle.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlClientStyle;
begin
  inherited;
  if Source is TdxRangeControlClientStyle then
  begin
    ASource := TdxRangeControlClientStyle(Source);
    RulerColor := ASource.RulerColor;
    RulerTextColor := ASource.RulerTextColor;
    GridLineColor := ASource.GridLineColor;
  end;
end;

procedure TdxRangeControlClientStyle.SetGridLineColor(Value: TdxAlphaColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    Changed;
  end;
end;

procedure TdxRangeControlClientStyle.SetRulerColor(Value: TdxAlphaColor);
begin
  if FRulerColor <> Value then
  begin
    FRulerColor := Value;
    Changed;
  end;
end;

procedure TdxRangeControlClientStyle.SetRulerTextColor(Value: TColor);
begin
  if FRulerTextColor <> Value then
  begin
    FRulerTextColor := Value;
    Changed;
  end;
end;

{ TdxRangeControlCustomNumericClientProperties }

constructor TdxRangeControlCustomNumericClientProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDisplayFormat := GetDefaultDisplayFormat;
  FRangeValueType := varInteger;
  FMinValue := VarAsType(0, varInteger);
  FMaxValue := VarAsType(10, varInteger);
end;

procedure TdxRangeControlCustomNumericClientProperties.CheckMaxValue;
begin
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue
  else
    if GetMaxPossibleRangeInterval > 0 then
      if (FMaxValue > 0) and
        ((FMaxValue - GetMaxPossibleRangeInterval) > FMinValue) or
        (FMaxValue <= 0) and
         ((FMinValue + GetMaxPossibleRangeInterval) < FMaxValue) then
        FMaxValue := FMinValue + GetMaxPossibleRangeInterval;
end;

procedure TdxRangeControlCustomNumericClientProperties.CheckMinPossibleRangeInterval(var AValue: Double);
var
  ARange: Double;
  AMinPossibleRangeInterval: Double;
begin
  ARange := GetRangeDelta;
  AMinPossibleRangeInterval := GetMinPossibleRangeInterval(GetRangeValueType);
  if (ARange >= AMinPossibleRangeInterval) and (ARange <= AMinPossibleRangeInterval * MaxDouble) then
    AValue := Min(AValue, ARange / AMinPossibleRangeInterval);
end;

procedure TdxRangeControlCustomNumericClientProperties.CheckMinValue;
begin
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue
  else
    if GetMaxPossibleRangeInterval > 0 then
      if (FMaxValue > 0) and
        ((FMaxValue - GetMaxPossibleRangeInterval) > FMinValue) or
        (FMaxValue <= 0) and
         ((FMinValue + GetMaxPossibleRangeInterval) < FMaxValue) then
          FMinValue := FMaxValue - GetMaxPossibleRangeInterval;
end;

function TdxRangeControlCustomNumericClientProperties.Convert(const AValue: Variant; AType: Word): Variant;

  function GetMaxPossibleValue(AType: Word): Variant;
  begin
    case AType of
      varDouble:
        Result := MaxDouble;
      varCurrency:
        Result := MaxCurrency;
      varInt64, varUInt64:
        Result := MaxInt64;
      varInteger:
        Result := MaxInt
    else
      Result := 0;
    end;
  end;

  function GetMinPossibleValue(AType: Word): Variant;
  begin
    case AType of
      varDouble:
        Result := -MaxDouble;
      varCurrency:
        Result := -MaxCurrency;
      varInt64, varUInt64:
        Result := MinInt64;
      varInteger:
        Result := MinInt;
    else
      Result := 0;
    end;
  end;

var
  AMaxPossibleValue, AMinPossibleValue: Variant;
begin
  AMaxPossibleValue := GetMaxPossibleValue(AType);
  AMinPossibleValue := GetMinPossibleValue(AType);
  if (AMaxPossibleValue <> 0) and (AValue >= AMaxPossibleValue) then
    Result := VarAsType(AMaxPossibleValue, AType)
  else
    if (AMinPossibleValue <> 0) and (AValue <= AMinPossibleValue) then
      Result := VarAsType(AMinPossibleValue, AType)
    else
      Result := VarAsType(AValue, AType);
end;

function TdxRangeControlCustomNumericClientProperties.CreateStyle: TdxRangeControlCustomClientStyle;
begin
  Result := TdxRangeControlClientStyle.Create(Self);
end;

function TdxRangeControlCustomNumericClientProperties.IsDisplayFormatStored: Boolean;
begin
  Result := not SameText(FDisplayFormat, GetDefaultDisplayFormat);
end;

function TdxRangeControlCustomNumericClientProperties.CreateViewInfo(
  AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo;
begin
  Result := TdxRangeControlNumericClientViewInfo.Create(AViewInfo);
end;

function TdxRangeControlCustomNumericClientProperties.GetInterval(const AMin, AMax: Double): Variant;
begin
  Result := GetOriginalValue(AMax) - GetOriginalValue(AMin);
end;

function TdxRangeControlCustomNumericClientProperties.GetNearestNormalizedAcceptableValue(const AValue: Double): Double;
begin
  Result := GetNormalizedValue(GetOriginalValue(AValue));
end;

procedure TdxRangeControlCustomNumericClientProperties.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlCustomNumericClientProperties;
begin
  inherited DoAssign(Source);
  if Source is TdxRangeControlCustomNumericClientProperties then
  begin
    ASource := TdxRangeControlCustomNumericClientProperties(Source);
    DisplayFormat := ASource.DisplayFormat;
    RangeValueType := ASource.RangeValueType;
  end;
end;

function TdxRangeControlCustomNumericClientProperties.GetDefaultDisplayFormat: string;
begin
  Result := GetLabelPattern;
end;

function TdxRangeControlCustomNumericClientProperties.GetNextValue(const AOriginal, AInterval: Variant; out AIsOverflow: Boolean): Variant;

  function IsUpOverflow(AValue, AIncrement: Variant): Boolean;
  begin
    Result := (GetMaxPossibleRangeInterval <> 0) and
      ((AIncrement > 0) and (AValue >= (GetMaxPossibleRangeInterval - AIncrement)));
  end;

  function IsDownOverflow(AValue, AIncrement: Variant): Boolean;
  begin
    Result := (GetMaxPossibleRangeInterval <> 0) and
      (((AIncrement < 0) and (AValue <= (-GetMaxPossibleRangeInterval - AIncrement))));
  end;

begin
  if IsUpOverflow(AOriginal, AInterval) then
  begin
    Result := MaxValue;
    AIsOverflow := True;
  end
  else
    if IsDownOverflow(AOriginal, AInterval) then
    begin
      Result := MinValue;
      AIsOverflow := True;
    end
    else
      Result := inherited GetNextValue(AOriginal, AInterval, AIsOverflow);
end;

function TdxRangeControlCustomNumericClientProperties.GetLabelPattern: string;
begin
  Result := '{Value}';
end;

function TdxRangeControlCustomNumericClientProperties.GetLabelText(const Value: Variant): string;

  function IsDefaultFormat: Boolean;
  begin
    Result := (FDisplayFormat = '') or SameText(FDisplayFormat, GetDefaultDisplayFormat);
  end;

begin
  Result := VarToStr(Value);
  if not IsDefaultFormat then
    Result := StringReplace(FDisplayFormat, GetLabelPattern, Result, [rfReplaceAll]);
end;

function TdxRangeControlCustomNumericClientProperties.GetMaxPossibleRangeInterval: Variant;
begin
  Result := GetMaxPossibleRangeInterval(GetRangeValueType);
end;

function TdxRangeControlCustomNumericClientProperties.GetMaxPossibleRangeInterval(AType: Word): Variant;
begin
  case AType of
    varDouble:
      Result := MaxDouble;
    varCurrency:
      Result := MaxCurrency;
    varInt64, varUInt64:
      Result := MaxInt64;
  else
    Result := 0;
  end;
end;

function TdxRangeControlCustomNumericClientProperties.GetMinPossibleRangeInterval(AType: Word): Variant;
begin
  case AType of
    varDouble:
      Result := MinDouble;
    varSingle:
      Result := MinSingle;
    varCurrency:
      Result := 0.0001;
  else // varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64
    Result := 1;
  end;
end;

function TdxRangeControlCustomNumericClientProperties.GetOriginalValue(const AValue: Double; AType: Word): Variant;
var
  AOriginalDelta: Variant;
begin
  if AType = varInt64 then
  begin
    AOriginalDelta := Convert(AValue * GetRangeDelta, AType);
    Result := MinValue + AOriginalDelta;
    if Result < MinValue then
      Result := MinValue
    else
      if Result > MaxValue then
        Result := MaxValue;
  end
  else
    Result := Convert(MinValue + AValue * GetRangeDelta, AType);
end;

function TdxRangeControlCustomNumericClientProperties.GetRangeValueType: Word;
begin
  Result := FRangeValueType;
end;

function TdxRangeControlCustomNumericClientProperties.GetScaleIntervalValueType: Word;
begin
  Result := FRangeValueType;
end;

function TdxRangeControlCustomNumericClientProperties.GetVisibleRangeValueType: Word;
begin
  if FRangeValueType in [varSmallInt, varInteger, varShortInt, varByte, varWord,
    varLongWord, varInt64, varDouble] then
    Result := varDouble
  else
    Result := FRangeValueType;
end;

function TdxRangeControlCustomNumericClientProperties.IsRangeValueTypeSupported(AValue: Word): Boolean;
begin
  Result := AValue in [varDouble, varSingle, varCurrency,
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64];
end;

procedure TdxRangeControlCustomNumericClientProperties.RangeValueTypeChanged;
begin
  BeginUpdate;
  try
    MinValue := MinValue;
    MaxValue := MaxValue;
    ScaleInterval := ScaleInterval;
    RangeControl.SelectedRangeMinValue := RangeControl.SelectedRangeMinValue;
    RangeControl.SelectedRangeMaxValue := RangeControl.SelectedRangeMaxValue;
  finally
    EndUpdate;
  end;
end;

procedure TdxRangeControlCustomNumericClientProperties.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Changed;
  end;
end;

procedure TdxRangeControlCustomNumericClientProperties.SetRangeValueType(Value: Word);
begin
  if IsRangeValueTypeSupported(Value) then
  begin
    FRangeValueType := Value;
    RangeValueTypeChanged;
  end;
end;

{ TdxRangeControlDateTimeClientHelper }

class function TdxRangeControlDateTimeClientHelper.CeilDate(const ADate: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
begin
  Result := TruncDate(ADate, AScaleUnit);
  if Result <> ADate then
    Result := IncDate(Result, AScaleUnit);
end;

class function TdxRangeControlDateTimeClientHelper.DayOfTheQuarter(const ADate: TDateTime): Integer;
var
  AStart: TDateTime;
begin
  AStart := StartOfTheQuarter(ADate);
  Result := DaysBetween(ADate, AStart);
end;

class function TdxRangeControlDateTimeClientHelper.DaysInQuarter(const ADate: TDateTime): Double;
var
  AStart, AEnd: TDateTime;
begin
  AStart := StartOfTheQuarter(ADate);
  AEnd := IncQuarter(AStart);
  Result := DaySpan(AEnd, AStart);
end;

class function TdxRangeControlDateTimeClientHelper.GetDateInterval(const AMin, AMax: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): Double;
begin
  case AScaleUnit of
    rcduSecond:
      Result := SecondSpan(AMin, AMax);
    rcduMinute:
      Result := MinuteSpan(AMin, AMax);
    rcduHour:
      Result := HourSpan(AMin, AMax);
    rcduDay:
      Result := DaySpan(AMin, AMax);
    rcduWeek:
      Result := WeekSpan(AMin, AMax);
    rcduMonth:
      Result := MonthSpan(AMin, AMax);
    rcduQuarter:
      Result := QuarterSpan(AMin, AMax);
  else //rcduYear
    Result := YearSpan(AMin, AMax);
  end;
end;

class function TdxRangeControlDateTimeClientHelper.IncQuarter(const AValue: TDateTime; ANumberOfQuarters: Integer = 1): TDateTime;
begin
  Result := IncMonth(AValue, 3 * ANumberOfQuarters);
end;

class function TdxRangeControlDateTimeClientHelper.IncDate(const AValue: Double; AScaleUnit: TdxRangeControlDateTimeScaleUnit; AIncrementValue: Int64 = 1): Double;
begin
  case AScaleUnit of
    rcduSecond:
      Result := IncSecond(AValue, AIncrementValue);
    rcduMinute:
      Result := IncMinute(AValue, AIncrementValue);
    rcduHour:
      Result := IncHour(AValue, AIncrementValue);
    rcduDay:
      Result := IncDay(AValue, AIncrementValue);
    rcduWeek:
      Result := IncWeek(AValue, AIncrementValue);
    rcduMonth:
      Result := IncMonth(AValue, AIncrementValue);
    rcduQuarter:
      Result := IncQuarter(AValue, AIncrementValue)
  else //rcduYear
      Result := IncYear(AValue, AIncrementValue)
  end;
end;

class function TdxRangeControlDateTimeClientHelper.QuarterSpan(const AMin: TDateTime; AMax: TDateTime): Double;
begin
  Result := MonthSpan(AMax, AMin) / 3;
end;

class function TdxRangeControlDateTimeClientHelper.RoundDate(const ADate: TDateTime;
  AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
begin
  Result := ADate;
  case AScaleUnit of
    rcduSecond:
      if MilliSecondOf(Result) >= 500 then
        Result := IncSecond(Result);
    rcduMinute:
      if SecondOf(Result) >= 30 then
        Result := IncMinute(Result);
    rcduHour:
      if MinuteOf(Result) >= 30 then
        Result := IncHour(Result);
    rcduDay:
      begin
      if HourOf(Result) >= 12 then
        Result := IncDay(Result);
      end;
    rcduWeek:
      if dxDayOfWeekOffset(Result) >= 3 then
        Result := IncWeek(Result);
    rcduQuarter:
      if DayOfTheQuarter(Result) >= DaysInQuarter(Result) / 2  then
        Result := IncQuarter(Result);
    rcduMonth:
      begin
        if DayOf(Result) >= DaysInMonth(Result) / 2 then
          Result := IncMonth(Result);
      end
  else //rcduYear
    if MonthOf(Result) >= 6 then
      Result := IncYear(Result);
  end;
  Result := TruncDate(Result, AScaleUnit);
end;

class function TdxRangeControlDateTimeClientHelper.StartOfTheQuarter(const ADate: TDateTime): TDateTime;
begin
  Result := StartOfTheMonth(ADate);
  while (MonthOf(Result) - 1) mod 3 <> 0 do
    Result := IncMonth(Result, -1);
end;

class function TdxRangeControlDateTimeClientHelper.TruncDate(const ADate: TDateTime; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TDateTime;
begin
  case AScaleUnit of
    rcduSecond:
      Result := RecodeMilliSecond(ADate, 0);
    rcduMinute:
      Result := RecodeTime(ADate, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, 0, 0);
    rcduHour:
      Result := RecodeTime(ADate, RecodeLeaveFieldAsIs, 0, 0, 0);
    rcduWeek:
      Result := dxGetStartDateOfWeek(ADate);
    rcduQuarter:
      Result := StartOfTheQuarter(ADate);
    rcduMonth:
      Result := StartOfTheMonth(ADate);
    rcduYear:
      Result := StartOfTheYear(ADate);
  else // rcduDay
    Result := StartOfTheDay(ADate);
  end;
end;

{ TdxRangeControlDateTimeClientViewInfo }

procedure TdxRangeControlDateTimeClientViewInfo.CalculateBounds(const ABounds: TRect);
var
  AValue: TDateTime;
  ANormalValue: Double;
  ATickMarkDistance: Integer;
  AScaleFactor: Double;
begin
  inherited;
  ScaleInterval := Properties.ScaleInterval;
  AScaleFactor := ViewInfo.ClientAreaNormalFactor;
  AValue := Properties.IncDate(Properties.MinValue, ScaleInterval);
  ANormalValue := Properties.GetNormalizedValue(AValue);
  ATickMarkDistance := Round(ANormalValue * AScaleFactor);
  if ATickMarkDistance < Properties.ScaleIntervalMinWidth then
  begin
    ANormalValue := Properties.ScaleIntervalMinWidth / AScaleFactor;
    AValue := Properties.GetOriginalValue(ANormalValue);
    ScaleInterval := VarAsType(
      Properties.GetDateInterval(Properties.MinValue, AValue), Properties.GetScaleIntervalValueType);
  end;
  NormalizedFirstTickMarkValue := Properties.GetNormalizedValue(Properties.IncDate(Properties.MinValue,
    Ceil(Properties.GetDateInterval(Properties.MinValue, Control.VisibleRangeMinValue) / ScaleInterval) * ScaleInterval));
end;

function TdxRangeControlDateTimeClientViewInfo.CreateRulerInfo: TdxRangeControlRulerViewInfo;
begin
  Result := TdxRangeControlSimpleRulerViewInfo.Create(ViewInfo);
end;

procedure TdxRangeControlDateTimeClientViewInfo.DrawScale(ACanvas: TcxCanvas);
begin
  DrawGridLines(ACanvas);
  inherited DrawScale(ACanvas);
end;

function TdxRangeControlDateTimeClientViewInfo.GetGridLineColor: TdxAlphaColor;
var
  AColor: TdxAlphaColor;
begin
  AColor := (Properties.Style as TdxRangeControlClientStyle).GridLineColor;
  if AColor <> dxacDefault then
    Result := AColor
  else
    Result := inherited GetGridLineColor;
end;

function TdxRangeControlDateTimeClientViewInfo.GetProperties: TdxRangeControlCustomDateTimeClientProperties;
begin
  Result := Control.ClientProperties as TdxRangeControlCustomDateTimeClientProperties;
end;

{ TdxRangeControlCustomDateTimeClientProperties }

constructor TdxRangeControlCustomDateTimeClientProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FScales := CreateScales;
  FMinValue := Date - 2;
  FMaxValue := Date + 2;
end;

destructor TdxRangeControlCustomDateTimeClientProperties.Destroy;
begin
  FreeAndNil(FScales);
  inherited Destroy;
end;

function TdxRangeControlCustomDateTimeClientProperties.GetInterval(const AMin, AMax: Double): Variant;
begin
  Result := Round(GetDateInterval(GetOriginalValue(AMin), GetOriginalValue(AMax)));
end;

function TdxRangeControlCustomDateTimeClientProperties.GetNearestNormalizedAcceptableValue(const AValue: Double): Double;
begin
  Result := GetNormalizedValue(RoundDate(GetOriginalValue(AValue)));
end;

function TdxRangeControlCustomDateTimeClientProperties.IncDate(const AValue: Double; AIncrementValue: Int64 = 1): Double;
begin
  Result := ActualPrimaryScale.IncDate(AValue, AIncrementValue);
end;

function TdxRangeControlCustomDateTimeClientProperties.TruncDate(const ADate: TDateTime): TDateTime;
begin
  Result := ActualPrimaryScale.TruncDate(ADate);
end;

function TdxRangeControlCustomDateTimeClientProperties.CeilDate(const ADate: TDateTime): TDateTime;
begin
  Result := ActualPrimaryScale.CeilDate(ADate);
end;

function TdxRangeControlCustomDateTimeClientProperties.CreatePredefinedDateTimeScale(
  AScales: TdxRangeControlDateTimeScales; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale;
begin
  Result := TdxRangeControlPredefinedDateTimeScale.Create(AScales, AScaleUnit);
end;

function TdxRangeControlCustomDateTimeClientProperties.CreateScales: TdxRangeControlDateTimeScales;
begin
  Result := TdxRangeControlDateTimeScales.Create(Self);
end;

function TdxRangeControlCustomDateTimeClientProperties.CreateStyle: TdxRangeControlCustomClientStyle;
begin
  Result := TdxRangeControlClientStyle.Create(Self);
end;

function TdxRangeControlCustomDateTimeClientProperties.CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo):
    TdxRangeControlCustomClientViewInfo;
begin
  Result := TdxRangeControlDateTimeClientViewInfo.Create(AViewInfo);
end;

procedure TdxRangeControlCustomDateTimeClientProperties.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlCustomDateTimeClientProperties;
begin
  inherited DoAssign(Source);
  if Source is TdxRangeControlCustomDateTimeClientProperties then
  begin
    ASource := TdxRangeControlCustomDateTimeClientProperties(Source);
    Scales := ASource.Scales;
  end;
end;

procedure TdxRangeControlCustomDateTimeClientProperties.DoGetLabelText(const AValue: TDateTime; var ADisplayText: string);
begin
  if Assigned(FOnGetLabelText) then
    FOnGetLabelText(Self, AValue, ADisplayText);
end;

function TdxRangeControlCustomDateTimeClientProperties.RoundDate(const ADate: TDateTime): TDateTime;
begin
  Result := ActualPrimaryScale.RoundDate(ADate);
end;

function TdxRangeControlCustomDateTimeClientProperties.GetDateInterval(const AMin, AMax: TDateTime): Double;
begin
  Result := ActualPrimaryScale.GetDateInterval(AMin, AMax);
end;

function TdxRangeControlCustomDateTimeClientProperties.GetLabelText(const AValue: Variant): string;
begin
  Result := ActualPrimaryScale.GetDefaultDisplayText(AValue);
  DoGetLabelText(AValue, Result);
end;

function TdxRangeControlCustomDateTimeClientProperties.DoGetNextValue(const AOriginal, AInterval: Variant): Variant;
begin
  Result := IncDate(RoundDate(AOriginal), Round(AInterval));
end;

function TdxRangeControlCustomDateTimeClientProperties.GetActualPrimaryScale: TdxRangeControlDateTimeScale;
begin
  Result := PrimaryScale;
end;

function TdxRangeControlCustomDateTimeClientProperties.GetPrimaryScale: TdxRangeControlDateTimeScale;
begin
  Result := Scales.PrimaryScale;
end;

function TdxRangeControlCustomDateTimeClientProperties.GetRangeValueType: Word;
begin
  Result := varDate;
end;

function TdxRangeControlCustomDateTimeClientProperties.GetVisibleRangeValueType: Word;
begin
  Result := varDate;
end;

procedure TdxRangeControlCustomDateTimeClientProperties.PrimaryScaleChanged;
begin
  BeginUpdate;
  try
    MinValue := MinValue;
    MaxValue := MaxValue;
    RangeControl.SelectedRangeMaxValue := RangeControl.SelectedRangeMaxValue;
    RangeControl.SelectedRangeMinValue := RangeControl.SelectedRangeMinValue;
  finally
    EndUpdate;
  end;
end;

procedure TdxRangeControlCustomDateTimeClientProperties.ScalesChanged;
begin
  Changed;
end;

procedure TdxRangeControlCustomDateTimeClientProperties.SetMaxValue(Value: Variant);
begin
  Value := CeilDate(Value);
  inherited SetMaxValue(Value);
end;

procedure TdxRangeControlCustomDateTimeClientProperties.SetMinValue(Value: Variant);
begin
  Value := TruncDate(Value);
  inherited SetMinValue(Value);
end;

procedure TdxRangeControlCustomDateTimeClientProperties.SetPrimaryScale(Value: TdxRangeControlDateTimeScale);
begin
  Scales.PrimaryScale := Value;
end;

procedure TdxRangeControlCustomDateTimeClientProperties.SetScales(AValue: TdxRangeControlDateTimeScales);
begin
  FScales.Assign(AValue);
end;

{ TdxRangeControlDateTimeClientProperties }

function TdxRangeControlDateTimeClientProperties.GetScale: TdxRangeControlDateTimeScale;
begin
  Result := PrimaryScale;
end;

procedure TdxRangeControlDateTimeClientProperties.SetScale(
  const Value: TdxRangeControlDateTimeScale);
begin
  PrimaryScale := Value;
end;

{ TdxRangeControlDateTimeScale }

function TdxRangeControlDateTimeScale.CeilDate(const ADate: TDateTime): TDateTime;
begin
  Result := TruncDate(ADate);
  if Result <> ADate then
    Result := IncDate(Result);
end;

function TdxRangeControlDateTimeScale.GetDisplayText(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := GetDefaultDisplayText(ADate);
end;

function TdxRangeControlDateTimeScale.GetDefaultDisplayText(const ADate: TDateTime): string;
begin
  Result := DateToStr(ADate);
end;

procedure TdxRangeControlDateTimeScale.DoAssign(Source: TPersistent);
var
  ASourceScale: TdxRangeControlDateTimeScale;
begin
  if Source is TdxRangeControlDateTimeScale then
  begin
    ASourceScale := TdxRangeControlDateTimeScale(Source);
    Active := ASourceScale.Active;
    Visible := ASourceScale.Visible;
  end;
end;

procedure TdxRangeControlDateTimeScale.Changed;
begin
  Scales.Changed;
end;

function TdxRangeControlDateTimeScale.GetScales: TdxRangeControlDateTimeScales;
begin
  Result := Owner as TdxRangeControlDateTimeScales;
end;

function TdxRangeControlDateTimeScale.GetActive: Boolean;
begin
  Result := Scales.PrimaryScale = Self;
end;

function TdxRangeControlDateTimeScale.GetAutoFormatHelper: TdxRangeControlDateTimeAutoFormatHelperClass;
begin
  Result := (Scales.Owner as TdxRangeControlCustomDateTimeHeaderClientProperties).AutoFormatHelper;
end;

function TdxRangeControlDateTimeScale.GetDateInterval(const AMin, AMax: TDateTime): Double;
begin
  Result := TdxRangeControlDateTimeClientHelper.GetDateInterval(AMin, AMax, ScaleUnit);
end;

function TdxRangeControlDateTimeScale.GetFormatCount: Integer;
begin
  Result := 1;
end;

function TdxRangeControlDateTimeScale.IncDate(const AValue: Double; AIncrementValue: Int64 = 1): Double;
begin
  Result := TdxRangeControlDateTimeClientHelper.IncDate(AValue, ScaleUnit, AIncrementValue);
end;

function TdxRangeControlDateTimeScale.RoundDate(const ADate: TDateTime): TDateTime;
begin
  Result := TdxRangeControlDateTimeClientHelper.RoundDate(ADate, ScaleUnit);
end;

function TdxRangeControlDateTimeScale.TruncDate(const ADate: TDateTime): TDateTime;
begin
  Result := TdxRangeControlDateTimeClientHelper.TruncDate(ADate, ScaleUnit);
end;

procedure TdxRangeControlDateTimeScale.SetActive(const Value: Boolean);
begin
  Scales.PrimaryScale := Self;
end;

procedure TdxRangeControlDateTimeScale.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TdxRangeControlPredefinedDateTimeScale }

constructor TdxRangeControlPredefinedDateTimeScale.Create(AOwner: TPersistent; AScaleUnit: TdxRangeControlDateTimeScaleUnit);
begin
  inherited Create(AOwner);
  FScaleUnit := AScaleUnit;
  FDisplayFormat := GetDefaultDisplayFormat;
end;

function TdxRangeControlPredefinedDateTimeScale.GetDefaultDisplayText(const ADate: TDateTime): string;
var
  ADisplayFormat: string;
begin
  ADisplayFormat := DisplayFormat;
  if ADisplayFormat = '' then
  begin
    case ScaleUnit of
      rcduSecond:
        Result := DateTimeToStr(ADate);
      rcduMinute:
        Result := DateTimeToStr(ADate);
      rcduHour:
        Result := DateTimeToStr(ADate);
      rcduDay:
        Result := DateToStr(ADate);
      rcduWeek:
        Result := DateToStr(ADate);
      rcduMonth:
        Result := FormatDateTime('MMMM yyyy', ADate);
      rcduQuarter:
        Result := Format('Q%d'' ', [(MonthOf(ADate) - 1) div 3 + 1]) + FormatDateTime('yyyy', ADate)
    else //rcduYear
        Result := FormatDateTime('yyyy', ADate);
    end;
  end
  else
  begin
    if ScaleUnit = rcduQuarter then
    begin
      ADisplayFormat := StringReplace(ADisplayFormat, '{Value}', '%d', []);
      Result := Format(ADisplayFormat, [(MonthOf(ADate) - 1) div 3 + 1]);
    end
    else
      Result := FormatDateTime(ADisplayFormat, ADate);
  end;
end;

procedure TdxRangeControlPredefinedDateTimeScale.DoAssign(Source: TPersistent);
var
  ASourceScale: TdxRangeControlPredefinedDateTimeScale;
begin
  if Source is TdxRangeControlPredefinedDateTimeScale then
  begin
    ASourceScale := TdxRangeControlPredefinedDateTimeScale(Source);
    DisplayFormat := ASourceScale.DisplayFormat;
  end;
end;

function TdxRangeControlPredefinedDateTimeScale.GetDefaultDisplayFormat: string;
begin
  Result := '';
end;

function TdxRangeControlPredefinedDateTimeScale.GetFormatCount: Integer;
begin
  Result := AutoFormatHelper.GetFormatCount(ScaleUnit);
end;

function TdxRangeControlPredefinedDateTimeScale.GetScaleUnit: TdxRangeControlDateTimeScaleUnit;
begin
  Result := FScaleUnit;
end;

procedure TdxRangeControlPredefinedDateTimeScale.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Changed;
  end;
end;

{ TdxRangeControlPredefinedDateHeaderScale }

function TdxRangeControlPredefinedDateHeaderScale.GetDisplayText(const ADate: TDateTime; AFormatId: Integer): string;
begin
  case ScaleUnit of
    rcduMinute:
      Result := AutoFormatHelper.FormatMinute(ADate, AFormatId);
    rcduHour:
      Result := AutoFormatHelper.FormatHour(ADate, AFormatId);
    rcduDay:
      Result := AutoFormatHelper.FormatDay(ADate, AFormatId);
    rcduWeek:
      Result := AutoFormatHelper.FormatWeek(ADate, AFormatId);
    rcduMonth:
      Result := AutoFormatHelper.FormatMonth(ADate, AFormatId);
    rcduQuarter:
      Result := AutoFormatHelper.FormatQuarter(ADate, AFormatId);
    rcduYear:
      Result := AutoFormatHelper.FormatYear(ADate, AFormatId);
  else // rcduSecond
    Result := AutoFormatHelper.FormatSecond(ADate, AFormatId);
  end;
end;

function TdxRangeControlPredefinedDateHeaderScale.GetDefaultDisplayText(const ADate: TDateTime): string;
var
  ADisplayFormat: string;
begin
  if DisplayFormat = '' then
    ADisplayFormat := GetDefaultDisplayFormat
  else
    ADisplayFormat := DisplayFormat;
  if ScaleUnit = rcduWeek then
    Result := FormatDateTime(ADisplayFormat, dxGetStartDateOfWeek(ADate)) + ' - ' +
      FormatDateTime(ADisplayFormat, dxGetStartDateOfWeek(ADate) + 6)
  else
    if ScaleUnit = rcduQuarter then
    begin
      ADisplayFormat := StringReplace(ADisplayFormat, '{Value}', '%d', []);
      Result := Format(ADisplayFormat, [(MonthOf(ADate) - 1) div 3 + 1]);
    end
    else
      Result := FormatDateTime(ADisplayFormat, ADate);
end;

function TdxRangeControlPredefinedDateHeaderScale.GetDefaultDisplayFormat: string;
begin
  case ScaleUnit of
    rcduSecond:
      Result := 'ss';
    rcduMinute:
      Result := 'nn';
    rcduHour:
      Result := 'hh';
    rcduDay:
      Result := 'd ddd';
    rcduWeek:
      Result := 'd';
    rcduMonth:
      Result := 'MMMM';
    rcduQuarter:
      Result := 'Q{Value}';
  else //rcduYear
    Result := 'yyyy';
  end;
end;

function TdxRangeControlPredefinedDateHeaderScale.IsDisplayFormatStored: Boolean;
begin
  Result := DisplayFormat <> GetDefaultDisplayFormat;
end;

{ TdxRangeControlDateTimeScales }

constructor TdxRangeControlDateTimeScales.Create(AOwner: TPersistent);
begin
  inherited;
  FYear := CreatePredefinedDateTimeScale(rcduYear);
  FMonth  := CreatePredefinedDateTimeScale(rcduMonth);
  FWeek := CreatePredefinedDateTimeScale(rcduWeek);
  FDay := CreatePredefinedDateTimeScale(rcduDay);
  FHour := CreatePredefinedDateTimeScale(rcduHour);
  FMinute := CreatePredefinedDateTimeScale(rcduMinute);
  FQuarter := CreatePredefinedDateTimeScale(rcduQuarter);
  FPrimaryScale := FDay;
  FCustomScales := TdxRangeControlDateTimeScaleList.Create;
end;

destructor TdxRangeControlDateTimeScales.Destroy;
begin
  FreeAndNil(FCustomScales);
  FreeAndNil(FQuarter);
  FreeAndNil(FMinute);
  FreeAndNil(FHour);
  FreeAndNil(FDay);
  FreeAndNil(FWeek);
  FreeAndNil(FMonth);
  FreeAndNil(FYear);
  inherited;
end;

procedure TdxRangeControlDateTimeScales.Changed;
begin
  (Owner as TdxRangeControlCustomDateTimeClientProperties).ScalesChanged;
end;

function TdxRangeControlDateTimeScales.CreatePredefinedDateTimeScale(AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale;
begin
  Result := (Owner as TdxRangeControlCustomDateTimeClientProperties).CreatePredefinedDateTimeScale(Self, AScaleUnit);
end;

procedure TdxRangeControlDateTimeScales.DoAssign(Source: TPersistent);
var
  ASourceScales: TdxRangeControlDateTimeScales;
begin
  if Source is TdxRangeControlDateTimeScales then
  begin
    ASourceScales := TdxRangeControlDateTimeScales(Source);
    Year := ASourceScales.Year;
    Quarter := ASourceScales.Quarter;
    Month  := ASourceScales.Month;
    Week := ASourceScales.Week;
    Day := ASourceScales.Day;
    Hour := ASourceScales.Hour;
    Minute := ASourceScales.Minute;
  end;
end;

function TdxRangeControlDateTimeScales.GetScale(
  AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale;
begin
   case AScaleUnit of
     rcduMinute:
       Result := Minute;
     rcduHour:
       Result := Hour;
     rcduDay:
       Result := Day;
     rcduWeek:
       Result := Week;
     rcduMonth:
       Result := Month;
     rcduQuarter:
       Result := Quarter;
     rcduYear:
       Result := Year
   else
     Result := nil;
   end;
end;

function TdxRangeControlDateTimeScales.RegisterScale(AClass: TdxRangeControlDateTimeScaleClass): TdxRangeControlDateTimeScale;
var
  AIndex: Integer;
begin
  AIndex := FCustomScales.Add(AClass.Create(Self));
  Result := FCustomScales[AIndex];
  Changed;
end;

procedure TdxRangeControlDateTimeScales.SetPrimaryScale(Value: TdxRangeControlDateTimeScale);
begin
  if FPrimaryScale <> Value then
  begin
    FPrimaryScale := Value;
    (Owner as TdxRangeControlCustomDateTimeClientProperties).PrimaryScaleChanged;
  end;
end;

function TdxRangeControlDateTimeScales.GetPrimaryScale: TdxRangeControlDateTimeScale;
begin
  Result := FPrimaryScale;
  if Result = nil then
    Result := Day;
end;

procedure TdxRangeControlDateTimeScales.SetDay(Value: TdxRangeControlDateTimeScale);
begin
  FDay.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.SetHour(Value: TdxRangeControlDateTimeScale);
begin
  FHour.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.SetMinute(Value: TdxRangeControlDateTimeScale);
begin
  FMinute.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.SetMonth(Value: TdxRangeControlDateTimeScale);
begin
  FMonth.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.SetQuarter(AValue: TdxRangeControlDateTimeScale);
begin
  FQuarter.Assign(AValue);
end;

procedure TdxRangeControlDateTimeScales.SetWeek(Value: TdxRangeControlDateTimeScale);
begin
  FWeek.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.SetYear(Value: TdxRangeControlDateTimeScale);
begin
  FYear.Assign(Value);
end;

procedure TdxRangeControlDateTimeScales.UnRegisterScale(AScale: TdxRangeControlDateTimeScale);
begin
  FCustomScales.Remove(AScale);
  Changed;
end;

{ TdxRangeControlDateTimeAutoFormatHelper }

class function TdxRangeControlDateTimeAutoFormatHelper.ChooseFormat(const ADate: TDateTime; AFont: TFont; AWidth: Integer; AScale: TdxRangeControlDateTimeScale): Integer;
var
  I: Integer;
  ATextWidth: Integer;
begin
  Result := 0;
  for I := 1 to AScale.GetFormatCount - 1 do
  begin
    ATextWidth := cxTextSize(AFont, AScale.GetDisplayText(ADate, I)).cx;
    if ATextWidth > AWidth then
      Break;
    Result := I;
  end;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatDay(const ADate: TDateTime; AFormatId: Integer): string;
begin
  cxGetDateFormat(ADate, Result, 0, FDayFormats[AFormatId]);
end;

class destructor TdxRangeControlDateTimeAutoFormatHelper.Finalize;
begin
  FreeAndNil(FDayFormats);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FindBeginOfSequence(
  const AStr: string; AStartPosition: Integer; const ASoughtForChars: string; AForward: Boolean): Integer;
var
  ACurrentChar: Char;
  ACount: Integer;
  I: Integer;
  AInStringLiteral: Boolean;
  AStep: Integer;
begin
  Result := 0;
  AInStringLiteral := False;
  ACount := Length(AStr);
  AStep := IfThen(AForward, 1, -1);
  I := AStartPosition;
  while (I >= 1) and (I <= ACount) do
  begin
    ACurrentChar := AStr[I];
    if ACurrentChar = '''' then
      AInStringLiteral := not AInStringLiteral
    else
      if not AInStringLiteral and (Pos(ACurrentChar, ASoughtForChars) > 0) then
      begin
        Result := I;
        Break;
      end;
    Inc(I, AStep);
  end;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FindEndOfSequence(
  const AStr: string; AStartPosition: Integer; const ASoughtForChar: Char): Integer;
var
  ACount: Integer;
  I: Integer;
begin
  ACount := Length(AStr);
  I := AStartPosition;
  while (I <= ACount) and (AStr[I] = ASoughtForChar) do
    Inc(I);
  Result := I;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatMonth(const ADate: TDateTime; AFormatId: Integer): string;
begin
  case AFormatId of
    0: Result := FormatDateTime('mm', ADate);
    1: Result := FormatDateTime('mmm', ADate);
  else
    Result := FormatDateTime('mmmm', ADate);
  end;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.GetFormatCount(AScaleUnit: TdxRangeControlDateTimeScaleUnit): Integer;
begin
  case AScaleUnit of
    rcduMinute:
      Result := 1;
    rcduHour:
      Result := 1;
    rcduDay:
      Result := FDayFormats.Count;
    rcduWeek:
      Result := FDayFormats.Count;
    rcduMonth:
      Result := 3;
    rcduQuarter:
      Result := 1;
    rcduYear:
      Result := 1;
  else // rcduSecond
    Result := 1;
  end;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatHour(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := FormatDateTime('hh', ADate);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatMinute(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := FormatDateTime('nn', ADate);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatQuarter(const ADate: TDateTime; AFormatId: Integer): string;
var
  AQuarterNumber: Integer;
begin
  AQuarterNumber := (MonthOf(ADate) - 1) div 3 + 1;
  Result := Format('Q%d', [AQuarterNumber]);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatSecond(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := FormatDateTime('ss', ADate);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatWeek(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := FormatDay(dxGetStartDateOfWeek(ADate), AFormatId) + ' - ' +
    FormatDay(dxGetStartDateOfWeek(ADate) + 6, AFormatId);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.FormatYear(const ADate: TDateTime; AFormatId: Integer): string;
begin
  Result := FormatDateTime('yyyy', ADate);
end;

class constructor TdxRangeControlDateTimeAutoFormatHelper.Initialize;
begin
  FDayFormats := TStringList.Create;
  InitializeDayFormats;
end;

class procedure TdxRangeControlDateTimeAutoFormatHelper.InitializeDayFormats;
var
  AShortFormat, ALongFormat: string;
  APattern: string;
begin
  AShortFormat := cxFormatController.LocalFormatSettings.ShortDateFormat;
  ALongFormat := StripYear(cxFormatController.LocalFormatSettings.LongDateFormat);
  if Pos('ddd', ALongFormat) = 0 then
    ALongFormat := Format('dddd, %s', [ALongFormat]);
  FDayFormats.Add(ALongFormat);

  APattern := StringReplace(ALongFormat, 'MMMM', 'MMM', [rfReplaceAll]);
  FDayFormats.Insert(0, APattern);

  APattern := StringReplace(APattern, 'dddd', 'ddd', [rfReplaceAll]);
  FDayFormats.Insert(0, APattern);

  APattern := StripYear(AShortFormat);
  FDayFormats.Insert(0, APattern);

  APattern := StripMonth(APattern);
  FDayFormats.Insert(0, APattern);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.Strip(const APattern: string; AFormatChar: Char; ALength: Integer): string;

const
  DateTimePatterns = 'ydfghHmMstz';
  DateTimeSeparators = '.,:/-';

var
  AComposedTerminators: string;
  ANextChar, APrevChar: string;
  APatternLength: Integer;
  ASequenceStart, ASequenceEnd: Integer;
  AWhiteSpace: string;
  AYearPrefixStart, AYearSuffixEnd: Integer;
begin
  AComposedTerminators := DateTimePatterns + DateTimeSeparators;
  Result := Trim(APattern);
  ASequenceEnd := 0;
  repeat
    ASequenceStart := FindBeginOfSequence(APattern, ASequenceEnd + 1, AFormatChar, True);
    if ASequenceStart > 0 then
      ASequenceEnd := FindEndOfSequence(APattern, ASequenceStart, AFormatChar);
  until (ASequenceStart <= 0) or (ALength = -1) or (ALength = ASequenceEnd - ASequenceStart);

  if ASequenceStart > 0 then
  begin
    if ASequenceStart = 1 then
    begin
      AYearSuffixEnd := FindBeginOfSequence(Result, ASequenceEnd, DateTimePatterns, True);
      if AYearSuffixEnd = 0 then
        Result := ''
      else
        Result := Copy(Result, 1, ASequenceStart - 1) + Copy(Result,  AYearSuffixEnd, Length(Result));
    end
    else
      if ASequenceEnd > Length(Result) then
      begin
        AYearPrefixStart := FindBeginOfSequence(Result, ASequenceStart - 1, DateTimePatterns, false);
        if AYearPrefixStart = 0 then
          Result := ''
        else
          Result := Copy(Result, 1, AYearPrefixStart) + Copy(Result, ASequenceEnd, Length(Result));
      end
      else
      begin
        AYearSuffixEnd := FindBeginOfSequence(Result, ASequenceEnd, AComposedTerminators, True);
        AYearPrefixStart := FindBeginOfSequence(Result, ASequenceStart - 1, DateTimePatterns, false);
        AWhiteSpace := '';
        if (AYearPrefixStart > 0) and (AYearSuffixEnd > 0) then
        begin
          APrevChar := IfThen(AYearPrefixStart > 1, Result[AYearPrefixStart], DateTimePatterns[1]);
          ANextChar := Result[AYearSuffixEnd];
          if (Pos(APrevChar, DateTimePatterns) > 0) and (Pos(ANextChar, DateTimePatterns) > 0) then
            AWhiteSpace := ' ';
        end;
        if AYearSuffixEnd = 0 then
          Result := Copy(Result, 1, ASequenceStart - 1)
        else
          Result := Copy(Result, 1, ASequenceStart - 1) + Copy(Result, AYearSuffixEnd, Length(Result));
        if AYearPrefixStart = 0 then
          Result := Copy(Result, ASequenceStart, Length(Result))
        else
        begin
          APatternLength := Length(Result);
          if (AYearPrefixStart <= APatternLength) and (ASequenceStart <= APatternLength) then
          begin
            APrevChar := Result[AYearPrefixStart];
            ANextChar := Result[ASequenceStart];
            if (Pos(APrevChar, DateTimePatterns) > 0) and (Pos(ANextChar, DateTimePatterns) > 0) then
              AWhiteSpace := ' ';
          end;
          Result := Copy(Result, 1, AYearPrefixStart) + AWhiteSpace + Copy(Result, ASequenceStart, Length(Result));
        end;
      end;
    Result := Trim(Result);
  end;
end;

class function TdxRangeControlDateTimeAutoFormatHelper.StripDay(const APattern: string): string;
begin
  Result := Strip(APattern, 'd', 2);
  Result := Strip(Result, 'd', 1);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.StripDayOfWeek(const APattern: string): string;
begin
  Result := Strip(APattern, 'd', 4);
  Result := Strip(Result, 'd', 3);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.StripMonth(const APattern: string): string;
begin
  Result := Strip(APattern, 'M', -1);
end;

class function TdxRangeControlDateTimeAutoFormatHelper.StripYear(const APattern: string): string;
begin
  Result := Strip(APattern, 'y', -1);
end;

{ TdxRangeControlDateTimeHeaderClientContentViewInfo }

constructor TdxRangeControlDateTimeHeaderClientContentViewInfo.Create(AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create(AViewInfo);
  FElements := TObjectList<TdxRangeControlDateTimeHeaderClientContentElementViewInfo>.Create;
end;

destructor TdxRangeControlDateTimeHeaderClientContentViewInfo.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TdxRangeControlDateTimeHeaderClientContentViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Elements.Count - 1 do
    Elements[I].Paint(ACanvas);
end;

function TdxRangeControlDateTimeHeaderClientContentViewInfo.AddElement: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
begin
  Result := CreateContentElementViewInfo;
  FElements.Add(Result);
end;

procedure TdxRangeControlDateTimeHeaderClientContentViewInfo.CalculateBounds(const ABounds: TRect);

  function CreateContentElement(APrimaryScale: TdxRangeControlDateTimeScale;
    ADate: TDateTime; const ARect: TRect): TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
  begin
    Result := AddElement;
    Result.CalculateBounds(ARect);
    Result.MinDate := ADate;
    Result.MaxDate := APrimaryScale.IncDate(ADate);
  end;

var
  ARect: TRect;
  I: Integer;
  ARuler: TdxRangeControlDateTimeHeaderClientRulerViewInfo;
  ARulerElement: TdxRangeControlScaleElementViewInfo;
  AContentElement: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
  APrimaryScaleInfo: TdxRangeControlRulerScaleViewInfo;
  APrimaryScale: TdxRangeControlDateTimeScale;
  AProperties: TdxRangeControlCustomDateTimeHeaderClientProperties;
  ADate: TDateTime;
  AVisibleRangeMinValue, AVisibleRangeMaxValue: TDateTime;
  AIntervalCount: Double;
  AIntervalWidth: Double;
begin
  inherited CalculateBounds(ABounds);
  Elements.Clear;
  AProperties := ViewInfo.ClientProperties as TdxRangeControlCustomDateTimeHeaderClientProperties;
  APrimaryScale := AProperties.ActualPrimaryScale;
  APrimaryScaleInfo := nil;
  ARuler := ViewInfo.Ruler as TdxRangeControlDateTimeHeaderClientRulerViewInfo;
  for I := 0 to ARuler.ScaleInfos.Count - 1 do
    if ARuler.ScaleInfos[I].Scale = APrimaryScale then
    begin
      APrimaryScaleInfo := ARuler.ScaleInfos[I];
      Break;
    end;
  if APrimaryScaleInfo <> nil then
  begin
    ARect := Bounds;
    for I := 0 to APrimaryScaleInfo.Elements.Count - 1 do
    begin
      ARulerElement := APrimaryScaleInfo.Elements[I];
      AContentElement := AddElement;
      AContentElement.MinDate := ARulerElement.MinValue;
      AContentElement.MaxDate := ARulerElement.MaxValue;
      ARect.Left := ARulerElement.Bounds.Left;
      ARect.Right := ARulerElement.Bounds.Right;
      AContentElement.CalculateBounds(ARect);
    end
  end
  else
  begin
    AVisibleRangeMinValue := Control.VisibleRangeMinValue;
    AVisibleRangeMaxValue := Control.VisibleRangeMaxValue;
    AIntervalCount := APrimaryScale.GetDateInterval(AVisibleRangeMinValue, AVisibleRangeMaxValue);
    AIntervalWidth := (Bounds.Width - 1) / AIntervalCount;
    if AIntervalWidth >= AProperties.ScaleIntervalMinWidth then
    begin
      ARect := Bounds;
      ADate := APrimaryScale.TruncDate(AVisibleRangeMinValue);
      while CreateContentElement(APrimaryScale, ADate, ARect).MaxDate < AVisibleRangeMaxValue do
      begin
        AContentElement := Elements.Last;
        ARect.Right := ViewInfo.GetPositionFromValue(AContentElement.MaxDate);
        AContentElement.CalculateBounds(ARect);
        ARect.Left := ARect.Right;
        ARect.Right := Bounds.Right;
        ADate := AContentElement.MaxDate;
      end;
    end;
  end;
end;

function TdxRangeControlDateTimeHeaderClientContentViewInfo.CreateContentElementViewInfo: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
begin
  Result := TdxRangeControlDateTimeHeaderClientContentElementViewInfo.Create(ViewInfo);
end;

{ TdxRangeControlDateTimeHeaderClientViewInfo }

procedure TdxRangeControlDateTimeHeaderClientViewInfo.CalculateBounds(const ABounds: TRect);

  procedure CreateScales;
  var
    AScaleInfo: TdxRangeControlRulerScaleViewInfo;
    AIntervalCount: Double;
    AIntervalWidth: Double;
    AScaleRect: TRect;
    I: Integer;
    AMin, AMax: Variant;
  begin
    AScaleRect := Bounds;
    AScaleRect.Bottom := AScaleRect.Top;
    AMin := Control.VisibleRangeMinValue;
    AMax := Control.VisibleRangeMaxValue;
    for I := 0 to Properties.VisibleScales.Count - 1 do
    begin
      AIntervalCount := Properties.VisibleScales[I].GetDateInterval(AMin, AMax);
      AIntervalWidth := (Bounds.Width - 1) / AIntervalCount;
      if AIntervalWidth >= Properties.ScaleIntervalMinWidth then
      begin
        AScaleRect.Top := AScaleRect.Bottom;
        AScaleRect.Bottom := AScaleRect.Top + GetScaleHeight;
        if AScaleRect.Bottom > Bounds.Bottom then
          Break;
        AScaleInfo := Ruler.AddScale(Properties.VisibleScales[I]);
        AScaleInfo.CalculateBounds(AScaleRect);
      end;
    end;
  end;

  function CreateRulerElement(AScaleInfo: TdxRangeControlRulerScaleViewInfo; ADate: TDateTime; const ARect: TRect): TdxRangeControlScaleElementViewInfo;
  begin
    Result := AScaleInfo.AddElement;
    Result.CalculateBounds(ARect);
    Result.MinValue := ADate;
    Result.MaxValue := AScaleInfo.Scale.IncDate(ADate);
  end;

var
  I, J: Integer;
  ADate: TDateTime;
  AElementRect, ARect: TRect;
  AMinFormatId, AFormatId: Integer;
  ACurrentScaleInfo: TdxRangeControlRulerScaleViewInfo;
  ARulerElement: TdxRangeControlScaleElementViewInfo;
  AAutoFormat: Boolean;
  AVisibleRangeMaxValue, AVisibleRangeMinValue: Variant;
begin
  inherited CalculateBounds(ABounds);
  Ruler.ScaleInfos.Clear;
  Properties.FMaxIntervalCount := (Bounds.Width - 1) / Properties.ScaleIntervalMinWidth;

  if (Properties.FMaxIntervalCount >= 1) and ViewInfo.RangeControl.ShowRuler then
    CreateScales;

  if Ruler.ScaleInfos.Count = 0 then
  begin
    Ruler.CalculateBounds(cxEmptyRect);
    Content.CalculateBounds(Bounds);
    Exit;
  end;

  AAutoFormat := Properties.AutoFormatScaleCaptions and (Properties.AutoFormatHelper <> nil);
  AVisibleRangeMaxValue := Control.VisibleRangeMaxValue;
  AVisibleRangeMinValue := Control.VisibleRangeMinValue;
  for I := 0 to Ruler.ScaleInfos.Count - 1 do
  begin
    ACurrentScaleInfo := Ruler.ScaleInfos[I];
    AElementRect := ACurrentScaleInfo.Bounds;
    ADate := ACurrentScaleInfo.Scale.TruncDate(AVisibleRangeMinValue);
    AMinFormatId := MaxInt;
    while CreateRulerElement(ACurrentScaleInfo, ADate, AElementRect).MaxValue < AVisibleRangeMaxValue do
    begin
      ARulerElement := ACurrentScaleInfo.Elements.Last;
      AElementRect.Right := ViewInfo.GetPositionFromValue(ARulerElement.MaxValue);
      ARulerElement.CalculateBounds(AElementRect);
      if AAutoFormat and (ARulerElement <> ACurrentScaleInfo.Elements.First) then
      begin
        AFormatId := Properties.AutoFormatHelper.ChooseFormat(ARulerElement.MinValue, ViewInfo.GetRulerLabelFont,
          ARulerElement.Bounds.Width - cxMarginsWidth(ARulerElement.GetContentOffsets), ACurrentScaleInfo.Scale);
        AMinFormatId := Min(AMinFormatId, AFormatId);
      end;
      AElementRect.Left := AElementRect.Right;
      AElementRect.Right := ACurrentScaleInfo.Bounds.Right;
      ADate := ARulerElement.MaxValue;
    end;

    if AAutoFormat then
    begin
      if InRange(ACurrentScaleInfo.Elements.Count, 1, 2) then
      begin
        ARulerElement := ACurrentScaleInfo.Elements[0];
        AMinFormatId := Properties.AutoFormatHelper.ChooseFormat(ARulerElement.MinValue, ViewInfo.GetRulerLabelFont,
          ARulerElement.Bounds.Width - cxMarginsWidth(ARulerElement.GetContentOffsets), ACurrentScaleInfo.Scale);
      end;
      for J := 0 to ACurrentScaleInfo.Elements.Count - 1 do
        ACurrentScaleInfo.Elements[J].Text := ACurrentScaleInfo.Scale.GetDisplayText(
          ACurrentScaleInfo.Elements[J].MinValue, AMinFormatId);
    end
    else
      for J := 0 to ACurrentScaleInfo.Elements.Count - 1 do
       ACurrentScaleInfo.Elements[J].Text := ACurrentScaleInfo.Scale.GetDefaultDisplayText(ACurrentScaleInfo.Elements[J].MinValue);
  end;

  ARect := Bounds;
  ARect.Top := Ruler.ScaleInfos.First.Bounds.Top;
  ARect.Bottom := Ruler.ScaleInfos.Last.Bounds.Bottom;
  Ruler.CalculateBounds(ARect);

  ARect := Bounds;
  ARect.Top := Ruler.Bounds.Bottom;
  Content.CalculateBounds(ARect);
  Ruler.Calculate;
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.CreateContentInfo: TdxRangeControlContentViewInfo;
begin
  Result := TdxRangeControlDateTimeHeaderClientContentViewInfo.Create(ViewInfo);
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.CreateRulerInfo: TdxRangeControlRulerViewInfo;
begin
  Result := TdxRangeControlDateTimeHeaderClientRulerViewInfo.Create(ViewInfo);
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.GetContent: TdxRangeControlDateTimeHeaderClientContentViewInfo;
begin
  Result := inherited Content as TdxRangeControlDateTimeHeaderClientContentViewInfo;
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.GetProperties: TdxRangeControlCustomDateTimeHeaderClientProperties;
begin
  Result := Control.ClientProperties as TdxRangeControlCustomDateTimeHeaderClientProperties;
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.GetRuler: TdxRangeControlDateTimeHeaderClientRulerViewInfo;
begin
  Result := inherited Ruler as TdxRangeControlDateTimeHeaderClientRulerViewInfo;
end;

function TdxRangeControlDateTimeHeaderClientViewInfo.GetScaleHeight: Integer;
begin
  Result := inherited GetScaleHeight;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

{ TdxRangeControlDateTimeHeaderClientStyle }

constructor TdxRangeControlDateTimeHeaderClientStyle.Create(AOwner: TPersistent);
begin
  inherited;
  RulerHeaderColor := dxacDefault;
  FRulerHeaderTextColor := clDefault;
  RulerHeaderHotColor := dxacDefault;
end;

procedure TdxRangeControlDateTimeHeaderClientStyle.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlDateTimeHeaderClientStyle;
begin
  inherited;
  if Source is TdxRangeControlDateTimeHeaderClientStyle then
  begin
    ASource := TdxRangeControlDateTimeHeaderClientStyle(Source);
    RulerHeaderColor := ASource.RulerHeaderColor;
    RulerHeaderTextColor := ASource.RulerHeaderTextColor;
    RulerHeaderHotColor := ASource.RulerHeaderHotColor;
  end;
end;

procedure TdxRangeControlDateTimeHeaderClientStyle.SetRulerHeaderColor(Value: TdxAlphaColor);
begin
  if FRulerHeaderColor <> Value then
  begin
    FRulerHeaderColor := Value;
    Changed;
  end;
end;

procedure TdxRangeControlDateTimeHeaderClientStyle.SetRulerHeaderHotColor(Value: TdxAlphaColor);
begin
  if FRulerHeaderHotColor <> Value then
  begin
    FRulerHeaderHotColor := Value;
    Changed;
  end;
end;

procedure TdxRangeControlDateTimeHeaderClientStyle.SetRulerHeaderTextColor(Value: TColor);
begin
  if FRulerHeaderTextColor <> Value then
  begin
    FRulerHeaderTextColor := Value;
    Changed;
  end;
end;

{ TdxRangeControlDateTimeHeaderClientProperties }

constructor TdxRangeControlCustomDateTimeHeaderClientProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoFormatHelper := TdxRangeControlDateTimeAutoFormatHelper;
  FAutoFormatScaleCaptions := True;
  FScaleIntervalMinWidth := 30;
  FVisibleScales := TdxRangeControlDateTimeScaleList.Create;
  Scales.Day.Visible := True;
end;

destructor TdxRangeControlCustomDateTimeHeaderClientProperties.Destroy;
begin
  FreeAndNil(FVisibleScales);
  inherited;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.CheckVisibleRangeMaxInterval;
begin
  RangeControl.ViewInfo.FCheckZoomNeeded := True;
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.CreatePredefinedDateTimeScale(AScales: TdxRangeControlDateTimeScales; AScaleUnit: TdxRangeControlDateTimeScaleUnit): TdxRangeControlDateTimeScale;
begin
  Result := TdxRangeControlPredefinedDateHeaderScale.Create(AScales, AScaleUnit);
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.CreateStyle: TdxRangeControlCustomClientStyle;
begin
  Result := TdxRangeControlDateTimeHeaderClientStyle.Create(Self);
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo;
begin
  Result := TdxRangeControlDateTimeHeaderClientViewInfo.Create(AViewInfo);
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.GetMinSelectedInterval: Variant;
begin
  Result := 1;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.DoAssign(Source: TPersistent);
var
  ASource: TdxRangeControlCustomDateTimeHeaderClientProperties;
begin
  inherited DoAssign(Source);
  if Source is TdxRangeControlCustomDateTimeHeaderClientProperties then
  begin
    ASource := TdxRangeControlCustomDateTimeHeaderClientProperties(Source);
    AutoFormatScaleCaptions := ASource.AutoFormatScaleCaptions;
    AutoFormatHelper := ASource.AutoFormatHelper;
    ScaleIntervalMinWidth := ASource.ScaleIntervalMinWidth;
  end;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.DoUpdateVisibleScales(AValues: TdxRangeControlDateTimeScaleList);

    procedure AddVisibleScale(AScale: TdxRangeControlDateTimeScale);
    begin
      if (AScale <> nil) and AScale.Visible then
        FVisibleScales.Add(AScale);
    end;

var
  AScaleUnit: TdxRangeControlDateTimeScaleUnit;
  I: Integer;
begin
  for AScaleUnit := High(TdxRangeControlDateTimeScaleUnit) downto Low(TdxRangeControlDateTimeScaleUnit) do
    AddVisibleScale(Scales.GetScale(AScaleUnit));
  for I := 0 to Scales.CustomScales.Count - 1 do
    AddVisibleScale(Scales.CustomScales[I]);
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.GetNormalizedVisibleRangeMaxInterval: Double;
var
  AMaxValue: TDateTime;
begin
  Result := 1;
  if not IsRangeEmpty and (FMaxIntervalCount >= 1) then
  begin
    RangeControl.ViewInfo.FCheckZoomNeeded := False;
    AMaxValue := ActualPrimaryScale.IncDate(MinValue, Trunc(FMaxIntervalCount));
    Result := GetNormalizedValue(AMaxValue);
  end;
end;

function TdxRangeControlCustomDateTimeHeaderClientProperties.IsAutoZoomSupported: Boolean;
begin
  Result := True;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.PrimaryScaleChanged;
begin
  CheckVisibleRangeMaxInterval;
  inherited PrimaryScaleChanged;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.ScaleIntervalMinWidthChanged;
begin
  CheckVisibleRangeMaxInterval;
  inherited ScaleIntervalMinWidthChanged;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.UpdateVisibleScales;
begin
  FVisibleScales.Clear;
  DoUpdateVisibleScales(FVisibleScales);
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.ScalesChanged;
begin
  UpdateVisibleScales;
  inherited ScalesChanged;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.SetAutoFormatHelper(
  Value: TdxRangeControlDateTimeAutoFormatHelperClass);
begin
  if FAutoFormatHelper <> Value then
  begin
    FAutoFormatHelper := Value;
    Changed;
  end;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.SetAutoFormatScaleCaptions(Value: Boolean);
begin
  if FAutoFormatScaleCaptions <> Value then
  begin
    FAutoFormatScaleCaptions := Value;
    Changed;
  end;
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.SetMaxValue(Value: Variant);
begin
  CheckVisibleRangeMaxInterval;
  inherited SetMaxValue(Value);
end;

procedure TdxRangeControlCustomDateTimeHeaderClientProperties.SetMinValue(Value: Variant);
begin
  CheckVisibleRangeMaxInterval;
  inherited SetMinValue(Value);
end;

{ TdxCustomRangeControlHitTest }

procedure TdxCustomRangeControlHitTest.Clear;
begin
  FFlags := 0;
  FHitObject := nil;
end;

function TdxCustomRangeControlHitTest.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

procedure TdxCustomRangeControlHitTest.SetBitState(AIndex: Integer;
  AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

{ TdxCustomRangeControlViewInfo }

constructor TdxCustomRangeControlViewInfo.Create(ARangeControl: TdxCustomRangeControl);
begin
  inherited Create;
  FRangeControl := ARangeControl;
  FHitTest := CreateHitTest;
  CreateInfos;
  FCheckZoomNeeded := True;
  FIsClientInfoDirty := True;
end;

destructor TdxCustomRangeControlViewInfo.Destroy;
begin
  FreeAndNil(FZoomScrollBar);
  FreeAndNil(FClientArea);
  FreeAndNil(FErrorInfo);
  FreeAndNil(FClientInfo);
  FreeAndNil(FHitTest);
  FreeAndNil(FContentImage);
  FreeAndNil(FContentMaskImage);
  inherited Destroy;
end;

procedure TdxCustomRangeControlViewInfo.CalculateBounds(const ABounds: TRect);
var
  R: TRect;
  ANormalizedVisibleRangeMin, ANormalizedVisibleRangeMax: Double;
  AFactor: Double;
  AStartPos: Integer;
begin
  FBounds := ABounds;
  if cxRectIsEmpty(FBounds) then
    Exit;
  FError := (ClientProperties = nil) or ClientProperties.IsRangeEmpty;
  if FError then
  begin
    FErrorInfo.Text := cxGetResourceString(@sdxRangeControlRangeIsEmpty);
    FErrorInfo.CalculateBounds(FBounds);
  end
  else
  begin
    R := FBounds;
    AFactor := R.Width - 1;
    if AFactor <= 0 then
      Exit;
    AStartPos := R.Left;
    ANormalizedVisibleRangeMin := RangeControl.NormalizedVisibleRangeMinValue;
    ANormalizedVisibleRangeMax := RangeControl.NormalizedVisibleRangeMaxValue;
    FMinZoomScrollBarVisiblePos := AStartPos + Round(ANormalizedVisibleRangeMin * AFactor);
    FMaxZoomScrollBarVisiblePos := AStartPos + Round(ANormalizedVisibleRangeMax * AFactor);
    FMinZoomScrollBarSelectedPos := AStartPos + Round(FRangeControl.FNormalizedSelectedEditingMinValue * AFactor);
    FMaxZoomScrollBarSelectedPos := AStartPos + Round(FRangeControl.FNormalizedSelectedEditingMaxValue * AFactor);
    FClientAreaNormalFactor := AFactor / (ANormalizedVisibleRangeMax - ANormalizedVisibleRangeMin);
    FMinClientSelectedPos := GetPositionInClientAreaFromNormalValue(FRangeControl.FNormalizedSelectedEditingMinValue);
    FMaxClientSelectedPos := GetPositionInClientAreaFromNormalValue(FRangeControl.FNormalizedSelectedEditingMaxValue);
    if PressedInfo = ClientArea then
    begin
      FMinClientSelectedTargetPos := GetPositionInClientAreaFromNormalValue(
        Min(FRangeControl.FStartSelectionTargetIntervalMinValue, FRangeControl.FEndSelectionTargetIntervalMinValue));
      FMaxClientSelectedTargetPos := GetPositionInClientAreaFromNormalValue(
        Max(FRangeControl.FStartSelectionTargetIntervalMaxValue, FRangeControl.FEndSelectionTargetIntervalMaxValue));
    end;

    R.Top := R.Bottom - IfThen(RangeControl.ShowZoomScrollBar, Painter.GetRangeControlScaledScrollAreaHeight(RangeControl.ScaleFactor));
    FZoomScrollBar.CalculateBounds(R);
    R := FBounds;
    R.Bottom := FZoomScrollBar.Bounds.Top;

    if FIsClientInfoDirty then
    begin
      FClientInfo.CalculateBounds(R);
      FIsClientInfoDirty := False;
      FIsClientInfoCacheValid := False;
    end;
    FClientArea.CalculateBounds(FClientInfo.Content.Bounds);
    CalculateHitTest(RangeControl.ScreenToClient(cxControls.GetMouseCursorPos), KeyboardStateToShiftState);
    HotInfo := HitTest.HitObject;
  end;
end;

procedure TdxCustomRangeControlViewInfo.CalculateHitTest(const APoint: TPoint; AShift: TShiftState);
begin
  FHitTest.Clear;
  FHitTest.HitPoint := APoint;
  if not FError then
    if not FZoomScrollBar.GetHitTest(FHitTest) then
      if not FClientArea.GetHitTest(FHitTest) then
        Ruler.GetHitTest(FHitTest);
end;

procedure TdxCustomRangeControlViewInfo.Paint(ACanvas: TcxCanvas);

const
  GrayScaleFilter: TdxGpColorMatrix =
    ((0.3, 0.3, 0.3, 0, 0),
     (0.59, 0.59, 0.59, 0, 0),
     (0.11, 0.11, 0.11, 0, 0),
     (0, 0, 0, 0.35, 0),
     (0, 0, 0, 0, 1));

  function ApplyColorMatrix(AImage: TdxSmartImage;
    AColorMatrix: TdxGpColorMatrix): TdxSmartImage;
  var
    AAttributes: TdxGPImageAttributes;
    AGpCanvas: TdxGPCanvas;
  begin
    Result := TdxSmartImage.CreateSize(AImage.ClientRect);
    AAttributes := TdxGPImageAttributes.Create;
    try
      AAttributes.SetColorMatrix(@AColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
      AGpCanvas := Result.CreateCanvas;
      try
        AGpCanvas.Draw(AImage, AImage.ClientRect, AAttributes);
      finally
        AGpCanvas.Free;
      end;
    finally
      AAttributes.Free;
    end;
  end;

  function CreateContentCache(const ABounds: TRect): TdxSmartImage;
  var
    ABitmap: TcxAlphaBitmap;
    ACanvas: TcxCanvas;
  begin
    ABitmap := TcxAlphaBitmap.CreateSize(ABounds);
    ABitmap.Canvas.Lock;
    try
      ACanvas := ABitmap.cxCanvas;
      dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ABitmap.ClientRect);
      dxGPPaintCanvas.FillRectangle(ABitmap.ClientRect, dxColorToAlphaColor(GetBackgroundColor));
      dxGPPaintCanvas.EndPaint;
      ACanvas.WindowOrg := ABounds.TopLeft;
      if not RangeControl.DoOnDrawContent(ACanvas, FClientInfo) then
        FClientInfo.DrawContent(ACanvas);
      ABitmap.SetAlphaChannel($FF);
      Result := TdxSmartImage.CreateFromBitmap(ABitmap);
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;

var
  R: TRect;
begin
  ACanvas.FillRect(FBounds, GetBackgroundColor);

  if FError then
    FErrorInfo.Paint(ACanvas)
  else
  begin
    if RangeControl.ShowZoomScrollBar then
      FZoomScrollBar.Paint(ACanvas);

    if not cxRectIsEmpty(FClientArea.Bounds) then
    begin
      if not FIsClientInfoCacheValid then
      begin
        FreeAndNil(FContentImage);
        FreeAndNil(FContentMaskImage);
        FContentImage := CreateContentCache(FClientArea.Bounds);
        FContentMaskImage := ApplyColorMatrix(FContentImage, GrayScaleFilter);
        FIsClientInfoCacheValid := True;
      end;

      R := FClientArea.Bounds;
      R.Left := FClientArea.SelectedRange.Bounds.Left;
      R.Right := FClientArea.SelectedRange.Bounds.Right;
      if not cxRectIntersect(R, R, FClientArea.Bounds) or (R.Width < FClientArea.Bounds.Width) then
      begin
          dxGPPaintCanvas.BeginPaint(ACanvas.Handle, FClientArea.Bounds);
          try
            if R.Width > 0 then
              dxGPPaintCanvas.SetClipRect(R, gmExclude);
            dxGPPaintCanvas.Draw(FContentMaskImage, FClientArea.Bounds, FContentMaskImage.ClientRect);
          finally
            dxGPPaintCanvas.EndPaint;
          end;
      end;
      if R.Width > 0 then
      begin
        dxGPPaintCanvas.BeginPaint(ACanvas.Handle, FClientArea.Bounds);
        try
          dxGPPaintCanvas.SetClipRect(R, gmIntersect);
          dxGPPaintCanvas.Draw(FContentImage, FClientArea.Bounds, FContentImage.ClientRect);
        finally
          dxGPPaintCanvas.EndPaint;
        end;
      end;
    end;

    if RangeControl.ShowRuler then
      if not RangeControl.DoOnDrawScale(ACanvas, FClientInfo) then
        FClientInfo.DrawScale(ACanvas);

    if not cxRectIsEmpty(FClientArea.Bounds) then
      FClientArea.Paint(ACanvas);
  end;

  if not RangeControl.Enabled then
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, FBounds);
    try
      dxGPPaintCanvas.FillRectangle(FBounds, Style.DisabledColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxCustomRangeControlViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := RangeControl.LookAndFeelPainter;
end;

function TdxCustomRangeControlViewInfo.GetPositionFromValue(const AValue: Variant): Integer;
begin
  Result := GetPositionInClientAreaFromNormalValue(ClientProperties.GetNormalizedValue(AValue));
end;

procedure TdxCustomRangeControlViewInfo.CheckCursorAtPoint(const APoint: TPoint; var ACursor: TCursor);
begin
  CalculateHitTest(APoint, []);
  if FHitTest.HitAtZoomScrollBarMinZoomGrip or
    FHitTest.HitAtZoomScrollBarMaxZoomGrip or
    FHitTest.HitAtClientAreaMinSelectionThumb or
    FHitTest.HitAtClientAreaMaxSelectionThumb then
    ACursor := crHSplit;
end;

function TdxCustomRangeControlViewInfo.CreateHitTest: TdxCustomRangeControlHitTest;
begin
  Result := TdxCustomRangeControlHitTest.Create;
end;

procedure TdxCustomRangeControlViewInfo.CreateInfos;
begin
  FErrorInfo := TdxRangeControlErrorInfo.Create(Self);
  FClientArea := TdxRangeControlClientAreaViewInfo.Create(Self);
  FZoomScrollBar := TdxRangeControlZoomScrollBarViewInfo.Create(Self);
end;

function TdxCustomRangeControlViewInfo.GetBackgroundColor: TColor;
begin
  if Style.Color <> clDefault then
    Result := Style.Color
  else
    Result := Painter.GetRangeControlBackColor;
end;

function TdxCustomRangeControlViewInfo.GetClientInfoTextFont: TFont;
begin
  Result := RangeControl.Font;
end;

procedure TdxCustomRangeControlViewInfo.RecreateClientInfo;
begin
  FreeAndNil(FClientInfo);
  if ClientProperties <> nil then
    FClientInfo := ClientProperties.CreateViewInfo(Self);
end;

function TdxCustomRangeControlViewInfo.GetClientProperties: TdxRangeControlCustomClientProperties;
begin
  Result := FRangeControl.ClientProperties;
end;

function TdxCustomRangeControlViewInfo.GetElementBorderColor: TdxAlphaColor;
begin
  if Style.ElementBorderColor = dxacDefault then
    Result := Painter.GetRangeControlElementsBorderColor
  else
    Result := Style.ElementBorderColor;
end;

function TdxCustomRangeControlViewInfo.GetPositionInClientAreaFromNormalValue(const AValue: Double): Integer;
begin
  Result := Round(FBounds.Left +
    (AValue - RangeControl.NormalizedVisibleRangeMinValue) * FClientAreaNormalFactor);
end;

function TdxCustomRangeControlViewInfo.GetRuler: TdxRangeControlRulerViewInfo;
begin
  if FClientInfo <> nil then
    Result := FClientInfo.Ruler
  else
    Result := nil;
end;

function TdxCustomRangeControlViewInfo.GetRulerLabelFont: TFont;
begin
  Result := RangeControl.Font;
end;

function TdxCustomRangeControlViewInfo.GetSelectedRegionBackgroundColor: TdxAlphaColor;
begin
  if Style.SelectionColor <> dxacDefault then
    Result := Style.SelectionColor
  else
    Result := Painter.GetRangeControlSelectedRegionBackgroundColor;
end;

function TdxCustomRangeControlViewInfo.GetSelectedRegionBorderColor: TdxAlphaColor;
begin
  if Style.SelectionBorderColor <> dxacDefault then
    Result := Style.SelectionBorderColor
  else
    Result := Painter.GetRangeControlSelectedRegionBorderColor;
end;

function TdxCustomRangeControlViewInfo.GetSelectedRangeThumbColor: TColor;
begin
  Result := Style.SelectedRangeThumbColor;
end;

function TdxCustomRangeControlViewInfo.GetStyle: TdxCustomRangeControlStyle;
begin
  Result := RangeControl.Style;
end;

{ TdxRangeControlBaseStyle }

procedure TdxRangeControlBaseStyle.Assign(Source: TPersistent);
begin
  RangeControl.BeginUpdate;
  try
    inherited Assign(Source);
  finally
    RangeControl.EndUpdate;
  end;
end;

procedure TdxRangeControlBaseStyle.Changed;
begin
  RangeControl.Changed;
end;

{ TdxCustomRangeControlStyle }

constructor TdxCustomRangeControlStyle.Create(AOwner: TPersistent);
begin
  inherited;
  FBorderColor := clDefault;
  FColor := clDefault;
  FDisabledColor := $A000000;
  FElementBorderColor := dxacDefault;
  FOutOfRangeColor := dxacDefault;
  FSelectedRangeThumbColor := clDefault;
  FSelectionBorderColor := dxacDefault;
  FSelectionColor := dxacDefault;
  FZoomScrollBarColor := clDefault;
  FZoomScrollBarSelectedRangeThumbColor := clDefault;
  FZoomScrollBarVisibleRangeThumbColor := clDefault;
end;

procedure TdxCustomRangeControlStyle.DoAssign(Source: TPersistent);
var
  ASource: TdxCustomRangeControlStyle;
begin
  inherited;
  if Source is TdxCustomRangeControlStyle then
  begin
    ASource := TdxCustomRangeControlStyle(Source);
    BorderColor := ASource.BorderColor;
    Color := ASource.Color;
    DisabledColor := ASource.DisabledColor;
    ElementBorderColor := ASource.ElementBorderColor;
    OutOfRangeColor := ASource.OutOfRangeColor;
    SelectedRangeThumbColor := ASource.SelectedRangeThumbColor;
    SelectionBorderColor := ASource.SelectionBorderColor;
    SelectionColor := ASource.SelectionColor;
    ZoomScrollBarColor := ASource.ZoomScrollBarColor;
    ZoomScrollBarSelectedRangeThumbColor := ASource.ZoomScrollBarSelectedRangeThumbColor;
    ZoomScrollBarVisibleRangeThumbColor := ASource.ZoomScrollBarVisibleRangeThumbColor;
  end;
end;

procedure TdxCustomRangeControlStyle.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    RangeControl.ViewInfo.FIsClientInfoCacheValid := False;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetDisabledColor(Value: TdxAlphaColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetElementBorderColor(Value: TdxAlphaColor);
begin
  if FElementBorderColor <> Value then
  begin
    FElementBorderColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetOutOfRangeColor(Value: TdxAlphaColor);
begin
  if FOutOfRangeColor <> Value then
  begin
    FOutOfRangeColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetSelectedRangeThumbColor(Value: TColor);
begin
  if FSelectedRangeThumbColor <> Value then
  begin
    FSelectedRangeThumbColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetSelectionBorderColor(Value: TdxAlphaColor);
begin
  if FSelectionBorderColor <> Value then
  begin
    FSelectionBorderColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetSelectionColor(Value: TdxAlphaColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetZoomScrollBarColor(Value: TColor);
begin
  if FZoomScrollBarColor <> Value then
  begin
    FZoomScrollBarColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetZoomScrollBarSelectedRangeThumbColor(Value: TColor);
begin
  if FZoomScrollBarSelectedRangeThumbColor <> Value then
  begin
    FZoomScrollBarSelectedRangeThumbColor := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeControlStyle.SetZoomScrollBarVisibleRangeThumbColor(Value: TColor);
begin
  if FZoomScrollBarVisibleRangeThumbColor <> Value then
  begin
    FZoomScrollBarVisibleRangeThumbColor := Value;
    Changed;
  end;
end;

{ TdxCustomRangeControl }

constructor TdxCustomRangeControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubclasses;
  Width := 420;
  Height := 90;
  BorderStyle := cxcbsDefault;
  FAnimation := True;
  FNormalizedVisibleRangeMaxValue := 1;
  FNormalizedVisibleRangeMinInterval := 0.1;
  FNormalizedVisibleRangeMaxInterval := 1;
  StoreVisibleRangeInterval;
  FShowRuler := True;
  FShowZoomScrollBar := True;
  Keys := [kArrows];
end;

destructor TdxCustomRangeControl.Destroy;
begin
  Client := nil;
  ClientPropertiesClass := nil;
  DestroySubclasses;
  inherited;
end;

function TdxCustomRangeControl.AllowAnimation: Boolean;
begin
  Result := Animation and not IsDesigning;
end;

procedure TdxCustomRangeControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomRangeControl.EndUpdate(AForceUpdate: Boolean = True);
begin
  Dec(FLockCount);
  if (FLockCount = 0) and AForceUpdate then
    Changed;
end;

procedure TdxCustomRangeControl.FontChanged;
begin
  inherited FontChanged;
  ViewInfo.FIsClientInfoDirty := True;
  Changed;
end;

procedure TdxCustomRangeControl.BoundsChanged;
begin
  ViewInfo.FCheckZoomNeeded := True;
  ViewInfo.FIsClientInfoDirty := True;
  Changed;
end;

procedure TdxCustomRangeControl.SetNormalizedSelectedRange(const AMin, AMax: Double; AChangeType: TdxRangeElementTypes; AAnimated: Boolean);
var
  APrevMaxValue, APrevMinValue: Double;
begin
  APrevMinValue := FNormalizedSelectedRangeMinValue;
  APrevMaxValue := FNormalizedSelectedRangeMaxValue;
  if retMin in AChangeType then
    SetNormalizedSelectedRangeByMinValue(AMin);
  if retMax in AChangeType then
    SetNormalizedSelectedRangeByMaxValue(AMax);
  SynchronizeSelectedRangeEditingValue(AAnimated);
  if (APrevMinValue <> FNormalizedSelectedRangeMinValue) or
    (APrevMaxValue <> FNormalizedSelectedRangeMaxValue) then
    NotifyClient;
  Changed;
end;

procedure TdxCustomRangeControl.SetNormalizedSelectedRangeByMaxValue(AValue: Double);
var
  AIsOverflow: Boolean;
begin
  AValue := EnsureRange(AValue, ClientProperties.GetNextNormalizedAcceptableValue(0, ClientProperties.GetMinSelectedInterval, AIsOverflow), 1);
  if FNormalizedSelectedRangeMaxValue <> AValue then
  begin
    FNormalizedSelectedRangeMaxValue := AValue;
    FNormalizedSelectedRangeMinValue := EnsureRange(FNormalizedSelectedRangeMinValue,
      0, ClientProperties.GetNextNormalizedAcceptableValue(FNormalizedSelectedRangeMaxValue, -ClientProperties.GetMinSelectedInterval, AIsOverflow));
  end;
end;

procedure TdxCustomRangeControl.SetNormalizedSelectedRangeByMinValue(AValue: Double);
var
  AIsOverflow: Boolean;
begin
  AValue := EnsureRange(AValue, 0, ClientProperties.GetNextNormalizedAcceptableValue(1, -ClientProperties.GetMinSelectedInterval, AIsOverflow));
  if FNormalizedSelectedRangeMinValue <> AValue then
  begin
    FNormalizedSelectedRangeMinValue := AValue;
    FNormalizedSelectedRangeMaxValue := EnsureRange(FNormalizedSelectedRangeMaxValue,
      ClientProperties.GetNextNormalizedAcceptableValue(FNormalizedSelectedRangeMinValue, ClientProperties.GetMinSelectedInterval, AIsOverflow), 1);
  end;
end;

procedure TdxCustomRangeControl.CheckChanges;
begin
  if FIsViewInfoDirty then
    Changed;
end;

procedure TdxCustomRangeControl.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Paint(Canvas);
end;

procedure TdxCustomRangeControl.InitControl;
begin
  inherited;
  CheckChanges;
end;

function TdxCustomRangeControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxCustomRangeControl.Loaded;
begin
  inherited Loaded;
  CheckChanges;
end;

procedure TdxCustomRangeControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  ViewInfo.FIsClientInfoDirty := True;
  Changed;
end;

procedure TdxCustomRangeControl.Changed;
begin
  if IsLocked then
    FIsViewInfoDirty := True
  else
  begin
    ViewInfo.CalculateBounds(Bounds);
    if (ClientProperties <> nil) and ClientProperties.IsAutoZoomSupported and
      ViewInfo.FCheckZoomNeeded then
      UpdateVisibleRangeMaxInterval(ClientProperties.GetNormalizedVisibleRangeMaxInterval);
    FIsViewInfoDirty := False;
    Invalidate;
  end;
end;

procedure TdxCustomRangeControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if ClientProperties <> nil then
    ClientProperties.ChangeScale(M, D);
end;

procedure TdxCustomRangeControl.ClientPropertiesChanged;
begin
  ViewInfo.FIsClientInfoDirty := True;
  Changed;
end;

procedure TdxCustomRangeControl.ContentChanged;
begin
  ViewInfo.FIsClientInfoDirty := True;
  Changed;
end;

function TdxCustomRangeControl.CreateStyle: TdxRangeControlStyle;
begin
  Result := TdxRangeControlStyle.Create(Self);
end;

procedure TdxCustomRangeControl.CreateSubclasses;
begin
  FStyle := CreateStyle;
  FViewInfo := CreateViewInfo;
end;

function TdxCustomRangeControl.CreateViewInfo: TdxCustomRangeControlViewInfo;
begin
  Result := TdxCustomRangeControlViewInfo.Create(Self);
end;

procedure TdxCustomRangeControl.DestroySubclasses;
begin
  FreeAndNil(FSelectedRangeAnimationController);
  FreeAndNil(FVisibleRangeAnimationController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FStyle);
end;

procedure TdxCustomRangeControl.DoCancelMode;
begin
  inherited DoCancelMode;
  DoInternalCancelMode;
end;

procedure TdxCustomRangeControl.DoInternalCancelMode;
begin
  ViewInfo.Dragging := False;
  FNormalizedSelectedEditingMinValue := FNormalizedSelectedRangeMinValue;
  FNormalizedSelectedEditingMaxValue := FNormalizedSelectedRangeMaxValue;
  Changed;
  ViewInfo.PressedInfo := nil;
end;

procedure TdxCustomRangeControl.DoSelectedRangeAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  if FNormalizedSelectedEditingMinValue <> FNormalizedSelectedRangeMinValue then
    FNormalizedSelectedEditingMinValue := FSelectedRangeAnimationNormalizedMinStartValue +
      (FNormalizedSelectedRangeMinValue - FSelectedRangeAnimationNormalizedMinStartValue) * APosition / FSelectedRangeAnimationLength;
  if FNormalizedSelectedEditingMaxValue <> FNormalizedSelectedRangeMaxValue then
    FNormalizedSelectedEditingMaxValue := FSelectedRangeAnimationNormalizedMaxStartValue +
      (FNormalizedSelectedRangeMaxValue - FSelectedRangeAnimationNormalizedMaxStartValue) * APosition / FSelectedRangeAnimationLength;
  Changed;
end;

procedure TdxCustomRangeControl.DoSelectedRangeAnimationTerminate(Sender: TObject);
begin
  FSelectedRangeAnimationActive := False;
  Invalidate;
end;

function TdxCustomRangeControl.DoOnDrawContent(ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo): Boolean;
begin
  Result := Assigned(FOnDrawContent);
  if Result then
    FOnDrawContent(Self, ACanvas, AViewInfo, Result);
end;

function TdxCustomRangeControl.DoOnDrawScale(ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo): Boolean;
begin
  Result := Assigned(FOnDrawScale);
  if Result then
    FOnDrawScale(Self, ACanvas, AViewInfo, Result);
end;

procedure TdxCustomRangeControl.DoVisibleRangeAnimate(Sender: TdxAnimationTransition;
  var APosition: Integer; var AFinished: Boolean);
var
  AInterval: Double;
begin
  AInterval := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
  BeginUpdate;
  try
    NormalizedVisibleRangeMinValue := FVisibleRangeAnimationNormalizedMinStartValue +
      (FVisibleRangeAnimationNormalizedMinFinishValue - FVisibleRangeAnimationNormalizedMinStartValue) * APosition / FVisibleRangeAnimationLength;
    NormalizedVisibleRangeMaxValue := NormalizedVisibleRangeMinValue + AInterval;
  finally
    EndUpdate(FIsViewInfoDirty);
  end;
end;

procedure TdxCustomRangeControl.DoVisibleRangeAnimationTerminate(Sender: TObject);
begin
  FVisibleRangeAnimationActive := False;
  Invalidate;
end;

procedure TdxCustomRangeControl.DrawBorder(ACanvas: TcxCanvas);
var
  AColor: TColor;
begin
  if Style.BorderColor = clDefault then
    AColor := LookAndFeelPainter.GetRangeControlBorderColor
  else
    AColor := Style.BorderColor;
  ACanvas.FrameRect(Bounds,  AColor);
end;

function TdxCustomRangeControl.GetBorderSize: Integer;
begin
  Result := IfThen(BorderStyle = cxcbsDefault, 1);
end;

function TdxCustomRangeControl.GetClient: IdxRangeControlClient;
begin
  Supports(FClient, IdxRangeControlClient, Result);
end;

function TdxCustomRangeControl.GetClientPropertiesClassName: string;
begin
  if FClientPropertiesClass <> nil then
    Result := FClientPropertiesClass.ClassName
  else
    Result := '';
end;

function TdxCustomRangeControl.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := inherited GetCurrentCursor(X, Y);
  if not IsDesigning then
    ViewInfo.CheckCursorAtPoint(cxPoint(X, Y), Result);
end;

function TdxCustomRangeControl.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading or IsDestroying or not HandleAllocated;
end;

function TdxCustomRangeControl.IsRangeValueStored: Boolean;
begin
  Result := ClientProperties <> nil;
end;

procedure TdxCustomRangeControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function CanDrag: Boolean;
  begin
    Result := HitTest.HitAtClientAreaMinSelectionThumb or
      HitTest.HitAtClientAreaMaxSelectionThumb or
      HitTest.HitAtClientAreaSelectedRange or
      HitTest.HitAtZoomScrollBarMinZoomGrip or
      HitTest.HitAtZoomScrollBarMaxZoomGrip or
      HitTest.HitAtZoomScrollBarSelectionScrollThumb or
      HitTest.HitAtZoomScrollBarViewportScrollThumb;
  end;

var
  AIsOverflow: Boolean;
begin
  inherited;
  if FVisibleRangeAnimationActive then
    StopVisibleRangeAnimation;
  ViewInfo.CalculateHitTest(Point(X, Y), Shift);
  ViewInfo.HotInfo := HitTest.HitObject;
  if Button <> mbLeft then
    Exit;
  ViewInfo.PressedInfo := HitTest.HitObject;
  if CanDrag then
    FDragPointOffset := X - ViewInfo.PressedInfo.Bounds.Left;
  FStoreSelectedRangeInterval := HitTest.HitAtClientAreaSelectedRange or
    HitTest.HitAtZoomScrollBarSelectionScrollThumb;
  if HitTest.HitAtClientArea or HitTest.HitAtRulerElement then
  begin
    GetNormalizedIntervalFromPosInClientArea(X,
      FStartSelectionTargetIntervalMinValue, FStartSelectionTargetIntervalMaxValue);
    if ClientProperties.GetMinSelectedInterval > 1 then
    begin
      FStartSelectionTargetIntervalMaxValue := ClientProperties.GetNextNormalizedAcceptableValue(
        FStartSelectionTargetIntervalMinValue, ClientProperties.GetMinSelectedInterval, AIsOverflow);
      if AIsOverflow then
        FStartSelectionTargetIntervalMinValue := ClientProperties.GetNextNormalizedAcceptableValue(
          FStartSelectionTargetIntervalMaxValue, -ClientProperties.GetMinSelectedInterval, AIsOverflow);
    end;
    FEndSelectionTargetIntervalMinValue := FStartSelectionTargetIntervalMinValue;
    FEndSelectionTargetIntervalMaxValue := FStartSelectionTargetIntervalMaxValue;
    if HitTest.HitAtClientArea then
      Changed;
  end
  else
    if ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar then
      SetViewportPosition(X, True);
  Invalidate;
end;

procedure TdxCustomRangeControl.MouseEnter(AControl: TControl);
begin
  inherited;

end;

procedure TdxCustomRangeControl.MouseLeave(AControl: TControl);
begin
  inherited;
  ViewInfo.HotInfo := nil;
  Invalidate;
end;

procedure TdxCustomRangeControl.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure DoDragging(Shift: TShiftState; X, Y: Integer);
  var
    AMax, AMin: Double;
    ARangeInterval: Double;
  begin
    if ViewInfo.PressedInfo = ViewInfo.ClientArea then
    begin
      GetNormalizedIntervalFromPosInClientArea(X,
        FEndSelectionTargetIntervalMinValue, FEndSelectionTargetIntervalMaxValue);
      Changed;
    end
    else
      if ViewInfo.PressedInfo = ViewInfo.ClientArea.MaxSelectionThumb then
      begin
        FNormalizedSelectedEditingMaxValue := EnsureRange(GetNormalizedValueFromPositionInClientArea(X), FNormalizedSelectedEditingMinValue, 1);
        Changed;
      end
      else
        if ViewInfo.PressedInfo = ViewInfo.ClientArea.MinSelectionThumb then
        begin
          FNormalizedSelectedEditingMinValue := EnsureRange(GetNormalizedValueFromPositionInClientArea(X), 0, FNormalizedSelectedEditingMaxValue);
          Changed;
        end
        else
          if ViewInfo.PressedInfo = ViewInfo.ClientArea.SelectedRange then
          begin
            ARangeInterval := FNormalizedSelectedEditingMaxValue - FNormalizedSelectedEditingMinValue;
            FNormalizedSelectedEditingMinValue := EnsureRange(GetNormalizedValueFromPositionInClientArea(X - FDragPointOffset), 0, 1 - ARangeInterval);
            FNormalizedSelectedEditingMaxValue := FNormalizedSelectedEditingMinValue + ARangeInterval;
            Changed;
          end
          else
            if ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.ViewportScrollThumb then
              SetViewportPosition(X - FDragPointOffset, False)
            else
              if ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.MinZoomGrip then
              begin
                AMin := EnsureRange(GetNormalizedValueFromPositionInZoomScrollBarArea(X - FDragPointOffset),
                  Max(0, NormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMaxInterval),
                  NormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinInterval);
                NormalizedVisibleRangeMinValue := AMin;
                StoreVisibleRangeInterval;
              end
              else
                if ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.MaxZoomGrip then
                begin
                  AMax := EnsureRange(GetNormalizedValueFromPositionInZoomScrollBarArea(X - FDragPointOffset),
                    NormalizedVisibleRangeMinValue + FNormalizedVisibleRangeMinInterval,
                    Min(1, NormalizedVisibleRangeMinValue + FNormalizedVisibleRangeMaxInterval));
                  NormalizedVisibleRangeMaxValue := AMax;
                  StoreVisibleRangeInterval;
                end
                else
                  if ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.SelectionScrollThumb then
                  begin
                    ARangeInterval := FNormalizedSelectedEditingMaxValue - FNormalizedSelectedEditingMinValue;
                    FNormalizedSelectedEditingMinValue := EnsureRange(GetNormalizedValueFromPositionInZoomScrollBarArea(X - FDragPointOffset), 0, 1 - ARangeInterval);
                    FNormalizedSelectedEditingMaxValue := FNormalizedSelectedEditingMinValue + ARangeInterval;
                    Changed;
                  end;
  end;

  function CanStartDragging(Shift: TShiftState; X, Y: Integer): Boolean;
  begin
    Result := (ViewInfo.PressedInfo <> nil) and (Abs(X - MouseDownPos.X) > 3); // check ViewInfo.PressedInfo.CanDrag
  end;

  procedure StartDragging(Shift: TShiftState; X, Y: Integer);
  begin
    ViewInfo.Dragging := True;
    if (ViewInfo.PressedInfo = ViewInfo.ClientArea.MinSelectionThumb) and
      ((X - FDragPointOffset) >= (ViewInfo.ClientArea.MaxSelectionThumb.Bounds.Left)) then
      ViewInfo.PressedInfo := ViewInfo.ClientArea.MaxSelectionThumb
    else
      if (ViewInfo.PressedInfo = ViewInfo.ClientArea.MaxSelectionThumb) and
        ((X - FDragPointOffset) <= (ViewInfo.ClientArea.MinSelectionThumb.Bounds.Left)) then
        ViewInfo.PressedInfo := ViewInfo.ClientArea.MinSelectionThumb;
    if FSelectedRangeAnimationActive and
    ((ViewInfo.PressedInfo = ViewInfo.ClientArea.MinSelectionThumb) or
    (ViewInfo.PressedInfo = ViewInfo.ClientArea.MaxSelectionThumb) or
    (ViewInfo.PressedInfo = ViewInfo.ClientArea.SelectedRange) or
    (ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.SelectionScrollThumb)) then
    begin
      StopSelectedRangeAnimation;
      FStoreSelectedRangeInterval := False;
    end;
  end;

var
  AInfo: TdxRangeControlElementViewInfo;
begin
  inherited;
  ViewInfo.CalculateHitTest(Point(X, Y), Shift);
  AInfo := ViewInfo.HotInfo;
  ViewInfo.HotInfo := HitTest.HitObject;
  if ViewInfo.Dragging then
    DoDragging(Shift, X, Y)
  else
    if CanStartDragging(Shift, X, Y) then
      StartDragging(Shift, X, Y);
  if ViewInfo.HotInfo <> AInfo then
  begin
    if (AInfo <> nil) and AInfo.IsHotTrackSupported then
      InvalidateRect(AInfo.Bounds, True);
    if (ViewInfo.HotInfo <> nil) and ViewInfo.HotInfo.IsHotTrackSupported then
      InvalidateRect(ViewInfo.HotInfo.Bounds, True);
  end;
end;

procedure TdxCustomRangeControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure SetSelectionTargetIntervalAsSelectedRange;
  var
    AMin, AMax: Double;
  begin
    AMin := Min(FStartSelectionTargetIntervalMinValue, FEndSelectionTargetIntervalMinValue);
    AMax := Max(FStartSelectionTargetIntervalMaxValue, FEndSelectionTargetIntervalMaxValue);
    AMin := ClientProperties.GetNearestNormalizedAcceptableValue(AMin);
    AMax := ClientProperties.GetNearestNormalizedAcceptableValue(AMax);
    SetNormalizedSelectedRange(AMin, AMax, [retMin, retMax], True);
  end;

var
  AValue: Double;
  AInterval: Variant;
  AMin, AMax: Double;
  AIsOverflow: Boolean;
  ANearestValue: Double;
begin
  inherited;
  ViewInfo.CalculateHitTest(Point(X, Y), Shift);
  ViewInfo.HotInfo := HitTest.HitObject;
  if ViewInfo.Dragging then
  begin
    ViewInfo.Dragging := False;
    if (ViewInfo.PressedInfo = ViewInfo.ClientArea.MaxSelectionThumb) then
    begin
      AMin := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMinValue);
      AValue := ClientProperties.GetNextNormalizedAcceptableValue(AMin, ClientProperties.GetMinSelectedInterval, AIsOverflow);
      if FNormalizedSelectedEditingMaxValue < AValue then
        AMax := AValue
      else
        AMax := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMaxValue);
      SetNormalizedSelectedRange(AMin, AMax, [retMin, retMax], False)
    end
    else
      if ViewInfo.PressedInfo = ViewInfo.ClientArea.MinSelectionThumb then
      begin
        AMax := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMaxValue);
        AValue := ClientProperties.GetNextNormalizedAcceptableValue(AMax, -ClientProperties.GetMinSelectedInterval, AIsOverflow);
        if FNormalizedSelectedEditingMinValue > AValue then
          AMin := AValue
        else
          AMin := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMinValue);
        SetNormalizedSelectedRange(AMin, AMax, [retMin, retMax], False);
      end
      else
        if (ViewInfo.PressedInfo = ViewInfo.ClientArea.SelectedRange) or
          (ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.SelectionScrollThumb) then
        begin
          AMin := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMinValue);
          if FStoreSelectedRangeInterval then
          begin
            AInterval := ClientProperties.GetInterval(FNormalizedSelectedRangeMinValue, FNormalizedSelectedRangeMaxValue);
            AMax := ClientProperties.GetNextNormalizedAcceptableValue(FNormalizedSelectedEditingMinValue, AInterval, AIsOverflow);
            if AIsOverflow then
              AMin := ClientProperties.GetNextNormalizedAcceptableValue(AMax, -AInterval, AIsOverflow);
          end
          else
            AMax := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMaxValue);
          SetNormalizedSelectedRange(AMin, AMax, [retMin, retMax], False);
        end;
  end
  else
    if ViewInfo.PressedInfo = ViewInfo.ClientArea.SelectedRange then
      SetSelectionTargetIntervalAsSelectedRange;

  if (ViewInfo.PressedInfo = ViewInfo.ClientArea) then
    SetSelectionTargetIntervalAsSelectedRange;
  if ViewInfo.PressedInfo is TdxRangeControlScaleElementViewInfo then
  begin
    AMin := ClientProperties.GetNormalizedValue(TdxRangeControlScaleElementViewInfo(ViewInfo.PressedInfo).MinValue);
    ANearestValue := ClientProperties.GetNearestNormalizedAcceptableValue(AMin);
    if ANearestValue <= AMin then
      AMin := ANearestValue
    else
      AMin := ClientProperties.GetNextNormalizedAcceptableValue(ANearestValue, -1, AIsOverflow);
    AMax := ClientProperties.GetNormalizedValue(TdxRangeControlScaleElementViewInfo(ViewInfo.PressedInfo).MaxValue);
    ANearestValue := ClientProperties.GetNearestNormalizedAcceptableValue(AMax);
    if ANearestValue >= AMax then
      AMax := ANearestValue
    else
      AMax := ClientProperties.GetNextNormalizedAcceptableValue(ANearestValue, 1, AIsOverflow);
    SetNormalizedSelectedRange(AMin, AMax, [retMin, retMax], True);
  end;
  ViewInfo.PressedInfo := nil;
end;

procedure TdxCustomRangeControl.SynchronizeSelectedRangeEditingValue(AAnimated: Boolean);
begin
  if AAnimated and AllowAnimation then
  begin
    FreeAndNil(FSelectedRangeAnimationController);
    FSelectedRangeAnimationLength := Max(Width, 100);
    FSelectedRangeAnimationController := TdxAnimationTransition.Create(AnimationTime, AnimationTransitionEffect, FSelectedRangeAnimationLength);
    FSelectedRangeAnimationController.OnAnimate := DoSelectedRangeAnimate;
    FSelectedRangeAnimationController.OnTerminate := DoSelectedRangeAnimationTerminate;
    FSelectedRangeAnimationController.FreeOnTerminate := False;
    FSelectedRangeAnimationActive := True;
    FSelectedRangeAnimationNormalizedMinStartValue := FNormalizedSelectedEditingMinValue;
    FSelectedRangeAnimationNormalizedMaxStartValue := FNormalizedSelectedEditingMaxValue;
    FSelectedRangeAnimationController.Resume;
  end
  else
  begin
    FNormalizedSelectedEditingMinValue := FNormalizedSelectedRangeMinValue;
    FNormalizedSelectedEditingMaxValue := FNormalizedSelectedRangeMaxValue;
  end;
end;

function TdxCustomRangeControl.GetHitTest: TdxCustomRangeControlHitTest;
begin
  Result := ViewInfo.HitTest;
end;

procedure TdxCustomRangeControl.GetNormalizedIntervalFromPosInClientArea(APos: Integer; out AMin, AMax: Double);
var
  AValue, AAcceptableValue: Double;
  AIsOverflow: Boolean;
  AInterval: Variant;
begin
   AValue := GetNormalizedValueFromPositionInClientArea(APos);
   AAcceptableValue := ClientProperties.GetNearestNormalizedAcceptableValue(AValue);
   if ClientProperties.GetMinSelectedInterval <> 0 then
     AInterval := 1
   else
     AInterval := 0;
   if AAcceptableValue <= AValue then
   begin
     AMin := AAcceptableValue;
     AMax := ClientProperties.GetNextNormalizedAcceptableValue(AAcceptableValue, AInterval, AIsOverflow);
   end
   else
   begin
     AMax := AAcceptableValue;
     AMin := ClientProperties.GetNextNormalizedAcceptableValue(AAcceptableValue, -AInterval, AIsOverflow);
   end;
end;

function TdxCustomRangeControl.GetNormalizedValueFromPosition(APos: Integer; AScaleAreaViewInfo: TdxRangeControlElementViewInfo; const AMin, AMax: Double): Double;
begin
  Result := AMin + (APos - AScaleAreaViewInfo.Bounds.Left) * (AMax - AMin) / AScaleAreaViewInfo.Bounds.Width;
  Result := EnsureRange(Result, 0, 1);
end;

function TdxCustomRangeControl.GetNormalizedValueFromPositionInClientArea(
  APos: Integer): Double;
begin
  Result := GetNormalizedValueFromPosition(APos, ViewInfo.ClientArea,
    FNormalizedVisibleRangeMinValue, FNormalizedVisibleRangeMaxValue);
end;

function TdxCustomRangeControl.GetNormalizedValueFromPositionInZoomScrollBarArea(
  APos: Integer): Double;
begin
  Result := GetNormalizedValueFromPosition(APos, ViewInfo.ZoomScrollBar, 0, 1);
end;

function TdxCustomRangeControl.GetPositionFromValue(const AValue: Variant): Integer;
begin
  Result := ViewInfo.GetPositionFromValue(AValue);
end;

function TdxCustomRangeControl.GetSelectedRangeMaxValue: Variant;
begin
  if ClientProperties <> nil then
    Result := ClientProperties.GetOriginalValue(FNormalizedSelectedRangeMaxValue)
  else
    Result := Null;
end;

function TdxCustomRangeControl.GetSelectedRangeMinValue: Variant;
begin
  if ClientProperties <> nil then
    Result := ClientProperties.GetOriginalValue(FNormalizedSelectedRangeMinValue)
  else
    Result := Null;
end;

function TdxCustomRangeControl.GetVisibleRangeMaxValue: Variant;
begin
  if ClientProperties <> nil then
    Result := ClientProperties.GetOriginalValue(FNormalizedVisibleRangeMaxValue, ClientProperties.GetVisibleRangeValueType)
  else
    Result := Null;
end;

function TdxCustomRangeControl.GetVisibleRangeMinValue: Variant;
begin
  if ClientProperties <> nil then
    Result := ClientProperties.GetOriginalValue(FNormalizedVisibleRangeMinValue, ClientProperties.GetVisibleRangeValueType)
  else
    Result := Null;
end;

function TdxCustomRangeControl.GetVisibleRangeMaxScaleFactor: Double;
begin
  Result := 1 / FNormalizedVisibleRangeMinInterval;
end;

procedure TdxCustomRangeControl.InitializeRanges;
var
  AIsOverflow: Boolean;
begin
  FNormalizedSelectedEditingMinValue := ClientProperties.GetNearestNormalizedAcceptableValue(FNormalizedSelectedEditingMinValue);
  FNormalizedSelectedEditingMaxValue := ClientProperties.GetNextNormalizedAcceptableValue(
    FNormalizedSelectedEditingMinValue, ClientProperties.GetMinSelectedInterval, AIsOverflow);
  FNormalizedVisibleRangeMaxValue := FNormalizedVisibleRangeMinValue + FNormalizedVisibleRangeMaxInterval;
  FNormalizedSelectedRangeMaxValue := FNormalizedSelectedEditingMaxValue;
  FNormalizedSelectedRangeMinValue := FNormalizedSelectedEditingMinValue;
end;

function TdxCustomRangeControl.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  Factor = 1.2;
var
  ANewInterval, ARangeInterval, ACenter: Double;
begin
  Result := ClientProperties <> nil;
  if Result then
  begin
    StopVisibleRangeAnimation;
    if ssCtrl in Shift then
    begin
      ARangeInterval := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
      BeginUpdate;
      try
        if WheelDelta < 0 then
        begin
          if ARangeInterval < FNormalizedVisibleRangeMaxInterval then
          begin
            ACenter := (FNormalizedVisibleRangeMaxValue + FNormalizedVisibleRangeMinValue) / 2;
            ANewInterval := Min(ARangeInterval * Factor, FNormalizedVisibleRangeMaxInterval);
            NormalizedVisibleRangeMinValue := ACenter - ANewInterval / 2;
            NormalizedVisibleRangeMaxValue := FNormalizedVisibleRangeMinValue + ANewInterval;
            NormalizedVisibleRangeMinValue := NormalizedVisibleRangeMaxValue - ANewInterval;
            StoreVisibleRangeInterval;
          end;
        end
        else
        begin
          if ARangeInterval > FNormalizedVisibleRangeMinInterval then
          begin
            ACenter := (FNormalizedVisibleRangeMaxValue + FNormalizedVisibleRangeMinValue) / 2;
            ANewInterval := Max(ARangeInterval / Factor, FNormalizedVisibleRangeMinInterval);
            NormalizedVisibleRangeMinValue := ACenter - ANewInterval / 2;
            NormalizedVisibleRangeMaxValue := FNormalizedVisibleRangeMinValue + ANewInterval;
            StoreVisibleRangeInterval;
          end;
        end;
      finally
        EndUpdate(FIsViewInfoDirty);
      end;
    end
    else
      OffsetVisibleRange(- Sign(WheelDelta) * Mouse.WheelScrollLines * 0.02, False);
  end;
end;

function TdxCustomRangeControl.IsCancelModeNeeded: Boolean;
begin
  Result := ((ViewInfo.PressedInfo = ViewInfo.ClientArea) or
    (ViewInfo.PressedInfo = ViewInfo.ClientArea.MinSelectionThumb) or
    (ViewInfo.PressedInfo = ViewInfo.ClientArea.MaxSelectionThumb) or
    (ViewInfo.PressedInfo = ViewInfo.ClientArea.SelectedRange) or
    (ViewInfo.PressedInfo = ViewInfo.ZoomScrollBar.SelectionScrollThumb) or
    (ViewInfo.PressedInfo is TdxRangeControlScaleElementViewInfo));
end;

function TdxCustomRangeControl.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := ClientProperties <> nil;
end;

procedure TdxCustomRangeControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_ESCAPE) and IsCancelModeNeeded then
    DoInternalCancelMode
  else
    case Key of
      VK_HOME:
        SetViewportStartValue(0, True);
      VK_END:
        SetViewportStartValue(1 - FNormalizedVisibleRangeMaxValue + FNormalizedVisibleRangeMinValue, True);
      VK_PRIOR:
        OffsetVisibleRange(-FNormalizedVisibleRangeMaxValue + FNormalizedVisibleRangeMinValue, True);
      VK_NEXT:
        OffsetVisibleRange(FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue, True);
    end;
end;

procedure TdxCustomRangeControl.LockClientNotification;
begin
  Inc(FLockNotifierCount);
end;

procedure TdxCustomRangeControl.MakeSelectedRangeVisible(AAnimated: Boolean);
var
  ASelectedRangeDelta, AVisibleRangeDelta: Double;
  AValue: Double;
begin
  ASelectedRangeDelta := FNormalizedSelectedRangeMaxValue - FNormalizedSelectedRangeMinValue;
  AVisibleRangeDelta := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
  if AVisibleRangeDelta >= ASelectedRangeDelta then
    AValue := (FNormalizedSelectedRangeMinValue + FNormalizedSelectedRangeMaxValue - AVisibleRangeDelta) / 2
  else
    AValue := FNormalizedSelectedRangeMinValue;
  AValue := Max(AValue, 0);
  AValue := Min(AValue, 1 - AVisibleRangeDelta);
  SetViewportStartValue(AValue, AAnimated);
end;

procedure TdxCustomRangeControl.TranslationChanged;
begin
  inherited;
  Changed;
end;

procedure TdxCustomRangeControl.NotifyClient;
begin
  if FLockNotifierCount = 0 then
  begin
    dxCallNotify(FOnSelectedRangeChanged, Self);
    if FClient <> nil then
      GetClient.SelectedRangeChanged;
  end;
end;

procedure TdxCustomRangeControl.OffsetVisibleRange(const AValue: Double; AAnimated: Boolean);
var
  ANextValue, ARangeInterval: Double;
begin
  ARangeInterval := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
  ANextValue := FNormalizedVisibleRangeMinValue + AValue;
  ANextValue := EnsureRange(ANextValue, 0, 1 - ARangeInterval);
  SetViewportStartValue(ANextValue, AAnimated);
end;

procedure TdxCustomRangeControl.RecreateClientProperties(Value: TdxRangeControlClientPropertiesClass);
begin
  FreeAndNil(FClientProperties);
  FClientPropertiesClass := Value;
  if FClient = nil then
    ResetRanges;
  if FClientPropertiesClass <> nil then
    FClientProperties := FClientPropertiesClass.Create(Self);
  ViewInfo.RecreateClientInfo;
  if FClientProperties <> nil then
    InitializeRanges;
end;

procedure TdxCustomRangeControl.ResetRanges;
begin
  FNormalizedSelectedEditingMinValue := 0;
  FNormalizedSelectedEditingMaxValue := 0;
  FNormalizedVisibleRangeMaxValue := 1;
  FNormalizedVisibleRangeMinValue := 0;
  FNormalizedSelectedRangeMinValue := 0;
  FNormalizedSelectedRangeMaxValue := 0;
  FNormalizedVisibleRangeMinInterval := 0.1;
  FNormalizedVisibleRangeMaxInterval := 1;
  StoreVisibleRangeInterval;
end;

procedure TdxCustomRangeControl.SetClient(Value: TComponent);
var
  AClientPropertiesClass: TdxRangeControlClientPropertiesClass;
begin
  if not Supports(Value, IdxRangeControlClient) then
    Value := nil;
  if FClient <> Value then
  begin
    BeginUpdate;
    try
      if FClient <> nil then
        GetClient.DetachRangeControl(Self);
      FClient := Value;
      if FClient <> nil then
      begin
        AClientPropertiesClass := GetClient.GetPropertiesClass;
        if (AClientPropertiesClass = nil) or (FClientPropertiesClass = nil) or
          not FClientPropertiesClass.InheritsFrom(AClientPropertiesClass) then
          RecreateClientProperties(AClientPropertiesClass);
        GetClient.AttachRangeControl(Self);
      end;
      FViewInfo.FIsClientInfoDirty := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxCustomRangeControl.SetClientProperties(Value: TdxRangeControlCustomClientProperties);
begin
  if ClientProperties <> nil then
    FClientProperties.Assign(Value);
end;

procedure TdxCustomRangeControl.SetClientPropertiesClass(Value: TdxRangeControlClientPropertiesClass);
begin
  if FClientPropertiesClass <> Value then
  begin
    BeginUpdate;
    try
      if (FClient <> nil) and
        ((Value = nil) or not Value.InheritsFrom(GetClient.GetPropertiesClass)) then
        Client := nil;
      RecreateClientProperties(Value);
      FViewInfo.FIsClientInfoDirty := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxCustomRangeControl.SetClientPropertiesClassName(const AValue: string);
begin
  ClientPropertiesClass := TdxRangeControlClientPropertiesClass(dxRangeControlClients.FindByClassName(AValue));
end;

procedure TdxCustomRangeControl.SetNormalizedVisibleRangeMaxValue(AValue: Double);

  procedure CheckNormalizedVisibleRangeMinValue;
  begin
    FNormalizedVisibleRangeMinValue := EnsureRange(FNormalizedVisibleRangeMinValue,
      Max(0, FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMaxInterval),
      FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinInterval);
  end;

begin
  AValue := EnsureRange(AValue, FNormalizedVisibleRangeMinInterval, 1);
  if FNormalizedVisibleRangeMaxValue <> AValue then
  begin
    FNormalizedVisibleRangeMaxValue := AValue;
    CheckNormalizedVisibleRangeMinValue;
    ViewInfo.FIsClientInfoDirty := True;
    Changed;
  end;
end;

procedure TdxCustomRangeControl.SetNormalizedVisibleRangeMinValue(AValue: Double);

  procedure CheckNormalizedVisibleRangeMaxValue;
  begin
    FNormalizedVisibleRangeMaxValue := EnsureRange(FNormalizedVisibleRangeMaxValue,
      FNormalizedVisibleRangeMinValue + FNormalizedVisibleRangeMinInterval,
      Min(1, FNormalizedVisibleRangeMinValue + FNormalizedVisibleRangeMaxInterval));
  end;

begin
  AValue := EnsureRange(AValue, 0, 1 - FNormalizedVisibleRangeMinInterval);
  if FNormalizedVisibleRangeMinValue <> AValue then
  begin
    FNormalizedVisibleRangeMinValue := AValue;
    CheckNormalizedVisibleRangeMaxValue;
    ViewInfo.FIsClientInfoDirty := True;
    Changed;
  end;
end;

procedure TdxCustomRangeControl.SetSelectedRangeMaxValue(Value: TdxRangeEditValue);
var
  ANormalizedValue: Double;
begin
  if ClientProperties <> nil then
  begin
    ANormalizedValue := ClientProperties.GetNormalizedValue(Value);
    ANormalizedValue := ClientProperties.GetNearestNormalizedAcceptableValue(ANormalizedValue);
    SetNormalizedSelectedRange(NormalizedSelectedRangeMinValue, ANormalizedValue, [retMax], True);
  end;
end;

procedure TdxCustomRangeControl.SetSelectedRangeMinValue(Value: TdxRangeEditValue);
var
  ANormalizedValue: Double;
begin
  if ClientProperties <> nil then
  begin
    ANormalizedValue := ClientProperties.GetNormalizedValue(Value);
    ANormalizedValue := ClientProperties.GetNearestNormalizedAcceptableValue(ANormalizedValue);
    SetNormalizedSelectedRange(ANormalizedValue, NormalizedSelectedRangeMaxValue, [retMin], True);
  end;
end;

procedure TdxCustomRangeControl.SetShowRuler(Value: Boolean);
begin
  if FShowRuler <> Value then
  begin
    FShowRuler := Value;
    ViewInfo.FIsClientInfoDirty := True;
    Changed;
  end;
end;

procedure TdxCustomRangeControl.SetShowZoomScrollBar(Value: Boolean);
begin
  if FShowZoomScrollBar <> Value then
  begin
    FShowZoomScrollBar := Value;
    ViewInfo.FIsClientInfoDirty := True;
    Changed;
  end;
end;

procedure TdxCustomRangeControl.SetStyle(Value: TdxCustomRangeControlStyle);
begin
  FStyle.Assign(Value);
end;

procedure TdxCustomRangeControl.SetViewportPosition(APos: Integer; AAnimated: Boolean);
var
  AValue: Double;
  ARangeInterval: Double;
begin
  ARangeInterval := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
  AValue := EnsureRange(GetNormalizedValueFromPositionInZoomScrollBarArea(APos), 0, 1 - ARangeInterval);
  SetViewportStartValue(AValue, AAnimated);
end;

procedure TdxCustomRangeControl.SetViewportStartValue(const AValue: Double; AAnimated: Boolean);
var
  ARangeInterval: Double;
begin
  ARangeInterval := FNormalizedVisibleRangeMaxValue - FNormalizedVisibleRangeMinValue;
  if AAnimated and AllowAnimation then
  begin
    FreeAndNil(FVisibleRangeAnimationController);
    FVisibleRangeAnimationLength := Max(Width, 100);
    FVisibleRangeAnimationController := TdxAnimationTransition.Create(AnimationTime,
      AnimationTransitionEffect, FVisibleRangeAnimationLength);
    FVisibleRangeAnimationController.OnAnimate := DoVisibleRangeAnimate;
    FVisibleRangeAnimationController.OnTerminate := DoVisibleRangeAnimationTerminate;
    FVisibleRangeAnimationController.FreeOnTerminate := False;
    FVisibleRangeAnimationNormalizedMinStartValue :=  FNormalizedVisibleRangeMinValue;
    FVisibleRangeAnimationNormalizedMinFinishValue := AValue;
    FVisibleRangeAnimationActive := True;
    FVisibleRangeAnimationController.Resume;
  end
  else
  begin
    BeginUpdate;
    try
      NormalizedVisibleRangeMinValue := AValue;
      NormalizedVisibleRangeMaxValue := AValue + ARangeInterval;
    finally
      EndUpdate(FIsViewInfoDirty);
    end;
  end;
end;

procedure TdxCustomRangeControl.SetVisibleRangeMaxValue(Value: TdxRangeEditValue);
begin
  if ClientProperties <> nil then
  begin
    StopVisibleRangeAnimation;
    NormalizedVisibleRangeMaxValue := ClientProperties.GetNormalizedValue(Value);
    StoreVisibleRangeInterval;
  end;
end;

procedure TdxCustomRangeControl.SetVisibleRangeMinValue(Value: TdxRangeEditValue);
begin
  if ClientProperties <> nil then
  begin
    StopVisibleRangeAnimation;
    NormalizedVisibleRangeMinValue := ClientProperties.GetNormalizedValue(Value);
    StoreVisibleRangeInterval;
  end;
end;

procedure TdxCustomRangeControl.SetVisibleRangeMaxScaleFactor(Value: Double);
var
  AInterval: Double;
begin
  Value := Max(Value, 1);
  if ClientProperties <> nil then
    ClientProperties.CheckMinPossibleRangeInterval(Value);
  AInterval := 1 / Value;
  AInterval := Min(AInterval, FNormalizedVisibleRangeMaxInterval);
  if FNormalizedVisibleRangeMinInterval <> AInterval then
  begin
    FNormalizedVisibleRangeMinInterval := AInterval;
    VisibleRangeMaxScaleChanged;
  end;
end;

procedure TdxCustomRangeControl.StopSelectedRangeAnimation;
begin
  FSelectedRangeAnimationActive := False;
  FreeAndNil(FSelectedRangeAnimationController);
end;

procedure TdxCustomRangeControl.StopVisibleRangeAnimation;
begin
  FVisibleRangeAnimationActive := False;
  FreeAndNil(FVisibleRangeAnimationController);
end;

procedure TdxCustomRangeControl.StoreVisibleRangeInterval;
begin
  FNormalizedVisibleRangeStoredInterval := NormalizedVisibleRangeMaxValue - NormalizedVisibleRangeMinValue;
end;

procedure TdxCustomRangeControl.UnlockClientNotification;
begin
  Dec(FLockNotifierCount);
  NotifyClient;
end;

procedure TdxCustomRangeControl.UpdateVisibleRangeMaxInterval(AValue: Double);
var
  ACheckIntervalNeeded: Boolean;
begin
  StopVisibleRangeAnimation;
  AValue := EnsureRange(AValue, 0, 1);
  if FNormalizedVisibleRangeMaxInterval <> AValue then
  begin
    ACheckIntervalNeeded := (AValue > FNormalizedVisibleRangeMaxInterval) and (FNormalizedVisibleRangeStoredInterval > 0);
    FNormalizedVisibleRangeMaxInterval := AValue;
    if FNormalizedVisibleRangeMinInterval > FNormalizedVisibleRangeMaxInterval then
      VisibleRangeMaxScaleFactor := VisibleRangeMaxScaleFactor;
    if (NormalizedVisibleRangeMaxValue - NormalizedVisibleRangeMinValue) > AValue then
      if NormalizedVisibleRangeMaxValue = 1 then
        NormalizedVisibleRangeMinValue := 1 - AValue
      else
        NormalizedVisibleRangeMaxValue := NormalizedVisibleRangeMinValue + AValue;
    if ACheckIntervalNeeded then
    begin
      if NormalizedVisibleRangeMaxValue = 1 then
        NormalizedVisibleRangeMinValue := 1 - FNormalizedVisibleRangeStoredInterval
      else
        NormalizedVisibleRangeMaxValue := NormalizedVisibleRangeMinValue + FNormalizedVisibleRangeStoredInterval;
    end;
  end;
end;

procedure TdxCustomRangeControl.VisibleRangeMaxScaleChanged;
var
  AInterval: Double;
begin
  StopVisibleRangeAnimation;
  AInterval := FNormalizedVisibleRangeMinInterval;
  if (NormalizedVisibleRangeMaxValue - NormalizedVisibleRangeMinValue) < AInterval then
    if NormalizedVisibleRangeMaxValue = 1 then
      NormalizedVisibleRangeMinValue := 1 - AInterval
    else
      NormalizedVisibleRangeMaxValue := NormalizedVisibleRangeMinValue + AInterval;
end;

initialization
  dxRangeControlClients.Register(TdxRangeControlNumericClientProperties, 'Numeric');
  dxRangeControlClients.Register(TdxRangeControlDateTimeClientProperties, 'DateTime');
  dxRangeControlClients.Register(TdxRangeControlDateTimeHeaderClientProperties, 'DateTimeHeader');

finalization
  dxRangeControlClients.Unregister(TdxRangeControlDateTimeHeaderClientProperties);
  dxRangeControlClients.Unregister(TdxRangeControlDateTimeClientProperties);
  dxRangeControlClients.Unregister(TdxRangeControlNumericClientProperties);
  FreeAndNil(FRangeControlClients);

end.


