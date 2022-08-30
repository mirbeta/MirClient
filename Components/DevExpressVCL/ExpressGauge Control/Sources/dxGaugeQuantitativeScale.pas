{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxGaugeQuantitativeScale;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Classes, Graphics, Windows, Generics.Defaults, Generics.Collections, cxClasses,  cxGeometry, dxCore, dxCoreGraphics, cxGraphics,
  dxGDIPlusAPI, dxGDIPlusClasses, dxAnimation, dxCompositeShape, dxXMLDoc, dxGaugeCustomScale;

const
  dxGaugeScaleDefaultAnimationInterval = 700;
  dxGaugeScaleDefaultAnimationFrameCount = 500;
type
  TdxGaugeCustomRange = class;
  TdxGaugeCustomRangeClass = class of TdxGaugeCustomRange;
  TdxGaugeCustomRangeParameters = class;
  TdxGaugeCustomRangeParametersClass = class of TdxGaugeCustomRangeParameters;
  TdxGaugeCustomRangeViewInfo = class;
  TdxGaugeCustomRangeViewInfoClass = class of TdxGaugeCustomRangeViewInfo;
  TdxGaugeRangeCollection = class;
  TdxGaugeQuantitativeScaleInfo = class;
  TdxGaugeQuantitativeScaleInfoClass = class of TdxGaugeQuantitativeScaleInfo;
  TdxGaugeQuantitativeScale = class;
  TdxGaugeQuantitativeScaleClass = class of TdxGaugeQuantitativeScale;
  TdxGaugeQuantitativeScaleAnimationController = class;
  TdxGaugeQuantitativeScaleAnimationControllerClass = class of TdxGaugeQuantitativeScaleAnimationController;
  TdxGaugeQuantitativeScaleCaption = class;
  TdxGaugeQuantitativeScaleOptionsAnimate = class;
  TdxGaugeQuantitativeScaleOptionsAnimateClass = class of TdxGaugeQuantitativeScaleOptionsAnimate;
  TdxGaugeQuantitativeScaleParameters = class;
  TdxGaugeQuantitativeScaleParametersClass = class of TdxGaugeQuantitativeScaleParameters;
  TdxGaugeQuantitativeScaleViewInfo = class;
  TdxGaugeQuantitativeScaleViewInfoClass = class of TdxGaugeQuantitativeScaleViewInfo;

  TdxGaugeQuantitativeScaleParameterValue = (spvFontColor, spvFontName, spvFontSize, spvShowFirstTick, spvShowLabels,
    spvShowLastTick, spvShowTicks, spvMajorTickCount, spvMinorTickCount);
  TdxGaugeQuantitativeScaleParameterValues = set of TdxGaugeQuantitativeScaleParameterValue;

  TdxGaugeScaleRangeValueLinkedWithScaleValue = (rlsvNone, rlsvValueStart, rlsvValueEnd);

  TdxGaugeQuantitativeScaleAnimationTransitionEffect = ateLinear..ateQuartic;

  TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo = record
    Color: TdxAlphaColor;
    Rect: TdxRectF;
    Text: string;
    Visible: Boolean;
  end;

  TdxGaugeQuantitativeScaleTickmarkTickDrawInfo = record
    Image: TGraphic;
    Offset: Single;
    Size: TdxSizeF;
    Visible: Boolean;
  end;

  TdxGaugeQuantitativeScaleTickmark = record
    LabelDrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo;
    TickDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo;

    Position: Single;
    Value: Single;
  end;

  TdxGaugeQuantitativeScaleTickmarks = array of TdxGaugeQuantitativeScaleTickmark;

  TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent = procedure(ASender: TObject; const AValue: Single;
    var ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo) of object;
  TdxGaugeQuantitativeScaleGetValuePositionFunc = function(AValue: Single): Single of object;

  TdxGaugeQuantitativeScaleTickmarkValue = record
    IsMajor: Boolean;
    Position: TdxPointF;
    Value: Single;
  end;

  { TdxGaugeQuantitativeScaleDefaultParameters }

  TdxGaugeQuantitativeScaleDefaultParameters = class(TdxGaugeCustomScaleDefaultParameters)
  public
    // Font
    FontColor: TColor;
    FontName: string;
    FontSize: Integer;
    // Labels
    LabelOffset: Single;
    // Ticks
    AllowOverlapMajorTicks: Boolean;
    RotateTicks: Boolean;
    ShowFirstTick: Boolean;
    ShowLabels: Boolean;
    ShowLastTick: Boolean;
    ShowTicks: Boolean;
    // Major ticks
    MajorTickCount: Integer;
    MajorTickOffset: Single;
    MajorTickScaleFactor: TdxPointF;
    MajorTickType: TdxGaugeElementType;
    // Minor ticks
    MinorTickCount: Integer;
    MinorTickOffset: Single;
    MinorTickScaleFactor: TdxPointF;
    MinorTickType: TdxGaugeElementType;
    // Value indicator
    SpindleCapSize: Single;
    ValueIndicatorStartOffset: Single;
    ValueIndicatorEndOffset: Single;

    constructor Create; virtual;
  end;

  { TdxGaugeQuantitativeScaleParameters }

  TdxGaugeQuantitativeScaleParameters = class(TdxGaugeCustomScaleParameters)
  public
    Font: Pointer;
    MajorTickCount: Integer;
    LogarithmicBase: Single;
    MaxValue: Single;
    MinValue: Single;
    MinorTickCount: Integer;
    ShowFirstTick: Boolean;
    ShowLabels: Boolean;
    ShowLastTick: Boolean;
    ShowTicks: Boolean;
    ShowValueIndicator: Boolean;
  end;

  { TdxGaugeQuantitativeScaleInfo }

  TdxGaugeQuantitativeScaleInfo = class(TdxGaugeCustomScaleInfo)
  public
    Font: Pointer;
    MaxValue: Single;
    MinValue: Single;
    GetValuePositionFunc: TdxGaugeQuantitativeScaleGetValuePositionFunc;
  end;

  { TdxGaugeCustomRangeParameters }

  TdxGaugeCustomRangeParameters = class
  public
    Color: TdxAlphaColor;
    LinkedWithScaleValue: TdxGaugeScaleRangeValueLinkedWithScaleValue;
    ValueEnd: Single;
    ValueEndPosition: Single;
    ValueStart: Single;
    ValueStartPosition: Single;
    Visible: Boolean;
    WidthFactor: Single;

    procedure Assign(ASource: TdxGaugeCustomRangeParameters);
  end;

  { TdxGaugeQuantitativeScaleCaptionParameters }

  TdxGaugeQuantitativeScaleCaptionParameters = class(TdxGaugeCustomCaptionParameters)
  public
    UseOwnFont: Boolean;
  end;

  { TdxGaugeQuantitativeScaleTickmarksCalculator }

  TdxGaugeQuantitativeScaleTickmarksCalculator = class
  private
    FAllowOverlapping: Boolean;
    FBounds: TdxRectF;
    FLogarithmicBase: Single;
    FMaxValue: Single;
    FMinValue: Single;
    FMajorValueCount: Integer;
    FMinorValueCount: Integer;

    FLockCount: Integer;
    FChanged: Boolean;

    FMajorValues: TList<Single>;
    FMinorValues: TList<Single>;
    FLength: TdxPointF;

    FOnChanged: TNotifyEvent;
    FTicks: TList<TdxGaugeQuantitativeScaleTickmarkValue>;

    procedure SetAllowOverlapping(const AValue: Boolean);
    procedure SetBounds(const AValue: TdxRectF);
    procedure SetLogarithmicBase(const AValue: Single);
    procedure SetMajorValueCount(const AValue: Integer);
    procedure SetMaxValue(const AValue: Single);
    procedure SetMinorValueCount(const AValue: Integer);
    procedure SetMinValue(const AValue: Single);

    function GetLogarithm(AValue: Single): Single;
		function GetMajorValuesFirstIndex: Integer;
		function GetMajorValuesLastIndex: Integer;
    function GetMaxValuePower: Integer;
    function GetMaxValueSign: Integer;
		function GetMinorValue(AValue, AMajorValue: Single; AMajorValueSign, AMajorValueIndex: Integer): Single;
    function GetMinValuePower: Integer;
    function GetMinValueSign: Integer;
		function GetEndValueOffset: Single;
		function GetStartValueOffset: Single;
    function GetMajorValueCount: Integer;
    function GetMinorValueCount: Integer;
    function GetValueRange: Single;
    function IsLogarithmic: Boolean;
    function IsValidValue(AValue: Single): Boolean;
    function ValuePercentToPoint(APercent: Single): TdxPointF;
    function ValuePointOffset(AValueOffset: Single): TdxPointF;
    function ValueToPercent(AValue: Single): Single;
    procedure CalculateLogarithmicMajorValues(AMaxValuePower, AMaxValueSign, AMinValuePower, AMinValueSign: Integer);
    procedure CalculateMajorValues;
    procedure CalculateMajorTicksPositions(var APositions: TdxPointsF);
    procedure CalculateMinorValues;
    procedure CalculatePositions;
    procedure CalculateScaleVector;
    procedure CalculateTickmarksPositions(AMajor: Boolean; ACount: Integer; var APositions: TdxPointsF);
    procedure CalculateValues;
    procedure Calculate;
    procedure Changed;
    procedure PopulateMajorTickmarksPositions(const APositions: TdxPointsF);
    procedure PopulateMinorTickmarksPositions(const APositions: TdxPointsF);
  protected
    function ValueToPoint(AValue: Single): TdxPointF;
    procedure BeginUpdate;
    procedure EndUpdate;

    property AllowOverlapping: Boolean read FAllowOverlapping write SetAllowOverlapping;
    property Bounds: TdxRectF read FBounds write SetBounds;
    property LogarithmicBase: Single read FLogarithmicBase write SetLogarithmicBase;
    property MaxValue: Single read FMaxValue write SetMaxValue;
    property MinValue: Single read FMinValue write SetMinValue;
    property MajorValueCount: Integer read FMajorValueCount write SetMajorValueCount;
    property MinorValueCount: Integer read FMinorValueCount write SetMinorValueCount;
    property Ticks: TList<TdxGaugeQuantitativeScaleTickmarkValue> read FTicks;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxGaugeQuantitativeScaleOptionsView }

  TdxGaugeQuantitativeScaleOptionsView = class(TdxGaugeCustomScaleOptionsView)
  private
    function GetLogarithmicBase: Single;
    function GetMajorTickCount: Integer;
    function GetMaxValue: Single;
    function GetMinorTickCount: Integer;
    function GetMinValue: Single;
    function GetFont: TFont;
    function GetShowFirstTick: Boolean;
    function GetShowLabels: Boolean;
    function GetShowLastTick: Boolean;
    function GetShowTicks: Boolean;
    procedure SetLogarithmicBase(const AValue: Single);
    procedure SetMajorTickCount(const AValue: Integer);
    procedure SetMaxValue(const AValue: Single);
    procedure SetMinorTickCount(const AValue: Integer);
    procedure SetMinValue(const AValue: Single);
    procedure SetFont(const AValue: TFont);
    procedure SetShowFirstTick(const AValue: Boolean);
    procedure SetShowLabels(const AValue: Boolean);
    procedure SetShowLastTick(const AValue: Boolean);
    procedure SetShowTicks(const AValue: Boolean);

    function GetScale: TdxGaugeQuantitativeScale;
    procedure ReadMaxValue(AReader: TReader);
    procedure WriteMaxValue(AWriter: TWriter);

    function IsMajorTickCountStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinorTickCountStored: Boolean;
    function IsFontStored: Boolean;
    function IsShowFirstTickStored: Boolean;
    function IsShowLabelsStored: Boolean;
    function IsShowLastTickStored: Boolean;
    function IsShowTicksStored: Boolean;
  protected
    procedure DefineProperties(AFiler: TFiler); override;

    function GetShowValueIndicator: Boolean;
    procedure SetShowValueIndicator(const AValue: Boolean);

    property LogarithmicBase: Single read GetLogarithmicBase write SetLogarithmicBase;
    property ShowValueIndicator: Boolean read GetShowValueIndicator write SetShowValueIndicator default True;
  published
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property MajorTickCount: Integer read GetMajorTickCount write SetMajorTickCount stored IsMajorTickCountStored;
    property MaxValue: Single read GetMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinorTickCount: Integer read GetMinorTickCount write SetMinorTickCount stored IsMinorTickCountStored;
    property MinValue: Single read GetMinValue write SetMinValue;
    property ShowFirstTick: Boolean read GetShowFirstTick write SetShowFirstTick stored IsShowFirstTickStored;
    property ShowLabels: Boolean read GetShowLabels write SetShowLabels stored IsShowLabelsStored;
    property ShowLastTick: Boolean read GetShowLastTick write SetShowLastTick stored IsShowLastTickStored;
    property ShowTicks: Boolean read GetShowTicks write SetShowTicks stored IsShowTicksStored;
  end;

  { TdxGaugeQuantitativeScaleViewInfo }

  TdxGaugeQuantitativeScaleViewInfo = class(TdxGaugeCustomScaleViewInfo)
  private
    FFont: TFont;
    FGpFont: GpFont;
    FScaleValuePosition: Single;

    FTickmarksCalculator: TdxGaugeQuantitativeScaleTickmarksCalculator;
    FMajorTick: TdxCompositeShape;
    FMinorTick: TdxCompositeShape;
    FMajorTickmarks: TdxGaugeQuantitativeScaleTickmarks;
    FMinorTickmarks: TdxGaugeQuantitativeScaleTickmarks;
    FMajorTicksDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo;
    FMinorTicksDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo;

    FOnGetTickmarkLabelDrawInfo: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent;
    procedure TickmarksChangeHandler(ASender: TObject);

    function GetDefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
    function GetParameters: TdxGaugeQuantitativeScaleParameters;
    function GetTickmarkLabelText(AScaleParameters: TdxGaugeQuantitativeScaleParameters; AValue: Single): string;
    procedure AddTickmark(AValue, APosition: Single; var ATickmarks: TdxGaugeQuantitativeScaleTickmarks);
    procedure CalculateScaleValuePosition;
    procedure CalculateTickmarks;
    procedure CalculateTickmarksDrawInfos;
    procedure CalculateTickmarksParameters;

    procedure DrawTickmarkLabel(AGPGraphics: TdxGPGraphics; APosition: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo);
    procedure DrawTickmarkLabels(AGPGraphics: TdxGPGraphics);
    procedure DrawTickmarkTick(AGPGraphics: TdxGPGraphics; const ATick: TdxGaugeQuantitativeScaleTickmark;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo);
    procedure DrawTickmarkTicks(AGPGraphics: TdxGPGraphics);
    procedure DrawTickmarks(AGPGraphics: TdxGPGraphics);
    procedure DrawValueIndicator(AGPGraphics: TdxGPGraphics);
  protected
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;

    function GetScaleInfoClass: TdxGaugeCustomScaleInfoClass; override;
    procedure CalculateContent; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure LoadScaleElements; override;
    procedure PopulateParameters(AParameters: TdxGaugeCustomScaleParameters); override;

    function GetScaleEndPoint: TdxPointF; virtual; abstract;
    function GetScaleStartPoint: TdxPointF; virtual; abstract;
    procedure CalculateReferenceParameters; virtual; abstract;
    procedure DoDrawValueIndicator(AGPGraphics: TdxGPGraphics); virtual; abstract;
    procedure DoDrawTickmarkLabel(AGPGraphics: TdxGPGraphics; APosition: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo); virtual; abstract;
    procedure DoDrawTickmarkTick(AGPGraphics: TdxGPGraphics; APosition: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo); virtual; abstract;

    function CanDrawValueIndicator: Boolean; virtual;
    function GetLabelTextRect(APosition: Single; const AText: string): TdxRectF; virtual; abstract;
    procedure CalculateScaleInfo; override;
    procedure DrawElements(AGPGraphics: TdxGPGraphics); virtual;

    function GetLabelTextRectSize(const AText: string): TdxSizeF;
    function ValueToPosition(AValue: Single): Single;

    // Internal calculated properties
    property Font: TFont read FFont;
    property MajorTicksDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo read FMajorTicksDrawInfo;
    property ScaleValuePosition: Single read FScaleValuePosition;

    property OnGetTickmarkLabelDrawInfo: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent
      read FOnGetTickmarkLabelDrawInfo write FOnGetTickmarkLabelDrawInfo;
  end;

  { TdxGaugeQuantitativeScale }

  TdxGaugeQuantitativeScale = class(TdxGaugeCustomScale)
  private
    FAnimationController: TdxGaugeQuantitativeScaleAnimationController;
    FAssignedValues: TdxGaugeQuantitativeScaleParameterValues;
    FFont: TFont;
    FRanges: TdxGaugeRangeCollection;

    FOnAnimate: TNotifyEvent;
    FOnAnimationComplete: TNotifyEvent;

    function GetFont: TFont;
    function GetLogarithmicBase: Single;
    function GetMajorTickCount: Integer;
    function GetMaxValue: Single;
    function GetMinorTickCount: Integer;
    function GetMinValue: Single;
    function GetOnGetTickmarkLabelDrawInfo: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent;
    function GetOptionsAnimate: TdxGaugeQuantitativeScaleOptionsAnimate;
    function GetShowFirstTick: Boolean;
    function GetShowLabels: Boolean;
    function GetShowLastTick: Boolean;
    function GetShowTicks: Boolean;
    procedure SetFont(const AValue: TFont);
    procedure SetLogarithmicBase(const AValue: Single);
    procedure SetMajorTickCount(const AValue: Integer);
    procedure SetMaxValue(const AValue: Single);
    procedure SetMinorTickCount(const AValue: Integer);
    procedure SetMinValue(const AValue: Single);
    procedure SetOnAnimate(const AValue: TNotifyEvent);
    procedure SetOnAnimationComplete(const AValue: TNotifyEvent);
    procedure SetOnGetTickmarkLabelDrawInfo(const AValue: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent);
    procedure SetOptionsAnimate(const AValue: TdxGaugeQuantitativeScaleOptionsAnimate);
    procedure SetRanges(const AValue: TdxGaugeRangeCollection);
    procedure SetShowFirstTick(const AValue: Boolean);
    procedure SetShowLabels(const AValue: Boolean);
    procedure SetShowLastTick(const AValue: Boolean);
    procedure SetShowTicks(const AValue: Boolean);
    procedure SetValue(const AValue: Single);

    procedure AnimateHandler(ASender: TObject);
    procedure AnimationCompleteHandler(ASender: TObject);
    procedure FontChangeHandler(ASender: TObject);
    procedure RangeChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification);

    function GetParameters: TdxGaugeQuantitativeScaleParameters;
    function GetViewInfo: TdxGaugeQuantitativeScaleViewInfo;
    procedure ApplyStyleFontParameters;
    procedure CalculateRanges;
    procedure CheckScaleValue;
    procedure CreateAnimationController;
    procedure CreateFont;
    procedure CreateRanges;
    procedure DrawRanges(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);

    function IsFontStored: Boolean;
    function IsMajorTickCountStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinorTickCountStored: Boolean;
    function IsShowFirstTickStored: Boolean;
    function IsShowLabelsStored: Boolean;
    function IsShowLastTickStored: Boolean;
    function IsShowTicksStored: Boolean;
    function IsValueStored: Boolean;
  protected
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;

    function GetCaptionClass: TdxGaugeCustomCaptionClass; override;
    function GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    function GetValidValue(const AValue: Variant): Variant; override;
    procedure ApplyStyleParameters; override;
    procedure Calculate(const AScaleBounds: TdxRectF); override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoAssign(AScale: TdxGaugeCustomScale); override;
    procedure DoSetValue(const AValue: Variant); override;
    procedure DrawScaleComponents(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure InitParameters; override;
    procedure InternalRestoreStyleParameters; override;
    procedure Loaded; override;

    function GetAnimationControllerClass: TdxGaugeQuantitativeScaleAnimationControllerClass; virtual;
    function GetRangeClass: TdxGaugeCustomRangeClass; virtual; abstract;

    function IsReading: Boolean;
    function GetShowValueIndicator: Boolean;
    function GetValue: Single;
    procedure SetShowValueIndicator(const AValue: Boolean);

    property AnimationController: TdxGaugeQuantitativeScaleAnimationController read FAnimationController;
    property Font: TFont read GetFont;
    property Ranges: TdxGaugeRangeCollection read FRanges write SetRanges;
    property OptionsAnimate: TdxGaugeQuantitativeScaleOptionsAnimate read GetOptionsAnimate write SetOptionsAnimate;
    property Value: Single read GetValue write SetValue stored IsValueStored;

    property OnAnimate: TNotifyEvent read FOnAnimate write SetOnAnimate;
    property OnAnimationComplete: TNotifyEvent read FOnAnimationComplete write SetOnAnimationComplete;
    property OnGetTickmarkLabelDrawInfo: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent
      read GetOnGetTickmarkLabelDrawInfo write SetOnGetTickmarkLabelDrawInfo;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxGaugeQuantitativeScaleStyleReader }

  TdxGaugeQuantitativeScaleStyleReader = class(TdxGaugeCustomScaleStyleReader)
  protected
    procedure ReadCustomQuantitativeScaleParameters(ANode: TdxXMLNode; AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
    procedure ReadFontParameters(ANode: TdxXMLNode; AParameters: TdxGaugeCustomScaleDefaultParameters);
    procedure ReadLabelsParameters(ANode: TdxXMLNode; AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
    procedure ReadMajorTicksParameters(ANode: TdxXMLNode; AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
    procedure ReadMinorTicksParameters(ANode: TdxXMLNode; AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
    procedure ReadTicksVisibility(ANode: TdxXMLNode; AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
  protected
    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; override;
    procedure ReadParameters(ANode: TdxXMLNode; AParameters: TdxGaugeCustomScaleDefaultParameters); override;

    procedure ReadCommonScaleParameters(ANode: TdxXMLNode;
      AParameters: TdxGaugeQuantitativeScaleDefaultParameters); virtual;
  end;

  { TdxGaugeCustomRangeViewInfo }

  TdxGaugeCustomRangeViewInfo = class
  private
    FParameters: TdxGaugeCustomRangeParameters;
    FScaleInfo: TdxGaugeQuantitativeScaleInfo;
  protected
    FBounds: TdxRectF;
    FSelectionBounds: TdxRectF;

    function GetParametersClass: TdxGaugeCustomRangeParametersClass; virtual;
    procedure CalculateBounds; virtual; abstract;
    procedure CalculateSelectionBounds; virtual;
    procedure DoDraw(AGPGraphics: TdxGPGraphics); virtual; abstract;

    function GetSelectionRect: TRect;
    function GetValueRange: Single;
    procedure Calculate(AParameters: TdxGaugeCustomRangeParameters; AScaleInfo: TdxGaugeCustomScaleInfo);
    procedure Draw(AGPGraphics: TdxGPGraphics);

    property ScaleInfo: TdxGaugeQuantitativeScaleInfo read FScaleInfo;
    property Parameters: TdxGaugeCustomRangeParameters read FParameters;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxGaugeCustomRange }

  TdxGaugeCustomRange = class(TcxComponentCollectionItem, IdxGaugeSelectableElement)
  private
    FParameters: TdxGaugeCustomRangeParameters;

    function GetColor: TdxAlphaColor;
    function GetLinkedWithScaleValue: TdxGaugeScaleRangeValueLinkedWithScaleValue;
    function GetValueEnd: Single;
    function GetValueStart: Single;
    function GetVisible: Boolean;
    function GetWidthFactor: Single;
    procedure SetColor(const AValue: TdxAlphaColor);
    procedure SetLinkedWithScaleValue(const AValue: TdxGaugeScaleRangeValueLinkedWithScaleValue);
    procedure SetValueEnd(const AValue: Single);
    procedure SetValueStart(const AValue: Single);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidthFactor(const AValue: Single);

    function GetValidValue(AValue: Single): Single;
    procedure DoAssign(ARange: TdxGaugeCustomRange);

    procedure ReadWidthFactor(AReader: TReader);
    procedure WriteWidthFactor(AWriter: TWriter);

    function IsColorStored: Boolean;
    function IsWidthFactorStored: Boolean;
  protected
    FViewInfo: TdxGaugeCustomRangeViewInfo;

    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure DefineProperties(AFiler: TFiler); override;

    function GetParametersClass: TdxGaugeCustomRangeParametersClass; virtual;
    function GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass; virtual;

    // IdxGaugeSelectableElement
    function GetSelectionContentRect: TRect;
    function GetSelectionRect: TRect;
    function GetSelectionMarkers: TRects;
    function GetSelectorRect: TRect;
    function IsSizable: Boolean;

    function IsDesigning: Boolean;
    procedure Calculate(AScaleInfo: TdxGaugeCustomScaleInfo);

    property ViewInfo: TdxGaugeCustomRangeViewInfo read FViewInfo;
    // Internal properties
    property Parameters: TdxGaugeCustomRangeParameters read FParameters;

    property Color: TdxAlphaColor read GetColor write SetColor stored IsColorStored;
    property LinkedWithScaleValue: TdxGaugeScaleRangeValueLinkedWithScaleValue
      read GetLinkedWithScaleValue write SetLinkedWithScaleValue default rlsvNone;
    property ValueEnd: Single read GetValueEnd write SetValueEnd;
    property ValueStart: Single read GetValueStart write SetValueStart;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property WidthFactor: Single read GetWidthFactor write SetWidthFactor stored IsWidthFactorStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  end;

  { TdxGaugeRangeCollection }

  TdxGaugeRangeCollection = class(TcxComponentCollection)
  private
    function GetRange(AIndex: Integer): TdxGaugeCustomRange;
    procedure SetRange(AIndex: Integer; const AValue: TdxGaugeCustomRange);
  protected
    function GetItemPrefixName: string; override;

    property Items[Index: Integer]: TdxGaugeCustomRange read GetRange write SetRange; default;
  public
    procedure Assign(ASource: TPersistent); override;
  end;

  { TdxGaugeQuantitativeScaleCaptionOptionsView }

  TdxGaugeQuantitativeScaleCaptionOptionsView = class(TdxGaugeCustomCaptionOptionsView)
  private
    function GetUseOwnFont: Boolean;
    procedure SetUseOwnFont(const AValue: Boolean);

    function GetParameters: TdxGaugeQuantitativeScaleCaptionParameters;
  protected
    function IsFontStored: Boolean; override;
    procedure FontChangeHandler(ASender: TObject); override;
  published
    property UseOwnFont: Boolean read GetUseOwnFont write SetUseOwnFont default False;
  end;

  { TdxGaugeQuantitativeScaleCaptionViewInfo }

  TdxGaugeQuantitativeScaleCaptionViewInfo = class(TdxGaugeCustomCaptionViewInfo)
  protected
    function GetParametersClass: TdxGaugeCustomCaptionParametersClass; override;
  end;

  { TdxGaugeQuantitativeScaleCaption }

  TdxGaugeQuantitativeScaleCaption = class(TdxGaugeCustomCaption)
  private
    function GetOptionsView: TdxGaugeQuantitativeScaleCaptionOptionsView;
    procedure SetOptionsView(const AValue: TdxGaugeQuantitativeScaleCaptionOptionsView);
  protected
    function GetOptionsViewClass: TdxGaugeCustomCaptionOptionsViewClass; override;
    function GetParametersClass: TdxGaugeCustomCaptionParametersClass; override;
    function GetViewInfoClass: TdxGaugeCustomCaptionViewInfoClass; override;

    function GetFont(AScaleInfo: TdxGaugeCustomScaleInfo): TFont; override;
  published
    property OptionsLayout;
    property Text;
    property Visible;

    property OptionsView: TdxGaugeQuantitativeScaleCaptionOptionsView read GetOptionsView write SetOptionsView;
  end;

  { TdxGaugeQuantitativeScaleOptionsAnimate }

  TdxGaugeQuantitativeScaleCustomOptionsAnimate = class(TdxGaugeScaleOptionsPersistent)
  private
    FTransitionEffect: TdxGaugeQuantitativeScaleAnimationTransitionEffect;
    FTransitionEffectMode: TdxAnimationTransitionEffectMode;
    FEnabled: Boolean;
    FFrameCount: Integer;
    FInterval: Integer;
    FOnChanged: TNotifyEvent;

    procedure SetTransitionEffect(const AValue: TdxGaugeQuantitativeScaleAnimationTransitionEffect);
    procedure SetTransitionEffectMode(const AValue: TdxAnimationTransitionEffectMode);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFrameCount(const AValue: Integer);
    procedure SetInterval(const AValue: Integer);

  protected
    procedure Changed;

    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Integer read FInterval write SetInterval default dxGaugeScaleDefaultAnimationInterval;
    property FrameCount: Integer read FFrameCount write SetFrameCount default dxGaugeScaleDefaultAnimationFrameCount;
    property TransitionEffect: TdxGaugeQuantitativeScaleAnimationTransitionEffect read FTransitionEffect
      write SetTransitionEffect default ateAccelerateDecelerate;
    property TransitionEffectMode: TdxAnimationTransitionEffectMode read FTransitionEffectMode
      write SetTransitionEffectMode default atmIn;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  end;

  { TdxGaugeQuantitativeScaleOptionsAnimate }

  TdxGaugeQuantitativeScaleOptionsAnimate = class(TdxGaugeQuantitativeScaleCustomOptionsAnimate)
  published
    property TransitionEffect;
    property TransitionEffectMode;
    property Enabled;
    property Interval;
  end;

  { TdxGaugeQuantitativeScaleAnimationController }

  TdxGaugeQuantitativeScaleAnimationController = class
  private
    FAnimating: Boolean;
    FOptionsAnimate: TdxGaugeQuantitativeScaleOptionsAnimate;
    FScale: TdxGaugeQuantitativeScale;
    FTransition: TdxAnimationTransition;

    FEndValue: Single;
    FStartValue: Single;
    FValue: Single;
    FValueDelta: Single;

    FOnAnimate: TNotifyEvent;
    FOnAnimationComplete: TNotifyEvent;

    function IsFinished(AAnimation: TdxAnimationTransition): Boolean;
    procedure CreateTransition;
    procedure RecreateTransition;

    procedure AnimateHandler(ASender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure OptionsAnimateChangeHandler(ASender: TObject);
  protected
    function GetOptionsAnimateClass: TdxGaugeQuantitativeScaleOptionsAnimateClass; virtual;
    procedure Animate(ACurrentValue, ANextValue: Single);

    property Animating: Boolean read FAnimating;
    property Value: Single read FValue;
    property OptionsAnimate: TdxGaugeQuantitativeScaleOptionsAnimate read FOptionsAnimate;

    property OnAnimate: TNotifyEvent read FOnAnimate write FOnAnimate;
    property OnAnimationComplete: TNotifyEvent read FOnAnimationComplete write FOnAnimationComplete;
  public
    constructor Create(AOwner: TdxGaugeQuantitativeScale);
    destructor Destroy; override;
  end;

implementation

uses
  Types, Math, StrUtils, cxFormats, dxGaugeUtils, dxDPIAwareUtils, cxControls;

const
  dxGaugeCustomRangeWidthFactor = 0.1;
  dxGaugeCustomScaleRangeColor = clDefault;

type
  TdxGaugeScaleStyleAccess = class(TdxGaugeScaleStyle);
  TdxAnimationTransitionAccess = class(TdxAnimationTransition);

{ TdxGaugeQuantitativeScaleDefaultParameters }

constructor TdxGaugeQuantitativeScaleDefaultParameters.Create;
begin
  inherited Create;
  MajorTickScaleFactor := dxPointF(cxNullPoint);
end;

{ TdxGaugeCustomRangeParameters }

procedure TdxGaugeCustomRangeParameters.Assign(ASource: TdxGaugeCustomRangeParameters);
begin
  cxCopyData(ASource, Self, SizeOf(ASource), SizeOf(Self), ASource.InstanceSize -  SizeOf(ASource));
end;

{ TdxGaugeQuantitativeScaleTickmarksCalculator }

constructor TdxGaugeQuantitativeScaleTickmarksCalculator.Create;
begin
  inherited Create;
  FMajorValues := TList<Single>.Create;
  FMinorValues := TList<Single>.Create;
  FTicks := TList<TdxGaugeQuantitativeScaleTickmarkValue>.Create;
end;

destructor TdxGaugeQuantitativeScaleTickmarksCalculator.Destroy;
begin
  FreeAndNil(FTicks);
  FreeAndNil(FMinorValues);
  FreeAndNil(FMajorValues);
  inherited Destroy;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.ValueToPoint(AValue: Single): TdxPointF;
begin
  Result := ValuePercentToPoint(ValueToPercent(AValue));
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.EndUpdate;
begin
  Dec(FLockCount);
  Calculate;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetAllowOverlapping(const AValue: Boolean);
begin
  if FAllowOverlapping <> AValue then
  begin
    FAllowOverlapping := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetBounds(const AValue: TdxRectF);
begin
  if AValue <> FBounds then
  begin
    FBounds := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetLogarithmicBase(const AValue: Single);
begin
  if FLogarithmicBase <> AValue then
  begin
    FLogarithmicBase := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetMajorValueCount(const AValue: Integer);
begin
  if FMajorValueCount <> AValue then
  begin
    FMajorValueCount := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetMaxValue(const AValue: Single);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetMinorValueCount(const AValue: Integer);
begin
  if FMinorValueCount <> AValue then
  begin
    FMinorValueCount := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.SetMinValue(const AValue: Single);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    Changed;
  end;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetLogarithm(AValue: Single): Single;
var
  AValidValue: Single;
begin
  if AValue = 0 then
    AValidValue := 1
  else
    AValidValue := Abs(AValue);
  Result := LogN(FLogarithmicBase, AValidValue);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMajorValuesFirstIndex: Integer;
begin
  Result := Integer(FMinValue > FMajorValues.First);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMajorValuesLastIndex: Integer;
begin
  Result := FMajorValues.Count - 1;
  if (FMajorValues.Count = 0) or (FMaxValue >= FMajorValues.Last) then
    Inc(Result);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMaxValuePower: Integer;
var
  AResult: Single;
begin
  AResult := GetLogarithm(FMaxValue);
  if FMaxValue >= 0 then
    Result := Ceil(AResult)
  else
    Result := Floor(AResult);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMaxValueSign: Integer;
begin
  if Sign(FMaxValue) <> 0 then
    Result := Sign(FMaxValue)
  else
    Result := Sign(FMinValue);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMinorValue(AValue, AMajorValue: Single;
  AMajorValueSign, AMajorValueIndex: Integer): Single;
begin
  if IsLogarithmic then
    Result := GetLogarithm(AValue) - GetLogarithm(AMajorValue)
  else
    Result := AValue - AMajorValue;
  if AMajorValueSign = -1 then
    Result := 1 - Abs(Result);
  Result := Abs(Result) + AMajorValueIndex - 1;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMinValuePower: Integer;
var
  AResult: Single;
begin
  AResult := GetLogarithm(FMinValue);
  if FMinValue >= 0 then
    Result := Floor(AResult)
  else
    Result := Ceil(AResult);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMinValueSign: Integer;
begin
  if Sign(FMinValue) <> 0 then
    Result := Sign(FMinValue)
  else
    Result := Sign(FMaxValue);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetEndValueOffset: Single;
begin
  if GetMajorValuesLastIndex = FMajorValues.Count then
    Result := 0
  else
    Result := GetMinorValue(FMaxValue, FMajorValues[GetMajorValuesLastIndex], 1, 1);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetStartValueOffset: Single;
begin
  if GetMajorValuesFirstIndex = 0 then
    Result := 0
  else
    Result := GetMinorValue(FMinValue, FMajorValues[GetMajorValuesFirstIndex - 1], 1, 1);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMajorValueCount: Integer;
begin
  Result := FMajorValues.Count;
  if IsLogarithmic and (Result <> 0) then
  begin
    if FMaxValue < FMajorValues.Last then
      Dec(Result);
    if FMinValue > FMajorValues.First then
      Dec(Result);
  end;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetMinorValueCount: Integer;
begin
  if IsLogarithmic then
    Result := FMinorValues.Count
  else
    Result := 1 + Max(1, FMajorValueCount) * FMinorValueCount
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.GetValueRange: Single;
begin
  if IsLogarithmic then
  begin
    Result := GetEndValueOffset;
    if Result <> 0 then
      if GetStartValueOffset = 0 then
        Result := 1 - GetEndValueOffset
      else
        Result := 2 - GetEndValueOffset - GetStartValueOffset
    else
      Result := 1;
    Result := Result + GetMajorValueCount - 1;
  end
  else
    Result := dxGaugeValueRange(FMaxValue, FMinValue);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.IsLogarithmic: Boolean;
begin
  Result := FLogarithmicBase > 1;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.IsValidValue(AValue: Single): Boolean;
begin
  Result := (FMinValue <= AValue) and (AValue <= FMaxValue);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.ValuePercentToPoint(APercent: Single): TdxPointF;
var
  AStartPointOffset: TdxPointF;
begin
  AStartPointOffset := ValuePointOffset(GetStartValueOffset);
  Result.X := FBounds.Left + AStartPointOffset.X + APercent * FLength.X;
  Result.Y := FBounds.Top + AStartPointOffset.Y + APercent * FLength.Y;
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.ValuePointOffset(AValueOffset: Single): TdxPointF;
begin
  if IsLogarithmic then
  begin
    Result.X := AValueOffset * (FBounds.Left - FBounds.Right) / GetValueRange;
    Result.Y := AValueOffset * (FBounds.Top - FBounds.Bottom) / GetValueRange;
  end
  else
    Result := cxPointF(cxNullPoint);
end;

function TdxGaugeQuantitativeScaleTickmarksCalculator.ValueToPercent(AValue: Single): Single;
var
  I: Integer;
begin
  if IsLogarithmic then
  begin
    Result := FMajorValues.IndexOf(AValue);
    if Result = -1 then
      if AValue = FMajorValues.Last then
        Result := GetMinorValue(AValue, AValue, Sign(AValue), FMajorValues.Count - 1 + Integer(AValue >= 0))
      else
        for I := 0 to FMajorValues.Count - 2 do
          if (AValue > FMajorValues[I]) and (AValue < FMajorValues[I + 1]) then
            Result := GetMinorValue(AValue, FMajorValues[I], 1, I + 1);
    Result := Result / (FMajorValues.Count - 1);
  end
  else
    Result := (AValue - FMinValue) / GetValueRange;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateLogarithmicMajorValues(AMaxValuePower, AMaxValueSign,
  AMinValuePower, AMinValueSign: Integer);

  procedure InternalCalculateMajorValues(AValueSign, AMinValuePower, AMaxValuePower: Integer);
  begin
    while AMinValuePower <= AMaxValuePower do
    begin
      FMajorValues.Add(Power(FLogarithmicBase, AMinValuePower) * AValueSign);
      Inc(AMinValuePower);
    end;
  end;

begin
  if AMaxValueSign + AMinValueSign = 0 then
  begin
    InternalCalculateMajorValues(AMinValueSign, 0, AMinValuePower);
    InternalCalculateMajorValues(AMaxValueSign, 0, AMaxValuePower);
    FMajorValues.Add(0);
  end
  else
  begin
    InternalCalculateMajorValues(AMinValueSign, Min(AMinValuePower, AMaxValuePower), Max(AMinValuePower, AMaxValuePower));
    if (FMaxValue = 0) or (FMinValue = 0) then
      FMajorValues.Add(0);
  end;
  FMajorValues.Sort;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateMajorValues;

  procedure InternalCalculateMajorValues;
  var
    I: Integer;
    AValue, AValueDelta: Single;
  begin
    AValue := FMinValue;
    AValueDelta := GetValueRange / FMajorValueCount;
    for I := 0 to FMajorValueCount do
    begin
      FMajorValues.Add(AValue);
      AValue := AValue + AValueDelta;
    end;
  end;

begin
  FMajorValues.Clear;
  if IsLogarithmic then
    CalculateLogarithmicMajorValues(GetMaxValuePower, GetMaxValueSign, GetMinValuePower, GetMinValueSign)
  else
    InternalCalculateMajorValues;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateMajorTicksPositions(var APositions: TdxPointsF);

  function GetPositionIndex(AIndex: Integer): Integer;
  begin
    Result := AIndex;
    if GetMajorValuesFirstIndex <> 0 then
      Result := AIndex - 1;
  end;

var
  I: Integer;
begin
  SetLength(APositions, GetMajorValueCount);
  for I := GetMajorValuesFirstIndex to GetMajorValuesLastIndex - 1 do
    APositions[GetPositionIndex(I)] := ValuePercentToPoint(I / (FMajorValues.Count - 1));
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateMinorValues;

  procedure AddMinorValues(AMajorValue, APreviousMajorValue: Single);
  var
    I: Integer;
    AValue, ACurrentMajorValue: Single;
  begin
    if not IsLogarithmic or (AMajorValue <> 0) and (APreviousMajorValue <> 0) then
    begin
      if Sign(AMajorValue) <> -1 then
        ACurrentMajorValue := APreviousMajorValue
      else
        ACurrentMajorValue := AMajorValue;
      for I := 1 to FMinorValueCount - 1 do
      begin
        AValue := ACurrentMajorValue + Sign(AMajorValue) * I *(AMajorValue - APreviousMajorValue) / FMinorValueCount;
        if IsValidValue(AValue) then
          FMinorValues.Add(GetMinorValue(AValue, ACurrentMajorValue, Sign(ACurrentMajorValue),
            FMajorValues.IndexOf(AMajorValue)));
      end;
    end;
  end;

var
  I: Integer;
begin
  FMinorValues.Clear;
  for I := 0 to FMajorValues.Count - 1 do
  begin
    if I > 0 then
      AddMinorValues(FMajorValues[I], FMajorValues[I - 1]);
    if FAllowOverlapping and IsValidValue(FMajorValues[I]) then
      if IsLogarithmic then
        FMinorValues.Add(I)
      else
        FMinorValues.Add(FMajorValues[I]);
  end;
  FMinorValues.Sort;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculatePositions;

  procedure InternalCalculatePosition(ACount: Integer; AMajor: Boolean);
  var
    APoints: TdxPointsF;
  begin
    CalculateTickmarksPositions(AMajor, ACount, APoints);
    if AMajor then
      PopulateMajorTickmarksPositions(APoints)
    else
      PopulateMinorTickmarksPositions(APoints);
    SetLength(APoints, 0);
  end;

begin
  FTicks.Clear;
  InternalCalculatePosition(GetMajorValueCount, True);
  InternalCalculatePosition(GetMinorValueCount, False);
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateScaleVector;

  function GetScaleVector(const AStartPointOffset, AEndPointOffset: TdxPointF): TdxPointF;
  begin
    Result.X := FBounds.Width - AEndPointOffset.X - AStartPointOffset.X;
    Result.Y := FBounds.Height - AEndPointOffset.Y - AStartPointOffset.Y;
  end;

begin
  FLength := GetScaleVector(ValuePointOffset(GetStartValueOffset), ValuePointOffset(GetEndValueOffset));
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateTickmarksPositions(AMajor: Boolean; ACount: Integer;
  var APositions: TdxPointsF);

  procedure AddPosition(const APosition: TdxPointF);
  var
    L: Integer;
  begin
    L := Length(APositions);
    SetLength(APositions, L + 1);
    APositions[L] := APosition;
  end;

  procedure CalculateMinorTickmarksPositions;
  var
    I: Integer;
  begin
    for I := 0 to FMinorValues.Count - 1 do
      AddPosition(ValuePercentToPoint(FMinorValues[I] / (FMajorValues.Count - 1)));
  end;

var
  I: Integer;
begin
  if IsLogarithmic then
    if AMajor then
      CalculateMajorTicksPositions(APositions)
    else
      CalculateMinorTickmarksPositions
  else
    for I := 0 to ACount - 1 do
      if AMajor or FAllowOverlapping or (not FAllowOverlapping and (I mod FMinorValueCount <> 0)) then
        AddPosition(ValuePercentToPoint(I / (ACount - 1)));
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.CalculateValues;
begin
  FMajorValues.Clear;
  FMinorValues.Clear;
  FTicks.Clear;
  CalculateMajorValues;
  CalculateMinorValues;
  CalculateScaleVector;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.Calculate;
begin
  if (FLockCount = 0) and FChanged then
  begin
    CalculateValues;
    CalculatePositions;
    FChanged := False;
    dxCallNotify(OnChanged, Self);
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.Changed;
begin
  FChanged := True;
  Calculate;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.PopulateMajorTickmarksPositions(const APositions: TdxPointsF);
var
  I: Integer;
  AIndex: Integer;
  AValue: Single;
  ATickmarkValue: TdxGaugeQuantitativeScaleTickmarkValue;
begin
  AIndex := 0;
  for I := Low(APositions) to High(APositions) do
  begin
    if IsLogarithmic and (FMinValue > FMajorValues[0]) then
      AValue := FMajorValues[AIndex + 1]
    else
      AValue := FMajorValues[AIndex];
    ATickmarkValue.Value := AValue;
    ATickmarkValue.Position := APositions[I];
    ATickmarkValue.IsMajor := True;
    FTicks.Add(ATickmarkValue);
    Inc(AIndex);
  end;
end;

procedure TdxGaugeQuantitativeScaleTickmarksCalculator.PopulateMinorTickmarksPositions(const APositions: TdxPointsF);
var
  I: Integer;
  ATickmarkValue: TdxGaugeQuantitativeScaleTickmarkValue;
begin
  for I := Low(APositions) to High(APositions) do
  begin
    ATickmarkValue.Value := 0;
    ATickmarkValue.Position := APositions[I];
    ATickmarkValue.IsMajor := False;
    FTicks.Add(ATickmarkValue);
  end;
end;

{ TdxGaugeQuantitativeScaleOptionsView }

procedure TdxGaugeQuantitativeScaleOptionsView.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('MaxValue', ReadMaxValue, WriteMaxValue, IsMaxValueStored);
end;

function TdxGaugeQuantitativeScaleOptionsView.GetShowValueIndicator: Boolean;
begin
  Result := GetScale.GetShowValueIndicator;
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetShowValueIndicator(const AValue: Boolean);
begin
  GetScale.SetShowValueIndicator(AValue);
end;

function TdxGaugeQuantitativeScaleOptionsView.GetLogarithmicBase: Single;
begin
  Result := GetScale.GetLogarithmicBase;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetMajorTickCount: Integer;
begin
  Result := GetScale.GetMajorTickCount;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetMaxValue: Single;
begin
  Result := GetScale.GetMaxValue;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetMinorTickCount: Integer;
begin
  Result := GetScale.GetMinorTickCount;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetMinValue: Single;
begin
  Result := GetScale.GetMinValue;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetFont: TFont;
begin
  Result := GetScale.FFont;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetShowFirstTick: Boolean;
begin
  Result := GetScale.GetShowFirstTick;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetShowLabels: Boolean;
begin
  Result := GetScale.GetShowLabels;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetShowLastTick: Boolean;
begin
  Result := GetScale.GetShowLastTick;
end;

function TdxGaugeQuantitativeScaleOptionsView.GetShowTicks: Boolean;
begin
  Result := GetScale.GetShowTicks;
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetLogarithmicBase(const AValue: Single);
begin
  GetScale.SetLogarithmicBase(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetMajorTickCount(const AValue: Integer);
begin
  GetScale.SetMajorTickCount(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetMaxValue(const AValue: Single);
begin
  GetScale.SetMaxValue(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetMinorTickCount(const AValue: Integer);
begin
  GetScale.SetMinorTickCount(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetMinValue(const AValue: Single);
begin
  GetScale.SetMinValue(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetFont(const AValue: TFont);
begin
  GetScale.SetFont(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetShowFirstTick(const AValue: Boolean);
begin
  GetScale.SetShowFirstTick(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetShowLabels(const AValue: Boolean);
begin
  GetScale.SetShowLabels(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetShowLastTick(const AValue: Boolean);
begin
  GetScale.SetShowLastTick(AValue);
end;

procedure TdxGaugeQuantitativeScaleOptionsView.SetShowTicks(const AValue: Boolean);
begin
  GetScale.SetShowTicks(AValue);
end;

function TdxGaugeQuantitativeScaleOptionsView.GetScale: TdxGaugeQuantitativeScale;
begin
  Result := inherited GetScale as TdxGaugeQuantitativeScale;
end;

procedure TdxGaugeQuantitativeScaleOptionsView.ReadMaxValue(AReader: TReader);
begin
  MaxValue := AReader.ReadDouble;
end;

procedure TdxGaugeQuantitativeScaleOptionsView.WriteMaxValue(AWriter: TWriter);
begin
  AWriter.WriteDouble(MaxValue);
end;

function TdxGaugeQuantitativeScaleOptionsView.IsMajorTickCountStored: Boolean;
begin
  Result := GetScale.IsMajorTickCountStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsMaxValueStored: Boolean;
begin
  Result := GetScale.IsMaxValueStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsMinorTickCountStored: Boolean;
begin
  Result := GetScale.IsMinorTickCountStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsFontStored: Boolean;
begin
  Result := GetScale.IsFontStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsShowFirstTickStored: Boolean;
begin
  Result := GetScale.IsShowFirstTickStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsShowLabelsStored: Boolean;
begin
  Result := GetScale.IsShowLabelsStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsShowLastTickStored: Boolean;
begin
  Result := GetScale.IsShowLastTickStored;
end;

function TdxGaugeQuantitativeScaleOptionsView.IsShowTicksStored: Boolean;
begin
  Result := GetScale.IsShowTicksStored;
end;

{ TdxGaugeQuantitativeScaleViewInfo }

function TdxGaugeQuantitativeScaleViewInfo.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeQuantitativeScaleParameters;
end;

function TdxGaugeQuantitativeScaleViewInfo.GetScaleInfoClass: TdxGaugeCustomScaleInfoClass;
begin
  Result := TdxGaugeQuantitativeScaleInfo;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFont := TFont.Create;
  FFont.PixelsPerInch := dxDefaultDPI;
  SetLength(FMajorTickmarks, 0);
  SetLength(FMinorTickmarks, 0);
  FTickmarksCalculator := TdxGaugeQuantitativeScaleTickmarksCalculator.Create;
  FTickmarksCalculator.OnChanged := TickmarksChangeHandler;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateContent;
begin
  inherited CalculateContent;
  CalculateReferenceParameters;
  CalculateTickmarks;
  CalculateScaleValuePosition;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DestroySubClasses;
begin
  SetLength(FMinorTickmarks, 0);
  SetLength(FMajorTickmarks, 0);
  FreeAndNil(FTickmarksCalculator);
  FreeAndNil(FFont);
  inherited DestroySubClasses;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  inherited DoDrawLayer(AGPGraphics, ALayerIndex);
  case ALayerIndex of
    1:
      DrawElements(AGPGraphics);
    2:
      DrawValueIndicator(AGPGraphics);
  end;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.LoadScaleElements;
begin
  inherited LoadScaleElements;
  FMajorTick := TdxGaugeScaleStyleAccess(Style).GetElement(GetDefaultParameters.MajorTickType);
  FMinorTick := TdxGaugeScaleStyleAccess(Style).GetElement(GetDefaultParameters.MinorTickType);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.PopulateParameters(AParameters: TdxGaugeCustomScaleParameters);
begin
  inherited PopulateParameters(AParameters);
  FFont.Assign((AParameters as TdxGaugeQuantitativeScaleParameters).Font);
end;

function TdxGaugeQuantitativeScaleViewInfo.CanDrawValueIndicator: Boolean;
begin
  Result := True;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateScaleInfo;
var
  AScaleInfo: TdxGaugeQuantitativeScaleInfo;
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  inherited CalculateScaleInfo;
  AScaleInfo := ScaleInfo as TdxGaugeQuantitativeScaleInfo;
  AScaleInfo.GetValuePositionFunc := ValueToPosition;
  AScaleParameters := GetParameters;
  AScaleInfo.MaxValue := AScaleParameters.MaxValue;
  AScaleInfo.Font := FFont;
  AScaleInfo.MinValue := AScaleParameters.MinValue;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawElements(AGPGraphics: TdxGPGraphics);
begin
  DrawTickmarks(AGPGraphics);
end;

function TdxGaugeQuantitativeScaleViewInfo.GetLabelTextRectSize(const AText: string): TdxSizeF;
var
  R: TdxRectF;
begin
  if FGpFont <> nil then
  begin
    R := dxGaugeGetTextRect(AText, FGpFont);
    Result := dxSizeF(R.Width, R.Height);
  end;
end;

function TdxGaugeQuantitativeScaleViewInfo.ValueToPosition(AValue: Single): Single;
begin
  Result := FTickmarksCalculator.ValueToPoint(AValue).Y;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.TickmarksChangeHandler(ASender: TObject);
var
  I: Integer;
begin
  SetLength(FMajorTickmarks, 0);
  SetLength(FMinorTickmarks, 0);
  for I := 0 to FTickmarksCalculator.Ticks.Count - 1 do
    if FTickmarksCalculator.Ticks[I].IsMajor then
      AddTickmark(FTickmarksCalculator.Ticks[I].Value, FTickmarksCalculator.Ticks[I].Position.Y, FMajorTickmarks)
    else
      AddTickmark(FTickmarksCalculator.Ticks[I].Value, FTickmarksCalculator.Ticks[I].Position.Y, FMinorTickmarks);
end;

function TdxGaugeQuantitativeScaleViewInfo.GetDefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  Result := DefaultParameters as TdxGaugeQuantitativeScaleDefaultParameters;
end;

function TdxGaugeQuantitativeScaleViewInfo.GetParameters: TdxGaugeQuantitativeScaleParameters;
begin
  Result := FParameters as TdxGaugeQuantitativeScaleParameters;
end;

function TdxGaugeQuantitativeScaleViewInfo.GetTickmarkLabelText(AScaleParameters: TdxGaugeQuantitativeScaleParameters;
  AValue: Single): string;
const
  dxGaugeQuantitativeScalePrecision = 3;
var
  I: Integer;
  ATemp: string;
begin
  AValue := Min(Max(AValue, AScaleParameters.MinValue), AScaleParameters.MaxValue);
  Result := Format('%*.*f', [Pos(dxFormatSettings.DecimalSeparator,
    dxFloatToStr(AValue, dxFormatSettings.DecimalSeparator)), dxGaugeQuantitativeScalePrecision, AValue]);
  ATemp := Result;
  for I := 0 to dxGaugeQuantitativeScalePrecision - 1 do
    if Result[Length(Result) - I] = '0' then
      ATemp := Copy(Result, 0, Length(Result) - I - 1)
    else
      Break;
  if ATemp[Length(ATemp)] = dxFormatSettings.DecimalSeparator then
    ATemp := Copy(ATemp, 0, Length(ATemp) - 1);
  Result := ATemp;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.AddTickmark(AValue, APosition: Single;
  var ATickmarks: TdxGaugeQuantitativeScaleTickmarks);
var
  L: Integer;
begin
  L := Length(ATickmarks);
  SetLength(ATickmarks, L + 1);
  ATickmarks[L].Value := AValue;
  ATickmarks[L].Position := APosition;
  ATickmarks[L].TickDrawInfo.Visible := True;
  ATickmarks[L].LabelDrawInfo.Visible := True;
  ATickmarks[L].LabelDrawInfo.Color := dxColorToAlphaColor(Font.Color);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateScaleValuePosition;
begin
  FScaleValuePosition := ValueToPosition(GetParameters.Value);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateTickmarks;
var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  CalculateTickmarksParameters;
  AScaleParameters := GetParameters;
  FTickmarksCalculator.BeginUpdate;
  FTickmarksCalculator.AllowOverlapping := GetDefaultParameters.AllowOverlapMajorTicks and (AScaleParameters.MinorTickCount > 0);
  FTickmarksCalculator.LogarithmicBase := AScaleParameters.LogarithmicBase;
  FTickmarksCalculator.MaxValue := AScaleParameters.MaxValue;
  FTickmarksCalculator.MinValue := AScaleParameters.MinValue;
  FTickmarksCalculator.MajorValueCount := AScaleParameters.MajorTickCount;
  FTickmarksCalculator.MinorValueCount := AScaleParameters.MinorTickCount;
  FTickmarksCalculator.Bounds := cxRectF(GetScaleStartPoint, GetScaleEndPoint);
  FTickmarksCalculator.EndUpdate;
  CalculateTickmarksDrawInfos;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateTickmarksDrawInfos;
var
  I: Integer;
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  if Length(FMajorTickmarks) > 0 then
  begin
    AScaleParameters := GetParameters;
    FMajorTickmarks[0].TickDrawInfo.Visible := AScaleParameters.ShowFirstTick;
    FMajorTickmarks[0].LabelDrawInfo.Visible := AScaleParameters.ShowFirstTick;
    FMajorTickmarks[High(FMajorTickmarks)].TickDrawInfo.Visible := AScaleParameters.ShowLastTick;
    FMajorTickmarks[High(FMajorTickmarks)].LabelDrawInfo.Visible := AScaleParameters.ShowLastTick;
    FGpFont := dxGpCreateFont(Font);
    try
      for I := Low(FMajorTickmarks) to High(FMajorTickmarks) do
      begin
        FMajorTickmarks[I].LabelDrawInfo.Color := dxColorToAlphaColor(Font.Color);
        FMajorTickmarks[I].LabelDrawInfo.Text := GetTickmarkLabelText(AScaleParameters, FMajorTickmarks[I].Value);
        FMajorTickmarks[I].LabelDrawInfo.Rect := cxRectF(cxInvalidRect);
        if Assigned(FOnGetTickmarkLabelDrawInfo) then
          OnGetTickmarkLabelDrawInfo(Self, FMajorTickmarks[I].Value, FMajorTickmarks[I].LabelDrawInfo);
        if FMajorTickmarks[I].LabelDrawInfo.Rect = cxRectF(cxInvalidRect) then
          FMajorTickmarks[I].LabelDrawInfo.Rect := GetLabelTextRect(FMajorTickmarks[I].Position,
            FMajorTickmarks[I].LabelDrawInfo.Text);
      end;
    finally
      GdipDeleteFont(FGpFont);
    end;
  end;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.CalculateTickmarksParameters;

  function GetMajorTicksDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo;
  var
    ADefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
  begin
    ADefaultParameters := GetDefaultParameters;
    Result.Image := FMajorTick;
    Result.Offset := ADefaultParameters.MajorTickOffset;
    Result.Size := GetElementImageOriginalSize(Result.Image);
    Result.Size.cx := Result.Size.cx * ADefaultParameters.MajorTickScaleFactor.X;
    Result.Size.cy := Result.Size.cy * ADefaultParameters.MajorTickScaleFactor.Y;
    Result.Visible := True;
  end;

  function GetMinorTicksDrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo;
  var
    ADefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
  begin
    ADefaultParameters := GetDefaultParameters;
    Result.Image := FMinorTick;
    Result.Offset := ADefaultParameters.MinorTickOffset;
    Result.Size := GetElementImageOriginalSize(Result.Image);
    Result.Size.cx := Result.Size.cx * ADefaultParameters.MinorTickScaleFactor.X;
    Result.Size.cy := Result.Size.cy * ADefaultParameters.MinorTickScaleFactor.Y;
    Result.Visible := True;
  end;

var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  FMajorTicksDrawInfo := GetMajorTicksDrawInfo;
  FMinorTicksDrawInfo := GetMinorTicksDrawInfo;

  AScaleParameters := GetParameters;
  if AScaleParameters.LogarithmicBase = 0 then
    AScaleParameters.MajorTickCount := AScaleParameters.MajorTickCount - 1
  else
    AScaleParameters.MajorTickCount := AScaleParameters.MajorTickCount;
  AScaleParameters.MinorTickCount := AScaleParameters.MinorTickCount + 1;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawTickmarkLabel(AGPGraphics: TdxGPGraphics; APosition: Single;
  const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo);
{$IFDEF DELPHIXE}
var
  APrevRenderingHint: TdxGpTextRenderingHint;
{$ENDIF}
begin
  if ADrawInfo.Visible then
{$IFDEF DELPHIXE}
  begin
    APrevRenderingHint := AGPGraphics.TextRenderingHint;
    if Font.Quality = fqNonAntialiased then
      AGPGraphics.TextRenderingHint := TdxGpTextRenderingHint.TextRenderingHintSystemDefault;
    try
{$ENDIF}
      DoDrawTickmarkLabel(AGPGraphics, APosition, ADrawInfo);
{$IFDEF DELPHIXE}
    finally
      AGPGraphics.TextRenderingHint := APrevRenderingHint;
    end;
  end;
{$ENDIF DELPHIXE}
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawTickmarkLabels(AGPGraphics: TdxGPGraphics);
var
  I: Integer;
begin
  if GetParameters.ShowLabels then
    for I := Low(FMajorTickmarks) to High(FMajorTickmarks) do
      DrawTickmarkLabel(AGPGraphics, FMajorTickmarks[I].Position, FMajorTickmarks[I].LabelDrawInfo);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawTickmarkTick(AGPGraphics: TdxGPGraphics;
  const ATick: TdxGaugeQuantitativeScaleTickmark; const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo);
begin
  if ATick.TickDrawInfo.Visible then
    DoDrawTickmarkTick(AGPGraphics, ATick.Position, ADrawInfo);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawTickmarkTicks(AGPGraphics: TdxGPGraphics);

  procedure InternalDrawTicks(const ATicks: TdxGaugeQuantitativeScaleTickmarks;
    const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo);
  var
    I: Integer;
  begin
    if CanDrawImage(ADrawInfo.Image) then
      for I := Low(ATicks) to High(ATicks) do
        DrawTickmarkTick(AGPGraphics, ATicks[I], ADrawInfo);
  end;

begin
  if GetParameters.ShowTicks then
  begin
    InternalDrawTicks(FMinorTickmarks, FMinorTicksDrawInfo);
    InternalDrawTicks(FMajorTickmarks, FMajorTicksDrawInfo);
  end;
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawTickmarks(AGPGraphics: TdxGPGraphics);
begin
  DrawTickmarkTicks(AGPGraphics);
  DrawTickmarkLabels(AGPGraphics);
end;

procedure TdxGaugeQuantitativeScaleViewInfo.DrawValueIndicator(AGPGraphics: TdxGPGraphics);
begin
  if GetParameters.ShowValueIndicator and CanDrawValueIndicator then
    DoDrawValueIndicator(AGPGraphics);
end;

{ TdxGaugeQuantitativeScale }

constructor TdxGaugeQuantitativeScale.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ApplyStyleFontParameters;
end;

class function TdxGaugeQuantitativeScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeQuantitativeScaleStyleReader;
end;

function TdxGaugeQuantitativeScale.GetCaptionClass: TdxGaugeCustomCaptionClass;
begin
  Result := TdxGaugeQuantitativeScaleCaption;
end;

function TdxGaugeQuantitativeScale.GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass;
begin
  Result := TdxGaugeQuantitativeScaleOptionsView;
end;

function TdxGaugeQuantitativeScale.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeQuantitativeScaleParameters;
end;

function TdxGaugeQuantitativeScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeQuantitativeScaleViewInfo;
end;

function TdxGaugeQuantitativeScale.GetValidValue(const AValue: Variant): Variant;
var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  AScaleParameters := GetParameters;
  Result := Min(Max(AValue, AScaleParameters.MinValue), AScaleParameters.MaxValue);
end;

procedure TdxGaugeQuantitativeScale.ApplyStyleParameters;
var
  AParameters: TdxGaugeQuantitativeScaleParameters;
  ADefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  inherited ApplyStyleParameters;
  ApplyStyleFontParameters;
  AParameters := GetParameters;
  ADefaultParameters := GetViewInfo.GetDefaultParameters;
  if not(spvShowFirstTick in FAssignedValues) then
    AParameters.ShowFirstTick := ADefaultParameters.ShowFirstTick;
  if not(spvShowLabels in FAssignedValues) then
    AParameters.ShowLabels := ADefaultParameters.ShowLabels;
  if not(spvShowLastTick in FAssignedValues) then
    AParameters.ShowLastTick := ADefaultParameters.ShowLastTick;
  if not(spvShowTicks in FAssignedValues) then
    AParameters.ShowTicks := ADefaultParameters.ShowTicks;
  if not(spvMajorTickCount in FAssignedValues) then
    AParameters.MajorTickCount := ADefaultParameters.MajorTickCount;
  if not(spvMinorTickCount in FAssignedValues) then
    AParameters.MinorTickCount := ADefaultParameters.MinorTickCount;
end;

procedure TdxGaugeQuantitativeScale.Calculate(const AScaleBounds: TdxRectF);
begin
  GetParameters.Font := FFont;
  inherited Calculate(AScaleBounds);
  CalculateRanges;
end;

procedure TdxGaugeQuantitativeScale.CreateSubClasses;
begin
  inherited CreateSubClasses;
  CreateAnimationController;
  CreateFont;
  CreateRanges;
end;

procedure TdxGaugeQuantitativeScale.DestroySubClasses;
begin
  FreeAndNil(FRanges);
  FreeAndNil(FFont);
  FreeAndNil(FAnimationController);
  inherited DestroySubClasses;
end;

procedure TdxGaugeQuantitativeScale.DoAssign(AScale: TdxGaugeCustomScale);
begin
  inherited DoAssign(AScale);
  OptionsAnimate.Assign((AScale as TdxGaugeQuantitativeScale).OptionsAnimate);
  Font.Assign((AScale as TdxGaugeQuantitativeScale).Font);
  Ranges.Assign((AScale as TdxGaugeQuantitativeScale).Ranges);
  FAssignedValues := (AScale as TdxGaugeQuantitativeScale).FAssignedValues;
end;

procedure TdxGaugeQuantitativeScale.DoSetValue(const AValue: Variant);
begin
  if not IsDesigning and AnimationController.OptionsAnimate.Enabled then
    AnimationController.Animate(Value, AValue)
  else
    inherited DoSetValue(AValue);
end;

procedure TdxGaugeQuantitativeScale.DrawScaleComponents(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  if ALayerIndex = 1 then
    DrawRanges(AGPGraphics, ALayerIndex);
  inherited DrawScaleComponents(AGPGraphics, ALayerIndex);
end;

procedure TdxGaugeQuantitativeScale.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
begin
  inherited GetChildren(AProc, ARoot);
  ProcessCollection(Ranges, AProc, ARoot);
end;

procedure TdxGaugeQuantitativeScale.InitParameters;
var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  inherited InitParameters;
  AScaleParameters := GetParameters;
  AScaleParameters.MaxValue := 100;
  AScaleParameters.MinValue := 0;
  AScaleParameters.Value := AScaleParameters.MinValue;
  AScaleParameters.ShowLabels := True;
  AScaleParameters.ShowValueIndicator := True;
end;

procedure TdxGaugeQuantitativeScale.InternalRestoreStyleParameters;
var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  FAssignedValues := [];
  AScaleParameters := GetParameters;
  AScaleParameters.ShowValueIndicator := True;
  AScaleParameters.ShowBackground := True;
  inherited InternalRestoreStyleParameters;
end;

procedure TdxGaugeQuantitativeScale.Loaded;
var
  AStyleName: string;
  AStoredFont: TFont;
begin
  inherited Loaded;
  if spvFontSize in FAssignedValues then
  begin
    Collection.BeginUpdate;
    AStoredFont := TFont.Create;
    try
      AStyleName := StyleName;
      AStoredFont.Assign(Font);
      StyleName := '';
      StyleName := AStyleName;
      FreeAndNil(FFont);
      CreateFont;
      Font.Assign(AStoredFont);
    finally
      AStoredFont.Free;
      Collection.EndUpdate;
    end;
  end;
end;

function TdxGaugeQuantitativeScale.GetAnimationControllerClass: TdxGaugeQuantitativeScaleAnimationControllerClass;
begin
  Result := TdxGaugeQuantitativeScaleAnimationController;
end;

function TdxGaugeQuantitativeScale.IsReading: Boolean;
begin
  Result := ComponentState * [csReading] <> [];
end;

function TdxGaugeQuantitativeScale.GetShowValueIndicator: Boolean;
begin
  Result := GetParameters.ShowValueIndicator;
end;

function TdxGaugeQuantitativeScale.GetValue: Single;
begin
  Result := Parameters.Value;
end;

procedure TdxGaugeQuantitativeScale.SetShowValueIndicator(const AValue: Boolean);
begin
  if GetParameters.ShowValueIndicator <> AValue then
  begin
    GetParameters.ShowValueIndicator := AValue;
    ScaleChanged([sclDynamicLayer, sclStaticLayer]);
  end;
end;

function TdxGaugeQuantitativeScale.GetLogarithmicBase: Single;
begin
  Result := GetParameters.LogarithmicBase;
end;

function TdxGaugeQuantitativeScale.GetFont: TFont;
begin
  Result := FFont;
end;

function TdxGaugeQuantitativeScale.GetMajorTickCount: Integer;
begin
  Result := GetParameters.MajorTickCount;
end;

function TdxGaugeQuantitativeScale.GetMaxValue: Single;
begin
  Result := GetParameters.MaxValue;
end;

function TdxGaugeQuantitativeScale.GetMinorTickCount: Integer;
begin
  Result := GetParameters.MinorTickCount;
end;

function TdxGaugeQuantitativeScale.GetMinValue: Single;
begin
  Result := GetParameters.MinValue;
end;

function TdxGaugeQuantitativeScale.GetOnGetTickmarkLabelDrawInfo: TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent;
begin
  Result := GetViewInfo.OnGetTickmarkLabelDrawInfo;
end;

function TdxGaugeQuantitativeScale.GetOptionsAnimate: TdxGaugeQuantitativeScaleOptionsAnimate;
begin
  Result := FAnimationController.OptionsAnimate;
end;

function TdxGaugeQuantitativeScale.GetShowFirstTick: Boolean;
begin
  Result := GetParameters.ShowFirstTick;
end;

function TdxGaugeQuantitativeScale.GetShowLabels: Boolean;
begin
  Result := GetParameters.ShowLabels;
end;

function TdxGaugeQuantitativeScale.GetShowLastTick: Boolean;
begin
  Result := GetParameters.ShowLastTick;
end;

function TdxGaugeQuantitativeScale.GetShowTicks: Boolean;
begin
  Result := GetParameters.ShowTicks;
end;

procedure TdxGaugeQuantitativeScale.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxGaugeQuantitativeScale.SetLogarithmicBase(const AValue: Single);
begin
  if (GetParameters.LogarithmicBase <> AValue) and (AValue >= 0) and (AValue <> 1) then
  begin
    GetParameters.LogarithmicBase := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetMajorTickCount(const AValue: Integer);
begin
  if (GetParameters.MajorTickCount <> AValue) or IsReading then
  begin
    GetParameters.MajorTickCount := Max(AValue, 2);
    if (GetParameters.MajorTickCount = GetViewInfo.GetDefaultParameters.MajorTickCount) and
      not IsReading then
      Exclude(FAssignedValues, spvMajorTickCount)
    else
      Include(FAssignedValues, spvMajorTickCount);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetMaxValue(const AValue: Single);
var
  AParameters: TdxGaugeQuantitativeScaleParameters;
begin
  AParameters := GetParameters;
  if (AParameters.MaxValue <> AValue) and ((AValue > AParameters.MinValue) or IsReading) then
  begin
    AParameters.MaxValue := AValue;
    CheckScaleValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetMinorTickCount(const AValue: Integer);
begin
  if (GetParameters.MinorTickCount <> AValue) and (AValue >= 0) or IsReading then
  begin
    GetParameters.MinorTickCount := AValue;
    if (GetParameters.MinorTickCount = GetViewInfo.GetDefaultParameters.MinorTickCount) and
      not IsReading then
      Exclude(FAssignedValues, spvMinorTickCount)
    else
      Include(FAssignedValues, spvMinorTickCount);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetMinValue(const AValue: Single);
var
  AParameters: TdxGaugeQuantitativeScaleParameters;
begin
  AParameters := GetParameters;
  if (AParameters.MinValue <> AValue) and ((AValue < AParameters.MaxValue) or IsReading) then
  begin
    AParameters.MinValue := AValue;
    CheckScaleValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetOnAnimate(const AValue: TNotifyEvent);
begin
  if not dxSameMethods(FOnAnimate, AValue) then
  begin
    FOnAnimate := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetOnAnimationComplete(const AValue: TNotifyEvent);
begin
  if not dxSameMethods(FOnAnimationComplete, AValue) then
  begin
    FOnAnimationComplete := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetOnGetTickmarkLabelDrawInfo(const AValue:
  TdxGaugeQuantitativeScaleGetTickmarkLabelDrawInfoEvent);
begin
  if not dxSameMethods(GetViewInfo.OnGetTickmarkLabelDrawInfo, AValue) then
  begin
    GetViewInfo.OnGetTickmarkLabelDrawInfo := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetOptionsAnimate(const AValue: TdxGaugeQuantitativeScaleOptionsAnimate);
begin
  FAnimationController.OptionsAnimate.Assign(AValue);
end;

procedure TdxGaugeQuantitativeScale.SetRanges(const AValue: TdxGaugeRangeCollection);
begin
  FRanges.Assign(AValue);
end;

procedure TdxGaugeQuantitativeScale.SetShowFirstTick(const AValue: Boolean);
begin
  if GetParameters.ShowFirstTick <> AValue then
  begin
    GetParameters.ShowFirstTick := AValue;
    if GetParameters.ShowFirstTick = GetViewInfo.GetDefaultParameters.ShowFirstTick then
      Exclude(FAssignedValues, spvShowFirstTick)
    else
      Include(FAssignedValues, spvShowFirstTick);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetShowLabels(const AValue: Boolean);
begin
  if GetParameters.ShowLabels <> AValue then
  begin
    GetParameters.ShowLabels := AValue;
    if GetParameters.ShowLabels = GetViewInfo.GetDefaultParameters.ShowLabels then
      Exclude(FAssignedValues, spvShowLabels)
    else
      Include(FAssignedValues, spvShowLabels);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetShowLastTick(const AValue: Boolean);
begin
  if GetParameters.ShowLastTick <> AValue then
  begin
    GetParameters.ShowLastTick := AValue;
    if GetParameters.ShowLastTick = GetViewInfo.GetDefaultParameters.ShowLastTick then
      Exclude(FAssignedValues, spvShowLastTick)
    else
      Include(FAssignedValues, spvShowLastTick);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetShowTicks(const AValue: Boolean);
begin
  if GetParameters.ShowTicks <> AValue then
  begin
    GetParameters.ShowTicks := AValue;
    if GetParameters.ShowTicks = GetViewInfo.GetDefaultParameters.ShowLastTick then
      Exclude(FAssignedValues, spvShowTicks)
    else
      Include(FAssignedValues, spvShowTicks);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.SetValue(const AValue: Single);
begin
  InternalSetValue(AValue);
end;

procedure TdxGaugeQuantitativeScale.AnimateHandler(ASender: TObject);
var
  AController: TdxGaugeQuantitativeScaleAnimationController;
begin
  AController := ASender as TdxGaugeQuantitativeScaleAnimationController;
  inherited DoSetValue(AController.Value);
  if AController.Animating then
    dxCallNotify(OnAnimate, Self);
end;

procedure TdxGaugeQuantitativeScale.AnimationCompleteHandler(ASender: TObject);
begin
  dxCallNotify(OnAnimationComplete, Self);
end;

procedure TdxGaugeQuantitativeScale.FontChangeHandler(ASender: TObject);
var
  ADefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  if LockCount = 0 then
  begin
    ADefaultParameters := GetViewInfo.GetDefaultParameters;
    if FFont.Name = ADefaultParameters.FontName then
      Exclude(FAssignedValues, spvFontName)
    else
      Include(FAssignedValues, spvFontName);
    if FFont.Color = ADefaultParameters.FontColor then
      Exclude(FAssignedValues, spvFontColor)
    else
      Include(FAssignedValues, spvFontColor);
    if not IsReading and (Font.Size = ADefaultParameters.FontSize) then
      Exclude(FAssignedValues, spvFontSize)
    else
      Include(FAssignedValues, spvFontSize);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeQuantitativeScale.RangeChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  ScaleChanged([sclDynamicLayer]);
end;

function TdxGaugeQuantitativeScale.GetParameters: TdxGaugeQuantitativeScaleParameters;
begin
  Result := Parameters as TdxGaugeQuantitativeScaleParameters;
end;

function TdxGaugeQuantitativeScale.GetViewInfo: TdxGaugeQuantitativeScaleViewInfo;
begin
  Result := inherited ViewInfo as TdxGaugeQuantitativeScaleViewInfo;
end;

procedure TdxGaugeQuantitativeScale.ApplyStyleFontParameters;
var
  ADefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  if Assigned(FFont) then
  begin
    BeginUpdate;
    ADefaultParameters := GetViewInfo.GetDefaultParameters;
    if not(spvFontName in FAssignedValues) then
      FFont.Name := ADefaultParameters.FontName;
    if not IsReading and not (spvFontSize in FAssignedValues) then
      FFont.Size := ADefaultParameters.FontSize;
    if not(spvFontColor in FAssignedValues) then
      FFont.Color := ADefaultParameters.FontColor;
    CancelUpdate;
  end;
end;

procedure TdxGaugeQuantitativeScale.CalculateRanges;
var
  I: Integer;
begin
  for I := 0 to Ranges.Count - 1 do
    Ranges[I].Calculate(GetViewInfo.ScaleInfo as TdxGaugeQuantitativeScaleInfo);
end;

procedure TdxGaugeQuantitativeScale.CheckScaleValue;
var
  AScaleParameters: TdxGaugeQuantitativeScaleParameters;
begin
  AScaleParameters := GetParameters;
  AScaleParameters.Value := Min(Max(AScaleParameters.Value, AScaleParameters.MinValue), AScaleParameters.MaxValue);
end;

procedure TdxGaugeQuantitativeScale.CreateAnimationController;
begin
  FAnimationController := GetAnimationControllerClass.Create(Self);
  FAnimationController.OnAnimate := AnimateHandler;
  FAnimationController.OnAnimationComplete := AnimationCompleteHandler;
end;

procedure TdxGaugeQuantitativeScale.CreateFont;
begin
  FFont := TFont.Create;
  FFont.PixelsPerInch := dxDefaultDPI;
  FFont.OnChange := FontChangeHandler;
end;

procedure TdxGaugeQuantitativeScale.CreateRanges;
begin
  FRanges := TdxGaugeRangeCollection.Create(Self, GetRangeClass);
  FRanges.OnChange := RangeChangeHandler;
end;

procedure TdxGaugeQuantitativeScale.DrawRanges(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to Ranges.Count - 1 do
    Ranges[I].ViewInfo.Draw(AGPGraphics);
end;

function TdxGaugeQuantitativeScale.IsFontStored: Boolean;
begin
  Result := (spvFontColor in FAssignedValues) or (spvFontName in FAssignedValues) or (spvFontSize in FAssignedValues);
end;

function TdxGaugeQuantitativeScale.IsMajorTickCountStored: Boolean;
begin
  Result := spvMajorTickCount in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsMaxValueStored: Boolean;
begin
  Result := not SameValue(GetParameters.MaxValue, 100);
end;

function TdxGaugeQuantitativeScale.IsMinorTickCountStored: Boolean;
begin
  Result := spvMinorTickCount in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsShowFirstTickStored: Boolean;
begin
  Result := spvShowFirstTick in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsShowLabelsStored: Boolean;
begin
  Result := spvShowLabels in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsShowLastTickStored: Boolean;
begin
  Result := spvShowLastTick in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsShowTicksStored: Boolean;
begin
  Result := spvShowTicks in FAssignedValues;
end;

function TdxGaugeQuantitativeScale.IsValueStored: Boolean;
begin
  Result := not SameValue(GetParameters.Value, 0);
end;

{ TdxGaugeQuantitativeScaleStyleReader }

function TdxGaugeQuantitativeScaleStyleReader.GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass;
begin
  Result := TdxGaugeQuantitativeScaleDefaultParameters;
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeCustomScaleDefaultParameters);
var
  AScaleParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  AScaleParameters := AParameters as TdxGaugeQuantitativeScaleDefaultParameters;
  ReadFontParameters(ANode, AScaleParameters);
  ReadLabelsParameters(ANode, AScaleParameters);
  ReadTicksVisibility(ANode, AScaleParameters);
  ReadMajorTicksParameters(ANode, AScaleParameters);
  ReadMinorTicksParameters(ANode, AScaleParameters);
  ReadCustomQuantitativeScaleParameters(ANode, AScaleParameters);
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadCommonScaleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  AChildNode: TdxXMLNode;
begin
  AChildNode := GetChildNode(ANode, 'RotateTicks');
  if AChildNode <> nil then
    AParameters.RotateTicks := GetAttributeValueAsBoolean(AChildNode, 'Value');
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadCustomQuantitativeScaleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
begin
  ReadCommonScaleParameters(ANode, AParameters);
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadFontParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeCustomScaleDefaultParameters);
var
  AFontNode: TdxXMLNode;
  AScaleDefaultParameters: TdxGaugeQuantitativeScaleDefaultParameters;
begin
  if GetChildNode(ANode, 'Font', AFontNode) then
  begin
    AScaleDefaultParameters := AParameters as TdxGaugeQuantitativeScaleDefaultParameters;
    AScaleDefaultParameters.FontColor := GetAttributeValueAsColor(GetChildNode(AFontNode, 'Color'), 'Value');
    AScaleDefaultParameters.FontName := GetAttributeValueAsString(GetChildNode(AFontNode, 'Name'), 'Value');
    AScaleDefaultParameters.FontSize := GetAttributeValueAsInteger(GetChildNode(AFontNode, 'Size'), 'Value');
  end;
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadLabelsParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  ALabelsNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'Labels', ALabelsNode) then
  begin
    AParameters.LabelOffset := GetAttributeValueAsDouble(GetChildNode(ALabelsNode, 'Offset'), 'Value');
    AParameters.ShowLabels := GetAttributeValueAsBoolean(GetChildNode(ALabelsNode, 'ShowLabels'), 'Value');
  end;
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadMajorTicksParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  AMajorTicksNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'MajorTicks', AMajorTicksNode) then
  begin
    AParameters.MajorTickCount := GetAttributeValueAsInteger(GetChildNode(AMajorTicksNode, 'Count'), 'Value');
    AParameters.MajorTickOffset := GetAttributeValueAsDouble(GetChildNode(AMajorTicksNode, 'Offset'), 'Value');
    AParameters.MajorTickScaleFactor := GetAttributeValueAsPointF(GetChildNode(AMajorTicksNode, 'ScaleFactor'), 'Value');
    AParameters.MajorTickType := GetAttributeValueAsElementType(GetChildNode(AMajorTicksNode, 'TickType'), 'Value');
  end;
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadMinorTicksParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  AMinorTicksNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'MinorTicks', AMinorTicksNode) then
  begin
    AParameters.MinorTickCount := GetAttributeValueAsInteger(GetChildNode(AMinorTicksNode, 'Count'), 'Value');
    AParameters.MinorTickOffset := GetAttributeValueAsDouble(GetChildNode(AMinorTicksNode, 'Offset'), 'Value');
    AParameters.MinorTickScaleFactor := GetAttributeValueAsPointF(GetChildNode(AMinorTicksNode, 'ScaleFactor'), 'Value');
    AParameters.MinorTickType := GetAttributeValueAsElementType(GetChildNode(AMinorTicksNode, 'TickType'), 'Value');
  end;
end;

procedure TdxGaugeQuantitativeScaleStyleReader.ReadTicksVisibility(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  ATicksVisibilityNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'TicksVisibility', ATicksVisibilityNode) then
  begin
    AParameters.AllowOverlapMajorTicks := GetAttributeValueAsBoolean(GetChildNode(ATicksVisibilityNode,
      'AllowOverlapMajorTicks'), 'Value');
    AParameters.ShowFirstTick := GetAttributeValueAsBoolean(GetChildNode(ATicksVisibilityNode, 'ShowFirstTick'), 'Value');
    AParameters.ShowLastTick := GetAttributeValueAsBoolean(GetChildNode(ATicksVisibilityNode, 'ShowLastTick'), 'Value');
    AParameters.ShowTicks := GetAttributeValueAsBoolean(GetChildNode(ATicksVisibilityNode, 'ShowTicks'), 'Value');
  end;
end;

{ TdxGaugeCustomRangeViewInfo }

constructor TdxGaugeCustomRangeViewInfo.Create;
begin
  inherited Create;
  FParameters := GetParametersClass.Create;
end;

destructor TdxGaugeCustomRangeViewInfo.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

function TdxGaugeCustomRangeViewInfo.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeCustomRangeParameters;
end;

procedure TdxGaugeCustomRangeViewInfo.CalculateSelectionBounds;
var
  AMatrix: TdxGPMatrix;
begin
  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Scale(FScaleInfo.ScaleFactor, cxRectCenter(FScaleInfo.Bounds));
    FSelectionBounds := AMatrix.TransformRect(FBounds);
  finally
    AMatrix.Free;
  end;
end;

function TdxGaugeCustomRangeViewInfo.GetSelectionRect: TRect;
begin
  Result := cxRectAdjust(cxRect(FSelectionBounds, False));
end;

function TdxGaugeCustomRangeViewInfo.GetValueRange: Single;
begin
  Result := dxGaugeValueRange(FScaleInfo.MaxValue, FScaleInfo.MinValue);
end;

procedure TdxGaugeCustomRangeViewInfo.Calculate(AParameters: TdxGaugeCustomRangeParameters;
  AScaleInfo: TdxGaugeCustomScaleInfo);
var
  AScaleValuePosition: Single;
begin
  FScaleInfo := AScaleInfo as TdxGaugeQuantitativeScaleInfo;
  FParameters.Assign(AParameters);
  FParameters.ValueEndPosition := FScaleInfo.GetValuePositionFunc(FParameters.ValueEnd);
  FParameters.ValueStartPosition := FScaleInfo.GetValuePositionFunc(FParameters.ValueStart);
  AScaleValuePosition := FScaleInfo.GetValuePositionFunc(AScaleInfo.Value);
  case FParameters.LinkedWithScaleValue of
    rlsvValueEnd:
      FParameters.ValueEndPosition := AScaleValuePosition;
    rlsvValueStart:
      FParameters.ValueStartPosition := AScaleValuePosition;
  end;
  CalculateBounds;
end;

procedure TdxGaugeCustomRangeViewInfo.Draw(AGPGraphics: TdxGPGraphics);
begin
  if FParameters.Visible and (FBounds.Width > 0) and (FBounds.Height > 0) then
    DoDraw(AGPGraphics);
end;

{ TdxGaugeCustomRange }

constructor TdxGaugeCustomRange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParameters := GetParametersClass.Create;
  FParameters.Color := dxColorToAlphaColor(clDefault);
  FParameters.Visible := True;
  FParameters.WidthFactor := dxGaugeCustomRangeWidthFactor;
  FViewInfo := GetViewInfoClass.Create;
end;

destructor TdxGaugeCustomRange.Destroy;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TdxGaugeCustomRange.Assign(ASource: TPersistent);
begin
  if ASource is TdxGaugeCustomRange then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(TdxGaugeCustomRange(ASource));
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(ASource);
end;

function TdxGaugeCustomRange.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxGaugeQuantitativeScale).Ranges;
end;

procedure TdxGaugeCustomRange.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('WidthFactor', ReadWidthFactor, WriteWidthFactor, SameValue(WidthFactor, 0));
end;

function TdxGaugeCustomRange.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeCustomRangeParameters;
end;

function TdxGaugeCustomRange.GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass;
begin
  Result := TdxGaugeCustomRangeViewInfo;
end;

function TdxGaugeCustomRange.GetSelectionContentRect: TRect;
begin
  Result := GetSelectionRect;
end;

function TdxGaugeCustomRange.GetSelectionRect: TRect;
begin
  Result := ViewInfo.GetSelectionRect;
end;

function TdxGaugeCustomRange.GetSelectionMarkers: TRects;
begin
  SetLength(Result, 0);
end;

function TdxGaugeCustomRange.GetSelectorRect: TRect;
begin
  Result := TdxControlsDesignSelectorHelper.CalculateBounds(
    GetSelectionRect, 2, 8, dxGetScaleFactor(Collection.ParentComponent));
end;

function TdxGaugeCustomRange.IsSizable: Boolean;
begin
  Result := False;
end;

function TdxGaugeCustomRange.IsDesigning: Boolean;
begin
  Result := ComponentState * [csDesigning] <> [];
end;

procedure TdxGaugeCustomRange.Calculate(AScaleInfo: TdxGaugeCustomScaleInfo);
var
  AInfo: TdxGaugeQuantitativeScaleInfo;
begin
  AInfo := AScaleInfo as TdxGaugeQuantitativeScaleInfo;
  FParameters.ValueEnd := Max(Min(FParameters.ValueEnd, AInfo.MaxValue), AInfo.MinValue);
  FParameters.ValueStart := Max(Min(FParameters.ValueStart, AInfo.MaxValue), AInfo.MinValue);
  ViewInfo.Calculate(FParameters, AScaleInfo);
  if IsDesigning then
    ViewInfo.CalculateSelectionBounds;
end;

function TdxGaugeCustomRange.GetColor: TdxAlphaColor;
begin
  Result := FParameters.Color;
end;

function TdxGaugeCustomRange.GetLinkedWithScaleValue: TdxGaugeScaleRangeValueLinkedWithScaleValue;
begin
  Result := FParameters.LinkedWithScaleValue;
end;

function TdxGaugeCustomRange.GetValueEnd: Single;
begin
  Result := FParameters.ValueEnd;
end;

function TdxGaugeCustomRange.GetValueStart: Single;
begin
  Result := FParameters.ValueStart;
end;

function TdxGaugeCustomRange.GetVisible: Boolean;
begin
  Result := FParameters.Visible;
end;

function TdxGaugeCustomRange.GetWidthFactor: Single;
begin
  Result := FParameters.WidthFactor;
end;

procedure TdxGaugeCustomRange.SetColor(const AValue: TdxAlphaColor);
begin
  if FParameters.Color <> AValue then
  begin
    FParameters.Color := AValue;
    Changed(False);
  end;
end;

procedure TdxGaugeCustomRange.SetLinkedWithScaleValue(const AValue: TdxGaugeScaleRangeValueLinkedWithScaleValue);
begin
  if FParameters.LinkedWithScaleValue <> AValue then
  begin
    FParameters.LinkedWithScaleValue := AValue;
    Changed(False);
  end;
end;

procedure TdxGaugeCustomRange.SetValueEnd(const AValue: Single);
begin
  if FParameters.ValueEnd <> AValue then
  begin
    FParameters.ValueEnd := GetValidValue(AValue);
    Changed(False);
  end;
end;

procedure TdxGaugeCustomRange.SetValueStart(const AValue: Single);
begin
  if FParameters.ValueStart <> AValue then
  begin
    FParameters.ValueStart := GetValidValue(AValue);
    Changed(False);
  end;
end;

procedure TdxGaugeCustomRange.SetVisible(const AValue: Boolean);
begin
  if FParameters.Visible <> AValue then
  begin
    FParameters.Visible := AValue;
    Changed(False);
  end;
end;

procedure TdxGaugeCustomRange.SetWidthFactor(const AValue: Single);
begin
  if not SameValue(FParameters.WidthFactor, AValue) then
  begin
    FParameters.WidthFactor := Min(Max(AValue, 0), 1);
    Changed(False);
  end;
end;

function TdxGaugeCustomRange.GetValidValue(AValue: Single): Single;

  function GetScale: TdxGaugeQuantitativeScale;
  begin
    Result := Collection.ParentComponent as TdxGaugeQuantitativeScale;
  end;

begin
  Result := Min(Max(AValue, GetScale.GetMinValue), GetScale.GetMaxValue);
end;

procedure TdxGaugeCustomRange.DoAssign(ARange: TdxGaugeCustomRange);
begin
  FParameters.Assign(ARange.Parameters);
end;

procedure TdxGaugeCustomRange.ReadWidthFactor(AReader: TReader);
begin
  WidthFactor := AReader.ReadDouble;
end;

procedure TdxGaugeCustomRange.WriteWidthFactor(AWriter: TWriter);
begin
  AWriter.WriteDouble(WidthFactor);
end;

function TdxGaugeCustomRange.IsColorStored: Boolean;
begin
  Result := FParameters.Color <> dxColorToAlphaColor(clDefault);
end;

function TdxGaugeCustomRange.IsWidthFactorStored: Boolean;
begin
  Result := not SameValue(RoundTo(FParameters.WidthFactor, -5), dxGaugeCustomRangeWidthFactor);
end;

{ TdxGaugeRangeCollection }

procedure TdxGaugeRangeCollection.Assign(ASource: TPersistent);
var
  I: Integer;
begin
  if ASource is TdxGaugeRangeCollection then
  begin
    Clear;
    for I := 0 to TdxGaugeRangeCollection(ASource).Count - 1 do
      Add;
    for I := 0 to TdxGaugeRangeCollection(ASource).Count - 1 do
      Items[I].Assign(TdxGaugeRangeCollection(ASource).Items[I]);
  end
  else
    inherited Assign(ASource);
end;

function TdxGaugeRangeCollection.GetItemPrefixName: string;
begin
  Result := LeftStr(ItemClass.ClassName, Length(ItemClass.ClassName) - Length('Range'));
end;

function TdxGaugeRangeCollection.GetRange(AIndex: Integer): TdxGaugeCustomRange;
begin
  Result := inherited GetItem(AIndex) as TdxGaugeCustomRange;
end;

procedure TdxGaugeRangeCollection.SetRange(AIndex: Integer; const AValue: TdxGaugeCustomRange);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TdxGaugeQuantitativeScaleCaptionOptionsView }

function TdxGaugeQuantitativeScaleCaptionOptionsView.IsFontStored: Boolean;
begin
  Result := UseOwnFont;
end;

procedure TdxGaugeQuantitativeScaleCaptionOptionsView.FontChangeHandler(ASender: TObject);
begin
  GetParameters.UseOwnFont := True;
  inherited FontChangeHandler(ASender);
end;

function TdxGaugeQuantitativeScaleCaptionOptionsView.GetUseOwnFont: Boolean;
begin
  Result := GetParameters.UseOwnFont;
end;

procedure TdxGaugeQuantitativeScaleCaptionOptionsView.SetUseOwnFont(const AValue: Boolean);
begin
  if GetParameters.UseOwnFont <> AValue then
  begin
    GetParameters.UseOwnFont := AValue;
    Changed;
  end;
end;

function TdxGaugeQuantitativeScaleCaptionOptionsView.GetParameters: TdxGaugeQuantitativeScaleCaptionParameters;
begin
  Result := Parameters as TdxGaugeQuantitativeScaleCaptionParameters;
end;

{ TdxGaugeQuantitativeScaleCaptionViewInfo }

function TdxGaugeQuantitativeScaleCaptionViewInfo.GetParametersClass: TdxGaugeCustomCaptionParametersClass;
begin
  Result := TdxGaugeQuantitativeScaleCaptionParameters;
end;

{ TdxGaugeQuantitativeScaleCaption }

function TdxGaugeQuantitativeScaleCaption.GetOptionsViewClass: TdxGaugeCustomCaptionOptionsViewClass;
begin
  Result := TdxGaugeQuantitativeScaleCaptionOptionsView;
end;

function TdxGaugeQuantitativeScaleCaption.GetParametersClass: TdxGaugeCustomCaptionParametersClass;
begin
  Result := TdxGaugeQuantitativeScaleCaptionParameters;
end;

function TdxGaugeQuantitativeScaleCaption.GetViewInfoClass: TdxGaugeCustomCaptionViewInfoClass;
begin
  Result := TdxGaugeQuantitativeScaleCaptionViewInfo;
end;

function TdxGaugeQuantitativeScaleCaption.GetFont(AScaleInfo: TdxGaugeCustomScaleInfo): TFont;
begin
  if OptionsView.UseOwnFont then
    Result := inherited GetFont(AScaleInfo)
  else
    Result := (AScaleInfo as TdxGaugeQuantitativeScaleInfo).Font;
end;

function TdxGaugeQuantitativeScaleCaption.GetOptionsView: TdxGaugeQuantitativeScaleCaptionOptionsView;
begin
  Result := inherited OptionsView as TdxGaugeQuantitativeScaleCaptionOptionsView;
end;

procedure TdxGaugeQuantitativeScaleCaption.SetOptionsView(const AValue: TdxGaugeQuantitativeScaleCaptionOptionsView);
begin
  inherited OptionsView := AValue;
end;

{ TdxGaugeQuantitativeScaleCustomOptionsAnimate }

constructor TdxGaugeQuantitativeScaleCustomOptionsAnimate.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTransitionEffect := ateAccelerateDecelerate;
  FFrameCount := dxGaugeScaleDefaultAnimationFrameCount;
  FInterval := dxGaugeScaleDefaultAnimationInterval;
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.Assign(ASource: TPersistent);
begin
  if ASource is TdxGaugeQuantitativeScaleCustomOptionsAnimate then
  begin
    TransitionEffect := (ASource as TdxGaugeQuantitativeScaleCustomOptionsAnimate).TransitionEffect;
    TransitionEffectMode := (ASource as TdxGaugeQuantitativeScaleCustomOptionsAnimate).TransitionEffectMode;
    Enabled := (ASource as TdxGaugeQuantitativeScaleCustomOptionsAnimate).Enabled;
    Interval := (ASource as TdxGaugeQuantitativeScaleCustomOptionsAnimate).Interval;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.Changed;
begin
  if Assigned(FOnChanged) then
    OnChanged(Self);
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.SetTransitionEffect(const AValue:
  TdxGaugeQuantitativeScaleAnimationTransitionEffect);
begin
  if FTransitionEffect <> AValue then
  begin
    FTransitionEffect := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.SetTransitionEffectMode(const AValue: TdxAnimationTransitionEffectMode);
begin
  if FTransitionEffectMode <> AValue then
  begin
    FTransitionEffectMode := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.SetFrameCount(const AValue: Integer);
begin
  if FFrameCount <> AValue then
  begin
    FFrameCount := AValue;
    Changed;
  end;
end;

procedure TdxGaugeQuantitativeScaleCustomOptionsAnimate.SetInterval(const AValue: Integer);
begin
  if (FInterval <> AValue) and (AValue > 0) then
  begin
    FInterval := AValue;
    Changed;
  end;
end;

{ TdxGaugeQuantitativeScaleAnimationController }

constructor TdxGaugeQuantitativeScaleAnimationController.Create(AOwner: TdxGaugeQuantitativeScale);
begin
  inherited Create;
  FScale := AOwner;
  FOptionsAnimate := GetOptionsAnimateClass.Create(FScale);
  FOptionsAnimate.OnChanged := OptionsAnimateChangeHandler;
end;

destructor TdxGaugeQuantitativeScaleAnimationController.Destroy;
begin
  FreeAndNil(FTransition);
  FreeAndNil(FOptionsAnimate);
  inherited Destroy;
end;

function TdxGaugeQuantitativeScaleAnimationController.GetOptionsAnimateClass: TdxGaugeQuantitativeScaleOptionsAnimateClass;
begin
  Result := TdxGaugeQuantitativeScaleOptionsAnimate;
end;

procedure TdxGaugeQuantitativeScaleAnimationController.Animate(ACurrentValue, ANextValue: Single);
begin
  FEndValue := ANextValue;
  FStartValue := ACurrentValue;
  FValueDelta := (FEndValue - FStartValue) / OptionsAnimate.FrameCount;
  if FAnimating then
  begin
    dxCallNotify(OnAnimationComplete, Self);
    FAnimating := False;
    RecreateTransition;
  end;
  FTransition.Finished := False;
  FTransition.Resume;
end;

function TdxGaugeQuantitativeScaleAnimationController.IsFinished(AAnimation: TdxAnimationTransition): Boolean;
begin
  Result := TdxAnimationTransitionAccess(AAnimation).Finish = TdxAnimationTransitionAccess(AAnimation).Current;
end;

procedure TdxGaugeQuantitativeScaleAnimationController.CreateTransition;
begin
  FTransition := TdxAnimationTransition.Create(OptionsAnimate.Interval, OptionsAnimate.TransitionEffect,
    OptionsAnimate.FrameCount, OptionsAnimate.TransitionEffectMode);
  FTransition.OnAnimate := AnimateHandler;
  FTransition.FreeOnTerminate := False;
end;

procedure TdxGaugeQuantitativeScaleAnimationController.RecreateTransition;
begin
  FreeAndNil(FTransition);
  CreateTransition;
end;

procedure TdxGaugeQuantitativeScaleAnimationController.AnimateHandler(ASender: TdxAnimationTransition;
  var APosition: Integer; var AFinished: Boolean);
begin
  AFinished := IsFinished(TdxAnimationTransition(ASender));
  FAnimating := not AFinished;
  if FAnimating then
    FValue := FStartValue + APosition * FValueDelta
  else
    FValue := FEndValue;
  dxCallNotify(OnAnimate, Self);
  if not FAnimating then
    dxCallNotify(OnAnimationComplete, Self);
end;

procedure TdxGaugeQuantitativeScaleAnimationController.OptionsAnimateChangeHandler(ASender: TObject);
begin
  RecreateTransition;
  if FAnimating and not OptionsAnimate.Enabled then
  begin
    FAnimating := False;
    FValue := FEndValue;
    dxCallNotify(OnAnimate, Self);
    dxCallNotify(OnAnimationComplete, Self);
  end;
  if FAnimating then
    Animate(Value, FEndValue);
end;

initialization
  Classes.RegisterClass(TdxGaugeQuantitativeScaleCaption);

finalization
  Classes.UnRegisterClass(TdxGaugeQuantitativeScaleCaption);

end.
