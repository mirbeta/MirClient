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

unit dxGaugeCustomScale;

{$I cxVer.inc}

interface

uses
  Windows, Graphics, Classes, Generics.Defaults, Generics.Collections, ImgList,
  dxCoreClasses, cxClasses, dxCore, cxGeometry, cxGraphics, dxCoreGraphics, dxGDIPlusClasses, dxXMLDoc, cxDataUtils,
  dxCompositeShape, dxShapePrimitives;

const
  dxGaugeAfricaSunsetStyleName = 'AfricaSunset';
  dxGaugeClassicStyleName = 'Classic';
  dxGaugeCleanWhiteStyleName = 'CleanWhite';
  dxGaugeCleverStyleName = 'Clever';
  dxGaugeCosmicStyleName = 'Cosmic';
  dxGaugeDarkNightStyleName = 'DarkNight';
  dxGaugeDeepFireStyleName = 'DeepFire';
  dxGaugeDiscoStyleName = 'Disco';
  dxGaugeEcoStyleName = 'Eco';
  dxGaugeFutureStyleName = 'Future';
  dxGaugeIceColdZoneStyleName = 'IceColdZone';
  dxGaugeiStyleStyleName = 'iStyle';
  dxGaugeMechanicalStyleName = 'Mechanical';
  dxGaugeMilitaryStyleName = 'Military';
  dxGaugeRedStyleName = 'Red';
  dxGaugeRetroStyleName = 'Retro';
  dxGaugeSilverBlurStyleName = 'SilverBlur';
  dxGaugeShiningDarkStyleName = 'ShiningDark';
  dxGaugeSmartStyleName = 'Smart';
  dxGaugeSportCarStyleName = 'SportCar';
  dxGaugeYellowSubmarineStyleName = 'YellowSubmarine';
  dxGaugeWhiteStyleName = 'White';

  dxGaugeScaleMinScaleFactor: TdxPointF = (X: 0.001; Y: 0.001);
type
  TdxGaugeCustomScale = class;
  TdxGaugeCustomScaleClass = class of TdxGaugeCustomScale;
  TdxGaugeCustomScaleDefaultParameters = class;
  TdxGaugeCustomScaleDefaultParametersClass = class of TdxGaugeCustomScaleDefaultParameters;
  TdxGaugeCustomScaleInfo = class;
  TdxGaugeCustomScaleInfoClass = class of TdxGaugeCustomScaleInfo;
  TdxGaugeCustomScaleOptionsView = class;
  TdxGaugeCustomScaleOptionsViewClass = class of TdxGaugeCustomScaleOptionsView;
  TdxGaugeCustomScaleParameters = class;
  TdxGaugeCustomScaleParametersClass = class of TdxGaugeCustomScaleParameters;
  TdxGaugeCustomScaleStyleReader = class;
  TdxGaugeCustomScaleStyleReaderClass = class of TdxGaugeCustomScaleStyleReader;
  TdxGaugeCustomScaleViewInfo = class;
  TdxGaugeCustomScaleViewInfoClass = class of TdxGaugeCustomScaleViewInfo;
  TdxGaugeCustomCaption = class;
  TdxGaugeCustomCaptionClass = class of TdxGaugeCustomCaption;
  TdxGaugeCustomCaptionParameters = class;
  TdxGaugeCustomCaptionParametersClass = class of TdxGaugeCustomCaptionParameters;
  TdxGaugeCustomCaptionOptionsView = class;
  TdxGaugeCustomCaptionOptionsViewClass = class of TdxGaugeCustomCaptionOptionsView;
  TdxGaugeCustomCaptionViewInfo = class;
  TdxGaugeCustomCaptionViewInfoClass = class of TdxGaugeCustomCaptionViewInfo;
  TdxGaugeCaptionCollection = class;
  TdxGaugeScaleOptionsCustomLayout = class;
  TdxGaugeScaleOptionsCustomLayoutClass = class of TdxGaugeScaleOptionsCustomLayout;
  TdxGaugeScaleStyle = class;
  TdxGaugeScaleStyleClass = class of TdxGaugeScaleStyle;

  TdxGaugeScaleChangedLayer = (sclStaticLayer, sclDynamicLayer);
  TdxGaugeScaleChangedLayers = set of TdxGaugeScaleChangedLayer;
  TdxGaugeScaleDragOperation = (sdoMove, sdoResizeTop, sdoResizeBottom, sdoResizeLeft, sdoResizeRight,
    sdoResizeTopLeft, sdoResizeBottomRight, sdoResizeTopRight, sdoResizeBottomLeft);
  TdxGaugeElementType = (etBackground1, etBackground2, etDigitalBackgroundStart, etDigitalBackgroundMiddle,
    etDigitalBackgroundEnd, etTick1, etTick2, etTick3, etTick4, etTick5, etSpindleCap, etNeedle, etLinearBarStart,
    etLinearBarEnd, etLinearBarEmpty, etLinearBarPacked);
  TdxGaugeScalePositionType = (sptFactor, sptPixels);
  TdxGaugeScaleType = (stCircularScale, stDigitalScale, stLinearScale, stCircularHalfScale, stCircularQuarterLeftScale,
    stCircularQuarterRightScale, stCircularThreeFourthScale, stCircularWideScale, stContainerScale);
  TdxGaugeScaleZOrderChangeType = (octBringToFront, octBringForward, octSendBackward, octSendToBack);

  TdxGaugeScaleLayerDrawProc = procedure(AGPGraphics: TdxGPGraphics) of object;

  { IdxGaugeSelectableElement }

  IdxGaugeSelectableElement = interface
  ['{CEECFD6E-888B-41A1-9559-CDD948830AD8}']
    function GetSelectionContentRect: TRect;
    function GetSelectionRect: TRect;
    function GetSelectionMarkers: TRects;
    function GetSelectorRect: TRect;
    function IsSizable: Boolean;
  end;

  { TdxGaugeCustomScaleParameters }

  TdxGaugeCustomScaleParameters = class
  public
    Value: Variant; // have to be top

    CenterPosition: TPoint;
    CenterPositionFactor: TdxPointF;
    CenterPositionType: TdxGaugeScalePositionType;
    Height: Integer;
    HeightFactor: Single;
    Width: Integer;
    WidthFactor: Single;
    ShowBackground: Boolean;
    Stretch: Boolean;
    Visible: Boolean;

    constructor Create; virtual;
    procedure Assign(ASource: TdxGaugeCustomScaleParameters); virtual;
  end;

  { TdxGaugeCustomScaleDefaultParameters }

  TdxGaugeCustomScaleDefaultParameters = class(TObject);

  { TdxGaugeCustomScaleInfo }

  TdxGaugeCustomScaleInfo = class
  public
    Value: Variant; // have to be top

    Bounds: TdxRectF;
    ContentBounds: TdxRectF;
    Selection: TRect;
    ScaleFactor: TdxPointF;

    constructor Create; virtual;
    procedure Assign(ASource: TdxGaugeCustomScaleInfo);
  end;

  { TdxGaugeScaleOptionsPersistent }

  TdxGaugeScaleOptionsPersistent = class(TcxOwnedPersistent)
  protected
    function GetScale: TdxGaugeCustomScale;
  end;

  { TdxGaugeScaleOptionsCustomLayout }

  TdxGaugeScaleOptionsCustomLayout = class(TdxGaugeScaleOptionsPersistent)
  private
    function GetCenterPositionFactorX: Single;
    function GetCenterPositionFactorY: Single;
    function GetCenterPositionX: Integer;
    function GetCenterPositionY: Integer;
    function GetCenterPositionType: TdxGaugeScalePositionType;
    function GetHeight: Integer;
    function GetHeightFactor: Single;
    function GetStretch: Boolean;
    function GetWidth: Integer;
    function GetWidthFactor: Single;
    procedure SetCenterPositionFactorX(const AValue: Single);
    procedure SetCenterPositionFactorY(const AValue: Single);
    procedure SetCenterPositionX(const AValue: Integer);
    procedure SetCenterPositionY(const AValue: Integer);
    procedure SetCenterPositionType(const AValue: TdxGaugeScalePositionType);
    procedure SetHeight(const AValue: Integer);
    procedure SetHeightFactor(const AValue: Single);
    procedure SetStretch(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
    procedure SetWidthFactor(const AValue: Single);

    procedure ReadCenterPositionFactorX(AReader: TReader);
    procedure ReadCenterPositionFactorY(AReader: TReader);
    procedure WriteCenterPositionFactorX(AWriter: TWriter);
    procedure WriteCenterPositionFactorY(AWriter: TWriter);

    function IsCenterPositionFactorXStored: Boolean;
    function IsCenterPositionFactorYStored: Boolean;
    function IsHeightFactorStored: Boolean;
    function IsWidthFactorStored: Boolean;
  protected
    procedure DefineProperties(AFiler: TFiler); override;

    property Height: Integer read GetHeight write SetHeight default 0;
    property HeightFactor: Single read GetHeightFactor write SetHeightFactor stored IsHeightFactorStored;
    property Stretch: Boolean read GetStretch write SetStretch default False;
    property Width: Integer read GetWidth write SetWidth default 0;
    property WidthFactor: Single read GetWidthFactor write SetWidthFactor stored IsWidthFactorStored;
  published
    property CenterPositionX: Integer read GetCenterPositionX write SetCenterPositionX default 0;
    property CenterPositionFactorX: Single read GetCenterPositionFactorX write SetCenterPositionFactorX
      stored IsCenterPositionFactorXStored;
    property CenterPositionY: Integer read GetCenterPositionY write SetCenterPositionY default 0;
    property CenterPositionFactorY: Single read GetCenterPositionFactorY write SetCenterPositionFactorY
      stored IsCenterPositionFactorYStored;
    property CenterPositionType: TdxGaugeScalePositionType read GetCenterPositionType
      write SetCenterPositionType default sptFactor;
  end;

  { TdxGaugeScaleOptionsRectangularLayout }

  TdxGaugeScaleOptionsRectangularLayout = class(TdxGaugeScaleOptionsCustomLayout)
  published
    property Height;
    property HeightFactor;
    property Stretch;
    property Width;
    property WidthFactor;
  end;

  { TdxGaugeCustomScaleOptionsView }

  TdxGaugeCustomScaleOptionsView = class(TdxGaugeScaleOptionsPersistent)
  private
    function GetShowBackground: Boolean;
    procedure SetShowBackground(const AValue: Boolean);
  published
    property ShowBackground: Boolean read GetShowBackground write SetShowBackground default True;
  end;

  { TdxGaugeCustomScaleViewInfo }

  TdxGaugeCustomScaleViewInfo = class
  private
    FStyle: TdxGaugeScaleStyle;
    FScaleInfo: TdxGaugeCustomScaleInfo;

    function GetStyleName: string;
    procedure SetStyle(const AValue: TdxGaugeScaleStyle);

    function GetDefaultParameters: TdxGaugeCustomScaleDefaultParameters;
    function GetParameters: TdxGaugeCustomScaleParameters;
    procedure DrawBackground(AGPGraphics: TdxGPGraphics);
    procedure RecreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
  protected
    FBackground: TdxCompositeShape;
    FParameters: TdxGaugeCustomScaleParameters;
    FScaleFactor: TdxPointF;

    function GetScaleInfoClass: TdxGaugeCustomScaleInfoClass; virtual;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; virtual;

    function GetContentBounds: TdxRectF; virtual;
    function GetContentOriginalSize: TdxSizeF; virtual;
    function GetLayoutSize(const ABounds: TdxRectF): TdxSizeF; virtual;
    procedure Calculate(AParameters: TdxGaugeCustomScaleParameters; const ABounds: TdxRectF); virtual;
    procedure CalculateBounds(const ABounds: TdxRectF); virtual;
    procedure CalculateContent; virtual;
    procedure CalculateScaleInfo; virtual;
    procedure CalculateScaleFactor; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); virtual;
    procedure PopulateParameters(AParameters: TdxGaugeCustomScaleParameters); virtual;

    function CanDrawBackground: Boolean; virtual;
    procedure CreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string); virtual;
    procedure DoDrawBackground(AGPGraphics: TdxGPGraphics); virtual;
    procedure LoadScaleElements; virtual;

    function CanDrawImage(AImage: TGraphic): Boolean;
    function GetBounds: TdxRectF;
    function GetCenter(const ABounds: TdxRectF): TdxPointF;
    function GetClientRect: TdxRectF;
    function GetScaledRect(const AOriginalSize: TdxSizeF): TdxRectF; overload;
    function GetScaledRect(const AOriginalSize: TdxSizeF; const AScaleFactor: TdxPointF): TdxRectF; overload;
    function NeedDrawElement: Boolean;
    function GetElementImage(AType: TdxGaugeElementType): TGraphic;
    function GetElementImageOriginalSize(AImage: TGraphic): TdxSizeF;
    function GetElementImageSize(AImage: TGraphic; const AImageScaleFactor: TdxPointF): TdxSizeF;
    procedure DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);

    property DefaultParameters: TdxGaugeCustomScaleDefaultParameters read GetDefaultParameters;
    property ScaleInfo: TdxGaugeCustomScaleInfo read FScaleInfo;
    property ScaleFactor: TdxPointF read FScaleFactor;
    property Style: TdxGaugeScaleStyle read FStyle write SetStyle;
    property StyleName: string read GetStyleName;
  public
    constructor Create(AScaleType: TdxGaugeScaleType; const AStyleName: string); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TdxGaugeCustomScale }

  TdxGaugeCustomScale = class(TcxComponentCollectionItem, IdxGaugeSelectableElement)
  private
    FAnchorScale: TdxGaugeCustomScale;
    FCaptions: TdxGaugeCaptionCollection;
    FChangedLayers: TdxGaugeScaleChangedLayers;
    FDataBinding: TcxCustomDataBinding;
    FLockCount: Integer;
    FOptionsLayout: TdxGaugeScaleOptionsCustomLayout;
    FOptionsView: TdxGaugeCustomScaleOptionsView;
    FParameters: TdxGaugeCustomScaleParameters;
    FStoredAnchorScaleIndex: Integer;
    FStoredZOrder: Integer;
    FStyleName: string;
    FViewInfo: TdxGaugeCustomScaleViewInfo;

    function GetAnchorScaleIndex: Integer;
    function GetBounds: TdxRectF;
    function GetCenterPositionType: TdxGaugeScalePositionType;
    function GetCenterPositionX: Integer;
    function GetCenterPositionFactorX: Single;
    function GetCenterPositionY: Integer;
    function GetCenterPositionFactorY: Single;
    function GetClientRect: TdxRectF;
    function GetStretch: Boolean;
    function GetVisible: Boolean;
    function GetZOrder: Integer;
    procedure SetAnchorScale(const AValue: TdxGaugeCustomScale);
    procedure SetAnchorScaleIndex(const AValue: Integer);
    procedure SetCenterPositionType(const AValue: TdxGaugeScalePositionType);
    procedure SetCenterPositionX(const AValue: Integer);
    procedure SetCenterPositionFactorX(const AValue: Single);
    procedure SetCenterPositionY(const AValue: Integer);
    procedure SetCenterPositionFactorY(const AValue: Single);
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsCustomLayout);
    procedure SetStretch(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);

    function GetAnchorScaleByIndex(AAnchorScaleIndex: Integer): TdxGaugeCustomScale;

    procedure ReadZOrder(AReader: TReader);
    procedure WriteZOrder(AWriter: TWriter);

    function IsAnchorScaleIndexStored: Boolean;
    function IsCenterPositionFactorXStored: Boolean;
    function IsCenterPositionFactorYStored: Boolean;
    function IsHeightFactorStored: Boolean;
    function IsWidthFactorStored: Boolean;

    function GetShowBackground: Boolean;
    function GetValue: Variant;
    procedure SetCaptions(const AValue: TdxGaugeCaptionCollection);
    procedure SetDataBinding(const AValue: TcxCustomDataBinding);
    procedure SetOptionsView(const AValue: TdxGaugeCustomScaleOptionsView);
    procedure SetShowBackground(const AValue: Boolean);
    procedure SetStyleName(const AValue: string);
    procedure SetValue(const AValue: Variant);

    procedure DataChangeHandler;
    procedure DataSetChangeHandler;
    procedure ScaleElementChangeHandler(ASender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification);

    function GetParameters: TdxGaugeCustomScaleParameters;
    procedure CalculateCaptions(AScaleInfo: TdxGaugeCustomScaleInfo);

    function IsStyleNameStored: Boolean;
  protected
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure DefineProperties(AFiler: TFiler); override;
    procedure Loaded; override;

    class function GetLayerCount: Integer; virtual;
    class function GetScaleName: string; virtual;
    class function GetScaleType: TdxGaugeScaleType; virtual;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; virtual;

    function GetCaptionClass: TdxGaugeCustomCaptionClass; virtual;
    function GetDataBindingClass: TcxCustomDataBindingClass; virtual;
    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; virtual; abstract;
    function GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass; virtual;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; virtual;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; virtual;

    function GetHeight: Integer; virtual;
    function GetHeightFactor: Single; virtual;
    function GetWidth: Integer; virtual;
    function GetWidthFactor: Single; virtual;
    procedure InternalSetValue(const AValue: Variant);
    procedure SetHeight(const AValue: Integer); virtual;
    procedure SetHeightFactor(const AValue: Single); virtual;
    procedure SetWidth(const AValue: Integer); virtual;
    procedure SetWidthFactor(const AValue: Single); virtual;

    function GetValueChangedLayers: TdxGaugeScaleChangedLayers; virtual;
    function GetValidValue(const AValue: Variant): Variant; virtual;
    procedure ApplyStyleParameters; virtual;
    procedure Calculate(const ABounds: TdxRectF); virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoAssign(AScale: TdxGaugeCustomScale); virtual;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); virtual;
    procedure DoSetValue(const AValue: Variant); virtual;
    procedure DrawCaptions(AGPGraphics: TdxGPGraphics); virtual;
    procedure DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); virtual;
    procedure DrawScaleComponents(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); virtual;
    procedure InitParameters; virtual;
    procedure InitStyleName; virtual;
    procedure InternalRestoreStyleParameters; virtual;

    // IdxGaugeSelectableElement
    function GetSelectionContentRect: TRect; virtual;
    function GetSelectionMarkers: TRects;
    function GetSelectionRect: TRect;
    function GetSelectorRect: TRect;
    function IsSizable: Boolean;

    function IsDesigning: Boolean;
    procedure AddChangeLayer(ALayer: TdxGaugeScaleChangedLayer);
    procedure BeginUpdate;
    procedure ScaleChanged(ALayers: TdxGaugeScaleChangedLayers);
    procedure ScaleFactorChanged(M, D: Integer); virtual;
    procedure EndUpdate;
    procedure CancelUpdate;
    procedure ProcessCollection(ACollection: TcxComponentCollection; AProc: TGetChildProc; ARoot: TComponent);

    // Internal properties
    property AnchorScale: TdxGaugeCustomScale read FAnchorScale write SetAnchorScale;
    property Bounds: TdxRectF read GetBounds;
    property CenterPositionX: Integer read GetCenterPositionX write SetCenterPositionX;
    property CenterPositionFactorX: Single read GetCenterPositionFactorX write SetCenterPositionFactorX;
    property CenterPositionY: Integer read GetCenterPositionY write SetCenterPositionY;
    property CenterPositionFactorY: Single read GetCenterPositionFactorY write SetCenterPositionFactorY;
    property CenterPositionType: TdxGaugeScalePositionType read GetCenterPositionType write SetCenterPositionType;
    property ClientRect: TdxRectF read GetClientRect;
    property ChangedLayers: TdxGaugeScaleChangedLayers read FChangedLayers write FChangedLayers;
    property Height: Integer read GetHeight write SetHeight;
    property HeightFactor: Single read GetHeightFactor write SetHeightFactor;
    property LockCount: Integer read FLockCount;
    property Parameters: TdxGaugeCustomScaleParameters read FParameters;
    property StoredZOrder: Integer read FStoredZOrder;
    property Width: Integer read GetWidth write SetWidth;
    property WidthFactor: Single read GetWidthFactor write SetWidthFactor;
    property ZOrder: Integer read GetZOrder;

    property AnchorScaleIndex: Integer read GetAnchorScaleIndex write SetAnchorScaleIndex stored
      IsAnchorScaleIndexStored;
    property Captions: TdxGaugeCaptionCollection read FCaptions write SetCaptions;
    property DataBinding: TcxCustomDataBinding read FDataBinding write SetDataBinding;
    property OptionsView: TdxGaugeCustomScaleOptionsView read FOptionsView write SetOptionsView;
    property OptionsLayout: TdxGaugeScaleOptionsCustomLayout read FOptionsLayout write SetOptionsLayout;
    property Value: Variant read GetValue write SetValue;
    property ViewInfo: TdxGaugeCustomScaleViewInfo read FViewInfo;
    property Visible: Boolean read GetVisible write SetVisible default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    procedure RestoreStyleParameters;

    property ScaleType: TdxGaugeScaleType read GetScaleType;
    property StyleName: string read FStyleName write SetStyleName stored IsStyleNameStored;
  end;

  { TdxGaugeCustomContainerScaleViewInfo }

  TdxGaugeCustomContainerScaleViewInfo = class(TdxGaugeCustomScaleViewInfo)
  private
    FCalculateExplicitSize: Boolean;
    FExplicitSize: TdxSizeF;
    FSizeFactor: TdxPointF;
  protected
    function CanDrawBackground: Boolean; override;
    function GetContentBounds: TdxRectF; override;
    function GetLayoutSize(const ABounds: TdxRectF): TdxSizeF; override;
    procedure CalculateScaleInfo; override;
    procedure CalculateScaleFactor; override;
    procedure CalculateSizeFactor(const ASize: TdxSizeF; const ABounds: TdxRectF);
    procedure CreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string); override;
    procedure DoDrawBackground(AGPGraphics: TdxGPGraphics); override;

    function GetExplicitSize(const ABounds: TdxRectF): TdxSizeF;
    function GetProportionalSize(const ASize: TdxSizeF): TdxSizeF;
    function GetRealHeightFactor: Single;
    function GetRealWidthFactor: Single;

    property CalculateExplicitSize: Boolean read FCalculateExplicitSize write FCalculateExplicitSize;
  public
    constructor Create(AScaleType: TdxGaugeScaleType; const AStyleName: string); override;
  end;

  { TdxGaugeCustomContainerScale }

  TdxGaugeCustomContainerScale = class(TdxGaugeCustomScale)
  private
    function GetViewInfo: TdxGaugeCustomContainerScaleViewInfo;
  protected
    function GetHeightFactor: Single; override;
    function GetWidthFactor: Single; override;
    procedure SetHeight(const AValue: Integer); override;
    procedure SetHeightFactor(const AValue: Single); override;
    procedure SetWidth(const AValue: Integer); override;
    procedure SetWidthFactor(const AValue: Single); override;

    class function GetLayerCount: Integer; override;
    class function GetScaleType: TdxGaugeScaleType; override;
    class function GetScaleName: string; override;

    // IdxGaugeSelectableElement
    function GetSelectionContentRect: TRect; override;

    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;
    procedure Calculate(const ABounds: TdxRectF); override;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure InitParameters; override;
    procedure InitStyleName; override;
  end;

  { TdxGaugeContainerScaleOptionsLayout }

  TdxGaugeContainerScaleOptionsLayout = class(TdxGaugeScaleOptionsCustomLayout)
  published
    property HeightFactor;
    property WidthFactor;
  end;

  { TdxGaugeContainerScale }

  TdxGaugeContainerScale = class(TdxGaugeCustomContainerScale)
  private
    function GetOptionsLayout: TdxGaugeContainerScaleOptionsLayout;
    procedure SetOptionsLayout(const AValue: TdxGaugeContainerScaleOptionsLayout);
  protected
    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;
  published
    property AnchorScaleIndex;
    property OptionsLayout: TdxGaugeContainerScaleOptionsLayout read GetOptionsLayout write SetOptionsLayout;
  end;

  { TdxGaugeScaleCollection }

  TdxGaugeScaleCollection = class(TcxComponentCollection)
  private
    FOnAdded: TNotifyEvent;
    FOnDeleted: TNotifyEvent;

    function GetScale(AIndex: Integer): TdxGaugeCustomScale;
    procedure SetScale(AIndex: Integer; const AValue: TdxGaugeCustomScale);
  protected
    function GetItemPrefixName: string; override;
    procedure Notify(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
    procedure ScaleFactorChanged(M, D: Integer); virtual;

    property OnAdded: TNotifyEvent read FOnAdded write FOnAdded;
    property OnDeleted: TNotifyEvent read FOnDeleted write FOnDeleted;
  public
    procedure Assign(ASource: TPersistent); override;

    function Add(AScaleClass: TdxGaugeCustomScaleClass): TdxGaugeCustomScale;

    property Items[Index: Integer]: TdxGaugeCustomScale read GetScale write SetScale; default;
  end;

  { TdxGaugeScaleStyle }

  TdxGaugeScaleStyle = class
  private
    FBaseStyleName: string;
    FDefaultParameters: TdxGaugeCustomScaleDefaultParameters;
    FElements: array [TdxGaugeElementType] of TdxCompositeShape;
    FName: string;
    FScaleTypeName: string;
    FShapes: TdxCompositeShape;
  protected
    function GetElement(AElementType: TdxGaugeElementType): TdxCompositeShape;
    procedure AddElement(AElementType: TdxGaugeElementType; AElement: TdxCompositeShape);

    property BaseStyleName: string read FBaseStyleName write FBaseStyleName;
    property DefaultParameters: TdxGaugeCustomScaleDefaultParameters read FDefaultParameters write FDefaultParameters;
    property Name: string read FName write FName;
    property Shapes: TdxCompositeShape read FShapes write FShapes;
    property ScaleTypeName: string read FScaleTypeName write FScaleTypeName;
  public
    constructor Create(const AStyleName: string); overload;
    constructor Create(const AStyleName, ABaseStyleName: string); overload;
    destructor Destroy; override;
  end;

  { TdxGaugeCustomScaleStyleReader }

  TdxGaugeCustomScaleStyleReader = class
  private
    function GetStyleRootNode(ADocument: TdxXMLDocument): TdxXMLNode;
    function ReadCompositeShape(ADocument: TdxXMLDocument): TdxCompositeShape;
    function ReadDefaultParameters(ADocument: TdxXMLDocument): TdxGaugeCustomScaleDefaultParameters;
  protected
    class function GetParametersNodeName: AnsiString;
    class function GetResourceNamePrefix: string; virtual;

    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; virtual; abstract;
    procedure ReadParameters(ANode: TdxXMLNode; AParameters: TdxGaugeCustomScaleDefaultParameters); virtual; abstract;

    function CreateStyle(ADocument: TdxXMLDocument): TdxGaugeScaleStyle;
    function GetAttributeValueAsAlphaColor(ANode: TdxXMLNode; const AAttributeName: AnsiString): TdxAlphaColor;
    function GetAttributeValueAsBoolean(ANode: TdxXMLNode; const AAttributeName: AnsiString): Boolean;
    function GetAttributeValueAsColor(ANode: TdxXMLNode; const AAttributeName: AnsiString): TColor;
    function GetAttributeValueAsDouble(ANode: TdxXMLNode; const AAttributeName: AnsiString): Double;
    function GetAttributeValueAsElementType(ANode: TdxXMLNode; const AAttributeName: AnsiString): TdxGaugeElementType;
    function GetAttributeValueAsInteger(ANode: TdxXMLNode; const AAttributeName: AnsiString): Integer;
    function GetAttributeValueAsPointF(ANode: TdxXMLNode; const AAttributeName: AnsiString): TdxPointF;
    function GetAttributeValueAsString(ANode: TdxXMLNode; const AAttributeName: AnsiString): string;
    function GetChildNode(ANode: TdxXMLNode; const AChildNodeName: AnsiString): TdxXMLNode; overload;
    function GetChildNode(ANode: TdxXMLNode; const AChildNodeName: AnsiString; out AChildNode: TdxXMLNode): Boolean;
      overload;
    function GetElementType(const ATypeName: string): TdxGaugeElementType;
  end;

  { TdxGaugeCustomCaptionParameters }

  TdxGaugeCustomCaptionParameters = class
  public
    Text: Variant; // have to be top

    Bounds: TdxRectF;
    CenterPositionFactor: TdxPointF;
    Font: Pointer;
    RotationAngle: Single;
    Visible: Boolean;

    constructor Create; virtual;
    procedure Assign(ASource: TdxGaugeCustomCaptionParameters);
  end;

  { TdxGaugeCustomCaptionOptionsPersistent }

  TdxGaugeCustomCaptionOptionsPersistent = class(TcxOwnedPersistent)
  private
    FParameters: TdxGaugeCustomCaptionParameters;
    FOnChange: TNotifyEvent;
  protected
    procedure InitParameters; virtual;
    procedure Changed;

    property Parameters: TdxGaugeCustomCaptionParameters read FParameters;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent; AParameters: TdxGaugeCustomCaptionParameters); reintroduce; virtual;
  end;

  { TdxGaugeCustomCaptionOptionsLayout }

  TdxGaugeCustomCaptionOptionsLayout = class(TdxGaugeCustomCaptionOptionsPersistent)
  private
    function GetCenterPositionFactorX: Single;
    function GetCenterPositionFactorY: Single;
    procedure SetCenterPositionFactorX(const AValue: Single);
    procedure SetCenterPositionFactorY(const AValue: Single);

    procedure ReadCenterPositionFactorX(AReader: TReader);
    procedure ReadCenterPositionFactorY(AReader: TReader);
    procedure WriteCenterPositionFactorX(AWriter: TWriter);
    procedure WriteCenterPositionFactorY(AWriter: TWriter);

    function IsCenterPositionFactorXStored: Boolean;
    function IsCenterPositionFactorYStored: Boolean;
  protected
    procedure DefineProperties(AFiler: TFiler); override;
  published
    property CenterPositionFactorX: Single read GetCenterPositionFactorX write SetCenterPositionFactorX stored
      IsCenterPositionFactorXStored;
    property CenterPositionFactorY: Single read GetCenterPositionFactorY write SetCenterPositionFactorY stored
      IsCenterPositionFactorYStored;
  end;

  { TdxGaugeCustomCaptionOptionsView }

  TdxGaugeCustomCaptionOptionsView = class(TdxGaugeCustomCaptionOptionsPersistent)
  private
    FFont: TFont;

    function GetRotationAngle: Single;
    procedure SetFont(const AValue: TFont);
    procedure SetRotationAngle(const AValue: Single);
  protected
    function IsFontStored: Boolean; virtual;
    procedure FontChangeHandler(ASender: TObject); virtual;
  public
    constructor Create(AOwner: TPersistent; AParameters: TdxGaugeCustomCaptionParameters); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
  end;

  { TdxGaugeCustomCaptionViewInfo }

  TdxGaugeCustomCaptionViewInfo = class
  private
    FParameters: TdxGaugeCustomCaptionParameters;
    FScaleBounds: TdxRectF;
    FScaleFactor: TdxPointF;
    FSelectionBounds: TdxRectF;

    function GetBounds: TdxRectF;
    function GetFont: TFont;
    procedure CalculateBounds(AScaleInfo: TdxGaugeCustomScaleInfo);
    procedure CalculateSelectionBounds;
    procedure DoDraw(AGPGraphics: TdxGPGraphics);
  protected
    function GetParametersClass: TdxGaugeCustomCaptionParametersClass; virtual;

    function GetTextCenter(AScaleInfo: TdxGaugeCustomScaleInfo): TdxPointF; virtual;
    procedure Calculate(AParameters: TdxGaugeCustomCaptionParameters; AScaleInfo: TdxGaugeCustomScaleInfo);

    function GetSelectionContentRect: TRect;
    procedure Draw(AGPGraphics: TdxGPGraphics);

    property Bounds: TdxRectF read GetBounds;
    property Parameters: TdxGaugeCustomCaptionParameters read FParameters;
    property ScaleFactor: TdxPointF read FScaleFactor;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TdxGaugeCustomCaption }

  TdxGaugeCustomCaption = class(TcxComponentCollectionItem, IdxGaugeSelectableElement)
  private
    FOptionsLayout: TdxGaugeCustomCaptionOptionsLayout;
    FOptionsView: TdxGaugeCustomCaptionOptionsView;
    FParameters: TdxGaugeCustomCaptionParameters;
    FViewInfo: TdxGaugeCustomCaptionViewInfo;

    function GetText: string;
    function GetVisible: Boolean;
    procedure SetOptionsLayout(const AValue: TdxGaugeCustomCaptionOptionsLayout);
    procedure SetOptionsView(const AValue: TdxGaugeCustomCaptionOptionsView);
    procedure SetText(const AValue: string);
    procedure SetVisible(const AValue: Boolean);

    procedure DoAssign(ACaption: TdxGaugeCustomCaption);
    procedure OptionsChangeHandler(ASender: TObject);
  protected
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;

    function GetOptionsViewClass: TdxGaugeCustomCaptionOptionsViewClass; virtual;
    function GetParametersClass: TdxGaugeCustomCaptionParametersClass; virtual;
    function GetViewInfoClass: TdxGaugeCustomCaptionViewInfoClass; virtual;

    function GetFont(AScaleInfo: TdxGaugeCustomScaleInfo): TFont; virtual;
    procedure Calculate(AScaleInfo: TdxGaugeCustomScaleInfo); virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    // IdxGaugeSelectableElement
    function GetSelectionContentRect: TRect;
    function GetSelectionRect: TRect;
    function GetSelectionMarkers: TRects;
    function GetSelectorRect: TRect;
    function IsSizable: Boolean;

    function IsDesigning: Boolean;
    // Internal properties
    property Parameters: TdxGaugeCustomCaptionParameters read FParameters;
    property ViewInfo: TdxGaugeCustomCaptionViewInfo read FViewInfo;

    property OptionsLayout: TdxGaugeCustomCaptionOptionsLayout read FOptionsLayout write SetOptionsLayout;
    property OptionsView: TdxGaugeCustomCaptionOptionsView read FOptionsView write SetOptionsView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  published
    property Text: string read GetText write SetText;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  { TdxGaugeCaptionCollection }

  TdxGaugeCaptionCollection = class(TcxComponentCollection)
  private
    function GetCaption(AIndex: Integer): TdxGaugeCustomCaption;
    procedure SetCaption(AIndex: Integer; const AValue: TdxGaugeCustomCaption);
  protected
    function GetItemPrefixName: string; override;

    property Items[Index: Integer]: TdxGaugeCustomCaption read GetCaption write SetCaption; default;
  public
    procedure Assign(ASource: TPersistent); override;
  end;

  // Scale Styles
function dxGaugeRegisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;
function dxGaugeRegisterStyleFromFile(AScaleType: TdxGaugeScaleType; const AFileName: string): Boolean;
procedure dxGaugeGetPredefinedStyleNames(AList: TStringList);
procedure dxGaugeGetRegisteredStyleNames(AScaleType: TdxGaugeScaleType; AList: TStringList);
procedure dxGaugeSaveStyleToFile(AScaleType: TdxGaugeScaleType; const AStyleName, AFileName: string);
procedure dxGaugeSaveStyleToStream(AScaleType: TdxGaugeScaleType; const AStyleName: string; AStream: TStream);
procedure dxGaugeUnregisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
procedure dxGaugeUnregisterStyles(AScaleType: TdxGaugeScaleType);

// Scales
procedure dxGaugeGetRegisteredScaleClasses(AList: TList);
procedure dxGaugeRegisterScale(AScaleClass: TdxGaugeCustomScaleClass; ARegisterPredefinedStyle: Boolean = True);
procedure dxGaugeUnregisterScale(AScaleClass: TdxGaugeCustomScaleClass; AUnregisterPredefinedStyle: Boolean = True);
procedure dxGaugeUnregisterScales;

implementation

uses
  {$IFDEF DELPHI16}
  System.UITypes,
  {$ENDIF}
  Types, SysUtils, StrUtils, Math, Variants, dxGDIPlusAPI, dxGaugeControl, dxGaugeUtils, dxDPIAwareUtils, cxControls;

{$R GaugeStyles.res}

const
  dxGaugeBackground1 = 'BackgroundLayer1';
  dxGaugeBackground2 = 'BackgroundLayer2';
  dxGaugeDigitalBackgroundStart = 'Back_Start';
  dxGaugeDigitalBackgroundMiddle = 'Back';
  dxGaugeDigitalBackgroundEnd = 'Back_End';
  dxGaugeLinearBarStart = 'BarStart';
  dxGaugeLinearBarEnd = 'BarEnd';
  dxGaugeLinearBarPacked = 'BarPacked';
  dxGaugeLinearBarEmpty = 'BarEmpty';
  dxGaugeTick1 = 'Tick1';
  dxGaugeTick2 = 'Tick2';
  dxGaugeTick3 = 'Tick3';
  dxGaugeTick4 = 'Tick4';
  dxGaugeTick5 = 'Tick5';
  dxGaugeSpindleCap = 'SpindleCap';
  dxGaugeNeedle = 'Needle';
  dxGaugeStyleElementNameMap: array [TdxGaugeElementType] of string = (dxGaugeBackground1, dxGaugeBackground2,
    dxGaugeDigitalBackgroundStart, dxGaugeDigitalBackgroundMiddle, dxGaugeDigitalBackgroundEnd, dxGaugeTick1,
    dxGaugeTick2, dxGaugeTick3, dxGaugeTick4, dxGaugeTick5, dxGaugeSpindleCap, dxGaugeNeedle, dxGaugeLinearBarStart,
    dxGaugeLinearBarEnd, dxGaugeLinearBarEmpty, dxGaugeLinearBarPacked);
  dxGaugeScaleDefaultValue = 0;

  dxGaugeDefaultCenterPositionFactor: TdxPointF = (X: 0.5; Y: 0.5);
  dxGaugeDefaultHeightFactor = 1;
  dxGaugeDefaultWidthFactor = 1;

  sdxGaugeScaleStyleCollectionInvalidStyleFileFormat =
    'The format of the style file does not correspond to the specified scale type';
  sdxGaugeScaleStyleCollectionInvalidStyleCount = 'No style is registered for the ' + '''' + '%s' + '''' +
    ' scale type';
  sdxGaugeScaleStyleCollectionDuplicateStyleName = 'Duplicate style name ' + '''' + '%s' + '''' +
    ' is found for the ' + '''' + '%s' + '''' + ' scale type';
  sdxGaugeScaleStyleCollectionBaseStyleUnregistered = 'The ' + '''' + '%s' + '''' +
    ' style used as the base for the ' + '''' + '%s' + '''' + ' style of the ' + '''' + '%s' + '''' +
    ' scale type is not registered for the ' + '''' + '%s' + '''' + ' scale type';
  sdxGaugeScaleFactoryDuplicateScaleClassName = 'Duplicate scale class name ' + '''' + '%s' + '''';

type
  TdxCompositeShapeAccess = class(TdxCompositeShape);
  TdxCustomGaugeControlAccess = class(TdxCustomGaugeControl);
  TdxCustomGaugeControlControllerAccess = class(TdxCustomGaugeControlController);
  TdxGaugeCustomElementViewInfoAccess = class(TdxGaugeCustomScaleViewInfo);

  { TdxGaugeScaleStyleInfo }

  TdxGaugeScaleStyleInfo = class
  public
    BaseStyleName: string;
    IsExternalStyle: Boolean;
    Name: string;
    ResourceName: string;
    ScaleType: TdxGaugeScaleType;
  end;

  { TdxGaugeScaleStyleCreator }

  TdxGaugeScaleStyleCreator = class
  private
    function CreateFromResource(AInstance: THandle; const AResName: string): TdxGaugeScaleStyle;
    function CreateFromStream(AStream: TStream): TdxGaugeScaleStyle;
    function CreateFromStyle(ADestinationStyleInfo, ASourceStyleInfo: TdxGaugeScaleStyleInfo): TdxGaugeScaleStyle;
    function GetStyleReaderClass(ADocument: TdxXMLDocument): TdxGaugeCustomScaleStyleReaderClass;
    function IdentifyReaderClass(ADefaultParametersNode: TdxXMLNode;
      var AReaderClass: TdxGaugeCustomScaleStyleReaderClass): Boolean;
    function InternalCreateStyleDocument(AStream: TStream): TdxXMLDocument;
    function IsStyleDocumentAvailable(ADocument: TdxXMLDocument): Boolean;
  protected
    function CreateStyle(AStyleInfo: TdxGaugeScaleStyleInfo): TdxGaugeScaleStyle; overload;
    function CreateStyle(AInstance: THandle; const AResName: string): TdxXMLDocument; overload;
    function CreateStyle(const AFileName: string): TdxGaugeScaleStyle; overload;
    function CreateStyleDocument(AStyleInfo: TdxGaugeScaleStyleInfo): TdxXMLDocument; overload;
    function CreateStyleDocument(const AFileName: string): TdxXMLDocument; overload;
  end;

  { TdxGaugeScaleStyleFactory }

  TdxGaugeScaleStyleFactory = class
  private
    FPredefinedStyleNames: TStringList;
    FStyleCreator: TdxGaugeScaleStyleCreator;
    FStyles: TDictionary<TdxGaugeScaleType, TStringList>;

    function CreateStyleInfo(const AFileName: string): TdxGaugeScaleStyleInfo;
    function CreateStyleList: TStringList;
    function ExtractStyleToDocument(AScaleType: TdxGaugeScaleType; const AStyleName: string): TdxXMLDocument;
    function GetStyleInfo(AStyles: TStringList; const AStyleName: string): TdxGaugeScaleStyleInfo;
    function InternalCreateStyle(AStyleInfo: TdxGaugeScaleStyleInfo): TdxGaugeScaleStyle;
    function InternalRegisterStyle(AScaleType: TdxGaugeScaleType; AStyleInfo: TdxGaugeScaleStyleInfo): Boolean;
    function InternalUnregisterStyle(AStyles: TStringList; const AStyleName: string): Boolean;
    function IsInheritedStyle(AScaleType: TdxGaugeScaleType): Boolean;
    function IsRegisteredStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;
    procedure DeleteStyle(AStyles: TStringList; AIndex: Integer);
    procedure InitPredefinedStyleNames;
    procedure InternalUnregisterStyles;
    procedure ProcessError(AStyleInfo: TdxGaugeScaleStyleInfo; AErrorCode: Integer);
  protected
    function CreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): TdxGaugeScaleStyle;
    function CreateStyleFromFile(const AFileName: string): TdxGaugeScaleStyle;
    function GetFirstStyleName(AScaleType: TdxGaugeScaleType): string;
    function GetStyles(AScaleType: TdxGaugeScaleType): TStringList;
    function HasStyles(AScaleType: TdxGaugeScaleType): Boolean;
    function RegisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;
    function RegisterStyleFromFile(AScaleType: TdxGaugeScaleType; const AFileName: string): Boolean;
    procedure GetPredefinedStyleNames(AList: TStringList);
    procedure RegisterPredefinedStyles(AScaleClass: TdxGaugeCustomScaleClass);
    procedure SaveStyleToFile(AScaleType: TdxGaugeScaleType; const AStyleName, AFileName: string);
    procedure SaveStyleToStream(AScaleType: TdxGaugeScaleType; const AStyleName: string; AStream: TStream);
    procedure UnregisterPredefinedStyles(AScaleClass: TdxGaugeCustomScaleClass);
    procedure UnregisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
    procedure UnregisterStyles(AScaleType: TdxGaugeScaleType);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxGaugeScaleFactory }

  TdxGaugeScaleFactory = class(TcxRegisteredClasses)
  protected
    function IsRegisteredScale(AScaleClass: TdxGaugeCustomScaleClass): Boolean;
    procedure RegisterScale(AScaleClass: TdxGaugeCustomScaleClass; ARegisterPredefinedStyle: Boolean = True);
    procedure UnregisterScale(AScaleClass: TdxGaugeCustomScaleClass; AUnregisterPredefinedStyle: Boolean = True);
    procedure UnregisterScales;
  end;

  { TdxGaugeScaleTypeInfo }

  TdxGaugeScaleTypeInfo = class
  public
    ReaderClass: TdxGaugeCustomScaleStyleReaderClass;
    ScaleType: TdxGaugeScaleType;
    ScaleTypeName: string;
  end;

  { TdxGaugeScaleTypeInfos }

  TdxGaugeScaleTypeInfos = class
  private
    FTypeInfos: TObjectList<TdxGaugeScaleTypeInfo>;

    function CreateTypeInfo(AReaderClass: TdxGaugeCustomScaleStyleReaderClass; AType: TdxGaugeScaleType;
      const ATypeName: string): TdxGaugeScaleTypeInfo;
  protected
    function GetTypeCount: Integer;
    function GetTypeByName(const ATypeName: string): TdxGaugeScaleType;
    function GetTypeInfo(AIndex: Integer): TdxGaugeScaleTypeInfo;
    function GetTypeName(AScaleType: TdxGaugeScaleType): string;
    function GetFormatedTypeName(AScaleType: TdxGaugeScaleType): string;
    procedure AddTypeInfo(AScaleClass: TdxGaugeCustomScaleClass);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  dxgGaugeScaleFactory: TdxGaugeScaleFactory;
  dxgGaugeScaleStyleFactory: TdxGaugeScaleStyleFactory;
  dxgGaugeScaleTypeInfos: TdxGaugeScaleTypeInfos;

function dxGaugeScaleFactory: TdxGaugeScaleFactory;
begin
  if dxgGaugeScaleFactory = nil then
    dxgGaugeScaleFactory := TdxGaugeScaleFactory.Create;
  Result := dxgGaugeScaleFactory;
end;

function dxGaugeScaleStyleFactory: TdxGaugeScaleStyleFactory;
begin
  if dxgGaugeScaleStyleFactory = nil then
    dxgGaugeScaleStyleFactory := TdxGaugeScaleStyleFactory.Create;
  Result := dxgGaugeScaleStyleFactory;
end;

function dxGaugeRegisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;
begin
  Result := dxGaugeScaleStyleFactory.RegisterStyle(AScaleType, AStyleName);
end;

function dxGaugeRegisterStyleFromFile(AScaleType: TdxGaugeScaleType; const AFileName: string): Boolean;
begin
  Result := dxGaugeScaleStyleFactory.RegisterStyleFromFile(AScaleType, AFileName);
end;

procedure dxGaugeGetPredefinedStyleNames(AList: TStringList);
begin
  AList.Clear;
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleStyleFactory.GetPredefinedStyleNames(AList);
end;

procedure dxGaugeGetRegisteredStyleNames(AScaleType: TdxGaugeScaleType; AList: TStringList);
begin
  AList.Clear;
  if dxgGaugeScaleFactory <> nil then
    AList.Assign(dxgGaugeScaleStyleFactory.GetStyles(AScaleType));
end;

procedure dxGaugeSaveStyleToFile(AScaleType: TdxGaugeScaleType; const AStyleName, AFileName: string);
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleStyleFactory.SaveStyleToFile(AScaleType, AStyleName, AFileName);
end;

procedure dxGaugeSaveStyleToStream(AScaleType: TdxGaugeScaleType; const AStyleName: string; AStream: TStream);
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleStyleFactory.SaveStyleToStream(AScaleType, AStyleName, AStream);
end;

procedure dxGaugeUnregisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleStyleFactory.UnregisterStyle(AScaleType, AStyleName);
end;

procedure dxGaugeUnregisterStyles(AScaleType: TdxGaugeScaleType);
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleStyleFactory.UnregisterStyles(AScaleType);
end;

procedure dxGaugeGetRegisteredScaleClasses(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  if dxgGaugeScaleFactory <> nil then
    for I := 0 to dxgGaugeScaleFactory.Count - 1 do
      AList.Add(dxgGaugeScaleFactory.Items[I]);
end;

procedure dxGaugeRegisterScale(AScaleClass: TdxGaugeCustomScaleClass; ARegisterPredefinedStyle: Boolean = True);
begin
  dxGaugeScaleFactory.RegisterScale(AScaleClass, ARegisterPredefinedStyle);
end;

procedure dxGaugeUnregisterScale(AScaleClass: TdxGaugeCustomScaleClass; AUnregisterPredefinedStyle: Boolean = True);
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleFactory.UnregisterScale(AScaleClass, AUnregisterPredefinedStyle);
end;

procedure dxGaugeUnregisterScales;
begin
  if dxgGaugeScaleFactory <> nil then
    dxgGaugeScaleFactory.UnregisterScales;
end;

function dxGaugeScaleTypeInfos: TdxGaugeScaleTypeInfos;
begin
  if dxgGaugeScaleTypeInfos = nil then
    dxgGaugeScaleTypeInfos := TdxGaugeScaleTypeInfos.Create;
  Result := dxgGaugeScaleTypeInfos;
end;

function dxGaugeController(ACollection: TdxGaugeScaleCollection): TdxCustomGaugeControlControllerAccess;
var
  AGaugeControl: TdxCustomGaugeControlAccess;
begin
  if ACollection <> nil then
  begin
    AGaugeControl := TdxCustomGaugeControlAccess(ACollection.ParentComponent as TdxCustomGaugeControl);
    Result := TdxCustomGaugeControlControllerAccess(AGaugeControl.Controller);
  end
  else
    Result := nil;
end;

{ TdxGaugeScaleTypeInfos }

constructor TdxGaugeScaleTypeInfos.Create;
begin
  inherited Create;
  FTypeInfos := TObjectList<TdxGaugeScaleTypeInfo>.Create(True);
end;

destructor TdxGaugeScaleTypeInfos.Destroy;
begin
  FreeAndNil(FTypeInfos);
  inherited Destroy;
end;

function TdxGaugeScaleTypeInfos.GetTypeCount: Integer;
begin
  Result := FTypeInfos.Count;
end;

function TdxGaugeScaleTypeInfos.GetTypeByName(const ATypeName: string): TdxGaugeScaleType;
var
  I: Integer;
begin
  Result := stCircularScale;
  for I := 0 to FTypeInfos.Count - 1 do
    if SameText(FTypeInfos[I].ScaleTypeName, ATypeName) then
    begin
      Result := FTypeInfos[I].ScaleType;
      Break;
    end;
end;

function TdxGaugeScaleTypeInfos.GetTypeInfo(AIndex: Integer): TdxGaugeScaleTypeInfo;
begin
  Result := FTypeInfos[AIndex];
end;

function TdxGaugeScaleTypeInfos.GetTypeName(AScaleType: TdxGaugeScaleType): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FTypeInfos.Count - 1 do
    if FTypeInfos[I].ScaleType = AScaleType then
    begin
      Result := FTypeInfos[I].ScaleTypeName;
      Break;
    end;
end;

function TdxGaugeScaleTypeInfos.GetFormatedTypeName(AScaleType: TdxGaugeScaleType): string;
var
  I: Integer;
  ASourceTypeName: string;
begin
  ASourceTypeName := GetTypeName(AScaleType);
  Result := '';
  for I := 1 to Length(ASourceTypeName) do
  begin
    if (UpperCase(ASourceTypeName[I]) = ASourceTypeName[I]) and (I <> 1) then
      Result := Result + ' ';
    Result := Result + ASourceTypeName[I];
  end;
end;

procedure TdxGaugeScaleTypeInfos.AddTypeInfo(AScaleClass: TdxGaugeCustomScaleClass);
var
  ATypeName: string;
begin
  ATypeName := StringReplace(AScaleClass.GetScaleName, ' ', '', [rfReplaceAll]);
  ATypeName := StringReplace(ATypeName, '-', '', [rfReplaceAll]);
  FTypeInfos.Add(CreateTypeInfo(AScaleClass.GetStyleReaderClass, AScaleClass.GetScaleType, ATypeName));
end;

function TdxGaugeScaleTypeInfos.CreateTypeInfo(AReaderClass: TdxGaugeCustomScaleStyleReaderClass;
  AType: TdxGaugeScaleType; const ATypeName: string): TdxGaugeScaleTypeInfo;
begin
  Result := TdxGaugeScaleTypeInfo.Create;
  Result.ReaderClass := AReaderClass;
  Result.ScaleType := AType;
  Result.ScaleTypeName := ATypeName;
end;

{ TdxGaugeCustomScaleParameters }

constructor TdxGaugeCustomScaleParameters.Create;
begin
  inherited Create;
  CenterPositionFactor := dxPointF(cxNullPoint);
end;

procedure TdxGaugeCustomScaleParameters.Assign(ASource: TdxGaugeCustomScaleParameters);
begin
  dxGaugeCopyParameters(ASource, Self, True);
  Value := ASource.Value
end;

{ TdxGaugeCustomElementInfo }

constructor TdxGaugeCustomScaleInfo.Create;
begin
  inherited Create;
  ScaleFactor := dxPointF(cxNullPoint);
end;

procedure TdxGaugeCustomScaleInfo.Assign(ASource: TdxGaugeCustomScaleInfo);
begin
  dxGaugeCopyParameters(ASource, Self, True);
  Value := ASource.Value;
end;


function TdxGaugeCustomScaleStyleReader.GetElementType(const ATypeName: string): TdxGaugeElementType;
var
  I: TdxGaugeElementType;
begin
  Result := etBackground1;
  for I := Low(dxGaugeStyleElementNameMap) to High(dxGaugeStyleElementNameMap) do
    if SameText(dxGaugeStyleElementNameMap[I], ATypeName) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxGaugeCustomScaleStyleReader.GetStyleRootNode(ADocument: TdxXMLDocument): TdxXMLNode;
begin
  Result := GetChildNode(ADocument.Root, 'ScaleStyle');
end;

function TdxGaugeCustomScaleStyleReader.ReadCompositeShape(ADocument: TdxXMLDocument): TdxCompositeShape;
var
  ACompositeShapeNode: TdxXMLNode;
begin
  if GetChildNode(GetStyleRootNode(ADocument), 'CompositeShape', ACompositeShapeNode) then
  begin
    Result := TdxCompositeShape.Create;
    TdxCompositeShapeAccess(Result).LoadFromNode(ACompositeShapeNode);
  end
  else
    Result := nil;
end;

function TdxGaugeCustomScaleStyleReader.ReadDefaultParameters(ADocument: TdxXMLDocument)
  : TdxGaugeCustomScaleDefaultParameters;
var
  ADefaultParametersNode: TdxXMLNode;
  ACustomScaleParametersNode: TdxXMLNode;
begin
  if GetChildNode(GetStyleRootNode(ADocument), 'DefaultParameters', ADefaultParametersNode) then
  begin
    Result := GetDefaultParametersClass.Create;
    if ADefaultParametersNode.FindChild(GetParametersNodeName, ACustomScaleParametersNode) then
      ReadParameters(ACustomScaleParametersNode, Result);
  end
  else
    Result := nil;
end;

{ TdxGaugeCustomCaptionParameters }

constructor TdxGaugeCustomCaptionParameters.Create;
begin
  inherited Create;
  CenterPositionFactor := dxPointF(cxNullPoint);
end;

procedure TdxGaugeCustomCaptionParameters.Assign(ASource: TdxGaugeCustomCaptionParameters);
begin
  dxGaugeCopyParameters(ASource, Self, True);
  Text := ASource.Text;
end;

{ TdxGaugeCustomCaptionOptionsPersistent }

constructor TdxGaugeCustomCaptionOptionsPersistent.Create(AOwner: TPersistent;
  AParameters: TdxGaugeCustomCaptionParameters);
begin
  inherited Create(AOwner);
  FParameters := AParameters;
  InitParameters;
end;

procedure TdxGaugeCustomCaptionOptionsPersistent.InitParameters;
begin
  // do nothing
end;

procedure TdxGaugeCustomCaptionOptionsPersistent.Changed;
begin
  dxCallNotify(OnChange, Self);
end;

{ TdxGaugeCustomCaptionOptionsLayout }

procedure TdxGaugeCustomCaptionOptionsLayout.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('CenterPositionFactorX', ReadCenterPositionFactorX, WriteCenterPositionFactorX,
    SameValue(CenterPositionFactorX, 0));
  AFiler.DefineProperty('CenterPositionFactorY', ReadCenterPositionFactorY, WriteCenterPositionFactorY,
    SameValue(CenterPositionFactorY, 0));
end;

function TdxGaugeCustomCaptionOptionsLayout.GetCenterPositionFactorX: Single;
begin
  Result := FParameters.CenterPositionFactor.X;
end;

function TdxGaugeCustomCaptionOptionsLayout.GetCenterPositionFactorY: Single;
begin
  Result := FParameters.CenterPositionFactor.Y;
end;

procedure TdxGaugeCustomCaptionOptionsLayout.SetCenterPositionFactorX(const AValue: Single);
begin
  if not SameValue(FParameters.CenterPositionFactor.X, AValue) then
  begin
    FParameters.CenterPositionFactor.X := AValue;
    Changed;
  end;
end;

procedure TdxGaugeCustomCaptionOptionsLayout.SetCenterPositionFactorY(const AValue: Single);
begin
  if not SameValue(FParameters.CenterPositionFactor.Y, AValue) then
  begin
    FParameters.CenterPositionFactor.Y := AValue;
    Changed;
  end;
end;

procedure TdxGaugeCustomCaptionOptionsLayout.ReadCenterPositionFactorX(AReader: TReader);
begin
  CenterPositionFactorX := AReader.ReadDouble;
end;

procedure TdxGaugeCustomCaptionOptionsLayout.ReadCenterPositionFactorY(AReader: TReader);
begin
  CenterPositionFactorY := AReader.ReadDouble;
end;

procedure TdxGaugeCustomCaptionOptionsLayout.WriteCenterPositionFactorX(AWriter: TWriter);
begin
  AWriter.WriteDouble(CenterPositionFactorX);
end;

procedure TdxGaugeCustomCaptionOptionsLayout.WriteCenterPositionFactorY(AWriter: TWriter);
begin
  AWriter.WriteDouble(CenterPositionFactorY);
end;

function TdxGaugeCustomCaptionOptionsLayout.IsCenterPositionFactorXStored: Boolean;
begin
  Result := not SameValue(FParameters.CenterPositionFactor.X, dxGaugeDefaultCenterPositionFactor.X);
end;

function TdxGaugeCustomCaptionOptionsLayout.IsCenterPositionFactorYStored: Boolean;
begin
  Result := not SameValue(FParameters.CenterPositionFactor.Y, dxGaugeDefaultCenterPositionFactor.Y);
end;

{ TdxGaugeCustomCaptionOptionsView }

constructor TdxGaugeCustomCaptionOptionsView.Create(AOwner: TPersistent; AParameters: TdxGaugeCustomCaptionParameters);
begin
  inherited Create(AOwner, AParameters);
  FFont := TFont.Create;
  FFont.OnChange := FontChangeHandler;
end;

destructor TdxGaugeCustomCaptionOptionsView.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TdxGaugeCustomCaptionOptionsView.IsFontStored: Boolean;
begin
  Result := True;
end;

procedure TdxGaugeCustomCaptionOptionsView.FontChangeHandler(ASender: TObject);
begin
  Changed;
end;

function TdxGaugeCustomCaptionOptionsView.GetRotationAngle: Single;
begin
  Result := FParameters.RotationAngle;
end;

procedure TdxGaugeCustomCaptionOptionsView.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxGaugeCustomCaptionOptionsView.SetRotationAngle(const AValue: Single);
begin
  if not SameValue(FParameters.RotationAngle, AValue) then
  begin
    FParameters.RotationAngle := AValue;
    Changed;
  end;
end;


{ TdxGaugeContainerScale }

function TdxGaugeContainerScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeContainerScaleOptionsLayout;
end;

function TdxGaugeContainerScale.GetOptionsLayout: TdxGaugeContainerScaleOptionsLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeContainerScaleOptionsLayout;
end;

procedure TdxGaugeContainerScale.SetOptionsLayout(const AValue: TdxGaugeContainerScaleOptionsLayout);
begin
  inherited OptionsLayout := AValue;
end;

{ TdxGaugeScaleCollection }

procedure TdxGaugeScaleCollection.Assign(ASource: TPersistent);
var
  I: Integer;
begin
  if ASource is TdxGaugeScaleCollection then
  begin
    Clear;
    for I := 0 to TdxGaugeScaleCollection(ASource).Count - 1 do
      Add(TdxGaugeCustomScaleClass(TdxGaugeScaleCollection(ASource).Items[I].ClassType));
    for I := 0 to TdxGaugeScaleCollection(ASource).Count - 1 do
      Items[I].Assign(TdxGaugeScaleCollection(ASource).Items[I]);
  end
  else
    inherited Assign(ASource);
end;

function TdxGaugeScaleCollection.Add(AScaleClass: TdxGaugeCustomScaleClass): TdxGaugeCustomScale;
begin
  Result := inherited Add(AScaleClass) as TdxGaugeCustomScale;
end;

function TdxGaugeScaleCollection.GetItemPrefixName: string;
begin
  Result := 'TdxGauge';
end;

procedure TdxGaugeScaleCollection.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  if (ParentComponent <> nil) and not(csDestroying in ParentComponent.ComponentState) then
    case AAction of
      ccnAdded:
        dxCallNotify(OnAdded, AItem);
      ccnExtracted, ccnDeleting:
        dxCallNotify(OnDeleted, AItem);
    end;
  inherited Notify(AItem, AAction);
end;

procedure TdxGaugeScaleCollection.ScaleFactorChanged(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ScaleFactorChanged(M, D);
end;

function TdxGaugeScaleCollection.GetScale(AIndex: Integer): TdxGaugeCustomScale;
begin
  Result := inherited GetItem(AIndex) as TdxGaugeCustomScale;
end;

procedure TdxGaugeScaleCollection.SetScale(AIndex: Integer; const AValue: TdxGaugeCustomScale);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TdxGaugeScaleStyle }

constructor TdxGaugeScaleStyle.Create(const AStyleName: string);
begin
  inherited Create;
  FName := AStyleName;
end;

constructor TdxGaugeScaleStyle.Create(const AStyleName, ABaseStyleName: string);
begin
  Create(AStyleName);
  FBaseStyleName := ABaseStyleName;
end;

destructor TdxGaugeScaleStyle.Destroy;
var
  I: TdxGaugeElementType;
begin
  for I := Low(FElements) to High(FElements) do
  begin
    FElements[I].Free;
    FElements[I] := nil;
  end;
  FreeAndNil(FDefaultParameters);
  FreeAndNil(FShapes);
  inherited Destroy;
end;

function TdxGaugeScaleStyle.GetElement(AElementType: TdxGaugeElementType): TdxCompositeShape;
begin
  if FElements[AElementType] = nil then
  begin
    FElements[AElementType] := TdxCompositeShape.Create;
    TdxCompositeShapeAccess(FElements[AElementType]).LoadFromShape(FShapes, dxGaugeStyleElementNameMap[AElementType]);
    if FElements[AElementType].Empty then
      FreeAndNil(FElements[AElementType]);
  end;
  Result := FElements[AElementType];
end;

procedure TdxGaugeScaleStyle.AddElement(AElementType: TdxGaugeElementType; AElement: TdxCompositeShape);
begin
  if FElements[AElementType] <> nil then
    FreeAndNil(FElements[AElementType]);
  FElements[AElementType] := AElement;
end;

{ TdxGaugeCustomScaleStyleReader }

class function TdxGaugeCustomScaleStyleReader.GetParametersNodeName: AnsiString;
begin
  Result := dxStringToAnsiString(GetResourceNamePrefix + 'Scale');
end;

class function TdxGaugeCustomScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := '';
end;

function TdxGaugeCustomScaleStyleReader.CreateStyle(ADocument: TdxXMLDocument): TdxGaugeScaleStyle;
begin
  Result := TdxGaugeScaleStyle.Create(GetAttributeValueAsString(ADocument.Root.First, 'Name'),
    GetAttributeValueAsString(ADocument.Root.First, 'BaseStyle'));
  Result.Shapes := ReadCompositeShape(ADocument);
  Result.DefaultParameters := ReadDefaultParameters(ADocument);
  Result.ScaleTypeName := GetAttributeValueAsString(ADocument.Root.First, 'Type');
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsAlphaColor(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): TdxAlphaColor;
begin
  Result := dxColorNameToAlphaColor(ANode.Attributes.GetValueAsString(AAttributeName));
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsBoolean(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): Boolean;
begin
  if ANode <> nil then
    Result := ANode.Attributes.GetValueAsBoolean(AAttributeName)
  else
    Result := False;
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsColor(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): TColor;
begin
  Result := dxAlphaColorToColor(dxColorNameToAlphaColor(ANode.Attributes.GetValueAsString(AAttributeName)));
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsDouble(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): Double;
begin
  Result := ANode.Attributes.GetValueAsFloat(AAttributeName);
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsElementType(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): TdxGaugeElementType;
begin
  Result := GetElementType(ANode.Attributes.GetValueAsString(AAttributeName));
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsInteger(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): Integer;
begin
  Result := ANode.Attributes.GetValueAsInteger(AAttributeName);
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsPointF(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): TdxPointF;
begin
  if ANode <> nil then
    Result := dxStrToPointF(ANode.Attributes.GetValueAsString(AAttributeName))
  else
    Result := cxPointF(0, 0);
end;

function TdxGaugeCustomScaleStyleReader.GetAttributeValueAsString(ANode: TdxXMLNode;
  const AAttributeName: AnsiString): string;
begin
  Result := ANode.Attributes.GetValueAsString(AAttributeName);
end;

function TdxGaugeCustomScaleStyleReader.GetChildNode(ANode: TdxXMLNode; const AChildNodeName: AnsiString;
  out AChildNode: TdxXMLNode): Boolean;
begin
  AChildNode := GetChildNode(ANode, AChildNodeName);
  Result := AChildNode <> nil;
end;

function TdxGaugeCustomScaleStyleReader.GetChildNode(ANode: TdxXMLNode; const AChildNodeName: AnsiString): TdxXMLNode;
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild(AChildNodeName, AChildNode) then
    Result := AChildNode
  else
    Result := nil;
end;


{ TdxGaugeCustomCaptionViewInfo }

constructor TdxGaugeCustomCaptionViewInfo.Create;
begin
  inherited Create;
  FParameters := GetParametersClass.Create;
end;

destructor TdxGaugeCustomCaptionViewInfo.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

function TdxGaugeCustomCaptionViewInfo.GetParametersClass: TdxGaugeCustomCaptionParametersClass;
begin
  Result := TdxGaugeCustomCaptionParameters;
end;

function TdxGaugeCustomCaptionViewInfo.GetTextCenter(AScaleInfo: TdxGaugeCustomScaleInfo): TdxPointF;
begin
  Result.X := AScaleInfo.ContentBounds.Left + FParameters.CenterPositionFactor.X * AScaleInfo.ContentBounds.Width;
  Result.Y := AScaleInfo.ContentBounds.Top + FParameters.CenterPositionFactor.Y * AScaleInfo.ContentBounds.Height;
end;

procedure TdxGaugeCustomCaptionViewInfo.Calculate(AParameters: TdxGaugeCustomCaptionParameters;
  AScaleInfo: TdxGaugeCustomScaleInfo);
begin
  FScaleFactor := AScaleInfo.ScaleFactor;
  FScaleBounds := AScaleInfo.Bounds;
  FParameters.Assign(AParameters);
  CalculateBounds(AScaleInfo);
end;

function TdxGaugeCustomCaptionViewInfo.GetSelectionContentRect: TRect;
begin
  Result := cxRect(FSelectionBounds);
end;

procedure TdxGaugeCustomCaptionViewInfo.Draw(AGPGraphics: TdxGPGraphics);
{$IFDEF DELPHIXE}
var
  APrevRenderingHint: TdxGpTextRenderingHint;
{$ENDIF}
begin
  if FParameters.Visible then
{$IFDEF DELPHIXE}
  begin
    APrevRenderingHint := AGPGraphics.TextRenderingHint;
    if TFont(FParameters.Font).Quality = fqNonAntialiased then
      AGPGraphics.TextRenderingHint := TdxGpTextRenderingHint.TextRenderingHintSystemDefault;
    try
{$ENDIF}
      DoDraw(AGPGraphics)
{$IFDEF DELPHIXE}
    finally
      AGPGraphics.TextRenderingHint := APrevRenderingHint;
    end;
  end;
{$ENDIF DELPHIXE}
end;

function TdxGaugeCustomCaptionViewInfo.GetBounds: TdxRectF;
begin
  Result := FParameters.Bounds;
end;

function TdxGaugeCustomCaptionViewInfo.GetFont: TFont;
begin
  Result := TFont(FParameters.Font);
end;

procedure TdxGaugeCustomCaptionViewInfo.CalculateBounds(AScaleInfo: TdxGaugeCustomScaleInfo);
var
  R: TdxRectF;
  P: TdxPointF;
begin
  P := GetTextCenter(AScaleInfo);
  R := dxGaugeGetTextRect(FParameters.Text, GetFont);
  FParameters.Bounds := cxRectInflate(cxRectF(P, P), R.Width / 2, R.Height / 2);
end;

procedure TdxGaugeCustomCaptionViewInfo.CalculateSelectionBounds;
var
  AMatrix: TdxGPMatrix;
  R: TdxRectF;
begin
  if FParameters.Text = '' then
  begin
    R := cxRectF(cxRectCenter(FParameters.Bounds), cxRectCenter(FParameters.Bounds));
    FSelectionBounds := cxRectInflate(R, 10, 6);
  end
  else
    FSelectionBounds := FParameters.Bounds;

  if FParameters.RotationAngle <> 0 then
  begin
    R := cxRectF(cxRectCenter(FSelectionBounds), cxRectCenter(FSelectionBounds));
    FSelectionBounds := cxRectInflate(R, Max(FSelectionBounds.Width, FSelectionBounds.Height) / 2,
      Max(FSelectionBounds.Width, FSelectionBounds.Height) / 2);
  end;

  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Scale(FScaleFactor, cxRectCenter(FScaleBounds));
    FSelectionBounds := AMatrix.TransformRect(FSelectionBounds);
  finally
    AMatrix.Free;
  end;
end;

procedure TdxGaugeCustomCaptionViewInfo.DoDraw(AGPGraphics: TdxGPGraphics);

  procedure DrawText;
  begin
    dxGaugeDrawText(AGPGraphics, FParameters.Text, FParameters.Bounds, FParameters.Font);
  end;

begin
  if FParameters.RotationAngle <> 0 then
  begin
    AGPGraphics.SaveWorldTransform;
    try
      AGPGraphics.RotateWorldTransform(360 - FParameters.RotationAngle, cxRectCenter(FParameters.Bounds));
      DrawText;
    finally
      AGPGraphics.RestoreWorldTransform;
    end
  end
  else
    DrawText;
end;

{ TdxGaugeCustomCaption }

constructor TdxGaugeCustomCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubClasses;
end;

destructor TdxGaugeCustomCaption.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxGaugeCustomCaption.Assign(ASource: TPersistent);
begin
  if ASource is TdxGaugeCustomCaption then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(TdxGaugeCustomCaption(ASource));
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(ASource);
end;

function TdxGaugeCustomCaption.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxGaugeCustomScale).Captions;
end;

function TdxGaugeCustomCaption.GetOptionsViewClass: TdxGaugeCustomCaptionOptionsViewClass;
begin
  Result := TdxGaugeCustomCaptionOptionsView;
end;

function TdxGaugeCustomCaption.GetParametersClass: TdxGaugeCustomCaptionParametersClass;
begin
  Result := TdxGaugeCustomCaptionParameters;
end;

function TdxGaugeCustomCaption.GetViewInfoClass: TdxGaugeCustomCaptionViewInfoClass;
begin
  Result := TdxGaugeCustomCaptionViewInfo;
end;

function TdxGaugeCustomCaption.GetFont(AScaleInfo: TdxGaugeCustomScaleInfo): TFont;
begin
  Result := FOptionsView.Font;
end;

procedure TdxGaugeCustomCaption.Calculate(AScaleInfo: TdxGaugeCustomScaleInfo);
begin
  Parameters.Font := GetFont(AScaleInfo);
  FViewInfo.Calculate(Parameters, AScaleInfo);
  if IsDesigning then
    FViewInfo.CalculateSelectionBounds;
end;

procedure TdxGaugeCustomCaption.CreateSubClasses;
begin
  FParameters := GetParametersClass.Create;
  FParameters.CenterPositionFactor := dxGaugeDefaultCenterPositionFactor;
  FParameters.Visible := True;

  FViewInfo := GetViewInfoClass.Create;

  FOptionsLayout := TdxGaugeCustomCaptionOptionsLayout.Create(Self, FParameters);
  OptionsLayout.OnChange := OptionsChangeHandler;

  FOptionsView := GetOptionsViewClass.Create(Self, FParameters);
  FOptionsView.OnChange := OptionsChangeHandler;
end;

procedure TdxGaugeCustomCaption.DestroySubClasses;
begin
  FreeAndNil(FOptionsView);
  FreeAndNil(FOptionsLayout);
  FreeAndNil(FViewInfo);
  FreeAndNil(FParameters);
end;

function TdxGaugeCustomCaption.GetSelectionContentRect: TRect;
begin
  Result := FViewInfo.GetSelectionContentRect;
end;

function TdxGaugeCustomCaption.GetSelectionRect: TRect;
begin
  Result := GetSelectionContentRect;
end;

function TdxGaugeCustomCaption.GetSelectionMarkers: TRects;
begin
  SetLength(Result, 0);
end;

function TdxGaugeCustomCaption.GetSelectorRect: TRect;
begin
  Result := TdxControlsDesignSelectorHelper.CalculateBounds(
    GetSelectionRect, 2, 8, dxGetScaleFactor(Collection.ParentComponent));
end;

function TdxGaugeCustomCaption.IsSizable: Boolean;
begin
  Result := False;
end;

function TdxGaugeCustomCaption.IsDesigning: Boolean;
begin
  Result := ComponentState * [csDesigning] <> [];
end;

function TdxGaugeCustomCaption.GetText: string;
begin
  Result := dxVariantToString(FParameters.Text);
end;

function TdxGaugeCustomCaption.GetVisible: Boolean;
begin
  Result := FParameters.Visible;
end;

procedure TdxGaugeCustomCaption.SetOptionsLayout(const AValue: TdxGaugeCustomCaptionOptionsLayout);
begin
  OptionsLayout.Assign(AValue);
end;

procedure TdxGaugeCustomCaption.SetOptionsView(const AValue: TdxGaugeCustomCaptionOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TdxGaugeCustomCaption.SetText(const AValue: string);
begin
  if not VarSameValue(FParameters.Text, AValue) then
  begin
    FParameters.Text := AValue;
    Changed(False);
  end;
end;


{ TdxGaugeCustomContainerScale }

function TdxGaugeCustomContainerScale.GetHeightFactor: Single;
begin
  Result := GetViewInfo.GetRealHeightFactor;
end;

function TdxGaugeCustomContainerScale.GetWidthFactor: Single;
begin
  Result := GetViewInfo.GetRealWidthFactor;
end;

procedure TdxGaugeCustomContainerScale.SetHeight(const AValue: Integer);
begin
  GetViewInfo.CalculateExplicitSize := True;
  inherited SetHeight(AValue);
end;

procedure TdxGaugeCustomContainerScale.SetHeightFactor(const AValue: Single);
begin
  GetViewInfo.CalculateExplicitSize := True;
  inherited SetHeightFactor(AValue);
end;

procedure TdxGaugeCustomContainerScale.SetWidth(const AValue: Integer);
begin
  GetViewInfo.CalculateExplicitSize := True;
  inherited SetWidth(AValue);
end;

procedure TdxGaugeCustomContainerScale.SetWidthFactor(const AValue: Single);
begin
  GetViewInfo.CalculateExplicitSize := True;
  inherited SetWidthFactor(AValue);
end;

class function TdxGaugeCustomContainerScale.GetLayerCount: Integer;
begin
  Result := 1;
end;

class function TdxGaugeCustomContainerScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stContainerScale;
end;

class function TdxGaugeCustomContainerScale.GetScaleName: string;
begin
  Result := 'Container ';
end;

function TdxGaugeCustomContainerScale.GetSelectionContentRect: TRect;
begin
  Result := cxNullRect;
end;

function TdxGaugeCustomContainerScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomContainerScaleViewInfo;
end;

procedure TdxGaugeCustomContainerScale.Calculate(const ABounds: TdxRectF);
begin
  inherited Calculate(ABounds);
  if GetViewInfo.CalculateExplicitSize then
  begin
    Parameters.HeightFactor := GetViewInfo.GetRealHeightFactor;
    Parameters.WidthFactor := GetViewInfo.GetRealWidthFactor;
    inherited Calculate(ABounds);
    GetViewInfo.CalculateExplicitSize := False;
  end;
end;

procedure TdxGaugeCustomContainerScale.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  if IsDesigning then
    GetViewInfo.DoDrawBackground(AGPGraphics);
end;

procedure TdxGaugeCustomContainerScale.InitParameters;
begin
  inherited InitParameters;
  Parameters.Stretch := True;
end;

procedure TdxGaugeCustomContainerScale.InitStyleName;
begin
  FStyleName := '';
end;

function TdxGaugeCustomContainerScale.GetViewInfo: TdxGaugeCustomContainerScaleViewInfo;
begin
  Result := inherited ViewInfo as TdxGaugeCustomContainerScaleViewInfo;
end;


procedure TdxGaugeCustomScale.ApplyStyleParameters;
begin
// do nothing
end;

procedure TdxGaugeCustomScale.Calculate(const ABounds: TdxRectF);
var
  AScaleInfo: TdxGaugeCustomScaleInfo;
begin
  Collection.BeginUpdate;
  ApplyStyleParameters;
  Collection.EndUpdate(False);
  FViewInfo.Calculate(Parameters, ABounds);
  AScaleInfo := TdxGaugeCustomElementViewInfoAccess(FViewInfo).ScaleInfo as TdxGaugeCustomScaleInfo;
  CalculateCaptions(AScaleInfo);
end;

procedure TdxGaugeCustomScale.CreateSubClasses;
begin
  FOptionsLayout := GetOptionsLayoutClass.Create(Self);
  FParameters := GetParametersClass.Create;

  FDataBinding := GetDataBindingClass.Create(Self, Self);
  FDataBinding.OnDataChange := DataChangeHandler;
  FDataBinding.OnDataSetChange := DataSetChangeHandler;
  FOptionsView := GetOptionsViewClass.Create(Self);

  FCaptions := TdxGaugeCaptionCollection.Create(Self, GetCaptionClass);
  FCaptions.OnChange := ScaleElementChangeHandler;
end;

procedure TdxGaugeCustomScale.DestroySubClasses;
begin
  FreeAndNil(FCaptions);
  FreeAndNil(FDataBinding);
  FreeAndNil(FOptionsView);
  FreeAndNil(FParameters);
  FreeAndNil(FOptionsLayout);
end;

procedure TdxGaugeCustomScale.DoAssign(AScale: TdxGaugeCustomScale);
begin
  AnchorScaleIndex := AScale.AnchorScaleIndex;
  StyleName := (AScale as TdxGaugeCustomScale).StyleName;
  FParameters.Assign(AScale.FParameters);
  FCaptions.Assign((AScale as TdxGaugeCustomScale).Captions);
end;

procedure TdxGaugeCustomScale.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  DrawScaleComponents(AGPGraphics, ALayerIndex);
  FViewInfo.DrawLayer(AGPGraphics, ALayerIndex);
end;

procedure TdxGaugeCustomScale.DoSetValue(const AValue: Variant);
begin
  Parameters.Value := AValue;
  ScaleChanged(GetValueChangedLayers);
end;

procedure TdxGaugeCustomScale.DrawCaptions(AGPGraphics: TdxGPGraphics);
var
  I: Integer;
begin
  for I := 0 to Captions.Count - 1 do
    Captions[I].ViewInfo.Draw(AGPGraphics);
end;

procedure TdxGaugeCustomScale.DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
var
  AMatrix: TdxGPMatrix;
begin
  if FViewInfo.NeedDrawElement then
  begin
    AGPGraphics.SaveWorldTransform;
    try
      AMatrix := TdxGPMatrix.Create;
      try
        AMatrix.Scale(FViewInfo.ScaleFactor, cxRectCenter(GetBounds));
        AGPGraphics.ModifyWorldTransform(AMatrix);
        DoDrawLayer(AGPGraphics, ALayerIndex);
      finally
        AMatrix.Free;
      end;
    finally
      AGPGraphics.RestoreWorldTransform;
    end;
  end;
end;

procedure TdxGaugeCustomScale.DrawScaleComponents(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  if ALayerIndex = 1 then
    DrawCaptions(AGPGraphics);
end;

procedure TdxGaugeCustomScale.InitParameters;
begin
  Parameters.WidthFactor := dxGaugeDefaultWidthFactor;
  Parameters.HeightFactor := dxGaugeDefaultHeightFactor;
  Parameters.CenterPositionFactor := dxGaugeDefaultCenterPositionFactor;
  Parameters.Visible := True;
  Parameters.Value := dxGaugeScaleDefaultValue;
  GetParameters.ShowBackground := True;
  InitStyleName;
end;

procedure TdxGaugeCustomScale.InitStyleName;
begin
  FStyleName := dxgGaugeScaleStyleFactory.GetFirstStyleName(GetScaleType);
end;

procedure TdxGaugeCustomScale.InternalRestoreStyleParameters;
begin
  ScaleChanged([sclDynamicLayer]);
end;

function TdxGaugeCustomScale.GetSelectionContentRect: TRect;
begin
  Result := cxRect(Bounds, False);
end;

function TdxGaugeCustomScale.GetSelectionMarkers: TRects;
begin
  Result := dxGaugeGetSelectionMarkers(GetSelectionRect);
end;

function TdxGaugeCustomScale.GetSelectionRect: TRect;
begin
  Result := FViewInfo.ScaleInfo.Selection;
end;

function TdxGaugeCustomScale.GetSelectorRect: TRect;
begin
  Result := TdxControlsDesignSelectorHelper.CalculateBounds(
    GetSelectionRect, 5, 10, dxGetScaleFactor(Collection.ParentComponent));
end;

function TdxGaugeCustomScale.IsSizable: Boolean;
begin
  Result := True;
end;

function TdxGaugeCustomScale.IsDesigning: Boolean;
begin
  Result := ComponentState * [csDesigning] <> [];
end;

procedure TdxGaugeCustomScale.AddChangeLayer(ALayer: TdxGaugeScaleChangedLayer);
begin
  Include(FChangedLayers, ALayer);
end;

procedure TdxGaugeCustomScale.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxGaugeCustomScale.ScaleChanged(ALayers: TdxGaugeScaleChangedLayers);
begin
  if sclStaticLayer in ALayers then
    Include(FChangedLayers, sclStaticLayer);
  if sclDynamicLayer in ALayers then
    Include(FChangedLayers, sclDynamicLayer);
  if LockCount = 0 then
    Changed(False);
end;

procedure TdxGaugeCustomScale.ScaleFactorChanged(M, D: Integer);
begin
  OptionsLayout.Width := MulDiv(OptionsLayout.Width, M, D);
  OptionsLayout.Height := MulDiv(OptionsLayout.Height, M, D);
  OptionsLayout.CenterPositionX := MulDiv(OptionsLayout.CenterPositionX, M, D);
  OptionsLayout.CenterPositionY := MulDiv(OptionsLayout.CenterPositionY, M, D);
end;

procedure TdxGaugeCustomScale.EndUpdate;
begin
  Dec(FLockCount);
  ScaleChanged(ChangedLayers);
end;

procedure TdxGaugeCustomScale.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxGaugeCustomScale.ProcessCollection(ACollection: TcxComponentCollection; AProc: TGetChildProc;
  ARoot: TComponent);
var
  I: Integer;
begin
  for I := 0 to ACollection.Count - 1 do
    if ACollection[I].Owner = ARoot then
      AProc(ACollection[I]);
end;

function TdxGaugeCustomScale.GetAnchorScaleIndex: Integer;
var
  AIndex: Integer;
begin
  if FAnchorScale = nil then
    AIndex := -1
  else
    AIndex := FAnchorScale.Index;
  Result := AIndex;
end;

function TdxGaugeCustomScale.GetBounds: TdxRectF;
begin
  Result := FViewInfo.GetBounds;
end;

function TdxGaugeCustomScale.GetCenterPositionType: TdxGaugeScalePositionType;
begin
  Result := Parameters.CenterPositionType;
end;

function TdxGaugeCustomScale.GetCenterPositionX: Integer;
begin
  Result := Parameters.CenterPosition.X;
end;

function TdxGaugeCustomScale.GetCenterPositionFactorX: Single;
begin
  Result := Parameters.CenterPositionFactor.X;
end;

function TdxGaugeCustomScale.GetCenterPositionY: Integer;
begin
  Result := Parameters.CenterPosition.Y;
end;

function TdxGaugeCustomScale.GetCenterPositionFactorY: Single;
begin
  Result := Parameters.CenterPositionFactor.Y;
end;

function TdxGaugeCustomScale.GetClientRect: TdxRectF;
begin
  Result := FViewInfo.GetClientRect;
end;

function TdxGaugeCustomScale.GetStretch: Boolean;
begin
  Result := Parameters.Stretch;
end;

function TdxGaugeCustomScale.GetVisible: Boolean;
begin
  Result := Parameters.Visible;
end;

function TdxGaugeCustomScale.GetZOrder: Integer;
begin
  if Collection <> nil then
    Result := dxGaugeController(TdxGaugeScaleCollection(Collection)).ZOrders.IndexOf(Self)
  else
    Result := -1;
end;

procedure TdxGaugeCustomScale.SetAnchorScale(const AValue: TdxGaugeCustomScale);
begin
  if (AValue = nil) or dxGaugeController(Collection as TdxGaugeScaleCollection).CanAnchorScale(Index, AValue.Index) then
  begin
    FAnchorScale := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetAnchorScaleIndex(const AValue: Integer);
begin
  if (ComponentState * [csReading, csLoading] <> []) then
    FStoredAnchorScaleIndex := AValue
  else if dxGaugeController(Collection as TdxGaugeScaleCollection).CanAnchorScale(Index, AValue) then
  begin
    FAnchorScale := GetAnchorScaleByIndex(AValue);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetCenterPositionType(const AValue: TdxGaugeScalePositionType);
begin
  if Parameters.CenterPositionType <> AValue then
  begin
    Parameters.CenterPositionType := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetCenterPositionX(const AValue: Integer);
begin
  if Parameters.CenterPosition.X <> AValue then
  begin
    Parameters.CenterPosition := Point(AValue, Parameters.CenterPosition.Y);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetCenterPositionFactorX(const AValue: Single);
begin
  if not SameValue(Parameters.CenterPositionFactor.X, AValue) then
  begin
    Parameters.CenterPositionFactor := dxPointF(AValue, Parameters.CenterPositionFactor.Y);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetCenterPositionY(const AValue: Integer);
begin
  if Parameters.CenterPosition.Y <> AValue then
  begin
    Parameters.CenterPosition := Point(Parameters.CenterPosition.X, AValue);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetCenterPositionFactorY(const AValue: Single);
begin
  if not SameValue(Parameters.CenterPositionFactor.Y, AValue) then
  begin
    Parameters.CenterPositionFactor := dxPointF(Parameters.CenterPositionFactor.X, AValue);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsCustomLayout);
begin
  OptionsLayout.Assign(AValue);
end;

procedure TdxGaugeCustomScale.SetStretch(const AValue: Boolean);
begin
  if Parameters.Stretch <> AValue then
  begin
    Parameters.Stretch := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetVisible(const AValue: Boolean);
begin
  if Parameters.Visible <> AValue then
  begin
    Parameters.Visible := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

function TdxGaugeCustomScale.GetAnchorScaleByIndex(AAnchorScaleIndex: Integer): TdxGaugeCustomScale;
begin
  if AAnchorScaleIndex <> -1 then
    Result := Collection[AAnchorScaleIndex] as TdxGaugeCustomScale
  else
    Result := nil;
end;

procedure TdxGaugeCustomScale.ReadZOrder(AReader: TReader);
begin
  FStoredZOrder := AReader.ReadInteger;
end;

procedure TdxGaugeCustomScale.WriteZOrder(AWriter: TWriter);
begin
  AWriter.WriteInteger(ZOrder);
end;

function TdxGaugeCustomScale.IsAnchorScaleIndexStored: Boolean;
begin
  Result := AnchorScaleIndex <> -1;
end;

function TdxGaugeCustomScale.IsCenterPositionFactorXStored: Boolean;
begin
  Result := not SameValue(Parameters.CenterPositionFactor.X, dxGaugeDefaultCenterPositionFactor.X);
end;

function TdxGaugeCustomScale.IsCenterPositionFactorYStored: Boolean;
begin
  Result := not SameValue(Parameters.CenterPositionFactor.Y, dxGaugeDefaultCenterPositionFactor.Y);
end;

function TdxGaugeCustomScale.IsHeightFactorStored: Boolean;
begin
  Result := not SameValue(Parameters.HeightFactor, dxGaugeDefaultHeightFactor);
end;

function TdxGaugeCustomScale.IsWidthFactorStored: Boolean;
begin
  Result := not SameValue(Parameters.WidthFactor, dxGaugeDefaultWidthFactor);
end;

function TdxGaugeCustomScale.GetShowBackground: Boolean;
begin
  Result := Parameters.ShowBackground;
end;

function TdxGaugeCustomScale.GetValue: Variant;
begin
  Result := Parameters.Value;
end;

procedure TdxGaugeCustomScale.SetCaptions(const AValue: TdxGaugeCaptionCollection);
begin
  FCaptions.Assign(AValue);
end;

procedure TdxGaugeCustomScale.SetDataBinding(const AValue: TcxCustomDataBinding);
begin
  FDataBinding.Assign(AValue);
end;

procedure TdxGaugeCustomScale.SetOptionsView(const AValue: TdxGaugeCustomScaleOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TdxGaugeCustomScale.SetShowBackground(const AValue: Boolean);
begin
  if Parameters.ShowBackground <> AValue then
  begin
    Parameters.ShowBackground := AValue;
    ScaleChanged([sclStaticLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetStyleName(const AValue: string);
begin
  if not SameText(FStyleName, AValue) then
  begin
    FStyleName := AValue;
    Collection.BeginUpdate;
    try
      (FViewInfo as TdxGaugeCustomScaleViewInfo).RecreateStyle(GetScaleType, FStyleName);
      ApplyStyleParameters;
    finally
      Collection.EndUpdate(False);
    end;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetValue(const AValue: Variant);
begin
  InternalSetValue(AValue);
end;

procedure TdxGaugeCustomScale.DataChangeHandler;

  function InternalGetValue: Variant;
  begin
    Result := 0;
    if FDataBinding.IsDataSourceLive then
    begin
      Result := FDataBinding.GetStoredValue(evsValue, True);
      if VarIsNull(Result) then
        Result := 0;
    end
  end;

begin
  Value := InternalGetValue;
end;

procedure TdxGaugeCustomScale.DataSetChangeHandler;
begin
  ScaleChanged([sclDynamicLayer]);
end;

procedure TdxGaugeCustomScale.ScaleElementChangeHandler(ASender: TObject; AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  ScaleChanged([sclDynamicLayer]);
end;

function TdxGaugeCustomScale.GetParameters: TdxGaugeCustomScaleParameters;
begin
  Result := Parameters as TdxGaugeCustomScaleParameters;
end;

procedure TdxGaugeCustomScale.CalculateCaptions(AScaleInfo: TdxGaugeCustomScaleInfo);
var
  I: Integer;
begin
  for I := 0 to Captions.Count - 1 do
    Captions[I].Calculate(AScaleInfo);
end;

function TdxGaugeCustomScale.IsStyleNameStored: Boolean;
begin
  Result := not SameText(FStyleName, dxgGaugeScaleStyleFactory.GetFirstStyleName(GetScaleType));
end;

{ TdxGaugeCustomContainerScaleViewInfo }

constructor TdxGaugeCustomContainerScaleViewInfo.Create(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  inherited Create(AScaleType, AStyleName);
  FExplicitSize := dxSizeF(-1, -1);
  ScaleInfo.ScaleFactor := dxPointF(1, 1);
end;

function TdxGaugeCustomContainerScaleViewInfo.CanDrawBackground: Boolean;
begin
  Result := True;
end;

function TdxGaugeCustomContainerScaleViewInfo.GetContentBounds: TdxRectF;
begin
  Result := ScaleInfo.Bounds;
end;

function TdxGaugeCustomContainerScaleViewInfo.GetLayoutSize(const ABounds: TdxRectF): TdxSizeF;
begin
  if CalculateExplicitSize then
    Result := GetExplicitSize(ABounds)
  else
    Result := GetProportionalSize( inherited GetLayoutSize(ABounds));
  CalculateSizeFactor(Result, ABounds);
end;

procedure TdxGaugeCustomContainerScaleViewInfo.CalculateScaleInfo;
begin
  ScaleInfo.ContentBounds := ScaleInfo.Bounds;
  ScaleInfo.Selection := cxRect(ScaleInfo.Bounds, False);
end;

procedure TdxGaugeCustomContainerScaleViewInfo.CalculateScaleFactor;
begin
  FScaleFactor := dxPointF(1, 1);
end;

procedure TdxGaugeCustomContainerScaleViewInfo.CalculateSizeFactor(const ASize: TdxSizeF; const ABounds: TdxRectF);
begin
  if (ABounds.Width > 0) and (ABounds.Height > 0) then
  begin
    FSizeFactor.X := ASize.cx / ABounds.Width;
    FSizeFactor.Y := ASize.cy / ABounds.Height;
  end
  else
    FSizeFactor := cxPointF(0, 0);
end;

procedure TdxGaugeCustomContainerScaleViewInfo.CreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
 // do nothing
end;

procedure TdxGaugeCustomContainerScaleViewInfo.DoDrawBackground(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.Rectangle(ScaleInfo.ContentBounds, dxColorToAlphaColor($BD8753, 40), dxColorToAlphaColor($582801, 10));
end;

function TdxGaugeCustomContainerScaleViewInfo.GetExplicitSize(const ABounds: TdxRectF): TdxSizeF;
begin
  if (FExplicitSize.cx <> -1) and (FExplicitSize.cy <> -1) and (FSizeFactor.X > 0) and (FSizeFactor.Y > 0)
    and not SameValue(FParameters.HeightFactor, FSizeFactor.Y) and not SameValue(FParameters.WidthFactor,
    FSizeFactor.X) then
    FExplicitSize := dxGaugeGetRectangularScaleLayoutSize(ABounds, FParameters.Width, FParameters.Height,
      FSizeFactor.X, FSizeFactor.Y)
  else
    FExplicitSize := inherited GetLayoutSize(ABounds);
  Result := FExplicitSize;
end;

function TdxGaugeCustomContainerScaleViewInfo.GetProportionalSize(const ASize: TdxSizeF): TdxSizeF;
var
  ARatio: Single;
begin
  if (FExplicitSize.cx <> -1) and (FExplicitSize.cy <> -1) then
  begin
    if (FExplicitSize.cx > 0) and (FExplicitSize.cy > 0) then
      ARatio := Min(ASize.cx / FExplicitSize.cx, ASize.cy / FExplicitSize.cy)
    else
      ARatio := 0.001;
    Result := dxSizeF(FExplicitSize.cx * ARatio, FExplicitSize.cy * ARatio);
    ScaleInfo.ScaleFactor.X := ARatio;
    ScaleInfo.ScaleFactor.Y := ScaleInfo.ScaleFactor.X;
  end
  else
  begin
    FExplicitSize := ASize;
    Result := ASize;
    ScaleInfo.ScaleFactor.X := 1;
    ScaleInfo.ScaleFactor.Y := ScaleInfo.ScaleFactor.X;
  end;
end;

function TdxGaugeCustomContainerScaleViewInfo.GetRealHeightFactor: Single;
begin
  Result := FSizeFactor.Y;
end;

function TdxGaugeCustomContainerScaleViewInfo.GetRealWidthFactor: Single;
begin
  Result := FSizeFactor.X;
end;


function TdxGaugeCustomScaleViewInfo.GetClientRect: TdxRectF;
begin
  Result := ScaleInfo.Bounds;
end;

function TdxGaugeCustomScaleViewInfo.GetScaledRect(const AOriginalSize: TdxSizeF): TdxRectF;
var
  P: TdxPointF;
begin
  P := cxRectCenter(ScaleInfo.Bounds);
  Result := cxRectFBounds(P.X - AOriginalSize.cx / 2, P.Y - AOriginalSize.cy / 2, AOriginalSize.cx, AOriginalSize.cy);
end;

function TdxGaugeCustomScaleViewInfo.GetScaledRect(const AOriginalSize: TdxSizeF;
  const AScaleFactor: TdxPointF): TdxRectF;
begin
  Result := GetScaledRect(dxSizeF(AOriginalSize.cx * AScaleFactor.X, AOriginalSize.cy * AScaleFactor.Y));
end;

function TdxGaugeCustomScaleViewInfo.NeedDrawElement: Boolean;
begin
  Result := FParameters.Visible and (FScaleFactor.X > 0.01) and (FScaleFactor.Y > 0.01);
end;

procedure TdxGaugeCustomScaleViewInfo.DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  DoDrawLayer(AGPGraphics, ALayerIndex);
end;

function TdxGaugeCustomScaleViewInfo.GetStyleName: string;
begin
  Result := FStyle.Name;
end;

procedure TdxGaugeCustomScaleViewInfo.SetStyle(const AValue: TdxGaugeScaleStyle);
begin
  FreeAndNil(FStyle);
  FStyle := AValue;
  LoadScaleElements;
end;

function TdxGaugeCustomScaleViewInfo.GetDefaultParameters: TdxGaugeCustomScaleDefaultParameters;
begin
  Result := FStyle.DefaultParameters;
end;

function TdxGaugeCustomScaleViewInfo.GetParameters: TdxGaugeCustomScaleParameters;
begin
  Result := FParameters as TdxGaugeCustomScaleParameters;
end;

procedure TdxGaugeCustomScaleViewInfo.DrawBackground(AGPGraphics: TdxGPGraphics);
begin
  if GetParameters.ShowBackground and CanDrawBackground then
    DoDrawBackground(AGPGraphics);
end;

procedure TdxGaugeCustomScaleViewInfo.RecreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  FreeAndNil(FStyle);
  CreateStyle(AScaleType, AStyleName);
end;

{ TdxGaugeCustomScale }

constructor TdxGaugeCustomScale.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnchorScale := nil;
  FStoredAnchorScaleIndex := -1;
  FChangedLayers := [sclStaticLayer, sclDynamicLayer];
  CreateSubClasses;
  InitParameters;
  FViewInfo := TdxGaugeCustomScaleViewInfoClass(GetViewInfoClass).Create(GetScaleType, FStyleName);
  ApplyStyleParameters;
end;

destructor TdxGaugeCustomScale.Destroy;
begin
  FreeAndNil(FViewInfo);
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxGaugeCustomScale.Assign(ASource: TPersistent);
begin
  if ASource is TdxGaugeCustomScale then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(TdxGaugeCustomScale(ASource));
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxGaugeCustomScale.RestoreStyleParameters;
begin
  InternalRestoreStyleParameters;
end;

function TdxGaugeCustomScale.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomGaugeControl).Scales;
end;

procedure TdxGaugeCustomScale.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
begin
  inherited GetChildren(AProc, ARoot);
  ProcessCollection(Captions, AProc, ARoot);
end;

procedure TdxGaugeCustomScale.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('ZOrder', ReadZOrder, WriteZOrder, ZOrder <> 0);
end;

procedure TdxGaugeCustomScale.Loaded;
begin
  inherited Loaded;
  Collection.BeginUpdate;
  AnchorScaleIndex := FStoredAnchorScaleIndex;
  FStoredAnchorScaleIndex := -1;
  Collection.EndUpdate;
end;

class function TdxGaugeCustomScale.GetLayerCount: Integer;
begin
  Result := 0;
end;

class function TdxGaugeCustomScale.GetScaleName: string;
begin
  Result := '';
end;

class function TdxGaugeCustomScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularScale;
end;

class function TdxGaugeCustomScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeCustomScaleStyleReader;
end;

function TdxGaugeCustomScale.GetCaptionClass: TdxGaugeCustomCaptionClass;
begin
  Result := TdxGaugeCustomCaption;
end;

function TdxGaugeCustomScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxCustomDataBinding;
end;

function TdxGaugeCustomScale.GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass;
begin
  Result := TdxGaugeCustomScaleOptionsView;
end;

function TdxGaugeCustomScale.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeCustomScaleParameters;
end;

function TdxGaugeCustomScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomScaleViewInfo;
end;

function TdxGaugeCustomScale.GetHeight: Integer;
begin
  Result := Parameters.Height;
end;

function TdxGaugeCustomScale.GetHeightFactor: Single;
begin
  Result := Parameters.HeightFactor;
end;

function TdxGaugeCustomScale.GetWidth: Integer;
begin
  Result := Parameters.Width;
end;

function TdxGaugeCustomScale.GetWidthFactor: Single;
begin
  Result := Parameters.WidthFactor;
end;

procedure TdxGaugeCustomScale.InternalSetValue(const AValue: Variant);
var
  AValidValue: Variant;
begin
  AValidValue := GetValidValue(AValue);
  if not VarSameValue(Parameters.Value, AValidValue) then
    DoSetValue(AValidValue);
end;

procedure TdxGaugeCustomScale.SetHeight(const AValue: Integer);
begin
  if (Parameters.Height <> AValue) and (AValue >= 0) then
  begin
    Parameters.Height := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetHeightFactor(const AValue: Single);
begin
  if (Parameters.HeightFactor <> AValue) then
  begin
    Parameters.HeightFactor := Max(AValue, dxGaugeScaleMinScaleFactor.Y);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetWidth(const AValue: Integer);
begin
  if (Parameters.Width <> AValue) and (AValue >= 0) then
  begin
    Parameters.Width := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomScale.SetWidthFactor(const AValue: Single);
begin
  if not SameValue(Parameters.WidthFactor, AValue) then
  begin
    Parameters.WidthFactor := Max(AValue, dxGaugeScaleMinScaleFactor.X);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

function TdxGaugeCustomScale.GetValueChangedLayers: TdxGaugeScaleChangedLayers;
begin
  Result := [sclDynamicLayer];
end;

function TdxGaugeCustomScale.GetValidValue(const AValue: Variant): Variant;
begin
  Result := AValue;
end;


{ TdxGaugeCustomScaleViewInfo }

constructor TdxGaugeCustomScaleViewInfo.Create(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  if dxgGaugeScaleStyleFactory.HasStyles(AScaleType) or (AScaleType = stContainerScale) then
  begin
    inherited Create;
    CreateSubClasses;
    CreateStyle(AScaleType, AStyleName);
    FScaleFactor := dxPointF(1, 1);
  end
  else
    dxGaugeRaiseException(sdxGaugeScaleStyleCollectionInvalidStyleCount,
      [dxGaugeScaleTypeInfos.GetFormatedTypeName(AScaleType)]);
end;

destructor TdxGaugeCustomScaleViewInfo.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

function TdxGaugeCustomScaleViewInfo.GetScaleInfoClass: TdxGaugeCustomScaleInfoClass;
begin
  Result := TdxGaugeCustomScaleInfo;
end;

function TdxGaugeCustomScaleViewInfo.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeCustomScaleParameters;
end;

function TdxGaugeCustomScaleViewInfo.GetContentBounds: TdxRectF;
begin
  Result := GetScaledRect(GetElementImageOriginalSize(FBackground));
end;

function TdxGaugeCustomScaleViewInfo.GetContentOriginalSize: TdxSizeF;
begin
  Result := GetElementImageOriginalSize(FBackground);
end;

function TdxGaugeCustomScaleViewInfo.GetLayoutSize(const ABounds: TdxRectF): TdxSizeF;
begin
  Result := dxGaugeGetRectangularScaleLayoutSize(ABounds, FParameters.Width, FParameters.Height,
    FParameters.WidthFactor, FParameters.HeightFactor);
end;

procedure TdxGaugeCustomScaleViewInfo.Calculate(AParameters: TdxGaugeCustomScaleParameters; const ABounds: TdxRectF);
begin
  PopulateParameters(AParameters);
  CalculateBounds(ABounds);
  CalculateScaleFactor;
  CalculateContent;
  CalculateScaleInfo;
end;

procedure TdxGaugeCustomScaleViewInfo.CalculateBounds(const ABounds: TdxRectF);
var
  ACenter: TdxPointF;
  ASize: TdxSizeF;
  R: TdxRectF;
begin
  R := cxRectContent(ABounds, cxRectF(0, 0, 1, 1));
  ACenter := GetCenter(R);
  ASize := GetLayoutSize(R);
  ASize := dxSizeF(ASize.cx / 2, ASize.cy / 2);
  ScaleInfo.Bounds := cxRectInflate(cxRectF(ACenter, ACenter), dxRectF(ASize.cx, ASize.cy, ASize.cx, ASize.cy));
end;

procedure TdxGaugeCustomScaleViewInfo.CalculateContent;
begin
// do nothing
end;

procedure TdxGaugeCustomScaleViewInfo.CalculateScaleInfo;
var
  ASize: TdxSizeF;
begin
  FScaleInfo.Value := FParameters.Value;
  FScaleInfo.ContentBounds := GetContentBounds;
  ASize := GetContentOriginalSize;
  ScaleInfo.Selection := cxRect(GetScaledRect(ASize, dxPointF(ScaleInfo.Bounds.Width / ASize.cx,
        ScaleInfo.Bounds.Height / ASize.cy)), False);
  ScaleInfo.ScaleFactor := FScaleFactor;
end;

procedure TdxGaugeCustomScaleViewInfo.CalculateScaleFactor;
var
  BB12BCDF1E7549E491DF6037C0C7DE6C: TdxSizeF;
begin
  BB12BCDF1E7549E491DF6037C0C7DE6C := GetContentOriginalSize;
  FScaleFactor.X := ScaleInfo.Bounds.Width / BB12BCDF1E7549E491DF6037C0C7DE6C.cx;
  FScaleFactor.Y := ScaleInfo.Bounds.Height / BB12BCDF1E7549E491DF6037C0C7DE6C.cy;
  if not FParameters.Stretch then
  begin
    FScaleFactor.X := Min(FScaleFactor.X, FScaleFactor.Y);
    FScaleFactor.Y := FScaleFactor.X;
  end;
end;

procedure TdxGaugeCustomScaleViewInfo.CreateSubClasses;
begin
  FParameters := GetParametersClass.Create;
  FScaleInfo := GetScaleInfoClass.Create;
end;

procedure TdxGaugeCustomScaleViewInfo.DestroySubClasses;
begin
  FreeAndNil(FStyle);
  FreeAndNil(FScaleInfo);
  FreeAndNil(FParameters);
end;

procedure TdxGaugeCustomScaleViewInfo.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  if ALayerIndex = 0 then
    DrawBackground(AGPGraphics);
end;

procedure TdxGaugeCustomScaleViewInfo.PopulateParameters(AParameters: TdxGaugeCustomScaleParameters);
begin
  FParameters.Assign(AParameters);
end;

function TdxGaugeCustomScaleViewInfo.CanDrawBackground: Boolean;
begin
  Result := CanDrawImage(FBackground);
end;

procedure TdxGaugeCustomScaleViewInfo.CreateStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  FStyle := dxgGaugeScaleStyleFactory.CreateStyle(AScaleType, AStyleName);
  LoadScaleElements;
end;

procedure TdxGaugeCustomScaleViewInfo.DoDrawBackground(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FBackground, GetContentBounds);
end;

procedure TdxGaugeCustomScaleViewInfo.LoadScaleElements;
begin
  FBackground := FStyle.GetElement(etBackground1);
end;

function TdxGaugeCustomScaleViewInfo.CanDrawImage(AImage: TGraphic): Boolean;
begin
  Result := (AImage <> nil) and not AImage.Empty;
end;

function TdxGaugeCustomScaleViewInfo.GetElementImage(AType: TdxGaugeElementType): TGraphic;
begin
  Result := FStyle.GetElement(AType);
end;

function TdxGaugeCustomScaleViewInfo.GetElementImageOriginalSize(AImage: TGraphic): TdxSizeF;
begin
  Result := TdxSizeF.Null;
  if AImage <> nil then
    if AImage is TdxCompositeShape then
    begin
      Result.cx := TdxCompositeShapeAccess(AImage).WidthF;
      Result.cy := TdxCompositeShapeAccess(AImage).HeightF;
    end
    else
    begin
      Result.cx := AImage.Width;
      Result.cy := AImage.Height;
    end;
end;

function TdxGaugeCustomScaleViewInfo.GetElementImageSize(AImage: TGraphic;
  const AImageScaleFactor: TdxPointF): TdxSizeF;
begin
  Result := TdxSizeF.Null;
  if CanDrawImage(AImage) then
  begin
    Result := GetElementImageOriginalSize(AImage);
    Result.cx := Result.cx * AImageScaleFactor.X;
    Result.cy := Max(Result.cy * AImageScaleFactor.Y, 1);
  end;
end;

function TdxGaugeCustomScaleViewInfo.GetBounds: TdxRectF;
var
  ABounds: TdxRectF;
  AMatrix: TdxGPMatrix;
begin
  ABounds := GetContentBounds;
  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Scale(FScaleFactor, cxRectCenter(ABounds));
    Result := AMatrix.TransformRect(ABounds);
  finally
    AMatrix.Free;
  end;
end;

function TdxGaugeCustomScaleViewInfo.GetCenter(const ABounds: TdxRectF): TdxPointF;
var
  AOffset: TdxPointF;
begin
  if FParameters.CenterPositionType = sptFactor then
  begin
    AOffset.X := FParameters.CenterPositionFactor.X * ABounds.Width;
    AOffset.Y := FParameters.CenterPositionFactor.Y * ABounds.Height;
  end
  else
  begin
    AOffset.X := FParameters.CenterPosition.X;
    AOffset.Y := FParameters.CenterPosition.Y;
  end;
  Result := cxPointOffset(ABounds.TopLeft, AOffset);
end;


{ TdxGaugeScaleOptionsPersistent }

function TdxGaugeScaleOptionsPersistent.GetScale: TdxGaugeCustomScale;
begin
  Result := Owner as TdxGaugeCustomScale;
end;

{ TdxGaugeScaleOptionsCustomLayout }

procedure TdxGaugeScaleOptionsCustomLayout.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('CenterPositionFactorX', ReadCenterPositionFactorX, WriteCenterPositionFactorX,
    SameValue(CenterPositionFactorX, 0));
  AFiler.DefineProperty('CenterPositionFactorY', ReadCenterPositionFactorY, WriteCenterPositionFactorY,
    SameValue(CenterPositionFactorY, 0));
end;

function TdxGaugeScaleOptionsCustomLayout.GetCenterPositionFactorX: Single;
begin
  Result := GetScale.GetCenterPositionFactorX;
end;

function TdxGaugeScaleOptionsCustomLayout.GetCenterPositionFactorY: Single;
begin
  Result := GetScale.GetCenterPositionFactorY;
end;

function TdxGaugeScaleOptionsCustomLayout.GetCenterPositionX: Integer;
begin
  Result := GetScale.GetCenterPositionX;
end;

function TdxGaugeScaleOptionsCustomLayout.GetCenterPositionY: Integer;
begin
  Result := GetScale.GetCenterPositionY;
end;

function TdxGaugeScaleOptionsCustomLayout.GetCenterPositionType: TdxGaugeScalePositionType;
begin
  Result := GetScale.GetCenterPositionType;
end;

function TdxGaugeScaleOptionsCustomLayout.GetHeight: Integer;
begin
  Result := GetScale.GetHeight;
end;

function TdxGaugeScaleOptionsCustomLayout.GetHeightFactor: Single;
begin
  Result := GetScale.GetHeightFactor;
end;

function TdxGaugeScaleOptionsCustomLayout.GetStretch: Boolean;
begin
  Result := GetScale.GetStretch;
end;

function TdxGaugeScaleOptionsCustomLayout.GetWidth: Integer;
begin
  Result := GetScale.GetWidth;
end;

function TdxGaugeScaleOptionsCustomLayout.GetWidthFactor: Single;
begin
  Result := GetScale.GetWidthFactor;
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetCenterPositionFactorX(const AValue: Single);
begin
  GetScale.SetCenterPositionFactorX(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetCenterPositionFactorY(const AValue: Single);
begin
  GetScale.SetCenterPositionFactorY(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetCenterPositionX(const AValue: Integer);
begin
  GetScale.SetCenterPositionX(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetCenterPositionY(const AValue: Integer);
begin
  GetScale.SetCenterPositionY(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetCenterPositionType(const AValue: TdxGaugeScalePositionType);
begin
  GetScale.SetCenterPositionType(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetHeight(const AValue: Integer);
begin
  GetScale.SetHeight(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetHeightFactor(const AValue: Single);
begin
  GetScale.SetHeightFactor(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetStretch(const AValue: Boolean);
begin
  GetScale.SetStretch(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetWidth(const AValue: Integer);
begin
  GetScale.SetWidth(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.SetWidthFactor(const AValue: Single);
begin
  GetScale.SetWidthFactor(AValue);
end;

procedure TdxGaugeScaleOptionsCustomLayout.ReadCenterPositionFactorX(AReader: TReader);
begin
  CenterPositionFactorX := AReader.ReadDouble;
end;

procedure TdxGaugeScaleOptionsCustomLayout.ReadCenterPositionFactorY(AReader: TReader);
begin
  CenterPositionFactorY := AReader.ReadDouble;
end;

procedure TdxGaugeScaleOptionsCustomLayout.WriteCenterPositionFactorX(AWriter: TWriter);
begin
  AWriter.WriteDouble(CenterPositionFactorX);
end;

procedure TdxGaugeScaleOptionsCustomLayout.WriteCenterPositionFactorY(AWriter: TWriter);
begin
  AWriter.WriteDouble(CenterPositionFactorY);
end;

function TdxGaugeScaleOptionsCustomLayout.IsCenterPositionFactorXStored: Boolean;
begin
  Result := GetScale.IsCenterPositionFactorXStored;
end;

function TdxGaugeScaleOptionsCustomLayout.IsCenterPositionFactorYStored: Boolean;
begin
  Result := GetScale.IsCenterPositionFactorYStored;
end;

function TdxGaugeScaleOptionsCustomLayout.IsHeightFactorStored: Boolean;
begin
  Result := GetScale.IsHeightFactorStored;
end;

function TdxGaugeScaleOptionsCustomLayout.IsWidthFactorStored: Boolean;
begin
  Result := GetScale.IsWidthFactorStored;
end;

{ TdxGaugeCustomScaleOptionsView }

function TdxGaugeCustomScaleOptionsView.GetShowBackground: Boolean;
begin
  Result := GetScale.GetShowBackground;
end;

procedure TdxGaugeCustomScaleOptionsView.SetShowBackground(const AValue: Boolean);
begin
  GetScale.SetShowBackground(AValue);
end;


procedure TdxGaugeCustomCaption.SetVisible(const AValue: Boolean);
begin
  if FParameters.Visible <> AValue then
  begin
    FParameters.Visible := AValue;
    Changed(False);
  end;
end;

procedure TdxGaugeCustomCaption.DoAssign(ACaption: TdxGaugeCustomCaption);
begin
  FParameters.Assign(ACaption.Parameters);
  FOptionsView.Font.Assign(ACaption.Parameters.Font);
end;

procedure TdxGaugeCustomCaption.OptionsChangeHandler(ASender: TObject);
begin
  Changed(False);
end;

{ TdxGaugeCaptionCollection }

procedure TdxGaugeCaptionCollection.Assign(ASource: TPersistent);
var
  I: Integer;
begin
  if ASource is TdxGaugeCaptionCollection then
  begin
    Clear;
    for I := 0 to TdxGaugeCaptionCollection(ASource).Count - 1 do
      Add;
    for I := 0 to TdxGaugeCaptionCollection(ASource).Count - 1 do
      Items[I].Assign(TdxGaugeCaptionCollection(ASource).Items[I]);
  end
  else
    inherited Assign(ASource);
end;

function TdxGaugeCaptionCollection.GetItemPrefixName: string;
begin
  Result := LeftStr(ItemClass.ClassName, Length(ItemClass.ClassName) - Length('Caption'));
end;

function TdxGaugeCaptionCollection.GetCaption(AIndex: Integer): TdxGaugeCustomCaption;
begin
  Result := inherited GetItem(AIndex) as TdxGaugeCustomCaption;
end;

procedure TdxGaugeCaptionCollection.SetCaption(AIndex: Integer; const AValue: TdxGaugeCustomCaption);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TdxGaugeScaleStyleCreator }

function TdxGaugeScaleStyleCreator.CreateStyle(AStyleInfo: TdxGaugeScaleStyleInfo): TdxGaugeScaleStyle;
begin
  if AStyleInfo <> nil then
    if not AStyleInfo.IsExternalStyle then
      Result := CreateFromResource(HInstance, AStyleInfo.ResourceName)
    else
      Result := CreateStyle(AStyleInfo.ResourceName)
    else
      Result := nil;
end;

function TdxGaugeScaleStyleCreator.CreateStyle(AInstance: THandle; const AResName: string): TdxXMLDocument;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(AInstance, AResName, RT_RCDATA);
  try
    Result := InternalCreateStyleDocument(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.CreateStyle(const AFileName: string): TdxGaugeScaleStyle;
var
  BB12BCDF1E7549E491DF6037C0C7DE6C: TStream;
begin
  BB12BCDF1E7549E491DF6037C0C7DE6C := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CreateFromStream(BB12BCDF1E7549E491DF6037C0C7DE6C);
  finally
    BB12BCDF1E7549E491DF6037C0C7DE6C.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.CreateStyleDocument(AStyleInfo: TdxGaugeScaleStyleInfo): TdxXMLDocument;
begin
  if AStyleInfo <> nil then
    if not AStyleInfo.IsExternalStyle then
      Result := CreateStyle(HInstance, AStyleInfo.ResourceName)
    else
      Result := CreateStyleDocument(AStyleInfo.ResourceName)
    else
      Result := nil;
end;

function TdxGaugeScaleStyleCreator.CreateStyleDocument(const AFileName: string): TdxXMLDocument;
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := InternalCreateStyleDocument(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.CreateFromResource(AInstance: THandle; const AResName: string): TdxGaugeScaleStyle;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(AInstance, AResName, RT_RCDATA);
  try
    Result := CreateFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.CreateFromStream(AStream: TStream): TdxGaugeScaleStyle;
var
  AXMLDocument: TdxXMLDocument;
  AReader: TdxGaugeCustomScaleStyleReader;
begin
  AXMLDocument := InternalCreateStyleDocument(AStream);
  AReader := GetStyleReaderClass(AXMLDocument).Create;
  try
    Result := AReader.CreateStyle(AXMLDocument);
  finally
    AReader.Free;
    AXMLDocument.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.CreateFromStyle(ADestinationStyleInfo, ASourceStyleInfo: TdxGaugeScaleStyleInfo):
  TdxGaugeScaleStyle;

  procedure LoadElements(ADestinationStyle, ASourceStyle: TdxGaugeScaleStyle);
  var
    I: TdxGaugeElementType;
    AShape: TdxCompositeShape;
  begin
    for I := Low(TdxGaugeElementType) to High(TdxGaugeElementType) do
      if ADestinationStyle.GetElement(I) = nil then
      begin
        AShape := TdxCompositeShape.Create;
        TdxCompositeShapeAccess(AShape).LoadFromShape(ASourceStyle.Shapes, dxGaugeStyleElementNameMap[I]);
        if not AShape.Empty then
          ADestinationStyle.AddElement(I, AShape)
        else
          AShape.Free;
      end;
  end;

var
  ASourceStyle: TdxGaugeScaleStyle;
begin
  ASourceStyle := CreateStyle(ASourceStyleInfo);
  try
    Result := CreateStyle(ADestinationStyleInfo);
    LoadElements(Result, ASourceStyle);
  finally
    ASourceStyle.Free;
  end;
end;

function TdxGaugeScaleStyleCreator.GetStyleReaderClass(ADocument: TdxXMLDocument): TdxGaugeCustomScaleStyleReaderClass;
var
  ANode: TdxXMLNode;
begin
  Result := TdxGaugeCustomScaleStyleReader;
  if not(IsStyleDocumentAvailable(ADocument) and ADocument.Root.FindChild('ScaleStyle',
      ANode) and ANode.FindChild('DefaultParameters', ANode) and IdentifyReaderClass(ANode, Result)) then
    dxGaugeRaiseException(sdxGaugeScaleStyleCollectionInvalidStyleFileFormat, []);
end;

function TdxGaugeScaleStyleCreator.IdentifyReaderClass(ADefaultParametersNode: TdxXMLNode;
  var AReaderClass: TdxGaugeCustomScaleStyleReaderClass): Boolean;
var
  I: Integer;
  ATypeInfo: TdxGaugeScaleTypeInfo;
begin
  Result := False;
  for I := 0 to dxGaugeScaleTypeInfos.GetTypeCount - 1 do
  begin
    ATypeInfo := dxGaugeScaleTypeInfos.GetTypeInfo(I);
    Result := ADefaultParametersNode.FindChild(ATypeInfo.ReaderClass.GetParametersNodeName) <> nil;
    if Result then
    begin
      AReaderClass := ATypeInfo.ReaderClass;
      Break;
    end;
  end;
end;

function TdxGaugeScaleStyleCreator.InternalCreateStyleDocument(AStream: TStream): TdxXMLDocument;
begin
  Result := TdxXMLDocument.Create(nil);
  Result.LoadFromStream(AStream);
  if not IsStyleDocumentAvailable(Result) then
    FreeAndNil(Result);
end;

function TdxGaugeScaleStyleCreator.IsStyleDocumentAvailable(ADocument: TdxXMLDocument): Boolean;
begin
  Result := (ADocument <> nil) and (ADocument.Root <> nil);
end;

{ TdxGaugeScaleStyleFactory }

constructor TdxGaugeScaleStyleFactory.Create;
begin
  inherited Create;
  FStyles := TDictionary<TdxGaugeScaleType, TStringList>.Create;
  FStyleCreator := TdxGaugeScaleStyleCreator.Create;
  FPredefinedStyleNames := TStringList.Create(True);
  InitPredefinedStyleNames;
end;

destructor TdxGaugeScaleStyleFactory.Destroy;
begin
  FreeAndNil(FPredefinedStyleNames);
  FreeAndNil(FStyleCreator);
  InternalUnregisterStyles;
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TdxGaugeScaleStyleFactory.CreateStyle(AScaleType: TdxGaugeScaleType;
  const AStyleName: string): TdxGaugeScaleStyle;
begin
  Result := InternalCreateStyle(GetStyleInfo(GetStyles(AScaleType), AStyleName));
end;

function TdxGaugeScaleStyleFactory.CreateStyleFromFile(const AFileName: string): TdxGaugeScaleStyle;
begin
  Result := FStyleCreator.CreateStyle(AFileName);
end;

function TdxGaugeScaleStyleFactory.GetFirstStyleName(AScaleType: TdxGaugeScaleType): string;
begin
  Result := GetStyles(AScaleType).Strings[0];
end;

function TdxGaugeScaleStyleFactory.GetStyles(AScaleType: TdxGaugeScaleType): TStringList;
var
  AList: TStringList;
begin
  if FStyles.TryGetValue(AScaleType, AList) then
    Result := AList
  else
  begin
    FStyles.Add(AScaleType, CreateStyleList);
    Result := GetStyles(AScaleType);
  end;
end;

function TdxGaugeScaleStyleFactory.HasStyles(AScaleType: TdxGaugeScaleType): Boolean;
begin
  Result := GetStyles(AScaleType).Count > 0;
end;

function TdxGaugeScaleStyleFactory.RegisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;

  // # duplicate from CreateStyleInfo
  // # internal styles are always valid
  function CreateStyleInfo: TdxGaugeScaleStyleInfo;
  begin
    Result := TdxGaugeScaleStyleInfo.Create;
    Result.Name := AStyleName;
    Result.ScaleType := AScaleType;
    Result.ResourceName := dxGaugeScaleTypeInfos.GetTypeName(AScaleType) + AStyleName;
    Result.IsExternalStyle := False;
    if IsInheritedStyle(Result.ScaleType) then
      Result.BaseStyleName := AStyleName;
  end;

begin
  Result := InternalRegisterStyle(AScaleType, CreateStyleInfo);
end;

function TdxGaugeScaleStyleFactory.RegisterStyleFromFile(AScaleType: TdxGaugeScaleType;
  const AFileName: string): Boolean;
begin
  Result := InternalRegisterStyle(AScaleType, CreateStyleInfo(AFileName));
end;

procedure TdxGaugeScaleStyleFactory.GetPredefinedStyleNames(AList: TStringList);
var
  I: Integer;
begin
  if AList <> nil then
    for I := 0 to FPredefinedStyleNames.Count - 1 do
      AList.Add(FPredefinedStyleNames.Strings[I]);
end;

procedure TdxGaugeScaleStyleFactory.RegisterPredefinedStyles(AScaleClass: TdxGaugeCustomScaleClass);

  function CheckPredefineStyleName(const AStyleName: string): Boolean;
  begin
    Result := FindResource(HInstance, PChar(dxgGaugeScaleTypeInfos.GetTypeName(AScaleClass.GetScaleType) + AStyleName),
      RT_RCDATA) <> 0;
  end;

var
  I: Integer;
begin
  for I := 0 to FPredefinedStyleNames.Count - 1 do
    if CheckPredefineStyleName(FPredefinedStyleNames.Strings[I]) then
      RegisterStyle(AScaleClass.GetScaleType, FPredefinedStyleNames.Strings[I]);
end;

procedure TdxGaugeScaleStyleFactory.SaveStyleToFile(AScaleType: TdxGaugeScaleType; const AStyleName, AFileName: string);
var
  AXMLDocument: TdxXMLDocument;
begin
  AXMLDocument := ExtractStyleToDocument(AScaleType, AStyleName);
  try
    AXMLDocument.SaveToFile(AFileName);
  finally
    AXMLDocument.Free;
  end;
end;


procedure TdxGaugeScaleStyleFactory.SaveStyleToStream(AScaleType: TdxGaugeScaleType; const AStyleName: string;
  AStream: TStream);
var
  AXMLDocument: TdxXMLDocument;
begin
  AXMLDocument := ExtractStyleToDocument(AScaleType, AStyleName);
  try
    AXMLDocument.SaveToStream(AStream);
  finally
    AXMLDocument.Free;
  end;
end;

procedure TdxGaugeScaleStyleFactory.UnregisterPredefinedStyles(AScaleClass: TdxGaugeCustomScaleClass);
var
  I: Integer;
begin
  for I := 0 to FPredefinedStyleNames.Count - 1 do
    UnregisterStyle(AScaleClass.GetScaleType, FPredefinedStyleNames.Strings[I]);
end;

procedure TdxGaugeScaleStyleFactory.UnregisterStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string);
var
  AStyleList: TStringList;
begin
  AStyleList := GetStyles(AScaleType);
  InternalUnregisterStyle(AStyleList, AStyleName);
  if AStyleList.Count = 0 then
  begin
    FStyles.Remove(AScaleType);
    AStyleList.Free;
  end;
end;

procedure TdxGaugeScaleStyleFactory.UnregisterStyles(AScaleType: TdxGaugeScaleType);

  procedure UnregistryStyles(BB12BCDF1E7549E491DF6037C0C7DE6C: TStringList);
  begin
    if BB12BCDF1E7549E491DF6037C0C7DE6C <> nil then
      while BB12BCDF1E7549E491DF6037C0C7DE6C.Count > 0 do
        DeleteStyle(BB12BCDF1E7549E491DF6037C0C7DE6C, 0);
  end;

var
  AStyleList: TStringList;
begin
  if FStyles.TryGetValue(AScaleType, AStyleList) then
  begin
    UnregistryStyles(AStyleList);
    FStyles.Remove(AScaleType);
    AStyleList.Free;
  end;
end;

function TdxGaugeScaleStyleFactory.CreateStyleInfo(const AFileName: string): TdxGaugeScaleStyleInfo;
var
  AStyle: TdxGaugeScaleStyle;
begin
  AStyle := FStyleCreator.CreateStyle(AFileName);
  try
    Result := TdxGaugeScaleStyleInfo.Create;
    Result.Name := AStyle.Name;
    Result.ResourceName := AFileName;
    Result.IsExternalStyle := True;
    Result.ScaleType := dxGaugeScaleTypeInfos.GetTypeByName(AStyle.ScaleTypeName);
    Result.BaseStyleName := AStyle.BaseStyleName;
  finally
    AStyle.Free;
  end;
end;

function TdxGaugeScaleStyleFactory.CreateStyleList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupError;
end;

function TdxGaugeScaleStyleFactory.ExtractStyleToDocument(AScaleType: TdxGaugeScaleType;
  const AStyleName: string): TdxXMLDocument;
var
  AStyleInfo: TdxGaugeScaleStyleInfo;
begin
  AStyleInfo := GetStyleInfo(GetStyles(AScaleType), AStyleName);
  Result := FStyleCreator.CreateStyleDocument(AStyleInfo);
end;

function TdxGaugeScaleStyleFactory.GetStyleInfo(AStyles: TStringList; const AStyleName: string): TdxGaugeScaleStyleInfo;
var
  AIndex: Integer;
begin
  if AStyles <> nil then
    if AStyles.Find(AStyleName, AIndex) then
      Result := TdxGaugeScaleStyleInfo(AStyles.Objects[AIndex])
    else
      Result := TdxGaugeScaleStyleInfo(AStyles.Objects[0])
    else
      Result := nil;
end;

function TdxGaugeScaleStyleFactory.InternalCreateStyle(AStyleInfo: TdxGaugeScaleStyleInfo): TdxGaugeScaleStyle;
begin
  if not SameText(AStyleInfo.BaseStyleName, '') then
    Result := FStyleCreator.CreateFromStyle(AStyleInfo, GetStyleInfo(GetStyles(stCircularScale),
        AStyleInfo.BaseStyleName))
  else
    Result := FStyleCreator.CreateStyle(AStyleInfo);
end;

function TdxGaugeScaleStyleFactory.InternalRegisterStyle(AScaleType: TdxGaugeScaleType;
  AStyleInfo: TdxGaugeScaleStyleInfo): Boolean;
var
  AStyles: TStringList;
begin
  AStyles := GetStyles(AStyleInfo.ScaleType);
  if AScaleType = AStyleInfo.ScaleType then
  begin
    if (AStyles <> nil) and (AStyles.IndexOf(AStyleInfo.Name) = -1) then
    begin
      if (AStyleInfo.BaseStyleName = '') then
        AStyles.AddObject(AStyleInfo.Name, TObject(AStyleInfo))
      else
      begin
        if IsRegisteredStyle(stCircularScale, AStyleInfo.BaseStyleName) then
          AStyles.AddObject(AStyleInfo.Name, TObject(AStyleInfo))
        else
          ProcessError(AStyleInfo, 0);
      end;
    end
    else
      ProcessError(AStyleInfo, 2);
  end
  else
    ProcessError(AStyleInfo, 1);
  Result := True;
end;

function TdxGaugeScaleStyleFactory.InternalUnregisterStyle(AStyles: TStringList; const AStyleName: string): Boolean;
var
  AIndex: Integer;
begin
  Result := AStyles.Find(AStyleName, AIndex);
  if Result then
    DeleteStyle(AStyles, AIndex);
end;

function TdxGaugeScaleStyleFactory.IsInheritedStyle(AScaleType: TdxGaugeScaleType): Boolean;
begin
  Result := not(AScaleType in [stDigitalScale, stLinearScale, stCircularScale]);
end;

function TdxGaugeScaleStyleFactory.IsRegisteredStyle(AScaleType: TdxGaugeScaleType; const AStyleName: string): Boolean;
var
  AStyles: TStringList;
begin
  AStyles := GetStyles(AScaleType);
  Result := (AStyles.Count > 0) and (AStyles.IndexOf(AStyleName) <> -1);
end;

procedure TdxGaugeScaleStyleFactory.DeleteStyle(AStyles: TStringList; AIndex: Integer);
begin
  if AStyles <> nil then
  begin
    AStyles.Objects[AIndex].Free;
    AStyles.Delete(AIndex);
  end;
end;

procedure TdxGaugeScaleStyleFactory.InitPredefinedStyleNames;
begin
  FPredefinedStyleNames.Add(dxGaugeAfricaSunsetStyleName);
  FPredefinedStyleNames.Add(dxGaugeClassicStyleName);
  FPredefinedStyleNames.Add(dxGaugeCleanWhiteStyleName);
  FPredefinedStyleNames.Add(dxGaugeCleverStyleName);
  FPredefinedStyleNames.Add(dxGaugeCosmicStyleName);
  FPredefinedStyleNames.Add(dxGaugeDarkNightStyleName);
  FPredefinedStyleNames.Add(dxGaugeDeepFireStyleName);
  FPredefinedStyleNames.Add(dxGaugeDiscoStyleName);
  FPredefinedStyleNames.Add(dxGaugeEcoStyleName);
  FPredefinedStyleNames.Add(dxGaugeIceColdZoneStyleName);
  FPredefinedStyleNames.Add(dxGaugeiStyleStyleName);
  FPredefinedStyleNames.Add(dxGaugeFutureStyleName);
  FPredefinedStyleNames.Add(dxGaugeMechanicalStyleName);
  FPredefinedStyleNames.Add(dxGaugeMilitaryStyleName);
  FPredefinedStyleNames.Add(dxGaugeRedStyleName);
  FPredefinedStyleNames.Add(dxGaugeRetroStyleName);
  FPredefinedStyleNames.Add(dxGaugeSilverBlurStyleName);
  FPredefinedStyleNames.Add(dxGaugeShiningDarkStyleName);
  FPredefinedStyleNames.Add(dxGaugeSmartStyleName);
  FPredefinedStyleNames.Add(dxGaugeSportCarStyleName);
  FPredefinedStyleNames.Add(dxGaugeYellowSubmarineStyleName);
  FPredefinedStyleNames.Add(dxGaugeWhiteStyleName);
end;

procedure TdxGaugeScaleStyleFactory.InternalUnregisterStyles;
var
  AScaleType: TdxGaugeScaleType;
begin
  for AScaleType := Low(TdxGaugeScaleType) to High(TdxGaugeScaleType) do
    UnregisterStyles(AScaleType);
end;

procedure TdxGaugeScaleStyleFactory.ProcessError(AStyleInfo: TdxGaugeScaleStyleInfo; AErrorCode: Integer);
var
  ABaseStyleName, AStyleName: string;
  AScaleType: TdxGaugeScaleType;
begin
  AStyleName := AStyleInfo.Name;
  ABaseStyleName := AStyleInfo.BaseStyleName;
  AScaleType := AStyleInfo.ScaleType;
  FreeAndNil(AStyleInfo);
  case AErrorCode of
    0:
      dxGaugeRaiseException(sdxGaugeScaleStyleCollectionBaseStyleUnregistered, [ABaseStyleName, AStyleName,
        dxGaugeScaleTypeInfos.GetFormatedTypeName(AScaleType),
        dxGaugeScaleTypeInfos.GetFormatedTypeName(stCircularScale)]);
    1:
      dxGaugeRaiseException(sdxGaugeScaleStyleCollectionInvalidStyleFileFormat, []);
    2:
      dxGaugeRaiseException(sdxGaugeScaleStyleCollectionDuplicateStyleName, [AStyleName,
        dxGaugeScaleTypeInfos.GetFormatedTypeName(AScaleType)]);
  end;
end;

{ TdxGaugeScaleFactory }

function TdxGaugeScaleFactory.IsRegisteredScale(AScaleClass: TdxGaugeCustomScaleClass): Boolean;
begin
  Result := GetIndexByClass(AScaleClass) <> -1;
end;

procedure TdxGaugeScaleFactory.RegisterScale(AScaleClass: TdxGaugeCustomScaleClass;
  ARegisterPredefinedStyle: Boolean = True);
begin
  if GetIndexByClass(AScaleClass) = -1 then
  begin
    if FindByClassName(AScaleClass.GetScaleName) = nil then
    begin
      dxGaugeScaleTypeInfos.AddTypeInfo(AScaleClass);
      Register(AScaleClass, AScaleClass.GetScaleName);
      if ARegisterPredefinedStyle then
        dxGaugeScaleStyleFactory.RegisterPredefinedStyles(AScaleClass);
    end
    else
      dxGaugeRaiseException(sdxGaugeScaleFactoryDuplicateScaleClassName, [AScaleClass.GetScaleName]);
  end;
end;

procedure TdxGaugeScaleFactory.UnregisterScale(AScaleClass: TdxGaugeCustomScaleClass;
  AUnregisterPredefinedStyle: Boolean = True);

  function CanUnregisterType(AScaleClass: TdxGaugeCustomScaleClass): Boolean;
  var
    I: Integer;
    AScaleClassIndex: Integer;
    AClass: TdxGaugeCustomScaleClass;
  begin
    Result := True;
    AScaleClassIndex := GetIndexByClass(AScaleClass);
    for I := 0 to Count - 1 do
      if I <> AScaleClassIndex then
      begin
        AClass := TdxGaugeCustomScaleClass(Items[I]);
        Result := AClass.GetScaleType <> AScaleClass.GetScaleType;
        if not Result then
          Break;
      end;
  end;

var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AScaleClass);
  if AIndex <> -1 then
  begin
    if CanUnregisterType(AScaleClass) and AUnregisterPredefinedStyle then
      dxGaugeScaleStyleFactory.UnregisterPredefinedStyles(AScaleClass);
    Unregister(AScaleClass);
  end;
end;

procedure TdxGaugeScaleFactory.UnregisterScales;
begin
  while Count > 0 do
    UnregisterScale(TdxGaugeCustomScaleClass(Items[0]));
end;

initialization
  dxGaugeRegisterScale(TdxGaugeContainerScale, True);

finalization
  dxGaugeUnregisterScale(TdxGaugeContainerScale, True);

  FreeAndNil(dxgGaugeScaleTypeInfos);
  FreeAndNil(dxgGaugeScaleStyleFactory);
  FreeAndNil(dxgGaugeScaleFactory);

end.
