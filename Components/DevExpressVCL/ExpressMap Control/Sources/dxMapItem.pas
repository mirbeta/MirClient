{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapItem;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Graphics, SysUtils, Math, Windows, Controls, Generics.Defaults, Generics.Collections,
  Contnrs, StrUtils,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGraphics, cxGeometry, cxClasses,
  dxGDIPlusClasses, dxGDIPlusApi, cxLookAndFeelPainters, dxScreenTip,
  dxMapLayer, dxMapControlTypes, dxMapItemStyle, dxMapControlElementViewInfo, dxMapControlGeometry;

type
  TdxMapItem = class;
  TdxMapPointer = class;
  TdxMapPath = class;
  TdxMapPathSegment = class;
  TdxMapDot = class;
  TdxMapItemTextViewInfo = class;
  TdxMapItemStyles = array [Low(TdxMapControlElementState)..High(TdxMapControlElementState)] of TdxMapItemStyle;

  TdxMapItemStylesHelper = class
  private
    FIsMaxFontSizeValid: Boolean;
    FMaxSizeFont: TFont;
    FStyles: TdxMapItemStyles;
    FOnChanged: TNotifyEvent;
    procedure StyleChanged(ASender: TObject);
    function GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;
    procedure SetStyle(AState: TdxMapControlElementState;
      const Value: TdxMapItemStyle);
  protected
    function GetActualStates: TdxMapControlElementStates; virtual;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure EnumActualStyles(AStyleProc: TdxMapControlEnumElementStateProc);
    function GetMaxSizeFont: TFont;
    property Style[AState: TdxMapControlElementState]: TdxMapItemStyle read GetStyle write SetStyle; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TdxCustomMapItemViewInfo = class(TdxMapControlElementViewInfo)
  protected
    procedure DoPaint; virtual;
    function GetVisibleBounds: TRect; virtual;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapItemViewInfo = class(TdxCustomMapItemViewInfo)
  private
    FIsCachedBoundsValid: Boolean;
    FIsStylesValid: Boolean;
    FIsTitleSizeValid: Boolean;
    FItem: TdxMapItem;
    FCachedRenderScale, FCachedRenderOffset: TdxPointDouble;
    FMapCenterPoint: TdxPointDouble;
    FMaxTitleSize: TdxSizeDouble;
    FScreenCenterPoint: TPoint;
    FStyles: TdxMapItemStylesHelper;
    FTextInfo: TdxMapItemTextViewInfo;
    procedure CalculateCachedBounds;
    function GetBorderColor(AState: TdxMapControlElementState): TdxAlphaColor;
    function GetBorderWidth(AState: TdxMapControlElementState): Integer;
    function GetColor(AState: TdxMapControlElementState): TdxAlphaColor;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewPortBounds: TRect;
    procedure MergeStyles(AState: TdxMapControlElementState);
    function ParseTextPattern(const ATextPattern: string): string;
    procedure ResetRenderCache;
  protected
    procedure CalculateScreenBounds; virtual;
    procedure CalculateTitleBounds;
    procedure DoCalculateCachedBounds; virtual;
    function DoCalculateTitleBounds: TRect; virtual;
    function DoCanBeVisible: Boolean; virtual;
    procedure DoElementDestroying; override;
    function GetForbiddenStates: TdxMapControlElementStates; override;
    function GetMaxSizeFont: TFont;
    function GetHitTestIndex: Integer; override;
    function GetMapBounds: TdxRectDouble; virtual;
    function GetStyle: TdxMapItemStyle; virtual;
    function GetText: string; virtual;
    function GetTextColor(AState: TdxMapControlElementState): TdxAlphaColor; virtual;
    function GetTextGlowColor(AState: TdxMapControlElementState): TdxAlphaColor; virtual;
    function GetTitle: TdxMapItemTextViewInfo; virtual;
    function GetVisibleBounds: TRect; override;
    function HasText: Boolean; virtual;
    procedure CapturePaintCanvas;
    function DoGetMapBounds: TdxRectDouble; virtual;
    procedure Invalidate; override;
    function IsBoundsScalable: Boolean; virtual;
    function IsOwnerVisible: Boolean; virtual;
    function IsTitleOutOfBounds: Boolean; virtual;
    function NeedCalculateScreenBoundsToDetermineVisibility: Boolean; virtual;
    function PtInElement(const APoint: TPoint): Boolean; override;
    procedure ReleasePaintCanvas;
    procedure UpdateStyles;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property TextInfo: TdxMapItemTextViewInfo read FTextInfo;
    property ViewPortBounds: TRect read GetViewPortBounds;
  public
    constructor Create(AOwner: TdxMapItem); reintroduce; virtual;
    destructor Destroy; override;
    procedure CalculateBounds; override;
    function CanBeVisible: Boolean; virtual;
    procedure ClearCache; override;
    function GetHint: string; override;
    function GetScreenTip: TdxScreenTip; override;
    function HasTitle: Boolean; virtual;
    function IsIntersect(const ARect: TRect): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property Item: TdxMapItem read FItem;
    property Style: TdxMapItemStyle read GetStyle;
    property Title: TdxMapItemTextViewInfo read GetTitle;
  end;

  TdxMapItemAttributes = class
  private
    FItems: TDictionary<string, Variant>;
    FOnChanged: TNotifyEvent;
    procedure Changed(Sender: TObject; const Item: Variant;
      Action: TCollectionNotification);
    function GetValues(const AName: string): Variant;
    procedure SetValues(const AName: string; const Value: Variant);
  protected
    property Items: TDictionary<string, Variant> read FItems;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AName: string; AValue: Variant);
    function ContainsName(const AName: string): Boolean;
    procedure GetNames(ANames: TStrings);
    procedure Remove(const AName: string);
    property Values[const AName: string]: Variant read GetValues write SetValues; default;
  end;

  TdxMapItemTitleVisibility = (mitvAuto, mitvVisible, mitvHidden);

  TdxMapItemTitleOptions = class(TcxOwnedPersistent)
  private
    FText: string;
    FVisibility: TdxMapItemTitleVisibility;
    FOnChanged: TNotifyEvent;
    procedure SetText(const Value: string);
    procedure SetVisibility(const Value: TdxMapItemTitleVisibility);
  protected
    procedure Changed; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Text: string read FText write SetText;
    property Visibility: TdxMapItemTitleVisibility read FVisibility write SetVisibility default mitvAuto;
  end;

  TdxMapItemTextViewInfo = class(TdxCustomMapItemViewInfo)
  private
    FCachedText: TdxSmartImage;
    FCachedState: TdxMapControlElementState;
    FClientBounds: TRect;
    FIsCacheTextValid: Boolean;
    FMapItemViewInfo: TdxMapItemViewInfo;
    FText: string;
  protected
    function DoCalculateSize: TSize; override;
    procedure DoPaint; override;
    function GetState: TdxMapControlElementState; override;
    property ClientBounds: TRect read FClientBounds;
  public
    constructor Create(AMapItemViewInfo: TdxMapItemViewInfo); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TdxMapItem = class(TcxComponentCollectionItem)
  private
 //   FAllowHotTrack: Boolean;
    FAttributes: TdxMapItemAttributes;
    FHint: string;
    FLayer: TdxMapLayer;
    FStyles: TdxMapItemStylesHelper;
    FViewInfo: TdxMapItemViewInfo;
    FScreenTip: TdxScreenTip;
    FTitleOptions: TdxMapItemTitleOptions;
    FVisible: Boolean;
    function GetSelected: Boolean;
    procedure AttributesChanged(ASender: TObject);
    procedure SetSelected(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure StyleChanged(ASender: TObject);
    procedure SetStyle(const Index: TdxMapControlElementState; const Value: TdxMapItemStyle);
    procedure SetTitleOptions(const Value: TdxMapItemTitleOptions);
    procedure TitleOptionsChanged(ASender: TObject);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateViewInfo: TdxMapItemViewInfo; virtual;
    procedure DoAssign(ASource: TdxMapItem); virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetMapBounds: TdxRectDouble; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SelectionChanged(AShift: TShiftState);

    property Layer: TdxMapLayer read FLayer write FLayer;
    property TitleOptions: TdxMapItemTitleOptions read FTitleOptions write SetTitleOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetHint: string;
    function GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;
    procedure InvalidateStylesInfo;
    procedure InvalidateTitleInfo;
    function IsPointer: Boolean; virtual;
    procedure SetParentComponent(Value: TComponent); override;

    property Attributes: TdxMapItemAttributes read FAttributes;
    property Selected: Boolean read GetSelected write SetSelected;
    property ViewInfo: TdxMapItemViewInfo read FViewInfo;
  published
  //  property AllowHotTrack: Boolean read FAllowHotTrack write FAllowHotTrack default False;
    property Hint: string read FHint write FHint;
    property ScreenTip: TdxScreenTip read FScreenTip write FScreenTip;
    property Style: TdxMapItemStyle index mcesNormal read GetStyle write SetStyle;
    property StyleHot: TdxMapItemStyle index mcesHot read GetStyle write SetStyle;
    property StyleSelected: TdxMapItemStyle index mcesSelected read GetStyle write SetStyle;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TdxMapItemClass = class of TdxMapItem;
  TdxMapItemList = class(TList<TdxMapItem>);

  TdxMapItems = class(TcxComponentCollection)
  private
    function GetItem(Index: Integer): TdxMapItem;
    procedure SetItem(Index: Integer; const Value: TdxMapItem);
  protected
    procedure ChangeScale(M, D: Integer);
    function GetItemPrefixName: string; override;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    function Add(AMapItemClass: TdxMapItemClass): TdxMapItem;

    property Items[Index: Integer]: TdxMapItem read GetItem write SetItem; default;
  end;

  TdxMapSinglePointItem = class(TdxMapItem)
  private
    FLocation: TdxMapControlGeoLocation;
    procedure LocationChanged(ASender: TObject);
    procedure SetLocation(Value: TdxMapControlGeoLocation);
  protected
    procedure DoAssign(ASource: TdxMapItem); override;
    property Location: TdxMapControlGeoLocation read FLocation write SetLocation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TdxMapDotShapeKind = (mcskCircle, mcskRectangle);

  TdxMapDotViewInfo = class(TdxMapItemViewInfo)
  private
    FMapLocation: TdxPointDouble;
    FRadius: Double;
    FRelativeBounds: TdxRectDouble;
    function GetMapDot: TdxMapDot;
  protected
    procedure CalculateScreenBounds; override;
    procedure DoCalculateCachedBounds; override;
    function DoCanBeVisible: Boolean; override;
    function DoGetMapBounds: TdxRectDouble; override;
    procedure DoPaint; override;
  public
    property MapDot: TdxMapDot read GetMapDot;
  end;

  TdxMapDot = class(TdxMapSinglePointItem)
  private
    FShapeKind: TdxMapDotShapeKind;
    FSize: Integer;
    procedure SetShapeKind(Value: TdxMapDotShapeKind);
    procedure SetSize(Value: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function CreateViewInfo: TdxMapItemViewInfo; override;
    procedure DoAssign(ASource: TdxMapItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    function IsPointer: Boolean; override;
  published
    property Location;
    property ShapeKind: TdxMapDotShapeKind read FShapeKind write SetShapeKind default mcskCircle;
    property Size: Integer read FSize write SetSize default 5;
  end;

  TdxMapRectangleViewInfo = class(TdxMapItemViewInfo)
  private
    FMapBounds: TdxRectDouble;
    FMapPoint1, FMapPoint2: TdxPointDouble;
    FVisibleRect: TRect;
  protected
    procedure CalculateScreenBounds; override;
    procedure DoCalculateCachedBounds; override;
    function DoCanBeVisible: Boolean; override;
    function DoGetMapBounds: TdxRectDouble; override;
    procedure DoPaint; override;
    function IsBoundsScalable: Boolean; override;
  end;

  TdxMapRectangle = class(TdxMapSinglePointItem)
  private
    FHeight: Double;
    FWidth: Double;
    procedure SetHeight(Value: Double);
    procedure SetWidth(Value: Double);
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
    procedure DoAssign(ASource: TdxMapItem); override;
  published
    property Height: Double read FHeight write SetHeight;
    property Location;
    property TitleOptions;
    property Width: Double read FWidth write SetWidth;
  end;

  TdxMapEllipseViewInfo = class(TdxMapRectangleViewInfo)
  protected
    procedure DoPaint; override;
    function PtInElement(const APoint: TPoint): Boolean; override;
  end;

  TdxMapEllipse = class(TdxMapRectangle)
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
  end;

  TdxMapControlGeoPointItem = class(TCollectionItem)
  private
    FGeoPoint: TdxMapControlGeoPoint;
    function GetLatitude: Double;
    function GetLongitude: Double;
    procedure SetGeoPoint(const Value: TdxMapControlGeoPoint);
    procedure SetLatitude(const Value: Double);
    procedure SetLongitude(const Value: Double);
  public
    procedure Assign(Source: TPersistent); override;
    property GeoPoint: TdxMapControlGeoPoint read FGeoPoint write SetGeoPoint;
  published
    property Longitude: Double read GetLongitude write SetLongitude;
    property Latitude: Double read GetLatitude write SetLatitude;
  end;

  TdxMapControlGeoPointCollection = class(TCollection)
  private
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TdxMapControlGeoPointItem;
    procedure SetItem(Index: Integer; const Value: TdxMapControlGeoPointItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TdxMapControlGeoPointItem;
    procedure AddRange(AGeoPoints: TdxMapControlGeoPoints);

    property Items[Index: Integer]: TdxMapControlGeoPointItem read GetItem write SetItem; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TdxMapCustomMultiPointItemViewInfo = class(TdxMapItemViewInfo)
  protected
    FMapBounds: TdxRectDouble;
    FMapPoints: TdxDoublePoints;
    FScreenPoints: TPoints;
    function DoGetMapBounds: TdxRectDouble; override;
  end;

  TdxMapCustomMultiPointItem = class(TdxMapItem)
  private
    FGeoPoints: TdxMapControlGeoPointCollection;
    procedure SetGeoPoints(const Value: TdxMapControlGeoPointCollection);
  protected
    procedure DoAssign(ASource: TdxMapItem); override;
    procedure GeoPointsChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property GeoPoints: TdxMapControlGeoPointCollection read FGeoPoints write SetGeoPoints;
  end;

  TdxMapPolylineViewInfo = class(TdxMapCustomMultiPointItemViewInfo)
  private
    FCheckIntersections: Boolean;
    FVisiblePolylines: TdxPointsArray;
  protected
    procedure CalculateScreenBounds; override;
    function CreatePath: TdxGpPath;
    procedure DoCalculateCachedBounds; override;
    function DoCanBeVisible: Boolean; override;
    procedure DoPaint; override;
    function GetGeoPoints: TdxMapControlGeoPointCollection; virtual;
    function InternalIsIntersect(const ARect: TRect): Boolean;
    function InternalPtInElement(const APoint: TPoint): Boolean;
    function IsBoundsScalable: Boolean; override;
    function PtInElement(const APoint: TPoint): Boolean; override;
  public
    function HasTitle: Boolean; override;
    function IsIntersect(const ARect: TRect): Boolean; override;
  end;

  TdxMapPolyline = class(TdxMapCustomMultiPointItem)
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
  published
    property GeoPoints;
  end;

  TdxMapCustomPolygonViewInfo = class(TdxMapCustomMultiPointItemViewInfo)
  private
    FMapArea: Double;
    FCheckIntersections: Boolean;
    FVisiblePolygons: TdxPointsArray;
  protected
    procedure AddToPath(APath: TdxGPPath);
    procedure CalculateScreenBounds; override;
    procedure DoCalculateCachedBounds; override;
    function DoCanBeVisible: Boolean; override;
    function GetGeoPoints: TdxMapControlGeoPointCollection; virtual; abstract;
    function IsBoundsScalable: Boolean; override;
  end;

  TdxMapPolygonViewInfo = class(TdxMapCustomPolygonViewInfo)
  protected
    function CreatePath: TdxGpPath;
    procedure DoPaint; override;
    function GetGeoPoints: TdxMapControlGeoPointCollection; override;
    function InternalIsIntersect(const ARect: TRect): Boolean;
    function InternalPtInElement(const APoint: TPoint): Boolean;
    function PtInElement(const APoint: TPoint): Boolean; override;
  public
    function IsIntersect(const ARect: TRect): Boolean; override;
  end;

  TdxMapPolygon = class(TdxMapCustomMultiPointItem)
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
  published
    property GeoPoints;
    property TitleOptions;
  end;

  TdxMapPathSegmentPolygonViewInfo = class(TdxMapCustomPolygonViewInfo)
  private
    FSegment: TdxMapPathSegment;
  protected
    function GetHitTestIndex: Integer; override;
    function GetStyle: TdxMapItemStyle; override;
    function GetTitle: TdxMapItemTextViewInfo; override;
    function IsOwnerVisible: Boolean; override;
    // custompolygonviewinfo
    function GetGeoPoints: TdxMapControlGeoPointCollection; override;
    property Segment: TdxMapPathSegment read FSegment;
  public
    constructor Create(AOwner: TdxMapPathSegment); reintroduce; virtual;
    function HasTitle: Boolean; override;
  end;

  TdxMapPathSegmentPolylineViewInfo = class(TdxMapPolylineViewInfo)
  private
    FSegment: TdxMapPathSegment;
  protected
    function GetHitTestIndex: Integer; override;
    function GetStyle: TdxMapItemStyle; override;
    function IsOwnerVisible: Boolean; override;
    // custompolylineviewinfo
    function GetGeoPoints: TdxMapControlGeoPointCollection; override;
    property Segment: TdxMapPathSegment read FSegment;
  public
    constructor Create(AOwner: TdxMapPathSegment); reintroduce; virtual;
  end;

  TdxMapSegmentType = (mstPolygon, mstPolyline);

  TdxMapPathSegment = class(TCollectionItem)
  private
    FGeoPoints: TdxMapControlGeoPointCollection;
    FViewInfo: TdxMapCustomMultiPointItemViewInfo;
    FVisible: Boolean;
    procedure SetGeoPoints(const Value: TdxMapControlGeoPointCollection);
    procedure SetVisible(const Value: Boolean);
  protected
    function CreateViewInfo: TdxMapCustomMultiPointItemViewInfo;
    procedure GeoPointsChanged(Sender: TObject);
    procedure RecreateViewInfo;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ViewInfo: TdxMapCustomMultiPointItemViewInfo read FViewInfo;
  published
    property GeoPoints: TdxMapControlGeoPointCollection read FGeoPoints write SetGeoPoints;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TdxMapPathSegments = class(TOwnedCollection)
  private
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TdxMapPathSegment;
    procedure SetItem(Index: Integer; const Value: TdxMapPathSegment);
  protected
    procedure RecreateViewInfos;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TdxMapPathSegment;

    property Items[Index: Integer]: TdxMapPathSegment read GetItem write SetItem; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TdxMapPathCustomViewInfo<T: TdxMapCustomMultiPointItemViewInfo> = class(TdxMapItemViewInfo)
  private
    function GetMapPath: TdxMapPath;
    function GetSegmentInfo(Index: Integer): T;
  protected
    procedure AddVisibleElements; override;
    function GetMapBounds: TdxRectDouble; override;
  public
    procedure CalculateBounds; override;
    procedure ClearCache; override;
    property MapPath: TdxMapPath read GetMapPath;
    property SegmentInfo[Index: Integer]: T read GetSegmentInfo;
  end;

  TdxMapPathPolygonViewInfo = class(TdxMapPathCustomViewInfo<TdxMapPathSegmentPolygonViewInfo>)
  private
    FTitleSegmentViewInfo: TdxMapPathSegmentPolygonViewInfo;
  protected
    procedure AddVisibleElements; override;
    function CreatePath: TdxGpPath;
    function DoCalculateTitleBounds: TRect; override;
    procedure DoPaint; override;
    function IsTitleOutOfBounds: Boolean; override;
    function PtInElement(const APoint: TPoint): Boolean; override;
  public
    function IsIntersect(const ARect: TRect): Boolean; override;
  end;

  TdxMapPathPolylineViewInfo = class(TdxMapPathCustomViewInfo<TdxMapPathSegmentPolylineViewInfo>)
  protected
    procedure DoPaint; override;
    function PtInElement(const APoint: TPoint): Boolean; override;
  public
    function HasTitle: Boolean; override;
    function IsIntersect(const ARect: TRect): Boolean; override;
  end;

  TdxMapPath = class(TdxMapItem)
  private
    FSegments: TdxMapPathSegments;
    FSegmentType: TdxMapSegmentType;
    procedure SegmentsChanged(ASender: TObject);
    procedure SetSegments(const Value: TdxMapPathSegments);
    procedure SetSegmentType(const Value: TdxMapSegmentType);
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
    procedure DoAssign(ASource: TdxMapItem); override;
    procedure RecreateViewInfos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Segments: TdxMapPathSegments read FSegments write SetSegments;
    property SegmentType: TdxMapSegmentType read FSegmentType write SetSegmentType default mstPolygon;
    property TitleOptions;
  end;

  TdxMapPointerViewInfo = class(TdxMapItemViewInfo)
  private
    FCachedImage: GpCachedBitmap;
    FImageRect: TRect;
    FIsCachedImageValid: Boolean;
    FRelativeImageRect: TRect;
    FRelativeTextRect: TRect;
    FRelativeBounds: TRect;
    FMapLocation: TdxPointDouble;
    function CheckCachedImageCreated(AGpCanvas: TdxGPCanvas): Boolean;
    function IsImageVisible: Boolean;
    function GetMapPointer: TdxMapPointer;
  protected
    procedure CalculateRelativeTextRect; virtual;
    procedure CalculateScreenBounds; override;
    procedure DoCalculateCachedBounds; override;
    function DoCalculateTextOffset: TPoint; virtual;
    function DoCanBeVisible: Boolean; override;
    function DoGetMapBounds: TdxRectDouble; override;
    procedure DoPaint; override;
    function GetImageOrigin: TdxPointDouble; virtual;
    function GetText: string; override;
    function HasText: Boolean; override;
    function UsePushpinImageAsDefault: Boolean; virtual;

    property RelativeImageRect: TRect read FRelativeImageRect;
    property RelativeTextRect: TRect read FRelativeTextRect;
  public
    destructor Destroy; override;
    property MapPointer: TdxMapPointer read GetMapPointer;
  end;

  TdxMapPointer = class(TdxMapSinglePointItem)
  private
    FImage: TdxSmartGlyph;
    FImageOrigin: TdxPointDoublePersistent;
    FImageVisible: Boolean;
    FText: string;
    procedure ImageChanged(ASender: TObject);
    procedure ImageOriginChanged(ASender: TObject);
    procedure SetImage(AValue: TdxSmartGlyph);
    procedure SetImageOrigin(Value: TdxPointDoublePersistent);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    function IsImageOriginStored: Boolean;
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateViewInfo: TdxMapItemViewInfo; override;
    procedure DoAssign(ASource: TdxMapItem); override;

    property Image: TdxSmartGlyph read FImage write SetImage;
    property ImageOrigin: TdxPointDoublePersistent read FImageOrigin write SetImageOrigin stored IsImageOriginStored;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default True;
    property Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsPointer: Boolean; override;
  end;

  TdxMapPushpinViewInfo = class(TdxMapPointerViewInfo)
  protected
    function DoCalculateTextOffset: TPoint; override;
    function GetTextColor(AState: TdxMapControlElementState): TdxAlphaColor; override;
    function GetTextGlowColor(AState: TdxMapControlElementState): TdxAlphaColor; override;
  end;

  TdxMapPushpin = class(TdxMapPointer)
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
  published
    property Location;
    property Text;
  end;

  TdxMapCustomElementViewInfo = class(TdxMapPointerViewInfo)
  protected
    procedure DoCalculateCachedBounds; override;
    procedure DoPaint; override;
  end;

  TdxMapCustomElement = class(TdxMapPointer)
  protected
    function CreateViewInfo: TdxMapItemViewInfo; override;
  published
    property Image;
    property ImageOrigin;
    property ImageVisible;
    property Location;
    property Text;
  end;

  TdxMapItemsZoomHelper = class
  public
    class function CalculateMapBounds(AItems: TdxMapItemList; out ARect: TdxRectDouble): Boolean;
  end;

implementation

uses
  dxCustomMapItemLayer, dxMapControlViewInfo, dxMapControl, dxSmartImage, dxDPIAwareUtils;

type
  TdxCustomMapControlAccess = class(TdxCustomMapControl);
  TdxMapItemStyleAccess = class(TdxMapItemStyle);

const
  dxMapPointerDefaultImageOrigin: TdxPointDouble = (X: 0.5; Y: 0.5);
  dxMaxGeometrySize = 5000;

function GetMapControl(AMapControl: TComponent): TdxCustomMapControlAccess;
begin
  Result := TdxCustomMapControlAccess(AMapControl);
end;

procedure CheckBoundingBox(var ABounds: TRect; const APoint: TPoint); inline;
begin
  ABounds.Left := Min(ABounds.Left, APoint.X);
  ABounds.Top := Min(ABounds.Top, APoint.Y);
  ABounds.Right := Max(ABounds.Right, APoint.X);
  ABounds.Bottom := Max(ABounds.Bottom, APoint.Y);
end;

procedure GeoPointsToMapPoints(ALayer: TdxMapLayer; AGeoPoints: TdxMapControlGeoPointCollection;
  var AMapPoints: TdxDoublePoints; var AMapBoundingBox: TdxRectDouble; AEnshureClosed: Boolean);
var
  I: Integer;
  ALeft, ATop, ARigth, ABottom: Double;
begin
  ALeft := 1;
  ATop := 1;
  ARigth := 0;
  ABottom := 0;
  SetLength(AMapPoints, AGeoPoints.Count);
  if Length(AMapPoints) < 2 then
    SetLength(AMapPoints, 0);
  for I := 0 to High(AMapPoints) do
  begin
    AMapPoints[I] := ALayer.GeoPointToMapUnit(AGeoPoints[I].GeoPoint);
    ALeft := Min(ALeft, AMapPoints[I].X);
    ATop := Min(ATop, AMapPoints[I].Y);
    ARigth := Max(ARigth, AMapPoints[I].X);
    ABottom := Max(ABottom, AMapPoints[I].Y);
  end;
  AMapBoundingBox := dxRectDouble(ALeft, ATop, ARigth - ALeft, ABottom - ATop);

  if AEnshureClosed and (Length(AMapPoints) <> 0) then
  begin
    SetLength(AMapPoints, AGeoPoints.Count + 1);
    AMapPoints[High(AMapPoints)] := AMapPoints[0];
  end;
end;

function dxPointsToDoublePoints(APoints: TPoints): TdxDoublePoints;
var
  I: Integer;
begin
  SetLength(Result, Length(APoints));
  for I := 0 to High(APoints) do
    Result[I] := dxPointDouble(APoints[I]);
end;

procedure CalculateVisiblePolygons(const AScreenPoints: TPoints;
  const AClipRect: TRect; var AVisiblePolygons: TdxPointsArray);
var
  ASubjectPolygon, AClipPolygon: TdxDoublePoints;
  APolygons: TdxDoublePointsArray;
  I, J: Integer;
begin
  ASubjectPolygon := dxPointsToDoublePoints(AScreenPoints);
  SetLength(AClipPolygon, 5);
  AClipPolygon[0] := dxPointDouble(AClipRect.TopLeft);
  AClipPolygon[1] := dxPointDouble(AClipRect.Right, AClipRect.Top);
  AClipPolygon[2] := dxPointDouble(AClipRect.BottomRight);
  AClipPolygon[3] := dxPointDouble(AClipRect.Left, AClipRect.Bottom);
  AClipPolygon[4] := AClipPolygon[0];
  dxClipPolygon(ASubjectPolygon, AClipPolygon, APolygons);
  SetLength(AVisiblePolygons, Length(APolygons));
  for I := 0 to High(APolygons) do
  begin
    SetLength(AVisiblePolygons[I], Length(APolygons[I]));
    for J := 0 to High(APolygons[I]) do
    begin
      AVisiblePolygons[I, J].X := Round((APolygons[I, J]).X);
      AVisiblePolygons[I, J].Y := Round((APolygons[I, J]).Y);
    end;
  end;
end;

function CheckIntersectionsNeeded(AGeometryItemBounds: TRect): Boolean;
begin
  Result := (cxRectWidth(AGeometryItemBounds) > dxMaxGeometrySize) or
    (cxRectHeight(AGeometryItemBounds) > dxMaxGeometrySize);
end;

{ TdxMapPathSegmentPolylineViewInfo }

constructor TdxMapPathSegmentPolylineViewInfo.Create(AOwner: TdxMapPathSegment);
begin
  inherited Create(AOwner.Collection.Owner as TdxMapItem);
  FSegment := AOwner;
end;

function TdxMapPathSegmentPolylineViewInfo.GetGeoPoints: TdxMapControlGeoPointCollection;
begin
  Result := Segment.GeoPoints;
end;

function TdxMapPathSegmentPolylineViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtNone;
end;

function TdxMapPathSegmentPolylineViewInfo.GetStyle: TdxMapItemStyle;
begin
  Result := Item.ViewInfo.Style;
end;

function TdxMapPathSegmentPolylineViewInfo.IsOwnerVisible: Boolean;
begin
  Result := Segment.Visible;
end;

{ TdxMapPathSegment }

constructor TdxMapPathSegment.Create(Collection: TCollection);
begin
  Collection.BeginUpdate;
  try
    inherited Create(Collection);
    FGeoPoints := TdxMapControlGeoPointCollection.Create(TdxMapControlGeoPointItem);
    FGeoPoints.OnChanged := GeoPointsChanged;
    RecreateViewInfo;
    FVisible := True;
  finally
    Collection.EndUpdate;
  end;
end;

destructor TdxMapPathSegment.Destroy;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FGeoPoints);
  inherited;
end;

procedure TdxMapPathSegment.Assign(Source: TPersistent);
begin
  if Source is TdxMapPathSegment then
  begin
    Collection.BeginUpdate;
    try
      GeoPoints := TdxMapPathSegment(Source).GeoPoints;
      Visible := TdxMapPathSegment(Source).Visible;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapPathSegment.CreateViewInfo: TdxMapCustomMultiPointItemViewInfo;
begin
  case (Collection.Owner as TdxMapPath).SegmentType of
    mstPolygon:
      Result := TdxMapPathSegmentPolygonViewInfo.Create(Self)
  else // mstPolyline
    Result := TdxMapPathSegmentPolylineViewInfo.Create(Self);
  end;
end;

procedure TdxMapPathSegment.GeoPointsChanged(Sender: TObject);
begin
  ViewInfo.FIsCachedBoundsValid := False;
  Changed(False);
end;

procedure TdxMapPathSegment.RecreateViewInfo;
begin
  FreeAndNil(FViewInfo);
  FViewInfo := CreateViewInfo;
end;

procedure TdxMapPathSegment.SetGeoPoints(const Value: TdxMapControlGeoPointCollection);
begin
  FGeoPoints.Assign(Value);
end;

procedure TdxMapPathSegment.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

{ TdxMapPathSegments }

function TdxMapPathSegments.Add: TdxMapPathSegment;
begin
  Result := inherited Add as TdxMapPathSegment;
end;

procedure TdxMapPathSegments.RecreateViewInfos;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RecreateViewInfo;
end;

procedure TdxMapPathSegments.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TdxMapPathSegments.GetItem(Index: Integer): TdxMapPathSegment;
begin
  Result := inherited Items[Index] as TdxMapPathSegment;
end;

procedure TdxMapPathSegments.SetItem(Index: Integer; const Value: TdxMapPathSegment);
begin
  inherited Items[Index] := Value;
end;

{ TdxMapPathCustomViewInfo<T> }

procedure TdxMapPathCustomViewInfo<T>.CalculateBounds;
var
  I: Integer;
  ABorderWidth: Integer;
begin
  Bounds := cxInvalidRect;
  if Count > 0 then
  begin
    SegmentInfo[0].CalculateBounds;
    Bounds := SegmentInfo[0].Bounds;
    for I := 1 to Count - 1 do
    begin
      SegmentInfo[I].CalculateBounds;
      Bounds := cxRectUnion(Bounds, SegmentInfo[I].Bounds);
    end;
    ABorderWidth := ScaleFactor.Apply(Style.BorderWidth);
    Bounds := cxRectInflate(Bounds, ABorderWidth, ABorderWidth);
  end;
  CalculateTitleBounds;
end;

procedure TdxMapPathCustomViewInfo<T>.AddVisibleElements;
var
  I: Integer;
begin
  for I := 0 to MapPath.Segments.Count - 1 do
    if MapPath.Segments[I].ViewInfo.CanBeVisible then
      Add(MapPath.Segments[I].ViewInfo);
end;

function TdxMapPathCustomViewInfo<T>.GetMapBounds: TdxRectDouble;
var
  I: Integer;
  AItem: TdxMapCustomMultiPointItemViewInfo;
begin
  Result := dxNullRectDouble;
  for I := 0 to MapPath.Segments.Count - 1 do
    if MapPath.Segments[I].Visible then
    begin
      AItem := MapPath.Segments[I].ViewInfo as TdxMapCustomMultiPointItemViewInfo;
      if Result.IsEmpty then
        Result := AItem.GetMapBounds
      else
        Result.Union(AItem.GetMapBounds);
    end;
end;

procedure TdxMapPathCustomViewInfo<T>.ClearCache;
var
  I: Integer;
begin
  for I := 0 to MapPath.Segments.Count - 1 do
    MapPath.Segments[I].ViewInfo.ClearCache;
end;

function TdxMapPathCustomViewInfo<T>.GetMapPath: TdxMapPath;
begin
  Result := Item as TdxMapPath;
end;

function TdxMapPathCustomViewInfo<T>.GetSegmentInfo(Index: Integer): T;
begin
  Result := T(Items[Index]);
end;

{ TdxMapPathSegmentViewInfo }

constructor TdxMapPathSegmentPolygonViewInfo.Create(
  AOwner: TdxMapPathSegment);
begin
  inherited Create(AOwner.Collection.Owner as TdxMapItem);
  FSegment := AOwner;
end;

function TdxMapPathSegmentPolygonViewInfo.HasTitle: Boolean;
begin
  Result := False;
end;

function TdxMapPathSegmentPolygonViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtNone;
end;

function TdxMapPathSegmentPolygonViewInfo.GetStyle: TdxMapItemStyle;
begin
  Result := Item.ViewInfo.Style;
end;

function TdxMapPathSegmentPolygonViewInfo.GetTitle: TdxMapItemTextViewInfo;
begin
  Result := Item.ViewInfo.Title;
end;

function TdxMapPathSegmentPolygonViewInfo.IsOwnerVisible: Boolean;
begin
  Result := Segment.Visible;
end;

function TdxMapPathSegmentPolygonViewInfo.GetGeoPoints: TdxMapControlGeoPointCollection;
begin
  Result := Segment.GeoPoints;
end;

{ TdxMapPolygon }

function TdxMapPolygon.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapPolygonViewInfo.Create(Self);
end;

{ TdxMapPath }

constructor TdxMapPath.Create(AOwner: TComponent);
begin
  inherited;
  FSegments := TdxMapPathSegments.Create(Self, TdxMapPathSegment);
  FSegments.OnChanged := SegmentsChanged;
end;

destructor TdxMapPath.Destroy;
begin
  FreeAndNil(FSegments);
  inherited;
end;

function TdxMapPath.CreateViewInfo: TdxMapItemViewInfo;
begin
  case FSegmentType of
    mstPolygon:
      Result := TdxMapPathPolygonViewInfo.Create(Self)
  else // mstPolyline
    Result := TdxMapPathPolylineViewInfo.Create(Self)
  end;
end;

procedure TdxMapPath.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapPath then
    Segments := TdxMapPath(ASource).Segments;
end;

procedure TdxMapPath.RecreateViewInfos;
begin
  FreeAndNil(FViewInfo);
  FViewInfo := CreateViewInfo;
  Segments.RecreateViewInfos;
end;

procedure TdxMapPath.SegmentsChanged(ASender: TObject);
begin
  Changed(False);
end;

procedure TdxMapPath.SetSegments(const Value: TdxMapPathSegments);
begin
  FSegments.Assign(Value);
end;

procedure TdxMapPath.SetSegmentType(const Value: TdxMapSegmentType);
begin
  if FSegmentType <> Value then
  begin
    FSegmentType := Value;
    RecreateViewInfos;
    Changed(False);
  end;
end;

{ TdxMapPathViewInfo }

function TdxMapPathPolygonViewInfo.IsIntersect(const ARect: TRect): Boolean;
var
  AGpCanvas: TdxGPGraphics;
  APath: TdxGPPath;
begin
  Result := inherited IsIntersect(ARect);
  if Result then
  begin
    APath := CreatePath;
    try
      if APath <> nil then
      begin
        AGpCanvas := TdxGPGraphics.Create(cxScreenCanvas.Handle);
        try
          AGpCanvas.SetClipPath(APath, gmReplace);
          Result := dxGpIsRectVisible(AGpCanvas.Handle, ARect);
        finally
          cxScreenCanvas.Dormant;
          AGpCanvas.Free;
        end;
      end;
    finally
      APath.Free;
    end;
  end;
end;

function TdxMapPathPolygonViewInfo.IsTitleOutOfBounds: Boolean;
begin
  if FTitleSegmentViewInfo <> nil then
    Result := FTitleSegmentViewInfo.IsTitleOutOfBounds
  else
    Result := False;
end;

procedure TdxMapPathPolygonViewInfo.AddVisibleElements;
var
  I: Integer;
  AVisible: Boolean;
  AMaxArea: Double;
  AViewInfo: TdxMapPathSegmentPolygonViewInfo;
begin
  FTitleSegmentViewInfo := nil;
  AMaxArea := 0;
  for I := 0 to MapPath.Segments.Count - 1 do
  begin
    AViewInfo := MapPath.Segments[I].ViewInfo as TdxMapPathSegmentPolygonViewInfo;
    AVisible := AViewInfo.CanBeVisible;
    if AViewInfo.FMapArea > AMaxArea then
    begin
      AMaxArea := AViewInfo.FMapArea;
      if AVisible then
        FTitleSegmentViewInfo := AViewInfo
      else
        FTitleSegmentViewInfo := nil;
    end;
    if AVisible then
      Add(MapPath.Segments[I].ViewInfo);
  end;
end;

function TdxMapPathPolygonViewInfo.CreatePath: TdxGpPath;
var
  I: Integer;
begin
  Result := nil;
  if Count > 0 then
  begin
    Result := TdxGPPath.Create;
    for I := 0 to Count - 1 do
      SegmentInfo[I].AddToPath(Result);
  end;
end;

function TdxMapPathPolygonViewInfo.DoCalculateTitleBounds: TRect;
begin
  if FTitleSegmentViewInfo <> nil then
    Result := FTitleSegmentViewInfo.DoCalculateTitleBounds
  else
    Result := cxEmptyRect;
end;

procedure TdxMapPathPolygonViewInfo.DoPaint;
var
  APath: TdxGPPath;
  AStyle: TdxMapItemStyle;
begin
  APath := CreatePath;
  try
    if APath <> nil then
    begin
      AStyle := Style;
      dxGPPaintCanvas.Path(APath, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth));
    end;
  finally
    APath.Free;
  end;
end;

function TdxMapPathPolygonViewInfo.PtInElement(const APoint: TPoint): Boolean;
var
  APath: TdxGPPath;
begin
  Result := inherited PtInElement(APoint);
  if Result then
  begin
    APath := CreatePath;
    try
      if APath <> nil then
        Result := APath.IsPointInPath(APoint);
    finally
      APath.Free;
    end;
  end;
end;

{ TdxMapPolygonViewInfo }

function TdxMapPolygonViewInfo.CreatePath: TdxGpPath;
begin
  Result := nil;
  if FCheckIntersections and (Length(FVisiblePolygons) > 0) or
    not FCheckIntersections and (Length(FScreenPoints) > 0) then
  begin
    Result := TdxGPPath.Create;
    AddToPath(Result);
  end;
end;

procedure TdxMapPolygonViewInfo.DoPaint;
var
  APath: TdxGPPath;
  AStyle: TdxMapItemStyle;
begin
  APath := CreatePath;
  try
    if APath <> nil then
    begin
      AStyle := Style;
      dxGPPaintCanvas.Path(APath, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth));
    end;
  finally
    APath.Free;
  end;
end;

function TdxMapPolygonViewInfo.GetGeoPoints: TdxMapControlGeoPointCollection;
begin
  Result := (Item as TdxMapCustomMultiPointItem).GeoPoints;
end;

function TdxMapPolygonViewInfo.InternalIsIntersect(const ARect: TRect): Boolean;
var
  AGpCanvas: TdxGPGraphics;
  APath: TdxGPPath;
begin
  Result := False;
  APath := CreatePath;
  try
    if APath <> nil then
    begin
      AGpCanvas := TdxGPGraphics.Create(cxScreenCanvas.Handle);
      try
        AGpCanvas.SetClipPath(APath, gmReplace);
        Result := dxGpIsRectVisible(AGpCanvas.Handle, ARect);
      finally
        cxScreenCanvas.Dormant;
        AGpCanvas.Free;
      end;
    end;
  finally
    APath.Free;
  end;
end;

function TdxMapPolygonViewInfo.InternalPtInElement(const APoint: TPoint): Boolean;
var
  APath: TdxGPPath;
begin
  Result := False;
  APath := CreatePath;
  try
    if APath <> nil then
      Result := APath.IsPointInPath(APoint);
  finally
    FreeAndNil(APath);
  end;
end;

function TdxMapPolygonViewInfo.IsIntersect(const ARect: TRect): Boolean;
begin
  Result := inherited IsIntersect(ARect) and InternalIsIntersect(ARect);
end;

function TdxMapPolygonViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := inherited PtInElement(APoint) and
    InternalPtInElement(APoint);
end;

{ TdxMapPointerViewInfo }

destructor TdxMapPointerViewInfo.Destroy;
begin
  if FCachedImage <> nil then
    GdipDeleteCachedBitmap(FCachedImage);
  inherited;
end;

procedure TdxMapPointerViewInfo.CalculateRelativeTextRect;
begin
  FRelativeTextRect := cxRectSetOrigin(TextInfo.ClientBounds, DoCalculateTextOffset);
end;

procedure TdxMapPointerViewInfo.CalculateScreenBounds;
var
  ALocation: TPoint;
begin
  ALocation := dxPointDoubleToPoint(MapPointer.Layer.MapUnitToScreenPoint(FMapLocation));
  FImageRect := cxRectOffset(FRelativeImageRect, ALocation);
  TextInfo.Bounds := cxRectOffset(FRelativeTextRect, ALocation);
  Bounds := cxRectOffset(FRelativeBounds, ALocation);
end;

procedure TdxMapPointerViewInfo.DoCalculateCachedBounds;
begin
  FMapLocation := MapPointer.Layer.GeoPointToMapUnit(MapPointer.Location.GeoPoint);
  FRelativeImageRect := cxNullRect;
  if IsImageVisible then
  begin
    if IsImageAssigned(MapPointer.Image) then
      FRelativeImageRect := ScaleFactor.Apply(MapPointer.Image.ClientRect)
    else
      FRelativeImageRect := cxRect(LookAndFeelPainter.MapControlGetMapPushpinSize(ScaleFactor));
    FRelativeImageRect := cxRectOffset(FRelativeImageRect, dxPointDoubleToPoint(
      dxPointDoubleScale(GetImageOrigin, dxPointDouble(cxRectWidth(FRelativeImageRect), cxRectHeight(FRelativeImageRect)))), False);
  end;
  TextInfo.CalculateSize;
  CalculateRelativeTextRect;
  FRelativeBounds := cxRectUnion(FRelativeImageRect, FRelativeTextRect);
end;

function TdxMapPointerViewInfo.DoCalculateTextOffset: TPoint;
begin
  Result := cxPoint(RelativeImageRect.Right, RelativeImageRect.Top);
end;

function TdxMapPointerViewInfo.DoCanBeVisible: Boolean;
begin
  Result := (cxRectWidth(Bounds) > 0) and (cxRectHeight(Bounds) > 0) and
    cxRectIntersect(ViewPortBounds, Bounds);
end;

function TdxMapPointerViewInfo.DoGetMapBounds: TdxRectDouble;
begin
  Result := dxRectDouble(FMapLocation, FMapLocation);
end;

procedure TdxMapPointerViewInfo.DoPaint;
begin
  if IsImageVisible then
    if IsImageAssigned(MapPointer.Image) then
    begin
      if CheckCachedImageCreated(dxGPPaintCanvas) then
        GdipDrawCachedBitmap(dxGPPaintCanvas.Handle, FCachedImage, FImageRect.Left, FImageRect.Top)
      else
        dxGPPaintCanvas.Draw(MapPointer.Image, FImageRect, $FF);
    end
    else
    begin
      CapturePaintCanvas;
      try
        LookAndFeelPainter.DrawMapPushpin(cxPaintCanvas, FImageRect, mcesNormal, ScaleFactor);
      finally
        ReleasePaintCanvas;
      end;
    end;
    TextInfo.DoPaint;
end;

function TdxMapPointerViewInfo.GetImageOrigin: TdxPointDouble;
begin
  if IsImageAssigned(MapPointer.Image) then
    Result := MapPointer.ImageOrigin.Point
  else
    Result := dxPointDouble(0.5, 1);
end;

function TdxMapPointerViewInfo.GetText: string;
begin
  Result := MapPointer.Text;
end;

function TdxMapPointerViewInfo.HasText: Boolean;
begin
  Result := True;
end;

function TdxMapPointerViewInfo.UsePushpinImageAsDefault: Boolean;
begin
  Result := MapPointer.ImageVisible;
end;

function TdxMapPointerViewInfo.CheckCachedImageCreated(AGpCanvas: TdxGPCanvas): Boolean;

  function CheckRecreated: Boolean;
  var
    AImage: TdxSmartImage;
  begin
    if FCachedImage <> nil then
      GdipDeleteCachedBitmap(FCachedImage);
    AImage := TdxSmartImage.Create;
    try
      AImage.Assign(MapPointer.Image);
      AImage.Scale(ScaleFactor.Numerator, ScaleFactor.Denominator);
      Result := GdipCreateCachedBitmap(AImage.Handle, AGpCanvas.Handle, FCachedImage) = Ok;
      FIsCachedImageValid := True;
    finally
      AImage.Free;
    end;
  end;

begin
  Result := IsWinSevenOrLater and not Supports(MapPointer.Image, IdxVectorImage) and
    (FIsCachedImageValid or CheckRecreated);
end;

function TdxMapPointerViewInfo.GetMapPointer: TdxMapPointer;
begin
  Result := Item as TdxMapPointer;
end;

function TdxMapPointerViewInfo.IsImageVisible: Boolean;
begin
  Result := MapPointer.ImageVisible;
end;

{ TdxMapPointer }

constructor TdxMapPointer.Create(AOwner: TComponent);
begin
  inherited;
  FImage := TdxSmartGlyph.Create;
  FImage.OnChange := ImageChanged;
  FImageVisible := True;
  FImageOrigin := TdxPointDoublePersistent.Create(Self, 0.5, 0.5);
  FImageOrigin.OnChange := ImageOriginChanged;
end;

destructor TdxMapPointer.Destroy;
begin
  FreeAndNil(FImageOrigin);
  FreeAndNil(FImage);
  inherited;
end;

function TdxMapPointer.IsPointer: Boolean;
begin
  Result := True;
end;

function TdxMapPointer.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapPointerViewInfo.Create(Self);
end;

procedure TdxMapPointer.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapPointer then
  begin
    Image := TdxMapPointer(ASource).Image;
    Text := TdxMapPointer(ASource).Text;
    ImageOrigin := TdxMapPointer(ASource).ImageOrigin;
    ImageVisible := TdxMapPointer(ASource).ImageVisible;
  end;
end;

procedure TdxMapPointer.ImageChanged(ASender: TObject);
begin
  ViewInfo.FIsCachedBoundsValid := False;
  (ViewInfo as TdxMapPointerViewInfo).FIsCachedImageValid := False;
  Changed(False);
end;

procedure TdxMapPointer.ChangeScale(M, D: Integer);
begin
  inherited;
  ViewInfo.FIsCachedBoundsValid := False;
  (ViewInfo as TdxMapPointerViewInfo).FIsCachedImageValid := False;
end;

procedure TdxMapPointer.ImageOriginChanged(ASender: TObject);
begin
  ViewInfo.FIsCachedBoundsValid := False;
  Changed(False);
end;

function TdxMapPointer.IsImageOriginStored: Boolean;
begin
  Result := not FImageOrigin.IsEqual(dxMapPointerDefaultImageOrigin);
end;

procedure TdxMapPointer.SetImage(AValue: TdxSmartGlyph);
begin
  FImage.Assign(AValue);
end;

procedure TdxMapPointer.SetImageOrigin(Value: TdxPointDoublePersistent);
begin
  FImageOrigin.Assign(Value);
end;

procedure TdxMapPointer.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    ViewInfo.FIsCachedBoundsValid := False;
    Changed(False);
  end;
end;

procedure TdxMapPointer.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    ViewInfo.FIsCachedBoundsValid := False;
    Changed(False);
  end;
end;

{ TdxMapPolyline }

function TdxMapPolyline.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapPolylineViewInfo.Create(Self);
end;

{ TdxMapPolylineViewInfo }

function TdxMapPolylineViewInfo.HasTitle: Boolean;
begin
  Result := False;
end;

function TdxMapPolylineViewInfo.IsIntersect(const ARect: TRect): Boolean;
begin
  Result := inherited IsIntersect(ARect) and InternalIsIntersect(ARect);
end;

procedure TdxMapPolylineViewInfo.CalculateScreenBounds;
var
  I, ACount: Integer;
  ABounds: TRect;
  AIsOffsetOnly: Boolean;
  APoint: TPoint;
begin
  ABounds := cxRect(MaxInt, MaxInt, MinInt, MinInt);
  AIsOffsetOnly := dxPointDoubleIsEqual(FCachedRenderScale, Item.Layer.RenderScale);
  if AIsOffsetOnly then
    for I := 0 to High(FScreenPoints) do
    begin
      FScreenPoints[I] := dxPointDoubleToPoint(dxPointDoubleOffset(dxPointDouble(FScreenPoints[I]),
        dxPointDoubleOffset(FCachedRenderOffset, Item.Layer.RenderOffset, False)));
      CheckBoundingBox(ABounds, FScreenPoints[I]);
    end
  else
  begin
    ACount := 0;
    SetLength(FScreenPoints, Length(FMapPoints));
    for I := 0 to High(FScreenPoints) do
    begin
      APoint := dxPointDoubleToPoint(Item.Layer.MapUnitToScreenPoint(FMapPoints[I]));
      if (ACount = 0) or not cxPointIsEqual(APoint, FScreenPoints[ACount - 1]) then
      begin
        FScreenPoints[ACount] := APoint;
        CheckBoundingBox(ABounds, FScreenPoints[ACount]);
        Inc(ACount);
      end;
    end;
    if ACount = 1 then
      ACount := 0;
    SetLength(FScreenPoints, ACount);
  end;
  Bounds := ABounds;
  FCheckIntersections := CheckIntersectionsNeeded(Bounds);
  if FCheckIntersections then
  begin
    SetLength(FVisiblePolylines, 0);
    dxClipPolylineByRect(FScreenPoints, cxRectInflate(ViewPortBounds, ScaleFactor.Apply(Style.BorderWidth)), FVisiblePolylines);
  end;
end;

function TdxMapPolylineViewInfo.CreatePath: TdxGpPath;
var
  I: Integer;
begin
  Result := nil;
  if FCheckIntersections then
  begin
    if Length(FVisiblePolylines) > 0 then
      Result := TdxGPPath.Create;
    for I := 0 to High(FVisiblePolylines) do
      Result.AddPolyline(FVisiblePolylines[I]);
  end
  else
    if Length(FScreenPoints) > 0 then
    begin
      Result := TdxGPPath.Create;
      Result.AddPolyline(FScreenPoints);
    end;
end;

procedure TdxMapPolylineViewInfo.DoCalculateCachedBounds;
begin
  GeoPointsToMapPoints(Item.Layer, GetGeoPoints, FMapPoints, FMapBounds, False);
  SetLength(FScreenPoints, Length(FMapPoints));
  inherited DoCalculateCachedBounds;
end;

function TdxMapPolylineViewInfo.DoCanBeVisible: Boolean;

  function Intersect(const ARect1, ARect2: TdxRectDouble): Boolean;
  var
    ALeft, ATop, ARight, ABottom: Double;
  begin
    ALeft := Max(ARect2.Left, ARect1.Left);
    ATop := Max(ARect2.Top, ARect1.Top);
    ARight := Min(ARect2.Left + ARect2.Width, ARect1.Left + ARect1.Width);
    ABottom := Min(ARect2.Top + ARect2.Height, ARect1.Top + ARect1.Height);
    Result := not ((ARight < ALeft) or (ABottom < ATop));
  end;

begin
  Result := (Length(FMapPoints) > 0) and
    Intersect(Item.Layer.ViewPort, FMapBounds);
end;

procedure TdxMapPolylineViewInfo.DoPaint;
var
  I: Integer;
  K413D87FBF674C21A84BB8DFFD124B00: TdxMapItemStyle;
begin
  K413D87FBF674C21A84BB8DFFD124B00 := Style;
  if FCheckIntersections then
    for I := 0 to High(FVisiblePolylines) do
      dxGPPaintCanvas.Polyline(FVisiblePolylines[I], K413D87FBF674C21A84BB8DFFD124B00.BorderColor, ScaleFactor.Apply(K413D87FBF674C21A84BB8DFFD124B00.BorderWidth))
  else
    if Length(FScreenPoints) > 0 then
      dxGPPaintCanvas.Polyline(FScreenPoints, K413D87FBF674C21A84BB8DFFD124B00.BorderColor, ScaleFactor.Apply(K413D87FBF674C21A84BB8DFFD124B00.BorderWidth));
end;

function TdxMapPolylineViewInfo.GetGeoPoints: TdxMapControlGeoPointCollection;
begin
  Result := (Item as TdxMapCustomMultiPointItem).GeoPoints;
end;

function TdxMapPolylineViewInfo.InternalIsIntersect(const ARect: TRect): Boolean;
var
  AVisiblePolylines: TdxPointsArray;
  I: Integer;
begin
  SetLength(AVisiblePolylines, 0);
  Result := False;
  if FCheckIntersections then
    for I := 0 to High(FVisiblePolylines) do
    begin
      dxClipPolylineByRect(FVisiblePolylines[I], ARect, AVisiblePolylines);
      Result := Length(AVisiblePolylines) <> 0;
      if Result then
        Exit;
    end
  else
    if Length(FScreenPoints) > 0 then
    begin
      dxClipPolylineByRect(FScreenPoints, ARect, AVisiblePolylines);
      Result := Length(AVisiblePolylines) <> 0;
    end;
end;

function TdxMapPolylineViewInfo.InternalPtInElement(const APoint: TPoint): Boolean;
var
  APath: TdxGPPath;
begin
  Result := False;
  APath := CreatePath;
  try
    if APath <> nil then
      Result := APath.IsPointInPathOutline(APoint, ScaleFactor.Apply(Style.BorderWidth));
  finally
    FreeAndNil(APath);
  end;
end;

function TdxMapPolylineViewInfo.IsBoundsScalable: Boolean;
begin
  Result := True;
end;

function TdxMapPolylineViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := inherited PtInElement(APoint) and
    InternalPtInElement(APoint);
end;

{ TdxMapPushpinViewInfo }

function TdxMapPushpinViewInfo.DoCalculateTextOffset: TPoint;
begin
  Result := cxPointOffset(RelativeImageRect.TopLeft, LookAndFeelPainter.MapControlGetMapPushpinTextOrigin(ScaleFactor));
  Result := cxPointOffset(Result, cxRectCenter(TextInfo.ClientBounds), False);
end;

function TdxMapPushpinViewInfo.GetTextColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  Result := LookAndFeelPainter.MapControlMapPushpinTextColor;
end;

function TdxMapPushpinViewInfo.GetTextGlowColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  Result := LookAndFeelPainter.MapControlMapPushpinTextGlowColor;
end;

{ TdxMapPushpin }

function TdxMapPushpin.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapPushpinViewInfo.Create(Self);
end;

{ TdxMapCustomMultiPointItem }

constructor TdxMapCustomMultiPointItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGeoPoints := TdxMapControlGeoPointCollection.Create(TdxMapControlGeoPointItem);
  FGeoPoints.OnChanged := GeoPointsChanged;
end;

destructor TdxMapCustomMultiPointItem.Destroy;
begin
  FreeAndNil(FGeoPoints);
  inherited Destroy;
end;

procedure TdxMapCustomMultiPointItem.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapCustomMultiPointItem then
    GeoPoints := TdxMapCustomMultiPointItem(ASource).GeoPoints;
end;

procedure TdxMapCustomMultiPointItem.GeoPointsChanged(Sender: TObject);
begin
  ViewInfo.FIsCachedBoundsValid := False;
  Changed(False);
end;

procedure TdxMapCustomMultiPointItem.SetGeoPoints(const Value: TdxMapControlGeoPointCollection);
begin
  FGeoPoints.Assign(Value);
end;

{ TdxMapControlGeoPointCollection }

function TdxMapControlGeoPointCollection.Add: TdxMapControlGeoPointItem;
begin
  Result := inherited Add as TdxMapControlGeoPointItem;
end;

procedure TdxMapControlGeoPointCollection.AddRange(AGeoPoints: TdxMapControlGeoPoints);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to High(AGeoPoints) do
      Add.GeoPoint := AGeoPoints[I];
  finally
    EndUpdate;
  end;
end;

procedure TdxMapControlGeoPointCollection.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TdxMapControlGeoPointCollection.GetItem(Index: Integer): TdxMapControlGeoPointItem;
begin
  Result := TdxMapControlGeoPointItem(inherited Items[Index]);
end;

procedure TdxMapControlGeoPointCollection.SetItem(Index: Integer;
  const Value: TdxMapControlGeoPointItem);
begin
  inherited Items[Index] := Value;
end;

{ TdxMapControlGeoPointItem }

procedure TdxMapControlGeoPointItem.Assign(Source: TPersistent);
begin
  if Source is TdxMapControlGeoPointItem then
  begin
    Collection.BeginUpdate;
    try
      GeoPoint := TdxMapControlGeoPointItem(Source).GeoPoint;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapControlGeoPointItem.GetLatitude: Double;
begin
  Result := FGeoPoint.Latitude;
end;

function TdxMapControlGeoPointItem.GetLongitude: Double;
begin
  Result := FGeoPoint.Longitude;
end;

procedure TdxMapControlGeoPointItem.SetGeoPoint(const Value: TdxMapControlGeoPoint);
begin
  if not dxMapControlGeoPointIsEqual(FGeoPoint, Value) then
  begin
    FGeoPoint := Value;
    Changed(False);
  end;
end;

procedure TdxMapControlGeoPointItem.SetLatitude(const Value: Double);
begin
  if FGeoPoint.Latitude <> Value then
  begin
    FGeoPoint.Latitude := Value;
    Changed(False);
  end;
end;

procedure TdxMapControlGeoPointItem.SetLongitude(const Value: Double);
begin
  if FGeoPoint.Longitude <> Value then
  begin
    FGeoPoint.Longitude := Value;
    Changed(False);
  end;
end;

{ TdxMapCustomElementViewInfo }

procedure TdxMapCustomElementViewInfo.DoCalculateCachedBounds;
begin
  inherited DoCalculateCachedBounds;
  FRelativeBounds := cxRectInflate(FRelativeBounds, LookAndFeelPainter.MapControlMapCustomElementSelectionOffset(ScaleFactor));
end;

procedure TdxMapCustomElementViewInfo.DoPaint;
begin
  CapturePaintCanvas;
  try
    LookAndFeelPainter.DrawMapCustomElementBackground(cxPaintCanvas, Bounds, State);
  finally
    ReleasePaintCanvas;
  end;
  inherited DoPaint;
end;

{ TdxMapCustomElement }

function TdxMapCustomElement.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapCustomElementViewInfo.Create(Self);
end;

{ TdxMapControlMapEllipse }

function TdxMapEllipse.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapEllipseViewInfo.Create(Self);
end;

{ TdxMapControlMapEllipseViewInfo }

procedure TdxMapEllipseViewInfo.DoPaint;
var
  AStyle: TdxMapItemStyle;
begin
  AStyle := Style;
  dxGPPaintCanvas.Ellipse(Bounds, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth));
end;

function TdxMapEllipseViewInfo.PtInElement(const APoint: TPoint): Boolean;
var
  ACenter: TPoint;
begin
  Result := inherited PtInElement(APoint);
  if Result then
  begin
    ACenter := cxRectCenter(Bounds);
    Result := Sqr(APoint.X - ACenter.X) / Sqr(cxRectWidth(Bounds) / 2) +
      Sqr(APoint.Y - ACenter.Y) / Sqr(cxRectHeight(Bounds) / 2) <= 1;
  end;
end;

{ TdxMapItemStylesHelper }

constructor TdxMapItemStylesHelper.Create(AOwner: TPersistent);
var
  I: TdxMapControlElementState;
begin
  inherited Create;
  for I := Low(TdxMapControlElementState) to High(TdxMapControlElementState) do
    if I in GetActualStates then
    begin
      FStyles[I] := TdxMapItemStyle.Create(AOwner);
      FStyles[I].OnChanged := StyleChanged;
    end
    else
      FStyles[I] := nil;
end;

destructor TdxMapItemStylesHelper.Destroy;
var
  I: TdxMapControlElementState;
begin
  for I := Low(TdxMapControlElementState) to High(TdxMapControlElementState) do
    FreeAndNil(FStyles[I]);
end;

procedure TdxMapItemStylesHelper.EnumActualStyles(
  AStyleProc: TdxMapControlEnumElementStateProc);
var
  I: TdxMapControlElementState;
begin
  for I := Low(TdxMapControlElementState) to High(TdxMapControlElementState) do
    if I in GetActualStates then
      AStyleProc(I);
end;

function TdxMapItemStylesHelper.GetActualStates: TdxMapControlElementStates;
begin
  Result := [mcesNormal, mcesHot, mcesSelected];
end;

function TdxMapItemStylesHelper.GetMaxSizeFont: TFont;
var
  I: TdxMapControlElementState;
  AMaxSize, ASize: Integer;
begin
  if not FIsMaxFontSizeValid then
  begin
    FMaxSizeFont := nil;
    AMaxSize := 0;
    for I := Low(TdxMapControlElementState) to High(TdxMapControlElementState) do
      if FStyles[I] <> nil then
      begin
        ASize := cxTextSize(FStyles[I].Font, 'Wg').cx;
        if ASize > AMaxSize then
        begin
          AMaxSize := ASize;
          FMaxSizeFont := FStyles[I].Font;
        end;
      end;
  end;
  Result := FMaxSizeFont;
end;

function TdxMapItemStylesHelper.GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;
begin
  Result := FStyles[AState];
end;

procedure TdxMapItemStylesHelper.SetStyle(AState: TdxMapControlElementState; const Value: TdxMapItemStyle);
begin
  FStyles[AState] := Value;
end;

procedure TdxMapItemStylesHelper.StyleChanged(ASender: TObject);
begin
  FIsMaxFontSizeValid := False;
  dxCallNotify(FOnChanged, ASender);
end;

{ TdxMapControlMapRectangle }

function TdxMapRectangle.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapRectangleViewInfo.Create(Self);
end;

procedure TdxMapRectangle.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapRectangle then
  begin
    Height := TdxMapRectangle(ASource).Height;
    Width := TdxMapRectangle(ASource).Width;
  end;
end;

procedure TdxMapRectangle.SetHeight(Value: Double);
begin
  if FHeight <> Value then
  begin
    ViewInfo.FIsCachedBoundsValid := False;
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TdxMapRectangle.SetWidth(Value: Double);
begin
  if FWidth <> Value then
  begin
    ViewInfo.FIsCachedBoundsValid := False;
    FWidth := Value;
    Changed(False);
  end;
end;

{ TdxMapControlMapRectangleViewInfo }

procedure TdxMapRectangleViewInfo.CalculateScreenBounds;
var
  APoint1, APoint2: TPoint;
  ADoublePoint1, ADoublePoint2: TdxPointDouble;
begin
  ADoublePoint1 := Item.Layer.MapUnitToScreenPoint(FMapPoint1);
  ADoublePoint2 := Item.Layer.MapUnitToScreenPoint(FMapPoint2);
  APoint1 := dxPointDoubleToPoint(ADoublePoint1);
  APoint2 := dxPointDoubleToPoint(ADoublePoint2);
  FMaxTitleSize := dxSizeDouble(APoint2.X - APoint1.X, APoint2.Y - APoint1.Y);
  Bounds := cxRect(APoint1, APoint2);
  FScreenCenterPoint := cxRectCenter(Bounds);
  cxRectIntersect(FVisibleRect, cxRectInflate(ViewPortBounds, ScaleFactor.Apply(Style.BorderWidth)), Bounds);
end;

procedure TdxMapRectangleViewInfo.DoCalculateCachedBounds;
var
  AGeoPoint2: TdxMapControlGeoPoint;
  AItem: TdxMapRectangle;
  AItemGeoSize: TdxSizeDouble;
begin
  AItem := Item as TdxMapRectangle;
  FMapPoint1 := Item.Layer.GeoPointToMapUnit(AItem.Location.GeoPoint);
  AItemGeoSize := Item.Layer.KilometersToGeoSize(AItem.Location.GeoPoint, dxSizeDouble(AItem.Width, AItem.Height));
  AGeoPoint2 := dxMapControlGeoPoint(AItem.Location.Latitude - AItemGeoSize.Height, AItem.Location.Longitude + AItemGeoSize.Width);
  FMapPoint2 := Item.Layer.GeoPointToMapUnit(AGeoPoint2);
  FMapBounds := dxRectDouble(FMapPoint1.X, FMapPoint1.Y, FMapPoint2.X - FMapPoint1.X, FMapPoint2.Y - FMapPoint1.Y);
end;

function TdxMapRectangleViewInfo.DoCanBeVisible: Boolean;
begin
  Result := (FMapBounds.Width * Item.Layer.RenderScale.X > 1) and
    (FMapBounds.Height * Item.Layer.RenderScale.Y > 1) and
    dxRectDoubleIntersect(Item.Layer.ViewPort, FMapBounds);
end;

function TdxMapRectangleViewInfo.DoGetMapBounds: TdxRectDouble;
begin
  Result := FMapBounds;
end;

procedure TdxMapRectangleViewInfo.DoPaint;
var
  AStyle: TdxMapItemStyle;
begin
  AStyle := Style;
  dxGPPaintCanvas.Rectangle(FVisibleRect, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth));
end;

function TdxMapRectangleViewInfo.IsBoundsScalable: Boolean;
begin
  Result := True;
end;

{ TdxMapControlMapDot }

constructor TdxMapDot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := 5;
end;

function TdxMapDot.IsPointer: Boolean;
begin
  Result := True;
end;

procedure TdxMapDot.ChangeScale(M, D: Integer);
begin
  inherited;
  Size := MulDiv(Size, M, D);
end;

function TdxMapDot.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapDotViewInfo.Create(Self);
end;

procedure TdxMapDot.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapDot then
  begin
    ShapeKind := TdxMapDot(ASource).ShapeKind;
    Size := TdxMapDot(ASource).Size;
  end;
end;

procedure TdxMapDot.SetShapeKind(Value: TdxMapDotShapeKind);
begin
  if FShapeKind <> Value then
  begin
    FShapeKind := Value;
    Changed(False);
  end;
end;

procedure TdxMapDot.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    ViewInfo.FIsCachedBoundsValid := False;
    FSize := Value;
    Changed(False);
  end;
end;

{ TdxMapControlMapDotViewInfo }

procedure TdxMapDotViewInfo.CalculateScreenBounds;
var
  ALocation: TdxPointDouble;
begin
  ALocation := Item.Layer.MapUnitToScreenPoint(FMapLocation);
  Bounds := dxRectDoubleToRect(dxRectDoubleOffset(FRelativeBounds, ALocation));
end;

procedure TdxMapDotViewInfo.DoCalculateCachedBounds;
begin
  FMapLocation := Item.Layer.GeoPointToMapUnit(MapDot.Location.GeoPoint);
  FRadius := MapDot.Size / 2.0;
  FRelativeBounds := dxRectDouble(-FRadius, -FRadius, MapDot.Size, MapDot.Size);
end;

function TdxMapDotViewInfo.DoCanBeVisible: Boolean;
begin
  Result := (MapDot.Size >= 1) and
    cxRectIntersect(ViewPortBounds, Bounds);
end;

function TdxMapDotViewInfo.DoGetMapBounds: TdxRectDouble;
begin
  Result := dxRectDouble(FMapLocation, FMapLocation);
end;

procedure TdxMapDotViewInfo.DoPaint;
var
  AStyle: TdxMapItemStyle;
begin
  AStyle := Style;
  case MapDot.ShapeKind  of
    mcskCircle:
      dxGPPaintCanvas.Ellipse(Bounds, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth))
  else // mcskRectangle
     dxGPPaintCanvas.Rectangle(Bounds, AStyle.BorderColor, AStyle.Color, ScaleFactor.Apply(AStyle.BorderWidth));
  end;
end;

function TdxMapDotViewInfo.GetMapDot: TdxMapDot;
begin
  Result := Item as TdxMapDot;
end;

{ TdxMapCustomPolygonViewInfo }

procedure TdxMapCustomPolygonViewInfo.AddToPath(APath: TdxGPPath);
var
  I: Integer;
begin
  if FCheckIntersections then
    for I := 0 to High(FVisiblePolygons) do
      APath.AddPolygon(FVisiblePolygons[I])
  else
    APath.AddPolygon(FScreenPoints);
end;

procedure TdxMapCustomPolygonViewInfo.CalculateScreenBounds;
var
  I: Integer;
  ABounds: TRect;
  APoint: TdxPointDouble;
begin
  FScreenCenterPoint := dxPointDoubleToPoint(Item.Layer.MapUnitToScreenPoint(FMapCenterPoint));
  ABounds := cxRect(MaxInt, MaxInt, MinInt, MinInt);
  for I := 0 to High(FScreenPoints) do
  begin
    APoint := Item.Layer.MapUnitToScreenPoint(FMapPoints[I]);
    FScreenPoints[I] := dxPointDoubleToPoint(APoint);
    CheckBoundingBox(ABounds, FScreenPoints[I]);
  end;
  FMaxTitleSize := dxSizeDoubleScale(FMapBounds.Size, Item.Layer.RenderScale);
  Bounds := ABounds;
  FCheckIntersections := CheckIntersectionsNeeded(Bounds);
  if FCheckIntersections then
    CalculateVisiblePolygons(FScreenPoints, cxRectInflate(ViewPortBounds, ScaleFactor.Apply(Style.BorderWidth)), FVisiblePolygons);
end;

function GetAreaIncrement(const APoint1, APoint2: TdxPointDouble): Double; inline;
begin
  Result := APoint1.X * APoint2.Y - APoint2.X * APoint1.Y;
end;

function CalculatePolygonArea(const APoints: TdxDoublePoints): Double; inline;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(APoints) - 1 do
    Result := Result + GetAreaIncrement(APoints[I], APoints[I + 1]);
  Result := Result / 2;
end;

function GetCenterIncrement(const APoint1, APoint2: TdxPointDouble): TdxPointDouble; inline;
var
  ACommonFactor: Double;
begin
  ACommonFactor := APoint1.X * APoint2.Y - APoint2.X * APoint1.Y;
  Result.X := (APoint1.X + APoint2.X) * ACommonFactor;
  Result.Y := (APoint1.Y + APoint2.Y) * ACommonFactor;
end;

function CalculatePolygonCenter(const APoints: TdxDoublePoints; AArea: Double): TdxPointDouble; inline;
var
  AInc: TdxPointDouble;
  I: Integer;
begin
  Result := dxNullPointDouble;
  if not IsZero(AArea) then
  begin
    for I := 0 to High(APoints) - 1 do
    begin
      AInc := GetCenterIncrement(APoints[I], APoints[I + 1]);
      Result.X := Result.X + AInc.X;
      Result.Y := Result.Y + AInc.Y;
    end;
    Result.X := Result.X / (AArea * 6);
    Result.Y := Result.Y / (AArea * 6);
  end;
end;

procedure TdxMapCustomPolygonViewInfo.DoCalculateCachedBounds;
begin
  GeoPointsToMapPoints(Item.Layer, GetGeoPoints, FMapPoints, FMapBounds, True);
  FMapArea := CalculatePolygonArea(FMapPoints);
  FMapCenterPoint := CalculatePolygonCenter(FMapPoints, FMapArea);
  SetLength(FScreenPoints, Length(FMapPoints));
end;

function TdxMapCustomPolygonViewInfo.DoCanBeVisible: Boolean;
begin
  Result := (Length(FScreenPoints) > 0) and (FMapBounds.Width * Item.Layer.RenderScale.X > 1) and
    (FMapBounds.Height * Item.Layer.RenderScale.Y > 1) and
    dxRectDoubleIntersect(Item.Layer.ViewPort, FMapBounds);
end;

function TdxMapCustomPolygonViewInfo.IsBoundsScalable: Boolean;
begin
  Result := True;
end;

{ TdxMapControlMapSinglePointItem }

constructor TdxMapSinglePointItem.Create(AOwner: TComponent);
begin
  inherited;
  FLocation := TdxMapControlGeoLocation.Create(Self);
  FLocation.OnChanged := LocationChanged;
end;

destructor TdxMapSinglePointItem.Destroy;
begin
  FreeAndNil(FLocation);
  inherited Destroy;
end;

procedure TdxMapSinglePointItem.DoAssign(ASource: TdxMapItem);
begin
  inherited DoAssign(ASource);
  if ASource is TdxMapSinglePointItem then
    Location := TdxMapSinglePointItem(ASource).Location;
end;

procedure TdxMapSinglePointItem.LocationChanged(ASender: TObject);
begin
  ViewInfo.FIsCachedBoundsValid := False;
  Changed(False);
end;

procedure TdxMapSinglePointItem.SetLocation(
  Value: TdxMapControlGeoLocation);
begin
  FLocation.Assign(Value);
end;

{ TdxMapItems }

function TdxMapItems.Add(AMapItemClass: TdxMapItemClass): TdxMapItem;
begin
  Result := inherited Add(AMapItemClass) as TdxMapItem;
end;

procedure TdxMapItems.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

function TdxMapItems.GetItemPrefixName: string;
begin
  Result := 'TdxMap';
end;

procedure TdxMapItems.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  if csDesigning in ParentComponent.ComponentState then
    inherited SetItemName(AItem, ABaseIndex);
end;

function TdxMapItems.GetItem(Index: Integer): TdxMapItem;
begin
  Result := inherited GetItem(Index) as TdxMapItem;
end;

procedure TdxMapItems.SetItem(Index: Integer; const Value: TdxMapItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxMapItem }

constructor TdxMapItem.Create(AOwner: TComponent);
begin
  inherited;
  FAttributes := TdxMapItemAttributes.Create;
  FAttributes.OnChanged := AttributesChanged;
  FStyles := TdxMapItemStylesHelper.Create(Self);
  FStyles.OnChanged := StyleChanged;
  FTitleOptions := TdxMapItemTitleOptions.Create(Self);
  FTitleOptions.OnChanged := TitleOptionsChanged;
  FViewInfo := CreateViewInfo;
  FVisible := True;
end;

destructor TdxMapItem.Destroy;
begin
  GetMapControl(Layer.GetParentComponent).Deselect(Self);
  FAttributes.OnChanged := nil;
  FreeAndNil(FTitleOptions);
  FreeAndNil(FViewInfo);
  FreeAndNil(FStyles);
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

procedure TdxMapItem.SetParentComponent(Value: TComponent);
begin
  FLayer := Value as TdxMapLayer;
  inherited SetParentComponent(Value);
end;

procedure TdxMapItem.Assign(Source: TPersistent);
begin
  if Source is TdxMapItem then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(TdxMapItem(Source));
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapItem.GetHint: string;
begin
  Result := FHint;
  if Result = '' then
    Result := (Layer as TdxCustomMapItemLayer).GetItemHint(Self);
end;

function TdxMapItem.GetSelected: Boolean;
begin
  Result := ViewInfo.State = mcesSelected;
end;

procedure TdxMapItem.AttributesChanged(ASender: TObject);
begin
  FViewInfo.FIsTitleSizeValid := False;
  Changed(False);
end;

function TdxMapItem.GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;
begin
  Result := FStyles[AState];
end;

procedure TdxMapItem.InvalidateStylesInfo;
begin
  FViewInfo.FIsCachedBoundsValid := False;
  FViewInfo.FIsStylesValid := False;
end;

procedure TdxMapItem.InvalidateTitleInfo;
begin
  FViewInfo.FIsTitleSizeValid := False;
end;

function TdxMapItem.IsPointer: Boolean;
begin
  Result := False;
end;
procedure TdxMapItem.ChangeScale(M, D: Integer);
begin
  // do nothing
end;


function TdxMapItem.CreateViewInfo: TdxMapItemViewInfo;
begin
  Result := TdxMapItemViewInfo.Create(Self);
end;

procedure TdxMapItem.DoAssign(ASource: TdxMapItem);
begin
//  AllowHotTrack := TdxMapItem(Source).AllowHotTrack;
  Hint := ASource.Hint;
  ScreenTip := ASource.ScreenTip;
  Style := ASource.Style;
  StyleHot := ASource.StyleHot;
  StyleSelected := ASource.StyleSelected;
  Visible := ASource.Visible;
end;

function TdxMapItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomMapItemLayer).MapItems;
end;

function TdxMapItem.GetMapBounds: TdxRectDouble;
begin
  Result := ViewInfo.GetMapBounds;
end;

procedure TdxMapItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = FScreenTip then
      FScreenTip := nil;
  end;
end;

procedure TdxMapItem.SelectionChanged(AShift: TShiftState);
begin
  GetMapControl(Layer.GetParentComponent).Select(Self, AShift);
end;

procedure TdxMapItem.SetSelected(const Value: Boolean);
begin
  if Value then
    GetMapControl(Layer.GetParentComponent).Select(Self)
  else
    GetMapControl(Layer.GetParentComponent).Deselect(Self);
end;

procedure TdxMapItem.SetStyle(const Index: TdxMapControlElementState; const Value: TdxMapItemStyle);
begin
  FStyles[Index].Assign(Value);
end;

procedure TdxMapItem.SetTitleOptions(const Value: TdxMapItemTitleOptions);
begin
  FTitleOptions.Assign(Value);
end;

procedure TdxMapItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TdxMapItem.StyleChanged(ASender: TObject);
begin
  InvalidateStylesInfo;
  Changed(False);
end;

procedure TdxMapItem.TitleOptionsChanged(ASender: TObject);
begin
  InvalidateTitleInfo;
  Changed(False);
end;

{ TdxMapPathPolylineViewInfo }

procedure TdxMapPathPolylineViewInfo.DoPaint;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    SegmentInfo[I].DoPaint;
end;

function TdxMapPathPolylineViewInfo.HasTitle: Boolean;
begin
  Result := False;
end;

function TdxMapPathPolylineViewInfo.IsIntersect(const ARect: TRect): Boolean;
var
  I: Integer;
begin
  Result := inherited IsIntersect(ARect);
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := SegmentInfo[I].IsIntersect(ARect);
      if Result then
        Break;
    end;
end;

function TdxMapPathPolylineViewInfo.PtInElement(const APoint: TPoint): Boolean;
var
  I: Integer;
begin
  Result := inherited PtInElement(APoint);
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := SegmentInfo[I].PtInElement(APoint);
      if Result then
        Break;
    end;
end;

function TdxMapCustomMultiPointItemViewInfo.DoGetMapBounds: TdxRectDouble;
begin
  Result := FMapBounds;
end;

{ TdxMapItemTitleViewInfo }

constructor TdxMapItemTextViewInfo.Create(
  AMapItemViewInfo: TdxMapItemViewInfo);
begin
  inherited Create;
  FMapItemViewInfo := AMapItemViewInfo;
end;

destructor TdxMapItemTextViewInfo.Destroy;
begin
  FreeAndNil(FCachedText);
  inherited;
end;

function TdxMapItemTextViewInfo.DoCalculateSize: TSize;
var
  AGpCanvas: TdxGPGraphics;
  AFont: TFont;
begin
  FText := FMapItemViewInfo.GetText;
  if FText <> '' then
  begin
    AGpCanvas := TdxGPGraphics.Create(cxScreenCanvas.Handle);
    try
      AFont := FMapItemViewInfo.GetMaxSizeFont;
      AFont.Height := FMapItemViewInfo.ScaleFactor.Apply(AFont.Height);
      try
        dxGPGetTextRect(AGpCanvas, FText, AFont, False, cxNullRect, FClientBounds);
      finally
        AFont.Height := FMapItemViewInfo.ScaleFactor.Revert(AFont.Height);
      end;
      Result := cxRectSize(FClientBounds);
    finally
      cxScreenCanvas.Dormant;
      AGpCanvas.Free;
    end;
  end
  else
  begin
    Result := cxNullSize;
    FClientBounds := cxEmptyRect;
  end;
  FIsCacheTextValid := False;
end;

procedure TdxMapItemTextViewInfo.DoPaint;

  procedure UpdateCacheText;
  var
    AStyle: TdxMapItemStyle;
    AGpImageCanvas: TdxGPCanvas;
  begin
    FreeAndNil(FCachedText);
    FCachedText := TdxSmartImage.CreateSize(Size);
    AGpImageCanvas := FCachedText.CreateCanvas;
    try
      AStyle := FMapItemViewInfo.GetStyle;
      AStyle.Font.Height := FMapItemViewInfo.ScaleFactor.Apply(AStyle.Font.Height);
      try
        dxGPDrawGlowText(AGpImageCanvas, FText, FClientBounds, AStyle.Font, AStyle.TextColor, AStyle.TextGlowColor);
      finally
        AStyle.Font.Height := FMapItemViewInfo.ScaleFactor.Revert(AStyle.Font.Height);
      end;
    finally
      AGpImageCanvas.Free;
    end;
    FIsCacheTextValid := True;
    FCachedState := State;
  end;

begin
  if not cxRectIsEmpty(Bounds) then
  begin
    if not FIsCacheTextValid or (FCachedState <> State) then
      UpdateCacheText;
    dxGPPaintCanvas.Draw(FCachedText, Bounds);
  end;
end;

function TdxMapItemTextViewInfo.GetState: TdxMapControlElementState;
begin
  Result := FMapItemViewInfo.State;
end;

{ TdxMapItemTitleOptions }

constructor TdxMapItemTitleOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

procedure TdxMapItemTitleOptions.Changed;
begin
  dxCallNotify(FOnChanged, Self);
end;

procedure TdxMapItemTitleOptions.DoAssign(Source: TPersistent);
var
  ATitleOptions: TdxMapItemTitleOptions;
begin
  inherited DoAssign(Source);
  if Source is TdxMapItemTitleOptions then
  begin
    ATitleOptions := TdxMapItemTitleOptions(Source);
    Text := ATitleOptions.Text;
    Visibility := ATitleOptions.Visibility;
  end;
end;

procedure TdxMapItemTitleOptions.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TdxMapItemTitleOptions.SetVisibility(
  const Value: TdxMapItemTitleVisibility);
begin
  if FVisibility <> Value then
  begin
    FVisibility := Value;
    Changed;
  end;
end;

{ TdxMapItemAttributes }

constructor TdxMapItemAttributes.Create;
begin
  inherited Create;
  FItems := TDictionary<string, Variant>.Create;
  FItems.OnValueNotify := Changed;
end;

destructor TdxMapItemAttributes.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxMapItemAttributes.Add(const AName: string; AValue: Variant);
begin
  FItems.AddOrSetValue(AName, AValue);
end;

function TdxMapItemAttributes.ContainsName(const AName: string): Boolean;
begin
  Result := FItems.ContainsKey(AName);
end;

procedure TdxMapItemAttributes.GetNames(ANames: TStrings);
var
  AName: string;
begin
  for AName in FItems.Keys do
    ANames.Add(AName);
end;

procedure TdxMapItemAttributes.Remove(const AName: string);
begin
  FItems.Remove(AName);
end;

procedure TdxMapItemAttributes.Changed(Sender: TObject; const Item: Variant;
  Action: TCollectionNotification);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TdxMapItemAttributes.GetValues(const AName: string): Variant;
begin
  Result := FItems[AName];
end;

procedure TdxMapItemAttributes.SetValues(const AName: string;
  const Value: Variant);
begin
  FItems[AName] := Value;
end;

{ TdxMapItemViewInfo }

constructor TdxMapItemViewInfo.Create(AOwner: TdxMapItem);
begin
  inherited Create;
  FItem := AOwner;
  ResetRenderCache;
  FStyles := TdxMapItemStylesHelper.Create(nil);
  if HasText then
    FTextInfo := TdxMapItemTextViewInfo.Create(Self);
end;

destructor TdxMapItemViewInfo.Destroy;
begin
  FreeAndNil(FTextInfo);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

procedure TdxMapItemViewInfo.CalculateBounds;
var
  ABorderWidth: Integer;
begin
  if not FIsCachedBoundsValid then
    CalculateCachedBounds;
  if not dxPointDoubleIsEqual(FCachedRenderScale, Item.Layer.RenderScale) or
    not dxPointDoubleIsEqual(FCachedRenderOffset, Item.Layer.RenderOffset) then
  begin
    CalculateScreenBounds;
    ABorderWidth := ScaleFactor.Apply(Style.BorderWidth);
    Bounds := cxRectInflate(Bounds, ABorderWidth, ABorderWidth);
    FCachedRenderScale := Item.Layer.RenderScale;
    FCachedRenderOffset := Item.Layer.RenderOffset;
  end;
  CalculateTitleBounds;
end;

function TdxMapItemViewInfo.CanBeVisible: Boolean;
begin
  Result := IsOwnerVisible;
  if Result then
  begin
    if NeedCalculateScreenBoundsToDetermineVisibility then
      CalculateBounds
    else
      if not FIsCachedBoundsValid then
        CalculateCachedBounds;
    Result := DoCanBeVisible;
  end;
end;

procedure TdxMapItemViewInfo.ClearCache;
begin
  FIsCachedBoundsValid := False;
  FIsTitleSizeValid := False;
  FIsStylesValid := False;
end;

function TdxMapItemViewInfo.GetScreenTip: TdxScreenTip;
begin
  Result := Item.ScreenTip;
end;

function TdxMapItemViewInfo.IsIntersect(const ARect: TRect): Boolean;
begin
  Result := cxRectIntersect(ARect, Bounds);
end;

procedure TdxMapItemViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Item.SelectionChanged(Shift);
end;

procedure TdxMapItemViewInfo.MergeStyles(AState: TdxMapControlElementState);
var
  AMergedStyle, AStyle, AOwnerStyle: TdxMapItemStyle;
begin
  AMergedStyle := FStyles[AState];
  AStyle := Item.GetStyle(AState);
  AOwnerStyle := (Item.Layer as TdxCustomMapItemLayer).GetStyle(AState);

  if mcsvColor in AStyle.AssignedValues then
    AMergedStyle.Color := AStyle.Color
  else
    if mcsvColor in AOwnerStyle.AssignedValues then
      AMergedStyle.Color := AOwnerStyle.Color
    else
      AMergedStyle.Color := dxacDefault;

  if AMergedStyle.Color = dxacDefault then
    AMergedStyle.Color := GetColor(AState);

  if mcsvBorderWidth in AStyle.AssignedValues then
    AMergedStyle.BorderWidth := AStyle.BorderWidth
  else
    if mcsvBorderWidth in AOwnerStyle.AssignedValues then
      AMergedStyle.BorderWidth := AOwnerStyle.BorderWidth
    else
      AMergedStyle.BorderWidth := GetBorderWidth(AState);

  if mcsvBorderColor in AStyle.AssignedValues then
    AMergedStyle.BorderColor := AStyle.BorderColor
  else
    if mcsvBorderColor in AOwnerStyle.AssignedValues then
      AMergedStyle.BorderColor := AOwnerStyle.BorderColor
    else
      AMergedStyle.BorderColor := dxacDefault;

  if AMergedStyle.BorderColor = dxacDefault then
    AMergedStyle.BorderColor := GetBorderColor(AState);

  if mcsvFont in AStyle.AssignedValues then
    AMergedStyle.Font := AStyle.Font
  else
    if mcsvFont in AOwnerStyle.AssignedValues then
      AMergedStyle.Font := AOwnerStyle.Font;

  if mcsvTextColor in AStyle.AssignedValues then
    AMergedStyle.TextColor := AStyle.TextColor
  else
    if mcsvTextColor in AOwnerStyle.AssignedValues then
      AMergedStyle.TextColor := AOwnerStyle.TextColor
    else
      AMergedStyle.TextColor := dxacDefault;

  if AMergedStyle.TextColor = dxacDefault then
    AMergedStyle.TextColor := GetTextColor(AState);

  if mcsvTextGlowColor in AStyle.AssignedValues then
    AMergedStyle.TextGlowColor := AStyle.TextGlowColor
  else
    if mcsvTextGlowColor in AOwnerStyle.AssignedValues then
      AMergedStyle.TextGlowColor := AOwnerStyle.TextGlowColor
    else
      AMergedStyle.TextGlowColor := dxacDefault;

  if AMergedStyle.TextGlowColor = dxacDefault then
    AMergedStyle.TextGlowColor := GetTextGlowColor(AState);
end;

function TdxMapItemViewInfo.ParseTextPattern(const ATextPattern: string): string;

  function FindOpenDelimeter(const S: string; ACurrentPos: Integer; out APos: Integer): Boolean;
  begin
    APos := PosEx('{', S, ACurrentPos);
    Result := APos <> 0;
  end;

  function FindCloseDelimeter(const S: string; ACurrentPos: Integer; out APos: Integer): Boolean;
  begin
    APos := PosEx('}', S, ACurrentPos);
    Result := APos <> 0;
  end;

var
  AName: string;
  AStartPatternPos, AEndPatternPos: Integer;
  AValue: Variant;
begin
  Result := ATextPattern;
  AEndPatternPos := 1;
  while FindOpenDelimeter(ATextPattern, AEndPatternPos, AStartPatternPos) and
    FindCloseDelimeter(ATextPattern, AStartPatternPos, AEndPatternPos) do
  begin
    AName := Trim(Copy(ATextPattern, AStartPatternPos + 1, AEndPatternPos - AStartPatternPos - 1));
    if Item.Attributes.Items.TryGetValue(AName, AValue) then
    begin
      Result := StringReplace(Result,
        Format('{%s}', [AName]), AValue, [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end;

procedure TdxMapItemViewInfo.CalculateScreenBounds;
begin
end;

procedure TdxMapItemViewInfo.CalculateTitleBounds;

  function GetTitleVisibility: TdxMapItemTitleVisibility;
  begin
    if (Item.TitleOptions.Visibility = mitvHidden) or
      ((Item.Layer as TdxCustomMapItemLayer).ItemTitleOptions.Visibility = mitvHidden) then
      Result := mitvHidden
    else
      if (Item.TitleOptions.Visibility = mitvVisible) or
        ((Item.Layer as TdxCustomMapItemLayer).ItemTitleOptions.Visibility = mitvVisible) then
        Result := mitvVisible
      else
        Result := mitvAuto;
  end;

var
  AVisibility: TdxMapItemTitleVisibility;
begin
  if HasTitle then
  begin
    AVisibility := GetTitleVisibility;
    if AVisibility = mitvHidden then
      Title.Bounds := cxEmptyRect
    else
    begin
      if not FIsTitleSizeValid then
      begin
        Title.CalculateSize;
        FIsTitleSizeValid := True;
      end;
      if (AVisibility = mitvAuto) and IsTitleOutOfBounds then
        Title.Bounds := cxEmptyRect
      else
        Title.Bounds := DoCalculateTitleBounds;
    end;
  end;
end;

procedure TdxMapItemViewInfo.DoCalculateCachedBounds;
begin
end;

function TdxMapItemViewInfo.DoCalculateTitleBounds: TRect;
begin
  if cxSizeIsEmpty(Title.Size) then
    Result := cxEmptyRect
  else
    Result := cxRectSetOrigin(Title.ClientBounds, cxPointOffset(FScreenCenterPoint,
      cxRectCenter(Title.ClientBounds), False));
end;

function TdxMapItemViewInfo.DoCanBeVisible: Boolean;
begin
  Result := True;
end;

procedure TdxMapItemViewInfo.DoElementDestroying;
begin
  GetMapControl(Item.Layer.GetParentComponent).Controller.ElementDestroying(Self);
end;

function TdxMapItemViewInfo.GetForbiddenStates: TdxMapControlElementStates;
begin
  Result := inherited GetForbiddenStates + [mcesPressed, mcesDisabled];
  if not (Item.Layer as TdxCustomMapItemLayer).AllowHotTrack then
    Result := Result + [mcesHot];
end;

function TdxMapItemViewInfo.GetMaxSizeFont: TFont;
begin
  if not FIsStylesValid then
    UpdateStyles;
  Result := FStyles.GetMaxSizeFont;
end;

function TdxMapItemViewInfo.GetHint: string;
begin
  Result := Item.GetHint;
  if Result <> '' then
    Result := ParseTextPattern(Result);
end;

function TdxMapItemViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtMapItem;
end;

function TdxMapItemViewInfo.GetMapBounds: TdxRectDouble;
begin
  if not FIsCachedBoundsValid then
    CalculateCachedBounds;
  Result := DoGetMapBounds;
end;

function TdxMapItemViewInfo.GetStyle: TdxMapItemStyle;
begin
  if not FIsStylesValid then
    UpdateStyles;
  Result := FStyles[State];
end;

function TdxMapItemViewInfo.GetText: string;

  function CheckTextPattern(out ATextPattern: string): Boolean;
  begin
    ATextPattern := Item.TitleOptions.Text;
    if ATextPattern = '' then
      ATextPattern := (Item.Layer as TdxCustomMapItemLayer).ItemTitleOptions.Text;
    Result := ATextPattern <> '';
  end;

var
  AText: string;
begin
  if CheckTextPattern(AText) then
    Result := ParseTextPattern(AText)
  else
    Result := '';
end;

function TdxMapItemViewInfo.GetTextColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  Result := LookAndFeelPainter.MapControlMapCustomElementTextColor;
end;

function TdxMapItemViewInfo.GetTextGlowColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  Result := LookAndFeelPainter.MapControlMapCustomElementTextGlowColor;
end;

function TdxMapItemViewInfo.GetTitle: TdxMapItemTextViewInfo;
begin
  Result := FTextInfo;
end;

function TdxMapItemViewInfo.GetVisibleBounds: TRect;
begin
  cxRectIntersect(Result, Bounds, ViewPortBounds);
end;

function TdxMapItemViewInfo.HasText: Boolean;
begin
  Result := HasTitle;
end;

procedure TdxMapItemViewInfo.CapturePaintCanvas;
var
  DC: HDC;
begin
  DC := dxGPPaintCanvas.GetHDC;
  cxPaintCanvas.BeginPaint(DC);
end;

function TdxMapItemViewInfo.HasTitle: Boolean;
begin
  Result := not Item.IsPointer;
end;

procedure TdxMapItemViewInfo.Invalidate;
var
  AVisibleBounds, ARect: TRect;
begin
  ARect := Bounds;
  if HasTitle then
    ARect := cxRectUnion(Title.Bounds, ARect);
  cxRectIntersect(AVisibleBounds, ARect, ViewPortBounds);
  Item.Layer.InvalidateRect(AVisibleBounds);
end;

function TdxMapItemViewInfo.IsBoundsScalable: Boolean;
begin
  Result := False;
end;

function TdxMapItemViewInfo.IsOwnerVisible: Boolean;
begin
  Result := Item.Visible;
end;

function TdxMapItemViewInfo.IsTitleOutOfBounds: Boolean;
var
  ASize: TSize;
begin
  ASize := cxSize(Bounds);
  Result := (FMaxTitleSize.Width < Title.Size.cx) or (FMaxTitleSize.Height < Title.Size.cy);
end;

function TdxMapItemViewInfo.NeedCalculateScreenBoundsToDetermineVisibility: Boolean;
begin
  Result := not IsBoundsScalable;
end;

function TdxMapItemViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := inherited PtInElement(APoint) and cxRectPtIn(ViewPortBounds, APoint);
end;

procedure TdxMapItemViewInfo.ReleasePaintCanvas;
var
  DC: HDC;
begin
  DC := cxPaintCanvas.Handle;
  cxPaintCanvas.EndPaint;
  dxGPPaintCanvas.ReleaseHDC(DC);
end;

procedure TdxMapItemViewInfo.UpdateStyles;
begin
   FStyles.EnumActualStyles(MergeStyles);
   FIsStylesValid := True;
   FIsTitleSizeValid := False;
end;

procedure TdxMapItemViewInfo.CalculateCachedBounds;
begin
  DoCalculateCachedBounds;
  FIsCachedBoundsValid := True;
  ResetRenderCache;
end;

function TdxMapItemViewInfo.DoGetMapBounds: TdxRectDouble;
begin
  Result := dxNullRectDouble;
end;

function TdxMapItemViewInfo.GetBorderColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  case AState of
    mcesNormal:
      Result := LookAndFeelPainter.MapControlShapeBorderColor;
    mcesSelected:
      Result := LookAndFeelPainter.MapControlShapeBorderSelectedColor;
  else
    Result := LookAndFeelPainter.MapControlShapeBorderHighlightedColor
  end;
end;

function TdxMapItemViewInfo.GetBorderWidth(AState: TdxMapControlElementState): Integer;
begin
  case AState of
    mcesNormal:
      Result := LookAndFeelPainter.MapControlShapeBorderWidth(dxDefaultScaleFactor);
    mcesSelected:
      Result := LookAndFeelPainter.MapControlShapeBorderSelectedWidth(dxDefaultScaleFactor);
  else
    Result := LookAndFeelPainter.MapControlShapeBorderHighlightedWidth(dxDefaultScaleFactor);
  end;
end;

function TdxMapItemViewInfo.GetColor(AState: TdxMapControlElementState): TdxAlphaColor;
begin
  case AState of
    mcesNormal:
      Result := LookAndFeelPainter.MapControlShapeColor;
    mcesSelected:
      Result := LookAndFeelPainter.MapControlShapeSelectedColor;
  else
    Result := LookAndFeelPainter.MapControlShapeHighlightedColor
  end;
end;

function TdxMapItemViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GetMapControl(Item.Layer.GetParentComponent).LookAndFeelPainter;
end;

function TdxMapItemViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GetMapControl(Item.Layer.GetParentComponent).ScaleFactor;
end;

function TdxMapItemViewInfo.GetViewPortBounds: TRect;
begin
  Result := cxRect(Item.Layer.ViewportInPixels);
end;

procedure TdxMapItemViewInfo.ResetRenderCache;
begin
  FCachedRenderScale := dxNullPointDouble;
  FCachedRenderOffset := dxNullPointDouble;
end;

{ TdxCustomMapItemViewInfo }

procedure TdxCustomMapItemViewInfo.DoPaint;
begin
end;

function TdxCustomMapItemViewInfo.GetVisibleBounds: TRect;
begin
  Result := Bounds;
end;

procedure TdxCustomMapItemViewInfo.Paint(ACanvas: TcxCanvas);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, GetVisibleBounds);
  try
    dxGPPaintCanvas.SmoothingMode := smAntiAlias;
    if dxGpIsRectVisible(dxGPPaintCanvas.Handle, Bounds) then
      DoPaint;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

{ TdxMapItemsZoomHelper }

class function TdxMapItemsZoomHelper.CalculateMapBounds(AItems: TdxMapItemList; out ARect: TdxRectDouble): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AItems.Count - 1 do
    if AItems[I].Layer.IsActuallyVisible and AItems[I].Visible then
      if not Result then
      begin
        ARect := AItems[I].GetMapBounds;
        Result := True;
      end
      else
        ARect.Union(AItems[I].GetMapBounds);
end;

end.
