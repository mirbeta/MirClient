{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxSVGCore;

{$I cxVer.inc}

interface

uses
  Types, TypInfo, Windows, Classes, Variants, Rtti, Graphics, Generics.Collections, Generics.Defaults, Contnrs,
  dxCore, dxSmartImage, dxXMLDoc, dxCoreGraphics, cxGraphics, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses,
  dxDPIAwareUtils, dxGenerics;

type
  TdxSVGElementRoot = class;

  TdxSVGContentUnits = (cuUserSpaceOnUse, cuObjectBoundingBox);
  TdxSVGFillMode = (fmNonZero, fmEvenOdd, fmInherit);
  TdxSVGLineCapStyle = (lcsDefault, lcsButt, lcsSquare, lcsRound);
  TdxSVGTextAnchor = (taStart, taMiddle, taEnd, taInherit);
  TdxSVGUnitsType = (utPx, utMm, utCm, utIn, utPc, utPt, utPercents);

  { TdxSVGFill }

  TdxSVGFill = record
    Data: Variant;

    class function Create(const AColor: TdxAlphaColor): TdxSVGFill; overload; static;
    class function Create(const AReference: string): TdxSVGFill; overload; static;
    class function Default: TdxSVGFill; static;
    function AsColor: TdxAlphaColor;
    function AsReference: string;
    function IsDefault: Boolean;
    function IsEmpty: Boolean;
    function IsReference: Boolean;
    procedure UpdateReference(const AOldReference, ANewReference: string);
  end;

  { TdxSVGValue }

  TdxSVGValue = record
    Data: Single;
    UnitsType: TdxSVGUnitsType;

    class function Create(const AValue: Single; AUnitsType: TdxSVGUnitsType): TdxSVGValue; static;
    function IsEmpty: Boolean; inline;
    function ToPixels(ATargetDPI: Integer = dxDefaultDPI): Single; overload;
    function ToPixels(const AParentSize: Single; ATargetDPI: Integer = dxDefaultDPI): Single; overload;
  end;

  { TdxSVGList<T> }

  TdxSVGList<T> = class(TList<T>)
  public
    procedure Assign(AValues: TdxSVGList<T>);
  {$IFNDEF DELPHIXE}
    function ToArray: TArray<T>;
  {$ENDIF}
  end;

  { TdxSVGReferences }

  TdxSVGReferences = class(TdxHashSet<string>)
  strict private const
    Template = 'id_%d';
  strict private
    FCounter: Integer;
  public
    procedure Clear;
    function Generate: string;
  end;

  { TdxSVGValues }

  TdxSVGValues = class(TdxSVGList<Single>);

  { TdxSVGPath }

  TdxSVGPath = class(TdxGPPath)
  strict private
    FCommandLine: string;
  public
    procedure Assign(ASource: TdxGPPath); override;
    procedure FromString(const S: string);
    function ToString: string; override;
  end;

  { TdxSVGPoints }

  TdxSVGPoints = class(TdxSVGList<TdxPointF>);

  { TdxSVGRect }

  TdxSVGRect = class
  strict private
    FValue: TdxRectF;

    function GetHeight: Single; inline;
    function GetWidth: Single; inline;
  public
    property Height: Single read GetHeight;
    property Width: Single read GetWidth;
    property Value: TdxRectF read FValue write FValue;
  end;

{$REGION 'Renderers'}

  { TdxSVGBrush }

  TdxSVGBrush = class(TdxGPBrush)
  strict private
    FGradientAutoScale: Boolean;
    FGradientLine: TdxRectF;
  protected
    procedure CreateGradientBrushHandle(out AHandle: Pointer); override;
    function NeedRecreateHandleOnTargetRectChange: Boolean; override;
  public
    property GradientAutoScale: Boolean read FGradientAutoScale write FGradientAutoScale;
    property GradientLine: TdxRectF read FGradientLine write FGradientLine;
  end;

  { TdxSVGPen }

  TdxSVGPen = class(TdxGpPen)
  strict private
    function GetBrush: TdxSVGBrush;
  protected
    function CreateBrush: TdxGPBrush; override;
  public
    property Brush: TdxSVGBrush read GetBrush;
  end;

  { TdxSVGCustomRenderer }

  TdxSVGCustomRenderer = class abstract
  strict private
    FBrush: TdxSVGBrush;
    FOpacity: Single;
    FOpacityAssigned: Boolean;
    FPalette: IdxColorPalette;
    FPen: TdxSVGPen;
  protected
    function ApplyOpacity(AColor: TdxAlphaColor): TdxAlphaColor;
    function TransformPenWidth(AWidth: Single): Single; virtual;
  public
    constructor Create(APalette: IdxColorPalette = nil);
    destructor Destroy; override;
    // Primitives
    procedure Ellipse(const ACenterX, ACenterY, ARadiusX, ARadiusY: Single); virtual; abstract;
    procedure Line(const X1, Y1, X2, Y2: Single); virtual; abstract;
    procedure Path(APath: TdxGPPath); virtual; abstract;
    procedure Polygon(const APoints: array of TdxPointF); virtual; abstract;
    procedure Polyline(const APoints: array of TdxPointF); virtual; abstract;
    procedure Rectangle(const X, Y, AWidth, AHeight: Single); virtual; abstract;
    procedure RoundRect(const X, Y, AWidth, AHeight, ARadiusX, ARadiusY: Single); virtual; abstract;
    procedure TextOut(const X, Y: Single; const AText: string; AFont: TdxGPFont); virtual; abstract;
    // Opacity
    function ModifyOpacity(const AOpacity: Single): Single; virtual;
    procedure SetOpacity(const AOpacity: Single); virtual;
    // State
    procedure RestoreClipRegion; virtual; abstract;
    procedure SaveClipRegion; virtual; abstract;
    procedure SetClipRegion(APath: TdxGPPath; AMode: TdxGPCombineMode = gmIntersect); virtual; abstract;
    // WorldTransform
    procedure ModifyWorldTransform(const AMatrix: TXForm); virtual; abstract;
    procedure RestoreWorldTransform; virtual; abstract;
    procedure SaveWorldTransform; virtual; abstract;
    //
    property Brush: TdxSVGBrush read FBrush;
    property Pen: TdxSVGPen read FPen;
    property Palette: IdxColorPalette read FPalette;
  end;

  { TdxSVGRenderer }

  TdxSVGRenderer = class(TdxSVGCustomRenderer)
  strict private
    FCanvas: TdxGPCanvas;
  protected
    function TransformPenWidth(AWidth: Single): Single; override;
  public
    constructor Create(ACanvas: TdxGPCanvas; APalette: IdxColorPalette = nil);
    // Primitives
    procedure Ellipse(const ACenterX, ACenterY, ARadiusX, ARadiusY: Single); override;
    procedure Line(const X1, Y1, X2, Y2: Single); override;
    procedure Path(APath: TdxGPPath); override;
    procedure Polygon(const APoints: array of TdxPointF); override;
    procedure Polyline(const APoints: array of TdxPointF); override;
    procedure Rectangle(const X, Y, AWidth, AHeight: Single); override;
    procedure RoundRect(const X, Y, AWidth, AHeight, ARadiusX, ARadiusY: Single); override;
    procedure TextOut(const X, Y: Single; const AText: string; AFont: TdxGPFont); override;
    // State
    procedure RestoreClipRegion; override;
    procedure SaveClipRegion; override;
    procedure SetClipRegion(APath: TdxGPPath; AMode: TdxGPCombineMode = gmIntersect); override;
    // WorldTransform
    procedure ModifyWorldTransform(const AMatrix: TXForm); override;
    procedure RestoreWorldTransform; override;
    procedure SaveWorldTransform; override;
  end;

  { TdxSVGRendererClipPath }

  TdxSVGRendererClipPath = class(TdxSVGCustomRenderer)
  strict private
    FClipPath: TdxGPPath;
    FSavedTransforms: TList<Integer>;
    FTransforms: TObjectList<TdxGPMatrix>;
  public
    constructor Create;
    destructor Destroy; override;
    // Primitives
    procedure Ellipse(const ACenterX, ACenterY, ARadiusX, ARadiusY: Single); override;
    procedure Line(const X1, Y1, X2, Y2: Single); override;
    procedure Path(APath: TdxGPPath); override;
    procedure Polygon(const APoints: array of TdxPointF); override;
    procedure Polyline(const APoints: array of TdxPointF); override;
    procedure Rectangle(const X, Y, AWidth, AHeight: Single); override;
    procedure RoundRect(const X, Y, AWidth, AHeight, ARadiusX, ARadiusY: Single); override;
    procedure TextOut(const X, Y: Single; const AText: string; AFont: TdxGPFont); override;
    // State
    procedure RestoreClipRegion; override;
    procedure SaveClipRegion; override;
    procedure SetClipRegion(APath: TdxGPPath; AMode: TdxGPCombineMode = gmIntersect); override;
    // WorldTransform
    procedure ModifyWorldTransform(const AMatrix: TXForm); override;
    procedure RestoreWorldTransform; override;
    procedure SaveWorldTransform; override;
    //
    property ClipPath: TdxGPPath read FClipPath;
  end;

{$ENDREGION}

{$REGION 'Elements'}

  { TdxSVGElement }

  TdxSVGElementClass = class of TdxSVGElement;
  TdxSVGElement = class(TPersistent)
  strict private
    FChildren: TObjectList;
    FClipPath: string;
    FFill: TdxSVGFill;
    FFillMode: TdxSVGFillMode;
    FFillOpacity: Single;
    FID: string;
    FOpacity: Single;
    FStroke: TdxSVGFill;
    FStrokeDashArray: TdxSVGValues;
    FStrokeDashOffset: Single;
    FStrokeLineCap: TdxSVGLineCapStyle;
    FStrokeLineJoin: TdxGpLineJoin;
    FStrokeMiterLimit: Single;
    FStrokeSize: Single;
    FStyleName: string;
    FTransform: TdxMatrix;

    function GetCount: Integer;
    function GetElement(Index: Integer): TdxSVGElement;
    procedure SetFillOpacity(const Value: Single);
    procedure SetOpacity(const Value: Single);
    procedure SetParent(const AParent: TdxSVGElement);
    procedure SetStrokeDashArray(const Value: TdxSVGValues);
    procedure SetTransform(const Value: TdxMatrix);
  private
    FParent: TdxSVGElement;
  protected
    procedure AssignCore(ASource: TdxSVGElement); virtual;
    function GetRoot: TdxSVGElementRoot; virtual;
    procedure UpdateReference(const AOldReference, ANewReference: string); virtual;
    // Clipping
    procedure ApplyClipping(ARenderer: TdxSVGCustomRenderer);
    function HasClipping: Boolean;
    // Drawing
    procedure InitializeBrush(ARenderer: TdxSVGCustomRenderer); virtual;
    procedure InitializePen(ARenderer: TdxSVGCustomRenderer); virtual;
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); virtual;
    procedure DrawCoreAndChildren(ARenderer: TdxSVGCustomRenderer);
    // Relative Coordinates and Sizes
    function GetX(const X: TdxSVGValue): Single; inline;
    function GetY(const Y: TdxSVGValue): Single; inline;
    // I/O
    procedure Load(const ANode: TdxXMLNode); virtual;
    procedure Save(const ANode: TdxXMLNode); virtual;
    // Actual Values
    function GetActualFill(APalette: IdxColorPalette): TdxSVGFill;
    function GetActualFillMode: TdxGPFillMode;
    function GetActualStroke(APalette: IdxColorPalette): TdxSVGFill;
    function GetActualStrokeDashArray: TdxSVGValues;
    function GetActualStrokeDashOffset: Single;
    function GetActualStrokeLineCap: TdxSVGLineCapStyle;
    function GetActualStrokeSize: Single;
  public
    constructor Create; overload; virtual;
    constructor Create(AParent: TdxSVGElement); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override; final;
    function Clone: TdxSVGElement;
    procedure Draw(ARenderer: TdxSVGCustomRenderer); virtual;
    function FindByID(const ID: string; out AElement: TdxSVGElement): Boolean; virtual;
    //
    property Count: Integer read GetCount;
    property Elements[Index: Integer]: TdxSVGElement read GetElement; default;
    property Parent: TdxSVGElement read FParent write SetParent;
    property Root: TdxSVGElementRoot read GetRoot;
  public
    property ID: string read FID write FID;
    property ClipPath: string read FClipPath write FClipPath;
    property Fill: TdxSVGFill read FFill write FFill;
    property FillMode: TdxSVGFillMode read FFillMode write FFillMode;
    property FillOpacity: Single read FFillOpacity write SetFillOpacity;
    property Opacity: Single read FOpacity write SetOpacity;
    property Stroke: TdxSVGFill read FStroke write FStroke;
    property StrokeDashArray: TdxSVGValues read FStrokeDashArray write SetStrokeDashArray;
    property StrokeDashOffset: Single read FStrokeDashOffset write FStrokeDashOffset;
    property StrokeSize: Single read FStrokeSize write FStrokeSize;
    property StrokeLineCap: TdxSVGLineCapStyle read FStrokeLineCap write FStrokeLineCap;
    property StrokeLineJoin: TdxGpLineJoin read FStrokeLineJoin write FStrokeLineJoin;
    property StrokeMiterLimit: Single read FStrokeMiterLimit write FStrokeMiterLimit;
    property StyleName: string read FStyleName write FStyleName;
    property Transform: TdxMatrix read FTransform write SetTransform;
  end;

  { TdxSVGElementGroup }

  TdxSVGElementGroup = class(TdxSVGElement)
  strict private
    FTag: string;
  protected
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    property Tag: string read FTag write FTag;
  end;

  { TdxSVGElementCircle }

  TdxSVGElementCircle = class(TdxSVGElement)
  strict private
    FRadius: TdxSVGValue;
    FCenterX: TdxSVGValue;
    FCenterY: TdxSVGValue;
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    property CenterX: TdxSVGValue read FCenterX write FCenterX;
    property CenterY: TdxSVGValue read FCenterY write FCenterY;
    property Radius: TdxSVGValue read FRadius write FRadius;
  end;

  { TdxSVGElementEllipse }

  TdxSVGElementEllipse = class(TdxSVGElement)
  strict private
    FCenterX: TdxSVGValue;
    FCenterY: TdxSVGValue;
    FRadiusX: TdxSVGValue;
    FRadiusY: TdxSVGValue;
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    property CenterX: TdxSVGValue read FCenterX write FCenterX;
    property CenterY: TdxSVGValue read FCenterY write FCenterY;
    property RadiusX: TdxSVGValue read FRadiusX write FRadiusX;
    property RadiusY: TdxSVGValue read FRadiusY write FRadiusY;
  end;

  { TdxSVGElementLine }

  TdxSVGElementLine = class(TdxSVGElement)
  strict private
    FX2: TdxSVGValue;
    FY2: TdxSVGValue;
    FX1: TdxSVGValue;
    FY1: TdxSVGValue;
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    property X1: TdxSVGValue read FX1 write FX1;
    property X2: TdxSVGValue read FX2 write FX2;
    property Y1: TdxSVGValue read FY1 write FY1;
    property Y2: TdxSVGValue read FY2 write FY2;
  end;

  { TdxSVGElementPath }

  TdxSVGElementPath = class(TdxSVGElement)
  strict private
    FPath: TdxSVGPath;

    procedure SetPath(const Value: TdxSVGPath);
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Path: TdxSVGPath read FPath write SetPath;
  end;

  { TdxSVGElementPolygon }

  TdxSVGElementPolygon = class(TdxSVGElement)
  strict private
    FPoints: TdxSVGPoints;

    procedure SetPoints(const Value: TdxSVGPoints);
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Points: TdxSVGPoints read FPoints write SetPoints;
  end;

  { TdxSVGElementPolyline }

  TdxSVGElementPolyline = class(TdxSVGElementPolygon)
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure InitializePen(ARenderer: TdxSVGCustomRenderer); override;
  end;

  { TdxSVGElementRectangle }

  TdxSVGElementRectangle = class(TdxSVGElement)
  strict private
    FCornerRadiusX: TdxSVGValue;
    FCornerRadiusY: TdxSVGValue;
    FHeight: TdxSVGValue;
    FWidth: TdxSVGValue;
    FX: TdxSVGValue;
    FY: TdxSVGValue;
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    property CornerRadiusX: TdxSVGValue read FCornerRadiusX write FCornerRadiusX;
    property CornerRadiusY: TdxSVGValue read FCornerRadiusY write FCornerRadiusY;
    property Height: TdxSVGValue read FHeight write FHeight;
    property Width: TdxSVGValue read FWidth write FWidth;
    property X: TdxSVGValue read FX write FX;
    property Y: TdxSVGValue read FY write FY;
  end;

  { TdxSVGElementText }

  TdxSVGElementText = class(TdxSVGElement)
  strict private
    FDX: TdxSVGValue;
    FDY: TdxSVGValue;
    FFontName: string;
    FFontSize: Single;
    FFontStyles: TFontStyles;
    FText: string;
    FTextAnchor: TdxSVGTextAnchor;
    FX: TdxSVGValue;
    FY: TdxSVGValue;

    function GetTextAssigned: Boolean;
    procedure SetFontStyleString(const AValue: string);
  protected
    FCachedFontFamily: TdxGPFontFamily;

    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
    function TryCreateFont(out AFont: TdxGpFont): Boolean;
    function TryCreateFontFamily(const AFamilyName: string; out AFamily: TdxGPFontFamily): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    //
    property TextAssigned: Boolean read GetTextAssigned;
  public
    property X: TdxSVGValue read FX write FX;
    property Y: TdxSVGValue read FY write FY;
    property DX: TdxSVGValue read FDX write FDX;
    property DY: TdxSVGValue read FDY write FDY;
    property FontName: string read FFontName write FFontName;
    property FontSize: Single read FFontSize write FFontSize;
    property FontStyles: TFontStyles read FFontStyles write FFontStyles;
    property Text: string read FText write FText;
    property TextAnchor: TdxSVGTextAnchor read FTextAnchor write FTextAnchor;
  end;

  { TdxSVGElementTSpan }

  TdxSVGElementTSpan = class(TdxSVGElementText)
  public
    constructor Create(AParent: TdxSVGElement); override;
  end;

  { TdxSVGElementRoot }

  TdxSVGElementRoot = class(TdxSVGElementGroup)
  strict private
    FBackground: TdxSVGRect;
    FHeight: TdxSVGValue;
    FViewBox: TdxSVGRect;
    FWidth: TdxSVGValue;
    FX: TdxSVGValue;
    FY: TdxSVGValue;

    function GetSize: TdxSizeF;
    procedure SetBackground(const Value: TdxSVGRect);
    procedure SetViewBox(const Value: TdxSVGRect);
  protected
    function GetRoot: TdxSVGElementRoot; override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    //
    property Size: TdxSizeF read GetSize;
  public
    property X: TdxSVGValue read FX write FX;
    property Y: TdxSVGValue read FY write FY;
    property Height: TdxSVGValue read FHeight write FHeight;
    property Width: TdxSVGValue read FWidth write FWidth;
    property Background: TdxSVGRect read FBackground write SetBackground;
    property ViewBox: TdxSVGRect read FViewBox write SetViewBox;
  end;

  { TdxSVGElementUse }

  TdxSVGElementUse = class(TdxSVGElement)
  strict private
    FReference: string;
    FX: TdxSVGValue;
    FY: TdxSVGValue;
  protected
    procedure DrawCore(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
    procedure UpdateReference(const AOldReference, ANewReference: string); override;
  public
    property X: TdxSVGValue read FX write FX;
    property Y: TdxSVGValue read FY write FY;
    property Reference: string read FReference write FReference;
  end;

{$ENDREGION}

{$REGION 'NeverRendered Elements'}

  { TdxSVGElementNeverRendered }

  TdxSVGElementNeverRendered = class(TdxSVGElement)
  public
    procedure Draw(ARenderer: TdxSVGCustomRenderer); override;
  end;

  { TdxSVGElementClipPath }

  TdxSVGElementClipPath = class(TdxSVGElementNeverRendered)
  public
    procedure ApplyTo(ARenderer: TdxSVGCustomRenderer; AElement: TdxSVGElement);
  end;

  { TdxSVGElementLinearGradientStop }

  TdxSVGElementLinearGradientStop = class(TdxSVGElementNeverRendered)
  strict private
    FColor: TdxAlphaColor;
    FColorOpacity: Single;
    FOffset: TdxSVGValue;
  protected
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    procedure AfterConstruction; override;
  public
    property Color: TdxAlphaColor read FColor write FColor;
    property ColorOpacity: Single read FColorOpacity write FColorOpacity;
    property Offset: TdxSVGValue read FOffset write FOffset;
  end;

  { TdxSVGElementLinearGradient }

  TdxSVGElementLinearGradient = class(TdxSVGElementNeverRendered)
  strict private
    FUnitsType: TdxSVGContentUnits;
    FX1: TdxSVGValue;
    FX2: TdxSVGValue;
    FY1: TdxSVGValue;
    FY2: TdxSVGValue;

    function GetElement(Index: Integer): TdxSVGElementLinearGradientStop;
    //
    function CalculateGradientLine(const APatternRect: TdxRectF): TdxRectF;
    procedure CalculateGradinentOffsets(const APatternRect, AGradientLine: TdxRectF;
      out AStartOffset, AFinishOffset: Single; out AInvertOrder: Boolean);
    procedure PopulateGradientPoints(APoints: TdxGPBrushGradientPoints; AStartOffset, AFinishOffset: Single; AInvertOrder: Boolean);
  protected
    procedure InitializeBrush(ARenderer: TdxSVGCustomRenderer); override;
    procedure InitializeBrushCore(ARenderer: TdxSVGCustomRenderer; ABrush: TdxSVGBrush);
    procedure InitializePen(ARenderer: TdxSVGCustomRenderer); override;
    procedure Load(const ANode: TdxXMLNode); override;
    procedure Save(const ANode: TdxXMLNode); override;
  public
    procedure AfterConstruction; override;
    //
    property Elements[Index: Integer]: TdxSVGElementLinearGradientStop read GetElement;
  public
    property X1: TdxSVGValue read FX1 write FX1;
    property Y1: TdxSVGValue read FY1 write FY1;
    property X2: TdxSVGValue read FX2 write FX2;
    property Y2: TdxSVGValue read FY2 write FY2;
    property UnitsType: TdxSVGContentUnits read FUnitsType write FUnitsType;
  end;

{$ENDREGION}

  { TdxSVGStyle }

  TdxSVGStyle = class(TdxXMLNode)
  strict private
    FName: string;
  public
    constructor Create(const AName: string); reintroduce;
    procedure Apply(AElement: TdxSVGElement);
    //
    property Name: string read FName;
  end;

  { TdxSVGStyles }

  TdxSVGStyles = class
  strict private const
    sInline = '_inline_%p';
  strict private
    FItems: TObjectDictionary<string, TdxSVGStyle>;

    function GetItem(const Name: string): TdxSVGStyle;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TdxSVGStyle;
    function AddInline(const AReference: TObject): TdxSVGStyle;
    procedure Apply(AElement: TdxSVGElement);
    function TryGetStyle(const AName: string; out AStyle: TdxSVGStyle): Boolean;
    //
    property Items[const Name: string]: TdxSVGStyle read GetItem; default;
  end;

  { TdxSVGFiler }

  TdxSVGFiler = class
  protected
    class var FElementMap: TdxMap<TdxXMLString, TdxSVGElementClass>;

    class procedure Finalize;
    class procedure Initialize;
  end;

  { TdxSVGImporter }

  TdxSVGImporter = class(TdxSVGFiler)
  strict private const
    StyleAttrName = 'style';
  protected
    class procedure ImportCore(AElement: TdxSVGElement; AStyles: TdxSVGStyles; ANode: TdxXMLNode);
    class procedure ImportNode(AParent: TdxSVGElement; AStyles: TdxSVGStyles; ANode: TdxXMLNode);
    class procedure ImportStyles(AStyles: TdxSVGStyles; ANode: TdxXMLNode);
  public
    class function GetSize(ADocument: TdxXMLDocument; out ASize: TSize): Boolean;
    class function Import(ADocument: TdxXMLDocument; out ARoot: TdxSVGElementRoot): Boolean;
  end;

  { TdxSVGExporter }

  TdxSVGExporter = class(TdxSVGFiler)
  protected
    class procedure ExportCore(AElement: TdxSVGElement; ANode: TdxXMLNode);
    class procedure ExportElement(AElement: TdxSVGElement; AParent: TdxXMLNode);
  public
    class procedure Export(ARoot: TdxSVGElementRoot; ADocument: TdxXMLDocument);
  end;

implementation

uses
  SysUtils, Math, StrUtils, Character, dxSVGCoreParsers, RTLConsts, dxStringHelper;

type
  TdxGPBrushGradientPointsAccess = class(TdxGPBrushGradientPoints);

  { TdxSVGNodeAdapter }

  TdxSVGNodeAdapter = class helper for TdxXMLNode
  public
    procedure SetAttr(const AName: TdxXMLString; const AValue, ADefaultValue: Single); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: Single); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: string); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxAlphaColor); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxGpLineJoin); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxMatrix); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxRectF; AAddNewPrefix: Boolean = False); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGContentUnits); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGFill); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGFillMode); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGLineCapStyle); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGTextAnchor); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGValue); overload;
    procedure SetAttr(const AName: TdxXMLString; const AValue: TdxSVGValues); overload;
  end;

  { TdxSVGAttributeConverter }

  TdxSVGAttributeConverter = class
  protected const
    RGBMacro = 'rgb(';
    URLPart1 = 'url(';
    URLPart2 = ')';
  protected const
    ContentUnitsToString: array[TdxSVGContentUnits] of string = ('userSpaceOnUse', 'objectBoundingBox');
    FillRuleToString: array[TdxSVGFillMode] of string = ('nonzero', 'evenodd', 'inherit');
    LineCapToString: array[TdxSVGLineCapStyle] of string = ('', 'butt', 'square', 'round');
    LineJoinToString: array[TdxGpLineJoin] of string = ('miter', 'bevel', 'round', '');
    TextAnchorToString: array[TdxSVGTextAnchor] of string = ('start', 'middle', 'end', 'inherit');
  protected
    class function AlphaColorToString(const AValue: TdxAlphaColor): string;
    class function ReferenceToString(const AValue: string; AIsLocalReference: Boolean): string;
    class function StringToAlphaColor(const AValue: string): TdxAlphaColor;
    class function StringToContentUnits(const S: string): TdxSVGContentUnits;
    class function StringToFillRule(const S: string): TdxSVGFillMode;
    class function StringToLineCap(const S: string): TdxSVGLineCapStyle;
    class function StringToLineJoin(const S: string): TdxGpLineJoin;
    class function StringToReference(S: string): string;
    class function StringToTextAnchor(const S: string): TdxSVGTextAnchor;
  end;

  { TdxSVGAttributeAdapter }

  TdxSVGAttributeAdapter = class helper for TdxXMLNodeAttribute
  strict private
    function GetValueAsColor: TdxAlphaColor;
    function GetValueAsContentUnits: TdxSVGContentUnits;
    function GetValueAsFill: TdxSVGFill;
    function GetValueAsFillMode: TdxSVGFillMode;
    function GetValueAsLineCap: TdxSVGLineCapStyle;
    function GetValueAsLineJoin: TdxGpLineJoin;
    function GetValueAsRectF: TdxRectF;
    function GetValueAsSvgValue: TdxSVGValue;
    function GetValueAsTextAnchor: TdxSVGTextAnchor;
    procedure SetValueAsColor(const Value: TdxAlphaColor);
    procedure SetValueAsContentUnits(const Value: TdxSVGContentUnits);
    procedure SetValueAsFill(const AValue: TdxSVGFill);
    procedure SetValueAsFillMode(const Value: TdxSVGFillMode);
    procedure SetValueAsLineCap(const Value: TdxSVGLineCapStyle);
    procedure SetValueAsLineJoin(const Value: TdxGpLineJoin);
    procedure SetValueAsRectF(const Value: TdxRectF);
    procedure SetValueAsSvgValue(const Value: TdxSVGValue);
    procedure SetValueAsTextAnchor(const Value: TdxSVGTextAnchor);
  public
    procedure GetValueAsArray(AArray: TdxSVGValues);
    procedure GetValueAsMatrix(AMatrix: TdxMatrix);
    procedure SetValueAsArray(AArray: TdxSVGValues);
    procedure SetValueAsMatrix(AMatrix: TdxMatrix);

    property ValueAsColor: TdxAlphaColor read GetValueAsColor write SetValueAsColor;
    property ValueAsContentUnits: TdxSVGContentUnits read GetValueAsContentUnits write SetValueAsContentUnits;
    property ValueAsFill: TdxSVGFill read GetValueAsFill write SetValueAsFill;
    property ValueAsFillMode: TdxSVGFillMode read GetValueAsFillMode write SetValueAsFillMode;
    property ValueAsLineCap: TdxSVGLineCapStyle read GetValueAsLineCap write SetValueAsLineCap;
    property ValueAsLineJoin: TdxGpLineJoin read GetValueAsLineJoin write SetValueAsLineJoin;
    property ValueAsRectF: TdxRectF read GetValueAsRectF write SetValueAsRectF;
    property ValueAsSvgValue: TdxSVGValue read GetValueAsSvgValue write SetValueAsSvgValue;
    property ValueAsTextAnchor: TdxSVGTextAnchor read GetValueAsTextAnchor write SetValueAsTextAnchor;
  end;

function GetDefaultAttribute(AProperty: TRttiProperty; out ADefault: Variant): Boolean;
{$IFDEF DELPHIXE3}
var
  AAttribute: TCustomAttribute;
{$ENDIF}
begin
{$IFDEF DELPHIXE3}
  for AAttribute in AProperty.GetAttributes do
    if AAttribute is DefaultAttribute then
    begin
      ADefault := DefaultAttribute(AAttribute).Value;
      Exit(True);
    end;
{$ENDIF}
  Result := False;
end;

function dxSVGFloatToString(const AValue: Single): string;
begin
  Result := FormatFloat('0.#####', AValue, dxInvariantFormatSettings);
end;

function dxGpApplyOpacity(AColor: TdxAlphaColor; AOpacity: Single): TdxAlphaColor;
begin
  if (AOpacity < 1) and not TdxAlphaColors.IsTransparentOrEmpty(AColor) then
  begin
    with dxAlphaColorToRGBQuad(AColor) do
      Result := dxMakeAlphaColor(Trunc(rgbReserved * AOpacity), rgbRed, rgbGreen, rgbBlue);
  end
  else
    Result := AColor;
end;

function dxGpMeasureString(AFont: TdxGpFont; const AText: string): TdxSizeF;
begin
  dxGPPaintCanvas.BeginPaint(cxScreenCanvas.Handle, cxSimpleRect);
  try
    Result := dxGPPaintCanvas.MeasureString(AText, AFont);
  finally
    dxGPPaintCanvas.EndPaint;
    cxScreenCanvas.Dormant;
  end;
end;

{ TdxSVGFill }

class function TdxSVGFill.Create(const AColor: TdxAlphaColor): TdxSVGFill;
begin
  Result.Data := AColor;
end;

class function TdxSVGFill.Create(const AReference: string): TdxSVGFill;
begin
  Result.Data := AReference;
end;

class function TdxSVGFill.Default: TdxSVGFill;
begin
  Result := TdxSVGFill.Create(TdxAlphaColors.Default);
end;

function TdxSVGFill.IsDefault: Boolean;
begin
  Result := VarIsNumeric(Data) and (Data = TdxAlphaColors.Default);
end;

function TdxSVGFill.IsEmpty: Boolean;
begin
  if IsReference then
    Result := AsReference = ''
  else
    Result := AsColor = TdxAlphaColors.Empty;
end;

function TdxSVGFill.IsReference: Boolean;
begin
  Result := VarIsStr(Data);
end;

procedure TdxSVGFill.UpdateReference(const AOldReference, ANewReference: string);
begin
  if IsReference then
  begin
    if dxSameText(AsReference, AOldReference) then
      Data := ANewReference;
  end;
end;

function TdxSVGFill.AsColor: TdxAlphaColor;
begin
  Result := Data;
end;

function TdxSVGFill.AsReference: string;
begin
  if IsReference then
    Result := Data
  else
    Result := '';
end;

{ TdxSVGValue }

class function TdxSVGValue.Create(const AValue: Single; AUnitsType: TdxSVGUnitsType): TdxSVGValue;
begin
  Result.Data := AValue;
  Result.UnitsType := AUnitsType;
end;

function TdxSVGValue.IsEmpty: Boolean;
begin
  Result := IsZero(Data);
end;

function TdxSVGValue.ToPixels(ATargetDPI: Integer): Single;
begin
  if UnitsType = utPercents then
    raise EInvalidArgument.Create('TdxSVGValue.ToPixels');
  Result := ToPixels(0, ATargetDPI);
end;

function TdxSVGValue.ToPixels(const AParentSize: Single; ATargetDPI: Integer = dxDefaultDPI): Single;
const
  CMPerInch = 2.54;
begin
  case UnitsType of
    utCm:
      Result := Data / CMPerInch * ATargetDPI;
    utIn:
      Result := Data * ATargetDPI;
    utMm:
      Result := Data / (10 * CMPerInch) * ATargetDPI;
    utPc:
      Result := Data * 12 / 72 * ATargetDPI;
    utPt:
      Result := Data / 72 * ATargetDPI;
    utPercents:
      Result := Data / 100 * AParentSize;
  else // pixels
    Result := Data;
  end;
end;

{ TdxSVGList<T> }

procedure TdxSVGList<T>.Assign(AValues: TdxSVGList<T>);
begin
  Clear;
  Capacity := AValues.Count;
  AddRange(AValues);
end;

{$IFNDEF DELPHIXE}
function TdxSVGList<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Items[I];
end;
{$ENDIF}

{ TdxSVGBrush }

procedure TdxSVGBrush.CreateGradientBrushHandle(out AHandle: Pointer);
var
  AColors: PdxAlphaColor;
  ACount: Integer;
  AFinishPoint: TdxGpPointF;
  AOffsets: PSingle;
  AStartPoint: TdxGpPointF;
begin
  if (GradientPoints.Count = 0) or GradientAutoScale and ((FTargetRect.Width = 0) or (FTargetRect.Height = 0)) then
  begin
    CreateEmptyBrushHandle(AHandle);
    Exit;
  end;

  AStartPoint := MakePoint(FGradientLine.Left, FGradientLine.Top);
  AFinishPoint := MakePoint(FGradientLine.Right, FGradientLine.Bottom);

  if GradientAutoScale then
  begin
    AStartPoint.X := FTargetRect.X + AStartPoint.X * FTargetRect.Width;
    AStartPoint.Y := FTargetRect.Y + AStartPoint.Y * FTargetRect.Height;
    AFinishPoint.X := FTargetRect.X + AFinishPoint.X * FTargetRect.Width;
    AFinishPoint.Y := FTargetRect.Y + AFinishPoint.Y * FTargetRect.Height;
  end;

  TdxGPBrushGradientPointsAccess(GradientPoints).CalculateParams(AColors, AOffsets, ACount);
  GdipCheck(GdipCreateLineBrush(@AStartPoint, @AFinishPoint, 0, 0, WrapModeTileFlipX, AHandle));
  GdipCheck(GdipSetLinePresetBlend(AHandle, AColors, AOffsets, ACount));
end;

function TdxSVGBrush.NeedRecreateHandleOnTargetRectChange: Boolean;
begin
  Result := (Style = gpbsGradient) and GradientAutoScale;
end;

{ TdxSVGPen }

function TdxSVGPen.CreateBrush: TdxGpBrush;
begin
  Result := TdxSVGBrush.Create;
end;

function TdxSVGPen.GetBrush: TdxSVGBrush;
begin
  Result := inherited Brush as TdxSVGBrush;
end;

{ TdxSVGReferences }

procedure TdxSVGReferences.Clear;
begin
  FCounter := 0;
  inherited;
end;

function TdxSVGReferences.Generate: string;
begin
  repeat
    Result := Format(Template, [FCounter]);
    Inc(FCounter);
  until not Contains(Result);
end;

{ TdxSVGPath }

procedure TdxSVGPath.Assign(ASource: TdxGPPath);
begin
  if ASource is TdxSVGPath then
  begin
    FCommandLine := TdxSVGPath(ASource).FCommandLine;
    inherited;
  end
  else
    raise EInvalidArgument.Create('');
end;

procedure TdxSVGPath.FromString(const S: string);
var
  AParser: TdxSVGParserPath;
  I: Integer;
begin
  Reset;
  FCommandLine := S;
  if S <> '' then
  begin
    AParser := TdxSVGParserPath.Create;
    try
      AParser.Parse(S);
      for I := 0 to AParser.CommandCount - 1 do
        AParser.Commands[I].Append(Self);
    finally
      AParser.Free;
    end;
  end;
end;

function TdxSVGPath.ToString: string;
begin
  Result := FCommandLine;
end;

{ TdxSVGRect }

function TdxSVGRect.GetHeight: Single;
begin
  Result := Value.Height;
end;

function TdxSVGRect.GetWidth: Single;
begin
  Result := Value.Width;
end;

{ TdxSVGCustomRenderer }

constructor TdxSVGCustomRenderer.Create(APalette: IdxColorPalette);
begin
  FPalette := APalette;
  FPen := TdxSVGPen.Create;
  FBrush := TdxSVGBrush.Create;
  SetOpacity(1.0);
end;

destructor TdxSVGCustomRenderer.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FPen);
  inherited;
end;

function TdxSVGCustomRenderer.ModifyOpacity(const AOpacity: Single): Single;
begin
  Result := FOpacity;
  SetOpacity(Result * AOpacity);
end;

procedure TdxSVGCustomRenderer.SetOpacity(const AOpacity: Single);
begin
  FOpacity := AOpacity;
  FOpacityAssigned := not SameValue(FOpacity, 1.0);
end;

function TdxSVGCustomRenderer.ApplyOpacity(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  if FOpacityAssigned then
    Result := dxGpApplyOpacity(AColor, FOpacity)
  else
    Result := AColor;
end;

function TdxSVGCustomRenderer.TransformPenWidth(AWidth: Single): Single;
begin
  Result := AWidth;
end;

{ TdxSVGRenderer }

constructor TdxSVGRenderer.Create(ACanvas: TdxGPCanvas; APalette: IdxColorPalette = nil);
begin
  inherited Create(APalette);
  FCanvas := ACanvas;
end;

procedure TdxSVGRenderer.Ellipse(const ACenterX, ACenterY, ARadiusX, ARadiusY: Single);
begin
  FCanvas.Ellipse(dxRectF(ACenterX - ARadiusX, ACenterY - ARadiusY, ACenterX + ARadiusX, ACenterY + ARadiusY), Pen, Brush);
end;

procedure TdxSVGRenderer.Line(const X1, Y1, X2, Y2: Single);
begin
  FCanvas.Line(X1, Y1, X2, Y2, Pen);
end;

procedure TdxSVGRenderer.Path(APath: TdxGPPath);
begin
  FCanvas.Path(APath, Pen, Brush);
end;

procedure TdxSVGRenderer.Polygon(const APoints: array of TdxPointF);
begin
  FCanvas.Polygon(APoints, Pen, Brush);
end;

procedure TdxSVGRenderer.Polyline(const APoints: array of TdxPointF);
begin
  FCanvas.Polyline(APoints, Pen);
end;

procedure TdxSVGRenderer.Rectangle(const X, Y, AWidth, AHeight: Single);
begin
  FCanvas.Rectangle(dxRectF(X, Y, X + AWidth, Y + AHeight), Pen, Brush);
end;

procedure TdxSVGRenderer.RoundRect(const X, Y, AWidth, AHeight, ARadiusX, ARadiusY: Single);
var
  APath: TdxGPPath;
begin
  APath := TdxGPPath.Create;
  try
    APath.AddRoundRect(dxRectF(X, Y, X + AWidth, Y + AHeight), ARadiusX, ARadiusY);
    Path(APath);
  finally
    APath.Free;
  end;
end;

procedure TdxSVGRenderer.TextOut(const X, Y: Single; const AText: string; AFont: TdxGPFont);
begin
  FCanvas.DrawString(AText, AFont, Brush, X, Y);
end;

procedure TdxSVGRenderer.RestoreClipRegion;
begin
  FCanvas.RestoreClipRegion;
end;

procedure TdxSVGRenderer.SaveClipRegion;
begin
  FCanvas.SaveClipRegion;
end;

procedure TdxSVGRenderer.SetClipRegion(APath: TdxGPPath; AMode: TdxGPCombineMode = gmIntersect);
begin
  FCanvas.SetClipPath(APath, AMode);
end;

procedure TdxSVGRenderer.ModifyWorldTransform(const AMatrix: TXForm);
var
  AGpMatrix: TdxGPMatrix;
begin
  AGpMatrix := TdxGPMatrix.CreateEx(AMatrix);
  try
    FCanvas.ModifyWorldTransform(AGpMatrix);
  finally
    AGpMatrix.Free;
  end;
end;

procedure TdxSVGRenderer.RestoreWorldTransform;
begin
  FCanvas.RestoreWorldTransform;
end;

procedure TdxSVGRenderer.SaveWorldTransform;
begin
  FCanvas.SaveWorldTransform;
end;

function TdxSVGRenderer.TransformPenWidth(AWidth: Single): Single;
var
  M11, M12, M21, M22, X: Single;
  APoint: TdxPointF;
begin
  if AWidth <= 0 then
    Exit(AWidth);

  with FCanvas.GetWorldTransform do
  try
    GetElements(M11, M12, M21, M22, X, X);
    SetElements(M11, M12, M21, M22, 0, 0);
    APoint := TransformPoint(dxPointF(AWidth, AWidth));
    Result := Sqrt(Abs(APoint.X * APoint.Y));
  finally
    Free;
  end;
end;

{ TdxSVGRendererClipPath }

constructor TdxSVGRendererClipPath.Create;
begin
  inherited Create(nil);
  FClipPath := TdxGPPath.Create;
end;

destructor TdxSVGRendererClipPath.Destroy;
begin
  FreeAndNil(FSavedTransforms);
  FreeAndNil(FTransforms);
  FreeAndNil(FClipPath);
  inherited;
end;

procedure TdxSVGRendererClipPath.Ellipse(const ACenterX, ACenterY, ARadiusX, ARadiusY: Single);
begin
  ClipPath.AddEllipse(dxRectF(ACenterX - ARadiusX, ACenterY - ARadiusY, ACenterX + ARadiusX, ACenterY + ARadiusY));
end;

procedure TdxSVGRendererClipPath.Line(const X1, Y1, X2, Y2: Single);
begin
  ClipPath.AddLine(X1, Y1, X2, Y2);
end;

procedure TdxSVGRendererClipPath.Path(APath: TdxGPPath);
begin
  ClipPath.AddPath(APath);
end;

procedure TdxSVGRendererClipPath.Polygon(const APoints: array of TdxPointF);
begin
  ClipPath.AddPolygon(APoints);
end;

procedure TdxSVGRendererClipPath.Polyline(const APoints: array of TdxPointF);
begin
  ClipPath.AddPolyline(APoints);
end;

procedure TdxSVGRendererClipPath.Rectangle(const X, Y, AWidth, AHeight: Single);
begin
  ClipPath.AddRect(dxRectF(X, Y, X + AWidth, Y + AHeight));
end;

procedure TdxSVGRendererClipPath.RoundRect(const X, Y, AWidth, AHeight, ARadiusX, ARadiusY: Single);
begin
  ClipPath.AddRoundRect(dxRectF(X, Y, X + AWidth, Y + AHeight), ARadiusX, ARadiusY);
end;

procedure TdxSVGRendererClipPath.TextOut(const X, Y: Single; const AText: string; AFont: TdxGPFont);
var
  ATextSize: TdxSizeF;
begin
  ATextSize := dxGpMeasureString(AFont, AText);
  ClipPath.AddString(AText, AFont, nil, dxRectF(X, Y, X + ATextSize.cx, Y + ATextSize.cy));
end;

procedure TdxSVGRendererClipPath.ModifyWorldTransform(const AMatrix: TXForm);
var
  AGpMatrix: TdxGPMatrix;
begin
  AGpMatrix := TdxGPMatrix.CreateEx(AMatrix);

  if FTransforms = nil then
    FTransforms := TObjectList<TdxGPMatrix>.Create;
  FTransforms.Add(AGpMatrix);

  ClipPath.Transform(AGpMatrix);
end;

procedure TdxSVGRendererClipPath.RestoreWorldTransform;
var
  AIndex, I: Integer;
begin
  if (FSavedTransforms <> nil) and (FSavedTransforms.Count > 0) then
  begin
    AIndex := FSavedTransforms.Last;
    FSavedTransforms.Delete(FSavedTransforms.Count - 1);
    for I := FTransforms.Count - 1 downto AIndex + 1 do
    begin
      FTransforms[I].Invert;
      ClipPath.Transform(FTransforms[I]);
      FTransforms.Delete(I);
    end;
  end;
end;

procedure TdxSVGRendererClipPath.SaveWorldTransform;
begin
  if FTransforms <> nil then
  begin
    if FSavedTransforms = nil then
      FSavedTransforms := TList<Integer>.Create;
    FSavedTransforms.Add(FTransforms.Count - 1);
  end;
end;

procedure TdxSVGRendererClipPath.RestoreClipRegion;
begin
  // do nothing
end;

procedure TdxSVGRendererClipPath.SaveClipRegion;
begin
  // do nothing
end;

procedure TdxSVGRendererClipPath.SetClipRegion(APath: TdxGPPath; AMode: TdxGPCombineMode);
begin
  // do nothing
end;

{ TdxSVGElement }

constructor TdxSVGElement.Create;
begin
  inherited Create;
  FOpacity := 1.0;
  FFillOpacity := 1.0;
  FStrokeMiterLimit := 4;
  FStrokeDashOffset := -1; // default
  FStrokeDashArray := TdxSVGValues.Create;
  FTransform := TdxMatrix.Create;
  Stroke := TdxSVGFill.Default;
  Fill := TdxSVGFill.Default;
end;

constructor TdxSVGElement.Create(AParent: TdxSVGElement);
begin
  Create;
  Parent := AParent;
end;

destructor TdxSVGElement.Destroy;
begin
  FreeAndNil(FStrokeDashArray);
  FreeAndNil(FTransform);
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxSVGElement.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if (Source is TdxSVGElement) and (Source <> Self) then
  begin
    FreeAndNil(FChildren);
    AssignCore(TdxSVGElement(Source));
    for I := 0 to TdxSVGElement(Source).Count - 1 do
      TdxSVGElement(Source).Elements[I].Clone.Parent := Self;
  end
  else
    inherited;
end;

function TdxSVGElement.Clone: TdxSVGElement;
begin
  Result := TdxSVGElementClass(ClassType).Create;
  Result.Assign(Self);
end;

procedure TdxSVGElement.Draw(ARenderer: TdxSVGCustomRenderer);
var
  AHasTransform: Boolean;
  APrevOpacity: Single;
begin
  APrevOpacity := ARenderer.ModifyOpacity(Opacity);
  AHasTransform := not Transform.IsIdentity;
  if AHasTransform then
  begin
    ARenderer.SaveWorldTransform;
    ARenderer.ModifyWorldTransform(Transform.XForm);
  end;

  InitializePen(ARenderer);
  InitializeBrush(ARenderer);

  if HasClipping then
  begin
    ARenderer.SaveClipRegion;
    try
      ApplyClipping(ARenderer);
      DrawCoreAndChildren(ARenderer);
    finally
      ARenderer.RestoreClipRegion;
    end;
  end
  else
    DrawCoreAndChildren(ARenderer);

  if AHasTransform then
    ARenderer.RestoreWorldTransform;
  ARenderer.SetOpacity(APrevOpacity);
end;

function TdxSVGElement.FindByID(const ID: string; out AElement: TdxSVGElement): Boolean;
var
  I: Integer;
begin
  if ID = '' then
    Exit(False);

  for I := 0 to Count - 1 do
    if dxSameText(Elements[I].ID, ID) then
    begin
      AElement := Elements[I];
      Exit(True);
    end;

  for I := 0 to Count - 1 do
  begin
    if Elements[I].FindByID(ID, AElement) then
      Exit(True);
  end;

  Result := False;
end;

procedure TdxSVGElement.AssignCore(ASource: TdxSVGElement);
var
  ANode: TdxXMLNode;
begin
  ANode := TdxXMLNode.Create;
  try
    ASource.Save(ANode);
    Load(ANode);
  finally
    ANode.Free;
  end;
end;

function TdxSVGElement.GetRoot: TdxSVGElementRoot;
begin
  if Parent <> nil then
    Result := Parent.Root
  else
    Result := nil;
end;

procedure TdxSVGElement.UpdateReference(const AOldReference, ANewReference: string);
var
  I: Integer;
begin
  if dxSameText(ID, AOldReference) then
    ID := ANewReference;
  if dxSameText(ClipPath, AOldReference) then
    ClipPath := ANewReference;
  Stroke.UpdateReference(AOldReference, ANewReference);
  Fill.UpdateReference(AOldReference, ANewReference);

  for I := 0 to Count - 1 do
    Elements[I].UpdateReference(AOldReference, ANewReference);
end;

procedure TdxSVGElement.ApplyClipping(ARenderer: TdxSVGCustomRenderer);

  function GetEffect(const ID: string; out AClipPath: TdxSVGElementClipPath): Boolean;
  var
    AElement: TdxSVGElement;
  begin
    Result := (ID <> '') and Root.FindByID(ID, AElement) and (AElement is TdxSVGElementClipPath);
    if Result then
      AClipPath := TdxSVGElementClipPath(AElement);
  end;

var
  AClipPath: TdxSVGElementClipPath;
begin
  if GetEffect(ClipPath, AClipPath) then
    AClipPath.ApplyTo(ARenderer, Self);
end;

function TdxSVGElement.HasClipping: Boolean;
begin
  Result := ClipPath <> '';
end;

procedure TdxSVGElement.InitializeBrush(ARenderer: TdxSVGCustomRenderer);
var
  AColor: TdxAlphaColor;
  AElement: TdxSVGElement;
  AFill: TdxSVGFill;
begin
  AFill := GetActualFill(ARenderer.Palette);
  if AFill.IsReference then
  begin
    if Root.FindByID(AFill.AsReference, AElement) then
      AElement.InitializeBrush(ARenderer);
  end
  else
  begin
    AColor := AFill.AsColor;
    if FillOpacity <> 1 then
      AColor := dxMakeAlphaColor(dxAlphaColorToColor(AColor), Round(MaxByte * FillOpacity));
    ARenderer.Brush.Style := gpbsSolid;
    ARenderer.Brush.Color := ARenderer.ApplyOpacity(AColor);
  end;
end;

procedure TdxSVGElement.InitializePen(ARenderer: TdxSVGCustomRenderer);
const
  LineCapMap: array[TdxSVGLineCapStyle] of TdxGPPenLineCapStyle = (gpcsFlat, gpcsFlat, gpcsSquare, gpcsRound);
var
  ADashes: TArray<Single>;
  AElement: TdxSVGElement;
  AFill: TdxSVGFill;
  APen: TdxSVGPen;
  APenWidth: Single;
  I: Integer;
begin
  APen := ARenderer.Pen;
  AFill := GetActualStroke(ARenderer.Palette);
  if AFill.IsReference then
  begin
    if Root.FindByID(AFill.AsReference, AElement) then
      AElement.InitializePen(ARenderer);
  end
  else
  begin
    APen.Brush.Style := gpbsSolid;
    APen.Brush.Color := ARenderer.ApplyOpacity(AFill.AsColor);
  end;

  APenWidth := GetActualStrokeSize;

  APen.Style := gppsSolid;
  APen.LineJoin := StrokeLineJoin;
  APen.LineStartCapStyle := LineCapMap[GetActualStrokeLineCap];
  APen.LineEndCapStyle := APen.LineStartCapStyle;
  APen.MiterLimit := StrokeMiterLimit;
  APen.Width := ARenderer.TransformPenWidth(APenWidth);

  ADashes := GetActualStrokeDashArray.ToArray;
  if Length(ADashes) > 0 then
  begin
    for I := Low(ADashes) to High(ADashes) do
      ADashes[I] := ADashes[I] / APenWidth;

    APen.Style := gppsDash;
    GdipCheck(GdipSetPenDashArray(APen.Handle, @ADashes[0], Length(ADashes)));
    GdipCheck(GdipSetPenDashOffset(APen.Handle, GetActualStrokeDashOffset));
  end;
end;

procedure TdxSVGElement.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  // do nothing
end;

procedure TdxSVGElement.DrawCoreAndChildren(ARenderer: TdxSVGCustomRenderer);
var
  I: Integer;
begin
  DrawCore(ARenderer);
  for I := 0 to Count - 1 do
    Elements[I].Draw(ARenderer);
end;

function TdxSVGElement.GetX(const X: TdxSVGValue): Single;
begin
  Result := X.ToPixels(Root.Size.cx);
end;

function TdxSVGElement.GetY(const Y: TdxSVGValue): Single;
begin
  Result := Y.ToPixels(Root.Size.cy);
end;

procedure TdxSVGElement.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if ANode.Attributes.Find('id', AAttr) then
    ID := AAttr.ValueAsString;
  if ANode.Attributes.Find('class', AAttr) then
    StyleName := AAttr.ValueAsString;
  if ANode.Attributes.Find('clip-path', AAttr) then
    ClipPath := TdxSVGAttributeConverter.StringToReference(AAttr.ValueAsString);
  if ANode.Attributes.Find('fill', AAttr) then
    Fill := AAttr.ValueAsFill;
  if ANode.Attributes.Find('fill-rule', AAttr) then
    FillMode := AAttr.ValueAsFillMode;
  if ANode.Attributes.Find('fill-opacity', AAttr) then
    FillOpacity := AAttr.ValueAsFloat;
  if ANode.Attributes.Find('opacity', AAttr) then
    Opacity := AAttr.ValueAsFloat;
  if ANode.Attributes.Find('stroke', AAttr) then
    Stroke := AAttr.ValueAsFill;
  if ANode.Attributes.Find('stroke-dasharray', AAttr) then
    AAttr.GetValueAsArray(StrokeDashArray);
  if ANode.Attributes.Find('stroke-dashoffset', AAttr) then
    StrokeDashOffset := AAttr.ValueAsFloat;
  if ANode.Attributes.Find('stroke-width', AAttr) then
    StrokeSize := AAttr.ValueAsSvgValue.ToPixels(0.0);
  if ANode.Attributes.Find('stroke-linecap', AAttr) then
    StrokeLineCap := AAttr.ValueAsLineCap;
  if ANode.Attributes.Find('stroke-linejoin', AAttr) then
    StrokeLineJoin := AAttr.ValueAsLineJoin;
  if ANode.Attributes.Find('stroke-miterlimit', AAttr) then
    StrokeMiterLimit := AAttr.ValueAsFloat;
  if ANode.Attributes.Find('transform', AAttr) then
    AAttr.GetValueAsMatrix(Transform);
end;

procedure TdxSVGElement.Save(const ANode: TdxXMLNode);
begin
  ANode.SetAttr('id', ID);

  ANode.SetAttr('clip-path', TdxSVGAttributeConverter.ReferenceToString(ClipPath, False));
  ANode.SetAttr('fill', Fill);
  ANode.SetAttr('fill-rule', FillMode);
  ANode.SetAttr('fill-opacity', FillOpacity, 1);
  ANode.SetAttr('opacity', Opacity, 1);
  ANode.SetAttr('stroke', Stroke);
  ANode.SetAttr('stroke-dasharray', StrokeDashArray);
  ANode.SetAttr('stroke-dashoffset', StrokeDashOffset, -1);
  ANode.SetAttr('stroke-linecap', StrokeLineCap);
  ANode.SetAttr('stroke-linejoin', StrokeLineJoin);
  ANode.SetAttr('stroke-miterlimit', StrokeMiterLimit, 4);
  ANode.SetAttr('stroke-width', StrokeSize, 0);
  ANode.SetAttr('transform', Transform);
end;

function TdxSVGElement.GetActualFill(APalette: IdxColorPalette): TdxSVGFill;
begin
  if APalette <> nil then
    Result := TdxSVGFill.Create(APalette.GetFillColor(StyleName))
  else
    Result := TdxSVGFill.Default;

  if Result.IsDefault then
    Result := Fill;

  if Result.IsDefault then
  begin
    if Parent <> nil then
      Result := Parent.GetActualFill(APalette)
    else
      Result := TdxSVGFill.Create(TdxAlphaColors.Black);
  end;
end;

function TdxSVGElement.GetActualFillMode: TdxGPFillMode;
begin
  case FillMode of
    fmEvenOdd:
      Result := gpfmAlternate;
    fmNonZero:
      Result := gpfmWinding;
  else
    // fmInherit
    if Parent <> nil then
      Result := Parent.GetActualFillMode
    else
      Result := gpfmWinding;
  end;
end;

function TdxSVGElement.GetActualStroke(APalette: IdxColorPalette): TdxSVGFill;
begin
  if (APalette <> nil) and not Stroke.IsDefault then
    Result := TdxSVGFill.Create(APalette.GetStrokeColor(StyleName))
  else
    Result := TdxSVGFill.Default;

  if Result.IsDefault then
    Result := Stroke;

  if Result.IsDefault then
  begin
    if Parent <> nil then
      Result := Parent.GetActualStroke(APalette)
    else
      Result := TdxSVGFill.Create(TdxAlphaColors.Empty);
  end;
end;

function TdxSVGElement.GetActualStrokeDashArray: TdxSVGValues;
begin
  Result := StrokeDashArray;
  if (Result.Count = 0) and (Parent <> nil) then
    Result := Parent.GetActualStrokeDashArray;
end;

function TdxSVGElement.GetActualStrokeDashOffset: Single;
begin
  Result := StrokeDashOffset;
  if Result < 0 then
  begin
    if Parent <> nil then
      Result := Parent.GetActualStrokeDashOffset
    else
      Result := 0;
  end;
end;

function TdxSVGElement.GetActualStrokeLineCap: TdxSVGLineCapStyle;
begin
  Result := StrokeLineCap;
  if (Result = lcsDefault) and (Parent <> nil) then
    Result := Parent.GetActualStrokeLineCap;
end;

function TdxSVGElement.GetActualStrokeSize: Single;
begin
  Result := StrokeSize;
  if Result = 0 then
  begin
    if Parent <> nil then
      Result := Parent.GetActualStrokeSize
    else
      Result := 1;
  end;
end;

function TdxSVGElement.GetCount: Integer;
begin
  if FChildren <> nil then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TdxSVGElement.GetElement(Index: Integer): TdxSVGElement;
begin
  if FChildren <> nil then
    Result := TdxSVGElement(FChildren[Index])
  else
    raise EInvalidArgument.CreateFmt(SListIndexError, [Index]);
end;

procedure TdxSVGElement.SetFillOpacity(const Value: Single);
begin
  FFillOpacity := EnsureRange(Value, 0, 1);
end;

procedure TdxSVGElement.SetOpacity(const Value: Single);
begin
  FOpacity := EnsureRange(Value, 0, 1);
end;

procedure TdxSVGElement.SetParent(const AParent: TdxSVGElement);
begin
  if AParent <> FParent then
  begin
    if FParent <> nil then
    begin
      FParent.FChildren.Extract(Self);
      FParent := nil;
    end;
    if AParent <> nil then
    begin
      FParent := AParent;
      if FParent.FChildren = nil then
        FParent.FChildren := TObjectList.Create;
      FParent.FChildren.Add(Self)
    end;
  end;
end;

procedure TdxSVGElement.SetStrokeDashArray(const Value: TdxSVGValues);
begin
  FStrokeDashArray.Assign(Value);
end;

procedure TdxSVGElement.SetTransform(const Value: TdxMatrix);
begin
  FTransform.Assign(Value);
end;

{ TdxSVGElementGroup }

procedure TdxSVGElementGroup.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('tag', AAttr) then
    Tag := AAttr.ValueAsString;
end;

procedure TdxSVGElementGroup.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('tag', Tag);
end;

{ TdxSVGElementCircle }

procedure TdxSVGElementCircle.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  ARenderer.Ellipse(GetX(CenterX), GetY(CenterY), GetX(Radius), GetX(Radius));
end;

procedure TdxSVGElementCircle.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('cx', AAttr) then
    CenterX := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('cy', AAttr) then
    CenterY := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('r', AAttr) then
    Radius := AAttr.ValueAsSvgValue;
end;

procedure TdxSVGElementCircle.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('cx', CenterX);
  ANode.SetAttr('cy', CenterY);
  ANode.SetAttr('r', Radius);
end;

{ TdxSVGElementEllipse }

procedure TdxSVGElementEllipse.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  ARenderer.Ellipse(GetX(CenterX), GetY(CenterY), GetX(RadiusX), GetY(RadiusY));
end;

procedure TdxSVGElementEllipse.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('cx', AAttr) then
    CenterX := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('cy', AAttr) then
    CenterY := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('rx', AAttr) then
    RadiusX := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('ry', AAttr) then
    RadiusY := AAttr.ValueAsSvgValue;
end;

procedure TdxSVGElementEllipse.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('cx', CenterX);
  ANode.SetAttr('cy', CenterY);
  ANode.SetAttr('rx', RadiusX);
  ANode.SetAttr('ry', RadiusY);
end;

{ TdxSVGElementLine }

procedure TdxSVGElementLine.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  ARenderer.Line(GetX(X1), GetY(Y1), GetX(X2), GetY(Y2));
end;

procedure TdxSVGElementLine.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('x1', AAttr) then
    X1 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y1', AAttr) then
    Y1 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('x2', AAttr) then
    X2 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y2', AAttr) then
    Y2 := AAttr.ValueAsSvgValue;
end;

procedure TdxSVGElementLine.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('x1', X1);
  ANode.SetAttr('y1', Y1);
  ANode.SetAttr('x2', X2);
  ANode.SetAttr('y2', Y2);
end;

{ TdxSVGElementPath }

constructor TdxSVGElementPath.Create;
begin
  inherited Create;
  FPath := TdxSVGPath.Create;
end;

destructor TdxSVGElementPath.Destroy;
begin
  FreeAndNil(FPath);
  inherited Destroy;
end;

procedure TdxSVGElementPath.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  Path.FillMode := GetActualFillMode;
  ARenderer.Path(Path);
end;

procedure TdxSVGElementPath.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('d', AAttr) then
    Path.FromString(AAttr.ValueAsString);
end;

procedure TdxSVGElementPath.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('d', Path.ToString);
end;

procedure TdxSVGElementPath.SetPath(const Value: TdxSVGPath);
begin
  FPath.Assign(Value);
end;

{ TdxSVGElementPolygon }

constructor TdxSVGElementPolygon.Create;
begin
  inherited Create;
  FPoints := TdxSVGPoints.Create;
end;

destructor TdxSVGElementPolygon.Destroy;
begin
  FreeAndNil(FPoints);
  inherited Destroy;
end;

procedure TdxSVGElementPolygon.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  ARenderer.Polygon(Points.ToArray);
end;

procedure TdxSVGElementPolygon.Load(const ANode: TdxXMLNode);
var
  AAttrs: TdxXMLNodeAttribute;
begin
  inherited;

  if ANode.Attributes.Find('points', AAttrs) then
    TdxSVGParserNumbers.AsPoints(AAttrs.ValueAsString, Points);
end;

procedure TdxSVGElementPolygon.Save(const ANode: TdxXMLNode);
var
  AStringBuilder: TStringBuilder;
  I: Integer;
begin
  inherited;

  if Points.Count > 0 then
  begin
    AStringBuilder := TdxStringBuilderManager.Get(256);
    try
      for I := 0 to Points.Count - 1 do
      begin
        if I > 0 then
          AStringBuilder.Append(' ');
        AStringBuilder.Append(dxSVGFloatToString(Points[I].X));
        AStringBuilder.Append(',');
        AStringBuilder.Append(dxSVGFloatToString(Points[I].Y));
      end;
      ANode.SetAttr('points', AStringBuilder.ToString);
    finally
      TdxStringBuilderManager.Release(AStringBuilder);
    end;
  end;
end;

procedure TdxSVGElementPolygon.SetPoints(const Value: TdxSVGPoints);
begin
  FPoints.Assign(Value);
end;

{ TdxSVGElementPolyline }

procedure TdxSVGElementPolyline.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  ARenderer.Polyline(Points.ToArray);
end;

procedure TdxSVGElementPolyline.InitializePen(ARenderer: TdxSVGCustomRenderer);
begin
  inherited;

  if ARenderer.Pen.IsEmpty then
  begin
    if ARenderer.Pen.Width = 0 then
      ARenderer.Pen.Width := ARenderer.TransformPenWidth(1);
    if ARenderer.Pen.Brush.IsEmpty then
      ARenderer.Pen.Brush.Assign(ARenderer.Brush);
  end;
end;

{ TdxSVGElementRectangle }

procedure TdxSVGElementRectangle.DrawCore(ARenderer: TdxSVGCustomRenderer);
begin
  if CornerRadiusX.IsEmpty and CornerRadiusY.IsEmpty then
    ARenderer.Rectangle(GetX(X), GetY(Y), GetX(Width), GetY(Height))
  else
    ARenderer.RoundRect(GetX(X), GetY(Y), GetX(Width), GetY(Height), GetX(CornerRadiusX), GetY(CornerRadiusY));
end;

procedure TdxSVGElementRectangle.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('x', AAttr) then
    X := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y', AAttr) then
    Y := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('height', AAttr) then
    Height := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('width', AAttr) then
    Width := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('rx', AAttr) then
    CornerRadiusX := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('ry', AAttr) then
    CornerRadiusY := AAttr.ValueAsSvgValue;
end;

procedure TdxSVGElementRectangle.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('x', X);
  ANode.SetAttr('y', Y);
  ANode.SetAttr('height', Height);
  ANode.SetAttr('width', Width);
  ANode.SetAttr('rx', CornerRadiusX);
  ANode.SetAttr('ry', CornerRadiusY);
end;

{ TdxSVGElementText }

constructor TdxSVGElementText.Create;
begin
  inherited;
  FFontName := string(DefFontData.Name);
  FFontSize := 8;
end;

destructor TdxSVGElementText.Destroy;
begin
  FreeAndNil(FCachedFontFamily);
  inherited;
end;

procedure TdxSVGElementText.DrawCore(ARenderer: TdxSVGCustomRenderer);
var
  AFont: TdxGPFont;
  AOffset: Single;
  APoint: TdxPointF;
  ATextSize: TdxSizeF;
begin
  if TextAssigned and TryCreateFont(AFont) then
  try
    ATextSize := dxGpMeasureString(AFont, Text);
    AOffset := AFont.Size / AFont.FontFamily.GetEmHeight(AFont.Style) * AFont.FontFamily.GetCellAscent(AFont.Style);
    APoint := dxPointF(GetX(X), GetY(Y) - AOffset);
    case TextAnchor of
      taMiddle:
        APoint.X := APoint.X - ATextSize.cx / 2;
      taEnd:
        APoint.X := APoint.X - ATextSize.cx;
    end;
    ARenderer.TextOut(APoint.X + GetX(dX), APoint.Y + GetY(dY), Text, AFont);
  finally
    AFont.Free;
  end;
end;

procedure TdxSVGElementText.Load(const ANode: TdxXMLNode);

  function StripFontName(const S: string): string;
  var
    I, J: Integer;
  begin
    J := Length(S);
    if J = 0 then
      Exit('');

    I := 1;
    if dxCharInSet(S[I], ['''', '"']) then
      Inc(I);
    if dxCharInSet(S[J], ['''', '"']) then
      Dec(J);
    Result := Trim(Copy(S, I, J - I + 1));
  end;

var
  AAttr: TdxXMLNodeAttribute;
  AStyle: TFontStyle;
begin
  inherited;

  if ANode.Text <> '' then
    Text := ANode.TextAsString;
  if ANode.Attributes.Find('x', AAttr) then
    X := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y', AAttr) then
    Y := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('dx', AAttr) then
    DX := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('dy', AAttr) then
    DY := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('font', AAttr) then
    SetFontStyleString(AAttr.ValueAsString);
  if ANode.Attributes.Find('font-family', AAttr) then
    FontName := StripFontName(AAttr.ValueAsString);
  if ANode.Attributes.Find('font-size', AAttr) then
    FontSize := AAttr.ValueAsSvgValue.ToPixels(0.0);
  if TdxSVGFontStyleString.TryStringToFontStyle(ANode.Attributes.GetValueAsString('font-style'), AStyle) then
    FontStyles := FontStyles + [AStyle];
  if TdxSVGFontStyleString.TryStringToFontStyle(ANode.Attributes.GetValueAsString('font-weight'), AStyle) then
    FontStyles := FontStyles + [AStyle];
  if ANode.Attributes.Find('text-anchor', AAttr) then
    TextAnchor := AAttr.ValueAsTextAnchor;
end;

procedure TdxSVGElementText.Save(const ANode: TdxXMLNode);
var
  ABuffer: TStringBuilder;
begin
  inherited;
  ANode.TextAsString := Text;
  ANode.SetAttr('x', X);
  ANode.SetAttr('y', Y);
  ANode.SetAttr('dx', DX);
  ANode.SetAttr('dy', DY);
  ANode.SetAttr('text-anchor', TextAnchor);

  ABuffer := TdxStringBuilderManager.Get(64);
  try
    // font
    ABuffer.Append('font: ');
    if fsItalic in FontStyles then
      ABuffer.Append(TdxSVGFontStyleString.NameMap[fsItalic]).Append(' ');
    if fsBold in FontStyles then
      ABuffer.Append(TdxSVGFontStyleString.NameMap[fsBold]).Append(' ');
    ABuffer.Append(Trunc(FontSize)).Append('px').Append(' ');
    ABuffer.Append(FontName);
    ABuffer.Append('; ');

    // font-style
    if fsUnderline in FontStyles then
    begin
      ABuffer.Append('font-style: ');
      ABuffer.Append(TdxSVGFontStyleString.NameMap[fsUnderline]);
      ABuffer.Append(';');
    end;

    ANode.SetAttr('style', ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

function TdxSVGElementText.TryCreateFont(out AFont: TdxGpFont): Boolean;
var
  AFontName: string;
  APos: Integer;
begin
  Result := False;

  if FCachedFontFamily = nil then
  begin
    if not TryCreateFontFamily(FontName, FCachedFontFamily) then
    begin
      AFontName := FontName;
      repeat
        APos := LastDelimiter('-', AFontName);
        if APos > 0 then
        begin
          AFontName := Copy(AFontName, 1, APos - 1);
          if TryCreateFontFamily(AFontName, FCachedFontFamily) then
            Break;
        end;
      until APos = 0;
    end;

    if (FCachedFontFamily = nil) and ContainsText('sans-serif', FontName) then
      FCachedFontFamily := TdxGPFontFamily.GenericSansSerif.Clone;
    if (FCachedFontFamily = nil) and ContainsText('serif', FontName) then
      FCachedFontFamily := TdxGPFontFamily.GenericSerif.Clone;
  end;

  if FCachedFontFamily <> nil then
  try
    AFont := TdxGPFont.Create(FCachedFontFamily, FontSize, TdxGPFontStyle(dxFontStylesToGpFontStyles(FontStyles)), guPixel);
    Result := True;
  except
    Result := False;
  end;
end;

function TdxSVGElementText.TryCreateFontFamily(const AFamilyName: string; out AFamily: TdxGPFontFamily): Boolean;
begin
  AFamily := TdxGPFontFamily.Create(AFamilyName);
  Result := AFamily.Handle <> nil;
  if not Result then
    FreeAndNil(AFamily);
end;

function TdxSVGElementText.GetTextAssigned: Boolean;
begin
  Result := Text <> '';
end;

procedure TdxSVGElementText.SetFontStyleString(const AValue: string);
var
  AName: string;
  ASize: Single;
  AStyles: TFontStyles;
begin
  if TdxSVGFontStyleString.Parse(AValue, AName, ASize, AStyles) then
  begin
    FFontName := AName;
    FFontSize := ASize;
    FFontStyles := AStyles;
  end;
end;

{ TdxSVGElementTSpan }

constructor TdxSVGElementTSpan.Create(AParent: TdxSVGElement);
begin
  inherited Create(AParent);

  if AParent is TdxSVGElementText then
    AssignCore(AParent);
end;

{ TdxSVGElementRoot }

constructor TdxSVGElementRoot.Create;
begin
  inherited Create;
  FViewBox := TdxSVGRect.Create;
  FBackground := TdxSVGRect.Create;
end;

destructor TdxSVGElementRoot.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FViewBox);
  inherited Destroy;
end;

function TdxSVGElementRoot.GetRoot: TdxSVGElementRoot;
begin
  Result := Self;
end;

function TdxSVGElementRoot.GetSize: TdxSizeF;
begin
  Result := ViewBox.Value.Size;
  if not Width.IsEmpty and (Width.UnitsType <> utPercents) then
    Result.cx := Width.ToPixels;
  if not Height.IsEmpty and (Height.UnitsType <> utPercents) then
    Result.cy := Height.ToPixels;
end;

procedure TdxSVGElementRoot.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('x', AAttr) then
    X := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y', AAttr) then
    Y := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('height', AAttr) then
    Height := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('width', AAttr) then
    Width := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('enable-background', AAttr) then
    Background.Value := AAttr.ValueAsRectF;
  if ANode.Attributes.Find('viewBox', AAttr) then
    ViewBox.Value := AAttr.ValueAsRectF;
end;

procedure TdxSVGElementRoot.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('x', X);
  ANode.SetAttr('y', Y);
  ANode.SetAttr('height', Height);
  ANode.SetAttr('width', Width);
  ANode.SetAttr('enable-background', Background.Value, True);
  ANode.SetAttr('viewBox', ViewBox.Value);
end;

procedure TdxSVGElementRoot.SetBackground(const Value: TdxSVGRect);
begin
  Background.Value := Value.Value;
end;

procedure TdxSVGElementRoot.SetViewBox(const Value: TdxSVGRect);
begin
  ViewBox.Value := Value.Value;
end;

{ TdxSVGElementUse }

procedure TdxSVGElementUse.DrawCore(ARenderer: TdxSVGCustomRenderer);
var
  AElement: TdxSVGElement;
  APrevParent: TdxSVGElement;
begin
  if Root.FindByID(Reference, AElement) then
  begin
    ARenderer.SaveWorldTransform;
    try
      ARenderer.ModifyWorldTransform(TXForm.CreateTranslateMatrix(GetX(X), GetY(Y)));

      APrevParent := AElement.FParent;
      AElement.FParent := Self;
      AElement.Draw(ARenderer);
      AElement.FParent := APrevParent;
    finally
      ARenderer.RestoreWorldTransform;
    end;
  end;
end;

procedure TdxSVGElementUse.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('x', AAttr) then
    X := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y', AAttr) then
    Y := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('href', AAttr) or ANode.Attributes.Find('xlink:href', AAttr) then
    Reference := TdxSVGAttributeConverter.StringToReference(AAttr.ValueAsString);
end;

procedure TdxSVGElementUse.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('x', X);
  ANode.SetAttr('y', Y);
  ANode.SetAttr('href', TdxSVGAttributeConverter.ReferenceToString(Reference, True));
end;

procedure TdxSVGElementUse.UpdateReference(const AOldReference, ANewReference: string);
begin
  if dxSameText(Reference, AOldReference) then
    Reference := ANewReference;
  inherited;
end;

{ TdxSVGElementNeverRendered }

procedure TdxSVGElementNeverRendered.Draw(ARenderer: TdxSVGCustomRenderer);
begin
  // do nothing
end;

{ TdxSVGElementClipPath }

procedure TdxSVGElementClipPath.ApplyTo(ARenderer: TdxSVGCustomRenderer; AElement: TdxSVGElement);
var
  ABuilder: TdxSVGRendererClipPath;
begin
  ABuilder := TdxSVGRendererClipPath.Create;
  try
    DrawCoreAndChildren(ABuilder);
    ARenderer.SetClipRegion(ABuilder.ClipPath);
  finally
    ABuilder.Free;
  end;
end;

{ TdxSVGElementLinearGradientStop }

procedure TdxSVGElementLinearGradientStop.AfterConstruction;
begin
  inherited;
  FColorOpacity := 1.0;
end;

procedure TdxSVGElementLinearGradientStop.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('stop-color', AAttr) then
    Color := AAttr.ValueAsColor;
  if ANode.Attributes.Find('stop-opacity', AAttr) then
    ColorOpacity := AAttr.ValueAsSvgValue.ToPixels(0.0);
  if ANode.Attributes.Find('offset', AAttr) then
    Offset := AAttr.ValueAsSvgValue;
end;

procedure TdxSVGElementLinearGradientStop.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('stop-color', Color);
  ANode.SetAttr('stop-opacity', ColorOpacity, 1.0);
  ANode.SetAttr('offset', Offset);
end;

{ TdxSVGElementLinearGradient }

procedure TdxSVGElementLinearGradient.AfterConstruction;
begin
  inherited;
  FX2 := TdxSVGValue.Create(1, utPx);
  FUnitsType := cuObjectBoundingBox;
end;

procedure TdxSVGElementLinearGradient.InitializeBrush(ARenderer: TdxSVGCustomRenderer);
begin
  InitializeBrushCore(ARenderer, ARenderer.Brush);
end;

procedure TdxSVGElementLinearGradient.InitializeBrushCore(ARenderer: TdxSVGCustomRenderer; ABrush: TdxSVGBrush);

  function IsEmpty(const R: TdxRectF): Boolean;
  begin
    Result := IsZero(R.Width) and IsZero(R.Height);
  end;

var
  AFinishOffset: Single;
  AInvertOrder: Boolean;
  APatternRect: TdxRectF;
  APatternRectEmpty: Boolean;
  AStartOffset: Single;
  I: Integer;
begin
  ABrush.FreeHandle;

  APatternRect := dxRectF(GetX(X1), GetY(Y1), GetX(X2), GetY(Y2));
  APatternRectEmpty := IsEmpty(APatternRect);
  if not APatternRectEmpty then
  begin
    ABrush.GradientAutoScale := UnitsType <> cuUserSpaceOnUse;
    ABrush.GradientLine := CalculateGradientLine(APatternRect);
    APatternRectEmpty := IsEmpty(ABrush.GradientLine);
  end;

  if APatternRectEmpty then
  begin
    PopulateGradientPoints(ABrush.GradientPoints, 0, 1, False);
    if ABrush.GradientPoints.Count > 0 then
      ABrush.Color := ARenderer.ApplyOpacity(ABrush.GradientPoints.Colors[ABrush.GradientPoints.Count - 1])
    else
      ABrush.Color := TdxAlphaColors.Empty;

    ABrush.Style := gpbsSolid;
  end
  else
  begin
    CalculateGradinentOffsets(APatternRect, ABrush.GradientLine, AStartOffset, AFinishOffset, AInvertOrder);
    PopulateGradientPoints(ABrush.GradientPoints, AStartOffset, AFinishOffset, False);
    for I := 0 to ABrush.GradientPoints.Count - 1 do
      ABrush.GradientPoints.Colors[I] := ARenderer.ApplyOpacity(ABrush.GradientPoints.Colors[I]);
    ABrush.Style := gpbsGradient;
  end;
  ABrush.HandleNeeded;
end;

procedure TdxSVGElementLinearGradient.InitializePen(ARenderer: TdxSVGCustomRenderer);
begin
  InitializeBrushCore(ARenderer, ARenderer.Pen.Brush);
end;

procedure TdxSVGElementLinearGradient.Load(const ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  inherited;
  if ANode.Attributes.Find('x1', AAttr) then
    X1 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y1', AAttr) then
    Y1 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('x2', AAttr) then
    X2 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('y2', AAttr) then
    Y2 := AAttr.ValueAsSvgValue;
  if ANode.Attributes.Find('gradientUnits', AAttr) then
    UnitsType := AAttr.ValueAsContentUnits;
end;

procedure TdxSVGElementLinearGradient.Save(const ANode: TdxXMLNode);
begin
  inherited;
  ANode.SetAttr('x1', X1);
  ANode.SetAttr('y1', Y1);
  ANode.SetAttr('x2', X2);
  ANode.SetAttr('y2', Y2);
  ANode.SetAttr('gradientUnits', UnitsType);
end;

function TdxSVGElementLinearGradient.CalculateGradientLine(const APatternRect: TdxRectF): TdxRectF;

  procedure AdjustPoint(var X, Y: Single; const ACanvasRect: TdxRectF; ADeltaX, ADeltaY: Single);
  var
    ACount: Single;
  begin
    ACount := 0;
    if ADeltaX < 0 then
      ACount := Max(ACount, (ACanvasRect.Left - X) / ADeltaX);
    if ADeltaX > 0 then
      ACount := Max(ACount, (ACanvasRect.Right - X) / ADeltaX);
    if ADeltaY < 0 then
      ACount := Max(ACount, (ACanvasRect.Top - Y) / ADeltaY);
    if ADeltaY > 0 then
      ACount := Max(ACount, (ACanvasRect.Bottom - Y) / ADeltaY);
    X := X + ACount * ADeltaX;
    Y := Y + ACount * ADeltaY;
  end;

var
  ACanvasRect: TdxRectF;
  ADeltaX: Single;
  ADeltaY: Single;
  ALength: Single;
begin
  if UnitsType = cuUserSpaceOnUse then
  begin
    ACanvasRect := cxRectSetSizeF(Root.ViewBox.Value, Root.Size);

    ALength := Sqrt(Sqr(APatternRect.Width) + Sqr(APatternRect.Height));
    ADeltaX := APatternRect.Width / ALength;
    ADeltaY := APatternRect.Height / ALength;

    Result := APatternRect;
    AdjustPoint(Result.Left, Result.Top, ACanvasRect, -ADeltaX, -ADeltaY);
    AdjustPoint(Result.Right, Result.Bottom, ACanvasRect, ADeltaX, ADeltaY);
  end
  else
    Result := APatternRect;
end;

procedure TdxSVGElementLinearGradient.CalculateGradinentOffsets(
  const APatternRect, AGradientLine: TdxRectF; out AStartOffset, AFinishOffset: Single; out AInvertOrder: Boolean);

  function CalculatePosition(X, Y: Single): Single;
  begin
    Result := Sqrt(Sqr(X) + Sqr(Y));
  end;

var
  ALength: Single;
begin
  ALength := CalculatePosition(AGradientLine.Width, AGradientLine.Height);
  AFinishOffset := CalculatePosition(APatternRect.Right - AGradientLine.Left, APatternRect.Bottom - AGradientLine.Top) / ALength;
  AStartOffset := CalculatePosition(APatternRect.Left - AGradientLine.Left, APatternRect.Top - AGradientLine.Top) / ALength;
  AInvertOrder := False;
end;

procedure TdxSVGElementLinearGradient.PopulateGradientPoints(
  APoints: TdxGPBrushGradientPoints; AStartOffset, AFinishOffset: Single; AInvertOrder: Boolean);

  function GetActualColor(AElement: TdxSVGElementLinearGradientStop): TdxAlphaColor;
  begin
    Result := dxGpApplyOpacity(AElement.Color, AElement.ColorOpacity);
  end;

  function GetActualOffset(AElement: TdxSVGElementLinearGradientStop): Single;
  begin
    Result := AElement.Offset.ToPixels(1.0);
    if AInvertOrder then
      Result := 1 - Result;
    Result := AStartOffset + Result * (AFinishOffset - AStartOffset);
  end;

var
  AElement: TdxSVGElementLinearGradientStop;
  I: Integer;
begin
  APoints.Clear;
  if Count > 0 then
  begin
    AStartOffset := EnsureRange(AStartOffset, 0, 1);
    AFinishOffset := Max(EnsureRange(AFinishOffset, 0, 1), AStartOffset);

    AElement := Elements[IfThen(AInvertOrder, Count - 1)];
    if GetActualOffset(AElement) > 0 then
      APoints.Add(0, GetActualColor(AElement));

    for I := 0 to Count - 1 do
    begin
      AElement := Elements[I];
      APoints.Add(GetActualOffset(AElement), GetActualColor(AElement));
    end;

    AElement := Elements[IfThen(AInvertOrder, 0, Count - 1)];
    if GetActualOffset(AElement) < 1 then
      APoints.Add(1, GetActualColor(AElement));
  end;
end;

function TdxSVGElementLinearGradient.GetElement(Index: Integer): TdxSVGElementLinearGradientStop;
begin
  Result := inherited Elements[Index] as TdxSVGElementLinearGradientStop;
end;

{ TdxSVGStyle }

constructor TdxSVGStyle.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TdxSVGStyle.Apply(AElement: TdxSVGElement);
begin
  AElement.Load(Self);
end;

{ TdxSVGStyles }

constructor TdxSVGStyles.Create;
begin
  inherited Create;
  FItems := TObjectDictionary<string, TdxSVGStyle>.Create([doOwnsValues]);
end;

destructor TdxSVGStyles.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxSVGStyles.Add(const AName: string): TdxSVGStyle;
begin
  if not FItems.TryGetValue(AName, Result) then
  begin
    Result := TdxSVGStyle.Create(AName);
    FItems.Add(AName, Result);
  end;
end;

function TdxSVGStyles.AddInline(const AReference: TObject): TdxSVGStyle;
begin
  Result := Add(Format(sInline, [Pointer(AReference)]));
end;

procedure TdxSVGStyles.Apply(AElement: TdxSVGElement);
var
  AStyle: TdxSVGStyle;
  AStyles: TStringDynArray;
  I: Integer;
begin
  AStyles := SplitString(AElement.StyleName, ' ');
  for I := 0 to Length(AStyles) - 1 do
  begin
    if TryGetStyle(AStyles[I], AStyle) then
      AStyle.Apply(AElement);
  end;
  if TryGetStyle(Format(sInline, [Pointer(AElement)]), AStyle) then
    AStyle.Apply(AElement);
  for I := 0 to AElement.Count - 1 do
    Apply(AElement[I]);
end;

function TdxSVGStyles.TryGetStyle(const AName: string; out AStyle: TdxSVGStyle): Boolean;
begin
  Result := (AName <> '') and FItems.TryGetValue(AName, AStyle);
end;

function TdxSVGStyles.GetItem(const Name: string): TdxSVGStyle;
begin
  Result := FItems.Items[Name]
end;

{ TdxSVGFiler }

class procedure TdxSVGFiler.Finalize;
begin
  FreeAndNil(FElementMap);
end;

class procedure TdxSVGFiler.Initialize;
begin
  FElementMap := TdxMap<TdxXMLString, TdxSVGElementClass>.Create;
  FElementMap.Add('circle', TdxSVGElementCircle);
  FElementMap.Add('clipPath', TdxSVGElementClipPath);
  FElementMap.Add('defs', TdxSVGElementNeverRendered);
  FElementMap.Add('ellipse', TdxSVGElementEllipse);
  FElementMap.Add('g', TdxSVGElementGroup);
  FElementMap.Add('line', TdxSVGElementLine);
  FElementMap.Add('linearGradient', TdxSVGElementLinearGradient);
  FElementMap.Add('path', TdxSVGElementPath);
  FElementMap.Add('polygon', TdxSVGElementPolygon);
  FElementMap.Add('polyline', TdxSVGElementPolyline);
  FElementMap.Add('rect', TdxSVGElementRectangle);
  FElementMap.Add('stop', TdxSVGElementLinearGradientStop);
  FElementMap.Add('svg', TdxSVGElementRoot);
  FElementMap.Add('text', TdxSVGElementText);
  FElementMap.Add('tspan', TdxSVGElementTSpan);
  FElementMap.Add('use', TdxSVGElementUse);
end;

{ TdxSVGImporter }

class function TdxSVGImporter.GetSize(ADocument: TdxXMLDocument; out ASize: TSize): Boolean;
var
  ANode: TdxXMLNode;
  ARoot: TdxSVGElementRoot;
begin
  Result := ADocument.FindChild('svg', ANode);
  if Result then
  begin
    ARoot := TdxSVGElementRoot.Create;
    try
      ARoot.Load(ANode);
      ASize := cxSize(ARoot.Size, False);
    finally
      ARoot.Free;
    end;
  end;
end;

class function TdxSVGImporter.Import(ADocument: TdxXMLDocument; out ARoot: TdxSVGElementRoot): Boolean;
var
  ANode: TdxXMLNode;
  AStyles: TdxSVGStyles;
begin
  Result := ADocument.FindChild('svg', ANode);
  if Result then
  begin
    AStyles := TdxSVGStyles.Create;
    try
      ARoot := TdxSVGElementRoot.Create;
      ImportCore(ARoot, AStyles, ANode);
      AStyles.Apply(ARoot);
    finally
      AStyles.Free;
    end;
  end;
end;

class procedure TdxSVGImporter.ImportCore(AElement: TdxSVGElement; AStyles: TdxSVGStyles; ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  AElement.Load(ANode);
  if ANode.Attributes.Find(StyleAttrName, AAttr) then
    TdxSVGParserInlineStyle.Parse(AStyles.AddInline(AElement), AAttr.ValueAsString);

  ANode := ANode.First;
  while ANode <> nil do
  begin
    ImportNode(AElement, AStyles, ANode);
    ANode := ANode.Next;
  end;
end;

class procedure TdxSVGImporter.ImportNode(AParent: TdxSVGElement; AStyles: TdxSVGStyles; ANode: TdxXMLNode);
var
  AElement: TdxSVGElement;
  AElementClass: TdxSVGElementClass;
begin
  if ANode.Name = StyleAttrName then
    ImportStyles(AStyles, ANode)
  else
    if FElementMap.TryGetValue(ANode.Name, AElementClass) then
    begin
      AElement := AElementClass.Create(AParent);
      AElement.StyleName := ANode.NameAsString;
      ImportCore(AElement, AStyles, ANode);
    end
end;

class procedure TdxSVGImporter.ImportStyles(AStyles: TdxSVGStyles; ANode: TdxXMLNode);

  procedure ParseStyle(AName: string; const AValue: string);
  begin
    AName := Trim(AName);
    if (AName <> '') and (AName[1] = '.') then
      Delete(AName, 1, 1);
    if AName <> '' then
      TdxSVGParserInlineStyle.Parse(AStyles.Add(AName), AValue);
  end;

var
  AStyleNames: TStringDynArray;
  AStyleParts: TStringDynArray;
  AStylesArray: TStringDynArray;
  I, J: Integer;
begin
  AStylesArray := SplitString(ANode.TextAsString, '}');
  for I := 0 to Length(AStylesArray) - 1 do
    if Pos('{', AStylesArray[I]) > 0 then
    begin
      AStyleParts := SplitString(Trim(AStylesArray[I]), '{');
      if Length(AStyleParts) = 2 then
      begin
        AStyleNames := SplitString(AStyleParts[0], ',');
        for J := 0 to Length(AStyleNames) - 1 do
          ParseStyle(AStyleNames[J], AStyleParts[1]);
      end;
    end;
end;

{ TdxSVGExporter }

class procedure TdxSVGExporter.Export(ARoot: TdxSVGElementRoot; ADocument: TdxXMLDocument);
var
  AChild: TdxXMLNode;
begin
  AChild := ADocument.AddChild('svg');
  AChild.Attributes.SetValue('version', '1.1');
  AChild.Attributes.SetValue('xmlns', 'http://www.w3.org/2000/svg');
  AChild.Attributes.SetValue('xmlns:xlink', 'http://www.w3.org/1999/xlink');
  ExportCore(ARoot, AChild);
end;

class procedure TdxSVGExporter.ExportCore(AElement: TdxSVGElement; ANode: TdxXMLNode);
var
  I: Integer;
begin
  AElement.Save(ANode);
  for I := 0 to AElement.Count - 1 do
    ExportElement(AElement[I], ANode);
end;

class procedure TdxSVGExporter.ExportElement(AElement: TdxSVGElement; AParent: TdxXMLNode);
var
  ANodeName: TdxXMLString;
begin
  if FElementMap.TryGetKey(TdxSVGElementClass(AElement.ClassType), ANodeName) then
    ExportCore(AElement, AParent.AddChild(ANodeName));
end;

{ TdxSVGAttributeConverter }

class function TdxSVGAttributeConverter.AlphaColorToString(const AValue: TdxAlphaColor): string;
begin
  if AValue <> TdxAlphaColors.Default then
    Result := TdxAlphaColors.ToHtml(AValue, False)
  else
    Result := '';
end;

class function TdxSVGAttributeConverter.ReferenceToString(const AValue: string; AIsLocalReference: Boolean): string;
begin
  if AValue <> '' then
  begin
    Result := '#' + AValue;
    if not AIsLocalReference then
      Result := URLPart1 + Result + URLPart2;
  end
  else
    Result := '';
end;

class function TdxSVGAttributeConverter.StringToAlphaColor(const AValue: string): TdxAlphaColor;
var
  ARgbMacro: Integer;
  ARgbValues: TdxSVGValues;
begin
  ARgbMacro := Pos(RGBMacro, AValue);
  if ARgbMacro > 0 then
  begin
    ARgbValues := TdxSVGParserNumbers.AsNumbers(Copy(AValue, ARgbMacro + Length(RGBMacro), MaxInt));
    try
      if ARgbValues.Count = 3 then
        Result := TdxAlphaColors.FromArgb(MaxByte, Round(ARgbValues[0]), Round(ARgbValues[1]), Round(ARgbValues[2]))
      else
        Result := TdxAlphaColors.Default;
    finally
      ARgbValues.Free;
    end;
  end
  else
    if AValue = 'inherit' then
      Result := TdxAlphaColors.Default
    else
      Result := TdxAlphaColors.FromHtml(AValue);
end;

class function TdxSVGAttributeConverter.StringToContentUnits(const S: string): TdxSVGContentUnits;
var
  AIndex: TdxSVGContentUnits;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    if ContentUnitsToString[AIndex] = S then
      Exit(AIndex);
  end;
  Result := cuObjectBoundingBox;
end;

class function TdxSVGAttributeConverter.StringToFillRule(const S: string): TdxSVGFillMode;
var
  ARule: TdxSVGFillMode;
begin
  for ARule := Low(TdxSVGFillMode) to High(TdxSVGFillMode) do
  begin
    if FillRuleToString[ARule] = S then
      Exit(ARule);
  end;
  Result := fmInherit;
end;

class function TdxSVGAttributeConverter.StringToLineCap(const S: string): TdxSVGLineCapStyle;
var
  AIndex: TdxSVGLineCapStyle;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    if LineCapToString[AIndex] = S then
      Exit(AIndex);
  end;
  Result := lcsDefault;
end;

class function TdxSVGAttributeConverter.StringToLineJoin(const S: string): TdxGpLineJoin;
var
  AIndex: TdxGpLineJoin;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    if LineJoinToString[AIndex] = S then
      Exit(AIndex);
  end;
  Result := LineJoinMiter;
end;

class function TdxSVGAttributeConverter.StringToTextAnchor(const S: string): TdxSVGTextAnchor;
var
  AIndex: TdxSVGTextAnchor;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    if TextAnchorToString[AIndex] = S then
      Exit(AIndex);
  end;
  Result := taInherit;
end;

class function TdxSVGAttributeConverter.StringToReference(S: string): string;
begin
  S := Trim(S);
  if StartsText(URLPart1, S) and EndsText(URLPart2, S)  then
    S := Copy(S, Length(URLPart1) + 1, Length(S) - Length(URLPart1) - Length(URLPart2));
  if StartsText('#', S) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := '';
end;

{ TdxSVGAttributeAdapter }

procedure TdxSVGAttributeAdapter.GetValueAsArray(AArray: TdxSVGValues);
var
  ASource: TdxSVGValues;
begin
  ASource := TdxSVGParserNumbers.AsNumbers(ValueAsString);
  try
    AArray.Assign(ASource);
  finally
    ASource.Free;
  end;
end;

procedure TdxSVGAttributeAdapter.SetValueAsArray(AArray: TdxSVGValues);
var
  AStringBuilder: TStringBuilder;
  I: Integer;
begin
  AStringBuilder := TdxStringBuilderManager.Get(256);
  try
    for I := 0 to AArray.Count - 1 do
    begin
      if I > 0 then
        AStringBuilder.Append(',');
      AStringBuilder.Append(dxSVGFloatToString(AArray[I]));
    end;
    ValueAsString := AStringBuilder.ToString;
  finally
    TdxStringBuilderManager.Release(AStringBuilder);
  end;
end;

function TdxSVGAttributeAdapter.GetValueAsColor: TdxAlphaColor;
begin
  Result := TdxSVGAttributeConverter.StringToAlphaColor(ValueAsString);
end;

function TdxSVGAttributeAdapter.GetValueAsFill: TdxSVGFill;
var
  AReference: string;
  AValue: string;
begin
  AValue := ValueAsString;
  if StartsText(TdxSVGAttributeConverter.URLPart1, Trim(AValue)) then
  begin
    AReference := TdxSVGAttributeConverter.StringToReference(AValue);
    if AReference <> '' then
      Result := TdxSVGFill.Create(AReference)
    else
      Result := TdxSVGFill.Default;
  end
  else
    Result := TdxSVGFill.Create(TdxSVGAttributeConverter.StringToAlphaColor(AValue));
end;

function TdxSVGAttributeAdapter.GetValueAsFillMode: TdxSVGFillMode;
begin
  Result := TdxSVGAttributeConverter.StringToFillRule(ValueAsString);
end;

function TdxSVGAttributeAdapter.GetValueAsLineCap: TdxSVGLineCapStyle;
begin
  Result := TdxSVGAttributeConverter.StringToLineCap(ValueAsString);
end;

function TdxSVGAttributeAdapter.GetValueAsLineJoin: TdxGpLineJoin;
begin
  Result := TdxSVGAttributeConverter.StringToLineJoin(ValueAsString);
end;

procedure TdxSVGAttributeAdapter.GetValueAsMatrix(AMatrix: TdxMatrix);
begin
  TdxSVGParserTransform.Parse(ValueAsString, AMatrix);
end;

function TdxSVGAttributeAdapter.GetValueAsRectF: TdxRectF;
begin
  Result := TdxSVGParserRect.Parse(ValueAsString);
end;

function TdxSVGAttributeAdapter.GetValueAsSvgValue: TdxSVGValue;
begin
  if not TdxSVGParserValue.Parse(ValueAsString, Result) then
    Result := TdxSVGValue.Create(0, utPx);
end;

function TdxSVGAttributeAdapter.GetValueAsTextAnchor: TdxSVGTextAnchor;
begin
  Result := TdxSVGAttributeConverter.StringToTextAnchor(ValueAsString);
end;

function TdxSVGAttributeAdapter.GetValueAsContentUnits: TdxSVGContentUnits;
begin
  Result := TdxSVGAttributeConverter.StringToContentUnits(ValueAsString);
end;

procedure TdxSVGAttributeAdapter.SetValueAsColor(const Value: TdxAlphaColor);
begin
  ValueAsString := TdxSVGAttributeConverter.AlphaColorToString(Value);
end;

procedure TdxSVGAttributeAdapter.SetValueAsFill(const AValue: TdxSVGFill);
begin
  if AValue.IsDefault then
    ValueAsString := ''
  else if AValue.IsReference then
    ValueAsString := TdxSVGAttributeConverter.ReferenceToString(AValue.AsReference, False)
  else if AValue.IsEmpty then
    ValueAsString := 'none'
  else
    ValueAsString := TdxSVGAttributeConverter.AlphaColorToString(AValue.AsColor);
end;

procedure TdxSVGAttributeAdapter.SetValueAsFillMode(const Value: TdxSVGFillMode);
begin
  ValueAsString := TdxSVGAttributeConverter.FillRuleToString[Value];
end;

procedure TdxSVGAttributeAdapter.SetValueAsLineCap(const Value: TdxSVGLineCapStyle);
begin
  ValueAsString := TdxSVGAttributeConverter.LineCapToString[Value];
end;

procedure TdxSVGAttributeAdapter.SetValueAsLineJoin(const Value: TdxGpLineJoin);
begin
  ValueAsString := TdxSVGAttributeConverter.LineJoinToString[Value];
end;

procedure TdxSVGAttributeAdapter.SetValueAsMatrix(AMatrix: TdxMatrix);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(256);
  try
    ABuffer.Append('matrix(');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eM11));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eM12));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eM21));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eM22));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eDx));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(AMatrix.XForm.eDy));
    ABuffer.Append(')');
    ValueAsString := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure TdxSVGAttributeAdapter.SetValueAsRectF(const Value: TdxRectF);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(32);
  try
    ABuffer.Append(dxSVGFloatToString(Value.Left));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(Value.Top));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(Value.Right));
    ABuffer.Append(' ');
    ABuffer.Append(dxSVGFloatToString(Value.Bottom));

    ValueAsString := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure TdxSVGAttributeAdapter.SetValueAsSvgValue(const Value: TdxSVGValue);
begin
  ValueAsString := dxSVGFloatToString(Value.Data) + TdxSVGParserValue.NameMap[Value.UnitsType];
end;

procedure TdxSVGAttributeAdapter.SetValueAsTextAnchor(const Value: TdxSVGTextAnchor);
begin
  ValueAsString := TdxSVGAttributeConverter.TextAnchorToString[Value];
end;

procedure TdxSVGAttributeAdapter.SetValueAsContentUnits(const Value: TdxSVGContentUnits);
begin
  ValueAsString := TdxSVGAttributeConverter.ContentUnitsToString[Value];
end;

{ TdxSVGNodeAdapter }

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: Single);
begin
  Attributes.SetValueAsString(AName, dxSVGFloatToString(AValue));
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: string);
begin
  if AValue <> '' then
    Attributes.SetValueAsString(AName, AValue);
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue, ADefaultValue: Single);
begin
  if not SameValue(AValue, ADefaultValue) then
    SetAttr(AName, AValue);
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGFill);
begin
  if not AValue.IsDefault then
    Attributes.Add(AName).ValueAsFill := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGFillMode);
begin
  if AValue <> fmNonZero then
    Attributes.Add(AName).ValueAsFillMode := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxGpLineJoin);
begin
  if AValue <> LineJoinMiter then
    Attributes.Add(AName).ValueAsLineJoin := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGLineCapStyle);
begin
  if AValue <> lcsDefault then
    Attributes.Add(AName).ValueAsLineCap := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGValues);
begin
  if AValue.Count > 0 then
    Attributes.Add(AName).SetValueAsArray(AValue);
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGTextAnchor);
begin
  if AValue <> taInherit then
    Attributes.Add(AName).ValueAsTextAnchor := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGContentUnits);
begin
  Attributes.Add(AName).ValueAsContentUnits := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxAlphaColor);
begin
  if AValue <> TdxAlphaColors.Default then
    Attributes.Add(AName).ValueAsColor := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxRectF; AAddNewPrefix: Boolean);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if not cxRectIsEmpty(AValue) then
  begin
    AAttr := Attributes.Add(AName);
    AAttr.ValueAsRectF := AValue;
    if AAddNewPrefix then
      AAttr.ValueAsString := 'new ' + AAttr.ValueAsString;
  end;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxSVGValue);
begin
//  if not AValue.IsEmpty then
    Attributes.Add(AName).ValueAsSvgValue := AValue;
end;

procedure TdxSVGNodeAdapter.SetAttr(const AName: TdxXMLString; const AValue: TdxMatrix);
begin
  if not AValue.IsIdentity then
    Attributes.Add(AName).SetValueAsMatrix(AValue);
end;

initialization
  TdxSVGFiler.Initialize;

finalization
  TdxSVGFiler.Finalize;
end.
