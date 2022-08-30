{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           GDI+ Library                                             }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE GDIPLUS LIBRARY AND ALL ACCOMPANYING  }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxGDIPlusClasses;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, SyncObjs, Graphics, ActiveX, Types, Clipbrd, ExtCtrls,
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Generics.Collections, Generics.Defaults,
  dxCore, dxCoreGraphics, dxDPIAwareUtils, cxGeometry, dxGDIPlusAPI, dxCoreClasses, Contnrs, dxSmartImage;

const
  DefaultAnimationFrameDelay = 75;

type
  TdxImageDataFormat = dxSmartImage.TdxImageDataFormat;

const
  dxImageUnknown = dxSmartImage.TdxImageDataFormat.dxImageUnknown;
  {$EXTERNALSYM dxImageUnknown}
  dxImageBitmap = dxSmartImage.TdxImageDataFormat.dxImageBitmap;
  {$EXTERNALSYM dxImageBitmap}
  dxImageJpeg = dxSmartImage.TdxImageDataFormat.dxImageJpeg;
  {$EXTERNALSYM dxImageJpeg}
  dxImagePng = dxSmartImage.TdxImageDataFormat.dxImagePng;
  {$EXTERNALSYM dxImagePng}
  dxImageTiff = dxSmartImage.TdxImageDataFormat.dxImageTiff;
  {$EXTERNALSYM dxImageTiff}
  dxImageGif = dxSmartImage.TdxImageDataFormat.dxImageGif;
  {$EXTERNALSYM dxImageGif}
  dxImageEmf = dxSmartImage.TdxImageDataFormat.dxImageEmf;
  {$EXTERNALSYM dxImageEmf}
  dxImageExif = dxSmartImage.TdxImageDataFormat.dxImageExif;
  {$EXTERNALSYM dxImageExif}
  dxImageIcon = dxSmartImage.TdxImageDataFormat.dxImageIcon;
  {$EXTERNALSYM dxImageIcon}
  dxImageMemoryBmp = dxSmartImage.TdxImageDataFormat.dxImageMemoryBmp;
  {$EXTERNALSYM dxImageMemoryBmp}
  dxImageWmf = dxSmartImage.TdxImageDataFormat.dxImageWmf;
  {$EXTERNALSYM dxImageWmf}

type
  TdxGPBrush = class;
  TdxGPCanvas = class;
  TdxGPCustomBrush = class;
  TdxGPImage = class;
  TdxGPImageAttributes = class;
  TdxGPImageHandle = class;
  TdxGPPath = class;
  TdxGPFont = class;
  TdxGPFontFamily = class;
  TdxGPStringFormat = class;

  TdxGPInterpolationMode = (
    imDefault = InterpolationModeDefault,
    imLowQuality = InterpolationModeLowQuality,
    imHighQuality = InterpolationModeHighQuality,
    imBilinear = InterpolationModeBilinear,
    imBicubic = InterpolationModeBicubic,
    imNearestNeighbor = InterpolationModeNearestNeighbor,
    imHighQualityBilinear = InterpolationModeHighQualityBilinear,
    imHighQualityBicubic = InterpolationModeHighQualityBicubic);

  TdxGPSmoothingMode = (smDefault, smHighSpeed, smHighQuality, smNone, smAntiAlias);

  TdxGPImagePropertyData = array of Byte;

  { IdxGPHandle }

  IdxGPHandle = interface
  ['{329D311E-2864-4E9F-93E7-5556D7E264AC}']
    function GetNativeHandle: Pointer;
  end;

  { TdxGPCustomGraphicObject }

  TdxGPCustomGraphicObject = class(TdxGpBase)
  private
    FChangeLockCount: Integer;
    FHandle: GpHandle;
    FOnChange: TNotifyEvent;

    function GetHandle: GpHandle;
    function GetHandleAllocated: Boolean;
  protected
    procedure Changed; virtual;
    procedure DoCreateHandle(out AHandle: GpHandle); virtual; abstract;
    procedure DoFreeHandle(AHandle: GpHandle); virtual; abstract;
    procedure DoHandleCreated; virtual;
  public
    constructor Create; overload; virtual;

    procedure Assign(ASource: TdxGPCustomGraphicObject); virtual;
    procedure BeforeDestruction; override;
    procedure FreeHandle;
    procedure HandleNeeded; virtual;
    //
    property Handle: GpHandle read GetHandle;
    property HandleAllocated: Boolean read GetHandleAllocated;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxGPMatrix }

  TdxGPMatrix = class(TdxGPCustomGraphicObject)
  strict private
    function GetIsIdentity: Boolean;
    function GetIsInvertible: Boolean;
    function GetOffsetX: Single;
    function GetOffsetY: Single;
  protected
    procedure DoCreateHandle(out AHandle: GpHandle); override;
    procedure DoFreeHandle(AHandle: GpHandle); override;
  public
    constructor CreateEx(const XForm: TXForm); overload;
    constructor CreateEx(M11, M12, M21, M22, DX, DY: Single); overload;
    constructor CreateFlip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single); overload;
    constructor CreateFlip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF); overload;

    procedure Assign(ASource: TdxGPCustomGraphicObject); override;
    function Clone: TdxGPMatrix;

    // Elements
    procedure GetElements(out M11, M12, M21, M22, DX, DY: Single);
    procedure SetElements(M11, M12, M21, M22, DX, DY: Single);

    procedure Flip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF);
    procedure Invert;
    function GetBoundingRectangle(const R: TdxRectF): TdxRectF;
    procedure Multiply(AMatrix: TdxGPMatrix; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure Reset;
    procedure Rotate(AAngle: Single; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure Rotate(AAngle: Single; const APivotPoint: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure Scale(const AScale: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload; inline;
    procedure Scale(const AScale, ACenter: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload; inline;
    procedure Scale(const AScaleX, AScaleY: Single; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure Shear(const AShear: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload; inline;
    procedure Shear(const AShearX, AShearY: Single; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure Translate(const AOffset: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload; inline;
    procedure Translate(const AOffsetX, AOffsetY: Single; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    function TransformPoint(const P: TdxPointF): TdxPointF; overload;
    function TransformPoint(const P: TPoint): TPoint; overload;
    procedure TransformPoints(var APoints: TArray<TPoint>); overload;
    procedure TransformPoints(var APoints: TArray<TdxPointF>); overload;
    procedure TransformPointsI(APoints: PPoint; ACount: Integer); overload;
    function TransformRect(const R: TdxRectF): TdxRectF; overload;
    function TransformRect(const R: TRect): TRect; overload;
    //
    property IsIdentity: Boolean read GetIsIdentity;
    property IsInvertible: Boolean read GetIsInvertible;
    property OffsetX: Single read GetOffsetX;
    property OffsetY: Single read GetOffsetY;
  end;

  { TdxGPBrushGradientPoints }

  TdxGPBrushGradientPoints = class(TdxGpBase)
  strict private
    FCapacity: Integer;
    FColors: array of TdxAlphaColor;
    FCount: Integer;
    FOffsets: array of Single;
    FOnChange: TNotifyEvent;

    procedure CheckIndex(Index: Integer); inline;
    procedure Grow;
    function GetColor(Index: Integer): TdxAlphaColor;
    function GetOffset(Index: Integer): Single;
    function InternalAdd(AOffset: Single; AColor: TdxAlphaColor): Integer; inline;
    procedure SetCapacity(AValue: Integer);
    procedure SetColor(Index: Integer; const AValue: TdxAlphaColor);
    procedure SetOffset(Index: Integer; const AValue: Single);
  protected
    procedure CalculateParams(out AColors: PdxAlphaColor; out AOffsets: PSingle; out ACount: Integer);
    procedure Changed;
    procedure ValidateOrder;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    function Add(AOffset: Single; AColor: TdxAlphaColor): Integer;
    procedure Assign(Source: TdxGPBrushGradientPoints);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure InvertOrder;
    //
    property Capacity: Integer read FCapacity write SetCapacity;
    property Colors[Index: Integer]: TdxAlphaColor read GetColor write SetColor;
    property Count: Integer read FCount;
    property Offsets[Index: Integer]: Single read GetOffset write SetOffset;
  end;

  { TdxGPCustomBrush }

  TdxGPCustomBrush = class(TdxGPCustomGraphicObject)
  protected
    FTargetRect: TdxGpRectF;

    procedure DoFreeHandle(AHandle: GpHandle); override;
    procedure DoTargetRectChanged; virtual;
    function NeedRecreateHandleOnTargetRectChange: Boolean; virtual;
  public
    procedure Assign(ASource: TdxGPCustomGraphicObject); override;
    procedure SetTargetRect(const R: TRect); overload; inline;
    procedure SetTargetRect(const R: TdxRectF); overload; inline;
    procedure SetTargetRect(const R: TdxGpRect); overload; inline;
    procedure SetTargetRect(const R: TdxGpRectF); overload; inline;

    property TargetRect: TdxGpRectF read FTargetRect;
  end;

  { TdxGPHatchBrush }

  TdxGPHatchBrush = class(TdxGPCustomBrush)
  strict private
    FBackgroundColor: TdxAlphaColor;
    FForegroundColor: TdxAlphaColor;
    FStyle: TdxGpHatchStyle;

    procedure SetBackgroundColor(const AValue: TdxAlphaColor);
    procedure SetForegroundColor(const AValue: TdxAlphaColor);
    procedure SetStyle(const AValue: TdxGpHatchStyle);
  protected
    procedure DoCreateHandle(out AHandle: GpHandle); override;
  public
    property BackgroundColor: TdxAlphaColor read FBackgroundColor write SetBackgroundColor;
    property ForegroundColor: TdxAlphaColor read FForegroundColor write SetForegroundColor;
    property Style: TdxGpHatchStyle read FStyle write SetStyle;
  end;

  { TdxGPBrush }

  TdxGPBrushGradientMode = (gpbgmHorizontal, gpbgmVertical, gpbgmForwardDiagonal, gpbgmBackwardDiagonal);
  TdxGPBrushStyle = (gpbsSolid, gpbsGradient, gpbsTexture, gpbsClear);

  TdxGPBrush = class(TdxGPCustomBrush)
  strict private
    FColor: TdxAlphaColor;
    FGradientMode: TdxGPBrushGradientMode;
    FGradientPoints: TdxGPBrushGradientPoints;
    FStyle: TdxGPBrushStyle;
    FTexture: TdxGPImage;
    FTextureTransform: TdxGPMatrix;

    procedure SetColor(const AValue: TdxAlphaColor);
    procedure SetGradientMode(const AValue: TdxGPBrushGradientMode);
    procedure SetGradientPoints(const AValue: TdxGPBrushGradientPoints);
    procedure SetStyle(const AValue: TdxGPBrushStyle);
    procedure SetTexture(const AValue: TdxGPImage);
    procedure SetTextureTransform(const AValue: TdxGpMatrix);
    //
    procedure PointsChangeHandler(Sender: TObject);
    procedure TextureChangeHandler(Sender: TObject);
    //
    function IsTextureBrush: Boolean;
    procedure ApplyTextureTransform;
  protected
    FOnHandleCreated: TNotifyEvent;

    procedure CreateEmptyBrushHandle(out AHandle: GpBrush); virtual;
    procedure CreateGradientBrushHandle(out AHandle: GpBrush); virtual;
    procedure CreateTextureBrushHandle(out AHandle: GpBrush); virtual;
    function GetIsEmpty: Boolean; virtual;
    //
    procedure DoCreateHandle(out AHandle: GpHandle); override;
    procedure DoHandleCreated; override;
    procedure DoTargetRectChanged; override;
    function NeedRecreateHandleOnTargetRectChange: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TdxGPCustomGraphicObject); override;
    //
    property Color: TdxAlphaColor read FColor write SetColor;
    property GradientMode: TdxGPBrushGradientMode read FGradientMode write SetGradientMode;
    property GradientPoints: TdxGPBrushGradientPoints read FGradientPoints write SetGradientPoints;
    property Style: TdxGPBrushStyle read FStyle write SetStyle;
    property Texture: TdxGPImage read FTexture write SetTexture;
    property TextureTransform: TdxGpMatrix read FTextureTransform write SetTextureTransform;
    //
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { TdxGPPen }

  TdxGPPenDashCapStyle = (gpdcFlat, gpdcRound, gpdcTriangle);
  TdxGPPenLineCapStyle = (
    gpcsFlat,
    gpcsSquare,
    gpcsRound
  );
  TdxGPPenStyle = (gppsSolid, gppsDash, gppsDot, gppsDashDot, gppsDashDotDot);

  TdxGPPen = class(TdxGPCustomGraphicObject)
  strict private
    FBrush: TdxGPBrush;
    FDashCapStyle: TdxGPPenDashCapStyle;
    FLineEndCapStyle: TdxGPPenLineCapStyle;
    FLineJoin: TdxGpLineJoin;
    FLineStartCapStyle: TdxGPPenLineCapStyle;
    FMiterLimit: Single;
    FStyle: TdxGPPenStyle;
    FWidth: Single;

    procedure BrushChangeHandler(Sender: TObject);
    procedure SetBrush(const AValue: TdxGPBrush);
    procedure SetDashCapStyle(const AValue: TdxGPPenDashCapStyle);
    procedure SetLineEndCapStyle(const AValue: TdxGPPenLineCapStyle);
    procedure SetLineJoin(const AValue: TdxGpLineJoin);
    procedure SetLineStartCapStyle(const AValue: TdxGPPenLineCapStyle);
    procedure SetMiterLimit(const AValue: Single);
    procedure SetStyle(const AValue: TdxGPPenStyle);
    procedure SetWidth(AValue: Single);
  protected
    function CreateBrush: TdxGPBrush; virtual;
    procedure DoCreateHandle(out AHandle: GpHandle); override;
    procedure DoFreeHandle(AHandle: GpHandle); override;
    procedure DoSetDashStyle(AHandle: GpHandle);
    procedure DoSetDashCapStyle(AHandle: GpHandle);
    procedure DoSetLineEndCapStyle(AHandle: GpHandle);
    procedure DoSetLineJoin(AHandle: GpHandle);
    procedure DoSetLineStartCapStyle(AHandle: GpHandle);
    procedure DoSetMiterLimit(AHandle: GpHandle);
    function GetIsEmpty: Boolean; virtual;
    procedure InitializePenPattern(APenStyle: TPenStyle; AScale: Single);
    procedure SetDashArray(const AValue: array of Single; AScale: Single); overload;
  public
    constructor Create; overload; override;
    constructor Create(AColor: TdxAlphaColor; AWidth: Single = 1; APenStyle: TPenStyle = psSolid; AScale: Single = 1); overload;
    destructor Destroy; override;
    procedure Assign(ASource: TdxGPCustomGraphicObject); override;
    procedure HandleNeeded; override;
    procedure SetDashArray(const AValue: array of Single); overload;
    procedure SetTargetRect(const R: TdxGpRect); overload; inline;
    procedure SetTargetRect(const R: TRect); overload; inline;
    procedure SetTargetRect(const R: TdxRectF); overload; inline;
    //
    property Brush: TdxGPBrush read FBrush write SetBrush;
    property Style: TdxGPPenStyle read FStyle write SetStyle;
    property Width: Single read FWidth write SetWidth;

    property DashCapStyle: TdxGPPenDashCapStyle read FDashCapStyle write SetDashCapStyle;
    property LineEndCapStyle: TdxGPPenLineCapStyle read FLineEndCapStyle write SetLineEndCapStyle;
    property LineJoin: TdxGpLineJoin read FLineJoin write SetLineJoin;
    property LineStartCapStyle: TdxGPPenLineCapStyle read FLineStartCapStyle write SetLineStartCapStyle;
    property MiterLimit: Single read FMiterLimit write SetMiterLimit;
    //
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { TdxGPPath }

  TdxGPFillMode = (gpfmAlternate, gpfmWinding);

  TdxGPPath = class(TdxGpBase)
  strict private
    FHandle: GpPath;

    function GetFillMode: TdxGPFillMode;
    procedure SetFillMode(AValue: TdxGPFillMode);
  public
    constructor Create; overload; virtual;
    constructor Create(AFillMode: TdxGPFillMode); overload; virtual;
    constructor Create(APoints: TdxGpPointFDynArray; const APointTypes: TBytes); overload; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TdxGPPath); virtual;
    //
    procedure Flatten(AMatrix: TdxGPMatrix = nil; AFlatness: Single = 0.25);
    procedure FigureFinish;
    procedure FigureStart;
    procedure Reset;
    //
    procedure AddArc(const X, Y, Width, Height, StartAngle, SweepAngle: Single);
    procedure AddBezier(const P1, P2, P3, P4: TdxPointF);
    procedure AddEllipse(const R: TRect); overload;
    procedure AddEllipse(const R: TdxRectF); overload;
    procedure AddLine(const X1, Y1, X2, Y2: Single);
    procedure AddPath(const Path: TdxGPPath);
    procedure AddPie(const X, Y, Width, Height, StartAngle, SweepAngle: Single);
    procedure AddPolygon(const APoints: array of TPoint); overload;
    procedure AddPolygon(const APoints: array of TdxPointF); overload;
    procedure AddPolyline(const APoints: array of TPoint); overload;
    procedure AddPolyline(const APoints: array of TdxPointF); overload;
    procedure AddRect(const R: TRect); overload;
    procedure AddRect(const R: TdxRectF); overload;
    procedure AddRoundRect(const R: TRect; ARadiusX, ARadiusY: Integer); overload;
    procedure AddRoundRect(const R: TdxRectF; ARadiusX, ARadiusY: Single); overload;

    procedure AddString(const S: string; AFont: TFont; AEmSize: Single; const ARect: TRect); overload;
    procedure AddString(const S: string; AFont: TdxGPFont; AFormat: TdxGPStringFormat; const ARect: TdxRectF); overload;
    procedure AddString(const S: string; AFont: TdxGPFont; AFormat: TdxGPStringFormat; const AOrigin: TdxPointF); overload;
    procedure AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single; const AOrigin: TdxPointF; AFormat: TdxGPStringFormat); overload;
    procedure AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single; const AOrigin: TPoint; AFormat: TdxGPStringFormat); overload;
    procedure AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single; const ALayoutRect: TdxRectF; AFormat: TdxGPStringFormat); overload;
    procedure AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single; const ALayoutRect: TRect; AFormat: TdxGPStringFormat); overload;
    //
    function GetBounds(APen: TdxGPPen = nil): TRect;
    function GetBoundsF(APen: TdxGPPen = nil): TdxRectF;
    function GetPointCount: Integer;
    function IsPointInPath(const P: TdxPointF; ACanvas: TdxGPCanvas = nil): Boolean; overload;
    function IsPointInPath(const P: TPoint; ACanvas: TdxGPCanvas = nil): Boolean; overload;
    function IsPointInPathOutline(const P: TPoint; APenWidth: Integer; ACanvas: TdxGPCanvas = nil): Boolean; overload;
    function IsPointInPathOutline(const P: TdxPointF; APen: TdxGPPen; ACanvas: TdxGPCanvas = nil): Boolean; overload;
    function IsPointInPathOutline(const P: TPoint; APen: TdxGPPen; ACanvas: TdxGPCanvas = nil): Boolean; overload;
    //
    procedure Transform(AMatrix: TdxGPMatrix);
    //
    property FillMode: TdxGPFillMode read GetFillMode write SetFillMode;
    property Handle: GpPath read FHandle;
  end;

  { TdxGPRegion }

  TdxGPCombineMode = (gmReplace, gmIntersect, gmUnion, gmXor, gmExclude, gmComplement);

  TdxGPRegion = class(TdxGPBase)
  strict private
    FHandle: GpRegion;
  public
    constructor Create; overload; virtual;
    constructor Create(APath: GpPath); overload; virtual;
    constructor Create(const ARect: TRect); overload; virtual;
    constructor Create(const ARect: TdxRectF); overload; virtual;
    constructor CreateFromRegion(ARegion: GpRegion; ADummy: Integer = 0); virtual;
    destructor Destroy; override;

    procedure CombineRegionRect(const ARect: TRect; AMode: TdxGPCombineMode); overload;
    procedure CombineRegionRect(const ARect: TdxRectF; AMode: TdxGPCombineMode); overload;
    procedure CombineRegionRegion(const ARegion: TdxGPRegion; AMode: TdxGPCombineMode);

    property Handle: GpRegion read FHandle;
  end;

  { TdxGPMemoryStream }

  TdxGPMemoryStream = class(TdxSmartImageDataStream)
  protected
    function Realloc(var ANewCapacity: Integer): Pointer; override;
  end;

  { TdxGPStringFormat }

  TdxGPStringFormat = class(TdxGPBase)
  strict private
    class var
      FGenericTypographic: TdxGPStringFormat;
      FGenericDefault: TdxGPStringFormat;
    class function GetGenericDefault: TdxGPStringFormat; static;
    class function GetGenericTypographic: TdxGPStringFormat; static;
  strict private
    FHandle: GpStringFormat;
    constructor CreateFromHandle(AHandle: GpStringFormat);
    function GetAlignment: TdxGpStringAlignment;
    function GetDigitSubstitutionLanguage: LANGID;
    function GetDigitSubstitutionMethod: TdxGPStringDigitSubstitute;
    function GetFormatFlags: Integer;
    function GetHotkeyPrefix: TdxGPHotkeyPrefix;
    function GetLineAlignment: TdxGPStringAlignment;
    function GetMeasurableCharacterRangeCount: Integer;
    function GetTrimming: TdxGPStringTrimming;

    procedure SetAlignment(const AValue: TdxGpStringAlignment);
    procedure SetFormatFlags(const AValue: Integer);
    procedure SetHotkeyPrefix(const AValue: TdxGPHotkeyPrefix);
    procedure SetLineAlignment(const AValue: TdxGPStringAlignment);
    procedure SetTrimming(const AValue: TdxGPStringTrimming);
  protected
    class procedure Finalize;
    //
    property Handle: GpStringFormat read FHandle;
  public
    constructor Create(AFormatAttributes: Integer = 0; ALanguage: LANGID = LANG_NEUTRAL); reintroduce; overload;
    constructor Create(AFormat: TdxGPStringFormat); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(ASource: TdxGPStringFormat);
    function Clone: TdxGPStringFormat;
    procedure SetTabStops(AFirstTabOffset: Single; ACount: Integer; const ATabStops: TArray<Single>);
    function GetTabStopCount: Integer;
    function GetTabStops(ACount: Integer; out AFirstTabOffset: Single): TArray<Single>;
    procedure SetDigitSubstitution(ALanguage: LANGID; ASubstitute: TdxGPStringDigitSubstitute);
    procedure SetMeasurableCharacterRanges(ACount: Integer; const ARanges: TArray<TdxGPCharacterRange>);

    property Alignment: TdxGpStringAlignment read GetAlignment write SetAlignment;
    property DigitSubstitutionLanguage: LANGID read GetDigitSubstitutionLanguage;
    property DigitSubstitutionMethod: TdxGPStringDigitSubstitute read GetDigitSubstitutionMethod;
    property FormatFlags: Integer read GetFormatFlags write SetFormatFlags;
    property HotkeyPrefix: TdxGPHotkeyPrefix read GetHotkeyPrefix write SetHotkeyPrefix;
    property LineAlignment: TdxGPStringAlignment read GetLineAlignment write SetLineAlignment;
    property MeasurableCharacterRangeCount: Integer read GetMeasurableCharacterRangeCount;
    property Trimming: TdxGPStringTrimming read GetTrimming write SetTrimming;

    class property GenericDefault: TdxGPStringFormat read GetGenericDefault;
    class property GenericTypographic: TdxGPStringFormat read GetGenericTypographic;
  end;

  { TdxGPFontFamily }

  TdxGPFontFamily = class(TdxGPBase)
  strict private
    class var
      FGenericSansSerif: TdxGPFontFamily;
      FGenericSerif: TdxGPFontFamily;
      FGenericMonospace: TdxGPFontFamily;
    class function GetGenericSansSerif: TdxGPFontFamily; static;
    class function GetGenericSerif: TdxGPFontFamily; static;
    class function GetGenericMonospace: TdxGPFontFamily; static;
  strict private
    FHandle: GpFontFamily;
    function GetNameProperty: string;
  protected
    constructor Create(AHandle: GpFontFamily{TODO: ; AFontCollection: TdxGPFontCollection = nil}); reintroduce; overload;
    class procedure Finalize;
  public
    constructor Create(const AName: string{TODO: ; AFontCollection: TdxGPFontCollection = nil}); reintroduce; overload;
    destructor Destroy; override;
    function GetName(ALanguage: LANGID): string;
    function Clone: TdxGPFontFamily;
    function GetCellAscent(AStyle: Integer): Word;
    function GetCellDescent(AStyle: Integer): Word;
    function GetEmHeight(AStyle: Integer): Word;
    function GetLineSpacing(AStyle: Integer): Word;
    function IsStyleAvailable(AStyle: Integer): Boolean;

    property Handle: GpFontFamily read FHandle;
    property Name: string read GetNameProperty;

    class property GenericSansSerif: TdxGPFontFamily read GetGenericSansSerif;
    class property GenericSerif: TdxGPFontFamily read GetGenericSerif;
    class property GenericMonospace: TdxGPFontFamily read GetGenericMonospace;
  end;

  TdxGPFontFamilies = TArray<TdxGPFontFamily>;

  { TdxGPFontCollection }

  TdxGPFontCollection = class(TdxGPBase)
  strict private
    function GetFamilyCount: Integer;
    function GetFamilies: TdxGPFontFamilies;
    procedure ClearFamilies;
  strict protected
    FFamilies: TdxGPFontFamilies;
    FHandle: GpFontCollection;
    procedure CreateHandle; virtual; abstract;
    procedure FreeHandle; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property FamilyCount: Integer read GetFamilyCount;
    property Families: TdxGPFontFamilies read GetFamilies;
  end;

  { TdxGPInstalledFontCollection }

  TdxGPInstalledFontCollection = class(TdxGPFontCollection)
  protected
    procedure CreateHandle; override;
  end;

  { TdxGPPrivateFontCollection }

  TdxGPPrivateFontCollection = class(TdxGPFontCollection)
  protected
    procedure CreateHandle; override;
    procedure FreeHandle; override;
  end;

  TdxGPFontStyle = (
    FontStyleRegular    = 0,
    FontStyleBold       = 1,
    FontStyleItalic     = 2,
    FontStyleUnderline  = 4,
    FontStyleStrikeout  = 8
  );
  TdxGPFontStyles = set of TdxGPFontStyle;

  { TdxGPFont }


  { TdxFont }

  TdxGPFont = class
  strict private
    FNativeFont: GpFont;
    FFontSize: Single;
    FFontStyle: Integer;
    FFontFamily: TdxGPFontFamily;
    FFontUnit: TdxGraphicUnit;
    FGdiCharSet: Byte;
    FGdiVerticalFont: Boolean;
    FIsOwnFontFamily: Boolean;
    FSystemFontName: string;
    FOriginalFontName: string;

    procedure CreateNativeFont;
    procedure Initialize(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle;
      AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean); overload;
    procedure Initialize(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle;
      AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean); overload;

    function GetFontFamily: TdxGPFontFamily;
    function GetBold: Boolean;
    function GetItalic: Boolean;
    function GetName: string;
    function GetStrikeout: Boolean;
    function GetUnderline: Boolean;
    function GetSizeInPoints: Single;
    function GetHeightProperty: Integer;
    function GetIsSystemFont: Boolean;
  public
    constructor Create(APrototype: TdxGPFont; ANewStyle: TdxGPFontStyle); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single; AUnit: TdxGraphicUnit); overload;
    constructor Create(AFamily: TdxGPFontFamily; AEmSize: Single); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single; AUnit: TdxGraphicUnit); overload;
    constructor Create(const AFamilyName: string; AEmSize: Single); overload;
    constructor Create(ANativeFont: GpFont; AGdiCharSet: Byte; AGdiVerticalFont: Boolean); overload;
    destructor Destroy; override;
    class function FromHFont(AHFont: THandle): TdxGPFont; static;
    class function FromLogFont(const ALogFont: TLogFont): TdxGPFont; overload; static;
    class function FromLogFont(const ALogFont: TLogFont; AHdc: THandle): TdxGPFont; overload; static;
    class function FromHdc(AHdc: THandle): TdxGPFont; static;
    class function SafeCreate(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle;
      AUnit: TdxGraphicUnit = guPoint; AGdiCharSet: Byte = DEFAULT_CHARSET): TdxGPFont;
    function Clone: TdxGPFont;
    procedure SetFontFamily(AFamily: TdxGPFontFamily);
    class function IsVerticalName(const AFamilyName: string): Boolean; static;
    function GetHashCode: Integer; override;
    class function StripVerticalName(const AFamilyName: string): string; static;
    procedure ToLogFont(out ALogFont: TLogFont); overload;
    procedure ToLogFont(out ALogFont: TLogFont; ACanvas: TdxGPCanvas); overload;
    function ToHfont: THandle;
    function GetHeight(AGraphics: TdxGPCanvas): Single; overload;
    function GetHeight(ADpi: Single): Single; overload;
    function GetHeight: Single; overload;
    procedure SetSystemFontName(const ASystemFontName: string);

    property NativeFont: GpFont read FNativeFont;
    property FontFamily: TdxGPFontFamily read GetFontFamily;
    property Bold: Boolean read GetBold;
    property GdiCharSet: Byte read FGdiCharSet;
    property GdiVerticalFont: Boolean read FGdiVerticalFont;
    property Italic: Boolean read GetItalic;
    property Name: string read GetName;
    property OriginalFontName: string read FOriginalFontName;
    property Strikeout: Boolean read GetStrikeout;
    property Underline: Boolean read GetUnderline;
    property Style: Integer read FFontStyle;
    property Size: Single read FFontSize;
    property SizeInPoints: Single read GetSizeInPoints;
    property &Unit: TdxGraphicUnit read FFontUnit;
    property Height: Integer read GetHeightProperty;
    property IsSystemFont: Boolean read GetIsSystemFont;
    property SystemFontName: string read FSystemFontName;
  end;

  { TdxGPCanvas }

  TdxGPCanvas = class(TdxGPBase)
  strict private
    FSavedClipRegions: TStack;
    FSavedWorldTransforms: TStack;

    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetInterpolationMode: TdxGPInterpolationMode;
    function GetPixelOffsetMode: TdxGpPixelOffsetMode;
    function GetSmoothingMode: TdxGPSmoothingMode;
    function GetTextRenderingHint: TdxGpTextRenderingHint;
    procedure SetInterpolationMode(AValue: TdxGPInterpolationMode);
    procedure SetPixelOffsetMode(AValue: TdxGpPixelOffsetMode);
    procedure SetSmoothingMode(AValue: TdxGPSmoothingMode);
    procedure SetTextRenderingHint(AValue: TdxGpTextRenderingHint);
  protected
    FHandle: GpGraphics;
    FIsLowColorsMode: Boolean;

    procedure CheckDestRect(var R: TRect);
    procedure CreateHandle(DC: HDC);
    procedure FreeHandle;
    function MeasureString(const AText: string; AFont: TdxGPFont; ALayoutRect: TdxGpRectF;
      AStringFormat: TdxGPStringFormat; out ACharactersFitted, ALinesFilled: Integer): TdxSizeF; overload;
  public
    constructor Create; overload; virtual;
    constructor Create(AHandle: GpGraphics); overload;
    constructor Create(DC: THandle); overload;
    destructor Destroy; override;
    //
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    procedure Clear(AColor: TColor);
    //
    procedure Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TdxRectF; AAlpha: Byte = 255); overload;
    procedure Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TRect; AAlpha: Byte = 255); overload;
    procedure Draw(AGraphic: TdxGPImage; const R: TdxRectF; AAlpha: Byte = 255); overload;
    procedure Draw(AGraphic: TdxGPImage; const R: TRect; AAlpha: Byte = 255); overload;
    procedure Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TdxRectF; AAttributes: TdxGPImageAttributes); overload;
    procedure Draw(AGraphic: TdxGPImage; const R: TRect; AAttributes: TdxGPImageAttributes); overload;
    procedure Draw(AGraphic: TdxGPImage; const APoints: array of TdxPointF); overload;
    procedure Draw(AGraphic: TdxGPImage; const APoints: array of TdxPointF; AAttributes: TdxGPImageAttributes); overload;
    procedure DrawBitmap(ABitmap: TBitmap; const R: TRect; AAlpha: Byte = 255); // deprecated
    procedure DrawTile(AGraphic: TdxGPImage; const R: TRect; AAlpha: Byte = 255); overload;
    procedure DrawTile(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TRect; AAlpha: Byte = 255); overload;

    // Clipping
    function IsClipEmpty: Boolean;
    procedure RestoreClipRegion;
    procedure SaveClipRegion;
    procedure SetClipPath(APath: TdxGPPath; AMode: TdxGPCombineMode);
    procedure SetClipRect(R: TRect; AMode: TdxGPCombineMode); overload;
    procedure SetClipRect(const R: TdxRectF; AMode: TdxGPCombineMode); overload;
    procedure SetClipRegion(ARgn: HRGN; AMode: TdxGPCombineMode); overload;
    procedure SetClipRegion(ARegion: TdxGPRegion; AMode: TdxGPCombineMode); overload;

    // Arc
    procedure Arc(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte); overload;
    procedure Arc(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;

    // Curve
    procedure Curve(const APoints: array of TPoint; APenColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte); overload;
    procedure Curve(const APoints: array of TPoint; APenColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Curve(const APoints: array of TdxPointF; APenColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte); overload;

    // Ellipse
    procedure Ellipse(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Ellipse(R: TdxRectF; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Ellipse(R: TdxRectF; APenColor, ABrushColor: TColor; APenWidth: Single;
      APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;
    procedure Ellipse(R: TRect; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Ellipse(R: TdxRectF; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Ellipse(R: TRect; APenColor, ABrushColor: TColor; APenWidth: Single;
      APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;

    // Line
    procedure Line(X1, Y1, X2, Y2: Integer; APenColor: TColor; APenWidth: Single;
      APenStyle: TPenStyle; APenColorAlpha: Byte); overload;
    procedure Line(X1, Y1, X2, Y2: Integer; APenColor: TdxAlphaColor; APenWidth: Single = 1;
      APenStyle: TPenStyle = psSolid); overload;
    procedure Line(X1, Y1, X2, Y2: Single; APenColor: TdxAlphaColor; APenWidth: Single = 1;
      APenStyle: TPenStyle = psSolid); overload;
    procedure Line(X1, Y1, X2, Y2: Integer; APen: TdxGPPen); overload;
    procedure Line(X1, Y1, X2, Y2: Single; APen: TdxGPPen); overload;

    // Path
    procedure Path(APath: TdxGPPath; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Path(APath: TdxGPPath; APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single = 1;
      APenStyle: TPenStyle = psSolid); overload;
    procedure Path(APath: TdxGPPath; APenColor, ABrushColor: TColor; APenWidth: Single;
      APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;

    // Pie
    procedure Pie(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TColor;
      ABrushColor: TColor; APenWidth: Single; APenStyle: TPenStyle;
      APenColorAlpha, ABrushColorAlpha: Byte); overload;
    procedure Pie(R: TRect; AStartAngle, ASweepAngle: Single; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Pie(R: TdxRectF; AStartAngle, ASweepAngle: Single; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;

    // Polygon
    procedure Polygon(const APoints: array of TPoint; APenColor, ABrushColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;
    procedure Polygon(const APoints: array of TPoint; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Polygon(const APoints: array of TdxPointF; APenColor, ABrushColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;
    procedure Polygon(const APoints: array of TdxPointF; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single; APenStyle: TdxGPPenStyle); overload;
    procedure Polygon(const APoints: array of TdxPointF; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Polygon(const APoints: array of TPoint; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;

    // Polyline
    procedure Polyline(const APoints: array of TPoint; APenColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte); overload;
    procedure Polyline(const APoints: array of TPoint; APenColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Polyline(const APoints: array of TdxPointF; APenColor: TColor;
      APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte); overload;
    procedure Polyline(const APoints: array of TdxPointF; APenColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Polyline(const APoints: array of TdxPointF; APen: TdxGPPen); overload;
    procedure Polyline(const APoints: array of TPoint; APen: TdxGPPen); overload;

    // Rectangle
    procedure FillRectangle(const R: TdxRectF; ABrushColor: TdxAlphaColor); overload;
    procedure FillRectangle(const R: TRect; ABrushColor: TdxAlphaColor); overload;
    procedure FillRectangle(const R: TdxRectF; ABrush: TdxGPCustomBrush); overload;
    procedure FillRectangle(const R: TRect; ABrush: TdxGPCustomBrush); overload;
    procedure FillRectangleByGradient(const R: TRect; AColor1, AColor2: TdxAlphaColor; AMode: TdxGPLinearGradientMode);
    procedure Rectangle(const R: TdxRectF; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Rectangle(const R: TdxRectF; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Rectangle(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush); overload;
    procedure Rectangle(R: TRect; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid); overload;
    procedure Rectangle(R: TRect; APenColor, ABrushColor: TColor; APenWidth: Single;
      APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte); overload;

    // Region
    procedure Region(ARegion: TdxGPRegion; APenColor, ABrushColor: TdxAlphaColor;
      APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);

    // RoundRect
    procedure RoundRect(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush; ARadiusX, ARadiusY: Integer); overload;
    procedure RoundRect(R: TRect; APenColor, ABrushColor: TColor; ARadiusX, ARadiusY: Integer;
      APenWidth: Integer; APenColorAlpha, ABrushColorAlpha: Byte); overload;
    procedure RoundRect(R: TRect; APenColor, ABrushColor: TdxAlphaColor; ARadiusX, ARadiusY: Integer;
      APenWidth: Single = 1); overload;

    // text
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; X, Y: Single); overload;
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; const ARect: TdxRectF); overload;
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; const APoint: TdxPointF); overload;
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; X, Y: Single; AFormat: TdxGPStringFormat); overload;
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; const APoint: TdxPointF; AFormat: TdxGPStringFormat); overload;
    procedure DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; const ARect: TdxRectF; AFormat: TdxGPStringFormat); overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF; AStringFormat: TdxGPStringFormat; out ACharactersFitted, ALinesFilled: Integer): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const AOrigin: TdxPointF; AStringFormat: TdxGPStringFormat): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF; AStringFormat: TdxGPStringFormat): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const AWidth: Double): TdxSizeF; overload;
    function MeasureString(const AText: string; AFont: TdxGPFont; const AWidth: Double; AFormat: TdxGPStringFormat): TdxSizeF; overload;
    function MeasureCharacterRanges(const AText: string; AFont: TdxGPFont; const ALayoutRect: TdxRectF; AStringFormat: TdxGPStringFormat): TArray<TdxGPRegion>;

    // World Transform
    procedure FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF); overload; inline;
    procedure FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TPoint); overload; inline;
    procedure FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single); overload;
    function GetWorldTransform: TdxGPMatrix;
    procedure ModifyWorldTransform(AMatrix: TdxGPMatrix; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
    procedure RotateWorldTransform(AAngle: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure RotateWorldTransform(AAngle: Single; const APivotPoint: TPoint; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure RotateWorldTransform(AAngle: Single; const APivotPoint: TdxPointF; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure ScaleWorldTransform(AScaleX, AScaleY: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure ScaleWorldTransform(AScaleX, AScaleY: Single; const ACenter: TdxPointF; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend); overload;
    procedure SetWorldTransform(AMatrix: TdxGPMatrix);
    procedure TranslateWorldTransform(AOffsetX, AOffsetY: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);

    procedure TransformPoints(ADestSpace, ASrcSpace: TdxGPCoordinateSpace; var APoints: TArray<TdxPointF>); overload;
    procedure TransformPoints(ADestSpace, ASrcSpace: TdxGPCoordinateSpace; var APoints: TArray<TPoint>); overload;
    //
    procedure ResetWorldTransform;
    procedure RestoreWorldTransform;
    procedure SaveWorldTransform;

    property DpiX: Single read GetDpiX;
    property DpiY: Single read GetDpiY;
    property Handle: GpGraphics read FHandle;
    property InterpolationMode: TdxGPInterpolationMode read GetInterpolationMode write SetInterpolationMode;
    property IsLowColorsMode: Boolean read FIsLowColorsMode;
    property PixelOffsetMode: TdxGpPixelOffsetMode read GetPixelOffsetMode write SetPixelOffsetMode;
    property SmoothingMode: TdxGPSmoothingMode read GetSmoothingMode write SetSmoothingMode;
    property TextRenderingHint: TdxGpTextRenderingHint read GetTextRenderingHint write SetTextRenderingHint;
  end;

  TdxGPGraphics = class(TdxGPCanvas);

  { TdxGPCustomPaintCanvas }

  TdxGPCustomPaintCanvas = class(TdxGPGraphics)
  strict private
    FLock: TCriticalSection;

    procedure CreateBuffer(DC: HDC; const R: TRect);
    procedure FreeBuffer;
    procedure OutputBuffer;
  protected
    FBuffer: TBitmap;
    FDrawDC: HDC;
    FDrawRect: TRect;

    procedure SaveState; virtual;
    procedure RestoreState; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginPaint(DC: HDC; const R: TRect); overload;
    procedure BeginPaint(AHandle: GpGraphics); overload;
    procedure EndPaint;
  end;

  { TdxGPPaintCanvas }

  TdxGPPaintCanvas = class(TdxGPCustomPaintCanvas)
  strict private type
  {$REGION 'Internal types'}
    TdxGPPaintCanvasState = record
      Handle: GpGraphics;
      Buffer: TBitmap;
      DrawRect: TRect;
      DC: HDC;
    end;
    TdxGPPaintCanvasStates = array of TdxGPPaintCanvasState;
  {$ENDREGION}
  strict private
    FCounter: Integer;
    FSavedStates: TdxGPPaintCanvasStates;

    procedure SetCapacity(AValue: Integer);
  protected
    procedure SaveState; override;
    procedure RestoreState; override;
  public
    destructor Destroy; override;
  end;

  { TdxGPStreamAdapter }

  TdxGPStreamAdapter = class(TStreamAdapter)
  public
  {$IFDEF DELPHI22}
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; override; stdcall;
  {$ELSE}
    function Stat(out StatStg: TStatStg; StatFlag: Integer): HRESULT; override; stdcall;
  {$ENDIF}
  end;

  { IdxGPGraphicDraw }

  IdxGPGraphicDraw = interface
  ['{12CC0C8A-2C68-4467-AF5F-EBA89E93E1CD}']
    procedure Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TdxRectF; AAttributes: TdxGPImageAttributes); overload;
    procedure Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TRect; AAttributes: TdxGPImageAttributes); overload;
  end;

  { IdxGpFlipContent }

  IdxGpFlipContent = interface
  ['{011BADAA-55B0-4493-9539-5F0811018C00}']
    procedure Flip(AHorizontally, AVertically: Boolean);
  end;

  { TdxGPImageHandle }

  TdxGPImageHandle = class(TdxSmartImageCustomHandle,
    IdxAnimatedImage,
    IdxGpFlipContent,
    IdxGPGraphicDraw,
    IdxImageDataFormat)
  strict private
    FActiveFrame: Cardinal;
    FDimensionID: TGUID;
    FHandle: GpImage;
    FImageFormat: TdxImageDataFormat;

    function GetPropertyValue(APropID: Cardinal; var AData: TdxGPImagePropertyData): Boolean;
    procedure SetHandle(AValue: GpImage);
  protected
    procedure FreeHandle;
    function GetSize: TSize; override;
    procedure SetSize(const AValue: TSize); override;
    // IdxImageDataFormat
    function GetImageFormat: TdxImageDataFormat;
    // IdxAnimatedImage
    function GetActiveFrame: Cardinal;
    function GetAnimationFrameCount: Cardinal;
    function GetAnimationFrameDelay: Integer;
    function GetAnimationLoopCount: Integer;
    procedure SetActiveFrame(AValue: Cardinal);
  public
    constructor Create(AHandle: GpImage);
    constructor CreateFromBits(AWidth, AHeight: Integer; const ABitsRef: TRGBColors; AAlphaFormat: TAlphaFormat = afPremultiplied);
    constructor CreateSize(AWidth, AHeight: Integer); overload;
    constructor CreateSize(const ASize: TSize); overload;
    destructor Destroy; override;
    function Clone: TdxGPImageHandle;
    function CreateCanvas: TdxGpCanvas;
    procedure Draw(DC: HDC; const ADest, ASource: TdxRectF; AAlpha: Byte = 255; APalette: IdxColorPalette = nil); override;
    procedure Draw(DC: HDC; const ADest, ASource: TRect; AAlpha: Byte = 255; APalette: IdxColorPalette = nil); override;
    procedure Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TdxRectF; AAttributes: TdxGPImageAttributes); overload;
    procedure Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TRect; AAttributes: TdxGPImageAttributes); overload;
    // IdxGpFlipContent
    procedure Flip(AHorizontally, AVertically: Boolean);
    function GetAsBitmap: TBitmap; override;
    function IsMetafile: Boolean;
    procedure Resize(const ASize: TSize;
      AInterpolationMode: TdxGPInterpolationMode = imDefault;
      APixelOffsetMode: TdxGpPixelOffsetMode = PixelOffsetModeDefault);
    //
    property ActiveFrame: Cardinal read GetActiveFrame write SetActiveFrame;
    property AnimationFrameCount: Cardinal read GetAnimationFrameCount;
    property AnimationFrameDelay: Integer read GetAnimationFrameDelay;
    property AnimationLoopCount: Integer read GetAnimationLoopCount;
    property Handle: GpImage read FHandle write SetHandle;
    property ImageDataFormat: TdxImageDataFormat read FImageFormat;
  end;

  { TdxGPImageCodec }

  TdxGPImageCodec = class(TdxSmartImageCodec)
  protected
    class function CheckHeader(AStream: TStream; const AHeader: AnsiString): Boolean;
    class function ReadInteger(AStream: TStream): Integer; inline;
    class function ReadWord(AStream: TStream): Word; inline;

    class function GetCodecID: TGUID;
    class function SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer = nil): Boolean; virtual;
  public
    class function CanSaveImage(AHandle: TdxSmartImageCustomHandle): Boolean; override;
    class function Load(AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean; override;
    class function Save(AStream: TStream; AHandle: TdxSmartImageCustomHandle): Boolean; override;
  end;

  { TdxGPImageCodecBMP }

  TdxGPImageCodecBMP = class(TdxGPImageCodec)
  public
    class function Ext: string; override;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; override;
    class function ID: TdxImageDataFormat; override;
    class function CanLoadFromBits: Boolean; override;
    class function CanLoadStream(AStream: TStream): Boolean; override;
    class function Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
      AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean; override;
  end;

  { TdxGPImageCodecGIF }

  TdxGPImageCodecGIF = class(TdxGPImageCodec)
  public
    class function CanLoadStream(AStream: TStream): Boolean; override;
    class function Ext: string; override;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPImageCodecJPEG }

  TdxGPImageCodecJPEG = class(TdxGPImageCodec)
  public const
    DefaultQuality = Cardinal(-1);
  strict private
    class var FQuality: Cardinal;

    class constructor Initialize;
    class procedure SetQuality(const Value: Cardinal); static;
  protected
    class function SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer = nil): Boolean; override;
  public
    class function CanLoadStream(AStream: TStream): Boolean; override;
    class function Ext: string; override;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; override;
    class function ID: TdxImageDataFormat; override;

    class property Quality: Cardinal read FQuality write SetQuality;
  end;

  { TdxGPImageCodecJPG }

  TdxGPImageCodecJPG = class(TdxGPImageCodecJPEG)
  public
    class function Description: string; override;
    class function Ext: string; override;
  end;

  { TdxGPImageCodecPNG }

  TdxGPImageCodecPNG = class(TdxGPImageCodec)
  public
    class function CanLoadStream(AStream: TStream): Boolean; override;
    class function Ext: string; override;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPImageCodecTIFF }

  TdxGPImageCodecTIFF = class(TdxGPImageCodec)
  strict private const
    TIFFHeader1 = #$49#$49#$2A#$00;
    TIFFHeader2 = #$4D#$4D#$00#$2A;
  public
    class function CanLoadStream(AStream: TStream): Boolean; override;
    class function Ext: string; override;
    class function GetSize(AStream: TStream; out ASize: TSize): Boolean; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPImageCodecTIF }

  TdxGPImageCodecTIF = class(TdxGPImageCodecTIFF)
  public
    class function Description: string; override;
    class function Ext: string; override;
  end;

  { TdxGPImageCodecEMF }

  TdxGPImageCodecEMF = class(TdxGPImageCodec)
  protected
    class function SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer = nil): Boolean; override;
  public
    class function Ext: string; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPImageCodecWMF }

  TdxGPImageCodecWMF = class(TdxGPImageCodecEMF)
  public
    class function Ext: string; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPImageAttributes }

  TdxGPImageAttributes = class(TdxGpBase)
  strict private
    FHandle: GpImageAttributes;

    constructor Create(const AHandle: GpImageAttributes); overload;
  protected
    property Handle: GpImageAttributes read FHandle;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure ClearBrushRemapTable;
    procedure ClearColorKey(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearColorMatrix(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearGamma(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearNoOp(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearOutputChannel(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearOutputChannelColorProfile(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearRemapTable(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure ClearThreshold(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    function Clone: TdxGPImageAttributes;
    procedure GetAdjustedPalette(APalette: TGpColorPalette; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetBrushRemapTable(const AMap: array of TGpColorMap);
    procedure SetColorKey(const AColorLow, AColorHigh: TdxAlphaColor;
      const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetColorMatrices(const ANewColorMatrix, AGrayMatrix: TdxGpColorMatrix;
      const AMode: TdxGPColorMatrixFlags = ColorMatrixFlagsDefault; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetColorMatrix(ANewColorMatrix: PdxGpColorMatrix;
      const AMode: TdxGPColorMatrixFlags = ColorMatrixFlagsDefault; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetGamma(AGamma: Single; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetNoOp(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetOutputChannel(const AFlags: TdxGpColorChannelFlags; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetOutputChannelColorProfile(const AColorProfileFilename: string; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetRemapTable(const AMap: array of TGpColorMap; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetThreshold(AThreshold: Single; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
    procedure SetWrapMode(const AMode: TdxGpWrapMode); overload;
    procedure SetWrapMode(const AMode: TdxGpWrapMode; AColor: TdxAlphaColor; AClamp: Boolean = False); overload;
  end;

  { TdxGPImage }

  TdxGPImage = class(TdxCustomSmartImage)
  strict private
    function GetHandle: GpImage;
    function GetHandleAsObject: TdxSmartImageCustomHandle; inline;
    procedure SetHandle(const Value: GpImage);
    procedure SetHandleAsObject(const Value: TdxSmartImageCustomHandle);
  protected
    function CreateCache(const ASize: TSize): TdxSmartImageCustomHandle; override;

    property HandleAsObject: TdxSmartImageCustomHandle read GetHandleAsObject write SetHandleAsObject;
  public
    function Clone: TdxGPImage;
    function CreateCanvas: TdxGPCanvas;
    procedure Flip(AHorizontally, AVertically: Boolean);
    function MakeComposition(AOverlayImage: TdxGPImage; AOverlayAlpha: Byte): TdxGPImage; overload;
    function MakeComposition(AOverlayImage: TdxGPImage; AOverlayAlpha, ASourceAlpha: Byte): TdxGPImage; overload;
    procedure StretchDraw(ACanvas: TdxGPCanvas; const ADest, ASource: TdxRectF;
      AAttributes: TdxGPImageAttributes; AColorPalette: IdxColorPalette = nil); overload;
    procedure StretchDraw(ACanvas: TdxGPCanvas; const ADest, ASource: TRect;
      AAttributes: TdxGPImageAttributes; AColorPalette: IdxColorPalette = nil); overload;

    property Handle: GpImage read GetHandle write SetHandle;
  end;

  { TdxSmartImage }

  TdxSmartImage = class(TdxGPImage);

  { TdxSmartGlyph }

  TdxSmartGlyph = class(TdxSmartImage,
    IdxSourceDPI,
    IdxSourceSize)
  strict private
    FSourceDPI: Integer;
    FSourceHeight: Integer;
    FSourceWidth: Integer;

    function GetSourceHeight: Integer;
    function GetSourceWidth: Integer;
    function IsSourceDPIStored: Boolean;
    function IsSourceHeightStored: Boolean;
    function IsSourceWidthStored: Boolean;
    procedure SetSourceHeight(AValue: Integer);
    procedure SetSourceWidth(AValue: Integer);
    // IdxSourceDPI
    function GetSourceDPI: Integer;
    procedure SetSourceDPI(AValue: Integer);
    // IdxSourceSize
    function GetSourceSize: TSize;
  protected
    FTransparent: Boolean;

    procedure AssignFromSmartImage(AImage: TdxCustomSmartImage); override;
    procedure CreateHandleFromBitmap(ABitmap: TBitmap); override;
    function IsBitmapStream(AStream: TStream): Boolean; override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
  published
    property SourceDPI: Integer read FSourceDPI write SetSourceDPI stored IsSourceDPIStored;
    property SourceHeight: Integer read GetSourceHeight write SetSourceHeight stored IsSourceHeightStored;
    property SourceWidth: Integer read GetSourceWidth write SetSourceWidth stored IsSourceWidthStored;
  end;

  { TdxPNGImage }

  TdxPNGImage = class(TdxSmartImage)
  public
    procedure SaveToStream(AStream: TStream); override;
  end;

  { TdxJPEGImage }

  TdxJPEGImage = class(TdxSmartImage)
  strict private
    FQuality: Cardinal;

    procedure SetQuality(AValue: Cardinal);
  public
    constructor Create; override;
    procedure SaveToStream(AStream: TStream); override;
    //
    property Quality: Cardinal read FQuality write SetQuality;
  end;

  { TdxGIFImage }

  TdxGIFImage = class(TdxSmartImage)
  public
    procedure SaveToStream(AStream: TStream); override;
  end;

  { TdxTIFFImage }

  TdxTIFFImage = class(TdxSmartImage)
  public
    procedure SaveToStream(AStream: TStream); override;
  end;

  { TdxBMPImage }

  TdxBMPImage = class(TdxSmartImage)
  public
    procedure SaveToStream(AStream: TStream); override;
  end;

const
  dxGpAlignmentToStringAlignment: array[TAlignment] of TdxGpStringAlignment = (
    StringAlignmentNear, StringAlignmentFar, StringAlignmentCenter
  );
  dxGpVerticalAlignmentToLineAlignment: array[TVerticalAlignment] of TdxGpStringAlignment = (
    StringAlignmentNear, StringAlignmentFar, StringAlignmentCenter
  );
  dxGpSmoothStretchModeMap: array[Boolean] of TdxGPInterpolationMode = (imNearestNeighbor, imHighQualityBicubic);
  dxGpWordWrapFlagsMap: array[Boolean] of TdxGPStringFormatFlags = (StringFormatFlagsNoWrap, StringFormatFlagsNone);

function dxGpIsDoubleBufferedNeeded(DC: HDC): Boolean;
function dxGpIsRectVisible(AGraphics: GpGraphics; const R: TdxRectF): LongBool; overload;
function dxGpIsRectVisible(AGraphics: GpGraphics; const R: TRect): LongBool; overload;

function dxGetNearestGradientMode(const AAngle: Double; out AInverseOrder: Boolean): TdxGPBrushGradientMode;

procedure dxGpDrawImage(AGraphics: GpGraphics; const ADestRect: TRect;
  const ASourceRect: TRect; AImage: GpImage; AAlpha: Byte = 255; AAttributes: GpImageAttributes = nil); overload;
procedure dxGpDrawImage(AGraphics: GpGraphics; const ADestRect: TdxRectF;
  const ASourceRect: TdxRectF; AImage: GpImage; AAlpha: Byte = 255; AAttributes: GpImageAttributes = nil); overload;
procedure dxGpRightToLeftDependentDraw(AGPCanvas: TdxGPCanvas; const R: TRect; AIsRightToLeftLayout: Boolean; AProc: TProc);

function dxFontStylesToGpFontStyles(AStyles: TFontStyles): Integer;
procedure dxGPDrawText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TRect; AFont: TFont; ATextColor: TdxAlphaColor;
  AHorzAlignment: TAlignment = taLeftJustify; AVertAlignment: TVerticalAlignment = taVerticalCenter;
  AWordWrap: Boolean = False; ARendering: TdxGpTextRenderingHint = TextRenderingHintSystemDefault;
  ATrimming: TdxGpStringTrimming = StringTrimmingNone; ARtlReading: Boolean = False); overload;
procedure dxGPDrawText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TdxRectF; AFont: TFont; ATextColor: TdxAlphaColor;
  AHorzAlignment: TAlignment = taLeftJustify; AVertAlignment: TVerticalAlignment = taVerticalCenter;
  AWordWrap: Boolean = False; ARendering: TdxGpTextRenderingHint = TextRenderingHintSystemDefault;
  ATrimming: TdxGpStringTrimming = StringTrimmingNone; ARtlReading: Boolean = False); overload;

procedure dxGPDrawGlowText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TRect; AFont: TFont; ATextColor, ATextGlowColor: TdxAlphaColor);

procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AFont: TFont;
  AWordWrap: Boolean; const ALayoutRect: TRect; out ATextRect: TRect); overload;
procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AFont: TFont;
  AWordWrap: Boolean; const ALayoutRect: TdxRectF; out ATextRect: TdxRectF); overload;
procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AGpFont: GpFont;
  AWordWrap: Boolean; const ALayoutRect: TdxRectF; out ATextRect: TdxRectF); overload;

procedure dxGpFillRect(DC: HDC; const R: TRect; AColor: TColor; AColorAlpha: Byte = 255); overload;
procedure dxGpFillRectByGradient(DC: HDC; const R: TRect; AColor1, AColor2: TColor;
  AMode: TdxGPLinearGradientMode; AColor1Alpha: Byte = 255; AColor2Alpha: Byte = 255);
procedure dxGpTilePart(DC: HDC; const ADestRect, ASourceRect: TRect; AImage: GpBitmap);
procedure dxGpTilePartEx(AGraphics: GpGraphics; const ADestRect, ASourceRect: TRect; AImage: GpBitmap; AAlpha: Byte = 255);
procedure dxGpRoundRect(DC: HDC; const R: TRect; APenColor: TColor; ABrushColor: TColor;
  ARadius: Integer; APenWidth: Integer = 1; APenColorAlpha: Byte = 255; ABrushColorAlpha: Byte = 255);

function dxGpBeginPaint(AHandle: GpGraphics): TdxGPGraphics; overload;
function dxGpBeginPaint(DC: HDC; const R: TRect): TdxGPGraphics; overload;
procedure dxGpEndPaint(var AGraphics: TdxGPGraphics);

function dxGetImageDataFormat(const AFormatId: TGUID): TdxImageDataFormat; overload;
function dxGetImageDataFormat(const AImage: GpImage): TdxImageDataFormat; overload;
function dxGetImageDataFormatExtension(AImageDataFormat: TdxImageDataFormat): string;
function dxGetImageEncoder(AImageDataFormat: TdxImageDataFormat): TGUID;
function dxGetImageDecoder(AImageDataFormat: TdxImageDataFormat): TGUID;

function dxGPPaintCanvas: TdxGPPaintCanvas;
implementation

uses
  Math, RTLConsts, dxHash, dxHashUtils;

const
  GpPenStyleToDashStyle: array[TdxGPPenStyle] of TdxGpDashStyle = (
    DashStyleSolid, DashStyleDash, DashStyleDot, DashStyleDashDot, DashStyleDashDotDot
  );
  PenLineStyleToGpLineCap: array[TdxGPPenLineCapStyle] of TdxGpLineCap = (
    LineCapFlat,
    LineCapSquare,
    LineCapRound
  );
  PenDashCapStyleToGpDashCap: array[TdxGPPenDashCapStyle] of TdxGpDashCap = (DashCapFlat, DashCapRound, DashCapTriangle);

type
  TdxSmartImageDataAccess = class(TdxSmartImageData);
  TdxWindowFromDCFunc = function (hDC: HDC): HWND; stdcall;

  { TdxGPHandle }

  TdxGPHandle = class(TInterfacedObject, IdxGPHandle)
  protected
    FHandle: Pointer;
  public
    constructor Create(AHandle: Pointer);
    // IdxGPHandle
    function GetNativeHandle: Pointer;
  end;

  { TdxGPHandleCacheManager }

  TdxGPHandleCacheManager<T> = class(TdxCustomValueCacheManager<T, IdxGPHandle>);

  { TdxGPPenHandle }

  TdxGPPenHandle = class(TdxGPHandle)
  public
    destructor Destroy; override;
  end;

  { TdxGPBrushHandle }

  TdxGPBrushHandle = class(TdxGPHandle)
  public
    destructor Destroy; override;
  end;

  { TdxGPSolidPenCacheID }

  TdxGPSolidPenCacheID = record
    Color: TdxAlphaColor;
    Style: DashStyle;
    Width: Single;

    constructor Create(AColor: TdxAlphaColor; AWidth: Single; AStyle: DashStyle);
  end;

  { TdxGPSolidPenCacheManager }

  TdxGPSolidPenCacheManager = class(TdxGPHandleCacheManager<TdxGPSolidPenCacheID>)
  protected
    function Compare(const Key1, Key2: TdxGPSolidPenCacheID): Integer; override;
    function CreateValue(const ID: TdxGPSolidPenCacheID): IdxGPHandle; override;
  end;

  { TdxGPSolidBrushCacheManager }

  TdxGPSolidBrushCacheManager = class(TdxGPHandleCacheManager<TdxAlphaColor>)
  protected
    function Compare(const Key1, Key2: TdxAlphaColor): Integer; override;
    function CreateValue(const ID: TdxAlphaColor): IdxGPHandle; override;
  end;

  { TdxGPResourceManager }

  TdxGPResourceManager = class
  strict private const
    CacheSize = 100;
  strict private
    class var FSolidBrushes: TdxGPSolidBrushCacheManager;
    class var FSolidPens: TdxGPSolidPenCacheManager;
  protected
    class procedure Finalize;
    class procedure Initialize;
  public
    class function GetPen(AColor: TColor; AColorAlpha: Byte; AWidth: Single; AStyle: TPenStyle; out AHandle: IdxGPHandle): Boolean; overload;
    class function GetPen(AColor: TdxAlphaColor; AWidth: Single; AStyle: DashStyle; out AHandle: IdxGPHandle): Boolean; overload;
    class function GetPen(AColor: TdxAlphaColor; AWidth: Single; AStyle: TdxGPPenStyle; out AHandle: IdxGPHandle): Boolean; overload;
    class function GetPen(AColor: TdxAlphaColor; AWidth: Single; AStyle: TPenStyle; out AHandle: IdxGPHandle): Boolean; overload;
    class function GetSolidBrush(const AColor: TColor; AColorAlpha: Byte; out AHandle: IdxGPHandle): Boolean; overload;
    class function GetSolidBrush(const AColor: TdxAlphaColor; out AHandle: IdxGPHandle): Boolean; overload;
  end;

  { TdxGPImageCodecMemoryBitmap }

  TdxGPImageCodecMemoryBitmap = class(TdxGPImageCodecBMP)
  public
    class function Description: string; override;
    class function ID: TdxImageDataFormat; override;
  end;

  { TdxGPAlphaBlendAttributes }

  TdxGPAlphaBlendAttributes = class(TdxGPImageAttributes)
  strict private
    FAlpha: Byte;
    FColorMatrix: TdxGPColorMatrix;

    procedure SetAlpha(AValue: Byte);
  public
    constructor Create;
    //
    property Alpha: Byte read FAlpha write SetAlpha;
  end;

var
  FWindowFromDC: TdxWindowFromDCFunc;
  GPAlphaBlendAttributes: TdxGPAlphaBlendAttributes;
  GPPaintCanvas: TdxGPPaintCanvas;

function dxWindowFromDC(DC: HDC): HWND;
begin
  if Assigned(FWindowFromDC) then
    Result := FWindowFromDC(DC)
  else
    Result := 0;
end;

function dxGpGetAlphaBlendAttributes(const AAlpha: Byte): TdxGPAlphaBlendAttributes;
begin
  if AAlpha = 255 then
    Exit(nil);

  if GPAlphaBlendAttributes = nil then
    GPAlphaBlendAttributes := TdxGPAlphaBlendAttributes.Create;
  GPAlphaBlendAttributes.Alpha := AAlpha;
  Result := GPAlphaBlendAttributes;
end;

function dxGpBeginPaint(AHandle: GpGraphics): TdxGPGraphics;
begin
  Result := TdxGPCustomPaintCanvas.Create(nil);
  TdxGPCustomPaintCanvas(Result).BeginPaint(AHandle);
end;

function dxGpBeginPaint(DC: HDC; const R: TRect): TdxGPGraphics;
begin
  Result := TdxGPCustomPaintCanvas.Create(nil);
  TdxGPCustomPaintCanvas(Result).BeginPaint(DC, R);
end;

procedure dxGpEndPaint(var AGraphics: TdxGPGraphics);
begin
  (AGraphics as TdxGPCustomPaintCanvas).EndPaint;
  FreeAndNil(AGraphics);
end;

function dxGpGetImageAttributesHandle(AAttrs: TdxGPImageAttributes): GpImageAttributes;
begin
  if AAttrs <> nil then
    Result := AAttrs.Handle
  else
    Result := nil;
end;

function dxGpGetCanvasHandle(ACanvas: TdxGPCanvas): GpGraphics;
begin
  if ACanvas <> nil then
    Result := ACanvas.Handle
  else
    Result := nil;
end;

function dxGetImageDataFormat(const AImage: GpImage): TdxImageDataFormat; overload;
var
  AFormatID: TGUID;
begin
  Result := dxImageUnknown;
  if AImage <> nil then
  begin
    if GdipGetImageRawFormat(AImage, AFormatID) = Ok then
      Result := dxGetImageDataFormat(AFormatID);
  end;
end;

function dxGetImageDataFormat(const AFormatId: TGUID): TdxImageDataFormat;
begin
  if IsEqualGUID(AFormatId, ImageFormatBMP) then
    Result := dxImageBitmap
  else

  if IsEqualGUID(AFormatId, ImageFormatMemoryBMP) then
    Result := dxImageMemoryBmp
  else

  if IsEqualGUID(AFormatId, ImageFormatJPEG) then
    Result := dxImageJpeg
  else

  if IsEqualGUID(AFormatId, ImageFormatPNG) then
    Result := dxImagePng
  else

  if IsEqualGUID(AFormatId, ImageFormatTIFF) then
    Result := dxImageTiff
  else

  if IsEqualGUID(AFormatId, ImageFormatGIF) then
    Result := dxImageGif
  else

  if IsEqualGUID(AFormatId, ImageFormatIcon) then
    Result := dxImageIcon
  else

  if IsEqualGUID(AFormatId, ImageFormatEMF) then
    Result := dxImageEmf
  else

  if IsEqualGUID(AFormatId, ImageFormatWMF) then
    Result := dxImageWmf

  else
    Result := dxImageUnknown;
end;

function dxGetImageDataFormatExtension(AImageDataFormat: TdxImageDataFormat): string;
const
  Map: array[TdxImageDataFormat] of string = ('', '.bmp', '.jpg', '.png', '.tif', '.gif', '.emf', '', '.ico', '', '.wmf');
begin
  Result := Map[AImageDataFormat];
end;

function dxGetImageEncoder(AImageDataFormat: TdxImageDataFormat): TGUID;
begin
  case AImageDataFormat of
    dxImageBitmap, dxImageMemoryBmp:
      Result := BMPEncoder;
    dxImageJpeg:
      Result := JPEGEncoder;
    dxImagePng:
      Result := PNGEncoder;
    dxImageTiff:
      Result := TIFFEncoder;
    dxImageGif:
      Result := GIFEncoder;
    dxImageEmf, dxImageIcon, dxImageWmf:
      ZeroMemory(@Result, SizeOf(Result));
  else
    raise EdxException.Create('dxGetImageEncoder fails');
  end;
end;

function dxGetImageDecoder(AImageDataFormat: TdxImageDataFormat): TGUID;
begin
  case AImageDataFormat of
    dxImageBitmap, dxImageMemoryBmp:
      Result := BMPDecoder;
    dxImageJpeg:
      Result := JPEGDecoder;
    dxImagePng:
      Result := PNGDecoder;
    dxImageTiff:
      Result := TIFFDecoder;
    dxImageGif:
      Result := GIFDecoder;
    dxImageEmf:
      Result := EMFDecoder;
    dxImageIcon:
      Result := ICONDecoder;
    dxImageWmf:
      Result := WMFDecoder;
  else
    raise EdxException.Create('dxGetImageDecoder fails');
  end;
end;

procedure dxGpDrawImage(AGraphics: GpGraphics; const ADestRect: TRect;
  const ASourceRect: TRect; AImage: GpImage; AAlpha: Byte = 255; AAttributes: GpImageAttributes = nil);
begin
  dxGpDrawImage(AGraphics, dxRectF(ADestRect), dxRectF(ASourceRect), AImage, AAlpha, AAttributes);
end;

procedure dxGpDrawImage(AGraphics: GpGraphics; const ADestRect: TdxRectF;
  const ASourceRect: TdxRectF; AImage: GpImage; AAlpha: Byte = 255; AAttributes: GpImageAttributes = nil);
var
  ADstH, ADstW, ASrcH, ASrcW: Single;
  APixelOffsetMode: TdxGpPixelOffsetMode;
  ARegion: GpRegion;
  AType: TdxGPImageType;
begin
  ARegion := nil;
  if dxGpIsRectVisible(AGraphics, ADestRect) then
  try
    ASrcW := ASourceRect.Right - ASourceRect.Left;
    ASrcH := ASourceRect.Bottom - ASourceRect.Top;
    ADstW := ADestRect.Right - ADestRect.Left;
    ADstH := ADestRect.Bottom - ADestRect.Top;

    if (ASrcW < 1) or (ASrcH < 1) or (ADstW < 1) or (ADstH < 1) then
      Exit;

    GdipCheck(GdipGetImageType(AImage, AType));
    if AType = ImageTypeMetafile then
    begin
      if (ADstW < 2) or (ADstH < 2) then
      begin
        GdipCheck(GdipCreateRegion(ARegion));
        GdipCheck(GdipGetClip(AGraphics, ARegion));
        GdipCheck(GdipSetClipRect(AGraphics, ADestRect.Left, ADestRect.Top, ADstW, ADstH, CombineModeIntersect));
        ADstH := Max(ADstH, 2);
        ADstW := Max(ADstW, 2);
      end;
    end
    else
    begin
      GdipCheck(GdipGetPixelOffsetMode(AGraphics, APixelOffsetMode));
      if APixelOffsetMode <> PixelOffsetModeHalf then
      begin
        if (ASrcW > 1) and (ADstW > ASrcW) then
          ASrcW := ASrcW - 1;
        if (ASrcH > 1) and (ADstH > ASrcH) then
          ASrcH := ASrcH - 1;
      end;
    end;

    if (AAttributes = nil) and (AAlpha < 255) then
      AAttributes := dxGpGetAlphaBlendAttributes(AAlpha).Handle;
    GdipCheck(GdipDrawImageRectRect(AGraphics, AImage, ADestRect.Left,
      ADestRect.Top, ADstW, ADstH, ASourceRect.Left, ASourceRect.Top, ASrcW, ASrcH,
      guPixel, AAttributes, nil, nil));
  finally
    if ARegion <> nil then
    begin
      GdipCheck(GdipSetClipRegion(AGraphics, ARegion, CombineModeReplace));
      GdipCheck(GdipDeleteRegion(ARegion));
    end;
  end;
end;

procedure dxGpRightToLeftDependentDraw(AGPCanvas: TdxGPCanvas; const R: TRect; AIsRightToLeftLayout: Boolean; AProc: TProc);
var
  APrevPixelOffsetMode: TdxGpPixelOffsetMode;
begin
  if AIsRightToLeftLayout then
  begin
    AGPCanvas.SaveWorldTransform;
    try
      AGPCanvas.FlipWorldTransform(True, False, (R.Left + R.Right) / 2, 0);
      APrevPixelOffsetMode := dxGPPaintCanvas.PixelOffsetMode;
      try
        AGPCanvas.PixelOffsetMode := PixelOffsetModeHalf;
        AProc;
      finally
        AGPCanvas.PixelOffsetMode := APrevPixelOffsetMode;
      end;
    finally
      AGPCanvas.RestoreWorldTransform;
    end;
  end
  else
    AProc;
end;

function dxFontStylesToGpFontStyles(AStyles: TFontStyles): Integer;
const
  Map: array[TFontStyle] of TdxGPFontStyle = (FontStyleBold, FontStyleItalic, FontStyleUnderline, FontStyleStrikeout);
var
  AStyle: TFontStyle;
begin
  Result := 0;
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
  begin
    if AStyle in AStyles then
      Result := Result or Ord(Map[AStyle]);
  end;
end;

procedure dxGPDrawText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TRect; AFont: TFont; ATextColor: TdxAlphaColor;
  AHorzAlignment: TAlignment = taLeftJustify; AVertAlignment: TVerticalAlignment = taVerticalCenter;
  AWordWrap: Boolean = False; ARendering: TdxGpTextRenderingHint = TextRenderingHintSystemDefault;
  ATrimming: TdxGpStringTrimming = StringTrimmingNone; ARtlReading: Boolean = False);
begin
  dxGPDrawText(AGraphics, AText, cxRectF(ARect), AFont, ATextColor, AHorzAlignment, AVertAlignment, AWordWrap,
    ARendering, ATrimming, ARtlReading);
end;

procedure dxGPDrawText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TdxRectF; AFont: TFont; ATextColor: TdxAlphaColor;
  AHorzAlignment: TAlignment = taLeftJustify; AVertAlignment: TVerticalAlignment = taVerticalCenter;
  AWordWrap: Boolean = False; ARendering: TdxGpTextRenderingHint = TextRenderingHintSystemDefault;
  ATrimming: TdxGpStringTrimming = StringTrimmingNone; ARtlReading: Boolean = False);
const
  RtlReadingFlag: array [Boolean] of Integer = (0, 1);
var
  ABrush: IdxGPHandle;
  AGpFont: GpFont;
  AGpRectF: TdxGpRectF;
  AStringFormat: TdxGPStringFormat;
begin
  if cxRectIsEmpty(ARect) or (AText = '') then
    Exit;

  if TdxGPResourceManager.GetSolidBrush(ATextColor, ABrush) then
  begin
    AStringFormat := TdxGPStringFormat.Create(Integer(dxGpWordWrapFlagsMap[AWordWrap]) or RtlReadingFlag[ARtlReading]);
    try
      AStringFormat.Trimming := ATrimming;
      AStringFormat.Alignment := dxGpAlignmentToStringAlignment[AHorzAlignment];
      AStringFormat.LineAlignment := dxGpVerticalAlignmentToLineAlignment[AVertAlignment];

      AGpFont := dxGpCreateFont(AFont);
      try
        AGraphics.TextRenderingHint := ARendering;
        AGpRectF := MakeRect(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
        GdipCheck(GdipDrawString(AGraphics.Handle, PWideChar(AText), -1, AGpFont, @AGpRectF, AStringFormat.Handle,
          ABrush.GetNativeHandle));
      finally
        GdipDeleteFont(AGpFont);
      end;
    finally
      AStringFormat.Free;
    end;
  end;
end;

procedure dxGPDrawGlowText(AGraphics: TdxGPCanvas; const AText: string;
  const ARect: TRect; AFont: TFont; ATextColor, ATextGlowColor: TdxAlphaColor);
const
  StrokeWidth = 4;
var
  ABrush: IdxGPHandle;
  APath: TdxGPPath;
  APen: GpPen;
begin
  if cxRectIsEmpty(ARect) or (AText = '') then Exit;

  APath := TdxGPPath.Create;
  try
    APath.AddString(PWideChar((AText)), AFont, -AFont.Height, cxRectSetNullOrigin(ARect));
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.TranslateWorldTransform(ARect.Left, ARect.Top);
    try
      GdipCheck(GdipCreatePen1(ATextGlowColor, StrokeWidth, guPixel, APen));
      GdipCheck(GdipSetPenDashStyle(APen, DashStyleSolid));
      GdipCheck(GdipSetPenLineJoin(APen, LineJoinRound));
      GdipCheck(GdipDrawPath(AGraphics.Handle, APen, APath.Handle));
      GdipCheck(GdipDeletePen(APen));

      if TdxGPResourceManager.GetSolidBrush(ATextColor, ABrush) then
        GdipCheck(GdipFillPath(AGraphics.Handle, ABrush.GetNativeHandle, APath.Handle));
    finally
      AGraphics.TranslateWorldTransform(-ARect.Left, -ARect.Top);
    end;
  finally
    APath.Free;
  end;
end;

procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AFont: TFont;
  AWordWrap: Boolean; const ALayoutRect: TRect; out ATextRect: TRect);
var
  R: TdxRectF;
begin
  dxGPGetTextRect(AGraphics, AText, AFont, AWordWrap, cxRectF(ALayoutRect), R);
  ATextRect := cxRect(Trunc(R.Left), Trunc(R.Top), Ceil(R.Left + cxRectWidth(R)), Ceil(R.Top + cxRectHeight(R)));
end;

procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AFont: TFont;
  AWordWrap: Boolean; const ALayoutRect: TdxRectF; out ATextRect: TdxRectF);
var
  AGpFont: GpFont;
begin
  AGpFont := dxGpCreateFont(AFont);
  try
    dxGPGetTextRect(AGraphics, AText, AGpFont, AWordWrap, ALayoutRect, ATextRect);
  finally
    GdipDeleteFont(AGpFont);
  end;
end;

procedure dxGPGetTextRect(AGraphics: TdxGPGraphics; const AText: string; AGpFont: GpFont;
  AWordWrap: Boolean; const ALayoutRect: TdxRectF; out ATextRect: TdxRectF);
const
  WordWrapMap: array[Boolean] of TdxGPStringFormatFlags = (StringFormatFlagsNoWrap, StringFormatFlagsNone);
var
  ALayoutRectF, ABoundingBoxF: TdxGpRectF;
  AStringFormat: GpStringFormat;
  ACodePointsFitted, ALinesFilled: Integer;
begin
  ALayoutRectF := MakeRect(ALayoutRect.Left, ALayoutRect.Top, cxRectWidth(ALayoutRect), cxRectHeight(ALayoutRect));
  GdipCheck(GdipCreateStringFormat(Integer(WordWrapMap[AWordWrap]), LANG_NEUTRAL, AStringFormat));
  try
    GdipCheck(GdipMeasureString(AGraphics.Handle, PWideChar(AText),
      Length(AText), AGpFont, @ALayoutRectF, AStringFormat, @ABoundingBoxF, ACodePointsFitted, ALinesFilled));
    ATextRect := cxRectF(ABoundingBoxF.X, ABoundingBoxF.Y, ABoundingBoxF.X + ABoundingBoxF.Width,
      ABoundingBoxF.Y + ABoundingBoxF.Height);
  finally
    GdipDeleteStringFormat(AStringFormat);
  end;
end;

procedure dxGpFillRect(DC: HDC; const R: TRect; AColor: TColor; AColorAlpha: Byte = 255);
begin
  dxGPPaintCanvas.BeginPaint(DC, R);
  dxGPPaintCanvas.FillRectangle(R, dxColorToAlphaColor(AColor, AColorAlpha));
  dxGPPaintCanvas.EndPaint;
end;

procedure dxGpFillRectByGradient(DC: HDC; const R: TRect;
  AColor1, AColor2: TColor; AMode: TdxGPLinearGradientMode;
  AColor1Alpha: Byte = 255; AColor2Alpha: Byte = 255);
begin
  if not IsRectEmpty(R) then
  begin
    dxGPPaintCanvas.BeginPaint(DC, R);
    dxGPPaintCanvas.FillRectangleByGradient(R,
      dxColorToAlphaColor(AColor1, AColor1Alpha),
      dxColorToAlphaColor(AColor2, AColor2Alpha), AMode);
    dxGPPaintCanvas.EndPaint;
  end;
end;

function dxGpIsDoubleBufferedNeeded(DC: HDC): Boolean;
var
  AWindowHandle: HWND;
begin
  Result := (GetDeviceCaps(DC, BITSPIXEL) <= 16) or
    (GetDeviceCaps(DC, TECHNOLOGY) = DT_RASPRINTER) or
    (GetDeviceCaps(DC, NUMCOLORS) > 1);

  if not (Result or IsWinVistaOrLater) then
  begin
    AWindowHandle := dxWindowFromDC(DC);
    if AWindowHandle <> 0 then
      Result := GetWindowLong(AWindowHandle, GWL_EXSTYLE) and WS_EX_LAYERED <> 0;
  end;
end;

function dxGpIsRectVisible(AGraphics: GpGraphics; const R: TdxRectF): LongBool;
begin
  GdipCheck(GdipIsVisibleRect(AGraphics, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, Result));
end;

function dxGpIsRectVisible(AGraphics: GpGraphics; const R: TRect): LongBool;
begin
  GdipCheck(GdipIsVisibleRectI(AGraphics, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), Result));
end;

function dxGetNearestGradientMode(const AAngle: Double; out AInverseOrder: Boolean): TdxGPBrushGradientMode;
var
  AIndex: Integer;
begin
  AIndex := Round(dxNormalizeAngle(AAngle) / 45);
  case AIndex of
    1, 5:
      Result := gpbgmForwardDiagonal;
    3, 7:
      Result := gpbgmBackwardDiagonal;
    2, 6:
      Result := gpbgmVertical;
  else
    Result := gpbgmHorizontal;
  end;
  AInverseOrder := InRange(AIndex, 4, 7);
end;

procedure dxGpTilePart(DC: HDC; const ADestRect, ASourceRect: TRect; AImage: GpBitmap);
begin
  dxGPPaintCanvas.BeginPaint(DC, ADestRect);
  dxGpTilePartEx(dxGPPaintCanvas.Handle, ADestRect, ASourceRect, AImage);
  dxGPPaintCanvas.EndPaint;
end;

procedure dxGpTilePartEx(AGraphics: GpGraphics;
  const ADestRect, ASourceRect: TRect; AImage: GpBitmap; AAlpha: Byte = 255);

  function CreateTextureBrush(const R: TRect; out ATexture: GpTexture): Boolean;
  begin
    Result := GdipCreateTexture2I(AImage, WrapModeTile,
      R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), ATexture) = Ok;
  end;

  procedure ManualTilePart(const ADest, ASource: TRect;
    ADestWidth, ADestHeight, ASourceWidth, ASourceHeight: Integer);
  var
    ALastCol, ALastRow, ACol, ARow: Integer;
    RDest, RSrc: TRect;
  begin
    ALastCol := ADestWidth div ASourceWidth - Ord(ADestWidth mod ASourceWidth = 0);
    ALastRow := ADestHeight div ASourceHeight - Ord(ADestHeight mod ASourceHeight = 0);
    for ARow := 0 to ALastRow do
    begin
      RSrc.Top := ASource.Top;
      RSrc.Bottom := ASource.Bottom;
      RDest.Top := ADest.Top + ASourceHeight * ARow;
      RDest.Bottom := RDest.Top + ASourceHeight;
      if RDest.Bottom > ADest.Bottom then
      begin
        Dec(RSrc.Bottom, RDest.Bottom - ADest.Bottom);
        RDest.Bottom := ADest.Bottom;
      end;
      for ACol := 0 to ALastCol do
      begin
        RSrc.Left := ASource.Left;
        RSrc.Right := ASource.Right;
        RDest.Left := ADest.Left + ASourceWidth * ACol;
        RDest.Right := RDest.Left + ASourceWidth;
        if RDest.Right > ADest.Right then
        begin
          Dec(RSrc.Right, RDest.Right - ADest.Right);
          RDest.Right := ADest.Right;
        end;
        dxGpDrawImage(AGraphics, RDest, RSrc, AImage, AAlpha);
      end;
    end;
  end;

  function TilePartByBrush(const R, ASource: TRect): Boolean;
  var
    ABitmap: GpBitmap;
    ABitmapGraphics: GpGraphics;
    ATexture: GpTexture;
    AWidth, AHeight: Integer;
  begin
    Result := CreateTextureBrush(ASource, ATexture);
    if Result then
    try
      AWidth := cxRectWidth(R);
      AHeight := cxRectHeight(R);
      GdipCheck(GdipCreateBitmapFromScan0(AWidth, AHeight, 0, PixelFormat32bppPARGB, nil, ABitmap));
      try
        GdipCheck(GdipGetImageGraphicsContext(ABitmap, ABitmapGraphics));
        GdipCheck(GdipFillRectangleI(ABitmapGraphics, ATexture, 0, 0, AWidth, AHeight));
        GdipCheck(GdipDrawImageRectI(AGraphics, ABitmap, R.Left, R.Top, AWidth, AHeight));
        GdipCheck(GdipDeleteGraphics(ABitmapGraphics));
      finally
        GdipCheck(GdipDisposeImage(ABitmap));
      end;
    finally
      GdipCheck(GdipDeleteBrush(ATexture));
    end;
  end;

var
  ADestWidth, ADestHeight: Integer;
  ASourceWidth, ASourceHeight: Integer;
begin
  if not IsRectEmpty(ASourceRect) and dxGpIsRectVisible(AGraphics, ADestRect) then
  begin
    ADestWidth := cxRectWidth(ADestRect);
    ADestHeight := cxRectHeight(ADestRect);
    ASourceWidth := cxRectWidth(ASourceRect);
    ASourceHeight := cxRectHeight(ASourceRect);
    if (AAlpha <> 255) or (ADestWidth <= ASourceWidth) and (ADestHeight <= ASourceHeight) or
      not TilePartByBrush(ADestRect, ASourceRect)
    then
      ManualTilePart(ADestRect, ASourceRect, ADestWidth, ADestHeight, ASourceWidth, ASourceHeight);
  end;
end;

procedure dxGpRoundRect(DC: HDC; const R: TRect; APenColor: TColor;
  ABrushColor: TColor; ARadius: Integer; APenWidth: Integer = 1;
  APenColorAlpha: Byte = 255; ABrushColorAlpha: Byte = 255);
begin
  dxGPPaintCanvas.BeginPaint(DC, R);
  try
    dxGPPaintCanvas.RoundRect(R, APenColor, ABrushColor,
      ARadius, ARadius, APenWidth, APenColorAlpha, ABrushColorAlpha);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function dxGPPaintCanvas: TdxGPPaintCanvas;
begin
  if GPPaintCanvas = nil then
    GPPaintCanvas := TdxGPPaintCanvas.Create(nil);
  Result := GPPaintCanvas;
end;

{ TdxGPCustomGraphicObject }

constructor TdxGPCustomGraphicObject.Create;
begin
  // do nothing
end;

procedure TdxGPCustomGraphicObject.Assign(ASource: TdxGPCustomGraphicObject);
begin
  // do nothing
end;

procedure TdxGPCustomGraphicObject.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FreeHandle;
end;

procedure TdxGPCustomGraphicObject.FreeHandle;
begin
  if HandleAllocated then
  begin
    DoFreeHandle(FHandle);
    FHandle := nil;
  end;
end;

procedure TdxGPCustomGraphicObject.HandleNeeded;
begin
  if FHandle = nil then
  begin
    Inc(FChangeLockCount);
    try
      DoCreateHandle(FHandle);
      DoHandleCreated;
    finally
      Dec(FChangeLockCount);
    end;
  end;
end;

procedure TdxGPCustomGraphicObject.Changed;
begin
  if FChangeLockCount = 0 then
    dxCallNotify(OnChange, Self);
end;

procedure TdxGPCustomGraphicObject.DoHandleCreated;
begin
  // do nothing
end;

function TdxGPCustomGraphicObject.GetHandle: GpHandle;
begin
  HandleNeeded;
  Result := FHandle;
end;

function TdxGPCustomGraphicObject.GetHandleAllocated: Boolean;
begin
  Result := FHandle <> nil;
end;

{ TdxGPMatrix }

constructor TdxGPMatrix.CreateEx(const XForm: TXForm);
begin
  CreateEx(XForm.eM11, XForm.eM12, XForm.eM21, XForm.eM22, XForm.eDx, XForm.eDy);
end;

constructor TdxGPMatrix.CreateEx(M11, M12, M21, M22, DX, DY: Single);
begin
  Create;
  SetElements(M11, M12, M21, M22, DX, DY);
end;

constructor TdxGPMatrix.CreateFlip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single);
const
  FlipValueMap: array[Boolean] of Integer = (1, -1);
begin
  CreateEx(FlipValueMap[AFlipHorizontally], 0, 0, FlipValueMap[AFlipVertically],
    IfThen(AFlipHorizontally, 2 * APivotPointX), IfThen(AFlipVertically, 2 * APivotPointY));
end;

constructor TdxGPMatrix.CreateFlip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF);
begin
  CreateFlip(AFlipHorizontally, AFlipVertically, APivotPoint.X, APivotPoint.Y);
end;

procedure TdxGPMatrix.Assign(ASource: TdxGPCustomGraphicObject);
var
  M11, M12, M21, M22, DX, DY: Single;
begin
  inherited Assign(ASource);
  if ASource is TdxGPMatrix then
  begin
    TdxGPMatrix(ASource).GetElements(M11, M12, M21, M22, DX, DY);
    SetElements(M11, M12, M21, M22, DX, DY);
  end
end;

function TdxGPMatrix.Clone: TdxGPMatrix;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxGPMatrix.Create;
  Result.Assign(Self);
end;

procedure TdxGPMatrix.GetElements(out M11, M12, M21, M22, DX, DY: Single);
var
  AElements: array[0..5] of Single;
begin
  GdipCheck(GdipGetMatrixElements(Handle, @AElements[0]));
  M11 := AElements[0];
  M12 := AElements[1];
  M21 := AElements[2];
  M22 := AElements[3];
  DX := AElements[4];
  DY := AElements[5];
end;

procedure TdxGPMatrix.SetElements(M11, M12, M21, M22, DX, DY: Single);
begin
  GdipCheck(GdipSetMatrixElements(Handle, M11, M12, M21, M22, DX, DY));
  Changed;
end;

procedure TdxGPMatrix.Flip(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF);
var
  AMatrix: TdxGPMatrix;
begin
  if AFlipHorizontally or AFlipVertically then
  begin
    AMatrix := TdxGPMatrix.CreateFlip(AFlipHorizontally, AFlipVertically, APivotPoint.X, APivotPoint.Y);
    try
      Multiply(AMatrix, MatrixOrderAppend);
    finally
      AMatrix.Free;
    end;
  end;
end;

procedure TdxGPMatrix.Invert;
begin
  GdipCheck(GdipInvertMatrix(Handle));
  Changed;
end;

function TdxGPMatrix.GetBoundingRectangle(const R: TdxRectF): TdxRectF;
var
  ATopLeft, ATopRight, ABottomLeft, ABottomRight: TdxPointF;
begin
  ATopLeft := TransformPoint(dxPointF(R.Left, R.Top));
  ATopRight := TransformPoint(dxPointF(R.Right, R.Top));
  ABottomLeft := TransformPoint(dxPointF(R.Left, R.Bottom));
  ABottomRight := TransformPoint(dxPointF(R.Right, R.Bottom));

  Result := dxRectF(
    Min(Min(ATopLeft.X, ATopRight.X), Min(ABottomLeft.X, ABottomRight.X)),
    Min(Min(ATopLeft.Y, ATopRight.Y), Min(ABottomLeft.Y, ABottomRight.Y)),
    Max(Max(ATopLeft.X, ATopRight.X), Max(ABottomLeft.X, ABottomRight.X)),
    Max(Max(ATopLeft.Y, ATopRight.Y), Max(ABottomLeft.Y, ABottomRight.Y)));
end;

procedure TdxGPMatrix.Multiply(AMatrix: TdxGPMatrix; AMatrixOrder: TdxGpMatrixOrder);
begin
  GdipCheck(GdipMultiplyMatrix(Handle, AMatrix.Handle, AMatrixOrder));
  Changed;
end;

procedure TdxGPMatrix.Reset;
begin
  SetElements(1, 0, 0, 1, 0, 0);
end;

procedure TdxGPMatrix.Rotate(AAngle: Single; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  GdipCheck(GdipRotateMatrix(Handle, AAngle, AMatrixOrder));
  Changed;
end;

procedure TdxGPMatrix.Rotate(AAngle: Single; const APivotPoint: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
const
  TranslateDirectionMap: array[TdxGpMatrixOrder] of Integer = (1, -1);
begin
  Translate(TranslateDirectionMap[AMatrixOrder] * APivotPoint.X,
    TranslateDirectionMap[AMatrixOrder] * APivotPoint.Y, AMatrixOrder);
  Rotate(AAngle, AMatrixOrder);
  Translate(-TranslateDirectionMap[AMatrixOrder] * APivotPoint.X,
    -TranslateDirectionMap[AMatrixOrder] * APivotPoint.Y, AMatrixOrder);
end;

procedure TdxGPMatrix.Scale(const AScale: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  Scale(AScale.X, AScale.Y, AMatrixOrder);
end;

procedure TdxGPMatrix.Scale(const AScale, ACenter: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  SetElements(AScale.X, 0.0, 0.0, AScale.Y, ACenter.X - AScale.X * ACenter.X, ACenter.Y - AScale.Y * ACenter.Y);
end;

procedure TdxGPMatrix.Scale(const AScaleX, AScaleY: Single; AMatrixOrder: TdxGpMatrixOrder);
begin
  GdipCheck(GdipScaleMatrix(Handle, AScaleX, AScaleY, AMatrixOrder));
  Changed;
end;

procedure TdxGPMatrix.Shear(const AShear: TdxPointF; AMatrixOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  Shear(AShear.X, AShear.Y, AMatrixOrder);
end;

procedure TdxGPMatrix.Shear(const AShearX, AShearY: Single; AMatrixOrder: TdxGpMatrixOrder);
begin
  GdipCheck(GdipShearMatrix(Handle, AShearX, AShearY, AMatrixOrder));
  Changed;
end;

procedure TdxGPMatrix.Translate(const AOffset: TdxPointF; AMatrixOrder: TdxGpMatrixOrder);
begin
  Translate(AOffset.X, AOffset.Y, AMatrixOrder);
end;

function TdxGPMatrix.TransformPoint(const P: TdxPointF): TdxPointF;
var
  M11, M12, M21, M22, DX, DY: Single;
begin
  GetElements(M11, M12, M21, M22, DX, DY);
  Result.X := P.X * M11 + P.Y * M21 + DX;
  Result.Y := P.X * M12 + P.Y * M22 + DY;
end;

function TdxGPMatrix.TransformPoint(const P: TPoint): TPoint;
begin
  Result := cxPoint(TransformPoint(dxPointF(P)), False);
end;

procedure TdxGPMatrix.TransformPoints(var APoints: TArray<TPoint>);
var
  M11, M12, M21, M22, DX, DY: Single;
  I: Integer;
begin
  GetElements(M11, M12, M21, M22, DX, DY);
  for I := Low(APoints) to High(APoints) do
    APoints[I] := Point(Round(APoints[I].X * M11 + APoints[I].Y * M21 + DX), Round(APoints[I].X * M12 + APoints[I].Y * M22 + DY));
end;

procedure TdxGPMatrix.TransformPoints(var APoints: TArray<TdxPointF>);
var
  M11, M12, M21, M22, DX, DY: Single;
  I: Integer;
begin
  GetElements(M11, M12, M21, M22, DX, DY);
  for I := Low(APoints) to High(APoints) do
    APoints[I] := dxPointF(APoints[I].X * M11 + APoints[I].Y * M21 + DX, APoints[I].X * M12 + APoints[I].Y * M22 + DY);
end;

procedure TdxGPMatrix.TransformPointsI(APoints: PPoint; ACount: Integer);
var
  M11, M12, M21, M22, DX, DY: Single;
  I: Integer;
begin
  if ACount < 1 then
    Exit;
  GetElements(M11, M12, M21, M22, DX, DY);
  for I := 0 to ACount - 1 do
  begin
    APoints^ := Point(Round(APoints.X * M11 + APoints.Y * M21 + DX), Round(APoints.X * M12 + APoints.Y * M22 + DY));
    Inc(APoints);
  end;
end;

function TdxGPMatrix.TransformRect(const R: TdxRectF): TdxRectF;
begin
  Result.TopLeft := TransformPoint(R.TopLeft);
  Result.BottomRight := TransformPoint(R.BottomRight);
end;

function TdxGPMatrix.TransformRect(const R: TRect): TRect;
begin
  Result.TopLeft := TransformPoint(R.TopLeft);
  Result.BottomRight := TransformPoint(R.BottomRight);
end;

procedure TdxGPMatrix.Translate(const AOffsetX, AOffsetY: Single; AMatrixOrder: TdxGpMatrixOrder);
begin
  GdipCheck(GdipTranslateMatrix(Handle, AOffsetX, AOffsetY, AMatrixOrder));
  Changed;
end;

procedure TdxGPMatrix.DoCreateHandle(out AHandle: GpHandle);
begin
  GdipCheck(GdipCreateMatrix(AHandle));
end;

procedure TdxGPMatrix.DoFreeHandle(AHandle: GpHandle);
begin
  GdipCheck(GdipDeleteMatrix(AHandle));
end;

function TdxGPMatrix.GetIsIdentity: Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsMatrixIdentity(Handle, AValue));
  Result := AValue;
end;

function TdxGPMatrix.GetIsInvertible: Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsMatrixInvertible(Handle, AValue));
  Result := AValue;
end;

function TdxGPMatrix.GetOffsetX: Single;
var
  AElements: array[0..5] of Single;
begin
  GdipCheck(GdipGetMatrixElements(Handle, @AElements[0]));
  Result := AElements[4];
end;

function TdxGPMatrix.GetOffsetY: Single;
var
  AElements: array[0..5] of Single;
begin
  GdipCheck(GdipGetMatrixElements(Handle, @AElements[0]));
  Result := AElements[5];
end;

{ TdxGPBrushGradientPoints }

constructor TdxGPBrushGradientPoints.Create;
begin
  inherited Create;
  SetLength(FColors, 2);
  SetLength(FOffsets, 2);
end;

function TdxGPBrushGradientPoints.Add(AOffset: Single; AColor: TdxAlphaColor): Integer;
begin
  if Count = Capacity then
    Grow;
  Result := InternalAdd(AOffset, AColor);
  Changed;
end;

procedure TdxGPBrushGradientPoints.Assign(Source: TdxGPBrushGradientPoints);
var
  I: Integer;
begin
  if (Source <> nil) and (Source <> Self) then
  begin
    Clear;
    Capacity := Source.Count;
    for I := 0 to Source.Count - 1 do
      InternalAdd(Source.Offsets[I], Source.Colors[I]);
    Changed;
  end;
end;

procedure TdxGPBrushGradientPoints.Clear;
begin
  Capacity := 0;
end;

procedure TdxGPBrushGradientPoints.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckIndex(Index);
  for I := Index to Count - 1 do
  begin
    FColors[I + 1] := FColors[I + 2];
    FOffsets[I + 1] := FOffsets[I + 2];
  end;
  Dec(FCount);
  Changed;
end;

procedure TdxGPBrushGradientPoints.InvertOrder;
var
  I: Integer;
begin
  if Count > 1 then
  begin
    for I := 0 to Count div 2 - 1 do
      ExchangeLongWords(FColors[I + 1], FColors[Count - I]);
    for I := 0 to Count div 2 - 1 do
      ExchangeLongWords(FOffsets[I + 1], FOffsets[Count - I]);
    for I := 0 to Count - 1 do
      FOffsets[I + 1] := 1 - FOffsets[I + 1];
    Changed;
  end;
end;

procedure TdxGPBrushGradientPoints.CalculateParams(out AColors: PdxAlphaColor; out AOffsets: PSingle; out ACount: Integer);
begin
  ACount := Count;
  if Offsets[0] > 0 then
  begin
    AOffsets := @FOffsets[0];
    AColors := @FColors[0];
    Inc(ACount);
  end
  else
  begin
    AOffsets := @FOffsets[1];
    AColors := @FColors[1];
  end;

  if Offsets[Count - 1] < 1 then
  begin
    FColors[Count + 1] := 0;
    FOffsets[Count + 1] := 1;
    Inc(ACount);
  end;
end;

procedure TdxGPBrushGradientPoints.Changed;
begin
  ValidateOrder;
  dxCallNotify(OnChange, Self);
end;

procedure TdxGPBrushGradientPoints.ValidateOrder;

  procedure Sort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while FOffsets[I] < FOffsets[P] do
          Inc(I);
        while FOffsets[J] > FOffsets[P] do
          Dec(J);

        if I <= J then
        begin
          if I <> J then
          begin
            ExchangeLongWords(FColors[I], FColors[J]);
            ExchangeLongWords(FOffsets[I], FOffsets[J]);
          end;

          if P = I then
            P := J
          else
            if P = J then
              P := I;

          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        Sort(L, J);
      L := I;
    until I >= R;
  end;

begin
  Sort(1, Count);
end;

procedure TdxGPBrushGradientPoints.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index]);
end;

procedure TdxGPBrushGradientPoints.Grow;
var
  ADelta: Integer;
begin
  if Capacity > 128 then
    ADelta := FCapacity shr 1
  else
    if Capacity > 16 then
      ADelta := 32
    else
      ADelta := 16;

  Capacity := Capacity + ADelta;
end;

function TdxGPBrushGradientPoints.GetColor(Index: Integer): TdxAlphaColor;
begin
  CheckIndex(Index);
  Result := FColors[Index + 1];
end;

function TdxGPBrushGradientPoints.GetOffset(Index: Integer): Single;
begin
  CheckIndex(Index);
  Result := FOffsets[Index + 1];
end;

function TdxGPBrushGradientPoints.InternalAdd(AOffset: Single; AColor: TdxAlphaColor): Integer;
begin
  Result := Count;
  FOffsets[Result + 1] := AOffset;
  FColors[Result + 1] := AColor;
  Inc(FCount);
end;

procedure TdxGPBrushGradientPoints.SetCapacity(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FCapacity then
  begin
    FCapacity := AValue;
    SetLength(FOffsets, Capacity + 2);
    SetLength(FColors, Capacity + 2);
    if Count > Capacity then
    begin
      FCount := Capacity;
      Changed;
    end;
  end;
end;

procedure TdxGPBrushGradientPoints.SetColor(Index: Integer; const AValue: TdxAlphaColor);
begin
  CheckIndex(Index);
  if FColors[Index + 1] <> AValue then
  begin
    FColors[Index + 1] := AValue;
    Changed;
  end;
end;

procedure TdxGPBrushGradientPoints.SetOffset(Index: Integer; const AValue: Single);
begin
  CheckIndex(Index);
  if not SameValue(FOffsets[Index + 1], AValue) then
  begin
    FOffsets[Index + 1] := AValue;
    Changed;
  end;
end;

{ TdxGPCustomBrush }

procedure TdxGPCustomBrush.Assign(ASource: TdxGPCustomGraphicObject);
begin
  inherited Assign(ASource);
  if ASource is TdxGPCustomBrush then
    SetTargetRect(TdxGPCustomBrush(ASource).TargetRect);
end;

procedure TdxGPCustomBrush.SetTargetRect(const R: TRect);
begin
  SetTargetRect(dxGpMakeRectF(R));
end;

procedure TdxGPCustomBrush.SetTargetRect(const R: TdxRectF);
begin
  SetTargetRect(MakeRect(R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCustomBrush.SetTargetRect(const R: TdxGpRect);
begin
  SetTargetRect(dxGpMakeRectF(R));
end;

procedure TdxGPCustomBrush.SetTargetRect(const R: TdxGpRectF);
var
  ARect: TdxGpRectF;
begin
  // Inflate: Avoid GDIPlus gradient fill bug
  ARect := MakeRect(R.X - 1, R.Y - 1, R.Width + 2, R.Height + 2);
  if not dxGpRectIsEqual(ARect, FTargetRect) then
  begin
    FTargetRect := ARect;
    DoTargetRectChanged;
  end;
end;

procedure TdxGPCustomBrush.DoFreeHandle(AHandle: GpHandle);
begin
  GdipCheck(GdipDeleteBrush(AHandle));
end;

procedure TdxGPCustomBrush.DoTargetRectChanged;
begin
  if NeedRecreateHandleOnTargetRectChange then
    FreeHandle;
end;

function TdxGPCustomBrush.NeedRecreateHandleOnTargetRectChange: Boolean;
begin
  Result := False;
end;

{ TdxGPHatchBrush }

procedure TdxGPHatchBrush.DoCreateHandle(out AHandle: GpHandle);
begin
  GdipCheck(GdipCreateHatchBrush(FStyle, FForegroundColor, FBackgroundColor, AHandle));
end;

procedure TdxGPHatchBrush.SetBackgroundColor(const AValue: TdxAlphaColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Changed;
  end;
end;

procedure TdxGPHatchBrush.SetForegroundColor(const AValue: TdxAlphaColor);
begin
  if FForegroundColor <> AValue then
  begin
    FForegroundColor := AValue;
    Changed;
  end;
end;

procedure TdxGPHatchBrush.SetStyle(const AValue: TdxGpHatchStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    Changed;
  end;
end;

{ TdxGPBrush }

constructor TdxGPBrush.Create;
begin
  inherited Create;
  FGradientPoints := TdxGPBrushGradientPoints.Create;
  FGradientPoints.OnChange := PointsChangeHandler;
  FTexture := TdxGPImage.Create;
  FTexture.OnChange := TextureChangeHandler;
  FTextureTransform := TdxGPMatrix.Create;
  FTextureTransform.OnChange := TextureChangeHandler;
end;

destructor TdxGPBrush.Destroy;
begin
  FreeAndNil(FTextureTransform);
  FreeAndNil(FTexture);
  FreeAndNil(FGradientPoints);
  inherited Destroy;
end;

procedure TdxGPBrush.Assign(ASource: TdxGPCustomGraphicObject);
begin
  inherited Assign(ASource);
  if ASource is TdxGPBrush then
  begin
    Color := TdxGPBrush(ASource).Color;
    GradientMode := TdxGPBrush(ASource).GradientMode;
    GradientPoints := TdxGPBrush(ASource).GradientPoints;
    Texture := TdxGPBrush(ASource).Texture;
    TextureTransform := TdxGPBrush(ASource).TextureTransform;
    Style := TdxGPBrush(ASource).Style;
  end;
end;

procedure TdxGPBrush.CreateEmptyBrushHandle(out AHandle: GpBrush);
begin
  GdipCheck(GdipCreateSolidFill(0, AHandle));
end;

procedure TdxGPBrush.CreateGradientBrushHandle(out AHandle: GpBrush);
const
  Map: array[TdxGPBrushGradientMode] of LinearGradientMode = (
    LinearGradientModeHorizontal, LinearGradientModeVertical,
    LinearGradientModeForwardDiagonal, LinearGradientModeBackwardDiagonal
  );
var
  AColors: PdxAlphaColor;
  ACount: Integer;
  AOffsets: PSingle;
begin
  if (GradientPoints.Count = 0) or (FTargetRect.Width = 0) or (FTargetRect.Height = 0) then
  begin
    CreateEmptyBrushHandle(AHandle);
    Exit;
  end;

  GradientPoints.CalculateParams(AColors, AOffsets, ACount);
  GdipCheck(GdipCreateLineBrushFromRect(@FTargetRect, 0, 0, Map[GradientMode], WrapModeTileFlipX, AHandle));
  GdipCheck(GdipSetLinePresetBlend(AHandle, AColors, AOffsets, ACount));
end;

procedure TdxGPBrush.CreateTextureBrushHandle(out AHandle: GpBrush);
begin
  if Texture.Empty then
    CreateEmptyBrushHandle(AHandle)
  else
  begin
    GdipCheck(GdipCreateTexture(Texture.Handle, WrapModeTile, AHandle));
    ApplyTextureTransform;
  end;
end;

function TdxGPBrush.GetIsEmpty: Boolean;
begin
  case Style of
    gpbsSolid:
      Result := Color = 0;
    gpbsClear:
      Result := True;
    gpbsGradient:
      Result := GradientPoints.Count = 0;
    gpbsTexture:
      Result := Texture.Empty;
  else
    Result := False;
  end;
end;

procedure TdxGPBrush.DoCreateHandle(out AHandle: GpHandle);
begin
  case Style of
    gpbsSolid:
      GdipCheck(GdipCreateSolidFill(Color, AHandle));
    gpbsClear:
      CreateEmptyBrushHandle(AHandle);
    gpbsGradient:
      CreateGradientBrushHandle(AHandle);
    gpbsTexture:
      CreateTextureBrushHandle(AHandle);
  end;
end;

procedure TdxGPBrush.DoHandleCreated;
begin
  if Assigned(FOnHandleCreated) then
    FOnHandleCreated(Self);
end;

procedure TdxGPBrush.DoTargetRectChanged;
begin
  inherited DoTargetRectChanged;

  if IsTextureBrush then
  begin
    ApplyTextureTransform;
    GdipCheck(GdipTranslateTextureTransform(Handle, FTargetRect.X, FTargetRect.Y, MatrixOrderAppend));
  end;
end;

function TdxGPBrush.NeedRecreateHandleOnTargetRectChange: Boolean;
begin
  Result := Style = gpbsGradient;
end;

procedure TdxGPBrush.SetColor(const AValue: TdxAlphaColor);
begin
  if AValue <> FColor then
  begin
    FColor := AValue;
    if HandleAllocated and (Style = gpbsSolid) then
      GdipCheck(GdipSetSolidFillColor(Handle, Color));
    Changed;
  end;
end;

procedure TdxGPBrush.SetGradientMode(const AValue: TdxGPBrushGradientMode);
begin
  if FGradientMode <> AValue then
  begin
    FGradientMode := AValue;
    if Style = gpbsGradient then
      FreeHandle;
    Changed;
  end;
end;

procedure TdxGPBrush.SetGradientPoints(const AValue: TdxGPBrushGradientPoints);
begin
  FGradientPoints.Assign(AValue);
end;

procedure TdxGPBrush.SetStyle(const AValue: TdxGPBrushStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    FreeHandle;
    Changed;
  end;
end;

procedure TdxGPBrush.SetTexture(const AValue: TdxGPImage);
begin
  FTexture.Assign(AValue);
end;

procedure TdxGPBrush.SetTextureTransform(const AValue: TdxGpMatrix);
begin
  FTextureTransform.Assign(AValue);
end;

procedure TdxGPBrush.PointsChangeHandler(Sender: TObject);
begin
  if Style = gpbsGradient then
    FreeHandle;
  Changed;
end;

procedure TdxGPBrush.TextureChangeHandler(Sender: TObject);
begin
  if IsTextureBrush then
  begin
    FreeHandle;
    ApplyTextureTransform;
  end;
  Changed;
end;

function TdxGPBrush.IsTextureBrush: Boolean;
begin
  Result := Style = gpbsTexture;
end;

procedure TdxGPBrush.ApplyTextureTransform;
begin
  if IsTextureBrush and TextureTransform.HandleAllocated then
    GdipCheck(GdipSetTextureTransform(Handle, FTextureTransform.Handle))
end;

{ TdxGPPen }

constructor TdxGPPen.Create;
begin
  Create(0);
end;

constructor TdxGPPen.Create(AColor: TdxAlphaColor; AWidth: Single = 1; APenStyle: TPenStyle = psSolid; AScale: Single = 1);
begin
  inherited Create;
  FBrush := CreateBrush;
  FBrush.Color := AColor;
  FBrush.FOnHandleCreated := BrushChangeHandler;
  FBrush.OnChange := BrushChangeHandler;
  FWidth := AWidth;
  FMiterLimit := 10.0;
  InitializePenPattern(APenStyle, 1 / Max(0.001, AWidth) * AScale);
end;

destructor TdxGPPen.Destroy;
begin
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TdxGPPen.Assign(ASource: TdxGPCustomGraphicObject);
begin
  inherited Assign(ASource);

  if ASource is TdxGPPen then
  begin
    Brush := TdxGPPen(ASource).Brush;
    Style := TdxGPPen(ASource).Style;
    Width := TdxGPPen(ASource).Width;
    DashCapStyle := TdxGPPen(ASource).DashCapStyle;
    LineStartCapStyle := TdxGPPen(ASource).LineStartCapStyle;
    LineEndCapStyle := TdxGPPen(ASource).LineEndCapStyle;
  end;
end;

function TdxGPPen.CreateBrush: TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
end;

procedure TdxGPPen.DoCreateHandle(out AHandle: GpHandle);
begin
  GdipCheck(GdipCreatePen2(Brush.Handle, Width, guPixel, AHandle));
  DoSetDashStyle(AHandle);
  DoSetDashCapStyle(AHandle);
  DoSetLineStartCapStyle(AHandle);
  DoSetLineJoin(AHandle);
  DoSetLineEndCapStyle(AHandle);
  DoSetMiterLimit(AHandle);
end;

procedure TdxGPPen.DoFreeHandle(AHandle: GpHandle);
begin
  GdipCheck(GdipDeletePen(AHandle));
end;

procedure TdxGPPen.DoSetDashStyle(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenDashStyle(AHandle, GpPenStyleToDashStyle[Style]));
end;

procedure TdxGPPen.DoSetDashCapStyle(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenDashCap197819(AHandle, PenDashCapStyleToGpDashCap[FDashCapStyle]));
end;

procedure TdxGPPen.DoSetLineEndCapStyle(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenEndCap(AHandle, PenLineStyleToGpLineCap[FLineEndCapStyle]));
end;

procedure TdxGPPen.DoSetLineJoin(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenLineJoin(AHandle, FLineJoin));
end;

procedure TdxGPPen.DoSetLineStartCapStyle(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenStartCap(AHandle, PenLineStyleToGpLineCap[FLineStartCapStyle]));
end;

procedure TdxGPPen.DoSetMiterLimit(AHandle: GpHandle);
begin
  GdipCheck(GdipSetPenMiterLimit(AHandle, FMiterLimit));
end;

function TdxGPPen.GetIsEmpty: Boolean;
begin
  Result := IsZero(Width) or Brush.IsEmpty;
end;

procedure TdxGPPen.InitializePenPattern(APenStyle: TPenStyle; AScale: Single);
begin
  case APenStyle of
    psDash:
      SetDashArray([9, 4], AScale);
    psDot:
      SetDashArray([2, 4], AScale);
    psDashDot:
      SetDashArray([9, 4, 2, 4], AScale);
    psDashDotDot:
      SetDashArray([9, 4, 2, 4, 2, 4], AScale);
  else
  end;
end;

procedure TdxGPPen.HandleNeeded;
begin
  Brush.HandleNeeded;
  inherited HandleNeeded;
end;

procedure TdxGPPen.BrushChangeHandler(Sender: TObject);
begin
  if HandleAllocated then
    GdipCheck(GdipSetPenBrushFill(Handle, Brush.Handle));
end;

procedure TdxGPPen.SetBrush(const AValue: TdxGPBrush);
begin
  FBrush.Assign(AValue);
end;

procedure TdxGPPen.SetDashArray(const AValue: array of Single);
begin
  SetDashArray(AValue, 1);
end;

procedure TdxGPPen.SetDashArray(const AValue: array of Single; AScale: Single);
var
  ALength: Integer;
  AScaledValue: array of Single;
  I: Integer;
begin
  ALength := Length(AValue);
  if ALength > 0 then
  begin
    if SameValue(AScale, 1) then
      GdipCheck(GdipSetPenDashArray(Handle, @AValue[0], ALength))
    else
    begin
      SetLength(AScaledValue, ALength);
      for I := 0 to ALength - 1 do
        AScaledValue[I] := AValue[I] * AScale;
      GdipCheck(GdipSetPenDashArray(Handle, @AScaledValue[0], ALength));
    end;
    FStyle := gppsDashDotDot;
    Changed;
  end
  else
    Style := gppsSolid;
end;

procedure TdxGPPen.SetDashCapStyle(const AValue: TdxGPPenDashCapStyle);
begin
  if FDashCapStyle <> AValue then
  begin
    FDashCapStyle := AValue;
    if HandleAllocated then
      DoSetDashCapStyle(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetLineEndCapStyle(const AValue: TdxGPPenLineCapStyle);
begin
  if FLineEndCapStyle <> AValue then
  begin
    FLineEndCapStyle := AValue;
    if HandleAllocated then
      DoSetLineEndCapStyle(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetLineJoin(const AValue: TdxGpLineJoin);
begin
  if FLineJoin <> AValue then
  begin
    FLineJoin := AValue;
    if HandleAllocated then
      DoSetLineJoin(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetLineStartCapStyle(const AValue: TdxGPPenLineCapStyle);
begin
  if FLineStartCapStyle <> AValue then
  begin
    FLineStartCapStyle := AValue;
    if HandleAllocated then
      DoSetLineStartCapStyle(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetMiterLimit(const AValue: Single);
begin
  if FMiterLimit <> AValue then
  begin
    FMiterLimit := AValue;
    if HandleAllocated then
      DoSetMiterLimit(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetStyle(const AValue: TdxGPPenStyle);
begin
  if (FStyle <> AValue) or (AValue = gppsDashDotDot) then
  begin
    FStyle := AValue;
    if HandleAllocated then
      DoSetDashStyle(Handle);
    Changed;
  end;
end;

procedure TdxGPPen.SetTargetRect(const R: TdxGpRect);
begin
  Brush.SetTargetRect(R);
end;

procedure TdxGPPen.SetTargetRect(const R: TRect);
begin
  Brush.SetTargetRect(R);
end;

procedure TdxGPPen.SetTargetRect(const R: TdxRectF);
begin
  Brush.SetTargetRect(R);
end;

procedure TdxGPPen.SetWidth(AValue: Single);
begin
  AValue := Max(AValue, 0);
  if not SameValue(FWidth, AValue) then
  begin
    FWidth := AValue;
    if HandleAllocated then
      GdipCheck(GdipSetPenWidth(Handle, Width));
    Changed;
  end;
end;

{ TdxGPPath }

constructor TdxGPPath.Create;
begin
  Create(gpfmAlternate);
end;

constructor TdxGPPath.Create(AFillMode: TdxGPFillMode);
begin
  inherited Create;
  GdipCheck(GdipCreatePath(dxGDIPlusAPI.FillMode(AFillMode), FHandle));
end;

constructor TdxGPPath.Create(APoints: TdxGpPointFDynArray; const APointTypes: TBytes);
begin
  inherited Create;
  GdipCheck(GdipCreatePath2(@APoints[0], @APointTypes[0], Length(APoints), FillModeAlternate, FHandle));
end;

destructor TdxGPPath.Destroy;
begin
  GdipCheck(GdipDeletePath(FHandle));
  inherited Destroy;
end;

procedure TdxGPPath.Assign(ASource: TdxGPPath);
var
  AHandle: GpHandle;
begin
  if ASource <> Self then
  begin
    GdipCheck(GdipClonePath(ASource.Handle, AHandle));
    GdipCheck(GdipDeletePath(FHandle));
    FHandle := AHandle;
  end;
end;

procedure TdxGPPath.Flatten(AMatrix: TdxGPMatrix = nil; AFlatness: Single = 0.25);
begin
  GdipCheck(GdipFlattenPath(Handle, AMatrix, AFlatness));
end;

procedure TdxGPPath.FigureFinish;
begin
  GdipCheck(GdipClosePathFigure(Handle));
end;

procedure TdxGPPath.FigureStart;
begin
  GdipCheck(GdipStartPathFigure(Handle));
end;

procedure TdxGPPath.Reset;
begin
  GdipCheck(GdipResetPath(Handle));
end;

procedure TdxGPPath.AddArc(const X, Y, Width, Height, StartAngle, SweepAngle: Single);
begin
  GdipCheck(GdipAddPathArc(Handle, X, Y, Width, Height, StartAngle, SweepAngle));
end;

procedure TdxGPPath.AddBezier(const P1, P2, P3, P4: TdxPointF);
begin
  GdipCheck(GdipAddPathBezier(Handle, P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y, P4.X, P4.Y));
end;

procedure TdxGPPath.AddEllipse(const R: TRect);
begin
  GdipCheck(GdipAddPathEllipseI(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPPath.AddEllipse(const R: TdxRectF);
begin
  GdipCheck(GdipAddPathEllipse(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPPath.AddLine(const X1, Y1, X2, Y2: Single);
begin
  GdipCheck(GdipAddPathLine(Handle, X1, Y1, X2, Y2));
end;

procedure TdxGPPath.AddPath(const Path: TdxGPPath);
begin
  GdipCheck(GdipAddPathPath(Handle, Path.Handle, True));
end;

procedure TdxGPPath.AddPie(const X, Y, Width, Height, StartAngle, SweepAngle: Single);
begin
  GdipCheck(GdipAddPathPie(Handle, X, Y, Width, Height, StartAngle, SweepAngle));
end;

procedure TdxGPPath.AddPolygon(const APoints: array of TPoint);
begin
  GdipCheck(GdipAddPathPolygonI(Handle, @APoints[0], Length(APoints)));
end;

procedure TdxGPPath.AddPolygon(const APoints: array of TdxPointF);
begin
  GdipCheck(GdipAddPathPolygon(Handle, @APoints[0], Length(APoints)));
end;

procedure TdxGPPath.AddPolyline(const APoints: array of TPoint);
begin
  GdipCheck(GdipAddPathLine2I(Handle, @APoints[0], Length(APoints)));
end;

procedure TdxGPPath.AddPolyline(const APoints: array of TdxPointF);
begin
  GdipCheck(GdipAddPathLine2(Handle, @APoints[0], Length(APoints)));
end;

procedure TdxGPPath.AddRect(const R: TRect);
begin
  GdipCheck(GdipAddPathRectangleI(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPPath.AddRect(const R: TdxRectF);
begin
  GdipCheck(GdipAddPathRectangle(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPPath.AddRoundRect(const R: TRect; ARadiusX, ARadiusY: Integer);
begin
  AddRoundRect(dxRectF(R), ARadiusX, ARadiusY);
end;

procedure TdxGPPath.AddRoundRect(const R: TdxRectF; ARadiusX, ARadiusY: Single);
begin
  ARadiusX := Min(ARadiusX, cxRectWidth(R) / 3);
  ARadiusY := Min(ARadiusY, cxRectHeight(R) / 3);

  FigureStart;
  try
    if (ARadiusX > 0) and (ARadiusY > 0) then
    begin
      AddLine(R.Left + ARadiusX, R.Top, R.Right - 2 * ARadiusX, R.Top);
      AddArc(R.Right - 2 * ARadiusX, R.Top, 2 * ARadiusX, 2 * ARadiusY, 270, 90);
      AddLine(R.Right, R.Top + ARadiusY, R.Right, R.Bottom - 2 * ARadiusY);
      AddArc(R.Right - 2 * ARadiusX, R.Bottom - 2 * ARadiusY, 2 * ARadiusX, 2 * ARadiusY, 0, 90);
      AddLine(R.Right - 2 * ARadiusX, R.Bottom, R.Left + ARadiusX, R.Bottom);
      AddArc(R.Left, R.Bottom - 2 * ARadiusY, 2 * ARadiusX, 2 * ARadiusY, 90, 90);
      AddLine(R.Left, R.Bottom - 2 * ARadiusY, R.Left, R.Top + ARadiusY);
      AddArc(R.Left, R.Top, 2 * ARadiusX, 2 * ARadiusY, 180, 90);
    end
    else
      AddRect(R);
  finally
    FigureFinish;
  end;
end;

procedure TdxGPPath.AddString(const S: string; AFont: TdxGPFont; AFormat: TdxGPStringFormat; const ARect: TdxRectF);
var
  AGpRectF: TdxGpRectF;
  ANativeFormat: GpStringFormat;
  ASize: Single;
  ADpi: Integer;
  ADC: HDC;
begin
  AGpRectF := ARect;
  if AFormat <> nil then
    ANativeFormat := AFormat.Handle
  else
    ANativeFormat := nil;
  ASize := AFont.SizeInPoints;
  ADC := GetDC(0);
  ADpi := GetDeviceCaps(ADC, LOGPIXELSY);
  ReleaseDC(0, ADC);
  ASize := ASize * ADpi / 72;
  GdipCheck(GdipAddPathString(Handle, PChar(S), Length(S), AFont.FontFamily.Handle, AFont.Style,
    ASize, @AGpRectF, ANativeFormat));
end;

procedure TdxGPPath.AddString(const S: string; AFont: TdxGPFont; AFormat: TdxGPStringFormat; const AOrigin: TdxPointF);
begin
  AddString(S, AFont, AFormat, TdxRectF.CreateSize(AOrigin.X, AOrigin.Y, 0, 0));
end;

procedure TdxGPPath.AddString(const S: string; AFont: TFont; AEmSize: Single; const ARect: TRect);
var
  AGpRect: TdxGpRect;
  ANativeFamily: GpFontFamily;
  ANativeFormat: GpStringFormat;
begin
  GdipCheck(GdipCreateStringFormat(0, LANG_NEUTRAL, ANativeFormat));
  GdipCheck(GdipCreateFontFamilyFromName(PChar(AFont.Name), nil, ANativeFamily));
  try
    AGpRect := ARect;
    GdipCheck(GdipAddPathStringI(Handle, PChar(S), Length(S), ANativeFamily,
      dxFontStylesToGpFontStyles(AFont.Style), AEmSize, @AGpRect, ANativeFormat));
  finally
    GdipDeleteFontFamily(ANativeFamily);
    GdipDeleteStringFormat(ANativeFormat);
  end;
end;

procedure TdxGPPath.AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single;
  const AOrigin: TdxPointF; AFormat: TdxGPStringFormat);
var
  ARectf: TdxGpRectF;
  ANativeFamily: GpFontFamily;
  ANativeFormat: GpStringFormat;
begin
  ARectf.Init(AOrigin.X, AOrigin.Y, 0, 0);
  if AFamily = nil then
    ANativeFamily := nil
  else
    ANativeFamily := AFamily.Handle;
  if AFormat = nil then
    ANativeFormat := nil
  else
    ANativeFormat := AFormat.Handle;
  GdipCheck(GdipAddPathString(Handle, PChar(S), Length(S), ANativeFamily, AStyle, AEmSize, @ARectf, ANativeFormat));
end;

procedure TdxGPPath.AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single;
  const AOrigin: TPoint; AFormat: TdxGPStringFormat);
var
  ARect: TdxGpRect;
  ANativeFamily: GpFontFamily;
  ANativeFormat: GpStringFormat;
begin
  ARect.Init(AOrigin.X, AOrigin.Y, 0, 0);
  if AFamily = nil then
    ANativeFamily := nil
  else
    ANativeFamily := AFamily.Handle;
  if AFormat = nil then
    ANativeFormat := nil
  else
    ANativeFormat := AFormat.Handle;
  GdipCheck(GdipAddPathStringI(Handle, PChar(S), Length(S), ANativeFamily, AStyle, AEmSize, @ARect, ANativeFormat));
end;

procedure TdxGPPath.AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single;
  const ALayoutRect: TdxRectF; AFormat: TdxGPStringFormat);
var
  ARectf: TdxGpRectF;
  ANativeFamily: GpFontFamily;
  ANativeFormat: GpStringFormat;
begin
  ARectf := ALayoutRect;
  if AFamily = nil then
    ANativeFamily := nil
  else
    ANativeFamily := AFamily.Handle;
  if AFormat = nil then
    ANativeFormat := nil
  else
    ANativeFormat := AFormat.Handle;
  GdipCheck(GdipAddPathString(Handle, PChar(S), Length(S), ANativeFamily, AStyle, AEmSize, @ARectf, ANativeFormat));
end;

procedure TdxGPPath.AddString(const S: string; AFamily: TdxGPFontFamily; AStyle: Integer; AEmSize: Single;
  const ALayoutRect: TRect; AFormat: TdxGPStringFormat);
var
  ARect: TdxGpRect;
  ANativeFamily: GpFontFamily;
  ANativeFormat: GpStringFormat;
begin
  ARect := ALayoutRect;
  if AFamily = nil then
    ANativeFamily := nil
  else
    ANativeFamily := AFamily.Handle;
  if AFormat = nil then
    ANativeFormat := nil
  else
    ANativeFormat := AFormat.Handle;
  GdipCheck(GdipAddPathStringI(Handle, PChar(S), Length(S), ANativeFamily, AStyle, AEmSize, @ARect, ANativeFormat));
end;

function TdxGPPath.GetBounds(APen: TdxGPPen = nil): TRect;
var
  R: TdxGpRect;
  APenHandle: GpPen;
begin
  if APen <> nil then
    APenHandle := APen.Handle
  else
    APenHandle := nil;

  GdipCheck(GdipGetPathWorldBoundsI(Handle, @R, nil, APenHandle));
  Result := cxRectBounds(R.X, R.Y, R.Width, R.Height);
end;

function TdxGPPath.GetBoundsF(APen: TdxGPPen = nil): TdxRectF;
var
  R: TdxGpRectF;
  APenHandle: GpPen;
begin
  if APen <> nil then
    APenHandle := APen.Handle
  else
    APenHandle := nil;

  GdipCheck(GdipGetPathWorldBounds(Handle, @R, nil, APenHandle));
  Result := cxRectFBounds(R.X, R.Y, R.Width, R.Height);
end;

function TdxGPPath.GetPointCount: Integer;
begin
  GdipCheck(GdipGetPointCount(Handle, Result));
end;

function TdxGPPath.IsPointInPath(const P: TPoint; ACanvas: TdxGPCanvas = nil): Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsVisiblePathPointI(Handle, P.X, P.Y, dxGpGetCanvasHandle(ACanvas), AValue));
  Result := AValue;
end;

function TdxGPPath.IsPointInPath(const P: TdxPointF; ACanvas: TdxGPCanvas = nil): Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsVisiblePathPoint(Handle, P.X, P.Y, dxGpGetCanvasHandle(ACanvas), AValue));
  Result := AValue;
end;

function TdxGPPath.IsPointInPathOutline(const P: TPoint; APen: TdxGPPen; ACanvas: TdxGPCanvas = nil): Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsOutlineVisiblePathPointI(Handle, P.X, P.Y, APen.Handle, dxGpGetCanvasHandle(ACanvas), AValue));
  Result := AValue;
end;

function TdxGPPath.IsPointInPathOutline(const P: TPoint; APenWidth: Integer; ACanvas: TdxGPCanvas = nil): Boolean;
var
  AHandle: IdxGPHandle;
  AValue: LongBool;
begin
  Result := False;
  if TdxGPResourceManager.GetPen(TdxAlphaColors.Red, APenWidth, psSolid, AHandle) then
  begin
    GdipCheck(GdipIsOutlineVisiblePathPointI(Handle, P.X, P.Y, AHandle.GetNativeHandle, dxGpGetCanvasHandle(ACanvas), AValue));
    Result := AValue;
  end;
end;

function TdxGPPath.IsPointInPathOutline(const P: TdxPointF; APen: TdxGPPen; ACanvas: TdxGPCanvas = nil): Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsOutlineVisiblePathPoint(Handle, P.X, P.Y, APen.Handle, dxGpGetCanvasHandle(ACanvas), AValue));
  Result := AValue;
end;

procedure TdxGPPath.Transform(AMatrix: TdxGPMatrix);
begin
  GdipCheck(GdipTransformPath(Handle, AMatrix.Handle));
end;

function TdxGPPath.GetFillMode: TdxGPFillMode;
var
  AMode: dxGDIPlusAPI.FillMode;
begin
  GdipCheck(GdipGetPathFillMode(Handle, AMode));
  Result := TdxGPFillMode(AMode);
end;

procedure TdxGPPath.SetFillMode(AValue: TdxGPFillMode);
begin
  GdipCheck(GdipSetPathFillMode(Handle, dxGDIPlusAPI.FillMode(AValue)));
end;

{ TdxGPRegion }

constructor TdxGPRegion.Create;
begin
  inherited Create;
  GdipCheck(GdipCreateRegion(FHandle));
end;

constructor TdxGPRegion.Create(APath: GpPath);
begin
  inherited Create;
  GdipCheck(GdipCreateRegionPath(APath, FHandle));
end;

constructor TdxGPRegion.Create(const ARect: TRect);
var
  R: TdxGPRect;
begin
  inherited Create;
  R := MakeRect(ARect);
  GdipCheck(GdipCreateRegionRectI(@R, FHandle));
end;

constructor TdxGPRegion.Create(const ARect: TdxRectF);
var
  R: TdxGPRectF;
begin
  inherited Create;
  R := MakeRect(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
  GdipCheck(GdipCreateRegionRect(@R, FHandle));
end;

constructor TdxGPRegion.CreateFromRegion(ARegion: GpRegion; ADummy: Integer = 0);
begin
  inherited Create;
  GdipCheck(GdipCloneRegion(ARegion, FHandle));
end;

destructor TdxGPRegion.Destroy;
begin
  GdipCheck(GdipDeleteRegion(FHandle));
  inherited Destroy;
end;

procedure TdxGPRegion.CombineRegionRect(const ARect: TRect; AMode: TdxGPCombineMode);
var
  R: TdxGPRect;
begin
  R := MakeRect(ARect);
  GdipCheck(GdipCombineRegionRectI(FHandle, @R, TGpCombineMode(AMode)));
end;

procedure TdxGPRegion.CombineRegionRect(const ARect: TdxRectF;
  AMode: TdxGPCombineMode);
var
  R: TdxGPRectF;
begin
  inherited Create;
  R := MakeRect(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
  GdipCheck(GdipCombineRegionRect(FHandle, @R, TGpCombineMode(AMode)));
end;

procedure TdxGPRegion.CombineRegionRegion(const ARegion: TdxGPRegion; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipCombineRegionRegion(Handle, ARegion.Handle, TGpCombineMode(AMode)));
end;

{ TdxGPMemoryStream }

function TdxGPMemoryStream.Realloc(var ANewCapacity: Integer): Pointer;
begin
  Result := Memory;
  if ANewCapacity <> Capacity then
  begin
    if ANewCapacity > 0 then
    begin
      Result := GdipAlloc(ANewCapacity);
      if Result = nil then
        raise EStreamError.CreateRes(@SMemoryStreamError);
      if Capacity > 0 then
        Move(Memory^, Result^, Min(Capacity, ANewCapacity));
    end
    else
      Result := nil;

    if Capacity > 0 then
      GdipFree(Memory);
  end;
end;

{ TdxGPStringFormat }

constructor TdxGPStringFormat.Create(AFormatAttributes: Integer = 0; ALanguage: LANGID = LANG_NEUTRAL);
begin
  inherited Create;
  GdipCheck(GdipCreateStringFormat(AFormatAttributes, ALanguage, FHandle));
end;

constructor TdxGPStringFormat.Create(AFormat: TdxGPStringFormat);
var
  AHandle: GpStringFormat;
begin
  inherited Create;
  if AFormat <> nil then
    AHandle := AFormat.FHandle
  else
    AHandle := nil;
  GdipCheck(GdipCloneStringFormat(AHandle, FHandle));
end;

constructor TdxGPStringFormat.CreateFromHandle(AHandle: GpStringFormat);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TdxGPStringFormat.Destroy;
begin
  if FHandle <> nil then
    GdipDeleteStringFormat(FHandle);
  inherited Destroy;
end;

class procedure TdxGPStringFormat.Finalize;
begin
  FreeAndNil(FGenericTypographic);
  FreeAndNil(FGenericDefault);
end;

procedure TdxGPStringFormat.Assign(ASource: TdxGPStringFormat);
begin
  GdipCheck(GdipDeleteStringFormat(FHandle));
  GdipCheck(GdipCloneStringFormat(ASource.FHandle, FHandle));
end;

class function TdxGPStringFormat.GetGenericDefault: TdxGPStringFormat;
var
  AHandle: GpStringFormat;
begin
  if FGenericDefault = nil then
  begin
    GdipCheck(GdipStringFormatGetGenericDefault(AHandle));
    FGenericDefault := TdxGPStringFormat.CreateFromHandle(AHandle);
  end;
  Result := FGenericDefault;
end;

class function TdxGPStringFormat.GetGenericTypographic: TdxGPStringFormat;
var
  AHandle: GpStringFormat;
begin
  if FGenericTypographic = nil then
  begin
    GdipCheck(GdipStringFormatGetGenericTypographic(AHandle));
    FGenericTypographic := TdxGPStringFormat.CreateFromHandle(AHandle);
  end;
  Result := FGenericTypographic;
end;

function TdxGPStringFormat.Clone: TdxGPStringFormat;
begin
  Result := TdxGPStringFormat.Create(Self);
end;

function TdxGPStringFormat.GetAlignment: TdxGPStringAlignment;
begin
  GdipCheck(GdipGetStringFormatAlign(FHandle, Result));
end;

function TdxGPStringFormat.GetFormatFlags: Integer;
begin
  GdipCheck(GdipGetStringFormatFlags(FHandle, Result));
end;

function TdxGPStringFormat.GetHotkeyPrefix: TdxGPHotkeyPrefix;
var
  AValue: Integer;
begin
  GdipCheck(GdipGetStringFormatHotkeyPrefix(FHandle, AValue));
  Result := TdxGPHotkeyPrefix(AValue);
end;

function TdxGPStringFormat.GetLineAlignment: TdxGPStringAlignment;
begin
  GdipCheck(GdipGetStringFormatLineAlign(FHandle, Result));
end;

function TdxGPStringFormat.GetTrimming: TdxGPStringTrimming;
begin
  GdipCheck(GdipGetStringFormatTrimming(FHandle, Result));
end;

procedure TdxGPStringFormat.SetTabStops(AFirstTabOffset: Single; ACount: Integer; const ATabStops: TArray<Single>);
begin
  GdipCheck(GdipSetStringFormatTabStops(FHandle, AFirstTabOffset, ACount, @ATabStops[0]));
end;

function TdxGPStringFormat.GetTabStopCount: Integer;
begin
  GdipCheck(GdipGetStringFormatTabStopCount(FHandle, Result));
end;

function TdxGPStringFormat.GetTabStops(ACount: Integer; out AFirstTabOffset: Single): TArray<Single>;
begin
  SetLength(Result, ACount);
  GdipCheck(GdipGetStringFormatTabStops(FHandle, ACount, @AFirstTabOffset, @Result[0]));
end;

procedure TdxGPStringFormat.SetDigitSubstitution(ALanguage: LANGID; ASubstitute: TdxGPStringDigitSubstitute);
begin
  GdipCheck(GdipSetStringFormatDigitSubstitution(FHandle, ALanguage, ASubstitute));
end;

function TdxGPStringFormat.GetDigitSubstitutionLanguage: LANGID;
begin
  GdipCheck(GdipGetStringFormatDigitSubstitution(FHandle, @Result, nil));
end;

function TdxGPStringFormat.GetDigitSubstitutionMethod: TdxGPStringDigitSubstitute;
begin
  GdipCheck(GdipGetStringFormatDigitSubstitution(FHandle, nil, @Result));
end;

procedure TdxGPStringFormat.SetAlignment(const AValue: TdxGPStringAlignment);
begin
  GdipCheck(GdipSetStringFormatAlign(FHandle, AValue));
end;

procedure TdxGPStringFormat.SetFormatFlags(const AValue: Integer);
begin
  GdipCheck(GdipSetStringFormatFlags(FHandle, AValue));
end;

procedure TdxGPStringFormat.SetHotkeyPrefix(const AValue: TdxGPHotkeyPrefix);
begin
  GdipCheck(GdipSetStringFormatHotkeyPrefix(FHandle, Ord(AValue)));
end;

procedure TdxGPStringFormat.SetLineAlignment(const AValue: TdxGPStringAlignment);
begin
  GdipCheck(GdipSetStringFormatLineAlign(FHandle, AValue));
end;

procedure TdxGPStringFormat.SetTrimming(const AValue: TdxGPStringTrimming);
begin
  GdipCheck(GdipSetStringFormatTrimming(FHandle, AValue));
end;

procedure TdxGPStringFormat.SetMeasurableCharacterRanges(ACount: Integer; const ARanges: TArray<TdxGPCharacterRange>);
begin
  GdipCheck(GdipSetStringFormatMeasurableCharacterRanges(FHandle, ACount, @ARanges[0]));
end;

function TdxGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  GdipCheck(GdipGetStringFormatMeasurableCharacterRangeCount(FHandle, Result));
end;

{ TdxGPFontFamily }

constructor TdxGPFontFamily.Create(const AName: string{TODO: ; AFontCollection: TdxGPFontCollection});
begin
  inherited Create;
  GdipCreateFontFamilyFromName(PChar(AName), nil{TODO: AFontCollection.Handle}, FHandle);
end;

constructor TdxGPFontFamily.Create(AHandle: GpFontFamily);
begin
  inherited Create;
  GdipCheck(GdipCloneFontFamily(AHandle, FHandle));
end;

destructor TdxGPFontFamily.Destroy;
begin
  if FHandle <> nil then
    GdipDeleteFontFamily(FHandle);
  inherited Destroy;
end;

class procedure TdxGPFontFamily.Finalize;
begin
  FreeAndNil(FGenericSansSerif);
  FreeAndNil(FGenericSerif);
  FreeAndNil(FGenericMonospace);
end;

function TdxGPFontFamily.Clone: TdxGPFontFamily;
begin
  Result := TdxGPFontFamily.Create;
  GdipCheck(GdipCloneFontFamily(FHandle, Result.FHandle));
end;

function TdxGPFontFamily.GetCellAscent(AStyle: Integer): Word;
begin
  GdipCheck(GdipGetCellAscent(FHandle, AStyle, Result));
end;

function TdxGPFontFamily.GetCellDescent(AStyle: Integer): Word;
begin
  GdipCheck(GdipGetCellDescent(FHandle, AStyle, Result));
end;

function TdxGPFontFamily.GetEmHeight(AStyle: Integer): Word;
begin
  GdipCheck(GdipGetEmHeight(FHandle, AStyle, Result));
end;

function TdxGPFontFamily.GetNameProperty: string;
begin
  Result := GetName(0);
end;

class function TdxGPFontFamily.GetGenericMonospace: TdxGPFontFamily;
begin
  if FGenericMonospace = nil then
  begin
    FGenericMonospace := TdxGPFontFamily.Create;
    GdipCheck(GdipGetGenericFontFamilyMonospace(FGenericMonospace.FHandle));
  end;
  Result := FGenericMonospace;
end;

class function TdxGPFontFamily.GetGenericSansSerif: TdxGPFontFamily;
begin
  if FGenericSansSerif = nil then
  begin
    FGenericSansSerif := TdxGPFontFamily.Create;
    GdipCheck(GdipGetGenericFontFamilySansSerif(FGenericSansSerif.FHandle));
  end;
  Result := FGenericSansSerif;
end;

class function TdxGPFontFamily.GetGenericSerif: TdxGPFontFamily;
begin
  if FGenericSerif = nil then
  begin
    FGenericSerif := TdxGPFontFamily.Create;
    GdipCheck(GdipGetGenericFontFamilySerif(FGenericSerif.FHandle));
  end;
  Result := FGenericSerif;
end;

function TdxGPFontFamily.GetLineSpacing(AStyle: Integer): Word;
begin
  GdipCheck(GdipGetLineSpacing(FHandle, AStyle, Result));
end;

function TdxGPFontFamily.GetName(ALanguage: LANGID): string;
begin
  SetLength(Result, 32);
  GdipCheck(GdipGetFamilyName(FHandle, @Result[1], ALanguage));
  Result := PChar(Result);
end;

function TdxGPFontFamily.IsStyleAvailable(AStyle: Integer): Boolean;
var
  AResult: BOOL;
begin
  GdipCheck(GdipIsStyleAvailable(FHandle, AStyle, AResult));
  Result := AResult;
end;

{ TdxGPFontCollection }

constructor TdxGPFontCollection.Create;
begin
  inherited Create;
  CreateHandle;
end;

destructor TdxGPFontCollection.Destroy;
begin
  ClearFamilies;
  FreeHandle;
  inherited Destroy;
end;

procedure TdxGPFontCollection.FreeHandle;
begin
// do nothing
end;

function TdxGPFontCollection.GetFamilyCount: Integer;
begin
  GdipCheck(GdipGetFontCollectionFamilyCount(FHandle, Result));
end;

function TdxGPFontCollection.GetFamilies: TdxGPFontFamilies;
var
  I, AActualCount, ACount: Integer;
  AFamilyArray: array of GpFontFamily;
begin
  ClearFamilies;
  ACount := FamilyCount;
  SetLength(AFamilyArray, ACount);
  GdipCheck(GdipGetFontCollectionFamilyList(FHandle, ACount, AFamilyArray, AActualCount));
  SetLength(FFamilies, AActualCount);
  for I := 0 to AActualCount - 1 do
    FFamilies[I] := TdxGPFontFamily.Create(AFamilyArray[I]);
  Result := FFamilies;
end;

procedure TdxGPFontCollection.ClearFamilies;
var
  I: Integer;
begin
  for I := 0 to Length(FFamilies) - 1 do
    FFamilies[I].Free;
end;

{ TdxGPInstalledFontCollection }

procedure TdxGPInstalledFontCollection.CreateHandle;
begin
  GdipCheck(GdipNewInstalledFontCollection(FHandle));
end;

{ TdxGPPrivateFontCollection }

procedure TdxGPPrivateFontCollection.CreateHandle;
begin
  GdipCheck(GdipNewPrivateFontCollection(FHandle));
end;

procedure TdxGPPrivateFontCollection.FreeHandle;
begin
  GdipCheck(GdipDeletePrivateFontCollection(FHandle));
end;

{ TdxFont }

constructor TdxGPFont.Create(ANativeFont: GpFont; AGdiCharSet: Byte; AGdiVerticalFont: Boolean);
var
  ASize: Single;
  AUnit: TdxGraphicUnit;
  AStyle: Integer;
  ANativeFamily: GpFontFamily;
begin
  inherited Create;
  FGdiCharSet := DEFAULT_CHARSET;

  AUnit := TdxGraphicUnit.guPoint;
  AStyle := 0;
  ANativeFamily := nil;

  FNativeFont := ANativeFont;

  GdipCheck(GdipGetFontUnit(ANativeFont, AUnit));
  GdipCheck(GdipGetFontSize(ANativeFont, ASize));
  GdipCheck(GdipGetFontStyle(ANativeFont, AStyle));
  GdipCheck(GdipGetFamily(ANativeFont, ANativeFamily));

  FIsOwnFontFamily := True;
  FFontFamily := TdxGPFontFamily.Create(ANativeFamily);

  Initialize(FFontFamily, ASize, TdxGPFontStyle(AStyle), AUnit, AGdiCharSet, AGdiVerticalFont);
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single);
begin
  Initialize(AFamilyName, AEmSize, FontStyleRegular, guPoint, DEFAULT_CHARSET, IsVerticalName(AFamilyName));
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single; AUnit: TdxGraphicUnit);
begin
  inherited Create;
  Initialize(AFamilyName, AEmSize, FontStyleRegular, AUnit, DEFAULT_CHARSET, IsVerticalName(AFamilyName));
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle);
begin
  inherited Create;
  Initialize(AFamilyName, AEmSize, AStyle, guPoint, DEFAULT_CHARSET, IsVerticalName(AFamilyName));
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit);
begin
  inherited Create;
  Initialize(AFamilyName, AEmSize, AStyle, AUnit, DEFAULT_CHARSET, IsVerticalName(AFamilyName));
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, FontStyleRegular, guPoint, DEFAULT_CHARSET, False);
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single; AUnit: TdxGraphicUnit);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, FontStyleRegular, AUnit, DEFAULT_CHARSET, False);
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, AStyle, guPoint, DEFAULT_CHARSET, False);
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single;
  AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean);
begin
  if IsNaN(AEmSize) or IsInfinite(AEmSize) or (AEmSize <= 0) then
    GdipCheck(InvalidParameter);
  Initialize(AFamilyName, AEmSize, AStyle, AUnit, AGdiCharSet, AGdiVerticalFont);
end;

constructor TdxGPFont.Create(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle;
  AUnit: TdxGraphicUnit; AGdiCharSet: Byte);
begin
  inherited Create;
  Initialize(AFamilyName, AEmSize, AStyle, AUnit, AGdiCharSet, IsVerticalName(AFamilyName));
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle;
  AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, AStyle, AUnit, AGdiCharSet, AGdiVerticalFont);
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single;
  AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, AStyle, AUnit, AGdiCharSet, False);
end;

constructor TdxGPFont.Create(AFamily: TdxGPFontFamily; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit);
begin
  inherited Create;
  Initialize(AFamily, AEmSize, AStyle, AUnit, DEFAULT_CHARSET, False);
end;

constructor TdxGPFont.Create(APrototype: TdxGPFont; ANewStyle: TdxGPFontStyle);
begin
  inherited Create;
  FOriginalFontName := APrototype.OriginalFontName;
  Initialize(APrototype.FontFamily, APrototype.Size, ANewStyle, APrototype.&Unit, DEFAULT_CHARSET, False);
end;

destructor TdxGPFont.Destroy;
begin
  if FNativeFont <> nil then
  begin
    GdipDeleteFont(FNativeFont);
    FNativeFont := nil;
  end;
  if FIsOwnFontFamily then
    FreeAndNil(FFontFamily);
  inherited Destroy;
end;

procedure TdxGPFont.CreateNativeFont;
begin
  GdipCheck(GdipCreateFont(FFontFamily.Handle, FFontSize, Ord(FFontStyle), Ord(FFontUnit), FNativeFont));
end;

class function TdxGPFont.SafeCreate(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle;
  AUnit: TdxGraphicUnit = guPoint; AGdiCharSet: Byte = DEFAULT_CHARSET): TdxGPFont;
var
  AFontFamily: TdxGPFontFamily;
  AIsGeneralFontFamily: Boolean;
begin
  AFontFamily := TdxGPFontFamily.Create(StripVerticalName(AFamilyName){TODO: , True});
  if AFontFamily.Handle = nil then
  begin
    AFontFamily := TdxGPFontFamily.GenericSansSerif;
    AIsGeneralFontFamily := True;
  end
  else
    AIsGeneralFontFamily := False;
  Result := TdxGPFont.Create(AFontFamily, AEmSize, AStyle, AUnit, AGdiCharSet);
  if AIsGeneralFontFamily then
    Result.FIsOwnFontFamily := False;
end;

procedure TdxGPFont.Initialize(const AFamilyName: string; AEmSize: Single; AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean);
begin
  FOriginalFontName := AFamilyName;
  FIsOwnFontFamily := True;
  FFontFamily := TdxGPFontFamily.Create(StripVerticalName(AFamilyName){TODO: , True});
  Initialize(FFontFamily, AEmSize, AStyle, AUnit, AGdiCharSet, AGdiVerticalFont);
end;

procedure TdxGPFont.Initialize(AFamily: TdxGPFontFamily; AEmSize: Single;
  AStyle: TdxGPFontStyle; AUnit: TdxGraphicUnit; AGdiCharSet: Byte; AGdiVerticalFont: Boolean);
begin
  if AFamily = nil then
    GdipCheck(FontFamilyNotFound);

  if IsNaN(AEmSize) or IsInfinite(AEmSize) or (AEmSize <= 0) then
    GdipCheck(InvalidParameter);

  FFontSize := AEmSize;
  FFontStyle := Ord(AStyle);
  FFontUnit := AUnit;
  FGdiCharSet := AGdiCharSet;
  FGdiVerticalFont := AGdiVerticalFont;

  if FFontFamily = nil then
  begin
    FFontFamily := AFamily;
    FIsOwnFontFamily := False;
  end;

  if FNativeFont = nil then
    CreateNativeFont;

  GdipCheck(GdipGetFontSize(FNativeFont, FFontSize));
end;

class function TdxGPFont.FromHFont(AHFont: THandle): TdxGPFont;
var
  ALogFont: TLogFont;
  AScreenDC: THandle;
begin
  GetObject(AHfont, SizeOf(ALogFont), @ALogFont);
  AScreenDC := GetDC(0);
  try
    Result := TdxGPFont.FromLogFont(ALogFont, AScreenDC);
  finally
    ReleaseDC(0, AScreenDC);
  end;
end;

class function TdxGPFont.FromLogFont(const ALogFont: TLogFont): TdxGPFont;
var
  AScreenDC: THandle;
begin
  AScreenDC := GetDC(0);
  try
    Result := TdxGPFont.FromLogFont(ALogFont, AScreenDC);
  finally
    ReleaseDC(0, AScreenDC);
  end;
end;

class function TdxGPFont.FromLogFont(const ALogFont: TLogFont; AHdc: THandle): TdxGPFont;
var
  AFont: GpFont;
begin
  AFont := nil;
  GdipCheck(GdipCreateFontFromLogfont(AHdc, @ALogFont, AFont));
  if AFont = nil then
    GdipCheck(NotTrueTypeFont);
  Result := TdxGPFont.Create(AFont, ALogFont.lfCharSet, ALogFont.lfFaceName[0] = '@');
end;

class function TdxGPFont.FromHdc(AHdc: THandle): TdxGPFont;
var
  AFont: GpFont;
begin
  AFont := nil;
  GdipCheck(GdipCreateFontFromDC(AHdc, AFont));
  Result := TdxGPFont.Create(AFont, 0, False);
end;

function TdxGPFont.Clone: TdxGPFont;
var
  ACloneFont: GpFont;
begin
  ACloneFont := nil;
  GdipCheck(GdipCloneFont(FNativeFont, ACloneFont));
  Result := TdxGPFont.Create(ACloneFont, FGdiCharSet, FGdiVerticalFont);
end;

function TdxGPFont.GetFontFamily: TdxGPFontFamily;
begin
  Result := FFontFamily;
end;

procedure TdxGPFont.SetFontFamily(AFamily: TdxGPFontFamily);
begin
  FFontFamily := AFamily;
end;

class function TdxGPFont.IsVerticalName(const AFamilyName: string): Boolean;
begin
  Result := (Length(AFamilyName) > 0) and (AFamilyName[1] = '@');
end;

function TdxGPFont.GetBold: Boolean;
begin
  Result := (Style and Ord(FontStyleBold)) <> 0;
end;

function TdxGPFont.GetItalic: Boolean;
begin
  Result := (Style and Ord(FontStyleBold)) <> 0;
end;

function TdxGPFont.GetName: string;
begin
  Result := FontFamily.Name;
end;

function TdxGPFont.GetStrikeout: Boolean;
begin
  Result := (Style and Ord(FontStyleBold)) <> 0;
end;

function TdxGPFont.GetUnderline: Boolean;
begin
  Result := (Style and Ord(FontStyleBold)) <> 0;
end;


function TdxGPFont.GetHashCode: Integer;
begin
  Result := ((FFontStyle shl 13) or       (FFontStyle shr 19)) xor
        ((Ord(FFontUnit) shl 26) or   (Ord(FFontUnit) shr 6)) xor
      ((Round(FFontSize) shl 7)  or (Round(FFontSize) shr 25));
end;

class function TdxGPFont.StripVerticalName(const AFamilyName: string): string;
begin
  Result := AFamilyName;
  if (Length(AFamilyName) > 1) and (AFamilyName[1] = '@') then
    Delete(Result, 1, 1);
end;

procedure TdxGPFont.ToLogFont(out ALogFont: TLogFont);
var
  AScreenDC: THandle;
  ACanvas: TdxGPCanvas;
begin
  AScreenDC := GetDC(0);
  try
    ACanvas := TdxGPCanvas.Create(AScreenDC);
    try
      ToLogFont(ALogFont, ACanvas);
    finally
      ACanvas.Free;
    end;
  finally
    ReleaseDC(0, AScreenDC);
  end;
end;

procedure TdxGPFont.ToLogFont(out ALogFont: TLogFont; ACanvas: TdxGPCanvas);
begin
  if ACanvas = nil then
    GdipCheck(InvalidParameter);

  GdipCheck(GdipGetLogFontW(NativeFont, ACanvas.Handle, ALogFont));
  if FGdiVerticalFont then
  begin
    Move(ALogFont.lfFaceName[0], ALogFont.lfFaceName[1], SizeOf(ALogFont.lfFaceName) - SizeOf(Char));
    ALogFont.lfFaceName[0] := '@';
  end;
  if ALogFont.lfCharSet = 0 then
    ALogFont.lfCharSet := FGdiCharSet;
end;

function TdxGPFont.ToHfont: THandle;
var
  ALogFont: TLogFont;
begin
  ZeroMemory(@ALogFont, SizeOf(ALogFont));
  ToLogFont(ALogFont);
  Result := CreateFontIndirect(ALogFont);
end;

function TdxGPFont.GetHeight(AGraphics: TdxGPCanvas): Single;
begin
  if AGraphics = nil then
    GdipCheck(InvalidParameter);

  GdipCheck(GdipGetFontHeight(NativeFont, AGraphics.Handle, Result));
end;

function TdxGPFont.GetHeight: Single;
var
  AScreenDC: THandle;
  AGraphics: TdxGPCanvas;
begin
  AScreenDC := GetDC(0);
  try
    AGraphics := TdxGPCanvas.Create(AScreenDC);
    try
      Result := GetHeight(AGraphics);
    finally
      AGraphics.Free;
    end;
  finally
    ReleaseDC(0, AScreenDC);
  end;
end;

function TdxGPFont.GetHeight(ADpi: Single): Single;
begin
  GdipCheck(GdipGetFontHeightGivenDPI(NativeFont, ADpi, Result));
end;

function TdxGPFont.GetSizeInPoints: Single;
var
  APixelsPerPoint, ALineSpacingInPixels, AEmHeightInPixels: Single;
  AScreenDC: THandle;
  AGraphics: TdxGPCanvas;
begin
  if FFontUnit = guPoint then
    Exit(Size)
  else
  begin
    AScreenDC := GetDC(0);
    try
      AGraphics := TdxGPCanvas.Create(AScreenDC);
      try
        APixelsPerPoint := AGraphics.DpiY / 72.0;
        ALineSpacingInPixels := GetHeight(AGraphics);
        AEmHeightInPixels := ALineSpacingInPixels * FontFamily.GetEmHeight(Style) / FontFamily.GetLineSpacing(Style);

        Result := AEmHeightInPixels / APixelsPerPoint;
      finally
        AGraphics.Free;
      end;
    finally
      ReleaseDC(0, AScreenDC);
    end;
  end;
end;

function TdxGPFont.GetHeightProperty: Integer;
begin
  Result := Ceil(GetHeight);
end;

function TdxGPFont.GetIsSystemFont: Boolean;
begin
  Result := FSystemFontName <> '';
end;

procedure TdxGPFont.SetSystemFontName(const ASystemFontName: string);
begin
  FSystemFontName := ASystemFontName;
end;

{ TdxGPCanvas }

constructor TdxGPCanvas.Create;
begin
  inherited Create;
  FSavedClipRegions := TStack.Create;
  FSavedWorldTransforms := TStack.Create;
end;

constructor TdxGPCanvas.Create(AHandle: GpGraphics);
begin
  Create;
  FHandle := AHandle;
end;

constructor TdxGPCanvas.Create(DC: THandle);
begin
  Create;
  CreateHandle(DC);
end;

destructor TdxGPCanvas.Destroy;
begin
  dxTestCheck(FSavedClipRegions.Count = 0, ClassName + '.FSavedClipRegions <> 0');
  dxTestCheck(FSavedWorldTransforms.Count = 0, ClassName + '.FSavedWorldTransforms <> 0');
  FreeHandle;
  FreeAndNil(FSavedWorldTransforms);
  FreeAndNil(FSavedClipRegions);
  inherited Destroy;
end;

procedure TdxGPCanvas.CheckDestRect(var R: TRect);
begin
  Dec(R.Bottom);
  Dec(R.Right);
end;

procedure TdxGPCanvas.Clear(AColor: TColor);
begin
  GdipCheck(GdipGraphicsClear(Handle, dxColorToAlphaColor(AColor)));
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const R: TdxRectF; AAlpha: Byte = 255);
begin
  Draw(AGraphic, R, dxRectF(AGraphic.ClientRect), AAlpha);
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const R: TRect; AAlpha: Byte = 255);
begin
  Draw(AGraphic, R, AGraphic.ClientRect, AAlpha);
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const R: TRect; AAttributes: TdxGPImageAttributes);
begin
  Draw(AGraphic, dxRectF(R), dxRectF(AGraphic.ClientRect), AAttributes);
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const APoints: array of TdxPointF);
begin
  GdipCheck(GdipDrawImagePoints(Handle, AGraphic.Handle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const APoints: array of TdxPointF; AAttributes: TdxGPImageAttributes);
begin
  GdipCheck(GdipDrawImagePointsRect(Handle, AGraphic.Handle, @APoints[0], Length(APoints),  0, 0,
    AGraphic.Width, AGraphic.Height, guPixel, AAttributes.Handle, nil, nil));
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TdxRectF; AAlpha: Byte = 255);
begin
  AGraphic.StretchDraw(Self, ADestRect, ASourceRect, dxGpGetAlphaBlendAttributes(AAlpha));
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TdxRectF; AAttributes: TdxGPImageAttributes);
begin
  AGraphic.StretchDraw(Self, ADestRect, ASourceRect, AAttributes);
end;

procedure TdxGPCanvas.Draw(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TRect; AAlpha: Byte = 255);
begin
  AGraphic.StretchDraw(Self, ADestRect, ASourceRect, dxGpGetAlphaBlendAttributes(AAlpha));
end;

procedure TdxGPCanvas.DrawBitmap(ABitmap: TBitmap; const R: TRect; AAlpha: Byte = 255);
var
  AData: TdxSmartImageDataAccess;
  AImage: TdxGPImageHandle;
begin
  if not ABitmap.Empty then
  begin
    AData := TdxSmartImageDataAccess.Create;
    try
      AData.LoadFromBitmap(ABitmap);
      AImage := TdxGPImageHandle.CreateFromBits(ABitmap.Width, ABitmap.Height, AData.FBits, AData.FBitsFormat);
      try
        AImage.Draw(Self, R, cxRect(AImage.Size), dxGpGetAlphaBlendAttributes(AAlpha));
      finally
        AImage.Free;
      end;
    finally
      AData.Free;
    end;
  end;
end;

procedure TdxGPCanvas.DrawTile(AGraphic: TdxGPImage; const R: TRect; AAlpha: Byte);
begin
  DrawTile(AGraphic, R, AGraphic.ClientRect, AAlpha);
end;

procedure TdxGPCanvas.DrawTile(AGraphic: TdxGPImage; const ADestRect, ASourceRect: TRect; AAlpha: Byte);
begin
  dxGpTilePartEx(Handle, ADestRect, ASourceRect, AGraphic.Handle, AAlpha);
end;

function TdxGPCanvas.IsClipEmpty: Boolean;
var
  AValue: LongBool;
begin
  GdipCheck(GdipIsClipEmpty(FHandle, @AValue));
  Result := AValue;
end;

procedure TdxGPCanvas.RestoreClipRegion;
var
  ARegion: GpRegion;
begin
  ARegion := FSavedClipRegions.Pop;
  GdipCheck(GdipSetClipRegion(Handle, ARegion, CombineModeReplace));
  GdipCheck(GdipDeleteRegion(ARegion));
end;

procedure TdxGPCanvas.SaveClipRegion;
var
  ARegion: GpRegion;
begin
  GdipCheck(GdipCreateRegion(ARegion));
  GdipCheck(GdipGetClip(Handle, ARegion));
  FSavedClipRegions.Push(ARegion);
end;

procedure TdxGPCanvas.SetClipPath(APath: TdxGPPath; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipSetClipPath(Handle, APath.Handle, TGpCombineMode(AMode)));
end;

procedure TdxGPCanvas.SetClipRect(R: TRect; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipSetClipRectI(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), TGpCombineMode(AMode)));
end;

procedure TdxGPCanvas.SetClipRect(const R: TdxRectF; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipSetClipRect(Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), TGpCombineMode(AMode)));
end;

procedure TdxGPCanvas.SetClipRegion(ARgn: HRGN; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipSetClipHrgn(Handle, ARgn, TGpCombineMode(AMode)));
end;

procedure TdxGPCanvas.SetClipRegion(ARegion: TdxGPRegion; AMode: TdxGPCombineMode);
begin
  GdipCheck(GdipSetClipRegion(Handle, ARegion.Handle, TGpCombineMode(AMode)));
end;

procedure TdxGPCanvas.Arc(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
var
  APen: IdxGPHandle;
begin
  CheckDestRect(R);
  if TdxGPResourceManager.GetPen(APenColor, APenColorAlpha, APenWidth, APenStyle, APen) then
    GdipCheck(GdipDrawArcI(Handle, APen.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), -AStartAngle, -ASweepAngle));
end;

procedure TdxGPCanvas.Arc(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TdxAlphaColor;
  APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AColor: TColor;
  AAlpha: Byte;
begin
  AColor := dxAlphaColorToColor(APenColor, AAlpha);
  Arc(R, AStartAngle, ASweepAngle, AColor, APenWidth, APenStyle, AAlpha);
end;

procedure TdxGPCanvas.Curve(const APoints: array of TPoint; APenColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
begin
  Curve(APoints, dxColorToAlphaColor(APenColor, APenColorAlpha), APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Curve(const APoints: array of TPoint; APenColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  APen: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, APen) then
    GdipCheck(GdipDrawCurveI(Handle, APen.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Curve(const APoints: array of TdxPointF; APenColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
var
  APen: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenColorAlpha, APenWidth, APenStyle, APen) then
    GdipCheck(GdipDrawCurve(Handle, APen.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Ellipse(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
begin
  CheckDestRect(R);

  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(R);
    GdipCheck(GdipFillEllipseI(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;

  if APen <> nil then
  begin
    APen.SetTargetRect(R);
    GdipCheck(GdipDrawEllipseI(Handle, APen.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;
end;

procedure TdxGPCanvas.Ellipse(R: TdxRectF; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
begin
  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(R);
    GdipCheck(GdipFillEllipse(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;

  if APen <> nil then
  begin
    APen.SetTargetRect(R);
    GdipCheck(GdipDrawEllipse(Handle, APen.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;
end;

procedure TdxGPCanvas.Ellipse(R: TRect; APenColor, ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Ellipse(R,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Ellipse(R: TRect; APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  CheckDestRect(R);
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillEllipseI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawEllipseI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.Ellipse(R: TdxRectF; APenColor, ABrushColor: TdxAlphaColor;
  APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillEllipse(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawEllipse(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.Ellipse(R: TdxRectF; APenColor, ABrushColor: TColor; APenWidth: Single;
  APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Ellipse(R,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Line(X1, Y1, X2, Y2: Integer; APenColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
begin
  Line(X1, Y1, X2, Y2, dxColorToAlphaColor(APenColor, APenColorAlpha), APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Line(X1, Y1, X2, Y2: Integer; APenColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawLineI(Handle, AHandle.GetNativeHandle, X1, Y1, X2, Y2));
end;

procedure TdxGPCanvas.Line(X1, Y1, X2, Y2: Single; APenColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawLine(Handle, AHandle.GetNativeHandle, X1, Y1, X2, Y2));
end;

procedure TdxGPCanvas.Line(X1, Y1, X2, Y2: Integer; APen: TdxGPPen);
begin
  GdipCheck(GdipDrawLineI(Handle, APen.Handle, X1, Y1, X2, Y2));
end;

procedure TdxGPCanvas.Line(X1, Y1, X2, Y2: Single; APen: TdxGPPen);
begin
  GdipCheck(GdipDrawLine(Handle, APen.Handle, X1, Y1, X2, Y2));
end;

procedure TdxGPCanvas.Path(APath: TdxGPPath; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
var
  R: TdxRectF;
begin
  R := APath.GetBoundsF;
  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(R);
    GdipCheck(GdipFillPath(Handle, ABrush.Handle, APath.Handle));
  end;

  if (APen <> nil) and not APen.IsEmpty then
  begin
    APen.SetTargetRect(R);
    GdipCheck(GdipDrawPath(Handle, APen.Handle, APath.Handle));
  end;
end;

procedure TdxGPCanvas.Path(APath: TdxGPPath; APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillPath(Handle, AHandle.GetNativeHandle, APath.Handle));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPath(Handle, AHandle.GetNativeHandle, APath.Handle));
end;

procedure TdxGPCanvas.Path(APath: TdxGPPath; APenColor, ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Path(APath,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Pie(R: TRect; AStartAngle, ASweepAngle: Single; APenColor: TColor; ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Pie(R, AStartAngle, ASweepAngle,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Pie(R: TRect; AStartAngle, ASweepAngle: Single;
  APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  CheckDestRect(R);
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillPieI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), -AStartAngle, -ASweepAngle));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPieI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), -AStartAngle, -ASweepAngle));
end;

procedure TdxGPCanvas.Pie(R: TdxRectF; AStartAngle, ASweepAngle: Single;
  APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillPie(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), -AStartAngle, -ASweepAngle));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPie(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), -AStartAngle, -ASweepAngle));
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TPoint; APenColor, ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Polygon(APoints,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TPoint;
  APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillPolygonI(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints), FillModeWinding));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPolygonI(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TdxPointF;
  APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single; APenStyle: TdxGPPenStyle);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillPolygon(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints), FillModeWinding));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPolygon(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TdxPointF; APenColor, ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, ABrushColorAlpha, AHandle) then
    GdipCheck(GdipFillPolygon(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints), FillModeWinding));
  if TdxGPResourceManager.GetPen(APenColor, APenColorAlpha, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawPolygon(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TdxPointF; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
var
  ABox: TdxRectF;
begin
  ABox := cxPointsBox(APoints);
  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(ABox);
    GdipCheck(GdipFillPolygon(Handle, ABrush.Handle, @APoints[0], Length(APoints), FillModeWinding));
  end;
  if APen <> nil then
  begin
    APen.SetTargetRect(ABox);
    GdipCheck(GdipDrawPolygon(Handle, APen.Handle, @APoints[0], Length(APoints)));
  end;
end;

procedure TdxGPCanvas.Polygon(const APoints: array of TPoint; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
var
  ABox: TRect;
begin
  ABox := cxPointsBox(APoints);
  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(ABox);
    GdipCheck(GdipFillPolygonI(Handle, ABrush.Handle, @APoints[0], Length(APoints), FillModeWinding));
  end;
  if APen <> nil then
  begin
    APen.SetTargetRect(ABox);
    GdipCheck(GdipDrawPolygonI(Handle, APen.Handle, @APoints[0], Length(APoints)));
  end;
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TPoint; APenColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
begin
  Polyline(APoints, dxColorToAlphaColor(APenColor, APenColorAlpha), APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TPoint;
  APenColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawLinesI(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TdxPointF;
  APenColor: TColor; APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha: Byte);
begin
  Polyline(APoints, dxColorToAlphaColor(APenColor, APenColorAlpha), APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TdxPointF;
  APenColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawLines(Handle, AHandle.GetNativeHandle, @APoints[0], Length(APoints)));
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TdxPointF; APen: TdxGPPen);
begin
  if (APen <> nil) and not APen.IsEmpty then
  begin
    APen.SetTargetRect(cxPointsBox(APoints));
    GdipCheck(GdipDrawLines(Handle, APen.Handle, @APoints[0], Length(APoints)));
  end;
end;

procedure TdxGPCanvas.Polyline(const APoints: array of TPoint; APen: TdxGPPen);
begin
  if (APen <> nil) and not APen.IsEmpty then
  begin
    APen.SetTargetRect(cxPointsBox(APoints));
    GdipCheck(GdipDrawLinesI(Handle, APen.Handle, @APoints[0], Length(APoints)));
  end;
end;

procedure TdxGPCanvas.FillRectangle(const R: TdxRectF; ABrushColor: TdxAlphaColor);
var
  ABrush: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, ABrush) then
    GdipCheck(GdipFillRectangle(Handle, ABrush.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.FillRectangle(const R: TRect; ABrushColor: TdxAlphaColor);
var
  ABrush: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, ABrush) then
    GdipCheck(GdipFillRectangleI(Handle, ABrush.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.FillRectangle(const R: TdxRectF; ABrush: TdxGPCustomBrush);
begin
  ABrush.SetTargetRect(R);
  GdipCheck(GdipFillRectangle(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.FillRectangle(const R: TRect; ABrush: TdxGPCustomBrush);
begin
  ABrush.SetTargetRect(R);
  GdipCheck(GdipFillRectangleI(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.FillRectangleByGradient(const R: TRect; AColor1, AColor2: TdxAlphaColor; AMode: TdxGPLinearGradientMode);
var
  ABrush: GpLineGradient;
  ABrushRect: TdxGPRect;
begin
  if (AColor1 <> AColor2) and (AColor2 <> dxacDefault) then
  begin
    // Inflate: Avoid GDIPlus gradient fill bug
    ABrushRect := MakeRect(R.Left - 1, R.Top - 1, R.Right - R.Left + 2, R.Bottom - R.Top + 2);
    GdipCheck(GdipCreateLineBrushFromRectI(@ABrushRect, AColor1, AColor2, AMode, WrapModeTile, ABrush));
    GdipCheck(GdipFillRectangleI(Handle, ABrush, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top));
    GdipCheck(GdipDeleteBrush(ABrush));
  end
  else
    FillRectangle(R, AColor1);
end;

procedure TdxGPCanvas.Rectangle(const R: TdxRectF; APenColor, ABrushColor: TdxAlphaColor;
  APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  AHandle: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillRectangle(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawRectangle(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.Rectangle(const R: TdxRectF; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
begin
  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(R);
    GdipCheck(GdipFillRectangle(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;

  if APen <> nil then
  begin
    APen.SetTargetRect(R);
    GdipCheck(GdipDrawRectangle(Handle, APen.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;
end;

procedure TdxGPCanvas.Rectangle(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush);
begin
  CheckDestRect(R);

  if ABrush <> nil then
  begin
    ABrush.SetTargetRect(R);
    GdipCheck(GdipFillRectangleI(Handle, ABrush.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;

  if APen <> nil then
  begin
    APen.SetTargetRect(R);
    GdipCheck(GdipDrawRectangleI(Handle, APen.Handle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  end;
end;

procedure TdxGPCanvas.Rectangle(R: TRect; APenColor, ABrushColor: TColor;
  APenWidth: Single; APenStyle: TPenStyle; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  Rectangle(R,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    APenWidth, APenStyle);
end;

procedure TdxGPCanvas.Rectangle(R: TRect; APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single; APenStyle: TPenStyle);
var
  AHandle: IdxGPHandle;
begin
  CheckDestRect(R);
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, AHandle) then
    GdipCheck(GdipFillRectangleI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
  if TdxGPResourceManager.GetPen(APenColor, APenWidth, APenStyle, AHandle) then
    GdipCheck(GdipDrawRectangleI(Handle, AHandle.GetNativeHandle, R.Left, R.Top, cxRectWidth(R), cxRectHeight(R)));
end;

procedure TdxGPCanvas.Region(ARegion: TdxGPRegion; APenColor, ABrushColor: TdxAlphaColor; APenWidth: Single = 1; APenStyle: TPenStyle = psSolid);
var
  ABrush: IdxGPHandle;
begin
  if TdxGPResourceManager.GetSolidBrush(ABrushColor, ABrush) then
    GdipCheck(GdipFillRegion(Handle, ABrush.GetNativeHandle, ARegion.Handle));
end;

procedure TdxGPCanvas.RoundRect(R: TRect; APen: TdxGPPen; ABrush: TdxGPCustomBrush; ARadiusX, ARadiusY: Integer);
var
  APath: TdxGPPath;
begin
  APath := TdxGPPath.Create;
  try
    CheckDestRect(R);
    APath.AddRoundRect(R, ARadiusX, ARadiusY);
    Path(APath, APen, ABrush);
  finally
    APath.Free;
  end;
end;

procedure TdxGPCanvas.RoundRect(R: TRect; APenColor, ABrushColor: TColor; ARadiusX, ARadiusY: Integer;
  APenWidth: Integer; APenColorAlpha, ABrushColorAlpha: Byte);
begin
  RoundRect(R,
    dxColorToAlphaColor(APenColor, APenColorAlpha),
    dxColorToAlphaColor(ABrushColor, ABrushColorAlpha),
    ARadiusX, ARadiusY, APenWidth);
end;

procedure TdxGPCanvas.RoundRect(R: TRect; APenColor, ABrushColor: TdxAlphaColor; ARadiusX, ARadiusY: Integer; APenWidth: Single);
var
  APath: TdxGPPath;
begin
  APath := TdxGPPath.Create;
  try
    CheckDestRect(R);
    APath.AddRoundRect(R, ARadiusX, ARadiusY);
    Path(APath, APenColor, ABrushColor, APenWidth, psSolid);
  finally
    APath.Free;
  end;
end;


procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush; X, Y: Single);
begin
  DrawString(S, AFont, ABrush, cxRectFBounds(X, Y, 0, 0), nil);
end;

procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush;
  const APoint: TdxPointF);
begin
  DrawString(S, AFont, ABrush, cxRectFBounds(APoint.X, APoint.Y, 0, 0), nil);
end;

procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush;
  const ARect: TdxRectF);
begin
  if cxRectIsEmpty(ARect) then
    Exit;
  DrawString(S, AFont, ABrush, ARect, nil);
end;

procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush;
  X, Y: Single; AFormat: TdxGPStringFormat);
begin
  DrawString(S, AFont, ABrush, cxRectFBounds(X, Y, 0, 0), AFormat);
end;

procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush;
  const APoint: TdxPointF; AFormat: TdxGPStringFormat);
begin
  DrawString(S, AFont, ABrush, cxRectFBounds(APoint.X, APoint.Y, 0, 0), AFormat);
end;

procedure TdxGPCanvas.DrawString(const S: string; AFont: TdxGPFont; ABrush: TdxGPCustomBrush;
  const ARect: TdxRectF; AFormat: TdxGPStringFormat);
var
  AGpRectF: TdxGpRectF;
  ANativeStringFormat: GpStringFormat;
begin
  if (ABrush = nil) or (AFont = nil) then
    GdipCheck(InvalidParameter);

  if Length(S) = 0 then
    Exit;

  if AFormat = nil then
    ANativeStringFormat := nil
  else
    ANativeStringFormat := AFormat.Handle;

  AGpRectF := MakeRect(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  GdipCheck(GdipDrawString(Handle, PChar(S), Length(S), AFont.NativeFont, @AGpRectF, ANativeStringFormat, ABrush.Handle));
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; ALayoutRect: TdxGpRectF;
  AStringFormat: TdxGPStringFormat; out ACharactersFitted, ALinesFilled: Integer): TdxSizeF;
var
  ABoundingBox: TdxGpRectF;
  ANativeFormat: GpStringFormat;
begin
  if AText = '' then
  begin
    ACharactersFitted := 0;
    ALinesFilled := 0;
    Result.cx := 0;
    Result.cy := 0;
    Exit;
  end;

  if AFont = nil then
    GdipCheck(InvalidParameter);

  ABoundingBox.Empty;

  if AStringFormat = nil then
    ANativeFormat := nil
  else
    ANativeFormat := AStringFormat.Handle;

  GdipCheck(GdipMeasureString(Handle, PChar(AText), Length(AText), AFont.NativeFont, @ALayoutRect,
    ANativeFormat, @ABoundingBox, ACharactersFitted, ALinesFilled));

  Result := ABoundingBox.SizeF;
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF;
  AStringFormat: TdxGPStringFormat; out ACharactersFitted, ALinesFilled: Integer): TdxSizeF;
var
  ALayoutRect: TdxGpRectF;
begin
  ALayoutRect.Init(0, 0, ALayoutArea.cx, ALayoutArea.cy);
  Result := MeasureString(AText, AFont, ALayoutRect, AStringFormat, ACharactersFitted, ALinesFilled);
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const AOrigin: TdxPointF;
  AStringFormat: TdxGPStringFormat): TdxSizeF;
var
  ALayoutRect: TdxGpRectF;
  A, B: Integer;
begin
  ALayoutRect.Init(AOrigin.X, AOrigin.Y, 0, 0);
  Result := MeasureString(AText, AFont, ALayoutRect, AStringFormat, A, B);
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF): TdxSizeF;
begin
  Result := MeasureString(AText, AFont, ALayoutArea, nil);
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const ALayoutArea: TdxSizeF;
  AStringFormat: TdxGPStringFormat): TdxSizeF;
var
  ALayoutRect: TdxGpRectF;
  A, B: Integer;
begin
  ALayoutRect.Init(0, 0, ALayoutArea.cx, ALayoutArea.cy);
  Result := MeasureString(AText, AFont, ALayoutRect, AStringFormat, A, B);
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF;
begin
  Result := MeasureString(AText, AFont, TdxSizeF.Null);
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const AWidth: Double): TdxSizeF;
begin
  Result := MeasureString(AText, AFont, TdxSizeF.Create(AWidth, 999999));
end;

function TdxGPCanvas.MeasureString(const AText: string; AFont: TdxGPFont; const AWidth: Double; AFormat: TdxGPStringFormat): TdxSizeF;
begin
  Result := MeasureString(AText, AFont, TdxSizeF.Create(AWidth, 999999), AFormat);
end;

function TdxGPCanvas.MeasureCharacterRanges(const AText: string; AFont: TdxGPFont; const ALayoutRect: TdxRectF;
  AStringFormat: TdxGPStringFormat): TArray<TdxGPRegion>;
var
  I, ACount: Integer;
  AGpRegions: TArray<GpRegion>;
  AGPLayoutRect: TdxGpRectF;
  ANativeFormat: GpStringFormat;
begin
  if Length(AText) = 0 then
    Exit(nil);

  if AFont = nil then
    GdipCheck(InvalidParameter);

  if AStringFormat = nil then
    ANativeFormat := AStringFormat.Handle
  else
    ANativeFormat := nil;

  GdipCheck(GdipGetStringFormatMeasurableCharacterRangeCount(ANativeFormat, ACount));

  SetLength(AGpRegions, ACount);
  try
    AGPLayoutRect := ALayoutRect;
    SetLength(Result, ACount);
    for I := 0 to ACount - 1 do
    begin
      Result[I] := TdxGPRegion.Create;
      AGpRegions[I] := Result[I].Handle;
    end;
    GdipCheck(GdipMeasureCharacterRanges(Handle, PChar(AText), Length(AText), AFont.NativeFont, @AGPLayoutRect, ANativeFormat, ACount, @AGpRegions[0]));
  finally
    AGpRegions := nil;
  end;
end;

procedure TdxGPCanvas.FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TdxPointF);
begin
  FlipWorldTransform(AFlipHorizontally, AFlipVertically, APivotPoint.X, APivotPoint.Y);
end;

procedure TdxGPCanvas.FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPoint: TPoint);
begin
  FlipWorldTransform(AFlipHorizontally, AFlipVertically, APivotPoint.X, APivotPoint.Y);
end;

procedure TdxGPCanvas.FlipWorldTransform(AFlipHorizontally, AFlipVertically: Boolean; const APivotPointX, APivotPointY: Single);
var
  AMatrix: TdxGPMatrix;
begin
  if AFlipHorizontally or AFlipVertically then
  begin
    AMatrix := TdxGPMatrix.CreateFlip(AFlipHorizontally, AFlipVertically, APivotPointX, APivotPointY);
    try
      ModifyWorldTransform(AMatrix);
    finally
      AMatrix.Free;
    end;
  end;
end;

function TdxGPCanvas.GetWorldTransform: TdxGPMatrix;
begin
  Result := TdxGPMatrix.Create;
  GdipCheck(GdipGetWorldTransform(Handle, Result.Handle));
end;

procedure TdxGPCanvas.ModifyWorldTransform(AMatrix: TdxGPMatrix; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  GdipCheck(GdipMultiplyWorldTransform(Handle, AMatrix.Handle, AOrder));
end;

procedure TdxGPCanvas.RotateWorldTransform(AAngle: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  GdipCheck(GdipRotateWorldTransform(Handle, AAngle, AOrder));
end;

procedure TdxGPCanvas.RotateWorldTransform(AAngle: Single;
  const APivotPoint: TPoint; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  RotateWorldTransform(AAngle, dxPointF(APivotPoint), AOrder);
end;

procedure TdxGPCanvas.RotateWorldTransform(AAngle: Single; const APivotPoint: TdxPointF; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
const
  DirectionMap: array[TdxGpMatrixOrder] of Integer = (1, -1);
begin
  TranslateWorldTransform(DirectionMap[AOrder] * APivotPoint.X, DirectionMap[AOrder] * APivotPoint.Y, AOrder);
  RotateWorldTransform(AAngle, AOrder);
  TranslateWorldTransform(-DirectionMap[AOrder] * APivotPoint.X, -DirectionMap[AOrder] * APivotPoint.Y, AOrder);
end;

procedure TdxGPCanvas.ScaleWorldTransform(AScaleX, AScaleY: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  GdipCheck(GdipScaleWorldTransform(Handle, AScaleX, AScaleY, AOrder));
end;

procedure TdxGPCanvas.ScaleWorldTransform(AScaleX, AScaleY: Single; const ACenter: TdxPointF;
  AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
var
  AMatrix: TdxGPMatrix;
begin
  AMatrix := TdxGPMatrix.CreateEx(AScaleX, 0.0, 0.0, AScaleY, ACenter.X - AScaleX * ACenter.X, ACenter.Y - AScaleY * ACenter.Y);
  try
    GdipCheck(GdipSetWorldTransform(Handle, AMatrix.Handle));
  finally
    AMatrix.Free;
  end;
end;

procedure TdxGPCanvas.SetWorldTransform(AMatrix: TdxGPMatrix);
begin
  GdipCheck(GdipSetWorldTransform(Handle, AMatrix.Handle));
end;

procedure TdxGPCanvas.TranslateWorldTransform(AOffsetX, AOffsetY: Single; AOrder: TdxGpMatrixOrder = MatrixOrderPrepend);
begin
  GdipCheck(GdipTranslateWorldTransform(Handle, AOffsetX, AOffsetY, AOrder));
end;

procedure TdxGPCanvas.ResetWorldTransform;
begin
  GdipCheck(GdipResetWorldTransform(Handle));
end;

procedure TdxGPCanvas.RestoreWorldTransform;
var
  AMatrix: GpMatrix;
begin
  AMatrix := FSavedWorldTransforms.Pop;
  GdipCheck(GdipSetWorldTransform(Handle, AMatrix));
  GdipCheck(GdipDeleteMatrix(AMatrix));
end;

procedure TdxGPCanvas.SaveWorldTransform;
var
  AMatrix: GpMatrix;
begin
  GdipCheck(GdipCreateMatrix(AMatrix));
  GdipCheck(GdipGetWorldTransform(Handle, AMatrix));
  FSavedWorldTransforms.Push(AMatrix);
end;

procedure TdxGPCanvas.TransformPoints(ADestSpace, ASrcSpace: TdxGPCoordinateSpace; var APoints: TArray<TdxPointF>);
begin
  if Length(APoints) = 0 then
    raise EArgumentOutOfRangeException.Create('points');
  GdipCheck(GdipTransformPoints(Handle, ADestSpace, ASrcSpace, Pointer(APoints), Length(APoints)));
end;

procedure TdxGPCanvas.TransformPoints(ADestSpace, ASrcSpace: TdxGPCoordinateSpace; var APoints: TArray<TPoint>);
begin
  if Length(APoints) = 0 then
    raise EArgumentOutOfRangeException.Create('points');
  GdipCheck(GdipTransformPointsI(Handle, ADestSpace, ASrcSpace, Pointer(APoints), Length(APoints)));
end;

procedure TdxGPCanvas.CreateHandle(DC: HDC);
begin
  GdipCheck(GdipCreateFromHDC(DC, FHandle));
end;

procedure TdxGPCanvas.FreeHandle;
begin
  if FHandle <> nil then
  begin
    GdipCheck(GdipDeleteGraphics(FHandle));
    FHandle := nil;
  end;
end;

function TdxGPCanvas.GetHDC: HDC;
begin
  GdipCheck(GdipGetDC(Handle, Result));
end;

procedure TdxGPCanvas.ReleaseHDC(DC: HDC);
begin
  GdipCheck(GdipReleaseDC(Handle, DC));
end;

function TdxGPCanvas.GetDpiX: Single;
begin
  GdipCheck(GdipGetDpiX(Handle, Result));
end;

function TdxGPCanvas.GetDpiY: Single;
begin
  GdipCheck(GdipGetDpiY(Handle, Result));
end;

function TdxGPCanvas.GetInterpolationMode: TdxGPInterpolationMode;
var
  AResult: Integer;
begin
  GdipCheck(GdipGetInterpolationMode(Handle, AResult));
  if AResult in [Ord(Low(TdxGPInterpolationMode)) .. Ord(High(TdxGPInterpolationMode))] then
    Result := TdxGPInterpolationMode(AResult)
  else
    Result := imDefault;
end;

function TdxGPCanvas.GetPixelOffsetMode: TdxGpPixelOffsetMode;
begin
  GdipCheck(GdipGetPixelOffsetMode(Handle, Result));
end;

function TdxGPCanvas.GetSmoothingMode: TdxGPSmoothingMode;
var
  AResult: Integer;
begin
  GdipCheck(GdipGetSmoothingMode(Handle, AResult));
  if AResult in [Ord(Low(TdxGPSmoothingMode)) .. Ord(High(TdxGPSmoothingMode))] then
    Result := TdxGPSmoothingMode(AResult)
  else
    Result := smDefault;
end;

function TdxGPCanvas.GetTextRenderingHint: TdxGpTextRenderingHint;
begin
  GdipCheck(GdipGetTextRenderingHint(Handle, Result));
end;

procedure TdxGPCanvas.SetInterpolationMode(AValue: TdxGPInterpolationMode);
begin
  GdipCheck(GdipSetInterpolationMode(Handle, Integer(AValue)));
end;

procedure TdxGPCanvas.SetPixelOffsetMode(AValue: TdxGpPixelOffsetMode);
begin
  GdipCheck(GdipSetPixelOffsetMode(Handle, AValue));
end;

procedure TdxGPCanvas.SetSmoothingMode(AValue: TdxGPSmoothingMode);
begin
  GdipCheck(GdipSetSmoothingMode(Handle, Integer(AValue)));
end;

procedure TdxGPCanvas.SetTextRenderingHint(AValue: TdxGpTextRenderingHint);
begin
  GdipCheck(GdipSetTextRenderingHint(Handle, AValue));
end;

{ TdxGPCustomPaintCanvas }

constructor TdxGPCustomPaintCanvas.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor TdxGPCustomPaintCanvas.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TdxGPCustomPaintCanvas.BeginPaint(DC: HDC; const R: TRect);
begin
  FLock.Enter;
  SaveState;
  FDrawRect := R;
  FDrawDC := DC;
  if dxGpIsDoubleBufferedNeeded(DC) then
  begin
    CreateBuffer(DC, R);
    DC := FBuffer.Canvas.Handle;
    FIsLowColorsMode := True;
  end
  else
    FBuffer := nil;

  CreateHandle(DC);
end;

procedure TdxGPCustomPaintCanvas.BeginPaint(AHandle: GpGraphics);
begin
  FLock.Enter;
  SaveState;
  FDrawRect := Rect(0, 0, 0, 0);
  FDrawDC := 0;
  FHandle := AHandle;
  FBuffer := nil;
end;

procedure TdxGPCustomPaintCanvas.EndPaint;
begin
  if FDrawDC <> 0 then
    FreeHandle;
  if FBuffer <> nil then
    FreeBuffer;
  RestoreState;
  FLock.Leave;
end;

procedure TdxGPCustomPaintCanvas.CreateBuffer(DC: HDC; const R: TRect);
begin
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.SetSize(cxRectWidth(R) + 1, cxRectHeight(R) + 1);
  FBuffer.Canvas.Lock;

  BitBlt(FBuffer.Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, R.Left, R.Top, SRCCOPY);
  SetWindowOrgEx(FBuffer.Canvas.Handle, R.Left, R.Top, nil);
end;

procedure TdxGPCustomPaintCanvas.FreeBuffer;
begin
  OutputBuffer;
  FBuffer.Canvas.Unlock;
  FreeAndNil(FBuffer);
end;

procedure TdxGPCustomPaintCanvas.OutputBuffer;
var
  ACanvas: TCanvas;
  ASaveIndex: Integer;
begin
  ACanvas := TCanvas.Create;
  try
    ACanvas.Lock;
    try
      ASaveIndex := SaveDC(FDrawDC);
      ACanvas.Handle := FDrawDC;
      SetWindowOrgEx(FBuffer.Canvas.Handle, 0, 0, nil);
      ACanvas.Draw(FDrawRect.Left, FDrawRect.Top, FBuffer);
      ACanvas.Handle := 0;
      RestoreDC(FDrawDC, ASaveIndex);
    finally
      ACanvas.Unlock;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TdxGPCustomPaintCanvas.SaveState;
begin
  //do nothing
end;

procedure TdxGPCustomPaintCanvas.RestoreState;
begin
  //do nothing
end;

{ TdxGPPaintCanvas }

destructor TdxGPPaintCanvas.Destroy;
begin
  Finalize(FSavedStates);
  inherited Destroy;
end;

procedure TdxGPPaintCanvas.SaveState;
begin
  SetCapacity(FCounter + 1);
  FSavedStates[FCounter].Handle := Handle;
  FSavedStates[FCounter].Buffer := FBuffer;
  FSavedStates[FCounter].DrawRect := FDrawRect;
  FSavedStates[FCounter].DC := FDrawDC;
  Inc(FCounter);
end;

procedure TdxGPPaintCanvas.RestoreState;
begin
  Dec(FCounter);
  FHandle := FSavedStates[FCounter].Handle;
  FBuffer := FSavedStates[FCounter].Buffer;
  FDrawRect := FSavedStates[FCounter].DrawRect;
  FDrawDC := FSavedStates[FCounter].DC;
end;

procedure TdxGPPaintCanvas.SetCapacity(AValue: Integer);
begin
  if AValue > Length(FSavedStates) then
    SetLength(FSavedStates, Max(AValue, Length(FSavedStates) + 4));
end;

{ TdxGPStreamAdapter }

{$IFDEF DELPHI22}
function TdxGPStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
begin
  Result := S_OK;
  try
    if @StatStg <> nil then
    begin
      ZeroMemory(@StatStg, SizeOf(StatStg));
      Result := inherited Stat(StatStg, grfStatFlag);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;
{$ELSE}
function TdxGPStreamAdapter.Stat(out StatStg: TStatStg; StatFlag: Integer): HRESULT;
begin
  Result := S_OK;
  try
    if @StatStg <> nil then
    begin
      ZeroMemory(@StatStg, SizeOf(StatStg));
      Result := inherited Stat(StatStg, StatFlag);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;
{$ENDIF}

{ TdxGPImage }

constructor TdxGPImageHandle.Create(AHandle: GpImage);
begin
  inherited Create;
  SetHandle(AHandle);
end;

constructor TdxGPImageHandle.CreateFromBits(AWidth, AHeight: Integer;
  const ABitsRef: TRGBColors; AAlphaFormat: TAlphaFormat = afPremultiplied);
const
  PixelFormatMap: array[TAlphaFormat] of Integer = (PixelFormat32bppRGB, PixelFormat32bppARGB, PixelFormat32bppPARGB);
var
  AHandle: GpImage;
begin
  GdipCheck(GdipCreateBitmapFromScan0(AWidth, AHeight, AWidth * 4, PixelFormatMap[AAlphaFormat], @ABitsRef[0], AHandle));
  Create(AHandle);
end;

constructor TdxGPImageHandle.CreateSize(AWidth, AHeight: Integer);
var
  AHandle: GpImage;
begin
  GdipCheck(GdipCreateBitmapFromScan0(AWidth, AHeight, AWidth * 4, PixelFormat32bppPARGB, nil, AHandle));
  Create(AHandle);
end;

constructor TdxGPImageHandle.CreateSize(const ASize: TSize);
begin
  CreateSize(ASize.cx, ASize.cy);
end;

destructor TdxGPImageHandle.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

function TdxGPImageHandle.Clone: TdxGPImageHandle;
var
  AHandle: GpImage;
begin
  GdipCheck(GdipCloneImage(Handle, AHandle));
  Result := TdxGPImageHandle.Create(AHandle);
end;

function TdxGPImageHandle.CreateCanvas: TdxGpCanvas;
var
  AGraphics: GpGraphics;
begin
  GdipCheck(GdipGetImageGraphicsContext(Handle, AGraphics));
  Result := TdxGPCanvas.Create(AGraphics);
end;

procedure TdxGPImageHandle.Draw(DC: HDC; const ADest, ASource: TdxRectF; AAlpha: Byte; APalette: IdxColorPalette);
begin
  if Handle <> nil then
  begin
    dxGPPaintCanvas.BeginPaint(DC, cxRect(ADest, False));
    try
      Draw(dxGPPaintCanvas, ADest, ASource, dxGpGetAlphaBlendAttributes(AAlpha));
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxGPImageHandle.Draw(DC: HDC; const ADest, ASource: TRect; AAlpha: Byte; APalette: IdxColorPalette);
begin
  if Handle <> nil then
  begin
    dxGPPaintCanvas.BeginPaint(DC, ADest);
    try
      Draw(dxGPPaintCanvas, ADest, ASource, dxGpGetAlphaBlendAttributes(AAlpha));
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxGPImageHandle.Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TdxRectF; AAttributes: TdxGPImageAttributes);
begin
  dxGpDrawImage(ACanvas.Handle, ADest, ASource, Handle, MaxByte, dxGpGetImageAttributesHandle(AAttributes));
end;

procedure TdxGPImageHandle.Draw(ACanvas: TdxGPCanvas; const ADest, ASource: TRect; AAttributes: TdxGPImageAttributes);
begin
  dxGpDrawImage(ACanvas.Handle, ADest, ASource, Handle, MaxByte, dxGpGetImageAttributesHandle(AAttributes));
end;

procedure TdxGPImageHandle.Flip(AHorizontally, AVertically: Boolean);
var
  ABitmap: GpBitmap;
  ABitmapGraphics: GpGraphics;
  ABitmapSize: TSize;
begin
  if (AHorizontally or AVertically) and not Empty then
  begin
    ABitmapSize := Size;
    ABitmap := dxGpCreateBitmap(ABitmapSize);
    try
      GdipCheck(GdipGetImageGraphicsContext(ABitmap, ABitmapGraphics));
      try
        dxGPPaintCanvas.BeginPaint(ABitmapGraphics);
        dxGPPaintCanvas.FlipWorldTransform(AHorizontally, AVertically, ABitmapSize.cx / 2, ABitmapSize.cy / 2);
        Draw(dxGPPaintCanvas, cxRect(ABitmapSize), cxRect(ABitmapSize), nil);
        dxGPPaintCanvas.EndPaint;
      finally
        GdipCheck(GdipDeleteGraphics(ABitmapGraphics));
      end;
      Handle := ABitmap;
    except
      GdipCheck(GdipDisposeImage(ABitmap));
      raise;
    end;
  end;
end;

function TdxGPImageHandle.GetAsBitmap: TBitmap;
var
  ABitmapHandle: HBITMAP;
begin
  if IsMetafile then
    Exit(inherited GetAsBitmap);

  Result := TBitmap.Create;
  Result.PixelFormat := pf32Bit;
  GdipCheck(GdipCreateHBITMAPFromBitmap(Handle, ABitmapHandle, 0));
  Result.Handle := ABitmapHandle;
end;

function TdxGPImageHandle.IsMetafile: Boolean;
begin
  Result := ImageDataFormat in [dxImageEmf, dxImageWmf];
end;

procedure TdxGPImageHandle.Resize(const ASize: TSize;
  AInterpolationMode: TdxGPInterpolationMode = imDefault;
  APixelOffsetMode: TdxGpPixelOffsetMode = PixelOffsetModeDefault);
var
  AGraphics: GpGraphics;
  AScaledImageHandle: GpBitmap;
begin
  AScaledImageHandle := dxGpCreateBitmap(ASize);
  try
    if Handle <> nil then
    begin
      GdipCheck(GdipGetImageGraphicsContext(AScaledImageHandle, AGraphics));
      if APixelOffsetMode <> PixelOffsetModeDefault then
        GdipCheck(GdipSetPixelOffsetMode(AGraphics, APixelOffsetMode));
      if AInterpolationMode <> imDefault then
        GdipCheck(GdipSetInterpolationMode(AGraphics, Integer(AInterpolationMode)));
      GdipCheck(GdipDrawImageRectI(AGraphics, Handle, 0, 0, ASize.cx, ASize.cy));
      GdipCheck(GdipDeleteGraphics(AGraphics));
    end;
  except
    GdipCheck(GdipDisposeImage(AScaledImageHandle));
    raise;
  end;
  Handle := AScaledImageHandle;
end;

procedure TdxGPImageHandle.FreeHandle;
begin
  if FHandle <> nil then
  begin
    GdipDisposeImage(FHandle);
    FHandle := nil;
  end;
end;

function TdxGPImageHandle.GetSize: TSize;
begin
  if Handle <> nil then
  begin
    GdipCheck(GdipGetImageWidth(Handle, Result.cx));
    GdipCheck(GdipGetImageHeight(Handle, Result.cy));
  end
  else
    Result := cxNullSize;
end;

procedure TdxGPImageHandle.SetSize(const AValue: TSize);
begin
  Resize(AValue, imDefault);
end;

function TdxGPImageHandle.GetImageFormat: TdxImageDataFormat;
begin
  Result := FImageFormat;
end;

function TdxGPImageHandle.GetActiveFrame: Cardinal;
begin
  Result := FActiveFrame;
end;

function TdxGPImageHandle.GetAnimationFrameCount: Cardinal;
begin
  Result := 0;
  if not Empty then
  begin
    GdipCheck(GdipImageGetFrameDimensionsCount(Handle, Result));
    if Result = 1 then
    begin
      GdipCheck(GdipImageGetFrameDimensionsList(Handle, @FDimensionID, 1));
      GdipCheck(GdipImageGetFrameCount(Handle, @FDimensionID, Result));
    end
    else
      Result := 0;
  end;
end;

function TdxGPImageHandle.GetAnimationFrameDelay: Integer;
const
  FrameDelayProperty = $5100;
var
  APropData: TdxGPImagePropertyData;
begin
  if GetPropertyValue(FrameDelayProperty, APropData) then
    Result := PInteger(@APropData[FActiveFrame * 4])^ * 10
  else
    Result := DefaultAnimationFrameDelay;
end;

function TdxGPImageHandle.GetAnimationLoopCount: Integer;
const
  LoopCountProperty = $5101;
var
  AData: TdxGPImagePropertyData;
begin
  if GetPropertyValue(LoopCountProperty, AData) then
    Result := PWord(@AData[0])^
  else
    Result := 0;
end;

procedure TdxGPImageHandle.SetActiveFrame(AValue: Cardinal);
begin
  FActiveFrame := AValue;
  if GdipImageSelectActiveFrame(Handle, @FDimensionID, AValue) = Ok then
    Exit
  else
    if AValue <> 0 then
      SetActiveFrame(0);
end;

function TdxGPImageHandle.GetPropertyValue(APropID: Cardinal; var AData: TdxGPImagePropertyData): Boolean;
var
  APropItem: PGpPropertyItem;
  ASize, ACount: Cardinal;
begin
  ASize := 0;
  SetLength(AData, 0);
  if (GdipGetPropertyItemSize(Handle, APropID, ASize) = Ok) and (ASize > SizeOf(TGpPropertyItem)) then
  begin
    APropItem := AllocMem(ASize);
    try
      if GdipGetPropertyItem(Handle, APropID, ASize, APropItem) = Ok then
      begin
        ACount := ASize - SizeOf(TGpPropertyItem);
        SetLength(AData, ACount);
        Move(APropItem^.value^, AData[0], ACount);
      end;
    finally
      FreeMem(APropItem);
    end;
  end;
  Result := Length(AData) > 0;
end;

procedure TdxGPImageHandle.SetHandle(AValue: GpImage);
begin
  if FHandle <> AValue then
  begin
    FreeHandle;
    FHandle := AValue;
    FImageFormat := dxGetImageDataFormat(Handle);
  end;
end;

{ TdxGPImageCodec }

class function TdxGPImageCodec.CanSaveImage(AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  Result := True;
end;

class function TdxGPImageCodec.Load(AStream: TStream; out AHandle: TdxSmartImageCustomHandle): Boolean;
var
  AImage: GpImage;
  AStreamAdapter: IStream;
begin
  AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
  try
    Result := GdipLoadImageFromStream(AStreamAdapter, AImage) = Ok;
    if Result then
      AHandle := TdxGPImageHandle.Create(AImage);
  finally
    AStreamAdapter := nil;
  end;
end;

class function TdxGPImageCodec.Save(AStream: TStream; AHandle: TdxSmartImageCustomHandle): Boolean;
var
  ATempHandle: TdxGPImageHandle;
begin
  Result := IsCodecIDValid(GetCodecID);
  if Result then
  begin
    if AHandle is TdxGPImageHandle then
      SaveCore(AStream, TdxGPImageHandle(AHandle))
    else
    begin
      ATempHandle := TdxGPImageHandle.CreateFromBits(AHandle.Width, AHandle.Height, AHandle.GetAsBitmapBits);
      try
        SaveCore(AStream, ATempHandle)
      finally
        ATempHandle.Free;
      end;
    end;
  end;
end;

class function TdxGPImageCodec.CheckHeader(AStream: TStream; const AHeader: AnsiString): Boolean;
var
  ABuffer: array of AnsiChar;
  APosition: Int64;
begin
  APosition := AStream.Position;
  try
    SetLength(ABuffer, Length(AHeader));
    Result := (AStream.Read(ABuffer[0], Length(ABuffer)) = Length(ABuffer)) and CompareMem(@ABuffer[0], @AHeader[1], Length(AHeader));
  finally
    AStream.Position := APosition;
  end;
end;

class function TdxGPImageCodec.ReadInteger(AStream: TStream): Integer;
begin
  AStream.Read(Result, SizeOf(Result));
end;

class function TdxGPImageCodec.ReadWord(AStream: TStream): Word;
begin
  AStream.Read(Result, SizeOf(Result));
end;

class function TdxGPImageCodec.GetCodecID: TGUID;
begin
  Result := dxGetImageEncoder(ID);
end;

class function TdxGPImageCodec.SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer = nil): Boolean;
var
  ACodec: TGUID;
  AStreamAdapter: IStream;
begin
  ACodec := GetCodecID;
  AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
  Result := GdipSaveImageToStream(AHandle.Handle, AStreamAdapter, @ACodec, AParameters) = Ok;
  AStreamAdapter := nil;
end;

{ TdxGPImageCodecBMP }

class function TdxGPImageCodecBMP.Ext: string;
begin
  Result := '.bmp';
end;

class function TdxGPImageCodecBMP.GetSize(AStream: TStream; out ASize: TSize): Boolean;
begin
  Result := CanLoadStream(AStream);
  if Result then
  begin
    AStream.Seek(SizeOf(TBitmapFileHeader), soCurrent);
    if ReadInteger(AStream) = SizeOf(TBitmapCoreHeader) then
    begin
      ASize.cx := ReadWord(AStream);
      ASize.cy := ReadWord(AStream);
    end
    else
    begin
      ASize.cx := ReadInteger(AStream);
      ASize.cy := ReadInteger(AStream);
    end;
  end;
end;

class function TdxGPImageCodecBMP.ID: TdxImageDataFormat;
begin
  Result := dxImageBitmap;
end;

class function TdxGPImageCodecBMP.CanLoadFromBits: Boolean;
begin
  Result := True;
end;

class function TdxGPImageCodecBMP.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := CheckHeader(AStream, #$42#$4D);
end;

class function TdxGPImageCodecBMP.Load(const ABits: TRGBColors; AAlphaFormat: TAlphaFormat;
  AWidth, AHeight: Integer; out AHandle: TdxSmartImageCustomHandle): Boolean;
begin
  AHandle := TdxGPImageHandle.CreateFromBits(AWidth, AHeight, ABits, AAlphaFormat);
  Result := True;
end;

{ TdxGPImageCodecGIF }

class function TdxGPImageCodecGIF.Ext: string;
begin
  Result := '.gif';
end;

class function TdxGPImageCodecGIF.GetSize(AStream: TStream; out ASize: TSize): Boolean;
begin
  Result := CanLoadStream(AStream);
  if Result then
  begin
    AStream.Seek(6, soCurrent);
    ASize.cx := ReadWord(AStream);
    ASize.cy := ReadWord(AStream);
  end;
end;

class function TdxGPImageCodecGIF.ID: TdxImageDataFormat;
begin
  Result := dxImageGif;
end;

class function TdxGPImageCodecGIF.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := CheckHeader(AStream, 'GIF89a') or CheckHeader(AStream, 'GIF87a');
end;

{ TdxGPImageCodecJPG }

class function TdxGPImageCodecJPG.Description: string;
begin
  Result := '';
end;

class function TdxGPImageCodecJPG.Ext: string;
begin
  Result := '.jpg';
end;

{ TdxGPImageCodecJPEG }

class function TdxGPImageCodecJPEG.Ext: string;
begin
  Result := '.jpeg';
end;

class function TdxGPImageCodecJPEG.GetSize(AStream: TStream; out ASize: TSize): Boolean;
var
  ARecordID: Word;
begin
  Result := False;
  if CanLoadStream(AStream) then
  begin
    AStream.Seek(2, soCurrent);
    while AStream.Read(ARecordID, SizeOf(ARecordID)) = SizeOf(ARecordID) do
    begin
      ARecordID := dxSwap16(ARecordID);
      if (ARecordID and $FF00 <> $FF00) or (ARecordID and $FF = $D9) then
        Break;
      case ARecordID and not $FF00 of
        $D0..$D9:
          Continue;
        $DD:
          AStream.Seek(4, soCurrent);
        $C0, $C3:
          begin
            AStream.Seek(3, soCurrent);
            ASize.cy := dxSwap16(ReadWord(AStream));
            ASize.cx := dxSwap16(ReadWord(AStream));
            Exit(True);
          end;
      else
        AStream.Seek(dxSwap16(ReadWord(AStream)) - 2, soCurrent);
      end;
    end;
  end;
end;

class function TdxGPImageCodecJPEG.ID: TdxImageDataFormat;
begin
  Result := dxImageJpeg;
end;

class constructor TdxGPImageCodecJPEG.Initialize;
begin
  FQuality := DefaultQuality;
end;

class function TdxGPImageCodecJPEG.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := CheckHeader(AStream, #$FF#$D8#$FF);
end;

class function TdxGPImageCodecJPEG.SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer = nil): Boolean;
var
  AEncoderParameters: TEncoderParameters;
begin
  if (Quality <> DefaultQuality) and (AParameters = nil) then
  begin
    AEncoderParameters.Count := 1;
    AEncoderParameters.Parameter[0].Guid := EncoderQuality;
    AEncoderParameters.Parameter[0].Type_ := EncoderParameterValueTypeLong;
    AEncoderParameters.Parameter[0].NumberOfValues := 1;
    AEncoderParameters.Parameter[0].Value := @FQuality;
    AParameters := @AEncoderParameters;
  end;
  Result := inherited SaveCore(AStream, AHandle, AParameters);
end;

class procedure TdxGPImageCodecJPEG.SetQuality(const Value: Cardinal);
begin
  if Value <> DefaultQuality then
    FQuality := Max(0, Min(Value, 100))
  else
    FQuality := DefaultQuality;
end;

{ TdxGPImageCodecPNG }

class function TdxGPImageCodecPNG.Ext: string;
begin
  Result := '.png';
end;

class function TdxGPImageCodecPNG.GetSize(AStream: TStream; out ASize: TSize): Boolean;
begin
  Result := CanLoadStream(AStream);
  if Result then
  begin
    AStream.Seek(12, soCurrent);
    if CheckHeader(AStream, 'IHDR') then
      AStream.Seek(4, soCurrent)
    else
      AStream.Seek(-4, soCurrent);

    ASize.cx := dxSwap32(ReadInteger(AStream));
    ASize.cy := dxSwap32(ReadInteger(AStream));
  end;
end;

class function TdxGPImageCodecPNG.ID: TdxImageDataFormat;
begin
  Result := dxImagePng;
end;

class function TdxGPImageCodecPNG.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := CheckHeader(AStream, #$89'PNG'#$0D#$0A#$1A#$0A);
end;

{ TdxGPImageCodecTIFF }

class function TdxGPImageCodecTIFF.Ext: string;
begin
  Result := '.tiff';
end;

class function TdxGPImageCodecTIFF.GetSize(AStream: TStream; out ASize: TSize): Boolean;
const
  HeightTag = $101;
  WidthTag  = $100;
type
  PTiffTagRecord = ^TTiffTagRecord;
  TTiffTagRecord = packed record
    Tag: Word;
    FieldType: Word;
    Count: LongWord;
    case Byte of
      0: (ByteValue:  Byte);
      1: (WordValue:  Word);
      2: (LongValue: LongWord);
      3: (LongValue2: LongWord);
  end;

  function GetTokenSize(AType: Word): Word;
  const
    Length: array[0..12] of Word = (1, 1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8);
  begin
    if AType <= 12 then
      Result := Length[AType]
    else
      Result := 1;
  end;

  function GetInt(AIsBigEndian: Boolean): Integer;
  begin
    Result := ReadInteger(AStream);
    if AIsBigEndian then
      Result := dxSwap32(Result);
  end;

  function GetWord(AIsBigEndian: Boolean): Word;
  begin
    Result := ReadWord(AStream);
    if AIsBigEndian then
      Result := dxSwap16(Result);
  end;

var
  AIsBigEndian: Boolean;
  AOffset: Integer;
  ATagInfo: TTiffTagRecord;
  I, ACount: Word;
begin
  Result := False;
  if CanLoadStream(AStream) then
  begin
    ASize := cxNullSize;
    AIsBigEndian := CheckHeader(AStream, TIFFHeader2);
    AStream.Seek(4, soCurrent);
    AOffset := GetInt(AIsBigEndian);
    AStream.Seek(AOffset - 8, soCurrent);
    ACount := GetWord(AIsBigEndian);
    for I := 0 to ACount - 1 do
    begin
      ATagInfo.Tag := GetWord(AIsBigEndian);
      ATagInfo.FieldType := GetWord(AIsBigEndian);
      ATagInfo.Count := GetInt(AIsBigEndian);
      case ATagInfo.Count * GetTokenSize(ATagInfo.FieldType) of
        2: ATagInfo.WordValue := GetWord(AIsBigEndian);
        4: ATagInfo.LongValue := GetInt(AIsBigEndian);
        8:
          begin
            ATagInfo.LongValue := GetInt(AIsBigEndian);
            ATagInfo.LongValue2 := GetInt(AIsBigEndian);
          end;
      end;
      case ATagInfo.Tag of
        HeightTag:
          ASize.cy := ATagInfo.LongValue;
        WidthTag:
          ASize.cx := ATagInfo.LongValue;
      end;
      if (ASize.cx > 0) and (ASize.cy > 0) then
        Exit(True);
    end;
  end;
end;

class function TdxGPImageCodecTIFF.ID: TdxImageDataFormat;
begin
  Result := dxImageTiff;
end;

class function TdxGPImageCodecTIFF.CanLoadStream(AStream: TStream): Boolean;
begin
  Result := CheckHeader(AStream, TIFFHeader1) or CheckHeader(AStream, TIFFHeader2);
end;

{ TdxGPImageCodecTIF }

class function TdxGPImageCodecTIF.Description: string;
begin
  Result := '';
end;

class function TdxGPImageCodecTIF.Ext: string;
begin
  Result := '.tif';
end;

{ TdxGPImageCodecEMF }

class function TdxGPImageCodecEMF.Ext: string;
begin
  Result := '.emf';
end;

class function TdxGPImageCodecEMF.ID: TdxImageDataFormat;
begin
  Result := dxImageEmf;
end;

class function TdxGPImageCodecEMF.SaveCore(AStream: TStream; AHandle: TdxGPImageHandle; AParameters: Pointer): Boolean;
var
  ACanvas: TCanvas;
  AMetafile: TMetafile;
  AMetafileStream: TStream;
begin
  AMetafile := TMetafile.Create;
  try
    if AHandle.IsMetafile then
    begin
      AMetafileStream := TdxGPMemoryStream.Create;
      try
        inherited SaveCore(AMetafileStream, AHandle);
        AMetafile.LoadFromStream(AMetafileStream);
        AMetafile.Enhanced := ID = dxImageEmf;
      finally
        AMetafileStream.Free;
      end;
    end
    else
    begin
      AMetafile.SetSize(AHandle.Width, AHandle.Height);
      ACanvas := TMetafileCanvas.Create(AMetafile, 0);
      try
        AHandle.Draw(ACanvas.Handle, AHandle.ClientRect, AHandle.ClientRect);
        AMetafile.Enhanced := ID = dxImageEmf;
      finally
        ACanvas.Free;
      end;
    end;
    AMetafile.SaveToStream(AStream);
    Result := True;
  finally
    AMetafile.Free;
  end;
end;

{ TdxGPImageCodecWMF }

class function TdxGPImageCodecWMF.Ext: string;
begin
  Result := '.wmf';
end;

class function TdxGPImageCodecWMF.ID: TdxImageDataFormat;
begin
  Result := dxImageWmf;
end;

{ TdxGPAlphaBlendAttributes }

constructor TdxGPAlphaBlendAttributes.Create;
begin
  inherited Create;
  FAlpha := MaxByte;
  FColorMatrix := dxGpDefaultColorMatrix;
end;

procedure TdxGPAlphaBlendAttributes.SetAlpha(AValue: Byte);
begin
  if AValue <> FAlpha then
  begin
    FAlpha := AValue;
    FColorMatrix[3, 3] := Alpha / MaxByte;
    SetColorMatrix(@FColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
  end;
end;

{ TdxGPImageAttributes }

constructor TdxGPImageAttributes.Create(const AHandle: GpImageAttributes);
begin
  inherited Create;
  FHandle := AHandle;
end;

constructor TdxGPImageAttributes.Create;
begin
  inherited Create;
  GdipCheck(GdipCreateImageAttributes(FHandle));
end;

destructor TdxGPImageAttributes.Destroy;
begin
  if FHandle <> nil then
  begin
    GdipCheck(GdipDisposeImageAttributes(FHandle));
    FHandle := nil;
  end;
  inherited Destroy;
end;

procedure TdxGPImageAttributes.ClearBrushRemapTable;
begin
  ClearRemapTable(ColorAdjustTypeBrush);
end;

procedure TdxGPImageAttributes.ClearColorKey(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesColorKeys(FHandle, AType, False, 0, 0));
end;

procedure TdxGPImageAttributes.ClearColorMatrix(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesColorMatrix(FHandle, AType, False, nil, nil, ColorMatrixFlagsDefault));
end;

procedure TdxGPImageAttributes.ClearGamma(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesGamma(FHandle, AType, False, 0));
end;

procedure TdxGPImageAttributes.ClearNoOp(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesNoOp(FHandle, AType, False));
end;

procedure TdxGPImageAttributes.ClearOutputChannel(
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesOutputChannel(FHandle, AType, False, ColorChannelFlagsLast));
end;

procedure TdxGPImageAttributes.ClearOutputChannelColorProfile(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesOutputChannelColorProfile(FHandle, AType, False, nil));
end;

procedure TdxGPImageAttributes.ClearRemapTable(
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesRemapTable(FHandle, AType, False, 0, nil));
end;

procedure TdxGPImageAttributes.ClearThreshold(
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesThreshold(FHandle, AType, False, 0));
end;

function TdxGPImageAttributes.Clone: TdxGPImageAttributes;
var
  AClone: GpImageAttributes;
begin
  GdipCheck(GdipCloneImageAttributes(FHandle, AClone));
  Result := TdxGPImageAttributes.Create(AClone);
end;

procedure TdxGPImageAttributes.GetAdjustedPalette(APalette: TGpColorPalette;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipGetImageAttributesAdjustedPalette(FHandle, @APalette, AType));
end;

procedure TdxGPImageAttributes.SetBrushRemapTable(const AMap: array of TGpColorMap);
begin
  SetRemapTable(AMap, ColorAdjustTypeBrush);
end;

procedure TdxGPImageAttributes.SetColorKey(const AColorLow,
  AColorHigh: TdxAlphaColor; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesColorKeys(FHandle, AType, True, AColorLow, AColorHigh));
end;

procedure TdxGPImageAttributes.SetColorMatrices(const ANewColorMatrix, AGrayMatrix: TdxGpColorMatrix;
  const AMode: TdxGPColorMatrixFlags = ColorMatrixFlagsDefault;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesColorMatrix(FHandle, AType, True, @ANewColorMatrix, @AGrayMatrix, AMode));
end;

procedure TdxGPImageAttributes.SetColorMatrix(ANewColorMatrix: PdxGpColorMatrix;
  const AMode: TdxGPColorMatrixFlags = ColorMatrixFlagsDefault;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesColorMatrix(FHandle, AType, True, ANewColorMatrix, nil, AMode));
end;

procedure TdxGPImageAttributes.SetGamma(AGamma: Single;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesGamma(FHandle, AType, True, AGamma));
end;

procedure TdxGPImageAttributes.SetNoOp(const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesNoOp(FHandle, AType, True));
end;

procedure TdxGPImageAttributes.SetOutputChannel(
  const AFlags: TdxGpColorChannelFlags; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesOutputChannel(FHandle, AType, True, AFlags));
end;

procedure TdxGPImageAttributes.SetOutputChannelColorProfile(
  const AColorProfileFilename: string; const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesOutputChannelColorProfile(FHandle, AType, True, PChar(AColorProfileFilename)));
end;

procedure TdxGPImageAttributes.SetRemapTable(const AMap: array of TGpColorMap;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesRemapTable(FHandle, AType, True, Length(AMap), @AMap[0]));
end;

procedure TdxGPImageAttributes.SetThreshold(AThreshold: Single;
  const AType: TdxGpColorAdjustType = ColorAdjustTypeDefault);
begin
  GdipCheck(GdipSetImageAttributesThreshold(FHandle, AType, True, AThreshold));
end;

procedure TdxGPImageAttributes.SetWrapMode(const AMode: TdxGpWrapMode);
begin
  SetWrapMode(AMode, TdxAlphaColors.Black);
end;

procedure TdxGPImageAttributes.SetWrapMode(const AMode: TdxGpWrapMode;
  AColor: TdxAlphaColor; AClamp: Boolean = False);
begin
  GdipCheck(GdipSetImageAttributesWrapMode(FHandle, AMode, AColor, AClamp));
end;

{ TdxGPImage }

function TdxGPImage.Clone: TdxGPImage;
begin
  Result := TdxGPImage(inherited Clone);
end;

function TdxGPImage.CreateCanvas: TdxGPCanvas;
var
  AGraphics: GpGraphics;
begin
  if not ImageData.Empty then
    ConvertToBitmap;
  GdipCheck(GdipGetImageGraphicsContext(Handle, AGraphics));
  Result := TdxGPCanvas.Create(AGraphics);
  Changed(Self);
end;

procedure TdxGPImage.Flip(AHorizontally, AVertically: Boolean);

  function TryFlip: Boolean;
  var
    AIntf: IdxGpFlipContent;
  begin
    Result := Supports(HandleAsObject, IdxGpFlipContent, AIntf);
    if Result then
      AIntf.Flip(AHorizontally, AVertically);
  end;

begin
  if (AHorizontally or AVertically) and not Empty then
  begin
    if not TryFlip then
    begin
      ConvertToBitmap;
      TryFlip;
    end;
    Changed;
  end;
end;

function TdxGPImage.MakeComposition(AOverlayImage: TdxGPImage; AOverlayAlpha: Byte): TdxGPImage;
begin
  Result := MakeComposition(AOverlayImage, AOverlayAlpha, 255);
end;

function TdxGPImage.MakeComposition(AOverlayImage: TdxGPImage; AOverlayAlpha, ASourceAlpha: Byte): TdxGPImage;
var
  ACanvas: TdxGPCanvas;
begin
  Result := TdxGPImage.CreateSize(Width, Height);
  if not Result.Empty then
  begin
    ACanvas := Result.CreateCanvas;
    try
      ACanvas.Draw(Self, Result.ClientRect, ASourceAlpha);
      ACanvas.Draw(AOverlayImage, Result.ClientRect, AOverlayImage.ClientRect, AOverlayAlpha);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TdxGPImage.StretchDraw(ACanvas: TdxGPCanvas; const ADest, ASource: TdxRectF;
  AAttributes: TdxGPImageAttributes; AColorPalette: IdxColorPalette = nil);
var
  AIntf: IdxGPGraphicDraw;
begin
  if Supports(HandleAsObject, IdxGPGraphicDraw, AIntf) then
    AIntf.Draw(ACanvas, ADest, ASource, AAttributes)
  else
    StretchDraw(ACanvas, cxRect(ADest), cxRect(ASource), AAttributes, AColorPalette);
end;

procedure TdxGPImage.StretchDraw(ACanvas: TdxGPCanvas; const ADest, ASource: TRect;
  AAttributes: TdxGPImageAttributes; AColorPalette: IdxColorPalette = nil);
var
  AAlpha: Byte;
  ADC: HDC;
  AIntf: IdxGPGraphicDraw;
  AMatrix: GpMatrix;
  APrevMode: Integer;
  APrevXForm, ATransform: TXForm;
begin
  if Supports(HandleAsObject, IdxGPGraphicDraw, AIntf) then
    AIntf.Draw(ACanvas, ADest, ASource, AAttributes)
  else
  begin
    if AAttributes is TdxGPAlphaBlendAttributes then
      AAlpha := TdxGPAlphaBlendAttributes(AAttributes).Alpha
    else
      AAlpha := MaxByte;

    GdipCheck(GdipCreateMatrix(AMatrix));
    try
      GdipCheck(GdipGetWorldTransform(ACanvas.Handle, AMatrix));
      GdipCheck(GdipGetMatrixElements(AMatrix, @ATransform));
      ADC := ACanvas.GetHDC;
      try
        APrevMode := SetGraphicsMode(ADC, GM_ADVANCED);
        GetWorldTransform(ADC, APrevXForm);
        try
          SetWorldTransform(ADC, ATransform);
          StretchDraw(ADC, ADest, ASource, AAlpha, AColorPalette);
        finally
          SetWorldTransform(ADC, APrevXForm);
          SetGraphicsMode(ADC, APrevMode);
        end;
      finally
        ACanvas.ReleaseHDC(ADC);
      end;
    finally
      GdipCheck(GdipDeleteMatrix(AMatrix));
    end;
  end;
end;

function TdxGPImage.CreateCache(const ASize: TSize): TdxSmartImageCustomHandle;
begin
  if HandleAsObject is TdxGPImageHandle then
  begin
    Result := TdxGPImageHandle(HandleAsObject).Clone;
    Result.Size := ASize;
  end
  else
    Result := inherited;
end;

function TdxGPImage.GetHandle: GpImage;
begin
  if not (HandleAsObject is TdxGPImageHandle) then
    ConvertToBitmap;
  Result := TdxGPImageHandle(HandleAsObject).Handle;
end;

function TdxGPImage.GetHandleAsObject: TdxSmartImageCustomHandle;
begin
  Result := inherited Handle;
end;

procedure TdxGPImage.SetHandle(const Value: GpImage);
begin
  HandleAsObject := TdxGPImageHandle.Create(Value);
end;

procedure TdxGPImage.SetHandleAsObject(const Value: TdxSmartImageCustomHandle);
begin
  inherited Handle := Value;
end;

{ TdxSmartGlyph }

constructor TdxSmartGlyph.Create;
begin
  inherited Create;
  FTransparent := True;
end;

procedure TdxSmartGlyph.AfterConstruction;
begin
  inherited AfterConstruction;
  FSourceDPI := dxDefaultDPI;
end;

procedure TdxSmartGlyph.AssignFromSmartImage(AImage: TdxCustomSmartImage);
begin
  inherited AssignFromSmartImage(AImage);
  if AImage is TdxSmartGlyph then
  begin
    SourceDPI := TdxSmartGlyph(AImage).SourceDPI;
    SourceHeight := TdxSmartGlyph(AImage).SourceHeight;
    SourceWidth := TdxSmartGlyph(AImage).SourceWidth;
  end;
end;

procedure TdxSmartGlyph.CreateHandleFromBitmap(ABitmap: TBitmap);
begin
  ImageData.Transparent := FTransparent;
  inherited CreateHandleFromBitmap(ABitmap);
end;

function TdxSmartGlyph.IsBitmapStream(AStream: TStream): Boolean;
var
  AHeader: TBitmapFileHeader;
  ASize: DWORD;
begin
  Result := inherited IsBitmapStream(AStream);
  if not Result and (AStream.Read(ASize, SizeOf(ASize)) = SizeOf(ASize)) and
    (AStream.Read(AHeader, SizeOf(AHeader)) = SizeOf(AHeader)) then
  begin
    Result := (ASize = AHeader.bfSize) and (AHeader.bfType = $4D42);
    AStream.Seek(-SizeOf(AHeader), soCurrent);
    if not Result then
      AStream.Seek(-SizeOf(ASize), soCurrent);
  end;
end;

function TdxSmartGlyph.IsSourceDPIStored: Boolean;
begin
  Result := not Empty;
end;

function TdxSmartGlyph.IsSourceHeightStored: Boolean;
begin
  Result := FSourceHeight <> 0;
end;

function TdxSmartGlyph.IsSourceWidthStored: Boolean;
begin
  Result := FSourceWidth <> 0;
end;

function TdxSmartGlyph.GetSourceDPI: Integer;
begin
  Result := FSourceDPI;
end;

function TdxSmartGlyph.GetSourceHeight: Integer;
begin
  if IsSourceHeightStored then
    Result := FSourceHeight
  else
    Result := Height;
end;

function TdxSmartGlyph.GetSourceWidth: Integer;
begin
  if IsSourceWidthStored then
    Result := FSourceWidth
  else
    Result := Width;
end;

procedure TdxSmartGlyph.SetSourceDPI(AValue: Integer);
begin
  AValue := dxCheckDPIValue(AValue);
  if FSourceDPI <> AValue then
  begin
    FSourceDPI := AValue;
    Changed(Self);
  end;
end;

procedure TdxSmartGlyph.SetSourceHeight(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue = Height then
    AValue := 0;
  if FSourceHeight <> AValue then
  begin
    FSourceHeight := AValue;
    Changed(Self);
  end;
end;

procedure TdxSmartGlyph.SetSourceWidth(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue = Width then
    AValue := 0;
  if FSourceWidth <> AValue then
  begin
    FSourceWidth := AValue;
    Changed(Self);
  end;
end;

function TdxSmartGlyph.GetSourceSize: TSize;
begin
  if IsSourceHeightStored or IsSourceWidthStored then
    Result := cxSize(SourceWidth, SourceHeight)
  else
    Result := Size;
end;

{ TdxPNGImage }

procedure TdxPNGImage.SaveToStream(AStream: TStream);
begin
  SaveToStreamByCodec(AStream, TdxGPImageCodecPNG);
end;

{ TdxJPEGImage }

constructor TdxJPEGImage.Create;
begin
  inherited Create;
  FQuality := 75;
end;

procedure TdxJPEGImage.SaveToStream(AStream: TStream);
begin
  TdxGPImageCodecJPEG.Quality := Quality;
  try
    SaveToStreamByCodec(AStream, TdxGPImageCodecJPEG);
  finally
    TdxGPImageCodecJPEG.Quality := TdxGPImageCodecJPEG.DefaultQuality;
  end;
end;

procedure TdxJPEGImage.SetQuality(AValue: Cardinal);
begin
  FQuality := Min(AValue, 100);
end;

{ TdxGIFImage }

procedure TdxGIFImage.SaveToStream(AStream: TStream);
begin
  SaveToStreamByCodec(AStream, TdxGPImageCodecGIF);
end;

{ TdxTIFFImage }

procedure TdxTIFFImage.SaveToStream(AStream: TStream);
begin
  SaveToStreamByCodec(AStream, TdxGPImageCodecTiff);
end;

{ TdxBMPImage }

procedure TdxBMPImage.SaveToStream(AStream: TStream);
begin
  SaveToStreamByCodec(AStream, TdxGPImageCodecBMP);
end;

{ TdxGPHandle }

constructor TdxGPHandle.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TdxGPHandle.GetNativeHandle: Pointer;
begin
  Result := FHandle;
end;

{ TdxGPPenHandle }

destructor TdxGPPenHandle.Destroy;
begin
  GdipCheck(GdipDeletePen(FHandle));
  inherited Destroy;
end;

{ TdxGPBrushHandle }

destructor TdxGPBrushHandle.Destroy;
begin
  GdipCheck(GdipDeleteBrush(FHandle));
  inherited Destroy;
end;

{ TdxGPSolidPenCacheID }

constructor TdxGPSolidPenCacheID.Create(AColor: TdxAlphaColor; AWidth: Single; AStyle: DashStyle);
begin
  Color := AColor;
  Width := AWidth;
  Style := AStyle;
end;

{ TdxGPSolidPenCacheManager }

function TdxGPSolidPenCacheManager.Compare(const Key1, Key2: TdxGPSolidPenCacheID): Integer;
begin
  Result := Key1.Color - Key2.Color;
  if Result = 0 then
    Result := Ord(Key1.Style) - Ord(Key2.Style);
  if Result = 0 then
    Result := CompareValue(Key1.Width, Key2.Width);
end;

function TdxGPSolidPenCacheManager.CreateValue(const ID: TdxGPSolidPenCacheID): IdxGPHandle;
var
  AHandle: GpPen;
begin
  GdipCheck(GdipCreatePen1(ID.Color, ID.Width, guPixel, AHandle));
  GdipCheck(GdipSetPenDashStyle(AHandle, ID.Style));
  Result := TdxGPPenHandle.Create(AHandle);
end;

{ TdxGPSolidBrushCacheManager }

function TdxGPSolidBrushCacheManager.Compare(const Key1, Key2: TdxAlphaColor): Integer;
begin
  Result := Key1 - Key2;
end;

function TdxGPSolidBrushCacheManager.CreateValue(const ID: TdxAlphaColor): IdxGPHandle;
var
  ABrush: GpBrush;
begin
  GdipCheck(GdipCreateSolidFill(ID, ABrush));
  Result := TdxGPBrushHandle.Create(ABrush);
end;

{ TdxGPResourceManager }

class function TdxGPResourceManager.GetPen(AColor: TColor; AColorAlpha: Byte;
  AWidth: Single; AStyle: TPenStyle; out AHandle: IdxGPHandle): Boolean;
begin
  Result := GetPen(dxColorToAlphaColor(AColor, AColorAlpha), AWidth, AStyle, AHandle);
end;

class function TdxGPResourceManager.GetPen(AColor: TdxAlphaColor; AWidth: Single; AStyle: TPenStyle; out AHandle: IdxGPHandle): Boolean;
const
  PenStyleToDashStyle: array[psSolid..psDashDotDot] of GpDashStyle = (
    DashStyleSolid, DashStyleDash, DashStyleDot, DashStyleDashDot, DashStyleDashDotDot
  );
begin
  Result := (AStyle >= psSolid) and (AStyle <= psDashDotDot) and GetPen(AColor, AWidth, PenStyleToDashStyle[AStyle], AHandle);
end;

class function TdxGPResourceManager.GetPen(AColor: TdxAlphaColor;
  AWidth: Single; AStyle: TdxGPPenStyle; out AHandle: IdxGPHandle): Boolean;
begin
  Result := GetPen(AColor, AWidth, GpPenStyleToDashStyle[AStyle], AHandle);
end;

class function TdxGPResourceManager.GetPen(AColor: TdxAlphaColor; AWidth: Single; AStyle: DashStyle; out AHandle: IdxGPHandle): Boolean;
begin
  Result := not TdxAlphaColors.IsTransparentOrEmpty(AColor) and (AWidth > 0);
  if Result then
    AHandle := FSolidPens.GetValue(TdxGPSolidPenCacheID.Create(AColor, AWidth, AStyle));
end;

class function TdxGPResourceManager.GetSolidBrush(const AColor: TdxAlphaColor; out AHandle: IdxGPHandle): Boolean;
begin
  Result := not TdxAlphaColors.IsTransparentOrEmpty(AColor);
  if Result then
    AHandle := FSolidBrushes.GetValue(AColor);
end;

class function TdxGPResourceManager.GetSolidBrush(const AColor: TColor; AColorAlpha: Byte; out AHandle: IdxGPHandle): Boolean;
begin
  Result := AColor <> clNone;
  if Result then
    AHandle := FSolidBrushes.GetValue(dxColorToAlphaColor(AColor, AColorAlpha));
end;

class procedure TdxGPResourceManager.Finalize;
begin
  FreeAndNil(FSolidBrushes);
  FreeAndNil(FSolidPens);
end;

class procedure TdxGPResourceManager.Initialize;
begin
  FSolidBrushes := TdxGPSolidBrushCacheManager.Create(CacheSize);
  FSolidPens := TdxGPSolidPenCacheManager.Create(CacheSize);
end;

{ TdxGPImageCodecMemoryBitmap }

class function TdxGPImageCodecMemoryBitmap.Description: string;
begin
  Result := '';
end;

class function TdxGPImageCodecMemoryBitmap.ID: TdxImageDataFormat;
begin
  Result := dxImageMemoryBmp;
end;

//

procedure RegisterAssistants;

  procedure RegisterSmartImageGraphicFormat(ACodec: TGUID;
    const AExtension, ADescription, AClipboardFormatDescription: string;
    AImageDataFormat: TdxImageDataFormat; AImageClass: TGraphicClass);
  var
    AClipboardFormat: Word;
  begin
    if not IsCodecIDValid(ACodec) then
      Exit;
  {$IFDEF DXREGISTERPNGIMAGE}
    TPicture.RegisterFileFormat(AExtension, ADescription, AImageClass);
  {$ENDIF}
    AClipboardFormat := RegisterClipboardFormat(PChar(AClipboardFormatDescription));
    TPicture.RegisterClipboardFormat(AClipboardFormat, AImageClass);
    ImageClipboardFormats[AImageDataFormat] := AClipboardFormat;
  end;

begin
  @FWindowFromDC := GetProcAddress(GetModuleHandle(user32), 'WindowFromDC');
  if CheckGdiPlus then
  begin
    TdxGPResourceManager.Initialize;
    CheckImageCodecs;
    RegisterClasses([TdxPNGImage, TdxSmartImage]);
    ImageClipboardFormats[dxImageBitmap] := CF_BITMAP;

    RegisterSmartImageGraphicFormat(PNGDecoder, 'PNG', TdxGPImageCodecPNG.Description, 'PNG', dxImagePng, TdxPNGImage);
    RegisterSmartImageGraphicFormat(TIFFDecoder, 'TIFF', TdxGPImageCodecTIFF.Description, 'TIFF', dxImageTiff, TdxSmartImage);
    RegisterSmartImageGraphicFormat(TIFFDecoder, 'TIF', TdxGPImageCodecTIFF.Description, 'TIFF', dxImageTiff, TdxSmartImage);
    RegisterSmartImageGraphicFormat(GIFDecoder, 'GIF', TdxGPImageCodecGIF.Description, 'GIF', dxImageGif, TdxSmartImage);
    RegisterSmartImageGraphicFormat(JPEGDecoder, 'JPEG', TdxGPImageCodecJPEG.Description, 'JPEG', dxImageJpeg, TdxSmartImage);
    RegisterSmartImageGraphicFormat(JPEGDecoder, 'JPG', TdxGPImageCodecJPEG.Description, 'JPEG', dxImageJpeg, TdxSmartImage);

  {$IFNDEF DXREGISTERPNGIMAGE}
    TPicture.RegisterFileFormat('', '', TdxGPImage);
    TPicture.RegisterFileFormat('', '', TdxPNGImage);
    TPicture.RegisterFileFormat('', '', TdxSmartImage);
  {$ENDIF}

    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecBMP);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecMemoryBitmap);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecGIF);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecJPG);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecJPEG);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecPNG);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecTIF);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecTIFF);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecEMF);
    TdxSmartImageCodecsRepository.Register(TdxGPImageCodecWMF);
  end;
end;

procedure UnregisterAssistants;
begin
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecBMP);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecMemoryBitmap);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecGIF);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecJPG);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecJPEG);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecPNG);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecTIF);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecTIFF);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecEMF);
  TdxSmartImageCodecsRepository.Unregister(TdxGPImageCodecWMF);

  TdxGPStringFormat.Finalize;
  TdxGPFontFamily.Finalize;
  TdxGPResourceManager.Finalize;
  FreeAndNil(GPAlphaBlendAttributes);
  FreeAndNil(GPPaintCanvas);
  TPicture.UnregisterGraphicClass(TdxGPImage);
  TPicture.UnregisterGraphicClass(TdxSmartImage);
  TPicture.UnregisterGraphicClass(TdxPNGImage);
  UnregisterClasses([TdxPNGImage, TdxSmartImage]);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);
end.

