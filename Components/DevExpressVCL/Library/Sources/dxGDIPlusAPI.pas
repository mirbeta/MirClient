
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

unit dxGDIPlusAPI;

{$I cxVer.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

(**************************************************************************\
*
*   GDI+ public header file
*
\**************************************************************************)

uses
  Types, Windows, Graphics, Classes, SysUtils, ActiveX, SysConst,
  dxCore, dxCoreGraphics, cxGeometry;

const
  QualityModeInvalid                        = -1;
  QualityModeDefault                        = 0;
  QualityModeLow                            = 1; // Best performance
  QualityModeHigh                           = 2; // Best rendering quality

  InterpolationModeInvalid                  = QualityModeInvalid;     //-1
  InterpolationModeDefault                  = QualityModeDefault;     //0
  InterpolationModeLowQuality               = QualityModeLow;         //1
  InterpolationModeHighQuality              = QualityModeHigh;        //2
  InterpolationModeBilinear                 = QualityModeHigh + 1;    //3
  InterpolationModeBicubic                  = QualityModeHigh + 2;    //4
  InterpolationModeNearestNeighbor          = QualityModeHigh + 3;    //5
  InterpolationModeHighQualityBilinear      = QualityModeHigh + 4;    //6
  InterpolationModeHighQualityBicubic       = QualityModeHigh + 5;    //7

// Alpha Compositing mode constants
  CompositingModeSourceOver                 = 0;
  CompositingModeSourceCopy                 = 1;

// Alpha Compositing quality constants
  CompositingQualityInvalid                 = QualityModeInvalid;     //-1
  CompositingQualityDefault                 = QualityModeDefault;     //0
  CompositingQualityHighSpeed               = QualityModeLow;         //1
  CompositingQualityHighQuality             = QualityModeHigh;        //2
  CompositingQualityGammaCorrected          = QualityModeHigh + 1;    //3
  CompositingQualityAssumeLinear            = QualityModeHigh + 2;    //4

  SmoothingModeInvalid     = QualityModeInvalid;                      //-1
  SmoothingModeDefault     = QualityModeDefault;                      //0
  SmoothingModeHighSpeed   = QualityModeLow;                          //1
  SmoothingModeHighQuality = QualityModeHigh;                         //2
  SmoothingModeNone        = QualityModeHigh + 1;                     //3
  SmoothingModeAntiAlias   = QualityModeHigh + 2;                     //4
                                                                      //5
  PixelFormat32bppRGB   = $22009;
  {$EXTERNALSYM PixelFormat32bppRGB}
  PixelFormat32bppARGB  = $26200A;
  {$EXTERNALSYM PixelFormat32bppARGB}
  PixelFormat32bppPARGB = $E200B;
  {$EXTERNALSYM PixelFormat32bppPARGB}

  // Image format IDs
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';

  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';

  EncoderParameterValueTypeASCII         : Integer = 2;
  EncoderParameterValueTypeByte          : Integer = 1;
  EncoderParameterValueTypeLong          : Integer = 4;
  EncoderParameterValueTypeLongRange     : Integer = 6;
  EncoderParameterValueTypeRational      : Integer = 5;
  EncoderParameterValueTypeRationalRange : Integer = 8;
  EncoderParameterValueTypeShort         : Integer = 3;
  EncoderParameterValueTypeUndefined     : Integer = 7;

const
  ImageLockModeRead         = $0001;
  {$EXTERNALSYM ImageLockModeRead}
  ImageLockModeWrite        = $0002;
  {$EXTERNALSYM ImageLockModeWrite}
  ImageLockModeUserInputBuf = $0004;
  {$EXTERNALSYM ImageLockModeUserInputBuf}

type

  // GDI+ base memory allocation class
  TdxGpBase = class(TObject)
  private
    FIsGpUsed: Boolean;
  public
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

  TgpImageAbort = function: Bool;
  GpEnumerateMetafileProc = function(ARecordType: Integer; AFlags: UINT;  ADataSize: UINT; AData: PBYTE; ACallbackData: Pointer): BOOL; stdcall;


// Fill mode constants
  FillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );

// Various wrap modes for brushes
  WrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TdxGpWrapMode = WrapMode;

// Various hatch styles
  HatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond,                // = 52,

    HatchStyleTotal                        // = 53,
  );

const
  HatchStyleLargeGrid = HatchStyleCross; // 4
  HatchStyleMin       = HatchStyleHorizontal;
  HatchStyleMax       = HatchStyleSolidDiamond;

type
  TdxGpGraphicsState = Cardinal;
  TdxGpGraphicsContainer = Cardinal;
  TdxGpPixelFormat = Integer;
  TdxGpHatchStyle = HatchStyle;

// Dash style constants
  DashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );
  TdxGpDashStyle = DashStyle;

// WarpMode constants
  WarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  TdxGpWarpMode = WarpMode;

// LineGradient Mode
  LinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  TdxGpLinearGradientMode = LinearGradientMode;

// Pen types
  PenAlignment = (
    PenAlignmentCenter,      // = 0,
    PenAlignmentInset,       // = 1
    PenAlignmentLeft,        // used in .net framework
    PenAlignmentOutSet,      // used in .net framework
    PenAlignmentRight        // used in .net framework
  );
  TdxGpPenAlignment = PenAlignment;

// Brush types
  BrushType = (
   BrushTypeSolidColor,      // = 0,
   BrushTypeHatchFill,       // = 1,
   BrushTypeTextureFill,     // = 2,
   BrushTypePathGradient,    // = 3,
   BrushTypeLinearGradient   // = 4
  );
  TdxGpBrushType = BrushType;

  TdxGpLineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,
    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence
    LineCapCustom           = $ff, // custom cap
    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );

  TdxGpDashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );

  TdxGpLineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );

  TdxGpCustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );

  TdxGpImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );

  TdxGpFlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );

  TdxGpRotateFlipType = (
    RotateNoneFlipNone, // = 0,
    Rotate90FlipNone,   // = 1,
    Rotate180FlipNone,  // = 2,
    Rotate270FlipNone,  // = 3,

    RotateNoneFlipX,    // = 4,
    Rotate90FlipX,      // = 5,
    Rotate180FlipX,     // = 6,
    Rotate270FlipX      // = 7,
  );

  TdxGpColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );

  TGpPropertyItem = record // NOT PACKED !!
    id        : PROPID;  // ID of this property
    length    : ULONG;   // Length of the property value, in bytes
    DataType  : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value     : Pointer; // property value
  end;
  PGpPropertyItem = ^TGpPropertyItem;

  TdxGpPixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone        = Ord(QualityModeHigh) + 1, // No pixel offset
    PixelOffsetModeHalf        = Ord(QualityModeHigh) + 2  // Offset by -0.5, -0.5 for fast anti-alias perf
  );

  TdxGpTextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );

  TdxGpCoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );

  TdxPWMFRect16 = packed record
    Left   : SmallInt;
    Top    : SmallInt;
    Right  : SmallInt;
    Bottom : SmallInt;
  end;
  PPWMFRect16 = ^TdxPWMFRect16;

  TdxGpWmfPlaceableFileHeader = packed record
    Key         : Cardinal;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : SmallInt;       // Metafile HANDLE number (always 0)
    BoundingBox : TdxPWMFRect16;  // Coordinates in metafile units
    Inch        : SmallInt;       // Number of metafile units per inch
    Reserved    : Cardinal;      // Reserved (always 0)
    Checksum    : SmallInt;       // Checksum value for previous 10 WORDs
  end;
  PGpWmfPlaceableFileHeader = ^TdxGpWmfPlaceableFileHeader;

  TdxGpMetafileFrameUnit = (
    MetafileFrameUnitPixel      = Ord(guPixel),
    MetafileFrameUnitPoint      = Ord(guPoint),
    MetafileFrameUnitInch       = Ord(guInch),
    MetafileFrameUnitDocument   = Ord(guDocument),
    MetafileFrameUnitMillimeter = Ord(guMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );

  TdxGpMetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );

  TdxGpEmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );

  TdxGpStringAlignment = (
    StringAlignmentNear, // Left edge for left-to-right text,
    StringAlignmentCenter, // right for right-to-left text,
    StringAlignmentFar // and top for vertical
  );

  TdxGpStringFormatFlags = (
    StringFormatFlagsNone                    = 0,
    StringFormatFlagsDirectionRightToLeft    = $0001,
    StringFormatFlagsDirectionVertical       = $0002,
    StringFormatFlagsNoFitBlackBox           = $0004,
    StringFormatFlagsDisplayFormatControl    = $0020,
    StringFormatFlagsNoFontFallback          = $0400,
    StringFormatFlagsMeasureTrailingSpaces   = $0800,
    StringFormatFlagsNoWrap                  = $1000,
    StringFormatFlagsLineLimit               = $2000,
    StringFormatFlagsNoClip                  = $4000
  );

  TdxGpStringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );

  TdxGpStringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );
  PdxGpStringDigitSubstitute = ^TdxGpStringDigitSubstitute;

  TdxGpCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;
  PdxGpCharacterRange = ^TdxGpCharacterRange;

  TdxGPHotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );

// Pen's Fill types
  {$EXTERNALSYM PenType}
  PenType = Integer;
const
  PenTypeSolidColor       =  0;
  PenTypeHatchFill        =  1;
  PenTypeTextureFill      =  2;
  PenTypePathGradient     =  3;
  PenTypeLinearGradient   =  4;
  PenTypeUnknown          = -1;

type
  TdxGpPenType = PenType;

// Status return values from GDI+ methods
type
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TdxGpStatus = Status;

  TdxGpColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );

  TdxGpColorMatrix = packed array[0..4, 0..4] of Single;
  PdxGpColorMatrix = ^TdxGpColorMatrix;

  TdxGpColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );

  TGpCombineMode = (
    CombineModeReplace,
    CombineModeIntersect,
    CombineModeUnion,
    CombineModeXor,
    CombineModeExclude,
    CombineModeComplement
  );

  TGpEncoderValues = (
    evColorTypeCMYK,
    evColorTypeYCCK,
    evCompressionLZW,
    evCompressionCCITT3,
    evCompressionCCITT4,
    evCompressionRle,
    evCompressionNone,
    evScanMethodInterlaced,
    evScanMethodNonInterlaced,
    evVersionGif87,
    evVersionGif89,
    evRenderProgressive,
    evRenderNonProgressive,
    evTransformRotate90,
    evTransformRotate180,
    evTransformRotate270,
    evTransformFlipHorizontal,
    evTransformFlipVertical,
    evMultiFrame,
    evLastFrame,
    evFlush,
    evFrameDimensionTime,
    evFrameDimensionResolution,
    evFrameDimensionPage
  );


type
  // Represents a dimension in a 2D coordinate system (floating-point coordinates)
  PdxGpSizeF = ^TdxGpSizeF;
  TdxGpSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

  // Represents a dimension in a 2D coordinate system (integer coordinates)
  PdxGpSize = ^TdxGpSize;
  TdxGpSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

  // Represents a location in a 2D coordinate system (floating-point coordinates)
  PdxGpPointF = ^TdxGpPointF;
  TdxGpPointF = packed record
    X : Single;
    Y : Single;
  end;
  TdxGpPointFDynArray = array of TdxGpPointF;

  // Represents a location in a 2D coordinate system (integer coordinates)
  PdxGpPoint = ^TdxGpPoint;
  TdxGpPoint = packed record
    X : Integer;
    Y : Integer;
  end;
  TdxGpPointDynArray = array of TdxGpPoint;

  // Represents a rectangle in a 2D coordinate system (floating-point coordinates)
  PdxGpRectF = ^TdxGpRectF;
  TdxGpRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
    procedure Empty;
    procedure Init(AX, AY, AWidth, AHeight: Single);
    class operator Implicit(const ARect: TRect): TdxGpRectF;
    class operator Implicit(const ARect: TdxRectF): TdxGpRectF;
    class operator Implicit(const ARect: TdxGpRectF): TRect;
    class operator Implicit(const ARect: TdxGpRectF): TdxRectF;
    function SizeF: TdxSizeF;
  end;
  TdxGpRectFDynArray = array of TdxGpRectF;

  PdxGpRect = ^TdxGpRect;
  TdxGpRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
    procedure Empty;
    procedure Init(AX, AY, AWidth, AHeight: Integer);
    class operator Implicit(const ARect: TRect): TdxGpRect;
  end;
  TdxGpRectDynArray = array of TdxGpRect;

  PdxGpImageCodecInfo = ^TdxGpImageCodecInfo;
  TdxGpImageCodecInfo = packed record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: Pointer;
    DllName: Pointer;
    FormatDescription: Pointer;
    FilenameExtension: Pointer;
    MimeType: Pointer;
    Flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PBYTE;
    SigMask: PBYTE;
  end;

  TEncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  PEncoderParameter = ^TEncoderParameter;

  TEncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // Parameter values
  end;
  PEncoderParameters = ^TEncoderParameters;

  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  DebugEventProc = procedure(level: DebugEventLevel; message: PChar); stdcall;

  NotificationHookProc = function(out token: TdxNativeInt): Status; stdcall;
  NotificationUnhookProc = procedure(token: TdxNativeInt); stdcall;

  // Input structure for GdiplusStartup
  GdiplusStartupInput = {$IFNDEF DELPHI16}packed{$ENDIF} record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;                                        // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;
  // Output structure for GdiplusStartup()
  GdiplusStartupOutput = packed record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

// Private GDI+ classes for internal type checking
  GpGraphics = Pointer;

  TGpColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: ^TdxAlphaColor; // Palette color entries
  end;
  PGpColorPalette = ^TGpColorPalette;

  TGpColorMap = packed record
    oldColor: TdxAlphaColor;
    newColor: TdxAlphaColor;
  end;
  PGpColorMap = ^TGpColorMap;

  TdxGpMatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );

  PUINT16 = ^WORD;

  GpHandle = Pointer;
  GpMetafile = Pointer;
  GpBrush = Pointer;
  GpTexture = Pointer;
  GpRegion = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpMatrix = Pointer;
  GpPathGradient = Pointer;
  GpPathIterator = Pointer;
  GpPath = Pointer;
  GpFont = Pointer;
  GpFontFamily = Pointer;
  GpFontCollection = Pointer;
  GpStringFormat = Pointer;
  GpHatch = Pointer;
  GpPen = Pointer;
  GpImage = Pointer;
  GpBitmap = Pointer;
  GpCachedBitmap = Pointer;
  GpImageAttributes = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;

  GpFillMode        = FillMode;
  GpStatus          = TdxGpStatus;
  GpWrapMode        = TdxGpWrapMode;
  GpUnit            = TdxGraphicUnit;
  GpPointF          = PdxGpPointF;
  GpPoint           = PdxGpPoint;
  GpRectF           = PdxGpRectF;
  GpRect            = PdxGpRect;
  GpSizeF           = PdxGpSizeF;
  GpHatchStyle      = TdxGpHatchStyle;
  GpDashStyle       = TdxGpDashStyle;
  GpPenAlignment    = TdxGpPenAlignment;
  GpPenType         = TdxGpPenType;
  GpBrushType       = TdxGpBrushType;

  BitmapData = packed record
    Width: UINT;
    Height: UINT;
    Stride: Integer;
    PixelFormat: Integer;
    Scan0: Pointer;
    Reserved: TdxNativeUInt;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

type
  { EdxGdipException }

  EdxGdipException = class(EdxException)
  private
    FStatus: GpStatus;
  public
    constructor Create(AStatus: GpStatus);
    //
    property Status: GpStatus read FStatus;
  end;

var
  // codecs
  BMPEncoder: TGUID;
  BMPDecoder: TGUID;
  GIFEncoder: TGUID;
  GIFDecoder: TGUID;
  JPEGEncoder: TGUID;
  JPEGDecoder: TGUID;
  PNGEncoder: TGUID;
  PNGDecoder: TGUID;
  TIFFEncoder: TGUID;
  TIFFDecoder: TGUID;
  EMFDecoder: TGUID;
  ICONDecoder: TGUID;
  WMFDecoder: TGUID;
  // GDI+ Memory management methods
  GdipAlloc: function(ASize: ULONG): Pointer; stdcall;
  GdipFree: procedure(APointer: pointer); stdcall;
  // GDI+ initialization/finalization methods
  GdiplusStartup: function(out AToken: TdxNativeInt; const AInput: GdiPlusStartupInput; AOutput: PGdiPlusStartupOutput): GpStatus; stdcall;
  GdiplusShutdown: procedure(AToken: TdxNativeInt); stdcall;
  // GDI+ Brush methods
  GdipCloneBrush: function(ABrush: GpBrush; var ACloneBrush: GpBrush): GpStatus; stdcall;
  GdipDeleteBrush: function(ABrush: GpBrush): GpStatus; stdcall;
  GdipGetBrushType: function(ABrush: GpBrush; var ABrushType: GpBrushType): GpStatus; stdcall;
  // GDI+ Solid Brush methods
  GdipCreateSolidFill: function(AColor: TdxAlphaColor; var ABrush: GpSolidFill): GpStatus; stdcall;
  GdipSetSolidFillColor: function(ABrush: GpSolidFill; AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipGetSolidFillColor: function(ABrush: GpSolidFill; var AColor: TdxAlphaColor): GpStatus; stdcall;
  // GDI+ Gradient Brush methods
  GdipCreatePathGradient: function(APoints: GpPointF; ACount: Integer; AWrapMode: TdxGpWrapMode; out APolyGradient: GpPathGradient): GpStatus; stdcall;
  GdipCreatePathGradientI: function(APoints: GpPoint; ACount: Integer; AWrapMode: TdxGpWrapMode; out APolyGradient: GpPathGradient): GpStatus; stdcall;
  GdipCreatePathGradientFromPath: function(APath: GpPath; out APolyGradient: GpPathGradient): GpStatus; stdcall;
  GdipGetPathGradientCenterColor: function(ABrush: GpPathGradient; out AColors: TdxAlphaColor): GpStatus; stdcall;
  GdipSetPathGradientCenterColor: function(ABrush: GpPathGradient; AColors: TdxAlphaColor): GpStatus; stdcall;
  GdipGetPathGradientSurroundColorsWithCount: function(ABrush: GpPathGradient; AColors: PdxAlphaColor; var ACount: Integer): GpStatus; stdcall;
  GdipSetPathGradientSurroundColorsWithCount: function(ABrush: GpPathGradient; AColors: PdxAlphaColor; var ACount: Integer): GpStatus; stdcall;
  GdipGetPathGradientPath: function(ABrush: GpPathGradient; APath: GpPath): GpStatus; stdcall;
  GdipSetPathGradientPath: function(ABrush: GpPathGradient; APath: GpPath): GpStatus; stdcall;
  GdipGetPathGradientCenterPoint: function(ABrush: GpPathGradient; APoints: GpPointF): GpStatus; stdcall;
  GdipGetPathGradientCenterPointI: function(ABrush: GpPathGradient; APoints: GpPoint): GpStatus; stdcall;
  GdipSetPathGradientCenterPoint: function(ABrush: GpPathGradient; APoints: GpPointF): GpStatus; stdcall;
  GdipSetPathGradientCenterPointI: function(ABrush: GpPathGradient; APoints: GpPoint): GpStatus; stdcall;
  GdipGetPathGradientRect: function(ABrush: GpPathGradient; ARect: GpRectF): GpStatus; stdcall;
  GdipGetPathGradientRectI: function(ABrush: GpPathGradient; ARect: GpRect): GpStatus; stdcall;
  GdipGetPathGradientPointCount: function(ABrush: GpPathGradient; var ACount: Integer): GpStatus; stdcall;
  GdipGetPathGradientSurroundColorCount: function(ABrush: GpPathGradient; var ACount: Integer): GpStatus; stdcall;
  GdipSetPathGradientGammaCorrection: function(ABrush: GpPathGradient; AUseGammaCorrection: Bool): GpStatus; stdcall;
  GdipGetPathGradientGammaCorrection: function(ABrush: GpPathGradient; var AUseGammaCorrection: Bool): GpStatus; stdcall;
  GdipGetPathGradientBlendCount: function(ABrush: GpPathGradient; var ACount: Integer): GpStatus; stdcall;
  GdipGetPathGradientBlend: function(ABrush: GpPathGradient; ABlend, APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetPathGradientBlend: function(ABrush: GpPathGradient; ABlend, APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipGetPathGradientPresetBlendCount: function(ABrush: GpPathGradient; var ACount: Integer): GpStatus; stdcall;
  GdipGetPathGradientPresetBlend: function(ABrush: GpPathGradient; ABlend: TdxAlphaColor; APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetPathGradientPresetBlend: function(ABrush: GpPathGradient; ABlend: TdxAlphaColor; APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetPathGradientSigmaBlend: function(ABrush: GpPathGradient; AFocus, AScale: Single): GpStatus; stdcall;
  GdipSetPathGradientLinearBlend: function(ABrush: GpPathGradient; AFocus, AScale: Single): GpStatus; stdcall;
  GdipGetPathGradientWrapMode: function(ABrush: GpPathGradient; var AWrapMode: TdxGpWrapMode): GpStatus; stdcall;
  GdipSetPathGradientWrapMode: function(ABrush: GpPathGradient; AWrapMode: TdxGpWrapMode): GpStatus; stdcall;
  GdipGetPathGradientTransform: function(ABrush: GpPathGradient; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipSetPathGradientTransform: function(ABrush: GpPathGradient; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetPathGradientTransform: function(ABrush: GpPathGradient): GpStatus; stdcall;
  GdipMultiplyPathGradientTransform: function(ABrush: GpPathGradient; AMatrix: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslatePathGradientTransform: function(ABrush: GpPathGradient; DX, DY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScalePathGradientTransform: function(ABrush: GpPathGradient; SX, SY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotatePathGradientTransform: function(ABrush: GpPathGradient; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipGetPathGradientFocusScales: function(ABrush: GpPathGradient; var AXScale: Single; var AYScale: Single): GpStatus; stdcall;
  GdipSetPathGradientFocusScales: function(ABrush: GpPathGradient; AXScale, AYScale: Single): GpStatus; stdcall;
  GdipCreateLineBrush: function(APoint1, APoint2: GpPointF; AColor1, AColor2: TdxAlphaColor; AWrapMode: TdxGpWrapMode; out ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipCreateLineBrushI: function(APoint1, APoint2: GpPoint; AColor1, AColor2: TdxAlphaColor; AWrapMode: TdxGpWrapMode; out ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipCreateLineBrushFromRect: function(const ARect: GpRectF; AColor1, AColor2: TdxAlphaColor; ALinearGradientMode: TdxGpLinearGradientMode; AWrapMode: TdxGpWrapMode; out ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipCreateLineBrushFromRectI: function(const ARect: GpRect; Acolor1, AColor2: TdxAlphaColor; ALinearGradientMode: LinearGradientMode; AWrapMode: GpWrapMode; var ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipCreateLineBrushFromRectWithAngle: function(ARect: GpRectF; AColor1, AColor2: TdxAlphaColor; AAngle: Single; AIsAngleScalable: Bool; AWrapMode: TdxGpWrapMode; out ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipCreateLineBrushFromRectWithAngleI: function(ARect: GpRect; AColor1, AColor2: TdxAlphaColor; AAngle: Single; AIsAngleScalable: Bool; AWrapMode: TdxGpWrapMode; out ALineGradient: GpLineGradient): GpStatus; stdcall;
  GdipGetLineRect: function(ABrush: GpLineGradient; var ARect: GpRectF): GpStatus; stdcall;
  GdipGetLineRectI: function(ABrush: GpLineGradient; var ARect: GpRect): GpStatus; stdcall;
  GdipSetLineColors: function(ABrush: GpLineGradient; AColor1, AColor2: TdxAlphaColor): GpStatus; stdcall;
  GdipGetLineColors: function(ABrush: GpLineGradient; AColors: PdxAlphaColor): GpStatus; stdcall;
  GdipSetLineGammaCorrection: function(ABrush: GpLineGradient; AUseGammaCorrection: Bool): GpStatus; stdcall;
  GdipGetLineGammaCorrection: function(ABrush: GpLineGradient; out AUseGammaCorrection: Bool): GpStatus; stdcall;
  GdipGetLineBlendCount: function(ABrush: GpLineGradient; out ACount: Integer): GpStatus; stdcall;
  GdipGetLineBlend: function(ABrush: GpLineGradient; ABlend, APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetLineBlend: function(ABrush: GpLineGradient; ABlend, APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipGetLinePresetBlendCount: function(ABrush: GpLineGradient; out ACount: Integer): GpStatus; stdcall;
  GdipGetLinePresetBlend: function(ABrush: GpLineGradient; ABlend: PdxAlphaColor; APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetLinePresetBlend: function(ABrush: GpLineGradient; ABlend: PdxAlphaColor; APositions: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipSetLineSigmaBlend: function(ABrush: GpLineGradient; AFocus, AScale: Single): GpStatus; stdcall;
  GdipSetLineLinearBlend: function(ABrush: GpLineGradient; AFocus, AScale: Single): GpStatus; stdcall;
  GdipSetLineWrapMode: function(ABrush: GpLineGradient; AWrapMode: GpWrapMode): GpStatus; stdcall;
  GdipGetLineWrapMode: function(ABrush: GpLineGradient; AWrapMode: GpWrapMode): GpStatus; stdcall;
  GdipGetLineTransform: function(ABrush: GpLineGradient; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipSetLineTransform: function(ABrush: GpLineGradient; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetLineTransform: function(ABrush: GpLineGradient): GpStatus; stdcall;
  GdipMultiplyLineTransform: function(ABrush: GpLineGradient; AMatrix: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslateLineTransform: function(ABrush: GpLineGradient; DX, DY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScaleLineTransform: function(ABrush: GpLineGradient; SX, SY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotateLineTransform: function(ABrush: GpLineGradient; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  // GDI+ Hatch Brush methods
  GdipCreateHatchBrush: function(AHatchStyle: GpHatchStyle; AForeColor, ABackColor: TdxAlphaColor; var ABrush: GpHatch): GpStatus; stdcall;
  GdipGetHatchStyle: function(ABrush: GpHatch; var AHatchStyle: GpHatchStyle): GpStatus; stdcall;
  GdipGetHatchForegroundColor: function(ABrush: GpHatch; var AForeColor: TdxAlphaColor): GpStatus; stdcall;
  GdipGetHatchBackgroundColor: function(ABrush: GpHatch; var ABackColor: TdxAlphaColor): GpStatus; stdcall;
  // GDI+ Pen methods
  GdipCreatePen1: function(AColor: TdxAlphaColor; AWidth: Single; AUnit: GpUnit; var APen: GpPen): GpStatus; stdcall;
  GdipCreatePen2: function(ABrush: GpBrush; AWidth: Single; AUnit: GpUnit; var APen :GpPen): GpStatus; stdcall;
  GdipClonePen: function(APen: GpPen; var AClonePen: GpPen): GpStatus; stdcall;
  GdipDeletePen: function(APen: GpPen): GpStatus; stdcall;
  GdipGetPenFillType: function(APen: GpPen; var APenType: GpPenType): GpStatus; stdcall;
  GdipSetPenBrushFill: function(APen: GpPen; ABrush: GpBrush): GpStatus; stdcall;
  GdipGetPenBrushFill: function(APen: GpPen; var ABrush: GpBrush): GpStatus; stdcall;
  GdipSetPenColor: function(APen: GpPen; AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipGetPenColor: function(APen: GpPen; var AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipSetPenMode: function(APen: GpPen; APenMode: GpPenAlignment): GpStatus; stdcall;
  GdipGetPenMode: function(pen :GpPen; var penMode :GpPenAlignment) :GpStatus; stdcall;
  GdipSetPenUnit: function(APen: GpPen; AUnit: GpUnit): GpStatus; stdcall;
  GdipGetPenUnit: function(APen: GpPen; var AUnit: GpUnit): GpStatus; stdcall;
  GdipSetPenWidth: function(APen: GpPen; AWidth: Single): GpStatus; stdcall;
  GdipGetPenWidth: function(APen: GpPen; var AWidth: Single): GpStatus; stdcall;
  GdipGetPenDashStyle: function(APen: GpPen; out ADashStyle: GpDashStyle): GpStatus; stdcall;
  GdipSetPenDashStyle: function(APen: GpPen; ADashStyle: GpDashStyle): GpStatus; stdcall;
  GdipSetPenLineCap197819: function(APen: GpPen; AStartCap: TdxGpLineCap; AEndCap: TdxGpLineCap; ADashCap: TdxGpDashCap): GpStatus; stdcall;
  GdipSetPenStartCap: function(APen: GpPen; AStartCap: TdxGpLineCap): GpStatus; stdcall;
  GdipSetPenEndCap: function(APen: GpPen; AEndCap: TdxGpLineCap): GpStatus; stdcall;
  GdipSetPenDashCap197819: function(APen: GpPen; ADashCap: TdxGpDashCap): GpStatus; stdcall;
  GdipGetPenStartCap: function(APen: GpPen; out AStartCap: TdxGpLineCap): GpStatus; stdcall;
  GdipGetPenEndCap: function(APen: GpPen; out AEndCap: TdxGpLineCap): GpStatus; stdcall;
  GdipGetPenDashCap197819: function(APen: GpPen; out ADashCap: TdxGpDashCap): GpStatus; stdcall;
  GdipSetPenLineJoin: function(APen: GpPen; ALineJoin: TdxGpLineJoin): GpStatus; stdcall;
  GdipGetPenLineJoin: function(APen: GpPen; var ALineJoin: TdxGpLineJoin): GpStatus; stdcall;
  GdipSetPenCustomStartCap: function(APen: GpPen; ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipGetPenCustomStartCap: function(APen: GpPen; out ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipSetPenCustomEndCap: function(APen: GpPen; ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipGetPenCustomEndCap: function(APen: GpPen; out ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipSetPenMiterLimit: function(APen: GpPen; AMiterLimit: Single): GpStatus; stdcall;
  GdipGetPenMiterLimit: function(APen: GpPen; out AMiterLimit: Single): GpStatus; stdcall;
  GdipSetPenTransform: function(APen: GpPen; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetPenTransform: function(APen: GpPen; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetPenTransform: function(APen: GpPen): GpStatus; stdcall;
  GdipMultiplyPenTransform: function(APen: GpPen; AMatrix: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslatePenTransform: function(APen: GpPen; DX, DY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScalePenTransform: function(APen: GpPen; SX, SY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotatePenTransform: function(APen: GpPen; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipGetPenDashOffset: function(APen: GpPen; out AOffset: Single): GpStatus; stdcall;
  GdipSetPenDashOffset: function(APen: GpPen; AOffset: Single): GpStatus; stdcall;
  GdipGetPenDashCount: function(APen: GpPen; var ACount: Integer): GpStatus; stdcall;
  GdipSetPenDashArray: function(APen: GpPen; ADash: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipGetPenDashArray: function(APen: GpPen; ADash: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipGetPenCompoundCount: function(APen: GpPen; out ACount: Integer): GpStatus; stdcall;
  GdipSetPenCompoundArray: function(APen: GpPen; ADash: PSingle; ACount: Integer): GpStatus; stdcall;
  GdipGetPenCompoundArray: function(APen: GpPen; ADash: PSingle; ACount: Integer): GpStatus; stdcall;
  // GDI+ CustomLineCap
  GdipCreateCustomLineCap: function(AFillPath, AStrokePath: GpPath; ABaseCap: TdxGpLineCap; ABaseInset: Single; out ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipDeleteCustomLineCap: function(ACustomCap: GpCustomLineCap): GpStatus; stdcall;
  GdipCloneCustomLineCap: function(ACustomCap: GpCustomLineCap; out AClonedCap: GpCustomLineCap): GpStatus; stdcall;
  GdipGetCustomLineCapType: function(ACustomCap: GpCustomLineCap; var ACapType: TdxGpCustomLineCapType): GpStatus; stdcall;
  GdipSetCustomLineCapStrokeCaps: function(ACustomCap: GpCustomLineCap; AStartCap: TdxGpLineCap): GpStatus; stdcall;
  GdipGetCustomLineCapStrokeCaps: function(ACustomCap: GpCustomLineCap; var AStartCap: TdxGpLineCap; var AEndCap: TdxGpLineCap): GpStatus; stdcall;
  GdipSetCustomLineCapStrokeJoin: function(ACustomCap: GpCustomLineCap; ALineJoin: TdxGpLineJoin): GpStatus; stdcall;
  GdipGetCustomLineCapStrokeJoin: function(ACustomCap: GpCustomLineCap; var ALineJoin: TdxGpLineJoin): GpStatus; stdcall;
  GdipSetCustomLineCapBaseCap: function(ACustomCap: GpCustomLineCap; ABaseCap: TdxGpLineCap): GpStatus; stdcall;
  GdipGetCustomLineCapBaseCap: function(ACustomCap: GpCustomLineCap; var ABaseCap: TdxGpLineCap): GpStatus; stdcall;
  GdipSetCustomLineCapBaseInset: function(ACustomCap: GpCustomLineCap; AInset: Single): GpStatus; stdcall;
  GdipGetCustomLineCapBaseInset: function(ACustomCap: GpCustomLineCap; var AInset: Single): GpStatus; stdcall;
  GdipSetCustomLineCapWidthScale: function(ACustomCap: GpCustomLineCap; AWidthScale: Single): GpStatus; stdcall;
  GdipGetCustomLineCapWidthScale: function(ACustomCap: GpCustomLineCap; var AWidthScale: Single): GpStatus; stdcall;
  // GDI+ AdjustableArrowCap
  GdipCreateAdjustableArrowCap: function(AHeight, AWidth: Single; AIsFilled: Bool; out ACap: GpAdjustableArrowCap): GpStatus; stdcall;
  GdipSetAdjustableArrowCapHeight: function(ACap: GpAdjustableArrowCap; AHeight: Single): GpStatus; stdcall;
  GdipGetAdjustableArrowCapHeight: function(ACap: GpAdjustableArrowCap; var AHeight: Single): GpStatus; stdcall;
  GdipSetAdjustableArrowCapWidth: function(ACap: GpAdjustableArrowCap; AWidth: Single): GpStatus; stdcall;
  GdipGetAdjustableArrowCapWidth: function(ACap: GpAdjustableArrowCap; var AWidth: Single): GpStatus; stdcall;
  GdipSetAdjustableArrowCapMiddleInset: function(ACap: GpAdjustableArrowCap; AMiddleInset: Single): GpStatus; stdcall;
  GdipGetAdjustableArrowCapMiddleInset: function(ACap: GpAdjustableArrowCap; var AMiddleInset: Single): GpStatus; stdcall;
  GdipSetAdjustableArrowCapFillState: function(ACap: GpAdjustableArrowCap; AFillState: Bool): GpStatus; stdcall;
  GdipGetAdjustableArrowCapFillState: function(ACap: GpAdjustableArrowCap; var AFillState: Bool): GpStatus; stdcall;
  // GDI+ Graphics methods
  GdipFlush: function (AGraphics: GpGraphics; AIntention: TdxGpFlushIntention): GpStatus; stdcall;
  GdipCreateFromHDC: function(DC: HDC; var AGraphics: GpGraphics): GpStatus; stdcall;
  GdipCreateFromHDC2: function(DC: HDC; AHDevice: THandle; out AGraphics: GpGraphics): GpStatus; stdcall;
  GdipCreateFromHWND: function(AWnd: HWND; out AGraphics: GpGraphics): GpStatus; stdcall;
  GdipCreateFromHWNDICM: function(AWnd: HWND; out AGraphics: GpGraphics): GpStatus; stdcall;
  GdipDeleteGraphics: function(AGraphics: GpGraphics): GpStatus; stdcall;
  GdipGetDC: function(AGraphics: GpGraphics; var DC: HDC): GpStatus; stdcall;
  GdipReleaseDC: function(AGraphics: GpGraphics; DC: HDC): GpStatus; stdcall;
  GdipGraphicsClear: function(AGraphics: GpGraphics; AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipCreateHalftonePalette: function: HPALETTE; stdcall;
  GdipDrawLine: function(AGraphics: GpGraphics; APen: GpPen; X1, Y1, X2, Y2: Single): GpStatus; stdcall;
  GdipDrawLineI: function(AGraphics: GpGraphics; APen: GpPen; X1, Y1, X2, Y2: Integer): GpStatus; stdcall;
  GdipDrawLines: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawLinesI: function(AGraphics: GpGraphics; APen: GpPen; const APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipFillRectangle: function(AGraphics: GpGraphics; ABrush: GpBrush; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipFillRectangleI: function(graphics :GpGraphics; brush :GpBrush; x,y,width,height :integer) :GpStatus; stdcall;
  GdipFillRectangles: function (AGraphics: GpGraphics; ABrush: GpBrush; ARects: GpRectF; ACount: Integer): GpStatus; stdcall;
  GdipFillRectanglesI: function (AGraphics: GpGraphics; ABrush: GpBrush; ARects: GpRect; ACount: Integer): GpStatus; stdcall;
  GdipFillRegion: function (AGraphics: GpGraphics; ABrush: GpBrush; ARegion: GpRegion): GpStatus; stdcall;
  GdipDrawImage: function(AGraphics: GpGraphics; AImage: GpImage; X, Y: Single): GpStatus; stdcall;
  GdipDrawImageI: function(AGraphics: GpGraphics; AImage: GpImage; X, Y: Integer): GpStatus; stdcall;
  GdipDrawArc: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight, AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipDrawArcI: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Integer; AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipDrawBezier: function(AGraphics: GpGraphics; APen: GpPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): GpStatus; stdcall;
  GdipDrawBezierI: function(AGraphics: GpGraphics; APen: GpPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): GpStatus; stdcall;
  GdipDrawBeziers: function (AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawBeziersI: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipDrawRectangle: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipDrawRectangleI: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipDrawRectangles: function(AGraphics: GpGraphics; APen: GpPen; ARects: GpRectF; ACount: Integer): GpStatus; stdcall;
  GdipDrawRectanglesI: function(AGraphics: GpGraphics; APen: GpPen; ARects: GpRect; ACount: Integer): GpStatus; stdcall;
  GdipDrawEllipse: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipDrawEllipseI: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipDrawPie: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight, AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipDrawPieI: function(AGraphics: GpGraphics; APen: GpPen; X, Y, AWidth, AHeight: Integer; AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipDrawPolygon: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawPolygonI: function(AGraphics: GpGraphics; APen: GpPen; const APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipDrawCurve: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawCurveI: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipDrawCurve2: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipDrawCurve2I: function(AGraphics: GpGraphics; APen: GpPen; const APoints: GpPoint; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipDrawCurve3: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount, AOffset, ANumberOfSegments: Integer; ATension: Single): GpStatus; stdcall;
  GdipDrawCurve3I: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPoint; ACount, AOffset, ANumberOfSegments: Integer; ATension: Single): GpStatus; stdcall;
  GdipDrawClosedCurve: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawClosedCurveI: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipDrawClosedCurve2: function(AGraphics: GpGraphics; APen: GpPen; APoints: GpPointF; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipDrawClosedCurve2I: function(AGraphics: GpGraphics; APen: GpPen; const APoints: GpPoint; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipFillPolygon: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPointF; ACount: Integer; AFillMode: GpFillMode): GpStatus; stdcall;
  GdipFillPolygonI: function(AGraphics: GpGraphics; ABrush: GpBrush; const APoints :GpPoint; ACount: Integer; AFillMode: GpFillMode): GpStatus; stdcall;
  GdipFillPolygon2: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipFillPolygon2I: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipFillEllipse: function(AGraphics: GpGraphics; ABrush: GpBrush; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipFillEllipseI: function(AGraphics: GpGraphics; ABrush: GpBrush; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipFillPie: function(AGraphics: GpGraphics; ABrush: GpBrush; X, Y, AWidth, AHeight, AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipFillPieI: function(AGraphics: GpGraphics; ABrush: GpBrush; X, Y, AWidth, AHeight: Integer; AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipFillClosedCurve: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipFillClosedCurveI: function(AGraphics: GpGraphics; ABrush: GpBrush; const APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipFillClosedCurve2: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPointF; ACount: Integer; ATension: Single; AFillMode: GpFillMode): GpStatus; stdcall;
  GdipFillClosedCurve2I: function(AGraphics: GpGraphics; ABrush: GpBrush; APoints: GpPoint; ACount: Integer; ATension: Single; AFillMode: GpFillMode): GpStatus; stdcall;
  GdipSetWorldTransform: function(AGraphics: GpGraphics; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetWorldTransform: function(AGraphics: GpGraphics): GpStatus; stdcall;
  GdipMultiplyWorldTransform: function(AGraphics: GpGraphics; AMatrix: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslateWorldTransform: function(AGraphics: GpGraphics; DX, DY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScaleWorldTransform: function(AGraphics: GpGraphics; SX, SY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotateWorldTransform: function(AGraphics: GpGraphics; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipGetWorldTransform: function(AGraphics: GpGraphics; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetPageTransform: function(AGraphics: GpGraphics): GpStatus; stdcall;
  GdipGetPageUnit: function(AGraphics: GpGraphics; var AUnit: GpUNIT): GpStatus; stdcall;
  GdipGetPageScale: function(AGraphics: GpGraphics; var AScale: Single): GpStatus; stdcall;
  GdipSetPageUnit: function(AGraphics: GpGraphics; AUnit: GpUNIT): GpStatus; stdcall;
  GdipSetPageScale: function(AGraphics: GpGraphics; AScale: Single): GpStatus; stdcall;
  GdipGetDpiX: function(AGraphics: GpGraphics; var DPI: Single): GpStatus; stdcall;
  GdipGetDpiY: function(AGraphics: GpGraphics; var DPI: Single): GpStatus; stdcall;
  GdipTransformPoints: function(AGraphics: GpGraphics; ADestSpace, ASrcSpace: TdxGpCoordinateSpace; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipTransformPointsI: function(AGraphics: GpGraphics; ADestSpace, ASrcSpace: TdxGpCoordinateSpace; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipGetNearestColor: function(AGraphics: GpGraphics; AColor: TdxAlphaColor): GpStatus; stdcall;

  // added from MSN
  GdipLoadImageFromStream: function(AStream: IStream; var AImage: GpImage): GpStatus; stdcall;
  GdipLoadImageFromStreamICM: function(AStream: IStream; out AImage: GpImage): GpStatus; stdcall;
  GdipSaveImageToStream: function(AImage: GpImage; AStream: IStream; AClsIDEncoder: PGUID; AEncoderParams: Pointer): GpStatus; stdcall;
  GdipSaveImageToFile: function(AImage: GpImage; AFilename: PWCHAR; AClsIDEncoder: PGUID; AEncoderParams: Pointer): GpStatus; stdcall;
  GdipGetImageRawFormat: function(AImage: GpImage; var AFormat: TGUID): GpStatus; stdcall;
  GdipGetImagePixelFormat: function(AImage: GpImage; out AFormat: TdxGpPixelFormat): GpStatus; stdcall;
  GdipGetImageThumbnail: function(AImage: GpImage; AThumbWidth, AThumbHeight: UINT; out AThumbImage: GpImage; ACallback: TGpImageAbort; ACallbackData: Pointer): GpStatus; stdcall;
  GdipCreateHBITMAPFromBitmap: function(AImage: GpBitmap; var ABitmap: HBitmap; AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipCreateBitmapFromHICON: function(AHicon: HICON; out ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateHICONFromBitmap: function(ABitmap: GpBitmap; out AHBMReturn: HICON): GpStatus; stdcall;
  GdipCreateBitmapFromResource: function(AHInstance: HMODULE; AAlpBitmapName: PWCHAR; out ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateBitmapFromHBITMAP: function(AHBitmap: HBITMAP; AHPalette: HPALETTE; out ABitmap: GpImage): GpStatus; stdcall;
  GdipCreateBitmapFromFile: function(AFileName: PWideChar; var ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateBitmapFromStream: function(AStream: IStream; var ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateBitmapFromStreamICM: function(AStream: IStream; out ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateBitmapFromFileICM: function(AFilename: PWCHAR; var ABitmap: GpBitmap): GpStatus; stdcall;
  GdipImageGetFrameCount: function(AImage: GpImage; ADimensionID: PGUID; var ACount: UINT): GpStatus; stdcall;
  GdipImageSelectActiveFrame: function(AImage: GpImage; ADimensionID: PGUID; AFrameIndex: UINT): GpStatus; stdcall;
  GdipImageRotateFlip: function(AImage: GpImage; ARotateFlipType: TdxGpRotateFlipType): GpStatus; stdcall;
  GdipGetImagePalette: function(AImage: GpImage; APalette: PGpColorPalette; ASize: Integer): GpStatus; stdcall;
  GdipSetImagePalette: function(AImage: GpImage; APalette: PGpColorPalette): GpStatus; stdcall;
  GdipGetImagePaletteSize: function(AImage: GpImage; var ASize: Integer): GpStatus; stdcall;
  GdipGetPropertyCount: function(AImage: GpImage; var ANumOfProperty: UINT): GpStatus; stdcall;
  GdipGetPropertyIdList: function(AImage: GpImage; ANumOfProperty: UINT; AList: PPropID): GpStatus; stdcall;
  GdipGetPropertyItemSize: function(AImage: GpImage; APropId: PROPID; var ASize: UINT): GpStatus; stdcall;
  GdipGetPropertyItem: function(AImage: GpImage; APropId: PROPID; APropSize: UINT; ABuffer: PGpPropertyItem): GpStatus; stdcall;
  GdipGetPropertySize: function(AImage: GpImage; var ATotalBufferSize: UINT; var ANumProperties: UINT): GpStatus; stdcall;
  GdipGetAllPropertyItems: function(AImage: GpImage; ATotalBufferSize: UINT; ANumProperties: UINT; AAllItems: PGpPropertyItem): GpStatus; stdcall;
  GdipRemovePropertyItem: function(AImage: GpImage; APropId: PROPID): GpStatus; stdcall;
  GdipSetPropertyItem: function(AImage: GpImage; AItem: PGpPropertyItem): GpStatus; stdcall;
  GdipImageForceValidation: function(AImage: GpImage): GpStatus; stdcall;
  GdipLoadImageFromFile: function(AFileName: PWideChar; var AImage: GpImage): GpStatus; stdcall;
  GdipLoadImageFromFileICM: function(AFileName: PWCHAR; out AImage: GpImage): GpStatus; stdcall;
  GdipGetImageDimension: function(AImage: GpImage; var AWidth, AHeight: Single): GpStatus; stdcall;
  GdipDrawImageRect: function(AGraphics: GpGraphics; AImage: GpImage; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipDrawImageRectI: function(AGraphics: GpGraphics; AImage: GpImage; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipDrawImagePoints: function(AGraphics: GpGraphics; AImage: GpImage; ADstPoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipDrawImagePointsI: function(AGraphics: GpGraphics; AImage: GpImage; ADstPoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipImageGetFrameDimensionsCount: function(AImage: GpImage; var ACount: UINT): GpStatus; stdcall;
  GdipImageGetFrameDimensionsList: function(AImage: GpImage; ADimensionIDs: PGUID; ACount: UINT): GpStatus; stdcall;
  GdipDisposeImage: function(AImage: GpImage): GpStatus; stdcall;
  GdipGetImageDecodersSize: function(out ANumDecoders: UINT; out ASize: UINT): GpStatus; stdcall;
  GdipGetImageDecoders: function(ANumDecoders: UINT; ASize: UINT; ADecoders: PdxGpImageCodecInfo): GpStatus; stdcall;
  GdipGetImageEncodersSize: function(out ANumEncoders: Integer; out ASize: Integer): GpStatus; stdcall;
  GdipGetImageEncoders: function(ANumEncoders: Integer; ASize: Integer; AEncoders: PdxGpImageCodecInfo): GpStatus; stdcall;
  GdipGetImageType: function(AImage: GpImage; var AType: TdxGpImageType): GpStatus; stdcall;
  GdipGetEncoderParameterListSize: function(AImage: GpImage; AClsIDEncoder: PGUID; out ASize: UINT): GpStatus; stdcall;
  GdipGetEncoderParameterList: function(AImage: GpImage; AClsIDEncoder: PGUID; ASize: UINT; ABuffer: PEncoderParameters): GpStatus; stdcall;
  GdipCreateBitmapFromGdiDib: function(AGdiBitmapInfo: PBitmapInfo; AGdiBitmapData: Pointer; out ABitmap: GpBitmap): GpStatus; stdcall;
  GdipCreateBitmapFromScan0: function(AWidth, AHeight, AStrIDE, AFormat: integer; AScan0: PBYTE; out ABitmap: GpImage): GpStatus; stdcall;
  GdipBitmapLockBits: function(ABitmap: GpBitmap; ARect: GpRect; AFlags: UINT; AFormat: Integer; ALockedBitmapData: PBitmapData): GpStatus; stdcall;
  GdipBitmapUnlockBits: function(ABitmap: GpBitmap; ALockedBitmapData: PBitmapData): GpStatus; stdcall;
  GdipBitmapGetPixel: function(ABitmap: GpBitmap; X, Y: Integer; var AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipBitmapSetPixel: function(ABitmap: GpBitmap; X, Y: Integer; AColor: TdxAlphaColor): GpStatus; stdcall;
  GdipBitmapSetResolution: function(ABitmap: GpBitmap; AXDPI, AYDPI: Single): GpStatus; stdcall;
  GdipDrawImageRectRect: function(AGraphics: GpGraphics; AImage: GpImage; ADstX, ADstY, ADstWidth, ADstHeight, ASrcX, ASrcY, ASrcWidth, ASrcHeight: Single; ASrcUnit: GpUnit; AImageAttributes: Pointer; ACallback: TgpImageAbort; ACallbackData: Pointer): GpStatus; stdcall;
  GdipDrawImageRectRectI: function(AGraphics: GpGraphics; AImage: GpImage; ADstX, ADstY, ADstWidth, ADstHeight, ASrcX, ASrcY, ASrcWidth, ASrcHeight: Integer; ASrcUnit: GpUnit; AImageAttributes: Pointer; ACallback: TgpImageAbort; ACallbackData: Pointer): GpStatus; stdcall;
  GdipDrawImagePointRect: function(AGraphics: GpGraphics; AImage: GpImage; X, Y, ASrcX, ASrcY, ASrcWidth, ASrcHeight: Single; ASrcUnit: GpUnit): GpStatus; stdcall;
  GdipDrawImagePointRectI: function(AGraphics: GpGraphics; AImage: GpImage; X, Y, ASrcX, ASrcY, ASrcWidth, ASrcHeight: Integer; ASrcUnit: GpUnit): GpStatus; stdcall;
  GdipDrawImagePointsRect: function (AGraphics: GpGraphics; AImage: GpImage; APoints: GpPointF; ACount: Integer; ASrcX, ASrcY, ASrcWidth, ASrcHeight: Single; ASrcUnit: GpUNIT; AImageAttributes: GpImageAttributes; ACallback: TGpImageAbort; ACallbackData: Pointer): GpStatus; stdcall;
  GdipDrawImagePointsRectI: function (AGraphics: GpGraphics; AImage: GpImage; APoints: GpPointF; ACount: Integer; ASrcX, ASrcY, ASrcWidth, ASrcHeight: Integer; ASrcUnit: GpUNIT; AImageAttributes: GpImageAttributes; ACallback: TGpImageAbort; ACallbackData: Pointer): GpStatus; stdcall;
  GdipCloneImage: function(AImage: GpImage; out ACloneImage: GpImage): GpStatus; stdcall;
  GdipCreateTexture: function(AImage: GpImage; AWrapMode: TdxGpWrapMode; var ATexture: GpTexture): GpStatus; stdcall;
  GdipCreateTexture2: function(AImage: GpImage; AWrapMode: TdxGpWrapMode; X, Y, AWidth, AHeight: Single; out ATexture: GpTexture): GpStatus; stdcall;
  GdipCreateTexture2I: function(AImage: GpImage; AWrapMode: GpWrapMode; X, Y, AWidth, AHeight: Integer; out ATexture: GpTexture): GpStatus; stdcall;
  GdipCreateTextureIA: function(AImage: GpImage; AImageAttributes: GpImageAttributes; X, Y, AWidth, AHeight: Single; out ATexture: GpTexture): GpStatus; stdcall;
  GdipCreateTextureIAI: function(AImage: GpImage; AImageAttributes: GpImageAttributes; X, Y, AWidth, AHeight: Integer; out ATexture: GpTexture): GpStatus; stdcall;
  GdipGetTextureTransform: function (ABrush: GpTexture; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipResetTextureTransform: function(ABrush: GpTexture): GpStatus; stdcall;
  GdipSetTextureTransform: function(ABrush: GpTexture; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipMultiplyTextureTransform: function(ABrush: GpTexture; AMatrix: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslateTextureTransform: function(ABrush: GpTexture; DX, DY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScaleTextureTransform: function(ABrush: GpTexture; SX, SY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotateTextureTransform: function(ABrush: GpTexture; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipSaveAdd: function(AImage: GpImage; AEncoderParams: Pointer): GpStatus; stdcall;
  GdipSaveAddImage: function(AImage, ANewImage: GpImage; AEncoderParams: Pointer): GpStatus; stdcall;
  GdipSetTextureWrapMode: function(ABrush: GpTexture; AWrapMode: TdxGpWrapMode): GpStatus; stdcall;
  GdipGetTextureWrapMode: function(ABrush: GpTexture; var AWrapMode: TdxGpWrapMode): GpStatus; stdcall;
  GdipGetTextureImage: function(ABrush: GpTexture; out AImage: GpImage): GpStatus; stdcall;

  GdipCreateImageAttributes: function(out AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipCloneImageAttributes: function(AImageAttributes: GpImageAttributes; out ACloneImageAttr: GpImageAttributes): GpStatus; stdcall;
  GdipDisposeImageAttributes: function(AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipSetImageAttributesToIdentity: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType): GpStatus; stdcall;
  GdipResetImageAttributes: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType): GpStatus; stdcall;
  GdipSetImageAttributesColorMatrix: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AColorMatrix: PdxGpColorMatrix; AGrayMatrix: PdxGpColorMatrix; AFlags: TdxGpColorMatrixFlags): GpStatus;  stdcall;
  GdipSetImageAttributesThreshold: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AThreshold: Single): GpStatus; stdcall;
  GdipSetImageAttributesGamma: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AGamma: Single): GpStatus; stdcall;
  GdipSetImageAttributesColorKeys: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AColorLow, AColorHigh: TdxAlphaColor): GpStatus; stdcall;
  GdipSetImageAttributesOutputChannel: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AChannelFlags: TdxGpColorChannelFlags): GpStatus; stdcall;
  GdipSetImageAttributesOutputChannelColorProfile: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AColorProfileFilename: PWCHAR): GpStatus; stdcall;
  GdipSetImageAttributesRemapTable: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool; AMapSize: UINT; AMap: PGpColorMap): GpStatus; stdcall;
  GdipSetImageAttributesWrapMode: function(AImageAttributes: GpImageAttributes; AWrap: TdxGpWrapMode; AColor: TdxAlphaColor; AClamp: Bool): GpStatus; stdcall;
  GdipSetImageAttributesICMMode: function(AImageAttributes: GpImageAttributes; On_: Bool): GpStatus; stdcall;
  GdipGetImageAttributesAdjustedPalette: function(AImageAttributes: GpImageAttributes; AColorPalette: PGpColorPalette; AColorAdjustType: TdxGpColorAdjustType): GpStatus; stdcall;
  GdipSetImageAttributesNoOp: function(AImageAttributes: GpImageAttributes; AType: TdxGpColorAdjustType; AEnableFlag: Bool): GpStatus; stdcall;

  // Paths
  GdipAddPathArc: function(APath: GpPath; X, Y, Width, Height, StartAngle, SweepAngle: Single): GpStatus; stdcall;
  GdipAddPathLine: function(APath: GpPath; X1, Y1, X2, Y2: Single): GpStatus; stdcall;
  GdipAddPathLine2: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathBezier: function(APath: GpPath; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): GpStatus; stdcall;
  GdipAddPathBeziers: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathCurve: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathCurve2: function(APath: GpPath; APoints: GpPointF; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathCurve3: function(APath: GpPath; APoints: GpPointF; ACount, AOffset, ANumberOfSegments: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathClosedCurve: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathClosedCurve2: function(APath: GpPath; APoints: GpPointF; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathRectangle: function(APath: GpPath; X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipAddPathRectangles: function(APath: GpPath; ARects: GpRectF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathEllipse: function(APath: GpPath;  X, Y, AWidth, AHeight: Single): GpStatus; stdcall;
  GdipAddPathPie: function(APath: GpPath; X, Y, AWidth, AHeight, AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipAddPathPolygon: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipAddPathPath: function(APath: GpPath; AAddingPath: GpPath; AConnect: Bool): GpStatus; stdcall;
  GdipAddPathString: function(APath: GpPath; AString: PWCHAR; ALength: Integer; AFamily: GpFontFamily; AStyle: Integer; AEmSize: Single; ALayoutRect: PdxGpRectF; AFormat: GpStringFormat): GpStatus; stdcall;
  GdipAddPathStringI: function(APath: GpPath; AString: PWCHAR; ALength: Integer; AFamily: GpFontFamily; AStyle: Integer; AEmSize: Single; ALayoutRect: PdxGpRect; AFormat: GpStringFormat): GpStatus; stdcall;
  GdipAddPathLineI: function(APath: GpPath; X1, Y1, X2, Y2: Integer): GpStatus; stdcall;
  GdipAddPathLine2I: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipAddPathArcI: function(APath: GpPath; X, Y, AWidth, AHeight: Integer; AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipAddPathBezierI: function(APath: GpPath; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): GpStatus; stdcall;
  GdipAddPathBeziersI: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus;
  GdipAddPathCurveI: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipAddPathCurve2I: function(APath: GpPath; APoints: GpPoint; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathCurve3I: function(APath: GpPath; APoints: GpPoint; ACount: Integer; AOffset: Integer; ANumberOfSegments: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathClosedCurveI: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipAddPathClosedCurve2I: function(APath: GpPath; APoints: GpPoint; ACount: Integer; ATension: Single): GpStatus; stdcall;
  GdipAddPathRectangleI: function(APath: GpPath; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipAddPathRectanglesI: function(APath: GpPath; ARects: GpRect; ACount: Integer): GpStatus; stdcall;
  GdipAddPathEllipseI: function(APath: GpPath; X, Y, AWidth, AHeight: Integer): GpStatus; stdcall;
  GdipAddPathPieI: function(APath: GpPath; X, Y, AWidth, AHeight: Integer; AStartAngle, ASweepAngle: Single): GpStatus; stdcall;
  GdipAddPathPolygonI: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipFlattenPath: function(APath: GpPath; AMatrix: GpMatrix; AFlatness: Single): GpStatus; stdcall;
  GdipWindingModeOutline: function(APath: GpPath; AMatrix: GpMatrix; AFlatness: Single): GpStatus; stdcall;
  GdipWidenPath: function(ANativePath: GpPath; APen: GpPen; AMatrix: GpMatrix; AFlatness: Single): GpStatus; stdcall;
  GdipWarpPath: function(APath: GpPath; AMatrix: GpMatrix; APoints: GpPointF; ACount: Integer; ASrcX, ASrcY, ASrcWidth, ASrcHeight: Single; AWarpMode: TdxGpWarpMode; AFlatness: Single): GpStatus; stdcall;
  GdipTransformPath: function(APath: GpPath; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetPathWorldBounds: function(APath: GpPath; ABounds: GpRectF; AMatrix: GpMatrix; APen: GpPen): GpStatus; stdcall;
  GdipGetPathWorldBoundsI: function(APath: GpPath; ABounds: GpRect; AMatrix: GpMatrix; APen: GpPen): GpStatus; stdcall;
  GdipIsVisiblePathPoint: function(APath: GpPath; X, Y: Single; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsVisiblePathPointI: function(APath: GpPath; X, Y: Integer; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsOutlineVisiblePathPoint: function(APath: GpPath; X, Y: Single; APen: GpPen; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsOutlineVisiblePathPointI: function(APath: GpPath; X, Y: Integer; APen: GpPen; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipCreatePath: function(AFillMode: GpFillMode; out APath: GpPath): GpStatus; stdcall;
  GdipCreatePath2: function(APoint: GpPointF; AValue1: PBYTE; AValue2: Integer; AFillMode: GpFillMode; out APath: GpPath): GpStatus; stdcall;
  GdipCreatePath2I: function(APoint: GpPoint; AValue1: PBYTE; AValue2: Integer; AFillMode: GpFillMode; out APath: GpPath): GpStatus; stdcall;
  GdipClonePath: function(APath: GpPath; out AClonePath: GpPath): GpStatus; stdcall;
  GdipDeletePath: function(APath: GpPath): GpStatus; stdcall;
  GdipResetPath: function(APath: GpPath): GpStatus; stdcall;
  GdipGetPointCount: function(APath: GpPath; out ACount: Integer): GpStatus; stdcall;
  GdipGetPathTypes: function(APath: GpPath; ATypes: PBYTE; ACount: Integer): GpStatus; stdcall;
  GdipGetPathPoints: function(APath: GpPath; APoints: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipGetPathPointsI: function(APath: GpPath; APoints: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipGetPathFillMode: function(APath: GpPath; var AFillMode: GpFillMode): GpStatus; stdcall;
  GdipSetPathFillMode: function(APath: GpPath; FillMode: GpFillMode): GpStatus; stdcall;
  GdipGetPathData: function(APath: GpPath; APathData: Pointer): GpStatus; stdcall;
  GdipDrawPath: function(AGraphics: GpGraphics; APen: GpPen; APath: GpPath): GpStatus; stdcall;
  GdipFillPath: function(AGraphics: GpGraphics; ABrush: GpBrush; APath: GpPath): GpStatus; stdcall;
  GdipStartPathFigure: function(APath: GpPath): GpStatus; stdcall;
  GdipClosePathFigure: function(APath: GpPath): GpStatus; stdcall;
  GdipClosePathFigures: function(APath: GpPath): GpStatus; stdcall;
  GdipSetPathMarker: function(APath: GpPath): GpStatus; stdcall;
  GdipClearPathMarkers: function(APath: GpPath): GpStatus; stdcall;
  GdipReversePath: function(APath: GpPath): GpStatus; stdcall;
  GdipGetPathLastPoint: function(APath: GpPath; ALastPoint: GpPointF): GpStatus; stdcall;

  // PathIterator
  GdipCreatePathIter: function(out AIterator: GpPathIterator; APath: GpPath): GpStatus; stdcall;
  GdipDeletePathIter: function(AIterator: GpPathIterator): GpStatus; stdcall;
  GdipPathIterNextSubpath: function(AIterator: GpPathIterator; var AResultCount: Integer; var AStartIndex: Integer; var AEndIndex: Integer; out AIsClosed: Bool): GpStatus; stdcall;
  GdipPathIterNextSubpathPath: function(AIterator: GpPathIterator; var AResultCount: Integer; APath: GpPath; out AIsClosed: Bool): GpStatus; stdcall;
  GdipPathIterNextPathType: function(AIterator: GpPathIterator; var AResultCount: Integer; APathType: PBYTE; var AStartIndex: Integer; var AEndIndex: Integer): GpStatus; stdcall;
  GdipPathIterNextMarker: function(AIterator: GpPathIterator; var AResultCount: Integer; var AStartIndex: Integer; var AEndIndex: Integer): GpStatus; stdcall;
  GdipPathIterNextMarkerPath: function(AIterator: GpPathIterator; var AResultCount: Integer; APath: GpPath): GpStatus; stdcall;
  GdipPathIterGetCount: function(AIterator: GpPathIterator; out ACount: Integer): GpStatus; stdcall;
  GdipPathIterGetSubpathCount: function(AIterator: GpPathIterator; out ACount: Integer): GpStatus; stdcall;
  GdipPathIterIsValid: function(AIterator: GpPathIterator; out AValid: Bool): GpStatus; stdcall;
  GdipPathIterHasCurve: function(AIterator: GpPathIterator; out AHasCurve: Bool): GpStatus; stdcall;
  GdipPathIterRewind: function(AIterator: GpPathIterator): GpStatus; stdcall;
  GdipPathIterEnumerate: function(AIterator: GpPathIterator; var AResultCount: Integer; APoints: GpPOINTF; ATypes: PBYTE; ACount: Integer): GpStatus; stdcall;
  GdipPathIterCopyData: function(AIterator: GpPathIterator; var AResultCount: Integer; APoints: GpPointF; ATypes: PBYTE; AStartIndex: Integer; AEndIndex: Integer): GpStatus; stdcall;

  // Matrix
  GdipCreateMatrix: function(out AMatrix: GpMatrix): GpStatus; stdcall;
  GdipCreateMatrix2: function(M11, M12, M21, M22, DX, DY: Single; out AMatrix: GpMatrix): GpStatus; stdcall;
  GdipCreateMatrix3: function(ARect: GpRectF; ADstplg: GpPointF; out AMatrix: GpMatrix): GpStatus; stdcall;
  GdipCreateMatrix3I: function(ARect: GpRect; ADstplg: GpPoint; out AMatrix: GpMatrix): GpStatus; stdcall;
  GdipCloneMatrix: function(AMatrix: GpMatrix; out ACloneMatrix: GpMatrix): GpStatus; stdcall;
  GdipDeleteMatrix: function(AMatrix: GpMatrix): GpStatus; stdcall;
  GdipSetMatrixElements: function(AMatrix: GpMatrix; M11, M12, M21, M22, DX, DY: Single): GpStatus; stdcall;
  GdipMultiplyMatrix: function(AMatrix1, AMatrix2: GpMatrix; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipTranslateMatrix: function(AMatrix: GpMatrix; AOffsetX, AOffsetY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipScaleMatrix: function(AMatrix: GpMatrix; AScaleX, AScaleY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipRotateMatrix: function(AMatrix: GpMatrix; AAngle: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipShearMatrix: function(AMatrix: GpMatrix; AShearX, AShearY: Single; AOrder: TdxGpMatrixOrder): GpStatus; stdcall;
  GdipInvertMatrix: function(AMatrix: GpMatrix): GpStatus; stdcall;
  GdipTransformMatrixPoints: function(AMatrix: GpMatrix; APoint: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipTransformMatrixPointsI: function(AMatrix: GpMatrix; APoint: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipVectorTransformMatrixPoints: function(AMatrix: GpMatrix; APoint: GpPointF; ACount: Integer): GpStatus; stdcall;
  GdipVectorTransformMatrixPointsI: function(AMatrix: GpMatrix; APoint: GpPoint; ACount: Integer): GpStatus; stdcall;
  GdipGetMatrixElements: function(AMatrix: GpMatrix; AMatrixOut: PSingle): GpStatus; stdcall;
  GdipIsMatrixInvertible: function(AMatrix: GpMatrix; out AResult: Bool): GpStatus; stdcall;
  GdipIsMatrixIdentity: function(AMatrix: GpMatrix; out AResult: Bool): GpStatus; stdcall;
  GdipIsMatrixEqual: function(AMatrix1, AMatrix2: GpMatrix; out AResult: Bool): GpStatus; stdcall;

  //lcm
  GdipSetInterpolationMode: function(AGraphics: GpGraphics; AInterpolationMode: Integer): GpStatus; stdcall;
  GdipGetInterpolationMode: function(AGraphics: GpGraphics; out AInterpolationMode: Integer): GpStatus; stdcall;
  GdipCreateCachedBitmap: function(ABitmap: GpBitmap; AGraphics: GpGraphics; out ACachedBitmap: GpCachedBitmap): GpStatus; stdcall;
  GdipDeleteCachedBitmap: function(ACachedBitmap: GpCachedBitmap): GpStatus; stdcall;
  GdipDrawCachedBitmap: function(AGraphics: GpGraphics; ACachedBitmap: GpCachedBitmap; X, Y: Integer): GpStatus; stdcall;
  GdipCreateBitmapFromGraphics: function(AWidth, AHeight: Integer; ATarget: GpGraphics; var ABitmap: GpBitmap): GpStatus; stdcall;
  GdipGetImageBounds: function(AImage: GpImage; ASrcRect: GpRectF; var ASrcUnit: GpUNIT): GpStatus; stdcall;
  GdipGetImageGraphicsContext: function(AImage: GpImage; out AGraphics: GpGraphics): GpStatus; stdcall;
  GdipGetImageWidth: function(AImage: GpImage; out AWidth: Integer): GpStatus; stdcall;
  GdipGetImageHeight: function(AImage: GpImage; out AHeight: Integer): GpStatus; stdcall;
  GdipGetImageHorizontalResolution: function(AImage: GpImage; var AResolution: Single): GpStatus; stdcall;
  GdipGetImageVerticalResolution: function(AImage: GpImage; var AResolution: Single): GpStatus; stdcall;
  GdipGetImageFlags: function(AImage: GpImage; var AFlags: UINT): GpStatus; stdcall;
  GdipSetCompositingMode: function(AGraphics: GpGraphics; ACompositingMode: Integer): GpStatus; stdcall;
  GdipGetCompositingMode: function(AGraphics: GpGraphics; out ACompositingMode: Integer): GpStatus; stdcall;
  GdipSetCompositingQuality: function(AGraphics: GpGraphics; ACompositingQuality: Integer): GpStatus; stdcall;
  GdipGetCompositingQuality: function(AGraphics: GpGraphics; out ACompositingQuality: Integer): GpStatus; stdcall;
  GdipSetSmoothingMode: function(AGraphics: GpGraphics; ASmoothingMode: Integer): GpStatus; stdcall;
  GdipGetSmoothingMode: function(AGraphics: GpGraphics; out ASmoothingMode: Integer): GpStatus; stdcall;
  GdipCloneBitmapArea: function(X, Y, AWidth, AHeight: Single; AFormat: TdxGpPixelFormat; ASrcBitmap: GpBitmap; out ADstBitmap: GpBitmap): GpStatus; stdcall;
  GdipCloneBitmapAreaI: function(X, Y, AWidth, AHeight: Integer; AFormat: Cardinal; ASrcBitmap: GpBitmap; out ADstBitmap: GpBitmap): GpStatus; stdcall;
  GdipSetRenderingOrigin: function(AGraphics: GpGraphics; X, Y: Integer): GpStatus; stdcall;
  GdipGetRenderingOrigin: function(AGraphics: GpGraphics; var X: Integer; var Y: Integer): GpStatus; stdcall;
  GdipSetPixelOffsetMode: function(AGraphics: GpGraphics; APixelOffsetMode: TdxGpPixelOffsetMode): GpStatus; stdcall;
  GdipGetPixelOffsetMode: function(AGraphics: GpGraphics; var APixelOffsetMode: TdxGpPixelOffsetMode): GpStatus; stdcall;
  GdipSetTextRenderingHint: function(AGraphics: GpGraphics; AMode: TdxGpTextRenderingHint): GpStatus; stdcall;
  GdipGetTextRenderingHint: function(AGraphics: GpGraphics; var AMode: TdxGpTextRenderingHint): GpStatus; stdcall;
  GdipSetTextContrast: function(AGraphics: GpGraphics; AContrast: Integer): GpStatus; stdcall;
  GdipGetTextContrast: function(AGraphics: GpGraphics; var AContrast: UINT): GpStatus; stdcall;

  //clipping
  GdipCreateRegion: function(out ARegion: GpRegion): GpStatus; stdcall;
  GdipCreateRegionPath: function(APath: GpPath; out ARegion: GpRegion): GpStatus; stdcall;
  GdipCreateRegionRect: function(ARect: GpRectF; out ARegion: GpRegion): GpStatus; stdcall;
  GdipCreateRegionRectI: function(ARect: GpRect; out ARegion: GpRegion): GpStatus; stdcall;
  GdipCreateRegionRgnData: function(ARegionData: PBYTE; ASize: Integer; out ARegion: GpRegion): GpStatus; stdcall;
  GdipCreateRegionHrgn: function(AHRgn: HRGN; out ARegion: GpRegion): GpStatus; stdcall;
  GdipCloneRegion: function(ARegion: GpRegion; out ACloneRegion: GpRegion): GpStatus; stdcall;
  GdipDeleteRegion: function(ARegion: GpRegion): GpStatus; stdcall;
  GdipSetInfinite: function(ARegion: GpRegion): GpStatus; stdcall;
  GdipSetEmpty: function(ARegion: GpRegion): GpStatus; stdcall;
  GdipCombineRegionRect: function(ARegion: GpRegion; ARect: GpRectF; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipCombineRegionRectI: function(ARegion: GpRegion; ARect: GpRect; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipCombineRegionPath: function(ARegion: GpRegion; APath: GpPath; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipCombineRegionRegion: function(ARegion1, ARegion2: GpRegion; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipTranslateRegion: function(ARegion: GpRegion; DX, DY: Single): GpStatus; stdcall;
  GdipTranslateRegionI: function(ARegion: GpRegion; DX, DY: Integer): GpStatus; stdcall;
  GdipTransformRegion: function(ARegion: GpRegion; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetRegionBounds: function(ARegion: GpRegion; AGraphics: GpGraphics; ARect: GpRectF): GpStatus; stdcall;
  GdipGetRegionBoundsI: function(ARegion: GpRegion; AGraphics: GpGraphics; ARect: GpRect): GpStatus; stdcall;
  GdipGetRegionHRgn: function(ARegion: GpRegion; AGraphics: GpGraphics; out AHRgn: HRGN): GpStatus; stdcall;
  GdipIsEmptyRegion: function(ARegion: GpRegion; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsInfiniteRegion: function(ARegion: GpRegion; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsEqualRegion: function(ARegion1, ARegion2: GpRegion; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipGetRegionDataSize: function(ARegion: GpRegion; out ABufferSize: UINT): GpStatus; stdcall;
  GdipGetRegionData: function(ARegion: GpRegion; ABuffer: PBYTE; ABufferSize: UINT; ASizeFilled: PUINT): GpStatus; stdcall;
  GdipIsVisibleRegionPoint: function(ARegion: GpRegion; X, Y: Single; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsVisibleRegionPointI: function(ARegion: GpRegion; X, Y: Integer; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsVisibleRegionRect: function(ARegion: GpRegion; X, Y, AWidth, AHeight: Single; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipIsVisibleRegionRectI: function(ARegion: GpRegion; X, Y, AWidth, AHeight: Integer; AGraphics: GpGraphics; out AResult: Bool): GpStatus; stdcall;
  GdipGetRegionScansCount: function(ARegion: GpRegion; out ACount: UINT; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetRegionScans: function(ARegion: GpRegion; ARects: GpRectF; out ACount: Integer; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetRegionScansI: function(ARegion: GpRegion; ARects: GpRect; out ACount: Integer; AMatrix: GpMatrix): GpStatus; stdcall;
  GdipGetClip: function(AGraphics: GpGraphics; ARegion: GpRegion): GpStatus; stdcall;
  GdipResetClip: function(AGraphics: GpGraphics): GpStatus; stdcall;
  GdipSetClipGraphics: function(AGraphics: GpGraphics; ASrcGraphics: GpGraphics; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipSetClipRegion: function(AGraphics: GpGraphics; ARegion: GpRegion; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipSetClipRect: function(AGraphics: GpGraphics; X, Y, AWidth, AHeight: Single; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipSetClipRectI: function(AGraphics: GpGraphics; X, Y, AWidth, AHeight: Integer; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipSetClipPath: function(AGraphics: GpGraphics; APath: GpPath; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipSetClipHrgn: function(AGraphics: GpGraphics; AHRgn: HRGN; ACombineMode: TGpCombineMode): GpStatus; stdcall;
  GdipTranslateClip: function(AGraphics: GpGraphics; DX, DY: Single): GpStatus; stdcall;
  GdipTranslateClipI: function(AGraphics: GpGraphics; DX, DY: Integer): GpStatus; stdcall;
  GdipGetClipBounds: function(AGraphics: GpGraphics; ARect: GpRectF): GpStatus; stdcall;
  GdipGetClipBoundsI: function(AGraphics: GpGraphics; ARect: GpRect): GpStatus; stdcall;
  GdipIsClipEmpty: function(AGraphics: GpGraphics; AResult: PBool): GpStatus; stdcall;
  GdipGetVisibleClipBounds: function(AGraphics: GpGraphics; ARect: GpRectF): GpStatus; stdcall;
  GdipGetVisibleClipBoundsI: function(AGraphics: GpGraphics; ARect: GpRect): GpStatus; stdcall;
  GdipIsVisibleClipEmpty: function(AGraphics: GpGraphics; var AResult: Bool): GpStatus; stdcall;
  GdipIsVisiblePoint: function(AGraphics: GpGraphics; X, Y: Single; var AResult: Bool): GpStatus; stdcall;
  GdipIsVisiblePointI: function(AGraphics: GpGraphics; X, Y: Integer; var AResult: Bool): GpStatus; stdcall;
  GdipSaveGraphics: function(AGraphics: GpGraphics; var AState: TdxGpGraphicsState): GpStatus; stdcall;
  GdipRestoreGraphics: function(AGraphics: GpGraphics; AState: TdxGpGraphicsState): GpStatus; stdcall;
  GdipBeginContainer: function(AGraphics: GpGraphics; ADstRect, ASrcRect: GpRectF; AUnit: GpUNIT; var AState: TdxGpGraphicsContainer): GpStatus; stdcall;
  GdipBeginContainerI: function(AGraphics: GpGraphics; ADstRect, ASrcRect: GpRect; AUnit: GpUNIT; var AState: TdxGpGraphicsContainer): GpStatus; stdcall;
  GdipBeginContainer2: function(AGraphics: GpGraphics; var AState: TdxGpGraphicsContainer): GpStatus; stdcall;
  GdipEndContainer: function(AGraphics: GpGraphics; AState: TdxGpGraphicsContainer): GpStatus; stdcall;

  GdipEnumerateMetafileDestPoint: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoint: PdxGpPointF; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileDestPointI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoint: PdxGpPoint; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileDestRect: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestRect: GpRectF; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileDestRectI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestRect: GpRect; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileDestPoints: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoints: GpPointF; ACount: Integer; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileDestPointsI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoints: GpPoint; ACount: Integer; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestPoint: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoint: GpPointF; ASrcRect: GpRectF; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestPointI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoint: GpPoint; ASrcRect: GpRect; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestRect: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestRect, ASrcRect: GpRectF; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestRectI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestRect, ASrcRect: GpRect; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestPoints: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoints: GpPointF; ACount: Integer; ASrcRect: GpRectF; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipEnumerateMetafileSrcRectDestPointsI: function(AGraphics: GpGraphics; AMetafile: GpMetafile; ADestPoints: GpPoint; ACount: Integer; ASrcRect: GpRect; ASrcUnit: GpUnit; ACallback: GpEnumerateMetafileProc; ACallbackData: Pointer; AImageAttributes: GpImageAttributes): GpStatus; stdcall;
  GdipPlayMetafileRecord: function(AMetafile: GpMetafile; ARecordType: Integer; AFlags: UINT; ADataSize: UINT; AData: PBYTE): GpStatus; stdcall;
  GdipGetMetafileHeaderFromWmf: function(AHWmf: HMETAFILE; AWmfPlaceableFileHeader: PGpWmfPlaceableFileHeader; AHeader: Pointer): GpStatus; stdcall;
  GdipGetMetafileHeaderFromEmf: function(AHEmf: HENHMETAFILE; AHeader: Pointer): GpStatus; stdcall;
  GdipGetMetafileHeaderFromFile: function(AFileName: PWCHAR; AHeader: Pointer): GpStatus; stdcall;
  GdipGetMetafileHeaderFromStream: function(AStream: IStream; AHeader: Pointer): GpStatus; stdcall;
  GdipGetMetafileHeaderFromMetafile: function(AMetafile: GpMetafile; AHeader: Pointer): GpStatus; stdcall;
  GdipGetHemfFromMetafile: function(AMetafile: GpMetafile; var hEmf: HENHMETAFILE): GpStatus; stdcall;
  GdipCreateStreamOnFile: function(AFileName: PWCHAR; AAccess: UINT; out AStream: IStream): GpStatus; stdcall;
  GdipCreateMetafileFromWmf: function(AHWmf: HMETAFILE; ADeleteWmf: Bool; AWmfPlaceableFileHeader: PGpWmfPlaceableFileHeader; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipCreateMetafileFromEmf: function(AHEmf: HENHMETAFILE; ADeleteEmf: Bool; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipCreateMetafileFromFile: function(AFileName: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipCreateMetafileFromWmfFile: function(AFileName: PWCHAR; AWmfPlaceableFileHeader: PGpWmfPlaceableFileHeader; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipCreateMetafileFromStream: function(AStream: IStream; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafile: function(AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRectF; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafileI: function(AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRect; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafileFileName: function(AFileName: PWCHAR; AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRectF; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafileFileNameI: function(AFileName: PWCHAR; AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRect; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafileStream: function(AStream: IStream; AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRectF; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipRecordMetafileStreamI: function(AStream: IStream; AReferenceDC: HDC; AType: TdxGpEmfType; AFrameRect: GpRect; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PWCHAR; out AMetafile: GpMetafile): GpStatus; stdcall;
  GdipSetMetafileDownLevelRasterizationLimit: function(AMetafile: GpMetafile; AMetafileRasterizationLimitDPI: UINT): GpStatus; stdcall;
  GdipGetMetafileDownLevelRasterizationLimit: function(AMetafile: GpMetafile; var AMetafileRasterizationLimitDPI: UINT): GpStatus; stdcall;
  GdipComment: function(AGraphics: GpGraphics; ASizeData: UINT; AData: PBYTE): GpStatus; stdcall;

  GdipIsVisibleRect: function(AGraphics: GpGraphics; X, Y, AWidth, AHeight: Single; var AResult: LongBool): GpStatus; stdcall;
  GdipIsVisibleRectI: function(AGraphics: GpGraphics; X, Y, AWidth, AHeight: Integer; var AResult: LongBool): GpStatus; stdcall;

  GdipCreateFontFamilyFromName: function(AName: PWCHAR; AFontCollection: GpFontCollection; out AFontFamily: GpFontFamily): GpStatus; stdcall;
  GdipDeleteFontFamily: function(AFontFamily: GpFontFamily): GpStatus; stdcall;
  GdipCloneFontFamily: function(AFontFamily: GpFontFamily; out AClonedFontFamily: GpFontFamily): GpStatus; stdcall;
  GdipGetGenericFontFamilySansSerif: function(out ANativeFamily: GpFontFamily): GpStatus; stdcall;
  GdipGetGenericFontFamilySerif: function(out ANativeFamily: GpFontFamily): GpStatus; stdcall;
  GdipGetGenericFontFamilyMonospace: function(out ANativeFamily: GpFontFamily): GpStatus; stdcall;
  GdipGetFamilyName: function(AFamily: GpFontFamily; AName: PWideChar; ALanguage: LANGID): GpStatus; stdcall;
  GdipIsStyleAvailable: function(AFamily: GpFontFamily; AStyle: Integer; var AIsStyleAvailable: Bool): GpStatus; stdcall;
  GdipFontCollectionEnumerable: function(AFontCollection: GpFontCollection; AGraphics: GpGraphics; var ANumFound: Integer): GpStatus; stdcall;
  GdipFontCollectionEnumerate: function(AFontCollection: GpFontCollection; ANumSought: Integer; AGpFamilies: array of GpFontFamily; var ANumFound: Integer; AGraphics: GpGraphics): GpStatus; stdcall;
  GdipGetEmHeight: function(AFamily: GpFontFamily; AStyle: Integer; out AEmHeight: WORD): GpStatus; stdcall;
  GdipGetCellAscent: function(AFamily: GpFontFamily; AStyle: Integer; var ACellAscent: WORD): GpStatus; stdcall;
  GdipGetCellDescent: function(AFamily: GpFontFamily; AStyle: Integer; var ACellDescent: WORD): GpStatus; stdcall;
  GdipGetLineSpacing: function(AFamily: GpFontFamily; AStyle: Integer; var ALineSpacing: WORD): GpStatus; stdcall;

  GdipCreateFontFromDC: function(DC: HDC; out AFont: GpFont): GpStatus; stdcall;
  GdipCreateFontFromLogfontA: function(DC: HDC; ALogFont: PLogFontA; out AFont: GpFont): GpStatus; stdcall;
  GdipCreateFontFromLogfontW: function(DC: HDC; ALogFont: PLogFontW; out AFont: GpFont): GpStatus; stdcall;
  GdipCreateFontFromLogfont: function(DC: HDC; ALogFont: PLogFont; out AFont: GpFont): GpStatus; stdcall;
  GdipCreateFont: function(AFontFamily: GpFontFamily; emSize: Single; AStyle: Integer; AUnit: Integer; out AFont: GpFont): GpStatus; stdcall;
  GdipCloneFont: function(AFont: GpFont; out ACloneFont: GpFont): GpStatus; stdcall;
  GdipDeleteFont: function(AFont: GpFont): GpStatus; stdcall;
  GdipGetFamily: function(AFont: GpFont; out AFamily: GpFontFamily): GpStatus; stdcall;
  GdipGetFontStyle: function(AFont: GpFont; var AStyle: Integer): GpStatus; stdcall;
  GdipGetFontSize: function(AFont: GpFont; var ASize: Single): GpStatus; stdcall;
  GdipGetFontUnit: function(AFont: GpFont; var AUnit: GpUnit): GpStatus; stdcall;
  GdipGetFontHeight: function(AFont: GpFont; AGraphics: GpGraphics; var AHeight: Single): GpStatus; stdcall;
  GdipGetFontHeightGivenDPI: function(AFont: GpFont; DPI: Single; var AHeight: Single): GpStatus; stdcall;
  GdipGetLogFontA: function(AFont: GpFont; AGraphics: GpGraphics; var ALogFontA: LogFontA): GpStatus; stdcall;
  GdipGetLogFontW: function(AFont: GpFont; AGraphics: GpGraphics; var ALogfontW: LogFontW): GpStatus; stdcall;
  GdipNewInstalledFontCollection: function(out AFontCollection: GpFontCollection): GpStatus; stdcall;
  GdipNewPrivateFontCollection: function(out AFontCollection: GpFontCollection): GpStatus; stdcall;
  GdipDeletePrivateFontCollection: function(out AFontCollection: GpFontCollection): GpStatus; stdcall;
  GdipGetFontCollectionFamilyCount: function(AFontCollection: GpFontCollection; var ANumFound: Integer): GpStatus; stdcall;
  GdipGetFontCollectionFamilyList: function(AFontCollection: GpFontCollection; ANumSought: Integer; AFamilies: GpFontFamily; var ANumFound: Integer): GpStatus; stdcall;
  GdipPrivateAddFontFile: function(AFontCollection: GpFontCollection; AFileName: PWCHAR): GpStatus; stdcall;
  GdipPrivateAddMemoryFont: function(AFontCollection: GpFontCollection; AMemory: Pointer; ALength: Integer): GpStatus; stdcall;

  GdipDrawString: function(AGraphics: GpGraphics; AString: PWCHAR; ALength: Integer; AFont: GpFont; ALayoutRect: GpRectF; AStringFormat: GpStringFormat; ABrush: GpBrush): GpStatus; stdcall;
  GdipMeasureString: function(AGraphics: GpGraphics; AString: PWCHAR; ALength: Integer; AFont: GpFont; ALayoutRect: GpRectF; AStringFormat: GpStringFormat; ABoundingBox: GpRectF; var ACodePointsFitted: Integer; var LinesFilled: Integer): GpStatus; stdcall;
  GdipMeasureCharacterRanges: function(AGraphics: GpGraphics; AString: PWCHAR; ALength: Integer; AFont: GpFont; ALayoutRect: GpRectF; AStringFormat: GpStringFormat; ARegionCount: Integer; const ARegions: GpRegion): GpStatus; stdcall;
  GdipDrawDriverString: function(AGraphics: GpGraphics; const AText: PWCHAR; ALength: Integer; const AFont: GpFont; const ABrush: GpBrush; const APositions: GpPointF; AFlags: Integer; const AMatrix: GpMatrix): GpStatus; stdcall;
  GdipMeasureDriverString: function(AGraphics: GpGraphics; AText: PUINT16; ALength: Integer; AFont: GpFont; APositions: GpPointF; AFlags: Integer; AMatrix: GpMatrix; ABoundingBox: GpRectF): GpStatus; stdcall;

  GdipCreateStringFormat: function(AFormatAttributes: Integer; ALanguage: LANGID; out AFormat: GpStringFormat): GpStatus; stdcall;
  GdipStringFormatGetGenericDefault: function(out AFormat: GpStringFormat): GpStatus; stdcall;
  GdipStringFormatGetGenericTypographic: function(out AFormat: GpStringFormat): GpStatus; stdcall;
  GdipDeleteStringFormat: function(AFormat: GpStringFormat): GpStatus; stdcall;
  GdipCloneStringFormat: function(AFormat: GpStringFormat; out ANewFormat: GpStringFormat): GpStatus; stdcall;
  GdipSetStringFormatFlags: function(AFormat: GpStringFormat; AFlags: Integer): GpStatus; stdcall;
  GdipGetStringFormatFlags: function(AFormat: GpStringFormat; out AFlags: Integer): GpStatus; stdcall;
  GdipSetStringFormatAlign: function(AFormat: GpStringFormat; AAlign: TdxGpStringAlignment): GpStatus; stdcall;
  GdipGetStringFormatAlign: function(AFormat: GpStringFormat; out AAlign: TdxGpStringAlignment): GpStatus; stdcall;
  GdipSetStringFormatLineAlign: function(AFormat: GpStringFormat; AAlign: TdxGpStringAlignment): GpStatus; stdcall;
  GdipGetStringFormatLineAlign: function(AFormat: GpStringFormat; out AAlign: TdxGpStringAlignment): GpStatus; stdcall;
  GdipSetStringFormatTrimming: function(AFormat: GpStringFormat; ATrimming: TdxGpStringTrimming): GpStatus; stdcall;
  GdipGetStringFormatTrimming: function(AFormat: GpStringFormat; out ATrimming: TdxGpStringTrimming): GpStatus; stdcall;
  GdipSetStringFormatHotkeyPrefix: function(AFormat: GpStringFormat; AHotkeyPrefix: Integer): GpStatus; stdcall;
  GdipGetStringFormatHotkeyPrefix: function(AFormat: GpStringFormat; out AHotkeyPrefix: Integer): GpStatus; stdcall;
  GdipSetStringFormatTabStops: function(AFormat: GpStringFormat; AFirstTabOffset: Single; ACount: Integer; ATabStops: PSingle): GpStatus; stdcall;
  GdipGetStringFormatTabStops: function(AFormat: GpStringFormat; ACount: Integer; AFirstTabOffset: PSingle; ATabStops: PSingle): GpStatus; stdcall;
  GdipGetStringFormatTabStopCount: function(AFormat: GpStringFormat; out ACount: Integer): GpStatus; stdcall;
  GdipSetStringFormatDigitSubstitution: function(AFormat: GpStringFormat; ALanguage: LANGID; ASubstitute: TdxGpStringDigitSubstitute): GpStatus; stdcall;
  GdipGetStringFormatDigitSubstitution: function(AFormat: GpStringFormat; ALanguage: PUINT; ASubstitute: PdxGpStringDigitSubstitute): GpStatus; stdcall;
  GdipGetStringFormatMeasurableCharacterRangeCount: function(AFormat: GpStringFormat; out ACount: Integer): GpStatus; stdcall;
  GdipSetStringFormatMeasurableCharacterRanges: function(AFormat: GpStringFormat; ARangeCount: Integer; ARanges: PdxGpCharacterRange): GpStatus; stdcall;

  GdipEmfToWmfBits: function(AHemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE; iMapMode: Integer; eFlags: Integer): UINT; stdcall;

var
  dxGpDefaultColorMatrix: TdxGpColorMatrix = (
    (1.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0)
  );

function MakePoint(X, Y: Integer): TdxGpPoint; overload;
function MakePoint(X, Y: Single): TdxGpPointF; overload;
function MakeRect(X, Y, AWidth, AHeight: Integer): TdxGpRect; overload;
function MakeRect(const ALocation: TdxGpPoint; const ASize: TdxGpSize): TdxGpRect; overload;
function MakeRect(const ARect: TRect): TdxGpRect; overload;
function MakeRect(X, Y, AWidth, AHeight: Single): TdxGpRectF; overload;
function MakeRect(const ALocationF: TdxGpPointF; const ASizeF: TdxGpSizeF): TdxGpRectF; overload;
function MakeSize(AWidth, AHeight: Single): TdxGpSizeF; overload;
function MakeSize(AWidth, AHeight: Integer): TdxGpSize; overload;

function dxGpMakeRectF(const ARect: TRect): TdxGpRectF; overload;
function dxGpMakeRectF(const ARect: TdxGpRect): TdxGpRectF; overload;

function dxGpRectIsEqual(const R1, R2: TdxGpRect): Boolean; overload;
function dxGpRectIsEqual(const R1, R2: TdxGpRectF): Boolean; overload

// codecs
procedure CheckImageCodecs;
procedure CheckPngCodec;
function GetEncoderID(const CodecName: string; out CodecID: TGUID): GpStatus;
function GetDecoderID(const CodecName: string; out DecoderID: TGUID): GpStatus;
function IsCodecIDValid(const CodecID: TGUID): Boolean;

// check errors
procedure GdipCheck(AStatus: Boolean); overload;
procedure GdipCheck(AStatus: GpStatus); overload;

function dxGpCreateFont(AFont: TFont): GpFont;
function dxGpCreateBitmap(AWidth, AHeight: Integer): GpBitmap; overload;
function dxGpCreateBitmap(const ASize: TSize): GpBitmap; overload;
function dxGpCreateBitmap(const R: TRect): GpBitmap; overload;
// CLR
function CheckGdiPlus(AHaltOnError: Boolean = False): Boolean;

resourcestring
  scxGdipInvalidOperation = 'Invalid operation in GDI+ (Code: %d)';
  scxGdipInvalidOperationWin32Error = 'Invalid operation in GDI+ (Win32 error code: %d)';

implementation

const
  GDIPlusLibraryName = 'gdiplus.dll';

var
  FGDIPlusLibrary: HMODULE;
  FGDIPlusToken: TdxNativeInt;
  FGdiPlusHook: TGdiplusStartupOutput;
  FGDIPresent: Boolean;

function dxGpCreateFont(AFont: TFont): GpFont;
const
  DefaultTrueTypeFont: PChar = 'Tahoma';
var
  ADC: HDC;
  AError: TdxGpStatus;
  ALogFont: TLogFont;
begin
  ADC := GetDC(0);
  try
    dxGetFontData(AFont.Handle, ALogFont);

    AError := GdipCreateFontFromLogfont(ADC, @ALogFont, Result);
    if AError = NotTrueTypeFont then
    begin
      StrLCopy(@ALogFont.lfFaceName[0], DefaultTrueTypeFont, Length(ALogFont.lfFaceName));
      AError := GdipCreateFontFromLogfont(ADC, @ALogFont, Result);
    end;

    GdipCheck(AError);
  finally
    ReleaseDC(0, ADC);
  end;
end;

function dxGpCreateBitmap(AWidth, AHeight: Integer): GpBitmap;
begin
  GdipCheck(GdipCreateBitmapFromScan0(AWidth, AHeight, 0, PixelFormat32bppPARGB, nil, Result));
end;

function dxGpCreateBitmap(const ASize: TSize): GpBitmap;
begin
  Result := dxGpCreateBitmap(ASize.cx, ASize.cy);
end;

function dxGpCreateBitmap(const R: TRect): GpBitmap;
begin
  Result := dxGpCreateBitmap(R.Right - R.Left, R.Bottom - R.Top);
end;


{ GDI+ loading }

procedure GdiPlusLoad;
const
  DefaultStartup: GdiplusStartupInput =
    (GdiplusVersion: 1;
     DebugEventCallback: nil;
     SuppressBackgroundThread: True;
     SuppressExternalCodecs: False);

  function LoadGdiPlusMethod(const ProcName: string): Pointer;
  begin
    Result := nil;
    if FGDIPresent then
      Result := GetProcAddress(FGDIPlusLibrary, PChar(ProcName));
    FGDIPresent := Result <> nil;
  end;

begin
  if FGDIPresent then
    Exit;
  FGDIPlusLibrary := SafeLoadLibrary(GDIPlusLibraryName);
  FGDIPresent := FGDIPlusLibrary <> 0;
  if FGDIPresent then
  begin
    // GDI+ Memory management methods loading
    GdipAlloc := LoadGdiPlusMethod('GdipAlloc');
    GdipFree := LoadGdiPlusMethod('GdipFree');
    // GDI+ initialization/finalization methods loading
    GdiplusStartup := LoadGdiPlusMethod('GdiplusStartup');
    GdiplusShutdown := LoadGdiPlusMethod('GdiplusShutdown');
    // GDI+ Brush methods loading
    GdipCloneBrush := LoadGdiPlusMethod('GdipCloneBrush');
    GdipDeleteBrush := LoadGdiPlusMethod('GdipDeleteBrush');
    GdipGetBrushType := LoadGdiPlusMethod('GdipGetBrushType');
    // GDI+ Solid Brush methods loading
    GdipCreateSolidFill := LoadGdiPlusMethod('GdipCreateSolidFill');
    GdipSetSolidFillColor := LoadGdiPlusMethod('GdipSetSolidFillColor');
    GdipGetSolidFillColor := LoadGdiPlusMethod('GdipGetSolidFillColor');
    // GDI+ Gradient Brush methods loading
    GdipCreatePathGradient := LoadGdiPlusMethod('GdipCreatePathGradient');
    GdipCreatePathGradientI := LoadGdiPlusMethod('GdipCreatePathGradientI');
    GdipCreatePathGradientFromPath := LoadGdiPlusMethod('GdipCreatePathGradientFromPath');
    GdipGetPathGradientCenterColor := LoadGdiPlusMethod('GdipGetPathGradientCenterColor');
    GdipSetPathGradientCenterColor := LoadGdiPlusMethod('GdipSetPathGradientCenterColor');
    GdipGetPathGradientSurroundColorsWithCount := LoadGdiPlusMethod('GdipGetPathGradientSurroundColorsWithCount');
    GdipSetPathGradientSurroundColorsWithCount := LoadGdiPlusMethod('GdipSetPathGradientSurroundColorsWithCount');
    GdipGetPathGradientPath := LoadGdiPlusMethod('GdipGetPathGradientPath');
    GdipSetPathGradientPath := LoadGdiPlusMethod('GdipSetPathGradientPath');
    GdipGetPathGradientCenterPoint := LoadGdiPlusMethod('GdipGetPathGradientCenterPoint');
    GdipGetPathGradientCenterPointI := LoadGdiPlusMethod('GdipGetPathGradientCenterPointI');
    GdipSetPathGradientCenterPoint := LoadGdiPlusMethod('GdipSetPathGradientCenterPoint');
    GdipSetPathGradientCenterPointI := LoadGdiPlusMethod('GdipSetPathGradientCenterPointI');
    GdipGetPathGradientRect := LoadGdiPlusMethod('GdipGetPathGradientRect');
    GdipGetPathGradientRectI := LoadGdiPlusMethod('GdipGetPathGradientRectI');
    GdipGetPathGradientPointCount := LoadGdiPlusMethod('GdipGetPathGradientPointCount');
    GdipGetPathGradientSurroundColorCount := LoadGdiPlusMethod('GdipGetPathGradientSurroundColorCount');
    GdipSetPathGradientGammaCorrection := LoadGdiPlusMethod('GdipSetPathGradientGammaCorrection');
    GdipGetPathGradientGammaCorrection := LoadGdiPlusMethod('GdipGetPathGradientGammaCorrection');
    GdipGetPathGradientBlendCount := LoadGdiPlusMethod('GdipGetPathGradientBlendCount');
    GdipGetPathGradientBlend := LoadGdiPlusMethod('GdipGetPathGradientBlend');
    GdipSetPathGradientBlend := LoadGdiPlusMethod('GdipSetPathGradientBlend');
    GdipGetPathGradientPresetBlendCount := LoadGdiPlusMethod('GdipGetPathGradientPresetBlendCount');
    GdipGetPathGradientPresetBlend := LoadGdiPlusMethod('GdipGetPathGradientPresetBlend');
    GdipSetPathGradientPresetBlend := LoadGdiPlusMethod('GdipSetPathGradientPresetBlend');
    GdipSetPathGradientSigmaBlend := LoadGdiPlusMethod('GdipSetPathGradientSigmaBlend');
    GdipSetPathGradientLinearBlend := LoadGdiPlusMethod('GdipSetPathGradientLinearBlend');
    GdipGetPathGradientWrapMode := LoadGdiPlusMethod('GdipGetPathGradientWrapMode');
    GdipSetPathGradientWrapMode := LoadGdiPlusMethod('GdipSetPathGradientWrapMode');
    GdipGetPathGradientTransform := LoadGdiPlusMethod('GdipGetPathGradientTransform');
    GdipSetPathGradientTransform := LoadGdiPlusMethod('GdipSetPathGradientTransform');
    GdipResetPathGradientTransform := LoadGdiPlusMethod('GdipResetPathGradientTransform');
    GdipMultiplyPathGradientTransform := LoadGdiPlusMethod('GdipMultiplyPathGradientTransform');
    GdipTranslatePathGradientTransform := LoadGdiPlusMethod('GdipTranslatePathGradientTransform');
    GdipScalePathGradientTransform := LoadGdiPlusMethod('GdipScalePathGradientTransform');
    GdipRotatePathGradientTransform := LoadGdiPlusMethod('GdipRotatePathGradientTransform');
    GdipGetPathGradientFocusScales := LoadGdiPlusMethod('GdipGetPathGradientFocusScales');
    GdipSetPathGradientFocusScales := LoadGdiPlusMethod('GdipSetPathGradientFocusScales');
    GdipCreateLineBrush := LoadGdiPlusMethod('GdipCreateLineBrush');
    GdipCreateLineBrushI := LoadGdiPlusMethod('GdipCreateLineBrushI');
    GdipCreateLineBrushFromRect := LoadGdiPlusMethod('GdipCreateLineBrushFromRect');
    GdipCreateLineBrushFromRectI := LoadGdiPlusMethod('GdipCreateLineBrushFromRectI');
    GdipCreateLineBrushFromRectWithAngle := LoadGdiPlusMethod('GdipCreateLineBrushFromRectWithAngle');
    GdipCreateLineBrushFromRectWithAngleI := LoadGdiPlusMethod('GdipCreateLineBrushFromRectWithAngleI');
    GdipGetLineRect := LoadGdiPlusMethod('GdipGetLineRect');
    GdipGetLineRectI := LoadGdiPlusMethod('GdipGetLineRectI');
    GdipSetLineColors := LoadGdiPlusMethod('GdipSetLineColors');
    GdipGetLineColors := LoadGdiPlusMethod('GdipGetLineColors');
    GdipSetLineGammaCorrection := LoadGdiPlusMethod('GdipSetLineGammaCorrection');
    GdipGetLineGammaCorrection := LoadGdiPlusMethod('GdipGetLineGammaCorrection');
    GdipGetLineBlendCount := LoadGdiPlusMethod('GdipGetLineBlendCount');
    GdipGetLineBlend := LoadGdiPlusMethod('GdipGetLineBlend');
    GdipSetLineBlend := LoadGdiPlusMethod('GdipSetLineBlend');
    GdipGetLinePresetBlendCount := LoadGdiPlusMethod('GdipGetLinePresetBlendCount');
    GdipGetLinePresetBlend := LoadGdiPlusMethod('GdipGetLinePresetBlend');
    GdipSetLinePresetBlend := LoadGdiPlusMethod('GdipSetLinePresetBlend');
    GdipSetLineSigmaBlend := LoadGdiPlusMethod('GdipSetLineSigmaBlend');
    GdipSetLineLinearBlend := LoadGdiPlusMethod('GdipSetLineLinearBlend');
    GdipSetLineWrapMode := LoadGdiPlusMethod('GdipSetLineWrapMode');
    GdipGetLineWrapMode := LoadGdiPlusMethod('GdipGetLineWrapMode');
    GdipGetLineTransform := LoadGdiPlusMethod('GdipGetLineTransform');
    GdipSetLineTransform := LoadGdiPlusMethod('GdipSetLineTransform');
    GdipResetLineTransform := LoadGdiPlusMethod('GdipResetLineTransform');
    GdipMultiplyLineTransform := LoadGdiPlusMethod('GdipMultiplyLineTransform');
    GdipTranslateLineTransform := LoadGdiPlusMethod('GdipTranslateLineTransform');
    GdipScaleLineTransform := LoadGdiPlusMethod('GdipScaleLineTransform');
    GdipRotateLineTransform := LoadGdiPlusMethod('GdipRotateLineTransform');
    // GDI+ Hatch Brush methods loading
    GdipCreateHatchBrush := LoadGdiPlusMethod('GdipCreateHatchBrush');
    GdipGetHatchStyle := LoadGdiPlusMethod('GdipGetHatchStyle');
    GdipGetHatchForegroundColor := LoadGdiPlusMethod('GdipGetHatchForegroundColor');
    GdipGetHatchBackgroundColor := LoadGdiPlusMethod('GdipGetHatchBackgroundColor');
    // GDI+ Pen methods loading
    GdipCreatePen1 := LoadGdiPlusMethod('GdipCreatePen1');
    GdipCreatePen2 := LoadGdiPlusMethod('GdipCreatePen2');
    GdipClonePen := LoadGdiPlusMethod('GdipClonePen');
    GdipDeletePen := LoadGdiPlusMethod('GdipDeletePen');
    GdipGetPenFillType := LoadGdiPlusMethod('GdipGetPenFillType');
    GdipSetPenBrushFill := LoadGdiPlusMethod('GdipSetPenBrushFill');
    GdipGetPenBrushFill := LoadGdiPlusMethod('GdipGetPenBrushFill');
    GdipSetPenColor := LoadGdiPlusMethod('GdipSetPenColor');
    GdipGetPenColor := LoadGdiPlusMethod('GdipGetPenColor');
    GdipSetPenMode := LoadGdiPlusMethod('GdipSetPenMode');
    GdipGetPenMode := LoadGdiPlusMethod('GdipGetPenMode');
    GdipSetPenUnit := LoadGdiPlusMethod('GdipSetPenUnit');
    GdipGetPenUnit := LoadGdiPlusMethod('GdipGetPenUnit');
    GdipSetPenWidth := LoadGdiPlusMethod('GdipSetPenWidth');
    GdipGetPenWidth := LoadGdiPlusMethod('GdipGetPenWidth');
    GdipGetPenDashStyle := LoadGdiPlusMethod('GdipGetPenDashStyle');
    GdipSetPenDashStyle := LoadGdiPlusMethod('GdipSetPenDashStyle');
    GdipSetPenLineCap197819 := LoadGdiPlusMethod('GdipSetPenLineCap197819');
    GdipSetPenStartCap := LoadGdiPlusMethod('GdipSetPenStartCap');
    GdipSetPenEndCap := LoadGdiPlusMethod('GdipSetPenEndCap');
    GdipSetPenDashCap197819 := LoadGdiPlusMethod('GdipSetPenDashCap197819');
    GdipGetPenStartCap := LoadGdiPlusMethod('GdipGetPenStartCap');
    GdipGetPenEndCap := LoadGdiPlusMethod('GdipGetPenEndCap');
    GdipGetPenDashCap197819 := LoadGdiPlusMethod('GdipGetPenDashCap197819');
    GdipSetPenLineJoin := LoadGdiPlusMethod('GdipSetPenLineJoin');
    GdipGetPenLineJoin := LoadGdiPlusMethod('GdipGetPenLineJoin');
    GdipSetPenCustomStartCap := LoadGdiPlusMethod('GdipSetPenCustomStartCap');
    GdipGetPenCustomStartCap := LoadGdiPlusMethod('GdipGetPenCustomStartCap');
    GdipSetPenCustomEndCap := LoadGdiPlusMethod('GdipSetPenCustomEndCap');
    GdipGetPenCustomEndCap := LoadGdiPlusMethod('GdipGetPenCustomEndCap');
    GdipSetPenMiterLimit := LoadGdiPlusMethod('GdipSetPenMiterLimit');
    GdipGetPenMiterLimit := LoadGdiPlusMethod('GdipGetPenMiterLimit');
    GdipSetPenTransform := LoadGdiPlusMethod('GdipSetPenTransform');
    GdipGetPenTransform := LoadGdiPlusMethod('GdipGetPenTransform');
    GdipResetPenTransform := LoadGdiPlusMethod('GdipResetPenTransform');
    GdipMultiplyPenTransform := LoadGdiPlusMethod('GdipMultiplyPenTransform');
    GdipTranslatePenTransform := LoadGdiPlusMethod('GdipTranslatePenTransform');
    GdipScalePenTransform := LoadGdiPlusMethod('GdipScalePenTransform');
    GdipRotatePenTransform := LoadGdiPlusMethod('GdipRotatePenTransform');
    GdipGetPenDashOffset := LoadGdiPlusMethod('GdipGetPenDashOffset');
    GdipSetPenDashOffset := LoadGdiPlusMethod('GdipSetPenDashOffset');
    GdipGetPenDashCount := LoadGdiPlusMethod('GdipGetPenDashCount');
    GdipSetPenDashArray := LoadGdiPlusMethod('GdipSetPenDashArray');
    GdipGetPenDashArray := LoadGdiPlusMethod('GdipGetPenDashArray');
    GdipGetPenCompoundCount := LoadGdiPlusMethod('GdipGetPenCompoundCount');
    GdipSetPenCompoundArray := LoadGdiPlusMethod('GdipSetPenCompoundArray');
    GdipGetPenCompoundArray := LoadGdiPlusMethod('GdipGetPenCompoundArray');
    // GDI+ CustomLineCap
    GdipCreateCustomLineCap := LoadGdiPlusMethod('GdipCreateCustomLineCap');
    GdipDeleteCustomLineCap := LoadGdiPlusMethod('GdipDeleteCustomLineCap');
    GdipCloneCustomLineCap := LoadGdiPlusMethod('GdipCloneCustomLineCap');
    GdipGetCustomLineCapType := LoadGdiPlusMethod('GdipGetCustomLineCapType');
    GdipSetCustomLineCapStrokeCaps := LoadGdiPlusMethod('GdipSetCustomLineCapStrokeCaps');
    GdipGetCustomLineCapStrokeCaps := LoadGdiPlusMethod('GdipGetCustomLineCapStrokeCaps');
    GdipSetCustomLineCapStrokeJoin := LoadGdiPlusMethod('GdipSetCustomLineCapStrokeJoin');
    GdipGetCustomLineCapStrokeJoin := LoadGdiPlusMethod('GdipGetCustomLineCapStrokeJoin');
    GdipSetCustomLineCapBaseCap := LoadGdiPlusMethod('GdipSetCustomLineCapBaseCap');
    GdipGetCustomLineCapBaseCap := LoadGdiPlusMethod('GdipGetCustomLineCapBaseCap');
    GdipSetCustomLineCapBaseInset := LoadGdiPlusMethod('GdipSetCustomLineCapBaseInset');
    GdipGetCustomLineCapBaseInset := LoadGdiPlusMethod('GdipGetCustomLineCapBaseInset');
    GdipSetCustomLineCapWidthScale := LoadGdiPlusMethod('GdipSetCustomLineCapWidthScale');
    GdipGetCustomLineCapWidthScale := LoadGdiPlusMethod('GdipGetCustomLineCapWidthScale');
    // GDI+ AdjustableArrowCap
    GdipCreateAdjustableArrowCap := LoadGdiPlusMethod('GdipCreateAdjustableArrowCap');
    GdipSetAdjustableArrowCapHeight := LoadGdiPlusMethod('GdipSetAdjustableArrowCapHeight');
    GdipGetAdjustableArrowCapHeight := LoadGdiPlusMethod('GdipGetAdjustableArrowCapHeight');
    GdipSetAdjustableArrowCapWidth := LoadGdiPlusMethod('GdipSetAdjustableArrowCapWidth');
    GdipGetAdjustableArrowCapWidth := LoadGdiPlusMethod('GdipGetAdjustableArrowCapWidth');
    GdipSetAdjustableArrowCapMiddleInset := LoadGdiPlusMethod('GdipSetAdjustableArrowCapMiddleInset');
    GdipGetAdjustableArrowCapMiddleInset := LoadGdiPlusMethod('GdipGetAdjustableArrowCapMiddleInset');
    GdipSetAdjustableArrowCapFillState := LoadGdiPlusMethod('GdipSetAdjustableArrowCapFillState');
    GdipGetAdjustableArrowCapFillState := LoadGdiPlusMethod('GdipGetAdjustableArrowCapFillState');
    // GDI+ Graphics methods loading
    GdipFlush := LoadGdiPlusMethod('GdipFlush');
    GdipCreateFromHDC := LoadGdiPlusMethod('GdipCreateFromHDC');
    GdipCreateFromHDC2 := LoadGdiPlusMethod('GdipCreateFromHDC2');
    GdipCreateFromHWND := LoadGdiPlusMethod('GdipCreateFromHWND');
    GdipCreateFromHWNDICM := LoadGdiPlusMethod('GdipCreateFromHWNDICM');
    GdipDeleteGraphics := LoadGdiPlusMethod('GdipDeleteGraphics');
    GdipGetDC := LoadGdiPlusMethod('GdipGetDC');
    GdipReleaseDC := LoadGdiPlusMethod('GdipReleaseDC');
    GdipGraphicsClear := LoadGdiPlusMethod('GdipGraphicsClear');
    GdipCreateHalftonePalette := LoadGdiPlusMethod('GdipCreateHalftonePalette');
    GdipDrawLine := LoadGdiPlusMethod('GdipDrawLine');
    GdipDrawLineI := LoadGdiPlusMethod('GdipDrawLineI');
    GdipDrawLines := LoadGdiPlusMethod('GdipDrawLines');
    GdipDrawLinesI := LoadGdiPlusMethod('GdipDrawLinesI');
    GdipFillRectangle := LoadGdiPlusMethod('GdipFillRectangle');
    GdipFillRectangleI := LoadGdiPlusMethod('GdipFillRectangleI');
    GdipFillRectangles := LoadGdiPlusMethod('GdipFillRectangles');
    GdipFillRectanglesI := LoadGdiPlusMethod('GdipFillRectanglesI');
    GdipFillRegion := LoadGdiPlusMethod('GdipFillRegion');
    GdipDrawImage := LoadGdiPlusMethod('GdipDrawImage');
    GdipDrawImageI := LoadGdiPlusMethod('GdipDrawImageI');
    GdipDrawArc := LoadGdiPlusMethod('GdipDrawArc');
    GdipDrawArcI := LoadGdiPlusMethod('GdipDrawArcI');
    GdipDrawBezier := LoadGdiPlusMethod('GdipDrawBezier');
    GdipDrawBezierI := LoadGdiPlusMethod('GdipDrawBezierI');
    GdipDrawBeziers := LoadGdiPlusMethod('GdipDrawBeziers');
    GdipDrawBeziersI := LoadGdiPlusMethod('GdipDrawBeziersI');
    GdipDrawRectangle := LoadGdiPlusMethod('GdipDrawRectangle');
    GdipDrawRectangleI := LoadGdiPlusMethod('GdipDrawRectangleI');
    GdipDrawRectangles := LoadGdiPlusMethod('GdipDrawRectangles');
    GdipDrawRectanglesI := LoadGdiPlusMethod('GdipDrawRectanglesI');
    GdipDrawEllipse := LoadGdiPlusMethod('GdipDrawEllipse');
    GdipDrawEllipseI := LoadGdiPlusMethod('GdipDrawEllipseI');
    GdipDrawPie := LoadGdiPlusMethod('GdipDrawPie');
    GdipDrawPieI := LoadGdiPlusMethod('GdipDrawPieI');
    GdipDrawPolygon := LoadGdiPlusMethod('GdipDrawPolygon');
    GdipDrawPolygonI := LoadGdiPlusMethod('GdipDrawPolygonI');
    GdipDrawCurve := LoadGdiPlusMethod('GdipDrawCurve');
    GdipDrawCurveI := LoadGdiPlusMethod('GdipDrawCurveI');
    GdipDrawCurve2 := LoadGdiPlusMethod('GdipDrawCurve2');
    GdipDrawCurve2I := LoadGdiPlusMethod('GdipDrawCurve2I');
    GdipDrawCurve3 := LoadGdiPlusMethod('GdipDrawCurve3');
    GdipDrawCurve3I := LoadGdiPlusMethod('GdipDrawCurve3I');
    GdipDrawClosedCurve := LoadGdiPlusMethod('GdipDrawClosedCurve');
    GdipDrawClosedCurveI := LoadGdiPlusMethod('GdipDrawClosedCurveI');
    GdipDrawClosedCurve2 := LoadGdiPlusMethod('GdipDrawClosedCurve2');
    GdipDrawClosedCurve2I := LoadGdiPlusMethod('GdipDrawClosedCurve2I');
    GdipFillPolygon := LoadGdiPlusMethod('GdipFillPolygon');
    GdipFillPolygonI := LoadGdiPlusMethod('GdipFillPolygonI');
    GdipFillPolygon2 := LoadGdiPlusMethod('GdipFillPolygon2');
    GdipFillPolygon2I := LoadGdiPlusMethod('GdipFillPolygon2I');
    GdipFillEllipse := LoadGdiPlusMethod('GdipFillEllipse');
    GdipFillEllipseI := LoadGdiPlusMethod('GdipFillEllipseI');
    GdipFillPie := LoadGdiPlusMethod('GdipFillPie');
    GdipFillPieI := LoadGdiPlusMethod('GdipFillPieI');
    GdipFillClosedCurve := LoadGdiPlusMethod('GdipFillClosedCurve');
    GdipFillClosedCurveI := LoadGdiPlusMethod('GdipFillClosedCurveI');
    GdipFillClosedCurve2 := LoadGdiPlusMethod('GdipFillClosedCurve2');
    GdipFillClosedCurve2I := LoadGdiPlusMethod('GdipFillClosedCurve2I');
    GdipSetWorldTransform := LoadGdiPlusMethod('GdipSetWorldTransform');
    GdipResetWorldTransform := LoadGdiPlusMethod('GdipResetWorldTransform');
    GdipMultiplyWorldTransform := LoadGdiPlusMethod('GdipMultiplyWorldTransform');
    GdipTranslateWorldTransform := LoadGdiPlusMethod('GdipTranslateWorldTransform');
    GdipScaleWorldTransform := LoadGdiPlusMethod('GdipScaleWorldTransform');
    GdipRotateWorldTransform := LoadGdiPlusMethod('GdipRotateWorldTransform');
    GdipGetWorldTransform := LoadGdiPlusMethod('GdipGetWorldTransform');
    GdipResetPageTransform := LoadGdiPlusMethod('GdipResetPageTransform');
    GdipGetPageUnit := LoadGdiPlusMethod('GdipGetPageUnit');
    GdipGetPageScale := LoadGdiPlusMethod('GdipGetPageScale');
    GdipSetPageUnit := LoadGdiPlusMethod('GdipSetPageUnit');
    GdipSetPageScale := LoadGdiPlusMethod('GdipSetPageScale');
    GdipGetDpiX := LoadGdiPlusMethod('GdipGetDpiX');
    GdipGetDpiY := LoadGdiPlusMethod('GdipGetDpiY');
    GdipTransformPoints := LoadGdiPlusMethod('GdipTransformPoints');
    GdipTransformPointsI := LoadGdiPlusMethod('GdipTransformPointsI');
    GdipGetNearestColor := LoadGdiPlusMethod('GdipGetNearestColor');

    // added from MSN
    GdipLoadImageFromStream := LoadGdiPlusMethod('GdipLoadImageFromStream');
    GdipLoadImageFromStreamICM := LoadGdiPlusMethod('GdipLoadImageFromStreamICM');
    GdipCreateBitmapFromFile := LoadGdiPlusMethod('GdipCreateBitmapFromFile');
    GdipCreateBitmapFromStream := LoadGdiPlusMethod('GdipCreateBitmapFromStream');
    GdipCreateBitmapFromStreamICM := LoadGdiPlusMethod('GdipCreateBitmapFromStreamICM');
    GdipCreateBitmapFromFileICM := LoadGdiPlusMethod('GdipCreateBitmapFromFileICM');
    GdipImageGetFrameCount := LoadGdiPlusMethod('GdipImageGetFrameCount');
    GdipImageSelectActiveFrame := LoadGdiPlusMethod('GdipImageSelectActiveFrame');
    GdipImageRotateFlip := LoadGdiPlusMethod('GdipImageRotateFlip');
    GdipGetImagePalette := LoadGdiPlusMethod('GdipGetImagePalette');
    GdipSetImagePalette := LoadGdiPlusMethod('GdipSetImagePalette');
    GdipGetImagePaletteSize := LoadGdiPlusMethod('GdipGetImagePaletteSize');
    GdipGetPropertyCount := LoadGdiPlusMethod('GdipGetPropertyCount');
    GdipGetPropertyIdList := LoadGdiPlusMethod('GdipGetPropertyIdList');
    GdipGetPropertyItemSize := LoadGdiPlusMethod('GdipGetPropertyItemSize');
    GdipGetPropertyItem := LoadGdiPlusMethod('GdipGetPropertyItem');
    GdipGetPropertySize := LoadGdiPlusMethod('GdipGetPropertySize');
    GdipGetAllPropertyItems := LoadGdiPlusMethod('GdipGetAllPropertyItems');
    GdipRemovePropertyItem := LoadGdiPlusMethod('GdipRemovePropertyItem');
    GdipSetPropertyItem := LoadGdiPlusMethod('GdipSetPropertyItem');
    GdipImageForceValidation := LoadGdiPlusMethod('GdipImageForceValidation');
    GdipCreateHBITMAPFromBitmap := LoadGdiPlusMethod('GdipCreateHBITMAPFromBitmap');
    GdipCreateBitmapFromHICON := LoadGdiPlusMethod('GdipCreateBitmapFromHICON');
    GdipCreateHICONFromBitmap := LoadGdiPlusMethod('GdipCreateHICONFromBitmap');
    GdipCreateBitmapFromResource := LoadGdiPlusMethod('GdipCreateBitmapFromResource');
    GdipLoadImageFromFile := LoadGdiPlusMethod('GdipLoadImageFromFile');
    GdipLoadImageFromFileICM := LoadGdiPlusMethod('GdipLoadImageFromFileICM');
    GdipGetImageDimension := LoadGdiPlusMethod('GdipGetImageDimension');
    GdipDrawImageRect := LoadGdiPlusMethod('GdipDrawImageRect');
    GdipDrawImageRectI := LoadGdiPlusMethod('GdipDrawImageRectI');
    GdipDrawImagePoints := LoadGdiPlusMethod('GdipDrawImagePoints');
    GdipDrawImagePointsI := LoadGdiPlusMethod('GdipDrawImagePointsI');
    GdipImageGetFrameDimensionsCount := LoadGdiPlusMethod('GdipImageGetFrameDimensionsCount');
    GdipImageGetFrameDimensionsList := LoadGdiPlusMethod('GdipImageGetFrameDimensionsList');
    GdipDisposeImage := LoadGdiPlusMethod('GdipDisposeImage');
    GdipGetImageDecodersSize := LoadGdiPlusMethod('GdipGetImageDecodersSize');
    GdipGetImageDecoders := LoadGdiPlusMethod('GdipGetImageDecoders');
    GdipGetImageEncodersSize := LoadGdiPlusMethod('GdipGetImageEncodersSize');
    GdipGetImageEncoders := LoadGdiPlusMethod('GdipGetImageEncoders');
    GdipGetImageType := LoadGdiPlusMethod('GdipGetImageType');
    GdipSaveImageToStream := LoadGdiPlusMethod('GdipSaveImageToStream');
    GdipSaveImageToFile := LoadGdiPlusMethod('GdipSaveImageToFile');
    GdipCreateBitmapFromHBITMAP := LoadGdiPlusMethod('GdipCreateBitmapFromHBITMAP');
    GdipIsVisibleRect := LoadGdiPlusMethod('GdipIsVisibleRect');
    GdipIsVisibleRectI := LoadGdiPlusMethod('GdipIsVisibleRectI');
    GdipGetEncoderParameterListSize := LoadGdiPlusMethod('GdipGetEncoderParameterListSize');
    GdipGetEncoderParameterList := LoadGdiPlusMethod('GdipGetEncoderParameterList');
    GdipCreateBitmapFromGdiDib := LoadGdiPlusMethod('GdipCreateBitmapFromGdiDib');
    GdipCreateBitmapFromScan0 := LoadGdiPlusMethod('GdipCreateBitmapFromScan0');
    GdipBitmapLockBits := LoadGdiPlusMethod('GdipBitmapLockBits');
    GdipBitmapUnlockBits := LoadGdiPlusMethod('GdipBitmapUnlockBits');
    GdipBitmapGetPixel := LoadGdiPlusMethod('GdipBitmapGetPixel');
    GdipBitmapSetPixel := LoadGdiPlusMethod('GdipBitmapSetPixel');
    GdipBitmapSetResolution := LoadGdiPlusMethod('GdipBitmapSetResolution');
    GdipDrawImageRectRectI := LoadGdiPlusMethod('GdipDrawImageRectRectI');
    GdipDrawImageRectRect := LoadGdiPlusMethod('GdipDrawImageRectRect');
    GdipDrawImagePointRect := LoadGdiPlusMethod('GdipDrawImagePointRect');
    GdipDrawImagePointRectI := LoadGdiPlusMethod('GdipDrawImagePointRectI');
    GdipDrawImagePointsRect := LoadGdiPlusMethod('GdipDrawImagePointsRect');
    GdipDrawImagePointsRectI := LoadGdiPlusMethod('GdipDrawImagePointsRectI');
    GdipCloneImage := LoadGdiPlusMethod('GdipCloneImage');
    GdipCreateTexture := LoadGdiPlusMethod('GdipCreateTexture');
    GdipCreateTexture2 := LoadGdiPlusMethod('GdipCreateTexture2');
    GdipCreateTexture2I := LoadGdiPlusMethod('GdipCreateTexture2I');
    GdipCreateTextureIA := LoadGdiPlusMethod('GdipCreateTextureIA');
    GdipCreateTextureIAI := LoadGdiPlusMethod('GdipCreateTextureIAI');
    GdipGetTextureTransform := LoadGdiPlusMethod('GdipGetTextureTransform');
    GdipResetTextureTransform := LoadGdiPlusMethod('GdipResetTextureTransform');
    GdipMultiplyTextureTransform := LoadGdiPlusMethod('GdipMultiplyTextureTransform');
    GdipSetTextureTransform := LoadGdiPlusMethod('GdipSetTextureTransform');
    GdipTranslateTextureTransform := LoadGdiPlusMethod('GdipTranslateTextureTransform');
    GdipScaleTextureTransform := LoadGdiPlusMethod('GdipScaleTextureTransform');
    GdipRotateTextureTransform := LoadGdiPlusMethod('GdipRotateTextureTransform');
    GdipSaveAdd := LoadGdiPlusMethod('GdipSaveAdd');
    GdipSaveAddImage := LoadGdiPlusMethod('GdipSaveAddImage');
    GdipSetTextureWrapMode := LoadGdiPlusMethod('GdipSetTextureWrapMode');
    GdipGetTextureWrapMode := LoadGdiPlusMethod('GdipGetTextureWrapMode');
    GdipGetTextureImage := LoadGdiPlusMethod('GdipGetTextureImage');

    GdipCreateImageAttributes := LoadGdiPlusMethod('GdipCreateImageAttributes');
    GdipCloneImageAttributes := LoadGdiPlusMethod('GdipCloneImageAttributes');
    GdipDisposeImageAttributes := LoadGdiPlusMethod('GdipDisposeImageAttributes');
    GdipSetImageAttributesToIdentity := LoadGdiPlusMethod('GdipSetImageAttributesToIdentity');
    GdipResetImageAttributes := LoadGdiPlusMethod('GdipResetImageAttributes');
    GdipSetImageAttributesColorMatrix := LoadGdiPlusMethod('GdipSetImageAttributesColorMatrix');
    GdipSetImageAttributesThreshold := LoadGdiPlusMethod('GdipSetImageAttributesThreshold');
    GdipSetImageAttributesGamma := LoadGdiPlusMethod('GdipSetImageAttributesGamma');
    GdipSetImageAttributesColorKeys := LoadGdiPlusMethod('GdipSetImageAttributesColorKeys');
    GdipSetImageAttributesOutputChannel := LoadGdiPlusMethod('GdipSetImageAttributesOutputChannel');
    GdipSetImageAttributesOutputChannelColorProfile := LoadGdiPlusMethod('GdipSetImageAttributesOutputChannelColorProfile');
    GdipSetImageAttributesRemapTable := LoadGdiPlusMethod('GdipSetImageAttributesRemapTable');
    GdipSetImageAttributesWrapMode := LoadGdiPlusMethod('GdipSetImageAttributesWrapMode');
    GdipGetImageAttributesAdjustedPalette := LoadGdiPlusMethod('GdipGetImageAttributesAdjustedPalette');
    GdipSetImageAttributesNoOp := LoadGdiPlusMethod('GdipSetImageAttributesNoOp');

    GdipAddPathArc := LoadGdiPlusMethod('GdipAddPathArc');
    GdipAddPathLine := LoadGdiPlusMethod('GdipAddPathLine');
    GdipAddPathLine2 := LoadGdiPlusMethod('GdipAddPathLine2');
    GdipAddPathBezier := LoadGdiPlusMethod('GdipAddPathBezier');
    GdipAddPathBeziers := LoadGdiPlusMethod('GdipAddPathBeziers');
    GdipAddPathCurve := LoadGdiPlusMethod('GdipAddPathCurve');
    GdipAddPathCurve2 := LoadGdiPlusMethod('GdipAddPathCurve2');
    GdipAddPathCurve3 := LoadGdiPlusMethod('GdipAddPathCurve3');
    GdipAddPathClosedCurve := LoadGdiPlusMethod('GdipAddPathClosedCurve');
    GdipAddPathClosedCurve2 := LoadGdiPlusMethod('GdipAddPathClosedCurve2');
    GdipAddPathRectangle := LoadGdiPlusMethod('GdipAddPathRectangle');
    GdipAddPathRectangles := LoadGdiPlusMethod('GdipAddPathRectangles');
    GdipAddPathEllipse := LoadGdiPlusMethod('GdipAddPathEllipse');
    GdipAddPathPie := LoadGdiPlusMethod('GdipAddPathPie');
    GdipAddPathPolygon := LoadGdiPlusMethod('GdipAddPathPolygon');
    GdipAddPathPath := LoadGdiPlusMethod('GdipAddPathPath');
    GdipAddPathString := LoadGdiPlusMethod('GdipAddPathString');
    GdipAddPathStringI := LoadGdiPlusMethod('GdipAddPathStringI');
    GdipAddPathLineI := LoadGdiPlusMethod('GdipAddPathLineI');
    GdipAddPathLine2I := LoadGdiPlusMethod('GdipAddPathLine2I');
    GdipAddPathArcI := LoadGdiPlusMethod('GdipAddPathArcI');
    GdipAddPathBezierI := LoadGdiPlusMethod('GdipAddPathBezierI');
    GdipAddPathBeziersI := LoadGdiPlusMethod('GdipAddPathBeziersI');
    GdipAddPathCurveI := LoadGdiPlusMethod('GdipAddPathCurveI');
    GdipAddPathCurve2I := LoadGdiPlusMethod('GdipAddPathCurve2I');
    GdipAddPathCurve3I := LoadGdiPlusMethod('GdipAddPathCurve3I');
    GdipAddPathClosedCurveI := LoadGdiPlusMethod('GdipAddPathClosedCurveI');
    GdipAddPathClosedCurve2I := LoadGdiPlusMethod('GdipAddPathClosedCurve2I');
    GdipAddPathRectangleI := LoadGdiPlusMethod('GdipAddPathRectangleI');
    GdipAddPathRectanglesI := LoadGdiPlusMethod('GdipAddPathRectanglesI');
    GdipAddPathEllipseI := LoadGdiPlusMethod('GdipAddPathEllipseI');
    GdipAddPathPieI := LoadGdiPlusMethod('GdipAddPathPieI');
    GdipAddPathPolygonI := LoadGdiPlusMethod('GdipAddPathPolygonI');
    GdipFlattenPath := LoadGdiPlusMethod('GdipFlattenPath');
    GdipWindingModeOutline := LoadGdiPlusMethod('GdipWindingModeOutline');
    GdipWidenPath := LoadGdiPlusMethod('GdipWidenPath');
    GdipWarpPath := LoadGdiPlusMethod('GdipWarpPath');
    GdipTransformPath := LoadGdiPlusMethod('GdipTransformPath');
    GdipGetPathWorldBounds := LoadGdiPlusMethod('GdipGetPathWorldBounds');
    GdipGetPathWorldBoundsI := LoadGdiPlusMethod('GdipGetPathWorldBoundsI');
    GdipIsVisiblePathPoint := LoadGdiPlusMethod('GdipIsVisiblePathPoint');
    GdipIsVisiblePathPointI := LoadGdiPlusMethod('GdipIsVisiblePathPointI');
    GdipIsOutlineVisiblePathPoint := LoadGdiPlusMethod('GdipIsOutlineVisiblePathPoint');
    GdipIsOutlineVisiblePathPointI := LoadGdiPlusMethod('GdipIsOutlineVisiblePathPointI');
    GdipCreatePath := LoadGdiPlusMethod('GdipCreatePath');
    GdipCreatePath2 := LoadGdiPlusMethod('GdipCreatePath2');
    GdipCreatePath2I := LoadGdiPlusMethod('GdipCreatePath2I');
    GdipClonePath := LoadGdiPlusMethod('GdipClonePath');
    GdipDeletePath := LoadGdiPlusMethod('GdipDeletePath');
    GdipResetPath := LoadGdiPlusMethod('GdipResetPath');
    GdipGetPointCount := LoadGdiPlusMethod('GdipGetPointCount');
    GdipGetPathTypes := LoadGdiPlusMethod('GdipGetPathTypes');
    GdipGetPathPoints := LoadGdiPlusMethod('GdipGetPathPoints');
    GdipGetPathPointsI := LoadGdiPlusMethod('GdipGetPathPointsI');
    GdipGetPathFillMode := LoadGdiPlusMethod('GdipGetPathFillMode');
    GdipSetPathFillMode := LoadGdiPlusMethod('GdipSetPathFillMode');
    GdipGetPathData := LoadGdiPlusMethod('GdipGetPathData');
    GdipDrawPath := LoadGdiPlusMethod('GdipDrawPath');
    GdipFillPath := LoadGdiPlusMethod('GdipFillPath');
    GdipStartPathFigure := LoadGdiPlusMethod('GdipStartPathFigure');
    GdipClosePathFigure := LoadGdiPlusMethod('GdipClosePathFigure');
    GdipClosePathFigures := LoadGdiPlusMethod('GdipClosePathFigures');
    GdipSetPathMarker := LoadGdiPlusMethod('GdipSetPathMarker');
    GdipClearPathMarkers := LoadGdiPlusMethod('GdipClearPathMarkers');
    GdipReversePath := LoadGdiPlusMethod('GdipReversePath');
    GdipGetPathLastPoint := LoadGdiPlusMethod('GdipGetPathLastPoint');

    // PathIterator
    GdipCreatePathIter := LoadGdiPlusMethod('GdipCreatePathIter');
    GdipDeletePathIter := LoadGdiPlusMethod('GdipDeletePathIter');
    GdipPathIterNextSubpath := LoadGdiPlusMethod('GdipPathIterNextSubpath');
    GdipPathIterNextSubpathPath := LoadGdiPlusMethod('GdipPathIterNextSubpathPath');
    GdipPathIterNextPathType := LoadGdiPlusMethod('GdipPathIterNextPathType');
    GdipPathIterNextMarker := LoadGdiPlusMethod('GdipPathIterNextMarker');
    GdipPathIterNextMarkerPath := LoadGdiPlusMethod('GdipPathIterNextMarkerPath');
    GdipPathIterGetCount := LoadGdiPlusMethod('GdipPathIterGetCount');
    GdipPathIterGetSubpathCount := LoadGdiPlusMethod('GdipPathIterGetSubpathCount');
    GdipPathIterIsValid := LoadGdiPlusMethod('GdipPathIterIsValid');
    GdipPathIterHasCurve := LoadGdiPlusMethod('GdipPathIterHasCurve');
    GdipPathIterRewind := LoadGdiPlusMethod('GdipPathIterRewind');
    GdipPathIterEnumerate := LoadGdiPlusMethod('GdipPathIterEnumerate');
    GdipPathIterCopyData := LoadGdiPlusMethod('GdipPathIterCopyData');

    // Matrix
    GdipCreateMatrix := LoadGdiPlusMethod('GdipCreateMatrix');
    GdipCreateMatrix2 := LoadGdiPlusMethod('GdipCreateMatrix2');
    GdipCreateMatrix3 := LoadGdiPlusMethod('GdipCreateMatrix3');
    GdipCreateMatrix3I := LoadGdiPlusMethod('GdipCreateMatrix3I');
    GdipCloneMatrix := LoadGdiPlusMethod('GdipCloneMatrix');
    GdipDeleteMatrix := LoadGdiPlusMethod('GdipDeleteMatrix');
    GdipSetMatrixElements := LoadGdiPlusMethod('GdipSetMatrixElements');
    GdipMultiplyMatrix := LoadGdiPlusMethod('GdipMultiplyMatrix');
    GdipTranslateMatrix := LoadGdiPlusMethod('GdipTranslateMatrix');
    GdipScaleMatrix := LoadGdiPlusMethod('GdipScaleMatrix');
    GdipRotateMatrix := LoadGdiPlusMethod('GdipRotateMatrix');
    GdipShearMatrix := LoadGdiPlusMethod('GdipShearMatrix');
    GdipInvertMatrix := LoadGdiPlusMethod('GdipInvertMatrix');
    GdipTransformMatrixPoints := LoadGdiPlusMethod('GdipTransformMatrixPoints');
    GdipTransformMatrixPointsI := LoadGdiPlusMethod('GdipTransformMatrixPointsI');
    GdipVectorTransformMatrixPoints := LoadGdiPlusMethod('GdipVectorTransformMatrixPoints');
    GdipVectorTransformMatrixPointsI := LoadGdiPlusMethod('GdipVectorTransformMatrixPointsI');
    GdipGetMatrixElements := LoadGdiPlusMethod('GdipGetMatrixElements');
    GdipIsMatrixInvertible := LoadGdiPlusMethod('GdipIsMatrixInvertible');
    GdipIsMatrixIdentity := LoadGdiPlusMethod('GdipIsMatrixIdentity');
    GdipIsMatrixEqual := LoadGdiPlusMethod('GdipIsMatrixEqual');

    //lcm
    GdipSetInterpolationMode := LoadGdiPlusMethod('GdipSetInterpolationMode');
    GdipGetInterpolationMode := LoadGdiPlusMethod('GdipGetInterpolationMode');
    GdipCreateCachedBitmap := LoadGdiPlusMethod('GdipCreateCachedBitmap');
    GdipDeleteCachedBitmap := LoadGdiPlusMethod('GdipDeleteCachedBitmap');
    GdipDrawCachedBitmap := LoadGdiPlusMethod('GdipDrawCachedBitmap');
    GdipCreateBitmapFromGraphics := LoadGdiPlusMethod('GdipCreateBitmapFromGraphics');
    GdipGetImageBounds := LoadGdiPlusMethod('GdipGetImageBounds');
    GdipGetImageGraphicsContext := LoadGdiPlusMethod('GdipGetImageGraphicsContext');
    GdipGetImageWidth := LoadGdiPlusMethod('GdipGetImageWidth');
    GdipGetImageHeight := LoadGdiPlusMethod('GdipGetImageHeight');
    GdipGetImageHorizontalResolution := LoadGdiPlusMethod('GdipGetImageHorizontalResolution');
    GdipGetImageVerticalResolution := LoadGdiPlusMethod('GdipGetImageVerticalResolution');
    GdipGetImageFlags := LoadGdiPlusMethod('GdipGetImageFlags');
    GdipSetCompositingMode := LoadGdiPlusMethod('GdipSetCompositingMode');
    GdipGetCompositingMode := LoadGdiPlusMethod('GdipGetCompositingMode');
    GdipSetCompositingQuality := LoadGdiPlusMethod('GdipSetCompositingQuality');
    GdipGetCompositingQuality := LoadGdiPlusMethod('GdipGetCompositingQuality');
    GdipSetSmoothingMode := LoadGdiPlusMethod('GdipSetSmoothingMode');
    GdipGetSmoothingMode := LoadGdiPlusMethod('GdipGetSmoothingMode');
    GdipCloneBitmapArea := LoadGdiPlusMethod('GdipCloneBitmapArea');
    GdipCloneBitmapAreaI := LoadGdiPlusMethod('GdipCloneBitmapAreaI');
    GdipSetRenderingOrigin := LoadGdiPlusMethod('GdipSetRenderingOrigin');
    GdipGetRenderingOrigin := LoadGdiPlusMethod('GdipGetRenderingOrigin');
    GdipSetPixelOffsetMode := LoadGdiPlusMethod('GdipSetPixelOffsetMode');
    GdipGetPixelOffsetMode := LoadGdiPlusMethod('GdipGetPixelOffsetMode');
    GdipSetTextRenderingHint := LoadGdiPlusMethod('GdipSetTextRenderingHint');
    GdipGetTextRenderingHint := LoadGdiPlusMethod('GdipGetTextRenderingHint');
    GdipSetTextContrast := LoadGdiPlusMethod('GdipSetTextContrast');
    GdipGetTextContrast := LoadGdiPlusMethod('GdipGetTextContrast');

    GdipGetImageRawFormat :=  LoadGdiPlusMethod('GdipGetImageRawFormat');
    GdipGetImagePixelFormat := LoadGdiPlusMethod('GdipGetImagePixelFormat');
    GdipGetImageThumbnail := LoadGdiPlusMethod('GdipGetImageThumbnail');

    GdipCreateRegion := LoadGdiPlusMethod('GdipCreateRegion');
    GdipCreateRegionPath := LoadGdiPlusMethod('GdipCreateRegionPath');
    GdipCreateRegionRect := LoadGdiPlusMethod('GdipCreateRegionRect');
    GdipCreateRegionRectI := LoadGdiPlusMethod('GdipCreateRegionRectI');
    GdipCreateRegionRgnData := LoadGdiPlusMethod('GdipCreateRegionRgnData');
    GdipCreateRegionHrgn := LoadGdiPlusMethod('GdipCreateRegionHrgn');
    GdipCloneRegion := LoadGdiPlusMethod('GdipCloneRegion');
    GdipDeleteRegion := LoadGdiPlusMethod('GdipDeleteRegion');
    GdipSetInfinite := LoadGdiPlusMethod('GdipSetInfinite');
    GdipSetEmpty := LoadGdiPlusMethod('GdipSetEmpty');
    GdipCombineRegionRect := LoadGdiPlusMethod('GdipCombineRegionRect');
    GdipCombineRegionRectI := LoadGdiPlusMethod('GdipCombineRegionRectI');
    GdipCombineRegionPath := LoadGdiPlusMethod('GdipCombineRegionPath');
    GdipCombineRegionRegion := LoadGdiPlusMethod('GdipCombineRegionRegion');
    GdipTranslateRegion := LoadGdiPlusMethod('GdipTranslateRegion');
    GdipTranslateRegionI := LoadGdiPlusMethod('GdipTranslateRegionI');
    GdipTransformRegion := LoadGdiPlusMethod('GdipTransformRegion');
    GdipGetRegionBounds := LoadGdiPlusMethod('GdipGetRegionBounds');
    GdipGetRegionBoundsI := LoadGdiPlusMethod('GdipGetRegionBoundsI');
    GdipGetRegionHRgn := LoadGdiPlusMethod('GdipGetRegionHRgn');
    GdipIsEmptyRegion := LoadGdiPlusMethod('GdipIsEmptyRegion');
    GdipIsInfiniteRegion := LoadGdiPlusMethod('GdipIsInfiniteRegion');
    GdipIsEqualRegion := LoadGdiPlusMethod('GdipIsEqualRegion');
    GdipGetRegionDataSize := LoadGdiPlusMethod('GdipGetRegionDataSize');
    GdipGetRegionData := LoadGdiPlusMethod('GdipGetRegionData');
    GdipIsVisibleRegionPoint := LoadGdiPlusMethod('GdipIsVisibleRegionPoint');
    GdipIsVisibleRegionPointI := LoadGdiPlusMethod('GdipIsVisibleRegionPointI');
    GdipIsVisibleRegionRect := LoadGdiPlusMethod('GdipIsVisibleRegionRect');
    GdipIsVisibleRegionRectI := LoadGdiPlusMethod('GdipIsVisibleRegionRectI');
    GdipGetRegionScansCount := LoadGdiPlusMethod('GdipGetRegionScansCount');
    GdipGetRegionScans := LoadGdiPlusMethod('GdipGetRegionScans');
    GdipGetRegionScansI := LoadGdiPlusMethod('GdipGetRegionScansI');
    GdipGetClip := LoadGdiPlusMethod('GdipGetClip');
    GdipResetClip := LoadGdiPlusMethod('GdipResetClip');
    GdipSetClipGraphics := LoadGdiPlusMethod('GdipSetClipGraphics');
    GdipSetClipRegion := LoadGdiPlusMethod('GdipSetClipRegion');
    GdipSetClipRect := LoadGdiPlusMethod('GdipSetClipRect');
    GdipSetClipRectI := LoadGdiPlusMethod('GdipSetClipRectI');
    GdipSetClipPath := LoadGdiPlusMethod('GdipSetClipPath');
    GdipSetClipHrgn := LoadGdiPlusMethod('GdipSetClipHrgn');
    GdipTranslateClip := LoadGdiPlusMethod('GdipTranslateClip');
    GdipTranslateClipI := LoadGdiPlusMethod('GdipTranslateClipI');
    GdipGetClipBounds := LoadGdiPlusMethod('GdipGetClipBounds');
    GdipGetClipBoundsI := LoadGdiPlusMethod('GdipGetClipBoundsI');
    GdipIsClipEmpty := LoadGdiPlusMethod('GdipIsClipEmpty');
    GdipGetVisibleClipBounds := LoadGdiPlusMethod('GdipGetVisibleClipBounds');
    GdipGetVisibleClipBoundsI := LoadGdiPlusMethod('GdipGetVisibleClipBoundsI');
    GdipIsVisibleClipEmpty := LoadGdiPlusMethod('GdipIsVisibleClipEmpty');
    GdipIsVisiblePoint := LoadGdiPlusMethod('GdipIsVisiblePoint');
    GdipIsVisiblePointI := LoadGdiPlusMethod('GdipIsVisiblePointI');
    GdipSaveGraphics := LoadGdiPlusMethod('GdipSaveGraphics');
    GdipRestoreGraphics := LoadGdiPlusMethod('GdipRestoreGraphics');
    GdipBeginContainer := LoadGdiPlusMethod('GdipBeginContainer');
    GdipBeginContainerI := LoadGdiPlusMethod('GdipBeginContainerI');
    GdipBeginContainer2 := LoadGdiPlusMethod('GdipBeginContainer2');
    GdipEndContainer := LoadGdiPlusMethod('GdipEndContainer');

    GdipEnumerateMetafileDestPoint := LoadGdiPlusMethod('GdipEnumerateMetafileDestPoint');
    GdipEnumerateMetafileDestPointI := LoadGdiPlusMethod('GdipEnumerateMetafileDestPointI');
    GdipEnumerateMetafileDestRect := LoadGdiPlusMethod('GdipEnumerateMetafileDestRect');
    GdipEnumerateMetafileDestRectI := LoadGdiPlusMethod('GdipEnumerateMetafileDestRectI');
    GdipEnumerateMetafileDestPoints := LoadGdiPlusMethod('GdipEnumerateMetafileDestPoints');
    GdipEnumerateMetafileDestPointsI := LoadGdiPlusMethod('GdipEnumerateMetafileDestPoints');
    GdipEnumerateMetafileSrcRectDestPoint := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestPoint');
    GdipEnumerateMetafileSrcRectDestPointI := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestPointI');
    GdipEnumerateMetafileSrcRectDestRect := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestRect');
    GdipEnumerateMetafileSrcRectDestRectI := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestRectI');
    GdipEnumerateMetafileSrcRectDestPoints := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestPoints');
    GdipEnumerateMetafileSrcRectDestPointsI := LoadGdiPlusMethod('GdipEnumerateMetafileSrcRectDestPointsI');
    GdipPlayMetafileRecord := LoadGdiPlusMethod('GdipPlayMetafileRecord');
    GdipGetMetafileHeaderFromWmf := LoadGdiPlusMethod('GdipGetMetafileHeaderFromWmf');
    GdipGetMetafileHeaderFromEmf := LoadGdiPlusMethod('GdipGetMetafileHeaderFromEmf');
    GdipGetMetafileHeaderFromFile := LoadGdiPlusMethod('GdipGetMetafileHeaderFromFile');
    GdipGetMetafileHeaderFromStream := LoadGdiPlusMethod('GdipGetMetafileHeaderFromStream');
    GdipGetMetafileHeaderFromMetafile := LoadGdiPlusMethod('GdipGetMetafileHeaderFromMetafile');
    GdipGetHemfFromMetafile := LoadGdiPlusMethod('GdipGetHemfFromMetafile');
    GdipCreateStreamOnFile := LoadGdiPlusMethod('GdipCreateStreamOnFile');
    GdipCreateMetafileFromWmf := LoadGdiPlusMethod('GdipCreateMetafileFromWmf');
    GdipCreateMetafileFromEmf := LoadGdiPlusMethod('GdipCreateMetafileFromEmf');
    GdipCreateMetafileFromFile := LoadGdiPlusMethod('GdipCreateMetafileFromFile');
    GdipCreateMetafileFromWmfFile := LoadGdiPlusMethod('GdipCreateMetafileFromWmfFile');
    GdipCreateMetafileFromStream := LoadGdiPlusMethod('GdipCreateMetafileFromStream');
    GdipRecordMetafile := LoadGdiPlusMethod('GdipRecordMetafile');
    GdipRecordMetafileI := LoadGdiPlusMethod('GdipRecordMetafileI');
    GdipRecordMetafileFileName := LoadGdiPlusMethod('GdipRecordMetafileFileName');
    GdipRecordMetafileFileNameI := LoadGdiPlusMethod('GdipRecordMetafileFileNameI');
    GdipRecordMetafileStream := LoadGdiPlusMethod('GdipRecordMetafileStream');
    GdipRecordMetafileStreamI := LoadGdiPlusMethod('GdipRecordMetafileStreamI');
    GdipSetMetafileDownLevelRasterizationLimit := LoadGdiPlusMethod('GdipSetMetafileDownLevelRasterizationLimit');
    GdipGetMetafileDownLevelRasterizationLimit := LoadGdiPlusMethod('GdipGetMetafileDownLevelRasterizationLimit');
    GdipComment := LoadGdiPlusMethod('GdipComment');

    GdipCreateFontFamilyFromName := LoadGdiPlusMethod('GdipCreateFontFamilyFromName');
    GdipDeleteFontFamily := LoadGdiPlusMethod('GdipDeleteFontFamily');
    GdipCloneFontFamily := LoadGdiPlusMethod('GdipCloneFontFamily');
    GdipGetGenericFontFamilySansSerif := LoadGdiPlusMethod('GdipGetGenericFontFamilySansSerif');
    GdipGetGenericFontFamilySerif := LoadGdiPlusMethod('GdipGetGenericFontFamilySerif');
    GdipGetGenericFontFamilyMonospace := LoadGdiPlusMethod('GdipGetGenericFontFamilyMonospace');
    GdipGetFamilyName := LoadGdiPlusMethod('GdipGetFamilyName');
    GdipIsStyleAvailable := LoadGdiPlusMethod('GdipIsStyleAvailable');
    GdipGetEmHeight := LoadGdiPlusMethod('GdipGetEmHeight');
    GdipGetCellAscent := LoadGdiPlusMethod('GdipGetCellAscent');
    GdipGetCellDescent := LoadGdiPlusMethod('GdipGetCellDescent');
    GdipGetLineSpacing := LoadGdiPlusMethod('GdipGetLineSpacing');

    GdipCreateFontFromDC := LoadGdiPlusMethod('GdipCreateFontFromDC');
    GdipCreateFontFromLogfontA := LoadGdiPlusMethod('GdipCreateFontFromLogfontA');
    GdipCreateFontFromLogfontW := LoadGdiPlusMethod('GdipCreateFontFromLogfontW');
    GdipCreateFontFromLogfont := GdipCreateFontFromLogfontW;
    GdipCreateFont := LoadGdiPlusMethod('GdipCreateFont');
    GdipCloneFont := LoadGdiPlusMethod('GdipCloneFont');
    GdipDeleteFont := LoadGdiPlusMethod('GdipDeleteFont');
    GdipGetFamily := LoadGdiPlusMethod('GdipGetFamily');
    GdipGetFontStyle := LoadGdiPlusMethod('GdipGetFontStyle');
    GdipGetFontSize := LoadGdiPlusMethod('GdipGetFontSize');
    GdipGetFontUnit := LoadGdiPlusMethod('GdipGetFontUnit');
    GdipGetFontHeight := LoadGdiPlusMethod('GdipGetFontHeight');
    GdipGetFontHeightGivenDPI := LoadGdiPlusMethod('GdipGetFontHeightGivenDPI');
    GdipGetLogFontA := LoadGdiPlusMethod('GdipGetLogFontA');
    GdipGetLogFontW := LoadGdiPlusMethod('GdipGetLogFontW');
    GdipNewInstalledFontCollection := LoadGdiPlusMethod('GdipNewInstalledFontCollection');
    GdipNewPrivateFontCollection := LoadGdiPlusMethod('GdipNewPrivateFontCollection');
    GdipDeletePrivateFontCollection := LoadGdiPlusMethod('GdipDeletePrivateFontCollection');
    GdipGetFontCollectionFamilyCount := LoadGdiPlusMethod('GdipGetFontCollectionFamilyCount');
    GdipGetFontCollectionFamilyList := LoadGdiPlusMethod('GdipGetFontCollectionFamilyList');
    GdipPrivateAddFontFile := LoadGdiPlusMethod('GdipPrivateAddFontFile');
    GdipPrivateAddMemoryFont := LoadGdiPlusMethod('GdipPrivateAddMemoryFont');

    GdipDrawString := LoadGdiPlusMethod('GdipDrawString');
    GdipMeasureString := LoadGdiPlusMethod('GdipMeasureString');
    GdipMeasureCharacterRanges := LoadGdiPlusMethod('GdipMeasureCharacterRanges');
    GdipDrawDriverString := LoadGdiPlusMethod('GdipDrawDriverString');
    GdipMeasureDriverString := LoadGdiPlusMethod('GdipMeasureDriverString');

    GdipCreateStringFormat := LoadGdiPlusMethod('GdipCreateStringFormat');
    GdipStringFormatGetGenericDefault := LoadGdiPlusMethod('GdipStringFormatGetGenericDefault');
    GdipStringFormatGetGenericTypographic := LoadGdiPlusMethod('GdipStringFormatGetGenericTypographic');
    GdipDeleteStringFormat := LoadGdiPlusMethod('GdipDeleteStringFormat');
    GdipCloneStringFormat := LoadGdiPlusMethod('GdipCloneStringFormat');
    GdipSetStringFormatFlags := LoadGdiPlusMethod('GdipSetStringFormatFlags');
    GdipGetStringFormatFlags := LoadGdiPlusMethod('GdipGetStringFormatFlags');
    GdipSetStringFormatAlign := LoadGdiPlusMethod('GdipSetStringFormatAlign');
    GdipGetStringFormatAlign := LoadGdiPlusMethod('GdipGetStringFormatAlign');
    GdipSetStringFormatLineAlign := LoadGdiPlusMethod('GdipSetStringFormatLineAlign');
    GdipGetStringFormatLineAlign := LoadGdiPlusMethod('GdipGetStringFormatLineAlign');
    GdipSetStringFormatTrimming := LoadGdiPlusMethod('GdipSetStringFormatTrimming');
    GdipGetStringFormatTrimming := LoadGdiPlusMethod('GdipGetStringFormatTrimming');
    GdipSetStringFormatHotkeyPrefix := LoadGdiPlusMethod('GdipSetStringFormatHotkeyPrefix');
    GdipGetStringFormatHotkeyPrefix := LoadGdiPlusMethod('GdipGetStringFormatHotkeyPrefix');
    GdipSetStringFormatTabStops := LoadGdiPlusMethod('GdipSetStringFormatTabStops');
    GdipGetStringFormatTabStops := LoadGdiPlusMethod('GdipGetStringFormatTabStops');
    GdipGetStringFormatTabStopCount := LoadGdiPlusMethod('GdipGetStringFormatTabStopCount');
    GdipSetStringFormatDigitSubstitution := LoadGdiPlusMethod('GdipSetStringFormatDigitSubstitution');
    GdipGetStringFormatDigitSubstitution := LoadGdiPlusMethod('GdipGetStringFormatDigitSubstitution');
    GdipGetStringFormatMeasurableCharacterRangeCount := LoadGdiPlusMethod('GdipGetStringFormatMeasurableCharacterRangeCount');
    GdipSetStringFormatMeasurableCharacterRanges := LoadGdiPlusMethod('GdipSetStringFormatMeasurableCharacterRanges');

    GdipEmfToWmfBits := LoadGdiPlusMethod('GdipEmfToWmfBits');

    //
    if (GdiPlusStartup(FGDIPlusToken, DefaultStartup, @FGdiPlusHook) <> OK) or
      (FGdiPlusHook.NotificationHook(FGDIPlusToken) <> Ok) then
    begin
      FGDIPresent := False;
      FillChar(FGdiPlusHook, SizeOf(FGdiPlusHook), 0);
    end;
  end;
end;

function CheckGdiPlus(AHaltOnError: Boolean = False): Boolean;
var
  ATitle: array[0..127] of Char;
const
  AMessage: PChar = 'This application requires the Microsoft GDI+ library to be installed. ' +
                    'Gdiplus.dll is included with Windows XP and Vista. GDI+ is available ' +
                    'as a redistributable file for Windows NT 4.0 SP6, Windows 2000, Windows 98, ' +
                     'and Windows Me.  To download the latest redistributable,  see ' +
                    'http://www.microsoft.com/downloads';

begin
  if not FGDIPresent then
    GdiPlusLoad;
  Result := FGDIPresent;
  if AHaltOnError and not Result then
  begin
    LoadString(FindResourceHInstance(HInstance), PResStringRec(@SExceptTitle).Identifier,
      ATitle, SizeOf(ATitle));
    MessageBox(0, AMessage, ATitle, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
    Halt;
  end;
end;

procedure GdiPlusUnload;
begin
  if FGDIPresent then
  begin
    FGdiPlusHook.NotificationUnhook(FGDIPlusToken);
    GdiPlusShutdown(FGDIPlusToken);
  end;
  if FGDIPlusLibrary <> 0 then
    FreeLibrary(FGDIPlusLibrary);
  FGDIPresent := False;
end;


{ TdxGpRectF }

procedure TdxGpRectF.Empty;
begin
  X := 0;
  Y := 0;
  Width := 0;
  Height := 0;
end;

procedure TdxGpRectF.Init(AX, AY, AWidth, AHeight: Single);
begin
  X := AX;
  Y := AY;
  Width := AWidth;
  Height := AHeight;
end;

function TdxGpRectF.SizeF: TdxSizeF;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

class operator TdxGpRectF.Implicit(const ARect: TRect): TdxGpRectF;
begin
  Result.X      := ARect.Left;
  Result.Y      := ARect.Top;
  Result.Width  := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;

class operator TdxGpRectF.Implicit(const ARect: TdxRectF): TdxGpRectF;
begin
  Result.X      := ARect.Left;
  Result.Y      := ARect.Top;
  Result.Width  := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;

class operator TdxGpRectF.Implicit(const ARect: TdxGpRectF): TRect;
begin
  Result.Left := Trunc(ARect.X);
  Result.Top := Trunc(ARect.Y);
  Result.Right := Result.Left + Trunc(ARect.Width);
  Result.Bottom := Result.Top + Trunc(ARect.Height);
end;

class operator TdxGpRectF.Implicit(const ARect: TdxGpRectF): TdxRectF;
begin
  Result.Left := ARect.X;
  Result.Top := ARect.Y;
  Result.Right := Result.Left + ARect.Width;
  Result.Bottom := Result.Top + ARect.Height;
end;

{ TdxGpRect }

procedure TdxGpRect.Empty;
begin
  X := 0;
  Y := 0;
  Width := 0;
  Height := 0;
end;

procedure TdxGpRect.Init(AX, AY, AWidth, AHeight: Integer);
begin
  X := AX;
  Y := AY;
  Width := AWidth;
  Height := AHeight;
end;

class operator TdxGpRect.Implicit(const ARect: TRect): TdxGpRect;
begin
  Result.X      := ARect.Left;
  Result.Y      := ARect.Top;
  Result.Width  := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;

{ TGdiplusBase }

procedure TdxGpBase.AfterConstruction;
begin
  inherited;
  FIsGpUsed := FGdiPresent;
end;

class function TdxGpBase.NewInstance: TObject;
begin
  if FGdiPresent then
    Result := InitInstance(GdipAlloc(ULONG(instanceSize)))
  else
    Result := inherited NewInstance;
end;

procedure TdxGpBase.FreeInstance;
begin
  CleanupInstance;
  if FIsGpUsed then
    GdipFree(Self)
  else
    inherited FreeInstance;
end;

{ TdxGpPoint }

function MakePoint(X, Y: Integer): TdxGpPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePoint(X, Y: Single): TdxGpPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TdxGpSizeF }

function MakeSize(AWidth, AHeight: Single): TdxGpSizeF;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function MakeSize(AWidth, AHeight: Integer): TdxGpSize;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

{ TdxGpRect }

function MakeRect(X, Y, AWidth, AHeight: Integer): TdxGpRect;
begin
  Result.X      := X;
  Result.Y      := Y;
  Result.Width  := AWidth;
  Result.Height := AHeight;
end;

function MakeRect(const ALocation: TdxGpPoint; const ASize: TdxGpSize): TdxGpRect;
begin
  Result.X      := ALocation.X;
  Result.Y      := ALocation.Y;
  Result.Width  := ASize.Width;
  Result.Height := ASize.Height;
end;

function MakeRect(const ARect: TRect): TdxGpRect;
begin
  Result.X      := ARect.Left;
  Result.Y      := ARect.Top;
  Result.Width  := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;


{ TdxGpRectF }

function MakeRect(X, Y, AWidth, AHeight: Single): TdxGpRectF;
begin
  Result.X      := X;
  Result.Y      := Y;
  Result.Width  := AWidth;
  Result.Height := AHeight;
end;

function MakeRect(const ALocationF: TdxGpPointF; const ASizeF: TdxGpSizeF): TdxGpRectF;
begin
  Result.X      := ALocationF.X;
  Result.Y      := ALocationF.Y;
  Result.Width  := ASizeF.Width;
  Result.Height := ASizeF.Height;
end;

function dxGpMakeRectF(const ARect: TRect): TdxGpRectF;
begin
  Result.X := ARect.Left;
  Result.Y := ARect.Top;
  Result.Width := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;

function dxGpMakeRectF(const ARect: TdxGpRect): TdxGpRectF;
begin
  Result.X := ARect.X;
  Result.Y := ARect.Y;
  Result.Width := ARect.Width;
  Result.Height := ARect.Height;
end;

function dxGpRectIsEqual(const R1, R2: TdxGpRect): Boolean;
begin
  Result := dxGpRectIsEqual(dxGpMakeRectF(R1), dxGpMakeRectF(R2));
end;

function dxGpRectIsEqual(const R1, R2: TdxGpRectF): Boolean;
begin
  Result := (R1.X = R2.X) and (R1.Y = R2.Y) and (R1.Width = R2.Width) and (R1.Height = R2.Height);
end;

function GetEncoderID(const CodecName: string; out CodecID: TGUID): GpStatus;
var
  ACodecInfo: PdxGpImageCodecInfo;
  ACount: Integer;
  AIndex: Integer;
  ASize: Integer;
  AStartInfo: PdxGpImageCodecInfo;
begin
  ACount := 0;
  ASize := 0;
  Result := GenericError;
  if CheckGdiPlus and (GdipGetImageEncodersSize(ACount, ASize) = Ok) and (ASize > 0) then
  begin
    GetMem(AStartInfo, ASize);
    ACodecInfo := AStartInfo;
    try
      if GdipGetImageEncoders(ACount, ASize, ACodecInfo) = Ok then
        for AIndex := 0 to ACount - 1 do
        begin
          if SameText(PWideChar(ACodecInfo^.MimeType), CodecName) then
          begin
             CodecID := ACodecInfo^.Clsid;
             Result := Ok;
             Break;
          end;
          Inc(TdxNativeInt(ACodecInfo), SizeOf(TdxGpImageCodecInfo));
        end;
    finally
      FreeMem(AStartInfo, ASize);
    end;
  end;
end;

function GetDecoderID(const CodecName: string; out DecoderID: TGUID): GpStatus;
var
  ACodecInfo: PdxGpImageCodecInfo;
  ACount: Cardinal;
  AIndex: Integer;
  ASize: Cardinal;
  AStartInfo: PdxGpImageCodecInfo;
begin
  ACount := 0;
  ASize := 0;
  Result := GenericError;
  if CheckGdiPlus and (GdipGetImageDecodersSize(ACount, ASize) = Ok) and (ASize > 0) then
  begin
    GetMem(AStartInfo, ASize);
    ACodecInfo := AStartInfo;
    try
      if GdipGetImageDecoders(ACount, ASize, ACodecInfo) = Ok then
        for AIndex := 0 to ACount - 1 do
        begin
          if SameText(PWideChar(ACodecInfo^.MimeType), CodecName) then
          begin
             DecoderID := ACodecInfo^.Clsid;
             Result := Ok;
             Break;
          end;
          Inc(TdxNativeInt(ACodecInfo), SizeOf(TdxGpImageCodecInfo));
        end;
    finally
      FreeMem(AStartInfo, ASize);
    end;
  end;
end;

function IsCodecIDValid(const CodecID: TGUID): Boolean;
begin
  Result := (CodecID.D1 <> 0) or (CodecID.D2 <> 0) or (CodecID.D3 <> 0);
end;

function CheckEncoder(const AImageMimeType: string; var ACodecID: TGUID): GpStatus;
begin
  if IsCodecIDValid(ACodecID) then
    Result := Ok
  else
    Result := GetEncoderID(AImageMimeType, ACodecID);
end;

function CheckDecoder(const AImageMimeType: string; var ACodecID: TGUID): GpStatus;
begin
  if IsCodecIDValid(ACodecID) then
    Result := Ok
  else
    Result := GetDecoderID(AImageMimeType, ACodecID);
end;

procedure CheckImageCodecs;

  procedure CheckOptionalEncoder(const AImageMimeType: string; out ACodecID: TGUID);
  begin
    if CheckEncoder(AImageMimeType, ACodecID) <> Ok then
      ZeroMemory(@ACodecID, SizeOf(ACodecID));
  end;

  procedure CheckOptionalDecoder(const AImageMimeType: string; out ACodecID: TGUID);
  begin
    if CheckDecoder(AImageMimeType, ACodecID) <> Ok then
      ZeroMemory(@ACodecID, SizeOf(ACodecID));
  end;

begin
  GdipCheck(CheckEncoder('image/png', PNGEncoder));
  GdipCheck(CheckDecoder('image/png', PNGDecoder));
  CheckOptionalEncoder('image/bmp', BMPEncoder);
  CheckOptionalDecoder('image/bmp', BMPDecoder);
  CheckOptionalEncoder('image/gif', GIFEncoder);
  CheckOptionalDecoder('image/gif', GIFDecoder);
  CheckOptionalEncoder('image/jpeg', JPEGEncoder);
  CheckOptionalDecoder('image/jpeg', JPEGDecoder);
  CheckOptionalEncoder('image/tiff', TIFFEncoder);
  CheckOptionalDecoder('image/tiff', TIFFDecoder);
  CheckOptionalDecoder('image/x-emf', EMFDecoder);
  CheckOptionalDecoder('image/x-wmf', WMFDecoder);
  CheckOptionalDecoder('image/x-icon', ICONDecoder);
end;

procedure CheckPngCodec;
begin
  CheckImageCodecs;
end;

procedure GdipCheck(AStatus: GpStatus);
begin
  if AStatus <> Ok then
    raise EdxGdipException.Create(AStatus);
end;

procedure GdipCheck(AStatus: Boolean);
begin
  if not AStatus then
    raise EdxGdipException.Create(InvalidParameter);
end;


{ EdxGdipException }

constructor EdxGdipException.Create(AStatus: GpStatus);
begin
  FStatus := AStatus;
  if (Status = Win32Error) and (GetLastError <> 0) then
    CreateFmt(scxGdipInvalidOperationWin32Error, [GetLastError])
  else
    CreateFmt(scxGdipInvalidOperation, [Ord(Status)]);
end;


procedure RegisterAssistants;
begin
  FGDIPresent := False;
end;

procedure UnregisterAssistants;
begin
  GdiPlusUnload;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);
end.

