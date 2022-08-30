{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.OfficeImage;

{$I cxVer.inc}
{$I dxRichEditControl.inc}
{.$DEFINE DXLOGGING}

interface

uses
  Types, Classes, SysUtils, Windows, SyncObjs, Graphics, Controls, ActiveX, Generics.Defaults, Generics.Collections,
  Contnrs,
  dxCore, cxGraphics, dxCoreClasses, dxCoreGraphics, dxGDIPlusClasses, dxGDIPlusApi, cxGeometry, dxSmartImage,
  dxRichEdit.ServiceManager,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter;

type
  TdxUriOfficeImage = class;

  TdxImageSizeMode = (
    Normal,
    StretchImage,
    AutoSize,
    CenterImage,
    ZoomImage,
    Squeeze,
    Tile);

  TdxOfficeImageFormat = (
    None,
    Bmp,
    Emf,
    Exif,
    Gif,
    Icon,
    Jpeg,
    MemoryBmp,
    Png,
    Tiff,
    Wmf);

  TdxMapMode = (
    Text = 1,
    LowMetric = 2,
    HighMetric = 3,
    LowEnglish = 4,
    HighEnglish = 5,
    Twips = 6,
    Isotropic = 7,
    Anisotropic = 8);

  TdxOfficePixelFormat = (
    DontCare = 0,
    Undefined = 0,
    Max = 15,
    Indexed = 65536,
    Gdi = 131072,
    Format16bppRgb555 = 135173,
    Format16bppRgb565 = 135174,
    Format24bppRgb = 137224,
    Format32bppRgb = 139273,
    Format1bppIndexed = 196865,
    Format4bppIndexed = 197634,
    Format8bppIndexed = 198659,
    Alpha = 262144,
    Format16bppArgb1555 = 397319,
    PAlpha = 524288,
    Format32bppPArgb = 925707,
    Extended = 1048576,
    Format16bppGrayScale = 1052676,
    Format48bppRgb = 1060876,
    Format64bppPArgb = 1851406,
    Canonical = 2097152,
    Format32bppArgb = 2498570,
    Format64bppArgb = 3424269);

  { TdxNativeImageChangedEvent }

  TdxNativeImageChangedEventArgs = class(TdxEventArgs)
  strict private
    FDesiredImageSizeInTwips: TSize;
  public
    constructor Create(const ADesiredImageSizeInTwips: TSize);

    property DesiredImageSizeInTwips: TSize read FDesiredImageSizeInTwips;
  end;

  TdxNativeImageChangedEvent = procedure(Sender: TObject; E: TdxNativeImageChangedEventArgs) of object;
  TdxNativeImageChangedEventHandler = TdxMulticastMethod<TdxNativeImageChangedEvent>;

  { TdxOfficeImage }

  TdxOfficeImage = class(TdxSmartImage)
  strict private const
{$REGION 'ContentTypeTable'}
    ContentTypeTable: array[TdxOfficeImageFormat] of string = (
      '',
      'image/bitmap',
      'application/x-msmetafile',
      '',
      'image/gif',
      'image/x-icon',
      'image/jpeg',
      '',
      'image/png',
      'image/tiff',
      'application/x-msmetafile'
    );
{$ENDREGION}
{$REGION 'ExtensionTable'}
    ExtensionTable: array[TdxOfficeImageFormat] of string = (
      '',
      'bmp',
      'emf',
      '',
      'gif',
      'ico',
      'jpg',
      '',
      'png',
      'tif',
      'wmf'
    );
{$ENDREGION}
  private
    FCrc32: TdxNullableInteger;
    FDesiredSizeAfterLoad: TSize;
    FIsLoaded: Boolean;
    FShouldSetDesiredSizeAfterLoad: Boolean;
    FSuppressStore: Boolean;
    FSuppressStorePlaceholder: Boolean;
    FUri: string;
    FOnChanged: TdxEventHandler;
    FOnChanging: TdxEventHandler;
    function GetClientRectInImageUnits: TdxRectF;
    function GetPaletteLength: Integer;
    function GetPixelFormat: TdxOfficePixelFormat;
    function GetSizeInImageUnits: TdxSizeF;
    function GetSizeInHundredthsOfMillimeter: TSize;
  strict protected
    function IsBitmapStream(AStream: TStream): Boolean; override;
    class function GetImageStreamDataFormat(AStream: TStream): TdxImageDataFormat;
    function GetUri: string; virtual;
    procedure SetUri(const Value: string); virtual;
    function GetIsLoaded: Boolean; virtual;
    function GetSizeInPixels: TSize; virtual;
    function GetSuppressStore: Boolean; virtual;
    function GetSizeInTwips: TSize; virtual;
    function GetRawFormat: TdxOfficeImageFormat; virtual;
    function GetHorizontalResolution: Single; virtual;
    function GetPhysicalDimension: TdxSizeF; virtual;
    function GetSizeInOriginalUnits: TSize; virtual;
    function GetVerticalResolution: Single; virtual;
    function GetDesiredSizeAfterLoad: TSize; virtual;
    function GetShouldSetDesiredSizeAfterLoad: Boolean; virtual;
    procedure SetDesiredSizeAfterLoad(const Value: TSize); virtual;
    procedure SetShouldSetDesiredSizeAfterLoad(const Value: Boolean); virtual;
    procedure SetSuppressStore(const Value: Boolean); virtual;
  protected
    procedure Changed(Sender: TObject); override;
    procedure CheckCrc32(const Value: Integer);

    procedure CreateHandleFromBitmap(ABitmap: TBitmap); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); override;

    function GetEmfImageBytes: TBytes; virtual;
    function GetWmfImageBytes: TBytes; virtual;
    class function GetBitmapImageBytesStream(ANativeImage: TdxOfficeImage;
      AImageFormat: TdxOfficeImageFormat): TBytesStream; static;

    class function GetColorPalette(AImage: GPImage): TArray<TdxAlphaColor>; static;

    function UnitsToTwips(const ASizeInUnits: TSize): TSize; virtual;
    function UnitsToHundredthsOfMillimeter(const ASizeInUnits: TSize): TSize; virtual;
    function EnsureNonZeroSize(const ASize: TSize): TSize;
  public
    class function CreateImage(const AStream: TStream): TdxOfficeImage; static;

    procedure AssignFromSmartImage(ASmartImage: TdxCustomSmartImage); override;
    function CalculateImageSizeInModelUnits(const AUnitConverter: IdxDocumentModelUnitConverter): TSize; virtual;
    function CanGetImageBytes(AImageFormat: TdxOfficeImageFormat): Boolean; virtual;
    procedure EnsureLoadComplete(ATimeOut: Cardinal = $FFFFFFFF); virtual;
    function GetImageBytesStream(AImageFormat: TdxOfficeImageFormat): TBytesStream; virtual;
    function GetImageBytesStreamSafe(AImageFormat: TdxOfficeImageFormat): TBytesStream;
    function GetImageBytesSafe(AImageFormat: TdxOfficeImageFormat): TArray<Byte>;
    function IsExportSupported(ARawFormat: TdxOfficeImageFormat): Boolean; virtual;
    procedure SetResolution(ADpiX, ADpiY: Single);

    function CalculateCrc32: Integer; virtual;
    function Clone: TdxOfficeImage; reintroduce;
    function GetEmfImageBytesStream: TBytesStream; virtual;
    function GetWmfImageBytesStream: TBytesStream; virtual;

    class function GetContentType(AImageFormat: TdxOfficeImageFormat): string; static; inline;
    class function GetExtension(AImageFormat: TdxOfficeImageFormat): string; static; inline;

    property ClientRectInImageUnits: TdxRectF read GetClientRectInImageUnits;
    property DesiredSizeAfterLoad: TSize read GetDesiredSizeAfterLoad write SetDesiredSizeAfterLoad;
    property IsLoaded: Boolean read GetIsLoaded;
    property RawFormat: TdxOfficeImageFormat read GetRawFormat;
    property ShouldSetDesiredSizeAfterLoad: Boolean read GetShouldSetDesiredSizeAfterLoad write SetShouldSetDesiredSizeAfterLoad;
    property HorizontalResolution: Single read GetHorizontalResolution;
    property PaletteLength: Integer read GetPaletteLength;
    property PixelFormat: TdxOfficePixelFormat read GetPixelFormat;
    property PhysicalDimension: TdxSizeF read GetPhysicalDimension;
    property SizeInOriginalUnits: TSize read GetSizeInOriginalUnits;
    property SizeInImageUnits: TdxSizeF read GetSizeInImageUnits;
    property SizeInPixels: TSize read GetSizeInPixels;
    property SizeInTwips: TSize read GetSizeInTwips;
    property SizeInHundredthsOfMillimeter: TSize read GetSizeInHundredthsOfMillimeter;
    property SuppressStore: Boolean read GetSuppressStore write SetSuppressStore;
    property SuppressStorePlaceholder: Boolean read FSuppressStorePlaceholder write FSuppressStorePlaceholder;
    property Uri: string read GetUri write SetUri;
    property VerticalResolution: Single read GetVerticalResolution;
    property OnChanged: TdxEventHandler read FOnChanged;
    property OnChanging: TdxEventHandler read FOnChanging;
  end;
  TdxOfficeImageClass = class of TdxOfficeImage;

  { TdxOfficeMetafile }

  TdxEmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault = $00000000,
    EmfToWmfBitsFlagsEmbedEmf = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip = $00000004);

  TdxOfficeMetafile = class(TdxOfficeImage)
  private const
    MetafileSizeInHundredthsOfMillimeterInvalidValue: TSize = (cx : -1; cy : -1);
  strict private
    FMetafileSizeInHundredthsOfMillimeter: TSize;
    function GetMetafileSizeInHundredthsOfMillimeter: TSize;
    procedure SetMetafileSizeInHundredthsOfMillimeter(const Value: TSize);
  strict protected
    constructor Create(const AStream: IStream; AReferenceDC: HDC; AType: TdxGpEmfType;
      AFrameRect: TdxGpRect; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PChar); reintroduce; overload;
    constructor Create(AStream: TStream; const ASize: TSize); reintroduce; overload;

    procedure FreeHandle; override;
    function GetHorizontalResolution: Single; override;
    function GetSizeInOriginalUnits: TSize; override;
    function GetVerticalResolution: Single; override;
    function GetWmfImageBytes: TBytes; override;
    function UnitsToTwips(const ASizeInUnits: TSize): TSize; override;
    function UnitsToHundredthsOfMillimeter(const ASizeInUnits: TSize): TSize; override;
  public
    constructor Create; overload; override;
    constructor CreateSize(AWidth, AHeight: Integer; AColor: TdxAlphaColor = 0); overload; override;
    constructor CreateFromStream(AStream: TStream); override;

    function GetHenhmetafile: HMETAFILE;

    function CalculateImageSizeInModelUnits(const AUnitConverter: IdxDocumentModelUnitConverter): TSize; override;
    property MetafileSizeInHundredthsOfMillimeter: TSize read GetMetafileSizeInHundredthsOfMillimeter
      write SetMetafileSizeInHundredthsOfMillimeter;
  end;

  { TdxOfficeWmfImage }

  TdxOfficeWmfImage = class(TdxOfficeMetafile)
  strict protected
    function GetWmfImageBytes: TBytes; override;
  end;

  { TdxOfficeEmfImage }

  TdxOfficeEmfImage = class(TdxOfficeMetafile);

  { TdxOfficeImageReference }

  TdxImageCache = class;

  TdxOfficeImageReference = class(TdxOfficeImage)
  strict private
    FImage: TdxOfficeImage;
    FOwner: TdxImageCache;
    FOnNativeImageChanged: TdxNativeImageChangedEventHandler;
    FOnNativeImageChanging: TdxEventHandler;
  private
    function GetSize: TSize;
  strict protected
    function CreateClone(AOwner: TdxImageCache; AImage: TdxOfficeImage): TdxOfficeImageReference; virtual;
    function GetUri: string; override;
    procedure SetUri(const Value: string); override;
    function GetSuppressStore: Boolean; override;
    function GetRawFormat: TdxOfficeImageFormat; override;
    function GetHorizontalResolution: Single; override;
    function GetVerticalResolution: Single; override;
    function GetSizeInOriginalUnits: TSize; override;
    function GetDesiredSizeAfterLoad: TSize; override;
    function GetShouldSetDesiredSizeAfterLoad: Boolean; override;
    function GetIsLoaded: Boolean; override;
    function GetSizeInPixels: TSize; override;
    function GetSizeInTwips: TSize; override;
    procedure SetDesiredSizeAfterLoad(const Value: TSize); override;
    procedure SetSuppressStore(const Value: Boolean); override;
    procedure SetShouldSetDesiredSizeAfterLoad(const Value: Boolean); override;
    procedure ReplaceImage(AImage: TdxOfficeImage); virtual;
    procedure RaiseNativeImageChanging; virtual;
    procedure RaiseNativeImageChanged(const ADesiredImageSize: TSize); virtual;
    function UnitsToHundredthsOfMillimeter(const ASizeInUnits: TSize): TSize; override;
  protected
    property Owner: TdxImageCache read FOwner;
  public
    constructor Create(AOwner: TdxImageCache; ABaseImage: TdxOfficeImage); reintroduce; virtual;

    function CalculateImageSizeInModelUnits(const AUnitConverter: IdxDocumentModelUnitConverter): TSize; override;
    function CanGetImageBytes(AImageFormat: TdxOfficeImageFormat): Boolean; override;
    function GetImageBytesStream(AImageFormat: TdxOfficeImageFormat): TBytesStream; override;
    function IsExportSupported(ARawFormat: TdxOfficeImageFormat): Boolean; override;

    function GetEmfImageBytesStream: TBytesStream; override;
    function GetWmfImageBytesStream: TBytesStream; override;

    function Clone(AOwner: TdxImageCache): TdxOfficeImageReference; overload;
    function Clone: TdxOfficeImageReference; overload;

    property DesiredSizeAfterLoad: TSize read GetDesiredSizeAfterLoad write SetDesiredSizeAfterLoad;
    property IsLoaded: Boolean read GetIsLoaded;
    property Image: TdxOfficeImage read FImage;
    property RawFormat: TdxOfficeImageFormat read GetRawFormat;
    property ShouldSetDesiredSizeAfterLoad: Boolean read GetShouldSetDesiredSizeAfterLoad write SetShouldSetDesiredSizeAfterLoad;
    property Size: TSize read GetSize;
    property SizeInPixels: TSize read GetSizeInPixels;
    property SizeInTwips: TSize read GetSizeInTwips;
    property SuppressStore: Boolean read GetSuppressStore write SetSuppressStore;
    property Uri: string read GetUri write SetUri;

    property NativeImageChanged: TdxNativeImageChangedEventHandler read FOnNativeImageChanged;
    property NativeImageChanging: TdxEventHandler read FOnNativeImageChanging;
  end;
  TdxOfficeImageReferenceClass = class of TdxOfficeImageReference;

  { TdxImageCache }

  TdxImageCache = class
  private
    FImages: TDictionary<Integer, TdxOfficeImage>;
  protected
    function GetCrc32(AImage: TdxOfficeImage): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddImage(AImage: TdxOfficeImage; ACrc32: Integer);
    procedure Clear;
    function GetImage(ACrc32: Integer): TdxOfficeImage;
  end;

  { TdxMetafilePhysicalDimensionCalculator }

  TdxMetafilePhysicalDimensionCalculator = class sealed
  strict private
    FMapMode: TdxMapMode;
    FWindowExtent: TSize;
  private
    function EnumEnhMetafileCallback(AHdc: HDC; AHandleTable: PHandleTable;
      AMetafileRecord: PMetaRecord; AObjectCount: Integer;
      AClientData: THandle): Integer; stdcall;
    function EnumMetafileCallback(AHdc: HDC; AHandleTable: PHandleTable;
      AMetafileRecord: PMetaRecord; AObjectCount: Integer;
      AClientData: THandle): Integer; stdcall;
  public
    function Calculate(AMetafile: TdxOfficeMetafile; const ABytes: TArray<Byte>): TSize;
    function CalculateWmfPhysicalDimension(AHmf: HMETAFILE; AMetafile: TdxOfficeMetafile): TSize;
    function CalculateEmfPhysicalDimension(AHEmf: HMETAFILE; AMetafile: TdxOfficeMetafile): TSize;
    function ConvertLogicalUnitsToHundredthsOfMillimeter(const ASize: TSize): TSize;
  end;

  { TdxMetafileHelper }

  TdxMetafileHelper = class sealed
  public const
    MetafileResolution = 96;
  protected
    class function TryCreateMetafileCore(AHandle: THandle): TMetafile;
  public
    class function CreateMetafile(AStream: TMemoryStream; AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer): TdxOfficeImage;
    class function TryCreateMetafile(AHandle: THandle): TMetafile;
    class procedure DeleteMetafileHandle(AHandle: THandle);
  end;

  { TdxDibHelper }

  TdxDibHelper = class sealed
  strict private
    class function CreateBmpFileStreamForDib(ADibStream: TStream; ADibHeight, ABytesInLine: Integer): TBytesStream; static;
  public
    class function CreateDib(AStream: TStream; AWidth, AHeight, ABytesInLine: Integer): TdxOfficeImage; static;
  end;

  { TdxBitmapHelper }

  TdxBitmapHelper = class sealed
  public
    class function CreateBitmap(AStream: TStream; AWidth, AHeight, AColorPlanesCount, ABitsPerPixel, ABytesInLine: Integer): TdxOfficeImage; static;
  end;

  { TdxBitmapFileHelper }

  TdxBitmapFileHelper = class sealed
  public const
    SizeofBITMAPFILEHEADER = 14;
    SizeofBITMAPINFOHEADER = 40;
  public
    class procedure WriteBITMAPFILEHEADER(AWriter: TBinaryWriter; AFileSize, ABitsOffset: Integer); static;
    class procedure WriteBITMAPINFOHEADER(AWriter: TBinaryWriter; AWidth, AHeight, AColorPlanesCount, ABitsPerPixel,
       ABytesInLine: Integer; const APalette: TArray<TdxAlphaColor>); static;
    class procedure WritePalette(AWriter: TBinaryWriter; const APalette: TArray<TdxAlphaColor>); static;
  end;

  { TdxImageLoaderHelper }

  TdxImageLoaderHelper = class sealed
  public
    class function ImageFromStream(AStream: TStream): TdxOfficeImage; static;
    class function ImageFromFile(const AFilename: string): TdxOfficeImage; static;
    class function GetStream(const AFileName: string): TStream; static;
    class function GetMemoryStream(AStream: TStream; ALength: Integer): TMemoryStream; static;
    class function IsImageStream(AStream: TStream): Boolean; static;
  end;

  { TdxImageTool }

  TdxImageTool = class sealed
  public
    class function CalculateImageRectCore(AClientRect: TRect; AImageSize: TSize; ASizeMode: TdxImageSizeMode): TdxRectF; static;
  end;

  { IdxUriOfficeImageUpdater }

  IdxUriOfficeImageUpdater = interface
    procedure Add(AReference: TdxUriOfficeImage);
    procedure Remove(AReference: TdxUriOfficeImage);
    procedure Update(AImage: TdxOfficeImage);
    procedure TaskCompeted;
  end;

  { TdxUriOfficeImage }

  TdxUriOfficeImage = class(TdxOfficeImageReference)
  strict private
    class var FPlaceHolder: TdxOfficeImage;
    class function GetPlaceHolder: TdxOfficeImage; static;
  protected
    class procedure FinalizePlaceHolder; static;
  strict private
    FEndLoadingTaskEvent: TSimpleEvent;
    FPixelTargetHeight: Integer;
    FPixelTargetWidth: Integer;
    FSuppressStorePlaceholder: Boolean;
    FUriList: TArray<string>;
    FUpdater: IdxUriOfficeImageUpdater;
    function CreatePlaceHolder(APixelTargetWidth, APixelTargetHeight: Integer): TdxOfficeImage;
    function GetDesiredSizeInTwips: TSize;
  strict protected
    function CreateClone(AOwner: TdxImageCache; AImage: TdxOfficeImage): TdxOfficeImageReference; override;
    class function CreateDefaultPlaceHolder: TdxOfficeImage; virtual;
    procedure ReplaceImage(AImage: TdxOfficeImage); override;
  protected
    procedure OnEndLoadingTask;
    procedure Update(AImage: TdxOfficeImage);

    property EndLoadingTaskEvent: TSimpleEvent read FEndLoadingTaskEvent;
  public
    constructor Create(AOwner: TdxImageCache; const AServiceProvider: IdxServiceProvider; AUriList: TArray<string>;
      APixelTargetWidth, APixelTargetHeight: Integer; AAsyncImageLoading: Boolean); reintroduce;
    destructor Destroy; override;
    procedure EnsureLoadComplete(ATimeOut: Cardinal = $FFFFFFFF); override;

    class property PlaceHolder: TdxOfficeImage read GetPlaceHolder;

    property SuppressStorePlaceholder: Boolean read FSuppressStorePlaceholder write FSuppressStorePlaceholder;
    property PixelTargetHeight: Integer read FPixelTargetHeight;
    property PixelTargetWidth: Integer read FPixelTargetWidth;
  end;

implementation

{$R dxRichEdit.Utils.OfficeImage.res}

uses
  Math, dxTypeHelpers, dxGenerics, dxThreading,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Utils.ThreadSyncService,
  dxRichEdit.Utils.UriStreamService,
  dxMeasurementUnits,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.CheckSumStream;



type
  TdxCustomSmartImageAccess = class(TdxCustomSmartImage);

  TEmfPlusRecordType = (
    EmfMin = 1,
    EmfHeader = 1,
    EmfPolyBezier = 2,
    EmfPolygon = 3,
    EmfPolyline = 4,
    EmfPolyBezierTo = 5,
    EmfPolyLineTo = 6,
    EmfPolyPolyline = 7,
    EmfPolyPolygon = 8,
    EmfSetWindowExtEx = 9,
    EmfSetWindowOrgEx = 10,
    EmfSetViewportExtEx = 11,
    EmfSetViewportOrgEx = 12,
    EmfSetBrushOrgEx = 13,
    EmfEof = 14,
    EmfSetPixelV = 15,
    EmfSetMapperFlags = 16,
    EmfSetMapMode = 17,
    EmfSetBkMode = 18,
    EmfSetPolyFillMode = 19,
    EmfSetROP2 = 20,
    EmfSetStretchBltMode = 21,
    EmfSetTextAlign = 22,
    EmfSetColorAdjustment = 23,
    EmfSetTextColor = 24,
    EmfSetBkColor = 25,
    EmfOffsetClipRgn = 26,
    EmfMoveToEx = 27,
    EmfSetMetaRgn = 28,
    EmfExcludeClipRect = 29,
    EmfIntersectClipRect = 30,
    EmfScaleViewportExtEx = 31,
    EmfScaleWindowExtEx = 32,
    EmfSaveDC = 33,
    EmfRestoreDC = 34,
    EmfSetWorldTransform = 35,
    EmfModifyWorldTransform = 36,
    EmfSelectObject = 37,
    EmfCreatePen = 38,
    EmfCreateBrushIndirect = 39,
    EmfDeleteObject = 40,
    EmfAngleArc = 41,
    EmfEllipse = 42,
    EmfRectangle = 43,
    EmfRoundRect = 44,
    EmfRoundArc = 45,
    EmfChord = 46,
    EmfPie = 47,
    EmfSelectPalette = 48,
    EmfCreatePalette = 49,
    EmfSetPaletteEntries = 50,
    EmfResizePalette = 51,
    EmfRealizePalette = 52,
    EmfExtFloodFill = 53,
    EmfLineTo = 54,
    EmfArcTo = 55,
    EmfPolyDraw = 56,
    EmfSetArcDirection = 57,
    EmfSetMiterLimit = 58,
    EmfBeginPath = 59,
    EmfEndPath = 60,
    EmfCloseFigure = 61,
    EmfFillPath = 62,
    EmfStrokeAndFillPath = 63,
    EmfStrokePath = 64,
    EmfFlattenPath = 65,
    EmfWidenPath = 66,
    EmfSelectClipPath = 67,
    EmfAbortPath = 68,
    EmfReserved069 = 69,
    EmfGdiComment = 70,
    EmfFillRgn = 71,
    EmfFrameRgn = 72,
    EmfInvertRgn = 73,
    EmfPaintRgn = 74,
    EmfExtSelectClipRgn = 75,
    EmfBitBlt = 76,
    EmfStretchBlt = 77,
    EmfMaskBlt = 78,
    EmfPlgBlt = 79,
    EmfSetDIBitsToDevice = 80,
    EmfStretchDIBits = 81,
    EmfExtCreateFontIndirect = 82,
    EmfExtTextOutA = 83,
    EmfExtTextOutW = 84,
    EmfPolyBezier16 = 85,
    EmfPolygon16 = 86,
    EmfPolyline16 = 87,
    EmfPolyBezierTo16 = 88,
    EmfPolylineTo16 = 89,
    EmfPolyPolyline16 = 90,
    EmfPolyPolygon16 = 91,
    EmfPolyDraw16 = 92,
    EmfCreateMonoBrush = 93,
    EmfCreateDibPatternBrushPt = 94,
    EmfExtCreatePen = 95,
    EmfPolyTextOutA = 96,
    EmfPolyTextOutW = 97,
    EmfSetIcmMode = 98,
    EmfCreateColorSpace = 99,
    EmfSetColorSpace = 100,
    EmfDeleteColorSpace = 101,
    EmfGlsRecord = 102,
    EmfGlsBoundedRecord = 103,
    EmfPixelFormat = 104,
    EmfDrawEscape = 105,
    EmfExtEscape = 106,
    EmfStartDoc = 107,
    EmfSmallTextOut = 108,
    EmfForceUfiMapping = 109,
    EmfNamedEscpae = 110,
    EmfColorCorrectPalette = 111,
    EmfSetIcmProfileA = 112,
    EmfSetIcmProfileW = 113,
    EmfAlphaBlend = 114,
    EmfSetLayout = 115,
    EmfTransparentBlt = 116,
    EmfReserved117 = 117,
    EmfGradientFill = 118,
    EmfSetLinkedUfis = 119,
    EmfSetTextJustification = 120,
    EmfColorMatchToTargetW = 121,
    EmfMax = 122,
    EmfCreateColorSpaceW = 122,
    Invalid = 16384,
    EmfPlusRecordBase = 16384,
    Min = 16385,
    Header = 16385,
    EndOfFile = 16386,
    Comment = 16387,
    GetDC = 16388,
    MultiFormatStart = 16389,
    MultiFormatSection = 16390,
    MultiFormatEnd = 16391,
    &Object = 16392,
    Clear = 16393,
    FillRects = 16394,
    DrawRects = 16395,
    FillPolygon = 16396,
    DrawLines = 16397,
    FillEllipse = 16398,
    DrawEllipse = 16399,
    FillPie = 16400,
    DrawPie = 16401,
    DrawArc = 16402,
    FillRegion = 16403,
    FillPath = 16404,
    DrawPath = 16405,
    FillClosedCurve = 16406,
    DrawClosedCurve = 16407,
    DrawCurve = 16408,
    DrawBeziers = 16409,
    DrawImage = 16410,
    DrawImagePoints = 16411,
    DrawString = 16412,
    SetRenderingOrigin = 16413,
    SetAntiAliasMode = 16414,
    SetTextRenderingHint = 16415,
    SetTextContrast = 16416,
    SetInterpolationMode = 16417,
    SetPixelOffsetMode = 16418,
    SetCompositingMode = 16419,
    SetCompositingQuality = 16420,
    Save = 16421,
    Restore = 16422,
    BeginContainer = 16423,
    BeginContainerNoParams = 16424,
    EndContainer = 16425,
    SetWorldTransform = 16426,
    ResetWorldTransform = 16427,
    MultiplyWorldTransform = 16428,
    TranslateWorldTransform = 16429,
    ScaleWorldTransform = 16430,
    RotateWorldTransform = 16431,
    SetPageTransform = 16432,
    ResetClip = 16433,
    SetClipRect = 16434,
    SetClipPath = 16435,
    SetClipRegion = 16436,
    OffsetClip = 16437,
    Max = 16438,
    DrawDriverString = 16438,
    Total = 16439,
    WmfRecordBase = 65536,
    WmfSaveDC = 65566,
    WmfRealizePalette = 65589,
    WmfSetPalEntries = 65591,
    WmfCreatePalette = 65783,
    WmfSetBkMode = 65794,
    WmfSetMapMode = 65795,
    WmfSetROP2 = 65796,
    WmfSetRelAbs = 65797,
    WmfSetPolyFillMode = 65798,
    WmfSetStretchBltMode = 65799,
    WmfSetTextCharExtra = 65800,
    WmfRestoreDC = 65831,
    WmfInvertRegion = 65834,
    WmfPaintRegion = 65835,
    WmfSelectClipRegion = 65836,
    WmfSelectObject = 65837,
    WmfSetTextAlign = 65838,
    WmfResizePalette = 65849,
    WmfDibCreatePatternBrush = 65858,
    WmfSetLayout = 65865,
    WmfDeleteObject = 66032,
    WmfCreatePatternBrush = 66041,
    WmfSetBkColor = 66049,
    WmfSetTextColor = 66057,
    WmfSetTextJustification = 66058,
    WmfSetWindowOrg = 66059,
    WmfSetWindowExt = 66060,
    WmfSetViewportOrg = 66061,
    WmfSetViewportExt = 66062,
    WmfOffsetWindowOrg = 66063,
    WmfOffsetViewportOrg = 66065,
    WmfLineTo = 66067,
    WmfMoveTo = 66068,
    WmfOffsetCilpRgn = 66080,
    WmfFillRegion = 66088,
    WmfSetMapperFlags = 66097,
    WmfSelectPalette = 66100,
    WmfCreatePenIndirect = 66298,
    WmfCreateFontIndirect = 66299,
    WmfCreateBrushIndirect = 66300,
    WmfPolygon = 66340,
    WmfPolyline = 66341,
    WmfScaleWindowExt = 66576,
    WmfScaleViewportExt = 66578,
    WmfExcludeClipRect = 66581,
    WmfIntersectClipRect = 66582,
    WmfEllipse = 66584,
    WmfFloodFill = 66585,
    WmfRectangle = 66587,
    WmfSetPixel = 66591,
    WmfFrameRegion = 66601,
    WmfAnimatePalette = 66614,
    WmfTextOut = 66849,
    WmfPolyPolygon = 66872,
    WmfExtFloodFill = 66888,
    WmfRoundRect = 67100,
    WmfPatBlt = 67101,
    WmfEscape = 67110,
    WmfCreateRegion = 67327,
    WmfArc = 67607,
    WmfPie = 67610,
    WmfChord = 67632,
    WmfBitBlt = 67874,
    WmfDibBitBlt = 67904,
    WmfExtTextOut = 68146,
    WmfStretchBlt = 68387,
    WmfDibStretchBlt = 68417,
    WmfSetDibToDev = 68915,
    WmfStretchDib = 69443);

  { TdxStreamHelper }

  TdxStreamHelper = class sealed
  public
    class procedure WriteTo(AInputStream: TStream; AOutputStream: TStream); static;
  end;

  { TdxPaletteHelper }

  TdxPaletteHelper = class
  strict private
    class var
      FInitialized: Boolean;
      FPalette0: TArray<TdxAlphaColor>;
      FPalette2: TArray<TdxAlphaColor>;
      FPalette16: TArray<TdxAlphaColor>;
      FPalette256: TArray<TdxAlphaColor>;
    class procedure Initialize; static;
  strict private
    class function PreparePalette256: TArray<TdxAlphaColor>; static;
    class function PreparePalette16: TArray<TdxAlphaColor>; static;
    class function PreparePalette2: TArray<TdxAlphaColor>; static;
    class function PreparePalette(APixelFormat: TdxOfficePixelFormat): TArray<TdxAlphaColor>; static;
  public
    class function GetPalette(ABitsPerPixel: Integer): TArray<TdxAlphaColor>; static;
  end;

  { TdxUriOfficeImageUpdater }

  TdxUriOfficeImageUpdater = class(TInterfacedObject, IdxUriOfficeImageUpdater)
  strict private
    FList: TdxList<TdxUriOfficeImage>;
  protected
    // IdxUriOfficeImageUpdater
    procedure Add(AReference: TdxUriOfficeImage);
    procedure Remove(AReference: TdxUriOfficeImage);
    procedure Update(AImage: TdxOfficeImage);
    procedure TaskCompeted;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxUriOfficeImageLoadingTask }

  TdxUriOfficeImageLoadingTask = class(TInterfacedObject, IdxTask)
  strict private
    FEndTaskEvent: TSimpleEvent;
    FImage: TdxOfficeImage;
    FUri: TArray<string>;
    FUriService: IdxUriStreamService;
    FUpdater: IdxUriOfficeImageUpdater;
  public
    constructor Create(const AUriService: IdxUriStreamService;
      const AUpdater: IdxUriOfficeImageUpdater; const AUri: TArray<string>; AEndTaskEvent: TSimpleEvent);
    destructor Destroy; override;

    function Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
  end;

{ TdxUriOfficeImageUpdater }

constructor TdxUriOfficeImageUpdater.Create;
begin
  inherited Create;
  FList := TdxList<TdxUriOfficeImage>.Create;
end;

destructor TdxUriOfficeImageUpdater.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxUriOfficeImageUpdater.Add(AReference: TdxUriOfficeImage);
begin
  FList.Add(AReference);
end;

procedure TdxUriOfficeImageUpdater.Remove(AReference: TdxUriOfficeImage);
begin
  FList.Remove(AReference);
end;

procedure TdxUriOfficeImageUpdater.TaskCompeted;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FList[I].OnEndLoadingTask;
end;

procedure TdxUriOfficeImageUpdater.Update(AImage: TdxOfficeImage);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FList[I].Update(AImage);
end;

{ TdxUriOfficeImageLoadingTask }

constructor TdxUriOfficeImageLoadingTask.Create(const AUriService: IdxUriStreamService;
  const AUpdater: IdxUriOfficeImageUpdater; const AUri: TArray<string>; AEndTaskEvent: TSimpleEvent);
begin
  inherited Create;
  FUriService := AUriService;
  FUpdater := AUpdater;
  FUri := AUri;
  FEndTaskEvent := AEndTaskEvent;
end;

destructor TdxUriOfficeImageLoadingTask.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TdxUriOfficeImageLoadingTask.Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AStream: TStream;
  AUri: string;
  I: Integer;
  AUriService2: IdxUriStreamService2;
begin
  if not Supports(FUriService, IdxUriStreamService2, AUriService2) then
    AUriService2 := nil;
  try
    Result := TdxTaskCompletedStatus.Success;
    try
      AStream := nil;
      for I := 0 to Length(FUri) - 1 do
      begin
        AUri := FUri[I];
        if AUriService2 <> nil then
          AStream := AUriService2.AsyncGetStream(AUri, ACancelStatus)
        else
          AStream := FUriService.GetStream(AUri);
        if AStream <> nil then
          Break;
        if ACancelStatus then
          Exit(TdxTaskCompletedStatus.Cancelled);
      end;
      try
        if AStream = nil then
          Exit(TdxTaskCompletedStatus.Fail);
        if not TdxImageLoaderHelper.IsImageStream(AStream) then
          Exit(TdxTaskCompletedStatus.Fail);
        AStream.Position := 0;
        FImage := TdxOfficeImage.CreateFromStream(AStream);
      finally
        AStream.Free;
      end;
    except
      Result := TdxTaskCompletedStatus.Fail;
    end;
  finally
    FUpdater.TaskCompeted;
  end;
end;

procedure TdxUriOfficeImageLoadingTask.OnComplete(AStatus: TdxTaskCompletedStatus);
begin
  if AStatus = TdxTaskCompletedStatus.Success then
  begin
    Assert(GetCurrentThreadID = MainThreadID);
    FUpdater.Update(FImage);
  end;
  FreeAndNil(FImage);
end;

{ TdxOfficeImage }

class function TdxOfficeImage.CreateImage(const AStream: TStream): TdxOfficeImage;
var
  AOldPosition: Integer;
  ADataFormat: TdxImageDataFormat;
begin
  ADataFormat := GetImageStreamDataFormat(AStream);
  AOldPosition := AStream.Position;
  case ADataFormat of
    dxImageUnknown:
      Exit(nil);
    dxImageEmf:
      Result := TdxOfficeEmfImage.Create;
    dxImageWmf:
      Result := TdxOfficeWmfImage.Create;
  else
    Result := TdxOfficeImage.Create;
  end;
  try
    Result.LoadFromStream(AStream);
  except
    AStream.Position := AOldPosition;
    FreeAndNil(Result);
  end;
end;

procedure TdxOfficeImage.AssignFromSmartImage(ASmartImage: TdxCustomSmartImage);
var
  AImage: TdxOfficeImage;
  AImageHandle: GpImage;
begin
  FreeHandle;
  if ASmartImage.HandleAllocated and (ASmartImage.ImageDataFormat = dxImagePng) and (ASmartImage is TdxGpImage) then
  begin
    GdipCheck(GdipCloneImage(TdxGpImage(ASmartImage).Handle, AImageHandle));
    Handle := AImageHandle;
  end
  else
    inherited AssignFromSmartImage(ASmartImage);

  AImage := Safe<TdxOfficeImage>.Cast(ASmartImage);
  if AImage <> nil then
  begin
    FCrc32 := AImage.CalculateCrc32;
    FDesiredSizeAfterLoad := AImage.FDesiredSizeAfterLoad;
    FIsLoaded := AImage.FIsLoaded;
    FShouldSetDesiredSizeAfterLoad := AImage.FShouldSetDesiredSizeAfterLoad;
    FSuppressStore := AImage.FSuppressStore;
    FSuppressStorePlaceholder := AImage.FSuppressStorePlaceholder;
    FUri := AImage.FUri;
    GdipImageForceValidation(Handle);
  end;
end;

function TdxOfficeImage.CalculateCrc32: Integer;
var
  AStream: TStream;
  ADataStream: TdxCrc32Stream;
begin
  if FCrc32.IsNull then
  begin
    AStream := TMemoryStream.Create;
    try
      ADataStream := TdxCrc32Stream.Create(AStream);
      try
        SaveToStream(ADataStream);
        FCrc32 := ADataStream.WriteCheckSum;
      finally
        ADataStream.Free;
      end;
    finally
      AStream.Free;
    end;
  end;
  Result := FCrc32;
end;

function TdxOfficeImage.CalculateImageSizeInModelUnits(const AUnitConverter: IdxDocumentModelUnitConverter): TSize;
begin
  Result := EnsureNonZeroSize(AUnitConverter.PixelsToModelUnits(SizeInOriginalUnits, HorizontalResolution, VerticalResolution));
end;

function TdxOfficeImage.CanGetImageBytes(AImageFormat: TdxOfficeImageFormat): Boolean;
begin
  Result := True;
end;

procedure TdxOfficeImage.EnsureLoadComplete(ATimeOut: Cardinal = $FFFFFFFF);
begin
end;

function TdxOfficeImage.Clone: TdxOfficeImage;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxOfficeImage(inherited Clone);
end;

function TdxOfficeImage.UnitsToHundredthsOfMillimeter(
  const ASizeInUnits: TSize): TSize;
begin
  Result := EnsureNonZeroSize(PixelsToHundredthsOfMillimeter(ASizeInUnits, HorizontalResolution, VerticalResolution));
end;

function TdxOfficeImage.UnitsToTwips(const ASizeInUnits: TSize): TSize;
begin
  Result := EnsureNonZeroSize(PixelsToTwips(ASizeInUnits, HorizontalResolution, VerticalResolution));
end;

function TdxOfficeImage.EnsureNonZeroSize(const ASize: TSize): TSize;
begin
  Result.Init(Max(1, ASize.Width), Max(1, ASize.Height));
end;

function TdxOfficeImage.GetClientRectInImageUnits: TdxRectF;
var
  ASize: TdxSizeF;
begin
  ASize := SizeInImageUnits;
  Result.InitSize(0, 0, ASize.cx, ASize.cy);
end;

function TdxOfficeImage.GetPaletteLength: Integer;
begin
  Result := Length(GetColorPalette(Handle));
end;

function TdxOfficeImage.GetPixelFormat: TdxOfficePixelFormat;
var
  AResult: TdxGpPixelFormat;
  AStatus: GpStatus;
begin
  AStatus := GdipGetImagePixelFormat(Handle, AResult);
  if AStatus <> Ok then
    Result := TdxOfficePixelFormat.Undefined
  else
    Result := TdxOfficePixelFormat(AResult);
end;

function TdxOfficeImage.GetEmfImageBytes: TBytes;
var
  AMetafile: TdxOfficeMetafile;
  ALength: Integer;
  AEmf: HMETAFILE;
begin
  AMetafile := TdxOfficeMetafile.Create;
  try
    AMetafile.AssignFromSmartImage(Self);
    AEmf := AMetafile.GetHenhmetafile;
    try
      ALength := GetEnhMetaFileBits(AEmf, 0, nil);
      if ALength = 0 then
        Exit;
      SetLength(Result, ALength);
      GetEnhMetaFileBits(AEmf, ALength, @Result[0]);
    finally
      DeleteObject(AEmf);
    end;
  finally
    AMetafile.Free;
  end;
end;

function TdxOfficeImage.GetWmfImageBytes: TBytes;
var
  AMetafile: TdxOfficeMetafile;
  ACanvas: TdxGPCanvas;
  AEmf: HMETAFILE;
  ALength: Integer;
begin
  AMetafile := TdxOfficeMetafile.CreateSize(SizeInPixels.Width + 1, SizeInPixels.Height + 1);
  try
    ACanvas := AMetafile.CreateCanvas;
    try
      ACanvas.Draw(Self, ClientRect);
    finally
      ACanvas.Free;
    end;
    AEmf := AMetafile.GetHenhmetafile;
    try
      ALength := GdipEmfToWmfBits(AEmf, 0, nil, MM_ANISOTROPIC, Ord(TdxEmfToWmfBitsFlags.EmfToWmfBitsFlagsDefault));
      if ALength = 0 then
        Exit;
      SetLength(Result, ALength);
      GdipEmfToWmfBits(AEmf, ALength, @Result[0], MM_ANISOTROPIC, Ord(TdxEmfToWmfBitsFlags.EmfToWmfBitsFlagsDefault));
    finally
      DeleteObject(AEmf);
    end;
  finally
    AMetafile.Free;
  end;
end;

class function TdxOfficeImage.GetBitmapImageBytesStream(ANativeImage: TdxOfficeImage;
  AImageFormat: TdxOfficeImageFormat): TBytesStream;
const
  AImageDataFormatMap: array[TdxOfficeImageFormat] of TdxImageDataFormat =
    (dxImageUnknown, dxImageBitmap, dxImageEmf, dxImageExif, dxImageGif,
    dxImageIcon, dxImageJpeg, dxImageMemoryBmp, dxImagePng, dxImageTiff,
    dxImageWmf);
begin
  Result := TBytesStream.Create;
  ANativeImage.SaveToStreamByCodec(Result, AImageDataFormatMap[AImageFormat]);
  Result.Position := 0;
end;

class function TdxOfficeImage.GetColorPalette(AImage: GPImage): TArray<TdxAlphaColor>;
var
  APalette: PGpColorPalette;
  AStatus: GpStatus;
  ASize: Integer;
  I: Integer;
begin
  Result := nil;
  AStatus := GdipGetImagePaletteSize(AImage, ASize);
  if (AStatus <> Ok) or (ASize = 0) then
    Exit;
  GetMem(APalette, ASize);
  try
    AStatus := GdipGetImagePalette(AImage, APalette, ASize);
    if AStatus <> Ok then
      Exit;
    SetLength(Result, APalette.Count);
    for I := 0 to APalette.Count - 1 do
    begin
      Result[I] := TdxAlphaColor(APalette.Entries);
      Inc(APalette.Entries);
    end;
  finally
    FreeMem(APalette);
  end;
end;

function TdxOfficeImage.GetEmfImageBytesStream: TBytesStream;
var
  ABytes: TBytes;
begin
  ABytes := GetEmfImageBytes;
  try
    Result := TBytesStream.Create(ABytes);
  finally
    SetLength(ABytes, 0);
  end;
end;

function TdxOfficeImage.IsBitmapStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxOfficeImage.GetImageStreamDataFormat(AStream: TStream): TdxImageDataFormat;
var
  AOldPosition: Cardinal;
  AHandle: GPImage;
  AStreamAdapter: IStream;
  AFormatID: TGUID;
begin
  AOldPosition := AStream.Position;
  try
    AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
    try
      GdipCheck(GdipLoadImageFromStream(AStreamAdapter, AHandle));
      try
        GdipCheck(GdipGetImageRawFormat(AHandle, AFormatID));
        Result := dxGetImageDataFormat(AFormatID);
      finally
        GdipDisposeImage(AHandle);
      end;
    finally
      AStreamAdapter := nil;
    end;
  finally
    AStream.Position := AOldPosition;
  end;
end;

procedure TdxOfficeImage.Changed(Sender: TObject);
begin
  inherited Changed(Sender);
  if not FOnChanged.Empty then
    FOnChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxOfficeImage.CheckCrc32(const Value: Integer);
begin
  FCrc32 := Value;
end;

procedure TdxOfficeImage.CreateHandleFromBitmap(ABitmap: TBitmap);
var
  AColors: TRGBColors;
begin
  ImageData.Transparent := ImageData.Transparent or ABitmap.Transparent;
  ImageData.TransparentColor := ABitmap.TransparentColor;
  ImageData.TransparentMode := ABitmap.TransparentMode;
  if dxCoreGraphics.GetBitmapBits(ABitmap, AColors, True) then
  begin
    CreateHandleFromBits(ABitmap.Width, ABitmap.Height, AColors,
       afIgnored
      );
  end;
end;

procedure TdxOfficeImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  inherited Progress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
  if (Stage = psStarting) and not FOnChanging.Empty then
    FOnChanging.Invoke(Self, TdxEventArgs.Empty);
end;

function TdxOfficeImage.GetHorizontalResolution: Single;
begin
  if (Handle = nil) or Empty or (GdipGetImageHorizontalResolution(Handle, Result) <> Ok) then
    Result := 96;
end;

function TdxOfficeImage.GetImageBytesStream(
  AImageFormat: TdxOfficeImageFormat): TBytesStream;
begin
  if AImageFormat = TdxOfficeImageFormat.Wmf then
    Result := GetWmfImageBytesStream
  else
  if AImageFormat = TdxOfficeImageFormat.Emf then
    Result := GetEmfImageBytesStream
  else
    Result := GetBitmapImageBytesStream(Self, AImageFormat);
end;

function TdxOfficeImage.GetImageBytesStreamSafe(AImageFormat: TdxOfficeImageFormat): TBytesStream;
begin
  try
    Result := GetImageBytesStream(AImageFormat);
  except
    Result := GetImageBytesStream(TdxOfficeImageFormat.Png);
  end;
end;

function TdxOfficeImage.GetImageBytesSafe(AImageFormat: TdxOfficeImageFormat): TArray<Byte>;
var
  AStream: TBytesStream;
  ASize: Integer;
begin
  AStream := GetImageBytesStreamSafe(AImageFormat);
  try
    ASize := AStream.Size;
    SetLength(Result, ASize);
    Move(AStream.Bytes[0], Result[0], ASize);
  finally
    AStream.Free;
  end;
end;

function TdxOfficeImage.GetPhysicalDimension: TdxSizeF;
var
  AResult: TdxGPSizeF;
begin
  if (Handle = nil) or Empty or (GdipGetImageDimension(Handle, AResult.Width, AResult.Height) <> Ok) then
    AResult := MakeSize(0.0, 0.0);
  Result := TdxSizeF.Create(AResult.Width, AResult.Height);
end;

function TdxOfficeImage.GetRawFormat: TdxOfficeImageFormat;
const
  ResultMap: array[TdxImageDataFormat] of TdxOfficeImageFormat = (
    TdxOfficeImageFormat.None, TdxOfficeImageFormat.Bmp, TdxOfficeImageFormat.Jpeg, TdxOfficeImageFormat.Png,
    TdxOfficeImageFormat.Tiff, TdxOfficeImageFormat.Gif, TdxOfficeImageFormat.Emf, TdxOfficeImageFormat.Exif,
    TdxOfficeImageFormat.Icon, TdxOfficeImageFormat.MemoryBmp, TdxOfficeImageFormat.Wmf);
begin
  Result := ResultMap[ImageDataFormat];
end;

function TdxOfficeImage.GetSizeInOriginalUnits: TSize;
begin
  Result := cxSize(Trunc(PhysicalDimension.Width), Trunc(PhysicalDimension.Height));
end;

function TdxOfficeImage.GetSizeInTwips: TSize;
begin
  Result := UnitsToTwips(SizeInOriginalUnits);
end;

function TdxOfficeImage.GetSuppressStore: Boolean;
begin
  Result := FSuppressStore or (SuppressStorePlaceholder and not IsLoaded);
end;

function TdxOfficeImage.GetSizeInHundredthsOfMillimeter: TSize;
begin
  Result := UnitsToHundredthsOfMillimeter(SizeInOriginalUnits);
end;

function TdxOfficeImage.GetSizeInImageUnits: TdxSizeF;
var
  ASrcRect: TdxGpRectF;
  ASrcUnit: GpUNIT;
begin
  if GdipGetImageBounds(Handle, @ASrcRect, ASrcUnit) = Ok then
  begin
    Result.cx := ASrcRect.Width;
    Result.cy := ASrcRect.Height;
  end
  else
    Result := dxSizeF(Size.cx, Size.cy);
end;

function TdxOfficeImage.GetUri: string;
begin
  Result := FUri;
end;

procedure TdxOfficeImage.SetUri(const Value: string);
begin
  FUri := Value;
end;

function TdxOfficeImage.GetIsLoaded: Boolean;
begin
  Result := FIsLoaded;
end;

function TdxOfficeImage.GetSizeInPixels: TSize;
begin
  Result := Size;
end;

function TdxOfficeImage.GetVerticalResolution: Single;
begin
  if (Handle = nil) or Empty or (GdipGetImageVerticalResolution(Handle, Result) <> Ok) then
    Result := 96;
end;

function TdxOfficeImage.GetDesiredSizeAfterLoad: TSize;
begin
  Result := FDesiredSizeAfterLoad;
end;

function TdxOfficeImage.GetShouldSetDesiredSizeAfterLoad: Boolean;
begin
  Result := FShouldSetDesiredSizeAfterLoad;
end;

procedure TdxOfficeImage.SetDesiredSizeAfterLoad(const Value: TSize);
begin
  FDesiredSizeAfterLoad := Value;
end;

procedure TdxOfficeImage.SetShouldSetDesiredSizeAfterLoad(const Value: Boolean);
begin
  FShouldSetDesiredSizeAfterLoad := Value;
end;

procedure TdxOfficeImage.SetSuppressStore(const Value: Boolean);
begin
  FSuppressStore := Value;
end;

function TdxOfficeImage.GetWmfImageBytesStream: TBytesStream;
var
  ABytes: TBytes;
begin
  ABytes := GetWmfImageBytes;
  try
    Result := TBytesStream.Create(ABytes);
  finally
    SetLength(ABytes, 0);
  end;
end;

class function TdxOfficeImage.GetContentType(AImageFormat: TdxOfficeImageFormat): string;
begin
  Result := ContentTypeTable[AImageFormat];
end;

class function TdxOfficeImage.GetExtension(AImageFormat: TdxOfficeImageFormat): string;
begin
  Result := ExtensionTable[AImageFormat];
end;

function TdxOfficeImage.IsExportSupported(
  ARawFormat: TdxOfficeImageFormat): Boolean;
begin
  Result := True;
end;

procedure TdxOfficeImage.SetResolution(ADpiX, ADpiY: Single);
begin
  if (HorizontalResolution = ADpiX) and (VerticalResolution = ADpiY) then
    Exit;
  GdipCheck(GdipBitmapSetResolution(Handle, ADpiX, ADpiY));
  Changed(Self);
end;

{ TdxOfficeMetafile }

constructor TdxOfficeMetafile.Create;
begin
  inherited Create;
  FMetafileSizeInHundredthsOfMillimeter := MetafileSizeInHundredthsOfMillimeterInvalidValue;
end;

constructor TdxOfficeMetafile.Create(const AStream: IStream; AReferenceDC: HDC;
  AType: TdxGpEmfType; AFrameRect: TdxGpRect; AFrameUnit: TdxGpMetafileFrameUnit; ADescription: PChar);
var
  AImageHandle: GpImage;
begin
  Create;
  if AStream = nil then
    GdipCheck(GdipRecordMetafileI(AReferenceDC, AType, @AFrameRect, AFrameUnit, ADescription, AImageHandle))
  else
    GdipCheck(GdipRecordMetafileStreamI(AStream, AReferenceDC, AType, @AFrameRect, AFrameUnit, ADescription, AImageHandle));

  Handle := AImageHandle;
end;

constructor TdxOfficeMetafile.Create(AStream: TStream; const ASize: TSize);
var
  AStreamAdapter: IStream;
  AGraphics: TdxGraphics;
  AGPRect: TdxGPRect;
  AReferenceDC: HDC;
  DC: HDC;
begin
  if AStream = nil then
    AStreamAdapter := nil
  else
    AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
  try
    DC := GetDC(0);
    try
      AGraphics := TdxGraphics.CreateFromHdc(DC);
      try
        AGPRect := TRect.CreateSize(ASize);
        AReferenceDC := AGraphics.GetHDC;
        try
          Create(AStreamAdapter, AReferenceDC, EmfTypeEmfPlusDual, AGPRect, MetafileFrameUnitPixel, nil);
        finally
          AGraphics.ReleaseHDC(AReferenceDC);
        end;
      finally
        AGraphics.Free;
      end;
    finally
      ReleaseDC(0, DC);
    end;
  finally
    AStreamAdapter := nil;
  end;
end;

constructor TdxOfficeMetafile.CreateSize(AWidth, AHeight: Integer; AColor: TdxAlphaColor = 0);
begin
  Create(nil, cxSize(AWidth, AHeight));
end;

constructor TdxOfficeMetafile.CreateFromStream(AStream: TStream);
var
  AImageHandle: GpImage;
  AStreamAdapter: IStream;
begin
  Create;
  AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
  try
    GdipCheck(GdipCreateMetafileFromStream(AStreamAdapter, AImageHandle));
    Handle := AImageHandle;
  finally
    AStreamAdapter := nil;
  end;
end;

function TdxOfficeMetafile.GetHenhmetafile: HMETAFILE;
begin
  Result := 0;
  GdipCheck(GdipGetHemfFromMetafile(Handle, Result));
end;

function TdxOfficeMetafile.CalculateImageSizeInModelUnits(const AUnitConverter: IdxDocumentModelUnitConverter): TSize;
begin
  Result := EnsureNonZeroSize(AUnitConverter.HundredthsOfMillimeterToModelUnits(SizeInOriginalUnits));
end;

procedure TdxOfficeMetafile.FreeHandle;
begin
  inherited FreeHandle;
  FMetafileSizeInHundredthsOfMillimeter := MetafileSizeInHundredthsOfMillimeterInvalidValue;
end;

function TdxOfficeMetafile.GetHorizontalResolution: Single;
begin
  Result := TdxMetafileHelper.MetafileResolution;
end;

function TdxOfficeMetafile.GetSizeInOriginalUnits: TSize;
begin
  Result := MetafileSizeInHundredthsOfMillimeter;
end;

function TdxOfficeMetafile.GetVerticalResolution: Single;
begin
  Result := TdxMetafileHelper.MetafileResolution;
end;

function TdxOfficeMetafile.GetWmfImageBytes: TBytes;
var
  ALength: Integer;
  AEmf: HMETAFILE;
  AMetafile: TdxOfficeMetafile;
begin
  AMetafile := TdxOfficeMetafile(Clone);
  try
    AEmf := AMetafile.GetHenhmetafile;
    try
      ALength := GdipEmfToWmfBits(AEmf, 0, nil, MM_ANISOTROPIC, Ord(TdxEmfToWmfBitsFlags.EmfToWmfBitsFlagsDefault));
      if ALength = 0 then
        Exit;
      SetLength(Result, ALength);
      GdipEmfToWmfBits(AEmf, ALength, @Result[0], MM_ANISOTROPIC, Ord(TdxEmfToWmfBitsFlags.EmfToWmfBitsFlagsDefault));
    finally
      DeleteObject(AEmf);
    end;
  finally
    AMetafile.Free;
  end;
end;

function TdxOfficeMetafile.UnitsToTwips(const ASizeInUnits: TSize): TSize;
begin
  Result := EnsureNonZeroSize(HundredthsOfMillimeterToTwips(ASizeInUnits));
end;

function TdxOfficeMetafile.UnitsToHundredthsOfMillimeter(const ASizeInUnits: TSize): TSize;
begin
  Result := ASizeInUnits;
end;

function TdxOfficeMetafile.GetMetafileSizeInHundredthsOfMillimeter: TSize;
begin
  if FMetafileSizeInHundredthsOfMillimeter.IsEqual(MetafileSizeInHundredthsOfMillimeterInvalidValue) then
  begin
    HandleNeeded;
    if HandleAllocated then
      FMetafileSizeInHundredthsOfMillimeter := EnsureNonZeroSize(TSize.Round(PhysicalDimension));
  end;
  Result := FMetafileSizeInHundredthsOfMillimeter;
end;

procedure TdxOfficeMetafile.SetMetafileSizeInHundredthsOfMillimeter(const Value: TSize);
begin
  FMetafileSizeInHundredthsOfMillimeter := Value;
end;

{ TdxOfficeWmfImage }

function TdxOfficeWmfImage.GetWmfImageBytes: TBytes;
var
  AMetafile: TdxOfficeMetafile;
  AEmf: HMETAFILE;
  ALength: Integer;
begin
  AMetafile := TdxOfficeMetafile(Clone);
  try
    AEmf := AMetafile.GetHenhmetafile;
    try
      ALength := GetMetaFileBitsEx(AEmf, 0, nil);
      if ALength = 0 then
        Exit;
      SetLength(Result, ALength);
      GetMetaFileBitsEx(AEmf, ALength, @Result[0]);
    finally
      DeleteObject(AEmf);
    end;
  finally
    AMetafile.Free;
  end;
end;

{ TdxOfficeImageReference }

constructor TdxOfficeImageReference.Create(AOwner: TdxImageCache;
  ABaseImage: TdxOfficeImage);
begin
  inherited Create;
  FImage := ABaseImage;
  FOwner := AOwner;
end;

function TdxOfficeImageReference.Clone(AOwner: TdxImageCache): TdxOfficeImageReference;
var
  AImage: TdxOfficeImage;
  ACrc32: Integer;
begin
  if Self = nil then
    Exit(nil);
  if FOwner = AOwner then
    AImage := Image
  else
  begin
    ACrc32 := FOwner.GetCrc32(FImage);
    AImage := AOwner.GetImage(ACrc32);
    if AImage = nil then
    begin
      AImage := FImage.Clone;
      AOwner.AddImage(AImage, ACrc32);
    end;
  end;
  Result := CreateClone(AOwner, AImage);
end;

function TdxOfficeImageReference.Clone: TdxOfficeImageReference;
begin
  if Self = nil then
    Exit(nil);
  Result := Clone(FOwner);
end;

function TdxOfficeImageReference.CreateClone(AOwner: TdxImageCache; AImage: TdxOfficeImage): TdxOfficeImageReference;
begin
  Result := TdxOfficeImageReferenceClass(ClassType).Create(AOwner, AImage);
end;

procedure TdxOfficeImageReference.ReplaceImage(AImage: TdxOfficeImage);
begin
  FImage := AImage;
end;

procedure TdxOfficeImageReference.RaiseNativeImageChanging;
begin
  if not NativeImageChanging.Empty then
    NativeImageChanging.Invoke(FImage, TdxEventArgs.Empty);
end;

procedure TdxOfficeImageReference.RaiseNativeImageChanged(const ADesiredImageSize: TSize);
var
  Args: TdxNativeImageChangedEventArgs;
begin
  if NativeImageChanged.Empty then
    Exit;
  Args := TdxNativeImageChangedEventArgs.Create(ADesiredImageSize);
  try
    NativeImageChanged.Invoke(FImage, Args);
  finally
    Args.Free;
  end;
end;

function TdxOfficeImageReference.CalculateImageSizeInModelUnits(
  const AUnitConverter: IdxDocumentModelUnitConverter): TSize;
begin
  Result := Image.CalculateImageSizeInModelUnits(AUnitConverter);
end;

function TdxOfficeImageReference.CanGetImageBytes(
  AImageFormat: TdxOfficeImageFormat): Boolean;
begin
  Result := Image.CanGetImageBytes(AImageFormat);
end;

function TdxOfficeImageReference.GetDesiredSizeAfterLoad: TSize;
begin
  Result := Image.DesiredSizeAfterLoad;
end;

function TdxOfficeImageReference.GetEmfImageBytesStream: TBytesStream;
begin
  Result := Image.GetEmfImageBytesStream;
end;

function TdxOfficeImageReference.GetHorizontalResolution: Single;
begin
  Result := Image.HorizontalResolution;
end;

function TdxOfficeImageReference.GetImageBytesStream(
  AImageFormat: TdxOfficeImageFormat): TBytesStream;
begin
  Result := Image.GetImageBytesStream(AImageFormat);
end;

function TdxOfficeImageReference.GetIsLoaded: Boolean;
begin
  Result := Image.IsLoaded;
end;

function TdxOfficeImageReference.GetRawFormat: TdxOfficeImageFormat;
begin
  Result := Image.RawFormat;
end;

function TdxOfficeImageReference.GetShouldSetDesiredSizeAfterLoad: Boolean;
begin
  Result := Image.FShouldSetDesiredSizeAfterLoad;
end;

function TdxOfficeImageReference.GetSizeInOriginalUnits: TSize;
begin
  Result := Image.SizeInOriginalUnits;
end;

function TdxOfficeImageReference.GetSizeInPixels: TSize;
begin
  Result := Image.SizeInPixels;
end;

function TdxOfficeImageReference.GetSizeInTwips: TSize;
begin
  Result := Image.SizeInTwips;
end;

function TdxOfficeImageReference.GetSize: TSize;
begin
  Result := Image.Size;
end;

function TdxOfficeImageReference.GetSuppressStore: Boolean;
begin
  Result := Image.SuppressStore;
end;

function TdxOfficeImageReference.GetUri: string;
begin
  Result := Image.Uri;
end;

function TdxOfficeImageReference.GetVerticalResolution: Single;
begin
  Result := Image.VerticalResolution;
end;

function TdxOfficeImageReference.GetWmfImageBytesStream: TBytesStream;
begin
  Result := Image.GetWmfImageBytesStream;
end;

function TdxOfficeImageReference.IsExportSupported(
  ARawFormat: TdxOfficeImageFormat): Boolean;
begin
  Result := Image.IsExportSupported(ARawFormat);
end;

procedure TdxOfficeImageReference.SetDesiredSizeAfterLoad(const Value: TSize);
begin
  Image.DesiredSizeAfterLoad := Value;
end;

procedure TdxOfficeImageReference.SetShouldSetDesiredSizeAfterLoad(
  const Value: Boolean);
begin
  Image.ShouldSetDesiredSizeAfterLoad := Value;
end;

procedure TdxOfficeImageReference.SetSuppressStore(const Value: Boolean);
begin
  Image.SuppressStore := Value;
end;

procedure TdxOfficeImageReference.SetUri(const Value: string);
begin
  Image.Uri := Value;
end;

function TdxOfficeImageReference.UnitsToHundredthsOfMillimeter(const ASizeInUnits: TSize): TSize;
begin
  Result := Image.UnitsToHundredthsOfMillimeter(ASizeInUnits);
end;

{ TdxImageCache }

constructor TdxImageCache.Create;
begin
  inherited Create;
  FImages := TDictionary<Integer, TdxOfficeImage>.Create;
end;

destructor TdxImageCache.Destroy;
begin
  Clear;
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure TdxImageCache.AddImage(AImage: TdxOfficeImage; ACrc32: Integer);
begin
  FImages.Add(ACrc32, AImage);
  AImage.CheckCrc32(ACrc32);
end;

function TdxImageCache.GetCrc32(AImage: TdxOfficeImage): Integer;
begin
  for Result in FImages.Keys do
    if FImages[Result] = AImage then
      Exit;
  Result := -1;
end;

function TdxImageCache.GetImage(ACrc32: Integer): TdxOfficeImage;
begin
  Result := nil;
  if not FImages.TryGetValue(ACrc32, Result) then
    Exit;
  if Result = nil then
  begin
    FImages.Remove(ACrc32);
    Exit;
  end;
end;

procedure TdxImageCache.Clear;
var
  AKey: Integer;
  AImage: TdxOfficeImage;
begin
  for AKey in FImages.Keys do
  begin
    AImage := FImages[AKey];
    AImage.Free;
  end;
  FImages.Clear;
end;

{ TdxPaletteHelper }

class procedure TdxPaletteHelper.Initialize;
begin
  SetLength(FPalette0, 0);
  FPalette2 := PreparePalette2;
  FPalette16 := PreparePalette16;
  FPalette256 := PreparePalette256;
end;

class function TdxPaletteHelper.GetPalette(ABitsPerPixel: Integer): TArray<TdxAlphaColor>;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Initialize;
  end;
  if ABitsPerPixel = 8 then
    Exit(FPalette256)
  else
    if ABitsPerPixel = 4 then
      Exit(FPalette16)
    else
      if ABitsPerPixel = 1 then
        Exit(FPalette2)
      else
        Exit(FPalette0);
end;

class function TdxPaletteHelper.PreparePalette256: TArray<TdxAlphaColor>;
begin
  Result := PreparePalette(TdxOfficePixelFormat.Format8bppIndexed);
end;

class function TdxPaletteHelper.PreparePalette16: TArray<TdxAlphaColor>;
begin
  Result := PreparePalette(TdxOfficePixelFormat.Format4bppIndexed);
end;

class function TdxPaletteHelper.PreparePalette2: TArray<TdxAlphaColor>;
begin
  Result := PreparePalette(TdxOfficePixelFormat.Format1bppIndexed);
end;

class function TdxPaletteHelper.PreparePalette(APixelFormat: TdxOfficePixelFormat): TArray<TdxAlphaColor>;
var
  ABmp: GPImage;
begin
  GdipCreateBitmapFromScan0(10, 10, 0, Ord(APixelFormat), nil, ABmp);
  try
    Result := TdxOfficeImage.GetColorPalette(ABmp);
  finally
    GdipDisposeImage(ABmp);
  end;
end;

{ TdxMetafileHelper }

class function TdxMetafileHelper.CreateMetafile(AStream: TMemoryStream;
  AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer): TdxOfficeImage;
var
  AHandle: THandle;
  AMfp: TMetafilePict;
  AMetafile: TMetafile;
begin
  AHandle := SetMetaFileBitsEx(AStream.Size, AStream.Memory);
  AMetafile := TryCreateMetafile(AHandle);
  try
    if AMetafile = nil then
    begin
      TdxMetafileHelper.DeleteMetafileHandle(AHandle);
      AHandle := SetEnhMetaFileBits(AStream.Size, AStream.Memory);
      AMetafile := TryCreateMetafile(AHandle);
      if AMetafile = nil then
      begin
        TdxMetafileHelper.DeleteMetafileHandle(AHandle);

        AMfp.mm := Ord(AMapMode);
        AMfp.xExt := APictureWidth;
        AMfp.yExt := APictureHeight;
        AMfp.hMF := 0;

        AHandle := SetWinMetaFileBits(AStream.Size, AStream.Memory, 0, AMfp);
        AMetafile := TryCreateMetafile(AHandle);
        if AMetafile = nil then
        begin
          TdxMetafileHelper.DeleteMetafileHandle(AHandle);
          AMetafile := TMetafile.Create;
          try
            AStream.Position := 0;
            AMetafile.LoadFromStream(AStream);
          except
            FreeAndNil(AMetafile);
          end;
        end;
      end;
    end;
    if AMetafile <> nil then
    begin
      if AMetafile.Enhanced then
        Result := TdxOfficeEmfImage.Create
      else
        Result := TdxOfficeWmfImage.Create;
      Result.AssignFromGraphic(AMetafile);
      Result.HandleNeeded;
    end
    else
    begin
      AStream.Position := 0;
      Result := TdxImageLoaderHelper.ImageFromStream(AStream);
    end;
  finally
    AMetafile.Free;
  end;
end;

class procedure TdxMetafileHelper.DeleteMetafileHandle(AHandle: THandle);
begin
  if not DeleteEnhMetaFile(AHandle) then
    DeleteMetaFile(AHandle);
end;

class function TdxMetafileHelper.TryCreateMetafile(AHandle: THandle): TMetafile;
begin
  if AHandle = 0 then
    Result := nil
  else
    Result := TryCreateMetafileCore(AHandle);
end;

class function TdxMetafileHelper.TryCreateMetafileCore(
  AHandle: THandle): TMetafile;
begin
  Result := TMetafile.Create;
  try
    Result.Handle := AHandle;
  except
    FreeAndNil(Result);
  end;
end;

{ TdxMetafilePhysicalDimensionCalculator }

function TdxMetafilePhysicalDimensionCalculator.Calculate(AMetafile: TdxOfficeMetafile; const ABytes: TArray<Byte>): TSize;
var
  AHmf, AHEmf: THandle;
  AMfp: METAFILEPICT;
begin
  Result := TSize.Create(0, 0);
  FWindowExtent := TSize.Create(0, 0);
  FMapMode := TdxMapMode.HighMetric;

  AHmf := SetMetaFileBitsEx(Length(ABytes), @ABytes[0]);
  if AHmf <> 0 then
  begin
    Result := CalculateWmfPhysicalDimension(AHmf, AMetafile);
    DeleteMetaFile(AHmf);
  end
  else
  begin
    AMfp.mm := Ord(TdxMapMode.Anisotropic);
    AMfp.xExt := -1;
    AMfp.yExt := -1;
    AHEmf := SetWinMetaFileBits(Length(ABytes), @ABytes[0], 0, AMfp);
    if AHEmf = 0 then
      AHEmf := SetEnhMetaFileBits(Length(ABytes), @ABytes[0]);

    if AHEmf <> 0 then
    begin
      Result := CalculateEmfPhysicalDimension(AHEmf, AMetafile);
      TdxMetafileHelper.DeleteMetafileHandle(AHEmf);
    end
    else
      Result := TSize.Round(AMetafile.PhysicalDimension);
  end;
end;

function TdxMetafilePhysicalDimensionCalculator.EnumMetafileCallback(AHdc: HDC;
  AHandleTable: PHandleTable; AMetafileRecord: PMetaRecord;
  AObjectCount: Integer; AClientData: THandle): Integer;
var
  ARecordType, AHeight, AWidth: Integer;
  AEmfRecordType: TEmfPlusRecordType;
  APointer: PWORD;
begin
  APointer := PWORD(AMetafileRecord);
  Inc(APointer, 2);
  ARecordType := APointer^;
  AEmfRecordType := TEmfPlusRecordType(Ord(TEmfPlusRecordType.WmfRecordBase) + ARecordType);
  if AEmfRecordType = TEmfPlusRecordType.WmfSetWindowExt then
  begin
    Inc(APointer);
    AHeight := APointer^;
    Inc(APointer);
    AWidth := APointer^;
    FWindowExtent := TSize.Create(AWidth, AHeight);
  end
  else
    if AEmfRecordType = TEmfPlusRecordType.WmfSetMapMode then
    begin
      Inc(APointer);
      FMapMode := TdxMapMode(APointer^);
    end;
  Result := 1;
end;

function TdxMetafilePhysicalDimensionCalculator.EnumEnhMetafileCallback(AHdc: HDC;
  AHandleTable: PHandleTable; AMetafileRecord: PMetaRecord;
  AObjectCount: Integer; AClientData: THandle): Integer;
var
  ARecordType: TEmfPlusRecordType;
  AWidth, AHeight: Integer;
  APointer: PInteger;
begin
  APointer := PInteger(AMetafileRecord);
  ARecordType := TEmfPlusRecordType(APointer^);

  if ARecordType = TEmfPlusRecordType.EmfSetWindowExtEx then
  begin
    Inc(APointer, 2);
    AWidth := APointer^;
    Inc(APointer, 1);
    AHeight := APointer^;
    FWindowExtent := TSize.Create(AWidth, AHeight);
  end
  else
    if ARecordType = TEmfPlusRecordType.WmfSetMapMode then
    begin
      Inc(APointer, 2);
      FMapMode := TdxMapMode(APointer^);
    end;
  Result := 1;
end;

var
  FEnumMetafileCallbackCalculator: TdxMetafilePhysicalDimensionCalculator;

function InternalEnumMetafileCallback(AHdc: HDC;
  AHandleTable: PHandleTable; AMetafileRecord: PMetaRecord;
  AObjectCount: Integer; AClientData: THandle): Integer; stdcall;
begin
  Result := FEnumMetafileCallbackCalculator.EnumMetafileCallback(AHdc, AHandleTable, AMetafileRecord,
    AObjectCount, AClientData);
end;

function TdxMetafilePhysicalDimensionCalculator.CalculateWmfPhysicalDimension(AHmf: HMETAFILE; AMetafile: TdxOfficeMetafile): TSize;
begin
  FEnumMetafileCallbackCalculator := Self;
  EnumMetaFile(0, AHmf, @InternalEnumMetafileCallback, 0);
  FEnumMetafileCallbackCalculator := nil;

  Result := FWindowExtent;
  if Result.IsEqual(TSize.Empty) then
    Result := AMetafile.Size;

  if FMapMode = TdxMapMode.HighMetric then
    Exit;

  Result := ConvertLogicalUnitsToHundredthsOfMillimeter(Result);
end;

var
  FEnumEnhMetafileCallbackCalculator: TdxMetafilePhysicalDimensionCalculator;

function InternalEnumEnhMetafileCallback(AHdc: HDC;
  AHandleTable: PHandleTable; AMetafileRecord: PMetaRecord;
  AObjectCount: Integer; AClientData: THandle): Integer; stdcall;
begin
  Result := FEnumEnhMetafileCallbackCalculator.EnumEnhMetafileCallback(AHdc, AHandleTable, AMetafileRecord,
    AObjectCount, AClientData);
end;

function TdxMetafilePhysicalDimensionCalculator.CalculateEmfPhysicalDimension(AHEmf: HMETAFILE;
  AMetafile: TdxOfficeMetafile): TSize;
var
  ARect: TRect;
begin
  FEnumEnhMetafileCallbackCalculator := Self;
  EnumEnhMetaFile(0, AHEmf, @InternalEnumEnhMetafileCallback, nil, ARect);
  FEnumEnhMetafileCallbackCalculator := nil;

  Result := FWindowExtent;
  if Result.IsEqual(TSize.Empty) then
    Result := AMetafile.Size;

  Result := ConvertLogicalUnitsToHundredthsOfMillimeter(Result);
end;

function TdxMetafilePhysicalDimensionCalculator.ConvertLogicalUnitsToHundredthsOfMillimeter(const ASize: TSize): TSize;
begin
  Result.Width := Integer(Round(2540.0 * Abs(ASize.Width) / TdxMetafileHelper.MetafileResolution));
  Result.Height := Integer(Round(2540.0 * Abs(ASize.Height) / TdxMetafileHelper.MetafileResolution));
end;

{ TdxNativeImageChangedEventArgs }

constructor TdxNativeImageChangedEventArgs.Create(
  const ADesiredImageSizeInTwips: TSize);
begin
  inherited Create;
  FDesiredImageSizeInTwips := ADesiredImageSizeInTwips;
end;

{ TdxDibHelper }

class function TdxDibHelper.CreateBmpFileStreamForDib(ADibStream: TStream; ADibHeight, ABytesInLine: Integer): TBytesStream;
var
  AWriter: TBinaryWriter;
  AFileSize, AOffset: Integer;
  ABitmapCoreHeader: TBitmapCoreHeader;
begin
  Result := TBytesStream.Create;
  AWriter := TBinaryWriter.Create(Result);
  try
    ADibStream.Read(ABitmapCoreHeader, SizeOf(ABitmapCoreHeader));
    AFileSize := TdxBitmapFileHelper.SizeofBITMAPFILEHEADER + Integer(ADibStream.Size);
    AOffset := TdxBitmapFileHelper.SizeofBITMAPFILEHEADER + ABitmapCoreHeader.bcSize;
    TdxBitmapFileHelper.WriteBITMAPFILEHEADER(AWriter, AFileSize, AOffset);
    ADibStream.Position := 0;
    TdxStreamHelper.WriteTo(ADibStream, Result)
  finally
    AWriter.Close;
    AWriter.Free;
  end;
  Result.Position := 0;
end;

class function TdxDibHelper.CreateDib(AStream: TStream; AWidth, AHeight, ABytesInLine: Integer): TdxOfficeImage;
var
  ABmpFileStream: TStream;
begin
  ABmpFileStream := CreateBmpFileStreamForDib(AStream, AHeight, ABytesInLine);
  try
    Result := TdxImageLoaderHelper.ImageFromStream(ABmpFileStream);
  finally
    ABmpFileStream.Free;
  end;
end;

{ TdxBitmapHelper }

class function TdxBitmapHelper.CreateBitmap(AStream: TStream; AWidth, AHeight,
   AColorPlanesCount, ABitsPerPixel, ABytesInLine: Integer): TdxOfficeImage;
var
  ABitmapStream: TMemoryStream;
  AWriter: TBinaryWriter;
  APalette: TArray<TdxAlphaColor>;
  AHeaderSize, APaletteSize, AFileSize, AOffset: Integer;
begin
  ABitmapStream := TMemoryStream.Create;
  try
    AWriter := TBinaryWriter.Create(ABitmapStream);
    try
      APalette := TdxPaletteHelper.GetPalette(ABitsPerPixel);
      AHeaderSize := TdxBitmapFileHelper.SizeofBITMAPFILEHEADER + TdxBitmapFileHelper.SizeofBITMAPINFOHEADER;
      APaletteSize := Length(APalette) * 4;
      AFileSize := AHeaderSize + APaletteSize + Integer(AStream.Size);
      AOffset := AHeaderSize + APaletteSize;

      TdxBitmapFileHelper.WriteBITMAPFILEHEADER(AWriter, AFileSize, AOffset);
      TdxBitmapFileHelper.WriteBITMAPINFOHEADER(AWriter, AWidth, AHeight, AColorPlanesCount, ABitsPerPixel, ABytesInLine, APalette);
      TdxBitmapFileHelper.WritePalette(AWriter, APalette);

      TdxStreamHelper.WriteTo(AStream, ABitmapStream);
    finally
      AWriter.Close;
      AWriter.Free;
    end;
    ABitmapStream.Position := 0;
    Result := TdxImageLoaderHelper.ImageFromStream(ABitmapStream);
  finally
    ABitmapStream.Free;
  end;
end;

{ TdxBitmapFileHelper }

class procedure TdxBitmapFileHelper.WriteBITMAPFILEHEADER(AWriter: TBinaryWriter; AFileSize, ABitsOffset: Integer);
begin
  AWriter.Write(Byte(66));
  AWriter.Write(Byte(77));
  AWriter.Write(Int32(AFileSize));
  AWriter.Write(Int16(0));
  AWriter.Write(Int16(0));
  AWriter.Write(Int32(ABitsOffset));
end;

class procedure TdxBitmapFileHelper.WriteBITMAPINFOHEADER(AWriter: TBinaryWriter; AWidth, AHeight, AColorPlanesCount, ABitsPerPixel,
       ABytesInLine: Integer; const APalette: TArray<TdxAlphaColor>);
var
  ABitsSize: Integer;
begin
  ABitsSize := AHeight * ABytesInLine;

  AWriter.Write(Int32(SizeofBITMAPINFOHEADER));
  AWriter.Write(Int32(AWidth));
  AWriter.Write(Int32(AHeight));
  AWriter.Write(Int16(AColorPlanesCount));
  AWriter.Write(Int16(ABitsPerPixel));
  AWriter.Write(Int32(0));
  AWriter.Write(Int32(ABitsSize));
  AWriter.Write(Int32(0));
  AWriter.Write(Int32(0));
  AWriter.Write(Int32(Length(APalette)));
  AWriter.Write(Int32(0));
end;

class procedure TdxBitmapFileHelper.WritePalette(AWriter: TBinaryWriter; const APalette: TArray<TdxAlphaColor>);
var
  ACount, I: Integer;
  C: TdxAlphaColor;
  ARgb: TRGBQuad;
begin
  ACount := Length(APalette);
  for I := 0 to ACount - 1 do
  begin
    C := APalette[I];
    ARgb := dxAlphaColorToRGBQuad(C);
    AWriter.Write(Byte(ARgb.rgbBlue));
    AWriter.Write(Byte(ARgb.rgbGreen));
    AWriter.Write(Byte(ARgb.rgbRed));
    AWriter.Write(Byte(0));
  end;
end;

{ TdxStreamHelper }

class procedure TdxStreamHelper.WriteTo(AInputStream: TStream; AOutputStream: TStream);
var
  ABuf: array[0..1023] of Byte;
  AReadedByteCount: Integer;
begin
  while True do
  begin
    AReadedByteCount := AInputStream.Read(ABuf, Length(ABuf));
    if AReadedByteCount = 0 then
      Break;
    AOutputStream.Write(ABuf, AReadedByteCount);
  end;
end;

{ TdxImageLoaderHelper }

class function TdxImageLoaderHelper.ImageFromStream(AStream: TStream): TdxOfficeImage;
var
  AImageStream: TMemoryStream;
begin
  AImageStream := GetMemoryStream(AStream, -1);
  try
    Result := TdxOfficeImage.CreateImage(AImageStream);
  finally
    AImageStream.Free;
  end;
end;

class function TdxImageLoaderHelper.ImageFromFile(const AFilename: string): TdxOfficeImage;
var
  AStream: TStream;
begin
  AStream := GetStream(AFilename);
  try
    Result := ImageFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

class function TdxImageLoaderHelper.GetStream(const AFileName: string): TStream;
var
  AStream: TdxMemoryStream;
begin
  AStream := TdxMemoryStream.Create(AFileName);
  try
    Result := TMemoryStream.Create;
    Result.CopyFrom(AStream, 0);
    Result.Position := 0;
  finally
    AStream.Free;
  end;
end;

class function TdxImageLoaderHelper.GetMemoryStream(AStream: TStream; ALength: Integer): TMemoryStream;
var
  ACount: Integer;
  ABuffer: TArray<Byte>;
begin
  if ALength < 0 then
    ACount := Integer(AStream.Size)
  else
    ACount := ALength;
  SetLength(ABuffer, ACount);
  AStream.Read(ABuffer[0], ACount);
  Result := TBytesStream.Create(ABuffer);
end;

class function TdxImageLoaderHelper.IsImageStream(AStream: TStream): Boolean;
var
  APosition: Integer;
  AStreamAdapter: IStream;
  AImage: GPImage;
begin
  if not CheckGdiPlus(False) then
    Exit(False);

  APosition := AStream.Position;
  try
    AStreamAdapter := TdxGPStreamAdapter.Create(AStream, soReference);
    Result := GdipLoadImageFromStream(AStreamAdapter, AImage) = Ok;
    try
      AStreamAdapter := nil;
    finally
      GdipDisposeImage(AImage);
    end;
  except
    Result := False;
  end;
  AStream.Position := APosition;
end;

{ TdxImageTool }

class function TdxImageTool.CalculateImageRectCore(AClientRect: TRect;
  AImageSize: TSize; ASizeMode: TdxImageSizeMode): TdxRectF;
var
  X, Y: Single;
begin
  Result.Init(0, 0, AImageSize.cx, AImageSize.cy);
  case ASizeMode of
    TdxImageSizeMode.AutoSize,
    TdxImageSizeMode.Normal:
      Result.Location := dxRectF(AClientRect).Location;
    TdxImageSizeMode.StretchImage:
      Result := dxRectF(AClientRect);
    TdxImageSizeMode.CenterImage:
      begin
        X := (Result.Left + Result.Right) / 2 - (AClientRect.Left + AClientRect.Right) / 2;
        Y := (Result.Top + Result.Bottom) / 2- (AClientRect.Top + AClientRect.Bottom) / 2;
        Result.Offset(X, Y);
      end;
    TdxImageSizeMode.ZoomImage:
      Result := Result.ZoomInto(dxRectF(AClientRect));
    TdxImageSizeMode.Squeeze:
      begin
        if (Result.Width > AClientRect.Width) or (Result.Height > AClientRect.Height) then
          Result := Result.ZoomInto(dxRectF(AClientRect));
      end;
  end;
end;

{ TdxUriOfficeImage }

class procedure TdxUriOfficeImage.FinalizePlaceHolder;
begin
  FreeAndNil(FPlaceHolder);
end;

constructor TdxUriOfficeImage.Create(AOwner: TdxImageCache;
  const AServiceProvider: IdxServiceProvider; AUriList: TArray<string>;
  APixelTargetWidth, APixelTargetHeight: Integer; AAsyncImageLoading: Boolean);
var
  I: Integer;
  AUri: string;
  AImage: TdxOfficeImage;
  ACrc32: Cardinal;
  AService: IdxUriStreamService;
  AThreadPoolService: IdxThreadPoolService;
  AStream: TStream;
  AHasImage: Boolean;
begin
  AHasImage := False;
  AImage := CreatePlaceHolder(APixelTargetWidth, APixelTargetHeight);
  try
    ACrc32 := AImage.CalculateCrc32;
    AHasImage := AOwner.GetImage(ACrc32) <> nil;
    if not AHasImage then
      AOwner.AddImage(AImage, ACrc32);
  finally
    if AHasImage then
      AImage.Free;
  end;
  AImage := AOwner.GetImage(ACrc32);
  inherited Create(AOwner, AImage);
  FUpdater := TdxUriOfficeImageUpdater.Create;
  FUpdater.Add(Self);
  FPixelTargetHeight := APixelTargetHeight;
  FPixelTargetWidth := APixelTargetWidth;
  FUriList := AUriList;
  Uri := FUriList[0];
  AThreadPoolService := AServiceProvider.GetService(IdxThreadPoolService) as IdxThreadPoolService;
  if AThreadPoolService = nil then
    AAsyncImageLoading := False;
  AService := AServiceProvider.GetService(IdxUriStreamService) as IdxUriStreamService;
  if AService <> nil then
  begin
    if AAsyncImageLoading and AService.IsAsyncLoadingSupported(Uri) then
    begin
      FEndLoadingTaskEvent := TSimpleEvent.Create;
      AThreadPoolService.AddTask(TdxUriOfficeImageLoadingTask.Create(AService, FUpdater, FUriList, FEndLoadingTaskEvent));
    end
    else
    begin
      AStream := nil;
      for I := 0 to Length(FUriList) - 1 do
      begin
        AUri := FUriList[I];
        AStream := AService.GetStream(Uri);
        if AStream <> nil then
          Break;
      end;
      try
        if (AStream <> nil) and TdxImageLoaderHelper.IsImageStream(AStream) then
        begin
          AStream.Position := 0;
          AImage := TdxOfficeImage.CreateFromStream(AStream);
          try
            Update(AImage);
          finally
            AImage.Free;
          end;
        end;
      finally
        AStream.Free;
      end;
    end;
  end;
end;

destructor TdxUriOfficeImage.Destroy;
begin
  FUpdater.Remove(Self);
  FUpdater := nil;
  FreeAndNil(FEndLoadingTaskEvent);
  inherited Destroy;
end;

procedure TdxUriOfficeImage.EnsureLoadComplete(ATimeOut: Cardinal);
begin
 if not IsLoaded and (FEndLoadingTaskEvent <> nil) then
   FEndLoadingTaskEvent.WaitFor(ATimeOut);
end;

procedure TdxUriOfficeImage.OnEndLoadingTask;
begin
  if FEndLoadingTaskEvent <> nil then
  begin
    FEndLoadingTaskEvent.SetEvent;
    FreeAndNil(FEndLoadingTaskEvent);
  end;
end;

function TdxUriOfficeImage.CreateClone(AOwner: TdxImageCache; AImage: TdxOfficeImage): TdxOfficeImageReference;
var
  AUriImage: TdxUriOfficeImage;
begin
  Result := inherited CreateClone(AOwner, AImage);
  AUriImage := TdxUriOfficeImage(Result);
  AUriImage.FUpdater := FUpdater;
  AUriImage.FPixelTargetHeight := FPixelTargetHeight;
  AUriImage.FPixelTargetWidth := FPixelTargetWidth;
  AUriImage.FSuppressStorePlaceholder := FSuppressStorePlaceholder;
  AUriImage.FUriList := FUriList;
  AUriImage.FEndLoadingTaskEvent := FEndLoadingTaskEvent;
  FEndLoadingTaskEvent := nil;

  FUpdater.Add(AUriImage);
end;

class function TdxUriOfficeImage.CreateDefaultPlaceHolder: TdxOfficeImage;
begin
  Result := TdxOfficeImage.Create;
  Result.LoadFromResource(HInstance, 'DXRICHEDITIMAGEPLACEHOLDER', 'PNG');
end;

procedure TdxUriOfficeImage.ReplaceImage(AImage: TdxOfficeImage);
begin
  RaiseNativeImageChanging;
  inherited ReplaceImage(AImage);
  RaiseNativeImageChanged(GetDesiredSizeInTwips);
end;

procedure TdxUriOfficeImage.Update(AImage: TdxOfficeImage);
var
  ACrc32: Integer;
  AUpdatedImage: TdxOfficeImage;
begin
  ACrc32 := AImage.CalculateCrc32;
  AUpdatedImage := Owner.GetImage(ACrc32);
  if AUpdatedImage = nil then
  begin
    AUpdatedImage := AImage.Clone;
    Owner.AddImage(AUpdatedImage, ACrc32);
  end;
  AUpdatedImage.FIsLoaded := True;
  AUpdatedImage.Uri := Uri;
  ReplaceImage(AUpdatedImage);
end;

class function TdxUriOfficeImage.GetPlaceHolder: TdxOfficeImage;
begin
  if FPlaceHolder = nil then
    FPlaceHolder := CreateDefaultPlaceHolder;
  Result := FPlaceHolder;
end;

function TdxUriOfficeImage.CreatePlaceHolder(APixelTargetWidth, APixelTargetHeight: Integer): TdxOfficeImage;
var
  ASize: TSize;
  ACanvas: TdxGPCanvas;
begin
  ASize.cx := IfThen(APixelTargetWidth > 0, APixelTargetWidth, PlaceHolder.Width);
  ASize.cy := IfThen(APixelTargetHeight > 0, APixelTargetHeight, PlaceHolder.Height);
  if (ASize.cx <> PlaceHolder.Width) or (ASize.cy <> PlaceHolder.Height) then
  begin
    Result := TdxOfficeImage.CreateSize(ASize);
    ACanvas := Result.CreateCanvas;
    try
      ACanvas.Draw(PlaceHolder, PlaceHolder.ClientRect);
      ACanvas.Rectangle(Result.ClientRect, TdxAlphaColors.Black, TdxAlphaColors.Empty);
    finally
      ACanvas.Free;
    end;
    Result.ImageDataFormat := dxImagePng;
  end
  else
    Result := PlaceHolder.Clone;
  Result.FIsLoaded := False;
end;

function TdxUriOfficeImage.GetDesiredSizeInTwips: TSize;
begin
  Result := TSize.Create(PixelsToTwips(FPixelTargetWidth, TdxDocumentModelDpi.DpiX),
    PixelsToTwips(FPixelTargetHeight, TdxDocumentModelDpi.DpiY));
end;

procedure Finalize;
begin
  TdxUriOfficeImage.FinalizePlaceHolder;
end;

initialization
  dxUnitsLoader.AddUnit(nil, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
