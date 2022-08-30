{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFCommandInterpreter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Types, Graphics, Classes, Generics.Defaults, Generics.Collections, dxCoreClasses, cxGeometry,
  cxGraphics, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxPDFCore, dxPDFFontEncoding, dxPDFBase, dxPDFTypes,
  dxPDFFont, dxPDFFontUtils, dxPDFInteractivity, dxPDFText;

type
  TdxPDFCustomCommandInterpreter = class;
  TdxPDFGraphicsPath = class;
  TdxPDFGraphicsPathBuilder = class;
  TdxPDFExportParametersClass = class of TdxPDFExportParameters;

  { TdxPDFRenderParameters }

  TdxPDFRenderParameters = class(TdxPDFExportParameters)
  public
    Canvas: TCanvas;
    Rect: TRect;
    Position: TdxPointF;
  end;

  { TdxPDFGraphicsState }

  TdxPDFGraphicsState = class(TdxPDFReferencedObject)
  strict private
    FNonStrokingColor: TdxPDFColor;
    FNonStrokingColorSpace: TdxPDFCustomColorSpace;
    FParameters: TdxPDFGraphicsStateParameters;
    FStrokingColor: TdxPDFColor;
    FStrokingColorSpace: TdxPDFCustomColorSpace;
    FTextState: TdxPDFTextState;

    FDeviceTransformMatrix: TdxPDFTransformationMatrix;
    FTransformMatrix: TdxPDFTransformationMatrix;

    procedure SetNonStrokingColor(const AValue: TdxPDFColor);
    procedure SetNonStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetStrokingColor(const AValue: TdxPDFColor);
    procedure SetStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(AGraphicsState: TdxPDFGraphicsState);
    procedure ApplyParameters(AParameters: TdxPDFGraphicsStateParameters);
    procedure RecreateTextState;

    property DeviceTransformMatrix: TdxPDFTransformationMatrix read FDeviceTransformMatrix;
    property NonStrokingColor: TdxPDFColor read FNonStrokingColor write SetNonStrokingColor;
    property NonStrokingColorSpace: TdxPDFCustomColorSpace read FNonStrokingColorSpace write SetNonStrokingColorSpace;
    property Parameters: TdxPDFGraphicsStateParameters read FParameters;
    property StrokingColor: TdxPDFColor read FStrokingColor write SetStrokingColor;
    property StrokingColorSpace: TdxPDFCustomColorSpace read FStrokingColorSpace write SetStrokingColorSpace;
    property TextState: TdxPDFTextState read FTextState write FTextState;
    property TransformMatrix: TdxPDFTransformationMatrix read FTransformMatrix write FTransformMatrix;
  end;

  { TdxPDFCustomCommandInterpreter }

  TdxPDFCustomCommandInterpreterClass = class of TdxPDFCustomCommandInterpreter;
  TdxPDFCustomCommandInterpreter = class(TcxIUnknownObject, IdxPDFCommandInterpreter)
  strict private
    FActualTextAngle: Single;
    FActualTextHeightFactor: Single;
    FActualTextMatrix: TdxPDFTransformationMatrix;
    FActualTextSkewAngle: Single;
    FActualTextWidthFactor: Single;
    FActualSize: TSize;
    FAngle: Integer;
    FBoundsOffset: TdxPointF;
    FGraphicsState: TdxPDFGraphicsState;
    FGraphicsStateStack: TObjectStack<TdxPDFGraphicsState>;
    FGraphicsStateStackLock: Integer;
    FTilingTransformMatrix: TdxPDFTransformationMatrix;
    FTilingTransformMatrixStack: TObjectStack<TdxPDFTransformationMatrix>;
    FTransformedBoundingBox: TdxPDFPoints;
    function GetActualSize: TSize;
    function GetBounds: TdxRectF;
    function GetDeviceTransformationMatrix: TdxPDFTransformationMatrix;
    function GetDocumentState: TdxPDFDocumentState;
    function GetRotationAngle: Single;
    function GetTextState: TdxPDFTextState; inline;
    function GetTextStateFont: TdxPDFCustomFont; inline;
    function GetTransformMatrix: TdxPDFTransformationMatrix; inline;
    function NeedDrawAnnotation(AAnnotation: TdxPDFCustomAnnotation): Boolean;
    procedure DrawAnnotation(AAnnotation: TdxPDFCustomAnnotation);
    procedure ExportAnnotation;
    procedure InitializeTransformedBoundingBox;
    procedure RestoreGraphicsState;
  private
    FParameters: TdxPDFExportParameters;
  strict protected
    FClipBounds: TdxRectF;
  protected
    function CheckRenderingMode: Boolean; virtual;
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
      AColor: TdxPDFColor): TcxBitmap32; virtual;
    function GetBoundsOffset: TdxPointF; virtual;
    function IsCanceled: Boolean;
    procedure AfterExport; virtual;
    procedure AfterExportPage(APage: TdxPDFPage); virtual;
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF); overload; virtual;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF); overload; virtual;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF); virtual;
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double); virtual;
    procedure ApplyGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters); virtual;
    procedure BeginPath(const AStartPoint: TdxPointF); virtual;
    procedure BeginText; virtual;
    procedure BeforeExport; virtual;
    procedure BeforeExportPage(AData: TdxPDFPageData; AParameters: TdxPDFExportParameters); virtual;
    procedure ClipPaths(AUseNonzeroWindingRule: Boolean); virtual;
    procedure ClosePath; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoExport(ACommands: TdxPDFReferencedObjects); virtual;
    procedure DrawImage(AImage: TdxPDFDocumentImage); virtual; abstract;
    procedure DrawShading(AShading: TdxPDFCustomShading); virtual;
    procedure DrawString(const ALocation: TdxPointF; const AData: TdxPDFStringData;
      const AOffsets: TDoubleDynArray); overload; virtual; abstract;
    procedure EndText; virtual;
    procedure Initialize; virtual;
    procedure InitializeClipBounds(const ATopLeft, ABottomRight: TdxPointF); virtual;
    procedure InitializeTransformMatrix; virtual;
    procedure FillPaths(AUseNonzeroWindingRule: Boolean); virtual;
    procedure Finalize; virtual;
    procedure RecreatePaths; virtual;
    procedure RestoreState; virtual;
    procedure SaveState; virtual;
    procedure SetColorForNonStrokingOperations(AColor: TdxPDFColor); virtual;
    procedure SetColorForStrokingOperations(AColor: TdxPDFColor); virtual;
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double); virtual;
    procedure SetTextMatrix(AMatrix: TdxPDFTransformationMatrix); overload; virtual;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode); virtual;
    procedure StrokePaths; virtual;
    procedure TransformPaths; virtual;
    procedure UpdateTilingTransformationMatrix;
    function CalculateAngle(const P1, P2: TdxPointF): Single; overload;
    function CalculateAngle(X, Y: Single): Single; overload;
    function IsNotRotated: Boolean;
    function IsType3Font: Boolean; inline;
    function ToCanvasPoint(const P: TdxPointF): TdxPointF; inline;
    function TransformSize(AMatrix: TdxPDFTransformationMatrix; const ABoundingBox: TdxRectF): TdxSizeF;
    function TransformShadingPoint(APoint: TdxPointF): TdxPointF;
    function VectorLength(AMatrix: TdxPDFTransformationMatrix; X, Y: Single): Single;
    procedure CalculateTextMatrix;
    procedure Clip(AUseNonzeroWindingRule: Boolean);
    procedure DrawForm(AForm: TdxPDFForm; AResources: TdxPDFResources);
    procedure DrawString(const AText: TBytes); overload;
    procedure DrawString(const AText: TBytes; var AOffsets: TDoubleDynArray); overload;
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm); virtual;
    procedure Export(APages: TdxPDFPages; AParameters: TdxPDFExportParameters); overload;
    procedure ExecuteCommand(ACommands: TdxPDFReferencedObjects); overload;
    procedure ExecuteCommand(const AInterpreter: IdxPDFCommandInterpreter; ACommands: TdxPDFReferencedObjects); overload;
    procedure ModifyTransformationMatrix(AMatrix: TdxPDFTransformationMatrix);
    procedure MoveToNextLine;
    procedure SetCharacterSpacing(ASpacing: Single);
    procedure SetColorSpaceForNonStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
    procedure SetColorSpaceForStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
    procedure SetFlatnessTolerance(AValue: Double);
    procedure SetLineCapStyle(ALineCapStyle: TdxPDFLineCapStyle);
    procedure SetLineJoinStyle(ALineJoinStyle: TdxPDFLineJoinStyle);
    procedure SetLineStyle(ALineStyle: TdxPDFLineStyle);
    procedure SetLineWidth(ALineWidth: Single);
    procedure SetMiterLimit(AMiterLimit: Single);
    procedure SetRenderingIntent(AValue: TdxPDFRenderingIntent);
    procedure SetTextLeading(ALeading: Double);
    procedure SetTextHorizontalScaling(AValue: Double);
    procedure SetTextMatrix(const AOffset: TdxPointF); overload;
    procedure SetTextRise(AValue: Double);
    procedure SetWordSpacing(AWordSpacing: Double);
    procedure UnknownCommand(const AName: string);

    property ActualSize: TSize read GetActualSize;
    property ActualTextAngle: Single read FActualTextAngle;
    property ActualTextHeightFactor: Single read FActualTextHeightFactor;
    property ActualTextMatrix: TdxPDFTransformationMatrix read FActualTextMatrix;
    property ActualTextSkewAngle: Single read FActualTextSkewAngle;
    property ActualTextWidthFactor: Single read FActualTextWidthFactor;
    property Bounds: TdxRectF read GetBounds;
    property BoundsOffset: TdxPointF read GetBoundsOffset;
    property ClipBounds: TdxRectF read FClipBounds;
    property DeviceTransformMatrix: TdxPDFTransformationMatrix read GetDeviceTransformationMatrix;
    property DocumentState: TdxPDFDocumentState read GetDocumentState;
    property GraphicsState: TdxPDFGraphicsState read FGraphicsState;
    property RotationAngle: Single read GetRotationAngle;
    property TextState: TdxPDFTextState read GetTextState;
    property TextStateFont: TdxPDFCustomFont read GetTextStateFont;
    property TilingTransformMatrix: TdxPDFTransformationMatrix read FTilingTransformMatrix;
    property TransformedBoundingBox: TdxPDFPoints read FTransformedBoundingBox;
    property TransformMatrix: TdxPDFTransformationMatrix read GetTransformMatrix;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Export(APage: TdxPDFPage; AParameters: TdxPDFExportParameters); overload; virtual;
    procedure Export(ACommands: TdxPDFReferencedObjects; AParameters: TdxPDFExportParameters); overload;
    procedure ExportAndPack(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
  end;

  { TdxPDFRenderingInterpreter }

  TdxPDFRenderingInterpreter = class(TdxPDFCustomCommandInterpreter)
  strict private type
  {$REGION 'Private types'}
    TImagePainter = class
    strict private
      FCorrectedDestinationPoints: TdxPointsF;
      FDestinationPoints: TdxPointsF;

      function ApplyPalette(ABitmap: GpBitmap; AInvertColor: Boolean; AData: TdxPDFDocumentImageData;
        ANonStrokingColor: TdxAlphaColor; AHasMask: Boolean): GPBitmap;
      procedure DoDraw(AGraphics: TdxGPCanvas; ABitmap: GpBitmap; const APoints: TdxPointsF); overload;
      procedure DoDraw(AGraphics: TdxGPCanvas; ABitmap: GpBitmap; const ASize: TSize;
        const APoints: TdxPointsF; AAttributes: TdxGPImageAttributes); overload;
    public
      constructor Create;
      destructor Destroy; override;
      function CreateAttributes(AAlpha: Double): TdxGPImageAttributes;
      procedure Draw(AGraphics: TdxGPCanvas; AImage: TdxPDFDocumentImage; AColor: TdxPDFColor; AAlpha: Double;
        AMatrix: TdxPDFTransformationMatrix; AImageDataStorage: TdxPDFDocumentImageDataStorage);
      procedure DrawBitmap(AGraphics: TdxGPCanvas; ABitmap: GpBitmap; const ASize: TSize;
        const APoints: TdxPointsF; AAlpha: Double);
    end;
  {$ENDREGION}
  strict private
    FActualFontNameCache: TdxPDFStringsDictionary;
    FClipRegion: TdxGPRegion;
    FCurrentClipRegion: TdxGPRegion;
    FGraphics: TdxGPCanvas;
    FHDC: HDC;
    FImageDataStorage: TdxPDFDocumentImageDataStorage;
    FImagePainter: TImagePainter;
    FInitialClipRegion: TdxGPRegion;
    FNonStrokingBrush: TdxGPBrush;
    FNonStrokingBrushStack: TObjectStack<TdxGPBrush>;
    FPaths: TObjectList<TdxPDFGraphicsPath>;
    FPrevAlpha: Integer;
    FPrevBkMode: Cardinal;
    FPrevClipRegion: Cardinal;
    FPrevGraphics: TdxGPCanvas;
    FPrevGraphicsMode: Cardinal;
    FPrevFontQuality: Word;
    FPrevSmoothingMode: Integer;
    FPrevTextAlign: Cardinal;
    FPrevTextColor: Cardinal;
    FPrevTextFont: Cardinal;
    FRegionStack: TObjectStack<TdxGPRegion>;
    FSemitransparentTextBitmap: TdxSmartImage;
    FSolidBrushCache: TObjectDictionary<TdxAlphaColor, TdxGPBrush>;
    FStrokingBrush: TdxGPBrush;
    FStrokingBrushStack: TObjectStack<TdxGPBrush>;
    FFontQuality: Word;
    FUseEmbeddedFontEncoding: Boolean;

    procedure SetGraphics(const AValue: TdxGPCanvas);

    function CalculateBitmapTransformationMatrix(ABitmapWidth, ABitmapHeight: Integer;
      const ABoundingBox: TdxRectF): TdxPDFTransformationMatrix;
    function CalculateLineWidth(const AStartPoint, AEndPoint: TdxPointF): Single;
    function CalculateLineWidthFactor(const AStartPoint, AEndPoint: TdxPointF): Single;
    function CalculatePenWidth(const AStartPoint, AEndPoint: TdxPointF): Single;
    function CalculateRotationAngle(const AStartPoint, AEndPoint: TdxPointF): Single;
    function CalculateVectorLength(X, Y: Single): Single;
    function Clone(ABrush: TdxGPBrush): TdxGPBrush; overload;
    function Clone(ARegion: TdxGPRegion): TdxGPRegion; overload;
    function CreateGraphicsDeviceParameters(ACanvas: TCanvas; const ASize: TSize): TdxPDFRenderParameters;
    function CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm; const ABoundingBox, AClipRect: TdxRectF): TdxSmartImage; overload;
    function CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm; const ABitmapSize: TSize;
      ATransformationMatrix: TdxPDFTransformationMatrix; const AClipRect: TdxRectF): TdxSmartImage; overload;
    function ExtractBackdropBitmap(X, Y, AWidth, AHeight: Integer): TdxSmartImage;
    function IsTextRendering: Boolean; inline;
    function IsVertical: Boolean;
    function GetActualFontName(const ASourceFontName: string): string;
    function GetCurrentPath: TdxPDFGraphicsPath;
    function GetImageDataStorage: TdxPDFDocumentImageDataStorage;
		function GetImageMatrix(ALocation: TdxPointF): TdxPDFTransformationMatrix;
    function GetNonStrokingColorAlpha: Word;
    function GetFillMode(AUseNonzeroWindingRule: Boolean): TdxGPFillMode; inline;
    function GetFontDataStorage: TdxPDFFontDataStorage;
    function GetSystemFontQuality: Word;
    function NeedExtendingLineSize: Boolean;
    function SupportCurrentBlendMode: Boolean;
    function TrimBoundingBox(const ABoundingBox: TdxRectF): TdxRectF;
    function UpdateTextColor: Cardinal;
    procedure ApplySoftMask(ABitmap: TdxSmartImage; ASoftMask: TdxPDFLuminositySoftMask; const AOffset: TdxPointF;
      AHeight: Integer);
    procedure ApplySemitransparentText;
    procedure BlendWithBackground(ABitmap: TdxSmartImage; const ABounds: TRect);
    procedure CalculatePenLineStyle(const AStartPoint, AEndPoint: TdxPointF; APen: TdxGPPen);
    procedure DestroyBrush(var ABrush: TdxGPBrush);
    procedure DoBeginText;
    procedure DoEndText;
    procedure DoDrawString(const AStringData: TdxPDFStringData; const APositions: TDoubleDynArray); overload;
    procedure DoDrawString(const P: TdxPointF; const AGlyphs: TWordDynArray; const ASpacing: TIntegerDynArray); overload;
    procedure DrawBitmap(ABitmap: TdxSmartImage; const ABoundingBox: TdxRectF);
    procedure DrawTilingCell(ABitmap: TcxBitmap32; APattern: TdxPDFTilingPattern; const ASize: TSize;
      AColor: TdxPDFColor); overload;
    procedure InitializeClipRegion;
    procedure InitializeSemitransparentText;
    procedure PerformGraphicsOperation(AProc: TProc);
    procedure PerformRendering(const ABounds: TdxRectF; AProc: TProc);
    procedure SetSmoothingMode(AMode: TdxGPSmoothingMode);
    procedure StrokeLine(const AStartPoint, AEndPoint: TdxPointF; APen: TdxGPPen);
    procedure StrokePath(APath: TdxPDFGraphicsPath; APen: TdxGPPen; APrevSmoothingMode: TdxGPSmoothingMode);
    procedure StrokeRectangle(APath: TdxPDFGraphicsPath; APen: TdxGPPen);
    procedure UpdateBrushes;
    procedure UpdateClip;
    procedure UpdateCurrentFont;
    procedure UpdateNonStrokingBrush;
    procedure UpdatePenDashPattern(APen: TdxGPPen; ALineWidth: Double);
    procedure UpdateSmoothingMode(AIsFilling: Boolean);
    procedure UpdateStrokingBrush;
  protected
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
      AColor: TdxPDFColor): TcxBitmap32; override;
    function GetBoundsOffset: TdxPointF; override;
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF); override;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF); override;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF); override;
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double); override;
    procedure ApplyGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters); override;
    procedure BeginPath(const AStartPoint: TdxPointF); override;
    procedure BeginText; override;
    procedure ClipPaths(AUseNonzeroWindingRule: Boolean); override;
    procedure ClosePath; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DrawImage(AImage: TdxPDFDocumentImage); override;
    procedure DrawShading(AShading: TdxPDFCustomShading); override;
    procedure DrawString(const ALocation: TdxPointF; const AData: TdxPDFStringData;
      const AOffsets: TDoubleDynArray); override;
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm); override;
    procedure EndText; override;
    procedure Initialize; override;
    procedure FillPaths(AUseNonzeroWindingRule: Boolean); override;
    procedure Finalize; override;
    procedure RecreatePaths; override;
    procedure RestoreState; override;
    procedure SaveState; override;
    procedure SetColorForNonStrokingOperations(AColor: TdxPDFColor); override;
    procedure SetColorForStrokingOperations(AColor: TdxPDFColor); override;
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double); override;
    procedure SetTextMatrix(AMatrix: TdxPDFTransformationMatrix); overload; override;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode); override;
    procedure StrokePaths; override;
    procedure TransformPaths; override;

    function CreateGraphics: TdxGPCanvas; virtual;
    procedure DestroyGraphics; virtual;
    procedure InitializeGraphics; virtual;

    class function UpdateWorldTransform(AGraphics: TdxGPCanvas; ATransform: TdxGPMatrix): Boolean;
    function GetBackdropBitmap(const ABoundingBox: TdxRectF; ABitmapWidth, ABitmapHeight: Integer): TdxSmartImage;
    function GetRenderParameters: TdxPDFRenderParameters;
    function UseRectangularGraphicsPath: Boolean;
    procedure DrawTilingCell(APattern: TdxPDFTilingPattern; AColor: TdxPDFColor;
      AParameters: TdxPDFExportParameters); overload;

    property CurrentPath: TdxPDFGraphicsPath read GetCurrentPath;
    property Graphics: TdxGPCanvas read FGraphics write SetGraphics;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage read GetImageDataStorage;
    property FontDataStorage: TdxPDFFontDataStorage read GetFontDataStorage;
    property FontQuality: Word read FFontQuality write FFontQuality;
    property SolidBrushCache: TObjectDictionary<TdxAlphaColor, TdxGPBrush> read FSolidBrushCache;
  end;

  { TdxPDFGraphicsPathSegment }

  TdxPDFGraphicsPathSegment = class
  strict private
    FEndPoint: TdxPointF;
  protected
    function GetFlat: Boolean; virtual; abstract;
    procedure Transform(AMatrix: TdxPDFTransformationMatrix); virtual;

    property Flat: Boolean read GetFlat;
  public
    constructor Create(const AEndPoint: TdxPointF);

    property EndPoint: TdxPointF read FEndPoint;
  end;

  { TdxPDFLineGraphicsPathSegment }

  TdxPDFLineGraphicsPathSegment = class(TdxPDFGraphicsPathSegment)
  protected
    function GetFlat: Boolean; override;
  end;

  { TdxPDFBezierGraphicsPathSegment }

  TdxPDFBezierGraphicsPathSegment = class(TdxPDFGraphicsPathSegment)
  strict private
    FControlPoint1: TdxPointF;
    FControlPoint2: TdxPointF;
  protected
    function GetFlat: Boolean; override;
    procedure Transform(AMatrix: TdxPDFTransformationMatrix); override;
  public
    constructor Create(const AControlPoint1, AControlPoint2, AEndPoint: TdxPointF);

    property ControlPoint1: TdxPointF read FControlPoint1;
    property ControlPoint2: TdxPointF read FControlPoint2;
  end;

  { TdxPDFGraphicsPath }

  TdxPDFGraphicsPath = class
  strict private
    FIsClosed: Boolean;
    FSegments: TObjectList<TdxPDFGraphicsPathSegment>;
    FStartPoint: TdxPointF;

    function GetEndPoint: TdxPointF;
  public
    constructor Create(const AStartPoint: TdxPointF);
    destructor Destroy; override;

    function IsFlat(AIsFilling: Boolean): Boolean; virtual;
    procedure AppendLineSegment(const AEndPoint: TdxPointF);
    procedure AppendBezierSegment(const AControlPoint1, AControlPoint2, AEndPoint: TdxPointF);
    procedure Transform(AMatrix: TdxPDFTransformationMatrix); virtual;

    property EndPoint: TdxPointF read GetEndPoint;
    property IsClosed: Boolean read FIsClosed write FIsClosed;
    property Segments: TObjectList<TdxPDFGraphicsPathSegment>read FSegments;
    property StartPoint: TdxPointF read FStartPoint write FStartPoint;
  end;

  { TdxPDFRectangularGraphicsPath }

  TdxPDFRectangularGraphicsPath = class(TdxPDFGraphicsPath)
  strict private
    FRectangle: TdxRectF;
  protected
    procedure UpdateRectangle(ALeft: Double; ABottom: Double; ARight: Double; ATop: Double);
  public
    constructor Create(ALeft, ABottom, AWidth, AHeight: Double);

    function IsFlat(AIsFilling: Boolean): Boolean; override;
    procedure Transform(AMatrix: TdxPDFTransformationMatrix); override;

    property Rectangle: TdxRectF read FRectangle;
  end;

  { TdxPDFGraphicsPathBuilder }

  TdxPDFGraphicsPathBuilder = class
  strict private
    FInterpreter: TdxPDFRenderingInterpreter;
    FUseRectangularGraphicsPath: Boolean;

    procedure AppendBezier(AGraphicsPath: TdxGPPath; const AStartPoint, AEndPoint: TdxPointF;
      ASegment: TdxPDFBezierGraphicsPathSegment);
    procedure AppendExtendLine(AGraphicsPath: TdxGPPath; const AStartPoint, AEndPoint: TdxPointF;
      ASize: Double; AFromLeft: Boolean);
    procedure AppendPath(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
    procedure AppendSegment(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
    procedure AppendSegments(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
  strict protected
    function CreatePath(APath: TdxPDFGraphicsPath; AExtendSize: Double): TdxGPPath; overload;
    function CreatePath(APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPPath; overload;
    function CreateRegion(APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPRegion;
    function CreateRectangle(APath: TdxPDFGraphicsPath): TdxRectF;
  public
    constructor Create(AInterpreter: TdxPDFRenderingInterpreter);

    class function CreateGPPath(AInterpreter: TdxPDFRenderingInterpreter;
      APath: TdxPDFGraphicsPath; AExtendSize: Double): TdxGPPath; overload; static;
    class function CreateGPPath(AInterpreter: TdxPDFRenderingInterpreter;
      APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPPath; overload; static;
    class function CreateGPRegion(AInterpreter: TdxPDFRenderingInterpreter;
      APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPRegion; static;
  end;

  { TdxPDFGraphicsDevice }

  TdxPDFGraphicsDevice = class(TdxPDFRenderingInterpreter)
  strict private
    procedure ExportBackground;
  protected
    procedure ExportContent(ACommands: TdxPDFReferencedObjects); virtual;
    procedure DoExport(ACommands: TdxPDFReferencedObjects); override;
  end;

  { TdxPDFDocumentContentRecognizer }

  TdxPDFDocumentContentRecognizer = class(TdxPDFCustomCommandInterpreter)
  strict private
    FContent: TdxPDFRecognizedContent;
    FTextParser: TdxPDFTextParser;
  protected
    function CheckRenderingMode: Boolean; override;
    procedure AfterExportPage(APage: TdxPDFPage); override;
    procedure DoExport(ACommands: TdxPDFReferencedObjects); override;
    procedure DrawImage(AImage: TdxPDFDocumentImage); override;
    procedure DrawString(const ALocation: TdxPointF; const AStringData: TdxPDFStringData; const AOffsets: TDoubleDynArray); override;
    procedure InitializeTransformMatrix; override;
  public
    constructor Create(AContent: TdxPDFRecognizedContent); reintroduce;
  end;

  { TdxPDFAttachmentUnpacker }

  TdxPDFAttachmentUnpacker = class(TdxPDFDocumentContentRecognizer)
  protected
    procedure BeforeExportPage(AData: TdxPDFPageData; AParameters: TdxPDFExportParameters); override;
    procedure DoExport(ACommands: TdxPDFReferencedObjects); override;
  end;

implementation

uses
  Math, Forms, dxCore, dxTypeHelpers, dxPDFColorSpace, dxPDFShading, dxFontFile, dxPDFUtils, dxPDFRecognizedObject;

const
  dxPDFMinDisplayedLineWidth = 0.1;

type
  TdxGpImageAttributesAccess = class(TdxGpImageAttributes);
  TdxPDFCustomAnnotationAccess = class(TdxPDFCustomAnnotation);
  TdxPDFCustomSoftMaskAccess = class(TdxPDFCustomSoftMask);
  TdxPDFHyperlinkAccess = class(TdxPDFHyperlink);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFPageAccess = class(TdxPDFPage);
  TdxPDFPageDataAccess = class(TdxPDFPageData);
  TdxPDFRecognizedContentAccess = class(TdxPDFRecognizedContent);
  TdxPDFWidgetAnnotationAccess = class(TdxPDFWidgetAnnotation);

  { TdxPDFBrushHelper }

  TdxPDFBrushHelper = class
  strict private
    class function CreateShadingBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
    class function CreateSolidBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
    class function CreateTextureBrush(ATexture: TcxBitmap32; AAlpha: Double): TdxGPBrush;
    class function CreateTillingBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
  public
    class function CreateBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
  end;

  { TdxPDFImageBlender }

  TdxPDFImageBlender = class
  strict private const
    ComponentCount = 4;
  strict private
    FBitmapHeight: Integer;
    FBitmapWidth: Integer;
    FSourceBitmap: TdxSmartImage;
    FSourceBitmapData: TBitmapData;
    FSourceBitmapDataReleased: Boolean;
    FSourceBitmapStride: Integer;
    FSourceData: TBytes;
    FSourceDataLength: Integer;
    FTargetBitmap: TdxSmartImage;
    FTargetBitmapData: TBitmapData;
    FTargetBitmapDataReleased: Boolean;
    FTargetBitmapStride: Integer;
    FTargetData: TBytes;
    FTargetDataLength: Integer;
    procedure ReleaseBitmaps;
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer; var ATargetData, ASourceData: TBytes); virtual; abstract;
    procedure Blend;
  public
    constructor Create(ATarget, ASource: TdxSmartImage);
    destructor Destroy; override;
  end;

  { TdxPDFBackdropImageBlender }

  TdxPDFBackdropImageBlender = class(TdxPDFImageBlender)
  public
    class function Supports(AMode: TdxPDFBlendMode): Boolean; static;
    class procedure Blend(AInterpreter: TdxPDFRenderingInterpreter; const ABoundingBox: TdxRectF;
      ABitmap: TdxSmartImage); overload; static;
    class procedure Blend(AMode: TdxPDFBlendMode; ABitmap, ABackdropBitmap: TdxSmartImage); overload; static;
  end;

  { TdxPDFMultiplyImageBlender }

  TdxPDFMultiplyImageBlender = class(TdxPDFBackdropImageBlender)
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer; var ATargetData, ASourceData: TBytes); override;
  end;

  { TdxPDFLuminosityMaskBlender }

  TdxPDFLuminosityMaskBlender = class(TdxPDFImageBlender)
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer; var ATargetData, ASourceData: TBytes); override;
  public
    class procedure Blend(ATarget, ASource: TdxSmartImage); overload; static;
  end;

  { TdxPDFBackdropMatrixCalculator }

  TdxPDFBackdropMatrixCalculator = class
  strict private const
    ColumnCount = 7;
    RowCount = 6;
  strict private
    FRows: array of TDoubleDynArray;
    function Solve: TdxGPMatrix;
    procedure Swap(ARowIndex: Integer);
  public
    constructor Create(X, Y: Integer; const APoints: TdxPDFPoints; const ADestinationCoordinates: TIntegerDynArray);
    class function CalculateTransformationMatrix(X, Y: Integer; const APoints: TdxPDFPoints;
      const ADestinationCoordinates: TIntegerDynArray): TdxGPMatrix;
  end;

var
  dxgPDFSystemFontList: TStringList;

function dxPDFSystemFontList: TStringList;
var
  I: Integer;
  AFontName: string;
begin
  if dxgPDFSystemFontList = nil then
  begin
    dxgPDFSystemFontList := TStringList.Create;
    for I := 0 to Screen.Fonts.Count - 1 do
    begin
      AFontName := StringReplace(Screen.Fonts.Strings[I], ' ', '', [rfReplaceAll]);
      dxgPDFSystemFontList.Values[AFontName] := Screen.Fonts.Strings[I];
    end;
  end;
  Result := dxgPDFSystemFontList;
end;

{ TdxPDFBrushHelper }

class function TdxPDFBrushHelper.CreateTillingBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor;
  AAlpha: Double): TdxGPBrush;
var
  ABitmap: TcxBitmap32;
  APattern: TdxPDFTilingPattern;
  ABrushTransform, APatternTransform: TdxPDFTransformationMatrix;
  ASize, ACellSize: TSize;
  M: TdxGPMatrix;
begin
  Result := nil;
  APattern := AColor.Pattern as TdxPDFTilingPattern;
  ASize := cxSize(AInterpreter.TransformSize(APattern.Matrix, dxRectF(0, 0, Abs(APattern.XStep), Abs(APattern.YStep))));
  if (AInterpreter.RotationAngle = 90) or (AInterpreter.RotationAngle = 270) then
    ASize := cxSize(Min(ASize.cy, AInterpreter.ActualSize.cy), Min(ASize.cx, AInterpreter.ActualSize.cx))
  else
    ASize := cxSize(Min(ASize.cx, AInterpreter.ActualSize.cx), Min(ASize.cy, AInterpreter.ActualSize.cy));
  ACellSize := cxSize(AInterpreter.TransformSize(APattern.Matrix, APattern.BoundingBox));
  ABitmap := AInterpreter.CreateTilingBitmap(APattern, ASize, ACellSize, AColor);
  if ABitmap <> nil then
  begin
    ABitmap.Canvas.Lock;
    APatternTransform := TdxPDFTransformationMatrix.Create;
    try
      APatternTransform.Assign(APattern.BoundingBox.Width / ACellSize.cx, 0, 0,
        -Abs(APattern.BoundingBox.Height) / ACellSize.cy, APattern.BoundingBox.Left, APattern.BoundingBox.Top);
      ABrushTransform := TdxPDFTransformationMatrix.MultiplyMatrix(APatternTransform, APattern.Matrix);
      try
        ABrushTransform.Multiply(AInterpreter.TilingTransformMatrix, moAppend);
        ABrushTransform.Multiply(AInterpreter.DeviceTransformMatrix, moAppend);
        Result := CreateTextureBrush(ABitmap, AAlpha);
        M := TdxPDFUtils.ConvertToGpMatrix(ABrushTransform);
        try
          Result.TextureTransform.Assign(M);
        finally
          M.Free;
        end;
      finally
        ABrushTransform.Free;
      end;
    finally
      APatternTransform.Free;
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

class function TdxPDFBrushHelper.CreateBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor;
  AAlpha: Double): TdxGPBrush;
begin
  Result := nil;
  if (AColor <> nil) and (AColor.Pattern <> nil) then
  begin
    if AColor.Pattern is TdxPDFShadingPattern then
      Result := CreateShadingBrush(AInterpreter, AColor, AAlpha)
    else
      if AColor.Pattern is TdxPDFTilingPattern then
        Result := CreateTillingBrush(AInterpreter, AColor, AAlpha);
  end
  else
    Result := CreateSolidBrush(AInterpreter, AColor, AAlpha);
end;

class function TdxPDFBrushHelper.CreateShadingBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor;
  AAlpha: Double): TdxGPBrush;
var
  ABitmap: TcxBitmap32;
begin
  Result := nil;
  ABitmap := TdxPDFShadingPainter.CreateBitmap(AInterpreter, AColor.Pattern as TdxPDFShadingPattern);
  if ABitmap <> nil then
  begin
    ABitmap.Canvas.Lock;
    try
      Result := CreateTextureBrush(ABitmap, AAlpha);
      Result.TextureTransform.Translate(AInterpreter.Bounds.BottomRight);
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

class function TdxPDFBrushHelper.CreateSolidBrush(AInterpreter: TdxPDFRenderingInterpreter; AColor: TdxPDFColor;
  AAlpha: Double): TdxGPBrush;
var
  AAlphaColor: TdxAlphaColor;
begin
  AAlphaColor := TdxPDFUtils.ConvertToAlphaColor(AColor, AAlpha);
  if not AInterpreter.SolidBrushCache.TryGetValue(AAlphaColor, Result) then
  begin
    Result := TdxGPBrush.Create;
    Result.Style := gpbsSolid;
    Result.Color := AAlphaColor;
    AInterpreter.SolidBrushCache.Add(AAlphaColor, Result);
  end;
end;

class function TdxPDFBrushHelper.CreateTextureBrush(ATexture: TcxBitmap32; AAlpha: Double): TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Style := gpbsTexture;
  Result.Texture.Assign(ATexture);
end;

{ TdxPDFImageBlender }

constructor TdxPDFImageBlender.Create(ATarget, ASource: TdxSmartImage);
var
  ABitmapRectangle: TRect;
begin
  inherited Create;

  FTargetBitmap := ATarget;
  FSourceBitmap := ASource;
  FBitmapWidth := FTargetBitmap.Width;
  FBitmapHeight := FTargetBitmap.Height;

  ABitmapRectangle := cxRect(0, 0, FBitmapWidth, FBitmapHeight);

  GdipCheck(GdipBitmapLockBits(FTargetBitmap.Handle, @ABitmapRectangle, 3,
    TdxPDFUtils.ConvertToGpPixelFormat(TdxPDFPixelFormat.pfArgb32bpp), @FTargetBitmapData));
  FTargetBitmapStride := FTargetBitmapData.Stride;
  FTargetDataLength := FTargetBitmapStride * FBitmapHeight;
  SetLength(FTargetData, FTargetDataLength);
  cxCopyData(FTargetBitmapData.Scan0, @FTargetData[0], FTargetDataLength);

  GdipCheck(GdipBitmapLockBits(FSourceBitmap.Handle, @ABitmapRectangle, 1,
    TdxPDFUtils.ConvertToGpPixelFormat(TdxPDFPixelFormat.pfArgb32bpp), @FSourceBitmapData));
  FSourceBitmapStride := FSourceBitmapData.Stride;
  FSourceDataLength := FSourceBitmapStride * FBitmapHeight;
  SetLength(FSourceData, FSourceDataLength);
  cxCopyData(FSourceBitmapData.Scan0, @FSourceData[0], FSourceDataLength);
end;

destructor TdxPDFImageBlender.Destroy;
begin
  ReleaseBitmaps;
  inherited Destroy;
end;

procedure TdxPDFImageBlender.Blend;
var
  ATargetColorData, ASourceColorData: TBytes;
  Y, AInitialTargetOffset, AInitialSourceOffset, X, ATargetOffset, ASourceOffset: Integer;
begin
  SetLength(ATargetColorData, ComponentCount);
  SetLength(ASourceColorData, ComponentCount);
  AInitialTargetOffset := 0;
  AInitialSourceOffset := 0;
  for Y := 0 to FBitmapHeight - 1 do
  begin
    ATargetOffset := AInitialTargetOffset;
    ASourceOffset := AInitialSourceOffset;
    for X := 0 to FBitmapWidth - 1 do
    begin
      BlendPixel(ATargetOffset, ASourceOffset, FTargetData, FSourceData);
      Inc(ATargetOffset, ComponentCount);
      Inc(ASourceOffset, ComponentCount);
    end;
    Inc(AInitialTargetOffset, FTargetBitmapStride);
    Inc(AInitialSourceOffset, FSourceBitmapStride);
  end;
  cxCopyData(@FTargetData[0], FTargetBitmapData.Scan0, FTargetDataLength);
  ReleaseBitmaps;
end;

procedure TdxPDFImageBlender.ReleaseBitmaps;
begin
  if not FSourceBitmapDataReleased then
  begin
    GdipCheck(GdipBitmapUnlockBits(FSourceBitmap.Handle, @FSourceBitmapData));
    FSourceBitmapDataReleased := True;
  end;
  if not FTargetBitmapDataReleased then
  begin
    GdipCheck(GdipBitmapUnlockBits(FTargetBitmap.Handle, @FTargetBitmapData));
    FTargetBitmapDataReleased := True;
  end;
end;

{ TdxPDFBackdropImageBlender }

class function TdxPDFBackdropImageBlender.Supports(AMode: TdxPDFBlendMode): Boolean;
begin
  Result := AMode = bmMultiply;
end;

class procedure TdxPDFBackdropImageBlender.Blend(AInterpreter: TdxPDFRenderingInterpreter;
  const ABoundingBox: TdxRectF; ABitmap: TdxSmartImage);
var
  ABackdropBitmap: TdxSmartImage;
begin
  if Supports(AInterpreter.GraphicsState.Parameters.BlendMode) then
  begin
    ABackdropBitmap := AInterpreter.GetBackdropBitmap(ABoundingBox, ABitmap.Width, ABitmap.Height);
    try
      Blend(AInterpreter.GraphicsState.Parameters.BlendMode, ABitmap, ABackdropBitmap);
    finally
      ABackdropBitmap.Free;
    end;
  end;
end;

class procedure TdxPDFBackdropImageBlender.Blend(AMode: TdxPDFBlendMode; ABitmap, ABackdropBitmap: TdxSmartImage);
var
  ABlender: TdxPDFMultiplyImageBlender;
begin
  if AMode = bmMultiply then
  begin
    ABlender := TdxPDFMultiplyImageBlender.Create(ABitmap, ABackdropBitmap);
    try
      ABlender.Blend;
    finally
      ABlender.Free;
    end;
  end;
end;

{ TdxPDFMultiplyImageBlender }

procedure TdxPDFMultiplyImageBlender.BlendPixel(ATargetOffset, ASourceOffset: Integer; var ATargetData, ASourceData: TBytes);
var
  I: Integer;
begin
  if ASourceData[ASourceOffset + 3] <> 0 then
    for I := 0 to 3 do
    begin
      ATargetData[ATargetOffset] := ASourceData[ASourceOffset] * ATargetData[ATargetOffset] div 255;
      Inc(ATargetOffset);
      Inc(ASourceOffset);
    end;
end;

{ TdxPDFLuminosityMaskBlender }

class procedure TdxPDFLuminosityMaskBlender.Blend(ATarget, ASource: TdxSmartImage);
var
  ABlender: TdxPDFLuminosityMaskBlender;
begin
  ABlender := TdxPDFLuminosityMaskBlender.Create(ATarget, ASource);
  try
    ABlender.Blend;
  finally
    ABlender.Free;
  end;
end;

procedure TdxPDFLuminosityMaskBlender.BlendPixel(ATargetOffset, ASourceOffset: Integer; var ATargetData, ASourceData: TBytes);
var
  AActualTargetOffset: Integer;
begin
  AActualTargetOffset := ATargetOffset + 3;
  ATargetData[AActualTargetOffset] := Byte(ATargetData[AActualTargetOffset] * ASourceData[ASourceOffset] div 255);
end;

{ TdxPDFBackdropMatrixCalculator }

constructor TdxPDFBackdropMatrixCalculator.Create(X, Y: Integer; const APoints: TdxPDFPoints; const ADestinationCoordinates: TIntegerDynArray);
var
  APoint: TdxPointF;
  ARowOffset, I: Integer;
  ATransformedX, ATransformedY: Double;
begin
  inherited Create;
  ARowOffset := 0;
  SetLength(FRows, RowCount);
  for I := 0 to 2 do
  begin
    APoint := APoints[I];
    ATransformedX := APoint.X - X;
    ATransformedY := APoint.Y - Y;

    SetLength(FRows[ARowOffset], 7);
    FRows[ARowOffset][0] := ATransformedX;
    FRows[ARowOffset][1] := 0;
    FRows[ARowOffset][2] := ATransformedY;
    FRows[ARowOffset][3] := 0;
    FRows[ARowOffset][4] := 1;
    FRows[ARowOffset][5] := 0;
    FRows[ARowOffset][6] := ADestinationCoordinates[ARowOffset];
    Inc(ARowOffset);

    SetLength(FRows[ARowOffset], 7);
    FRows[ARowOffset][0] := 0;
    FRows[ARowOffset][1] := ATransformedX;
    FRows[ARowOffset][2] := 0;
    FRows[ARowOffset][3] := ATransformedY;
    FRows[ARowOffset][4] := 0;
    FRows[ARowOffset][5] := 1;
    FRows[ARowOffset][6] := ADestinationCoordinates[ARowOffset];
    Inc(ARowOffset);
  end;
end;

class function TdxPDFBackdropMatrixCalculator.CalculateTransformationMatrix(X, Y: Integer;
  const APoints: TdxPDFPoints; const ADestinationCoordinates: TIntegerDynArray): TdxGPMatrix;
var
  ACalculator: TdxPDFBackdropMatrixCalculator;
begin
  ACalculator := TdxPDFBackdropMatrixCalculator.Create(X, Y, APoints, ADestinationCoordinates);
  try
    Result := ACalculator.Solve;
  finally
    ACalculator.Free;
  end;
end;

function TdxPDFBackdropMatrixCalculator.Solve: TdxGPMatrix;
var
  I, J, K, ALastColumnIndex: Integer;
  ACurrentRow, ANextRow: TDoubleDynArray;
  AFactor, AValue: Double;
  AElements: TSingleDynArray;
begin
  for I := 0 to RowCount - 2 do
  begin
    Swap(I);
    ACurrentRow := FRows[I];
    for J := I + 1 to RowCount - 1 do
    begin
      ANextRow := FRows[J];
      AFactor := -ANextRow[I] / ACurrentRow[I];
      for K := I to ColumnCount - 1 do
        ANextRow[K] := ANextRow[K] + ACurrentRow[K] * AFactor;
    end;
  end;
  SetLength(AElements, RowCount);
  ALastColumnIndex := ColumnCount - 1;
  for I := RowCount - 1 downto 0 do
  begin
    ACurrentRow := FRows[I];
    AValue := ACurrentRow[ALastColumnIndex];
    for J := I + 1 to RowCount - 1 do
      AValue := AValue - ACurrentRow[J] * AElements[J];
    AElements[I] := AValue / ACurrentRow[I];
  end;
  Result := TdxGPMatrix.CreateEx(AElements[0], AElements[1], AElements[2], AElements[3], AElements[4], AElements[5]);
end;

procedure TdxPDFBackdropMatrixCalculator.Swap(ARowIndex: Integer);
var
  ARowWithMaxElement, I: Integer;
  AMaxValue, ACurrentValue: Double;
  ARowToSwap: TDoubleDynArray;
begin
  ARowWithMaxElement := ARowIndex;
  AMaxValue := Abs(FRows[ARowWithMaxElement][ARowIndex]);
  for I := ARowIndex + 1 to RowCount - 1 do
  begin
    ACurrentValue := Abs(FRows[I][ARowIndex]);
    if ACurrentValue > AMaxValue then
    begin
      ARowWithMaxElement := I;
      AMaxValue := ACurrentValue;
    end;
  end;
  ARowToSwap := FRows[ARowIndex];
  FRows[ARowIndex] := FRows[ARowWithMaxElement];
  FRows[ARowWithMaxElement] := ARowToSwap;
end;

{ TdxPDFGraphicsState }

constructor TdxPDFGraphicsState.Create;
var
  AComponents: TDoubleDynArray;
begin
  inherited Create;
  FParameters := TdxPDFGraphicsStateParameters.Create(nil);
  RecreateTextState;

  FDeviceTransformMatrix := TdxPDFTransformationMatrix.Create;
  FTransformMatrix := TdxPDFTransformationMatrix.Create;

  SetLength(AComponents, 1);
  AComponents[0] := 0;
  StrokingColor := TdxPDFColor.Create(AComponents);
  NonStrokingColor := TdxPDFColor.Create(AComponents);

  StrokingColorSpace := TdxPDFGrayDeviceColorSpace.Create(nil);
  NonStrokingColorSpace := TdxPDFGrayDeviceColorSpace.Create(nil);
end;

destructor TdxPDFGraphicsState.Destroy;
begin
  NonStrokingColorSpace := nil;
  StrokingColorSpace := nil;
  NonStrokingColor := nil;
  StrokingColor := nil;
  FreeAndNil(FTransformMatrix);
  FreeAndNil(FDeviceTransformMatrix);
  FreeAndNil(FTextState);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TdxPDFGraphicsState.Assign(AGraphicsState: TdxPDFGraphicsState);
begin
  if AGraphicsState <> nil then
  begin
    ApplyParameters(AGraphicsState.Parameters);
    StrokingColor := AGraphicsState.StrokingColor;
    StrokingColorSpace := AGraphicsState.StrokingColorSpace;
    NonStrokingColor := AGraphicsState.NonStrokingColor;
    NonStrokingColorSpace := AGraphicsState.NonStrokingColorSpace;
    TextState.Assign(AGraphicsState.TextState);
    TransformMatrix.Assign(AGraphicsState.TransformMatrix);
  end;
end;

procedure TdxPDFGraphicsState.ApplyParameters(AParameters: TdxPDFGraphicsStateParameters);
begin
  if AParameters <> nil then
  begin
    Parameters.Assign(AParameters);
    if AParameters.Font <> nil then
    begin
      TextState.Font := AParameters.Font;
      TextState.FontSize := AParameters.FontSize;
    end;
  end;
end;

procedure TdxPDFGraphicsState.RecreateTextState;
begin
  FreeAndNil(FTextState);
  FTextState := TdxPDFTextState.Create;
end;

procedure TdxPDFGraphicsState.SetNonStrokingColor(const AValue: TdxPDFColor);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNonStrokingColor));
end;

procedure TdxPDFGraphicsState.SetNonStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNonStrokingColorSpace));
end;

procedure TdxPDFGraphicsState.SetStrokingColor(const AValue: TdxPDFColor);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FStrokingColor));
end;

procedure TdxPDFGraphicsState.SetStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FStrokingColorSpace));
end;

{ TdxPDFCustomCommandInterpreter }

constructor TdxPDFCustomCommandInterpreter.Create;
begin
  inherited Create;
  CreateSubClasses;
end;

destructor TdxPDFCustomCommandInterpreter.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFCustomCommandInterpreter.Export(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
begin
  TdxPDFPageAccess(APage).Export(Self, AParameters);
end;

function TdxPDFCustomCommandInterpreter.CheckRenderingMode: Boolean;
begin
  Result := TextState.RenderingMode <> trmInvisible;
end;

function TdxPDFCustomCommandInterpreter.CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
  AColor: TdxPDFColor): TcxBitmap32;
begin
  Result := nil;
end;

function TdxPDFCustomCommandInterpreter.GetBoundsOffset: TdxPointF;
begin
  Result := dxPointF(cxNullPoint);
end;

function TdxPDFCustomCommandInterpreter.IsCanceled: Boolean;
begin
  Result := FParameters.IsCanceled;
end;

procedure TdxPDFCustomCommandInterpreter.AfterExport;
begin
  Finalize;
end;

procedure TdxPDFCustomCommandInterpreter.AfterExportPage(APage: TdxPDFPage);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathBezierSegment(const P2, AEndPoint: TdxPointF);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathBezierSegment(const P1, P2, P3: TdxPointF);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathLineSegment(const AEndPoint: TdxPointF);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.AppendRectangle(X, Y, AWidth, AHeight: Double);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.ApplyGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters);
begin
  FGraphicsState.ApplyParameters(AParameters);
end;

procedure TdxPDFCustomCommandInterpreter.BeginPath(const AStartPoint: TdxPointF);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.BeginText;
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := TdxPDFTransformationMatrix.Create;
  try
    SetTextMatrix(AMatrix);
  finally
    AMatrix.Free;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.BeforeExport;
begin
  Initialize;
end;

procedure TdxPDFCustomCommandInterpreter.BeforeExportPage(AData: TdxPDFPageData; AParameters: TdxPDFExportParameters);
begin
  AData.ExtractCommands;
  AParameters.Annotations := AData.Annotations;
end;

procedure TdxPDFCustomCommandInterpreter.ClipPaths(AUseNonzeroWindingRule: Boolean);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.ClosePath;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.CreateSubClasses;
begin
  FActualTextMatrix := TdxPDFTransformationMatrix.Create;
  FGraphicsState := TdxPDFGraphicsState.Create;
  FGraphicsStateStack := TObjectStack<TdxPDFGraphicsState>.Create;
  FTilingTransformMatrix := TdxPDFTransformationMatrix.Create;
  FTilingTransformMatrixStack := TObjectStack<TdxPDFTransformationMatrix>.Create;
end;

procedure TdxPDFCustomCommandInterpreter.DestroySubClasses;
begin
  FreeAndNil(FTilingTransformMatrixStack);
  FreeAndNil(FTilingTransformMatrix);
  FreeAndNil(FGraphicsStateStack);
  FreeAndNil(FGraphicsState);
  FreeAndNil(FActualTextMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.DoExport(ACommands: TdxPDFReferencedObjects);
begin
  ExecuteCommand(ACommands);
  ExportAnnotation;
end;

procedure TdxPDFCustomCommandInterpreter.DrawShading(AShading: TdxPDFCustomShading);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.EndText;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.Export(ACommands: TdxPDFReferencedObjects; AParameters: TdxPDFExportParameters);
begin
  FParameters := AParameters;
  BeforeExport;
  try
    DoExport(ACommands);
  finally
    AfterExport;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.ExportAndPack(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
var
  AData: TdxPDFPageData;
begin
  AParameters.Bounds := APage.Bounds;
  FAngle := APage.CalculateRotationAngle(AParameters.Angle);
  if not AParameters.IsCanceled then
  begin
    AData := TdxPDFPageAccess(APage).Data;
    if AData <> nil then
    begin
      if not AParameters.IsCanceled then
      begin
        BeforeExportPage(AData, AParameters);
        Export(AData.Commands, AParameters);
      end;
      AfterExportPage(APage);
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.Initialize;
var
  AScale: Double;
  ABottomRight, ATopLeft: TdxPointF;
begin
  FBoundsOffset := GetBoundsOffset;
  AScale := FParameters.ScaleFactor;
  if (FAngle = 90) or (FAngle = 270) then
  begin
    FActualSize := cxSize(Trunc(Abs(Bounds.Height) * AScale), Trunc(Abs(Bounds.Width) * AScale));
    ATopLeft := dxPointF(cxNullPoint);
    ABottomRight := dxPointF(Abs(Bounds.Height), -Abs(Bounds.Width));
  end
  else
  begin
    FActualSize := cxSize(Trunc(Abs(Bounds.Width) * AScale), Trunc(Abs(Bounds.Height) * AScale));
    FBoundsOffset.Y := FBoundsOffset.Y + Abs(Bounds.Height) * AScale;
    ATopLeft := dxPointF(0, Abs(Bounds.Height));
    ABottomRight := dxPointF(Abs(Bounds.Width), 0);
  end;
  InitializeClipBounds(ATopLeft, ABottomRight);
  InitializeTransformMatrix;
  InitializeTransformedBoundingBox;
  UpdateTilingTransformationMatrix;
end;

procedure TdxPDFCustomCommandInterpreter.InitializeClipBounds(const ATopLeft, ABottomRight: TdxPointF);
var
  P1, P2: TdxPointF;
begin
  P1 := ToCanvasPoint(ATopLeft);
  P2 := ToCanvasPoint(ABottomRight);
  FClipBounds := dxRectF(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TdxPDFCustomCommandInterpreter.InitializeTransformMatrix;
begin
  case FAngle of
    90:
      FGraphicsState.TransformMatrix.Assign(0, -1, 1, 0, -Bounds.Bottom, Bounds.Left);
    180:
      FGraphicsState.TransformMatrix.Assign(-1, 0, 0, -1, Bounds.Right, Bounds.Top);
    270:
      FGraphicsState.TransformMatrix.Assign(0, 1, -1, 0, Bounds.Top, -Bounds.Right);
  else
    FGraphicsState.TransformMatrix.Assign(1, 0, 0, 1, -Bounds.Left, -Bounds.Bottom);
  end;
end;

procedure TdxPDFCustomCommandInterpreter.FillPaths(AUseNonzeroWindingRule: Boolean);
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.Finalize;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.RecreatePaths;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.RestoreState;
begin
  RestoreGraphicsState;
end;

procedure TdxPDFCustomCommandInterpreter.SaveState;
var
  AState: TdxPDFGraphicsState;
begin
  AState := TdxPDFGraphicsState.Create;
  AState.Assign(FGraphicsState);
  FGraphicsStateStack.Push(AState);
end;

procedure TdxPDFCustomCommandInterpreter.SetColorForNonStrokingOperations(AColor: TdxPDFColor);
begin
  if GraphicsState.NonStrokingColorSpace <> nil then
    FGraphicsState.NonStrokingColor := FGraphicsState.NonStrokingColorSpace.Transform(AColor);
end;

procedure TdxPDFCustomCommandInterpreter.SetColorForStrokingOperations(AColor: TdxPDFColor);
begin
  if FGraphicsState.StrokingColorSpace <> nil then
    FGraphicsState.StrokingColor := FGraphicsState.StrokingColorSpace.Transform(AColor);
end;

procedure TdxPDFCustomCommandInterpreter.SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double);
begin
  FGraphicsState.TextState.Font := AFont;
  FGraphicsState.TextState.FontSize := AFontSize;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextMatrix(AMatrix: TdxPDFTransformationMatrix);
var
  AState: TdxPDFTextState;
begin
  AState := TextState;
  AState.TextMatrix.Assign(AMatrix);
  AState.TextLineMatrix.Assign(AMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.SetTextRenderingMode(AMode: TdxPDFTextRenderingMode);
begin
  TextState.RenderingMode := AMode;
end;

procedure TdxPDFCustomCommandInterpreter.StrokePaths;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.TransformPaths;
begin
// do nothing
end;

procedure TdxPDFCustomCommandInterpreter.UpdateTilingTransformationMatrix;
begin
  if FTilingTransformMatrix <> nil then
    FTilingTransformMatrix.Free;
  FTilingTransformMatrix := TdxPDFTransformationMatrix.CreateEx(TransformMatrix);
end;

function TdxPDFCustomCommandInterpreter.CalculateAngle(const P1, P2: TdxPointF): Single;
begin
  Result := ArcTan2(P2.X - P1.X, P2.Y - P1.Y);
end;

function TdxPDFCustomCommandInterpreter.CalculateAngle(X, Y: Single): Single;
var
  P: TdxPointF;
begin
  P := ActualTextMatrix.Transform(dxPointF(X, Y));
  Result := ArcTan2(P.Y, P.X);
end;

function TdxPDFCustomCommandInterpreter.IsNotRotated: Boolean;
begin
  Result := not TransformMatrix.IsRotated;
end;

function TdxPDFCustomCommandInterpreter.IsType3Font: Boolean;
begin
  Result := FGraphicsState.TextState.Font is TdxPDFType3Font;
end;

function TdxPDFCustomCommandInterpreter.ToCanvasPoint(const P: TdxPointF): TdxPointF;
begin
  Result.X := FBoundsOffset.X + FParameters.ScaleFactor * P.X;
  Result.Y := FBoundsOffset.Y - FParameters.ScaleFactor * P.Y;
end;

function TdxPDFCustomCommandInterpreter.TransformSize(AMatrix: TdxPDFTransformationMatrix;
  const ABoundingBox: TdxRectF): TdxSizeF;

  function CalculateDistance(const APoint1, APoint2: TdxPointF): Double;
  var
    ADx, ADy: Double;
  begin
    ADx := APoint2.X - APoint1.X;
    ADy := APoint2.Y - APoint1.Y;
    Result := ADx * ADx + ADy * ADy;
  end;

var
  ATopLeft, ATopRight, ABottomRight, ABottomLeft: TdxPointF;
begin
  if (ABoundingBox.Width <> 0) and (ABoundingBox.Height <> 0) then
  begin
    ATopLeft := ToCanvasPoint(AMatrix.Transform(ABoundingBox.TopLeft));
    ATopRight := ToCanvasPoint(AMatrix.Transform(ABoundingBox.TopRight));
    ABottomRight := ToCanvasPoint(AMatrix.Transform(ABoundingBox.BottomRight));
    ABottomLeft := ToCanvasPoint(AMatrix.Transform(ABoundingBox.BottomLeft));
    Result.cx := Max(1, Sqrt(Max(CalculateDistance(ATopRight, ATopLeft), CalculateDistance(ABottomRight, ABottomLeft))));
    Result.cy := Max(1, Sqrt(Max(CalculateDistance(ATopLeft, ABottomLeft), CalculateDistance(ABottomRight, ATopRight))));
  end
  else
    Result := dxSizeF(0, 0);
end;

function TdxPDFCustomCommandInterpreter.TransformShadingPoint(APoint: TdxPointF): TdxPointF;
var
  ATransformedPoint: TdxPointF;
begin
  ATransformedPoint := ToCanvasPoint(APoint);
  Result := dxPointF(ATransformedPoint.X - Bounds.Left, ATransformedPoint.Y - Bounds.Bottom);
end;

function TdxPDFCustomCommandInterpreter.VectorLength(AMatrix: TdxPDFTransformationMatrix; X, Y: Single): Single;
var
  P: TdxPointF;
begin
  P := AMatrix.Transform(dxPointF(X, Y));
  Result := Sqrt(P.X * P.X + P.Y * P.Y);
end;

procedure TdxPDFCustomCommandInterpreter.CalculateTextMatrix;
var
  AXForm: TXForm;
begin
  FActualTextMatrix.Assign(FGraphicsState.TextState.TextMatrix);
  FActualTextMatrix.Multiply(FGraphicsState.TransformMatrix, moAppend);
  AXForm := FActualTextMatrix.XForm;
  AXForm.eDx := 0;
  AXForm.eDy := 0;
  FActualTextMatrix.XForm := AXForm;
  FActualTextAngle := CalculateAngle(1, 0);
  FActualTextSkewAngle := Tan(CalculateAngle(0, 1) - FActualTextAngle - PI / 2);
  FActualTextHeightFactor := VectorLength(ActualTextMatrix, 0, 1);
  FActualTextWidthFactor := VectorLength(ActualTextMatrix, 1, 0);
end;

procedure TdxPDFCustomCommandInterpreter.Clip(AUseNonzeroWindingRule: Boolean);
begin
  TransformPaths;
  ClipPaths(AUseNonzeroWindingRule);
  RecreatePaths;
end;

procedure TdxPDFCustomCommandInterpreter.DrawForm(AForm: TdxPDFForm; AResources: TdxPDFResources);
var
  ATilingMatrix: TdxPDFTransformationMatrix;
  I, APreviousGraphicsStateStackLock, ACountToRestore: Integer;
begin
  if not IsCanceled then
  begin
    SaveState;
    ModifyTransformationMatrix(AForm.Matrix);
    ATilingMatrix := TdxPDFTransformationMatrix.Create;
    ATilingMatrix.Assign(FTilingTransformMatrix);
    FTilingTransformMatrixStack.Push(ATilingMatrix);
    APreviousGraphicsStateStackLock := FGraphicsStateStackLock;
    FGraphicsStateStackLock := FGraphicsStateStack.Count;
    try
      UpdateTilingTransformationMatrix;
      AppendRectangle(AForm.BBox.Left, AForm.BBox.Top, AForm.BBox.Width, (AForm.BBox.Height));
      Clip(True);
      ExecuteCommand(AForm.GetCommands);
    finally
      ACountToRestore := FGraphicsStateStack.Count - FGraphicsStateStackLock;
      for I := 0 to ACountToRestore - 1 do
        RestoreGraphicsState;
      FGraphicsStateStackLock := APreviousGraphicsStateStackLock;
      if FTilingTransformMatrixStack.Count > 0 then
      begin
        FTilingTransformMatrix.Free;
        FTilingTransformMatrix := FTilingTransformMatrixStack.Extract;
      end;
      RestoreState;
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.DrawString(const AText: TBytes);
var
  APosition: TDoubleDynArray;
begin
  SetLength(APosition, 0);
  DrawString(AText, APosition);
end;

procedure TdxPDFCustomCommandInterpreter.DrawString(const AText: TBytes; var AOffsets: TDoubleDynArray);

  function GetLocation(AOffset, ARotationAngle: Double): TdxPointF;
  var
    M: TdxPDFTransformationMatrix;
  begin
    M := TextState.CreateTextRenderingMatrix;
    try
      M.Multiply(TransformMatrix, moAppend);
      Result.X := M.E - Cos(ARotationAngle) * AOffset;
      Result.Y := M.F - Sin(ARotationAngle) * AOffset;
    finally
      M.Free;
    end;
  end;

  procedure UpdateTextMatrix(const APositions: TDoubleDynArray; AFirstGlyphOffset, ATextWidthFactor: Double);
  var
    ADelta: Double;
    M, ATextMatrix: TdxPDFTransformationMatrix;
    P: TdxPointF;
  begin
    if Length(APositions) > 0 then
    begin
      if ATextWidthFactor > 0 then
        ADelta := APositions[Length(APositions) - 1] / ATextWidthFactor - AFirstGlyphOffset
      else
        ADelta := 0;
      ATextMatrix := TextState.TextMatrix;
      M := TdxPDFTransformationMatrix.CreateEx(ATextMatrix.A, ATextMatrix.B, ATextMatrix.C, ATextMatrix.D, 0, 0);
      try
        P := M.Transform(dxPointF(ADelta, 0));
      finally
        M.Free;
      end;
      ATextMatrix.Translate(P.X, P.Y, moAppend);
    end;
  end;

var
  AStringData: TdxPDFStringData;
  APositions: TDoubleDynArray;
  AFirstGlyphOffset: Double;
  ATextState: TdxPDFTextState;
  AFont: TdxPDFCustomFont;
begin
  ATextState := TextState;
  AFont := TextStateFont;
  if AFont.Encoding <> nil then
  begin
    AStringData := AFont.Encoding.GetStringData(AText, AOffsets);
    if AStringData.Offsets <> nil then
    begin
      CalculateTextMatrix;
      SetLength(APositions, 0);
      AFirstGlyphOffset := 0;
      if Length(AStringData.Offsets) > 0 then
        AFirstGlyphOffset := AStringData.Offsets[0] * ATextState.AbsoluteFontSize * 0.001;
      AFont.GetGlyphPositions(AStringData, ATextState.FontSize, ATextState.CharacterSpacing,
        ATextState.WordSpacing, ActualTextWidthFactor * ATextState.HorizontalScaling / 100, APositions);
      if CheckRenderingMode then
        DrawString(GetLocation(AFirstGlyphOffset * ActualTextWidthFactor, ActualTextAngle), AStringData, APositions);
      UpdateTextMatrix(APositions, AFirstGlyphOffset, ActualTextWidthFactor);
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.DrawTransparencyGroup(AForm: TdxPDFGroupForm);
begin
  DrawForm(AForm, AForm.Resources);
end;

procedure TdxPDFCustomCommandInterpreter.Export(APages: TdxPDFPages; AParameters: TdxPDFExportParameters);
var
  I: Integer;
begin
  for I := 0 to APages.Count - 1 do
    Export(APages[I], AParameters);
end;

procedure TdxPDFCustomCommandInterpreter.ExecuteCommand(ACommands: TdxPDFReferencedObjects);
begin
  ExecuteCommand(Self, ACommands);
end;

procedure TdxPDFCustomCommandInterpreter.ExecuteCommand(const AInterpreter: IdxPDFCommandInterpreter;
  ACommands: TdxPDFReferencedObjects);
var
  I: Integer;
begin
  for I := 0 to ACommands.Count - 1 do
    if Assigned(FParameters.CancelCallback) and FParameters.CancelCallback then
      Exit
    else
      try
        (ACommands[I] as TdxPDFCustomCommand).Execute(AInterpreter);
      except
        on EdxGdipException do;
        on EdxPDFException do;
        on EdxPDFAbortException do;
        else
          raise;
      end;
end;

procedure TdxPDFCustomCommandInterpreter.ModifyTransformationMatrix(AMatrix: TdxPDFTransformationMatrix);
begin
  FGraphicsState.TransformMatrix.Multiply(AMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.MoveToNextLine;
begin
  SetTextMatrix(dxPointF(0, -TextState.Leading));
end;

procedure TdxPDFCustomCommandInterpreter.SetCharacterSpacing(ASpacing: Single);
begin
  FGraphicsState.TextState.CharacterSpacing := ASpacing;
end;

procedure TdxPDFCustomCommandInterpreter.SetColorSpaceForNonStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
begin
  FGraphicsState.NonStrokingColorSpace := AColorSpace;
end;

procedure TdxPDFCustomCommandInterpreter.SetColorSpaceForStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
begin
  FGraphicsState.StrokingColorSpace := AColorSpace;
end;

procedure TdxPDFCustomCommandInterpreter.SetFlatnessTolerance(AValue: Double);
begin
  FGraphicsState.Parameters.FlatnessTolerance := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineCapStyle(ALineCapStyle: TdxPDFLineCapStyle);
begin
  FGraphicsState.Parameters.LineCapStyle := ALineCapStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineJoinStyle(ALineJoinStyle: TdxPDFLineJoinStyle);
begin
  FGraphicsState.Parameters.LineJoinStyle := ALineJoinStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineStyle(ALineStyle: TdxPDFLineStyle);
begin
  FGraphicsState.Parameters.LineStyle := ALineStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineWidth(ALineWidth: Single);
begin
  FGraphicsState.Parameters.LineWidth := ALineWidth;
end;

procedure TdxPDFCustomCommandInterpreter.SetMiterLimit(AMiterLimit: Single);
begin
  FGraphicsState.Parameters.MiterLimit := AMiterLimit;
end;

procedure TdxPDFCustomCommandInterpreter.SetRenderingIntent(AValue: TdxPDFRenderingIntent);
begin
  GraphicsState.Parameters.RenderingIntent := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextLeading(ALeading: Double);
begin
  FGraphicsState.TextState.Leading := ALeading;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextHorizontalScaling(AValue: Double);
begin
  FGraphicsState.TextState.HorizontalScaling := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextMatrix(const AOffset: TdxPointF);
var
  M: TdxPDFTransformationMatrix;
begin
  M := TdxPDFTransformationMatrix.CreateEx(1, 0, 0, 1, AOffset.X, AOffset.Y);
  try
    M.Multiply(TextState.TextLineMatrix, moAppend);
    SetTextMatrix(M);
  finally
    M.Free;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextRise(AValue: Double);
begin
  TextState.Rise := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetWordSpacing(AWordSpacing: Double);
begin
  FGraphicsState.TextState.WordSpacing := AWordSpacing;
end;

procedure TdxPDFCustomCommandInterpreter.UnknownCommand(const AName: string);
begin
// do nothing
end;

function TdxPDFCustomCommandInterpreter.GetActualSize: TSize;
begin
  Result := FActualSize;
end;

function TdxPDFCustomCommandInterpreter.GetBounds: TdxRectF;
begin
  Result := FParameters.Bounds;
end;

function TdxPDFCustomCommandInterpreter.GetDeviceTransformationMatrix: TdxPDFTransformationMatrix;
var
  AScale: Double;
begin
  AScale := FParameters.ScaleFactor;
  FGraphicsState.DeviceTransformMatrix.Assign(AScale, 0, 0, -AScale, FBoundsOffset.X, FBoundsOffset.Y);
  Result := FGraphicsState.DeviceTransformMatrix;
end;

function TdxPDFCustomCommandInterpreter.GetDocumentState: TdxPDFDocumentState;
begin
  Result := FParameters.DocumentState;
end;

function TdxPDFCustomCommandInterpreter.GetRotationAngle: Single;
begin
  Result := FAngle * PI / 180;
end;

function TdxPDFCustomCommandInterpreter.GetTextState: TdxPDFTextState;
begin
  Result := FGraphicsState.TextState;
end;

function TdxPDFCustomCommandInterpreter.GetTextStateFont: TdxPDFCustomFont;
begin
  Result := TextState.Font as TdxPDFCustomFont;
end;

function TdxPDFCustomCommandInterpreter.GetTransformMatrix: TdxPDFTransformationMatrix;
begin
  Result := FGraphicsState.TransformMatrix;
end;

function TdxPDFCustomCommandInterpreter.NeedDrawAnnotation(AAnnotation: TdxPDFCustomAnnotation): Boolean;
begin
  Result := not IsCanceled and TdxPDFCustomAnnotationAccess(AAnnotation).Visible;
end;

procedure TdxPDFCustomCommandInterpreter.DrawAnnotation(AAnnotation: TdxPDFCustomAnnotation);
var
  AForm: TdxPDFForm;
  AMatrix: TdxPDFTransformationMatrix;
begin
  if NeedDrawAnnotation(AAnnotation) then
  begin
    AForm := TdxPDFCustomAnnotationAccess(AAnnotation).GetAppearanceForm(DocumentState);
    if AForm <> nil then
    begin
      SaveState;
      try
        InitializeTransformMatrix;
        AMatrix := AForm.GetTransformationMatrix(TdxPDFCustomAnnotationAccess(AAnnotation).Rect);
        try
          ModifyTransformationMatrix(AMatrix);
        finally
          AMatrix.Free;
        end;
        DrawForm(AForm, nil);
      finally
        RestoreGraphicsState;
      end;
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.ExportAnnotation;
var
  AAnnotation: TdxPDFReferencedObject;
begin
  if (FParameters.Annotations <> nil) and not IsCanceled then
  begin
    GraphicsState.TransformMatrix.Reset;
    GraphicsState.RecreateTextState;
    for AAnnotation in FParameters.Annotations do
      DrawAnnotation(AAnnotation as TdxPDFCustomAnnotation);
  end;
end;

procedure TdxPDFCustomCommandInterpreter.InitializeTransformedBoundingBox;
var
  ABottomLeft, ATopRight: TdxPointF;
begin
  ABottomLeft := TransformMatrix.Transform(Bounds.BottomLeft);
  ATopRight := TransformMatrix.Transform(Bounds.TopRight);

  SetLength(FTransformedBoundingBox, 4);
  FTransformedBoundingBox[0] := ABottomLeft;
  FTransformedBoundingBox[1] := dxPointF(ABottomLeft.X, ATopRight.Y);
  FTransformedBoundingBox[2] := ATopRight;
  FTransformedBoundingBox[3] := dxPointF(ATopRight.X, ABottomLeft.Y);
end;

procedure TdxPDFCustomCommandInterpreter.RestoreGraphicsState;
begin
  if FGraphicsStateStack.Count > FGraphicsStateStackLock then
  begin
    FGraphicsState.Free;
    FGraphicsState := FGraphicsStateStack.Extract;
  end
end;

{ TdxPDFRenderingInterpreter }

procedure TdxPDFRenderingInterpreter.EndText;
begin
  inherited EndText;
  DoEndText;
  ApplySemitransparentText;
end;

procedure TdxPDFRenderingInterpreter.Initialize;
begin
  FGraphics := CreateGraphics;
  FFontQuality := GetSystemFontQuality;
  FPrevAlpha := 255;
  InitializeGraphics;
  InitializeClipRegion;
  inherited Initialize;
  UpdateClip;
  UpdateBrushes;
end;

procedure TdxPDFRenderingInterpreter.FillPaths(AUseNonzeroWindingRule: Boolean);
var
  APath: TdxGPPath;
begin
  APath := TdxPDFGraphicsPathBuilder.CreateGPPath(Self, FPaths, GetFillMode(AUseNonzeroWindingRule));
  try
    PerformRendering(APath.GetBoundsF,
      procedure
      begin
        FPrevSmoothingMode := Integer(Graphics.SmoothingMode);
        try
          EndText;
          UpdateSmoothingMode(True);
          if FNonStrokingBrush <> nil then
            GdipCheck(GdipFillPath(Graphics.Handle, FNonStrokingBrush.Handle, APath.Handle));
        finally
          SetSmoothingMode(TdxGPSmoothingMode(FPrevSmoothingMode));
        end;
      end);
  finally
    APath.Free;
   end;
end;

procedure TdxPDFRenderingInterpreter.Finalize;
begin
  EndText;
  DestroyGraphics;
  inherited Finalize;
end;

procedure TdxPDFRenderingInterpreter.RecreatePaths;
begin
  FreeAndNil(FPaths);
  FPaths := TObjectList<TdxPDFGraphicsPath>.Create;
end;

procedure TdxPDFRenderingInterpreter.RestoreState;

  function RestoreBrush(AStack: TObjectStack<TdxGPBrush>; ABrush: TdxGPBrush): TdxGPBrush;
  begin
    if AStack.Count > 0 then
    begin
      DestroyBrush(ABrush);
      Result := AStack.Extract;
    end
    else
      Result := ABrush;
  end;

  procedure RestoreBrushes;
  begin
    FStrokingBrush := RestoreBrush(FStrokingBrushStack, FStrokingBrush);
    FNonStrokingBrush := RestoreBrush(FNonStrokingBrushStack, FNonStrokingBrush);
  end;

begin
  RestoreBrushes;
  if FCurrentClipRegion <> nil then
    FCurrentClipRegion.Free;
  if FRegionStack.Count > 0 then
    FCurrentClipRegion := FRegionStack.Extract
  else
    FCurrentClipRegion := nil;
  UpdateClip;
  inherited RestoreState;
end;

procedure TdxPDFRenderingInterpreter.SaveState;

  procedure SaveClipRegion;
  begin
    if FCurrentClipRegion <> nil then
      FRegionStack.Push(Clone(FCurrentClipRegion));
  end;

  procedure SaveBrushes;
  begin
    FStrokingBrushStack.Push(Clone(FStrokingBrush));
    FNonStrokingBrushStack.Push(Clone(FNonStrokingBrush));
  end;

begin
  inherited SaveState;
  SaveClipRegion;
  SaveBrushes;
end;

procedure TdxPDFRenderingInterpreter.SetColorForNonStrokingOperations(AColor: TdxPDFColor);
begin
  inherited SetColorForNonStrokingOperations(AColor);
  UpdateNonStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.SetColorForStrokingOperations(AColor: TdxPDFColor);
begin
  inherited SetColorForStrokingOperations(AColor);
  UpdateStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double);
begin
  inherited SetTextFont(AFont,  AFontSize);
  UpdateCurrentFont;
end;

procedure TdxPDFRenderingInterpreter.SetTextMatrix(AMatrix: TdxPDFTransformationMatrix);
begin
  inherited SetTextMatrix(AMatrix);
  UpdateCurrentFont;
end;

procedure TdxPDFRenderingInterpreter.SetTextRenderingMode(AMode: TdxPDFTextRenderingMode);
begin
  inherited SetTextRenderingMode(AMode);
  UpdateCurrentFont;
end;

function TdxPDFRenderingInterpreter.CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
  AColor: TdxPDFColor): TcxBitmap32;
var
  ABitmap: TcxBitmap32;
  I, J: Integer;
  Y, X: Single;
  AStep: TdxPointF;
  AStepCount, AMaxStepCount: TPoint;
begin
  if (AKeySize.cx = 0) or (AKeySize.cy = 0) or (ASize.cx = 0) or (ASize.cy = 0) then
    Exit(nil);
  Result := TcxBitmap32.CreateSize(ASize.cx, ASize.cy);
  if (ASize.cx >= AKeySize.cx) and (ASize.cy >= AKeySize.cy) then
    DrawTilingCell(Result, APattern, AKeySize, AColor)
  else
  begin
    ABitmap := TcxBitmap32.CreateSize(AKeySize.cx, AKeySize.cy);
    ABitmap.Canvas.Lock;
    try
      DrawTilingCell(ABitmap, APattern, AKeySize, AColor);
      AStepCount := cxPoint(Ceil(AKeySize.cx / ASize.cx - 1), Ceil(AKeySize.cx / ASize.cy - 1));
      AStep := dxPointF(IfThen(APattern.XStep < 0, -ASize.cx, ASize.cx), IfThen(APattern.YStep > 0, -ASize.cy, ASize.cy));
      AMaxStepCount := cxPoint(AStepCount.X * 2, AStepCount.Y * 2);
      Y := -AStep.Y * AStepCount.Y;
      cxPaintCanvas.BeginPaint(Result.Canvas.Handle);
      try
        for I := 0 to AMaxStepCount.Y - 1 do
        begin
          X := -AStep.X * AStepCount.X;
          for J := 0 to AMaxStepCount.X - 1 do
          begin
            X := X + AStep.X;
            cxPaintCanvas.Draw(Trunc(X), Trunc(Y), ABitmap);
          end;
          Y := Y + AStep.Y;
        end;
      finally
        cxPaintCanvas.EndPaint;
      end;
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.GetBoundsOffset: TdxPointF;
begin
  Result := GetRenderParameters.Position;
end;

procedure TdxPDFRenderingInterpreter.AppendPathBezierSegment(const P2, AEndPoint: TdxPointF);
begin
  CurrentPath.AppendBezierSegment(CurrentPath.EndPoint, P2, AEndPoint);
end;

procedure TdxPDFRenderingInterpreter.AppendPathBezierSegment(const P1, P2, P3: TdxPointF);
begin
  CurrentPath.AppendBezierSegment(P1, P2, P3);
end;

procedure TdxPDFRenderingInterpreter.AppendPathLineSegment(const AEndPoint: TdxPointF);
begin
  CurrentPath.AppendLineSegment(AEndPoint);
end;

procedure TdxPDFRenderingInterpreter.AppendRectangle(X, Y, AWidth, AHeight: Double);
begin
  FPaths.Add(TdxPDFRectangularGraphicsPath.Create(X, Y, AWidth, AHeight));
end;

procedure TdxPDFRenderingInterpreter.ApplyGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters);
begin
  inherited ApplyGraphicsStateParameters(AParameters);
  if IsTextRendering and (GetNonStrokingColorAlpha <> FPrevAlpha) then
  begin
    DoEndText;
    ApplySemitransparentText;
    InitializeSemitransparentText;
    DoBeginText;
  end;
  UpdateBrushes;
end;

procedure TdxPDFRenderingInterpreter.BeginPath(const AStartPoint: TdxPointF);
begin
  FPaths.Add(TdxPDFGraphicsPath.Create(AStartPoint));
end;

procedure TdxPDFRenderingInterpreter.BeginText;
begin
  inherited BeginText;
  InitializeSemitransparentText;
  DoBeginText;
end;

procedure TdxPDFRenderingInterpreter.ClipPaths(AUseNonzeroWindingRule: Boolean);
var
  APathRegion: TdxGPRegion;
begin
  if FPaths.Count > 0 then
  begin
    APathRegion := TdxPDFGraphicsPathBuilder.CreateGPRegion(Self, FPaths, GetFillMode(AUseNonzeroWindingRule));
    if APathRegion <> nil then
    begin
      if FCurrentClipRegion = nil then
        FCurrentClipRegion := APathRegion
      else
      begin
        FCurrentClipRegion.CombineRegionRegion(APathRegion, gmIntersect);
        APathRegion.Free;
      end;
      UpdateClip;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.ClosePath;
begin
  if CurrentPath <> nil then
  begin
    if (CurrentPath.StartPoint.X <> CurrentPath.EndPoint.X) or (CurrentPath.StartPoint.Y <> CurrentPath.EndPoint.Y) then
      CurrentPath.AppendLineSegment(CurrentPath.StartPoint);
    CurrentPath.IsClosed := True;
  end;
end;

procedure TdxPDFRenderingInterpreter.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FSolidBrushCache := TObjectDictionary<TdxAlphaColor, TdxGPBrush>.Create([doOwnsValues]);

  FStrokingBrush := TdxGPBrush.Create;
  FNonStrokingBrush := TdxGPBrush.Create;

  FImagePainter := TImagePainter.Create;
  FActualFontNameCache := TdxPDFStringsDictionary.Create;

  FStrokingBrushStack := TObjectStack<TdxGPBrush>.Create;
  FNonStrokingBrushStack := TObjectStack<TdxGPBrush>.Create;

  FPaths := TObjectList<TdxPDFGraphicsPath>.Create;
  FRegionStack := TObjectStack<TdxGPRegion>.Create;

  FImageDataStorage := TdxPDFDocumentImageDataStorage.Create(300);
end;

procedure TdxPDFRenderingInterpreter.DestroySubClasses;

  procedure ClearBrushStack(AStack: TObjectStack<TdxGPBrush>);
  var
    ABrush: TdxGPBrush;
  begin
    while (AStack <> nil) and (AStack.Count > 0) do
    begin
      ABrush := AStack.Extract;
      DestroyBrush(ABrush);
    end;
  end;

begin
  if FImageDataStorage <> nil then
    FreeAndNil(FImageDataStorage);
  if FRegionStack <> nil then
    FreeAndNil(FRegionStack);
  if FPaths <> nil then
    FreeAndNil(FPaths);
  ClearBrushStack(FNonStrokingBrushStack);
  ClearBrushStack(FStrokingBrushStack);
  if FNonStrokingBrushStack <> nil then
    FreeAndNil(FNonStrokingBrushStack);
  if FStrokingBrushStack <> nil then
    FreeAndNil(FStrokingBrushStack);
  FreeAndNil(FActualFontNameCache);
  FreeAndNil(FImagePainter);
  if FClipRegion <> nil then
    FreeAndNil(FClipRegion);
  if FCurrentClipRegion <> nil then
    FreeAndNil(FCurrentClipRegion);
  if FInitialClipRegion <> nil then
    FreeAndNil(FInitialClipRegion);
  DestroyBrush(FNonStrokingBrush);
  DestroyBrush(FStrokingBrush);
  if FSolidBrushCache <> nil then
    FreeAndNil(FSolidBrushCache);
  FreeAndNil(FSemitransparentTextBitmap);
  inherited DestroySubClasses;
end;

procedure TdxPDFRenderingInterpreter.DrawImage(AImage: TdxPDFDocumentImage);
begin
  PerformGraphicsOperation(
    procedure
    var
      AMatrix: TdxPDFTransformationMatrix;
    begin
      AMatrix := GetImageMatrix(ToCanvasPoint(TransformMatrix.Transform(dxPointF(0, 0))));
      AMatrix.Translate(0, -1);
      try
        FImagePainter.Draw(Graphics, AImage, GraphicsState.NonStrokingColor, GraphicsState.Parameters.NonStrokingColorAlpha,
          AMatrix, ImageDataStorage);
      finally
        AMatrix.Free;
      end;
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawShading(AShading: TdxPDFCustomShading);
begin
  PerformGraphicsOperation(
    procedure
    var
      ABitmap: TcxBitmap32;
      AMatrix: TdxGPMatrix;
      ABitmapMatrix: TdxPDFTransformationMatrix;
      AGPImage: TdxSmartImage;
      APoints: TdxPointsF;
      ASavedInterpolationMode: TdxGPInterpolationMode;
    begin
      if not IsCanceled then
      begin
        if AShading.BoundingBox = dxRectF(cxNullRect) then
          TdxPDFShadingPainter.Draw(Graphics, Self, AShading)
        else
        begin
          ABitmap := TdxPDFShadingPainter.CreateBitmap(Self, AShading);
          if ABitmap <> nil then
          begin
            ABitmapMatrix := GetImageMatrix(ToCanvasPoint(TransformMatrix.Transform(AShading.BoundingBox.TopLeft)));
            AMatrix := TdxPDFUtils.ConvertToGpMatrix(ABitmapMatrix);
            AGPImage := TdxSmartImage.CreateFromBitmap(ABitmap);
            SetLength(APoints, 3);
            APoints[0] := dxPointF(0, 0);
            APoints[1] := dxPointF(Abs(AShading.BoundingBox.Width), 0);
            APoints[2] := dxPointF(0, Abs(AShading.BoundingBox.Height));
            Graphics.SaveWorldTransform;
            ASavedInterpolationMode := Graphics.InterpolationMode;
            try
              if UpdateWorldTransform(Graphics, AMatrix) then
              begin
                if IsNotRotated then
                  Graphics.InterpolationMode := imNearestNeighbor;
                GdipDrawImagePoints(Graphics.Handle, AGPImage.Handle, @APoints[0], Length(APoints));
              end;
            finally
              AGPImage.Free;
              AMatrix.Free;
              ABitmapMatrix.Free;
              ABitmap.Free;
              Graphics.InterpolationMode := ASavedInterpolationMode;
              Graphics.RestoreWorldTransform;
            end;
          end
        end;
      end;
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawString(const ALocation: TdxPointF; const AData: TdxPDFStringData;
  const AOffsets: TDoubleDynArray);
var
  AScale, ADiff: Double;
  AAbsoluteHorizontalScaling: Double;
  AOrigin: TdxPointF;
  P: TdxPointF;
  AStringLength, APreviousX, I: Integer;
  ASpacing: TIntegerDynArray;
  ATextState: TdxPDFTextState;
begin
  if not IsType3Font then
  begin
    DoBeginText;
    P := ALocation;
    AScale := FParameters.ScaleFactor;
    ATextState := TextState;
    if ATextState.AbsoluteFontSize * AScale * ActualTextHeightFactor >= 0.5 then
    begin
      AOrigin := ToCanvasPoint(ALocation);
      P := AOrigin;
      ADiff := P.X - AOrigin.X;
      AAbsoluteHorizontalScaling := 100 / ATextState.AbsoluteHorizontalScaling;
      if AAbsoluteHorizontalScaling <> 1 then
      begin
        AScale := AScale * AAbsoluteHorizontalScaling;
        if IsVertical then
          P.Y := P.Y * AAbsoluteHorizontalScaling
        else
          P.X := P.X * AAbsoluteHorizontalScaling;
      end;
      AStringLength := Length(AData.Glyphs);
      SetLength(ASpacing, AStringLength);
      APreviousX := 0;
      for I := 0 to AStringLength - 1 do
      begin
        ASpacing[I] := Round(AOffsets[I] * AScale + ADiff) - APreviousX;
        Inc(APreviousX, ASpacing[I]);
      end;
      DoDrawString(P, AData.Glyphs, ASpacing);
    end;
  end
  else
  begin
    DoEndText;
    DoDrawString(AData, AOffsets)
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawTransparencyGroup(AForm: TdxPDFGroupForm);
var
  AFormBoundingBox, ASoftMaskBoundingBox, AIntersection: TdxRectF;
  ABitmap, ASoftMaskBitmap: TdxSmartImage;
  ASoftMask: TdxPDFCustomSoftMask;
  ATransparencyGroup: TdxPDFGroupForm;
begin
  SaveState;
  try
    ModifyTransformationMatrix(AForm.Matrix);
    AFormBoundingBox := TrimBoundingBox(AForm.BBox);
    if AFormBoundingBox <> dxRectF(cxNullRect) then
    begin
      ABitmap := CreateTransparencyGroupBitmap(AForm, AFormBoundingBox, dxRectF(cxNullRect));
      try
        ASoftMask := GraphicsState.Parameters.SoftMask;
        if (ASoftMask = nil) or (ASoftMask <> nil) and (ASoftMask is TdxPDFEmptySoftMask) then
          TdxPDFBackdropImageBlender.Blend(Self, AFormBoundingBox, ABitmap)
        else
        begin
          ATransparencyGroup := TdxPDFCustomSoftMaskAccess(ASoftMask).TransparencyGroup;
          ASoftMaskBoundingBox := TrimBoundingBox(ATransparencyGroup.BBox);
          if ASoftMaskBoundingBox <> dxRectF(cxNullRect) then
          begin
            AIntersection := TdxPDFUtils.TrimRect(ASoftMaskBoundingBox, AFormBoundingBox);
            if AIntersection = dxRectF(cxNullRect) then
              Exit;
            ASoftMaskBitmap := CreateTransparencyGroupBitmap(ATransparencyGroup, AFormBoundingBox, AIntersection);
            try
              TdxPDFLuminosityMaskBlender.Blend(ABitmap, ASoftMaskBitmap);
            finally
              ASoftMaskBitmap.Free;
            end;
          end;
        end;
        DrawBitmap(ABitmap, AFormBoundingBox);
      finally
        ABitmap.Free;
      end;
    end;
  finally
    RestoreState;
  end;
end;

function TdxPDFRenderingInterpreter.CreateGraphics: TdxGPCanvas;
begin
  if GetRenderParameters.Canvas <> nil then
  begin
    GetRenderParameters.Canvas.Lock;
    Result := dxGpBeginPaint(GetRenderParameters.Canvas.Handle, GetRenderParameters.Rect);
  end
  else
    Result := nil;
end;

procedure TdxPDFRenderingInterpreter.DestroyGraphics;
begin
  if GetRenderParameters.Canvas <> nil then
  begin
    GetRenderParameters.Canvas.Unlock;
    (FGraphics as TdxGPCustomPaintCanvas).EndPaint;
    FreeAndNil(FGraphics);
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeGraphics;
begin
  if Graphics <> nil then
  begin
    GdipCheck(GdipSetPageUnit(Graphics.Handle, guPixel));
    SetSmoothingMode(smAntiAlias);
    GdipCheck(GdipSetPixelOffsetMode(Graphics.Handle, TdxGpPixelOffsetMode.PixelOffsetModeHighQuality));
    Graphics.InterpolationMode := imHighQualityBicubic;
  end;
end;

class function TdxPDFRenderingInterpreter.UpdateWorldTransform(AGraphics: TdxGPCanvas; ATransform: TdxGPMatrix): Boolean;
begin
  Result := (AGraphics <> nil) and (ATransform <> nil);
  if Result then
    Result := GdipSetWorldTransform(AGraphics.Handle, ATransform.Handle) = Ok;
end;

function TdxPDFRenderingInterpreter.GetBackdropBitmap(const ABoundingBox: TdxRectF;
  ABitmapWidth, ABitmapHeight: Integer): TdxSmartImage;
var
  ATransformationMatrix: TdxPDFTransformationMatrix;
  AMatrix: TdxPDFTransformationMatrix;
  AActualTransform: TdxGPMatrix;
  ABoundingBoxWidth, ABoundingBoxHeight: Single;
  APoints: TdxPDFPoints;
  ADestinationPoints: TIntegerDynArray;
  AMinX, AMaxX, AMinY, AMaxY: Double;
  APoint: TdxPointF;
  X, Y: Integer;
  ABackdropGraphics: TdxGPCanvas;
begin
  GdipCheck(GdipFlush(FGraphics.Handle, TdxGpFlushIntention.FlushIntentionFlush));
  ATransformationMatrix := GraphicsState.TransformMatrix;
  AMatrix := GetImageMatrix(ToCanvasPoint(ATransformationMatrix.Transform(ABoundingBox.TopLeft)));
  try
    ABoundingBoxWidth := ABoundingBox.Width;
    ABoundingBoxHeight := Abs(ABoundingBox.Height);
    SetLength(APoints, 4);
    APoints[1] := dxPointF(ABoundingBoxWidth, 0);
    APoints[2] := dxPointF(0, ABoundingBoxHeight);
    APoints[3] := dxPointF(ABoundingBoxWidth, ABoundingBoxHeight);

    APoints := AMatrix.TransformPoints(APoints);
    AMinX := MaxDouble;
    AMaxX := MinDouble;
    AMinY := MaxDouble;
    AMaxY := MinDouble;
    for APoint in APoints do
    begin
      AMinX := TdxPDFUtils.Min(AMinX, APoint.X);
      AMaxX := TdxPDFUtils.Max(AMaxX, APoint.X);
      AMinY := TdxPDFUtils.Min(AMinY, APoint.Y);
      AMaxY := TdxPDFUtils.Max(AMaxY, APoint.Y);
    end;
    X := Trunc(AMinX);
    Y := Trunc(AMinY);
    Result := ExtractBackdropBitmap(X, Y, ABitmapWidth, ABitmapHeight);

    ABackdropGraphics := Result.CreateCanvas;
    try
      ABackdropGraphics.PixelOffsetMode := TdxGpPixelOffsetMode.PixelOffsetModeHalf;
      if IsNotRotated then
        ABackdropGraphics.InterpolationMode := imNearestNeighbor;

      SetLength(ADestinationPoints, 6);
      ADestinationPoints[2] := ABitmapWidth;
      ADestinationPoints[5] := ABitmapHeight;

      AActualTransform := TdxPDFBackdropMatrixCalculator.CalculateTransformationMatrix(X, Y, APoints, ADestinationPoints);
      try
        ABackdropGraphics.SetWorldTransform(AActualTransform);
        ABackdropGraphics.Draw(Result, Result.ClientRect);
      finally
        AActualTransform.Free;
      end;
    finally
      ABackdropGraphics.Free;
    end;
  finally
    AMatrix.Free;
  end;
end;

function TdxPDFRenderingInterpreter.GetRenderParameters: TdxPDFRenderParameters;
begin
  Result := FParameters as TdxPDFRenderParameters;
end;

function TdxPDFRenderingInterpreter.UseRectangularGraphicsPath: Boolean;
begin
  Result := (FPaths.Count <= 1) and IsNotRotated;
end;

procedure TdxPDFRenderingInterpreter.DrawTilingCell(APattern: TdxPDFTilingPattern; AColor: TdxPDFColor;
  AParameters: TdxPDFExportParameters);
var
  M: TdxPDFTransformationMatrix;
  ATempColor: TdxPDFColor;
begin
  FParameters := AParameters;
  if not IsCanceled then
  begin
    Initialize;
    SaveState;
    try
      M := APattern.CreateMatrix(Trunc(Abs(Bounds.Width)), Trunc(Abs(Bounds.Height)));
      try
        ModifyTransformationMatrix(M);
      finally
        M.Free;
      end;

      if not APattern.Colored then
      begin
        ATempColor := TdxPDFColor.Create(AColor.Components);
        try
          SetColorForNonStrokingOperations(ATempColor);
        finally
          ATempColor.Free;
        end;
      end;
      ExecuteCommand(APattern.Commands);
    finally
      RestoreState;
      Finalize;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.CalculateLineWidth(const AStartPoint, AEndPoint: TdxPointF): Single;
var
  AAngle: Double;
begin
  Result := GraphicsState.Parameters.LineWidth;
  if Result < dxPDFMinDisplayedLineWidth then
  begin
    AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
    Result := 1 / (CalculateVectorLength(Cos(AAngle), Sin(AAngle)) * FParameters.ScaleFactor);
  end;
end;

procedure TdxPDFRenderingInterpreter.SetGraphics(const AValue: TdxGPCanvas);
begin
  if FGraphics <> AValue then
  begin
    FGraphics := AValue;
    InitializeGraphics;
    InitializeClipRegion;
    UpdateClip;
    UpdateBrushes;
  end;
end;

function TdxPDFRenderingInterpreter.CalculateBitmapTransformationMatrix(ABitmapWidth, ABitmapHeight: Integer;
  const ABoundingBox: TdxRectF): TdxPDFTransformationMatrix;
var
  AFactor: TdxPointF;
begin
  AFactor.X := ABitmapWidth / ABoundingBox.Width;
  AFactor.Y := ABitmapHeight / Abs(ABoundingBox.Height);
  Result := TdxPDFTransformationMatrix.CreateEx(AFactor.X, 0, 0, AFactor.Y, -ABoundingBox.Left * AFactor.X,
    -ABoundingBox.Bottom * AFactor.Y);
end;

function TdxPDFRenderingInterpreter.CalculateLineWidthFactor(const AStartPoint, AEndPoint: TdxPointF): Single;
var
  AValue, AAngle, ASin, ACos: Single;
begin
  AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
  ASin := Sin(AAngle);
  ACos := Cos(AAngle);
  Result := CalculateVectorLength(ASin, ACos);
  AValue := CalculateVectorLength(ACos, ASin);
  if AValue <> 0 then
    Result := Result / AValue;
end;

function TdxPDFRenderingInterpreter.CalculatePenWidth(const AStartPoint, AEndPoint: TdxPointF): Single;
var
  AAngle: Double;
begin
  AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
  if GraphicsState.Parameters.LineWidth < dxPDFMinDisplayedLineWidth then
    Result := 1
  else
    Result := GraphicsState.Parameters.LineWidth * CalculateVectorLength(Cos(AAngle), Sin(AAngle)) *
      FParameters.ScaleFactor;
end;

function TdxPDFRenderingInterpreter.CalculateRotationAngle(const AStartPoint, AEndPoint: TdxPointF): Single;
begin
  Result := TdxPDFUtils.NormalizeAngle(CalculateAngle(AStartPoint, AEndPoint) + RotationAngle);
end;

function TdxPDFRenderingInterpreter.CalculateVectorLength(X, Y: Single): Single;
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := TdxPDFTransformationMatrix.CreateEx(TransformMatrix.A, TransformMatrix.B, TransformMatrix.C,
    TransformMatrix.D, 0, 0);
  try
    Result := VectorLength(AMatrix, X, Y);
  finally
    AMatrix.Free;
  end;
end;

function TdxPDFRenderingInterpreter.Clone(ABrush: TdxGPBrush): TdxGPBrush;
begin
  if ABrush <> nil then
  begin
    if ABrush.Style = gpbsSolid then
      Result := ABrush
    else
    begin
      Result := TdxGPBrush.Create;
      Result.Assign(ABrush);
    end;
  end
  else
    Result := TdxGPBrush.Create;
end;

function TdxPDFRenderingInterpreter.Clone(ARegion: TdxGPRegion): TdxGPRegion;
begin
  Result := TdxGPRegion.CreateFromRegion(ARegion.Handle);
end;

function TdxPDFRenderingInterpreter.CreateGraphicsDeviceParameters(ACanvas: TCanvas; const ASize: TSize): TdxPDFRenderParameters;
begin
  Result := TdxPDFRenderParameters.Create(DocumentState);
  Result.Canvas := ACanvas;
  Result.Angle := ra0;
  Result.Bounds := dxRectF(0, ASize.cy, ASize.cx, 0);
  Result.Position := cxRectAdjustF(Result.Bounds).TopLeft;
  Result.ScaleFactor := 1;
  Result.CancelCallback := FParameters.CancelCallback;
end;

function TdxPDFRenderingInterpreter.CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm;
  const ABoundingBox, AClipRect: TdxRectF): TdxSmartImage;
var
  ASize: TdxSizeF;
  M: TdxPDFTransformationMatrix;
  P: TPoint;
begin
  ASize := TransformSize(GraphicsState.TransformMatrix, ABoundingBox);
  P.X := Round(ASize.cx);
  P.Y := Round(ASize.cy);
  M := CalculateBitmapTransformationMatrix(P.X, P.Y, ABoundingBox);
  try
    Result := CreateTransparencyGroupBitmap(AForm, cxSize(P.X, P.Y), M, AClipRect);
  finally
    M.Free;
  end;
end;

function TdxPDFRenderingInterpreter.CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm; const ABitmapSize: TSize;
  ATransformationMatrix: TdxPDFTransformationMatrix; const AClipRect: TdxRectF): TdxSmartImage;
var
  ADevice: TdxPDFGraphicsDevice;
  AParameters: TdxPDFRenderParameters;
  ACanvas: TdxGPCanvas;
begin
  Result := TdxSmartImage.CreateSize(ABitmapSize.cx, ABitmapSize.cy);
  ACanvas := Result.CreateCanvas;
  AParameters := CreateGraphicsDeviceParameters(nil, ABitmapSize);
  ADevice := TdxPDFGraphicsDevice.Create;
  try
    ADevice.FParameters := AParameters;
    ADevice.Initialize;
    ADevice.Graphics := ACanvas;
    ADevice.FontQuality := NONANTIALIASED_QUALITY;
    ADevice.ModifyTransformationMatrix(ATransformationMatrix);
    ADevice.UpdateTilingTransformationMatrix;
    if AClipRect <> dxRectF(cxNullRect) then
    begin
      AppendRectangle(AClipRect.Left, AClipRect.Bottom, AClipRect.Width, Abs(AClipRect.Height));
      Clip(True);
    end;
    ADevice.Graphics.Clear(clNone);
    ADevice.ExecuteCommand(AForm.GetCommands);
  finally
    ADevice.AfterExport;
    ADevice.Free;
    AParameters.Free;
    ACanvas.Free;
  end;
end;

function TdxPDFRenderingInterpreter.ExtractBackdropBitmap(X, Y, AWidth, AHeight: Integer): TdxSmartImage;
var
  ADC: HDC;
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(AWidth, AHeight);
  ADC := Graphics.GetHDC;
  try
    cxBitBlt(ABitmap.Canvas.Handle, ADC, ABitmap.ClientRect, cxPoint(X, Y), SRCCOPY);
    Result := TdxSmartImage.CreateFromBitmap(ABitmap);
  finally
    Graphics.ReleaseHDC(ADC);
    ABitmap.Free;
  end;
end;

function TdxPDFRenderingInterpreter.IsTextRendering: Boolean;
begin
  Result := FHDC <> 0;
end;

function TdxPDFRenderingInterpreter.IsVertical: Boolean;
var
  AAngle: Double;
begin
  AAngle := Abs(ActualTextAngle);
  Result := (AAngle > PI / 4) and (AAngle < PI * 3 / 4);
end;

function TdxPDFRenderingInterpreter.GetActualFontName(const ASourceFontName: string): string;
begin
  if not FActualFontNameCache.TryGetValue(ASourceFontName, Result) then
  begin
    Result := dxPDFSystemFontList.Values[ASourceFontName];
    if Result = '' then
      Result := ASourceFontName;
    FActualFontNameCache.Add(ASourceFontName, Result);
  end;
end;

function TdxPDFRenderingInterpreter.GetCurrentPath: TdxPDFGraphicsPath;
begin
  if FPaths.Count = 0 then
    Result := nil
  else
    Result := FPaths.Last;
end;

function TdxPDFRenderingInterpreter.GetImageDataStorage: TdxPDFDocumentImageDataStorage;
begin
  Result := FImageDataStorage;
end;

function TdxPDFRenderingInterpreter.GetImageMatrix(ALocation: TdxPointF): TdxPDFTransformationMatrix;
var
  AScale: Double;
begin
  AScale := FParameters.ScaleFactor;
  Result := TdxPDFTransformationMatrix.Create;
  Result.Assign(GraphicsState.TransformMatrix.A * AScale, -GraphicsState.TransformMatrix.B * AScale,
    -GraphicsState.TransformMatrix.C * AScale, GraphicsState.TransformMatrix.D * AScale, ALocation.X, ALocation.Y);
end;

function TdxPDFRenderingInterpreter.GetNonStrokingColorAlpha: Word;
begin
  Result := TdxPDFUtils.ConvertToByte(GraphicsState.Parameters.NonStrokingColorAlpha * 255);
end;

function TdxPDFRenderingInterpreter.GetFillMode(AUseNonzeroWindingRule: Boolean): TdxGPFillMode;
begin
  if AUseNonzeroWindingRule then
    Result := gpfmWinding
  else
    Result := gpfmAlternate;
end;

function TdxPDFRenderingInterpreter.GetFontDataStorage: TdxPDFFontDataStorage;
begin
  Result := DocumentState.FontDataStorage;
end;

function TdxPDFRenderingInterpreter.GetSystemFontQuality: Word;
const
  CLEARTYPE_QUALITY = 5;
  CLEARTYPE_NATURAL_QUALITY = 6;
begin
  if IsWin2K then
    Result := CLEARTYPE_QUALITY
  else
    if IsWinSevenOrLater then
      Result := CLEARTYPE_NATURAL_QUALITY
    else
      Result := DEFAULT_QUALITY;
end;

function TdxPDFRenderingInterpreter.NeedExtendingLineSize: Boolean;
begin
  Result := GraphicsState.Parameters.LineCapStyle in [lcsProjectingSquare, lcsRound];
end;

function TdxPDFRenderingInterpreter.SupportCurrentBlendMode: Boolean;
begin
  Result := TdxPDFBackdropImageBlender.Supports(GraphicsState.Parameters.BlendMode);
end;

function TdxPDFRenderingInterpreter.TrimBoundingBox(const ABoundingBox: TdxRectF): TdxRectF;
var
  M: TdxPDFTransformationMatrix;
  APoints: TdxPDFPoints;
begin
  M := TdxPDFTransformationMatrix.Invert(GraphicsState.TransformMatrix);
  try
    APoints := M.TransformPoints(TransformedBoundingBox);
  finally
    M.Free;
  end;
  if GraphicsState.TransformMatrix.IsInvertable then
    Result := TdxPDFUtils.TrimRect(ABoundingBox, TdxPDFUtils.CreateBoundingRectangle(APoints))
  else
    Result := ABoundingBox;
end;

procedure TdxPDFRenderingInterpreter.ApplySoftMask(ABitmap: TdxSmartImage; ASoftMask: TdxPDFLuminositySoftMask;
  const AOffset: TdxPointF; AHeight: Integer);
var
  AMaskBitmap: TdxSmartImage;
  ATransform, AMaskTransformation, ATempMaskTransformation: TdxPDFTransformationMatrix;
begin
  ATransform := TdxPDFTransformationMatrix.MultiplyMatrix(GraphicsState.TransformMatrix, DeviceTransformMatrix);
  AMaskTransformation := TdxPDFTransformationMatrix.TranslateMatrix(ATransform, AOffset);
  ATempMaskTransformation := TdxPDFTransformationMatrix.CreateEx(1, 0, 0, -1, 0, AHeight);
  AMaskTransformation.Multiply(ATempMaskTransformation);
  try
    AMaskBitmap := CreateTransparencyGroupBitmap(ASoftMask.TransparencyGroup, ABitmap.Size,
      AMaskTransformation, dxRectF(cxNullRect));
    try
      TdxPDFLuminosityMaskBlender.Blend(ABitmap, AMaskBitmap);
    finally
      AMaskBitmap.Free;
    end;
  finally
    ATempMaskTransformation.Free;
    AMaskTransformation.Free;
    ATransform.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.ApplySemitransparentText;
var
  ARect: TdxRectF;
begin
  if FSemitransparentTextBitmap <> nil then
  begin
    FFontQuality := FPrevFontQuality;
    FGraphics.Free;
    FGraphics := FPrevGraphics;
    FPrevGraphics := nil;
    ARect := cxRectOffset(dxRectF(0, 0, ActualSize.cx, ActualSize.cy), BoundsOffset);
    Graphics.Draw(FSemitransparentTextBitmap, ARect, ARect,
      FImagePainter.CreateAttributes(GraphicsState.Parameters.NonStrokingColorAlpha));
    FreeAndNil(FSemitransparentTextBitmap);
  end;
  FPrevAlpha := 255;
end;

procedure TdxPDFRenderingInterpreter.BlendWithBackground(ABitmap: TdxSmartImage; const ABounds: TRect);
var
  ABackdropBitmap: TdxSmartImage;
begin
  ABackdropBitmap := ExtractBackdropBitmap(ABounds.Left, ABounds.Top, ABounds.Width, ABounds.Height);
  try
    TdxPDFBackdropImageBlender.Blend(GraphicsState.Parameters.BlendMode, ABitmap, ABackdropBitmap);
  finally
    ABackdropBitmap.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.CalculatePenLineStyle(const AStartPoint, AEndPoint: TdxPointF; APen: TdxGPPen);
begin
  APen.Width := CalculatePenWidth(AStartPoint, AEndPoint);
  UpdatePenDashPattern(APen, CalculateLineWidth(AStartPoint, AEndPoint));
  APen.MiterLimit := GraphicsState.Parameters.MiterLimit;
end;

procedure TdxPDFRenderingInterpreter.DestroyBrush(var ABrush: TdxGPBrush);
begin
  if (ABrush <> nil) and not SolidBrushCache.ContainsValue(ABrush) then
    FreeAndNil(ABrush);
end;

procedure TdxPDFRenderingInterpreter.DoBeginText;

  function GetCurrentClipRegionHRgn: HRGN;
  begin
    GdipCheck(GdipGetRegionHRgn(FClipRegion.Handle, Graphics.Handle, Result));
  end;

begin
  if IsTextRendering then
    Exit;
  FPrevClipRegion := GetCurrentClipRegionHRgn;
  FHDC := Graphics.GetHDC;
  UpdateCurrentFont;
  SelectClipRgn(FHDC, FPrevClipRegion);
  FPrevGraphicsMode := SetGraphicsMode(FHDC, GM_ADVANCED);
  FPrevTextAlign := SetTextAlign(FHDC, TA_BASELINE);
  FPrevBkMode := SetBkMode(FHDC, TRANSPARENT);
  FPrevTextColor := UpdateTextColor;
end;

procedure TdxPDFRenderingInterpreter.DoEndText;
begin
  if IsTextRendering then
  begin
    if FPrevTextFont <> 0 then
    begin
      DeleteObject(SelectObject(FHDC, FPrevTextFont));
      FPrevTextFont := 0;
    end;
    SelectClipRgn(FHDC, 0);
    SetTextAlign(FHDC, FPrevTextAlign);
    SetBkMode(FHDC, FPrevBkMode);
    SetTextColor(FHDC, FPrevTextColor);
    SetGraphicsMode(FHDC, FPrevGraphicsMode);
    DeleteObject(FPrevClipRegion);
    Graphics.ReleaseHDC(FHDC);
    FHDC := 0;
  end;
end;

procedure TdxPDFRenderingInterpreter.DoDrawString(const AStringData: TdxPDFStringData;
  const APositions: TDoubleDynArray);
var
  ACurrentTransformationMatrix, ATextMatrix, ATextLineMatrix, AInitialTransformationMatrix: TdxPDFTransformationMatrix;
  AAngle, AAngleCos, AAngleSin, AFontSize: Double;
  ACharProcs: TDictionary<string, TdxPDFReferencedObjects>;
  AEncoding: TdxPDFSimpleFontEncoding;
  ALength, I: Integer;
  ACommands: TdxPDFReferencedObjects;
  AFont: TdxPDFType3Font;
begin
  DoEndText;
  if not IsCanceled then
  begin
    AFont := TextState.Font as TdxPDFType3Font;

    ACurrentTransformationMatrix := TdxPDFTransformationMatrix.CreateEx(GraphicsState.TransformMatrix);
    ATextMatrix := TdxPDFTransformationMatrix.CreateEx(TextState.TextMatrix);
    ATextLineMatrix := TdxPDFTransformationMatrix.CreateEx(TextState.TextLineMatrix);
    AAngle := ActualTextAngle;
    AAngleCos := Cos(AAngle);
    AAngleSin := Sin(AAngle);
    AFontSize := TextState.FontSize;
    AInitialTransformationMatrix := TdxPDFTransformationMatrix.CreateEx(AFontSize * TextState.HorizontalScaling / 100, 0, 0, AFontSize, 0, TextState.Rise);
    AInitialTransformationMatrix.Multiply(AFont.Matrix);
    ATextMatrix.Translate(-AStringData.Offsets[0] * AFontSize * 0.001, 0);
    AInitialTransformationMatrix.Multiply(ATextMatrix, moAppend);
    AInitialTransformationMatrix.Multiply(ACurrentTransformationMatrix, moAppend);

    GraphicsState.TransformMatrix.Assign(AInitialTransformationMatrix);

    ACharProcs := AFont.CharProcs;
    AEncoding := AFont.Encoding as TdxPDFSimpleFontEncoding;
    try
      ALength := Length(AStringData.Glyphs);
      for I := 0 to ALength - 1 do
      begin
        if not IsCanceled then
        begin
          ACommands := nil;
          if ACharProcs.TryGetValue(AEncoding.GetGlyphName(Byte(AStringData.Glyphs[I])), ACommands) and (ACommands <> nil) then
          begin
            SaveState;
            try
              ExecuteCommand(ACommands);
            finally
              RestoreState;
            end;
          end;
          GraphicsState.TransformMatrix.Free;
          GraphicsState.TransformMatrix :=
            TdxPDFTransformationMatrix.TranslateMatrix(AInitialTransformationMatrix, dxPointF(APositions[I] * AAngleCos, APositions[I] * AAngleSin));
        end;
      end;
    finally
      AInitialTransformationMatrix.Free;
      TextState.TextMatrix.Assign(ATextMatrix);
      ATextMatrix.Free;
      TextState.TextLineMatrix.Assign(ATextLineMatrix);
      ATextLineMatrix.Free;
      GraphicsState.TransformMatrix.Assign(ACurrentTransformationMatrix);
      ACurrentTransformationMatrix.Free;
      DoBeginText;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.DoDrawString(const P: TdxPointF; const AGlyphs: TWordDynArray;
  const ASpacing: TIntegerDynArray);

  procedure TextOut(AOptions: Integer; AGlyphs: PSmallInt; AGlyphCount: Integer; ASpacing: PInteger);
  var
    R: TRect;
    ANeedTransform: Boolean;
    AScaling: Double;
    AWorldTransform, ASavedWorldTransform: TXForm;
  begin
    AScaling := TextState.AbsoluteHorizontalScaling / 100;
    ANeedTransform := (AScaling <> 1) or not SameValue(ActualTextSkewAngle, 0.0, 0.001);
    if ANeedTransform then
    begin
      if IsVertical then
        AWorldTransform := TXForm.CreateMatrix(1, -ActualTextSkewAngle, 0, AScaling, 0, P.X * ActualTextSkewAngle)
      else
        AWorldTransform := TXForm.CreateMatrix(AScaling, 0, ActualTextSkewAngle, 1, -P.Y * ActualTextSkewAngle, 0);
      GetWorldTransform(FHDC, ASavedWorldTransform);
      SetWorldTransform(FHDC, AWorldTransform);
    end;
    ExtTextOut(FHDC, TdxPDFUtils.ConvertToInt(P.X), TdxPDFUtils.ConvertToInt(P.Y), AOptions, @R, PChar(AGlyphs),
      AGlyphCount, ASpacing);
    if ANeedTransform then
      SetWorldTransform(FHDC, ASavedWorldTransform);
  end;

var
  AGlyphCount: Integer;
  AIsSymbolic: Boolean;
  AFont: TdxPDFCustomFont;
  ADescriptor: TdxPDFFontDescriptor;
  AStr: TSmallIntDynArray;
  AOptions: UINT;
begin
  AGlyphCount := Length(AGlyphs);
  SetLength(AStr, AGlyphCount);
  cxCopyData(@AGlyphs[0], @AStr[0], 0, 0, AGlyphCount * SizeOf(Word));
  AFont := TextStateFont;
  AOptions := IfThen(AFont.UseGlyphIndexes, ETO_GLYPH_INDEX, ETO_IGNORELANGUAGE);
  if not (AFont.Encoding is TdxPDFSimpleFontEncoding) then
    AFont.UpdateGlyphs(AStr)
  else
  begin
    ADescriptor := AFont.FontDescriptor;
    AIsSymbolic := (ADescriptor <> nil) and ((Integer(ADescriptor.Flags) and Integer(ffSymbolic)) <> 0);
    if FUseEmbeddedFontEncoding or (AFont is TdxPDFTrueTypeFont) and AIsSymbolic then
      AFont.UpdateGlyphs(AStr)
    else
      TdxPDFTrueTypeFont.UpdateGlyphs(AStr, AFont.Encoding as TdxPDFSimpleFontEncoding);
  end;
  TextOut(AOptions, @AStr[0], AGlyphCount, @ASpacing[0]);
end;

procedure TdxPDFRenderingInterpreter.DrawBitmap(ABitmap: TdxSmartImage; const ABoundingBox: TdxRectF);
var
  ABox: TdxRectF;
begin
  ABox := ABoundingBox;
  PerformGraphicsOperation(
    procedure
    var
      AMatrix: TdxGPMatrix;
      APoints: TdxPointsF;
      ASavedInterpolationMode: TdxGPInterpolationMode;
      ATransform: TdxPDFTransformationMatrix;
      AWorldTransform: TdxGPMatrix;
      R: TdxRectF;
    begin
      ASavedInterpolationMode := Graphics.InterpolationMode;
      Graphics.SaveWorldTransform;
      AWorldTransform := Graphics.GetWorldTransform;
      try
        ATransform := GetImageMatrix(ToCanvasPoint(GraphicsState.TransformMatrix.Transform(ABox.TopLeft)));
        AMatrix := TdxPDFUtils.ConvertToGpMatrix(ATransform);
        try
          AWorldTransform.Multiply(AMatrix, MatrixOrderAppend);
          Graphics.SetWorldTransform(AWorldTransform);
        finally
          ATransform.Free;
          AMatrix.Free;
        end;
        if IsNotRotated then
          Graphics.InterpolationMode := imNearestNeighbor;
        SetLength(APoints, 3);
        R := cxRectAdjustF(ABox);
        APoints[0] := dxPointF(0, 0);
        APoints[1] := dxPointF(R.Width, 0);
        APoints[2] := dxPointF(0, R.Height);
        FImagePainter.DrawBitmap(Graphics, ABitmap.Handle, cxSize(ABitmap.Width, ABitmap.Height), APoints,
          GraphicsState.Parameters.NonStrokingColorAlpha);
      finally
        Graphics.InterpolationMode := ASavedInterpolationMode;
        Graphics.RestoreWorldTransform;
      end;
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawTilingCell(ABitmap: TcxBitmap32; APattern: TdxPDFTilingPattern;
  const ASize: TSize; AColor: TdxPDFColor);
var
  ADevice: TdxPDFGraphicsDevice;
  AParameters: TdxPDFRenderParameters;
begin
  if ABitmap <> nil then
  begin
    AParameters := CreateGraphicsDeviceParameters(ABitmap.Canvas, ASize);
    ADevice := TdxPDFGraphicsDevice.Create;
    try
      ADevice.DrawTilingCell(APattern, AColor, AParameters);
    finally
      ADevice.Free;
      AParameters.Free;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeClipRegion;
var
  ARegion, ARegion2: GpRegion;
begin
  if Graphics <> nil then
  begin
    GdipCheck(GdipCreateRegion(ARegion));
    GdipCheck(GdipGetClip(Graphics.Handle, ARegion));
    GdipCheck(GdipCloneRegion(ARegion, ARegion2));
    FInitialClipRegion := TdxGPRegion.CreateFromRegion(ARegion2);
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeSemitransparentText;
begin
  FPrevAlpha := GetNonStrokingColorAlpha;
  if FPrevAlpha <> 255 then
  begin
    FPrevGraphics := Graphics;
    FPrevFontQuality := FFontQuality;
    FSemitransparentTextBitmap := TdxSmartImage.CreateSize(Round(BoundsOffset.X + ActualSize.cx), Round(BoundsOffset.Y + ActualSize.cy));
    Graphics := FSemitransparentTextBitmap.CreateCanvas;
    FFontQuality := NONANTIALIASED_QUALITY;
  end;
end;

procedure TdxPDFRenderingInterpreter.PerformGraphicsOperation(AProc: TProc);
var
  AShouldResetTextDrawing: Boolean;
begin
  AShouldResetTextDrawing := IsTextRendering;
  if AShouldResetTextDrawing then
    DoEndText;
  try
    AProc;
  finally
    if AShouldResetTextDrawing then
      DoBeginText;
  end;
end;

procedure TdxPDFRenderingInterpreter.PerformRendering(const ABounds: TdxRectF; AProc: TProc);
var
  R: TdxRectF;
begin
  R := ABounds;
  PerformGraphicsOperation(
    procedure
    var
      AActualBounds: TRect;
      ASoftMask: TdxPDFLuminositySoftMask;
      AActualGraphics, ABitmapGraphics: TdxGPCanvas;
      ABitmap: TdxSmartImage;
    begin
      if SupportCurrentBlendMode and (GraphicsState.Parameters.SoftMask is TdxPDFLuminositySoftMask) then
      begin
        AActualBounds := cxRect(R);
        ABitmap := TdxSmartImage.CreateSize(AActualBounds);
        ABitmapGraphics := ABitmap.CreateCanvas;
        try
          AActualGraphics := Graphics;
          ABitmapGraphics.TranslateWorldTransform(-AActualBounds.Left, -AActualBounds.Top);
          try
            FGraphics := ABitmapGraphics;
            AProc;
          finally
            FGraphics := AActualGraphics;
          end;
          ASoftMask := TdxPDFLuminositySoftMask(GraphicsState.Parameters.SoftMask);
          if ASoftMask <> nil then
          begin
            ApplySoftMask(ABitmap, ASoftMask, dxPointF(-AActualBounds.Left, -AActualBounds.Top), AActualBounds.Height);
            BlendWithBackground(ABitmap, AActualBounds);
            GdipCheck(GdipDrawImageI(Graphics.Handle, ABitmap.Handle, AActualBounds.Left, AActualBounds.Top));
          end;
        finally
          ABitmapGraphics.Free;
          ABitmap.Free;
        end;
      end
      else
        AProc;
    end);
end;

procedure TdxPDFRenderingInterpreter.SetSmoothingMode(AMode: TdxGPSmoothingMode);
begin
  Graphics.SmoothingMode := AMode;
end;

procedure TdxPDFRenderingInterpreter.StrokeLine(const AStartPoint, AEndPoint: TdxPointF; APen: TdxGPPen);
var
  P1, P2: TdxPointF;
begin
  P1 := ToCanvasPoint(dxPointF(AStartPoint.X, AStartPoint.Y));
  P2 := ToCanvasPoint(dxPointF(AEndPoint.X, AEndPoint.Y));
  Graphics.Line(P1.X, P1.Y, P2.X, P2.Y, APen);
end;

procedure TdxPDFRenderingInterpreter.StrokePath(APath: TdxPDFGraphicsPath; APen: TdxGPPen;
  APrevSmoothingMode: TdxGPSmoothingMode);
begin
  PerformGraphicsOperation(
    procedure
    var
      ALineWidth, ALineWidthFactor: Single;
      AGPPath: TdxGPPath;
      AEndPoint: TdxPointF;
      ABounds: TdxRectF;
      AWholeSmoothingMode: TdxGPSmoothingMode;
    begin
      if APath.Segments.Count > 0 then
      begin
        AEndPoint := APath.Segments[0].EndPoint;
        ALineWidth := 0;
        if NeedExtendingLineSize and not(APath.IsClosed or cxPointIsEqual(APath.StartPoint, APath.EndPoint)) then
        begin
          ALineWidthFactor := CalculateLineWidthFactor(APath.StartPoint, AEndPoint);
          if ALineWidthFactor > 2 then
            ALineWidth := CalculateLineWidth(APath.StartPoint, AEndPoint) * ALineWidthFactor * 0.5
          else
            ALineWidth := 0;
        end
        else
          StrokeRectangle(APath, APen);
        CalculatePenLineStyle(APath.StartPoint, AEndPoint, APen);
        AGPPath := TdxPDFGraphicsPathBuilder.CreateGPPath(Self, APath, ALineWidth);
        AWholeSmoothingMode := Graphics.SmoothingMode;
        try
          if APen.Width < 1 then
            SetSmoothingMode(APrevSmoothingMode);
          ABounds := AGPPath.GetBoundsF;
          if (ABounds.Height >= 0.08) or (ABounds.Width >= 0.08) then
            GdipCheck(GdipDrawPath(Graphics.Handle, APen.Handle, AGPPath.Handle));
        finally
          SetSmoothingMode(AWholeSmoothingMode);
          AGPPath.Free;
        end;
      end;
    end);
end;

procedure TdxPDFRenderingInterpreter.StrokeRectangle(APath: TdxPDFGraphicsPath; APen: TdxGPPen);
var
  R: TdxRectF;
  AExtend, AScaleFactor, ALineWidthFactor, AActualLeft, AActualRight, AActualBottom, AActualTop: Double;
  ALineCap: TdxGPPenLineCapStyle;
begin
  if APath is TdxPDFRectangularGraphicsPath then
  begin
    R := TdxPDFRectangularGraphicsPath(APath).Rectangle;
    ALineWidthFactor := CalculateLineWidthFactor(R.BottomLeft, R.BottomRight);
    if (ALineWidthFactor <= 0.5) or (ALineWidthFactor >= 2) then
    begin
      ALineCap := APen.LineStartCapStyle;
      try
        AScaleFactor := FParameters.ScaleFactor * 2;
        APen.LineStartCapStyle := gpcsFlat;
        APen.LineEndCapStyle := gpcsFlat;

        CalculatePenLineStyle(R.BottomLeft, R.BottomRight, APen);
        AExtend := CalculatePenWidth(R.BottomLeft, R.TopLeft) / AScaleFactor;
        AActualLeft := R.Left - AExtend;
        AActualRight := R.Right + AExtend;
        StrokeLine(dxPointF(AActualLeft, R.Bottom), dxPointF(AActualRight, R.Bottom), APen);
        StrokeLine(dxPointF(AActualLeft, R.Top), dxPointF(AActualRight, R.Top), APen);

        CalculatePenLineStyle(R.BottomLeft, R.TopLeft, APen);
        AExtend := CalculatePenWidth(R.BottomLeft, R.BottomRight) / AScaleFactor;
        AActualBottom := R.Bottom - AExtend;
        AActualTop := R.Top + AExtend;
        StrokeLine(dxPointF(R.Left, AActualBottom), dxPointF(R.Left, AActualTop), APen);
        StrokeLine(dxPointF(R.Right, AActualBottom), dxPointF(R.Right, AActualTop), APen);
      finally
        APen.LineStartCapStyle := ALineCap;
        APen.LineEndCapStyle := ALineCap;
      end;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateBrushes;
begin
  UpdateStrokingBrush;
  UpdateNonStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.UpdateClip;
begin
  if Graphics <> nil then
    PerformGraphicsOperation(
      procedure
      begin
        if FClipRegion <> nil then
          FClipRegion.Free;
        FClipRegion := TdxGPRegion.CreateFromRegion(FInitialClipRegion.Handle);
        FClipRegion.CombineRegionRect(ClipBounds, gmIntersect);
        if FCurrentClipRegion <> nil then
          FClipRegion.CombineRegionRegion(FCurrentClipRegion, gmIntersect);
        Graphics.SetClipRegion(FClipRegion, gmReplace);
        end);
end;

procedure TdxPDFRenderingInterpreter.UpdateCurrentFont;

  function GetTextSizeFactor: TdxSizeF;
  begin
    Result.cx := VectorLength(ActualTextMatrix, 1, 0);
    Result.cy := VectorLength(ActualTextMatrix, 0, 1);
  end;

  function TryUpdate(const AFontName: string; const AData: TdxPDFFontRegistrationData; AHeight, AWidth: Integer): Boolean;
  var
    AAngle, AWeight: Integer;
    AHandle, APreviousFont: THandle;
  begin
    AAngle := Round(ActualTextAngle * 1800 / PI);
    AWeight := IfThen(TextState.RenderingMode = TdxPDFTextRenderingMode.trmFillAndStroke, 700, AData.Weight);
    AHandle := CreateFont(AHeight, AWidth, AAngle, AAngle, AWeight, Cardinal(AData.Italic), 0, 0,
      DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, FFontQuality, AData.PitchAndFamily, PChar(AFontName));
    Result := AHandle <> 0;
    if Result then
    begin
      APreviousFont := SelectObject(FHDC, AHandle);
      if FPrevTextFont = 0 then
        FPrevTextFont := APreviousFont
      else
        DeleteObject(APreviousFont);
    end;
  end;

var
  AData: TdxPDFFontRegistrationData;
  AFontName: string;
  AFontHeight, AFontWidthFactor, AWidthToHeightRatio: Double;
  AHeight, AUpdatedCharWidth: Integer;
  ATextState: TdxPDFTextState;
  ATextMetric: TEXTMETRIC;
  ATextSizeFactor: TdxSizeF;
begin
  ATextState := TextState;
  if IsTextRendering and (ATextState.Font <> nil) and not IsType3Font then
  begin
    CalculateTextMatrix;
    AData := FontDataStorage.Add(TextStateFont);
    FUseEmbeddedFontEncoding := AData.UseEmbeddedEncoding;
    AFontName := GetActualFontName(AData.Name);
    AFontHeight := ATextState.AbsoluteFontSize * FParameters.ScaleFactor;
    AFontWidthFactor := AData.WidthFactor;
    ATextSizeFactor := GetTextSizeFactor;
    AHeight := Round(-AFontHeight * ATextSizeFactor.cy);
    if (AHeight <> 0) and TryUpdate(AFontName, AData, AHeight,
      Round(AFontHeight * AFontWidthFactor * ATextSizeFactor.cx)) and (AFontWidthFactor = 0) then
    begin
      AWidthToHeightRatio := ATextSizeFactor.cx / ATextSizeFactor.cy;
      if AWidthToHeightRatio <> 1 then
      begin
        GetTextMetrics(FHDC, ATextMetric);
        AUpdatedCharWidth := Round(ATextMetric.tmAveCharWidth * AWidthToHeightRatio);
        if AUpdatedCharWidth <> ATextMetric.tmAveCharWidth then
          TryUpdate(AFontName, AData, AHeight, AUpdatedCharWidth);
      end;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateNonStrokingBrush;
begin
  DestroyBrush(FNonStrokingBrush);
  FNonStrokingBrush := TdxPDFBrushHelper.CreateBrush(Self, GraphicsState.NonStrokingColor,
    GraphicsState.Parameters.NonStrokingColorAlpha);
  UpdateTextColor;
end;

procedure TdxPDFRenderingInterpreter.UpdatePenDashPattern(APen: TdxGPPen; ALineWidth: Double);
var
  I, ALength: Integer;
  AData: TSingleDynArray;
  ADashPattern: TDoubleDynArray;
  V: Single;
begin
  if (GraphicsState.Parameters.LineStyle <> nil) and GraphicsState.Parameters.LineStyle.IsDashed then
  begin
    SetLength(ADashPattern, 0);
    TdxPDFUtils.AddData(GraphicsState.Parameters.LineStyle.Pattern, ADashPattern);
    ALength := Length(ADashPattern);
    if (ALength = 2) and (ADashPattern[0] = 0) then
      ADashPattern[1] := ADashPattern[1] - 1;
    SetLength(AData, 0);
    for I := 0 to ALength - 1 do
    begin
      V := ADashPattern[I] / ALineWidth;
      if V <= 0 then
      begin
        if GraphicsState.Parameters.LineCapStyle <> lcsButt then
          TdxPDFUtils.AddValue(1, AData);
      end
      else
        TdxPDFUtils.AddValue(V, AData);
    end;
    ALength := Length(AData);
    if ALength > 0 then
    begin
      if ALength = 1 then
        TdxPDFUtils.AddValue(AData[0], AData);
      GdipCheck(GdipSetPenDashOffset(APen.Handle, GraphicsState.Parameters.LineStyle.Phase));
      APen.SetDashArray(AData);
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateSmoothingMode(AIsFilling: Boolean);
var
  APath: TdxPDFGraphicsPath;
begin
  if UseRectangularGraphicsPath and (GraphicsState.Parameters.SmoothnessTolerance = 0) then
  begin
    for APath in FPaths do
      if not APath.IsFlat(AIsFilling) then
        Exit;
     SetSmoothingMode(smNone);
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateStrokingBrush;
begin
  DestroyBrush(FStrokingBrush);
  FStrokingBrush := TdxPDFBrushHelper.CreateBrush(Self, GraphicsState.StrokingColor,
    GraphicsState.Parameters.StrokingColorAlpha);
end;

function TdxPDFRenderingInterpreter.UpdateTextColor: Cardinal;
var
  ATextColor: Integer;
  AColor: TdxPDFARGBColor;
begin
  AColor := TdxPDFARGBColor.Create(GraphicsState.NonStrokingColor);
  try
    ATextColor := Trunc(AColor.Blue * 255) shl 16 + Trunc(AColor.Green * 255) shl 8 + Trunc(AColor.Red * 255);
  finally
    AColor.Free;
  end;
  Result := SetTextColor(FHDC, ATextColor);
end;

procedure TdxPDFRenderingInterpreter.StrokePaths;

  function CreatePen: TdxGPPen;
  const
    LineCapStyleMap: array[TdxPDFLineCapStyle] of TdxGPPenLineCapStyle = (gpcsFlat, gpcsRound, gpcsSquare);
    LineDashStyleMap: array[TdxPDFLineCapStyle] of TdxGPPenDashCapStyle = (gpdcFlat, gpdcRound, gpdcFlat);
    LineJoinMap: array[TdxPDFLineJoinStyle] of TdxGpLineJoin = (LineJoinMiterClipped, LineJoinRound, LineJoinBevel);
  begin
    Result := TdxGPPen.Create;
    Result.Brush.Assign(FStrokingBrush);
    Result.DashCapStyle := LineDashStyleMap[GraphicsState.Parameters.LineCapStyle];
    Result.LineStartCapStyle := LineCapStyleMap[GraphicsState.Parameters.LineCapStyle];
    Result.LineEndCapStyle := Result.LineStartCapStyle;
    Result.MiterLimit := GraphicsState.Parameters.MiterLimit;
    GdipCheck(GdipSetPenLineJoin(Result.Handle, LineJoinMap[GraphicsState.Parameters.LineJoinStyle]));
  end;

var
  APen: TdxGPPen;
begin
  APen := CreatePen;
  try
    PerformGraphicsOperation(
      procedure
      var
        APath: TdxPDFGraphicsPath;
        APrevSmoothingMode: TdxGPSmoothingMode;
      begin
        APrevSmoothingMode := Graphics.SmoothingMode;
        try
          UpdateSmoothingMode(False);
          for APath in FPaths do
            StrokePath(APath, APen, APrevSmoothingMode);
        finally
          SetSmoothingMode(APrevSmoothingMode);
        end;
      end);
  finally
    APen.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.TransformPaths;
var
  APath: TdxPDFGraphicsPath;
begin
  for APath in FPaths do
    APath.Transform(TransformMatrix);
end;

{ TdxPDFRenderingInterpreter.TImagePainter }

constructor TdxPDFRenderingInterpreter.TImagePainter.Create;
begin
  inherited Create;
  SetLength(FCorrectedDestinationPoints, 3);
  FCorrectedDestinationPoints[0] := dxPointF(-0.0008, -0.0008);
  FCorrectedDestinationPoints[1] := dxPointF(1.0008, -0.0008);
  FCorrectedDestinationPoints[2] := dxPointF(-0.0008, 1.0008);

  SetLength(FDestinationPoints, 3);
  FDestinationPoints[0] := dxPointF(0, 0);
  FDestinationPoints[1] := dxPointF(1, 0);
  FDestinationPoints[2] := dxPointF(0, 1);
end;

destructor TdxPDFRenderingInterpreter.TImagePainter.Destroy;
begin
  SetLength(FDestinationPoints, 0);
  SetLength(FCorrectedDestinationPoints, 0);
  inherited Destroy;
end;

function TdxPDFRenderingInterpreter.TImagePainter.CreateAttributes(AAlpha: Double): TdxGPImageAttributes;
var
  AMatrix: TdxGpColorMatrix;
begin
  AMatrix := dxGpDefaultColorMatrix;
  AMatrix[3, 3] := AAlpha;
  Result := TdxGPImageAttributes.Create;
  Result.SetColorMatrix(@AMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
end;

procedure TdxPDFRenderingInterpreter.TImagePainter.Draw(AGraphics: TdxGPCanvas; AImage:
  TdxPDFDocumentImage; AColor: TdxPDFColor; AAlpha: Double; AMatrix: TdxPDFTransformationMatrix;
  AImageDataStorage: TdxPDFDocumentImageDataStorage);

  function NeedInvertColor: Boolean;
  begin
    Result := (Length(AImage.DecodeRanges) > 0) and (AImage.DecodeRanges[0].Min <> 0);
  end;

var
  ABitmap: GpBitmap;
  AData: TdxPDFDocumentImageData;
  ATransform: TdxGPMatrix;
  APrevInterpolationMode: TdxGPInterpolationMode;
begin
  AData := AImageDataStorage[AImage];
  if AData <> nil then
  begin
    ATransform := TdxGPMatrix.CreateEx(AMatrix.A, AMatrix.B, AMatrix.C, AMatrix.D, AMatrix.E, AMatrix.F);
    try
      ABitmap := ApplyPalette(AData.Bitmap, NeedInvertColor, AData, TdxPDFUtils.ConvertToAlphaColor(AColor, AAlpha), AImage.HasMask);
      APrevInterpolationMode := AGraphics.InterpolationMode;
      AGraphics.SaveWorldTransform;
      try
        if TdxPDFRenderingInterpreter.UpdateWorldTransform(AGraphics, ATransform) then
        begin
          if (AImage.BitsPerComponent > 1) and (AAlpha = 1) and
            (TdxPDFUtils.ConvertToGpPixelFormat(AData.PixelFormat) <> PixelFormat32bppARGB) then
          begin
            AGraphics.InterpolationMode := imNearestNeighbor;
            DrawBitmap(AGraphics, ABitmap, cxSize(AData.Width, AData.Height), FCorrectedDestinationPoints, AAlpha);
            AGraphics.InterpolationMode := imDefault;
            DrawBitmap(AGraphics, ABitmap, cxSize(AData.Width, AData.Height), FDestinationPoints, AAlpha);
          end
          else
          begin
            AGraphics.InterpolationMode := imHighQualityBicubic;
            DrawBitmap(AGraphics, ABitmap, cxSize(AData.Width, AData.Height), FDestinationPoints, AAlpha);
          end;
        end;
      finally
        AGraphics.InterpolationMode := APrevInterpolationMode;
        AGraphics.RestoreWorldTransform;
      end;
    finally
      ATransform.Free;
      AImageDataStorage.CheckCapacity;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.TImagePainter.DrawBitmap(AGraphics: TdxGPCanvas; ABitmap: GpBitmap;
  const ASize: TSize; const APoints: TdxPointsF; AAlpha: Double);
var
  Attributes: TdxGPImageAttributes;
begin
  if AAlpha = 1 then
    DoDraw(AGraphics, ABitmap, APoints)
  else
  begin
    Attributes := CreateAttributes(AAlpha);
    try
      DoDraw(AGraphics, ABitmap, ASize, APoints, Attributes)
    finally
      Attributes.Free;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.TImagePainter.ApplyPalette(ABitmap: GpBitmap; AInvertColor: Boolean;
  AData: TdxPDFDocumentImageData; ANonStrokingColor: TdxAlphaColor; AHasMask: Boolean): GpBitmap;
var
  APalette: PGpColorPalette;
  ASize: Integer;
  I: Integer;
  AColor: TdxPDFARGBColor;
  AEntry: PdxAlphaColor;
  ACurrentColor: TdxAlphaColor;
begin
  Result := ABitmap;
  GdipCheck(GdipGetImagePaletteSize(ABitmap, ASize));
  GetMem(APalette, ASize);
  try
    GdipCheck(GdipGetImagePalette(ABitmap, APalette, ASize));
    if APalette.Count > 0 then
    begin
      Result := TdxPDFDocumentImage.CreateBitmap(AData);
      AEntry := @APalette.Entries;
      if AHasMask then
      begin
        ACurrentColor := ANonStrokingColor;
        if not AInvertColor then
        begin
          AEntry^ := ACurrentColor;
          Inc(AEntry);
          AEntry^ := TdxAlphaColors.FromArgb(0, 0, 0, 0);
        end
        else
        begin
          AEntry^ := TdxAlphaColors.FromArgb(0, 0, 0, 0);
          Inc(AEntry);
          AEntry^ := ACurrentColor;
        end;
      end
      else
        for I := 0 to AData.Palette.Count - 1 do
        begin
          AColor := AData.Palette[I] as TdxPDFARGBColor;
          AEntry^ := TdxAlphaColors.FromArgb(AColor.Alpha, Trunc(AColor.Red), Trunc(AColor.Green), Trunc(AColor.Blue));
          Inc(AEntry);
        end;
      GdipCheck(GdipSetImagePalette(Result, APalette));
    end;
  finally
    FreeMem(APalette);
  end;
end;

procedure TdxPDFRenderingInterpreter.TImagePainter.DoDraw(AGraphics: TdxGPCanvas; ABitmap: GpBitmap;
  const APoints: TdxPointsF);
begin
  GdipCheck(GdipDrawImagePoints(AGraphics.Handle, ABitmap, @APoints[0], Length(APoints)));
end;

procedure TdxPDFRenderingInterpreter.TImagePainter.DoDraw(AGraphics: TdxGPCanvas; ABitmap: GpBitmap;
  const ASize: TSize; const APoints: TdxPointsF; AAttributes: TdxGPImageAttributes);
begin
  GdipCheck(GdipDrawImagePointsRect(AGraphics.Handle, ABitmap, @APoints[0], Length(APoints),  0, 0,
    ASize.cx, ASize.cy, guPixel, TdxGpImageAttributesAccess(AAttributes).Handle, nil, nil));
end;

{ TdxPDFGraphicsPathSegment }

constructor TdxPDFGraphicsPathSegment.Create(const AEndPoint: TdxPointF);
begin
  inherited Create;
  FEndPoint := AEndPoint;
end;

procedure TdxPDFGraphicsPathSegment.Transform(AMatrix: TdxPDFTransformationMatrix);
begin
  FEndPoint := AMatrix.Transform(FEndPoint);
end;

{ TdxPDFLineGraphicsPathSegment }

function TdxPDFLineGraphicsPathSegment.GetFlat: Boolean;
begin
  Result := True;
end;

{ TdxPDFBezierGraphicsPathSegment }

constructor TdxPDFBezierGraphicsPathSegment.Create(const AControlPoint1, AControlPoint2, AEndPoint: TdxPointF);
begin
  inherited Create(AEndPoint);
  FControlPoint1 := AControlPoint1;
  FControlPoint2 := AControlPoint2;
end;

function TdxPDFBezierGraphicsPathSegment.GetFlat: Boolean;
begin
  Result := False;
end;

procedure TdxPDFBezierGraphicsPathSegment.Transform(AMatrix: TdxPDFTransformationMatrix);
begin
  inherited Transform(AMatrix);
  FControlPoint1 := AMatrix.Transform(FControlPoint1);
  FControlPoint2 := AMatrix.Transform(FControlPoint2);
end;

{ TdxPDFGraphicsPath }

constructor TdxPDFGraphicsPath.Create(const AStartPoint: TdxPointF);
begin
  inherited Create;
  FSegments := TObjectList<TdxPDFGraphicsPathSegment>.Create;
  FStartPoint := AStartPoint;
end;

destructor TdxPDFGraphicsPath.Destroy;
begin
  FreeAndNil(FSegments);
  inherited Destroy;
end;

function TdxPDFGraphicsPath.GetEndPoint: TdxPointF;
var
  ACount: Integer;
begin
  ACount := FSegments.Count;
  if ACount = 0 then
    Result := FStartPoint
  else
    Result := FSegments[ACount - 1].EndPoint;
end;

function TdxPDFGraphicsPath.IsFlat(AIsFilling: Boolean): Boolean;
var
  X, Y: Double;
  AEnd: TdxPointDouble;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  X := FStartPoint.X;
  Y := FStartPoint.Y;
  for ASegment in FSegments do
  begin
    if not ASegment.Flat then
      Exit(False);
    AEnd := dxPointDouble(ASegment.EndPoint);
    if (AEnd.X <> X) and (AEnd.Y <> Y) then
      Exit(False);
    X := AEnd.X;
    Y := AEnd.Y;
  end;
  Result := not AIsFilling or FIsClosed or (StartPoint.X = X) and (StartPoint.Y = Y);
end;

procedure TdxPDFGraphicsPath.AppendLineSegment(const AEndPoint: TdxPointF);
begin
  FSegments.Add(TdxPDFLineGraphicsPathSegment.Create(AEndPoint));
end;

procedure TdxPDFGraphicsPath.AppendBezierSegment(const AControlPoint1, AControlPoint2, AEndPoint: TdxPointF);
begin
  FSegments.Add(TdxPDFBezierGraphicsPathSegment.Create(AControlPoint1, AControlPoint2, AEndPoint));
end;

procedure TdxPDFGraphicsPath.Transform(AMatrix: TdxPDFTransformationMatrix);
var
  ASegment: TdxPDFGraphicsPathSegment;
begin
  FStartPoint := AMatrix.Transform(FStartPoint);
  for ASegment in FSegments do
    ASegment.Transform(AMatrix);
end;

{ TdxPDFRectangularGraphicsPath }

constructor TdxPDFRectangularGraphicsPath.Create(ALeft, ABottom, AWidth, AHeight: Double);
var
  ARight, ATop: Double;
begin
  inherited Create(dxPointF(ALeft, ABottom));
  ARight := ALeft + AWidth;
  ATop := ABottom + AHeight;
  AppendLineSegment(dxPointF(ARight, ABottom));
  AppendLineSegment(dxPointF(ARight, ATop));
  AppendLineSegment(dxPointF(ALeft, ATop));
  AppendLineSegment(dxPointF(ALeft, ABottom));
  UpdateRectangle(ALeft, ABottom, ARight, ATop);
  IsClosed := True;
end;

function TdxPDFRectangularGraphicsPath.IsFlat(AIsFilling: Boolean): Boolean;
begin
  Result := True;
end;

procedure TdxPDFRectangularGraphicsPath.UpdateRectangle(ALeft: Double; ABottom: Double; ARight: Double; ATop: Double);
var
  ATemp: Double;
begin
  if ARight < ALeft then
  begin
    ATemp := ALeft;
    ALeft := ARight;
    ARight := ATemp;
  end;
  if ATop < ABottom then
  begin
    ATemp := ABottom;
    ABottom := ATop;
    ATop := ATemp;
  end;
  FRectangle := dxRectF(ALeft, ABottom, ARight, ATop);
end;

procedure TdxPDFRectangularGraphicsPath.Transform(AMatrix: TdxPDFTransformationMatrix);
var
  ABottomLeft, ATopRight: TdxPointF;
begin
  inherited Transform(AMatrix);
  ABottomLeft := AMatrix.Transform(FRectangle.BottomLeft);
  ATopRight := AMatrix.Transform(FRectangle.TopRight);
  UpdateRectangle(ABottomLeft.X, ABottomLeft.Y, ATopRight.X, ATopRight.Y);
end;

{ TdxPDFGraphicsPathBuilder }

constructor TdxPDFGraphicsPathBuilder.Create(AInterpreter: TdxPDFRenderingInterpreter);
begin
  inherited Create;
  FInterpreter := AInterpreter;
  FUseRectangularGraphicsPath := AInterpreter.UseRectangularGraphicsPath;
end;

class function TdxPDFGraphicsPathBuilder.CreateGPPath(AInterpreter: TdxPDFRenderingInterpreter;
  APath: TdxPDFGraphicsPath; AExtendSize: Double): TdxGPPath;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
begin
  ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
  try
    Result := ABuilder.CreatePath(APath, AExtendSize);
  finally
    ABuilder.Free;
  end;
end;

class function TdxPDFGraphicsPathBuilder.CreateGPPath(AInterpreter: TdxPDFRenderingInterpreter;
  APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPPath;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
begin
  ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
  try
    Result := ABuilder.CreatePath(APaths, AFillMode);
  finally
    ABuilder.Free;
  end;
end;

class function TdxPDFGraphicsPathBuilder.CreateGPRegion(AInterpreter: TdxPDFRenderingInterpreter;
  APaths: TObjectList<TdxPDFGraphicsPath>; AFillMode: TdxGPFillMode): TdxGPRegion;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
  R: TdxRectF;
begin
  ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
  try
    R := ABuilder.CreateRectangle(APaths.First);
    if TdxPDFUtils.IsRectEmpty(R) then
      Result := ABuilder.CreateRegion(APaths, AFillMode)
    else
      Result := TdxGPRegion.Create(R);
  finally
    ABuilder.Free;
  end;
end;

function TdxPDFGraphicsPathBuilder.CreateRegion(APaths: TObjectList<TdxPDFGraphicsPath>;
  AFillMode: TdxGPFillMode): TdxGPRegion;
var
  APath: TdxGPPath;
begin
  APath := CreatePath(APaths, AFillMode);
  try
    if APath.GetPointCount = 0 then
      Result := nil
    else
      Result := TdxGPRegion.Create(APath.Handle);
  finally
    APath.Free;
  end;
end;

function TdxPDFGraphicsPathBuilder.CreatePath(APath: TdxPDFGraphicsPath; AExtendSize: Double): TdxGPPath;

  function NeedAppendExtendLine(ASegment: TdxPDFGraphicsPathSegment): Boolean;
  begin
    Result := (AExtendSize > 0) and (ASegment is TdxPDFLineGraphicsPathSegment);
  end;

var
  P: TdxPointF;
  ALastSegmentIndex: Integer;
begin
  Result := TdxGPPath.Create;
  ALastSegmentIndex := APath.Segments.Count - 1;
  if ALastSegmentIndex >= 0 then
  begin
    if NeedAppendExtendLine(APath.Segments[0]) then
      AppendExtendLine(Result, APath.StartPoint, APath.Segments[0].EndPoint, AExtendSize, True);
    AppendPath(Result, APath);
    if NeedAppendExtendLine(APath.Segments[0]) then
    begin
      P := APath.Segments[ALastSegmentIndex].EndPoint;
      if ALastSegmentIndex = 0 then
        P := APath.StartPoint;
      AppendExtendLine(Result, P, APath.Segments[ALastSegmentIndex].EndPoint, AExtendSize, False)
    end;
  end;
end;

function TdxPDFGraphicsPathBuilder.CreatePath(APaths: TObjectList<TdxPDFGraphicsPath>;
  AFillMode: TdxGPFillMode): TdxGPPath;
var
  APath: TdxPDFGraphicsPath;
begin
  Result := TdxGPPath.Create(AFillMode);
  for APath in APaths do
    AppendPath(Result, APath);
end;

function TdxPDFGraphicsPathBuilder.CreateRectangle(APath: TdxPDFGraphicsPath): TdxRectF;

  procedure CompareValues(var V1, V2: Single);
  var
    ATemp: Single;
  begin
    if V1 > V2 then
    begin
      ATemp := V1;
      V1 := V2;
      V2 := ATemp;
    end;
  end;

var
  R: TdxRectF;
  ATopLeft, ABottomRight: TdxPointF;
begin
  Result := dxRectF(cxNullRect);
  if FUseRectangularGraphicsPath and (APath is TdxPDFRectangularGraphicsPath) then
  begin
    R := TdxPDFRectangularGraphicsPath(APath).Rectangle;
    ATopLeft := FInterpreter.ToCanvasPoint(R.TopLeft);
    ABottomRight := FInterpreter.ToCanvasPoint(R.BottomRight);
    CompareValues(ATopLeft.X, ABottomRight.X);
    ABottomRight.X := Max(ABottomRight.X, ATopLeft.X + 1);
    CompareValues(ATopLeft.Y, ABottomRight.Y);
    Result.Left := ATopLeft.X;
    Result.Top := ATopLeft.Y;
    Result.Width := Max(ABottomRight.X - Result.Left, 1);
    Result.Height := Max(ABottomRight.Y - Result.Top, 1);
  end
end;

procedure TdxPDFGraphicsPathBuilder.AppendBezier(AGraphicsPath: TdxGPPath; const AStartPoint, AEndPoint: TdxPointF;
  ASegment: TdxPDFBezierGraphicsPathSegment);
var
  P1, P2: TdxPointF;
begin
  P1 := FInterpreter.ToCanvasPoint(ASegment.ControlPoint1);
  P2 := FInterpreter.ToCanvasPoint(ASegment.ControlPoint2);
  AGraphicsPath.AddBezier(AStartPoint, P1, P2, AEndPoint)
end;

procedure TdxPDFGraphicsPathBuilder.AppendExtendLine(AGraphicsPath: TdxGPPath; const AStartPoint, AEndPoint: TdxPointF;
  ASize: Double; AFromLeft: Boolean);
var
  AAngle: Double;
  AExtendSize, P1, P2: TdxPointF;
begin
  AAngle := FInterpreter.CalculateAngle(AStartPoint, AEndPoint);
  AExtendSize := dxPointF(Sin(AAngle) * ASize, Cos(AAngle) * ASize);
  if AFromLeft then
  begin
    P1 := cxPointOffset(AStartPoint, AExtendSize, False);
    P2 := AStartPoint;
  end
  else
  begin
    P1 := AEndPoint;
    P2 := cxPointOffset(AEndPoint, AExtendSize);
  end;
  P1 := FInterpreter.ToCanvasPoint(P1);
  P2 := FInterpreter.ToCanvasPoint(P2);
  AGraphicsPath.AddLine(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TdxPDFGraphicsPathBuilder.AppendPath(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
var
  R: TdxRectF;
begin
  R := CreateRectangle(APath);
  if R <> cxRectF(cxNullRect) then
    AGPPath.AddRect(R)
  else
    if APath.Segments.Count = 1 then
      AppendSegment(AGPPath, APath)
    else
      AppendSegments(AGPPath, APath);
  if APath.IsClosed then
    AGPPath.FigureFinish
  else
    AGPPath.FigureStart;
end;

procedure TdxPDFGraphicsPathBuilder.AppendSegment(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
var
  P, AStartPoint, AEndPoint: TdxPointF;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  AStartPoint := FInterpreter.ToCanvasPoint(APath.StartPoint);
  ASegment := APath.Segments[0];
  AEndPoint := FInterpreter.ToCanvasPoint(ASegment.EndPoint);
  if not(ASegment is TdxPDFBezierGraphicsPathSegment) then
  begin
    if cxPointIsEqual(AStartPoint, AEndPoint) then
    begin
      if (FInterpreter.RotationAngle = 90) or (FInterpreter.RotationAngle = 270) then
        P := dxPointF(AEndPoint.X, AEndPoint.Y + 1)
      else
        P := dxPointF(AEndPoint.X + 1, AEndPoint.Y);
      AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, P.X, P.Y)
    end
    else
      AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);
  end
  else
    AppendBezier(AGPPath, AStartPoint, AEndPoint, ASegment as TdxPDFBezierGraphicsPathSegment);
end;

procedure TdxPDFGraphicsPathBuilder.AppendSegments(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath);
var
  AStartPoint, AEndPoint: TdxPointF;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  AStartPoint := FInterpreter.ToCanvasPoint(APath.StartPoint);
  for ASegment in APath.Segments do
  begin
    AEndPoint := FInterpreter.ToCanvasPoint(ASegment.EndPoint);
    if not (ASegment is TdxPDFBezierGraphicsPathSegment) then
      AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y)
    else
      AppendBezier(AGPPath, AStartPoint, AEndPoint, ASegment as TdxPDFBezierGraphicsPathSegment);
    AStartPoint := AEndPoint;
  end;
end;

{ TdxPDFGraphicsDevice }

procedure TdxPDFGraphicsDevice.DoExport(ACommands: TdxPDFReferencedObjects);
var
  AParameters: TdxPDFRenderParameters;
begin
  AParameters := GetRenderParameters;
  AParameters.Canvas.Lock;
  try
    ExportBackground;
    ExportContent(ACommands);
  finally
    AParameters.Canvas.Unlock;
  end;
end;

procedure TdxPDFGraphicsDevice.ExportContent(ACommands: TdxPDFReferencedObjects);
begin
  inherited DoExport(ACommands);
end;

procedure TdxPDFGraphicsDevice.ExportBackground;
var
  AParameters: TdxPDFRenderParameters;
  APrevColor: TColor;
begin
  AParameters := GetRenderParameters;
  APrevColor := AParameters.Canvas.Brush.Color;
  try
    AParameters.Canvas.Brush.Color := clWhite;
    AParameters.Canvas.FillRect(AParameters.Rect);
  finally
    AParameters.Canvas.Brush.Color := APrevColor;
  end;
end;

{ TdxPDFDocumentContentRecognizer }

constructor TdxPDFDocumentContentRecognizer.Create(AContent: TdxPDFRecognizedContent);
begin
  inherited Create;
  FContent := AContent;
end;

function TdxPDFDocumentContentRecognizer.CheckRenderingMode: Boolean;
begin
  Result := True;
end;

procedure TdxPDFDocumentContentRecognizer.AfterExportPage(APage: TdxPDFPage);
var
  AAnnotation: TdxPDFReferencedObject;
  AData: TdxPDFPageData;
  AField: TdxPDFAcroFormField;
begin
  inherited AfterExportPage(APage);
  AData := TdxPDFPageAccess(APage).Data;
  for AAnnotation in AData.Annotations do
  begin
    AField := TdxPDFCustomAnnotationAccess(AAnnotation).CreateField(DocumentState);
    if AField <> nil then
      case AField.HitCode of
        hcHyperlink:
          FContent.Hyperlinks.Add(TdxPDFHyperlink(AField));
        hcAnnotationObject, hcAttachment:
          TdxPDFRecognizedContentAccess(FContent).AddAnnotationField(TdxPDFAnnotationField(AField));
      else
        FContent.AcroFormFields.Add(AField);
      end;
  end;
end;

procedure TdxPDFDocumentContentRecognizer.DoExport(ACommands: TdxPDFReferencedObjects);
begin
  FTextParser := TdxPDFTextParser.Create(FParameters.Bounds, FContent);
  try
    inherited DoExport(ACommands);
    FTextParser.Parse;
  finally
    FTextParser.Free;
  end;
end;

procedure TdxPDFDocumentContentRecognizer.DrawImage(AImage: TdxPDFDocumentImage);
var
  ACorners: TdxPointsF;
  ACorner, P, ABottomLeft: TdxPointF;
  I: Integer;
  X, Y, AMinX, AMinY, AMaxX, AMaxY: Single;
  ATempImage: TdxPDFImage;
begin
  SetLength(ACorners, 3);
  ACorners[0] := dxPointF(0, 1);
  ACorners[1] := dxPointF(1, 0);
  ACorners[2] := dxPointF(1, 1);

  ABottomLeft := dxPointF(cxNullPoint);
  P := TransformMatrix.Transform(ABottomLeft);
  AMinX := P.X;
  AMaxX := AMinX;
  AMinY := P.Y;
  AMaxY := AMinY;
  for I := 0 to 2 do
  begin
    ACorner := ACorners[I];
    P := TransformMatrix.Transform(ACorner);
    X := P.X;
    if X < AMinX then
      AMinX := X
    else
      if X > AMaxX then
        AMaxX := X;
    Y := P.Y;
    if Y < AMinY then
      AMinY := Y
    else
      if Y > AMaxY then
        AMaxY := Y;
  end;
  ATempImage := TdxPDFImage.Create;
  TdxPDFImageAccess(ATempImage).SetBounds(dxRectF(AMinX, AMinY, AMaxX, AMaxY));
  TdxPDFImageAccess(ATempImage).DocumentImage := AImage;
  FContent.Images.Insert(0, ATempImage);
end;

procedure TdxPDFDocumentContentRecognizer.DrawString(const ALocation: TdxPointF; const AStringData: TdxPDFStringData;
  const AOffsets: TDoubleDynArray);
begin
  FTextParser.AddBlock(AStringData, TextState, ActualTextWidthFactor, ActualTextHeightFactor, AOffsets, ALocation,
    ActualTextAngle);
end;

procedure TdxPDFDocumentContentRecognizer.InitializeTransformMatrix;
begin
  GraphicsState.TransformMatrix.Assign(1, 0, 0, 1, -FParameters.Bounds.Left, -FParameters.Bounds.Bottom);
end;

{ TdxPDFAttachmentUnpacker }

procedure TdxPDFAttachmentUnpacker.BeforeExportPage(AData: TdxPDFPageData; AParameters: TdxPDFExportParameters);
begin
// do nothing;
end;

procedure TdxPDFAttachmentUnpacker.DoExport(ACommands: TdxPDFReferencedObjects);
begin
// do nothing;
end;

initialization

finalization
  FreeAndNil(dxgPDFSystemFontList);

end.



