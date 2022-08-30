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

unit dxPDFCore;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Graphics, Windows, Classes, Controls, Generics.Defaults, Generics.Collections, dxCoreClasses,
  cxGraphics, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses, dxProtectionUtils, dxThreading, dxPDFParser,
  dxPDFStreamFilter, dxPDFCharacterMapping, dxPDFBase, dxPDFTypes, dxPDFText, dxFontFile, dxPDFEncryption,
  dxPDFRecognizedObject;

const
  // HitTests bits
  hcButton = 2;
  hcHyperlink = 8;
  hcText = 4;
  hcTextBox = 12;
  hcAnnotationObject = 16;
  hcAttachment = 8192;

  dxPDFDocumentFontCacheSize: Integer = 32; // for internal use

type
  TdxPDFAcroFormActionField = class;
  TdxPDFAcroFormField = class;
  TdxPDFAcroFormFieldClass = class of TdxPDFAcroFormField;
  TdxPDFCatalog = class;
  TdxPDFCustomAction = class;
  TdxPDFCustomAnnotation = class;
  TdxPDFCustomColorSpace = class;
  TdxPDFCustomColorSpaceTransformation = class;
  TdxPDFCustomColorSpaceTransformationClass = class of TdxPDFCustomColorSpaceTransformation;
  TdxPDFCustomCommand = class;
  TdxPDFCustomCommandClass = class of TdxPDFCustomCommand;
  TdxPDFCustomDestination = class;
  TdxPDFCustomEncoding = class;
  TdxPDFCustomFont = class;
  TdxPDFCustomShading = class;
  TdxPDFCustomSoftMask = class;
  TdxPDFDocumentImage = class;
  TdxPDFDocumentImageData = class;
  TdxPDFDocumentImageDataStorage = class;
  TdxPDFDocumentRepository = class;
  TdxPDFDocumentState = class;
  TdxPDFFileAttachment = class;
  TdxPDFFileAttachmentList = class;
  TdxPDFFontDataStorage = class;
  TdxPDFForm = class;
  TdxPDFGraphicsStateParameters = class;
  TdxPDFGroupForm = class;
  TdxPDFHyperlink = class;
  TdxPDFHyperlinkList = class;
  TdxPDFInteractiveForm = class;
  TdxPDFInteractiveFormFieldCollection = class;
  TdxPDFInteractiveFormField = class;
  TdxPDFLineStyle = class;
  TdxPDFObject = class;
  TdxPDFObjectClass = class of TdxPDFObject;
  TdxPDFPage = class;
  TdxPDFPages = class;
  TdxPDFPageTreeObject = class;
  TdxPDFPageTreeObjectList = class;
  TdxPDFReaderDictionary = class;
  TdxPDFRecognizedContent = class;
  TdxPDFResources = class;
  TdxPDFTilingPattern = class;

  TdxPDFAnnotationFlags = (afNone = $000, afInvisible = $001, afHidden = $002, afPrint = $004, afNoZoom = $008,
    afNoRotate = $010, afNoView = $020, afReadOnly = $040, afLocked = $080, afToggleNoView = $100, afLockedContents = $200); // for internal use
  TdxPDFAnnotationHighlightingMode = (ahmNone, ahmInvert, ahmOutline, ahmPush, ahmToggle); // for internal use
  TdxPDFAssociatedFileRelationship = (frSource, frData, frAlternative, frSupplement, frEncryptedPayload, frUnspecified);
  TdxPDFInteractiveFormFieldFlags = (ffNone = $0000000, ffReadOnly = $0000001, ffRequired = $0000002,
    ffNoExport = $0000004, ffMultiline = $0001000, ffPassword = $0002000, ffNoToggleToOff = $0004000,
    ffRadio = $0008000, ffPushButton = $0010000, ffCombo = $0020000, ffEdit = $0040000, ffSort = $0080000,
    ffFileSelect = $0100000, ffMultiSelect = $0200000, ffDoNotSpellCheck = $0400000, ffDoNotScroll = $0800000,
    ffComb = $1000000, ffRichText = $2000000, ffRadiosInUnison = $2000000, ffCommitOnSelChange = $4000000); // for internal use
  TdxPDFSignatureFlags = (sfNone, sfSignaturesExist, sfAppendOnly); // for internal use
  TdxPDFTargetMode = (tmXYZ, tmFit, tmFitHorizontally, tmFitVertically, tmFitRectangle, tmFitBBox, tmFitBBoxHorizontally,
    tmFitBBoxVertically);
  TdxPDFTextJustification = (tjLeftJustified, tjCentered, tjRightJustified); // for internal use
  TdxPDFTilingType = (ttConstantSpacing, ttNoDistortion, ttFasterTiling); // for internal use

  TdxPDFDeferredObjectInfo = record // for internal use
    Key: string;
    Name: string;
    Number: Integer;
    SourceObject: TdxPDFBase;
  end;

  TdxPDFExportParameters = class // for internal use
  strict private
    FDocumentState: TdxPDFDocumentState;
  public
    Angle: TcxRotationAngle;
    Annotations: TdxPDFReferencedObjects;
    Bounds: TdxRectF;
    CancelCallback: TdxTaskCancelCallback;
    ScaleFactor: Single;

    constructor Create(AState: TdxPDFDocumentState);
    function IsCanceled: Boolean;

    property DocumentState: TdxPDFDocumentState read FDocumentState;
  end;

  { TdxPDFDestinationInfo }

  TdxPDFDestinationInfo = record // for internal use
  strict private
    FDestination: TdxPDFCustomDestination;
    FName: string;
  public
    class function Create(ADestination: TdxPDFCustomDestination): TdxPDFDestinationInfo; overload; static;
    class function Create(const AName: string): TdxPDFDestinationInfo; overload; static;
    class function Invalid: TdxPDFDestinationInfo; static;
    procedure Finalize;

    function GetDestination(ACatalog: TdxPDFCatalog; AInternal: Boolean): TdxPDFCustomDestination;
    function IsValid: Boolean;

    property Name: string read FName;
  end;

 { TdxPDFTarget }

  TdxPDFTarget = record
  strict private
    FHeight: Double;
    FMode: TdxPDFTargetMode;
    FPageIndex: Integer;
    FWidth: Double;
    FX: Single;
    FY: Single;
    FZoom: Single;
  public
    class function Create(APageIndex: Integer; X, Y, AZoom: Single): TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer): TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer; const R: TdxRectF): TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer; X, Y: Single): TdxPDFTarget; overload; static;
    class function CreateEx(AMode: TdxPDFTargetMode; APageIndex: Integer; X, Y, AWidth, AHeight, AZoom: Single): TdxPDFTarget; overload; static;
    class function Invalid: TdxPDFTarget; static;
    function IsValid: Boolean;

    property Height: Double read FHeight;
    property Mode: TdxPDFTargetMode read FMode;
    property PageIndex: Integer read FPageIndex;
    property Width: Double read FWidth;
    property X: Single read FX;
    property Y: Single read FY;
    property Zoom: Single read FZoom;
  end;

  { TdxPDFInteractiveOperation }

  TdxPDFInteractiveOperation = record
  strict private
    FAction: TdxPDFCustomAction;
    FDestination: TdxPDFCustomDestination;
    function GetTarget: TdxPDFTarget;

    property Destination: TdxPDFCustomDestination read FDestination;
  public
    class function Create(AAction: TdxPDFCustomAction): TdxPDFInteractiveOperation; overload; static;
    class function Create(AAction: TdxPDFCustomAction;
      ADestination: TdxPDFCustomDestination): TdxPDFInteractiveOperation; overload; static;
    class function Invalid: TdxPDFInteractiveOperation; static;
    function IsValid: Boolean;

    property Action: TdxPDFCustomAction read FAction;
    property Target: TdxPDFTarget read GetTarget;
  end;

  { IdxPDFInteractivityController }

  IdxPDFInteractivityController = interface // for internal use
  ['{12BCE71F-D47A-4354-8049-A88730C9EDF3}']
    procedure ExecuteOperation(AField: TdxPDFAcroFormActionField);
    procedure GoToFirstPage;
    procedure GoToLastPage;
    procedure GoToNextPage;
    procedure GoToPrevPage;
    procedure OpenURI(const AURI: string);
    procedure ShowDocumentPosition(const ATarget: TdxPDFTarget);
    end;

  { IdxPDFOpacityStream }

  IdxPDFOpacityStream = interface // for internal use
    ['{C760A927-31F3-43C3-AEE6-4F5E5995E63B}']
    function GetNextValue: Byte;
  end;

  { IdxPDFAnnotationAppearanceBuilder }

  IdxPDFAnnotationAppearanceBuilder = interface // for internal use
  ['{8341D820-A735-44F5-9AC8-4DBF060702D9}']
    procedure RebuildAppearance(AForm: TdxPDFForm);
  end;

   { IdxPDFCommandInterpreter }

  IdxPDFCommandInterpreter = interface // for internal use
    ['{0F9503DE-2E5A-4785-A6CE-8FC4B2F51D75}']
    function GetActualSize: TSize;
    function GetBounds: TdxRectF;
    function GetDeviceTransformationMatrix: TdxPDFTransformationMatrix;
    function GetRotationAngle: Single;
    function GetTransformMatrix: TdxPDFTransformationMatrix;

    function CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize; AColor: TdxPDFColor): TcxBitmap32;
    function TransformSize(AMatrix: TdxPDFTransformationMatrix; const ABoundingBox: TdxRectF): TdxSizeF;
    procedure ExecuteCommand(ACommands: TdxPDFReferencedObjects); overload;
    procedure ExecuteCommand(const AInterpreter: IdxPDFCommandInterpreter; ACommands: TdxPDFReferencedObjects); overload;

    procedure UnknownCommand(const AName: string);
    // Drawing
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF); overload;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF); overload;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF);
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double);
    procedure BeginPath(const AStartPoint: TdxPointF);
    procedure Clip(AUseNonzeroWindingRule: Boolean);
    procedure ClosePath;
    procedure DrawImage(AImage: TdxPDFDocumentImage);
    procedure DrawForm(AForm: TdxPDFForm; AResources: TdxPDFResources);
    procedure DrawShading(AShading: TdxPDFCustomShading);
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm);
    procedure FillPaths(AUseNonzeroWindingRule: Boolean);
    procedure RecreatePaths;
    procedure SetColorForNonStrokingOperations(AColor: TdxPDFColor);
    procedure SetColorForStrokingOperations(AColor: TdxPDFColor);
    procedure SetColorSpaceForNonStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
    procedure SetColorSpaceForStrokingOperations(AColorSpace: TdxPDFCustomColorSpace);
    procedure SetFlatnessTolerance(AValue: Double);
    procedure SetLineCapStyle(ALineCapStyle: TdxPDFLineCapStyle);
    procedure SetLineJoinStyle(ALineJoinStyle: TdxPDFLineJoinStyle);
    procedure SetLineStyle(ALineStyle: TdxPDFLineStyle);
    procedure SetLineWidth(ALineWidth: Single);
    procedure StrokePaths;
    procedure TransformPaths;
    function TransformShadingPoint(APoint: TdxPointF): TdxPointF;
    // Text
    procedure BeginText;
    procedure EndText;
    procedure SetCharacterSpacing(ASpacing: Single);
    procedure SetMiterLimit(AMiterLimit: Single);
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double);
    procedure SetTextLeading(ALeading: Double);
    procedure SetTextHorizontalScaling(AValue: Double);
    procedure SetTextMatrix(const AOffset: TdxPointF); overload;
    procedure SetTextMatrix(AMatrix: TdxPDFTransformationMatrix); overload;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode);
    procedure SetTextRise(AValue: Double);
    procedure SetWordSpacing(AWordSpacing: Double);
    procedure DrawString(const AText: TBytes); overload;
    procedure DrawString(const AText: TBytes; var AOffsets: TDoubleDynArray); overload;
    procedure MoveToNextLine;
    // Graphics State
    procedure ApplyGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters);
    procedure ModifyTransformationMatrix(AMatrix: TdxPDFTransformationMatrix);
    procedure SaveState;
    procedure SetRenderingIntent(AValue: TdxPDFRenderingIntent);
    procedure RestoreState;

    property ActualSize: TSize read GetActualSize;
    property Bounds: TdxRectF read GetBounds;
    property DeviceTransformMatrix: TdxPDFTransformationMatrix read GetDeviceTransformationMatrix;
    property RotationAngle: Single read GetRotationAngle;
    property TransformMatrix: TdxPDFTransformationMatrix read GetTransformMatrix;
  end;

  TdxPDFShadingInfo = record // for internal use
    Interpreter: IdxPDFCommandInterpreter;
    Graphics: TdxGPCanvas;
    NeedDrawBackground: Boolean;
    Size: TdxSizeF;
    Shading: TdxPDFCustomShading;
    TransformMatrix: TdxPDFTransformationMatrix;
    UseTransparency: Boolean;
  end;

  TdxPDFFontInfo = record // for internal use
  strict private
    function GetFontLineSize: Single;
  public
    FontData: TObject;
    FontSize: Single;
    property FontLineSize: Single read GetFontLineSize;
  end;

  { IdxPDFShadingPainter }

  IdxPDFShadingPainter = interface // for internal use
  ['{07E917B6-A92E-4B0D-B5ED-2DB7C94B0182}']
    procedure Draw(const AShadingInfo: TdxPDFShadingInfo);
  end;

  { IdxPDFTillingPainter }

  IdxPDFTillingPainter = interface // for internal use
  ['{07E917B6-A92E-4B0D-B5ED-2DB7C94B0182}']
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize; AColor: TdxPDFColor): TcxBitmap32;
    procedure Draw(const AShadingInfo: TdxPDFShadingInfo);
  end;

  { IdxPDFInteractiveObject }

  IdxPDFInteractiveObject = interface // for internal use
  ['{C19910F1-1382-4619-BFAA-83E9DBA6139F}']
    function GetCursor: TCursor;
    function GetPageIndex: Integer;
    function GetRect: TdxRectF;
    function IsResetFocusingNeeded: Boolean;
    procedure ExecuteOperation(const AController: IdxPDFInteractivityController);
  end;

  { IdxPDFHintableObject }

  IdxPDFHintableObject = interface(IdxPDFInteractiveObject) // for internal use
  ['{D2D693F3-0536-42BE-8626-AD245A592E22}']
    function GetHint: string;
  end;

  { TdxPDFObject }

  TdxPDFObject = class(TdxPDFBase)
  strict private
    FOwner: TObject;
    FRepository: TdxPDFDocumentRepository;
  protected
    class function GetTypeName: string; virtual; // for internal use
    function GetRepository: TdxPDFDocumentRepository; virtual; // for internal use
    procedure CreateSubClasses; virtual; // for internal use
    procedure DestroySubClasses; virtual; // for internal use
    procedure Initialize; virtual; // for internal use
    procedure Read(ADictionary: TdxPDFReaderDictionary); virtual; // for internal use
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); virtual; // for internal use
    procedure SetOwner(const AValue: TObject); virtual; // for internal use
    procedure SetRepository(const AValue: TdxPDFDocumentRepository); virtual; // for internal use

    function GetObject(const AName: string; ASourceDictionary: TdxPDFDictionary; out AObject: TdxPDFBase): Boolean; // for internal use

    property Repository: TdxPDFDocumentRepository read GetRepository write SetRepository; // for internal use
    property Owner: TObject read FOwner write SetOwner; // for internal use
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;
  end;

  { TdxPDFGlyphMapper }

  TdxPDFGlyphMapper = class // for internal use
  public
    function CreateGlyphRun: TdxPDFGlyphRun; virtual; abstract;
    function GetGlyphIndex(ACh: Char): Integer; virtual; abstract;
    function MapString(const S: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult; virtual; abstract;
  end;

  { TdxPDFFullTrustGlyphMapper }

  TdxPDFFullTrustGlyphMapper = class(TdxPDFGlyphMapper) // for internal use
  strict private
    FCMapTables: TList<TdxFontFileCMapCustomFormatRecord>;
    FFactor: Single;
    FFontFile: TdxFontFile;
    FMappedGlyphsCache: TDictionary<Integer, Integer>;

    function MapStringWithoutCTL(const AStr: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
  protected
    function IsWritingOrderControl(AChar: Char): Boolean;

    property FontFile: TdxFontFile read FFontFile;
  public
    constructor Create(AFontFile: TdxFontFile);
    destructor Destroy; override;

    function CreateGlyph(AGlyphIndex: Integer; ACh: Char; AWidth, AGlyphOffset: Double): TdxPDFGlyph; virtual; abstract;
    function CreateGlyphRun(const AGlyphs: TdxPDFGlyphList): TdxPDFGlyphRun; reintroduce; overload; virtual; abstract;

    class function GetCMapEntryPriority(AEntry: TdxFontFileCMapCustomFormatRecord; AIsSymbolic: Boolean): Integer;
    function GetGlyphIndex(ACharacter: Char): Integer; override;
    function MapString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult; override;
  end;

  { TdxPDFEmbeddedGlyphMapper }

  TdxPDFEmbeddedGlyphMapper = class(TdxPDFFullTrustGlyphMapper) // for internal use
  public
    function CreateGlyphRun: TdxPDFGlyphRun; overload; override;
    function CreateGlyph(AGlyphIndex: Integer; ACh: Char; AWidth, AGlyphOffset: Double): TdxPDFGlyph; override;
    function CreateGlyphRun(const AGlyphs: TdxPDFGlyphList): TdxPDFGlyphRun; overload; override;
  end;

  { TdxPDFDocumentState }

  TdxPDFDocumentState = class(TdxPDFObject) // for internal use
  strict private
    FRotationAngle: TcxRotationAngle;
    FOnRotationAngleChanged: TNotifyEvent;
    function GetImageDataStorage: TdxPDFDocumentImageDataStorage;
    function GetFontDataStorage: TdxPDFFontDataStorage;
    procedure SetRotationAngle(const AValue: TcxRotationAngle);
    procedure CalculateFontParameters(ACommand: TdxPDFCustomCommand; out AFontName: string;
      out AFontStyle: TdxGPFontStyle; var APitchAndFamily: Byte; var AIsEmptyFontName: Boolean);
  protected
    function GetRepository: TdxPDFDocumentRepository; override;

    function CreateFontData(const AFontFamilyName: string; AFontStyle: TdxGPFontStyle): TObject;
    function GetPageIndex(APage: TdxPDFPage): Integer;
    function SearchFontData(AFontCommand: TdxPDFCustomCommand): TObject;
  public
    property FontDataStorage: TdxPDFFontDataStorage read GetFontDataStorage;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage read GetImageDataStorage;
    property RotationAngle: TcxRotationAngle read FRotationAngle write SetRotationAngle;

    property OnRotationAngleChanged: TNotifyEvent read FOnRotationAngleChanged write FOnRotationAngleChanged;
  end;

  { TdxPDFStreamObject }

  TdxPDFStreamObject = class(TdxPDFObject) // for internal use
  strict private
    FStream: TdxPDFStream;

    function GetUncompressedData: TBytes;
    procedure SetStream(const AValue: TdxPDFStream);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function GetData: TBytes; virtual;

    property Stream: TdxPDFStream read FStream write SetStream;
    property UncompressedData: TBytes read GetUncompressedData;
  end;

  { TdxPDFCustomEncoding }

  TdxPDFCustomEncoding = class(TdxPDFObject) // for internal use
  strict private
    FFontFileEncoding: TdxFontFileCustomEncoding;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; virtual;
  public
    function GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData; virtual; abstract;
    function ShouldUseEmbeddedFontEncoding: Boolean; virtual; abstract;

    property FontFileEncoding: TdxFontFileCustomEncoding read FFontFileEncoding;
  end;

  { TdxPDFCustomFont }

  TdxPDFCustomFontClass = class of TdxPDFCustomFont;
  TdxPDFCustomFont = class(TdxPDFObject, IdxPDFFont) // for internal use
  strict private
    FAverageWidth: Double;
    FBaseFont: string;
    FEncoding: TdxPDFCustomEncoding;
    FID: string;
    FFontDescriptor: TdxPDFFontDescriptor;
    FListeners: TInterfaceList;
    FName: string;
    FRegistrationName: string;

    FCharacterMapping: TdxPDFCharacterMapping;
    FSubsetName: string;
    FWidths: TDictionary<Integer, Double>;

    procedure SetEncoding(const AValue: TdxPDFCustomEncoding);
    procedure SetFontDescriptor(const AValue: TdxPDFFontDescriptor);

    function GetBoldWeight: Integer;
    function GetCharacterMapping: TdxPDFCharacterMapping;
    function GetID: string;
    function GetInstance: TObject;
    function GetFontBBox: TdxRectF;
    function GetShouldUseEmbeddedFontEncoding: Boolean;
    function GetSubsetNameLength: Integer;
    function GetSubsetPrefixLength: Integer;
    function GetWidthToHeightFactor: Double;
    procedure SetCharacterMapping(const AValue: TdxPDFCharacterMapping);
    procedure SetWidths(const AValue: TDictionary<Integer, Double>);
    procedure ReadFontName;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    function GetFontDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary; virtual;
    function GetFontDescriptorDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary; virtual;
    class function GetSubTypeName: string; virtual;
    function GetHeightFactor: Double; virtual;
    function GetUseGlyphIndexes: Boolean; virtual;
    function GetWidthFactor: Double; virtual;
    procedure ReadEncoding(ASourceObject: TdxPDFBase); virtual;
    procedure ReadFontDescriptor(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure ReadToUnicode(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure ReadWidths(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure SetActualCompactFontFileData(const AValue: TBytes); virtual;

    function GetStream(const AKey: string; ADictionary: TdxPDFReaderDictionary): TdxPDFStream;
    function ReadOpenTypeFontFileData(ADictionary: TdxPDFReaderDictionary;
      ASuppressException: Boolean): TBytes;
    procedure AddWidth(AKey: Integer; AWidth: Double);
    procedure GenerateRegistrationName;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IdxPDFFont
    function CreateGlyphWidths: TdxPDFDoubleList; virtual; abstract;
    function GetActualCompactFontFileData: TBytes; virtual;
    function GetCompactFontFileData: TBytes; virtual;
    function GetBaseFontName: string;
    function GetCharset: TDictionary<SmallInt, SmallInt>; virtual;
    function GetGlyphCount: Integer;
    function GetFontDescriptor: TdxPDFFontDescriptor;
    function GetFontName: string;
    function GetForceBold: Boolean;
    function GetItalic: Boolean;
    function GetPitchAndFamily: Byte;
    function GetTrueTypeFontFileData: TBytes; virtual;
    function GetType1FontFileData: TdxPDFType1FontFileData; virtual;
    function GetRegistrationName: string;
    function GetWeight: Integer;
    procedure AddListener(AListener: IdxPDFDocumentSharedObjectListener);
    procedure RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);

    property ActualCompactFontFileData: TBytes read GetActualCompactFontFileData write SetActualCompactFontFileData;
    property SubsetNameLength: Integer read GetSubsetNameLength;
    property SubsetPrefixLength: Integer read GetSubsetPrefixLength;
  public
    constructor Create(AOwner: TObject); overload; override;
    constructor Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor); reintroduce; overload; virtual;
    class function Parse(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont; static;

    procedure GetGlyphPositions(const AStringData: TdxPDFStringData; AFontSizeFactor, ACharacterSpacing, AWordSpacing,
      AScalingFactor: Double; var AResult: TDoubleDynArray); virtual; abstract;
    procedure UpdateGlyphs(var AGlyphs: TSmallIntDynArray); virtual;

    property BaseFont: string read FBaseFont write FBaseFont;
    property CharacterMapping: TdxPDFCharacterMapping read GetCharacterMapping write SetCharacterMapping;
    property Encoding: TdxPDFCustomEncoding read FEncoding write SetEncoding;
    property FontDescriptor: TdxPDFFontDescriptor read FFontDescriptor write SetFontDescriptor;
    property HeightFactor: Double read GetHeightFactor;
    property ID: string read GetID;
    property Name: string read FName write FName;
    property RegistrationName: string read GetRegistrationName;
    property UseGlyphIndexes: Boolean read GetUseGlyphIndexes;
    property WidthFactor: Double read GetWidthFactor;
    property Widths: TDictionary<Integer, Double> read FWidths write SetWidths;
    property WidthToHeightFactor: Double read GetWidthToHeightFactor;
  end;

  { TdxPDFColorSpaceTransformResult }

  TdxPDFColorSpaceTransformResult = record // for internal use
  public
    Data: TBytes;
    IsInvalid: Boolean;
    MaskData: TBytes;
    PixelFormat: TdxPDFPixelFormat;

    function Create(const AData: TBytes): TdxPDFColorSpaceTransformResult; overload;
    function Create(const AData: TBytes; AFormat: TdxPDFPixelFormat): TdxPDFColorSpaceTransformResult; overload;
    function Create(const AData, AMaskData: TBytes): TdxPDFColorSpaceTransformResult; overload;
    function Create(const AData, AMaskData: TBytes; AFormat: TdxPDFPixelFormat): TdxPDFColorSpaceTransformResult; overload;
  end;

  { TdxPDFCustomColorSpace }

  TdxPDFCustomColorSpace = class(TdxPDFStreamObject) // for internal use
  strict private
    FName: string;
    FAlternateColorSpace: TdxPDFCustomColorSpace;
    FComponentCount: Integer;

    procedure SetAlternateColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetComponentCount(const AValue: Integer);

    function GetDecodedValue(AValue: Double; const ADecodeArrayEntry: TdxPDFRange): Double;
    function NeedDataDecoding(const ADecode: TdxPDFRanges; ABitsPerComponent: Integer): Boolean;
    procedure DecodeData(const AData: TBytes; AImage: TdxPDFDocumentImage; out ADecodedData: TBytes);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;

    function CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges; virtual;
    function GetComponentCount: Integer; virtual;
    function GetTransformationClass: TdxPDFCustomColorSpaceTransformationClass; virtual; abstract;
    function CanRead(ASize: Integer): Boolean; virtual;
    procedure CheckComponentCount; virtual;
    procedure DoRead(AArray: TdxPDFArray); virtual;

    property Name: string read FName write FName;
  public
    class function CreateColorSpace(const AName: string; AResources: TdxPDFResources): TdxPDFCustomColorSpace;
    class function Parse(ARepository: TdxPDFDocumentRepository;
      AObject: TdxPDFBase; AResources: TdxPDFResources = nil): TdxPDFCustomColorSpace;

    function Transform(AColor: TdxPDFColor): TdxPDFColor; overload;
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray; overload; virtual;
    function Transform(const AInfo: TdxPDFImageInfo): TdxPDFColorSpaceTransformResult; overload; virtual; abstract;

    function Transform(AImage: TdxPDFDocumentImage; const AData: TBytes): TdxPDFColorSpaceTransformResult; overload;

    property AlternateColorSpace: TdxPDFCustomColorSpace read FAlternateColorSpace write SetAlternateColorSpace;
    property ComponentCount: Integer read GetComponentCount write SetComponentCount;
  end;

  { TdxPDFCustomColorSpaceTransformation }

  TdxPDFCustomColorSpaceTransformation = class // for internal use
  strict private
    FInfo: TdxPDFImageInfo;
  protected
    function GetComponentCount: Integer; virtual; abstract;

    function UnpackData(const AData: TBytes): TBytes;

    property Info: TdxPDFImageInfo read FInfo;
    property ComponentCount: Integer read GetComponentCount;
  public
    constructor Create(const AInfo: TdxPDFImageInfo);
    function Transform(const AData: TBytes): TdxPDFColorSpaceTransformResult; virtual; abstract;
  end;

  { TdxPDFDeferredObject }

  TdxPDFDeferredObject = class(TdxPDFObject) // for internal use
  strict private
    FInfo: TdxPDFDeferredObjectInfo;
    FResolvedObject: TdxPDFObject;

    function GetResolvedObject: TdxPDFObject;
    function GetSourceObject: TdxPDFBase;
    procedure SetResolvedObject(const AValue: TdxPDFObject);
    procedure SetSourceObject(const AValue: TdxPDFBase);
    procedure ResolveObject;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function IsResolved: Boolean;

    property SourceObject: TdxPDFBase read GetSourceObject write SetSourceObject;
  public
    constructor Create(AOwner: TdxPDFObject; const AInfo: TdxPDFDeferredObjectInfo); reintroduce; overload;
    constructor Create(AOwner, AResolvedObject: TdxPDFObject); reintroduce; overload;

    property ResolvedObject: TdxPDFObject read GetResolvedObject write SetResolvedObject;
  end;

  { TdxPDFObjectList }

  TdxPDFObjectListClass = class of TdxPDFObjectList;
  TdxPDFObjectList = class(TdxPDFObject) // for internal use
  strict private
    FInternalObjects: TdxPDFReferencedObjectDictionary;
    FNames: TdxPDFNamedObjectDictionary;
    function GetCount: Integer;
    procedure DoAdd(AObjectList: TdxPDFObjectList; ANeedClear: Boolean);
    procedure DoReadObject(const AObjectName: string; ADictionary: TdxPDFReaderDictionary);
  private
    property InternalObjects: TdxPDFReferencedObjectDictionary read FInternalObjects;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    class function GetTypePrefix: string; virtual;
    function GetTypeDictionaryKey: string; virtual;
    procedure ReadObject(AStructureObject: TdxPDFBase; ANumber: Integer; const AName: string); virtual;

    function GetObject(const AName: string): TdxPDFObject;
    procedure Clear;
    procedure InternalAdd(const AName: string; AObject: TdxPDFObject);
    procedure ReadList(ADictionary: TdxPDFReaderDictionary);
  public
    function Contains(const AName: string): Boolean;
    function Add(AObject: TdxPDFObject): string;
    function AddReference(ANumber: Integer): string;
    procedure Append(AList: TdxPDFObjectList);
    procedure Assign(AList: TdxPDFObjectList);

    property Count: Integer read GetCount;
  end;

  { TdxPDFFonts }

  TdxPDFFonts = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;

    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    function GetFont(const AName: string): TdxPDFCustomFont;
  end;

  { TdxPDFGraphicsStateParameters }

  TdxPDFGraphicsStateParameters = class(TdxPDFObject) // for internal use
  strict private type
    TAssignedValue = (gspFlatnessTolerance, gspFont, gspFontSize, gspLineCapStyle, gspLineJoinStyle,
      gspLineStyle, gspLineWidth, gspMiterLimit, gspNonStrokingColorAlpha, gspRenderingIntent, gspSoftMask,
      gspSmoothnessTolerance, gspStrokingColorAlpha, gspTextKnockout, gspBlendMode);
    TAssignedValues = set of TAssignedValue;
  strict private
    FAssignedValues: TAssignedValues;
    FBlendMode: TdxPDFBlendMode;
    FFlatnessTolerance: Double;
    FFont: TdxPDFCustomFont;
    FFontSize: Double;
    FLineCapStyle: TdxPDFLineCapStyle;
    FLineJoinStyle: TdxPDFLineJoinStyle;
    FLineStyle: TdxPDFLineStyle;
    FLineWidth: Double;
    FMiterLimit: Double;
    FNonStrokingColorAlpha: Double;
    FRenderingIntent: TdxPDFRenderingIntent;
    FSoftMask: TdxPDFCustomSoftMask;
    FSmoothnessTolerance: Double;
    FStrokingColorAlpha: Double;
    FTextKnockout: Boolean;

    procedure SetFont(const AValue: TdxPDFCustomFont);
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
    procedure SetLineWidth(const AValue: Double);
    procedure SetSoftMask(const AValue: TdxPDFCustomSoftMask);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    property AssignedValue: TAssignedValues read FAssignedValues;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFGraphicsStateParameters;
    procedure Assign(AParameters: TdxPDFGraphicsStateParameters; ACheckAssignedValues: Boolean = True);

    property BlendMode: TdxPDFBlendMode read FBlendMode write FBlendMode;
    property Font: TdxPDFCustomFont read FFont write SetFont;
    property FontSize: Double read FFontSize write FFontSize;
    property LineCapStyle: TdxPDFLineCapStyle read FLineCapStyle write FLineCapStyle;
    property LineJoinStyle: TdxPDFLineJoinStyle read FLineJoinStyle write FLineJoinStyle;
    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property NonStrokingColorAlpha: Double read FNonStrokingColorAlpha write FNonStrokingColorAlpha;
    property FlatnessTolerance: Double read FFlatnessTolerance write FFlatnessTolerance;
    property RenderingIntent: TdxPDFRenderingIntent read FRenderingIntent write FRenderingIntent;
    property SmoothnessTolerance: Double read FSmoothnessTolerance write FSmoothnessTolerance;
    property SoftMask: TdxPDFCustomSoftMask read FSoftMask write SetSoftMask;
    property StrokingColorAlpha: Double read FStrokingColorAlpha write FStrokingColorAlpha;
    property TextKnockout: Boolean read FTextKnockout write FTextKnockout;
  end;

  { TdxPDFGraphicsStateParametersList }

  TdxPDFGraphicsStateParametersList = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;
    function GetTypeDictionaryKey: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    function GetParameters(const AName: string): TdxPDFGraphicsStateParameters;
    procedure Add(const AName: string; AStateParameters: TdxPDFGraphicsStateParameters); overload;
  end;

  { TdxPDFPageContentItem }

  TdxPDFPageContentItem = class(TdxPDFStreamObject) // for internal use
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFXObject }

  TdxPDFXObject = class(TdxPDFStreamObject) // for internal use
  strict private
    FLock: TRTLCriticalSection;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDraw(const AInterpreter: IdxPDFCommandInterpreter); virtual;
  public
    class function Parse(ARepository: TdxPDFCustomRepository; AStream: TdxPDFStream; const ASubtype: string = ''): TdxPDFXObject;
    procedure Draw(const AInterpreter: IdxPDFCommandInterpreter);
  end;

  { TdxPDFOpacityStream }

  TdxPDFOpacityStream = class(TInterfacedObject, IdxPDFOpacityStream) // for internal use
  strict private
    FCurrentBit: Byte;
    FCurrentByte: Byte;
    FData: TBytes;
    FMaskValue: Byte;
    FPaintValue: Byte;
    FPosition: Int64;
    FStride: Integer;
    FStridePosition: Int64;
  protected
    function GetNextValue: Byte;

    property Position: Int64 read FPosition;
    property StridePosition: Int64 read FStridePosition;
  public
    constructor Create(AWidth: Integer; const ADecode: TdxPDFRange; const AData: TBytes);
  end;

  { TdxPDFInterpolatedOpacityStream }

  TdxPDFInterpolatedOpacityStream = class(TcxIUnknownObject, IdxPDFOpacityStream) // for internal use
  strict private
    FMaskData: TBytes;
    FPosition: Integer;
  protected
    function GetNextValue: Byte;
  public
    constructor Create(const AMaskData: TBytes);
  end;

  { TdxPDFOpacityInterpolator }

  TdxPDFOpacityInterpolator = class // for internal use
  strict private
    FData: TBytes;
    FNext: TBytes;
    FPrev: TBytes;
    FResult: TBytes;

    FSize: Integer;
    FFactor: Double;
    FDestinationSize: Integer;
    FPrevResultPosition: Integer;
    FSourcePosition: Integer;
    FResultPosition: Integer;
  public
    constructor Create(const AData: TBytes; ASize: Integer; const AResult: TBytes; ASourceSize, ADestinationSize: Integer);

    class function HorizontalInterpolation(const AData: TBytes; ASourceWidth, ASourceStride, ADestinationWidth, AHeight,
      AComponentCount: Integer): TBytes;
    class function VerticalInterpolation(const AData: TBytes; AWidth, ASourceHeight, ADestinationHeight: Integer): TBytes;

    function NextInterpolationArray: TBytes;
    procedure FillResultData(const AArray: TBytes);
    procedure Start(APosition: Integer);
    procedure Interpolate(AValue: Integer);
    procedure DoEnd;
  end;

  { TdxPDFDocumentImage }

  TdxPDFForm = class(TdxPDFXObject) // for internal use
  strict private
    FBBox: TdxRectF;
    FCommands: TdxPDFReferencedObjects;
    FMatrix: TdxPDFTransformationMatrix;
    FRepository: TdxPDFCustomRepository;
    FResources: TdxPDFResources;
    FStreamRef: TdxPDFStream;
    FUseOwnResources: Boolean;

    procedure CheckFormType(ADictionary: TdxPDFDictionary);
    procedure ReadResources(ADictionary: TdxPDFReaderDictionary);
    procedure SetMatrix(const AValue: TdxPDFTransformationMatrix);
    procedure SetResources(const AValue: TdxPDFResources);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDraw(const AInterpreter: IdxPDFCommandInterpreter); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function CreateForm(AStream: TdxPDFStream; AParentResources: TdxPDFResources): TdxPDFForm;
    constructor Create(AOwner: TObject); overload; override;
    constructor Create(ACatalog: TdxPDFCatalog; const ABBox: TdxRectF); reintroduce; overload;
    function GetCommands: TdxPDFReferencedObjects;
    function GetTransformationMatrix(const ARect: TdxRectF): TdxPDFTransformationMatrix;
    procedure ReplaceCommands(const ACommandData: TBytes);

    property BBox: TdxRectF read FBBox write FBBox;
    property Matrix: TdxPDFTransformationMatrix read FMatrix write SetMatrix;
    property Resources: TdxPDFResources read FResources write SetResources;
  end;

  { TdxPDFCustomSoftMask }

  TdxPDFCustomSoftMask = class abstract(TdxPDFObject) // for internal use
  strict private
    FTransparencyGroup: TdxPDFGroupForm;
    FTransparencyFunction: TdxPDFObject;
    procedure SetTransparencyGroup(const AValue: TdxPDFGroupForm);
    procedure SetTransparencyFunction(const AValue: TdxPDFObject);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function Parse(ARepository: TdxPDFCustomRepository; ASourceObject: TdxPDFBase): TdxPDFCustomSoftMask;

    property TransparencyGroup: TdxPDFGroupForm read FTransparencyGroup write SetTransparencyGroup;
    property TransparencyFunction: TdxPDFObject read FTransparencyFunction write SetTransparencyFunction;
  end;

  { TdxPDFLuminositySoftMask }

  TdxPDFLuminositySoftMask = class(TdxPDFCustomSoftMask) // for internal use
  strict private
    FBackdropColor: TdxPDFColor;
  protected
    class function GetTypeName: string; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  end;

  { TdxPDFAlphaSoftMask }

  TdxPDFAlphaSoftMask = class(TdxPDFCustomSoftMask) // for internal use
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFEmptySoftMask }

  TdxPDFEmptySoftMask = class(TdxPDFCustomSoftMask)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFTransparencyGroup }

  TdxPDFTransparencyGroup = class(TdxPDFObject) // for internal use
  strict private const
    SubtypeKey = 'S';
    ColorSpaceKey = 'CS';
    IsolatedKey = 'I';
    KnockoutKey = 'K';
  strict private
    FColorSpace: TdxPDFCustomColorSpace;
    FIsolated: Boolean;
    FKnockout: Boolean;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace;
    property Isolated: Boolean read FIsolated;
    property Knockout: Boolean read FKnockout;
  end;

  { TdxPDFGroupForm }

  TdxPDFGroupForm = class(TdxPDFForm) // for internal use
  strict private
    FGroup: TdxPDFTransparencyGroup;
    function GetColorSpace: TdxPDFCustomColorSpace;
    procedure ReadGroup(ADictionary: TdxPDFReaderDictionary);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDraw(const AInterpreter: IdxPDFCommandInterpreter); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property ColorSpace: TdxPDFCustomColorSpace read GetColorSpace;
  end;

  { TdxPDFDocumentImage }

  TdxPDFDocumentImage = class(TdxPDFXObject) // for internal use
  strict private
    FBitsPerComponent: Integer;
    FColorKeyMask: TdxPDFRanges;
    FColorSpace: TdxPDFCustomColorSpace;
    FComponentCount: Integer;
    FDecodeRanges: TdxPDFRanges;
    FID: string;
    FIntent: string;
    FListeners: TInterfaceList;
    FGUID: string;
    FHasMask: Boolean;
    FHeight: Integer;
    FMatte: TDoubleDynArray;
    FMask: TdxPDFDocumentImage;
    FNeedInterpolate: Boolean;
    FSoftMask: TdxPDFDocumentImage;
    FStructParent: Integer;
    FWidth: Integer;

    procedure SetColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetMask(const AValue: TdxPDFDocumentImage);
    procedure SetSoftMask(const AValue: TdxPDFDocumentImage);

    function CreateMaskOpacityStream(const AData: TBytes; AWidth, AHeight: Integer): IdxPDFOpacityStream;
    function ApplyDecodeRanges(const AData: TBytes): TBytes;
    function ApplyColorSpace(const ASoftMaskDecodedData, ASoftMaskData: TBytes): TBytes;
    function InterpolateSoftMaskImageData(const AData: TBytes; AWidth, AHeight: Integer; var ASoftMaskData: TBytes;
      out AActualWidth, AActualHeight, AMaskStride: Integer): TBytes;
    function InterpolateStencilMaskData(const AData: TBytes; AWidth, AHeight, AComponentCount: Integer;
      var AStride: Integer): TBytes;
    procedure ApplyMask(AImageData: TdxPDFDocumentImageData; const AMaskData: TBytes);
    procedure ApplySoftMask(AImageData: TdxPDFDocumentImageData);
    procedure ApplyStencilMask(AImageData: TdxPDFDocumentImageData);
    procedure CalculateComponentCount(ADictionary: TdxPDFDictionary);
    procedure DoRead(ADictionary: TdxPDFReaderDictionary);
    procedure ReadColorKeyMask(AArray: TdxPDFArray);
    procedure ReadColorSpace(ADictionary: TdxPDFReaderDictionary);
    procedure ReadDecodeRanges(ADictionary: TdxPDFDictionary);
    procedure ReadMask(ADictionary: TdxPDFReaderDictionary);
    procedure ReadMatte(ADictionary: TdxPDFDictionary);
    procedure ReadSoftMask(ADictionary: TdxPDFReaderDictionary);

    function HasValidStencilMask: Boolean;
    function ToRGB(const AData: TBytes; APixelFormat: TdxPDFPixelFormat; AWidth, AHeight: Integer): TBytes;
  protected
    class function GetTypeName: string; override;
    function GetData: TBytes; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDraw(const AInterpreter: IdxPDFCommandInterpreter); override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    procedure Read(ARepository: TdxPDFCustomRepository; AStream: TdxPDFStream); reintroduce; overload;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    function GetDecodedImageData: TdxPDFDocumentDecodedImageData;
    procedure AddListener(AListener: IdxPDFDocumentSharedObjectListener);
    procedure RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);

    property ColorKeyMask: TdxPDFRanges read FColorKeyMask;
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace write SetColorSpace;
    property GUID: string read FGUID;
    property Mask: TdxPDFDocumentImage read FMask write SetMask;
    property Matte: TDoubleDynArray read FMatte;
    property SoftMask: TdxPDFDocumentImage read FSoftMask write SetSoftMask;
  public
    constructor Create(const AData: TBytes; AColorSpace: TdxPDFCustomColorSpace; AFilters: TdxPDFFilters;
      AWidth, AHeight, ABitsPerComponent: Integer; AHasMask: Boolean; ADictionary: TdxPDFDictionary); reintroduce; overload;

    class function CreateBitmap(AData: TdxPDFDocumentImageData): GpBitmap;
    function CreateImageData: TdxPDFDocumentImageData;
    function GetAsBitmap: Graphics.TBitmap;

    property BitsPerComponent: Integer read FBitsPerComponent;
    property DecodeRanges: TdxPDFRanges read FDecodeRanges;
    property HasMask: Boolean read FHasMask;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  { TdxPDFDocumentImageData }

  TdxPDFDocumentImageData = class(TdxPDFReferencedObject) // for internal use
  strict private
    FBitmap: GpBitmap;
    FData: TBytes;
    FHeight: Integer;
    FPalette: TdxPDFReferencedObjects;
    FPixelFormat: TdxPDFPixelFormat;
    FStride: Integer;
    FWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Clone: TdxPDFDocumentImageData;
    procedure Assign(AImageData: TdxPDFDocumentImageData);
    procedure CalculateParameters(out AComponentCount, ASourceStride, AActualWidth: Integer);
    procedure CalculateStride(AWidth, AComponentCount: Integer);
    procedure PopulateData(const ASourceData: TBytes; AWidth, ASourceStride, AComponentCount: Integer);

    property Bitmap: GpBitmap read FBitmap write FBitmap;
    property Data: TBytes read FData write FData;
    property Height: Integer read FHeight write FHeight;
    property Palette: TdxPDFReferencedObjects read FPalette;
    property PixelFormat: TdxPDFPixelFormat read FPixelFormat write FPixelFormat;
    property Stride: Integer read FStride write FStride;
    property Width: Integer read FWidth write FWidth;
  end;

  { TdxPDFDocumentImageDataStorage }

  TdxPDFDocumentImageDataStorage = class(TcxIUnknownObject, IdxPDFDocumentSharedObjectListener) // for internal use
  strict private
    FDictionary: TObjectDictionary<TdxPDFDocumentImage, TdxPDFDocumentImageData>;
    FQueue: TList<TdxPDFDocumentImage>;
    FLimit: Int64;
    FReferences: TdxPDFUniqueReferences;
    FSize: Int64;
    function GetImageData(AImage: TdxPDFDocumentImage): TdxPDFDocumentImageData;
    procedure RemoveListener(AImage: TdxPDFDocumentImage);
    // IdxPDFDocumentSharedObjectListener
    procedure IdxPDFDocumentSharedObjectListener.DestroyHandler = ImageDestroyHandler;
    procedure ImageDestroyHandler(Sender: TdxPDFBase);
  protected
    function GetValueSize(AValue: TdxPDFDocumentImageData): Int64;
  public
    constructor Create(ALimit: Int64);
    destructor Destroy; override;

    function Add(AImage: TdxPDFDocumentImage): TdxPDFDocumentImageData;
    function TryGetReference(ANumber: Integer; out AImage: TdxPDFDocumentImage): Boolean;
    procedure AddReference(AImage: TdxPDFDocumentImage);
    procedure Clear; overload;
    procedure CheckCapacity;
    procedure Remove(AImage: TdxPDFDocumentImage); overload;

    property Data[AImage: TdxPDFDocumentImage]: TdxPDFDocumentImageData read GetImageData; default;
  end;

  { TdxPDFFontDataStorage }

  TdxPDFFontDataStorage = class(TcxIUnknownObject, IdxPDFDocumentSharedObjectListener) // for internal use
  strict private
    FFontCache: TObject;
    FDictionary: TDictionary<TdxPDFCustomFont, TdxPDFFontRegistrationData>;
    FFolderName: string;
    FLastRegisteredFont: TdxPDFCustomFont;
    FLastRegisteredFontData: TdxPDFFontRegistrationData;
    FLock: TRTLCriticalSection;
    FQueue: TList<TdxPDFCustomFont>;
    FReferences: TdxPDFUniqueReferences;
    procedure InternalAdd(AFont: TdxPDFCustomFont);
    procedure RemoveListener(AFont: TdxPDFCustomFont);
    // IdxPDFDocumentSharedObjectListener
    procedure IdxPDFDocumentSharedObjectListener.DestroyHandler = FontDestroyHandler;
    procedure FontDestroyHandler(Sender: TdxPDFBase);
  public
    constructor Create(const ATempFolder: string);
    destructor Destroy; override;

    function Add(AFont: TdxPDFCustomFont): TdxPDFFontRegistrationData;
    function CreateSubstituteFontData(AFont: TdxPDFCustomFont): TdxPDFFontRegistrationData;
    function SearchFontData(const AFontFamilyName: string; AFontStyle: TdxGPFontStyle): TObject;
    function TryGetValue(ANumber: Integer; out AFont: TdxPDFCustomFont): Boolean;
    procedure Clear;
    procedure Delete(AFont: TdxPDFCustomFont);
  end;

  { TdxPDFXObjects }

  TdxPDFXObjects = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;
  public
    function GetXObject(const AName: string): TdxPDFXObject;
  end;

  { TdxPDFColorSpaces }

  TdxPDFColorSpaces = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    function GetColorSpace(const AName: string): TdxPDFCustomColorSpace;
  end;

  { TdxPDFCustomShading }

  TdxPDFCustomShading = class(TdxPDFObject) // for internal use
  strict private
    FBackgroundColor: TdxPDFColor;
    FBoundingBox: TdxRectF;
    FColorSpace: TdxPDFCustomColorSpace;
    FFunctions: TdxPDFReferencedObjects;
    FUseAntiAliasing: Boolean;
    function CreateFunctions(ASourceObject: TdxPDFBase): TdxPDFReferencedObjects;
    procedure ReadBackgroundColor(AArray: TdxPDFArray);
    procedure ReadColorSpace(ASourceObject: TdxPDFBase);
    procedure ReadFunctions(ASourceObject: TdxPDFBase);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    class function GetShadingType: Integer; virtual;
    function GetPainter: IdxPDFShadingPainter; virtual; abstract;
    function GetDomainDimension: Integer; virtual;
    function IsFunctionRequired: Boolean; virtual;
  public
    class function Parse(ARepository: TdxPDFCustomRepository; ASourceObject: TdxPDFBase): TdxPDFCustomShading;

    function TransformFunction(const AArguments: TDoubleDynArray): TdxPDFColor;

    property BackgroundColor: TdxPDFColor read FBackgroundColor;
    property BoundingBox: TdxRectF read FBoundingBox;
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace;
    property Functions: TdxPDFReferencedObjects read FFunctions;
    property UseAntiAliasing: Boolean read FUseAntiAliasing;
  end;

  { TdxPDFShadings }

  TdxPDFShadings = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    function GetShading(const AName: string): TdxPDFCustomShading;
  end;

  { TdxPDFCustomPattern }

  TdxPDFCustomPattern = class(TdxPDFObject) // for internal use
  strict private
    FMatrix: TdxPDFTransformationMatrix;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    class function GetPatternType: Integer; virtual;
  public
    class function Parse(ARepository: TdxPDFCustomRepository; ASourceObject: TdxPDFBase): TdxPDFCustomPattern;

    property Matrix: TdxPDFTransformationMatrix read FMatrix;
  end;

  { TdxPDFPatterns }

  TdxPDFPatterns = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypeName: string; override;
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    function GetPattern(const AName: string): TdxPDFCustomPattern;
  end;

  { TdxPDFShadingPattern }

  TdxPDFShadingPattern = class(TdxPDFCustomPattern) // for internal use
  strict private
    FGraphicsState: TdxPDFGraphicsStateParameters;
    FShading: TdxPDFCustomShading;
    procedure SetGraphicsStateParameters(const AValue: TdxPDFGraphicsStateParameters);
    procedure SetShading(const AValue: TdxPDFCustomShading);
  protected
    class function GetPatternType: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    property GraphicsState: TdxPDFGraphicsStateParameters read FGraphicsState write SetGraphicsStateParameters;
    property Shading: TdxPDFCustomShading read FShading write SetShading;
  end;

  { TdxPDFTilingPattern }

  TdxPDFTilingPattern = class(TdxPDFCustomPattern) // for internal use
  strict private
    FColoredPaintType: Integer;
    FUncoloredPaintType: Integer;
    FColored: Boolean;
    FTilingType: TdxPDFTilingType;
    FBoundingBox: TdxRectF;
    FXStep: Double;
    FYStep: Double;
    FCommands: TdxPDFReferencedObjects;
    FResources: TdxPDFResources;

    procedure ReadCommands(ADictionary: TdxPDFReaderDictionary);
    procedure ReadStep(ADictionary: TdxPDFDictionary);
    procedure ReadTilingType(ADictionary: TdxPDFDictionary);
  protected
    class function GetPatternType: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure SetOwner(const AValue: TObject); override;
  public
    function CreateMatrix(AWidth: Integer; AHeight: Integer): TdxPDFTransformationMatrix;

    property BoundingBox: TdxRectF read FBoundingBox;
    property Colored: Boolean read FColored;
    property Commands: TdxPDFReferencedObjects read FCommands;
    property Resources: TdxPDFResources read FResources write FResources;
    property TilingType: TdxPDFTilingType read FTilingType;
    property XStep: Double read FXStep;
    property YStep: Double read FYStep;
  end;

  { TdxPDFResources }

  TdxPDFResources = class(TdxPDFObject) // for internal use
  strict private
    FColorSpaces: TdxPDFColorSpaces;
    FDictionary: TdxPDFReaderDictionary;
    FFonts: TdxPDFFonts;
    FGraphicStatesParametersList: TdxPDFGraphicsStateParametersList;
    FLock: TRTLCriticalSection;
    FID: string;
    FPatterns: TdxPDFPatterns;
    FShadings: TdxPDFShadings;
    FXObjects: TdxPDFXObjects;

    function GetColorSpaces: TdxPDFColorSpaces;
    function GetFonts: TdxPDFFonts;
    function GetList(AVariable: TdxPDFObjectList; AClass: TdxPDFObjectListClass; const AKey: string): TdxPDFObjectList;
    function GetGraphicStatesParametersList: TdxPDFGraphicsStateParametersList;
    function GetPatterns: TdxPDFPatterns;
    function GetShadings: TdxPDFShadings;
    function GetXObjects: TdxPDFXObjects;
    procedure SetColorSpaces(const AValue: TdxPDFColorSpaces);
    procedure SetDictionary(const AValue: TdxPDFReaderDictionary);
    procedure SetFonts(const AValue: TdxPDFFonts);
    procedure SetGraphicStatesParametersList(const AValue: TdxPDFGraphicsStateParametersList);
    procedure SetPatterns(const AValue: TdxPDFPatterns);
    procedure SetShadings(const AValue: TdxPDFShadings);
    procedure SetXObjects(const AValue: TdxPDFXObjects);
    procedure Clear;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function GetOwnerResources: TdxPDFResources; virtual;

    function InternalGetColorSpace(const AName: string): TdxPDFCustomColorSpace;
    function InternalGetFont(const AName: string): TdxPDFCustomFont;
    function InternalGetGraphicsStateParameters(const AName: string): TdxPDFGraphicsStateParameters;
    function InternalGetPattern(const AName: string): TdxPDFCustomPattern;
    function InternalGetShading(const AName: string): TdxPDFCustomShading;
    function InternalGetXObject(const AName: string): TdxPDFXObject;

    function AddGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters): string;
    function AddPattern(APattern: TdxPDFCustomPattern): string;
    function AddXObject(ANumber: Integer): string;

    property ColorSpaces: TdxPDFColorSpaces read GetColorSpaces write SetColorSpaces;
    property Dictionary: TdxPDFReaderDictionary read FDictionary write SetDictionary;
    property Fonts: TdxPDFFonts read GetFonts write SetFonts;
    property GraphicStatesParametersList: TdxPDFGraphicsStateParametersList read GetGraphicStatesParametersList
      write SetGraphicStatesParametersList;
    property ID: string read FID;
    property Patterns: TdxPDFPatterns read GetPatterns write SetPatterns;
    property Shadings: TdxPDFShadings read GetShadings write SetShadings;
    property XObjects: TdxPDFXObjects read GetXObjects write SetXObjects;
  public
    function AddFont(AFont: TdxPDFCustomFont): string;
    function GetColorSpace(const AName: string): TdxPDFCustomColorSpace; virtual;
    function GetFont(const AName: string): TdxPDFCustomFont; virtual;
    function GetGraphicsStateParameters(const AName: string): TdxPDFGraphicsStateParameters; virtual;
    function GetPattern(const AName: string): TdxPDFCustomPattern; virtual;
    function GetShading(const AName: string): TdxPDFCustomShading; virtual;
    function GetXObject(const AName: string): TdxPDFXObject; virtual;
    function GetProperties(const AName: string): TdxPDFCustomProperties; virtual;
    procedure Append(AResources: TdxPDFResources);
    procedure Pack;
  end;

  { TdxPDFPageContents }

  TdxPDFPageContents = class(TdxPDFStreamObject) // for internal use
  strict private
    FCommands: TdxPDFReferencedObjects;
    FContentList: TdxPDFReferencedObjects;

    function GetCommandCount: Integer;
    function GetResources: TdxPDFResources;
    procedure ReadItem(AStream: TdxPDFStream);
    procedure ReadContentList(ADictionary: TdxPDFReaderDictionary; AContentObject: TdxPDFBase);
  protected
    class function GetTypeName: string; override;
    function GetData: TBytes; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    procedure ClearCommands;
    procedure PopulateCommands(AResources: TdxPDFResources);

    property CommandCount: Integer read GetCommandCount;
    property Commands: TdxPDFReferencedObjects read FCommands;
    property ContentList: TdxPDFReferencedObjects read FContentList;
    property Resources: TdxPDFResources read GetResources;
  end;

  { TdxPDFPageData }

  TdxPDFPageData = class(TdxPDFObject) // for internal use
  strict private
    FAnnotations: TdxPDFReferencedObjects;
    FContents: TdxPDFPageContents;
    FDictionary: TdxPDFReaderDictionary;
    FNeedReadAnnotations: Boolean;
    FTransparencyGroup: TdxPDFTransparencyGroup;

    function GetAnnotations: TdxPDFReferencedObjects;
    function GetCommands: TdxPDFReferencedObjects;
    function GetResources: TdxPDFResources;
    procedure SetContents(const AValue: TdxPDFPageContents);
    procedure ReadAnnotations;
    procedure ReadGroup;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property Contents: TdxPDFPageContents read FContents write SetContents;
    property Resources: TdxPDFResources read GetResources;
  public
    procedure ClearCommands;
    procedure ExtractCommands;

    property Annotations: TdxPDFReferencedObjects read GetAnnotations;
    property Commands: TdxPDFReferencedObjects read GetCommands;
  end;

  { TdxPDFPageTreeObject }

  TdxPDFPageTreeObject = class(TdxPDFObject) // for internal use
  strict private
    FArtBox: TdxRectF;
    FBleedBox: TdxRectF;
    FTrimBox: TdxRectF;
    FUserUnit: Integer;
    FUseParentRotationAngle: Boolean;
    FResources: TdxPDFResources;
    function GetArtBox: TdxRectF;
    function GetBleedBox: TdxRectF;
    function GetCropBox: TdxRectF;
    function GetMediaBox: TdxRectF;
    function GetTrimBox: TdxRectF;
    function GetUserUnit: Integer;
    function GetParent: TdxPDFPageTreeObject;
    function GetParentArtBox: TdxRectF;
    function GetParentBleedBox: TdxRectF;
    function GetParentMediaBox: TdxRectF;
    function GetParentRotationAngle: Integer;
    function GetParentTrimBox: TdxRectF;
    function GetParentUseParentRotationAngle: Boolean;
    function GetParentUserUnit: Integer;
    function GetResources: TdxPDFResources;
    function GetRotationAngle: Integer;
    function GetOwnerResources: TdxPDFResources;
    procedure SetResources(const AValue: TdxPDFResources);
    procedure ReadResources(ADictionary: TdxPDFReaderDictionary);
  strict protected
    FCropBox: TdxRectF;
    FMediaBox: TdxRectF;
    FRotationAngle: Integer;
  protected
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    procedure Clear; virtual;

    property ArtBox: TdxRectF read GetArtBox;
    property BleedBox: TdxRectF read GetBleedBox;
    property CropBox: TdxRectF read GetCropBox;
    property MediaBox: TdxRectF read GetMediaBox;
    property TrimBox: TdxRectF read GetTrimBox;
    property UseParentRotationAngle: Boolean read FUseParentRotationAngle;

    property OwnerResources: TdxPDFResources read GetOwnerResources;
    property Resources: TdxPDFResources read GetResources write SetResources;
  public
    property RotationAngle: Integer read GetRotationAngle;
    property UserUnit: Integer read GetUserUnit;
  end;

  { TdxPDFPage }

  TdxPDFPage = class(TdxPDFPageTreeObject) // for internal use
  strict private type
    TRecognizeMode = (rmAll, rmAttachment);
  strict private
    FDeferredData: TdxPDFDeferredObject;
    FLock: TRTLCriticalSection;
    FRecognizedContent: TdxPDFRecognizedContent;
    FOnPack: TNotifyEvent;

    function GetBounds: TdxRectF;
    function GetData: TdxPDFPageData;
    function GetDocumentState: TdxPDFDocumentState;
    function GetNormalizedRotationAngle: Integer;
    function GetRecognizedContent: TdxPDFRecognizedContent;
    function GetSize: TdxPointF;
    procedure SetData(const AValue: TdxPDFPageData);

    function GetTextExpansionFactor(const AScaleFactor: TdxPointF): TdxPointF;
    procedure PackData;
    procedure RecognizeContent(AContent: TdxPDFRecognizedContent; AMode: TRecognizeMode);
  protected
    Locked: Boolean;
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function ScaleFactor(const ADPI, AScaleFactor: TdxPointF): TdxPointF;
    function UserSpaceFactor(const ADPI: TdxPointF): TdxPointF;
    procedure Export(ADevice: TObject; AParameters: TdxPDFExportParameters); overload;
    procedure Export(AParameters: TdxPDFExportParameters; AStream: TStream); overload;
    procedure LockAndExecute(AProc: TProc; ATryLock: Boolean = False);
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);

    property Data: TdxPDFPageData read GetData write SetData;
    property DocumentState: TdxPDFDocumentState read GetDocumentState;
    property NormalizedRotationAngle: Integer read GetNormalizedRotationAngle;
  public
    constructor Create(AOwner: TdxPDFObject; AInfo: TdxPDFDeferredObjectInfo); reintroduce; overload;
    constructor Create(APages: TdxPDFPages; AMediaBox, ACropBox: TdxRectF; ARotationAngle: Integer); reintroduce; overload;
    destructor Destroy; override;

    function CalculateRotationAngle(ARotationAngle: TcxRotationAngle): Integer;

    function Find(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF): TdxPDFRecognizedObject;
    function FindHyperlink(const P: TdxPointF; const AScaleFactor: TdxPointF; out AHyperlink: TdxPDFHyperlink): Boolean;
    function FindImage(const P: TdxPointF): TdxPDFImage;
    function FindInteractiveObject(const P: TdxPointF; const AScaleFactor: TdxPointF; out AField: TdxPDFAcroFormField): Boolean;
    function FindLine(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF; out ALine: TdxPDFTextLine): Boolean;
    function FindStartTextPosition(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF): TdxPDFTextPosition;

    function GetTopLeft(AAngle: TcxRotationAngle): TdxPointF;
    function FromUserSpace(const P, ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
      AAngle: TcxRotationAngle): TdxPointF; overload;
    function FromUserSpace(const R: TdxRectF; ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
      AAngle: TcxRotationAngle): TdxRectF; overload;
    function ToUserSpace(const P, ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
      AAngle: TcxRotationAngle): TdxPointF; overload;
    function ToUserSpace(const R: TdxRectF; const ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
      AAngle: TcxRotationAngle): TdxRectF; overload;
    procedure Pack;
    procedure PackRecognizedContent;

    property Bounds: TdxRectF read GetBounds;
    property RecognizedContent: TdxPDFRecognizedContent read GetRecognizedContent;
    property Size: TdxPointF read GetSize;
    property OnPack: TNotifyEvent read FOnPack write FOnPack;
  end;

  { TdxPDFPageTreeObjectList }

  TdxPDFPageTreeOnCreatePageNodeEvent = function (AOwner: TdxPDFPageTreeObjectList;
    ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject of object; // for internal use

  TdxPDFPageTreeObjectList = class(TdxPDFPageTreeObject) // for internal use
  strict private
    FChildren: TObjectList<TdxPDFPageTreeObject>;
    FOnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxPDFPageTreeObject;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    procedure Add(AChild: TdxPDFPageTreeObject);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPDFPageTreeObject read GetItem; default;
    property OnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent read FOnCreatePageNode write FOnCreatePageNode;
  end;

  { TdxPDFPageTreeNode }

  TdxPDFPageTreeNode = class(TdxPDFPageTreeObject) // for internal use
  strict private
    FNodeList: TdxPDFPageTreeObjectList;
    FOnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent;
    function OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
      ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    procedure AddNode(AChild: TdxPDFPageTreeObject);

    property NodeList: TdxPDFPageTreeObjectList read FNodeList;
    property OnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent read FOnCreatePageNode write FOnCreatePageNode;
  end;

  { TdxPDFPages }

  TdxPDFPages = class(TdxPDFPageTreeNode) // for internal use
  strict private
    FDictionary: TDictionary<Integer, TdxPDFPage>;
    FPageList: TList<TdxPDFPage>;
    FOnPagePack: TNotifyEvent;

    function GetCount: Integer;
    function GetPage(AIndex: Integer): TdxPDFPage;
    function OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList; ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;
  protected
    class function GetTypeName: string; override;
    procedure Clear; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function FindPage(ANumber: Integer): TdxPDFPage;
    function IndexOf(APage: TdxPDFPage): Integer;
    procedure AddPage(APage: TdxPDFPage);

    property OnPagePack: TNotifyEvent read FOnPagePack write FOnPagePack;
  public
    property Count: Integer read GetCount;
    property Page[AIndex: Integer]: TdxPDFPage read GetPage; default;
  end;

  { TdxPDFCustomDestination }

  TdxPDFCustomDestinationClass = class of TdxPDFCustomDestination;
  TdxPDFCustomDestination = class(TdxPDFObject) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FPage: TdxPDFPage;
    FPageID: Integer;
    FPageIndex: Integer;
    FPageObject: TdxPDFBase;

    function GetPage: TdxPDFPage;
    function GetPageIndex: Integer;
    function GetPages: TdxPDFPages;
    procedure SetPageObject(const AValue: TdxPDFBase);
    procedure ResolvePage;
  protected
    class function GetTypeName: string; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ACatalog: TdxPDFCatalog; AArray: TdxPDFArray); reintroduce;

    function GetTarget: TdxPDFTarget; virtual; abstract;
    procedure ReadParameters(AArray: TdxPDFArray); virtual;

    function IsSame(ADestination: TdxPDFCustomDestination): Boolean;
    procedure ResolveInternalPage;

    class function GetSingleValue(AArray: TdxPDFArray): Single; static;
    function CalculatePageIndex(APages: TdxPDFPages): Integer;
    function ValidateVerticalCoordinate(ATop: Single): Single;

    property Catalog: TdxPDFCatalog read FCatalog;
    property Page: TdxPDFPage read GetPage;
    property PageID: Integer read FPageID;
    property PageIndex: Integer read GetPageIndex;
    property PageObject: TdxPDFBase read FPageObject write SetPageObject;
    property Pages: TdxPDFPages read GetPages;
  public
    class function Parse(ACatalog: TdxPDFCatalog; AObject: TdxPDFBase): TdxPDFCustomDestination; static;
  end;

  { TdxPDFFileSpecificationData }

  TdxPDFFileSpecificationData = class(TdxPDFObject) // for internal use
  strict private
    FCreationDate: TDateTime;
    FData: TBytes;
    FDataStreamRef: TdxPDFStream;
    FHasModificationDate: Boolean;
    FMimeType: string;
    FModificationDate: TDateTime;
    FParametersDictionary: TdxPDFReaderDictionary;
    FSize: Integer;

    function GetData: TBytes;
    procedure SetData(const AValue: TBytes);
    procedure ResolveData;
  protected
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property Data: TBytes read GetData write SetData;
    property HasModificationDate: Boolean read FHasModificationDate;
    property MimeType: string read FMimeType write FMimeType;
    property ModificationDate: TDateTime read FModificationDate write FModificationDate;
    property Size: Integer read FSize;
  end;

  { TdxPDFFileSpecification }

  TdxPDFFileSpecification = class(TdxPDFObject) // for internal use
  strict private
    FAttachment: TdxPDFFileAttachment;
    FDescription: string;
    FFileName: string;
    FFileSpecificationData: TdxPDFFileSpecificationData;
    FFileSystem: string;
    FIndex: Integer;
    FRelationship: TdxPDFAssociatedFileRelationship;

    function GetAttachment: TdxPDFFileAttachment;
    function GetCreationDate: TDateTime;
    function GetFileData: TBytes;
    function GetMimeType: string;
    function GetModificationDate: TDateTime;
    function GetHasModificationDate: Boolean;
    function GetSize: Integer;
    procedure SetAttachment(const AValue: TdxPDFFileAttachment);
    procedure SetCreationDate(const AValue: TDateTime);
    procedure SetMimeType(const AValue: string);
    procedure SetModificationDate(const AValue: TDateTime);
    procedure SetFileData(const AValue: TBytes);
    procedure ReadFileName(ADictionary: TdxPDFReaderDictionary);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    constructor Create(const AFileName: string); reintroduce;
    class function Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFFileSpecification; static;

    property Attachment: TdxPDFFileAttachment read GetAttachment write SetAttachment;
    property CreationDate: TDateTime read GetCreationDate write SetCreationDate;
    property Description: string read FDescription write FDescription;
    property FileData: TBytes read GetFileData write SetFileData;
    property FileName: string read FFileName write FFileName;
    property FileSystem: string read FFileSystem;
    property HasModificationDate: Boolean read GetHasModificationDate;
    property Index: Integer read FIndex;
    property MimeType: string read GetMimeType write SetMimeType;
    property ModificationDate: TDateTime read GetModificationDate write SetModificationDate;
    property Relationship: TdxPDFAssociatedFileRelationship read FRelationship write FRelationship;
    property Size: Integer read GetSize;
  end;

  { TdxPDFCustomAction }

  TdxPDFActionList = class(TList<TdxPDFCustomAction>); // for internal use

  TdxPDFCustomAction = class(TdxPDFObject) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FNext: TdxPDFActionList;
    FNextValue: TdxPDFBase;
    function GetNext: TdxPDFActionList;
    procedure SetNextValue(const AValue: TdxPDFBase);
    procedure EnsureNextActions;
  protected
    procedure DestroySubClasses; override;
    procedure Execute(const AController: IdxPDFInteractivityController); virtual;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property Catalog: TdxPDFCatalog read FCatalog;
    property Next: TdxPDFActionList read GetNext;
    property NextValue: TdxPDFBase read FNextValue write SetNextValue;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFCustomAction; static;
  end;

  { TdxPDFCustomTree }

  TdxPDFCustomTree = class // for internal use
  strict private
    FDictionary: TdxPDFReferencedObjectDictionary;
    FNodeName: string;
    function CreateDeferredObject(ARepository: TdxPDFDocumentRepository; AValue: TdxPDFBase): TdxPDFDeferredObject;
    function CreateKey(AValue: TdxPDFBase): string;
    function GetCount: Integer;
    function GetItems: TDictionary<string, TdxPDFReferencedObject>;
    procedure ReadBranch(ARepository: TdxPDFDocumentRepository; AReferences: TdxPDFArray);
    procedure ReadKids(ARepository: TdxPDFDocumentRepository; AReferences: TdxPDFArray);
    procedure ReadNode(ARepository: TdxPDFDocumentRepository; APageObject: TdxPDFBase);
  strict protected
    function InternalGetValue(const AKey: string): TdxPDFObject;
    function GetDeferredObjectKey: string; virtual; abstract;
  protected
    procedure Read(ADictionary: TdxPDFReaderDictionary; const ANodeName: string);
    property Items: TDictionary<string, TdxPDFReferencedObject> read GetItems;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;

  { TdxPDFDestinationTree }

  TdxPDFDestinationTree = class(TdxPDFCustomTree) // for internal use
  strict protected
    function GetDeferredObjectKey: string; override;
  public
    function GetValue(const AKey: string): TdxPDFCustomDestination;
  end;

  { TdxPDFEmbeddedFileSpecificationTree }

  TdxPDFEmbeddedFileSpecificationTree = class(TdxPDFCustomTree) // for internal use
  strict protected
    function GetDeferredObjectKey: string; override;
  public
    function GetValue(const AKey: string): TdxPDFFileSpecification;
  end;

  { TdxPDFAnnotationAppearance }

  TdxPDFAnnotationAppearance = class(TdxPDFObject) // for internal use
  strict private
    FDefaultForm: TdxPDFForm;
    FForms: TDictionary<string, TdxPDFForm>;
    procedure SetDefaultForm(const AValue: TdxPDFForm);
  protected
    procedure DestroySubClasses; override;

    procedure SetForm(const AName: string; AForm: TdxPDFForm);

    property DefaultForm: TdxPDFForm read FDefaultForm write SetDefaultForm;
    property Forms: TDictionary<string, TdxPDFForm> read FForms;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary; const AKey: string): TdxPDFAnnotationAppearance; static;
    constructor Create(ACatalog: TdxPDFCatalog; const ABBox: TdxRectF); reintroduce; overload;
    constructor Create(ADefaultForm: TdxPDFForm; AForms: TDictionary<string, TdxPDFForm>); reintroduce; overload;
  end;

  { TdxPDFAnnotationAppearances }

  TdxPDFAnnotationAppearances = class(TdxPDFObject) // for internal use
  strict private
    FForm: TdxPDFForm;
    FDown: TdxPDFAnnotationAppearance;
    FNormal: TdxPDFAnnotationAppearance;
    FRollover: TdxPDFAnnotationAppearance;
    procedure SetForm(const AValue: TdxPDFForm);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; const AParentBox: TdxRectF); reintroduce; overload;
    procedure Read(AForm: TdxPDFForm); reintroduce; overload;

    procedure SetAnnotationForm(const AName: string; AForm: TdxPDFForm);

    property Normal: TdxPDFAnnotationAppearance read FNormal;
    property Form: TdxPDFForm read FForm write SetForm;
  end;

  { TdxPDFAnnotationBorderStyle }

  TdxPDFAnnotationBorderStyle = class(TdxPDFObject) // for internal use
  strict private
    FLineStyle: TdxPDFLineStyle;
    FStyleName: string;
    FWidth: Single;
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFAnnotationBorderStyle; static;
    class function ParseLineStyle(AArray: TdxPDFArray): TdxPDFLineStyle; static;

    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property StyleName: string read FStyleName;
    property Width: Single read FWidth;
  end;

  { TdxPDFAnnotationBorder }

  TdxPDFAnnotationBorder = class(TdxPDFObject) // for internal use
  strict private const
    DefaultHorizontalCornerRadius = 0;
    DefaultVerticalCornerRadius = 0;
    DefaultLineWidth = 1;
  strict private
    FHorizontalCornerRadius: Double;
    FVerticalCornerRadius: Double;
    FLineWidth: Double;
    FLineStyle: TdxPDFLineStyle;
    function GetIsDefault: Boolean;
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    property HorizontalCornerRadius: Double read FHorizontalCornerRadius;
    property IsDefault: Boolean read GetIsDefault;
    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property LineWidth: Double read FLineWidth;
    property VerticalCornerRadius: Double read FVerticalCornerRadius;
  end;

  { TdxPDFCustomAnnotation }

  TdxPDFCustomAnnotation = class(TdxPDFObject) // for internal use
  strict private
    FAppearance: TdxPDFAnnotationAppearances;
    FAppearanceName: string;
    FBorder: TdxPDFAnnotationBorder;
    FCatalog: TdxPDFCatalog;
    FColor: TdxPDFColor;
    FContents: string;
    FDictionary: TdxPDFReaderDictionary;
    FFlags: TdxPDFAnnotationFlags;
    FName: string;
    FPage: TdxPDFPage;
    FRect: TdxRectF;
    FResolved: Boolean;
    FStructParent: Integer;
    function GetAppearanceName: string;
    function GetBorder: TdxPDFAnnotationBorder;
    function GetColor: TdxPDFColor;
    function GetContents: string;
    function GetName: string;
    function GetRect: TdxRectF;
    procedure SetAppearance(const AValue: TdxPDFAnnotationAppearances);
    procedure SetAppearanceName(const AValue: string);
    procedure SetBorder(const AValue: TdxPDFAnnotationBorder);
    procedure SetColor(const AValue: TdxPDFColor);
    procedure SetContents(const AValue: string);
    procedure SetDictionary(const AValue: TdxPDFReaderDictionary);
    procedure SetName(const AValue: string);

    function EnsureAppearance(AState: TdxPDFDocumentState; AForm: TdxPDFForm): TdxPDFForm;
    function GetActualAppearanceForm: TdxPDFForm;
  protected
    class function GetTypeName: string; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage); reintroduce; virtual;

    function CreateAppearanceForm(const AName: string): TdxPDFForm; virtual;
    function CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject; virtual;
    function CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField; virtual; abstract;
    function GetAppearanceFormBoundingBox: TdxRectF; virtual;
    function GetUseDefaultForm: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    function NeedCreateAppearance(AForm: TdxPDFForm): Boolean; virtual;
    procedure Ensure; virtual;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); virtual;

    function GetAppearanceForm(AState: TdxPDFDocumentState): TdxPDFForm;
    function HasFlag(AFlags: TdxPDFAnnotationFlags): Boolean;

    property Appearance: TdxPDFAnnotationAppearances read FAppearance write SetAppearance;
    property AppearanceName: string read GetAppearanceName write SetAppearanceName;
    property Catalog: TdxPDFCatalog read FCatalog;
    property Color: TdxPDFColor read GetColor write SetColor;
    property Contents: string read GetContents write SetContents;
    property Dictionary: TdxPDFReaderDictionary read FDictionary write SetDictionary;
    property Name: string read GetName write SetName;
    property Page: TdxPDFPage read FPage;
    property Rect: TdxRectF read GetRect;
    property UseDefaultForm: Boolean read GetUseDefaultForm;
    property Visible: Boolean read GetVisible;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage): TdxPDFCustomAnnotation; static;

    property Border: TdxPDFAnnotationBorder read GetBorder write SetBorder;
  end;

  { TdxPDFFileAttachment }

  TdxPDFFileAttachment = class
  strict private
    FFileSpecification: TdxPDFFileSpecification;
    function GetCreationDate: TDateTime;
    function GetData: TBytes;
    function GetDescription: string;
    function GetFileName: string;
    function GetMimeType: string;
    function GetModificationDate: TDateTime;
    function GetRelationship: TdxPDFAssociatedFileRelationship;
    function GetSize: Integer;
  strict protected
    property FileSpecification: TdxPDFFileSpecification read FFileSpecification;
  protected
    constructor Create(AFileSpecification: TdxPDFFileSpecification);

    function GetModificationDateAsString: string;
    function GetSizeAsString: string;

    property MimeType: string read GetMimeType;
    property Relationship: TdxPDFAssociatedFileRelationship read GetRelationship;
  public
    property CreationDate: TDateTime read GetCreationDate;
    property Data: TBytes read GetData;
    property Description: string read GetDescription;
    property FileName: string read GetFileName;
    property ModificationDate: TDateTime read GetModificationDate;
    property Size: Integer read GetSize;
  end;

  { TdxPDFFileAttachmentList }

  TdxPDFFileAttachmentList = class(TList<TdxPDFFileAttachment>)
  protected
    procedure Populate(ACatalog: TdxPDFCatalog);
  end;

  { TdxPDFNames }

  TdxPDFNames = class(TdxPDFObject) // for internal use
  strict private
    FEmbeddedFileSpecifications: TdxPDFEmbeddedFileSpecificationTree;
    FPageDestinations: TdxPDFDestinationTree;
    procedure ReadEmbeddedFileSpecifications(ADictionary: TdxPDFReaderDictionary);
    procedure ReadPageDestinations(ADictionary: TdxPDFReaderDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function GetEmbeddedFileSpecification(const AName: string): TdxPDFFileSpecification;
    function GetPageDestination(const AName: string): TdxPDFCustomDestination;
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
  end;

  { TdxPDFInteractiveFormFieldTextState }

  TdxPDFInteractiveFormFieldTextState = class(TdxPDFObject) // for internal use
  strict private const
    DefaultFontSize  = 12;
  strict private
    FCharacterSpacing: Double;
    FCommandsAsBytes: TBytes;
    FFontCommand: TdxPDFCustomCommand;
    FHorizontalScaling: Double;
    FWordSpacing: Double;
    function GetAppearanceCommandsInheritable(AFormField: TdxPDFInteractiveFormField): TdxPDFReferencedObjects;
    function GetFontSize: Double;
    function FindSetTextFontCommand(ACommands: TdxPDFReferencedObjects): TdxPDFCustomCommand;
    procedure ConvertCommandsToBytes(AField: TdxPDFInteractiveFormField);
  protected
    procedure DestroySubClasses; override;
    procedure Initialize; override;
  public
    constructor Create(AField: TdxPDFInteractiveFormField); reintroduce;

    property CharacterSpacing: Double read FCharacterSpacing;
    property CommandsAsBytes: TBytes read FCommandsAsBytes;
    property FontCommand: TdxPDFCustomCommand read FFontCommand;
    property FontSize: Double read GetFontSize;
    property HorizontalScaling: Double read FHorizontalScaling;
    property WordSpacing: Double read FWordSpacing;
  end;

  { TdxPDFInteractiveFormField }

  TdxPDFInteractiveFormField = class(TdxPDFObject) // for internal use
  strict private
    FAlternateName: string;
    FAppearanceCommands: TdxPDFReferencedObjects;
    FName: string;
    FMappingName: string;
    FFormCreated: Boolean;
    FForm: TdxPDFInteractiveForm;
    FFlags: TdxPDFInteractiveFormFieldFlags;
    FParent: TdxPDFInteractiveFormField;
    FKids: TdxPDFInteractiveFormFieldCollection;
    FTextJustification : TdxPDFTextJustification;
    FTextState: TdxPDFInteractiveFormFieldTextState;
    FWidget: TdxPDFCustomAnnotation;
    FValuesProvider: TdxPDFInteractiveFormField;
    FOnValueChanged: TNotifyEvent;
    function GetFullName: string;
    function GetTextState: TdxPDFInteractiveFormFieldTextState;
    procedure ReadKids(ADictionary: TdxPDFReaderDictionary; AWidgetNumber: Integer);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
      ADictionary: TdxPDFReaderDictionary; ANumber: Integer); reintroduce; virtual;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    function CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject; virtual;
    function GetAcroFieldClass: TdxPDFAcroFormFieldClass; virtual;
    procedure InternalSetValue(const AValue: Variant; AState: TdxPDFDocumentState); virtual;

    function CreateAcroField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
    procedure Changed(AState: TdxPDFDocumentState);

    property AlternateName: string read FAlternateName;
    property Form: TdxPDFInteractiveForm read FForm;
    property ValuesProvider:TdxPDFInteractiveFormField read FValuesProvider;
    property Widget: TdxPDFCustomAnnotation read FWidget;

    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  public
    class function Parse(AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
      ADictionary: TdxPDFReaderDictionary; ANumber: Integer): TdxPDFInteractiveFormField; static;

    function GetFontInfo(AState: TdxPDFDocumentState): TdxPDFFontInfo;
    function GetValue: Variant; virtual;
    function UseDefaultAppearanceForm: Boolean; virtual;
    procedure SetValue(const AValue: Variant; AState: TdxPDFDocumentState);

    property AppearanceCommands: TdxPDFReferencedObjects read FAppearanceCommands;
    property FullName: string read GetFullName;
    property Flags: TdxPDFInteractiveFormFieldFlags read FFlags;
    property Parent: TdxPDFInteractiveFormField read FParent;
    property TextJustification: TdxPDFTextJustification read FTextJustification;
    property TextState: TdxPDFInteractiveFormFieldTextState read GetTextState;
  end;

  { TdxPDFInteractiveFormFieldCollection }

  TdxPDFInteractiveFormFieldCollection = class(TdxPDFObject) // for internal use
  strict private
    FItems: TdxPDFReferencedObjects;
    FOnInteractiveFormFieldValueChanged: TNotifyEvent;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; AFieldArray: TdxPDFArray; AForm: TdxPDFInteractiveForm;
      AParent: TdxPDFInteractiveFormField); reintroduce;
    property OnInteractiveFormFieldValueChanged: TNotifyEvent read FOnInteractiveFormFieldValueChanged
      write FOnInteractiveFormFieldValueChanged;
  end;

  { TdxPDFXFAForm }

  TdxPDFXFAForm = record // for internal use
  public
    Content: string;
    procedure Initialize(const AData: TBytes); overload;
    procedure Initialize(ARepository: TdxPDFDocumentRepository; AArray: TdxPDFArray); overload;
  end;

  { TdxPDFInteractiveForm }

  TdxPDFInteractiveForm = class(TdxPDFObject) // for internal use
  strict private
    FFields: TdxPDFInteractiveFormFieldCollection;
    FDefaultAppearanceCommands: TdxPDFReferencedObjects;
    FDefaultAppearanceCommandsData: TBytes;
    FDefaultTextJustification: TdxPDFTextJustification;
    FNeedAppearances: Boolean;
    FResources: TdxPDFResources;
    FSignatureFlags: TdxPDFSignatureFlags;
    FXFAForm: TdxPDFXFAForm;

    FOnInteractiveFormFieldValueChanged: TNotifyEvent;
    procedure SetResources(const AValue: TdxPDFResources);
    procedure ReadXFAForm(ADictionary: TdxPDFReaderDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property Resources: TdxPDFResources read FResources write SetResources;
    property OnInteractiveFormFieldValueChanged: TNotifyEvent read FOnInteractiveFormFieldValueChanged
      write FOnInteractiveFormFieldValueChanged;
  public
    property DefaultAppearanceCommands: TdxPDFReferencedObjects read FDefaultAppearanceCommands;
    property NeedAppearances: Boolean read FNeedAppearances write FNeedAppearances;
  end;

  { TdxPDFCatalog }

  TdxPDFCatalog = class(TdxPDFObject) // for internal use
  strict private
    FAcroForm: TdxPDFInteractiveForm;
    FDictionary: TdxPDFReaderDictionary;
    FNeedReadNames: Boolean;
    FNeedReadOutlines: Boolean;
    FNames: TdxPDFNames;
    FOpenAction: TdxPDFCustomAction;
    FOpenDestination: TdxPDFCustomDestination;
    FOutlines: TdxPDFObject;
    FPages: TdxPDFPages;
    FOnInteractiveFormFieldValueChanged: TNotifyEvent;
    function GetNames: TdxPDFNames;
    function GetOutlines: TdxPDFObject;
    procedure SetPages(const AValue: TdxPDFPages);
    procedure ReadAcroForm;
    procedure ReadInteractiveObjects;
    procedure ReadNames(ADictionary: TdxPDFReaderDictionary);
    procedure ReadOutlines(ADictionary: TdxPDFReaderDictionary);
    procedure ReadPages;
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure SetRepository(const AValue: TdxPDFDocumentRepository); override;

    function GetEmbeddedFileSpecification(const AName: string): TdxPDFFileSpecification;
    function GetDestination(const AName: string): TdxPDFCustomDestination;
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);

    property Names: TdxPDFNames read GetNames;
    property OpenAction: TdxPDFCustomAction read FOpenAction;
    property OpenDestination: TdxPDFCustomDestination read FOpenDestination;
    property Outlines: TdxPDFObject read GetOutlines;
  public
    property Pages: TdxPDFPages read FPages write SetPages;
    property OnInteractiveFormFieldValueChanged: TNotifyEvent read FOnInteractiveFormFieldValueChanged
      write FOnInteractiveFormFieldValueChanged;
  end;

  { TdxPDFDocumentInformation }

  TdxPDFDocumentInformation = class(TdxPDFObject)
  strict private
    FApplication: string;
    FAuthor: string;
    FCreationDate: TDateTime;
    FKeywords: string;
    FModificationDate: TDateTime;
    FProducer: string;
    FSubject: string;
    FTitle: string;
  protected
    FFileName: string;
    FFileSize: Int64;
    FVersion: TdxPDFVersion;

    class function GetTypeName: string; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    property Application: string read FApplication;
    property Author: string read FAuthor;
    property CreationDate: TDateTime read FCreationDate;
    property Keywords: string read FKeywords;
    property ModificationDate: TDateTime read FModificationDate;
    property FileName: string read FFileName;
    property FileSize: Int64 read FFileSize;
    property Producer: string read FProducer;
    property Subject: string read FSubject;
    property Version: TdxPDFVersion read FVersion;
    property Title: string read FTitle;
  end;

  { TdxPDFCommandOperandStack }

  TdxPDFCommandOperandStack = class(TdxPDFReferencedObject) // for internal use
  strict private
    FStack: TObjectStack<TdxPDFBase>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function TryPopLastName: string;
    function PopAsArray: TdxPDFArray;
    function PopAsBytes: TBytes;
    function PopAsInteger: Integer;
    function PopAsObject: TdxPDFBase;
    function PopAsSingle: Single;
    function PopAsString: string;
    procedure Clear;
    procedure Push(AObject: TdxPDFBase);

    property Count: Integer read GetCount;
  end;

  { TdxPDFCustomCommand }

  TdxPDFCustomCommand = class(TdxPDFBase) // for internal use
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure WriteCommandName(AStream: TdxPDFWriterStream);
    procedure WriteOperand(AStream: TdxPDFWriterStream; AOperand: Double); overload;
    procedure WriteOperand(AStream: TdxPDFWriterStream; AOperand: Integer); overload;
    procedure WriteOperand(AStream: TdxPDFWriterStream; const AOperand: TdxSizeF); overload;
    procedure WriteOperand(AStream: TdxPDFWriterStream; const AOperand: TdxPointF); overload;
    procedure WriteOperand(AStream: TdxPDFWriterStream; const AOperand: string); overload;
    procedure WriteUnaryCommand(AStream: TdxPDFWriterStream; AOperand: Single); overload;
    procedure WriteUnaryCommand(AStream: TdxPDFWriterStream; AOperand: Integer); overload;
    procedure WriteUnaryCommand(AStream: TdxPDFWriterStream; const AOperand: string); overload;
    procedure WriteUnaryCommand(AStream: TdxPDFWriterStream; const AOperand: TdxPointF); overload;
  public
    constructor Create; overload; override;
    constructor Create(AOperands: TdxPDFCommandOperandStack; AResources: TdxPDFResources); overload; virtual;

    class function GetName: string; virtual;
    procedure Execute(ACommandInterpreter: IdxPDFCommandInterpreter); virtual; abstract;
    procedure Write(AStream: TdxPDFWriterStream; AResources: TdxPDFResources); virtual; abstract;
  end;

  { TdxPDFLineStyle }

  TdxPDFLineStyle = class(TdxPDFReferencedObject) // for internal use
  strict private
    FPattern: TDoubleDynArray;
    FPhase: Double;

    function AsDouble(AValue: TdxPDFNumericObject): Double;
    procedure ReadPattern(APattern: TdxPDFArray);
  public
    class function CreateSolid: TdxPDFLineStyle; static;
    constructor Create(const APattern: TDoubleDynArray; APhase: Double); reintroduce; overload;
    constructor Create(APattern: TdxPDFReferencedObject); reintroduce; overload;
    constructor Create(APattern, APhase: TdxPDFReferencedObject); reintroduce; overload;
    constructor Create(AParameters: TdxPDFArray); reintroduce; overload;

    function IsDashed: Boolean;

    property Pattern: TDoubleDynArray read FPattern;
    property Phase: Double read FPhase;
  end;

  { TdxPDFDocumentRepository }

  TdxPDFDocumentRepository = class(TdxPDFCustomRepository) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FEncryptionInfo: TdxPDFEncryptionInfo;
    FFolderName: string;
    FFontDataStorage: TdxPDFFontDataStorage;
    FImageDataStorage: TdxPDFDocumentImageDataStorage;
    FLock: TRTLCriticalSection;
    FParser: TdxPDFDocumentParser;
    FSharedResources: TdxPDFUniqueReferences;
    FStream: TStream;

    function CreateObjectParser(AObjectNumber: Integer): TdxPDFStructureParser;
    function ResolveIndirectObject(AObject: TdxPDFIndirectObject): TdxPDFReferencedObject;
    function ResolveIndirectReference(AReference: TdxPDFReference): TdxPDFReferencedObject;
    function ResolveStreamElement(AElement: TdxPDFStreamElement): TdxPDFReferencedObject;
    procedure ReplaceObject(ANumber: Integer; AObject: TdxPDFReferencedObject);
  protected
    function ResolveObject(ANumber: Integer): TdxPDFReferencedObject; override;

    function IsResourcesShared(AResources: TdxPDFResources): Boolean;
    function GetResources(ADictionary: TdxPDFReaderDictionary): TdxPDFResources;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    procedure Clear; override;

    function CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
    function CreateFont(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont;
    function CreateImage(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFDocumentImage;
    function GetAction(ANumber: Integer): TdxPDFCustomAction; overload;
    function GetAction(AObject: TdxPDFBase): TdxPDFCustomAction; overload;
    function GetAnnotation(ANumber: Integer; APage: TdxPDFPage): TdxPDFCustomAnnotation;
    function GetDestination(ANumber: Integer): TdxPDFCustomDestination; overload;
    function GetDestination(AObject: TdxPDFBase): TdxPDFCustomDestination; overload;
    function GetDictionary(ANumber: Integer): TdxPDFReaderDictionary; overload;
    function GetInteractiveFormField(AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
      AValue: TdxPDFBase): TdxPDFInteractiveFormField;
    function GetPage(ANumber: Integer): TdxPDFPage;
    function GetString(ANumber: Integer): string;
    function GetWidget(ANumber: Integer): TdxPDFCustomAnnotation;
    function IsSharedResources(AResources: TdxPDFResources): Boolean;
    function IsValidReferences: Boolean;
    function TryGetAnnotation(ANumber: Integer; out AAnnotation: TdxPDFCustomAnnotation): Boolean;
    procedure AddStreamElement(ANumber: Integer; AObject: TdxPDFReferencedObject);
    procedure ReadEncryptionInfo(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
    procedure RemoveCorruptedObjects;

    property Catalog: TdxPDFCatalog read FCatalog write FCatalog;
    property EncryptionInfo: TdxPDFEncryptionInfo read FEncryptionInfo;
    property FolderName: string read FFolderName;
    property FontDataStorage: TdxPDFFontDataStorage read FFontDataStorage;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage read FImageDataStorage;
    property Parser: TdxPDFDocumentParser read FParser;
    property Stream: TStream read FStream;
  end;

  { TdxPDFReaderDictionary }

  TdxPDFReaderDictionary = class(TdxPDFDictionary) // for internal use
  strict private
    FRepository: TdxPDFDocumentRepository;
  public
    constructor Create(ARepository: TdxPDFDocumentRepository);

    function GetAction(const AKey: string): TdxPDFCustomAction;
    function GetAnnotationAppearance(AObject: TdxPDFBase; const AParentBBox: TdxRectF): TdxPDFAnnotationAppearances;
    function GetAnnotationHighlightingMode: TdxPDFAnnotationHighlightingMode;
    function GetAppearance(AResources: TdxPDFResources): TdxPDFReferencedObjects;
    function GetAssociatedFileRelationship: TdxPDFAssociatedFileRelationship;
    function GetColor(const AKey: string): TdxPDFColor;
    function GetDeferredFormFieldCollection(const AKey: string): TdxPDFInteractiveFormFieldCollection;
    function GetDestinationInfo(const AKey: string): TdxPDFDestinationInfo;
    function GetDictionary(const AKey: string): TdxPDFReaderDictionary; overload;
    function GetForm(ANumber: Integer): TdxPDFForm;
    function GetObject(const AKey: string): TdxPDFBase; override;
    function GetObjectNumber(const AKey: string): Integer;
    function GetTextJustification: TdxPDFTextJustification;
    function TryGetDictionary(const AKey: string; out AValue: TdxPDFReaderDictionary): Boolean;
    function TryGetStreamDictionary(const AKey: string; out AValue: TdxPDFReaderDictionary): Boolean;
    function GetResources(const AKey: string): TdxPDFResources;

    property Repository: TdxPDFDocumentRepository read FRepository;
  end;

  { TdxPDFAcroFormField }

  TdxPDFAcroFormField = class(TdxPDFRecognizedObject)
  strict private
    FDocumentState: TdxPDFDocumentState;
    FField: TdxPDFInteractiveFormField;
    function GetName: string;
    procedure SetField(const AValue: TdxPDFInteractiveFormField);
  strict protected
    function GetAnnotation: TdxPDFCustomAnnotation; virtual;
    function GetBounds: TdxPDFOrientedRect; virtual;
    function GetCursor: TCursor; virtual;
    function GetFlags: TdxPDFInteractiveFormFieldFlags; virtual;
    function GetHint: string; virtual;
    function GetInteractiveOperation: TdxPDFInteractiveOperation; virtual;

    function GetPage: TdxPDFPage;
    function GetPageIndex: Integer;
    function GetRect: TdxRectF;

    property Annotation: TdxPDFCustomAnnotation read GetAnnotation;
    property Flags: TdxPDFInteractiveFormFieldFlags read GetFlags;
    property Hint: string read GetHint;
    property Name: string read GetName;
    property Page: TdxPDFPage read GetPage;
  protected
    property Bounds: TdxPDFOrientedRect read GetBounds;
    property DocumentState: TdxPDFDocumentState read FDocumentState write FDocumentState;
    property InteractiveOperation: TdxPDFInteractiveOperation read GetInteractiveOperation;
    property Field: TdxPDFInteractiveFormField read FField write SetField;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TdxPDFAcroFormActionField }

  TdxPDFAcroFormActionField = class(TdxPDFAcroFormField, IdxPDFInteractiveObject, IdxPDFHintableObject)
  strict protected
    function GetCursor: TCursor; override;
    function GetInteractiveOperation: TdxPDFInteractiveOperation; override;

    function IsResetFocusingNeeded: Boolean; virtual;
  protected
    procedure ExecuteOperation(const AController: IdxPDFInteractivityController);
  end;

  { TdxPDFAnnotationField }

  TdxPDFAnnotationFieldClass = class of TdxPDFAnnotationField;
  TdxPDFAnnotationField = class(TdxPDFAcroFormActionField)
  strict protected
    FAnnotation: TdxPDFCustomAnnotation;
    function GetAnnotation: TdxPDFCustomAnnotation; override;
    function GetBounds: TdxPDFOrientedRect; override;
    function GetHint: string; override;
    function GetHitCode: Integer; override;
  protected
    procedure SetAnnotation(const AValue: TdxPDFCustomAnnotation);
  public
    destructor Destroy; override;
  end;

  { TdxPDFFileAttachmentAnnotationField }

  TdxPDFFileAttachmentAnnotationField = class(TdxPDFAnnotationField)
  strict private
    function GetAttachment: TdxPDFFileAttachment;
  strict protected
    function GetCursor: TCursor; override;
    function GetHitCode: Integer; override;
    function IsResetFocusingNeeded: Boolean; override;
  protected
    property Attachment: TdxPDFFileAttachment read GetAttachment;
  end;

  { TdxPDFHyperlink }

  TdxPDFHyperlink = class(TdxPDFAnnotationField)
  protected
    function GetHint: string; override;
    function GetHitCode: Integer; override;
  public
    property Hint;
  end;

  TdxPDFAcroFormFieldList = class(TdxPDFRecognizedObjectList<TdxPDFAcroFormField>);
  TdxPDFAnnotationFieldList = class(TdxPDFRecognizedObjectList<TdxPDFAnnotationField>);
  TdxPDFHyperlinkList = class(TdxPDFRecognizedObjectList<TdxPDFHyperlink>);

  { TdxPDFRecognizedContent }

  TdxPDFRecognizedContent = class
  strict private
    FAcroFormFields: TdxPDFAcroFormFieldList;
    FAnnotationFields: TdxPDFAnnotationFieldList;
    FAttachments: TList<TdxPDFFileAttachment>;
    FHyperlinks: TdxPDFHyperlinkList;
    FImages: TdxPDFImageList;
    FTextLines: TdxPDFTextLineList;
    function GetText: string;
  protected
    procedure AddAnnotationField(AField: TdxPDFAnnotationField);

    property Attachments: TList<TdxPDFFileAttachment> read FAttachments; // for internal use
    property AnnotationFields: TdxPDFAnnotationFieldList read FAnnotationFields;
  public
    constructor Create;
    destructor Destroy; override;

    property AcroFormFields: TdxPDFAcroFormFieldList read FAcroFormFields;
    property Hyperlinks: TdxPDFHyperlinkList read FHyperlinks;
    property Images: TdxPDFImageList read FImages;
    property Text: string read GetText;
    property TextLines: TdxPDFTextLineList read FTextLines;
  end;

  { TdxPDFTextParser }

  TdxPDFTextParser = class
  strict private
    FContent: TdxPDFRecognizedContent;
    FFontDataStorage: TObjectDictionary<TdxPDFCustomFont, TdxPDFFontData>;
    FPageBlocks: TObjectList<TdxPDFTextBlock>;
    FPageCropBox: TdxRectF;
    FParserState: TdxPDFTextParserState;
  public
    constructor Create(const APageCropBox: TdxRectF; AContent: TdxPDFRecognizedContent);
    destructor Destroy; override;

    procedure AddBlock(const AStringData: TdxPDFStringData; AState: TdxPDFTextState; ATextWidthFactor,
      ATextHeightFactor: Single; const AGlyphOffsets: TDoubleDynArray; ALocation: TdxPointF; AAngle: Single);
    procedure Parse;
  end;

  { TdxPDFTextUtils }

  TdxPDFTextUtils = class(TdxPDFRecognizedTextUtils)
  public
    class function ConvertToString(const ARanges: TdxPDFPageTextRanges; APages: TdxPDFPages): string; overload;
  end;

function dxPDFGetDocumentObjectClass(const ATypeName: string): TdxPDFObjectClass;
procedure dxPDFRegisterDocumentObjectClass(AClass: TdxPDFObjectClass); overload;
procedure dxPDFRegisterDocumentObjectClass(const AName: string; AClass: TdxPDFObjectClass); overload;
procedure dxPDFUnregisterDocumentObjectClass(AClass: TdxPDFObjectClass); overload;
procedure dxPDFUnregisterDocumentObjectClass(const AName: string; AClass: TdxPDFObjectClass); overload;

function dxPDFCreateDocumentObject(AOwner: TdxPDFObject; ADictionary: TdxPDFDictionary;
  const ATypeName: string; ARepository: TdxPDFCustomRepository): TdxPDFObject;

implementation

uses
  Variants, Math, TypInfo, Character, Contnrs, IOUtils, cxLibraryConsts, dxCore, dxCoreGraphics, dxStringHelper,
  dxTypeHelpers, dxHash, dxPDFDocument, dxPDFCommandInterpreter, dxPDFColorSpace, dxPDFFont, dxPDFFontEncoding,
  dxPDFCommand, dxPDFFunction, dxPDFShading, dxPDFType1Font, dxPDFFontUtils, dxPDFUtils, dxPDFInteractivity,
  dxDPIAwareUtils;

type
  TdxPDFCustomCommandAccess = class(TdxPDFCustomCommand);
  TdxPDFCustomCommandInterpreterAccess = class(TdxPDFCustomCommandInterpreter);
  TdxPDFCustomObjectAccess = class(TdxPDFBase);
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFDocumentPageContentsAccess = class(TdxPDFPageContents);
  TdxPDFDocumentPagesAccess = class(TdxPDFPages);
  TdxPDFDocumentStreamObjectAccess = class(TdxPDFStreamObject);
  TdxPDFEmbeddedFileSpecificationTreeAccess = class(TdxPDFEmbeddedFileSpecificationTree);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFJumpActionAccess = class(TdxPDFJumpAction);
  TdxPDFLinkAnnotationAccess = class(TdxPDFLinkAnnotation);
  TdxPDFMarkupAnnotationAccess = class(TdxPDFMarkupAnnotation);
  TdxPDFModifyTransformationMatrixCommandAccess = class(TdxPDFModifyTransformationMatrixCommand);
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);
  TdxPDFRecognizedObjectAccess = class(TdxPDFReferencedObject);
  TdxPDFReferencedObjectDictionaryAccess = class(TdxPDFReferencedObjectDictionary);
  TdxPDFTextLineAccess = class(TdxPDFTextLine);
  TdxPDFWidgetAnnotationAccess = class(TdxPDFWidgetAnnotation);

  EdxPDFCommandOperandsException = class(EdxPDFException);
  TdxPDFBaseParserAccess = class(TdxPDFBaseParser);
  TdxPDFCommandGroupAccess = class(TdxPDFCommandGroup);

  { TdxPDFTextWordCharacterComparer }

  TdxPDFTextWordCharacterComparer = class(TComparer<TdxPDFTextCharacter>)
  public
    function Compare(const Left, Right: TdxPDFTextCharacter): Integer; override;
  end;

  { TdxPDFFileAttachmentComparer }

  TdxPDFFileAttachmentComparer = class(TInterfacedObject, IComparer<TdxPDFFileAttachment>)
  strict private
    function Compare(const Left, Right: TdxPDFFileAttachment): Integer;
  end;

  { TdxPDFObjectFactory }

  TdxPDFObjectFactory = class(TdxPDFFactory<TdxPDFObjectClass>)
  public
    function GetObjectClass(const AType: string): TdxPDFObjectClass;
    procedure RegisterClass(AClass: TdxPDFObjectClass); overload;
    procedure UnregisterClass(AClass: TdxPDFObjectClass); overload;
  end;

  { TdxPDFImageInterpolator }

  TdxPDFImageInterpolator = class
  strict private
    FArrayPosition: Integer;
    FData: TBytes;
    FFactor: Double;
    FLastSourcePosition: Integer;
    FNext: TBytes;
    FPrevious: TBytes;
    FResult: TBytes;
    FResultPosition: Integer;
    FSize: Integer;
    FSourcePosition: Integer;

    procedure FillComponents(const ASourceComponents: TBytes; AIndex: Integer; var AComponents: TBytes);
    procedure Start(APosition: Integer);
    procedure Interpolate(AValue: Integer);
  public
    constructor Create(const AData: TBytes; ASize, ASourceSize, ADestinationSize: Integer; var AResult: TBytes);

    class function HorizontalInterpolation(const AData: TBytes; ASourceWidth, ASourceStride, ADestinationWidth, AHeight,
      AComponentCount: Integer): TBytes;
    class function VerticalInterpolation(const AData: TBytes; AWidth, ASourceHeight, ADestinationHeight: Integer): TBytes;
  end;

  { TdxPDFReaderObjectStream }

  TdxPDFReaderObjectStream = class(TdxPDFBase)
  strict private
    FObjects: TdxPDFReferencedObjects;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber: Integer; AStream: TdxPDFStream);
    destructor Destroy; override;

    property Objects: TdxPDFReferencedObjects read FObjects;
  end;

  { TdxPDFCommandList }

  TdxPDFCommandList = class(TdxPDFReferencedObjects)
  public
    function ToByteArray(AResources: TdxPDFResources): TBytes;
  end;

var
  dxgPDFDocumentObjectFactory: TdxPDFObjectFactory;

function HasFlag(ASourceFlags, AFlags: TdxPDFGlyphMappingFlags): Boolean;
begin
  Result := (Integer(ASourceFlags) and Integer(AFlags)) <> 0;
end;

function dxPDFCreateDocumentObject(AOwner: TdxPDFObject; ADictionary: TdxPDFDictionary;
  const ATypeName: string; ARepository: TdxPDFCustomRepository): TdxPDFObject;
var
  AClass: TdxPDFObjectClass;
begin
  Result := nil;
  AClass := dxPDFGetDocumentObjectClass(ADictionary.GetString(ATypeName));
  if (AClass <> nil) then
    Result := AClass.Create(AOwner)
end;

function dxPDFIsObjectSupported(AOwner: TdxPDFObject; ADictionary: TdxPDFDictionary;
  const ATypeName: string; ARepository: TdxPDFCustomRepository): Boolean;
begin
  Result := dxPDFGetDocumentObjectClass(ADictionary.GetString(ATypeName)) <> nil;
end;

function dxPDFDocumentObjectFactory: TdxPDFObjectFactory;
begin
  if dxgPDFDocumentObjectFactory = nil then
    dxgPDFDocumentObjectFactory := TdxPDFObjectFactory.Create;
  Result := dxgPDFDocumentObjectFactory;
end;

function dxPDFGetDocumentObjectClass(const ATypeName: string): TdxPDFObjectClass;
begin
  Result := dxPDFDocumentObjectFactory.GetObjectClass(ATypeName);
end;

procedure dxPDFUnregisterDocumentObjectClass(const AName: string; AClass: TdxPDFObjectClass);
begin
  if dxgPDFDocumentObjectFactory <> nil then
    dxPDFDocumentObjectFactory.UnregisterClass(AName);
end;

procedure dxPDFUnregisterDocumentObjectClass(AClass: TdxPDFObjectClass);
begin
  dxPDFUnregisterDocumentObjectClass(AClass.GetTypeName, AClass);
end;

procedure dxPDFRegisterDocumentObjectClass(AClass: TdxPDFObjectClass);
begin
  dxPDFRegisterDocumentObjectClass(AClass.GetTypeName, AClass);
end;

procedure dxPDFRegisterDocumentObjectClass(const AName: string; AClass: TdxPDFObjectClass);
begin
  dxPDFDocumentObjectFactory.Register(AName, AClass);
end;

{ TdxPDFTextWordCharacterComparer }

function TdxPDFTextWordCharacterComparer.Compare(const Left, Right: TdxPDFTextCharacter): Integer;
begin
  Result := IfThen(TdxPDFTextUtils.GetOrientedDistance(Left.Bounds.TopLeft, Right.Bounds.TopLeft, Left.Bounds.Angle) < 0, 1, -1);
end;

{ TdxPDFFileAttachmentComparer }

function TdxPDFFileAttachmentComparer.Compare(const Left, Right: TdxPDFFileAttachment): Integer;
begin
  Result := CompareStr(Left.FileName, Right.FileName);
end;

{ TdxPDFExportParameters }

constructor TdxPDFExportParameters.Create(AState: TdxPDFDocumentState);
begin
  inherited Create;
  FDocumentState := AState;
  Angle := FDocumentState.RotationAngle;
end;

function TdxPDFExportParameters.IsCanceled: Boolean;
begin
  Result := Assigned(CancelCallback) and CancelCallback;
end;

{ TdxPDFObjectFactory }

function TdxPDFObjectFactory.GetObjectClass(const AType: string): TdxPDFObjectClass;
begin
  if not TryGetClass(AType, Result) then
    Result := nil;
end;

procedure TdxPDFObjectFactory.RegisterClass(AClass: TdxPDFObjectClass);
begin
  Register(AClass.GetTypeName, AClass);
end;

procedure TdxPDFObjectFactory.UnregisterClass(AClass: TdxPDFObjectClass);
begin
  UnregisterClass(AClass.GetTypeName);
end;

{ TdxPDFImageInterpolator }

constructor TdxPDFImageInterpolator.Create(const AData: TBytes; ASize, ASourceSize, ADestinationSize: Integer; var AResult: TBytes);
begin
  inherited Create;
  FData := AData;
  FSize := ASize;
  FResult := AResult;
  FLastSourcePosition := ASourceSize - 1;
  FFactor := ASourceSize / ADestinationSize;
  SetLength(FPrevious, ASize);
  SetLength(FNext, ASize);
end;

class function TdxPDFImageInterpolator.HorizontalInterpolation(const AData: TBytes; ASourceWidth, ASourceStride,
  ADestinationWidth, AHeight, AComponentCount: Integer): TBytes;
var
  ATempData: TBytes;
  AInterpolator: TdxPDFImageInterpolator;
  X, Y, ADestinationIndex, ASourceX, ADestinationX: Integer;
begin
  SetLength(Result, ADestinationWidth * AHeight * AComponentCount);
  AInterpolator := TdxPDFImageInterpolator.Create(AData, AComponentCount, ASourceWidth, ADestinationWidth, Result);
  try
    if ASourceWidth = 1 then
    begin
      ASourceX := 0;
      ADestinationIndex := 0;
      SetLength(ATempData, AComponentCount);
      for Y := 0 to AHeight - 1 do
      begin
        AInterpolator.FillComponents(AData, ASourceX, ATempData);
        for ADestinationX := 0 to ADestinationWidth - 1 do
        begin
          TdxPDFUtils.CopyData(ATempData, 0, Result, ADestinationIndex, AComponentCount);
          Inc(ADestinationIndex, AComponentCount);
        end;
        Inc(ASourceX, ASourceStride);
      end;
    end
    else
    begin
      ASourceX := 0;
      for Y := 0 to AHeight - 1 do
      begin
        AInterpolator.Start(ASourceX);
        for X := 0 to ADestinationWidth - 1 do
          AInterpolator.Interpolate(X);
        Inc(ASourceX, ASourceStride);
      end;
    end;
  finally
    AInterpolator.Free;
  end;
end;

class function TdxPDFImageInterpolator.VerticalInterpolation(const AData: TBytes; AWidth: Integer;
  ASourceHeight: Integer; ADestinationHeight: Integer): TBytes;
var
  Y, ADestinationIndex: Integer;
  AInterpolator: TdxPDFImageInterpolator;
begin
  SetLength(Result, AWidth * ADestinationHeight);
  if ASourceHeight = 1 then
  begin
    ADestinationIndex := 0;
    for Y := 0 to ADestinationHeight - 1 do
    begin
      TdxPDFUtils.CopyData(AData, 0, Result, ADestinationIndex, AWidth);
      Inc(ADestinationIndex, AWidth);
    end;
  end
  else
  begin
    AInterpolator := TdxPDFImageInterpolator.Create(AData, AWidth, ASourceHeight, ADestinationHeight, Result);
    try
      AInterpolator.Start(0);
      for Y := 0 to ADestinationHeight - 1 do
        AInterpolator.Interpolate(Y);
    finally
      AInterpolator.Free;
    end;
  end;
end;

procedure TdxPDFImageInterpolator.FillComponents(const ASourceComponents: TBytes; AIndex: Integer; var AComponents: TBytes);
begin
  TdxPDFUtils.CopyData(ASourceComponents, AIndex, AComponents, 0, FSize);
end;

procedure TdxPDFImageInterpolator.Start(APosition: Integer);
begin
  FArrayPosition := APosition;
  FillComponents(FData, FArrayPosition, FPrevious);
  FArrayPosition := FArrayPosition + FSize;
  FillComponents(FData, FArrayPosition, FNext);
  FArrayPosition := FArrayPosition + FSize;
  FSourcePosition := 0;
end;

procedure TdxPDFImageInterpolator.Interpolate(AValue: Integer);
var
  X: Integer;
  ATempData: TBytes;
  ARatio, ATemp: Double;
begin
  ARatio := AValue * FFactor - FSourcePosition;
  if ARatio >= 1 then
  begin
    ATempData := FPrevious;
    FPrevious := FNext;
    FNext := ATempData;
    Inc(FSourcePosition);
    if FSourcePosition < FLastSourcePosition then
    begin
      FillComponents(FData, FArrayPosition, FNext);
      FArrayPosition := FArrayPosition + FSize;
    end
    else
      FillComponents(FNext, 0, FPrevious);
    ARatio := ARatio - 1;
  end;
  for X := 0 to FSize - 1 do
  begin
    ATemp := FPrevious[X];
    FResult[FResultPosition] := TdxPDFUtils.ConvertToByte(ATemp + (FNext[X] - ATemp) * ARatio);
    Inc(FResultPosition);
  end;
end;

{ TdxPDFReaderObjectStream }

constructor TdxPDFReaderObjectStream.Create(ANumber: Integer; AStream: TdxPDFStream);
var
  I, AElementCount, ACurrentPosition: Integer;
  AData: TBytes;
  AParser: TdxPDFStructureParser;
begin
  inherited Create(ANumber, ANumber);
  AElementCount := AStream.Dictionary.GetInteger(TdxPDFKeywords.Count);
  ACurrentPosition := AStream.Dictionary.GetInteger('First');
  if TdxPDFUtils.IsIntegerValid(AElementCount) or TdxPDFUtils.IsIntegerValid(ACurrentPosition) and
    (AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey) = TdxPDFKeywords.ObjectStream) then
  begin
    FObjects := TdxPDFReferencedObjects.Create;
    AData := AStream.UncompressedData;
    AParser := TdxPDFStructureParser.Create(TdxPDFReaderDictionary(AStream.Dictionary).Repository);
    try
      for I := 0 to AElementCount - 1 do
      begin
        FObjects.Add(AParser.ReadObject(AData, ACurrentPosition));
        ACurrentPosition := TdxPDFBaseParserAccess(AParser).CurrentPosition;
      end;
    finally
      AParser.Free;
    end;
  end;
end;

destructor TdxPDFReaderObjectStream.Destroy;
begin
  FreeAndNil(FObjects);
  inherited Destroy;
end;

class function TdxPDFReaderObjectStream.GetObjectType: TdxPDFBaseType;
begin
  Result := otObjectStream;
end;

{ TdxPDFCommandList }

function TdxPDFCommandList.ToByteArray(AResources: TdxPDFResources): TBytes;
var
  I: Integer;
  AStream: TdxPDFWriterStream;
begin
  AStream := TdxPDFWriterStream.Create;
  try
    for I := 0 to Count - 1 do
      (Items[I] as TdxPDFCustomCommand).Write(AStream, AResources);
    Result := AStream.Data;
  finally
    AStream.Free;
  end;
end;

{ TdxPDFFontInfo }

function TdxPDFFontInfo.GetFontLineSize: Single;
begin
  Result := FontSize / 14.0;
end;

{ TdxPDFObject }

constructor TdxPDFObject.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  CreateSubClasses;
  Initialize;
end;

destructor TdxPDFObject.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

function TdxPDFObject.GetObject(const AName: string; ASourceDictionary: TdxPDFDictionary; out AObject: TdxPDFBase): Boolean;
begin
  AObject := ASourceDictionary.GetObject(AName);
  Result := AObject <> nil;
  if Result and TdxPDFUtils.IsIntegerValid((AObject as TdxPDFBase).Number) then
    AObject := Repository.GetObject((AObject as TdxPDFBase).Number) as TdxPDFBase;
end;

class function TdxPDFObject.GetTypeName: string;
begin
  Result := '';
end;

function TdxPDFObject.GetRepository: TdxPDFDocumentRepository;
begin
  Result := FRepository;
end;

procedure TdxPDFObject.CreateSubClasses;
begin
  Repository := nil;
end;

procedure TdxPDFObject.DestroySubClasses;
begin
  Repository := nil;
end;

procedure TdxPDFObject.Initialize;
begin
// do nothing
end;

procedure TdxPDFObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    Repository := ADictionary.Repository;
    Number := ADictionary.Number;
    ReadProperties(ADictionary);
  end;
end;

procedure TdxPDFObject.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
// do nothing
end;

procedure TdxPDFObject.SetOwner(const AValue: TObject);
begin
  FOwner := AValue;
end;

procedure TdxPDFObject.SetRepository(const AValue: TdxPDFDocumentRepository);
begin
  FRepository := AValue;
end;

{ TdxPDFPageData }

procedure TdxPDFPageData.ClearCommands;
begin
  FContents.ClearCommands;
end;

procedure TdxPDFPageData.ExtractCommands;
begin
  if FContents.CommandCount = 0 then
    FContents.PopulateCommands(Resources);
end;

class function TdxPDFPageData.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Page;
end;

procedure TdxPDFPageData.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FAnnotations := TdxPDFReferencedObjects.Create;
  FContents := TdxPDFPageContents.Create(Self);
end;

procedure TdxPDFPageData.DestroySubClasses;
begin
  Contents := nil;
  FreeAndNil(FTransparencyGroup);
  FreeAndNil(FAnnotations);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageData.Initialize;
begin
  inherited Initialize;
  FNeedReadAnnotations := True;
end;

procedure TdxPDFPageData.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  FDictionary := ADictionary;
  Contents.Read(ADictionary);
  ReadGroup;
end;

function TdxPDFPageData.GetAnnotations: TdxPDFReferencedObjects;
begin
  if FNeedReadAnnotations then
  begin
    ReadAnnotations;
    FNeedReadAnnotations := False;
  end;
  Result := FAnnotations;
end;

function TdxPDFPageData.GetCommands: TdxPDFReferencedObjects;
begin
  Result := Contents.Commands;
end;

function TdxPDFPageData.GetResources: TdxPDFResources;
begin
  Result := (Owner as TdxPDFPage).Resources;
end;

procedure TdxPDFPageData.SetContents(const AValue: TdxPDFPageContents);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FContents));
end;

procedure TdxPDFPageData.ReadAnnotations;
var
  I: Integer;
  AAnnotation: TdxPDFCustomAnnotation;
  AArray: TdxPDFArray;
  AObject: TdxPDFBase;
begin
  if FDictionary <> nil then
  begin
    AArray := FDictionary.GetArray(TdxPDFKeywords.Annotations);
    if AArray <> nil then
    begin
      for AObject in AArray.ElementList do
      begin
        AAnnotation := FDictionary.Repository.GetAnnotation(AObject.Number, Self.Owner as TdxPDFPage);
        if AAnnotation <> nil then
          FAnnotations.Add(AAnnotation as TdxPDFCustomAnnotation);
      end;
      for I := 0 to FAnnotations.Count - 1 do
        TdxPDFCustomAnnotation(FAnnotations[I]).Ensure;
    end;
  end;
end;

procedure TdxPDFPageData.ReadGroup;
var
  AGroupDictionary: TdxPDFReaderDictionary;
begin
  AGroupDictionary := FDictionary.GetDictionary(TdxPDFKeywords.Group);
  if AGroupDictionary <> nil then
  begin
    FTransparencyGroup := TdxPDFTransparencyGroup.Create(nil);
    FTransparencyGroup.Read(AGroupDictionary);
  end;
end;

{ TdxPDFPage }

constructor TdxPDFPage.Create(AOwner: TdxPDFObject; AInfo: TdxPDFDeferredObjectInfo);
begin
  inherited Create(AOwner);
  FDeferredData := TdxPDFDeferredObject.Create(Self, AInfo);
end;

constructor TdxPDFPage.Create(APages: TdxPDFPages; AMediaBox, ACropBox: TdxRectF; ARotationAngle: Integer);
begin
  inherited Create(APages);
  FMediaBox := AMediaBox;
  FCropBox := ACropBox;
  FRotationAngle := ARotationAngle;
  FDeferredData := TdxPDFDeferredObject.Create(Self, TdxPDFPageData.Create(Self));
end;

destructor TdxPDFPage.Destroy;
begin
  Pack;
  FreeAndNil(FDeferredData);
  inherited Destroy;
end;

function TdxPDFPage.CalculateRotationAngle(ARotationAngle: TcxRotationAngle): Integer;
begin
  Result := TdxPDFUtils.NormalizeRotate(RotationAngle - TdxPDFUtils.ConvertToIntEx(ARotationAngle));
end;

function TdxPDFPage.Find(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF): TdxPDFRecognizedObject;
var
  AField: TdxPDFAcroFormField;
  AResult: TdxPDFRecognizedObject;
  ADocumentPosition: TdxPDFPosition;
  ADocumentScaleFactor: TdxPointF;
begin
  ADocumentPosition := APosition;
  ADocumentScaleFactor := AScaleFactor;
  LockAndExecute(
    procedure
    var
      ALine: TdxPDFTextLine;
    begin
      if FindInteractiveObject(ADocumentPosition.Point, ADocumentScaleFactor, AField) then
        AResult := AField
      else
        if FindLine(ADocumentPosition, ADocumentScaleFactor, ALine) then
          AResult := ALine
        else
          AResult := FindImage(ADocumentPosition.Point);
    end, True);
  Result := AResult;
end;

function TdxPDFPage.FindHyperlink(const P: TdxPointF; const AScaleFactor: TdxPointF; out AHyperlink: TdxPDFHyperlink): Boolean;
var
  ADocumentHyperlink: TdxPDFHyperlink;
  ADocumentPosition: TdxPointF;
  ADocumentScaleFactor: TdxPointF;
begin
  ADocumentPosition := P;
  ADocumentScaleFactor := AScaleFactor;
  LockAndExecute(
    procedure
    var
      ALink: TdxPDFHyperlink;
      ATextExpansionFactor: TdxPointF;
    begin
      ADocumentHyperlink := nil;
      ATextExpansionFactor := GetTextExpansionFactor(ADocumentScaleFactor);
      for ALink in RecognizedContent.Hyperlinks do
        if ALink.Bounds.PtInRect(ADocumentPosition, ATextExpansionFactor.X) then
        begin
          ADocumentHyperlink := ALink;
          Break;
        end;
    end, True);
  AHyperlink := ADocumentHyperlink;
  Result := AHyperlink <> nil;
end;

function TdxPDFPage.FindImage(const P: TdxPointF): TdxPDFImage;
var
  AImage: TdxPDFImage;
begin
  Result := nil;
  for AImage in RecognizedContent.Images do
    if PtInRect(cxRect(TdxPDFImageAccess(AImage).Bounds), cxPoint(P)) then
      Exit(AImage);
end;

function TdxPDFPage.FindInteractiveObject(const P: TdxPointF; const AScaleFactor: TdxPointF;
  out AField: TdxPDFAcroFormField): Boolean;
var
  AHyperlink: TdxPDFHyperlink;
  AFormField: TdxPDFAcroFormField;
  ADocumentPosition: TdxPointF;
  ADocumentScaleFactor: TdxPointF;
begin
  Result := FindHyperlink(P, AScaleFactor, AHyperlink);
  if not Result then
  begin
    ADocumentPosition := P;
    ADocumentScaleFactor := AScaleFactor;
    LockAndExecute(
      procedure

        function CheckField(const APosition, AFactor: TdxPointF; F: TdxPDFAcroFormField; out AResult: TdxPDFAcroFormField): Boolean;
        begin
          Result := F.Bounds.PtInRect(APosition, AFactor.X) and Supports(F, IdxPDFInteractiveObject);
          if Result then
            AResult := F
          else
            AResult := nil;
        end;

      var
        F: TdxPDFAcroFormField;
        ATextExpansionFactor: TdxPointF;
      begin
        AFormField := nil;
        ATextExpansionFactor := GetTextExpansionFactor(ADocumentScaleFactor);
        for F in RecognizedContent.AcroFormFields do
          if CheckField(ADocumentPosition, ATextExpansionFactor, F, AFormField) then
            Exit;
        for F in RecognizedContent.AnnotationFields do
          if CheckField(ADocumentPosition, ATextExpansionFactor, F, AFormField) then
            Exit;
      end, True);
    AField := AFormField;
    Result := AField <> nil;
  end
  else
    AField := AHyperlink;
end;

function TdxPDFPage.FindLine(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF;
  out ALine: TdxPDFTextLine): Boolean;
begin
  Result := RecognizedContent.TextLines.Find(APosition, GetTextExpansionFactor(AScaleFactor), ALine);
end;

function TdxPDFPage.FindStartTextPosition(const APosition: TdxPDFPosition; const AScaleFactor: TdxPointF): TdxPDFTextPosition;
var
  ALine: TdxPDFTextLine;
begin
  if FindLine(APosition, AScaleFactor, ALine) then
    Result := TdxPDFTextLineAccess(ALine).GetPosition(APosition.PageIndex, APosition.Point)
  else
    Result := TdxPDFTextPosition.Invalid;
end;

function TdxPDFPage.GetTopLeft(AAngle: TcxRotationAngle): TdxPointF;
var
  ACropBox: TdxRectF;
begin
  ACropBox := CropBox;
  case CalculateRotationAngle(AAngle) of
    90:
      Result := dxPointF(ACropBox.Left, ACropBox.Bottom);
    180:
      Result := ACropBox.BottomRight;
    270:
      Result := dxPointF(ACropBox.Right, ACropBox.Top);
  else
    Result := ACropBox.TopLeft;
  end;
end;

function TdxPDFPage.FromUserSpace(const P, ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
  AAngle: TcxRotationAngle): TdxPointF;

  function Convert(const P, ADPI: TdxPointF; AAngle: TcxRotationAngle): TdxPointF;
  var
    AUserScapeFactor: TdxPointF;
  begin
    AUserScapeFactor := UserSpaceFactor(ADPI);
    case CalculateRotationAngle(AAngle) of
      90:
        begin
          Result.X := P.Y / AUserScapeFactor.Y;
          Result.Y := P.X / AUserScapeFactor.X;
        end;
      180:
        begin
          Result.X := Abs(Bounds.Width) - P.X / AUserScapeFactor.X;
          Result.Y := P.Y / AUserScapeFactor.Y;
        end;
      270:
        begin
          Result.X := Abs(Bounds.Width)  - P.Y / AUserScapeFactor.Y;
          Result.Y := Abs(Bounds.Height) -  P.X / AUserScapeFactor.X;
        end;
    else
       Result.X := P.X / AUserScapeFactor.X;
       Result.Y := Abs(Bounds.Height) - P.Y / AUserScapeFactor.Y;
    end;
  end;

var
  AMousePoint, AUserScapeFactor: TdxPointF;
begin
  AMousePoint := P;
  AUserScapeFactor := UserSpaceFactor(ADPI);
  AMousePoint.X := (AMousePoint.X - ABounds.Left) / (AScaleFactor.X);
  AMousePoint.Y := (AMousePoint.Y - ABounds.Top) / (AScaleFactor.Y);
  Result := Convert(AMousePoint, ADPI, AAngle);
end;

function TdxPDFPage.FromUserSpace(const R: TdxRectF; ADPI, AScaleFactor: TdxPointF;
  const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxRectF;
begin
  Result.TopLeft := FromUserSpace(R.TopLeft, ADPI, AScaleFactor, ABounds, AAngle);
  Result.BottomRight := FromUserSpace(R.BottomRight, ADPI, AScaleFactor, ABounds, AAngle);
end;

function TdxPDFPage.ToUserSpace(const R: TdxRectF; const ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
  AAngle: TcxRotationAngle): TdxRectF;
begin
  Result.TopLeft := ToUserSpace(R.TopLeft, ADPI, AScaleFactor, ABounds, AAngle);
  Result.BottomRight := ToUserSpace(R.BottomRight, ADPI, AScaleFactor, ABounds, AAngle);
  Result := cxRectAdjustF(Result);
end;

procedure TdxPDFPage.Pack;
begin
  PackData;
  PackRecognizedContent;
end;

procedure TdxPDFPage.PackRecognizedContent;
begin
  LockAndExecute(
    procedure
    begin
      FreeAndNil(FRecognizedContent);
    end);
end;

function TdxPDFPage.ToUserSpace(const P, ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
  AAngle: TcxRotationAngle): TdxPointF;
var
  ARealScaleFactor, AUserScapeFactor: TdxPointF;
begin
  AUserScapeFactor := UserSpaceFactor(ADPI);
  ARealScaleFactor.X := AUserScapeFactor.X * AScaleFactor.X;
  ARealScaleFactor.Y := AUserScapeFactor.Y * AScaleFactor.y;
  case CalculateRotationAngle(AAngle) of
    90:
      begin
        Result.X := P.Y * ARealScaleFactor.X;
        Result.Y := P.X * ARealScaleFactor.Y;
      end;
    180:
      begin
        Result.X := (Abs(Bounds.Width) - P.X) * ARealScaleFactor.X;
        Result.Y := P.Y * ARealScaleFactor.Y;
      end;
    270:
      begin
        Result.X := (Abs(Bounds.Height) - P.Y) * ARealScaleFactor.X;
        Result.Y := (Abs(Bounds.Width) - P.X) * ARealScaleFactor.Y;
      end;
  else
     Result.X := P.X * ARealScaleFactor.X;
     Result.Y := (Abs(Bounds.Height) - P.Y) * ARealScaleFactor.Y;
  end;

  Result := cxPointOffset(Result, ABounds.TopLeft);
end;

class function TdxPDFPage.GetTypeName: string;
begin
  Result := 'FakePage';
end;

procedure TdxPDFPage.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFPage.DestroySubClasses;
begin
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

function TdxPDFPage.ScaleFactor(const ADPI, AScaleFactor: TdxPointF): TdxPointF;
var
  ASpaceFactor: TdxPointF;
begin
  ASpaceFactor := UserSpaceFactor(ADPI);
  Result.X := ASpaceFactor.X * AScaleFactor.X;
  Result.Y := ASpaceFactor.Y * AScaleFactor.Y;
end;

function TdxPDFPage.UserSpaceFactor(const ADPI: TdxPointF): TdxPointF;
begin
  Result := dxPointF(ADPI.X / 72 * UserUnit, ADPI.Y / 72 * UserUnit);
end;

procedure TdxPDFPage.Export(ADevice: TObject; AParameters: TdxPDFExportParameters);
begin
  LockAndExecute(
    procedure
    begin
      (ADevice as TdxPDFCustomCommandInterpreter).ExportAndPack(Self, AParameters);
    end);
end;

procedure TdxPDFPage.Export(AParameters: TdxPDFExportParameters; AStream: TStream);
var
  ABitmap: TcxBitmap;
  ADevice: TdxPDFGraphicsDevice;
  ARenderParameters: TdxPDFRenderParameters;
begin
  ARenderParameters := AParameters as TdxPDFRenderParameters;
  if not ARenderParameters.IsCanceled then
  begin
    ABitmap := TcxBitmap.CreateSize(ARenderParameters.Rect);
    try
      ABitmap.PixelFormat := pf24bit;
      ARenderParameters.Canvas := ABitmap.Canvas;
      ARenderParameters.Canvas.Lock;
      ADevice := TdxPDFGraphicsDevice.Create;
      try
        ADevice.Export(Self, ARenderParameters);
        ABitmap.SaveToStream(AStream);
      finally
        ARenderParameters.Canvas.Unlock;
        ADevice.Free;
      end;
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TdxPDFPage.LockAndExecute(AProc: TProc; ATryLock: Boolean = False);
var
  ALocked: Boolean;
begin
  ALocked := True;
  if ATryLock then
    ALocked := TryEnterCriticalSection(FLock)
  else
    EnterCriticalSection(FLock);
  if ALocked then
    try
      Locked := True;
      AProc;
    finally
      LeaveCriticalSection(FLock);
      Locked := False;
    end;
end;

procedure TdxPDFPage.PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
begin
  LockAndExecute(
    procedure
    var
      AContent: TdxPDFRecognizedContent;
    begin
      AContent := TdxPDFRecognizedContent.Create;
      try
        RecognizeContent(AContent, rmAttachment);
      finally
        AContent.Free;
      end;
    end, True);
end;

function TdxPDFPage.GetBounds: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(CropBox) then
    Result := MediaBox
  else
    Result := CropBox;
end;

function TdxPDFPage.GetData: TdxPDFPageData;
begin
  Result := FDeferredData.ResolvedObject as TdxPDFPageData
end;

function TdxPDFPage.GetDocumentState: TdxPDFDocumentState;
begin
  Result := TdxPDFDocumentAccess(Repository.Catalog.Owner as TdxPDFDocument).State;
end;

function TdxPDFPage.GetNormalizedRotationAngle: Integer;
begin
  Result := TdxPDFUtils.NormalizeRotate(RotationAngle);
end;

function TdxPDFPage.GetRecognizedContent: TdxPDFRecognizedContent;
begin
  if FRecognizedContent = nil then
  begin
    FRecognizedContent := TdxPDFRecognizedContent.Create;
    RecognizeContent(FRecognizedContent, rmAll);
  end;
  Result := FRecognizedContent;
end;

function TdxPDFPage.GetSize: TdxPointF;
var
  ABounds: TdxRectF;
begin
  ABounds := Bounds;
  if (NormalizedRotationAngle = 90) or (NormalizedRotationAngle = 270) then
    Result := dxPointF(Abs(ABounds.Height), Abs(ABounds.Width))
  else
    Result := dxPointF(Abs(ABounds.Width), Abs(ABounds.Height));
end;

procedure TdxPDFPage.SetData(const AValue: TdxPDFPageData);
begin
  FDeferredData.ResolvedObject := nil;
end;

function TdxPDFPage.GetTextExpansionFactor(const AScaleFactor: TdxPointF): TdxPointF;
begin
  Result.X := 15 / AScaleFactor.X;
  Result.Y := 5 / AScaleFactor.Y;
end;

procedure TdxPDFPage.PackData;
begin
  LockAndExecute(
    procedure
    begin
      if FDeferredData.IsResolved then
        Data := nil;
      if Resources <> nil then
        Resources.Pack;
    end);
end;

procedure TdxPDFPage.RecognizeContent(AContent: TdxPDFRecognizedContent; AMode: TRecognizeMode);

  function CreateRenderParameters: TdxPDFRenderParameters;
  begin
    Result := TdxPDFRenderParameters.Create(GetDocumentState);
    Result.Angle := ra0;
    Result.ScaleFactor := cxGetCurrentDPI / 72;
  end;

  function CreateRecognizer: TdxPDFDocumentContentRecognizer;
  begin
    if AMode = rmAttachment then
      Result := TdxPDFAttachmentUnpacker.Create(AContent)
    else
      Result := TdxPDFDocumentContentRecognizer.Create(AContent);
  end;

var
  AParameters: TdxPDFRenderParameters;
  ARecognizer: TdxPDFDocumentContentRecognizer;
begin
  if AContent <> nil then
  begin
    AParameters := CreateRenderParameters;
    ARecognizer := CreateRecognizer;
    try
      LockAndExecute(
        procedure
        begin
          ARecognizer.ExportAndPack(Self, AParameters);
        end, True);
    finally
      ARecognizer.Free;
      AParameters.Free;
    end;
  end;
end;

{ TdxPDFInteractiveFormFieldTextState }

procedure TdxPDFInteractiveFormFieldTextState.DestroySubClasses;
begin
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormFieldTextState.Initialize;
begin
  inherited Initialize;
  FHorizontalScaling := 100;
end;

constructor TdxPDFInteractiveFormFieldTextState.Create(AField: TdxPDFInteractiveFormField);
var
  AAppearance: TdxPDFAnnotationAppearances;
  AWidgetCommands: TdxPDFReferencedObjects;
begin
  inherited Create(nil);
  ConvertCommandsToBytes(AField);
  if (FFontCommand = nil) or (TdxPDFSetTextFontCommand(FFontCommand).Font = nil) then
  begin
    if AField.Widget = nil then
      AAppearance := nil
    else
      AAppearance := AField.Widget.Appearance;
    if (AAppearance = nil) or (AAppearance.Normal = nil) or (AAppearance.Normal.DefaultForm = nil) then
      AWidgetCommands := nil
    else
      AWidgetCommands := AAppearance.Normal.DefaultForm.GetCommands;
    if AWidgetCommands <> nil then
      FFontCommand := FindSetTextFontCommand(AWidgetCommands);
  end;
end;

function TdxPDFInteractiveFormFieldTextState.GetFontSize: Double;
begin
  if FFontCommand = nil then
    Result := DefaultFontSize
  else
    Result := (FFontCommand as TdxPDFSetTextFontCommand).FontSize;
end;

function TdxPDFInteractiveFormFieldTextState.FindSetTextFontCommand(ACommands: TdxPDFReferencedObjects): TdxPDFCustomCommand;
var
  ACommandObject: TdxPDFReferencedObject;
  AFontCommand: TdxPDFCustomCommand;
begin
  Result := nil;
  for ACommandObject in ACommands do
    if ACommandObject is TdxPDFSetTextFontCommand then
      Exit(TdxPDFSetTextFontCommand(ACommandObject))
    else
      if ACommandObject is TdxPDFMarkedContentCommand then
      begin
        AFontCommand := FindSetTextFontCommand(TdxPDFMarkedContentCommand(ACommandObject).Commands);
        if AFontCommand <> nil then
          Exit(AFontCommand);
      end;
end;

procedure TdxPDFInteractiveFormFieldTextState.ConvertCommandsToBytes(AField: TdxPDFInteractiveFormField);
var
  ACommands: TdxPDFReferencedObjects;
  ACommandsToFillList: TdxPDFCommandList;
  AAppearanceCommand: TdxPDFReferencedObject;
  AResources: TdxPDFResources;
begin
  ACommands := GetAppearanceCommandsInheritable(AField);
  if ACommands <> nil then
  begin
    ACommandsToFillList := TdxPDFCommandList.Create;
    try
      for AAppearanceCommand in ACommands do
        if AAppearanceCommand is TdxPDFSetWordSpacingCommand then
          FWordSpacing := TdxPDFSetWordSpacingCommand(AAppearanceCommand).Spacing
        else
          if AAppearanceCommand is TdxPDFSetCharacterSpacingCommand then
            FCharacterSpacing := TdxPDFSetCharacterSpacingCommand(AAppearanceCommand).Spacing
          else
            if AAppearanceCommand is TdxPDFSetTextHorizontalScalingCommand then
              FHorizontalScaling := TdxPDFSetTextHorizontalScalingCommand(AAppearanceCommand).HorizontalScaling
            else
              if AAppearanceCommand is TdxPDFSetTextFontCommand then
                FFontCommand := TdxPDFSetTextFontCommand(AAppearanceCommand)
              else
                ACommandsToFillList.Add(AAppearanceCommand);
      AResources := TdxPDFResources.Create(nil);
      try
        FCommandsAsBytes := ACommandsToFillList.ToByteArray(AResources);
      finally
        dxPDFFreeObject(AResources);
      end;
    finally
      ACommandsToFillList.Free;
    end;
  end;
end;

function TdxPDFInteractiveFormFieldTextState.GetAppearanceCommandsInheritable(AFormField: TdxPDFInteractiveFormField): TdxPDFReferencedObjects;
begin
  Result := nil;
  if AFormField <> nil then
    if AFormField.AppearanceCommands <> nil then
      Result := AFormField.AppearanceCommands
    else
      if (AFormField.Parent = nil) and (AFormField.Form <> nil) then
        Result := AFormField.Form.DefaultAppearanceCommands
      else
        Result := GetAppearanceCommandsInheritable(AFormField.Parent);
end;

{ TdxPDFInteractiveFormField }

class function TdxPDFInteractiveFormField.Parse(AForm: TdxPDFInteractiveForm;
  AParent: TdxPDFInteractiveFormField; ADictionary: TdxPDFReaderDictionary;
  ANumber: Integer): TdxPDFInteractiveFormField;

  function GetFiledType: string;
  begin
    Result := ADictionary.GetString('FT');
    if (Result = '') and (AParent <> nil) then
      Result := AParent.GetTypeName;
  end;

var
  AType: string;
  AClass: TdxPDFObjectClass;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AType := GetFiledType;
    if AType = '' then
    begin
      Result := nil;
      if ADictionary.Contains(TdxPDFKeywords.Kids) then
        Result := TdxPDFInteractiveFormField.Create(nil);
    end
    else
    begin
      AClass := dxPDFGetDocumentObjectClass(AType);
      if AClass <> nil then
        Result := AClass.Create(nil) as TdxPDFInteractiveFormField;
    end;
  end;
  if Result <> nil then
    Result.Read(AForm, AParent, ADictionary, ANumber);
end;

procedure TdxPDFInteractiveFormField.CreateSubClasses;
begin
  inherited CreateSubClasses;
end;

procedure TdxPDFInteractiveFormField.DestroySubClasses;
begin
  FWidget := nil;
  FParent := nil;
  FreeAndNil(FAppearanceCommands);
  FreeAndNil(FKids);
  FreeAndNil(FTextState);
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormField.Read(AForm: TdxPDFInteractiveForm;
  AParent: TdxPDFInteractiveFormField; ADictionary: TdxPDFReaderDictionary; ANumber: Integer);
var
  AResources: TdxPDFResources;
begin
  FParent := AParent;
  FForm := AForm;
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    ReadKids(ADictionary, ANumber);
    if (FName = '') and (FParent <> nil) then
      FValuesProvider := FParent
    else
      FValuesProvider := Self;
    if AForm <> nil then
      AResources := AForm.Resources
    else
      AResources := TdxPDFResources.Create(nil);
    FAppearanceCommands := ADictionary.GetAppearance(AResources);
  end;
end;

procedure TdxPDFInteractiveFormField.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  if ADictionary <> nil then
  begin
    FName := ADictionary.GetTextString('T');
    FAlternateName := ADictionary.GetTextString('TU');
    FMappingName := ADictionary.GetTextString('TM');
    if ADictionary.Contains('Ff') then
      FFlags := TdxPDFInteractiveFormFieldFlags(ADictionary.GetInteger('Ff'));
    FTextJustification := TdxPDFTextJustification(ADictionary.GetInteger('Q', 0));
  end;
end;

function TdxPDFInteractiveFormField.CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject;
begin
  Result := nil;
end;

function TdxPDFInteractiveFormField.GetAcroFieldClass: TdxPDFAcroFormFieldClass;
begin
  Result := nil;
end;

procedure TdxPDFInteractiveFormField.InternalSetValue(const AValue: Variant; AState: TdxPDFDocumentState);
begin
// do nothing
end;

function TdxPDFInteractiveFormField.CreateAcroField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
var
  AClass: TdxPDFAcroFormFieldClass;
begin
  AClass := GetAcroFieldClass;
  if AClass <> nil then
  begin
    Result := AClass.Create;
    Result.DocumentState := AState;
    Result.Field := Self;
  end
  else
    Result := nil;
end;

procedure TdxPDFInteractiveFormField.Changed(AState: TdxPDFDocumentState);
var
  ABuilder: TObject;
  AAppearanceForm: TdxPDFForm;
  AIntf: IdxPDFAnnotationAppearanceBuilder;
begin
  if Form <> nil then
  begin
    if Widget <> nil then
    begin
      ABuilder := CreateAppearanceBuilder(AState);
      if Supports(ABuilder, IdxPDFAnnotationAppearanceBuilder, AIntf) then
      begin
        if FFormCreated then
          AAppearanceForm := Widget.GetAppearanceForm(AState)
        else
        begin
          AAppearanceForm := Widget.CreateAppearanceForm(Widget.AppearanceName);
          FFormCreated := True;
        end;
        AIntf.RebuildAppearance(AAppearanceForm);
        AIntf := nil;
      end
      else
        ABuilder.Free;
    end;
    if Assigned(FOnValueChanged) then
      OnValueChanged(Self);
  end;
end;

function TdxPDFInteractiveFormField.GetFontInfo(AState: TdxPDFDocumentState): TdxPDFFontInfo;
var
  AFontData: TdxPDFEditableFontData;
  AFontSize: Single;
begin
  AFontData := AState.SearchFontData(TextState.FontCommand) as TdxPDFEditableFontData;
  AFontSize := TextState.FontSize;
  if SameValue(AFontSize, 0) then
  begin

  end;
  Result.FontSize := AFontSize;
  Result.FontData := AFontData;
end;

function TdxPDFInteractiveFormField.GetValue: Variant;
begin
  Result := '';
end;

function TdxPDFInteractiveFormField.UseDefaultAppearanceForm: Boolean;
begin
  Result := True;
end;

procedure TdxPDFInteractiveFormField.SetValue(const AValue: Variant; AState: TdxPDFDocumentState);
begin
  FValuesProvider.InternalSetValue(AValue, AState);
end;

function TdxPDFInteractiveFormField.GetFullName: string;
var
  AParentName: string;
begin
  Result := FName;
  if FParent <> nil then
  begin
    AParentName := Parent.FullName;
    if AParentName = '' then
      Result := FName
    else
      Result := Result + AParentName;
      if FName <> '' then
        Result := Result + '.' + FName;
  end;
end;

function TdxPDFInteractiveFormField.GetTextState: TdxPDFInteractiveFormFieldTextState;
begin
  if FTextState = nil then
    FTextState := TdxPDFInteractiveFormFieldTextState.Create(Self);
  Result := FTextState;
end;

procedure TdxPDFInteractiveFormField.ReadKids(ADictionary: TdxPDFReaderDictionary; AWidgetNumber: Integer);

  function GetPage(ADictionary: TdxPDFReaderDictionary): TdxPDFPage;
  var
    ANumber: Integer;
  begin
    if ADictionary.TryGetReference('P', ANumber) then
      Result := Repository.GetPage(ANumber)
    else
      Result := nil;
  end;

var
  AKidArray: TdxPDFArray;
begin
  AKidArray := ADictionary.GetArray(TdxPDFKeywords.Kids);
  if AKidArray = nil then
  begin
    FWidget := Repository.GetWidget(AWidgetNumber) as TdxPDFWidgetAnnotation;
    if FWidget = nil then
      FWidget := Repository.GetAnnotation(AWidgetNumber, GetPage(ADictionary)) as TdxPDFWidgetAnnotation
    else
      (FWidget as TdxPDFWidgetAnnotation).InteractiveFormField := Self;
  end
  else
  begin
    FKids := TdxPDFInteractiveFormFieldCollection.Create(nil);
    FKids.Read(ADictionary, AKidArray, FForm, Self);
  end;
end;

{ TdxPDFInteractiveFormFieldCollection }

procedure TdxPDFInteractiveFormFieldCollection.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FItems := TdxPDFReferencedObjects.Create;
end;

procedure TdxPDFInteractiveFormFieldCollection.DestroySubClasses;
begin
  FreeAndNil(FItems);
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormFieldCollection.Read(ADictionary: TdxPDFReaderDictionary; AFieldArray: TdxPDFArray;
  AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField);
var
  I: Integer;
  AField: TdxPDFInteractiveFormField;
begin
  inherited Read(ADictionary);
  if AFieldArray <> nil then
    for I := 0 to AFieldArray.Count - 1 do
    begin
      AField := Repository.GetInteractiveFormField(AForm, AParent, AFieldArray.ElementList[I]);
      if AField <> nil then
      begin
        FItems.Add(AField);
        AField.OnValueChanged := OnInteractiveFormFieldValueChanged;
      end;
    end;
end;

{ TdxPDFXFAForm }

procedure TdxPDFXFAForm.Initialize(const AData: TBytes);
begin
  Content := Content + TdxPDFUtils.ConvertToUTF8String(AData);
end;

procedure TdxPDFXFAForm.Initialize(ARepository: TdxPDFDocumentRepository; AArray: TdxPDFArray);
var
  AStream: TdxPDFStream;
  I, AIndex: Integer;
begin
  if AArray <> nil then
  begin
    if (AArray.Count mod 2 <> 0) then
      TdxPDFUtils.RaiseException;
    Content := '';
    AIndex := 0;
    for I := 0 to (AArray.Count div 2) - 1 do
    begin
      if not (AArray[AIndex] is TdxPDFString) then
        TdxPDFUtils.RaiseException;
      Inc(AIndex);
      if AArray[AIndex].ObjectType <> otIndirectReference then
        TdxPDFUtils.RaiseException;
      AStream := ARepository.GetStream(AArray[AIndex].Number);
      Inc(AIndex);
      if AStream = nil then
        TdxPDFUtils.RaiseException;
      if I > 0 then
        Content := Content + '\n';
      Initialize(AStream.UncompressedData);
    end;
  end;
end;

{ TdxPDFInteractiveForm }

procedure TdxPDFInteractiveForm.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Resources := nil;
end;

procedure TdxPDFInteractiveForm.DestroySubClasses;
begin
  FreeAndNil(FFields);
  FreeAndNil(FDefaultAppearanceCommands);
  Resources := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveForm.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    Resources := ADictionary.GetResources('DR');
    FNeedAppearances := ADictionary.GetBoolean('NeedAppearances');
    FSignatureFlags := TdxPDFSignatureFlags(ADictionary.GetInteger('SigFlags', 0));
    FDefaultAppearanceCommandsData := ADictionary.GetBytes(TdxPDFKeywords.DictionaryAppearance);
    FDefaultAppearanceCommands := ADictionary.GetAppearance(Resources);
    FDefaultTextJustification := ADictionary.GetTextJustification;
    ReadXFAForm(ADictionary);
    FFields := TdxPDFInteractiveFormFieldCollection.Create(nil);
    FFields.OnInteractiveFormFieldValueChanged := OnInteractiveFormFieldValueChanged;
    FFields.Read(ADictionary, ADictionary.GetArray('Fields'), Self, nil);
  end;
end;

procedure TdxPDFInteractiveForm.SetResources(const AValue: TdxPDFResources);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources));
end;

procedure TdxPDFInteractiveForm.ReadXFAForm(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  AStream: TdxPDFStream;
begin
  if ADictionary.TryGetArray('XFA', AArray) then
    FXFAForm.Initialize(Repository, AArray)
  else
    if ADictionary.TryGetStream('XFA', AStream) then
      FXFAForm.Initialize(AStream.UncompressedData);
end;

{ TdxPDFCatalog }

class function TdxPDFCatalog.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Catalog;
end;

procedure TdxPDFCatalog.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPages := TdxPDFPages.Create(Self);
end;

procedure TdxPDFCatalog.DestroySubClasses;
begin
  FreeAndNil(FOutlines);
  FreeAndNil(FOpenDestination);
  FreeAndNil(FOpenAction);
  FreeAndNil(FNames);
  FreeAndNil(FPages);
  FreeAndNil(FAcroForm);
  inherited DestroySubClasses;
end;

procedure TdxPDFCatalog.Initialize;
begin
  inherited Initialize;
  FAcroForm := nil;
  FNeedReadNames := True;
  FNeedReadOutlines := True;
  FNames := nil;
  FOpenAction := nil;
  FOpenDestination := nil;
end;

procedure TdxPDFCatalog.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FDictionary := ADictionary;
    ReadPages;
    ReadInteractiveObjects;
  end
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFCatalog.SetRepository(const AValue: TdxPDFDocumentRepository);
begin
  inherited SetRepository(AValue);
  if Repository <> nil then
    Repository.Catalog := Self;
end;

function TdxPDFCatalog.GetEmbeddedFileSpecification(const AName: string): TdxPDFFileSpecification;
begin
  if Names <> nil then
    Result := Names.GetEmbeddedFileSpecification(AName)
  else
    Result := nil;
end;

function TdxPDFCatalog.GetDestination(const AName: string): TdxPDFCustomDestination;
begin
  if Names <> nil then
    Result := Names.GetPageDestination(AName)
  else
    Result := nil;
end;

procedure TdxPDFCatalog.PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
var
  I: Integer;
begin
  AList.Clear;
  if Names <> nil then
    Names.PopulateAttachmentList(AList);
  for I := 0 to Pages.Count - 1 do
    Pages[I].PopulateAttachmentList(AList);
end;

procedure TdxPDFCatalog.SetPages(const AValue: TdxPDFPages);
begin
  FreeAndNil(FPages);
  FPages := AValue;
end;

procedure TdxPDFCatalog.ReadAcroForm;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := FDictionary.GetDictionary(TdxPDFKeywords.AcroForm);
  if ADictionary <> nil then
  begin
    FAcroForm := TdxPDFInteractiveForm.Create(nil);
    FAcroForm.OnInteractiveFormFieldValueChanged := OnInteractiveFormFieldValueChanged;
    FAcroForm.Read(ADictionary);
  end;
end;

function TdxPDFCatalog.GetNames: TdxPDFNames;
begin
  ReadNames(FDictionary.GetDictionary(TdxPDFKeywords.Names));
  Result := FNames;
end;

function TdxPDFCatalog.GetOutlines: TdxPDFObject;
begin
  ReadOutlines(FDictionary.GetDictionary(TdxPDFKeywords.Outlines));
  Result := FOutlines;
end;

procedure TdxPDFCatalog.ReadInteractiveObjects;
var
  AObject: TdxPDFBase;
begin
  AObject := FDictionary.GetObject(TdxPDFKeywords.OpenAction);
  if AObject <> nil then
  begin
    if AObject.ObjectType <> otArray then
      FOpenAction := FDictionary.GetAction(TdxPDFKeywords.OpenAction)
    else
    begin
      FOpenDestination := Repository.GetDestination(AObject);
      if FOpenDestination <> nil then
        FOpenDestination.ResolveInternalPage;
    end;
  end;
  ReadAcroForm;
end;

procedure TdxPDFCatalog.ReadNames(ADictionary: TdxPDFReaderDictionary);
begin
  if FNeedReadNames and (ADictionary <> nil) then
  begin
    FNames := TdxPDFNames.Create(Self);
    FNames.Read(ADictionary);
    FNeedReadNames := False;
  end;
end;

procedure TdxPDFCatalog.ReadOutlines(ADictionary: TdxPDFReaderDictionary);
begin
  if FNeedReadOutlines and (ADictionary <> nil) then
  begin
    FOutlines := TdxPDFOutlines.Create(Self);
    FOutlines.Read(ADictionary);
    FNeedReadOutlines := False;
  end;
end;

procedure TdxPDFCatalog.ReadPages;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := FDictionary.GetDictionary(TdxPDFKeywords.Pages);
  if ADictionary <> nil then
  begin
    Pages.Clear;
    Pages.Read(ADictionary);
  end
  else
    TdxPDFUtils.Abort;
end;

{ TdxPDFCustomFont }

function TdxPDFFonts.GetFont(const AName: string): TdxPDFCustomFont;
begin
  Result := GetObject(AName) as TdxPDFCustomFont;
end;

class function TdxPDFFonts.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Font;
end;

class function TdxPDFFonts.GetTypePrefix: string;
begin
  Result := 'F';
end;

procedure TdxPDFFonts.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

{ TdxPDFGraphicsStateParametersList }

function TdxPDFGraphicsStateParametersList.GetParameters(const AName: string): TdxPDFGraphicsStateParameters;
begin
  Result := GetObject(AName) as TdxPDFGraphicsStateParameters;
end;

procedure TdxPDFGraphicsStateParametersList.Add(const AName: string; AStateParameters: TdxPDFGraphicsStateParameters);
begin
  InternalAdd(AName, AStateParameters);
end;

class function TdxPDFGraphicsStateParametersList.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ExtGState;
end;

class function TdxPDFGraphicsStateParametersList.GetTypePrefix: string;
begin
  Result := 'P';
end;

function TdxPDFGraphicsStateParametersList.GetTypeDictionaryKey: string;
begin
  Result := TdxPDFKeywords.TypeKey;
end;

procedure TdxPDFGraphicsStateParametersList.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

{ TdxPDFPageContentItem }

class function TdxPDFPageContentItem.GetTypeName: string;
begin
  Result := '';
end;

{ TdxPDFXObject }

class function TdxPDFXObject.Parse(ARepository: TdxPDFCustomRepository; AStream: TdxPDFStream;
  const ASubtype: string = ''): TdxPDFXObject;
var
  AType, ADefaultSubtype: string;
begin
  AType := AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey);
  ADefaultSubtype := AStream.Dictionary.GetString(TdxPDFKeywords.Subtype);
  if ADefaultSubtype = '' then
    ADefaultSubtype := ASubtype;

  if ((AType <> '') and (AType <> TdxPDFKeywords.XObject) and (AType <> TdxPDFKeywords.XObject2)) or (ASubType = '') then
    TdxPDFUtils.RaiseTestException;

  if ADefaultSubtype = TdxPDFDocumentImage.GetTypeName then
  begin
    Result := TdxPDFDocumentImage.Create(nil);
    TdxPDFDocumentImage(Result).Read(ARepository, AStream);
  end
  else
    if ADefaultSubtype = 'Form' then
      Result := TdxPDFForm.CreateForm(AStream, nil)
    else
      Result := nil;
end;

procedure TdxPDFXObject.Draw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  EnterCriticalSection(FLock);
  try
    DoDraw(AInterpreter);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class function TdxPDFXObject.GetTypeName: string;
begin
  Result := TdxPDFKeywords.XObject;
end;

procedure TdxPDFXObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFXObject.DestroySubClasses;
begin
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFXObject.DoDraw(const AInterpreter: IdxPDFCommandInterpreter);
begin
// do nothing
end;

{ TdxPDFOpacityStream }

constructor TdxPDFOpacityStream.Create(AWidth: Integer; const ADecode: TdxPDFRange; const AData: TBytes);
begin
  inherited Create;
  FPosition := -1;
  FData := AData;
  FPaintValue := IfThen(ADecode.Min <= ADecode.Max, 255);
  FMaskValue := IfThen(ADecode.Min <= ADecode.Max, 0, 255);
  FStride := AWidth;
end;

{ TdxPDFInterpolatedOpacityStream }

constructor TdxPDFInterpolatedOpacityStream.Create(const AMaskData: TBytes);
begin
  inherited Create;
  FPosition := -1;
  FMaskData := AMaskData;
end;

function TdxPDFInterpolatedOpacityStream.GetNextValue: Byte;
begin
  Inc(FPosition);
  Result := FMaskData[FPosition];
end;

{ TdxPDFOpacityInterpolator }

constructor TdxPDFOpacityInterpolator.Create(const AData: TBytes; ASize: Integer; const AResult: TBytes;
  ASourceSize, ADestinationSize: Integer);
begin
  inherited Create;
  FData := AData;
  FSize := ASize;
  FResult := AResult;
  FDestinationSize := ADestinationSize;
  FFactor := ADestinationSize / ASourceSize;
  SetLength(FPrev, ASize);
  SetLength(FNext, ASize);
end;

class function TdxPDFOpacityInterpolator.HorizontalInterpolation(const AData: TBytes;
  ASourceWidth, ASourceStride, ADestinationWidth, AHeight, AComponentCount: Integer): TBytes;
var
  X, Y, ASourceIndex: Integer;
  AInterpolator: TdxPDFOpacityInterpolator;
begin
  SetLength(Result, ADestinationWidth * AHeight * AComponentCount);
  AInterpolator := TdxPDFOpacityInterpolator.Create(AData, AComponentCount, Result, ASourceWidth, ADestinationWidth);
  try
    ASourceIndex := 0;
    for Y := 0 to AHeight - 1 do
    begin
      AInterpolator.Start(ASourceIndex);
      for X := 1 to ASourceWidth - 1 do
        AInterpolator.Interpolate(X);
      AInterpolator.DoEnd;
      Inc(ASourceIndex, ASourceStride);
    end;
  finally
    AInterpolator.Free;
  end;
end;

class function TdxPDFOpacityInterpolator.VerticalInterpolation(const AData: TBytes;
  AWidth, ASourceHeight, ADestinationHeight: Integer): TBytes;
var
  Y: Integer;
  AInterpolator: TdxPDFOpacityInterpolator;
begin
  SetLength(Result, AWidth * ADestinationHeight);
  AInterpolator := TdxPDFOpacityInterpolator.Create(AData, AWidth, Result, ASourceHeight, ADestinationHeight);
  try
    AInterpolator.Start(0);
    for Y := 1 to ASourceHeight - 1 do
      AInterpolator.Interpolate(Y);
    AInterpolator.DoEnd;
  finally
    AInterpolator.Free;
  end;
end;

function TdxPDFOpacityInterpolator.NextInterpolationArray: TBytes;
begin
  SetLength(Result, FSize);
  TdxPDFUtils.CopyData(FData, FSourcePosition, Result, 0, FSize);
  Inc(FSourcePosition, FSize);
end;

procedure TdxPDFOpacityInterpolator.FillResultData(const AArray: TBytes);
begin
  TdxPDFUtils.CopyData(AArray, 0, FResult, FResultPosition, FSize);
  Inc(FResultPosition, FSize);
end;

procedure TdxPDFOpacityInterpolator.Start(APosition: Integer);
begin
  FSourcePosition := APosition;
  FPrev := NextInterpolationArray;
  FPrevResultPosition := 0;
end;

procedure TdxPDFOpacityInterpolator.Interpolate(AValue: Integer);
var
  ANextResultPosition, AFraction: Double;
  APosition, I: Integer;
  ACurrent: TBytes;
begin
  FNext := NextInterpolationArray;
  ANextResultPosition := AValue * FFactor;
  APosition := Trunc(ANextResultPosition);
  for I := FPrevResultPosition to APosition - 1 do
  begin
    FillResultData(FPrev);
    Inc(FPrevResultPosition);
  end;
  AFraction := ANextResultPosition - APosition;
  if AFraction = 0 then
    FPrevResultPosition := APosition
  else
  begin
    SetLength(ACurrent, FSize);
    for I := 0 to FSize - 1 do
      ACurrent[I] := TdxPDFUtils.ConvertToByte(FPrev[I] + (FNext[I] - FPrev[I]) * AFraction);
    FillResultData(ACurrent);
    Inc(APosition);
    FPrevResultPosition := APosition;
  end;
  FPrev := FNext;
end;

procedure TdxPDFOpacityInterpolator.DoEnd;
var
  I: Integer;
begin
  for I := FPrevResultPosition to FDestinationSize - 1 do
  begin
    FillResultData(FPrev);
    Inc(FPrevResultPosition);
  end;
end;

function TdxPDFOpacityStream.GetNextValue: Byte;
begin
  if FStridePosition >= FStride then
  begin
    FStridePosition := 0;
    FCurrentBit := 0;
  end;
  FCurrentBit := FCurrentBit shr 1;
  if FCurrentBit = 0 then
  begin
    Inc(FPosition);
    FCurrentByte := FData[FPosition];
    FCurrentBit := $80;
  end;
  Inc(FStridePosition);
  Result := IfThen((FCurrentByte and FCurrentBit) = 0, FPaintValue, FMaskValue);
end;

{ TdxPDFForm }

class function TdxPDFForm.CreateForm(AStream: TdxPDFStream; AParentResources: TdxPDFResources): TdxPDFForm;
begin
  if AStream.Dictionary.Contains('Group') then
    Result := TdxPDFGroupForm.Create(nil)
  else
    Result := TdxPDFForm.Create(nil);
  AStream.Dictionary.StreamRef := AStream;
  Result.Read(AStream.Dictionary as TdxPDFReaderDictionary);
end;

constructor TdxPDFForm.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
end;

constructor TdxPDFForm.Create(ACatalog: TdxPDFCatalog; const ABBox: TdxRectF);
begin
  Create(nil);
  Repository := ACatalog.Repository;
  FBBox := ABBox;
  FMatrix := TdxPDFTransformationMatrix.Create;
  Stream := TdxPDFStream.Create(nil);
  Resources := TdxPDFResources.Create(nil);
  FUseOwnResources := True;
end;

function TdxPDFForm.GetCommands: TdxPDFReferencedObjects;
var
  AActualStream: TdxPDFStream;
begin
  if FCommands = nil then
  begin
    FCommands := TdxPDFReferencedObjects.Create;
    AActualStream := nil;
    if Stream <> nil then
      AActualStream := Stream
    else
      if FStreamRef <> nil then
        AActualStream := FStreamRef;
    if AActualStream <> nil then
      TdxPDFCommandStreamParser.Parse(FRepository, AActualStream.UncompressedData, FCommands, Resources);
  end;
  Result := FCommands;
end;

function TdxPDFForm.GetTransformationMatrix(const ARect: TdxRectF): TdxPDFTransformationMatrix;
var
  ABoundingBox: TdxRectF;
  I: Integer;
  ALeft, ARight, ATop, ABottom, AXMin, AXMax, AYMin, AYMax, X, Y, AScaleX, AScaleY: Double;
  ABottomLeft, APoint, AOffset: TdxPointF;
  APoints: TdxPDFPoints;
begin
  ABoundingBox := BBox;
  ALeft := ABoundingBox.Left;
  ARight := ABoundingBox.Right;
  ATop := ABoundingBox.Top;
  ABottom := ABoundingBox.Bottom;
  ABottomLeft := FMatrix.Transform(dxPointF(ALeft, ABottom));
  AXMin := ABottomLeft.X;
  AXMax := ABottomLeft.X;
  AYMin := ABottomLeft.Y;
  AYMax := ABottomLeft.Y;
  SetLength(APoints, 3);
  APoints[0] := FMatrix.Transform(dxPointF(ALeft, ATop));
  APoints[1] := FMatrix.Transform(dxPointF(ARight, ATop));
  APoints[2] := FMatrix.Transform(dxPointF(ARight, ABottom));

  for I := Low(APoints) to High(APoints) do
  begin
    APoint := APoints[I];
    X := APoint.X;
    AXMin := TdxPDFUtils.Min(AXMin, X);
    AXMax := TdxPDFUtils.Max(AXMax, X);
    Y := APoint.Y;
    AYMin := TdxPDFUtils.Min(AYMin, Y);
    AYMax := TdxPDFUtils.Max(AYMax, Y);
  end;

  if (AXMax - AXMin) <> 0 then
  begin
    AScaleX := ARect.Width / (AXMax - AXMin);
    AOffset.X := ARect.Left - AXMin * AScaleX;
  end
  else
  begin
    AScaleX := 1;
    AOffset.X := 0;
  end;

  if (AYMax - AYMin) <> 0 then
  begin
    AScaleY := Abs(ARect.Height) / (AYMax - AYMin);
    AOffset.Y := ARect.Bottom - AYMin * AScaleY;
  end
  else
  begin
    AScaleY := 1;
    AOffset.Y := 0;
  end;


  Result := TdxPDFTransformationMatrix.CreateEx(AScaleX, 0, 0, AScaleY, AOffset.X, AOffset.Y);
end;

procedure TdxPDFForm.ReplaceCommands(const ACommandData: TBytes);
begin
  if Stream = nil then
    Stream := TdxPDFStream.Create;
  Stream.RawData := ACommandData;
end;

procedure TdxPDFForm.DoDraw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawForm(Self, Resources);
end;

class function TdxPDFForm.GetTypeName: string;
begin
  Result := 'Form';
end;

procedure TdxPDFForm.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FMatrix := nil;
  FCommands := nil;
  FRepository := nil;
  Resources := nil;
end;

procedure TdxPDFForm.DestroySubClasses;
begin
  Resources := nil;
  FRepository := nil;
  FreeAndNil(FCommands);
  FreeAndNil(FMatrix);
  inherited DestroySubClasses;
end;

procedure TdxPDFForm.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FStreamRef := ADictionary.StreamRef;
    FBBox := ADictionary.GetRectangle(TdxPDFKeywords.BBox);
    CheckFormType(ADictionary);
    FMatrix := TdxPDFUtils.ArrayToMatrix(ADictionary.GetArray(TdxPDFKeywords.Matrix));
    FRepository := Repository;
    ReadResources(ADictionary);
  end;
end;

procedure TdxPDFForm.CheckFormType(ADictionary: TdxPDFDictionary);
var
  AFormType: Integer;
begin
  AFormType := ADictionary.GetInteger(TdxPDFKeywords.FormType);
  if (TdxPDFUtils.IsIntegerValid(AFormType) and (AFormType <> 1)) then
    TdxPDFUtils.RaiseTestException;
end;

procedure TdxPDFForm.ReadResources(ADictionary: TdxPDFReaderDictionary);
begin
  Resources := Repository.GetResources(ADictionary);
end;

procedure TdxPDFForm.SetMatrix(const AValue: TdxPDFTransformationMatrix);
begin
  if FMatrix <> nil then
    FreeAndNil(FMatrix);
  FMatrix := AValue;
end;

procedure TdxPDFForm.SetResources(const AValue: TdxPDFResources);
begin
  if FUseOwnResources then
    dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources))
  else
    FResources := AValue;
end;

{ TdxPDFCustomSoftMask }

class function TdxPDFCustomSoftMask.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Mask;
end;

procedure TdxPDFCustomSoftMask.CreateSubClasses;
begin
  inherited CreateSubClasses;
  TransparencyGroup := nil;
  TransparencyFunction := nil;
end;

procedure TdxPDFCustomSoftMask.DestroySubClasses;
begin
  TransparencyFunction := nil;
  TransparencyGroup := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomSoftMask.Read(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
  AValue: TdxPDFStream;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AValue := ADictionary.GetStream('G');
    if AValue <> nil then
      TransparencyGroup := TdxPDFXObject.Parse(Repository, AValue, TdxPDFGroupForm.GetTypeName) as TdxPDFGroupForm;
    if ADictionary.TryGetObject('TR', AObject) then
      TransparencyFunction := TdxPDFCustomFunction.Parse(Repository, AObject)
    else
      TransparencyFunction := TdxPDFIdentityFunction.Create(nil);
  end;
end;

class function TdxPDFCustomSoftMask.Parse(ARepository: TdxPDFCustomRepository;
  ASourceObject: TdxPDFBase): TdxPDFCustomSoftMask;
var
  ADictionary: TdxPDFReaderDictionary;
  AType, ASoftMaskType: string;
begin
  Result := nil;
  if ASourceObject <> nil then
  begin
    case ASourceObject.ObjectType of
      otIndirectReference:
        Result := TdxPDFCustomSoftMask.Parse(ARepository, ARepository.GetObject(ASourceObject.Number) as TdxPDFBase);
      otName:
        begin
          AType := TdxPDFString(ASourceObject).Value;
          if AType <> TdxPDFEmptySoftMask.GetTypeName then
            TdxPDFUtils.RaiseTestException;
          Result := TdxPDFEmptySoftMask.Create(nil);
        end;
      otDictionary:
        begin
          ADictionary := TdxPDFReaderDictionary(ASourceObject);
          AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
          ASoftMaskType := ADictionary.GetString('S');
          if ASoftMaskType = TdxPDFLuminositySoftMask.GetTypeName then
            Result := TdxPDFLuminositySoftMask.Create(nil)
          else
            if ASoftMaskType = TdxPDFAlphaSoftMask.GetTypeName then
              Result := TdxPDFLuminositySoftMask.Create(nil);
          if Result <> nil then
            Result.Read(ADictionary);
        end;
    else
      Result := nil;
    end;
  end;
end;

procedure TdxPDFCustomSoftMask.SetTransparencyGroup(const AValue: TdxPDFGroupForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTransparencyGroup));
end;

procedure TdxPDFCustomSoftMask.SetTransparencyFunction(const AValue: TdxPDFObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTransparencyFunction));
end;

{ TdxPDFLuminositySoftMask }

class function TdxPDFLuminositySoftMask.GetTypeName: string;
begin
  Result := 'Luminosity';
end;

procedure TdxPDFLuminositySoftMask.DestroySubClasses;
begin
  FreeAndNil(FBackdropColor);
  inherited DestroySubClasses;
end;

procedure TdxPDFLuminositySoftMask.Read(ADictionary: TdxPDFReaderDictionary);
var
  ABackdropColorArray: TdxPDFArray;
  AColorSpace: TdxPDFCustomColorSpace;
  I, ACount: Integer;
  AComponents: TDoubleDynArray;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AColorSpace := TransparencyGroup.ColorSpace;
    ABackdropColorArray := ADictionary.GetArray('BC');
    if ABackdropColorArray <> nil then
    begin
      ACount := ABackdropColorArray.Count;
      if ACount = AColorSpace.ComponentCount then
      begin
        SetLength(AComponents, ACount);
        for I := 0 to ACount - 1 do
          AComponents[I] := TdxPDFUtils.ConvertToDouble(ABackdropColorArray[I]);
        FBackdropColor := TdxPDFColor.Create(AComponents);
      end;
    end;
  end;
end;

{ TdxPDFAlphaSoftMask }

class function TdxPDFAlphaSoftMask.GetTypeName: string;
begin
  Result := 'Alpha';
end;

{ TdxPDFEmptySoftMask }

class function TdxPDFEmptySoftMask.GetTypeName: string;
begin
  Result := 'None';
end;

{ TdxPDFTransparencyGroup }

class function TdxPDFTransparencyGroup.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Group;
end;

procedure TdxPDFTransparencyGroup.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorSpace := nil;
end;

procedure TdxPDFTransparencyGroup.DestroySubClasses;
begin
  FreeAndNil(FColorSpace);
  inherited DestroySubClasses;
end;

procedure TdxPDFTransparencyGroup.Read(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
  AType, ASubtype: string;
begin
  inherited Read(ADictionary);
  AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
  ASubtype := ADictionary.GetString(SubtypeKey);
  if (AType <> '') and (AType <> TdxPDFKeywords.Group) or (ASubtype <> '') and (ASubtype <> TdxPDFKeywords.Transparency) then
    TdxPDFUtils.RaiseTestException;
  if ADictionary.TryGetObject(ColorSpaceKey, AObject) then
    FColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
  FIsolated := ADictionary.GetBoolean(IsolatedKey);
  FKnockout := ADictionary.GetBoolean(KnockoutKey);
end;

{ TdxPDFGroupForm }

class function TdxPDFGroupForm.GetTypeName: string;
begin
  Result := 'Group';
end;

procedure TdxPDFGroupForm.CreateSubClasses;
begin
  inherited CreateSubClasses;
end;

procedure TdxPDFGroupForm.DestroySubClasses;
begin
  FreeAndNil(FGroup);
  inherited DestroySubClasses;
end;

procedure TdxPDFGroupForm.DoDraw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawTransparencyGroup(Self);
end;

procedure TdxPDFGroupForm.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadGroup(ADictionary);
end;

function TdxPDFGroupForm.GetColorSpace: TdxPDFCustomColorSpace;
begin
  if FGroup <> nil then
    Result := FGroup.ColorSpace
  else
    Result := nil;
end;

procedure TdxPDFGroupForm.ReadGroup(ADictionary: TdxPDFReaderDictionary);
begin
  FGroup := TdxPDFTransparencyGroup.Create(nil);
  FGroup.Read(ADictionary.GetDictionary('Group'));
end;

{ TdxPDFDocumentImage }

constructor TdxPDFDocumentImage.Create(const AData: TBytes; AColorSpace: TdxPDFCustomColorSpace; AFilters: TdxPDFFilters;
  AWidth, AHeight, ABitsPerComponent: Integer; AHasMask: Boolean; ADictionary: TdxPDFDictionary);
begin
  Create(nil);
  FWidth := AWidth;
  FHeight := AHeight;
  FBitsPerComponent := ABitsPerComponent;
  ColorSpace := AColorSpace;
  Stream := TdxPDFStream.Create;
  Stream.Filters.AddRange(AFilters);
  Stream.RawData := AData;
  FHasMask := AHasMask;
  if HasMask then
  begin
    if AColorSpace = nil then
      ColorSpace := TdxPDFGrayDeviceColorSpace.Create(Self)
    else
      if not(ColorSpace is TdxPDFCustomDeviceColorSpace) then
        TdxPDFUtils.RaiseTestException('Create inline image');
  end
  else
    if AColorSpace = nil then
      TdxPDFUtils.RaiseTestException('Create inline image');
  ReadDecodeRanges(ADictionary);
end;

class function TdxPDFDocumentImage.CreateBitmap(AData: TdxPDFDocumentImageData): GpBitmap;
begin
  GdipCheck(GdipCreateBitmapFromScan0(AData.Width, AData.Height, AData.Stride,
    TdxPDFUtils.ConvertToGpPixelFormat(AData.PixelFormat), @AData.Data[0], Result));
end;

function TdxPDFDocumentImage.CreateImageData: TdxPDFDocumentImageData;
var
  AData, AMaskData: TBytes;
  ADecodedData: TdxPDFDocumentDecodedImageData;
  ASourceStride, AComponentCount, AWidth: Integer;
  ATransformResult: TdxPDFColorSpaceTransformResult;
begin
  ADecodedData := GetDecodedImageData;
  AData := ADecodedData.Data;
  if Length(AData) = 0 then
    Exit(nil);
  Result := TdxPDFDocumentImageData.Create;
  Result.Width := Width;
  Result.Height := Height;
  if HasMask then
    Result.PixelFormat := pfGray1bit
  else
  begin
    if ColorSpace = nil then
    begin
      Result.PixelFormat := ADecodedData.PixelFormat;
      if Result.PixelFormat = pfUnknown then
      begin
        Result.Free;
        Result := nil;
        Exit;
      end;
    end
    else
    begin
      ATransformResult := ColorSpace.Transform(Self, AData);
      if not ATransformResult.IsInvalid then
      begin
        SetLength(AData, 0);
        AData := ATransformResult.Data;
        Result.PixelFormat := ATransformResult.PixelFormat;
        AMaskData := ATransformResult.MaskData;
      end
      else
      begin
        Result.Free;
        Result := nil;
        Exit;
      end;
    end;
  end;
  Result.Data := AData;
  Result.Width := Width;
  Result.Height := Height;
  if SoftMask <> nil then
  begin
    SoftMask.ApplySoftMask(Result);
    if Length(Result.Data) <> 0 then
    begin
      Result.Bitmap := CreateBitmap(Result);
      Exit;
    end;
  end;
  if not(Result.PixelFormat in [pfGray1bit, pfGray8bit]) and (Length(AMaskData) <> 0) then
  begin
    Result.Stride := Width;
    ApplyMask(Result, AMaskData);
    ApplyStencilMask(Result);
  end
  else
  begin
    Result.CalculateParameters(AComponentCount, ASourceStride, AWidth);
    if SoftMask <> nil then
      SoftMask.ApplySoftMask(Result)
    else
    begin
      Result.PopulateData(AData, AWidth, ASourceStride, AComponentCount);
      SetLength(AData, 0);
      ApplyStencilMask(Result);
    end;
  end;
  Result.Bitmap := CreateBitmap(Result);
end;

function TdxPDFDocumentImage.GetAsBitmap: Graphics.TBitmap;
var
  AData: TdxPDFDocumentImageData;
  ATemp: TdxSmartImage;
begin
  AData := CreateImageData;
  try
    ATemp := TdxSmartImage.Create;
    try
      ATemp.Handle := CreateBitmap(AData);
      Result := ATemp.GetAsBitmap;
    finally
      ATemp.Free;
    end;
  finally
    AData.Free;
  end;
end;

class function TdxPDFDocumentImage.GetTypeName: string;
begin
  Result := 'Image';
end;

function TdxPDFDocumentImage.GetData: TBytes;
begin
  Result := GetDecodedImageData.Data;
end;

procedure TdxPDFDocumentImage.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorSpace := nil;
  FMask := nil;
  FSoftMask := nil;
  FListeners := TInterfaceList.Create;
end;

procedure TdxPDFDocumentImage.DestroySubClasses;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxPDFDocumentSharedObjectListener).DestroyHandler(Self);
  FreeAndNil(FListeners);
  ColorSpace := nil;
  Mask := nil;
  SoftMask := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFDocumentImage.DoDraw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawImage(Self)
end;

procedure TdxPDFDocumentImage.Initialize;
begin
  inherited Initialize;
  FGUID := TdxPDFUtils.GenerateGUID;
  FBitsPerComponent := 1;
end;

procedure TdxPDFDocumentImage.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  DoRead(ADictionary);
end;

procedure TdxPDFDocumentImage.Read(ARepository: TdxPDFCustomRepository; AStream: TdxPDFStream);
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := AStream.Dictionary as TdxPDFReaderDictionary;
  if ADictionary <> nil then
  begin
    inherited Read(ADictionary);
    Stream := AStream;
    DoRead(ADictionary);
  end;
end;

procedure TdxPDFDocumentImage.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FBitsPerComponent := ADictionary.GetInteger(TdxPDFKeywords.BitsPerComponent, 1);
  FHeight := ADictionary.GetInteger(TdxPDFKeywords.Height, 0);
  FWidth := ADictionary.GetInteger(TdxPDFKeywords.Width, 0);
  FID := ADictionary.GetString(TdxPDFKeywords.ID);
  FHasMask := ADictionary.GetBoolean(TdxPDFKeywords.ImageMask);
  FIntent := ADictionary.GetString(TdxPDFKeywords.Intent);
  FNeedInterpolate := ADictionary.GetBoolean(TdxPDFKeywords.Interpolate);
  FStructParent := ADictionary.GetInteger(TdxPDFKeywords.StructParent);
end;

function TdxPDFDocumentImage.GetDecodedImageData: TdxPDFDocumentDecodedImageData;
var
  I, AFilterIndex, ALineWidth, ADivider, AComponentCount: Integer;
  AFilter: TdxPDFCustomStreamFilter;
begin
  Result.Data := Stream.RawData;
  Result.PixelFormat := pfUnknown;
  AFilterIndex := Stream.Filters.Count - 1;
  if AFilterIndex >= 0 then
  begin
    for I := 0 to AFilterIndex - 1 do
      Result.Data := Stream.Filters[I].Decode(Result.Data);
    AFilter := Stream.Filters[AFilterIndex];
    if ColorSpace = nil then
      AComponentCount := 1
    else
      AComponentCount := ColorSpace.ComponentCount;
    Result := AFilter.DecodeImageData(Result.Data, Width, Height, AComponentCount);
  end;
  ALineWidth := Width;
  if FColorSpace <> nil then
    ALineWidth := ALineWidth * FColorSpace.ComponentCount
  else
    if Result.PixelFormat = pfArgb24bpp then
      ALineWidth := ALineWidth * 3;
  if FBitsPerComponent in [1, 2, 4] then
  begin
    ADivider := 8 div FBitsPerComponent;
    ALineWidth := IfThen(ALineWidth mod ADivider = 0, ALineWidth div ADivider + 0, ALineWidth div ADivider + 1);
  end
  else
    ALineWidth := ALineWidth * FBitsPerComponent div 8;
  if Length(Result.Data) < ALineWidth * FHeight then
    SetLength(Result.Data, 0);
end;

procedure TdxPDFDocumentImage.AddListener(AListener: IdxPDFDocumentSharedObjectListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TdxPDFDocumentImage.RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);
begin
  FListeners.Remove(AListener);
end;

procedure TdxPDFDocumentImage.SetColorSpace(const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FColorSpace));
end;

procedure TdxPDFDocumentImage.SetMask(const AValue: TdxPDFDocumentImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FMask));
end;

procedure TdxPDFDocumentImage.SetSoftMask(const AValue: TdxPDFDocumentImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FSoftMask));
end;

function TdxPDFDocumentImage.CreateMaskOpacityStream(const AData: TBytes; AWidth, AHeight: Integer): IdxPDFOpacityStream;
var
  I: Integer;
  ATempData: TBytes;
begin
  Result := TdxPDFOpacityStream.Create(FMask.Width, FMask.DecodeRanges[0], AData);
  if (AWidth > FMask.Width) or (AHeight > FMask.Height) then
  begin
    SetLength(ATempData, FMask.Width * FMask.Height);
    for I := 0 to Length(ATempData) - 1 do
      ATempData[I] := Result.GetNextValue;
    if AWidth > FMask.Width then
      ATempData := TdxPDFOpacityInterpolator.HorizontalInterpolation(ATempData, FMask.Width, FMask.Width, AWidth, FMask.Height, 1);
    if AHeight > FMask.Height then
      ATempData := TdxPDFOpacityInterpolator.VerticalInterpolation(ATempData, AWidth, FMask.Height, AHeight);
    Result := TdxPDFInterpolatedOpacityStream.Create(ATempData);
  end;
end;

function TdxPDFDocumentImage.ApplyDecodeRanges(const AData: TBytes): TBytes;
var
  I, APosition, ARowPosition: Integer;
  ACurrentByte, ABit, B, AMinValue, AMaxValue: Byte;
  ARange: TdxPDFRange;
begin
  if FBitsPerComponent = 1 then
  begin
    AMinValue := 0;
    AMaxValue := 255;
    if Length(DecodeRanges) > 0 then
    begin
      ARange := DecodeRanges[0];
      AMinValue := TdxPDFUtils.ConvertToByte(ARange.Min * 255);
      AMaxValue := TdxPDFUtils.ConvertToByte(ARange.Max * 255);
    end;
    SetLength(Result, Length(AData) * 8);
    APosition := 0;
    ARowPosition := 0;
    for I := 0 to Length(AData) - 1 do
    begin
      ACurrentByte := AData[I];
      B := ACurrentByte;
      for ABit := 0 to 7 do
      begin
        Result[APosition] := IfThen((B and $80) = 0, AMinValue, AMaxValue);
        Inc(APosition);
        Inc(ARowPosition);
        if ARowPosition = Width then
        begin
          ARowPosition := 0;
          Break;
        end;
        B := B shl 1;
      end;
    end;
  end;
end;

function TdxPDFDocumentImage.ApplyColorSpace(const ASoftMaskDecodedData, ASoftMaskData: TBytes): TBytes;
var
  ATransformResult: TdxPDFColorSpaceTransformResult;
  AInfo: TdxPDFImageInfo;
begin
  if FBitsPerComponent = 1 then
  begin
    AInfo := dxPDFImageInfo(FWidth, FHeight, 8, ColorKeyMask);
    AInfo.Data := ASoftMaskData;
    ATransformResult := ColorSpace.Transform(AInfo);
  end
  else
    ATransformResult := ColorSpace.Transform(Self, ASoftMaskDecodedData);
  Result := ATransformResult.Data;
end;

function TdxPDFDocumentImage.InterpolateSoftMaskImageData(const AData: TBytes;  AWidth, AHeight: Integer; var ASoftMaskData: TBytes;
  out AActualWidth, AActualHeight, AMaskStride: Integer): TBytes;
begin
  AActualWidth := Max(Width, AWidth);
  AActualHeight := Max(Height, AHeight);

  AMaskStride := Width;
  Result := AData;
  if AActualWidth > AWidth then
    Result := TdxPDFImageInterpolator.HorizontalInterpolation(Result, AWidth, AWidth * 3, AActualWidth, AHeight, 3)
  else
    if AActualWidth > Width then
    begin
      ASoftMaskData := TdxPDFImageInterpolator.HorizontalInterpolation(ASoftMaskData, Width, AMaskStride, AActualWidth, Height, 1);
      AMaskStride := AActualWidth;
    end;

  if AActualHeight > AHeight then
    Result := TdxPDFImageInterpolator.VerticalInterpolation(Result, AActualWidth * 3, AHeight, AActualHeight)
  else
    if AActualHeight > Height then
      ASoftMaskData := TdxPDFImageInterpolator.VerticalInterpolation(ASoftMaskData, AMaskStride, Height, AActualHeight);
end;

function TdxPDFDocumentImage.InterpolateStencilMaskData(const AData: TBytes; AWidth, AHeight, AComponentCount: Integer;
  var AStride: Integer): TBytes;
begin
  Result := AData;
  if AWidth > Width then
  begin
    Result := TdxPDFImageInterpolator.HorizontalInterpolation(Result, Width, AStride, AWidth, Height, AComponentCount);
    AStride := AWidth * AComponentCount;
  end;

  if AHeight > Height then
    Result := TdxPDFImageInterpolator.VerticalInterpolation(Result, AWidth * AComponentCount, Height, AHeight);
end;

procedure TdxPDFDocumentImage.ApplyMask(AImageData: TdxPDFDocumentImageData; const AMaskData: TBytes);
var
  AData: TBytes;
  X, Y, ASourceIndex, ADestinationIndex, AMaskIndex, AStartMaskIndex, AStride: Integer;
begin
  AStride := AImageData.Stride;
  AImageData.Stride := AImageData.Width * 4;
  AImageData.PixelFormat := pfArgb32bpp;

  ASourceIndex := 0;
  ADestinationIndex := 0;
  AStartMaskIndex := 0;
  SetLength(AData, AImageData.Stride * AImageData.Height);
  for Y := 0 to AImageData.Height - 1 do
  begin
    AMaskIndex := AStartMaskIndex;
    for X := 0 to AImageData.Width - 1 do
    begin
      AData[ADestinationIndex] := AImageData.Data[ASourceIndex + 2];
      AData[ADestinationIndex + 1] := AImageData.Data[ASourceIndex + 1];
      AData[ADestinationIndex + 2] := AImageData.Data[ASourceIndex];
      AData[ADestinationIndex + 3] := AMaskData[AMaskIndex];
      Inc(ADestinationIndex, 4);
      Inc(ASourceIndex, 3);
      Inc(AMaskIndex);
    end;
    Inc(AStartMaskIndex, AStride);
  end;
  AImageData.Data := AData;
end;

procedure TdxPDFDocumentImage.ApplySoftMask(AImageData: TdxPDFDocumentImageData);
var
  ADecodedSoftMaskData, ASoftMaskData, AData: TBytes;
  ASoftMaskStride, ARealWidth, ARealHeight: Integer;
begin
  AData := ToRGB(AImageData.Data, AImageData.PixelFormat, FWidth, FHeight);
  ADecodedSoftMaskData := GetDecodedImageData.Data;
  ASoftMaskData := ApplyDecodeRanges(ADecodedSoftMaskData);
  ASoftMaskData := ApplyColorSpace(ADecodedSoftMaskData, ASoftMaskData);
  AData := InterpolateSoftMaskImageData(AData, AImageData.Width, AImageData.Height, ASoftMaskData,
    ARealWidth, ARealHeight, ASoftMaskStride);
  AImageData.Data := AData;
  AImageData.Width := ARealWidth;
  AImageData.Height := ARealHeight;
  AImageData.Stride := ASoftMaskStride;
  ApplyMask(AImageData, ASoftMaskData);
end;

procedure TdxPDFDocumentImage.ApplyStencilMask(AImageData: TdxPDFDocumentImageData);
var
  AOpacityStream: IdxPDFOpacityStream;
  ADecodedMaskData, AData, ATempImageData: TBytes;
  X, Y, ASourceIndex, ADestinationIndex, AActualWidth, AActualHeight, AStride, ASourceStride: Integer;
begin
  if not HasValidStencilMask then
    Exit;
  AData := ToRGB(AImageData.Data, AImageData.PixelFormat, FWidth, FHeight);
  ADecodedMaskData := FMask.GetDecodedImageData.Data;
  if (Length(AData) = 0) or (Length(ADecodedMaskData) = 0) then
    Exit;

  AActualWidth := Max(Width, FMask.Width);
  AActualHeight := Max(Height, FMask.Height);

  ASourceStride := Length(AData) div AImageData.Height;
  ATempImageData := InterpolateStencilMaskData(AData, AActualWidth, AActualHeight,
    IfThen(AImageData.PixelFormat = pfArgb32bpp, 4, 3), ASourceStride);

  AOpacityStream := CreateMaskOpacityStream(ADecodedMaskData, AActualWidth, AActualHeight);
  ADestinationIndex := 0;
  AStride := AActualWidth * 4;
  SetLength(AData, AStride * AActualHeight);
  for Y := 0 to AActualHeight - 1 do
  begin
    ASourceIndex := ASourceStride * Y;
    for X := 0 to AActualWidth - 1 do
    begin
      TdxPDFUtils.CopyData(ATempImageData, ASourceIndex, AData, ADestinationIndex, 3);
      AData[ADestinationIndex + 3] := AOpacityStream.GetNextValue;
      Inc(ADestinationIndex, 4);
      Inc(ASourceIndex, 3);
    end;
  end;
  AImageData.Data := AData;
  AImageData.Height := AActualHeight;
  AImageData.Width := AActualWidth;
  AImageData.Stride := AStride;
  AImageData.PixelFormat := pfArgb32bpp;
end;

procedure TdxPDFDocumentImage.CalculateComponentCount(ADictionary: TdxPDFDictionary);
var
  ALastFilterIndex: Integer;
begin
  if not HasMask then
  begin
    if not ADictionary.Contains(TdxPDFKeywords.ColorSpace) or not TdxPDFUtils.IsIntegerValid(FBitsPerComponent) then
    begin
      ALastFilterIndex := Stream.Filters.Count - 1;
      if (ALastFilterIndex < 0) or not (Stream.Filters[ALastFilterIndex] is TdxPDFJPXDecodeFilter) then
        TdxPDFUtils.RaiseTestException(ClassName + ': CalculateComponentCount');
      FComponentCount := 0;
    end
    else
    begin
      FComponentCount := ColorSpace.ComponentCount;
      if not(FBitsPerComponent in [1, 2, 4, 8, 16]) then
        TdxPDFUtils.RaiseTestException(ClassName + ': CalculateComponentCount');
    end;
  end
  else
    FComponentCount := 1;
end;

procedure TdxPDFDocumentImage.DoRead(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    ReadColorSpace(ADictionary);
    ReadDecodeRanges(ADictionary);
    ReadMatte(ADictionary);
    CalculateComponentCount(ADictionary);
    ReadSoftMask(ADictionary);
    ReadMask(ADictionary);
  end;
end;

procedure TdxPDFDocumentImage.ReadColorKeyMask(AArray: TdxPDFArray);
var
  I, AIndex, AMin, AMax, AMaxMaskValue: Integer;
  AMaxSourceObject, AMinSourceObject: TdxPDFBase;
begin
  AMaxMaskValue := (1 shl FBitsPerComponent) - 1;
  AIndex := 0;
  SetLength(FColorKeyMask, 0);
  for I := 0 to FComponentCount - 1 do
  begin
    AMinSourceObject := AArray[AIndex];
    AMaxSourceObject := AArray[AIndex + 1];
    Inc(AIndex, 2);
    if (AMinSourceObject.ObjectType <> otInteger) or (AMaxSourceObject.ObjectType <> otInteger) then
      TdxPDFUtils.RaiseTestException;
    AMin := TdxPDFInteger(AMinSourceObject).Value;
    AMin := IfThen(AMin < 0, Integer(AMin = 0), AMin);
    AMax := TdxPDFInteger(AMaxSourceObject).Value;
    AMax := IfThen(AMax > AMaxMaskValue, AMaxMaskValue, AMax);
    if AMax < AMin then
      TdxPDFUtils.RaiseTestException;
    TdxPDFUtils.AddRange(TdxPDFRange.Create(AMin, AMax), FColorKeyMask);
  end;
end;

procedure TdxPDFDocumentImage.ReadColorSpace(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if HasMask then
  begin
    FBitsPerComponent := IfThen(TdxPDFUtils.IsIntegerValid(FBitsPerComponent), FBitsPerComponent, 1);
    if (FBitsPerComponent <> 1) or ADictionary.Contains(TdxPDFKeywords.Mask) then
      TdxPDFUtils.RaiseTestException('TdxPDFDocumentImage.ReadColorSpace');
    if ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
    begin
      ColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
      if not (ColorSpace is TdxPDFGrayDeviceColorSpace) then
        TdxPDFUtils.RaiseTestException('TdxPDFDocumentImage.ReadColorSpace');
    end
    else
      ColorSpace := TdxPDFGrayDeviceColorSpace.Create(Self);
  end
  else
    if ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
      ColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
end;

procedure TdxPDFDocumentImage.ReadDecodeRanges(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
  AComponentCount, I, AIndex: Integer;
begin
  SetLength(FDecodeRanges, 0);
  AArray := ADictionary.GetArray(TdxPDFKeywords.Decode, 'D');
  if AArray = nil then
  begin
    if FColorSpace = nil then
      TdxPDFUtils.AddRange(TdxPDFRange.Create(0, 1), FDecodeRanges)
    else
      FDecodeRanges := FColorSpace.CreateDefaultDecodeArray(FBitsPerComponent);
  end
  else
  begin
    AComponentCount := IfThen(FColorSpace = nil, 1, FColorSpace.ComponentCount);
    if AArray.Count < AComponentCount * 2 then
      TdxPDFUtils.RaiseTestException;
    AIndex := 0;
    for I := 0 to AComponentCount - 1 do
    begin
      TdxPDFUtils.AddRange(TdxPDFRange.Create(TdxPDFUtils.ConvertToDouble(AArray[AIndex]),
        TdxPDFUtils.ConvertToDouble(AArray[AIndex + 1])) , FDecodeRanges);
      Inc(AIndex, 2);
    end;
  end;
end;

procedure TdxPDFDocumentImage.ReadMask(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  ASourceObject: TdxPDFBase;
begin
  if not HasMask and ADictionary.TryGetObject(TdxPDFKeywords.Mask, ASourceObject) then
    if ASourceObject.ObjectType = otArray then
    begin
      AArray := ASourceObject as TdxPDFArray;
      if (FBitsPerComponent = 0) or (FComponentCount = 0) or (AArray.Count <> FComponentCount * 2) then
        TdxPDFUtils.RaiseTestException;
      ReadColorKeyMask(AArray);
    end
    else
    begin
      if ASourceObject.ObjectType = otIndirectReference then
        ASourceObject := Repository.GetObject(TdxPDFReference(ASourceObject).Number) as TdxPDFBase;
      Mask := TdxPDFXObject.Parse(Repository, ASourceObject as TdxPDFStream, TdxPDFDocumentImage.GetTypeName) as TdxPDFDocumentImage;
    end;
end;

procedure TdxPDFDocumentImage.ReadMatte(ADictionary: TdxPDFDictionary);
var
  I: Integer;
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.Matte);
  if not HasMask and (AArray <> nil) then
  begin
    SetLength(FMatte, AArray.Count);
    for I := 0 to AArray.Count - 1 do
      FMatte[I] := TdxPDFUtils.ConvertToDouble(AArray[I]);
  end;
end;

procedure TdxPDFDocumentImage.ReadSoftMask(ADictionary: TdxPDFReaderDictionary);
var
  ASoftMaskObject: TdxPDFBase;
  AStream: TdxPDFStream;
begin
  if ADictionary.TryGetObject(TdxPDFKeywords.SoftMask, ASoftMaskObject) then
  begin
    case ASoftMaskObject.ObjectType of
      otIndirectReference:
        AStream := Repository.GetStream((ASoftMaskObject as TdxPDFReference).Number);
      otStream:
        AStream := ASoftMaskObject as TdxPDFStream;
    else
      AStream := nil;
    end;
    if AStream <> nil then
    begin
      SoftMask := TdxPDFXObject.Parse(Repository, AStream, TdxPDFDocumentImage.GetTypeName) as TdxPDFDocumentImage;
      if (SoftMask = nil) or (FComponentCount = 0) or (SoftMask.ColorSpace = nil) or not
        (SoftMask.ColorSpace is TdxPDFGrayDeviceColorSpace) or (SoftMask.Matte <> nil) and
        (Length(SoftMask.Matte) <> FComponentCount) then
        TdxPDFUtils.RaiseTestException(ClassName + ': Error reading soft mask');
    end;
  end;
end;

function TdxPDFDocumentImage.HasValidStencilMask: Boolean;
begin
   Result := (FMask <> nil) and (FMask.BitsPerComponent = 1) and ((FMask.ColorSpace = nil) or
     (FMask.ColorSpace.ComponentCount = 1));
end;

function TdxPDFDocumentImage.ToRGB(const AData: TBytes; APixelFormat: TdxPDFPixelFormat; AWidth, AHeight: Integer): TBytes;
var
  AZero, AOne, B, AMask, AComponent: Byte;
  ADataLength, Y, ASrc, ADest, X, ASize, I, AIndex: Integer;
  ARGBData: TBytes;
begin
  AZero := 0;
  AOne := 255;
  case APixelFormat of
    pfGray1bit:
      begin
        ADataLength := Length(AData);
        SetLength(ARGBData, AWidth * AHeight * 3);
        ASrc := 0;
        ADest := 0;
        for Y := 0 to AHeight - 1 do
        begin
          B := AData[ASrc];
          AMask := $80;
          for X := 0 to AWidth - 1 do
          begin
            AComponent := IfThen((B and AMask) = 0, AZero, AOne);
            ARGBData[ADest] := AComponent;
            ARGBData[ADest + 1] := AComponent;
            ARGBData[ADest + 2] := AComponent;
            Inc(ADest, 3);
            AMask := AMask shr 1;
            if AMask = 0 then
            begin
              AMask := $80;
              Inc(ASrc);
              if ASrc >= ADataLength then
                Break;
              B := AData[ASrc];
            end;
          end;
          if AMask <> $80 then
            Inc(ASrc);
        end;
        Result := ARGBData;
      end;
    pfGray8bit:
      begin
        ASize := Length(AData);
        SetLength(ARGBData, ASize * 3);
        AIndex := 0;
        for I := 0 to ASize - 1 do
        begin
          B := AData[I];
          ARGBData[AIndex] := B;
          ARGBData[AIndex + 1] := B;
          ARGBData[AIndex + 2] := B;
          Inc(AIndex, 3);
        end;
        Result := ARGBData;
      end;
    else
      Result := AData;
  end;
end;

{ TdxPDFDocumentImageData }

constructor TdxPDFDocumentImageData.Create;
begin
  inherited Create;
  FPalette := TdxPDFReferencedObjects.Create;
end;

destructor TdxPDFDocumentImageData.Destroy;
begin
  if Assigned(FBitmap) then
    GdipCheck(GdipDisposeImage(FBitmap));
  FreeAndNil(FPalette);
  inherited Destroy;
end;

function TdxPDFDocumentImageData.Clone: TdxPDFDocumentImageData;
begin
  Result := TdxPDFDocumentImageData.Create;
  Result.Data := Data;
  Result.Width := Width;
  Result.Height := Height;
  Result.Stride := Stride;
  Result.PixelFormat := PixelFormat;
  Result.Palette.AddRange(Palette);
end;

procedure TdxPDFDocumentImageData.Assign(AImageData: TdxPDFDocumentImageData);
begin
  SetLength(FData, Length(AImageData.Data));
  TdxPDFUtils.CopyData(AImageData.Data, 0, FData, 0, Length(AImageData.Data));
  Width := AImageData.Width;
  Height := AImageData.Height;
  Stride := AImageData.Stride;
  PixelFormat := AImageData.PixelFormat;
  Palette.Clear;
  if AImageData.Palette <> nil then
    Palette.AddRange(AImageData.Palette);
end;

procedure TdxPDFDocumentImageData.CalculateStride(AWidth, AComponentCount: Integer);
var
  ATemp, ARemain: Integer;
begin
  ATemp := AWidth * AComponentCount;
  ARemain := ATemp mod 4;
  Stride := IfThen(ARemain > 0, ATemp + 4 - ARemain, ATemp);
end;

procedure TdxPDFDocumentImageData.PopulateData(const ASourceData: TBytes; AWidth, ASourceStride, AComponentCount: Integer);
var
  AComponents: TBytes;
  ARowIndex, ACurrentSourceIndex, ACurrentDestinationIndex, AColumnIndex, ADestinationIndex, ASourceIndex, I: Integer;
begin
  ACurrentSourceIndex := 0;
  ACurrentDestinationIndex := 0;
  SetLength(AComponents, AComponentCount);
  SetLength(FData, Stride * Height);
  for ARowIndex := 0 to Height - 1 do
  begin
    ASourceIndex := ACurrentSourceIndex;
    ADestinationIndex := ACurrentDestinationIndex;
    for AColumnIndex := 0 to AWidth - 1 do
    begin
      TdxPDFUtils.CopyData(ASourceData, ASourceIndex, AComponents, 0, AComponentCount);
      Inc(ASourceIndex, AComponentCount);
      for I := AComponentCount - 1 downto 0 do
      begin
        FData[ADestinationIndex] := AComponents[I];
        Inc(ADestinationIndex);
      end;
    end;
    Inc(ACurrentSourceIndex, ASourceStride);
    Inc(ACurrentDestinationIndex, Stride);
  end;
end;

procedure TdxPDFDocumentImageData.CalculateParameters(out AComponentCount, ASourceStride, AActualWidth: Integer);
var
  I: Integer;
begin
  Palette.Clear;
  AComponentCount := 1;
  case PixelFormat of
    pfGray1bit:
      begin
        AActualWidth := Width div 8;
        if Width mod 8 > 0 then
          Inc(AActualWidth);
        ASourceStride := AActualWidth;
        Palette.Add(TdxPDFARGBColor.CreateFromRGB(0, 0, 0, 255));
        Palette.Add(TdxPDFARGBColor.CreateFromRGB(255, 255, 255, 255));
      end;
    pfGray8bit:
      begin
        AActualWidth := Width;
        ASourceStride := AActualWidth;
        for I := 0 to 256 - 1 do
          Palette.Add(TdxPDFARGBColor.CreateFromRGB(I, I, I, 255));
      end;
  else
    AComponentCount := 3;
    AActualWidth := Width;
    ASourceStride := AActualWidth * 3;
  end;
  CalculateStride(AActualWidth, AComponentCount);
end;

{ TdxPDFCustomEncoding }

class function TdxPDFCustomEncoding.GetTypeName: string;
begin
  Result := '';
end;

procedure TdxPDFCustomEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFontFileEncoding := GetFontFileEncodingClass.Create;
end;

procedure TdxPDFCustomEncoding.DestroySubClasses;
begin
  FreeAndNil(FFontFileEncoding);
  inherited DestroySubClasses;
end;

function TdxPDFCustomEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileCustomEncoding;
end;

{ TdxPDFCustomFont }

constructor TdxPDFCustomFont.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FID := TdxPDFUtils.GenerateGUID;
end;

constructor TdxPDFCustomFont.Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor);
begin
  Create(nil);
  FBaseFont := ABaseFont;
  ReadFontName;
  FontDescriptor := AFontDescriptor;
end;

class function TdxPDFCustomFont.Parse(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont;
var
  AType, ASubtype, ABaseFont: string;
  AClass: TdxPDFCustomFontClass;
  AFontDictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if (ADictionary <> nil) and not ADictionary.Repository.FontDataStorage.TryGetValue(ADictionary.Number, Result) then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    ASubtype := ADictionary.GetString(TdxPDFKeywords.Subtype);
    ABaseFont := ADictionary.GetString(TdxPDFKeywords.BaseFont);
    AClass := nil;
    if (AType = '') and (ASubtype = '') and (ABaseFont = '') then
      AClass := nil
    else
      if (AType <> '') and (AType <> GetTypeName) or (ASubtype <> TdxPDFType3Font.GetSubTypeName) and (ABaseFont = '') then
        TdxPDFUtils.RaiseTestException
      else
      begin
        if (ASubtype = TdxPDFCompositeFont.GetSubTypeName) and (ASubtype <> '') then
        begin
          AFontDictionary := TdxPDFCompositeFont.GetDictionary(ADictionary);
          if AFontDictionary <> nil then
            ASubtype := AFontDictionary.GetString(TdxPDFKeywords.Subtype)
          else
            ASubtype := '';
        end;
        if not dxPDFFontFactory.TryGetClass(ASubtype, AClass) then
        begin
          if ASubtype = '' then
            AClass := TdxPDFUnknownFont
          else
            AClass := nil;
        end;
      end;
    if AClass <> nil then
    begin
      Result := AClass.Create(nil);
      Result.BaseFont := ABaseFont;
      Result.Number := ADictionary.Number;
      Result.Read(ADictionary);
      ADictionary.Repository.FontDataStorage.Add(Result);
    end;
  end;
end;

procedure TdxPDFCustomFont.UpdateGlyphs(var AGlyphs: TSmallIntDynArray);
begin
// do nothing
end;

class function TdxPDFCustomFont.GetTypeName: string;
begin
  Result := 'Font';
end;

procedure TdxPDFCustomFont.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FEncoding := nil;
  FCharacterMapping := nil;
  FFontDescriptor := TdxPDFFontDescriptor.Create;
  FWidths := TDictionary<Integer, Double>.Create;
  FListeners := TInterfaceList.Create;
end;

procedure TdxPDFCustomFont.DestroySubClasses;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxPDFDocumentSharedObjectListener).DestroyHandler(Self);
  FreeAndNil(FListeners);
  FreeAndNil(FWidths);
  FreeAndNil(FCharacterMapping);
  Encoding := nil;
  FontDescriptor := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomFont.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadFontName;
  ReadEncoding(ADictionary.GetObject(TdxPDFKeywords.Encoding));
  ReadWidths(GetFontDictionary(ADictionary));
  ReadToUnicode(ADictionary);
  ReadFontDescriptor(GetFontDescriptorDictionary(ADictionary));
end;

procedure TdxPDFCustomFont.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FBaseFont := ADictionary.GetString(TdxPDFKeywords.BaseFont);
end;

function TdxPDFCustomFont.GetFontDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
begin
  Result := ADictionary;
end;

function TdxPDFCustomFont.GetFontDescriptorDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
begin
  Result := ADictionary.GetDictionary(TdxPDFKeywords.FontDescriptor);
end;

class function TdxPDFCustomFont.GetSubTypeName: string;
begin
  Result := '';
end;

function TdxPDFCustomFont.GetHeightFactor: Double;
begin
  Result := GetWidthFactor;
end;

function TdxPDFCustomFont.GetUseGlyphIndexes: Boolean;
begin
  Result := False;
end;

function TdxPDFCustomFont.GetWidthFactor: Double;
begin
  Result := 0.001;
end;

procedure TdxPDFCustomFont.ReadEncoding(ASourceObject: TdxPDFBase);
begin
  Encoding := nil;
end;

procedure TdxPDFCustomFont.ReadFontDescriptor(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
    FontDescriptor.Read(ADictionary);
end;

procedure TdxPDFCustomFont.ReadToUnicode(ADictionary: TdxPDFReaderDictionary);
var
  ASourceObject: TdxPDFBase;
begin
  ASourceObject := ADictionary.GetObject(TdxPDFKeywords.ToUnicode);
  if (ASourceObject <> nil) and (ASourceObject.ObjectType <> otName) then
    case ASourceObject.ObjectType of
      otStream:
        try
          FCharacterMapping := TdxPDFCMapStreamParser.Parse(Repository, (ASourceObject as TdxPDFStream).UncompressedData);
        except
        end;
      otIndirectReference:
        begin
          try
            ASourceObject := Repository.GetObject(ASourceObject.Number) as TdxPDFBase;
            FCharacterMapping := TdxPDFCMapStreamParser.Parse(Repository, (ASourceObject as TdxPDFStream).UncompressedData);
          except
          end;
        end;
    end;
end;

procedure TdxPDFCustomFont.ReadWidths(ADictionary: TdxPDFReaderDictionary);
begin
// do nothing
end;

procedure TdxPDFCustomFont.SetActualCompactFontFileData(const AValue: TBytes);
begin
// do nothing
end;

function TdxPDFCustomFont.GetStream(const AKey: string; ADictionary: TdxPDFReaderDictionary): TdxPDFStream;
var
  AObject: TdxPDFBase;
begin
  Result := nil;
  AObject := ADictionary.GetObject(AKey);
  if AObject <> nil then
    case AObject.ObjectType of
      otStream:
        Result := TdxPDFStream(AObject);
      otIndirectReference:
        Result := Repository.GetStream(AObject.Number);
    end;
end;

function TdxPDFCustomFont.ReadOpenTypeFontFileData(ADictionary: TdxPDFReaderDictionary;
  ASuppressException: Boolean): TBytes;
var
  AStream: TdxPDFStream;
begin
  SetLength(Result, 0);
  AStream := GetStream(TdxPDFKeywords.FontFile3, ADictionary);
  if AStream = nil then
    Exit(nil);
  if (AStream.Dictionary.GetString(TdxPDFKeywords.Subtype) = TdxPDFKeywords.OpenTypeFont) then
    Result := AStream.UncompressedData
  else
   if not ASuppressException then
      TdxPDFUtils.RaiseTestException('TdxPDFType1Font.ReadOpenTypeFontFileData');
end;

procedure TdxPDFCustomFont.AddListener(AListener: IdxPDFDocumentSharedObjectListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TdxPDFCustomFont.AddWidth(AKey: Integer; AWidth: Double);
begin
  if not FWidths.ContainsKey(AKey) then
    FWidths.Add(AKey, AWidth);
end;

procedure TdxPDFCustomFont.GenerateRegistrationName;
begin
  FRegistrationName := FID;
end;

procedure TdxPDFCustomFont.RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);
begin
  FListeners.Remove(AListener);
end;

function TdxPDFCustomFont.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxPDFCustomFont._AddRef: Integer;
begin
  Result := -1;
end;

function TdxPDFCustomFont._Release: Integer;
begin
  Result := -1;
end;

function TdxPDFCustomFont.GetCharset: TDictionary<SmallInt, SmallInt>;
begin
  Result := nil;
end;

function TdxPDFCustomFont.GetGlyphCount: Integer;
var
  ACharset: TDictionary<SmallInt, SmallInt>;
begin
  ACharset := GetCharset;
  if ACharset = nil then
    Result := 1
  else
    Result := ACharset.Count + 1;
end;

function TdxPDFCustomFont.GetActualCompactFontFileData: TBytes;
begin
  Result := nil;
end;

function TdxPDFCustomFont.GetCompactFontFileData: TBytes;
begin
  Result := nil;
end;

function TdxPDFCustomFont.GetBaseFontName: string;
begin
  Result := BaseFont;
end;

function TdxPDFCustomFont.GetFontDescriptor: TdxPDFFontDescriptor;
begin
  Result := FontDescriptor;
end;

function TdxPDFCustomFont.GetFontName: string;
begin
  Result := Name;
end;

function TdxPDFCustomFont.GetForceBold: Boolean;
begin
  Result := (FontDescriptor <> nil) and ((Integer(FontDescriptor.Flags) and  Integer(ffForceBold)) = Integer(ffForceBold));
end;

function TdxPDFCustomFont.GetItalic: Boolean;
begin
  Result := (FontDescriptor <> nil) and (FontDescriptor.ItalicAngle <> 0);
end;

function TdxPDFCustomFont.GetPitchAndFamily: Byte;
begin
  Result := VARIABLE_PITCH;
  if FontDescriptor <> nil then
    if (Integer(FontDescriptor.Flags) and Integer(ffFixedPitch)) = Integer(ffFixedPitch) then
      Result := FIXED_PITCH;
  if (Integer(FontDescriptor.Flags) and Integer(ffSerif)) > 0 then
    Result := Result or FF_ROMAN;
  if (Integer(FontDescriptor.Flags) and Integer(ffScript)) > 0 then
    Result := Result or FF_SCRIPT;
end;

function TdxPDFCustomFont.GetTrueTypeFontFileData: TBytes;
begin
  Result := nil;
end;

function TdxPDFCustomFont.GetType1FontFileData: TdxPDFType1FontFileData;
begin
  Result := nil;
end;

function TdxPDFCustomFont.GetRegistrationName: string;
begin
  if FRegistrationName = '' then
    Result := BaseFont
  else
    Result := FRegistrationName;
end;

function TdxPDFCustomFont.GetWeight: Integer;
begin
  if FontDescriptor = nil then
    Result := 400
  else
    if (Integer(FontDescriptor.Flags) and Integer(ffForceBold)) = Integer(ffForceBold) then
      Result := 700
    else
      Result := FontDescriptor.FontWeight;
end;

procedure TdxPDFCustomFont.SetEncoding(const AValue: TdxPDFCustomEncoding);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FEncoding));
end;

procedure TdxPDFCustomFont.SetFontDescriptor(const AValue: TdxPDFFontDescriptor);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFontDescriptor));
end;

function TdxPDFCustomFont.GetBoldWeight: Integer;
begin
  Result := 700;
end;

function TdxPDFCustomFont.GetCharacterMapping: TdxPDFCharacterMapping;
begin
  Result := FCharacterMapping;
end;

function TdxPDFCustomFont.GetID: string;
begin
  Result := FID;
end;

function TdxPDFCustomFont.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxPDFCustomFont.GetFontBBox: TdxRectF;
begin
  Result := FFontDescriptor.FontBBox;
end;

function TdxPDFCustomFont.GetShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := (FEncoding <> nil) and FEncoding.ShouldUseEmbeddedFontEncoding;
end;

function TdxPDFCustomFont.GetSubsetNameLength: Integer;
begin
  Result := 6;
end;

function TdxPDFCustomFont.GetSubsetPrefixLength: Integer;
begin
  Result := SubsetNameLength + 1;
end;

function TdxPDFCustomFont.GetWidthToHeightFactor: Double;
var
  ASum, AWidth: Double;
  ACount: Integer;
begin
  if FontDescriptor = nil then Exit(0);
  if FAverageWidth = 0 then
  begin
    FAverageWidth := FontDescriptor.AvgWidth;
    if FAverageWidth = 0 then
    begin
      ASum := 0.0;
      ACount := 0;
      for AWidth in Widths.Values do
        if AWidth > 0 then
        begin
          ASum := ASum + AWidth;
          Inc(ACount);
        end;
      if ACount > 0 then
        FAverageWidth := ASum / ACount;
    end;
  end;
  if FontDescriptor.Height <> 0 then
    Result := FAverageWidth / FontDescriptor.Height
  else
    Result := FAverageWidth;
end;

procedure TdxPDFCustomFont.SetCharacterMapping(const AValue: TdxPDFCharacterMapping);
begin
  if FCharacterMapping <> nil then
    FreeAndNil(FCharacterMapping);
  FCharacterMapping := AValue;
end;

procedure TdxPDFCustomFont.SetWidths(const AValue: TDictionary<Integer, Double>);
begin
  if (FWidths <> nil) and (FWidths <> AValue) then
    FreeAndNil(FWidths);
  FWidths := AValue;
end;

procedure TdxPDFCustomFont.ReadFontName;
var
  I: Integer;
begin
  if (Length(BaseFont) >= SubsetPrefixLength) and (BaseFont[SubsetNameLength + 1] = '+') then
  begin
    FSubsetName := Copy(BaseFont, 1, SubsetNameLength);
    for I := 1 to Length(FSubsetName) do
      if FSubsetName[I] <> UpperCase(FSubsetName[I]) then
      begin
        FSubsetName := '';
        Break;
      end;
  end;
  if FSubsetName = '' then
    Name := BaseFont
  else
    Name := Copy(BaseFont, SubsetPrefixLength + 1, MaxInt);
end;

{ TdxPDFColorSpaceTransformResult }

function TdxPDFColorSpaceTransformResult.Create(const AData: TBytes): TdxPDFColorSpaceTransformResult;
begin
  Result := Create(AData, nil, pfArgb24bpp);
end;

function TdxPDFColorSpaceTransformResult.Create(const AData: TBytes;
  AFormat: TdxPDFPixelFormat): TdxPDFColorSpaceTransformResult;
begin
  Result := Create(AData, nil, AFormat);
end;

function TdxPDFColorSpaceTransformResult.Create(const AData, AMaskData: TBytes): TdxPDFColorSpaceTransformResult;
begin
  Result := Create(AData, AMaskData, pfArgb24bpp);
end;

function TdxPDFColorSpaceTransformResult.Create(const AData, AMaskData: TBytes;
  AFormat: TdxPDFPixelFormat): TdxPDFColorSpaceTransformResult;
begin
  Result.Data := AData;
  Result.PixelFormat := AFormat;
  Result.MaskData := AMaskData;
  Result.IsInvalid := False;
end;

{ TdxPDFCustomColorSpace }

class function TdxPDFCustomColorSpace.CreateColorSpace(const AName: string; AResources: TdxPDFResources): TdxPDFCustomColorSpace;
var
  AObjectClass: TdxPDFObjectClass;
begin
  Result := nil;
  AObjectClass := dxPDFGetDocumentObjectClass(AName);
  if AObjectClass <> nil then
    Result := AObjectClass.Create(nil) as TdxPDFCustomColorSpace
  else
    if (AObjectClass = nil) and (AResources <> nil) then
      Result := AResources.GetColorSpace(AName);
end;

class function TdxPDFCustomColorSpace.Parse(ARepository: TdxPDFDocumentRepository; AObject: TdxPDFBase;
  AResources: TdxPDFResources): TdxPDFCustomColorSpace;
var
  AAttributesDictionary: TdxPDFDictionary;
  AParameters: TdxPDFBase;
  AReference: TdxPDFReference;
  AArray: TdxPDFArray;
  ATempName: string;
  AName: TdxPDFName;
  AObjectClass: TdxPDFObjectClass;
begin
  Result := nil;
  case AObject.ObjectType of
    otIndirectReference:
      begin
        AReference := AObject as TdxPDFReference;
        AParameters := ARepository.GetObject(AReference.Number) as TdxPDFBase;
        Result := Parse(ARepository, AParameters, AResources);
      end;
    otString, otName:
      begin
        Result := CreateColorSpace(TdxPDFName(AObject).Value, AResources);
        if (Result = nil) and (AResources = nil) then
          TdxPDFUtils.RaiseTestException;
      end;
  else
    begin
      AArray := AObject as TdxPDFArray;
      if AArray[0].ObjectType = otName then
      begin
        AName := AArray[0] as TdxPDFName;
        AObjectClass := dxPDFGetDocumentObjectClass(AName.Value);
        if AArray.Count = 5 then
        begin
          if AArray[4].ObjectType <> otDictionary then
          begin
            AAttributesDictionary := ARepository.GetDictionary((AArray[4] as TdxPDFReference).Number);
            if AAttributesDictionary = nil then
              TdxPDFUtils.RaiseTestException;
          end
          else
            AAttributesDictionary := AArray[4] as TdxPDFDictionary;
          ATempName := AAttributesDictionary.GetString(TdxPDFKeywords.Subtype);
          if ATempName = '' then
            AObjectClass := TdxPDFDeviceNColorSpace
          else
            AObjectClass := dxPDFGetDocumentObjectClass(ATempName);
        end;
        if AObjectClass <> nil then
        begin
          Result := AObjectClass.Create(nil) as TdxPDFCustomColorSpace;
          Result.Repository := ARepository;
          Result.DoRead(AArray);
        end
        else
          TdxPDFUtils.RaiseTestException('Unknown color space:' + AName.Value);
      end
      else
        TdxPDFUtils.RaiseTestException;
    end;
  end;
end;

procedure TdxPDFCustomColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
begin
  inherited Read(ADictionary);
  if (ADictionary <> nil) and ADictionary.TryGetArray(Name, AArray) and CanRead(AArray.Count) then
    DoRead(AArray)
  else
    TdxPDFUtils.RaiseTestException;
end;

procedure TdxPDFCustomColorSpace.DoRead(AArray: TdxPDFArray);
begin
// do nothing
end;

class function TdxPDFCustomColorSpace.GetTypeName: string;
begin
  Result := '';
end;

procedure TdxPDFCustomColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  AlternateColorSpace := nil;
end;

procedure TdxPDFCustomColorSpace.DestroySubClasses;
begin
  AlternateColorSpace := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomColorSpace.Initialize;
begin
  inherited Initialize;
  FComponentCount := GetComponentCount;
end;

function TdxPDFCustomColorSpace.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

function TdxPDFCustomColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxPDFCustomColorSpace.CheckComponentCount;
begin
  if not (ComponentCount in [1, 3, 4]) then
    TdxPDFUtils.RaiseTestException('Invalid color space component count');
end;

function TdxPDFCustomColorSpace.CreateDefaultDecodeArray(ABitsPerComponent: Integer): TdxPDFRanges;
var
  I: Integer;
begin
  SetLength(Result, ComponentCount);
  for I := 0 to ComponentCount - 1 do
    Result[I] := TdxPDFRange.Create(0, 1);
end;

function TdxPDFCustomColorSpace.Transform(AColor: TdxPDFColor): TdxPDFColor;
begin
  Result := TdxPDFColor.Create(Transform(AColor.Components));
  Result.Pattern := AColor.Pattern;
end;

function TdxPDFCustomColorSpace.Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
begin
  Result := AComponents;
end;

function TdxPDFCustomColorSpace.Transform(AImage: TdxPDFDocumentImage; const AData: TBytes): TdxPDFColorSpaceTransformResult;
var
  ADecodedData: TBytes;
begin
  if not NeedDataDecoding(AImage.DecodeRanges, AImage.BitsPerComponent) then
    Result := Transform(dxPDFImageInfo(AData, AImage.Width, AImage.Height, AImage.BitsPerComponent, AImage.ColorKeyMask))
  else
  begin
    DecodeData(AData, AImage, ADecodedData);
    Result := Transform(dxPDFImageInfo(ADecodedData, AImage.Width, AImage.Height, AImage.BitsPerComponent, AImage.ColorKeyMask));
  end;
end;

procedure TdxPDFCustomColorSpace.SetAlternateColorSpace(const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAlternateColorSpace));
end;

procedure TdxPDFCustomColorSpace.SetComponentCount(const AValue: Integer);
begin
  if FComponentCount <> AValue then
  begin
    FComponentCount := AValue;
    CheckComponentCount;
  end;
end;

function TdxPDFCustomColorSpace.GetDecodedValue(AValue: Double; const ADecodeArrayEntry: TdxPDFRange): Double;
var
  AMin, AMax: Double;
begin
  AMin := ADecodeArrayEntry.Min;
  AMax := ADecodeArrayEntry.Max;
  if AMin < AMax then
    Result := AMin + (AValue - AMin) / (AMax - AMin)
  else
    Result := 1.0 - AMax - (AValue - AMax) / (AMin - AMax);
  if AValue < 0 then
    Result := 0
  else
    if AValue > 1 then
      Result := 1;
end;

function TdxPDFCustomColorSpace.NeedDataDecoding(const ADecode: TdxPDFRanges; ABitsPerComponent: Integer): Boolean;
var
  I, ALength: Integer;
  AArray: TdxPDFRanges;
begin
  Result := False;
  AArray := CreateDefaultDecodeArray(ABitsPerComponent);
  ALength := Length(AArray);
  if ALength = Length(ADecode) then
    for I := 0 to ALength - 1 do
      if not AArray[I].IsSame(ADecode[I]) then
      begin
        Result := True;
        Break;
      end;
end;

procedure TdxPDFCustomColorSpace.DecodeData(const AData: TBytes; AImage: TdxPDFDocumentImage; out ADecodedData: TBytes);
var
  ACurrentByte, AMask: Byte;
  I, J, ALength, AComponentIndex, AShift: Integer;
  AMaxPossibleValue, AValue: Double;
  ANeedReverting: Boolean;
  ARange: TdxPDFRange;
begin
  AMaxPossibleValue := Power(2, AImage.BitsPerComponent) - 1;
  ALength := Length(AData);
  SetLength(ADecodedData, ALength);
  if AImage.BitsPerComponent = 8 then
  begin
    AComponentIndex := 0;
    for J := 0 to ALength - 1 do
    begin
      ADecodedData[J] := TdxPDFUtils.ConvertToByte(
        GetDecodedValue(AData[J] / AMaxPossibleValue, AImage.DecodeRanges[AComponentIndex]) * AMaxPossibleValue);
      Inc(AComponentIndex);
      if AComponentIndex = ComponentCount then
        AComponentIndex := 0;
    end;
  end
  else
  begin
    if AImage.BitsPerComponent = 1 then
    begin
      ANeedReverting := True;
      for I := 0 to Length(AImage.DecodeRanges) - 1 do
      begin
        ARange := AImage.DecodeRanges[I];
        if ARange.Min < ARange.Max then
        begin
          ANeedReverting := False;
          Break;
        end;
      end;
      if ANeedReverting then
      begin
        for J := 0 to ALength - 1 do
          ADecodedData[J] := AData[J] xor $FF;
        Exit;
      end;
    end;
    AComponentIndex := 0;
    for J := 0 to ALength - 1 do
    begin
      ACurrentByte := 0;
      AMask := $FF - ($FF shr AImage.BitsPerComponent);
      AShift := 8 - AImage.BitsPerComponent;
      while AShift >= 0 do
      begin
        AValue := GetDecodedValue(((AData[J] and AMask) shr AShift) / AMaxPossibleValue,
          AImage.DecodeRanges[AComponentIndex]) * AMaxPossibleValue;
        Inc(ACurrentByte, TdxPDFUtils.ConvertToByte(AValue) shl AShift);
        Inc(AComponentIndex);
        if AComponentIndex = ComponentCount then
          AComponentIndex := 0;
        Dec(AShift, AImage.BitsPerComponent);
        AMask := AMask shr AImage.BitsPerComponent;
      end;
      ADecodedData[J] := ACurrentByte;
    end;
  end;
end;

{ TdxPDFCustomColorSpaceTransformation }

constructor TdxPDFCustomColorSpaceTransformation.Create(const AInfo: TdxPDFImageInfo);
begin
  inherited Create;
  FInfo := AInfo;
end;

function TdxPDFCustomColorSpaceTransformation.UnpackData(const AData: TBytes): TBytes;
var
  AMask: Byte;
  AFactor: Double;
  X, Y, ASourceIndex, ADestinationIndex: Integer;
  AUnpackedDataSize, AStartShift, AShift, AComponent: Integer;
begin
  case Info.BitsPerComponent of
    8:
      Result := AData;
    16:
      begin
        AUnpackedDataSize := Length(AData) div 2;
        SetLength(Result, AUnpackedDataSize);
        X := 0;
        for Y := 0 to AUnpackedDataSize - 1 do
        begin
          Result[Y] := AData[X];
          Inc(X, 2);
        end;
      end;
  else
    AUnpackedDataSize := Info.Width * Info.Height * ComponentCount;
    if Length(AData) * 8 / Info.BitsPerComponent < AUnpackedDataSize then
      Exit(AData);
    SetLength(Result, AUnpackedDataSize);
    AMask := $FF shr (8 - Info.BitsPerComponent);
    AStartShift := 8 - Info.BitsPerComponent;
    AFactor := 255.0 / AMask;
    ASourceIndex := 0;
    ADestinationIndex := 0;
    for Y := 0 to Info.Height - 1 do
    begin
      AShift := AStartShift;
      for X := 0 to Info.Width - 1 do
        for AComponent := 0 to ComponentCount - 1 do
        begin
          if AShift < 0 then
          begin
            AShift := AStartShift;
            Inc(ASourceIndex);
          end;
          Result[ADestinationIndex] := TdxPDFUtils.ConvertToByte(AData[ASourceIndex] shr AShift and AMask * AFactor);
          Dec(AShift, Info.BitsPerComponent);
          Inc(ADestinationIndex);
        end;
      Inc(ASourceIndex);
    end;
  end;
end;

{ TdxPDFDeferredObject }

constructor TdxPDFDeferredObject.Create(AOwner: TdxPDFObject; const AInfo: TdxPDFDeferredObjectInfo);
begin
  inherited Create(AOwner);
  SourceObject := AInfo.SourceObject;
  if AOwner <> nil then
    Repository := AOwner.Repository;
  FInfo.Name := AInfo.Name;
  FInfo.Key := AInfo.Key;
  FInfo.Number := AInfo.Number;
end;

constructor TdxPDFDeferredObject.Create(AOwner, AResolvedObject: TdxPDFObject);
begin
  inherited Create(AOwner);
  ResolvedObject := AResolvedObject;
  FInfo.Number := FResolvedObject.Number;
  FInfo.Key := FResolvedObject.GetTypeName;
end;

class function TdxPDFDeferredObject.GetTypeName: string;
begin
  Result := '';
end;

procedure TdxPDFDeferredObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  SourceObject := nil;
  ResolvedObject := nil;
end;

procedure TdxPDFDeferredObject.DestroySubClasses;
begin
  ResolvedObject := nil;
  SourceObject := nil;
  inherited DestroySubClasses;
end;

function TdxPDFDeferredObject.IsResolved: Boolean;
begin
  Result := FResolvedObject <> nil;
end;

procedure TdxPDFDeferredObject.ResolveObject;
var
  ADictionary: TdxPDFReaderDictionary;
  AIsGroupForm: Boolean;
begin
  ADictionary := nil;
  if SourceObject <> nil then
    case SourceObject.ObjectType of
      otDictionary:
         ADictionary := SourceObject as TdxPDFReaderDictionary;
      otIndirectReference:
         ADictionary := Repository.GetDictionary(SourceObject.Number);
    else
      ADictionary := nil;
    end;
  if FInfo.Key = TdxPDFCustomDestination.GetTypeName then
    ResolvedObject := TdxPDFCustomDestination.Parse(Repository.Catalog, SourceObject)
  else
    if ADictionary <> nil then
    begin
      ADictionary.Number := FInfo.Number;
      if FInfo.Key = TdxPDFColorSpaces.GetTypeName then
        ResolvedObject := TdxPDFCustomColorSpace.Parse(Repository, ADictionary.GetObject(FInfo.Name))
      else
        if FInfo.Key = TdxPDFPatterns.GetTypeName then
          ResolvedObject := TdxPDFCustomPattern.Parse(Repository, ADictionary.GetObject(FInfo.Name))
       else
         if FInfo.Key = TdxPDFShadings.GetTypeName then
           ResolvedObject := TdxPDFCustomShading.Parse(Repository, ADictionary.GetObject(FInfo.Name))
         else
           if FInfo.Key = TdxPDFCustomFont.GetTypeName then
           begin
             ADictionary := ADictionary.GetDictionary(FInfo.Name);
             ADictionary.Number := FInfo.Number;
             if not TdxPDFUtils.IsIntegerValid(ADictionary.Number) then
               TdxPDFUtils.RaiseTestException('');
             ResolvedObject := Repository.CreateFont(Owner as TdxPDFObject, ADictionary);
           end
           else
             if FInfo.Key = TdxPDFGraphicsStateParametersList.GetTypeName then
               ResolvedObject := TdxPDFGraphicsStateParameters.Parse(ADictionary.GetDictionary(FInfo.Name))
             else
               if (ADictionary.GetString(TdxPDFKeywords.Subtype) = TdxPDFForm.GetTypeName) and ADictionary.Contains(TdxPDFGroupForm.GetTypeName) then
               begin
                 AIsGroupForm := ADictionary.Contains('Group');
                 if AIsGroupForm then
                   ResolvedObject := TdxPDFGroupForm.Create(Owner)
                 else
                   ResolvedObject := TdxPDFForm.Create(Owner);
                 ResolvedObject.Read(ADictionary);
               end
               else
                 if ADictionary.GetString(FInfo.Key) = 'Image' then
                   ResolvedObject := Repository.CreateImage(Owner as TdxPDFObject, ADictionary)
                 else
                   if FInfo.Key = TdxPDFFileSpecification.GetTypeName then
                     ResolvedObject := TdxPDFFileSpecification.Parse(ADictionary)
                   else
                   begin
                     ResolvedObject := dxPDFCreateDocumentObject(Owner as TdxPDFObject, ADictionary, FInfo.Key, Repository);
                     if IsResolved then
                       ResolvedObject.Read(ADictionary);
                   end;
    end;
  if IsResolved then
    ResolvedObject.Owner := Owner;
end;

function TdxPDFDeferredObject.GetResolvedObject: TdxPDFObject;
begin
  if FResolvedObject = nil then
    ResolveObject;
  Result := FResolvedObject;
end;

function TdxPDFDeferredObject.GetSourceObject: TdxPDFBase;
begin
  Result := FInfo.SourceObject;
end;

procedure TdxPDFDeferredObject.SetResolvedObject(const AValue: TdxPDFObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResolvedObject));
end;

procedure TdxPDFDeferredObject.SetSourceObject(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FInfo.SourceObject));
end;

{ TdxPDFGraphicsStateParameters }

class function TdxPDFGraphicsStateParameters.Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFGraphicsStateParameters;
begin
  Result := TdxPDFGraphicsStateParameters.Create(nil);
  Result.Read(ADictionary);
end;

procedure TdxPDFGraphicsStateParameters.Assign(AParameters: TdxPDFGraphicsStateParameters;
  ACheckAssignedValues: Boolean = True);
begin
  if not ACheckAssignedValues or (gspStrokingColorAlpha in AParameters.AssignedValue) then
    StrokingColorAlpha := AParameters.StrokingColorAlpha;
  if not ACheckAssignedValues or (gspNonStrokingColorAlpha in AParameters.AssignedValue) then
    NonStrokingColorAlpha := AParameters.NonStrokingColorAlpha;
  if not ACheckAssignedValues or (gspLineCapStyle in AParameters.AssignedValue) then
    LineCapStyle := AParameters.LineCapStyle;
  if not ACheckAssignedValues or (gspLineJoinStyle in AParameters.AssignedValue) then
    LineJoinStyle := AParameters.LineJoinStyle;
  if not ACheckAssignedValues or (gspLineStyle in AParameters.AssignedValue) then
    LineStyle := AParameters.LineStyle;
  if not ACheckAssignedValues or (gspLineWidth in AParameters.AssignedValue) then
    LineWidth := AParameters.LineWidth;
  if not ACheckAssignedValues or (gspMiterLimit in AParameters.AssignedValue) then
    MiterLimit := AParameters.MiterLimit;
  if not ACheckAssignedValues or (gspSmoothnessTolerance in AParameters.AssignedValue) then
    SmoothnessTolerance := AParameters.SmoothnessTolerance;
  if not ACheckAssignedValues or (gspTextKnockout in AParameters.AssignedValue) then
    TextKnockout := AParameters.TextKnockout;
  if not ACheckAssignedValues or (gspRenderingIntent in AParameters.AssignedValue) then
    RenderingIntent := AParameters.RenderingIntent;
  if not ACheckAssignedValues or (gspSoftMask in AParameters.AssignedValue) then
    SoftMask := AParameters.SoftMask;
  if not ACheckAssignedValues or (gspBlendMode in AParameters.AssignedValue) then
    BlendMode := AParameters.BlendMode;
end;

procedure TdxPDFGraphicsStateParameters.SetFont(const AValue: TdxPDFCustomFont);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFont));
end;

procedure TdxPDFGraphicsStateParameters.SetLineStyle(const AValue: TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

procedure TdxPDFGraphicsStateParameters.SetLineWidth(const AValue: Double);
begin
  FLineWidth := AValue;
  Include(FAssignedValues, gspLineWidth);
end;

procedure TdxPDFGraphicsStateParameters.SetSoftMask(const AValue: TdxPDFCustomSoftMask);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FSoftMask));
end;

class function TdxPDFGraphicsStateParameters.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ExtGState;
end;

procedure TdxPDFGraphicsStateParameters.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := nil;
  SoftMask := nil;
end;

procedure TdxPDFGraphicsStateParameters.DestroySubClasses;
begin
  SoftMask := nil;
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFGraphicsStateParameters.Initialize;
begin
  inherited Initialize;
  FFlatnessTolerance := 1.0;
  FLineWidth := 1;
  FLineCapStyle := lcsButt;
  FMiterLimit := 10.0;
  FNonStrokingColorAlpha := 1.0;
  FTextKnockout := True;
  FStrokingColorAlpha := 1.0;
  FSmoothnessTolerance := 0.0;
  FRenderingIntent := riRelativeColorimetric;
  FBlendMode := bmNormal;
end;

procedure TdxPDFGraphicsStateParameters.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FAssignedValues := [];

  if ADictionary.Contains('LC') then
  begin
    LineCapStyle := TdxPDFLineCapStyle(ADictionary.GetInteger('LC'));
    Include(FAssignedValues, gspLineCapStyle);
  end;
  if ADictionary.Contains('LJ') then
  begin
    LineJoinStyle := TdxPDFLineJoinStyle(ADictionary.GetInteger('LJ'));
    Include(FAssignedValues, gspLineJoinStyle);
  end;
  if ADictionary.Contains('D') then
  begin
    LineStyle := TdxPDFLineStyle.Create(ADictionary.GetArray('D'));
    Include(FAssignedValues, gspLineStyle);
  end;
  if ADictionary.Contains('LW') then
  begin
    LineWidth := ADictionary.GetInteger('LW');
    Include(FAssignedValues, gspLineWidth);
  end;
  if ADictionary.Contains('FL') then
  begin
    FlatnessTolerance := ADictionary.GetDouble('FL');
    Include(FAssignedValues, gspFlatnessTolerance);
  end;
  if ADictionary.Contains('SM') then
  begin
    SmoothnessTolerance := ADictionary.GetDouble('SM');
    Include(FAssignedValues, gspSmoothnessTolerance);
  end;
  if ADictionary.Contains('CA') then
  begin
    StrokingColorAlpha := ADictionary.GetDouble('CA');
    Include(FAssignedValues, gspStrokingColorAlpha);
  end;
  if ADictionary.Contains('ca') then
  begin
    NonStrokingColorAlpha := ADictionary.GetDouble('ca');
    Include(FAssignedValues, gspNonStrokingColorAlpha);
  end;
  if ADictionary.Contains('ML') then
  begin
    MiterLimit := ADictionary.GetInteger('ML');
    Include(FAssignedValues, gspMiterLimit);
  end;
  if ADictionary.Contains('TK') then
  begin
    TextKnockout := ADictionary.GetBoolean('TK');
    Include(FAssignedValues, gspTextKnockout);
  end;
  if ADictionary.Contains('TK') then
  begin
    TextKnockout := ADictionary.GetBoolean('TK');
    Include(FAssignedValues, gspTextKnockout);
  end;
  if ADictionary.Contains('BM') then
  begin
    if not dxPDFBlendModeDictionary.TryGetValue(ADictionary.GetString('BM'), FBlendMode) then
      BlendMode := bmNormal;
    Include(FAssignedValues, gspBlendMode);
  end;
  if ADictionary.Contains('SMask') then
  begin
    SoftMask := TdxPDFCustomSoftMask.Parse(Repository, ADictionary.GetObject('SMask'));
    Include(FAssignedValues, gspSoftMask);
  end;
end;

{ TdxPDFObjectList }

function TdxPDFObjectList.Add(AObject: TdxPDFObject): string;
begin
  Result := '';
  if not FNames.ContainsValue(AObject.Number) then
  begin
    Result := FNames.GetNewResourceName(InternalObjects);
    InternalAdd(Result, AObject);
  end;
end;

function TdxPDFObjectList.AddReference(ANumber: Integer): string;
begin
  Result := FNames.GetNewResourceName(InternalObjects);
  InternalObjects.Add(Result, TdxPDFReference.Create(ANumber, 0));
end;

function TdxPDFObjectList.Contains(const AName: string): Boolean;
begin
  Result := InternalObjects.ContainsKey(AName);
end;

procedure TdxPDFObjectList.Append(AList: TdxPDFObjectList);
begin
  DoAdd(AList, False);
end;

procedure TdxPDFObjectList.Assign(AList: TdxPDFObjectList);
begin
  if AList is TdxPDFObjectList then
    DoAdd(AList, True);
end;

procedure TdxPDFObjectList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FInternalObjects := TdxPDFReferencedObjectDictionary.Create;
  FNames := TdxPDFNamedObjectDictionary.Create(GetTypeName, GetTypePrefix);
end;

procedure TdxPDFObjectList.DestroySubClasses;
begin
  FreeAndNil(FNames);
  FreeAndNil(FInternalObjects);
  inherited DestroySubClasses;
end;

procedure TdxPDFObjectList.Read(ADictionary: TdxPDFReaderDictionary);
var
  AName: string;
  ANumber: Integer;
begin
  inherited Read(ADictionary);
  Clear;
  if ADictionary <> nil then
    for AName in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      ANumber := ADictionary.GetObjectNumber(AName);
      if TdxPDFUtils.IsIntegerValid(ANumber) then
        ReadObject(ADictionary.GetObject(AName), ANumber, AName);
    end;
end;

function TdxPDFObjectList.GetObject(const AName: string): TdxPDFObject;
var
  AResult: TdxPDFReferencedObject;
begin
  if InternalObjects.TryGetValue(AName, AResult) then
  begin
    if AResult is TdxPDFDeferredObject then
      Result := TdxPDFDeferredObject(AResult).ResolvedObject
    else
      Result := AResult as TdxPDFObject
  end
  else
    Result := nil;
end;

class function TdxPDFObjectList.GetTypePrefix: string;
begin
  Result:= 'ResourcePrefix';
end;

function TdxPDFObjectList.GetTypeDictionaryKey: string;
begin
  Result := TdxPDFKeywords.Subtype;
end;

procedure TdxPDFObjectList.ReadObject(AStructureObject: TdxPDFBase; ANumber: Integer; const AName: string);
var
  AShareObject: TdxPDFBase;
begin
  AShareObject := AStructureObject;
  AShareObject.Number := ANumber;
  AStructureObject := Repository.GetDictionary(AStructureObject.Number);
  if (AStructureObject = nil) and (AShareObject.ObjectType = otDictionary) then
  begin
    if dxPDFIsObjectSupported(Self, AShareObject as TdxPDFDictionary, GetTypeDictionaryKey, Repository) then
      DoReadObject(AName, AShareObject as TdxPDFReaderDictionary);
  end
  else
    if AStructureObject <> nil then
    begin
      AStructureObject.Number := AShareObject.Number;
      DoReadObject(AName, AStructureObject as TdxPDFReaderDictionary);
    end;
end;

function TdxPDFObjectList.GetCount: Integer;
begin
  Result := InternalObjects.Count;
end;

procedure TdxPDFObjectList.DoAdd(AObjectList: TdxPDFObjectList; ANeedClear: Boolean);
var
  APair: TPair<string, TdxPDFReferencedObject>;
begin
  if AObjectList is TdxPDFObjectList then
  begin
    if ANeedClear then
      Clear;
    for APair in TdxPDFReferencedObjectDictionaryAccess(AObjectList.InternalObjects).Items do
    begin
      if InternalObjects.ContainsKey(APair.Key) then
        InternalObjects.Remove(APair.Key);
      InternalObjects.Add(APair.Key, APair.Value);
    end;
  end;
end;

procedure TdxPDFObjectList.DoReadObject(const AObjectName: string; ADictionary: TdxPDFReaderDictionary);
var
  AInfo: TdxPDFDeferredObjectInfo;
  AItem: TdxPDFDeferredObject;
begin
  AInfo.Name := AObjectName;
  AInfo.Key := GetTypeDictionaryKey;
  AInfo.Number := ADictionary.Number;
  AInfo.SourceObject := ADictionary;
  AItem := TdxPDFDeferredObject.Create(Owner as TdxPDFObject, AInfo);
  InternalAdd(AObjectName, AItem);
end;

procedure TdxPDFObjectList.Clear;
begin
  InternalObjects.Clear;
end;

procedure TdxPDFObjectList.InternalAdd(const AName: string; AObject: TdxPDFObject);
begin
  InternalObjects.Add(AName, AObject);
end;

procedure TdxPDFObjectList.ReadList(ADictionary: TdxPDFReaderDictionary);
var
  AKey: string;
  AInfo: TdxPDFDeferredObjectInfo;
begin
  if ADictionary <> nil then
  begin
    AInfo.Key := GetTypeName;
    AInfo.SourceObject := ADictionary;
    AInfo.Number := ADictionary.Number;
    for AKey in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      AInfo.Name := AKey;
      AInfo.Number := (TdxPDFDictionaryAccess(ADictionary).Items[AKey] as TdxPDFBase).Number;
      InternalAdd(AKey, TdxPDFDeferredObject.Create(Owner as TdxPDFObject, AInfo));
    end;
  end;
end;

{ TdxPDFDocumentImageDataStorage }

constructor TdxPDFDocumentImageDataStorage.Create(ALimit: Int64);
begin
  inherited Create;
  FReferences := TdxPDFUniqueReferences.Create;
  FDictionary := TObjectDictionary<TdxPDFDocumentImage, TdxPDFDocumentImageData>.Create([doOwnsValues]);
  FQueue := TList<TdxPDFDocumentImage>.Create;
  FLimit := ALimit * 1024 * 1024;
end;

destructor TdxPDFDocumentImageDataStorage.Destroy;
begin
  Clear;
  FreeAndNil(FQueue);
  FreeAndNil(FDictionary);
  FreeAndNil(FReferences);
  inherited Destroy;
end;

procedure TdxPDFDocumentImageDataStorage.Clear;
var
  AImage: TdxPDFDocumentImage;
begin
  for AImage in FDictionary.Keys do
    RemoveListener(AImage);
  FDictionary.Clear;
  FQueue.Clear;
  FReferences.Clear;
  FSize := 0;
end;

procedure TdxPDFDocumentImageDataStorage.CheckCapacity;
var
  AFirst: TdxPDFDocumentImage;
  AFistData: TdxPDFDocumentImageData;
begin
  if FLimit > 0 then
    while FSize > FLimit do
    begin
      AFirst := FQueue.First;
      if FDictionary.ContainsKey(AFirst) then
      begin
        AFistData := FDictionary[AFirst];
        Dec(FSize, GetValueSize(AFistData));
      end;
      Remove(AFirst);
    end;
end;

procedure TdxPDFDocumentImageDataStorage.Remove(AImage: TdxPDFDocumentImage);
begin
  if FDictionary.ContainsKey(AImage) then
  begin
    RemoveListener(AImage);
    FQueue.Remove(AImage);
    FDictionary.Remove(AImage);
    FReferences.Remove(AImage.GUID);
  end;
end;

function TdxPDFDocumentImageDataStorage.GetValueSize(AValue: TdxPDFDocumentImageData): Int64;
begin
  if (AValue = nil) or (AValue.Data = nil) or (Length(AValue.Data) = 0) then
    Result := 0
  else
    Result := Length(AValue.Data);
end;

function TdxPDFDocumentImageDataStorage.Add(AImage: TdxPDFDocumentImage): TdxPDFDocumentImageData;
begin
  AImage.AddListener(Self);
  Result := AImage.CreateImageData;
  Inc(FSize, GetValueSize(Result));
  FDictionary.Add(AImage, Result);
  FQueue.Add(AImage);
  AddReference(AImage);
  CheckCapacity;
end;

function TdxPDFDocumentImageDataStorage.TryGetReference(ANumber: Integer; out AImage: TdxPDFDocumentImage): Boolean;
var
  AObject: TdxPDFBase;
begin
  AImage := nil;
  Result := FReferences.TryGetValue(ANumber, AObject);
  if Result then
    AImage := AObject as TdxPDFDocumentImage;
end;

procedure TdxPDFDocumentImageDataStorage.AddReference(AImage: TdxPDFDocumentImage);
begin
  if not FReferences.ContainsKey(AImage.GUID, AImage.Number) then
    FReferences.Add(AImage.GUID, AImage.Number, AImage);
end;

function TdxPDFDocumentImageDataStorage.GetImageData(AImage: TdxPDFDocumentImage): TdxPDFDocumentImageData;
begin
  if not FDictionary.TryGetValue(AImage, Result) then
    Result := Add(AImage)
end;

procedure TdxPDFDocumentImageDataStorage.RemoveListener(AImage: TdxPDFDocumentImage);
begin
  AImage.RemoveListener(Self);
end;

procedure TdxPDFDocumentImageDataStorage.ImageDestroyHandler(Sender: TdxPDFBase);
begin
  FDictionary.Remove(Sender as TdxPDFDocumentImage);
end;

{ TdxPDFFontDataStorage }

constructor TdxPDFFontDataStorage.Create(const ATempFolder: string);
begin
  inherited Create;
  FFolderName := ATempFolder;
  FDictionary := TDictionary<TdxPDFCustomFont, TdxPDFFontRegistrationData>.Create;
  FReferences := TdxPDFUniqueReferences.Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FFontCache := TdxPDFGDIEditableFontDataCache.Create(nil);
  FQueue := TList<TdxPDFCustomFont>.Create;
end;

destructor TdxPDFFontDataStorage.Destroy;
begin
  Clear;
  FreeAndNil(FQueue);
  FreeAndNil(FFontCache);
  FreeAndNil(FReferences);
  FreeAndNil(FDictionary);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxPDFFontDataStorage.Add(AFont: TdxPDFCustomFont): TdxPDFFontRegistrationData;
var
  ARegistrator: TdxPDFFontCustomRegistrator;
begin
  EnterCriticalSection(FLock);
  try
    if AFont = FLastRegisteredFont then
      Result := FLastRegisteredFontData
    else
    begin
      FLastRegisteredFont := AFont;
      if FReferences.ContainsKey(AFont.ID, AFont.Number) and FDictionary.TryGetValue(AFont, FLastRegisteredFontData) then
        Exit(FLastRegisteredFontData);
      FLastRegisteredFont := AFont;
      ARegistrator := TdxPDFFontCustomRegistrator.CreateRegistrator(AFont, FFolderName);
      if ARegistrator <> nil then
      begin
        FLastRegisteredFontData := ARegistrator.Register;
        if FLastRegisteredFontData.Registrator = nil then
          ARegistrator.Free;
      end
      else
        FLastRegisteredFontData := TdxPDFFontRegistrationData.Create(AFont.GetFontName, 0, AFont.GetWeight, AFont.GetItalic,
          AFont.GetPitchAndFamily, False, nil, AFont is TdxPDFType3Font);
      if not FReferences.ContainsKey(AFont.ID, AFont.Number) then
        InternalAdd(AFont)
      else
        TdxPDFUtils.RaiseTestException;
      Result := FLastRegisteredFontData;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFFontDataStorage.CreateSubstituteFontData(AFont: TdxPDFCustomFont): TdxPDFFontRegistrationData;
var
  AFontRegistrator: TdxPDFFontCustomRegistrator;
begin
  AFontRegistrator := TdxPDFFontCustomRegistrator.CreateRegistrator(AFont, FFolderName);
  try
    Result := AFontRegistrator.CreateSubstituteFontData;
  finally
    AFontRegistrator.Free;
  end;
end;

function TdxPDFFontDataStorage.SearchFontData(const AFontFamilyName: string; AFontStyle: TdxGPFontStyle): TObject;
begin
  Result := (FFontCache as TdxPDFGDIEditableFontDataCache).SearchFontData(AFontFamilyName, AFontStyle);
end;

function TdxPDFFontDataStorage.TryGetValue(ANumber: Integer; out AFont: TdxPDFCustomFont): Boolean;
var
  AObject: TdxPDFBase;
begin
  AFont := nil;
  Result :=  FReferences.TryGetValue(ANumber, AObject);
  if Result then
    AFont := AObject as TdxPDFCustomFont;
end;

procedure TdxPDFFontDataStorage.Clear;
var
  AData: TdxPDFFontRegistrationData;
  AFont: TdxPDFCustomFont;
begin
  FQueue.Clear;
  for AFont in FDictionary.Keys do
    RemoveListener(AFont);
  for AData in FDictionary.Values do
    if AData.Registrator <> nil then
      AData.Registrator.Free;
  FDictionary.Clear;
  FReferences.Clear;
  (FFontCache as TdxPDFGDIEditableFontDataCache).Clear;
end;

procedure TdxPDFFontDataStorage.Delete(AFont: TdxPDFCustomFont);
var
  ARegistrationData: TdxPDFFontRegistrationData;
  ARegistrator: TdxPDFFontCustomRegistrator;
begin
  if FDictionary.TryGetValue(AFont, ARegistrationData) then
  begin
    if FLastRegisteredFont = AFont then
      FLastRegisteredFont := nil;
    FDictionary.Remove(AFont);
    FReferences.Remove(AFont.ID);
    FQueue.Remove(AFont);
    ARegistrator := ARegistrationData.Registrator as TdxPDFFontCustomRegistrator;
    if ARegistrator <> nil then
      ARegistrator.Free;
  end;
end;

procedure TdxPDFFontDataStorage.InternalAdd(AFont: TdxPDFCustomFont);
var
  I: Integer;
begin
  if FQueue.Count > dxPDFDocumentFontCacheSize then
    for I := 0 to FQueue.Count - 1 do
      Delete(FQueue[0]);
  FReferences.Add(AFont.ID, AFont.Number, AFont);
  FDictionary.Add(AFont, FLastRegisteredFontData);
  FQueue.Add(AFont);
  AFont.AddListener(Self);
end;

procedure TdxPDFFontDataStorage.RemoveListener(AFont: TdxPDFCustomFont);
begin
  AFont.RemoveListener(Self);
end;

procedure TdxPDFFontDataStorage.FontDestroyHandler(Sender: TdxPDFBase);
begin
  Delete(Sender as TdxPDFCustomFont);
end;

{ TdxPDFXObjects }

class function TdxPDFXObjects.GetTypeName: string;
begin
  Result := TdxPDFKeywords.XObject;
end;

class function TdxPDFXObjects.GetTypePrefix: string;
begin
  Result := 'O';
end;

function TdxPDFXObjects.GetXObject(const AName: string): TdxPDFXObject;
begin
  Result := GetObject(AName) as TdxPDFXObject;
end;

{ TdxPDFColorSpaces }

function TdxPDFColorSpaces.GetColorSpace(const AName: string): TdxPDFCustomColorSpace;
begin
  Result := GetObject(AName) as TdxPDFCustomColorSpace;
  if Result = nil then
    Result := TdxPDFCustomColorSpace.CreateColorSpace(AName, nil);
end;

class function TdxPDFColorSpaces.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ColorSpace;
end;

class function TdxPDFColorSpaces.GetTypePrefix: string;
begin
  Result := 'CS';
end;

procedure TdxPDFColorSpaces.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

{ TdxPDFCustomShading }

class function TdxPDFCustomShading.Parse(ARepository: TdxPDFCustomRepository;
  ASourceObject: TdxPDFBase): TdxPDFCustomShading;
var
  AType: Integer;
  ADictionary: TdxPDFDictionary;
begin
  case ASourceObject.ObjectType of
    otDictionary:
        ADictionary := ASourceObject as TdxPDFDictionary;
    otStream:
        ADictionary := TdxPDFStream(ASourceObject).Dictionary;
    otIndirectReference:
        ADictionary := ARepository.GetDictionary(TdxPDFReference(ASourceObject).Number);
  else
    ADictionary := nil;
  end;
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetInteger(TdxPDFKeywords.ShadingType);
    if not TdxPDFUtils.IsIntegerValid(AType) then
      TdxPDFUtils.RaiseTestException('Incorrect shading type');
    case AType of
      2:
       Result := TdxPDFAxialShading.Create(nil);
      3:
       Result := TdxPDFRadialShading.Create(nil);
    else
      Result := nil;
    end;
    if Result <> nil then
      Result.Read(ADictionary as TdxPDFReaderDictionary);
  end
  else
    Result := nil;
end;

function TdxPDFCustomShading.TransformFunction(const AArguments: TDoubleDynArray): TdxPDFColor;
var
  AIndex: Integer;
  AComponents, AColorComponents: TDoubleDynArray;
begin
  if (FFunctions = nil) or (FFunctions.Count = 0) then
    Result := TdxPDFColor.Create(FColorSpace.Transform(AArguments))
  else
  begin
    if FFunctions.Count = 1 then
      AColorComponents := (FFunctions[0] as TdxPDFCustomFunction).CreateTransformedComponents(AArguments)
    else
    begin
      SetLength(AColorComponents, FFunctions.Count);
      SetLength(AComponents, 1);
      AComponents[0] := 0;
      for AIndex := 0 to FFunctions.Count - 1 do
        TdxPDFUtils.AddData((FFunctions[AIndex] as TdxPDFCustomFunction).CreateTransformedComponents(AComponents), AColorComponents);
    end;
    Result := TdxPDFColor.Create(FColorSpace.Transform(AColorComponents));
  end;
end;

class function TdxPDFCustomShading.GetTypeName: string;
begin
  Result := 'ShadingItem';
end;

procedure TdxPDFCustomShading.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorSpace := nil;
  FBackgroundColor := nil;
  FFunctions := nil;
end;

procedure TdxPDFCustomShading.DestroySubClasses;
begin
  FreeAndNil(FColorSpace);
  FreeAndNil(FBackgroundColor);
  FreeAndNil(FFunctions);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomShading.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if (ADictionary <> nil) and ADictionary.Contains(TdxPDFKeywords.ColorSpace) then
  begin
    ReadColorSpace(ADictionary.GetObject(TdxPDFKeywords.ColorSpace));
    ReadBackgroundColor(ADictionary.GetArray(TdxPDFKeywords.Background));
    ReadFunctions(ADictionary.GetObject(TdxPDFKeywords.FunctionType));
    FBoundingBox := ADictionary.GetRectangle(TdxPDFKeywords.BBox);
    FUseAntiAliasing := ADictionary.GetBoolean(TdxPDFKeywords.AntiAlias, False);
  end;
end;

class function TdxPDFCustomShading.GetShadingType: Integer;
begin
  Result := -1;
end;

function TdxPDFCustomShading.GetDomainDimension: Integer;
begin
  Result := 1;
end;

function TdxPDFCustomShading.IsFunctionRequired: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomShading.CreateFunctions(ASourceObject: TdxPDFBase): TdxPDFReferencedObjects;
var
  ATempObject: TdxPDFBase;
begin
  Result := TdxPDFReferencedObjects.Create;
  if ASourceObject.ObjectType = otArray then
    for ATempObject in TdxPDFArray(ASourceObject).ElementList do
      Result.Add(TdxPDFCustomFunction.Parse(Repository, ATempObject))
  else
    Result.Add(TdxPDFCustomFunction.Parse(Repository, ASourceObject));
end;

procedure TdxPDFCustomShading.ReadBackgroundColor(AArray: TdxPDFArray);
var
  I: Integer;
  AComponents: TDoubleDynArray;
begin
  if AArray <> nil then
  begin
    SetLength(AComponents, AArray.ElementList.Count);
    for I := 0 to AArray.ElementList.Count - 1 do
      AComponents[I] := TdxPDFUtils.ConvertToDouble(AArray.ElementList[I]);
    FBackgroundColor := TdxPDFColor.Create(AComponents);
    if Length(FBackgroundColor.Components) <> AArray.Count then
      TdxPDFUtils.RaiseTestException('Incorrect background color component count');
  end;
end;

procedure TdxPDFCustomShading.ReadColorSpace(ASourceObject: TdxPDFBase);
begin
  FColorSpace := TdxPDFCustomColorSpace.Parse(Repository, ASourceObject);
  if FColorSpace is TdxPDFPatternColorSpace then
    TdxPDFUtils.RaiseTestException;
end;

procedure TdxPDFCustomShading.ReadFunctions(ASourceObject: TdxPDFBase);
var
  AFunction: TdxPDFCustomFunction;
  I: Integer;
begin
  if ASourceObject <> nil then
  begin
    FFunctions.Free;
    if ASourceObject.ObjectType = otIndirectReference then
    begin
      ASourceObject := Repository.GetObject(TdxPDFReference(ASourceObject).Number) as TdxPDFBase;
      FFunctions := CreateFunctions(ASourceObject);
    end
    else
      FFunctions := CreateFunctions(ASourceObject);
    if FColorSpace is TdxPDFIndexedColorSpace then
      TdxPDFUtils.RaiseTestException;
    if FFunctions.Count = 1 then
    begin
      AFunction := FFunctions[0] as TdxPDFCustomFunction;
      if (Length(AFunction.Domain) <> GetDomainDimension) or (AFunction.RangeCount <> FColorSpace.ComponentCount) then
        TdxPDFUtils.RaiseTestException;
    end
    else
      if FFunctions.Count <> FColorSpace.ComponentCount then
        for I := 0 to FFunctions.Count - 1 do
        begin
          AFunction := FFunctions[0] as TdxPDFCustomFunction;
          if (Length(AFunction.Domain) <> GetDomainDimension) or (AFunction.RangeCount <> 1) then
            TdxPDFUtils.RaiseTestException;
        end
      else
        TdxPDFUtils.RaiseTestException;
  end
  else
  begin
    if IsFunctionRequired then
      TdxPDFUtils.RaiseTestException;
  end;
end;

{ TdxPDFShadings }

function TdxPDFShadings.GetShading(const AName: string): TdxPDFCustomShading;
begin
  Result := GetObject(AName) as TdxPDFCustomShading;
end;

class function TdxPDFShadings.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Shading;
end;

class function TdxPDFShadings.GetTypePrefix: string;
begin
  Result := 'S';
end;

procedure TdxPDFShadings.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

{ TdxPDFCustomPattern }

class function TdxPDFCustomPattern.Parse(ARepository: TdxPDFCustomRepository; ASourceObject: TdxPDFBase): TdxPDFCustomPattern;
begin
  Result := nil;
  if ASourceObject <> nil then
    case ASourceObject.ObjectType of
      otDictionary:
        begin
          Result := TdxPDFShadingPattern.Create(nil);
          Result.Read(ASourceObject as TdxPDFReaderDictionary);
        end;
      otStream:
        begin
          Result := TdxPDFTilingPattern.Create(nil);
          TdxPDFStream(ASourceObject).Dictionary.StreamRef := TdxPDFStream(ASourceObject);
          TdxPDFStream(ASourceObject).Dictionary.Number := ASourceObject.Number;
          Result.Read((ASourceObject as TdxPDFStream).Dictionary as TdxPDFReaderDictionary);
        end;
    else
      TdxPDFUtils.RaiseTestException('Incorrect pattern source object');
    end;
end;

class function TdxPDFCustomPattern.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

procedure TdxPDFCustomPattern.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FMatrix := nil;
end;

procedure TdxPDFCustomPattern.DestroySubClasses;
begin
  FreeAndNil(FMatrix);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomPattern.Read(ADictionary: TdxPDFReaderDictionary);
var
  AType: string;
  APatternType: Integer;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    APatternType := ADictionary.GetInteger(TdxPDFKeywords.PatternType);
    if (AType <> '') and (AType <> TdxPDFKeywords.Pattern) or
      not TdxPDFUtils.IsIntegerValid(APatternType) or (APatternType <> GetPatternType) then
      TdxPDFUtils.RaiseTestException('Incorrect pattern type');
    FMatrix := TdxPDFUtils.ArrayToMatrix(ADictionary.GetArray(TdxPDFKeywords.Matrix));
  end;
end;

class function TdxPDFCustomPattern.GetPatternType: Integer;
begin
  Result := -MaxInt;
end;

{ TdxPDFPatterns }

function TdxPDFPatterns.GetPattern(const AName: string): TdxPDFCustomPattern;
begin
  Result := GetObject(AName) as TdxPDFCustomPattern;
end;

procedure TdxPDFPatterns.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

class function TdxPDFPatterns.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

class function TdxPDFPatterns.GetTypePrefix: string;
begin
  Result := 'Ptrn';
end;

{ TdxPDFShadingPattern }

class function TdxPDFShadingPattern.GetPatternType: Integer;
begin
  Result := 2;
end;

procedure TdxPDFShadingPattern.CreateSubClasses;
begin
  Shading := nil;
  GraphicsState := nil;
  inherited CreateSubClasses;
end;

procedure TdxPDFShadingPattern.DestroySubClasses;
begin
  GraphicsState := nil;
  Shading := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFShadingPattern.Read(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    if not ADictionary.TryGetObject(TdxPDFKeywords.Shading, AObject) then
      TdxPDFUtils.RaiseTestException;
    Shading := TdxPDFCustomShading.Parse(Repository, AObject);
    GraphicsState := TdxPDFGraphicsStateParameters.Create(Self);
    GraphicsState.Read(ADictionary.GetDictionary(TdxPDFKeywords.ExtGState));
  end;
end;

procedure TdxPDFShadingPattern.SetGraphicsStateParameters(const AValue: TdxPDFGraphicsStateParameters);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FGraphicsState));
end;

procedure TdxPDFShadingPattern.SetShading(const AValue: TdxPDFCustomShading);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FShading));
end;

{ TdxPDFTilingPattern }

function TdxPDFTilingPattern.CreateMatrix(AWidth: Integer; AHeight: Integer): TdxPDFTransformationMatrix;
var
  AFactorX, AFactorY: Double;
begin
  AFactorX := AWidth / Abs(FBoundingBox.Width);
  AFactorY := AHeight / Abs(FBoundingBox.Height);
  Result := TdxPDFTransformationMatrix.Create;
  Result.Assign(AFactorX, 0, 0, AFactorY, -FBoundingBox.Left * AFactorX, -FBoundingBox.Bottom * AFactorY);
end;

class function TdxPDFTilingPattern.GetPatternType: Integer;
begin
  Result := 1;
end;

procedure TdxPDFTilingPattern.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCommands := nil;
  FColoredPaintType := 1;
  FUncoloredPaintType := 2;
  FColored := True;
  Resources := nil;
end;

procedure TdxPDFTilingPattern.DestroySubClasses;
begin
  Resources := nil;
  FreeAndNil(FCommands);
  inherited DestroySubClasses;
end;

procedure TdxPDFTilingPattern.Read(ADictionary: TdxPDFReaderDictionary);

  procedure ReadResources(ADictionary: TdxPDFReaderDictionary);
  begin
    if ADictionary <> nil then
      Resources := Repository.GetResources(ADictionary);
  end;

var
  APaintType: Integer;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FColoredPaintType := 1;
    FUncoloredPaintType := 2;

    APaintType := ADictionary.GetInteger(TdxPDFKeywords.PaintType);
    ReadTilingType(ADictionary);
    FBoundingBox := ADictionary.GetRectangle(TdxPDFKeywords.BBox);

    if not TdxPDFUtils.IsIntegerValid(APaintType) or (FBoundingBox = cxRectF(cxNullRect)) then
      TdxPDFUtils.RaiseTestException('Error reading tiling pattern');

    case APaintType of
      1:
        FColored := True;
      2:
        FColored := False;
      else
        TdxPDFUtils.RaiseTestException('Incorrect tiling pattern type');
    end;
    ReadResources(ADictionary);
    ReadStep(ADictionary);
    ReadCommands(ADictionary);
  end;
end;

procedure TdxPDFTilingPattern.SetOwner(const AValue: TObject);
begin
  inherited SetOwner(AValue);
  if Resources <> nil then
    Resources.Owner := Owner;
end;

procedure TdxPDFTilingPattern.ReadCommands(ADictionary: TdxPDFReaderDictionary);
begin
  FCommands := TdxPDFReferencedObjects.Create;
  if ADictionary.StreamRef <> nil then
    TdxPDFCommandStreamParser.Parse(Repository, ADictionary.StreamRef.UncompressedData, FCommands, Resources);
end;

procedure TdxPDFTilingPattern.ReadStep(ADictionary: TdxPDFDictionary);
const
  sdxErrorMessage = 'Error reading tiling pattern steps';
var
  AXStepValue, AYStepValue: Double;
begin
  AXStepValue := ADictionary.GetDouble(TdxPDFKeywords.XStep);
  AYStepValue := ADictionary.GetDouble(TdxPDFKeywords.YStep);
  if not TdxPDFUtils.IsDoubleValid(AXStepValue) or not TdxPDFUtils.IsDoubleValid(AYStepValue) then
    TdxPDFUtils.RaiseTestException(sdxErrorMessage);
  FXStep := AXStepValue;
  FYStep := AYStepValue;
  if (FXStep = 0) or (FYStep = 0) then
    TdxPDFUtils.RaiseTestException(sdxErrorMessage);
end;

procedure TdxPDFTilingPattern.ReadTilingType(ADictionary: TdxPDFDictionary);
var
  AType: Integer;
begin
  AType := ADictionary.GetInteger(TdxPDFKeywords.TilingType);
  if TdxPDFUtils.IsIntegerValid(AType) then
    FTilingType := TdxPDFTilingType(AType)
  else
    TdxPDFUtils.RaiseTestException('Incorrect tiling type');
end;

{ TdxPDFFullTrustGlyphMapper }

constructor TdxPDFFullTrustGlyphMapper.Create(AFontFile: TdxFontFile);
var
  AIsSymbolic: Boolean;
  AComparison: TComparison<TdxFontFileCMapCustomFormatRecord>;
begin
  inherited Create;
  FFontFile := AFontFile;
  FMappedGlyphsCache := TDictionary<Integer, Integer>.Create;

  if FFontFile.HheaTable = nil then
    FFactor := 1000 / 2048
  else
    FFactor := 1000.0 / AFontFile.HeadTable.UnitsPerEm;

  FCMapTables := TList<TdxFontFileCMapCustomFormatRecord>.Create;
  if AFontFile.CMapTable <> nil then
  begin
    FCMapTables.AddRange(AFontFile.CMapTable.CMapTables);
    AIsSymbolic := (AFontFile.OS2Table <> nil) and AFontFile.OS2Table.IsSymbolic;
    AComparison :=
      function(const Left, Right: TdxFontFileCMapCustomFormatRecord): Integer
      begin
        Result := GetCMapEntryPriority(Left, AIsSymbolic) - GetCMapEntryPriority(Right, AIsSymbolic);
      end;
    FCMapTables.Sort(TComparer<TdxFontFileCMapCustomFormatRecord>.Construct(AComparison));
  end;
end;

destructor TdxPDFFullTrustGlyphMapper.Destroy;
begin
  FreeAndNil(FCMapTables);
  FreeAndNil(FMappedGlyphsCache);
  inherited Destroy;
end;

class function TdxPDFFullTrustGlyphMapper.GetCMapEntryPriority(AEntry: TdxFontFileCMapCustomFormatRecord;
  AIsSymbolic: Boolean): Integer;
begin
  case AEntry.PlatformId of
    TdxFontFilePlatformID.Microsoft:
      Result := 0;
    TdxFontFilePlatformID.ISO:
      Result := 100;
  else
    Result := 200;
  end;
  case AEntry.EncodingId of
    TdxFontFileEncodingID.UGL:
      Inc(Result, IfThen(AIsSymbolic, 10, 0));
    TdxFontFileEncodingID.Undefined:
      Inc(Result, IfThen(AIsSymbolic, 0, 10));
  else
    Inc(Result, 20);
  end;
  if not (AEntry is TdxFontFileCMapSegmentMappingRecord) then
    Inc(Result, 1);
end;

function TdxPDFFullTrustGlyphMapper.GetGlyphIndex(ACharacter: Char): Integer;
var
  ACMap: TdxFontFileCMapCustomFormatRecord;
begin
  Result := 0;
  if not FMappedGlyphsCache.TryGetValue(Integer(ACharacter), Result) then
  begin
    for ACMap in FCMapTables do
    begin
      Result := ACMap.MapCode(ACharacter);
      if Result <> TdxFontFileCMapCustomFormatRecord.NotdefGlyphIndex then
        Break;
    end;
    FMappedGlyphsCache.Add(Integer(ACharacter), Result);
  end;
end;

function TdxPDFFullTrustGlyphMapper.MapString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
begin
  Result := MapStringWithoutCTL(AStr, AFlags);
end;

function TdxPDFFullTrustGlyphMapper.IsWritingOrderControl(AChar: Char): Boolean;
begin
  Result := False;
end;

function TdxPDFFullTrustGlyphMapper.MapStringWithoutCTL(const AStr: string;
  AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
var
  ALength, I, AVal: Integer;
  AGlyphIndices: TList<Integer>;
  ASb: TStringBuilder;
  ACh: Char;
  AResult: TDictionary<Integer, string>;
  AGlyphs: TdxPDFGlyphList;
  ACodes: TBytes;
  AKern: TdxFontFileKernTable;
  AKerningShouldBeUsed: Boolean;
  AActualText: string;
  AGlyphOffset: Double;
  AGlyph: TdxPDFGlyph;
begin
  ALength := Length(AStr);
  AGlyphIndices := TList<Integer>.Create;
  try
    ASb := TStringBuilder.Create;
    try
      if FCMapTables <> nil then
        for I := 1 to ALength do
        begin
          ACh := AStr[I];
          if not IsWritingOrderControl(ACh) then
          begin
            AGlyphIndices.Add(GetGlyphIndex(ACh));
            ASb.Append(ACh);
          end;
        end;
      AResult := TDictionary<Integer, string>.Create(ALength);
      AGlyphs := TdxPDFGlyphList.Create;
      ALength := AGlyphIndices.Count;
      SetLength(ACodes, ALength * 2);

      AKern := FFontFile.KernTable;
      AKerningShouldBeUsed := HasFlag(AFlags, mfUseKerning) and (AKern <> nil) and (AGlyphIndices <> nil);
      AActualText := ASb.ToString;
    finally
      ASb.Free;
    end;

    for I := 0 to ALength - 1 do
    begin
      ACh := AActualText[I + 1];
      if AGlyphIndices = nil then
        AVal := Integer(ACh)
      else
        AVal := AGlyphIndices[I];
      AGlyphOffset := 0;
      if AKerningShouldBeUsed and (I > 0) then
        AGlyphOffset := -AKern.GetKerning(AGlyphIndices[I - 1], AVal) * FFactor;
      AGlyph := CreateGlyph(AVal, ACh, FFontFile.GetCharacterWidth(AVal), AGlyphOffset);
      if not AResult.ContainsKey(AGlyph.Index) then
        AResult.Add(AGlyph.Index, ACh);
      AGlyphs.Add(AGlyph);
    end;
    Result := TdxPDFGlyphMappingResult.Create(CreateGlyphRun(AGlyphs), AResult);
  finally
    AGlyphIndices.Free;
  end;
end;

{ TdxPDFEmbeddedGlyphMapper }

function TdxPDFEmbeddedGlyphMapper.CreateGlyphRun: TdxPDFGlyphRun;
begin
  Result := TdxPDFCompositeFontGlyphRun.Create;
end;

function TdxPDFEmbeddedGlyphMapper.CreateGlyph(AGlyphIndex: Integer; ACh: Char; AWidth, AGlyphOffset: Double): TdxPDFGlyph;
begin
  Result := TdxPDFGlyph.Create(AGlyphIndex, AWidth, AGlyphOffset);
end;

function TdxPDFEmbeddedGlyphMapper.CreateGlyphRun(const AGlyphs: TdxPDFGlyphList): TdxPDFGlyphRun;
begin
  Result := TdxPDFCompositeFontGlyphRun2.Create(AGlyphs);
end;

{ TdxPDFDocumentState }

function TdxPDFDocumentState.GetRepository: TdxPDFDocumentRepository;
begin
  Result := TdxPDFDocumentAccess(Owner as TdxPDFDocument).Repository;
end;

function TdxPDFDocumentState.CreateFontData(const AFontFamilyName: string; AFontStyle: TdxGPFontStyle): TObject;
begin
  Result := FontDataStorage.SearchFontData(AFontFamilyName, AFontStyle);
end;

function TdxPDFDocumentState.GetPageIndex(APage: TdxPDFPage): Integer;
begin
  Result := TdxPDFDocumentAccess(Owner as TdxPDFDocument).Pages.IndexOf(APage);
end;

function TdxPDFDocumentState.SearchFontData(AFontCommand: TdxPDFCustomCommand): TObject;
var
  AFontName: string;
  AFontStyle: TdxGPFontStyle;
  AIsEmptyFontName: Boolean;
  APitchAndFamily: Byte;
begin
  if AFontCommand <> nil then
  begin
    AIsEmptyFontName := False;
    APitchAndFamily := DEFAULT_PITCH;
    CalculateFontParameters(AFontCommand, AFontName, AFontStyle, APitchAndFamily, AIsEmptyFontName);
    Result := CreateFontData(AFontName, AFontStyle);
    if (Result = nil) and not AIsEmptyFontName then
      if TdxStringHelper.Contains(AFontName, TdxPDFKeywords.TimesNewRomanFontName2) or
        TdxStringHelper.Contains(AFontName, TdxPDFKeywords.TimesNewRomanFontName) then
        Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName, AFontStyle)
      else
        if TdxStringHelper.Contains(AFontName, TdxPDFKeywords.CourierFontName) then
          Result := CreateFontData(TdxPDFKeywords.CourierFontName, AFontStyle)
        else
          if TdxStringHelper.Contains(AFontName, TdxPDFKeywords.ArialFontName) then
            Result := CreateFontData(TdxPDFKeywords.ArialFontName, AFontStyle)
          else
            if APitchAndFamily = FF_ROMAN then
              Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName2, AFontStyle)
            else
              if APitchAndFamily = FIXED_PITCH then
                Result := CreateFontData(TdxPDFKeywords.CourierNewFontName2, AFontStyle)
              else
                Result := CreateFontData(TdxPDFKeywords.ArialFontName, AFontStyle);
  end
  else
    Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName2, TdxGPFontStyle.FontStyleRegular);
end;

function TdxPDFDocumentState.GetImageDataStorage: TdxPDFDocumentImageDataStorage;
begin
  Result := GetRepository.ImageDataStorage;
end;

function TdxPDFDocumentState.GetFontDataStorage: TdxPDFFontDataStorage;
begin
  Result := GetRepository.FontDataStorage;
end;

procedure TdxPDFDocumentState.SetRotationAngle(const AValue: TcxRotationAngle);
begin
  if FRotationAngle <> AValue then
  begin
    FRotationAngle := AValue;
    dxCallNotify(OnRotationAngleChanged, Self);
  end;
end;

procedure TdxPDFDocumentState.CalculateFontParameters(ACommand: TdxPDFCustomCommand; out AFontName: string;
  out AFontStyle: TdxGPFontStyle; var APitchAndFamily: Byte; var AIsEmptyFontName: Boolean);
var
  ARegistrationData: TdxPDFFontRegistrationData;
begin
  AFontStyle := TdxGPFontStyle.FontStyleRegular;
  if (ACommand is TdxPDFSetTextFontCommand) and (TdxPDFSetTextFontCommand(ACommand).Font <> nil) then
  begin
    ARegistrationData := FontDataStorage.CreateSubstituteFontData(TdxPDFSetTextFontCommand(ACommand).Font);
    if ARegistrationData.Weight > 400 then
      AFontStyle := TdxGPFontStyle(Integer(AFontStyle) or Integer(TdxGPFontStyle.FontStyleBold));
    if ARegistrationData.Italic then
      AFontStyle := TdxGPFontStyle(Integer(AFontStyle) or Integer(TdxGPFontStyle.FontStyleItalic));
    AFontName := ARegistrationData.Name;
    APitchAndFamily := ARegistrationData.PitchAndFamily;
  end
  else
    AFontName := TdxPDFSetTextFontCommand(ACommand).FontName;
  AIsEmptyFontName := AFontName = '';
  if AIsEmptyFontName then
    AFontName := TdxPDFKeywords.TimesNewRomanFontName2;
end;

{ TdxPDFStreamObject }

procedure TdxPDFStreamObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Stream := nil;
end;

procedure TdxPDFStreamObject.DestroySubClasses;
begin
  Stream := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFStreamObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  Stream := Repository.GetStream(ADictionary.Number);
end;

function TdxPDFStreamObject.GetData: TBytes;
begin
  Result := Stream.UncompressedData;
end;

function TdxPDFStreamObject.GetUncompressedData: TBytes;
begin
  Result := Stream.UncompressedData;
end;

procedure TdxPDFStreamObject.SetStream(const AValue: TdxPDFStream);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FStream));
end;

{ TdxPDFResources }

function TdxPDFResources.AddFont(AFont: TdxPDFCustomFont): string;
begin
  Result := Fonts.Add(AFont);
end;

function TdxPDFResources.GetColorSpace(const AName: string): TdxPDFCustomColorSpace;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetColorSpace(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetColorSpace(AName);
  end;
end;

function TdxPDFResources.GetFont(const AName: string): TdxPDFCustomFont;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetFont(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetFont(AName);
  end;
end;

function TdxPDFResources.GetGraphicsStateParameters(const AName: string): TdxPDFGraphicsStateParameters;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetGraphicsStateParameters(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetGraphicsStateParameters(AName);
  end;
end;

function TdxPDFResources.GetPattern(const AName: string): TdxPDFCustomPattern;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetPattern(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetPattern(AName);
  end;
end;

function TdxPDFResources.GetShading(const AName: string): TdxPDFCustomShading;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetShading(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetShading(AName);
  end;
end;

function TdxPDFResources.GetXObject(const AName: string): TdxPDFXObject;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetXObject(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetXObject(AName);
  end;
end;

function TdxPDFResources.GetProperties(const AName: string): TdxPDFCustomProperties;
begin
  Result := nil;
end;

procedure TdxPDFResources.Append(AResources: TdxPDFResources);
begin
  if AResources.Fonts.Count > 0 then
    Fonts.Append(AResources.Fonts);
  if AResources.GraphicStatesParametersList.Count > 0 then
    GraphicStatesParametersList.Append(AResources.GraphicStatesParametersList);
  if AResources.XObjects.Count > 0 then
    XObjects.Append(AResources.XObjects);
  if AResources.ColorSpaces.Count > 0 then
    ColorSpaces.Append(AResources.ColorSpaces);
end;

procedure TdxPDFResources.Pack;
begin
  if (ReferenceCount = 1) or Repository.IsResourcesShared(Self) and (ReferenceCount <= 2) then
    Clear;
end;

class function TdxPDFResources.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Resources;
end;

procedure TdxPDFResources.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFResources.DestroySubClasses;
begin
  Clear;
  Dictionary := nil;
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFResources.Initialize;
begin
  inherited Initialize;
  FID := TdxPDFUtils.GenerateGUID;
end;

procedure TdxPDFResources.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    Dictionary := ADictionary;
end;

function TdxPDFResources.GetOwnerResources: TdxPDFResources;
begin
  if Owner is TdxPDFPageData then
    Result := TdxPDFPageData(Owner).Resources
  else
    if Owner is TdxPDFPages then
      Result := TdxPDFPages(Owner).Resources
    else
      Result := nil;
end;

function TdxPDFResources.InternalGetColorSpace(const AName: string): TdxPDFCustomColorSpace;
begin
  Result := ColorSpaces.GetColorSpace(AName);
end;

function TdxPDFResources.InternalGetFont(const AName: string): TdxPDFCustomFont;
begin
  Result := Fonts.GetFont(AName);
end;

function TdxPDFResources.InternalGetGraphicsStateParameters(const AName: string): TdxPDFGraphicsStateParameters;
begin
  Result := GraphicStatesParametersList.GetParameters(AName) as TdxPDFGraphicsStateParameters;
end;

function TdxPDFResources.InternalGetPattern(const AName: string): TdxPDFCustomPattern;
begin
  Result := Patterns.GetPattern(AName);
end;

function TdxPDFResources.InternalGetShading(const AName: string): TdxPDFCustomShading;
begin
  Result := Shadings.GetShading(AName);
end;

function TdxPDFResources.InternalGetXObject(const AName: string): TdxPDFXObject;
begin
  Result := XObjects.GetXObject(AName);
end;

function TdxPDFResources.AddGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters): string;
begin
  Result := FGraphicStatesParametersList.Add(AParameters);
end;

function TdxPDFResources.AddPattern(APattern: TdxPDFCustomPattern): string;
begin
  Result := FPatterns.Add(APattern);
end;

function TdxPDFResources.AddXObject(ANumber: Integer): string;
begin
  Result := FXObjects.AddReference(ANumber);
end;

function TdxPDFResources.GetColorSpaces: TdxPDFColorSpaces;
begin
  FColorSpaces := GetList(FColorSpaces, TdxPDFColorSpaces, TdxPDFKeywords.ColorSpace) as TdxPDFColorSpaces;
  Result := FColorSpaces;
end;

function TdxPDFResources.GetFonts: TdxPDFFonts;
begin
  FFonts := GetList(FFonts, TdxPDFFonts, TdxPDFKeywords.Font) as TdxPDFFonts;
  Result := FFonts;
end;

function TdxPDFResources.GetList(AVariable: TdxPDFObjectList; AClass: TdxPDFObjectListClass; const AKey: string): TdxPDFObjectList;
begin
  EnterCriticalSection(FLock);
  try
    if AVariable = nil then
    begin
      Result := AClass.Create(Self);
      if Dictionary <> nil then
        Result.Read(Dictionary.GetDictionary(AKey));
    end
    else
      Result := AVariable;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFResources.GetGraphicStatesParametersList: TdxPDFGraphicsStateParametersList;
begin
  FGraphicStatesParametersList := GetList(FGraphicStatesParametersList, TdxPDFGraphicsStateParametersList,
    TdxPDFKeywords.ExtGState) as TdxPDFGraphicsStateParametersList;
  Result := FGraphicStatesParametersList;
end;

function TdxPDFResources.GetPatterns: TdxPDFPatterns;
begin
  FPatterns := GetList(FPatterns, TdxPDFPatterns, TdxPDFKeywords.Pattern) as TdxPDFPatterns;
  Result := FPatterns;
end;

function TdxPDFResources.GetShadings: TdxPDFShadings;
begin
  FShadings := GetList(FShadings, TdxPDFShadings, TdxPDFKeywords.Shading) as TdxPDFShadings;
  Result := FShadings;
end;

function TdxPDFResources.GetXObjects: TdxPDFXObjects;
begin
  FXObjects := GetList(FXObjects, TdxPDFXObjects, TdxPDFKeywords.XObject) as TdxPDFXObjects;
  Result := FXObjects;
end;

procedure TdxPDFResources.SetColorSpaces(const AValue: TdxPDFColorSpaces);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FColorSpaces));
end;

procedure TdxPDFResources.SetDictionary(const AValue: TdxPDFReaderDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

procedure TdxPDFResources.SetFonts(const AValue: TdxPDFFonts);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFonts));
end;

procedure TdxPDFResources.SetGraphicStatesParametersList(const AValue: TdxPDFGraphicsStateParametersList);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FGraphicStatesParametersList));
end;

procedure TdxPDFResources.SetPatterns(const AValue: TdxPDFPatterns);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FPatterns));
end;

procedure TdxPDFResources.SetShadings(const AValue: TdxPDFShadings);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FShadings));
end;

procedure TdxPDFResources.SetXObjects(const AValue: TdxPDFXObjects);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FXObjects));
end;

procedure TdxPDFResources.Clear;
begin
  Patterns := nil;
  Shadings := nil;
  XObjects := nil;
  GraphicStatesParametersList := nil;
  Fonts := nil;
  ColorSpaces := nil;
end;

{ TdxPDFPageContents }

class function TdxPDFPageContents.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Contents;
end;

function TdxPDFPageContents.GetData: TBytes;
var
  I: Integer;
begin
  for I := 0 to ContentList.Count - 1 do
  begin
    TdxPDFUtils.AddData((ContentList[I] as TdxPDFStreamObject).Stream.UncompressedData, Result);
    TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.Space, Result);
  end;
end;

procedure TdxPDFPageContents.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCommands := TdxPDFReferencedObjects.Create;
  FContentList := TdxPDFReferencedObjects.Create;
end;

procedure TdxPDFPageContents.DestroySubClasses;
begin
  FreeAndNil(FContentList);
  FreeAndNil(FCommands);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageContents.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadContentList(ADictionary, ADictionary.GetObject(TdxPDFKeywords.Contents));
end;

procedure TdxPDFPageContents.ClearCommands;
begin
  FCommands.Clear;
end;

procedure TdxPDFPageContents.PopulateCommands(AResources: TdxPDFResources);
begin
  TdxPDFCommandStreamParser.Parse(nil, GetData, FCommands, AResources);
end;

function TdxPDFPageContents.GetCommandCount: Integer;

  function InternalGetCommandCount(AList: TdxPDFReferencedObjects): Integer;
  var
    I: Integer;
    ACommand: TdxPDFCustomCommand;
  begin
    Result := 0;
    for I := 0 to AList.Count - 1 do
    begin
      ACommand := AList[I] as TdxPDFCustomCommand;
      if ACommand is TdxPDFCommandGroup then
        Inc(Result, InternalGetCommandCount(TdxPDFCommandGroupAccess(ACommand).Commands))
      else
        Inc(Result);
    end;
  end;

begin
  Result := InternalGetCommandCount(Commands);
end;

function TdxPDFPageContents.GetResources: TdxPDFResources;
begin
  Result := (Owner as TdxPDFPageData).Resources;
end;

procedure TdxPDFPageContents.ReadItem(AStream: TdxPDFStream);
var
  AContentItem: TdxPDFPageContentItem;
begin
  AContentItem := TdxPDFPageContentItem.Create(Self);
  AStream.Dictionary.Number := AStream.Number;
  AContentItem.Read(AStream.Dictionary as TdxPDFReaderDictionary);
  TdxPDFDocumentStreamObjectAccess(AContentItem).Stream := AStream;
  ContentList.Add(AContentItem);
end;

procedure TdxPDFPageContents.ReadContentList(ADictionary: TdxPDFReaderDictionary;
  AContentObject: TdxPDFBase);
var
  I: Integer;
  AStream: TdxPDFBase;
begin
  if (AContentObject <> nil) and not ((AContentObject.ObjectType = otDictionary) and
    (TdxPDFDictionary(AContentObject).Count = 0)) then
    case AContentObject.ObjectType of
      otIndirectReference, otDictionary, otStream:
        if GetObject(TdxPDFKeywords.Contents, ADictionary, AStream) then
          if AStream.ObjectType = otStream then
            ReadItem(AStream as TdxPDFStream)
          else
            ReadContentList(ADictionary, AStream);
      otArray:
        for I := 0 to (AContentObject as TdxPDFArray).Count - 1 do
        begin
          AStream := Repository.GetStream((TdxPDFArray(AContentObject)[I] as TdxPDFBase).Number);
          AStream.Number := (TdxPDFArray(AContentObject)[I] as TdxPDFBase).Number;
          if AStream <> nil then
            ReadItem(TdxPDFStream(AStream));
        end;
    end;
end;

{ TdxPDFPageTreeObject }

procedure TdxPDFPageTreeObject.DestroySubClasses;
begin
  Resources := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadResources(ADictionary);
end;

procedure TdxPDFPageTreeObject.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  if ADictionary <> nil then
  begin
    Repository := ADictionary.Repository;
    FArtBox := ADictionary.GetRectangle(TdxPDFKeywords.ArtBox, FArtBox);
    FBleedBox := ADictionary.GetRectangle(TdxPDFKeywords.BleedBox, FBleedBox);
    FCropBox := ADictionary.GetRectangle(TdxPDFKeywords.CropBox, FCropBox);
    FMediaBox := ADictionary.GetRectangle(TdxPDFKeywords.MediaBox, FMediaBox);
    FTrimBox := ADictionary.GetRectangle(TdxPDFKeywords.TrimBox, FTrimBox);
    FUserUnit := ADictionary.GetInteger(TdxPDFKeywords.UserUnit, 1);
    if ADictionary.Contains(TdxPDFKeywords.Rotate) then
    begin
      FRotationAngle := ADictionary.GetInteger(TdxPDFKeywords.Rotate);
      FUseParentRotationAngle := False;
    end
    else
    begin
      FUseParentRotationAngle := True;
      FRotationAngle := 0;
    end;
  end;
end;

procedure TdxPDFPageTreeObject.Clear;
begin
  DestroySubClasses;
  CreateSubClasses;
end;

function TdxPDFPageTreeObject.GetArtBox: TdxRectF;
begin
  Result := FArtBox;
  if TdxPDFUtils.IsRectEmpty(FArtBox) then
    Result := GetParentArtBox;
  if TdxPDFUtils.IsRectEmpty(Result) then
    Result := CropBox;
end;

function TdxPDFPageTreeObject.GetBleedBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FBleedBox) then
    Result := GetParentBleedBox
  else
    Result := BleedBox;
  if TdxPDFUtils.IsRectEmpty(Result) then
    Result := CropBox;
end;

function TdxPDFPageTreeObject.GetCropBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FCropBox) then
    Result := MediaBox
  else
    Result := FCropBox;
end;

function TdxPDFPageTreeObject.GetMediaBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FMediaBox) then
    Result := GetParentMediaBox
  else
    Result := FMediaBox;
end;

function TdxPDFPageTreeObject.GetTrimBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FTrimBox) then
    Result := GetParentTrimBox
  else
    Result := FTrimBox;
end;

function TdxPDFPageTreeObject.GetUserUnit: Integer;
begin
  if FUserUnit = 0 then
    Result := GetParentUserUnit
  else
    Result := FUserUnit;
end;

function TdxPDFPageTreeObject.GetParent: TdxPDFPageTreeObject;
begin
  if (Owner is TdxPDFPageTreeObject) then
    Result := TdxPDFPageTreeObject(Owner)
  else
    Result := nil;
end;

function TdxPDFPageTreeObject.GetParentArtBox: TdxRectF;
begin
  if GetParent <> nil then
    Result := GetParent.ArtBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentBleedBox: TdxRectF;
begin
  if GetParent <> nil then
    Result := GetParent.BleedBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentMediaBox: TdxRectF;
begin
  if GetParent <> nil then
    Result := GetParent.MediaBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentRotationAngle: Integer;
begin
  if GetParent <> nil then
    Result := GetParent.RotationAngle
  else
    Result := dxPDFInvalidValue;
end;

function TdxPDFPageTreeObject.GetParentTrimBox: TdxRectF;
begin
  if GetParent <> nil then
    Result := GetParent.TrimBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentUseParentRotationAngle: Boolean;
begin
  if GetParent <> nil then
    Result := GetParent.UseParentRotationAngle
  else
    Result := False;
end;

function TdxPDFPageTreeObject.GetParentUserUnit: Integer;
begin
  if GetParent <> nil then
    Result := GetParent.UserUnit
  else
    Result := 1;
end;

function TdxPDFPageTreeObject.GetResources: TdxPDFResources;
begin
  if FResources <> nil then
    Result := FResources
  else
    Result := GetOwnerResources;
end;

function TdxPDFPageTreeObject.GetRotationAngle: Integer;
begin
  if UseParentRotationAngle then
  begin
    if GetParentUseParentRotationAngle then
      Result := GetParentRotationAngle
    else
      Result := 0;
  end
  else
    Result := FRotationAngle;
end;

function TdxPDFPageTreeObject.GetOwnerResources: TdxPDFResources;
begin
  if (Owner is TdxPDFPageTreeObject) then
    Result := TdxPDFPageTreeObject(Owner).Resources
  else
    Result := nil;
end;

procedure TdxPDFPageTreeObject.SetResources(const AValue: TdxPDFResources);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources));
end;

procedure TdxPDFPageTreeObject.ReadResources(ADictionary: TdxPDFReaderDictionary);
begin
  Resources := Repository.GetResources(ADictionary);
end;

{ TdxPDFPageTreeObjectList }

procedure TdxPDFPageTreeObjectList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FChildren := TObjectList<TdxPDFPageTreeObject>.Create;
end;

procedure TdxPDFPageTreeObjectList.DestroySubClasses;
begin
  FreeAndNil(FChildren);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeObjectList.Read(ADictionary: TdxPDFReaderDictionary);

  procedure AddNode(ANodeDictionary: TdxPDFReaderDictionary; ANumber: Integer);
  var
    ANode: TdxPDFPageTreeObject;
    ANodeType: string;
  begin
    if ANodeDictionary <> nil then
    begin
      ANodeDictionary.Number := ANumber;
      ANodeType := ANodeDictionary.GetString(TdxPDFKeywords.TypeKey);
      if ANodeType = TdxPDFKeywords.Pages then
      begin
        ANode := TdxPDFPageTreeNode.Create(Self);
        TdxPDFPageTreeNode(ANode).OnCreatePageNode := OnCreatePageNode;
        ANode.Read(ANodeDictionary);
        Add(ANode);
      end
      else
        if Assigned(OnCreatePageNode) then
          Add(OnCreatePageNode(Self, ANodeDictionary));
    end;
  end;

var
  I: Integer;
  ANodeDictionary: TdxPDFReaderDictionary;
  AType: string;
  AKidReferences: TdxPDFArray;
begin
  if ADictionary <> nil then
  begin
    ReadProperties(ADictionary);
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    AKidReferences := ADictionary.GetArray(TdxPDFKeywords.Kids);
    if ((AType = TdxPDFKeywords.Pages) or (AType = '')) and (AKidReferences <> nil) then
      for I := 0 to AKidReferences.Count - 1 do
      begin
        ANodeDictionary := Repository.GetDictionary(AKidReferences[I].Number);
        AddNode(ANodeDictionary, AKidReferences[I].Number);
      end
    else
      if AType = TdxPDFKeywords.Page then
        AddNode(ADictionary, ADictionary.Number);
  end;
end;

procedure TdxPDFPageTreeObjectList.Add(AChild: TdxPDFPageTreeObject);
begin
  FChildren.Add(AChild);
end;

function TdxPDFPageTreeObjectList.getCount: Integer;
begin
  Result := FChildren.Count;
end;

function TdxPDFPageTreeObjectList.GetItem(AIndex: Integer): TdxPDFPageTreeObject;
begin
  Result := FChildren[AIndex];
end;

{ TdxPDFPageTreeNode }

procedure TdxPDFPageTreeNode.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FNodeList := TdxPDFPageTreeObjectList.Create(Self);
  FNodeList.OnCreatePageNode := OnCreatePageNodeHandler;
end;

procedure TdxPDFPageTreeNode.DestroySubClasses;
begin
  FreeAndNil(FNodeList);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeNode.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    FNodeList.Read(ADictionary);
end;

procedure TdxPDFPageTreeNode.AddNode(AChild: TdxPDFPageTreeObject);
begin
  FNodeList.Add(AChild);
end;

function TdxPDFPageTreeNode.OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
  ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;
begin
  if Assigned(OnCreatePageNode) then
    Result := OnCreatePageNode(AOwner, ADictionary)
  else
    Result := TdxPDFPageTreeObject.Create(AOwner);
end;

{ TdxPDFPages }

class function TdxPDFPages.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pages;
end;

procedure TdxPDFPages.Clear;
begin
  inherited Clear;
  FDictionary.Clear;
end;

procedure TdxPDFPages.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPageList := TList<TdxPDFPage>.Create;
  OnCreatePageNode := OnCreatePageNodeHandler;
  FDictionary := TDictionary<Integer, TdxPDFPage>.Create;
end;

procedure TdxPDFPages.DestroySubClasses;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FPageList);
  inherited DestroySubClasses;
end;

function TdxPDFPages.FindPage(ANumber: Integer): TdxPDFPage;
begin
  if not FDictionary.TryGetValue(ANumber, Result) then
    Result := nil;
end;

function TdxPDFPages.IndexOf(APage: TdxPDFPage): Integer;
begin
  Result := FPageList.IndexOf(APage);
end;

procedure TdxPDFPages.AddPage(APage: TdxPDFPage);
begin
  FPageList.Add(APage);
  FDictionary.Add(APage.Number, APage);
end;

function TdxPDFPages.GetCount: Integer;
begin
  Result := FPageList.Count;
end;

function TdxPDFPages.GetPage(AIndex: Integer): TdxPDFPage;
begin
  Result := FPageList[AIndex] as TdxPDFPage;
end;

function TdxPDFPages.OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
  ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;

  function CreatePage(const AInfo: TdxPDFDeferredObjectInfo): TdxPDFPage;
  begin
    Result := TdxPDFPage.Create(AOwner, AInfo);
    Result.OnPack := OnPagePack;
    Result.Read(ADictionary);
    AddPage(Result);
  end;

var
  AInfo: TdxPDFDeferredObjectInfo;
begin
  AInfo.Name := GetTypeName;
  AInfo.Key := TdxPDFKeywords.TypeKey;
  AInfo.Number := ADictionary.Number;
  AInfo.SourceObject := ADictionary;
  Result := CreatePage(AInfo);
end;

{ TdxPDFFileSpecificationData }

procedure TdxPDFFileSpecificationData.DestroySubClasses;
begin
  dxPDFFreeObject(FParametersDictionary);
  inherited DestroySubClasses;
end;

procedure TdxPDFFileSpecificationData.Read(ADictionary: TdxPDFReaderDictionary);
const
  CreationDateDictionaryKey = 'CreationDate';
  ModificationDateDictionaryKey = 'ModDate';
  ParametersDictionaryKey = 'Params';
  SizeDictionaryKey = 'Size';
begin
  inherited Read(ADictionary);
  FMimeType := ADictionary.GetString(TdxPDFKeywords.Subtype);
  FParametersDictionary := ADictionary.GetDictionary(ParametersDictionaryKey);
  if FParametersDictionary <> nil then
  begin
    FParametersDictionary.Reference;
    FSize := FParametersDictionary.GetInteger(SizeDictionaryKey, 0);
    FCreationDate := FParametersDictionary.GetDate(CreationDateDictionaryKey);
    FHasModificationDate := FParametersDictionary.Contains(ModificationDateDictionaryKey);
    if FHasModificationDate then
      FModificationDate := FParametersDictionary.GetDate(ModificationDateDictionaryKey);
  end;
  SetLength(FData, 0);
  FDataStreamRef := ADictionary.StreamRef;
end;

function TdxPDFFileSpecificationData.GetData: TBytes;
begin
  if Length(FData) = 0 then
    ResolveData;
  Result := FData;
end;

procedure TdxPDFFileSpecificationData.SetData(const AValue: TBytes);
begin
  FData := AValue;
  FSize := Length(FData);
end;

procedure TdxPDFFileSpecificationData.ResolveData;
begin
  if FDataStreamRef <> nil then
    FData := FDataStreamRef.UncompressedData;
end;

{ TdxPDFCustomDestination }

class function TdxPDFCustomDestination.Parse(ACatalog: TdxPDFCatalog; AObject: TdxPDFBase): TdxPDFCustomDestination;

  function GetClassName(AArray: TdxPDFArray): string;
  begin
    if AArray.Count < 1 then
      Result := ''
    else
    begin
      Result := TdxPDFKeywords.XYZDestination;
      if (AArray.Count > 1) and (AArray[1] is TdxPDFString) then
        Result := TdxPDFString(AArray[1]).Value;
    end;
  end;

var
  AArray: TdxPDFArray;
  AClass: TdxPDFObjectClass;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if AObject <> nil then
  begin
    case AObject.ObjectType of
      otDictionary:
        AArray := TdxPDFDictionary(AObject).GetArray('D');
      otArray:
        AArray := TdxPDFArray(AObject);
      otIndirectReference:
        begin
          if ACatalog <> nil then
          begin
            AArray := ACatalog.Repository.GetArray(AObject.Number);
            if AArray = nil then
            begin
              ADictionary := ACatalog.Repository.GetDictionary(AObject.Number);
              if ADictionary <> nil then
                Result := TdxPDFCustomDestination.Parse(ACatalog, ADictionary.GetObject('D'));
            end
            else
              Result := TdxPDFCustomDestination.Parse(ACatalog, AArray);
          end;
          AArray := nil;
        end;
    else
      AArray := nil;
    end;
    if AArray <> nil then
    begin
      AClass := dxPDFGetDocumentObjectClass(GetClassName(AArray));
      if AClass <> nil then
      begin
        Result := AClass.Create(nil) as TdxPDFCustomDestination;
        Result.Read(ACatalog, AArray);
      end;
    end;
  end;
end;

class function TdxPDFCustomDestination.GetTypeName: string;
begin
  Result := 'CustomDestination';
end;

procedure TdxPDFCustomDestination.DestroySubClasses;
begin
  PageObject := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomDestination.Initialize;
begin
  inherited Initialize;
  PageObject := nil;
  FPageIndex := -1;
end;

procedure TdxPDFCustomDestination.Read(ACatalog: TdxPDFCatalog; AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FCatalog := ACatalog;
    if AArray.Count < 1 then
      TdxPDFUtils.RaiseException;
    ReadParameters(AArray);
  end;
end;

procedure TdxPDFCustomDestination.ReadParameters(AArray: TdxPDFArray);
begin
  PageObject := AArray[0];
end;

function TdxPDFCustomDestination.IsSame(ADestination: TdxPDFCustomDestination): Boolean;
begin
  Result := (FPage = ADestination.Page) and (FPageIndex = ADestination.PageIndex);
end;

procedure TdxPDFCustomDestination.ResolveInternalPage;
begin
  ResolvePage;
  if (FPage = nil) and (Catalog <> nil) and (FPageIndex >= 0) and (FPageIndex < Catalog.Pages.Count) then
  begin
    FPage := Catalog.Pages[FPageIndex];
    FPageIndex := -1;
  end;
end;

class function TdxPDFCustomDestination.GetSingleValue(AArray: TdxPDFArray): Single;
begin
  case AArray.Count of
    2:
      Result := dxPDFInvalidValue;
    3:
      Result := TdxPDFUtils.ConvertToSingle(AArray[2]);
    else
      TdxPDFUtils.RaiseException;
      Result := dxPDFInvalidValue;
  end;
end;

function TdxPDFCustomDestination.CalculatePageIndex(APages: TdxPDFPages): Integer;
begin
  if Page = nil then
    Result := FPageIndex
  else
    Result := APages.IndexOf(FPage);
end;

function TdxPDFCustomDestination.ValidateVerticalCoordinate(ATop: Single): Single;
begin
  Result := ATop;
  if TdxPDFUtils.IsDoubleValid(ATop) and (FPage <> nil) then
    Result := TdxPDFUtils.Min(ATop, Abs(FPage.CropBox.Height));
end;

function TdxPDFCustomDestination.GetPage: TdxPDFPage;
begin
  ResolvePage;
  Result := FPage;
end;

function TdxPDFCustomDestination.GetPageIndex: Integer;
begin
  ResolvePage;
  if FPageIndex >= 0 then
    Result := FPageIndex
  else
    Result := CalculatePageIndex(FCatalog.Pages);
end;

function TdxPDFCustomDestination.GetPages: TdxPDFPages;
begin
  if FCatalog <> nil then
    Result := FCatalog.Pages
  else
    Result := nil;
end;

procedure TdxPDFCustomDestination.SetPageObject(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FPageObject));
end;

procedure TdxPDFCustomDestination.ResolvePage;
begin
  if FPageObject <> nil then
  begin
    if FPageObject.ObjectType = otIndirectReference then
      FPage := Catalog.Pages.FindPage(FPageObject.Number)
    else
      if FPageObject.ObjectType = otInteger then
        FPageIndex := TdxPDFInteger(FPageObject).Value;
    PageObject := nil;
  end;
end;

{ TdxPDFFileSpecification }

constructor TdxPDFFileSpecification.Create(const AFileName: string);
begin
  inherited Create(nil);
  FFileName := AFileName;
end;

class function TdxPDFFileSpecification.Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFFileSpecification;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    Result := TdxPDFFileSpecification.Create('');
    Result.Read(ADictionary);
  end;
end;

class function TdxPDFFileSpecification.GetTypeName: string;
begin
  Result := 'FileSpec';
end;

procedure TdxPDFFileSpecification.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFileSpecificationData := TdxPDFFileSpecificationData.Create(nil);
  FAttachment := nil;
end;

procedure TdxPDFFileSpecification.DestroySubClasses;
begin
  FreeAndNil(FAttachment);
  FreeAndNil(FFileSpecificationData);
  inherited DestroySubClasses;
end;

procedure TdxPDFFileSpecification.Read(ADictionary: TdxPDFReaderDictionary);
var
  AType: string;
  AEmbeddedFileDictionary, AEmbeddedStreamDictionary, ACollectionItemDictionary: TdxPDFReaderDictionary;
begin
  inherited Read(ADictionary);
  AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
  if (AType <> '') and ((AType = 'Filespec') or (AType = GetTypeName) or (AType <> 'F')) then
  begin
    FFileSystem := ADictionary.GetString('FS');
    ReadFileName(ADictionary);
    AEmbeddedFileDictionary := ADictionary.GetDictionary('EF');
    if (AEmbeddedFileDictionary <> nil) and
      (AEmbeddedFileDictionary.TryGetStreamDictionary('F', AEmbeddedStreamDictionary) or
      AEmbeddedFileDictionary.TryGetStreamDictionary('DOS', AEmbeddedStreamDictionary) or
      AEmbeddedFileDictionary.TryGetStreamDictionary('Unix', AEmbeddedStreamDictionary)) then
      FFileSpecificationData.Read(AEmbeddedStreamDictionary);
    FDescription := ADictionary.GetTextString('Desc');
    if ADictionary.TryGetDictionary('CI', ACollectionItemDictionary) then
      FIndex := ACollectionItemDictionary.GetInteger('Index', 0);
    FRelationship := ADictionary.GetAssociatedFileRelationship;
  end;
end;

function TdxPDFFileSpecification.GetMimeType: string;
begin
  Result := FFileSpecificationData.MimeType;
end;

procedure TdxPDFFileSpecification.SetMimeType(const AValue: string);
begin
  FFileSpecificationData.MimeType := AValue;
end;

function TdxPDFFileSpecification.GetCreationDate: TDateTime;
begin
  Result := FFileSpecificationData.CreationDate;
end;

procedure TdxPDFFileSpecification.SetCreationDate(const AValue: TDateTime);
begin
  FFileSpecificationData.CreationDate := AValue;
end;

function TdxPDFFileSpecification.GetModificationDate: TDateTime;
begin
  Result := FFileSpecificationData.ModificationDate;
end;

function TdxPDFFileSpecification.GetHasModificationDate: Boolean;
begin
  Result := FFileSpecificationData.HasModificationDate;
end;

procedure TdxPDFFileSpecification.SetModificationDate(const AValue: TDateTime);
begin
  FFileSpecificationData.ModificationDate := AValue;
end;

function TdxPDFFileSpecification.GetFileData: TBytes;
begin
  Result := FFileSpecificationData.Data;
end;

procedure TdxPDFFileSpecification.SetFileData(const AValue: TBytes);
begin
  FFileSpecificationData.Data := AValue;
end;

function TdxPDFFileSpecification.GetSize: Integer;
begin
  if TdxPDFUtils.IsIntegerValid(FFileSpecificationData.Size) then
    Result := FFileSpecificationData.Size
  else
    Result := 0;
end;

function TdxPDFFileSpecification.GetAttachment: TdxPDFFileAttachment;
begin
  if FAttachment = nil then
    FAttachment := TdxPDFFileAttachment.Create(Self);
  Result := FAttachment;
end;

procedure TdxPDFFileSpecification.SetAttachment(const AValue: TdxPDFFileAttachment);
begin
  FAttachment := AValue;
end;

procedure TdxPDFFileSpecification.ReadFileName(ADictionary: TdxPDFReaderDictionary);
begin
  if not ((ADictionary <> nil) and (ADictionary.TryGetTextString('UF', FFileName) or
    ADictionary.TryGetTextString('F', FFileName) or ADictionary.TryGetTextString('DOS', FFileName) or
    ADictionary.TryGetTextString('Mac', FFileName) or ADictionary.TryGetTextString('Unix', FFileName))) then
    FFileName := '';
end;

{ TdxPDFCustomAction }

class function TdxPDFCustomAction.Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFCustomAction;
var
  AClass: TdxPDFObjectClass;
  AType, AActionType: string;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    AActionType := ADictionary.GetString(TdxPDFKeywords.ActionType);
    if (AType <> '') and (AType <> TdxPDFKeywords.Action) and (AType <> 'A') then
      TdxPDFUtils.RaiseException;
    AClass := dxPDFGetDocumentObjectClass(AActionType);
    if AClass <> nil then
    begin
      Result := AClass.Create(nil) as TdxPDFCustomAction;
      Result.Read(ADictionary);
    end;
  end;
end;

function TdxPDFCustomAction.GetNext: TdxPDFActionList;
begin
  EnsureNextActions;
  Result := FNext;
end;

procedure TdxPDFCustomAction.SetNextValue(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNextValue));
end;

procedure TdxPDFCustomAction.EnsureNextActions;
var
  I: Integer;
  AActionArray: TdxPDFArray;
begin
  if (FNext = nil) and (FNextValue <> nil) then
  begin
    FNext := TdxPDFActionList.Create;
    case FNextValue.ObjectType of
      otIndirectReference:
        AActionArray := Repository.GetArray(FNextValue.Number);
      otArray:
        AActionArray := TdxPDFArray(FNextValue);
    else
      AActionArray := nil;
    end;
    if AActionArray = nil then
      FNext.Add(FCatalog.Repository.GetAction(FNextValue))
    else
      for I := 0 to AActionArray.Count - 1 do
        FNext.Add(FCatalog.Repository.GetAction(AActionArray[I]));
  end;
end;

procedure TdxPDFCustomAction.DestroySubClasses;
begin
  NextValue := nil;
  FreeAndNil(FNext);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomAction.Execute(const AController: IdxPDFInteractivityController);
begin
// do nothing
end;

procedure TdxPDFCustomAction.Read(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    Number := ADictionary.Number;
    FCatalog := Repository.Catalog;
    NextValue := nil;
    if ADictionary.TryGetObject(TdxPDFKeywords.ActionNext, AObject) then
      NextValue := AObject;
  end;
end;

{ TdxPDFCustomTree }

constructor TdxPDFCustomTree.Create;
begin
  inherited Create;
  FDictionary := TdxPDFReferencedObjectDictionary.Create;
end;

destructor TdxPDFCustomTree.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxPDFCustomTree.InternalGetValue(const AKey: string): TdxPDFObject;
var
  AObject: TdxPDFReferencedObject;
begin
  if FDictionary.TryGetValue(AKey, AObject) then
  begin
    if AObject is TdxPDFDeferredObject then
      Result := TdxPDFDeferredObject(AObject).ResolvedObject as TdxPDFObject
    else
      Result := AObject as TdxPDFObject;
  end
  else
    Result := nil;
end;

procedure TdxPDFCustomTree.Read(ADictionary: TdxPDFReaderDictionary;
  const ANodeName: string);
var
  AArray: TdxPDFArray;
begin
  if ADictionary <> nil then
  begin
    FNodeName := ANodeName;
    if ADictionary.TryGetArray(FNodeName, AArray) then
      ReadBranch(ADictionary.Repository, AArray)
    else
      ReadNode(ADictionary.Repository, ADictionary);
  end;
end;

function TdxPDFCustomTree.CreateKey(AValue: TdxPDFBase): string;
begin
  if AValue is TdxPDFString then
    Result := TdxPDFString(AValue).Value
  else
    Result := '';
end;

function TdxPDFCustomTree.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TdxPDFCustomTree.GetItems: TDictionary<string, TdxPDFReferencedObject>;
begin
  Result := FDictionary.Items;
end;

function TdxPDFCustomTree.CreateDeferredObject(ARepository: TdxPDFDocumentRepository;
  AValue: TdxPDFBase): TdxPDFDeferredObject;
var
  AInfo: TdxPDFDeferredObjectInfo;
begin
  AInfo.Name := '';
  AInfo.Key := GetDeferredObjectKey;
  AInfo.Number := AValue.Number;
  AInfo.SourceObject := AValue;
  Result := TdxPDFDeferredObject.Create(nil, AInfo);
  Result.Repository := ARepository;
end;

procedure TdxPDFCustomTree.ReadBranch(ARepository: TdxPDFDocumentRepository; AReferences: TdxPDFArray);
var
  I, AIndex: Integer;
  AValue: TdxPDFBase;
  AKey: string;
begin
  AIndex := 0;
  for I := 0 to AReferences.Count div 2 - 1 do
  begin
    AValue := AReferences[AIndex];
    if AValue.ObjectType = otIndirectReference then
      AValue := ARepository.GetObject(AValue.Number) as TdxPDFBase;
    AKey := CreateKey(AValue);
    FDictionary.AddOrSetValue(AKey, CreateDeferredObject(ARepository, AReferences[AIndex + 1]));
    Inc(AIndex, 2);
  end;
end;

procedure TdxPDFCustomTree.ReadKids(ARepository: TdxPDFDocumentRepository; AReferences: TdxPDFArray);
var
  I: Integer;
  ADictionary: TdxPDFDictionary;
  AValue: TdxPDFBase;
begin
  for I := 0 to AReferences.Count - 1 do
  begin
    AValue := AReferences[I];
    case AValue.ObjectType of
      otIndirectReference:
        ADictionary := ARepository.GetDictionary(AValue.Number);
      otDictionary:
        ADictionary := AValue as TdxPDFDictionary;
    else
      ADictionary := nil;
      TdxPDFUtils.Abort;
    end;
    ReadNode(ARepository, ADictionary);
  end;
end;

procedure TdxPDFCustomTree.ReadNode(ARepository: TdxPDFDocumentRepository; APageObject: TdxPDFBase);
var
  ADictionary: TdxPDFReaderDictionary;
  AKids: TdxPDFArray;
begin
  if APageObject <> nil then
    case APageObject.ObjectType of
      otArray:
        ReadBranch(ARepository, TdxPDFArray(APageObject));
      otDictionary:
        begin
          ADictionary := APageObject as TdxPDFReaderDictionary;
          if ADictionary.Count > 0 then
          begin
            AKids := ADictionary.GetArray(TdxPDFKeywords.Kids);
            if AKids <> nil then
              ReadKids(ARepository, AKids)
            else
              Read(ADictionary, FNodeName);
          end;
        end;
    end;
end;

{ TdxPDFDestinationTree }

function TdxPDFDestinationTree.GetValue(const AKey: string): TdxPDFCustomDestination;
begin
  Result := InternalGetValue(AKey) as TdxPDFCustomDestination;
end;

function TdxPDFDestinationTree.GetDeferredObjectKey: string;
begin
  Result := TdxPDFCustomDestination.GetTypeName;
end;

{ TdxPDFEmbeddedFileSpecificationTree }

function TdxPDFEmbeddedFileSpecificationTree.GetValue(const AKey: string): TdxPDFFileSpecification;
begin
  Result := InternalGetValue(AKey) as TdxPDFFileSpecification;
end;

function TdxPDFEmbeddedFileSpecificationTree.GetDeferredObjectKey: string;
begin
  Result := TdxPDFFileSpecification.GetTypeName;
end;

{ TdxPDFAnnotationAppearance }

class function TdxPDFAnnotationAppearance.Parse(ADictionary: TdxPDFReaderDictionary;
  const AKey: string): TdxPDFAnnotationAppearance;
var
  AAppearanceSubDictionary: TdxPDFReaderDictionary;
  AValue: TdxPDFBase;
  APair: TPair<string, TdxPDFReferencedObject>;
  ADefaultForm: TdxPDFForm;
  AForms: TDictionary<string, TdxPDFForm>;
  AName: string;
  AForm: TdxPDFForm;
begin
  Result := nil;
  AValue := ADictionary.GetObject(AKey);
  if AValue <> nil then
  begin
    AForms := nil;
    ADefaultForm := nil;
    case AValue.ObjectType of
      otStream:
        begin
          ADefaultForm := ADictionary.GetForm(AValue.Number);
          if ADefaultForm = nil then
            Exit(nil);
        end;
      otDictionary:
        begin
          ADefaultForm := nil;
          AForms := TObjectDictionary<string, TdxPDFForm>.Create([doOwnsValues]);
          AAppearanceSubDictionary := TdxPDFReaderDictionary(AValue);
          for APair in AAppearanceSubDictionary.Items do
          begin
            AName := APair.Key;
            if APair.Value = nil then
              AForms.Add(AName, nil)
            else
            begin
              AForm := ADictionary.GetForm((APair.Value as TdxPDFBase).Number);
              AForms.Add(AName, AForm);
              if (AName = 'On') or (ADefaultForm = nil) and (AName <> 'Off') then
                ADefaultForm := AForm;
            end;
          end;
        end;
    end;
    Result := TdxPDFAnnotationAppearance.Create(ADefaultForm, AForms);
  end;
end;

constructor TdxPDFAnnotationAppearance.Create(ACatalog: TdxPDFCatalog; const ABBox: TdxRectF);
begin
  inherited Create(nil);
  FDefaultForm := TdxPDFForm.Create(ACatalog, ABBox);
end;

constructor TdxPDFAnnotationAppearance.Create(ADefaultForm: TdxPDFForm; AForms: TDictionary<string, TdxPDFForm>);
begin
  inherited Create(nil);
  FForms := AForms;
  DefaultForm := ADefaultForm;
end;

procedure TdxPDFAnnotationAppearance.DestroySubClasses;
var
  APair: TPair<string, TdxPDFForm>;
begin
  if FForms <> nil then
    for APair in FForms do
      if APair.Value = FDefaultForm then
      begin
        FForms.ExtractPair(APair.Key);
        DefaultForm := nil;
      end;
  DefaultForm := nil;
  FreeAndNil(FForms);
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationAppearance.SetForm(const AName: string; AForm: TdxPDFForm);
begin
  if AName = '' then
    DefaultForm := AForm
  else
  begin
    if FForms = nil then
      FForms := TDictionary<string, TdxPDFForm>.Create;
    FForms.Add(AName, AForm);
  end;
end;

procedure TdxPDFAnnotationAppearance.SetDefaultForm(const AValue: TdxPDFForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDefaultForm));
end;

{ TdxPDFAnnotationAppearances }

procedure TdxPDFAnnotationAppearances.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Form := nil;
end;

procedure TdxPDFAnnotationAppearances.DestroySubClasses;
begin
  Form := nil;
  FreeAndNil(FRollover);
  FreeAndNil(FNormal);
  FreeAndNil(FDown);
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationAppearances.Read(ADictionary: TdxPDFReaderDictionary; const AParentBox: TdxRectF);
begin
  inherited Read(ADictionary);
  FNormal := TdxPDFAnnotationAppearance.Parse(ADictionary, 'N');
  if (FNormal = nil) and (AParentBox <> dxRectF(cxNullRect)) then
    FNormal := TdxPDFAnnotationAppearance.Create(ADictionary.Repository.Catalog, AParentBox);
  FRollover := TdxPDFAnnotationAppearance.Parse(ADictionary, 'R');
  FDown := TdxPDFAnnotationAppearance.Parse(ADictionary, 'D');
end;

procedure TdxPDFAnnotationAppearances.Read(AForm: TdxPDFForm);
begin
  Form := AForm;
end;

procedure TdxPDFAnnotationAppearances.SetAnnotationForm(const AName: string; AForm: TdxPDFForm);
begin
  if FNormal = nil then
    FNormal := TdxPDFAnnotationAppearance.Create(nil);
  FNormal.SetForm(AName, AForm);
end;

procedure TdxPDFAnnotationAppearances.SetForm(const AValue: TdxPDFForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FForm));
end;

{ TdxPDFAnnotationBorderStyle }

class function TdxPDFAnnotationBorderStyle.Parse(ADictionary: TdxPDFReaderDictionary): TdxPDFAnnotationBorderStyle;
var
  ABorderStyleDictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if (ADictionary <> nil) and ADictionary.TryGetDictionary('BS', ABorderStyleDictionary) then
  begin
    Result := TdxPDFAnnotationBorderStyle.Create(nil);
    Result.Read(ABorderStyleDictionary);
  end;
end;

class function TdxPDFAnnotationBorderStyle.ParseLineStyle(AArray: TdxPDFArray): TdxPDFLineStyle;
begin
  if (AArray <> nil) and ((AArray.Count > 1) or (AArray.Count = 1) and (TdxPDFUtils.ConvertToDouble(AArray[0]) <> 0)) then
    Exit(TdxPDFLineStyle.Create(AArray));
  Result := TdxPDFLineStyle.CreateSolid
end;

procedure TdxPDFAnnotationBorderStyle.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := nil;
end;

procedure TdxPDFAnnotationBorderStyle.DestroySubClasses;
begin
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationBorderStyle.Read(ADictionary: TdxPDFReaderDictionary);
var
  AType: string;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    FWidth := ADictionary.GetDouble('W', 1);
    if (AType = 'Border') or (AType = 'BorderStyle') then
    begin
      FStyleName := ADictionary.GetString('S', 'S');
      FLineStyle := ParseLineStyle(ADictionary.GetArray('D'));
    end;
  end;
end;

procedure TdxPDFAnnotationBorderStyle.SetLineStyle(const AValue: TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

{ TdxPDFAnnotationBorder }

procedure TdxPDFAnnotationBorder.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := TdxPDFLineStyle.CreateSolid;
end;

procedure TdxPDFAnnotationBorder.DestroySubClasses;
begin
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationBorder.Initialize;
begin
  inherited Initialize;
  FLineWidth := DefaultLineWidth;
  FHorizontalCornerRadius := DefaultHorizontalCornerRadius;
  FVerticalCornerRadius := DefaultVerticalCornerRadius;
end;

procedure TdxPDFAnnotationBorder.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray, ALineStyleArray: TdxPDFArray;
  AValue: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary.TryGetArray('Border', AArray) and (AArray.Count >= 3) then
  begin
    FHorizontalCornerRadius := TdxPDFUtils.ConvertToDouble(AArray[0]);
    FVerticalCornerRadius := TdxPDFUtils.ConvertToDouble(AArray[1]);
    FLineWidth := TdxPDFUtils.ConvertToDouble(AArray[2]);
    if (FHorizontalCornerRadius < 0) or (FVerticalCornerRadius < 0) or (FLineWidth < 0) then
      TdxPDFUtils.Abort;
    if AArray.Count = 4 then
    begin
      AValue := AArray[3] as TdxPDFBase;
      ALineStyleArray := nil;
      try
        if AValue.ObjectType <> otArray then
        begin
          ALineStyleArray := TdxPDFArray.Create;
          if AValue.ObjectType in [otInteger, otDouble] then
            ALineStyleArray.Add(AValue);
        end
        else
        begin
          ALineStyleArray := TdxPDFArray(AValue);
          ALineStyleArray.Reference;
        end;
        LineStyle := TdxPDFAnnotationBorderStyle.ParseLineStyle(ALineStyleArray)
      finally
        dxPDFFreeObject(ALineStyleArray);
      end;
    end
    else
      LineStyle := TdxPDFLineStyle.CreateSolid;
  end;
end;

function TdxPDFAnnotationBorder.GetIsDefault: Boolean;
begin
  Result := not LineStyle.IsDashed and SameValue(HorizontalCornerRadius, DefaultHorizontalCornerRadius) and
    SameValue(LineWidth, DefaultLineWidth) and SameValue(VerticalCornerRadius, DefaultVerticalCornerRadius);
end;

procedure TdxPDFAnnotationBorder.SetLineStyle(const AValue: TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

{ TdxPDFCustomAnnotation }

class function TdxPDFCustomAnnotation.Parse(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage): TdxPDFCustomAnnotation;
var
  AClass: TdxPDFObjectClass;
  AType: string;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.Subtype);
    if AType = '' then
      AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if AType <> '' then
    begin
      AClass := dxPDFGetDocumentObjectClass(AType);
      if AClass <> nil then
      begin
        Result := AClass.Create(nil) as TdxPDFCustomAnnotation;
        Result.Read(ADictionary, APage);
      end;
    end;
  end;
end;

function TdxPDFCustomAnnotation.GetRect: TdxRectF;
begin
  Ensure;
  Result := FRect;
end;

function TdxPDFCustomAnnotation.GetContents: string;
begin
  Ensure;
  Result := FContents;
end;

function TdxPDFCustomAnnotation.GetName: string;
begin
  Ensure;
  Result := FName;
end;

procedure TdxPDFCustomAnnotation.SetAppearance(const AValue: TdxPDFAnnotationAppearances);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAppearance));
end;

function TdxPDFCustomAnnotation.GetColor: TdxPDFColor;
begin
  Ensure;
  Result := FColor;
end;

procedure TdxPDFCustomAnnotation.SetColor(const AValue: TdxPDFColor);
begin
  Ensure;
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FColor));
end;

procedure TdxPDFCustomAnnotation.SetContents(const AValue: string);
begin
  Ensure;
  FContents := AValue;
end;

procedure TdxPDFCustomAnnotation.SetDictionary(const AValue: TdxPDFReaderDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

procedure TdxPDFCustomAnnotation.SetName(const AValue: string);
begin
  Ensure;
  FName := AValue;
end;

function TdxPDFCustomAnnotation.EnsureAppearance(AState: TdxPDFDocumentState; AForm: TdxPDFForm): TdxPDFForm;
var
  ABuilder: TObject;
  ACurrentForm: TdxPDFForm;
  AIntf: IdxPDFAnnotationAppearanceBuilder;
  ANeedCreateAppearance: Boolean;
begin
  try
    ANeedCreateAppearance := NeedCreateAppearance(AForm);
  except
    ANeedCreateAppearance := True;
  end;
  ACurrentForm := AForm;
  if ANeedCreateAppearance then
  begin
    ABuilder := CreateAppearanceBuilder(AState);
    if ABuilder <> nil then
    begin
      if Supports(ABuilder, IdxPDFAnnotationAppearanceBuilder, AIntf) then
      begin
        if ACurrentForm = nil then
          ACurrentForm := CreateAppearanceForm('');
        if ACurrentForm <> nil then
          AIntf.RebuildAppearance(ACurrentForm);
        AIntf := nil;
      end
      else
        ABuilder.Free;
    end;
  end;
  Result := ACurrentForm;
end;

function TdxPDFCustomAnnotation.GetActualAppearanceForm: TdxPDFForm;
var
  AForms: TDictionary<string, TdxPDFForm>;
begin
  Ensure;
  if FAppearance <> nil then
  begin
    Result := nil;
    if FAppearanceName <> '' then
    begin
      AForms := FAppearance.Normal.Forms;
      if (AForms <> nil) and not AForms.TryGetValue(FAppearanceName, Result) then
        Result := nil;
    end;
    if (Result = nil) and UseDefaultForm then
      Result := FAppearance.Normal.DefaultForm;
  end
  else
    Result := nil;
end;

function TdxPDFCustomAnnotation.GetAppearanceName: string;
begin
  Ensure;
  Result := FAppearanceName;
end;

function TdxPDFCustomAnnotation.GetBorder: TdxPDFAnnotationBorder;
begin
  Ensure;
  Result := FBorder;
end;

procedure TdxPDFCustomAnnotation.SetAppearanceName(const AValue: string);
begin
  Ensure;
  FAppearanceName := AValue;
end;

procedure TdxPDFCustomAnnotation.SetBorder(const AValue: TdxPDFAnnotationBorder);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FBorder));
end;

class function TdxPDFCustomAnnotation.GetTypeName: string;
begin
  Result := 'Annot';
end;

procedure TdxPDFCustomAnnotation.DestroySubClasses;
begin
  FResolved := True;
  Border := nil;
  Color := nil;
  Appearance := nil;
  Dictionary := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomAnnotation.Initialize;
begin
  inherited Initialize;
  Appearance := nil;
end;

procedure TdxPDFCustomAnnotation.Read(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage);
begin
  inherited Read(ADictionary);
  FPage := APage;
  Dictionary := ADictionary;
  Number := ADictionary.Number;
end;

function TdxPDFCustomAnnotation.GetUseDefaultForm: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomAnnotation.GetVisible: Boolean;
begin
  Result := not HasFlag(TdxPDFAnnotationFlags.afHidden) and (Rect.Width <> 0) and (Rect.Height <> 0);
end;

function TdxPDFCustomAnnotation.NeedCreateAppearance(AForm: TdxPDFForm): Boolean;
begin
  Result := (AForm = nil) or (AForm.GetCommands = nil) or (AForm.GetCommands.Count = 0);
end;

procedure TdxPDFCustomAnnotation.Ensure;
begin
  if not FResolved then
  begin
    Resolve(FDictionary);
    FResolved := True;
  end;
end;

function TdxPDFCustomAnnotation.CreateAppearanceForm(const AName: string): TdxPDFForm;
begin
  Ensure;
  Result := TdxPDFForm.Create(Repository.Catalog, GetAppearanceFormBoundingBox);
  if FAppearance = nil then
    FAppearance := TdxPDFAnnotationAppearances.Create(nil);
  FAppearance.SetAnnotationForm(AName, Result);
end;

function TdxPDFCustomAnnotation.CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject;
begin
  Result := nil;
end;

function TdxPDFCustomAnnotation.GetAppearanceFormBoundingBox: TdxRectF;
begin
  Result := dxRectF(0, 0, Rect.Width, Abs(Rect.Height));
end;

procedure TdxPDFCustomAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if ADictionary.Contains('Rect') then
    FRect := ADictionary.GetRectangle('Rect')
  else
    TdxPDFUtils.Abort;
  FContents := ADictionary.GetTextString('Contents');
  FName := ADictionary.GetTextString('NM');
  FFlags := TdxPDFAnnotationFlags(ADictionary.GetInteger('F', 0));
  FAppearanceName := ADictionary.GetString('AS');
  FStructParent := ADictionary.GetInteger('StructParent');
  FColor := ADictionary.GetColor('C');
  if FColor <> nil then
    FColor.Reference;
  if ADictionary.TryGetObject('AP', AObject) then
    Appearance := ADictionary.GetAnnotationAppearance(AObject, FRect);
  FBorder := TdxPDFAnnotationBorder.Create(nil);
  FBorder.Read(ADictionary);
  FBorder.Reference;
end;

function TdxPDFCustomAnnotation.GetAppearanceForm(AState: TdxPDFDocumentState): TdxPDFForm;
begin
  Result := EnsureAppearance(AState, GetActualAppearanceForm);
end;

function TdxPDFCustomAnnotation.HasFlag(AFlags: TdxPDFAnnotationFlags): Boolean;
begin
  Result := (Integer(FFlags) and Integer(AFlags)) <> 0;
end;

{ TdxPDFFileAttachment }

constructor TdxPDFFileAttachment.Create(AFileSpecification: TdxPDFFileSpecification);
begin
  inherited Create;
  FFileSpecification := AFileSpecification;
  FFileSpecification.Attachment := Self;
end;

function TdxPDFFileAttachment.GetCreationDate: TDateTime;
begin
  Result := FFileSpecification.CreationDate;
end;

function TdxPDFFileAttachment.GetModificationDate: TDateTime;
begin
  Result := FFileSpecification.ModificationDate;
end;

function TdxPDFFileAttachment.GetMimeType: string;
begin
  Result := FFileSpecification.MimeType;
end;

function TdxPDFFileAttachment.GetData: TBytes;
begin
  Result := FFileSpecification.FileData;
end;

function TdxPDFFileAttachment.GetSize: Integer;
begin
  Result := FFileSpecification.Size;
end;

function TdxPDFFileAttachment.GetFileName: string;
begin
  Result := FFileSpecification.FileName;
end;

function TdxPDFFileAttachment.GetRelationship: TdxPDFAssociatedFileRelationship;
begin
  Result := FFileSpecification.Relationship;
end;

function TdxPDFFileAttachment.GetDescription: string;
begin
  Result := FFileSpecification.Description;
end;

function TdxPDFFileAttachment.GetModificationDateAsString: string;
begin
  if FFileSpecification.HasModificationDate then
    Result := DateTimeToStr(ModificationDate)
  else
    Result := '';
end;

function TdxPDFFileAttachment.GetSizeAsString: string;
begin
  if Size > 0 then
    Result := TdxPDFUtils.FormatFileSize(Size)
  else
    Result := '';
end;

{ TdxPDFFileAttachmentList }

procedure TdxPDFFileAttachmentList.Populate(ACatalog: TdxPDFCatalog);
var
  AComparer: IComparer<TdxPDFFileAttachment>;
begin
  if ACatalog <> nil then
  begin
    ACatalog.PopulateAttachmentList(Self);
    AComparer := TdxPDFFileAttachmentComparer.Create;
    Sort(AComparer);
  end;
end;

{ TdxPDFNames }

procedure TdxPDFNames.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPageDestinations := TdxPDFDestinationTree.Create;
  FEmbeddedFileSpecifications := TdxPDFEmbeddedFileSpecificationTree.Create;
end;

procedure TdxPDFNames.DestroySubClasses;
begin
  FreeAndNil(FEmbeddedFileSpecifications);
  FreeAndNil(FPageDestinations);
  inherited DestroySubClasses;
end;

procedure TdxPDFNames.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadPageDestinations(ADictionary.GetDictionary(TdxPDFKeywords.Destinations));
  ReadEmbeddedFileSpecifications(ADictionary.GetDictionary(TdxPDFKeywords.EmbeddedFiles));
end;

function TdxPDFNames.GetEmbeddedFileSpecification(const AName: string): TdxPDFFileSpecification;
begin
  Result := FEmbeddedFileSpecifications.GetValue(AName);
end;

function TdxPDFNames.GetPageDestination(const AName: string): TdxPDFCustomDestination;
begin
  Result := FPageDestinations.GetValue(AName);
end;

procedure TdxPDFNames.PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
var
  APair: TPair<string, TdxPDFReferencedObject>;
  ASpecification: TdxPDFFileSpecification;
begin
  for APair in FEmbeddedFileSpecifications.Items do
  begin
    ASpecification := FEmbeddedFileSpecifications.GetValue(APair.Key);
    if ASpecification <> nil then
      AList.Add(ASpecification.Attachment);
  end;
end;

procedure TdxPDFNames.ReadEmbeddedFileSpecifications(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
    FEmbeddedFileSpecifications.Read(ADictionary, TdxPDFKeywords.Names);
end;

procedure TdxPDFNames.ReadPageDestinations(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
    FPageDestinations.Read(ADictionary, TdxPDFKeywords.Names);
end;

{ TdxPDFDocumentInformation }

class function TdxPDFDocumentInformation.GetTypeName: string;
begin
  Result := TdxPDFKeywords.DocumentInfo;
end;

procedure TdxPDFDocumentInformation.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FAuthor := ADictionary.GetTextString(TdxPDFKeywords.Author);
  FCreationDate := ADictionary.GetDate(TdxPDFKeywords.CreationDate);
  FApplication := ADictionary.GetTextString(TdxPDFKeywords.Creator);
  FKeywords := ADictionary.GetTextString(TdxPDFKeywords.Keywords);
  FModificationDate := ADictionary.GetDate(TdxPDFKeywords.ModDate);
  FProducer := ADictionary.GetTextString(TdxPDFKeywords.Producer);
  FSubject := ADictionary.GetTextString(TdxPDFKeywords.Subject);
  FTitle := ADictionary.GetTextString(TdxPDFKeywords.Title);
end;

{ TdxPDFCommandOperandStack }

constructor TdxPDFCommandOperandStack.Create;
begin
  inherited Create;
  FStack := TObjectStack<TdxPDFBase>.Create;
end;

destructor TdxPDFCommandOperandStack.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

function TdxPDFCommandOperandStack.TryPopLastName: string;
var
  ALastIndex: Integer;
  ALastParameter: TdxPDFBase;
begin
  ALastIndex := FStack.Count - 1;
  if ALastIndex < 0 then
    TdxPDFUtils.RaiseTestException('List index out of bounds');
  ALastParameter := PopAsObject;
  if ALastParameter <> nil then
  begin
    if (ALastParameter <> nil) and (ALastParameter.ObjectType in [otName, otString]) then
    begin
      Result := TdxPDFString(ALastParameter).Value;
      dxPDFFreeObject(ALastParameter);
      Exit;
    end;
    Result := '';
    Push(ALastParameter);
  end;
end;

function TdxPDFCommandOperandStack.PopAsArray: TdxPDFArray;
var
  AObject: TdxPDFBase;
begin
  AObject := PopAsObject;
  if (AObject <> nil) and (AObject.ObjectType = otArray) then
    Result := TdxPDFArray(AObject)
  else
    Result := nil;
end;

function TdxPDFCommandOperandStack.PopAsBytes: TBytes;
var
  AResult: string;
  I: Integer;
begin
  AResult := PopAsString;
  SetLength(Result, Length(AResult));
  for I := 0 to Length(Result) - 1 do
    Result[I] := Byte(AResult[I + 1]);
end;

function TdxPDFCommandOperandStack.PopAsInteger: Integer;
var
  AObject: TdxPDFBase;
begin
  Result := 0;
  AObject := PopAsObject;
  if AObject <> nil then
    try
      if AObject.ObjectType = otInteger then
        Result := TdxPDFInteger(AObject).Value
    finally
      dxPDFFreeObject(AObject);
    end;
end;

function TdxPDFCommandOperandStack.PopAsObject: TdxPDFBase;
begin
  Result := nil;
  if FStack.Count > 0 then
    Result := FStack.Extract
  else
    try
      TdxPDFUtils.RaiseTestException('Stack is empty');
    except
    end;
end;

function TdxPDFCommandOperandStack.PopAsSingle: Single;
var
  AObject: TdxPDFBase;
begin
  Result := 0;
  AObject := PopAsObject;
  if AObject <> nil then
    try
      case AObject.ObjectType of
        otDouble:
          Result := TdxPDFDouble(AObject).Value;
        otInteger:
          Result := TdxPDFInteger(AObject).Value;
      end;
    finally
      dxPDFFreeObject(AObject);
    end;
end;

function TdxPDFCommandOperandStack.PopAsString: string;
var
  AObject: TdxPDFBase;
begin
  Result := '';
  AObject := PopAsObject;
  if AObject <> nil then
    try
      case AObject.ObjectType of
        otName:
          Result := TdxPDFName(AObject).Value;
        otString:
          Result := TdxPDFString(AObject).Value;
      end;
    finally
      dxPDFFreeObject(AObject);
    end;
end;

procedure TdxPDFCommandOperandStack.Clear;
begin
  FStack.Clear;
end;

procedure TdxPDFCommandOperandStack.Push(AObject: TdxPDFBase);
begin
  FStack.Push(AObject);
end;

function TdxPDFCommandOperandStack.GetCount: Integer;
begin
  Result := FStack.Count;
end;

{ TdxPDFCustomCommand }

constructor TdxPDFCustomCommand.Create;
begin
  inherited Create;
end;

constructor TdxPDFCustomCommand.Create(AOperands: TdxPDFCommandOperandStack; AResources: TdxPDFResources);
begin
  Create;
end;

class function TdxPDFCustomCommand.GetObjectType: TdxPDFBaseType;
begin
  Result := otCommand;
end;

procedure TdxPDFCustomCommand.WriteCommandName(AStream: TdxPDFWriterStream);
begin
  AStream.WriteSpace;
  AStream.WriteString(GetName);
end;

procedure TdxPDFCustomCommand.WriteOperand(AStream: TdxPDFWriterStream; AOperand: Double);
begin
  AStream.WriteSpace;
  AStream.WriteDouble(AOperand);
end;

procedure TdxPDFCustomCommand.WriteOperand(AStream: TdxPDFWriterStream; AOperand: Integer);
begin
  AStream.WriteSpace;
  AStream.WriteInt(AOperand);
end;

procedure TdxPDFCustomCommand.WriteOperand(AStream: TdxPDFWriterStream; const AOperand: TdxSizeF);
begin
  WriteOperand(AStream, AOperand.cx);
  WriteOperand(AStream, AOperand.cy);
end;

procedure TdxPDFCustomCommand.WriteOperand(AStream: TdxPDFWriterStream; const AOperand: TdxPointF);
begin
  WriteOperand(AStream, AOperand.X);
  WriteOperand(AStream, AOperand.Y);
end;

procedure TdxPDFCustomCommand.WriteOperand(AStream: TdxPDFWriterStream; const AOperand: string);
begin
  AStream.WriteSpace;
  AStream.WriteString(AOperand);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AStream: TdxPDFWriterStream; AOperand: Single);
begin
  WriteOperand(AStream, AOperand);
  WriteCommandName(AStream);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AStream: TdxPDFWriterStream; AOperand: Integer);
begin
  AStream.WriteSpace;
  AStream.WriteInt(AOperand);
  WriteCommandName(AStream);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AStream: TdxPDFWriterStream; const AOperand: string);
begin
  WriteOperand(AStream, AOperand);
  WriteCommandName(AStream);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AStream: TdxPDFWriterStream; const AOperand: TdxPointF);
begin
  WriteOperand(AStream, AOperand);
  WriteCommandName(AStream);
end;

class function TdxPDFCustomCommand.GetName: string;
begin
  Result := '';
end;

{ TdxPDFLineStyle }

class function TdxPDFLineStyle.CreateSolid: TdxPDFLineStyle;
begin
  Result := TdxPDFLineStyle.Create;
end;

constructor TdxPDFLineStyle.Create(const APattern: TDoubleDynArray; APhase: Double);
begin
  inherited Create;
  FPattern := APattern;
  FPhase := APhase;
end;

constructor TdxPDFLineStyle.Create(APattern: TdxPDFReferencedObject);
var
  APhase: TdxPDFInteger;
begin
  APhase := TdxPDFInteger.Create(0);
  try
    Create(APattern, APhase);
  finally
    APhase.Free;
  end;
end;

constructor TdxPDFLineStyle.Create(APattern, APhase: TdxPDFReferencedObject);
begin
  inherited Create;
  if APhase <> nil then
    FPhase := AsDouble(APhase as TdxPDFNumericObject);
  if APattern <> nil then
    ReadPattern(APattern as TdxPDFArray);
end;

constructor TdxPDFLineStyle.Create(AParameters: TdxPDFArray);
begin
  inherited Create;
  if AParameters.Count = 0 then
    Create(AParameters[0], AParameters[1]);
end;

function TdxPDFLineStyle.AsDouble(AValue: TdxPDFNumericObject): Double;
begin
  Result := TdxPDFNumericObjectAccess(AValue).InternalValue;
end;

procedure TdxPDFLineStyle.ReadPattern(APattern: TdxPDFArray);
var
  I: Integer;
begin
  SetLength(FPattern, 0);
  for I := 0 to APattern.Count - 1 do
  begin
    TdxPDFUtils.AddValue(AsDouble(APattern[I] as TdxPDFNumericObject), FPattern);
    if FPattern[I] < 0 then
      TdxPDFUtils.AddValue(AsDouble(APattern[I] as TdxPDFNumericObject), FPattern);
  end;
end;

function TdxPDFLineStyle.IsDashed: Boolean;
begin
  Result := Length(FPattern) <> 0;
end;

{ TdxPDFDocumentRepository }

constructor TdxPDFDocumentRepository.Create(AStream: TStream);

  function GetTempFolderPath: string;
  begin
    if sdxPDFTempFolder = '' then
      Result := TPath.GetTempPath
    else
      Result := sdxPDFTempFolder;
  end;

begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FParser := TdxPDFDocumentParser.Create(Self, AStream);
  FSharedResources := TdxPDFUniqueReferences.Create;
  FEncryptionInfo := nil;
  FStream := AStream;
  FFolderName := GetTempFolderPath + dxGenerateID + TPath.DirectorySeparatorChar;
  FFontDataStorage := TdxPDFFontDataStorage.Create(FFolderName);
  FImageDataStorage := TdxPDFDocumentImageDataStorage.Create(300);
end;

destructor TdxPDFDocumentRepository.Destroy;
begin
  FreeAndNil(FImageDataStorage);
  FreeAndNil(FFontDataStorage);
  if FEncryptionInfo <> nil then
    FreeAndNil(FEncryptionInfo);
  FreeAndNil(FStream);
  FreeAndNil(FSharedResources);
  FreeAndNil(FParser);
  if DirectoryExists(FFolderName) then
    RemoveDir(FFolderName);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TdxPDFDocumentRepository.Clear;
begin
  inherited Clear;
  FFontDataStorage.Clear;
  FSharedResources.Clear;
end;

function TdxPDFDocumentRepository.CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
begin
  Result := FEncryptionInfo.CheckPassword(AAttemptsLimit, AOnGetPasswordEvent);
end;

function TdxPDFDocumentRepository.CreateFont(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont;
begin
  EnterCriticalSection(FLock);
  try
    Result := TdxPDFCustomFont.Parse(AOwner, ADictionary);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFDocumentRepository.CreateImage(AOwner: TdxPDFObject; ADictionary: TdxPDFReaderDictionary): TdxPDFDocumentImage;
begin
  EnterCriticalSection(FLock);
  try
    if (ADictionary <> nil) and not FImageDataStorage.TryGetReference(ADictionary.Number, Result) then
    begin
      Result := TdxPDFDocumentImage.Create(AOwner);
      Result.Read(ADictionary);
      FImageDataStorage.AddReference(Result);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFDocumentRepository.GetAction(ANumber: Integer): TdxPDFCustomAction;
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := GetObject(ANumber);
  if (AObject <> nil) and (AObject is TdxPDFCustomAction) then
    Result := TdxPDFCustomAction(AObject)
  else
    if AObject is TdxPDFReaderDictionary then
    begin
      Result := TdxPDFCustomAction.Parse(TdxPDFReaderDictionary(AObject));
      ReplaceObject(ANumber, Result);
    end
    else
      Result := nil;
end;

function TdxPDFDocumentRepository.GetAction(AObject: TdxPDFBase): TdxPDFCustomAction;
begin
  Result := GetAction(AObject.Number);
end;

function TdxPDFDocumentRepository.GetAnnotation(ANumber: Integer; APage: TdxPDFPage): TdxPDFCustomAnnotation;
begin
  if not TryGetAnnotation(ANumber, Result) then
  begin
    Result := TdxPDFCustomAnnotation.Parse(GetDictionary(ANumber), APage);
    if Result <> nil then
    begin
      Result.Number := ANumber;
      ReplaceObject(Result.Number, Result);
    end;
  end;
end;

function TdxPDFDocumentRepository.GetDestination(ANumber: Integer): TdxPDFCustomDestination;
var
  AAction: TdxPDFJumpAction;
  AActionType, AType: string;
  ADictionary: TdxPDFDictionary;
begin
  ADictionary := GetDictionary(ANumber);
  if ADictionary <> nil then
  begin
    AActionType := ADictionary.GetString(TdxPDFKeywords.ActionType);
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AActionType <> '') and ((AType = '') or (AType = TdxPDFKeywords.Action) or (AType = 'A')) then
    begin
      AAction := GetAction(ANumber) as TdxPDFJumpAction;
      if AAction <> nil then
      begin
        ReplaceObject(ANumber, AAction);
        Exit(TdxPDFJumpActionAccess(AAction).Destination);
      end;
    end;
  end;
  Result := GetDestination(GetObject(ANumber) as TdxPDFBase);
  if Result <> nil then
    ReplaceObject(ANumber, Result);
end;

function TdxPDFDocumentRepository.GetDestination(AObject: TdxPDFBase): TdxPDFCustomDestination;
begin
  Result := TdxPDFCustomDestination.Parse(Catalog, AObject);
end;

function TdxPDFDocumentRepository.GetDictionary(ANumber: Integer): TdxPDFReaderDictionary;
begin
  Result := inherited GetDictionary(ANumber) as TdxPDFReaderDictionary;
end;

function TdxPDFDocumentRepository.GetInteractiveFormField(AForm: TdxPDFInteractiveForm;
  AParent: TdxPDFInteractiveFormField; AValue: TdxPDFBase): TdxPDFInteractiveFormField;
var
  ADictionary: TdxPDFReaderDictionary;
  AWidget: TdxPDFWidgetAnnotation;
begin
  Result := nil;
  if (AValue <> nil) then
  begin
    ADictionary := GetDictionary(AValue.Number);
    if ADictionary <> nil then
      Result := TdxPDFInteractiveFormField.Parse(AForm, AParent, ADictionary, AValue.Number)
    else
    begin
      AWidget := GetWidget(AValue.Number) as TdxPDFWidgetAnnotation;
      if AWidget.InteractiveFormField <> nil then
        Result := AWidget.InteractiveFormField
      else
        Result := TdxPDFInteractiveFormField.Parse(AForm, AParent, TdxPDFWidgetAnnotationAccess(AWidget).Dictionary, AValue.Number)
    end;
  end;
end;

function TdxPDFDocumentRepository.GetPage(ANumber: Integer): TdxPDFPage;
begin
  Result := Catalog.Pages.FindPage(ANumber);
end;

function TdxPDFDocumentRepository.GetString(ANumber: Integer): string;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(ANumber) as TdxPDFBase;
  if AObject is TdxPDFString then
    Result := TdxPDFString(AObject).Value
  else
    Result := '';
end;

function TdxPDFDocumentRepository.GetWidget(ANumber: Integer): TdxPDFCustomAnnotation;
var
  I: Integer;
  AAnnotation, AObject: TdxPDFReferencedObject;
begin
  Result := nil;
  AObject := GetObject(ANumber);
  if not (AObject is TdxPDFWidgetAnnotation) then
  begin
    for I := 0 to Catalog.Pages.Count - 1 do
      for AAnnotation in Catalog.Pages[I].Data.Annotations do
        if (AAnnotation is TdxPDFWidgetAnnotation) and ((AAnnotation as TdxPDFBase).Number = ANumber) then
        begin
          Result := TdxPDFWidgetAnnotation(AAnnotation);
          Break;
        end;
  end
  else
    Result := AObject as TdxPDFCustomAnnotation;
end;

function TdxPDFDocumentRepository.IsSharedResources(AResources: TdxPDFResources): Boolean;
begin
  Result := (AResources <> nil) and FSharedResources.ContainsKey(AResources.ID, AResources.Number);
end;

function TdxPDFDocumentRepository.IsValidReferences: Boolean;
var
  AKey: Integer;
  AReference: TdxPDFReference;
begin
  Result := True;
  for AKey in References.Keys do
    if References[AKey] is TdxPDFReference then
    begin
      AReference := TdxPDFReference(References[AKey]);
      if AReference.IsSlot and (AReference.Offset <> 0) and not FParser.IsObjectValid(AReference) then
      begin
        Remove(AKey);
        Result := False;
      end;
    end;
end;

function TdxPDFDocumentRepository.TryGetAnnotation(ANumber: Integer; out AAnnotation: TdxPDFCustomAnnotation): Boolean;
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := GetObject(ANumber);
  Result := AObject is TdxPDFCustomAnnotation;
  if Result then
    AAnnotation := TdxPDFCustomAnnotation(AObject)
  else
    AAnnotation := nil;
end;

procedure TdxPDFDocumentRepository.AddStreamElement(ANumber: Integer; AObject: TdxPDFReferencedObject);
begin
  TryAdd(ANumber, AObject, False);
end;

procedure TdxPDFDocumentRepository.ReadEncryptionInfo(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
begin
  if FEncryptionInfo <> nil then
    FreeAndNil(FEncryptionInfo);
  FEncryptionInfo := TdxPDFEncryptionInfo.Create(ADictionary, ADocumentID);
end;

procedure TdxPDFDocumentRepository.RemoveCorruptedObjects;
var
  AKey: Integer;
  AValue: TdxPDFReferencedObject;
  AList: TList<Integer>;
begin
  AList:= TList<Integer>.Create;
  AList.AddRange(References.Keys);
  try
    for AKey in AList do
    if References.TryGetValue(AKey, AValue) and not (AValue is TdxPDFStreamElement) then
      Remove(AKey);
  finally
    AList.Free;
  end;
end;

function TdxPDFDocumentRepository.ResolveObject(ANumber: Integer): TdxPDFReferencedObject;
begin
  EnterCriticalSection(FLock);
  try
    if not References.TryGetValue(ANumber, Result) then
      if not References.TryGetValue(ANumber, Result) then
        Exit(nil);
    if Result is TdxPDFBase then
      case TdxPDFBase(Result).ObjectType of
        otIndirectObject:
          Result := ResolveIndirectObject(TdxPDFIndirectObject(Result));
        otIndirectReference:
          Result := ResolveIndirectReference(TdxPDFReference(Result));
        otStreamElement:
          Result := ResolveStreamElement(TdxPDFStreamElement(Result));
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFDocumentRepository.IsResourcesShared(AResources: TdxPDFResources): Boolean;
begin
  Result := FSharedResources.ContainsValue(AResources);
end;

function TdxPDFDocumentRepository.GetResources(ADictionary: TdxPDFReaderDictionary): TdxPDFResources;

  function DoRead(ANumber: Integer; ADictionary: TdxPDFReaderDictionary): TdxPDFResources;
  var
    AObject: TdxPDFBase;
    ATempResources: TdxPDFResources;
  begin
    if not TdxPDFUtils.IsIntegerValid(ANumber) or not FSharedResources.TryGetValue(ANumber, AObject) then
    begin
      ATempResources := TdxPDFResources.Create(nil);
      ATempResources.Read(ADictionary);
      FSharedResources.Add(ATempResources.ID, ANumber, ATempResources)
    end
    else
      ATempResources := AObject as TdxPDFResources;
    Result := ATempResources;
  end;

var
  ANumber: Integer;
  AResourcesDictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  AResourcesDictionary := ADictionary.GetDictionary(TdxPDFKeywords.Resources);
  if AResourcesDictionary <> nil then
  begin
    ANumber := AResourcesDictionary.Number;
    if not TdxPDFUtils.IsIntegerValid(ANumber) then
    begin
      ANumber := ADictionary.Number;
      if not TdxPDFUtils.IsIntegerValid(ANumber) and (ADictionary.StreamRef <> nil) then
        ANumber := ADictionary.StreamRef.Number;
    end;
    Result := DoRead(ANumber, AResourcesDictionary);
  end;
end;

function TdxPDFDocumentRepository.CreateObjectParser(AObjectNumber: Integer): TdxPDFStructureParser;
begin
  if EncryptionInfo <> nil then
    Result := TdxPDFEncryptedStructureParser.Create(Self, AObjectNumber)
  else
    Result := TdxPDFStructureParser.Create(Self);
end;

function TdxPDFDocumentRepository.ResolveIndirectObject(AObject: TdxPDFIndirectObject): TdxPDFReferencedObject;
var
  AParser: TdxPDFStructureParser;
begin
  AParser := CreateObjectParser(AObject.Number);
  try
    Result := AParser.ReadObject(AObject.Data);
    ReplaceObject(AObject.Number, Result);
  finally
    AParser.Free;
  end;
end;

function TdxPDFDocumentRepository.ResolveIndirectReference(AReference: TdxPDFReference): TdxPDFReferencedObject;
var
  AObject: TdxPDFBase;
begin
  AObject := FParser.ReadIndirectObject(AReference.Offset);
  try
    Result := ResolveIndirectObject(AObject as TdxPDFIndirectObject);
  finally
    AObject.Free;
  end;
end;

function TdxPDFDocumentRepository.ResolveStreamElement(AElement: TdxPDFStreamElement): TdxPDFReferencedObject;
var
  AStream: TdxPDFStream;
begin
  Result := GetObject(AElement.Number);
  if Result is TdxPDFBase then
  begin
    if TdxPDFBase(Result).ObjectType <> otObjectStream then
    begin
      if TdxPDFBase(Result).ObjectType = otStream then
        AStream := Result as TdxPDFStream
      else
        AStream := GetStream(AElement.Number);
      TdxPDFBase(AStream).Number := AElement.Number;
      Result := TdxPDFReaderObjectStream.Create(AElement.Number, AStream);
      Replace(AElement.Number, Result);
    end;
    if TdxPDFBase(Result).ObjectType <> otStream then
      Result := TdxPDFReaderObjectStream(Result).Objects[AElement.Index] as TdxPDFBase
    else
      Result := AElement;
  end;
end;

procedure TdxPDFDocumentRepository.ReplaceObject(ANumber: Integer; AObject: TdxPDFReferencedObject);
begin
  if AObject <> nil then
  begin
    Replace(ANumber, AObject);
    TdxPDFBase(AObject).Number := ANumber;
  end;
end;

{ TdxPDFReaderDictionary }

constructor TdxPDFReaderDictionary.Create(ARepository: TdxPDFDocumentRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

function TdxPDFReaderDictionary.GetAction(const AKey: string): TdxPDFCustomAction;
var
  AObject: TdxPDFBase;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if TryGetObject(AKey, AObject) then
  begin
    case AObject.ObjectType of
      otIndirectReference:
        ADictionary := Repository.GetObject(AObject.Number) as TdxPDFReaderDictionary;
      otDictionary:
        ADictionary := AObject as TdxPDFReaderDictionary;
    else
      ADictionary := nil;
    end;
    if ADictionary <> nil then
      Result := TdxPDFCustomAction.Parse(ADictionary);
  end;
end;

function TdxPDFReaderDictionary.GetAnnotationAppearance(AObject: TdxPDFBase;
  const AParentBBox: TdxRectF): TdxPDFAnnotationAppearances;
var
  AResolvedObject: TdxPDFBase;
begin
  Result := nil;
  if AObject <> nil then
    case AObject.ObjectType of
      otDictionary:
        begin
          Result := TdxPDFAnnotationAppearances.Create(nil);
          Result.Read(TdxPDFReaderDictionary(AObject), AParentBBox);
        end;
      otStream:
        Result := TdxPDFAnnotationAppearances.Create(TdxPDFForm.CreateForm(TdxPDFStream(AObject), nil));
      otIndirectReference:
        begin
          AResolvedObject := Repository.GetObject(AObject.Number) as TdxPDFBase;
          if not(AResolvedObject is TdxPDFAnnotationAppearances) then
          begin
            if AResolvedObject.ObjectType = otStream then
            begin
              Result := TdxPDFAnnotationAppearances.Create(nil);
              Result.Read(GetForm(AResolvedObject.Number));
            end
            else
              Result := GetAnnotationAppearance(AResolvedObject, AParentBBox);
          end
          else
            Result := TdxPDFAnnotationAppearances(AResolvedObject);
        end
    end;
end;

function TdxPDFReaderDictionary.GetAnnotationHighlightingMode: TdxPDFAnnotationHighlightingMode;
var
  AName: string;
begin
  AName := GetString('H');
  if AName = 'N' then
    Result := ahmNone
  else
    if AName = 'O' then
      Result := ahmOutline
    else
    if AName = 'P' then
      Result := ahmPush
    else
      if AName = 'Y' then
        Result := ahmToggle
      else
        Result := ahmInvert;
end;

function TdxPDFReaderDictionary.GetAppearance(AResources: TdxPDFResources): TdxPDFReferencedObjects;
var
  AData: TBytes;
begin
  AData := GetBytes(TdxPDFKeywords.DictionaryAppearance);
  if (Length(AData) > 0) and (AResources <> nil) then
  begin
    Result := TdxPDFReferencedObjects.Create;
    TdxPDFCommandStreamParser.Parse(Repository, AData, Result, AResources);
  end
  else
    Result := nil;
end;

function TdxPDFReaderDictionary.GetColor(const AKey: string): TdxPDFColor;
var
  AArray: TdxPDFArray;
  AComponents: TDoubleDynArray;
  AValue: Double;
  I: Integer;
begin
  Result := nil;
  AArray := GetArray(AKey);
  if (AArray <> nil) and (AArray.Count in [1, 3, 4]) then
  begin
    SetLength(AComponents, AArray.Count);
    for I := 0 to AArray.Count - 1 do
    begin
      AValue := TdxPDFUtils.ConvertToDouble(AArray[I]);
      AComponents[I] := IfThen(AValue < 0, 0, IfThen(AValue > 1, 1, AValue));
    end;
    Result := TdxPDFColor.Create(AComponents);
  end;
end;

function TdxPDFReaderDictionary.GetDeferredFormFieldCollection(const AKey: string): TdxPDFInteractiveFormFieldCollection;
begin
  Result := nil;
end;

function TdxPDFReaderDictionary.GetDestinationInfo(const AKey: string): TdxPDFDestinationInfo;
var
  ANumber: Integer;
  AObject: TdxPDFBase;
begin
  Result := TdxPDFDestinationInfo.Invalid;
  if TryGetObject(AKey, AObject) then
    case AObject.ObjectType of
      otIndirectReference:
        begin
          ANumber := AObject.Number;
          AObject := Repository.GetDestination(ANumber);
          if AObject <> nil then
            Result := TdxPDFDestinationInfo.Create(AObject as TdxPDFCustomDestination)
          else
            Result := TdxPDFDestinationInfo.Create(Repository.GetString(ANumber))
        end;
      otString, otName:
        Result := TdxPDFDestinationInfo.Create(TdxPDFString(AObject).Value);
      otArray:
        Result := TdxPDFDestinationInfo.Create(Repository.GetDestination(TdxPDFArray(AObject)));
    else
      TdxPDFUtils.RaiseException;
    end;
end;

function TdxPDFReaderDictionary.GetDictionary(const AKey: string): TdxPDFReaderDictionary;
begin
  Result := inherited GetDictionary(AKey) as TdxPDFReaderDictionary;
end;

function TdxPDFReaderDictionary.GetForm(ANumber: Integer): TdxPDFForm;
var
  AObjectType: string;
  AStream: TdxPDFStream;
begin
  Result := nil;
  AStream := Repository.GetStream(ANumber);
  if AStream <> nil then
  begin
    AObjectType := AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AObjectType = '') or (AObjectType = TdxPDFKeywords.XObject) then
      Result := TdxPDFForm.CreateForm(AStream, nil);
  end;
end;

function TdxPDFReaderDictionary.GetObject(const AKey: string): TdxPDFBase;
begin
  Result := inherited GetObject(AKey);
  if (Result <> nil) and (Result.ObjectType = otIndirectReference) and (FRepository <> nil) then
    Result := FRepository.GetObject(Result.Number) as TdxPDFBase;
end;

function TdxPDFReaderDictionary.GetObjectNumber(const AKey: string): Integer;
begin
  if Contains(AKey) then
  begin
    Result := inherited GetObject(AKey).Number;
    if Result = 0 then
      Result := dxPDFInvalidValue;
  end
  else
    Result := dxPDFInvalidValue;
end;

function TdxPDFReaderDictionary.GetTextJustification: TdxPDFTextJustification;
begin
  Result := TdxPDFTextJustification(GetInteger('Q', 0));
end;

function TdxPDFReaderDictionary.GetAssociatedFileRelationship: TdxPDFAssociatedFileRelationship;
var
  AName: string;
begin
  Result := frSource;
  if TryGetString('AFRelationship', AName) then
    if AName = 'Data' then
      Result := frData
    else
      if AName = 'Alternative' then
        Result := frAlternative
      else
        if AName = 'Supplement' then
          Result := frSupplement
        else
          if AName = 'EncryptedPayload' then
            Result := frEncryptedPayload
          else
            if AName = 'Unspecified' then
              Result := frUnspecified;
end;

function TdxPDFReaderDictionary.TryGetDictionary(const AKey: string; out AValue: TdxPDFReaderDictionary): Boolean;
begin
  AValue := GetDictionary(AKey);
  Result := AValue <> nil;
end;

function TdxPDFReaderDictionary.TryGetStreamDictionary(const AKey: string; out AValue: TdxPDFReaderDictionary): Boolean;
var
  AStream: TdxPDFStream;
begin
  Result := TryGetStream(AKey, AStream);
  if Result then
  begin
    AValue := AStream.Dictionary as TdxPDFReaderDictionary;
    AValue.StreamRef := AStream;
  end
  else
    Result := TryGetDictionary(AKey, AValue);
end;

function TdxPDFReaderDictionary.GetResources(const AKey: string): TdxPDFResources;
var
  AObject: TdxPDFBase;
begin
  Result := TdxPDFResources.Create(nil);
  if TryGetObject(AKey, AObject) then
    Result.Read(Repository.GetDictionary(AObject.Number));
end;

{ TdxPDFAcroFormField }

constructor TdxPDFAcroFormField.Create;
begin
  inherited Create;
  FDocumentState := nil;
  Field := nil;
end;

destructor TdxPDFAcroFormField.Destroy;
begin
  Field := nil;
  inherited Destroy;
end;

function TdxPDFAcroFormField.GetAnnotation: TdxPDFCustomAnnotation;
begin
  if Field <> nil then
    Result := Field.Widget
  else
    Result := nil;
end;

function TdxPDFAcroFormField.GetBounds: TdxPDFOrientedRect;
begin
  Result := TdxPDFOrientedRect.Create(Annotation.Rect);
end;

function TdxPDFAcroFormField.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPDFAcroFormField.GetFlags: TdxPDFInteractiveFormFieldFlags;
begin
  Result := ffNone;
end;

function TdxPDFAcroFormField.GetHint: string;
begin
  Result := '';
end;

function TdxPDFAcroFormField.GetInteractiveOperation: TdxPDFInteractiveOperation;
begin
  Result := Result.Invalid;
end;

function TdxPDFAcroFormField.GetName: string;
begin
  if Field <> nil then
    Result := Field.FullName
  else
    Result := '';
end;

procedure TdxPDFAcroFormField.SetField(const AValue: TdxPDFInteractiveFormField);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FField));
end;

function TdxPDFAcroFormField.GetPage: TdxPDFPage;
begin
  if Annotation <> nil then
    Result := Annotation.Page
  else
    Result := nil;
end;

function TdxPDFAcroFormField.GetPageIndex: Integer;
begin
  Result := FDocumentState.GetPageIndex(Page);
end;

function TdxPDFAcroFormField.GetRect: TdxRectF;
begin
  Result := Bounds.Rect;
end;

{ TdxPDFAcroFormActionField }

function TdxPDFAcroFormActionField.GetCursor: TCursor;
begin
  if GetInteractiveOperation.IsValid then
    Result := crHandPoint
  else
    Result := inherited GetCursor;
end;

function TdxPDFAcroFormActionField.GetInteractiveOperation: TdxPDFInteractiveOperation;
begin
  Result := (Annotation as TdxPDFActionAnnotation).InteractiveOperation;
end;

function TdxPDFAcroFormActionField.IsResetFocusingNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxPDFAcroFormActionField.ExecuteOperation(const AController: IdxPDFInteractivityController);
begin
  AController.ExecuteOperation(Self);
end;

{ TdxPDFAnnotationField }

destructor TdxPDFAnnotationField.Destroy;
begin
  SetAnnotation(nil);
  inherited Destroy;
end;

procedure TdxPDFAnnotationField.SetAnnotation(const AValue: TdxPDFCustomAnnotation);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAnnotation));
end;

function TdxPDFAnnotationField.GetAnnotation: TdxPDFCustomAnnotation;
begin
  Result := FAnnotation;
end;

function TdxPDFAnnotationField.GetBounds: TdxPDFOrientedRect;
begin
  Result := TdxPDFOrientedRect.Create(TdxPDFMarkupAnnotationAccess(FAnnotation).Bounds);
end;

function TdxPDFAnnotationField.GetHint: string;
begin
  Result := TdxPDFMarkupAnnotationAccess(FAnnotation).Hint;
end;

function TdxPDFAnnotationField.GetHitCode: Integer;
begin
  Result := hcAnnotationObject;
end;

{ TdxPDFHyperlink }

function TdxPDFHyperlink.GetHitCode: Integer;
begin
  Result := hcHyperlink;
end;

function TdxPDFHyperlink.GetHint: string;
begin
  Result := TdxPDFLinkAnnotation(FAnnotation).Hint;
end;

{ TdxPDFRecognizedContent }

constructor TdxPDFRecognizedContent.Create;
begin
  inherited Create;
  FAcroFormFields := TdxPDFAcroFormFieldList.Create;
  FAnnotationFields := TdxPDFAnnotationFieldList.Create;
  FAttachments := TList<TdxPDFFileAttachment>.Create;
  FHyperlinks := TdxPDFHyperlinkList.Create;
  FImages := TdxPDFImageList.Create;
  FTextLines := TdxPDFTextLineList.Create;
end;

destructor TdxPDFRecognizedContent.Destroy;
begin
  FreeAndNil(FTextLines);
  FreeAndNil(FImages);
  FreeAndNil(FHyperlinks);
  FreeAndNil(FAttachments);
  FreeAndNil(FAnnotationFields);
  FreeAndNil(FAcroFormFields);
  inherited Destroy;
end;

function TdxPDFRecognizedContent.GetText: string;
var
  ARange: TdxPDFPageTextRange;
begin
  ARange := TdxPDFPageTextRange.Create(0);
  Result := TdxPDFTextUtils.ConvertToString(ARange.StartPosition, ARange.EndPosition, FTextLines);
end;

procedure TdxPDFRecognizedContent.AddAnnotationField(AField: TdxPDFAnnotationField);
begin
  FAnnotationFields.Add(AField);
  if AField.HitCode = hcAttachment then
    FAttachments.Add(TdxPDFFileAttachmentAnnotationField(AField).Attachment);
end;

{ TdxPDFDestinationInfo }

class function TdxPDFDestinationInfo.Create(ADestination: TdxPDFCustomDestination): TdxPDFDestinationInfo;
begin
  Result := Invalid;
  Result.FDestination := ADestination;
  ADestination.Reference;
end;

class function TdxPDFDestinationInfo.Create(const AName: string): TdxPDFDestinationInfo;
begin
  Result := Invalid;
  Result.FName := AName;
end;

class function TdxPDFDestinationInfo.Invalid: TdxPDFDestinationInfo;
begin
  Result.FDestination := nil;
  Result.FName := '';
end;

procedure TdxPDFDestinationInfo.Finalize;
begin
  dxPDFFreeObject(FDestination);
end;

function TdxPDFDestinationInfo.GetDestination(ACatalog: TdxPDFCatalog; AInternal: Boolean): TdxPDFCustomDestination;
begin
  if (FDestination = nil) and (FName <> '') then
  begin
    FDestination := ACatalog.GetDestination(FName);
    if FDestination <> nil then
      FDestination.Reference;
  end;
  if AInternal and (FDestination <> nil) then
    FDestination.ResolveInternalPage;
  Result := FDestination;
end;

function TdxPDFDestinationInfo.IsValid: Boolean;
begin
  Result := (FDestination <> nil) or (FName <> '');
end;

{ TdxPDFTarget }

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer; X, Y: Single): TdxPDFTarget;
begin
  Result := CreateEx(AMode, APageIndex, X, Y, 0, 0, dxPDFInvalidValue);
end;

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer): TdxPDFTarget;
begin
  Result := CreateEx(AMode, APageIndex, dxPDFInvalidValue, dxPDFInvalidValue, 0, 0, dxPDFInvalidValue);
end;

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer; const R: TdxRectF): TdxPDFTarget;
begin
  Result := Invalid;
  Result.FMode := AMode;
  Result.FPageIndex := APageIndex;
  Result.FX := R.Left;
  Result.FY := R.Top;
  Result.FWidth := R.Width;
  Result.FHeight := R.Height;
  Result.FZoom := dxPDFInvalidValue;
end;

class function TdxPDFTarget.Create(APageIndex: Integer; X, Y, AZoom: Single): TdxPDFTarget;
begin
  Result := CreateEx(tmXYZ, APageIndex, X, Y, 0, 0, AZoom);
end;

class function TdxPDFTarget.CreateEx(AMode: TdxPDFTargetMode; APageIndex: Integer;
  X, Y, AWidth, AHeight, AZoom: Single): TdxPDFTarget;
begin
  Result := Invalid;
  Result.FMode := AMode;
  Result.FPageIndex := APageIndex;
  Result.FX := X;
  Result.FY := Y;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.FZoom := AZoom;
end;

class function TdxPDFTarget.Invalid: TdxPDFTarget;
begin
  Result.FPageIndex := dxPDFInvalidValue;
  Result.FMode := tmXYZ;
  Result.FHeight := dxPDFInvalidValue;
  Result.FWidth := dxPDFInvalidValue;
  Result.FX := dxPDFInvalidValue;
  Result.FY := dxPDFInvalidValue;
  Result.FZoom := dxPDFInvalidValue;
end;

function TdxPDFTarget.IsValid: Boolean;
begin
  Result := TdxPDFUtils.IsIntegerValid(FPageIndex);
end;

{ TdxPDFInteractiveOperation }

class function TdxPDFInteractiveOperation.Create(AAction: TdxPDFCustomAction): TdxPDFInteractiveOperation;
begin
  Result := Invalid;
  Result.FAction := AAction;
end;

class function TdxPDFInteractiveOperation.Create(AAction: TdxPDFCustomAction;
  ADestination: TdxPDFCustomDestination): TdxPDFInteractiveOperation;
begin
  Result.FAction := AAction;
  Result.FDestination := ADestination;
end;

class function TdxPDFInteractiveOperation.Invalid: TdxPDFInteractiveOperation;
begin
  Result.FAction := nil;
  Result.FDestination := nil;
end;

function TdxPDFInteractiveOperation.IsValid: Boolean;
begin
  Result := (FDestination <> nil) or (FAction <> nil);
end;

function TdxPDFInteractiveOperation.GetTarget: TdxPDFTarget;
begin
  if IsValid and (Destination <> nil) then
    Result := Destination.GetTarget
  else
    Result := TdxPDFTarget.Invalid;
end;

{ TdxPDFTextParser }

constructor TdxPDFTextParser.Create(const APageCropBox: TdxRectF; AContent: TdxPDFRecognizedContent);
begin
  inherited Create;
  FPageBlocks := TObjectList<TdxPDFTextBlock>.Create;
  FFontDataStorage := TObjectDictionary<TdxPDFCustomFont, TdxPDFFontData>.Create([doOwnsValues]);
  FPageCropBox := cxRectAdjustF(APageCropBox);
  FContent := AContent;
end;

destructor TdxPDFTextParser.Destroy;
begin
  FreeAndNil(FFontDataStorage);
  FreeAndNil(FPageBlocks);
  inherited Destroy;
end;

procedure TdxPDFTextParser.AddBlock(const AStringData: TdxPDFStringData; AState: TdxPDFTextState;
  ATextWidthFactor, ATextHeightFactor: Single; const AGlyphOffsets: TDoubleDynArray; ALocation: TdxPointF; AAngle: Single);
var
  AFontData: TdxPDFFontData;
  AFont: TdxPDFCustomFont;
begin
  AFont := AState.Font as TdxPDFCustomFont;
  if (Length(AStringData.CharacterCodes) > 0) and (Length(AGlyphOffsets) > 0) then
  begin
    if not FFontDataStorage.TryGetValue(AFont, AFontData) then
    begin
      AFontData := TdxPDFFontData.CreateFontData(AFont);
      FFontDataStorage.Add(AFont, AFontData);
    end;
    FPageBlocks.Add(TdxPDFTextBlock.Create(AStringData, AFontData, AState, ATextWidthFactor, ATextHeightFactor,
      AGlyphOffsets, ALocation, AAngle));
  end;
end;

procedure TdxPDFTextParser.Parse;
var
  ABuilder: TdxPDFPageTextLineBuilder;
begin
  if (FPageBlocks <> nil) and (FPageBlocks.Count >= 1) then
  begin
    FParserState := TdxPDFTextParserState.Create(FPageBlocks, dxRectF(0, 0, FPageCropBox.Width, FPageCropBox.Height));
    try
      ABuilder := TdxPDFPageTextLineBuilder.Create(FParserState);
      try
        ABuilder.Populate(FContent.TextLines);
      finally
        ABuilder.Free;
      end;
    finally
      FParserState.Free;
    end;
  end;
end;

{ TdxPDFTextUtils }

class function TdxPDFTextUtils.ConvertToString(const ARanges: TdxPDFPageTextRanges; APages: TdxPDFPages): string;
var
  I, ALength: Integer;
  APage: TdxPDFPage;
  ARange: TdxPDFPageTextRange;
  ATextBuilder: TdxBiDiStringBuilder;
begin
  ATextBuilder := TdxBiDiStringBuilder.Create;
  try
    ALength := Length(ARanges);
    for I := 0 to ALength - 1 do
    begin
      ARange := ARanges[I];
      APage := APages[ARange.PageIndex];
      Append(ATextBuilder, ARange.StartPosition, ARange.EndPosition, APage.RecognizedContent.TextLines);
      if (I <> ALength - 1) and (ARange.PageIndex = (ARanges[I + 1]).PageIndex) and not ATextBuilder.Empty and
        not ATextBuilder.EndsWithNewLine then
        ATextBuilder.AppendLine;
    end;
    Result := ATextBuilder.EndCurrentLineAndGetString;
  finally
    ATextBuilder.Free;
  end;
end;

{ TdxPDFFileAttachmentAnnotationField }

function TdxPDFFileAttachmentAnnotationField.GetAttachment: TdxPDFFileAttachment;
begin
  Result := (FAnnotation as TdxPDFFileAttachmentAnnotation).FileSpecification.Attachment;
end;

function TdxPDFFileAttachmentAnnotationField.GetCursor: TCursor;
begin
  Result := crdxPDFViewerContext;
end;

function TdxPDFFileAttachmentAnnotationField.GetHitCode: Integer;
begin
  Result := hcAttachment;
end;

function TdxPDFFileAttachmentAnnotationField.IsResetFocusingNeeded: Boolean;
begin
  Result := False;
end;

initialization
  dxPDFRegisterDocumentObjectClass(TdxPDFCatalog);
  dxPDFRegisterDocumentObjectClass(TdxPDFColorSpaces);
  dxPDFRegisterDocumentObjectClass(TdxPDFDocumentImage);
  dxPDFRegisterDocumentObjectClass(TdxPDFForm);
  dxPDFRegisterDocumentObjectClass(TdxPDFDocumentInformation);
  dxPDFRegisterDocumentObjectClass(TdxPDFFonts);
  dxPDFRegisterDocumentObjectClass(TdxPDFGraphicsStateParameters);
  dxPDFRegisterDocumentObjectClass(TdxPDFPageData);
  dxPDFRegisterDocumentObjectClass(TdxPDFPageContents);
  dxPDFRegisterDocumentObjectClass(TdxPDFPages);
  dxPDFRegisterDocumentObjectClass(TdxPDFResources);
  dxPDFRegisterDocumentObjectClass(TdxPDFXObjects);
  dxPDFRegisterDocumentObjectClass(TdxPDFFileSpecification);

finalization
  dxPDFUnregisterDocumentObjectClass(TdxPDFFileSpecification);
  dxPDFUnregisterDocumentObjectClass(TdxPDFXObjects);
  dxPDFUnregisterDocumentObjectClass(TdxPDFResources);
  dxPDFUnregisterDocumentObjectClass(TdxPDFPages);
  dxPDFUnregisterDocumentObjectClass(TdxPDFPageContents);
  dxPDFUnregisterDocumentObjectClass(TdxPDFPageData);
  dxPDFUnregisterDocumentObjectClass(TdxPDFGraphicsStateParameters);

  dxPDFUnregisterDocumentObjectClass(TdxPDFFonts);
  dxPDFUnregisterDocumentObjectClass(TdxPDFForm);
  dxPDFUnregisterDocumentObjectClass(TdxPDFDocumentImage);
  dxPDFUnregisterDocumentObjectClass(TdxPDFDocumentInformation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFColorSpaces);
  dxPDFUnregisterDocumentObjectClass(TdxPDFCatalog);

  FreeAndNil(dxgPDFDocumentObjectFactory);

end.



