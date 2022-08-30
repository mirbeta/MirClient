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

unit dxPDFDocument;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Windows, Generics.Defaults, Generics.Collections, cxClasses, dxCoreClasses, SysUtils, cxGraphics,
  cxGeometry, dxGDIPlusClasses, dxProtectionUtils, dxThreading, dxPDFBase, dxPDFTypes, dxPDFParser, dxPDFCore, dxPDFText,
  dxPDFFontUtils, dxPDFEncryption, dxPDFInteractivity, dxPDFRecognizedObject;

const
  dxPDFDefaultPasswordAttemptsLimit = 3;

type
  IdxPDFDocumentListener = interface // for internal use
  ['{E09155EC-FB27-4A4F-8517-B5F96D14CEAA}']
    procedure Changed(Sender: TObject);
  end;

  TdxPDFDocument = class;
  TdxPDFDocumentSequentialTextSearch = class;

  TdxPDFDocumentLoadInfo = record
    FileName: string;
    FileStream: TStream;
  end;

  TdxPDFDocumentLoadedEvent = procedure(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo) of object;
  TdxPDFDocumentTextSearchProgressEvent = procedure(Sender: TdxPDFDocument; APageIndex, APercent: Integer) of object;

  TdxPDFDocumentTextSearchDirection = (tsdForward, tsdBackward);
  TdxPDFDocumentTextSearchStatus = (tssFound, tssNotFound, tssFinished);

  TdxPDFDocumentTextSearchOptions = record
  public
    CaseSensitive: Boolean;
    Direction: TdxPDFDocumentTextSearchDirection;
    WholeWords: Boolean;

    class function Default: TdxPDFDocumentTextSearchOptions; static;
  end;

  TdxPDFDocumentTextSearchResult = record
    Range: TdxPDFPageTextRange;
    Status: TdxPDFDocumentTextSearchStatus;
  end;

  TdxPDFPageInfo = record
  private
    FPage: TdxPDFPage;
  public
    Hyperlinks: TdxPDFHyperlinkList;
    Images: TdxPDFImageList;
    Size: TdxPointF;
    Text: string;
    UserUnit: Integer;
    procedure Initialize(APage: TdxPDFPage; AAllowContentExtraction: Boolean); // for internal use
    procedure Pack;
  end;

  { TdxPDFDocument }

  TdxPDFDocument = class
  strict private
    FCatalog: TdxPDFCatalog;
    FEncryptionDictionary: TdxPDFDictionary;
    FEncryptionDictionaryNumber: Integer;
    FFileAttachments: TdxPDFFileAttachmentList;
    FID: TdxPDFDocumentID;
    FInfoObjectNumber: Integer;
    FInformation: TdxPDFDocumentInformation;
    FIsLoaded: Boolean;
    FLoadInfo: TdxPDFDocumentLoadInfo;
    FListenerList: IInterfaceList;
    FModified: Boolean;
    FOutlineTree: TdxPDFOutlineTree;
    FPasswordAttemptsLimit: Integer;
    FRepository: TdxPDFDocumentRepository;
    FRootObjectNumber: Integer;
    FTextSearch: TdxPDFDocumentSequentialTextSearch;

    FOnGetPassword: TdxGetPasswordEvent;
    FOnLoaded: TdxPDFDocumentLoadedEvent;
    FOnSearchProgress: TdxPDFDocumentTextSearchProgressEvent;
    FOnUnloaded: TNotifyEvent;

    function GetAllowContentExtraction: Boolean;
    function GetAllowPrinting: Boolean;
    function GetEncryptionInfo: TdxPDFEncryptionInfo;
    function GetFileAttachments: TdxPDFFileAttachmentList;
    function GetFileName: string;
    function GetFileSize: Int64;
    function GetIsEncrypted: Boolean;
    function GetOutlineTree: TdxPDFOutlineTree;
    function GetPageCount: Integer;
    function GetPageInfo(AIndex: Integer): TdxPDFPageInfo;
    function GetPages: TdxPDFPages;
    function GetParser: TdxPDFDocumentParser;
    procedure SetCatalog(const AValue: TdxPDFCatalog);
    procedure SetEncryptionDictionary(const AValue: TdxPDFDictionary);
    procedure SetOnSearchProgress(const AValue: TdxPDFDocumentTextSearchProgressEvent);

    function CreateStream(AStream: TStream): TMemoryStream; overload;
    function CreateStream(const AFileName: string): TMemoryStream; overload;
    function DoLoad: Boolean;
    procedure Changed;
    procedure ClearEncryptionDictionaryNumber;
    procedure ClearLoadInfo;
    procedure CreateRepository(AStream: TStream);
    procedure DoClear;
    procedure PopulateLoadInfo(const AFileName: string; AStream: TStream);
    procedure ReadCorruptedDocument;
    procedure ReadID(AObject: TdxPDFBase);
    procedure ReadObjectNumber(ADictionary: TdxPDFDictionary; const AKey: string; var ANumber: Integer);
    procedure ReadTrailer;
    procedure ReadVersion;
    procedure UpdateInformation;
    procedure UpdateTrailer(ADictionary: TdxPDFDictionary);

    procedure OnInteractiveFormFieldValueChangedHandler(Sender: TObject);

    property EncryptionDictionary: TdxPDFDictionary read FEncryptionDictionary write SetEncryptionDictionary;
  strict protected
    FState: TdxPDFDocumentState;
  protected
    procedure CreateSubClasses;
    procedure DestroySubClasses;
    procedure Load(AStream: TStream; const AFileName: string = '');

    procedure CreateDocumentState; virtual;

    procedure AddListener(const AListener: IdxPDFDocumentListener);
    procedure RemoveListener(const AListener: IdxPDFDocumentListener);

    property Catalog: TdxPDFCatalog read FCatalog write SetCatalog;
    property EncryptionInfo: TdxPDFEncryptionInfo read GetEncryptionInfo;
    property ID: TdxPDFDocumentID read FID write FID;
    property IsLoaded: Boolean read FIsLoaded;
    property IsEncrypted: Boolean read GetIsEncrypted;
    property OutlineTree: TdxPDFOutlineTree read GetOutlineTree;
    property Pages: TdxPDFPages read GetPages;
    property Parser: TdxPDFDocumentParser read GetParser;
    property Repository: TdxPDFDocumentRepository read FRepository;
    property State: TdxPDFDocumentState read FState;
    property TextSearch: TdxPDFDocumentSequentialTextSearch read FTextSearch;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function FindText(const AText: string): TdxPDFDocumentTextSearchResult; overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    function FindText(const AText: string;
      const AOptions: TdxPDFDocumentTextSearchOptions): TdxPDFDocumentTextSearchResult; overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    function FindText(const AText: string;
      const AOptions: TdxPDFDocumentTextSearchOptions; APageIndex: Integer): TdxPDFDocumentTextSearchResult; overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure FindText(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
      var AFoundRanges: TdxPDFPageTextRanges); overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);

    property AllowContentExtraction: Boolean read GetAllowContentExtraction;
    property AllowPrinting: Boolean read GetAllowPrinting;
    property FileAttachments: TdxPDFFileAttachmentList read GetFileAttachments;
    property Information: TdxPDFDocumentInformation read FInformation;
    property PageCount: Integer read GetPageCount;
    property PageInfo[Index: Integer]: TdxPDFPageInfo read GetPageInfo;
    property PasswordAttemptsLimit: Integer read FPasswordAttemptsLimit write FPasswordAttemptsLimit;

    property OnGetPassword: TdxGetPasswordEvent read FOnGetPassword write FOnGetPassword;
    property OnLoaded: TdxPDFDocumentLoadedEvent read FOnLoaded write FOnLoaded;
    property OnSearchProgress: TdxPDFDocumentTextSearchProgressEvent read FOnSearchProgress write SetOnSearchProgress;
    property OnUnloaded: TNotifyEvent read FOnUnloaded write FOnUnloaded;
  end;

  { TdxPDFDocumentSequentialTextSearch }

  TdxPDFDocumentSequentialTextSearch = class
  strict private type
    TMoveNextProc = procedure of object;
  strict private
    FAborted: Boolean;
    FArabicNumericReplacements: TdxPDFStringsDictionary;
    FCompleted: Boolean;
    FDocument: TdxPDFDocument;
    FHasResults: Boolean;
    FFoundWords: TdxPDFTextWordList;
    FLastSearchResult: TdxPDFDocumentTextSearchResult;
    FMoveNext: TMoveNextProc;
    FPageIndex: Integer;
    FPageLines: TdxPDFTextLineList;
    FPageWords: TdxPDFTextWordList;
    FProcessedPageIndexes: TList<Integer>;
    FOptions: TdxPDFDocumentTextSearchOptions;
    FSearchStart: Boolean;
    FSearchString: string;
    FStartPageIndex: Integer;
    FStartWordIndex: Integer;
    FSearchWords: TStringList;
    FWordDelimiters: TStringList;
    FWordIndex: Integer;

    FOnComplete: TNotifyEvent;
    FOnProgress: TdxPDFDocumentTextSearchProgressEvent;

    procedure SetCompleted(const AValue: Boolean);
    procedure SetDocument(const AValue: TdxPDFDocument);
    procedure SetFoundWords(const AValue: TdxPDFTextWordList);
    procedure SetOptions(const AValue: TdxPDFDocumentTextSearchOptions);
    procedure SetSearchWords(const AValue: TStringList);

    function CanCompare: Boolean;
    function CanSearch(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;APageIndex: Integer): Boolean;
    function CompareWordList(APageWordList: TStringList): Boolean;
    function CompareWords(const AWord1, AWord2: string): Boolean;
    function CreateWordList(const AText: string): TStringList;
    function GetPageText(AWordIndex, ACount: Integer): string;
    function GetProgressPercent: Integer;
    function GetStepDirection(ADirection: TdxPDFDocumentTextSearchDirection): TMoveNextProc;
    function Initialize(const AText: string; APageIndex: Integer; const AOptions: TdxPDFDocumentTextSearchOptions): Boolean;
    function PrepareComparingWord(const AWord: string): string;
    function TryCompare: Boolean;
    procedure PackCurrentPageRecognizedContent;
    procedure ProgressChanged;
    procedure RecognizeCurrentPage;
    procedure ResetRecognizedText;
    procedure StepBackward;
    procedure StepForward;
    procedure UpdateProcessedPageIndexes;

    property Document: TdxPDFDocument read FDocument write SetDocument;
    property FoundWords: TdxPDFTextWordList read FFoundWords write SetFoundWords;
    property SearchWords: TStringList read FSearchWords write SetSearchWords;
  strict protected
    procedure ClearProcessedPageIndexes;
  protected
    function GetLastSearchRecord: TdxPDFDocumentTextSearchResult; virtual;
    procedure ClearAfterComplete; virtual;
    procedure DirectionChanged; virtual;
    procedure InternalClear; virtual;
    procedure InternalFind; virtual;
    procedure SetLastSearchRecord(const AValue: TdxPDFDocumentTextSearchResult); virtual;

    function DoFind: Boolean;
    function GetSearchResult(APageIndex: Integer; AWords: TdxPDFTextWordList;
      AStatus: TdxPDFDocumentTextSearchStatus): TdxPDFDocumentTextSearchResult;
    procedure Clear;

    property Completed: Boolean read FCompleted write SetCompleted;
    property HasResults: Boolean read FHasResults;
    property LastSearchResult: TdxPDFDocumentTextSearchResult read GetLastSearchRecord write SetLastSearchRecord;
    property Options: TdxPDFDocumentTextSearchOptions read FOptions write SetOptions;
    property PageIndex: Integer read FPageIndex;
    property SearchStart: Boolean read FSearchStart;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(ADocument: TdxPDFDocument; const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
      APageIndex: Integer): TdxPDFDocumentTextSearchResult;

    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnProgress: TdxPDFDocumentTextSearchProgressEvent read FOnProgress write FOnProgress;
  end;

  { TdxPDFDocumentContinuousTextSearch }

  TdxPDFDocumentContinuousTextSearch = class(TdxPDFDocumentSequentialTextSearch)
  strict private
    FCurrentResultIndex: Integer;
    FNotFoundRecordIndex: Integer;
    FSearchResultList: TList<TdxPDFDocumentTextSearchResult>;
    function GetFoundRanges: TdxPDFPageTextRanges;
    procedure CalculateCurrentResultIndex;
  protected
    function GetLastSearchRecord: TdxPDFDocumentTextSearchResult; override;
    procedure ClearAfterComplete; override;
    procedure DirectionChanged; override;
    procedure InternalClear; override;
    procedure InternalFind; override;
    procedure SetLastSearchRecord(const AValue: TdxPDFDocumentTextSearchResult); override;
  public
    constructor Create;
    destructor Destroy; override;

    property FoundRanges : TdxPDFPageTextRanges read GetFoundRanges;
  end;

function dxPDFDocumentExportToImage(ADocument: TdxPDFDocument; APageIndex, AWidth: Integer;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0; ACancelCallback: TdxTaskCancelCallback = nil): Boolean;
function dxPDFDocumentExportToImageEx(ADocument: TdxPDFDocument; APageIndex: Integer; AZoomFactor: Double;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0; ACancelCallback: TdxTaskCancelCallback = nil): Boolean;
function dxPDFDocumentExportToPNG(ADocument: TdxPDFDocument; APageIndex: Integer; AZoomFactor: Double;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;
function dxPDFDocumentExportToPNG(ADocument: TdxPDFDocument; const AFolder, AFilePrefix: string; AZoomFactor: Double;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;
function dxPDFDocumentExportToTIFF(ADocument: TdxPDFDocument; AZoomFactor: Double; AImage: TdxSmartImage;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;
function dxPDFDocumentExportToTIFF(ADocument: TdxPDFDocument; const AFileName: string; AZoomFactor: Double;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;

implementation

{$R dxBiDiData.res}

uses
  Math, Graphics, Contnrs, dxCore, dxPDFUtils, dxPDFDocumentStrs, dxPDFCommandInterpreter;

type
  TdxPDFCatalogAccess = class(TdxPDFCatalog);
  TdxPDFCustomDocumentAccess = class(TdxPDFDocument);
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);
  TdxPDFDocumentInformationAccess = class(TdxPDFDocumentInformation);
  TdxPDFDocumentRepositoryAccess = class(TdxPDFDocumentRepository);
  TdxPDFEncryptionInfoAccess = class(TdxPDFEncryptionInfo);
  TdxPDFFileAttachmentListAccess = class(TdxPDFFileAttachmentList);
  TdxPDFObjectAccess = class(TdxPDFObject);
  TdxPDFOutlineTreeAccess = class(TdxPDFOutlineTree);
  TdxPDFPageAccess = class(TdxPDFPage);
  TdxPDFPagesAccess = class(TdxPDFPages);
  TdxPDFTextLineAccess = class(TdxPDFTextLine);
  TdxPDFTextLineListAccess = class(TdxPDFTextLineList);
  TdxPDFTextWordAccess = class(TdxPDFTextWord);
  TdxPDFTextWordPartAccess = class(TdxPDFTextWordPart);

  { TdxPDFDocumentExport }

  TdxPDFDocumentExport = class
  strict private
    FCancelCallback: TdxTaskCancelCallback;
    FDocument: TdxPDFDocument;
    FFixedWidth: Integer;
    FHandler: TObject;
    FProgressHelper: TcxProgressCalculationHelper;
    FRotationAngle: TcxRotationAngle;
    FZoomFactor: Double;

    function CreateRenderParameters(APage: TdxPDFPage): TdxPDFRenderParameters;
    procedure ProgressHandler(Sender: TObject; APercent: Integer);
    procedure SaveToStream(APageIndex: Integer; AStream: TStream);
  protected
    function GetPageImage(AIndex: Integer; const ASize: TSize; AMode: TcxImageFitMode; var AFrame: TdxSmartImage): Boolean;
    function DoExportToBitmap(APageIndex: Integer; AImage: TdxSmartImage): Boolean; overload;
    function DoExportToBitmap(const AFolder, AFilePrefix: string): Boolean; overload;
    function DoExportToTIFF(AImage: TdxSmartImage): Boolean; overload;
    function DoExportToTIFF(const AFileName: string): Boolean; overload;
  public
    constructor Create(ADocument: TdxPDFDocument; AZoomFactor: Double; ARotationAngle: TcxRotationAngle;
      AHandler: TObject = nil; ACancelCallback: TdxTaskCancelCallback = nil); overload;
    constructor CreateEx(ADocument: TdxPDFDocument; AWidth: Integer; ARotationAngle: TcxRotationAngle;
      AHandler: TObject = nil; ACancelCallback: TdxTaskCancelCallback = nil); overload;
    destructor Destroy; override;

    class function ExportToBitmapBySize(ADocument: TdxPDFDocument; APageIndex, AWidth: Integer;
      ARotationAngle: TcxRotationAngle; AImage: TdxSmartImage; const ACancelCallback: TdxTaskCancelCallback): Boolean; static;
    class function ExportToBitmapByZoomFactor(ADocument: TdxPDFDocument; APageIndex: Integer; AZoomFactor: Double;
      ARotationAngle: TcxRotationAngle; AImage: TdxSmartImage; ACancelCallback: TdxTaskCancelCallback = nil): Boolean; static;
    class function ExportToBitmaps(ADocument: TdxPDFDocument; const AFolder, AFilePrefix: string; AZoomFactor: Double;
      ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean; static;
    class function ExportToTIFF(ADocument: TdxPDFDocument; AZoomFactor: Double; AImage: TdxSmartImage;
      ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean; overload; static;
    class function ExportToTIFF(ADocument: TdxPDFDocument; const AFileName: string; AZoomFactor: Double;
      ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean; overload; static;
  end;

function dxPDFDocumentExportToImage(ADocument: TdxPDFDocument; APageIndex, AWidth: Integer;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0; ACancelCallback: TdxTaskCancelCallback = nil): Boolean;
begin
  Result := TdxPDFDocumentExport.ExportToBitmapBySize(ADocument, APageIndex, AWidth, ARotationAngle, AImage,
    ACancelCallback);
end;

function dxPDFDocumentExportToImageEx(ADocument: TdxPDFDocument; APageIndex: Integer; AZoomFactor: Double;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0; ACancelCallback: TdxTaskCancelCallback = nil): Boolean;
begin
  Result := TdxPDFDocumentExport.ExportToBitmapByZoomFactor(ADocument, APageIndex, AZoomFactor, ARotationAngle, AImage,
    ACancelCallback);
end;

function dxPDFDocumentExportToPNG(ADocument: TdxPDFDocument; APageIndex: Integer; AZoomFactor: Double;
  AImage: TdxSmartImage; ARotationAngle: TcxRotationAngle = ra0): Boolean;
begin
  Result := TdxPDFDocumentExport.ExportToBitmapByZoomFactor(ADocument, APageIndex, AZoomFactor, ARotationAngle, AImage,
    nil);
end;

function dxPDFDocumentExportToPNG(ADocument: TdxPDFDocument; const AFolder, AFilePrefix: string; AZoomFactor: Double;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean;
begin
  Result := TdxPDFDocumentExport.ExportToBitmaps(ADocument, AFolder, AFilePrefix, AZoomFactor, ARotationAngle, AHandler);
end;

function dxPDFDocumentExportToTIFF(ADocument: TdxPDFDocument; AZoomFactor: Double; AImage: TdxSmartImage;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;
begin
  Result := TdxPDFDocumentExport.ExportToTIFF(ADocument, AZoomFactor, AImage, ARotationAngle, AHandler);
end;

function dxPDFDocumentExportToTIFF(ADocument: TdxPDFDocument; const AFileName: string; AZoomFactor: Double;
  AHandler: TObject = nil; ARotationAngle: TcxRotationAngle = ra0): Boolean; overload;
begin
  Result := TdxPDFDocumentExport.ExportToTIFF(ADocument, AFileName, AZoomFactor, ARotationAngle, AHandler);
end;

{ TdxPDFDocumentExport }

constructor TdxPDFDocumentExport.Create(ADocument: TdxPDFDocument; AZoomFactor: Double; ARotationAngle: TcxRotationAngle;
  AHandler: TObject = nil; ACancelCallback: TdxTaskCancelCallback = nil);
begin
  inherited Create;
  FDocument := ADocument;
  FZoomFactor := AZoomFactor;
  FHandler := AHandler;
  FProgressHelper := TcxProgressCalculationHelper.Create(1, Self, ProgressHandler);
  FRotationAngle := ARotationAngle;
  FCancelCallback := ACancelCallback;
end;

constructor TdxPDFDocumentExport.CreateEx(ADocument: TdxPDFDocument; AWidth: Integer; ARotationAngle: TcxRotationAngle;
  AHandler: TObject = nil; ACancelCallback: TdxTaskCancelCallback = nil);
begin
  Create(ADocument, 1, ARotationAngle, AHandler, ACancelCallback);
  FFixedWidth := AWidth;
end;

destructor TdxPDFDocumentExport.Destroy;
begin
  FreeAndNil(FProgressHelper);
  inherited Destroy;
end;

class function TdxPDFDocumentExport.ExportToBitmapBySize(ADocument: TdxPDFDocument; APageIndex, AWidth: Integer;
  ARotationAngle: TcxRotationAngle; AImage: TdxSmartImage; const ACancelCallback: TdxTaskCancelCallback): Boolean;
var
  AHelper: TdxPDFDocumentExport;
begin
  AHelper := TdxPDFDocumentExport.CreateEx(ADocument, AWidth, ARotationAngle, nil, ACancelCallback);
  try
    Result := AHelper.DoExportToBitmap(APageIndex, AImage);
  finally
    AHelper.Free;
  end;
end;

class function TdxPDFDocumentExport.ExportToBitmapByZoomFactor(ADocument: TdxPDFDocument; APageIndex: Integer;
  AZoomFactor: Double; ARotationAngle: TcxRotationAngle; AImage: TdxSmartImage;
  ACancelCallback: TdxTaskCancelCallback = nil): Boolean;
var
  AHelper: TdxPDFDocumentExport;
begin
  AHelper := TdxPDFDocumentExport.Create(ADocument, AZoomFactor, ARotationAngle, nil, ACancelCallback);
  try
    Result := AHelper.DoExportToBitmap(APageIndex, AImage);
  finally
    AHelper.Free;
  end;
end;

class function TdxPDFDocumentExport.ExportToBitmaps(ADocument: TdxPDFDocument; const AFolder, AFilePrefix: string;
  AZoomFactor: Double; ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean;
var
  AHelper: TdxPDFDocumentExport;
begin
  AHelper := TdxPDFDocumentExport.Create(ADocument, AZoomFactor, ARotationAngle, AHandler);
  try
    Result := AHelper.DoExportToBitmap(AFolder, AFilePrefix);
  finally
    AHelper.Free;
  end;
end;

class function TdxPDFDocumentExport.ExportToTIFF(ADocument: TdxPDFDocument; AZoomFactor: Double; AImage: TdxSmartImage;
  ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean;
var
  AHelper: TdxPDFDocumentExport;
begin
  AHelper := TdxPDFDocumentExport.Create(ADocument, AZoomFactor, ARotationAngle, AHandler);
  try
    Result := AHelper.DoExportToTIFF(AImage);
  finally
    AHelper.Free;
  end;
end;

class function TdxPDFDocumentExport.ExportToTIFF(ADocument: TdxPDFDocument; const AFileName: string; AZoomFactor: Double;
  ARotationAngle: TcxRotationAngle; AHandler: TObject = nil): Boolean;
var
  AHelper: TdxPDFDocumentExport;
begin
  AHelper := TdxPDFDocumentExport.Create(ADocument, AZoomFactor, ARotationAngle, AHandler);
  try
    Result := AHelper.DoExportToTIFF(AFileName);
  finally
    AHelper.Free;
  end;
end;

function TdxPDFDocumentExport.GetPageImage(AIndex: Integer; const ASize: TSize; AMode: TcxImageFitMode;
  var AFrame: TdxSmartImage): Boolean;
begin
  AFrame := TdxSmartImage.Create;
  Result := ExportToBitmapByZoomFactor(FDocument, AIndex, FZoomFactor, FRotationAngle, AFrame);
  FProgressHelper.NextTask;
end;

function TdxPDFDocumentExport.DoExportToBitmap(APageIndex: Integer; AImage: TdxSmartImage): Boolean;
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(APageIndex, AStream);
    AStream.Position := 0;
    AImage.LoadFromStream(AStream);
    Result := True;
  finally
    AStream.Free;
  end;
end;

function TdxPDFDocumentExport.DoExportToBitmap(const AFolder, AFilePrefix: string): Boolean;
var
  I: Integer;
  AImage: TdxSmartImage;
  AFileStream: TFileStream;
  AOutputFolder: string;
begin
  if not DirectoryExists(AFolder) then
    CreateDir(AFolder);
  AOutputFolder := dxIncludeTrailingPathDelimiter(AFolder);
  FProgressHelper.BeginStage(FDocument.PageCount);
  Result := False;
  try
    for I := 0 to FDocument.PageCount - 1 do
    begin
      AImage := TdxSmartImage.Create;
      try
        Result := ExportToBitmapByZoomFactor(FDocument, I, FZoomFactor, FRotationAngle, AImage);
        if Result then
        begin
          AFileStream := TFileStream.Create(AOutputFolder + AFilePrefix + IntToStr(I) + '.png', fmCreate);
          try
            AImage.SaveToStreamByCodec(AFileStream, dxImagePng);
          finally
            AFileStream.Free;
          end;
        end;
      finally
        AImage.Free;
      end;
      FProgressHelper.NextTask;
    end;
  finally
    FProgressHelper.EndStage;
  end;
end;

function TdxPDFDocumentExport.DoExportToTIFF(AImage: TdxSmartImage): Boolean;
var
  ATemp: TdxSmartImage;
begin
  FProgressHelper.BeginStage(FDocument.PageCount);
  try
    try
      ATemp := dxCreateMultiFrameTIFF(FDocument.PageCount, cxNullSize, ifmNormal, GetPageImage);
      Result := ATemp <> nil;
      if Result then
        try
          AImage.Assign(ATemp);
        finally
          ATemp.Free;
        end;
    except
    {$IFNDEF DELPHI102TOKYO}
       Result := False;
    {$ENDIF}
    end;
    Result := AImage <> nil;
  finally
    FProgressHelper.EndStage;
  end;
end;

function TdxPDFDocumentExport.DoExportToTIFF(const AFileName: string): Boolean;
var
  AImage: TdxSmartImage;
begin
  AImage := TdxSmartImage.Create;
  try
    Result := DoExportToTIFF(AImage);
    if Result then
      try
        AImage.SaveToFile(AFileName);
      except
        Result := False;
      end;
  finally
    AImage.Free;
  end;
end;

function TdxPDFDocumentExport.CreateRenderParameters(APage: TdxPDFPage): TdxPDFRenderParameters;

  function GetScaleFactor: Double;
  begin
    if FFixedWidth = 0 then
      Result := FZoomFactor * APage.UserUnit * 96 / 72
    else
      Result := FFixedWidth / APage.Size.X;
  end;

  function GetRect(AScaleFactor: Double): TRect;
  begin
    if FFixedWidth = 0 then
      Result := cxRect(0, 0, Trunc(APage.Size.X * AScaleFactor), Trunc(APage.Size.Y * AScaleFactor))
    else
      Result := cxRect(0, 0, FFixedWidth, Round(FFixedWidth * APage.Size.Y / APage.Size.X));
    if (FRotationAngle = raPlus90) or (FRotationAngle = raMinus90) then
      Result := cxRectRotate(Result);
  end;

begin
  Result := TdxPDFRenderParameters.Create(FDocument.State);
  Result.ScaleFactor := GetScaleFactor;
  Result.Rect := GetRect(Result.ScaleFactor);
  Result.Angle := FRotationAngle;
  Result.CancelCallback := FCancelCallback;
end;

procedure TdxPDFDocumentExport.ProgressHandler(Sender: TObject; APercent: Integer);
var
  AIntf: IcxProgress;
begin
  if Supports(FHandler, IcxProgress, AIntf) then
    AIntf.OnProgress(Sender, APercent);
end;

procedure TdxPDFDocumentExport.SaveToStream(APageIndex: Integer; AStream: TStream);
var
  APage: TdxPDFPage;
  AParameters: TdxPDFRenderParameters;
begin
  APage := FDocument.Pages[APageIndex];
  AParameters := CreateRenderParameters(APage);
  try
    TdxPDFPageAccess(APage).Export(AParameters, AStream);
  finally
    AParameters.Free;
  end;
end;

{ TdxPDFDocumentTextSearchOptions }

class function TdxPDFDocumentTextSearchOptions.Default: TdxPDFDocumentTextSearchOptions;
begin
  Result.CaseSensitive := False;
  Result.Direction := tsdForward;
  Result.WholeWords := False;
end;

{ TdxPDFPageInfo }

procedure TdxPDFPageInfo.Pack;
begin
  FPage.Pack;
end;

procedure TdxPDFPageInfo.Initialize(APage: TdxPDFPage; AAllowContentExtraction: Boolean);
var
  AContent: TdxPDFRecognizedContent;
begin
  FPage := APage;
  Hyperlinks := nil;
  Images := nil;
  Text := '';
  if AAllowContentExtraction then
  begin
    AContent := FPage.RecognizedContent;
    if AContent <> nil then
    begin
      Hyperlinks := AContent.Hyperlinks;
      Images := AContent.Images;
      Text := AContent.Text;
    end;
  end;
  Size := FPage.Size;
  UserUnit := FPage.UserUnit;
end;

{ TdxPDFDocument }

constructor TdxPDFDocument.Create;
begin
  inherited Create;
  FListenerList := TInterfaceList.Create;
  CreateSubClasses;
  FPasswordAttemptsLimit := dxPDFDefaultPasswordAttemptsLimit;
end;

destructor TdxPDFDocument.Destroy;
begin
  FOnGetPassword := nil;
  FOnLoaded := nil;
  FOnUnloaded := nil;
  FOnSearchProgress := nil;
  Clear;
  DestroySubClasses;
  FListenerList := nil;
  inherited Destroy;
end;

function TdxPDFDocument.FindText(const AText: string): TdxPDFDocumentTextSearchResult;
var
  AOptions: TdxPDFDocumentTextSearchOptions;
begin
  AOptions.CaseSensitive := False;
  AOptions.WholeWords := False;
  AOptions.Direction := tsdForward;
  Result := FindText(AText, AOptions);
end;

function TdxPDFDocument.FindText(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions): TdxPDFDocumentTextSearchResult;
begin
  Result := FindText(AText, AOptions, 0);
end;

function TdxPDFDocument.FindText(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
  APageIndex: Integer): TdxPDFDocumentTextSearchResult;
begin
  Result := FTextSearch.Find(Self, AText, AOptions, APageIndex);
end;

procedure TdxPDFDocument.FindText(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
  var AFoundRanges: TdxPDFPageTextRanges);
var
  AAdvancedTextSearch: TdxPDFDocumentContinuousTextSearch;
begin
  AAdvancedTextSearch := TdxPDFDocumentContinuousTextSearch.Create;
  AAdvancedTextSearch.OnProgress := OnSearchProgress;
  try
    AAdvancedTextSearch.Find(Self, AText, AOptions, 0);
    AFoundRanges := AAdvancedTextSearch.FoundRanges;
  finally
    AAdvancedTextSearch.Free;
  end;
end;

procedure TdxPDFDocument.Clear;
begin
  DoClear;
  dxCallNotify(OnUnloaded, Self);
end;

procedure TdxPDFDocument.LoadFromFile(const AFileName: string);
begin
  Load(CreateStream(AFileName), AFileName);
end;

procedure TdxPDFDocument.LoadFromStream(AStream: TStream);
begin
  Load(CreateStream(AStream));
end;

procedure TdxPDFDocument.CreateSubClasses;
begin
  ClearEncryptionDictionaryNumber;
  Catalog := TdxPDFCatalog.Create(Self);
  FInformation := TdxPDFDocumentInformation.Create(nil);
  FRepository := nil;
  FEncryptionDictionary := nil;
  FTextSearch := TdxPDFDocumentSequentialTextSearch.Create;
  FTextSearch.OnProgress := OnSearchProgress;
  FOutlineTree := nil;
  FFileAttachments := nil;
  CreateDocumentState;
end;

procedure TdxPDFDocument.DestroySubClasses;
begin
  FreeAndNil(FState);
  FreeAndNil(FFileAttachments);
  FreeAndNil(FOutlineTree);
  FreeAndNil(FTextSearch);
  EncryptionDictionary := nil;
  FreeAndNil(FInformation);
  FreeAndNil(FCatalog);
  if Repository <> nil then
    FRepository.Clear;
  dxPDFFreeObject(FRepository);
end;

procedure TdxPDFDocument.Load(AStream: TStream; const AFileName: string = '');
begin
  Clear;
  PopulateLoadInfo(AFileName, AStream);
  CreateRepository(AStream);
  if not DoLoad then
    DoClear
  else
    if Assigned(OnLoaded) then
      FOnLoaded(Self, FLoadInfo);
end;

procedure TdxPDFDocument.CreateDocumentState;
begin
  FState := TdxPDFDocumentState.Create(Self);
end;

procedure TdxPDFDocument.AddListener(const AListener: IdxPDFDocumentListener);
begin
  FListenerList.Add(AListener);
end;

procedure TdxPDFDocument.RemoveListener(const AListener: IdxPDFDocumentListener);
begin
  FListenerList.Remove(AListener);
end;

function TdxPDFDocument.GetAllowContentExtraction: Boolean;
begin
  Result := (EncryptionInfo = nil) or EncryptionInfo.AllowContentExtraction;
end;

function TdxPDFDocument.GetAllowPrinting: Boolean;
begin
  Result := (EncryptionInfo = nil) or EncryptionInfo.AllowPrinting;
end;

function TdxPDFDocument.GetEncryptionInfo: TdxPDFEncryptionInfo;
begin
  if FRepository <> nil then
    Result := FRepository.EncryptionInfo
  else
    Result := nil;
end;

function TdxPDFDocument.GetFileAttachments: TdxPDFFileAttachmentList;
begin
  if FFileAttachments = nil then
  begin
    FFileAttachments := TdxPDFFileAttachmentList.Create;
    TdxPDFFileAttachmentListAccess(FFileAttachments).Populate(FCatalog);
  end;
  Result := FFileAttachments;
end;

function TdxPDFDocument.GetFileName: string;
begin
  Result := FLoadInfo.FileName;
end;

function TdxPDFDocument.GetFileSize: Int64;
begin
  if FLoadInfo.FileStream <> nil then
    Result := FLoadInfo.FileStream.Size
  else
    Result := 0;
end;

function TdxPDFDocument.GetIsEncrypted: Boolean;
begin
  Result := FEncryptionDictionary <> nil;
  if not Result and TdxPDFUtils.IsIntegerValid(FEncryptionDictionaryNumber) then
  begin
    EncryptionDictionary := Repository.GetDictionary(FEncryptionDictionaryNumber);
    Result := EncryptionDictionary <> nil;
  end;
end;

function TdxPDFDocument.GetOutlineTree: TdxPDFOutlineTree;
begin
  if FOutlineTree = nil then
  begin
    FOutlineTree := TdxPDFOutlineTree.Create;
    TdxPDFOutlineTreeAccess(FOutlineTree).Read(FCatalog);
  end;
  Result := FOutlineTree;
end;

function TdxPDFDocument.GetPageCount: Integer;
begin
  Result := Pages.Count;
end;

function TdxPDFDocument.GetPageInfo(AIndex: Integer): TdxPDFPageInfo;
begin
  Result.Initialize(Pages[AIndex], AllowContentExtraction);
end;

function TdxPDFDocument.GetPages: TdxPDFPages;
begin
  Result := Catalog.Pages;
end;

function TdxPDFDocument.GetParser: TdxPDFDocumentParser;
begin
  Result := Repository.Parser;
end;

procedure TdxPDFDocument.SetCatalog(const AValue: TdxPDFCatalog);
begin
  if FCatalog <> nil then
    FCatalog.Free;
  FCatalog := AValue;
  FCatalog.OnInteractiveFormFieldValueChanged := OnInteractiveFormFieldValueChangedHandler;
end;

procedure TdxPDFDocument.SetEncryptionDictionary(const AValue: TdxPDFDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FEncryptionDictionary));
end;

procedure TdxPDFDocument.SetOnSearchProgress(const AValue: TdxPDFDocumentTextSearchProgressEvent);
begin
  FOnSearchProgress := AValue;
  FTextSearch.OnProgress := FOnSearchProgress;
end;

function TdxPDFDocument.CreateStream(AStream: TStream): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromStream(AStream);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TdxPDFDocument.CreateStream(const AFileName: string): TMemoryStream;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CreateStream(AFileStream);
  finally
    AFileStream.Free;
  end;
end;

function TdxPDFDocument.DoLoad: Boolean;

  function DoReadDocument: Boolean;
  begin
    Result := True;
    if IsEncrypted then
    begin
      Repository.ReadEncryptionInfo(FEncryptionDictionary, FID);
      Result := Repository.CheckPassword(PasswordAttemptsLimit, OnGetPassword);
    end;
    if Result then
    begin
      Repository.Catalog := FCatalog;
      TdxPDFObjectAccess(FCatalog).Read(Repository.GetDictionary(FRootObjectNumber));
      if TdxPDFUtils.IsIntegerValid(FInfoObjectNumber) then
        TdxPDFObjectAccess(Information).Read(Repository.GetDictionary(FInfoObjectNumber));
      FIsLoaded := True;
      FModified := False;
    end;
  end;

var
  ASavedStartPosition: Int64;
begin
  Result := False;
  ASavedStartPosition := 0;
  try
    try
      ReadVersion;
      Parser.SaveStartPosition;
      ASavedStartPosition := Parser.StartPosition;
      ReadTrailer;
      Result := DoReadDocument;
    except
      on E: EdxPDFEncryptionException do
        raise;
    else
      try
        Parser.StartPosition := ASavedStartPosition;
        ReadCorruptedDocument;
        Result := DoReadDocument;
      except
        on E: EdxPDFAbortException do
          TdxPDFUtils.RaiseException;
        else
          raise;
       end;
    end;
    UpdateInformation;
  except
    DoClear;
    raise;
  end;
end;

procedure TdxPDFDocument.Changed;
var
  I: Integer;
begin
  FModified := True;
  for I := 0 to FListenerList.Count - 1 do
    IdxPDFDocumentListener(FListenerList[I]).Changed(Self);
end;

procedure TdxPDFDocument.ClearEncryptionDictionaryNumber;
begin
  FEncryptionDictionaryNumber := dxPDFInvalidValue;
end;

procedure TdxPDFDocument.ClearLoadInfo;
begin
  FLoadInfo.FileName := '';
  FLoadInfo.FileStream := nil;
end;

procedure TdxPDFDocument.CreateRepository(AStream: TStream);
begin
  if FRepository <> nil then
    FRepository.Free;
  FRepository := TdxPDFDocumentRepository.Create(AStream);
  FRepository.Reference;
end;

procedure TdxPDFDocument.DoClear;
begin
  ClearEncryptionDictionaryNumber;
  TextSearch.Clear;
  FIsLoaded := False;
  ClearLoadInfo;
  DestroySubClasses;
  CreateSubClasses;
  Changed;
end;

procedure TdxPDFDocument.PopulateLoadInfo(const AFileName: string; AStream: TStream);
begin
  FLoadInfo.FileName := AFileName;
  FLoadInfo.FileStream := AStream;
end;

procedure TdxPDFDocument.ReadCorruptedDocument;
var
  ADictionary: TdxPDFDictionary;
begin
  Parser.FindObjects;
  if Parser.FindToken(TdxPDFKeywords.Trailer) then
  begin
    ADictionary := Parser.ReadDictionary(Parser.ReadTrailerData);
    try
      UpdateTrailer(ADictionary);
    finally
      dxPDFFreeObject(ADictionary);
    end;
  end;
end;

procedure TdxPDFDocument.ReadID(AObject: TdxPDFBase);

  procedure PopulateDocumentID(const AID0, AID1: TBytes);
  begin
    FID[0] := AID0;
    FID[1] := AID1;
  end;

var
  AArray: TdxPDFArray;
  AIDReferences: TdxPDFReferencedObjects;
begin
  if AObject <> nil then
  begin
    AArray := AObject as TdxPDFArray;
    if AArray.Count <> 2 then
      TdxPDFUtils.RaiseTestException('Error id count');
    case AArray[0].ObjectType of
      otString:
          PopulateDocumentID(TdxPDFUtils.StrToByteArray((AArray[0] as TdxPDFString).Value),
            TdxPDFUtils.StrToByteArray((AArray[1] as TdxPDFString).Value));
      otIndirectReference:
        begin
          AIDReferences := TdxPDFReferencedObjects.Create;
          try
            AIDReferences.Add(GetParser.ReadIndirectObject((AArray[0] as TdxPDFReference).Offset));
            AIDReferences.Add(GetParser.ReadIndirectObject((AArray[1] as TdxPDFReference).Offset));
            PopulateDocumentID((AIDReferences[0] as TdxPDFIndirectObject).Data,
              (AIDReferences[1] as TdxPDFIndirectObject).Data);
          finally
            AIDReferences.Free;
          end;
        end;
    end;
  end;
end;

procedure TdxPDFDocument.ReadObjectNumber(ADictionary: TdxPDFDictionary; const AKey: string; var ANumber: Integer);
var
  AObject: TdxPDFBase;
begin
  ANumber := dxPDFInvalidValue;
  if ADictionary = nil then
    TdxPDFUtils.Abort;
  if ADictionary.Contains(AKey) then
    if ADictionary.TryGetObject(AKey, AObject) then
      ANumber := AObject.Number;
end;

procedure TdxPDFDocument.ReadTrailer;
var
  AOffset: Int64;
  ATrailer: TdxPDFDictionary;
  ANeedUpdateTrailer: Boolean;
begin
  ATrailer := nil;
  AOffset := Parser.ReadCrossReferencesOffset;
  if AOffset > 0 then
  begin
    ANeedUpdateTrailer := True;
    repeat
      try
        Parser.ReadTrailer(AOffset, ATrailer);
        AOffset := dxPDFInvalidValue;
        if ATrailer <> nil then
        begin
          if ANeedUpdateTrailer then
            UpdateTrailer(ATrailer);
          ANeedUpdateTrailer := False;
          AOffset := ATrailer.GetInteger('Prev');
        end;
      finally
        dxPDFFreeObject(ATrailer);
      end;
    until not TdxPDFUtils.IsIntegerValid(AOffset)
  end;
end;

procedure TdxPDFDocument.ReadVersion;
begin
  Parser.ReadVersion(TdxPDFDocumentInformationAccess(Information).FVersion);
end;

procedure TdxPDFDocument.UpdateInformation;
begin
  TdxPDFDocumentInformationAccess(Information).FFileSize := GetFileSize;
  TdxPDFDocumentInformationAccess(Information).FFileName := GetFileName;
end;

procedure TdxPDFDocument.UpdateTrailer(ADictionary: TdxPDFDictionary);
var
  ANumber: Integer;
begin
  ReadObjectNumber(ADictionary, 'Root', FRootObjectNumber);
  ReadObjectNumber(ADictionary, 'Info', FInfoObjectNumber);
  ReadID(ADictionary.GetObject('ID'));
  if EncryptionDictionary = nil then
  begin
    EncryptionDictionary := ADictionary.GetDictionary('Encrypt');
    if (EncryptionDictionary = nil) and not TdxPDFUtils.IsIntegerValid(FEncryptionDictionaryNumber) and
      ADictionary.TryGetReference('Encrypt', ANumber) then
      FEncryptionDictionaryNumber := ANumber;
  end;
end;

procedure TdxPDFDocument.OnInteractiveFormFieldValueChangedHandler(Sender: TObject);
begin
  Changed;
end;

{ TdxPDFDocumentSequentialTextSearch }

constructor TdxPDFDocumentSequentialTextSearch.Create;
begin
  inherited Create;
  SearchWords := nil;
  FoundWords := nil;
  FProcessedPageIndexes := TList<Integer>.Create;

  FWordDelimiters := TStringList.Create;
  FWordDelimiters.Add('?');
  FWordDelimiters.Add('<');
  FWordDelimiters.Add('=');
  FWordDelimiters.Add(',');
  FWordDelimiters.Add('.');
  FWordDelimiters.Add('!');
  FWordDelimiters.Add('@');
  FWordDelimiters.Add('#');
  FWordDelimiters.Add('$');
  FWordDelimiters.Add('%');
  FWordDelimiters.Add('^');
  FWordDelimiters.Add('&');
  FWordDelimiters.Add('*');
  FWordDelimiters.Add('(');
  FWordDelimiters.Add(')');
  FWordDelimiters.Add('+');
  FWordDelimiters.Add('_');
  FWordDelimiters.Add('-');
  FWordDelimiters.Add('=');
  FWordDelimiters.Add('~');
  FWordDelimiters.Add('`');
  FWordDelimiters.Add('{');
  FWordDelimiters.Add('}');
  FWordDelimiters.Add('[');
  FWordDelimiters.Add('}');
  FWordDelimiters.Add(';');
  FWordDelimiters.Add(':');
  FWordDelimiters.Add('"');
  FWordDelimiters.Add('>');
  FWordDelimiters.Add('||');
  FWordDelimiters.Add('|');
  FWordDelimiters.Add('\');
  FWordDelimiters.Add('/');

  FArabicNumericReplacements := TdxPDFStringsDictionary.Create;
  FArabicNumericReplacements.Add(Char(1632), '0');
  FArabicNumericReplacements.Add(Char(1633), '1');
  FArabicNumericReplacements.Add(Char(1634), '2');
  FArabicNumericReplacements.Add(Char(1635), '3');
  FArabicNumericReplacements.Add(Char(1636), '4');
  FArabicNumericReplacements.Add(Char(1637), '5');
  FArabicNumericReplacements.Add(Char(1638), '6');
  FArabicNumericReplacements.Add(Char(1639), '7');
  FArabicNumericReplacements.Add(Char(1640), '8');
  FArabicNumericReplacements.Add(Char(1641), '9');
  FArabicNumericReplacements.TrimExcess;
end;

destructor TdxPDFDocumentSequentialTextSearch.Destroy;
begin
  FreeAndNil(FArabicNumericReplacements);
  FreeAndNil(FWordDelimiters);
  FreeAndNil(FProcessedPageIndexes);
  FoundWords := nil;
  SearchWords := nil;
  inherited Destroy;
end;

function TdxPDFDocumentSequentialTextSearch.GetLastSearchRecord: TdxPDFDocumentTextSearchResult;
begin
  Result := FLastSearchResult;
end;

procedure TdxPDFDocumentSequentialTextSearch.ClearAfterComplete;
begin
  FCompleted := False;
  FProcessedPageIndexes.Clear;
  ProgressChanged;
end;

procedure TdxPDFDocumentSequentialTextSearch.DirectionChanged;
begin
  InternalClear;
  FProcessedPageIndexes.Clear;
  ProgressChanged;
end;

procedure TdxPDFDocumentSequentialTextSearch.InternalClear;
begin
  FSearchStart := True;
  FCompleted := False;
end;

procedure TdxPDFDocumentSequentialTextSearch.InternalFind;
const
  StatusMap: array[Boolean] of TdxPDFDocumentTextSearchStatus = (tssNotFound, tssFinished);
begin
  try
    Completed := not DoFind;
    if Completed then
      LastSearchResult := GetSearchResult(FPageIndex, nil, StatusMap[FHasResults]);
  except
    on EdxPDFAbortException do;
    on EAbort do;
    else
      raise;
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.SetLastSearchRecord(const AValue: TdxPDFDocumentTextSearchResult);
begin
  FLastSearchResult := AValue;
end;

function TdxPDFDocumentSequentialTextSearch.DoFind: Boolean;
begin
  Result := False;
  repeat
    FoundWords := TdxPDFTextWordList.Create;
    FMoveNext;
    if CanCompare and TryCompare then
    begin
      FStartWordIndex := IfThen(FStartWordIndex = -1, 0, FStartWordIndex);
      if FSearchStart then
      begin
        FStartWordIndex := FWordIndex;
        FStartPageIndex := FPageIndex;
        FSearchStart := False;
        FHasResults := True;
      end
      else
        if (FPageIndex = FStartPageIndex) and (FWordIndex = FStartWordIndex) then
        begin
          FSearchStart := True;
          FWordIndex := IfThen(FOptions.Direction = tsdForward, FWordIndex - 1, FWordIndex + 1);
          Break;
        end;
      LastSearchResult := GetSearchResult(FPageIndex, FoundWords, tssFound);
      Exit(True);
    end;
    if (FPageIndex = FStartPageIndex) and (FWordIndex = FStartWordIndex) then
      Break;
    FStartWordIndex := IfThen(FStartWordIndex = -1, 0, FStartWordIndex);
  until not (FSearchStart or not ((FPageIndex = FStartPageIndex) and (FWordIndex = FStartWordIndex)));
end;

function TdxPDFDocumentSequentialTextSearch.Find(ADocument: TdxPDFDocument; const AText: string;
  const AOptions: TdxPDFDocumentTextSearchOptions; APageIndex: Integer): TdxPDFDocumentTextSearchResult;
begin
  Document := ADocument;
  try
    if CanSearch(AText, AOptions, APageIndex) then
    begin
      Options := AOptions;
      InternalFind;
    end
    else
    begin
      LastSearchResult := GetSearchResult(FPageIndex, nil, tssNotFound);
      Completed := True;
    end;
  finally
    Result := LastSearchResult;
  end;
end;

function TdxPDFDocumentSequentialTextSearch.GetSearchResult(APageIndex: Integer; AWords: TdxPDFTextWordList;
  AStatus: TdxPDFDocumentTextSearchStatus): TdxPDFDocumentTextSearchResult;

  function GetRange(APageIndex: Integer; AWords: TdxPDFTextWordList): TdxPDFPageTextRange;
  var
    ALastWord: TdxPDFTextWord;
  begin
    Result := TdxPDFPageTextRange.Invalid;
    Result.PageIndex := APageIndex;
    if AWords <> nil then
    begin
      ALastWord := AWords[AWords.Count - 1];
      Result := TdxPDFPageTextRange.Create(APageIndex, AWords[0].Index, 0, ALastWord.Index,
        Length(TdxPDFTextWordAccess(ALastWord).Characters));
    end;
  end;

begin
  Result.Status := AStatus;
  Result.Range := GetRange(APageIndex, AWords);
end;

procedure TdxPDFDocumentSequentialTextSearch.Clear;
begin
  ClearProcessedPageIndexes;
  FSearchString := '';
  ResetRecognizedText;
  InternalClear;
end;

procedure TdxPDFDocumentSequentialTextSearch.ClearProcessedPageIndexes;
begin
  FProcessedPageIndexes.Clear;
end;

procedure TdxPDFDocumentSequentialTextSearch.SetCompleted(const AValue: Boolean);
begin
  if FCompleted <> AValue then
  begin
    FCompleted := AValue;
    dxCallNotify(OnComplete, Self);
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.SetDocument(const AValue: TdxPDFDocument);
begin
  if FDocument <> AValue then
  begin
    FAborted := True;
    FDocument := AValue;
    InternalClear;
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.SetFoundWords(const AValue: TdxPDFTextWordList);
begin
  if FFoundWords <> nil then
    FFoundWords.Free;
  FFoundWords := AValue;
end;

procedure TdxPDFDocumentSequentialTextSearch.SetOptions(const AValue: TdxPDFDocumentTextSearchOptions);
begin
  if FOptions.Direction <> AValue.Direction then
    DirectionChanged;
  FOptions := AValue;
  FMoveNext := GetStepDirection(FOptions.Direction);
end;

procedure TdxPDFDocumentSequentialTextSearch.SetSearchWords(const AValue: TStringList);
begin
  if FSearchWords <> nil then
    FSearchWords.Free;
  FSearchWords := AValue;
end;

function TdxPDFDocumentSequentialTextSearch.CanCompare: Boolean;
begin
  Result := (FPageWords <> nil) and not ((FPageWords.Count = 0) or (FSearchWords = nil) or (FSearchWords <> nil) and
      (FSearchWords.Count = 0) or (FWordIndex + FSearchWords.Count > FPageWords.Count));
end;

function TdxPDFDocumentSequentialTextSearch.CanSearch(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
  APageIndex: Integer): Boolean;
begin
  Result := (AText <> '') and (Document <> nil) and (Document.PageCount > 0) and Initialize(AText, APageIndex, AOptions);
end;

function TdxPDFDocumentSequentialTextSearch.CompareWordList(APageWordList: TStringList): Boolean;
var
  I: Integer;
  S1, S2: string;
begin
  Result := True;
  for I := 1 to FSearchWords.Count - 1 do
  begin
    S1 := PrepareComparingWord(APageWordList[I]);
    S2 := PrepareComparingWord(FSearchWords[I]);
    if I < FSearchWords.Count - 1 then
    begin
      if S1 <> S2 then
        Exit(False);
    end
    else
      if FOptions.WholeWords and (S1 <> S2) or
        not(FOptions.WholeWords or TdxPDFTextUtils.StartsWith(S1, S2)) then
        Exit(False);
  end;
  for I := FWordIndex to FWordIndex + FSearchWords.Count - 1 do
    FFoundWords.Add(FPageWords[I]);
end;

function TdxPDFDocumentSequentialTextSearch.CompareWords(const AWord1, AWord2: string): Boolean;
begin
  if FOptions.WholeWords then
  begin
    Result := AWord1 = AWord2;
    if Result then
      FFoundWords.Add(FPageWords[FWordIndex]);
  end
  else
  begin
    Result := Pos(AWord2, AWord1) > 0;
    if Result then
      FFoundWords.Add(FPageWords[FWordIndex]);
  end;
end;

function TdxPDFDocumentSequentialTextSearch.CreateWordList(const AText: string): TStringList;
const
  LineBreakMarker = #13#10;
var
  I: Integer;
  S, S1, ADelimiter: string;
begin
  S := StringReplace(AText, #9, LineBreakMarker, [rfReplaceAll]);
  for ADelimiter in FWordDelimiters do
    S := StringReplace(S, ADelimiter, ADelimiter + LineBreakMarker, [rfReplaceAll]);
  for I := 1 to Length(S) do
    if TdxPDFTextUtils.IsCJK(S[I]) then
      S1 := S1 + S[I] + LineBreakMarker
    else
      S1 := S1 + S[I];
  S1 := StringReplace(S1, ' ', LineBreakMarker, [rfReplaceAll]);
  Result := TStringList.Create;
  if S1 = LineBreakMarker then
    S1 := '';
  Result.Text := S1;
end;

function TdxPDFDocumentSequentialTextSearch.GetPageText(AWordIndex, ACount: Integer): string;

  function TryGetNumericReplacement(const ACharacter: string): string;
  begin
    Result := '';
    if not FArabicNumericReplacements.TryGetValue(ACharacter, Result) then
      Result := ACharacter;
  end;

var
  I, J, ACurrentWordIndex: Integer;
  ABuilder: TdxBiDiStringBuilder;
  APart: TdxPDFTextWordPart;
  AWord: TdxPDFTextWord;
begin
  ACurrentWordIndex := FWordIndex;
  ABuilder := TdxBiDiStringBuilder.Create;
  if FPageWords <> nil then
    try
      for I := 0 to ACount - 1 do
      begin
        AWord := FPageWords[ACurrentWordIndex];
        if AWord <> nil then
          for APart in TdxPDFTextWordAccess(AWord).PartList do
          begin
            for J := Low(APart.Characters) to High(APart.Characters) do
              ABuilder.Append(TryGetNumericReplacement(APart.Characters[J].Text));
            if TdxPDFTextWordPartAccess(APart).WordEnded then
              ABuilder.Append(' ');
          end;
        Inc(ACurrentWordIndex);
      end;
      Result := TrimRight(ABuilder.EndCurrentLineAndGetString);
    finally
      ABuilder.Free;
    end;
end;

function TdxPDFDocumentSequentialTextSearch.GetProgressPercent: Integer;
begin
  Result := Round(FProcessedPageIndexes.Count / FDocument.PageCount * 100);
end;

function TdxPDFDocumentSequentialTextSearch.GetStepDirection(ADirection: TdxPDFDocumentTextSearchDirection): TMoveNextProc;
begin
  if ADirection = tsdForward then
    Result := StepForward
  else
    Result := StepBackward;
end;

function TdxPDFDocumentSequentialTextSearch.Initialize(const AText: string; APageIndex: Integer;
  const AOptions: TdxPDFDocumentTextSearchOptions): Boolean;

  function SameTextOptions(const P1, P2: TdxPDFDocumentTextSearchOptions): Boolean;
  begin
    Result := (P1.CaseSensitive = P2.CaseSensitive) and (P1.WholeWords = P2.WholeWords);
  end;

var
  ARecognitionsStartIndex: Integer;
begin
  Result := True;
  if FAborted or (AText <> FSearchString) or not SameTextOptions(Options, AOptions) then
  begin
    FAborted := False;
    FMoveNext := GetStepDirection(AOptions.Direction);
    FLastSearchResult.Range := TdxPDFPageTextRange.Invalid;
    FLastSearchResult.Status := tssNotFound;
    FPageIndex := APageIndex;
    FWordIndex := -1;
    ARecognitionsStartIndex := FPageIndex;
    RecognizeCurrentPage;
    FProcessedPageIndexes.Clear;
    if FPageLines <> nil then
      while (FPageWords = nil) or (FPageWords.Count = 0) do
      begin
        FMoveNext;
        RecognizeCurrentPage;
        if FPageIndex = ARecognitionsStartIndex then
          Exit(False);
      end;
    InternalClear;
    FHasResults := False;
    FSearchString := AText;
    FStartPageIndex := FPageIndex;
    FStartWordIndex := -1;
    SearchWords := CreateWordList(FSearchString);
  end;
  if FCompleted then
    ClearAfterComplete;
end;

function TdxPDFDocumentSequentialTextSearch.PrepareComparingWord(const AWord: string): string;
begin
  Result := AWord;
  if not FOptions.CaseSensitive then
    Result := UpperCase(Result);
end;

function TdxPDFDocumentSequentialTextSearch.TryCompare: Boolean;
var
  APageWord, ASearchWord: string;
  APageWordList: TStringList;
begin
  Result := False;
  APageWordList := CreateWordList(GetPageText(FWordIndex, FSearchWords.Count));
  try
    if APageWordList.Count > 0 then
    begin
      APageWord := PrepareComparingWord(APageWordList[0]);
      ASearchWord := PrepareComparingWord(FSearchWords[0]);
      if FSearchWords.Count = 1 then
        Result := CompareWords(APageWord, ASearchWord);
      if not Result then
      begin
        if FOptions.WholeWords and (APageWord <> ASearchWord) or  not (FOptions.WholeWords or
          TdxPDFTextUtils.EndsWith(APageWord, ASearchWord)) then
          Exit(False);
         Result := CompareWordList(APageWordList);
      end;
    end;
  finally
    APageWordList.Free;
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.PackCurrentPageRecognizedContent;
begin
  ResetRecognizedText;
  if InRange(FPageIndex, 0, FDocument.PageCount - 1) then
    FDocument.Pages[FPageIndex].PackRecognizedContent;
end;

procedure TdxPDFDocumentSequentialTextSearch.ProgressChanged;
begin
  if Assigned(OnProgress) then
    try
      OnProgress(FDocument, FPageIndex, GetProgressPercent);
    except
      on EdxPDFAbortException do
      begin
        FAborted := True;
        raise
      end
      else
        raise;
    end;
end;

procedure TdxPDFDocumentSequentialTextSearch.RecognizeCurrentPage;
var
  APage: TdxPDFPage;
begin
  ResetRecognizedText;
  APage := FDocument.Pages[FPageIndex];
  if APage.RecognizedContent <> nil then
  begin
    FPageLines := APage.RecognizedContent.TextLines;
    FPageWords := TdxPDFTextLineListAccess(FPageLines).WordList;
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.ResetRecognizedText;
begin
  FPageLines := nil;
  FPageWords := nil;
end;

procedure TdxPDFDocumentSequentialTextSearch.StepBackward;
begin
  if FWordIndex <= 0 then
  begin
    UpdateProcessedPageIndexes;
    PackCurrentPageRecognizedContent;
    FPageIndex := IfThen(FPageIndex <= 0, FDocument.PageCount - 1, FPageIndex - 1);
    RecognizeCurrentPage;
    FWordIndex := FPageWords.Count - 1;
  end
  else
    Dec(FWordIndex);
end;

procedure TdxPDFDocumentSequentialTextSearch.StepForward;
begin
  if FPageWords <> nil then
  begin
    if FWordIndex >= FPageWords.Count - 1 then
    begin
      UpdateProcessedPageIndexes;
      PackCurrentPageRecognizedContent;
      FPageIndex := IfThen(FPageIndex >= FDocument.PageCount - 1, 0, FPageIndex + 1);
      FWordIndex := 0;
      RecognizeCurrentPage;
    end
    else
      Inc(FWordIndex);
  end;
end;

procedure TdxPDFDocumentSequentialTextSearch.UpdateProcessedPageIndexes;
begin
  if not FProcessedPageIndexes.Contains(FPageIndex) then
  begin
    FProcessedPageIndexes.Add(FPageIndex);
    ProgressChanged;
  end;
end;

{ TdxPDFDocumentContinuousTextSearch }

constructor TdxPDFDocumentContinuousTextSearch.Create;
begin
  inherited Create;
  FSearchResultList := TList<TdxPDFDocumentTextSearchResult>.Create;
end;

destructor TdxPDFDocumentContinuousTextSearch.Destroy;
begin
  FreeAndNil(FSearchResultList);
  inherited Destroy;
end;

function TdxPDFDocumentContinuousTextSearch.GetLastSearchRecord: TdxPDFDocumentTextSearchResult;
begin
  if FSearchResultList.Count > 0 then
    Result := FSearchResultList[Max(FCurrentResultIndex, 0)]
  else
    Result := GetSearchResult(PageIndex, nil, tssNotFound);
end;

procedure TdxPDFDocumentContinuousTextSearch.ClearAfterComplete;
begin
//
end;

procedure TdxPDFDocumentContinuousTextSearch.DirectionChanged;
var
  ANotFoundRecord: TdxPDFDocumentTextSearchResult;
begin
  if FSearchResultList.Count > 0 then
  begin
    ANotFoundRecord := FSearchResultList[FNotFoundRecordIndex];
    FSearchResultList.Delete(FNotFoundRecordIndex);
    FSearchResultList.Insert(FCurrentResultIndex, ANotFoundRecord);
    FNotFoundRecordIndex := FCurrentResultIndex;
  end;
end;

procedure TdxPDFDocumentContinuousTextSearch.InternalClear;
begin
  inherited InternalClear;
  FCurrentResultIndex := -1;
  Completed := False;
  FSearchResultList.Clear;
end;

procedure TdxPDFDocumentContinuousTextSearch.InternalFind;
const
  StatusMap: array[Boolean] of TdxPDFDocumentTextSearchStatus = (tssNotFound, tssFinished);
begin
  if not Completed and SearchStart then
  begin
    ClearProcessedPageIndexes;
    InternalClear;
    FCurrentResultIndex := 0;
    if DoFind then
      while not (LastSearchResult.Status in [tssNotFound, tssFinished]) do
      begin
        Completed := not DoFind;
        if Completed then
        begin
          LastSearchResult := GetSearchResult(PageIndex, nil, StatusMap[HasResults]);
          FNotFoundRecordIndex := FSearchResultList.Count - 1;
          Break;
        end;
      end;
    Completed := True;
    FCurrentResultIndex := -1;
  end;
  CalculateCurrentResultIndex;
end;

procedure TdxPDFDocumentContinuousTextSearch.SetLastSearchRecord(const AValue: TdxPDFDocumentTextSearchResult);
begin
  FSearchResultList.Add(AValue);
end;

function TdxPDFDocumentContinuousTextSearch.GetFoundRanges: TdxPDFPageTextRanges;
var
  I: Integer;
begin
  SetLength(Result, FSearchResultList.Count);
  for I := 0 to FSearchResultList.Count - 1 do
    if FSearchResultList[I].Status = tssFound then
      Result[I] := FSearchResultList[I].Range;
end;

procedure TdxPDFDocumentContinuousTextSearch.CalculateCurrentResultIndex;
begin
  if Options.Direction = tsdForward then
    Inc(FCurrentResultIndex)
  else
    Dec(FCurrentResultIndex);
  if FCurrentResultIndex < 0 then
    FCurrentResultIndex := FSearchResultList.Count - 1;
  if FCurrentResultIndex > FSearchResultList.Count - 1 then
    FCurrentResultIndex := 0;
end;

end.
