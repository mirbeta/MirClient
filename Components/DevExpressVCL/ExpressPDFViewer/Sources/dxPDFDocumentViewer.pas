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

unit dxPDFDocumentViewer;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Controls, Windows, Graphics, Generics.Defaults, Generics.Collections, cxGraphics, cxGeometry,
  cxClasses, cxControls, cxLookAndFeels, dxCustomPreview, dxPDFCore, dxPDFDocument, dxPDFCommandInterpreter;

const
  dxPDFDocumentPageThumbnailViewerMaxSize = 1000;
  dxPDFDocumentPageThumbnailViewerMinSize = 160;
  dxPDFDocumentPageThumbnailViewerSizeStep = 50;
  dxPDFDocumentViewerDefaultRenderContentDelay = 100;

type
  TdxPDFDocumentCustomViewer = class;
  TdxPDFDocumentCustomViewerPage = class;
  TdxPDFDocumentPageThumbnailViewer = class;

  TdxPDFDocumentViewerOnCancelRendering = procedure(Sender: TObject; const APageIndexes: TIntegerDynArray) of object;
  TdxPDFDocumentViewerOnGetRenderFactor = function(APageIndex: Integer): Single of object;

  TdxPDFDocumentViewerCachePageInfo = class // for internal use
  public
    Index: Integer;
    Factor: Single;
  end;

  TdxPDFPreRenderPageInfo = record
    Bounds: TRect;
    PageIndex: Integer;
    Scale: Single;
    Thumbnail: TBitmap;
  end;

  TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    const APageInfo: TdxPDFPreRenderPageInfo; var ADone: Boolean) of object;

  { TdxPDFDocumentViewerCacheStorage }

  TdxPDFDocumentViewerCacheStorage = class // for internal use
  strict private type
  {$REGION 'internal types'}
    { TCacheItem }

    TCacheItem = class
    public
      Bitmap: TBitmap;
      Factor: Single;
      constructor Create(ABitmap: TBitmap; AFactor: Single);
      destructor Destroy; override;
    end;

    { TCache }

    TCache = class
    strict private
      FDictionary: TObjectDictionary<Integer, TCacheItem>;
      FLimit: Int64;
      FQueue: TList<Integer>;
      FSize: Int64;
      function GetSize(AValue: TBitmap): Int64;
      procedure CheckSize;
    protected
      function TryGetValue(APageIndex: Integer; out ABitmap: TBitmap): Boolean; overload;
      function TryGetValue(APageIndex: Integer; out ACache: TCacheItem): Boolean; overload;
      function TryGetValue(APageIndex: Integer; AFactor: Single; out ACache: TCacheItem): Boolean; overload;
      procedure Add(APageIndex: Integer; ACache: TCacheItem);
      procedure Clear; overload;
      procedure Remove(APageIndex: Integer);
    public
      constructor Create(ALimit: Int64);
      destructor Destroy; override;
    end;
  {$ENDREGION}
  strict private
    FCacheInfos: TDictionary<Integer, Single>;
    FCacheVisiblePagesOnly: Boolean;
    FContainer: TObjectDictionary<Integer, TCacheItem>;
    procedure SetCacheVisiblePagesOnly(const AValue: Boolean);
    procedure AddThumbnail(APageIndex: Integer; ACache: TCacheItem);
  protected
    FThumbnails: TCache;
    OnCancelRendering: TdxPDFDocumentViewerOnCancelRendering;
    function CheckThumbnails(APageIndex: Integer; AFactor: Single): Boolean;
    function Contains(APageIndex: Integer; AFactor: Single): Boolean;
    function ContainsThumbnail(APageIndex: Integer; AFactor: Single): Boolean;
    function CreateCache(ABitmap: TBitmap; AFactor: Single): TCacheItem;
    function GetBitmap(APageIndex: Integer): TBitmap;
    function GetThumbnail(APageIndex: Integer): TBitmap;
    procedure Add(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);
    procedure Clear;
    procedure Remove(APageIndex: Integer; AForceAddThumbnail: Boolean);
    procedure SetVisiblePages(APages: TList<TdxPDFDocumentViewerCachePageInfo>);

    property CacheVisiblePagesOnly: Boolean read FCacheVisiblePagesOnly write SetCacheVisiblePagesOnly;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxPDFRenderTaskInfo }

  TdxPDFRenderTaskInfo = record // for internal use
    Factor: Single;
    PageIndex: Integer;
    class function Create(APageIndex: Integer; AFactor: Single): TdxPDFRenderTaskInfo; static;
  end;

  { TdxPDFDocumentViewerCustomRenderer }

  TdxPDFDocumentViewerCustomRendererClass = class of TdxPDFDocumentViewerCustomRenderer;
  TdxPDFDocumentViewerCustomRenderer = class
  strict private
    FEndVisiblePageIndex: Integer;
    FNeedPackDocumentContent: Boolean;
    FQueue: TQueue<TdxPDFRenderTaskInfo>;
    FStartVisiblePageIndex: Integer;
    FStorage: TdxPDFDocumentViewerCacheStorage;
    FTimer: TcxTimer;
    FViewer: TdxPDFDocumentCustomViewer;

    FOnGetRenderFactor: TdxPDFDocumentViewerOnGetRenderFactor;

    function GetCacheVisiblePagesOnly: Boolean;
    procedure SetCacheVisiblePagesOnly(const AValue: Boolean);

    function CreatePageInfo(APageIndex: Integer): TdxPDFDocumentViewerCachePageInfo;
    procedure AddPageIndex(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>; APageIndex: Integer);
    procedure AddPageIndexes(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>; AStartIndex, AEndIndex: Integer);
    procedure AddPageIndexesToRenderQueue(APages: TList<TdxPDFDocumentViewerCachePageInfo>; AForce: Boolean);
    procedure InitializeVisiblePageIndexes;
    procedure OnCancelRenderingHandler(Sender: TObject; const APageIndexes: TIntegerDynArray);
    procedure OnTimerHandler(Sender: TObject);
  strict protected
    function GetPreRenderPageScale(APageIndex: Integer): Single; virtual;
    function NeedProcessRenderTask(const ATask: TdxPDFRenderTaskInfo): Boolean; virtual;
    function TryPackDocumentContent: Boolean; virtual;
    procedure CancelRendering(const APageIndexes: TIntegerDynArray); virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoDrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect); virtual;
    procedure Render(APageIndex: Integer; AFactor: Single); virtual;
    procedure RenderNextPage; virtual;

    procedure Add(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);

    property CacheVisiblePagesOnly: Boolean read GetCacheVisiblePagesOnly write SetCacheVisiblePagesOnly;
    property Storage: TdxPDFDocumentViewerCacheStorage read FStorage;
    property Viewer: TdxPDFDocumentCustomViewer read FViewer;
  protected
    function GetRenderingPageRowDelta: Integer; virtual;
    procedure Clear; virtual;
    procedure DrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect); virtual;
    procedure Start; virtual;
    procedure Stop; virtual;

    procedure UpdateRenderQueue(AForce: Boolean = False);

    property OnGetRenderFactor: TdxPDFDocumentViewerOnGetRenderFactor read FOnGetRenderFactor write FOnGetRenderFactor;
  public
    constructor Create(AViewer: TdxPDFDocumentCustomViewer); virtual;
    destructor Destroy; override;
  end;

  { TdxPDFDocumentViewerAsyncRenderer }

  TdxPDFDocumentViewerAsyncRenderer = class(TdxPDFDocumentViewerCustomRenderer)
  strict private
    FStopped: Boolean;
    procedure OnCompleteHandler(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);
  strict protected
    FService: TObject;
  protected
    function NeedProcessRenderTask(const ATask: TdxPDFRenderTaskInfo): Boolean; override;
    function TryPackDocumentContent: Boolean; override;
    procedure CancelRendering(const APageIndexes: TIntegerDynArray); override;
    procedure Clear; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Render(APageIndex: Integer; AFactor: Single); override;
    procedure RenderNextPage; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  { TdxPDFViewerPageSizeOptions }

  TdxPDFViewerPageSizeOptions = class(TdxPreviewPageSizeOptions)
  protected
    function GetActualSizeInPixels: TPoint; override;
    function GetDefaultMinUsefulSize: TPoint; override;
  end;

  { TdxPDFViewerDocumentState }

  TdxPDFViewerDocumentState = class(TdxPDFDocumentState) // for internal use
  strict private
    FDocumentToViewerFactor: TdxPointF;
    FDPI: TdxPointF;
    FHandTool: Boolean;
    FViewer: TdxPDFDocumentCustomViewer;
    function GetScaleFactor: TdxPointF;
    function GetViewerPage(APageIndex: Integer): TdxPDFDocumentCustomViewerPage;
  protected
    procedure Initialize; override;

    function CreateRenderParameters: TdxPDFRenderParameters;
    function ToDocumentPoint(APage: TdxPDFDocumentCustomViewerPage; const P: TdxPointF): TdxPointF;

    property DPI: TdxPointF read FDPI;
    property Viewer: TdxPDFDocumentCustomViewer read FViewer;
  public
    constructor Create(AOwner: TObject; AViewer: TdxPDFDocumentCustomViewer); reintroduce;

    function ToDocumentRect(APage: TdxPDFDocumentCustomViewerPage; const R: TdxRectF): TdxRectF;
    function ToViewerPoint(APage: TdxPDFDocumentCustomViewerPage; const P: TdxPointF): TdxPointF;
    function ToViewerRect(const AObject: IdxPDFInteractiveObject): TRect; overload;
    function ToViewerRect(APage: TdxPDFDocumentCustomViewerPage; const R: TdxRectF): TdxRectF; overload;
    function ToViewerRect(AHyperlink: TdxPDFHyperlink): TRect; overload;
    procedure CalculateScreenFactors;

    property DocumentToViewerFactor: TdxPointF read FDocumentToViewerFactor;
    property HandTool: Boolean read FHandTool write FHandTool;
    property ScaleFactor: TdxPointF read GetScaleFactor;
  end;

  { TdxPDFDocumentCustomViewerPage }

  TdxPDFDocumentCustomViewerPage = class(TdxPreviewPage)
  strict private
    FDocumentPage: TdxPDFPage;
    function GetDocumentState: TdxPDFViewerDocumentState;
    procedure SetDocumentPage(const AValue: TdxPDFPage);
  protected
    function GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass; override;

    procedure CalculatePageSize; virtual; abstract; // for internal use

    function GetPageSize(const AScreenFactor: TdxPointF): TPoint; // for internal use
    function ToDocumentPoint(const P: TdxPointF): TdxPointF; // for internal use
    function ToDocumentRect(const R: TdxRectF): TdxRectF; // for internal use
    function ToViewerPoint(const P: TdxPointF): TdxPointF; // for internal use
    function ToViewerRect(const R: TdxRectF): TdxRectF; // for internal use

    property DocumentPage: TdxPDFPage read FDocumentPage write SetDocumentPage; // for internal use
    property DocumentState: TdxPDFViewerDocumentState read GetDocumentState; // for internal use
  end;

  { TViewerDocument }

  TdxPDFViewerDocument = class(TdxPDFDocument) // for internal use
  strict private
    FViewer: TdxPDFDocumentCustomViewer;
  protected
    procedure CreateDocumentState; override;
  public
    constructor Create(AViewer: TdxPDFDocumentCustomViewer); reintroduce;
  end;

  { TdxPDFDocumentCustomViewer }

  TdxPDFDocumentCustomViewer = class(TdxCustomPreview, IdxPDFDocumentListener)
  strict private type
  {$REGION 'internal types'}
    TDefaultRenderer = class(TdxPDFDocumentViewerCustomRenderer)
    strict private
      procedure ClearViewerCache;
    protected
      procedure DoDrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect); override;
      procedure Render(APageIndex: Integer; AFactor: Single); override;
    public
      destructor Destroy; override;
      procedure Start; override;
    end;
  {$ENDREGION}
  strict private
    FIsZooming: Boolean;
    FRenderContentDelay: Integer;
    FRenderContentInBackground: Boolean;
    FRenderContentTimer: TcxTimer;
    FRenderer: TdxPDFDocumentViewerCustomRenderer;

    FOnCustomDrawPreRenderPage: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent;

    function GetDocumentPages: TdxPDFPages;
    function GetDocumentState: TdxPDFViewerDocumentState;
    function GetIsZooming: Boolean;
    function GetRotationAngle: TcxRotationAngle;
    procedure SetRenderContentDelay(const AValue: Integer);
    procedure SetRenderContentInBackground(const AValue: Boolean);

    procedure StartZoomTimer;
    procedure StopZoomTimer;

    // IdxPDFDocumentListener
    procedure Changed(Sender: TObject);

    function OnGetRenderFactorHandler(APageIndex: Integer): Single;
    procedure OnInteractiveFormFieldValueChangedHandler(Sender: TObject);
    procedure OnRenderContentTimerHandler(Sender: TObject);
  strict protected
    FDocument: TdxPDFViewerDocument;
    procedure ClearRenderer;
  protected
    FForceUpdate: Boolean;
    procedure Calculate(AType: TdxChangeType); override;
    procedure ScaleFactorChanged; override;
    procedure CheckMargins; override;
    procedure ResyncMargins; override;

    function CanUpdateRenderQueue: Boolean; virtual;
    function CreateRenderer: TdxPDFDocumentViewerCustomRenderer; virtual;
    function GetPageRenderFactor(APageIndex: Integer): Single; virtual; abstract;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoCalculate; virtual;
    procedure Initialize; virtual;

    function GetNearestPageIndex(const P: TPoint): Integer;
    function IsPageVisible(AIndex: Integer): Boolean;
    procedure RecreateRenderer;
    procedure RestartRenderContentTimer;
    procedure UpdateRenderQueue(AForce: Boolean = False);

    property Document: TdxPDFViewerDocument read FDocument;
    property DocumentPages: TdxPDFPages read GetDocumentPages;
    property DocumentState: TdxPDFViewerDocumentState read GetDocumentState;
    property IsZooming: Boolean read GetIsZooming;
    property RenderContentDelay: Integer read FRenderContentDelay write SetRenderContentDelay;
    property RenderContentInBackground: Boolean read FRenderContentInBackground write SetRenderContentInBackground;
    property RotationAngle: TcxRotationAngle read GetRotationAngle;
    property Renderer: TdxPDFDocumentViewerCustomRenderer read FRenderer;

    property OnCustomDrawPreRenderPage: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent read FOnCustomDrawPreRenderPage
      write FOnCustomDrawPreRenderPage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxPDFDocumentPageThumbnailViewer }

  TdxPDFDocumentPageThumbnailViewer = class(TdxPDFDocumentCustomViewer, IdxSkinSupport, IcxFontListener)
  strict private type
  {$REGION 'internal types'}
    TRenderer = class(TdxPDFDocumentViewerAsyncRenderer)
    protected
      function GetPreRenderPageScale(APageIndex: Integer): Single; override;
      function GetRenderingPageRowDelta: Integer; override;
      procedure CreateSubClasses; override;
      procedure Render(APageIndex: Integer; AFactor: Single); override;
    end;

    TPage = class(TdxPDFDocumentCustomViewerPage)
    strict private const
      DefaultIndent = 10;
    strict private
      FCaptionBounds: TRect;
      FImageBorderBounds: TRect;
      FImageBounds: TRect;
      FSelectionBounds: TRect;
      FThumbnailBounds: TRect;

      function GetImageSize: Integer;
      function GetSelectionSize: TRect;
      function GetThumbnailsPreview: TdxPDFDocumentPageThumbnailViewer;

      procedure DrawBackground(ACanvas: TcxCanvas);
      procedure DrawContent(ACanvas: TcxCanvas);
      procedure DrawFocusRect(ACanvas: TcxCanvas);
      procedure DrawSelection(ACanvas: TcxCanvas);
      procedure DrawIndex(ACanvas: TcxCanvas);
    protected
      procedure CalculatePageSize; override;
      procedure CalculateLayout;

      property ImageSize: Integer read GetImageSize;
      property ThumbnailsPreview: TdxPDFDocumentPageThumbnailViewer read GetThumbnailsPreview;
    public
      procedure Draw(ACanvas: TcxCanvas); override;
    end;
  {$ENDREGION}
  strict private
    FCurrentFocusedPageIndex: Integer;
    FLastClickedPageIndex: Integer;
    FMaxSize: Integer;
    FMinSize: Integer;
    FPageCaptionSize: TSize;
    FPopupMenu: TComponent;
    FSelectedPages: TList<Integer>;
    FSize: Integer;

    FOnThumbnailSizeChanged: TNotifyEvent;

    function GetPageCaptionAreaHeight: Integer;
    function GetPageCaptionMargin: Integer;
    procedure SetDocument(const AValue: TdxPDFViewerDocument);
    procedure SetMaxSize(const AValue: Integer);
    procedure SetMinSize(const AValue: Integer);
    procedure SetSize(const AValue: Integer);

    procedure Changed(Sender: TObject; AFont: TFont);
    procedure CheckSize;
    procedure CalculateCaptionSize;
    procedure RecreatePages(AForce: Boolean = True);
    procedure UpdateSelectionRange;

    procedure OnRotationAngleChangedHandler(Sender: TObject);
  protected
    function CreateRenderer: TdxPDFDocumentViewerCustomRenderer; override;
    function GetPageRenderFactor(APageIndex: Integer): Single; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoCalculate; override;
    procedure Initialize; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    function CreatePage: TdxPreviewPage; override;
    function GetAbsoluteIndentLeft: Integer; override;
    function GetAbsoluteIndentRight: Integer; override;
    function GetCursor: TCursor; override;
    function GetDefaultZoomStep: Integer; override;
    function GetPageClass: TdxPreviewPageClass; override;
    function NonVerticalCenterizePages: Boolean; override;
    procedure Calculate(AType: TdxChangeType); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoSelectedPageChanged; override;
    procedure DoZoomIn; override;
    procedure DoZoomOut; override;
    procedure DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean); override;
    procedure DrawViewerBackground(ACanvas: TcxCanvas; const R: TRect); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer); override;

    function IsPageSelected(APageIndex: Integer): Boolean;
    procedure Clear;

    function GetPagesToPrint: TIntegerDynArray;

    property Document: TdxPDFViewerDocument read FDocument write SetDocument;
    property MaxSize: Integer read FMaxSize write SetMaxSize;
    property MinSize: Integer read FMinSize write SetMinSize;
    property PageCaptionAreaHeight: Integer read GetPageCaptionAreaHeight;
    property PageCaptionSize: TSize read FPageCaptionSize;
    property PageCaptionMargin: Integer read GetPageCaptionMargin;
    property SelPageIndex;
    property Size: Integer read FSize write SetSize;
    property OnThumbnailSizeChanged: TNotifyEvent read FOnThumbnailSizeChanged write FOnThumbnailSizeChanged;
  end;

implementation

uses
  Math, SysUtils, SyncObjs, dxCore, dxCoreGraphics, dxThreading, dxGDIPlusClasses, dxTypeHelpers, dxPDFTypes, dxPDFViewer,
  dxPDFViewerPopupMenu, dxPDFUtils;

const
  dxPDFDocumentViewerPageCacheSize = 100;

type
  TdxCustomPreviewAccess = class(TdxCustomPreview);
  TdxPDFBackgroundService = class;
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFDocumentStateAccess = class(TdxPDFDocumentState);
  TdxPDFHyperlinkAccess = class(TdxPDFHyperlink);
  TdxPDFPagesAccess = class(TdxPDFPages);
  TdxPreviewPageContentCachePoolAccess = class(TdxPreviewPageContentCachePool);

  TdxPDFOnRenderingComplete = procedure(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap) of object;

  TdxPDFDocumentViewerCachePageInfoComparer = class(TInterfacedObject, IComparer<TdxPDFDocumentViewerCachePageInfo>)
  strict private
    function Compare(const Left, Right: TdxPDFDocumentViewerCachePageInfo): Integer;
  end;

  { TdxPDFRenderPageTask }

  TdxPDFRenderPageTaskClass = class of TdxPDFRenderPageTask;
  TdxPDFRenderPageTask = class(TInterfacedObject, IdxTask)
  strict private
    FService: TdxPDFBackgroundService;
  protected
    function Render(var AImage: TdxSmartImage; const ACancelStatus: TdxTaskCancelCallback): Boolean; virtual; abstract;
  public
    Bitmap: TBitmap;
    Handle: THandle;
    PageIndex: Integer;
    Factor: Single;
    Viewer: TdxPDFDocumentCustomViewer;
    constructor Create(AService: TdxPDFBackgroundService; AViewer: TdxPDFDocumentCustomViewer; APageIndex: Integer;
      AFactor: Single);
    destructor Destroy; override;

    function Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
  end;

  { TdxPDFRenderPageByScaleFactorTask }

  TdxPDFRenderPageByScaleFactorTask = class(TdxPDFRenderPageTask)
  protected
    function Render(var AImage: TdxSmartImage; const ACancelStatus: TdxTaskCancelCallback): Boolean; override;
  end;

  { TdxPDFRenderPageBySizeTask }

  TdxPDFRenderPageBySizeTask = class(TdxPDFRenderPageTask)
  protected
    function Render(var AImage: TdxSmartImage; const ACancelStatus: TdxTaskCancelCallback): Boolean; override;
  end;

  { TdxPDFBackgroundService }

  TdxPDFBackgroundService = class
  strict private
    FTasks: TThreadList;
    function CreateRenderingTask(AClass: TdxPDFRenderPageTaskClass; AViewer: TdxPDFDocumentCustomViewer;
      APageIndex: Integer; AFactor: Single): IdxTask;
    function Cancel(ATaskHandle: THandle; AWaitFor: Boolean = False): Boolean;
    function GetTaskCount: Integer;
    procedure AddRenderingTask(AClass: TdxPDFRenderPageTaskClass; AViewer: TdxPDFDocumentCustomViewer;
      APageIndex: Integer; AFactor: Single);
    procedure CancelTasks(AWaitFor: Boolean);
  protected
    procedure Complete(const ATask: IdxTask; AStatus: TdxTaskCompletedStatus);
  public
    OnRenderingComplete: TdxPDFOnRenderingComplete;
    constructor Create;
    destructor Destroy; override;

    procedure AddRenderingByScaleFactorTask(AViewer: TdxPDFDocumentCustomViewer; APageIndex: Integer; AFactor: Single);
    procedure AddRenderingBySizeTask(AViewer: TdxPDFDocumentCustomViewer; APageIndex: Integer; AFactor: Single);
    procedure CancelAll;
    procedure CancelAllAndWait;
    procedure CancelTask(APageIndex: Integer);

    property TaskCount: Integer read GetTaskCount;
  end;

{ TdxPDFRenderPageTask }

constructor TdxPDFRenderPageTask.Create(AService: TdxPDFBackgroundService; AViewer: TdxPDFDocumentCustomViewer;
  APageIndex: Integer; AFactor: Single);
begin
  inherited Create;
  FService := AService;
  PageIndex := APageIndex;
  Factor := AFactor;
  Viewer := AViewer;
end;

destructor TdxPDFRenderPageTask.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited Destroy;
end;

function TdxPDFRenderPageTask.Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AImage: TdxSmartImage;
begin
  Result := TdxTaskCompletedStatus.Fail;
  if not ACancelStatus then
  try
    AImage := TdxSmartImage.Create;
    try
      if Render(AImage, ACancelStatus) and not ACancelStatus then
      begin
        Bitmap := AImage.GetAsBitmap;
        Result := TdxTaskCompletedStatus.Success;
      end
      else
        Result := TdxTaskCompletedStatus.Cancelled;
    finally
      AImage.Free;
    end;
  except
    Result := TdxTaskCompletedStatus.Fail;
  end;
end;

procedure TdxPDFRenderPageTask.OnComplete(AStatus: TdxTaskCompletedStatus);
begin
  FService.Complete(Self, AStatus);
end;

{ TdxPDFRenderPageByScaleFactorTask }

function TdxPDFRenderPageByScaleFactorTask.Render(var AImage: TdxSmartImage; const ACancelStatus: TdxTaskCancelCallback): Boolean;
var
  ABitmap: TcxBitmap;
  ADevice: TdxPDFGraphicsDevice;
  AParameters: TdxPDFRenderParameters;
begin
  Result := not ACancelStatus;
  if Result then
  begin
    ABitmap := TcxBitmap.CreateSize(Viewer.PageList[PageIndex].Bounds);
    try
      AParameters := Viewer.DocumentState.CreateRenderParameters;
      try
        AParameters.CancelCallback := ACancelStatus;
        AParameters.Canvas := ABitmap.cxCanvas.Canvas;
        AParameters.Rect := ABitmap.ClientRect;
        Result := not ACancelStatus;
        if Result then
        begin
          ADevice := TdxPDFGraphicsDevice.Create;
          try
            ADevice.Export(Viewer.Document.Pages[PageIndex], AParameters);
          finally
            ADevice.Free;
          end;
        end;
      finally
        AParameters.Free;
      end;
      Result := not ACancelStatus;
      if Result then
      begin
        FreeAndNil(AImage);
        AImage := TdxSmartImage.CreateFromBitmap(ABitmap);
      end;
    finally
      ABitmap.Free;
    end;
  end;
end;

{ TdxPDFRenderPageBySizeTask }

function TdxPDFRenderPageBySizeTask.Render(var AImage: TdxSmartImage; const ACancelStatus: TdxTaskCancelCallback): Boolean;
begin
  Result := dxPDFDocumentExportToImage(Viewer.Document, PageIndex, Trunc(Factor), AImage, Viewer.RotationAngle,
    ACancelStatus);
end;

{ TdxPDFDocumentViewerCachePageInfoComparer }

function TdxPDFDocumentViewerCachePageInfoComparer.Compare(const Left, Right: TdxPDFDocumentViewerCachePageInfo): Integer;
begin
  Result := Left.Index - Right.Index;
end;

{ TdxPDFBackgroundService }

constructor TdxPDFBackgroundService.Create;
begin
  inherited Create;
  FTasks := TThreadList.Create;
end;

destructor TdxPDFBackgroundService.Destroy;
begin
  CancelAllAndWait;
  FreeAndNil(FTasks);
  inherited Destroy;
end;

procedure TdxPDFBackgroundService.AddRenderingByScaleFactorTask(AViewer: TdxPDFDocumentCustomViewer;
  APageIndex: Integer; AFactor: Single);
begin
  AddRenderingTask(TdxPDFRenderPageByScaleFactorTask, AViewer, APageIndex, AFactor);
end;

procedure TdxPDFBackgroundService.AddRenderingBySizeTask(AViewer: TdxPDFDocumentCustomViewer;
  APageIndex: Integer; AFactor: Single);
begin
  AddRenderingTask(TdxPDFRenderPageBySizeTask, AViewer, APageIndex, AFactor);
end;

procedure TdxPDFBackgroundService.CancelAll;
begin
  CancelTasks(False);
end;

procedure TdxPDFBackgroundService.CancelAllAndWait;
begin
  CancelTasks(True);
end;

procedure TdxPDFBackgroundService.CancelTask(APageIndex: Integer);
var
  I: Integer;
  AHandle: THandle;
  AList: TList;
begin
  AList := FTasks.LockList;
  try
    for I := 0 to AList.Count - 1 do
    begin
      AHandle := THandle(AList[I]);
      if (AHandle <> 0) and (TdxPDFRenderPageTask(AHandle).PageIndex = APageIndex) then
      begin
        Cancel(AHandle);
        Break;
      end;
    end;
  finally
    FTasks.UnlockList;
  end;
end;

function TdxPDFBackgroundService.CreateRenderingTask(AClass: TdxPDFRenderPageTaskClass;
  AViewer: TdxPDFDocumentCustomViewer; APageIndex: Integer; AFactor: Single): IdxTask;
begin
  Result := AClass.Create(Self, AViewer, APageIndex, AFactor);
end;

function TdxPDFBackgroundService.Cancel(ATaskHandle: THandle; AWaitFor: Boolean = False): Boolean;
begin
  Result := SyncObjs.TWaitResult(dxTasksDispatcher.Cancel(ATaskHandle, IfThen(AWaitFor, INFINITE))) <> wrError;
end;

function TdxPDFBackgroundService.GetTaskCount: Integer;
begin
  Result := FTasks.LockList.Count;
  FTasks.UnlockList;
end;

procedure TdxPDFBackgroundService.AddRenderingTask(AClass: TdxPDFRenderPageTaskClass; AViewer: TdxPDFDocumentCustomViewer;
  APageIndex: Integer; AFactor: Single);
var
  AHandle: THandle;
  AList: TList;
  ATask: TdxPDFRenderPageTask;
begin
  if AViewer.Document <> nil then
  begin
    CancelTask(APageIndex);
    AList := FTasks.LockList;
    try
      ATask := CreateRenderingTask(AClass, AViewer, APageIndex, AFactor) as TdxPDFRenderPageTask;
      AHandle := dxTasksDispatcher.Run(ATask);
      ATask.Handle := AHandle;
      AList.Add(Pointer(ATask.Handle));
    finally
      FTasks.UnlockList;
    end;
  end;
end;

procedure TdxPDFBackgroundService.CancelTasks(AWaitFor: Boolean);
var
  I: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    AList.Assign(FTasks.LockList);
    FTasks.UnlockList;
    for I := 0 to AList.Count - 1 do
      Cancel(THandle(AList[I]), AWaitFor);
  finally
    AList.Free;
  end;
end;

procedure TdxPDFBackgroundService.Complete(const ATask: IdxTask; AStatus: TdxTaskCompletedStatus);
var
  APDFTask: TdxPDFRenderPageTask;
begin
  if ATask is TdxPDFRenderPageTask then
  begin
    APDFTask := TdxPDFRenderPageTask(ATask);
    FTasks.Remove(Pointer(APDFTask.Handle));
    if (AStatus = TdxTaskCompletedStatus.Success) and Assigned(OnRenderingComplete) then
      OnRenderingComplete(APDFTask.PageIndex, APDFTask.Factor, APDFTask.Bitmap);
  end;
end;

{ TdxPDFDocumentViewerCacheStorage }

constructor TdxPDFDocumentViewerCacheStorage.Create;
begin
  inherited Create;
  FThumbnails := TCache.Create(dxPDFDocumentViewerPageCacheSize);
  FContainer := TObjectDictionary<Integer, TCacheItem>.Create([doOwnsValues]);
  FCacheInfos := TDictionary<Integer, Single>.Create;
end;

destructor TdxPDFDocumentViewerCacheStorage.Destroy;
begin
  FreeAndNil(FThumbnails);
  FreeAndNil(FContainer);
  FreeAndNil(FCacheInfos);
  inherited Destroy;
end;

function TdxPDFDocumentViewerCacheStorage.CheckThumbnails(APageIndex: Integer; AFactor: Single): Boolean;
var
 ACache: TCacheItem;
begin
  Result := FThumbnails.TryGetValue(APageIndex, AFactor, ACache);
  if Result then
  begin
    Add(APageIndex, AFactor, ACache.Bitmap);
    FThumbnails.Remove(APageIndex);
  end
end;

function TdxPDFDocumentViewerCacheStorage.Contains(APageIndex: Integer; AFactor: Single): Boolean;
var
  ACache: TCacheItem;
begin
  Result := FContainer.TryGetValue(APageIndex, ACache);
  if Result then
    Result := SameValue(ACache.Factor, AFactor);
end;

function TdxPDFDocumentViewerCacheStorage.ContainsThumbnail(APageIndex: Integer; AFactor: Single): Boolean;
var
  ACache: TCacheItem;
begin
  Result := FThumbnails.TryGetValue(APageIndex, ACache);
  if Result then
    Result := SameValue(ACache.Factor, AFactor);
end;

function TdxPDFDocumentViewerCacheStorage.CreateCache(ABitmap: TBitmap; AFactor: Single): TCacheItem;
var
  AClone: TBitmap;
begin
  AClone := TBitmap.Create;
  AClone.Assign(ABitmap);
  Result := TCacheItem.Create(AClone, AFactor);
end;

function TdxPDFDocumentViewerCacheStorage.GetBitmap(APageIndex: Integer): TBitmap;
var
  AInfo: TCacheItem;
begin
  if FContainer.TryGetValue(APageIndex, AInfo) then
    Result := AInfo.Bitmap
  else
    Result := nil;
end;

function TdxPDFDocumentViewerCacheStorage.GetThumbnail(APageIndex: Integer): TBitmap;
begin
  if not FThumbnails.TryGetValue(APageIndex, Result) then
    Result := nil;
end;

procedure TdxPDFDocumentViewerCacheStorage.Add(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);
var
  AScale: Single;
begin
  AScale := 0;
  if FCacheInfos.TryGetValue(APageIndex, AScale) or SameValue(AFactor, AScale) then
    FContainer.AddOrSetValue(APageIndex, CreateCache(ABitmap, AFactor));
end;

procedure TdxPDFDocumentViewerCacheStorage.SetCacheVisiblePagesOnly(const AValue: Boolean);
begin
  if FCacheVisiblePagesOnly <> AValue then
  begin
    FCacheVisiblePagesOnly := AValue;
    if CacheVisiblePagesOnly then
      FThumbnails.Clear;
  end;
end;

procedure TdxPDFDocumentViewerCacheStorage.AddThumbnail(APageIndex: Integer; ACache: TCacheItem);
var
  AClone: TBitmap;
begin
  AClone := TBitmap.Create;
  AClone.Assign(ACache.Bitmap);
  FThumbnails.Add(APageIndex, TCacheItem.Create(AClone, ACache.Factor));
end;

procedure TdxPDFDocumentViewerCacheStorage.Clear;
begin
  FContainer.Clear;
  FCacheInfos.Clear;
  FThumbnails.Clear;
end;

procedure TdxPDFDocumentViewerCacheStorage.Remove(APageIndex: Integer; AForceAddThumbnail: Boolean);
var
  ACache: TCacheItem;
begin
  if (AForceAddThumbnail or not (AForceAddThumbnail or CacheVisiblePagesOnly)) and
    FContainer.TryGetValue(APageIndex, ACache) and not ACache.Bitmap.Empty then
    AddThumbnail(APageIndex, ACache);
  FContainer.Remove(APageIndex);
end;

procedure TdxPDFDocumentViewerCacheStorage.SetVisiblePages(APages: TList<TdxPDFDocumentViewerCachePageInfo>);
var
  AKeysToRemove: TIntegerDynArray;
  APageIndexes: TList<Integer>;
  APageIndex: Integer;
  APageInfo: TdxPDFDocumentViewerCachePageInfo;
begin
  FCacheInfos.Clear;
  APageIndexes := TList<Integer>.Create;
  try
    for APageInfo in APages do
    begin
      FCacheInfos.Add(APageInfo.Index, APageInfo.Factor);
      APageIndexes.Add(APageInfo.Index);
    end;
    SetLength(AKeysToRemove, 0);
    for APageIndex in FContainer.Keys do
      if not APageIndexes.Contains(APageIndex) then
        TdxPDFUtils.AddValue(APageIndex, AKeysToRemove);
    if Assigned(OnCancelRendering) then
      OnCancelRendering(Self, AKeysToRemove);
    for APageIndex in AKeysToRemove do
      Remove(APageIndex, False);
  finally
    APageIndexes.Free;
  end;
end;

{ TdxPDFDocumentViewerCacheStorage.TCacheItem }

constructor TdxPDFDocumentViewerCacheStorage.TCacheItem.Create(ABitmap: TBitmap; AFactor: Single);
begin
  inherited Create;
  Bitmap := ABitmap;
  Factor := AFactor;
end;

destructor TdxPDFDocumentViewerCacheStorage.TCacheItem.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited Destroy;
end;

{ TdxPDFDocumentViewerCacheStorage.TCache }

constructor TdxPDFDocumentViewerCacheStorage.TCache.Create(ALimit: Int64);
begin
  inherited Create;
  FDictionary := TObjectDictionary<Integer, TCacheItem>.Create([doOwnsValues]);
  FQueue := TList<Integer>.Create;
  FLimit := ALimit * 1024 * 1024;
end;

destructor TdxPDFDocumentViewerCacheStorage.TCache.Destroy;
begin
  Clear;
  FreeAndNil(FQueue);
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

procedure TdxPDFDocumentViewerCacheStorage.TCache.Clear;
begin
  FDictionary.Clear;
  FQueue.Clear;
  FSize := 0;
end;

function TdxPDFDocumentViewerCacheStorage.TCache.GetSize(AValue: TBitmap): Int64;
var
  AStream: TStream;
begin
  Result := 0;
  if AValue <> nil then
  begin
    AStream := TMemoryStream.Create;
    try
      AValue.SaveToStream(AStream);
      Result := AStream.Size;
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxPDFDocumentViewerCacheStorage.TCache.CheckSize;
var
  ADestSize: Integer;
begin
  if (FLimit > 0) and (FSize > FLimit) then
  begin
    ADestSize := FLimit - Trunc(FLimit * 0.25);
    while FSize > ADestSize do
      Remove(FQueue.First);
  end;
end;

procedure TdxPDFDocumentViewerCacheStorage.TCache.Remove(APageIndex: Integer);
var
  ABitmap: TBitmap;
begin
  if TryGetValue(APageIndex, ABitmap) then
  begin
    Dec(FSize, GetSize(ABitmap));
    FQueue.Remove(APageIndex);
    FDictionary.Remove(APageIndex);
  end;
end;

procedure TdxPDFDocumentViewerCacheStorage.TCache.Add(APageIndex: Integer;
  ACache: TCacheItem);
begin
  Inc(FSize, GetSize(ACache.Bitmap));
  if FDictionary.ContainsKey(APageIndex) then
    Remove(APageIndex);
  FDictionary.Add(APageIndex, ACache);
  FQueue.Add(APageIndex);
  CheckSize;
end;

function TdxPDFDocumentViewerCacheStorage.TCache.TryGetValue(APageIndex: Integer;
  out ABitmap: TBitmap): Boolean;
var
  ACache: TCacheItem;
begin
  Result := TryGetValue(APageIndex, ACache);
  if Result then
    ABitmap := ACache.Bitmap;
end;

function TdxPDFDocumentViewerCacheStorage.TCache.TryGetValue(APageIndex: Integer;
  out ACache: TCacheItem): Boolean;
begin
  Result := FDictionary.TryGetValue(APageIndex, ACache);
end;

function TdxPDFDocumentViewerCacheStorage.TCache.TryGetValue(APageIndex: Integer; AFactor: Single;
  out ACache: TCacheItem): Boolean;
begin
  Result := FDictionary.TryGetValue(APageIndex, ACache);
  if Result then
    Result := SameValue(AFactor, ACache.Factor);
end;

{ TdxPDFRenderTaskInfo }

class function TdxPDFRenderTaskInfo.Create(APageIndex: Integer; AFactor: Single): TdxPDFRenderTaskInfo;
begin
  Result.Factor := AFactor;
  Result.PageIndex := APageIndex;
end;

{ TdxPDFDocumentViewerCustomRenderer }

constructor TdxPDFDocumentViewerCustomRenderer.Create(AViewer: TdxPDFDocumentCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
  CreateSubClasses;
end;

destructor TdxPDFDocumentViewerCustomRenderer.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

function TdxPDFDocumentViewerCustomRenderer.GetRenderingPageRowDelta: Integer;
begin
  Result := 2;
end;

procedure TdxPDFDocumentViewerCustomRenderer.Clear;
begin
  FQueue.Clear;
  FStorage.Clear;
  InitializeVisiblePageIndexes;
end;

procedure TdxPDFDocumentViewerCustomRenderer.DrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect);
begin
  DoDrawPage(ACanvas, APageIndex, ARect)
end;

procedure TdxPDFDocumentViewerCustomRenderer.Start;
begin
// do nothing
end;

procedure TdxPDFDocumentViewerCustomRenderer.Stop;
begin
// do nothing
end;

procedure TdxPDFDocumentViewerCustomRenderer.UpdateRenderQueue(AForce: Boolean = False);

  procedure CalculateAdditionalPageIndexes(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>;
    APageIndex, ARowDelta: Integer);
  var
    I, J, ARowIndex: Integer;
    ARows: TdxPreviewPagesRowList;
  begin
    ARows := TdxCustomPreviewAccess(FViewer).PagesRows;
    for I := 0 to ARows.Count - 1 do
      if InRange(APageIndex, ARows[I].StartIndex, ARows[I].FinishIndex) then
      begin
        for J := 1 to Abs(ARowDelta) do
        begin
          ARowIndex := Min(Max(I + J * Sign(ARowDelta), 0), ARows.Count - 1);
          AddPageIndexes(APages, ARows[ARowIndex].StartIndex, ARows[ARowIndex].FinishIndex);
        end;
        Break;
      end;
  end;

  procedure CalculateVisiblePageIndexes(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>;
    out AStartPageIndex, AEndPageIndex: Integer);
  var
    I: Integer;
  begin
    AStartPageIndex := -1;
    AEndPageIndex := -1;
    for I := 0 to Viewer.PageCount - 1 do
      if Viewer.IsPageVisible(I) then
      begin
        if APages.Count = 0 then
          AStartPageIndex := I;
        AEndPageIndex := Max(I, AEndPageIndex);
        AddPageIndex(APages, I);
      end;
  end;

var
  AStartPageIndex, AEndPageIndex: Integer;
  APair: TPair<Integer, TdxPDFDocumentViewerCachePageInfo>;
  APages: TObjectDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>;
  APageIndexes: TList<TdxPDFDocumentViewerCachePageInfo>;
  APageRowDelta: Integer;
begin
  if Viewer.PageCount > 0 then
  begin
    APages := TObjectDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>.Create([doOwnsValues]);
    try
      CalculateVisiblePageIndexes(APages, AStartPageIndex, AEndPageIndex);
      if AForce or (FStartVisiblePageIndex <> AStartPageIndex) or (FEndVisiblePageIndex <> AEndPageIndex) then
      begin
        FStartVisiblePageIndex := AStartPageIndex;
        FEndVisiblePageIndex := AEndPageIndex;
        APageRowDelta := GetRenderingPageRowDelta;
        CalculateAdditionalPageIndexes(APages, FStartVisiblePageIndex, -APageRowDelta);
        CalculateAdditionalPageIndexes(APages, FEndVisiblePageIndex, APageRowDelta);
        APageIndexes := TList<TdxPDFDocumentViewerCachePageInfo>.Create;
        try
          for APair in APages do
            APageIndexes.Add(APair.Value);
          AddPageIndexesToRenderQueue(APageIndexes, AForce);
        finally
          APageIndexes.Free;
        end;
      end;
    finally
      APages.Free;
    end;
  end;
end;

function TdxPDFDocumentViewerCustomRenderer.GetPreRenderPageScale(APageIndex: Integer): Single;
begin
  Result := Viewer.DocumentState.ScaleFactor.X;
end;

function TdxPDFDocumentViewerCustomRenderer.NeedProcessRenderTask(const ATask: TdxPDFRenderTaskInfo): Boolean;
begin
  Result := FViewer.FForceUpdate;
end;

function TdxPDFDocumentViewerCustomRenderer.TryPackDocumentContent: Boolean;
var
  I: Integer;
begin
  for I := 0 to Viewer.DocumentPages.Count - 1 do
    Viewer.DocumentPages.Page[I].Pack;
  Result := True;
end;

procedure TdxPDFDocumentViewerCustomRenderer.CancelRendering(const APageIndexes: TIntegerDynArray);
begin
// do nothing
end;

procedure TdxPDFDocumentViewerCustomRenderer.CreateSubClasses;
begin
  FQueue := TQueue<TdxPDFRenderTaskInfo>.Create;

  FStorage := TdxPDFDocumentViewerCacheStorage.Create;
  FStorage.OnCancelRendering := OnCancelRenderingHandler;

  FTimer := TcxTimer.Create(nil);
  FTimer.Enabled := not FViewer.IsDesigning;
  FTimer.Interval := 10;
  FTimer.OnTimer := OnTimerHandler;

  InitializeVisiblePageIndexes;
end;

procedure TdxPDFDocumentViewerCustomRenderer.DestroySubClasses;
begin
  Clear;
  FreeAndNil(FTimer);
  FreeAndNil(FQueue);
  FreeAndNil(FStorage);
end;

procedure TdxPDFDocumentViewerCustomRenderer.DoDrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect);

  function GetPreRenderPageInfo(ABitmap: TBitmap): TdxPDFPreRenderPageInfo;
  begin
    Result.Bounds := ARect;
    Result.PageIndex := APageIndex;
    Result.Scale := GetPreRenderPageScale(Result.PageIndex);
    Result.Thumbnail := ABitmap;
  end;

  procedure Draw(ABitmap: TBitmap);
  begin
    if ABitmap <> nil then
      ACanvas.StretchDraw(ARect, ABitmap)
    else
      ACanvas.FillRect(ARect, clWhite);
  end;

  function IsStretchNeeded(ABitmap: TBitmap; const ARect: TRect): Boolean;
  begin
    Result := (ABitmap.Width > ARect.Width) or (ABitmap.Height > ARect.Height);
  end;

var
  ABitmap: TBitmap;
  ADone: Boolean;
begin
  ABitmap := FStorage.GetBitmap(APageIndex);
  if ABitmap <> nil then
  begin
    if IsStretchNeeded(ABitmap, ARect) then
      ACanvas.StretchDraw(ARect, ABitmap)
    else
      ACanvas.Draw(ARect.Left, ARect.Top, ABitmap)
  end
  else
  begin
    ABitmap := FStorage.GetThumbnail(APageIndex);
    ADone := False;
    if Assigned(Viewer.OnCustomDrawPreRenderPage) then
    begin
      Viewer.OnCustomDrawPreRenderPage(Viewer, ACanvas, GetPreRenderPageInfo(ABitmap), ADone);
      if not ADone then
        Draw(ABitmap);
    end
    else
      Draw(ABitmap);
  end;
end;

procedure TdxPDFDocumentViewerCustomRenderer.Render(APageIndex: Integer; AFactor: Single);
var
  ABitmap: TBitmap;
  AImage: TdxSmartImage;
begin
  AImage := TdxSmartImage.Create;
  try
    if dxPDFDocumentExportToImageEx(Viewer.Document, APageIndex, AFactor, AImage, Viewer.RotationAngle) then
    begin
      ABitmap := AImage.GetAsBitmap;
      try
        Add(APageIndex, AFactor, ABitmap);
      finally
        ABitmap.Free;
      end;
    end;
  finally
    AImage.Free;
  end;
end;

procedure TdxPDFDocumentViewerCustomRenderer.RenderNextPage;
var
  AFactor: Single;
  APageIndex: Integer;
  ATask: TdxPDFRenderTaskInfo;
begin
  if FQueue.Count > 0 then
  begin
    ATask := FQueue.Dequeue;
    APageIndex := ATask.PageIndex;
    AFactor := ATask.Factor;
    if NeedProcessRenderTask(ATask) then
    begin
      Render(APageIndex, AFactor);
      FNeedPackDocumentContent := True;
      if FQueue.Count = 0 then
        FViewer.FForceUpdate := False;
    end;
    Viewer.Invalidate;
  end
  else
    if FNeedPackDocumentContent then
      FNeedPackDocumentContent := not TryPackDocumentContent;
end;

procedure TdxPDFDocumentViewerCustomRenderer.Add(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);
begin
  FStorage.Add(APageIndex, AFactor, ABitmap);
end;

function TdxPDFDocumentViewerCustomRenderer.GetCacheVisiblePagesOnly: Boolean;
begin
  Result := FStorage.CacheVisiblePagesOnly;
end;

procedure TdxPDFDocumentViewerCustomRenderer.SetCacheVisiblePagesOnly(const AValue: Boolean);
begin
  FStorage.CacheVisiblePagesOnly := AValue;
end;

function TdxPDFDocumentViewerCustomRenderer.CreatePageInfo(APageIndex: Integer): TdxPDFDocumentViewerCachePageInfo;
begin
  Result := TdxPDFDocumentViewerCachePageInfo.Create;
  Result.Index := APageIndex;
  Result.Factor := OnGetRenderFactor(APageIndex);
end;

procedure TdxPDFDocumentViewerCustomRenderer.AddPageIndex(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>;
  APageIndex: Integer);
begin
  if not APages.ContainsKey(APageIndex) then
    APages.Add(APageIndex, CreatePageInfo(APageIndex));
end;

procedure TdxPDFDocumentViewerCustomRenderer.AddPageIndexes(APages: TDictionary<Integer, TdxPDFDocumentViewerCachePageInfo>;
  AStartIndex, AEndIndex: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AEndIndex do
    AddPageIndex(APages, I);
end;

procedure TdxPDFDocumentViewerCustomRenderer.AddPageIndexesToRenderQueue(APages: TList<TdxPDFDocumentViewerCachePageInfo>;
  AForce: Boolean);
var
  I: Integer;
  AComparer: IComparer<TdxPDFDocumentViewerCachePageInfo>;
  APageInfo: TdxPDFDocumentViewerCachePageInfo;
  APageIndex: Integer;
  AFactor: Single;
begin
  FQueue.Clear;
  AComparer := TdxPDFDocumentViewerCachePageInfoComparer.Create;
  APages.Sort(AComparer);
  FStorage.SetVisiblePages(APages);
  for I := 0 to APages.Count - 1 do
  begin
    APageInfo := APages[I];
    APageIndex := APageInfo.Index;
    AFactor := APageInfo.Factor;
    if AForce or not FStorage.Contains(APageIndex, AFactor) then
    begin
      FStorage.Remove(APageIndex, True);
      FQueue.Enqueue(TdxPDFRenderTaskInfo.Create(APageIndex, AFactor));
    end;
  end;
end;

procedure TdxPDFDocumentViewerCustomRenderer.InitializeVisiblePageIndexes;
begin
  FStartVisiblePageIndex := -1;
  FEndVisiblePageIndex := -1;
end;

procedure TdxPDFDocumentViewerCustomRenderer.OnCancelRenderingHandler(Sender: TObject;
  const APageIndexes: TIntegerDynArray);
begin
  CancelRendering(APageIndexes);
end;

procedure TdxPDFDocumentViewerCustomRenderer.OnTimerHandler(Sender: TObject);
begin
  RenderNextPage;
end;

{ TdxPDFDocumentViewerAsyncRenderer }

function TdxPDFDocumentViewerAsyncRenderer.NeedProcessRenderTask(const ATask: TdxPDFRenderTaskInfo): Boolean;
begin
  Result := inherited NeedProcessRenderTask(ATask) or not Storage.CheckThumbnails(ATask.PageIndex, ATask.Factor);
end;

function TdxPDFDocumentViewerAsyncRenderer.TryPackDocumentContent: Boolean;
begin
  Result := TdxPDFBackgroundService(FService).TaskCount = 0;
  if Result then
    Result := inherited TryPackDocumentContent;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.CancelRendering(const APageIndexes: TIntegerDynArray);
var
  I: Integer;
begin
  inherited CancelRendering(APageIndexes);
  for I := 0 to Length(APageIndexes) - 1 do
    TdxPDFBackgroundService(FService).CancelTask(APageIndexes[I]);
end;

procedure TdxPDFDocumentViewerAsyncRenderer.Clear;
begin
  inherited Clear;
  if FService <> nil then
    TdxPDFBackgroundService(FService).CancelAllAndWait;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FService := TdxPDFBackgroundService.Create;
  TdxPDFBackgroundService(FService).OnRenderingComplete := OnCompleteHandler;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.DestroySubClasses;
begin
  FreeAndNil(FService);
  inherited DestroySubClasses;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.RenderNextPage;
begin
  if not FStopped then
    inherited RenderNextPage;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.Render(APageIndex: Integer; AFactor: Single);
begin
  TdxPDFBackgroundService(FService).AddRenderingByScaleFactorTask(Viewer, APageIndex, AFactor);
end;

procedure TdxPDFDocumentViewerAsyncRenderer.OnCompleteHandler(APageIndex: Integer; AFactor: Single; ABitmap: TBitmap);
begin
  Add(APageIndex, AFactor, ABitmap);
  Viewer.InvalidatePage(APageIndex);
end;

procedure TdxPDFDocumentViewerAsyncRenderer.Start;
begin
  FStopped := False;
end;

procedure TdxPDFDocumentViewerAsyncRenderer.Stop;
begin
  FStopped := True;
  TdxPDFBackgroundService(FService).CancelAll;
end;

{ TdxPDFViewerDocumentState }

constructor TdxPDFViewerDocumentState.Create(AOwner: TObject; AViewer: TdxPDFDocumentCustomViewer);
begin
  inherited Create(AOwner);
  FViewer := AViewer;
end;

function TdxPDFViewerDocumentState.ToDocumentRect(APage: TdxPDFDocumentCustomViewerPage; const R: TdxRectF): TdxRectF;
begin
  Result := APage.DocumentPage.FromUserSpace(R, DPI , ScaleFactor, dxRectF(APage.Bounds), Viewer.RotationAngle);
end;

function TdxPDFViewerDocumentState.ToViewerPoint(APage: TdxPDFDocumentCustomViewerPage; const P: TdxPointF): TdxPointF;
begin
  Result := APage.DocumentPage.ToUserSpace(P, DPI, ScaleFactor, dxRectF(APage.Bounds), Viewer.RotationAngle);
end;

function TdxPDFViewerDocumentState.ToViewerRect(const AObject: IdxPDFInteractiveObject): TRect;
var
  APage: TdxPDFDocumentCustomViewerPage;
begin
  Result := cxInvalidRect;
  if AObject <> nil then
  begin
    APage := GetViewerPage(AObject.GetPageIndex);
    if APage <> nil then
      Result := cxRect(APage.ToViewerRect(AObject.GetRect));
  end;
end;

function TdxPDFViewerDocumentState.ToViewerRect(APage: TdxPDFDocumentCustomViewerPage; const R: TdxRectF): TdxRectF;
begin
  Result := APage.DocumentPage.ToUserSpace(R, DPI, ScaleFactor, dxRectF(APage.Bounds), Viewer.RotationAngle);
end;

function TdxPDFViewerDocumentState.ToViewerRect(AHyperlink: TdxPDFHyperlink): TRect;
var
  AIntf: IdxPDFInteractiveObject;
begin
  if (AHyperlink <> nil) and Supports(AHyperlink, IdxPDFInteractiveObject, AIntf) then
    Result := ToViewerRect(AIntf);
end;

procedure TdxPDFViewerDocumentState.CalculateScreenFactors;
begin
  FDocumentToViewerFactor := dxPointF(FDPI.X / 72, FDPI.Y / 72);
end;

procedure TdxPDFViewerDocumentState.Initialize;
begin
  inherited Initialize;
  FDPI := dxPointF(96, 96);
end;

function TdxPDFViewerDocumentState.CreateRenderParameters: TdxPDFRenderParameters;
begin
  Result := TdxPDFRenderParameters.Create(Self);
  Result.Angle := Viewer.RotationAngle;
  Result.ScaleFactor := ScaleFactor.X * DocumentToViewerFactor.X;
end;

function TdxPDFViewerDocumentState.ToDocumentPoint(APage: TdxPDFDocumentCustomViewerPage; const P: TdxPointF): TdxPointF;
begin
  Result := APage.DocumentPage.FromUserSpace(P, DPI , ScaleFactor, dxRectF(APage.Bounds), Viewer.RotationAngle);
end;

function TdxPDFViewerDocumentState.GetScaleFactor: TdxPointF;
begin
  Result.X := Viewer.ScaleFactor.Apply(Viewer.ZoomFactor) / 100;
  Result.Y := Viewer.ScaleFactor.Apply(Viewer.ZoomFactor) / 100;
end;

function TdxPDFViewerDocumentState.GetViewerPage(APageIndex: Integer): TdxPDFDocumentCustomViewerPage;
begin
  if APageIndex > -1 then
    Result := Viewer.PageList[APageIndex] as TdxPDFDocumentCustomViewerPage
  else
    Result := nil;
end;

{ TdxPDFViewerDocument }

constructor TdxPDFViewerDocument.Create(AViewer: TdxPDFDocumentCustomViewer);
begin
  FViewer := AViewer;
  inherited Create;
end;

procedure TdxPDFViewerDocument.CreateDocumentState;
begin
  FState := TdxPDFViewerDocumentState.Create(Self, FViewer);
end;

{ TdxPDFDocumentCustomViewer }

constructor TdxPDFDocumentCustomViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubClasses;
  Initialize;
end;

destructor TdxPDFDocumentCustomViewer.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFDocumentCustomViewer.Calculate(AType: TdxChangeType);
begin
  inherited Calculate(AType);
  DoCalculate;
  UpdateRenderQueue;
end;

procedure TdxPDFDocumentCustomViewer.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  ClearRenderer;
end;

procedure TdxPDFDocumentCustomViewer.CheckMargins;
begin
// do nothing
end;

procedure TdxPDFDocumentCustomViewer.ResyncMargins;
begin
// do nothing
end;

function TdxPDFDocumentCustomViewer.CanUpdateRenderQueue: Boolean;
begin
  Result := not IsUpdateLocked;
end;

function TdxPDFDocumentCustomViewer.CreateRenderer: TdxPDFDocumentViewerCustomRenderer;
begin
  if RenderContentInBackground then
    Result := TdxPDFDocumentViewerAsyncRenderer.Create(Self)
  else
    Result := TDefaultRenderer.Create(Self);
end;

procedure TdxPDFDocumentCustomViewer.CreateSubClasses;
begin
  RecreateRenderer;
  FRenderContentTimer := TcxTimer.Create(Self);
  FRenderContentTimer.OnTimer := OnRenderContentTimerHandler;
end;

procedure TdxPDFDocumentCustomViewer.DestroySubClasses;
begin
  StopZoomTimer;
  FreeAndNil(FRenderContentTimer);
  FreeAndNil(FRenderer);
end;

procedure TdxPDFDocumentCustomViewer.DoCalculate;
begin
// do nothing
end;

procedure TdxPDFDocumentCustomViewer.Initialize;
begin
  RenderContentInBackground := True;
  RenderContentDelay := dxPDFDocumentViewerDefaultRenderContentDelay;
end;

procedure TdxPDFDocumentCustomViewer.RecreateRenderer;
begin
  FreeAndNil(FRenderer);
  FRenderer := CreateRenderer;
  FRenderer.OnGetRenderFactor := OnGetRenderFactorHandler;
end;

procedure TdxPDFDocumentCustomViewer.RestartRenderContentTimer;
begin
  if RenderContentDelay > 0 then
  begin
    StopZoomTimer;
    StartZoomTimer;
  end;
end;

procedure TdxPDFDocumentCustomViewer.UpdateRenderQueue(AForce: Boolean = False);
begin
  if CanUpdateRenderQueue then
    FRenderer.UpdateRenderQueue(AForce);
end;

function TdxPDFDocumentCustomViewer.GetNearestPageIndex(const P: TPoint): Integer;
var
  I: Integer;
  AMinDistance, ADistance: Single;
  R: TdxRectF;
begin
  Result := -1;
  AMinDistance := MaxSingle;
  for I := 0 to PageCount - 1 do
  begin
    R := dxRectF(Pages[I].Bounds);
    ADistance := TdxPDFUtils.DistanceToRect(P, dxRectF(R.Left, R.Top, R.Right, R.Bottom));
    if ADistance < AMinDistance then
    begin
      AMinDistance := ADistance;
      Result := I;
    end;
  end;
end;

function TdxPDFDocumentCustomViewer.IsPageVisible(AIndex: Integer): Boolean;
var
  APage: TdxPDFViewerPage;
begin
  APage := TdxPDFViewerPage(Pages[AIndex]);
  Result := cxRectIntersect(ClientBounds, APage.Bounds);
end;

procedure TdxPDFDocumentCustomViewer.ClearRenderer;
begin
  FRenderer.Clear;
end;

function TdxPDFDocumentCustomViewer.GetDocumentPages: TdxPDFPages;
begin
  Result := TdxPDFDocumentAccess(FDocument).Pages;
end;

function TdxPDFDocumentCustomViewer.GetDocumentState: TdxPDFViewerDocumentState;
begin
  Result := TdxPDFDocumentAccess(FDocument).State as TdxPDFViewerDocumentState;
end;

function TdxPDFDocumentCustomViewer.GetIsZooming: Boolean;
begin
  Result := FIsZooming and (FRenderContentDelay > 0);
end;

function TdxPDFDocumentCustomViewer.GetRotationAngle: TcxRotationAngle;
begin
  Result := TdxPDFDocumentAccess(Document).State.RotationAngle;
end;

procedure TdxPDFDocumentCustomViewer.SetRenderContentDelay(const AValue: Integer);
begin
  if FRenderContentDelay <> AValue then
  begin
    FRenderContentDelay := Min(Max(AValue, 50), 1000);
    FRenderContentTimer.Interval := RenderContentDelay;
  end;
end;

procedure TdxPDFDocumentCustomViewer.SetRenderContentInBackground(const AValue: Boolean);
begin
  if dxCanUseMultiThreading and (FRenderContentInBackground <> AValue) then
  begin
    FRenderContentInBackground := AValue;
    RecreateRenderer;
    DoZoomFactorChanged;
  end;
end;

procedure TdxPDFDocumentCustomViewer.StartZoomTimer;
begin
  Renderer.Stop;
  FIsZooming := True;
  FRenderContentTimer.Enabled := False;
  FRenderContentTimer.Enabled := True;
end;

procedure TdxPDFDocumentCustomViewer.StopZoomTimer;
begin
  FRenderContentTimer.Enabled := False;
  InvalidatePages;
  if Renderer <> nil then
    Renderer.Start;
  FIsZooming := False;
end;

procedure TdxPDFDocumentCustomViewer.Changed(Sender: TObject);
begin
  OnInteractiveFormFieldValueChangedHandler(Sender);
end;

function TdxPDFDocumentCustomViewer.OnGetRenderFactorHandler(APageIndex: Integer): Single;
begin
  Result := GetPageRenderFactor(APageIndex);
end;

procedure TdxPDFDocumentCustomViewer.OnInteractiveFormFieldValueChangedHandler(Sender: TObject);
begin
  FForceUpdate := True;
  UpdateRenderQueue(FForceUpdate);
end;

procedure TdxPDFDocumentCustomViewer.OnRenderContentTimerHandler(Sender: TObject);
begin
  StopZoomTimer;
end;

{ TdxPDFDocumentCustomViewer.TDefaultRenderer }

destructor TdxPDFDocumentCustomViewer.TDefaultRenderer.Destroy;
begin
  ClearViewerCache;
  inherited Destroy;
end;

procedure TdxPDFDocumentCustomViewer.TDefaultRenderer.Start;
begin
  ClearViewerCache;
end;

procedure TdxPDFDocumentCustomViewer.TDefaultRenderer.DoDrawPage(ACanvas: TcxCanvas; APageIndex: Integer; const ARect: TRect);
var
  ACache: TdxPreviewPageContentCache;
  ADevice: TdxPDFGraphicsDevice;
  APage: TdxPDFDocumentCustomViewerPage;
  AParameters: TdxPDFRenderParameters;
  AViewer: TdxCustomPreviewAccess;
begin
  if not Viewer.IsZooming then
  begin
    AViewer := TdxCustomPreviewAccess(Viewer);
    APage := AViewer.Pages[APageIndex] as TdxPDFDocumentCustomViewerPage;
    ACache := AViewer.PagesContentCache.Add(APage);
    if ACache.Dirty then
    begin
      ACache.Canvas.Lock;
      try
        ACache.Dirty := False;
        ACache.cxCanvas.WindowOrg := APage.Bounds.TopLeft;
        AViewer.DrawPageBackground(ACache.cxCanvas, APage, APage.Selected);
        ACache.cxCanvas.WindowOrg := cxNullPoint;
        AParameters := Viewer.DocumentState.CreateRenderParameters;
        AParameters.Canvas := ACache.cxCanvas.Canvas;
        AParameters.Rect := ACache.ClientRect;
        ADevice := TdxPDFGraphicsDevice.Create;
        try
          ADevice.Export(APage.DocumentPage, AParameters);
          APage.DocumentPage.Pack;
        finally
          ADevice.Free;
          AParameters.Free;
        end;
      finally
        ACache.Canvas.Unlock;
      end;
      Add(APageIndex, Viewer.DocumentState.ScaleFactor.X, ACache);
    end;
    cxBitBlt(ACanvas.Handle, ACache.Canvas.Handle, APage.Bounds, cxNullPoint, SRCCOPY);
  end
  else
    inherited DoDrawPage(ACanvas, APageIndex, ARect);
end;

procedure TdxPDFDocumentCustomViewer.TDefaultRenderer.Render(APageIndex: Integer; AFactor: Single);
begin
  Viewer.InvalidatePage(APageIndex);
end;

procedure TdxPDFDocumentCustomViewer.TDefaultRenderer.ClearViewerCache;
begin
  TdxPreviewPageContentCachePoolAccess(TdxCustomPreviewAccess(Viewer).PagesContentCache).Clear;
end;

{ TdxPDFDocumentPageThumbnailViewer }

function TdxPDFDocumentPageThumbnailViewer.GetPagesToPrint: TIntegerDynArray;
var
  I: Integer;
begin
  FSelectedPages.Sort;
  SetLength(Result, FSelectedPages.Count);
  for I := 0 to FSelectedPages.Count - 1 do
    Result[I] := FSelectedPages[I] + 1;
end;

function TdxPDFDocumentPageThumbnailViewer.CreateRenderer: TdxPDFDocumentViewerCustomRenderer;
begin
  Result := TRenderer.Create(Self);
end;

function TdxPDFDocumentPageThumbnailViewer.GetPageRenderFactor(APageIndex: Integer): Single;
begin
  Result := (PageList[APageIndex] as TPage).ImageSize;
end;

procedure TdxPDFDocumentPageThumbnailViewer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FSelectedPages := TList<Integer>.Create;
end;

procedure TdxPDFDocumentPageThumbnailViewer.DestroySubClasses;
begin
  FreeAndNil(FPopupMenu);
  FreeAndNil(FSelectedPages);
  inherited DestroySubClasses;
end;

procedure TdxPDFDocumentPageThumbnailViewer.DoCalculate;
var
  I: Integer;
begin
  inherited DoCalculate;
  for I := 0 to PageCount - 1 do
    TPage(PageList[I]).CalculateLayout;
end;

procedure TdxPDFDocumentPageThumbnailViewer.Initialize;
begin
  inherited Initialize;
  RenderContentDelay := 250;
  InternalOptionsBehavior := [pobThumbTracking];
  InternalOptionsView := [povAutoHideScrollBars, povDefaultDrawPageBackground];
  InternalOptionsZoom := [];
  OptionsHint := [];
  Keys := [kArrows, kChars];
  FMaxSize := dxPDFDocumentPageThumbnailViewerMaxSize;
  FMinSize := dxPDFDocumentPageThumbnailViewerMinSize;
  FSize := MinSize;
end;

procedure TdxPDFDocumentPageThumbnailViewer.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  ClearRenderer;
  LayoutChanged(ctHard);
end;

function TdxPDFDocumentPageThumbnailViewer.CreatePage: TdxPreviewPage;
var
  ADocument: TdxPDFDocumentAccess;
  APage: TPage;
begin
  ADocument := TdxPDFDocumentAccess(FDocument);
  if (ADocument <> nil) and (ADocument.Pages <> nil) then
  begin
    Result := inherited CreatePage;
    APage := Result as TPage;
    if ADocument.IsLoaded then
      APage.DocumentPage := ADocument.Pages[PageCount - 1];
  end
  else
    Result := nil;
end;

function TdxPDFDocumentPageThumbnailViewer.GetAbsoluteIndentLeft: Integer;
begin
  Result := 0;
end;

function TdxPDFDocumentPageThumbnailViewer.GetAbsoluteIndentRight: Integer;
begin
  Result := 0;
end;

function TdxPDFDocumentPageThumbnailViewer.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPDFDocumentPageThumbnailViewer.GetDefaultZoomStep: Integer;
begin
  Result := 0;
end;

function TdxPDFDocumentPageThumbnailViewer.GetPageCaptionAreaHeight: Integer;
begin
  Result := PageCaptionSize.cy + PageCaptionMargin;
end;

function TdxPDFDocumentPageThumbnailViewer.GetPageCaptionMargin: Integer;
begin
  Result := ScaleFactor.Apply(10);
end;

function TdxPDFDocumentPageThumbnailViewer.GetPageClass: TdxPreviewPageClass;
begin
  Result := TPage;
end;

function TdxPDFDocumentPageThumbnailViewer.NonVerticalCenterizePages: Boolean;
begin
  Result := True;
end;

procedure TdxPDFDocumentPageThumbnailViewer.Calculate(AType: TdxChangeType);
begin
  CalculateCaptionSize;
  inherited Calculate(AType);
end;

procedure TdxPDFDocumentPageThumbnailViewer.DoContextPopup(MousePos: TPoint; var Handled: Boolean);

  function ContextPopup(const P: TPoint): Boolean;
  begin
    if FPopupMenu = nil then
    begin
      FreeAndNil(FPopupMenu);
      FPopupMenu := TdxPDFViewerThumbnailsPopupMenu.Create(Owner);
    end;
    Result := (FPopupMenu as TdxPDFViewerCustomPopupMenu).Popup(P);
  end;

begin
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
    Handled := ContextPopup(ClientToScreen(MousePos));
end;

procedure TdxPDFDocumentPageThumbnailViewer.DoSelectedPageChanged;
begin
  inherited DoSelectedPageChanged;
  FCurrentFocusedPageIndex := Max(SelPageIndex, 0);
end;

procedure TdxPDFDocumentPageThumbnailViewer.DoZoomIn;
begin
  Size := Size + dxPDFDocumentPageThumbnailViewerSizeStep;
end;

procedure TdxPDFDocumentPageThumbnailViewer.DoZoomOut;
begin
  Size := Size - dxPDFDocumentPageThumbnailViewerSizeStep;
end;

procedure TdxPDFDocumentPageThumbnailViewer.DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean);
begin
// do nothing
end;

procedure TdxPDFDocumentPageThumbnailViewer.DrawViewerBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.PDFViewerDrawPageThumbnailPreviewBackground(ACanvas, R);
end;

procedure TdxPDFDocumentPageThumbnailViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  if not (Key in [VK_RETURN, VK_ADD, VK_SUBTRACT]) then
  begin
    inherited KeyDown(Key, Shift);
    case Key of
      VK_ESCAPE:
        FSelectedPages.Clear;
      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END:
        if ssShift in Shift then
          UpdateSelectionRange;
      Ord('A'):
        if IsCtrlPressed then
        begin
          FSelectedPages.Clear;
          for I := 0 to PageCount - 1 do
            FSelectedPages.Add(I);
        end;
    end;
    Invalidate;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APageIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
  begin
    APageIndex := GetNearestPageIndex(cxPoint(X, Y));
    APageIndex := IfThen(APageIndex = -1, 0, APageIndex);
    if not FSelectedPages.Contains(APageIndex) then
    begin
      FSelectedPages.Clear;
      FSelectedPages.Add(APageIndex);
      Invalidate;
    end;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer);
var
  APageIndex: Integer;
  AIsMouseLeftButtonPressed: Boolean;
  P: TPoint;
begin
  P := cxPoint(X, Y);
  AIsMouseLeftButtonPressed := True;
  APageIndex := PageList.PageIndexFromPoint(X, Y);
  if APageIndex >= 0 then
  begin
    if ssCtrl in Shift then
    begin
      if FSelectedPages.Contains(APageIndex) then
        FSelectedPages.Remove(APageIndex)
      else
        FSelectedPages.Add(APageIndex);
      FLastClickedPageIndex := APageIndex;
      SelPageIndex := APageIndex;
    end
    else
      if ssShift in Shift then
      begin
        FCurrentFocusedPageIndex := APageIndex;
        UpdateSelectionRange;
        FLastClickedPageIndex := APageIndex;
        SelPageIndex := APageIndex;
      end
      else
      begin
        FLastClickedPageIndex := APageIndex;
        SelPageIndex := APageIndex;
        FSelectedPages.Clear;
        FSelectedPages.Add(APageIndex);
      end;
    AIsMouseLeftButtonPressed := False;
  end;
  if AIsMouseLeftButtonPressed then
    if not (ssShift in Shift) then
      FSelectedPages.Clear;
  Invalidate;
end;

function TdxPDFDocumentPageThumbnailViewer.IsPageSelected(APageIndex: Integer): Boolean;
begin
  Result := FSelectedPages.Contains(APageIndex);
end;

procedure TdxPDFDocumentPageThumbnailViewer.Clear;
begin
  Document := nil;
end;

procedure TdxPDFDocumentPageThumbnailViewer.SetDocument(const AValue: TdxPDFViewerDocument);
begin
  if FDocument <> AValue then
  begin
    Renderer.Clear;
    Size := 0;
    if Document <> nil then
      TdxPDFDocumentAccess(Document).RemoveListener(Self);
    FDocument := AValue;
    if Document <> nil then
    begin
      TdxPDFDocumentAccess(Document).State.OnRotationAngleChanged := OnRotationAngleChangedHandler;
      TdxPDFDocumentAccess(Document).AddListener(Self);
    end;
    RecreatePages;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.SetMaxSize(const AValue: Integer);
begin
  if FMaxSize <> AValue then
  begin
    FMaxSize := AValue;
    CheckSize;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.SetMinSize(const AValue: Integer);
begin
  if FMinSize <> AValue then
  begin
    FMinSize := AValue;
    CheckSize;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.SetSize(const AValue: Integer);
var
  I, ACurrentPageIndex, AActualValue: Integer;
begin
  AActualValue := Min(Max(AValue, MinSize), MaxSize);
  if FSize <> AActualValue then
  begin
    FSize := AActualValue;
    ACurrentPageIndex := SelPageIndex;
    BeginUpdate;
    try
      if Document <> nil then
        for I := 0 to Document.PageCount - 1 do
          TdxPDFDocumentCustomViewerPage(PageList[I]).CalculatePageSize;
      dxCallNotify(OnThumbnailSizeChanged, Self);
    finally
      EndUpdate;
    end;
    MakeVisible(ACurrentPageIndex);
    RestartRenderContentTimer;
    UpdateRenderQueue(True);
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.CalculateCaptionSize;
begin
  if Document <> nil then
    FPageCaptionSize := cxTextSize(Font, IntToStr(Document.PageCount + 1))
  else
    FPageCaptionSize := cxNullSize;
end;

procedure TdxPDFDocumentPageThumbnailViewer.Changed(Sender: TObject; AFont: TFont);
begin
  Font.Assign(AFont);
  LayoutChanged(ctLight);
end;

procedure TdxPDFDocumentPageThumbnailViewer.CheckSize;
begin
  Size := TdxPDFUtils.MinMax(Size, FMinSize, FMaxSize);
end;

procedure TdxPDFDocumentPageThumbnailViewer.RecreatePages(AForce: Boolean = True);
var
  I: Integer;
begin
  BeginUpdate;
  try
    if AForce then
    begin
      Renderer.Clear;
      FSelectedPages.Clear;
    end;
    PageCount := 0;
    if (Document <> nil) and TdxPDFDocumentAccess(Document).IsLoaded then
    begin
      CalculateCaptionSize;
      for I := 0 to Document.PageCount - 1 do
        CreatePage;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.UpdateSelectionRange;
var
  I, AMaxIndex: Integer;
begin
  FSelectedPages.Clear;
  AMaxIndex := Max(FCurrentFocusedPageIndex, FLastClickedPageIndex);
  for I := Min(FCurrentFocusedPageIndex, FLastClickedPageIndex) to AMaxIndex do
    FSelectedPages.Add(I);
end;

procedure TdxPDFDocumentPageThumbnailViewer.OnRotationAngleChangedHandler(Sender: TObject);
begin
  RecreatePages;
end;

{ TdxPDFDocumentPageThumbnailViewer.TPage }

procedure TdxPDFDocumentPageThumbnailViewer.TPage.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.Lock;
  try
    DrawSelection(ACanvas);
    DrawFocusRect(ACanvas);
    DrawBackground(ACanvas);
    DrawContent(ACanvas);
    DrawIndex(ACanvas);
  finally
    ACanvas.Unlock;
  end;
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.CalculatePageSize;
var
  ASize: TPoint;
begin
  ASize := GetPageSize(dxPointF(1, 1));
  PageSize.Assigned := not PageSize.Assigned;
  PageSize.Assigned := True;
  PageSize.MinUsefulSize := cxPoint(ThumbnailsPreview.Size, ThumbnailsPreview.Size);
  PageSize.Size := PageSize.MinUsefulSize;
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.CalculateLayout;

  function GetImageMargins: TSize;
  begin
    Result.cx := Bounds.Width - ThumbnailsPreview.ScaleFactor.Apply(DefaultIndent * 2) -
      Max(ThumbnailsPreview.PageBorders.Left, ThumbnailsPreview.PageBorders.Top);
    Result.cy := Bounds.Height - ThumbnailsPreview.ScaleFactor.Apply(DefaultIndent * 2) -
      ThumbnailsPreview.PageCaptionAreaHeight - Max(ThumbnailsPreview.PageBorders.Right, ThumbnailsPreview.PageBorders.Bottom);
  end;

  function GetRealPageSize: TPoint;
  var
    AMaximumSize: TSize;
    APageSize: TPoint;
    AScale: Single;
  begin
    AMaximumSize := GetImageMargins;
    APageSize := GetPageSize(dxPointF(1, 1));
    AScale := Min(AMaximumSize.cx / Max(1, APageSize.X), AMaximumSize.cy / Max(1, APageSize.Y));
    Result := GetPageSize(dxPointF(AScale, AScale));
  end;

var
  APageSize: TPoint;
  ASelectionSize: TRect;
begin
  FThumbnailBounds := Bounds;

  APageSize := GetRealPageSize;
  FImageBounds.Left := Bounds.Left + (Bounds.Width - APageSize.X) div 2;
  FImageBounds.Top := Bounds.Top + (Bounds.Height - APageSize.Y - ThumbnailsPreview.PageCaptionSize.cy) div 2;
  FImageBounds.Width := APageSize.X;
  FImageBounds.Height := APageSize.Y;

  FImageBorderBounds := cxRectInflate(FImageBounds, ThumbnailsPreview.PageBorders);

  ASelectionSize := GetSelectionSize;
  FSelectionBounds := cxRectInflate(FImageBounds, ASelectionSize);

  FCaptionBounds.Left := Bounds.Left + (Bounds.Width - ThumbnailsPreview.PageCaptionSize.cx) div 2;
  FCaptionBounds.Top := FSelectionBounds.Bottom + ThumbnailsPreview.PageCaptionMargin div 2;
  FCaptionBounds.Width := ThumbnailsPreview.PageCaptionSize.cx;
  FCaptionBounds.Height := ThumbnailsPreview.PageCaptionSize.cy;
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.DrawBackground(ACanvas: TcxCanvas);
begin
  ThumbnailsPreview.LookAndFeel.Painter.DrawPrintPreviewPageBackground(ACanvas, FImageBorderBounds, FImageBounds,
    False, False);
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.DrawContent(ACanvas: TcxCanvas);
begin
  ThumbnailsPreview.Renderer.DrawPage(ACanvas, Index, FImageBounds);
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.DrawFocusRect(ACanvas: TcxCanvas);
begin
  if Selected then
    ACanvas.DrawFocusRect(FSelectionBounds);
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.DrawSelection(ACanvas: TcxCanvas);
begin
  if ThumbnailsPreview.IsPageSelected(Index) then
    dxGpFillRect(ACanvas.Handle, FSelectionBounds, ThumbnailsPreview.LookAndFeel.Painter.DefaultSelectionColor, 200);
end;

procedure TdxPDFDocumentPageThumbnailViewer.TPage.DrawIndex(ACanvas: TcxCanvas);
begin
  ACanvas.Font.Assign(ThumbnailsPreview.Font);
  ACanvas.Font.Color := cxGetActualColor(ThumbnailsPreview.LookAndFeelPainter.GetWindowContentTextColor, clWindowText);
  cxDrawText(ACanvas, IntToStr(Index + 1), FCaptionBounds, DT_VCENTER or DT_SINGLELINE or DT_CENTER);
end;

function TdxPDFDocumentPageThumbnailViewer.TPage.GetImageSize: Integer;
begin
  if ThumbnailsPreview.RotationAngle in [ra0, ra180] then
    Result := FImageBounds.Width
  else
    Result := FImageBounds.Height;
end;

function TdxPDFDocumentPageThumbnailViewer.TPage.GetSelectionSize: TRect;
begin
  Result := ThumbnailsPreview.ScaleFactor.Apply(cxRect(DefaultIndent, DefaultIndent, DefaultIndent, DefaultIndent));
end;

function TdxPDFDocumentPageThumbnailViewer.TPage.GetThumbnailsPreview: TdxPDFDocumentPageThumbnailViewer;
begin
  Result := Preview as TdxPDFDocumentPageThumbnailViewer;
end;

{ TdxPDFDocumentPageThumbnailViewer.TRenderer }

function TdxPDFDocumentPageThumbnailViewer.TRenderer.GetPreRenderPageScale(APageIndex: Integer): Single;
begin
  Result := Viewer.GetPageRenderFactor(APageIndex) / Viewer.Document.PageInfo[APageIndex].Size.X;
end;

function TdxPDFDocumentPageThumbnailViewer.TRenderer.GetRenderingPageRowDelta: Integer;
begin
  Result := 0;
end;

procedure TdxPDFDocumentPageThumbnailViewer.TRenderer.CreateSubClasses;
begin
  inherited CreateSubClasses;
  CacheVisiblePagesOnly := True;
end;

procedure TdxPDFDocumentPageThumbnailViewer.TRenderer.Render(APageIndex: Integer; AFactor: Single);
begin
  TdxPDFBackgroundService(FService).AddRenderingBySizeTask(Viewer, APageIndex, AFactor);
end;

{ TdxPDFViewerPageSizeOptions }

function TdxPDFViewerPageSizeOptions.GetActualSizeInPixels: TPoint;
begin
  Result := Size;
end;

function TdxPDFViewerPageSizeOptions.GetDefaultMinUsefulSize: TPoint;
begin
  Result := cxPoint(2, 2);
end;

{ TdxPDFDocumentCustomViewerPage }

function TdxPDFDocumentCustomViewerPage.GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass;
begin
  Result := TdxPDFViewerPageSizeOptions;
end;

function TdxPDFDocumentCustomViewerPage.GetPageSize(const AScreenFactor: TdxPointF): TPoint;
var
  ASize: TdxPointF;
begin
  ASize := TdxPDFViewerViewState.CalculatePageSize(DocumentPage.Size, TdxPDFDocumentCustomViewer(Preview).RotationAngle);
  Result.X := Round(ASize.X * AScreenFactor.X);
  Result.Y := Round(ASize.Y * AScreenFactor.Y);
end;

function TdxPDFDocumentCustomViewerPage.ToDocumentPoint(const P: TdxPointF): TdxPointF;
begin
  Result := DocumentState.ToDocumentPoint(Self, P);
end;

function TdxPDFDocumentCustomViewerPage.ToDocumentRect(const R: TdxRectF): TdxRectF;
begin
  Result := DocumentState.ToDocumentRect(Self, R);
end;

function TdxPDFDocumentCustomViewerPage.ToViewerPoint(const P: TdxPointF): TdxPointF;
begin
  Result := DocumentState.ToViewerPoint(Self, P);
end;

function TdxPDFDocumentCustomViewerPage.ToViewerRect(const R: TdxRectF): TdxRectF;
begin
  Result := DocumentState.ToViewerRect(Self, R);
end;

function TdxPDFDocumentCustomViewerPage.GetDocumentState: TdxPDFViewerDocumentState;
begin
  Result := TdxPDFDocumentCustomViewer(Preview).DocumentState;
end;

procedure TdxPDFDocumentCustomViewerPage.SetDocumentPage(const AValue: TdxPDFPage);
begin
  if DocumentPage <> AValue then
  begin
    FDocumentPage := AValue;
    CalculatePageSize;
  end;
end;

end.
