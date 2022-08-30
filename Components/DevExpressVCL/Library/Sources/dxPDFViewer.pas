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

unit dxPDFViewer;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Buttons, Messages, Forms, Controls, ComCtrls, StdCtrls, Graphics, Generics.Defaults,
  Generics.Collections, dxCore, dxCoreClasses, cxClasses, dxCoreGraphics, cxGraphics, cxGeometry, cxListView,
  dxGDIPlusClasses, dxThreading, dxProtectionUtils, dxAnimation, dxFading, cxLookAndFeelPainters, cxLookAndFeels,
  cxControls, cxTreeView, dxImCtrl, dxCustomHint, dxCustomPreview, cxTextEdit, cxTrackBar, dxPDFCore, dxPDFDocument,
  dxPDFText, dxPDFCommandInterpreter, dxPDFFontUtils, dxPDFTypes, dxPDFSelection, dxPDFDocumentViewer,
  dxPDFInteractivity, dxPDFRecognizedObject;

const
  dxPDFViewerDefaultScrollStep: Integer = 30;
  dxPDFViewerDefaultSize: TSize = (cx: 200; cy: 150);
  dxPDFViewerDefaultZoomFactor = 100;
  dxPDFViewerFindPanelDefaultAnimationTime = 200;
  dxPDFViewerMaxZoomFactor = 500;
  dxPDFViewerMinZoomFactor = 10;
  dxPDFViewerZoomStep = 10;
  // HitTests bits
  hcPageArea = 16;
  hcBackground = 32;
  hcSelectionFrame = 4096;
  hcFindPanel = 32768;
  hcNavigationPaneSplitter = 65536;

type
  TdxPDFCustomViewer = class;
  TdxPDFViewerAttachments = class;
  TdxPDFViewerBookmarks = class;
  TdxPDFViewerButtonFadingHelper = class;
  TdxPDFViewerCellHitTest = class;
  TdxPDFViewerCellViewInfo = class;
  TdxPDFViewerContainerController = class;
  TdxPDFViewerContainerViewInfo = class;
  TdxPDFViewerCustomController = class;
  TdxPDFViewerCustomHitTest = class;
  TdxPDFViewerController = class;
  TdxPDFViewerControllerClass = class of TdxPDFViewerController;
  TdxPDFViewerDocumentHitTest = class;
  TdxPDFViewerInteractivityController = class;
  TdxPDFViewerHighlights = class;
  TdxPDFViewerFindPanel = class;
  TdxPDFViewerFindPanelAnimationController = class;
  TdxPDFViewerFindPanelViewInfo = class;
  TdxPDFViewerNavigationPane = class;
  TdxPDFViewerNavigationPaneController = class;
  TdxPDFViewerNavigationPanePage = class;
  TdxPDFViewerNavigationPanePageViewInfo = class;
  TdxPDFViewerNavigationPaneViewInfo = class;
  TdxPDFViewerOptionsNavigationPane = class;
  TdxPDFViewerPageThumbnailPreview = class;
  TdxPDFViewerPainter = class;
  TdxPDFViewerSelection = class;
  TdxPDFViewerSelectionController = class;
  TdxPDFViewerTextSearch = class;
  TdxPDFViewerThumbnails = class;
  TdxPDFViewerViewInfo = class;
  TdxPDFViewerViewInfoClass = class of TdxPDFViewerViewInfo;
  TdxPDFViewerViewStateHistory = class;
  TdxPDFViewerViewStateHistoryController = class;

  TdxPDFViewerGetFindPanelVisibilityEvent = function(Sender: TdxPDFCustomViewer): Boolean of object;
  TdxPDFViewerOnAttachmentActionEvent = procedure(Sender: TdxPDFCustomViewer; AAttachment: TdxPDFFileAttachment; var AHandled: Boolean) of object;
  TdxPDFViewerOnHyperlinkClickEvent = procedure(Sender: TdxPDFCustomViewer; const AURI: string; var AHandled: Boolean) of object;
  TdxPDFViewerPrepareLockedStateImageEvent = procedure(Sender: TdxPDFCustomViewer; AImage: TcxBitmap32;
    var ADone: Boolean) of object;

  TdxPDFViewerNavigationPaneActivePage = (apNone, apThumbnails, apBookmarks, apAttachments);
  TdxPDFViewerBookmarksTextSize = (btsSmall, btsMedium, btsLarge);
  TdxPDFViewerFindPanelAnimation = (fpaSlide, fpaFade);
  TdxPDFViewerFindPanelDisplayMode = (fpdmManual, fpdmAlways, fpdmNever);
  TdxPDFViewerFindPanelAlignment = (fpalTopClient, fpalTopRight, fpalTopCenter, fpalTopLeft, fpalBottomClient, fpalBottomRight,
    fpalBottomCenter, fpalBottomLeft);
  TdxPDFViewerViewStateChangeType = (vsctNone, vsctRotation, vsctZooming, vsctSelecting, vsctScrolling);

  { TdxPDFViewerOptionsPersistent }

  TdxPDFViewerOptionsPersistent = class(TcxOwnedPersistent)
  strict private
    function GetViewer: TdxPDFCustomViewer;
  protected
    procedure Initialize; virtual;

    procedure Changed; overload;
    procedure Changed(AType: TdxChangeType); overload;

    property Viewer: TdxPDFCustomViewer read GetViewer;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxPDFViewerOptionsBehavior }

  TdxPDFViewerOptionsBehavior = class(TdxPDFViewerOptionsPersistent)
  strict private
    FShowHints: Boolean;
    function GetRenderContentDelay: Integer;
    function GetRenderContentInBackground: Boolean;
    procedure SetRenderContentDelay(const AValue: Integer);
    procedure SetRenderContentInBackground(const AValue: Boolean);
    procedure SetShowHints(const AValue: Boolean);
  protected
    procedure DoAssign(ASource: TPersistent); override;
    property RenderContentInBackground: Boolean read GetRenderContentInBackground write
      SetRenderContentInBackground default False;
  published
    constructor Create(AOwner: TPersistent); override;
    property RenderContentDelay: Integer read GetRenderContentDelay write SetRenderContentDelay
      default dxPDFDocumentViewerDefaultRenderContentDelay;
    property ShowHints: Boolean read FShowHints write SetShowHints default True;
  end;

  { TdxPDFViewerOptionsZoom }

  TdxPDFViewerOptionsZoom = class(TdxPDFViewerOptionsPersistent)
  strict private
    function GetMaxZoomFactor: Integer;
    function GetMinZoomFactor: Integer;
    function GetZoomFactor: Integer;
    function GetZoomMode: TdxPreviewZoomMode;
    function GetZoomStep: Integer;
    procedure SetMaxZoomFactor(const AValue: Integer);
    procedure SetMinZoomFactor(const AValue: Integer);
    procedure SetZoomFactor(const AValue: Integer);
    procedure SetZoomMode(const AValue: TdxPreviewZoomMode);
    procedure SetZoomStep(const AValue: Integer);
  protected
    procedure DoAssign(ASource: TPersistent); override;
  published
    property MaxZoomFactor: Integer read GetMaxZoomFactor write SetMaxZoomFactor default dxPDFViewerMaxZoomFactor;
    property MinZoomFactor: Integer read GetMinZoomFactor write SetMinZoomFactor default dxPDFViewerMinZoomFactor;
    property ZoomFactor: Integer read GetZoomFactor write SetZoomFactor default dxPDFViewerDefaultZoomFactor;
    property ZoomMode: TdxPreviewZoomMode read GetZoomMode write SetZoomMode default pzmNone;
    property ZoomStep: Integer read GetZoomStep write SetZoomStep default dxPDFViewerZoomStep;
  end;

  { TdxPDFViewerFindPanelInnerTextEdit }

  TdxPDFViewerFindPanelInnerTextEdit = class(TcxCustomInnerTextEdit)
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

  { TdxPDFViewerFindPanelTextEdit }

  TdxPDFViewerFindPanelTextEdit = class(TcxCustomTextEdit)
  strict private
    FFindPanel: TdxPDFViewerFindPanel;
  protected
    function GetInnerEditClass: TControlClass; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure FocusChanged; override;
  public
    constructor Create(AFindPanel: TdxPDFViewerFindPanel); reintroduce;
  end;

  { TdxPDFViewerCustomObject }

  TdxPDFViewerCustomObject = class
  strict private
    FViewer: TdxPDFCustomViewer;

    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TdxPDFViewerViewInfo;
  protected
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Viewer: TdxPDFCustomViewer read FViewer;
    property ViewInfo: TdxPDFViewerViewInfo read GetViewInfo;
  public
    constructor Create(AViewer: TdxPDFCustomViewer); virtual;
    destructor Destroy; override;
  end;

  { TdxPDFViewerOptionsFindPanel }

  TdxPDFViewerOptionsFindPanel = class(TdxPDFViewerOptionsPersistent)
  strict private
    FAlignment: TdxPDFViewerFindPanelAlignment;
    FAnimation: TdxPDFViewerFindPanelAnimation;
    FAnimationTime: Integer;
    FClearSearchStringOnClose: Boolean;
    FDisplayMode: TdxPDFViewerFindPanelDisplayMode;
    FHighlightSearchResults: Boolean;
    FOptions: TdxPDFDocumentTextSearchOptions;
    FSearchString: string;
    FShowCloseButton: Boolean;
    FShowNextButton: Boolean;
    FShowOptionsButton: Boolean;
    FShowPreviousButton: Boolean;

    function GetCaseSensitive: Boolean;
    function GetDirection: TdxPDFDocumentTextSearchDirection;
    function GetWholeWords: Boolean;
    procedure SetAlignment(const AValue: TdxPDFViewerFindPanelAlignment);
    procedure SetAnimation(const AValue: TdxPDFViewerFindPanelAnimation);
    procedure SetAnimationTime(const AValue: Integer);
    procedure SetCaseSensitive(const AValue: Boolean);
    procedure SetClearSearchStringOnClose(const AValue: Boolean);
    procedure SetDirection(const AValue: TdxPDFDocumentTextSearchDirection);
    procedure SetDisplayMode(const AValue: TdxPDFViewerFindPanelDisplayMode);
    procedure SetHighlightSearchResults(const AValue: Boolean);
    procedure SetSearchString(const AValue: string);
    procedure SetShowCloseButton(const AValue: Boolean);
    procedure SetShowNextButton(const AValue: Boolean);
    procedure SetShowOptionsButton(const AValue: Boolean);
    procedure SetShowPreviousButton(const AValue: Boolean);
    procedure SetWholeWords(const AValue: Boolean);

    procedure ClearTextSearch;
  protected
    procedure DoAssign(ASource: TPersistent); override;
    procedure Initialize; override;

    property Direction: TdxPDFDocumentTextSearchDirection read GetDirection write SetDirection;
    property ShowCloseButton: Boolean read FShowCloseButton write SetShowCloseButton;
    property ShowNextButton: Boolean read FShowNextButton write SetShowNextButton;
    property ShowOptionsButton: Boolean read FShowOptionsButton write SetShowOptionsButton;
    property ShowPreviousButton: Boolean read FShowPreviousButton write SetShowPreviousButton;
  public
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property HighlightSearchResults: Boolean read FHighlightSearchResults write SetHighlightSearchResults;
    property SearchString: string read FSearchString write SetSearchString;
    property WholeWords: Boolean read GetWholeWords write SetWholeWords;
  published
    property Alignment: TdxPDFViewerFindPanelAlignment read FAlignment write SetAlignment default fpalTopClient;
    property Animation: TdxPDFViewerFindPanelAnimation read FAnimation write SetAnimation default fpaSlide;
    property AnimationTime: Integer read FAnimationTime write SetAnimationTime default dxPDFViewerFindPanelDefaultAnimationTime;
    property ClearSearchStringOnClose: Boolean read FClearSearchStringOnClose write SetClearSearchStringOnClose default False;
    property DisplayMode: TdxPDFViewerFindPanelDisplayMode read FDisplayMode write SetDisplayMode default fpdmManual;
  end;

  { TdxPDFViewerFindPanel }

  TdxPDFViewerFindPanel = class(TdxPDFViewerCustomObject)
  strict private
    FOptionsButtonGlyph: TdxSmartGlyph;
    FEdit: TdxPDFViewerFindPanelTextEdit;
    FLockCount: Integer;
    FOptions: TdxPDFViewerOptionsFindPanel;
    FVisible: Boolean;

    procedure SetEdit(const AValue: TdxPDFViewerFindPanelTextEdit);
    procedure SetOptions(const AValue: TdxPDFViewerOptionsFindPanel);
    procedure SetVisible(const AValue: Boolean);

    function CreateEdit: TdxPDFViewerFindPanelTextEdit;
    procedure DestroyEdit;
    procedure InternalSetEdit(const AValue: TdxPDFViewerFindPanelTextEdit);
    procedure UpdateSearchString;

    procedure EditKeyPressHandler(Sender: TObject; var Key: Char);
    procedure EditKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditValueChangedHandler(Sender: TObject);
  private
    FAnimationController: TdxPDFViewerFindPanelAnimationController;
  protected
    procedure CreateAnimationController; virtual;

    function IsLocked: Boolean;
    procedure BeginUpdate;
    procedure Changed;
    procedure EndUpdate;
    procedure HideBeep(var AKey: Char);
    procedure Find;

    property AnimationController: TdxPDFViewerFindPanelAnimationController read FAnimationController;
    property Edit: TdxPDFViewerFindPanelTextEdit read FEdit write SetEdit;
    property Options: TdxPDFViewerOptionsFindPanel read FOptions write SetOptions;
    property OptionsButtonGlyph: TdxSmartGlyph read FOptionsButtonGlyph;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(AViewer: TdxPDFCustomViewer); reintroduce;
    destructor Destroy; override;
  end;

  { TdxPDFViewerPage }

  TdxPDFViewerPage = class(TdxPDFDocumentCustomViewerPage)
  strict private
    function CreatePath(ASelection: TdxPDFTextHighlight; AExcludeTextSelection: Boolean = False): TdxGPPath;
    function GetPainter: TdxPDFViewerPainter;
    function GetSelectionBackColor(AColor: TdxAlphaColor): TdxAlphaColor;
    function GetSelectionFrameColor(AColor: TdxAlphaColor): TdxAlphaColor;
    function GetViewer: TdxPDFCustomViewer;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawContent(ACanvas: TcxCanvas);
    procedure DrawFocusedAnnotation(ACanvas: TcxCanvas);
    procedure DrawHighlights(ACanvas: TcxCanvas);
    procedure DrawImageSelection(ACanvas: TcxCanvas; ASelection: TdxPDFImageSelection);
    procedure DrawPath(ACanvas: TcxCanvas; APath: TdxGPPath; AColor, AFrameColor: TdxAlphaColor);
    procedure DrawSelection(ACanvas: TcxCanvas);
    procedure DrawTextSelection(ACanvas: TcxCanvas; ASelection: TdxPDFTextSelection);
  protected
    function GetVisible: Boolean; override;
    procedure CalculatePageSize; override;

    property Painter: TdxPDFViewerPainter read GetPainter;
    property Viewer: TdxPDFCustomViewer read GetViewer;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
  end;

  { TdxPDFViewerPainter }

  TdxPDFViewerPainterClass = class of TdxPDFViewerPainter;
  TdxPDFViewerPainter = class(TdxPDFViewerCustomObject)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
  protected
    function ButtonSymbolColor(AState: TcxButtonState): TColor; virtual;
    function ButtonTextShift: Integer; virtual;
    function DropDownButtonWidth: Integer;
    function FindPanelCloseButtonSize: TSize; virtual;
    function FindPanelOptionsDropDownButtonWidth: Integer; virtual;

    function NavigationPaneButtonContentOffsets: TRect;
    function NavigationPaneButtonOverlay: TPoint;
    function NavigationPaneButtonSize: TSize;
    function NavigationPanePageCaptionContentOffsets: TRect;
    function NavigationPanePageCaptionTextColor: TColor;
    function NavigationPanePageToolbarContentOffsets: TRect;

    function NavigationPaneContentOffsets: TRect;
    function NavigationPanePageContentOffsets: TRect;
    function HighlightBackColor: TdxAlphaColor; virtual;
    function HighlightFrameColor: TdxAlphaColor; virtual;
    function SelectionBackColor: TdxAlphaColor; virtual;
    function SelectionFrameColor: TdxAlphaColor; virtual;
    function TitleTextColor: TColor; virtual;
    procedure DrawButton(ACanvas: TcxCanvas; const ARect: TRect; const ACaption: string; AState: TcxButtonState); virtual;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect; AState: TcxButtonState); overload;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect; AState: TcxButtonState;
      AColorize: Boolean); overload;
    procedure DrawDropDownButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
    procedure DrawDropDownButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect;
      AState: TcxButtonState; AColorize: Boolean = True);
    procedure DrawFocusRect(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure DrawFindPanelBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure DrawFindPanelCloseButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawFindPanelOptionsButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawFindPanelOptionsDropDownButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AMinimized, ASelected, AIsFirst: Boolean);
    procedure DrawNavigationPaneButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect;
      AState: TcxButtonState);
    procedure DrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
    procedure DrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawPageBackground(ACanvas: TcxCanvas; const ABorderRect, AContentRect: TRect; ASelected: Boolean); virtual;

    function IsFadingAvailable: Boolean;
    function IsSkinUsed: Boolean;

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxPDFViewerLockedStatePaintHelper }

  TdxPDFViewerLockedStatePaintHelper = class(TcxLockedStatePaintHelper)
  strict private
    function GetViewer: TdxPDFCustomViewer;
  protected
    function CanCreateLockedImage: Boolean; override;
    function DoPrepareImage: Boolean; override;
    function GetControl: TcxControl; override;
    function GetOptions: TcxLockedStateImageOptions; override;

    property Viewer: TdxPDFCustomViewer read GetViewer;
  end;

  { TdxPDFViewerLockedStateImageOptions }

  TdxPDFViewerLockedStateImageOptions = class(TcxLockedStateImageOptions)
  protected
    function GetFont: TFont; override;
  published
    property AssignedValues;
    property Color;
    property Effect;
    property Font;
    property ShowText;
    property Text;
  end;

  { TdxPDFCustomViewer }

  TdxPDFCustomViewer = class(TdxPDFDocumentCustomViewer, IdxSkinSupport, IcxLockedStatePaint)
  strict private type
    TChangePageProc = procedure of object;
  strict private
    FActiveController: TdxPDFViewerCustomController;
    FCaretBitmap: TcxBitmap32;
    FController: TdxPDFViewerController;
    FHighlights: TdxPDFViewerHighlights;
    FHitTest: TdxPDFViewerDocumentHitTest;
    FFindPanel: TdxPDFViewerFindPanel;
    FLockedStatePaintHelper: TdxPDFViewerLockedStatePaintHelper;
    FNavigationPane: TdxPDFViewerNavigationPane;
    FSelection: TdxPDFViewerSelection;

    FTextSearch: TdxPDFViewerTextSearch;
    FViewInfo: TdxPDFViewerViewInfo;

    FDialogsLookAndFeel: TcxLookAndFeel;
    FIsDocumentClearing: Boolean;
    FIsDocumentLoading: Boolean;
    FOptionsBehavior: TdxPDFViewerOptionsBehavior;
    FOptionsZoom: TdxPDFViewerOptionsZoom;
    FOptionsLockedStateImage: TdxPDFViewerLockedStateImageOptions;
    FPasswordAttemptsLimit: Integer;

    FOnDocumentLoaded: TdxPDFDocumentLoadedEvent;
    FOnDocumentUnloaded: TNotifyEvent;
    FOnGetPassword: TdxGetPasswordEvent;
    FOnSearchProgress: TdxPDFDocumentTextSearchProgressEvent;
    FOnSelectionChanged: TNotifyEvent;

    FOnAttachmentOpen: TdxPDFViewerOnAttachmentActionEvent;
    FOnAttachmentSave: TdxPDFViewerOnAttachmentActionEvent;
    FOnGetFindPanelVisibility: TdxPDFViewerGetFindPanelVisibilityEvent;
    FOnHideFindPanel: TNotifyEvent;
    FOnHyperlinkClick: TdxPDFViewerOnHyperlinkClickEvent;
    FOnPrepareLockedStateImage: TdxPDFViewerPrepareLockedStateImageEvent;
    FOnShowFindPanel: TNotifyEvent;

    function GetActivePage: TdxPDFViewerPage;
    function GetAttachments: TdxPDFViewerAttachments;
    function GetBookmarks: TdxPDFViewerBookmarks;
    function GetCurrentPageIndex: Integer;
    function GetDocument: TdxPDFDocument;

    function GetDocumentScaleFactor: TdxPointF;
    function GetDocumentToViewerFactor: TdxPointF;
    function GetHandTool: Boolean;
    function GetOptionsFindPanel: TdxPDFViewerOptionsFindPanel;
    function GetOptionsNavigationPane: TdxPDFViewerOptionsNavigationPane;
    function GetOnCustomDrawPreRenderPageThumbnail: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent;
    function GetPainter: TdxPDFViewerPainter;
    function GetRotationAngle: TcxRotationAngle;
    function GetSelectionController: TdxPDFViewerSelectionController;
    function GetThumbnails: TdxPDFViewerThumbnails;
    procedure SetCurrentPageIndex(const AValue: Integer);
    procedure SetDialogsLookAndFeel(const AValue: TcxLookAndFeel);
    procedure SetHandTool(const AValue: Boolean);
    procedure SetOnSearchProgress(const AValue: TdxPDFDocumentTextSearchProgressEvent);
    procedure SetOnCustomDrawPreRenderPageThumbnail(const AValue: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent);
    procedure SetOptionsBehavior(const AValue: TdxPDFViewerOptionsBehavior);
    procedure SetOptionsFindPanel(const AValue: TdxPDFViewerOptionsFindPanel);
    procedure SetOptionsLockedStateImage(AValue: TdxPDFViewerLockedStateImageOptions);
    procedure SetOptionsNavigationPane(const AValue: TdxPDFViewerOptionsNavigationPane);
    procedure SetOptionsZoom(const AValue: TdxPDFViewerOptionsZoom);
    procedure SetRotationAngle(const AValue: TcxRotationAngle);

    function ControllerFromPoint(const P: TPoint; var AController: TdxPDFViewerCustomController): Boolean; overload;
    function ControllerFromPoint(X, Y: Integer; var AController: TdxPDFViewerCustomController): Boolean; overload;
    procedure AfterLoadDocument;
    procedure BeforeLoadDocument;

    procedure CalculateScrollPositions(var ALeftTop: TPoint);
    procedure ChangePage(AProc: TChangePageProc);
    procedure CreateDocument;
    procedure LoadDocumentPages;
    // IcxLockedStatePaint
    function IcxLockedStatePaint.GetImage = GetLockedStateImage;
    function IcxLockedStatePaint.GetTopmostControl = GetLockedStateTopmostControl;
    function GetLockedStateImage: TcxBitmap32;
    function GetLockedStateTopmostControl: TcxControl;

    function OnGetPasswordHandler(Sender: TObject; {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} APassword: string): Boolean;
    procedure OnDocumentLoadedHandler(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure OnDocumentUnLoadedHandler(Sender: TObject);
    procedure OnHighlightsChangedHandler(Sender: TObject);
    procedure OnSelectionChangedHandler(Sender: TObject);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    function CanUpdateRenderQueue: Boolean; override;
    function GetPageRenderFactor(APageIndex: Integer): Single; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoCalculate; override;
    procedure Initialize; override;

    function CanProcessScrollEvents(var Message: TMessage): Boolean; override;
    function CreatePage: TdxPreviewPage; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetClientBounds: TRect; override;
    function GetDefaultHeight: Integer; override;
    function GetDefaultMaxZoomFactor: Integer; override;
    function GetDefaultMinZoomFactor: Integer; override;
    function GetDefaultZoomFactor: Integer; override;
    function GetDefaultZoomStep: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetPageClass: TdxPreviewPageClass; override;
    function GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass; override;
    function GetScrollStep: Integer; override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure CalculatePageNumberHintText(AStartPage, AEndPage: Integer; var AText: string); override;
    procedure CalculateZoomFactorForPageWidthPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer); override;
    procedure CalculateZoomFactorForPagesPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer); override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoSelectedPageChanged; override;
    procedure DoZoomFactorChanged; override;
    procedure DoZoomModeChanged; override;
    procedure DrawContent(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawNoPages(ACanvas: TcxCanvas); override;
    procedure DrawScrollBars(ACanvas: TcxCanvas); override;
    procedure InternalGoToFirstPage; override;
    procedure InternalGoToLastPage; override;
    procedure InternalGoToNextPage; override;
    procedure InternalGoToPrevPage; override;
    procedure FontChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MakeVisible(APageIndex: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer); override;
    procedure SetPaintRegion; override;
    procedure SetSelPageIndex(Value: Integer); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure ScrollPosChanged(const AOffset: TPoint); override;
    procedure SelectFirstPage; override;
    procedure SelectLastPage; override;
    procedure SelectNextPage; override;
    procedure SelectPrevPage; override;
    procedure UpdateLeftTopPosition(const ATopLeft: TPoint);
    procedure UpdateSelectedPageIndex; override;
    procedure UpdateScrollPositions; override;

    function CanOpenAttachment(AAttachment: TdxPDFFileAttachment): Boolean; virtual;
    function CanOpenUri(const AUri: string): Boolean; virtual;
    function CanSaveAttachment(AAttachment: TdxPDFFileAttachment): Boolean; virtual;
    function DoCreateDocument: TdxPDFDocument; virtual;
    function DoGetPassword(var APassword: string): Boolean; virtual;
    function DoPrepareLockedStateImage: Boolean; virtual;

    function CreateLockedStatePaintHelper: TdxPDFViewerLockedStatePaintHelper; virtual;
    function CreateOptionsLockedStateImage: TdxPDFViewerLockedStateImageOptions; virtual;
    function GetControllerClass: TdxPDFViewerControllerClass; virtual;
    function GetPainterClass: TdxPDFViewerPainterClass; virtual;
    function GetViewInfoClass: TdxPDFViewerViewInfoClass; virtual;

    function CanChangeVisibility: Boolean;
    function CanShowFindPanel: Boolean; virtual;
    function CanHideFindPanel: Boolean; virtual;
    procedure SetFindPanelFocus; virtual;

    function CanExtractContent: Boolean;
    function CanDrawCaret: Boolean;
    function CanPrint: Boolean;
    function CanUseAnimation: Boolean;
    function CanZoomIn: Boolean;
    function CanZoomOut: Boolean;
    function CreatePainter: TdxPDFViewerPainter;
    function IsDocumentAvailable: Boolean;
    function IsDocumentPanning: Boolean;
    function IsDocumentSelecting: Boolean;
    function GetFocusedCellAsAttachment: TdxPDFFileAttachment;
    procedure DeleteCaret;
    procedure DrawCaret; overload;
    procedure DrawCaret(const P, ASize: TPoint); overload;
    procedure FindNext;
    procedure RecreatePages;
    procedure UpdateActiveController(const P: TPoint);

    procedure OpenAttachment(AAttachment: TdxPDFFileAttachment);
    procedure SaveAttachment(AAttachment: TdxPDFFileAttachment);

    property ActiveController: TdxPDFViewerCustomController read FActiveController write FActiveController;
    property ActivePage: TdxPDFViewerPage read GetActivePage;
    property Attachments: TdxPDFViewerAttachments read GetAttachments;
    property Bookmarks: TdxPDFViewerBookmarks read GetBookmarks;
    property DialogsLookAndFeel: TcxLookAndFeel read FDialogsLookAndFeel write SetDialogsLookAndFeel;
    property DocumentScaleFactor: TdxPointF read GetDocumentScaleFactor;
    property DocumentToViewerFactor: TdxPointF read GetDocumentToViewerFactor;
    property FindPanel: TdxPDFViewerFindPanel read FFindPanel;
    property FocusedCellAsAttachment: TdxPDFFileAttachment read GetFocusedCellAsAttachment;
    property LockedStatePaintHelper: TdxPDFViewerLockedStatePaintHelper read FLockedStatePaintHelper;
    property NavigationPane: TdxPDFViewerNavigationPane read FNavigationPane;
    property OptionsBehavior: TdxPDFViewerOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsFindPanel: TdxPDFViewerOptionsFindPanel read GetOptionsFindPanel write SetOptionsFindPanel;
    property OptionsLockedStateImage: TdxPDFViewerLockedStateImageOptions read FOptionsLockedStateImage write SetOptionsLockedStateImage;
    property OptionsNavigationPane: TdxPDFViewerOptionsNavigationPane read GetOptionsNavigationPane write SetOptionsNavigationPane;
    property OptionsZoom: TdxPDFViewerOptionsZoom read FOptionsZoom write SetOptionsZoom;
    property Painter: TdxPDFViewerPainter read GetPainter;
    property SelectionController: TdxPDFViewerSelectionController read GetSelectionController;
    property Thumbnails: TdxPDFViewerThumbnails read GetThumbnails;
    property ViewInfo: TdxPDFViewerViewInfo read FViewInfo;
    property ViewerController: TdxPDFViewerController read FController;

    property OnAttachmentOpen: TdxPDFViewerOnAttachmentActionEvent read FOnAttachmentOpen write FOnAttachmentOpen;
    property OnAttachmentSave: TdxPDFViewerOnAttachmentActionEvent read FOnAttachmentSave write FOnAttachmentSave;
    property OnDocumentLoaded: TdxPDFDocumentLoadedEvent read FOnDocumentLoaded write FOnDocumentLoaded;
    property OnDocumentUnloaded: TNotifyEvent read FOnDocumentUnloaded write FOnDocumentUnloaded;
    property OnGetFindPanelVisibility: TdxPDFViewerGetFindPanelVisibilityEvent read FOnGetFindPanelVisibility
      write FOnGetFindPanelVisibility;
    property OnGetPassword: TdxGetPasswordEvent read FOnGetPassword write FOnGetPassword;
    property OnHideFindPanel: TNotifyEvent read FOnHideFindPanel write FOnHideFindPanel;
    property OnHyperlinkClick: TdxPDFViewerOnHyperlinkClickEvent  read FOnHyperlinkClick write FOnHyperlinkClick;
    property OnPrepareLockedStateImage: TdxPDFViewerPrepareLockedStateImageEvent read FOnPrepareLockedStateImage
      write FOnPrepareLockedStateImage;
    property OnCustomDrawPreRenderPageThumbnail: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent
      read GetOnCustomDrawPreRenderPageThumbnail write SetOnCustomDrawPreRenderPageThumbnail;
    property OnSearchProgress: TdxPDFDocumentTextSearchProgressEvent read FOnSearchProgress write SetOnSearchProgress;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnShowFindPanel: TNotifyEvent read FOnShowFindPanel write FOnShowFindPanel;
  public
    function CanGoToNextView: Boolean;
    function CanGoToPrevView: Boolean;
    function IsDocumentLoaded: Boolean;
    procedure Clear;
    procedure ClearViewStateHistory;
    procedure GoToNextView;
    procedure GoToPrevView;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure RotateClockwise;
    procedure RotateCounterclockwise;

    function IsFindPanelVisible: Boolean;
    procedure HideFindPanel;
    procedure ShowFindPanel;

    property CurrentPageIndex: Integer read GetCurrentPageIndex write SetCurrentPageIndex;
    property Document: TdxPDFDocument read GetDocument;
    property HandTool: Boolean read GetHandTool write SetHandTool;
    property Highlights: TdxPDFViewerHighlights read FHighlights;
    property HitTest: TdxPDFViewerDocumentHitTest read FHitTest;
    property PasswordAttemptsLimit: Integer read FPasswordAttemptsLimit write FPasswordAttemptsLimit;
    property RotationAngle: TcxRotationAngle read GetRotationAngle write SetRotationAngle;
    property Selection: TdxPDFViewerSelection read FSelection;
    property TextSearch: TdxPDFViewerTextSearch read FTextSearch;
  end;

  { TdxPDFViewer }

  TdxPDFViewer = class(TdxPDFCustomViewer)
  published
    property Align;
    property Anchors;
    property BorderStyle default cxcbsDefault;
    property Constraints;
    property Font;
    property LookAndFeel;
    property ParentColor default False;
    property PopupMenu;
    property Transparent;
    property Visible;

    property DialogsLookAndFeel;
    property OptionsBehavior;
    property OptionsFindPanel;
    property OptionsLockedStateImage;
    property OptionsNavigationPane;
    property OptionsZoom;

    property OnClick;
    property OnContextPopup;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnAttachmentOpen;
    property OnAttachmentSave;
    property OnCustomDrawPreRenderPage;
    property OnCustomDrawPreRenderPageThumbnail;
    property OnDocumentLoaded;
    property OnDocumentUnloaded;
    property OnGetPassword;
    property OnHideFindPanel;
    property OnHyperlinkClick;
    property OnPrepareLockedStateImage;
    property OnShowFindPanel;
    property OnSearchProgress;
    property OnSelectedPageChanged;
    property OnSelectionChanged;
    property OnZoomFactorChanged;
  end;

  { TdxPDFViewerSelection }

  TdxPDFViewerSelection = class
  strict private
    FController: TdxPDFViewerSelectionController;
    function GetSelection: TdxPDFCustomSelection;
    procedure SetSelection(const AValue: TdxPDFCustomSelection);
    procedure MakeVisible(AMake: Boolean);
  protected
    function IsImageSelection: Boolean;
    function IsTextSelection: Boolean;

    property Selection: TdxPDFCustomSelection read GetSelection write SetSelection;
  public
    constructor Create(AViewer: TdxPDFCustomViewer);

    function AsBitmap: TBitmap;
    function AsText: string;
    function IsEmpty: Boolean;
    procedure Clear;
    procedure CopyToClipboard;
    procedure Select(const ARect: TRect; AMakeVisible: Boolean = False); overload;
    procedure SelectText(const ARange: TdxPDFPageTextRange; AMakeVisible: Boolean = True); overload;
    procedure SelectAll;
  end;

  { TdxPDFViewerTextSearch }

  TdxPDFViewerTextSearch = class(TdxPDFViewerCustomObject)
  strict private
    FLockCount: Integer;
    FOptions: TdxPDFDocumentTextSearchOptions;
    FSearchString: string;

    FAdvancedTextSearch: TdxPDFDocumentContinuousTextSearch;
    FTextSearch: TdxPDFDocumentSequentialTextSearch;

    function GetDocumentTextSearch: TdxPDFDocumentSequentialTextSearch;
    function GetSelection: TdxPDFViewerSelection;
    function GetSearchCompleteMessage: string;
    function GetSearchNoMatchesFoundMessage: string;

    function DoFind: TdxPDFDocumentTextSearchResult;
    procedure AdvancedSearchCompleteHandler(Sender: TObject);

    property DocumentTextSearch: TdxPDFDocumentSequentialTextSearch read GetDocumentTextSearch;
    property Selection: TdxPDFViewerSelection read GetSelection;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function InternalFind: TdxPDFDocumentTextSearchResult;
    function IsLocked: Boolean;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
  public
    function Find(const AText: string): TdxPDFDocumentTextSearchResult; overload;
    function Find(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions): TdxPDFDocumentTextSearchResult; overload;
    procedure Find(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
      var AFoundRanges: TdxPDFPageTextRanges); overload;
  end;

  { TdxPDFViewerHighlights }

  TdxPDFViewerHighlights = class
  strict private
    FController: TdxPDFViewerSelectionController;
    FItems: TObjectList<TdxPDFTextHighlight>;
    FVisible: TdxDefaultBoolean;

    FOnChanged: TNotifyEvent;

    procedure SetVisible(const AValue: TdxDefaultBoolean);

    procedure Changed;
    procedure InternalAdd(AValue: TdxPDFTextHighlight);
  protected
    property Items: TObjectList<TdxPDFTextHighlight> read FItems;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AViewer: TdxPDFCustomViewer);
    destructor Destroy; override;

    procedure Add(const ARange: TdxPDFPageTextRange; ABackColor, AFrameColor: TdxAlphaColor); overload;
    procedure Add(const ARanges: TdxPDFPageTextRanges; ABackColor, AFrameColor: TdxAlphaColor); overload;
    procedure Remove(const ARange: TdxPDFPageTextRange);
    procedure Clear;

    property Visible: TdxDefaultBoolean read FVisible write SetVisible;
  end;

  { TdxPDFViewerCustomHitTest }

  TdxPDFViewerCustomHitTest = class(TdxPDFViewerCustomObject)
  strict protected
    FHitPoint: TPoint;
  protected
    procedure DestroySubClasses; override;

    function CanCalculate: Boolean; virtual;
    function GetPopupMenuClass: TComponentClass; virtual;
    procedure Clear; virtual;
    procedure DoCalculate(const AHitPoint: TPoint); virtual;

    procedure Calculate(const AHitPoint: TPoint);
  public
    property HitPoint: TPoint read FHitPoint;
    property Viewer;
  end;

  { TdxPDFViewerCellHitTest }

  TdxPDFViewerCellHitTest = class(TdxPDFViewerCustomHitTest)
  strict private
    FHitCode: Int64;
    FHitObject: TdxPDFViewerCellViewInfo;
    function GetCursor: TCursor;
  protected
    procedure Clear; override;
    procedure DoCalculate(const AHitPoint: TPoint); override;

    function DoGetCursor: TCursor; virtual;

    function GetHitCode(ACode: Integer): Boolean; inline;
    procedure SetHitCode(ACode: Integer; AValue: Boolean); inline;

    property Cursor: TCursor read GetCursor;
    property HitCodes[ACode: Integer]: Boolean read GetHitCode write SetHitCode;
    property HitObject: TdxPDFViewerCellViewInfo read FHitObject write FHitObject;
  end;

  { TdxPDFViewerDocumentHitTest }

  TdxPDFViewerHitTest = class(TdxPDFViewerCellHitTest);
  TdxPDFViewerDocumentHitTest = class(TdxPDFViewerHitTest)
  strict private
    FDocumentHitObject: TdxPDFRecognizedObject;
    FPosition: TdxPDFPosition;
    FPreviousRecognizedPage: TdxPDFPage;

    function GetDocumentHitObject(APage: TdxPDFPage): TdxPDFRecognizedObject;
    function GetHitAtDocumentViewer: Boolean;
    function GetHitAtSelection: Boolean;
    function GetPage: TdxPDFViewerPage;
    function GetNearestPage: TdxPDFViewerPage;
    function TryGetDocumentHitObjectAsInteractiveObject(out AIntf: IdxPDFInteractiveObject): Boolean;
    function TryGetDocumentHitObjectAsHintableObject(out AIntf: IdxPDFHintableObject): Boolean;
    procedure SetDocumentHitObject(const AValue: TdxPDFRecognizedObject);
  protected
    function CanCalculate: Boolean; override;
    function DoGetCursor: TCursor; override;
    function GetPopupMenuClass: TComponentClass; override;
    procedure Clear; override;
    procedure DoCalculate(const AHitPoint: TPoint); override;
    procedure ResetPreviousRecognizedPage;

    function CanShowHint: Boolean;
    function GetHintBounds: TRect;
    function GetHintText: string;
    function HitAtHintableObject: Boolean;
    function HitAtInteractiveObject: Boolean;

    property DocumentHitObject: TdxPDFRecognizedObject read FDocumentHitObject write SetDocumentHitObject;
    property HitAtDocumentViewer: Boolean read GetHitAtDocumentViewer;
    property HitAtFindPanel: Boolean index hcFindPanel read GetHitCode;
    property HitAtNavigationPaneSplitter: Boolean index hcNavigationPaneSplitter read GetHitCode;
    property Position: TdxPDFPosition read FPosition;
  public
    property Cursor;
    property HitAtAttachment: Boolean index hcAttachment read GetHitCode;
    property HitAtBackground: Boolean index hcBackground read GetHitCode;
    property HitAtImage: Boolean index hcImage read GetHitCode;
    property HitAtHyperlink: Boolean index hcHyperlink read GetHitCode;
    property HitAtText: Boolean index hcText read GetHitCode;
    property HitAtSelection: Boolean index hcSelectionFrame read GetHitCode;
    property HitAtPage: Boolean index hcPageArea read GetHitCode;
  end;

  { TdxPDFViewerNavigationPaneHitTest }

  TdxPDFViewerNavigationPaneHitTest = class(TdxPDFViewerCellHitTest)
  protected
    function DoGetCursor: TCursor; override;
    procedure DoCalculate(const AHitPoint: TPoint); override;
    property HitAtSplitter: Boolean index hcNavigationPaneSplitter read GetHitCode;
  end;

  { TdxPDFViewerCustomController }

  TdxPDFViewerCustomController = class(TdxPDFViewerCustomObject)
  strict private
    FPopupMenu: TComponent;
  protected
    FMouseButtonPressed: Boolean;

    function GetCursor: TCursor; virtual;
    function GetPopupMenuClass: TComponentClass; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    procedure CalculateMouseButtonPressed(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseEnter(AControl: TControl); virtual;
    procedure DoMouseLeave(AControl: TControl); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure UpdateCursor; virtual;
    procedure UpdateStates; virtual;

    function ContextPopup(const P: TPoint): Boolean;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property Cursor: TCursor read GetCursor;
    property MouseButtonPressed: Boolean read FMouseButtonPressed;
    property PopupMenu: TComponent read FPopupMenu;
  end;

  { TdxPDFViewerViewState }

  TdxPDFViewerViewState = class
  strict private
    FChangeType: TdxPDFViewerViewStateChangeType;
    FCurrentPageIndex: Integer;
    FRotationAngle: TcxRotationAngle;
    FScrollPosition: TPoint;
    FTimeStamp: TTimeStamp;
    FZoomFactor: Integer;
    FZoomMode: TdxPreviewZoomMode;
  protected
    function CalculatePageSize(APage: TdxPDFPage): TdxPointF; overload;
    function IsSame(AView: TdxPDFViewerViewState): Boolean;

    property ChangeType: TdxPDFViewerViewStateChangeType read FChangeType;
    property CurrentPageIndex: Integer read FCurrentPageIndex write FCurrentPageIndex;
    property RotationAngle: TcxRotationAngle read FRotationAngle write FRotationAngle;
    property ScrollPosition: TPoint read FScrollPosition write FScrollPosition;
    property TimeStamp: TTimeStamp read FTimeStamp;
    property ZoomFactor: Integer read FZoomFactor write FZoomFactor;
    property ZoomMode: TdxPreviewZoomMode read FZoomMode write FZoomMode;
  public
    constructor Create(AChangeType: TdxPDFViewerViewStateChangeType);
    class function CalculatePageSize(const ASize: TdxPointF; ARotationAngle: TcxRotationAngle): TdxPointF; overload;
  end;

  { TdxPDFViewerViewStateHistory }

  TdxPDFViewerViewStateHistory = class
  strict private
    FCurrentViewStateIndex: Integer;
    FLockCount: Integer;
    FViewStateList: TObjectList<TdxPDFViewerViewState>;

    function GetCurrentViewState: TdxPDFViewerViewState;
    function GetCount: Integer;
    function SameView(AView, ACurrentView: TdxPDFViewerViewState): Boolean;
  protected
    function CanGoToNextView: Boolean;
    function CanGoToPreviousView: Boolean;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    procedure GoToNextView;
    procedure GoToPrevView;
    procedure Initialize(AView: TdxPDFViewerViewState);
    procedure StoreViewState(AView: TdxPDFViewerViewState);

    property Count: Integer read GetCount;
    property CurrentViewState: TdxPDFViewerViewState read GetCurrentViewState;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
  end;

  { TdxPDFViewerViewStateHistoryController }

  TdxPDFViewerViewStateHistoryController = class(TdxPDFViewerCustomController)
  strict private
    FHistory: TdxPDFViewerViewStateHistory;

    function CreateView(AChangeType: TdxPDFViewerViewStateChangeType): TdxPDFViewerViewState;
    function GetCurrentViewState: TdxPDFViewerViewState;
    procedure RecreateHistory;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function CanGoToNextView: Boolean;
    function CanGoToPreviousView: Boolean;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    procedure GoToNextView;
    procedure GoToPrevView;
    procedure StoreCurrentViewState(AChangeType: TdxPDFViewerViewStateChangeType);
    procedure RestoreViewState;

    property CurrentViewState: TdxPDFViewerViewState read GetCurrentViewState;
    property History: TdxPDFViewerViewStateHistory read FHistory;
  end;

  { TdxPDFViewerContentSelector }

  TdxPDFViewerContentSelector = class
  strict private
    FInProgress: Boolean;
    FViewer: TdxPDFCustomViewer;
    function GetHitTest: TdxPDFViewerDocumentHitTest;
    function GetSelection: TdxPDFCustomSelection;
    function GetSelectionBackColor: TdxAlphaColor;
    function GetSelectionFrameColor: TdxAlphaColor;
    procedure SetSelection(const AValue: TdxPDFCustomSelection);
  protected
    procedure Clear; virtual;
    procedure Reset; virtual;

    property InProgress: Boolean read FInProgress write FInProgress;
    property HitTest: TdxPDFViewerDocumentHitTest read GetHitTest;
    property Selection: TdxPDFCustomSelection read GetSelection write SetSelection;
    property SelectionBackColor: TdxAlphaColor read GetSelectionBackColor;
    property SelectionFrameColor: TdxAlphaColor read GetSelectionFrameColor;
    property Viewer: TdxPDFCustomViewer read FViewer;
  public
    constructor Create(AViewer: TdxPDFCustomViewer);
  end;

  { TdxPDFViewerImageSelector }

  TdxPDFViewerImageSelector = class(TdxPDFViewerContentSelector)
  strict private
    FIsSelected: Boolean;
    FSelectedImage: TdxPDFImage;
    FSelectedImagePageIndex: Integer;
    FStartPosition: TdxPDFPosition;
    function CreateImageSelection(const ABounds: TdxRectF): TdxPDFImageSelection;
  protected
    procedure Clear; override;
    procedure Reset; override;

    function Select: Boolean;
    procedure StartSelection(var AInOutsideContent: Boolean);

    property SelectedImage: TdxPDFImage read FSelectedImage;
  end;

  { TdxPDFViewerTextSelector }

  TdxPDFViewerTextSelector = class(TdxPDFViewerContentSelector)
  strict private type
    TdxPDFMovingCaretProc = procedure of object;
  strict private
    FStartPageIndex: Integer;
    FStartPoint: TdxPointF;
    FStartTextPosition: TdxPDFTextPosition;

    function GetCaret: TdxPDFDocumentCaret;
    function GetPage(AIndex: Integer): TdxPDFPage;
    function GetScaleFactor: TdxPointF;
    function GetTextLines(APageIndex: Integer): TdxPDFTextLineList;
    procedure SetCaret(const AValue: TdxPDFDocumentCaret);

    function CreateRangeList(const AStart, AEnd: TdxPDFTextPosition): TdxPDFPageTextRanges; overload;
    function CreateRangeList(const AStart: TdxPDFTextPosition; const AEnd: TdxPDFPosition): TdxPDFPageTextRanges; overload;
    function HasCaret: Boolean;
    function IsArrowKeys(ADirection: TdxPDFMovementDirection): Boolean;
    function IsEmptySelection: Boolean;
    function IsPositionInLine(APageIndex, ALineIndex, AWordIndex, AOffset: Integer): Boolean;
    function FindLine(const APosition: TdxPDFTextPosition; out ALine: TdxPDFTextLine): Boolean;
    function FindNearestLineByDistance(const APosition: TdxPDFPosition): TdxPDFTextLine;
    function FindNearestPosition(const APosition: TdxPDFPosition; const ATextPosition: TdxPDFTextPosition): TdxPDFTextPosition;
    function FindWordEndPosition(APosition: TdxPDFTextPosition): Integer;
    function MoveCaretToLeft: Boolean;
    function MoveCaretToRight: Boolean;
    function MoveRight(AWordParts: TdxPDFTextWordPartList; APageIndex, ALineIndex, AWordIndex, AOffset: Integer;
      AProcessLastWordPart: Boolean): Boolean; overload;
    function NormalizeDirection(ADirection: TdxPDFMovementDirection): TdxPDFMovementDirection;
    function SameNextWordPartIndex(APageIndex, ALineIndex, AWordIndex: Integer): Boolean;
    function ValidateRanges(const ARanges: TdxPDFPageTextRanges): Boolean;
    procedure DoMoveCaret(AProc: TdxPDFMovingCaretProc);
    procedure MakeCaretVisible;
    procedure MoveAndMakeCaretVisible(const APosition: TdxPDFTextPosition);
    procedure MoveDown;
    procedure MoveLeft;
    procedure MoveRight; overload;
    procedure MoveToDocumentEnd;
    procedure MoveToDocumentStart;
    procedure MoveToLineStart;
    procedure MoveToLineEnd;
    procedure MoveToNextWord;
    procedure MoveToPreviousWord;
    procedure MoveUp;
    procedure Select(APage: TdxPDFPage; AProc: TProc); overload;
    procedure SetCaretPosition(const APosition: TdxPDFTextPosition);
    procedure StoreSelectionStartTextPosition;
    procedure UpdateSelection(const APosition: TdxPDFTextPosition);
  protected
    function CreateTextHighlights(const ARanges: TdxPDFPageTextRanges;
      ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight;
    function CreateTextSelection(const ARange: TdxPDFPageTextRange): TdxPDFTextSelection; overload;
    function CreateTextSelection(const ARanges: TdxPDFPageTextRanges): TdxPDFTextSelection; overload;
    function GetCaretViewData(const APosition: TdxPDFTextPosition): TdxPDFDocumentCaretViewData;
    function StartSelection(const APosition: TdxPDFPosition): Boolean;
    procedure MoveCaret(const APosition: TdxPDFTextPosition); overload;
    procedure MoveCaret(ADirection: TdxPDFMovementDirection); overload;
    procedure Select(const APosition: TdxPDFPosition); overload;
    procedure SelectByKeyboard(ADirection: TdxPDFMovementDirection);
    procedure SelectLine(const APosition: TdxPDFPosition);
    procedure SelectPage(const APosition: TdxPDFPosition);
    procedure SelectWord(const APosition: TdxPDFPosition);

    property Caret: TdxPDFDocumentCaret read GetCaret write SetCaret;
    property Page[AIndex: Integer]: TdxPDFPage read GetPage;
    property TextLines[APageIndex: Integer]: TdxPDFTextLineList read GetTextLines;
    property ScaleFactor: TdxPointF read GetScaleFactor;
  end;

  { TdxPDFViewerSelectionController }

  TdxPDFViewerSelectionController = class(TdxPDFViewerCustomObject)
  strict private type
    TMakeRectVisibleEvent = procedure(APageIndex: Integer; const ARect: TdxRectF) of object;
  strict private
    FCaret: TdxPDFDocumentCaret;
    FClickController: TdxPDFViewerClickController;
    FImageSelector: TdxPDFViewerImageSelector;
    FInOutsideContent: Boolean;
    FSelection: TdxPDFCustomSelection;
    FStartSelectionPosition: TdxPDFPosition;
    FTextSelector: TdxPDFViewerTextSelector;

    FOnMakeRectVisible: TMakeRectVisibleEvent;
    FOnSelectionChanged: TNotifyEvent;

    function GetHitTest: TdxPDFViewerDocumentHitTest;
    function GetImages(APageIndex: Integer): TdxPDFImageList;
    function GetScaleFactor: TdxPointF;
    function GetTextLines(APageIndex: Integer): TdxPDFTextLineList;
    procedure SetCaret(const AValue: TdxPDFDocumentCaret);
    procedure SetSelection(const AValue: TdxPDFCustomSelection);

    function CanExtractSelectedContent(AHitCode: Int64): Boolean;
    function IsEmptySelection: Boolean;
    function GetImageSelection(const AArea: TdxPDFDocumentArea): TdxPDFImageSelection;
    function GetTextRanges(const AArea: TdxPDFDocumentArea): TdxPDFPageTextRanges;
    function GetTextSelection(const AArea: TdxPDFDocumentArea): TdxPDFTextSelection;
    procedure CopyImageToClipboard;
    procedure CopyTextToClipboard;
    procedure EndSelection; overload;
    procedure EndSelection(AShift: TShiftState); overload;
    procedure DoSelect; overload;
    procedure DoSelect(AShift: TShiftState); overload;
    procedure SelectByKeyboard(ADirection: TdxPDFMovementDirection);
    procedure SelectImage(const AArea: TdxPDFDocumentArea); overload;
    procedure SelectText(const ARanges: TdxPDFPageTextRanges); overload;
    procedure StartSelection; overload;
    procedure StartSelection(AShift: TShiftState); overload;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function CreateHighlight(ARange: TdxPDFPageTextRange;
      ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight; overload;
    function CreateHighlight(const ARanges: TdxPDFPageTextRanges;
      ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight; overload;
    function KeyDown(var AKey: Word; AShift: TShiftState): Boolean;
    function GetSelectionAsBitmap: TcxBitmap;
    function GetSelectionAsText: string;
    function GetPage(AIndex: Integer): TdxPDFPage;
    function HitAtSelection: Boolean;
    procedure Clear;
    procedure LockedClear;
    procedure CopyToClipboard;
    procedure HideCaret;
    procedure MakeVisible;
    procedure Select(const ARect: TRect);
    procedure SelectText(ARange: TdxPDFPageTextRange); overload;
    procedure SelectAll;
    procedure SelectionChanged;

    property Caret: TdxPDFDocumentCaret read FCaret write SetCaret;
    property ClickController: TdxPDFViewerClickController read FClickController;
    property HitTest: TdxPDFViewerDocumentHitTest read GetHitTest;
    property Images[APageIndex: Integer]: TdxPDFImageList read GetImages;
    property Page[AIndex: Integer]: TdxPDFPage read GetPage;
    property Selection: TdxPDFCustomSelection read FSelection write SetSelection;
    property ScaleFactor: TdxPointF read GetScaleFactor;
    property TextLines[APageIndex: Integer]: TdxPDFTextLineList read GetTextLines;

    property OnMakeRectVisible: TMakeRectVisibleEvent read FOnMakeRectVisible write FOnMakeRectVisible;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  public
    constructor Create(AViewer: TdxPDFCustomViewer); override;
    destructor Destroy; override;
  end;

  { TdxPDFViewerContainerController }

  TdxPDFViewerContainerController = class(TdxPDFViewerCustomController)
  strict private
    FFocusedCell: TdxPDFViewerCellViewInfo;
    FHotCell: TdxPDFViewerCellViewInfo;
    FPressedCell: TdxPDFViewerCellViewInfo;
    FPrevFocusedCell: TdxPDFViewerCellViewInfo;
    procedure SetHotCell(const AValue: TdxPDFViewerCellViewInfo);
    procedure SetPressedCell(const AValue: TdxPDFViewerCellViewInfo);
    procedure SetFocusedCell(const AValue: TdxPDFViewerCellViewInfo);
    procedure CalculateHitTests(X, Y: Integer); overload;
  protected
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseEnter(AControl: TControl); override;
    procedure DoMouseLeave(AControl: TControl); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function GetHitTest: TdxPDFViewerCellHitTest; virtual;
    function ProcessChildKey(var AKey: Word; AShiftState: TShiftState): Boolean; virtual;
    procedure Clear; virtual;
    procedure DoCalculateHitTests(const P: TPoint); virtual;
    procedure UpdateCursor; override;
    procedure UpdateState; virtual;

    function FindNextFocusableCell(ACell: TdxPDFViewerCellViewInfo; AGoForward: Boolean): TdxPDFViewerCellViewInfo;
    procedure CalculateHitTests(const P: TPoint); overload;
    procedure CellRemoving(ACell: TdxPDFViewerCellViewInfo);
    procedure FocusNextCell(AGoForward: Boolean);
    procedure ProcessAccel(ACell: TdxPDFViewerCellViewInfo);
    procedure ProcessClick(ACell: TdxPDFViewerCellViewInfo);

    property FocusedCell: TdxPDFViewerCellViewInfo read FFocusedCell write SetFocusedCell;
    property HitTest: TdxPDFViewerCellHitTest read GetHitTest;
    property HotCell: TdxPDFViewerCellViewInfo read FHotCell write SetHotCell;
    property PressedCell: TdxPDFViewerCellViewInfo read FPressedCell write SetPressedCell;
  end;

  { TdxPDFViewerNavigationPaneController }

  TdxPDFViewerNavigationPaneController = class(TdxPDFViewerContainerController)
  strict private type
    {$REGION 'private types'}
    THintHelper = class(TcxControlHintHelper)
    private
      FViewer: TdxPDFCustomViewer;
    protected
      procedure CorrectHintWindowRect(var ARect: TRect); override;
      function GetOwnerControl: TcxControl; override;
      function IsHintWindowVisible: Boolean;
    public
      constructor Create(AViewer: TdxPDFCustomViewer);
    end;
  {$ENDREGION}
  strict private
    FHintCell: TdxPDFViewerCellViewInfo;
    FHintHelper: THintHelper;
    FShowHintTimer: TcxTimer;
    function GetHintText: string;
    function GetNavigationPane: TdxPDFViewerNavigationPane;
    function NeedShowHint(const AHint: string): Boolean;
    procedure CheckHint;
    procedure ShowHintTimerExpired(Sender: TObject);
  protected
    function GetHitTest: TdxPDFViewerCellHitTest; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure CreateSubClasses; override;
    procedure Clear; override;
    procedure DestroySubClasses; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateCursor; override;
    procedure UpdateStates; override;

    function InPaneRect(const P: TPoint): Boolean;
    function InPaneSplitterRect(const P: TPoint): Boolean;
    procedure ExecuteOperation(const AOperation: TdxPDFInteractiveOperation);
    procedure Refresh;

    property NavigationPane: TdxPDFViewerNavigationPane read GetNavigationPane;
  end;

  { TdxPDFViewerController }

  TdxPDFViewerController = class(TdxPDFViewerContainerController)
  strict private type
  {$REGION 'private types'}
    TInteractiveObjectHinHelper = class(TcxControlHintHelper)
    private
      FShowingTimer: TcxTimer;
      FViewer: TdxPDFCustomViewer;
      procedure ShowingTimerHandler(Sender: TObject);
    protected
      function CanShowHint: Boolean; override;
      function GetOwnerControl: TcxControl; override;
      procedure CorrectHintWindowRect(var ARect: TRect); override;
      procedure MouseLeave; override;

      procedure EnableShowing;
    public
      constructor Create(AViewer: TdxPDFCustomViewer);
      destructor Destroy; override;
      procedure MouseDown; override;
    end;
  {$ENDREGION}
  strict private
    FCanClick: Boolean;
    FClickTimer: TcxTimer;
    FFocusedField: TdxPDFAcroFormField;
    FInteractiveObjectHintHelper: TInteractiveObjectHinHelper;
    FInteractivityController: TdxPDFViewerInteractivityController;
    FPrevHandPoint: TPoint;
    FSelectionController: TdxPDFViewerSelectionController;
    FViewStateHistoryController: TdxPDFViewerViewStateHistoryController;

    function GetDocumentHitTest: TdxPDFViewerDocumentHitTest;
    function GetHandToolCursor: TCursor;
    function GetSelectToolCursor: TCursor;
    procedure SetFocusedField(const AValue: TdxPDFAcroFormField);

    function GetAttachmentSavingPath(AAttachment: TdxPDFFileAttachment): string;
    procedure DoSaveAttachment(const APath: string; AAttachment: TdxPDFFileAttachment);

    function IsAltPressed(AShift: TShiftState): Boolean;
    function IsCtrlShiftPressed(AShift: TShiftState): Boolean;
    function NeedHorizontalScroll: Boolean;
    function ProcessViewerKeyDown(var AKey: Word; AShift: TShiftState): Boolean;
    procedure ClickTimerHandler(Sender: TObject);
    procedure OnMakeRectVisibleHandler(APageIndex: Integer; const ARect: TdxRectF);
    procedure UpdateFocusedField(AButton: TMouseButton);
  protected
    function GetCursor: TCursor; override;
    function GetHitTest: TdxPDFViewerCellHitTest; override;
    function GetPopupMenuClass: TComponentClass; override;
    procedure CalculateMouseButtonPressed(Shift: TShiftState; X, Y: Integer); override;
    procedure Clear; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function ProcessChildKey(var AKey: Word; AShiftState: TShiftState): Boolean; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseLeave(AControl: TControl); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateCursor; override;
    procedure UpdateStates; override;

    function GetPageScrollPosition(APage: TdxPDFViewerPage; AScrollPositionX, AScrollPositionY: Single): TdxPointF;
    function GetPageTopLeft(APage: TdxPDFViewerPage): TdxPointF;

    function CanSearchText: Boolean;
    function IsDocumentPanning: Boolean;
    function IsDocumentSelecting: Boolean;
    function InFindPanelRect (const P: TPoint): Boolean;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean;
    procedure DoFindText;
    procedure ExecuteOperation(const AOperation: TdxPDFInteractiveOperation);
    procedure MakeRectVisible(const ARect: TdxRectF; AType: TdxVisibilityType = vtCentered; AIsTopAlignment: Boolean = False);
    procedure MakeSelectionRectVisible(APageIndex: Integer; const ARect: TdxRectF);
    procedure OffsetContent(const AOffset: TPoint);
    procedure Rotate(AAngleDelta: Integer);
    procedure ScrollPosChanged;
    procedure ShowDocumentPosition(const ATarget: TdxPDFTarget);

    procedure OpenAttachment(AAttachment: TdxPDFFileAttachment);
    procedure SaveAttachment(AAttachment: TdxPDFFileAttachment);

    property DocumentHitTest: TdxPDFViewerDocumentHitTest read GetDocumentHitTest;
    property FocusedField: TdxPDFAcroFormField read FFocusedField write SetFocusedField;
    property SelectionController: TdxPDFViewerSelectionController read FSelectionController;
    property ViewStateHistoryController: TdxPDFViewerViewStateHistoryController read FViewStateHistoryController;
  end;

  { TdxPDFViewerCellViewInfo }

  TdxPDFViewerCellViewInfoClass = class of TdxPDFViewerCellViewInfo;
  TdxPDFViewerCellViewInfo = class(TcxIUnknownObject)
  private
    FBounds: TRect;
    FController: TdxPDFViewerContainerController;
    FPainter: TdxPDFViewerPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewer: TdxPDFCustomViewer;
    procedure SetBounds(const ABounds: TRect);
  protected
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    function CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean; virtual;
    function CanDrawContent: Boolean; virtual;
    function CanFocus: Boolean; virtual;
    function IsFocused: Boolean; virtual;
    function IsHot: Boolean; virtual;
    function IsPressed: Boolean; virtual;
    function GetClipRect: TRect; virtual;
    function GetFont: TFont; virtual;
    function GetVisible: Boolean; virtual;
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;
    procedure Calculate; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawChildren(ACanvas: TcxCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure Invalidate; virtual;
    procedure UpdateState; virtual;

    function ApplyScaleFactor(AValue: Integer): Integer; overload;
    function ApplyScaleFactor(AValue: TSize): TSize; overload;
    function ApplyScaleFactor(AValue: TRect): TRect; overload;
    procedure Draw(ACanvas: TcxCanvas; AForce: Boolean = False); virtual;

    property Bounds: TRect read FBounds write SetBounds;
    property Controller: TdxPDFViewerContainerController read FController;
    property Painter: TdxPDFViewerPainter read FPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Viewer: TdxPDFCustomViewer read GetViewer;
    property Visible: Boolean read GetVisible;
  public
    constructor Create(AController: TdxPDFViewerContainerController); virtual;
    destructor Destroy; override;
  end;

  { TdxPDFViewerViewInfoList }

  TdxPDFViewerViewInfoList = class(TObjectList<TdxPDFViewerCellViewInfo>)
  protected
    function CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean;
    function MaxMeasureHeight: Integer;
    procedure Draw(ACanvas: TcxCanvas);
    procedure UpdateState;
  end;

  { TdxPDFViewerContainerViewInfo }

  TdxPDFViewerContainerViewInfo = class(TdxPDFViewerCellViewInfo)
  strict private
    FCellList: TdxPDFViewerViewInfoList;
  protected
    function CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean; override;
    function MeasureHeight: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DrawChildren(ACanvas: TcxCanvas); override;
    procedure UpdateState; override;

    function GetContentMargins: TRect; virtual;
    function GetIndentBetweenElements: Integer; virtual;
    procedure Calculate; override;

    procedure DoCalculate; virtual;
    procedure ClearCells; virtual;
    procedure CreateCells; virtual;

    function AddCell(ACellClass: TdxPDFViewerCellViewInfoClass): TdxPDFViewerCellViewInfo; overload;
    function AddCell(ACellClass: TdxPDFViewerCellViewInfoClass;
      AController: TdxPDFViewerContainerController): TdxPDFViewerCellViewInfo; overload;
    function AlignToTopClientSide(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
    function AlignToLeftSide(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
    function AlignToRightSide(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
    function GetContentBounds: TRect;
    procedure RecreateCells;
    procedure SetEmptyBounds(AViewInfo: TdxPDFViewerCellViewInfo);

    property CellList: TdxPDFViewerViewInfoList read FCellList;
    property ContentMargins: TRect read GetContentMargins;
    property IndentBetweenElements: Integer read GetIndentBetweenElements;
  end;

  { TdxPDFViewerViewInfo }

  TdxPDFViewerViewInfo = class(TdxPDFViewerContainerViewInfo)
  strict private
    FDocumentViewerBounds: TRect;
    FFindPanel: TdxPDFViewerFindPanelViewInfo;
    function GetController: TdxPDFViewerController;
    function GetNavigationPane: TdxPDFViewerNavigationPaneViewInfo;
    procedure CalculateFindPanelBounds(var ABounds: TRect);
  protected
    function CanFocus: Boolean; override;
    procedure Calculate; override;
    procedure CreateCells; override;
    procedure Draw(ACanvas: TcxCanvas; AForce: Boolean = False); override;

    function GetTransitionEffectAreaBounds: TRect; virtual;
    procedure CreateFindPanelViewInfo; virtual;
    procedure PopulateTabOrders(AList: TdxPDFViewerViewInfoList); virtual;

    property Controller: TdxPDFViewerController read GetController;
    property DocumentViewerBounds: TRect read FDocumentViewerBounds;
    property FindPanel: TdxPDFViewerFindPanelViewInfo read FFindPanel;
    property NavigationPane: TdxPDFViewerNavigationPaneViewInfo read GetNavigationPane;
  end;

  { TdxPDFViewerCaptionViewInfo }

  TdxPDFViewerCaptionViewInfo = class(TdxPDFViewerCellViewInfo)
  protected
    function CanDrawContent: Boolean; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure DrawContent(ACanvas: TcxCanvas); override;

    function GetPainterTextColor: TColor; virtual;
    function GetText: string; virtual;
    function GetTextAlignment: TAlignment; virtual;
    procedure PrepareCanvas(ACanvas: TcxCanvas); virtual;

    property Font: TFont read GetFont;
    property Text: string read GetText;
  end;

  { TdxPDFViewerButtonViewInfo }

  TdxPDFViewerButtonViewInfoClass = class of TdxPDFViewerButtonViewInfo;
  TdxPDFViewerButtonViewInfo = class(TdxPDFViewerCellViewInfo, IcxHintableObject)
  strict private
    FFadingHelper: TdxPDFViewerButtonFadingHelper;
    FState: TcxButtonState;
    function GetGlyphSize: TSize;
    procedure SetState(const AState: TcxButtonState);

    function GetButtonOffset(AButtonState: TcxButtonState): TPoint;
    procedure DrawFading(ACanvas: TcxCanvas);
    procedure DrawFocusRect(ACanvas: TcxCanvas);
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
  protected
    FCaption: string;
    FCaptionRect: TRect;
    FGlyph: TdxSmartGlyph;
    FGlyphRect: TRect;

    function CanFocus: Boolean; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure Calculate; override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    procedure UpdateState; override;

    function CalculateState: TcxButtonState; virtual;
    function IsDefault: Boolean; virtual;
    function IsEnabled: Boolean; virtual;
    function IsFadingAvailable: Boolean;
    function IsSkinUsed: Boolean;
    function GetDefaultCaption: string; virtual;
    function GetGlyph: TdxSmartGlyph; virtual;
    function GetGlyphAlignmentHorz: TAlignment; virtual;
    function GetHint: string; virtual;
    function GetMargins: TRect; virtual;
    procedure CalculateCaptionRect; virtual;
    procedure CalculateGlyphRect; virtual;
    procedure DoExecute; virtual;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawButtonContent(ACanvas: TcxCanvas); virtual;

    procedure DrawButton(ACanvas: TcxCanvas);
    procedure Execute;

    property CaptionRect: TRect read FCaptionRect;
    property FadingHelper: TdxPDFViewerButtonFadingHelper read FFadingHelper;
    property Font: TFont read GetFont;
    property GlyphRect: TRect read FGlyphRect;
    property GlyphSize: TSize read GetGlyphSize;
    property Margins: TRect read GetMargins;
    property State: TcxButtonState read FState write SetState;
  public
    constructor Create(AController: TdxPDFViewerContainerController); override;
    destructor Destroy; override;
  end;

  { TdxPDFViewerCustomDropDownButtonViewInfo }

  TdxPDFViewerCustomDropDownButtonViewInfo = class(TdxPDFViewerButtonViewInfo)
  strict private
    FDropDownButtonArrowRect: TRect;
    procedure CalculateDropDownButtonArrowRect;
  protected
    function IsEnabled: Boolean; override;
    function MeasureWidth: Integer; override;
    procedure Calculate; override;
    procedure DoExecute; override;
    procedure DrawButtonContent(ACanvas: TcxCanvas); override;

    function GetColorizeGlyph: Boolean; virtual;
    function GetPopupMenuClass: TComponentClass; virtual; abstract;
  end;

  { TdxPDFViewerButtonFadingHelper }

  TdxPDFViewerButtonFadingHelper = class(TdxFadingObjectHelper)
  private
    FButtonViewInfo: TdxPDFViewerButtonViewInfo;
  protected
    function CanFade: Boolean; override;
    procedure DrawFadeImage; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;

    property ButtonViewInfo: TdxPDFViewerButtonViewInfo read FButtonViewInfo;
  public
    constructor Create(AViewInfo: TdxPDFViewerButtonViewInfo); virtual;
  end;

  { TdxPDFViewerImageAnimationTransition }

  TdxPDFViewerImageAnimationTransition = class(TdxAnimationTransition)
  strict private
    FDestination: TcxBitmap32;
    FImage: TdxGPImage;
    FIsHiding: Boolean;
    FMode: TdxDrawAnimationMode;

    function TransitionLength(AImageHeight: Integer): Integer;
    procedure Draw(AGraphics: TdxGPGraphics; const ADestRect: TRect); overload;
    procedure DrawFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    procedure DrawScrollDown(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollDownFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollUp(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollUpFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure PrepareImage(AImage: TGraphic);
  protected
    procedure Draw(ACanvas: TCanvas; const ADestRect: TRect); overload;
  public
    constructor Create(AImage: TGraphic; ATime: Cardinal; AMode: TdxDrawAnimationMode; AIsHiding: Boolean); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TdxPDFViewerFindPanelAnimationController }

  TdxPDFViewerFindPanelAnimationController = class(TdxPDFViewerCustomObject)
  private
    FActive: Boolean;
    FAnimatedRect: TRect;
    FAnimation: TdxPDFViewerImageAnimationTransition;
    function CreateFindPanelBitmap: TcxBitmap;
    function GetAnimation: TdxPDFViewerFindPanelAnimation;
    procedure DoAnimate(AShowing: Boolean);
    procedure AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure RedrawArea(const ARect: TRect);
    procedure RunAnimation(AImage: TBitmap; ATime: Cardinal; AMode: TdxDrawAnimationMode; AIsHiding: Boolean);
  protected
    procedure Animate(AShowing: Boolean);
    procedure Draw(ACanvas: TcxCanvas);

    property Active: Boolean read FActive;
  end;

  { TdxPDFViewerFindPanelViewInfo }

  TdxPDFViewerFindPanelViewInfo = class(TdxPDFViewerContainerViewInfo)
  strict private type
  {$REGION 'private types'}
    TEditViewInfo = class(TdxPDFViewerCellViewInfo)
    strict private
      function GetEdit: TdxPDFViewerFindPanelTextEdit;
      function GetMaxWidth: Integer;
      function GetMinWidth: Integer;
    protected
      ActualWidth: Integer;

      function CanFocus: Boolean; override;
      function MeasureHeight: Integer; override;
      function MeasureWidth: Integer; override;
      procedure Calculate; override;
      procedure UpdateState; override;

      procedure SetFocus;

      property MaxWidth: Integer read GetMaxWidth;
      property MinWidth: Integer read GetMinWidth;
      property InternalEdit: TdxPDFViewerFindPanelTextEdit read GetEdit;
    public
      constructor Create(AController: TdxPDFViewerContainerController); override;
    end;

    TButtonViewInfo = class(TdxPDFViewerButtonViewInfo)
    protected
      function IsEnabled: Boolean; override;
      function MeasureWidth: Integer; override;
    end;

    TCloseButtonViewInfo = class(TButtonViewInfo)
    protected
      function IsEnabled: Boolean; override;
      function MeasureHeight: Integer; override;
      function MeasureWidth: Integer; override;
      function GetVisible: Boolean; override;
      procedure DoExecute; override;
      procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
      procedure DrawButtonContent(ACanvas: TcxCanvas); override;
    end;

    TSearchButtonViewInfo = class(TButtonViewInfo)
    protected
      procedure DoExecute; override;
      function GetSearchDirection: TdxPDFDocumentTextSearchDirection; virtual;
    end;

    TNextButtonViewInfo = class(TSearchButtonViewInfo)
    protected
      function GetDefaultCaption: string; override;
      function GetVisible: Boolean; override;
    end;

    TOptionsButtonViewInfo = class(TdxPDFViewerCustomDropDownButtonViewInfo)
    protected
      function IsEnabled: Boolean; override;
      function GetGlyph: TdxSmartGlyph; override;
      function GetPopupMenuClass: TComponentClass; override;
      function GetVisible: Boolean; override;
    end;

    TPreviousButtonViewInfo = class(TSearchButtonViewInfo)
    protected
      function GetDefaultCaption: string; override;
      function GetSearchDirection: TdxPDFDocumentTextSearchDirection; override;
      function GetVisible: Boolean; override;
    end;
  {$ENDREGION}
  strict private
    FCaption: TdxPDFViewerCaptionViewInfo;
    FCloseButton: TCloseButtonViewInfo;
    FEdit: TEditViewInfo;
    FNextButton: TNextButtonViewInfo;
    FOptions: TOptionsButtonViewInfo;
    FPreviousButton: TPreviousButtonViewInfo;

    function GetActualNextButtonViewInfo: TButtonViewInfo;
  protected
    function GetContentMargins: TRect; override;
    function GetIndentBetweenElements: Integer; override;
    function GetVisible: Boolean; override;
    function MeasureWidth(const ABounds: TRect): Integer; reintroduce;
    procedure CreateCells; override;
    procedure DoCalculate; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;

    procedure Update;

    property ActualNextButtonViewInfo: TButtonViewInfo read GetActualNextButtonViewInfo;
    property Caption: TdxPDFViewerCaptionViewInfo read FCaption;
    property CloseButton: TCloseButtonViewInfo read FCloseButton;
    property Edit: TEditViewInfo read FEdit;
    property NextButton: TNextButtonViewInfo read FNextButton;
    property OptionsButton: TOptionsButtonViewInfo read FOptions;
    property PreviousButton: TPreviousButtonViewInfo read FPreviousButton;
  end;

  { TdxPDFViewerInteractivityController }

  TdxPDFViewerInteractivityController = class(TdxPDFViewerCustomObject, IUnknown, IdxPDFInteractivityController)
  strict private
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IdxPDFInteractivityController
    procedure ExecuteOperation(AField: TdxPDFAcroFormActionField); overload;
    procedure GoToFirstPage;
    procedure GoToLastPage;
    procedure GoToNextPage;
    procedure GoToPrevPage;
    procedure OpenUri(const AUri: string);

    procedure ExecuteActions(AAction: TdxPDFCustomAction; AExecutedActions: TList<TdxPDFCustomAction>);
  protected
    class procedure Execute(AViewer: TdxPDFCustomViewer; const AOperation: TdxPDFInteractiveOperation); overload; static;
    procedure ExecuteOperation(const AOperation: TdxPDFInteractiveOperation); overload;
    procedure ShowDocumentPosition(const ATarget: TdxPDFTarget);
  public
    class procedure Execute(AViewer: TdxPDFCustomViewer; AOutline: TdxPDFOutlineTreeItem); overload; static;
    class procedure Execute(AViewer: TdxPDFCustomViewer; AHyperlink: TdxPDFHyperlink); overload; static;
  end;

  { TdxPDFViewerBookmarkTreeView }
  TdxPDFViewerNavigationPaneInternalControlClass = class of TdxPDFViewerNavigationPaneInternalControl;
  TdxPDFViewerNavigationPaneInternalControl = class(TdxPDFViewerCustomObject)
  strict private
    function GetBounds: TRect;
    function GetVisible: Boolean;
    procedure SetBounds(const AValue: TRect);
  strict protected
    FInternalControl: TWinControl;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function GetEmpty: Boolean; virtual; abstract;
    procedure ClearInternalControl; virtual; abstract;
    procedure CreateInternalControl; virtual; abstract;
    procedure PopulateInternalControl; virtual; abstract;
    procedure UpdateInternalControlTextSize; virtual; abstract;

    procedure InitializeInternalControl; virtual;
  protected
    procedure Clear;
    procedure Refresh;
    procedure UpdateState;
    procedure UpdateTextSize;

    property Bounds: TRect read GetBounds write SetBounds;
    property Empty: Boolean read GetEmpty;
    property Visible: Boolean read GetVisible;
  end;

  { TdxPDFViewerBookmarkTreeView }

  TdxPDFViewerBookmarkTreeView = class(TdxPDFViewerNavigationPaneInternalControl)
  strict private type
  {$REGION 'private types'}
    TTree = class(TcxTreeView, IcxFontListener)
    strict private
      FFontSize: Integer;
      function GetViewer: TdxPDFCustomViewer;
      procedure Changed(Sender: TObject; AFont: TFont);
    protected
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

      procedure SetTextSize(ATextSize: TdxPDFViewerBookmarksTextSize);

      property Viewer: TdxPDFCustomViewer read GetViewer;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    end;
  {$ENDREGION}
  strict private
    function GetLookAndFeel: TcxLookAndFeel;
    function GetOutlineTree: TdxPDFOutlineTree;
    function GetSelectedOutline: TdxPDFOutlineTreeItem;
    function GetTreeView: TTree;

    function GetPrintPageNumbers(APrintSections: Boolean): TIntegerDynArray;
    procedure OnChangeHandler(Sender: TObject; Node: TTreeNode);
    procedure OnCustomDrawItemHandler(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure OnMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);

    property OutlineTree: TdxPDFOutlineTree read GetOutlineTree;
    property SelectedOutline: TdxPDFOutlineTreeItem read GetSelectedOutline;
    property TreeView: TTree read GetTreeView;
  strict protected
    function GetEmpty: Boolean; override;
    procedure ClearInternalControl; override;
    procedure CreateInternalControl; override;
    procedure InitializeInternalControl; override;
    procedure PopulateInternalControl; override;
    procedure UpdateInternalControlTextSize; override;
  protected
    function CanExpandSelectedBookmark: Boolean;
    function IsBookmarkSelected: Boolean;
    function IsTopLevelBookmarksExpanded: Boolean;
    procedure ExpandCollapseTopLevelBookmarks;
    procedure ExpandCurrentBookmark;
    procedure GoToBookmark;
    procedure PrintSelectedPages(APrintSections: Boolean);

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
  end;

  { TdxPDFViewerAttachmentFileList }

  TdxPDFViewerAttachmentFileList = class(TdxPDFViewerNavigationPaneInternalControl)
  strict private
    FImages: TcxImageList;
    FPrevItemIndex: Integer;

    function GetDocumentAttachments: TdxPDFFileAttachmentList;
    function GetView: TcxListView;
    function GetShowHint: Boolean;
    procedure SetShowHint(const AValue: Boolean);

    procedure OpenAttachment;
    procedure OnClickHandler(Sender: TObject);
    procedure OnContextPopupHandler(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure OnDblClickHandler(Sender: TObject);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnMouseLeaveHandler(Sender: TObject);
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  strict protected
    function GetEmpty: Boolean; override;
    procedure ClearInternalControl; override;
    procedure CreateInternalControl; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure InitializeInternalControl; override;
    procedure PopulateInternalControl; override;
    procedure UpdateInternalControlTextSize; override;
  protected
    property DocumentAttachments: TdxPDFFileAttachmentList read GetDocumentAttachments;
    property View: TcxListView read GetView;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

  { TdxPDFViewerNavigationPanePageCaptionButton }

  TdxPDFViewerNavigationPanePageCaptionButton = class(TdxPDFViewerButtonViewInfo)
  protected
    function CanFocus: Boolean; override;
    function GetGlyphAlignmentHorz: TAlignment; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure DrawButtonContent(ACanvas: TcxCanvas); override;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;

    function GetColorizeGlyph: Boolean; virtual;
  end;

  { TdxPDFViewerNavigationPanePageButtonViewInfo }

  TdxPDFViewerNavigationPanePageButtonViewInfo = class(TdxPDFViewerButtonViewInfo)
  strict private
    FPage: TdxPDFViewerNavigationPanePageViewInfo;
    function IsActive: Boolean;
    function IsFirst: Boolean;
  protected
    function CalculateState: TcxButtonState; override;
    function CanFocus: Boolean; override;
    function IsEnabled: Boolean; override;
    function GetClipRect: TRect; override;
    function GetGlyph: TdxSmartGlyph; override;
    function GetGlyphAlignmentHorz: TAlignment; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure DoExecute; override;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    procedure DrawButtonContent(ACanvas: TcxCanvas); override;
  public
    constructor Create(AController: TdxPDFViewerContainerController; APage: TdxPDFViewerNavigationPanePageViewInfo); reintroduce;
  end;

  { TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo }

  TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo = class(TdxPDFViewerCustomDropDownButtonViewInfo)
  protected
    OnGetPopupMenuClass: TComponentClass;
    function CanFocus: Boolean; override;
    function IsEnabled: Boolean; override;
    function GetColorizeGlyph: Boolean; override;
    function GetGlyph: TdxSmartGlyph; override;
    function GetHint: string; override;
    function GetPopupMenuClass: TComponentClass; override;
    function MeasureHeight: Integer; override;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
  end;

  { TdxPDFViewerNavigationPanePageToolBarViewInfo }

  TdxPDFViewerNavigationPanePageToolBarViewInfo = class(TdxPDFViewerContainerViewInfo)
  strict private
    FActualContentBounds: TRect;
    FOptionsButton: TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo;
  protected
    function GetContentMargins: TRect; override;
    function MeasureWidth: Integer; override;
    function HasOptionsButton: Boolean; virtual;
    procedure CreateCells; override;
    procedure DoCalculate; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;

    function GetPopupMenuClass: TComponentClass; virtual;

    property ActualContentBounds: TRect read FActualContentBounds;
  end;

  { TdxPDFViewerNavigationPanePageViewInfo }

  TdxPDFViewerNavigationPanePageViewInfoClass = class of TdxPDFViewerNavigationPanePageViewInfo;
  TdxPDFViewerNavigationPanePageViewInfo = class(TdxPDFViewerContainerViewInfo)
  strict protected type
  {$REGION 'private types'}
    THeaderOnGetTextEvent = function: string of object;

    TCaptionText = class(TdxPDFViewerCaptionViewInfo)
    protected
      OnGetText: THeaderOnGetTextEvent;
      function GetFont: TFont; override;
      function GetPainterTextColor: TColor; override;
      function GetText: string; override;
      function GetTextAlignment: TAlignment; override;
      procedure PrepareCanvas(ACanvas: TcxCanvas); override;
    end;

    TMinimizeButton  = class(TdxPDFViewerNavigationPanePageCaptionButton)
    protected
      function GetGlyph: TdxSmartGlyph; override;
      function GetHint: string; override;
      procedure DoExecute; override;
    end;

    TMaximizeButton  = class(TdxPDFViewerNavigationPanePageCaptionButton)
    protected
      function GetGlyph: TdxSmartGlyph; override;
      function GetHint: string; override;
      procedure DoExecute; override;
    end;

    THeader = class(TdxPDFViewerContainerViewInfo)
    strict private
      FCaption: TCaptionText;
      FMinimizeButton: TdxPDFViewerNavigationPanePageCaptionButton;
      FMaximizeButton: TMaximizeButton;
      function GetOnGetText: THeaderOnGetTextEvent;
      procedure SetOnGetText(const AValue: THeaderOnGetTextEvent);
      function AlignToClient(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
    protected
      function GetContentMargins: TRect; override;
      function MeasureHeight: Integer; override;
      function MeasureWidth: Integer; override;
      procedure CreateCells; override;
      procedure DoCalculate; override;
      procedure DrawBackground(ACanvas: TcxCanvas); override;

      property OnGetText: THeaderOnGetTextEvent read GetOnGetText write SetOnGetText;
    end;
  {$ENDREGION}
  strict private
    FCaption: THeader;
    FButton: TdxPDFViewerNavigationPanePageButtonViewInfo;
    FPage: TdxPDFViewerNavigationPanePage;
    function OnGetHeaderTextHandler: string;
  protected
    FActualContentBounds: TRect;
    FToolBar: TdxPDFViewerNavigationPanePageToolBarViewInfo;
    function GetGlyph: TdxSmartGlyph;
  protected
    function CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean; override;
    function MeasureWidth: Integer; override;
    procedure Calculate; override;
    procedure CreateCells; override;
    procedure DoCalculate; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;

    function GetToolbarClass: TdxPDFViewerCellViewInfoClass; virtual;
    function CanShow: Boolean;

    property Button: TdxPDFViewerNavigationPanePageButtonViewInfo read FButton;
    property Glyph: TdxSmartGlyph read GetGlyph;
    property Page: TdxPDFViewerNavigationPanePage read FPage;
  public
    constructor Create(AController: TdxPDFViewerContainerController; APage: TdxPDFViewerNavigationPanePage); reintroduce;
  end;

  { TdxPDFViewerBookmarksPageViewInfo }

  TdxPDFViewerBookmarksPageViewInfo = class(TdxPDFViewerNavigationPanePageViewInfo)
  strict private type
  {$REGION 'private types'}
    TExpandBookmarkButton =  class(TdxPDFViewerNavigationPanePageCaptionButton)
    protected
      function GetColorizeGlyph: Boolean; override;
      function GetGlyph: TdxSmartGlyph; override;
      function GetHint: string; override;
      function IsEnabled: Boolean; override;
      procedure DoExecute; override;
      procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    end;

    TBookmarksToolBar = class(TdxPDFViewerNavigationPanePageToolBarViewInfo)
    strict private
      FExpandButton: TExpandBookmarkButton;
    protected
      function GetPopupMenuClass: TComponentClass; override;
      procedure CreateCells; override;
      procedure DoCalculate; override;
    end;
  {$ENDREGION}
  strict private
    function GetOutlineTreeView: TdxPDFViewerBookmarkTreeView;
  protected
    function GetToolbarClass: TdxPDFViewerCellViewInfoClass; override;
    procedure Calculate; override;
    procedure CreateCells; override;

    property Tree: TdxPDFViewerBookmarkTreeView read GetOutlineTreeView;
  end;

  { TdxPDFViewerThumbnailsPageViewInfo }

  TdxPDFViewerThumbnailsPageViewInfo = class(TdxPDFViewerNavigationPanePageViewInfo)
  strict private type
  {$REGION 'private types'}
    TThumbnailsToolBar = class(TdxPDFViewerNavigationPanePageToolBarViewInfo)
    protected
      function GetPopupMenuClass: TComponentClass; override;
    end;
  {$ENDREGION}
  strict private
    function GetPreview: TdxPDFViewerPageThumbnailPreview;
    function GetZoomTrackBar: TcxTrackBar;
  protected
    function GetToolbarClass: TdxPDFViewerCellViewInfoClass; override;
    procedure Calculate; override;

    property Preview: TdxPDFViewerPageThumbnailPreview read GetPreview;
    property SizeTrackBar: TcxTrackBar read GetZoomTrackBar;
  end;

  { TdxPDFViewerAttachmentsPageViewInfo }

  TdxPDFViewerAttachmentsPageViewInfo = class(TdxPDFViewerNavigationPanePageViewInfo)
  strict private type
  {$REGION 'private types'}
    TOpenButton =  class(TdxPDFViewerNavigationPanePageCaptionButton)
    protected
      function GetColorizeGlyph: Boolean; override;
      function GetGlyph: TdxSmartGlyph; override;
      function GetHint: string; override;
      function IsEnabled: Boolean; override;
      procedure DoExecute; override;
      procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    end;

    TSaveButton =  class(TdxPDFViewerNavigationPanePageCaptionButton)
    protected
      function GetColorizeGlyph: Boolean; override;
      function GetGlyph: TdxSmartGlyph; override;
      function GetHint: string; override;
      function IsEnabled: Boolean; override;
      procedure DoExecute; override;
      procedure DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    end;

    TAttachmentsToolBar = class(TdxPDFViewerNavigationPanePageToolBarViewInfo)
    strict private
      FOpenButton: TOpenButton;
      FSaveButton: TSaveButton;
    protected
      function GetPopupMenuClass: TComponentClass; override;
      function HasOptionsButton: Boolean; override;
      procedure CreateCells; override;
      procedure DoCalculate; override;
    end;
  {$ENDREGION}
  strict private
    function GetFileList: TdxPDFViewerAttachmentFileList;
  protected
    function GetToolbarClass: TdxPDFViewerCellViewInfoClass; override;
    procedure Calculate; override;

    property FileList: TdxPDFViewerAttachmentFileList read GetFileList;
  end;

  { TdxPDFViewerPageThumbnailPreview }

  TdxPDFViewerPageThumbnailPreview = class(TdxPDFViewerNavigationPaneInternalControl)
  strict private type
    TThumbnailPreviewAccess = class(TdxPDFDocumentPageThumbnailViewer);
  strict private
    FLockCount: Integer;

    function GetLookAndFeel: TcxLookAndFeel;
    function GetMaxSize: Integer;
    function GetMinSize: Integer;
    function GetOnCustomDrawPreRenderPage: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent;
    function GetOnSizeChanged: TNotifyEvent;
    function GetPreview: TThumbnailPreviewAccess;
    function GetSelectedPageIndex: Integer;
    function GetSize: Integer;

    procedure SetMaxSize(const AValue: Integer);
    procedure SetMinSize(const AValue: Integer);
    procedure SetOnCustomDrawPreRenderPage(const AValue: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent);
    procedure SetOnSizeChanged(const AValue: TNotifyEvent);
    procedure SetSelectedPageIndex(const AValue: Integer);
    procedure SetSize(const AValue: Integer);
    procedure OnSelectedPageChangedHandler(Sender: TObject; APageIndex: Integer);

    property Preview: TThumbnailPreviewAccess read GetPreview;
  strict protected
    function GetEmpty: Boolean; override;
    procedure ClearInternalControl; override;
    procedure CreateInternalControl; override;
    procedure DestroySubClasses; override;
    procedure InitializeInternalControl; override;
    procedure PopulateInternalControl; override;
    procedure UpdateInternalControlTextSize; override;
  protected
    function Locked: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure PrintSelectedPages;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property MaxSize: Integer read GetMaxSize write SetMaxSize;
    property MinSize: Integer read GetMinSize write SetMinSize;
    property SelectedPageIndex: Integer read GetSelectedPageIndex write SetSelectedPageIndex;
    property Size: Integer read GetSize write SetSize;

    property OnCustomDrawPreRenderPage: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent read GetOnCustomDrawPreRenderPage
      write SetOnCustomDrawPreRenderPage;
    property OnSizeChanged: TNotifyEvent read GetOnSizeChanged write SetOnSizeChanged;
  end;

  { TdxPDFViewerNavigationPaneViewInfo }

  TdxPDFViewerNavigationPaneViewInfo = class(TdxPDFViewerContainerViewInfo)
  strict private
    FButtonsBounds: TRect;
    FNavigationPane: TdxPDFViewerNavigationPane;
    FPageBounds: TRect;
    FPageButtons: TList<TdxPDFViewerNavigationPanePageButtonViewInfo>;
    FSplitterBounds: TRect;
    FPages: TList<TdxPDFViewerNavigationPanePageViewInfo>;

    function GetActivePage: TdxPDFViewerNavigationPanePageViewInfo;
    function GetButtonsWidth: Integer;
    function GetFirstPageButton: TdxPDFViewerNavigationPanePageButtonViewInfo;
    function GetPageWidth: Integer;
    function ButtonMaxWidth: Integer;
    procedure AddPage(APage: TdxPDFViewerNavigationPanePage);
    procedure CalculateButtonsBounds;
    procedure CalculatePagesBounds;
    procedure CalculateSplitterBounds;
  protected
    function GetContentMargins: TRect; override;
    function GetIndentBetweenElements: Integer; override;
    function MeasureWidth: Integer; override;
    procedure ClearCells; override;
    procedure CreateSubClasses; override;
    procedure CreateCells; override;
    procedure DestroySubClasses; override;
    procedure DoCalculate; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawChildren(ACanvas: TcxCanvas); override;

    function GetPageIndex(AButton: TdxPDFViewerNavigationPanePageButtonViewInfo): Integer;

    property ActivePage: TdxPDFViewerNavigationPanePageViewInfo read GetActivePage;
    property ButtonsBounds: TRect read FButtonsBounds;
    property FirstPageButton: TdxPDFViewerNavigationPanePageButtonViewInfo read GetFirstPageButton;
    property SplitterBounds: TRect read FSplitterBounds;
  public
    constructor Create(AController: TdxPDFViewerContainerController; ANavigationPane: TdxPDFViewerNavigationPane); reintroduce;
  end;

  { TdxPDFViewerOptionsNavigationPage }

  TdxPDFViewerOptionsNavigationPage = class(TdxPDFViewerOptionsPersistent)
  strict private
    FPage: TdxPDFViewerNavigationPanePage;
    function GetGlyph: TdxSmartGlyph;
    function GetVisible: TdxDefaultBoolean;
    procedure SetGlyph(const AValue: TdxSmartGlyph);
    procedure SetVisible(const AValue: TdxDefaultBoolean);
  protected
    procedure DoAssign(ASource: TPersistent); override;
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property Visible: TdxDefaultBoolean read GetVisible write SetVisible;
  public
    constructor Create(AOwner: TPersistent; APage: TdxPDFViewerNavigationPanePage); reintroduce;
  end;

  { TdxPDFViewerOptionsBookmarks }

  TdxPDFViewerOptionsBookmarks = class(TdxPDFViewerOptionsNavigationPage)
  strict private
    FHideAfterUse: Boolean;
    FTextSize: TdxPDFViewerBookmarksTextSize;
    function GetShowEmpty: Boolean;
    procedure SetHideAfterUse(const AValue: Boolean);
    procedure SetTextSize(const AValue: TdxPDFViewerBookmarksTextSize);
  protected
    procedure DoAssign(ASource: TPersistent); override;
    property ShowEmpty: Boolean read GetShowEmpty;
  published
    property Glyph;
    property HideAfterUse: Boolean read FHideAfterUse write SetHideAfterUse default False;
    property TextSize: TdxPDFViewerBookmarksTextSize read FTextSize write SetTextSize default btsSmall;
    property Visible default bDefault;
  end;

  { TdxPDFViewerOptionsThumbnails }

  TdxPDFViewerOptionsThumbnails = class(TdxPDFViewerOptionsNavigationPage)
  published
    property Glyph;
    property Visible default bDefault;
  end;

  { TdxPDFViewerOptionsAttachments }

  TdxPDFViewerOptionsAttachments = class(TdxPDFViewerOptionsNavigationPage)
  published
    property Glyph;
    property Visible default bDefault;
  end;

  { TdxPDFViewerNavigationPanePage }

  TdxPDFViewerNavigationPanePage = class(TdxPDFViewerCustomObject)
  strict private
    FGlyph: TdxSmartGlyph;
    FOptions: TdxPDFViewerOptionsNavigationPage;
    FVisible: TdxDefaultBoolean;
    function GetBounds: TRect;
    function GetEmpty: Boolean;
    function GetGlyph: TdxSmartGlyph;
    procedure SetGlyph(const AValue: TdxSmartGlyph);
    procedure SetOptions(const AValue: TdxPDFViewerOptionsNavigationPage);
    procedure SetVisible(const AValue: TdxDefaultBoolean);
    procedure OnGlyphChangeHandler(Sender: TObject);
  private
    FControl: TdxPDFViewerNavigationPaneInternalControl;
    FDefaultGlyph: TdxSmartGlyph;
    FViewInfo: TdxPDFViewerNavigationPanePageViewInfo;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass; virtual; abstract;
    procedure LoadDefaultGlyphs; virtual; abstract;

    function CanShow: Boolean; virtual;
    function CreateOptions: TdxPDFViewerOptionsNavigationPage; virtual;
    procedure SetBounds(const AValue: TRect); virtual;

    function IsFirst: Boolean;
    procedure Clear;
    procedure Refresh;

    property Bounds: TRect read GetBounds write SetBounds;
    property Caption: string read GetCaption;
    property Empty: Boolean read GetEmpty;
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property Options: TdxPDFViewerOptionsNavigationPage read FOptions write SetOptions;
    property ViewInfo: TdxPDFViewerNavigationPanePageViewInfo read FViewInfo;
    property Visible: TdxDefaultBoolean read FVisible write SetVisible;
  end;

  { TdxPDFViewerThumbnails }

  TdxPDFViewerThumbnails = class(TdxPDFViewerNavigationPanePage)
  strict private
    FSizeTrackBar: TcxTrackBar;

    function GetShowHints: Boolean;
    function GetThumbnailPreview: TdxPDFViewerPageThumbnailPreview;
    procedure SetShowHints(const AValue: Boolean);

    procedure CreateSizeTrackBar;
    procedure OnSizeChangedHandler(Sender: TObject);
    procedure OnThumbnailSizeChangedHandler(Sender: TObject);
  protected
    function CanShow: Boolean; override;
    function CreateOptions: TdxPDFViewerOptionsNavigationPage; override;
    function CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo; override;
    function GetCaption: string; override;
    function GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure LoadDefaultGlyphs; override;
    procedure SetBounds(const AValue: TRect); override;

    function CanEnlargePageThumbnails: Boolean;
    function CanReducePageThumbnails: Boolean;
    procedure SynchronizeSelectedPage;

    property ShowHints: Boolean read GetShowHints write SetShowHints;
    property SizeTrackBar: TcxTrackBar read FSizeTrackBar;
    property ThumbnailPreview: TdxPDFViewerPageThumbnailPreview read GetThumbnailPreview;
  public
    procedure EnlargePageThumbnails;
    procedure PrintPages;
    procedure ReducePageThumbnails;
  end;

  { TdxPDFViewerOptionsNavigationPane }

  TdxPDFViewerOptionsNavigationPane = class(TdxPDFViewerOptionsPersistent)
  strict private
    FVisible: Boolean;
    function GetActivePage: TdxPDFViewerNavigationPaneActivePage;
    function GetActivePageState: TWindowState;
    function GetAttachments: TdxPDFViewerOptionsAttachments;
    function GetBookmarks: TdxPDFViewerOptionsBookmarks;
    function GetThumbnails: TdxPDFViewerOptionsThumbnails;
    procedure SetActivePage(const AValue: TdxPDFViewerNavigationPaneActivePage);
    procedure SetActivePageState(const AValue: TWindowState);
    procedure SetAttachments(const AValue: TdxPDFViewerOptionsAttachments);
    procedure SetBookmarks(const AValue: TdxPDFViewerOptionsBookmarks);
    procedure SetThumbnails(const AValue: TdxPDFViewerOptionsThumbnails);
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure DoAssign(ASource: TPersistent); override;
  public
    property ActivePage: TdxPDFViewerNavigationPaneActivePage read GetActivePage write SetActivePage default apNone;
    property ActivePageState: TWindowState read GetActivePageState write SetActivePageState default wsMinimized;
  published
    property Attachments: TdxPDFViewerOptionsAttachments read GetAttachments write SetAttachments;
    property Bookmarks: TdxPDFViewerOptionsBookmarks read GetBookmarks write SetBookmarks;
    property Thumbnails: TdxPDFViewerOptionsThumbnails read GetThumbnails write SetThumbnails;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TdxPDFViewerBookmarks }

  TdxPDFViewerBookmarks = class(TdxPDFViewerNavigationPanePage)
  strict private
    FExpandBookmarkGlyph: TdxSmartGlyph;
    function GetTree: TdxPDFViewerBookmarkTreeView;
  protected
    function CanShow: Boolean; override;
    function CreateOptions: TdxPDFViewerOptionsNavigationPage; override;
    function CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo; override;
    function GetCaption: string; override;
    function GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass; override;
    procedure DestroySubClasses; override;
    procedure LoadDefaultGlyphs; override;

    function CanExpandCurrentBookmark: Boolean;
    function IsBookmarkSelected: Boolean;
    function IsTopLevelBookmarksExpanded: Boolean;

    property ExpandBookmarkGlyph: TdxSmartGlyph read FExpandBookmarkGlyph;
    property Tree: TdxPDFViewerBookmarkTreeView read GetTree;
  public
    procedure ExpandCollapseTopLevelBookmarks;
    procedure ExpandCurrentBookmark;
    procedure GoToBookmark;
    procedure PrintPages;
    procedure PrintSections;
  end;

  { TdxPDFViewerAttachments }

  TdxPDFViewerAttachments = class(TdxPDFViewerNavigationPanePage)
  strict private
    FOpenAttachmentGlyph: TdxSmartGlyph;
    FSaveAttachmentGlyph: TdxSmartGlyph;
    FShowHints: Boolean;
    function GetFileList: TdxPDFViewerAttachmentFileList;
    function GetSelectedAttachment: TdxPDFFileAttachment;
    procedure SetShowHints(const AValue: Boolean);
  protected
    function CanShow: Boolean; override;
    function CreateOptions: TdxPDFViewerOptionsNavigationPage; override;
    function CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo; override;
    function GetCaption: string; override;
    function GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure LoadDefaultGlyphs; override;

    property FileList: TdxPDFViewerAttachmentFileList read GetFileList;
    property OpenAttachmentGlyph: TdxSmartGlyph read FOpenAttachmentGlyph;
    property SaveAttachmentGlyph: TdxSmartGlyph read FSaveAttachmentGlyph;
    property SelectedAttachment: TdxPDFFileAttachment read GetSelectedAttachment;
    property ShowHints: Boolean read FShowHints write SetShowHints;
  public
    function HasAttachments: Boolean;
    procedure OpenAttachment;
    procedure SaveAttachment;
  end;

  { TdxPDFViewerNavigationPane }

  TdxPDFViewerNavigationPane = class(TcxIUnknownObject, IcxFontListener)
  strict private
    FMaximizeButtonGlyph: TdxSmartGlyph;
    FMenuButtonGlyph: TdxSmartGlyph;
    FMinimizeButtonGlyph: TdxSmartGlyph;
    FRestoreButtonGlyph: TdxSmartGlyph;

    FActivePage: TdxPDFViewerNavigationPanePage;
    FActivePageState: TWindowState;
    FAttachments: TdxPDFViewerAttachments;
    FBookmarks: TdxPDFViewerBookmarks;
    FController: TdxPDFViewerNavigationPaneController;
    FFont: TFont;
    FHitTest: TdxPDFViewerNavigationPaneHitTest;
    FOptions: TdxPDFViewerOptionsNavigationPane;
    FPages: TObjectList<TdxPDFViewerNavigationPanePage>;
    FPageSize: Integer;
    FPrevActivePageState: TWindowState;
    FShowHints: Boolean;
    FThumbnails: TdxPDFViewerThumbnails;
    FViewer: TdxPDFCustomViewer;
    FViewInfo: TdxPDFViewerNavigationPaneViewInfo;
    FVisiblePages: TList<TdxPDFViewerNavigationPanePage>;

    function GetActivePageType: TdxPDFViewerNavigationPaneActivePage;
    function GetMaximizeButtonGlyph: TdxSmartGlyph;
    function GetVisiblePages: TList<TdxPDFViewerNavigationPanePage>;
    procedure SetActivePage(const AValue: TdxPDFViewerNavigationPanePage);
    procedure SetActivePageType(const AValue: TdxPDFViewerNavigationPaneActivePage);
    procedure SetActivePageState(const AValue: TWindowState);
    procedure SetOptions(const AValue: TdxPDFViewerOptionsNavigationPane);
    procedure SetPageSize(const AValue: Integer);
    procedure SetShowHints(const AValue: Boolean);
    procedure Changed(Sender: TObject; AFont: TFont); overload;
    procedure LoadGlyphs;
    procedure UpdateVisiblePages;
  protected
    function CanShow: Boolean;
    function CalculateParentClientBounds(const AClientRect: TRect): TRect;
    function IsFirst(APage: TdxPDFViewerNavigationPanePage): Boolean;
    function IsVisible(APage: TdxPDFViewerNavigationPanePage): Boolean;
    function IsMaximized: Boolean;
    function MeasureWidth: Integer;
    procedure AddPages;
    procedure ActivatePage;
    procedure Calculate(var ABounds: TRect);
    procedure Changed; overload;
    procedure Clear;
    procedure MaximizePage;
    procedure MinimizePage;
    procedure RestorePage;
    procedure Refresh;
    procedure VisibilityChanged(APage: TdxPDFViewerNavigationPanePage);

    property Controller: TdxPDFViewerNavigationPaneController read FController;
    property HitTest: TdxPDFViewerNavigationPaneHitTest read FHitTest;
    property ViewInfo: TdxPDFViewerNavigationPaneViewInfo read FViewInfo;

    property MaximizeButtonGlyph: TdxSmartGlyph read GetMaximizeButtonGlyph;
    property MenuButtonGlyph: TdxSmartGlyph read FMenuButtonGlyph;
    property MinimizeButtonGlyph: TdxSmartGlyph read FMinimizeButtonGlyph;
    property RestoreButtonGlyph: TdxSmartGlyph read FRestoreButtonGlyph;

    property ActivePage: TdxPDFViewerNavigationPanePage read FActivePage write SetActivePage;
    property ActivePageState: TWindowState read FActivePageState write SetActivePageState;
    property ActivePageType: TdxPDFViewerNavigationPaneActivePage read GetActivePageType write SetActivePageType;
    property Attachments: TdxPDFViewerAttachments read FAttachments;
    property Bookmarks: TdxPDFViewerBookmarks read FBookmarks;
    property Font: TFont read FFont;
    property Options: TdxPDFViewerOptionsNavigationPane read FOptions write SetOptions;
    property Pages: TObjectList<TdxPDFViewerNavigationPanePage> read FPages;
    property PageSize: Integer read FPageSize write SetPageSize;
    property ShowHints: Boolean read FShowHints write SetShowHints;
    property Thumbnails: TdxPDFViewerThumbnails read FThumbnails;
    property VisiblePages: TList<TdxPDFViewerNavigationPanePage> read GetVisiblePages;
  public
    constructor Create(AViewer: TdxPDFCustomViewer);
    destructor Destroy; override;
  end;

procedure ShowPrintDialog(AViewer: TdxPDFCustomViewer);

implementation

{$R dxPDFViewer.res}

uses
  Math, Clipbrd, Variants, Contnrs, Dialogs, ShellApi, IOUtils, dxTypeHelpers, cxLibraryConsts, dxPrinting,
  dxGDIPlusAPI, cxEdit, cxContainer, cxSplitter, dxPDFBase, dxPDFUtils, dxPDFViewerPopupMenu, dxPDFViewerPasswordDialog,
  dxPDFViewerDialogsStrs;

const
  dxPDFViewerViewCreationTimeOut = 2000;

const
  dxPDFViewerFindPanelContentMargins: TRect = (Left: 9; Top: 9; Right: 9; Bottom: 9);
  dxPDFViewerFindPanelDefaultButtonWidth = 79;
  dxPDFViewerFindPanelEditMaxWidth = 150;
  dxPDFViewerFindPanelEditMinWidth = 40;
  dxPDFViewerIndentBetweenElements = 6;
  dxPDFViewerNavigationPaneContentMargins: TRect = (Left: 2; Top: 2; Right: 2; Bottom: 2);
  dxPDFViewerNavigationPageToolbarButtonHeight = 26;
  dxPDFViewerBookmarksPageContentMargins: TRect = (Left: 7; Top: 7; Right: 7; Bottom: 7);

type
  TcxScrollingControlAccess = class(TcxScrollingControl);
  TdxCustomPreviewAccess = class(TdxCustomPreview);
  TdxPDFAcroFormActionFieldAccess = class(TdxPDFAcroFormActionField);
  TdxPDFCustomActionAccess = class(TdxPDFCustomAction);
  TdxPDFCustomSelectionAccess = class(TdxPDFCustomSelection);
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFDocumentSequentialTextSearchAccess = class(TdxPDFDocumentSequentialTextSearch);
  TdxPDFDocumentViewerCustomRendererAccess = class(  TdxPDFDocumentViewerCustomRenderer);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFImageSelectionAccess = class(TdxPDFImageSelection);
  TdxPDFFileAttachmentAccess = class(TdxPDFFileAttachment);
  TdxPDFFileAttachmentAnnotationFieldAccess = class(  TdxPDFFileAttachmentAnnotationField);
  TdxPDFHyperlinkAccess = class(TdxPDFHyperlink);
  TdxPDFOutlineTreeAccess = class(TdxPDFOutlineTree);
  TdxPDFOutlineTreeItemAccess = class(TdxPDFOutlineTreeItem);
  TdxPDFPageAccess = class(TdxPDFPage);
  TdxPDFRecognizedImageAccess = class(TdxPDFImage);
  TdxPDFTextHighlightAccess = class(TdxPDFTextHighlight);
  TdxPDFTextLineAccess = class(TdxPDFTextLine);
  TdxPDFTextObjectAccess = class(TdxPDFTextObject);
  TdxPDFTextWordAccess = class(TdxPDFTextWord);
  TdxPDFTextWordPartAccess = class(TdxPDFTextWordPart);
  TdxPDFViewerAccess = class(TdxPDFCustomViewer);
  TdxPDFViewerDocumentStateAccess = class(TdxPDFViewerDocumentState);
  TdxPDFViewerPopupMenuAccess = class(TdxPDFViewerCustomPopupMenu);
  TdxSplitterDragImageAccess = class(TdxSplitterDragImage);

  { TdxPDFViewerResizeNavigationPaneDragAndDropObject }

  TdxPDFViewerResizeNavigationPaneDragAndDropObject = class(TcxDragAndDropObject)
  strict private
    function GetDragDropArea: TRect;
    function GetDragImageBounds: TRect;
    function GetPadding: Integer;
    function GetViewer: TdxPDFCustomViewer;
  protected
    FDragImage: TdxSplitterDragImage;
    FStartMousePos: TPoint;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    property Viewer: TdxPDFCustomViewer read GetViewer;
  public
    destructor Destroy; override;
  end;

{ TdxPDFViewerResizeNavigationPaneDragAndDropObject }

destructor TdxPDFViewerResizeNavigationPaneDragAndDropObject.Destroy;
begin
  FreeAndNil(FDragImage);
  inherited Destroy;
end;

procedure TdxPDFViewerResizeNavigationPaneDragAndDropObject.BeginDragAndDrop;
begin
  FStartMousePos := GetMouseCursorPos;
  FDragImage := TdxSplitterDragImage.Create;
  TdxSplitterDragImageAccess(FDragImage).Canvas.Brush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  Viewer.NavigationPane.RestorePage;
  FDragImage.BoundsRect := GetDragImageBounds;
  FDragImage.Show;
end;

procedure TdxPDFViewerResizeNavigationPaneDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  ADragDropArea: TRect;
begin
  ADragDropArea := GetDragDropArea;
  Accepted := InRange(P.X, ADragDropArea.Left, ADragDropArea.Right);
  FDragImage.BoundsRect := GetDragImageBounds;
  inherited;
end;

procedure TdxPDFViewerResizeNavigationPaneDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
const
  Sign: array[Boolean] of Integer = (-1, 1);
var
  R: TRect;
begin
  if Accepted then
  begin
    R := cxRectOffset(GetDragImageBounds, Control.ClientToScreen(cxNullPoint), False);
    if R.Right = Viewer.ClientBounds.Right then
      Viewer.NavigationPane.MaximizePage
    else
      if R.Left = Viewer.ViewInfo.NavigationPane.ButtonsBounds.Right then
        Viewer.NavigationPane.MinimizePage
      else
        Viewer.NavigationPane.PageSize := Viewer.NavigationPane.PageSize +
          (R.Left - Viewer.ViewInfo.NavigationPane.SplitterBounds.Left) * Sign[True];
  end;
  inherited EndDragAndDrop(Accepted);
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetDragDropArea: TRect;
var
  APadding: Integer;
begin
  APadding := GetPadding;
  Result.Left := Viewer.ViewInfo.NavigationPane.ButtonsBounds.Right + APadding;
  Result.Right := Viewer.ClientBounds.Right - APadding;
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := crcxHorzSize;
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetDragImageBounds: TRect;
var
  R: TRect;
  AOffset: TPoint;
begin
  AOffset := cxPointOffset(GetMouseCursorPos, FStartMousePos, False);
  Result := cxRectOffset(Viewer.ViewInfo.NavigationPane.SplitterBounds, Control.ClientToScreen(cxNullPoint));
  R := cxRectOffset(GetDragDropArea, Control.ClientToScreen(cxNullPoint));
  Result := cxRectOffset(Result, AOffset.X, 0);
  if Result.Left < R.Left then
    Result := cxRectSetLeft(Result, R.Left - GetPadding)
  else
    if Result.Right > R.Right then
      Result := cxRectSetRight(Result, R.Right + GetPadding);
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetPadding: Integer;
begin
  Result := Viewer.ScaleFactor.Apply(50);
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetViewer: TdxPDFCustomViewer;
begin
  Result := TdxPDFCustomViewer(Control);
end;

function TdxPDFViewerResizeNavigationPaneDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

procedure ShowPrintDialog(AViewer: TdxPDFCustomViewer);
begin
  dxPrintingRepository.PrintReport(AViewer);
end;

{ TdxPDFViewerOptionsPersistent }

constructor TdxPDFViewerOptionsPersistent.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Initialize;
end;

procedure TdxPDFViewerOptionsPersistent.Assign(Source: TPersistent);
begin
  if Source is TcxOwnedPersistent then
  begin
    Viewer.BeginUpdate;
    try
      DoAssign(Source);
    finally
      Viewer.EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TdxPDFViewerOptionsPersistent.Initialize;
begin
// do nothing
end;

procedure TdxPDFViewerOptionsPersistent.Changed;
begin
  Changed(ctLight);
end;

procedure TdxPDFViewerOptionsPersistent.Changed(AType: TdxChangeType);
begin
  Viewer.LayoutChanged(AType);
end;

function TdxPDFViewerOptionsPersistent.GetViewer: TdxPDFCustomViewer;
begin
  Result := Owner as TdxPDFCustomViewer;
end;

{ TdxPDFViewerOptionsBehavior }

constructor TdxPDFViewerOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FShowHints := True;
end;

procedure TdxPDFViewerOptionsBehavior.DoAssign(ASource: TPersistent);
begin
  RenderContentDelay := TdxPDFViewerOptionsBehavior(ASource).RenderContentDelay;
  RenderContentInBackground := TdxPDFViewerOptionsBehavior(ASource).RenderContentInBackground;
end;

function TdxPDFViewerOptionsBehavior.GetRenderContentDelay: Integer;
begin
  Result := Viewer.RenderContentDelay;
end;

function TdxPDFViewerOptionsBehavior.GetRenderContentInBackground: Boolean;
begin
  Result := Viewer.RenderContentInBackground;
end;

procedure TdxPDFViewerOptionsBehavior.SetRenderContentDelay(const AValue: Integer);
begin
  Viewer.RenderContentDelay := AValue;
end;

procedure TdxPDFViewerOptionsBehavior.SetRenderContentInBackground(const AValue: Boolean);
begin
  Viewer.RenderContentInBackground := AValue;
end;

procedure TdxPDFViewerOptionsBehavior.SetShowHints(const AValue: Boolean);
begin
  if FShowHints <> AValue then
  begin
    FShowHints := AValue;
    Viewer.NavigationPane.ShowHints := FShowHints;
    Changed;
  end;
end;

{ TdxPDFViewerOptionsZoom }

procedure TdxPDFViewerOptionsZoom.DoAssign(ASource: TPersistent);
begin
  ZoomFactor := TdxPDFViewerOptionsZoom(ASource).ZoomFactor;
  MaxZoomFactor := TdxPDFViewerOptionsZoom(ASource).MaxZoomFactor;
  MinZoomFactor := TdxPDFViewerOptionsZoom(ASource).MinZoomFactor;
end;

function TdxPDFViewerOptionsZoom.GetMaxZoomFactor: Integer;
begin
  Result := Viewer.MaxZoomFactor;
end;

function TdxPDFViewerOptionsZoom.GetMinZoomFactor: Integer;
begin
  Result := Viewer.MinZoomFactor;
end;

function TdxPDFViewerOptionsZoom.GetZoomFactor: Integer;
begin
  Result := Viewer.ZoomFactor;
end;

function TdxPDFViewerOptionsZoom.GetZoomMode: TdxPreviewZoomMode;
begin
  Result := Viewer.ZoomMode;
end;

function TdxPDFViewerOptionsZoom.GetZoomStep: Integer;
begin
  Result := Viewer.ZoomStep;
end;

procedure TdxPDFViewerOptionsZoom.SetMaxZoomFactor(const AValue: Integer);
begin
  Viewer.MaxZoomFactor := AValue;
end;

procedure TdxPDFViewerOptionsZoom.SetMinZoomFactor(const AValue: Integer);
begin
  Viewer.MinZoomFactor := AValue;
end;

procedure TdxPDFViewerOptionsZoom.SetZoomFactor(const AValue: Integer);
begin
  Viewer.ZoomFactor := AValue;
end;

procedure TdxPDFViewerOptionsZoom.SetZoomMode(const AValue: TdxPreviewZoomMode);
begin
  Viewer.ZoomMode := AValue;
end;

procedure TdxPDFViewerOptionsZoom.SetZoomStep(const AValue: Integer);
begin
  Viewer.ZoomStep := AValue;
end;

{ TdxPDFViewerFindPanelTextEdit }

constructor TdxPDFViewerFindPanelTextEdit.Create(AFindPanel: TdxPDFViewerFindPanel);
begin
  inherited Create(AFindPanel.Viewer);
  FFindPanel := AFindPanel;
  Parent := AFindPanel.Viewer;
  AutoSize := False;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  DoubleBuffered := False;
  LookAndFeel.MasterLookAndFeel := AFindPanel.Viewer.LookAndFeel;
  Properties.BeepOnError := False;
  Visible := False;
end;

function TdxPDFViewerFindPanelTextEdit.GetInnerEditClass: TControlClass;
begin
  Result := TdxPDFViewerFindPanelInnerTextEdit;
end;

function TdxPDFViewerFindPanelTextEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if PtInRect(FFindPanel.ViewInfo.FindPanel.Edit.Bounds, GetMouseCursorClientPos) then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := TdxPDFViewerAccess(FFindPanel.Viewer).ProcessMouseWheelMessage(WheelDelta);
end;

procedure TdxPDFViewerFindPanelTextEdit.FocusChanged;
begin
  if csDestroying in Parent.ComponentState then
    Exit;
  inherited FocusChanged;
  if Focused or FFindPanel.Viewer.ViewInfo.FindPanel.Edit.IsFocused then
  begin
    if Focused then
      FFindPanel.Viewer.ViewerController.FocusedCell := FFindPanel.Viewer.ViewInfo.FindPanel.Edit
    else
      FFindPanel.Viewer.ViewerController.FocusedCell := FFindPanel.Viewer.ViewInfo;
    TdxPDFViewerAccess(FFindPanel.Viewer).FocusChanged;
  end;
end;

{ TdxPDFViewerFindPanelInnerTextEdit }

procedure TdxPDFViewerFindPanelInnerTextEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

{ TdxPDFViewerCustomObject }

constructor TdxPDFViewerCustomObject.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
  CreateSubClasses;
end;

destructor TdxPDFViewerCustomObject.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFViewerCustomObject.CreateSubClasses;
begin
// do nothing
end;

procedure TdxPDFViewerCustomObject.DestroySubClasses;
begin
// do nothing
end;

function TdxPDFViewerCustomObject.GetScaleFactor: TdxScaleFactor;
begin
  Result := Viewer.ScaleFactor;
end;

function TdxPDFViewerCustomObject.GetViewInfo: TdxPDFViewerViewInfo;
begin
  Result := Viewer.ViewInfo;
end;

{ TdxPDFViewerOptionsFinPanel }

procedure TdxPDFViewerOptionsFindPanel.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);
  FAlignment := TdxPDFViewerOptionsFindPanel(ASource).Alignment;
  FAnimationTime := TdxPDFViewerOptionsFindPanel(ASource).AnimationTime;
  FAnimation := TdxPDFViewerOptionsFindPanel(ASource).Animation;
  FShowCloseButton := TdxPDFViewerOptionsFindPanel(ASource).ShowCloseButton;
  FShowPreviousButton := TdxPDFViewerOptionsFindPanel(ASource).ShowPreviousButton;
  FShowOptionsButton := TdxPDFViewerOptionsFindPanel(ASource).ShowOptionsButton;
  FShowNextButton := TdxPDFViewerOptionsFindPanel(ASource).ShowNextButton;
end;

procedure TdxPDFViewerOptionsFindPanel.Initialize;
begin
  inherited Initialize;
  FAnimationTime := dxPDFViewerFindPanelDefaultAnimationTime;
  FShowCloseButton := True;
  FShowNextButton := True;
  FShowOptionsButton := True;
  FShowPreviousButton := True;
end;

function TdxPDFViewerOptionsFindPanel.GetCaseSensitive: Boolean;
begin
  Result := FOptions.CaseSensitive;
end;

function TdxPDFViewerOptionsFindPanel.GetDirection: TdxPDFDocumentTextSearchDirection;
begin
  Result := FOptions.Direction;
end;

function TdxPDFViewerOptionsFindPanel.GetWholeWords: Boolean;
begin
  Result := FOptions.WholeWords;
end;

procedure TdxPDFViewerOptionsFindPanel.SetCaseSensitive(const AValue: Boolean);
begin
  if FOptions.CaseSensitive <> AValue then
  begin
    FOptions.CaseSensitive := AValue;
    ClearTextSearch;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetDirection(const AValue: TdxPDFDocumentTextSearchDirection);
begin
  FOptions.Direction := AValue;
end;

procedure TdxPDFViewerOptionsFindPanel.SetSearchString(const AValue: string);
begin
  if FSearchString <> AValue then
  begin
    FSearchString := AValue;
    ClearTextSearch;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetWholeWords(const AValue: Boolean);
begin
  if FOptions.WholeWords <> AValue then
  begin
    FOptions.WholeWords := AValue;
    ClearTextSearch;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.ClearTextSearch;
begin
  if HighlightSearchResults then
    Viewer.TextSearch.Clear;
end;

procedure TdxPDFViewerOptionsFindPanel.SetAlignment(const AValue: TdxPDFViewerFindPanelAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetAnimation(const AValue: TdxPDFViewerFindPanelAnimation);
begin
  if FAnimation <> AValue then
  begin
    FAnimation := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetAnimationTime(const AValue: Integer);
begin
  if FAnimationTime <> AValue then
  begin
    FAnimationTime := Max(AValue, 0);
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetClearSearchStringOnClose(const AValue: Boolean);
begin
  if FClearSearchStringOnClose <> AValue then
  begin
    FClearSearchStringOnClose := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetDisplayMode(const AValue: TdxPDFViewerFindPanelDisplayMode);
const
  VisibilityMap: array[fpdmAlways..fpdmNever] of Boolean = (True, False);
begin
  if FDisplayMode <> AValue then
  begin
    FDisplayMode := AValue;
    Changed;
    if not Viewer.IsDesigning then
    begin
      if DisplayMode in [fpdmNever, fpdmAlways] then
        Viewer.FindPanel.Visible := VisibilityMap[DisplayMode] and Viewer.IsDocumentLoaded
    end
    else
      Viewer.FindPanel.Visible := (DisplayMode = fpdmAlways) and Viewer.IsDocumentLoaded;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetHighlightSearchResults(const AValue: Boolean);
begin
  if FHighlightSearchResults <> AValue then
  begin
    FHighlightSearchResults := AValue;
    ClearTextSearch;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetShowCloseButton(const AValue: Boolean);
begin
  if FShowCloseButton <> AValue then
  begin
    FShowCloseButton := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetShowNextButton(const AValue: Boolean);
begin
  if FShowNextButton <> AValue then
  begin
    FShowNextButton := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetShowOptionsButton(const AValue: Boolean);
begin
  if FShowOptionsButton <> AValue then
  begin
    FShowOptionsButton := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsFindPanel.SetShowPreviousButton(const AValue: Boolean);
begin
  if FShowPreviousButton <> AValue then
  begin
    FShowPreviousButton := AValue;
    Changed;
  end;
end;

{ TdxPDFViewerFindPanel }

constructor TdxPDFViewerFindPanel.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create(AViewer);
  FOptions := TdxPDFViewerOptionsFindPanel.Create(Viewer);
  FOptionsButtonGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERFINDPANELOPTIONSBUTTON', 'PNG');
  CreateAnimationController;
  Edit := CreateEdit;
end;

destructor TdxPDFViewerFindPanel.Destroy;
begin
  Edit := nil;
  FreeAndNil(FOptions);
  FreeAndNil(FAnimationController);
  FreeAndNil(FOptionsButtonGlyph);
  inherited Destroy;
end;

procedure TdxPDFViewerFindPanel.CreateAnimationController;
begin
  FAnimationController := TdxPDFViewerFindPanelAnimationController.Create(Viewer);
end;

function TdxPDFViewerFindPanel.IsLocked: Boolean;
begin
  Result := FLockCount <> 0;
end;

procedure TdxPDFViewerFindPanel.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxPDFViewerFindPanel.Changed;
begin
  Viewer.LayoutChanged(ctLight);
end;

procedure TdxPDFViewerFindPanel.EndUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxPDFViewerFindPanel.HideBeep(var AKey: Char);
begin
  if Ord(AKey) in [13, 27] then
    AKey := #0;
end;

procedure TdxPDFViewerFindPanel.Find;
begin
  if not Viewer.TextSearch.IsLocked and (Trim(Edit.EditingValue) <> '') then
  begin
    UpdateSearchString;
    Viewer.OptionsFindPanel.Direction := tsdForward;
    Viewer.ViewerController.DoFindText;
  end;
end;

procedure TdxPDFViewerFindPanel.SetEdit(const AValue: TdxPDFViewerFindPanelTextEdit);
begin
  InternalSetEdit(AValue);
end;

procedure TdxPDFViewerFindPanel.SetOptions(const AValue: TdxPDFViewerOptionsFindPanel);
begin
  FOptions.Assign(AValue);
end;

procedure TdxPDFViewerFindPanel.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    AnimationController.Animate(AValue);
    Edit.Visible := FVisible;
    Viewer.BeginUpdate;
    try
      if not FVisible then
      begin
        Viewer.ViewerController.FocusedCell := Viewer.ViewInfo;
        if Options.ClearSearchStringOnClose then
          Edit.EditValue := '';
      end
      else
      begin
        Edit.EditValue := Viewer.OptionsFindPanel.SearchString;
        Viewer.ViewerController.FocusedCell := Viewer.ViewInfo.FindPanel.Edit;
      end;
    finally
      Viewer.CancelUpdate;
    end;
    Changed;
  end;
end;

function TdxPDFViewerFindPanel.CreateEdit: TdxPDFViewerFindPanelTextEdit;
begin
  Result := TdxPDFViewerFindPanelTextEdit.Create(Self);
  Result.EditValue := FOptions.SearchString;
  Result.OnKeyDown := EditKeyDownHandler;
  Result.OnKeyPress := EditKeyPressHandler;
  Result.Properties.OnChange := EditValueChangedHandler;
  Result.Properties.OnEditValueChanged := EditValueChangedHandler;
end;

procedure TdxPDFViewerFindPanel.DestroyEdit;
begin
  FreeAndNil(FEdit);
end;

procedure TdxPDFViewerFindPanel.InternalSetEdit(const AValue: TdxPDFViewerFindPanelTextEdit);
begin
  if FEdit <> AValue then
  begin
    if FEdit <> nil then
    begin
      if AValue <> nil then
        AValue.Properties.LookupItems.Assign(FEdit.Properties.LookupItems);
      DestroyEdit;
    end;
    FEdit := AValue;
  end;
end;

procedure TdxPDFViewerFindPanel.UpdateSearchString;
begin
  Options.SearchString := Edit.EditingValue;
  ViewInfo.FindPanel.Update;
end;

procedure TdxPDFViewerFindPanel.EditKeyPressHandler(Sender: TObject; var Key: Char);
begin
  HideBeep(Key);
end;

procedure TdxPDFViewerFindPanel.EditKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_F3] then
  begin
    Find;
    Key := 0;
  end
  else
    Viewer.KeyDown(Key, Shift);
end;

procedure TdxPDFViewerFindPanel.EditValueChangedHandler(Sender: TObject);
begin
  UpdateSearchString;
end;

{ TdxPDFViewerPage }

procedure TdxPDFViewerPage.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.Lock;
  try
    DrawBackground(ACanvas);
    DrawContent(ACanvas);
    DrawHighlights(ACanvas);
    DrawSelection(ACanvas);
    DrawFocusedAnnotation(ACanvas);
  finally
    ACanvas.Unlock;
  end;
end;

function TdxPDFViewerPage.GetVisible: Boolean;
begin
  Result := inherited GetVisible or cxRectIntersect(Bounds, TdxPDFViewerAccess(Preview).GetPagesArea);
end;

procedure TdxPDFViewerPage.CalculatePageSize;
begin
  PageSize.Size := GetPageSize(TdxPDFViewerAccess(Preview).DocumentToViewerFactor);
  PageSize.MinUsefulSize := PageSize.Size;
end;

function TdxPDFViewerPage.CreatePath(ASelection: TdxPDFTextHighlight; AExcludeTextSelection: Boolean = False): TdxGPPath;

  function IsTextSelection: Boolean;
  begin
    Result := not Viewer.Selection.IsEmpty and (Viewer.Selection.Selection.HitCode = hcTextSelection);
  end;

  function InflateSelectionRect(const ARect: TdxRectF): TdxRectF;
  begin
    Result := cxRectInflate(ARect, DocumentState.ScaleFactor.X, DocumentState.ScaleFactor.X);
  end;

var
  R: TdxRectF;
  AHighlights: TList<TdxRectF>;
  ATextSelectionRects: TList<TdxRectF>;
begin
  Result := TdxGPPath.Create(gpfmWinding);
  AHighlights := TdxPDFTextHighlightAccess(ASelection).PageRects[Index];
  if AHighlights <> nil then
  begin
    if IsTextSelection then
      ATextSelectionRects := TdxPDFTextHighlightAccess(Viewer.Selection.Selection).PageRects[Index]
    else
      ATextSelectionRects := nil;
    Result.FigureStart;
    try
      for R in AHighlights do
        if not AExcludeTextSelection or ((ATextSelectionRects <> nil) and not ATextSelectionRects.Contains(R) or
          (ATextSelectionRects = nil)) then
          Result.AddRect(InflateSelectionRect(ToViewerRect(R)));
    finally
      Result.FigureFinish;
    end;
  end;
end;

function TdxPDFViewerPage.GetPainter: TdxPDFViewerPainter;
begin
  Result := Viewer.Painter;
end;

function TdxPDFViewerPage.GetSelectionBackColor(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  if AColor = dxacDefault then
    Result := Painter.SelectionBackColor
  else
    Result := AColor;
end;

function TdxPDFViewerPage.GetSelectionFrameColor(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  if AColor = dxacDefault then
    Result := Painter.SelectionFrameColor
  else
    Result := AColor;
end;

function TdxPDFViewerPage.GetViewer: TdxPDFCustomViewer;
begin
  Result := Preview as TdxPDFCustomViewer;
end;

procedure TdxPDFViewerPage.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawPageBackground(ACanvas, SiteBounds, Bounds, Selected);
end;

procedure TdxPDFViewerPage.DrawContent(ACanvas: TcxCanvas);
begin
  TdxPDFDocumentViewerCustomRendererAccess(Viewer.Renderer).DrawPage(ACanvas, Index, Bounds);
end;

procedure TdxPDFViewerPage.DrawFocusedAnnotation(ACanvas: TcxCanvas);
var
  AIntf: IdxPDFInteractiveObject;
  ARect: TRect;
begin
  if (Viewer.ViewerController.FocusedField <> nil) and
    Supports(Viewer.ViewerController.FocusedField, IdxPDFInteractiveObject, AIntf) then
  begin
    ARect := DocumentState.ToViewerRect(AIntf);
    if cxRectIntersect(Bounds, ARect) then
      ACanvas.DrawFocusRect(ARect);
  end;
end;

procedure TdxPDFViewerPage.DrawHighlights(ACanvas: TcxCanvas);

  function GetBackColor(AColor: TdxAlphaColor): TdxAlphaColor;
  begin
    if AColor = dxacDefault then
      Result := Painter.HighlightBackColor
    else
      Result := AColor;
  end;

  function GetFrameColor(AColor: TdxAlphaColor): TdxAlphaColor;
  begin
    if AColor = dxacDefault then
      Result := Painter.HighlightFrameColor
    else
      Result := AColor;
  end;

  procedure DrawHighlight(ACanvas: TcxCanvas; AHighlight: TdxPDFTextHighlight);
  var
    APath: TdxGPPath;
  begin
    APath := CreatePath(AHighlight, True);
    try
      DrawPath(ACanvas, APath, GetBackColor(TdxPDFTextHighlightAccess(AHighlight).BackColor),
        GetFrameColor(TdxPDFTextHighlightAccess(AHighlight).FrameColor));
    finally
      APath.Free;
    end;
  end;

  function NeedDraw: Boolean;
  begin
   Result := (Viewer.Highlights.Visible = bTrue) or (Viewer.Highlights.Visible = bDefault) and Viewer.IsFindPanelVisible;
  end;

var
  AHighlight: TdxPDFTextHighlight;
begin
  if NeedDraw then
    for AHighlight in Viewer.Highlights.Items do
      DrawHighlight(ACanvas, AHighlight);
end;

procedure TdxPDFViewerPage.DrawImageSelection(ACanvas: TcxCanvas; ASelection: TdxPDFImageSelection);
var
  APath: TdxGPPath;
begin
  if TdxPDFImageSelectionAccess(ASelection).PageIndex = Index then
  begin
   APath := TdxGPPath.Create(gpfmWinding);
   try
     APath.FigureStart;
     APath.AddRect(ToViewerRect(TdxPDFImageSelection(ASelection).Bounds));
     APath.FigureFinish;
     DrawPath(ACanvas, APath, GetSelectionBackColor(ASelection.BackColor), GetSelectionFrameColor(ASelection.FrameColor));
   finally
      APath.Free;
   end;
  end;
end;

procedure TdxPDFViewerPage.DrawPath(ACanvas: TcxCanvas; APath: TdxGPPath; AColor, AFrameColor: TdxAlphaColor);
var
  AGraphics: TdxGPGraphics;
  ARegion: TdxGPRegion;
begin
  AGraphics := TdxGPGraphics.Create(ACanvas.Handle);
  ARegion := TdxGPRegion.Create;
  ARegion.CombineRegionRect(Bounds, gmIntersect);
  AGraphics.SaveClipRegion;
  try
    AGraphics.SetClipRegion(ARegion, gmIntersect);
    AGraphics.Path(APath, AFrameColor, AColor);
  finally
    AGraphics.RestoreClipRegion;
    ARegion.Free;
    AGraphics.Free;
  end;
end;

procedure TdxPDFViewerPage.DrawSelection(ACanvas: TcxCanvas);
var
  ASelection: TdxPDFCustomSelection;
begin
  ASelection := Viewer.Selection.Selection;
  if ASelection <> nil then
    case ASelection.HitCode of
      hcImage:
        DrawImageSelection(ACanvas, TdxPDFImageSelection(ASelection));
      hcTextSelection:
        DrawTextSelection(ACanvas, TdxPDFTextSelection(ASelection));
    end;
end;

procedure TdxPDFViewerPage.DrawTextSelection(ACanvas: TcxCanvas; ASelection: TdxPDFTextSelection);
var
  APath: TdxGPPath;
begin
  APath := CreatePath(ASelection);
  try
    DrawPath(ACanvas, APath, GetSelectionBackColor(ASelection.BackColor), GetSelectionFrameColor(ASelection.FrameColor));
  finally
    APath.Free;
  end;
end;

{ TdxPDFViewerPainter }

function TdxPDFViewerPainter.ButtonSymbolColor(AState: TcxButtonState): TColor;
begin
  Result := LookAndFeelPainter.ButtonSymbolColor(AState);
end;

function TdxPDFViewerPainter.ButtonTextShift: Integer;
begin
  Result := LookAndFeelPainter.ScaledButtonTextShift(ScaleFactor);
end;

function TdxPDFViewerPainter.DropDownButtonWidth: Integer;
begin
  Result := LookAndFeelPainter.GetScaledDropDownButtonRightPartSize(ScaleFactor);
end;

function TdxPDFViewerPainter.FindPanelCloseButtonSize: TSize;
begin
  Result := cxSize(LookAndFeelPainter.ScaledFilterCloseButtonSize(ScaleFactor));
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxPDFViewerPainter.FindPanelOptionsDropDownButtonWidth: Integer;
begin
  Result := LookAndFeelPainter.GetScaledDropDownButtonRightPartSize(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPaneButtonContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPaneButtonContentOffsets(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPaneButtonOverlay: TPoint;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPaneButtonOverlay(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPaneButtonSize: TSize;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPaneButtonSize(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPanePageCaptionContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPanePageCaptionContentOffsets(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPanePageCaptionTextColor: TColor;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPanePageCaptionTextColor;
end;

function TdxPDFViewerPainter.NavigationPanePageToolbarContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPanePageToolbarContentOffsets(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPaneContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPaneContentOffsets(ScaleFactor);
end;

function TdxPDFViewerPainter.NavigationPanePageContentOffsets: TRect;
begin
  Result := LookAndFeelPainter.PDFViewerNavigationPanePageContentOffsets(ScaleFactor);
end;

function TdxPDFViewerPainter.HighlightBackColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(LookAndFeelPainter.PDFViewerSelectionColor, 30);
end;

function TdxPDFViewerPainter.HighlightFrameColor: TdxAlphaColor;
begin
  Result := SelectionBackColor;
end;

function TdxPDFViewerPainter.SelectionBackColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(LookAndFeelPainter.PDFViewerSelectionColor, 127);
end;

function TdxPDFViewerPainter.SelectionFrameColor: TdxAlphaColor;
begin
  Result := dxacNone;
end;

function TdxPDFViewerPainter.TitleTextColor: TColor;
begin
  Result := LookAndFeelPainter.GetWindowContentTextColor;
end;

procedure TdxPDFViewerPainter.DrawButton(ACanvas: TcxCanvas; const ARect: TRect; const ACaption: string;
  AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledButton(ACanvas, ARect, ACaption, AState, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  DrawButton(ACanvas, ARect, '', AState);
end;

procedure TdxPDFViewerPainter.DrawButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect;
  AState: TcxButtonState);
begin
  cxDrawImage(ACanvas, ARect, AImage, nil, -1, AState <> cxbsDisabled, nil, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect;
  AState: TcxButtonState; AColorize: Boolean);
begin
  if AColorize then
    AImage.ChangeColor(ButtonSymbolColor(AState));
  DrawButtonGlyph(ACanvas, AImage, ARect, AState);
end;

procedure TdxPDFViewerPainter.DrawDropDownButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledScrollBarArrow(ACanvas, ARect, AState, adDown, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawDropDownButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph;
  const ARect: TRect; AState: TcxButtonState; AColorize: Boolean = True);
begin
  DrawButtonGlyph(ACanvas, AImage, ARect, AState, AColorize);
end;

procedure TdxPDFViewerPainter.DrawFocusRect(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.DrawFocusRect(LookAndFeelPainter.ScaledButtonFocusRect(ACanvas, ARect, ScaleFactor));
end;

procedure TdxPDFViewerPainter.DrawFindPanelBackground(ACanvas: TcxCanvas; const ARect: TRect);
const
  VisibleBordersMap: array[TdxPDFViewerFindPanelAlignment] of TcxBorders = (
    [bBottom], [bBottom, bLeft], [bBottom, bLeft, bRight], [bBottom, bRight],
    [bTop], [bTop, bLeft], [bTop, bLeft, bRight], [bTop, bRight]);
begin
  LookAndFeelPainter.PDFViewerDrawFindPanelBackground(ACanvas, ARect,
    VisibleBordersMap[Viewer.OptionsFindPanel.Alignment]);
end;

procedure TdxPDFViewerPainter.DrawFindPanelCloseButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledFilterCloseButton(ACanvas, ARect, AState, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawFindPanelOptionsButtonGlyph(
  ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect; AState: TcxButtonState);
begin
  AImage.ChangeColor(ButtonSymbolColor(AState));
  cxDrawImage(ACanvas, ARect, AImage, nil, -1, True, nil, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawFindPanelOptionsDropDownButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledScrollBarArrow(ACanvas, ARect, AState, adDown, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  LookAndFeelPainter.PDFViewerDrawNavigationPaneBackground(ACanvas, ARect, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
  AMinimized, ASelected, AIsFirst: Boolean);
begin
  LookAndFeelPainter.PDFViewerDrawNavigationPaneButton(ACanvas, ARect, AState, ScaleFactor, AMinimized, ASelected,
    AIsFirst);
end;

procedure TdxPDFViewerPainter.DrawNavigationPaneButtonGlyph(ACanvas: TcxCanvas; AImage: TdxSmartGlyph; const ARect: TRect;
  AState: TcxButtonState);
begin
  cxDrawImage(ACanvas, ARect, AImage, nil, -1, True, LookAndFeelPainter.PDFViewerNavigationPaneButtonColorPalette(AState),
    ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  LookAndFeelPainter.PDFViewerDrawNavigationPanePageBackground(ACanvas, ARect);
end;

procedure TdxPDFViewerPainter.DrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  if AState in [cxbsHot, cxbsPressed, cxbsDisabled] then
    LookAndFeelPainter.PDFViewerDrawNavigationPanePageButton(ACanvas, ARect, AState, ScaleFactor);
end;

procedure TdxPDFViewerPainter.DrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  LookAndFeelPainter.PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas, ARect);
end;

procedure TdxPDFViewerPainter.DrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  LookAndFeelPainter.PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas, ARect);
end;

procedure TdxPDFViewerPainter.DrawPageBackground(ACanvas: TcxCanvas; const ABorderRect, AContentRect: TRect; ASelected: Boolean);
begin
  LookAndFeelPainter.DrawPrintPreviewPageBackground(ACanvas, ABorderRect, AContentRect, ASelected, True);
end;

function TdxPDFViewerPainter.IsFadingAvailable: Boolean;
begin
  Result := (LookAndFeelPainter.LookAndFeelStyle = lfsNative) or IsSkinUsed;
end;

function TdxPDFViewerPainter.IsSkinUsed: Boolean;
begin
  Result := LookAndFeelPainter.LookAndFeelStyle = lfsSkin;
end;

function TdxPDFViewerPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Viewer.LookAndFeelPainter;
end;

function TdxPDFViewerPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Viewer.ScaleFactor;
end;

{ TdxPDFViewerLockedStatePaintHelper }

function TdxPDFViewerLockedStatePaintHelper.CanCreateLockedImage: Boolean;
begin
  Result := inherited CanCreateLockedImage and not Viewer.IsUpdateLocked;
end;

function TdxPDFViewerLockedStatePaintHelper.DoPrepareImage: Boolean;
begin
  Result := Viewer.DoPrepareLockedStateImage;
end;

function TdxPDFViewerLockedStatePaintHelper.GetControl: TcxControl;
begin
  Result := Viewer;
end;

function TdxPDFViewerLockedStatePaintHelper.GetOptions: TcxLockedStateImageOptions;
begin
  Result := Viewer.OptionsLockedStateImage;
end;

function TdxPDFViewerLockedStatePaintHelper.GetViewer: TdxPDFCustomViewer;
begin
  Result := TdxPDFCustomViewer(Owner);
end;

{ TdxPDFViewerLockedStateImageOptions }

function TdxPDFViewerLockedStateImageOptions.GetFont: TFont;
begin
  Result := (Owner as TdxPDFCustomViewer).Font;
end;

{ TdxPDFCustomViewer }

function TdxPDFCustomViewer.CanGoToNextView: Boolean;
begin
  Result := FController.ViewStateHistoryController.CanGoToNextView;
end;

function TdxPDFCustomViewer.CanGoToPrevView: Boolean;
begin
  Result := FController.ViewStateHistoryController.CanGoToPreviousView;
end;

function TdxPDFCustomViewer.IsDocumentLoaded: Boolean;
begin
  Result := not FIsDocumentClearing and (FDocument <> nil) and TdxPDFDocumentAccess(FDocument).IsLoaded;
end;

procedure TdxPDFCustomViewer.Clear;
begin
  if not TextSearch.IsLocked then
  begin
    if not FIsDocumentClearing then
    begin
      FIsDocumentClearing := True;
      try
        TdxPDFDocumentViewerCustomRendererAccess(Renderer).Clear;
        NavigationPane.Clear;
        FController.Clear;
        FDocument.Clear;
      finally
        FIsDocumentClearing := False;
      end
    end
    else
    begin
      RecreatePages;
      if OptionsFindPanel.DisplayMode <> fpdmNever then
        HideFindPanel;
      NavigationPane.Refresh;
      dxCallNotify(OnDocumentUnloaded, Self);
    end;
  end;
end;

procedure TdxPDFCustomViewer.ClearViewStateHistory;
begin
  ViewerController.ViewStateHistoryController.Clear;
end;

procedure TdxPDFCustomViewer.GoToNextView;
begin
  FController.ViewStateHistoryController.GoToNextView;
end;

procedure TdxPDFCustomViewer.GoToPrevView;
begin
  FController.ViewStateHistoryController.GoToPrevView;
end;

procedure TdxPDFCustomViewer.LoadFromFile(const AFileName: string);
begin
  BeforeLoadDocument;
  try
    try
      FDocument.LoadFromFile(AFileName);
    except
      Clear;
      raise;
    end;
  finally
    AfterLoadDocument;
  end;
end;

procedure TdxPDFCustomViewer.LoadFromStream(AStream: TStream);
begin
  BeforeLoadDocument;
  try
    try
      FDocument.LoadFromStream(AStream);
    except
      Clear;
      raise;
    end;
  finally
    AfterLoadDocument;
  end;
end;

procedure TdxPDFCustomViewer.RotateClockwise;
begin
  FController.Rotate(-90);
end;

procedure TdxPDFCustomViewer.RotateCounterclockwise;
begin
  FController.Rotate(90);
end;

function TdxPDFCustomViewer.IsFindPanelVisible: Boolean;
begin
  if Assigned(FOnGetFindPanelVisibility) then
    Result := OnGetFindPanelVisibility(Self)
  else
    Result := not FindPanel.AnimationController.Active and FindPanel.Visible;
end;

procedure TdxPDFCustomViewer.HideFindPanel;
begin
  if CanHideFindPanel then
  begin
    FindPanel.Visible := False;
    if OptionsFindPanel.ClearSearchStringOnClose then
      Highlights.Clear;
    dxCallNotify(OnHideFindPanel, Self);
  end;
end;

procedure TdxPDFCustomViewer.ShowFindPanel;
begin
  if CanShowFindPanel then
  begin
    if not IsFindPanelVisible then
    begin
      dxCallNotify(OnShowFindPanel, Self);
      FindPanel.Visible := True;
    end
    else
      SetFindPanelFocus;
  end;
end;

function TdxPDFCustomViewer.CanUpdateRenderQueue: Boolean;
begin
  Result := IsDocumentAvailable;
end;

function TdxPDFCustomViewer.GetPageRenderFactor(APageIndex: Integer): Single;
begin
  Result := ZoomFactor / 100;
end;

procedure TdxPDFCustomViewer.CreateSubClasses;

  procedure CreateOptions;
  begin
    FOptionsZoom := TdxPDFViewerOptionsZoom.Create(Self);
    FOptionsLockedStateImage := CreateOptionsLockedStateImage;
    FOptionsBehavior := TdxPDFViewerOptionsBehavior.Create(Self);
  end;

begin
  CreateDocument;
  CreateOptions;
  inherited CreateSubClasses;
  FDialogsLookAndFeel := TcxLookAndFeel.Create(Self);
  FDialogsLookAndFeel.MasterLookAndFeel := LookAndFeel;
  FTextSearch := TdxPDFViewerTextSearch.Create(Self);
  FFindPanel := TdxPDFViewerFindPanel.Create(Self);
  FController := GetControllerClass.Create(Self);
  FNavigationPane := TdxPDFViewerNavigationPane.Create(Self);
  FNavigationPane.AddPages;
  FViewInfo := GetViewInfoClass.Create(FController);
  ViewInfo.CreateCells;
  FSelection := TdxPDFViewerSelection.Create(Self);
  SelectionController.OnSelectionChanged := OnSelectionChangedHandler;
  FHighlights := TdxPDFViewerHighlights.Create(Self);
  FHighlights.OnChanged := OnHighlightsChangedHandler;
  FHitTest := TdxPDFViewerDocumentHitTest.Create(Self);
  FLockedStatePaintHelper := CreateLockedStatePaintHelper;
  RecreateRenderer;
end;

procedure TdxPDFCustomViewer.DestroySubClasses;
begin
  TdxPDFDocumentAccess(FDocument).RemoveListener(Self);
  Clear;
  FreeAndNil(FLockedStatePaintHelper);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FOptionsLockedStateImage);
  FreeAndNil(FHitTest);
  FreeAndNil(FDocument);
  FreeAndNil(FHighlights);
  FreeAndNil(FSelection);
  FreeAndNil(FOptionsZoom);
  FreeAndNil(FViewInfo);
  FreeAndNil(FDialogsLookAndFeel);
  FreeAndNil(FNavigationPane);
  FreeAndNil(FFindPanel);
  FreeAndNil(FController);
  FreeAndNil(FTextSearch);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomViewer.DoCalculate;
begin
  inherited DoCalculate;
  ViewInfo.Calculate;
end;

procedure TdxPDFCustomViewer.Initialize;
begin
  inherited Initialize;
  FPasswordAttemptsLimit := dxPDFDefaultPasswordAttemptsLimit;
  InternalOptionsBehavior := [pobKeyNavigation, pobThumbTracking];
  InternalOptionsView := [povAutoHideScrollBars, povDefaultDrawPageBackground, povPageSelection];
  OptionsHint := [pohShowOnScroll];
  Keys := [kArrows, kChars];
end;

function TdxPDFCustomViewer.CanProcessScrollEvents(var Message: TMessage): Boolean;
begin
  Result := inherited CanProcessScrollEvents(Message) and not IsDesigning;
end;

function TdxPDFCustomViewer.CreatePage: TdxPreviewPage;
var
  APage: TdxPDFViewerPage;
begin
  if (FDocument <> nil) and (TdxPDFDocumentAccess(FDocument).Pages <> nil) then
  begin
    Result := inherited CreatePage;
    APage := Result as TdxPDFViewerPage;
    APage.DocumentPage := TdxPDFDocumentAccess(FDocument).Pages[PageCount - 1];
  end
  else
    Result := nil;
end;

function TdxPDFCustomViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  AController: TdxPDFViewerCustomController;
begin
  Result := IsDocumentLoaded;
  if Result then
  begin
    AController := ActiveController;
    if (AController <> nil) or ControllerFromPoint(GetMouseCursorClientPos, AController) then
      Result := AController.MouseWheel(Shift, WheelDelta, ScreenToClient(MousePos));
  end;
  if not Result then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TdxPDFCustomViewer.GetClientBounds: TRect;
begin
  Result := inherited GetClientBounds;
  if NavigationPane.CanShow then
    Result := NavigationPane.CalculateParentClientBounds(Result);
end;

function TdxPDFCustomViewer.GetDefaultHeight: Integer;
begin
  Result := dxPDFViewerDefaultSize.cy;
end;

function TdxPDFCustomViewer.GetDefaultMaxZoomFactor: Integer;
begin
  Result := dxPDFViewerMaxZoomFactor;
end;

function TdxPDFCustomViewer.GetDefaultMinZoomFactor: Integer;
begin
  Result := dxPDFViewerMinZoomFactor;
end;

function TdxPDFCustomViewer.GetDefaultWidth: Integer;
begin
  Result := dxPDFViewerDefaultSize.cx;
end;

function TdxPDFCustomViewer.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxPDFViewerResizeNavigationPaneDragAndDropObject;
end;

function TdxPDFCustomViewer.GetDefaultZoomFactor: Integer;
begin
  Result := dxPDFViewerDefaultZoomFactor;
end;

function TdxPDFCustomViewer.GetDefaultZoomStep: Integer;
begin
  Result := dxPDFViewerZoomStep;
end;

function TdxPDFCustomViewer.GetPageClass: TdxPreviewPageClass;
begin
  Result := TdxPDFViewerPage;
end;

function TdxPDFCustomViewer.GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass;
begin
  Result := TdxPDFViewerPageSizeOptions;
end;

function TdxPDFCustomViewer.GetScrollStep: Integer;
begin
  Result := dxPDFViewerDefaultScrollStep;
end;

function TdxPDFCustomViewer.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := not IsDesigning and HitTest.HitAtNavigationPaneSplitter;
end;

procedure TdxPDFCustomViewer.CalculatePageNumberHintText(AStartPage, AEndPage: Integer; var AText: string);
begin
  AText := AText + IntToStr(CurrentPageIndex + 1)
end;

procedure TdxPDFCustomViewer.CalculateZoomFactorForPageWidthPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer);

  function GetMaxPageSize(AFirstIndex, ALastIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AFirstIndex to ALastIndex do
      Result := Max(Result, ScaleFactor.Apply(Pages[I].PageSize.ActualSizeInPixels.X));
  end;

begin
  ZoomFactor := MulDiv(100, cxRectWidth(GetPagesArea), GetMaxPageSize(AFirstPageIndex, ALastPageIndex));
end;

procedure TdxPDFCustomViewer.CalculateZoomFactorForPagesPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer);
begin
  ZoomFactor := MulDiv(100, cxRectHeight(GetPagesArea), ScaleFactor.Apply(Pages[SelPageIndex].PageSize.ActualSizeInPixels.Y));
end;

procedure TdxPDFCustomViewer.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScaleEx(M, D, isDpiChange);
  NavigationPane.PageSize := MulDiv(NavigationPane.PageSize, M, D);
end;

procedure TdxPDFCustomViewer.DoContextPopup(MousePos: TPoint; var Handled: Boolean);

  function CanShowContextPopup: Boolean;
  begin
    Result := not (IsFindPanelVisible and HitTest.HitAtFindPanel);
  end;

begin
  inherited DoContextPopup(MousePos, Handled);
  if not Handled and IsDocumentLoaded and not TextSearch.IsLocked then
  begin
    UpdateActiveController(GetMouseCursorClientPos);
    if CanShowContextPopup then
      Handled := ActiveController.ContextPopup(ClientToScreen(GetMouseCursorClientPos));
    ActiveController := nil;
  end;
end;

procedure TdxPDFCustomViewer.DoSelectedPageChanged;
begin
  NavigationPane.Thumbnails.SynchronizeSelectedPage;
  inherited DoSelectedPageChanged;
end;

procedure TdxPDFCustomViewer.DoZoomFactorChanged;
begin
  inherited DoZoomFactorChanged;
  RestartRenderContentTimer;
  UpdateRenderQueue(True);
  FController.ViewStateHistoryController.StoreCurrentViewState(vsctZooming);
end;

procedure TdxPDFCustomViewer.DoZoomModeChanged;
begin
  inherited DoZoomModeChanged;
  if not IsUpdateLocked then
    FController.ViewStateHistoryController.StoreCurrentViewState(vsctZooming);
end;

procedure TdxPDFCustomViewer.DrawContent(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    if GetScrollbarMode <> sbmHybrid then
      ACanvas.IntersectClipRect(ClientBounds);
    inherited DrawContent(ACanvas, R);
    DrawCaret;
    ViewInfo.Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxPDFCustomViewer.DrawNoPages(ACanvas: TcxCanvas);
begin
  if not OptionsLockedStateImage.Enabled or OptionsLockedStateImage.Enabled and not FIsDocumentLoading then
    inherited DrawNoPages(ACanvas);
end;

procedure TdxPDFCustomViewer.DrawScrollBars(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ViewInfo.DocumentViewerBounds);
    inherited DrawScrollBars(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxPDFCustomViewer.InternalGoToFirstPage;
begin
  ChangePage(SelectFirstPage);
end;

procedure TdxPDFCustomViewer.InternalGoToLastPage;
begin
  ChangePage(SelectLastPage);
end;

procedure TdxPDFCustomViewer.InternalGoToNextPage;
begin
  ChangePage(SelectNextPage);
end;

procedure TdxPDFCustomViewer.InternalGoToPrevPage;
begin
  ChangePage(SelectPrevPage);
end;

procedure TdxPDFCustomViewer.FontChanged;
begin
  inherited FontChanged;
  ViewInfo.Calculate;
end;

procedure TdxPDFCustomViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if IsDocumentLoaded and not TextSearch.IsLocked then
  begin
    if not FController.KeyDown(Key, Shift) then
      inherited KeyDown(Key, Shift);
    MakeVisible(CurrentPageIndex);
  end;
end;

procedure TdxPDFCustomViewer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if IsDocumentLoaded and not TextSearch.IsLocked then
    if Assigned(OnKeyUp) then
      OnKeyUp(Self, Key, Shift)
    else
      if ViewInfo.IsFocused then
        inherited KeyUp(Key, Shift);
end;

procedure TdxPDFCustomViewer.KeyPress(var Key: Char);
begin
  if IsDocumentLoaded and not TextSearch.IsLocked then
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, Key)
    else
      inherited KeyPress(Key);
end;

procedure TdxPDFCustomViewer.Loaded;
begin
  inherited Loaded;
  LayoutChanged(ctHard);
end;

procedure TdxPDFCustomViewer.MakeVisible(APageIndex: Integer);
var
  ATopLeft: TPoint;
  AStartIndex, AEndIndex: Integer;
begin
  PageList.GetVisiblePageRanges(AStartIndex, AEndIndex);
  if not InRange(CurrentPageIndex, AStartIndex, AEndIndex) then
  begin
    ATopLeft := cxPoint(LeftPos, TopPos);
    inherited MakeVisible(APageIndex);
    LeftPos := ATopLeft.X;
  end;
end;

procedure TdxPDFCustomViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AController: TdxPDFViewerCustomController;
begin
  ActiveController := nil;
  if ControllerFromPoint(X, Y, AController) then
    ActiveController := AController;
  inherited MouseDown(Button, Shift, X, Y);
  if ActiveController <> nil then
    ActiveController.MouseDown(Button, Shift, X, Y);
end;

procedure TdxPDFCustomViewer.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if not ViewInfo.FindPanel.Edit.IsFocused and (DragAndDropState <> ddsInProcess) then
  begin
    UpdateActiveController(GetMouseCursorClientPos);
    ActiveController.MouseLeave(AControl);
    ActiveController.UpdateCursor;
  end;
  ActiveController := nil;
end;

procedure TdxPDFCustomViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if DragAndDropState <> ddsInProcess then
  begin
    UpdateActiveController(cxPoint(X, Y));
    ActiveController.MouseMove(Shift, X, Y);
    ActiveController.UpdateCursor;
  end;
  ActiveController := nil;
end;

procedure TdxPDFCustomViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AController: TdxPDFViewerCustomController;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ControllerFromPoint(X, Y, AController) then
    AController.MouseUp(Button, Shift, X, Y);
  ActiveController := nil;
end;

procedure TdxPDFCustomViewer.Paint;
begin
  inherited Paint;
  NavigationPane.ViewInfo.Draw(Canvas);
end;

procedure TdxPDFCustomViewer.ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer);
begin
// do nothing
end;

procedure TdxPDFCustomViewer.SetPaintRegion;
begin
// do nothing
end;

procedure TdxPDFCustomViewer.SetSelPageIndex(Value: Integer);
begin
  Value := TdxPDFUtils.MinMax(Value, -1, PageCount - 1);
  if (FSelPageIndex <> Value) and ((Value = -1) or CanSelectPage(Value)) then
  begin
    DoSelectedPageChanging;
    FSelPageIndex := Value;
    DoSelectedPageChanged;
  end;
end;

procedure TdxPDFCustomViewer.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if not IsDesigning then
    inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TdxPDFCustomViewer.ScrollPosChanged(const AOffset: TPoint);
begin
  if not IsUpdateLocked then
  begin
    LayoutChanged(ctLight);
    FController.ScrollPosChanged;
  end;
end;

procedure TdxPDFCustomViewer.SelectFirstPage;
var
  APreviousPageIndex: Integer;
begin
  BeginUpdate;
  try
    APreviousPageIndex := CurrentPageIndex;
    inherited SelectFirstPage;
    if APreviousPageIndex = CurrentPageIndex then
    begin
      UpdateLeftTopPosition(cxNullPoint);
      CurrentPageIndex := 0;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.SelectLastPage;
var
  APreviousPageIndex: Integer;
begin
  BeginUpdate;
  try
    APreviousPageIndex := CurrentPageIndex;
    inherited SelectLastPage;
    if APreviousPageIndex = CurrentPageIndex then
    begin
      UpdateLeftTopPosition(cxPoint(LeftPos, MaxInt));
      CurrentPageIndex := PageCount - 1;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.SelectNextPage;
var
  APreviousPageIndex: Integer;
begin
  BeginUpdate;
  try
    APreviousPageIndex := CurrentPageIndex;
    inherited SelectNextPage;
    if APreviousPageIndex = CurrentPageIndex then
      SelectLastPage;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.SelectPrevPage;
var
  APreviousPageIndex: Integer;
begin
  BeginUpdate;
  try
    APreviousPageIndex := CurrentPageIndex;
    inherited SelectPrevPage;
    if APreviousPageIndex = CurrentPageIndex then
      SelectFirstPage;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.UpdateLeftTopPosition(const ATopLeft: TPoint);
begin
  LockScrollBars;
  try
    Thumbnails.ThumbnailPreview.BeginUpdate;
    try
      SetLeftTop(ATopLeft);
      UpdateSelectedPageIndex;
    finally
      Thumbnails.ThumbnailPreview.EndUpdate;
    end;
  finally
    UnlockScrollBars;
  end;
end;

procedure TdxPDFCustomViewer.UpdateSelectedPageIndex;
var
  APageIndex: Integer;
begin
  BeginUpdate;
  try
    PageList.GetVisibleSelectedPageIndex(APageIndex);
    SelPageIndex := APageIndex;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.UpdateScrollPositions;
var
  ALeftTop: TPoint;
begin
  inherited UpdateScrollPositions;
  if not FIsDocumentLoading and (PrevZoomFactor <> ZoomFactor) then
  begin
    CalculateScrollPositions(ALeftTop);
    UpdateLeftTopPosition(ALeftTop);
  end;
end;

function TdxPDFCustomViewer.CanOpenAttachment(AAttachment: TdxPDFFileAttachment): Boolean;
begin
  if Assigned(FOnAttachmentOpen) then
  begin
    FOnAttachmentOpen(Self, AAttachment, Result);
    Result := not Result;
  end
  else
    Result := True;
end;

function TdxPDFCustomViewer.CanOpenUri(const AUri: string): Boolean;
begin
  if Assigned(FOnHyperlinkClick) then
  begin
    FOnHyperlinkClick(Self, AUri, Result);
    Result := not Result;
  end
  else
    Result := True;
end;

function TdxPDFCustomViewer.CanSaveAttachment(AAttachment: TdxPDFFileAttachment): Boolean;
begin
  if Assigned(FOnAttachmentSave) then
  begin
    FOnAttachmentSave(Self, AAttachment, Result);
    Result := not Result;
  end
  else
    Result := True;
end;

function TdxPDFCustomViewer.DoCreateDocument: TdxPDFDocument;
begin
  Result := TdxPDFViewerDocument.Create(Self);
  Result.PasswordAttemptsLimit := FPasswordAttemptsLimit;
end;

function TdxPDFCustomViewer.DoGetPassword(var APassword: string): Boolean;
begin
  if Assigned(OnGetPassword) then
    Result := OnGetPassword(Self, APassword)
  else
    Result := ShowPasswordDialog(Self, APassword);
end;

function TdxPDFCustomViewer.DoPrepareLockedStateImage: Boolean;
begin
  Result := False;
  if Assigned(OnPrepareLockedStateImage) then
    OnPrepareLockedStateImage(Self, LockedStatePaintHelper.Bitmap, Result);
end;

function TdxPDFCustomViewer.CreateLockedStatePaintHelper: TdxPDFViewerLockedStatePaintHelper;
begin
  Result := TdxPDFViewerLockedStatePaintHelper.Create(Self);
end;

function TdxPDFCustomViewer.CreateOptionsLockedStateImage: TdxPDFViewerLockedStateImageOptions;
begin
  Result := TdxPDFViewerLockedStateImageOptions.Create(Self);
end;

function TdxPDFCustomViewer.GetControllerClass: TdxPDFViewerControllerClass;
begin
  Result := TdxPDFViewerController;
end;

function TdxPDFCustomViewer.GetPainterClass: TdxPDFViewerPainterClass;
begin
  Result := TdxPDFViewerPainter;
end;

function TdxPDFCustomViewer.GetViewInfoClass: TdxPDFViewerViewInfoClass;
begin
  Result := TdxPDFViewerViewInfo;
end;

function TdxPDFCustomViewer.CanChangeVisibility: Boolean;
begin
  Result := (OptionsFindPanel.DisplayMode = fpdmManual) or Assigned(OnShowFindPanel) and Assigned(OnHideFindPanel) and
    Assigned(OnGetFindPanelVisibility);
end;

function TdxPDFCustomViewer.CanShowFindPanel: Boolean;
begin
  Result := IsDocumentLoaded and (OptionsFindPanel.DisplayMode <> fpdmNever) and not TextSearch.IsLocked;
end;

function TdxPDFCustomViewer.CanHideFindPanel: Boolean;
begin
  Result := not TextSearch.IsLocked and (not IsDestroying and not IsDocumentLoaded and IsFindPanelVisible or
    IsDocumentLoaded and (OptionsFindPanel.DisplayMode <> fpdmAlways));
end;

procedure TdxPDFCustomViewer.SetFindPanelFocus;
begin
  ViewInfo.FindPanel.Edit.InternalEdit.SetFocus;
end;

function TdxPDFCustomViewer.CanExtractContent: Boolean;
begin
  Result := IsDocumentLoaded and Document.AllowContentExtraction;
end;

function TdxPDFCustomViewer.CanDrawCaret: Boolean;
begin
  Result := IsDocumentAvailable and not FindPanel.AnimationController.Active and
    SelectionController.Caret.IsValid and (not IsFindPanelVisible or IsFindPanelVisible and not FindPanel.Edit.Focused);
end;

function TdxPDFCustomViewer.CanPrint: Boolean;
begin
  Result := dxPrintingRepository.CanBuildReport(Self) and IsDocumentLoaded and Document.AllowPrinting;
end;

function TdxPDFCustomViewer.CanUseAnimation: Boolean;
begin
  Result := not IsDesigning and Visible and HandleAllocated and IsWindowVisible(Handle) and
    (OptionsFindPanel.AnimationTime > 0);
end;

function TdxPDFCustomViewer.CanZoomIn: Boolean;
begin
  Result := ZoomFactor < MaxZoomFactor;
end;

function TdxPDFCustomViewer.CanZoomOut: Boolean;
begin
  Result := ZoomFactor > MinZoomFactor;
end;

function TdxPDFCustomViewer.CreatePainter: TdxPDFViewerPainter;
begin
  Result := GetPainterClass.Create(Self);
end;

function TdxPDFCustomViewer.IsDocumentAvailable: Boolean;
begin
  Result := not IsUpdateLocked and IsDocumentLoaded and not FIsDocumentClearing;
end;

function TdxPDFCustomViewer.IsDocumentPanning: Boolean;
begin
  Result := ViewerController.IsDocumentPanning;
end;

function TdxPDFCustomViewer.IsDocumentSelecting: Boolean;
begin
  Result := ViewerController.IsDocumentSelecting;
end;

function TdxPDFCustomViewer.GetFocusedCellAsAttachment: TdxPDFFileAttachment;
begin
  Result := nil;
  if (ViewerController.FocusedField <> nil) and (ViewerController.FocusedField.HitCode = hcAttachment) then
    Result := TdxPDFFileAttachmentAnnotationFieldAccess(ViewerController.FocusedField).Attachment
end;

procedure TdxPDFCustomViewer.DeleteCaret;
begin
  if FCaretBitmap <> nil then
  begin
    DeleteObject(FCaretBitmap.Handle);
    FreeAndNil(FCaretBitmap);
    DestroyCaret;
    Invalidate;
  end;
end;

procedure TdxPDFCustomViewer.DrawCaret;
var
  ASize: TPoint;
  AHeight: Double;
  R: TdxRectF;
  AAngle: Integer;
  ACaretPage: TdxPDFViewerPage;
  ARotationAngle: TcxRotationAngle;
begin
  if CanDrawCaret then
  begin
    ACaretPage := Pages[SelectionController.Caret.Position.PageIndex] as TdxPDFViewerPage;
    R.TopLeft := SelectionController.Caret.ViewData.TopLeft;
    R.Right := R.Left + 1;
    R.Bottom := R.Top - SelectionController.Caret.ViewData.Height;
    R := ACaretPage.ToViewerRect(cxRectAdjustF(R));
    AHeight := SelectionController.Caret.ViewData.Height * DocumentScaleFactor.X * DocumentToViewerFactor.X;
    ASize.X := 1;
    ASize.Y := Round(AHeight) + 1;
    ARotationAngle := RotationAngle;
    AAngle := Trunc(RadToDeg(SelectionController.Caret.ViewData.Angle));
    if (ARotationAngle in [raPlus90, raMinus90]) or (AAngle = 90) then
    begin
      ASize.X := Round(AHeight) + 1;
      ASize.Y := 1;
    end;
    DrawCaret(cxPoint(R.TopLeft), ASize);
  end;
end;

procedure TdxPDFCustomViewer.DrawCaret(const P, ASize: TPoint);
const
  Color = clBlack;
var
  ACaretRect: TRect;
  ARealSize, ARealDestPoint: TPoint;
begin
  ARealSize := ASize;
  ARealDestPoint := P;
  ACaretRect := cxRectSetSize(cxRect(P, P), ASize.X, ASize.Y) ;
  if cxRectIntersect(ACaretRect, ViewInfo.FindPanel.Bounds) and IsFindPanelVisible then
    case RotationAngle of
      raMinus90, raPlus90:
        begin
          if ViewerController.InFindPanelRect(P) then
          begin
            ARealSize.X := ARealDestPoint.X + ARealSize.X - ViewInfo.FindPanel.Bounds.Right;
            ARealDestPoint.X := ViewInfo.FindPanel.Bounds.Right;
          end
          else
            ARealSize.X := ViewInfo.FindPanel.Bounds.Left - ARealDestPoint.X;
          ACaretRect := cxRect(0, 0, ARealSize.X, ARealSize.Y);
        end;
      ra0, ra180:
        begin
          if ViewerController.InFindPanelRect(ARealDestPoint) then
          begin
            ARealSize.Y := ARealDestPoint.Y - ViewInfo.FindPanel.Bounds.Top;
            ARealDestPoint.Y := ViewInfo.FindPanel.Bounds.Bottom;
          end
          else
            ARealSize.Y := ViewInfo.FindPanel.Bounds.Top - ARealDestPoint.Y;
         if RotationAngle = ra180 then
           ACaretRect := cxRect(0, ARealSize.Y, ARealSize.X, 0);
        end;
    end;
  DestroyCaret;
  if (ARealSize.X > 0) and (ARealSize.Y > 0) and PtInRect(ClientBounds, ARealDestPoint) then
  begin
    FreeAndNil(FCaretBitmap);
    FCaretBitmap := TcxBitmap32.Create;
    FCaretBitmap.SetSize(ARealSize.X, ARealSize.Y);
    dxGPPaintCanvas.BeginPaint(FCaretBitmap.Canvas.Handle, FCaretBitmap.ClientRect);
    try
      dxGPPaintCanvas.Line(ACaretRect.Left, ACaretRect.Top, ACaretRect.Right, ACaretRect.Bottom, Color);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
    CreateCaret(Handle, FCaretBitmap.Handle, 0, 0);
    SetCaretPos(ARealDestPoint.X, ARealDestPoint.Y);
  end;
end;

procedure TdxPDFCustomViewer.FindNext;
begin
  if not IsFindPanelVisible then
    ShowFindPanel;
  FindPanel.Find;
end;


procedure TdxPDFCustomViewer.RecreatePages;
begin
  BeginUpdate;
  try
    TdxPDFDocumentViewerCustomRendererAccess(Renderer).Clear;
    DocumentState.CalculateScreenFactors;
    LoadDocumentPages;
    CurrentPageIndex := FController.ViewStateHistoryController.CurrentViewState.CurrentPageIndex;
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFCustomViewer.OpenAttachment(AAttachment: TdxPDFFileAttachment);
begin
  ViewerController.OpenAttachment(AAttachment);
end;

procedure TdxPDFCustomViewer.SaveAttachment(AAttachment: TdxPDFFileAttachment);
begin
  ViewerController.SaveAttachment(AAttachment);
end;

procedure TdxPDFCustomViewer.UpdateActiveController(const P: TPoint);

  procedure CalculateHitTest(AController: TdxPDFViewerContainerController; const P: TPoint);
  begin
    if AController <> ActiveController then
      AController.HotCell := nil;
    AController.CalculateHitTests(P);
  end;

var
  AController: TdxPDFViewerCustomController;
begin
  if ControllerFromPoint(P, AController) then
    ActiveController := AController
  else
    ActiveController := ViewerController;
  CalculateHitTest(ViewerController, P);
  CalculateHitTest(NavigationPane.Controller, P);
end;

function TdxPDFCustomViewer.GetActivePage: TdxPDFViewerPage;
begin
  Result := Pages[CurrentPageIndex] as TdxPDFViewerPage;
end;

function TdxPDFCustomViewer.GetAttachments: TdxPDFViewerAttachments;
begin
  Result := NavigationPane.Attachments;
end;

function TdxPDFCustomViewer.GetBookmarks: TdxPDFViewerBookmarks;
begin
  Result := NavigationPane.Bookmarks;
end;

function TdxPDFCustomViewer.GetCurrentPageIndex: Integer;
begin
  Result := IfThen(SelPageIndex <> -1, SelPageIndex, 0);
end;

function TdxPDFCustomViewer.GetDocument: TdxPDFDocument;
begin
  Result := FDocument;
end;

function TdxPDFCustomViewer.GetDocumentScaleFactor: TdxPointF;
begin
  Result := DocumentState.ScaleFactor;
end;

function TdxPDFCustomViewer.GetDocumentToViewerFactor: TdxPointF;
begin
  Result := DocumentState.DocumentToViewerFactor;
end;

function TdxPDFCustomViewer.GetHandTool: Boolean;
begin
  Result := DocumentState.HandTool;
end;

function TdxPDFCustomViewer.GetOptionsFindPanel: TdxPDFViewerOptionsFindPanel;
begin
  Result := FindPanel.Options;
end;

function TdxPDFCustomViewer.GetOptionsNavigationPane: TdxPDFViewerOptionsNavigationPane;
begin
  Result := NavigationPane.Options;
end;

function TdxPDFCustomViewer.GetOnCustomDrawPreRenderPageThumbnail: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent;
begin
  Result := Thumbnails.ThumbnailPreview.OnCustomDrawPreRenderPage;
end;

function TdxPDFCustomViewer.GetPainter: TdxPDFViewerPainter;
begin
  Result := ViewInfo.Painter;
end;

function TdxPDFCustomViewer.GetRotationAngle: TcxRotationAngle;
begin
  Result := DocumentState.RotationAngle;
end;

function TdxPDFCustomViewer.GetSelectionController: TdxPDFViewerSelectionController;
begin
  Result := FController.SelectionController;
end;

function TdxPDFCustomViewer.GetThumbnails: TdxPDFViewerThumbnails;
begin
  Result := NavigationPane.Thumbnails;
end;

procedure TdxPDFCustomViewer.SetCurrentPageIndex(const AValue: Integer);
begin
  if (SelPageIndex <> AValue) and (PageCount > 0) then
  begin
    BeginUpdate;
    try
      SelPageIndex := AValue;
    finally
      EndUpdate;
      MakeVisible(SelPageIndex);
      FController.ViewStateHistoryController.StoreCurrentViewState(vsctScrolling);
    end;
  end;
end;

procedure TdxPDFCustomViewer.SetDialogsLookAndFeel(const AValue: TcxLookAndFeel);
begin
  FDialogsLookAndFeel.Assign(AValue);
end;

procedure TdxPDFCustomViewer.SetHandTool(const AValue: Boolean);
begin
  DocumentState.HandTool := AValue;
end;

procedure TdxPDFCustomViewer.SetOnSearchProgress(const AValue: TdxPDFDocumentTextSearchProgressEvent);
begin
  FOnSearchProgress := AValue;
  if IsDocumentLoaded then
    FDocument.OnSearchProgress := FOnSearchProgress;
end;

procedure TdxPDFCustomViewer.SetOnCustomDrawPreRenderPageThumbnail(const AValue: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent);
begin
  Thumbnails.ThumbnailPreview.OnCustomDrawPreRenderPage := AValue;
end;

procedure TdxPDFCustomViewer.SetOptionsBehavior(const AValue: TdxPDFViewerOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxPDFCustomViewer.SetOptionsFindPanel(const AValue: TdxPDFViewerOptionsFindPanel);
begin
  FindPanel.Options := AValue;
end;

procedure TdxPDFCustomViewer.SetOptionsLockedStateImage(AValue: TdxPDFViewerLockedStateImageOptions);
begin
  FOptionsLockedStateImage.Assign(AValue);
end;

procedure TdxPDFCustomViewer.SetOptionsNavigationPane(const AValue: TdxPDFViewerOptionsNavigationPane);
begin
  NavigationPane.Options := AValue;
end;

procedure TdxPDFCustomViewer.SetOptionsZoom(const AValue: TdxPDFViewerOptionsZoom);
begin
  FOptionsZoom.Assign(AValue);
end;

procedure TdxPDFCustomViewer.SetRotationAngle(const AValue: TcxRotationAngle);
begin
  if RotationAngle <> AValue then
  begin
    DocumentState.RotationAngle := AValue;
    RecreatePages;
    FController.ViewStateHistoryController.StoreCurrentViewState(vsctRotation);
  end;
end;

function TdxPDFCustomViewer.ControllerFromPoint(const P: TPoint; var AController: TdxPDFViewerCustomController): Boolean;
begin
  Result := IsDocumentLoaded and not TextSearch.IsLocked and not (DragAndDropState = ddsInProcess);
  if Result then
  begin
    AController := ActiveController;
    if AController = nil then
    begin
      if PtInRect(ViewInfo.DocumentViewerBounds,  P) or ViewerController.MouseButtonPressed then
        AController := ViewerController
      else
        if NavigationPane.Controller.InPaneRect(P) then
          AController := NavigationPane.Controller;
    end;
    Result := AController <> nil;
  end;
end;

function TdxPDFCustomViewer.ControllerFromPoint(X, Y: Integer; var AController: TdxPDFViewerCustomController): Boolean;
begin
  Result := ControllerFromPoint(Point(X, Y), AController);
end;

procedure TdxPDFCustomViewer.AfterLoadDocument;
begin
  HideHourglassCursor;
  EndUpdate;
  LockedStatePaintHelper.EndLockedPaint;
  FIsDocumentLoading := False;
end;

procedure TdxPDFCustomViewer.BeforeLoadDocument;
begin
  FIsDocumentLoading := True;
  Clear;
  LockedStatePaintHelper.BeginLockedPaint(lsimImmediate);
  BeginUpdate;
  ShowHourglassCursor;
end;

procedure TdxPDFCustomViewer.CalculateScrollPositions(var ALeftTop: TPoint);
var
  AScale: Single;
  ADelta, AHalfClientSize: TdxPointF;
begin
  AHalfClientSize := cxPointF(GetClientSize.cx / 2, GetClientSize.cy/ 2);
  if PrevZoomFactor <> 0 then
  begin
    AScale := ZoomFactor / PrevZoomFactor;
    ADelta.X := (LeftPos + AHalfClientSize.X) * AScale;
    ADelta.Y := (TopPos + AHalfClientSize.Y) * AScale;
  end
  else
    ADelta := cxPointF(cxNullPoint);
  ALeftTop.X := TdxPDFUtils.ConvertToInt(ADelta.X - AHalfClientSize.X);
  ALeftTop.Y := TdxPDFUtils.ConvertToInt(ADelta.Y - AHalfClientSize.Y);
end;

procedure TdxPDFCustomViewer.ChangePage(AProc: TChangePageProc);
begin
  BeginUpdate;
  try
    AProc;
  finally
    EndUpdate;
  end;
  MakeVisible(SelPageIndex);
end;

procedure TdxPDFCustomViewer.CreateDocument;
begin
  FDocument := DoCreateDocument as TdxPDFViewerDocument;
  FDocument.OnGetPassword := OnGetPasswordHandler;
  FDocument.OnLoaded := OnDocumentLoadedHandler;
  FDocument.OnUnloaded := OnDocumentUnLoadedHandler;
  FDocument.OnSearchProgress := OnSearchProgress;
  TdxPDFDocumentAccess(FDocument).AddListener(Self);
end;

procedure TdxPDFCustomViewer.LoadDocumentPages;
var
  I: Integer;
begin
  PageCount := 0;
  if not FIsDocumentClearing then
    for I := 0 to DocumentPages.Count - 1 do
      CreatePage;
end;

function TdxPDFCustomViewer.GetLockedStateImage: TcxBitmap32;
begin
  Result := LockedStatePaintHelper.GetImage;
end;

function TdxPDFCustomViewer.GetLockedStateTopmostControl: TcxControl;
begin
  Result := Self;
end;

function TdxPDFCustomViewer.OnGetPasswordHandler(Sender: TObject;
  {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} APassword: string): Boolean;
begin
  Result := DoGetPassword(APassword);
end;

procedure TdxPDFCustomViewer.OnDocumentLoadedHandler(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  BeginUpdate;
  try
    RotationAngle := ra0;
    RecreatePages;
    GoToFirstPage;
    ClearViewStateHistory;
    if OptionsFindPanel.DisplayMode = fpdmAlways then
      ShowFindPanel;
    NavigationPane.Refresh;
  finally
    EndUpdate;
  end;
  if Assigned(FOnDocumentLoaded) then
    OnDocumentLoaded(Document, AInfo);
end;

procedure TdxPDFCustomViewer.OnDocumentUnLoadedHandler(Sender: TObject);
begin
  if not IsUpdateLocked and not IsDestroying then
    Clear;
end;

procedure TdxPDFCustomViewer.OnSelectionChangedHandler(Sender: TObject);
begin
  dxCallNotify(OnSelectionChanged, Self);
end;

procedure TdxPDFCustomViewer.OnHighlightsChangedHandler(Sender: TObject);
begin
  if not IsDestroying then
    Invalidate;
end;

procedure TdxPDFCustomViewer.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TdxPDFCustomViewer.WMSetCursor(var Message: TWMSetCursor);

  function NeedUpdateCursorUnderScrollBars(const P: TPoint): Boolean;
  begin
    Result := not ViewerController.InFindPanelRect(P) or HandTool or IsScrollBarsArea(P) or
      NavigationPane.Controller.InPaneRect(P);
  end;

var
  AController: TdxPDFViewerCustomController;
  P: TPoint;
begin
  if not IsDesigning then
  begin
    P := GetMouseCursorClientPos;
    if NeedUpdateCursorUnderScrollBars(P) and ControllerFromPoint(P, AController) then
    begin
      (AController as TdxPDFViewerContainerController).CalculateHitTests(GetMouseCursorClientPos);
      (AController as TdxPDFViewerContainerController).UpdateCursor;
    end;
  end
  else
    inherited;
end;

{ TdxPDFViewerSelection }

constructor TdxPDFViewerSelection.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FController := AViewer.SelectionController;
end;

function TdxPDFViewerSelection.IsEmpty: Boolean;
begin
  Result := Selection = nil;
end;

procedure TdxPDFViewerSelection.Clear;
begin
  FController.Clear;
end;

procedure TdxPDFViewerSelection.CopyToClipboard;
begin
  FController.CopyToClipboard;
end;

procedure TdxPDFViewerSelection.Select(const ARect: TRect; AMakeVisible: Boolean = False);
begin
  FController.Select(ARect);
  MakeVisible(AMakeVisible);
end;

procedure TdxPDFViewerSelection.SelectText(const ARange: TdxPDFPageTextRange; AMakeVisible: Boolean = True);
begin
  if ARange.IsValid then
  begin
    FController.SelectText(ARange);
    MakeVisible(AMakeVisible);
  end;
end;

procedure TdxPDFViewerSelection.SelectAll;
begin
  FController.SelectAll;
end;

function TdxPDFViewerSelection.AsBitmap: TBitmap;
begin
  Result := FController.GetSelectionAsBitmap;
end;

function TdxPDFViewerSelection.AsText: string;
begin
  Result := FController.GetSelectionAsText;
end;

function TdxPDFViewerSelection.IsImageSelection: Boolean;
begin
  Result := (Selection <> nil) and (Selection.HitCode = hcImage);
end;

function TdxPDFViewerSelection.IsTextSelection: Boolean;
begin
  Result := (Selection <> nil) and (Selection.HitCode = hcTextSelection);
end;

function TdxPDFViewerSelection.GetSelection: TdxPDFCustomSelection;
begin
  Result := FController.Selection;
end;

procedure TdxPDFViewerSelection.SetSelection(const AValue: TdxPDFCustomSelection);
begin
  FController.Selection := AValue;
end;

procedure TdxPDFViewerSelection.MakeVisible(AMake: Boolean);
begin
  if AMake then
    FController.MakeVisible;
end;

{ TdxPDFViewerTextSearch }

function TdxPDFViewerTextSearch.Find(const AText: string): TdxPDFDocumentTextSearchResult;
begin
  Result := Find(AText, FOptions);
end;

function TdxPDFViewerTextSearch.Find(const AText: string;
  const AOptions: TdxPDFDocumentTextSearchOptions): TdxPDFDocumentTextSearchResult;
begin
  FSearchString := AText;
  FOptions := AOptions;
  Result := InternalFind;
end;

procedure TdxPDFViewerTextSearch.Find(const AText: string; const AOptions: TdxPDFDocumentTextSearchOptions;
  var AFoundRanges: TdxPDFPageTextRanges);
begin
  FSearchString := AText;
  FOptions := AOptions;
  TdxPDFDocumentSequentialTextSearchAccess(FAdvancedTextSearch).Clear;
  TdxPDFDocumentSequentialTextSearchAccess(FAdvancedTextSearch).Find(Viewer.Document, AText, AOptions, 0);
  AFoundRanges := FAdvancedTextSearch.FoundRanges;
end;

procedure TdxPDFViewerTextSearch.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FAdvancedTextSearch := TdxPDFDocumentContinuousTextSearch.Create;
  FAdvancedTextSearch.OnComplete := AdvancedSearchCompleteHandler;
  FTextSearch := TdxPDFDocumentSequentialTextSearch.Create;
end;

procedure TdxPDFViewerTextSearch.DestroySubClasses;
begin
  FreeAndNil(FTextSearch);
  FreeAndNil(FAdvancedTextSearch);
  inherited DestroySubClasses;
end;

function TdxPDFViewerTextSearch.InternalFind: TdxPDFDocumentTextSearchResult;
var
  AMessage: string;
begin
  if Viewer.IsDocumentLoaded then
  begin
    Result := DoFind;
    if Result.Status = tssFound then
      Selection.SelectText(Result.Range)
    else
    begin
      if Result.Status = tssNotFound then
        AMessage := GetSearchNoMatchesFoundMessage
      else
        AMessage := GetSearchCompleteMessage;
      Application.MessageBox(PChar(AMessage), PChar(Application.Title), MB_OK OR MB_ICONINFORMATION);
      if Viewer.IsFindPanelVisible then
        Viewer.SetFindPanelFocus;
      Selection.Clear;
    end;
  end;
end;

function TdxPDFViewerTextSearch.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxPDFViewerTextSearch.BeginUpdate;
begin
  Inc(FLockCount);
  Viewer.ViewInfo.UpdateState;
end;

procedure TdxPDFViewerTextSearch.Clear;
begin
  TdxPDFDocumentSequentialTextSearchAccess(DocumentTextSearch).Clear;
end;

procedure TdxPDFViewerTextSearch.EndUpdate;
begin
  Dec(FLockCount);
  if not IsLocked then
    Viewer.ViewInfo.UpdateState;
end;

function TdxPDFViewerTextSearch.DoFind: TdxPDFDocumentTextSearchResult;
begin
  BeginUpdate;
  try
    Result := TdxPDFDocumentSequentialTextSearchAccess(DocumentTextSearch).Find(Viewer.Document, FSearchString,
      FOptions, Viewer.CurrentPageIndex);
  finally
    EndUpdate;
  end;
end;

procedure TdxPDFViewerTextSearch.AdvancedSearchCompleteHandler(Sender: TObject);
var
  ATextSearch: TdxPDFDocumentContinuousTextSearch;
begin
  if DocumentTextSearch is TdxPDFDocumentContinuousTextSearch then
  begin
    ATextSearch := TdxPDFDocumentContinuousTextSearch(DocumentTextSearch);
    Viewer.BeginUpdate;
    try
      Viewer.Highlights.Clear;
      Viewer.Highlights.Add(ATextSearch.FoundRanges, dxacDefault, dxacDefault);
    finally
      Viewer.EndUpdate;
    end;
  end;
end;

function TdxPDFViewerTextSearch.GetDocumentTextSearch: TdxPDFDocumentSequentialTextSearch;
begin
  if Viewer.OptionsFindPanel.HighlightSearchResults then
    Result := FAdvancedTextSearch
  else
    Result := FTextSearch;
  Result.OnProgress := Viewer.OnSearchProgress;
end;

function TdxPDFViewerTextSearch.GetSelection: TdxPDFViewerSelection;
begin
  Result := Viewer.Selection;
end;

function TdxPDFViewerTextSearch.GetSearchCompleteMessage: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerTextSearchingCompleteMessage);
end;

function TdxPDFViewerTextSearch.GetSearchNoMatchesFoundMessage: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerTextSearchingNoMatchesFoundMessage);
end;

{ TdxPDFViewerHighlights }

constructor TdxPDFViewerHighlights.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FController := AViewer.SelectionController;
  FItems := TObjectList<TdxPDFTextHighlight>.Create;
  FVisible := bDefault;
end;

destructor TdxPDFViewerHighlights.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxPDFViewerHighlights.Add(const ARange: TdxPDFPageTextRange; ABackColor, AFrameColor: TdxAlphaColor);
begin
  InternalAdd(FController.CreateHighlight(ARange, ABackColor, AFrameColor));
end;

procedure TdxPDFViewerHighlights.Add(const ARanges: TdxPDFPageTextRanges; ABackColor, AFrameColor: TdxAlphaColor);
begin
  InternalAdd(FController.CreateHighlight(ARanges, ABackColor, AFrameColor));
end;

procedure TdxPDFViewerHighlights.Remove(const ARange: TdxPDFPageTextRange);
var
  AExcluded, AFound: Boolean;
  AHighlight: TdxPDFTextHighlight;
begin
  AFound := False;
  for AHighlight in Items do
  begin
    AExcluded := TdxPDFTextHighlightAccess(AHighlight).Exclude(ARange);
    if not AFound then
      AFound := AExcluded;
  end;
  if AFound then
    Changed;
end;

procedure TdxPDFViewerHighlights.Clear;
begin
  FItems.Clear;
  Changed;
end;

procedure TdxPDFViewerHighlights.SetVisible(const AValue: TdxDefaultBoolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerHighlights.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

procedure TdxPDFViewerHighlights.InternalAdd(AValue: TdxPDFTextHighlight);
begin
  if (AValue <> nil) and not Items.Contains(AValue) then
  begin
    Items.Add(AValue);
    Changed;
  end;
end;

{ TdxPDFViewerCustomHitTest }

procedure TdxPDFViewerCustomHitTest.DestroySubClasses;
begin
  Clear;
  inherited DestroySubClasses;
end;

function TdxPDFViewerCustomHitTest.CanCalculate: Boolean;
begin
  Result := True;
end;

function TdxPDFViewerCustomHitTest.GetPopupMenuClass: TComponentClass;
begin
  Result := nil;
end;

procedure TdxPDFViewerCustomHitTest.Clear;
begin
  FHitPoint := cxInvalidPoint;
end;

procedure TdxPDFViewerCustomHitTest.DoCalculate(const AHitPoint: TPoint);
begin
  FHitPoint := AHitPoint;
end;

procedure TdxPDFViewerCustomHitTest.Calculate(const AHitPoint: TPoint);
begin
  Clear;
  if CanCalculate then
    DoCalculate(AHitPoint);
end;

{ TdxPDFViewerDocumentHitTest }

function TdxPDFViewerDocumentHitTest.CanCalculate: Boolean;
begin
  Result := not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerDocumentHitTest.DoGetCursor: TCursor;
var
  AIntf: IdxPDFInteractiveObject;
begin
  Result := inherited DoGetCursor;
  if HitAtNavigationPaneSplitter then
    Result := crHSplit
  else
    if TryGetDocumentHitObjectAsInteractiveObject(AIntf) then
      Result := AIntf.GetCursor
    else
      if HitAtSelection then
        Result := crdxPDFViewerContext
      else
        if HitAtImage or Viewer.IsDocumentSelecting and Viewer.Selection.IsImageSelection then
          Result := crdxPDFViewerCross
        else
          if HitAtText then
            Result := crIBeam;
end;

function TdxPDFViewerDocumentHitTest.GetPopupMenuClass: TComponentClass;
begin
  if HitAtImage and HitAtSelection then
    Result := TdxPDFViewerImagePopupMenu
  else
    if HitAtText and HitAtSelection then
      Result := TdxPDFViewerTextPopupMenu
    else
      if HitAtAttachment then
        Result := TdxPDFViewerAttachmentPopupMenu
      else
          Result := TdxPDFViewerPagePopupMenu;
end;

procedure TdxPDFViewerDocumentHitTest.Clear;
begin
  inherited Clear;
  FPosition.PageIndex := -1;
  FPosition.Point := dxNullPointF;
  DocumentHitObject := nil;
end;

procedure TdxPDFViewerDocumentHitTest.DoCalculate(const AHitPoint: TPoint);
var
  APage: TdxPDFViewerPage;
begin
  inherited DoCalculate(AHitPoint);
  Viewer.ViewInfo.CalculateHitTest(Self);
  HitCodes[hcNavigationPaneSplitter] := Viewer.NavigationPane.Controller.InPaneSplitterRect(HitPoint);
  HitCodes[hcFindPanel] := Viewer.IsFindPanelVisible and Viewer.ViewerController.InFindPanelRect(HitPoint);
  APage := GetPage;
  HitCodes[hcBackground] := APage = nil;
  if HitAtBackground then
    APage := GetNearestPage;
  if APage <> nil then
  begin
    FPosition.PageIndex := APage.Index;
    FPosition.Point := APage.ToDocumentPoint(dxPointF(HitPoint));
    if HitAtDocumentViewer then
    begin
      HitCodes[hcPageArea] := not HitAtBackground;
      FDocumentHitObject := nil;
      if HitAtPage and Viewer.IsDocumentLoaded and not Viewer.IsZooming then
        DocumentHitObject := GetDocumentHitObject(APage.DocumentPage);
      if DocumentHitObject <> nil then
        HitCodes[DocumentHitObject.HitCode] := True;
      HitCodes[hcSelectionFrame] := GetHitAtSelection;
    end;
  end;
end;

procedure TdxPDFViewerDocumentHitTest.ResetPreviousRecognizedPage;
begin
  FPreviousRecognizedPage := nil;
end;

function TdxPDFViewerDocumentHitTest.CanShowHint;
begin
  Result := GetHintText <> '';
end;

function TdxPDFViewerDocumentHitTest.GetHintBounds: TRect;
var
  AIntf: IdxPDFInteractiveObject;
begin
  Calculate(Viewer.GetMouseCursorClientPos);
  if Position.IsValid and TryGetDocumentHitObjectAsInteractiveObject(AIntf) then
    Result := Viewer.DocumentState.ToViewerRect(AIntf)
  else
    Result := cxNullRect;
end;

function TdxPDFViewerDocumentHitTest.GetHintText: string;
var
  AIntf: IdxPDFHintableObject;
begin
  if TryGetDocumentHitObjectAsHintableObject(AIntf) then
    Result := AIntf.GetHint
  else
    Result := '';
end;

function TdxPDFViewerDocumentHitTest.HitAtHintableObject: Boolean;
var
  AIntf: IdxPDFHintableObject;
begin
  Result := TryGetDocumentHitObjectAsHintableObject(AIntf);
end;

function TdxPDFViewerDocumentHitTest.HitAtInteractiveObject: Boolean;
var
  AIntf: IdxPDFInteractiveObject;
begin
  Result := TryGetDocumentHitObjectAsInteractiveObject(AIntf);
end;

function TdxPDFViewerDocumentHitTest.GetDocumentHitObject(APage: TdxPDFPage): TdxPDFRecognizedObject;

  function CanCalculate: Boolean;
  begin
    Result := not TdxPDFPageAccess(APage).Locked;
  end;

var
  AHyperlink: TdxPDFHyperlink;
begin
  AHyperlink := nil;
  Result := AHyperlink;
  if CanCalculate then
  begin
    if APage.FindHyperlink(Position.Point, Viewer.DocumentScaleFactor, AHyperlink) then
      Result := AHyperlink
    else
      Result := APage.Find(Position, Viewer.DocumentScaleFactor);
  end;
end;

function TdxPDFViewerDocumentHitTest.GetHitAtDocumentViewer: Boolean;
begin
  Result := not (Viewer.NavigationPane.Controller.InPaneRect(HitPoint) or HitAtNavigationPaneSplitter or
    (Viewer.DragAndDropState = ddsInProcess));
end;

function TdxPDFViewerDocumentHitTest.GetHitAtSelection: Boolean;
begin
  Result := Viewer.SelectionController.HitAtSelection;
end;

function TdxPDFViewerDocumentHitTest.GetPage: TdxPDFViewerPage;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Viewer.PageCount - 1 do
    if PtInRect(Viewer.Pages[I].Bounds, HitPoint) then
      Exit(Viewer.Pages[I] as TdxPDFViewerPage);
end;

function TdxPDFViewerDocumentHitTest.GetNearestPage: TdxPDFViewerPage;
var
  APageIndex: Integer;
begin
  Result := nil;
  if Viewer.IsDocumentLoaded and (Viewer.Document.PageCount > 0) then
  begin
    APageIndex := Viewer.GetNearestPageIndex(HitPoint);
    Result := Viewer.Pages[IfThen(APageIndex = -1, 0, APageIndex)] as TdxPDFViewerPage;
  end;
end;

function TdxPDFViewerDocumentHitTest.TryGetDocumentHitObjectAsInteractiveObject(out AIntf: IdxPDFInteractiveObject): Boolean;
begin
  Result := Supports(DocumentHitObject, IdxPDFInteractiveObject, AIntf);
end;

function TdxPDFViewerDocumentHitTest.TryGetDocumentHitObjectAsHintableObject(out AIntf: IdxPDFHintableObject): Boolean;
begin
  Result := Supports(DocumentHitObject, IdxPDFHintableObject, AIntf);
end;

procedure TdxPDFViewerDocumentHitTest.SetDocumentHitObject(const AValue: TdxPDFRecognizedObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDocumentHitObject));
end;

{ TdxPDFViewerNavigationPaneHitTest }

function TdxPDFViewerNavigationPaneHitTest.DoGetCursor: TCursor;
begin
  Result := inherited DoGetCursor;
  if Viewer.NavigationPane.Controller.InPaneSplitterRect(HitPoint) and HitAtSplitter then
    Result := crHSplit;
end;

procedure TdxPDFViewerNavigationPaneHitTest.DoCalculate(const AHitPoint: TPoint);
begin
  inherited DoCalculate(AHitPoint);
  Viewer.ViewInfo.NavigationPane.CalculateHitTest(Self);
end;

{ TdxPDFViewerCustomController }

function TdxPDFViewerCustomController.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPDFViewerCustomController.GetPopupMenuClass: TComponentClass;
begin
  Result := nil;
end;

function TdxPDFViewerCustomController.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxPDFViewerCustomController.CalculateMouseButtonPressed(Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonPressed := ssLeft in Shift;
end;

procedure TdxPDFViewerCustomController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalculateMouseButtonPressed(Shift, X, Y);
end;

procedure TdxPDFViewerCustomController.DoMouseEnter(AControl: TControl);
begin
// do nothing
end;

procedure TdxPDFViewerCustomController.DoMouseLeave(AControl: TControl);
begin
// do nothing
end;

procedure TdxPDFViewerCustomController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
// do nothing
end;

procedure TdxPDFViewerCustomController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonPressed := False;
end;

procedure TdxPDFViewerCustomController.UpdateCursor;
begin
// do nothing
end;

procedure TdxPDFViewerCustomController.UpdateStates;
begin
// do nothing
end;

function TdxPDFViewerCustomController.ContextPopup(const P: TPoint): Boolean;
var
  APopupMenuClass: TComponentClass;
begin
  APopupMenuClass := GetPopupMenuClass;
  if (APopupMenuClass <> nil) and ((FPopupMenu = nil) or (FPopupMenu.ClassType <> APopupMenuClass)) then
  begin
    FreeAndNil(FPopupMenu);
    if APopupMenuClass <> nil then
      FPopupMenu := APopupMenuClass.Create(Viewer);
  end;
  Result := (FPopupMenu <> nil) and (FPopupMenu as TdxPDFViewerCustomPopupMenu).Popup(P);
end;

function TdxPDFViewerCustomController.MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TdxPDFViewerCustomController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(Button, Shift, X, Y);
  UpdateStates;
end;

procedure TdxPDFViewerCustomController.MouseEnter(AControl: TControl);
begin
  DoMouseEnter(AControl);
end;

procedure TdxPDFViewerCustomController.MouseLeave(AControl: TControl);
begin
  DoMouseLeave(AControl);
  UpdateStates;
end;

procedure TdxPDFViewerCustomController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(Shift, X, Y);
  UpdateStates;
end;

procedure TdxPDFViewerCustomController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, X, Y);
  UpdateStates;
end;

{ TdxPDFViewerViewState }

constructor TdxPDFViewerViewState.Create(AChangeType: TdxPDFViewerViewStateChangeType);
begin
  inherited Create;
  FChangeType := AChangeType;
  FTimeStamp := DateTimeToTimeStamp(Now);
end;

class function TdxPDFViewerViewState.CalculatePageSize(const ASize: TdxPointF; ARotationAngle: TcxRotationAngle): TdxPointF;
begin
  if (ARotationAngle = raPlus90) or (ARotationAngle = raMinus90) then
    Result := dxPointF(ASize.Y, ASize.X)
  else
    Result := ASize;
end;

function TdxPDFViewerViewState.CalculatePageSize(APage: TdxPDFPage): TdxPointF;
begin
  Result := TdxPDFViewerViewState.CalculatePageSize(APage.Size, RotationAngle);
end;

function TdxPDFViewerViewState.IsSame(AView: TdxPDFViewerViewState): Boolean;
begin
  Result := (ChangeType = AView.ChangeType) and (ChangeType in [vsctZooming, vsctScrolling, vsctSelecting]) and
    (AView.TimeStamp.Date = TimeStamp.Date) and (AView.TimeStamp.Time - TimeStamp.Time <= dxPDFViewerViewCreationTimeOut);
end;

{ TdxPDFViewerViewStateHistory }

constructor TdxPDFViewerViewStateHistory.Create;
begin
  inherited Create;
  FViewStateList := TObjectList<TdxPDFViewerViewState>.Create;
end;

destructor TdxPDFViewerViewStateHistory.Destroy;
begin
  FreeAndNil(FViewStateList);
  inherited Destroy;
end;

function TdxPDFViewerViewStateHistory.CanGoToNextView: Boolean;
begin
  Result := FCurrentViewStateIndex < FViewStateList.Count - 1;
end;

function TdxPDFViewerViewStateHistory.CanGoToPreviousView: Boolean;
begin
  Result := FCurrentViewStateIndex > 0;
end;

procedure TdxPDFViewerViewStateHistory.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxPDFViewerViewStateHistory.Clear;
begin
  FCurrentViewStateIndex := 0;
  FViewStateList.Clear;
end;

procedure TdxPDFViewerViewStateHistory.EndUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxPDFViewerViewStateHistory.GoToNextView;
begin
  if CanGoToNextView then
    Inc(FCurrentViewStateIndex);
end;

procedure TdxPDFViewerViewStateHistory.GoToPrevView;
begin
  if CanGoToPreviousView then
    Dec(FCurrentViewStateIndex);
end;

procedure TdxPDFViewerViewStateHistory.Initialize(AView: TdxPDFViewerViewState);
begin
  Clear;
  FViewStateList.Add(AView);
end;

procedure TdxPDFViewerViewStateHistory.StoreViewState(AView: TdxPDFViewerViewState);
var
  ACountToDelete: Integer;
begin
  if (FLockCount = 0) and not SameView(AView, CurrentViewState) then
  begin
    if CurrentViewState.IsSame(AView) then
      GoToPrevView;
    Inc(FCurrentViewStateIndex);
    ACountToDelete := FViewStateList.Count - FCurrentViewStateIndex;
    if ACountToDelete > 0 then
      FViewStateList.DeleteRange(FCurrentViewStateIndex, ACountToDelete);
    if not SameView(AView, FViewStateList[FCurrentViewStateIndex - 1]) then
      FViewStateList.Add(AView)
    else
    begin
      Dec(FCurrentViewStateIndex);
      AView.Free;
    end;
  end
  else
    AView.Free;
end;

function TdxPDFViewerViewStateHistory.GetCurrentViewState: TdxPDFViewerViewState;
begin
  Result := FViewStateList[FCurrentViewStateIndex];
end;

function TdxPDFViewerViewStateHistory.GetCount: Integer;
begin
  Result := FViewStateList.Count;
end;

function TdxPDFViewerViewStateHistory.SameView(AView, ACurrentView: TdxPDFViewerViewState): Boolean;
begin
  Result := (ACurrentView.ChangeType = AView.ChangeType) and (ACurrentView.RotationAngle = AView.RotationAngle) and
    (ACurrentView.ZoomFactor = AView.ZoomFactor) and (ACurrentView.ZoomMode = AView.ZoomMode) and
    (ACurrentView.CurrentPageIndex = AView.CurrentPageIndex) and
    cxPointIsEqual(ACurrentView.ScrollPosition, AView.ScrollPosition);
end;

{ TdxPDFViewerViewStateHistoryController }

function TdxPDFViewerViewStateHistoryController.CanGoToNextView: Boolean;
begin
  Result := FHistory.CanGoToNextView;
end;

function TdxPDFViewerViewStateHistoryController.CanGoToPreviousView: Boolean;
begin
  Result := FHistory.CanGoToPreviousView;
end;

procedure TdxPDFViewerViewStateHistoryController.Clear;
begin
  RecreateHistory;
end;

procedure TdxPDFViewerViewStateHistoryController.GoToNextView;
begin
  FHistory.GoToNextView;
  RestoreViewState;
end;

procedure TdxPDFViewerViewStateHistoryController.GoToPrevView;
begin
  FHistory.GoToPrevView;
  RestoreViewState;
end;

procedure TdxPDFViewerViewStateHistoryController.StoreCurrentViewState(AChangeType: TdxPDFViewerViewStateChangeType);
begin
  if not Viewer.IsUpdateLocked then
    FHistory.StoreViewState(CreateView(AChangeType));
end;

procedure TdxPDFViewerViewStateHistoryController.RestoreViewState;
begin
  FHistory.BeginUpdate;
  try
    Viewer.BeginUpdate;
    try
      Viewer.RotationAngle := FHistory.CurrentViewState.RotationAngle;
      Viewer.ZoomFactor := FHistory.CurrentViewState.ZoomFactor;
      Viewer.CurrentPageIndex := FHistory.CurrentViewState.CurrentPageIndex;
      Viewer.SetLeftTop(FHistory.CurrentViewState.ScrollPosition);
    finally
      Viewer.EndUpdate;
    end;
    Viewer.ZoomMode := FHistory.CurrentViewState.ZoomMode;
  finally
    FHistory.EndUpdate;
  end;
end;

procedure TdxPDFViewerViewStateHistoryController.CreateSubClasses;
begin
  inherited CreateSubClasses;
  RecreateHistory;
end;

procedure TdxPDFViewerViewStateHistoryController.DestroySubClasses;
begin
  FreeAndNil(FHistory);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerViewStateHistoryController.BeginUpdate;
begin
  FHistory.BeginUpdate;
end;

procedure TdxPDFViewerViewStateHistoryController.EndUpdate;
begin
  FHistory.EndUpdate;
end;

function TdxPDFViewerViewStateHistoryController.CreateView(AChangeType: TdxPDFViewerViewStateChangeType): TdxPDFViewerViewState;
begin
  Result := TdxPDFViewerViewState.Create(AChangeType);
  Result.CurrentPageIndex := Viewer.CurrentPageIndex;
  Result.ScrollPosition := cxPoint(Viewer.LeftPos, Viewer.TopPos);
  Result.RotationAngle := Viewer.RotationAngle;
  Result.ZoomFactor := Viewer.ZoomFactor;
  Result.ZoomMode := Viewer.ZoomMode;
end;

function TdxPDFViewerViewStateHistoryController.GetCurrentViewState: TdxPDFViewerViewState;
begin
  Result := FHistory.CurrentViewState;
end;

procedure TdxPDFViewerViewStateHistoryController.RecreateHistory;
begin
  if FHistory <> nil then
    FreeAndNil(FHistory);
  FHistory := TdxPDFViewerViewStateHistory.Create(Viewer);
  FHistory.Initialize(CreateView(vsctNone));
end;

{ TdxPDFViewerContentSelector }

constructor TdxPDFViewerContentSelector.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
end;

procedure TdxPDFViewerContentSelector.Clear;
begin
  FInProgress := False;
end;

procedure TdxPDFViewerContentSelector.Reset;
begin
  FInProgress := False;
end;

function TdxPDFViewerContentSelector.GetHitTest: TdxPDFViewerDocumentHitTest;
begin
  Result := Viewer.HitTest;
end;

function TdxPDFViewerContentSelector.GetSelection: TdxPDFCustomSelection;
begin
  Result := FViewer.Selection.Selection;
end;

function TdxPDFViewerContentSelector.GetSelectionBackColor: TdxAlphaColor;
begin
  Result := dxacDefault;
end;

function TdxPDFViewerContentSelector.GetSelectionFrameColor: TdxAlphaColor;
begin
  Result := dxacDefault;
end;

procedure TdxPDFViewerContentSelector.SetSelection(const AValue: TdxPDFCustomSelection);
begin
  FViewer.Selection.Selection := AValue;
end;

{ TdxPDFViewerImageSelector }

procedure TdxPDFViewerImageSelector.Clear;
begin
  inherited Clear;
  FIsSelected := False;
end;

procedure TdxPDFViewerImageSelector.Reset;
begin
  inherited Reset;
  FSelectedImage := nil;
  FSelectedImagePageIndex := -1;
  FStartPosition.Invalid;
end;

function TdxPDFViewerImageSelector.Select: Boolean;
const
  Precision = 0.001;
var
  ASelectionRect, AImageBounds: TdxRectF;
  AStartPositionPage: TdxPDFDocumentCustomViewerPage;
begin
  Result := not FIsSelected and (FSelectedImage <> nil);
  if Result then
  begin
    if (Abs(FStartPosition.Point.X - HitTest.Position.Point.X) < Precision) or
      (Abs(FStartPosition.Point.Y - HitTest.Position.Point.Y) < Precision) then
    begin
      if not InProgress then
        Selection := CreateImageSelection(dxNullRectF);
    end
    else
      if InProgress then
      begin
        AImageBounds := TdxPDFImageAccess(FSelectedImage).Bounds;
        AStartPositionPage := Viewer.PageList[FStartPosition.PageIndex] as TdxPDFDocumentCustomViewerPage;
        ASelectionRect.TopLeft := Viewer.DocumentState.ToViewerPoint(AStartPositionPage, FStartPosition.Point);
        ASelectionRect.BottomRight := HitTest.HitPoint;
        ASelectionRect.Normalize;
        AImageBounds := Viewer.DocumentState.ToViewerRect(AStartPositionPage, AImageBounds);
        ASelectionRect.Intersect(AImageBounds);
        ASelectionRect := Viewer.DocumentState.ToDocumentRect(AStartPositionPage, ASelectionRect);
        ASelectionRect.Normalize;
        Selection := CreateImageSelection(ASelectionRect);
      end;
  end;
end;

procedure TdxPDFViewerImageSelector.StartSelection(var AInOutsideContent: Boolean);
begin
  FStartPosition := HitTest.Position;
  InProgress := HitTest.HitAtImage;
  if HitTest.HitAtImage then
    FSelectedImage := HitTest.DocumentHitObject as TdxPDFImage
  else
    FSelectedImage := nil;
  FSelectedImagePageIndex := HitTest.Position.PageIndex;
  AInOutsideContent := InProgress;
end;

function TdxPDFViewerImageSelector.CreateImageSelection(const ABounds: TdxRectF): TdxPDFImageSelection;
begin
  Result := TdxPDFImageSelection.Create(SelectionBackColor, SelectionFrameColor, FSelectedImagePageIndex,
    FSelectedImage, ABounds);
end;

{ TdxPDFViewerTextSelector }

function TdxPDFViewerTextSelector.CreateTextHighlights(const ARanges: TdxPDFPageTextRanges;
  ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight;
begin
  if ValidateRanges(ARanges) then
    Result := TdxPDFTextHighlight.Create(ABackColor, AFrameColor, Viewer.DocumentPages, ARanges)
  else
    Result := nil;
end;

function TdxPDFViewerTextSelector.CreateTextSelection(const ARange: TdxPDFPageTextRange): TdxPDFTextSelection;
var
  ARanges: TdxPDFPageTextRanges;
begin
  SetLength(ARanges, 1);
  ARanges[0] := ARange;
  Result := CreateTextSelection(ARanges);
end;

function TdxPDFViewerTextSelector.CreateTextSelection(const ARanges: TdxPDFPageTextRanges): TdxPDFTextSelection;
begin
  if ValidateRanges(ARanges) then
    Result := TdxPDFTextSelection.Create(SelectionBackColor, SelectionFrameColor, Viewer.DocumentPages, ARanges)
  else
    Result := nil;
end;

function TdxPDFViewerTextSelector.GetCaretViewData(const APosition: TdxPDFTextPosition): TdxPDFDocumentCaretViewData;
var
  ACurrentPart: TdxPDFTextWordPart;
  AFoundLine: TdxPDFTextLine;
  ALine: TdxPDFTextLineAccess;
  ALocation: TdxPointF;
  AOffset: Integer;
  X, Y, AK0, K: Double;
  AWordPart: TdxPDFTextWordPartAccess;
begin
  Result := TdxPDFDocumentCaretViewData.Create(dxPointF(0, 0), 0, 0);
  if FindLine(APosition, AFoundLine) then
  begin
    ALine := TdxPDFTextLineAccess(AFoundLine);
    for ACurrentPart in ALine.WordPartList do
    begin
      AWordPart := TdxPDFTextWordPartAccess(ACurrentPart);
      if AWordPart.Same(APosition.Position.WordIndex, APosition.Position.Offset) then
      begin
        AOffset := APosition.Position.Offset - AWordPart.WrapOffset;
        if AOffset = Length(AWordPart.Characters) then
          ALocation := AWordPart.Characters[AOffset - 1].Bounds.TopRight
        else
          ALocation := AWordPart.Characters[AOffset].Bounds.TopLeft;
        if (AWordPart.Bounds.Angle = 0) or (AWordPart.Bounds.Angle = PI) then
        begin
          X := ALocation.X;
          Y := AWordPart.Bounds.Top;
        end
        else
          if (AWordPart.Bounds.Angle = 3 * PI / 2) or (AWordPart.Bounds.Angle = PI / 2) then
          begin
            X := AWordPart.Bounds.Left;
            Y := ALocation.Y;
          end
          else
          begin
            AK0 := Tan(AWordPart.Bounds.Angle);
            K := Tan(AWordPart.Bounds.Angle + PI / 2);
            X := (AWordPart.Bounds.Left * AK0 - AWordPart.Bounds.Top - ALocation.X * K + ALocation.Y) / (AK0 - K);
            Y := AK0 * (X - AWordPart.Bounds.Left) + AWordPart.Bounds.Top;
          end;
        Exit(TdxPDFDocumentCaretViewData.Create(dxPointF(X, Y), AWordPart.Bounds.Height, ALine.Bounds.Angle));
      end;
    end;
    Result := TdxPDFDocumentCaretViewData.Create(dxPointF(cxNullPoint), 0, TdxPDFTextObjectAccess(ALine).Bounds.Angle);
  end;
end;

function TdxPDFViewerTextSelector.StartSelection(const APosition: TdxPDFPosition): Boolean;
var
  ATextPosition: TdxPDFTextPosition;
begin
  ATextPosition := Page[APosition.PageIndex].FindStartTextPosition(APosition, ScaleFactor);
  InProgress := ATextPosition.IsValid;
  if not InProgress then
  begin
    FStartPageIndex := -1;
    FStartPoint := dxPointF(cxNullPoint);
    if Viewer.SelectionController.Caret.IsValid then
      Caret := TdxPDFDocumentCaret.Invalid;
  end
  else
  begin
    FStartPageIndex := APosition.PageIndex;
    FStartPoint := APosition.Point;
    UpdateSelection(ATextPosition);
  end;
  Result := InProgress;
end;

procedure TdxPDFViewerTextSelector.MoveCaret(const APosition: TdxPDFTextPosition);
var
  AData: TdxPDFDocumentCaretViewData;
begin
  AData := GetCaretViewData(APosition);
  Caret := TdxPDFDocumentCaret.Create(APosition, AData, AData.TopLeft);
end;

procedure TdxPDFViewerTextSelector.MoveCaret(ADirection: TdxPDFMovementDirection);
var
  ACaretDirection: TdxPDFMovementDirection;
begin
  if IsArrowKeys(ADirection) then
    ACaretDirection := NormalizeDirection(ADirection)
  else
    ACaretDirection := ADirection;
  case ACaretDirection of
    mdLeft:
      if not MoveCaretToLeft then
        DoMoveCaret(MoveLeft);
    mdRight:
      if not MoveCaretToRight and Caret.IsValid then
        DoMoveCaret(MoveRight);
    mdUp:
      begin
        MoveCaretToLeft;
        DoMoveCaret(MoveUp);
      end;
    mdDown:
      begin
        MoveCaretToRight;
        DoMoveCaret(MoveDown);
      end;
    mdNextWord:
      DoMoveCaret(MoveToNextWord);
    mdPreviousWord:
      DoMoveCaret(MoveToPreviousWord);
    mdLineStart:
      DoMoveCaret(MoveToLineStart);
    mdLineEnd:
      DoMoveCaret(MoveToLineEnd);
    mdDocumentStart:
      DoMoveCaret(MoveToDocumentStart);
    mdDocumentEnd:
      DoMoveCaret(MoveToDocumentEnd);
  end;
end;

procedure TdxPDFViewerTextSelector.Select(const APosition: TdxPDFPosition);
var
  AEnd: TdxPDFPageTextPosition;
  ARange: TdxPDFPageTextRange;
  ARanges: TdxPDFPageTextRanges;
  ATextPosition: TdxPDFPageTextPosition;
begin
  if InProgress and ((APosition.PageIndex <> FStartPageIndex) or not cxPointIsEqual(APosition.Point, FStartPoint)) then
  begin
    StoreSelectionStartTextPosition;
    ARanges := CreateRangeList(FStartTextPosition, APosition);
    if Length(ARanges) > 0 then
    begin
      ARange := ARanges[Length(ARanges) - 1];
      AEnd := ARange.EndPosition;
      if AEnd.IsSame(FStartTextPosition.Position) then
        ATextPosition := ARange.StartPosition
      else
        ATextPosition := AEnd;
      MoveCaret(TdxPDFTextPosition.Create(ARange.PageIndex, ATextPosition));
      Selection := CreateTextSelection(ARanges);
    end
  end;
end;

procedure TdxPDFViewerTextSelector.SelectByKeyboard(ADirection: TdxPDFMovementDirection);
var
  ACaretDirection: TdxPDFMovementDirection;
  ARanges: TdxPDFPageTextRanges;
begin
  if IsArrowKeys(ADirection) then
    ACaretDirection := NormalizeDirection(ADirection)
  else
    ACaretDirection := ADirection;
  StoreSelectionStartTextPosition;
  case ACaretDirection of
    mdLeft:
      MoveLeft;
    mdRight:
      MoveRight;
    mdUp:
      MoveUp;
    mdDown:
      MoveDown;
    mdNextWord:
      MoveToNextWord;
    mdPreviousWord:
      MoveToPreviousWord;
    mdLineStart:
      MoveToLineStart;
    mdLineEnd:
      MoveToLineEnd;
    mdDocumentStart:
      MoveToDocumentStart;
    mdDocumentEnd:
      MoveToDocumentEnd;
  end;
  if Caret.IsValid then
  begin
    ARanges := CreateRangeList(FStartTextPosition, Caret.Position);
    Selection := CreateTextSelection(ARanges);
    MakeCaretVisible;
  end;
end;

procedure TdxPDFViewerTextSelector.SelectLine(const APosition: TdxPDFPosition);
var
  ADocumentPosition: TdxPDFPosition;
  APage: TdxPDFPage;
begin
  ADocumentPosition := APosition;
  APage := Page[APosition.PageIndex];
  Select(APage,
    procedure
    var
      ALine: TdxPDFTextLine;
      AWordPartList: TdxPDFTextWordPartList;
    begin
      if APage.FindLine(ADocumentPosition, ScaleFactor, ALine) then
      begin
        AWordPartList := TdxPDFTextLineAccess(ALine).WordPartList;
        if AWordPartList.Count = 0 then
          Selection := nil
        else
          Selection := CreateTextSelection(TdxPDFPageTextRange.Create(ADocumentPosition.PageIndex,
            AWordPartList[0].WordIndex, 0, AWordPartList.Last.WordIndex, Length(AWordPartList.Last.Characters)));
      end;
    end);
end;

procedure TdxPDFViewerTextSelector.SelectPage(const APosition: TdxPDFPosition);
var
  ADocumentPosition: TdxPDFPosition;
  APage: TdxPDFPage;
begin
  ADocumentPosition := APosition;
  APage := Page[APosition.PageIndex];
  Select(APage,
    procedure
    begin
      if (ADocumentPosition.PageIndex >= 0) and APage.FindStartTextPosition(ADocumentPosition, ScaleFactor).IsValid then
        Selection := CreateTextSelection(TdxPDFPageTextRange.Create(ADocumentPosition.PageIndex));
    end);
end;

procedure TdxPDFViewerTextSelector.SelectWord(const APosition: TdxPDFPosition);
var
  ADocumentPosition: TdxPDFPosition;
  APage: TdxPDFPage;
begin
  ADocumentPosition := APosition;
  APage := Page[APosition.PageIndex];
  Select(APage,
    procedure
    var
      AFoundPosition: TdxPDFTextPosition;
      ARange: TdxPDFPageTextRange;
    begin
      AFoundPosition := APage.FindStartTextPosition(ADocumentPosition, ScaleFactor);
      if AFoundPosition.IsValid then
      begin
        ARange := TdxPDFPageTextRange.Create(AFoundPosition.PageIndex,
          TdxPDFPageTextPosition.Create(AFoundPosition.Position.WordIndex, 0),
          TdxPDFPageTextPosition.Create(AFoundPosition.Position.WordIndex, FindWordEndPosition(AFoundPosition)));
        Selection := CreateTextSelection(ARange);
      end;
    end);
end;

function TdxPDFViewerTextSelector.GetCaret: TdxPDFDocumentCaret;
begin
  Result := Viewer.SelectionController.Caret;
end;

function TdxPDFViewerTextSelector.GetPage(AIndex: Integer): TdxPDFPage;
begin
  Result := Viewer.SelectionController.GetPage(AIndex);
end;

function TdxPDFViewerTextSelector.GetScaleFactor: TdxPointF;
begin
  Result := Viewer.DocumentScaleFactor;
end;

function TdxPDFViewerTextSelector.GetTextLines(APageIndex: Integer): TdxPDFTextLineList;
var
  AContent: TdxPDFRecognizedContent;
begin
  AContent := Page[APageIndex].RecognizedContent;
  if AContent <> nil then
    Result := AContent.TextLines
  else
    Result := nil;
end;

procedure TdxPDFViewerTextSelector.SetCaret(const AValue: TdxPDFDocumentCaret);
begin
  Viewer.SelectionController.Caret := AValue;
end;

function TdxPDFViewerTextSelector.CreateRangeList(const AStart, AEnd: TdxPDFTextPosition): TdxPDFPageTextRanges;
var
  I: Integer;
  AStartPosition, AEndPosition: TdxPDFTextPosition;
begin
  SetLength(Result, 0);
  if AEnd.IsValid then
  begin
    if AStart.PageIndex = AEnd.PageIndex then
    begin
      if (AStart.Position.WordIndex > AEnd.Position.WordIndex) or
        (AStart.Position.WordIndex = AEnd.Position.WordIndex) and (AStart.Position.Offset > AEnd.Position.Offset) then
        TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(AEnd.PageIndex, AEnd.Position, AStart.Position), Result)
      else
        TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(AStart.PageIndex, AStart.Position, AEnd.Position), Result);
    end
    else
    begin
      AStartPosition := AStart;
      AEndPosition := AEnd;
      if AStartPosition.PageIndex > AEndPosition.PageIndex then
      begin
        AStartPosition := AEnd;
        AEndPosition := AStart;
      end;
      TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(AStartPosition.PageIndex, AStartPosition.Position,
        TdxPDFPageTextPosition.Create(0, -1)), Result);

      for I := AStartPosition.PageIndex + 1 to AEndPosition.PageIndex - 1 do
        TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(I), Result);

      TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(AEndPosition.PageIndex, TdxPDFPageTextPosition.Create(0, 0),
        AEndPosition.Position), Result);
    end;
  end;
end;

function TdxPDFViewerTextSelector.CreateRangeList(const AStart: TdxPDFTextPosition;
  const AEnd: TdxPDFPosition): TdxPDFPageTextRanges;
begin
  if AStart.IsValid then
    Result := CreateRangeList(AStart, FindNearestPosition(AEnd, AStart))
  else
    SetLength(Result, 0);
end;

function TdxPDFViewerTextSelector.HasCaret: Boolean;
begin
  Result := Caret.IsValid;
end;

function TdxPDFViewerTextSelector.IsArrowKeys(ADirection: TdxPDFMovementDirection): Boolean;
begin
  Result := ADirection in [mdUp, mdDown, mdLeft, mdRight];
end;

function TdxPDFViewerTextSelector.IsEmptySelection: Boolean;
begin
  Result := Viewer.Selection.IsEmpty;
end;

function TdxPDFViewerTextSelector.IsPositionInLine(APageIndex, ALineIndex, AWordIndex, AOffset: Integer): Boolean;
var
  ALine: TdxPDFTextLineAccess;
  ALines: TdxPDFTextLineList;
  AParts: TdxPDFTextWordPartList;
  ALastPageIndex: Integer;
begin
  ALines := TextLines[APageIndex];

  ALine := TdxPDFTextLineAccess(ALines[ALineIndex]);
  if not ALine.PositionInLine(AWordIndex, AOffset) then
    Exit(False);

  AParts := TdxPDFTextLineAccess(ALine).WordPartList;
  if AParts[AParts.Count - 1].WordIndex <> AWordIndex then
    Exit(True);
  if ALineIndex < ALines.Count - 1 then
    Exit(not TdxPDFTextLineAccess(ALines[ALineIndex + 1]).PositionInLine(AWordIndex, AOffset));

  ALastPageIndex := Viewer.PageCount - 1;
  repeat
    if APageIndex = ALastPageIndex then
      Exit(True);
    ALines := TextLines[APageIndex];
    Inc(APageIndex);
  until not ((ALines = nil) or (ALines.Count = 0));

  ALine := TdxPDFTextLineAccess(ALines[0]);
  Result := not ALine.PositionInLine(AWordIndex, AOffset) or (ALine.WordPartList[0].WordIndex <> AWordIndex);
end;

function TdxPDFViewerTextSelector.FindLine(const APosition: TdxPDFTextPosition; out ALine: TdxPDFTextLine): Boolean;
var
  I: Integer;
begin
  ALine := nil;
  Result := False;
  for I := 0 to Page[APosition.PageIndex].RecognizedContent.TextLines.Count - 1 do
    if IsPositionInLine(APosition.PageIndex, I, APosition.Position.WordIndex, APosition.Position.Offset) then
    begin
      ALine := Page[APosition.PageIndex].RecognizedContent.TextLines[I];
      Exit(True);
    end;
end;

function TdxPDFViewerTextSelector.FindNearestLineByDistance(const APosition: TdxPDFPosition): TdxPDFTextLine;
var
  I: Integer;
  ADistance, AMinDistance: Double;
  ALine: TdxPDFTextLineAccess;
  ALineRect: TdxRectF;
  ALines: TdxPDFTextLineList;
  APart: TdxPDFTextWordPart;
begin
  Result := nil;
  AMinDistance := MaxDouble;
  ALines := TextLines[APosition.PageIndex];
  if ALines <> nil then
  begin
    for I := 0 to ALines.Count - 1  do
    begin
      ALine := TdxPDFTextLineAccess(ALines[I]);
      if ALine.Bounds.PtInRect(APosition.Point) then
        for APart in TdxPDFTextLineAccess(ALine).WordPartList do
          if TdxPDFTextObjectAccess(APart).Bounds.PtInRect(APosition.Point) then
            Exit(ALine);
      ADistance := TdxPDFUtils.DistanceToRect(APosition.Point, ALine.Bounds.RotatedRect);
      if ADistance < AMinDistance then
      begin
        Result := ALine;
        AMinDistance := ADistance;
      end;
    end;
    if Result = nil then
      for I := ALines.Count - 1 downto 0 do
      begin
        ALine := TdxPDFTextLineAccess(ALines[I]);
        ALineRect := ALine.Bounds.RotatedRect;
        if (ALineRect.Bottom <= APosition.Point.Y) and (ALineRect.Right >= APosition.Point.X) then
          Result := ALine;
      end;
  end;
end;

function TdxPDFViewerTextSelector.FindNearestPosition(const APosition: TdxPDFPosition;
  const ATextPosition: TdxPDFTextPosition): TdxPDFTextPosition;
var
  I, AWordIndex, ALastLineIndex: Integer;
  ALines: TdxPDFTextLineList;
  ALine, ANearestLine: TdxPDFTextLineAccess;
begin
  Result := TdxPDFTextPosition.Invalid;
  if APosition.PageIndex >= 0 then
  begin
    ANearestLine := TdxPDFTextLineAccess(FindNearestLineByDistance(APosition));
    if ANearestLine = nil then
    begin
      AWordIndex := ATextPosition.Position.WordIndex;
      ALines := TextLines[ATextPosition.PageIndex];
      for I := 0 to ALines.Count - 1  do
      begin
        ALine := TdxPDFTextLineAccess(ALines[I]);
        begin
          ALastLineIndex := ALine.WordList.Count - 1;
          if ((ALastLineIndex >= 0) and (AWordIndex >= ALine.WordList[0].Index)) and
            (ALine.WordList[ALastLineIndex].Index >= AWordIndex) then
          begin
            ANearestLine := ALine;
            Break;
          end;
        end;
      end;
    end;
    if ANearestLine <> nil then
      Result := ANearestLine.GetPosition(APosition);
  end
end;

function TdxPDFViewerTextSelector.FindWordEndPosition(APosition: TdxPDFTextPosition): Integer;
var
  ALineIndex, ALastWordPartIndex, J: Integer;
  ALine: TdxPDFTextLine;
  AWordParts: TdxPDFTextWordPartList;
  AWordPart, ALastWordPart: TdxPDFTextWordPart;
begin
  Result := -1;
  ALineIndex := 0;
  for ALine in TextLines[APosition.PageIndex] do
  begin
    AWordParts := TdxPDFTextLineAccess(ALine).WordPartList;
    ALastWordPartIndex := AWordParts.Count - 1;
    if ALastWordPartIndex >= 0 then
    begin
      for J := 0 to ALastWordPartIndex - 1 do
      begin
        AWordPart := AWordParts[J];
        if AWordPart.WordIndex = APosition.Position.WordIndex then
          Exit(TdxPDFTextWordPartAccess(AWordPart).EndWordPosition);
      end;
      ALastWordPart := AWordParts[ALastWordPartIndex];
      if (ALastWordPart.WordIndex = APosition.Position.WordIndex) and
        not SameNextWordPartIndex(APosition.PageIndex, ALineIndex, APosition.Position.WordIndex) then
          Exit(TdxPDFTextWordPartAccess(ALastWordPart).EndWordPosition);
    end;
    Inc(ALineIndex);
  end;
end;

function TdxPDFViewerTextSelector.MoveCaretToLeft: Boolean;
var
  ARange: TdxPDFPageTextRange;
begin
  Result := Selection is TdxPDFTextSelection;
  if Result then
  begin
    ARange := TdxPDFTextHighlightAccess(Selection).Ranges[0];
    UpdateSelection(TdxPDFTextPosition.Create(ARange.PageIndex, ARange.StartPosition));
  end;
end;

function TdxPDFViewerTextSelector.MoveCaretToRight: Boolean;
var
  ARange: TdxPDFPageTextRange;
  ARanges: TdxPDFPageTextRanges;
begin
  Result := Selection is TdxPDFTextSelection;
  if Result then
  begin
    ARanges := TdxPDFTextHighlightAccess(Selection).Ranges;
    ARange := ARanges[Length(ARanges) - 1];
    UpdateSelection(TdxPDFTextPosition.Create(ARange.PageIndex, ARange.EndPosition));
  end;
end;

function TdxPDFViewerTextSelector.MoveRight(AWordParts: TdxPDFTextWordPartList;
  APageIndex, ALineIndex, AWordIndex, AOffset: Integer; AProcessLastWordPart: Boolean): Boolean;
var
  ALastWordPartIndex, ALastWordIndex, J: Integer;
  AWordPart: TdxPDFTextWordPart;
begin
  ALastWordPartIndex := AWordParts.Count - 1;
  ALastWordIndex := AWordParts[ALastWordPartIndex].WordIndex;
  if not AProcessLastWordPart then
    Dec(ALastWordPartIndex);
  for J := 0 to ALastWordPartIndex do
  begin
    AWordPart := AWordParts[J];
    if AWordPart.Same(AWordIndex, AOffset) then
    begin
      if (TdxPDFTextWordPartAccess(AWordPart).NextWrapOffset > AOffset) or (((AWordIndex = ALastWordIndex) and
        SameNextWordPartIndex(APageIndex, ALineIndex, AWordIndex))) then
        MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex, AOffset + 1)))
      else
        if TdxPDFTextWordPartAccess(AWordPart).WordEnded then
          MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex + 1, 0)))
        else
          MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex + 1, 1)));
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxPDFViewerTextSelector.NormalizeDirection(ADirection: TdxPDFMovementDirection): TdxPDFMovementDirection;
const
  AngleMap: array[TdxPDFMovementDirection] of Integer = (0, 90, 180, 270, 270, 270, 270, 270, 270, 270);
  DocumentAngleMap: array[TcxRotationAngle] of Integer = (0, 270, 90, 180);
var
  AAngle: Integer;
begin
  AAngle := AngleMap[ADirection];
  AAngle := Trunc(TdxPDFUtils.NormalizeRotate(AAngle + DocumentAngleMap[Viewer.RotationAngle]));
  case AAngle of
    0:
      Result := mdLeft;
    90:
      Result := mdDown;
    180:
      Result := mdRight;
    else
      Result := mdUp;
  end;
end;

function TdxPDFViewerTextSelector.SameNextWordPartIndex(APageIndex, ALineIndex, AWordIndex: Integer): Boolean;
var
  I: Integer;
  ALines: TdxPDFTextLineList;
begin
  ALines := Page[APageIndex].RecognizedContent.TextLines;
  Inc(ALineIndex);
  if ALineIndex < ALines.Count then
    Exit(TdxPDFTextLineAccess(ALines[ALineIndex]).WordPartList[0].WordIndex = AWordIndex);
  for I := APageIndex + 1 to Viewer.PageCount - 1 do
    if Page[I].RecognizedContent.TextLines.Count > 0 then
      Exit(TdxPDFTextLineAccess(Page[I].RecognizedContent.TextLines[I]).WordPartList[0].WordIndex = AWordIndex);
  Result := False;
end;

function TdxPDFViewerTextSelector.ValidateRanges(const ARanges: TdxPDFPageTextRanges): Boolean;
var
  I: Integer;
begin
  Result := Length(ARanges) > 0;
  for I := 0 to Length(ARanges) - 1 do
  begin
    Result := Result and ARanges[I].IsValid;
    if not Result then
      Break;
  end;
end;

procedure TdxPDFViewerTextSelector.DoMoveCaret(AProc: TdxPDFMovingCaretProc);
begin
  if HasCaret and Assigned(AProc) then
  begin
    AProc;
    Selection := nil;
    Viewer.SelectionController.SelectionChanged;
  end;
end;

procedure TdxPDFViewerTextSelector.MakeCaretVisible;
var
  R: TdxRectF;
begin
  if Caret.IsValid then
  begin
    R.TopLeft := Caret.ViewData.TopLeft;
    R.Right := R.TopLeft.X + Caret.ViewData.Height * Sin(Caret.ViewData.Angle);
    R.Bottom := R.Top - Caret.ViewData.Height;
    Viewer.ViewerController.MakeSelectionRectVisible(Caret.Position.PageIndex, R);
  end;
end;

procedure TdxPDFViewerTextSelector.MoveAndMakeCaretVisible(const APosition: TdxPDFTextPosition);
begin
  MoveCaret(APosition);
  MakeCaretVisible;
end;

procedure TdxPDFViewerTextSelector.MoveDown;
var
  ALines: TdxPDFTextLineList;
  ALastLineIndex, AWordIndex, AOffset, I, ANextPageIndex: Integer;
begin
  ALines := TextLines[Caret.Position.PageIndex];
  ALastLineIndex := ALines.Count - 1;
  if ALastLineIndex >= 0 then
  begin
    AWordIndex := Caret.Position.Position.WordIndex;
    AOffset := Caret.Position.Position.Offset;
    for I := 0 to ALastLineIndex - 1 do
      if IsPositionInLine(Caret.Position.PageIndex, I, AWordIndex, AOffset) then
      begin
        SetCaretPosition(TdxPDFTextLineAccess(ALines[I + 1]).GetPosition(Caret.Position.PageIndex, Caret.StartCoordinates));
        Exit;
      end;
    for ANextPageIndex := Caret.Position.PageIndex + 1 to Viewer.PageCount - 1 do
    begin
      ALines := Page[ANextPageIndex].RecognizedContent.TextLines;
      if ALines.Count > 0 then
      begin
        SetCaretPosition(TdxPDFTextLineAccess(ALines[0]).GetPosition(ANextPageIndex, Caret.StartCoordinates));
        Exit;
      end;
    end;
  end;
  SetCaretPosition(Caret.Position);
end;

procedure TdxPDFViewerTextSelector.MoveLeft;
var
  APageIndex, AWordIndex, AOffset, APreviousPageIndex, ALastLineIndex: Integer;
  ALines: TdxPDFTextLineList;
  AWordParts: TdxPDFTextWordPartList;
  AWordPart: TdxPDFTextWordPart;
begin
  APageIndex := Caret.Position.PageIndex;
  AWordIndex := Caret.Position.Position.WordIndex;
  AOffset := Caret.Position.Position.Offset;
  if AOffset > 0 then
    MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex, AOffset - 1)))
  else
    if AWordIndex > 1 then
      MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(
        AWordIndex - 1, FindWordEndPosition(TdxPDFTextPosition.Create(APageIndex, AWordIndex - 1, 0)))))
    else
    begin
      for APreviousPageIndex := APageIndex - 1 downto 0 do
      begin
        ALines := Page[APreviousPageIndex].RecognizedContent.TextLines;
        ALastLineIndex := ALines.Count - 1;
        if ALastLineIndex >= 0 then
        begin
          AWordParts := TdxPDFTextLineAccess(ALines[ALastLineIndex]).WordPartList;
          AWordPart := AWordParts[AWordParts.Count - 1];
          MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APreviousPageIndex,
            TdxPDFPageTextPosition.Create(AWordPart.WordIndex, TdxPDFTextWordPartAccess(AWordPart).NextWrapOffset)));
          Exit;
        end;
      end;
      MoveAndMakeCaretVisible(Caret.Position);
    end;
end;

procedure TdxPDFViewerTextSelector.MoveRight;
var
  APageIndex, ALastLineIndex, AWordIndex, AOffset, I, ALastWordPartIndex, ANextPageIndex: Integer;
  ALines: TdxPDFTextLineList;
  ALine, ALastLine: TdxPDFTextLineAccess;
  AWordParts: TdxPDFTextWordPartList;
  ALastWordPart: TdxPDFTextWordPartAccess;
begin
  APageIndex := Caret.Position.PageIndex;
  ALines := Page[APageIndex].RecognizedContent.TextLines;
  ALastLineIndex := ALines.Count - 1;
  if ALastLineIndex >= 0 then
  begin
    AWordIndex := Caret.Position.Position.WordIndex;
    AOffset := Caret.Position.Position.Offset;
    for I := 0 to ALastLineIndex - 1 do
    begin
      ALine := TdxPDFTextLineAccess(ALines[I]);
      if ALine.PositionInLine(AWordIndex, AOffset) and
        MoveRight(ALine.WordPartList, APageIndex, I, AWordIndex, AOffset, True) then
        Exit;
    end;
    ALastLine := TdxPDFTextLineAccess(ALines[ALastLineIndex]);
    if ALastLine.PositionInLine(AWordIndex, AOffset) then
    begin
      AWordParts := ALastLine.WordPartList;
      if MoveRight(AWordParts, APageIndex, ALastLineIndex, AWordIndex, AOffset, False) then
        Exit;
      ALastWordPartIndex := AWordParts.Count - 1;
      ALastWordPart := TdxPDFTextWordPartAccess(AWordParts[ALastWordPartIndex]);
      if ALastWordPart.Same(AWordIndex, AOffset) then
      begin
        if (ALastWordPart.NextWrapOffset > AOffset) or (((AWordIndex = AWordParts[ALastWordPartIndex].WordIndex) and
          SameNextWordPartIndex(APageIndex, ALastLineIndex, AWordIndex))) then
        begin
          MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex, AOffset + 1)));
          Exit;
        end;
        for ANextPageIndex := APageIndex + 1 to Viewer.PageCount - 1 do
          if Page[ANextPageIndex].RecognizedContent.TextLines.Count > 0 then
          begin
            MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(ANextPageIndex, TdxPDFPageTextPosition.Create(1, 0)));
            Exit;
          end;
      end;
    end;
  end;
  MoveAndMakeCaretVisible(Caret.Position);
end;

procedure TdxPDFViewerTextSelector.MoveToDocumentEnd;
var
  I, ALineCount: Integer;
  ALines: TdxPDFTextLineList;
  ALastLine: TdxPDFTextWordPartList;
  ALastWord: TdxPDFTextWordPartAccess;
begin
  for I := Viewer.PageCount - 1 downto 0 do
  begin
    ALines := TextLines[I];
    if ALines = nil then
      ALineCount := 0
    else
      ALineCount := ALines.Count;
    if ALineCount > 0 then
    begin
      ALastLine := TdxPDFTextLineAccess(ALines[ALineCount - 1]).WordPartList;
      if ALastLine.Count > 0 then
      begin
        ALastWord := TdxPDFTextWordPartAccess(ALastLine[ALastLine.Count - 1]);
        MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(I,
          TdxPDFPageTextPosition.Create(ALastWord.WordIndex, Length(ALastWord.Characters))));
        Exit;
      end;
    end;
  end;
end;

procedure TdxPDFViewerTextSelector.MoveToDocumentStart;
var
  I: Integer;
begin
  for I := 0 to Viewer.PageCount - 1 do
    if TextLines[I].Count > 0 then
    begin
      MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(I, 1, 0));
      Break;
    end;
end;

procedure TdxPDFViewerTextSelector.MoveToLineStart;
var
  I: Integer;
  ALine: TdxPDFTextLineAccess;
  ALines: TdxPDFTextLineList;
begin
  ALines := TextLines[Caret.Position.PageIndex];
  for I := 0 to ALines.Count - 1 do
  begin
    ALine := TdxPDFTExtLineAccess(ALines[I]);
    if ALine.PositionInLine(Caret.Position.Position.WordIndex, Caret.Position.Position.Offset) then
      MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(
        Caret.Position.PageIndex,
        TdxPDFPageTextPosition.Create(ALine.WordPartList[0].WordIndex,
        TdxPDFTextWordPartAccess(ALine.WordPartList[0]).WrapOffset)));
  end;
end;

procedure TdxPDFViewerTextSelector.MoveToLineEnd;
var
  I, ALastWordIndex: Integer;
  ALines: TdxPDFTextLineList;
  ALine: TdxPDFTextLineAccess;
  AParts: TdxPDFTextWordPartList;
  ALast: TdxPDFTextWordPartAccess;
begin
  ALines := TextLines[Caret.Position.PageIndex];
  for I := 0 to ALines.Count - 1 do
  begin
    ALine := TdxPDFTextLineAccess(ALines[I]);
    if ALine.PositionInLine(Caret.Position.Position.WordIndex, Caret.Position.Position.Offset) then
    begin
      AParts := ALine.WordPartList;
      ALastWordIndex := AParts.Count - 1;
      ALast := TdxPDFTextWordPartAccess(AParts[ALastWordIndex]);
      MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(Caret.Position.PageIndex,
        TdxPDFPageTextPosition.Create(ALast.WordIndex, Length(ALast.Characters))));
    end;
  end;
end;

procedure TdxPDFViewerTextSelector.MoveToNextWord;
var
  ACaretWordIndex, ACaretPageIndex, ACaretOffset, ALineCount, I: Integer;
  ALines: TdxPDFTextLineList;
  ALastLine, ALine: TdxPDFTextLineAccess;
  AHasNextWord: Boolean;
  AWord: TdxPDFTextWordPart;
begin
  ACaretWordIndex := Caret.Position.Position.WordIndex;
  ACaretPageIndex := Caret.Position.PageIndex;
  ACaretOffset := Caret.Position.Position.Offset;

  ALines := Page[ACaretPageIndex].RecognizedContent.TextLines;

  ALastLine := TdxPDFTextLineAccess(ALines[ALines.Count - 1]);
  AHasNextWord := False;
  if (ALastLine.WordList.Count <> 0) and (ALastLine.WordList[ALastLine.WordList.Count - 1].Index = ACaretWordIndex) then
  begin
    while (ACaretPageIndex < Viewer.PageCount - 1) and not AHasNextWord do
    begin
      Inc(ACaretPageIndex);
      ALines := Page[ACaretPageIndex].RecognizedContent.TextLines;
      AHasNextWord := (ALines <> nil) and (ALines.Count > 0);
    end;
    if AHasNextWord then
      ACaretWordIndex := 0;
  end
  else
    AHasNextWord := True;
  if AHasNextWord then
    MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(ACaretPageIndex, ACaretWordIndex + 1, 0))
  else
  begin
    ALines := Page[Caret.Position.PageIndex].RecognizedContent.TextLines;
    ALineCount := ALines.Count;
    for I := 0 to ALineCount - 1 do
    begin
      ALine := TdxPDFTextLineAccess(ALines[I]);
      if ALine.PositionInLine(ACaretWordIndex, ACaretOffset) then
        for AWord in ALine.WordPartList do
          if AWord.WordIndex = ACaretWordIndex then
            MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(Caret.Position.PageIndex, ACaretWordIndex, Length(AWord.Characters)));
    end;
  end;
end;

procedure TdxPDFViewerTextSelector.MoveToPreviousWord;
var
  ACaretPageIndex, ACaretOffset, ACaretWordIndex, ALastWordIndex: Integer;
  ALines: TdxPDFTextLineList;
  ALastLine: TdxPDFTextWordPartList;
begin
  ACaretPageIndex := Caret.Position.PageIndex;
  ACaretOffset := Caret.Position.Position.Offset;
  ACaretWordIndex := Caret.Position.Position.WordIndex;
  if ACaretOffset <> 0 then
    MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(ACaretPageIndex, ACaretWordIndex, 0))
  else
    if ACaretWordIndex = 1 then
    begin
      while ACaretPageIndex > 0 do
      begin
        Dec(ACaretPageIndex);
        ALines := TextLines[ACaretPageIndex];
        if ALines.Count > 0 then
        begin
          ALastLine := TdxPDFTextLineAccess(ALines[ALines.Count - 1]).WordPartList;
          ALastWordIndex := ALastLine[ALastLine.Count - 1].WordIndex;
          MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(ACaretPageIndex, ALastWordIndex, 0));
          Exit;
        end;
      end;
    end
    else
      MoveAndMakeCaretVisible(TdxPDFTextPosition.Create(ACaretPageIndex, ACaretWordIndex - 1, 0));
end;

procedure TdxPDFViewerTextSelector.MoveUp;
var
  ANewPosition: TdxPDFTextPosition;
  APageIndex, AWordIndex, AOffset, I, APrevPageIndex: Integer;
  ALines: TdxPDFTextLineList;
  ALine: TdxPDFTextLineAccess;
begin
  APageIndex := Caret.Position.PageIndex;
  AWordIndex := Caret.Position.Position.WordIndex;
  AOffset := Caret.Position.Position.Offset;
  ALines := Page[APageIndex].RecognizedContent.TextLines;
  for I := ALines.Count - 1 downto 0 + 1 do
  begin
    ALine := TdxPDFTextLineAccess(ALines[I]);
    if ALine.PositionInLine(AWordIndex, AOffset) then
    begin
      ANewPosition := TdxPDFTextLineAccess(ALines[I - 1]).GetPosition(APageIndex, Caret.StartCoordinates);
      AWordIndex := ANewPosition.Position.WordIndex;
      AOffset := ANewPosition.Position.Offset;
      if ALine.PositionInLine(AWordIndex, AOffset) then
        SetCaretPosition(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex, AOffset - 1)))
      else
        SetCaretPosition(TdxPDFTextPosition.Create(APageIndex, TdxPDFPageTextPosition.Create(AWordIndex, AOffset)));
      Exit;
    end;
  end;
  for APrevPageIndex := APageIndex - 1 downto 0 do
  begin
    ALines := Page[APrevPageIndex].RecognizedContent.TextLines;
    if ALines.Count > 0 then
    begin
      SetCaretPosition(TdxPDFTextLineAccess(ALines[ALines.Count - 1]).GetPosition(APrevPageIndex, Caret.StartCoordinates));
      Exit;
    end;
  end;
  SetCaretPosition(Caret.Position);
end;

procedure TdxPDFViewerTextSelector.Select(APage: TdxPDFPage; AProc: TProc);
begin
  if APage <> nil then
    TdxPDFPageAccess(APage).LockAndExecute(AProc);
end;

procedure TdxPDFViewerTextSelector.SetCaretPosition(const APosition: TdxPDFTextPosition);
begin
  if not HasCaret then
    Caret := TdxPDFDocumentCaret.Create(APosition, GetCaretViewData(APosition), Caret.StartCoordinates)
  else
    Caret := TdxPDFDocumentCaret.Create(APosition, GetCaretViewData(APosition), dxNullPointF);
  MakeCaretVisible;
end;

procedure TdxPDFViewerTextSelector.StoreSelectionStartTextPosition;
begin
  if IsEmptySelection and HasCaret then
    FStartTextPosition := Caret.Position;
end;

procedure TdxPDFViewerTextSelector.UpdateSelection(const APosition: TdxPDFTextPosition);
begin
  MoveCaret(APosition);
  if not IsEmptySelection then
  begin
    Selection := nil;
    Viewer.SelectionController.SelectionChanged;
  end;
end;

{ TdxPDFViewerSelectionController }

constructor TdxPDFViewerSelectionController.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create(AViewer);
  FImageSelector := TdxPDFViewerImageSelector.Create(Viewer);
  FTextSelector := TdxPDFViewerTextSelector.Create(Viewer);
  FClickController := TdxPDFViewerClickController.Create;
  FSelection := nil;
  FCaret.Invalid;
end;

destructor TdxPDFViewerSelectionController.Destroy;
begin
  FreeAndNil(FSelection);
  FreeAndNil(FClickController);
  FreeAndNil(FTextSelector);
  FreeAndNil(FImageSelector);
  inherited Destroy;
end;

function TdxPDFViewerSelectionController.CanExtractSelectedContent(AHitCode: Int64): Boolean;
begin
  Result := Viewer.CanExtractContent and (Selection <> nil) and  (Selection.HitCode = AHitCode);
end;

function TdxPDFViewerSelectionController.IsEmptySelection: Boolean;
begin
  Result := FSelection = nil;
end;

procedure TdxPDFViewerSelectionController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Viewer.IsDocumentLoaded then
  begin
    FClickController.Click(Button, X, Y);
    if (ssLeft in Shift) and HitTest.HitAtPage and not HitTest.HitAtInteractiveObject then
    begin
      if not Viewer.HandTool then
        StartSelection(Shift)
    end
    else
      if not HitTest.HitAtSelection then
        LockedClear;
  end;
end;

procedure TdxPDFViewerSelectionController.MouseMove(Shift: TShiftState; X, Y: Integer);

  function NeedSelect(AShift: TShiftState): Boolean;
  begin
    Result := (ssLeft in AShift) and ((FClickController.ClickCount <= 1) or HitTest.HitAtImage or
      FImageSelector.InProgress);
  end;

begin
  if Viewer.IsDocumentLoaded and NeedSelect(Shift) then
    DoSelect;
end;

procedure TdxPDFViewerSelectionController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Viewer.IsDocumentLoaded then
    EndSelection(Shift);
end;

procedure TdxPDFViewerSelectionController.DoSelect;
begin
  if not FImageSelector.Select then
    FTextSelector.Select(HitTest.Position);
end;

procedure TdxPDFViewerSelectionController.DoSelect(AShift: TShiftState);
begin
  if ssShift in AShift then
  begin
    FTextSelector.InProgress := True;
    DoSelect;
  end
  else
    StartSelection;
end;

function TdxPDFViewerSelectionController.GetImageSelection(const AArea: TdxPDFDocumentArea): TdxPDFImageSelection;
var
  AImage: TdxPDFImage;
begin
  Result := nil;
  for AImage in Images[AArea.PageIndex] do
    if TdxPDFUtils.Intersects(AArea.Rect, TdxPDFImageAccess(AImage).Bounds) then
      Result := TdxPDFImageSelection.Create(Viewer.Painter.SelectionBackColor, Viewer.Painter.SelectionFrameColor,
        AArea.PageIndex, AImage, TdxPDFImageAccess(AImage).Bounds);
end;

function TdxPDFViewerSelectionController.GetTextRanges(const AArea: TdxPDFDocumentArea): TdxPDFPageTextRanges;
var
  ATextSelection: TdxPDFTextHighlightAccess;
begin
  ATextSelection := TdxPDFTextHighlightAccess(GetTextSelection(AArea));
  if ATextSelection <> nil then
  try
    SetLength(Result, Length(ATextSelection.Ranges));
    cxCopyData(@ATextSelection.Ranges[0], @Result[0], 0, 0, Length(ATextSelection.Ranges) * SizeOf(TdxPDFPageTextRange));
  finally
    ATextSelection.Free;
  end;
end;

function TdxPDFViewerSelectionController.GetTextSelection(const AArea: TdxPDFDocumentArea): TdxPDFTextSelection;
var
  ALine: TdxPDFTextLine;
  APageIndex: Integer;
  ARange: TdxPDFPageTextRange;
  ARanges: TdxPDFPageTextRanges;
begin
  SetLength(ARanges, 0);
  APageIndex := AArea.PageIndex;
  for ALine in TextLines[APageIndex] do
    if TdxPDFUtils.Intersects(AArea.Rect, TdxPDFTextLineAccess(ALine).Bounds.RotatedRect) then
    begin
      ARange := TdxPDFTextLineAccess(ALine).GetRange(APageIndex, AArea.Rect);
      if not TdxPDFPageTextRange.Same(ARange, TdxPDFPageTextRange.Invalid)  then
        TdxPDFTextUtils.AddRange(ARange, ARanges);
    end;
  Result := FTextSelector.CreateTextSelection(ARanges);
end;

function TdxPDFViewerSelectionController.HitAtSelection: Boolean;
begin
  if FTextSelector.InProgress then
    Result := False
  else
    if FImageSelector.InProgress then
      Result := False
    else
      Result := not IsEmptySelection and TdxPDFCustomSelectionAccess(Selection).Contains(HitTest.Position)
end;

procedure TdxPDFViewerSelectionController.Clear;
begin
  LockedClear;
  SelectionChanged;
end;

procedure TdxPDFViewerSelectionController.LockedClear;
begin
  Selection := nil;
  HideCaret;
  FImageSelector.Clear;
  FTextSelector.Clear;
end;

procedure TdxPDFViewerSelectionController.CopyToClipboard;
begin
  if Viewer.CanExtractContent then
  begin
    if not IsEmptySelection then
    begin
      if Selection is TdxPDFImageSelection then
        CopyImageToClipboard;
      if Selection is TdxPDFTextSelection then
        CopyTextToClipboard;
    end;
  end;
end;

procedure TdxPDFViewerSelectionController.HideCaret;
begin
  Caret := TdxPDFDocumentCaret.Invalid;
  Viewer.DeleteCaret;
end;

procedure TdxPDFViewerSelectionController.MakeVisible;
var
  APageIndex: Integer;
  ARect:TdxRectF;
begin
  if Selection <> nil then
  begin
    APageIndex := -1;
    ARect := dxNullRectF;
    if Selection is TdxPDFImageSelection then
    begin
      APageIndex := TdxPDFImageSelectionAccess(Selection).PageIndex;
      ARect := TdxPDFImageSelectionAccess(Selection).Bounds;
    end
    else
      if Selection is TdxPDFTextSelection then
      begin
        APageIndex := TdxPDFTextHighlightAccess(Selection).Ranges[0].PageIndex;
        ARect := TdxPDFTextHighlightAccess(Selection).PageRects[APageIndex][0];
      end;
    if Assigned(FOnMakeRectVisible) and (APageIndex >= 0) then
      OnMakeRectVisible(APageIndex, ARect);
  end;
end;

procedure TdxPDFViewerSelectionController.Select(const ARect: TRect);
var
  I: Integer;
  AArea: TdxPDFDocumentArea;
  AAreaList: TList<TdxPDFDocumentArea>;
  AIntersection, R, ADocumentRect: TdxRectF;
  ARanges: TdxPDFPageTextRanges;
  AViewerPage: TdxPDFViewerPage;
begin
  if Viewer.IsDocumentLoaded then
  begin
    ADocumentRect := dxRectF(ARect);
    AAreaList := TList<TdxPDFDocumentArea>.Create;
    try
      for I := 0 to Viewer.PageCount - 1 do
      begin
        AViewerPage := Viewer.PageList[I] as TdxPDFViewerPage;
        if TdxPDFUtils.Intersects(AIntersection, dxRectF(AViewerPage.Bounds), ADocumentRect) then
        begin
          R := AViewerPage.ToDocumentRect(AIntersection);
          AArea := TdxPDFDocumentArea.Create(I, cxRectAdjustF(R));
          AAreaList.Add(AArea);
          ARanges := GetTextRanges(AArea);
        end;
      end;
      LockedClear;
      SelectText(ARanges);
      if Selection = nil then
        for AArea in AAreaList do
        begin
          SelectImage(AArea);
          if Selection <> nil then
            Break;
        end;
    finally
      AAreaList.Free;
    end;
  end;
end;

procedure TdxPDFViewerSelectionController.SelectText(ARange: TdxPDFPageTextRange);
begin
  if Viewer.IsDocumentLoaded then
  begin
    Selection := FTextSelector.CreateTextSelection(ARange);
    SelectionChanged;
  end;
end;

procedure TdxPDFViewerSelectionController.SelectAll;
var
  I: Integer;
  ARanges: TdxPDFPageTextRanges;
begin
  if Viewer.IsDocumentLoaded then
  begin
    SetLength(ARanges, 0);
    for I := 0 to Viewer.PageCount - 1 do
      TdxPDFTextUtils.AddRange(TdxPDFPageTextRange.Create(I), ARanges);
    SelectText(ARanges);
  end;
end;

procedure TdxPDFViewerSelectionController.SelectionChanged;
begin
  if not Viewer.HandTool and not Viewer.HitTest.HitAtBackground and not Viewer.IsDestroying then
    dxCallNotify(OnSelectionChanged, Self);
end;

procedure TdxPDFViewerSelectionController.SelectByKeyboard(ADirection: TdxPDFMovementDirection);
begin
  FTextSelector.SelectByKeyboard(ADirection);
  SelectionChanged;
end;

procedure TdxPDFViewerSelectionController.SelectImage(const AArea: TdxPDFDocumentArea);
begin
  Selection := GetImageSelection(AArea);
  SelectionChanged;
end;

procedure TdxPDFViewerSelectionController.SelectText(const ARanges: TdxPDFPageTextRanges);
begin
  Selection := FTextSelector.CreateTextSelection(ARanges);
  SelectionChanged;
end;

procedure TdxPDFViewerSelectionController.EndSelection;
begin
  FInOutsideContent := False;
  FTextSelector.Reset;
  DoSelect;
  SelectionChanged;
  FImageSelector.Reset;
  FStartSelectionPosition.Invalid;
end;

procedure TdxPDFViewerSelectionController.EndSelection(AShift: TShiftState);
begin
  if FStartSelectionPosition.NearTo(HitTest.Position) then
  begin
    if HitTest.HitAtImage then
      LockedClear
    else
      DoSelect(AShift);
  end;
  EndSelection;
end;

function TdxPDFViewerSelectionController.CreateHighlight(ARange: TdxPDFPageTextRange;
  ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight;
var
  ARanges: TdxPDFPageTextRanges;
begin
  SetLength(ARanges, 0);
  TdxPDFTextUtils.AddRange(ARange, ARanges);
  Result := CreateHighlight(ARanges, ABackColor, AFrameColor);
end;

function TdxPDFViewerSelectionController.CreateHighlight(const ARanges: TdxPDFPageTextRanges;
  ABackColor, AFrameColor: TdxAlphaColor): TdxPDFTextHighlight;
begin
  Result := FTextSelector.CreateTextHighlights(ARanges, ABackColor, AFrameColor);
end;

function TdxPDFViewerSelectionController.KeyDown(var AKey: Word; AShift: TShiftState): Boolean;
var
  AShiftPressed: Boolean;
begin
  Result := True;
  AShiftPressed := ssShift in AShift;
  case AKey of
     VK_LEFT:
       if IsCtrlPressed then
       begin
         if AShiftPressed then
           SelectByKeyboard(mdPreviousWord)
         else
           FTextSelector.MoveCaret(mdPreviousWord)
       end
       else
         if AShiftPressed then
           SelectByKeyboard(mdLeft)
         else
           FTextSelector.MoveCaret(mdLeft);
     VK_RIGHT:
       if IsCtrlPressed then
       begin
         if AShiftPressed then
           SelectByKeyboard(mdNextWord)
         else
           FTextSelector.MoveCaret(mdNextWord);
       end
       else
         if AShiftPressed then
           SelectByKeyboard(mdRight)
         else
           FTextSelector.MoveCaret(mdRight);
     VK_UP:
       if AShiftPressed then
         SelectByKeyboard(mdUp)
       else
         FTextSelector.MoveCaret(mdUp);
     VK_DOWN:
       if AShiftPressed then
         SelectByKeyboard(mdDown)
       else
         FTextSelector.MoveCaret(mdDown);
     VK_HOME:
       if IsCtrlPressed then
       begin
         if AShiftPressed then
           SelectByKeyboard(mdDocumentStart)
         else
           FTextSelector.MoveCaret(mdDocumentStart);
       end
       else
         if AShiftPressed then
           SelectByKeyboard(mdLineStart)
         else
           FTextSelector.MoveCaret(mdLineStart);
     VK_END:
       if IsCtrlPressed then
       begin
         if AShiftPressed then
           SelectByKeyboard(mdDocumentEnd)
         else
           FTextSelector.MoveCaret(mdDocumentEnd);
       end
       else
         if AShiftPressed then
           SelectByKeyboard(mdLineEnd)
         else
           FTextSelector.MoveCaret(mdLineEnd);
  else
    Result := False;
  end;
end;

procedure TdxPDFViewerSelectionController.StartSelection;
begin
  FStartSelectionPosition := HitTest.Position;
  LockedClear;
  if FStartSelectionPosition.IsValid then
    if not FTextSelector.StartSelection(FStartSelectionPosition) then
      FImageSelector.StartSelection(FInOutsideContent);
end;

procedure TdxPDFViewerSelectionController.StartSelection(AShift: TShiftState);
begin
  if HitTest.HitAtImage then
  begin
    if HitTest.HitAtSelection then
      FStartSelectionPosition := HitTest.Position
    else
      StartSelection;
  end
  else
    case FClickController.ClickCount of
      2:
        FTextSelector.SelectWord(HitTest.Position);
      3:
        FTextSelector.SelectLine(HitTest.Position);
      4:
        FTextSelector.SelectPage(HitTest.Position);
    else
      if HitTest.HitAtSelection then
        FStartSelectionPosition := HitTest.Position
      else
        DoSelect(AShift);
    end;
end;

function TdxPDFViewerSelectionController.GetSelectionAsBitmap: TcxBitmap;

  function GetActualRotationAngle(APageIndex: Integer): TcxRotationAngle;
  var
    AAngle: Integer;
    APage: TdxPDFPageAccess;
  begin
    APage := TdxPDFPageAccess(TdxPDFViewerPage(Viewer.PageList[APageIndex]).DocumentPage);
    AAngle := APage.CalculateRotationAngle(Viewer.RotationAngle);
    case AAngle of
      90:
        Result := raMinus90;
      270:
        Result := raPlus90;
      180:
        Result := ra180;
    else
      Result := ra0;
    end;
  end;

var
  ASelection: TdxPDFImageSelectionAccess;
begin
  Result := nil;
  if CanExtractSelectedContent(hcImage) then
  begin
    ASelection := TdxPDFImageSelectionAccess(Selection as TdxPDFImageSelection);
    Result := ASelection.GetImageBitmap(Viewer);
    Result.Rotate(GetActualRotationAngle(ASelection.PageIndex));
  end;
end;

function TdxPDFViewerSelectionController.GetSelectionAsText: string;
begin
  if CanExtractSelectedContent(hcTextSelection) then
    Result := TdxPDFTextSelection(Selection).Text
  else
    Result := '';
end;

function TdxPDFViewerSelectionController.GetPage(AIndex: Integer): TdxPDFPage;
begin
  Result := Viewer.DocumentPages[AIndex];
end;

procedure TdxPDFViewerSelectionController.SetCaret(const AValue: TdxPDFDocumentCaret);
begin
  if not FCaret.Same(AValue) then
    FCaret := AValue;
end;

procedure TdxPDFViewerSelectionController.SetSelection(const AValue: TdxPDFCustomSelection);
begin
  if IsEmptySelection or not TdxPDFCustomSelectionAccess(Selection).Same(AValue) then
  begin
    FreeAndNil(FSelection);
    FSelection := AValue;
    Viewer.Invalidate;
  end
  else
    AValue.Free;
end;

function TdxPDFViewerSelectionController.GetImages(APageIndex: Integer): TdxPDFImageList;
begin
  Result := Page[APageIndex].RecognizedContent.Images;
end;

function TdxPDFViewerSelectionController.GetScaleFactor: TdxPointF;
begin
  Result := Viewer.DocumentScaleFactor;
end;

function TdxPDFViewerSelectionController.GetTextLines(APageIndex: Integer): TdxPDFTextLineList;
begin
  Result := Page[APageIndex].RecognizedContent.TextLines;
end;

function TdxPDFViewerSelectionController.GetHitTest: TdxPDFViewerDocumentHitTest;
begin
  Result := Viewer.HitTest;
end;

procedure TdxPDFViewerSelectionController.CopyImageToClipboard;
var
  ABitmap: TcxBitmap;
begin
  ABitmap := GetSelectionAsBitmap;
  if ABitmap <> nil then
    try
      Clipboard.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
end;

procedure TdxPDFViewerSelectionController.CopyTextToClipboard;
begin
  Clipboard.Open;
  try
    Clipboard.Clear;
    Clipboard.AsText := GetSelectionAsText;
  finally
    Clipboard.Close;
  end;
end;

{ TdxPDFViewerContainerController }

procedure TdxPDFViewerContainerController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  CalculateHitTests(cxPoint(X, Y));
  if ssLeft in Shift then
  begin
    PressedCell := HitTest.HitObject;
    FocusedCell := HitTest.HitObject;
  end;
end;

procedure TdxPDFViewerContainerController.DoMouseEnter(AControl: TControl);
begin
  inherited DoMouseEnter(AControl);
  CalculateHitTests(Viewer.GetMouseCursorClientPos);
  HotCell := nil;
  FocusedCell := nil;
end;

procedure TdxPDFViewerContainerController.DoMouseLeave(AControl: TControl);
begin
  inherited DoMouseLeave(AControl);
  HitTest.Clear;
  HotCell := nil;
  FocusedCell := nil;
end;

procedure TdxPDFViewerContainerController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
  CalculateHitTests(X, Y);
  HotCell := HitTest.HitObject;
  UpdateStates;
end;

procedure TdxPDFViewerContainerController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseUp(Button, Shift, X, Y);
  CalculateHitTests(X, Y);
  if (HitTest.HitObject <> nil) and HitTest.HitObject.IsPressed then
    ProcessClick(PressedCell);
  PressedCell := nil;
end;

function TdxPDFViewerContainerController.GetHitTest: TdxPDFViewerCellHitTest;
begin
  Result := nil;
end;

function TdxPDFViewerContainerController.ProcessChildKey(var AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := (FocusedCell <> ViewInfo.FindPanel) and (FocusedCell <> ViewInfo);
  if Result then
    if AKey = VK_TAB then
      FocusNextCell(True)
    else
      if FocusedCell is TdxPDFViewerButtonViewInfo then
        if AKey = VK_RETURN then
          TdxPDFViewerButtonViewInfo(FocusedCell).Execute;
    AKey := 0;
end;

procedure TdxPDFViewerContainerController.Clear;
begin
// do nothing
end;

procedure TdxPDFViewerContainerController.DoCalculateHitTests(const P: TPoint);
begin
  HitTest.Calculate(P);
  UpdateStates;
end;

procedure TdxPDFViewerContainerController.UpdateCursor;
begin
// do nothing
end;

procedure TdxPDFViewerContainerController.UpdateState;
begin
  if not Viewer.IsDestroying then
    ViewInfo.UpdateState;
end;

function TdxPDFViewerContainerController.FindNextFocusableCell(ACell: TdxPDFViewerCellViewInfo;
  AGoForward: Boolean): TdxPDFViewerCellViewInfo;
const
  StepMap: array[Boolean] of Integer = (-1, 1);
var
  AIndex: Integer;
  ATabOrderList: TdxPDFViewerViewInfoList;
begin
  Result := nil;
  ATabOrderList := TdxPDFViewerViewInfoList.Create(False);
  try
    ViewInfo.PopulateTabOrders(ATabOrderList);
    AIndex := (ATabOrderList.IndexOf(ACell) + StepMap[AGoForward]) mod ATabOrderList.Count;
    while (AIndex >= 0) and (AIndex < ATabOrderList.Count) do
    begin
      if ATabOrderList[AIndex].CanFocus then
      begin
        Result := ATabOrderList[AIndex];
        Break;
      end;
      Inc(AIndex, StepMap[AGoForward]);
    end;
    if Result = nil then
      Result := ATabOrderList[0];
  finally
    ATabOrderList.Free;
  end;
end;

procedure TdxPDFViewerContainerController.CalculateHitTests(const P: TPoint);
begin
  DoCalculateHitTests(P);
end;

procedure TdxPDFViewerContainerController.CalculateHitTests(X, Y: Integer);
begin
  if Viewer.IsDocumentAvailable then
    CalculateHitTests(cxPoint(X, Y));
end;

procedure TdxPDFViewerContainerController.CellRemoving(ACell: TdxPDFViewerCellViewInfo);
begin
  if ACell.IsHot then
    FHotCell := nil;
  if ACell.IsPressed then
    FPressedCell := nil;
  if FPrevFocusedCell = ACell then
    FPrevFocusedCell := nil;
  if ACell.IsFocused then
    FFocusedCell := nil;
end;

procedure TdxPDFViewerContainerController.FocusNextCell(AGoForward: Boolean);
begin
  FocusedCell := FindNextFocusableCell(FocusedCell, AGoForward);
end;

procedure TdxPDFViewerContainerController.ProcessAccel(ACell: TdxPDFViewerCellViewInfo);
begin
  FocusedCell := ACell;
  ProcessClick(ACell);
end;

procedure TdxPDFViewerContainerController.ProcessClick(ACell: TdxPDFViewerCellViewInfo);
begin
  if ACell is TdxPDFViewerButtonViewInfo then
    TdxPDFViewerButtonViewInfo(ACell).Execute;
end;

procedure TdxPDFViewerContainerController.SetHotCell(const AValue: TdxPDFViewerCellViewInfo);
begin
  if FHotCell <> AValue then
  begin
    FHotCell := AValue;
    UpdateState;
  end;
end;

procedure TdxPDFViewerContainerController.SetPressedCell(const AValue: TdxPDFViewerCellViewInfo);
begin
  if FPressedCell <> AValue then
  begin
    FPressedCell := AValue;
    UpdateState;
  end;
end;

procedure TdxPDFViewerContainerController.SetFocusedCell(const AValue: TdxPDFViewerCellViewInfo);
begin
  if (FFocusedCell <> AValue) and ((AValue = nil) or AValue.CanFocus) then
  begin
    FFocusedCell := AValue;
    if Viewer.HandleAllocated and Viewer.CanFocusEx and Viewer.IsDocumentLoaded then
    begin
      if FFocusedCell <> ViewInfo.FindPanel.Edit then
        Viewer.SetFocus
      else
        Viewer.ViewInfo.FindPanel.Edit.SetFocus;
    end;
    UpdateState;
  end;
end;

{ TdxPDFViewerNavigationPaneController }

function TdxPDFViewerNavigationPaneController.GetHitTest: TdxPDFViewerCellHitTest;
begin
  Result := NavigationPane.HitTest;
end;

function TdxPDFViewerNavigationPaneController.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
end;

procedure TdxPDFViewerNavigationPaneController.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FHintHelper := THintHelper.Create(Viewer);
  FShowHintTimer := TcxTimer.Create(nil);
  FShowHintTimer.OnTimer := ShowHintTimerExpired;
end;

procedure TdxPDFViewerNavigationPaneController.Clear;
var
  APage: TdxPDFViewerNavigationPanePage;
begin
  inherited Clear;
  for APage in NavigationPane.Pages do
    APage.Clear;
  NavigationPane.ActivePage := nil;
end;

procedure TdxPDFViewerNavigationPaneController.DestroySubClasses;
begin
  FHintHelper.HideHint;
  FreeAndNil(FShowHintTimer);
  FreeAndNil(FHintHelper);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerNavigationPaneController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalculateHitTests(cxPoint(X, Y));
  FHintHelper.MouseDown;
  if PtInRect(ViewInfo.NavigationPane.ButtonsBounds, HitTest.HitPoint) then
  begin
    if (HitTest.HitObject <> nil) and HitTest.HitObject.IsHot then
      ProcessClick(HotCell);
    HotCell := nil;
  end
  else
    inherited DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxPDFViewerNavigationPaneController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
  CheckHint;
end;

procedure TdxPDFViewerNavigationPaneController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalculateHitTests(cxPoint(X, Y));
  if not PtInRect(ViewInfo.NavigationPane.ButtonsBounds, HitTest.HitPoint) then
    inherited DoMouseUp(Button, Shift, X, Y);
end;

procedure TdxPDFViewerNavigationPaneController.UpdateCursor;
begin
  SetCursor(Screen.Cursors[HitTest.Cursor]);
end;

procedure TdxPDFViewerNavigationPaneController.UpdateStates;
begin
  ViewInfo.NavigationPane.UpdateState;
end;

function TdxPDFViewerNavigationPaneController.InPaneRect(const P: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.NavigationPane.Bounds, P);
end;

function TdxPDFViewerNavigationPaneController.InPaneSplitterRect(const P: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.NavigationPane.SplitterBounds, P);
end;

procedure TdxPDFViewerNavigationPaneController.ExecuteOperation(const AOperation: TdxPDFInteractiveOperation);
begin
  Viewer.ViewerController.ExecuteOperation(AOperation);
  if Viewer.OptionsNavigationPane.Bookmarks.HideAfterUse then
    NavigationPane.MinimizePage;
end;

procedure TdxPDFViewerNavigationPaneController.Refresh;
var
  APage: TdxPDFViewerNavigationPanePage;
begin
  for APage in NavigationPane.Pages do
    APage.Refresh;
end;

function TdxPDFViewerNavigationPaneController.GetHintText: string;
begin
  if (FHintCell <> nil) and (FHintCell is TdxPDFViewerButtonViewInfo) then
    Result := TdxPDFViewerButtonViewInfo(FHintCell).GetHint
  else
    Result := '';
end;

function TdxPDFViewerNavigationPaneController.GetNavigationPane: TdxPDFViewerNavigationPane;
begin
  Result := Viewer.NavigationPane;
end;

function TdxPDFViewerNavigationPaneController.NeedShowHint(const AHint: string): Boolean;
begin
  Result := Viewer.OptionsBehavior.ShowHints and (FHintCell <> nil) and (AHint <> '') and
    Supports(FHintCell, IcxHintableObject) and cxCanShowHint(Viewer);
end;

procedure TdxPDFViewerNavigationPaneController.CheckHint;
var
  APreviousHotCell: TdxPDFViewerCellViewInfo;
  AVisible: Boolean;
begin
  APreviousHotCell := FHintCell;
  FHintCell := HotCell;
  if FHintCell <> APreviousHotCell then
  begin
    FShowHintTimer.Enabled := False;
    AVisible := FHintHelper.IsHintWindowVisible;
    FHintHelper.ResetLastHintElement;
    if NeedShowHint(GetHintText) then
    begin
      if AVisible then
        FShowHintTimer.Interval := Application.HintShortPause
      else
        FShowHintTimer.Interval := Application.HintPause;
      FShowHintTimer.Enabled := True;
    end;
  end;
end;

procedure TdxPDFViewerNavigationPaneController.ShowHintTimerExpired(Sender: TObject);
var
  AHint: string;
  AHintAreaBounds: TRect;
begin
  FShowHintTimer.Enabled := False;
  if HotCell <> nil then
  begin
    AHint := GetShortHint(GetHintText);
    AHintAreaBounds := HotCell.Bounds;
  end;
  if NeedShowHint(AHint) then
    FHintHelper.ShowHint(AHintAreaBounds, AHintAreaBounds, AHint, False, HotCell, Viewer.Font);
end;

{ TdxPDFViewerNavigationPaneController.THintHelper }

constructor TdxPDFViewerNavigationPaneController.THintHelper.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
end;

procedure TdxPDFViewerNavigationPaneController.THintHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited;
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

function TdxPDFViewerNavigationPaneController.THintHelper.GetOwnerControl: TcxControl;
begin
  Result := FViewer;
end;

function TdxPDFViewerNavigationPaneController.THintHelper.IsHintWindowVisible: Boolean;
begin
  Result := (HintWindow <> nil) and HintWindow.HandleAllocated and
    IsWindowVisible(HintWindow.Handle);
end;

{ TdxPDFViewerController }

procedure TdxPDFViewerController.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FSelectionController := TdxPDFViewerSelectionController.Create(Viewer);
  FSelectionController.OnMakeRectVisible := OnMakeRectVisibleHandler;
  FViewStateHistoryController := TdxPDFViewerViewStateHistoryController.Create(Viewer);
  FInteractivityController := TdxPDFViewerInteractivityController.Create(Viewer);
  FInteractiveObjectHintHelper := TInteractiveObjectHinHelper.Create(Viewer);
  FClickTimer := cxCreateTimer(ClickTimerHandler, GetDblClickInterval, False);
end;

procedure TdxPDFViewerController.DestroySubClasses;
begin
  FreeAndNil(FClickTimer);
  FreeAndNil(FInteractivityController);
  FreeAndNil(FInteractiveObjectHintHelper);
  FreeAndNil(FViewStateHistoryController);
  FreeAndNil(FSelectionController);
  inherited DestroySubClasses;
end;

function TdxPDFViewerController.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := InFindPanelRect(Viewer.ScreenToClient(MousePos));
end;

function TdxPDFViewerController.ProcessChildKey(var AKey: Word; AShiftState: TShiftState): Boolean;
begin
  if AKey = VK_ESCAPE then
    Viewer.HideFindPanel;
  Result := inherited ProcessChildKey(AKey, AShiftState);
end;

procedure TdxPDFViewerController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPrevHandPoint := Point(X, Y);
  inherited DoMouseDown(Button, Shift, X, Y);
  FInteractiveObjectHintHelper.MouseDown;
  if not DocumentHitTest.HitAtFindPanel then
  begin
    UpdateFocusedField(Button);
    FSelectionController.MouseDown(Button, Shift, X, Y);
    FClickTimer.Enabled := True;
    FCanClick := True;
  end;
end;

procedure TdxPDFViewerController.DoMouseLeave(AControl: TControl);
begin
  inherited DoMouseLeave(AControl);
  DocumentHitTest.Clear;
end;

procedure TdxPDFViewerController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited DoMouseMove(Shift, X, Y);
  P := cxPoint(X, Y);
  if IsDocumentPanning then
    OffsetContent(cxPointOffset(P, FPrevHandPoint, False))
  else
  begin
    if DocumentHitTest.HitAtHintableObject then
      FInteractiveObjectHintHelper.EnableShowing;
    SelectionController.MouseMove(Shift, X, Y);
  end;
  FPrevHandPoint := P;
end;

procedure TdxPDFViewerController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIsDocumentArea: Boolean;
  AIntf: IdxPDFInteractiveObject;
begin
  AIsDocumentArea := not DocumentHitTest.HitAtFindPanel;
  inherited DoMouseUp(Button, Shift, X, Y);
  if AIsDocumentArea then
  begin
    if IsDocumentPanning then
      ViewStateHistoryController.StoreCurrentViewState(vsctScrolling)
    else
      if FCanClick and Supports(DocumentHitTest.DocumentHitObject, IdxPDFInteractiveObject, AIntf) then
      begin
        AIntf.ExecuteOperation(FInteractivityController);
        if AIntf.IsResetFocusingNeeded then
          FocusedField := nil;
      end
      else
        SelectionController.MouseUp(Button, Shift, X, Y);
    FCanClick := True;
    FClickTimer.Enabled := False;
  end;
end;

procedure TdxPDFViewerController.UpdateCursor;

  function HitAtFindPanelEdit: Boolean;
  begin
    Result := PtInRect(ViewInfo.FindPanel.Edit.Bounds, HitTest.HitPoint);
  end;

  procedure DoUpdate(ACursor: TCursor);
  begin
    SetCursor(Screen.Cursors[ACursor]);
  end;

begin
  if not Viewer.IsUpdateLocked then
    if HitAtFindPanelEdit then
      Exit
    else
      if DocumentHitTest.HitAtFindPanel and not (IsDocumentPanning or IsDocumentSelecting) then
        DoUpdate(crDefault)
      else
        if not HitAtFindPanelEdit  then
          DoUpdate(Cursor);
end;

procedure TdxPDFViewerController.UpdateStates;
begin
  ViewInfo.UpdateState;
end;

function TdxPDFViewerController.GetPageScrollPosition(APage: TdxPDFViewerPage; AScrollPositionX,
  AScrollPositionY: Single): TdxPointF;
var
  ACropBox: TdxRectF;
  APosition: TdxPointF;
  AHasX, AHasY: Boolean;
begin
  Result := dxPointF(cxNullPoint);
  AHasX := TdxPDFUtils.IsDoubleValid(AScrollPositionX);
  AHasY := TdxPDFUtils.IsDoubleValid(AScrollPositionY);
  if not AHasX or not AHasY then
  begin
    APosition := dxPointF(cxPointOffset(cxPoint(Viewer.LeftPos, Viewer.TopPos),
      Viewer.Pages[Viewer.CurrentPageIndex].Bounds.TopLeft, False));
    Result := APage.ToDocumentPoint(APosition);
  end;
  if APage <> nil then
  begin
    ACropBox := TdxPDFPageAccess(APage.DocumentPage).CropBox;
    Result.X := IfThen(AHasX, AScrollPositionX - ACropBox.Left, Result.X);
    Result.Y := IfThen(AHasY, AScrollPositionY - ACropBox.Bottom, Result.Y);
  end;
end;

function TdxPDFViewerController.GetPageTopLeft(APage: TdxPDFViewerPage): TdxPointF;
begin
  Result := APage.DocumentPage.GetTopLeft(Viewer.RotationAngle);
end;

function TdxPDFViewerController.CanSearchText: Boolean;
begin
  Result := Viewer.IsDocumentLoaded and (Viewer.ViewInfo.FindPanel.Edit.InternalEdit.EditingValue <> '') and
    not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerController.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  if (Key in [VK_TAB, VK_RETURN, VK_ESCAPE]) then
    ProcessChildKey(Key, Shift)
  else
    if Viewer.Focused then
    begin
      if Assigned(Viewer.OnKeyDown) then
        Viewer.OnKeyDown(Viewer, Key, Shift);
      if not SelectionController.Caret.IsValid then
        Result := ProcessViewerKeyDown(Key, Shift)
      else
        Result := SelectionController.KeyDown(Key, Shift);
      if not Result then
      begin
        Result := True;
        case Key of
          VK_ADD:
            if IsCtrlShiftPressed(Shift) then
              Viewer.RotateClockwise
            else
              if IsCtrlPressed then
                Viewer.ZoomIn;
          VK_SUBTRACT:
            if IsCtrlShiftPressed(Shift) then
              Viewer.RotateCounterclockwise
            else
              if IsCtrlPressed then
                Viewer.ZoomOut;
          VK_RETURN:
             Viewer.GoToNextPage;
          Ord('C'):
             if IsCtrlPressed then
               Viewer.Selection.CopyToClipboard;
          Ord('A'):
            if IsCtrlPressed then
              Viewer.Selection.SelectAll;
          Ord('P'):
            if IsCtrlPressed then
              ShowPrintDialog(Viewer);
          Ord('F'):
            if IsCtrlPressed then
              Viewer.ShowFindPanel;
          VK_F3:
            Viewer.FindNext;
        else
          Result := False;
        end
      end;
    end;
end;

procedure TdxPDFViewerController.MakeRectVisible(const ARect: TdxRectF; AType: TdxVisibilityType = vtCentered;
  AIsTopAlignment: Boolean = False);

  procedure DoMakeRectVisible(const R: TRect);

    function GetOffset(AItemMin, AItemMax, AClientMin, AClientMax: Integer): Integer;
    begin
      Result := 0;
      if AItemMin < AClientMax then
        Result := AItemMin - AClientMin
      else
        if AItemMax > AClientMax then
        begin
          if AIsTopAlignment then
            Result := Max(AItemMax - AClientMax, AItemMin - AClientMin)
          else
            Result := Min(AItemMax - AClientMax, AItemMin - AClientMin)
        end;
    end;

  var
    AClientRect: TRect;
    P: TPoint;
  begin
    AClientRect := Viewer.ClientBounds;
    P.X := Viewer.LeftPos + GetOffset(R.Left, R.Right, AClientRect.Left, AClientRect.Right);
    P.Y := Viewer.TopPos + GetOffset(R.Top, R.Bottom, AClientRect.Top, AClientRect.Bottom);
    Viewer.SetLeftTop(P);
  end;

var
  R: TRect;
begin
  FViewStateHistoryController.BeginUpdate;
  try
    R := cxRect(ARect, True);
    if not AIsTopAlignment then
      TcxScrollingControlAccess(Viewer).MakeVisible(R, AType)
    else
      DoMakeRectVisible(R);
    Viewer.UpdateSelectedPageIndex;
  finally
    FViewStateHistoryController.EndUpdate;
  end;
  FViewStateHistoryController.StoreCurrentViewState(vsctSelecting);
end;

procedure TdxPDFViewerController.MakeSelectionRectVisible(APageIndex: Integer; const ARect: TdxRectF);

  function GetRect(ALeft, ATop, ARight, ABottom: Double; APageIndex: Integer): TdxRectF;
  begin
    Result.TopLeft := FSelectionController.Caret.ViewData.TopLeft;
    Result.Right := Result.Left + 1;
    Result.Bottom := Result.Top - Viewer.SelectionController.Caret.ViewData.Height;
    Result := (Viewer.Pages[APageIndex] as TdxPDFViewerPage).ToViewerRect(cxRectAdjustF(Result));
    Result := cxRectOffset(Result, dxPointF(Viewer.ClientRect.TopLeft), False);
  end;

begin
  MakeRectVisible(GetRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, APageIndex), vtFully);
end;

procedure TdxPDFViewerController.OffsetContent(const AOffset: TPoint);
begin
  Viewer.UpdateLeftTopPosition(cxPoint(Viewer.LeftPos - AOffset.X, Viewer.TopPos - AOffset.Y));
end;

procedure TdxPDFViewerController.ScrollPosChanged;
begin
  ViewStateHistoryController.StoreCurrentViewState(vsctScrolling);
  CalculateHitTests(Viewer.GetMouseCursorClientPos);
end;

procedure TdxPDFViewerController.ShowDocumentPosition(const ATarget: TdxPDFTarget);
begin
  FInteractivityController.ShowDocumentPosition(ATarget);
end;

procedure TdxPDFViewerController.OpenAttachment(AAttachment: TdxPDFFileAttachment);
var
  AFileName: string;
begin
  if (AAttachment <> nil) and Viewer.CanOpenAttachment(AAttachment) then
  begin
    AFileName := AAttachment.FileName;
    if AFileName = '' then
      AFileName := TPath.GetRandomFileName;
    AFileName := TPath.Combine(TdxPDFDocumentAccess(Viewer.Document).Repository.FolderName, AFileName);
    DoSaveAttachment(AFileName, AAttachment);
    dxShellExecute(AFileName, SW_SHOWMAXIMIZED);
  end;
end;

procedure TdxPDFViewerController.SaveAttachment(AAttachment: TdxPDFFileAttachment);
var
  APath: string;
begin
  if (AAttachment <> nil) and Viewer.CanSaveAttachment(AAttachment) then
  begin
    APath := GetAttachmentSavingPath(AAttachment);
    if APath <> '' then
      DoSaveAttachment(APath, AAttachment);
  end;
end;

function TdxPDFViewerController.IsDocumentPanning: Boolean;
begin
  Result := Viewer.IsDocumentLoaded and Viewer.DocumentState.HandTool and MouseButtonPressed;
end;

function TdxPDFViewerController.IsDocumentSelecting: Boolean;
begin
  Result := Viewer.IsDocumentLoaded and not IsDocumentPanning and MouseButtonPressed;
end;

function TdxPDFViewerController.InFindPanelRect(const P: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.FindPanel.Bounds, P);
end;

procedure TdxPDFViewerController.Rotate(AAngleDelta: Integer);
begin
  case TdxPDFUtils.NormalizeRotate(TdxPDFUtils.ConvertToIntEx(Viewer.RotationAngle) + AAngleDelta) of
    -90, 270:
      Viewer.RotationAngle := raMinus90;
    90, -270:
      Viewer.RotationAngle := raPlus90;
    -180, 180:
      Viewer.RotationAngle := ra180;
  else
    Viewer.RotationAngle := ra0;
  end;
end;

procedure TdxPDFViewerController.DoFindText;
var
  AOptions: TdxPDFDocumentTextSearchOptions;
begin
  AOptions.CaseSensitive := Viewer.OptionsFindPanel.CaseSensitive;
  AOptions.WholeWords := Viewer.OptionsFindPanel.WholeWords;
  AOptions.Direction := Viewer.OptionsFindPanel.Direction;
  Viewer.TextSearch.Find(Viewer.OptionsFindPanel.SearchString, AOptions);
  FocusedCell := Viewer.ViewInfo.FindPanel.Edit;
end;

procedure TdxPDFViewerController.ExecuteOperation(const AOperation: TdxPDFInteractiveOperation);
begin
  FInteractivityController.ExecuteOperation(AOperation);
end;

function TdxPDFViewerController.GetCursor: TCursor;
begin
  if not DocumentHitTest.HitAtInteractiveObject and Viewer.DocumentState.HandTool then
    Result := GetHandToolCursor
  else
    Result := GetSelectToolCursor;
end;

function TdxPDFViewerController.GetHitTest: TdxPDFViewerCellHitTest;
begin
  Result := Viewer.HitTest;
end;

function TdxPDFViewerController.GetPopupMenuClass: TComponentClass;
begin
  Result := DocumentHitTest.GetPopupMenuClass;
end;

procedure TdxPDFViewerController.CalculateMouseButtonPressed(Shift: TShiftState; X, Y: Integer);
begin
  inherited CalculateMouseButtonPressed(Shift, X, Y);
  FMouseButtonPressed := FMouseButtonPressed and not InFindPanelRect(cxPoint(X, Y));
end;

procedure TdxPDFViewerController.Clear;
begin
  inherited;
  Viewer.BeginUpdate;
  try
    Viewer.HitTest.ResetPreviousRecognizedPage;
    FSelectionController.Clear;
    FViewStateHistoryController.Clear;
    Viewer.DeleteCaret;
    Viewer.HandTool := False;
    Viewer.Highlights.Clear;
    Viewer.TextSearch.Clear;
    Viewer.PageCount := 0;
  finally
    Viewer.EndUpdate;
  end;
end;

function TdxPDFViewerController.GetHandToolCursor: TCursor;
begin
  Result := crDefault;
  if MouseButtonPressed then
    Result := crcxHandDrag
  else
    if not Viewer.IsScrollBarsArea(HitTest.HitPoint) and not cxPointIsInvalid(DocumentHitTest.HitPoint) then
      Result := crcxHand;
end;

function TdxPDFViewerController.GetDocumentHitTest: TdxPDFViewerDocumentHitTest;
begin
  Result := inherited HitTest as TdxPDFViewerDocumentHitTest;
end;

function TdxPDFViewerController.GetSelectToolCursor: TCursor;
begin
  Result := DocumentHitTest.Cursor;
end;

procedure TdxPDFViewerController.SetFocusedField(const AValue: TdxPDFAcroFormField);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFocusedField));
  Viewer.InvalidatePages;
end;

function TdxPDFViewerController.GetAttachmentSavingPath(AAttachment: TdxPDFFileAttachment): string;
var
  ASaveDialog: TSaveDialog;
begin
  ASaveDialog := TSaveDialog.Create(nil);
  try
    ASaveDialog.FileName := AAttachment.FileName;
    ASaveDialog.Filter := '*.*';
    if ASaveDialog.Execute then
      Result := ASaveDialog.FileName
    else
      Result := '';
  finally
    ASaveDialog.Free;
  end;
end;

procedure TdxPDFViewerController.DoSaveAttachment(const APath: string; AAttachment: TdxPDFFileAttachment);
var
  ADirectoryName: string;
begin
  ADirectoryName := TPath.GetDirectoryName(APath);
  if not DirectoryExists(ADirectoryName) then
    if not ForceDirectories(ADirectoryName) then
      TdxPDFUtils.RaiseException('Cannot create temp directory');
  TdxPDFUtils.SaveToFile(APath, AAttachment.Data);
end;

function TdxPDFViewerController.IsAltPressed(AShift: TShiftState): Boolean;
begin
  Result := ssAlt in AShift;
end;

function TdxPDFViewerController.IsCtrlShiftPressed(AShift: TShiftState): Boolean;
begin
  Result := (ssShift in AShift) and IsCtrlPressed;
end;

function TdxPDFViewerController.NeedHorizontalScroll: Boolean;
begin
  Result := Viewer.ZoomFactor > 100;
end;

function TdxPDFViewerController.ProcessViewerKeyDown(var AKey: Word; AShift: TShiftState): Boolean;
var
  AAltPressed: Boolean;
begin
  Result := True;
  AAltPressed := IsAltPressed(AShift);
  case AKey of
    VK_UP:
      if IsCtrlPressed then
        Viewer.GoToPrevPage
      else
        TdxPDFViewerAccess(Viewer).ScrollPage(psdUp);
    VK_DOWN:
      if IsCtrlPressed then
        Viewer.GoToNextPage
      else
        TdxPDFViewerAccess(Viewer).ScrollPage(psdDown);
    VK_LEFT:
      if AAltPressed then
        Viewer.GoToPrevView
      else
        if NeedHorizontalScroll then
          TdxPDFViewerAccess(Viewer).ScrollPage(psdLeft)
        else
          Viewer.GoToPrevPage;
    VK_RIGHT:
      if AAltPressed then
        Viewer.GoToNextView
      else
        if NeedHorizontalScroll then
          TdxPDFViewerAccess(Viewer).ScrollPage(psdRight)
        else
          Viewer.GoToNextPage;
  else
    Result := False;
  end;
end;

procedure TdxPDFViewerController.ClickTimerHandler(Sender: TObject);
begin
  FClickTimer.Enabled := False;
  FCanClick := False;
  UpdateCursor;
end;

procedure TdxPDFViewerController.OnMakeRectVisibleHandler(APageIndex: Integer; const ARect: TdxRectF);

  function IsRectVisible(const ARect: TdxRectF): Boolean;
  var
    R: TdxRectF;
  begin
    Result := TdxPDFUtils.Intersects(R, ARect, dxRectF(Viewer.ClientRect)) and TdxPDFUtils.RectIsEqual(R, ARect, 0.001);
  end;

var
  R: TdxRectF;
begin
  R := (Viewer.Pages[Max(APageIndex, 0)] as TdxPDFViewerPage).ToViewerRect(cxRectAdjustF(ARect));
  if not IsRectVisible(R) then
    MakeRectVisible(cxRectOffset(R, dxPointF(Viewer.ClientRect.TopLeft), False));
end;

procedure TdxPDFViewerController.UpdateFocusedField(AButton: TMouseButton);

  procedure SetFocusedDocumentHitObject;
  begin
    FocusedField := Viewer.HitTest.DocumentHitObject as TdxPDFAcroFormField;
  end;

var
  AIntf: IdxPDFInteractiveObject;
begin
  if Supports(Viewer.HitTest.DocumentHitObject, IdxPDFInteractiveObject, AIntf) then
  begin
    case AButton of
      mbLeft:
        SetFocusedDocumentHitObject;
      mbRight:
        if not AIntf.IsResetFocusingNeeded then
          SetFocusedDocumentHitObject;
    end;
  end
  else
    FocusedField := nil;
end;

{ TdxPDFViewerController.TInteractiveObjectHinHelper }

constructor TdxPDFViewerController.TInteractiveObjectHinHelper.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
  FShowingTimer := cxCreateTimer(ShowingTimerHandler, 1000, False);
end;

destructor TdxPDFViewerController.TInteractiveObjectHinHelper.Destroy;
begin
  FreeAndNil(FShowingTimer);
  inherited Destroy;
end;

procedure TdxPDFViewerController.TInteractiveObjectHinHelper.MouseDown;
begin
  FShowingTimer.Enabled := False;
  inherited MouseDown;
  ResetLastHintElement;
end;

procedure TdxPDFViewerController.TInteractiveObjectHinHelper.EnableShowing;
begin
  FShowingTimer.Enabled := True;
end;

function TdxPDFViewerController.TInteractiveObjectHinHelper.CanShowHint: Boolean;
begin
  Result := FViewer.OptionsBehavior.ShowHints and FViewer.HitTest.CanShowHint;
end;

function TdxPDFViewerController.TInteractiveObjectHinHelper.GetOwnerControl: TcxControl;
begin
  Result := FViewer;
end;

procedure TdxPDFViewerController.TInteractiveObjectHinHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited CorrectHintWindowRect(ARect);
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy + FViewer.ScaleFactor.Apply(5));
end;

procedure TdxPDFViewerController.TInteractiveObjectHinHelper.MouseLeave;
begin
// do nothing
end;

procedure TdxPDFViewerController.TInteractiveObjectHinHelper.ShowingTimerHandler(Sender: TObject);
var
  ABounds: TRect;
begin
  ABounds := FViewer.HitTest.GetHintBounds;
  if not cxRectIsEmpty(ABounds) then
  begin
    ShowHint(ABounds, ABounds, FViewer.HitTest.GetHintText, False, Self, FViewer.Font);
    FShowingTimer.Enabled := False;
  end;
end;

{ TdxPDFViewerCellHitTest }

procedure TdxPDFViewerCellHitTest.Clear;
begin
  inherited Clear;
  FHitCode := 0;
end;

procedure TdxPDFViewerCellHitTest.DoCalculate(const AHitPoint: TPoint);
begin
  inherited DoCalculate(AHitPoint);
  FHitObject := nil;
end;

function TdxPDFViewerCellHitTest.DoGetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPDFViewerCellHitTest.GetCursor: TCursor;
begin
  if Viewer.IsScrollBarsArea(HitPoint) then
    Result := crDefault
  else
    Result := DoGetCursor;
end;

function TdxPDFViewerCellHitTest.GetHitCode(ACode: Integer): Boolean;
begin
  Result := FHitCode and ACode <> 0;
end;

procedure TdxPDFViewerCellHitTest.SetHitCode(ACode: Integer; AValue: Boolean);
begin
  if AValue then
    FHitCode := FHitCode or ACode
  else
    FHitCode := FHitCode and not ACode;
end;

{ TdxPDFViewerCellViewInfo }

constructor TdxPDFViewerCellViewInfo.Create(AController: TdxPDFViewerContainerController);
begin
  inherited Create;
  FController := AController;
  CreateSubClasses;
end;

destructor TdxPDFViewerCellViewInfo.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFViewerCellViewInfo.CreateSubClasses;
begin
  FPainter := Viewer.CreatePainter;
end;

procedure TdxPDFViewerCellViewInfo.DestroySubClasses;
begin
  Controller.CellRemoving(Self);
  FreeAndNil(FPainter);
end;

function TdxPDFViewerCellViewInfo.CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean;
begin
  Result := Visible and cxRectPtIn(Bounds, AHitTest.HitPoint);
  if Result then
    AHitTest.HitObject := Self;
end;

function TdxPDFViewerCellViewInfo.CanDrawContent: Boolean;
begin
  Result := Visible;
end;

function TdxPDFViewerCellViewInfo.CanFocus: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerCellViewInfo.IsFocused: Boolean;
begin
  Result := Self = Controller.FocusedCell;
end;

function TdxPDFViewerCellViewInfo.IsHot: Boolean;
begin
  Result := Self = Controller.HotCell;
end;

function TdxPDFViewerCellViewInfo.IsPressed: Boolean;
begin
  Result := Self = Controller.PressedCell;
end;

function TdxPDFViewerCellViewInfo.GetClipRect: TRect;
begin
  Result := Bounds;
end;

function TdxPDFViewerCellViewInfo.GetFont: TFont;
begin
  Result := Viewer.Font;
end;

function TdxPDFViewerCellViewInfo.GetVisible: Boolean;
begin
  Result := not cxRectIsEmpty(Bounds);
end;

function TdxPDFViewerCellViewInfo.MeasureHeight: Integer;
begin
  Result := 0;
end;

function TdxPDFViewerCellViewInfo.MeasureWidth: Integer;
begin
  Result := 0;
end;

procedure TdxPDFViewerCellViewInfo.Calculate;
begin
// do nothing
end;

procedure TdxPDFViewerCellViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
// do nothing
end;

procedure TdxPDFViewerCellViewInfo.DrawChildren(ACanvas: TcxCanvas);
begin
// do nothing
end;

procedure TdxPDFViewerCellViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
// do nothing
end;

procedure TdxPDFViewerCellViewInfo.Invalidate;
begin
  Viewer.InvalidateRect(Bounds, True);
end;

procedure TdxPDFViewerCellViewInfo.UpdateState;
begin
// do nothing
end;

function TdxPDFViewerCellViewInfo.ApplyScaleFactor(AValue: Integer): Integer;
begin
  Result := Painter.ScaleFactor.Apply(AValue);
end;

function TdxPDFViewerCellViewInfo.ApplyScaleFactor(AValue: TSize): TSize;
begin
  Result := Painter.ScaleFactor.Apply(AValue);
end;

function TdxPDFViewerCellViewInfo.ApplyScaleFactor(AValue: TRect): TRect;
begin
  Result := Painter.ScaleFactor.Apply(AValue);
end;

procedure TdxPDFViewerCellViewInfo.Draw(ACanvas: TcxCanvas; AForce: Boolean = False);
begin
  if ACanvas.RectVisible(Bounds) and CanDrawContent or AForce then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(GetClipRect);
      DrawBackground(ACanvas);
      DrawContent(ACanvas);
      DrawChildren(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxPDFViewerCellViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Viewer.ScaleFactor;
end;

function TdxPDFViewerCellViewInfo.GetViewer: TdxPDFCustomViewer;
begin
  Result := Controller.Viewer;
end;

procedure TdxPDFViewerCellViewInfo.SetBounds(const ABounds: TRect);
begin
  FBounds := ABounds;
  Calculate;
end;

{ TdxPDFViewerViewInfoList }

function TdxPDFViewerViewInfoList.CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean;
var
  AViewInfo: TdxPDFViewerCellViewInfo;
begin
  Result := False;
  for AViewInfo in Self do
    if AViewInfo.CalculateHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxPDFViewerViewInfoList.MaxMeasureHeight: Integer;
var
  AViewInfo: TdxPDFViewerCellViewInfo;
begin
  Result := 0;
  for AViewInfo in Self do
    Result := Max(Result, AViewInfo.MeasureHeight);
end;

procedure TdxPDFViewerViewInfoList.Draw(ACanvas: TcxCanvas);
var
  AViewInfo: TdxPDFViewerCellViewInfo;
begin
  for AViewInfo in Self do
    AViewInfo.Draw(ACanvas);
end;

procedure TdxPDFViewerViewInfoList.UpdateState;
var
  AViewInfo: TdxPDFViewerCellViewInfo;
begin
  for AViewInfo in Self do
    AViewInfo.UpdateState;
end;

{ TdxPDFViewerContainerViewInfo }

function TdxPDFViewerContainerViewInfo.CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest) and CellList.CalculateHitTest(AHitTest);
end;

function TdxPDFViewerContainerViewInfo.MeasureHeight: Integer;
begin
  Result := CellList.MaxMeasureHeight + cxMarginsHeight(ContentMargins);
end;

procedure TdxPDFViewerContainerViewInfo.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCellList := TdxPDFViewerViewInfoList.Create;
  CreateCells;
end;

procedure TdxPDFViewerContainerViewInfo.DestroySubClasses;
begin
  ClearCells;
  FreeAndNil(FCellList);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerContainerViewInfo.DrawChildren(ACanvas: TcxCanvas);
begin
  CellList.Draw(ACanvas);
end;

procedure TdxPDFViewerContainerViewInfo.UpdateState;
begin
  inherited UpdateState;
  CellList.UpdateState;
end;

function TdxPDFViewerContainerViewInfo.GetContentMargins: TRect;
begin
  Result := cxNullRect;
end;

function TdxPDFViewerContainerViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := 0;
end;

procedure TdxPDFViewerContainerViewInfo.Calculate;
var
  I: Integer;
begin
  inherited Calculate;
  if not cxRectIsEmpty(Bounds) then
    DoCalculate
  else
    for I := 0 to CellList.Count - 1 do
      SetEmptyBounds(CellList[I]);
  UpdateState;
end;

procedure TdxPDFViewerContainerViewInfo.DoCalculate;
begin
// do nothing
end;

procedure TdxPDFViewerContainerViewInfo.ClearCells;
begin
  CellList.Clear;
end;

procedure TdxPDFViewerContainerViewInfo.CreateCells;
begin
// do nothing
end;

procedure TdxPDFViewerContainerViewInfo.RecreateCells;
begin
  ClearCells;
  CreateCells;
end;

procedure TdxPDFViewerContainerViewInfo.SetEmptyBounds(AViewInfo: TdxPDFViewerCellViewInfo);
begin
  if AViewInfo <> nil then
    AViewInfo.Bounds := cxNullRect;
end;

function TdxPDFViewerContainerViewInfo.AddCell(ACellClass: TdxPDFViewerCellViewInfoClass): TdxPDFViewerCellViewInfo;
begin
  Result := AddCell(ACellClass, Controller);
end;

function TdxPDFViewerContainerViewInfo.AddCell(ACellClass: TdxPDFViewerCellViewInfoClass;
  AController: TdxPDFViewerContainerController): TdxPDFViewerCellViewInfo;
begin
  Result := ACellClass.Create(AController);
  CellList.Add(Result);
end;

function TdxPDFViewerContainerViewInfo.AlignToTopClientSide(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := AViewInfo <> nil;
  if Result then
  begin
    ARect := cxRectSetTop(R, R.Top, AViewInfo.MeasureHeight);
    AViewInfo.Bounds := cxRectCenterHorizontally(ARect, ARect.Width);
    R.Top := AViewInfo.Bounds.Bottom;
    Inc(R.Top, IndentBetweenElements);
  end;
end;

function TdxPDFViewerContainerViewInfo.AlignToLeftSide(AViewInfo: TdxPDFViewerCellViewInfo;
  var R: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := AViewInfo <> nil;
  if Result then
  begin
    ARect := cxRectSetLeft(R, R.Left, AViewInfo.MeasureWidth);
    AViewInfo.Bounds := cxRectCenterVertically(ARect, AViewInfo.MeasureHeight);
    R.Left := AViewInfo.Bounds.Right;
    Inc(R.Left, IndentBetweenElements);
  end;
end;

function TdxPDFViewerContainerViewInfo.AlignToRightSide(AViewInfo: TdxPDFViewerCellViewInfo; var R: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := AViewInfo <> nil;
  if Result then
  begin
    ARect := cxRectSetRight(R, R.Right, AViewInfo.MeasureWidth);
    AViewInfo.Bounds := cxRectCenterVertically(ARect, AViewInfo.MeasureHeight);
    R.Right := AViewInfo.Bounds.Left;
    Dec(R.Right, IndentBetweenElements);
  end;
end;

function TdxPDFViewerContainerViewInfo.GetContentBounds: TRect;
begin
  Result := cxRectContent(Bounds, ContentMargins);
end;

{ TdxPDFViewerViewInfo }

function TdxPDFViewerViewInfo.CanFocus: Boolean;
begin
  Result := True;
end;

procedure TdxPDFViewerViewInfo.Calculate;
var
  R: TRect;
begin
  RecreateCells;
  R := cxRectInflate(Viewer.Bounds, -Viewer.BorderSize);
  Viewer.NavigationPane.Calculate(R);

  FDocumentViewerBounds := R;
  FBounds := Viewer.Bounds;
  R := Viewer.ClientBounds;
  CalculateFindPanelBounds(R);
  UpdateState;
end;

procedure TdxPDFViewerViewInfo.CreateCells;
begin
  inherited CreateCells;
  CreateFindPanelViewInfo;
end;

function TdxPDFViewerViewInfo.GetTransitionEffectAreaBounds: TRect;
begin
  Result := FindPanel.Bounds;
end;

procedure TdxPDFViewerViewInfo.CreateFindPanelViewInfo;
begin
  FFindPanel := AddCell(TdxPDFViewerFindPanelViewInfo) as TdxPDFViewerFindPanelViewInfo;
end;

procedure TdxPDFViewerViewInfo.Draw(ACanvas: TcxCanvas; AForce: Boolean = False);
begin
  if Viewer.FindPanel.AnimationController.Active then
  begin
    NavigationPane.Draw(ACanvas);
    Viewer.FindPanel.AnimationController.Draw(ACanvas);
  end
  else
    inherited Draw(ACanvas, AForce);
end;

procedure TdxPDFViewerViewInfo.PopulateTabOrders(AList: TdxPDFViewerViewInfoList);

  procedure AddTabOrder(AButton: TdxPDFViewerCellViewInfo);
  begin
    if (AButton <> nil) and AButton.Visible then
      AList.Add(AButton);
  end;

begin
  AddTabOrder(FindPanel.Edit);
  AddTabOrder(FindPanel.OptionsButton);
  if FindPanel.PreviousButton.IsEnabled then
    AddTabOrder(FindPanel.PreviousButton);
  if FindPanel.NextButton.IsEnabled then
    AddTabOrder(FindPanel.NextButton);
  AddTabOrder(FindPanel.CloseButton);
  AddTabOrder(NavigationPane);
end;

function TdxPDFViewerViewInfo.GetController: TdxPDFViewerController;
begin
  Result := inherited Controller as TdxPDFViewerController;
end;

function TdxPDFViewerViewInfo.GetNavigationPane: TdxPDFViewerNavigationPaneViewInfo;
begin
  Result := Viewer.NavigationPane.ViewInfo;
end;

procedure TdxPDFViewerViewInfo.CalculateFindPanelBounds(var ABounds: TRect);
var
  ARect: TRect;
  AWidth: Integer;
begin
  ARect := ABounds;
  AWidth := FindPanel.MeasureWidth(ABounds);
  if AWidth > cxRectWidth(ABounds) then
    ARect := cxNullRect
  else
  begin
    case Viewer.OptionsFindPanel.Alignment of
      fpalBottomLeft, fpalTopLeft:
        ARect.Right := ARect.Left + AWidth;
      fpalBottomRight, fpalTopRight:
        ARect.Left := ARect.Right - AWidth;
      fpalTopCenter, fpalBottomCenter:
        begin
          ARect.Left := cxRectCenter(ARect).X - AWidth div 2;
          ARect.Right := ARect.Left + AWidth;
        end;
    end;

    case Viewer.OptionsFindPanel.Alignment of
      fpalBottomClient, fpalBottomLeft, fpalBottomCenter, fpalBottomRight:
        ARect.Top := ABounds.Bottom - FindPanel.MeasureHeight;
      fpalTopClient, fpalTopLeft, fpalTopCenter, fpalTopRight:
        begin
          ARect.Top := ABounds.Top;
          ARect.Bottom := ARect.Top + FindPanel.MeasureHeight;
        end;
    end;
  end;
  FindPanel.Bounds := ARect;
end;

{ TdxPDFViewerCaptionViewInfo }

function TdxPDFViewerCaptionViewInfo.CanDrawContent: Boolean;
begin
  Result := True;
end;

function TdxPDFViewerCaptionViewInfo.MeasureHeight: Integer;
begin
  Result := cxTextHeight(Font);
end;

function TdxPDFViewerCaptionViewInfo.MeasureWidth: Integer;
begin
  Result := cxTextWidth(Font, Text) + Painter.ScaleFactor.Apply(cxTextOffset * 2);
end;

procedure TdxPDFViewerCaptionViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  ACanvas.SaveState;
  try
    PrepareCanvas(ACanvas);
    ACanvas.DrawTexT(Text, Bounds, GetTextAlignment, vaCenter, False, True);
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxPDFViewerCaptionViewInfo.GetPainterTextColor: TColor;
begin
  Result := Painter.TitleTextColor;
end;

function TdxPDFViewerCaptionViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerFindPanelFindCaption);
end;

function TdxPDFViewerCaptionViewInfo.GetTextAlignment: TAlignment;
begin
  Result := taCenter;
end;

procedure TdxPDFViewerCaptionViewInfo.PrepareCanvas(ACanvas: TcxCanvas);
begin
  ACanvas.Font := Font;
  ACanvas.Font.Color := cxGetActualColor(GetPainterTextColor, clWindowText);
end;

{ TdxPDFViewerButtonViewInfo }

constructor TdxPDFViewerButtonViewInfo.Create(AController: TdxPDFViewerContainerController);
begin
  inherited Create(AController);
  FFadingHelper := TdxPDFViewerButtonFadingHelper.Create(Self);
  FGlyph := TdxSmartGlyph.Create;
  FCaption := GetDefaultCaption;
  FState := cxbsNormal;
end;

destructor TdxPDFViewerButtonViewInfo.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FFadingHelper);
  inherited Destroy;
end;

function TdxPDFViewerButtonViewInfo.CanFocus: Boolean;
begin
  Result := Visible and IsEnabled;
end;

function TdxPDFViewerButtonViewInfo.MeasureHeight: Integer;
var
  AActualMeasureWidth: Integer;
begin
  Result := cxTextHeight(Font) + ApplyScaleFactor(cxMarginsHeight(Margins));
  AActualMeasureWidth := MeasureWidth;
  if GlyphSize.cx > AActualMeasureWidth then
    Result := Trunc(Result * (AActualMeasureWidth / GlyphSize.cx));
end;

function TdxPDFViewerButtonViewInfo.MeasureWidth: Integer;
begin
  Result := ApplyScaleFactor(cxMarginsWidth(Margins)) + GlyphSize.cx
end;

procedure TdxPDFViewerButtonViewInfo.Calculate;
begin
  CalculateCaptionRect;
  CalculateGlyphRect;
end;

procedure TdxPDFViewerButtonViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  DrawFading(ACanvas);
  ACanvas.SaveState;
  try
    DrawButtonContent(ACanvas);
    if CanFocus then
      DrawFocusRect(ACanvas);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxPDFViewerButtonViewInfo.UpdateState;
begin
  State := CalculateState;
end;

function TdxPDFViewerButtonViewInfo.CalculateState: TcxButtonState;
begin
  if not IsEnabled then
    Result := cxbsDisabled
  else
    if IsPressed and (not Viewer.MouseCapture or IsHot) then
      Result := cxbsPressed
    else
      if IsHot then
        Result := cxbsHot
      else
        if IsDefault or IsFocused then
          Result := cxbsDefault
        else
          Result := cxbsNormal;
end;

function TdxPDFViewerButtonViewInfo.IsDefault: Boolean;
begin
  Result := (Controller.FocusedCell = nil) and (Controller.ViewInfo.FindPanel.ActualNextButtonViewInfo = Self);
end;

function TdxPDFViewerButtonViewInfo.IsEnabled: Boolean;
begin
  Result := True;
end;

function TdxPDFViewerButtonViewInfo.IsFadingAvailable: Boolean;
begin
  Result := Painter.IsFadingAvailable;
end;

function TdxPDFViewerButtonViewInfo.IsSkinUsed: Boolean;
begin
  Result := Painter.IsSkinUsed;
end;

function TdxPDFViewerButtonViewInfo.GetDefaultCaption: string;
begin
  Result := '';
end;

function TdxPDFViewerButtonViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := FGlyph;
end;

function TdxPDFViewerButtonViewInfo.GetGlyphAlignmentHorz: TAlignment;
begin
  Result := taLeftJustify;
end;

function TdxPDFViewerButtonViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxPDFViewerButtonViewInfo.GetMargins: TRect;
begin
  Result := Rect(4, 4, 4, 4);
end;

procedure TdxPDFViewerButtonViewInfo.CalculateCaptionRect;
begin
  FCaptionRect := cxRectContent(Bounds, Margins);
end;

procedure TdxPDFViewerButtonViewInfo.CalculateGlyphRect;

  procedure PlaceGlyphRect(const AContentBounds: TRect; const AWidth, AHeight: Integer);
  begin
    FGlyphRect := cxRectCenterVertically(AContentBounds, AHeight);
    if GetGlyphAlignmentHorz = taLeftJustify then
      FGlyphRect := cxRectSetLeft(GlyphRect, AContentBounds.Left, AWidth)
    else
      FGlyphRect := cxRectCenterHorizontally(GlyphRect, AWidth);
  end;

  procedure CheckGlyphRectSize(const AContentBounds: TRect; const AGlyphRect: TRect);
  begin
    if (cxRectWidth(AContentBounds) < cxRectWidth(AGlyphRect)) or
      (cxRectHeight(AContentBounds) < cxRectHeight(AGlyphRect)) then
      FGlyphRect := cxRectProportionalStretch(AContentBounds,
        cxRectWidth(AGlyphRect), cxRectHeight(AGlyphRect));
    PlaceGlyphRect(AContentBounds, cxRectWidth(GlyphRect), cxRectHeight(GlyphRect));
  end;

var
  AContentBounds: TRect;
begin
  FGlyphRect := cxNullRect;
  if not cxSizeIsEmpty(GlyphSize) then
  begin
    AContentBounds := cxRectContent(Bounds, Margins);
    PlaceGlyphRect(AContentBounds, GlyphSize.cx, GlyphSize.cy);
    CheckGlyphRectSize(AContentBounds, GlyphRect);
    FCaptionRect.Left := GlyphRect.Right + ApplyScaleFactor(cxTextOffset);
  end;
end;

procedure TdxPDFViewerButtonViewInfo.DoExecute;
begin
// do nothing
end;

procedure TdxPDFViewerButtonViewInfo.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  Painter.DrawButtonBackground(ACanvas, ARect, AState);
end;

procedure TdxPDFViewerButtonViewInfo.DrawButtonContent(ACanvas: TcxCanvas);
begin
  ACanvas.Font := Font;
  ACanvas.Font.Color := Painter.ButtonSymbolColor(State);
  if not cxRectIsEmpty(CaptionRect) then
  begin
    cxDrawText(ACanvas, FCaption, cxRectOffset(CaptionRect, GetButtonOffset(State)),
      DT_VCENTER or DT_SINGLELINE or DT_CENTER);
  end;
end;

procedure TdxPDFViewerButtonViewInfo.Execute;
begin
  if IsEnabled then
    DoExecute;
end;

function TdxPDFViewerButtonViewInfo.GetButtonOffset(AButtonState: TcxButtonState): TPoint;
var
  AShift: Integer;
begin
  if AButtonState = cxbsPressed then
  begin
    AShift := Painter.ButtonTextShift;
    Result := cxPoint(AShift, AShift);
  end
  else
    Result := cxNullPoint;
end;

procedure TdxPDFViewerButtonViewInfo.DrawButton(ACanvas: TcxCanvas);
begin
  Painter.DrawButton(ACanvas, Bounds, FCaption, State);
end;

procedure TdxPDFViewerButtonViewInfo.DrawFading(ACanvas: TcxCanvas);
begin
  if not FadingHelper.DrawImage(ACanvas.Handle, Bounds) then
    DrawButtonBackground(ACanvas, Bounds, State);
end;

procedure TdxPDFViewerButtonViewInfo.DrawFocusRect(ACanvas: TcxCanvas);
begin
  if IsFocused then
    Painter.DrawFocusRect(ACanvas, Bounds);
end;

function TdxPDFViewerButtonViewInfo.GetGlyphSize: TSize;
var
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := GetGlyph;
  if AGlyph <> nil then
    Result := ApplyScaleFactor(AGlyph.Size)
  else
    Result := cxNullSize;
end;

procedure TdxPDFViewerButtonViewInfo.SetState(const AState: TcxButtonState);
begin
  if State <> AState then
  begin
    FadingHelper.CheckStartFading(State, AState);
    FState := AState;
    Invalidate;
  end;
end;

function TdxPDFViewerButtonViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, P);
end;

function TdxPDFViewerButtonViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerButtonViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

{ TdxPDFViewerCustomDropDownButtonViewInfo }

function TdxPDFViewerCustomDropDownButtonViewInfo.IsEnabled: Boolean;
begin
  Result := not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerCustomDropDownButtonViewInfo.MeasureWidth: Integer;
begin
  Result := cxMarginsWidth(Margins) + GlyphSize.cx + Painter.DropDownButtonWidth;
end;

procedure TdxPDFViewerCustomDropDownButtonViewInfo.Calculate;
begin
  inherited Calculate;
  CalculateDropDownButtonArrowRect;
end;

procedure TdxPDFViewerCustomDropDownButtonViewInfo.DoExecute;
var
  APopupMenu: TComponent;
begin
  APopupMenu := GetPopupMenuClass.Create(Viewer);
  if APopupMenu <> nil then
    try
      TdxPDFViewerPopupMenuAccess(APopupMenu).Popup(Viewer.ClientToScreen(cxPoint(Bounds.Left, Bounds.Bottom)));
    finally
      APopupMenu.Free;
    end;
end;

procedure TdxPDFViewerCustomDropDownButtonViewInfo.DrawButtonContent(ACanvas: TcxCanvas);
begin
  Painter.DrawDropDownButton(ACanvas, FDropDownButtonArrowRect, State);
  Painter.DrawDropDownButtonGlyph(ACanvas, GetGlyph, GlyphRect, State, GetColorizeGlyph);
end;

procedure TdxPDFViewerCustomDropDownButtonViewInfo.CalculateDropDownButtonArrowRect;
begin
  FDropDownButtonArrowRect := FGlyphRect;
  FDropDownButtonArrowRect.Left := FGlyphRect.Right;
  FDropDownButtonArrowRect.Right := FDropDownButtonArrowRect.Left + Painter.DropDownButtonWidth;
  if not Painter.IsSkinUsed then
    Inc(FDropDownButtonArrowRect.Right, 2);
end;

function TdxPDFViewerCustomDropDownButtonViewInfo.GetColorizeGlyph: Boolean;
begin
  Result := True;
end;

{ TdxPDFViewerButtonFadingHelper }

constructor TdxPDFViewerButtonFadingHelper.Create(AViewInfo: TdxPDFViewerButtonViewInfo);
begin
  inherited Create;
  FButtonViewInfo := AViewInfo;
end;

function TdxPDFViewerButtonFadingHelper.CanFade: Boolean;
begin
  Result := ButtonViewInfo.IsFadingAvailable and not ButtonViewInfo.Viewer.IsDesigning;
end;

procedure TdxPDFViewerButtonFadingHelper.DrawFadeImage;
begin
  ButtonViewInfo.Invalidate;
end;

procedure TdxPDFViewerButtonFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(ButtonViewInfo.Bounds, True);
    ButtonViewInfo.DrawButtonBackground(Result.cxCanvas, Result.ClientRect, AState);
  end;

const
  StateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsDefault);
begin
  AFadeOutImage := PrepareImage(StateMap[ButtonViewInfo.IsDefault or ButtonViewInfo.IsFocused]);
  AFadeInImage := PrepareImage(cxbsHot);
end;

{ TdxPDFViewerImageAnimationTransition }

constructor TdxPDFViewerImageAnimationTransition.Create(AImage: TGraphic; ATime: Cardinal;
  AMode: TdxDrawAnimationMode; AIsHiding: Boolean);
begin
  FMode := AMode;
  FIsHiding := AIsHiding;
  PrepareImage(AImage);
  FDestination := TcxBitmap32.CreateSize(FImage.Width, FImage.Height);
  inherited Create(ATime, ateLinear, TransitionLength(FImage.Height));
end;

destructor TdxPDFViewerImageAnimationTransition.Destroy;
begin
  FreeAndNil(FDestination);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxPDFViewerImageAnimationTransition.Draw(ACanvas: TCanvas; const ADestRect: TRect);
begin
  FDestination.Clear;
  cxBitBlt(FDestination.Canvas.Handle, ACanvas.Handle, cxRect(cxRectSize(ADestRect)), ADestRect.TopLeft, SRCCOPY);
  dxGPPaintCanvas.BeginPaint(FDestination.Canvas.Handle, ADestRect);
  try
    Draw(dxGPPaintCanvas, cxRect(cxRectSize(ADestRect)));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
  cxBitBlt(ACanvas.Handle, FDestination.Canvas.Handle, ADestRect, cxNullPoint, SRCCOPY);
end;

function TdxPDFViewerImageAnimationTransition.TransitionLength(AImageHeight: Integer): Integer;
begin
  Result := IfThen(FMode = amFade, 100, AImageHeight);
end;

procedure TdxPDFViewerImageAnimationTransition.Draw(AGraphics: TdxGPGraphics; const ADestRect: TRect);
var
  APosition, AWidth, AHeight: Integer;
begin
  AWidth := ADestRect.Right - ADestRect.Left;
  AHeight := ADestRect.Bottom - ADestRect.Top;
  APosition := Position;
  case FMode of
    amScrollUp:
      DrawScrollUp(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, APosition);
    amScrollUpFade:
      DrawScrollUpFade(AGraphics, ADestRect.Left, ADestRect.Bottom, AWidth, AHeight, APosition);
    amScrollDown:
      DrawScrollDown(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, APosition);
    amScrollDownFade:
      DrawScrollDownFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, APosition);
    amFade:
      begin
        APosition := IfThen(not FIsHiding, Position, Integer(Length) - Position);
        DrawFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, APosition);
      end;
  end;
end;

procedure TdxPDFViewerImageAnimationTransition.DrawFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
begin
  AGraphics.Draw(FImage, cxRectBounds(ALeft, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), MulDiv(255, AProgress, 100));
end;

procedure TdxPDFViewerImageAnimationTransition.DrawScrollDown(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(FImage, cxRectBounds(ALeft, ATop - AHeight + AOffset, AWidth, AHeight));
end;

procedure TdxPDFViewerImageAnimationTransition.DrawScrollDownFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(FImage, cxRectBounds(ALeft, ATop + AOffset, AWidth, AHeight));
end;

procedure TdxPDFViewerImageAnimationTransition.DrawScrollUp(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(FImage, cxRectBounds(ALeft, ATop - AOffset, AWidth, AHeight));
end;

procedure TdxPDFViewerImageAnimationTransition.DrawScrollUpFade(AGraphics: TdxGPGraphics; ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(FImage, cxRectBounds(ALeft, ATop - AOffset, AWidth, AHeight));
end;

procedure TdxPDFViewerImageAnimationTransition.PrepareImage(AImage: TGraphic);
var
  ATemp: TcxBitmap32;
begin
  ATemp := TcxBitmap32.CreateSize(AImage.Width, AImage.Height);
  try
    ATemp.Canvas.Draw(0, 0, AImage);
    FImage := TdxGPImage.CreateFromBitmap(ATemp);
  finally
    ATemp.Free;
  end;
end;

{ TdxPDFViewerFindPanelAnimationController }

procedure TdxPDFViewerFindPanelAnimationController.Animate(AShowing: Boolean);
begin
  FActive := Viewer.CanUseAnimation;
  if Active then
    try
      DoAnimate(AShowing);
    finally
      FActive := False;
    end;
end;

procedure TdxPDFViewerFindPanelAnimationController.Draw(ACanvas: TcxCanvas);
begin
  if FAnimation <> nil then
    FAnimation.Draw(ACanvas.Canvas, FAnimatedRect);
end;

function TdxPDFViewerFindPanelAnimationController.CreateFindPanelBitmap: TcxBitmap;
var
  R: TRect;
begin
  Result := TcxBitmap.CreateSize(FAnimatedRect, pf24bit);
  Result.Canvas.Lock;
  try
    Result.cxCanvas.WindowOrg := FAnimatedRect.TopLeft;
    try
      ViewInfo.FindPanel.Draw(Result.cxCanvas, True);
    finally
      Result.cxCanvas.WindowOrg := cxNullPoint;
    end;
    ViewInfo.FindPanel.Edit.InternalEdit.Visible := True;
    ViewInfo.FindPanel.Edit.Calculate;
    R := FAnimatedRect;
    R.Intersect(ViewInfo.FindPanel.Edit.Bounds);
    SendMessage(ViewInfo.FindPanel.Edit.InternalEdit.Handle, WM_SETREDRAW, 1, 0);
    try
      cxPaintControlTo(ViewInfo.FindPanel.Edit.InternalEdit, Result.cxCanvas,
        cxPoint(R.Left - FAnimatedRect.Left, R.Top - FAnimatedRect.Top),
        ViewInfo.FindPanel.Edit.InternalEdit.Bounds, True, False);
    finally
      ViewInfo.FindPanel.Edit.InternalEdit.Visible := False;
      SendMessage(ViewInfo.FindPanel.Edit.InternalEdit.Handle, WM_SETREDRAW, 0, 0);
    end;
  finally
    Result.Canvas.Unlock;
  end;
end;

function TdxPDFViewerFindPanelAnimationController.GetAnimation: TdxPDFViewerFindPanelAnimation;
begin
  Result := Viewer.OptionsFindPanel.Animation;
end;

procedure TdxPDFViewerFindPanelAnimationController.DoAnimate(AShowing: Boolean);

  function GetMode: TdxDrawAnimationMode;
  begin
    Result := amFade;
    if GetAnimation = fpaSlide then
      case Viewer.OptionsFindPanel.Alignment of
        fpalTopClient, fpalTopLeft, fpalTopCenter, fpalTopRight:
          if AShowing then
            Result := amScrollDown
          else
            Result := amScrollUp;
        fpalBottomClient, fpalBottomLeft, fpalBottomCenter, fpalBottomRight:
          if AShowing then
            Result := amScrollUpFade
          else
            Result := amScrollDownFade;
      end;
  end;

var
  AFindPanelBitmap: TcxBitmap;
begin
  FAnimatedRect := ViewInfo.FindPanel.Bounds;
  AFindPanelBitmap := CreateFindPanelBitmap;
  try
    RunAnimation(AFindPanelBitmap, Viewer.OptionsFindPanel.AnimationTime, GetMode, not AShowing);
  finally
    AFindPanelBitmap.Free;
  end;
end;

procedure TdxPDFViewerFindPanelAnimationController.AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
begin
  if AFinished then
    RedrawArea(Viewer.ClientRect)
  else
    RedrawArea(FAnimatedRect);
end;

procedure TdxPDFViewerFindPanelAnimationController.RedrawArea(const ARect: TRect);
begin
  Viewer.InvalidateRect(ARect, False);
  Viewer.Update;
end;

procedure TdxPDFViewerFindPanelAnimationController.RunAnimation(AImage: TBitmap; ATime: Cardinal;
  AMode: TdxDrawAnimationMode; AIsHiding: Boolean);
begin
  FAnimation := TdxPDFViewerImageAnimationTransition.Create(AImage, ATime, AMode, AIsHiding);
  try
    Viewer.DeleteCaret;
    FAnimation.FreeOnTerminate := False;
    FAnimation.OnAnimate := AnimationHandler;
    FAnimation.ImmediateAnimation;
  finally
    FreeAndNil(FAnimation);
  end;
end;

{ TdxPDFViewerFindPanelViewInfo }

function TdxPDFViewerFindPanelViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(dxPDFViewerFindPanelContentMargins);
end;

function TdxPDFViewerFindPanelViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := ScaleFactor.Apply(dxPDFViewerIndentBetweenElements);
end;

function TdxPDFViewerFindPanelViewInfo.GetVisible: Boolean;
begin
  Result := inherited GetVisible and Viewer.IsFindPanelVisible;
end;

function TdxPDFViewerFindPanelViewInfo.MeasureWidth(const ABounds: TRect): Integer;

  function MeasureWidth(AViewInfo: TdxPDFViewerCellViewInfo): Integer;
  begin
    if AViewInfo <> nil then
      Result := AViewInfo.MeasureWidth + dxPDFViewerIndentBetweenElements
    else
      Result := 0;
  end;

var
  AEditWidth: Integer;
begin
  Result := cxMarginsWidth(ContentMargins);
  Inc(Result, MeasureWidth(FCaption));
  Inc(Result, MeasureWidth(FCloseButton));
  Inc(Result, MeasureWidth(FNextButton));
  Inc(Result, MeasureWidth(FPreviousButton));
  Inc(Result, MeasureWidth(FOptions));
  Inc(Result, MeasureWidth(FEdit));
  if FEdit <> nil then
  begin
    AEditWidth := FEdit.MeasureWidth;
    while (Result > cxRectWidth(ABounds)) and (AEditWidth > FEdit.MinWidth) do
    begin
      Dec(Result, AEditWidth + IndentBetweenElements);
      Dec(AEditWidth, 1);
      Inc(Result, AEditWidth + IndentBetweenElements);
    end;
    while (Result < cxRectWidth(ABounds)) and (AEditWidth < FEdit.MaxWidth) do
    begin
      Dec(Result, AEditWidth + IndentBetweenElements);
      Inc(AEditWidth, 1);
      Inc(Result, AEditWidth + IndentBetweenElements);
    end;
    dxAdjustToTouchableSize(AEditWidth, ScaleFactor);
    FEdit.ActualWidth := AEditWidth;
  end;
end;

procedure TdxPDFViewerFindPanelViewInfo.DoCalculate;
var
  AContentBounds: TRect;
begin
  inherited DoCalculate;
  AContentBounds := cxRectContent(Bounds, dxPDFViewerFindPanelContentMargins);
  AlignToRightSide(FCloseButton, AContentBounds);
  AlignToLeftSide(FCaption, AContentBounds);
  AlignToLeftSide(FEdit, AContentBounds);
  AlignToLeftSide(FOptions, AContentBounds);
  AlignToLeftSide(FPreviousButton, AContentBounds);
  AlignToLeftSide(FNextButton, AContentBounds);
end;

procedure TdxPDFViewerFindPanelViewInfo.CreateCells;
begin
  inherited CreateCells;
  FCaption := AddCell(TdxPDFViewerCaptionViewInfo) as TdxPDFViewerCaptionViewInfo;
  if Viewer.OptionsFindPanel.ShowCloseButton and (Viewer.OptionsFindPanel.DisplayMode = fpdmManual) then
    FCloseButton := AddCell(TCloseButtonViewInfo) as TCloseButtonViewInfo;
  FEdit := AddCell(TEditViewInfo) as TEditViewInfo;
  if Viewer.OptionsFindPanel.ShowNextButton then
    FNextButton := AddCell(TNextButtonViewInfo) as TNextButtonViewInfo;
  if Viewer.OptionsFindPanel.ShowPreviousButton then
    FPreviousButton := AddCell(TPreviousButtonViewInfo) as TPreviousButtonViewInfo;
  if Viewer.OptionsFindPanel.ShowOptionsButton then
    FOptions := AddCell(TOptionsButtonViewInfo) as TOptionsButtonViewInfo;
end;

procedure TdxPDFViewerFindPanelViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawFindPanelBackground(ACanvas, Bounds);
end;

procedure TdxPDFViewerFindPanelViewInfo.Update;
begin
  Calculate;
  UpdateState;
  Invalidate;
end;

function TdxPDFViewerFindPanelViewInfo.GetActualNextButtonViewInfo: TButtonViewInfo;
begin
  Result := CloseButton;
end;

{ TdxPDFViewerFindPanelViewInfo.TEditViewInfo }

constructor TdxPDFViewerFindPanelViewInfo.TEditViewInfo.Create(AController: TdxPDFViewerContainerController);
begin
  inherited Create(AController);
  ActualWidth := MaxWidth;
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.CanFocus: Boolean;
begin
  Result := (InternalEdit <> nil) and InternalEdit.CanFocus;
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.MeasureHeight: Integer;
begin
  Result := ApplyScaleFactor(21);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.MeasureWidth: Integer;
begin
  Result := ActualWidth;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

procedure TdxPDFViewerFindPanelViewInfo.TEditViewInfo.Calculate;
begin
  inherited Calculate;
  if InternalEdit.Visible then
    InternalEdit.BoundsRect := Bounds
  else
    InternalEdit.BoundsRect := cxNullRect;
end;

procedure TdxPDFViewerFindPanelViewInfo.TEditViewInfo.UpdateState;
begin
  InternalEdit.Enabled := not Viewer.TextSearch.IsLocked;
end;

procedure TdxPDFViewerFindPanelViewInfo.TEditViewInfo.SetFocus;
begin
  if InternalEdit.HandleAllocated then
  begin
    InternalEdit.SetFocus;
    InternalEdit.SelectAll;
  end;
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.GetEdit: TdxPDFViewerFindPanelTextEdit;
begin
  Result := Viewer.FindPanel.Edit;
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.GetMaxWidth: Integer;
begin
  Result := ApplyScaleFactor(dxPDFViewerFindPanelEditMaxWidth);
end;

function TdxPDFViewerFindPanelViewInfo.TEditViewInfo.GetMinWidth: Integer;
begin
  Result := ApplyScaleFactor(dxPDFViewerFindPanelEditMinWidth);
end;

{ TdxPDFViewerFindPanelViewInfo.TButtonViewInfo }

function TdxPDFViewerFindPanelViewInfo.TButtonViewInfo.IsEnabled: Boolean;
begin
  Result := (Controller as TdxPDFViewerController).CanSearchText;
end;

function TdxPDFViewerFindPanelViewInfo.TButtonViewInfo.MeasureWidth: Integer;
begin
  Result := ApplyScaleFactor(dxPDFViewerFindPanelDefaultButtonWidth);
end;

{ TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo }

function TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.IsEnabled: Boolean;
begin
  Result := not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.MeasureHeight: Integer;
begin
  Result := Painter.FindPanelCloseButtonSize.cy;
end;

function TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.MeasureWidth: Integer;
begin
  Result := Painter.FindPanelCloseButtonSize.cx;
end;

function TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.GetVisible: Boolean;
begin
  Result := Viewer.OptionsFindPanel.ShowCloseButton;
end;

procedure TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.DoExecute;
begin
  Viewer.HideFindPanel;
end;

procedure TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
//
end;

procedure TdxPDFViewerFindPanelViewInfo.TCloseButtonViewInfo.DrawButtonContent(ACanvas: TcxCanvas);
begin
  Painter.DrawFindPanelCloseButton(ACanvas, Bounds, State);
end;

{ TdxPDFViewerFindPanelViewInfo.TSearchButtonViewInfo }

procedure TdxPDFViewerFindPanelViewInfo.TSearchButtonViewInfo.DoExecute;
begin
  Viewer.OptionsFindPanel.Direction := GetSearchDirection;
  (Controller as TdxPDFViewerController).DoFindText;
end;

function TdxPDFViewerFindPanelViewInfo.TSearchButtonViewInfo.GetSearchDirection: TdxPDFDocumentTextSearchDirection;
begin
  Result := tsdForward;
end;

{ TdxPDFViewerFindPanelViewInfo.TNextButtonViewInfo }

function TdxPDFViewerFindPanelViewInfo.TNextButtonViewInfo.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerFindPanelNextButtonCaption);
end;

function TdxPDFViewerFindPanelViewInfo.TNextButtonViewInfo.GetVisible: Boolean;
begin
  Result := Viewer.OptionsFindPanel.ShowNextButton;
end;

{ TdxPDFViewerFindPanelViewInfo.TOptionsButtonViewInfo }

function TdxPDFViewerFindPanelViewInfo.TOptionsButtonViewInfo.IsEnabled: Boolean;
begin
  Result := not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerFindPanelViewInfo.TOptionsButtonViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.FindPanel.OptionsButtonGlyph;
end;

function TdxPDFViewerFindPanelViewInfo.TOptionsButtonViewInfo.GetPopupMenuClass: TComponentClass;
begin
  Result := TdxPDFViewerFindPanelOptionsPopupMenu;
end;

function TdxPDFViewerFindPanelViewInfo.TOptionsButtonViewInfo.GetVisible: Boolean;
begin
  Result := Viewer.OptionsFindPanel.ShowOptionsButton;
end;

{ TdxPDFViewerFindPanelViewInfo.TPreviousButtonViewInfo }

function TdxPDFViewerFindPanelViewInfo.TPreviousButtonViewInfo.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerFindPanelPreviousButtonCaption);
end;

function TdxPDFViewerFindPanelViewInfo.TPreviousButtonViewInfo.GetSearchDirection: TdxPDFDocumentTextSearchDirection;
begin
  Result := tsdBackward;
end;

function TdxPDFViewerFindPanelViewInfo.TPreviousButtonViewInfo.GetVisible: Boolean;
begin
  Result := Viewer.OptionsFindPanel.ShowPreviousButton;
end;

{ TdxPDFViewerInteractivityController }

class procedure TdxPDFViewerInteractivityController.Execute(AViewer: TdxPDFCustomViewer; AOutline: TdxPDFOutlineTreeItem);
begin
  if AOutline <> nil then
    Execute(AViewer, TdxPDFOutlineTreeItemAccess(AOutline).InteractiveOperation);
end;

class procedure TdxPDFViewerInteractivityController.Execute(AViewer: TdxPDFCustomViewer; AHyperlink: TdxPDFHyperlink);
begin
  if AHyperlink <> nil then
    Execute(AViewer, TdxPDFHyperlinkAccess(AHyperlink).InteractiveOperation);
end;

class procedure TdxPDFViewerInteractivityController.Execute(AViewer: TdxPDFCustomViewer;
  const AOperation: TdxPDFInteractiveOperation);
var
  AController: TdxPDFViewerInteractivityController;
begin
  if AViewer <> nil then
  begin
    AController := TdxPDFViewerInteractivityController.Create(AViewer);
    try
      AController.ExecuteOperation(AOperation);
    finally
      AController.Free;
    end;
  end;
end;

procedure TdxPDFViewerInteractivityController.ExecuteOperation(const AOperation: TdxPDFInteractiveOperation);
var
  AExecutedActions: TList<TdxPDFCustomAction>;
begin
  if AOperation.IsValid then
  begin
    ShowDocumentPosition(AOperation.Target);
    AExecutedActions := TList<TdxPDFCustomAction>.Create;
    try
      ExecuteActions(AOperation.Action, AExecutedActions);
    finally
      AExecutedActions.Free;
    end;
  end;
end;

procedure TdxPDFViewerInteractivityController.ShowDocumentPosition(const ATarget: TdxPDFTarget);
var
  APage: TdxPDFViewerPage;
  P: TdxPointF;
  ARect: TdxRectF;
begin
  if ATarget.IsValid and InRange(ATarget.PageIndex, 0, Viewer.PageCount - 1) then
  begin
    Viewer.ViewerController.ViewStateHistoryController.BeginUpdate;
    Viewer.BeginUpdate;
    try
      APage := Viewer.Pages[ATarget.PageIndex] as TdxPDFViewerPage;
      if ATarget.Mode = tmXYZ then
      begin
        P := dxPointF(ATarget.X - TdxPDFPageAccess(APage.DocumentPage).CropBox.Left,
          ATarget.Y - TdxPDFPageAccess(APage.DocumentPage).CropBox.Bottom);
        ARect := APage.ToViewerRect(dxRectF(P.X, P.Y, P.X + 1, P.Y - 1));
      end
      else
        ARect := APage.ToViewerRect(TdxPDFPageAccess(APage.DocumentPage).CropBox);
      if TdxPDFUtils.IsDoubleValid(ATarget.Zoom) then
        Viewer.OptionsZoom.ZoomFactor := Trunc(ATarget.Zoom * 100);
    finally
      Viewer.EndUpdate;
    end;
    Viewer.ViewerController.MakeRectVisible(ARect, vtFully, True);
    Viewer.ViewerController.ViewStateHistoryController.EndUpdate;
    Viewer.ViewerController.ViewStateHistoryController.StoreCurrentViewState(vsctScrolling);
  end;
end;

function TdxPDFViewerInteractivityController.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxPDFViewerInteractivityController._AddRef: Integer;
begin
  Result := -1;
end;

function TdxPDFViewerInteractivityController._Release: Integer;
begin
  Result := -1;
end;

procedure TdxPDFViewerInteractivityController.ExecuteOperation(AField: TdxPDFAcroFormActionField);
begin
  Execute(Viewer, TdxPDFAcroFormActionFieldAccess(AField).InteractiveOperation);
end;

procedure TdxPDFViewerInteractivityController.GoToFirstPage;
begin
  Viewer.GoToFirstPage;
end;

procedure TdxPDFViewerInteractivityController.GoToLastPage;
begin
  Viewer.GoToLastPage;
end;

procedure TdxPDFViewerInteractivityController.GoToNextPage;
begin
  Viewer.GoToNextPage;
end;

procedure TdxPDFViewerInteractivityController.GoToPrevPage;
begin
  Viewer.GoToPrevPage;
end;

procedure TdxPDFViewerInteractivityController.OpenUri(const AUri: string);
begin
  if (AUri <> '') and Viewer.CanOpenUri(AUri) then
    dxShellExecute(AUri, SW_SHOWMAXIMIZED);
end;

procedure TdxPDFViewerInteractivityController.ExecuteActions(AAction: TdxPDFCustomAction;
  AExecutedActions: TList<TdxPDFCustomAction>);
var
  ACurrentAction: TdxPDFCustomAction;
begin
  if (AAction <> nil) and not AExecutedActions.Contains(AAction) then
  begin
    TdxPDFCustomActionAccess(AAction).Execute(Self);
    AExecutedActions.Add(AAction);
    if TdxPDFCustomActionAccess(AAction).Next <> nil then
      for ACurrentAction in TdxPDFCustomActionAccess(AAction).Next do
        ExecuteActions(ACurrentAction, AExecutedActions);
  end;
end;

{ TdxPDFViewerNavigationPaneInternalControl }

procedure TdxPDFViewerNavigationPaneInternalControl.Clear;
begin
  ClearInternalControl;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.Refresh;
begin
  Clear;
  PopulateInternalControl;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.UpdateState;
begin
  Viewer.NavigationPane.ViewInfo.UpdateState;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.UpdateTextSize;
begin
  UpdateInternalControlTextSize;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.CreateSubClasses;
begin
  inherited CreateSubClasses;
  CreateInternalControl;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.DestroySubClasses;
begin
  FreeAndNil(FInternalControl);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.InitializeInternalControl;
begin
  FInternalControl.Parent := Viewer;
  FInternalControl.Visible := False;
end;

function TdxPDFViewerNavigationPaneInternalControl.GetBounds: TRect;
begin
  Result := FInternalControl.BoundsRect;
end;

function TdxPDFViewerNavigationPaneInternalControl.GetVisible: Boolean;
begin
  Result := FInternalControl.Visible;
end;

procedure TdxPDFViewerNavigationPaneInternalControl.SetBounds(const AValue: TRect);
begin
  if not cxRectIsEqual(FInternalControl.BoundsRect, AValue) then
  begin
    FInternalControl.BoundsRect := AValue;
    FInternalControl.Visible := not cxRectIsEmpty(FInternalControl.BoundsRect);
  end;
end;

{ TdxPDFViewerBookmarkTreeView }

function TdxPDFViewerBookmarkTreeView.CanExpandSelectedBookmark: Boolean;
begin
  Result := IsBookmarkSelected and (TreeView.Selected.Count > 0) and not TreeView.Selected.Expanded;
end;

function TdxPDFViewerBookmarkTreeView.IsBookmarkSelected: Boolean;
begin
  Result := TreeView.SelectionCount > 0;
end;

function TdxPDFViewerBookmarkTreeView.IsTopLevelBookmarksExpanded: Boolean;
var
  ANode: TTreeNode;
begin
  Result := False;
  ANode := TreeView.Items.GetFirstNode;
  while ANode <> nil do
  begin
    Result := (ANode.Count > 0) and ANode.Expanded;
    if Result then
      Break;
    ANode := ANode.getNextSibling;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.ExpandCollapseTopLevelBookmarks;
var
  AExpanded: Boolean;
  ANode: TTreeNode;
begin
  if TreeView.Items.Count > 0 then
  begin
    AExpanded := not IsTopLevelBookmarksExpanded;
    TreeView.Items.BeginUpdate;
    try
      ANode := TreeView.Items.GetFirstNode;
      while ANode <> nil do
      begin
        ANode.Expanded := AExpanded;
        ANode := ANode.getNextSibling;
      end;
    finally
      TreeView.Items.EndUpdate;
      TreeView.Items[0].MakeVisible;
    end;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.ExpandCurrentBookmark;
begin
  if IsBookmarkSelected then
    TreeView.Selected.Expand(False);
end;

procedure TdxPDFViewerBookmarkTreeView.GoToBookmark;
var
  AOutline: TdxPDFOutlineTreeItem;
begin
  AOutline := SelectedOutline;
  if AOutline <> nil then
    Viewer.NavigationPane.Controller.ExecuteOperation(TdxPDFOutlineTreeItemAccess(AOutline).InteractiveOperation);
end;

procedure TdxPDFViewerBookmarkTreeView.PrintSelectedPages(APrintSections: Boolean);
var
  APages: TIntegerDynArray;
begin
  if Viewer.CanPrint then
  begin
    APages := GetPrintPageNumbers(APrintSections);
    if Length(APages) > 0  then
      dxPrintingRepository.PrintReport(Viewer, APages);
  end;
end;

function TdxPDFViewerBookmarkTreeView.GetEmpty: Boolean;
begin
  Result := OutlineTree = nil;
  if not Result then
    Result := OutlineTree.Count = 0;
end;

procedure TdxPDFViewerBookmarkTreeView.ClearInternalControl;
begin
  TreeView.Items.Clear;
end;

procedure TdxPDFViewerBookmarkTreeView.CreateInternalControl;
begin
  FInternalControl := TTree.Create(Viewer);
  InitializeInternalControl;
end;

procedure TdxPDFViewerBookmarkTreeView.InitializeInternalControl;
var
  ATreeView: TTree;
begin
  inherited InitializeInternalControl;
  ATreeView := TreeView;
  ATreeView.Style.LookAndFeel.MasterLookAndFeel := Viewer.LookAndFeel;
  ATreeView.OnCustomDrawItem := OnCustomDrawItemHandler;
  ATreeView.OnMouseUp := OnMouseUpHandler;
  ATreeView.OnKeyDown := OnKeyDownHandler;
  ATreeView.OnExpanded := OnChangeHandler;
  ATreeView.OnCollapsed := OnChangeHandler;
  ATreeView.OnChange := OnChangeHandler;
end;

procedure TdxPDFViewerBookmarkTreeView.PopulateInternalControl;

  procedure PopulateTree(AParent: TTreeNode; AParentID, AStartIndex: Integer);
  var
    AIndex: Integer;
    AOutline: TdxPDFOutlineTreeItemAccess;
    ANode: TTreeNode;
  begin
    TreeView.Items.BeginUpdate;
    try
      for AIndex := AStartIndex to OutlineTree.Count - 1 do
      begin
        AOutline := TdxPDFOutlineTreeItemAccess(OutlineTree[AIndex]);
        if AOutline.ParentID = AParentID then
        begin
          ANode := TreeView.Items.AddChildObject(AParent, AOutline.Title, AOutline);
          if AOutline.HasChildren then
            PopulateTree(ANode, AOutline.ID, AIndex);
        end;
      end;
    finally
      TreeView.Items.EndUpdate;
    end;
  end;

begin
  if OutlineTree <> nil then
    PopulateTree(nil, 0, 0);
end;

procedure TdxPDFViewerBookmarkTreeView.UpdateInternalControlTextSize;
begin
  TreeView.SetTextSize(Viewer.OptionsNavigationPane.Bookmarks.TextSize);
end;

function TdxPDFViewerBookmarkTreeView.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TreeView.LookAndFeel;
end;

function TdxPDFViewerBookmarkTreeView.GetOutlineTree: TdxPDFOutlineTree;
begin
  if Viewer.IsDocumentLoaded then
    Result := TdxPDFDocumentAccess(Viewer.Document).OutlineTree
  else
    Result := nil;
end;

function TdxPDFViewerBookmarkTreeView.GetSelectedOutline: TdxPDFOutlineTreeItem;
begin
  if TreeView.Selected <> nil then
    Result := TdxPDFOutlineTreeItem(TreeView.Selected.Data)
  else
    Result := nil;
end;

function TdxPDFViewerBookmarkTreeView.GetTreeView: TTree;
begin
  Result := FInternalControl as TTree;
end;

function TdxPDFViewerBookmarkTreeView.GetPrintPageNumbers(APrintSections: Boolean): TIntegerDynArray;

  function GetSelectedOutlines: TList<TdxPDFOutlineTreeItem>;
  var
    I: Integer;
    ANode: TTreeNode;
  begin
    Result := TList<TdxPDFOutlineTreeItem>.Create;
    for I := 0 to TreeView.SelectionCount - 1 do
    begin
      ANode := TreeView.Selections[I];
      if ANode.Data <> nil then
        Result.Add(TdxPDFOutlineTreeItem(ANode.Data));
    end;
  end;

var
  ASelectedOutlines: TList<TdxPDFOutlineTreeItem>;
begin
  ASelectedOutlines := GetSelectedOutlines;
  try
    Result := TdxPDFOutlineTreeAccess(OutlineTree).GetPrintPageNumbers(ASelectedOutlines, APrintSections);
  finally
    ASelectedOutlines.Free;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.OnChangeHandler(Sender: TObject; Node: TTreeNode);
begin
  UpdateState;
end;

procedure TdxPDFViewerBookmarkTreeView.OnCustomDrawItemHandler(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  AOutline: TdxPDFOutlineTreeItemAccess;
begin
  if Node.Deleting then
    Exit;
  with Sender as TCustomTreeView do
  begin
    AOutline := TdxPDFOutlineTreeItemAccess(Node.Data);
    DefaultDraw := AOutline <> nil;
    if DefaultDraw then
    begin
      if AOutline.Color <> dxPDFOutlineTreeItemDefaultColor then
        Canvas.Font.Color := AOutline.Color
      else
        Canvas.Font.Color := TreeView.LookAndFeelPainter.DefaultContentTextColor;
      if AOutline.IsBold then
        Canvas.Font.Style := [fsBold];
      if AOutline.IsItalic then
        Canvas.Font.Style := [fsItalic];
      DefaultDraw := True;
    end;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.OnMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  AItem: TTreeNode;
begin
  if Button = mbLeft then
  begin
    AItem := TreeView.GetNodeAt(X, Y);
    if (AItem <> nil) and (AItem = TreeView.Selected) then
      GoToBookmark;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    GoToBookmark;
end;

{ TdxPDFViewerAttachmentFileList }

procedure TdxPDFViewerAttachmentFileList.ClearInternalControl;
begin
  View.Items.Clear;
end;

procedure TdxPDFViewerAttachmentFileList.CreateInternalControl;
begin
  FInternalControl := TcxListView.Create(Viewer);
  InitializeInternalControl;
end;

procedure TdxPDFViewerAttachmentFileList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FImages := TcxImageList.Create(nil);
end;

procedure TdxPDFViewerAttachmentFileList.DestroySubClasses;
begin
  FreeAndNil(FImages);
  inherited DestroySubClasses;
end;

function TdxPDFViewerAttachmentFileList.GetEmpty: Boolean;
begin
  Result := DocumentAttachments = nil;
  if not Result then
    Result := DocumentAttachments.Count = 0;
end;

procedure TdxPDFViewerAttachmentFileList.InitializeInternalControl;
var
  AColumn: TListColumn;
  AView: TcxListView;
begin
  inherited InitializeInternalControl;
  AView := View;
  AView.ReadOnly := True;
  AView.Style.BorderStyle := cbsNone;
  AView.Style.LookAndFeel.MasterLookAndFeel := Viewer.LookAndFeel;
  AView.StyleDisabled.BorderStyle := cbsNone;
  AView.StyleHot.BorderStyle := cbsNone;
  AView.StyleFocused.BorderStyle := cbsNone;
  AView.MultiSelect := False;
  AView.ShowHint := Viewer.NavigationPane.ShowHints;
  AView.ViewStyle := vsReport;
  AView.ShowColumnHeaders := False;

  AView.OnClick := OnClickHandler;
  AView.OnDblClick := OnDblClickHandler;
  AView.OnKeyDown := OnKeyDownHandler;
  AView.OnMouseMove := OnMouseMoveHandler;
  AView.OnContextPopup := OnContextPopupHandler;
  AView.OnMouseLeave := OnMouseLeaveHandler;

  AColumn := AView.Columns.Add;
  AColumn.Caption := 'Name';
  AColumn.AutoSize := True;
end;

procedure TdxPDFViewerAttachmentFileList.PopulateInternalControl;

  function TryGetFileIcon(const AExtension: string; out AIcon: TIcon): Boolean;
  const
    Flags = SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON or SHGFI_ICON;
  var
    AFileInfo: TSHFileInfo;
  begin
    AIcon := nil;
    Result := SHGetFileInfo(PChar(AExtension), FILE_ATTRIBUTE_NORMAL, AFileInfo, SizeOf(AFileInfo), Flags) <> 0;
    if Result then
    begin
      AIcon := TIcon.Create;
      AIcon.Handle := AFileInfo.hIcon;
    end;
  end;

var
  I, AImageIndex: Integer;
  AAttachment: TdxPDFFileAttachment;
  AIcon: TIcon;
begin
  if not Empty then
  begin
    View.SmallImages := FImages;
    FImages.Clear;
    for I := 0 to DocumentAttachments.Count - 1 do
    begin
      AImageIndex := -1;
      AAttachment := DocumentAttachments[I];
      if TryGetFileIcon(TPath.GetExtension(AAttachment.FileName), AIcon) then
      begin
        FImages.AddIcon(AIcon);
        AImageIndex := FImages.Count - 1;
        AIcon.Free;
      end;
      View.AddItem(AAttachment.FileName, AAttachment);
      View.Items[I].ImageIndex := AImageIndex;
    end;
  end;
  UpdateState;
end;

procedure TdxPDFViewerAttachmentFileList.UpdateInternalControlTextSize;
begin
// do nothing
end;

function TdxPDFViewerAttachmentFileList.GetDocumentAttachments: TdxPDFFileAttachmentList;
begin
  if Viewer.IsDocumentLoaded then
    Result := Viewer.Document.FileAttachments
  else
    Result := nil;
end;

function TdxPDFViewerAttachmentFileList.GetView: TcxListView;
begin
  Result := FInternalControl as TcxListView;
end;

function TdxPDFViewerAttachmentFileList.GetShowHint: Boolean;
begin
  Result := View.ShowHint;
end;

procedure TdxPDFViewerAttachmentFileList.SetShowHint(const AValue: Boolean);
begin
  View.ShowHint := AValue;
end;

procedure TdxPDFViewerAttachmentFileList.OpenAttachment;
begin
  Viewer.Attachments.OpenAttachment;
end;

procedure TdxPDFViewerAttachmentFileList.OnClickHandler(Sender: TObject);
begin
  UpdateState;
end;

procedure TdxPDFViewerAttachmentFileList.OnContextPopupHandler(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  APopupMenu: TdxPDFViewerNavigationPaneAttachmentPopupMenu;
begin
  if not Handled then
  begin
    Handled := True;
    if View.Selected <> nil then
    begin
      APopupMenu := TdxPDFViewerNavigationPaneAttachmentPopupMenu.Create(Viewer);
      try
        Handled := TdxPDFViewerPopupMenuAccess(APopupMenu).Popup(View.ClientToScreen(MousePos));
      finally
        FreeAndNil(APopupMenu);
      end;
    end;
  end;
end;

procedure TdxPDFViewerAttachmentFileList.OnDblClickHandler(Sender: TObject);
begin
  if View.ItemIndex <> -1 then
    OpenAttachment;
end;

procedure TdxPDFViewerAttachmentFileList.OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_SPACE] then
    OpenAttachment;
end;

procedure TdxPDFViewerAttachmentFileList.OnMouseLeaveHandler(Sender: TObject);
begin
  Viewer.SetFocus;
end;

procedure TdxPDFViewerAttachmentFileList.OnMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  function GetCurrentItemIndex(const P: TPoint): Integer;
  var
    AItem: TListItem;
  begin
    AItem := View.GetItemAt(P.X, P.Y);
    if AItem <> nil then
      Result := AItem.Index
    else
      Result := -1;
  end;

  function NeedCancelPrevHint(ACurrentIndex: Integer): Boolean;
  begin
    Result := FPrevItemIndex <> ACurrentIndex;
  end;

  function GetItemHint(AIndex: Integer): string;
  var
    AAttachment: TdxPDFFileAttachmentAccess;
    S: string;
  begin
    AAttachment := TdxPDFFileAttachmentAccess(DocumentAttachments[AIndex]);
    if AAttachment.FileName <> '' then
      Result := cxGetResourceString(@sdxPDFViewerNavigationPageAttachmentFileNameCaption) + AAttachment.FileName + dxCRLF;
    if AAttachment.Description <> '' then
      Result := Result + cxGetResourceString(@sdxPDFViewerNavigationPageAttachmentDescriptionCaption) +
        AAttachment.Description + dxCRLF;
    S := AAttachment.GetModificationDateAsString;
    if S <> '' then
      Result := Result + cxGetResourceString(@sdxPDFViewerNavigationPageAttachmentModifiedCaption) + S + dxCRLF;
    S := AAttachment.GetSizeAsString;
    if S <> '' then
      Result := Result + cxGetResourceString(@sdxPDFViewerNavigationPageAttachmentFileSizeCaption) + S + dxCRLF;
    if Length(Result) > 0 then
      Result := Copy(Result, 1, Length(Result) - Length(dxCRLF))
  end;

var
  ACurrentItemIndex: Integer;
begin
  ACurrentItemIndex := GetCurrentItemIndex(cxPoint(X, Y));
  if NeedCancelPrevHint(ACurrentItemIndex) then
    Application.CancelHint;
  FPrevItemIndex := ACurrentItemIndex;
  if InRange(ACurrentItemIndex, 0, View.Items.Count - 1) then
    View.Hint := GetItemHint(ACurrentItemIndex)
  else
    View.Hint := '';
end;

{ TdxPDFViewerBookmarkTreeView.TTree }

constructor TdxPDFViewerBookmarkTreeView.TTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style.LookAndFeel := Viewer.LookAndFeel;
  Style.BorderStyle := cbsNone;
  StyleDisabled.BorderStyle := cbsNone;
  StyleFocused.BorderStyle := cbsNone;
  StyleHot.BorderStyle := cbsNone;
  HideSelection := False;
  MultiSelect := True;
  Viewer.AddFontListener(Self);
end;

destructor TdxPDFViewerBookmarkTreeView.TTree.Destroy;
begin
  Viewer.RemoveFontListener(Self);
  inherited Destroy;
end;

function TdxPDFViewerBookmarkTreeView.TTree.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if not Viewer.HitTest.HitAtDocumentViewer then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := Viewer.ProcessMouseWheelMessage(WheelDelta);
end;

procedure TdxPDFViewerBookmarkTreeView.TTree.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
  APopupMenu: TdxPDFViewerBookmarkPopupMenu;
begin
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
  begin
    Handled := True;
    P := GetMouseCursorClientPos;
    if (Selected <> nil) and PtInRect(Selected.DisplayRect(True), P) then
    begin
      APopupMenu := TdxPDFViewerBookmarkPopupMenu.Create(Owner);
      try
        Handled := TdxPDFViewerPopupMenuAccess(APopupMenu).Popup(ClientToScreen(P));
      finally
        FreeAndNil(APopupMenu);
      end;
    end;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.TTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('F')) then
    Viewer.ShowFindPanel
  else
    inherited KeyDown(Key, Shift);
end;

procedure TdxPDFViewerBookmarkTreeView.TTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: TTreeNode;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
  begin
    AItem := GetNodeAt(X, Y);
    if AItem <> nil then
      AItem.Selected := True;
  end;
end;

procedure TdxPDFViewerBookmarkTreeView.TTree.SetTextSize(ATextSize: TdxPDFViewerBookmarksTextSize);
const
  FontSizeDelta: array[TdxPDFViewerBookmarksTextSize] of Integer = (0, 2, 4);
begin
  Style.Font.Size := FFontSize + FontSizeDelta[ATextSize];
end;

function TdxPDFViewerBookmarkTreeView.TTree.GetViewer: TdxPDFCustomViewer;
begin
  Result := Owner as TdxPDFCustomViewer;
end;

procedure TdxPDFViewerBookmarkTreeView.TTree.Changed(Sender: TObject; AFont: TFont);
begin
  Style.Font.Assign(AFont);
  FFontSize := Style.Font.Size;
end;

{ TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo }

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.CanFocus: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.IsEnabled: Boolean;
begin
  Result := not Viewer.TextSearch.IsLocked;
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.GetColorizeGlyph: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.NavigationPane.MenuButtonGlyph;
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.GetHint: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageOptionsButtonHint);
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.GetPopupMenuClass: TComponentClass;
begin
  if Assigned(OnGetPopupMenuClass) then
    Result := OnGetPopupMenuClass
  else
    Result := nil;
end;

function TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.MeasureHeight: Integer;
begin
  Result := ApplyScaleFactor(dxPDFViewerNavigationPageToolbarButtonHeight);
end;

procedure TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  if AState in [cxbsHot, cxbsPressed] then
    Painter.DrawButtonBackground(ACanvas, ARect, AState);
end;

{ TdxPDFViewerNavigationPanePageToolBarViewInfo }

function TdxPDFViewerNavigationPanePageToolBarViewInfo.GetContentMargins: TRect;
begin
  Result := Painter.NavigationPanePageToolbarContentOffsets;
end;

function TdxPDFViewerNavigationPanePageToolBarViewInfo.MeasureWidth: Integer;
begin
  Result := ApplyScaleFactor(300);
end;

function TdxPDFViewerNavigationPanePageToolBarViewInfo.HasOptionsButton: Boolean;
begin
  Result := True;
end;

procedure TdxPDFViewerNavigationPanePageToolBarViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  FActualContentBounds := GetContentBounds;
  if HasOptionsButton then
    AlignToLeftSide(FOptionsButton, FActualContentBounds);
end;

procedure TdxPDFViewerNavigationPanePageToolBarViewInfo.CreateCells;
begin
  inherited CreateCells;
  if HasOptionsButton then
  begin
    FOptionsButton := AddCell(TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo) as
      TdxPDFViewerNavigationPanePageToolBarOptionsButtonViewInfo;
    FOptionsButton.OnGetPopupMenuClass := GetPopupMenuClass;
  end;
end;

procedure TdxPDFViewerNavigationPanePageToolBarViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPanePageToolbarBackground(ACanvas, Bounds);
end;

function TdxPDFViewerNavigationPanePageToolBarViewInfo.GetPopupMenuClass: TComponentClass;
begin
  Result := nil;
end;

{ TdxPDFViewerNavigationPanePageViewInfo }

constructor TdxPDFViewerNavigationPanePageViewInfo.Create(AController: TdxPDFViewerContainerController;
  APage: TdxPDFViewerNavigationPanePage);
begin
  inherited Create(AController);
  FPage := APage;
end;

function TdxPDFViewerNavigationPanePageViewInfo.CalculateHitTest(AHitTest: TdxPDFViewerCellHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if not FActualContentBounds.IsEmpty then
    AHitTest.HitCodes[hcNavigationPaneSplitter] := not cxRectPtIn(FActualContentBounds, AHitTest.HitPoint);
end;

function TdxPDFViewerNavigationPanePageViewInfo.MeasureWidth: Integer;
begin
  Result := Viewer.NavigationPane.PageSize;
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.Calculate;
begin
  FActualContentBounds := cxNullRect;
  inherited Calculate;
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.CreateCells;
begin
  inherited CreateCells;
  FCaption := AddCell(THeader) as THeader;
  FCaption.OnGetText := OnGetHeaderTextHandler;
  FToolBar := AddCell(GetToolbarClass) as TdxPDFViewerNavigationPanePageToolBarViewInfo;
  FButton := TdxPDFViewerNavigationPanePageButtonViewInfo.Create(Controller, Self);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.DoCalculate;
var
  AContentBounds: TRect;
begin
  inherited DoCalculate;
  AContentBounds := Bounds;

  AContentBounds.Top := Painter.LookAndFeelPainter.PDFViewerNavigationPaneButtonRect(
    Viewer.ViewInfo.NavigationPane.FirstPageButton.Bounds, cxbsPressed, True, ScaleFactor).Top;
  AlignToTopClientSide(FCaption, AContentBounds);
  FCaption.Bounds := cxRect(FCaption.Bounds.Left, Bounds.Top, FCaption.Bounds.Right, FCaption.Bounds.Bottom);

  FActualContentBounds := AContentBounds;

  FActualContentBounds := cxRectContent(FActualContentBounds, cxRect(Painter.NavigationPanePageContentOffsets.Left, 0,
    Painter.NavigationPanePageContentOffsets.Right, Painter.NavigationPanePageContentOffsets.Bottom));
  AlignToTopClientSide(FToolBar, FActualContentBounds);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPanePageBackground(ACanvas, Bounds);
end;

function TdxPDFViewerNavigationPanePageViewInfo.GetToolbarClass: TdxPDFViewerCellViewInfoClass;
begin
  Result := TdxPDFViewerNavigationPanePageToolBarViewInfo;
end;

function TdxPDFViewerNavigationPanePageViewInfo.CanShow: Boolean;
begin
  Result := FPage.CanShow;
end;

function TdxPDFViewerNavigationPanePageViewInfo.OnGetHeaderTextHandler: string;
begin
  Result := FPage.Caption;
end;

function TdxPDFViewerNavigationPanePageViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := FPage.Glyph;
end;

{ TdxPDFViewerNavigationPanePageViewInfo.TCaptionText }

function TdxPDFViewerNavigationPanePageViewInfo.TCaptionText.GetFont: TFont;
begin
  Result := Viewer.NavigationPane.Font;
end;

function TdxPDFViewerNavigationPanePageViewInfo.TCaptionText.GetPainterTextColor: TColor;
begin
  Result := Painter.TitleTextColor;
end;

function TdxPDFViewerNavigationPanePageViewInfo.TCaptionText.GetText: string;
begin
  if Assigned(OnGetText) then
    Result := OnGetText
  else
    Result := '';
end;

function TdxPDFViewerNavigationPanePageViewInfo.TCaptionText.GetTextAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.TCaptionText.PrepareCanvas(ACanvas: TcxCanvas);
begin
  inherited PrepareCanvas(ACanvas);
  ACanvas.Font.Color := Painter.NavigationPanePageCaptionTextColor;
  ACanvas.Font.Style := [fsBold];
end;

{ TdxPDFViewerNavigationPanePageCaptionButton }

function TdxPDFViewerNavigationPanePageCaptionButton.CanFocus: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerNavigationPanePageCaptionButton.GetGlyphAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TdxPDFViewerNavigationPanePageCaptionButton.MeasureHeight: Integer;
begin
  Result := ApplyScaleFactor(dxPDFViewerNavigationPageToolbarButtonHeight);
end;

function TdxPDFViewerNavigationPanePageCaptionButton.MeasureWidth: Integer;
begin
  Result := MeasureHeight;
end;

procedure TdxPDFViewerNavigationPanePageCaptionButton.DrawButtonContent(ACanvas: TcxCanvas);
begin
  Painter.DrawButtonGlyph(ACanvas, GetGlyph, Bounds, State, GetColorizeGlyph);
end;

procedure TdxPDFViewerNavigationPanePageCaptionButton.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  Painter.DrawNavigationPanePageButton(ACanvas, Bounds, State);
end;

function TdxPDFViewerNavigationPanePageCaptionButton.GetColorizeGlyph: Boolean;
begin
  Result := True;
end;

{ TdxPDFViewerNavigationPanePageButtonViewInfo }

constructor TdxPDFViewerNavigationPanePageButtonViewInfo.Create(AController: TdxPDFViewerContainerController;
  APage: TdxPDFViewerNavigationPanePageViewInfo);
begin
  inherited Create(AController);
  FPage := APage;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.CalculateState: TcxButtonState;
begin
  Result := inherited CalculateState;
  if not cxRectIsEmpty(FPage.Bounds) and IsActive then
    Result := cxbsPressed;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.CanFocus: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.IsEnabled: Boolean;
begin
  Result := FPage.CanShow;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.GetClipRect: TRect;
begin
  if IsActive then
    Result := Viewer.ViewInfo.NavigationPane.Bounds
  else
    Result := Bounds;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := FPage.GetGlyph;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.GetGlyphAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.MeasureHeight: Integer;
begin
  Result := Max(GlyphSize.cy + cxMarginsHeight(Painter.NavigationPaneButtonContentOffsets),
    Min(Painter.NavigationPaneButtonSize.cy, Painter.NavigationPaneButtonSize.cy));
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.MeasureWidth: Integer;
begin
  Result := Max(GlyphSize.cx + cxMarginsWidth(Painter.NavigationPaneButtonContentOffsets),
    Min(Painter.NavigationPaneButtonSize.cx, Painter.NavigationPaneButtonSize.cy));
end;

procedure TdxPDFViewerNavigationPanePageButtonViewInfo.DoExecute;
begin
  Viewer.NavigationPane.ActivePage := FPage.Page;
end;

procedure TdxPDFViewerNavigationPanePageButtonViewInfo.DrawButtonBackground(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState);
begin
// do nothing
end;

procedure TdxPDFViewerNavigationPanePageButtonViewInfo.DrawButtonContent(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPaneButton(ACanvas, Bounds, State, Viewer.OptionsNavigationPane.ActivePageState = wsMinimized,
    IsActive, IsFirst);
  Painter.DrawNavigationPaneButtonGlyph(ACanvas, GetGlyph, GlyphRect, State);
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.IsActive: Boolean;
var
  AActivePage: TdxPDFViewerNavigationPanePageViewInfo;
begin
  Result := (Viewer.ViewInfo <> nil) and (Viewer.ViewInfo.NavigationPane <> nil);
  if Result then
  begin
    AActivePage := Viewer.ViewInfo.NavigationPane.ActivePage;
    Result := (AActivePage <> nil) and (Viewer.OptionsNavigationPane.ActivePageState <> wsMinimized) and
      (AActivePage.Button = Self);
  end;
end;

function TdxPDFViewerNavigationPanePageButtonViewInfo.IsFirst: Boolean;
begin
  Result := FPage.Page.IsFirst;
end;

{ TdxPDFViewerNavigationPanePageViewInfo.TMinimizeButton }

function TdxPDFViewerNavigationPanePageViewInfo.TMinimizeButton.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.NavigationPane.MinimizeButtonGlyph;
end;

function TdxPDFViewerNavigationPanePageViewInfo.TMinimizeButton.GetHint: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageHideButtonHint);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.TMinimizeButton.DoExecute;
begin
  Viewer.NavigationPane.MinimizePage;
end;

{ TdxPDFViewerNavigationPanePageViewInfo.TMaximizeButton }

function TdxPDFViewerNavigationPanePageViewInfo.TMaximizeButton.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.NavigationPane.MaximizeButtonGlyph;
end;

function TdxPDFViewerNavigationPanePageViewInfo.TMaximizeButton.GetHint: string;
begin
  if Viewer.NavigationPane.IsMaximized then
    Result := cxGetResourceString(@sdxPDFViewerNavigationPageCollapseButtonHint)
  else
    Result := cxGetResourceString(@sdxPDFViewerNavigationPageExpandButtonHint);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.TMaximizeButton.DoExecute;
begin
  Viewer.NavigationPane.MaximizePage;
end;

{ TdxPDFViewerNavigationPanePageViewInfo.THeader }

function TdxPDFViewerNavigationPanePageViewInfo.THeader.GetContentMargins: TRect;
begin
  Result := Painter.NavigationPanePageCaptionContentOffsets;
end;

function TdxPDFViewerNavigationPanePageViewInfo.THeader.GetOnGetText: THeaderOnGetTextEvent;
begin
  Result := FCaption.OnGetText;
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.THeader.SetOnGetText(const AValue: THeaderOnGetTextEvent);
begin
  FCaption.OnGetText := AValue;
end;

function TdxPDFViewerNavigationPanePageViewInfo.THeader.AlignToClient(AViewInfo: TdxPDFViewerCellViewInfo;
  var R: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := AViewInfo <> nil;
  if Result then
  begin
    ARect := R;
    AViewInfo.Bounds := cxRectCenterVertically(ARect, AViewInfo.MeasureHeight);
    R.Left := AViewInfo.Bounds.Right;
    Inc(R.Left, IndentBetweenElements);
  end;
end;

function TdxPDFViewerNavigationPanePageViewInfo.THeader.MeasureHeight: Integer;
begin
  Result := Viewer.ViewInfo.NavigationPane.FirstPageButton.Bounds.Height + Painter.NavigationPaneButtonOverlay.Y;
end;

function TdxPDFViewerNavigationPanePageViewInfo.THeader.MeasureWidth: Integer;
begin
  Result := ApplyScaleFactor(300);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.THeader.CreateCells;
begin
  inherited CreateCells;
  FCaption := AddCell(TCaptionText) as TCaptionText;
  FMinimizeButton := AddCell(TMinimizeButton) as TMinimizeButton;
  FMaximizeButton := AddCell(TMaximizeButton) as TMaximizeButton;
  FCaption.OnGetText := OnGetText;
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.THeader.DoCalculate;
var
  AContentBounds: TRect;
begin
  inherited DoCalculate;
  AContentBounds := GetContentBounds;
  AlignToRightSide(FMaximizeButton, AContentBounds);
  AlignToRightSide(FMinimizeButton, AContentBounds);
  AlignToClient(FCaption, AContentBounds);
end;

procedure TdxPDFViewerNavigationPanePageViewInfo.THeader.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPanePageCaptionBackground(ACanvas, Bounds);
end;

{ TdxPDFViewerBookmarksPageViewInfo }

function TdxPDFViewerBookmarksPageViewInfo.GetOutlineTreeView: TdxPDFViewerBookmarkTreeView;
begin
  Result := Viewer.Bookmarks.Tree;
end;

function TdxPDFViewerBookmarksPageViewInfo.GetToolbarClass: TdxPDFViewerCellViewInfoClass;
begin
  Result := TBookmarksToolBar;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.Calculate;
begin
  inherited Calculate;
  Tree.Bounds := FActualContentBounds;
  Tree.UpdateTextSize;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.CreateCells;
begin
  inherited CreateCells;
  FToolBar := AddCell(TBookmarksToolBar) as TBookmarksToolBar;
end;

{ TdxPDFViewerThumbnailsPageViewInfo }

function TdxPDFViewerThumbnailsPageViewInfo.GetPreview: TdxPDFViewerPageThumbnailPreview;
begin
  Result := Viewer.Thumbnails.ThumbnailPreview;
end;

function TdxPDFViewerThumbnailsPageViewInfo.GetZoomTrackBar: TcxTrackBar;
begin
  Result := Viewer.Thumbnails.SizeTrackBar;
end;

function TdxPDFViewerThumbnailsPageViewInfo.GetToolbarClass: TdxPDFViewerCellViewInfoClass;
begin
  Result := TThumbnailsToolBar;
end;

procedure TdxPDFViewerThumbnailsPageViewInfo.Calculate;

  procedure CalculateZoomTrackBarBounds;
  var
    ABounds, AContentBounds: TRect;
  begin
    AContentBounds := cxRectSetRight(FToolBar.ActualContentBounds, FToolBar.ActualContentBounds.Right, 196);
    ABounds := cxRectCenterVertically(AContentBounds, AContentBounds.Height);
    if FToolBar.ActualContentBounds.Width < ABounds.Width then
      ABounds := cxNullRect;
    if not cxRectIsEqual(Viewer.Thumbnails.SizeTrackBar.BoundsRect, ABounds) then
    begin
      Viewer.Thumbnails.SizeTrackBar.BoundsRect := ABounds;
      Viewer.Thumbnails.SizeTrackBar.Visible := not cxRectIsEmpty(Viewer.Thumbnails.SizeTrackBar.BoundsRect);
    end;
  end;

begin
  inherited Calculate;
  Preview.Bounds := FActualContentBounds;
  CalculateZoomTrackBarBounds;
end;

{ TdxPDFViewerThumbnailsPageViewInfo.TThumbnailsToolBar }

function TdxPDFViewerThumbnailsPageViewInfo.TThumbnailsToolBar.GetPopupMenuClass: TComponentClass;
begin
  Result := TdxPDFViewerThumbnailsPopupMenu;
end;

{ TdxPDFViewerAttachmentsPageViewInfo }

function TdxPDFViewerAttachmentsPageViewInfo.GetFileList: TdxPDFViewerAttachmentFileList;
begin
  Result := Viewer.Attachments.FileList;
end;

function TdxPDFViewerAttachmentsPageViewInfo.GetToolbarClass: TdxPDFViewerCellViewInfoClass;
begin
  Result := TAttachmentsToolBar;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.Calculate;
begin
  inherited Calculate;
  FileList.Bounds := FActualContentBounds;
end;

{ TdxPDFViewerAttachmentsPageViewInfo.TAttachmentsToolBar }

function TdxPDFViewerAttachmentsPageViewInfo.TAttachmentsToolBar.GetPopupMenuClass: TComponentClass;
begin
  Result := nil;
end;

function TdxPDFViewerAttachmentsPageViewInfo.TAttachmentsToolBar.HasOptionsButton: Boolean;
begin
  Result := False;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TAttachmentsToolBar.CreateCells;
begin
  inherited CreateCells;
  FOpenButton := AddCell(TOpenButton) as TOpenButton;
  FSaveButton := AddCell(TSaveButton) as TSaveButton;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TAttachmentsToolBar.DoCalculate;
var
  AContentBounds: TRect;
begin
  inherited DoCalculate;
  AContentBounds := ActualContentBounds;
  AlignToLeftSide(FOpenButton, AContentBounds);
  AlignToLeftSide(FSaveButton, AContentBounds);
end;

{ TdxPDFViewerAttachmentsPageViewInfo.TOpenButton }

function TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.GetColorizeGlyph: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.Attachments.OpenAttachmentGlyph;
end;

function TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.GetHint: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageOpenAttachmentButtonHint);
end;

function TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.IsEnabled: Boolean;
begin
  Result := Viewer.Attachments.HasAttachments;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.DoExecute;
begin
  Viewer.Attachments.OpenAttachment;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TOpenButton.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  if AState in [cxbsHot, cxbsPressed] then
    Painter.DrawButtonBackground(ACanvas, ARect, AState);
end;

{ TdxPDFViewerAttachmentsPageViewInfo.TSaveButton }

function TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.GetColorizeGlyph: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.Attachments.SaveAttachmentGlyph;
end;

function TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.GetHint: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageSaveAttachmentButtonHint);
end;

function TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.IsEnabled: Boolean;
begin
  Result := Viewer.Attachments.HasAttachments;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.DoExecute;
begin
  Viewer.Attachments.SaveAttachment;
end;

procedure TdxPDFViewerAttachmentsPageViewInfo.TSaveButton.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  if AState in [cxbsHot, cxbsPressed] then
    Painter.DrawButtonBackground(ACanvas, ARect, AState);
end;

{ TdxPDFViewerPageThumbnailPreview }

function TdxPDFViewerPageThumbnailPreview.Locked: Boolean;
begin
  Result := FLockCount <> 0;
end;

procedure TdxPDFViewerPageThumbnailPreview.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxPDFViewerPageThumbnailPreview.EndUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxPDFViewerPageThumbnailPreview.PrintSelectedPages;
begin
  if Viewer.CanPrint then
    dxPrintingRepository.PrintReport(Viewer, Preview.GetPagesToPrint)
end;

function TdxPDFViewerPageThumbnailPreview.GetEmpty: Boolean;
begin
  Result := False;
end;

procedure TdxPDFViewerPageThumbnailPreview.ClearInternalControl;
begin
  Preview.Clear;
end;

procedure TdxPDFViewerPageThumbnailPreview.CreateInternalControl;
begin
  FInternalControl := TdxPDFDocumentPageThumbnailViewer.Create(Viewer);
  InitializeInternalControl;
end;

procedure TdxPDFViewerPageThumbnailPreview.DestroySubClasses;
begin
  Viewer.RemoveFontListener(Preview);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerPageThumbnailPreview.InitializeInternalControl;
begin
  inherited InitializeInternalControl;
  LookAndFeel.MasterLookAndFeel := Viewer.LookAndFeel;
  Preview.BeginUpdate;
  try
    Preview.BorderStyle := cxcbsNone;
    Preview.OnSelectedPageChanged := OnSelectedPageChangedHandler;
  finally
    Preview.EndUpdate;
  end;
  Viewer.AddFontListener(Preview);
end;

procedure TdxPDFViewerPageThumbnailPreview.PopulateInternalControl;
begin
  Preview.Document := Viewer.Document as TdxPDFViewerDocument;
end;

procedure TdxPDFViewerPageThumbnailPreview.UpdateInternalControlTextSize;
begin
// do nothing
end;

function TdxPDFViewerPageThumbnailPreview.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := Preview.LookAndFeel;
end;

function TdxPDFViewerPageThumbnailPreview.GetMaxSize: Integer;
begin
  Result := Preview.MaxSize;
end;

function TdxPDFViewerPageThumbnailPreview.GetMinSize: Integer;
begin
  Result := Preview.MinSize;
end;

function TdxPDFViewerPageThumbnailPreview.GetOnCustomDrawPreRenderPage: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent;
begin
  Result := Preview.OnCustomDrawPreRenderPage;
end;

function TdxPDFViewerPageThumbnailPreview.GetOnSizeChanged: TNotifyEvent;
begin
  Result := Preview.OnThumbnailSizeChanged;
end;

function TdxPDFViewerPageThumbnailPreview.GetPreview: TThumbnailPreviewAccess;
begin
  Result := TThumbnailPreviewAccess(FInternalControl as TdxPDFDocumentPageThumbnailViewer);
end;

function TdxPDFViewerPageThumbnailPreview.GetSelectedPageIndex: Integer;
begin
  Result := Preview.SelPageIndex;
end;

function TdxPDFViewerPageThumbnailPreview.GetSize: Integer;
begin
  Result := Preview.Size;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetMaxSize(const AValue: Integer);
begin
  Preview.MaxSize := AValue;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetMinSize(const AValue: Integer);
begin
  Preview.MinSize := AValue;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetOnCustomDrawPreRenderPage(
  const AValue: TdxPDFDocumentViewerOnCustomDrawPreRenderPageEvent);
begin
  Preview.OnCustomDrawPreRenderPage := AValue;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetOnSizeChanged(const AValue: TNotifyEvent);
begin
  Preview.OnThumbnailSizeChanged := AValue;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetSelectedPageIndex(const AValue: Integer);
begin
  if not Locked then
  begin
    BeginUpdate;
    try
      Preview.SelPageIndex := AValue;
      if not Preview.Focused and Viewer.CanFocus then
        Viewer.SetFocus;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxPDFViewerPageThumbnailPreview.SetSize(const AValue: Integer);
begin
  Preview.Size := AValue;
end;

procedure TdxPDFViewerPageThumbnailPreview.OnSelectedPageChangedHandler(Sender: TObject; APageIndex: Integer);
begin
  if not Locked then
  begin
    BeginUpdate;
    try
      Viewer.CurrentPageIndex := APageIndex;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton }

function TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.GetColorizeGlyph: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.GetGlyph: TdxSmartGlyph;
begin
  Result := Viewer.Bookmarks.ExpandBookmarkGlyph;
end;

function TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.GetHint: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageExpandBookmarkButtonHint);
end;

function TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.IsEnabled: Boolean;
begin
  Result := Viewer.Bookmarks.CanExpandCurrentBookmark;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.DoExecute;
begin
  Viewer.Bookmarks.ExpandCurrentBookmark;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.TExpandBookmarkButton.DrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);
begin
  if AState in [cxbsHot, cxbsPressed] then
    Painter.DrawButtonBackground(ACanvas, ARect, AState);
end;

{ TdxPDFViewerBookmarksPageViewInfo.TBookmarksToolBar }

function TdxPDFViewerBookmarksPageViewInfo.TBookmarksToolBar.GetPopupMenuClass: TComponentClass;
begin
  Result := TdxPDFViewerBookmarksPageOptionsPopupMenu;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.TBookmarksToolBar.CreateCells;
begin
  inherited CreateCells;
  FExpandButton := AddCell(TExpandBookmarkButton) as  TExpandBookmarkButton;
end;

procedure TdxPDFViewerBookmarksPageViewInfo.TBookmarksToolBar.DoCalculate;
var
  AContentBounds: TRect;
begin
  inherited DoCalculate;
  AContentBounds := GetContentBounds;
  AlignToRightSide(FExpandButton, AContentBounds);
end;

{ TdxPDFViewerNavigationPaneViewInfo }

constructor TdxPDFViewerNavigationPaneViewInfo.Create(AController: TdxPDFViewerContainerController;
  ANavigationPane: TdxPDFViewerNavigationPane);
begin
  FNavigationPane := ANavigationPane;
  inherited Create(AController);
end;

function TdxPDFViewerNavigationPaneViewInfo.GetContentMargins: TRect;
begin
  Result := Painter.NavigationPaneContentOffsets;
end;

function TdxPDFViewerNavigationPaneViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := 0;
end;

function TdxPDFViewerNavigationPaneViewInfo.MeasureWidth: Integer;
begin
  Result := GetButtonsWidth + GetPageWidth;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.ClearCells;
begin
  inherited ClearCells;
  FPages.Clear;
  FPageButtons.Clear;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.CreateSubClasses;
begin
  FPages := TList<TdxPDFViewerNavigationPanePageViewInfo>.Create;
  FPageButtons := TList<TdxPDFViewerNavigationPanePageButtonViewInfo>.Create;
  inherited CreateSubClasses;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.CreateCells;
var
  I: Integer;
  APage: TdxPDFViewerNavigationPanePage;
begin
  inherited CreateCells;
  for I := 0 to FNavigationPane.VisiblePages.Count - 1 do
  begin
    APage := FNavigationPane.VisiblePages[I];
    if APage.CanShow then
      AddPage(APage)
  end;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.DestroySubClasses;
begin
  inherited DestroySubClasses;
  FreeAndNil(FPageButtons);
  FreeAndNil(FPages);
end;

procedure TdxPDFViewerNavigationPaneViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  CalculateButtonsBounds;
  CalculatePagesBounds;
  CalculateSplitterBounds;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPaneBackground(ACanvas, FButtonsBounds);
end;

procedure TdxPDFViewerNavigationPaneViewInfo.DrawChildren(ACanvas: TcxCanvas);
var
  APage: TdxPDFViewerNavigationPanePageViewInfo;
begin
  for APage in FPages do
  begin
    APage.Draw(ACanvas);
    if APage <> ActivePage then
      APage.Button.Draw(ACanvas);
  end;
  if (ActivePage <> nil) and (ActivePage.Button <> nil) then
    ActivePage.Button.Draw(ACanvas, True);
end;

function TdxPDFViewerNavigationPaneViewInfo.GetPageIndex(AButton: TdxPDFViewerNavigationPanePageButtonViewInfo): Integer;
begin
  Result := FPageButtons.IndexOf(AButton);
end;

function TdxPDFViewerNavigationPaneViewInfo.GetActivePage: TdxPDFViewerNavigationPanePageViewInfo;
begin
  if Viewer.NavigationPane.ActivePage <> nil then
    Result := Viewer.NavigationPane.ActivePage.ViewInfo
  else
    Result := nil;
end;

function TdxPDFViewerNavigationPaneViewInfo.GetButtonsWidth: Integer;
begin
  Result := ButtonMaxWidth + cxMarginsWidth(ContentMargins);
end;

function TdxPDFViewerNavigationPaneViewInfo.GetFirstPageButton: TdxPDFViewerNavigationPanePageButtonViewInfo;
begin
  Result := FPages.First.Button;
end;

function TdxPDFViewerNavigationPaneViewInfo.GetPageWidth: Integer;
begin
  Result := 0;
  if Viewer.NavigationPane.CanShow then
    case Viewer.NavigationPane.ActivePageState of
      wsNormal:
        if ActivePage <> nil then
          Result := ActivePage.MeasureWidth;
      wsMaximized:
        Result := Viewer.ViewInfo.Bounds.Width;
    else
      Result := 0;
    end;
end;

function TdxPDFViewerNavigationPaneViewInfo.ButtonMaxWidth: Integer;
begin
  Result := FirstPageButton.MeasureWidth;
end;

procedure TdxPDFViewerNavigationPaneViewInfo.AddPage(APage: TdxPDFViewerNavigationPanePage);
var
  AViewInfo: TdxPDFViewerNavigationPanePageViewInfo;
begin
  AViewInfo := APage.CreateViewInfo;
  FPages.Add(AViewInfo);
  FPageButtons.Add(AViewInfo.Button);
  CellList.Add(AViewInfo);
  CellList.Add(AViewInfo.Button);
end;

procedure TdxPDFViewerNavigationPaneViewInfo.CalculateButtonsBounds;
var
  I: Integer;
  AContentBounds: TRect;
begin
  FButtonsBounds := cxRectSetWidth(Bounds, GetButtonsWidth);
  AContentBounds := cxRectContent(FButtonsBounds, ContentMargins);
  for I := 0 to FPages.Count - 1 do
    AlignToTopClientSide(FPages[I].Button, AContentBounds);
end;

procedure TdxPDFViewerNavigationPaneViewInfo.CalculatePagesBounds;
var
  AActivePage, APage: TdxPDFViewerNavigationPanePageViewInfo;
begin
  FPageBounds := cxNullRect;
  if GetPageWidth > 0 then
  begin
    FPageBounds := Bounds;
    FPageBounds.Left := FButtonsBounds.Right;
  end;
  AActivePage := ActivePage;
  for APage in FPages do
    if APage = AActivePage then
      APage.Bounds := FPageBounds
    else
      SetEmptyBounds(APage);
end;

procedure TdxPDFViewerNavigationPaneViewInfo.CalculateSplitterBounds;
const
  SplitterHotZone = 4;
begin
  if not cxRectIsEmpty(FPageBounds) and (Viewer.NavigationPane.ActivePageState = wsNormal) then
  begin
    FSplitterBounds := FPageBounds;
    FSplitterBounds.Left := FPageBounds.Right - ApplyScaleFactor(SplitterHotZone);
    FSplitterBounds.Right := FPageBounds.Right + ApplyScaleFactor(SplitterHotZone);
  end
  else
    FSplitterBounds := cxNullRect;
end;

{ TdxPDFViewerOptionsNavigationPage }

constructor TdxPDFViewerOptionsNavigationPage.Create(AOwner: TPersistent; APage: TdxPDFViewerNavigationPanePage);
begin
  inherited Create(AOwner);
  FPage := APage;
end;

procedure TdxPDFViewerOptionsNavigationPage.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);
  Visible := TdxPDFViewerOptionsNavigationPage(ASource).Visible;
end;

function TdxPDFViewerOptionsNavigationPage.GetGlyph: TdxSmartGlyph;
begin
  Result := FPage.Glyph;
end;

function TdxPDFViewerOptionsNavigationPage.GetVisible: TdxDefaultBoolean;
begin
  Result := FPage.Visible;
end;

procedure TdxPDFViewerOptionsNavigationPage.SetGlyph(const AValue: TdxSmartGlyph);
begin
  FPage.Glyph.Assign(AValue);
end;

procedure TdxPDFViewerOptionsNavigationPage.SetVisible(const AValue: TdxDefaultBoolean);
begin
  FPage.Visible := AValue;
end;

{ TdxPDFViewerOptionsBookmarks }

procedure TdxPDFViewerOptionsBookmarks.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);
  HideAfterUse := TdxPDFViewerOptionsBookmarks(ASource).HideAfterUse;
  TextSize := TdxPDFViewerOptionsBookmarks(ASource).TextSize;
end;

function TdxPDFViewerOptionsBookmarks.GetShowEmpty: Boolean;
begin
  Result := Visible = bTrue;
end;

procedure TdxPDFViewerOptionsBookmarks.SetHideAfterUse(const AValue: Boolean);
begin
  if FHideAfterUse <> AValue then
  begin
    FHideAfterUse := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerOptionsBookmarks.SetTextSize(const AValue: TdxPDFViewerBookmarksTextSize);
begin
  if FTextSize <> AValue then
  begin
    FTextSize := AValue;
    Changed;
  end;
end;

{ TdxPDFViewerNavigationPanePage }

procedure TdxPDFViewerNavigationPanePage.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FOptions := CreateOptions;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := OnGlyphChangeHandler;
  LoadDefaultGlyphs;
  FVisible := bDefault;
  FControl := GetControlClass.Create(Viewer);
end;

procedure TdxPDFViewerNavigationPanePage.DestroySubClasses;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FDefaultGlyph);
  FreeAndNil(FOptions);
  FreeAndNil(FControl);
  inherited DestroySubClasses;
end;

function TdxPDFViewerNavigationPanePage.CanShow: Boolean;
begin
  Result := Options.Visible in [bTrue];
end;

function TdxPDFViewerNavigationPanePage.CreateOptions: TdxPDFViewerOptionsNavigationPage;
begin
  Result := TdxPDFViewerOptionsNavigationPage.Create(Viewer, Self);
end;

procedure TdxPDFViewerNavigationPanePage.SetBounds(const AValue: TRect);
begin
  FControl.Bounds := AValue;
end;

function TdxPDFViewerNavigationPanePage.IsFirst: Boolean;
begin
  Result := Viewer.NavigationPane.IsFirst(Self);
end;

procedure TdxPDFViewerNavigationPanePage.Clear;
begin
  FControl.Clear;
end;

procedure TdxPDFViewerNavigationPanePage.Refresh;
begin
  FControl.Refresh;
end;

function TdxPDFViewerNavigationPanePage.GetBounds: TRect;
begin
  Result := FControl.Bounds;
end;

function TdxPDFViewerNavigationPanePage.GetEmpty: Boolean;
begin
  Result := FControl.Empty;
end;

function TdxPDFViewerNavigationPanePage.GetGlyph: TdxSmartGlyph;
begin
  if IsImageAssigned(FGlyph) then
    Result := FGlyph
  else
    Result := FDefaultGlyph;
end;

procedure TdxPDFViewerNavigationPanePage.SetGlyph(const AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxPDFViewerNavigationPanePage.SetOptions(const AValue: TdxPDFViewerOptionsNavigationPage);
begin
  FOptions.Assign(AValue);
end;

procedure TdxPDFViewerNavigationPanePage.SetVisible(const AValue: TdxDefaultBoolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Viewer.NavigationPane.VisibilityChanged(Self);
  end;
end;

procedure TdxPDFViewerNavigationPanePage.OnGlyphChangeHandler(Sender: TObject);
begin
  Viewer.NavigationPane.Changed;
end;

{ TdxPDFViewerThumbnails }

procedure TdxPDFViewerThumbnails.EnlargePageThumbnails;
begin
  FSizeTrackBar.Position := FSizeTrackBar.Position + dxPDFDocumentPageThumbnailViewerSizeStep;
end;

procedure TdxPDFViewerThumbnails.PrintPages;
begin
  ThumbnailPreview.PrintSelectedPages;
end;

procedure TdxPDFViewerThumbnails.ReducePageThumbnails;
begin
  FSizeTrackBar.Position := FSizeTrackBar.Position - dxPDFDocumentPageThumbnailViewerSizeStep;
end;

function TdxPDFViewerThumbnails.CanShow: Boolean;
begin
  Result := inherited CanShow or (Options.Visible = bDefault);
end;

function TdxPDFViewerThumbnails.CreateOptions: TdxPDFViewerOptionsNavigationPage;
begin
  Result := TdxPDFViewerOptionsThumbnails.Create(Viewer, Self);
end;

function TdxPDFViewerThumbnails.CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo;
begin
  FViewInfo := TdxPDFViewerThumbnailsPageViewInfo.Create(Viewer.NavigationPane.Controller, Self);
  Result := FViewInfo;
end;

function TdxPDFViewerThumbnails.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageThumbnailsCaption);
end;

function TdxPDFViewerThumbnails.GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass;
begin
  Result := TdxPDFViewerPageThumbnailPreview;
end;

procedure TdxPDFViewerThumbnails.CreateSubClasses;

  procedure InitializeThumbnailPreview;
  var
    AThumbnailPreview: TdxPDFViewerPageThumbnailPreview;
  begin
    AThumbnailPreview := ThumbnailPreview;
    ThumbnailPreview.BeginUpdate;
    try
      AThumbnailPreview.MaxSize := dxPDFDocumentPageThumbnailViewerMaxSize;
      AThumbnailPreview.MinSize := dxPDFDocumentPageThumbnailViewerMinSize;
      AThumbnailPreview.Size := AThumbnailPreview.Size;
      AThumbnailPreview.OnSizeChanged := OnThumbnailSizeChangedHandler;
    finally
      AThumbnailPreview.EndUpdate;
    end;
  end;

begin
  inherited CreateSubClasses;
  InitializeThumbnailPreview;
  CreateSizeTrackBar;
end;

procedure TdxPDFViewerThumbnails.DestroySubClasses;
begin
  FreeAndNil(FSizeTrackBar);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerThumbnails.LoadDefaultGlyphs;
begin
  FDefaultGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERTHUMBNAILSBUTTON', 'SVG');
end;

procedure TdxPDFViewerThumbnails.SetBounds(const AValue: TRect);
begin
  inherited SetBounds(AValue);
  SizeTrackBar.Visible := ThumbnailPreview.Visible;
end;

function TdxPDFViewerThumbnails.CanEnlargePageThumbnails: Boolean;
begin
  Result := FSizeTrackBar.Position < FSizeTrackBar.Properties.Max;
end;

function TdxPDFViewerThumbnails.CanReducePageThumbnails: Boolean;
begin
  Result := FSizeTrackBar.Position > FSizeTrackBar.Properties.Min;
end;

procedure TdxPDFViewerThumbnails.SynchronizeSelectedPage;
begin
  if FControl.Visible then
    ThumbnailPreview.SelectedPageIndex := Viewer.CurrentPageIndex;
end;

function TdxPDFViewerThumbnails.GetShowHints: Boolean;
begin
  Result := FSizeTrackBar.ShowHint;
end;

function TdxPDFViewerThumbnails.GetThumbnailPreview: TdxPDFViewerPageThumbnailPreview;
begin
  Result := FControl as TdxPDFViewerPageThumbnailPreview;
end;

procedure TdxPDFViewerThumbnails.SetShowHints(const AValue: Boolean);
begin
  FSizeTrackBar.ShowHint := AValue;
end;

procedure TdxPDFViewerThumbnails.CreateSizeTrackBar;
begin
  FSizeTrackBar := TcxTrackBar.Create(Viewer);
  FSizeTrackBar.Parent := Viewer;
  FSizeTrackBar.Visible := False;
  FSizeTrackBar.Hint := cxGetResourceString(@sdxPDFViewerNavigationPageThumbnailsSizeTrackBarHint);
  FSizeTrackBar.ShowHint := Viewer.OptionsBehavior.ShowHints;
  FSizeTrackBar.Style.BeginUpdate;
  try
    FSizeTrackBar.Style.LookAndFeel.MasterLookAndFeel := Viewer.LookAndFeel;
    FSizeTrackBar.Style.BorderStyle := ebsNone;
  finally
    FSizeTrackBar.Style.EndUpdate;
  end;
  try
    FSizeTrackBar.Properties.BeginUpdate;
    FSizeTrackBar.Properties.ShowChangeButtons := True;
    FSizeTrackBar.Properties.ShowTicks := False;
    FSizeTrackBar.Properties.ThumbStep := cxtsJump;
    FSizeTrackBar.Properties.Max := ThumbnailPreview.MaxSize;
    FSizeTrackBar.Properties.Min := ThumbnailPreview.MinSize;
    FSizeTrackBar.Properties.LineSize := dxPDFDocumentPageThumbnailViewerSizeStep;
  finally
    FSizeTrackBar.Properties.EndUpdate;
  end;
  FSizeTrackBar.Position := FSizeTrackBar.Properties.Min + 1;
  FSizeTrackBar.Properties.OnEditValueChanged := OnSizeChangedHandler;
  FSizeTrackBar.Position := FSizeTrackBar.Properties.Min;
  FSizeTrackBar.Transparent := True;
end;

procedure TdxPDFViewerThumbnails.OnSizeChangedHandler(Sender: TObject);
begin
  ThumbnailPreview.Size := FSizeTrackBar.Position;
end;

procedure TdxPDFViewerThumbnails.OnThumbnailSizeChangedHandler(Sender: TObject);
begin
  SizeTrackBar.Position := ThumbnailPreview.Size;
end;

{ TdxPDFViewerOptionsNavigationPane }

procedure TdxPDFViewerOptionsNavigationPane.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);
  ActivePageState := TdxPDFViewerOptionsNavigationPane(ASource).ActivePageState;
  Bookmarks.Assign(TdxPDFViewerOptionsNavigationPane(ASource).Bookmarks);
  Thumbnails.Assign(TdxPDFViewerOptionsNavigationPane(ASource).Thumbnails);
  Visible := TdxPDFViewerOptionsNavigationPane(ASource).Visible;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetActivePageState(const AValue: TWindowState);
begin
  Viewer.NavigationPane.ActivePageState := AValue;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetAttachments(const AValue: TdxPDFViewerOptionsAttachments);
begin
  Viewer.NavigationPane.Attachments.Options := AValue;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetBookmarks(const AValue: TdxPDFViewerOptionsBookmarks);
begin
  Viewer.NavigationPane.Bookmarks.Options := AValue;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetThumbnails(const AValue: TdxPDFViewerOptionsThumbnails);
begin
  Viewer.NavigationPane.Thumbnails.Options := AValue;
end;

function TdxPDFViewerOptionsNavigationPane.GetActivePage: TdxPDFViewerNavigationPaneActivePage;
begin
  Result := Viewer.NavigationPane.ActivePageType;
end;

function TdxPDFViewerOptionsNavigationPane.GetActivePageState: TWindowState;
begin
  Result := Viewer.NavigationPane.ActivePageState;
end;

function TdxPDFViewerOptionsNavigationPane.GetAttachments: TdxPDFViewerOptionsAttachments;
begin
  Result := Viewer.NavigationPane.Attachments.Options as TdxPDFViewerOptionsAttachments;
end;

function TdxPDFViewerOptionsNavigationPane.GetBookmarks: TdxPDFViewerOptionsBookmarks;
begin
  Result := Viewer.NavigationPane.Bookmarks.Options as TdxPDFViewerOptionsBookmarks;
end;

function TdxPDFViewerOptionsNavigationPane.GetThumbnails: TdxPDFViewerOptionsThumbnails;
begin
  Result := Viewer.NavigationPane.Thumbnails.Options as TdxPDFViewerOptionsThumbnails;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetActivePage(const AValue: TdxPDFViewerNavigationPaneActivePage);
begin
  Viewer.NavigationPane.ActivePageType := AValue;
end;

procedure TdxPDFViewerOptionsNavigationPane.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(ctLight);
  end;
end;

{ TdxPDFViewerBookmarks }

procedure TdxPDFViewerBookmarks.ExpandCollapseTopLevelBookmarks;
begin
  Tree.ExpandCollapseTopLevelBookmarks;
end;

procedure TdxPDFViewerBookmarks.ExpandCurrentBookmark;
begin
  Tree.ExpandCurrentBookmark;
end;

procedure TdxPDFViewerBookmarks.GoToBookmark;
begin
  Tree.GoToBookmark;
end;

procedure TdxPDFViewerBookmarks.PrintPages;
begin
  Tree.PrintSelectedPages(False);
end;

procedure TdxPDFViewerBookmarks.PrintSections;
begin
  Tree.PrintSelectedPages(True);
end;

function TdxPDFViewerBookmarks.CanShow: Boolean;
begin
  Result := inherited CanShow or ((Options.Visible = bDefault) and not Empty);
end;

function TdxPDFViewerBookmarks.CreateOptions: TdxPDFViewerOptionsNavigationPage;
begin
  Result := TdxPDFViewerOptionsBookmarks.Create(Viewer, Self);
end;

function TdxPDFViewerBookmarks.CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo;
begin
  FViewInfo := TdxPDFViewerBookmarksPageViewInfo.Create(Viewer.NavigationPane.Controller, Self);
  Result := FViewInfo;
end;

function TdxPDFViewerBookmarks.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageBookmarksCaption);
end;

function TdxPDFViewerBookmarks.GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass;
begin
  Result := TdxPDFViewerBookmarkTreeView;
end;

procedure TdxPDFViewerBookmarks.DestroySubClasses;
begin
  FreeAndNil(FExpandBookmarkGlyph);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerBookmarks.LoadDefaultGlyphs;
begin
  FDefaultGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERBOOKMARKSBUTTON', 'SVG');
  FExpandBookmarkGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWEREXPANDBOOKMARKBUTTON', 'PNG');
end;

function TdxPDFViewerBookmarks.CanExpandCurrentBookmark: Boolean;
begin
  Result := Tree.CanExpandSelectedBookmark;
end;

function TdxPDFViewerBookmarks.IsBookmarkSelected: Boolean;
begin
  Result := Tree.IsBookmarkSelected;
end;

function TdxPDFViewerBookmarks.IsTopLevelBookmarksExpanded: Boolean;
begin
  Result := Tree.IsTopLevelBookmarksExpanded;
end;

function TdxPDFViewerBookmarks.GetTree: TdxPDFViewerBookmarkTreeView;
begin
  Result := FControl as TdxPDFViewerBookmarkTreeView;
end;

{ TdxPDFViewerAttachments }

function TdxPDFViewerAttachments.HasAttachments: Boolean;
begin
  Result := FileList.View.SelCount > 0;
end;

procedure TdxPDFViewerAttachments.OpenAttachment;
begin
  Viewer.OpenAttachment(GetSelectedAttachment);
end;

procedure TdxPDFViewerAttachments.SaveAttachment;
begin
  Viewer.SaveAttachment(GetSelectedAttachment);
end;

function TdxPDFViewerAttachments.CanShow: Boolean;
begin
  Result := inherited CanShow or ((Options.Visible = bDefault) and not Empty);
end;

function TdxPDFViewerAttachments.CreateOptions: TdxPDFViewerOptionsNavigationPage;
begin
  Result := TdxPDFViewerOptionsAttachments.Create(Viewer, Self);
end;

function TdxPDFViewerAttachments.CreateViewInfo: TdxPDFViewerNavigationPanePageViewInfo;
begin
  FViewInfo := TdxPDFViewerAttachmentsPageViewInfo.Create(Viewer.NavigationPane.Controller, Self);
  Result := FViewInfo;
end;

function TdxPDFViewerAttachments.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxPDFViewerNavigationPageAttachmentsCaption);
end;

function TdxPDFViewerAttachments.GetControlClass: TdxPDFViewerNavigationPaneInternalControlClass;
begin
  Result := TdxPDFViewerAttachmentFileList;
end;

procedure TdxPDFViewerAttachments.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FShowHints := Viewer.OptionsBehavior.ShowHints;
end;

procedure TdxPDFViewerAttachments.DestroySubClasses;
begin
  FreeAndNil(FSaveAttachmentGlyph);
  FreeAndNil(FOpenAttachmentGlyph);
  inherited DestroySubClasses;
end;

procedure TdxPDFViewerAttachments.LoadDefaultGlyphs;
begin
  FDefaultGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERATTACHMENTSBUTTON', 'SVG');
  FOpenAttachmentGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERATTACHMENTOPENBUTTON', 'PNG');
  FSaveAttachmentGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERATTACHMENTSAVEBUTTON', 'PNG');
end;

function TdxPDFViewerAttachments.GetFileList: TdxPDFViewerAttachmentFileList;
begin
  Result := FControl as TdxPDFViewerAttachmentFileList;
end;

function TdxPDFViewerAttachments.GetSelectedAttachment: TdxPDFFileAttachment;
begin
  if FileList.View.ItemIndex >= 0 then
    Result := FileList.DocumentAttachments[FileList.View.ItemIndex]
  else
    Result := nil;
end;

procedure TdxPDFViewerAttachments.SetShowHints(const AValue: Boolean);
begin
  if FShowHints <> AValue then
  begin
    FShowHints := AValue;
    FileList.ShowHint := FShowHints;
  end;
end;

{ TdxPDFViewerNavigationPane }

constructor TdxPDFViewerNavigationPane.Create(AViewer: TdxPDFCustomViewer);
begin
  inherited Create;
  FViewer := AViewer;
  FViewer.AddFontListener(Self);
  LoadGlyphs;
  FActivePage := nil;
  FActivePageState := wsMinimized;
  FPageSize := FViewer.ScaleFactor.Apply(300);
  FPrevActivePageState := wsNormal;
  FActivePageState := wsMinimized;
  FFont := TFont.Create;
  FFont.Assign(FViewer.Font);
  FHitTest := TdxPDFViewerNavigationPaneHitTest.Create(FViewer);
  FOptions := TdxPDFViewerOptionsNavigationPane.Create(FViewer);
  FController := TdxPDFViewerNavigationPaneController.Create(FViewer);
  FVisiblePages := TList<TdxPDFViewerNavigationPanePage>.Create;
  FShowHints := FViewer.OptionsBehavior.ShowHints;
end;

destructor TdxPDFViewerNavigationPane.Destroy;
begin
  FViewer.RemoveFontListener(Self);
  FreeAndNil(FVisiblePages);
  FreeAndNil(FViewInfo);
  FreeAndNil(FPages);
  FreeAndNil(FController);
  FreeAndNil(FRestoreButtonGlyph);
  FreeAndNil(FMinimizeButtonGlyph);
  FreeAndNil(FMenuButtonGlyph);
  FreeAndNil(FMaximizeButtonGlyph);
  FreeAndNil(FOptions);
  FreeAndNil(FHitTest);
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TdxPDFViewerNavigationPane.CanShow: Boolean;
var
  APage: TdxPDFViewerNavigationPanePage;
begin
  Result := False;
  if FViewer.IsDocumentAvailable and FOptions.Visible then
    for APage in Pages do
    begin
      Result := APage.CanShow;
      if Result then
       Break;
     end;
end;

function TdxPDFViewerNavigationPane.CalculateParentClientBounds(const AClientRect: TRect): TRect;
begin
  if IsMaximized then
    Result := cxNullRect
  else
  begin
    Result := AClientRect;
    Result.Left := Result.Left + MeasureWidth;
    if Result.Left >= Result.Right + FViewer.VScrollBar.Width then
      Result := cxNullRect;
  end;
end;

function TdxPDFViewerNavigationPane.IsFirst(APage: TdxPDFViewerNavigationPanePage): Boolean;
begin
  Result := FVisiblePages.IndexOf(APage) = 0;
end;

function TdxPDFViewerNavigationPane.IsVisible(APage: TdxPDFViewerNavigationPanePage): Boolean;
begin
  Result := FVisiblePages.Contains(APage);
end;

function TdxPDFViewerNavigationPane.IsMaximized: Boolean;
begin
  Result := ActivePageState = wsMaximized;
end;

function TdxPDFViewerNavigationPane.MeasureWidth: Integer;
begin
  Result := FViewInfo.MeasureWidth;
end;

procedure TdxPDFViewerNavigationPane.AddPages;
begin
  FBookmarks := TdxPDFViewerBookmarks.Create(FViewer);
  FThumbnails := TdxPDFViewerThumbnails.Create(FViewer);
  FAttachments := TdxPDFViewerAttachments.Create(FViewer);

  FPages := TObjectList<TdxPDFViewerNavigationPanePage>.Create;
  FPages.Add(FThumbnails);
  FPages.Add(FBookmarks);
  FPages.Add(FAttachments);

  UpdateVisiblePages;

  FViewInfo := TdxPDFViewerNavigationPaneViewInfo.Create(FController, Self);
end;

procedure TdxPDFViewerNavigationPane.ActivatePage;
begin
  if ActivePageState = wsMinimized then
    ActivePageState := FPrevActivePageState
  else
    MinimizePage;
end;

procedure TdxPDFViewerNavigationPane.Calculate(var ABounds: TRect);
var
  ARect: TRect;
begin
  if CanShow then
  begin
    if IsMaximized then
      ARect := ABounds
    else
    begin
      ARect := ABounds;
      ARect.Right := ARect.Left + MeasureWidth
    end;
    ABounds.Left := ARect.Right
  end
  else
    ARect := cxNullRect;

  ViewInfo.Bounds := ARect;
end;

procedure TdxPDFViewerNavigationPane.Changed;
begin
  ViewInfo.RecreateCells;
  FViewer.LayoutChanged(ctHard);
  Thumbnails.SynchronizeSelectedPage;
end;

procedure TdxPDFViewerNavigationPane.Clear;
begin
  Controller.Clear;
end;

procedure TdxPDFViewerNavigationPane.MaximizePage;
begin
  if IsMaximized then
    RestorePage
  else
    ActivePageState := wsMaximized;
end;

procedure TdxPDFViewerNavigationPane.MinimizePage;
begin
  ActivePageState := wsMinimized;
end;

procedure TdxPDFViewerNavigationPane.RestorePage;
begin
  if ActivePage <> nil then
    ActivePageState := wsNormal
  else
    ActivePageState := wsMinimized;
end;

procedure TdxPDFViewerNavigationPane.Refresh;
begin
  Changed;
  Controller.Refresh;
  if ActivePage <> nil then
    MinimizePage;
end;

procedure TdxPDFViewerNavigationPane.VisibilityChanged(APage: TdxPDFViewerNavigationPanePage);
begin
  UpdateVisiblePages;
  if (ActivePage = APage) and not APage.CanShow then
    ActivePageType := apNone;
  Changed;
end;

function TdxPDFViewerNavigationPane.GetActivePageType: TdxPDFViewerNavigationPaneActivePage;
begin
  if ActivePage = nil then
    Result := apNone
  else
    if ActivePage = Bookmarks then
      Result := apBookmarks
    else
      if ActivePage = Attachments then
        Result := apAttachments
      else
        Result := apThumbnails;
end;

function TdxPDFViewerNavigationPane.GetMaximizeButtonGlyph: TdxSmartGlyph;
begin
  if IsMaximized then
    Result := RestoreButtonGlyph
  else
    Result := FMaximizeButtonGlyph;
end;

function TdxPDFViewerNavigationPane.GetVisiblePages: TList<TdxPDFViewerNavigationPanePage>;
begin
  UpdateVisiblePages;
  Result := FVisiblePages;
end;

procedure TdxPDFViewerNavigationPane.SetActivePage(const AValue: TdxPDFViewerNavigationPanePage);
begin
  if FActivePage <> AValue then
  begin
    FActivePage := AValue;
    RestorePage;
    Changed;
  end
  else
    ActivatePage;
end;

procedure TdxPDFViewerNavigationPane.SetActivePageType(const AValue: TdxPDFViewerNavigationPaneActivePage);
var
  APage: TdxPDFViewerNavigationPanePage;
begin
  APage := nil;
  case AValue of
    apBookmarks:
      if Bookmarks.CanShow then
        APage := Bookmarks;
    apThumbnails:
      if Thumbnails.CanShow then
        APage := Thumbnails;
    apAttachments:
      if Attachments.CanShow then
        APage := Attachments;
  end;
  ActivePage := APage;
end;

procedure TdxPDFViewerNavigationPane.SetActivePageState(const AValue: TWindowState);
begin
  if FActivePageState <> AValue then
  begin
    FPrevActivePageState := FActivePageState;
    if ActivePageType = apNone then
      FActivePageState := wsMinimized
    else
      FActivePageState := AValue;
    Changed;
  end;
end;

procedure TdxPDFViewerNavigationPane.SetOptions(const AValue: TdxPDFViewerOptionsNavigationPane);
begin
  FOptions.Assign(AValue);
end;

procedure TdxPDFViewerNavigationPane.SetPageSize(const AValue: Integer);
begin
  if FPageSize <> AValue then
  begin
    FPageSize := AValue;
    Changed;
    FViewer.MakeVisible(FViewer.CurrentPageIndex);
  end;
end;

procedure TdxPDFViewerNavigationPane.SetShowHints(const AValue: Boolean);
begin
  if FShowHints <> AValue then
  begin
    FShowHints := AValue;
    Attachments.ShowHints := ShowHints;
    Thumbnails.ShowHints := ShowHints;
  end;
end;

procedure TdxPDFViewerNavigationPane.Changed(Sender: TObject; AFont: TFont);
begin
  FFont.Assign(AFont);
  FFont.Style := [fsBold];
end;

procedure TdxPDFViewerNavigationPane.LoadGlyphs;
begin
  FMaximizeButtonGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERMAXIMIZEBUTTON', 'PNG');
  FMenuButtonGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERMENUBUTTON', 'PNG');
  FMinimizeButtonGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERMINIMIZEBUTTON', 'PNG');
  FRestoreButtonGlyph := TdxPDFUtils.LoadGlyph('DX_PDFVIEWERRESTOREBUTTON', 'PNG');
end;

procedure TdxPDFViewerNavigationPane.UpdateVisiblePages;
var
  I: Integer;
begin
  FVisiblePages.Clear;
  for I := 0 to FPages.Count - 1 do
    if FPages[I].CanShow then
      FVisiblePages.Add(FPages[I])
    else
      FPages[I].Bounds := cxNullRect;
end;

initialization
  Screen.Cursors[crdxPDFViewerContext] := LoadCursor(HInstance, 'DXPDFVIEWER_CONTEXT');
  Screen.Cursors[crdxPDFViewerCross] := LoadCursor(HInstance, 'DXPDFVIEWER_CROSS');

end.


