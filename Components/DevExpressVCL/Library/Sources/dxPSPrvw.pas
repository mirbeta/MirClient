{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPrVw;

interface

{$I cxVer.inc}

uses
  Windows, Classes, SysUtils, Messages, Controls, ExtCtrls, ComCtrls, Buttons, ImgList, Dialogs, Forms, Graphics,
  IniFiles,
  dxMessages, dxCustomPreview, dxPSGlbl, dxPSCore, dxPSESys, dxPrevw, dxPSPrVwOpt, dxPrnPg, dxExtCtrls, dxBkgnd,
  cxLookAndFeels, cxControls, cxGraphics, cxGeometry, cxSplitter, cxLookAndFeelPainters, dxBase, cxProgressBar,
  cxButtons;

type
  TfmPreviewStatusSection = (ssCurrentPage, ssPageCount, ssPaperSize, ssStatus);
  TfmPreviewStatusSections = set of TfmPreviewStatusSection;

  TdxPSPreviewWindowStatusBarMode = (psbmNormal, psbmProgress);

  TdxPSCustomStatusBar = class;
  TdxPSPreviewWindow = class;
  TdxPSStatusBarPanel = class;

  TdxPSPreviewWindowAddExplorerCommandEvent = procedure (Sender: TObject; ACommand: TCustomdxPSExplorerContextCommand) of object;
  TdxPSPreviewWindowCanShowMarginHint = procedure (Sender: TObject; var AAllow: Boolean) of object;

  { IdxPSPreviewMeasurementUnitsProvider }

  IdxPSPreviewMeasurementUnitsProvider = interface
  ['{74946B75-5EFB-43A1-B08E-74CA6F00AD7D}']
    function GetMeasurementUnits: TdxPreviewMeasurementUnits;
  end;

  { TdxPSExplorerToolBar }

  TdxPSExplorerToolBar = class(TcxControl)
  protected
    procedure Paint; override;
  public
    property Font;
  end;

  { TdxPSExplorerHostPanel }

  TdxPSExplorerHostPanel = class(TcxControl)
  protected
    procedure Paint; override;
  end;

  { TdxPSExplorerCloseButton }

  TdxPSExplorerCloseButton = class(TcxButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPSPreviewExplorerChangeNotifier }

  TdxPSPreviewExplorerChangeNotifier = class(TdxPSExplorerChangeNotifierAdapter)
  private
    FPreview: TdxPSPreviewWindow;
  protected
    procedure ItemDataLoaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); override;
  public
    constructor Create(APreview: TdxPSPreviewWindow);
    //
    property Preview: TdxPSPreviewWindow read FPreview;
  end;

  { TdxPSPreviewWindowCustomHost }

  TdxPSPreviewWindowCustomHostOnSize = procedure (Sender: TObject; var AWidth, AHeight: Integer) of object;

  TdxPSPreviewWindowCustomHost = class(TcxControl)
  strict private
    FTransparent: Boolean;

    FOnSizeChanging: TdxPSPreviewWindowCustomHostOnSize;
  protected
    procedure DoSizeChanging(var AWidth, AHeight: Integer); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    function HasBackground: Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    //
    property Transparent: Boolean read FTransparent write FTransparent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //
    property OnResize;
    property OnSizeChanging: TdxPSPreviewWindowCustomHostOnSize read FOnSizeChanging write FOnSizeChanging;
  end;

  { TdxPSPreviewWindowOptions }

  TdxPSPreviewWindowOptions = class(TdxPreviewWindowOptions)
  strict private
    FShowMarginBar: Boolean;
    FShowStatusBar: Boolean;
    FZoomOnClick: Boolean;

    procedure SetShowMarginBar(AValue: Boolean);
    procedure SetShowStatusBar(AValue: Boolean);
    procedure SetZoomOnClick(AValue: Boolean);
  protected
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure DoRestoreDefaults; override;
  public
    constructor Create; override;
  published
    property ShowMarginBar: Boolean read FShowMarginBar write SetShowMarginBar default True;
    property ShowStatusBar: Boolean read FShowStatusBar write SetShowStatusBar default True;
    property ZoomOnClick: Boolean read FZoomOnClick write SetZoomOnClick default True;
  end;

  { TdxPSPreviewWindow }

  TdxPSPreviewWindowLoadPropertiesEvent = procedure (Sender: TObject; AIniFile: TCustomIniFile; const ASectionName: string) of object;

  TdxPSPreviewWindow = class(TdxPSCustomPreviewWindow,
    IdxComponentPrinterListener,
    IdxSkinSupport,
    IdxPSExplorerTreeContainerHost)
  strict private
    FBuildEventsSubscriber: TdxEventSubscriber;
    FCurrentProgressValue: Integer;
    FFullPageCount: Integer;
    FHFEditPart: TdxPageTitlePart;
    FHFFunctionList: TStringList;
    FHFTextEntriesChangedSubscriber: TdxEventSubscriber;
    FLastOpCompleted: Integer;
    FLastValidZoomFactor: Integer;
    FLockPageSelection: Boolean;
    FPredefinedZooms: TStringList;
    FPreviewPopupMenu: TComponent;
    FPrintEventsSubscriber: TdxEventSubscriber;
    FProgressBar: TcxProgressBar;
    FSavePageIndex: Integer;
    FState: TdxPSPreviewState;
    FStyleEventsSubscriber: TdxEventSubscriber;
    FTransparent: Boolean;
    FUpdateCount: Integer;

    FOnAddExplorerCommand: TdxPSPreviewWindowAddExplorerCommandEvent;
    FOnCanShowMarginHint: TdxPSPreviewWindowCanShowMarginHint;
    FOnHFTextEntriesChanged: TNotifyEvent;
    FOnInitContent: TNotifyEvent;
    FOnLoadProperties: TdxPSPreviewWindowLoadPropertiesEvent;
    FOnPreviewDblClick: TNotifyEvent;
    FOnSaveProperties: TdxPSPreviewWindowLoadPropertiesEvent;
    FOnStyleListChanged: TNotifyEvent;
    FOnUpdateControls: TNotifyEvent;
    FOnUpdateExplorerCommands: TNotifyEvent;
    FOnZoomFactorChanged: TNotifyEvent;
    FOnZoomModeChanged: TNotifyEvent;

    function CanCallEvents: Boolean;
    function GetActualEnableOptions: TdxPreviewEnableOptions;
    function GetActualVisibleOptions: TdxPreviewVisibleOptions;
    function GetExplorer: TCustomdxPSExplorer;
    function GetExplorerPaneWidth: Integer;
    function GetFlat: Boolean;
    function GetIsExplorerAvailable: Boolean;
    function GetOptions: TdxPSPreviewWindowOptions;
    function GetPrinterPage: TdxPrinterPage;
    function GetProgressStatusPanel: TdxPSStatusBarPanel;
    function GetStatusTextPanel: TdxPSStatusBarPanel;
    function GetThumbnailsPaneWidth: Integer;
    procedure SetExplorerPaneWidth(Value: Integer);
    procedure SetOptions(AValue: TdxPSPreviewWindowOptions);
    procedure SetPreviewPopupMenu(AValue: TComponent);
    procedure SetThumbnailsPaneWidth(Value: Integer);
    procedure SetTransparent(AValue: Boolean);

    function CreateSplitter(AAlign: TAlign; const AName: string; ALeft: Integer;
      AParent: TWinControl; AResizeControl: TControl; OnCanResizeEvent: TCanResizeEvent): TcxCustomSplitter;
    procedure FillEffectsApply(Sender: TObject);
    procedure ExplorerCloseHandler(Sender: TObject);
    procedure ExplorerSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure ExplorerToolBarResize(Sender: TObject);
    procedure MarginBarDblClick(Sender: TObject);
    procedure MarginBarGetDrawParams(ASender: TdxPSCustomStatusBar; APanel: TdxPSStatusBarPanel; var ABkgColor, ATextColor: TColor);
    procedure PreviewAfterDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
    procedure PreviewBeforeDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
    procedure PreviewCanShowMarginHint(Sender: TObject; var ACanShowHint: Boolean);
    procedure PreviewDblClick(Sender: TObject);
    procedure PreviewDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
    procedure PreviewMarginsChanged(Sender: TObject; AMargin: TdxPreviewPageMargin);
    procedure PreviewSelectedPageChanged(Sender: TObject; APageIndex: Integer);
    procedure PreviewZoomFactorChanged(Sender: TObject);
    procedure PreviewZoomModeChanged(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ThumbnailsPreviewSelectedPageChanged(Sender: TObject; APageIndex: Integer);
    procedure ThumbnailsSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);

    procedure EndGenerateReport(Sender: TObject; AReportLink: TBasedxReportLink);
    procedure GenerateReportProgress(Sender: TObject; AReportLink: TBasedxReportLink; APercentDone: Double);
    procedure StartGenerateReport(Sender: TObject; AReportLink: TBasedxReportLink);

    procedure EndPrint(Sender: TObject; AReportLink: TBasedxReportLink);
    procedure NewPage(Sender: TObject; AReportLink: TBasedxReportLink; APageIndex: Integer);
    procedure StartPrint(Sender: TObject; AReportLink: TBasedxReportLink; FullPageCount: Integer);

    procedure LoadZooms;
    procedure UpdateMarginBar;
    procedure UpdateStatusText;
    procedure DXMRecalculate(var Message: TMessage); message DXM_RECALCULATE;
    procedure WMAppCommand(var Message: TMessage); message WM_APPCOMMAND;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSettingChange(var Message: TWMSettingChange); message WM_SETTINGCHANGE;
  protected
    FAreMarginsValid: Boolean;
    FExplorerChangeNotifier: TdxPSPreviewExplorerChangeNotifier;
    FExplorerCloseButton: TdxPSExplorerCloseButton;
    FExplorerPane: TdxPSPreviewWindowCustomHost;
    FExplorerSplitter: TcxCustomSplitter;
    FExplorerToolBar: TdxPSExplorerToolBar;
    FExplorerTree: TCustomdxPSExplorerTreeContainer;
    FExplorerTreeHost: TdxPSExplorerHostPanel;
    FMarginBar: TdxPSCustomStatusBar;
    FPreview: TdxPreview;
    FPreviewPane: TdxPSPreviewWindowCustomHost;
    FReleased: Boolean;
    FStatusBar: TdxPSCustomStatusBar;
    FThumbnailsPane: TdxPSPreviewWindowCustomHost;
    FThumbnailsPreview: TdxPreview;
    FThumbnailsSplitter: TcxCustomSplitter;

    procedure AdjustClientRect(var Rect: TRect); override;
    function CalculateStatusBarHeight: Integer;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure FontChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ScaleFactorChanged; override;
    procedure WndProc(var message: TMessage); override;

    function CreateOptions: TdxPreviewWindowOptions; override;
    function GetActivePageIndex: Integer; override;
    function GetBackground: TdxBackground; override;
    function GetExplorerTree: TCustomdxPSExplorerTreeContainer; override;
    function GetHFEditPart: TdxPageTitlePart; override;
    function GetPageCount: Integer; override;
    function GetState: TdxPSPreviewState; override;
    function GetZoomFactor: Integer; override;
    procedure SetActivePageIndex(Value: Integer); override;
    procedure SetBackground(const Value: TdxBackground); override;
    procedure SetFocusToControl(AControl: TWinControl);
    procedure SetHFEditPart(const Value: TdxPageTitlePart); override;
    procedure SetState(const Value: TdxPSPreviewState); override;
    procedure SetZoomFactor(Value: Integer); override;

    procedure AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand); override;
    procedure AfterComponentPrinterChanged; override;
    procedure BeforeComponentPrinterChanged; override;
    procedure BorderStyleChanged; override;
    procedure BoundsChanged; override;
    procedure CreateHandle; override;
    procedure DoInitContent; virtual;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    function HasBackground: Boolean; override;
    procedure InitializeControlLookAndFeel(AControl: TControl);
    procedure InitializePreviewWindowLayout;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure OptionsChanged; override;
    procedure UpdateSplittersBackgroundColor;

  {$IFDEF OLEDRAGANDDROP}
    function DoCanAccept: Boolean; override;
  {$ENDIF}

    procedure DoUpdatePanesState(Sender: TObject);
    procedure ExplorerPaneSizeChanging(Sender: TObject; var AWidth, AHeight: Integer);
    procedure ThumbnailsPreviewSizeChanging(Sender: TObject; var AWidth, AHeight: Integer);
    procedure UpdateThumbnailsSize;

    function MarginStatusPanel(AMargin: TdxPreviewPageMargin): TdxPSStatusBarPanel;
    function ProgressBarGetMaxValue: Integer;
    function SectionStatusPanel(AStatusSection: TfmPreviewStatusSection): TdxPSStatusBarPanel;
    procedure InvalidatePagesHeadersOrFooters;
    procedure PrepareProgress;
    procedure ProgressBarHide;
    procedure ProgressBarPlace;
    procedure ProgressBarRefresh;
    procedure ProgressBarShow;
    procedure RefreshMarginBar(AMargin: TdxPreviewPageMargin);
    procedure RefreshStatusBar(AStatusSections: TfmPreviewStatusSections);
    procedure SectionStatusPanelSetText(AStatusSection: TfmPreviewStatusSection; const AText: string);
    procedure UnprepareProgress;
    procedure UpdateControlsPosition;
    procedure UpdateStatusBarPanelWidths(AStatusBar: TdxPSCustomStatusBar);
    procedure UpdateTransparency;

    procedure LoadProperties(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SaveProperties(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SavePreferences(AData: TdxPreviewOptionsDlgData);

    procedure CreateControls; virtual;
    procedure CreateEventSubscribers; virtual;
    procedure CreateExplorerPane; virtual;
    procedure CreateExplorerSplitter; virtual;
    procedure CreateExplorerToolBar; virtual;
    procedure CreateExplorerTreeHost; virtual;
    procedure CreateMarginBar; virtual;
    procedure CreatePreview; virtual;
    procedure CreatePreviewPane; virtual;
    procedure CreateProgressBar; virtual;
    procedure CreateStatusBar; virtual;
    procedure CreateThumbnailsPane; virtual;
    procedure CreateThumbnailsPreview; virtual;
    procedure CreateThumbnailsSplitter; virtual;
    procedure RefreshStatusPanels(AStatusBarMode: TdxPSPreviewWindowStatusBarMode); virtual;

    procedure DoExplorerButtonsPosUpdate;
    procedure DoExplorerShowToggled(Value: Boolean);
    procedure DoLoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure DoPreviewAfterDragMargin(APreview: TdxPreview; AMargin: TdxPreviewPageMargin); virtual;
    procedure DoPreviewBeforeDragMargin(APreview: TdxPreview; AMargin: TdxPreviewPageMargin); virtual;
    procedure DoPreviewDblClick(APreview: TdxPreview); virtual;
    procedure DoPreviewDragMargin(APreview: TdxPreview; AMargin: TdxPreviewPageMargin); virtual;
    procedure DoPreviewMarginChanged(APreview: TdxPreview; AMargin: TdxPreviewPageMargin); virtual;
    procedure DoSaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure DoThumbnailsToggleShow(Value: Boolean);
    procedure DoUpdateControls; virtual;
    procedure HFTextEntriesChanged; virtual;
    procedure LoadStrings; virtual;
    procedure StyleListChanged(Sender: TObject); virtual;
    procedure UpdateStatusBarsHeight;
    function ValidateMargins: Boolean; virtual;

    // IdxComponentPrinterListener
    procedure BeforeDestroyReport(AReportLink: TBasedxReportLink);
    procedure CurrentLinkChanged(AReportLink: TBasedxReportLink);
    procedure ExplorerChanged(AReportLink: TBasedxReportLink);
    procedure LayoutChanged(AReportLink: TBasedxReportLink);
    procedure PageParamsChanged(AReportLink: TBasedxReportLink);
    procedure PrepareBuildReport(AReportLink: TBasedxReportLink);
    procedure UnprepareBuildReport(AReportLink: TBasedxReportLink);

    { IdxPSExplorerTreeContainerHost }
    function IdxPSExplorerTreeContainerHost.GetFlat = IdxPSExplorerTreeContainerHost_GetFlat;
    function IdxPSExplorerTreeContainerHost.GetReportLink = IdxPSExplorerTreeContainerHost_GetReportLink;
    function IdxPSExplorerTreeContainerHost_GetFlat: Boolean;
    function IdxPSExplorerTreeContainerHost_GetReportLink: TBasedxReportLink;
    function GetTreeContainerParent: TcxControl;
    procedure UpdateState;

    property Flat: Boolean read GetFlat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure GoToFirstPage; override;
    procedure GoToLastPage; override;
    procedure GoToNextPage; override;
    procedure GoToPrevPage; override;
    procedure InitContent; override;
    procedure InvalidateAllPages; override;
    procedure InvalidateContent; override;
    procedure InvalidatePage(APageIndex: Integer); override;
    procedure InvalidatePagesContent; override;
    procedure InvalidatePagesFooterContent; override;
    procedure InvalidatePagesHeaderContent; override;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure UpdateControls; override;
    procedure UpdateExplorerContextCommands; override;

    function CanChangeMargins: Boolean;
    function CanClosePreviewWindow: Boolean; virtual;
    function CanDesign: Boolean;
    function CanExport: Boolean;
    function CanLoadReport: Boolean;
    function CanPageSetup: Boolean;
    function CanPrint: Boolean;
    function CanPrintDialog: Boolean;
    function CanPrintStyle: Boolean;
    function CanRebuild: Boolean;
    function CanSaveReport: Boolean;
    function CanShowOptionsDialog: Boolean;
    function CanShowPageBackgroundDialog: Boolean;
    function CanUnloadReport: Boolean;
    function IsAutoHFTextEntriesAvailable: Boolean;
    function IsBuilding: Boolean;
    function IsCommandLoadReportVisible: Boolean;
    function IsCommandSaveReportVisible: Boolean;
    function IsCommandUnloadReportVisible: Boolean;
    function IsEnabled(AOption: TdxPreviewEnableOption): Boolean;
    function IsPrinting: Boolean;
    function IsProgressState: Boolean;
    function IsVisible(AOption: TdxPreviewVisibleOption): Boolean;

    procedure DoClearHF;
    procedure DoDesignReport;
    procedure DoFormatFootnotes;
    procedure DoFormatTitle;
    procedure DoInsertHF(const S: string);
    procedure DoInvokeHelp;
    procedure DoLoadReportLinkDataFromFile;
    procedure DoPageSetupReport(APageIndex: Integer = 0);
    procedure DoPrintReport(AShowDialog: Boolean);
    procedure DoSaveReportLinkDataToFile;
    procedure DoSetupZoomFactor(AZoomFactor, APageXCount, APageYCount: Integer; AZoomMode: TdxPreviewZoomMode);
    procedure DoShowFormatDateTimeDlg;
    procedure DoShowFormatPageNumbersDlg;
    procedure DoShowHFBackgroundDlg(const Pt: TPoint);
    procedure DoShowMultiplySelectPagesDlg(AImageList: TCustomImageList; AImageIndex: Integer; const Pt: TPoint; AYShift: Integer);
    procedure DoShowOptionsDlg;
    procedure DoShowPageBackgroundDlg(const Pt: TPoint);
    procedure DoShowPageMargins(Value: Boolean);
    procedure DoShowZoomDlg;
    procedure DoSyncPrintingPageBackground;
    procedure DoUnloadReportLinkData;

    procedure DoExplorerCreateNewFolder;
    procedure DoExplorerCreateNewItem;
    procedure DoExplorerDeleteItem;
    function DoExplorerItemShowPropertySheets: Boolean;
    procedure DoExplorerLoadItemData;
    procedure DoExplorerRenameItem;
    procedure DoExplorerUnloadItemData;

    procedure BeginUpdate; override;
    procedure CancelUpdate; override;
    procedure EndUpdate; override;
    function Locked: Boolean; override;
    procedure FullRefresh; override;

    procedure SetZoomFactorByText(const AText: string);

    property ActualEnableOptions: TdxPreviewEnableOptions read GetActualEnableOptions;
    property ActualVisibleOptions: TdxPreviewVisibleOptions read GetActualVisibleOptions;
    property HFFunctionList: TStringList read FHFFunctionList;
    property MarginBar: TdxPSCustomStatusBar read FMarginBar;
    property PredefinedZooms: TStringList read FPredefinedZooms;
    property Preview: TdxPreview read FPreview;
    property PreviewPane: TdxPSPreviewWindowCustomHost read FPreviewPane;
    property PrinterPage: TdxPrinterPage read GetPrinterPage;
    property ProgressBar: TcxProgressBar read FProgressBar;
    property ProgressStatusPanel: TdxPSStatusBarPanel read GetProgressStatusPanel;
    property StatusBar: TdxPSCustomStatusBar read FStatusBar;
    property StatusTextPanel: TdxPSStatusBarPanel read GetStatusTextPanel;
    // Explorer
    property Explorer: TCustomdxPSExplorer read GetExplorer;
    property ExplorerCloseButton: TdxPSExplorerCloseButton read FExplorerCloseButton;
    property ExplorerPane: TdxPSPreviewWindowCustomHost read FExplorerPane;
    property ExplorerPaneWidth: Integer read GetExplorerPaneWidth write SetExplorerPaneWidth;
    property ExplorerSplitter: TcxCustomSplitter read FExplorerSplitter;
    property ExplorerToolBar: TdxPSExplorerToolBar read FExplorerToolBar;
    property ExplorerTreeHost: TdxPSExplorerHostPanel read FExplorerTreeHost;
    property IsExplorerAvailable: Boolean read GetIsExplorerAvailable;
    // Thumbnails
    property ThumbnailsPane: TdxPSPreviewWindowCustomHost read FThumbnailsPane;
    property ThumbnailsPaneWidth: Integer read GetThumbnailsPaneWidth write SetThumbnailsPaneWidth;
    property ThumbnailsPreview: TdxPreview read FThumbnailsPreview;
    property ThumbnailsSplitter: TcxCustomSplitter read FThumbnailsSplitter;
  published
    property Align;
    property BorderStyle default cxcbsDefault;
    property ComponentPrinter;
    property LookAndFeel;
    property Options: TdxPSPreviewWindowOptions read GetOptions write SetOptions;
    property PreviewPopupMenu: TComponent read FPreviewPopupMenu write SetPreviewPopupMenu;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnAddExplorerCommand: TdxPSPreviewWindowAddExplorerCommandEvent read FOnAddExplorerCommand write FOnAddExplorerCommand;
    property OnCanShowMarginHint: TdxPSPreviewWindowCanShowMarginHint read FOnCanShowMarginHint write FOnCanShowMarginHint;
    property OnHFTextEntriesChanged: TNotifyEvent read FOnHFTextEntriesChanged write FOnHFTextEntriesChanged;
    property OnInitContent: TNotifyEvent read FOnInitContent write FOnInitContent;
    property OnLoadProperties: TdxPSPreviewWindowLoadPropertiesEvent read FOnLoadProperties write FOnLoadProperties;
    property OnPreviewDblClick: TNotifyEvent read FOnPreviewDblClick write FOnPreviewDblClick;
    property OnSaveProperties: TdxPSPreviewWindowLoadPropertiesEvent read FOnSaveProperties write FOnSaveProperties;
    property OnStyleListChanged: TNotifyEvent read FOnStyleListChanged write FOnStyleListChanged;
    property OnUpdateControls: TNotifyEvent read FOnUpdateControls write FOnUpdateControls;
    property OnUpdateExplorerCommands: TNotifyEvent read FOnUpdateExplorerCommands write FOnUpdateExplorerCommands;
    property OnZoomFactorChanged: TNotifyEvent read FOnZoomFactorChanged write FOnZoomFactorChanged;
    property OnZoomModeChanged: TNotifyEvent read FOnZoomModeChanged write FOnZoomModeChanged;

    property OnContextPopup;
    property OnResize;
  end;

  { TdxPSStatusBarViewInfo }

  TdxPSStatusBarViewInfo = class(TObject)
  public
    BorderWidths: TRect;
    Bounds: TRect;
    SeparatorSize: Integer;
    SizeGripSize: TSize;
    TextIndent: Integer;
  end;

  { TdxPSStatusBarPanel }

  TdxPSStatusBarPanelStyle = (sbpsPanel, sbpsSeparator);

  TdxPSStatusBarPanel = class(TCollectionItem)
  strict private
    FAlignment: TAlignment;
    FBounds: TRect;
    FShowBorders: Boolean;
    FStyle: TdxPSStatusBarPanelStyle;
    FText: string;
    FTextRect: TRect;
    FWidth: Integer;

    function GetScaleFactor: TdxScaleFactor;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetShowBorders(AValue: Boolean);
    procedure SetStyle(AStyle: TdxPSStatusBarPanelStyle);
    procedure SetText(const AText: string);
    procedure SetWidth(AValue: Integer);
  protected
    procedure Calculate(AViewInfo: TdxPSStatusBarViewInfo); virtual;
    //
    property Bounds: TRect read FBounds;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property TextRect: TRect read FTextRect;
  public
    constructor Create(Collection: TCollection); override;
    //
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ShowBorders: Boolean read FShowBorders write SetShowBorders;
    property Style: TdxPSStatusBarPanelStyle read FStyle write SetStyle;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;

  { TdxPSStatusBarPanels }

  TdxPSStatusBarPanels = class(TCollection)
  strict private
    FStatusBar: TdxPSCustomStatusBar;

    function GetItem(Index: Integer): TdxPSStatusBarPanel;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AStatusBar: TdxPSCustomStatusBar); virtual;
    function Add: TdxPSStatusBarPanel;
    function First: TdxPSStatusBarPanel;
    function Last: TdxPSStatusBarPanel;
    //
    property Items[Index: Integer]: TdxPSStatusBarPanel read GetItem; default;
    property StatusBar: TdxPSCustomStatusBar read FStatusBar;
  end;

  TdxPSStatusBarPanelGetDrawParamsEvent = procedure (ASender: TdxPSCustomStatusBar;
    APanel: TdxPSStatusBarPanel; var ABkgColor, ATextColor: TColor) of object;

  { TdxPSCustomStatusBar }

  TdxPSCustomStatusBar = class(TdxPSPreviewWindowCustomHost)
  strict private
    FPanels: TdxPSStatusBarPanels;
    FSizeGrip: Boolean;
    FSizeGripRect: TRect;
    FUpdateCount: Integer;

    FOnPanelGetDrawParams: TdxPSStatusBarPanelGetDrawParamsEvent;

    function GetTextColor: TColor;
    procedure SetSizeGrip(AValue: Boolean);
  protected
    function CanShowSizeGrip: Boolean;
    procedure Calculate; virtual;
    procedure CalculateSizeGripBounds(AViewInfo: TdxPSStatusBarViewInfo); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateViewInfo: TdxPSStatusBarViewInfo; virtual;
    procedure DoGetPanelDrawParams(APanel: TdxPSStatusBarPanel; var ABkgColor, ATextColor: TColor); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel); virtual;
    procedure DrawPanelItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel); virtual;
    procedure DrawPanelItemText(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel; ATextColor: TColor); virtual;
    procedure DrawSepartorItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel); virtual;
    procedure DrawSizeGrip(ACanvas: TcxCanvas; const R: TRect); virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    procedure DoPaint; override;
    procedure Recalculate; virtual;
    procedure UpdateStateChanged(AUnlocked: Boolean);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    //
    property SizeGripRect: TRect read FSizeGripRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PanelFromPoint(APoint: TPoint; var APanel: TdxPSStatusBarPanel): Boolean; virtual;
    function PanelRect(AIndex: Integer): TRect; virtual;
    procedure InvalidatePanel(AIndex: Integer); virtual;
    //
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //
    property OnPanelGetDrawParams: TdxPSStatusBarPanelGetDrawParamsEvent read FOnPanelGetDrawParams write FOnPanelGetDrawParams;
    property Panels: TdxPSStatusBarPanels read FPanels;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip;
  end;

  { TdxPSStatusBar }

  TdxPSStatusBar = class(TdxPSCustomStatusBar)
  published
    property OnPanelGetDrawParams;
    property Panels;
    property SizeGrip;
  end;

function GetCurrentPrinterAsHint: string;

function AddStatusPanel(AStatusBar: TdxPSCustomStatusBar; AAlignment: TAlignment;
  AShowBorder: Boolean; const AText: string; AWidth: Integer;
  AStyle: TdxPSStatusBarPanelStyle = sbpsPanel): TdxPSStatusBarPanel;

// runtime created component names

const
  sdxExplorerCloseButton = 'ExplorerCloseButton';        // Don't localize
  sdxExplorerPane = 'ExplorerPane';                      // Don't localize
  sdxExplorerSplitter = 'ExplorerSplitter';              // Don't localize
  sdxExplorerToolBar = 'ExplorerToolBar';                // Don't localize
  sdxExplorerTreeHost = 'ExplorerTreeHost';              // Don't localize
  sdxMarginBar = 'MarginBar';                            // Don't localize
  sdxPreviewControl = 'PreviewControl';                  // Don't localize
  sdxPreviewPane = 'PreviewPane';                        // Don't localize
  sdxProgressBar = 'ProgressBar';                        // Don't localize
  sdxStatusBar = 'StatusBar';                            // Don't localize
  sdxThumbnailsPane = 'ThumbnailsPane';                  // Don't localize
  sdxThumbnailsPreviewControl = 'ThumbnailsPreview';     // Don't localize
  sdxThumbnailsSplitter = 'ThumbnailsSplitter';          // Don't localize

implementation

uses
  CommCtrl, Math, Registry, ShlObj, ShellApi, UxTheme, Themes,
{$IFDEF OLEDRAGANDDROP}
  ActiveX,
{$ENDIF}
  dxCore, cxClasses, dxPSRes,
  dxPrnDev, dxPSUtl, dxPgsDlg, dxfmClr, dxfmMnPg, dxfmDTFmt, dxfmPNFmt, dxPSEngn,
  dxPSExtDlgs, dxPSPopupMan, dxPSEvnt, dxfmZoom, Types, dxPSForm, dxPSHFLibrary, StrUtils, dxDPIAwareUtils;

type
  TdxReportLinkAccess = class(TBasedxReportLink);
  TdxPreviewAccess = class(TdxPreview);
  TcxSplitterAccess = class(TcxSplitter);

function MinPreviewSize: TPoint;
begin
  Result.X := 200;
  Result.Y := 200;
end;

const
  CloseGlyphIndex = 114;
  DefaultExplorerPaneWidth: Integer = 220;
  DefaultThumbnailsPaneWidth: Integer = 400;
  PredefinedZoomValueCount: Integer = 8;

  pssAll: TfmPreviewStatusSections = [ssCurrentPage, ssPageCount, ssPaperSize, ssStatus];

  // used as registry key(value) names when storing properties in registry

  //sdxPreviewControl = 'PreviewControl';                  // Don't localize
  sdxShowMarginBar = 'MarginBar';                        // Don't localize
  sdxShowStatusBar = 'StatusBar';                        // Don't localize
  sdxExplorerVisibilityState = 'ExplorerVisibility';     // Don't localize
  sdxExplorerPaneWidth = 'ExplorerPaneWidth';            // Don't localize
  sdxThumbnailsVisibilityState = 'ThumbnailsVisibility'; // Don't localize
  sdxThumbnailsPaneWidth = 'ThumbnailsPaneWidth';        // Don't localize

var
  ClosePaneGlyph: TBitmap;

{ Color Management - mostly borrowed from dxBar.pas }

function GetUltraFlatButtonBorderColor: TColor;
begin
  Result := dxGetNearestColor(GetLightColor(-2, 30, 20));
end;

function GetUltraFlatButtonDownedColor: TColor;
begin
  Result := dxGetNearestColor(GetLightColor(14, 44, 40));
end;

function GetUltraFlatButtonSelColor: TColor;
begin
  Result := dxGetNearestColor(GetLightColor(-2, 30, 72));
end;

{ TdxPSExplorerToolBar }

procedure TdxPSExplorerToolBar.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  LookAndFeelPainter.DrawPanelBackground(Canvas, Self, R, Color);
  LookAndFeelPainter.DrawPanelContent(Canvas, R, False);
  InflateRect(R, -2, -2);
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := LookAndFeelPainter.PanelTextColor;
  Canvas.Brush.Style := bsClear;
  Canvas.DrawTexT(Caption, R, cxAlignLeft or cxAlignVCenter);
end;

{ TdxPSExplorerHostPanel }

procedure TdxPSExplorerHostPanel.Paint;
begin
  LookAndFeelPainter.DrawPanelBackground(Canvas, Self, ClientRect, Color);
  LookAndFeelPainter.DrawPanelContent(Canvas, ClientRect, False);
end;

{ TdxPSExplorerCloseButton }

constructor TdxPSExplorerCloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := sdxExplorerCloseButton;
  Hint := cxGetResourceString(@sdxCloseExplorerHint);
  SpeedButtonOptions.CanBeFocused := False;
  SpeedButtonOptions.Transparent := True;
  SpeedButtonOptions.Flat := True;
  Glyph.Assign(ClosePaneGlyph);
  ShowHint := True;
  Caption := '';
end;

{ helpers }

function GetCurrentPrinterAsHint: string;
begin
  Result := dxPrintDevice.CurrentDevice;
  if Result <> '' then
    Result := ' (' + Result + ')';
end;

function MeasureStatusPanelWidth(AStatusBar: TdxPSCustomStatusBar; const AText: string): Integer;
begin
  cxScreenCanvas.Font := AStatusBar.Font;
  Result := 6 + cxScreenCanvas.TextWidth(AText);
  cxScreenCanvas.Dormant;
end;

function AddStatusPanel(AStatusBar: TdxPSCustomStatusBar; AAlignment: TAlignment; AShowBorder: Boolean;
  const AText: string; AWidth: Integer; AStyle: TdxPSStatusBarPanelStyle = sbpsPanel): TdxPSStatusBarPanel;
begin
  AStatusBar.BeginUpdate;
  try
    Result := AStatusBar.Panels.Add;
    Result.Alignment := AAlignment;
    Result.ShowBorders := AShowBorder;
    Result.Text := AText;
    if (AWidth <> -1) and (AText <> '') then
      AWidth := Max(AWidth, MeasureStatusPanelWidth(AStatusBar, AText));
    Result.Width := AWidth;
    Result.Style := AStyle;
  finally
    AStatusBar.EndUpdate;
  end;
end;

function LoMetricToThousandthsOfInch(Value: Integer): Integer;
begin
  Result := MulDiv(Value, 1000, 254);
end;

function LoMetricToThousandthsOfMM(Value: Integer): Integer;
begin
  Result := 100 * Value;
end;

function DropPercentChar(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = PercentSymbol) do Dec(I);
  Result := Copy(S, 1, I);
end;

{ TdxPSPreviewExplorerChangeNotifier }

constructor TdxPSPreviewExplorerChangeNotifier.Create(APreview: TdxPSPreviewWindow);
begin
  FPreview := APreview;
  inherited Create(Preview.Explorer);
end;

procedure TdxPSPreviewExplorerChangeNotifier.ItemDataLoaded(AnItem: TdxPSExplorerItem);
begin
  Preview.UpdateControls;
end;

procedure TdxPSPreviewExplorerChangeNotifier.ItemDataUnloaded(AnItem: TdxPSExplorerItem);
begin
  Preview.UpdateControls;
end;

{ TdxPSPreviewWindowCustomHost }

constructor TdxPSPreviewWindowCustomHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
end;

procedure TdxPSPreviewWindowCustomHost.DoSizeChanging(var AWidth, AHeight: Integer);
begin
  if Assigned(OnSizeChanging) then
    OnSizeChanging(Self, AWidth, AHeight);
end;

procedure TdxPSPreviewWindowCustomHost.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, LookAndFeelPainter.DefaultControlColor);
end;

procedure TdxPSPreviewWindowCustomHost.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if IsTransparentBackground then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect, False)
  else
    DrawBackground(ACanvas, ARect);
end;

function TdxPSPreviewWindowCustomHost.HasBackground: Boolean;
begin
  Result := True;
end;

function TdxPSPreviewWindowCustomHost.IsDoubleBufferedNeeded: Boolean;
begin
  Result := not (csPaintCopy in ControlState) and inherited IsDoubleBufferedNeeded;
end;

function TdxPSPreviewWindowCustomHost.IsTransparentBackground: Boolean;
begin
  Result := Transparent;
end;

procedure TdxPSPreviewWindowCustomHost.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  DoSizeChanging(AWidth, AHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ TdxPSPreviewWindowOptions }

constructor TdxPSPreviewWindowOptions.Create;
begin
  inherited Create;
  FShowStatusBar := True;
  FShowMarginBar := True;
  FZoomOnClick := True;
end;

procedure TdxPSPreviewWindowOptions.DoAssign(Source: TdxBaseObject);
begin
  inherited DoAssign(Source);

  if Source is TdxPSPreviewWindowOptions then
  begin
    ShowMarginBar := TdxPSPreviewWindowOptions(Source).ShowMarginBar;
    ShowStatusBar := TdxPSPreviewWindowOptions(Source).ShowStatusBar;
    ZoomOnClick := TdxPSPreviewWindowOptions(Source).ZoomOnClick;
  end;
end;

procedure TdxPSPreviewWindowOptions.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;
  ShowMarginBar := True;
  ShowStatusBar := True;
  ZoomOnClick := True;
end;

procedure TdxPSPreviewWindowOptions.SetShowMarginBar(AValue: Boolean);
begin
  if FShowMarginBar <> AValue then
  begin
    FShowMarginBar := AValue;
    Changed;
  end;
end;

procedure TdxPSPreviewWindowOptions.SetShowStatusBar(AValue: Boolean);
begin
  if FShowStatusBar <> AValue then
  begin
    FShowStatusBar := AValue;
    Changed;
  end;
end;

procedure TdxPSPreviewWindowOptions.SetZoomOnClick(AValue: Boolean);
begin
  if FZoomOnClick <> AValue then
  begin
    FZoomOnClick := AValue;
    Changed;
  end;
end;

{ TdxPSPreviewWindow }

constructor TdxPSPreviewWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := prsNone;
  FHFEditPart := tpLeft;
  FLastValidZoomFactor := 100;
  BorderStyle := cxcbsDefault;
  SetBounds(Left, Top, 480, 250);
end;

destructor TdxPSPreviewWindow.Destroy;
begin
  dxPSPopupMan.dxPSPopupMenuController.UnregisterControl(ThumbnailsPreview);
  DoExplorerUnloadItemData;
  FreeAndNil(FExplorerChangeNotifier);
  FreeAndNil(FExplorerTree);
  FreeAndNil(FHFTextEntriesChangedSubscriber);
  FreeAndNil(FBuildEventsSubscriber);
  FreeAndNil(FPrintEventsSubscriber);
  FreeAndNil(FPredefinedZooms);
  FreeAndNil(FStyleEventsSubscriber);
  FreeAndNil(FHFFunctionList);
  inherited Destroy;
end;

procedure TdxPSPreviewWindow.AfterConstruction;
begin
  LoadZooms;
  FHFFunctionList := TStringList.Create;
  dxGetHFFunctionsList(FHFFunctionList);
  CreateControls;
  LoadStrings;
  inherited AfterConstruction;
  InitContent;
end;

procedure TdxPSPreviewWindow.BeforeDestruction;
{$IFDEF OLEDRAGANDDROP}
var
  DropTarget: IDropTarget;
{$ENDIF}
begin
{$IFDEF OLEDRAGANDDROP}
  if Supports(TObject(Self), IDropTarget, DropTarget) then
    CoLockObjectExternal(DropTarget, False, True);
  if Preview.HandleAllocated then
    RevokeDragDrop(Preview.Handle);
{$ENDIF}
  inherited BeforeDestruction;
end;

procedure TdxPSPreviewWindow.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderStyle <> cxcbsNone then
    Rect := cxRectInflate(Rect, -LookAndFeelPainter.BorderSize);
end;

function TdxPSPreviewWindow.CalculateStatusBarHeight: Integer;
begin
  Result := 2 * ScaleFactor.Apply(LookAndFeelPainter.BorderSize) + 2 * (3 + Ord(Flat)) + 2 * cxTextOffset + cxTextHeight(Font);
end;

procedure TdxPSPreviewWindow.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  ExplorerPaneWidth := MulDiv(ExplorerPaneWidth, M, D);
  ThumbnailsPaneWidth := MulDiv(ThumbnailsPaneWidth, M, D);
end;

procedure TdxPSPreviewWindow.FontChanged;
begin
  inherited;
  UpdateStatusBarsHeight;
end;

procedure TdxPSPreviewWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_ESCAPE) and (ComponentPrinter <> nil) then
  begin
    if IsBuilding then
      ComponentPrinter.AbortBuilding := True;
    if IsPrinting then
      ComponentPrinter.AbortPrinting := True;
  end;
end;

procedure TdxPSPreviewWindow.ScaleFactorChanged;
begin
  inherited;
  UpdateStatusBarsHeight;
end;

procedure TdxPSPreviewWindow.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  case message.Msg of
    DXM_PS_UPDATEMARGINS:
      UpdateMarginBar;
    DXM_PS_UPDATESTATUSPROGRESS:
      begin
        RefreshStatusPanels(psbmNormal);
        UpdateStatusText;
        UpdateControls;
      end;
  end;
end;

procedure TdxPSPreviewWindow.CreateControls;
{$IFDEF OLEDRAGANDDROP}
var
  DropTarget: IDropTarget;
{$ENDIF}
begin
  CreateProgressBar;
  CreateStatusBar;
  CreateExplorerPane;
  CreateExplorerSplitter;
  CreatePreviewPane;
  CreatePreview;
  CreateMarginBar;
  CreateThumbnailsPane;
  CreateThumbnailsSplitter;
  CreateThumbnailsPreview;

  CreateEventSubscribers;
{$IFDEF OLEDRAGANDDROP}
  if Supports(TObject(Self), IDropTarget, DropTarget) then
  begin
    RegisterDragDrop(Preview.Handle, DropTarget);
    CoLockObjectExternal(DropTarget, True, False);
  end;
{$ENDIF}

  UpdateStatusBarsHeight;
  AfterComponentPrinterChanged;
  UpdateSplittersBackgroundColor;
end;

procedure TdxPSPreviewWindow.CreateEventSubscribers;
begin
  FPrintEventsSubscriber := TdxPSPrintReportSubscriber.Create([TdxPSPrintEvent]);
  TdxPSPrintReportSubscriber(FPrintEventsSubscriber).OnEndPrint := EndPrint;
  TdxPSPrintReportSubscriber(FPrintEventsSubscriber).OnProgressPrint := NewPage;
  TdxPSPrintReportSubscriber(FPrintEventsSubscriber).OnStartPrint := StartPrint;

  FBuildEventsSubscriber := TdxPSBuildReportSubscriber.Create([TdxPSBuildEvent]);
  TdxPSBuildReportSubscriber(FBuildEventsSubscriber).OnEndGenerateReport := EndGenerateReport;
  TdxPSBuildReportSubscriber(FBuildEventsSubscriber).OnGenerateReportProgress := GenerateReportProgress;
  TdxPSBuildReportSubscriber(FBuildEventsSubscriber).OnStartGenerateReport := StartGenerateReport;

  FStyleEventsSubscriber := TdxStyleListChangedSubscriber.Create([TdxSMStyleListChangedEvent]);
  TdxStyleListChangedSubscriber(FStyleEventsSubscriber).OnStyleListChanged := StyleListChanged;

  FHFTextEntriesChangedSubscriber := TdxHFTextEntriesChangedSubscriber.Create([TdxHFTextEntriesChangedEvent]);
  TdxHFTextEntriesChangedSubscriber(FHFTextEntriesChangedSubscriber).OnHFTextEntriesChanged := HFTextEntriesChanged;
end;

procedure TdxPSPreviewWindow.CreateExplorerPane;
begin
  FExplorerPane := TdxPSPreviewWindowCustomHost.Create(Self);
  ExplorerPane.Parent := Self;
  ExplorerPane.Align := alLeft;
  ExplorerPane.Name := sdxExplorerPane;
  ExplorerPane.Tag := ScaleFactor.Apply(DefaultExplorerPaneWidth);
  CreateExplorerToolBar;
  CreateExplorerTreeHost;
  InitializeControlLookAndFeel(ExplorerPane);
end;

procedure TdxPSPreviewWindow.CreateExplorerSplitter;
begin
  FExplorerSplitter := CreateSplitter(alLeft, sdxExplorerSplitter,
    ExplorerPane.Left + ExplorerPane.Width + 1, Self, ExplorerPane,
    ExplorerSplitterCanResize);
end;

procedure TdxPSPreviewWindow.CreateExplorerToolBar;

  function CalculateHeight: Integer;
  begin
    Result := MulDiv(cxTextHeight(FExplorerToolBar.Font), 3, 2);
  end;

  procedure DoCreateToolBar;
  begin
    FExplorerToolBar := TdxPSExplorerToolBar.Create(Self);
    FExplorerToolBar.Parent := ExplorerPane;
    FExplorerToolBar.Align := alTop;
    FExplorerToolBar.Caption := ' ' + cxGetResourceString(@sdxExplorerCaption);
    FExplorerToolBar.Height := CalculateHeight;
    FExplorerToolBar.Name := sdxExplorerToolBar;
    FExplorerToolBar.OnResize := ExplorerToolBarResize;
    InitializeControlLookAndFeel(FExplorerToolBar);
  end;

  procedure DoCreateCloseButton;
  const
    ButtonSize = 18;
  begin
    FExplorerCloseButton := TdxPSExplorerCloseButton.Create(Self);
    FExplorerCloseButton.Parent := FExplorerToolBar;
    FExplorerCloseButton.SetBounds(0, (FExplorerToolBar.Height - ButtonSize) div 2, ButtonSize, ButtonSize);
    FExplorerCloseButton.OptionsImage.Layout := blGlyphBottom;
    FExplorerCloseButton.Anchors := [akTop, akRight];
    FExplorerCloseButton.OnClick := ExplorerCloseHandler;
    FExplorerCloseButton.LookAndFeel.MasterLookAndFeel := LookAndFeel;
    DoExplorerButtonsPosUpdate;
  end;

begin
  DoCreateToolBar;
  DoCreateCloseButton;
end;

procedure TdxPSPreviewWindow.CreateExplorerTreeHost;
begin
  FExplorerTreeHost := TdxPSExplorerHostPanel.Create(Self);
  FExplorerTreeHost.Parent := ExplorerPane;
  FExplorerTreeHost.Align := alClient;
  FExplorerTreeHost.Name := sdxExplorerTreeHost;
  FExplorerTreeHost.AlignWithMargins := True;
  InitializeControlLookAndFeel(FExplorerTreeHost);
end;

procedure TdxPSPreviewWindow.CreateMarginBar;
begin
  FMarginBar := TdxPSStatusBar.Create(Self);
  FMarginBar.Parent := FPreviewPane;
  FMarginBar.Align := alTop;
  FMarginBar.AlignWithMargins := True;
  FMarginBar.Margins.Left := 0;
  FMarginBar.Margins.Top := 0;
  FMarginBar.Margins.Right := 0;
  FMarginBar.Name := sdxMarginBar;
  FMarginBar.SizeGrip := False;
  FMarginBar.OnDblClick := MarginBarDblClick;
  FMarginBar.OnPanelGetDrawParams := MarginBarGetDrawParams;
  FMarginBar.LookAndFeel.MasterLookAndFeel := LookAndFeel;

  AddStatusPanel(FMarginBar, taLeftJustify, False, DropAmpersand(cxGetResourceString(@sdxMargins)), 55);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxLeft)), 40);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxTop)), 40);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxRight)), 50);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxBottom)), 60);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxHeader2)), 50);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxFooter2)), 50);
  AddStatusPanel(FMarginBar, taRightJustify, not Flat, '', 70);
  AddStatusPanel(FMarginBar, taRightJustify, False, '',  -1);
end;

procedure TdxPSPreviewWindow.CreatePreview;
begin
  FPreview := TdxPreview.Create(Self);
  FPreview.Parent := FPreviewPane;
  FPreview.Align := alClient;
  if IsWin9X then
    Preview.OptionsBehavior := Preview.OptionsBehavior - [pobThumbTracking];
  Preview.Name := sdxPreviewControl;

  Preview.OnCanShowMarginHint := PreviewCanShowMarginHint;
  Preview.OnDblClick := PreviewDblClick;
  Preview.OnDrawPageContent := PaintPage;
  Preview.OnSelectedPageChanged := PreviewSelectedPageChanged;
  Preview.OnAfterDragMargin := PreviewAfterDragMargin;
  Preview.OnBeforeDragMargin := PreviewBeforeDragMargin;
  Preview.OnDragMargin := PreviewDragMargin;
  Preview.OnMarginChanged := PreviewMarginsChanged;
  Preview.OnZoomFactorChanged := PreviewZoomFactorChanged;
  Preview.OnZoomModeChanged := PreviewZoomModeChanged;

  Preview.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  Preview.PopupMenu := PreviewPopupMenu;

  TdxPreviewPageBackground(Preview.PageBackground).OnApply := FillEffectsApply;
end;

procedure TdxPSPreviewWindow.CreatePreviewPane;
begin
  FPreviewPane := TdxPSPreviewWindowCustomHost.Create(Self);
  FPreviewPane.Parent := Self;
  FPreviewPane.Align := alClient;
  FPreviewPane.AlignWithMargins := True;
  FPreviewPane.Caption := '';
  FPreviewPane.Name := sdxPreviewPane;
  InitializeControlLookAndFeel(FPreviewPane);
end;

procedure TdxPSPreviewWindow.CreateProgressBar;
begin
  FProgressBar := TcxProgressBar.Create(Self);
  FProgressBar.Name := sdxProgressBar;
  FProgressBar.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxPSPreviewWindow.CreateStatusBar;
begin
  FStatusBar := TdxPSStatusBar.Create(Self);
  FStatusBar.AlignWithMargins := True;
  FStatusBar.Margins.Top := 0;
  FStatusBar.Parent := Self;
  FStatusBar.ShowHint := True;
  FStatusBar.SizeGrip := True;
  FStatusBar.Name := sdxStatusBar;
  FStatusBar.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  RefreshStatusPanels(psbmNormal);
end;

function TdxPSPreviewWindow.CreateSplitter(AAlign: TAlign; const AName: string; ALeft: Integer;
  AParent: TWinControl; AResizeControl: TControl; OnCanResizeEvent: TCanResizeEvent): TcxCustomSplitter;
begin
  Result := TcxSplitter.Create(Self);
  Result.ControlStyle := Result.ControlStyle + [csNoDesignVisible];
  Result.Parent := AParent;
  Result.Align := AAlign;
  Result.Width := 5;
  Result.Name := AName;
  Result.Left := ALeft;
  InitializeControlLookAndFeel(Result);
  TcxSplitter(Result).OnCanResize := OnCanResizeEvent;
  TcxSplitter(Result).Control := AResizeControl;
end;

procedure TdxPSPreviewWindow.CreateThumbnailsPane;
begin
  FThumbnailsPane := TdxPSPreviewWindowCustomHost.Create(Self);
  ThumbnailsPane.Parent := PreviewPane;
  ThumbnailsPane.Align := alRight;
  ThumbnailsPane.BevelInner := bvNone;
  ThumbnailsPane.BevelOuter := bvNone;
  ThumbnailsPane.Caption := '';
  ThumbnailsPane.Name := sdxThumbnailsPane;
  ThumbnailsPane.Tag := ScaleFactor.Apply(DefaultThumbnailsPaneWidth);
  InitializeControlLookAndFeel(FPreviewPane);
end;

procedure TdxPSPreviewWindow.CreateThumbnailsSplitter;
begin
  FThumbnailsSplitter := CreateSplitter(alRight, sdxThumbnailsSplitter,
    FThumbnailsPane.Left - 1, PreviewPane, ThumbnailsPane, ThumbnailsSplitterCanResize);
end;

procedure TdxPSPreviewWindow.CreateThumbnailsPreview;
begin
  FThumbnailsPreview := TdxPreview.Create(Self);
  ThumbnailsPreview.Parent := FThumbnailsPane;
  ThumbnailsPreview.Align := alClient;
  ThumbnailsPreview.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  ThumbnailsPreview.MinZoomFactor := 5;
  ThumbnailsPreview.Name := sdxThumbnailsPreviewControl;
  ThumbnailsPreview.OptionsBehavior := ThumbnailsPreview.OptionsBehavior + [pobNonCenterizePages];
  if IsWin9X then
    ThumbnailsPreview.OptionsBehavior := ThumbnailsPreview.OptionsBehavior - [pobThumbTracking];
  ThumbnailsPreview.OptionsView := ThumbnailsPreview.OptionsView - [povMargins];
  ThumbnailsPreview.OptionsZoom := ThumbnailsPreview.OptionsZoom - [pozZoomOnClick];
  ThumbnailsPreview.ZoomFactor := dxThumbnailsZoomFactors[Options.ThumbnailsOptions.Size];

  ThumbnailsPreview.OnSelectedPageChanged := ThumbnailsPreviewSelectedPageChanged;
  ThumbnailsPreview.OnDrawPageContent := PaintThumbnailPage;
end;

procedure TdxPSPreviewWindow.RefreshStatusPanels(AStatusBarMode: TdxPSPreviewWindowStatusBarMode);

  function GetProgressModeCaption: string;
  begin
    if IsBuilding then
      Result := cxGetResourceString(@sdxBuildingReportStatusText)
    else
      if IsPrinting  then
        Result := cxGetResourceString(@sdxPrintingReportStatusText)
      else
        Result := '';
  end;

begin
  StatusBar.BeginUpdate;
  try
    StatusBar.Panels.Clear;
    case AStatusBarMode of
      psbmNormal:
        begin
          AddStatusPanel(StatusBar, taRightJustify, False, DropAmpersand(cxGetResourceString(@sdxPage)) +  ':', 40);
          AddStatusPanel(StatusBar, taRightJustify, not Flat, '', 45);
          AddStatusPanel(StatusBar, taCenter, False, cxGetResourceString(@sdxOf), 30);
          AddStatusPanel(StatusBar, taRightJustify, not Flat, '', 45);
          AddStatusPanel(StatusBar, taRightJustify, False, cxGetResourceString(@sdxPages), 50);
          AddStatusPanel(StatusBar, taLeftJustify, not Flat, '', 4, sbpsSeparator);
          AddStatusPanel(StatusBar, taRightJustify, False, cxGetResourceString(@sdxPaperSize), 80);
          AddStatusPanel(StatusBar, taRightJustify, not Flat, '', 150);
          AddStatusPanel(StatusBar, taRightJustify, False, cxGetResourceString(@sdxStatus), 60);
          AddStatusPanel(StatusBar, taLeftJustify, not Flat, cxGetResourceString(@sdxStatusReady), -1);

          StatusBar.OnDblClick := StatusBarDblClick;
          StatusBar.OnMouseMove := StatusBarMouseMove;
        end;

     psbmProgress:
       begin
         AddStatusPanel(StatusBar, taLeftJustify, False, '', Max(Min(300, Width div 2), 100));
         AddStatusPanel(StatusBar, taLeftJustify, False, GetProgressModeCaption, -1);
         StatusBar.OnDblClick := nil;
         StatusBar.OnMouseMove := nil;
       end;
    end;
  finally
    StatusBar.EndUpdate;
  end;
end;

procedure TdxPSPreviewWindow.BeginUpdate;
begin
  Preview.BeginUpdate;
  Inc(FUpdateCount);
end;

procedure TdxPSPreviewWindow.CancelUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Preview.CancelUpdate;
    Dec(FUpdateCount);
  end;
end;

procedure TdxPSPreviewWindow.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Preview.EndUpdate;
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      UpdateControls;
  end;
end;

function TdxPSPreviewWindow.Locked: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

{$IFDEF OLEDRAGANDDROP}
function TdxPSPreviewWindow.DoCanAccept: Boolean;
begin
  Result := inherited DoCanAccept and IsVisible(pvoReportFileOperations);
end;
{$ENDIF}

procedure TdxPSPreviewWindow.LoadZooms;
begin
  FPredefinedZooms := TStringList.Create;
  with FPredefinedZooms do
  begin
    AddObject('500' + PercentSymbol, TObject(Integer(500)));
    AddObject('200' + PercentSymbol, TObject(Integer(200)));
    AddObject('150' + PercentSymbol, TObject(Integer(150)));
    AddObject('100' + PercentSymbol, TObject(Integer(100)));
    AddObject( '75' + PercentSymbol, TObject(Integer(75)));
    AddObject( '50' + PercentSymbol, TObject(Integer(50)));
    AddObject( '25' + PercentSymbol, TObject(Integer(25)));
    AddObject( '10' + PercentSymbol, TObject(Integer(10)));
    AddObject(cxGetResourceString(@sdxPageWidth), TObject(Integer(-1)));
    AddObject(cxGetResourceString(@sdxWholePage), TObject(Integer(-1)));
    AddObject(cxGetResourceString(@sdxTwoPages), TObject(Integer(-1)));
    AddObject(cxGetResourceString(@sdxFourPages), TObject(Integer(-1)));
    AddObject(cxGetResourceString(@sdxWidenToSourceWidth), TObject(Integer(-1)));
  end;
end;

function TdxPSPreviewWindow.CanClosePreviewWindow: Boolean;
begin
  Result := True;
  if not FReleased and (ComponentPrinter <> nil) then
  begin
    Result := not (IsBuilding or IsPrinting) or Application.Terminated;
    FReleased := not Application.Terminated and (IsBuilding or
      (IsPrinting and MessageQuestion(cxGetResourceString(@sdxAbortPrinting))));
    if FReleased then
      ComponentPrinter.AbortPrinting := True; {1. - Abort}
  end;
end;

function TdxPSPreviewWindow.CreateOptions: TdxPreviewWindowOptions;
begin
  Result := TdxPSPreviewWindowOptions.Create;
end;

function TdxPSPreviewWindow.GetActivePageIndex: Integer;
begin
  if Preview <> nil then
    Result := TdxPreviewAccess(FPreview).SelPageIndex
  else
    Result := 0;
end;

function TdxPSPreviewWindow.CanCallEvents: Boolean;
begin
  Result := not (IsLoading or IsDestroying);
end;

function TdxPSPreviewWindow.GetActualEnableOptions: TdxPreviewEnableOptions;
begin
  Result := [];
  if ReportLink <> nil then
  begin
    Result := Options.EnableOptions;
    if not (rlcPageSetup in ReportLink.Capabilities) then
      Result := Result - [peoCanChangeMargins, peoPageSetup];
    if not ReportLink.CheckToDesign then
      Result := Result - [peoReportDesign];
  end;
end;

function TdxPSPreviewWindow.GetActualVisibleOptions: TdxPreviewVisibleOptions;
begin
  Result := [];
  if ReportLink <> nil then
  begin
    Result := Options.VisibleOptions;
    if not (rlcPageSetup in ReportLink.Capabilities) then
      Result := Result - [pvoPageSetup, pvoPageBackground];
  end;
end;

function TdxPSPreviewWindow.GetBackground: TdxBackground;
begin
  if Preview <> nil then
    Result := Preview.PageBackground
  else
    Result := nil;
end;

function TdxPSPreviewWindow.GetExplorerTree: TCustomdxPSExplorerTreeContainer;
begin
  Result := FExplorerTree;
end;

function TdxPSPreviewWindow.GetState: TdxPSPreviewState;
begin
  Result := FState;
end;

function TdxPSPreviewWindow.GetZoomFactor: Integer;
begin
  if Preview <> nil then
    Result := Preview.ZoomFactor
  else
    Result := 100;
end;

function TdxPSPreviewWindow.GetExplorer: TCustomdxPSExplorer;
begin
  if ComponentPrinter <> nil then
    Result := ComponentPrinter.Explorer
  else
    Result := nil;
end;

function TdxPSPreviewWindow.GetExplorerPaneWidth: Integer;
begin
  if ExplorerPane = nil then
    Result := 0
  else
    Result := ExplorerPane.Width;
end;

function TdxPSPreviewWindow.GetFlat: Boolean;
begin
  Result := (dxPSEngine.DialogsLookAndFeel.Kind in [lfFlat, lfOffice11]) and not
    (dxPSEngine.IsSkinsStyle or dxPSEngine.IsNativeStyle or cxIsVCLThemesEnabled);
end;

function TdxPSPreviewWindow.GetIsExplorerAvailable: Boolean;
begin
  Result := Explorer <> nil;
end;

function TdxPSPreviewWindow.GetOptions: TdxPSPreviewWindowOptions;
begin
  Result := inherited Options as TdxPSPreviewWindowOptions;
end;

function TdxPSPreviewWindow.GetPrinterPage: TdxPrinterPage;
begin
  if ReportLink <> nil then
    Result := ReportLink.RealPrinterPage
  else
    Result := nil;
end;

function TdxPSPreviewWindow.GetHFEditPart: TdxPageTitlePart;
begin
  Result := FHFEditPart;
end;

function TdxPSPreviewWindow.GetPageCount: Integer;
begin
  if Preview <> nil then
    Result := Preview.PageCount
  else
    Result := 0;
end;

function TdxPSPreviewWindow.GetProgressStatusPanel: TdxPSStatusBarPanel;
begin
  Result := StatusBar.Panels.First;
end;

function TdxPSPreviewWindow.GetStatusTextPanel: TdxPSStatusBarPanel;
begin
  Result := StatusBar.Panels.Last;
end;

function TdxPSPreviewWindow.GetThumbnailsPaneWidth: Integer;
begin
  if ThumbnailsPane <> nil then
    Result := ThumbnailsPane.Width
  else
    Result := 0;
end;

procedure TdxPSPreviewWindow.SetPreviewPopupMenu(AValue: TComponent);
begin
  FPreviewPopupMenu := AValue;
  if Assigned(Preview) then
    Preview.PopupMenu := AValue;
end;

procedure TdxPSPreviewWindow.SetActivePageIndex(Value: Integer);
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).SelPageIndex := Value;
end;

procedure TdxPSPreviewWindow.SetState(const Value: TdxPSPreviewState);
begin
  FState := Value;
end;

procedure TdxPSPreviewWindow.SetZoomFactor(Value: Integer);
begin
  if Preview <> nil then
    Preview.ZoomFactor := Value;
end;

procedure TdxPSPreviewWindow.BorderStyleChanged;
begin
  inherited BorderStyleChanged;
  Realign;
end;

procedure TdxPSPreviewWindow.BoundsChanged;
begin
  inherited BoundsChanged;

  if StatusBar <> nil then
  begin
    StatusBar.Calculate;
    StatusBar.Invalidate;
  end;
end;

procedure TdxPSPreviewWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not IsLoading then
    PostMessage(Handle, DXM_RECALCULATE, 0, 0);
end;

procedure TdxPSPreviewWindow.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if IsTransparentBackground then
    inherited EraseBackground(ACanvas, ARect)
  else
    ACanvas.FillRect(ARect, LookAndFeelPainter.DefaultControlColor);
end;

function TdxPSPreviewWindow.HasBackground: Boolean;
begin
  Result := True;
end;

procedure TdxPSPreviewWindow.InitializeControlLookAndFeel(AControl: TControl);
var
  AContainer: IcxLookAndFeelContainer;
begin
  if Supports(AControl, IcxLookAndFeelContainer, AContainer) then
    AContainer.GetLookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxPSPreviewWindow.InitializePreviewWindowLayout;
begin
  PreviewPane.OnResize := DoUpdatePanesState;
  if Assigned(ThumbnailsPane) then
  begin
    ThumbnailsPane.OnResize := DoUpdatePanesState;
    ThumbnailsPane.OnSizeChanging := ThumbnailsPreviewSizeChanging;
  end;
  if Assigned(ExplorerPane) then
  begin
    ExplorerPane.OnResize := DoUpdatePanesState;
    ExplorerPane.OnSizeChanging := ExplorerPaneSizeChanging;
  end;
  PreviewPane.Realign;
end;

function TdxPSPreviewWindow.IsDoubleBufferedNeeded: Boolean;
begin
  Result := not (csPaintCopy in ControlState) and inherited IsDoubleBufferedNeeded;
end;

function TdxPSPreviewWindow.IsTransparentBackground: Boolean;
begin
  Result := Transparent;
end;

procedure TdxPSPreviewWindow.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateSplittersBackgroundColor;
  InvalidateWithChildren;
end;

procedure TdxPSPreviewWindow.Loaded;
begin
  inherited Loaded;
  PostMessage(Handle, DXM_RECALCULATE, 0, 0);
end;

procedure TdxPSPreviewWindow.OptionsChanged;
begin
  if Options.ZoomOnClick then
    Preview.OptionsZoom := Preview.OptionsZoom + [pozZoomOnClick]
  else
    Preview.OptionsZoom := Preview.OptionsZoom - [pozZoomOnClick];

  if Options.SaveZoomPosition then
    Preview.OptionsStore := Preview.OptionsStore + [posZoom]
  else
    Preview.OptionsStore := Preview.OptionsStore - [posZoom];

  if pvoPageMargins in Options.VisibleOptions then
    Preview.OptionsView := Preview.OptionsView + [povMargins]
  else
    Preview.OptionsView := Preview.OptionsView - [povMargins];

  DoExplorerShowToggled(Options.ShowExplorer);
  DoThumbnailsToggleShow(Options.ThumbnailsOptions.Visible);
  UpdateThumbnailsSize;
  UpdateControls;
  FullRefresh;
end;

procedure TdxPSPreviewWindow.UpdateSplittersBackgroundColor;
begin
  if ExplorerSplitter <> nil then
    TcxSplitterAccess(ExplorerSplitter).Color := LookAndFeelPainter.DefaultControlColor;
  if ThumbnailsSplitter <> nil then
    TcxSplitterAccess(ThumbnailsSplitter).Color := LookAndFeelPainter.DefaultControlColor;
end;

procedure TdxPSPreviewWindow.SetBackground(const Value: TdxBackground);
begin
  if Preview <> nil then
    Preview.PageBackground := Value;
  UpdateControls;
end;

procedure TdxPSPreviewWindow.AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand);
begin
  if CanCallEvents then
  begin
    if Assigned(OnAddExplorerCommand) then
      OnAddExplorerCommand(Self, ACommand);
  end;
end;

procedure TdxPSPreviewWindow.DoInitContent;
begin
  if CanCallEvents then
    dxCallNotify(OnInitContent, Self);
end;

procedure TdxPSPreviewWindow.UpdateExplorerContextCommands;
begin
  if CanCallEvents then
    dxCallNotify(OnUpdateExplorerCommands, Self);
end;

procedure TdxPSPreviewWindow.UpdateControlsPosition;
begin
  if IsExplorerAvailable then
    ExplorerSplitter.Left := ExplorerPane.BoundsRect.Right + 1;
  if Assigned(ThumbnailsPane) then
  begin
    ThumbnailsPane.AdjustSize;
    ThumbnailsSplitter.Left := ThumbnailsPane.Left - ThumbnailsSplitter.Width - 1;
  end;
end;

procedure TdxPSPreviewWindow.AfterComponentPrinterChanged;
var
  AExplorerContextCommands: IdxPSExplorerContextCommands;
begin
  inherited AfterComponentPrinterChanged;

  ExplorerPane.Visible := IsExplorerAvailable;
  ExplorerSplitter.Visible := IsExplorerAvailable;
  if IsExplorerAvailable then
  begin
    FExplorerTree := Explorer.CreateTree(Self);
    Explorer.BuildTree(ExplorerTree);
    FExplorerChangeNotifier := TdxPSPreviewExplorerChangeNotifier.Create(Self);
  end;

  if Supports(Explorer, IdxPSExplorerContextCommands, AExplorerContextCommands) then
    AExplorerContextCommands.BuildCommandSet(Self);

  DoExplorerShowToggled(Options.ShowExplorer);
  DoThumbnailsToggleShow(Options.ThumbnailsOptions.Visible);

  InitContent;
  UpdateControls;
  FAreMarginsValid := ValidateMargins;

  if ReportLink <> nil then
    StyleListChanged(ReportLink.StyleManager);
end;

procedure TdxPSPreviewWindow.BeforeComponentPrinterChanged;
begin
  inherited BeforeComponentPrinterChanged;
  FreeAndNil(FExplorerChangeNotifier);
  FreeAndNil(FExplorerTree);
end;

procedure TdxPSPreviewWindow.SetFocusToControl(AControl: TWinControl);
var
  AForm: TCustomForm;
begin
  if Assigned(AControl) and Visible and Enabled then
  begin
    AForm := GetParentForm(Self);
    if (AForm <> nil) and AForm.Visible then
      if AControl.CanFocus then
      try
        AControl.SetFocus;
      except
        // do nothing
      end;
  end;
end;

procedure TdxPSPreviewWindow.SetHFEditPart(const Value: TdxPageTitlePart);
begin
  FHFEditPart := Value;
end;

procedure TdxPSPreviewWindow.SetOptions(AValue: TdxPSPreviewWindowOptions);
begin
  inherited Options := AValue;
end;

procedure TdxPSPreviewWindow.SetExplorerPaneWidth(Value: Integer);
begin
  if ExplorerPane <> nil then
  begin
    if Options.ShowExplorer then
      ExplorerPane.Width := Value
    else
      ExplorerPane.Tag := Value;
  end;
end;

procedure TdxPSPreviewWindow.SetThumbnailsPaneWidth(Value: Integer);
begin
  if ThumbnailsPane <> nil then
  begin
    if Options.ThumbnailsOptions.Visible then
      ThumbnailsPane.Width := Value
    else
      ThumbnailsPane.Tag := Value;
  end;
end;

procedure TdxPSPreviewWindow.SetTransparent(AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    UpdateTransparency;
  end;
end;

procedure TdxPSPreviewWindow.GoToFirstPage;
begin
  if Preview <> nil then
    Preview.GoToFirstPage;
end;

procedure TdxPSPreviewWindow.GoToLastPage;
begin
  if Preview <> nil then
    Preview.GoToLastPage;
end;

procedure TdxPSPreviewWindow.GoToNextPage;
begin
  if Preview <> nil then
    Preview.GoToNextPage;
end;

procedure TdxPSPreviewWindow.GoToPrevPage;
begin
  if Preview <> nil then
    Preview.GoToPrevPage;
end;

procedure TdxPSPreviewWindow.HFTextEntriesChanged;
begin
  if CanCallEvents then
    dxCallNotify(OnHFTextEntriesChanged, Self);
end;

procedure TdxPSPreviewWindow.LoadStrings;
begin
  if IsDesignTime then Exit;

  with FStatusBar do
  begin
    Panels.Items[0].Text := DropAmpersand(cxGetResourceString(@sdxPage)) + ':';
    Panels.Items[2].Text := LowerCase(cxGetResourceString(@sdxOf));
    Panels.Items[4].Text := cxGetResourceString(@sdxPages);
    Panels.Items[6].Text := cxGetResourceString(@sdxPaperSize);
    Panels.Items[8].Text := cxGetResourceString(@sdxStatus);
    Panels.Items[9].Text := cxGetResourceString(@sdxStatusReady);
  end;
end;

procedure TdxPSPreviewWindow.StyleListChanged(Sender: TObject);
begin
  if CanCallEvents then
    dxCallNotify(OnStyleListChanged, Self);
end;

procedure TdxPSPreviewWindow.UpdateStatusBarsHeight;
var
  AHeight: Integer;
begin
  AHeight := CalculateStatusBarHeight;
  FStatusBar.Height := AHeight;
  FMarginBar.Height := AHeight;
end;

function TdxPSPreviewWindow.ValidateMargins: Boolean;
begin
  Result := (PrinterPage = nil) or PrinterPage.ValidateMargins;
end;

function TdxPSPreviewWindow.IdxPSExplorerTreeContainerHost_GetFlat: Boolean;
begin
  Result := Flat;
end;

function TdxPSPreviewWindow.IdxPSExplorerTreeContainerHost_GetReportLink: TBasedxReportLink;
begin
  Result := Self.ReportLink;
end;

procedure TdxPSPreviewWindow.BeforeDestroyReport(AReportLink: TBasedxReportLink);
begin
  if AReportLink = ReportLink then
    FSavePageIndex := ActivePageIndex;
end;

procedure TdxPSPreviewWindow.CurrentLinkChanged(AReportLink: TBasedxReportLink);
begin
  PageParamsChanged(AReportLink);
  CheckRebuildReport;
end;

procedure TdxPSPreviewWindow.ExplorerChanged(AReportLink: TBasedxReportLink);
begin
  BeforeComponentPrinterChanged;
  AfterComponentPrinterChanged;
end;

procedure TdxPSPreviewWindow.LayoutChanged(AReportLink: TBasedxReportLink);
begin
  if AReportLink = ReportLink then
    FullRefresh;
end;

procedure TdxPSPreviewWindow.PageParamsChanged(AReportLink: TBasedxReportLink);
begin
  if AReportLink = ReportLink then
  begin
    InitContent;
    InvalidateContent;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.PrepareBuildReport(AReportLink: TBasedxReportLink);
begin
  // do nothing
end;

procedure TdxPSPreviewWindow.UnprepareBuildReport(AReportLink: TBasedxReportLink);
begin
  if AReportLink = ReportLink then
  begin
    if PageCount <> AReportLink.PageCount then
    begin
      InitContent;
      ActivePageIndex := FSavePageIndex;
      if (PageCount > 0) and (ActivePageIndex = -1) then
        ActivePageIndex := 0;
    end
    else
      InvalidateContent;
  end;
end;

function TdxPSPreviewWindow.GetTreeContainerParent: TcxControl;
begin
  Result := FExplorerTreeHost;
end;

procedure TdxPSPreviewWindow.UpdateState;
var
  Item: TCustomdxPSExplorerItem;
begin
  if not (csDestroying in ComponentState) then
  begin
    Item := ExplorerTree.FocusedItem;
    if Item is TdxPSExplorerFolder then
      ComponentPrinter.Explorer.ActiveFolder := TdxPSExplorerFolder(Item)
    else
      ComponentPrinter.Explorer.ActiveFolder := nil;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.InitContent;

  procedure SetupPageSize(APageSize: TdxPreviewPageSizeOptions; APage: TdxPrinterPage);
  begin
    APageSize.Assigned := True;
    APageSize.Size := APage.PageSizeLoMetric;
    APageSize.Orientation := TdxPreviewPaperOrientation(APage.Orientation);
    APageSize.MinUsefulSize := Point(APage.MinPrintableAreaLoMetric, APage.MinPrintableAreaLoMetric);
    APageSize.MarginsMinValues := APage.MinMarginsLoMetric;
    APageSize.Margins := APage.MarginsLoMetric;
    APageSize.Footer := APage.FooterLoMetric;
    APageSize.Header := APage.HeaderLoMetric;
  end;

  procedure SetupPreview(APreview: TdxPreview; ALink: TBasedxReportLink; APrinterPage: TdxPrinterPage);
  const
    PreviewPaperOrientationMap: array[TdxPrinterOrientation] of TdxPreviewPaperOrientation = (ppoPortrait, ppoLandscape, ppoPortrait);
  var
    APage: TdxPreviewPage;
    APrinterPageForPage: TdxPrinterPage;
    I: Integer;
  begin
    if (ALink <> nil) and ALink.DataProviderPresent then
    begin
      BeginUpdate;
      APreview.BeginUpdate;
      try
        APreview.PageCount := ALink.PageCount;
        SetupPageSize(TdxPreviewAccess(APreview).PageSize, APrinterPage);
        for I := 0 to ALink.PageCount - 1 do
        begin
          APage := TdxPreviewAccess(APreview).Pages[I];
          APrinterPageForPage := ReportLink.Renderer.RenderInfo.GetActualPageRenderInfo(I).PrinterPage;
          if not APrinterPageForPage.IsEqual(APrinterPage) then
          begin
            SetupPageSize(APage.PageSize, APrinterPageForPage);
            APage.Tag := TdxNativeInt(APrinterPageForPage);
          end
          else
            APage.Tag := TdxNativeInt(APrinterPage);
        end;
      finally
        APreview.EndUpdate;
        EndUpdate;
        TdxPreviewAccess(APreview).Calculate(ctHard);
      end;
    end
    else
      APreview.PageCount := 0;
  end;

var
  AIntf: IdxPSPreviewMeasurementUnitsProvider;
begin
  if FThumbnailsPreview <> nil then
    SetupPreview(FThumbnailsPreview, ReportLink, PrinterPage);

  SetupPreview(FPreview, ReportLink, PrinterPage);
  FPreview.MaxZoomFactor := 500;
  FPreview.MinZoomFactor := 10;
  if ReportLink <> nil then
  begin
    FPreview.MeasurementUnits := TdxPreviewMeasurementUnits(PrinterPage.MeasurementUnits);
    if Supports(ReportLink, IdxPSPreviewMeasurementUnitsProvider, AIntf) then
      TdxPreviewAccess(FPreview).DefaultMeasurementUnits := AIntf.GetMeasurementUnits;
    if not PrinterPage.AutoSwapMargins then
      FPreview.OptionsBehavior := FPreview.OptionsBehavior - [pobAutoSwapMargins];
    TdxPreviewAccess(FPreview).PageBackground := PrinterPage.Background;
    TdxPreviewAccess(FPreview).PageBackgroundApplyToEntirePage := TdxReportLinkAccess(ReportLink).IsApplyBackgroundToEntirePage;
  end;

  if not Options.SaveZoomPosition then
    FPreview.ZoomFactor := 100;
  HFTextEntriesChanged;
  InitializePreviewWindowLayout;

  UpdateStatusText;
  UpdateControls;
  DoInitContent;
end;

procedure TdxPSPreviewWindow.InvalidateContent;
begin
  if Preview <> nil then
    Preview.Invalidate;
  if ThumbnailsPreview <> nil then
    ThumbnailsPreview.Invalidate;
end;

procedure TdxPSPreviewWindow.InvalidatePage(APageIndex: Integer);
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).InvalidatePage(APageIndex);
  if ThumbnailsPreview <> nil then
    TdxPreviewAccess(ThumbnailsPreview).InvalidatePage(APageIndex);
end;

procedure TdxPSPreviewWindow.InvalidateAllPages;
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).InvalidatePages;
  if ThumbnailsPreview <> nil then
    TdxPreviewAccess(ThumbnailsPreview).InvalidatePages;
end;

procedure TdxPSPreviewWindow.InvalidatePagesContent;
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).InvalidatePagesContent;
  if ThumbnailsPreview <> nil then
    TdxPreviewAccess(ThumbnailsPreview).InvalidatePagesContent;
end;

procedure TdxPSPreviewWindow.InvalidatePagesHeaderContent;
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).InvalidatePagesHeader;
  if ThumbnailsPreview <> nil then
    TdxPreviewAccess(ThumbnailsPreview).InvalidatePagesHeader;
end;

procedure TdxPSPreviewWindow.InvalidatePagesFooterContent;
begin
  if Preview <> nil then
    TdxPreviewAccess(Preview).InvalidatePagesFooter;
  if ThumbnailsPreview <> nil then
    TdxPreviewAccess(ThumbnailsPreview).InvalidatePagesFooter;
end;

procedure TdxPSPreviewWindow.UpdateControls;
begin
  if not (Locked or IsDestroying or IsLoading) then
  begin
    inherited UpdateControls;

    BeginUpdate;
    try
      if CanChangeMargins then
        Preview.OptionsBehavior := Preview.OptionsBehavior + [pobAllowDragMargins]
      else
        Preview.OptionsBehavior := Preview.OptionsBehavior - [pobAllowDragMargins];

      RefreshStatusBar(pssAll);
      RefreshMarginBar(nil);

      FMarginBar.ShowHint := CanChangeMargins;
    finally
      CancelUpdate;
    end;

    FMarginBar.Visible := Options.ShowMarginBar;
    FStatusBar.Visible := Options.ShowStatusBar;

    DoUpdateControls;
    UpdateControlsPosition;
  end;
end;

// event handlers

procedure TdxPSPreviewWindow.ExplorerCloseHandler(Sender: TObject);
begin
  Options.ShowExplorer := False;
end;

procedure TdxPSPreviewWindow.ExplorerSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
var
  ATempValue: Integer;
begin
  Accept := True;
  ExplorerPaneSizeChanging(Sender, NewSize, ATempValue);
end;

procedure TdxPSPreviewWindow.ExplorerToolBarResize(Sender: TObject);
begin
  DoExplorerButtonsPosUpdate;
end;

procedure TdxPSPreviewWindow.FillEffectsApply(Sender: TObject);
begin
  FPreview.PageBackground := TdxBackground(Sender);
  UpdateControls;
end;

procedure TdxPSPreviewWindow.FullRefresh;

  procedure RefreshPreview(APreview: TdxPreviewAccess);
  begin
    if APreview <> nil then
      APreview.FullRefresh;
  end;

begin
  RefreshPreview(TdxPreviewAccess(Preview));
  RefreshPreview(TdxPreviewAccess(ThumbnailsPreview));
end;

procedure TdxPSPreviewWindow.PreviewMarginsChanged(Sender: TObject; AMargin: TdxPreviewPageMargin);
var
  APage: TdxPrinterPage;
  AValue: Integer;
begin
  if Locked then Exit;

  AValue := AMargin.Value;
  case TdxPreviewAccess(FPreview).ActualMeasurementUnits of
    pmuInches:
      AValue := LoMetricToThousandthsOfInch(AValue);
    pmuMillimeters:
      AValue := LoMetricToThousandthsOfMM(AValue);
  end;

  if Preview.SelPage <> nil then
  begin
    APage := TdxPrinterPage(Preview.SelPage.Tag);
    if APage <> nil then
      if AMargin is TdxPreviewPageMarginLeft then
        APage.Margins.Left := AValue
      else if AMargin is TdxPreviewPageMarginTop then
        APage.Margins.Top := AValue
      else if AMargin is TdxPreviewPageMarginRight then
        APage.Margins.Right := AValue
      else if AMargin is TdxPreviewPageMarginBottom then
        APage.Margins.Bottom := AValue
      else if AMargin is TdxPreviewPageMarginFooter then
        APage.Footer := AValue
      else if AMargin is TdxPreviewPageMarginHeader then
        APage.Header := AValue;
  end;
  DoPreviewMarginChanged(TdxPreview(Sender), AMargin);
end;

procedure TdxPSPreviewWindow.PreviewAfterDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
begin
  DoPreviewAfterDragMargin(TdxPreview(Sender), AMargin);
  UpdateControls;
end;

procedure TdxPSPreviewWindow.PreviewBeforeDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
begin
  DoPreviewBeforeDragMargin(TdxPreview(Sender), AMargin);
  UpdateControls;
end;

procedure TdxPSPreviewWindow.PreviewDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);
begin
  DoPreviewDragMargin(TdxPreview(Sender), AMargin);
end;

procedure TdxPSPreviewWindow.PreviewZoomFactorChanged(Sender: TObject);
begin
  if CanCallEvents then
    dxCallNotify(OnZoomFactorChanged, Self)
end;

procedure TdxPSPreviewWindow.PreviewZoomModeChanged(Sender: TObject);
begin
  if CanCallEvents then
    dxCallNotify(OnZoomModeChanged, Self);
end;

procedure TdxPSPreviewWindow.PreviewCanShowMarginHint(Sender: TObject; var ACanShowHint: Boolean);
begin
  if CanCallEvents then
  begin
    if Assigned(OnCanShowMarginHint) then
      OnCanShowMarginHint(Sender, ACanShowHint);
  end;
end;

procedure TdxPSPreviewWindow.PreviewSelectedPageChanged(Sender: TObject; APageIndex: Integer);
begin
  if ReportLink <> nil then
    ReportLink.CurrentPage := APageIndex + 1;

  UpdateControls;
  FStatusBar.Update;
  if not FLockPageSelection then
  begin
    if (APageIndex = -1) and (ThumbnailsPreview.PageCount <> 0) then
      APageIndex := 0;
    TdxPreviewAccess(ThumbnailsPreview).SelPageIndex := APageIndex;
  end;
end;

procedure TdxPSPreviewWindow.PreviewDblClick(Sender: TObject);
var
  HitTests: TdxPreviewHitTests;
  Pt: TPoint;
begin
  DoPreviewDblClick(TdxPreview(Sender));
  GetCursorPos(Pt);
  Pt := FPreview.ScreenToClient(Pt);
  HitTests := TdxPreviewAccess(FPreview).GetHitInfoAt(Pt.X, Pt.Y);
  if (phtNoWhere in HitTests) and CanDesign then
    DoDesignReport
  else
    if (HitTests * phtMargins <> []) and CanChangeMargins and CanPageSetup then
      DoPageSetupReport(1);
end;

procedure TdxPSPreviewWindow.StatusBarDblClick(Sender: TObject);
var
  APoint: TPoint;
begin
  if CanPageSetup then
  begin
    GetCursorPos(APoint);
    if PtInRect(StatusBar.PanelRect(7), APoint) then
      DoPageSetupReport(0);
  end;
end;

procedure TdxPSPreviewWindow.StatusBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  PtInRect: Boolean = False;
var
  APoint: TPoint;
begin
  if CanPageSetup then
  begin
    APoint := Point(X, Y);
    if Windows.PtInRect(StatusBar.PanelRect(7), APoint) xor PtInRect then
    begin
      PtInRect := not PtInRect;
      Application.CancelHint;
      if PtInRect then
        StatusBar.Hint := cxGetResourceString(@sdxHintDoubleClickForChangePaperSize)
      else
        StatusBar.Hint := '';
    end;
  end;
end;

procedure TdxPSPreviewWindow.ThumbnailsPreviewSelectedPageChanged(
  Sender: TObject; APageIndex: Integer);
begin
  if not FLockPageSelection then
  begin
    FLockPageSelection := True;
    try
      TdxPreviewAccess(Preview).SelPageIndex := APageIndex;
    finally
      FLockPageSelection := False;
    end;
  end;
end;

procedure TdxPSPreviewWindow.ThumbnailsSplitterCanResize(
  Sender: TObject; var NewSize: Integer; var Accept: Boolean);
var
  ATempValue: Integer;
begin
  Accept := True;
  ThumbnailsPreviewSizeChanging(Sender, NewSize, ATempValue);
end;

procedure TdxPSPreviewWindow.MarginBarDblClick(Sender: TObject);
begin
  if CanPageSetup then
    DoPageSetupReport(1);
end;

procedure TdxPSPreviewWindow.MarginBarGetDrawParams(
  ASender: TdxPSCustomStatusBar; APanel: TdxPSStatusBarPanel;
  var ABkgColor, ATextColor: TColor);
begin
  if MarginStatusPanel(TdxPreviewAccess(Preview).DraggingMargin) = APanel then
  begin
    ABkgColor := LookAndFeelPainter.DefaultSelectionColor;
    ATextColor := LookAndFeelPainter.DefaultSelectionTextColor;
  end;
end;

procedure TdxPSPreviewWindow.DoShowFormatDateTimeDlg;
begin
  if ReportLink.ShowDateTimeFormatsDlg then
  begin
    TdxPreviewAccess(Preview).InvalidatePagesHeader;
    TdxPreviewAccess(Preview).InvalidatePagesFooter;
  end;
end;

procedure TdxPSPreviewWindow.DoShowFormatPageNumbersDlg;
begin
  if ReportLink.ShowPageNumberFormatsDlg then
  begin
    TdxPreviewAccess(Preview).InvalidatePagesHeader;
    TdxPreviewAccess(Preview).InvalidatePagesFooter;
  end;
end;

procedure TdxPSPreviewWindow.DoShowZoomDlg;
begin
  dxZoomDlg(Preview);
end;

procedure TdxPSPreviewWindow.DoShowPageBackgroundDlg(const Pt: TPoint);
begin
  if CanShowPageBackgroundDialog then
  begin
    if dxChooseBackgroundDlg(Preview.PageBackground, Pt, dxDefaultBackgroundDlgData) then
      DoSyncPrintingPageBackground;
  end;
end;

procedure TdxPSPreviewWindow.DoShowMultiplySelectPagesDlg(
  AImageList: TCustomImageList; AImageIndex: Integer; const Pt: TPoint; AYShift: Integer);
var
  AColCount: Integer;
  AMaxColCount: Integer;
  AMaxRowCount: Integer;
  AOrigin: TPoint;
  APageSize: TPoint;
  ARowCount: Integer;
begin
  AOrigin := Pt;
  Inc(AOrigin.Y, AYShift);

  APageSize := cxPointScale(TdxPreviewAccess(FPreview).PageSize.ActualSizeInPixels, FPreview.MinZoomFactor, 100);
  AMaxColCount := Max(1, Floor(cxRectWidth(TdxPreviewAccess(FPreview).GetPagesArea) / (TdxPreviewAccess(FPreview).Indent + APageSize.X)));
  AMaxRowCount := Max(1, Floor(cxRectHeight(TdxPreviewAccess(FPreview).GetPagesArea) / (TdxPreviewAccess(FPreview).Indent + APageSize.Y)));

  AColCount := Min(AMaxColCount, 3);
  ARowCount := Min(AMaxRowCount, 2);

  if dxChooseMultiplePages(AImageList, AImageIndex, AOrigin, AYShift, AMaxColCount, AMaxRowCount, AColCount, ARowCount) then
    DoSetupZoomFactor(0, AColCount, ARowCount, pzmPages)
end;

procedure TdxPSPreviewWindow.DoShowOptionsDlg;
var
  AData: TdxPreviewOptionsDlgData;
begin
  if CanShowOptionsDialog then
  begin
    FillChar(AData, SizeOf(TdxPreviewOptionsDlgData), 0);
    AData.MarginColor := Preview.MarginColor;
    AData.MeasurementUnits := TdxMeasurementUnits(Preview.MeasurementUnits);
    AData.ShowMarginsHintWhileDragging := pohShowOnDrag in Preview.OptionsHint;
    AData.ShowMarginHints := pohShowForMargins in Preview.OptionsHint;
    AData.ShowMargins := povMargins in Preview.OptionsView;
    AData.ZoomOnMouseRoll := pozZoomOnMouseRoll in Preview.OptionsZoom;
    AData.ZoomStep := Preview.ZoomStep;

    if dxShowPSPreviewOptionsDlg(AData) then
      SavePreferences(AData);
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.DoSyncPrintingPageBackground;
begin
  if PrinterPage <> nil then
    PrinterPage.Background := Preview.PageBackground;
end;

procedure TdxPSPreviewWindow.DoUpdateControls;
begin
  if CanCallEvents then
    dxCallNotify(OnUpdateControls, Self);
end;

procedure TdxPSPreviewWindow.DoThumbnailsToggleShow(Value: Boolean);
begin
  if Value <> (ThumbnailsPane.Width <> 0) then
  begin
    if Value then
    begin
      ThumbnailsPane.Width := ThumbnailsPane.Tag;
      SetFocusToControl(ThumbnailsPreview);
    end
    else
    begin
      ThumbnailsPane.Width := 0;
      ThumbnailsPane.Left := ThumbnailsPane.Parent.Width;
      SetFocusToControl(Preview);
    end;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.DoUnloadReportLinkData;
begin
  ReportLink.DataSource := rldsComponent;
end;

procedure TdxPSPreviewWindow.DoUpdatePanesState(Sender: TObject);
begin
  if Assigned(ExplorerPane) then
  begin
    if ExplorerPane.Width > 0 then
      ExplorerPane.Tag := ExplorerPane.Width;
    Options.ShowExplorer := ExplorerPane.Width > 0;
  end;
  if Assigned(ThumbnailsPane) then
  begin
    if ThumbnailsPane.Width > 0 then
      ThumbnailsPane.Tag := ThumbnailsPane.Width;
    Options.ThumbnailsOptions.Visible := ThumbnailsPane.Width > 0;
  end;
  UpdateControlsPosition;
end;

procedure TdxPSPreviewWindow.ExplorerPaneSizeChanging(Sender: TObject; var AWidth, AHeight: Integer);
begin
  AWidth := Min(ClientWidth - MinPreviewSize.X, AWidth);
end;

procedure TdxPSPreviewWindow.ThumbnailsPreviewSizeChanging(Sender: TObject; var AWidth, AHeight: Integer);
begin
  AWidth := Min(PreviewPane.ClientWidth - MinPreviewSize.X, AWidth);
end;

procedure TdxPSPreviewWindow.UpdateThumbnailsSize;
begin
  ThumbnailsPreview.ZoomFactor := dxPSCore.dxThumbnailsZoomFactors[Options.ThumbnailsOptions.Size];
end;

procedure TdxPSPreviewWindow.DoDesignReport;
begin
  if CanDesign then
  begin
    ReportLink.DesignReport;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.DoLoadReportLinkDataFromFile;
var
  Dialog: TdxPSOpenReportDialog;
begin
  if (ReportLink <> nil) and ReportLink.CanLoadData then
  begin
    Dialog := TdxPSOpenReportDialog.Create(nil);
    with Dialog do
    try
      if Execute then
        ReportLink.LoadDataFromFile(FileName);
    finally
      Free;
    end;
  end;
end;

procedure TdxPSPreviewWindow.DoPageSetupReport(APageIndex: Integer = 0);
var
  PreviewBtnClicked, PrintBtnClicked: Boolean;
begin
  if CanPageSetup then
  begin
    if ComponentPrinter.PageSetupEx(APageIndex, False, CanPrintDialog, PreviewBtnClicked, PrintBtnClicked) then
    begin
      InitContent;
      UpdateControls;
    end;
    InvalidatePagesContent;
    if PrintBtnClicked then
      DoPrintReport(True);
  end;
end;

procedure TdxPSPreviewWindow.DoPrintReport(AShowDialog: Boolean);
begin
  if CanPrint then
  begin
    ComponentPrinter.Print(AShowDialog, nil, nil);
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.DoSaveReportLinkDataToFile;
var
  ADialog: TdxPSSaveReportDialog;
begin
  if ReportLink <> nil then
  begin
    ADialog := TdxPSSaveReportDialog.Create(nil);
    try
      ADialog.FileName := ReportLink.GetNewReportStorageName;
      if ADialog.Execute then
      begin
        ADialog.FileName := ChangeFileExt(ADialog.FileName, '.' + dxPSCore.dxPSReportFileShortExtension);
        ReportLink.SaveDataToFile(ADialog.FileName);
      end;
    finally
      ADialog.Free;
    end;
  end;
end;

function TdxPSPreviewWindow.CanDesign: Boolean;
begin
  Result := IsVisible(pvoReportDesign) and IsEnabled(peoReportDesign) and
    (ReportLink <> nil) and ReportLink.CheckToDesign and not IsPrinting and not IsBuilding;
end;

function TdxPSPreviewWindow.CanPrint: Boolean;
begin
  Result := IsVisible(pvoPrint) and IsEnabled(peoPrint) and (ReportLink <> nil) and
    (ReportLink.PageCount > 0) and (dxPrintDevice.CurrentDevice <> '') and not IsPrinting;
end;

function TdxPSPreviewWindow.CanPrintDialog: Boolean;
begin
  Result := IsVisible(pvoPrint) and IsEnabled(peoPrint) and
    IsVisible(pvoPrintDialog) and IsEnabled(peoPrintDialog) and
    (ReportLink <> nil) and (ReportLink.PageCount > 0) and not IsPrinting;
end;

function TdxPSPreviewWindow.CanPrintStyle: Boolean;
begin
  Result := (ReportLink <> nil) and (ReportLink.StyleManager <> nil) and IsVisible(pvoPrintStyles);
end;

function TdxPSPreviewWindow.CanRebuild: Boolean;
begin
  Result := (ReportLink <> nil) and ReportLink.DataProviderPresent and (ComponentPrinter.State * [cpsBuilding, cpsPrinting] = [])
end;

function TdxPSPreviewWindow.CanExport: Boolean;
begin
  Result := (ReportLink <> nil) and (ReportLink.PageCount > 0) and not IsPrinting;
end;

function TdxPSPreviewWindow.CanLoadReport: Boolean;
begin
  if IsExplorerAvailable then
    Result := (ExplorerTree <> nil) and ExplorerTree.CanLoadSelectedItemData and not IsBuilding
  else
    Result := IsVisible(pvoReportFileOperations) and (ReportLink <> nil) and ReportLink.CanLoadData and not IsBuilding;
end;

function TdxPSPreviewWindow.CanPageSetup: Boolean;
begin
  Result := IsVisible(pvoPageSetup) and IsEnabled(peoPageSetup) and (ComponentPrinter <> nil) and not IsPrinting;
end;

function TdxPSPreviewWindow.CanChangeMargins: Boolean;
begin
  Result := IsEnabled(peoCanChangeMargins) and not IsPrinting and not IsBuilding;
end;

function TdxPSPreviewWindow.CanSaveReport: Boolean;
begin
  if (ComponentPrinter <> nil) and not ComponentPrinter.IsExplorerMode then
  begin
    if IsExplorerAvailable and (ReportLink = nil) then
      Result := (ExplorerTree <> nil) and ExplorerTree.CanCreateItem
    else
      Result := (ReportLink <> nil) and IsVisible(pvoReportFileOperations) and ReportLink.CanSaveData;
  end
  else
    Result := False;
end;

function TdxPSPreviewWindow.CanShowOptionsDialog: Boolean;
begin
  Result := IsVisible(pvoPreferences) and IsEnabled(peoPreferences);
end;

function TdxPSPreviewWindow.CanShowPageBackgroundDialog: Boolean;
begin
  Result := IsVisible(pvoPageBackground) and IsEnabled(peoPageBackground);
end;

function TdxPSPreviewWindow.CanUnloadReport: Boolean;
begin
  if IsExplorerAvailable then
    Result := (ExplorerTree <> nil) and ExplorerTree.CanUnloadItemData
  else
    Result := IsVisible(pvoReportFileOperations) and (ReportLink <> nil) and ReportLink.CanUnloadData;
end;

function TdxPSPreviewWindow.IsCommandLoadReportVisible: Boolean;
begin
  Result := IsExplorerAvailable or IsVisible(pvoReportFileOperations);
end;

function TdxPSPreviewWindow.IsCommandSaveReportVisible: Boolean;
begin
  Result := IsExplorerAvailable or IsVisible(pvoReportFileOperations);
end;

function TdxPSPreviewWindow.IsCommandUnloadReportVisible: Boolean;
begin
  Result := IsExplorerAvailable or IsVisible(pvoReportFileOperations);
end;

function TdxPSPreviewWindow.IsEnabled(AOption: TdxPreviewEnableOption): Boolean;
begin
  Result := AOption in GetActualEnableOptions;
end;

function TdxPSPreviewWindow.IsVisible(AOption: TdxPreviewVisibleOption): Boolean;
begin
  Result := AOption in ActualVisibleOptions;
end;

function TdxPSPreviewWindow.IsAutoHFTextEntriesAvailable: Boolean;
begin
  Result := (ReportLink <> nil) and (ReportLink.StyleManager <> nil);
end;

function TdxPSPreviewWindow.IsBuilding: Boolean;
begin
  Result := (ComponentPrinter <> nil) and (cpsBuilding in ComponentPrinter.State);
end;

function TdxPSPreviewWindow.IsPrinting: Boolean;
begin
  Result := ((ComponentPrinter <> nil) and (cpsPrinting in ComponentPrinter.State)) or dxPrintDevice.Printing;
end;

function TdxPSPreviewWindow.IsProgressState: Boolean;
begin
  Result := (ComponentPrinter <> nil) and (ComponentPrinter.State * [cpsBuilding, cpsPrinting] <> []);
end;

procedure TdxPSPreviewWindow.InvalidatePagesHeadersOrFooters;
begin
  case State of
    prsEditHeaders:
      TdxPreviewAccess(FPreview).InvalidatePagesHeader;
    prsEditFooters:
      TdxPreviewAccess(FPreview).InvalidatePagesFooter;
  end;
end;

procedure TdxPSPreviewWindow.RefreshStatusBar(AStatusSections: TfmPreviewStatusSections);
begin
  if not IsProgressState then
  begin
    if ssCurrentPage in AStatusSections then
      SectionStatusPanelSetText(ssCurrentPage, IfThen(ActivePageIndex >= 0, IntToStr(ActivePageIndex + 1), ''));
    if ssPageCount in AStatusSections then
      SectionStatusPanelSetText(ssPageCount, IfThen(PageCount > 0, IntToStr(PageCount), ''));
    if ssPaperSize in AStatusSections then
      SectionStatusPanelSetText(ssPaperSize, TdxPreviewAccess(FPreview).PageSizeToString);
    if ssStatus in AStatusSections then
      StatusBar.InvalidatePanel(StatusBar.Panels.Count - 1);
    UpdateStatusBarPanelWidths(StatusBar);
  end;
end;

procedure TdxPSPreviewWindow.RefreshMarginBar(AMargin: TdxPreviewPageMargin);
begin
  with TdxPreviewAccess(FPreview) do
  begin
    if Assigned(AMargin) and (AMargin.DraggingValue >= 0) then
      MarginStatusPanel(AMargin).Text := MarginValueToString(AMargin.DraggingValue)
    else
    begin
      MarginStatusPanel(Margins.Left).Text := MarginValueToString(Margins.Left.Value);
      MarginStatusPanel(Margins.Top).Text := MarginValueToString(Margins.Top.Value);
      MarginStatusPanel(Margins.Right).Text := MarginValueToString(Margins.Right.Value);
      MarginStatusPanel(Margins.Bottom).Text := MarginValueToString(Margins.Bottom.Value);
      MarginStatusPanel(Margins.Header).Text := MarginValueToString(Margins.Header.Value);
      MarginStatusPanel(Margins.Footer).Text := MarginValueToString(Margins.Footer.Value);
    end;
  end;
  MarginBar.Invalidate;
end;

function TdxPSPreviewWindow.MarginStatusPanel(AMargin: TdxPreviewPageMargin): TdxPSStatusBarPanel;
//pmLeft, pmTop, pmRight, pmBottom, pmHeader, pmFooter
var
  AIndex: Integer;
begin
  AIndex := -1;
  if AMargin is TdxPreviewPageMarginLeft  then AIndex := 2;
  if AMargin is TdxPreviewPageMarginRight then AIndex := 6;
  if AMargin is TdxPreviewPageMarginTop   then AIndex := 4;
  if AMargin is TdxPreviewPageMarginBottom then AIndex := 8;
  if AMargin is TdxPreviewPageMarginFooter then AIndex := 12;
  if AMargin is TdxPreviewPageMarginHeader then AIndex := 10;

  if AIndex >= 0 then
    Result := FMarginBar.Panels.Items[AIndex]
  else
    Result := nil;
end;

function TdxPSPreviewWindow.SectionStatusPanel(AStatusSection: TfmPreviewStatusSection): TdxPSStatusBarPanel;
const
  Indexes: array [TfmPreviewStatusSection] of Integer = (1, 3, 7, 9);
begin
  if Indexes[AStatusSection] < FStatusBar.Panels.Count then
    Result := FStatusBar.Panels.Items[Indexes[AStatusSection]]
  else
    Result := nil;
end;

procedure TdxPSPreviewWindow.SectionStatusPanelSetText(AStatusSection: TfmPreviewStatusSection; const AText: string);
var
  APanel: TdxPSStatusBarPanel;
begin
  APanel := SectionStatusPanel(AStatusSection);
  if APanel <> nil then
    APanel.Text := AText;
end;

procedure TdxPSPreviewWindow.DoPreviewMarginChanged(APreview: TdxPreview; AMargin: TdxPreviewPageMargin);
begin
  if TdxPreviewAccess(FPreview).DraggingMargin = nil then
    RefreshMarginBar(AMargin);
end;

procedure TdxPSPreviewWindow.DoLoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  if CanCallEvents then
  begin
    if Assigned(OnLoadProperties) then
      OnLoadProperties(Self, AIniFile, ASectionName);
  end;
end;

procedure TdxPSPreviewWindow.DoSaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  if CanCallEvents then
  begin
    if Assigned(OnSaveProperties) then
      OnSaveProperties(Self, AIniFile, ASectionName);
  end;
end;

procedure TdxPSPreviewWindow.DoPreviewAfterDragMargin(
  APreview: TdxPreview; AMargin: TdxPreviewPageMargin);
var
  MarginsValid: Boolean;
begin
  RefreshMarginBar(AMargin);
  MarginsValid := ValidateMargins;
  if (MarginsValid <> FAreMarginsValid) and not MarginsValid then
    SysUtils.Beep;
  FAreMarginsValid := MarginsValid;
  UpdateStatusText;
  RefreshStatusBar([ssStatus]);
end;

procedure TdxPSPreviewWindow.DoPreviewBeforeDragMargin(APreview: TdxPreview;
  AMargin: TdxPreviewPageMargin);
begin
  RefreshMarginBar(AMargin);
end;

procedure TdxPSPreviewWindow.DoPreviewDragMargin(
  APreview: TdxPreview; AMargin: TdxPreviewPageMargin);
begin
  RefreshMarginBar(AMargin);
end;

procedure TdxPSPreviewWindow.DoPreviewDblClick(APreview: TdxPreview);
begin
  if CanCallEvents then
    dxCallNotify(OnPreviewDblClick, Self);
end;

procedure TdxPSPreviewWindow.DoSetupZoomFactor(AZoomFactor, APageXCount,
  APageYCount: Integer; AZoomMode: TdxPreviewZoomMode);
begin
  FPreview.ZoomMode := AZoomMode;
  case FPreview.ZoomMode of
    pzmNone:
      FPreview.ZoomFactor := AZoomFactor;
    pzmPages:
      TdxPreviewAccess(FPreview).SetPageXYCount(APageXCount, APageYCount);
  end;
  UpdateControls;
end;

procedure TdxPSPreviewWindow.DoExplorerButtonsPosUpdate;
begin
  FExplorerCloseButton.Left := FExplorerToolBar.Width - FExplorerCloseButton.Width - 1;
end;

procedure TdxPSPreviewWindow.DoExplorerCreateNewFolder;
begin
  if (ExplorerTree <> nil) and ExplorerTree.CanCreateFolder then
    Explorer.CreateNewFolder(ExplorerTree.CreationParent);
end;

procedure TdxPSPreviewWindow.DoExplorerCreateNewItem;
begin
  if CanSaveReport then
  begin
    if not IsExplorerAvailable then
      DoSaveReportLinkDataToFile
    else
      if Assigned(ExplorerTree) then
      begin
        ExplorerTree.CreateItem;
        Options.ShowExplorer := True;
      end;
  end;
end;

procedure TdxPSPreviewWindow.DoExplorerDeleteItem;
begin
  if ExplorerTree <> nil then
    ExplorerTree.DeleteSelection;
end;

function TdxPSPreviewWindow.DoExplorerItemShowPropertySheets: Boolean;
begin
  Result := (ExplorerTree <> nil) and ExplorerTree.ShowSelectedItemPropertySheets;
end;

procedure TdxPSPreviewWindow.DoExplorerLoadItemData;
begin
  if CanLoadReport then
  begin
    if IsExplorerAvailable then
      ExplorerTree.LoadSelectedItemData
    else
      DoLoadReportLinkDataFromFile;
  end;
end;

procedure TdxPSPreviewWindow.DoExplorerRenameItem;
begin
  ExplorerTree.BeginEdit;
end;

procedure TdxPSPreviewWindow.DoExplorerShowToggled(Value: Boolean);
begin
  if IsExplorerAvailable and (Value <> (ExplorerPane.Width <> 0)) then
  begin
    if Value then
    begin
      if ExplorerTree <> nil then
        ExplorerTree.SetFocus;
      ExplorerPane.Width := ExplorerPane.Tag;
    end
    else
    begin
      ExplorerPane.Width := 0;
      ExplorerPane.Left := 0;
      SetFocusToControl(Preview);
    end;
    UpdateControlsPosition;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.DoExplorerUnloadItemData;
begin
  if CanUnloadReport then
  begin
    if IsExplorerAvailable then
      ExplorerTree.UnloadItemData
    else
      DoUnloadReportLinkData;
  end;
end;

procedure TdxPSPreviewWindow.DoFormatFootnotes;
begin
  if ReportLink <> nil then
    ReportLink.ShowFootnotesPropertiesDlg;
end;

procedure TdxPSPreviewWindow.DoFormatTitle;
begin
  if ReportLink <> nil then
    ReportLink.ShowTitlePropertiesDlg;
end;

procedure TdxPSPreviewWindow.DoInsertHF(const S: string);
var
  Strings: TStrings;
begin
  case State of
    prsEditHeaders:
      Strings := PrinterPage.PageHeader.Titles[HFEditPart];
    prsEditFooters:
      Strings := PrinterPage.PageFooter.Titles[HFEditPart];
  else
    Exit;
  end;
  Strings.Text := Strings.Text + S;
  InvalidatePagesHeadersOrFooters;
end;

procedure TdxPSPreviewWindow.DoInvokeHelp;
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext);
end;

procedure TdxPSPreviewWindow.DoClearHF;
begin
  if PrinterPage <> nil then
  begin
    case State of
      prsEditHeaders:
        PrinterPage.PageHeader.Titles[HFEditPart].Text := '';
      prsEditFooters:
        PrinterPage.PageFooter.Titles[HFEditPart].Text := '';
     end;
    InvalidatePagesHeadersOrFooters;
  end;
end;

procedure TdxPSPreviewWindow.DoShowHFBackgroundDlg(const Pt: TPoint);
var
  Background: TdxBackground;
begin
  case State of
    prsEditHeaders:
      Background := PrinterPage.PageHeader.Background;
    prsEditFooters:
      Background := PrinterPage.PageFooter.Background;
  else
    Exit;
  end;
  if dxChooseBackgroundDlg(Background, Pt, dxDefaultBackgroundDlgData) then
    InvalidatePagesHeadersOrFooters;
end;

procedure TdxPSPreviewWindow.DoShowPageMargins(Value: Boolean);
begin
  if Value then
    Options.VisibleOptions := Options.VisibleOptions + [pvoPageMargins]
  else
    Options.VisibleOptions := Options.VisibleOptions - [pvoPageMargins];
end;

procedure TdxPSPreviewWindow.PrepareProgress;
begin
  RefreshStatusPanels(psbmProgress);
  ProgressBarShow;
  FLastOpCompleted := 0;
  FCurrentProgressValue := 0;
end;

function TdxPSPreviewWindow.ProgressBarGetMaxValue: Integer;
begin
  if cpsPrinting in ComponentPrinter.State then
    Result := FFullPageCount
  else
    Result := 100;
end;

procedure TdxPSPreviewWindow.ProgressBarHide;
begin
  ProgressBar.Parent := nil;
end;

procedure TdxPSPreviewWindow.ProgressBarPlace;
var
  R: TRect;
begin
  R := cxRectInflate(StatusBar.PanelRect(ProgressStatusPanel.Index), -1);
  if Flat then
    Dec(R.Bottom);
  ProgressBar.BoundsRect := R;
end;

procedure TdxPSPreviewWindow.ProgressBarRefresh;
begin
  ProgressBar.Position := FCurrentProgressValue;
  ProgressBar.Update;
end;

procedure TdxPSPreviewWindow.ProgressBarShow;
begin
  ProgressBar.Position := 0;
  ProgressBar.Properties.Max := ProgressBarGetMaxValue;
  ProgressBar.Parent := StatusBar;
  ProgressBarPlace;
  Update;
end;

procedure TdxPSPreviewWindow.UnprepareProgress;
begin
  ProgressBarHide;
  PostMessage(Handle, DXM_PS_UPDATESTATUSPROGRESS, 0, 0);
end;

procedure TdxPSPreviewWindow.GenerateReportProgress(Sender: TObject;
  AReportLink: TBasedxReportLink; APercentDone: Double {mask : '##0.00'});
begin
  if Sender = ComponentPrinter then
  begin
    FCurrentProgressValue := Trunc(APercentDone);
    ProgressBarRefresh;
  end;
end;

procedure TdxPSPreviewWindow.EndGenerateReport(Sender: TObject; AReportLink: TBasedxReportLink);
begin
  if Sender = ComponentPrinter then
  begin
    UnprepareProgress;
    if TdxPreviewAccess(Preview).SelPageIndex > ReportLink.PageCount - 1 then
      TdxPreviewAccess(Preview).SelPageIndex := ReportLink.PageCount - 1;
  end;
end;

procedure TdxPSPreviewWindow.StartGenerateReport(Sender: TObject; AReportLink: TBasedxReportLink);
begin
  if Sender = ComponentPrinter then
  begin
    Preview.PageCount := 0;
    Preview.Update;

    ThumbnailsPreview.PageCount := 0;
    ThumbnailsPreview.Update;

    PrepareProgress;
    UpdateControls;
  end;
end;

procedure TdxPSPreviewWindow.EndPrint(Sender: TObject; AReportLink: TBasedxReportLink);
begin
  if Sender <> ComponentPrinter then Exit;
  UnprepareProgress;
  UpdateControls;
end;

procedure TdxPSPreviewWindow.NewPage(Sender: TObject; AReportLink: TBasedxReportLink;
  APageIndex: Integer);
begin
  if Sender <> ComponentPrinter then Exit;
  FCurrentProgressValue := APageIndex;
  ProgressBarRefresh;
end;

procedure TdxPSPreviewWindow.StartPrint(Sender: TObject; AReportLink: TBasedxReportLink;
  FullPageCount: Integer);
begin
  if Sender <> ComponentPrinter then Exit;
  FFullPageCount := FullPageCount;
  UpdateControls;
  PrepareProgress;
end;

procedure TdxPSPreviewWindow.UpdateMarginBar;
begin
  MarginBar.Refresh;
end;

procedure TdxPSPreviewWindow.SetZoomFactorByText(const AText: string);
var
  V, I, PageXCount, PageYCount: Integer;
begin
  I := FPredefinedZooms.IndexOf(AText);
  if I > -1 then
    if I < PredefinedZoomValueCount then
    begin
      FPreview.ZoomMode := pzmNone;
      FPreview.ZoomFactor := Integer(FPredefinedZooms.Objects[I]);
    end
    else
    begin
      if I = PredefinedZoomValueCount then
        FPreview.ZoomMode := pzmPageWidth
      else
        FPreview.ZoomMode := pzmPages;

      case I - PredefinedZoomValueCount of
        1: TdxPreviewAccess(FPreview).SetPageXYCount(1, 1);
        2: TdxPreviewAccess(FPreview).SetPageXYCount(2, 1);
        3: TdxPreviewAccess(FPreview).SetPageXYCount(2, 2);
        4: begin
             ReportLink.GetPageColRowCount(PageXCount, PageYCount);
             TdxPreviewAccess(FPreview).SetPageXYCount(PageXCount, 1);
           end;
      end;
    end
  else
  begin
    try
      V := StrToInt(DropPercentChar(AText));
    except
      try
        V := Round(StrToFloat(DropPercentChar(AText)));
      except
        V := FLastValidZoomFactor;
      end;
    end;
    FPreview.ZoomFactor := V;
  end;
  FLastValidZoomFactor := FPreview.ZoomFactor;
end;

procedure TdxPSPreviewWindow.UpdateStatusBarPanelWidths(AStatusBar: TdxPSCustomStatusBar);

  procedure UpdateStatusBarPanelWidth(APanel: TdxPSStatusBarPanel);
  begin
    if APanel.Width >= 0 then
      APanel.Width := Max(APanel.Width, MeasureStatusPanelWidth(AStatusBar, APanel.Text));
  end;

var
  I: Integer;
begin
  cxScreenCanvas.Font := AStatusBar.Font;
  try
    for I := 0 to AStatusBar.Panels.Count - 1 do
      UpdateStatusBarPanelWidth(AStatusBar.Panels.Items[I]);
  finally
    cxScreenCanvas.Dormant;
  end;
end;

procedure TdxPSPreviewWindow.UpdateTransparency;
begin
  if MarginBar <> nil then
    MarginBar.Transparent := Transparent;
  if StatusBar <> nil then
    StatusBar.Transparent := Transparent;
  if PreviewPane <> nil then
    PreviewPane.Transparent := Transparent;
  if ThumbnailsPreview <> nil then
    ThumbnailsPreview.Transparent := Transparent;
  if ThumbnailsPane <> nil then
    ThumbnailsPane.Transparent := Transparent;
  if Preview <> nil then
    Preview.Transparent := Transparent;

  InvalidateWithChildren;
end;

procedure TdxPSPreviewWindow.SavePreferences(AData: TdxPreviewOptionsDlgData);
begin
  if AData.ShowMarginHints then
    Preview.OptionsHint := Preview.OptionsHint + [pohShowForMargins]
  else
    Preview.OptionsHint := Preview.OptionsHint - [pohShowForMargins];

  if AData.ShowMarginsHintWhileDragging then
    Preview.OptionsHint := Preview.OptionsHint + [pohShowOnDrag]
  else
    Preview.OptionsHint := Preview.OptionsHint - [pohShowOnDrag];

  if AData.ZoomOnMouseRoll then
    Preview.OptionsZoom := Preview.OptionsZoom + [pozZoomOnMouseRoll]
  else
    Preview.OptionsZoom := Preview.OptionsZoom - [pozZoomOnMouseRoll];

  if AData.ShowMargins then
    Options.VisibleOptions := Options.VisibleOptions + [pvoPageMargins]
  else
    Options.VisibleOptions := Options.VisibleOptions - [pvoPageMargins];

  Preview.ZoomStep := AData.ZoomStep;
  Preview.MarginColor := AData.MarginColor;
  Preview.MeasurementUnits := TdxPreviewMeasurementUnits(AData.MeasurementUnits);
  PrinterPage.MeasurementUnits := AData.MeasurementUnits;
end;

procedure TdxPSPreviewWindow.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  DoLoadFromIniFile(AIniFile, ASectionName);
  LoadProperties(AIniFile, ASectionName);
  InitContent;
end;

procedure TdxPSPreviewWindow.SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  DoSaveToIniFile(AIniFile, ASectionName);
  SaveProperties(AIniFile, ASectionName);
end;

procedure TdxPSPreviewWindow.LoadProperties(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  BeginUpdate;
  try
    TdxPreviewAccess(Preview).LoadFromIniFile(AIniFile, dxValidatePath(ASectionName) + sdxPreviewControl);
    Options.ShowMarginBar := AIniFile.ReadBool(ASectionName, sdxShowMarginBar, Options.ShowMarginBar);
    Options.ShowStatusBar := AIniFile.ReadBool(ASectionName, sdxShowStatusBar, Options.ShowStatusBar);

    ExplorerPaneWidth := ScaleFactor.Apply(AIniFile.ReadInteger(ASectionName, sdxExplorerPaneWidth, DefaultExplorerPaneWidth));
    Options.ShowExplorer := AIniFile.ReadInteger(ASectionName, sdxExplorerVisibilityState, 1) > 1;

    ThumbnailsPaneWidth := ScaleFactor.Apply(AIniFile.ReadInteger(ASectionName, sdxThumbnailsPaneWidth, DefaultThumbnailsPaneWidth));
    Options.ThumbnailsOptions.Visible := AIniFile.ReadInteger(ASectionName, sdxThumbnailsVisibilityState, 1) > 1;
  finally
    EndUpdate;
  end;
end;

procedure TdxPSPreviewWindow.SaveProperties(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  TdxPreviewAccess(Preview).SaveToIniFile(AIniFile, dxValidatePath(ASectionName) + sdxPreviewControl);
  AIniFile.WriteBool(ASectionName, sdxShowMarginBar, Options.ShowMarginBar);
  AIniFile.WriteBool(ASectionName, sdxShowStatusBar, Options.ShowStatusBar);

  AIniFile.WriteInteger(ASectionName, sdxExplorerVisibilityState, Ord(Options.ShowExplorer) + 1);
  if ExplorerPaneWidth <> 0 then
    AIniFile.WriteInteger(ASectionName, sdxExplorerPaneWidth, ScaleFactor.Revert(ExplorerPaneWidth));

  AIniFile.WriteInteger(ASectionName, sdxThumbnailsVisibilityState, Ord(Options.ThumbnailsOptions.Visible) + 1);
  if ThumbnailsPaneWidth <> 0 then
    AIniFile.WriteInteger(ASectionName, sdxThumbnailsPaneWidth, ScaleFactor.Revert(ThumbnailsPaneWidth));
end;

procedure TdxPSPreviewWindow.UpdateStatusText;

  function GetStatusText: string;
  begin
    if ValidateMargins then
      Result := cxGetResourceString(@sdxStatusReady)
    else
      Result := cxGetResourceString(@sdxOutsideMargins);
  end;

begin
  StatusTextPanel.Text := GetStatusText;
  StatusBar.Update;
end;

procedure TdxPSPreviewWindow.DXMRecalculate(var Message: TMessage);
begin
  inherited;
  CurrentLinkChanged(ReportLink);
  RefreshStatusPanels(psbmNormal);
  UpdateControls;
  FullRefresh;
end;

procedure TdxPSPreviewWindow.WMAppCommand(var Message: TMessage);
begin
  with Message do
    case dxPSGlbl.GET_APPCOMMAND_LPARAM(lParam) of
      APPCOMMAND_BROWSER_BACKWARD:
        begin
          GotoPrevPage;
          Result := 1;
        end;

      APPCOMMAND_BROWSER_FORWARD:
        begin
          GotoNextPage;
          Result := 1;
        end;

      APPCOMMAND_BROWSER_HOME:
        begin
          GotoFirstPage;
          Result := 1;
        end;

      APPCOMMAND_HELP:
        begin
          DoInvokeHelp;
          Result := 1;
        end;

      APPCOMMAND_OPEN:
        begin
          DoExplorerLoadItemData;
          Result := 1;
        end;

      APPCOMMAND_CLOSE:
        begin
          GetParentForm(Self).Close;
          Result := 1;
        end;

      APPCOMMAND_SAVE:
        begin
          Result := Ord(CanSaveReport);
          if Result = 1 then
            DoExplorerCreateNewItem;
        end;

      APPCOMMAND_PRINT:
        begin
          DoPrintReport(True);
          Result := 1;
        end;
    end;
  inherited;
end;

procedure TdxPSPreviewWindow.WMSetFocus(var Message: TWMSetFocus);
begin
  if Assigned(Preview) and Preview.HandleAllocated then
    Windows.SetFocus(Preview.Handle)
  else
    inherited;
end;

procedure TdxPSPreviewWindow.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  with Message.MinMaxInfo.ptMinTrackSize do
  begin
    X := 360;
    Y := 300;
  end;
end;

procedure TdxPSPreviewWindow.WMSettingChange(var Message: TWMSettingChange);
begin
  inherited;
  with Message do
    if (Flag = 0) and (Section = 'intl') then
    begin
      RefreshMarginBar(nil);
      RefreshStatusBar([ssPaperSize]);
    end;
end;

{ TdxPSCustomStatusBar }

constructor TdxPSCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanels := TdxPSStatusBarPanels.Create(Self);
  DoubleBuffered := True;
  Align := alBottom;
end;

destructor TdxPSCustomStatusBar.Destroy;
begin
  FreeAndNil(FPanels);
  inherited Destroy;
end;

procedure TdxPSCustomStatusBar.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then
    UpdateStateChanged(False);
end;

procedure TdxPSCustomStatusBar.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateStateChanged(True);
end;

function TdxPSCustomStatusBar.CanShowSizeGrip: Boolean;
begin
  Result := SizeGrip and TcxSizeGrip.IsAvailable(Self);
end;

procedure TdxPSCustomStatusBar.Calculate;
var
  I: Integer;
  AViewInfo: TdxPSStatusBarViewInfo;
begin
  AViewInfo := CreateViewInfo;
  try
    CalculateSizeGripBounds(AViewInfo);
    for I := 0 to Panels.Count - 1 do
      Panels.Items[I].Calculate(AViewInfo);
  finally
    AViewInfo.Free;
  end;
end;

procedure TdxPSCustomStatusBar.CalculateSizeGripBounds(AViewInfo: TdxPSStatusBarViewInfo);
begin
  FSizeGripRect := cxEmptyRect;
  if SizeGrip and CanShowSizeGrip then
  begin
    FSizeGripRect := cxRectContent(AViewInfo.Bounds, AViewInfo.BorderWidths);
    FSizeGripRect.Left := FSizeGripRect.Right - AViewInfo.SizeGripSize.cx;
    if cxRectHeight(FSizeGripRect) > AViewInfo.SizeGripSize.cy then
      FSizeGripRect := cxRectCenter(FSizeGripRect, AViewInfo.SizeGripSize);
  end;
end;

procedure TdxPSCustomStatusBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style or CS_HREDRAW or CS_VREDRAW;
end;

function TdxPSCustomStatusBar.CreateViewInfo: TdxPSStatusBarViewInfo;
var
  ABorderSize: Integer;
begin
  ABorderSize := ScaleFactor.Apply(LookAndFeelPainter.BorderSize);
  Result := TdxPSStatusBarViewInfo.Create;
  Result.BorderWidths := cxRect(ABorderSize, ABorderSize, ABorderSize, ABorderSize);
  Result.Bounds := cxRectInflate(ClientBounds, -cxTextOffset);
  Result.TextIndent := ScaleFactor.Apply(cxTextOffset);
  Result.SizeGripSize := LookAndFeelPainter.ScaledSizeGripSize(ScaleFactor);
  Result.SeparatorSize := ScaleFactor.Apply(LookAndFeelPainter.LabelLineHeight);
end;

procedure TdxPSCustomStatusBar.DoGetPanelDrawParams(APanel: TdxPSStatusBarPanel; var ABkgColor, ATextColor: TColor);
begin
  if Assigned(OnPanelGetDrawParams) then
    OnPanelGetDrawParams(Self, APanel, ABkgColor, ATextColor);
end;

procedure TdxPSCustomStatusBar.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.DrawPanelBackground(ACanvas, Self, R, Color);
  LookAndFeelPainter.DrawPanelContent(ACanvas, R, False);
end;

procedure TdxPSCustomStatusBar.DrawItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel);
begin
  case APanel.Style of
    sbpsPanel:
      DrawPanelItem(ACanvas, APanel);
    sbpsSeparator:
      DrawSepartorItem(ACanvas, APanel);
  end;
end;

procedure TdxPSCustomStatusBar.DrawPanelItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel);
var
  ATextColor, ABkgColor: TColor;
begin
  ABkgColor := clNone;
  ATextColor := GetTextColor;
  DoGetPanelDrawParams(APanel, ABkgColor, ATextColor);
  ACanvas.FillRect(APanel.Bounds, ABkgColor);
  if APanel.ShowBorders then
    LookAndFeelPainter.DrawBorder(ACanvas, APanel.Bounds);
  DrawPanelItemText(ACanvas, APanel, ATextColor);
end;

procedure TdxPSCustomStatusBar.DrawSepartorItem(ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel);
begin
  LookAndFeelPainter.DrawLabelLine(ACanvas, APanel.Bounds, clDefault, clDefault, True);
end;

procedure TdxPSCustomStatusBar.DrawPanelItemText(
  ACanvas: TcxCanvas; APanel: TdxPSStatusBarPanel; ATextColor: TColor);
const
  TextAlignFlagsMap: array[TAlignment] of Integer = (cxAlignLeft, cxAlignRight, cxAlignHCenter);
begin
  ACanvas.Font := Font;
  ACanvas.Font.Color := ATextColor;
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(APanel.Text, APanel.TextRect,
    cxAlignVCenter or cxShowEndEllipsis or TextAlignFlagsMap[APanel.Alignment]);
end;

procedure TdxPSCustomStatusBar.DrawSizeGrip(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.DrawSizeGrip(ACanvas, R, clNone);
end;

function TdxPSCustomStatusBar.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

procedure TdxPSCustomStatusBar.InvalidatePanel(AIndex: Integer);
begin
  InvalidateRect(PanelRect(AIndex), True);
  Update;
end;

procedure TdxPSCustomStatusBar.DoPaint;
var
  I: Integer;
begin
  for I := 0 to Panels.Count - 1 do
    DrawItem(Canvas, Panels.Items[I]);
  DrawSizeGrip(Canvas, SizeGripRect);
end;

function TdxPSCustomStatusBar.PanelFromPoint(APoint: TPoint; var APanel: TdxPSStatusBarPanel): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Panels.Count - 1 do
  begin
    Result := PtInRect(PanelRect(I), APoint);
    if Result then
    begin
      APanel := Panels.Items[I];
      Break;
    end;
  end;
end;

function TdxPSCustomStatusBar.PanelRect(AIndex: Integer): TRect;
begin
  Result := Panels.Items[AIndex].Bounds;
end;

procedure TdxPSCustomStatusBar.Recalculate;
begin
  if FUpdateCount = 0 then
  begin
    Calculate;
    Invalidate;
  end;
end;

procedure TdxPSCustomStatusBar.UpdateStateChanged(AUnlocked: Boolean);
begin
  if HandleAllocated then
    SendMessage(Handle, WM_SETREDRAW, Ord(AUnlocked), Ord(AUnlocked));
  if AUnlocked then
    Recalculate;
end;

procedure TdxPSCustomStatusBar.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not IsDesigning then
  begin
    if PtInRect(SizeGripRect, ScreenToClient(SmallPointToPoint(Message.Pos))) then
      Message.Result := HTBOTTOMRIGHT;
  end;
end;

function TdxPSCustomStatusBar.GetTextColor: TColor;
begin
  if (Font.Color <> clWindowText) or (LookAndFeelPainter.PanelTextColor = clDefault) then
    Result := Font.Color
  else
    Result := LookAndFeelPainter.PanelTextColor;
end;

procedure TdxPSCustomStatusBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HandleAllocated then
    Recalculate;
end;

procedure TdxPSCustomStatusBar.SetSizeGrip(AValue: Boolean);
begin
  if AValue <> FSizeGrip then
  begin
    FSizeGrip := AValue;
    Recalculate;
  end;
end;

{ TdxPSStatusBarPanels }

constructor TdxPSStatusBarPanels.Create(AStatusBar: TdxPSCustomStatusBar);
begin
  inherited Create(TdxPSStatusBarPanel);
  FStatusBar := AStatusBar;
end;

function TdxPSStatusBarPanels.Add: TdxPSStatusBarPanel;
begin
  Result := TdxPSStatusBarPanel(inherited Add);
end;

function TdxPSStatusBarPanels.First: TdxPSStatusBarPanel;
begin
  Result := Items[0];
end;

function TdxPSStatusBarPanels.Last: TdxPSStatusBarPanel;
begin
  Result := Items[Count - 1];
end;

function TdxPSStatusBarPanels.GetItem(Index: Integer): TdxPSStatusBarPanel;
begin
  Result := TdxPSStatusBarPanel(inherited Items[Index]);
end;

function TdxPSStatusBarPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure TdxPSStatusBarPanels.Update(Item: TCollectionItem);
begin
  if Item = nil then
    FStatusBar.Recalculate
  else
    FStatusBar.InvalidatePanel(Item.Index);
end;

{ TdxPSStatusBarPanel }

constructor TdxPSStatusBarPanel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAlignment := taLeftJustify;
  FShowBorders := True;
  FWidth := 100;
end;

procedure TdxPSStatusBarPanel.Calculate(AViewInfo: TdxPSStatusBarViewInfo);

  function CalculateBounds(const R: TRect): TRect;
  begin
    Result := R;
    if Style = sbpsSeparator then
      FWidth := AViewInfo.SeparatorSize;
    if Width >= 0 then
      Result.Right := Result.Left + ScaleFactor.Apply(Width);
  end;

  function CalculateTextRect: TRect;
  begin
    Result := cxRectInflate(Bounds, -AViewInfo.TextIndent);
    if ShowBorders then
      Result := cxRectContent(Result, AViewInfo.BorderWidths);
  end;

begin
  FBounds := CalculateBounds(AViewInfo.Bounds);
  FTextRect := CalculateTextRect;
  AViewInfo.Bounds.Left := Bounds.Right + 1;
end;

function TdxPSStatusBarPanel.GetScaleFactor: TdxScaleFactor;
begin
  Result := (Collection as TdxPSStatusBarPanels).StatusBar.ScaleFactor
end;

procedure TdxPSStatusBarPanel.SetAlignment(AValue: TAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    Changed(False);
  end;
end;

procedure TdxPSStatusBarPanel.SetShowBorders(AValue: Boolean);
begin
  if AValue <> FShowBorders then
  begin
    FShowBorders := AValue;
    Changed(False);
  end;
end;

procedure TdxPSStatusBarPanel.SetStyle(AStyle: TdxPSStatusBarPanelStyle);
begin
  if AStyle <> FStyle then
  begin
    FStyle := AStyle;
    Changed(True);
  end;
end;

procedure TdxPSStatusBarPanel.SetText(const AText: string);
begin
  if AText <> FText then
  begin
    FText := AText;
    Changed(False);
  end;
end;

procedure TdxPSStatusBarPanel.SetWidth(AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    Changed(True);
  end;
end;

initialization
  ClosePaneGlyph := CreateGlyphBitmap(CloseGlyphIndex, 0, 0, 7);

finalization
  FreeAndNil(ClosePaneGlyph);
end.

