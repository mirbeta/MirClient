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

unit dxRichEdit.Control.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

{.$DEFINE DXLOGGING}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections, Controls,
  StdCtrls, Forms, ActiveX, dxCoreClasses, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses,
  dxMessages, dxGDIPlusClasses, cxLookAndFeelPainters, dxSkinsCore, dxSkinInfo,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.Layout.Painters,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.MenuBuilder,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Export.Core,
  dxRichEdit.InnerControl,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Scroll,
  dxRichEdit.Types,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.ServiceManager,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo;

{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF}

type
  TdxRichEditControlBase = class;

  { TdxRichEditControlPainter }

  TdxRichEditControlPainter = class
  strict private
    FControl: TdxRichEditControlBase;
    FDeferredDraws: TList<TdxDrawDelegate>;
    function GetLookAndFeel: TcxLookAndFeel;
  protected
    function GetCaret: TdxCaret; virtual;
    function GetDragCaret: TdxDragCaret; virtual;
    function GetActiveView: TdxRichEditView; virtual;
    procedure RenderDocument(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    procedure RenderDocumentDecorators(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    procedure DrawCaretCore(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); overload; virtual;
    procedure DrawDragCaretCore(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    procedure DrawCaretCore(ACaret: TdxCaret; AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); overload; virtual;
    function GetActualBounds(AGraphics: TdxGraphics; APage: TdxPageViewInfo; const ABounds: TRect): TRect; virtual;
    procedure DrawReversibleFrameCore(AGraphics: TdxGraphics; const ABounds: TRect; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleHorizontalLineCore(AGraphics: TdxGraphics; Y: Integer; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleVerticalLineCore(AGraphics: TdxGraphics; X: Integer; APage: TdxPageViewInfo); virtual;

    property Caret: TdxCaret read GetCaret;
    property DragCaret: TdxDragCaret read GetDragCaret;
    property ActiveView: TdxRichEditView read GetActiveView;
  public
    constructor Create(AControl: TdxRichEditControlBase);
    destructor Destroy; override;

    procedure Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    procedure DrawCaret; overload; virtual;
    procedure DrawDragCaret; virtual;
    procedure DrawCaret(const ADrawCaret: TdxDrawDelegate); overload;
    procedure RegisterDeferredDraw(const ADraw: TdxDrawDelegate);
    procedure DeferredDraw(APage: TdxPageViewInfo; ADrawAtPage: TdxDrawAtPageDelegate);
    procedure DeferredDrawReversibleFrame(const ABounds: TRect; APage: TdxPageViewInfo);
    procedure DrawReversibleHorizontalLine(Y: Integer; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleVerticalLine(X: Integer; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleCore(APage: TdxPageViewInfo; const ADraw: TdxDrawDelegate); virtual;
    procedure PerformRenderingInUnits(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter; const ADraw: TdxDrawDelegate); virtual;
    procedure PerformRenderingInPixels(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter; const ADraw: TdxDrawDelegate); virtual;

    property Control: TdxRichEditControlBase read FControl;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
  end;

  { TdxEmptyRichEditControlPainter }

  TdxEmptyRichEditControlPainter = class(TdxRichEditControlPainter)
  public
    procedure Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); override;
    procedure DrawCaret; override;
  end;

  { TdxRichEditCustomInnerControl }

  TdxRichEditCustomInnerControl = class abstract(TdxInnerRichEditControl)
  protected
    function CreateKeyboardController: TdxCustomKeyboardController; override;
    function CreateNativeDocument: IdxRichEditDocument; override;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument; override;
    function CreateMouseController: TdxRichEditCustomMouseController; override;
    function GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions; override;
  public
    function CreateMailMergeOptions: IdxRichEditMailMergeOptions; override;
  end;

  { TdxRichEditControlBase }

  TdxRichEditControlBase = class abstract(TdxVCLControl,
    IdxBatchUpdateable,
    IDropTarget,
    IDropSource,
    IdxInnerRichEditDocumentServerOwner,
    IdxInnerRichEditControlOwner,
    IdxRichEditControl,
    IdxRichEditDocumentLayoutProvider)
  strict private
    class var FSystemCaretBitmap: TBitmap;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FAutoSizeMode: TdxRichEditAutoSizeMode;
    FCaret: TdxCaret;
    FCaretTimer: TcxTimer;
    FCommentPadding: TdxCommentPadding;
    FContentSize: TSize;
    FControlCreated: Boolean;
    FDragCaret: TdxDragCaret;
    FFlushPendingTextInputTimer: TcxTimer;
    FInnerControl: TdxInnerRichEditControl;
    FInsideResize: Boolean;
    FIsInsideRefresh: Boolean;
    FIsUpdateAutoSizeMode: Boolean;
    FLastSize: TSize;
    FShowCaretInReadOnly: Boolean;
    FWantReturns: Boolean;
    FWantTabs: Boolean;

    FPainter: TdxRichEditControlPainter;
    FViewPainter: TdxRichEditViewPainter;
    FBackgroundPainter: TdxRichEditViewBackgroundPainter;

    FHorizontalScrollBar: TdxOfficeScrollbar;
    FVerticalScrollBar: TdxOfficeScrollbar;
    FVerticalScrollBarUpdateTimer: TcxTimer;

    FOnAfterExport: TNotifyEvent;
    FBuiltInPopupMenu: TdxRichEditCustomPopupMenu;
    FOnActiveViewChanged: TNotifyEvent;
    FOnDocumentLoaded: TNotifyEvent;
    FOnEmptyDocumentCreated: TNotifyEvent;
    FOnHyperlinkClick: TdxHyperlinkClickEvent;
    FOnDocumentClosing: TCloseQueryEvent;
    FOnContentChanged: TNotifyEvent;
    FOnModifiedChanged: TNotifyEvent;
    FOnPageBackgroundChanged: TNotifyEvent;
    FOnPlainTextChanged: TNotifyEvent;
    FOnReadOnlyChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnShortCut: TdxRichEditShortCutEvent;
    FOnZoomChanged: TNotifyEvent;
    FOnDocumentProtectionChanged: TNotifyEvent;

    FOnCalculateDocumentVariable: TdxCalculateDocumentVariableEvent;
    FOnCustomizeMergeFields: TdxCustomizeMergeFieldsEvent;
    FOnMailMergeFinished: TdxMailMergeFinishedEvent;
    FOnMailMergeRecordFinished: TdxMailMergeRecordFinishedEvent;
    FOnMailMergeRecordStarted: TdxMailMergeRecordStartedEvent;
    FOnMailMergeStarted: TdxMailMergeStartedEvent;
    FOnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEvent;
    FLastValidClientRectangle: TRect;
    FSizeGripBounds: TRect;
    FViewBounds: TRect;

    FDragObject: IDataObject;
    FSkinInfo: TdxSkinInfo;
    function CalculateInitialClientBounds: TRect;
    function GetActiveView: TdxRichEditView;
    function GetActiveViewType: TdxRichEditViewType;
    function GetBackgroundThreadUIUpdater: TdxBackgroundThreadUIUpdater;
    function GetLayoutUnit: TdxDocumentLayoutUnit;
    function GetDocument: IdxRichEditDocument;
    function GetDocumentModel: TdxDocumentModel;
    function IdxRichEditControl.GetDocumentLayout = InternalGetDocumentLayout;
    function InternalGetDocumentLayout: TdxRichEditDocumentLayout;
    function GetMeasurementUnit: TdxMeasurementUnit;
    function GetReadOnly: Boolean;
    function GetViews: TdxRichEditCustomViewRepository;
    function IsActiveViewTypeStored: Boolean;
    procedure SetActiveViewType(const Value: TdxRichEditViewType);
    procedure SetAutoSizeMode(const Value: TdxRichEditAutoSizeMode);
    procedure SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
    procedure SetViews(const Value: TdxRichEditCustomViewRepository);
    procedure CaretTimerHandler(Sender: TObject);
    function GetControlDeferredChanges: TdxRichEditControlDeferredChanges;
    function GetMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetModified: Boolean;
    function GetOptions: TdxRichEditControlCustomOptions;
    procedure SetOptions(const Value: TdxRichEditControlCustomOptions);
    procedure SetShowCaretInReadOnly(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetMeasurementUnit(const Value: TdxMeasurementUnit);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetWantReturns(const Value: Boolean);
    procedure SetWantTabs(const Value: Boolean);
    procedure CMNCSizeChanged(var Message: TMessage); message DXM_NCSIZECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    function GetKeyboardController: TdxCustomKeyboardController;
    function GetMouseController: TdxRichEditCustomMouseController;
  protected
    FClientBounds: TRect;

    function IDropTarget.DragEnter = OleDragEnter;
    function OleDragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function IDropTarget.DragOver = OleDragOver;
    function OleDragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function IDropTarget.DragLeave = OleDragLeave;
    function OleDragLeave: HResult; stdcall;
    function IDropTarget.Drop = OleDrop;
    function OleDrop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;

    function IDropSource.QueryContinueDrag = OleQueryContinueDrag;
    function OleQueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function IDropSource.GiveFeedback = OleGiveFeedback;
    function OleGiveFeedback(dwEffect: Longint): HResult; stdcall;

    function CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer; overload; virtual; abstract;
    function CreateMeasurementAndDrawingStrategy(ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy; virtual;
    function GetLookAndFeel: TcxLookAndFeel;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase}; virtual; abstract;
    procedure RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions); virtual;

    procedure ActivateViewPlatformSpecific(AView: TdxRichEditView); virtual;
    function CreateViewRepository: TdxRichEditCustomViewRepository; virtual; abstract;
    procedure IdxInnerRichEditControlOwner.OnResize = BoundsChanged;
    function IsHandleCreated: Boolean;
    procedure OnOptionsChangedPlatformSpecific(E: TdxOptionChangedEventArgs); virtual;
    procedure OnResizeCore; overload;
    procedure OnResizeCore(AEnsureCaretVisibleOnResize: Boolean); overload;
    procedure OnResizeCore(AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean); overload;
    procedure OnZoomFactorChangingPlatformSpecific;
    procedure IdxInnerRichEditControlOwner.OnActiveViewBackColorChanged = OnBackColorChanged;

    function GetLayoutCalculationMode: TdxCalculationModeType;
    function GetDocumentLayout: TdxDocumentLayout;
    function GetDocumentLayoutAsync: TdxDocumentLayout;
    procedure PerformPageSecondaryFormatting(APage: TdxPage);
    procedure AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddDocumentFormatted(const AHandler: TdxEvent);
    procedure RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemoveDocumentFormatted(const AHandler: TdxEvent);
    procedure CreateDragCaret;
    procedure DestroyDragCaret;
    procedure DrawDragCaret;
    function DoDragDrop(const AData: IDataObject; AllowedEffects: TdxDragDropEffects): TdxDragDropEffects;
    function GetBackgroundPainter: TdxRichEditViewBackgroundPainter;
    function GetDragCaret: TdxDragCaret;
    function GetCommentPadding: TdxCommentPadding;
    function GetOvertype: Boolean;
    function GetViewBounds: TRect;
    function IsHyperlinkActive: Boolean;
    procedure SetOvertype(const Value: Boolean);
    procedure UpdateControlAutoSize;
    function ShowWarningMessage(const AMessage: string): TModalResult;
    function ShowErrorMessage(const AMessage: string): TModalResult;
    procedure ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI; const ACallback: TdxShowColumnsSetupFormCallback;
      ACallbackData: TObject); virtual;
    procedure ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload; virtual;
    procedure ShowEditStyleForm(ACharacterSourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload; virtual;
    procedure ShowTableStyleForm(AStyle: TdxTableStyle); virtual;
    procedure ShowInsertTableForm(const AParameters: TdxCreateTableParameters; const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject); virtual;
    procedure ShowInsertTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject); virtual;
    procedure ShowDeleteTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject); virtual;
    procedure ShowSplitTableCellsForm(const AParameters: TdxSplitTableCellsParameters; const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject); virtual;
    procedure ShowRangeEditingPermissionsForm; virtual;
    procedure ShowDocumentEncryptQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); virtual;
    procedure ShowDocumentProtectionQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); virtual;
    procedure ShowDocumentProtectionQueryPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); virtual;
    procedure ShowBookmarkForm; virtual;
    procedure ShowReplaceForm; virtual;
    procedure ShowSearchForm; virtual;
    procedure ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo; const ATitle: string; const ACallback: TdxShowHyperlinkFormCallback); virtual;
    procedure ShowSymbolForm(const ASymbolProperties: TdxSymbolProperties; const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject); virtual;
    procedure ShowNumberingListForm(AParagraphs: TdxParagraphList; const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject); virtual;
    procedure ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties; const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject); virtual;
    procedure ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties; const ACallback: TdxShowFontFormCallback; ACallbackData: TObject); virtual;
    procedure ShowTabsForm(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; const ACallback: TdxShowTabsFormCallback;
      ACallbackData: TObject); virtual;
    procedure ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection); virtual;
    procedure ShowFloatingInlineObjectLayoutOptionsForm(const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
      const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject); virtual;
    procedure ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject = nil); virtual;
    procedure ShowLineNumberingForm(AProperties: TdxLineNumberingInfo; const ACallback: TdxShowLineNumberingFormCallback;
      ACallbackData: TObject); virtual;
    procedure ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
      ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage); virtual;
    procedure ShowInsertMergeFieldForm; virtual;
    procedure ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const ACallback: TdxShowMergeDatabaseRecordsFormCallback); virtual;
    procedure ShowTOCForm(AField: TdxField); virtual;

    function GetScaleFactor: TdxScaleFactor;

    function GetSkinLeftMargin: Integer; virtual;
    function GetSkinRightMargin: Integer; virtual;
    function GetSkinTopMargin: Integer; virtual;
    function GetSkinBottomMargin: Integer; virtual;
    function GetUseSkinMargins: Boolean;
    function CalcBestSize(AWidth, AHeight: Integer; AFixedWidth: Boolean): TSize;
    function CalcViewBestSize(AFixedWidth: Boolean): TSize; virtual;
    function CalculateActualViewBounds(const APreviousViewBounds: TRect): TRect;
    function CalculateHorizontalScrollbarHeight: Integer;
    function CalculateVerticalScrollbarWidth: Integer;
    procedure HideCaret;
    procedure ShowCaret;
    procedure OnViewPaddingChanged;
    procedure OnUpdateUI;
    function GetCanShowNumberingListForm: Boolean; virtual;
    function GetCursor: TCursor;
    function GetInnerControl: IdxInnerControl;
    function GetRichEditControl: IdxRichEditControl;
    function CreateRichEditViewVerticalScrollController(ARichEditView: TdxRichEditView): TdxRichEditViewVerticalScrollController;
    function CreateRichEditViewHorizontalScrollController(ARichEditView: TdxRichEditView): TdxRichEditViewHorizontalScrollController;
    function CreatePlatformSpecificScrollBarAdapter: IdxPlatformSpecificScrollBarAdapter;
    procedure OnDeferredResizeCore; virtual;
    procedure RedrawEnsureSecondaryFormattingComplete(Action: TdxRefreshAction); overload; virtual;
    procedure SetCursor(Value: TCursor);
    procedure UpdateUIFromBackgroundThread(const AMethod: TdxAction);
    function UseStandardDragDropMode: Boolean;

    function CreatePainter: TdxRichEditControlPainter; virtual;
    procedure CustomRefresh;
    procedure Redraw; overload;
    procedure Redraw(AAfterEndUpdate: Boolean); overload; virtual;
    procedure Redraw(AAction: TdxRefreshAction); overload;
    procedure RedrawAfterEndUpdate;
    procedure RedrawEnsureSecondaryFormattingComplete; overload; virtual;

    procedure ApplyChangesCorePlatformSpecific(AChangeActions: TdxDocumentModelChangeActions); virtual;


    procedure ApplyFontAndForeColor; virtual;
    procedure OnBackColorChanged; virtual;

    function CreateInnerControl: TdxInnerRichEditControl; virtual; abstract;

    function CreateViewPainter(AView: TdxRichEditView): TdxRichEditViewPainter; virtual;
    function CreateBackgroundPainter(AView: TdxRichEditView): TdxRichEditViewBackgroundPainter; virtual;
    procedure RecreateBackgroundPainter(AView: TdxRichEditView);
    procedure RecreateViewPainter(AView: TdxRichEditView);

    procedure DoBeforeExport(Sender: TObject; Args: TdxBeforeExportEventArgs); virtual;
    procedure DoAfterExport(Sender: TObject); virtual;
    procedure DoPlainTextChanged(Sender: TObject); virtual;
    procedure DoDocumentClosing(Sender: TObject; var CanClose: Boolean); virtual;
    procedure DoContentChanged(Sender: TObject); virtual;
    procedure DoHyperlinkClick(Sender: TObject; const Args: TdxHyperlinkClickEventArgs); virtual;
    procedure DoModifiedChanged(Sender: TObject); virtual;
    procedure DoReadOnlyChanged(Sender: TObject); virtual;
    procedure DoReadOnlyChangedPlatformSpecific; virtual;
    procedure DoActiveViewChanged(Sender: TObject); virtual;
    procedure DoPageBackgroundChanged(Sender: TObject); virtual;
    procedure DoEmptyDocumentCreated(Sender: TObject); virtual;
    procedure DoDocumentLoaded(Sender: TObject); virtual;
    procedure DoDocumentProtectionChanged(Sender: TObject); virtual;
    procedure DoSelectionChanged(Sender: TObject); virtual;
    procedure DoShortCut(Args: TdxRichEditShortCutEventArgs); virtual;
    procedure DoZoomChanged(Sender: TObject); virtual;

    procedure DoCalculateDocumentVariable(Sender: TObject; Args: TdxCalculateDocumentVariableEventArgs);
    procedure DoCustomizeMergeFields(Sender: TObject; const Args: TdxCustomizeMergeFieldsEventArgs);
    procedure DoMailMergeFinished(Sender: TObject; const Args: TdxMailMergeFinishedEventArgs);
    procedure DoMailMergeRecordFinished(Sender: TObject; const Args: TdxMailMergeRecordFinishedEventArgs);
    procedure DoMailMergeRecordStarted(Sender: TObject; const Args: TdxMailMergeRecordStartedEventArgs);
    procedure DoMailMergeStarted(Sender: TObject; const Args: TdxMailMergeStartedEventArgs);
    procedure DoMailMergeGetTargetDocument(Sender: TObject; const Args: TdxMailMergeGetTargetDocumentEventArgs);

    procedure BeginInitialize;
    procedure EndInitialize;
    procedure EndInitializeCommon;
    procedure DisposeCommon; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure OnLookAndFeelChanged; virtual;
    procedure SubscribeInnerControlEvents; virtual;
    procedure UnsubscribeInnerControlEvents; virtual;
    function DefaultLayoutUnit: TdxDocumentLayoutUnit; virtual;
    function IsUpdateLocked: Boolean;
    procedure PerformDeferredUIUpdates(ADeferredUpdater: TdxDeferredBackgroundThreadUIUpdater); virtual;

    procedure ResizeView(AEnsureCaretVisibleOnResize: Boolean); overload; virtual;
    procedure ResizeView(AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean); overload; virtual;

    function CreateHorizontalScrollBar: IdxOfficeScrollbar; virtual;
    function CreateVerticalScrollBar: IdxOfficeScrollbar; virtual;
    function CreateHorizontalRuler: IdxRulerControl; virtual; abstract;
    function CreateVerticalRuler: IdxRulerControl; virtual; abstract;
    function CreateScrollBarCore(AScrollBar: TdxOfficeScrollbar): IdxOfficeScrollbar; virtual;
    procedure UpdateScrollbarsVisibility; virtual;
    function CalculateVerticalScrollbarVisibility: Boolean; virtual;
    function CalculateHorizontalScrollbarVisibility: Boolean; virtual;
    procedure ScrollContent(ADirection: TcxDirection); override;
    procedure ScrollContentByGesture(ADeltaX, ADeltaY: Integer); override;
    procedure UpdateVerticalScrollBar(AAvoidJump: Boolean); virtual;

    function GetVerticalScrollbarBounds(AWidth, AOffset, AHorizontalScrollbarHeight: Integer): TRect; virtual;
    function PerformResize(const AInitialClientBounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean): Boolean; virtual;
    function CalculateViewPixelBounds(AClientBounds: TRect): TRect; virtual;
    function CalculateViewBounds(AClientBounds: TRect): TRect; virtual;

    function GetClientBounds: TRect; override;
    procedure DoCancelMode; override;

    procedure DrawBackground(const ARect: TRect); virtual;
    procedure DoPaint; override;

    procedure BoundsChanged; override;
    function GetSizeGripBounds: TRect; override;
    procedure InitializeControl; override;
    procedure FinalizeControl; override;

    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure EnabledChanged; override;

    function IsScrollBarsArea(const APoint: TPoint): Boolean; override;
    function IsScrollBarsCapture: Boolean; override;
    function IsSizeGripArea(const APoint: TPoint): Boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure Zoom(ADelta: Integer; var AHandled: Boolean); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
       MousePos: TPoint): Boolean; override;

    procedure InitializeFlushPendingTextInputTimer; virtual;
    procedure DestroyFlushPendingTextInputTimer; virtual;
    procedure StartPendingInput; virtual;
    procedure StopPendingInput; virtual;
    procedure OnFlushPendingTextInputTimerTick(ASender: TObject); virtual;
    procedure ForceFlushPendingTextInput; virtual;
    function GetKeyboardHandler: IdxKeyboardHandlerService;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    class function CreateSystemCaretBitmap: TcxBitmap;
    procedure EnsureSystemCaretCreated; virtual;
    procedure HideCaretCore;
    function ShouldShowCaret: Boolean;
    procedure ShowCaretCore;
    procedure ShowSystemCaret; virtual;
    procedure HideSystemCaret; virtual;
    procedure StartCaretBlinking;
    procedure StopCaretBlinking;
    procedure StartCaretTimer;
    procedure StopCaretTimer;

    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    procedure PerformRulersResize; virtual;
    procedure UpdateVerticalRuler; virtual;
    procedure UpdateHorizontalRuler; virtual;
    procedure UpdateRulers; virtual;
    procedure UpdateRulersCore; virtual;
    procedure UpdateSkinInfo;
    procedure InitializeScrollBars; virtual;
    procedure InitializeRulers; virtual;
    procedure InitializeVerticalScrollBarUpdateTimer; virtual;
    procedure DestroyVerticalScrollBarUpdateTimer; virtual;
    procedure OnVerticalScrollBarUpdateTimer(ASender: TObject); virtual;

    property BackgroundPainter: TdxRichEditViewBackgroundPainter read FBackgroundPainter;
    property MeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy read GetMeasurementAndDrawingStrategy;
    property FlushPendingTextInputTimer: TcxTimer read FFlushPendingTextInputTimer;
    property KeyboardController: TdxCustomKeyboardController read GetKeyboardController;
    property MouseController: TdxRichEditCustomMouseController read GetMouseController;
    property SkinInfo: TdxSkinInfo read FSkinInfo;
    property ViewPainter: TdxRichEditViewPainter read FViewPainter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateDocumentServer: IdxRichEditDocumentServer; overload;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions;
    procedure MailMerge(const ADocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;

    // for internal use
    procedure AddKeyboardService(const AService: IdxKeyboardHandlerService);
    procedure RemoveKeyboardService(const AService: IdxKeyboardHandlerService);
    function GetPixelPhysicalBounds(APageViewInfo: TdxPageViewInfo; ALogicalBounds: TRect): TRect;
    procedure EnsureCaretVisible(ACheckCaretVisibility: Boolean = True); virtual;

    // ServiceContainer Members
    procedure AddService(AServiceType: TdxServiceType; ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure AddService(AServiceType: TdxServiceType; ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(AServiceType: TdxServiceType; AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(AServiceType: TdxServiceType; AServiceInstance: IInterface); overload;
    procedure RemoveService(AServiceType: TdxServiceType; APromote: Boolean); overload;
    procedure RemoveService(AServiceType: TdxServiceType); overload;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property DocumentLayout: TdxRichEditDocumentLayout read InternalGetDocumentLayout;
    property DpiX: Single read GetDpiX;
    property DpiY: Single read GetDpiY;
    property DragCaret: TdxDragCaret read FDragCaret;
    property InnerControl: TdxInnerRichEditControl read FInnerControl;
    property HorizontalScrollBar: TdxOfficeScrollbar read FHorizontalScrollBar;
    property VerticalScrollBar: TdxOfficeScrollbar read FVerticalScrollBar;
    property BackgroundThreadUIUpdater: TdxBackgroundThreadUIUpdater read GetBackgroundThreadUIUpdater;
    property ControlDeferredChanges: TdxRichEditControlDeferredChanges read GetControlDeferredChanges;
    property Painter: TdxRichEditControlPainter read FPainter;

    //public methods
    function CanUndo: Boolean;
    procedure Undo;
    function CanRedo: Boolean;
    procedure Redo;
    procedure ClearUndo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure SelectAll;
    procedure DeselectAll;
    function IsSelectionInTable: Boolean;
    function IsFloatingObjectSelected: Boolean;
    function IsSelectionInHeader: Boolean;
    function IsSelectionInFooter: Boolean;
    function IsSelectionInHeaderOrFooter: Boolean;
    function IsSelectionInTextBox: Boolean;
    procedure EnsureImagesLoadComplete;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    function CreateNewDocument(ARaiseDocumentClosing: Boolean = False): Boolean; virtual;
    procedure LoadDocumentTemplate(const AFileName: string); overload;
    procedure LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(const AFileName: string); overload;
    procedure LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure SaveDocument; overload;
    procedure SaveDocument(const AFileName: string); overload;
    procedure SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload;

    property ActiveView: TdxRichEditView read GetActiveView;
    property ActiveViewType: TdxRichEditViewType read GetActiveViewType write SetActiveViewType stored IsActiveViewTypeStored;
    property AutoSizeMode: TdxRichEditAutoSizeMode read FAutoSizeMode write SetAutoSizeMode default TdxRichEditAutoSizeMode.None;
    property Caret: TdxCaret read FCaret;
    property DocumentModelModified: Boolean read GetModified write SetModified;
    property LookAndFeel;
    property LayoutUnit: TdxDocumentLayoutUnit read GetLayoutUnit write SetLayoutUnit;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowCaretInReadOnly: Boolean read FShowCaretInReadOnly write SetShowCaretInReadOnly default True;

    property Views: TdxRichEditCustomViewRepository read GetViews write SetViews;
    property ViewBounds: TRect read FViewBounds;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default True;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default True;
    property Options: TdxRichEditControlCustomOptions read GetOptions write SetOptions;
    property MeasurementUnit: TdxMeasurementUnit read GetMeasurementUnit write SetMeasurementUnit default TdxMeasurementUnit.Document;

    property Document: IdxRichEditDocument read GetDocument;

    property OnAfterExport: TNotifyEvent read FOnAfterExport write FOnAfterExport;
    property OnActiveViewChanged: TNotifyEvent read FOnActiveViewChanged write FOnActiveViewChanged;
    property OnDocumentClosing: TCloseQueryEvent read FOnDocumentClosing write FOnDocumentClosing;
    property OnContentChanged: TNotifyEvent read FOnContentChanged write FOnContentChanged;
    property OnDocumentLoaded: TNotifyEvent read FOnDocumentLoaded write FOnDocumentLoaded;
    property OnDocumentProtectionChanged: TNotifyEvent read FOnDocumentProtectionChanged write FOnDocumentProtectionChanged;
    property OnEmptyDocumentCreated: TNotifyEvent read FOnEmptyDocumentCreated write FOnEmptyDocumentCreated;
    property OnHyperlinkClick: TdxHyperlinkClickEvent read FOnHyperlinkClick write FOnHyperlinkClick;
    property OnModifiedChanged: TNotifyEvent read FOnModifiedChanged write FOnModifiedChanged;
    property OnPageBackgroundChanged: TNotifyEvent read FOnPageBackgroundChanged write FOnPageBackgroundChanged;
    property OnPlainTextChanged: TNotifyEvent read FOnPlainTextChanged write FOnPlainTextChanged;
    property OnReadOnlyChanged: TNotifyEvent read FOnReadOnlyChanged write FOnReadOnlyChanged;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnShortCut: TdxRichEditShortCutEvent read FOnShortCut write FOnShortCut;
    property OnZoomChanged: TNotifyEvent read FOnZoomChanged write FOnZoomChanged;

    property OnCalculateDocumentVariable: TdxCalculateDocumentVariableEvent read FOnCalculateDocumentVariable write FOnCalculateDocumentVariable;
    property OnCustomizeMergeFields: TdxCustomizeMergeFieldsEvent read FOnCustomizeMergeFields write FOnCustomizeMergeFields;
    property OnMailMergeFinished: TdxMailMergeFinishedEvent read FOnMailMergeFinished write FOnMailMergeFinished;
    property OnMailMergeRecordFinished: TdxMailMergeRecordFinishedEvent read FOnMailMergeRecordFinished write FOnMailMergeRecordFinished;
    property OnMailMergeRecordStarted: TdxMailMergeRecordStartedEvent read FOnMailMergeRecordStarted write FOnMailMergeRecordStarted;
    property OnMailMergeStarted: TdxMailMergeStartedEvent read FOnMailMergeStarted write FOnMailMergeStarted;
    property OnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEvent read FOnMailMergeGetTargetDocument write FOnMailMergeGetTargetDocument;
  end;

implementation

uses
  Dialogs, Math, ComObj, IOUtils,
  dxCore, dxDPIAwareUtils, dxTypeHelpers, cxScrollBar, dxThreading,
  dxRichEdit.Api.MailMerge,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.PlainText,
  dxRichEdit.Rtf,
  dxRichEdit.InnerControl.DrawingStrategy,
  dxRichEdit.Export.Formats;

{ TdxRichEditControlPainter }

constructor TdxRichEditControlPainter.Create(AControl: TdxRichEditControlBase);
begin
  inherited Create;
  FDeferredDraws := TList<TdxDrawDelegate>.Create;
  FControl := AControl;
end;

destructor TdxRichEditControlPainter.Destroy;
begin
  FreeAndNil(FDeferredDraws);
  inherited Destroy;
end;

function TdxRichEditControlPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := Control.LookAndFeel;
end;

function TdxRichEditControlPainter.GetCaret: TdxCaret;
begin
  Result := Control.Caret;
end;

function TdxRichEditControlPainter.GetDragCaret: TdxDragCaret;
begin
  Result := Control.DragCaret;
end;

function TdxRichEditControlPainter.GetActiveView: TdxRichEditView;
begin
  Result := Control.ActiveView;
end;

procedure TdxRichEditControlPainter.Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
var
  I, ACount: Integer;
  AClipState: TdxGraphics.TClipState;
begin
  if Control.InnerControl = nil then
    Exit;

  Control.InnerControl.BeginDocumentRendering;
  try
    AClipState := AGraphics.SaveStateAndSetClip(Control.ClientBounds);
    try
      PerformRenderingInUnits(AGraphics, ACustomMarkExporter, RenderDocument);
      PerformRenderingInPixels(AGraphics, ACustomMarkExporter, RenderDocumentDecorators);

      ACount := FDeferredDraws.Count;
      if ACount > 0 then
      begin
        for I := 0 to ACount - 1 do
          PerformRenderingInUnits(AGraphics, nil, FDeferredDraws[I]);
        FDeferredDraws.Clear;
      end;
    finally
      AGraphics.RestoreClipState(AClipState);
    end;
  finally
    Control.InnerControl.EndDocumentRendering;
  end;
end;

procedure TdxRichEditControlPainter.DrawCaret;
begin
  DrawCaret(DrawCaretCore);
end;

procedure TdxRichEditControlPainter.DrawDragCaret;
begin
  DrawCaret(DrawDragCaretCore);
end;

procedure TdxRichEditControlPainter.DrawCaret(const ADrawCaret: TdxDrawDelegate);
var
  AHdc: HDC;
  AGraphics: TdxGraphics;
  AClipState: TdxGraphics.TClipState;
begin
  AHdc := GetWindowDC(Control.Handle);
  try
    AGraphics := TdxGraphics.CreateFromHdc(AHdc);
    try
      AClipState := AGraphics.GetClipState;
      try
        PerformRenderingInPixels(AGraphics, nil, ADrawCaret);
      finally
        AGraphics.RestoreClipState(AClipState);
      end;
    finally
      AGraphics.Free;
    end;
  finally
    ReleaseDC(Control.Handle, AHdc);
  end;
end;

procedure TdxRichEditControlPainter.RegisterDeferredDraw(const ADraw: TdxDrawDelegate);
begin
  FDeferredDraws.Add(ADraw);
end;

procedure TdxRichEditControlPainter.DeferredDraw(APage: TdxPageViewInfo; ADrawAtPage: TdxDrawAtPageDelegate);
var
  ADraw: TdxDrawDelegate;
begin
  ADraw :=
    procedure (AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter)
    begin
      Control.ViewPainter.DrawAtPageCore(AGraphics, APage, ADrawAtPage);
    end;
  RegisterDeferredDraw(ADraw);
end;

procedure TdxRichEditControlPainter.DeferredDrawReversibleFrame(const ABounds: TRect; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawDelegate;
  ABoundsForDelegate: TRect;
begin
  ABoundsForDelegate := ABounds;
  ADraw :=
    procedure (AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter)
    begin
      Control.ViewPainter.DrawReversibleFrameAtPage(AGraphics, ABoundsForDelegate, APage);
    end;
  RegisterDeferredDraw(ADraw);
end;

procedure TdxRichEditControlPainter.DrawReversibleHorizontalLine(Y: Integer; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawDelegate;
begin
  ADraw :=
    procedure (AGraphicsCache: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter)
    begin
      DrawReversibleHorizontalLineCore(AGraphicsCache, Y, APage);
    end;
  DrawReversibleCore(APage, ADraw);
end;

procedure TdxRichEditControlPainter.DrawReversibleVerticalLine(X: Integer; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawDelegate;
begin
  ADraw :=
    procedure (AGraphicsCache: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter)
    begin
      DrawReversibleVerticalLineCore(AGraphicsCache, X, APage);
    end;
  DrawReversibleCore(APage, ADraw);
end;

procedure TdxRichEditControlPainter.DrawReversibleCore(APage: TdxPageViewInfo; const ADraw: TdxDrawDelegate);
var
  AHdc: THandle;
  AGraphics: TdxGraphics;
  AClipState: TdxGraphics.TClipState;
begin
  AHdc := GetWindowDC(Control.Handle);
  try
    AGraphics := TdxGraphics.CreateFromHdc(AHdc);
    try
      AClipState := AGraphics.SaveStateAndSetClip(Control.ClientBounds);
      try
        PerformRenderingInUnits(AGraphics, nil, ADraw);
      finally
        AGraphics.RestoreClipState(AClipState);
      end;
    finally
      AGraphics.Free;
    end;
  finally
    ReleaseDC(Control.Handle, AHdc);
  end;
end;

procedure TdxRichEditControlPainter.PerformRenderingInUnits(AGraphics: TdxGraphics;
  const ACustomMarkExporter: IdxCustomMarkExporter; const ADraw: TdxDrawDelegate);
var
  AModifier: TdxGraphicsToLayoutUnitsModifier;
  AHdcOriginModifier: TdxHdcOriginModifier;
  APaintOrg: TPoint;
  ALayoutUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  ALayoutUnitConverter := Control.DocumentModel.LayoutUnitConverter;
  AModifier := TdxGraphicsToLayoutUnitsModifier.Create(AGraphics, ALayoutUnitConverter);
  try
    APaintOrg := Control.ViewBounds.TopLeft;
    AGraphics.TranslateWorldTransform(APaintOrg.X, APaintOrg.Y);

    APaintOrg.Offset(Control.Canvas.DCOrigin);

    AHdcOriginModifier := TdxHdcOriginModifier.Create(AGraphics, APaintOrg, 1.0);
    try
      ADraw(AGraphics, ACustomMarkExporter);
    finally
      AHdcOriginModifier.Free;
    end;
  finally
    AModifier.Free;
  end;
end;

procedure TdxRichEditControlPainter.PerformRenderingInPixels(AGraphics: TdxGraphics;
  const ACustomMarkExporter: IdxCustomMarkExporter; const ADraw: TdxDrawDelegate);
begin
  AGraphics.SetClip(Control.ClientBounds, True);
  ADraw(AGraphics, ACustomMarkExporter);
end;

procedure TdxRichEditControlPainter.RenderDocument(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  ActiveView.SelectionLayout.Update;
  Control.ViewPainter.Draw(AGraphics, ACustomMarkExporter);
end;

procedure TdxRichEditControlPainter.RenderDocumentDecorators(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  Control.ViewPainter.DrawDecorators(AGraphics);
  if not Caret.IsHidden then
    DrawCaretCore(AGraphics, ACustomMarkExporter);

  if (DragCaret <> nil) and not DragCaret.IsHidden then
    DrawDragCaretCore(AGraphics, ACustomMarkExporter);
end;

procedure TdxRichEditControlPainter.DrawCaretCore(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  DrawCaretCore(Caret, AGraphics, ACustomMarkExporter);
end;

procedure TdxRichEditControlPainter.DrawDragCaretCore(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  DrawCaretCore(DragCaret, AGraphics, ACustomMarkExporter);
end;

procedure TdxRichEditControlPainter.DrawCaretCore(ACaret: TdxCaret; AGraphics: TdxGraphics;
  const ACustomMarkExporter: IdxCustomMarkExporter);
var
  APosition: TdxCaretPosition;
  AViewBounds, ABounds, APadding: TRect;
begin
  if not ACaret.ShouldDrawCaret(ActiveView.DocumentModel) then
    Exit;

  APosition := ACaret.GetCaretPosition(ActiveView);
  if not APosition.Update(TdxDocumentLayoutDetailsLevel.Character) then
    Exit;
  AViewBounds := ActiveView.CreateLogicalRectangle(APosition.PageViewInfo, ActiveView.Bounds);
  APadding := ActiveView.ActualPadding;
  AViewBounds.Offset(-APadding.Left, -APadding.Top);
  AViewBounds.Width := AViewBounds.Width + (APadding.Left + APadding.Right);
  AViewBounds.Height := AViewBounds.Height + (APadding.Top + APadding.Bottom);

  ABounds := APosition.CalculateCaretBounds;
  if ABounds.IntersectsWith(AViewBounds) then
  begin
    ABounds.Intersect(AViewBounds);
    ABounds := Control.GetPixelPhysicalBounds(APosition.PageViewInfo, ABounds);

    ACaret.Bounds := ABounds;
    if ABounds.IsZero or (ABounds.Height < 0) or (ABounds.Width < 0) then
      Exit;

    ABounds.Width := Max(1, ABounds.Width);
    ABounds.Height := Max(1, ABounds.Height);
    ACaret.Bounds := ABounds;
    ACaret.Draw(AGraphics);
  end;
end;

function TdxRichEditControlPainter.GetActualBounds(AGraphics: TdxGraphics; APage: TdxPageViewInfo; const ABounds: TRect): TRect;
begin
  Result := ActiveView.CreatePhysicalRectangleFast(APage, ABounds);
  Result := ActiveView.DocumentLayout.UnitConverter.LayoutUnitsToPixels(Result, AGraphics.DpiX, AGraphics.DpiY);
end;

procedure TdxRichEditControlPainter.DrawReversibleFrameCore(AGraphics: TdxGraphics; const ABounds: TRect; APage: TdxPageViewInfo);
begin
  Control.ViewPainter.DrawReversibleFrameAtPage(AGraphics, ABounds, APage);
  if not ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character) then
    Exit;
end;

procedure TdxRichEditControlPainter.DrawReversibleHorizontalLineCore(AGraphics: TdxGraphics; Y: Integer; APage: TdxPageViewInfo);
begin
  Control.ViewPainter.DrawReversibleHorizontalLineAtPage(AGraphics, Y, APage);
end;

procedure TdxRichEditControlPainter.DrawReversibleVerticalLineCore(AGraphics: TdxGraphics; X: Integer; APage: TdxPageViewInfo);
begin
  Control.ViewPainter.DrawReversibleVerticalLineAtPage(AGraphics, X, APage);
end;

{ TdxEmptyRichEditControlPainter }

procedure TdxEmptyRichEditControlPainter.Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
end;

procedure TdxEmptyRichEditControlPainter.DrawCaret;
begin
end;

{ TdxRichEditCustomInnerControl }

function TdxRichEditCustomInnerControl.CreateKeyboardController: TdxCustomKeyboardController;
begin
  Result := TdxRichEditKeyboardController.Create(Self);
end;

function TdxRichEditCustomInnerControl.CreateNativeDocument: IdxRichEditDocument;
begin
  Result := TdxNativeDocument.Create(DocumentModel.MainPieceTable, Self);
end;

function TdxRichEditCustomInnerControl.CreateNativeSubDocument(
  APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, Self);
end;

function TdxRichEditCustomInnerControl.CreateMouseController: TdxRichEditCustomMouseController;
begin
  Result := TdxRichEditMouseController.Create(Owner.RichEditControl);
end;

function TdxRichEditCustomInnerControl.GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions(AOptions).GetInternalMailMergeOptions;
end;

function TdxRichEditCustomInnerControl.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions.Create;
end;

{ TdxRichEditControlBase }

constructor TdxRichEditControlBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(Left, Top, 300, 200);
  FDoubleBuffered := True;
  FShowCaretInReadOnly := True;
  FWantTabs := True;
  FWantReturns := True;
  Keys := [kAll, kArrows, kChars, kTab];
  BorderStyle := cxcbsDefault;
  FAutoSizeMode := TdxRichEditAutoSizeMode.None;

  FInnerControl := CreateInnerControl;
  BeginInitialize;
  LayoutUnit := DefaultLayoutUnit;
  FCaretTimer := TcxTimer.Create(nil);
  EndInitialize;
end;

destructor TdxRichEditControlBase.Destroy;
begin
  StopCaretTimer;
  FreeAndNil(FVerticalScrollBarUpdateTimer);
  FreeAndNil(FBuiltInPopupMenu);

  DisposeCommon;

  FreeAndNil(FBackgroundPainter);
  FreeAndNil(FPainter);
  FreeAndNil(FViewPainter);
  FreeAndNil(FCaretTimer);
  FreeAndNil(FCaret);
  DestroyCaret;
  inherited Destroy;
end;

function TdxRichEditControlBase.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  if InnerControl <> nil then
    Result := InnerControl.CreateMailMergeOptions
  else
    Result := TdxNativeMailMergeOptions.Create;
end;

procedure TdxRichEditControlBase.MailMerge(const ADocument: IdxRichEditDocument);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(ADocument);
end;

procedure TdxRichEditControlBase.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(AOptions, ATargetDocument);
end;

procedure TdxRichEditControlBase.MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(AFileName, AFormat);
end;

procedure TdxRichEditControlBase.MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(AStream, AFormat);
end;

procedure TdxRichEditControlBase.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(AOptions, AFileName, AFormat);
end;

procedure TdxRichEditControlBase.MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.MailMerge(AOptions, AStream, AFormat);
end;

class constructor TdxRichEditControlBase.Initialize;
begin
  FSystemCaretBitmap := CreateSystemCaretBitmap;
end;

class destructor TdxRichEditControlBase.Finalize;
begin
  FreeAndNil(FSystemCaretBitmap);
end;

function TdxRichEditControlBase.CalculateActualViewBounds(const APreviousViewBounds: TRect): TRect;
begin
  Result := CalculateViewBounds(ClientBounds);
end;

function TdxRichEditControlBase.CalculateInitialClientBounds: TRect;
var
  AForm: TCustomForm;
  ABounds: TRect;
begin
  if HandleAllocated then
    ABounds := ClientRect
  else
    ABounds.InitSize(0, 0, Width, Height);

  AForm := GetParentForm(Self);
  if AForm <> nil then
  begin
    if AForm.WindowState <> wsMinimized then
      FLastValidClientRectangle := ABounds;
  end
  else
    FLastValidClientRectangle := ABounds;
  Result := FLastValidClientRectangle;
  InflateRect(Result, -BorderSize, -BorderSize);
end;

function TdxRichEditControlBase.CalculateHorizontalScrollbarHeight: Integer;
begin
  if CalculateHorizontalScrollbarVisibility then
    Result := GetScaledScrollBarSize(ScaleFactor).cy
  else
    Result := 0;
end;

function TdxRichEditControlBase.CalculateVerticalScrollbarWidth: Integer;
begin
  if CalculateVerticalScrollbarVisibility then
    Result := GetScaledScrollBarSize(ScaleFactor).cx
  else
    Result := 0;
end;

function TdxRichEditControlBase.CalculateViewBounds(AClientBounds: TRect): TRect;
var
  AViewPixelBounds: TRect;
begin
  AViewPixelBounds := CalculateViewPixelBounds(AClientBounds);
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(AViewPixelBounds, DpiX, DpiY);
end;

function TdxRichEditControlBase.CalculateViewPixelBounds(AClientBounds: TRect): TRect;
var
  AViewPadding: TRect;
begin
  Result := ClientBounds;
  AViewPadding := ActiveView.ActualPadding;
  Result.Inflate(-AViewPadding.Left, -AViewPadding.Top, -AViewPadding.Right, -AViewPadding.Bottom);
end;

procedure TdxRichEditControlBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ABestSize: TSize;
begin
  if (AutoSizeMode <> TdxRichEditAutoSizeMode.None) and not FIsUpdateAutoSizeMode and not FInsideResize then
  begin
    ABestSize := CalcBestSize(AWidth, AHeight, False);
    AWidth := ABestSize.Width;
    AHeight := ABestSize.Height;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TdxRichEditControlBase.GetClientBounds: TRect;
begin
  Result := FClientBounds;
end;

procedure TdxRichEditControlBase.DoPaint;
var
  ACustomMarkExporter: TdxCustomMarkExporter;
begin
  Canvas.SaveState;
  try
    if not FSizeGripBounds.IsEmpty then
    begin
      Canvas.FillRect(FSizeGripBounds, LookAndFeel.Painter.DefaultSizeGripAreaColor);
      Canvas.ExcludeClipRect(FSizeGripBounds);
    end;
    inherited DoPaint;
    DrawBackground(ClientBounds);
    if IsUpdateLocked then
    begin
      ControlDeferredChanges.Redraw := True;
      Exit;
    end;
    if Painter <> nil then
    begin
      ACustomMarkExporter := TdxCustomMarkExporter.Create;
      try
        Painter.Draw(Graphics, ACustomMarkExporter);
      finally
        ACustomMarkExporter.Free;
      end;
    end;
  finally
    Canvas.RestoreState;
  end;
end;

function TdxRichEditControlBase.CreateDocumentServer: IdxRichEditDocumentServer;
begin
  Result := CreateDocumentServer(nil) as IdxRichEditDocumentServer;
end;

function TdxRichEditControlBase.GetLayoutCalculationMode: TdxCalculationModeType;
begin
  Result := TdxCalculationModeType.Automatic;
end;

function TdxRichEditControlBase.GetDocumentLayout: TdxDocumentLayout;
begin
  Result := GetDocumentLayoutAsync();
end;

function TdxRichEditControlBase.GetDocumentLayoutAsync: TdxDocumentLayout;
begin
  Result := ActiveView.DocumentLayout;
end;

procedure TdxRichEditControlBase.PerformPageSecondaryFormatting(APage: TdxPage);
begin
  InnerControl.Formatter.PerformPageSecondaryFormatting(APage);
end;

procedure TdxRichEditControlBase.AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.DocumentLayoutInvalidated.Add(AHandler);
end;

procedure TdxRichEditControlBase.AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.PageFormatted.Add(AHandler);
end;

procedure TdxRichEditControlBase.AddDocumentFormatted(const AHandler: TdxEvent);
begin
  InnerControl.DocumentFormatted.Add(AHandler);
end;

procedure TdxRichEditControlBase.RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.DocumentLayoutInvalidated.Remove(AHandler);
end;

procedure TdxRichEditControlBase.RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.PageFormatted.Remove(AHandler);
end;

procedure TdxRichEditControlBase.RemoveDocumentFormatted(const AHandler: TdxEvent);
begin
  InnerControl.DocumentFormatted.Remove(AHandler);
end;

procedure TdxRichEditControlBase.CreateDragCaret;
begin
  FDragCaret := TdxDragCaret.Create(ActiveView);
end;

procedure TdxRichEditControlBase.DestroyDragCaret;
begin
  if (DragCaret <> nil) and not DragCaret.IsHidden then
    DrawDragCaret;
  FreeAndNil(FDragCaret);
end;

function TdxRichEditControlBase.GetDragCaret: TdxDragCaret;
begin
  Result := FDragCaret;
end;

function TdxRichEditControlBase.GetCommentPadding: TdxCommentPadding;
begin
  Result := FCommentPadding;
end;

function TdxRichEditControlBase.GetOvertype: Boolean;
begin
  Result := InnerControl.Overtype;
end;

function TdxRichEditControlBase.GetViewBounds: TRect;
begin
  Result := FViewBounds;
end;

function TdxRichEditControlBase.IsHyperlinkActive: Boolean;
var
  AMouseController: TdxRichEditMouseController;
  AField: TdxField;
begin
  if (InnerControl <> nil) and InnerControl.IsHyperlinkModifierKeysPress then
  begin
    AMouseController := Safe<TdxRichEditMouseController>.Cast(MouseController);
    if AMouseController <> nil then
    begin
      AField := Safe<TdxField>.Cast(AMouseController.ActiveObject);
      if (AField <> nil) and DocumentModel.ActivePieceTable.IsHyperlinkField(AField) then
        Exit(True);
    end;
  end;
  Result := False;
end;

procedure TdxRichEditControlBase.SetOvertype(const Value: Boolean);
begin
  InnerControl.Overtype := Value;
end;

procedure TdxRichEditControlBase.SetReadOnly(const Value: Boolean);
begin
  if InnerControl <> nil then
    InnerControl.ReadOnly := Value;
end;

procedure TdxRichEditControlBase.UpdateControlAutoSize;
var
  ANewSize: TSize;
begin
  if AutoSizeMode = TdxRichEditAutoSizeMode.None then
    Exit;
  if not FIsUpdateAutoSizeMode and not FInsideResize then
  begin
    FIsUpdateAutoSizeMode := True;
    try
      ANewSize := CalcBestSize(Width, Height, (AutoSizeMode <> TdxRichEditAutoSizeMode.Horizontal) or
        (Width = Constraints.MaxWidth) and (Constraints.MaxWidth <> 0));
      FContentSize := ANewSize;
      if not ANewSize.IsEqual(FLastSize) then
      begin
        SetBounds(Left, Top, ANewSize.cx, ANewSize.cy);
        FLastSize := ANewSize;
      end;
    finally
      FIsUpdateAutoSizeMode := False;
    end;
  end;
end;

procedure TdxRichEditControlBase.ShowSearchForm;
begin
end;

procedure TdxRichEditControlBase.ShowSplitTableCellsForm(const AParameters: TdxSplitTableCellsParameters;
  const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowRangeEditingPermissionsForm;
begin
end;

procedure TdxRichEditControlBase.ShowReplaceForm;
begin
end;

procedure TdxRichEditControlBase.ShowNumberingListForm(AParagraphs: TdxParagraphList;
  const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
  ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage);
begin
end;

procedure TdxRichEditControlBase.ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties;
  const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowFloatingInlineObjectLayoutOptionsForm(
  const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
  const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties;
  const ACallback: TdxShowFontFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo;
  const ATitle: string; const ACallback: TdxShowHyperlinkFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowInsertMergeFieldForm;
begin
end;

procedure TdxRichEditControlBase.ShowInsertTableCellsForm(const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowInsertTableForm(const AParameters: TdxCreateTableParameters;
  const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowLineNumberingForm(AProperties: TdxLineNumberingInfo;
  const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
  const ACallback: TdxShowMergeDatabaseRecordsFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowSymbolForm(const ASymbolProperties: TdxSymbolProperties;
  const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject);
begin
end;

function TdxRichEditControlBase.GetSkinLeftMargin: Integer;
begin
  Result := 19;
end;

function TdxRichEditControlBase.GetSkinRightMargin: Integer;
begin
  Result := 19;
end;

function TdxRichEditControlBase.GetSkinTopMargin: Integer;
begin
  Result := 19;
end;

function TdxRichEditControlBase.GetSkinBottomMargin: Integer;
begin
  Result := 19;
end;

function TdxRichEditControlBase.GetUseSkinMargins: Boolean;
begin
  Result := True;
end;

procedure TdxRichEditControlBase.ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection);
begin
end;

procedure TdxRichEditControlBase.ShowTableStyleForm(AStyle: TdxTableStyle);
begin
end;

procedure TdxRichEditControlBase.ShowTabsForm(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer;
  const ACallback: TdxShowTabsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowTOCForm(AField: TdxField);
begin
end;

function TdxRichEditControlBase.OleDragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  Args: TdxDragEventArgs;
  P: TPoint;
begin
  if not (IsDesigning or IsDestroying) and Enabled then
  begin
    FDragObject := dataObj;
    P := ScreenToClient(pt);
    Args := TdxDragEventArgs.CreateFormOle(dataObj, grfKeyState, P, dwEffect);
    MouseController.DoDragEnter(Args);
    dwEffect := dxDragDropEffectsToOleDragDropEffects(Args.Effect);
    Result := S_OK;
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TdxRichEditControlBase.OleDragLeave: HResult;
begin
  FDragObject := nil;
  MouseController.DoDragLeave;
  Result := S_OK;
end;

function TdxRichEditControlBase.OleDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  Args: TdxDragEventArgs;
begin
  if not (IsDesigning or IsDestroying) and Enabled then
  begin
    Args := TdxDragEventArgs.CreateFormOle(FDragObject, grfKeyState, pt, dwEffect);
    MouseController.DoDragOver(Args);
    dwEffect := dxDragDropEffectsToOleDragDropEffects(Args.Effect);
    Result := S_OK;
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED
  end;
end;

function TdxRichEditControlBase.OleDrop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  Args: TdxDragEventArgs;
begin
  if not (IsDesigning or IsDestroying) and Enabled then
  begin
    Args := TdxDragEventArgs.CreateFormOle(dataObj, grfKeyState, pt, dwEffect);
    MouseController.DoDragDrop(Args);
    dwEffect := dxDragDropEffectsToOleDragDropEffects(Args.Effect);
    Result := S_OK;
    Application.BringToFront;
    SetFocus;
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
  FDragObject := nil;
end;

function TdxRichEditControlBase.OleQueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
var
  Args: TdxQueryContinueDragEventArgs;
begin
  Args := TdxQueryContinueDragEventArgs.Create(fEscapePressed, grfKeyState);
  MouseController.QueryContinueDrag(@Args);
  case Args.Action of
    TdxDragAction.Cancel:
      Result := DRAGDROP_S_CANCEL;
    TdxDragAction.Drop:
      Result := DRAGDROP_S_DROP;
  else
    Result := S_OK;
  end;
end;

function TdxRichEditControlBase.OleGiveFeedback(dwEffect: Longint): HResult;
var
  Args: TdxGiveFeedbackEventArgs;
begin
  Args := TdxGiveFeedbackEventArgs.Create(dwEffect);
  MouseController.GiveFeedback(@Args);
  if Args.UseDefaultCursors then
    Result := DRAGDROP_S_USEDEFAULTCURSORS
  else
    Result := S_OK;
end;

procedure TdxRichEditControlBase.DrawDragCaret;
begin
  Painter.DrawDragCaret;
end;

function TdxRichEditControlBase.DoDragDrop(const AData: IDataObject; AllowedEffects: TdxDragDropEffects): TdxDragDropEffects;
var
  AAllowedEffects, AEffect: Longint;
begin
  Result := [];
  AAllowedEffects := dxDragDropEffectsToOleDragDropEffects(AllowedEffects);
  try
    ActiveX.DoDragDrop(AData, Self, AAllowedEffects, AEffect);
  except

    on E: Exception do

      raise;
  end;
  Result := dxOleDragDropEffectsToDragDropEffects(AEffect);
end;

function TdxRichEditControlBase.GetBackgroundPainter: TdxRichEditViewBackgroundPainter;
begin
  Result := FBackgroundPainter;
end;

function TdxRichEditControlBase.CalcBestSize(AWidth, AHeight: Integer; AFixedWidth: Boolean): TSize;
var
  ABestSize: TSize;
begin
  ABestSize := CalcViewBestSize(AFixedWidth);
  Inc(ABestSize.cx, CalculateVerticalScrollbarWidth + BorderSize * 2);
  Inc(ABestSize.cy, CalculateHorizontalScrollbarHeight + BorderSize * 2);
  case AutoSizeMode of
    TdxRichEditAutoSizeMode.Vertical:
      Result.Init(AWidth, ABestSize.cy);
    TdxRichEditAutoSizeMode.Horizontal:
      Result.Init(ABestSize.cx, AHeight);
    TdxRichEditAutoSizeMode.Both:
      Result.Init(ABestSize.cx, ABestSize.cy);
    else
      Result.Init(0, 0);
  end;
end;

function TdxRichEditControlBase.CalcViewBestSize(AFixedWidth: Boolean): TSize;
var
  AViewPadding: TRect;
begin
  Result := ActiveView.CalcBestSize(AFixedWidth);
  AViewPadding := ActiveView.ActualPadding;
  Inc(Result.cx, AViewPadding.Left + AViewPadding.Right);
  Inc(Result.cy, AViewPadding.Top + AViewPadding.Bottom);
end;

procedure TdxRichEditControlBase.DrawBackground(const ARect: TRect);
begin
  if BackgroundPainter <> nil then
    BackgroundPainter.Draw(Graphics, ARect);
end;

function TdxRichEditControlBase.GetScaleFactor: TdxScaleFactor;
begin
  Result := InternalScaleFactor;
end;

function TdxRichEditControlBase.GetSizeGripBounds: TRect;
begin
  Result := FSizeGripBounds;
end;

procedure TdxRichEditControlBase.ActivateViewPlatformSpecific(AView: TdxRichEditView);
begin
  RecreateBackgroundPainter(AView);
  RecreateViewPainter(AView);
end;

function TdxRichEditControlBase.IsHandleCreated: Boolean;
begin
  Result := HandleAllocated;
end;

procedure TdxRichEditControlBase.ApplyChangesCorePlatformSpecific(AChangeActions: TdxDocumentModelChangeActions);
begin
  if TdxDocumentModelChangeAction.Redraw in AChangeActions then
    RedrawEnsureSecondaryFormattingComplete;
end;

procedure TdxRichEditControlBase.BeginInitialize;
begin
  InnerControl.BeginInitialize;
end;

function TdxRichEditControlBase.GetIsUpdateLocked: Boolean;
begin
  Result := (InnerControl <> nil) and InnerControl.IsUpdateLocked;
end;

function TdxRichEditControlBase.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  if InnerControl <> nil then
    Result := InnerControl.BatchUpdateHelper
  else
    Result := nil;
end;

procedure TdxRichEditControlBase.BeginUpdate;
begin
  if InnerControl <> nil then
    InnerControl.BeginUpdate;
end;

procedure TdxRichEditControlBase.CancelUpdate;
begin
  if InnerControl <> nil then
    InnerControl.CancelUpdate;
end;

procedure TdxRichEditControlBase.ApplyFontAndForeColor;
begin
  if InnerControl <> nil then
    InnerControl.ApplyFontAndForeColor;
end;

procedure TdxRichEditControlBase.OnBackColorChanged;
begin
  Redraw(False);
end;

function TdxRichEditControlBase.CreateMeasurementAndDrawingStrategy(
  ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
begin
  Result := TdxGdiMeasurementAndDrawingStrategy.Create(ADocumentModel);
end;

function TdxRichEditControlBase.CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
begin
  Result := CreateDocumentServer(TdxDocumentModel(ADocumentModel));
end;

function TdxRichEditControlBase.CreatePlatformSpecificScrollBarAdapter: IdxPlatformSpecificScrollBarAdapter;
begin
  Result := TdxWinFormsScrollBarAdapter.Create;
end;

procedure TdxRichEditControlBase.EnsureCaretVisible(ACheckCaretVisibility: Boolean = True);
begin
  if (ActiveView <> nil) then
    UpdateUIFromBackgroundThread(
      procedure ()
      begin
        ActiveView.EnsureCaretVisible(ACheckCaretVisibility)
      end);
end;

procedure TdxRichEditControlBase.OnDeferredResizeCore;
begin
  if IsUpdateLocked then
    ControlDeferredChanges.Resize := True
  else
  begin
    OnResizeCore(True);
    RedrawEnsureSecondaryFormattingComplete;
  end;
end;

procedure TdxRichEditControlBase.RedrawEnsureSecondaryFormattingComplete(Action: TdxRefreshAction);
begin
  RedrawEnsureSecondaryFormattingComplete;
end;

function TdxRichEditControlBase.CreateRichEditViewHorizontalScrollController(
  ARichEditView: TdxRichEditView): TdxRichEditViewHorizontalScrollController;
begin
  Result := TdxWinFormsRichEditViewHorizontalScrollController.Create(ARichEditView);
end;

function TdxRichEditControlBase.CreateViewPainter(AView: TdxRichEditView): TdxRichEditViewPainter;
begin
  Result := AView.CreatePainter;
end;

function TdxRichEditControlBase.CreateBackgroundPainter(AView: TdxRichEditView): TdxRichEditViewBackgroundPainter;
begin
  Result := AView.CreateBackgroundPainter;
end;

procedure TdxRichEditControlBase.RecreateBackgroundPainter(AView: TdxRichEditView);
begin
  FreeAndNil(FBackgroundPainter);
  FBackgroundPainter := CreateBackgroundPainter(AView);
end;

procedure TdxRichEditControlBase.RecreateViewPainter(AView: TdxRichEditView);
begin
  FreeAndNil(FViewPainter);
  FViewPainter := CreateViewPainter(AView);
end;

procedure TdxRichEditControlBase.DoCancelMode;
begin
  inherited DoCancelMode;
  MouseController.DoCancelMode;
end;

procedure TdxRichEditControlBase.DoBeforeExport(Sender: TObject; Args: TdxBeforeExportEventArgs);
begin
end;

procedure TdxRichEditControlBase.DoAfterExport(Sender: TObject);
begin
  dxCallNotify(OnAfterExport, Self);
end;

procedure TdxRichEditControlBase.DoPlainTextChanged(Sender: TObject);
begin
  dxCallNotify(OnPlainTextChanged, Self);
end;

procedure TdxRichEditControlBase.DoDocumentClosing(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FOnDocumentClosing) then
    FOnDocumentClosing(Sender, CanClose);
end;

procedure TdxRichEditControlBase.DoContentChanged(Sender: TObject);
begin
  dxCallNotify(OnContentChanged, Self);
end;

procedure TdxRichEditControlBase.DoHyperlinkClick(Sender: TObject; const Args: TdxHyperlinkClickEventArgs);
begin
  if Assigned(OnHyperlinkClick) then
    OnHyperlinkClick(Self, Args);
end;

procedure TdxRichEditControlBase.DoModifiedChanged(Sender: TObject);
begin
  dxCallNotify(OnModifiedChanged, Self);
end;

procedure TdxRichEditControlBase.DoReadOnlyChanged(Sender: TObject);
begin
  dxCallNotify(OnReadOnlyChanged, Self);
  DoReadOnlyChangedPlatformSpecific;
end;

procedure TdxRichEditControlBase.DoReadOnlyChangedPlatformSpecific;
begin
  if ShouldShowCaret then
    StartCaretBlinking
  else
    StopCaretBlinking;
end;

procedure TdxRichEditControlBase.DoActiveViewChanged(Sender: TObject);
begin
  dxCallNotify(OnActiveViewChanged, Self);
end;

procedure TdxRichEditControlBase.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  Args: TdxMouseEventArgs;
  AMenuClass: TdxRichEditCustomPopupMenuClass;
  ACapture: THandle;
  P: TPoint;
begin
  ACapture := GetCapture;
  if (ACapture <> 0) and (ACapture <> Handle) then
    Exit;
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
  begin
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      P := ClientToScreen(Caret.Bounds.TopLeft)
    else
      P := ClientToScreen(MousePos);
    Handled := DoShowPopupMenu(PopupMenu, P.X, P.Y);
  end;
  if not Handled and Options.Behavior.ShowPopupMenuAllowed then
  begin
    Args := TdxMouseEventArgs.Create([mbRight], [], MousePos);
    if MouseController.DoContextPopup(Args) then
    begin
      AMenuClass := TdxRichEditContentPopupMenu;

      if (FBuiltInPopupMenu = nil) or (AMenuClass <> FBuiltInPopupMenu.ClassType) then
      begin
        FBuiltInPopupMenu.Free;
        FBuiltInPopupMenu := AMenuClass.Create(Self);
      end;
      if (MousePos.X = -1) and (MousePos.Y = -1) then
        Args.MousePos := Caret.Bounds.TopLeft;
      FBuiltInPopupMenu.Popup(Args.MousePos);
      Handled := True;
    end;
  end;
end;

procedure TdxRichEditControlBase.Zoom(ADelta: Integer; var AHandled: Boolean);
begin
  ActiveView.ZoomFactor :=
    EnsureRange(
      ActiveView.ZoomFactor + ADelta / 400,
      Options.Behavior.MinZoomFactor,
      Options.Behavior.MaxZoomFactor);
  AHandled := True;
end;

procedure TdxRichEditControlBase.DoPageBackgroundChanged(Sender: TObject);
begin
  dxCallNotify(OnPageBackgroundChanged, Self);
end;

procedure TdxRichEditControlBase.DoEmptyDocumentCreated(Sender: TObject);
begin
  dxCallNotify(OnEmptyDocumentCreated, Self);
end;

procedure TdxRichEditControlBase.DoDocumentLoaded(Sender: TObject);
begin
  dxCallNotify(OnDocumentLoaded, Self);
end;

procedure TdxRichEditControlBase.DoDocumentProtectionChanged(Sender: TObject);
begin
  dxCallNotify(OnDocumentProtectionChanged, Self);
end;

procedure TdxRichEditControlBase.DoSelectionChanged(Sender: TObject);
begin
  dxCallNotify(OnSelectionChanged, Self);
end;

procedure TdxRichEditControlBase.DoShortCut(Args: TdxRichEditShortCutEventArgs);
begin
  if Assigned(FOnShortCut) then
    FOnShortCut(Self, Args);
end;

procedure TdxRichEditControlBase.DoZoomChanged(Sender: TObject);
begin
  dxCallNotify(OnZoomChanged, Self);
end;

procedure TdxRichEditControlBase.DoCalculateDocumentVariable(Sender: TObject; Args: TdxCalculateDocumentVariableEventArgs);
begin
  if Assigned(FOnCalculateDocumentVariable) then
    FOnCalculateDocumentVariable(Self, Args);
end;

procedure TdxRichEditControlBase.DoCustomizeMergeFields(Sender: TObject; const Args: TdxCustomizeMergeFieldsEventArgs);
begin
  if Assigned(FOnCustomizeMergeFields) then
    FOnCustomizeMergeFields(Self, Args);
end;

procedure TdxRichEditControlBase.DoMailMergeFinished(Sender: TObject; const Args: TdxMailMergeFinishedEventArgs);
begin
  if Assigned(FOnMailMergeFinished) then
    FOnMailMergeFinished(Self, Args);
end;

procedure TdxRichEditControlBase.DoMailMergeGetTargetDocument(Sender: TObject;
  const Args: TdxMailMergeGetTargetDocumentEventArgs);
begin
  if Assigned(FOnMailMergeGetTargetDocument) then
    FOnMailMergeGetTargetDocument(Self, Args);
end;

procedure TdxRichEditControlBase.DoMailMergeRecordFinished(Sender: TObject; const Args: TdxMailMergeRecordFinishedEventArgs);
begin
  if Assigned(FOnMailMergeRecordFinished) then
    FOnMailMergeRecordFinished(Self, Args);
end;

procedure TdxRichEditControlBase.DoMailMergeRecordStarted(Sender: TObject; const Args: TdxMailMergeRecordStartedEventArgs);
begin
  if Assigned(FOnMailMergeRecordStarted) then
    FOnMailMergeRecordStarted(Self, Args);
end;

procedure TdxRichEditControlBase.DoMailMergeStarted(Sender: TObject; const Args: TdxMailMergeStartedEventArgs);
begin
  if Assigned(FOnMailMergeStarted) then
    FOnMailMergeStarted(Self, Args);
end;

function TdxRichEditControlBase.CreateRichEditViewVerticalScrollController(ARichEditView: TdxRichEditView): TdxRichEditViewVerticalScrollController;
begin
  Result := TdxWinFormsRichEditViewVerticalScrollController.Create(ARichEditView);
end;

function TdxRichEditControlBase.DefaultLayoutUnit: TdxDocumentLayoutUnit;
begin
  Result := TdxDocumentLayoutUnit.Pixel;
end;

procedure TdxRichEditControlBase.EndInitialize;
begin
  FPainter := CreatePainter;
  EndInitializeCommon;
  FCaret := TdxCaret.Create;
end;

procedure TdxRichEditControlBase.EndInitializeCommon;
begin
  SubscribeInnerControlEvents;
  InnerControl.EndInitialize;
end;

procedure TdxRichEditControlBase.DisposeCommon;
begin
  FInnerControl.StopFormatting;
  TdxUIThreadSyncService.Unsubscribe(Self);
  UnsubscribeInnerControlEvents;

  FInnerControl.Free;
  FInnerControl := nil;

  FreeAndNil(FHorizontalScrollBar);
  FreeAndNil(FVerticalScrollBar);
end;

procedure TdxRichEditControlBase.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  UpdateSkinInfo;
  RecreateBackgroundPainter(ActiveView);
  RecreateViewPainter(ActiveView);
  OnLookAndFeelChanged;
end;

procedure TdxRichEditControlBase.OnLookAndFeelChanged;
begin
end;

function TdxRichEditControlBase.GetPixelPhysicalBounds(APageViewInfo: TdxPageViewInfo; ALogicalBounds: TRect): TRect;
var
  APhysicalBounds: TRect;
begin
  if APageViewInfo = nil then
    Exit(cxNullRect);
  APhysicalBounds := ActiveView.CreatePhysicalRectangle(APageViewInfo, ALogicalBounds);
  APhysicalBounds := cxRectOffset(APhysicalBounds, ViewBounds.Left, ViewBounds.Top);
  Result := ActiveView.DocumentLayout.UnitConverter.LayoutUnitsToPixels(APhysicalBounds, DpiX, DpiY);
end;

procedure TdxRichEditControlBase.EndUpdate;
begin
  if InnerControl <> nil then
    InnerControl.EndUpdate;
end;

procedure TdxRichEditControlBase.PerformRulersResize;
begin
end;

procedure TdxRichEditControlBase.UpdateVerticalRuler;
begin
end;

procedure TdxRichEditControlBase.UpdateHorizontalRuler;
begin
end;

procedure TdxRichEditControlBase.UpdateRulers;
begin
end;

procedure TdxRichEditControlBase.UpdateRulersCore;
begin
end;

procedure TdxRichEditControlBase.UpdateSkinInfo;
begin
  if LookAndFeel.ActiveStyle = lfsSkin then
    LookAndFeel.Painter.GetPainterData(FSkinInfo)
  else
    FSkinInfo := nil;
end;

procedure TdxRichEditControlBase.InitializeScrollBars;
begin
  FHorizontalScrollBar.Parent := Self;
  FHorizontalScrollBar.HandleNeeded;
  FVerticalScrollBar.Parent := Self;
  FVerticalScrollBar.HandleNeeded;
end;

procedure TdxRichEditControlBase.InitializeRulers;
begin
end;

function TdxRichEditControlBase.IsScrollBarsArea(
  const APoint: TPoint): Boolean;
begin
  Result := inherited IsScrollBarsArea(APoint);
  if not Result then
    Result := FHorizontalScrollBar.Visible and FHorizontalScrollBar.BoundsRect.Contains(APoint);
  if not Result then
    Result := FVerticalScrollBar.Visible and FVerticalScrollBar.BoundsRect.Contains(APoint);
end;

function TdxRichEditControlBase.IsScrollBarsCapture: Boolean;
var
  ACaptureControl: TControl;
begin
  ACaptureControl := GetCaptureControl;
  Result := (ACaptureControl = FVerticalScrollBar) or (ACaptureControl = FHorizontalScrollBar);
end;

function TdxRichEditControlBase.CanUndo: Boolean;
begin
  Result := InnerControl.CanUndo;
end;

procedure TdxRichEditControlBase.Undo;
begin
  InnerControl.Undo;
end;

function TdxRichEditControlBase.CanRedo: Boolean;
begin
  Result := InnerControl.CanRedo;
end;

procedure TdxRichEditControlBase.Redo;
begin
  InnerControl.Redo;
end;

procedure TdxRichEditControlBase.ClearUndo;
begin
  InnerControl.ClearUndo;
end;

procedure TdxRichEditControlBase.Cut;
begin
  InnerControl.Cut;
end;

procedure TdxRichEditControlBase.Copy;
begin
  InnerControl.Copy;
end;

procedure TdxRichEditControlBase.Paste;
begin
  InnerControl.Paste;
end;

procedure TdxRichEditControlBase.SelectAll;
begin
  InnerControl.SelectAll;
end;

procedure TdxRichEditControlBase.DeselectAll;
begin
  InnerControl.DeselectAll;
end;

function TdxRichEditControlBase.IsSelectionInTable: Boolean;
begin
  Result := DocumentModel.Selection.IsWholeSelectionInOneTable;
end;

function TdxRichEditControlBase.IsFloatingObjectSelected: Boolean;
begin
  Result := DocumentModel.Selection.IsFloatingObjectSelected;
end;

function TdxRichEditControlBase.IsSelectionInHeader: Boolean;
begin
  Result := DocumentModel.Selection.PieceTable.IsHeader;
end;

function TdxRichEditControlBase.IsSelectionInFooter: Boolean;
begin
  Result := DocumentModel.Selection.PieceTable.IsFooter;
end;

function TdxRichEditControlBase.IsSelectionInHeaderOrFooter: Boolean;
begin
  Result := DocumentModel.Selection.PieceTable.IsHeaderFooter;
end;


function TdxRichEditControlBase.IsSelectionInTextBox: Boolean;
begin
  Result := DocumentModel.Selection.PieceTable.IsTextBox;
end;

procedure TdxRichEditControlBase.EnsureImagesLoadComplete;
begin
  DocumentModel.EnsureImagesLoadComplete;
  Assert(GetCurrentThreadID = MainThreadID);
  Application.ProcessMessages;
end;

function TdxRichEditControlBase.IsSizeGripArea(const APoint: TPoint): Boolean;
begin
  Result := SizeGripBounds.Contains(APoint);
end;

function TdxRichEditControlBase.GetCanShowNumberingListForm: Boolean;
begin
  Result := False;
end;

function TdxRichEditControlBase.GetInnerControl: IdxInnerControl;
begin
  Result := InnerControl;
end;

function TdxRichEditControlBase.GetRichEditControl: IdxRichEditControl;
begin
  Result := Self;
end;

function TdxRichEditControlBase.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

function TdxRichEditControlBase.GetActiveViewType: TdxRichEditViewType;
begin
  Result := InnerControl.ActiveViewType;
end;

function TdxRichEditControlBase.GetBackgroundThreadUIUpdater: TdxBackgroundThreadUIUpdater;
begin
  if InnerControl <> nil then
    Result := InnerControl.BackgroundThreadUIUpdater
  else
    Result := nil;
end;

function TdxRichEditControlBase.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxRichEditControlBase.CaretTimerHandler(Sender: TObject);
begin
  if IsUpdateLocked then
    Exit;
  FCaret.IsHidden := not Caret.IsHidden;
  Painter.DrawCaret;
end;

function TdxRichEditControlBase.GetControlDeferredChanges: TdxRichEditControlDeferredChanges;
begin
  if InnerControl <> nil then
    Result := InnerControl.ControlDeferredChanges
  else
    Result := nil;
end;

function TdxRichEditControlBase.GetCursor: TCursor;
begin
  Result := Cursor;
end;

function TdxRichEditControlBase.GetDocumentModel: TdxDocumentModel;
begin
  if InnerControl <> nil then
    Result := InnerControl.DocumentModel
  else
    Result := nil;
end;

function TdxRichEditControlBase.InternalGetDocumentLayout: TdxRichEditDocumentLayout;
begin
   if InnerControl <> nil then
     Result := InnerControl.DocumentLayout
   else
     Result := nil;
end;

function TdxRichEditControlBase.GetMeasurementUnit: TdxMeasurementUnit;
begin
  if InnerControl = nil then
    Result := TdxMeasurementUnit.Document
  else
    Result := InnerControl.MeasurementUnit;
end;

function TdxRichEditControlBase.GetReadOnly: Boolean;
begin
  Result := (InnerControl = nil) or InnerControl.ReadOnly;
end;

function TdxRichEditControlBase.IsActiveViewTypeStored: Boolean;
begin
  if InnerControl <> nil then
    Result := GetActiveViewType <> Views[0].&Type
  else
    Result := True;
end;

function TdxRichEditControlBase.GetViews: TdxRichEditCustomViewRepository;
begin
  if InnerControl <> nil then
    Result := TdxRichEditCustomViewRepository(InnerControl.Views)
  else
    Result := nil;
end;

function TdxRichEditControlBase.GetDpiX: Single;
begin
  Result := DocumentModel.DpiX;
end;

function TdxRichEditControlBase.GetDpiY: Single;
begin
  Result := DocumentModel.DpiY;
end;

function TdxRichEditControlBase.GetLayoutUnit: TdxDocumentLayoutUnit;
begin
  if InnerControl <> nil then
    Result := InnerControl.LayoutUnit
  else
    Result := DefaultLayoutUnit;
end;

function TdxRichEditControlBase.GetDocument: IdxRichEditDocument;
begin
  if InnerControl <> nil then
    Result := InnerControl.Document
  else
    Result := nil;
end;

function TdxRichEditControlBase.GetMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
begin
  if InnerControl <> nil then
    Result := InnerControl.MeasurementAndDrawingStrategy
  else
    Result := nil;
end;

function TdxRichEditControlBase.GetModified: Boolean;
begin
  if InnerControl <> nil then
    Result := InnerControl.Modified
  else
    Result := False;
end;

function TdxRichEditControlBase.GetMouseController: TdxRichEditCustomMouseController;
begin
  Result := InnerControl.MouseController;
end;

function TdxRichEditControlBase.GetOptions: TdxRichEditControlCustomOptions;
begin
  Result := TdxRichEditControlCustomOptions(InnerControl.Options);
end;

procedure TdxRichEditControlBase.HideCaret;
begin
  if Focused then
    HideCaretCore;
end;

class function TdxRichEditControlBase.CreateSystemCaretBitmap: TcxBitmap;
begin
  Result := TcxBitmap.CreateSize(16, 16);
  Result.Canvas.Brush.Color := clBlack;
  Result.Canvas.FillRect(Result.ClientRect);
end;

procedure TdxRichEditControlBase.EnsureSystemCaretCreated;
begin
  if not HandleAllocated or not Focused then
    Exit;
  DestroyCaret;
  if not IsPopupMenuShown then
    CreateCaret(Handle, FSystemCaretBitmap.Handle, 0, 0);
end;

procedure TdxRichEditControlBase.HideCaretCore;
begin
  if IsUpdateLocked then
    Exit;
  EnsureSystemCaretCreated;
  if not Caret.IsHidden then
  begin
    HideSystemCaret;
    Painter.DrawCaret;
    Caret.IsHidden := True;
  end;
end;

procedure TdxRichEditControlBase.InitializeControl;
var
  ADeferredUpdater: TdxDeferredBackgroundThreadUIUpdater;
begin
  FControlCreated := True;
  inherited InitializeControl;
  OleCheck(RegisterDragDrop(Handle, Self));

  BeginUpdate;
  try
    ApplyFontAndForeColor;
    OnLookAndFeelChanged;
    InitializeFlushPendingTextInputTimer;
    InitializeVerticalScrollBarUpdateTimer;
    ADeferredUpdater := BackgroundThreadUIUpdater as TdxDeferredBackgroundThreadUIUpdater;
    try
      InnerControl.BackgroundThreadUIUpdater := TdxBeginInvokeBackgroundThreadUIUpdater.Create;
      PerformDeferredUIUpdates(ADeferredUpdater);
    finally
      FreeAndNil(ADeferredUpdater);
    end;

    InnerControl.UpdateUIOnIdle := True;
    InitializeScrollBars;
    InitializeRulers;
    OnResizeCore(True);
    InnerControl.UpdateUIOnIdle := False;

    OnUpdateUI;
    UpdateControlAutoSize;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditControlBase.FinalizeControl;
begin
  RevokeDragDrop(Handle);
  if InnerControl <> nil then
    InnerControl.UpdateUIOnIdle := False;
  OnUpdateUI;

  StopCaretTimer;
  DestroyCaret;
  DestroyVerticalScrollBarUpdateTimer;
  DestroyFlushPendingTextInputTimer;

  BackgroundThreadUIUpdater.Free;
  if InnerControl <> nil then
    InnerControl.BackgroundThreadUIUpdater := TdxDeferredBackgroundThreadUIUpdater.Create;

  inherited FinalizeControl;
end;

function TdxRichEditControlBase.IsUpdateLocked: Boolean;
begin
  Result := FControlCreated and (InnerControl <> nil) and InnerControl.IsUpdateLocked;
end;

procedure TdxRichEditControlBase.PerformDeferredUIUpdates(ADeferredUpdater: TdxDeferredBackgroundThreadUIUpdater);
var
  ACount, I: Integer;
  ADeferredUpdates: TList<TdxOwnedProcedure>;
begin
  ADeferredUpdates := ADeferredUpdater.Updates;
  ACount := ADeferredUpdates.Count;
  for I := 0 to ACount - 1 do
    BackgroundThreadUIUpdater.UpdateUI(ADeferredUpdates[I]);
end;

procedure TdxRichEditControlBase.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_TAB) and not WantTabs then
    Key := 0;
  if (Key = VK_RETURN) and not WantReturns then
    Key := 0;
  if Key <> 0 then
    KeyboardController.KeyDown(Key, Shift);
end;

procedure TdxRichEditControlBase.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  KeyboardController.KeyPress(Key);
end;

procedure TdxRichEditControlBase.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  KeyboardController.KeyUp(Key, Shift);
end;

function TdxRichEditControlBase.CreateNewDocument(ARaiseDocumentClosing: Boolean = False): Boolean;
begin
  if InnerControl <> nil then
    Result := InnerControl.CreateNewDocument(ARaiseDocumentClosing)
  else
    Result := True;
end;

procedure TdxRichEditControlBase.LoadDocumentTemplate(const AFileName: string);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocumentTemplate(AFileName);
end;

procedure TdxRichEditControlBase.LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocumentTemplate(AFileName, ADocumentFormat);
end;

procedure TdxRichEditControlBase.LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocument(AStream, ADocumentFormat);
end;

procedure TdxRichEditControlBase.LoadDocument(const AFileName: string);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocument(AFileName);
end;

procedure TdxRichEditControlBase.LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocument(AFileName, ADocumentFormat);
end;

procedure TdxRichEditControlBase.LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.LoadDocument(AStream, ADocumentFormat);
end;

procedure TdxRichEditControlBase.SaveDocument;
begin
  if InnerControl <> nil then
    InnerControl.SaveDocument;
end;

procedure TdxRichEditControlBase.SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.SaveDocument(AFileName, ADocumentFormat);
end;

procedure TdxRichEditControlBase.SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerControl <> nil then
    InnerControl.SaveDocument(AStream, ADocumentFormat);
end;

procedure TdxRichEditControlBase.SaveDocument(const AFileName: string);
var
  AFormat: TdxRichEditDocumentFormat;
begin
  if InnerControl <> nil then
  begin
    if Length(TPath.GetExtension(AFileName)) > 1 then
    begin
      AFormat := DocumentModel.AutodetectDocumentFormat(AFileName, False);
      InnerControl.SaveDocument(AFileName, AFormat);
    end
    else
    begin
      AFormat := Options.DocumentSaveOptions.DefaultFormat;
      InnerControl.SaveDocument(TPath.ChangeExtension(AFileName, TdxExportFileFormats.GetFileExtension(AFormat)), AFormat);
    end;
  end;
end;

procedure TdxRichEditControlBase.OnOptionsChangedPlatformSpecific(E: TdxOptionChangedEventArgs);
begin
  OnDeferredResizeCore;
end;

procedure TdxRichEditControlBase.OnResizeCore;
begin
  OnResizeCore(True);
end;

procedure TdxRichEditControlBase.OnResizeCore(AEnsureCaretVisibleOnResize: Boolean);
begin
  OnResizeCore(AEnsureCaretVisibleOnResize, True);
end;

procedure TdxRichEditControlBase.OnZoomFactorChangingPlatformSpecific;
begin
end;

procedure TdxRichEditControlBase.OnResizeCore(AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean);
var
  AInitialClientBounds: TRect;
  AInitialWidth, AInitialHeight: Integer;
begin
  AInitialWidth := Width;
  AInitialHeight := Height;
  AInitialClientBounds := CalculateInitialClientBounds;
  while True do
  begin
    UpdateScrollbarsVisibility;
    if not PerformResize(AInitialClientBounds, AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition) then
      Break;
    if (AInitialWidth <> Width) or (AInitialHeight <> Height) then
    begin
      AInitialWidth := Width;
      AInitialHeight := Height;
      AInitialClientBounds := CalculateInitialClientBounds;
    end;
  end;
  UpdateRulersCore;
  UpdateVerticalScrollBar(False);
end;

procedure TdxRichEditControlBase.OnViewPaddingChanged;
begin
  BeginUpdate;
  try
    OnResizeCore(True);
    RedrawEnsureSecondaryFormattingComplete;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditControlBase.OnUpdateUI;
begin
  if InnerControl <> nil then
    InnerControl.OnUpdateUI;
end;

procedure TdxRichEditControlBase.UpdateVerticalScrollBar(AAvoidJump: Boolean);
begin
  if InnerControl <> nil then
    InnerControl.UpdateVerticalScrollBar(AAvoidJump);
end;
function TdxRichEditControlBase.GetVerticalScrollbarBounds(AWidth, AOffset, AHorizontalScrollbarHeight: Integer): TRect;
begin
  Result.InitSize(FClientBounds.Right - AWidth, FClientBounds.Top + AOffset, AWidth, FClientBounds.Height - AOffset - AHorizontalScrollbarHeight);
end;

procedure TdxRichEditControlBase.InitializeVerticalScrollBarUpdateTimer;
begin
  FVerticalScrollBarUpdateTimer := TcxTimer.Create(nil);
  FVerticalScrollBarUpdateTimer.Interval := 3000;
  FVerticalScrollBarUpdateTimer.OnTimer := OnVerticalScrollBarUpdateTimer;
  FVerticalScrollBarUpdateTimer.Enabled := True;
end;

procedure TdxRichEditControlBase.DestroyVerticalScrollBarUpdateTimer;
begin
  if FVerticalScrollBarUpdateTimer <> nil then
  begin
    FVerticalScrollBarUpdateTimer.Enabled := False;
    FreeAndNil(FVerticalScrollBarUpdateTimer);
  end;
end;

procedure TdxRichEditControlBase.OnVerticalScrollBarUpdateTimer(ASender: TObject);
begin
  if IsUpdateLocked then
    Exit;

  UpdateVerticalScrollBar(True);
end;

procedure TdxRichEditControlBase.ResizeView(AEnsureCaretVisibleOnResize: Boolean);
begin
  ResizeView(AEnsureCaretVisibleOnResize, True);
end;

procedure TdxRichEditControlBase.ResizeView(AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean);
var
  ANormalizedViewBounds: TRect;
begin
  FViewBounds := CalculateViewBounds(ClientBounds);
  ANormalizedViewBounds := FViewBounds;
  ANormalizedViewBounds.Offset(-FViewBounds.Left, -FViewBounds.Top);
  ActiveView.OnResize(ANormalizedViewBounds, AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition);
end;

function TdxRichEditControlBase.PerformResize(const AInitialClientBounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean): Boolean;
var
  AVerticalScrollbarWidth, AHorizontalScrollbarHeight, AVerticalScrollbarHeight: Integer;
begin
  Assert(not DocumentModel.IsUpdateLocked);
  FClientBounds := AInitialClientBounds;
  AVerticalScrollbarWidth := CalculateVerticalScrollbarWidth;
  AHorizontalScrollbarHeight := CalculateHorizontalScrollbarHeight;

  if HorizontalScrollbar.IsOverlapScrollBar then
    AVerticalScrollbarHeight := 0
  else
    AVerticalScrollbarHeight := AHorizontalScrollbarHeight;
  VerticalScrollbar.BoundsRect := GetVerticalScrollbarBounds(AVerticalScrollbarWidth, 0, AVerticalScrollbarHeight);

  HorizontalScrollbar.BoundsRect := TRect.CreateSize(FClientBounds.Left, FClientBounds.Bottom - AHorizontalScrollbarHeight, FClientBounds.Width - AVerticalScrollbarWidth, AHorizontalScrollbarHeight);

  if not HorizontalScrollbar.IsOverlapScrollBar then
  begin
    Dec(FClientBounds.Right, AVerticalScrollbarWidth);
    Dec(FClientBounds.Bottom, AHorizontalScrollbarHeight);
    FSizeGripBounds.Init(
      HorizontalScrollbar.BoundsRect.Right, VerticalScrollbar.BoundsRect.Bottom,
      FClientBounds.Right + AVerticalScrollbarWidth, FClientBounds.Bottom + AHorizontalScrollbarHeight);
  end
  else
    FSizeGripBounds.Empty;

  PerformRulersResize;


  ResizeView(AEnsureCaretVisibleonResize, AEnsureOptimalHorizontalScrollbarPosition);
  Result := AHorizontalScrollbarHeight <> CalculateHorizontalScrollbarHeight;
end;

procedure TdxRichEditControlBase.RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions);
var
  AInnerControl: TdxInnerRichEditControl;
begin
  if [TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated, TdxDocumentModelChangeAction.RaiseDocumentLoaded] * AChangeActions <> [] then
    OnBackColorChanged;

  AInnerControl := InnerControl;
  if (AInnerControl <> nil) and not IsDestroying then
    TdxUIThreadSyncService.EnqueueInvokeInUIThread(
      AInnerControl,
      procedure
      begin
        InnerControl.RaiseDeferredEventsCore(AChangeActions);
      end);
end;

procedure TdxRichEditControlBase.CustomRefresh;
begin
  FIsInsideRefresh := True;
  try
    UpdateControlAutoSize;
    Repaint;
  finally
    FIsInsideRefresh := False;
  end;
end;

function TdxRichEditControlBase.CreatePainter: TdxRichEditControlPainter;
begin
  Result := TdxRichEditControlPainter.Create(Self);
end;

procedure TdxRichEditControlBase.Redraw;
begin
  Redraw(False);
end;

procedure TdxRichEditControlBase.Redraw(AAfterEndUpdate: Boolean);
begin
  if IsUpdateLocked then
    ControlDeferredChanges.Redraw := True
  else
    if not AAfterEndUpdate then
      CustomRefresh
    else
      if HandleAllocated and not FIsInsideRefresh then
        BackgroundThreadUIUpdater.UpdateUI(Self, CustomRefresh);
end;

procedure TdxRichEditControlBase.Redraw(AAction: TdxRefreshAction);
begin
  Redraw(False);
end;

procedure TdxRichEditControlBase.RedrawAfterEndUpdate;
begin
  Redraw(True);
end;

procedure TdxRichEditControlBase.RedrawEnsureSecondaryFormattingComplete;
begin
  if IsUpdateLocked then
  begin
    InnerControl.BeginDocumentRendering;
    InnerControl.EndDocumentRendering;
    ControlDeferredChanges.Redraw := True;
  end
  else
    CustomRefresh;
end;

procedure TdxRichEditControlBase.BoundsChanged;
var
  AOldPainter: TdxRichEditControlPainter;
  ANestedResize: Boolean;
  ABestSize: TSize;
begin
  if CreatingWindow or not HandleAllocated then
    Exit;
  ANestedResize := FInsideResize;
  FInsideResize := True;
  BeginUpdate;
  try
    AOldPainter := FPainter;
    FPainter := TdxEmptyRichEditControlPainter.Create(Self);
    try
      inherited BoundsChanged;
      OnResizeCore(False);
      if not ANestedResize and ((AutoSizeMode = TdxRichEditAutoSizeMode.Both) or (AutoSizeMode = TdxRichEditAutoSizeMode.Vertical)) then
      begin
        ABestSize := CalcBestSize(Width, Height, True);
        SetBounds(Left, Top, ABestSize.cx, ABestSize.cy);
      end;
    finally
      FreeAndNil(FPainter);
      FPainter := AOldPainter;
    end;
  finally
    EndUpdate;
    FInsideResize := False;
  end;
end;


procedure TdxRichEditControlBase.FocusEnter;
begin
  inherited DoEnter;
  if ShouldShowCaret then
    StartCaretBlinking;
  StartPendingInput;
  InnerControl.OnUpdateUI;
end;

procedure TdxRichEditControlBase.FocusLeave;
begin
  inherited FocusLeave;
  if IsDestroying then
    Exit;
  if not ShouldShowCaret then
  begin
    StopCaretBlinking;
    Windows.DestroyCaret;
  end;
  StopPendingInput;
  InnerControl.OnUpdateUI;
end;

procedure TdxRichEditControlBase.EnabledChanged;
begin
  inherited EnabledChanged;
  if Enabled and Focused then
  begin
    if ShouldShowCaret then
      StartCaretBlinking;
    StartPendingInput;
  end
  else
  begin
    if not ShouldShowCaret then
      StopCaretBlinking;
    StopPendingInput;
  end;
  VerticalScrollBar.Enabled := Enabled;
  HorizontalScrollBar.Enabled := Enabled;

  OnUpdateUI;
end;

procedure TdxRichEditControlBase.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbMiddle then
    DoScrolling
  else
    MouseController.MouseDown(Button, Shift, X, Y);
end;

procedure TdxRichEditControlBase.MouseMove(Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  MouseController.MouseMove(Shift, X, Y);
end;

procedure TdxRichEditControlBase.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseController.MouseUp(Button, Shift, X, Y);
end;

function TdxRichEditControlBase.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    Result := MouseController.MouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TdxRichEditControlBase.AddKeyboardService(const AService: IdxKeyboardHandlerService);
begin
  KeyboardController.AddHandler(AService);
end;

procedure TdxRichEditControlBase.AddService(AServiceType: TdxServiceType; ACallback: IdxServiceCreatorCallback; APromote: Boolean);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).AddService(AServiceType, ACallback, APromote);
end;

procedure TdxRichEditControlBase.AddService(AServiceType: TdxServiceType; ACallback: IdxServiceCreatorCallback);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).AddService(AServiceType, ACallback);
end;

procedure TdxRichEditControlBase.AddService(AServiceType: TdxServiceType; AServiceInstance: IInterface; APromote: Boolean);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).AddService(AServiceType, AServiceInstance, APromote);
end;

procedure TdxRichEditControlBase.AddService(AServiceType: TdxServiceType; AServiceInstance: IInterface);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).AddService(AServiceType, AServiceInstance);
end;

procedure TdxRichEditControlBase.RemoveService(AServiceType: TdxServiceType; APromote: Boolean);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).RemoveService(AServiceType, APromote);
end;

procedure TdxRichEditControlBase.RemoveService(AServiceType: TdxServiceType);
begin
  if InnerControl <> nil then
    (InnerControl as IdxServiceContainer).RemoveService(AServiceType);
end;

function TdxRichEditControlBase.GetKeyboardController: TdxCustomKeyboardController;
begin
  Result := InnerControl.KeyboardController;
end;

procedure TdxRichEditControlBase.InitializeFlushPendingTextInputTimer;
begin
  FFlushPendingTextInputTimer := TcxTimer.Create(nil);
  FFlushPendingTextInputTimer.Enabled := False;
  FFlushPendingTextInputTimer.Interval := TdxRichEditKeyboardDefaultHandler.PendingTextFlushMilliseconds;
  FFlushPendingTextInputTimer.OnTimer := OnFlushPendingTextInputTimerTick;
end;

procedure TdxRichEditControlBase.DestroyFlushPendingTextInputTimer;
begin
  FreeAndNil(FFlushPendingTextInputTimer);
end;

procedure TdxRichEditControlBase.OnFlushPendingTextInputTimerTick(ASender: TObject);
begin
  if IsUpdateLocked then
    Exit;
  ForceFlushPendingTextInput;
end;

procedure TdxRichEditControlBase.ForceFlushPendingTextInput;
var
  AHandler: TdxRichEditKeyboardDefaultHandler;
begin
  AHandler := TdxRichEditKeyboardDefaultHandler(KeyboardController.Handler);
  if AHandler <> nil then
    AHandler.FlushPendingTextInput;
end;

procedure TdxRichEditControlBase.StartPendingInput;
begin
  if FlushPendingTextInputTimer <> nil then
    FlushPendingTextInputTimer.Enabled := True;
end;

procedure TdxRichEditControlBase.StopPendingInput;
begin
  if FlushPendingTextInputTimer <> nil then
    FlushPendingTextInputTimer.Enabled := False;
end;

function TdxRichEditControlBase.GetKeyboardHandler: IdxKeyboardHandlerService;
begin
  Result := KeyboardController.Handler;
end;

procedure TdxRichEditControlBase.RemoveKeyboardService(const AService: IdxKeyboardHandlerService);
begin
  KeyboardController.RemoveHandler(AService);
end;

procedure TdxRichEditControlBase.SetCursor(Value: TCursor);
begin
    Cursor := Value;
end;

procedure TdxRichEditControlBase.UpdateUIFromBackgroundThread(const AMethod: TdxAction);
begin
  BackgroundThreadUIUpdater.UpdateUI(Self, AMethod);
end;

function TdxRichEditControlBase.ShowWarningMessage(const AMessage: string): TModalResult;
begin
  Result := MessageDlg(AMessage, mtWarning, [mbOK], 0);
end;

function TdxRichEditControlBase.ShowErrorMessage(const AMessage: string): TModalResult;
begin
  Result := MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure TdxRichEditControlBase.SetActiveViewType(const Value: TdxRichEditViewType);
begin
  if InnerControl <> nil then
    InnerControl.ActiveViewType := Value;
end;

procedure TdxRichEditControlBase.SetAutoSizeMode(const Value: TdxRichEditAutoSizeMode);
begin
  if FAutoSizeMode <> Value then
  begin
    FAutoSizeMode := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TdxRichEditControlBase.SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
begin
  if InnerControl <> nil then
    InnerControl.LayoutUnit := Value;
end;

procedure TdxRichEditControlBase.SetModified(const Value: Boolean);
begin
  if InnerControl <> nil then
    InnerControl.Modified := Value;
end;

procedure TdxRichEditControlBase.SetMeasurementUnit(const Value: TdxMeasurementUnit);
begin
  if InnerControl <> nil then
    InnerControl.MeasurementUnit := Value;
end;

procedure TdxRichEditControlBase.SetViews(const Value: TdxRichEditCustomViewRepository);
begin
  if Views <> nil then
    Views.Assign(Value);
end;

procedure TdxRichEditControlBase.SetWantReturns(const Value: Boolean);
begin
  if FWantReturns <> Value then
  begin
    FWantReturns := Value;
    Changed;
  end;
end;

procedure TdxRichEditControlBase.SetWantTabs(const Value: Boolean);
begin
  if FWantTabs <> Value then
  begin
    FWantTabs := Value;
    Changed;
  end;
end;

procedure TdxRichEditControlBase.CMNCSizeChanged(var Message: TMessage);
var
  ANewHeight, ANewWidth: Integer;
begin
  if not HandleAllocated then
    Exit;
  ANewHeight := dxGetSystemMetrics(SM_CYHSCROLL, ScaleFactor);
  ANewWidth  := dxGetSystemMetrics(SM_CXVSCROLL, ScaleFactor);
  if (FHorizontalScrollBar.Height <> ANewHeight) or (FVerticalScrollBar.Width <> ANewWidth) then
  begin
    FHorizontalScrollBar.Height := ANewHeight;
    FVerticalScrollBar.Width := ANewWidth;
    BoundsChanged;
  end;
end;

procedure TdxRichEditControlBase.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FWantTabs then
    Message.Result := Message.Result or DLGC_WANTTAB
  else
    Message.Result := Message.Result and not DLGC_WANTTAB;
  if not FWantReturns then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TdxRichEditControlBase.SetOptions(const Value: TdxRichEditControlCustomOptions);
begin
  InnerControl.Options.Assign(Value);
end;

procedure TdxRichEditControlBase.SetShowCaretInReadOnly(const Value: Boolean);
begin
  if ShowCaretInReadOnly = Value then
    Exit;
  FShowCaretInReadOnly := Value;
  DoReadOnlyChangedPlatformSpecific;
end;

function TdxRichEditControlBase.ShouldShowCaret: Boolean;
begin
  if not Enabled or not Focused then
    Exit(False);
  if ShowCaretInReadOnly then
    Result := True
  else
    Result := not ReadOnly;
end;

procedure TdxRichEditControlBase.StartCaretBlinking;
begin
  StopCaretTimer;
  StartCaretTimer;
  ShowCaretCore;
end;

procedure TdxRichEditControlBase.StopCaretBlinking;
begin
  StopCaretTimer;
  HideCaretCore;
end;

procedure TdxRichEditControlBase.StartCaretTimer;
begin
  if FCaretTimer.Enabled then
    FCaretTimer.Enabled := False;
  FCaretTimer.Interval := GetCaretBlinkTime;
  FCaretTimer.OnTimer := CaretTimerHandler;
  FCaretTimer.Enabled := True;
end;

procedure TdxRichEditControlBase.StopCaretTimer;
begin
  if FCaretTimer <> nil then
    FCaretTimer.Enabled := False;
end;

procedure TdxRichEditControlBase.ShowBookmarkForm;
begin
end;

procedure TdxRichEditControlBase.ShowCaret;
begin
  if ShouldShowCaret then
    ShowCaretCore;
end;

procedure TdxRichEditControlBase.ShowCaretCore;
begin
  if IsUpdateLocked then
    Exit;
  StopCaretTimer;
  EnsureSystemCaretCreated;
  if FCaret.IsHidden then
  begin
    ShowSystemCaret;
    Painter.DrawCaret;
    FCaret.IsHidden := False;
  end;
  FCaretTimer.Enabled := True;
end;

procedure TdxRichEditControlBase.ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI;
  const ACallback: TdxShowColumnsSetupFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowDeleteTableCellsForm(const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditControlBase.ShowDocumentEncryptQueryNewPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowDocumentProtectionQueryNewPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowDocumentProtectionQueryPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowEditStyleForm(ACharacterSourceStyle: TdxCharacterStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
end;

procedure TdxRichEditControlBase.ShowSystemCaret;
var
  ACaretBounds: TRect;
begin
  if not HandleAllocated then
    Exit;
  if IsPopupMenuShown then
    Exit;
  ACaretBounds := ActiveView.GetCursorBounds;
  if Bounds.Contains(ACaretBounds.Location) then
  begin
    Windows.SetCaretPos(ACaretBounds.Left, ACaretBounds.Top);
    Windows.ShowCaret(Handle);
  end
  else
  begin
    Windows.HideCaret(Handle);
    Windows.DestroyCaret;
  end;
end;

procedure TdxRichEditControlBase.HideSystemCaret;
var
  ACaretBounds: TRect;
begin
  if not HandleAllocated then
    Exit;
  ACaretBounds := ActiveView.GetCursorBounds;
  if Bounds.Contains(ACaretBounds.Location) then
  begin
    Windows.SetCaretPos(ACaretBounds.Left, ACaretBounds.Top);
    Windows.HideCaret(Handle);
  end
  else
  begin
    Windows.HideCaret(Handle);
    Windows.DestroyCaret;
  end;
end;

procedure TdxRichEditControlBase.SubscribeInnerControlEvents;
begin
  InnerControl.BeforeExport.Add(DoBeforeExport);
  InnerControl.AfterExport.Add(DoAfterExport);
  InnerControl.PlainTextChanged.Add(DoPlainTextChanged);
  InnerControl.DocumentClosing.Add(DoDocumentClosing);
  InnerControl.ContentChanged.Add(DoContentChanged);
  InnerControl.ModifiedChanged.Add(DoModifiedChanged);
  InnerControl.HyperlinkClick.Add(DoHyperlinkClick);
  InnerControl.ReadOnlyChanged.Add(DoReadOnlyChanged);
  InnerControl.ActiveViewChanged.Add(DoActiveViewChanged);
  InnerControl.PageBackgroundChanged.Add(DoPageBackgroundChanged);
  InnerControl.EmptyDocumentCreated.Add(DoEmptyDocumentCreated);
  InnerControl.DocumentLoaded.Add(DoDocumentLoaded);
  InnerControl.DocumentProtectionChanged.Add(DoDocumentProtectionChanged);
  InnerControl.SelectionChanged.Add(DoSelectionChanged);
  InnerControl.ZoomChanged.Add(DoZoomChanged);
  InnerControl.CalculateDocumentVariable.Add(DoCalculateDocumentVariable);
  InnerControl.CustomizeMergeFields.Add(DoCustomizeMergeFields);
  InnerControl.MailMergeFinished.Add(DoMailMergeFinished);
  InnerControl.MailMergeRecordFinished.Add(DoMailMergeRecordFinished);
  InnerControl.MailMergeRecordStarted.Add(DoMailMergeRecordStarted);
  InnerControl.MailMergeStarted.Add(DoMailMergeStarted);
  InnerControl.MailMergeGetTargetDocument.Add(DoMailMergeGetTargetDocument);
end;

procedure TdxRichEditControlBase.UnsubscribeInnerControlEvents;
begin
  InnerControl.BeforeExport.Remove(DoBeforeExport);
  InnerControl.AfterExport.Remove(DoAfterExport);
  InnerControl.PlainTextChanged.Remove(DoPlainTextChanged);
  InnerControl.ContentChanged.Remove(DoContentChanged);
  InnerControl.HyperlinkClick.Remove(DoHyperlinkClick);
  InnerControl.ModifiedChanged.Remove(DoModifiedChanged);
  InnerControl.ReadOnlyChanged.Remove(DoReadOnlyChanged);
  InnerControl.ActiveViewChanged.Remove(DoActiveViewChanged);
  InnerControl.PageBackgroundChanged.Remove(DoPageBackgroundChanged);
  InnerControl.EmptyDocumentCreated.Remove(DoEmptyDocumentCreated);
  InnerControl.DocumentLoaded.Remove(DoDocumentLoaded);
  InnerControl.DocumentProtectionChanged.Remove(DoDocumentProtectionChanged);
  InnerControl.SelectionChanged.Remove(DoSelectionChanged);
  InnerControl.ZoomChanged.Remove(DoZoomChanged);
  InnerControl.CalculateDocumentVariable.Remove(DoCalculateDocumentVariable);
  InnerControl.CustomizeMergeFields.Remove(DoCustomizeMergeFields);
  InnerControl.MailMergeFinished.Remove(DoMailMergeFinished);
  InnerControl.MailMergeRecordFinished.Remove(DoMailMergeRecordFinished);
  InnerControl.MailMergeRecordStarted.Remove(DoMailMergeRecordStarted);
  InnerControl.MailMergeStarted.Remove(DoMailMergeStarted);
  InnerControl.MailMergeGetTargetDocument.Remove(DoMailMergeGetTargetDocument);
end;

function TdxRichEditControlBase.CreateVerticalScrollBar: IdxOfficeScrollbar;
begin
  FVerticalScrollBar := TdxOfficeScrollbar.Create(nil);
  FVerticalScrollBar.Kind := sbVertical;
  Result := CreateScrollBarCore(FVerticalScrollBar);
end;

function TdxRichEditControlBase.CreateHorizontalScrollBar: IdxOfficeScrollbar;
begin
  FHorizontalScrollBar := TdxOfficeScrollbar.Create(nil);
  FHorizontalScrollBar.Kind := sbHorizontal;
  Result := CreateScrollBarCore(FHorizontalScrollBar);
end;

function TdxRichEditControlBase.CreateScrollBarCore(AScrollBar: TdxOfficeScrollbar): IdxOfficeScrollbar;
begin
  AScrollBar.Enabled := False;
  AScrollBar.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  Result := AScrollBar;
end;

procedure TdxRichEditControlBase.UpdateScrollbarsVisibility;
begin
  FVerticalScrollbar.Visible := CalculateVerticalScrollbarVisibility;
  FHorizontalScrollbar.Visible := CalculateHorizontalScrollbarVisibility;
  if FHorizontalScrollbar.Visible then
    FHorizontalScrollbar.Enabled := ActiveView.HorizontalScrollController.IsScrollPossible;
  if FVerticalScrollbar.Visible then
    FVerticalScrollbar.Enabled := ActiveView.VerticalScrollController.IsScrollPossible;
end;

function TdxRichEditControlBase.CalculateVerticalScrollbarVisibility: Boolean;
begin
  case Options.VerticalScrollbar.Visibility of
    TdxRichEditScrollbarVisibility.Hidden:
      Result := False;
  else
    Result := True;
  end;
end;

function TdxRichEditControlBase.CalculateHorizontalScrollbarVisibility: Boolean;
var
  AIsScrollPossible: Boolean;
begin
  case Options.HorizontalScrollbar.Visibility of
    TdxRichEditScrollbarVisibility.Visible:
      Result := True;
    TdxRichEditScrollbarVisibility.Hidden:
      Result := False;
    else
    begin
      AIsScrollPossible := ActiveView.HorizontalScrollController.IsScrollPossible;
      if AIsScrollPossible then
        Exit(True);
      ActiveView.PageViewInfoGenerator.CalculateMaxPageWidth;
      Result := ActiveView.PageViewInfoGenerator.MaxPageWidth > ActiveView.PageViewInfoGenerator.ViewPortBounds.Width + 1;
    end;
  end;
end;

procedure TdxRichEditControlBase.ScrollContent(ADirection: TcxDirection);
begin
  case ADirection of
    dirUp:
      ActiveView.VerticalScrollController.ScrollLineUp;
    dirDown:
      ActiveView.VerticalScrollController.ScrollLineDown;
  else
    Exit;
  end;
end;

procedure TdxRichEditControlBase.ScrollContentByGesture(ADeltaX, ADeltaY: Integer);
begin
  if ADeltaY <> 0 then
    VerticalScrollBar.Position := VerticalScrollBar.Position - Trunc(ADeltaY / ActiveView.ScaleFactor);
  if ADeltaX <> 0 then
    HorizontalScrollBar.Position := HorizontalScrollBar.Position - Trunc(ADeltaX / ActiveView.ScaleFactor);
end;

function TdxRichEditControlBase.UseStandardDragDropMode: Boolean;
begin
  Result := True;
end;

end.
