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

unit dxRichEdit.Inplace.Render;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Windows, Classes, Controls, Graphics, Variants, ActiveX,
  dxCore, dxCoreClasses, dxCoreGraphics, cxLookAndFeels, cxGraphics, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.Layout.Painters,
  dxRichEdit.InnerControl,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Inplace,
  dxRichEdit.Inplace,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.CommentPadding;

type
  { TdxRenderView }

  TdxRenderView = class(TdxInplaceView)
  protected
    procedure ActivateScrollBars; override;
    procedure DeactivateScrollBars; override;
    procedure EnsureVerticalScrollBarSynchronized; override;
    function GetPhysicalLeftInvisibleWidth: Integer; override;
    function HasSelection: Boolean; override;
    procedure SetOptimalHorizontalScrollbarPosition; override;
  public
    procedure EnsureCaretVisible(ACheckCaretVisibility: Boolean = True); override;
    procedure UpdateHorizontalScrollBar; override;
    procedure UpdateVerticalScrollBar; override;
  end;

  { TdxRichEditRenderViewRepository }

  TdxRichEditRenderViewRepository = class(TdxInplaceRichEditViewRepository)
  protected
    procedure CreateViews; override;
  end;

  { TdxInplaceFormatter }

  TdxInplaceFormatter = class(TdxBackgroundFormatter)
  protected
    procedure InitializeWorkThread; override;
    procedure DoneWorkThread; override;
    procedure SetCommandEvent(ACommand: TdxBackgroundFormatter.TCommand); override;
    procedure ResetCommandEvent(ACommand: TdxBackgroundFormatter.TCommand); override;

    procedure CheckExecutedAtWorkerThread; override;
    procedure CheckExecutedAtUIThread; override;
    procedure DoBeginDocumentRendering; override;
  public
    procedure Start; override;
    function SuspendWorkThread: Boolean; override;
    procedure ResumeWorkThread; override;
  end;

  { TdxRichEditRenderInnerControl }

  TdxRichEditRenderInnerControl = class(TdxInplaceRichEditInnerControl)
  protected
    function CreateBackgroundFormatter(AController: TdxDocumentFormattingController): TdxBackgroundFormatter; override;
    function CreateKeyboardController: TdxCustomKeyboardController; override;
    function CreateNativeDocument: IdxRichEditDocument; override;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument; override;
    function CreateMouseController: TdxRichEditCustomMouseController; override;
    function CreateSpellCheckerController: TdxSpellCheckerCustomController; override;
    function CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager; override;
    procedure PopulateCommands; override;
  public
    procedure UpdateVerticalScrollBar(AAvoidJump: Boolean); override;
  end;

  { TdxRichEditRenderDocumentServer }

  TdxRichEditRenderDocumentServer = class(TdxInnerRichEditDocumentServer)
  protected
    function CreateNativeDocument: IdxRichEditDocument; override;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument; override;
  public
    procedure BeginInitialize; override;
  end;

  { TdxRichEditRenderInternalDocumentServer }

  TdxRichEditRenderInternalDocumentServer = class(TdxInternalRichEditDocumentServer)
  protected
    function CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer; override;
  end;

  TdxRichEditRenderOptions = class(TdxRichEditControlCustomOptions);

  { TdxRichEditRender }

  TdxRichEditRender = class(TcxIUnknownObject,
    IdxBatchUpdateable,
    IdxInnerRichEditDocumentServerOwner,
    IdxInnerRichEditControlOwner,
    IdxRichEditControl,
    IdxRichEditDocumentLayoutProvider)
  public type
    TSetEditValueFunc = reference to procedure(ADocumentModel: TdxDocumentModel);
    TInitializeRenderFunc = reference to procedure(ARender: TdxRichEditRender);
  strict private
    FIsEditValueNull: Boolean;
    FBackgroundPainter: TdxRichEditViewBackgroundPainter;
    FBounds: TRect;
    FScaleFactor: TdxScaleFactor;
    FInnerControl: TdxRichEditRenderInnerControl;
    FIsDestroying: Boolean;
    FViewBounds: TRect;

    function CalculateViewBounds(const AClientBounds: TRect): TRect;
    procedure RecreatePainters(AView: TdxRichEditView);

    function GetActiveView: TdxRichEditView;
    function GetEditValue: Variant;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetBounds(const Value: TRect);
    procedure SetEditValue(const Value: Variant);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);

  {$REGION 'IdxBatchUpdateable'}
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
  {$ENDREGION}
  {$REGION 'IdxInnerRichEditDocumentServerOwner'}
    // IdxInnerRichEditDocumentContainerOwner
    function CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
    // IdxInnerRichEditDocumentServerOwner
    function GetControl: TWinControl;
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer{IdxRichEditDocumentServer};
    function CreateMeasurementAndDrawingStrategy(ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
    procedure RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions);
  {$ENDREGION}
  {$REGION 'IdxInnerRichEditControlOwner'}
    // IdxInnerRichEditControlOwner
    function GetEnabled: Boolean;
    function GetRichEditControl: IdxRichEditControl;
    function GetCommentPadding: TdxCommentPadding;
    function CreateVerticalScrollBar: IdxOfficeScrollbar;
    function CreateHorizontalScrollBar: IdxOfficeScrollbar;
    function CreateVerticalRuler: IdxRulerControl;
    function CreateHorizontalRuler: IdxRulerControl;
		procedure ActivateViewPlatformSpecific(AView: TdxRichEditView);
    procedure RedrawEnsureSecondaryFormattingComplete; overload;
    procedure RedrawEnsureSecondaryFormattingComplete(AAction: TdxRefreshAction); overload;
    procedure OnResizeCore;
    function CalculateActualViewBounds(const APreviousViewBounds: TRect): TRect;
    function CreateViewRepository: TdxRichEditCustomViewRepository;
    procedure OnOptionsChangedPlatformSpecific(E: TdxOptionChangedEventArgs);
    function IsHandleCreated: Boolean;
    procedure UpdateRulers;
    procedure UpdateHorizontalRuler;
    procedure UpdateVerticalRuler;
    procedure OnResize;
    procedure Redraw; overload;
    procedure Redraw(AAction: TdxRefreshAction); overload;
    procedure RedrawAfterEndUpdate;
    procedure ApplyChangesCorePlatformSpecific(AChangeActions: TdxDocumentModelChangeActions);
    procedure OnZoomFactorChangingPlatformSpecific;
    procedure OnActiveViewBackColorChanged;
    procedure ResizeView(AEnsureCaretVisibleOnResize: Boolean);
    procedure ShowCaret;
    procedure HideCaret;
    procedure ShowBookmarkForm;
    procedure ShowSearchForm;
    procedure ShowReplaceForm;
    procedure ShowNumberingListForm(AParagraphs: TdxParagraphList; const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject);
    procedure ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload;
    procedure ShowEditStyleForm(ACharacterSourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload;
    procedure ShowTableStyleForm(AStyle: TdxTableStyle);
    procedure ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties; const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject);
    procedure ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties; const ACallback: TdxShowFontFormCallback; ACallbackData: TObject);
    procedure ShowTabsForm(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; const ACallback: TdxShowTabsFormCallback; ACallbackData: TObject);
    procedure ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo; const ATitle: string; const ACallback: TdxShowHyperlinkFormCallback);
    procedure ShowSymbolForm(const ASymbolProperties: TdxSymbolProperties; const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
    procedure ShowInsertMergeFieldForm;
    procedure ShowInsertTableForm(const AParameters: TdxCreateTableParameters; const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject );
    procedure ShowInsertTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
    procedure ShowDeleteTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
    procedure ShowSplitTableCellsForm(const AParameters: TdxSplitTableCellsParameters; const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject);
    procedure ShowRangeEditingPermissionsForm;
    procedure ShowDocumentProtectionQueryNewPasswordForm(const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowDocumentProtectionQueryPasswordForm(const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowLineNumberingForm(AProperties: TdxLineNumberingInfo; const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
    procedure ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback; ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage);
    procedure ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI; const ACallback: TdxShowColumnsSetupFormCallback; ACallbackData: TObject);
    procedure ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection);
    procedure ShowFloatingInlineObjectLayoutOptionsForm(const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
      const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject);
    procedure ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject = nil);
    procedure ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const ACallback: TdxShowMergeDatabaseRecordsFormCallback);
  {$ENDREGION}
  {$REGION 'IdxRichEditControl'}
    function GetCanShowNumberingListForm: Boolean;
    function GetInnerControl: IdxInnerControl;
    function GetCursor: TCursor;
    function GetDocument: IdxRichEditDocument;
    function GetDocumentLayout: TdxRichEditDocumentLayout;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetLayoutUnit: TdxDocumentLayoutUnit;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetOvertype: Boolean;
    function GetPixelPhysicalBounds(APageViewInfo: TdxPageViewInfo; ALogicalBounds: TRect): TRect;
    function GetScaleFactor: TdxScaleFactor;
    function GetIsDestroying: Boolean;
    function GetViewBounds: TRect;
    procedure SetCursor(Value: TCursor);
    procedure SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
    procedure SetOvertype(const Value: Boolean);
    function UseStandardDragDropMode: Boolean;
    function CreateRichEditViewVerticalScrollController(ARichEditView: TdxRichEditView): TdxRichEditViewVerticalScrollController;
    function CreateRichEditViewHorizontalScrollController(ARichEditView: TdxRichEditView): TdxRichEditViewHorizontalScrollController;
    function CreatePlatformSpecificScrollBarAdapter: IdxPlatformSpecificScrollBarAdapter;
    function ShowWarningMessage(const AMessage: string): TModalResult;
    function ShowErrorMessage(const AMessage: string): TModalResult;
    function IsHyperlinkActive: Boolean;
    procedure UpdateUIFromBackgroundThread(const AMethod: TdxAction);
    procedure CreateDragCaret;
    procedure DestroyDragCaret;
    function DoDragDrop(const AData: IDataObject; AllowedEffects: TdxDragDropEffects): TdxDragDropEffects;
    procedure DrawDragCaret;
    function GetDragCaret: TdxDragCaret;
    procedure OnViewPaddingChanged;
    procedure AddKeyboardService(const AService: IdxKeyboardHandlerService);
    function GetKeyboardHandler: IdxKeyboardHandlerService;
    procedure RemoveKeyboardService(const AService: IdxKeyboardHandlerService);
    procedure ShowDocumentEncryptQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowTOCForm(AField: TdxField);
    procedure UpdateControlAutoSize;
    procedure EnsureCaretVisible(ACheckCaretVisibility: Boolean = True);
    function GetSkinLeftMargin: Integer;
    function GetSkinRightMargin: Integer;
    function GetSkinTopMargin: Integer;
    function GetSkinBottomMargin: Integer;
    function GetUseSkinMargins: Boolean;
    function GetReadOnly: Boolean;
    function GetBackgroundPainter: TdxRichEditViewBackgroundPainter;
    function GetDocumentModel: TdxDocumentModel;
    function GetMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
    procedure SetReadOnly(const Value: Boolean);
  {$ENDREGION}
  {$REGION 'IdxRichEditDocumentLayoutProvider'}
    function GetLayoutCalculationMode: TdxCalculationModeType;
    function IdxRichEditDocumentLayoutProvider.GetDocumentLayout = DocumentLayoutProviderGetDocumentLayout;
    function DocumentLayoutProviderGetDocumentLayout: TdxDocumentLayout;
    function GetDocumentLayoutAsync: TdxDocumentLayout;
    procedure AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddDocumentFormatted(const AHandler: TdxEvent);
    procedure RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemoveDocumentFormatted(const AHandler: TdxEvent);
    procedure PerformPageSecondaryFormatting(APage: TdxPage);
  {$ENDREGION}
  protected
    procedure DoDraw(AGraphics: TdxGraphics);
    procedure InternalDraw(ACanvas: TcxCanvas; const ABounds: TRect; const ADCOrigin: TPoint);
    procedure Initialize;
    procedure DoInitialize; virtual;

    class function CalculateSize(const AWidth: Integer; const AInitializeRenderFunc: TInitializeRenderFunc): TSize; overload; static;
    class procedure Draw(ACanvas: TcxCanvas; const ABounds: TRect; const AInitializeRenderFunc: TInitializeRenderFunc); overload; static;

    property ActiveView: TdxRichEditView read GetActiveView;
    property InnerControl: TdxRichEditRenderInnerControl read FInnerControl;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    class function CalculateSize(const AEditValue: Variant;
      AProperties: TdxCustomInplaceRichEditProperties;
      ADefaultFont: TFont; const AWidth: Integer = -1): TSize; overload; static;
    class function CalculateSize(const ASetEditValueFunc: TSetEditValueFunc;
      const AWidth: Integer = -1): TSize; overload; static;

    class procedure Draw(ACanvas: TcxCanvas; const AEditValue: Variant;
      AProperties: TdxCustomInplaceRichEditProperties;
      ADefaultFont: TFont; AForeColor: TdxAlphaColor; const ABounds: TRect); overload; static;
    class procedure Draw(ACanvas: TcxCanvas; const ASetEditValueFunc: TSetEditValueFunc;
      const ABounds: TRect); overload; static;

    procedure Draw(ACanvas: TcxCanvas; const P: TPoint); overload;

    procedure AssignProperties(AProperties: TdxCustomInplaceRichEditProperties);
    procedure SetDefaultFont(AFont: TFont; AForeColor: TdxAlphaColor);

    property Bounds: TRect read FBounds write SetBounds;
    property Document: IdxRichEditDocument read GetDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property EditValue: Variant read GetEditValue write SetEditValue;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

implementation

uses
  dxTypeHelpers, dxThreading,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.InnerControl.DrawingStrategy,
  dxRichEdit.Platform.Win.Scroll,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Api.NativeDocument;

type
  { TdxRenderMeasurementAndDrawingStrategy }

  TdxRenderMeasurementAndDrawingStrategy = class(TdxGdiPlusMeasurementAndDrawingStrategyBase)
  public
    function CreateDocumentPainter(AGraphics: TdxGraphics): TObject; override;
    function CreateBoxMeasurer: TdxBoxMeasurer; override;
  end;

{ TdxRenderMeasurementAndDrawingStrategy }

function TdxRenderMeasurementAndDrawingStrategy.CreateDocumentPainter(AGraphics: TdxGraphics): TObject;
begin
  Result := TdxRichEditGdiPainter.Create(AGraphics, TdxGdiBoxMeasurer(Measurer));
end;

function TdxRenderMeasurementAndDrawingStrategy.CreateBoxMeasurer: TdxBoxMeasurer;
begin
  Result := TdxGdiBoxMeasurer.Create(DocumentModel, MeasureGraphics);
end;

{ TdxRenderView }

procedure TdxRenderView.ActivateScrollBars;
begin
end;

procedure TdxRenderView.DeactivateScrollBars;
begin
end;

procedure TdxRenderView.EnsureCaretVisible(ACheckCaretVisibility: Boolean = True);
begin
end;

procedure TdxRenderView.EnsureVerticalScrollBarSynchronized;
begin
end;

function TdxRenderView.GetPhysicalLeftInvisibleWidth: Integer;
begin
  Result := 0;
end;

function TdxRenderView.HasSelection: Boolean;
begin
  Result := False;
end;

procedure TdxRenderView.SetOptimalHorizontalScrollbarPosition;
begin
end;

procedure TdxRenderView.UpdateHorizontalScrollBar;
begin
end;

procedure TdxRenderView.UpdateVerticalScrollBar;
begin
end;

{ TdxRichEditRenderViewRepository }

procedure TdxRichEditRenderViewRepository.CreateViews;
begin
  AddView(TdxRenderView.Create(RichEditControl));
end;

{ TdxInplaceFormatter }

procedure TdxInplaceFormatter.Start;
begin
end;

function TdxInplaceFormatter.SuspendWorkThread: Boolean;
begin
  Result := True;
end;

procedure TdxInplaceFormatter.ResumeWorkThread;
begin
end;

procedure TdxInplaceFormatter.InitializeWorkThread;
begin
end;

procedure TdxInplaceFormatter.DoneWorkThread;
begin
end;

procedure TdxInplaceFormatter.SetCommandEvent(ACommand: TdxBackgroundFormatter.TCommand);
begin
end;

procedure TdxInplaceFormatter.ResetCommandEvent(ACommand: TdxBackgroundFormatter.TCommand);
begin
end;

procedure TdxInplaceFormatter.CheckExecutedAtWorkerThread;
begin
end;

procedure TdxInplaceFormatter.CheckExecutedAtUIThread;
begin
end;

procedure TdxInplaceFormatter.DoBeginDocumentRendering;
begin
  if not PrimaryLayoutComplete then
  begin
    repeat
      PerformPrimaryLayout;
    until PrimaryLayoutComplete;
  end;
end;

{ TdxRichEditRenderInnerControl }

function TdxRichEditRenderInnerControl.CreateBackgroundFormatter(AController: TdxDocumentFormattingController): TdxBackgroundFormatter;
begin
  Result := TdxInplaceFormatter.Create(AController, Owner.CommentPadding);
end;

function TdxRichEditRenderInnerControl.CreateKeyboardController: TdxCustomKeyboardController;
begin
  Result := nil;
end;

function TdxRichEditRenderInnerControl.CreateMouseController: TdxRichEditCustomMouseController;
begin
  Result := nil;
end;

function TdxRichEditRenderInnerControl.CreateSpellCheckerController: TdxSpellCheckerCustomController;
begin
  Result := TdxEmptySpellCheckerController.Create;
end;

function TdxRichEditRenderInnerControl.CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager;
begin
  Result := TdxEmptySpellCheckerManager.Create(APieceTable);
end;

function TdxRichEditRenderInnerControl.CreateNativeDocument: IdxRichEditDocument;
begin
  Result := TdxNativeDocument.Create(DocumentModel.MainPieceTable, Self);
end;

function TdxRichEditRenderInnerControl.CreateNativeSubDocument(
  APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, Self);
end;

procedure TdxRichEditRenderInnerControl.PopulateCommands;
begin
end;

procedure TdxRichEditRenderInnerControl.UpdateVerticalScrollBar(AAvoidJump: Boolean);
begin
end;

{ TdxRichEditRenderDocumentServer }

procedure TdxRichEditRenderDocumentServer.BeginInitialize;
begin
  inherited BeginInitialize;
  DocumentModel.LayoutUnit := TdxDocumentLayoutUnit.Pixel;
end;

function TdxRichEditRenderDocumentServer.CreateNativeDocument: IdxRichEditDocument;
begin
  Result := TdxNativeDocument.Create(DocumentModel.MainPieceTable, Self);
end;

function TdxRichEditRenderDocumentServer.CreateNativeSubDocument(
  APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, Self);
end;

{ TdxRichEditRenderInternalDocumentServer }

function TdxRichEditRenderInternalDocumentServer.CreateInnerServer(
  ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer;
begin
  if ADocumentModel = nil then
    Result := TdxRichEditRenderDocumentServer.Create(Self)
  else
    Result := TdxRichEditRenderDocumentServer.Create(Self, ADocumentModel);
end;

{ TdxRichEditRender }

constructor TdxRichEditRender.Create;
begin
  inherited Create;
  FScaleFactor := TdxScaleFactor.Create;
  FBounds := cxNullRect;
  FInnerControl := TdxRichEditRenderInnerControl.Create(Self);
  Initialize;
end;

destructor TdxRichEditRender.Destroy;
begin
  FInnerControl.StopFormatting;
  FreeAndNil(FBackgroundPainter);
  FInnerControl.Free;
  FInnerControl := nil;
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TdxRichEditRender.BeforeDestruction;
begin
  FIsDestroying := True;
  inherited BeforeDestruction;
end;

class function TdxRichEditRender.CalculateSize(const AWidth: Integer; const AInitializeRenderFunc: TInitializeRenderFunc): TSize;
var
  ACalculateBestFit: Boolean;
  ARender: TdxRichEditRender;
  APadding: TRect;
begin
  ACalculateBestFit := AWidth <= 0;
  ARender := TdxRichEditRender.Create;
  try
    if not ACalculateBestFit then
      ARender.Width := AWidth;

    AInitializeRenderFunc(ARender);

    Result := ARender.ActiveView.CalcBestSize(not ACalculateBestFit);
    APadding := ARender.ActiveView.Padding.Value;
    Inc(Result.cx, APadding.Left + APadding.Right);
    Inc(Result.cy, APadding.Top + APadding.Bottom);
  finally
    ARender.Free;
  end;
end;

procedure TdxRichEditRender.Draw(ACanvas: TcxCanvas; const P: TPoint);
var
  AWindowOrg: TPoint;
  ABounds: TRect;
begin
  ACanvas.Lock;
  try
    ACanvas.SaveClipRegion;
    try
      AWindowOrg := ACanvas.DCOrigin;
      ABounds := Bounds;
      ABounds.Location := P;
      InternalDraw(ACanvas, ABounds, AWindowOrg);
    finally
      ACanvas.RestoreClipRegion;
    end;
  finally
    ACanvas.Unlock;
  end;
end;

class procedure TdxRichEditRender.Draw(ACanvas: TcxCanvas;
  const ABounds: TRect; const AInitializeRenderFunc: TInitializeRenderFunc);
var
  ARender: TdxRichEditRender;
begin
  ARender := TdxRichEditRender.Create;
  try
    ARender.Bounds := ABounds;
    AInitializeRenderFunc(ARender);
    ARender.Draw(ACanvas, ABounds.TopLeft);
  finally
    ARender.Free;
  end;
end;

class function TdxRichEditRender.CalculateSize(const AEditValue: Variant;
  AProperties: TdxCustomInplaceRichEditProperties;
  ADefaultFont: TFont; const AWidth: Integer): TSize;
var
  AValue: Variant;
begin
  AValue := AEditValue;
  Result := TdxRichEditRender.CalculateSize(AWidth,
    procedure(ARender: TdxRichEditRender)
    begin
      ARender.AssignProperties(AProperties);
      ARender.EditValue := AValue;
      ARender.SetDefaultFont(ADefaultFont, TdxAlphaColors.Black);
    end);
end;

class function TdxRichEditRender.CalculateSize(const ASetEditValueFunc: TSetEditValueFunc;
  const AWidth: Integer = -1): TSize;
begin
  Result := TdxRichEditRender.CalculateSize(AWidth,
    procedure(ARender: TdxRichEditRender)
    begin
      ASetEditValueFunc(ARender.DocumentModel);
    end);
end;

class procedure TdxRichEditRender.Draw(ACanvas: TcxCanvas;
  const AEditValue: Variant; AProperties: TdxCustomInplaceRichEditProperties;
  ADefaultFont: TFont; AForeColor: TdxAlphaColor; const ABounds: TRect);
var
  AValue: Variant;
begin
  AValue := AEditValue;
  TdxRichEditRender.Draw(ACanvas, ABounds,
    procedure (ARender: TdxRichEditRender)
    begin
      ARender.AssignProperties(AProperties);
      ARender.EditValue := AValue;
      ARender.SetDefaultFont(ADefaultFont, AForeColor);
    end);
end;

class procedure TdxRichEditRender.Draw(ACanvas: TcxCanvas; const ASetEditValueFunc: TSetEditValueFunc;
  const ABounds: TRect);
begin
  TdxRichEditRender.Draw(ACanvas, ABounds,
    procedure (ARender: TdxRichEditRender)
    begin
      ASetEditValueFunc(ARender.DocumentModel);
    end);
end;

procedure TdxRichEditRender.AssignProperties(AProperties: TdxCustomInplaceRichEditProperties);
var
  AOptions: TdxRichEditControlCustomOptions;
begin
  AOptions := InnerControl.Options as TdxRichEditControlCustomOptions;
  TcxInplaceRichEditDocumentHelper.SynchronizePropertiesAndOptions(AProperties, AOptions)
end;

procedure TdxRichEditRender.SetDefaultFont(AFont: TFont; AForeColor: TdxAlphaColor);
begin
  TcxInplaceRichEditDocumentHelper.SetDefaultFont(InnerControl.Document, AFont, AForeColor);
end;

procedure TdxRichEditRender.ActivateViewPlatformSpecific(
  AView: TdxRichEditView);
begin
  RecreatePainters(AView);
end;

procedure TdxRichEditRender.AddKeyboardService(
  const AService: IdxKeyboardHandlerService);
begin
end;

procedure TdxRichEditRender.ApplyChangesCorePlatformSpecific(
  AChangeActions: TdxDocumentModelChangeActions);
begin
end;

procedure TdxRichEditRender.BeginUpdate;
begin
  if InnerControl <> nil then
    InnerControl.BeginUpdate;
end;

function TdxRichEditRender.CalculateActualViewBounds(
  const APreviousViewBounds: TRect): TRect;
var
  AViewPadding: TRect;
begin
  Result := FBounds;
  AViewPadding := ActiveView.ActualPadding;
  Result.Inflate(-AViewPadding.Left, -AViewPadding.Top, -AViewPadding.Right, -AViewPadding.Bottom);
end;

procedure TdxRichEditRender.CancelUpdate;
begin
  if InnerControl <> nil then
    InnerControl.CancelUpdate;
end;

function TdxRichEditRender.CreateDocumentContainer(
  ADocumentModel: TObject): IdxRichEditDocumentContainer;
begin
  Result := CreateDocumentServer(TdxDocumentModel(ADocumentModel));
end;

function TdxRichEditRender.CreateDocumentServer(
  ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer;
begin
  Result := TdxRichEditRenderInternalDocumentServer.Create(ADocumentModel);
end;

procedure TdxRichEditRender.CreateDragCaret;
begin
end;

function TdxRichEditRender.CreateHorizontalRuler: IdxRulerControl;
begin
end;

function TdxRichEditRender.CreateHorizontalScrollBar: IdxOfficeScrollbar;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateMeasurementAndDrawingStrategy(
  ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
begin
  Result := TdxRenderMeasurementAndDrawingStrategy.Create(ADocumentModel);
end;

function TdxRichEditRender.CreateOptions(
  const ADocumentServer: TObject): TObject;
begin
  Result := TdxRichEditRenderOptions.Create(TdxInnerRichEditDocumentServer(ADocumentServer));
end;

function TdxRichEditRender.CreatePlatformSpecificScrollBarAdapter: IdxPlatformSpecificScrollBarAdapter;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateRichEditViewHorizontalScrollController(
  ARichEditView: TdxRichEditView): TdxRichEditViewHorizontalScrollController;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateRichEditViewVerticalScrollController(
  ARichEditView: TdxRichEditView): TdxRichEditViewVerticalScrollController;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateVerticalRuler: IdxRulerControl;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateVerticalScrollBar: IdxOfficeScrollbar;
begin
  Result := nil;
end;

function TdxRichEditRender.CreateViewRepository: TdxRichEditCustomViewRepository;
begin
  Result := TdxRichEditRenderViewRepository.Create(Self);
end;

procedure TdxRichEditRender.DestroyDragCaret;
begin
end;

function TdxRichEditRender.DoDragDrop(const AData: IDataObject;
  AllowedEffects: TdxDragDropEffects): TdxDragDropEffects;
begin
  Result := [TdxDragDropEffect.None];
end;

procedure TdxRichEditRender.DrawDragCaret;
begin
end;

procedure TdxRichEditRender.EndUpdate;
begin
  if InnerControl <> nil then
    InnerControl.EndUpdate;
end;

procedure TdxRichEditRender.EnsureCaretVisible(ACheckCaretVisibility: Boolean);
begin
end;

function TdxRichEditRender.GetBackgroundPainter: TdxRichEditViewBackgroundPainter;
begin
  Result := FBackgroundPainter;
end;

function TdxRichEditRender.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  if InnerControl <> nil then
    Result := InnerControl.BatchUpdateHelper
  else
    Result := nil;
end;

function TdxRichEditRender.GetCanShowNumberingListForm: Boolean;
begin
  Result := False;
end;

function TdxRichEditRender.GetCommentPadding: TdxCommentPadding;
begin
  Result := TdxCommentPadding.GetDefaultCommentPadding(InnerControl.DocumentModel);
end;

function TdxRichEditRender.GetControl: TWinControl;
begin
  Result := nil;
end;

function TdxRichEditRender.GetCursor: TCursor;
begin
  Result := crNone;
end;

function TdxRichEditRender.GetDocument: IdxRichEditDocument;
begin
  if InnerControl <> nil then
    Result := InnerControl.Document
  else
    Result := nil;
end;

function TdxRichEditRender.GetDocumentLayout: TdxRichEditDocumentLayout;
begin
   if InnerControl <> nil then
     Result := InnerControl.DocumentLayout
   else
     Result := nil;
end;

function TdxRichEditRender.GetDocumentModel: TdxDocumentModel;
begin
  Result := InnerControl.DocumentModel;
end;

function TdxRichEditRender.GetDpiX: Single;
begin
  Result := DocumentModel.DpiX;
end;

function TdxRichEditRender.GetDpiY: Single;
begin
  Result := DocumentModel.DpiY;
end;

function TdxRichEditRender.GetDragCaret: TdxDragCaret;
begin
  Result := nil;
end;

function TdxRichEditRender.GetEnabled: Boolean;
begin
  Result := True;
end;

function TdxRichEditRender.GetInnerControl: IdxInnerControl;
begin
  Result := FInnerControl;
end;

function TdxRichEditRender.GetIsDestroying: Boolean;
begin
  Result := FIsDestroying;
end;

function TdxRichEditRender.GetIsUpdateLocked: Boolean;
begin
  Result := (InnerControl <> nil) and InnerControl.IsUpdateLocked;
end;

function TdxRichEditRender.GetKeyboardHandler: IdxKeyboardHandlerService;
begin
  Result := nil;
end;

function TdxRichEditRender.GetLayoutUnit: TdxDocumentLayoutUnit;
begin
  Result := InnerControl.LayoutUnit;
end;

function TdxRichEditRender.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := nil;
end;

function TdxRichEditRender.GetMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
begin
  if InnerControl <> nil then
    Result := InnerControl.MeasurementAndDrawingStrategy
  else
    Result := nil;
end;

function TdxRichEditRender.GetOvertype: Boolean;
begin
  Result := False;
end;

function TdxRichEditRender.GetPixelPhysicalBounds(
  APageViewInfo: TdxPageViewInfo; ALogicalBounds: TRect): TRect;
begin
  Result := cxNullRect;
end;

function TdxRichEditRender.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TdxRichEditRender.GetReadOnly: Boolean;
begin
  Result := False;
end;

function TdxRichEditRender.GetRichEditControl: IdxRichEditControl;
begin
  Result := Self;
end;

function TdxRichEditRender.GetSkinBottomMargin: Integer;
begin
  Result := 0;
end;

function TdxRichEditRender.GetSkinLeftMargin: Integer;
begin
  Result := 0;
end;

function TdxRichEditRender.GetSkinRightMargin: Integer;
begin
  Result := 0;
end;

function TdxRichEditRender.GetSkinTopMargin: Integer;
begin
  Result := 0;
end;

function TdxRichEditRender.GetUseSkinMargins: Boolean;
begin
  Result := False;
end;

function TdxRichEditRender.GetViewBounds: TRect;
begin
  Result := FViewBounds;
end;

procedure TdxRichEditRender.HideCaret;
begin
end;

procedure TdxRichEditRender.DoDraw(AGraphics: TdxGraphics);
var
  AViewPainter: TdxRichEditViewPainter;
  ACustomMarkExporter: TdxCustomMarkExporter;
begin
  ACustomMarkExporter := TdxCustomMarkExporter.Create;
  try
    InnerControl.BeginDocumentRendering;
    try
      AViewPainter := ActiveView.CreatePainter;
      try
        AViewPainter.Draw(AGraphics, ACustomMarkExporter);
      finally
        AViewPainter.Free;
      end;
    finally
      InnerControl.EndDocumentRendering;
    end;
  finally
    ACustomMarkExporter.Free;
  end;
end;

procedure TdxRichEditRender.InternalDraw(ACanvas: TcxCanvas; const ABounds: TRect;
  const ADCOrigin: TPoint);

  function GetHdcOriginZoomFactor: Single;
  var
    ADC: HDC;
    ATransform: XFORM;
  begin
    ADC := ACanvas.Handle;
    GetWorldTransform(ADC, ATransform);
    Result := ATransform.eM11;
  end;

var
  AModifier: TdxGraphicsToLayoutUnitsModifier;
  ALayoutUnitConverter: TdxDocumentLayoutUnitConverter;
  AHdcOriginModifier: TdxHdcOriginModifier;
  APaintOrg, P: TPoint;
  AHdcOriginZoomFactor: Single;
  AGraphics: TdxGraphics;
  AClipRegion: TcxRegion;
  R: TdxRectF;
begin
  AGraphics := TdxGraphics.CreateFromHdc(ACanvas.Handle);
  try
    AHdcOriginZoomFactor := 1 / GetHdcOriginZoomFactor;
    AGraphics.SetClipRect(ABounds, gmReplace);
    AClipRegion := ACanvas.GetClipRegion;
    try
      R := cxRectF(AClipRegion.BoundsRect);
      R := cxRectScale(R, AHdcOriginZoomFactor);
      AGraphics.SetClip(R);
    finally
      AClipRegion.Free;
    end;
    ALayoutUnitConverter := DocumentModel.LayoutUnitConverter;
    AModifier := TdxGraphicsToLayoutUnitsModifier.Create(AGraphics, ALayoutUnitConverter);
    try
      APaintOrg := FViewBounds.TopLeft;
      P := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(cxPointOffset(ABounds.TopLeft, Bounds.TopLeft, False), DocumentModel.DpiX, DocumentModel.DpiY);
      APaintOrg.Offset(P);
      APaintOrg.Offset(ADCOrigin);
      P := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(ADCOrigin, DocumentModel.DpiX, DocumentModel.DpiY);
      APaintOrg.Offset(cxPointInvert(P));
      AGraphics.TranslateWorldTransform(APaintOrg.X, APaintOrg.Y);
      APaintOrg.Offset(P);
      AHdcOriginModifier := TdxHdcOriginModifier.Create(AGraphics, APaintOrg, AHdcOriginZoomFactor);
      try
        DoDraw(AGraphics);
      finally
        AHdcOriginModifier.Free;
      end;
    finally
      AModifier.Free;
    end;
  finally
    AGraphics.Free;
  end;
end;

procedure TdxRichEditRender.Initialize;
begin
  InnerControl.BeginInitialize;
  try
    DoInitialize;
  finally
    InnerControl.EndInitialize;
  end;
end;

procedure TdxRichEditRender.DoInitialize;
begin
end;

function TdxRichEditRender.IsHandleCreated: Boolean;
begin
  Result := False;
end;

function TdxRichEditRender.IsHyperlinkActive: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditRender.OnActiveViewBackColorChanged;
begin
end;

procedure TdxRichEditRender.OnOptionsChangedPlatformSpecific(
  E: TdxOptionChangedEventArgs);
begin
end;

procedure TdxRichEditRender.OnResize;
begin
  OnResizeCore;
end;

procedure TdxRichEditRender.OnResizeCore;
begin
  ResizeView(False);
end;

procedure TdxRichEditRender.OnViewPaddingChanged;
begin
  BeginUpdate;
  try
    OnResizeCore;
    RedrawEnsureSecondaryFormattingComplete;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditRender.OnZoomFactorChangingPlatformSpecific;
begin
end;

procedure TdxRichEditRender.RaiseDeferredEvents(
  AChangeActions: TdxDocumentModelChangeActions);
var
  AInnerControl: TdxInnerRichEditControl;
begin
  AInnerControl := InnerControl;
  if (AInnerControl <> nil) and not FIsDestroying then
    TdxUIThreadSyncService.EnqueueInvokeInUIThread(
      AInnerControl,
      procedure
      begin
        AInnerControl.RaiseDeferredEventsCore(AChangeActions);
      end);
end;

procedure TdxRichEditRender.Redraw;
begin
end;

procedure TdxRichEditRender.Redraw(AAction: TdxRefreshAction);
begin
end;

procedure TdxRichEditRender.RedrawAfterEndUpdate;
begin
end;

procedure TdxRichEditRender.RedrawEnsureSecondaryFormattingComplete;
begin
end;

procedure TdxRichEditRender.RedrawEnsureSecondaryFormattingComplete(
  AAction: TdxRefreshAction);
begin
end;

procedure TdxRichEditRender.RemoveKeyboardService(
  const AService: IdxKeyboardHandlerService);
begin
end;

procedure TdxRichEditRender.ResizeView(AEnsureCaretVisibleOnResize: Boolean);
var
  ANormalizedViewBounds: TRect;
begin
  FViewBounds := CalculateViewBounds(FBounds);
  ANormalizedViewBounds := FViewBounds;
  ANormalizedViewBounds.Offset(-FViewBounds.Left, -FViewBounds.Top);
  ActiveView.OnResize(ANormalizedViewBounds, False);
end;

procedure TdxRichEditRender.SetCursor(Value: TCursor);
begin
end;

procedure TdxRichEditRender.SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
begin
  InnerControl.LayoutUnit := Value;
end;

procedure TdxRichEditRender.SetOvertype(const Value: Boolean);
begin
end;

procedure TdxRichEditRender.SetReadOnly(const Value: Boolean);
begin
end;

procedure TdxRichEditRender.ShowBookmarkForm;
begin
end;

procedure TdxRichEditRender.ShowCaret;
begin
end;

procedure TdxRichEditRender.ShowColumnsSetupForm(
  const AProperties: TdxColumnsInfoUI;
  const ACallback: TdxShowColumnsSetupFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowDeleteTableCellsForm(
  const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback;
  ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowDocumentEncryptQueryNewPasswordForm(
  const APassword: string; const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditRender.ShowDocumentProtectionQueryNewPasswordForm(
  const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditRender.ShowDocumentProtectionQueryPasswordForm(
  const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
begin
end;

procedure TdxRichEditRender.ShowEditStyleForm(
  ACharacterSourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex;
  const ACallback: TdxShowEditStyleFormCallback);
begin
end;

procedure TdxRichEditRender.ShowEditStyleForm(
  AParagraphSourceStyle: TdxParagraphStyle; AIndex: TdxParagraphIndex;
  const ACallback: TdxShowEditStyleFormCallback);
begin
end;

function TdxRichEditRender.ShowErrorMessage(
  const AMessage: string): TModalResult;
begin
  Result := mrOk;
end;

procedure TdxRichEditRender.ShowFloatingInlineObjectLayoutOptionsForm(
  const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
  const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback;
  ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowFontForm(
  ACharacterProperties: TdxMergedCharacterProperties;
  const ACallback: TdxShowFontFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo;
  ARunInfo: TdxRunInfo; const ATitle: string;
  const ACallback: TdxShowHyperlinkFormCallback);
begin
end;

procedure TdxRichEditRender.ShowInsertMergeFieldForm;
begin
end;

procedure TdxRichEditRender.ShowInsertTableCellsForm(
  const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback;
  ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowInsertTableForm(
  const AParameters: TdxCreateTableParameters;
  const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowLineNumberingForm(
  AProperties: TdxLineNumberingInfo;
  const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowMergeDatabaseRecordsForm(
  const AMergeRecordsParameters: TdxMergeRecordsParameters;
  const ACallback: TdxShowMergeDatabaseRecordsFormCallback);
begin
end;

procedure TdxRichEditRender.ShowNumberingListForm(AParagraphs: TdxParagraphList;
  const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowPageSetupForm(AProperties: TdxPageSetupInfo;
  const ACallback: TdxShowPageSetupFormCallback; ACallbackData: TObject;
  AInitialTabPage: TdxPageSetupFormInitialTabPage);
begin
end;

procedure TdxRichEditRender.ShowParagraphForm(
  AParagraphProperties: TdxMergedParagraphProperties;
  const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowRangeEditingPermissionsForm;
begin
end;

procedure TdxRichEditRender.ShowReplaceForm;
begin
end;

procedure TdxRichEditRender.ShowSearchForm;
begin
end;

procedure TdxRichEditRender.ShowSplitTableCellsForm(
  const AParameters: TdxSplitTableCellsParameters;
  const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowSymbolForm(
  const ASymbolProperties: TdxSymbolProperties;
  const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowTableOptionsForm(ATable: TdxTable;
  AOwner: TObject);
begin
end;

procedure TdxRichEditRender.ShowTablePropertiesForm(
  ASelectedCells: TdxSelectedCellsCollection);
begin
end;

procedure TdxRichEditRender.ShowTableStyleForm(AStyle: TdxTableStyle);
begin
end;

procedure TdxRichEditRender.ShowTabsForm(ATabInfo: TdxTabFormattingInfo;
  ADefaultTabWidth: Integer; const ACallback: TdxShowTabsFormCallback;
  ACallbackData: TObject);
begin
end;

procedure TdxRichEditRender.ShowTOCForm(AField: TdxField);
begin
end;

function TdxRichEditRender.ShowWarningMessage(
  const AMessage: string): TModalResult;
begin
  Result := mrOK;
end;

procedure TdxRichEditRender.UpdateControlAutoSize;
begin
end;

procedure TdxRichEditRender.UpdateHorizontalRuler;
begin
//$ do nothing
end;

procedure TdxRichEditRender.UpdateRulers;
begin
end;

procedure TdxRichEditRender.UpdateUIFromBackgroundThread(
  const AMethod: TdxAction);
begin
end;

procedure TdxRichEditRender.UpdateVerticalRuler;
begin
end;

function TdxRichEditRender.UseStandardDragDropMode: Boolean;
begin
  Result := True;
end;

function TdxRichEditRender.GetLayoutCalculationMode: TdxCalculationModeType;
begin
  Result := TdxCalculationModeType.Automatic;
end;

function TdxRichEditRender.DocumentLayoutProviderGetDocumentLayout: TdxDocumentLayout;
begin
  Result := GetDocumentLayoutAsync;
end;

function TdxRichEditRender.GetDocumentLayoutAsync: TdxDocumentLayout;
begin
  Result := ActiveView.DocumentLayout;
end;

procedure TdxRichEditRender.AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.DocumentLayoutInvalidated.Add(AHandler);
end;

procedure TdxRichEditRender.AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.PageFormatted.Add(AHandler);
end;

procedure TdxRichEditRender.AddDocumentFormatted(const AHandler: TdxEvent);
begin
  InnerControl.DocumentFormatted.Add(AHandler);
end;

procedure TdxRichEditRender.RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.DocumentLayoutInvalidated.Remove(AHandler);
end;

procedure TdxRichEditRender.RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  InnerControl.PageFormatted.Remove(AHandler);
end;

procedure TdxRichEditRender.RemoveDocumentFormatted(const AHandler: TdxEvent);
begin
  InnerControl.DocumentFormatted.Remove(AHandler);
end;

procedure TdxRichEditRender.PerformPageSecondaryFormatting(APage: TdxPage);
begin
  InnerControl.Formatter.PerformPageSecondaryFormatting(APage);
end;

function TdxRichEditRender.CalculateViewBounds(const AClientBounds: TRect): TRect;
var
  AViewPadding: TRect;
begin
  Result := FBounds;
  AViewPadding := ActiveView.ActualPadding;
  Result.Inflate(-AViewPadding.Left, -AViewPadding.Top, -AViewPadding.Right, -AViewPadding.Bottom);
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(Result, DocumentModel.DpiX, DocumentModel.DpiY);
end;

procedure TdxRichEditRender.RecreatePainters(AView: TdxRichEditView);
begin
  FBackgroundPainter.Free;
  FBackgroundPainter := AView.CreateBackgroundPainter;
end;

function TdxRichEditRender.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

function TdxRichEditRender.GetEditValue: Variant;
begin
  if FIsEditValueNull then
    Result := NULL
  else
    Result := TcxInplaceRichEditDocumentHelper.GetEditValue(InnerControl.Document);
end;

function TdxRichEditRender.GetHeight: Integer;
begin
  Result := FBounds.Height;
end;

function TdxRichEditRender.GetWidth: Integer;
begin
  Result := FBounds.Width;
end;

procedure TdxRichEditRender.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    FBounds.Height := Value;
    OnResize;
  end;
end;

procedure TdxRichEditRender.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    FBounds.Width := Value;
    OnResize;
  end;
end;

procedure TdxRichEditRender.SetBounds(const Value: TRect);
begin
  if not FBounds.IsEqual(Value) then
  begin
    FBounds := Value;
    OnResize;
  end;
end;

procedure TdxRichEditRender.SetEditValue(const Value: Variant);
begin
  FIsEditValueNull := VarIsNull(Value);
  TcxInplaceRichEditDocumentHelper.SetEditValue(InnerControl.DocumentModel, InnerControl.Document, Value);
end;

end.
