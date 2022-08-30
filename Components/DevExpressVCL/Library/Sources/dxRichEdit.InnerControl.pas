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

unit dxRichEdit.InnerControl;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Controls, Generics.Defaults, Generics.Collections, Forms, Rtti,
  cxControls, dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusClasses, cxGeometry,

  dxGenerics,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.Layout.Painters,
  dxRichEdit.ServiceManager,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Commands.IDs,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.View.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.ImportExportHelper,
  dxRichEdit.Export.Core,
  dxRichEdit.Import.Core,
  dxRichEdit.Options.Core,
  dxRichEdit.Options,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Types;

type
  TdxDocumentModelAccessor = class;
  TdxInnerRichEditDocumentServer = class;
  TdxRichEditDocumentServerOptions = class;

  { TdxRichEditControlDeferredChanges }

  TdxRichEditControlDeferredChanges = class
  private
    FResize: Boolean;
    FRedraw: Boolean;
    FRedrawAction: TdxRefreshAction;
    FRaiseUpdateUI: Boolean;
  public
		property Redraw: Boolean read FRedraw write FRedraw;
    property Resize: Boolean read FResize write FResize;
    property RaiseUpdateUI: Boolean read FRaiseUpdateUI write FRaiseUpdateUI;
    property RedrawAction: TdxRefreshAction read FRedrawAction write FRedrawAction;
  end;

  { TdxDocumentDeferredChanges }

  TdxDocumentDeferredChanges = class
  private
    FChangeActions: TdxDocumentModelChangeActions;
    FStartRunIndex: TdxRunIndex;
    FEndRunIndex: TdxRunIndex;
  public
    property ChangeActions: TdxDocumentModelChangeActions read FChangeActions write FChangeActions;
    property StartRunIndex: TdxRunIndex read FStartRunIndex write FStartRunIndex;
    property EndRunIndex: TdxRunIndex read FEndRunIndex write FEndRunIndex;
  end;

  { IdxInnerRichEditControlOwner }

  IdxInnerRichEditControlOwner = interface(IdxInnerRichEditDocumentServerOwner)
  ['{06330B38-247B-48C7-9296-42FE3CE94C12}']
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
      property Enabled: Boolean read GetEnabled;
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

    property RichEditControl: IdxRichEditControl read GetRichEditControl;


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
    property CommentPadding: TdxCommentPadding read GetCommentPadding;
  end;

  { IdxRichEditDocumentServer }

  TdxCancelEventHandler = TdxMulticastMethod<TCloseQueryEvent>;

  { TdxDocumentModelAccessor }

  TdxDocumentModelAccessor = class
  private
    FDocumentModel: TdxDocumentModel;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    property DocumentModel: TdxDocumentModel read FDocumentModel;
  end;

  { TdxDocumentContentChangedEventArgs }

  TdxDocumentContentChangedEventArgs = class(TdxEventArgs)
  strict private
    FSuppressBindingNotifications: Boolean;
  public
    constructor Create(ASuppressBindingNotifications: Boolean);
    property SuppressBindingNotifications: Boolean read FSuppressBindingNotifications;
  end;

  TdxDocumentContentChangedEvent = procedure(Sender: TObject; E: TdxDocumentContentChangedEventArgs) of object;

  { TdxExplicitBoxMeasurerProvider }

  TdxExplicitBoxMeasurerProvider = class(TInterfacedObject, IdxBoxMeasurerProvider)
  strict private
    FMeasurer: TdxBoxMeasurer;
    function GetMeasurer: TdxBoxMeasurer;
  public
    constructor Create(AMeasurer: TdxBoxMeasurer);

    property Measurer: TdxBoxMeasurer read FMeasurer;
  end;

  { TdxHeaderFooterEditingEventArgs }

  TdxHeaderFooterEditingEventArgs = class(TdxEventArgs);

  { TdxHeaderFooterEditingEvent }

  TdxHeaderFooterEditingEvent = procedure(Sender: TObject; E: TdxHeaderFooterEditingEventArgs) of object;
  TdxHeaderFooterEditingEventHandler = TdxMulticastMethod<TdxHeaderFooterEditingEvent>;

  TdxBeforePagePaintEvent = procedure(Sender: TObject; E: TdxRichEditBeforePagePaintEventArgs) of object;
  TdxBeforePagePaintEventHandler = TdxMulticastMethod<TdxBeforePagePaintEvent>;

  { TdxInnerRichEditDocumentServer }

  TdxInnerRichEditDocumentServer = class(TcxIUnknownObject,
    IdxBatchUpdateable,
    IdxBatchUpdateHandler,
    IdxBoxMeasurerProvider,
    IdxServiceContainer,
    IdxRichEditDocumentContainer,
    IdxRichEditDocumentServer)
  strict private
    FOwner: IdxInnerRichEditDocumentServerOwner;
    FIsDisposed: Boolean;
    FThreadId: Integer;
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
    FExistingDocumentModel: TdxDocumentModel;
    FDocumentModel: TdxDocumentModel;
    FDocumentModelAccessor: TdxDocumentModelAccessor;
    FDocumentModelTemplate: TdxDocumentModel;
    FOptions: TdxRichEditControlOptionsBase;
    FMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
    FDocumentDeferredChanges: TdxDocumentDeferredChanges;
    FDocumentDeferredChangesOnIdle: TdxDocumentDeferredChanges;
    FMeasurementUnit: TdxMeasurementUnit;
    FNativeDocument: IdxRichEditDocument;
    FDocumentLayout: TdxRichEditDocumentLayout;
    FDpiX: Single;
    FDpiY: Single;
    FLayoutCalculationMode: TdxCalculationModeType;
    FFormatter: TdxBackgroundFormatter;
    FFormattingController: TdxDocumentFormattingController;
    FModelDocumentLayout: TdxDocumentLayout;
    FGraphics: TdxGraphics;
    FMeasurer: TdxBoxMeasurer;
    FOnPlainTextChanged: TdxNotifyEventHandler;
    FOnRtfTextChanged: TdxNotifyEventHandler;
    FOnHtmlTextChanged: TdxNotifyEventHandler;
    FOnOpenXmlBytesChanged: TdxNotifyEventHandler;
    FOnModifiedChanged: TdxNotifyEventHandler;
    FOnUnitChanging: TdxNotifyEventHandler;
    FOnUnitChanged: TdxNotifyEventHandler;
    FOnCalculateDocumentVariable: TdxCalculateDocumentVariableEventHandler;
    FOnBeforeImport: TdxBeforeImportEventHandler;
    FOnBeforeExport: TdxBeforeExportEventHandler;
    FOnAfterExport: TdxNotifyEventHandler;
    FOnInitializeDocument: TdxEventHandler;
    FOnInvalidFormatException: TdxRichEditInvalidFormatExceptionEventHandler;
    FOnUnhandledException: TdxRichEditUnhandledExceptionEventHandler;
    FOnSelectionChanged: TdxNotifyEventHandler;
    FOnStartHeaderFooterEditing: TdxHeaderFooterEditingEventHandler;
    FOnFinishHeaderFooterEditing: TdxHeaderFooterEditingEventHandler;
    FOnDocumentProtectionChanged: TdxNotifyEventHandler;
    FOnActiveViewChanged: TdxNotifyEventHandler;
    FOnDocumentLoaded: TdxNotifyEventHandler;
    FOnEmptyDocumentCreated: TdxNotifyEventHandler;
    FOnUpdateUI: TdxNotifyEventHandler;
    FOnDocumentClosing: TdxCancelEventHandler;
    FOnCustomizeMergeFields: TdxCustomizeMergeFieldsEventHandler;
    FOnMailMergeStarted: TdxMailMergeStartedEventHandler;
    FOnMailMergeRecordStarted: TdxMailMergeRecordStartedEventHandler;
    FOnMailMergeRecordFinished: TdxMailMergeRecordFinishedEventHandler;
    FOnMailMergeFinished: TdxMailMergeFinishedEventHandler;
    FOnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEventHandler;
    FOnPageBackgroundChanged: TdxNotifyEventHandler;
    FOnBeforePagePaint: TdxBeforePagePaintEventHandler;
    FDocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler;
    FDocumentFormatted: TdxEventHandler;
    FPageFormatted: TdxPageFormattedEventHandler;
    FOnContentChanged: TdxNotifyEventHandler;

    function GetControl: TWinControl;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetMeasurementUnit: TdxMeasurementUnit;
    function IdxRichEditDocumentServer.GetDocumentModel = GetDocumentModelIntf;
    function GetDocumentModelIntf: TdxDocumentModel;
    procedure AddAfterExportHandler(const AHandler: TNotifyEvent);
    procedure RemoveAfterExportHandler(const AHandler: TNotifyEvent);
    procedure AddSelectionChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveSelectionChangedHandler(const AHandler: TNotifyEvent);
    procedure AddDocumentLoadedHandler(const AHandler: TNotifyEvent);
    procedure RemoveDocumentLoadedHandler(const AHandler: TNotifyEvent);
    procedure AddEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
    procedure RemoveEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
    procedure AddDocumentClosingHandler(const AHandler: TCloseQueryEvent);
    procedure RemoveDocumentClosingHandler(const AHandler: TCloseQueryEvent);
    procedure AddContentChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveContentChangedHandler(const AHandler: TNotifyEvent);
    procedure AddModifiedChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveModifiedChangedHandler(const AHandler: TNotifyEvent);
    procedure AddUnitChangingHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangingHandler(const AHandler: TNotifyEvent);
    procedure AddUnitChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangedHandler(const AHandler: TNotifyEvent);
    procedure AddCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
    procedure RemoveCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
    procedure AddBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
    procedure RemoveBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
    procedure AddBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
    procedure RemoveBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
    procedure AddInitializeDocumentHandler(const AHandler: TdxEvent);
    procedure RemoveInitializeDocumentHandler(const AHandler: TdxEvent);
    procedure AddRtfTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveRtfTextChangedHandler(const AHandler: TNotifyEvent);
    procedure AddHtmlTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveHtmlTextChangedHandler(const AHandler: TNotifyEvent);

    procedure SetLayoutCalculationMode(const AValue: TdxCalculationModeType);
    function GetDocument: IdxRichEditDocument;
    function GetDocumentModelTemplate: TdxDocumentModel;
    function GetNumberingListsTemplate: TdxAbstractNumberingListCollection;
    function GetMeasurer: TdxBoxMeasurer;
    function GetLayoutUnit: TdxDocumentLayoutUnit;
    procedure SetLayoutUnitProperty(const AValue: TdxDocumentLayoutUnit);
    procedure SetMeasurementUnitProperty(AValue: TdxMeasurementUnit);
    function GetModified: Boolean;
    procedure SetModified(AValue: Boolean);
    function GetIsDocumentProtected: Boolean;
    function GetIsEditable: Boolean;
    function GetUIUnit: TdxMeasurementUnit;
    function GetNativeDocument: IdxRichEditDocument;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetRtfText: string;
    procedure SetRtfText(const AValue: string);
    function GetHtmlText: string;
    procedure SetHtmlText(const AValue: string);
    function GetDocumentLayout: TdxRichEditDocumentLayout;
    function GetIsUpdateLocked: Boolean;
    function GetDocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler;
    function GetDocumentFormatted: TdxEventHandler;
    function GetPageFormatted: TdxPageFormattedEventHandler;
    function GetOwner: IdxInnerRichEditDocumentServerOwner;
  protected
    // IdxServiceProvider
    function GetService(const AServiceType: TdxServiceType): IInterface; overload;
    // IdxServiceContainer
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType; APromote: Boolean); overload;

    function GetFormattingController: TdxDocumentFormattingController; virtual;
    function GetModelDocumentLayout: TdxDocumentLayout; virtual;
    function GetDefaultLayoutCalculationMode: TdxCalculationModeType; virtual;
    procedure InitializeBackgroundFormatter; virtual;
    procedure InitializeEmptyDocumentModel(ADocumentModel: TdxDocumentModel); virtual;
    procedure OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
    procedure DisposeBackgroundFormatter; virtual;
    procedure OnDocumentLayoutChanged(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs); virtual;
    function NotifyDocumentLayoutChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
      ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(const AValue: Boolean); virtual;
    function GetEnabled: Boolean; virtual;
    function CreateNativeDocument: IdxRichEditDocument; virtual;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument; virtual;
    function GetActualReadOnly: Boolean; virtual;
    function GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean; virtual;
    procedure InitializeDocumentLayout;
    function CreateDocumentLayout: TdxRichEditDocumentLayout; virtual;
    procedure RaiseContentChanged(ASuppressBindingNotifications: Boolean); virtual;
    procedure RaisePlainTextChanged; virtual;
    procedure RaiseRtfTextChanged; virtual;
    procedure RaiseHtmlTextChanged; virtual;
    procedure RaiseOpenXmlBytesChanged; virtual;
    procedure RaiseModifiedChanged; virtual;
    procedure RaiseUnitChanging; virtual;
    procedure RaiseUnitChanged; virtual;
    function RaiseCalculateDocumentVariable(AArgs: TdxCalculateDocumentVariableEventArgs): Boolean; virtual;
    procedure RaiseBeforeImport(AArgs: TdxBeforeImportEventArgs); virtual;
    procedure RaiseBeforeExport(AArgs: TdxBeforeExportEventArgs); virtual;
    procedure RaiseAfterExport; virtual;
    procedure RaiseInitializeDocument(AArgs: TdxEventArgs); virtual;
    procedure RaiseInvalidFormatException(E: Exception); virtual;
    function RaiseUnhandledException(E: Exception): Boolean; virtual;
    procedure RaiseSelectionChanged; virtual;
    procedure RaiseStartHeaderFooterEditing; virtual;
    procedure RaiseFinishHeaderFooterEditing; virtual;
    procedure RaiseDocumentProtectionChanged; virtual;
    procedure RaiseActiveViewChanged; virtual;
    procedure RaiseDocumentLoaded; virtual;
    procedure RaiseEmptyDocumentCreated; virtual;
    procedure RaiseUpdateUI; virtual;
    function RaiseDocumentClosing: Boolean; virtual;
    function RaiseCustomizeMergeFields(AArgs: TdxCustomizeMergeFieldsEventArgs): TArray<IdxRichEditMergeFieldName>; virtual;
    procedure RaiseMailMergeStarted(AArgs: TdxMailMergeStartedEventArgs); virtual;
    procedure RaiseMailMergeRecordStarted(AArgs: TdxMailMergeRecordStartedEventArgs); virtual;
    procedure RaiseMailMergeRecordFinished(AArgs: TdxMailMergeRecordFinishedEventArgs); virtual;
    procedure RaiseMailMergeFinished(AArgs: TdxMailMergeFinishedEventArgs); virtual;
    procedure RaiseMailMergeGetTargetDocument(AArgs: TdxMailMergeGetTargetDocumentEventArgs); virtual;
    procedure RaisePageBackgroundChanged; virtual;
    procedure RaiseBeforePagePaint(AArgs: TdxRichEditBeforePagePaintEventArgs); virtual;
    procedure CreateDocumentModelTemplate; virtual;
    procedure OnFirstBeginUpdateCore; virtual;
    procedure OnLastEndUpdateCore; virtual;
    function GetDocumentModel: TdxDocumentModel; virtual;
    function CreateDocumentModelCore: TdxDocumentModel; virtual;
    procedure SubscribeDocumentModelEvents; virtual;
    procedure UnsubscribeDocumentModelEvents; virtual;
    procedure OnSelectionChanged(ASender: TObject); virtual;
    procedure OnInnerSelectionChanged(ASender: TObject); virtual;
    procedure OnBeginDocumentUpdate(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnEndDocumentUpdate(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs); virtual;
    procedure OnBeginDocumentUpdateCore; virtual;
    function ProcessEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions; virtual;
    function OnEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions; virtual;
    procedure OnInnerContentChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnContentChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnContentChangedCore(ASuppressBindingNotifications: Boolean; ASuppressUpdateUI: Boolean); virtual;
    procedure RaiseBindingNotifications; virtual;
    procedure OnModifiedChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnCalculateDocumentVariable(ASender: TObject; E: TdxCalculateDocumentVariableEventArgs); virtual;
    procedure OnMailMergeStarted(ASender: TObject; const E: TdxMailMergeStartedEventArgs); virtual;
    procedure OnMailMergeRecordStarted(ASender: TObject; const E: TdxMailMergeRecordStartedEventArgs); virtual;
    procedure OnMailMergeRecordFinished(ASender: TObject; const E: TdxMailMergeRecordFinishedEventArgs); virtual;
    procedure OnMailMergeFinished(ASender: TObject; const E: TdxMailMergeFinishedEventArgs); virtual;
    procedure OnMailMergeGetTargetDocument(ASender: TObject; const E: TdxMailMergeGetTargetDocumentEventArgs); virtual;
    procedure OnPageBackgroundChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnAfterExport(Sender: TObject);
    procedure OnBeforeExport(ASender: TObject; E: TdxBeforeExportEventArgs); virtual;
    procedure OnBeforeImport(ASender: TObject; E: TdxBeforeImportEventArgs); virtual;
    procedure OnDocumentCleared(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnInvalidFormatException(ASender: TObject; E: TdxRichEditInvalidFormatExceptionEventArgs); virtual;
    procedure OnEmptyDocumentCreated; virtual;
    procedure OnDocumentLoaded; virtual;
    procedure OnActivePieceTableChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure ApplyChangesCore(const AChangeActions: TdxDocumentModelChangeActions); virtual;
    procedure PerformRaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions); virtual;

    procedure RaiseDeferredEvents(const AChangeActions: TdxDocumentModelChangeActions); virtual;
    function CreateMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy; virtual;
    procedure CreateNewMeasurementAndDrawingStrategy; virtual;
    procedure RecreateMeasurementAndDrawingStrategy; virtual;
    function CreateOptions: TdxRichEditControlOptionsBase; virtual;
    procedure SubscribeOptionsEvents; virtual;
    procedure UnsubscribeOptionsEvents; virtual;
    procedure OnOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs); virtual;
    procedure OnOptionsMailMergeChanged; virtual;
    procedure SetMeasurementUnit(AValue: TdxMeasurementUnit); virtual;
    procedure SetLayoutUnit(AUnit: TdxDocumentLayoutUnit); virtual;
    procedure SetLayoutUnitCore(AUnit: TdxDocumentLayoutUnit); virtual;
    procedure SetDocumentModelLayoutUnit(AUnit: TdxDocumentLayoutUnit); virtual;
    procedure SetDocumentModelLayoutUnitCore(AUnit: TdxDocumentLayoutUnit); virtual;
    function CanCloseExistingDocument: Boolean; virtual;
    function CanCloseExistingDocumentCore: Boolean; virtual;
    procedure OnApplicationIdle; virtual;
    procedure LoadDocumentCore(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat; const ASourceUri: string); overload; virtual;
    procedure LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat; ALoadAsTemplate: Boolean); overload; virtual;
    procedure SaveDocumentCore(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat; const ATargetUri: string); overload; virtual;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; ATargetModel: TdxDocumentModel); overload;
    function GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions; virtual;
    function CreateCopySelectionManager: TdxCopySelectionManager; virtual;

    property ThreadId: Integer read FThreadId;
    property PredefinedFontSizeCollection: TdxPredefinedFontSizeCollection read FPredefinedFontSizeCollection;
    property NumberingListsTemplate: TdxAbstractNumberingListCollection read GetNumberingListsTemplate;
    property Measurer: TdxBoxMeasurer read GetMeasurer;
    property DocumentDeferredChanges: TdxDocumentDeferredChanges read FDocumentDeferredChanges;
    property ActualReadOnly: Boolean read GetActualReadOnly;
    property BeforePagePaint: TdxBeforePagePaintEventHandler read FOnBeforePagePaint;
    property Control: TWinControl read GetControl;
  public
    constructor Create(const AOwner: IdxInnerRichEditDocumentServerOwner); overload; virtual;
    constructor Create(const AOwner: IdxInnerRichEditDocumentServerOwner; ADpiX: Single; ADpiY: Single); overload;
    constructor Create(const AOwner: IdxInnerRichEditDocumentServerOwner; ADocumentModel: TdxDocumentModel); overload;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function GetService<T: IInterface>: T; overload;
    procedure ApplyFontAndForeColor; virtual;
    procedure OnUpdateUI; virtual;

    procedure OnLayoutCalculationModeChanged;
    function CalculatePrintDocumentLayout(Server: TObject{TdxInternalRichEditDocumentServer}): TdxDocumentLayout;
    class function CalculateDocumentLayoutResetType(AChanges: TdxDocumentModelDeferredChanges): TdxDocumentLayoutResetType; static;
    procedure RaiseDocumentLayoutInvalidated(AArgs: TdxDocumentLayoutInvalidatedEventArgs);
    procedure RaiseDocumentFormatted;
    procedure RaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions); virtual;
    procedure RaisePageFormatted(AArgs: TdxPageFormattedEventArgs);
    procedure BeginInitialize; virtual;
    procedure EndInitialize; virtual;
    procedure AddServices;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;

    function CreateNewDocument(ARaiseDocumentClosing: Boolean): Boolean; virtual;
    procedure LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocument(const AFileName: string); overload; virtual;
    procedure LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocumentTemplate(const AFileName: string); overload; virtual;
    procedure LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat; const AOptions: TdxDocumentSaveOptions); overload;

    procedure SaveDocumentCore(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat; const AOptions: TdxDocumentSaveOptions); overload;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions; virtual;
    procedure MailMerge(const ADocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;

    property Owner: IdxInnerRichEditDocumentServerOwner read GetOwner;
    property IsDisposed: Boolean read FIsDisposed;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property DocumentModelTemplate: TdxDocumentModel read GetDocumentModelTemplate;
    property DpiX: Single read FDpiX;
    property DpiY: Single read FDpiY;
    property BackgroundFormatter: TdxBackgroundFormatter read FFormatter write FFormatter;
    property FormattingController: TdxDocumentFormattingController read GetFormattingController;
    property ModelDocumentLayout: TdxDocumentLayout read GetModelDocumentLayout;
    property LayoutCalculationMode: TdxCalculationModeType read FLayoutCalculationMode write SetLayoutCalculationMode;
    property Model: TdxDocumentModelAccessor read FDocumentModelAccessor;
    property Options: TdxRichEditControlOptionsBase read FOptions;
    property LayoutUnit: TdxDocumentLayoutUnit read GetLayoutUnit write SetLayoutUnitProperty;
    property MeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy read FMeasurementAndDrawingStrategy;
    property Modified: Boolean read GetModified write SetModified;
    property NativeDocument: IdxRichEditDocument read GetNativeDocument;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Enabled: Boolean read GetEnabled;
    property IsDocumentProtected: Boolean read GetIsDocumentProtected;
    property IsEditable: Boolean read GetIsEditable;
    property MeasurementUnit: TdxMeasurementUnit read FMeasurementUnit write SetMeasurementUnitProperty;
    property UIUnit: TdxMeasurementUnit read GetUIUnit;

    property Text: string read GetText write SetText;
    property RtfText: string read GetRtfText write SetRtfText;
    property HtmlText: string read GetHtmlText write SetHtmlText;
    property Document: IdxRichEditDocument read GetDocument;
    property DocumentLayout: TdxRichEditDocumentLayout read GetDocumentLayout;
    property BatchUpdateHelper: TdxBatchUpdateHelper read FBatchUpdateHelper;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;

    property DocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler read GetDocumentLayoutInvalidated;
    property DocumentFormatted: TdxEventHandler read GetDocumentFormatted;
    property PageFormatted: TdxPageFormattedEventHandler read GetPageFormatted;
    property ContentChanged: TdxNotifyEventHandler read FOnContentChanged;
    property PageBackgroundChanged: TdxNotifyEventHandler read FOnPageBackgroundChanged;
    property PlainTextChanged: TdxNotifyEventHandler read FOnPlainTextChanged;
    property RtfTextChanged: TdxNotifyEventHandler read FOnRtfTextChanged;
    property HtmlTextChanged: TdxNotifyEventHandler read FOnHtmlTextChanged;
    property OpenXmlBytesChanged: TdxNotifyEventHandler read FOnOpenXmlBytesChanged;
    property ModifiedChanged: TdxNotifyEventHandler read FOnModifiedChanged;
    property CalculateDocumentVariable: TdxCalculateDocumentVariableEventHandler read FOnCalculateDocumentVariable;
    property BeforeImport: TdxBeforeImportEventHandler read FOnBeforeImport;
    property BeforeExport: TdxBeforeExportEventHandler read FOnBeforeExport;
    property AfterExport: TdxNotifyEventHandler read FOnAfterExport;
    property InitializeDocument: TdxEventHandler read FOnInitializeDocument;
    property InvalidFormatException: TdxRichEditInvalidFormatExceptionEventHandler read FOnInvalidFormatException;
    property UnhandledException: TdxRichEditUnhandledExceptionEventHandler read FOnUnhandledException;
    property SelectionChanged: TdxNotifyEventHandler read FOnSelectionChanged;
    property StartHeaderFooterEditing: TdxHeaderFooterEditingEventHandler read FOnStartHeaderFooterEditing;
    property FinishHeaderFooterEditing: TdxHeaderFooterEditingEventHandler read FOnFinishHeaderFooterEditing;
    property DocumentProtectionChanged: TdxNotifyEventHandler read FOnDocumentProtectionChanged;
    property ActiveViewChanged: TdxNotifyEventHandler read FOnActiveViewChanged;
    property DocumentLoaded: TdxNotifyEventHandler read FOnDocumentLoaded;
    property EmptyDocumentCreated: TdxNotifyEventHandler read FOnEmptyDocumentCreated;
    property UpdateUI: TdxNotifyEventHandler read FOnUpdateUI;
    property DocumentClosing: TdxCancelEventHandler read FOnDocumentClosing;
    property UnitChanging: TdxNotifyEventHandler read FOnUnitChanging;
    property UnitChanged: TdxNotifyEventHandler read FOnUnitChanged;
    property CustomizeMergeFields: TdxCustomizeMergeFieldsEventHandler read FOnCustomizeMergeFields;
    property MailMergeStarted: TdxMailMergeStartedEventHandler read FOnMailMergeStarted;
    property MailMergeRecordStarted: TdxMailMergeRecordStartedEventHandler read FOnMailMergeRecordStarted;
    property MailMergeRecordFinished: TdxMailMergeRecordFinishedEventHandler read FOnMailMergeRecordFinished;
    property MailMergeFinished: TdxMailMergeFinishedEventHandler read FOnMailMergeFinished;
    property MailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEventHandler read FOnMailMergeGetTargetDocument;
  public
    class procedure SaveDocumentCore(ADocumentModel: TdxDocumentModel; AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string); overload; static;
  end;

  { TdxInnerRichEditControl }

  TdxInnerRichEditControl = class(TdxInnerRichEditDocumentServer,
    IdxInnerControl//,
//    IdxGestureStateIndicator
    )
  strict private
    FBackgroundThreadUIUpdater: TdxBackgroundThreadUIUpdater;
    FActiveView: TdxRichEditView;
    FViews: TdxRichEditCustomViewRepository;
    FActiveViewTypeBeforeActiveViewCreation: TdxNullableValue<TdxRichEditViewType>;
    FReadOnly: Boolean;
    FDeferredChanges: TdxRichEditControlDeferredChanges;
    FUpdateUIOnIdle: Boolean;
    FForceUpdateUIOnIdle: Boolean;
    FVerticalScrollbar: IdxOfficeScrollbar;
    FHorizontalScrollbar: IdxOfficeScrollbar;
    FHorizontalRuler: IdxRulerControl;
    FVerticalRuler: IdxRulerControl;
    FGestureActivated: Boolean;
    FOvertype: Boolean;
    FOnReadOnlyChanged: TdxNotifyEventHandler;
    FOnOvertypeChanged: TdxNotifyEventHandler;
    FOnSearchComplete: TdxSearchCompleteEvent;
    FOnZoomChanged: TdxNotifyEventHandler;
    FOnHyperlinkClick: TdxHyperlinkClickEventHandler;
    FOnLayoutUnitChanged: TdxNotifyEventHandler;
    FOnVisiblePagesChanged: TdxNotifyEventHandler;

    FCommandTable: TdxEnumeratedDictionary<TdxRichEditCommandId, TdxRichEditCommandClass>;
    FKeyboardController: TdxCustomKeyboardController;
    FMouseController: TdxRichEditCustomMouseController;

    procedure PopulateFieldCommands;
    procedure PopulateFloatingObjectCommands;
    procedure PopulateFormattingCommands;
    procedure PopulateSelectionCommands;
    procedure PopulateTableCommands;

    function GetOwner: IdxInnerRichEditControlOwner;
    function GetActiveViewType: TdxRichEditViewType;
    procedure SetActiveViewType(const AValue: TdxRichEditViewType);
    function GetFont: TdxGPFont;
    function GetForeColor: TdxAlphaColor;
    function GetFormatter: TdxBackgroundFormatter;
    procedure SetOvertype(const AValue: Boolean);
    function GetViewBounds: TRect;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetIsHandleCreated: Boolean;
    function GetDocumentModelIntf: TdxDocumentModel;
  protected
    function IdxInnerControl.GetDocumentModel = GetDocumentModelIntf;
    function CreateCopySelectionManager: TdxCopySelectionManager; override;
    function CreateMouseCursorCalculator: TdxMouseCursorCalculator;
    function GetActiveView: TdxRichEditView;
    function GetMouseController: TdxRichEditCustomMouseController;
    function GetOptions: TdxRichEditControlOptionsBase;
    function GetPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
    function GetHorizontalScrollBar: IdxOfficeScrollbar;
    function GetVerticalScrollBar: IdxOfficeScrollbar;
    function GetHorizontalRuler: IdxRulerControl;
    function GetVerticalRuler: IdxRulerControl;
    function GetOnSearchComplete: TdxSearchCompleteEvent;
    procedure SetOnSearchComplete(const Value: TdxSearchCompleteEvent);
    procedure DoSearchComplete(E: TdxSearchCompleteEventArgs);

    function CanSpelling: Boolean;
    function CreateSpellCheckerController: TdxSpellCheckerCustomController; virtual;
    function CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager; virtual;
    procedure OnSpellCheckerOptionsChanged;
    procedure OnSpellCheckerChanged(Sender: TObject);
    procedure RecreateSpellCheckerController; virtual;
    procedure RecreateSpellCheckerManager; virtual;

    function GetFormattingController: TdxDocumentFormattingController; override;
    function GetDefaultViewType: TdxRichEditViewType; virtual;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(const AValue: Boolean); override;
    function GetActualReadOnly: Boolean; override;
    function GetEnabled: Boolean; override;
    procedure SetMeasurementUnit(AValue: TdxMeasurementUnit); override;
    function GetModelDocumentLayout: TdxDocumentLayout; override;
    procedure CreateNewMeasurementAndDrawingStrategy; override;
    procedure OnFirstBeginUpdateCore; override;
    procedure OnLastEndUpdateCore; override;
    procedure OnBeginDocumentUpdateCore; override;
    function OnEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions; override;
    procedure OnInnerSelectionChanged(ASender: TObject); override;
    procedure ApplyChangesCore(const AChangeActions: TdxDocumentModelChangeActions); override;

    procedure ActivateMainPieceTable(const AControl: IdxRichEditControl; AStart: TdxDocumentLogPosition);
    procedure PerformRaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure OnOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs); override;
    procedure OnHiddenTextOptionChanged;
    procedure OnOptionsFormattingMarkChanged;
    procedure OnOptionsBookmarksChanged;
    procedure OnOptionsRangePermissionsChanged;
    procedure OnOptionsCommentsChanged;
    procedure OnOptionsFieldsChanged;
    procedure OnOptionsDocumentCapabilitiesChanged;
    procedure OnOptionsHorizontalRulerChanged;
    procedure DocumentModelApplyChanges(AChangeActions: TdxDocumentModelChangeActions);
    procedure RedrawEnsureSecondaryFormattingComplete; overload; virtual;
    procedure RedrawEnsureSecondaryFormattingComplete(const AAction: TdxRefreshAction); overload; virtual;
    procedure UpdateRulers; virtual;
    procedure UpdateHorizontalRuler; virtual;
    procedure UpdateVerticalRuler; virtual;
    procedure OnResize; virtual;
    function CreateViewRepository: TdxRichEditCustomViewRepository; virtual;
    procedure CreateViews; virtual;
    procedure DisposeViews; virtual;
    procedure SetActiveView(ANewView: TdxRichEditView); virtual;
    procedure SetActiveViewCore(ANewView: TdxRichEditView); virtual;
    function DeactivateView(AView: TdxRichEditView): TRect; virtual;
    procedure DeactivateViewAndClearActiveView(AView: TdxRichEditView); virtual;
    procedure ActivateView(AView: TdxRichEditView; const AViewBounds: TRect); virtual;
    function GetDefaultLayoutCalculationMode: TdxCalculationModeType; override;
    function NotifyDocumentLayoutChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
      ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; override;
    procedure InitializeBackgroundFormatter; override;
    function CreateBackgroundFormatter(AController: TdxDocumentFormattingController): TdxBackgroundFormatter; virtual;
    procedure SubscribeActiveViewEvents; virtual;
    procedure UnsubscribeActiveViewEvents; virtual;
    procedure OnActiveViewZoomChanging(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnActiveViewZoomChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure OnActiveViewBackColorChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure ActivateViewPlatformSpecific(AView: TdxRichEditView); virtual;
    procedure DeactivateViewPlatformSpecific(AView: TdxRichEditView); virtual;
    procedure OnResizeCore; virtual;
    function CalculateActualViewBounds(const APreviousViewBounds: TRect): TRect; virtual;
    procedure OnUpdateUICore; virtual;
    procedure OnApplicationIdle; override;
    procedure OnOptionsChangedPlatformSpecific(E: TdxOptionChangedEventArgs); virtual;
    procedure ApplyFontAndForeColorCore(ACharacterProperties: TdxCharacterProperties); virtual;
    function GetDefaultCharacterProperties: TdxCharacterProperties; virtual;
    function ShouldApplyForeColor: Boolean; virtual;
    function ShouldApplyFont: Boolean; virtual;
    procedure ApplyFont(ACharacterProperties: TdxCharacterProperties; AFont: TdxGPFont); virtual;
    procedure OnReadOnlyChanged; virtual;
    procedure OnOvertypeChanged; virtual;
    procedure ApplyChangesCorePlatformSpecific(AChangeActions: TdxDocumentModelChangeActions); virtual;
    procedure OnZoomFactorChangingPlatformSpecific; virtual;
    function GetHyperlinkCollection(AField: TdxField): IdxRichEditHyperlinkCollection;
    function OnHyperlinkClick(AField: TdxField; AAllowForModifiers: Boolean): Boolean; virtual;
    procedure OpenHyperlink(AField: TdxField);
    function UnitsToLayoutUnits(const APoint: TdxPointF): TPoint;
    function LayoutUnitsToUnits(const ASize: TSize): TdxSizeF; overload;
    function LayoutUnitsToUnits(const ABounds: TRect): TdxRectF; overload;

    procedure SetLayoutUnitCore(AUnit: TdxDocumentLayoutUnit); override;
    procedure SetDocumentModelLayoutUnitCore(AUnit: TdxDocumentLayoutUnit); override;
    procedure BeginScrollbarUpdate(const AScrollbar: IdxOfficeScrollbar); virtual;
    procedure EndScrollbarUpdate(const AScrollbar: IdxOfficeScrollbar); virtual;
    procedure LoadDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner); virtual;
    function ExportDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner; ACalc: TdxExportersCalculator): Boolean; overload;
    function ExportDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner; ACalc: TdxExportersCalculator;
      const AOptions: IdxDocumentSaveOptions): Boolean; overload;
    procedure AddServices; virtual;
    function GetVerticalScrollValue: Int64; virtual;
    procedure SetVerticalScrollValue(AValue: Int64); virtual;
    function GetVerticalScrollPosition: Int64; virtual;
    procedure SetVerticalScrollPosition(AValue: Int64); virtual;
    procedure OnGestureBegin;
    procedure OnGestureEnd;

    procedure RaiseReadOnlyChanged; virtual;
    procedure RaiseOvertypeChanged; virtual;
    procedure RaiseSearchComplete(E: TdxSearchCompleteEventArgs);
    procedure RaiseZoomChanged; virtual;
    procedure RaiseHyperlinkClick(AArgs: TdxHyperlinkClickEventArgs); virtual;
    procedure RaiseLayoutUnitChanged; virtual;
    procedure RaiseVisiblePagesChanged; virtual;

    function CreateKeyboardController: TdxCustomKeyboardController; virtual; abstract;
    function CreateMouseController: TdxRichEditCustomMouseController; virtual; abstract;
    procedure PopulateCommands; virtual;
    procedure RaiseDocumentProtectionChanged; override;
    property CommandTable: TdxEnumeratedDictionary<TdxRichEditCommandId, TdxRichEditCommandClass> read FCommandTable;

    property ForceUpdateUIOnIdle: Boolean read FForceUpdateUIOnIdle write FForceUpdateUIOnIdle;
    property ViewBounds: TRect read GetViewBounds;
    property VerticalScrollBar: IdxOfficeScrollbar read FVerticalScrollbar;
    property HorizontalScrollBar: IdxOfficeScrollbar read FHorizontalScrollbar;
    property HorizontalRuler: IdxRulerControl read FHorizontalRuler;
    property VerticalRuler: IdxRulerControl read FVerticalRuler;
    property IsHandleCreated: Boolean read GetIsHandleCreated;
    property GestureActivated: Boolean read FGestureActivated;
  public
    constructor Create(const AOwner: IdxInnerRichEditControlOwner); overload;
    constructor Create(const AOwner: IdxInnerRichEditControlOwner; ADpiX: Single; ADpiY: Single); overload;
    destructor Destroy; override;

    procedure StopFormatting; virtual;
    procedure BeginInitialize; override;
    procedure EndInitialize; override;

    function CreateCommand(ACommandId: TdxRichEditCommandId): TdxRichEditCommand; virtual;
    procedure ScrollToCaret(ARelativeVerticalPosition: Single);
    procedure Undo;
    procedure Redo;
    procedure ClearUndo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure SelectAll;
    procedure DeselectAll;
    procedure LoadDocument; overload; virtual;
    procedure LoadDocument(const AParent: IdxInnerRichEditDocumentServerOwner); overload; virtual;
    function SaveDocumentAs: Boolean; overload; virtual;
    function SaveDocumentAs(const AParent: IdxInnerRichEditDocumentServerOwner): Boolean; overload; virtual;
    function SaveDocument: Boolean; overload; virtual;
    function SaveDocument(const AParent: IdxInnerRichEditDocumentServerOwner): Boolean; overload; virtual;

    procedure ApplyFontAndForeColor; override;
    procedure BeginDocumentRendering; virtual;
    procedure DoShortCut(Args: TdxRichEditShortCutEventArgs); virtual;
    procedure EndDocumentRendering; virtual;
    function IsHyperlinkModifierKeysPress: Boolean; virtual;
    procedure UpdateVerticalScrollBar(AAvoidJump: Boolean); virtual;
    procedure OnUpdateUI; override;

    property BackgroundThreadUIUpdater: TdxBackgroundThreadUIUpdater read FBackgroundThreadUIUpdater write FBackgroundThreadUIUpdater;
    property ControlDeferredChanges: TdxRichEditControlDeferredChanges read FDeferredChanges;
    property UpdateUIOnIdle: Boolean read FUpdateUIOnIdle write FUpdateUIOnIdle;
    procedure RegisterCommand(ACommandClass: TdxRichEditCommandClass);
    procedure UnregisterCommand(ACommandClass: TdxRichEditCommandClass);
    property KeyboardController: TdxCustomKeyboardController read FKeyboardController;
    property MouseController: TdxRichEditCustomMouseController read FMouseController;

    property Owner: IdxInnerRichEditControlOwner read GetOwner;
    property ActiveView: TdxRichEditView read FActiveView;
    property ActiveViewType: TdxRichEditViewType read GetActiveViewType write SetActiveViewType;
    property DefaultViewType: TdxRichEditViewType read GetDefaultViewType;
    property Views: TdxRichEditCustomViewRepository read FViews;
    property Font: TdxGPFont read GetFont;
    property ForeColor: TdxAlphaColor read GetForeColor;
    property Formatter: TdxBackgroundFormatter read GetFormatter;
    property Overtype: Boolean read FOvertype write SetOvertype;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property VerticalScrollPosition: Int64 read GetVerticalScrollPosition write SetVerticalScrollPosition;
    property VerticalScrollValue: Int64 read GetVerticalScrollValue write SetVerticalScrollValue;
    property ReadOnlyChanged: TdxNotifyEventHandler read FOnReadOnlyChanged;
    property OvertypeChanged: TdxNotifyEventHandler read FOnOvertypeChanged;
    property SearchComplete: TdxSearchCompleteEvent read FOnSearchComplete;
    property ZoomChanged: TdxNotifyEventHandler read FOnZoomChanged;
    property HyperlinkClick: TdxHyperlinkClickEventHandler read FOnHyperlinkClick;
    property VisiblePagesChanged: TdxNotifyEventHandler read FOnVisiblePagesChanged;
  end;

  { TdxRichEditScrollbarOptions }

  TdxRichEditScrollbarOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      Visibility = TdxRulerOptions.TAction.Visibility;
    end;
  strict private
    FVisibility: TdxRichEditScrollbarVisibility;
  private
    procedure SetVisibility(const Value: TdxRichEditScrollbarVisibility);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property Visibility: TdxRichEditScrollbarVisibility read FVisibility write SetVisibility default TdxRichEditScrollbarVisibility.Auto;
  end;

  { TdxVerticalScrollbarOptions }

  TdxVerticalScrollbarOptions = class(TdxRichEditScrollbarOptions)
  published
    property Visibility;
  end;

  { TdxHorizontalScrollbarOptions }

  TdxHorizontalScrollbarOptions = class(TdxRichEditScrollbarOptions)
  published
    property Visibility;
  end;

  { RichEditDocumentServerOptions }

  TdxRichEditDocumentServerOptions = class(TdxRichEditControlOptionsBase)
  public
    constructor Create(const ADocumentServer: IdxRichEditDocumentServer); override;
  published
    property Behavior;
    property DocumentCapabilities;
    property DocumentSaveOptions;
    property Fields;
    property HorizontalRuler;
    property Hyperlinks;
    property Layout;
    property MailMerge;
    property Bookmarks;
    property TableOptions;
    property VerticalRuler;
  end;

  { TdxRichEditControlCustomOptions }

  TdxRichEditControlCustomOptions = class(TdxRichEditControlOptionsBase)
  strict private
    FVerticalScrollbar: TdxVerticalScrollbarOptions;
    FHorizontalScrollbar: TdxHorizontalScrollbarOptions;
    procedure SetHorizontalScrollbar(const Value: TdxHorizontalScrollbarOptions);
    procedure SetVerticalScrollbar(const Value: TdxVerticalScrollbarOptions);
  protected
    procedure CreateInnerOptions; override;
    procedure SubscribeInnerOptions; override;
  public
    destructor Destroy; override;
    property Behavior;
    property DocumentCapabilities;
    property DocumentSaveOptions;
    property Fields;
    property HorizontalRuler;
    property HorizontalScrollbar: TdxHorizontalScrollbarOptions read FHorizontalScrollbar write SetHorizontalScrollbar;
    property Hyperlinks;
    property Layout;
    property MailMerge;
    property Bookmarks;
    property TableOptions;
    property VerticalRuler;
    property VerticalScrollbar: TdxVerticalScrollbarOptions read FVerticalScrollbar write SetVerticalScrollbar;
  end;

  { TdxDocumentChangesHandler }

  TdxDocumentChangesHandler = class abstract
  strict private
    FFormattingController: TdxDocumentFormattingController;
    FFormatter: TdxBackgroundFormatter;
  protected
    procedure SubscribePageFormattingComplete; virtual; abstract;
    procedure UnsubscribePageFormattingComplete; virtual; abstract;
    procedure ProcessSelectionChanges(AChangeActions: TdxDocumentModelChangeActions); virtual; abstract;
    procedure ProcessSpellChanges(AChangeActions: TdxDocumentModelChangeActions); virtual; abstract;
    procedure ResetPages; virtual; abstract;
    procedure GeneratePages; virtual; abstract;
  public
    constructor Create(AFormatter: TdxBackgroundFormatter);
    function NotifyDocumentChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
      ADebugSuppressControllerReset: Boolean; ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; virtual;
    class function SetNewPosition(const APos: TdxDocumentModelPosition; AParagraph: TdxParagraphBase): TdxDocumentModelPosition; static;
    class function CalculateResetFromPosition(AFormattingController: TdxDocumentFormattingController; APieceTable: TdxCustomPieceTable;
      AChanges: TdxDocumentModelDeferredChanges; AResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; static;
    class function EnsurePositionVisibleWhenHiddenTextNotShown(ADocumentModel: TdxCustomDocumentModel;
      const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition; static;
    class function EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition; static;
    class function EnsureTopLevelParagraph(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; static;

    property FormattingController: TdxDocumentFormattingController read FFormattingController;
    property Formatter: TdxBackgroundFormatter read FFormatter;
  end;

implementation

uses
  Math, Menus, Windows, RTLConsts,
  dxTypeHelpers, dxSpellCheckerCore, dxThreading,

  dxRichEdit.Printing,
  dxRichEdit.Utils.ThreadSyncService,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.Api.NativeDocumentBase,
  dxRichEdit.DocumentLayout,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.MailMergeHelper,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.View.PrintLayout,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Bookmarks,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Keyboard,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.Commands.FileOperations,
  dxRichEdit.Commands.FloatingObject,
  dxRichEdit.Commands.Hyperlink,
  dxRichEdit.Commands.Columns,
  dxRichEdit.Commands.Fields,
  dxRichEdit.Commands.HeaderFooter,
  dxRichEdit.Commands.SpellChecker,
  dxRichEdit.Commands.TableOfContents,
  dxRichEdit.Control.AutoCorrect,
  dxRichEdit.Export.DocumentExportHelper,
  dxRichEdit.Import.DocumentImportHelper,
  dxRichEdit.InternalRichEditDocumentServer;

type
  { TdxDocumentServerLayoutService }

  TdxDocumentServerLayoutService = class(TInterfacedObject, IdxDocumentLayoutService)
  strict private
    FServer: TdxInnerRichEditDocumentServer;
    FCachedLayout: TdxDocumentLayout;
    FMeasurer: TdxBoxMeasurer;
    procedure SubscribeEvents;
    procedure UnsubscribeEvents;
    procedure OnDocumentModelBeforeEndDocumentUpdate(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
  protected
    property Server: TdxInnerRichEditDocumentServer read FServer;
  public
    constructor Create(AServer: TdxInnerRichEditDocumentServer);
    destructor Destroy; override;

    function CalculateDocumentLayout: TdxDocumentLayout;
    procedure ResetLayout;
    function CreateService(ADocumentModel: TdxDocumentModel): IdxDocumentLayoutService;
    procedure RemoveService(ADocumentModel: TdxDocumentModel);
  end;

  { TdxControlDocumentChangesHandler }

  TdxControlDocumentChangesHandler = class(TdxDocumentChangesHandler)
  strict private
    FView: TdxRichEditView;
  protected
    procedure ProcessSelectionChanges(AChangeActions: TdxDocumentModelChangeActions); override;
    procedure ProcessSpellChanges(AChangeActions: TdxDocumentModelChangeActions); override;
    procedure ResetPages; override;
    procedure GeneratePages; override;
    procedure SubscribePageFormattingComplete; override;
    procedure UnsubscribePageFormattingComplete; override;
  public
    constructor Create(AView: TdxRichEditView);
    function NotifyDocumentChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
      ADebugSuppressControllerReset: Boolean; ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; override;

    property View: TdxRichEditView read FView;
  end;

  { TdxServerDocumentChangesHandler }

  TdxServerDocumentChangesHandler = class(TdxDocumentChangesHandler)
  protected
    procedure ProcessSelectionChanges(AChangeActions: TdxDocumentModelChangeActions); override;
    procedure ProcessSpellChanges(AChangeActions: TdxDocumentModelChangeActions); override;
    procedure ResetPages; override;
    procedure GeneratePages; override;
    procedure SubscribePageFormattingComplete; override;
    procedure UnsubscribePageFormattingComplete; override;
  public
    function NotifyDocumentChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
      ADebugSuppressControllerReset: Boolean; ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; override;
  end;

{ TdxDocumentServerLayoutService }

constructor TdxDocumentServerLayoutService.Create(AServer: TdxInnerRichEditDocumentServer);
begin
  inherited Create;
  FServer := AServer;
  SubscribeEvents;
end;

destructor TdxDocumentServerLayoutService.Destroy;
begin
  FreeAndNil(FMeasurer);
  FreeAndNil(FCachedLayout);
  inherited Destroy;
end;

procedure TdxDocumentServerLayoutService.SubscribeEvents;
begin
  FServer.DocumentModel.BeforeEndDocumentUpdate.Add(OnDocumentModelBeforeEndDocumentUpdate);
end;

procedure TdxDocumentServerLayoutService.UnsubscribeEvents;
begin
  FServer.DocumentModel.BeforeEndDocumentUpdate.Remove(OnDocumentModelBeforeEndDocumentUpdate);
end;

procedure TdxDocumentServerLayoutService.OnDocumentModelBeforeEndDocumentUpdate(ASender: TObject;
  E: TdxDocumentUpdateCompleteEventArgs);
var
  AChanges: TdxDocumentModelChangeActions;
begin
  AChanges := E.DeferredChanges.ChangeActions;
  if AChanges * [TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
      TdxDocumentModelChangeAction.ResetPrimaryLayout, TdxDocumentModelChangeAction.ResetSecondaryLayout] <> [] then
    FreeAndNil(FCachedLayout);
end;

function TdxDocumentServerLayoutService.CalculateDocumentLayout: TdxDocumentLayout;
var
  AControl: TdxInnerRichEditControl;
  ADocumentPrinter: TdxBrickDocumentPrinter;
begin
  if FCachedLayout <> nil then
    Exit(FCachedLayout);

  AControl := Server as TdxInnerRichEditControl;
  if AControl <> nil then
    AControl.Formatter.BeginDocumentUpdate;
  Server.DocumentModel.ResetParagraphs;
  try
    ADocumentPrinter := TdxBrickDocumentPrinter.Create(Server.DocumentModel);
    try
      ADocumentPrinter.Format;
      FCachedLayout := ADocumentPrinter.DocumentLayout;
      if FMeasurer <> ADocumentPrinter.DocumentLayout.Measurer then
      begin
        FMeasurer.Free;
        FMeasurer := ADocumentPrinter.DocumentLayout.Measurer;
      end;
    finally
      ADocumentPrinter.Free;
    end;
  finally
    Server.DocumentModel.ResetParagraphs;
    if AControl <> nil then
      AControl.Formatter.EndDocumentUpdate;
  end;
  Result := FCachedLayout;
end;

procedure TdxDocumentServerLayoutService.ResetLayout;
begin
  FreeAndNil(FCachedLayout);
end;

function TdxDocumentServerLayoutService.CreateService(ADocumentModel: TdxDocumentModel): IdxDocumentLayoutService;
var
  AServer: TdxInnerRichEditDocumentServer;
begin
  AServer := TdxInnerRichEditDocumentServer.Create(Server.Owner, ADocumentModel);
  AServer.BeginInitialize;
  AServer.EndInitialize;
  Result := ADocumentModel.GetService<IdxDocumentLayoutService>;
end;

procedure TdxDocumentServerLayoutService.RemoveService(ADocumentModel: TdxDocumentModel);
var
  AService: TdxDocumentServerLayoutService;
  AContainer: IdxServiceContainer;
begin
  AContainer := ADocumentModel as IdxServiceContainer;
  AService := Safe<TdxDocumentServerLayoutService>.Cast(TObject(AContainer.GetService(IdxDocumentLayoutService)));
  AService.UnsubscribeEvents;
  AContainer.RemoveService(IdxDocumentLayoutService);
  FreeAndNil(AService.FServer);
end;

{ TdxDocumentChangesHandler }

constructor TdxDocumentChangesHandler.Create(AFormatter: TdxBackgroundFormatter);
begin
  FFormattingController := AFormatter.DocumentFormatter.Controller;
  FFormatter := AFormatter;
end;

function TdxDocumentChangesHandler.NotifyDocumentChanged(APieceTable: TdxCustomPieceTable; const AChanges: TdxDocumentModelDeferredChanges;
  ADebugSuppressControllerReset: Boolean; ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
var
  AFrom, ATo, ASecondaryFrom: TdxDocumentModelPosition;
begin
  Assert(APieceTable.DocumentModel.IsUpdateLockedOrOverlapped);

  if ADocumentLayoutResetType = TdxDocumentLayoutResetType.AllPrimaryLayout then
  begin
    AChanges.SetChangeParagraphStart(0);
    AChanges.SetChangeParagraphEnd(APieceTable.Paragraphs.Count - 1);
  end;
  ProcessSelectionChanges(AChanges.ChangeActions);

  AFrom := CalculateResetFromPosition(FormattingController, APieceTable, AChanges, ADocumentLayoutResetType);
  ATo := AChanges.ChangeEnd;
  if ATo < AFrom then
    ATo := AFrom;
  ProcessSpellChanges(AChanges.ChangeActions);

  if (ADocumentLayoutResetType = TdxDocumentLayoutResetType.AllPrimaryLayout) and not ADebugSuppressControllerReset then
  begin
    ResetPages;
    FormattingController.Reset(False);
    Formatter.UpdateSecondaryPositions(AFrom, ATo);
    Formatter.NotifyDocumentChanged(AFrom, ATo, ADocumentLayoutResetType);
    Exit(TdxDocumentModelPosition.Create(APieceTable));
  end;
  if ADocumentLayoutResetType = TdxDocumentLayoutResetType.PrimaryLayoutFormPosition then
  begin
    APieceTable.ResetParagraphs(AFrom.ParagraphIndex, ATo.ParagraphIndex);

    ResetPages;

    UnsubscribePageFormattingComplete;
    try
      ASecondaryFrom := FormattingController.ResetFrom(AFrom, False);
    finally
      SubscribePageFormattingComplete;
    end;
    Formatter.UpdateSecondaryPositions(ASecondaryFrom, ATo);
    Formatter.NotifyDocumentChanged(AFrom, ATo, ADocumentLayoutResetType);

    GeneratePages;
  end
  else
  begin
    ASecondaryFrom := AFrom;
    Formatter.UpdateSecondaryPositions(ASecondaryFrom, ATo);
    Formatter.NotifyDocumentChanged(AFrom, ATo, ADocumentLayoutResetType);
  end;
  Result := ASecondaryFrom;
end;

class function TdxDocumentChangesHandler.SetNewPosition(const APos: TdxDocumentModelPosition; AParagraph: TdxParagraphBase): TdxDocumentModelPosition;
begin
  Result := APos;
  Result.RunIndex := AParagraph.FirstRunIndex;
  Result.ParagraphIndex := AParagraph.Index;
  Result.LogPosition := AParagraph.LogPosition;
  Result.RunStartLogPosition := AParagraph.LogPosition;
end;

class function TdxDocumentChangesHandler.CalculateResetFromPosition(AFormattingController: TdxDocumentFormattingController;
  APieceTable: TdxCustomPieceTable; AChanges: TdxDocumentModelDeferredChanges; AResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
var
  AChangeStart, AModelPosition: TdxDocumentModelPosition;
  AParagraphIndex: TdxParagraphIndex;
  AState: TdxTablesControllerTableState;
  ATable: TdxTable;
begin
  AChangeStart := AChanges.ChangeStart;
  if AResetType = TdxDocumentLayoutResetType.PrimaryLayoutFormPosition then
  begin
    AParagraphIndex := AChangeStart.ParagraphIndex;
    if TdxSimplePieceTable(APieceTable).Paragraphs[AParagraphIndex].ContextualSpacing and (AParagraphIndex > 0) then
      Dec(AParagraphIndex, 1);
    if AFormattingController.RowsController.TablesController.IsInsideTable then
    begin
      AState := Safe<TdxTablesControllerTableState>.Cast(AFormattingController.RowsController.TablesController.State);
      ATable := AState.TableViewInfoManager.TopLevelTableViewInfoManager.ColumnController.CurrentCell.TableViewInfo.Table;
      if ATable.FirstRow.FirstCell.StartParagraphIndex < AParagraphIndex then
        AParagraphIndex := ATable.FirstRow.FirstCell.StartParagraphIndex;
    end;
    AModelPosition := TdxDocumentModelPosition.FromParagraphStart(APieceTable, AParagraphIndex);
    AModelPosition := EnsurePositionVisibleWhenHiddenTextNotShown(APieceTable.DocumentModel, AModelPosition);
    AModelPosition := EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(AModelPosition);
    Exit(EnsureTopLevelParagraph(AModelPosition));
  end;
  Result := AChangeStart;
end;

class function TdxDocumentChangesHandler.EnsurePositionVisibleWhenHiddenTextNotShown(ADocumentModel: TdxCustomDocumentModel;
  const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  APieceTable: TdxSimplePieceTable;
  AVisibleTextFilter: IdxVisibleTextFilter;
  AParagraphIndex: TdxParagraphIndex;
begin
  APieceTable := TdxSimplePieceTable(AModelPosition.PieceTable);
  AVisibleTextFilter := APieceTable.VisibleTextFilter;
  AParagraphIndex := AModelPosition.ParagraphIndex;
  while AParagraphIndex > 0 do
  begin
    if AVisibleTextFilter.IsRunVisible(APieceTable.Paragraphs[AParagraphIndex - 1].LastRunIndex) then
      Break;
    Dec(AParagraphIndex);
  end;

  if AParagraphIndex = AModelPosition.ParagraphIndex then
    Result := AModelPosition
  else
    Result := TdxDocumentModelPosition.FromParagraphStart(APieceTable, AParagraphIndex);
end;

class function TdxDocumentChangesHandler.EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(
  const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AParagraphs: TdxParagraphBaseCollection;
  ARuns: TdxRunCollection;
  AParagraph: TdxParagraphBase;
begin
  Result := AModelPosition;
  AParagraphs := Result.PieceTable.Paragraphs;
  ARuns := Result.PieceTable.Runs;
  AParagraph := AParagraphs[Result.ParagraphIndex];
  while (Result.RunIndex > 0) and (AParagraph.Length = 1) and (ARuns[Result.RunIndex] is TdxSectionRun) do
  begin
    Result.ParagraphIndex := Result.ParagraphIndex - 1;
    AParagraph := AParagraphs[Result.ParagraphIndex];
    Result.RunIndex := AParagraph.FirstRunIndex;
    Result.LogPosition := AParagraph.LogPosition;
    Result.RunStartLogPosition := Result.LogPosition;
  end;
end;

class function TdxDocumentChangesHandler.EnsureTopLevelParagraph(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AParagraphs: TdxParagraphCollection;
  AParagraphIndex, I: TdxParagraphIndex;
  ACell: TdxTableCell;
  AParagraph: TdxParagraph;
begin
  AParagraphs := TdxParagraphCollection(APos.PieceTable.Paragraphs);
  AParagraphIndex := APos.ParagraphIndex;
  ACell := AParagraphs[AParagraphIndex].GetCell;
  if ACell = nil then
  begin
    if AParagraphIndex <> 0 then
    begin
      for I := AParagraphIndex - 1 downto 0 do
      begin
        AParagraph := AParagraphs[I];
        if (AParagraph.FrameProperties <> nil) and not AParagraph.IsInCell then
          AParagraphIndex := I
        else
          Break;
      end;
      Exit(SetNewPosition(APos, AParagraphs[AParagraphIndex]));
    end;
    Exit(APos);
  end;

  while ACell.Table.ParentCell <> nil do
    ACell := ACell.Table.ParentCell;

  ACell := ACell.Table.FirstRow.FirstCell;
  AParagraphIndex := ACell.StartParagraphIndex;
  Result := SetNewPosition(APos, AParagraphs[AParagraphIndex]);
end;

{ TdxControlDocumentChangesHandler }

constructor TdxControlDocumentChangesHandler.Create(AView: TdxRichEditView);
begin
  inherited Create(AView.Formatter);
  FView := AView;
end;

function TdxControlDocumentChangesHandler.NotifyDocumentChanged(APieceTable: TdxCustomPieceTable;
  const AChanges: TdxDocumentModelDeferredChanges; ADebugSuppressControllerReset: Boolean;
  ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
begin
  if TdxDocumentModelChangeAction.ActivePieceTableChanged in AChanges.ChangeActions then
    View.OnActivePieceTableChanged;

  Result := inherited NotifyDocumentChanged(APieceTable, AChanges, ADebugSuppressControllerReset, ADocumentLayoutResetType);
end;

procedure TdxControlDocumentChangesHandler.ProcessSelectionChanges(AChangeActions: TdxDocumentModelChangeActions);
begin
  View.ProcessSelectionChanges(AChangeActions);
end;

procedure TdxControlDocumentChangesHandler.ProcessSpellChanges(AChangeActions: TdxDocumentModelChangeActions);
begin
  if TdxDocumentModelChangeAction.ResetUncheckedIntervals in AChangeActions then
    View.DocumentModel.ResetUncheckedSpellIntervals;
end;

procedure TdxControlDocumentChangesHandler.ResetPages;
begin
  View.ResetPages(TdxPageGenerationStrategyType.RunningHeight);
end;

procedure TdxControlDocumentChangesHandler.GeneratePages;
begin
  View.GeneratePages;
end;

procedure TdxControlDocumentChangesHandler.SubscribePageFormattingComplete;
begin
  View.SubscribePageFormattingComplete;
end;

procedure TdxControlDocumentChangesHandler.UnsubscribePageFormattingComplete;
begin
  View.UnsubscribePageFormattingComplete;
end;

{ TdxServerDocumentChangesHandler }

function TdxServerDocumentChangesHandler.NotifyDocumentChanged(APieceTable: TdxCustomPieceTable;
  const AChanges: TdxDocumentModelDeferredChanges; ADebugSuppressControllerReset: Boolean;
  ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
begin
  if ADocumentLayoutResetType = TdxDocumentLayoutResetType.None then
    Result.Invalidate
  else
    Result := inherited NotifyDocumentChanged(APieceTable, AChanges, ADebugSuppressControllerReset, ADocumentLayoutResetType);
end;

procedure TdxServerDocumentChangesHandler.ProcessSelectionChanges(AChangeActions: TdxDocumentModelChangeActions);
begin
end;

procedure TdxServerDocumentChangesHandler.ProcessSpellChanges(AChangeActions: TdxDocumentModelChangeActions);
begin
end;

procedure TdxServerDocumentChangesHandler.ResetPages;
begin
end;

procedure TdxServerDocumentChangesHandler.GeneratePages;
begin
end;

procedure TdxServerDocumentChangesHandler.SubscribePageFormattingComplete;
begin
end;

procedure TdxServerDocumentChangesHandler.UnsubscribePageFormattingComplete;
begin
end;

{ TdxDocumentModelAccessor }

constructor TdxDocumentModelAccessor.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

{ TdxDocumentContentChangedEventArgs }

constructor TdxDocumentContentChangedEventArgs.Create(ASuppressBindingNotifications: Boolean);
begin
  inherited Create;
  FSuppressBindingNotifications := ASuppressBindingNotifications;
end;

{ TdxExplicitBoxMeasurerProvider }

constructor TdxExplicitBoxMeasurerProvider.Create(AMeasurer: TdxBoxMeasurer);
begin
  inherited Create;
  Assert(AMeasurer <> nil);
  FMeasurer := AMeasurer;
end;

function TdxExplicitBoxMeasurerProvider.GetMeasurer: TdxBoxMeasurer;
begin
  Result := FMeasurer;
end;

{ TdxInnerRichEditDocumentServer }

constructor TdxInnerRichEditDocumentServer.Create(const AOwner: IdxInnerRichEditDocumentServerOwner; ADocumentModel: TdxDocumentModel);
begin
  Assert(AOwner <> nil);
  Assert(ADocumentModel <> nil);
  Create(AOwner, ADocumentModel.ScreenDpiX, ADocumentModel.ScreenDpiY);
  FExistingDocumentModel := ADocumentModel;
end;

constructor TdxInnerRichEditDocumentServer.Create(const AOwner: IdxInnerRichEditDocumentServerOwner; ADpiX: Single; ADpiY: Single);
begin
  Assert(AOwner <> nil);
  inherited Create;
  Pointer(FOwner) := Pointer(AOwner);
  FDpiX := ADpiX;
  FDpiY := ADpiY;
  FLayoutCalculationMode := GetDefaultLayoutCalculationMode;
end;

constructor TdxInnerRichEditDocumentServer.Create(const AOwner: IdxInnerRichEditDocumentServerOwner);
begin
  Create(AOwner, TdxDocumentModelDpi.DpiX, TdxDocumentModelDpi.DpiY);
end;

destructor TdxInnerRichEditDocumentServer.Destroy;
begin
  DisposeBackgroundFormatter;
  FreeAndNil(FDocumentLayout);
  FNativeDocument := nil;

  UnsubscribeOptionsEvents;
  FreeAndNil(FOptions);

  UnsubscribeDocumentModelEvents;
  FreeAndNil(FDocumentDeferredChanges);
  FreeAndNil(FDocumentDeferredChangesOnIdle);
  if FExistingDocumentModel <> FDocumentModel then
    FreeAndNil(FDocumentModel);

  FreeAndNil(FDocumentModelAccessor);
  FreeAndNil(FMeasurementAndDrawingStrategy);
  FreeAndNil(FPredefinedFontSizeCollection);
  FreeAndNil(FBatchUpdateHelper);
  if FExistingDocumentModel <> FDocumentModelTemplate then
    FreeAndNil(FDocumentModelTemplate);
  inherited Destroy;
end;

procedure TdxInnerRichEditDocumentServer.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FNativeDocument <> nil then
    TdxNativeSubDocumentBase(FNativeDocument).Finalize;
end;

function TdxInnerRichEditDocumentServer.GetService<T>: T;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.GetService<T>
  else
    Result := Default(T);
end;

function TdxInnerRichEditDocumentServer.GetService(const AServiceType: TdxServiceType): IInterface;
begin
  if DocumentModel <> nil then
    Result := (DocumentModel as IdxServiceProvider).GetService(AServiceType)
  else
    Result := nil;
end;

procedure TdxInnerRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).AddService(AServiceType, AServiceInstance);
end;

procedure TdxInnerRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).AddService(AServiceType, AServiceInstance, APromote);
end;

procedure TdxInnerRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).AddService(AServiceType, ACallback);
end;

procedure TdxInnerRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).AddService(AServiceType, ACallback, APromote);
end;

procedure TdxInnerRichEditDocumentServer.RemoveService(const AServiceType: TdxServiceType);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).RemoveService(AServiceType);
end;

procedure TdxInnerRichEditDocumentServer.RemoveService(const AServiceType: TdxServiceType; APromote: Boolean);
begin
  if DocumentModel <> nil then
    (DocumentModel as IdxServiceContainer).RemoveService(AServiceType, APromote);
end;

function TdxInnerRichEditDocumentServer.GetFormattingController: TdxDocumentFormattingController;
begin
  Result := FFormattingController;
end;

function TdxInnerRichEditDocumentServer.GetModelDocumentLayout: TdxDocumentLayout;
begin
  Result := FModelDocumentLayout;
end;

function TdxInnerRichEditDocumentServer.GetControl: TWinControl;
begin
  Result := Owner.Control;
end;

function TdxInnerRichEditDocumentServer.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxInnerRichEditDocumentServer.GetDpiX: Single;
begin
  Result := FDpiX;
end;

function TdxInnerRichEditDocumentServer.GetDpiY: Single;
begin
  Result := FDpiY;
end;

function TdxInnerRichEditDocumentServer.GetMeasurementUnit: TdxMeasurementUnit;
begin
  Result := FMeasurementUnit;
end;

function TdxInnerRichEditDocumentServer.GetDocumentModelIntf: TdxDocumentModel;
begin
  Result := FDocumentModel;
end;

procedure TdxInnerRichEditDocumentServer.AddAfterExportHandler(const AHandler: TNotifyEvent);
begin
  FOnAfterExport.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveAfterExportHandler(const AHandler: TNotifyEvent);
begin
  FOnAfterExport.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddSelectionChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnSelectionChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveSelectionChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnSelectionChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddDocumentLoadedHandler(const AHandler: TNotifyEvent);
begin
  FOnDocumentLoaded.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveDocumentLoadedHandler(const AHandler: TNotifyEvent);
begin
  FOnDocumentLoaded.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
begin
  FOnEmptyDocumentCreated.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
begin
  FOnEmptyDocumentCreated.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddDocumentClosingHandler(const AHandler: TCloseQueryEvent);
begin
  FOnDocumentClosing.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveDocumentClosingHandler(const AHandler: TCloseQueryEvent);
begin
  FOnDocumentClosing.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddContentChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnContentChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveContentChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnContentChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddModifiedChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnModifiedChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveModifiedChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnModifiedChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddUnitChangingHandler(const AHandler: TNotifyEvent);
begin
  FOnUnitChanging.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveUnitChangingHandler(const AHandler: TNotifyEvent);
begin
  FOnUnitChanging.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddUnitChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnUnitChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveUnitChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnUnitChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
begin
  FOnCalculateDocumentVariable.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
begin
  FOnCalculateDocumentVariable.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
begin
  FOnBeforeImport.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
begin
  FOnBeforeImport.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
begin
  FOnBeforeExport.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
begin
  FOnBeforeExport.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddInitializeDocumentHandler(const AHandler: TdxEvent);
begin
  FOnInitializeDocument.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveInitializeDocumentHandler(const AHandler: TdxEvent);
begin
  FOnInitializeDocument.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddRtfTextChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnRtfTextChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveRtfTextChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnRtfTextChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.AddHtmlTextChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnHtmlTextChanged.Add(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.RemoveHtmlTextChangedHandler(const AHandler: TNotifyEvent);
begin
  FOnHtmlTextChanged.Remove(AHandler);
end;

procedure TdxInnerRichEditDocumentServer.SetLayoutCalculationMode(const AValue: TdxCalculationModeType);
begin
  if FLayoutCalculationMode <> AValue then
  begin
    FLayoutCalculationMode := AValue;
    OnLayoutCalculationModeChanged;
  end;
end;

function TdxInnerRichEditDocumentServer.GetDocument: IdxRichEditDocument;
begin
  Result := NativeDocument;
end;

procedure TdxInnerRichEditDocumentServer.OnLayoutCalculationModeChanged;
begin
  if FLayoutCalculationMode = TdxCalculationModeType.Automatic then
    InitializeBackgroundFormatter
  else
    DisposeBackgroundFormatter;
end;

function TdxInnerRichEditDocumentServer.GetDefaultLayoutCalculationMode: TdxCalculationModeType;
begin
  Result := TdxCalculationModeType.Manual;
end;

procedure TdxInnerRichEditDocumentServer.InitializeBackgroundFormatter;
begin
  FGraphics := TdxGraphics.Create;
  FMeasurer := TdxGdiBoxMeasurer.Create(DocumentModel, FGraphics);
  FModelDocumentLayout := TdxDocumentLayout.Create(DocumentModel, TdxExplicitBoxMeasurerProvider.Create(FMeasurer));
  FFormattingController := TdxPrintLayoutViewDocumentFormattingController.Create(ModelDocumentLayout, DocumentModel.MainPieceTable);
  FFormatter := TdxBackgroundFormatter.Create(FFormattingController, TdxCommentPadding.GetDefaultCommentPadding(DocumentModel));
  FFormatter.PageFormattingComplete.Add(OnPageFormattingComplete);
  FFormatter.Start;
end;

procedure TdxInnerRichEditDocumentServer.InitializeEmptyDocumentModel(ADocumentModel: TdxDocumentModel);
begin
end;

function TdxInnerRichEditDocumentServer.CalculatePrintDocumentLayout(
  Server: TObject{TdxInternalRichEditDocumentServer}): TdxDocumentLayout;
var
  AServer: TdxInternalRichEditDocumentServer absolute Server;
begin
  Assert(Server is TdxInternalRichEditDocumentServer);
  if ModelDocumentLayout <> nil then
    Exit(ModelDocumentLayout);
  NotImplemented;
  Result := ModelDocumentLayout;
end;

procedure TdxInnerRichEditDocumentServer.OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
var
  AArgs: TdxPageFormattedEventArgs;
begin
  AArgs := TdxPageFormattedEventArgs.Create(E.Page.PageIndex);
  try
    RaisePageFormatted(AArgs);
  finally
    AArgs.Free;
  end;
  if E.DocumentFormattingComplete then
    RaiseDocumentFormatted;
end;

procedure TdxInnerRichEditDocumentServer.DisposeBackgroundFormatter;
begin
  if FFormatter <> nil then
  begin
    FFormatter.PageFormattingComplete.Remove(OnPageFormattingComplete);
    FFormatter.Free;
    FFormatter := nil;
  end;
  FFormattingController := nil;
  FModelDocumentLayout := nil;
  if FMeasurer <> nil then
  begin
    FMeasurer.Free;
    FMeasurer := nil;
  end;
  if FGraphics <> nil then
  begin
    FGraphics.Free;
    FGraphics := nil;
  end;
end;

procedure TdxInnerRichEditDocumentServer.OnDocumentLayoutChanged(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
var
  ADocumentLayoutResetType: TdxDocumentLayoutResetType;
  AChangeStart, AInvalidateFrom: TdxDocumentModelPosition;
  AChangedPageIndex: Integer;
  AArgs: TdxDocumentLayoutInvalidatedEventArgs;
begin
  if ModelDocumentLayout = nil then
    Exit;
  ADocumentLayoutResetType := CalculateDocumentLayoutResetType(E.DeferredChanges);
  if LayoutCalculationMode = TdxCalculationModeType.Automatic then
  begin

    AChangeStart := E.DeferredChanges.ChangeStart;

    AInvalidateFrom := NotifyDocumentLayoutChanged(AChangeStart.PieceTable, E.DeferredChanges, ADocumentLayoutResetType);

    AChangedPageIndex := ModelDocumentLayout.Pages.BinarySearchBoxIndex(AChangeStart.PieceTable, AInvalidateFrom.LogPosition);
    if AChangedPageIndex < 0 then
      AChangedPageIndex := not AChangedPageIndex;
    if ADocumentLayoutResetType <> TdxDocumentLayoutResetType.None then
    begin
      AArgs := TdxDocumentLayoutInvalidatedEventArgs.Create(AChangedPageIndex);
      try
        RaiseDocumentLayoutInvalidated(AArgs);
      finally
        AArgs.Free;
      end;
    end;
  end
  else
  begin
    if ADocumentLayoutResetType <> TdxDocumentLayoutResetType.None then
    begin
      FModelDocumentLayout := nil;
      AArgs := TdxDocumentLayoutInvalidatedEventArgs.Create(0);
      try
        RaiseDocumentLayoutInvalidated(AArgs);
      finally
        AArgs.Free;
      end;
    end;
  end;
end;

function TdxInnerRichEditDocumentServer.NotifyDocumentLayoutChanged(APieceTable: TdxCustomPieceTable;
  const AChanges: TdxDocumentModelDeferredChanges; ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
var
  AHandler: TdxServerDocumentChangesHandler;
begin
  AHandler := TdxServerDocumentChangesHandler.Create(BackgroundFormatter);
  try
    Result := AHandler.NotifyDocumentChanged(APieceTable, AChanges, False, ADocumentLayoutResetType);
  finally
    AHandler.Free;
  end;
end;

class function TdxInnerRichEditDocumentServer.CalculateDocumentLayoutResetType(
  AChanges: TdxDocumentModelDeferredChanges): TdxDocumentLayoutResetType;
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := AChanges.ChangeActions;
  if TdxDocumentModelChangeAction.ResetAllPrimaryLayout in AChangeActions then
    Exit(TdxDocumentLayoutResetType.AllPrimaryLayout);
  if TdxDocumentModelChangeAction.ResetPrimaryLayout in AChangeActions then
    Exit(TdxDocumentLayoutResetType.PrimaryLayoutFormPosition);

  if TdxDocumentModelChangeAction.ResetSecondaryLayout in AChangeActions then
    Result := TdxDocumentLayoutResetType.PrimaryLayoutFormPosition
  else
    Result := TdxDocumentLayoutResetType.None;
end;

procedure TdxInnerRichEditDocumentServer.RaiseDocumentLayoutInvalidated(AArgs: TdxDocumentLayoutInvalidatedEventArgs);
begin
  if not FDocumentLayoutInvalidated.Empty then
    FDocumentLayoutInvalidated.Invoke(Self, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseDocumentFormatted;
begin
  if not FDocumentFormatted.Empty then
    FDocumentFormatted.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxInnerRichEditDocumentServer.RaisePageFormatted(AArgs: TdxPageFormattedEventArgs);
begin
  if not FPageFormatted.Empty then
    FPageFormatted.Invoke(Self, AArgs);
end;

function TdxInnerRichEditDocumentServer.GetDocumentModelTemplate: TdxDocumentModel;
begin
  if FDocumentModelTemplate = nil then
    CreateDocumentModelTemplate;
  Result := FDocumentModelTemplate;
end;

function TdxInnerRichEditDocumentServer.GetNumberingListsTemplate: TdxAbstractNumberingListCollection;
begin
  Result := DocumentModelTemplate.AbstractNumberingLists;
end;

function TdxInnerRichEditDocumentServer.GetMeasurer: TdxBoxMeasurer;
begin
  if MeasurementAndDrawingStrategy <> nil then
    Result := MeasurementAndDrawingStrategy.Measurer
  else
    Result := nil;
end;

function TdxInnerRichEditDocumentServer.GetLayoutUnit: TdxDocumentLayoutUnit;
begin
  Result := TdxDocumentLayoutUnit(DocumentModel.LayoutUnit);
end;

procedure TdxInnerRichEditDocumentServer.SetLayoutUnitProperty(const AValue: TdxDocumentLayoutUnit);
begin
  if AValue = LayoutUnit then
    Exit;
  SetLayoutUnit(AValue);
end;

function TdxInnerRichEditDocumentServer.GetModified: Boolean;
begin
  Result := DocumentModel.InternalAPI.Modified;
end;

procedure TdxInnerRichEditDocumentServer.SetModified(AValue: Boolean);
begin
  DocumentModel.InternalAPI.Modified := AValue;
end;

function TdxInnerRichEditDocumentServer.GetReadOnly: Boolean;
begin
  Result := False;
end;

procedure TdxInnerRichEditDocumentServer.SetReadOnly(const AValue: Boolean);
begin
end;

function TdxInnerRichEditDocumentServer.GetEnabled: Boolean;
begin
  Result := True;
end;

function TdxInnerRichEditDocumentServer.CreateNativeDocument: IdxRichEditDocument;
begin
  Result := nil;
end;

function TdxInnerRichEditDocumentServer.CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := nil;
end;

function TdxInnerRichEditDocumentServer.GetIsDocumentProtected: Boolean;
begin
  Result := DocumentModel.ProtectionProperties.EnforceProtection;
end;

function TdxInnerRichEditDocumentServer.GetIsEditable: Boolean;
begin
  Result := not ReadOnly and Enabled;
end;

procedure TdxInnerRichEditDocumentServer.SetMeasurementUnitProperty(AValue: TdxMeasurementUnit);
begin
  if FMeasurementUnit = AValue then
    Exit;
  SetMeasurementUnit(AValue);
end;

function TdxInnerRichEditDocumentServer.GetUIUnit: TdxMeasurementUnit;
begin
  if MeasurementUnit = TdxMeasurementUnit.Document then
    Result := TdxMeasurementUnit.Inch
  else
    Result := MeasurementUnit;
end;

function TdxInnerRichEditDocumentServer.GetNativeDocument: IdxRichEditDocument;
begin
  if FNativeDocument = nil then
    FNativeDocument := CreateNativeDocument;
  Result := FNativeDocument;
end;

function TdxInnerRichEditDocumentServer.GetActualReadOnly: Boolean;
begin
  Result := False;
end;

function TdxInnerRichEditDocumentServer.GetText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.Text
  else
    Result := '';
end;

procedure TdxInnerRichEditDocumentServer.SetText(const AValue: string);
begin
  if DocumentModel <> nil then
    DocumentModel.InternalAPI.Text := AValue;
end;

function TdxInnerRichEditDocumentServer.GetRtfText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.RtfText
  else
    Result := '';
end;

procedure TdxInnerRichEditDocumentServer.SetRtfText(const AValue: string);
begin
  if DocumentModel <> nil then
    DocumentModel.InternalAPI.RtfText := AValue;
end;

function TdxInnerRichEditDocumentServer.GetHtmlText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.HtmlText
  else
    Result := '';
end;

procedure TdxInnerRichEditDocumentServer.SetHtmlText(const AValue: string);
begin
  if DocumentModel <> nil then
    DocumentModel.InternalAPI.HtmlText := AValue;
end;


function TdxInnerRichEditDocumentServer.GetOwner: IdxInnerRichEditDocumentServerOwner;
begin
  Result := FOwner;
end;

function TdxInnerRichEditDocumentServer.GetDocumentLayout: TdxRichEditDocumentLayout;
begin
  InitializeDocumentLayout;
  Result := FDocumentLayout;
end;

procedure TdxInnerRichEditDocumentServer.InitializeDocumentLayout;
begin
  if FDocumentLayout = nil then
    FDocumentLayout := CreateDocumentLayout;
end;

function TdxInnerRichEditDocumentServer.CreateDocumentLayout: TdxRichEditDocumentLayout;
var
  ALayoutProvider: IdxRichEditDocumentLayoutProvider;
begin
  if Supports(Owner, IdxRichEditDocumentLayoutProvider, ALayoutProvider) then
    Result := TdxRichEditDocumentLayout.Create(ALayoutProvider)
  else
    Result := nil;
end;

procedure TdxInnerRichEditDocumentServer.RaiseContentChanged(ASuppressBindingNotifications: Boolean);
var
  AArgs: TdxDocumentContentChangedEventArgs;
begin
  if FOnContentChanged.Empty then
    Exit;
  AArgs := TdxDocumentContentChangedEventArgs.Create(ASuppressBindingNotifications);
  try
    FOnContentChanged.Invoke(Owner.Control);
  finally
    AArgs.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.RaisePlainTextChanged;
begin
  if not FOnPlainTextChanged.Empty then
    FOnPlainTextChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseRtfTextChanged;
begin
  if not FOnRtfTextChanged.Empty then
    FOnRtfTextChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseHtmlTextChanged;
begin
  if not FOnHtmlTextChanged.Empty then
    FOnHtmlTextChanged.Invoke(Owner.Control);
end;


procedure TdxInnerRichEditDocumentServer.RaiseOpenXmlBytesChanged;
begin
  if not FOnOpenXmlBytesChanged.Empty then
    FOnOpenXmlBytesChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseModifiedChanged;
begin
  if not FOnModifiedChanged.Empty then
    FOnModifiedChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseUnitChanging;
begin
  if not FOnUnitChanging.Empty then
    FOnUnitChanging.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseUnitChanged;
begin
  if not FOnUnitChanged.Empty then
    FOnUnitChanged.Invoke(Owner.Control);
end;

function TdxInnerRichEditDocumentServer.RaiseCalculateDocumentVariable(AArgs: TdxCalculateDocumentVariableEventArgs): Boolean;
begin
  if not FOnCalculateDocumentVariable.Empty then
  begin
    FOnCalculateDocumentVariable.Invoke(Owner.Control, AArgs);
    Result := AArgs.Handled;
  end
  else
    Result := False;
end;

procedure TdxInnerRichEditDocumentServer.RaiseBeforeImport(AArgs: TdxBeforeImportEventArgs);
begin
  if not FOnBeforeImport.Empty then
    FOnBeforeImport.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseBeforeExport(AArgs: TdxBeforeExportEventArgs);
begin
  if not FOnBeforeExport.Empty then
    FOnBeforeExport.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseAfterExport;
begin
  if not FOnAfterExport.Empty then
    FOnAfterExport.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseInitializeDocument(AArgs: TdxEventArgs);
begin
  if not FOnInitializeDocument.Empty then
    FOnInitializeDocument.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseInvalidFormatException(E: Exception);
var
  Args: TdxRichEditInvalidFormatExceptionEventArgs;
begin
  if FOnInvalidFormatException.Empty then
    Exit;
  Args := TdxRichEditInvalidFormatExceptionEventArgs.Create(E);
  try
    FOnInvalidFormatException.Invoke(Owner.Control, Args);
  finally
    Args.Free;
  end;
end;

function TdxInnerRichEditDocumentServer.RaiseUnhandledException(E: Exception): Boolean;
var
  AArgs: TdxRichEditUnhandledExceptionEventArgs;
begin
  try
    if not FOnUnhandledException.Empty then
    begin
      AArgs := TdxRichEditUnhandledExceptionEventArgs.Create(E);
      try
        FOnUnhandledException.Invoke(Owner.Control, AArgs);
        Result := AArgs.Handled;
      finally
        AArgs.Free;
      end;
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

procedure TdxInnerRichEditDocumentServer.RaiseSelectionChanged;
begin
  if not FOnSelectionChanged.Empty then
    FOnSelectionChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseStartHeaderFooterEditing;
var
  AArgs: TdxHeaderFooterEditingEventArgs;
begin
  if FOnStartHeaderFooterEditing.Empty then
    Exit;
  AArgs := TdxHeaderFooterEditingEventArgs.Create;
  try
    FOnStartHeaderFooterEditing.Invoke(Owner.Control, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.RaiseFinishHeaderFooterEditing;
var
  AArgs: TdxHeaderFooterEditingEventArgs;
begin
  if FOnFinishHeaderFooterEditing.Empty then
    Exit;
  AArgs := TdxHeaderFooterEditingEventArgs.Create;
  try
    FOnFinishHeaderFooterEditing.Invoke(Owner.Control, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.RaiseDocumentProtectionChanged;
begin
  if not FOnDocumentProtectionChanged.Empty then
    FOnDocumentProtectionChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseActiveViewChanged;
begin
  if not FOnActiveViewChanged.Empty then
    FOnActiveViewChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseDocumentLoaded;
begin
  if not FOnDocumentLoaded.Empty then
    FOnDocumentLoaded.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseEmptyDocumentCreated;
begin
  if not FOnEmptyDocumentCreated.Empty then
    FOnEmptyDocumentCreated.Invoke(Owner.Control);
end;


procedure TdxInnerRichEditDocumentServer.RaiseUpdateUI;
begin
  if not FOnUpdateUI.Empty then
    FOnUpdateUI.Invoke(Owner.Control);
end;

function TdxInnerRichEditDocumentServer.RaiseDocumentClosing: Boolean;
var
  ACanClose: Boolean;
begin
  if not FOnDocumentClosing.Empty then
  begin
    ACanClose := True;
    FOnDocumentClosing.Invoke(Owner.Control, ACanClose);
    Result := ACanClose;
  end
  else
    Result := True;
end;

function TdxInnerRichEditDocumentServer.RaiseCustomizeMergeFields(AArgs: TdxCustomizeMergeFieldsEventArgs): TArray<IdxRichEditMergeFieldName>;
begin
  if not FOnCustomizeMergeFields.Empty then
    FOnCustomizeMergeFields.Invoke(Owner.Control, AArgs);
  Result := AArgs.MergeFieldsNames;
end;

procedure TdxInnerRichEditDocumentServer.RaiseMailMergeStarted(AArgs: TdxMailMergeStartedEventArgs);
begin
  if not FOnMailMergeStarted.Empty then
    FOnMailMergeStarted.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseMailMergeRecordStarted(AArgs: TdxMailMergeRecordStartedEventArgs);
begin
  if not FOnMailMergeRecordStarted.Empty then
    FOnMailMergeRecordStarted.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseMailMergeRecordFinished(AArgs: TdxMailMergeRecordFinishedEventArgs);
begin
  if not FOnMailMergeRecordFinished.Empty then
    FOnMailMergeRecordFinished.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseMailMergeFinished(AArgs: TdxMailMergeFinishedEventArgs);
begin
  if not FOnMailMergeFinished.Empty then
    FOnMailMergeFinished.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaiseMailMergeGetTargetDocument(AArgs: TdxMailMergeGetTargetDocumentEventArgs);
begin
  if not FOnMailMergeGetTargetDocument.Empty then
    FOnMailMergeGetTargetDocument.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.RaisePageBackgroundChanged;
begin
  if not FOnPageBackgroundChanged.Empty then
    FOnPageBackgroundChanged.Invoke(Owner.Control);
end;

procedure TdxInnerRichEditDocumentServer.RaiseBeforePagePaint(AArgs: TdxRichEditBeforePagePaintEventArgs);
begin
  if not FOnBeforePagePaint.Empty then
    FOnBeforePagePaint.Invoke(Owner.Control, AArgs);
end;

procedure TdxInnerRichEditDocumentServer.BeginInitialize;
begin
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
  FThreadId := GetCurrentThreadId;
  FPredefinedFontSizeCollection := TdxPredefinedFontSizeCollection.Create;

  FDocumentModel := GetDocumentModel;
  FDocumentModelAccessor := TdxDocumentModelAccessor.Create(FDocumentModel);

  SubscribeDocumentModelEvents;

  FOptions := CreateOptions;
end;

procedure TdxInnerRichEditDocumentServer.EndInitialize;
begin
  UnsubscribeOptionsEvents;
  SubscribeOptionsEvents;
  CreateNewMeasurementAndDrawingStrategy;
  AddServices;
end;

procedure TdxInnerRichEditDocumentServer.AddServices;
begin
  AddService(IdxDocumentLayoutService, TdxDocumentServerLayoutService.Create(Self));
end;

procedure TdxInnerRichEditDocumentServer.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxInnerRichEditDocumentServer.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxInnerRichEditDocumentServer.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

function TdxInnerRichEditDocumentServer.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxInnerRichEditDocumentServer.GetDocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler;
begin
  Result := FDocumentLayoutInvalidated.Clone;
end;

function TdxInnerRichEditDocumentServer.GetDocumentFormatted: TdxEventHandler;
begin
  Result := FDocumentFormatted.Clone;
end;

function TdxInnerRichEditDocumentServer.GetPageFormatted: TdxPageFormattedEventHandler;
begin
  Result := FPageFormatted.Clone;
end;

procedure TdxInnerRichEditDocumentServer.OnFirstBeginUpdate;
begin
  OnFirstBeginUpdateCore;
end;

procedure TdxInnerRichEditDocumentServer.OnBeginUpdate;
begin
end;

procedure TdxInnerRichEditDocumentServer.OnEndUpdate;
begin
end;

procedure TdxInnerRichEditDocumentServer.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxInnerRichEditDocumentServer.OnCancelUpdate;
begin
end;

procedure TdxInnerRichEditDocumentServer.OnLastCancelUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxInnerRichEditDocumentServer.CreateDocumentModelTemplate;
begin
  FDocumentModelTemplate := GetDocumentModel;

  FDocumentModelTemplate.BeginSetContent;
  try
    TdxDefaultNumberingListHelper.InsertNumberingLists(FDocumentModelTemplate, DocumentModel.UnitConverter,
      DocumentModel.DocumentProperties.DefaultTabWidth);
  finally
    FDocumentModelTemplate.EndSetContent(TdxDocumentModelChangeType.None, False, nil);
  end;
end;

procedure TdxInnerRichEditDocumentServer.OnFirstBeginUpdateCore;
begin
end;

procedure TdxInnerRichEditDocumentServer.OnLastEndUpdateCore;
begin
end;

function TdxInnerRichEditDocumentServer.GetDocumentModel: TdxDocumentModel;
begin
  if FExistingDocumentModel <> nil then
    Exit(FExistingDocumentModel);
  Result := CreateDocumentModelCore;
end;

function TdxInnerRichEditDocumentServer.CreateDocumentModelCore: TdxDocumentModel;
begin
  Result := TdxDocumentModel.Create;
end;

procedure TdxInnerRichEditDocumentServer.SubscribeDocumentModelEvents;
begin
  DocumentModel.BeginDocumentUpdate.Add(OnBeginDocumentUpdate);
  DocumentModel.EndDocumentUpdate.Add(OnEndDocumentUpdate);
  DocumentModel.InnerSelectionChanged.Add(OnInnerSelectionChanged);
  DocumentModel.SelectionChanged.Add(OnSelectionChanged);
  DocumentModel.InnerContentChanged.Add(OnInnerContentChanged);
  DocumentModel.ContentChanged.Add(OnContentChanged);
  DocumentModel.ModifiedChanged.Add(OnModifiedChanged);
  DocumentModel.BeforeExport.Add(OnBeforeExport);
  DocumentModel.AfterExport.Add(OnAfterExport);
  DocumentModel.BeforeImport.Add(OnBeforeImport);
  DocumentModel.DocumentCleared.Add(OnDocumentCleared);
  DocumentModel.InvalidFormatException.Add(OnInvalidFormatException);
  DocumentModel.CalculateDocumentVariable.Add(OnCalculateDocumentVariable);
  DocumentModel.MailMergeStarted.Add(OnMailMergeStarted);
  DocumentModel.MailMergeRecordStarted.Add(OnMailMergeRecordStarted);
  DocumentModel.MailMergeRecordFinished.Add(OnMailMergeRecordFinished);
  DocumentModel.MailMergeFinished.Add(OnMailMergeFinished);
  DocumentModel.MailMergeGetTargetDocument.Add(OnMailMergeGetTargetDocument);

  DocumentModel.PageBackgroundChanged.Add(OnPageBackgroundChanged);

  DocumentModel.OnEncryptionPasswordQuery := GetEncryptionPassword;
end;



procedure TdxInnerRichEditDocumentServer.UnsubscribeDocumentModelEvents;
begin
  DocumentModel.BeginDocumentUpdate.Remove(OnBeginDocumentUpdate);
  DocumentModel.EndDocumentUpdate.Remove(OnEndDocumentUpdate);
  DocumentModel.InnerSelectionChanged.Remove(OnInnerSelectionChanged);
  DocumentModel.SelectionChanged.Remove(OnSelectionChanged);
  DocumentModel.InnerContentChanged.Remove(OnInnerContentChanged);
  DocumentModel.ContentChanged.Remove(OnContentChanged);
  DocumentModel.ModifiedChanged.Remove(OnModifiedChanged);
  DocumentModel.BeforeExport.Remove(OnBeforeExport);
  DocumentModel.AfterExport.Remove(OnAfterExport);
  DocumentModel.BeforeImport.Remove(OnBeforeImport);
  DocumentModel.DocumentCleared.Remove(OnDocumentCleared);
  DocumentModel.InvalidFormatException.Remove(OnInvalidFormatException);
  DocumentModel.CalculateDocumentVariable.Remove(OnCalculateDocumentVariable);
  DocumentModel.MailMergeStarted.Remove(OnMailMergeStarted);
  DocumentModel.MailMergeRecordStarted.Remove(OnMailMergeRecordStarted);
  DocumentModel.MailMergeRecordFinished.Remove(OnMailMergeRecordFinished);
  DocumentModel.MailMergeFinished.Remove(OnMailMergeFinished);
  DocumentModel.MailMergeGetTargetDocument.Remove(OnMailMergeGetTargetDocument);

  DocumentModel.PageBackgroundChanged.Remove(OnPageBackgroundChanged);
  DocumentModel.OnEncryptionPasswordQuery := nil;
end;

procedure TdxInnerRichEditDocumentServer.OnSelectionChanged(ASender: TObject);
begin
  RaiseSelectionChanged;
  OnUpdateUI;
end;

procedure TdxInnerRichEditDocumentServer.OnInnerSelectionChanged(ASender: TObject);
begin
end;

procedure TdxInnerRichEditDocumentServer.OnBeginDocumentUpdate(ASender: TObject; E: TdxEventArgs);
begin
  FDocumentDeferredChanges := TdxDocumentDeferredChanges.Create;
  BeginUpdate;

  OnBeginDocumentUpdateCore;
end;

procedure TdxInnerRichEditDocumentServer.OnEndDocumentUpdate(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
begin
  OnEndDocumentUpdateCore(ASender, E);
  EndUpdate;
  FreeAndNil(FDocumentDeferredChanges);
end;

procedure TdxInnerRichEditDocumentServer.OnBeginDocumentUpdateCore;
begin
end;

function TdxInnerRichEditDocumentServer.ProcessEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions;
begin
  Result := E.DeferredChanges.ChangeActions;
  if TdxDocumentModelChangeAction.PerformActionsOnIdle in Result then
  begin
    if FDocumentDeferredChangesOnIdle = nil then
      FDocumentDeferredChangesOnIdle := TdxDocumentDeferredChanges.Create;
    FDocumentDeferredChangesOnIdle.ChangeActions := FDocumentDeferredChangesOnIdle.ChangeActions + Result;
    FDocumentDeferredChangesOnIdle.ChangeActions := FDocumentDeferredChangesOnIdle.ChangeActions - [TdxDocumentModelChangeAction.PerformActionsOnIdle];
    FDocumentDeferredChangesOnIdle.StartRunIndex := Min(E.DeferredChanges.ChangeStart.RunIndex, FDocumentDeferredChangesOnIdle.StartRunIndex);
    FDocumentDeferredChangesOnIdle.EndRunIndex := Max(E.DeferredChanges.ChangeEnd.RunIndex, FDocumentDeferredChangesOnIdle.EndRunIndex);
    Result := [];
  end;

  if [TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated, TdxDocumentModelChangeAction.RaiseDocumentLoaded] * Result <> [] then
    ApplyFontAndForeColor;
end;

function TdxInnerRichEditDocumentServer.OnEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions;
begin
  Result := ProcessEndDocumentUpdateCore(ASender, E);
  OnDocumentLayoutChanged(Self, E);
  ApplyChangesCore(Result);
end;

procedure TdxInnerRichEditDocumentServer.OnInnerContentChanged(ASender: TObject; E: TdxEventArgs);
begin
end;

procedure TdxInnerRichEditDocumentServer.OnContentChanged(ASender: TObject; E: TdxEventArgs);
begin
  OnContentChangedCore(False, False);
end;

procedure TdxInnerRichEditDocumentServer.OnContentChangedCore(ASuppressBindingNotifications: Boolean; ASuppressUpdateUI: Boolean);
begin
  RaiseContentChanged(ASuppressBindingNotifications);
  if not ASuppressBindingNotifications then
    RaiseBindingNotifications;
  if not ASuppressUpdateUI then
    OnUpdateUI;
end;

procedure TdxInnerRichEditDocumentServer.RaiseBindingNotifications;
begin
  RaiseRtfTextChanged;
  RaiseHtmlTextChanged;
  RaiseOpenXmlBytesChanged;
  RaisePlainTextChanged;
end;

procedure TdxInnerRichEditDocumentServer.OnModifiedChanged(ASender: TObject; E: TdxEventArgs);
begin
  RaiseModifiedChanged;
  OnUpdateUI;
end;

procedure TdxInnerRichEditDocumentServer.OnCalculateDocumentVariable(ASender: TObject; E: TdxCalculateDocumentVariableEventArgs);
begin
  RaiseCalculateDocumentVariable(E);
end;

procedure TdxInnerRichEditDocumentServer.OnMailMergeStarted(ASender: TObject; const E: TdxMailMergeStartedEventArgs);
begin
  RaiseMailMergeStarted(E);
end;

procedure TdxInnerRichEditDocumentServer.OnMailMergeRecordStarted(ASender: TObject; const E: TdxMailMergeRecordStartedEventArgs);
begin
  RaiseMailMergeRecordStarted(E);
end;

procedure TdxInnerRichEditDocumentServer.OnMailMergeRecordFinished(ASender: TObject; const E: TdxMailMergeRecordFinishedEventArgs);
begin
  RaiseMailMergeRecordFinished(E);
end;

procedure TdxInnerRichEditDocumentServer.OnMailMergeFinished(ASender: TObject; const E: TdxMailMergeFinishedEventArgs);
begin
  RaiseMailMergeFinished(E);
end;

procedure TdxInnerRichEditDocumentServer.OnMailMergeGetTargetDocument(ASender: TObject;
  const E: TdxMailMergeGetTargetDocumentEventArgs);
begin
  RaiseMailMergeGetTargetDocument(E);
end;

procedure TdxInnerRichEditDocumentServer.OnPageBackgroundChanged(ASender: TObject; E: TdxEventArgs);
begin
  RaisePageBackgroundChanged;
end;

procedure TdxInnerRichEditDocumentServer.OnAfterExport(Sender: TObject);
begin
  RaiseAfterExport;
end;

procedure TdxInnerRichEditDocumentServer.OnBeforeExport(ASender: TObject; E: TdxBeforeExportEventArgs);
begin
  RaiseBeforeExport(E);
end;

procedure TdxInnerRichEditDocumentServer.OnBeforeImport(ASender: TObject; E: TdxBeforeImportEventArgs);
begin
  RaiseBeforeImport(E);
end;

procedure TdxInnerRichEditDocumentServer.OnDocumentCleared(ASender: TObject; E: TdxEventArgs);
begin
  RaiseInitializeDocument(E);
end;

procedure TdxInnerRichEditDocumentServer.OnInvalidFormatException(ASender: TObject; E: TdxRichEditInvalidFormatExceptionEventArgs);
begin
  RaiseInvalidFormatException(E.Exception);
end;

procedure TdxInnerRichEditDocumentServer.OnEmptyDocumentCreated;
begin
  DocumentModel.SwitchToEmptyHistory(False);
  try
    ApplyFontAndForeColor;
    RaiseEmptyDocumentCreated;
  finally
    DocumentModel.SwitchToNormalHistory(False);
  end;
end;

procedure TdxInnerRichEditDocumentServer.OnDocumentLoaded;
begin
  DocumentModel.SwitchToEmptyHistory(False);
  try
    ApplyFontAndForeColor;
    RaiseDocumentLoaded;
  finally
    DocumentModel.SwitchToNormalHistory(False);
  end;
end;

procedure TdxInnerRichEditDocumentServer.OnActivePieceTableChanged(ASender: TObject; E: TdxEventArgs);
begin
  if DocumentModel.ActivePieceTable.IsHeaderFooter then
    RaiseStartHeaderFooterEditing
  else
    RaiseFinishHeaderFooterEditing;

  OnUpdateUI;
end;

procedure TdxInnerRichEditDocumentServer.ApplyFontAndForeColor;
begin
end;

procedure TdxInnerRichEditDocumentServer.ApplyChangesCore(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if DocumentModel.IsUpdateLocked then
    DocumentDeferredChanges.ChangeActions := DocumentDeferredChanges.ChangeActions + AChangeActions
  else
    RaiseDeferredEvents(AChangeActions);
end;

procedure TdxInnerRichEditDocumentServer.PerformRaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions);
var
  ASuppressBindingNotifications, ASuppressUpdateUI: Boolean;
begin
  if TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated in AChangeActions then
    OnEmptyDocumentCreated;

  if TdxDocumentModelChangeAction.RaiseDocumentLoaded in AChangeActions then
    OnDocumentLoaded;

  if TdxDocumentModelChangeAction.RaiseContentChanged in AChangeActions then
  begin
    ASuppressBindingNotifications := TdxDocumentModelChangeAction.SuppressBindingsNotifications in AChangeActions;
    ASuppressUpdateUI := TdxDocumentModelChangeAction.RaiseSelectionChanged in AChangeActions;
    OnContentChangedCore(ASuppressBindingNotifications, ASuppressUpdateUI);
  end;

  if TdxDocumentModelChangeAction.RaiseModifiedChanged in AChangeActions then
    OnModifiedChanged(Self, nil);
  if TdxDocumentModelChangeAction.ActivePieceTableChanged in AChangeActions then
    OnActivePieceTableChanged(Self, nil);
  if TdxDocumentModelChangeAction.RaiseSelectionChanged in AChangeActions then
    OnSelectionChanged(Self);
  if TdxDocumentModelChangeAction.RaiseDocumentProtectionChanged in AChangeActions then
    RaiseDocumentProtectionChanged;
end;

procedure TdxInnerRichEditDocumentServer.RaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions);
begin
  System.TMonitor.Enter(Self);
  try
    if IsDisposed then
      Exit;
    PerformRaiseDeferredEventsCore(AChangeActions);
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TdxInnerRichEditDocumentServer.RaiseDeferredEvents(const AChangeActions: TdxDocumentModelChangeActions);
begin
  Owner.RaiseDeferredEvents(AChangeActions);
end;

function TdxInnerRichEditDocumentServer.CreateMeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy;
begin
  Result := Owner.CreateMeasurementAndDrawingStrategy(DocumentModel);
end;

procedure TdxInnerRichEditDocumentServer.CreateNewMeasurementAndDrawingStrategy;
begin
  FMeasurementAndDrawingStrategy.Free;
  FMeasurementAndDrawingStrategy := CreateMeasurementAndDrawingStrategy;
  FMeasurementAndDrawingStrategy.Initialize;
end;

procedure TdxInnerRichEditDocumentServer.RecreateMeasurementAndDrawingStrategy;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout];
var
  APieceTable: TdxPieceTable;
begin
  BeginUpdate;
  try
    DocumentModel.BeginUpdate;
    try
      CreateNewMeasurementAndDrawingStrategy;
      APieceTable := DocumentModel.MainPieceTable;
      DocumentModel.ApplyChangesCore(APieceTable, ChangeActions, 0 , MaxInt);
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxInnerRichEditDocumentServer.CreateOptions: TdxRichEditControlOptionsBase;
begin
  Result := TdxRichEditControlOptionsBase(Owner.CreateOptions(Self));
end;

procedure TdxInnerRichEditDocumentServer.SubscribeOptionsEvents;
begin
  Options.Changed.Add(OnOptionsChanged);
end;

procedure TdxInnerRichEditDocumentServer.UnsubscribeOptionsEvents;
begin
  Options.Changed.Remove(OnOptionsChanged);
end;

procedure TdxInnerRichEditDocumentServer.OnOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  if [TdxRichEditOptionsAction.DataSource, TdxRichEditOptionsAction.ViewMergedData] * E.Actions <> [] then
    OnOptionsMailMergeChanged;
end;

procedure TdxInnerRichEditDocumentServer.OnOptionsMailMergeChanged;
var
  AOptions: TdxRichEditMailMergeOptions;
begin
  BeginUpdate;
  try
    DocumentModel.BeginUpdate;
    try
      AOptions := Options.MailMerge;
      DocumentModel.MailMergeProperties.ViewMergedData := AOptions.ViewMergedData;
      DocumentModel.MailMergeDataController.DataSource := AOptions.DataSource;
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxInnerRichEditDocumentServer.SetMeasurementUnit(AValue: TdxMeasurementUnit);
begin
  RaiseUnitChanging;
  FMeasurementUnit := AValue;
  TdxNativeSubDocumentBase(NativeDocument).UnitsChanged;
  RaiseUnitChanged;
end;

procedure TdxInnerRichEditDocumentServer.SetLayoutUnit(AUnit: TdxDocumentLayoutUnit);
begin
  BeginUpdate;
  try
    SetLayoutUnitCore(AUnit);
  finally
    EndUpdate;
  end;
end;

procedure TdxInnerRichEditDocumentServer.SetLayoutUnitCore(AUnit: TdxDocumentLayoutUnit);
begin
  SetDocumentModelLayoutUnit(AUnit);
end;

procedure TdxInnerRichEditDocumentServer.SetDocumentModelLayoutUnit(AUnit: TdxDocumentLayoutUnit);
begin
  DocumentModel.BeginUpdate;
  try
    SetDocumentModelLayoutUnitCore(AUnit);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxInnerRichEditDocumentServer.SetDocumentModelLayoutUnitCore(AUnit: TdxDocumentLayoutUnit);
begin
  DocumentModel.LayoutUnit := AUnit;
  RecreateMeasurementAndDrawingStrategy;
  MeasurementAndDrawingStrategy.OnLayoutUnitChanged;
end;

function TdxInnerRichEditDocumentServer.CanCloseExistingDocument: Boolean;
begin
  if CanCloseExistingDocumentCore then
    Exit(True);
  Result := RaiseDocumentClosing;
end;

function TdxInnerRichEditDocumentServer.CanCloseExistingDocumentCore: Boolean;
begin
  Result := not Modified;
end;

procedure TdxInnerRichEditDocumentServer.OnApplicationIdle;
begin
  if FDocumentDeferredChangesOnIdle <> nil then
  begin
    DocumentModel.BeginUpdate;
    try
      DocumentModel.MainPieceTable.ApplyChangesCore(FDocumentDeferredChangesOnIdle.ChangeActions,
        FDocumentDeferredChangesOnIdle.StartRunIndex, FDocumentDeferredChangesOnIdle.EndRunIndex);
    finally
      DocumentModel.EndUpdate;
    end;
    FreeAndNil(FDocumentDeferredChangesOnIdle);
  end;
end;

function TdxInnerRichEditDocumentServer.CreateNewDocument(ARaiseDocumentClosing: Boolean): Boolean;
begin
  if Modified then
    if ARaiseDocumentClosing and not RaiseDocumentClosing then
      Exit(False);
  DocumentModel.InternalAPI.CreateNewDocument;
  Result := True;
end;

procedure TdxInnerRichEditDocumentServer.LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  LoadDocumentCore(AStream, ADocumentFormat, '');
end;

procedure TdxInnerRichEditDocumentServer.LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  LoadDocumentCore(AStream, ADocumentFormat, '');
end;

procedure TdxInnerRichEditDocumentServer.LoadDocumentCore(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat;
  const ASourceUri: string);
begin
  DocumentModel.LoadDocument(AStream, ADocumentFormat, ASourceUri);
end;

procedure TdxInnerRichEditDocumentServer.LoadDocument(const AFileName: string);
begin
  LoadDocument(AFileName, TdxRichEditDocumentFormat.Undefined);
end;

procedure TdxInnerRichEditDocumentServer.LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  LoadDocument(AFileName, ADocumentFormat, False);
end;

procedure TdxInnerRichEditDocumentServer.LoadDocumentTemplate(const AFileName: string);
begin
  LoadDocumentTemplate(AFileName, TdxRichEditDocumentFormat.Undefined);
end;

procedure TdxInnerRichEditDocumentServer.LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  LoadDocument(AFileName, ADocumentFormat, True);
end;

procedure TdxInnerRichEditDocumentServer.LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat; ALoadAsTemplate: Boolean);
var
  AStream: TStream;
  ASaveOptions: TdxDocumentSaveOptions;
  AOriginPassword: string;
  APreviousFileName: string;
  APreviousDocumentFormat: TdxRichEditDocumentFormat;
begin
  if ADocumentFormat = TdxRichEditDocumentFormat.Undefined then
    ADocumentFormat := DocumentModel.AutodetectDocumentFormat(AFileName);

  ASaveOptions := DocumentModel.DocumentSaveOptions;
  APreviousFileName := ASaveOptions.CurrentFileName;
  APreviousDocumentFormat := ASaveOptions.CurrentFormat;
  AOriginPassword := DocumentModel.EncryptionProperties.Password;
  try
    DocumentModel.EncryptionProperties.Password := '';
    if not ALoadAsTemplate then
    begin
      ASaveOptions.CurrentFileName := AFileName;
      ASaveOptions.CurrentFormat := ADocumentFormat;
    end;
    AStream := TdxMemoryStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      LoadDocumentCore(AStream, ADocumentFormat, AFileName);
    finally
      AStream.Free;
    end;
  except
    ASaveOptions.CurrentFileName := APreviousFileName;
    ASaveOptions.CurrentFormat := APreviousDocumentFormat;
    DocumentModel.EncryptionProperties.Password := AOriginPassword;
    raise;
  end;
end;

procedure TdxInnerRichEditDocumentServer.SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  SaveDocumentCore(AStream, ADocumentFormat, '');
end;

procedure TdxInnerRichEditDocumentServer.SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  SaveDocumentCore(AFileName, ADocumentFormat, DocumentModel.DocumentSaveOptions);
end;

procedure TdxInnerRichEditDocumentServer.SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat;
  const AOptions: TdxDocumentSaveOptions);
begin
  SaveDocumentCore(AFileName, ADocumentFormat, AOptions);
end;

procedure TdxInnerRichEditDocumentServer.SaveDocumentCore(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat;
  const AOptions: TdxDocumentSaveOptions);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveDocumentCore(AStream, ADocumentFormat, AFileName);
    AOptions.CurrentFileName := AFileName;
    AOptions.CurrentFormat := ADocumentFormat;
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

function TdxInnerRichEditDocumentServer.GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean;
begin
  APassword := '';
  Result := False;
end;

procedure TdxInnerRichEditDocumentServer.SaveDocumentCore(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat;
  const ATargetUri: string);
begin
  DocumentModel.SaveDocument(AStream, ADocumentFormat, ATargetUri);
end;

function TdxInnerRichEditDocumentServer.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  Result := nil;
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const ADocument: IdxRichEditDocument);
var
  AOptions: IdxRichEditMailMergeOptions;
begin
  AOptions := CreateMailMergeOptions;
  MailMerge(AOptions, ADocument);
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument);
begin
  Assert(ATargetDocument <> nil);
  MailMerge(AOptions, TdxNativeSubDocumentBase(ATargetDocument).DocumentModel);
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  MailMerge(CreateMailMergeOptions, AFileName, AFormat);
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
var
  AOptions: IdxRichEditMailMergeOptions;
begin
  AOptions := CreateMailMergeOptions;
  MailMerge(AOptions, AStream, AFormat);
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat);
var
  AStream: TdxMemoryStream;
  AResultDocumentModel: TdxDocumentModel;
  AEventRouter: TdxCalculateDocumentVariableEventRouter;
begin
  AStream := TdxMemoryStream.Create;
  try
    AResultDocumentModel := DocumentModel.CreateNew;
    try
      AEventRouter := TdxCalculateDocumentVariableEventRouter.Create(DocumentModel);
      try
        AResultDocumentModel.CalculateDocumentVariable.Add(AEventRouter.OnCalculateDocumentVariable);
        try
          AResultDocumentModel.DocumentExportOptions.CopyFrom(DocumentModel.DocumentExportOptions);
          MailMerge(AOptions, AResultDocumentModel);
          SaveDocumentCore(AResultDocumentModel, AStream, AFormat, AFileName);
          AStream.SaveToFile(AFileName);
        finally
          AResultDocumentModel.CalculateDocumentVariable.Remove(AEventRouter.OnCalculateDocumentVariable);
        end;
      finally
        AEventRouter.Free;
      end;
    finally
      AResultDocumentModel.Free;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions;
  AStream: TStream; AFormat: TdxRichEditDocumentFormat);
var
  AResultDocumentModel: TdxDocumentModel;
  AEventRouter: TdxCalculateDocumentVariableEventRouter;
begin
  AResultDocumentModel := DocumentModel.CreateNew;
  try
    AEventRouter := TdxCalculateDocumentVariableEventRouter.Create(DocumentModel);
    try
      AResultDocumentModel.CalculateDocumentVariable.Add(AEventRouter.OnCalculateDocumentVariable);
      try
        AResultDocumentModel.DocumentExportOptions.CopyFrom(DocumentModel.DocumentExportOptions);
        MailMerge(AOptions, AResultDocumentModel);
        SaveDocumentCore(AResultDocumentModel, AStream, AFormat, '');
      finally
        AResultDocumentModel.CalculateDocumentVariable.Remove(AEventRouter.OnCalculateDocumentVariable);
      end;
    finally
      AEventRouter.Free;
    end;
  finally
    AResultDocumentModel.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions;
  ATargetModel: TdxDocumentModel);
var
  AMergeHelper: TdxMailMergeHelper;
  AInnerOptions: TdxMailMergeOptions;
  AHasDataSet: Boolean;
begin
  Assert(ATargetModel <> nil);
  Assert(AOptions <> nil);
  ATargetModel.BeginUpdate;
  try
    ATargetModel.InternalAPI.CreateNewDocument;
    ATargetModel.DocumentSaveOptions.CurrentFileName := DocumentModel.DocumentSaveOptions.CurrentFileName;
    AInnerOptions := GetMailMergeOptions(AOptions);
    try
      AHasDataSet := (AInnerOptions.DataSource <> nil) and (AInnerOptions.DataSource.DataSet <> nil);
      if AHasDataSet then
        AInnerOptions.DataSource.DataSet.DisableControls;
      try
        AMergeHelper := TdxMailMergeHelper.Create(FOwner, DocumentModel, AInnerOptions);
        try
          AMergeHelper.ExecuteMailMerge(ATargetModel);
        finally
          AMergeHelper.Free;
        end;
      finally
        if AHasDataSet then
          AInnerOptions.DataSource.DataSet.EnableControls;
      end;
    finally
      AInnerOptions.Free;
    end;
    ATargetModel.DocumentSaveOptions.CurrentFileName := '';
  finally
    ATargetModel.EndUpdate;
  end;
end;

function TdxInnerRichEditDocumentServer.GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions;
begin
  Result := nil;
end;

class procedure TdxInnerRichEditDocumentServer.SaveDocumentCore(ADocumentModel: TdxDocumentModel; AStream: TStream;
  AFormat: TdxRichEditDocumentFormat; const ATargetUri: string);
var
  AExportManagerService: IdxDocumentExportManagerService;
  AExportHelper: TdxExportHelper;
begin
  AExportManagerService := ADocumentModel.GetService<IdxDocumentExportManagerService>;
  if AExportManagerService = nil then
    TdxRichEditExceptions.ThrowInvalidOperationException('Could not find service: IdxDocumentExportManagerService');

  AExportHelper := ADocumentModel.CreateDocumentExportHelper(AFormat) as TdxExportHelper;
  try
    AExportHelper.Export(AStream, AFormat, ATargetUri, AExportManagerService);
  finally
    AExportHelper.Free;
  end;
end;

procedure TdxInnerRichEditDocumentServer.OnUpdateUI;
begin
end;

function TdxInnerRichEditDocumentServer.CreateCopySelectionManager: TdxCopySelectionManager;
begin
  Result := nil;
end;

{ TdxInnerRichEditControl }

constructor TdxInnerRichEditControl.Create(const AOwner: IdxInnerRichEditControlOwner; ADpiX: Single; ADpiY: Single);
begin
  inherited Create(AOwner, ADpiX, ADpiY);
  FGestureActivated := False;
  FCommandTable := TdxEnumeratedDictionary<TdxRichEditCommandId, TdxRichEditCommandClass>.Create;
  PopulateCommands;
  FMouseController := CreateMouseController;
  FKeyboardController := CreateKeyboardController;
  TdxSpellCheckerInstance.SpellCheckerChanged.Add(OnSpellCheckerChanged);
end;

constructor TdxInnerRichEditControl.Create(const AOwner: IdxInnerRichEditControlOwner);
begin
  Create(AOwner, TdxDocumentModelDpi.DpiX, TdxDocumentModelDpi.DpiY);
end;

destructor TdxInnerRichEditControl.Destroy;
begin
  TdxSpellCheckerInstance.SpellCheckerChanged.Remove(OnSpellCheckerChanged);
  DisposeBackgroundFormatter;
  DisposeViews;

  FreeAndNil(FBackgroundThreadUIUpdater);

  FreeAndNil(FKeyboardController);
  FreeAndNil(FMouseController);
  FreeAndNil(FCommandTable);

  FHorizontalScrollbar := nil;
  FVerticalScrollbar := nil;
  FHorizontalRuler := nil;
  FVerticalRuler := nil;
  inherited Destroy;
end;

function TdxInnerRichEditControl.CreateCopySelectionManager: TdxCopySelectionManager;
begin
  Result := TdxCopySelectionManager.Create(Self);
end;

function TdxInnerRichEditControl.CreateMouseCursorCalculator: TdxMouseCursorCalculator;
begin
  Result := TdxMouseCursorCalculator.Create(ActiveView);
end;

function TdxInnerRichEditControl.GetActiveView: TdxRichEditView;
begin
  Result := ActiveView;
end;

function TdxInnerRichEditControl.GetMouseController: TdxRichEditCustomMouseController;
begin
  Result := MouseController;
end;

function TdxInnerRichEditControl.GetOptions: TdxRichEditControlOptionsBase;
begin
  Result := inherited Options;
end;

function TdxInnerRichEditControl.GetPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
begin
  Result := PredefinedFontSizeCollection;
end;

function TdxInnerRichEditControl.GetHorizontalScrollBar: IdxOfficeScrollbar;
begin
  Result := FHorizontalScrollBar;
end;

function TdxInnerRichEditControl.GetVerticalScrollBar: IdxOfficeScrollbar;
begin
  Result := FVerticalScrollBar
end;

function TdxInnerRichEditControl.GetHorizontalRuler: IdxRulerControl;
begin
  Result := FHorizontalRuler;
end;

function TdxInnerRichEditControl.GetVerticalRuler: IdxRulerControl;
begin
  Result := FVerticalRuler;
end;

function TdxInnerRichEditControl.GetOnSearchComplete: TdxSearchCompleteEvent;
begin
  Result := FOnSearchComplete;
end;

procedure TdxInnerRichEditControl.SetOnSearchComplete(const Value: TdxSearchCompleteEvent);
begin
  FOnSearchComplete := Value;
end;

procedure TdxInnerRichEditControl.DoSearchComplete(E: TdxSearchCompleteEventArgs);
begin
  if Assigned(FOnSearchComplete) then
    FOnSearchComplete(Self, E);
end;

function TdxInnerRichEditControl.CanSpelling: Boolean;
begin
  Result := (TdxSpellCheckerInstance.ISpellChecker3 <> nil) and IsEditable;
end;

function TdxInnerRichEditControl.CreateSpellCheckerController: TdxSpellCheckerCustomController;
var
  ASyntaxCheckService: IdxSyntaxCheckService;
begin
  ASyntaxCheckService := DocumentModel.GetService<IdxSyntaxCheckService>;
  if ASyntaxCheckService <> nil then
    Exit(TdxSyntaxCheckController.Create(Self, ASyntaxCheckService));
  if CanSpelling then
    Result := TdxSpellCheckerController.Create(Self)
  else
    Result := TdxEmptySpellCheckerController.Create;
end;

function TdxInnerRichEditControl.CreateSpellCheckerManager(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager;
begin
  if CanSpelling then
    Result := TdxRichEditSpellCheckerManager.Create(APieceTable)
  else
    Result := TdxEmptySpellCheckerManager.Create(APieceTable);
end;

function TdxInnerRichEditControl.GetFormattingController: TdxDocumentFormattingController;
begin
  if ActiveView <> nil then
    Result := ActiveView.FormattingController
  else
    Result := nil;
end;

function TdxInnerRichEditControl.GetOwner: IdxInnerRichEditControlOwner;
begin
  Result := inherited Owner as IdxInnerRichEditControlOwner;
end;

function TdxInnerRichEditControl.GetActiveViewType: TdxRichEditViewType;
begin
  if ActiveView <> nil then
    Result := ActiveView.&Type
  else
    if FActiveViewTypeBeforeActiveViewCreation.HasValue then
      Result := FActiveViewTypeBeforeActiveViewCreation.Value
    else
      Result := DefaultViewType;
end;

procedure TdxInnerRichEditControl.SetActiveViewType(const AValue: TdxRichEditViewType);
begin
  if ActiveView = nil then
  begin
    FActiveViewTypeBeforeActiveViewCreation := AValue;
    Exit;
  end;
  if ActiveViewType = AValue then
    Exit;
  SetActiveView(FViews.GetViewByType(AValue));
  OnUpdateUI;
end;

function TdxInnerRichEditControl.GetDefaultViewType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.PrintLayout;
end;

function TdxInnerRichEditControl.GetFont: TdxGPFont;
begin
  Result := nil
end;

function TdxInnerRichEditControl.GetForeColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.Red;
end;

function TdxInnerRichEditControl.GetFormatter: TdxBackgroundFormatter;
begin
  Result := BackgroundFormatter;
end;

function TdxInnerRichEditControl.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TdxInnerRichEditControl.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  OnReadOnlyChanged;
end;

function TdxInnerRichEditControl.GetActualReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

procedure TdxInnerRichEditControl.SetOvertype(const AValue: Boolean);
begin
  if FOvertype = AValue then
    Exit;
  FOvertype := AValue;
  OnOvertypeChanged;
end;

function TdxInnerRichEditControl.GetEnabled: Boolean;
begin
  Result := Owner.Enabled;
end;

procedure TdxInnerRichEditControl.SetMeasurementUnit(AValue: TdxMeasurementUnit);
begin
  inherited SetMeasurementUnit(AValue);
  UpdateHorizontalRuler;
  UpdateVerticalRuler;
end;

function TdxInnerRichEditControl.GetViewBounds: TRect;
begin
  Result.InitSize(0, 0, 100, 100);
end;

function TdxInnerRichEditControl.GetCanUndo: Boolean;
var
  ACommand: TdxUndoCommand;
begin
  ACommand := TdxUndoCommand.Create(Owner.RichEditControl);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

function TdxInnerRichEditControl.GetCanRedo: Boolean;
var
  ACommand: TdxRedoCommand;
begin
  ACommand := TdxRedoCommand.Create(Owner.RichEditControl);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

function TdxInnerRichEditControl.GetModelDocumentLayout: TdxDocumentLayout;
begin
  if ActiveView <> nil then
    Result := ActiveView.DocumentLayout
  else
    Result := nil;
end;

procedure TdxInnerRichEditControl.BeginInitialize;
begin
  inherited BeginInitialize;
  FBackgroundThreadUIUpdater := TdxDeferredBackgroundThreadUIUpdater.Create;
end;

procedure TdxInnerRichEditControl.EndInitialize;
begin
  if Formatter <> nil then
    Formatter.SuspendWorkThread;

  inherited EndInitialize;

  if Formatter <> nil then
    Formatter.ResumeWorkThread;

  FVerticalScrollbar := Owner.CreateVerticalScrollBar;
  FHorizontalScrollbar := Owner.CreateHorizontalScrollBar;


  AddServices;
  CreateViews;

  FHorizontalRuler := Owner.CreateHorizontalRuler;
  FVerticalRuler := Owner.CreateVerticalRuler;
  RecreateSpellCheckerManager;
end;

procedure TdxInnerRichEditControl.CreateNewMeasurementAndDrawingStrategy;
begin
  inherited CreateNewMeasurementAndDrawingStrategy;
  if Formatter <> nil then
    Formatter.OnNewMeasurementAndDrawingStrategyChanged;
end;

procedure TdxInnerRichEditControl.OnFirstBeginUpdateCore;
begin
  inherited OnFirstBeginUpdateCore;
  FDeferredChanges := TdxRichEditControlDeferredChanges.Create;
  Owner.HideCaret;
  BeginScrollbarUpdate(HorizontalScrollBar);
  BeginScrollbarUpdate(VerticalScrollBar);
end;

procedure TdxInnerRichEditControl.OnLastEndUpdateCore;
begin
  inherited OnLastEndUpdateCore;
  EndScrollbarUpdate(VerticalScrollBar);
  EndScrollbarUpdate(HorizontalScrollBar);

  if FDeferredChanges.Resize then
    OnResizeCore;

  if FDeferredChanges.Redraw then
  begin
    if FDeferredChanges.RedrawAction = TdxRefreshAction.Selection then
      Owner.Redraw(TdxRefreshAction.Selection)
    else
      Owner.RedrawAfterEndUpdate;
  end;
  Owner.ShowCaret;
  if FDeferredChanges.RaiseUpdateUI then
    RaiseUpdateUI;
  FreeAndNil(FDeferredChanges);
end;

procedure TdxInnerRichEditControl.OnBeginDocumentUpdateCore;
begin
  inherited OnBeginDocumentUpdateCore;
  if ActiveView <> nil then
    ActiveView.OnBeginDocumentUpdate;
end;

function TdxInnerRichEditControl.OnEndDocumentUpdateCore(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs): TdxDocumentModelChangeActions;
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := ProcessEndDocumentUpdateCore(ASender, E);

  if ActiveView <> nil then
  begin
    OnDocumentLayoutChanged(Self, E);
    if not DocumentModel.IsUpdateLocked then
    begin
      if TdxDocumentModelChangeAction.ResetSelectionLayout in AChangeActions then
        ActiveView.SelectionLayout.Invalidate;
      if TdxDocumentModelChangeAction.ScrollToBeginOfDocument in AChangeActions then
        ActiveView.PageViewInfoGenerator.ResetAnchors;
    end;
    ActiveView.OnEndDocumentUpdate;
    ApplyChangesCore(AChangeActions);
  end;
  Result := AChangeActions;
end;

procedure TdxInnerRichEditControl.OnInnerSelectionChanged(ASender: TObject);
begin
  inherited OnInnerSelectionChanged(ASender);
  if ActiveView <> nil then
    ActiveView.OnSelectionChanged;
end;

procedure TdxInnerRichEditControl.ApplyChangesCore(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if not DocumentModel.IsUpdateLocked then
  begin
    ApplyChangesCorePlatformSpecific(AChangeActions);
    if DocumentModel.DeferredChanges.EnsureCaretVisible then
      ActiveView.EnsureCaretVisible;
  end;
  inherited ApplyChangesCore(AChangeActions);

  if TdxDocumentModelChangeAction.ApplyAutoCorrect in AChangeActions then
    ActiveView.UpdateCaretPosition;

end;


procedure TdxInnerRichEditControl.ActivateMainPieceTable(const AControl: IdxRichEditControl; AStart: TdxDocumentLogPosition);
var
  ACommand: TdxChangeActivePieceTableCommand;
begin
  DocumentModel.BeginUpdate;
  try
    ACommand := TdxChangeActivePieceTableCommand.Create(AControl, DocumentModel.MainPieceTable, nil, -1);
    try
      ACommand.Execute;
      DocumentModel.Selection.Start := AStart;
      DocumentModel.Selection.&End := AStart;
    finally
      ACommand.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxInnerRichEditControl.PerformRaiseDeferredEventsCore(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if TdxDocumentModelChangeAction.ForceResize in AChangeActions then
    OnResize
  else
  begin
    if TdxDocumentModelChangeAction.ResetRuler in AChangeActions then
      UpdateRulers;
    if TdxDocumentModelChangeAction.ForceResetHorizontalRuler in AChangeActions then
      UpdateHorizontalRuler;
    if TdxDocumentModelChangeAction.ForceResetVerticalRuler in AChangeActions then
      UpdateVerticalRuler;
  end;

  inherited PerformRaiseDeferredEventsCore(AChangeActions);
end;

procedure TdxInnerRichEditControl.OnOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  inherited OnOptionsChanged(ASender, E);

  if [TdxRichEditOptionsAction.HighlightColor,
      TdxRichEditOptionsAction.HighlightMode] * E.Actions <> [] then
    OnOptionsFieldsChanged;

  if [TdxRichEditOptionsAction.BookmarkVisibility,
      TdxRichEditOptionsAction.BookmarkColor] * E.Actions <> [] then
    OnOptionsBookmarksChanged;

  if [TdxRichEditOptionsAction.RangePermissionsVisibility,
      TdxRichEditOptionsAction.RangePermissionsColor] * E.Actions <> [] then
    OnOptionsRangePermissionsChanged;


  if [TdxRichEditOptionsAction.ParagraphMark,
      TdxRichEditOptionsAction.TabCharacter,
      TdxRichEditOptionsAction.Space] * E.Actions <> [] then
    OnOptionsFormattingMarkChanged;

  if TdxRichEditOptionsAction.HiddenText in E.Actions then
    OnHiddenTextOptionChanged;

  if [TdxRichEditOptionsAction.ForeColorSource,
      TdxRichEditOptionsAction.FontSource] * E.Actions <> [] then
    ApplyFontAndForeColor;

  if [TdxRichEditOptionsAction.ShowLeftIndent,
      TdxRichEditOptionsAction.ShowRightIndent,
      TdxRichEditOptionsAction.ShowTabs] * E.Actions <> [] then
    OnOptionsHorizontalRulerChanged;

  if [TdxRichEditOptionsAction.ParagraphTabs,
      TdxRichEditOptionsAction.Sections,
      TdxRichEditOptionsAction.Tables,
      TdxRichEditOptionsAction.ParagraphFormatting] * E.Actions <> [] then
    OnOptionsDocumentCapabilitiesChanged;

  if [TdxRichEditOptionsAction.AuthenticationEMail,
      TdxRichEditOptionsAction.AuthenticationUserName,
      TdxRichEditOptionsAction.AuthenticationGroup] * E.Actions <> [] then
    DocumentModelApplyChanges([
      TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetUncheckedIntervals]);

  if TdxRichEditOptionsAction.AutoDetectDocumentCulture in E.Actions then
    OnSpellCheckerOptionsChanged;

  OnOptionsChangedPlatformSpecific(E);
  OnUpdateUI;
end;

procedure TdxInnerRichEditControl.OnSpellCheckerOptionsChanged;
var
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
  I: Integer;
begin
  APieceTables := DocumentModel.GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := TdxPieceTable(APieceTables[I]);
      DocumentModel.ResetSpellCheck(APieceTable, APieceTable.Runs.First.GetRunIndex, APieceTable.Runs.Last.GetRunIndex, False);
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxInnerRichEditControl.OnSpellCheckerChanged(Sender: TObject);
begin
  if DocumentModel = nil then
    Exit;
  DocumentModel.BeginUpdate;
  try
    RecreateSpellCheckerController;
    RecreateSpellCheckerManager;
    DocumentModel.ApplyChangesCore(DocumentModel.MainPieceTable,
      [TdxDocumentModelChangeAction.ResetSecondaryLayout], 0, MaxInt);
  finally
    DocumentModel.EndUpdate;
  end;
  RedrawEnsureSecondaryFormattingComplete;
end;

procedure TdxInnerRichEditControl.RecreateSpellCheckerController;
begin
  if Formatter = nil then
    Exit;

  Formatter.BeginDocumentUpdate;
  try
    Formatter.SpellCheckerController := CreateSpellCheckerController;
  finally
    Formatter.EndDocumentUpdate;
  end;
end;

procedure TdxInnerRichEditControl.RecreateSpellCheckerManager;
var
  APieceTables: TdxFastList;
  ACount, I: Integer;
  APieceTable: TdxPieceTable;
begin
  APieceTables := DocumentModel.GetPieceTables(False);
  try
    ACount := APieceTables.Count;
    for I := 0 to ACount - 1 do
    begin
      APieceTable := TdxPieceTable(APieceTables[I]);
      APieceTable.SpellCheckerManager := CreateSpellCheckerManager(APieceTable);
      APieceTable.SpellCheckerManager.Initialize;
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxInnerRichEditControl.OnHiddenTextOptionChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler]);
end;

procedure TdxInnerRichEditControl.OnOptionsFormattingMarkChanged;
begin
  DocumentModelApplyChanges([TdxDocumentModelChangeAction.Redraw]);
end;

procedure TdxInnerRichEditControl.OnOptionsBookmarksChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSecondaryLayout]);
end;

procedure TdxInnerRichEditControl.OnOptionsRangePermissionsChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSecondaryLayout]);
end;

procedure TdxInnerRichEditControl.OnOptionsCommentsChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSecondaryLayout]);
end;

procedure TdxInnerRichEditControl.OnOptionsFieldsChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler]);
end;

procedure TdxInnerRichEditControl.OnOptionsDocumentCapabilitiesChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetRuler]);
end;

procedure TdxInnerRichEditControl.OnOptionsHorizontalRulerChanged;
begin
  DocumentModelApplyChanges([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetRuler]);
end;

procedure TdxInnerRichEditControl.DocumentModelApplyChanges(AChangeActions: TdxDocumentModelChangeActions);
var
  APieceTable: TdxPieceTable;
begin
  BeginUpdate;
  try
    DocumentModel.BeginUpdate;
    try
      APieceTable := DocumentModel.MainPieceTable;
      APieceTable.ApplyChangesCore(AChangeActions, 0, MaxInt);
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxInnerRichEditControl.RedrawEnsureSecondaryFormattingComplete;
begin
  Owner.RedrawEnsureSecondaryFormattingComplete;
end;

procedure TdxInnerRichEditControl.RedrawEnsureSecondaryFormattingComplete(const AAction: TdxRefreshAction);
begin
  Owner.RedrawEnsureSecondaryFormattingComplete(AAction);
end;

procedure TdxInnerRichEditControl.UpdateRulers;
begin
  Owner.UpdateRulers;
end;

procedure TdxInnerRichEditControl.UpdateHorizontalRuler;
begin
  Owner.UpdateHorizontalRuler;
end;

procedure TdxInnerRichEditControl.UpdateVerticalRuler;
begin
  Owner.UpdateVerticalRuler;
end;

procedure TdxInnerRichEditControl.OnResize;
begin
  Owner.OnResize;
end;

function TdxInnerRichEditControl.CreateViewRepository: TdxRichEditCustomViewRepository;
begin
  Result := Owner.CreateViewRepository;
end;

procedure TdxInnerRichEditControl.CreateViews;
begin
  FViews := CreateViewRepository;
  if FActiveViewTypeBeforeActiveViewCreation.HasValue then
    SetActiveViewCore(FViews.GetViewByType(FActiveViewTypeBeforeActiveViewCreation.Value))
  else
    SetActiveViewCore(FViews.GetViewByType(DefaultViewType));
end;

procedure TdxInnerRichEditControl.StopFormatting;
var
  I: Integer;
begin
  for I := 0 to FViews.ViewCount - 1 do
    FViews[I].StopFormatting;
  TdxUIThreadSyncService.Unsubscribe(Self);
end;

procedure TdxInnerRichEditControl.DisposeViews;
begin
  FreeAndNil(FViews);
  FActiveView := nil;
end;

procedure TdxInnerRichEditControl.SetActiveView(ANewView: TdxRichEditView);
begin
  BeginUpdate;
  try
    SetActiveViewCore(ANewView);
  finally
    EndUpdate;
  end;
end;

procedure TdxInnerRichEditControl.SetActiveViewCore(ANewView: TdxRichEditView);
var
  AViewBounds: TRect;
begin
  Assert(ANewView <> nil);
  if FActiveView <> nil then
    AViewBounds := DeactivateView(FActiveView)
  else
    AViewBounds := TRect.CreateSize(0, 0, 10, 10);

  FActiveView := ANewView;

  AViewBounds := CalculateActualViewBounds(AViewBounds);
  AViewBounds.X := 0;
  AViewBounds.Y := 0;
  ActivateView(FActiveView, AViewBounds);
  RaiseActiveViewChanged;
  RedrawEnsureSecondaryFormattingComplete;
  ActiveView.CorrectZoomFactor;
end;

function TdxInnerRichEditControl.DeactivateView(AView: TdxRichEditView): TRect;
begin

  if not DocumentModel.ActivePieceTable.IsMain then
  begin
    DocumentModel.SetActivePieceTableCore(DocumentModel.MainPieceTable, nil);
    AView.OnActivePieceTableChanged;
    RaiseFinishHeaderFooterEditing;
  end;

  UnsubscribeActiveViewEvents;
  DeactivateViewPlatformSpecific(AView);
  AView.Deactivate;
  Formatter.Free;
  Result := AView.Bounds;
end;

procedure TdxInnerRichEditControl.DeactivateViewAndClearActiveView(AView: TdxRichEditView);
begin
  FActiveViewTypeBeforeActiveViewCreation := ActiveViewType;
  DeactivateView(AView);
  FActiveView := nil;
end;

procedure TdxInnerRichEditControl.ActivateView(AView: TdxRichEditView; const AViewBounds: TRect);
begin
  Assert(AViewBounds.TopLeft.IsZero);

  ActiveView.CorrectZoomFactor;

  ActivateViewPlatformSpecific(AView);
  InitializeBackgroundFormatter;
  Formatter.SpellCheckerController := CreateSpellCheckerController;

  AView.Activate(AViewBounds);
  Formatter.Start;

  SubscribeActiveViewEvents;
  if IsHandleCreated then
  begin
    ActiveView.EnsureCaretVisible;
    OnResizeCore;
  end;
end;

function TdxInnerRichEditControl.GetDefaultLayoutCalculationMode: TdxCalculationModeType;
begin
  Result := TdxCalculationModeType.Automatic;
end;

function TdxInnerRichEditControl.NotifyDocumentLayoutChanged(APieceTable: TdxCustomPieceTable;
  const AChanges: TdxDocumentModelDeferredChanges;
  ADocumentLayoutResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
var
  AHandler: TdxControlDocumentChangesHandler;
begin
  AHandler := TdxControlDocumentChangesHandler.Create(ActiveView);
  try
    Result := AHandler.NotifyDocumentChanged(APieceTable, AChanges, False, ADocumentLayoutResetType);
  finally
    AHandler.Free;
  end;
end;

procedure TdxInnerRichEditControl.InitializeBackgroundFormatter;
begin
  InitializeDocumentLayout;
  BackgroundFormatter := CreateBackgroundFormatter(ActiveView.FormattingController);
  BackgroundFormatter.PageFormattingComplete.Add(OnPageFormattingComplete);
end;

function TdxInnerRichEditControl.CreateBackgroundFormatter(AController: TdxDocumentFormattingController): TdxBackgroundFormatter;
begin
  Result := TdxBackgroundFormatter.Create(AController, Owner.CommentPadding);
end;

procedure TdxInnerRichEditControl.SubscribeActiveViewEvents;
begin
  ActiveView.ZoomChanging.Add(OnActiveViewZoomChanging);
  ActiveView.ZoomChanged.Add(OnActiveViewZoomChanged);
  ActiveView.BackColorChanged.Add(OnActiveViewBackColorChanged);
end;

procedure TdxInnerRichEditControl.UnsubscribeActiveViewEvents;
begin
  ActiveView.ZoomChanging.Remove(OnActiveViewZoomChanging);
  ActiveView.ZoomChanged.Remove(OnActiveViewZoomChanged);
  ActiveView.BackColorChanged.Remove(OnActiveViewBackColorChanged);
end;

procedure TdxInnerRichEditControl.OnActiveViewZoomChanging(ASender: TObject; E: TdxEventArgs);
begin
  BeginUpdate;
  OnZoomFactorChangingPlatformSpecific;
end;

procedure TdxInnerRichEditControl.OnActiveViewZoomChanged(ASender: TObject; E: TdxEventArgs);
begin
  OnResizeCore;
  UpdateVerticalScrollBar(False);
  EndUpdate;
  RaiseZoomChanged;
end;

procedure TdxInnerRichEditControl.OnActiveViewBackColorChanged(ASender: TObject; E: TdxEventArgs);
begin
  Owner.OnActiveViewBackColorChanged;
end;

procedure TdxInnerRichEditControl.ActivateViewPlatformSpecific(AView: TdxRichEditView);
begin
  Owner.ActivateViewPlatformSpecific(AView);
end;

procedure TdxInnerRichEditControl.DeactivateViewPlatformSpecific(AView: TdxRichEditView);
begin
end;

function TdxInnerRichEditControl.GetIsHandleCreated: Boolean;
begin
  Result := Owner.IsHandleCreated;
end;

function TdxInnerRichEditControl.GetDocumentModelIntf: TdxDocumentModel;
begin
  Result := inherited DocumentModel;
end;

procedure TdxInnerRichEditControl.OnResizeCore;
begin
  Owner.OnResizeCore;
end;

function TdxInnerRichEditControl.CalculateActualViewBounds(const APreviousViewBounds: TRect): TRect;
begin
  Result := Owner.CalculateActualViewBounds(APreviousViewBounds);
end;

procedure TdxInnerRichEditControl.OnUpdateUI;
begin
  if UpdateUIOnIdle then
    ForceUpdateUIOnIdle := True
  else
    OnUpdateUICore;
end;

procedure TdxInnerRichEditControl.OnUpdateUICore;
begin
  if IsUpdateLocked then
    FDeferredChanges.RaiseUpdateUI := True
  else
    RaiseUpdateUI;
end;

procedure TdxInnerRichEditControl.OnApplicationIdle;
begin
  inherited OnApplicationIdle;
  if FForceUpdateUIOnIdle then
  begin
    OnUpdateUICore;
    FForceUpdateUIOnIdle := False;
  end;
end;

procedure TdxInnerRichEditControl.OnOptionsChangedPlatformSpecific(E: TdxOptionChangedEventArgs);
begin
  Owner.OnOptionsChangedPlatformSpecific(E);
end;

procedure TdxInnerRichEditControl.ApplyFontAndForeColor;
var
  ACharacterProperties: TdxCharacterProperties;
begin
  inherited ApplyFontAndForeColor;

  ACharacterProperties := GetDefaultCharacterProperties;
  if ACharacterProperties = nil then
    Exit;

  DocumentModel.History.DisableHistory;
  try
    ApplyFontAndForeColorCore(ACharacterProperties);
  finally
    DocumentModel.History.EnableHistory;
  end;
end;

procedure TdxInnerRichEditControl.ApplyFontAndForeColorCore(ACharacterProperties: TdxCharacterProperties);
begin
  ACharacterProperties.BeginUpdate;
  try
  finally
    ACharacterProperties.EndUpdate;
  end;
end;

function TdxInnerRichEditControl.GetDefaultCharacterProperties: TdxCharacterProperties;
begin
  if DocumentModel = nil then
    Exit(nil);

  if DocumentModel.CharacterStyles.Count <= 0 then
    Exit(nil);

  Result := DocumentModel.CharacterStyles.DefaultItem.CharacterProperties;
end;

function TdxInnerRichEditControl.ShouldApplyForeColor: Boolean;
begin
  Result := True;
end;

function TdxInnerRichEditControl.ShouldApplyFont: Boolean;
begin
  Result := True;
end;

procedure TdxInnerRichEditControl.ApplyFont(ACharacterProperties: TdxCharacterProperties; AFont: TdxGPFont);
begin
end;


procedure TdxInnerRichEditControl.OnReadOnlyChanged;
begin
  Formatter.SpellCheckerController.Reset;
  RaiseReadOnlyChanged;
  OnUpdateUI;
end;

procedure TdxInnerRichEditControl.OnOvertypeChanged;
begin
  RaiseOvertypeChanged;
  OnUpdateUI;
end;

procedure TdxInnerRichEditControl.ApplyChangesCorePlatformSpecific(AChangeActions: TdxDocumentModelChangeActions);
begin
  Owner.ApplyChangesCorePlatformSpecific(AChangeActions);
end;

procedure TdxInnerRichEditControl.OnZoomFactorChangingPlatformSpecific;
begin
  Owner.OnZoomFactorChangingPlatformSpecific;
end;

function TdxInnerRichEditControl.GetHyperlinkCollection(AField: TdxField): IdxRichEditHyperlinkCollection;
var
  AShapes: IdxRichEditReadOnlyShapeCollection;
  AShape: IdxRichEditShape;
  ADocument: TdxNativeSubDocumentBase;
  ASubDocument: IdxRichEditSubDocument;
  I: Integer;
  APieceTable: TdxPieceTable;
begin
  APieceTable := TdxPieceTable(AField.PieceTable);
  if TdxNativeSubDocumentBase(NativeDocument).PieceTable = APieceTable then
    Result := NativeDocument.Hyperlinks
  else
  begin
    if APieceTable.IsTextBox then
    begin
      AShapes := NativeDocument.Shapes.Get(NativeDocument.Range);
      for I := 0 to AShapes.Count - 1 do
      begin
        AShape := AShapes[I];
        if AShape.TextBox = nil then
          Continue;
        ADocument := Safe<TdxNativeSubDocumentBase>.Cast(TObject(AShape.TextBox.Document));
        if ADocument.PieceTable = AField.PieceTable then
          Exit(AShape.TextBox.Document.Hyperlinks);
      end;
    end;

    if APieceTable.IsHeader or APieceTable.IsFooter then
    begin
      ASubDocument := CreateNativeSubDocument(APieceTable);
      Result := ASubDocument.Hyperlinks;
      ASubDocument := nil;
      Exit;
    end;

    Result := NativeDocument.Hyperlinks;
  end;
end;

function TdxInnerRichEditControl.OnHyperlinkClick(AField: TdxField; AAllowForModifiers: Boolean): Boolean;
var
  AHyperlinks: IdxRichEditHyperlinkCollection;
  AHyperlink: IdxRichEditHyperLink;
  E: TdxHyperlinkClickEventArgs;
begin
  AHyperlinks := GetHyperlinkCollection(AField);
  Result := (AHyperlinks <> nil) and AHyperlinks.FindHyperlink(AField.Index, AHyperlink);
  if not Result then
    Exit;

  E := TdxHyperlinkClickEventArgs.Create(AHyperlink, TdxKeyboardHelper.ModifierKeys);
  try
    RaiseHyperlinkClick(E);
    Result := E.Handled;
  finally
    E.Free;
  end;

  if not Result and (not AAllowForModifiers or IsHyperlinkModifierKeysPress) then
  begin
    OpenHyperlink(AField);
    Result := True;
  end;
end;

function TdxInnerRichEditControl.IsHyperlinkModifierKeysPress: Boolean;
var
  AShortCut: TShortCut;
  AModifierKeys: TShortCut;
begin
  AShortCut := Options.Hyperlinks.ModifierKeys;
  AModifierKeys := AShortCut and not (scShift + scCtrl + scAlt);
  if AModifierKeys = VK_MENU then
  begin
    AModifierKeys := scAlt;
    AShortCut := AShortCut and not VK_MENU;
  end;
  if AModifierKeys = VK_CONTROL then
  begin
    AModifierKeys := scCtrl;
    AShortCut := AShortCut and not VK_CONTROL;
  end;
  if AModifierKeys = VK_SHIFT then
  begin
    AModifierKeys := scShift;
    AShortCut := AShortCut and not VK_SHIFT;
  end;
  AModifierKeys := (AModifierKeys or AShortCut) and (scShift or scCtrl or scAlt);
  Result := TdxKeyboardHelper.ModifierKeys = AModifierKeys;
end;

procedure TdxInnerRichEditControl.OpenHyperlink(AField: TdxField);
var
  ACommand: TdxFollowHyperlinkCommand;
begin
  ACommand := TdxFollowHyperlinkCommand.Create(Owner.RichEditControl, AField);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxInnerRichEditControl.UnitsToLayoutUnits(const APoint: TdxPointF): TPoint;
begin
end;

function TdxInnerRichEditControl.LayoutUnitsToUnits(const ASize: TSize): TdxSizeF;
begin
end;

function TdxInnerRichEditControl.LayoutUnitsToUnits(const ABounds: TRect): TdxRectF;
begin
end;


function TdxInnerRichEditControl.CreateCommand(ACommandId: TdxRichEditCommandId): TdxRichEditCommand;
var
  ACommandClass: TdxRichEditCommandClass;
begin
  if CommandTable.TryGetValue(ACommandID, ACommandClass) then
    Result := ACommandClass.Create(Owner.RichEditControl)
  else
    Result := nil;
end;


procedure TdxInnerRichEditControl.ScrollToCaret(ARelativeVerticalPosition: Single);
var
  AScrollVertically: TdxEnsureCaretVisibleVerticallyCommand;
  AScrollHorizontally: TdxEnsureCaretVisibleHorizontallyCommand;
begin
  BeginUpdate;
  try
    AScrollVertically := TdxEnsureCaretVisibleVerticallyCommand.Create(Owner.RichEditControl);
    try
      AScrollVertically.RelativeCaretPosition := ARelativeVerticalPosition;
      AScrollVertically.Execute;
    finally
      AScrollVertically.Free;
    end;

    AScrollHorizontally := TdxEnsureCaretVisibleHorizontallyCommand.Create(Owner.RichEditControl);
    try
      AScrollHorizontally.Execute;
    finally
      AScrollHorizontally.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxInnerRichEditControl.Undo;
var
  ACommand: TdxUndoCommand;
begin
  ACommand := TdxUndoCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.Redo;
var
  ACommand: TdxRedoCommand;
begin
  ACommand := TdxRedoCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.ClearUndo;
var
  ACommand: TdxClearUndoCommand;
begin
  ACommand := TdxClearUndoCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.Cut;
var
  ACommand: TdxCutSelectionCommand;
begin
  ACommand := TdxCutSelectionCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.Copy;
var
  ACommand: TdxCopySelectionCommand;
begin
  ACommand := TdxCopySelectionCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.Paste;
var
  ACommand: TdxPasteSelectionCommand;
begin
  ACommand := TdxPasteSelectionCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.SelectAll;
var
  ACommand: TdxSelectAllCommand;
begin
  ACommand := TdxSelectAllCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.DeselectAll;
var
  ACommand: TdxDeselectAllCommand;
begin
  ACommand := TdxDeselectAllCommand.Create(Owner.RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.UpdateVerticalScrollBar(AAvoidJump: Boolean);
begin
  if AAvoidJump then
    ActiveView.VerticalScrollController.ScrollBarAdapter.SynchronizeScrollBarAvoidJump
  else
    ActiveView.VerticalScrollController.ScrollBarAdapter.EnsureSynchronized;
end;

procedure TdxInnerRichEditControl.BeginDocumentRendering;
begin
  if ActiveView <> nil then
    ActiveView.BeginDocumentRendering;
end;

procedure TdxInnerRichEditControl.DoShortCut(Args: TdxRichEditShortCutEventArgs);
begin
end;

procedure TdxInnerRichEditControl.EndDocumentRendering;
begin
  if ActiveView <> nil then
    ActiveView.EndDocumentRendering;
end;

procedure TdxInnerRichEditControl.SetLayoutUnitCore(AUnit: TdxDocumentLayoutUnit);
begin
  inherited SetLayoutUnitCore(AUnit);
  if ActiveView = nil then
    Exit;

  Owner.ResizeView(True);

  UpdateHorizontalRuler;
  UpdateVerticalRuler;
  UpdateVerticalScrollBar(False);
  ActiveView.UpdateHorizontalScrollBar;
  Owner.Redraw;
end;

procedure TdxInnerRichEditControl.SetDocumentModelLayoutUnitCore(AUnit: TdxDocumentLayoutUnit);
begin
  if ActiveView <> nil then
    ActiveView.OnLayoutUnitChanging;
  inherited SetDocumentModelLayoutUnitCore(AUnit);
  RaiseLayoutUnitChanged;
  if ActiveView <> nil then
    ActiveView.OnLayoutUnitChanged;
end;

procedure TdxInnerRichEditControl.BeginScrollbarUpdate(const AScrollbar: IdxOfficeScrollbar);
begin
  if AScrollbar <> nil then
    AScrollbar.BeginUpdate;
end;

procedure TdxInnerRichEditControl.EndScrollbarUpdate(const AScrollbar: IdxOfficeScrollbar);
begin
  if AScrollbar <> nil then
    AScrollbar.EndUpdate;
end;

procedure TdxInnerRichEditControl.LoadDocument;
begin
  LoadDocument(Owner);
end;

procedure TdxInnerRichEditControl.LoadDocument(const AParent: IdxInnerRichEditDocumentServerOwner);
begin
  LoadDocumentCore(AParent);
end;

procedure TdxInnerRichEditControl.LoadDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner);
var
  AImportManagerService: IdxDocumentImportManagerService;
  AImportHelper: TdxDocumentImportHelper;
  AImportSource: TdxImportSource<TdxRichEditDocumentFormat, Boolean>;
  AOldCursor: TCursor;
begin
  AImportManagerService := GetService<IdxDocumentImportManagerService>;
  AImportHelper := TdxDocumentImportHelper.Create(DocumentModel);
  try
    AImportSource := AImportHelper.InvokeImportDialog(AParent.Control, AImportManagerService);
    try
      if AImportSource = nil then
        Exit;
      AOldCursor := Screen.Cursor;
      try
        Screen.Cursor := TdxRichEditCursors.WaitCursor;
        LoadDocument(AImportSource.Storage, AImportSource.Importer.Format);
      finally
        Screen.Cursor := AOldCursor;
      end;
    finally
      AImportSource.Free;
    end;
  finally
    AImportHelper.Free;
  end;
end;

function TdxInnerRichEditControl.SaveDocumentAs: Boolean;
begin
  Result := SaveDocumentAs(Owner);
end;

function TdxInnerRichEditControl.SaveDocumentAs(const AParent: IdxInnerRichEditDocumentServerOwner): Boolean;
begin
  Result := ExportDocumentCore(AParent, nil);
end;

function TdxInnerRichEditControl.ExportDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner; ACalc: TdxExportersCalculator): Boolean;
var
  AOptions: TdxDocumentSaveOptions;
begin
  AOptions := DocumentModel.DocumentSaveOptions;
  Result := ExportDocumentCore(AParent, ACalc, AOptions);
end;

function TdxInnerRichEditControl.ExportDocumentCore(const AParent: IdxInnerRichEditDocumentServerOwner;
  ACalc: TdxExportersCalculator; const AOptions: IdxDocumentSaveOptions): Boolean;
var
  AExportManagerService: IdxDocumentExportManagerService;
  AExportHelper: TdxDocumentExportHelper;
  ATarget: TdxExportTarget;
  AOldCursor: TCursor;
begin
  AExportManagerService := GetService<IdxDocumentExportManagerService>;
  AExportHelper := TdxDocumentExportHelper.Create(DocumentModel);
  try
    ATarget := AExportHelper.InvokeExportDialog(AParent.Control, AExportManagerService, ACalc);
    try
      if ATarget = nil then
        Exit(False);
      AOldCursor := Screen.Cursor;
      try
        Screen.Cursor := TdxRichEditCursors.WaitCursor;
        SaveDocument(ATarget.Storage, ATarget.Exporter.Format);
      finally
        Screen.Cursor := AOldCursor;
      end;
    finally
      ATarget.Free;
    end;
  finally
    AExportHelper.Free;
  end;
  Result := True;
end;

function TdxInnerRichEditControl.SaveDocument: Boolean;
begin
  Result := SaveDocument(Owner);
end;

function TdxInnerRichEditControl.SaveDocument(const AParent: IdxInnerRichEditDocumentServerOwner): Boolean;
var
  ADocumentSaveOptions: TdxDocumentSaveOptions;
begin
  ADocumentSaveOptions := DocumentModel.DocumentSaveOptions;
  if not ADocumentSaveOptions.CanSaveToCurrentFileName or (ADocumentSaveOptions.CurrentFileName = '') or
      (ADocumentSaveOptions.CurrentFormat = TdxRichEditDocumentFormat.Undefined) then
    Result := SaveDocumentAs(AParent)
  else
  begin
    SaveDocument(ADocumentSaveOptions.CurrentFileName, ADocumentSaveOptions.CurrentFormat);
    Result := True;
  end;
end;

procedure TdxInnerRichEditControl.AddServices;
begin
  AddService(IdxThreadPoolService, TdxBackgroundTaskController.Create);
  AddService(IdxAutoCorrectService, TdxAutoCorrectService.Create(Self));
end;

function TdxInnerRichEditControl.GetVerticalScrollValue: Int64;
begin
  Result := ActiveView.VerticalScrollController.ScrollBarAdapter.Value;
end;

procedure TdxInnerRichEditControl.SetVerticalScrollValue(AValue: Int64);
begin
  ActiveView.VerticalScrollController.ScrollToAbsolutePosition(AValue);
  ActiveView.OnVerticalScroll;
end;

function TdxInnerRichEditControl.GetVerticalScrollPosition: Int64;
begin
  Result := ActiveView.VerticalScrollController.ScrollBarAdapter.Value;
end;

procedure TdxInnerRichEditControl.SetVerticalScrollPosition(AValue: Int64);
var
  APreviousValue: Int64;
  ACommand: TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand;
begin
  APreviousValue := GetVerticalScrollPosition;
  ACommand := TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand.Create(Owner.RichEditControl);
  try
    ACommand.PhysicalOffset := Integer((AValue - APreviousValue));
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInnerRichEditControl.OnGestureBegin;
begin
  FGestureActivated := True;
end;

procedure TdxInnerRichEditControl.OnGestureEnd;
begin
  FGestureActivated := False;
end;

procedure TdxInnerRichEditControl.RaiseReadOnlyChanged;
begin
  if not FOnReadOnlyChanged.Empty then
    FOnReadOnlyChanged.Invoke(Control);
end;

procedure TdxInnerRichEditControl.RaiseOvertypeChanged;
begin
  if not FOnOvertypeChanged.Empty then
    FOnOvertypeChanged.Invoke(Control);
end;

procedure TdxInnerRichEditControl.RaiseSearchComplete(E: TdxSearchCompleteEventArgs);
begin
end;

procedure TdxInnerRichEditControl.RaiseZoomChanged;
begin
  if not FOnZoomChanged.Empty then
    FOnZoomChanged.Invoke(Control);
end;

procedure TdxInnerRichEditControl.RaiseDocumentProtectionChanged;
begin
  inherited RaiseDocumentProtectionChanged;
  Formatter.SpellCheckerController.Reset;
end;

procedure TdxInnerRichEditControl.RaiseHyperlinkClick(AArgs: TdxHyperlinkClickEventArgs);
begin
  if not FOnHyperlinkClick.Empty then
    FOnHyperlinkClick.Invoke(Control, AArgs);
end;

procedure TdxInnerRichEditControl.RaiseLayoutUnitChanged;
begin
  if not FOnLayoutUnitChanged.Empty then
    FOnLayoutUnitChanged.Invoke(Control);
end;

procedure TdxInnerRichEditControl.RaiseVisiblePagesChanged;
begin
  if not FOnVisiblePagesChanged.Empty then
    FOnVisiblePagesChanged.Invoke(Control);
end;

procedure TdxInnerRichEditControl.PopulateSelectionCommands;
begin
  RegisterCommand(TdxStartOfDocumentCommand);
  RegisterCommand(TdxEndOfDocumentCommand);
  RegisterCommand(TdxExtendStartOfDocumentCommand);
  RegisterCommand(TdxExtendEndOfDocumentCommand);
  RegisterCommand(TdxSelectAllCommand);
  RegisterCommand(TdxDeselectAllCommand);
  RegisterCommand(TdxCopySelectionCommand);
  RegisterCommand(TdxPasteSelectionCommand);
  RegisterCommand(TdxCutSelectionCommand);
  RegisterCommand(TdxSelectTableColumnsCommand);
  RegisterCommand(TdxSelectTableCellCommand);
  RegisterCommand(TdxSelectTableRowCommand);
  RegisterCommand(TdxDeleteCommand);
  RegisterCommand(TdxBackSpaceKeyCommand);
  RegisterCommand(TdxDeleteWordCommand);
  RegisterCommand(TdxDeleteWordBackCommand);
  RegisterCommand(TdxSelectUpperLevelObjectCommand);
end;

procedure TdxInnerRichEditControl.PopulateTableCommands;
begin
  RegisterCommand(TdxInsertTableCommand);
  RegisterCommand(TdxInsertTableRowBelowCommand);
  RegisterCommand(TdxInsertTableRowAboveCommand);
  RegisterCommand(TdxDeleteTableRowsMenuCommand);
  RegisterCommand(TdxMergeTableElementMenuCommand);
  RegisterCommand(TdxMergeTableCellsCommand);
  RegisterCommand(TdxSplitTableCommand);
  RegisterCommand(TdxToggleTableCellsAllBordersCommand);
  RegisterCommand(TdxResetTableCellsBordersCommand);
  RegisterCommand(TdxToggleTableCellsOutsideBorderCommand);
  RegisterCommand(TdxToggleTableCellsInsideBorderCommand);
  RegisterCommand(TdxToggleTableCellsLeftBorderCommand);
  RegisterCommand(TdxToggleTableCellsRightBorderCommand);
  RegisterCommand(TdxToggleTableCellsTopBorderCommand);
  RegisterCommand(TdxToggleTableCellsBottomBorderCommand);
  RegisterCommand(TdxToggleTableCellsInsideHorizontalBorderCommand);
  RegisterCommand(TdxToggleTableCellsInsideVerticalBorderCommand);
  RegisterCommand(TdxInsertTableColumnToTheLeftCommand);
  RegisterCommand(TdxInsertTableColumnToTheRightCommand);
  RegisterCommand(TdxDeleteTableCommand);
  RegisterCommand(TdxDeleteTableColumnsCommand);
  RegisterCommand(TdxDeleteTableRowsCommand);
  RegisterCommand(TdxChangeTableCellsContentAlignmentPlaceholderCommand);
  RegisterCommand(TdxToggleTableCellsTopLeftAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsTopCenterAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsTopRightAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsMiddleLeftAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsMiddleCenterAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsMiddleRightAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsBottomLeftAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsBottomCenterAlignmentCommand);
  RegisterCommand(TdxToggleTableCellsBottomRightAlignmentCommand);
  RegisterCommand(TdxDeleteTableColumnsMenuCommand);
  RegisterCommand(TdxToggleShowTableGridLinesCommand);
  RegisterCommand(TdxToggleTableAutoFitPlaceholderMenuCommand);
  RegisterCommand(TdxToggleTableAutoFitPlaceholderCommand);
  RegisterCommand(TdxToggleTableAutoFitContentsCommand);
  RegisterCommand(TdxToggleTableAutoFitWindowCommand);
  RegisterCommand(TdxToggleTableFixedColumnWidthCommand);
end;

procedure TdxInnerRichEditControl.PopulateFieldCommands;
begin
  RegisterCommand(TdxShowBookmarkFormCommand);
  RegisterCommand(TdxCreateBookmarkContextMenuItemCommand);
  RegisterCommand(TdxShowHyperlinkFormCommand);
  RegisterCommand(TdxCreateHyperlinkContextMenuItemCommand);
  RegisterCommand(TdxEditHyperlinkCommand);
  RegisterCommand(TdxCreateFieldCommand);
  RegisterCommand(TdxUpdateFieldCommand);
  RegisterCommand(TdxToggleFieldCodesCommand);
  RegisterCommand(TdxShowInsertMergeFieldFormCommand);
  RegisterCommand(TdxToggleViewMergedDataCommand);
  RegisterCommand(TdxShowAllFieldCodesCommand);
  RegisterCommand(TdxShowAllFieldResultsCommand);
  RegisterCommand(TdxInsertPageNumberFieldCommand);
  RegisterCommand(TdxInsertPageCountFieldCommand);
  RegisterCommand(TdxInsertMergeFieldCommand);
  RegisterCommand(TdxSetParagraphHeading1LevelCommand);
  RegisterCommand(TdxSetParagraphHeading2LevelCommand);
  RegisterCommand(TdxSetParagraphHeading3LevelCommand);
  RegisterCommand(TdxSetParagraphHeading4LevelCommand);
  RegisterCommand(TdxSetParagraphHeading5LevelCommand);
  RegisterCommand(TdxSetParagraphHeading6LevelCommand);
  RegisterCommand(TdxSetParagraphHeading7LevelCommand);
  RegisterCommand(TdxSetParagraphHeading8LevelCommand);
  RegisterCommand(TdxSetParagraphHeading9LevelCommand);
  RegisterCommand(TdxAddParagraphsToTableOfContentsCommand);
  RegisterCommand(TdxInsertTableOfContentsCommand);
  RegisterCommand(TdxInsertTableOfEquationsCommand);
  RegisterCommand(TdxInsertTableOfFiguresCommand);
  RegisterCommand(TdxInsertTableOfTablesCommand);
  RegisterCommand(TdxInsertTableOfFiguresPlaceholderCommand);
  RegisterCommand(TdxInsertEquationCaptionCommand);
  RegisterCommand(TdxInsertFigureCaptionCommand);
  RegisterCommand(TdxInsertTableCaptionCommand);
  RegisterCommand(TdxInsertCaptionPlaceholderCommand);
  RegisterCommand(TdxUpdateTableOfContentsCommand);
  RegisterCommand(TdxUpdateTableOfFiguresCommand);
  RegisterCommand(TdxUpdateFieldsCommand);
  RegisterCommand(TdxOpenHyperlinkCommand);
  RegisterCommand(TdxRemoveHyperlinkFieldCommand);
end;

procedure TdxInnerRichEditControl.PopulateFloatingObjectCommands;
begin
  RegisterCommand(TdxSetFloatingObjectSquareTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectBehindTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectInFrontOfTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectThroughTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectTightTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand);
  RegisterCommand(TdxSetFloatingObjectTopLeftAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectTopCenterAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectTopRightAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectMiddleLeftAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectMiddleCenterAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectMiddleRightAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectBottomLeftAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectBottomCenterAlignmentCommand);
  RegisterCommand(TdxSetFloatingObjectBottomRightAlignmentCommand);
  RegisterCommand(TdxChangeFloatingObjectTextWrapTypeCommand);
  RegisterCommand(TdxChangeFloatingObjectAlignmentCommand);
  RegisterCommand(TdxFloatingObjectBringForwardPlaceholderCommand);
  RegisterCommand(TdxFloatingObjectBringForwardCommand);
  RegisterCommand(TdxFloatingObjectBringToFrontCommand);
  RegisterCommand(TdxFloatingObjectBringInFrontOfTextCommand);
  RegisterCommand(TdxFloatingObjectSendBackwardCommand);
  RegisterCommand(TdxFloatingObjectSendToBackCommand);
  RegisterCommand(TdxFloatingObjectSendBehindTextCommand);
  RegisterCommand(TdxFloatingObjectSendBackwardPlaceholderCommand);
  RegisterCommand(TdxChangeFloatingObjectFillColorCommand);
  RegisterCommand(TdxChangeFloatingObjectOutlineColorCommand);
  RegisterCommand(TdxChangeFloatingObjectOutlineWidthCommand);
  RegisterCommand(TdxInsertTextBoxCommand);
  RegisterCommand(TdxInsertFloatingObjectPictureCommand);
end;

procedure TdxInnerRichEditControl.PopulateFormattingCommands;
begin
  RegisterCommand(TdxToggleFontItalicCommand);
  RegisterCommand(TdxToggleFontUnderlineCommand);
  RegisterCommand(TdxToggleFontDoubleUnderlineCommand);
  RegisterCommand(TdxToggleFontStrikeoutCommand);
  RegisterCommand(TdxToggleFontDoubleStrikeoutCommand);
  RegisterCommand(TdxIncreaseFontSizeCommand);
  RegisterCommand(TdxDecreaseFontSizeCommand);
  RegisterCommand(TdxIncrementFontSizeCommand);
  RegisterCommand(TdxDecrementFontSizeCommand);
  RegisterCommand(TdxToggleFontSuperscriptCommand);
  RegisterCommand(TdxToggleFontSubscriptCommand);
  RegisterCommand(TdxToggleParagraphAlignmentLeftCommand);
  RegisterCommand(TdxToggleParagraphAlignmentCenterCommand);
  RegisterCommand(TdxToggleParagraphAlignmentRightCommand);
  RegisterCommand(TdxToggleParagraphAlignmentJustifyCommand);
  RegisterCommand(TdxSetSingleParagraphSpacingCommand);
  RegisterCommand(TdxSetDoubleParagraphSpacingCommand);
  RegisterCommand(TdxSetSesquialteralParagraphSpacingCommand);
  RegisterCommand(TdxIncrementNumerationFromParagraphCommand);
  RegisterCommand(TdxDecrementNumerationFromParagraphCommand);
  RegisterCommand(TdxIncrementIndentCommand);
  RegisterCommand(TdxDecrementIndentCommand);
  RegisterCommand(TdxToggleSimpleNumberingListCommand);
  RegisterCommand(TdxToggleMultiLevelListCommand);
  RegisterCommand(TdxToggleBulletedListCommand);
  RegisterCommand(TdxChangeFontColorCommand);
  RegisterCommand(TdxChangeFontBackColorCommand);
  RegisterCommand(TdxChangeFontNameCommand);
  RegisterCommand(TdxChangeFontSizeCommand);
  RegisterCommand(TdxIncrementParagraphOutlineLevelCommand);
  RegisterCommand(TdxDecrementParagraphOutlineLevelCommand);
  RegisterCommand(TdxSetParagraphBodyTextLevelCommand);
  RegisterCommand(TdxChangePageColorCommand);
end;

procedure TdxInnerRichEditControl.PopulateCommands;
begin
  PopulateSelectionCommands;
  PopulateFormattingCommands;
  PopulateTableCommands;
  PopulateFieldCommands;
  PopulateFloatingObjectCommands;

  RegisterCommand(TdxToggleShowWhitespaceCommand);
  RegisterCommand(TdxRedoCommand);
  RegisterCommand(TdxClearUndoCommand);
  RegisterCommand(TdxInsertParagraphCommand);
  RegisterCommand(TdxInsertLineBreakCommand);
  RegisterCommand(TdxInsertPageBreakCommand);
  RegisterCommand(TdxInsertBreakCommand);
  RegisterCommand(TdxInsertSectionBreakNextPageCommand);
  RegisterCommand(TdxInsertSectionBreakOddPageCommand);
  RegisterCommand(TdxInsertSectionBreakEvenPageCommand);
  RegisterCommand(TdxInsertSectionBreakContinuousCommand);
  RegisterCommand(TdxInsertNonBreakingSpaceCommand);
  RegisterCommand(TdxInsertColumnBreakCommand);
  RegisterCommand(TdxInsertEnDashCommand);
  RegisterCommand(TdxInsertEmDashCommand);
  RegisterCommand(TdxInsertCopyrightSymbolCommand);
  RegisterCommand(TdxInsertRegisteredTrademarkSymbolCommand);
  RegisterCommand(TdxInsertTrademarkSymbolCommand);
  RegisterCommand(TdxInsertEllipsisCommand);

  RegisterCommand(TdxSaveDocumentAsCommand);
  RegisterCommand(TdxZoomInCommand);
  RegisterCommand(TdxZoomOutCommand);
  RegisterCommand(TdxInsertPictureCommand);
  RegisterCommand(TdxSwitchToDraftViewCommand);
  RegisterCommand(TdxSwitchToPrintLayoutViewCommand);
  RegisterCommand(TdxSwitchToSimpleViewCommand);
  RegisterCommand(TdxEditPageHeaderCommand);
  RegisterCommand(TdxEditPageFooterCommand);
  RegisterCommand(TdxClosePageHeaderFooterCommand);
  RegisterCommand(TdxGoToPageHeaderCommand);
  RegisterCommand(TdxGoToPageFooterCommand);
  RegisterCommand(TdxToggleHeaderFooterLinkToPreviousCommand);
  RegisterCommand(TdxGoToPreviousPageHeaderFooterCommand);
  RegisterCommand(TdxGoToNextPageHeaderFooterCommand);
  RegisterCommand(TdxToggleDifferentFirstPageCommand);
  RegisterCommand(TdxToggleDifferentOddAndEvenPagesCommand);
  RegisterCommand(TdxCheckSpellingCommand);
  RegisterCommand(TdxZoomCommand);
  RegisterCommand(TdxZoomPercentCommand);
  RegisterCommand(TdxSetSectionOneColumnCommand);
  RegisterCommand(TdxSetSectionTwoColumnsCommand);
  RegisterCommand(TdxSetSectionThreeColumnsCommand);
  RegisterCommand(TdxSetSectionColumnsPlaceholderCommand);
  RegisterCommand(TdxMakeTextUpperCaseCommand);
  RegisterCommand(TdxMakeTextLowerCaseCommand);
  RegisterCommand(TdxToggleTextCaseCommand);
  RegisterCommand(TdxChangeTextCasePlaceholderCommand);
  RegisterCommand(TdxToggleShowHorizontalRulerCommand);
  RegisterCommand(TdxToggleShowVerticalRulerCommand);
  RegisterCommand(TdxChangeSectionLineNumberingCommand);
  RegisterCommand(TdxSetSectionLineNumberingNoneCommand);
  RegisterCommand(TdxSetSectionLineNumberingContinuousCommand);
  RegisterCommand(TdxSetSectionLineNumberingRestartNewPageCommand);
  RegisterCommand(TdxSetSectionLineNumberingRestartNewSectionCommand);
  RegisterCommand(TdxDeleteRepeatedWordCommand);
  RegisterCommand(TdxIgnoreMisspellingCommand);
  RegisterCommand(TdxIgnoreAllMisspellingsCommand);
  RegisterCommand(TdxAddWordToDictionaryCommand);
  RegisterCommand(TdxReplaceMisspellingCommand);
  RegisterCommand(TdxAutoCorrectMisspellingCommand);
  RegisterCommand(TdxOvertypeTextCommand);
  RegisterCommand(TdxInsertTextCommand);
end;

procedure TdxInnerRichEditControl.RegisterCommand(ACommandClass: TdxRichEditCommandClass);
begin
  CommandTable.AddOrSetValue(ACommandClass.Id, ACommandClass);
end;

procedure TdxInnerRichEditControl.UnregisterCommand(ACommandClass: TdxRichEditCommandClass);
begin
  CommandTable.Remove(ACommandClass.Id);
end;

{ TdxRichEditScrollbarOptions }

procedure TdxRichEditScrollbarOptions.Assign(Source: TPersistent);
begin
  if Source is TdxRichEditScrollbarOptions then
    FVisibility := TdxRichEditScrollbarOptions(Source).Visibility;
  inherited Assign(Source);
end;

procedure TdxRichEditScrollbarOptions.DoReset;
begin
  inherited DoReset;
  Visibility := TdxRichEditScrollbarVisibility.Auto;
end;

procedure TdxRichEditScrollbarOptions.SetVisibility(const Value: TdxRichEditScrollbarVisibility);
begin
  if FVisibility <> Value then
  begin
    FVisibility := Value;
    DoChanged(TAction.Visibility);
  end;
end;

{ TdxRichEditDocumentServerOptions }

constructor TdxRichEditDocumentServerOptions.Create(
  const ADocumentServer: IdxRichEditDocumentServer);
begin
  inherited Create(ADocumentServer);
  Import.Html.DefaultAsyncImageLoading := False;
  Import.Html.AsyncImageLoading := False;
end;

{ TdxRichEditControlCustomOptions }

procedure TdxRichEditControlCustomOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FVerticalScrollbar := TdxVerticalScrollbarOptions.Create;
  FHorizontalScrollbar:= TdxHorizontalScrollbarOptions.Create;
end;

destructor TdxRichEditControlCustomOptions.Destroy;
begin
  FreeAndNil(FHorizontalScrollbar);
  FreeAndNil(FVerticalScrollbar);
  inherited Destroy;
end;

procedure TdxRichEditControlCustomOptions.SetHorizontalScrollbar(const Value: TdxHorizontalScrollbarOptions);
begin
  FHorizontalScrollbar.Assign(Value);
end;

procedure TdxRichEditControlCustomOptions.SetVerticalScrollbar(const Value: TdxVerticalScrollbarOptions);
begin
  FVerticalScrollbar.Assign(Value);
end;

procedure TdxRichEditControlCustomOptions.SubscribeInnerOptions;
begin
  inherited SubscribeInnerOptions;
  FVerticalScrollbar.Changed.Add(DoInnerOptionsChanged);
  FHorizontalScrollbar.Changed.Add(DoInnerOptionsChanged);
end;

end.
