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

unit dxRichEdit.Control;

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
  dxRichEdit.Control.Core,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.MenuBuilder,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.EventArgs,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
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
  dxRichEdit.View.Draft,
  dxRichEdit.View.PrintLayout,
  dxRichEdit.View.Simple,
  dxRichEdit.View.ViewInfo;

type
  TdxCustomRichEditControl = class;

  { TdxRichEditControlKeyboardDefaultHandler }

  TdxRichEditControlKeyboardDefaultHandler = class(TdxRichEditKeyboardDefaultHandler)
  protected
    procedure PopulateDialogsCommandTable; override;
  end;

  { TdxRichEditControlKeyboardController }

  TdxRichEditControlKeyboardController = class(TdxRichEditKeyboardController)
  protected
    function CreateDefaultHandler: IdxKeyboardHandlerService; override;
  end;

  { TdxRichEditInnerControl }

  TdxRichEditInnerControl = class(TdxRichEditCustomInnerControl)
  protected
    function CreateKeyboardController: TdxCustomKeyboardController; override;
    function GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean; override;
    procedure PopulateCommands; override;
  public
    procedure DoShortCut(Args: TdxRichEditShortCutEventArgs); override;
  end;

  { TdxRichEditInnerDocumentServer }

  TdxRichEditInnerDocumentServer = class(TdxInnerRichEditDocumentServer)
  protected
    function CreateNativeDocument: IdxRichEditDocument; override;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument; override;
    function GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean; override;
    function GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions; override;
  public
    function CreateMailMergeOptions: IdxRichEditMailMergeOptions; override;
  end;

  { TdxRichEditInternalDocumentServer }

  TdxRichEditInternalDocumentServer = class(TdxInternalRichEditDocumentServer)
  protected
    function CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer; override;
  end;

  { TdxRichEditViewRepository }

  TdxRichEditViewRepository = class(TdxRichEditCustomViewRepository)
  private
    function GetSimpleView: TdxSimpleView;
    procedure SetSimpleView(const Value: TdxSimpleView);
    function GetPrintLayoutView: TdxPrintLayoutView;
    procedure SetPrintLayoutView(const Value: TdxPrintLayoutView);
    function GetDraftView: TdxDraftView;
    procedure SetDraftView(const Value: TdxDraftView);
  protected
    procedure CreateViews; override;
  published
    property Draft: TdxDraftView read GetDraftView write SetDraftView;
    property PrintLayoutView: TdxPrintLayoutView read GetPrintLayoutView write SetPrintLayoutView;
    property Simple: TdxSimpleView read GetSimpleView write SetSimpleView;
  end;

  { TdxRichEditControlOptions }

  TdxRichEditControlOptions = class(TdxRichEditControlCustomOptions)
  published
    property Authentication;
    property AutoCorrect;
    property Behavior;
    property Bookmarks;
    property CopyPaste;
    property DocumentCapabilities;
    property DocumentSaveOptions;
    property Export;
    property Fields;
    property FormattingMarkVisibility;
    property HorizontalRuler;
    property HorizontalScrollbar;
    property Hyperlinks;
    property Import;
    property Layout;
    property MailMerge;
    property Printing;
    property RangePermissions;
    property Search;
    property TableOptions;
    property VerticalRuler;
    property VerticalScrollbar;
  end;

  { TdxCustomRichEditRulerControl }

  TdxCustomRichEditRulerControl = class abstract (TdxVCLControl, IdxRulerControl)
  protected
    function GetRichEditControl: IdxRichEditControl; virtual; abstract;
    function GetIsVisible: Boolean; virtual; abstract;
    function GetRulerSizeInPixels: Integer; virtual; abstract;

    function CanUpdate: Boolean; virtual; abstract;
    function GetRulerHeightInPixels: Integer; virtual;
    function GetRulerWidthInPixels: Integer; virtual;
    procedure OnLookAndFeelChanged; virtual; abstract;
    procedure RecreatePainter; virtual; abstract;
    procedure Reset; virtual; abstract;

    property IsVisible: Boolean read GetIsVisible;
  end;

  TdxRichEditControlBeforeExportEvent = procedure (Sender: TdxCustomRichEditControl; ADocumentFormat: TdxRichEditDocumentFormat; var AUriTarget: string) of object;
  TdxRichEditControlQueryEncryptionPasswordEvent = procedure (Sender: TdxCustomRichEditControl; var APassword: string; var AHandled: Boolean) of object;

  { TdxCustomRichEditControl }

  TdxCustomRichEditControl = class(TdxRichEditControlBase)
  strict private
    FSearchForm: TForm;
    FInsertMergeFieldForm: TForm;

    FHorizontalRuler: TdxCustomRichEditRulerControl;
    FVerticalRuler: TdxCustomRichEditRulerControl;

    FOnBeforeExport: TdxRichEditControlBeforeExportEvent;
    FOnTabsFormShowing: TdxTabsFormShowingEvent;
    FOnColumnsSetupFormShowing: TdxColumnsSetupFormShowingEvent;
    FOnParagraphFormShowing: TdxParagraphFormShowingEvent;
    FOnSplitTableCellsFormShowing: TdxSplitTableCellsFormShowingEvent;
    FOnFontFormShowing: TdxFontFormShowingEvent;
    FOnDeleteTableCellsFormShowing: TdxDeleteTableCellsFormShowingEvent;
    FOnEditStyleFormShowing: TdxEditStyleFormShowingEvent;
    FOnInsertTableCellsFormShowing: TdxInsertTableCellsFormShowingEvent;
    FOnTablePropertiesFormShowing: TdxTablePropertiesFormShowingEvent;
    FOnHyperlinkFormShowing: TdxHyperlinkFormShowingEvent;
    FOnTableOptionsFormShowing: TdxTableOptionsFormShowingEvent;
    FOnInsertTableFormShowing: TdxInsertTableFormShowingEvent;
    FOnSymbolFormShowing: TdxSymbolFormShowingEvent;
    FOnNumberingListFormShowing: TdxNumberingListFormShowingEvent;
    FOnFloatingInlineObjectLayoutOptionsFormShowing: TdxFloatingInlineObjectLayoutOptionsFormShowingEvent;
    FOnSearchFormShowing: TdxSearchFormShowingEvent;
    FOnLineNumberingFormShowing: TdxLineNumberingFormShowingEvent;
    FOnPageSetupFormShowing: TdxPageSetupFormShowingEvent;
    FOnInsertMergeFieldFormShowing: TdxInsertMergeFieldFormShowingEvent;
    FOnBookmarkFormShowing: TdxBookmarkFormShowingEvent;
    FOnMergeDatabaseRecordsFormShowing: TdxMergeDatabaseRecordsFormShowingEvent;
    FOnTableStyleFormShowing: TdxTableStyleFormShowingEvent;
    FOnDocumentProtectionQuerySetPasswordFormShowing: TdxDocumentProtectionSetPasswordFormShowingEvent;
    FOnDocumentProtectionQueryGetPasswordFormShowing: TdxDocumentProtectionGetPasswordFormShowingEvent;
    FOnDocumentEncryptionQueryPassword: TdxRichEditControlQueryEncryptionPasswordEvent;
    FOnRangeEditingPermissionsFormShowing: TdxRangeEditingPermissionsFormShowingEvent;
    FOnDocumentEncryptQueryNewPasswordFormShowing: TdxDocumentEncryptSetPasswordFormShowingEvent;
    function GetOptions: TdxRichEditControlOptions;
    function GetViews: TdxRichEditViewRepository;
    procedure SetOptions(const Value: TdxRichEditControlOptions);
    procedure SetViews(const Value: TdxRichEditViewRepository);
  protected
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer; overload; override;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase}; override;

    procedure ActivateViewPlatformSpecific(AView: TdxRichEditView); override;
    function CreateViewRepository: TdxRichEditCustomViewRepository; override;

    function CreateSearchForm(AControllerParameters: TdxFormControllerParameters): TForm; virtual;
    procedure SearchFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure InsertMergeFieldFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI; const ACallback: TdxShowColumnsSetupFormCallback;
      ACallbackData: TObject); override;
    procedure ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload; override;
    procedure ShowEditStyleForm(ACharacterSourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload; override;
    procedure ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle; ACharacterSourceStyle: TdxCharacterStyle;
      AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload;
    procedure ShowTableStyleForm(AStyle: TdxTableStyle); override;
    procedure ApplyShowTableStyleFormResults(AStyle: TdxTableStyle);
    procedure ShowFindReplaceForm(AControllerParameters: TdxFormControllerParameters);
    procedure ShowInsertTableForm(const AParameters: TdxCreateTableParameters; const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject); override;
    procedure ShowInsertTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject); override;
    procedure ShowDeleteTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject); override;
    procedure ShowSplitTableCellsForm(const AParameters: TdxSplitTableCellsParameters; const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject); override;
    procedure ShowRangeEditingPermissionsForm; override;
    procedure ShowDocumentEncryptQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); override;
    procedure ShowDocumentProtectionQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); override;
    procedure ShowDocumentProtectionQueryPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback); override;
    procedure ShowBookmarkForm; override;
    procedure ShowReplaceForm; override;
    procedure ShowSearchForm; override;
    procedure ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo; const ATitle: string; const ACallback: TdxShowHyperlinkFormCallback); override;
    procedure ShowSymbolForm(const ASymbolProperties: TdxSymbolProperties; const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject); override;
    procedure ShowNumberingListForm(AParagraphs: TdxParagraphList; const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject); override;
    procedure ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties; const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject); override;
    procedure ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties; const ACallback: TdxShowFontFormCallback; ACallbackData: TObject); override;
    procedure ShowTabsForm(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; const ACallback: TdxShowTabsFormCallback;
      ACallbackData: TObject); override;
    procedure ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection); override;
    procedure ShowFloatingInlineObjectLayoutOptionsForm(const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
      const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject); override;
    procedure ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject = nil); override;
    procedure ShowLineNumberingForm(AProperties: TdxLineNumberingInfo; const ACallback: TdxShowLineNumberingFormCallback;
      ACallbackData: TObject); override;
    procedure ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
      ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage); override;
    procedure ShowInsertMergeFieldForm; override;
    procedure ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const ACallback: TdxShowMergeDatabaseRecordsFormCallback); override;
    procedure ShowTOCForm(AField: TdxField); override;

    function GetSkinLeftMargin: Integer; override;
    function GetSkinRightMargin: Integer; override;
    function GetSkinTopMargin: Integer; override;
    function GetSkinBottomMargin: Integer; override;
    function GetCanShowNumberingListForm: Boolean; override;
    procedure RedrawEnsureSecondaryFormattingComplete(Action: TdxRefreshAction); override;

    function CreateInnerControl: TdxInnerRichEditControl; override;
    procedure DoBeforeExport(Sender: TObject; Args: TdxBeforeExportEventArgs); override;

    procedure DoParagraphFormShowing(const AArgs: TdxParagraphFormShowingEventArgs);
    procedure DoNumberingListFormShowing(const AArgs: TdxNumberingListFormShowingEventArgs);
    procedure DoSymbolFormShowing(const AArgs: TdxSymbolFormShowingEventArgs);
    procedure DoFontFormShowing(const AArgs: TdxFontFormShowingEventArgs);
    procedure DoTablePropertiesFormShowing(const AArgs: TdxTablePropertiesFormShowingEventArgs);
    procedure DoTableOptionsFormShowing(const AArgs: TdxTableOptionsFormShowingEventArgs);
    procedure DoTableStyleFormShowing(const AArgs: TdxTableStyleFormShowingEventArgs);
    procedure DoTabsFormShowing(const AArgs: TdxTabsFormShowingEventArgs);
    procedure DoInsertTableFormShowing(const AArgs: TdxInsertTableFormShowingEventArgs);
    procedure DoSplitTableCellsFormShowing(const AArgs: TdxSplitTableCellsFormShowingEventArgs);
    procedure DoHyperlinkFormShowing(const AArgs: TdxHyperlinkFormShowingEventArgs);
    procedure DoDeleteTableCellsFormShowing(const AArgs: TdxDeleteTableCellsFormShowingEventArgs);
    procedure DoInsertTableCellsFormShowing(const AArgs: TdxInsertTableCellsFormShowingEventArgs);
    procedure DoColumnsSetupFormShowing(const AArgs: TdxColumnsSetupFormShowingEventArgs);
    procedure DoEditStyleFormShowing(const AArgs: TdxEditStyleFormShowingEventArgs);
    procedure DoFloatingInlineObjectLayoutOptionsFormShowing(const AArgs: TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs);
    procedure DoSearchFormShowing(const AArgs: TdxSearchFormShowingEventArgs);
    procedure DoLineNumberingFormShowing(const AArgs: TdxLineNumberingFormShowingEventArgs);
    procedure DoPageSetupFormShowing(const AArgs: TdxPageSetupFormShowingEventArgs);
    procedure DoInsertMergeFieldFormShowing(const AArgs: TdxInsertMergeFieldFormShowingEventArgs);
    procedure DoBookmarkFormShowing(const AArgs: TdxBookmarkFormShowingEventArgs);
    procedure DoMergeDatabaseRecordsFormShowing(const AArgs: TdxMergeDatabaseRecordsFormShowingEventArgs);
    procedure DoDocumentProtectionQueryNewPasswordFormShowing(const AArgs: TdxDocumentProtectionSetPasswordFormShowingEventArgs);
    procedure DoDocumentProtectionQueryPasswordFormShowing(const AArgs: TdxDocumentProtectionGetPasswordFormShowingEventArgs);
    procedure DoRangeEditingPermissionsFormShowing(const AArgs: TdxRangeEditingPermissionsFormShowingEventArgs);
    function GetDocumentEncryptionPassword(var APassword: string): Boolean; virtual;
    procedure DoDocumentEncryptQueryNewPasswordFormShowing(const AArgs: TdxDocumentEncryptSetPasswordFormShowingEventArgs);
    procedure DisposeCommon; override;

    procedure OnLookAndFeelChanged; override;
    procedure ScaleFactorChanged; override;
    function CreateHorizontalRuler: IdxRulerControl; override;
    function CreateVerticalRuler: IdxRulerControl; override;
    function CalculateVerticalRulerWidth: Integer; virtual;
    function CalculateHorizontalRulerHeight: Integer; virtual;
    procedure PerformRulersResize; override;
    function ShouldUpdateRulers: Boolean; virtual;
    procedure InitializeRulers; override;
    procedure UpdateVerticalRuler; override;
    procedure UpdateHorizontalRuler; override;
    procedure UpdateRulersCore; override;
    procedure UpdateRulers; override;
    procedure UpdateHorizontalRulerCore; virtual;
    procedure UpdateVerticalRulerCore; virtual;

    property HorizontalRuler: TdxCustomRichEditRulerControl read FHorizontalRuler;
    property VerticalRuler: TdxCustomRichEditRulerControl read FVerticalRuler;
    property SearchForm: TForm read FSearchForm;
  public
    // for internal use
    function CalculateVerticalRulerVisibility: Boolean; virtual;
    function CalculateHorizontalRulerVisibility: Boolean; virtual;

    property Views: TdxRichEditViewRepository read GetViews write SetViews;
    property Options: TdxRichEditControlOptions read GetOptions write SetOptions;
    property OnBeforeExport: TdxRichEditControlBeforeExportEvent read FOnBeforeExport write FOnBeforeExport;

    property OnParagraphFormShowing: TdxParagraphFormShowingEvent read FOnParagraphFormShowing write FOnParagraphFormShowing;
    property OnNumberingListFormShowing: TdxNumberingListFormShowingEvent read FOnNumberingListFormShowing write FOnNumberingListFormShowing;
    property OnSymbolFormShowing: TdxSymbolFormShowingEvent read FOnSymbolFormShowing write FOnSymbolFormShowing;
    property OnFontFormShowing: TdxFontFormShowingEvent read FOnFontFormShowing write FOnFontFormShowing;
    property OnTablePropertiesFormShowing: TdxTablePropertiesFormShowingEvent read FOnTablePropertiesFormShowing write FOnTablePropertiesFormShowing;
    property OnTableOptionsFormShowing: TdxTableOptionsFormShowingEvent read FOnTableOptionsFormShowing write FOnTableOptionsFormShowing;
    property OnTabsFormShowing: TdxTabsFormShowingEvent read FOnTabsFormShowing write FOnTabsFormShowing;
    property OnInsertTableFormShowing: TdxInsertTableFormShowingEvent read FOnInsertTableFormShowing write FOnInsertTableFormShowing;
    property OnSplitTableCellsFormShowing: TdxSplitTableCellsFormShowingEvent read FOnSplitTableCellsFormShowing write FOnSplitTableCellsFormShowing;
    property OnHyperlinkFormShowing: TdxHyperlinkFormShowingEvent read FOnHyperlinkFormShowing write FOnHyperlinkFormShowing;
    property OnDeleteTableCellsFormShowing: TdxDeleteTableCellsFormShowingEvent read FOnDeleteTableCellsFormShowing write FOnDeleteTableCellsFormShowing;
    property OnInsertTableCellsFormShowing: TdxInsertTableCellsFormShowingEvent read FOnInsertTableCellsFormShowing write FOnInsertTableCellsFormShowing;
    property OnColumnsSetupFormShowing: TdxColumnsSetupFormShowingEvent read FOnColumnsSetupFormShowing write FOnColumnsSetupFormShowing;
    property OnEditStyleFormShowing: TdxEditStyleFormShowingEvent read FOnEditStyleFormShowing write FOnEditStyleFormShowing;
    property OnFloatingInlineObjectLayoutOptionsFormShowing: TdxFloatingInlineObjectLayoutOptionsFormShowingEvent read FOnFloatingInlineObjectLayoutOptionsFormShowing write FOnFloatingInlineObjectLayoutOptionsFormShowing;
    property OnSearchFormShowing: TdxSearchFormShowingEvent read FOnSearchFormShowing write FOnSearchFormShowing;
    property OnLineNumberingFormShowing: TdxLineNumberingFormShowingEvent read FOnLineNumberingFormShowing write FOnLineNumberingFormShowing;
    property OnPageSetupFormShowing: TdxPageSetupFormShowingEvent read FOnPageSetupFormShowing write FOnPageSetupFormShowing;
    property OnInsertMergeFieldFormShowing: TdxInsertMergeFieldFormShowingEvent read FOnInsertMergeFieldFormShowing write FOnInsertMergeFieldFormShowing;
    property OnBookmarkFormShowing: TdxBookmarkFormShowingEvent read FOnBookmarkFormShowing write FOnBookmarkFormShowing;
    property OnMergeDatabaseRecordsFormShowing: TdxMergeDatabaseRecordsFormShowingEvent read FOnMergeDatabaseRecordsFormShowing write FOnMergeDatabaseRecordsFormShowing;
    property OnTableStyleFormShowing: TdxTableStyleFormShowingEvent read FOnTableStyleFormShowing write FOnTableStyleFormShowing;
    property OnDocumentProtectionQuerySetPasswordFormShowing: TdxDocumentProtectionSetPasswordFormShowingEvent read FOnDocumentProtectionQuerySetPasswordFormShowing write FOnDocumentProtectionQuerySetPasswordFormShowing;
    property OnDocumentProtectionQueryGetPasswordFormShowing: TdxDocumentProtectionGetPasswordFormShowingEvent read FOnDocumentProtectionQueryGetPasswordFormShowing write FOnDocumentProtectionQueryGetPasswordFormShowing;
    property OnDocumentEncryptionQueryPassword: TdxRichEditControlQueryEncryptionPasswordEvent read FOnDocumentEncryptionQueryPassword write FOnDocumentEncryptionQueryPassword;
    property OnRangeEditingPermissionsFormShowing: TdxRangeEditingPermissionsFormShowingEvent read FOnRangeEditingPermissionsFormShowing write FOnRangeEditingPermissionsFormShowing;
    property OnDocumentEncryptQueryNewPasswordFormShowing: TdxDocumentEncryptSetPasswordFormShowingEvent read FOnDocumentEncryptQueryNewPasswordFormShowing write FOnDocumentEncryptQueryNewPasswordFormShowing;
  end;

  { TdxRichEditControl }

  TdxRichEditControl = class(TdxCustomRichEditControl)
  published
    property ActiveViewType;
    property Align;
    property Anchors;
    property AutoSizeMode;
    property BorderStyle default cxcbsDefault;
    property Color;
    property DragMode;
    property Enabled;
    property Touch;
    property LookAndFeel;
    property Options;
    property PopupMenu;
    property ReadOnly;
    property ShowCaretInReadOnly;
    property Views;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property MeasurementUnit;
    property WantReturns;
    property WantTabs;

    property OnBeforeExport;
    property OnAfterExport;
    property OnActiveViewChanged;
    property OnDocumentClosing;
    property OnContentChanged;
    property OnContextPopup;
    property OnDocumentLoaded;
    property OnDocumentProtectionChanged;
    property OnEmptyDocumentCreated;
    property OnHyperlinkClick;
    property OnModifiedChanged;
    property OnPageBackgroundChanged;
    property OnPlainTextChanged;
    property OnReadOnlyChanged;
    property OnSelectionChanged;
    property OnShortCut;
    property OnZoomChanged;

    property OnCalculateDocumentVariable;
    property OnCustomizeMergeFields;
    property OnMailMergeFinished;
    property OnMailMergeRecordFinished;
    property OnMailMergeRecordStarted;
    property OnMailMergeStarted;
    property OnMailMergeGetTargetDocument;
    // dialogs
    property OnBookmarkFormShowing;
    property OnColumnsSetupFormShowing;
    property OnDeleteTableCellsFormShowing;
    property OnEditStyleFormShowing;
    property OnFloatingInlineObjectLayoutOptionsFormShowing;
    property OnFontFormShowing;
    property OnHyperlinkFormShowing;
    property OnInsertMergeFieldFormShowing;
    property OnInsertTableCellsFormShowing;
    property OnInsertTableFormShowing;
    property OnLineNumberingFormShowing;
    property OnMergeDatabaseRecordsFormShowing;
    property OnNumberingListFormShowing;
    property OnParagraphFormShowing;
    property OnSearchFormShowing;
    property OnSplitTableCellsFormShowing;
    property OnSymbolFormShowing;
    property OnTableOptionsFormShowing;
    property OnTablePropertiesFormShowing;
    property OnTabsFormShowing;
    property OnDocumentProtectionQuerySetPasswordFormShowing;
    property OnDocumentProtectionQueryGetPasswordFormShowing;
    property OnDocumentEncryptionQueryPassword;
    property OnRangeEditingPermissionsFormShowing;

    property OnClick;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnEndDock;
    property OnExit;
    property OnEnter;
    property OnFocusChanged;
    property OnGesture;
    property OnKeyUp;
    property OnKeyPress;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnStartDock;
  end;

implementation

uses
  Contnrs, Math, Dialogs, ShellAPI, ComObj, ShlObj, UxTheme, DwmApi, dxTypeHelpers, dxCore,
  cxScrollBar,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Api.MailMerge,
  dxRichEdit.Rtf,
  dxRichEdit.PlainText,
  dxRichEdit.DocumentLayout,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.InnerControl.DrawingStrategy,
  dxRichEdit.Options,
  dxRichEdit.Ruler,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Commands.Dialogs,
  dxRichEdit.Commands.FindAndReplace,
  dxRichEdit.Commands.FileOperations,
  dxRichEdit.Commands.Hyperlink,
  dxRichEdit.Dialogs.FindAndReplaceFormHelpers,
  dxRichEdit.Dialogs.NumberingFormController,
  dxRichEdit.Dialogs.Numbering,
  dxRichEdit.Dialogs.ParagraphFormController,
  dxRichEdit.Dialogs.Paragraph,
  dxRichEdit.Dialogs.SymbolFormController,
  dxRichEdit.Dialogs.Symbol,
  dxRichEdit.Dialogs.FontsFormController,
  dxRichEdit.Dialogs.Fonts,
  dxRichEdit.Dialogs.TablePropertiesFormController,
  dxRichEdit.Dialogs.TableProperties,
  dxRichEdit.Dialogs.TableOptionsController,
  dxRichEdit.Dialogs.TableOptions,
  dxRichEdit.Dialogs.SearchText,
  dxRichEdit.Dialogs.TabsFormController,
  dxRichEdit.Dialogs.Tabs,
  dxRichEdit.Dialogs.InsertTableFormController,
  dxRichEdit.Dialogs.InsertTable,
  dxRichEdit.Dialogs.SplitTableCellsFormController,
  dxRichEdit.Dialogs.SplitTableCells,
  dxRichEdit.Dialogs.HyperlinkFormController,
  dxRichEdit.Dialogs.Hyperlink,
  dxRichEdit.Dialogs.InsertDeleteTableCellsFormController,
  dxRichEdit.Dialogs.DeleteTableCells,
  dxRichEdit.Dialogs.InsertTableCells,
  dxRichEdit.Dialogs.CustomInsertDeleteTableCells,
  dxRichEdit.Dialogs.ColumnsSetupFormController,
  dxRichEdit.Dialogs.ColumnsSetup,
  dxRichEdit.Dialogs.EditStyleController,
  dxRichEdit.Dialogs.EditStyle,
  dxRichEdit.Dialogs.FloatingObjectLayoutFormController,
  dxRichEdit.Dialogs.FloatingObjectLayoutForm,
  dxRichEdit.Dialogs.LineNumberingController,
  dxRichEdit.Dialogs.LineNumbering,
  dxRichEdit.Dialogs.PageSetupController,
  dxRichEdit.Dialogs.PageSetup,
  dxRichEdit.Dialogs.RangeEditingPermissionsFormController,
  dxRichEdit.Dialogs.RangeEditingPermissions,
  dxRichEdit.Dialogs.QueryPassword,
  dxRichEdit.Dialogs.InsertMergeField,
  dxRichEdit.Dialogs.Bookmark,
  dxRichEdit.Dialogs.BookmarkFormController,
  dxRichEdit.Dialogs.MergeOptions,
  dxRichEdit.Dialogs.MergeOptionsController,
  dxRichEdit.Dialogs.TableOfContents,
  dxRichEdit.Dialogs.TableOfContentsController,
  dxRichEdit.Dialogs.TableStyle,
  dxRichEdit.Dialogs.TableStyleFormController,

  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.FormControllers;

{ TdxRichEditControlKeyboardDefaultHandler }

procedure TdxRichEditControlKeyboardDefaultHandler.PopulateDialogsCommandTable;
begin
  inherited PopulateDialogsCommandTable;
  RegisterKeyCommand('D', [ssCtrl], TdxShowFontFormCommand);
  RegisterKeyCommand('S', [ssCtrl], TdxSaveDocumentCommand);
  RegisterKeyCommand(VK_F12, [], TdxSaveDocumentAsCommand);
  RegisterKeyCommand('K', [ssCtrl], TdxShowHyperlinkFormCommand);
  RegisterKeyCommand('O', [ssCtrl], TdxLoadDocumentCommand);
  RegisterKeyCommand('N', [ssCtrl], TdxCreateEmptyDocumentCommand);
  RegisterKeyCommand(VK_F3, [], TdxFindAndSelectForwardCommand);
  RegisterKeyCommand(VK_F3, [ssShift], TdxFindAndSelectBackwardCommand);
  RegisterKeyCommand('F', [ssCtrl], TdxFindCommand);
  RegisterKeyCommand('H', [ssCtrl], TdxReplaceCommand);
end;

{ TdxRichEditControlKeyboardController }

function TdxRichEditControlKeyboardController.CreateDefaultHandler: IdxKeyboardHandlerService;
begin
  Result := TdxRichEditControlKeyboardDefaultHandler.Create(Self);
end;

{ TdxRichEditInnerControl }

function TdxRichEditInnerControl.CreateKeyboardController: TdxCustomKeyboardController;
begin
  Result := TdxRichEditControlKeyboardController.Create(Self);
end;

function TdxRichEditInnerControl.GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean;
begin
  Result := TdxCustomRichEditControl(Control).GetDocumentEncryptionPassword(APassword);
  if not Result then
    Result := ShowQueryPasswordDialog(TdxQueryPasswordKind.Query, APassword);
end;

procedure TdxRichEditInnerControl.DoShortCut(Args: TdxRichEditShortCutEventArgs);
begin
  inherited DoShortCut(Args);
  TdxCustomRichEditControl(Owner.Control).DoShortCut(Args);
end;

procedure TdxRichEditInnerControl.PopulateCommands;
begin
  inherited PopulateCommands;
  RegisterCommand(TdxReplaceForwardCommand);
  RegisterCommand(TdxReplaceAllForwardCommand);
  RegisterCommand(TdxShowFontFormCommand);
  RegisterCommand(TdxShowParagraphFormCommand);
  RegisterCommand(TdxShowSymbolFormCommand);
  RegisterCommand(TdxShowNumberingListFormCommand);
  RegisterCommand(TdxSetPortraitPageOrientationCommand);
  RegisterCommand(TdxSetLandscapePageOrientationCommand);
  RegisterCommand(TdxChangeSectionPaperKindCommand);
  RegisterCommand(TdxShowInsertTableCellsFormCommand);
  RegisterCommand(TdxShowDeleteTableCellsFormCommand);
  RegisterCommand(TdxShowSplitTableCellsFormCommand);
  RegisterCommand(TdxShowSplitTableCellsFormMenuCommand);
  RegisterCommand(TdxShowDeleteTableCellsFormMenuCommand);
  RegisterCommand(TdxShowLineNumberingFormCommand);
  RegisterCommand(TdxShowPageSetupFormCommand);
  RegisterCommand(TdxShowColumnsSetupFormCommand);
  RegisterCommand(TdxShowTablePropertiesFormCommand);
  RegisterCommand(TdxShowTablePropertiesFormMenuCommand);
  RegisterCommand(TdxShowPageMarginsSetupFormCommand);
  RegisterCommand(TdxShowPagePaperSetupFormCommand);
  RegisterCommand(TdxShowEditStyleFormCommand);
  RegisterCommand(TdxShowFloatingObjectLayoutOptionsFormCommand);
  RegisterCommand(TdxShowTOCFormCommand);
  RegisterCommand(TdxShowMergeDatabaseRecordsFormCommand);
  RegisterCommand(TdxShowTableStyleFormCommand);
end;

{ TdxRichEditInnerDocumentServer }

function TdxRichEditInnerDocumentServer.CreateNativeDocument: IdxRichEditDocument;
begin
  Result := TdxNativeDocument.Create(DocumentModel.MainPieceTable, Self);
end;

function TdxRichEditInnerDocumentServer.CreateNativeSubDocument(APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, Self);
end;

function TdxRichEditInnerDocumentServer.GetEncryptionPassword(Sender: TObject; var APassword: string): Boolean;
begin
  Result := ShowQueryPasswordDialog(TdxQueryPasswordKind.Query, APassword);
end;

function TdxRichEditInnerDocumentServer.GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions(AOptions).GetInternalMailMergeOptions;
end;

function TdxRichEditInnerDocumentServer.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions.Create;
end;

{ TdxRichEditInternalDocumentServer }

function TdxRichEditInternalDocumentServer.CreateInnerServer(
  ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer;
begin
  if ADocumentModel = nil then
    Result := TdxRichEditInnerDocumentServer.Create(Self)
  else
    Result := TdxRichEditInnerDocumentServer.Create(Self, ADocumentModel);
end;

{ TdxRichEditViewRepository }

procedure TdxRichEditViewRepository.CreateViews;
begin
  AddView(TdxPrintLayoutView.Create(RichEditControl));
  AddView(TdxSimpleView.Create(RichEditControl));
  AddView(TdxDraftView.Create(RichEditControl));
end;

function TdxRichEditViewRepository.GetPrintLayoutView: TdxPrintLayoutView;
begin
  Result := Views[0] as TdxPrintLayoutView;
end;

function TdxRichEditViewRepository.GetSimpleView: TdxSimpleView;
begin
  Result := Views[1] as TdxSimpleView;
end;

function TdxRichEditViewRepository.GetDraftView: TdxDraftView;
begin
  Result := Views[2] as TdxDraftView;
end;

procedure TdxRichEditViewRepository.SetPrintLayoutView(const Value: TdxPrintLayoutView);
begin
  (Views[0] as TdxPrintLayoutView).Assign(Value);
end;

procedure TdxRichEditViewRepository.SetSimpleView(const Value: TdxSimpleView);
begin
  (Views[1] as TdxSimpleView).Assign(Value);
end;

procedure TdxRichEditViewRepository.SetDraftView(const Value: TdxDraftView);
begin
  (Views[2] as TdxSimpleView).Assign(Value);
end;

{ TdxCustomRichEditControl }

procedure TdxCustomRichEditControl.ShowSearchForm;
var
  AControllerParameters: TdxSearchFormControllerParameters;
begin
  AControllerParameters := TdxSearchFormControllerParameters.Create(Self, TdxSearchFormActivePage.Find);
  try
    ShowFindReplaceForm(AControllerParameters);
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowSplitTableCellsForm(const AParameters: TdxSplitTableCellsParameters;
  const ACallback: TdxShowSplitTableCellsFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxSplitTableCellsFormControllerParameters;
  AArgs: TdxSplitTableCellsFormShowingEventArgs;
  AForm: TdxRichEditSplitTableCellsDialogForm;
begin
  AControllerParameters := TdxSplitTableCellsFormControllerParameters.Create(Self, AParameters);
  AArgs := TdxSplitTableCellsFormShowingEventArgs.Create(AControllerParameters);
  try
    DoSplitTableCellsFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditSplitTableCellsDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceParameters, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
    begin
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.Parameters, ACallbackData);
    end;
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowRangeEditingPermissionsForm;
var
  AControllerParameters: TdxRangeEditingPermissionsFormControllerParameters;
  AArgs: TdxRangeEditingPermissionsFormShowingEventArgs;
  AForm: TdxRangeEditingPermissionsForm;
begin
  AControllerParameters := TdxRangeEditingPermissionsFormControllerParameters.Create(Self);
  try
    AArgs := TdxRangeEditingPermissionsFormShowingEventArgs.Create(AControllerParameters);
    try
      DoRangeEditingPermissionsFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRangeEditingPermissionsForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowReplaceForm;
var
  AControllerParameters: TdxSearchFormControllerParameters;
begin
  AControllerParameters := TdxSearchFormControllerParameters.Create(Self, TdxSearchFormActivePage.Replace);
  try
    ShowFindReplaceForm(AControllerParameters);
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowNumberingListForm(AParagraphs: TdxParagraphList;
  const ACallback: TdxShowNumberingListFormCallback; ACallbackData: TObject);
var
  AForm: TdxRichEditNumberingListDialogForm;
  AControllerParameters: TdxNumberingListFormControllerParameters;
  AArgs: TdxNumberingListFormShowingEventArgs;
begin
  AControllerParameters := TdxNumberingListFormControllerParameters.Create(Self, AParagraphs);
  try
    AArgs := TdxNumberingListFormShowingEventArgs.Create(AControllerParameters);
    try
      DoNumberingListFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditNumberingListDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if AForm.ShowModal = mrOk then
            ACallback(AParagraphs, ACallbackData);
        finally
          AForm.Free;
        end;
      end
      else
      begin
        if AArgs.DialogResult = mrOk then
          ACallback(AParagraphs, ACallbackData);
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
  ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage);
var
  AControllerParameters: TdxPageSetupFormControllerParameters;
  AArgs: TdxPageSetupFormShowingEventArgs;
  AForm: TdxRichEditPageSetupDialogForm;
begin
  AControllerParameters := TdxPageSetupFormControllerParameters.Create(Self, AProperties);
  try
    AControllerParameters.InitialTabPage := AInitialTabPage;
    AArgs := TdxPageSetupFormShowingEventArgs.Create(AControllerParameters);
    try
      DoPageSetupFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditPageSetupDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if AForm.ShowModal = mrOk then
            ACallback(AForm.Controller.SourcePageSetupInfo, ACallbackData);
        finally
          AForm.Free;
        end;
      end
      else
      begin
        if AArgs.DialogResult = mrOK then
          ACallback(AControllerParameters.PageSetupInfo, ACallbackData);
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties;
  const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject);
var
  AForm: TdxRichEditParagraphDialogForm;
  AArgs: TdxParagraphFormShowingEventArgs;
  AControllerParameters: TdxParagraphFormControllerParameters;
  AResult: TModalResult;
begin
  AControllerParameters := TdxParagraphFormControllerParameters.Create(Self, AParagraphProperties, DocumentModel.UnitConverter);
  try
    AArgs := TdxParagraphFormShowingEventArgs.Create(AControllerParameters);
    try
      DoParagraphFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditParagraphDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AResult := AForm.ShowModal;
          if AResult = mrOk then
            ACallback(AForm.Controller.SourceProperties, ACallbackData);
        finally
          AForm.Free;
        end;
      end
      else
      begin
        AResult := AArgs.DialogResult;
        if AResult = mrOk then
          ACallback(AControllerParameters.ParagraphProperties, ACallbackData);
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowFindReplaceForm(AControllerParameters: TdxFormControllerParameters);
var
  AArgs: TdxSearchFormShowingEventArgs;
begin
  AArgs := TdxSearchFormShowingEventArgs.Create(AControllerParameters);
  try
    DoSearchFormShowing(AArgs);
    if AArgs.Handled then
      Exit;
  finally
    AArgs.Free;
  end;

  if FSearchForm = nil then
  begin
    FSearchForm := CreateSearchForm(AControllerParameters);
    FSearchForm.OnClose := SearchFormClosed;
  end;
  TdxRichEditSearchTextDialogForm(FSearchForm).ActivePage := TdxSearchFormControllerParameters(AControllerParameters).ActivePage;
  FSearchForm.Show;
end;

procedure TdxCustomRichEditControl.ShowFloatingInlineObjectLayoutOptionsForm(
  const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
  const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxFloatingInlineObjectLayoutOptionsFormControllerParameters;
  AArgs: TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs;
  AResult: TModalResult;
  AForm: TdxFloatingObjectLayoutDialogForm;
begin
  AControllerParameters := TdxFloatingInlineObjectLayoutOptionsFormControllerParameters.Create(Self, AFloatingObjectParameters);
  try
    AArgs := TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs.Create(AControllerParameters);
    try
      DoFloatingInlineObjectLayoutOptionsFormShowing(AArgs);
      if AArgs.Handled then
        AResult := AArgs.DialogResult
      else
      begin
        AForm := TdxFloatingObjectLayoutDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AResult := AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
      if (AResult = mrOK) and Assigned(ACallback) then
        ACallback(AControllerParameters.FloatingInlineObjectParameters, ACallbackData);
    finally
      AArgs.Free
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties;
  const ACallback: TdxShowFontFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxFontFormControllerParameters;
  AArgs: TdxFontFormShowingEventArgs;
  AForm: TdxRichEditFontDialogForm;
begin
  AControllerParameters := TdxFontFormControllerParameters.Create(Self, ACharacterProperties);
  try
    AArgs := TdxFontFormShowingEventArgs.Create(AControllerParameters);
    try
      DoFontFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditFontDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if (AForm.ShowModal = mrOk) and Assigned(ACallback) then
            ACallback(AForm.Controller.SourceCharacterProperties, ACallbackData);
        finally
          AForm.Free;
        end;
      end
      else
        if (AArgs.DialogResult = mrOK) and Assigned(ACallback) then
           ACallback(AControllerParameters.SourceCharacterProperties, ACallbackData);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowHyperlinkForm(AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo;
  const ATitle: string; const ACallback: TdxShowHyperlinkFormCallback);
var
  AControllerParameters: TdxHyperlinkFormControllerParameters;
  AArgs: TdxHyperlinkFormShowingEventArgs;
  AForm: TdxRichEditHyperlinkDialogForm;
begin
  AControllerParameters := TdxHyperlinkFormControllerParameters.Create(Self, AHyperlinkInfo, ARunInfo);
  AArgs := TdxHyperlinkFormShowingEventArgs.Create(AControllerParameters);
  try
    DoHyperlinkFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditHyperlinkDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        AForm.Caption := ATitle;
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.HyperlinkInfo.Clone, AForm.Controller.TextSource, ARunInfo, AForm.Controller.TextToDisplay);
      finally
        AForm.Free;
      end;
    end
    else
    begin
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.HyperlinkInfo.Clone, AControllerParameters.TextSource, ARunInfo, AControllerParameters.TextToDisplay);
    end;
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowInsertMergeFieldForm;
var
  AControllerParameters: TdxInsertMergeFieldFormControllerParameters;
  AArgs: TdxInsertMergeFieldFormShowingEventArgs;
begin
  if FInsertMergeFieldForm <> nil then
  begin
    FInsertMergeFieldForm.Show;
    Exit;
  end;

  AControllerParameters := TdxInsertMergeFieldFormControllerParameters.Create(Self);
  try
    AArgs := TdxInsertMergeFieldFormShowingEventArgs.Create(AControllerParameters);
    try
      DoInsertMergeFieldFormShowing(AArgs);
      if AArgs.Handled then
        Exit;
    finally
      AArgs.Free;
    end;
    FInsertMergeFieldForm := TdxRichEditInsertMergeFieldForm.Create(Self);
    TdxRichEditInsertMergeFieldForm(FInsertMergeFieldForm).Initialize(AControllerParameters);
    FInsertMergeFieldForm.OnClose := InsertMergeFieldFormClosed;
    FInsertMergeFieldForm.Show;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowInsertTableCellsForm(const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxInsertTableCellsFormControllerParameters;
  AArgs: TdxInsertTableCellsFormShowingEventArgs;
  AForm: TdxRichEditInsertTableCellsDialogForm;
begin
  AControllerParameters := TdxInsertTableCellsFormControllerParameters.Create(Self, AParameters);
  AArgs := TdxInsertTableCellsFormShowingEventArgs.Create(AControllerParameters);
  try
    DoInsertTableCellsFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditInsertTableCellsDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceParameters, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.CellsParameters, ACallbackData);
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowInsertTableForm(const AParameters: TdxCreateTableParameters;
  const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxInsertTableFormControllerParameters;
  AArgs: TdxInsertTableFormShowingEventArgs;
  AForm: TdxRichEditInsertTableForm;
begin
  AControllerParameters := TdxInsertTableFormControllerParameters.Create(Self, AParameters);
  AArgs := TdxInsertTableFormShowingEventArgs.Create(AControllerParameters);
  try
    DoInsertTableFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditInsertTableForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceParameters, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
    begin
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.Parameters, ACallbackData);
    end;
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowLineNumberingForm(AProperties: TdxLineNumberingInfo;
  const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxLineNumberingFormControllerParameters;
  AArgs: TdxLineNumberingFormShowingEventArgs;
  AForm: TdxRichEditLineNumberingDialogForm;
begin
  AControllerParameters := TdxLineNumberingFormControllerParameters.Create(Self, AProperties);
  try
    AArgs := TdxLineNumberingFormShowingEventArgs.Create(AControllerParameters);
    try
      DoLineNumberingFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditLineNumberingDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if AForm.ShowModal = mrOk then
            ACallback(AForm.Controller.SourceLineNumberingInfo, ACallbackData);
        finally
          AForm.Free;
        end;
      end
      else
        if AArgs.DialogResult = mrOK then
          ACallback(AControllerParameters.LineNumberingInfo, ACallbackData);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;


procedure TdxCustomRichEditControl.ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
  const ACallback: TdxShowMergeDatabaseRecordsFormCallback);
var
  AForm: TdxRichEditMergeOptionsDialogForm;
  AArgs: TdxMergeDatabaseRecordsFormShowingEventArgs;
  AControllerParameters: TdxMergeOptionsFormControllerParameters;
begin
  AControllerParameters := TdxMergeOptionsFormControllerParameters.Create(Self, AMergeRecordsParameters);
  try
    AArgs := TdxMergeDatabaseRecordsFormShowingEventArgs.Create(AControllerParameters);
    try
      DoMergeDatabaseRecordsFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditMergeOptionsDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if AForm.ShowModal = mrOk then
            ACallback(AForm.Controller.MergeRecordsParameters, nil);
        finally
          AForm.Free;
        end;
      end
      else
        if AArgs.DialogResult = mrOK then
          ACallback(AControllerParameters.MergeRecordsParameters, nil);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowSymbolForm(const ASymbolProperties: TdxSymbolProperties;
  const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
var
  AForm: TdxRichEditSymbolDialogForm;
  AArgs: TdxSymbolFormShowingEventArgs;
  AControllerParameters: TdxRichEditInsertSymbolControllerParameters;
begin
  AControllerParameters := TdxRichEditInsertSymbolControllerParameters.Create(Self,  ASymbolProperties, ACallback, ACallbackData);
  try
    AArgs := TdxSymbolFormShowingEventArgs.Create(AControllerParameters);
    try
      DoSymbolFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditSymbolDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject);
var
  AControllerParameters: TdxTableOptionsFormControllerParameters;
  AArgs: TdxTableOptionsFormShowingEventArgs;
  AForm: TdxRichEditTableOptionsDialogForm;
  AOwnerForm: TComponent;
begin
  AControllerParameters := TdxTableOptionsFormControllerParameters.Create(Self, ATable);
  try
    AArgs := TdxTableOptionsFormShowingEventArgs.Create(AControllerParameters);
    try
      DoTableOptionsFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        if Assigned(AOwner) and (AOwner is TComponent) then
          AOwnerForm := AOwner as TComponent
        else
          AOwnerForm := Self;
        AForm := TdxRichEditTableOptionsDialogForm.Create(AOwnerForm);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

function TdxCustomRichEditControl.GetSkinLeftMargin: Integer;
begin
  if SkinInfo = nil then
    Result := 19
  else
    Result := TdxPrintLayoutViewSkinPainter.GetSkinEdges(SkinInfo.PrintingPageBorder).Left;
end;

function TdxCustomRichEditControl.GetSkinRightMargin: Integer;
begin
  if SkinInfo = nil then
    Result := 19
  else
    Result := TdxPrintLayoutViewSkinPainter.GetSkinEdges(SkinInfo.PrintingPageBorder).Right;
end;

function TdxCustomRichEditControl.GetSkinTopMargin: Integer;
begin
  if SkinInfo = nil then
    Result := 17
  else
    Result := TdxPrintLayoutViewSkinPainter.GetSkinEdges(SkinInfo.PrintingPageBorder).Top;
end;

function TdxCustomRichEditControl.GetSkinBottomMargin: Integer;
begin
  if SkinInfo = nil then
    Result := 21
  else
    Result := TdxPrintLayoutViewSkinPainter.GetSkinEdges(SkinInfo.PrintingPageBorder).Bottom;
end;

procedure TdxCustomRichEditControl.ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection);
var
  AForm: TdxRichEditTablePropertiesDialogForm;
  AControllerParameters: TdxTablePropertiesFormControllerParameters;
  AArgs: TdxTablePropertiesFormShowingEventArgs;
begin
  AControllerParameters := TdxTablePropertiesFormControllerParameters.Create(Self, ASelectedCells);
  try
    AArgs := TdxTablePropertiesFormShowingEventArgs.Create(AControllerParameters);
    try
      DoTablePropertiesFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditTablePropertiesDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowTableStyleForm(AStyle: TdxTableStyle);
var
  AControllerParameters: TdxTableStyleFormControllerParameters;
  AArgs: TdxTableStyleFormShowingEventArgs;
  AForm: TdxRichEditTableStyleDialogForm;
begin
  AControllerParameters := TdxTableStyleFormControllerParameters.Create(Self, AStyle);
  try
    AArgs := TdxTableStyleFormShowingEventArgs.Create(AControllerParameters);
    try
      DoTableStyleFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditTableStyleDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          if AForm.ShowModal = mrOk then
            ApplyShowTableStyleFormResults(AStyle);
        finally
          AForm.Free;
        end;
      end
      else
        if AArgs.DialogResult = mrOk then
          ApplyShowTableStyleFormResults(AStyle);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowTabsForm(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer;
  const ACallback: TdxShowTabsFormCallback; ACallbackData: TObject);
var
  ACommandState: TdxTabsFormCommandUIState;
  ATabFormOwner: IdxFormOwner;
  AControllerParameters: TdxTabsFormControllerParameters;
  AArgs: TdxTabsFormShowingEventArgs;
  AForm: TdxRichEditTabsDialogForm;
begin
  ACommandState := ACallbackData as TdxTabsFormCommandUIState;
  if Assigned(ACommandState) then
    ATabFormOwner := ACommandState.TabsFormOwner
  else
    ATabFormOwner := nil;
  AControllerParameters := TdxTabsFormControllerParameters.Create(Self, ATabInfo, ADefaultTabWidth,
    DocumentModel.UnitConverter, ATabFormOwner);
  AArgs := TdxTabsFormShowingEventArgs.Create(AControllerParameters);
  try
    DoTabsFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditTabsDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceTabInfo, AForm.Controller.SourceDefaultTabWidth, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.TabInfo, AControllerParameters.DefaultTabWidth, ACallbackData);
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowTOCForm(AField: TdxField);
var
  AControllerParameters: TdxTOCFormControllerParameters;
  AForm: TdxRichEditTableOfContentsForm;
begin
  AControllerParameters := TdxTOCFormControllerParameters.Create(Self, AField);
  try
    AForm := TdxRichEditTableOfContentsForm.Create(Self);
    try
      AForm.Initialize(AControllerParameters);
      AForm.ShowModal;
    finally
      AForm.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ActivateViewPlatformSpecific(AView: TdxRichEditView);
begin
  inherited ActivateViewPlatformSpecific(AView);
  if FHorizontalRuler <> nil then
    FHorizontalRuler.RecreatePainter;
  if FVerticalRuler <> nil then
    FVerticalRuler.RecreatePainter;
end;

function TdxCustomRichEditControl.CreateViewRepository: TdxRichEditCustomViewRepository;
begin
  Result := TdxRichEditViewRepository.Create(Self);
end;

procedure TdxCustomRichEditControl.ApplyShowTableStyleFormResults(AStyle: TdxTableStyle);
var
  AModelStyle: TdxTableStyle;
begin
  DocumentModel.BeginUpdate;
  AModelStyle := DocumentModel.TableStyles.GetStyleByName(AStyle.StyleName) as TdxTableStyle;
  if AModelStyle = nil then
    DocumentModel.TableStyles.AddNewStyle(AStyle)
  else
    AModelStyle.CopyProperties(AStyle);
  DocumentModel.EndUpdate;
end;

function TdxCustomRichEditControl.CreateInnerControl: TdxInnerRichEditControl;
begin
  Result := TdxRichEditInnerControl.Create(Self);
end;

function TdxCustomRichEditControl.CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer;
begin
  Result := TdxRichEditInternalDocumentServer.Create(nil);
end;

procedure TdxCustomRichEditControl.RedrawEnsureSecondaryFormattingComplete(Action: TdxRefreshAction);
begin
  inherited RedrawEnsureSecondaryFormattingComplete(Action);
  if not IsUpdateLocked and (Action = TdxRefreshAction.Transforms) then
  begin
    if HorizontalRuler.IsVisible then
      HorizontalRuler.Repaint;
    if VerticalRuler.IsVisible then
      VerticalRuler.Repaint;
    Refresh;
  end;
end;

procedure TdxCustomRichEditControl.DoBeforeExport(Sender: TObject; Args: TdxBeforeExportEventArgs);
var
  AUri: string;
begin
  if Assigned(FOnBeforeExport) then
  begin
    AUri := Args.Options.TargetUri;
    FOnBeforeExport(Self, Args.Format, AUri);
    Args.Options.TargetUri := AUri;
  end;
end;

procedure TdxCustomRichEditControl.DoDocumentEncryptQueryNewPasswordFormShowing(
  const AArgs: TdxDocumentEncryptSetPasswordFormShowingEventArgs);
begin
  if Assigned(FOnDocumentEncryptQueryNewPasswordFormShowing) then
    FOnDocumentEncryptQueryNewPasswordFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoRangeEditingPermissionsFormShowing(
  const AArgs: TdxRangeEditingPermissionsFormShowingEventArgs);
begin
  if Assigned(FOnRangeEditingPermissionsFormShowing) then
    FOnRangeEditingPermissionsFormShowing(Self, AArgs);
end;

function TdxCustomRichEditControl.GetDocumentEncryptionPassword(var APassword: string): Boolean;
begin
  Result := False;
  if Assigned(FOnDocumentEncryptionQueryPassword) then
    FOnDocumentEncryptionQueryPassword(Self, APassword, Result);
end;

procedure TdxCustomRichEditControl.DoBookmarkFormShowing(const AArgs: TdxBookmarkFormShowingEventArgs);
begin
  if Assigned(FOnBookmarkFormShowing) then
    FOnBookmarkFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoPageSetupFormShowing(const AArgs: TdxPageSetupFormShowingEventArgs);
begin
  if Assigned(FOnPageSetupFormShowing) then
    FOnPageSetupFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoDocumentProtectionQueryNewPasswordFormShowing(
  const AArgs: TdxDocumentProtectionSetPasswordFormShowingEventArgs);
begin
  if Assigned(FOnDocumentProtectionQuerySetPasswordFormShowing) then
    FOnDocumentProtectionQuerySetPasswordFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoDocumentProtectionQueryPasswordFormShowing(
  const AArgs: TdxDocumentProtectionGetPasswordFormShowingEventArgs);
begin
  if Assigned(FOnDocumentProtectionQueryGetPasswordFormShowing) then
    FOnDocumentProtectionQueryGetPasswordFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoMergeDatabaseRecordsFormShowing(
  const AArgs: TdxMergeDatabaseRecordsFormShowingEventArgs);
begin
  if Assigned(FOnMergeDatabaseRecordsFormShowing) then
    FOnMergeDatabaseRecordsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoParagraphFormShowing(const AArgs: TdxParagraphFormShowingEventArgs);
begin
  if Assigned(FOnParagraphFormShowing) then
    FOnParagraphFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoNumberingListFormShowing(const AArgs: TdxNumberingListFormShowingEventArgs);
begin
  if Assigned(FOnNumberingListFormShowing) then
    FOnNumberingListFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoSymbolFormShowing(const AArgs: TdxSymbolFormShowingEventArgs);
begin
  if Assigned(FOnSymbolFormShowing) then
    FOnSymbolFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoFontFormShowing(const AArgs: TdxFontFormShowingEventArgs);
begin
  if Assigned(FOnFontFormShowing) then
    FOnFontFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoTablePropertiesFormShowing(const AArgs: TdxTablePropertiesFormShowingEventArgs);
begin
  if Assigned(FOnTablePropertiesFormShowing) then
    FOnTablePropertiesFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoTableStyleFormShowing(const AArgs: TdxTableStyleFormShowingEventArgs);
begin
  if Assigned(FOnTableStyleFormShowing) then
    FOnTableStyleFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoTableOptionsFormShowing(const AArgs: TdxTableOptionsFormShowingEventArgs);
begin
  if Assigned(FOnTableOptionsFormShowing) then
    FOnTableOptionsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoTabsFormShowing(const AArgs: TdxTabsFormShowingEventArgs);
begin
  if Assigned(FOnTabsFormShowing) then
    FOnTabsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoInsertTableFormShowing(const AArgs: TdxInsertTableFormShowingEventArgs);
begin
  if Assigned(FOnInsertTableFormShowing) then
    FOnInsertTableFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoLineNumberingFormShowing(const AArgs: TdxLineNumberingFormShowingEventArgs);
begin
  if Assigned(FOnLineNumberingFormShowing) then
    FOnLineNumberingFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoSplitTableCellsFormShowing(const AArgs: TdxSplitTableCellsFormShowingEventArgs);
begin
  if Assigned(FOnSplitTableCellsFormShowing) then
    FOnSplitTableCellsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoHyperlinkFormShowing(const AArgs: TdxHyperlinkFormShowingEventArgs);
begin
  if Assigned(FOnHyperlinkFormShowing) then
    FOnHyperlinkFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoDeleteTableCellsFormShowing(const AArgs: TdxDeleteTableCellsFormShowingEventArgs);
begin
  if Assigned(FOnDeleteTableCellsFormShowing) then
    FOnDeleteTableCellsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoInsertMergeFieldFormShowing(const AArgs: TdxInsertMergeFieldFormShowingEventArgs);
begin
  if Assigned(FOnInsertMergeFieldFormShowing) then
    FOnInsertMergeFieldFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoInsertTableCellsFormShowing(const AArgs: TdxInsertTableCellsFormShowingEventArgs);
begin
  if Assigned(FOnInsertTableCellsFormShowing) then
    FOnInsertTableCellsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoColumnsSetupFormShowing(const AArgs: TdxColumnsSetupFormShowingEventArgs);
begin
  if Assigned(FOnColumnsSetupFormShowing) then
    FOnColumnsSetupFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoEditStyleFormShowing(const AArgs: TdxEditStyleFormShowingEventArgs);
begin
  if Assigned(FOnEditStyleFormShowing) then
    FOnEditStyleFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoFloatingInlineObjectLayoutOptionsFormShowing(const AArgs: TdxFloatingInlineObjectLayoutOptionsFormShowingEventArgs);
begin
  if Assigned(FOnFloatingInlineObjectLayoutOptionsFormShowing) then
    FOnFloatingInlineObjectLayoutOptionsFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DoSearchFormShowing(const AArgs: TdxSearchFormShowingEventArgs);
begin
  if Assigned(FOnSearchFormShowing) then
    FOnSearchFormShowing(Self, AArgs);
end;

procedure TdxCustomRichEditControl.DisposeCommon;
begin
  inherited DisposeCommon;
  FreeAndNil(FHorizontalRuler);
  FreeAndNil(FVerticalRuler);
end;

procedure TdxCustomRichEditControl.OnLookAndFeelChanged;
begin
  inherited OnLookAndFeelChanged;
  FHorizontalRuler.OnLookAndFeelChanged;
  FVerticalRuler.OnLookAndFeelChanged;
  RecreateBackgroundPainter(ActiveView);
  RecreateViewPainter(ActiveView);
  ActiveView.EnforceFormattingCompleteForVisibleArea;
  OnResizeCore(True);
  RedrawEnsureSecondaryFormattingComplete;
end;

procedure TdxCustomRichEditControl.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  OnLookAndFeelChanged;
end;

function TdxCustomRichEditControl.CalculateVerticalRulerWidth: Integer;
var
  AVisibility: TdxRichEditRulerVisibility;
begin
  AVisibility := Options.VerticalRuler.Visibility;
  case AVisibility of
    TdxRichEditRulerVisibility.Visible:
      Result := FVerticalRuler.GetRulerWidthInPixels;
    TdxRichEditRulerVisibility.Hidden:
      Result := 0;
    else
      if ActiveView.ShowVerticalRulerByDefault then
        Result := FVerticalRuler.GetRulerWidthInPixels
      else
        Result := 0;
  end;
end;

function TdxCustomRichEditControl.CalculateHorizontalRulerHeight: Integer;
var
  AVisibility: TdxRichEditRulerVisibility;
begin
  AVisibility := Options.HorizontalRuler.Visibility;
  case AVisibility of
    TdxRichEditRulerVisibility.Visible:
      Result := FHorizontalRuler.GetRulerHeightInPixels;
    TdxRichEditRulerVisibility.Hidden:
      Result := 0;
    else
      if ActiveView.ShowHorizontalRulerByDefault then
        Result := FHorizontalRuler.GetRulerHeightInPixels
      else
        Result := 0;
  end;
end;

function TdxCustomRichEditControl.ShouldUpdateRulers: Boolean;
begin
  Result := HandleAllocated and (Parent <> nil);
end;

procedure TdxCustomRichEditControl.InitializeRulers;
begin
  FVerticalRuler.Parent := Self;
  FVerticalRuler.HandleNeeded;
  FHorizontalRuler.Parent := Self;
  FHorizontalRuler.HandleNeeded;
end;

procedure TdxCustomRichEditControl.UpdateVerticalRuler;
begin
  UpdateVerticalRulerCore;
end;

procedure TdxCustomRichEditControl.UpdateHorizontalRuler;
begin
  UpdateHorizontalRulerCore;
end;

procedure TdxCustomRichEditControl.UpdateRulersCore;
begin
  UpdateHorizontalRulerCore;
  UpdateVerticalRulerCore;
end;

procedure TdxCustomRichEditControl.UpdateRulers;
begin
  if (FHorizontalRuler <> nil) and FHorizontalRuler.CanUpdate then
    UpdateHorizontalRulerCore;
  if (FVerticalRuler <> nil) and FVerticalRuler.CanUpdate then
    UpdateVerticalRulerCore;
end;

procedure TdxCustomRichEditControl.UpdateHorizontalRulerCore;
var
  AVisible: Boolean;
begin
  AVisible := CalculateHorizontalRulerVisibility;
  FHorizontalRuler.Visible := AVisible;
  if (FHorizontalRuler <> nil) and AVisible then
    FHorizontalRuler.Reset;
end;

procedure TdxCustomRichEditControl.UpdateVerticalRulerCore;
var
  AVisible: Boolean;
begin
  AVisible := CalculateVerticalRulerVisibility;
  VerticalRuler.Visible := AVisible;
  if (FVerticalRuler <> nil) and AVisible then
    FVerticalRuler.Reset;
end;

function TdxCustomRichEditControl.CalculateVerticalRulerVisibility: Boolean;
var
  AVisibility: TdxRichEditRulerVisibility;
begin
  AVisibility := Options.VerticalRuler.Visibility;
  case AVisibility of
    TdxRichEditRulerVisibility.Visible:
      Exit(True);
    TdxRichEditRulerVisibility.Hidden:
      Exit(False);
    else
      Exit(ActiveView.ShowVerticalRulerByDefault);
  end;
end;

function TdxCustomRichEditControl.CalculateHorizontalRulerVisibility: Boolean;
var
  AVisibility: TdxRichEditRulerVisibility;
begin
  AVisibility := Options.HorizontalRuler.Visibility;
  case AVisibility of
    TdxRichEditRulerVisibility.Visible:
      Exit(True);
    TdxRichEditRulerVisibility.Hidden:
      Exit(False);
    else
      Exit(ActiveView.ShowHorizontalRulerByDefault);
  end;
end;

function TdxCustomRichEditControl.GetCanShowNumberingListForm: Boolean;
begin
  Result := True;
end;

function TdxCustomRichEditControl.CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
begin
  Result := TdxRichEditControlOptions.Create(TdxInnerRichEditDocumentServer(ADocumentServer));
end;

function TdxCustomRichEditControl.GetViews: TdxRichEditViewRepository;
begin
  Result := TdxRichEditViewRepository(inherited Views);
end;

function TdxCustomRichEditControl.GetOptions: TdxRichEditControlOptions;
begin
  Result := TdxRichEditControlOptions(inherited Options);
end;

procedure TdxCustomRichEditControl.InsertMergeFieldFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  if (FInsertMergeFieldForm <> Sender) or (Action in [caNone, caMinimize]) then
    Exit;
  FInsertMergeFieldForm.OnClose := nil;
  FInsertMergeFieldForm := nil;
  Action := caFree;
end;

procedure TdxCustomRichEditControl.PerformRulersResize;
var
  AHorizontalRulerHeight, AVerticalRulerWidth: Integer;
begin
  inherited PerformRulersResize;
  AHorizontalRulerHeight := CalculateHorizontalRulerHeight;
  AVerticalRulerWidth := CalculateVerticalRulerWidth;
  FHorizontalRuler.SetBounds(FClientBounds.Left, FClientBounds.Top, FClientBounds.Width, AHorizontalRulerHeight);
  FVerticalRuler.SetBounds(FClientBounds.Left, FClientBounds.Top, AVerticalRulerWidth, FClientBounds.Height);
  Inc(FClientBounds.Top, AHorizontalRulerHeight);
  Inc(FClientBounds.Left, AVerticalRulerWidth);
end;

procedure TdxCustomRichEditControl.SearchFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  if (FSearchForm = nil) or (Action in [caNone, caMinimize]) then
    Exit;

  FSearchForm.OnClose := nil;
  FSearchForm := nil;
  Action := caFree;
end;

procedure TdxCustomRichEditControl.SetViews(const Value: TdxRichEditViewRepository);
begin
  inherited Views := Value;
end;

procedure TdxCustomRichEditControl.SetOptions(const Value: TdxRichEditControlOptions);
begin
  inherited Options := Value;
end;

procedure TdxCustomRichEditControl.ShowBookmarkForm;
var
  AControllerParameters: TdxBookmarkFormControllerParameters;
  AArgs: TdxBookmarkFormShowingEventArgs;
  AForm: TdxRichEditBookmarkDialogForm;
begin
  AControllerParameters := TdxBookmarkFormControllerParameters.Create(Self);
  try
    AArgs := TdxBookmarkFormShowingEventArgs.Create(AControllerParameters);
    try
      DoBookmarkFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        AForm := TdxRichEditBookmarkDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI;
  const ACallback: TdxShowColumnsSetupFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxColumnsSetupFormControllerParameters;
  AArgs: TdxColumnsSetupFormShowingEventArgs;
  AForm: TdxRichEditColumnsSetupDialogForm;
begin
  AControllerParameters := TdxColumnsSetupFormControllerParameters.Create(Self, AProperties);
  AArgs := TdxColumnsSetupFormShowingEventArgs.Create(AControllerParameters);
  try
    DoColumnsSetupFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditColumnsSetupDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceColumnsInfo, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
      if AArgs.DialogResult = mrOK then
        ACallback(AControllerParameters.ColumnsInfo, ACallbackData);
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowDeleteTableCellsForm(const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
var
  AControllerParameters: TdxDeleteTableCellsFormControllerParameters;
  AArgs: TdxDeleteTableCellsFormShowingEventArgs;
  AForm: TdxRichEditDeleteTableCellsDialogForm;
begin
  AControllerParameters := TdxDeleteTableCellsFormControllerParameters.Create(Self, AParameters);
  AArgs := TdxDeleteTableCellsFormShowingEventArgs.Create(AControllerParameters);
  try
    DoDeleteTableCellsFormShowing(AArgs);
    if not AArgs.Handled then
    begin
      AForm := TdxRichEditDeleteTableCellsDialogForm.Create(Self);
      try
        AForm.Initialize(AControllerParameters);
        if AForm.ShowModal = mrOk then
          ACallback(AForm.Controller.SourceParameters, ACallbackData);
      finally
        AForm.Free;
      end;
    end
    else
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.CellsParameters, ACallbackData);
  finally
    AControllerParameters.Free;
    AArgs.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowDocumentEncryptQueryNewPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
var
  AControllerParameters: TdxDocumentEncryptQueryPasswordFormControllerParameters;
  AArgs: TdxDocumentEncryptSetPasswordFormShowingEventArgs;
  ANewPassword: string;
begin
  AControllerParameters := TdxDocumentEncryptQueryPasswordFormControllerParameters.Create(Self, APassword);
  try
    AArgs := TdxDocumentEncryptSetPasswordFormShowingEventArgs.Create(AControllerParameters);
    try
      DoDocumentEncryptQueryNewPasswordFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        ANewPassword := APassword;
        if ShowQueryPasswordDialog(TdxQueryPasswordKind.SetNewWithOptionalConfirmation, ANewPassword,
          cxGetResourceString(@sdxDocumentEncryptionQueryNewPasswordForm), Self) then
          ACallback(ANewPassword);
      end
      else
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.Password);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowDocumentProtectionQueryNewPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
var
  AControllerParameters: TdxDocumentProtectionSetPasswordFormControllerParameters;
  AArgs: TdxDocumentProtectionSetPasswordFormShowingEventArgs;
  ANewPassword: string;
begin
  AControllerParameters := TdxDocumentProtectionSetPasswordFormControllerParameters.Create(Self, APassword);
  try
    AArgs := TdxDocumentProtectionSetPasswordFormShowingEventArgs.Create(AControllerParameters);
    try
      DoDocumentProtectionQueryNewPasswordFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        if ShowQueryPasswordDialog(TdxQueryPasswordKind.SetNewWithConfirmation, ANewPassword,
          cxGetResourceString(@sdxDocumentProtectionQueryNewPasswordForm), Self) then
          ACallback(ANewPassword);
      end
      else
      if AArgs.DialogResult = mrOk then
        ACallback(AControllerParameters.Password);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowDocumentProtectionQueryPasswordForm(const APassword: string;
  const ACallback: TdxPasswordFormCallback);
var
  AControllerParameters: TdxDocumentProtectionGetPasswordFormControllerParameters;
  AArgs: TdxDocumentProtectionGetPasswordFormShowingEventArgs;
  ANewPassword: string;
begin
  AControllerParameters := TdxDocumentProtectionGetPasswordFormControllerParameters.Create(Self, APassword);
  try
    AArgs := TdxDocumentProtectionGetPasswordFormShowingEventArgs.Create(AControllerParameters);
    try
      DoDocumentProtectionQueryPasswordFormShowing(AArgs);
      if not AArgs.Handled then
      begin
        if ShowQueryPasswordDialog(TdxQueryPasswordKind.Query, ANewPassword,
          cxGetResourceString(@sdxDocumentProtectionQueryPasswordForm), Self) then
          ACallback(ANewPassword);
      end
      else
        if AArgs.DialogResult = mrOk then
          ACallback(AControllerParameters.Password);
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxCustomRichEditControl.ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
  ShowEditStyleForm(AParagraphSourceStyle, nil, AIndex, ACallback);
end;

procedure TdxCustomRichEditControl.ShowEditStyleForm(ACharacterSourceStyle: TdxCharacterStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
  ShowEditStyleForm(nil, ACharacterSourceStyle, AIndex, ACallback);
end;

procedure TdxCustomRichEditControl.ShowEditStyleForm(AParagraphSourceStyle: TdxParagraphStyle;
  ACharacterSourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
var
  AControllerParameters: TdxEditStyleFormControllerParameters;
  AArgs: TdxEditStyleFormShowingEventArgs;
  AForm: TdxRichEditEditStyleDialogForm;
begin
  if ACharacterSourceStyle = nil then
    AControllerParameters := TdxEditStyleFormControllerParameters.Create(Self, AParagraphSourceStyle, AIndex)
  else
    AControllerParameters := TdxEditStyleFormControllerParameters.Create(Self, ACharacterSourceStyle, AIndex);
  try
    AArgs := TdxEditStyleFormShowingEventArgs.Create(AControllerParameters);
    try
      DoEditStyleFormShowing(AArgs);
      if not AArgs.Handled then
       begin
        AForm := TdxRichEditEditStyleDialogForm.Create(Self);
        try
          AForm.Initialize(AControllerParameters);
          AForm.ShowModal;
        finally
          AForm.Free;
        end;
      end;
    finally
      AArgs.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

function TdxCustomRichEditControl.CreateSearchForm(AControllerParameters: TdxFormControllerParameters): TForm;
var
  AForm: TdxRichEditSearchTextDialogForm;
begin
  AForm := TdxRichEditSearchTextDialogForm.Create(Self);
  AForm.Initialize(AControllerParameters);
  Result := AForm;
end;

function TdxCustomRichEditControl.CreateHorizontalRuler: IdxRulerControl;
begin
  FHorizontalRuler := TdxHorizontalRulerControl.Create(Self);
  Result := FHorizontalRuler;
end;

function TdxCustomRichEditControl.CreateVerticalRuler: IdxRulerControl;
begin
  FVerticalRuler := TdxVerticalRulerControl.Create(Self);
  Result := FVerticalRuler;
end;

{ TdxCustomRichEditRulerControl }

function TdxCustomRichEditRulerControl.GetRulerHeightInPixels: Integer;
begin
  Result := 0;
end;

function TdxCustomRichEditRulerControl.GetRulerWidthInPixels: Integer;
begin
  Result := 0;
end;


end.
