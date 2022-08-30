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

unit dxRichEdit.View.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

{.$DEFINE DXLOGGING}


uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Types, SysUtils, Generics.Defaults, Generics.Collections, Controls, Graphics, SyncObjs, Classes, ActiveX,
  cxGeometry, cxGraphics, dxSkinsCore, dxSkinInfo, dxCoreClasses, cxLookAndFeels, cxControls,
  dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses,
  dxRichEdit.DocumentLayout.UnitConverter,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.Layout.Painters,
  dxGenerics,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.DataObject,
  dxEncoding,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.Export.Core,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.ServiceManager,
  dxRichEdit.Types,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.View.ViewInfo;

type
  TdxCommand = class;
  TdxDragCaret = class;
  TdxDragCaretPosition = class;
  TdxCopySelectionManager = class;
  TdxRichEditControlOptionsBase = class;
  TdxRichEditViewBackgroundPainter = class;
  TdxRichEditView = class;
  TdxSelectionLayout = class;
  TdxRichEditViewHorizontalScrollController = class;
  TdxRichEditViewVerticalScrollController = class;
  TdxScrollBarAdapter = class;
  TdxBackgroundFormatter = class;
  TdxMouseCursorCalculator = class;
  TdxOfficeScrollControllerBase = class;

  IdxInnerControl = interface;
  IdxRichEditControl = interface;

  TdxRefreshAction = (
    AllDocument,
    Zoom,
    Transforms,
    Selection
  );

  TdxScrollEventType = (
    EndScroll,
    First,
    LargeDecrement,
    LargeIncrement,
    Last,
    SmallDecrement,
    SmallIncrement,
    ThumbPosition,
    ThumbTrack
  );

  TdxScrollOrientation = (
    HorizontalScroll,
    VerticalScroll
  );

  { TdxRichEditControlPadding }

  TdxRichEditControlPadding = class(TcxGeometryObject)
  private
    FValue: TRect;
    FDefaultValue: TRect;
    function GetProperty(AIndex: Integer): Integer;
    function IsPropertyStored(AIndex: Integer): Boolean;
    procedure SetValue(const Value: TRect);
    procedure SetProperty(AIndex, AValue: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent; const ADefaultValue: TRect); reintroduce; overload;
    constructor Create(AOwner: TPersistent; ADefaultValue: Integer = 0); reintroduce; overload;
    function IsDefault: Boolean; inline;
    function IsEqual(const APadding: TdxRichEditControlPadding): Boolean;
    procedure Reset;

    property Value: TRect read FValue write SetValue;
  published
    property All: Integer index 0 read GetProperty write SetProperty stored IsPropertyStored;
    property Left: Integer index 1 read GetProperty write SetProperty stored IsPropertyStored;
    property Top: Integer index 2 read GetProperty write SetProperty stored IsPropertyStored;
    property Right: Integer index 3 read GetProperty write SetProperty stored IsPropertyStored;
    property Bottom: Integer index 4 read GetProperty write SetProperty stored IsPropertyStored;
  end;

  { TdxScrollEventArgs }

  TdxScrollEventArgs = class
  private
    FScrollOrientation: TdxScrollOrientation;
    FNewValue: Integer;
    FOldValue: Integer;
    FType: TdxScrollEventType;
  public
    constructor Create(AType: TdxScrollEventType; ANewValue: Integer;
      AScrollOrientation: TdxScrollOrientation = TdxScrollOrientation.HorizontalScroll); overload;
    constructor Create(AType: TdxScrollEventType; AOldValue, ANewValue: Integer;
      AScrollOrientation: TdxScrollOrientation = TdxScrollOrientation.HorizontalScroll); overload;

    property NewValue: Integer read FNewValue write FNewValue;
    property OldValue: Integer read FOldValue;
    property ScrollOrientation: TdxScrollOrientation read FScrollOrientation;
    property &Type: TdxScrollEventType read FType;
  end;

  { IdxPlatformSpecificScrollBarAdapter }

  IdxPlatformSpecificScrollBarAdapter = interface
  ['{8B96CD9D-C46B-4913-9D25-81D0557E6A6C}']
    procedure OnScroll(AAdapter: TdxScrollBarAdapter; ASender: TObject; E: TdxScrollEventArgs);
    procedure ApplyValuesToScrollBarCore(AAdapter: TdxScrollBarAdapter);
    function GetRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function SetRawScrollBarValue(AAdapter: TdxScrollBarAdapter; Value: Integer): Boolean;
    function GetPageUpRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function GetPageDownRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function CreateLastScrollEventArgs(AAdapter: TdxScrollBarAdapter): TdxScrollEventArgs;
  end;
  TdxScrollEvent = procedure (ASender: TObject; E: TdxScrollEventArgs) of object;
  TdxScrollEventHandler = TdxMulticastMethod<TdxScrollEvent>;

  { IdxOfficeScrollbar }

  IdxOfficeScrollbar = interface
  ['{10B08EBE-9E5B-4BFB-89CD-874D8CCC020C}']
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetLargeChange: Integer;
    procedure SetLargeChange(Value: Integer);
    function GetMaximum: Integer;
    procedure SetMaximum(Value: Integer);
    function GetMinimum: Integer;
    procedure SetMinimum(Value: Integer);
    function GetSmallChange: Integer;
    procedure SetSmallChange(Value: Integer);
    function GetValue: Integer;
    procedure SetValue(Value: Integer);
    function GetScroll: TdxScrollEventHandler;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property LargeChange: Integer read GetLargeChange write SetLargeChange;
    property Maximum: Integer read GetMaximum write SetMaximum;
    property Minimum: Integer read GetMinimum write SetMinimum;
    property SmallChange: Integer read GetSmallChange write SetSmallChange;
    property Value: Integer read GetValue write SetValue;
    property Scroll: TdxScrollEventHandler read GetScroll;
  end;

  { IdxRulerControl }

  IdxRulerControl = interface
  ['{B20F0F88-FE44-4E8E-BB7C-CA8AD07FA21E}']
    function GetRichEditControl: IdxRichEditControl;
    function GetIsVisible: Boolean;
    function GetRulerSizeInPixels: Integer;

    property IsVisible: Boolean read GetIsVisible;
    property RichEditControl: IdxRichEditControl read GetRichEditControl;
  end;

  { IdxDecoratorPainter }

  IdxDecoratorPainter = interface
  ['{CD9487C5-BFE6-4CFF-BD8E-EA49830C7F8B}']
    procedure DrawDecorators(APainter: TdxPainter; AViewInfos: TdxPageViewInfoCollection);
  end;

  { IdxTableViewInfoDecorator }

  IdxTableViewInfoDecorator = interface
  ['{746F77E6-F7FC-442C-9144-A1BB7D6209B4}']
    procedure Decorate;
    procedure Free;
  end;

  { TdxTableViewInfoController }

  TdxTableViewInfoController = class abstract
  strict private
    FMousePosition: TPoint;
    procedure SetMousePosition(const AValue: TPoint);
  protected
    procedure OnMousePositionChanged; virtual;
  public
    constructor Create(const AMousePosition: TPoint);
    function CreateDecorator(APainter: TdxPainter): IdxTableViewInfoDecorator; virtual; abstract;
    procedure Update(const APhysicalPoint: TPoint; AHitTestResult: TdxRichEditHitTestResult); virtual; abstract;

    property MousePosition: TPoint read FMousePosition write SetMousePosition;
  end;

  TdxCommandSourceType = (Unknown, Menu, Keyboard, Mouse);

  { IdxCommandUIState }

  IdxCommandUIState = interface
  ['{999C52BA-7260-4AFC-90AF-301D267711CC}']
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    function GetChecked: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Visible: Boolean read GetVisible write SetVisible;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  { IdxCommandExecutionListenerService }

  IdxCommandExecutionListenerService = interface
  ['{8E5060BE-CCB1-442E-8C4E-A27C7D2C8345}']
    procedure BeginCommandExecution(ACommand: TdxCommand; const AState: IdxCommandUIState);
		procedure EndCommandExecution(ACommand: TdxCommand; const AState: IdxCommandUIState);
  end;

  { TdxDefaultCommandUIState }

  TdxDefaultCommandUIState = class(TInterfacedObject, IdxCommandUIState)
  private
    FIsEnabled: Boolean;
    FIsChecked: Boolean;
    FIsVisible: Boolean;
  protected
    function GetEnabled: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    function GetChecked: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetChecked(const Value: Boolean); virtual;
  public
    constructor Create;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Visible: Boolean read GetVisible write SetVisible;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  { TdxCommand }

  TdxCommand = class abstract (TcxIUnknownObject)
  private
    FCommandSourceType: TdxCommandSourceType;
    FHideDisabled: Boolean;
  protected
    function GetCommandSourceType: TdxCommandSourceType; virtual;
    procedure SetCommandSourceType(const Value: TdxCommandSourceType); virtual;
    function GetShowsModalDialog: Boolean; virtual;

    procedure UpdateUIStateCore(const AState: IdxCommandUIState); virtual; abstract;
    procedure UpdateUIStateViaService(const AState: IdxCommandUIState); virtual;
    function ServiceProvider: IdxServiceProvider; virtual;
  public
    procedure Execute; virtual;

    function CanExecute: Boolean; virtual;
    function CreateDefaultCommandUIState: IdxCommandUIState; virtual;
    procedure ForceExecute(const AState: IdxCommandUIState); virtual; abstract;
    procedure UpdateUIState(const AState: IdxCommandUIState); virtual;

    class function NeedUseObjectLocalization: Boolean; virtual;
    function GetObjectDescription: string; virtual;
    function GetObjectMenuCaption: string; virtual;

    class function GetDescription: string; virtual;
    class function GetMenuCaption: string; virtual;

    property CommandSourceType: TdxCommandSourceType read GetCommandSourceType write SetCommandSourceType;
    property HideDisabled: Boolean read FHideDisabled write FHideDisabled;
    property ShowsModalDialog: Boolean read GetShowsModalDialog;
  end;

  { TdxControlCommand }

  TdxControlCommand = class abstract(TdxCommand)
  private
    FRichEditControl: IdxRichEditControl;

    function GetControl: TCustomControl;
  protected
    function GetCommandExecutionListener: IdxCommandExecutionListenerService; virtual;
    procedure NotifyBeginCommandExecution(const AState: IdxCommandUIState); virtual;
    procedure NotifyEndCommandExecution(const AState: IdxCommandUIState); virtual;
    function ServiceProvider: IdxServiceProvider; override;

    property Control: TCustomControl read GetControl;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); virtual;
    class function GetImageName: string; virtual;
    class function Id: TdxRichEditCommandId; virtual;

    property RichEditControl: IdxRichEditControl read FRichEditControl;
  end;
  TdxControlCommandClass = class of TdxControlCommand;

  { TdxRichEditCommand }

  TdxRichEditCommand = class abstract (TdxControlCommand)
  private
    function GetActivePieceTable: TdxPieceTable;
    function GetActiveViewType: TdxRichEditViewType;
    function GetDocumentModel: TdxDocumentModel;
    function GetInnerControl: IdxInnerControl;
    function GetOptions: TdxRichEditControlOptionsBase;
  protected
    function GetActiveView: TdxRichEditView;

    procedure CheckExecutedAtUIThread; virtual;

    procedure ApplyCommandsRestriction(const AState: IdxCommandUIState; AOption: TdxDocumentCapability; AdditionEnabledCondition: Boolean = True);
    procedure ApplyCommandRestrictionOnEditableControl(const AState: IdxCommandUIState; AOption: TdxDocumentCapability); overload; virtual;
    procedure ApplyCommandRestrictionOnEditableControl(const AState: IdxCommandUIState; AOption: TdxDocumentCapability; AdditionEnabledCondition: Boolean); overload; virtual;
    procedure ApplyCommandRestrictionOnReadOnlyControl(const AState: IdxCommandUIState); virtual;
    procedure ApplyDocumentProtectionToSelectedCharacters(const AState: IdxCommandUIState); virtual;
    procedure ApplyDocumentProtectionToSelectedParagraphs(const AState: IdxCommandUIState);
    procedure ApplyDocumentProtectionToSelectedSections(const AState: IdxCommandUIState); virtual;
    procedure ApplyDocumentProtectionToTable(const AState: IdxCommandUIState; ATable: TdxTable);
    function CanEditSelection: Boolean; virtual;
    function CanEditTable(ATable: TdxTable): Boolean;

    function IsContentEditable: Boolean; virtual;
    function ExtendSelectionToParagraphBoundary: TdxSelectionItemList;
    function ExtendSelectionToSectionBoundary: TdxSelectionItem;
    function ExtendToParagraphBoundary(ASelectionItem: TdxSelectionItem): TdxSelectionItem;

    property ActivePieceTable: TdxPieceTable read GetActivePieceTable;
    property ActiveView: TdxRichEditView read GetActiveView;
    property ActiveViewType: TdxRichEditViewType read GetActiveViewType;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InnerControl: IdxInnerControl read GetInnerControl;
  public
    property Options: TdxRichEditControlOptionsBase read GetOptions;
  end;
  TdxRichEditCommandClass = class of TdxRichEditCommand;

  { IdxInnerControl }

  IdxInnerControl = interface(IdxBoxMeasurerProvider)
  ['{496D044E-9CE8-4C97-ADB4-BDE4215693C2}']
    function GetActiveView: TdxRichEditView;
    function GetDocumentModel: TdxDocumentModel;
    function GetDocumentModelTemplate: TdxDocumentModel;
    function GetFormatter: TdxBackgroundFormatter;
    function GetModified: Boolean;
    function GetMouseController: TdxRichEditCustomMouseController;
    function GetOptions: TdxRichEditControlOptionsBase;
    function GetPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
    function GetHorizontalScrollBar: IdxOfficeScrollbar;
    function GetVerticalScrollBar: IdxOfficeScrollbar;
    function GetHorizontalRuler: IdxRulerControl;
    function GetVerticalRuler: IdxRulerControl;
    function GetUIUnit: TdxMeasurementUnit;
    function GetOnSearchComplete: TdxSearchCompleteEvent;
    procedure SetOnSearchComplete(const Value: TdxSearchCompleteEvent);

    function GetActiveViewType: TdxRichEditViewType;
    procedure SetActiveViewType(const Value: TdxRichEditViewType);

    procedure RaiseUpdateUI;
    procedure SetModified(AValue: Boolean);

    function CreateCommand(ACommandID: TdxRichEditCommandID): TdxRichEditCommand;
    procedure RegisterCommand(ACommandClass: TdxRichEditCommandClass);
    procedure UnregisterCommand(ACommandClass: TdxRichEditCommandClass);

    function CreateCopySelectionManager: TdxCopySelectionManager;
    function CreateMouseCursorCalculator: TdxMouseCursorCalculator;

    function GetIsEditable: Boolean;
    function IsHyperlinkModifierKeysPress: Boolean;
    function OnHyperlinkClick(AField: TdxField; AAllowForModifiers: Boolean): Boolean;

    procedure BeginDocumentRendering;
    procedure EndDocumentRendering;

    function CanCloseExistingDocument: Boolean;
    function SaveDocumentAs: Boolean;
    function SaveDocument: Boolean;
    procedure LoadDocument; overload;
    procedure LoadDocument(const AFileName: string); overload;

    procedure OnAfterExport(Sender: TObject);
    procedure OnBeforeExport(Sender: TObject; E: TdxBeforeExportEventArgs);
    procedure OnCalculateDocumentVariable(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);

    procedure RaiseVisiblePagesChanged;
    function RaiseDocumentClosing: Boolean;

    procedure DoSearchComplete(E: TdxSearchCompleteEventArgs);

    property ActiveView: TdxRichEditView read GetActiveView;
    property ActiveViewType: TdxRichEditViewType read GetActiveViewType write SetActiveViewType;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property DocumentModelTemplate: TdxDocumentModel read GetDocumentModelTemplate;
    property Formatter: TdxBackgroundFormatter read GetFormatter;
    property IsEditable: Boolean read GetIsEditable;
    property Modified: Boolean read GetModified write SetModified;
    property Options: TdxRichEditControlOptionsBase read GetOptions;
    property PredefinedFontSizeCollection: TdxPredefinedFontSizeCollection read GetPredefinedFontSizeCollection;

    property UIUnit: TdxMeasurementUnit read GetUIUnit;

    property HorizontalScrollBar: IdxOfficeScrollbar read GetHorizontalScrollBar;
    property VerticalScrollBar: IdxOfficeScrollbar read GetVerticalScrollBar;

    property HorizontalRuler: IdxRulerControl read GetHorizontalRuler;
    property VerticalRuler: IdxRulerControl read GetVerticalRuler;

    property OnSearchComplete: TdxSearchCompleteEvent read GetOnSearchComplete write SetOnSearchComplete;
    //for internal use
    property MouseController: TdxRichEditCustomMouseController read GetMouseController;
 end;

  TdxShowFontFormCallback = reference to procedure(AProperties: TdxMergedCharacterProperties; AData: TObject);
  TdxShowParagraphFormCallback = reference to procedure(AProperties: TdxMergedParagraphProperties; AData: TObject);
  TdxShowEditStyleFormCallback = reference to procedure(ASourceStyle, ATargetStyle: TdxStyleBase);
  TdxShowTableStyleFormCallback = reference to procedure(ASourceStyle, ATargetStyle: TdxTableStyle);
  TdxShowLineNumberingFormCallback = reference to procedure(AProperties: TdxLineNumberingInfo; AData: TObject);
  TdxShowPageSetupFormCallback = reference to procedure(AProperties: TdxPageSetupInfo; AData: TObject);
  TdxShowColumnsSetupFormCallback = reference to procedure(const AProperties: TdxColumnsInfoUI; AData: TObject);
  TdxShowTabsFormCallback = reference to procedure(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; AData: TObject);
  TdxShowNumberingListFormCallback = reference to procedure(AParagraphs: TdxParagraphList; AData: TObject);
  TdxShowSymbolFormCallback = reference to procedure(const ASymbolProperties: TdxSymbolProperties; AData: TObject);
  TdxShowHyperlinkFormCallback = reference to procedure(AHyperlinkInfo: TdxHyperlinkInfo; ASource: TdxTextToDisplaySource; ARunInfo: TdxRunInfo; const AText: string);
  TdxShowInsertTableFormCallback = reference to procedure(const AParameters: TdxCreateTableParameters; AData: TObject);
  TdxShowInsertDeleteTableCellsFormCallback = reference to procedure(const AParameters: TdxTableCellsParameters; AData: TObject);
  TdxShowSplitTableCellsFormCallback = reference to procedure(const AParameters: TdxSplitTableCellsParameters; AData: TObject);
  TdxPasswordFormCallback = reference to procedure(const APasswordInfo: string);
  TdxShowFloatingInlineObjectLayoutOptionsFormCallback = reference to procedure(const AFloatingObjectParameters: TdxFloatingInlineObjectParameters; AData: TObject);
  TdxShowMergeDatabaseRecordsFormCallback = reference to procedure(const AMergeRecordsParameters: TdxMergeRecordsParameters; AData: TObject);

  { IdxRichEditControl }

  IdxRichEditControl = interface(IdxBatchUpdateable)
  ['{1E263549-ED22-4B0D-92C4-CE611754B1DB}']
    function GetCanShowNumberingListForm: Boolean;
    function GetInnerControl: IdxInnerControl;
    function GetControl: TWinControl;
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

    procedure HideCaret;
    procedure ShowCaret;
    procedure OnViewPaddingChanged;

    procedure OnResizeCore;

    procedure AddKeyboardService(const AService: IdxKeyboardHandlerService);
    function GetKeyboardHandler: IdxKeyboardHandlerService;
    procedure RemoveKeyboardService(const AService: IdxKeyboardHandlerService);

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
    procedure ShowDocumentEncryptQueryNewPasswordForm(const APassword: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowDocumentProtectionQueryNewPasswordForm(const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowDocumentProtectionQueryPasswordForm(const APasswordInfo: string; const ACallback: TdxPasswordFormCallback);
    procedure ShowLineNumberingForm(AProperties: TdxLineNumberingInfo; const ACallback: TdxShowLineNumberingFormCallback;
      ACallbackData: TObject);
    procedure ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
      ACallbackData: TObject; AInitialTabPage: TdxPageSetupFormInitialTabPage);
    procedure ShowColumnsSetupForm(const AProperties: TdxColumnsInfoUI; const ACallback: TdxShowColumnsSetupFormCallback;
      ACallbackData: TObject);
    procedure ShowTablePropertiesForm(ASelectedCells: TdxSelectedCellsCollection);
    procedure ShowFloatingInlineObjectLayoutOptionsForm(const AFloatingObjectParameters: TdxFloatingInlineObjectParameters;
      const ACallback: TdxShowFloatingInlineObjectLayoutOptionsFormCallback; ACallbackData: TObject);
    procedure ShowTableOptionsForm(ATable: TdxTable; AOwner: TObject = nil);
    procedure ShowMergeDatabaseRecordsForm(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const ACallback: TdxShowMergeDatabaseRecordsFormCallback);
    procedure ShowTOCForm(AField: TdxField);
    procedure UpdateControlAutoSize;
    procedure Redraw(Action: TdxRefreshAction);
    procedure RedrawEnsureSecondaryFormattingComplete(Action: TdxRefreshAction);
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
    property BackgroundPainter: TdxRichEditViewBackgroundPainter read GetBackgroundPainter;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property MeasurementAndDrawingStrategy: TdxMeasurementAndDrawingStrategy read GetMeasurementAndDrawingStrategy;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    property CanShowNumberingListForm: Boolean read GetCanShowNumberingListForm;
    property Control: TWinControl read GetControl;
    property Cursor: TCursor read GetCursor write SetCursor;
    property Document: IdxRichEditDocument read GetDocument;
    property DocumentLayout: TdxRichEditDocumentLayout read GetDocumentLayout;
    property DpiX: Single read GetDpiX;
    property DpiY: Single read GetDpiY;
    property DragCaret: TdxDragCaret read GetDragCaret;
    property InnerControl: IdxInnerControl read GetInnerControl;
    property IsDestroying: Boolean read GetIsDestroying;
    property KeyboardHandler: IdxKeyboardHandlerService read GetKeyboardHandler;
    property LayoutUnit: TdxDocumentLayoutUnit read GetLayoutUnit write SetLayoutUnit;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property Overtype: Boolean read GetOvertype write SetOvertype;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SkinLeftMargin: Integer read GetSkinLeftMargin;
    property SkinRightMargin: Integer read GetSkinRightMargin;
    property SkinTopMargin: Integer read GetSkinTopMargin;
    property SkinBottomMargin: Integer read GetSkinBottomMargin;
    property UseSkinMargins: Boolean read GetUseSkinMargins;
    property ViewBounds: TRect read GetViewBounds;
  end;

  { TdxRichEditControlOptionsBase }

  TdxRichEditControlOptionsBase = class abstract(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      ShowHiddenText = TdxSimpleFormattingMarkVisibilityOptions.TAction.ShowHiddenText;
    end;
  strict private
    FDocumentServer: IdxRichEditDocumentServer;
    FVerticalRuler: TdxVerticalRulerOptions;
    FHorizontalRuler: TdxHorizontalRulerOptions;
    FHyperlinks: TdxHyperlinkOptions;
  private
    function GetAutoCorrect: TdxAutoCorrectOptions;
    function GetBehavior: TdxRichEditBehaviorOptions;
    function GetDocumentCapabilities: TdxDocumentCapabilitiesOptions;
    function GetDocumentSaveOptions: TdxDocumentSaveOptions;
    function GetExport: TdxRichEditDocumentExportOptions;
    function GetImport: TdxRichEditDocumentImportOptions;
    function GetFields: TdxFieldOptions;
    function GetFormattingMarkVisibility: TdxFormattingMarkVisibilityOptions;
    function GetTableOptions: TdxTableOptions;
    function GetLayout: TdxRichEditLayoutOptions;
    function GetMailMerge: TdxRichEditMailMergeOptions;
    function GetBookmarks: TdxBookmarkOptions;
    function GetShowHiddenText: Boolean;
    function GetSpellChecker: TdxSpellCheckerOptions;
    procedure SetAutoCorrect(const Value: TdxAutoCorrectOptions);
    procedure SetBehavior(const Value: TdxRichEditBehaviorOptions);
    procedure SetDocumentCapabilities(const Value: TdxDocumentCapabilitiesOptions);
    procedure SetDocumentSaveOptions(const Value: TdxDocumentSaveOptions);
    procedure SetExport(const Value: TdxRichEditDocumentExportOptions);
    procedure SetImport(const Value: TdxRichEditDocumentImportOptions);
    procedure SetFields(const Value: TdxFieldOptions);
    procedure SetFormattingMarkVisibility(const Value: TdxFormattingMarkVisibilityOptions);
    procedure SetHorizontalRuler(const Value: TdxHorizontalRulerOptions);
    procedure SetHyperlinks(const Value: TdxHyperlinkOptions);
    procedure SetTableOptions(const Value: TdxTableOptions);
    procedure SetLayout(const Value: TdxRichEditLayoutOptions);
    procedure SetMailMerge(const Value: TdxRichEditMailMergeOptions);
    procedure SetBookmarks(const Value: TdxBookmarkOptions);
    procedure SetShowHiddenText(const Value: Boolean);
    procedure SetSpellChecker(const Value: TdxSpellCheckerOptions);
    procedure SetVerticalRuler(const Value: TdxVerticalRulerOptions);
    function GetAuthentication: TdxAuthenticationOptions;
    function GetCopyPaste: TdxCopyPasteOptions;
    function GetPrinting: TdxPrintingOptions;
    function GetRangePermissions: TdxRangePermissionOptions;
    function GetSearch: TdxDocumentSearchOptions;
    procedure SetAuthentication(const Value: TdxAuthenticationOptions);
    procedure SetCopyPaste(const Value: TdxCopyPasteOptions);
    procedure SetPrinting(const Value: TdxPrintingOptions);
    procedure SetRangePermissions(const Value: TdxRangePermissionOptions);
    procedure SetSearch(const Value: TdxDocumentSearchOptions);
  protected
    procedure CreateInnerOptions; override;
    procedure SubscribeInnerOptionsEvents; virtual;
    procedure UnsubscribeInnerOptionsEvents; virtual;
    property DocumentServer: IdxRichEditDocumentServer read FDocumentServer;

    property ShowHiddenText: Boolean read GetShowHiddenText write SetShowHiddenText default False;
    property SpellChecker: TdxSpellCheckerOptions read GetSpellChecker write SetSpellChecker;
  public
    constructor Create(const ADocumentServer: IdxRichEditDocumentServer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Authentication: TdxAuthenticationOptions read GetAuthentication write SetAuthentication;
    property AutoCorrect: TdxAutoCorrectOptions read GetAutoCorrect write SetAutoCorrect;
    property Behavior: TdxRichEditBehaviorOptions read GetBehavior write SetBehavior;
    property Bookmarks: TdxBookmarkOptions read GetBookmarks write SetBookmarks;
    property CopyPaste: TdxCopyPasteOptions read GetCopyPaste write SetCopyPaste;
    property DocumentCapabilities: TdxDocumentCapabilitiesOptions read GetDocumentCapabilities write SetDocumentCapabilities;
    property DocumentSaveOptions: TdxDocumentSaveOptions read GetDocumentSaveOptions write SetDocumentSaveOptions;
    property Export: TdxRichEditDocumentExportOptions read GetExport write SetExport;
    property Fields: TdxFieldOptions read GetFields write SetFields;
    property FormattingMarkVisibility: TdxFormattingMarkVisibilityOptions read GetFormattingMarkVisibility write SetFormattingMarkVisibility;
    property HorizontalRuler: TdxHorizontalRulerOptions read FHorizontalRuler write SetHorizontalRuler;
    property Hyperlinks: TdxHyperlinkOptions read FHyperLinks write SetHyperlinks;
    property Import: TdxRichEditDocumentImportOptions read GetImport write SetImport;
    property Layout: TdxRichEditLayoutOptions read GetLayout write SetLayout;
    property MailMerge: TdxRichEditMailMergeOptions read GetMailMerge write SetMailMerge;
    property Printing: TdxPrintingOptions read GetPrinting write SetPrinting;
    property RangePermissions: TdxRangePermissionOptions read GetRangePermissions write SetRangePermissions;
    property Search: TdxDocumentSearchOptions read GetSearch write SetSearch;
    property TableOptions: TdxTableOptions read GetTableOptions write SetTableOptions;
    property VerticalRuler: TdxVerticalRulerOptions read FVerticalRuler write SetVerticalRuler;
  end;

  { IdxRichEditViewVisitor }

  IdxRichEditViewVisitor = interface
  ['{8810AB47-931E-40A8-9F5F-F532EFF14E50}']

    procedure Visit(AView: TdxRichEditView);
  end;

  { TdxMouseCursorCalculator }

  TdxMouseCursorCalculator = class
  private
    FView: TdxRichEditView;
  protected
    function CalculateCore(AResult: TdxRichEditHitTestResult): TCursor;
    function CalculateHotZoneCursor(AResult: TdxRichEditHitTestResult): TCursor;
    function CalculateHotZoneCursorCore(AHotZone: TdxHotZone): TCursor;
    function CalculateCharacterMouseCursor(AResult: TdxRichEditHitTestResult): TCursor;
    function IsEditable: Boolean;
  public
    constructor Create(AView: TdxRichEditView);

    function Calculate(AHitTestResult: TdxRichEditHitTestResult; const APhysicalPoint: TPoint): TCursor; virtual;

    property View: TdxRichEditView read FView;
  end;

  { TdxCaretDocumentLayoutPosition }

  TdxCaretDocumentLayoutPosition = class(TdxDocumentLayoutPosition)
  private
    FView: TdxRichEditView;
  protected
    procedure EnsureFormattingComplete; override;
    procedure EnsurePageSecondaryFormattingComplete(APage: TdxPage); override;
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
    function GetPieceTable: TdxPieceTable; override;
  public
    constructor Create(AView: TdxRichEditView); reintroduce;
  end;

  { TdxCaretPosition }

  TdxCaretPosition = class
  private
    FView: TdxRichEditView;
    FCaretBoundsPosition: TdxDocumentLayoutPosition;
    FX: Integer;
    FPageViewInfo: TdxPageViewInfo;
    FTableCellTopAnchorIndex: Integer;
    FPosition: TdxDocumentLayoutPosition;
    FInputPosition: TdxInputPosition;
    FModelPosition: TdxDocumentModelPosition;
    FPreferredPageIndex: Integer;

    function GetActualLogPosition(AEndPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
    function GetDocumentModel: TdxDocumentModel;
    function GetPreferredPageIndex: Integer;
    procedure SetPreferredPageIndex(const Value: Integer);
    procedure SetPageViewInfo(const Value: TdxPageViewInfo);
    procedure SetCaretBoundsPosition(const Value: TdxDocumentLayoutPosition);
  protected
    function GetVirtualLogPosition: TdxDocumentLogPosition; virtual;
    function GetLogPosition: TdxDocumentLogPosition; virtual;
    function GetUsePreviousBoxBounds: Boolean; virtual;
    function CreateDragCaretPosition: TdxDragCaretPosition; virtual;

    procedure Invalidate;
    procedure InvalidatePageViewInfo;
    procedure InvalidateInputPosition;
    function UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; virtual;
    function UpdateCaretDocumentLayoutPosition(ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutPosition;
    procedure UpdateModelPosition(ALogPosition: TdxDocumentLogPosition);
    function GetCaretBoundsDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
    function CalculateCurrentFormatingPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentModelPosition;
    function ShouldUpdateModelPosition(ALogPosition: TdxDocumentLogPosition): Boolean;

    property View: TdxRichEditView read FView;
    property VirtualLogPosition: TdxDocumentLogPosition read GetVirtualLogPosition;
    property UsePreviousBoxBounds: Boolean read GetUsePreviousBoxBounds;
    property CaretBoundsPosition: TdxDocumentLayoutPosition read FCaretBoundsPosition write SetCaretBoundsPosition;
  public
    constructor Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
    destructor Destroy; override;

    function CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition; virtual;
    function Update(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; virtual;
    function GetInputPosition: TdxInputPosition; virtual;
    function TryGetInputPosition: TdxInputPosition; virtual;
    function CalculateCaretBounds: TRect; virtual;
    function CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition; virtual;
    function CreateInputPosition: TdxInputPosition; overload; virtual;
    function CreateInputPosition(ALogPosition: TdxDocumentLogPosition): TdxInputPosition; overload; virtual;
    function GetRowBoundsRelativeToPage: TRect; virtual;

    property PageViewInfo: TdxPageViewInfo read FPageViewInfo write SetPageViewInfo;
    property X: Integer read FX write FX;
    property TableCellTopAnchorIndex: Integer read FTableCellTopAnchorIndex write FTableCellTopAnchorIndex;
    property LayoutPosition: TdxDocumentLayoutPosition read FPosition;
    property LogPosition: TdxDocumentLogPosition read GetLogPosition;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PreferredPageIndex: Integer read GetPreferredPageIndex write SetPreferredPageIndex;
  end;

  { TdxDragCaretPosition }

  TdxDragCaretPosition = class(TdxCaretPosition)
  strict private
    FLogPosition: TdxDocumentLogPosition;
  protected
    function GetLogPosition: Integer; override;
    function GetVirtualLogPosition: Integer; override;
    function GetUsePreviousBoxBounds: Boolean; override;
  public
    procedure SetLogPosition(ALogPosition: TdxDocumentLogPosition);
  end;

  TdxExplicitCaretPosition = class(TdxCaretPosition)
  strict private
    FLogPosition: TdxDocumentLogPosition;
  protected
    function GetLogPosition: Integer; override;
    function GetVirtualLogPosition: Integer; override;
    function GetUsePreviousBoxBounds: Boolean; override;
    function UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; override;
  public
    constructor Create(AView: TdxRichEditView; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer); reintroduce;
  end;

  { TdxHeaderFooterCaretPosition }

  TdxHeaderFooterCaretPosition = class(TdxCaretPosition)
  protected
    function CreateDragCaretPosition: TdxDragCaretPosition; override;
  public
    function CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition; override;
    function CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition; override;
  end;

  { TdxTextBoxCaretPosition }

  TdxTextBoxCaretPosition = class(TdxCaretPosition)
  protected
    function CreateDragCaretPosition: TdxDragCaretPosition; override;
  public
    function CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition; override;
    function CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition; override;
  end;

  { TdxExplicitTextBoxCaretPosition }

  TdxExplicitTextBoxCaretPosition = class(TdxTextBoxCaretPosition)
  strict private
    FLogPosition: TdxDocumentLogPosition;
  protected
    function GetLogPosition: TdxDocumentLogPosition; override;
    function GetVirtualLogPosition: TdxDocumentLogPosition; override;
    function GetUsePreviousBoxBounds: Boolean; override;
    function UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; override;
  public
    constructor Create(AView: TdxRichEditView; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
  end;

  { TdxHeaderFooterDragCaretPosition }

  TdxHeaderFooterDragCaretPosition = class(TdxDragCaretPosition)
  public
    function CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition; override;
    function CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition; override;
  end;

  { TdxHeaderFooterCaretDocumentLayoutPosition }

  TdxHeaderFooterCaretDocumentLayoutPosition = class(TdxHeaderFooterDocumentLayoutPosition)
  strict private
    FView: TdxRichEditView;
  protected
    function GetPieceTable: TdxPieceTable; override;
    procedure EnsureFormattingComplete; override;
    procedure EnsurePageSecondaryFormattingComplete(APage: TdxPage); override;
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
  public
    constructor Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
  end;

  { TdxExplicitHeaderFooterCaretPosition }

  TdxExplicitHeaderFooterCaretPosition = class(TdxHeaderFooterCaretPosition)
  strict private
    FLogPosition: TdxDocumentLogPosition;
  protected
    function GetLogPosition: TdxDocumentLogPosition; override;
    function GetVirtualLogPosition: TdxDocumentLogPosition; override;
    function GetUsePreviousBoxBounds: Boolean; override;
    function UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; override;
  public
    constructor Create(AView: TdxRichEditView; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
  end;

  { TdxTextBoxCaretDocumentLayoutPosition }

  TdxTextBoxCaretDocumentLayoutPosition = class(TdxTextBoxDocumentLayoutPosition)
  strict private
    FView: TdxRichEditView;
    FTextBoxContentType: TdxTextBoxContentType;
  protected
    function GetPieceTable: TdxPieceTable; override;
    procedure EnsureFormattingComplete; override;
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
  public
    constructor Create(AView: TdxRichEditView; APreferredPageIndex: Integer); overload;
    constructor Create(AView: TdxRichEditView; ATextBoxPieceTable: TdxTextBoxContentType; APreferredPageIndex: Integer); overload;
    function IsValid(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; override;
    function Update(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; override;
  end;

  { TdxTextBoxDragCaretPosition }

  TdxTextBoxDragCaretPosition = class(TdxDragCaretPosition)
  public
    function CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition; override;
    function CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition; override;
  end;

  { TdxCaret }

  TdxCaret = class
  private
    FHidden: Boolean;
    FBounds: TRect;
    FOnChanged: TdxNotifyEventHandler;
    procedure SetBounds(const Value: TRect);
  protected
    procedure Changed;
    procedure DrawCore(AHdc: THandle); virtual;
  public
    constructor Create;

    procedure Draw(AGraphics: TdxGraphics);
    function ShouldDrawCaret(ADocumentModel: TdxDocumentModel): Boolean; virtual;
    function GetCaretPosition(AView: TdxRichEditView): TdxCaretPosition; virtual;

    property Bounds: TRect read FBounds write SetBounds;
    property IsHidden: Boolean read FHidden write FHidden;
    property OnChanged: TdxNotifyEventHandler read FOnChanged write FOnChanged;
  end;

  { TdxDragCaret }

  TdxDragCaret = class(TdxCaret)
  strict private
    FPosition: TdxDragCaretPosition;
  protected
    procedure DrawCore(AHdc: THandle); override;
  public
    constructor Create(AView: TdxRichEditView);
    destructor Destroy; override;

    function ShouldDrawCaret(ADocumentModel: TdxDocumentModel): Boolean; override;
    function GetCaretPosition(AView: TdxRichEditView): TdxCaretPosition; override;
    procedure SetLogPosition(ALogPosition: TdxDocumentLogPosition);
  end;

  { TdxDocumentSelectionLayout }

  TdxDocumentSelectionLayout = class(TcxIUnknownObject, IdxSelectionLayoutItem)
  protected
    function GetType: TClass;
  private
    FUpdatedSuccessfully: Boolean;
    FHotZones: TdxHotZoneCollection;
    FNestedItems: TObjectDictionary<Integer, TdxSelectionLayoutItemList>;
    FSelectionLayout: TdxSelectionLayout;
    FPieceTable: TdxPieceTable;
    FStart: TdxDocumentLayoutPosition;
    FEnd: TdxDocumentLayoutPosition;
    FDocumentLayout: TdxDocumentLayout;
    function FindCellViewInfo(AModelCell: TdxTableCell; AColumn: TdxColumn): TdxTableCellViewInfo;
    function GetView: TdxRichEditView;
    function GetDocumentModel: TdxDocumentModel;
    function FindFloatingObject(AFloatingObjects: TdxFloatingObjectBoxList; AIndex: TdxRunIndex): TdxFloatingObjectBox; overload;
    function FindFloatingObject(APage: TdxPage; AIndex: TdxRunIndex): TdxFloatingObjectBox; overload;
    function TryToCreateRectangularObjectSelectionLayoutForSinglePosition(AStart, AEnd: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
    function TryToCreateRectangularObjectSelectionLayoutByBox(ALayoutPos: TdxDocumentLayoutPosition;
      ALogPos: TdxDocumentLogPosition): TdxRectangularObjectSelectionLayout;
    function UpdateNestedItems: Boolean;

    procedure TryAddFloatingObjectsToSelection;
    function TryCreatePictureSelection: Boolean;
    function TryGetNextPosition(var APos: TdxDocumentLayoutPosition): Boolean;
    function TryGetNextPositionCore(AParagraph: TdxParagraph; ACell: TdxTableCell;
      var APos: TdxDocumentLayoutPosition): Boolean;
    function TryCreateRectangularObjectSelectionLayout(AStart, AEnd: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
    function TryCreateRectangularObjectSelectionLayoutByAnchorRun(ARunInfo: TdxDocumentModelPosition;
      out APos: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
    function TryToCreateRectangularObjectSelectionLayoutByField(AField: TdxField): TdxRectangularObjectSelectionLayout;
    function TryGetNextPositionBySelectionLayoutItem(AStop: TdxDocumentLayoutPosition; AVisibleEndLogPosition: TdxDocumentLogPosition; ACell: TdxTableCell; var APos: TdxDocumentLayoutPosition): Boolean;
    function TryGetNextPositionBySelectionLayoutItemCore(const AItem: IdxSelectionLayoutItem; AStop: TdxDocumentLayoutPosition; AVisibleEndLogPosition: TdxDocumentLogPosition; var APos: TdxDocumentLayoutPosition): Boolean;

    property SelectionLayout: TdxSelectionLayout read FSelectionLayout;
    property NestedItems: TObjectDictionary<Integer, TdxSelectionLayoutItemList> read FNestedItems;
  protected
    function GetBox(APos: TdxDocumentLayoutPosition): TdxPage;
    function CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition; virtual;
    procedure AddNestedItem(APageIndex: Integer; const AItem: IdxSelectionLayoutItem);
    function UpdateCore: Boolean; virtual;

    property PieceTable: TdxPieceTable read FPieceTable;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property View: TdxRichEditView read GetView;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ASelectionLayout: TdxSelectionLayout; AStart, AEnd: TdxDocumentLayoutPosition;
      APieceTable: TdxPieceTable);
    destructor Destroy; override;
    procedure ForceUpdateForEmptySelection; virtual;
    procedure AddToPageSelection(APage: TdxPage; ATarget: TdxPageSelectionLayoutsCollection); virtual;
    procedure Draw(const ASelectionPainter: IdxSelectionPainter);
    function Update: Boolean; virtual;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean;

    property HotZones: TdxHotZoneCollection read FHotZones;
    property UpdatedSuccessfully: Boolean read FUpdatedSuccessfully;
    property Start: TdxDocumentLayoutPosition read FStart;
    property &End: TdxDocumentLayoutPosition read FEnd;
  end;

  { TdxTextBoxDocumentSelectionLayout }

  TdxTextBoxDocumentSelectionLayout = class(TdxDocumentSelectionLayout)
  strict private
    FTextBoxFrameSelection: TdxRectangularObjectSelectionLayout;
  protected
    function UpdateCore: Boolean; override;
    procedure TryToAddTextBoxFrameSelectionLayout(AStart: TdxDocumentLayoutPosition; AEnd: TdxDocumentLayoutPosition); virtual;
  public
    procedure AddToPageSelection(APage: TdxPage; AWhere: TdxPageSelectionLayoutsCollection); override;
    procedure ForceUpdateForEmptySelection; override;
  end;

  { TdxSelectionLayout }

  TdxSelectionLayout = class
  private
    FView: TdxRichEditView;
    FPreferredPageIndex: Integer;
    FDocumentSelectionLayouts: TdxObjectList<TdxDocumentSelectionLayout>;
    function GetFirstDocumentSelectionLayout: TdxDocumentSelectionLayout;
    function GetDocumentLayout: TdxDocumentLayout;
    function GetPreferredPageIndex: Integer;
    procedure SetPreferredPageIndex(const Value: Integer);
    function GetStartLayoutPosition: TdxDocumentLayoutPosition;
    function GetEndLayoutPosition: TdxDocumentLayoutPosition;
    function GetLastDocumentSelectionLayout: TdxDocumentSelectionLayout;
  protected
    function CreateDocumentSelectionLayout(AStart, AEnd: TdxDocumentLayoutPosition): TdxDocumentSelectionLayout; virtual;

    property FirstDocumentSelectionLayout: TdxDocumentSelectionLayout read GetFirstDocumentSelectionLayout;
    property DocumentLayout: TdxDocumentLayout read GetDocumentLayout;
  public
    constructor Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
    destructor Destroy; override;

    function CalculateHotZone(AResult: TdxRichEditHitTestResult; AView: TdxRichEditView): TdxHotZone; virtual;
    procedure Invalidate; virtual;
    function IsSelectionStartFromBeginRow: Boolean; virtual;
    procedure Update; virtual;
    function CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition; virtual;
    function HitTest(ALogPosition: TdxDocumentLogPosition; ALogicalPoint: TPoint): Boolean;
    function GetPageSelection(APage: TdxPage): TdxPageSelectionLayoutsCollection; virtual;

    property PreferredPageIndex: Integer read GetPreferredPageIndex write SetPreferredPageIndex;
    property StartLayoutPosition: TdxDocumentLayoutPosition read GetStartLayoutPosition;
    property EndLayoutPosition: TdxDocumentLayoutPosition read GetEndLayoutPosition;
    property LastDocumentSelectionLayout: TdxDocumentSelectionLayout read GetLastDocumentSelectionLayout;
    property View: TdxRichEditView read FView;
  end;

  { TdxTextBoxSelectionLayout }

  TdxTextBoxSelectionLayout = class(TdxSelectionLayout)
  protected
    function CreateDocumentSelectionLayout(AStart: TdxDocumentLayoutPosition; AEnd: TdxDocumentLayoutPosition): TdxDocumentSelectionLayout; override;
  public
    function CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition; override;
  end;

  { TdxHeaderFooterSelectionLayout }

  TdxHeaderFooterSelectionLayout = class(TdxSelectionLayout)
  public
    function CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition; override;
  end;

  { TdxSpellCheckerControllerBase }

  TdxSpellCheckerControllerBase = class abstract
  public
    procedure CheckPages(AFrom: Integer); virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure ResetCore; virtual; abstract;
  end;

  { TdxBackgroundFormatter }

  TdxBackgroundFormatter = class
  public const
    Infinite = -1;
  public type
    TCommand = (
      Shutdown,
      PerformSecondaryLayout,
      PerformPrimaryLayout,
      None
    );

    TCommands = array[TCommand] of TSimpleEvent;

    TWorkThread = class(TThread)
    private type
      TWaitCommandHandles = array[TCommand] of THandle;
      TWaitContinueLayoutHandles = array[0..1] of THandle;
    private
      FFormatter: TdxBackgroundFormatter;
      function GetWaitCommandHandles: TWaitCommandHandles;
      function GetWaitContinueLayoutHandles: TWaitContinueLayoutHandles;
      function WaitForContinueLayoutAllowed: Boolean;
    protected
      function WorkBody: Boolean;
      procedure Execute; override;
    public
      constructor Create(AFormatter: TdxBackgroundFormatter);
    end;

  private
    FLock: TObject;
    FWorkThread: TWorkThread;
    FContinueLayout: TSimpleEvent;
    FSecondaryLayoutComplete: TSimpleEvent;
    FCommands: TCommands;
    FCurrentPosition: TdxDocumentModelPosition;
    FDocumentFormatter: TdxDocumentFormatter;
    FSecondaryLayoutStart: TdxDocumentModelPosition;
    FSecondaryLayoutEnd: TdxDocumentModelPosition;
    FSecondaryFormatter: TdxParagraphFinalFormatter;
    FSpellCheckerController: TdxSpellCheckerControllerBase;
    FPerformPrimaryLayoutUntil: TdxPredicate<TdxDocumentModelPosition>;
    FPrimaryLayoutComplete: Boolean;
    FController: TdxDocumentFormattingController;
    FResetSecondaryLayoutFromPage: Integer;
    FDocumentBeginUpdateCount: Integer;
    FThreadSuspendCount: Integer;
    FCommentPadding: TdxCommentPadding;
    FOnPageFormattingComplete: TdxPageFormattingCompleteEventHandler;
    FPredicatePos: TdxDocumentModelPosition;
    function PredicateDocumentModelPosition(const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean;

    function GetDocumentLayout: TdxDocumentLayout;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function GetAlreadySuspended: Boolean;
    function HandleCommand(ACommand: TCommand): Boolean;
    function IsTableFormattingComplete(const ANextPosition: TdxDocumentModelPosition): Boolean;
    procedure RefreshSecondaryLayout(AFirstPageIndex: Integer);
    procedure SetSpellCheckerController(const Value: TdxSpellCheckerControllerBase);
  protected
    function CreateParagraphFinalFormatter(ADocumentLayout: TdxDocumentLayout): TdxParagraphFinalFormatter; virtual;

    procedure CheckExecutedAtWorkerThread; virtual;
    procedure CheckExecutedAtUIThread; virtual;

    procedure InitializeWorkThread; virtual;
    procedure DoneWorkThread; virtual;
    procedure SetCommandEvent(ACommand: TCommand); overload; virtual;
    procedure SetCommandEvent(AEvent: TSimpleEvent); overload; virtual;
    procedure ResetCommandEvent(ACommand: TCommand); overload; virtual;
    procedure ResetCommandEvent(AEvent: TSimpleEvent); overload; virtual;

    procedure DoBeginDocumentRendering; virtual;

    procedure PerformSecondaryLayout; virtual;
    procedure PerformSecondaryLayoutCore; virtual;
    function DoPerformPrimaryLayout: TdxFormattingProcessResult;
    procedure PerformPrimaryLayout; virtual;
    procedure PerformPrimaryLayoutCore; virtual;
    function IsPrimaryLayoutComplete(const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean; virtual;
    function ShouldResetPrimaryLayout(const AFrom, ATo: TdxDocumentModelPosition): Boolean; virtual;
    function ShouldResetSecondaryLayout(const AFrom, ATo: TdxDocumentModelPosition): Boolean; virtual;
    procedure ResetSecondaryLayout; virtual;

    procedure ResetCalculators;
    procedure ResetPrimaryLayout; overload; virtual;
    procedure ResetPrimaryLayout(const AFrom, ATo: TdxDocumentModelPosition); overload; virtual;

    property ContinueLayout: TSimpleEvent read FContinueLayout;
    property SecondaryLayoutComplete: TSimpleEvent read FSecondaryLayoutComplete;
    property Commands: TCommands read FCommands;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property PrimaryLayoutComplete: Boolean read FPrimaryLayoutComplete;
  public
    constructor Create(AController: TdxDocumentFormattingController; const ACommentPadding: TdxCommentPadding);
    destructor Destroy; override;
    procedure Start; virtual;

    procedure BeginDocumentRendering(APerformPrimaryLayoutUntil: TdxPredicate<TdxDocumentModelPosition>);
    procedure EndDocumentRendering;
    procedure BeginDocumentUpdate;
    procedure EndDocumentUpdate;
    procedure PerformPageSecondaryFormatting(APage: TdxPage); virtual;
    procedure WaitForPrimaryLayoutReachesLogPosition(ALogPosition: TdxDocumentLogPosition);
    procedure WaitForPrimaryLayoutReachesPreferredPage(APreferredPageIndex: Integer);
    procedure WaitForPagePrimaryLayoutComplete(APage: TdxPage);
    procedure WaitForPrimaryLayoutComplete;
    procedure WaitForSecondaryLayoutReachesPosition(const APos: TdxDocumentModelPosition);
    procedure NotifyDocumentChanged(const AFrom, ATo: TdxDocumentModelPosition; ADocumentLayoutResetType: TdxDocumentLayoutResetType);
    procedure UpdateSecondaryPositions(const AFrom, ATo: TdxDocumentModelPosition);

    function SuspendWorkThread: Boolean; virtual;
    procedure ResumeWorkThread; virtual;

    procedure OnNewMeasurementAndDrawingStrategyChanged; virtual;
    procedure ResetSecondaryFormattingForPage(APage: TdxPage; APageIndex: Integer);

    procedure RaisePageFormattingComplete(E: TdxPageFormattingCompleteEventArgs);
    procedure SubscribeToFormattingControllerEvents;
    procedure UnsubscribeFromFormattingControllerEvents;
    procedure OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);

    property PageFormattingComplete: TdxPageFormattingCompleteEventHandler read FOnPageFormattingComplete;

    property AlreadySuspended: Boolean read GetAlreadySuspended;
    property DocumentFormatter: TdxDocumentFormatter read FDocumentFormatter;
    property DocumentLayout: TdxDocumentLayout read GetDocumentLayout;
    property SecondaryLayoutStart: TdxDocumentModelPosition read FSecondaryLayoutStart;
    property SecondaryLayoutEnd: TdxDocumentModelPosition read FSecondaryLayoutEnd;
    property SpellCheckerController: TdxSpellCheckerControllerBase read FSpellCheckerController write SetSpellCheckerController;
    property WorkThread: TWorkThread read FWorkThread;
    property CommentPadding: TdxCommentPadding read FCommentPadding;
  end;

  { TdxRichEditViewInfo }

  TdxRichEditViewInfo = class
  strict private
    FPageViewInfos: TdxPageViewInfoCollection;
    FOwnedPageViewInfos: TdxPageViewInfoCollection;
    FOwnedRows: TdxOwnedRowInfoList;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    destructor Destroy; override;
    procedure Clear;

    property PageViewInfos: TdxPageViewInfoCollection read FPageViewInfos;
    property OwnedPageViewInfos: TdxPageViewInfoCollection read FOwnedPageViewInfos;
    property OwnedRows: TdxOwnedRowInfoList read FOwnedRows;
  end;

  TdxDrawDelegate = reference to procedure (AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
  TdxDrawAtPageDelegate = reference to procedure (AGraphics: TdxGraphics);
  TdxDrawReversibleDelegate = reference to procedure (AHdc: THandle; const ABounds: TRect);

  { TdxRichEditPaintersBase<TdxLayoutItem, TdxPainter> }

  TdxRichEditPaintersBase<TLayoutItem; TPainter: class> = class
  private
    FDefaultPainter: TPainter;
    FPainters: TObjectDictionary<TClass, TPainter>;
    procedure InternalAdd(AType: TClass; APainter: TPainter);
    procedure InternalRemove(AType: TClass);
  protected
    property DefaultPainter: TPainter read FDefaultPainter;
    property Painters: TObjectDictionary<TClass, TPainter> read FPainters;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddDefault(APainter: TPainter);
    procedure RemoveDefault;
    procedure Add<T: class>(APainter: TPainter);
    procedure Remove<T: class>;
    function Get(AType: TClass): TPainter;
  end;

  { TdxRichEditSelectionPainters }

  TdxRichEditSelectionPainters = class(TdxRichEditPaintersBase<IdxSelectionLayoutItem, TdxRichEditSelectionPainter>);

  { TdxRichEditViewPainter }

  TdxRichEditViewPainter = class abstract (TcxIUnknownObject)
  strict private
    FView: TdxRichEditView;
    FControl: IdxRichEditControl;
    FSelectionPainters: TdxRichEditSelectionPainters;
    FHoverPainters: TdxRichEditHoverPainters;
    FDecoratorPainters: TList<IdxDecoratorPainter>;
    FCache: TdxGraphics;
    FGraphics: TdxGraphics;
    FOriginalTransform: TdxGPMatrix;
    FPhysicalLeftInvisibleWidth: Integer;
    FMinReadableTextHeight: Integer;
    function BeginDrawPageContentInPixels(APageViewInfo: TdxPageViewInfo): TdxGPMatrix;
    function ClipPageContentForDrawingInPixels(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter): TdxRectF;
    procedure EndDrawPageContentInPixels(AOldTransform: TdxGPMatrix);
    function GetDocumentModel: TdxDocumentModel;
  protected
    FOriginalDrawPagesTransform: TdxGPMatrix;
    FZoomModifier: TdxHdcZoomModifier;
    FOriginModifier: TdxHdcOriginModifier;
    FOldClipBounds: TdxRectF;
    FPixelHdcDpiModifier: TdxHdcDpiModifier;
    function GetLookAndFeel: TcxLookAndFeel; virtual;
    procedure BeginDraw(AGraphics: TdxGraphics); virtual;
    procedure AddSelectionPainters(AGraphics: TdxGraphics); virtual;
    procedure CacheInitialize(AGraphics: TdxGraphics);
    procedure EndDraw; virtual;
    procedure CacheDispose;
    function BeginDrawPagesContent: TdxGPMatrix; virtual;
    procedure EndDrawPagesContent; virtual;
    procedure BeginDrawPageContent(APage: TdxPageViewInfo; ATransform: TdxGPMatrix); virtual;
    procedure ClipPageContent(APage: TdxPageViewInfo; APainter: TdxPainter); virtual;
    procedure EndDrawPageContent(APage: TdxPageViewInfo; ATransform: TdxGPMatrix); virtual;
    procedure BeginDrawInPixels(AGraphics: TdxGraphics); virtual;
    procedure EndDrawInPixels; virtual;
    procedure DrawPageContent(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter); virtual;
    procedure DrawPageComment(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter); virtual;
    function CommentsVisible(ACount: Integer): Boolean;
    procedure DrawCommentsBackground(APageViewInfo: TdxPageViewInfo; AGraphics: TdxGraphics); virtual;
    function GetPageBounds(APage: TdxPageViewInfo): TRect; virtual;
    procedure DrawPagesContent(ATransform: TdxGPMatrix); virtual;
    procedure RestoreOldClipBounds(APainter: TdxPainter); virtual;
    function GetNewClientBounds(const AClipBounds: TRect; const AOldClipBounds: TdxRectF): TdxRectF; virtual;
    procedure DrawBoxesInPixels(const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    procedure IntersectClipBounds(APainter: TdxPainter; const AOldClipBounds: TdxRectF; X: Single; Y: Single; AWidth: Single; AHeight: Single);
    function CalculateRulerHeight: Integer; virtual;
    procedure DecorateTables(APainter: TdxPainter); virtual;
    procedure DecorateTablesCore(AController: TdxTableViewInfoController; APainter: TdxPainter);
    procedure DrawDecorators(APainter: TdxPainter); overload; virtual;
    procedure ApplyGraphicsTransform(AGraphics: TdxGraphics; ACustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection); virtual;
    procedure DrawCaretCore(AGraphics: TdxGraphics; ACaret: TdxCaret); virtual;
    procedure DrawReversibleHorizontalLine(AGraphics: TdxGraphics; const ABounds: TRect); overload; virtual;
    procedure DrawReversibleHorizontalLine(AHdc: THandle; const ABounds: TRect); overload; virtual;
    procedure DrawReversibleVerticalLine(AGraphics: TdxGraphics; const ABounds: TRect); overload; virtual;
    procedure DrawReversibleVerticalLine(AHdc: THandle; const ABounds: TRect); overload; virtual;
    procedure DrawReversibleFrame(AGraphics: TdxGraphics; const ABounds: TRect); overload; virtual;
    procedure DrawReversibleFrame(AHdc: THandle; const ABounds: TRect); overload; virtual;
    procedure DrawReversible(AHdc: THandle; const ABounds: TRect; const ADraw: TdxDrawReversibleDelegate); virtual;
    procedure DrawReversibleFrameCore(AHdc: THandle; const ABounds: TRect); virtual;
    procedure DrawReversibleHorizontalLineCore(AHdc: THandle; const ABounds: TRect); virtual;
    procedure DrawReversibleVerticalLineCore(AHdc: THandle; const ABounds: TRect); virtual;
    procedure DrawPageSelection(AGraphics: TdxGraphics; APage: TdxPageViewInfo); virtual;
    procedure DrawPageSelectionCore(const ASelection: IdxSelectionLayoutItem);
    procedure DrawHover(APainter: TdxPainter); virtual;
    procedure DrawHoverCore(const AHoverLayoutItem: IdxHoverLayoutItem; APainter: TdxPainter);
    procedure DrawEmptyPages(AGraphics: TdxGraphics); virtual;
    procedure ResetCache; virtual;
    procedure DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo); virtual; abstract;
    procedure DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); virtual; abstract;
    procedure DrawEmptyExtensionComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); virtual; abstract;

    property DecoratorPainters: TList<IdxDecoratorPainter> read FDecoratorPainters;
    property SelectionPainters: TdxRichEditSelectionPainters read FSelectionPainters;
    property HoverPainters: TdxRichEditHoverPainters read FHoverPainters;
    property Control: IdxRichEditControl read FControl;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Cache: TdxGraphics read FCache;
    property Graphics: TdxGraphics read FGraphics;
  public
    constructor Create(AView: TdxRichEditView);
    destructor Destroy; override;

    procedure AddDecoratorPainter(const APainter: IdxDecoratorPainter);
    procedure DrawDecorators(AGraphics: TdxGraphics); overload; virtual;
    procedure Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter); virtual;
    function GetCommentBounds(ACommentViewInfo: TdxCommentViewInfo): TRect;
    procedure DrawAtPageCore(AGraphics: TdxGraphics; APage: TdxPageViewInfo; const ADraw: TdxDrawAtPageDelegate); virtual;
    procedure DrawCaretAtPage(AGraphics: TdxGraphics; ACaret: TdxCaret; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleHorizontalLineAtPage(AGraphics: TdxGraphics; Y: Integer; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleVerticalLineAtPage(AGraphics: TdxGraphics; X: Integer; APage: TdxPageViewInfo); virtual;
    procedure DrawReversibleFrameAtPage(AGraphics: TdxGraphics; const ABounds: TRect; APage: TdxPageViewInfo); virtual;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property View: TdxRichEditView read FView;
  end;

  { TdxRichEditViewBackgroundPainter }

  TdxRichEditViewBackgroundPainter = class abstract
  private
    FView: TdxRichEditView;
  protected
    function GetActualPageBackColor: TdxAlphaColor; virtual;
    function GetLookAndFeel: TcxLookAndFeel; virtual;
  public
    constructor Create(AView: TdxRichEditView); virtual;
    procedure Draw(AGraphics: TdxGraphics; const ABounds: TRect); virtual;

    property ActualPageBackColor: TdxAlphaColor read GetActualPageBackColor;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property View: TdxRichEditView read FView;
  end;

  { TdxRichEditViewColorBasedBackgroundPainter }

  TdxRichEditViewColorBasedBackgroundPainter = class(TdxRichEditViewBackgroundPainter);

  { TdxRichEditViewSkinBackgroundPainter }

  TdxRichEditViewSkinBackgroundPainter = class(TdxRichEditViewBackgroundPainter)
  strict private
    FBackground: TBitmap;
    FSkinInfo: TdxSkinInfo;
  protected
    function GetBackground(const ABounds: TRect): TBitmap; virtual;
    property Background: TBitmap read FBackground;
    property SkinInfo: TdxSkinInfo read FSkinInfo;
  public
    constructor Create(AView: TdxRichEditView); override;
    destructor Destroy; override;

    procedure Draw(AGraphics: TdxGraphics; const ABounds: TRect); override;
  end;

  { TdxRichEditView }

  TdxRichEditView = class abstract(TPersistent)
  public const
    MinZoomFactor     = 0.0001;
    MaxZoomFactor     = 10000.0;
    DefaultZoomFactor = 1.0;
    DefaultMinWidth   = 5;
    DefaultMinHeight  = 5;
  private
    FBackColor: TdxAlphaColor;
    FControl: IdxRichEditControl;
    FBounds: TRect;
    FZoomFactor: Single;
    FAllowDisplayLineNumbers: Boolean;
    FDocumentLayout: TdxDocumentLayout;
    FFormattingController: TdxDocumentFormattingController;
    FVerticalScrollController: TdxRichEditViewVerticalScrollController;
    FHorizontalScrollController: TdxRichEditViewHorizontalScrollController;
    FPageViewInfoGenerator: TdxPageViewInfoGenerator;
    FSelectionLayout: TdxSelectionLayout;
    FSuspendFormattingCount: Integer;
    FHoverLayout: IdxHoverLayoutItem;
    FCaretPosition: TdxCaretPosition;
    FOldFirstPageIndex: Integer;
    FOldLastPageIndex: Integer;
    FTableController: TdxTableViewInfoController;
    FViewInfo: TdxRichEditViewInfo;
    FPadding: TdxRichEditControlPadding;
    FPageViewInfoGenerationComplete: Boolean;
    FPageViewInfoGenerationIsAboutToComplete: Boolean;
    FOnBackColorChanged: TdxEventHandler;
    FOnZoomChanging: TdxEventHandler;
    FOnZoomChanged: TdxEventHandler;
    procedure DocumentModelActivePieceTableTextBoxHitTest(var ARequest: TdxRichEditHitTestRequest;
      var AResult: TdxRichEditHitTestResult; APageViewInfo: TdxPageViewInfo);
    function IsPaddingStored: Boolean;
    function IsPrimaryFormattingCompleteForVisibleHeight2(const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean;
    procedure PaddingChanged(Sender: TObject);
    function EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function IsActivePieceTableFloatingObjectBox(const ABox: TdxFloatingObjectBox): Boolean;
    function IsActiveFloatingObjectTextBox(const ABox: TdxFloatingObjectBox): Boolean;
    function IsFloatingObjectHit(ABox: TdxFloatingObjectBox; APoint: TPoint): Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetScaleFactor: Single;
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBounds(const Value: TRect);
    procedure SetZoomFactor(Value: Single);
    procedure SetAllowDisplayLineNumbers(const Value: Boolean);
    function GetFormatter: TdxBackgroundFormatter;
    procedure SetPageViewInfoGenerationComplete(const Value: Boolean);
    function GetPageViewInfos: TdxPageViewInfoCollection; inline;
    procedure SetPadding(Value: TdxRichEditControlPadding);
    function GetTextColors: TdxTextColors;

    class function SetNewPosition(const APos: TdxDocumentModelPosition; AParagraph: TdxParagraph): TdxDocumentModelPosition;

    function IsAllowDisplayLineNumbersStored: Boolean;
    function IsZoomFactorStored: Boolean;
    function IsBackColorStored: Boolean;
    procedure SetCaretPosition(const Value: TdxCaretPosition);
    procedure SetSelectionLayout(const Value: TdxSelectionLayout);
  protected
    procedure ApplyChanges(APieceTable: TdxPieceTable); virtual;
    function CreatePadding: TdxRichEditControlPadding; virtual;
    function GetActualPadding: TRect; virtual;
    procedure ResetPadding; virtual;
    function GetDefaultAllowDisplayLineNumbers: Boolean; virtual;
    function GetFixedLeftTextBorderOffset: Integer; virtual;
    function GetMatchHorizontalTableIndentsToTextEdge: Boolean; virtual; abstract;
    function GetShowHorizontalRulerByDefault: Boolean; virtual; abstract;
    function GetShowVerticalRulerByDefault: Boolean; virtual; abstract;
    function GetType: TdxRichEditViewType; virtual; abstract;
    procedure OnViewPaddingChanged; virtual;
    function CreateDocumentLayout(ADocumentModel: TdxDocumentModel; const AMeasurerProvider: IdxBoxMeasurerProvider): TdxDocumentLayout; virtual;
    function CreateDocumentFormattingController: TdxDocumentFormattingController; virtual; abstract;
    function CreatePageViewInfoGenerator: TdxPageViewInfoGenerator; virtual; abstract;
    procedure OnZoomFactorChanging; virtual;
    procedure OnZoomFactorChanged(AOldValue, ANewValue: Single); virtual;
    procedure PerformZoomFactorChanged; virtual;
    procedure OnZoomFactorChangedCore; virtual;
    procedure ShowCaret; virtual;
    procedure HideCaret; virtual;
    function CalculateDocumentLayoutResetType(AChanges: TdxDocumentModelDeferredChanges): TdxDocumentLayoutResetType; virtual;
    function CalculateResetFromPosition(APieceTable: TdxPieceTable; AChanges: TdxDocumentModelDeferredChanges; AResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition; virtual;
    function EnsureTopLevelParagraph(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy; virtual;
    procedure ValidateSelectionInterval; virtual;
    function GetVisibleSelectionPosition: TdxDocumentLogPosition; virtual;
    function GetVisibleSelectionStart: TdxDocumentLogPosition;
    function GetVisibleSelectionEnd: TdxDocumentLogPosition;
    procedure OnResetSecondaryFormattingForPage(ASender: TObject; E: TdxResetSecondaryFormattingForPageArgs); virtual;
    procedure OnPageFormattingStarted(ASender: TObject; E: TdxPageFormattingCompleteEventArgs); virtual;
    procedure OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs); virtual;
    procedure SetOptimalHorizontalScrollbarPosition; virtual;
    procedure SetOptimalHorizontalScrollbarPositionCore; virtual;
    function CalculateOptimalHorizontalScrollbarPosition(APageViewInfo: TdxPageViewInfo): Integer; virtual;
    function ChoosePageViewInfoForOptimalHorizontalScrollbarPosition: TdxPageViewInfo; virtual;
    procedure SubscribeDocumentLayoutEvents; virtual;
    procedure UnsubscribeDocumentLayoutEvents; virtual;
    procedure BeforeCreateDetailRowHandler(ASender: TObject); virtual;
    procedure AfterCreateDetailRowHandler(ASender: TObject); virtual;
    procedure ScrollToPage(APageIndex: Integer); virtual;
    procedure EnsureFormattingCompleteForLogPosition(ALogPosition: TdxDocumentLogPosition); virtual;
    procedure EnsurePageSecondaryFormattingComplete(APage: TdxPage); virtual;
    function CreateTransformMatrix(const AClientBounds: TRect): TdxTransformMatrix; virtual;
    function GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow; virtual;
    procedure HitTest(APage: TdxPage; const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult); virtual;
    function PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean; virtual;
    procedure PerformResize(const ABounds: TRect); virtual;
    procedure OnAutoSizeModeChanged; virtual; abstract;
    procedure OnResizeCore; virtual;
    procedure SuspendFormatting; virtual;
    procedure ResumeFormatting; virtual;
    function HitTestByPhysicalPoint(const APt: TPoint; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TRect; virtual;
    function GetPhysicalBounds(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect; virtual;
    function ExpandClipBoundsToPaddings(const AClipBounds: TRect): TRect; virtual;
    function CalculateVisiblePageBounds(const APageBounds: TRect; APageViewInfo: TdxPageViewInfo): TRect; virtual;
    procedure ResetBackColor; virtual;
    procedure LayoutOptionsChanged(ASender: TObject; Args: TdxRichEditNotificationOptionsChangedArgs); virtual;

    procedure ActivateScrollBars; virtual;
    procedure DeactivateScrollBars; virtual;
    procedure EnsureVerticalScrollBarSynchronized; virtual;
    function GetPhysicalLeftInvisibleWidth: Integer; virtual;
    function HasSelection: Boolean; virtual;

    function GetActualBackColor: TdxAlphaColor; virtual;
    function GetMinWidth: Integer; virtual;
    function GetMinHeight: Integer; virtual;

    property FixedLeftTextBorderOffset: Integer read GetFixedLeftTextBorderOffset;
    property MinHeight: Integer read GetMinHeight;
    property MinWidth: Integer read GetMinWidth;
    property TextColors: TdxTextColors read GetTextColors;
    property PageViewInfoGenerationIsAboutToComplete: Boolean read FPageViewInfoGenerationIsAboutToComplete write FPageViewInfoGenerationIsAboutToComplete;
    property PageViewInfoGenerationComplete: Boolean read FPageViewInfoGenerationComplete write SetPageViewInfoGenerationComplete;
    property ViewInfo: TdxRichEditViewInfo read FViewInfo;
  public
    constructor Create(const AControl: IdxRichEditControl); virtual;
    destructor Destroy; override;

    procedure StopFormatting; virtual;

    class function EnsurePositionVisibleWhenHiddenTextNotShown(ADocumentModel: TdxDocumentModel;
      const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition; static;

    procedure CheckExecutedAtUIThread; inline;
    procedure CheckExecutedAtBackgroundThread; inline;

    function CreateLogicalPoint(const AClientBounds: TRect; const APoint: TPoint): TPoint; virtual;
    function CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter; virtual; abstract;

    procedure Activate(const AViewBounds: TRect); virtual;
    procedure BeginDocumentRendering; virtual;
    procedure CorrectZoomFactor; virtual;
    function CalcBestSize(AFixedWidth: Boolean): TSize; virtual; abstract;
    function CalculateFirstInvisiblePageIndexBackward: Integer; virtual;
    function CalculateFirstInvisiblePageIndexForward: Integer; virtual;
    function CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect; virtual;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
    function CreatePainter: TdxRichEditViewPainter; virtual; abstract;
    function CreateBackgroundPainter: TdxRichEditViewBackgroundPainter; virtual; abstract;
    function CreatePhysicalPoint(APageViewInfo: TdxPageViewInfo; const APoint: TPoint): TPoint; virtual;
    procedure Deactivate; virtual;
    procedure EndDocumentRendering; virtual;
    procedure EnforceFormattingCompleteForVisibleArea;
    procedure EnsureFormattingCompleteForSelection; virtual;
    procedure EnsureCaretVisibleOnResize; virtual;
    procedure OnActivePieceTableChanged; virtual;
    procedure OnLayoutUnitChanging; virtual;
    procedure OnLayoutUnitChanged; virtual;
    procedure EnsureCaretVisible(ACheckCaretVisibility: Boolean = True); virtual;
    procedure OnEndDocumentUpdate; virtual;
    procedure OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize: Boolean); overload;
    procedure OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean); overload; virtual;
    function CreateLogicalRectangle(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect; virtual;
    function CreatePhysicalRectangle(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect; overload; virtual;
    function CreatePhysicalRectangle(const APageViewInfoClientBounds, ABounds: TRect): TRect; overload; virtual;
    procedure ResetPages(AStrategy: TdxPageGenerationStrategyType); virtual;
    procedure GeneratePages; virtual;

    procedure EnsureFormattingCompleteForPreferredPage(APreferredPageIndex: Integer); virtual;
    function GetPageViewInfoFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfo; virtual;
    function LookupPageViewInfoByPage(APage: TdxPage): TdxPageViewInfo; virtual;
    procedure OnSelectionChanged; virtual;
    procedure ProcessSelectionChanges(const AChangeActions: TdxDocumentModelChangeActions);
    procedure SubscribePageFormattingComplete; virtual;
    procedure UnsubscribePageFormattingComplete; virtual;
    procedure UpdateCaretPosition; virtual;
    property Formatter: TdxBackgroundFormatter read GetFormatter;

    procedure OnBeginDocumentUpdate; virtual;
    procedure OnVerticalScroll; virtual;
    procedure OnHorizontalScroll; virtual;
    procedure UpdateHorizontalScrollBar; virtual;
    procedure UpdateVerticalScrollBar; virtual;

    function CreatePhysicalRectangleFast(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect; virtual;
    function CalculateNearestCharacterHitTest(const APoint: TPoint; APieceTable: TdxPieceTable): TdxRichEditHitTestResult; virtual;
    function CalculateNearestPageHitTest(const APoint: TPoint; AStrictHitIntoPageBounds: Boolean): TdxRichEditHitTestResult; virtual;
    function CalculateFloatingObjectHitTest(APage: TdxPage; const APoint: TPoint;
      const APredicate: TdxPredicate<TdxFloatingObjectBox>): TdxFloatingObjectBox; virtual;
    function CalculateFloatingObjectHitTestCore(const APoint: TPoint; AList: TdxFloatingObjectBoxList;
      const APredicate: TdxPredicate<TdxFloatingObjectBox>): TdxFloatingObjectBox; virtual;
    function CalculateHitTest(const APoint: TPoint; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxRichEditHitTestResult; virtual;
    function HitTestCore(var ARequest: TdxRichEditHitTestRequest; AStrictHitIntoPageBounds: Boolean): TdxRichEditHitTestResult; virtual;
		procedure RaiseZoomChanging;
		procedure RaiseZoomChanged; virtual;
		procedure RaiseBackColorChanged; virtual;

    function GetCursorBounds: TRect; virtual;
    function GetHyperlinkField(AHitTestResult: TdxRichEditHitTestResult): TdxField;
    procedure Visit(const AVisitor: IdxRichEditViewVisitor); virtual; abstract;

    property VerticalScrollController: TdxRichEditViewVerticalScrollController read FVerticalScrollController;
    property HorizontalScrollController: TdxRichEditViewHorizontalScrollController read FHorizontalScrollController;
    property MatchHorizontalTableIndentsToTextEdge: Boolean read GetMatchHorizontalTableIndentsToTextEdge;

    property ActualBackColor: TdxAlphaColor read GetActualBackColor;
    property ActualPadding: TRect read GetActualPadding;
    property AllowDisplayLineNumbers: Boolean read FAllowDisplayLineNumbers write SetAllowDisplayLineNumbers stored IsAllowDisplayLineNumbersStored;
    property Bounds: TRect read FBounds write SetBounds;
    property CaretPosition: TdxCaretPosition read FCaretPosition write SetCaretPosition;
    property Control: IdxRichEditControl read FControl;
    property DefaultHitTestPageAccuracy: TdxHitTestAccuracy read GetDefaultHitTestPageAccuracy;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FormattingController: TdxDocumentFormattingController read FFormattingController;
    property HoverLayout: IdxHoverLayoutItem read FHoverLayout write FHoverLayout;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property PageViewInfoGenerator: TdxPageViewInfoGenerator read FPageViewInfoGenerator;
    property PageViewInfos: TdxPageViewInfoCollection read GetPageViewInfos;
    property &Type: TdxRichEditViewType read GetType;
    property ZoomFactor: Single read FZoomFactor write SetZoomFactor stored IsZoomFactorStored;
    property Padding: TdxRichEditControlPadding read FPadding write SetPadding stored IsPaddingStored;
    property BackColor: TdxAlphaColor read FBackColor write SetBackColor stored IsBackColorStored;
    property BackColorChanged: TdxEventHandler read FOnBackColorChanged;
    property SelectionLayout: TdxSelectionLayout read  FSelectionLayout write SetSelectionLayout;
    property ScaleFactor: Single read GetScaleFactor;
    property ShowHorizontalRulerByDefault: Boolean read GetShowHorizontalRulerByDefault;
    property ShowVerticalRulerByDefault: Boolean read GetShowVerticalRulerByDefault;
    property TableController: TdxTableViewInfoController read FTableController;
    property ZoomChanging: TdxEventHandler read FOnZoomChanging;
    property ZoomChanged: TdxEventHandler read FOnZoomChanged;
  end;
  TdxRichEditViewClass = class of TdxRichEditView;

  { TdxPageBasedRichEditView }

  TdxPageBasedRichEditView = class abstract(TdxRichEditView)
  private
    FOnPageCountChanged: TdxEventHandler;
  protected
    function GetCurrentPageIndex: Integer; virtual;
    function GetPageCount: Integer; virtual;
    procedure RaisePageCountChanged;
    procedure OnPageCountChanged(ASender: TObject; E: TdxEventArgs);
  public
    procedure Activate(const AViewBounds: TRect); override;
    function CalcBestSize(AFixedWidth: Boolean): TSize; override;
    procedure Deactivate; override;
    procedure OnAutoSizeModeChanged; override;

    property CurrentPageIndex: Integer read GetCurrentPageIndex;
    property PageCount: Integer read GetPageCount;
    property PageCountChanged: TdxEventHandler read FOnPageCountChanged;
  end;

  { TdxRichEditCustomViewRepository }

  TdxRichEditCustomViewRepository = class(TcxOwnedPersistent)
  private
    FRichEditControl: IdxRichEditControl;
    FViews: TdxFastObjectList;
    function GetView(Index: Integer): TdxRichEditView;
    function GetViewCount: Integer;
  protected
    procedure AddView(AView: TdxRichEditView);
    procedure CreateViews; virtual; abstract;

    property RichEditControl: IdxRichEditControl read FRichEditControl;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); reintroduce; virtual;
    destructor Destroy; override;
    function GetViewByType(AType: TdxRichEditViewType): TdxRichEditView; virtual;

    property ViewCount: Integer read GetViewCount;
    property Views[Index: Integer]: TdxRichEditView read GetView; default;
  end;

  { TdxScrollBarAdapter }

  TdxScrollBarAdapter = class abstract (TcxIUnknownObject, IdxBatchUpdateHandler, IdxBatchUpdateable)
  private
    FScrollBar: IdxOfficeScrollbar;
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FAdapter: IdxPlatformSpecificScrollBarAdapter;
    FFactor: Double;
    FMinimum: Int64;
    FMaximum: Int64;
    FValue: Int64;
    FLargeChange: Int64;
    FSmallChange: Int64;
    FEnabled: Boolean;
    FOnScroll: TdxScrollEventHandler;
    procedure SetMinimum(const Value: Int64);
    procedure SetMaximum(const Value: Int64);
    procedure SetValue(const Value: Int64);
    procedure SetLargeChange(const Value: Int64);
    procedure SetEnabled(const Value: Boolean);
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    function GetIsUpdateLocked: Boolean;
    function GetScroll: TdxScrollEventHandler;
  protected
    function GetDeferredScrollBarUpdate: Boolean; virtual; abstract;
    function GetSynchronized: Boolean; virtual; abstract;
    procedure SetSynchronized(AValue: Boolean); virtual; abstract;

    procedure OnLastEndUpdateCore; virtual;
    procedure ValidateValues; virtual;
    procedure ValidateValuesCore; virtual;
    procedure SubscribeScrollbarEvents; virtual;
    procedure UnsubscribeScrollbarEvents; virtual;
    function ShouldSynchronize: Boolean; virtual;
    procedure OnScroll(ASender: TObject; E: TdxScrollEventArgs); virtual;
    procedure ApplyValuesToScrollBarCore; virtual;

    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;

    property DeferredScrollBarUpdate: Boolean read GetDeferredScrollBarUpdate;
    property Synchronized: Boolean read GetSynchronized write SetSynchronized;
    property Adapter: IdxPlatformSpecificScrollBarAdapter read FAdapter;
  public
    constructor Create(const AScrollBar: IdxOfficeScrollbar; const AAdapter: IdxPlatformSpecificScrollBarAdapter);
    destructor Destroy; override;
    procedure RaiseScroll(AArgs: TdxScrollEventArgs); virtual;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    procedure Activate; virtual;
    procedure Deactivate; virtual;
    function SynchronizeScrollBarAvoidJump: Boolean; virtual;
    function EnsureSynchronizedCore: Boolean;
    procedure EnsureSynchronized; virtual;
    procedure ApplyValuesToScrollBar; virtual;
    procedure RefreshValuesFromScrollBar; virtual;
    function CreateEmulatedScrollEventArgs(AEventType: TdxScrollEventType): TdxScrollEventArgs; virtual;
    function GetRawScrollBarValue: Integer;
    function SetRawScrollBarValue(AValue: Integer): Boolean;
    function GetPageUpRawScrollBarValue: Integer; virtual;
    function GetPageDownRawScrollBarValue: Integer; virtual;

    property Factor: Double read FFactor write FFactor;
    property Minimum: Int64 read FMinimum write SetMinimum;
    property Maximum: Int64 read FMaximum write SetMaximum;
    property Value: Int64 read FValue write SetValue;
    property LargeChange: Int64 read FLargeChange write SetLargeChange;
    property SmallChange: Int64 read FSmallChange write FSmallChange;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ScrollBar: IdxOfficeScrollbar read FScrollBar;
    property Scroll: TdxScrollEventHandler read GetScroll;
    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
  end;

  { TdxVerticalScrollBarAdapter }

  TdxVerticalScrollBarAdapter = class(TdxScrollBarAdapter)
  public
    FSynchronized: Boolean;
  protected
    function GetDeferredScrollBarUpdate: Boolean; override;
    function GetSynchronized: Boolean; override;
    procedure SetSynchronized(AValue: Boolean); override;
  end;

  { TdxHorizontalScrollBarAdapter }

  TdxHorizontalScrollBarAdapter = class(TdxScrollBarAdapter)
  protected
    function GetDeferredScrollBarUpdate: Boolean; override;
    function GetSynchronized: Boolean; override;
    procedure SetSynchronized(AValue: Boolean); override;
  end;

  { TdxOfficeScrollControllerBase }

  TdxOfficeScrollControllerBase = class abstract
  private
    FScrollBarAdapter: TdxScrollBarAdapter;
    FView: TdxRichEditView;
    function GetSupportsDeferredUpdate: Boolean;
  protected
    function GetScrollBar: IdxOfficeScrollbar; virtual; abstract;
    procedure Initialize; virtual;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    function CreateScrollBarAdapter: TdxScrollBarAdapter; virtual; abstract;
    procedure SubscribeScrollBarAdapterEvents; virtual;
    procedure UnsubscribeScrollBarAdapterEvents; virtual;
    procedure EmulateScroll(AEventType: TdxScrollEventType);
    procedure OnScroll(ASender: TObject; E: TdxScrollEventArgs); virtual; abstract;
    procedure SynchronizeScrollbar; virtual;
    procedure UpdateScrollBarAdapter; virtual; abstract;

    property SupportsDeferredUpdate: Boolean read GetSupportsDeferredUpdate;
    property ScrollBar: IdxOfficeScrollbar read GetScrollBar;
  public
    constructor Create(AView: TdxRichEditView);
    destructor Destroy; override;
    procedure UpdateScrollBar; virtual;
    function IsScrollPossible: Boolean; virtual;

    property ScrollBarAdapter: TdxScrollBarAdapter read FScrollBarAdapter;
    property View: TdxRichEditView read FView;
  end;

  { TdxRichEditViewHorizontalScrollController }

  TdxRichEditViewHorizontalScrollController = class abstract (TdxOfficeScrollControllerBase)
  protected
    function GetScrollBar: IdxOfficeScrollbar; override;

    function CreateScrollBarAdapter: TdxScrollBarAdapter; override;
    procedure OnScroll(ASender: TObject; E: TdxScrollEventArgs); override;
    procedure OnScrollCore(E: TdxScrollEventArgs); virtual;
    procedure UpdateScrollBarAdapter; override;
  public
    function GetLeftInvisibleWidth: Int64;
    function GetPhysicalLeftInvisibleWidth: Integer;
    procedure ScrollByLeftInvisibleWidthDelta(ALeftInvisibleWidthDelta: Int64); virtual;
    procedure ScrollToAbsolutePosition(AValue: Int64); virtual;
  end;

  { TdxRichEditViewVerticalScrollController }

  TdxRichEditViewVerticalScrollController = class abstract (TdxOfficeScrollControllerBase)
  protected
    function CreateScrollBarAdapter: TdxScrollBarAdapter; override;
    function IsScrollTypeValid(E: TdxScrollEventArgs): Boolean; virtual;
    procedure OnScroll(ASender: TObject; E: TdxScrollEventArgs); override;
    function CalculateScrollDelta(E: TdxScrollEventArgs): Integer; virtual;
    procedure ApplyNewScrollValue(AValue: Integer); virtual;
    procedure ApplyNewScrollValueToScrollEventArgs(E: TdxScrollEventArgs; AValue: Integer); virtual;
    function UpdatePageNumberOnScroll(E: TdxScrollEventArgs): Boolean; virtual;
    procedure UpdateScrollBarAdapter; override;
    function GetScrollBar: IdxOfficeScrollbar; override;
    function GetTopInvisibleHeight: Int64;
  public
    procedure ScrollByTopInvisibleHeightDelta(ATopInvisibleHeightDelta: Int64); virtual;
    procedure ScrollToAbsolutePosition(AValue: Int64); virtual;
    function ScrollPageUp: Boolean; virtual;
    function ScrollPageDown: Boolean; virtual;
    function ScrollLineUp: Integer; virtual;
    function ScrollLineDown: Integer; virtual;
    function ScrollLineUpDown(APhysicalOffset: Integer): Integer; virtual;
  end;

  { TdxScrollByPhysicalHeightCalculator }

  TdxScrollByPhysicalHeightCalculator = class
  strict private
    FView: TdxRichEditView;
  protected
    function CalculateScrollDeltaForInvisibleRows(APhysicalVerticalOffset: Integer): Int64;
    function CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset: Integer; out ADelta: Int64): Boolean; virtual; abstract;
    function LookupIntersectingRowIndexInInvisibleRows(AGenerator: TdxInvisiblePageRowsGenerator; Y: Integer): Integer;
    function CalculateFirstInvisiblePageIndex: Integer; virtual; abstract;
    function CalculateFirstInvalidPageIndex: Integer; virtual; abstract;
    function CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset: Integer): Integer; virtual; abstract;
    function CalculateVisibleRowsScrollDelta: Int64; virtual; abstract;
    function GetDefaultScrollDeltaForInvisibleRows: Int64; virtual; abstract;
    function CalculateRowsTotalVisibleHeight(AGenerator: TdxPageViewInfoGeneratorBase; ALastRowIndex: Integer; ABottomY: Integer): Int64;
  public
    constructor Create(AView: TdxRichEditView);
    function CalculateScrollDelta(APhysicalVerticalOffset: Integer): Int64; virtual;

    property View: TdxRichEditView read FView;
  end;

  { TdxScrollUpByPhysicalHeightCalculator }

  TdxScrollUpByPhysicalHeightCalculator = class(TdxScrollByPhysicalHeightCalculator)
  protected
    function CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset: Integer; out ADelta: Int64): Boolean; override;
    function CalculateFirstInvisiblePageIndex: Integer; override;
    function CalculateFirstInvalidPageIndex: Integer; override;
    function CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset: Integer): Integer; override;
    function CalculateVisibleRowsScrollDelta: Int64; override;
    function GetDefaultScrollDeltaForInvisibleRows: Int64; override;
  end;

  { TdxScrollDownByPhysicalHeightCalculator }

  TdxScrollDownByPhysicalHeightCalculator = class(TdxScrollByPhysicalHeightCalculator)
  protected
    function CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset: Integer; out ADelta: Int64): Boolean; override;
    function CalculateFirstInvisiblePageIndex: Integer; override;
    function CalculateFirstInvalidPageIndex: Integer; override;
    function CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset: Integer): Integer; override;
    function CalculateVisibleRowsScrollDelta: Int64; override;
    function GetDefaultScrollDeltaForInvisibleRows: Int64; override;
    function LookupIntersectingRowIndex(Y: Integer): Integer; virtual;
  end;

  { TdxNonPrintViewPageControllerBase }

  TdxNonPrintViewPageControllerBase = class(TdxPageController)
  protected
    function GetColumnBottom(AColumn: TdxColumn): Integer; virtual;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
  end;

  { TdxCopySelectionManager }

  TdxTextFragmentOptions = TdxRichEditTextFragmentOptions;
  PdxTextFragmentOptions = ^TdxTextFragmentOptions;

  TdxCopySelectionManager = class
  strict private
    FDocumentServer: IdxInnerControl;
    FFixLastParagraph: Boolean;
    FAllowCopyWholeFieldResult: Boolean;
    FKeepFieldCodeViewState: Boolean;
    FDefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions;
  protected
    function CreateDocumentModel(AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
      AFormattingCopyOptions: TdxFormattingCopyOptions; ASourcePieceTable: TdxPieceTable;
      ASelectionRanges: TdxSelectionRangeCollection; ASuppressFieldsUpdate: Boolean = False;
      AGetTextOptions: PdxTextFragmentOptions = nil): TdxDocumentModel;
    function IsLastParagraphRunSelected(APieceTable: TdxPieceTable; ASelectionRanges: TdxSelectionRangeCollection): Boolean;
    procedure SetDataObject(APieceTable: TdxPieceTable; ASelectionCollection: TdxSelectionRangeCollection;
      const ADataObject: IdxDataObject);
    function ShouldExtendRange(AInfo: TdxRunInfo; AField: TdxField): Boolean;
    function UpdateSelectionCollection(ASourcePieceTable: TdxPieceTable;
      ASelection: TdxSelectionRangeCollection): TdxSelectionRangeCollection;

    procedure DoAfterExport(Sender: TObject);
    procedure DoBeforeExport(Sender: TObject; E: TdxBeforeExportEventArgs);
    procedure DoCalculateDocumentVariable(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);

    procedure SubscribeTargetModelEvents(ATargetModel: TdxDocumentModel);
    procedure UnsubscribeTargetModelEvents(ATargetModel: TdxDocumentModel);

    property DocumentServer: IdxInnerControl read FDocumentServer;
  public
    constructor Create(const ADocumentServer: IdxInnerControl);

    procedure CopyDocumentRange(APieceTable: TdxPieceTable; ASelectionCollection: TdxSelectionRangeCollection);

    function GetRtfText(APieceTable: TdxPieceTable; ASelectionRanges: TdxSelectionRangeCollection;
      AOptions: TdxRtfDocumentExporterOptions = nil;
      AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): string;
    function GetPlainText(APieceTable: TdxPieceTable; ASelectionCollection: TdxSelectionRangeCollection;
      AOptions: TdxPlainTextDocumentExporterOptions = nil; AGetTextOptions: PdxTextFragmentOptions = nil): string;
    function GetOpenXmlBytes(APieceTable: TdxPieceTable; ASelectionCollection: TdxSelectionRangeCollection;
      AOptions: TdxOpenXmlDocumentExporterOptions = nil): TBytes;
    function GetSuppressStoreImageSizeCollection(APieceTable: TdxPieceTable;
      ASelectionCollection: TdxSelectionRangeCollection): string;

    property AllowCopyWholeFieldResult: Boolean read FAllowCopyWholeFieldResult write FAllowCopyWholeFieldResult;
    property DefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions read FDefaultPropertiesCopyOptions write FDefaultPropertiesCopyOptions;
    property FixLastParagraph: Boolean read FFixLastParagraph write FFixLastParagraph;
    property KeepFieldCodeViewState: Boolean read FKeepFieldCodeViewState write FKeepFieldCodeViewState;
  end;

implementation

uses
  Contnrs, RTLConsts, cxLibraryConsts, dxTypeHelpers, Math, dxCore, dxThreading,

  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.FieldController,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.DocumentLayout.NotPrintableExporters,
  dxRichEdit.InnerControl;

type
  TdxInnerRichEditControlAccess = class(TdxInnerRichEditControl);

{ TdxRichEditControlPadding }

constructor TdxRichEditControlPadding.Create(AOwner: TPersistent; const ADefaultValue: TRect);
begin
  inherited Create(AOwner);
  FDefaultValue := ADefaultValue;
  FValue := ADefaultValue;
end;

constructor TdxRichEditControlPadding.Create(AOwner: TPersistent; ADefaultValue: Integer = 0);
begin
  Create(AOwner, TRect.Create(ADefaultValue, ADefaultValue, ADefaultValue, ADefaultValue));
end;

function TdxRichEditControlPadding.IsDefault: Boolean;
begin
  Result := cxRectIsEqual(FDefaultValue, FValue);
end;

procedure TdxRichEditControlPadding.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxRichEditControlPadding then
    Value := TdxRichEditControlPadding(Source).Value;
end;

function TdxRichEditControlPadding.GetProperty(AIndex: Integer): Integer;
begin
  case AIndex of
    1: Result := FValue.Left;
    2: Result := FValue.Top;
    3: Result := FValue.Right;
    4: Result := FValue.Bottom;
  else
    if (FValue.Left = FValue.Top) and (FValue.Left = FValue.Right) and (FValue.Left = FValue.Bottom) then
      Result := FValue.Left
    else
      Result := -1;
  end;
end;

procedure TdxRichEditControlPadding.SetValue(const Value: TRect);
begin
  if not cxRectIsEqual(Value, FValue) then
  begin
    FValue := Value;
    DoChange;
  end;
end;

procedure TdxRichEditControlPadding.SetProperty(AIndex, AValue: Integer);
begin
  if GetProperty(AIndex) <> AValue then
  begin
    case AIndex of
      1: FValue.Left := AValue;
      2: FValue.Top := AValue;
      3: FValue.Right := AValue;
      4: FValue.Bottom := AValue;
    else
      if AValue = -1 then
        FValue := FDefaultValue
      else
        FValue := cxRect(AValue, AValue, AValue, AValue);
    end;
    DoChange;
  end;
end;

function TdxRichEditControlPadding.IsEqual(const APadding: TdxRichEditControlPadding): Boolean;
begin
  Result := cxRectIsEqual(Value, APadding.Value);
end;

procedure TdxRichEditControlPadding.Reset;
begin
  Value := FDefaultValue;
end;

function TdxRichEditControlPadding.IsPropertyStored(AIndex: Integer): Boolean;
begin
  case AIndex of
    1: Result := FValue.Left <> FDefaultValue.Left;
    2: Result := FValue.Top <> FDefaultValue.Top;
    3: Result := FValue.Right <> FDefaultValue.Right;
    4: Result := FValue.Bottom <> FDefaultValue.Bottom;
  else
    Result := (All <> -1) and not IsDefault;
  end;
end;

{ TdxScrollEventArgs }

constructor TdxScrollEventArgs.Create(AType: TdxScrollEventType; ANewValue: Integer;
  AScrollOrientation: TdxScrollOrientation);
begin
  inherited Create;
  FType := AType;
  FNewValue := ANewValue;
  FScrollOrientation := AScrollOrientation;
end;

constructor TdxScrollEventArgs.Create(AType: TdxScrollEventType; AOldValue, ANewValue: Integer;
  AScrollOrientation: TdxScrollOrientation);
begin
  inherited Create;
  FType := AType;
  FOldValue := AOldValue;
  FNewValue := ANewValue;
  FScrollOrientation := AScrollOrientation;
end;

{ TdxTableViewInfoController }

constructor TdxTableViewInfoController.Create(const AMousePosition: TPoint);
begin
  inherited Create;
  FMousePosition := AMousePosition;
end;

procedure TdxTableViewInfoController.SetMousePosition(const AValue: TPoint);
begin
  if FMousePosition.IsEqual(AValue) then
    Exit;

  FMousePosition := AValue;
  OnMousePositionChanged;
end;

procedure TdxTableViewInfoController.OnMousePositionChanged;
begin
end;

{ TdxDefaultCommandUIState }

constructor TdxDefaultCommandUIState.Create;
begin
  inherited Create;
  FIsEnabled := True;
  FIsChecked := False;
  FIsVisible := True;
end;

function TdxDefaultCommandUIState.GetChecked: Boolean;
begin
  Result := FIsChecked;
end;

function TdxDefaultCommandUIState.GetEnabled: Boolean;
begin
  Result := FIsEnabled;
end;

function TdxDefaultCommandUIState.GetVisible: Boolean;
begin
  Result := FIsVisible;
end;

procedure TdxDefaultCommandUIState.SetChecked(const Value: Boolean);
begin
  FIsChecked := Value;
end;

procedure TdxDefaultCommandUIState.SetEnabled(const Value: Boolean);
begin
  FIsEnabled := Value;
end;

procedure TdxDefaultCommandUIState.SetVisible(const Value: Boolean);
begin
  FIsVisible := Value;
end;

{ TdxCommand }

procedure TdxCommand.Execute;
var
  AState: IdxCommandUIState;
begin
  AState := CreateDefaultCommandUIState;
  UpdateUIState(AState);
  if AState.Visible and AState.Enabled then
    ForceExecute(AState);
end;

function TdxCommand.GetCommandSourceType: TdxCommandSourceType;
begin
  Result := FCommandSourceType;
end;

function TdxCommand.GetObjectDescription: string;
begin
  Result := GetDescription;
end;

function TdxCommand.GetObjectMenuCaption: string;
begin
  Result := GetMenuCaption;
end;

class function TdxCommand.NeedUseObjectLocalization: Boolean;
begin
  Result := False;
end;

class function TdxCommand.GetDescription: string;
begin
  AbstractErrorProc();
end;

class function TdxCommand.GetMenuCaption: string;
begin
  AbstractErrorProc();
end;

function TdxCommand.ServiceProvider: IdxServiceProvider;
begin
  Result := nil;
end;

procedure TdxCommand.SetCommandSourceType(const Value: TdxCommandSourceType);
begin
  FCommandSourceType := Value;
end;

function TdxCommand.GetShowsModalDialog: Boolean;
begin
  Result := False;
end;

function TdxCommand.CanExecute: Boolean;
var
  AState: IdxCommandUIState;
begin
  AState := CreateDefaultCommandUIState;
  UpdateUIState(AState);
  Result := AState.Visible and AState.Enabled;
end;

function TdxCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultCommandUIState.Create;
end;

procedure TdxCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
  UpdateUIStateCore(AState);
  UpdateUIStateViaService(AState);
  if HideDisabled and not AState.Enabled then
    AState.Visible := False;
end;

procedure TdxCommand.UpdateUIStateViaService(const AState: IdxCommandUIState);
var
  AServiceProvider: IdxServiceProvider;
begin
  AServiceProvider := ServiceProvider;
  if AServiceProvider = nil then
    Exit;
Assert(False, 'not implemented');

end;

{ TdxControlCommand }

constructor TdxControlCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create;
  FRichEditControl := ARichEditControl;
end;

class function TdxControlCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.None;
end;

function TdxControlCommand.GetCommandExecutionListener: IdxCommandExecutionListenerService;
begin
  Result := nil;
end;

function TdxControlCommand.GetControl: TCustomControl;
begin
  Result := TCustomControl(RichEditControl.Control);
end;

class function TdxControlCommand.GetImageName: string;
begin
  Result := '';
end;

procedure TdxControlCommand.NotifyBeginCommandExecution(
  const AState: IdxCommandUIState);
var
  AListener: IdxCommandExecutionListenerService;
begin
  AListener := GetCommandExecutionListener;
  if AListener <> nil then
    AListener.BeginCommandExecution(Self, AState);
end;

procedure TdxControlCommand.NotifyEndCommandExecution(
  const AState: IdxCommandUIState);
var
  AListener: IdxCommandExecutionListenerService;
begin
  AListener := GetCommandExecutionListener;
  if AListener <> nil then
    AListener.EndCommandExecution(Self, AState);
end;

function TdxControlCommand.ServiceProvider: IdxServiceProvider;
begin
  if not Supports(Control, IdxServiceProvider, Result) then
    Result := nil;
end;

{ TdxRichEditCommand }

function TdxRichEditCommand.CanEditSelection: Boolean;
begin
  Result := ActivePieceTable.CanEditSelection;
end;

function TdxRichEditCommand.CanEditTable(ATable: TdxTable): Boolean;
var
  APieceTable: TdxCustomPieceTable;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  AStartParagraph, AEndParagraph: TdxParagraphBase;
begin
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit(True);
  APieceTable := ATable.PieceTable;
  AStartParagraphIndex := ATable.FirstRow.FirstCell.StartParagraphIndex;
  AEndParagraphIndex := ATable.LastRow.LastCell.EndParagraphIndex;
  AStartParagraph := APieceTable.Paragraphs[AStartParagraphIndex];
  AEndParagraph := APieceTable.Paragraphs[AEndParagraphIndex];
  Result := ATable.PieceTable.CanEditRange(AStartParagraph.LogPosition, AEndParagraph.EndLogPosition);
end;

function TdxRichEditCommand.IsContentEditable: Boolean;
begin
  Result := InnerControl.IsEditable;
end;

procedure TdxRichEditCommand.CheckExecutedAtUIThread;
begin
  ActiveView.CheckExecutedAtUIThread;
end;

function TdxRichEditCommand.ExtendSelectionToParagraphBoundary: TdxSelectionItemList;
var
  AItems: TdxSelectionItems;
  I: Integer;
begin
  AItems := DocumentModel.Selection.Items;
  Result := TdxSelectionItemList.Create;
  Result.Capacity := AItems.Count;
  for I := 0 to AItems.Count - 1 do
    Result.Add(ExtendToParagraphBoundary(AItems[I]));
end;

function TdxRichEditCommand.ExtendSelectionToSectionBoundary: TdxSelectionItem;
var
  ASelection: TdxSelection;
  AFirstSectionIndex, ALastSectionIndex: TdxSectionIndex;
  AFirstSection, ALastSection: TdxSection;
  AMainPieceTable: TdxPieceTable;
  ALastParagraph: TdxParagraph;
begin
  ASelection := DocumentModel.Selection;
  AFirstSectionIndex := DocumentModel.FindSectionIndex(ASelection.NormalizedStart);
  if AFirstSectionIndex < 0 then
    Exit(ASelection.Items[0]);
  ALastSectionIndex := DocumentModel.FindSectionIndex(ASelection.NormalizedEnd);
  if ALastSectionIndex < 0 then
    Exit(ASelection.Items[0]);
  AFirstSection := DocumentModel.Sections[AFirstSectionIndex];
  ALastSection := DocumentModel.Sections[ALastSectionIndex];
  AMainPieceTable := DocumentModel.MainPieceTable;
  Result := TdxSelectionItem.Create(AMainPieceTable);
  Result.Start := AMainPieceTable.Paragraphs[AFirstSection.FirstParagraphIndex].LogPosition;
  ALastParagraph := AMainPieceTable.Paragraphs[ALastSection.LastParagraphIndex];
  Result.Start := ALastParagraph.LogPosition + ALastParagraph.Length;
end;

function TdxRichEditCommand.ExtendToParagraphBoundary(
  ASelectionItem: TdxSelectionItem): TdxSelectionItem;
var
  APieceTable: TdxCustomPieceTable;
  ALastParagraph: TdxParagraphBase;
begin
  APieceTable := ASelectionItem.PieceTable;
  Result := TdxSelectionItem.Create(APieceTable);
  Result.Start := APieceTable.Paragraphs[ASelectionItem.Interval.NormalizedStart.ParagraphIndex].LogPosition;
  ALastParagraph := APieceTable.Paragraphs[ASelectionItem.Interval.NormalizedEnd.ParagraphIndex];
  if ALastParagraph.Index >= APieceTable.Paragraphs.Count - 1 then
    Result.&End := ALastParagraph.LogPosition + ALastParagraph.Length - 1
  else
    Result.&End := ALastParagraph.LogPosition + ALastParagraph.Length;
end;

procedure TdxRichEditCommand.ApplyCommandsRestriction(const AState: IdxCommandUIState;
  AOption: TdxDocumentCapability; AdditionEnabledCondition: Boolean  = True);
begin
  AState.Enabled := AdditionEnabledCondition and (AOption <> TdxDocumentCapability.Disabled) and (AOption <> TdxDocumentCapability.Hidden);
  AState.Visible := (AOption <> TdxDocumentCapability.Hidden);
end;

procedure TdxRichEditCommand.ApplyCommandRestrictionOnEditableControl(const AState: IdxCommandUIState;
  AOption: TdxDocumentCapability);
begin
  AState.Enabled := IsContentEditable and (AOption <> TdxDocumentCapability.Disabled) and
    (AOption <> TdxDocumentCapability.Hidden);
  AState.Visible := AOption <> TdxDocumentCapability.Hidden;
end;

procedure TdxRichEditCommand.ApplyCommandRestrictionOnEditableControl(const AState: IdxCommandUIState;
  AOption: TdxDocumentCapability; AdditionEnabledCondition: Boolean);
begin
  ApplyCommandRestrictionOnEditableControl(AState, AOption);
  AState.Enabled := AState.Enabled and AdditionEnabledCondition;
end;

procedure TdxRichEditCommand.ApplyCommandRestrictionOnReadOnlyControl(const AState: IdxCommandUIState);
begin
  if RichEditControl.ReadOnly then
    AState.Enabled := False;
end;

procedure TdxRichEditCommand.ApplyDocumentProtectionToSelectedCharacters(const AState: IdxCommandUIState);
begin
  AState.Enabled := AState.Enabled and CanEditSelection;
end;

procedure TdxRichEditCommand.ApplyDocumentProtectionToSelectedParagraphs(const AState: IdxCommandUIState);
var
  AItems: TdxSelectionItemList;
begin
  if AState.Enabled then
  begin
    if DocumentModel.IsDocumentProtectionEnabled then
    begin
      AItems := ExtendSelectionToParagraphBoundary;
      try
        AState.Enabled := ActivePieceTable.CanEditSelectionItems(AItems);
      finally
        AItems.Free;
      end;
    end;
  end;
end;

procedure TdxRichEditCommand.ApplyDocumentProtectionToSelectedSections(const AState: IdxCommandUIState);
var
  AItem: TdxSelectionItem;
begin
  if AState.Enabled then
  begin
    if DocumentModel.IsDocumentProtectionEnabled then
    begin
      AItem := ExtendSelectionToSectionBoundary;
      try
        AState.Enabled := DocumentModel.MainPieceTable.CanEditRange(AItem.NormalizedStart, AItem.NormalizedEnd);
      finally
        AItem.Free;
      end;
    end;
  end;
end;

procedure TdxRichEditCommand.ApplyDocumentProtectionToTable(const AState: IdxCommandUIState; ATable: TdxTable);
begin
  if AState.Enabled then
    AState.Enabled := CanEditTable(ATable);
end;

function TdxRichEditCommand.GetActivePieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

function TdxRichEditCommand.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

function TdxRichEditCommand.GetActiveViewType: TdxRichEditViewType;
begin
  Result := InnerControl.ActiveViewType;
end;

function TdxRichEditCommand.GetDocumentModel: TdxDocumentModel;
begin
  Result := InnerControl.DocumentModel;
end;

function TdxRichEditCommand.GetInnerControl: IdxInnerControl;
begin
  Result := RichEditControl.InnerControl;
end;

function TdxRichEditCommand.GetOptions: TdxRichEditControlOptionsBase;
begin
  Result := InnerControl.Options;
end;

{ TdxCaretPosition }

constructor TdxCaretPosition.Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
begin
  inherited Create;
  Assert(AView <> nil);
  Assert(APreferredPageIndex >= 0);
  FView := AView;
  FPosition := CreateCaretDocumentLayoutPosition;
  CaretBoundsPosition := FPosition;
  PreferredPageIndex := APreferredPageIndex;
end;

destructor TdxCaretPosition.Destroy;
begin
  PageViewInfo := nil;
  CaretBoundsPosition := nil;
  FreeAndNil(FPosition);
  FreeAndNil(FInputPosition);
  inherited Destroy;
end;

function TdxCaretPosition.CalculateCaretBounds: TRect;
var
  AWidth: Integer;
begin
  AWidth := View.DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(1, DocumentModel.DpiX);
  if UsePreviousBoxBounds then
  begin
    Result := FPosition.Character.TightBounds;
    Result.Intersect(FPosition.Row.Bounds);
    Result.Offset(Result.Width, 0);
    Result.Width := AWidth;
  end
  else
  begin
    if (FCaretBoundsPosition = FPosition) or (FCaretBoundsPosition.Row <> FPosition.Row) then
    begin
      Result := FPosition.Character.TightBounds;
      Result.Intersect(FPosition.Row.Bounds);
      Result.Width := AWidth;
    end
    else
    begin
      Result := FCaretBoundsPosition.Character.TightBounds;
      Result.Intersect(FCaretBoundsPosition.Row.Bounds);
      Result.X := FPosition.Character.TightBounds.Left;
      Result.Width := AWidth;
    end;
  end;
end;

function TdxCaretPosition.CalculateCurrentFormatingPosition(
  ALogPosition: TdxDocumentLogPosition): TdxDocumentModelPosition;
var
  AFormattingLogPosition: TdxDocumentLogPosition;
  APieceTable: TdxPieceTable;
  ARunInfo: TdxRunInfo;
  ARun: TdxTextRunBase;
begin
  AFormattingLogPosition := Max(ALogPosition - 1, 0);
  APieceTable := DocumentModel.Selection.PieceTable;
  ARunInfo := TdxRunInfo.Create(APieceTable);
  try
    APieceTable.CalculateRunInfoStart(AFormattingLogPosition, ARunInfo);
  finally
    ARunInfo.Free;
  end;
  ARunInfo := APieceTable.FindRunInfo(AFormattingLogPosition, 1);
  try
    ARun := APieceTable.Runs[ARunInfo.Start.RunIndex];
    if not (ARun is TdxTextRun) then
      APieceTable.CalculateRunInfoStart(LogPosition, ARunInfo);
    Result := ARunInfo.Start;
  finally
    ARunInfo.Free;
  end;
end;

function TdxCaretPosition.ShouldUpdateModelPosition(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  ARunIndex: TdxRunIndex;
  AParagraph: TdxParagraph;
begin
  AParagraphs := TdxPieceTable(FModelPosition.PieceTable).Paragraphs;
  if FModelPosition.ParagraphIndex >= AParagraphs.Count then
    Exit(True);
  ARunIndex := FModelPosition.RunIndex;
  if ARunIndex >= FModelPosition.PieceTable.Runs.Count then
    Exit(True);
  if (ALogPosition < FModelPosition.RunStartLogPosition) or (ALogPosition > FModelPosition.RunEndLogPosition) then
    Exit(True);
  AParagraph := AParagraphs[FModelPosition.ParagraphIndex];
  if (ARunIndex > AParagraph.LastRunIndex) or (ARunIndex < AParagraph.FirstRunIndex) then
    Exit(True);
  Result := False;
end;

function TdxCaretPosition.CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := TdxCaretDocumentLayoutPosition.Create(FView);
end;

function TdxCaretPosition.CreateDragCaretPosition: TdxDragCaretPosition;
begin
  Result := TdxDragCaretPosition.Create(View, PreferredPageIndex);
end;

function TdxCaretPosition.CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition;
begin
  Result := TdxExplicitCaretPosition.Create(View, ALogPosition, PreferredPageIndex);
end;

function TdxCaretPosition.CreateInputPosition(ALogPosition: TdxDocumentLogPosition): TdxInputPosition;
var
  ACurrentFormattingPosition: TdxDocumentModelPosition;
  ARunIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
  AFormattingRunIndex: TdxRunIndex;
  AField: TdxField;
begin
  ACurrentFormattingPosition := CalculateCurrentFormatingPosition(ALogPosition);
  ARunIndex := ACurrentFormattingPosition.RunIndex;
  ARun := DocumentModel.ActivePieceTable.Runs[ARunIndex];
  Result := TdxInputPosition.Create(DocumentModel.ActivePieceTable);
  Result.LogPosition := ALogPosition;
  Result.ParagraphIndex := ARun.Paragraph.Index;
  AFormattingRunIndex := ARunIndex;
  if ARun is TdxFieldResultEndRun then
  begin
    AField := DocumentModel.ActivePieceTable.FindFieldByRunIndex(AFormattingRunIndex);
    ARun := DocumentModel.ActivePieceTable.Runs[AField.FirstRunIndex]
  end
  else
    if not DocumentModel.ActivePieceTable.VisibleTextFilter.IsRunVisible(AFormattingRunIndex)then
    begin
      AFormattingRunIndex := DocumentModel.ActivePieceTable.VisibleTextFilter.GetPrevVisibleRunIndex(ARunIndex);
      ARun := DocumentModel.ActivePieceTable.Runs[AFormattingRunIndex];
      if not (ARun is TdxTextRun) then
      begin
        AFormattingRunIndex := DocumentModel.ActivePieceTable.VisibleTextFilter.GetNextVisibleRunIndex(ARunIndex);
        ARun := DocumentModel.ActivePieceTable.Runs[AFormattingRunIndex];
      end;
    end;
  Result.CharacterStyleIndex := ARun.CharacterStyleIndex;
  Result.CharacterFormatting.CopyFrom(ARun.CharacterProperties.Info);
  Result.MergedCharacterFormatting.CopyFrom(ARun.MergedCharacterFormatting);
end;

function TdxCaretPosition.GetRowBoundsRelativeToPage: TRect;
begin
  Result := LayoutPosition.Row.Bounds;
end;

function TdxCaretPosition.CreateInputPosition: TdxInputPosition;
begin
  Result := CreateInputPosition(LogPosition);
end;

function TdxCaretPosition.GetActualLogPosition(AEndPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ASelection: TdxSelection;
  AEnd, AVirtualEndLogPositionCompensation: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  if ASelection.Start <= ASelection.&End then
    AEnd := Max(0, AEndPosition - ASelection.ActiveSelection.RightOffset)
  else
  begin
    AVirtualEndLogPositionCompensation := 0;
    if UsePreviousBoxBounds then
      Inc(AVirtualEndLogPositionCompensation);
    AEnd := AEndPosition + ASelection.ActiveSelection.LeftOffset + AVirtualEndLogPositionCompensation;
  end;
  Result := Min(AEnd, ASelection.PieceTable.DocumentEndLogPosition);
end;

function TdxCaretPosition.GetCaretBoundsDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
var
  ACurrentFormattingPosition: TdxDocumentModelPosition;
begin
  ACurrentFormattingPosition := CalculateCurrentFormatingPosition(ALogPosition);
  if (FCaretBoundsPosition <> nil) and (FCaretBoundsPosition.LogPosition = ACurrentFormattingPosition.LogPosition) then
    Exit(FCaretBoundsPosition);

  if ACurrentFormattingPosition.LogPosition = FPosition.LogPosition then
    Exit(FPosition);

  Result := CreateCaretDocumentLayoutPosition;
  Result.SetLogPosition(ACurrentFormattingPosition.LogPosition);
end;

function TdxCaretPosition.GetDocumentModel: TdxDocumentModel;
begin
  Result := View.DocumentModel;
end;

function TdxCaretPosition.GetInputPosition: TdxInputPosition;
begin
  if FInputPosition = nil then
    FInputPosition := CreateInputPosition;
  Result := FInputPosition;
end;

function TdxCaretPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := GetActualLogPosition(DocumentModel.Selection.&End);
end;

function TdxCaretPosition.GetPreferredPageIndex: Integer;
begin
  Result := FPreferredPageIndex;
end;

function TdxCaretPosition.GetUsePreviousBoxBounds: Boolean;
begin
  Result := DocumentModel.Selection.UsePreviousBoxBounds;
end;

function TdxCaretPosition.GetVirtualLogPosition: TdxDocumentLogPosition;
begin
  Result := GetActualLogPosition(DocumentModel.Selection.VirtualEnd);
end;

procedure TdxCaretPosition.Invalidate;
begin
  FPosition.Invalidate;
  CaretBoundsPosition := FPosition;
  InvalidatePageViewInfo;
end;

procedure TdxCaretPosition.InvalidateInputPosition;
begin
  FreeAndNil(FInputPosition);
end;

procedure TdxCaretPosition.InvalidatePageViewInfo;
begin
  PageViewInfo := nil;
end;

procedure TdxCaretPosition.SetCaretBoundsPosition(const Value: TdxDocumentLayoutPosition);
begin
  if FCaretBoundsPosition <> Value then
  begin
    if FCaretBoundsPosition <> FPosition then
      FreeAndNil(FCaretBoundsPosition);
    FCaretBoundsPosition := Value;
  end;
end;

procedure TdxCaretPosition.SetPageViewInfo(const Value: TdxPageViewInfo);
begin
  if FPageViewInfo <> Value then
  begin
    TdxPageViewInfo.Release(FPageViewInfo);
    FPageViewInfo := Value;
    TdxPageViewInfo.AddReference(FPageViewInfo);
  end;
end;

procedure TdxCaretPosition.SetPreferredPageIndex(const Value: Integer);
begin
  Assert(Value >= 0);
  FPreferredPageIndex := Value;

  if LayoutPosition is TdxHeaderFooterDocumentLayoutPosition then
    TdxHeaderFooterDocumentLayoutPosition(LayoutPosition).PreferredPageIndex := FPreferredPageIndex;
  if CaretBoundsPosition is TdxHeaderFooterDocumentLayoutPosition then
    TdxHeaderFooterDocumentLayoutPosition(CaretBoundsPosition).PreferredPageIndex := FPreferredPageIndex;
  if LayoutPosition is TdxTextBoxDocumentLayoutPosition then
    TdxTextBoxDocumentLayoutPosition(LayoutPosition).PreferredPageIndex := FPreferredPageIndex;
  if CaretBoundsPosition is TdxTextBoxDocumentLayoutPosition then
    TdxTextBoxDocumentLayoutPosition(CaretBoundsPosition).PreferredPageIndex := FPreferredPageIndex;
end;

function TdxCaretPosition.TryGetInputPosition: TdxInputPosition;
begin
  Result := FInputPosition;
end;

function TdxCaretPosition.Update(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  View.CheckExecutedAtUIThread;
  FPosition.SetLogPosition(VirtualLogPosition);
  UpdateModelPosition(Min(FPosition.LogPosition, FPosition.PieceTable.DocumentEndLogPosition));

  if not FPosition.Update(View.FormattingController.PageController.Pages, ADetailsLevel) then
  begin
    if FPosition.DetailsLevel > TdxDocumentLayoutDetailsLevel.None then
      PageViewInfo := View.LookupPageViewInfoByPage(FPosition.Page);

    CaretBoundsPosition := FPosition;
    Exit(False);
  end;

  CaretBoundsPosition := UpdateCaretDocumentLayoutPosition(FPosition.DetailsLevel);
  if PageViewInfo <> nil then
    Exit(True);

  if FPosition.DetailsLevel > TdxDocumentLayoutDetailsLevel.None then
    PageViewInfo := View.LookupPageViewInfoByPage(FPosition.Page);
  Result := PageViewInfo <> nil;
end;

function TdxCaretPosition.UpdateCaretDocumentLayoutPosition(
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutPosition;
var
  APos: TdxDocumentModelPosition;
  ATextFilter: IdxVisibleTextFilter;
  ALogPosition: TdxDocumentLogPosition;
begin
  if UsePreviousBoxBounds then
    Exit(FPosition);

  APos := FModelPosition;
  if APos.RunOffset <> 0 then
    Exit(FPosition);

  if APos.LogPosition = APos.PieceTable.Paragraphs[APos.ParagraphIndex].LogPosition then
    Exit(FPosition);
  ATextFilter := TdxPieceTable(APos.PieceTable).VisibleTextFilter;
  ALogPosition := ATextFilter.GetPrevVisibleLogPosition(VirtualLogPosition, False);
  if ALogPosition < 0 then
    Exit(FPosition);
  Result := GetCaretBoundsDocumentLayoutPosition(ALogPosition);
  Result.Update(View.FormattingController.PageController.Pages, ADetailsLevel);
  Assert(Result.IsValid(ADetailsLevel));
end;

procedure TdxCaretPosition.UpdateModelPosition(ALogPosition: TdxDocumentLogPosition);
begin
  if not FModelPosition.IsValid or (FModelPosition.PieceTable <> DocumentModel.ActivePieceTable) then
    FModelPosition := TdxDocumentModelPosition.Create(DocumentModel.ActivePieceTable);
  FModelPosition.LogPosition := ALogPosition;
  if ShouldUpdateModelPosition(ALogPosition) then
    FModelPosition.Update;
end;

function TdxCaretPosition.UpdatePositionTrySetUsePreviousBoxBounds(
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
var
  ASelection: TdxSelection;
begin
  Result := False;
  ASelection := DocumentModel.Selection;
  if (not UsePreviousBoxBounds) and (ASelection.Length > 0) and (ASelection.&End > ASelection.Start) then
    if FPosition.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
      if FPosition.Row.GetFirstPosition(FPosition.PieceTable).LogPosition = FPosition.LogPosition then
      begin
        ASelection.UsePreviousBoxBounds := True;
        FPosition.Invalidate;
        Exit(Update(ADetailsLevel));
      end;
end;

{ TdxDocumentSelectionLayout }

function TdxDocumentSelectionLayout.GetType: TClass;
begin
  Result := ClassType;
end;

procedure TdxDocumentSelectionLayout.AddNestedItem(APageIndex: Integer; const AItem: IdxSelectionLayoutItem);
begin
  if not FNestedItems.ContainsKey(APageIndex) then
    FNestedItems.Add(APageIndex, TdxSelectionLayoutItemList.Create);
  FNestedItems[APageIndex].Add(AItem);
end;

procedure TdxDocumentSelectionLayout.AddToPageSelection(APage: TdxPage; ATarget: TdxPageSelectionLayoutsCollection);
var
  ASet: TdxSelectionLayoutItemList;
begin
  if APage = nil then
    Exit;
  if not NestedItems.TryGetValue(APage.PageIndex, ASet) then
    Exit;
  ATarget.AddRange(ASet);
end;

constructor TdxDocumentSelectionLayout.Create(ASelectionLayout: TdxSelectionLayout; AStart, AEnd: TdxDocumentLayoutPosition;
  APieceTable: TdxPieceTable);
begin
  inherited Create;
  FUpdatedSuccessfully := False;
  FHotZones := TdxHotZoneCollection.Create;
  FNestedItems := TObjectDictionary<Integer, TdxSelectionLayoutItemList>.Create([doOwnsValues]);
  FDocumentLayout := AStart.DocumentLayout;
  FSelectionLayout := ASelectionLayout;
  FPieceTable := APieceTable;
  FStart := AStart;
  FEnd := AEnd;
end;

destructor TdxDocumentSelectionLayout.Destroy;
begin
  FreeAndNil(FEnd);
  FreeAndNil(FStart);
  FreeAndNil(FNestedItems);
  FreeAndNil(FHotZones);
  inherited Destroy;
end;

function TdxDocumentSelectionLayout.CreateDocumentLayoutPosition(
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
begin
  Result := SelectionLayout.CreateDocumentLayoutPosition(ALogPosition);
end;

procedure TdxDocumentSelectionLayout.Draw(const ASelectionPainter: IdxSelectionPainter);
begin
  NotImplemented;
end;


function TdxDocumentSelectionLayout.FindCellViewInfo(AModelCell: TdxTableCell; AColumn: TdxColumn): TdxTableCellViewInfo;
var
  ACell: TdxTableCellViewInfo;
  ATable: TdxTableViewInfo;
  I, J: Integer;
begin
  if (AColumn = nil) or (AModelCell = nil) then
    Exit(nil);
  for I := 0 to AColumn.Tables.Count - 1 do
  begin
    ATable := AColumn.Tables[I];
    if ATable.Table = AModelCell.Table then
      for J := 0 to ATable.Cells.Count - 1 do
      begin
        ACell := ATable.Cells[J];
        if ACell.Cell = AModelCell then
          Exit(ACell);
      end;
  end;
  Result := nil;
end;

function TdxDocumentSelectionLayout.FindFloatingObject(APage: TdxPage; AIndex: TdxRunIndex): TdxFloatingObjectBox;
var
  AResultBox: TdxFloatingObjectBox;
begin
  AResultBox := FindFloatingObject(APage.InnerBackgroundFloatingObjects, AIndex);
  if AResultBox <> nil then
    Exit(AResultBox);
  AResultBox := FindFloatingObject(APage.InnerFloatingObjects, AIndex);
  if AResultBox <> nil then
    Exit(AResultBox);
  Result := FindFloatingObject(APage.InnerForegroundFloatingObjects, AIndex);
end;

function TdxDocumentSelectionLayout.FindFloatingObject(AFloatingObjects: TdxFloatingObjectBoxList;
  AIndex: TdxRunIndex): TdxFloatingObjectBox;
var
  APieceTable: TdxCustomPieceTable;
  ABox: TdxFloatingObjectBox;
  I: Integer;
begin
  if AFloatingObjects = nil then
    Exit(nil);
  Result := nil;
  APieceTable := DocumentLayout.DocumentModel.GetActivePieceTableCore;
  for I := 0 to AFloatingObjects.Count - 1 do
  begin
    ABox := AFloatingObjects[I];
    if (ABox.StartPos.RunIndex = AIndex) and (APieceTable = ABox.PieceTable) then
    begin
      Result := ABox;
      Break;
    end;
  end;
end;



procedure TdxDocumentSelectionLayout.ForceUpdateForEmptySelection;
begin
  NestedItems.Clear;
  FUpdatedSuccessfully := True;
end;

function TdxDocumentSelectionLayout.GetBox(APos: TdxDocumentLayoutPosition): TdxPage;
begin
  APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page);
  Result := APos.Page;
end;

function TdxDocumentSelectionLayout.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxDocumentSelectionLayout.GetView: TdxRichEditView;
begin
  Result := FSelectionLayout.View;
end;

function TdxDocumentSelectionLayout.HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean;
begin
  Result := (ALogPosition >= Start.LogPosition) and (ALogPosition < &End.LogPosition);
end;

function TdxDocumentSelectionLayout.TryToCreateRectangularObjectSelectionLayoutForSinglePosition(AStart, AEnd: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
var
  ASelection: TdxSelection;
  APos: TdxDocumentLayoutPosition;
  ABox: TdxFloatingObjectBox;
begin
  ASelection := TdxDocumentModel(DocumentLayout.DocumentModel).Selection;
  if (AStart.LogPosition > ASelection.NormalizedStart) or (ASelection.NormalizedStart > AEnd.LogPosition) then
    Exit(nil);
  APos := CreateDocumentLayoutPosition(ASelection.NormalizedStart);
  try
    APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Box);
    if APos.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    begin
      ABox := FindFloatingObject(APos.Page, ASelection.Interval.NormalizedStart.RunIndex);
      if (ABox <> nil) and (ABox.PieceTable = DocumentLayout.DocumentModel.GetActivePieceTableCore) then
      begin
        Result := TdxResizeableRectangularObjectSelectionLayout.Create(View, ABox, ASelection.NormalizedStart, PieceTable);
        Result.HitTestTransform := ABox.CreateBackwardTransformUnsafe;
        Exit;
      end;
    end;
    Result := TryToCreateRectangularObjectSelectionLayoutByBox(APos, ASelection.NormalizedStart);
  finally
    APos.Free;
  end;
end;

function TdxDocumentSelectionLayout.TryToCreateRectangularObjectSelectionLayoutByBox(ALayoutPos: TdxDocumentLayoutPosition;
  ALogPos: TdxDocumentLogPosition): TdxRectangularObjectSelectionLayout;
var
  AInlinePicture: TdxInlinePictureBox;
begin
  Result := nil;
  if ALayoutPos.IsValid(TdxDocumentLayoutDetailsLevel.Box) then
  begin
    if ALayoutPos.Box is TdxInlinePictureBox then
    begin
      AInlinePicture := TdxInlinePictureBox(ALayoutPos.Box);
      Result := TdxResizeableRectangularObjectSelectionLayout.Create(View, AInlinePicture, ALogPos, PieceTable);
    end
  end;
end;

function TdxDocumentSelectionLayout.Update: Boolean;
begin
  HotZones.Clear;
  NestedItems.Clear;
  FUpdatedSuccessfully := UpdateCore and UpdateNestedItems;
  Result := UpdatedSuccessfully;
end;

function TdxDocumentSelectionLayout.UpdateCore: Boolean;
var
  APos, APrevPos: TdxDocumentLayoutPosition;
begin
  Result := True;
  if TryCreatePictureSelection then
    Exit;
  TryAddFloatingObjectsToSelection;

  APos := Start;
  try
    while APos.LogPosition < &End.LogPosition do
    begin
      APrevPos := APos;
      try
        if not TryGetNextPosition(APos) then
          Exit(False);
      finally
        if (APrevPos <> APos) and (APrevPos <> Start) then
          APrevPos.Free;
      end;
    end;
  finally
    if APos <> Start then
      APos.Free;
  end;
end;

function TdxDocumentSelectionLayout.UpdateNestedItems: Boolean;
var
  AList: TdxSelectionLayoutItemList;
  AItem: IdxSelectionLayoutItem;
  I: Integer;
begin
  Result := True;
  for AList in NestedItems.Values do
    for I := 0 to AList.Count - 1 do
    begin
      AItem := AList[I];
      if not AItem.Update then
      begin
        Result := False;
        Break;
      end;
    end;
end;

procedure TdxDocumentSelectionLayout.TryAddFloatingObjectsToSelection;
var
  ASelectionRunsInfo: TdxRunInfo;
  ARunInfo: TdxDocumentModelPosition;
  AAnchorPos: TdxDocumentLayoutPosition;
  AFloatSelection: TdxRectangularObjectSelectionLayout;
begin
  ASelectionRunsInfo := PieceTable.FindRunInfo(FStart.LogPosition, FEnd.LogPosition - FStart.LogPosition);
  try
    ARunInfo := ASelectionRunsInfo.Start;
  finally
    ASelectionRunsInfo.Free;
  end;
  while ARunInfo.LogPosition < Min(FEnd.LogPosition, PieceTable.DocumentEndLogPosition + 1) do
  begin
    AFloatSelection := TryCreateRectangularObjectSelectionLayoutByAnchorRun(ARunInfo, AAnchorPos);
    try
      if AFloatSelection <> nil then
        AddNestedItem(AAnchorPos.Page.PageIndex, AFloatSelection);
      ASelectionRunsInfo := PieceTable.FindRunInfo(ARunInfo.RunEndLogPosition + 1, 0);
      try
        ARunInfo := ASelectionRunsInfo.Start;
      finally
        ASelectionRunsInfo.Free;
      end;
    finally
      AAnchorPos.Free;
    end;
  end;
end;

function TdxDocumentSelectionLayout.TryCreatePictureSelection: Boolean;
var
  APictureSelection: TdxRectangularObjectSelectionLayout;
begin
  APictureSelection := TryCreateRectangularObjectSelectionLayout(FStart, FEnd);
  if APictureSelection = nil then
    Exit(False);
  FStart.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page);
  AddNestedItem(FStart.Page.PageIndex, APictureSelection);
  Result := True;
end;

function TdxDocumentSelectionLayout.TryGetNextPosition(var APos: TdxDocumentLayoutPosition): Boolean;
var
  AParagraph: TdxParagraph;
  ACell: TdxTableCell;
  ARealBoxStart: TdxDocumentLogPosition;
begin
  if not APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) then
  begin
    APos := CreateDocumentLayoutPosition(APos.LogPosition + 1);
    Exit(True);
  end;
  AParagraph := PieceTable.FindParagraph(APos.LogPosition);
  ACell := PieceTable.TableCellsManager.GetCell(AParagraph);
  if (ACell <> nil) and (ACell.VerticalMerging = TdxMergingState.Continue) then
  begin
    APos := CreateDocumentLayoutPosition(APos.LogPosition + 1);
    Exit(True);
  end;
  ARealBoxStart := APos.Row.GetFirstPosition(PieceTable).LogPosition;
  if ARealBoxStart > APos.LogPosition then
  begin
    APos := CreateDocumentLayoutPosition(ARealBoxStart);
    Exit(True);
  end;
  Result := TryGetNextPositionCore(AParagraph, ACell, APos);
end;

function TdxDocumentSelectionLayout.TryGetNextPositionCore(AParagraph: TdxParagraph;
  ACell: TdxTableCell; var APos: TdxDocumentLayoutPosition): Boolean;
var
  AStop: TdxDocumentLayoutPosition;
  AVisibleEndLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  AParent: TdxTableCell;
  AField: TdxField;
begin
  AVisibleEndLogPosition := &End.LogPosition - 1;
  if (ACell <> nil) and
    ((Start.LogPosition > PieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition) or
    (&End.LogPosition <= PieceTable.Paragraphs[ACell.EndParagraphIndex].EndLogPosition)) then
  begin
    ACell := nil;
    if &End.LogPosition < PieceTable.DocumentEndLogPosition then
      AEndLogPosition := &End.LogPosition
    else
      AEndLogPosition := PieceTable.DocumentEndLogPosition;
    AVisibleEndLogPosition := PieceTable.VisibleTextFilter.GetPrevVisibleLogPosition(AEndLogPosition, True);
    AStop := CreateDocumentLayoutPosition(AVisibleEndLogPosition);
  end
  else
  begin
    if ACell <> nil then
    begin
      AParent := ACell.Table.ParentCell;
      while (AParent <> nil) and
        ((Start.LogPosition <= PieceTable.Paragraphs[AParent.StartParagraphIndex].LogPosition) or
        (&End.LogPosition > PieceTable.Paragraphs[AParent.EndParagraphIndex].EndLogPosition)) do
      begin
        ACell := AParent;
        AParent := ACell.Table.ParentCell;
      end;
      AParagraph := PieceTable.Paragraphs[ACell.EndParagraphIndex];
    end;
    if AParagraph.EndLogPosition > &End.LogPosition - 1 then
    begin
      AVisibleEndLogPosition := PieceTable.VisibleTextFilter.GetPrevVisibleLogPosition(&End.LogPosition, True);
      AStop := CreateDocumentLayoutPosition(AVisibleEndLogPosition);
    end
    else
    begin
      AField := PieceTable.FindFieldByRunIndex(AParagraph.LastRunIndex);
      if (AField <> nil) and AField.IsCodeView and AField.Result.Contains(AParagraph.LastRunIndex) and
        (&End.LogPosition <> PieceTable.DocumentEndLogPosition + 1) then
      begin
        AVisibleEndLogPosition := PieceTable.VisibleTextFilter.GetPrevVisibleLogPosition(&End.LogPosition, True);
        AStop := CreateDocumentLayoutPosition(AVisibleEndLogPosition);
      end
      else
        AStop := CreateDocumentLayoutPosition(AParagraph.EndLogPosition);
    end;
  end;
  try
    if not AStop.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) then
      Exit(False);
    Result := TryGetNextPositionBySelectionLayoutItem(AStop, AVisibleEndLogPosition, ACell, APos);
  finally
    AStop.Free;
  end;
end;

function TdxDocumentSelectionLayout.TryCreateRectangularObjectSelectionLayout(AStart,
  AEnd: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
var
  ASelection: TdxSelection;
  ASelectedParagraphs: TdxParagraphList;
  AController: TdxFieldController;
  AField: TdxField;
begin
  ASelection := TdxDocumentModel(DocumentLayout.DocumentModel).Selection;
  ASelectedParagraphs := ASelection.GetSelectedParagraphs;
  try
    if (ASelection.Length = 1) or ((ASelectedParagraphs.Count > 0) and
      (ASelectedParagraphs.First.FrameProperties <> nil) and not ASelectedParagraphs.First.IsInCell)
    then
      Exit(TryToCreateRectangularObjectSelectionLayoutForSinglePosition(AStart, AEnd));

  {$IFNDEF DELPHI102TOKYO}
    Result := nil;
  {$ENDIF}
    AController := TdxFieldController.Create;
    try
      AField := AController.FindFieldBySelection(ASelection);
      if AField = nil then
        Exit(nil);
      Result := TryToCreateRectangularObjectSelectionLayoutByField(AField);
    finally
      AController.Free;
    end;
  finally
    ASelectedParagraphs.Free;
  end;
end;

function TdxDocumentSelectionLayout.TryCreateRectangularObjectSelectionLayoutByAnchorRun(ARunInfo: TdxDocumentModelPosition;
  out APos: TdxDocumentLayoutPosition): TdxRectangularObjectSelectionLayout;
var
  ASelection: TdxSelection;
  ABox: TdxFloatingObjectBox;
begin
  APos := nil;
  ASelection := TdxDocumentModel(DocumentLayout.DocumentModel).Selection;
  if (FStart.LogPosition > ASelection.NormalizedStart) or
      (ASelection.NormalizedStart > FEnd.LogPosition) then
    Exit(nil);
  if not (PieceTable.Runs[ARunInfo.RunIndex] is TdxFloatingObjectAnchorRun) then
    Exit(nil);
  APos := CreateDocumentLayoutPosition(ARunInfo.RunStartLogPosition);
  APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Box);
  if not APos.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    Exit(nil);
  ABox := FindFloatingObject(APos.Page, ARunInfo.RunIndex);
  if (ABox <> nil) and (ABox.PieceTable = DocumentLayout.DocumentModel.GetActivePieceTableCore) then
  begin
    Result := TdxRectangularObjectSelectionLayout.Create(View, ABox, ASelection.NormalizedStart, FPieceTable);
    Result.HitTestTransform := ABox.CreateBackwardTransformUnsafe;
    Exit;
  end;
  Result := nil;
end;

function TdxDocumentSelectionLayout.TryToCreateRectangularObjectSelectionLayoutByField(AField: TdxField): TdxRectangularObjectSelectionLayout;
var
  ASelection: TdxSelection;
  ALogPos: TdxDocumentLogPosition;
  APos: TdxDocumentLayoutPosition;
begin
  if AField.Result.&End - AField.Result.Start <> 1 then
    Exit(nil);
  ASelection := TdxDocumentModel(DocumentLayout.DocumentModel).Selection;
  ALogPos := ASelection.PieceTable.GetRunLogPosition(AField.Result.Start);
  APos := CreateDocumentLayoutPosition(ALogPos);
  try
    APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Box);
    Result := TryToCreateRectangularObjectSelectionLayoutByBox(APos, ALogPos);
  finally
    APos.Free;
  end;
end;

function TdxDocumentSelectionLayout.TryGetNextPositionBySelectionLayoutItem(AStop: TdxDocumentLayoutPosition;
  AVisibleEndLogPosition: TdxDocumentLogPosition; ACell: TdxTableCell;
  var APos: TdxDocumentLayoutPosition): Boolean;
var
  AItem: IdxSelectionLayoutItem;
  AFracture: TdxDocumentLayoutPosition;
  AInfo: TdxTableCellViewInfo;
begin
  AItem := nil;
  AFracture := nil;
  try
    if ACell = nil then
    begin
      if APos.Row <> AStop.Row then
      begin
        AFracture := CreateDocumentLayoutPosition(APos.Row.GetLastPosition(PieceTable).LogPosition);
        if not AFracture.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) then
          Exit(False);
        if AFracture.LogPosition < AStop.LogPosition then
          AStop := AFracture;
      end;
    end
    else
    begin
      AInfo := FindCellViewInfo(ACell, APos.Column);
      if AInfo <> nil then
      begin
        if APos.Column <> AStop.Column then
        begin
          AFracture := CreateDocumentLayoutPosition(AInfo.GetLastRow(APos.Column).GetLastPosition(PieceTable).LogPosition);
          if not AFracture.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) then
            Exit(False);
          if AFracture.LogPosition < AStop.LogPosition then
            AStop := AFracture;
        end;
        if (ACell.Table.ParentCell <> nil) and
          (PieceTable.Paragraphs[ACell.Table.ParentCell.StartParagraphIndex].LogPosition >= Start.LogPosition) and
          (PieceTable.Paragraphs[ACell.Table.ParentCell.EndParagraphIndex].LogPosition < &End.LogPosition) then
        begin
          APos := CreateDocumentLayoutPosition(PieceTable.Paragraphs[ACell.Table.ParentCell.EndParagraphIndex].LogPosition + 1);
          Exit(True);
        end
        else
          AItem := TdxTableCellSelectionLayout.Create(AInfo, APos.Page);
      end;
    end;
    Result := TryGetNextPositionBySelectionLayoutItemCore(AItem, AStop, AVisibleEndLogPosition, APos);
  finally
    AFracture.Free;
  end;
end;

function TdxDocumentSelectionLayout.TryGetNextPositionBySelectionLayoutItemCore(const AItem: IdxSelectionLayoutItem;
  AStop: TdxDocumentLayoutPosition; AVisibleEndLogPosition: TdxDocumentLogPosition;
  var APos: TdxDocumentLayoutPosition): Boolean;
var
  ANextLogPosition: TdxDocumentLogPosition;
  ACheckItem: IdxSelectionLayoutItem;
begin
  ACheckItem := AItem;
  if ACheckItem = nil then
  begin
    if (FStart = APos) or (AStop.LogPosition >= AVisibleEndLogPosition) then
    begin
      if not APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Character, True) or
          not AStop.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Character, True) then
        Exit(False);
      ACheckItem := TdxRowSelectionLayout.Create(APos, AStop, APos.Page);
    end
    else
    begin
      if not APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) or
          not AStop.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row, True) then
        Exit(False);
      ACheckItem := TdxEntireRowSelectionLayout.Create(APos, AStop, APos.Page);
    end;
  end;

  AddNestedItem(APos.Page.PageIndex, ACheckItem);
  ANextLogPosition := PieceTable.VisibleTextFilter.GetNextVisibleLogPosition(AStop.LogPosition, True, True);
  APos := CreateDocumentLayoutPosition(ANextLogPosition);
  Result := True;
end;

{ TdxTextBoxDocumentSelectionLayout }

procedure TdxTextBoxDocumentSelectionLayout.AddToPageSelection(APage: TdxPage; AWhere: TdxPageSelectionLayoutsCollection);
var
  ABox: TdxFloatingObjectBox;
  APos: TdxDocumentLayoutPosition;
begin
  inherited AddToPageSelection(APage, AWhere);
  if FTextBoxFrameSelection <> nil then
  begin
    ABox := TdxFloatingObjectBox(FTextBoxFrameSelection.Box);
    APos := TdxDocumentLayoutPosition(DocumentLayout.CreateLayoutPosition(ABox.PieceTable, FTextBoxFrameSelection.LogStart, 0));
    if APos.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page) and (APage = APos.Page) then
      AWhere.Add(FTextBoxFrameSelection);
  end;
end;

procedure TdxTextBoxDocumentSelectionLayout.ForceUpdateForEmptySelection;
begin
  FTextBoxFrameSelection := nil;
  inherited ForceUpdateForEmptySelection;
  TryToAddTextBoxFrameSelectionLayout(Start, &End);
  UpdateNestedItems;
end;

function TdxTextBoxDocumentSelectionLayout.UpdateCore: Boolean;
begin
  FTextBoxFrameSelection := nil;
  TryToAddTextBoxFrameSelectionLayout(Start, &End);
  Result := inherited UpdateCore;
end;

procedure TdxTextBoxDocumentSelectionLayout.TryToAddTextBoxFrameSelectionLayout(AStart: TdxDocumentLayoutPosition; AEnd: TdxDocumentLayoutPosition);
var
  ATextBoxContentType: TdxTextBoxContentType;
  AAnchorRun: TdxFloatingObjectAnchorRun;
  ALogPosition: TdxDocumentLogPosition;
  APages: TdxPageCollection;
  AStartBoxIndex, AEndBoxIndex, I: Integer;
  ABox: TdxFloatingObjectBox;
  AResult: TdxResizeableRectangularObjectSelectionLayout;
begin
  ATextBoxContentType := Safe<TdxTextBoxContentType>.Cast(DocumentModel.ActivePieceTable.ContentType);
  if ATextBoxContentType = nil then
    Exit;

  AAnchorRun := ATextBoxContentType.AnchorRun;
  ALogPosition := AAnchorRun.PieceTable.GetRunLogPosition(AAnchorRun);
  APages := DocumentLayout.Pages;
  AStartBoxIndex := APages.IndexOf(GetBox(AStart));
  AEndBoxIndex := APages.IndexOf(GetBox(AEnd));
  if (AStartBoxIndex >= 0) and (AStartBoxIndex <= AEndBoxIndex) then
  begin
    for I := AStartBoxIndex to AEndBoxIndex do
    begin
      ABox := APages[I].FindFloatingObject(AAnchorRun);
      if ABox = nil then
        Continue;

      AResult := TdxResizeableRectangularObjectSelectionLayout.Create(View, ABox, ALogPosition, TdxPieceTable(ATextBoxContentType.PieceTable));
      AResult.HitTestTransform := ABox.CreateBackwardTransformUnsafe;
      AddNestedItem(I, AResult);
      Break;
    end;
  end;
end;

{ TdxSelectionLayout }

function TdxSelectionLayout.GetDocumentLayout: TdxDocumentLayout;
begin
  Result := View.DocumentLayout;
end;

function TdxSelectionLayout.GetEndLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := LastDocumentSelectionLayout.&End;
  Result.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Character);
end;

function TdxSelectionLayout.GetFirstDocumentSelectionLayout: TdxDocumentSelectionLayout;
begin
  if FDocumentSelectionLayouts = nil then
    Update;
  Result := FDocumentSelectionLayouts[0];
end;

function TdxSelectionLayout.GetLastDocumentSelectionLayout: TdxDocumentSelectionLayout;
begin
  if (FDocumentSelectionLayouts = nil) or (FDocumentSelectionLayouts.Count = 0) then
    Update;
  Result := FDocumentSelectionLayouts[FDocumentSelectionLayouts.Count - 1];
end;

function TdxSelectionLayout.GetPageSelection(APage: TdxPage): TdxPageSelectionLayoutsCollection;
var
  I: Integer;
begin
  if FDocumentSelectionLayouts = nil then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxPageSelectionLayoutsCollection.Create;
  for I := 0 to FDocumentSelectionLayouts.Count - 1 do
    FDocumentSelectionLayouts[I].AddToPageSelection(APage, Result);
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

function TdxSelectionLayout.GetPreferredPageIndex: Integer;
begin
  Result := FPreferredPageIndex;
end;

function TdxSelectionLayout.GetStartLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := FirstDocumentSelectionLayout.Start;
  Result.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Character);
end;

function TdxSelectionLayout.HitTest(ALogPosition: TdxDocumentLogPosition; ALogicalPoint: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  Update;
  for I := 0 to FDocumentSelectionLayouts.Count - 1 do
    if FDocumentSelectionLayouts[I].HitTest(ALogPosition, ALogicalPoint) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxSelectionLayout.Invalidate;
begin
  if FDocumentSelectionLayouts = nil then
    Exit;
  FreeAndNil(FDocumentSelectionLayouts);
end;

function TdxSelectionLayout.IsSelectionStartFromBeginRow: Boolean;
begin
  Result := (StartLayoutPosition.Row.Boxes[0].StartPos = StartLayoutPosition.Character.StartPos);
end;

procedure TdxSelectionLayout.SetPreferredPageIndex(const Value: Integer);
begin
  Assert(Value >= 0);
  FPreferredPageIndex := Value;
end;

procedure TdxSelectionLayout.Update;
var
  ASelection: TdxSelection;
  I: Integer;
  ASelectionItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentLayoutPosition;
  ADocumentSelectionLayout: TdxDocumentSelectionLayout;
begin
  if (FDocumentSelectionLayouts <> nil) and (FDocumentSelectionLayouts.Count > 0) and
    LastDocumentSelectionLayout.UpdatedSuccessfully then
    Exit;

  ASelection := TdxDocumentModel(DocumentLayout.DocumentModel).Selection;
  FDocumentSelectionLayouts.Free;
  FDocumentSelectionLayouts := TdxObjectList<TdxDocumentSelectionLayout>.Create;
  for I := 0 to ASelection.Items.Count - 1 do
  begin
    ASelectionItem := ASelection.Items[I];

    AStart := CreateDocumentLayoutPosition(ASelectionItem.NormalizedStart);
    AEnd := CreateDocumentLayoutPosition(ASelectionItem.NormalizedEnd);

    AStart.LeftOffset := ASelectionItem.LeftOffset;
    AEnd.RightOffset := ASelectionItem.RightOffset;

    ADocumentSelectionLayout := CreateDocumentSelectionLayout(AStart, AEnd);
    FDocumentSelectionLayouts.Add(ADocumentSelectionLayout);

    if (ASelection.Length <= 0) and (ASelection.Items.Count = 1) and (ASelectionItem.LeftOffset = 0) and
      (ASelectionItem.RightOffset = 0) then
      ADocumentSelectionLayout.ForceUpdateForEmptySelection
    else
      ADocumentSelectionLayout.Update;
  end;
end;

function TdxSelectionLayout.CalculateHotZone(AResult: TdxRichEditHitTestResult; AView: TdxRichEditView): TdxHotZone;
var
  AHotZones: TdxHotZoneCollection;
  AHotZone: TdxHotZone;
  I: Integer;
begin
  Update;
  AHotZones := LastDocumentSelectionLayout.HotZones;
  Result := nil;
  for I := 0 to AHotZones.Count - 1 do
  begin
    AHotZone := AHotZones[I];
    if AHotZone.HitTest(AResult.LogicalPoint, AView.DocumentModel.Dpi, AView.ScaleFactor) then
    begin
      Result := AHotZone;
      Break;
    end;
  end;
end;

constructor TdxSelectionLayout.Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
begin
  inherited Create;
  Assert(AView <> nil);
  Assert(APreferredPageIndex >= 0);
  FView := AView;
  FPreferredPageIndex := APreferredPageIndex;
end;

destructor TdxSelectionLayout.Destroy;
begin
  FreeAndNil(FDocumentSelectionLayouts);
  inherited Destroy;
end;

function TdxSelectionLayout.CreateDocumentLayoutPosition(
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
begin
  Result := TdxDocumentLayoutPosition.Create(DocumentLayout, TdxDocumentModel(DocumentLayout.DocumentModel).Selection.PieceTable, ALogPosition);
end;

function TdxSelectionLayout.CreateDocumentSelectionLayout(AStart, AEnd: TdxDocumentLayoutPosition): TdxDocumentSelectionLayout;
begin
  Result := TdxDocumentSelectionLayout.Create(Self, AStart, AEnd, View.DocumentModel.ActivePieceTable);
end;

{ TdxTextBoxSelectionLayout }

function TdxTextBoxSelectionLayout.CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
begin
  Result := TdxTextBoxDocumentLayoutPosition.Create(DocumentLayout, TdxTextBoxContentType(TdxDocumentModel(DocumentLayout.DocumentModel).Selection.PieceTable.ContentType), ALogPosition, PreferredPageIndex);
end;

function TdxTextBoxSelectionLayout.CreateDocumentSelectionLayout(AStart: TdxDocumentLayoutPosition; AEnd: TdxDocumentLayoutPosition): TdxDocumentSelectionLayout;
begin
  Result := TdxTextBoxDocumentSelectionLayout.Create(Self, AStart, AEnd, View.DocumentModel.ActivePieceTable);
end;

{ TdxHeaderFooterSelectionLayout }

function TdxHeaderFooterSelectionLayout.CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterDocumentLayoutPosition.Create(DocumentLayout, TdxDocumentModel(DocumentLayout.DocumentModel).Selection.PieceTable, ALogPosition, PreferredPageIndex);
end;

{ TdxRichEditViewInfo }

constructor TdxRichEditViewInfo.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create;
  FPageViewInfos := TdxRichEditViewPageViewInfoCollection.Create(ADocumentLayout);
  FOwnedRows := TdxOwnedRowInfoList.Create;
  FOwnedPageViewInfos := TdxPageViewInfoCollection.Create;
end;

destructor TdxRichEditViewInfo.Destroy;
begin
  FreeAndNil(FOwnedRows);
  FreeAndNil(FPageViewInfos);
  FreeAndNil(FOwnedPageViewInfos);
  inherited Destroy;
end;

procedure TdxRichEditViewInfo.Clear;
begin
  PageViewInfos.Clear;
  OwnedPageViewInfos.Clear;
  OwnedRows.Clear;
end;

{ TdxRichEditPaintersBase }

constructor TdxRichEditPaintersBase<TLayoutItem, TPainter>.Create;
begin
  inherited Create;
  FPainters := TObjectDictionary<TClass, TPainter>.Create([doOwnsValues]);
end;

destructor TdxRichEditPaintersBase<TLayoutItem, TPainter>.Destroy;
begin
  FreeAndNil(FPainters);
  FreeAndNil(FDefaultPainter);
  inherited Destroy;
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.Clear;
begin
  RemoveDefault;
  Painters.Clear;
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.Add<T>(APainter: TPainter);
begin
  InternalAdd(T, APainter);
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.Remove<T>;
begin
  InternalRemove(T);
end;

function TdxRichEditPaintersBase<TLayoutItem, TPainter>.Get(AType: TClass): TPainter;
begin
  if not Painters.TryGetValue(AType, Result) then
    Result := DefaultPainter;
end;


procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.AddDefault(APainter: TPainter);
begin
  FreeAndNil(FDefaultPainter);
  FDefaultPainter := APainter;
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.RemoveDefault;
begin
  Assert(False);
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.InternalAdd(AType: TClass; APainter: TPainter);
begin
  Painters.AddOrSetValue(AType, APainter);
end;

procedure TdxRichEditPaintersBase<TLayoutItem, TPainter>.InternalRemove(AType: TClass);
begin
  Painters.Remove(AType);
end;

{ TdxRichEditViewPainter }

constructor TdxRichEditViewPainter.Create(AView: TdxRichEditView);
begin
  inherited Create;
  Assert(AView is TdxRichEditView);
  FView := AView;
  FControl := AView.Control;
  FDecoratorPainters := TList<IdxDecoratorPainter>.Create;
  FSelectionPainters := TdxRichEditSelectionPainters.Create;
  FHoverPainters := TdxRichEditHoverPainters.Create;
end;

destructor TdxRichEditViewPainter.Destroy;
begin
  FreeAndNil(FHoverPainters);
  FreeAndNil(FSelectionPainters);
  FreeAndNil(FDecoratorPainters);
  inherited Destroy;
end;

function TdxRichEditViewPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := View.Control.LookAndFeel;
end;

function TdxRichEditViewPainter.BeginDrawPageContentInPixels(APageViewInfo: TdxPageViewInfo): TdxGPMatrix;
var
  AOrigin: TPoint;
  AMeasurer: TdxGdiBoxMeasurer;
begin
  Result := FGraphics.Transform;
  FGraphics.ResetTransform;

  AOrigin := DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(APageViewInfo.ClientBounds.Location, FGraphics.DpiX, FGraphics.DpiY);
  AOrigin.X := AOrigin.X - DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(FPhysicalLeftInvisibleWidth, FGraphics.DpiX);

  AMeasurer := TdxGdiBoxMeasurer(Control.InnerControl.Measurer);
  AOrigin.X := AMeasurer.SnapToPixels(AOrigin.X, FGraphics.DpiX);

  FGraphics.TranslateWorldTransform(AOrigin.X, AOrigin.Y);
  FGraphics.MultiplyTransform(Result);
end;

function TdxRichEditViewPainter.ClipPageContentForDrawingInPixels(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter): TdxRectF;
var
  AClipBounds: TRect;
  AActualClipBounds, AOldClipBounds: TdxRectF;
  AScaleFactor: Single;
begin
  AScaleFactor := View.ScaleFactor;
  AClipBounds := View.CalculatePageContentClipBounds(APageViewInfo);
  AClipBounds := DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(AClipBounds, FGraphics.DpiX, FGraphics.DpiY);
  AActualClipBounds := TdxRectF.Null;
  AActualClipBounds.Left := AClipBounds.X * AScaleFactor;
  AActualClipBounds.Top := AClipBounds.Y * AScaleFactor;
  AActualClipBounds.Width := AClipBounds.Width * AScaleFactor;
  AActualClipBounds.Height := AClipBounds.Height * AScaleFactor;
  AOldClipBounds := Graphics.ClipBounds;
  APainter.ClipBounds := GetNewClientBounds(cxRect(AActualClipBounds), AOldClipBounds);
  Result := AOldClipBounds;
end;

procedure TdxRichEditViewPainter.EndDrawPageContentInPixels(AOldTransform: TdxGPMatrix);
begin
  FGraphics.Transform := AOldTransform;
  AOldTransform.Free;
end;

function TdxRichEditViewPainter.GetDocumentModel: TdxDocumentModel;
begin
  Result := View.Control.DocumentModel;
end;

procedure TdxRichEditViewPainter.AddDecoratorPainter(const APainter: IdxDecoratorPainter);
begin
  DecoratorPainters.Add(APainter);
end;


procedure TdxRichEditViewPainter.BeginDraw(AGraphics: TdxGraphics);
begin
  CacheInitialize(AGraphics);
  FMinReadableTextHeight := Round(DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(6, FGraphics.DpiY) / View.ZoomFactor);
  FPhysicalLeftInvisibleWidth := View.GetPhysicalLeftInvisibleWidth;
  AddSelectionPainters(AGraphics);

  FOriginalTransform := FGraphics.Transform;

  FGraphics.TranslateWorldTransform(-FPhysicalLeftInvisibleWidth, 0);
end;

procedure TdxRichEditViewPainter.AddSelectionPainters(AGraphics: TdxGraphics);
begin
  SelectionPainters.AddDefault(TdxSemitransparentSelectionPainter.Create(AGraphics));
  SelectionPainters.Add<TdxRectangularObjectSelectionLayout>(TdxRectangularObjectSelectionPainter.Create(AGraphics));
end;

procedure TdxRichEditViewPainter.CacheInitialize(AGraphics: TdxGraphics);
begin

  FGraphics := AGraphics;
end;

procedure TdxRichEditViewPainter.EndDraw;
begin
  FGraphics.Transform := FOriginalTransform;
  FreeAndNil(FOriginalTransform);

  CacheDispose;
end;

procedure TdxRichEditViewPainter.CacheDispose;
begin
  FGraphics := nil;

  FCache := nil;
end;

function TdxRichEditViewPainter.BeginDrawPagesContent: TdxGPMatrix;
var
  AScaleFactor: Single;
begin
  FOriginalDrawPagesTransform := Graphics.Transform;
  AScaleFactor := View.ScaleFactor;
  Graphics.ScaleWorldTransform(AScaleFactor, AScaleFactor);

  FZoomModifier := TdxHdcZoomModifier.Create(FGraphics, AScaleFactor);

  Result := FGraphics.Transform;
end;

procedure TdxRichEditViewPainter.EndDrawPagesContent;
begin
  FreeAndNil(FZoomModifier);

  FGraphics.Transform := FOriginalDrawPagesTransform;
  FreeAndNil(FOriginalDrawPagesTransform);
end;

procedure TdxRichEditViewPainter.BeginDrawPageContent(APage: TdxPageViewInfo; ATransform: TdxGPMatrix);
var
  AMeasurer: TdxGdiBoxMeasurer;
  AOrigin: TPoint;
begin
  Graphics.ResetTransform;
  Graphics.TranslateWorldTransform(APage.ClientBounds.X, APage.ClientBounds.Y);
  Graphics.MultiplyTransform(ATransform);

  AMeasurer := Control.InnerControl.Measurer as TdxGdiBoxMeasurer;
  AOrigin := APage.ClientBounds.Location;
  AOrigin.X := AOrigin.X - FPhysicalLeftInvisibleWidth;
  AOrigin.X := AMeasurer.SnapToPixels(AOrigin.X, FGraphics.DpiX);
  FOriginModifier := TdxHdcOriginModifier.Create(FGraphics, AOrigin, View.ScaleFactor, TdxHdcOriginModifier.TMode.Combine);
end;

procedure TdxRichEditViewPainter.ClipPageContent(APage: TdxPageViewInfo; APainter: TdxPainter);
var
  AClipBounds: TRect;
  ANewClipBounds: TdxRectF;
begin
  AClipBounds := View.CalculatePageContentClipBounds(APage);
  FOldClipBounds := Graphics.ClipBounds;
  ANewClipBounds := AClipBounds.ToRectF;
  ANewClipBounds.Intersect(FOldClipBounds);
  APainter.ClipBounds := ANewClipBounds;
end;

procedure TdxRichEditViewPainter.EndDrawPageContent(APage: TdxPageViewInfo; ATransform: TdxGPMatrix);
begin
  FreeAndNil(FOriginModifier);
end;

procedure TdxRichEditViewPainter.BeginDrawInPixels(AGraphics: TdxGraphics);
var
  ATransform: TdxGPMatrix;
  ADpi, AScaleX, AScaleY: Single;
begin
  FGraphics := AGraphics;

  AGraphics.Save;

  ATransform := FGraphics.Transform;

  ADpi := View.DocumentModel.LayoutUnitConverter.Dpi;
  AScaleX := FGraphics.DpiX / ADpi;
  AScaleY := FGraphics.DpiY / ADpi;

  ATransform.Translate(ATransform.OffsetX * (AScaleX - 1), ATransform.OffsetY * (AScaleY - 1));

  FGraphics.PageUnit := TdxGraphicsUnit.guPixel;
  FGraphics.PageScale := 1.0;

  FPixelHdcDpiModifier := TdxHdcDpiModifier.Create(FGraphics, TSize.Create(4096, 4096), Round(View.Control.DpiX));

  FGraphics.Transform := ATransform;
  ATransform.Free;
end;

procedure TdxRichEditViewPainter.EndDrawInPixels;
begin
  FreeAndNil(FPixelHdcDpiModifier);

  FGraphics.Restore;

  FGraphics := nil;

  FCache := nil;
end;

procedure TdxRichEditViewPainter.DrawPageContent(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter);
var
  AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
  APageBounds: TRect;
  AExporter: TdxDocumentLayoutExporter;
  APagePainter: TdxRichEditPagePainter;
  AArgs: TdxRichEditBeforePagePaintEventArgs;
  AInnerControl: IdxInnerControl;
  AControl: TdxInnerRichEditControl;
begin
  AAdapter := TdxWinFormsGraphicsDocumentLayoutExporterAdapter.Create;
  APageBounds := GetPageBounds(APageViewInfo);
  AExporter := View.CreateDocumentLayoutExporter(APainter, AAdapter, APageViewInfo, APageBounds);
  try
    AExporter.ShowWhitespace := FControl.DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText;
    AExporter.MinReadableTextHeight := FMinReadableTextHeight;
    AExporter.SetBackColor(Control.BackgroundPainter.GetActualPageBackColor, APageBounds);
    AExporter.ReadOnly := Control.ReadOnly;

    Assert(APageViewInfo.Page.SecondaryFormattingComplete);

    APagePainter := TdxRichEditPagePainter.Create;
    try
      AArgs := TdxRichEditBeforePagePaintEventArgs.Create(APagePainter, APageViewInfo.Page.PageIndex);
      try
        AInnerControl := Control.InnerControl;
        AControl := TdxInnerRichEditControl(AInnerControl);
        TdxInnerRichEditControlAccess(AControl).RaiseBeforePagePaint(AArgs);
        if AArgs.Painter <> nil then
        begin
          AArgs.Painter.Exporter := AExporter;
          AArgs.Painter.ModelPage := APageViewInfo.Page;
          AArgs.Painter.Page := Control.DocumentLayout.GetPage(APageViewInfo.Page.PageIndex);
          AArgs.Painter.Draw;
        end;
      finally
        AArgs.Free;
      end;
    finally
      APagePainter.Free;
    end;
  finally
    AExporter.Free;
  end;
end;

procedure TdxRichEditViewPainter.DrawPageComment(APageViewInfo: TdxPageViewInfo; APainter: TdxPainter);
begin
  NotImplemented;
end;

function TdxRichEditViewPainter.CommentsVisible(ACount: Integer): Boolean;
begin
  NotImplemented;
  Result := False;
end;

procedure TdxRichEditViewPainter.DrawCommentsBackground(APageViewInfo: TdxPageViewInfo; AGraphics: TdxGraphics);
begin
  NotImplemented;
end;

function TdxRichEditViewPainter.GetPageBounds(APage: TdxPageViewInfo): TRect;
begin
  Result := APage.Page.Bounds;
end;

procedure TdxRichEditViewPainter.DrawPagesContent(ATransform: TdxGPMatrix);
var
  APainter: TdxPainter;
  ACount, I: Integer;
  APageViewInfo: TdxPageViewInfo;
begin
  APainter := TdxPainter(Control.MeasurementAndDrawingStrategy.CreateDocumentPainter(Graphics));
  try
    ACount := View.PageViewInfos.Count;
    for I := 0 to ACount - 1 do
    begin
      APageViewInfo := View.PageViewInfos[I];
      BeginDrawPageContent(APageViewInfo, ATransform);
      try
        ClipPageContent(APageViewInfo, APainter);

        DrawPageContent(APageViewInfo, APainter);

        DrawPageSelection(FGraphics, APageViewInfo);

        RestoreOldClipBounds(APainter);

        DrawHover(APainter);
      finally
        EndDrawPageContent(APageViewInfo, ATransform);
      end;
    end;
  finally
    APainter.Free;
  end;
end;

procedure TdxRichEditViewPainter.RestoreOldClipBounds(APainter: TdxPainter);
begin
  APainter.ClipBounds := FOldClipBounds;
end;

function TdxRichEditViewPainter.GetNewClientBounds(const AClipBounds: TRect; const AOldClipBounds: TdxRectF): TdxRectF;
var
  R: TRect;
begin
  R := TRect.Round(AOldClipBounds);
  R.Intersect(AClipBounds);
  Result := R.ToRectF;
end;

procedure TdxRichEditViewPainter.DrawBoxesInPixels(const ACustomMarkExporter: IdxCustomMarkExporter);
var
  APainter: TdxPainter;
  AExporter: TdxWinFormsNotPrintableGraphicsBoxExporter;
  ACount, I: Integer;
  APageViewInfo: TdxPageViewInfo;
  AOldTransform: TdxGPMatrix;
  AOldClipBounds: TdxRectF;
begin
  APainter := TdxPainter(FControl.MeasurementAndDrawingStrategy.CreateDocumentPainter(Graphics));
  try
    AExporter := TdxWinFormsNotPrintableGraphicsBoxExporter.Create(DocumentModel, APainter, View, ACustomMarkExporter);
    try
      ACount := View.PageViewInfos.Count;

      for I := 0 to ACount - 1 do
      begin
        APageViewInfo := View.PageViewInfos[I];
        AOldTransform := BeginDrawPageContentInPixels(APageViewInfo);
        try
          AOldClipBounds := ClipPageContentForDrawingInPixels(APageViewInfo, APainter);
          AExporter.ExportPage(APageViewInfo);
          APainter.ClipBounds := AOldClipBounds;
        finally
          EndDrawPageContentInPixels(AOldTransform);
        end;
      end;
    finally
      AExporter.Free;
    end;
  finally
    APainter.Free;
  end;
end;



procedure TdxRichEditViewPainter.IntersectClipBounds(APainter: TdxPainter; const AOldClipBounds: TdxRectF; X: Single; Y: Single; AWidth: Single; AHeight: Single);
var
  AClipBounds: TdxRectF;
begin
  AClipBounds.InitSize(X, Y, AWidth, AHeight);
  AClipBounds.Intersect(AOldClipBounds);
  APainter.ClipBounds := AClipBounds;
end;

function TdxRichEditViewPainter.CalculateRulerHeight: Integer;
begin
  Result := 0;
end;

procedure TdxRichEditViewPainter.DrawDecorators(AGraphics: TdxGraphics);
var
  APainter: TdxPainter;
begin
  APainter := TdxPainter(FControl.MeasurementAndDrawingStrategy.CreateDocumentPainter(AGraphics));
  try
    DrawDecorators(APainter);
    DecorateTables(APainter);
  finally
    APainter.Free;
  end;
end;

procedure TdxRichEditViewPainter.DecorateTables(APainter: TdxPainter);
var
  AController: TdxTableViewInfoController;
begin
  AController := View.TableController;
  if AController <> nil then
    DecorateTablesCore(AController, APainter);
end;

procedure TdxRichEditViewPainter.DecorateTablesCore(AController: TdxTableViewInfoController; APainter: TdxPainter);
var
  ADecorator: IdxTableViewInfoDecorator;
begin
  ADecorator := AController.CreateDecorator(APainter);
  try
    ADecorator.Decorate;
  finally
    ADecorator.Free;
  end;
end;

procedure TdxRichEditViewPainter.DrawDecorators(APainter: TdxPainter);
var
  ACount, I: Integer;
  AViewInfos: TdxPageViewInfoCollection;
  ADecoratorPainter: IdxDecoratorPainter;
begin
  ACount := DecoratorPainters.Count;
  AViewInfos := View.PageViewInfos;
  for I := 0 to ACount - 1 do
  begin
    ADecoratorPainter := DecoratorPainters[I];
    ADecoratorPainter.DrawDecorators(APainter, AViewInfos);
  end;
end;

procedure TdxRichEditViewPainter.Draw(AGraphics: TdxGraphics; const ACustomMarkExporter: IdxCustomMarkExporter);
var
  AOldTransform: TdxGPMatrix;
begin
  BeginDraw(AGraphics);
  DrawEmptyPages(AGraphics);
  AOldTransform := BeginDrawPagesContent;
  try
    DrawPagesContent(AOldTransform);
  finally
    AOldTransform.Free;
    EndDrawPagesContent;
  end;
  EndDraw;

  BeginDrawInPixels(AGraphics);
  DrawBoxesInPixels(ACustomMarkExporter);
  ApplyGraphicsTransform(AGraphics, ACustomMarkExporter.CustomMarkVisualInfoCollection);
  EndDrawInPixels;
end;

function TdxRichEditViewPainter.GetCommentBounds(ACommentViewInfo: TdxCommentViewInfo): TRect;
begin
  NotImplemented;
end;

procedure TdxRichEditViewPainter.ApplyGraphicsTransform(AGraphics: TdxGraphics; ACustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection);
var
  ACount, I: Integer;
  APts: TArray<TPoint>;
  AVisualInfo: TdxCustomMarkVisualInfo;
begin
  ACount := ACustomMarkVisualInfoCollection.Count;
  if ACount = 0 then
    Exit;

  SetLength(APts, ACount * 2);
  for I := 0 to ACount - 1 do
  begin
    AVisualInfo := ACustomMarkVisualInfoCollection[I];
    APts[I * 2] := AVisualInfo.Bounds.TopLeft;
    APts[I * 2 + 1] := AVisualInfo.Bounds.BottomRight;
  end;
   AGraphics.TransformPoints(CoordinateSpacePage, CoordinateSpaceWorld, APts);
   for I := 0 to ACount - 1 do
   begin
     AVisualInfo := ACustomMarkVisualInfoCollection[I];
     AVisualInfo.Bounds := TRect.Create(APts[I * 2], APts[I * 2 + 1]);
   end;
end;

procedure TdxRichEditViewPainter.DrawAtPageCore(AGraphics: TdxGraphics; APage: TdxPageViewInfo; const ADraw: TdxDrawAtPageDelegate);
var
  APageViewInfoClientBounds: TRect;
  AOldTransform: TdxGPMatrix;
  AZoomModifier: TdxHdcZoomModifier;
  AScaleFactor: Single;
  AOrigin: TPoint;
  AOriginModifier: TdxHdcOriginModifier;
begin
  APageViewInfoClientBounds := APage.ClientBounds;

  AOldTransform := AGraphics.Transform;
  try
    AGraphics.ResetTransform;
    AGraphics.TranslateWorldTransform(
      APageViewInfoClientBounds.X - View.GetPhysicalLeftInvisibleWidth,
      APageViewInfoClientBounds.Y);
    AGraphics.MultiplyTransform(AOldTransform);
    AScaleFactor := View.ScaleFactor;
    AGraphics.ScaleWorldTransform(AScaleFactor, AScaleFactor);

    AZoomModifier := TdxHdcZoomModifier.Create(AGraphics, AScaleFactor);
    try
      AOrigin := APageViewInfoClientBounds.Location;
      AOrigin.X := AOrigin.X - View.GetPhysicalLeftInvisibleWidth;
      AOriginModifier := TdxHdcOriginModifier.Create(AGraphics, AOrigin, AScaleFactor, TdxHdcOriginModifier.TMode.Combine);
      try
        ADraw(AGraphics);
      finally
        AOriginModifier.Free;
      end;
    finally
      AZoomModifier.Free;
    end;
  finally
    AGraphics.Transform := AOldTransform;
    AOldTransform.Free;
  end;
end;

procedure TdxRichEditViewPainter.DrawCaretCore(AGraphics: TdxGraphics; ACaret: TdxCaret);
begin
  ACaret.Draw(AGraphics);
end;

procedure TdxRichEditViewPainter.DrawCaretAtPage(AGraphics: TdxGraphics; ACaret: TdxCaret; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawAtPageDelegate;
begin
  ADraw :=
    procedure (C: TdxGraphics)
    begin
      DrawCaretCore(C, ACaret);
    end;
  DrawAtPageCore(AGraphics, APage, ADraw);
end;

procedure TdxRichEditViewPainter.DrawReversibleHorizontalLineAtPage(AGraphics: TdxGraphics; Y: Integer; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawAtPageDelegate;
begin
  ADraw :=
    procedure (G: TdxGraphics)
    var
      ABounds: TRect;
    begin
      ABounds := APage.Page.Bounds;
      ABounds.Y := Y;
      ABounds.Height := 0;
      DrawReversibleHorizontalLine(G, ABounds);
    end;
  DrawAtPageCore(AGraphics, APage, ADraw);
end;

procedure TdxRichEditViewPainter.DrawReversibleVerticalLineAtPage(AGraphics: TdxGraphics; X: Integer; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawAtPageDelegate;
begin
  ADraw :=
    procedure (G: TdxGraphics)
    var
      ABounds: TRect;
    begin
      ABounds := APage.Page.Bounds;
      ABounds.X := X;
      ABounds.Width := 0;
      DrawReversibleVerticalLine(G, ABounds);
    end;
  DrawAtPageCore(AGraphics, APage, ADraw);
end;

procedure TdxRichEditViewPainter.DrawReversibleHorizontalLine(AGraphics: TdxGraphics; const ABounds: TRect);
var
  AHdc: THandle;
begin
  AHdc := AGraphics.GetHdc;
  try
    DrawReversibleHorizontalLine(AHdc, ABounds);
  finally
    AGraphics.ReleaseHdc(AHdc);
  end;
end;

procedure TdxRichEditViewPainter.DrawReversibleHorizontalLine(AHdc: THandle; const ABounds: TRect);
begin
  DrawReversible(AHdc, ABounds, DrawReversibleHorizontalLineCore);
end;

procedure TdxRichEditViewPainter.DrawReversibleVerticalLine(AGraphics: TdxGraphics; const ABounds: TRect);
var
  AHdc: THandle;
begin
  AHdc := AGraphics.GetHdc;
  try
    DrawReversibleVerticalLine(AHdc, ABounds);
  finally
    AGraphics.ReleaseHdc(AHdc);
  end;
end;

procedure TdxRichEditViewPainter.DrawReversibleVerticalLine(AHdc: THandle; const ABounds: TRect);
begin
  DrawReversible(AHdc, ABounds, DrawReversibleVerticalLineCore);
end;

procedure TdxRichEditViewPainter.DrawReversibleFrameAtPage(AGraphics: TdxGraphics; const ABounds: TRect; APage: TdxPageViewInfo);
var
  ADraw: TdxDrawAtPageDelegate;
begin
  ADraw :=
    procedure (G: TdxGraphics)
    begin
    end;
  DrawAtPageCore(AGraphics, APage, ADraw);
  NotImplemented;
end;

procedure TdxRichEditViewPainter.DrawReversibleFrame(AGraphics: TdxGraphics; const ABounds: TRect);
var
  AHdc: THandle;
begin
  AHdc := AGraphics.GetHdc;
  try
    DrawReversibleFrame(AHdc, ABounds);
  finally
    AGraphics.ReleaseHdc(AHdc);
  end;
end;

procedure TdxRichEditViewPainter.DrawReversibleFrame(AHdc: THandle; const ABounds: TRect);
begin
  DrawReversible(AHdc, ABounds, DrawReversibleFrameCore);
end;

procedure TdxRichEditViewPainter.DrawReversible(AHdc: THandle; const ABounds: TRect; const ADraw: TdxDrawReversibleDelegate);
var
  AOldRop2: Integer;
  APen, AOldPen, ABrush, AOldBrush: THandle;
begin
  AOldRop2 := SetROP2(AHdc, R2_NOTXORPEN);
  try
    APen := CreatePen(PS_DOT, 0, 0);

    AOldPen := SelectObject(AHdc, APen);
    try
      ABrush := GetStockObject(NULL_BRUSH);
      AOldBrush := SelectObject(AHdc, ABrush);
      try
        ADraw(AHdc, ABounds);
      finally
        SelectObject(AHdc, AOldBrush);
      end;
    finally
      SelectObject(AHdc, AOldPen);
      DeleteObject(APen);
    end;
  finally
    SetROP2(AHdc, AOldRop2);
  end;
end;

procedure TdxRichEditViewPainter.DrawReversibleFrameCore(AHdc: THandle; const ABounds: TRect);
begin
  MoveToEx(AHdc, ABounds.Left, ABounds.Top, nil);
  LineTo(AHdc, ABounds.Right, ABounds.Top);
  LineTo(AHdc, ABounds.Right, ABounds.Bottom);
  LineTo(AHdc, ABounds.Left, ABounds.Bottom);
  LineTo(AHdc, ABounds.Left, ABounds.Top);
end;

procedure TdxRichEditViewPainter.DrawReversibleHorizontalLineCore(AHdc: THandle; const ABounds: TRect);
begin
  MoveToEx(AHdc, ABounds.Left, ABounds.Top, nil);
  LineTo(AHdc, ABounds.Right, ABounds.Top);
end;

procedure TdxRichEditViewPainter.DrawReversibleVerticalLineCore(AHdc: THandle; const ABounds: TRect);
begin
  MoveToEx(AHdc, ABounds.Left, ABounds.Top, nil);
  LineTo(AHdc, ABounds.Left, ABounds.Bottom);
end;

procedure TdxRichEditViewPainter.DrawPageSelection(AGraphics: TdxGraphics; APage: TdxPageViewInfo);
var
  ASelections: TdxPageSelectionLayoutsCollection;
  I: Integer;
begin
  if not View.HasSelection then
    Exit;
  ASelections := View.SelectionLayout.GetPageSelection(APage.Page);
  try
    if ASelections = nil then
      Exit;
    for I := 0 to ASelections.Count - 1 do
      DrawPageSelectionCore(ASelections[I]);
  finally
    ASelections.Free;
  end;
end;

procedure TdxRichEditViewPainter.DrawPageSelectionCore(const ASelection: IdxSelectionLayoutItem);
begin
  if ASelection <> nil then
    ASelection.Draw(SelectionPainters.Get(ASelection.GetType));
end;

procedure TdxRichEditViewPainter.DrawHover(APainter: TdxPainter);
var
  AHoverLayoutItem: IdxHoverLayoutItem;
begin
  AHoverLayoutItem := View.HoverLayout;
  if AHoverLayoutItem <> nil then
    DrawHoverCore(AHoverLayoutItem, APainter);
end;

procedure TdxRichEditViewPainter.DrawHoverCore(const AHoverLayoutItem: IdxHoverLayoutItem; APainter: TdxPainter);
begin
  NotImplemented;
end;

procedure TdxRichEditViewPainter.DrawEmptyPages(AGraphics: TdxGraphics);
var
  ACount, I: Integer;
begin
  ACount := View.PageViewInfos.Count;
  for I := 0 to ACount - 1 do
    DrawEmptyPage(AGraphics, View.PageViewInfos[I]);
end;

procedure TdxRichEditViewPainter.ResetCache;
begin
end;

{ TdxRichEditViewBackgroundPainter }

constructor TdxRichEditViewBackgroundPainter.Create(AView: TdxRichEditView);
begin
  inherited Create;
  FView := AView;
end;

procedure TdxRichEditViewBackgroundPainter.Draw(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, TdxAlphaColors._3DDkShadow);
end;

function TdxRichEditViewBackgroundPainter.GetActualPageBackColor: TdxAlphaColor;
var
  ADocumentProperties: TdxDocumentProperties;
  APageBackColor: TdxAlphaColor;
begin
  ADocumentProperties := View.DocumentModel.DocumentProperties;
  APageBackColor := ADocumentProperties.PageBackColor;
  if ADocumentProperties.DisplayBackgroundShape and not TdxAlphaColors.IsEmpty(APageBackColor) then
    Result := APageBackColor
  else
    Result := View.ActualBackColor;
end;

function TdxRichEditViewBackgroundPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := View.Control.LookAndFeel;
end;

{ TdxRichEditViewSkinBackgroundPainter }

constructor TdxRichEditViewSkinBackgroundPainter.Create(AView: TdxRichEditView);
begin
  inherited Create(AView);
  View.Control.LookAndFeel.Painter.GetPainterData(FSkinInfo);
end;

destructor TdxRichEditViewSkinBackgroundPainter.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TdxRichEditViewSkinBackgroundPainter.Draw(AGraphics: TdxGraphics;
  const ABounds: TRect);
var
  AHdc: HDC;
  B: TBitmap;
begin
  if SkinInfo.PrintingPreviewBackground.Image <> nil then
  begin
    AHdc := AGraphics.GetHDC;
    try
      B := GetBackground(ABounds);
      cxBitBlt(AHdc, B.Canvas.Handle, ABounds, TPoint.Null, SRCCOPY);
    finally
      AGraphics.ReleaseHDC(AHdc);
    end;
  end
  else
    SkinInfo.PrintingPreviewBackground.DrawEx(AGraphics, ABounds);
end;

function TdxRichEditViewSkinBackgroundPainter.GetBackground(const ABounds: TRect): TBitmap;
var
  ASize: TSize;
begin
  if (Background = nil) or (Background.Width <> ABounds.Width) or (Background.Height <> ABounds.Height) then
  begin
    FreeAndNil(FBackground);
    ASize.Init(Max(8, ABounds.Width), Max(8, ABounds.Height));
    FBackground := TBitmap.Create;
    FBackground.SetSize(ASize.Width, ASize.Height);
    SkinInfo.PrintingPreviewBackground.Draw(FBackground.Canvas.Handle, TRect.Create(0, 0, ASize.cx, ASize.cy));
  end;
  Result := FBackground;
end;

{ TdxRichEditView }

constructor TdxRichEditView.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  Assert(AControl <> nil);
  FControl := AControl;
  FBackColor := TdxAlphaColors.Window;
  FZoomFactor := 1;
  FBackColor := TdxAlphaColors.Window;
  FOldFirstPageIndex := -1;
  FOldLastPageIndex := -1;
  Bounds := TRect.CreateSize(0, 0, MinWidth, MinHeight);
  FAllowDisplayLineNumbers := GetDefaultAllowDisplayLineNumbers;
  FDocumentLayout := CreateDocumentLayout(DocumentModel, AControl.InnerControl);
  FFormattingController := CreateDocumentFormattingController;
  FVerticalScrollController := AControl.CreateRichEditViewVerticalScrollController(Self);
  FHorizontalScrollController := AControl.CreateRichEditViewHorizontalScrollController(Self);
  FViewInfo := TdxRichEditViewInfo.Create(DocumentLayout);
  FPageViewInfoGenerator := CreatePageViewInfoGenerator;
  FSelectionLayout := TdxSelectionLayout.Create(Self, 0);
  FCaretPosition := TdxCaretPosition.Create(Self, 0);

  FFormattingController.ResetSecondaryFormattingForPage.Add(OnResetSecondaryFormattingForPage);

  FPadding := CreatePadding;
  FPadding.OnChange := PaddingChanged;
  DocumentModel.LayoutOptions.DraftView.Changed.Add(LayoutOptionsChanged);
  DocumentModel.LayoutOptions.SimpleView.Changed.Add(LayoutOptionsChanged);
  DocumentModel.LayoutOptions.PrintLayoutView.Changed.Add(LayoutOptionsChanged);
end;

destructor TdxRichEditView.Destroy;
begin
  FreeAndNil(FDocumentLayout);
  FreeAndNil(FCaretPosition);
  FreeAndNil(FSelectionLayout);
  FreeAndNil(FPageViewInfoGenerator);
  FreeAndNil(FViewInfo);
  FreeAndNil(FHorizontalScrollController);
  FreeAndNil(FVerticalScrollController);
  FreeAndNil(FFormattingController);
  FreeAndNil(FPadding);
  inherited Destroy;
end;

procedure TdxRichEditView.StopFormatting;
begin
  SuspendFormatting;
  TdxUIThreadSyncService.Unsubscribe(Self);
  UnsubscribePageFormattingComplete;
  if (DocumentModel <> nil) and (DocumentModel.LayoutOptions <> nil) then
  begin
    DocumentModel.LayoutOptions.DraftView.Changed.Remove(LayoutOptionsChanged);
    DocumentModel.LayoutOptions.SimpleView.Changed.Remove(LayoutOptionsChanged);
    DocumentModel.LayoutOptions.PrintLayoutView.Changed.Remove(LayoutOptionsChanged);
  end;
end;

procedure TdxRichEditView.CheckExecutedAtBackgroundThread;
begin
end;

procedure TdxRichEditView.CheckExecutedAtUIThread;
begin
end;

procedure TdxRichEditView.Activate(const AViewBounds: TRect);
var
  APieceTable: TdxPieceTable;
  AFrom, ATo: TdxDocumentModelPosition;
begin
  Assert(AViewBounds.Location.IsEqual(cxNullPoint));
  CaretPosition.Invalidate;
  CaretPosition.InvalidateInputPosition;
  SelectionLayout.Invalidate;
  ResetPages(TdxPageGenerationStrategyType.RunningHeight);
  FormattingController.Reset(False);

  ActivateScrollBars;

  SubscribeDocumentLayoutEvents;
  SubscribePageFormattingComplete;

  PerformResize(AViewBounds);

  APieceTable := DocumentModel.MainPieceTable;
  AFrom := TdxDocumentModelPosition.Create(APieceTable);
  ATo := TdxDocumentModelPosition.FromParagraphEnd(APieceTable, APieceTable.Paragraphs.Last.Index);
  Formatter.UpdateSecondaryPositions(AFrom, ATo);
  ApplyChanges(APieceTable);
  Formatter.NotifyDocumentChanged(AFrom, ATo, TdxDocumentLayoutResetType.AllPrimaryLayout);
end;

procedure TdxRichEditView.BeginDocumentRendering;
var
  I: Integer;
  ALastRow: TdxRow;
  APage, ALastPage: TdxPage;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  AMinGapInLayoutUnit: Integer;
begin
  if Control.UseSkinMargins then
  begin
    AUnitConverter := DocumentModel.LayoutUnitConverter;
    AMinGapInLayoutUnit := Ceil(AUnitConverter.DocumentsToLayoutUnits(30) / ScaleFactor);
    PageViewInfoGenerator.HorizontalPageGap := Max(AMinGapInLayoutUnit,
      Ceil(AUnitConverter.PixelsToLayoutUnits(Ceil(Control.SkinLeftMargin + Control.SkinRightMargin)) / ScaleFactor));
    PageViewInfoGenerator.VerticalPageGap := Max(AMinGapInLayoutUnit,
      Ceil(AUnitConverter.PixelsToLayoutUnits(Ceil(Control.SkinTopMargin + Control.SkinBottomMargin)) / ScaleFactor));
  end;
  Formatter.BeginDocumentRendering(IsPrimaryFormattingCompleteForVisibleHeight2);
  if Formatter.AlreadySuspended then
    Exit;
  if PageViewInfos.Count = 0 then
  begin
    ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
    ALastPage := DocumentLayout.Pages.Last;
    PageViewInfoGenerator.FirstPageOffsetAnchor.PageIndex := DocumentLayout.Pages.Count - 1;
    ALastRow := ALastPage.Areas.Last.Columns.Last.Rows.Last;
    FPageViewInfoGenerator.FirstPageOffsetAnchor.VerticalOffset := ALastRow.Bounds.Top;
    GeneratePages;
  end;
  for I := 0 to PageViewInfos.Count - 1 do
  begin
    APage := PageViewInfos[I].Page;
    Assert(APage.PrimaryFormattingComplete);
    if not APage.SecondaryFormattingComplete then
      Formatter.PerformPageSecondaryFormatting(APage);
    Assert(APage.SecondaryFormattingComplete);
  end;
  Assert(PageViewInfos.Count > 0);
end;

function TdxRichEditView.CalculateDocumentLayoutResetType(
  AChanges: TdxDocumentModelDeferredChanges): TdxDocumentLayoutResetType;
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := AChanges.ChangeActions;
  if TdxDocumentModelChangeAction.ResetAllPrimaryLayout in AChangeActions then
    Exit(TdxDocumentLayoutResetType.AllPrimaryLayout);
  if TdxDocumentModelChangeAction.ResetPrimaryLayout  in AChangeActions then
    Exit(TdxDocumentLayoutResetType.PrimaryLayoutFormPosition);
  if TdxDocumentModelChangeAction.ResetSecondaryLayout in AChangeActions then
    Exit(TdxDocumentLayoutResetType.PrimaryLayoutFormPosition)
  else
    Exit(TdxDocumentLayoutResetType.None);
end;

function TdxRichEditView.CalculateFirstInvisiblePageIndexBackward: Integer;
var
  AFirstRow: TdxPageViewInfoRow;
  APageViewInfo: TdxPageViewInfo;
begin
  Result := -1;
  AFirstRow := PageViewInfoGenerator.ActiveGenerator.PageRows.First;
  APageViewInfo := AFirstRow.First;
  if APageViewInfo <> nil then
    Result := FormattingController.PageController.Pages.IndexOf(APageViewInfo.Page) - 1;
end;

function TdxRichEditView.CalculateFirstInvisiblePageIndexForward: Integer;
var
  ALastRow: TdxPageViewInfoRow;
  APageViewInfo: TdxPageViewInfo;
begin
  ALastRow := PageViewInfoGenerator.ActiveGenerator.PageRows.Last;
  APageViewInfo := ALastRow.Last;
  if APageViewInfo = nil then
    Result := -1
  else
    Result := 1 + FormattingController.PageController.Pages.IndexOf(APageViewInfo.Page);
end;


function TdxRichEditView.CalculateFloatingObjectHitTest(APage: TdxPage; const APoint: TPoint;
  const APredicate: TdxPredicate<TdxFloatingObjectBox>): TdxFloatingObjectBox;
var
  AFloatingObjects: TdxFloatingObjectBoxList;
begin
  AFloatingObjects := APage.GetSortedNonBackgroundFloatingObjects;
  try
    Result := CalculateFloatingObjectHitTestCore(APoint, AFloatingObjects, APredicate);
    if Result = nil then
      Result := CalculateFloatingObjectHitTestCore(APoint, APage.InnerBackgroundFloatingObjects, APredicate);
  finally
    AFloatingObjects.Free;
  end;
end;

function TdxRichEditView.CalculateFloatingObjectHitTestCore(const APoint: TPoint; AList: TdxFloatingObjectBoxList;
  const APredicate: TdxPredicate<TdxFloatingObjectBox>): TdxFloatingObjectBox;
var
  I: Integer;
begin
  Result := nil;
  if AList <> nil then
  begin
    for I := AList.Count - 1 downto 0 do
      if IsFloatingObjectHit(AList[I], APoint) and APredicate(AList[I]) then
        Exit(AList[I]);
  end;
end;

function TdxRichEditView.CalculateHitTest(const APoint: TPoint;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  ARequest.PhysicalPoint := APoint;
  ARequest.DetailsLevel := ADetailsLevel;
  ARequest.Accuracy := DefaultHitTestPageAccuracy or NearestPageArea or NearestColumn or
    NearestTableRow or NearestTableCell or NearestRow or NearestBox or NearestCharacter;

  Result := HitTestCore(ARequest, True);
  if not Result.IsValid(ADetailsLevel) then
    Result := nil;
end;

function TdxRichEditView.CalculateNearestCharacterHitTest(const APoint: TPoint; APieceTable: TdxPieceTable): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(APieceTable);
  ARequest.PhysicalPoint := APoint;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Character;
  ARequest.Accuracy := DefaultHitTestPageAccuracy or NearestPageArea or NearestColumn or
    NearestTableRow or NearestTableCell or NearestRow or NearestBox or NearestCharacter;

  Result := HitTestCore(ARequest, (DefaultHitTestPageAccuracy and ExactPage) <> 0);
  if not Result.IsValid(TdxDocumentLayoutDetailsLevel.Character) then
    FreeAndNil(Result);
end;

function TdxRichEditView.CalculateNearestPageHitTest(const APoint: TPoint;
  AStrictHitIntoPageBounds: Boolean): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  ARequest.PhysicalPoint := APoint;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Page;
  ARequest.Accuracy := NearestPage or NearestPageArea or NearestColumn or
    NearestTableRow or NearestTableCell or NearestRow or NearestBox or NearestCharacter;

  Result := HitTestCore(ARequest, AStrictHitIntoPageBounds);
  if not Result.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    FreeAndNil(Result);
end;

function TdxRichEditView.CalculateOptimalHorizontalScrollbarPosition(APageViewInfo: TdxPageViewInfo): Integer;
var
  ALogicalVisibleWidth: Int64;
  APageClientBounds: TRect;
  AHalfOfHorizontalPageGap: Integer;
begin
  ALogicalVisibleWidth := Round(Bounds.Width / ScaleFactor);
  APageClientBounds := APageViewInfo.Page.ClientBounds;
  AHalfOfHorizontalPageGap := Trunc(PageViewInfoGenerator.HorizontalPageGap / 2);
  if ALogicalVisibleWidth > APageViewInfo.Page.Bounds.Width + PageViewInfoGenerator.HorizontalPageGap then
    Result := 0
  else
    if ALogicalVisibleWidth > APageViewInfo.Page.Bounds.Width then
      Result := Trunc((APageViewInfo.Page.Bounds.Width + PageViewInfoGenerator.HorizontalPageGap - ALogicalVisibleWidth) / 2 - FixedLeftTextBorderOffset)
    else
      if ALogicalVisibleWidth > APageViewInfo.Page.Bounds.Right - APageClientBounds.Left + AHalfOfHorizontalPageGap then
        Result := APageViewInfo.Page.Bounds.Width + PageViewInfoGenerator.HorizontalPageGap - ALogicalVisibleWidth - FixedLeftTextBorderOffset
      else
        Result := AHalfOfHorizontalPageGap + APageClientBounds.Left - FixedLeftTextBorderOffset;
end;

function TdxRichEditView.CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect;
var
  AScaleFactor: Single;
  AClipWidth, AClipHeight: Integer;
begin
  AScaleFactor := ScaleFactor;
  AClipWidth := APage.ClientBounds.Width;
  AClipHeight := APage.ClientBounds.Height;
  Result := TRect.CreateSize(0, 0, AClipWidth, AClipHeight);
  Result.X := Ceil(Result.X / AScaleFactor);
  Result.Y := Ceil(Result.Y / AScaleFactor);
  Result.Width := Ceil(Result.Width / AScaleFactor);
  Result.Height := Ceil(Result.Height / AScaleFactor) + 1;
  Result := ExpandClipBoundsToPaddings(Result);
end;

function TdxRichEditView.CalculateResetFromPosition(APieceTable: TdxPieceTable;
  AChanges: TdxDocumentModelDeferredChanges; AResetType: TdxDocumentLayoutResetType): TdxDocumentModelPosition;
var
  AChangeStart: TdxDocumentModelPosition;
  AModelPosition: TdxDocumentModelPosition;
begin
  AChangeStart := AChanges.ChangeStart;
  if AResetType = TdxDocumentLayoutResetType.PrimaryLayoutFormPosition then
  begin
    AModelPosition := TdxDocumentModelPosition.FromParagraphStart(APieceTable, AChangeStart.ParagraphIndex);
    AModelPosition := EnsurePositionVisibleWhenHiddenTextNotShown(DocumentModel, AModelPosition);
    AModelPosition := EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(AModelPosition);
    Exit(EnsureTopLevelParagraph(AModelPosition));
  end;
  Result := AChangeStart;
end;

function TdxRichEditView.CalculateVisiblePageBounds(const APageBounds: TRect; APageViewInfo: TdxPageViewInfo): TRect;
var
  APageViewInfoTotalHeight: Integer;
  AViewPort, AExtendedViewPortBounds, APageViewInfoVisibleBounds, AVisibleBounds: TRect;
begin
  APageViewInfoTotalHeight := APageViewInfo.Bounds.Height;
  if APageViewInfoTotalHeight > 0 then
  begin
    AViewPort := PageViewInfoGenerator.ViewPortBounds;
    AExtendedViewPortBounds := TRect.CreateSize(0, AViewPort.Top, MaxInt, AViewPort.Height);

    APageViewInfoVisibleBounds := AExtendedViewPortBounds;
    APageViewInfoVisibleBounds.Intersect(APageViewInfo.Bounds);

    AVisibleBounds := APageBounds;
    AVisibleBounds := cxRectSetTop(AVisibleBounds, Math.Floor(APageBounds.Height / APageViewInfoTotalHeight * (APageViewInfoVisibleBounds.Top - APageViewInfo.Bounds.Top)));
    AVisibleBounds.Height := Math.Ceil(APageBounds.Height / APageViewInfoTotalHeight * APageViewInfoVisibleBounds.Height);
    Result := AVisibleBounds;
  end
  else
    Result := cxNullRect;
end;

procedure TdxRichEditView.ResetBackColor;
begin
  BackColor := TdxAlphaColors.Window;
end;

procedure TdxRichEditView.LayoutOptionsChanged(ASender: TObject; Args: TdxRichEditNotificationOptionsChangedArgs);
begin
  if TdxRichEditOptionsAction.MatchHorizontalTableIndentsToTextEdge in Args.Actions then
  begin
    DocumentModel.BeginUpdate;
    try
      DocumentModel.OnMatchHorizontalTableIndentsToTextEdgeOptionsChanged;
      FFormattingController.RowsController.MatchHorizontalTableIndentsToTextEdge := TdxViewLayoutOptionsBase(ASender).MatchHorizontalTableIndentsToTextEdge;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

function TdxRichEditView.GetActualBackColor: TdxAlphaColor;
begin
  Result := BackColor;
end;

function TdxRichEditView.GetMinWidth: Integer;
begin
  Result := DefaultMinWidth;
end;

function TdxRichEditView.GetMinHeight: Integer;
begin
  Result := DefaultMinHeight;
end;

function TdxRichEditView.GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactPage;
end;

function TdxRichEditView.ChoosePageViewInfoForOptimalHorizontalScrollbarPosition: TdxPageViewInfo;
var
  APageViewInfoRows: TdxPageViewInfoRowCollection;
  ARow: TdxPageViewInfoRow;
  X, AOffset: Single;
  I: Integer;
  APageViewInfo: TdxPageViewInfo;
begin
  APageViewInfoRows := PageViewInfoGenerator.ActiveGenerator.PageRows;
  ARow := APageViewInfoRows[0];
  if ARow.Count <= 0 then
    Exit(nil);
  Result := ARow[0];
  X := Result.Page.ClientBounds.Left * ScaleFactor + Result.Bounds.Left;
  for I := 0 to APageViewInfoRows.Count - 1 do
  begin
    APageViewInfo := APageViewInfoRows[I][0];
    AOffset := APageViewInfo.Page.ClientBounds.Left * ScaleFactor + APageViewInfo.Bounds.Left;
    if AOffset > X then
    begin
      X := AOffset;
      Result := APageViewInfo;
    end;
  end;
end;

procedure TdxRichEditView.CorrectZoomFactor;
var
  AOptions: TdxRichEditBehaviorOptions;
  AZoom, ADefaultMaxZoomFactor: Single;
begin
  AOptions := Control.InnerControl.Options.Behavior;
  AZoom := Math.Max(DefaultZoomFactor, AOptions.MinZoomFactor);
  ADefaultMaxZoomFactor := AOptions.DefaultMaxZoomFactor;
  if SameValue(AOptions.MaxZoomFactor, ADefaultMaxZoomFactor) then
    FZoomFactor := AZoom
  else
    FZoomFactor := Math.Min(AZoom, AOptions.MaxZoomFactor);
end;

function TdxRichEditView.CreateDocumentLayout(ADocumentModel: TdxDocumentModel;
  const AMeasurerProvider: IdxBoxMeasurerProvider): TdxDocumentLayout;
begin
  Result := TdxDocumentLayout.Create(ADocumentModel, AMeasurerProvider);
end;

function TdxRichEditView.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
  AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := FormattingController.PageController.CreateHitTestCalculator(ARequest, AResult);
end;

function TdxRichEditView.CreateLogicalPoint(const AClientBounds: TRect; const APoint: TPoint): TPoint;
var
  AMatrix: TdxTransformMatrix;
begin
  AMatrix := CreateTransformMatrix(AClientBounds);
  try
    AMatrix.Invert;
    Result := AMatrix.TransformPoint(APoint);
  finally
    AMatrix.Free;
  end;
end;

function TdxRichEditView.CreateLogicalRectangle(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect;
var
  AMatrix: TdxTransformMatrix;
begin
  AMatrix := CreateTransformMatrix(APageViewInfo.ClientBounds);
  try
    AMatrix.Invert;
    Result := AMatrix.TransformRect(ABounds);
  finally
    AMatrix.Free;
  end;
end;

function TdxRichEditView.CreatePhysicalPoint(APageViewInfo: TdxPageViewInfo; const APoint: TPoint): TPoint;
var
  AMatrix: TdxTransformMatrix;
begin
  AMatrix := CreateTransformMatrix(APageViewInfo.ClientBounds);
  try
    Result := AMatrix.TransformPoint(APoint);
  finally
    AMatrix.Free;
  end;
end;

function TdxRichEditView.CreatePhysicalRectangle(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect;
begin
  Result := CreatePhysicalRectangle(APageViewInfo.ClientBounds, ABounds);
end;

function TdxRichEditView.CreatePhysicalRectangle(const APageViewInfoClientBounds, ABounds: TRect): TRect;
var
  AMatrix: TdxTransformMatrix;
begin
  AMatrix := CreateTransformMatrix(APageViewInfoClientBounds);
  try
    Result := AMatrix.TransformRect(ABounds);
  finally
    AMatrix.Free;
  end;
end;

function TdxRichEditView.CreatePhysicalRectangleFast(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect;
var
  AClientBounds: TRect;
  X, Y, W, H: Single;
begin
  AClientBounds := APageViewInfo.ClientBounds;
  X := ScaleFactor * ABounds.Left + AClientBounds.Left - GetPhysicalLeftInvisibleWidth;
  Y := ScaleFactor * ABounds.Top + AClientBounds.Top;
  W := ScaleFactor * ABounds.Width;
  H := ScaleFactor * ABounds.Height;
  Result.InitSize(Trunc(X), Trunc(Y), Trunc(W), Trunc(H));
end;

function TdxRichEditView.CreateTransformMatrix(const AClientBounds: TRect): TdxTransformMatrix;
begin
  Result := TdxTransformMatrix.Create;
  Result.Translate(AClientBounds.Left - GetPhysicalLeftInvisibleWidth, AClientBounds.Top);
  Result.Scale(ScaleFactor, ScaleFactor);
end;

procedure TdxRichEditView.Deactivate;
begin
  UnsubscribePageFormattingComplete;
  UnsubscribeDocumentLayoutEvents;
  DeactivateScrollBars;
end;

procedure TdxRichEditView.EndDocumentRendering;
begin
  Formatter.EndDocumentRendering;
end;

procedure TdxRichEditView.EnforceFormattingCompleteForVisibleArea;
begin
  if Control.InnerControl.DocumentModel.IsUpdateLocked then
    Exit;
  BeginDocumentRendering;
  EndDocumentRendering;
end;

procedure TdxRichEditView.EnsureCaretVisible(ACheckCaretVisibility: Boolean = True);
var
  ACommand: TdxRichEditCommand;
begin
  if DocumentModel.IsUpdateLocked then
  begin
    DocumentModel.DeferredChanges.EnsureCaretVisible := True;
    Exit;
  end;

  if ACheckCaretVisibility and (DocumentModel.Selection.Length > 0) then
    Exit;

  ACommand := TdxEnsureCaretVisibleVerticallyCommand.Create(Control);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  ACommand := TdxEnsureCaretVisibleHorizontallyCommand.Create(Control);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditView.EnsureFormattingCompleteForLogPosition(ALogPosition: TdxDocumentLogPosition);
begin
  CheckExecutedAtUIThread;
  Formatter.WaitForPrimaryLayoutReachesLogPosition(ALogPosition);
end;

procedure TdxRichEditView.EnsureFormattingCompleteForPreferredPage(APreferredPageIndex: Integer);
begin
  CheckExecutedAtUIThread;
  Formatter.WaitForPrimaryLayoutReachesPreferredPage(APreferredPageIndex);
end;

procedure TdxRichEditView.EnsureFormattingCompleteForSelection;
begin
  EnsureFormattingCompleteForLogPosition(DocumentModel.Selection.NormalizedEnd);
end;

procedure TdxRichEditView.EnsureCaretVisibleOnResize;
begin
end;

procedure TdxRichEditView.EnsurePageSecondaryFormattingComplete(APage: TdxPage);
var
  APos: TdxDocumentModelPosition;
begin
  CheckExecutedAtUIThread;
  if APage.PrimaryFormattingComplete and APage.SecondaryFormattingComplete then
    Exit;
  Formatter.WaitForPagePrimaryLayoutComplete(APage);
  Assert(APage.PrimaryFormattingComplete);
  APos := APage.GetLastPosition(DocumentModel.ActivePieceTable);
  Formatter.WaitForSecondaryLayoutReachesPosition(APos);
end;

function TdxRichEditView.EnsurePositionNotBeforeSectionBreakAfterParagraphBreak(
  const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AParagraphs: TdxParagraphBaseCollection;
  ARuns: TdxRunCollection;
  AParagraph: TdxParagraphBase;
begin
  AParagraphs := AModelPosition.PieceTable.Paragraphs;
  ARuns := AModelPosition.PieceTable.Runs;
  AParagraph := AParagraphs[AModelPosition.ParagraphIndex];
  while (AModelPosition.RunIndex > 0) and (AParagraph.Length = 1) and
    (ARuns[AModelPosition.RunIndex] is TdxSectionRun) do
  begin
    AModelPosition.ParagraphIndex := AModelPosition.ParagraphIndex - 1;
    AParagraph := AParagraphs[AModelPosition.ParagraphIndex];
    AModelPosition.RunIndex := AParagraph.FirstRunIndex;
    AModelPosition.LogPosition := AParagraph.LogPosition;
    AModelPosition.RunStartLogPosition := AModelPosition.LogPosition;
  end;
  Result := AModelPosition;
end;

class function TdxRichEditView.EnsurePositionVisibleWhenHiddenTextNotShown(ADocumentModel: TdxDocumentModel;
  const AModelPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  APieceTable: TdxPieceTable;
  AVisibleTextFilter: TdxVisibleTextFilterBase;
  AParagraphIndex: TdxParagraphIndex;
begin
  APieceTable := TdxPieceTable(AModelPosition.PieceTable);
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

function TdxRichEditView.EnsureTopLevelParagraph(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  I: Integer;
  AParagraphs: TdxParagraphCollection;
  AParagraphIndex: TdxParagraphIndex;
  ACell: TdxTableCell;
begin
  AParagraphs := TdxParagraphCollection(APos.PieceTable.Paragraphs);
  AParagraphIndex := APos.ParagraphIndex;
  ACell := AParagraphs[AParagraphIndex].GetCell;
  if ACell = nil then
  begin
    if AParagraphIndex <> 0 then
    begin
      for I := AParagraphIndex - 1 downto 0 do
        if AParagraphs[I].FrameProperties <> nil then
          AParagraphIndex := I
        else
          Break;
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

function TdxRichEditView.ExpandClipBoundsToPaddings(const AClipBounds: TRect): TRect;
var
  APadding: TRect;
  ALeftPadding, ATopPadding: Integer;
begin
  APadding := ActualPadding;
  ALeftPadding := MulDiv(3, DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(APadding.Left), 4);
  ATopPadding := MulDiv(3, DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(APadding.Top), 4);
  Result := AClipBounds;
  Result.Height := Result.Height + ATopPadding;
  Result.Y := Result.Y - ATopPadding;
  Result.Width := Result.Width + ALeftPadding;
  Result.X := Result.X - ALeftPadding;
end;

procedure TdxRichEditView.GeneratePages;
var
  I: Integer;
  APage: TdxPage;
  APages: TdxPageCollection;
  AFrom, ATo: TdxDocumentModelPosition;
  ALastProcessedPage, AFirstProcessedPage: TdxPageViewInfo;
begin
  Formatter.CheckExecutedAtUIThread;

  PageViewInfos.Clear;

  APages := FormattingController.PageController.Pages;
  for I := 0 to APages.Count - 1 do
  begin
    APage := APages[I];
    if APage.PrimaryFormattingComplete then
      PageViewInfoGenerationComplete := PageViewInfoGenerator.ProcessPage(APage, I) = TdxProcessPageResult.VisiblePagesGenerationComplete;
  end;
  if PageViewInfos.Last <> nil then
  begin
    PageViewInfoGenerator.CalculateWidthParameters;
    UpdateVerticalScrollBar;
    UpdateHorizontalScrollBar;
    ALastProcessedPage := PageViewInfos.Last;
    if ALastProcessedPage <> nil then
    begin
      AFirstProcessedPage := PageViewInfos.First;
      Assert(AFirstProcessedPage.Page.PrimaryFormattingComplete);
      Assert(ALastProcessedPage.Page.PrimaryFormattingComplete);
      AFrom := AFirstProcessedPage.Page.GetFirstPosition(DocumentModel.MainPieceTable);
      ATo := ALastProcessedPage.Page.GetLastPosition(DocumentModel.MainPieceTable);
      Formatter.UpdateSecondaryPositions(AFrom, ATo);
      Formatter.NotifyDocumentChanged(AFrom, ATo, TdxDocumentLayoutResetType.SecondaryLayout);

      if (FOldFirstPageIndex <> PageViewInfos.First.Index) or (FOldLastPageIndex <> PageViewInfos.Last.Index) then
        Control.InnerControl.RaiseVisiblePagesChanged;
    end;
  end;
  FOldFirstPageIndex := -1;
  FOldLastPageIndex := -1;
end;

procedure TdxRichEditView.RaiseZoomChanging;
begin
  if not FOnZoomChanging.Empty then
    FOnZoomChanging.Invoke(Self, nil);
end;

procedure TdxRichEditView.RaiseZoomChanged;
begin
  if not FOnZoomChanged.Empty then
    FOnZoomChanged.Invoke(Self, nil);
end;

procedure TdxRichEditView.RaiseBackColorChanged;
begin
  if not FOnBackColorChanged.Empty then
    FOnBackColorChanged.Invoke(Self, nil);
end;

function TdxRichEditView.GetCursorBounds: TRect;
var
  ACaretBounds: TRect;
begin
  if DocumentModel.Selection.Length > 0 then
    Exit(cxNullRect);
  if not CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character) then
    Exit(cxNullRect);
  ACaretBounds := CaretPosition.CalculateCaretBounds;
  Result := GetPhysicalBounds(CaretPosition.PageViewInfo, ACaretBounds);
end;

function TdxRichEditView.GetHyperlinkField(AHitTestResult: TdxRichEditHitTestResult): TdxField;
begin
  if (AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Box) or
      ((AHitTestResult.Accuracy and ExactBox) = 0) then
    Result := nil
  else
    Result := AHitTestResult.PieceTable.GetHyperlinkField(AHitTestResult.Box.StartPos.RunIndex);
end;


function TdxRichEditView.GetPageViewInfoFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfo;
var
  APageViewInfoRow: TdxPageViewInfoRow;
begin
  Result := nil;
  APageViewInfoRow := GetPageViewInfoRowFromPoint(APoint, True);
  if APageViewInfoRow <> nil then
    Result := APageViewInfoRow.GetPageAtPoint(APoint, AStrictSearch);
end;

function TdxRichEditView.GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
begin
  Result := PageViewInfoGenerator.GetPageRowAtPoint(APoint, AStrictSearch);
end;

function TdxRichEditView.GetPhysicalBounds(APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TRect;

  function GetRulerSize(const ARuler: IdxRulerControl): Integer;
  begin
    if (ARuler <> nil) and ARuler.IsVisible then
      Result := ARuler.GetRulerSizeInPixels
    else
      Result := 0;
  end;

var
  AViewBounds: TRect;
  APosition, AScreenPosition: TPoint;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  X, Y, AScreenWidth, AScreenHeight, AHorizontalRulerHeight, AVerticalRulerWidth: Integer;
begin
  AViewBounds := CreateLogicalRectangle(APageViewInfo, Bounds);
  X := Trunc((ABounds.Location.X - AViewBounds.Location.X) * ScaleFactor);
  Y := Trunc((ABounds.Location.Y - AViewBounds.Location.Y) * ScaleFactor);
  APosition.Init(X, Y);
  ABounds.Width := Trunc(ABounds.Width * ScaleFactor);
  ABounds.Height := Trunc(ABounds.Height * ScaleFactor);
  AUnitConverter := DocumentLayout.UnitConverter;
  AScreenPosition := AUnitConverter.LayoutUnitsToPixels(APosition, DocumentModel.DpiX, DocumentModel.DpiY);
  AScreenWidth := AUnitConverter.LayoutUnitsToPixels(ABounds.Width, DocumentModel.DpiX);
  AScreenHeight := AUnitConverter.LayoutUnitsToPixels(ABounds.Height, DocumentModel.DpiY);
  AHorizontalRulerHeight := GetRulerSize(Control.InnerControl.HorizontalRuler);
  AVerticalRulerWidth := GetRulerSize(Control.InnerControl.VerticalRuler);
  Result.InitSize(
    AScreenPosition.X + ActualPadding.Left + AVerticalRulerWidth,
    AScreenPosition.Y + ActualPadding.Top + AHorizontalRulerHeight,
    AScreenWidth, AScreenHeight);
end;

procedure TdxRichEditView.ActivateScrollBars;
begin
  VerticalScrollController.Activate;
  HorizontalScrollController.Activate;
end;

procedure TdxRichEditView.DeactivateScrollBars;
begin
  VerticalScrollController.Deactivate;
  HorizontalScrollController.Deactivate;
end;

procedure TdxRichEditView.EnsureVerticalScrollBarSynchronized;
begin
  VerticalScrollController.ScrollBarAdapter.EnsureSynchronized;
end;

function TdxRichEditView.GetPhysicalLeftInvisibleWidth: Integer;
begin
  Result := HorizontalScrollController.GetPhysicalLeftInvisibleWidth;
end;

function TdxRichEditView.HasSelection: Boolean;
begin
  Result := True;
end;

function TdxRichEditView.GetVisibleSelectionEnd: TdxDocumentLogPosition;
var
  AEndVisible: Boolean;
  ASelection: TdxSelection;
  AEndRunIndex: TdxRunIndex;
  AEnd, ALastSelectedSymbol: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  AEnd := ASelection.&End;
  if ASelection.Length > 0 then
    ALastSelectedSymbol := Max(AEnd - 1, ASelection.PieceTable.DocumentStartLogPosition)
  else
    ALastSelectedSymbol :=  ASelection.Start;
  AEndRunIndex := ASelection.Interval.&End.RunIndex;
  if (ASelection.Interval.&End.RunOffset = 0) and (AEndRunIndex > 0) then
    Dec(AEndRunIndex);
  AEndVisible := ASelection.PieceTable.VisibleTextFilter.IsRunVisible(AEndRunIndex);
  if AEndVisible then
    Result := AEnd
  else
    Result := ASelection.PieceTable.VisibleTextFilter.GetNextVisibleLogPosition(ALastSelectedSymbol, False);
end;

function TdxRichEditView.GetVisibleSelectionPosition: TdxDocumentLogPosition;
var
  ARunIndex: TdxRunIndex;
  ASelection: TdxSelection;
  APosition: TdxDocumentModelPosition;
  AVisibleTextFilter: TdxVisibleTextFilterBase;
  AStartRunVisible, APositionVisible: Boolean;
begin
  ASelection := DocumentModel.Selection;
  APosition := ASelection.Interval.Start;
  ARunIndex := APosition.RunIndex;
  AVisibleTextFilter := ASelection.PieceTable.VisibleTextFilter;
  AStartRunVisible := AVisibleTextFilter.IsRunVisible(ARunIndex);
  APositionVisible := (APosition.RunOffset = 0) and (ARunIndex > 0) and
    AVisibleTextFilter.IsRunVisible(ARunIndex - 1);
  if AStartRunVisible or APositionVisible then
    Result := APosition.LogPosition
  else
    Result := AVisibleTextFilter.GetNextVisibleLogPosition(APosition, False);
end;

function TdxRichEditView.GetVisibleSelectionStart: TdxDocumentLogPosition;
var
  AStartVisible: Boolean;
  ASelection: TdxSelection;
  AStart: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  AStart := ASelection.Start;
  AStartVisible := ASelection.PieceTable.VisibleTextFilter.IsRunVisible(ASelection.Interval.Start.RunIndex);
  if AStartVisible then
    Result := AStart
  else
    Result := ASelection.PieceTable.VisibleTextFilter.GetNextVisibleLogPosition(AStart, False);
end;

procedure TdxRichEditView.HideCaret;
begin
  Control.HideCaret;
end;

procedure TdxRichEditView.HitTest(APage: TdxPage; const ARequest: TdxRichEditHitTestRequest;
  AResult: TdxRichEditHitTestResult);
var
  ACalculator, ARowCalculator, ABoxCalculator, ADetailRowCalculator: TdxBoxHitTestCalculator;
  ARowRequest, ABoxRequest: TdxRichEditHitTestRequest;
  ABoxResult: TdxRichEditHitTestResult;
  ADetailRow: TdxDetailRow;
  AStrictHitTest: Boolean;
begin
  ACalculator := CreateHitTestCalculator(ARequest, AResult);
  try
    if ARequest.DetailsLevel <> TdxDocumentLayoutDetailsLevel.Character then
      ACalculator.CalcHitTest(APage)
    else
    begin
      ARowRequest := ARequest.Clone;
      ARowRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Row;

      ARowCalculator := CreateHitTestCalculator(ARowRequest, AResult);
      try
        ARowCalculator.CalcHitTest(APage);
        if not AResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
          Exit;

        ABoxResult := TdxRichEditHitTestResult.Create(DocumentLayout, ARequest.PieceTable);
        try
          ABoxResult.CopyFrom(AResult);
          ABoxRequest := ARequest.Clone;
          ABoxRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Box;

          ABoxCalculator := CreateHitTestCalculator(ABoxRequest, ABoxResult);
          try
            ABoxCalculator.CalcHitTest(ABoxResult.Row);
            if not ABoxResult.IsValid(TdxDocumentLayoutDetailsLevel.Box) then
              Exit;
            AResult.CopyFrom(ABoxResult);
            ADetailRow := DocumentLayout.CreateDetailRow(AResult.Row);
            try
              AStrictHitTest := (ARequest.Accuracy and ExactCharacter) <> 0;
              ADetailRowCalculator := CreateHitTestCalculator(ARequest, AResult);
              try
                ADetailRowCalculator.FastHitTestCharacter(ADetailRow.Characters, AStrictHitTest);
              finally
                ADetailRowCalculator.Free;
              end;
            finally
              ADetailRow.Free;
            end;
          finally
            ABoxCalculator.Free;
          end;
        finally
          ABoxResult.Free;
        end;
      finally
        ARowCalculator.Free;
      end;
    end;
  finally
    ACalculator.Free;
  end;
end;

function TdxRichEditView.HitTestByPhysicalPoint(const APt: TPoint; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TRect;
var
  ARow: TdxRow;
  APageViewInfo: TdxPageViewInfo;
  AHitTest: TdxRichEditHitTestResult;
begin
  AHitTest := CalculateHitTest(APt, ADetailsLevel);
  try
    if AHitTest = nil then
      Exit(TRect.Null);
    ARow := AHitTest.Row;
    APageViewInfo := LookupPageViewInfoByPage(AHitTest.Page);
    Result := GetPhysicalBounds(APageViewInfo, ARow.Bounds);
  finally
    AHitTest.Free;
  end;
end;

function TdxRichEditView.HitTestCore(var ARequest: TdxRichEditHitTestRequest;
  AStrictHitIntoPageBounds: Boolean): TdxRichEditHitTestResult;
var
  APageViewInfo: TdxPageViewInfo;
  P: TPoint;
  APieceTable: TdxPieceTable;
begin
  APieceTable := ARequest.PieceTable;
  Result := TdxRichEditHitTestResult.Create(DocumentLayout, APieceTable);
  Result.PhysicalPoint := ARequest.PhysicalPoint;
  APageViewInfo := GetPageViewInfoFromPoint(ARequest.PhysicalPoint, AStrictHitIntoPageBounds);
  if APageViewInfo = nil then
    Exit;

  P := ARequest.PhysicalPoint;
  P.X := P.X + GetPhysicalLeftInvisibleWidth;
  if AStrictHitIntoPageBounds and not PerformStrictPageViewInfoHitTest(APageViewInfo, P) then
    Exit;
  Result.Page := APageViewInfo.Page;
  Result.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);
  if ARequest.DetailsLevel <= TdxDocumentLayoutDetailsLevel.Page then
  begin
    Result.LogicalPoint := CreateLogicalPoint(APageViewInfo.ClientBounds, ARequest.PhysicalPoint);
    Result.FloatingObjectBox := CalculateFloatingObjectHitTest(Result.Page, Result.LogicalPoint, IsActivePieceTableFloatingObjectBox);
    Result.FloatingObjectBoxPage := Result.Page;
    Exit;
  end;

  ARequest.LogicalPoint := CreateLogicalPoint(APageViewInfo.ClientBounds, ARequest.PhysicalPoint);
  if APieceTable.IsTextBox then
    DocumentModelActivePieceTableTextBoxHitTest(ARequest, Result, APageViewInfo)
  else
  begin
    if APieceTable.IsComment then
    else
    begin
      Result.FloatingObjectBox := CalculateFloatingObjectHitTest(Result.Page, ARequest.LogicalPoint, IsActivePieceTableFloatingObjectBox);
      HitTest(APageViewInfo.Page, ARequest, Result);
    end;
  end;
end;

function TdxRichEditView.IsActiveFloatingObjectTextBox(const ABox: TdxFloatingObjectBox): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxTextBoxFloatingObjectContent;
begin
  ARun := ABox.GetFloatingObjectRun;
  AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  Result := (ABox.DocumentLayout <> nil) and (AContent <> nil) and
    (AContent.TextBox.PieceTable = DocumentModel.ActivePieceTable);
end;

function TdxRichEditView.IsActivePieceTableFloatingObjectBox(const ABox: TdxFloatingObjectBox): Boolean;
begin
  Result := ABox.PieceTable = DocumentModel.ActivePieceTable;
end;

function TdxRichEditView.IsAllowDisplayLineNumbersStored: Boolean;
begin
  Result := FAllowDisplayLineNumbers <> GetDefaultAllowDisplayLineNumbers;
end;

function TdxRichEditView.IsBackColorStored: Boolean;
begin
  Result := FBackColor <> TdxAlphaColors.Window;
end;

function TdxRichEditView.IsZoomFactorStored: Boolean;
const
  DefaultValue: Single = DefaultZoomFactor;
begin
  Result := not SameValue(DefaultValue, ZoomFactor);
end;

function TdxRichEditView.IsFloatingObjectHit(ABox: TdxFloatingObjectBox; APoint: TPoint): Boolean;
begin
  Result := ABox.Bounds.Contains(ABox.TransformPointBackward(APoint));
end;

procedure TdxRichEditView.DocumentModelActivePieceTableTextBoxHitTest(var ARequest: TdxRichEditHitTestRequest; var AResult: TdxRichEditHitTestResult; APageViewInfo: TdxPageViewInfo);
var
  ABox: TdxFloatingObjectBox;
  APage: TdxPage;
  ACopy: TdxRichEditHitTestResult;
begin
  ABox := CalculateFloatingObjectHitTest(AResult.Page, ARequest.LogicalPoint, IsActiveFloatingObjectTextBox);
  if ABox <> nil then
  begin
    APage := ABox.DocumentLayout.Pages.First;
    ARequest.SearchAnyPieceTable := True;
    AResult.FloatingObjectBox := ABox;
    AResult.FloatingObjectBoxPage := AResult.Page;
    HitTest(APage, ARequest, AResult);
  end
  else
  begin
    HitTest(APageViewInfo.Page, ARequest, AResult);
    if (AResult.PageArea <> nil) and (AResult.PageArea.PieceTable <> AResult.PieceTable) then
    begin
      ACopy := TdxRichEditHitTestResult.Create(DocumentLayout, TdxPieceTable(AResult.PageArea.PieceTable));
      ACopy.CopyFrom(AResult);
      AResult.Free;
      AResult := ACopy;
    end;
  end;
end;

function TdxRichEditView.IsPaddingStored: Boolean;
begin
  Result := not Padding.IsDefault;
end;

function TdxRichEditView.IsPrimaryFormattingCompleteForVisibleHeight2(
  const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean;
begin
  Result := PageViewInfoGenerationComplete;
end;

procedure TdxRichEditView.PaddingChanged(Sender: TObject);
begin
  OnViewPaddingChanged;
end;

function TdxRichEditView.LookupPageViewInfoByPage(APage: TdxPage): TdxPageViewInfo;
var
  APageViewInfos: TdxPageViewInfoCollection;
  I: Integer;
begin
  APageViewInfos := PageViewInfos;
  for I := 0 to APageViewInfos.Count - 1 do
    if APageViewInfos[I].Page = APage then
      Exit(APageViewInfos[I]);
  Result := nil;
end;

procedure TdxRichEditView.OnActivePieceTableChanged;
begin
  if DocumentModel.ActivePieceTable.IsTextBox then
  begin
    SelectionLayout := TdxTextBoxSelectionLayout.Create(Self, 0);
    CaretPosition := TdxTextBoxCaretPosition.Create(Self, 0);
  end
  else
  begin
    SelectionLayout := TdxSelectionLayout.Create(Self, 0);
    CaretPosition := TdxCaretPosition.Create(Self, 0);
  end;
end;

procedure TdxRichEditView.AfterCreateDetailRowHandler(ASender: TObject);
begin
  ResumeFormatting;
end;

procedure TdxRichEditView.BeforeCreateDetailRowHandler(ASender: TObject);
begin
  SuspendFormatting;
end;

procedure TdxRichEditView.OnBeginDocumentUpdate;
begin
  SuspendFormatting;
end;

procedure TdxRichEditView.OnEndDocumentUpdate;
begin
  ResumeFormatting;
end;

procedure TdxRichEditView.OnHorizontalScroll;
begin
  PageViewInfoGenerator.LeftInvisibleWidth := HorizontalScrollController.GetLeftInvisibleWidth;
  Control.RedrawEnsureSecondaryFormattingComplete(TdxRefreshAction.Transforms);
end;

procedure TdxRichEditView.OnLayoutUnitChanged;
var
  APieceTable: TdxPieceTable;
  AFrom, ATo: TdxDocumentModelPosition;
begin
  CaretPosition.Invalidate;
  CaretPosition.InvalidateInputPosition;
  PageViewInfoGenerator.OnLayoutUnitChanged(DocumentModel.LayoutUnitConverter);
  FormattingController.Reset(False);
  APieceTable := DocumentModel.MainPieceTable;
  AFrom := TdxDocumentModelPosition.Create(APieceTable);
  ATo := TdxDocumentModelPosition.FromParagraphEnd(APieceTable, APieceTable.Paragraphs.Last.Index);
  Formatter.UpdateSecondaryPositions(AFrom, ATo);
  Formatter.NotifyDocumentChanged(AFrom, ATo, TdxDocumentLayoutResetType.AllPrimaryLayout);
end;

procedure TdxRichEditView.OnLayoutUnitChanging;
begin
  ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
  PageViewInfoGenerator.OnLayoutUnitChanging(DocumentModel.LayoutUnitConverter);
end;

procedure TdxRichEditView.OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
var
  ALastPage: TdxPage;
  ALastPageViewInfo: TdxPageViewInfo;
  APrevPageViewInfoGenerationComplete: Boolean;
begin
  if Control.IsDestroying then
    Exit;

  ALastPage := E.Page;
  Assert(ALastPage = FormattingController.PageController.Pages.Last);
  Assert(ALastPage.PrimaryFormattingComplete);
  ALastPageViewInfo := PageViewInfos.Last;
  if (ALastPageViewInfo <> nil) and (ALastPageViewInfo.Page = ALastPage) then
  begin
    if E.DocumentFormattingComplete then
      Control.UpdateUIFromBackgroundThread(EnsureVerticalScrollBarSynchronized);
    Exit;
  end;
  APrevPageViewInfoGenerationComplete := PageViewInfoGenerationComplete;
  PageViewInfoGenerationComplete := PageViewInfoGenerator.ProcessPage(ALastPage, FormattingController.PageController.Pages.Count - 1) = TdxProcessPageResult.VisiblePagesGenerationComplete;
  if (PageViewInfoGenerationComplete <> APrevPageViewInfoGenerationComplete) or E.DocumentFormattingComplete then
  begin
    PageViewInfoGenerator.CalculateWidthParameters;
    Control.UpdateUIFromBackgroundThread(UpdateHorizontalScrollBar);
  end;
  if PageViewInfoGenerationComplete then
  begin
    Formatter.UpdateSecondaryPositions(Formatter.SecondaryLayoutStart, ALastPage.GetLastPosition(DocumentModel.MainPieceTable));
    Formatter.ResetSecondaryLayout;
  end;
  Control.UpdateUIFromBackgroundThread(UpdateVerticalScrollBar);
  if E.DocumentFormattingComplete then
    Control.UpdateUIFromBackgroundThread(EnsureVerticalScrollBarSynchronized);
end;

procedure TdxRichEditView.OnPageFormattingStarted(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
var
  ALastPage: TdxPage;
begin
  ALastPage := E.Page;
  Assert(ALastPage = FormattingController.PageController.Pages.Last);
  if not PageViewInfoGenerationIsAboutToComplete then
    PageViewInfoGenerationIsAboutToComplete := (PageViewInfoGenerator.PreProcessPage(ALastPage, FormattingController.PageController.Pages.Count - 1) = TdxProcessPageResult.VisiblePagesGenerationComplete);
end;

procedure TdxRichEditView.OnResetSecondaryFormattingForPage(ASender: TObject;
  E: TdxResetSecondaryFormattingForPageArgs);
begin
  Formatter.ResetSecondaryFormattingForPage(E.Page, E.PageIndex);
end;

procedure TdxRichEditView.OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize: Boolean);
begin
  OnResize(ABounds, AEnsureCaretVisibleOnResize, True);
end;

procedure TdxRichEditView.OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean);
begin
  PerformResize(ABounds);
  Control.InnerControl.BeginDocumentRendering;
  Control.InnerControl.EndDocumentRendering;
  if AEnsureOptimalHorizontalScrollbarPosition then
    SetOptimalHorizontalScrollbarPosition;
end;

procedure TdxRichEditView.OnResizeCore;
begin
end;

procedure TdxRichEditView.OnSelectionChanged;
begin
  HideCaret;
  CaretPosition.Invalidate;
  CaretPosition.InvalidateInputPosition;
  SelectionLayout.Invalidate;
  ShowCaret;
end;

procedure TdxRichEditView.ProcessSelectionChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  CaretPosition.Invalidate;
  if TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting in AChangeActions then
    CaretPosition.InvalidateInputPosition;

  if TdxDocumentModelChangeAction.ValidateSelectionInterval in AChangeActions then
    ValidateSelectionInterval;
end;

procedure TdxRichEditView.OnVerticalScroll;
begin
  SuspendFormatting;
  try
    ResetPages(TdxPageGenerationStrategyType.RunningHeight);
    PageViewInfoGenerator.TopInvisibleHeight := VerticalScrollController.GetTopInvisibleHeight;
    GeneratePages;
  finally
    ResumeFormatting;
  end;
  Control.RedrawEnsureSecondaryFormattingComplete(TdxRefreshAction.Transforms);
end;

procedure TdxRichEditView.OnViewPaddingChanged;
begin
  Control.OnViewPaddingChanged;
end;

procedure TdxRichEditView.OnZoomFactorChanged(AOldValue, ANewValue: Single);
begin
  PerformZoomFactorChanged;
  RaiseZoomChanged;
  Control.RedrawEnsureSecondaryFormattingComplete(TdxRefreshAction.Zoom);
  Control.InnerControl.RaiseUpdateUI;
end;

procedure TdxRichEditView.OnZoomFactorChangedCore;
begin
end;

procedure TdxRichEditView.OnZoomFactorChanging;
begin
  RaiseZoomChanging;
end;

procedure TdxRichEditView.PerformResize(const ABounds: TRect);
begin
  DocumentModel.BeginUpdate;
  try
    Bounds := ABounds;
    ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
    OnResizeCore;
    Assert(PageViewInfoGenerator.ActiveGenerator.Pages.Count <= 0);
    GeneratePages;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxRichEditView.PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean;
begin
  Result := cxRectPtIn(APageViewInfo.ClientBounds, APt);
end;

procedure TdxRichEditView.PerformZoomFactorChanged;
begin
  SuspendFormatting;
  try
    ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
    OnZoomFactorChangedCore;
    GeneratePages;
  finally
    ResumeFormatting;
  end;
  Control.InnerControl.BeginDocumentRendering;
  Control.InnerControl.EndDocumentRendering;
  SetOptimalHorizontalScrollbarPosition;
end;

procedure TdxRichEditView.ResetPages(AStrategy: TdxPageGenerationStrategyType);
begin
  Formatter.CheckExecutedAtUIThread;

  if (PageViewInfos.First <> nil) and (PageViewInfos.Last <> nil) then
  begin
    FOldFirstPageIndex := PageViewInfos.First.Index;
    FOldLastPageIndex := PageViewInfos.Last.Index;
  end;

  CaretPosition.InvalidatePageViewInfo;

  ViewInfo.Clear;
  PageViewInfoGenerator.ActiveGenerator.PageRows.Clear;
  PageViewInfos.Clear;

  PageViewInfoGenerationIsAboutToComplete := False;
  PageViewInfoGenerationComplete := False;
  PageViewInfoGenerator.Reset(AStrategy);

end;

procedure TdxRichEditView.ScrollToPage(APageIndex: Integer);
begin
  SuspendFormatting;
  try
    ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
    PageViewInfoGenerator.FirstPageOffsetAnchor.PageIndex := APageIndex;
    PageViewInfoGenerator.FirstPageOffsetAnchor.VerticalOffset := 0;
    GeneratePages;
    ResetPages(TdxPageGenerationStrategyType.RunningHeight);
    GeneratePages;
  finally
    ResumeFormatting;
  end;
  UpdateVerticalScrollbar;
  Control.RedrawEnsureSecondaryFormattingComplete(TdxRefreshAction.Transforms);
end;

procedure TdxRichEditView.SetAllowDisplayLineNumbers(const Value: Boolean);
begin
  if FAllowDisplayLineNumbers <> Value then
  begin
    FAllowDisplayLineNumbers := Value;
    Control.Redraw(TdxRefreshAction.AllDocument);
  end;
end;

procedure TdxRichEditView.ApplyChanges(APieceTable: TdxPieceTable);
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  DocumentModel.BeginUpdate;
  try
    AChangeActions := [TdxDocumentModelChangeAction.ResetAllPrimaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
      TdxDocumentModelChangeAction.ForceResetVerticalRuler];
    APieceTable.ApplyChangesCore(AChangeActions, 0, MaxInt);
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxRichEditView.CreatePadding: TdxRichEditControlPadding;
begin
  Result := TdxRichEditControlPadding.Create(Self, 0);
end;

function TdxRichEditView.GetActualPadding: TRect;
begin
  if FPadding <> nil then
    Result := Control.ScaleFactor.Apply(FPadding.Value)
  else
    Result.Empty;
end;

procedure TdxRichEditView.ResetPadding;
begin
  if FPadding <> nil then
    FPadding.Reset;
end;

function TdxRichEditView.GetDefaultAllowDisplayLineNumbers: Boolean;
begin
  Result := False;
end;

function TdxRichEditView.GetDocumentModel: TdxDocumentModel;
begin
  Result := FControl.InnerControl.DocumentModel;
end;

function TdxRichEditView.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := Control.LookAndFeel;
end;

function TdxRichEditView.GetScaleFactor: Single;
var
  AScaleFactor: TdxScaleFactor;
begin
  AScaleFactor := FControl.ScaleFactor;
  Result := AScaleFactor.ApplyF(ZoomFactor);
end;

function TdxRichEditView.GetFixedLeftTextBorderOffset: Integer;
begin
  Result := 0;
end;

function TdxRichEditView.GetFormatter: TdxBackgroundFormatter;
begin
  Result := FControl.InnerControl.Formatter;
end;

procedure TdxRichEditView.SetBackColor(const Value: TdxAlphaColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    RaiseBackColorChanged;
  end;
end;

procedure TdxRichEditView.SetBounds(const Value: TRect);
begin
  if cxRectIsEqual(FBounds, Value) then
    Exit;
  if Value.Width < MinWidth then
    Value.Width := MinWidth;
  if Value.Height < MinHeight then
    Value.Height := MinHeight;
  FBounds := Value;
end;

procedure TdxRichEditView.SetCaretPosition(const Value: TdxCaretPosition);
begin
  if FCaretPosition <> Value then
  begin
    FreeAndNil(FCaretPosition);
    FCaretPosition := Value;
  end;
end;

procedure TdxRichEditView.SetOptimalHorizontalScrollbarPosition;
begin
  DocumentModel.BeginUpdate;
  try
    SetOptimalHorizontalScrollbarPositionCore;
    if DocumentModel.IsUpdateLocked then
      DocumentModel.MainPieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.Redraw], dxRunIndexDontCare,
        dxRunIndexDontCare);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxRichEditView.SetOptimalHorizontalScrollbarPositionCore;
var
  APageViewInfo: TdxPageViewInfo;
begin
  if PageViewInfoGenerator.VisibleWidth >= PageViewInfoGenerator.TotalWidth then
    PageViewInfoGenerator.LeftInvisibleWidth := 0
  else
  begin
    APageViewInfo := ChoosePageViewInfoForOptimalHorizontalScrollbarPosition;
    if APageViewInfo = nil then
      PageViewInfoGenerator.LeftInvisibleWidth := 0
    else
      PageViewInfoGenerator.LeftInvisibleWidth := Max(0, CalculateOptimalHorizontalScrollbarPosition(APageViewInfo));
  end;
  HorizontalScrollController.UpdateScrollBar;
end;

procedure TdxRichEditView.SetPageViewInfoGenerationComplete(const Value: Boolean);
begin
  FPageViewInfoGenerationComplete := Value;
end;

procedure TdxRichEditView.SetSelectionLayout(const Value: TdxSelectionLayout);
begin
  if FSelectionLayout <> Value then
  begin
    FreeAndNil(FSelectionLayout);
    FSelectionLayout := Value;
  end;
end;

function TdxRichEditView.GetPageViewInfos: TdxPageViewInfoCollection;
begin
  Result := ViewInfo.PageViewInfos;
end;

procedure TdxRichEditView.SetPadding(Value: TdxRichEditControlPadding);
begin
  FPadding.Assign(Value);
end;

function TdxRichEditView.GetTextColors: TdxTextColors;
begin
  Result := TdxTextColors.Defaults;
end;

class function TdxRichEditView.SetNewPosition(const APos: TdxDocumentModelPosition;
  AParagraph: TdxParagraph): TdxDocumentModelPosition;
begin
  Result := APos;
  Result.RunIndex := AParagraph.FirstRunIndex;
  Result.ParagraphIndex := AParagraph.Index;
  Result.LogPosition := AParagraph.LogPosition;
  Result.RunStartLogPosition := AParagraph.LogPosition;
end;

procedure TdxRichEditView.SetZoomFactor(Value: Single);
var
  AOldValue: Single;
begin
  if Value < MinZoomFactor then
    Value := MinZoomFactor;
  if Value > MaxZoomFactor then
    Value := MaxZoomFactor;
  if FZoomFactor = Value then
    Exit;
  OnZoomFactorChanging;
  AOldValue := FZoomFactor;
  FZoomFactor := Value;
  OnZoomFactorChanged(AOldValue, FZoomFactor);
end;

procedure TdxRichEditView.ShowCaret;
begin
  Control.ShowCaret;
end;

procedure TdxRichEditView.SubscribeDocumentLayoutEvents;
begin
  DocumentLayout.OnBeforeCreateDetailRow.Add(BeforeCreateDetailRowHandler);
  DocumentLayout.OnAfterCreateDetailRow.Add(AfterCreateDetailRowHandler);
end;

procedure TdxRichEditView.SubscribePageFormattingComplete;
begin
  FormattingController.PageFormattingStarted.Add(OnPageFormattingStarted);
  FormattingController.PageFormattingComplete.Add(OnPageFormattingComplete);
end;

procedure TdxRichEditView.SuspendFormatting;
begin
  if FSuspendFormattingCount = 0 then
  begin
    Formatter.BeginDocumentUpdate;
  end;
  Inc(FSuspendFormattingCount);
end;

procedure TdxRichEditView.ResumeFormatting;
begin
  Dec(FSuspendFormattingCount);
  if FSuspendFormattingCount = 0 then
  begin
    Formatter.EndDocumentUpdate;
  end;
end;

procedure TdxRichEditView.UnsubscribeDocumentLayoutEvents;
begin
  DocumentLayout.OnBeforeCreateDetailRow.Remove(BeforeCreateDetailRowHandler);
  DocumentLayout.OnAfterCreateDetailRow.Remove(AfterCreateDetailRowHandler);
end;

procedure TdxRichEditView.UnsubscribePageFormattingComplete;
begin
  FormattingController.PageFormattingStarted.Remove(OnPageFormattingStarted);
  FormattingController.PageFormattingComplete.Remove(OnPageFormattingComplete);
end;

procedure TdxRichEditView.UpdateCaretPosition;
begin
  CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character);
end;

procedure TdxRichEditView.UpdateHorizontalScrollBar;
begin
  HorizontalScrollController.UpdateScrollBar;
end;

procedure TdxRichEditView.UpdateVerticalScrollBar;
begin
  VerticalScrollController.UpdateScrollBar;
end;

procedure TdxRichEditView.ValidateSelectionInterval;
var
  ASelection: TdxSelection;
  AStart, AEnd, AVisibleStart, AVisibleEnd, ANewPosition: TdxDocumentLogPosition;
  ANonEmptyVisibleSelection: Boolean;
begin
  ASelection := DocumentModel.Selection;
  AStart := ASelection.Start;
  AEnd := ASelection.&End;
  if ASelection.Length > 0 then
  begin
    AVisibleStart := GetVisibleSelectionStart;
    AVisibleEnd := GetVisibleSelectionEnd;
    ANonEmptyVisibleSelection := (AStart < AEnd) and (AVisibleStart < AVisibleEnd) or
      (AStart > AEnd) and (AVisibleStart > AVisibleEnd);
    if not ANonEmptyVisibleSelection then
      AVisibleEnd := AVisibleStart;
    DocumentModel.Selection.Start := AVisibleStart;
    DocumentModel.Selection.&End := AVisibleEnd;
  end
  else
  begin
    ANewPosition := GetVisibleSelectionPosition;
    DocumentModel.Selection.Start := ANewPosition;
    DocumentModel.Selection.&End := ANewPosition;
  end;
end;

{ TdxPageBasedRichEditView }

function TdxPageBasedRichEditView.GetCurrentPageIndex: Integer;
var
  APosition: TdxDocumentLayoutPosition;
begin
  if CaretPosition = nil then
    Exit(-1);
  APosition := TdxDocumentLayoutPosition(CaretPosition.LayoutPosition.Clone);
  try
    if not APosition.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
      APosition.Update(FormattingController.PageController.Pages, TdxDocumentLayoutDetailsLevel.Page);
    if APosition.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
      Result := APosition.Page.PageIndex
    else
    begin
      if FormattingController.PageController.Pages.Count = 1 then
        Result := 0
      else
        Result := -1;
    end;
  finally
    FreeAndNil(APosition);
  end;
end;

function TdxPageBasedRichEditView.GetPageCount: Integer;
begin
  if FormattingController <> nil then
    Result := FormattingController.PageCount
  else
    Result := 0;
end;

procedure TdxPageBasedRichEditView.RaisePageCountChanged;
begin
  if not FOnPageCountChanged.Empty then
    FOnPageCountChanged.Invoke(Self, nil);
end;

procedure TdxPageBasedRichEditView.OnPageCountChanged(ASender: TObject; E: TdxEventArgs);
begin
  Control.UpdateUIFromBackgroundThread(RaisePageCountChanged);
end;

procedure TdxPageBasedRichEditView.Activate(const AViewBounds: TRect);
begin
  inherited Activate(AViewBounds);
  FormattingController.PageCountChanged.Add(OnPageCountChanged);
end;

procedure TdxPageBasedRichEditView.Deactivate;
begin
  FormattingController.PageCountChanged.Remove(OnPageCountChanged);
  inherited Deactivate;
end;

function TdxPageBasedRichEditView.CalcBestSize(AFixedWidth: Boolean): TSize;
begin
  Result := cxNullSize;
end;

procedure TdxPageBasedRichEditView.OnAutoSizeModeChanged;
begin
end;

{ TdxRichEditCustomViewRepository }

constructor TdxRichEditCustomViewRepository.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl.Control);
  FRichEditControl := ARichEditControl;
  FViews := TdxFastObjectList.Create;
  CreateViews;
end;

destructor TdxRichEditCustomViewRepository.Destroy;
begin
  FreeAndNil(FViews);
  inherited Destroy;
end;

procedure TdxRichEditCustomViewRepository.AddView(AView: TdxRichEditView);
begin
  FViews.Add(AView);
end;

function TdxRichEditCustomViewRepository.GetViewByType(AType: TdxRichEditViewType): TdxRichEditView;
var
  I: Integer;
begin
  for I := ViewCount - 1 downto 0 do
    if Views[I].&Type = AType then
      Exit(Views[I]);
  Result := nil;
end;

function TdxRichEditCustomViewRepository.GetView(Index: Integer): TdxRichEditView;
begin
  Result := TdxRichEditView(FViews[Index]);
end;

function TdxRichEditCustomViewRepository.GetViewCount: Integer;
begin
  Result := FViews.Count;
end;

{ TdxOfficeScrollControllerBase }

constructor TdxOfficeScrollControllerBase.Create(AView: TdxRichEditView);
begin
  inherited Create;
  Assert(AView <> nil, 'view = nil');
  FView := AView;
  Initialize;
end;

destructor TdxOfficeScrollControllerBase.Destroy;
begin
  FreeAndNil(FScrollBarAdapter);
  inherited Destroy;
end;

procedure TdxOfficeScrollControllerBase.Activate;
begin
  ScrollBarAdapter.Activate;
  SubscribeScrollBarAdapterEvents;
end;

procedure TdxOfficeScrollControllerBase.Deactivate;
begin
  ScrollBarAdapter.Deactivate;
  UnsubscribeScrollBarAdapterEvents;
end;

procedure TdxOfficeScrollControllerBase.EmulateScroll(AEventType: TdxScrollEventType);
var
  AArgs: TdxScrollEventArgs;
begin
  AArgs := ScrollBarAdapter.CreateEmulatedScrollEventArgs(AEventType);
  OnScroll(TObject(ScrollBar), AArgs);
end;

function TdxOfficeScrollControllerBase.GetSupportsDeferredUpdate: Boolean;
begin
  Result := False;
end;

procedure TdxOfficeScrollControllerBase.Initialize;
begin
  FScrollBarAdapter := CreateScrollBarAdapter;
  ScrollBarAdapter.RefreshValuesFromScrollBar;
end;

function TdxOfficeScrollControllerBase.IsScrollPossible: Boolean;
begin
  Result := (ScrollBarAdapter.Maximum - ScrollBarAdapter.Minimum > ScrollBarAdapter.LargeChange) and
    (ScrollBarAdapter.Value >= ScrollBarAdapter.Minimum) and
    (ScrollBarAdapter.Value <= ScrollBarAdapter.Maximum - ScrollBarAdapter.LargeChange + 1);
end;

procedure TdxOfficeScrollControllerBase.SubscribeScrollBarAdapterEvents;
begin
  ScrollBarAdapter.Scroll.Add(OnScroll);
end;

procedure TdxOfficeScrollControllerBase.SynchronizeScrollbar;
begin
  UnsubscribeScrollBarAdapterEvents;
  try
    ScrollBarAdapter.ApplyValuesToScrollBar;
  finally
    SubscribeScrollBarAdapterEvents;
  end;
end;

procedure TdxOfficeScrollControllerBase.UnsubscribeScrollBarAdapterEvents;
begin
  ScrollBarAdapter.Scroll.Remove(OnScroll);
end;

procedure TdxOfficeScrollControllerBase.UpdateScrollBar;
begin
  if (ScrollBar = nil) or (View <> View.Control.InnerControl.ActiveView) then
    Exit;
  UpdateScrollBarAdapter;
  SynchronizeScrollbar;
end;

{ TdxRichEditViewHorizontalScrollController }

function TdxRichEditViewHorizontalScrollController.CreateScrollBarAdapter: TdxScrollBarAdapter;
begin
  Result := TdxHorizontalScrollBarAdapter.Create(ScrollBar, View.Control.CreatePlatformSpecificScrollBarAdapter);
end;

function TdxRichEditViewHorizontalScrollController.GetLeftInvisibleWidth: Int64;
begin
  Result := ScrollBarAdapter.Value;
end;

function TdxRichEditViewHorizontalScrollController.GetPhysicalLeftInvisibleWidth: Integer;
begin
  if ScrollBar = nil then
    Exit(0);
  Result := Round(View.PageViewInfoGenerator.LeftInvisibleWidth * View.ScaleFactor);
end;

function TdxRichEditViewHorizontalScrollController.GetScrollBar: IdxOfficeScrollbar;
begin
  Result := nil;
  if (View.Control = nil) or (View.Control.InnerControl = nil) then
    Exit;
  Result := View.Control.InnerControl.HorizontalScrollBar;
end;

procedure TdxRichEditViewHorizontalScrollController.OnScroll(ASender: TObject;
  E: TdxScrollEventArgs);
begin
  OnScrollCore(E);
  View.OnHorizontalScroll;
end;

procedure TdxRichEditViewHorizontalScrollController.OnScrollCore(E: TdxScrollEventArgs);
begin
end;

procedure TdxRichEditViewHorizontalScrollController.ScrollByLeftInvisibleWidthDelta(ALeftInvisibleWidthDelta: Int64);
begin
  ScrollToAbsolutePosition(ScrollBarAdapter.Value + ALeftInvisibleWidthDelta);
end;

procedure TdxRichEditViewHorizontalScrollController.ScrollToAbsolutePosition(AValue: Int64);
begin
  ScrollBarAdapter.BeginUpdate;
  try
    ScrollBarAdapter.Maximum := View.PageViewInfoGenerator.TotalWidth - 1;
    ScrollBarAdapter.Value := AValue;
  finally
    ScrollBarAdapter.EndUpdate;
  end;
  SynchronizeScrollbar;
  ScrollBarAdapter.EnsureSynchronized();
end;

procedure TdxRichEditViewHorizontalScrollController.UpdateScrollBarAdapter;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := View.PageViewInfoGenerator;
  if AGenerator.VisibleWidth < 0 then
    Exit;
  ScrollBarAdapter.BeginUpdate;
  try
    ScrollBarAdapter.Minimum := 0;
    ScrollBarAdapter.Maximum := AGenerator.TotalWidth - 1;
    ScrollBarAdapter.LargeChange := AGenerator.VisibleWidth;
    ScrollBarAdapter.SmallChange := Round(View.DocumentLayout.UnitConverter.DocumentsToLayoutUnits(210));
    ScrollBarAdapter.Value := AGenerator.LeftInvisibleWidth;
    ScrollBarAdapter.Enabled := IsScrollPossible;
  finally
    ScrollBarAdapter.EndUpdate;
  end;
end;

{ TdxRichEditViewVerticalScrollController }


procedure TdxRichEditViewVerticalScrollController.ApplyNewScrollValue(AValue: Integer);
begin
  dxAbstractError;
end;

procedure TdxRichEditViewVerticalScrollController.ApplyNewScrollValueToScrollEventArgs(
  E: TdxScrollEventArgs; AValue: Integer);
begin
  dxAbstractError;
end;

function TdxRichEditViewVerticalScrollController.CalculateScrollDelta(
  E: TdxScrollEventArgs): Integer;
begin
  dxAbstractError;
  Result := 0;
end;

function TdxRichEditViewVerticalScrollController.CreateScrollBarAdapter: TdxScrollBarAdapter;
begin
  Result := TdxVerticalScrollBarAdapter.Create(ScrollBar, View.Control.CreatePlatformSpecificScrollBarAdapter);
end;

function TdxRichEditViewVerticalScrollController.IsScrollTypeValid(E: TdxScrollEventArgs): Boolean;
begin
  dxAbstractError;
  Result := True;
end;

function TdxRichEditViewVerticalScrollController.GetScrollBar: IdxOfficeScrollbar;
begin
  Result := nil;
  if (View.Control = nil) or (View.Control.InnerControl = nil) then
    Exit;
  Result := View.Control.InnerControl.VerticalScrollBar;
end;

function TdxRichEditViewVerticalScrollController.GetTopInvisibleHeight: Int64;
begin
  Result := ScrollBarAdapter.Value;
end;

procedure TdxRichEditViewVerticalScrollController.OnScroll(ASender: TObject; E: TdxScrollEventArgs);
var
  ADelta, AValue: Integer;
begin
  if UpdatePageNumberOnScroll(E) then
    Exit;
  if not IsScrollTypeValid(E) then
    Exit;
  ADelta := CalculateScrollDelta(E);
  AValue := E.NewValue;
  if ADelta <> 0 then
  begin
    if ADelta = 1 then
      AValue := ScrollLineDown
    else
      if ADelta = -1 then
        AValue := ScrollLineUp
      else
        ApplyNewScrollValue(AValue);
    View.OnVerticalScroll;
    ApplyNewScrollValueToScrollEventArgs(E, AValue);
  end;
end;

procedure TdxRichEditViewVerticalScrollController.ScrollByTopInvisibleHeightDelta(ATopInvisibleHeightDelta: Int64);
begin
  ScrollToAbsolutePosition(ScrollBarAdapter.Value + ATopInvisibleHeightDelta);
end;

function TdxRichEditViewVerticalScrollController.ScrollLineDown: Integer;
begin
  Result := ScrollLineUpDown(View.DocumentLayout.UnitConverter.DocumentsToLayoutUnits(50));
end;

function TdxRichEditViewVerticalScrollController.ScrollLineUp: Integer;
begin
  Result := ScrollLineUpDown(-View.DocumentLayout.UnitConverter.DocumentsToLayoutUnits(50));
end;

function TdxRichEditViewVerticalScrollController.ScrollLineUpDown(APhysicalOffset: Integer): Integer;
var
  ACommand: TdxScrollVerticallyByPhysicalOffsetCommand;
begin
  ACommand := TdxScrollVerticallyByPhysicalOffsetCommand.Create(View.Control);
  try
    ACommand.UpdateScrollBarBeforeExecution := False;
    ACommand.PhysicalOffset := APhysicalOffset;
    ACommand.Execute;
    Result := ScrollBarAdapter.GetRawScrollBarValue;
  finally
    ACommand.Free;
  end;
end;

function TdxRichEditViewVerticalScrollController.ScrollPageDown: Boolean;
begin
  Result := ScrollBarAdapter.SetRawScrollBarValue(ScrollBarAdapter.GetPageDownRawScrollBarValue);
end;

function TdxRichEditViewVerticalScrollController.ScrollPageUp: Boolean;
begin
  Result := ScrollBarAdapter.SetRawScrollBarValue(ScrollBarAdapter.GetPageUpRawScrollBarValue);
end;

procedure TdxRichEditViewVerticalScrollController.ScrollToAbsolutePosition(AValue: Int64);
begin
  ScrollBarAdapter.BeginUpdate;
  try
    ScrollBarAdapter.Maximum := View.PageViewInfoGenerator.TotalHeight - 1;
    ScrollBarAdapter.Value := AValue;
  finally
    ScrollBarAdapter.EndUpdate;
  end;
  SynchronizeScrollbar;
  ScrollBarAdapter.EnsureSynchronized;
end;

function TdxRichEditViewVerticalScrollController.UpdatePageNumberOnScroll(
  E: TdxScrollEventArgs): Boolean;
begin
  dxAbstractError;
  Result := True;
end;

procedure TdxRichEditViewVerticalScrollController.UpdateScrollBarAdapter;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := View.PageViewInfoGenerator;
  if AGenerator.VisibleHeight < 0 then
    Exit;

  ScrollBarAdapter.BeginUpdate;
  try
    ScrollBarAdapter.Minimum := 0;
    ScrollBarAdapter.Maximum := AGenerator.TotalHeight - 1;
    ScrollBarAdapter.LargeChange := AGenerator.GetVerticalScrollBarLargeChange;
    ScrollBarAdapter.Value := Max(0, Min(AGenerator.TopInvisibleHeight, ScrollBarAdapter.Maximum - ScrollBarAdapter.LargeChange + 1));
    ScrollBarAdapter.Enabled := IsScrollPossible;
    if not ScrollBarAdapter.Enabled and (ScrollBarAdapter.Value <> AGenerator.TopInvisibleHeight) then
      View.OnVerticalScroll;
  finally
    ScrollBarAdapter.EndUpdate;
  end;
end;

{ TdxScrollBarAdapter }

constructor TdxScrollBarAdapter.Create(const AScrollBar: IdxOfficeScrollbar; const AAdapter: IdxPlatformSpecificScrollBarAdapter);
begin
  inherited Create;
  FFactor := 1.0;
  FScrollBar := AScrollBar;
  FAdapter := AAdapter;
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
end;

destructor TdxScrollBarAdapter.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

procedure TdxScrollBarAdapter.Activate;
begin
  RefreshValuesFromScrollBar;
  SubscribeScrollbarEvents;
end;

procedure TdxScrollBarAdapter.ApplyValuesToScrollBar;
begin
  if DeferredScrollBarUpdate then
    Synchronized := False
  else
    ApplyValuesToScrollBarCore;
end;

procedure TdxScrollBarAdapter.ApplyValuesToScrollBarCore;
begin
  Adapter.ApplyValuesToScrollBarCore(Self);
  Synchronized := True;
end;

procedure TdxScrollBarAdapter.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxScrollBarAdapter.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

function TdxScrollBarAdapter.CreateEmulatedScrollEventArgs(AEventType: TdxScrollEventType): TdxScrollEventArgs;
begin
  EnsureSynchronized;
  case AEventType of
    TdxScrollEventType.First:
      Result := TdxScrollEventArgs.Create(AEventType, ScrollBar.Minimum);
    TdxScrollEventType.Last:
      Result := FAdapter.CreateLastScrollEventArgs(Self);
    TdxScrollEventType.SmallIncrement:
      Result := TdxScrollEventArgs.Create(AEventType, ScrollBar.Value + ScrollBar.SmallChange);
    TdxScrollEventType.SmallDecrement:
      Result := TdxScrollEventArgs.Create(AEventType, ScrollBar.Value - ScrollBar.SmallChange);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(nil);
  end;
end;

procedure TdxScrollBarAdapter.Deactivate;
begin
  UnsubscribeScrollbarEvents;
end;

procedure TdxScrollBarAdapter.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxScrollBarAdapter.EnsureSynchronized;
begin
  EnsureSynchronizedCore;
end;

function TdxScrollBarAdapter.EnsureSynchronizedCore: Boolean;
begin
  Result := False;
  if ShouldSynchronize then
  begin
    ApplyValuesToScrollBarCore;
    Result := True;
  end;
  Assert(Synchronized);
end;

function TdxScrollBarAdapter.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxScrollBarAdapter.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxScrollBarAdapter.GetPageDownRawScrollBarValue: Integer;
begin
  EnsureSynchronized;
  Result := Adapter.GetPageDownRawScrollBarValue(Self);
end;

function TdxScrollBarAdapter.GetPageUpRawScrollBarValue: Integer;
begin
  EnsureSynchronized;
  Result := Adapter.GetPageUpRawScrollBarValue(Self);
end;

function TdxScrollBarAdapter.GetRawScrollBarValue: Integer;
begin
  EnsureSynchronized;
  Result := Adapter.GetRawScrollBarValue(Self);
end;

function TdxScrollBarAdapter.GetScroll: TdxScrollEventHandler;
begin
  Result := FOnScroll.Clone;
end;

procedure TdxScrollBarAdapter.OnBeginUpdate;
begin
end;

procedure TdxScrollBarAdapter.OnCancelUpdate;
begin
end;

procedure TdxScrollBarAdapter.OnEndUpdate;
begin
end;

procedure TdxScrollBarAdapter.OnFirstBeginUpdate;
begin
end;

procedure TdxScrollBarAdapter.OnLastCancelUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxScrollBarAdapter.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxScrollBarAdapter.OnLastEndUpdateCore;
begin
  ValidateValues;
end;

procedure TdxScrollBarAdapter.OnScroll(ASender: TObject; E: TdxScrollEventArgs);
begin
  Adapter.OnScroll(Self, ASender, E);
end;

procedure TdxScrollBarAdapter.RaiseScroll(AArgs: TdxScrollEventArgs);
begin
  if not FOnScroll.Empty then
    FOnScroll.Invoke(TObject(ScrollBar), AArgs);
end;

procedure TdxScrollBarAdapter.RefreshValuesFromScrollBar;
begin
  BeginUpdate;
  try
    Minimum := Round(ScrollBar.Minimum / Factor);
    Maximum := Round(ScrollBar.Maximum / Factor);
    LargeChange := Round(ScrollBar.LargeChange / Factor);
    SmallChange := Round(ScrollBar.SmallChange / Factor);
    Value := Round(ScrollBar.Value / Factor);
    Enabled := ScrollBar.Enabled;
  finally
    EndUpdate;
  end;
  Synchronized := True;
end;

procedure TdxScrollBarAdapter.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TdxScrollBarAdapter.SetLargeChange(const Value: Int64);
begin
  if FLargeChange = Value then
    Exit;
  FLargeChange := Value;
  ValidateValues;
end;

procedure TdxScrollBarAdapter.SetMaximum(const Value: Int64);
begin
  if FMaximum = Value then
    Exit;
  FMaximum := Value;
  ValidateValues;
end;

procedure TdxScrollBarAdapter.SetMinimum(const Value: Int64);
begin
  if FMinimum = Value then
    Exit;
  FMinimum := Value;
  ValidateValues;
end;

function TdxScrollBarAdapter.SetRawScrollBarValue(AValue: Integer): Boolean;
begin
  Assert(Synchronized);
  Result := Adapter.SetRawScrollBarValue(Self, AValue);
end;

procedure TdxScrollBarAdapter.SetValue(const Value: Int64);
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  ValidateValues;
end;

function TdxScrollBarAdapter.ShouldSynchronize: Boolean;
begin
  Result := DeferredScrollBarUpdate and not Synchronized;
end;

procedure TdxScrollBarAdapter.SubscribeScrollbarEvents;
begin
  ScrollBar.Scroll.Add(OnScroll);
end;

procedure TdxScrollBarAdapter.UnsubscribeScrollbarEvents;
begin
  ScrollBar.Scroll.Remove(OnScroll);
end;

function TdxScrollBarAdapter.SynchronizeScrollBarAvoidJump: Boolean;
var
  ARelativePos, AActualRelativePos: Double;
begin
  Result := False;
  if ShouldSynchronize then
  begin
    ARelativePos := Value / Math.Max(1, (Maximum - LargeChange + 1 - Minimum));
    AActualRelativePos := ScrollBar.Value / Max(1, ScrollBar.Maximum - ScrollBar.LargeChange + 1 - ScrollBar.Minimum);
    if ARelativePos <= AActualRelativePos then
    begin
      ApplyValuesToScrollBarCore;
      Assert(Synchronized);
      Result := True;
    end;
  end;
end;

procedure TdxScrollBarAdapter.ValidateValues;
begin
  if not IsUpdateLocked then
    ValidateValuesCore;
end;

procedure TdxScrollBarAdapter.ValidateValuesCore;
begin
  if LargeChange < 0 then
    TdxRichEditExceptions.ThrowArgumentException('LargeChange', LargeChange);

  if Minimum > Maximum then
    FMaximum := Minimum;
  FLargeChange := Math.Min(LargeChange, Maximum - Minimum + 1);
  FValue := Math.Max(Value, Minimum);
  FValue := Math.Min(Value, Maximum - LargeChange + 1);
end;

{ TdxVerticalScrollBarAdapter }

function TdxVerticalScrollBarAdapter.GetDeferredScrollBarUpdate: Boolean;
begin
  Result := True;
end;

function TdxVerticalScrollBarAdapter.GetSynchronized: Boolean;
begin
  Result := FSynchronized;
end;

procedure TdxVerticalScrollBarAdapter.SetSynchronized(AValue: Boolean);
begin
  FSynchronized := AValue;
end;

{ TdxHorizontalScrollBarAdapter }

function TdxHorizontalScrollBarAdapter.GetDeferredScrollBarUpdate: Boolean;
begin
  Result := False;
end;

function TdxHorizontalScrollBarAdapter.GetSynchronized: Boolean;
begin
  Result := True;
end;

procedure TdxHorizontalScrollBarAdapter.SetSynchronized(AValue: Boolean);
begin
end;

{ TdxMouseCursorCalculator }

constructor TdxMouseCursorCalculator.Create(AView: TdxRichEditView);
begin
  inherited Create;
  FView := AView;
end;

function TdxMouseCursorCalculator.Calculate(AHitTestResult: TdxRichEditHitTestResult;
  const APhysicalPoint: TPoint): TCursor;
begin
  if AHitTestResult = nil then
  begin
    AHitTestResult := View.CalculateNearestPageHitTest(APhysicalPoint, False);
    try
      if AHitTestResult <> nil then
        Result := CalculateHotZoneCursor(AHitTestResult)
      else
        Result := TdxRichEditCursors.Default;
    finally
      AHitTestResult.Free;
    end;
  end
  else
    Result := CalculateCore(AHitTestResult);
end;

function TdxMouseCursorCalculator.CalculateCore(AResult: TdxRichEditHitTestResult): TCursor;
var
  ASelectionManager: TdxEnhancedSelectionManager;
  ATableRow: TdxTableRowViewInfoBase;
  AVirtualColumn: TdxVirtualTableColumn;
begin
  Result := CalculateHotZoneCursor(AResult);
  if Result <> TdxRichEditCursors.Default then
    Exit;
  ASelectionManager := TdxEnhancedSelectionManager.Create(View.DocumentModel.ActivePieceTable);
  try
    ATableRow := ASelectionManager.CalculateTableRowToResize(AResult);
    if ASelectionManager.ShouldResizeTableRow(View.Control, AResult, ATableRow) then
      Exit(TdxRichEditCursors.ResizeTableRow);

    AVirtualColumn := ASelectionManager.CalculateTableCellsToResizeHorizontally(AResult);
    try
      if ASelectionManager.ShouldResizeTableCellsHorizontally(View.Control, AResult, AVirtualColumn) then
        Exit(TdxRichEditCursors.ResizeTableColumn)
      else if ASelectionManager.ShouldSelectEntireTableRow(AResult) then
        Exit(TdxRichEditCursors.SelectRow)
      else if ASelectionManager.ShouldSelectEntireTableCell(AResult) then
        Exit(TdxRichEditCursors.SelectTableCell)
      else if ASelectionManager.ShouldSelectEntireTableColumn(AResult) then
        Exit(TdxRichEditCursors.SelectTableColumn)
      else if ASelectionManager.ShouldSelectFloatingObject(AResult) then
      begin
        if IsEditable then
          Result := TdxRichEditCursors.SizeAll
        else
          Result := CalculateCharacterMouseCursor(AResult);
      end
      else if ASelectionManager.ShouldSelectEntireRow(AResult) then
        Result := TdxRichEditCursors.SelectRow
      else
        Result := CalculateCharacterMouseCursor(AResult);
    finally
      AVirtualColumn.Free;
    end;
  finally
    ASelectionManager.Free;
  end;
end;

function TdxMouseCursorCalculator.CalculateHotZoneCursor(AResult: TdxRichEditHitTestResult): TCursor;
var
  AHotZone: TdxHotZone;
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  AHotZone := View.SelectionLayout.CalculateHotZone(AResult, View);
  if AHotZone = nil then
  begin
    ASelectionManager := TdxEnhancedSelectionManager.Create(View.DocumentModel.ActivePieceTable);
    try
      if ASelectionManager.ShouldSelectCommentMoreButton(AResult) then
        Exit(TdxRichEditCursors.Hand);
      if ASelectionManager.ShouldSelectComment(AResult) then
        Exit(TdxRichEditCursors.IBeam);
      if ASelectionManager.ShouldSelectFloatingObject(AResult) and IsEditable then
        Exit(TdxRichEditCursors.SizeAll);
    finally
      ASelectionManager.Free;
    end;
  end;
  Result := CalculateHotZoneCursorCore(AHotZone);
end;

function TdxMouseCursorCalculator.CalculateHotZoneCursorCore(AHotZone: TdxHotZone): TCursor;
begin
  if (AHotZone <> nil) and View.Control.InnerControl.IsEditable then
    Result := AHotZone.Cursor
  else
    Result := TdxRichEditCursors.Default;
end;

function TdxMouseCursorCalculator.CalculateCharacterMouseCursor(AResult: TdxRichEditHitTestResult): TCursor;

  function IsCursorInSelection: Boolean;
  var
    APos: TdxDocumentModelPosition;
    ASelectionLayout: TdxSelectionLayout;
    APieceTable: TdxPieceTable;
  begin
    if AResult.Box = nil then
      Exit(True);

    APieceTable := AResult.PieceTable;
    APos := AResult.Character.GetFirstPosition(APieceTable);

    if not APos.IsValid then
      Exit(False);

    ASelectionLayout := FView.SelectionLayout;
    Result := ASelectionLayout.HitTest(APos.LogPosition, AResult.LogicalPoint);
  end;

var
  ACharacter: TdxCharacterBox;
begin
  if IsCursorInSelection then
    Result := TdxRichEditCursors.Default
  else
  begin
    ACharacter := AResult.Character;
    if TdxTextRunBase(ACharacter.GetRun(AResult.PieceTable)).FontItalic then
      Result := TdxRichEditCursors.IBeamItalic
    else
      Result := TdxRichEditCursors.IBeam;
  end;
end;

function TdxMouseCursorCalculator.IsEditable: Boolean;
begin
  Result := View.Control.InnerControl.IsEditable;
end;

{ TdxCaretDocumentLayoutPosition }

constructor TdxCaretDocumentLayoutPosition.Create(AView: TdxRichEditView);
begin
  inherited Create(AView.DocumentLayout, AView.DocumentModel.MainPieceTable, 0);
  FView := AView;
end;

function TdxCaretDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxCaretDocumentLayoutPosition.Create(FView);
end;

procedure TdxCaretDocumentLayoutPosition.EnsureFormattingComplete;
begin
  FView.EnsureFormattingCompleteForSelection;
end;

procedure TdxCaretDocumentLayoutPosition.EnsurePageSecondaryFormattingComplete(APage: TdxPage);
begin
  FView.EnsurePageSecondaryFormattingComplete(APage);
end;

function TdxCaretDocumentLayoutPosition.GetPieceTable: TdxPieceTable;
begin
  Result := FView.DocumentModel.ActivePieceTable;
end;

{ TdxScrollByPhysicalHeightCalculator }

constructor TdxScrollByPhysicalHeightCalculator.Create(AView: TdxRichEditView);
begin
  inherited Create;
  FView := AView;
end;

function TdxScrollByPhysicalHeightCalculator.CalculateRowsTotalVisibleHeight(AGenerator: TdxPageViewInfoGeneratorBase;
  ALastRowIndex: Integer; ABottomY: Integer): Int64;
var
  AViewPortBounds: TRect;
  I: Integer;
  ARows: TdxPageViewInfoRowCollection;
  ALayoutManager: TdxPageGeneratorLayoutManager;
begin
  ALayoutManager := FView.PageViewInfoGenerator;
  ARows := AGenerator.PageRows;
  Result := 0;
  if ALastRowIndex > 0 then
  begin
    Result := ALayoutManager.CalculatePagesTotalLogicalHeightBelow(ARows[0], 0);
    for I := 1 to ALastRowIndex - 1 do
      Inc(Result, ALayoutManager.CalculatePagesTotalLogicalHeight(ARows[I]));
  end;
  AViewPortBounds.InitSize(0, 0, 0, ABottomY);
  Inc(Result, ALayoutManager.CalculatePagesTotalVisibleLogicalHeight(ARows[ALastRowIndex], AViewPortBounds));
end;

function TdxScrollByPhysicalHeightCalculator.CalculateScrollDelta(APhysicalVerticalOffset: Integer): Int64;
var
  AInvisiblePhysicalVerticalOffset: Integer;
begin
  Assert(APhysicalVerticalOffset >= 0);
  if APhysicalVerticalOffset <= 0 then
    Exit(0);
  if not CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset, Result) then
  begin
    AInvisiblePhysicalVerticalOffset := CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset);
    Result := CalculateScrollDeltaForInvisibleRows(AInvisiblePhysicalVerticalOffset);
  end;
end;

function TdxScrollByPhysicalHeightCalculator.CalculateScrollDeltaForInvisibleRows(APhysicalVerticalOffset: Integer): Int64;
var
  AIndex, AFirstInvisiblePageIndex: Integer;
  AGenerator: TdxInvisiblePageRowsGenerator;
begin
  AFirstInvisiblePageIndex := CalculateFirstInvisiblePageIndex;
  if AFirstInvisiblePageIndex < 0 then
    Exit(0);
  AGenerator := TdxInvisiblePageRowsGenerator.Create(View.FormattingController.PageController.Pages, View.PageViewInfoGenerator);
  try
    AGenerator.FirstPageIndex := AFirstInvisiblePageIndex;
    AGenerator.FirstInvalidPageIndex := CalculateFirstInvalidPageIndex;
    AIndex := LookupIntersectingRowIndexInInvisibleRows(AGenerator, APhysicalVerticalOffset);
    if AIndex < 0 then
      Exit(GetDefaultScrollDeltaForInvisibleRows);
    Result := CalculateRowsTotalVisibleHeight(AGenerator.Generator, AIndex, APhysicalVerticalOffset);
    Inc(Result, CalculateVisibleRowsScrollDelta);
  finally
    AGenerator.Free;
  end;
end;

function TdxScrollByPhysicalHeightCalculator.LookupIntersectingRowIndexInInvisibleRows(AGenerator: TdxInvisiblePageRowsGenerator; Y: Integer): Integer;
var
  ARow: TdxPageViewInfoRow;
  ARowIndex: Integer;
begin
  ARowIndex := 0;
  while True do
  begin
    ARow := AGenerator.GenerateNextRow;
    if ARow = nil then
      Exit(-1);
    if ARow.IntersectsWithHorizontalLine(Y) then
      Exit(ARowIndex);
    Inc(ARowIndex);
  end;
end;

{ TdxScrollUpByPhysicalHeightCalculator }

function TdxScrollUpByPhysicalHeightCalculator.CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset: Integer; out ADelta: Int64): Boolean;
var
  AFirstRow: TdxPageViewInfoRow;
  AViewPortBounds: TRect;
  APageIndex: Integer;
begin
  AFirstRow := View.PageViewInfoGenerator.ActiveGenerator.PageRows.First;
  if AFirstRow.IntersectsWithHorizontalLine(-APhysicalVerticalOffset) or (AFirstRow.Bounds.Top = -APhysicalVerticalOffset) then
  begin
    AViewPortBounds.InitSize(0, -APhysicalVerticalOffset, 0, APhysicalVerticalOffset);
    ADelta := View.PageViewInfoGenerator.CalculatePagesTotalVisibleLogicalHeight(AFirstRow, AViewPortBounds);
    Exit(True);
  end
  else
  begin
    APageIndex := View.FormattingController.PageController.Pages.IndexOf(AFirstRow.First.Page);
    Result := APageIndex = 0;
    if Result then
      ADelta := View.PageViewInfoGenerator.CalculatePagesTotalLogicalHeightAbove(AFirstRow, 0);
  end;
end;

function TdxScrollUpByPhysicalHeightCalculator.CalculateFirstInvisiblePageIndex: Integer;
begin
  Result := View.CalculateFirstInvisiblePageIndexBackward;
end;

function TdxScrollUpByPhysicalHeightCalculator.CalculateFirstInvalidPageIndex: Integer;
begin
  Result := -1;
end;

function TdxScrollUpByPhysicalHeightCalculator.CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset: Integer): Integer;
var
  AFirstRow: TdxPageViewInfoRow;
begin
  AFirstRow := View.PageViewInfoGenerator.ActiveGenerator.PageRows.First;
  Result := APhysicalVerticalOffset + AFirstRow.Bounds.Top;
end;

function TdxScrollUpByPhysicalHeightCalculator.CalculateVisibleRowsScrollDelta: Int64;
var
  AViewPortBounds: TRect;
  ALayoutManager: TdxPageGeneratorLayoutManager;
  AFirstRow: TdxPageViewInfoRow;
begin
  ALayoutManager := View.PageViewInfoGenerator;
  AFirstRow := View.PageViewInfoGenerator.ActiveGenerator.PageRows.First;
  AViewPortBounds := ALayoutManager.ViewPortBounds;
  AViewPortBounds.Top := AFirstRow.Bounds.Top;
  AViewPortBounds.Height := -AViewPortBounds.Top;
  Result := ALayoutManager.CalculatePagesTotalVisibleLogicalHeight(AFirstRow, AViewPortBounds);
end;

function TdxScrollUpByPhysicalHeightCalculator.GetDefaultScrollDeltaForInvisibleRows: Int64;
begin
  Result := View.PageViewInfoGenerator.TopInvisibleHeight;
end;

{ TdxScrollDownByPhysicalHeightCalculator }

function TdxScrollDownByPhysicalHeightCalculator.CalculateScrollDeltaForVisibleRows(APhysicalVerticalOffset: Integer; out ADelta: Int64): Boolean;
var
  AIntersectingRowIndex: Integer;
begin
  AIntersectingRowIndex := LookupIntersectingRowIndex(APhysicalVerticalOffset);
  Result := AIntersectingRowIndex >= 0;
  if Result then
    ADelta := CalculateRowsTotalVisibleHeight(View.PageViewInfoGenerator.ActiveGenerator, AIntersectingRowIndex, APhysicalVerticalOffset);
end;

function TdxScrollDownByPhysicalHeightCalculator.CalculateFirstInvisiblePageIndex: Integer;
begin
  Result := View.CalculateFirstInvisiblePageIndexForward;
end;

function TdxScrollDownByPhysicalHeightCalculator.CalculateFirstInvalidPageIndex: Integer;
begin
  Result := View.FormattingController.PageController.Pages.Count;
end;

function TdxScrollDownByPhysicalHeightCalculator.CalculateInvisiblePhysicalVerticalOffset(APhysicalVerticalOffset: Integer): Integer;
var
  AVisibleRows: TdxPageViewInfoRowCollection;
  I, ATotalRowsHeight: Integer;
begin
  AVisibleRows := View.PageViewInfoGenerator.ActiveGenerator.PageRows;
  ATotalRowsHeight := AVisibleRows[0].Bounds.Bottom;
  for I := 1 to AVisibleRows.Count - 1 do
    Inc(ATotalRowsHeight, AVisibleRows[I].Bounds.Height);
  Result := APhysicalVerticalOffset - ATotalRowsHeight;
end;

function TdxScrollDownByPhysicalHeightCalculator.CalculateVisibleRowsScrollDelta: Int64;
var
  ALayoutManager: TdxPageGeneratorLayoutManager;
  I: Integer;
  ARows: TdxPageViewInfoRowCollection;
begin
  ALayoutManager := View.PageViewInfoGenerator;
  ARows := View.PageViewInfoGenerator.ActiveGenerator.PageRows;
  Result := ALayoutManager.CalculatePagesTotalLogicalHeightBelow(ARows[0], ALayoutManager.ViewPortBounds.Top);
  for I := 1 to ARows.Count - 1 do
    Inc(Result, ALayoutManager.CalculatePagesTotalLogicalHeight(ARows[I]));
end;

function TdxScrollDownByPhysicalHeightCalculator.GetDefaultScrollDeltaForInvisibleRows: Int64;
var
  ARows: TdxPageViewInfoRowCollection;
  ALastRow: TdxPageViewInfoRow;
begin
  try
    ARows := View.PageViewInfoGenerator.ActiveGenerator.PageRows;
    ALastRow := ARows.Last;
    if ALastRow = nil then
      Result := 0
    else
      Result := CalculateRowsTotalVisibleHeight(View.PageViewInfoGenerator.ActiveGenerator, ARows.Count - 1, ALastRow.Bounds.Bottom);
  except
    Result := 0;
  end;
end;

function TdxScrollDownByPhysicalHeightCalculator.LookupIntersectingRowIndex(Y: Integer): Integer;
var
  ARows: TdxPageViewInfoRowCollection;
  I: Integer;
begin
  ARows := View.PageViewInfoGenerator.ActiveGenerator.PageRows;
  for I := 0 to ARows.Count - 1 do
    if ARows[I].IntersectsWithHorizontalLine(Y) then
      Exit(I);
  Exit(-1);
end;

{ TdxNonPrintViewPageControllerBase }

constructor TdxNonPrintViewPageControllerBase.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create(ADocumentLayout, nil, nil);
end;

function TdxNonPrintViewPageControllerBase.GetColumnBottom(AColumn: TdxColumn): Integer;
var
  I: Integer;
  ALastRow: TdxRow;
  AColumnBounds: TRect;
  ATableCellRow: TdxTableCellRow;
  ATableViewInfo: TdxTableViewInfo;
  AFloatingObjects: TdxFloatingObjectBoxList;
  AParagraphFrames: TdxParagraphFrameBoxList;
begin
  Result := 0;
  if AColumn.Rows.Count > 0 then
  begin
    ALastRow := AColumn.Rows.Last;
    ATableCellRow := Safe<TdxTableCellRow>.Cast(ALastRow);
    if ATableCellRow <> nil then
    begin
      ATableViewInfo := ATableCellRow.CellViewInfo.TableViewInfo;
      while ATableViewInfo.ParentTableCellViewInfo <> nil do
        ATableViewInfo := ATableViewInfo.ParentTableCellViewInfo.TableViewInfo;
      Result := ATableViewInfo.GetTableBottom;
    end
    else
      Result := ALastRow.Bounds.Bottom;
  end;
  AColumnBounds := AColumn.Bounds;
  AColumnBounds.Height := MaxInt - AColumnBounds.Top;
  AFloatingObjects := FloatingObjectsLayout.GetAllObjectsInRectangle(AColumnBounds);
  try
    for I := 0 to AFloatingObjects.Count - 1 do
      Result := Max(Result, AFloatingObjects[I].ExtendedBounds.Bottom);
  finally
    AFloatingObjects.Free;
  end;
  if ParagraphFramesLayout <> nil then
  begin
    AParagraphFrames := ParagraphFramesLayout.GetObjectsInRectangle(AColumnBounds);
    try
      for I := 0 to AParagraphFrames.Count - 1 do
        Result := Max(Result, AParagraphFrames[I].ExtendedBounds.Bottom);
    finally
      AParagraphFrames.Free;
    end;
  end;
end;

{ TdxBackgroundFormatter.TWorkThread }

constructor TdxBackgroundFormatter.TWorkThread.Create(AFormatter: TdxBackgroundFormatter);
begin
  inherited Create(True);
  FFormatter := AFormatter;
  Priority := tpLower;
end;

procedure TdxBackgroundFormatter.TWorkThread.Execute;
begin
  while not Terminated do
    if not WorkBody then
      Break;
end;

function TdxBackgroundFormatter.TWorkThread.GetWaitCommandHandles: TWaitCommandHandles;
var
  I: TCommand;
begin
  for I := Low(FFormatter.Commands) to High(FFormatter.Commands) do
    Result[I] := FFormatter.Commands[I].Handle;
end;

function TdxBackgroundFormatter.TWorkThread.GetWaitContinueLayoutHandles: TWaitContinueLayoutHandles;
begin
  Result[0] := FFormatter.ContinueLayout.Handle;
  Result[1] := FFormatter.Commands[TCommand.Shutdown].Handle;
end;

function TdxBackgroundFormatter.TWorkThread.WorkBody: Boolean;
var
  AHandles: TWaitCommandHandles;
  AResult: HRESULT;
  ACommand: TCommand;
begin
  Result := False;
  if not WaitForContinueLayoutAllowed then
    Exit;

  AHandles := GetWaitCommandHandles;
  repeat
    AResult := WaitForMultipleObjects(Length(AHandles), @AHandles, False, 1000);
    if Terminated then
      Exit(False);
    if AResult = WAIT_TIMEOUT then
      Continue;

    ACommand := TCommand(AResult - WAIT_OBJECT_0);
    if ACommand in [Low(AHandles)..High(AHandles)] then
      Exit(FFormatter.HandleCommand(ACommand))
    else
      Exit(False);
  until Terminated;
end;

function TdxBackgroundFormatter.TWorkThread.WaitForContinueLayoutAllowed: Boolean;
var
  AHandles: TWaitContinueLayoutHandles;
  AResult: HRESULT;
begin
  Result := False;
  if Terminated then
    Exit;

  AHandles := GetWaitContinueLayoutHandles;
  repeat
    AResult := WaitForMultipleObjects(Length(AHandles), @AHandles, False, 1000);
    if AResult = WAIT_TIMEOUT then
      Continue;

    if AResult - WAIT_OBJECT_0 = 0 then
      Exit(True)
    else
      Exit(False);
  until Terminated;
end;

{ TdxBackgroundFormatter }

constructor TdxBackgroundFormatter.Create(AController: TdxDocumentFormattingController; const ACommentPadding: TdxCommentPadding);
begin
  Assert(AController <> nil);
  inherited Create;
  FLock := TObject.Create;
  FController := AController;

  FSpellCheckerController := TdxEmptySpellCheckerController.Create;

  FPerformPrimaryLayoutUntil := IsPrimaryLayoutComplete;

  FDocumentFormatter := TdxDocumentFormatter.Create(AController);
  FCommentPadding := ACommentPadding;
  FSecondaryFormatter := CreateParagraphFinalFormatter(AController.DocumentLayout);

  FCurrentPosition := TdxDocumentModelPosition.Create(PieceTable);
  FCurrentPosition.LogPosition := -1;
  FSecondaryLayoutStart := TdxDocumentModelPosition.Create(PieceTable);
  FSecondaryLayoutEnd := TdxDocumentModelPosition.Create(PieceTable);
  FResetSecondaryLayoutFromPage := -1;

  SubscribeToFormattingControllerEvents;
  InitializeWorkThread;

  ResetPrimaryLayout;
  ResetSecondaryLayout;
end;

destructor TdxBackgroundFormatter.Destroy;
begin
  DoneWorkThread;
  FreeAndNil(FDocumentFormatter);
  FreeAndNil(FSecondaryFormatter);
  FreeAndNil(FSpellCheckerController);
  FLock.Free;
  inherited Destroy;
end;


procedure TdxBackgroundFormatter.CheckExecutedAtUIThread;
begin
end;


procedure TdxBackgroundFormatter.CheckExecutedAtWorkerThread;
begin
end;

procedure TdxBackgroundFormatter.BeginDocumentRendering(
  APerformPrimaryLayoutUntil: TdxPredicate<TdxDocumentModelPosition>);
begin
  CheckExecutedAtUIThread;
  if not Assigned(APerformPrimaryLayoutUntil) then
    APerformPrimaryLayoutUntil := IsPrimaryLayoutComplete;
  SuspendWorkThread;
  if AlreadySuspended then
  begin
    Exit;
  end;
  FPerformPrimaryLayoutUntil := APerformPrimaryLayoutUntil;
  DoBeginDocumentRendering;
  if FResetSecondaryLayoutFromPage >= 0 then
  begin
    RefreshSecondaryLayout(FResetSecondaryLayoutFromPage);
    FResetSecondaryLayoutFromPage := -1;
  end;
end;

procedure TdxBackgroundFormatter.BeginDocumentUpdate;
begin
  CheckExecutedAtUIThread;
  Inc(FDocumentBeginUpdateCount);
  SuspendWorkThread;
end;

function TdxBackgroundFormatter.CreateParagraphFinalFormatter(
  ADocumentLayout: TdxDocumentLayout): TdxParagraphFinalFormatter;
begin
  Result := TdxParagraphFinalFormatter.Create(ADocumentLayout);
end;

procedure TdxBackgroundFormatter.InitializeWorkThread;
begin
  FContinueLayout := TSimpleEvent.Create(nil, False, True, '');
  FSecondaryLayoutComplete := TSimpleEvent.Create(nil, True, False, '');
  FCommands[TCommand.Shutdown] := TSimpleEvent.Create(nil, True, False, '');
  FCommands[TCommand.PerformPrimaryLayout] := TSimpleEvent.Create(nil, True, False, '');
  FCommands[TCommand.PerformSecondaryLayout] := TSimpleEvent.Create(nil, True, False, '');
  FCommands[TCommand.None] := TSimpleEvent.Create(nil, True, False, '');

  FWorkThread := TWorkThread.Create(Self);
end;

procedure TdxBackgroundFormatter.DoneWorkThread;
begin
  SuspendWorkThread;
  FWorkThread.Terminate;
  SetCommandEvent(TCommand.Shutdown);
  ResumeWorkThread;

  FreeAndNil(FWorkThread);
  UnsubscribeFromFormattingControllerEvents;
  FreeAndNil(FContinueLayout);
  FreeAndNil(FSecondaryLayoutComplete);
  FreeAndNil(FCommands[TCommand.Shutdown]);
  FreeAndNil(FCommands[TCommand.PerformPrimaryLayout]);
  FreeAndNil(FCommands[TCommand.PerformSecondaryLayout]);
  FreeAndNil(FCommands[TCommand.None]);
end;

procedure TdxBackgroundFormatter.SetCommandEvent(ACommand: TCommand);
begin
  SetCommandEvent(Commands[ACommand]);
end;

procedure TdxBackgroundFormatter.SetCommandEvent(AEvent: TSimpleEvent);
begin
  if AEvent <> nil then
    AEvent.SetEvent;
end;

procedure TdxBackgroundFormatter.ResetCommandEvent(ACommand: TCommand);
begin
  ResetCommandEvent(Commands[ACommand]);
end;

procedure TdxBackgroundFormatter.ResetCommandEvent(AEvent: TSimpleEvent);
begin
  if AEvent <> nil then
    AEvent.ResetEvent;
end;

procedure TdxBackgroundFormatter.EndDocumentRendering;
begin
  CheckExecutedAtUIThread;
  FPerformPrimaryLayoutUntil := IsPrimaryLayoutComplete;
  ResumeWorkThread;
end;

procedure TdxBackgroundFormatter.EndDocumentUpdate;
begin
  CheckExecutedAtUIThread;
  Assert(FDocumentBeginUpdateCount > 0);
  Dec(FDocumentBeginUpdateCount);
  ResumeWorkThread;
end;

function TdxBackgroundFormatter.GetDocumentLayout: TdxDocumentLayout;
begin
  Result := FController.DocumentLayout;
end;

function TdxBackgroundFormatter.GetDocumentModel: TdxDocumentModel;
begin
  Result := FController.DocumentModel;
end;

function TdxBackgroundFormatter.GetPieceTable: TdxPieceTable;
begin
  Result := FController.PieceTable;
end;

function TdxBackgroundFormatter.GetAlreadySuspended: Boolean;
begin
  Result := FThreadSuspendCount > 1;
end;

function TdxBackgroundFormatter.HandleCommand(ACommand: TCommand): Boolean;
begin
  case ACommand of
    TCommand.Shutdown:
      begin
        ContinueLayout.SetEvent;
        Exit(False);
      end;
    TCommand.PerformPrimaryLayout:
      begin
        PerformPrimaryLayout;
        ContinueLayout.SetEvent;
      end;
    TCommand.PerformSecondaryLayout:
      begin
        PerformSecondaryLayout;
        ContinueLayout.SetEvent;
      end;
    TCommand.None:
      ContinueLayout.SetEvent;
  end;
  Result := True;
end;

function TdxBackgroundFormatter.IsPrimaryLayoutComplete(const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean;
begin
  CheckExecutedAtWorkerThread;
  Result := FPrimaryLayoutComplete;
end;

function TdxBackgroundFormatter.IsTableFormattingComplete(const ANextPosition: TdxDocumentModelPosition): Boolean;
begin
  Result := not DocumentFormatter.ParagraphFormatter.RowsController.TablesController.IsInsideTable;
end;

procedure TdxBackgroundFormatter.NotifyDocumentChanged(const AFrom, ATo: TdxDocumentModelPosition;
  ADocumentLayoutResetType: TdxDocumentLayoutResetType);
begin
  case ADocumentLayoutResetType of
    TdxDocumentLayoutResetType.SecondaryLayout:
      ResetSecondaryLayout;
    TdxDocumentLayoutResetType.AllPrimaryLayout:
      begin
        ResetSecondaryLayout;
        ResetPrimaryLayout;
      end;
    TdxDocumentLayoutResetType.PrimaryLayoutFormPosition:
      begin
        ResetSecondaryLayout;
        if ShouldResetPrimaryLayout(AFrom, ATo) then
          ResetPrimaryLayout(AFrom, ATo);
        ResetCalculators;
      end;
    TdxDocumentLayoutResetType.None:
      ;
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxBackgroundFormatter.OnNewMeasurementAndDrawingStrategyChanged;
begin
  FDocumentFormatter.OnNewMeasurementAndDrawingStrategyChanged;
end;

procedure TdxBackgroundFormatter.PerformPageSecondaryFormatting(APage: TdxPage);
begin
  TMonitor.Enter(FLock);
  try
    FSecondaryFormatter.FormatPage(APage);
    APage.SecondaryFormattingComplete := APage.PrimaryFormattingComplete;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TdxBackgroundFormatter.PerformPrimaryLayout;
begin
  CheckExecutedAtWorkerThread;
  PerformPrimaryLayoutCore;
end;

function TdxBackgroundFormatter.DoPerformPrimaryLayout: TdxFormattingProcessResult;
var
  APrevParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  I, AOffset, ACount: Integer;
begin
  Result := FDocumentFormatter.FormatNextRow;
  if Result.FormattingProcess = TdxFormattingProcess.ContinueFromParagraph then
    APrevParagraphIndex :=  Result.ParagraphIndex - 1
  else
    APrevParagraphIndex :=  FDocumentFormatter.ParagraphIndex;
  if APrevParagraphIndex >= 0 then
  begin
    AParagraph := PieceTable.Paragraphs[APrevParagraphIndex];
    FCurrentPosition := TdxDocumentModelPosition.Create(PieceTable);
    FCurrentPosition.ParagraphIndex := APrevParagraphIndex;
    FCurrentPosition.RunIndex := FDocumentFormatter.ParagraphFormatter.Iterator.RunIndex;
    AOffset := 0;
    if Result.FormattingProcess = TdxFormattingProcess.ContinueFromParagraph then
      ACount := AParagraph.LastRunIndex + 1
    else
      ACount := FDocumentFormatter.ParagraphFormatter.Iterator.RunIndex;
    for I := AParagraph.FirstRunIndex to ACount - 1 do
      Inc(AOffset, PieceTable.Runs[I].Length);
    FCurrentPosition.RunStartLogPosition := AParagraph.LogPosition + AOffset;
    if Result.FormattingProcess <> TdxFormattingProcess.ContinueFromParagraph then
    begin
      Inc(AOffset, FDocumentFormatter.ParagraphFormatter.Iterator.Offset);
      FCurrentPosition.LogPosition := AParagraph.LogPosition + AOffset;
    end
    else
      FCurrentPosition.LogPosition := AParagraph.EndLogPosition;
  end
  else
  begin
    FCurrentPosition := TdxDocumentModelPosition.Create(PieceTable);
    FCurrentPosition.LogPosition := -1;
  end;
end;

procedure TdxBackgroundFormatter.PerformPrimaryLayoutCore;
var
  AResult: TdxFormattingProcessResult;
begin
  AResult := DoPerformPrimaryLayout;
  if AResult.FormattingProcess = TdxFormattingProcess.Finish then
  begin
    ResetCommandEvent(TCommand.PerformPrimaryLayout);
    FPrimaryLayoutComplete := True;
    UpdateSecondaryPositions(SecondaryLayoutStart, FController.PageController.Pages.Last.GetLastPosition(FDocumentFormatter.DocumentModel.MainPieceTable));
  end;
end;

procedure TdxBackgroundFormatter.DoBeginDocumentRendering;
begin
  ResumeWorkThread;

  SecondaryLayoutComplete.WaitFor(Windows.INFINITE);
  SuspendWorkThread;
end;

procedure TdxBackgroundFormatter.PerformSecondaryLayout;
begin
  CheckExecutedAtWorkerThread;
  if IsPrimaryLayoutComplete(FCurrentPosition) then
    PerformSecondaryLayoutCore
  else
    if FPerformPrimaryLayoutUntil(FCurrentPosition) and IsTableFormattingComplete(FCurrentPosition) then
      PerformSecondaryLayoutCore
    else
      PerformPrimaryLayout;
end;

procedure TdxBackgroundFormatter.PerformSecondaryLayoutCore;
var
  APage: TdxPage;
  APages: TdxPageCollection;
  I, AFirstPageIndex, ALastPageIndex: Integer;
  ABoxAndLogPositionComparable: TdxBoxAndLogPositionComparable;
begin
  CheckExecutedAtWorkerThread;
  APages := FController.PageController.Pages;
  ABoxAndLogPositionComparable := TdxBoxAndLogPositionComparable.Create(DocumentFormatter.DocumentModel.MainPieceTable, FSecondaryLayoutStart.LogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(APages, ABoxAndLogPositionComparable, AFirstPageIndex) then
      AFirstPageIndex := 0;
  finally
    ABoxAndLogPositionComparable.Free;
  end;

  ABoxAndLogPositionComparable := TdxBoxAndLogPositionComparable.Create(DocumentFormatter.DocumentModel.MainPieceTable, FSecondaryLayoutEnd.LogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(APages, ABoxAndLogPositionComparable, ALastPageIndex) then
      ALastPageIndex := APages.Count - 1;
  finally
    ABoxAndLogPositionComparable.Free;
  end;

  for I := AFirstPageIndex to ALastPageIndex do
  begin
    APage := APages[I];
    if not APage.SecondaryFormattingComplete  then
      PerformPageSecondaryFormatting(APage);
  end;
  SpellCheckerController.CheckPages(AFirstPageIndex);
  SetCommandEvent(SecondaryLayoutComplete);
  Commands[TCommand.PerformSecondaryLayout].ResetEvent;
end;

procedure TdxBackgroundFormatter.RefreshSecondaryLayout(AFirstPageIndex: Integer);
var
  I: Integer;
  APage: TdxPage;
  APages: TdxPageCollection;
begin
  APages := FController.PageController.Pages;
  for I := 0 to APages.Count - 1 do
  begin
    APage := APages[I];
    if not APage.PrimaryFormattingComplete then
      Break;
    if not APage.SecondaryFormattingComplete then
      PerformPageSecondaryFormatting(APage);
  end;
end;

procedure TdxBackgroundFormatter.SetSpellCheckerController(const Value: TdxSpellCheckerControllerBase);
begin
  Assert(Value <> nil);
  FSpellCheckerController.Free;
  FSpellCheckerController := Value;
end;

procedure TdxBackgroundFormatter.ResetCalculators;
begin
  FSecondaryFormatter.BookmarkCalculator.Reset;
  FSecondaryFormatter.RangePermissionCalculator.Reset;
  FSpellCheckerController.ResetCore;
end;

procedure TdxBackgroundFormatter.ResetPrimaryLayout;
begin
  FDocumentFormatter.ParagraphIndex := -1;
  FDocumentFormatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);

  FCurrentPosition := TdxDocumentModelPosition.Create(PieceTable);
  FCurrentPosition.LogPosition := -1;
  FPrimaryLayoutComplete := False;
  SetCommandEvent(TCommand.PerformPrimaryLayout);

  ResetCalculators;
end;

procedure TdxBackgroundFormatter.ResetPrimaryLayout(const AFrom, ATo: TdxDocumentModelPosition);
begin
  FDocumentFormatter.ParagraphIndex := AFrom.ParagraphIndex - 1;
  FDocumentFormatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
  FCurrentPosition := TdxDocumentModelPosition.FromParagraphStart(PieceTable, AFrom.ParagraphIndex);
  FPrimaryLayoutComplete := False;
  Commands[TCommand.PerformPrimaryLayout].SetEvent;
end;

procedure TdxBackgroundFormatter.ResetSecondaryFormattingForPage(APage: TdxPage; APageIndex: Integer);
begin
  if FResetSecondaryLayoutFromPage >= 0 then
    FResetSecondaryLayoutFromPage := Math.Min(APageIndex, FResetSecondaryLayoutFromPage)
  else
    FResetSecondaryLayoutFromPage := APageIndex;
end;

procedure TdxBackgroundFormatter.RaisePageFormattingComplete(E: TdxPageFormattingCompleteEventArgs);
begin
  if not FOnPageFormattingComplete.Empty then
    FOnPageFormattingComplete.Invoke(Self, E);
end;

procedure TdxBackgroundFormatter.SubscribeToFormattingControllerEvents;
begin
  FController.PageFormattingComplete.Add(OnPageFormattingComplete);
end;

procedure TdxBackgroundFormatter.UnsubscribeFromFormattingControllerEvents;
begin
  FController.PageFormattingComplete.Remove(OnPageFormattingComplete);
end;

procedure TdxBackgroundFormatter.OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
begin
  RaisePageFormattingComplete(E);
end;

procedure TdxBackgroundFormatter.ResetSecondaryLayout;
begin
  ResetCommandEvent(SecondaryLayoutComplete);
  SetCommandEvent(TCommand.PerformSecondaryLayout);
end;


function TdxBackgroundFormatter.ShouldResetPrimaryLayout(const AFrom, ATo: TdxDocumentModelPosition): Boolean;
begin
  Result := (AFrom.LogPosition <= FCurrentPosition.LogPosition) or (ATo.LogPosition <= FCurrentPosition.LogPosition);
end;

function TdxBackgroundFormatter.ShouldResetSecondaryLayout(const AFrom, ATo: TdxDocumentModelPosition): Boolean;
begin
  Result := not ((AFrom.LogPosition > FSecondaryLayoutEnd.LogPosition) or
    (ATo.LogPosition < FSecondaryLayoutStart.LogPosition));
end;

procedure TdxBackgroundFormatter.Start;
begin
  FWorkThread.Start;
end;

function TdxBackgroundFormatter.SuspendWorkThread: Boolean;
begin
  CheckExecutedAtUIThread;
  Inc(FThreadSuspendCount);
  if AlreadySuspended then
    Exit(False);
  Commands[TCommand.None].SetEvent;
  ContinueLayout.WaitFor(Windows.INFINITE);
  Commands[TCommand.None].ResetEvent;
  Result := True;
end;

procedure TdxBackgroundFormatter.ResumeWorkThread;
begin
  CheckExecutedAtUIThread;
  Assert(FThreadSuspendCount > 0);
  Dec(FThreadSuspendCount);
  if FThreadSuspendCount = 0 then
    ContinueLayout.SetEvent;
end;

procedure TdxBackgroundFormatter.UpdateSecondaryPositions(const AFrom, ATo: TdxDocumentModelPosition);
begin
  FSecondaryLayoutStart := AFrom;
  FSecondaryLayoutEnd := ATo;
end;

procedure TdxBackgroundFormatter.WaitForPagePrimaryLayoutComplete(APage: TdxPage);
begin
  CheckExecutedAtUIThread;
  BeginDocumentUpdate;
  try
    while not APage.PrimaryFormattingComplete and not FPrimaryLayoutComplete do
      PerformPrimaryLayoutCore;
  finally
    EndDocumentUpdate;
  end;
end;

procedure TdxBackgroundFormatter.WaitForPrimaryLayoutComplete;
begin
  CheckExecutedAtUIThread;
  BeginDocumentUpdate;
  try
    while not FPrimaryLayoutComplete do
      PerformPrimaryLayoutCore;
  finally
    EndDocumentUpdate;
  end;
end;


procedure TdxBackgroundFormatter.WaitForPrimaryLayoutReachesLogPosition(ALogPosition: TdxDocumentLogPosition);
begin
  CheckExecutedAtUIThread;
  BeginDocumentUpdate;
  try
    while ((FCurrentPosition.LogPosition <= ALogPosition) or
      not IsTableFormattingComplete(FCurrentPosition)) and not FPrimaryLayoutComplete do
      PerformPrimaryLayoutCore;
  finally
    EndDocumentUpdate;
  end;
end;

procedure TdxBackgroundFormatter.WaitForPrimaryLayoutReachesPreferredPage(APreferredPageIndex: Integer);
begin
  CheckExecutedAtUIThread;
  BeginDocumentUpdate;
  try
    while ((DocumentLayout.Pages.Count - 1 <= APreferredPageIndex) or
    not IsTableFormattingComplete(FCurrentPosition)) and not FPrimaryLayoutComplete do
      PerformPrimaryLayoutCore;
  finally
    EndDocumentUpdate;
  end;
end;

function TdxBackgroundFormatter.PredicateDocumentModelPosition(const ACurrentFormatterPosition: TdxDocumentModelPosition): Boolean;
begin
  Result := ACurrentFormatterPosition.LogPosition > FPredicatePos.LogPosition;
end;

procedure TdxBackgroundFormatter.WaitForSecondaryLayoutReachesPosition(const APos: TdxDocumentModelPosition);
var
  AOriginalStartPos, AOriginalEndPos, AStartPos, AEndPos: TdxDocumentModelPosition;
begin
  CheckExecutedAtUIThread;
  BeginDocumentUpdate;
  try
    AOriginalStartPos := SecondaryLayoutStart;
    AOriginalEndPos := SecondaryLayoutEnd;
    if APos.LogPosition < SecondaryLayoutStart.LogPosition then
      AStartPos := APos
    else
      AStartPos := SecondaryLayoutStart;
    if APos.LogPosition > SecondaryLayoutEnd.LogPosition then
      AEndPos := APos
    else
      AEndPos := SecondaryLayoutEnd;
    UpdateSecondaryPositions(AStartPos, AEndPos);
    NotifyDocumentChanged(AStartPos, AEndPos, TdxDocumentLayoutResetType.SecondaryLayout);
  finally
    EndDocumentUpdate;
  end;
  FPredicatePos := APos;
  BeginDocumentRendering(PredicateDocumentModelPosition);
  UpdateSecondaryPositions(AOriginalStartPos, AOriginalEndPos);
  NotifyDocumentChanged(AOriginalStartPos, AOriginalEndPos, TdxDocumentLayoutResetType.SecondaryLayout);
  EndDocumentRendering;
end;

{ TdxCaret }

constructor TdxCaret.Create;
begin
  inherited Create;
  FHidden := True;
end;

procedure TdxCaret.Draw(AGraphics: TdxGraphics);
var
  AHDC: THandle;
begin
  AHDC := AGraphics.GetHDC;
  try
    DrawCore(AHDC);
  finally
    AGraphics.ReleaseHDC(AHDC);
  end;
end;

procedure TdxCaret.Changed;
begin
  FOnChanged.Invoke(Self);
end;

procedure TdxCaret.DrawCore(AHdc: THandle);
var
  ABrush, AOldBrush: HBRUSH;
begin
  ABrush := GetStockObject(WHITE_BRUSH);
  AOldBrush := SelectObject(AHdc, ABrush);
  PatBlt(AHdc, Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height, PATINVERT);
  SelectObject(AHdc, AOldBrush);
end;

function TdxCaret.GetCaretPosition(AView: TdxRichEditView): TdxCaretPosition;
begin
  Result := AView.CaretPosition;
end;

function TdxCaret.ShouldDrawCaret(ADocumentModel: TdxDocumentModel): Boolean;
begin
  Result := ADocumentModel.Selection.Length <= 0;
end;

procedure TdxCaret.SetBounds(const Value: TRect);
begin
  if FBounds.IsEqual(Value) then
    Exit;
  FBounds := Value;
  Changed;
end;

{ TdxDragCaret }

constructor TdxDragCaret.Create(AView: TdxRichEditView);
begin
  inherited Create;
  FPosition := AView.CaretPosition.CreateDragCaretPosition;
end;

destructor TdxDragCaret.Destroy;
begin
  FreeAndNil(FPosition);
  inherited;
end;

procedure TdxDragCaret.DrawCore(AHdc: THandle);
var
  ABrush, AOldBrush: THandle;
  ABounds: TRect;
  AWidth, ADashHeight, Y: Integer;
begin
  ABrush := GetStockObject(WHITE_BRUSH);
  AOldBrush := SelectObject(AHdc, ABrush);
  try
    ABounds := Bounds;
    AWidth := ABounds.Width * 2;
    ADashHeight := Max(AWidth div 2, 1);
    Y := ABounds.Top;
    while Y < ABounds.Bottom do
    begin
      PatBlt(AHdc, ABounds.X, Y, AWidth, ADashHeight, PATINVERT);
      Inc(Y, ADashHeight * 2);
    end;
  finally
    SelectObject(AHdc, AOldBrush);
  end;
end;

function TdxDragCaret.GetCaretPosition(
  AView: TdxRichEditView): TdxCaretPosition;
begin
  Result := FPosition;
end;

procedure TdxDragCaret.SetLogPosition(ALogPosition: TdxDocumentLogPosition);
begin
  FPosition.SetLogPosition(ALogPosition);
end;

function TdxDragCaret.ShouldDrawCaret(
  ADocumentModel: TdxDocumentModel): Boolean;
begin
  Result := True;
end;

{ TdxDragCaretPosition }

function TdxDragCaretPosition.GetLogPosition: Integer;
begin
  Result := FLogPosition;
end;

function TdxDragCaretPosition.GetUsePreviousBoxBounds: Boolean;
begin
  Result := False;
end;

function TdxDragCaretPosition.GetVirtualLogPosition: Integer;
begin
  Result := FLogPosition;
end;

procedure TdxDragCaretPosition.SetLogPosition(
  ALogPosition: TdxDocumentLogPosition);
begin
  if FLogPosition <> ALogPosition then
  begin
    FLogPosition := ALogPosition;
    Invalidate;
  end;
end;

{ TdxExplicitCaretPosition }

constructor TdxExplicitCaretPosition.Create(AView: TdxRichEditView;
  ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
begin
  inherited Create(AView, APreferredPageIndex);
  FLogPosition := Max(0, Min(DocumentModel.MainPieceTable.DocumentEndLogPosition, ALogPosition));
end;

function TdxExplicitCaretPosition.GetLogPosition: Integer;
begin
  Result := FLogPosition;
end;

function TdxExplicitCaretPosition.GetUsePreviousBoxBounds: Boolean;
begin
  Result := False;
end;

function TdxExplicitCaretPosition.GetVirtualLogPosition: Integer;
begin
  Result := FLogPosition;
end;

function TdxExplicitCaretPosition.UpdatePositionTrySetUsePreviousBoxBounds(
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := False;
end;

{ TdxHeaderFooterCaretPosition }

function TdxHeaderFooterCaretPosition.CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterCaretDocumentLayoutPosition.Create(View, PreferredPageIndex);
end;

function TdxHeaderFooterCaretPosition.CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition;
begin
  Result := TdxExplicitHeaderFooterCaretPosition.Create(View, ALogPosition, PreferredPageIndex);
end;

function TdxHeaderFooterCaretPosition.CreateDragCaretPosition: TdxDragCaretPosition;
begin
  Result := TdxHeaderFooterDragCaretPosition.Create(View, PreferredPageIndex);
end;

{ TdxTextBoxCaretPosition }

function TdxTextBoxCaretPosition.CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := TdxTextBoxCaretDocumentLayoutPosition.Create(View, PreferredPageIndex);
end;

function TdxTextBoxCaretPosition.CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition;
begin
  Result := TdxExplicitTextBoxCaretPosition.Create(View, ALogPosition, PreferredPageIndex);
end;

function TdxTextBoxCaretPosition.CreateDragCaretPosition: TdxDragCaretPosition;
begin
  Result := TdxTextBoxDragCaretPosition.Create(View, PreferredPageIndex);
end;

{ TdxExplicitTextBoxCaretPosition }

constructor TdxExplicitTextBoxCaretPosition.Create(AView: TdxRichEditView; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
begin
  inherited Create(AView, APreferredPageIndex);
  FLogPosition := Max(0, Min(DocumentModel.ActivePieceTable.DocumentEndLogPosition, ALogPosition));
end;

function TdxExplicitTextBoxCaretPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := FLogPosition;
end;

function TdxExplicitTextBoxCaretPosition.GetVirtualLogPosition: TdxDocumentLogPosition;
begin
  Result := FLogPosition;
end;

function TdxExplicitTextBoxCaretPosition.GetUsePreviousBoxBounds: Boolean;
begin
  Result := False;
end;

function TdxExplicitTextBoxCaretPosition.UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := False;
end;

{ TdxHeaderFooterDragCaretPosition }

function TdxHeaderFooterDragCaretPosition.CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterCaretDocumentLayoutPosition.Create(View, PreferredPageIndex);
end;

function TdxHeaderFooterDragCaretPosition.CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition;
begin
  Result := TdxExplicitHeaderFooterCaretPosition.Create(View, ALogPosition, PreferredPageIndex);
end;

{ TdxHeaderFooterCaretDocumentLayoutPosition }

constructor TdxHeaderFooterCaretDocumentLayoutPosition.Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
begin
  inherited Create(AView.DocumentLayout, AView.DocumentModel.MainPieceTable, 0, APreferredPageIndex);
  Assert(AView <> nil);
  FView := AView;
end;

function TdxHeaderFooterCaretDocumentLayoutPosition.GetPieceTable: TdxPieceTable;
begin
  Result := FView.DocumentModel.ActivePieceTable;
end;

procedure TdxHeaderFooterCaretDocumentLayoutPosition.EnsureFormattingComplete;
begin
  FView.EnsureFormattingCompleteForPreferredPage(PreferredPageIndex);
end;

procedure TdxHeaderFooterCaretDocumentLayoutPosition.EnsurePageSecondaryFormattingComplete(APage: TdxPage);
begin
  Assert(FView.DocumentLayout.Pages[PreferredPageIndex] = APage);
  FView.EnsurePageSecondaryFormattingComplete(APage);
end;

function TdxHeaderFooterCaretDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterCaretDocumentLayoutPosition.Create(FView, PreferredPageIndex);
end;

{ TdxExplicitHeaderFooterCaretPosition }

constructor TdxExplicitHeaderFooterCaretPosition.Create(AView: TdxRichEditView; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
begin
  inherited Create(AView, APreferredPageIndex);
  FLogPosition := Max(0, Min(DocumentModel.ActivePieceTable.DocumentEndLogPosition, ALogPosition));
end;

function TdxExplicitHeaderFooterCaretPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := FLogPosition;
end;

function TdxExplicitHeaderFooterCaretPosition.GetVirtualLogPosition: TdxDocumentLogPosition;
begin
  Result := FLogPosition;
end;

function TdxExplicitHeaderFooterCaretPosition.GetUsePreviousBoxBounds: Boolean;
begin
  Result := False;
end;

function TdxExplicitHeaderFooterCaretPosition.UpdatePositionTrySetUsePreviousBoxBounds(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := False;
end;

{ TdxTextBoxCaretDocumentLayoutPosition }

constructor TdxTextBoxCaretDocumentLayoutPosition.Create(AView: TdxRichEditView; ATextBoxPieceTable: TdxTextBoxContentType; APreferredPageIndex: Integer);
begin
  inherited Create(AView.DocumentLayout, ATextBoxPieceTable, 0, APreferredPageIndex);
  Assert(AView <> nil);
  FView := AView;
  FTextBoxContentType := ATextBoxPieceTable;
end;

constructor TdxTextBoxCaretDocumentLayoutPosition.Create(AView: TdxRichEditView; APreferredPageIndex: Integer);
begin
  Create(AView, TdxTextBoxContentType(AView.DocumentModel.ActivePieceTable.ContentType), APreferredPageIndex);
end;

function TdxTextBoxCaretDocumentLayoutPosition.GetPieceTable: TdxPieceTable;
begin
  if AnchorPieceTable <> nil then
    Result := AnchorPieceTable
  else
    Result := TdxPieceTable(FTextBoxContentType.PieceTable);
end;

function TdxTextBoxCaretDocumentLayoutPosition.Update(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  if (FTextBoxContentType = nil) or (FTextBoxContentType.AnchorRun = nil) then
    Result := False
  else
    Result := inherited Update(APages, ADetailsLevel);
end;

procedure TdxTextBoxCaretDocumentLayoutPosition.EnsureFormattingComplete;
begin
  FView.EnsureFormattingCompleteForLogPosition(FTextBoxContentType.AnchorRun.Paragraph.EndLogPosition);
end;

function TdxTextBoxCaretDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxTextBoxCaretDocumentLayoutPosition.Create(FView, FTextBoxContentType, PreferredPageIndex);
end;

function TdxTextBoxCaretDocumentLayoutPosition.IsValid(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  if (FTextBoxContentType = nil) or (FTextBoxContentType.AnchorRun = nil) then
    Result := False
  else
    Result := inherited IsValid(ADetailsLevel);
end;

{ TdxTextBoxDragCaretPosition }

function TdxTextBoxDragCaretPosition.CreateCaretDocumentLayoutPosition: TdxDocumentLayoutPosition;
begin
  Result := TdxTextBoxCaretDocumentLayoutPosition.Create(View, PreferredPageIndex);
end;

function TdxTextBoxDragCaretPosition.CreateExplicitCaretPosition(ALogPosition: TdxDocumentLogPosition): TdxCaretPosition;
begin
  Result := TdxExplicitTextBoxCaretPosition.Create(View, ALogPosition, PreferredPageIndex);
end;

{ TdxCopySelectionManager }

procedure TdxCopySelectionManager.CopyDocumentRange(APieceTable: TdxPieceTable;
  ASelectionCollection: TdxSelectionRangeCollection);
var
  ADataObject: IdxDataObject;
  AOldCopyOptions: TdxDefaultPropertiesCopyOptions;
begin
  if (ASelectionCollection.Count <= 0) or (ASelectionCollection[0].Length <= 0) then
    Exit;
  ADataObject := TdxDataObject.Create;
  AOldCopyOptions := DefaultPropertiesCopyOptions;
  try
    SetDataObject(APieceTable, ASelectionCollection, ADataObject);
  finally
    DefaultPropertiesCopyOptions := AOldCopyOptions;
  end;
end;

constructor TdxCopySelectionManager.Create(const ADocumentServer: IdxInnerControl);
begin
  inherited Create;
  FDocumentServer := ADocumentServer;
  FDefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Never;
end;

function TdxCopySelectionManager.CreateDocumentModel(AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
  AFormattingCopyOptions: TdxFormattingCopyOptions; ASourcePieceTable: TdxPieceTable;
  ASelectionRanges: TdxSelectionRangeCollection; ASuppressFieldsUpdate: Boolean = False;
  AGetTextOptions: PdxTextFragmentOptions = nil): TdxDocumentModel;
var
  ATextOptions: TdxTextFragmentOptions;
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
begin
  Result := ASourcePieceTable.DocumentModel.CreateNew;
  if AGetTextOptions <> nil then
    ATextOptions := AGetTextOptions^
  else
    ATextOptions := TdxTextFragmentOptions.Create;
  if ATextOptions.PreserveOriginalNumbering then
  begin
    Result.MainPieceTable.PrecalculatedNumberingListTexts := TDictionary<TdxParagraph, string>.Create;
  end;
  if not ATextOptions.AllowExtendingDocumentRange then
    ASelectionRanges := UpdateSelectionCollection(ASourcePieceTable, ASelectionRanges);
  try
    Result.InternalAPI.ImporterFactory := ASourcePieceTable.DocumentModel.InternalAPI.ImporterFactory;
    Result.InternalAPI.ExporterFactory := ASourcePieceTable.DocumentModel.InternalAPI.ExporterFactory;
    Result.FieldOptions.UpdateFieldsOnPaste := ASourcePieceTable.DocumentModel.FieldOptions.UpdateFieldsOnPaste;
    Result.CalculateDocumentVariable.Add(DoCalculateDocumentVariable);
    Result.BeginUpdate;
    try
      AOptions := TdxDocumentModelCopyOptions.Create(ASelectionRanges);
      try
        AOptions.ParagraphNumerationCopyOptions := AParagraphNumerationCopyOptions;
        AOptions.FormattingCopyOptions := AFormattingCopyOptions;
        AOptions.DefaultPropertiesCopyOptions := DefaultPropertiesCopyOptions;
        ACopyCommand := TdxDocumentModelCopyCommand(ASourcePieceTable.DocumentModel.CreateDocumentModelCopyCommand(ASourcePieceTable,
          Result, AOptions));
        try
          ACopyCommand.FixLastParagraph := FixLastParagraph;
          ACopyCommand.SuppressFieldsUpdate := ASuppressFieldsUpdate;
          ACopyCommand.AllowCopyWholeFieldResult := AllowCopyWholeFieldResult;
          ACopyCommand.Execute;
        finally
          ACopyCommand.Free;
        end;
      finally
        AOptions.Free;
      end;
    finally
      Result.EndUpdate;
    end;
    Result.CalculateDocumentVariable.Remove(DoCalculateDocumentVariable);
  finally
    if not ATextOptions.AllowExtendingDocumentRange then
      ASelectionRanges.Free;
  end;
end;

function TdxCopySelectionManager.GetRtfText(APieceTable: TdxPieceTable;
  ASelectionRanges: TdxSelectionRangeCollection;
  AOptions: TdxRtfDocumentExporterOptions = nil;
  AForceRaiseBeforeExport: Boolean = False;
  AForceRaiseAfterExport: Boolean = False): string;
var
  ATarget: TdxDocumentModel;
  ALastParagraphRunSelected: Boolean;
begin
  try
    ATarget := CreateDocumentModel(TdxParagraphNumerationCopyOptions.CopyIfWholeSelected,
      TdxFormattingCopyOptions.UseDestinationStyles,
      APieceTable, ASelectionRanges, True, nil);
    try
      SubscribeTargetModelEvents(ATarget);
      ALastParagraphRunSelected := IsLastParagraphRunSelected(APieceTable, ASelectionRanges);
      Result := ATarget.InternalAPI.GetDocumentRtfContent(AOptions, not ALastParagraphRunSelected,
        KeepFieldCodeViewState, AForceRaiseBeforeExport, AForceRaiseAfterExport);
      UnsubscribeTargetModelEvents(ATarget);
    finally
      ATarget.Free;
    end;
  except
    Result := '';
  end;
end;

function TdxCopySelectionManager.GetPlainText(APieceTable: TdxPieceTable;
  ASelectionCollection: TdxSelectionRangeCollection; AOptions: TdxPlainTextDocumentExporterOptions = nil;
  AGetTextOptions: PdxTextFragmentOptions = nil): string;
var
  ATarget: TdxDocumentModel;
begin
  try
    ATarget := CreateDocumentModel(TdxParagraphNumerationCopyOptions.CopyAlways,
      TdxFormattingCopyOptions.UseDestinationStyles, APieceTable,
      ASelectionCollection, True, AGetTextOptions);
    try
      SubscribeTargetModelEvents(ATarget);
      Result := ATarget.InternalAPI.GetDocumentPlainTextContent(AOptions);
      UnsubscribeTargetModelEvents(ATarget);
    finally
      ATarget.Free;
    end;
  except
    Result := '';
  end;
end;

function TdxCopySelectionManager.GetOpenXmlBytes(
  APieceTable: TdxPieceTable; ASelectionCollection: TdxSelectionRangeCollection;
  AOptions: TdxOpenXmlDocumentExporterOptions): TBytes;
var
  ATarget: TdxDocumentModel;
begin
  ATarget := CreateDocumentModel(TdxParagraphNumerationCopyOptions.CopyAlways,
    TdxFormattingCopyOptions.UseDestinationStyles, APieceTable, ASelectionCollection);
  try
    SubscribeTargetModelEvents(ATarget);
    try
      Result := ATarget.InternalAPI.GetDocumentOpenXmlContent(AOptions);
    finally
      UnsubscribeTargetModelEvents(ATarget);
    end;
  finally
    ATarget.Free;
  end;
end;

function TdxCopySelectionManager.GetSuppressStoreImageSizeCollection(APieceTable: TdxPieceTable;
  ASelectionCollection: TdxSelectionRangeCollection): string;
var
  ATarget: TdxDocumentModel;
  ARuns: TdxTextRunCollection;
  ASuppressStoreImageScale: TStringBuilder;
  I: Integer;
  APictureRun: TdxInlinePictureRun;
begin
  ASuppressStoreImageScale := TStringBuilder.Create;
  try
    ATarget := CreateDocumentModel(TdxParagraphNumerationCopyOptions.CopyIfWholeSelected,
      TdxFormattingCopyOptions.UseDestinationStyles, APieceTable, ASelectionCollection);
    try
      ARuns := ATarget.ActivePieceTable.Runs;
      for I := 0 to ARuns.Count - 1 do
      begin
        if ARuns[I] is TdxInlinePictureRun then
        begin
          APictureRun := TdxInlinePictureRun(ARuns[I]);
          if APictureRun.Image.SuppressStore then
            ASuppressStoreImageScale.Append(TdxStringHelper.Format('%g,%g', [APictureRun.ScaleX, APictureRun.ScaleY]));
        end;
      end;
      UnsubscribeTargetModelEvents(ATarget);
    finally
      ATarget.Free;
    end;
    Result := ASuppressStoreImageScale.ToString;
  finally
    ASuppressStoreImageScale.Free;
  end;
end;

function TdxCopySelectionManager.IsLastParagraphRunSelected(APieceTable: TdxPieceTable; ASelectionRanges: TdxSelectionRangeCollection): Boolean;
var
  ALastRange: TdxSelectionRange;
  ASelectionRangeIndex: Integer;
  ALastLogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  AParagraph: TdxParagraph;
begin
  ALastRange := ASelectionRanges.Last;
  ASelectionRangeIndex := ASelectionRanges.Count - 1;
  while (ALastRange.Length = 0) and (ASelectionRangeIndex > 0) do
  begin
    Dec(ASelectionRangeIndex);
    ALastRange := ASelectionRanges[ASelectionRangeIndex];
  end;
  if ALastRange.Length = 0 then
    Result := False
  else
  begin
    ALastLogPosition := Max(ALastRange.Start, ALastRange.&End);
    AParagraphIndex := APieceTable.FindParagraphIndexCore(ALastLogPosition);
    AParagraphs := APieceTable.Paragraphs;
    if AParagraphIndex = not AParagraphs.Count then
      Result := True
    else
    begin
      if AParagraphIndex < 0 then
        Result := False
      else
      begin
        AParagraph := AParagraphs[AParagraphIndex];
        Result := (ALastLogPosition = AParagraph.LogPosition) or
          (ALastLogPosition = AParagraph.EndLogPosition + 1);
      end;
    end;
  end;
end;

procedure TdxCopySelectionManager.SetDataObject(APieceTable: TdxPieceTable;
  ASelectionCollection: TdxSelectionRangeCollection; const ADataObject: IdxDataObject);
var
  AOptions: TdxRtfDocumentExporterOptions;
  APlainText: string;
  ARtfText: string;
  ARtf: TArray<Byte>;
  ASuppressStoreImageScale: string;
begin
  ADataObject.Open;
  try
    DefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Always;
    AOptions := TdxRtfDocumentExporterOptions.Create;
    try
      AOptions.ExportFinalParagraphMark := TdxExportFinalParagraphMark.Never;
      ARtfText := GetRtfText(APieceTable, ASelectionCollection, AOptions, True, True);
      if ARtfText <> '' then
      begin
        ARtf := TdxEncoding.ANSI.GetBytes(ARtfText);
        ADataObject.SetData(TdxOfficeDataFormats.Rtf, ARtf);
        ADataObject.SetData(TdxOfficeDataFormats.RtfWithoutObjects, ARtf);
      end;
    finally
      AOptions.Free;
    end;
    APlainText := GetPlainText(APieceTable, ASelectionCollection);
    if APlainText <> '' then
      ADataObject.SetData(TdxOfficeDataFormats.UnicodeText, APlainText);
    ASuppressStoreImageScale := GetSuppressStoreImageSizeCollection(APieceTable, ASelectionCollection);
    if ASuppressStoreImageScale <> '' then
      ADataObject.SetData(TdxOfficeDataFormats.SuppressStoreImageSize, ASuppressStoreImageScale);
  finally
    ADataObject.Close;
  end;
end;

function TdxCopySelectionManager.ShouldExtendRange(AInfo: TdxRunInfo; AField: TdxField): Boolean;
begin
  if AField = nil then
    Exit(False);
  Result := not TdxFieldsOperation.IsFieldCodeTextAffectedOnly(AInfo, AField) and
    not TdxFieldsOperation.IsFieldResultTextAffectedOnly(AInfo, AField) and
    not TdxFieldsOperation.IsEntireFieldAffected(AInfo, AField);
end;

function TdxCopySelectionManager.UpdateSelectionCollection(ASourcePieceTable: TdxPieceTable;
  ASelection: TdxSelectionRangeCollection): TdxSelectionRangeCollection;
var
  I: Integer;
  ARange: TdxSelectionRange;
  AInfo: TdxRunInfo;
  AStart, AEnd: TdxDocumentLogPosition;
  AField: TdxField;
begin
  AllowCopyWholeFieldResult := False;
  Result := TdxSelectionRangeCollection.Create;
  for I := 0 to ASelection.Count - 1 do
  begin
    ARange := ASelection[I];
    AInfo := ASourcePieceTable.FindRunInfo(ARange.Start, ARange.Length);
    try
      AStart := ARange.Start;
      AEnd := ARange.&End;
      AField := ASourcePieceTable.FindFieldByRunIndex(AInfo.Start.RunIndex);
      if ShouldExtendRange(AInfo, AField) then
      begin
        AStart := Max(AStart, ASourcePieceTable.GetRunLogPosition(AField.Result.Start));
        AEnd := Min(AEnd, ASourcePieceTable.GetRunLogPosition(AField.Result.&End));
        if AStart < AEnd then
          Result.Add(TdxSelectionRange.Create(AStart, AEnd - AStart));
        AStart := AEnd + 1;
      end;
      AField := ASourcePieceTable.FindFieldByRunIndex(AInfo.&End.RunIndex);
      if ShouldExtendRange(AInfo, AField) then
      begin
        AEnd := ASourcePieceTable.GetRunLogPosition(AField.Code.Start);
        if AStart >= AEnd then
          Continue;
        Result.Add(TdxSelectionRange.Create(AStart, AEnd - AStart));
        AStart := ASourcePieceTable.GetRunLogPosition(AField.Result.Start);
        AEnd := ARange.&End;
        if AStart < AEnd then
          Result.Add(TdxSelectionRange.Create(AStart, AEnd - AStart));
      end
      else
        Result.Add(TdxSelectionRange.Create(AStart, ARange.&End - AStart));
    finally
      AInfo.Free;
    end;
  end;
end;

procedure TdxCopySelectionManager.DoAfterExport(Sender: TObject);
begin
  DocumentServer.OnAfterExport(Sender);
end;

procedure TdxCopySelectionManager.DoBeforeExport(Sender: TObject; E: TdxBeforeExportEventArgs);
begin
  DocumentServer.OnBeforeExport(Sender, E);
end;

procedure TdxCopySelectionManager.DoCalculateDocumentVariable(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
begin
  DocumentServer.OnCalculateDocumentVariable(sender, e);
end;

procedure TdxCopySelectionManager.SubscribeTargetModelEvents(ATargetModel: TdxDocumentModel);
begin
  ATargetModel.AfterExport.Add(DoAfterExport);
  ATargetModel.BeforeExport.Add(DoBeforeExport);
  ATargetModel.CalculateDocumentVariable.Add(DoCalculateDocumentVariable);
end;

procedure TdxCopySelectionManager.UnsubscribeTargetModelEvents(ATargetModel: TdxDocumentModel);
begin
  ATargetModel.AfterExport.Remove(DoAfterExport);
  ATargetModel.BeforeExport.Remove(DoBeforeExport);
  ATargetModel.CalculateDocumentVariable.Remove(DoCalculateDocumentVariable);
end;

{ TdxRichEditControlOptionsBase }

constructor TdxRichEditControlOptionsBase.Create(const ADocumentServer: IdxRichEditDocumentServer);
begin
  inherited Create;
  FDocumentServer := ADocumentServer;
  SubscribeInnerOptionsEvents;
end;

procedure TdxRichEditControlOptionsBase.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FVerticalRuler := TdxVerticalRulerOptions.Create;
  FHorizontalRuler := TdxHorizontalRulerOptions.Create;
  FHyperlinks := TdxHyperlinkOptions.Create;
end;

destructor TdxRichEditControlOptionsBase.Destroy;
begin
  UnsubscribeInnerOptionsEvents;
  FreeAndNil(FVerticalRuler);
  FreeAndNil(FHorizontalRuler);
  FreeAndNil(FHyperlinks);
  inherited Destroy;
end;

procedure TdxRichEditControlOptionsBase.Assign(Source: TPersistent);
var
  ASource: TdxRichEditControlOptionsBase;
begin
  BeginUpdate;
  try
    if Source is TdxRichEditControlOptionsBase then
    begin
      ASource := TdxRichEditControlOptionsBase(Source);
      ShowHiddenText := ASource.ShowHiddenText;
      Authentication := ASource.Authentication;
      AutoCorrect := ASource.AutoCorrect;
      Behavior := ASource.Behavior;
      Bookmarks := ASource.Bookmarks;
      CopyPaste := ASource.CopyPaste;
      DocumentCapabilities := ASource.DocumentCapabilities;
      DocumentSaveOptions := ASource.DocumentSaveOptions;
      Export := ASource.Export;
      Fields := ASource.Fields;
      FormattingMarkVisibility := ASource.FormattingMarkVisibility;
      HorizontalRuler := ASource.HorizontalRuler;
      Hyperlinks := ASource.Hyperlinks;
      Import := ASource.Import;
      Layout := ASource.Layout;
      MailMerge := ASource.MailMerge;
      Printing := ASource.Printing;
      RangePermissions := ASource.RangePermissions;
      Search := ASource.Search;
      SpellChecker := ASource.SpellChecker;
      TableOptions := ASource.TableOptions;
      VerticalRuler := ASource.VerticalRuler;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxRichEditControlOptionsBase.GetAuthentication: TdxAuthenticationOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.AuthenticationOptions;
end;

function TdxRichEditControlOptionsBase.GetAutoCorrect: TdxAutoCorrectOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.AutoCorrectOptions;
end;

function TdxRichEditControlOptionsBase.GetBehavior: TdxRichEditBehaviorOptions;
begin
  Result := DocumentServer.DocumentModel.BehaviorOptions;
end;

function TdxRichEditControlOptionsBase.GetLayout: TdxRichEditLayoutOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.LayoutOptions;
end;

function TdxRichEditControlOptionsBase.GetMailMerge: TdxRichEditMailMergeOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.MailMergeOptions;
end;

function TdxRichEditControlOptionsBase.GetPrinting: TdxPrintingOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.PrintingOptions;
end;

function TdxRichEditControlOptionsBase.GetRangePermissions: TdxRangePermissionOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.RangePermissionOptions;
end;

function TdxRichEditControlOptionsBase.GetBookmarks: TdxBookmarkOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.BookmarkOptions;
end;

function TdxRichEditControlOptionsBase.GetCopyPaste: TdxCopyPasteOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.CopyPasteOptions;
end;

function TdxRichEditControlOptionsBase.GetDocumentCapabilities: TdxDocumentCapabilitiesOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.DocumentCapabilities;
end;

function TdxRichEditControlOptionsBase.GetDocumentSaveOptions: TdxDocumentSaveOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.DocumentSaveOptions;
end;

function TdxRichEditControlOptionsBase.GetExport: TdxRichEditDocumentExportOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.DocumentExportOptions;
end;

function TdxRichEditControlOptionsBase.GetImport: TdxRichEditDocumentImportOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.DocumentImportOptions;
end;

function TdxRichEditControlOptionsBase.GetFields: TdxFieldOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.FieldOptions;
end;

function TdxRichEditControlOptionsBase.GetFormattingMarkVisibility: TdxFormattingMarkVisibilityOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.FormattingMarkVisibilityOptions;
end;

function TdxRichEditControlOptionsBase.GetTableOptions: TdxTableOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.TableOptions;
end;

function TdxRichEditControlOptionsBase.GetSearch: TdxDocumentSearchOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.SearchOptions;
end;

function TdxRichEditControlOptionsBase.GetShowHiddenText: Boolean;
begin
  Result := FormattingMarkVisibility.ShowHiddenText;
end;

function TdxRichEditControlOptionsBase.GetSpellChecker: TdxSpellCheckerOptions;
begin
  if DocumentServer = nil then
    Result := nil
  else
    Result := DocumentServer.DocumentModel.SpellCheckerOptions;
end;

procedure TdxRichEditControlOptionsBase.SetAuthentication(const Value: TdxAuthenticationOptions);
begin
  Authentication.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetAutoCorrect(const Value: TdxAutoCorrectOptions);
begin
  AutoCorrect.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetBehavior(const Value: TdxRichEditBehaviorOptions);
begin
  Behavior.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetDocumentCapabilities(const Value: TdxDocumentCapabilitiesOptions);
begin
  DocumentCapabilities.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetDocumentSaveOptions(const Value: TdxDocumentSaveOptions);
begin
  DocumentSaveOptions.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetExport(const Value: TdxRichEditDocumentExportOptions);
begin
  Export.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetImport(const Value: TdxRichEditDocumentImportOptions);
begin
  Import.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetFields(const Value: TdxFieldOptions);
begin
  Fields.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetFormattingMarkVisibility(const Value: TdxFormattingMarkVisibilityOptions);
begin
  FormattingMarkVisibility.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetHorizontalRuler(const Value: TdxHorizontalRulerOptions);
begin
  FHorizontalRuler.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetHyperlinks(const Value: TdxHyperlinkOptions);
begin
  FHyperlinks.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetTableOptions(const Value: TdxTableOptions);
begin
  TableOptions.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetLayout(const Value: TdxRichEditLayoutOptions);
begin
  Layout.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetMailMerge(const Value: TdxRichEditMailMergeOptions);
begin
  MailMerge.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetPrinting(const Value: TdxPrintingOptions);
begin
  Printing.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetRangePermissions(const Value: TdxRangePermissionOptions);
begin
  RangePermissions.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetBookmarks(const Value: TdxBookmarkOptions);
begin
  Bookmarks.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetCopyPaste(const Value: TdxCopyPasteOptions);
begin
  CopyPaste.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetSearch(const Value: TdxDocumentSearchOptions);
begin
  Search.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetShowHiddenText(const Value: Boolean);
begin
  if ShowHiddenText <> Value then
  begin
    FormattingMarkVisibility.ShowHiddenText := Value;
    DoChanged(TAction.ShowHiddenText);
  end;
end;

procedure TdxRichEditControlOptionsBase.SetSpellChecker(const Value: TdxSpellCheckerOptions);
begin
  SpellChecker.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SetVerticalRuler(const Value: TdxVerticalRulerOptions);
begin
  FVerticalRuler.Assign(Value);
end;

procedure TdxRichEditControlOptionsBase.SubscribeInnerOptionsEvents;
begin
  if DocumentSaveOptions <> nil then
    DocumentSaveOptions.Changed.Add(DoInnerOptionsChanged);
  if Fields <> nil then
    Fields.Changed.Add(DoInnerOptionsChanged);
  if MailMerge <> nil then
    MailMerge.Changed.Add(DoInnerOptionsChanged);
  if DocumentCapabilities <> nil then
    DocumentCapabilities.Changed.Add(DoInnerOptionsChanged);
  if Behavior <> nil then
    Behavior.Changed.Add(DoInnerOptionsChanged);
  if Bookmarks <> nil then
    Bookmarks.Changed.Add(DoInnerOptionsChanged);
  if Search <> nil then
    Search.Changed.Add(DoInnerOptionsChanged);
  if FormattingMarkVisibility <> nil then
    FormattingMarkVisibility.Changed.Add(DoInnerOptionsChanged);
  if Hyperlinks <> nil then
    Hyperlinks.Changed.Add(DoInnerOptionsChanged);
  if VerticalRuler <> nil then
    VerticalRuler.Changed.Add(DoInnerOptionsChanged);
  if HorizontalRuler <> nil then
    HorizontalRuler.Changed.Add(DoInnerOptionsChanged);
  if Authentication <> nil then
    Authentication.Changed.Add(DoInnerOptionsChanged);
  if Layout <> nil then
    Layout.Changed.Add(DoInnerOptionsChanged);
  if Printing <> nil then
    Printing.Changed.Add(DoInnerOptionsChanged);
  if RangePermissions <> nil then
    RangePermissions.Changed.Add(DoInnerOptionsChanged);
  if SpellChecker <> nil then
    SpellChecker.Changed.Add(DoInnerOptionsChanged);
end;

procedure TdxRichEditControlOptionsBase.UnsubscribeInnerOptionsEvents;
begin
  if DocumentSaveOptions <> nil then
    DocumentSaveOptions.Changed.Remove(DoInnerOptionsChanged);
  if Fields <> nil then
    Fields.Changed.Remove(DoInnerOptionsChanged);
  if MailMerge <> nil then
    MailMerge.Changed.Remove(DoInnerOptionsChanged);
  if DocumentCapabilities <> nil then
    DocumentCapabilities.Changed.Remove(DoInnerOptionsChanged);
  if Behavior <> nil then
    Behavior.Changed.Remove(DoInnerOptionsChanged);
  if Bookmarks <> nil then
    Bookmarks.Changed.Remove(DoInnerOptionsChanged);
  if Search <> nil then
    Search.Changed.Remove(DoInnerOptionsChanged);
  if FormattingMarkVisibility <> nil then
    FormattingMarkVisibility.Changed.Remove(DoInnerOptionsChanged);
  if Hyperlinks <> nil then
    Hyperlinks.Changed.Remove(DoInnerOptionsChanged);
  if VerticalRuler <> nil then
    VerticalRuler.Changed.Remove(DoInnerOptionsChanged);
  if HorizontalRuler <> nil then
    HorizontalRuler.Changed.Remove(DoInnerOptionsChanged);
  if Authentication <> nil then
    Authentication.Changed.Remove(DoInnerOptionsChanged);
  if Layout <> nil then
    Layout.Changed.Remove(DoInnerOptionsChanged);
  if Printing <> nil then
    Printing.Changed.Remove(DoInnerOptionsChanged);
  if RangePermissions <> nil then
    RangePermissions.Changed.Remove(DoInnerOptionsChanged);
  if SpellChecker <> nil then
    SpellChecker.Changed.Remove(DoInnerOptionsChanged);
end;


end.
