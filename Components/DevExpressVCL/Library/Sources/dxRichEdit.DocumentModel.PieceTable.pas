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

unit dxRichEdit.DocumentModel.PieceTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Controls, Generics.Defaults, Generics.Collections,
  RegularExpressions, Rtti,
  dxCrypto, dxCore, dxCoreClasses, cxClasses, cxGeometry, dxCoreGraphics,
  cxGraphics, dxCultureInfo, dxSpellCheckerCore, dxProtectionUtils,

  dxGenerics,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DataController,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Token,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.WidthsContentInfo,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.PieceTableModifiers.Core,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.PieceTableRange,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableCellsManager,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.Control.Hyphenations,
  dxRichEdit.Export.Core,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentLayout.CommentPadding;

type
  TdxDocumentModel = class;
  TdxMeasurementAndDrawingStrategy = class;
  TdxParagraph = class;
  TdxParagraphCollection = class;
  TdxPieceTable = class;
  TdxPieceTableList = class;
  TdxSelection = class;
  TdxSelectionItem = class;
  TdxSelectionItemList = class;
  TdxRichEditSpellCheckerManager = class;

  TdxCustomInternalAPI = class;

  TdxCustomMark = class;

  TdxRichEditCommandsCreationStrategy = class;

  IdxInnerRichEditDocumentServerOwner = interface(IdxInnerRichEditDocumentContainerOwner)
  ['{0513020F-29EA-4D34-B3F4-9004ED3C4493}']
    function GetControl: TWinControl;
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer{IdxRichEditDocumentServer};
    function CreateMeasurementAndDrawingStrategy(ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
    procedure RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions);

    property Control: TWinControl read GetControl;
  end;

  { TdxLastInsertedFloatingObjectAnchorRunInfo }

  TdxLastInsertedFloatingObjectAnchorRunInfo = class(TdxLastInsertedRunInfoBase);

  { TdxSelectedCellsIntervalInRow }

  TdxSelectedCellsIntervalInRow = class
  strict private
    FRow: TdxTableRow;
    FStartCellIndex: Integer;
    FEndCellIndex: Integer;
    function GetNormalizedStartCellIndex: Integer;
    function GetNormalizedEndCellIndex: Integer;
    function GetNormalizedStartCell: TdxTableCell;
    function GetNormalizedEndCell: TdxTableCell;
    function GetStartCell: TdxTableCell;
    function GetEndCell: TdxTableCell;
    function GetIsContainsOnlyOneCell: Boolean;
    function GetLeftCell: TdxTableCell;
    function GetNormalizedLength: Integer;
    function GetTable: TdxTable;
  public
    constructor Create(ARow: TdxTableRow; AStartCellIndex: Integer;
      AEndCellIndex: Integer);
    function GetNormalizedColumnSpan: Integer;
    function ContainsCell(ACell: TdxTableCell): Boolean; virtual;

    property Row: TdxTableRow read FRow;
    property NormalizedStartCellIndex: Integer read GetNormalizedStartCellIndex;
    property NormalizedEndCellIndex: Integer read GetNormalizedEndCellIndex;
    property NormalizedStartCell: TdxTableCell read GetNormalizedStartCell;
    property NormalizedEndCell: TdxTableCell read GetNormalizedEndCell;
    property StartCellIndex: Integer read FStartCellIndex write FStartCellIndex;
    property EndCellIndex: Integer read FEndCellIndex write FEndCellIndex;
    property StartCell: TdxTableCell read GetStartCell;
    property EndCell: TdxTableCell read GetEndCell;
    property IsContainsOnlyOneCell: Boolean read GetIsContainsOnlyOneCell;
    property LeftCell: TdxTableCell read GetLeftCell;
    property NormalizedLength: Integer read GetNormalizedLength;
    property Table: TdxTable read GetTable;
  end;
  TdxSelectedCellsIntervalInRowList = class(TdxObjectList<TdxSelectedCellsIntervalInRow>);

  TdxCommentContentType = Pointer;


  { TdxFieldsOperation }

  TdxFieldsOperation = class abstract
  public
    class function IsEntireFieldAffected(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
    class function IsFieldCodeTextAffectedOnly(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
    class function IsFieldResultTextAffectedOnly(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
    class function IsEntireFieldResultAffected(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
    class procedure EnsureIntervalContainsField(AInterval: TdxRunInfo; AField: TdxField);
  end;

  { TdxCopyFieldsOperation }

  TdxCopyFieldsOperation = class(TdxFieldsOperation)
  public type
    TdxUpdateFieldsParameters = class
    private
      FFields: TdxFieldList;
      FFieldsToDelete: TdxFieldList;
      FParent: TdxField;
    public
      constructor Create(const AFields, AFieldsToDelete: TdxFieldList; AParent: TdxField);
      destructor Destroy; override;

      property Fields: TdxFieldList read FFields;
      property FieldsToDelete: TdxFieldList read FFieldsToDelete;
      property Parent: TdxField read FParent;
    end;
  strict private
    FAllowCopyWholeFieldResult: Boolean;
    FSourcePieceTable: TdxPieceTable;
    FSuppressFieldsUpdate: Boolean;
    FTargetPieceTable: TdxPieceTable;
    FUpdateFieldsParameters: TdxUpdateFieldsParameters;
    FUpdateFieldOperationType: TdxUpdateFieldOperationType;
    function CalculateCopingFields(AOriginFields: TdxFieldList; ARunOffset: Integer): TdxFieldList;
    procedure CalculateFieldsHierarchy(AOriginFields, ANewFields: TdxFieldList; AParentFieldIndex: Integer);
    procedure ChangeRunInfo(ARunInfo: TdxRunInfo; AField: TdxField);
    function GetIndexToInsert(AParentFieldIndex: Integer; ARunIndex: TdxRunIndex): Integer;

    function GetSourceModel: TdxDocumentModel;
    function GetTargetModel: TdxDocumentModel;
  protected
    function ShouldExtendInterval(ARunInfo: TdxRunInfo; AField: TdxField): Boolean; virtual;
  public
    constructor Create(ASourcePieceTable, ATargetPieceTable: TdxPieceTable);
    destructor Destroy; override;

    procedure Execute(ARunInfo: TdxRunInfo; ARunIndex: TdxRunIndex);
    procedure RecalculateRunInfo(ARunInfo: TdxRunInfo);
    procedure UpdateCopiedFields;

    property AllowCopyWholeFieldResult: Boolean read FAllowCopyWholeFieldResult write FAllowCopyWholeFieldResult;
    property SourcePieceTable: TdxPieceTable read FSourcePieceTable;
    property SourceModel: TdxDocumentModel read GetSourceModel;
    property SuppressFieldsUpdate: Boolean read FSuppressFieldsUpdate write FSuppressFieldsUpdate;
    property TargetPieceTable: TdxPieceTable read FTargetPieceTable;
    property TargetModel: TdxDocumentModel read GetTargetModel;
    property UpdateFieldOperationType: TdxUpdateFieldOperationType read FUpdateFieldOperationType write FUpdateFieldOperationType;
  end;

  { TdxCalculateDocumentVariableEventRouter }

  TdxCalculateDocumentVariableEventRouter = class
  strict private
    FTargetModel: TdxDocumentModel;
  public
    constructor Create(ATargetModel: TdxDocumentModel);
    procedure OnCalculateDocumentVariable(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
  end;

  { TdxExtendedDocumentProperties }

  TdxExtendedDocumentProperties = class
  private
    FPages: Integer;
  protected
    procedure SetPages(APages: Integer);
  public
    property Pages: Integer read FPages write SetPages;
  end;

  { TdxDocumentModelDeferredChanges }

  TdxDocumentModelDeferredChanges = class(TdxSimpleDocumentModelDeferredChanges, IdxDocumentModelStructureChangedListener)
  private
    FAdditionalChangedPieceTables: TdxPieceTableList;
    FChangeActions: TdxDocumentModelChangeActions;
    FChangeStart: TdxDocumentModelPosition;
    FChangeEnd: TdxDocumentModelPosition;
    FStartAnchor: TdxDocumentModelPositionAnchor;
    FEndAnchor: TdxDocumentModelPositionAnchor;
    FOriginalSelectionUsePreviousBoxBounds: Boolean;
    FSelectionChanged: Boolean;
    FSuppressClearOutdatedSelectionItems: Boolean;
    FEnsureCaretVisible: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetSelectionChanged: Boolean;
  protected
    procedure ResetSelectionChanged; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;
    procedure ApplyChanges(APieceTable: TdxPieceTable; AActions: TdxDocumentModelChangeActions;
      AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
    procedure AddAdditionalChangedPieceTable(AAdditionalChangedPieceTable: TdxPieceTable);
    procedure RegisterSelectionChanged;
    function ShouldUpdatePositions(const AActions: TdxDocumentModelChangeActions): Boolean;

    procedure SetChangeParagraphStart(AParagraphIndex: TdxParagraphIndex);
    procedure SetChangeParagraphEnd(AParagraphIndex: TdxParagraphIndex);

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
  {$ENDREGION}

    property AdditionalChangedPieceTables: TdxPieceTableList read FAdditionalChangedPieceTables;
    property ChangeActions: TdxDocumentModelChangeActions read FChangeActions write FChangeActions;
    property ChangeStart: TdxDocumentModelPosition read FChangeStart;
    property ChangeEnd: TdxDocumentModelPosition read FChangeEnd;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property EnsureCaretVisible: Boolean read FEnsureCaretVisible write FEnsureCaretVisible;
    property EndAnchor: TdxDocumentModelPositionAnchor read FEndAnchor;
    property SelectionChanged: Boolean read GetSelectionChanged;
    property SuppressClearOutdatedSelectionItems: Boolean read FSuppressClearOutdatedSelectionItems write FSuppressClearOutdatedSelectionItems;
    property StartAnchor: TdxDocumentModelPositionAnchor read FStartAnchor;
  end;


  { TdxDocumentModelCopyOptions }

  TdxDocumentModelCopyOptions = class
  strict private
    FCopyDocumentVariables: Boolean;
    FSelectionRanges: TdxSelectionRangeCollection;
    FOwnSelectionRanges: Boolean;
    FParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
    FDefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions;
    FFormattingCopyOptions: TdxFormattingCopyOptions;
  private
    function GetFrom: TdxDocumentLogPosition;
    function GetLength: Integer;
    procedure SetFrom(const Value: TdxDocumentLogPosition);
    procedure SetLength(const Value: Integer);
  public
    constructor Create(ASelectionRanges: TdxSelectionRangeCollection); overload;
    constructor Create(AFrom: TdxDocumentLogPosition; ALength: Integer); overload;
    destructor Destroy; override;

    property CopyDocumentVariables: Boolean read FCopyDocumentVariables write FCopyDocumentVariables;
    property DefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions read FDefaultPropertiesCopyOptions write FDefaultPropertiesCopyOptions;
    property FormattingCopyOptions: TdxFormattingCopyOptions read FFormattingCopyOptions write FFormattingCopyOptions;
    property From: TdxDocumentLogPosition read GetFrom write SetFrom;
    property Length: Integer read GetLength write SetLength;
    property ParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions read FParagraphNumerationCopyOptions write FParagraphNumerationCopyOptions;
    property SelectionRanges: TdxSelectionRangeCollection read FSelectionRanges;
  end;

  { TdxMainContentType }

  TdxMainContentType = class(TdxSimpleMainContentType)
  public
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); override;
    procedure SetPageCount(APageCount: Integer); override;
  end;

  { TdxHyperlinkInfoEvent }

  TdxHyperlinkInfoEventArgs = class(TdxEventArgs)
  strict private
    FPieceTable: TdxCustomPieceTable;
    FFieldIndex: Integer;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property FieldIndex: Integer read FFieldIndex;
  end;

  TdxHyperlinkInfoEvent = procedure(Sender: TObject; E: TdxHyperlinkInfoEventArgs) of object;
  TdxHyperlinkInfoEventHandler = TdxMulticastMethod<TdxHyperlinkInfoEvent>;

  { TdxDocumentUpdateCompleteEventArgs }

  TdxDocumentUpdateCompleteEventArgs = class(TdxEventArgs)
  strict private
    FDeferredChanges: TdxDocumentModelDeferredChanges;
  public
    constructor Create(ADeferredChanges: TdxDocumentModelDeferredChanges);

    property DeferredChanges: TdxDocumentModelDeferredChanges read FDeferredChanges;
  end;

  { TdxDocumentUpdateCompleteEventHandler }

  TdxDocumentUpdateCompleteEvent = procedure (ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs) of object;

  TdxDocumentUpdateCompleteEventHandler = TdxMulticastMethod<TdxDocumentUpdateCompleteEvent>;

  { TdxSafeDocumentModelEditor }

  TdxSafeDocumentModelEditor = class
  strict private
    FDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function GetUnsafeEditor: TdxUnsafeDocumentModelEditor;
  protected
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    procedure InsertSectionCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition); overload;
    procedure InsertSectionCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); overload;
    procedure PerformInsertSectionCore(AParagraphIndex: TdxParagraphIndex); virtual;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property UnsafeEditor: TdxUnsafeDocumentModelEditor read GetUnsafeEditor;
  end;

  { TdxBufferedRegexSearchResult }

  TdxBufferedRegexSearchResult = class
  strict private
    FRegEx: TRegex;
    FMatch: TMatch;
    FOffset: TdxDocumentModelPosition;
    FAbsolutePosition: Integer;
  public
    constructor Create(const ARegEx: TRegex; const AMatch: TMatch; const AOffset: TdxDocumentModelPosition);

    property Match: TMatch read FMatch;
    property Offset: TdxDocumentModelPosition read FOffset;
    property AbsolutePosition: Integer read FAbsolutePosition write FAbsolutePosition;
  end;

  { TdxSearchContext }

  TdxSearchContext = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  strict private const
    LogicalActionsTable: array[TdxSearchAction] of array[TdxDirection] of TdxSearchLogicalAction =
      ((TdxSearchLogicalAction.FindReplaceForward, TdxSearchLogicalAction.FindReplaceBackward),
       (TdxSearchLogicalAction.FindReplaceForward, TdxSearchLogicalAction.FindReplaceBackward),
       (TdxSearchLogicalAction.ReplaceAllForward, TdxSearchLogicalAction.ReplaceAllBackward));
  strict private
    FSearchInfo: TdxSearchContextInfo;
    FMatchInfo: TdxBufferedRegexSearchResult;
    FStartSelectionPosition: TdxDocumentModelPositionAnchor;
    FEndSelectionPosition: TdxDocumentModelPositionAnchor;
    FSearchOffset: Integer;
    FPieceTable: TdxPieceTable;
    FLogicalAction: TdxSearchLogicalAction;
    FAction: TdxSearchAction;
    FDirection: TdxDirection;
    FSuspendCount: Integer;
    FStartPos: TdxDocumentModelPosition;
    FEndPos: TdxDocumentModelPosition;
    function GetDocumentModel: TdxDocumentModel;
    function GetIsExecuteLocked: Boolean;
    function GetStartOfSearch: Boolean;
    function GetEndOfSearch: Boolean;
    function GetMatchCount: Integer;
    function GetReplaceCount: Integer;
    function GetStartOfIntervalSearch: Boolean;
    function GetSearchState: TdxSearchState;
    function GetSearchScope: TdxSearchScope;
    function GetStartAt: TdxDocumentLogPosition;
    function GetStartSelection: TdxDocumentLogPosition;
    function GetEndSelection: TdxDocumentLogPosition;
    procedure SetMatchCount(const Value: Integer);
    procedure SetReplaceCount(const Value: Integer);
    procedure SetStartOfIntervalSearch(const Value: Boolean);
    procedure SetSearchState(const Value: TdxSearchState);
    procedure SetSearchScope(const Value: TdxSearchScope);
    procedure SetStartAt(const Value: TdxDocumentLogPosition);
    procedure SetMatchInfo(const Value: TdxBufferedRegexSearchResult);
  protected
    function GetSearchInfo: TdxSearchContextInfo; virtual;
    procedure EnableHandling;
    procedure DisableHandling;
    procedure OnSelectionChanged(ASender: TObject);
    procedure ClearCore;
    procedure ResetSearchInfo;
    procedure Initialize;
  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset: Integer; ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
  {$ENDREGION}

    property SearchOffset: Integer read FSearchOffset write FSearchOffset;
    property SearchInfo: TdxSearchContextInfo read GetSearchInfo;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;
    procedure BeginSearch(AAction: TdxSearchAction; ADirection: TdxDirection); virtual;
    function CreateEventArgs(const ASearchString: string; const AReplaceString: string): TdxSearchCompleteEventArgs;
    procedure EndSearch; virtual;
    procedure StopSearching; virtual;
    procedure Clear; virtual;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
    property Action: TdxSearchAction read FAction;
    property Direction: TdxDirection read FDirection;
    property LogicalAction: TdxSearchLogicalAction read FLogicalAction;
    property IsExecuteLocked: Boolean read GetIsExecuteLocked;
    property StartOfSearch: Boolean read GetStartOfSearch;
    property EndOfSearch: Boolean read GetEndOfSearch;
    property MatchCount: Integer read GetMatchCount write SetMatchCount;
    property ReplaceCount: Integer read GetReplaceCount write SetReplaceCount;
    property StartOfIntervalSearch: Boolean read GetStartOfIntervalSearch write SetStartOfIntervalSearch;
    property SearchState: TdxSearchState read GetSearchState write SetSearchState;
    property SearchScope: TdxSearchScope read GetSearchScope write SetSearchScope;
    property StartAt: TdxDocumentLogPosition read GetStartAt write SetStartAt;
    property StartSelection: TdxDocumentLogPosition read GetStartSelection;
    property StartSelectionAnchor: TdxDocumentModelPositionAnchor read FStartSelectionPosition write FStartSelectionPosition;
    property EndSelectionAnchor: TdxDocumentModelPositionAnchor read FEndSelectionPosition write FEndSelectionPosition;
    property EndSelection: TdxDocumentLogPosition read GetEndSelection;
    property MatchInfo: TdxBufferedRegexSearchResult read FMatchInfo write SetMatchInfo;
  end;

  { TdxDocumentModel }

  TdxDocumentModel = class(TdxSimpleDocumentModel,
    IdxDocumentModelStructureChangedListener,
    IdxCellPropertiesOwner,
    IdxTableStylesContainer,
    IdxDefaultTablePropertiesContainer,
    IdxDocumentEncryptionPropertiesContainer)
  strict private
    class var FDefaultUnderlineRepository: TdxUnderlineRepository;
    class var FDefaultStrikeoutRepository: TdxStrikeoutRepository;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FEmptyTopBorder: TdxTopBorder;
    FEmptyBottomBorder: TdxBottomBorder;
    FUnderlineRepository: TdxUnderlineRepository;
    FBorderLineRepository: TdxBorderLineRepository;
    FStrikeoutRepository: TdxStrikeoutRepository;
    FFieldResultModel: Boolean;
    FIntermediateModel: Boolean;
    FShouldApplyAppearanceProperties: Boolean;
    FTableStyles: TdxTableStyleCollection;
    FTableCellStyles: TdxTableCellStyleCollection;
    FNumberingListStyles: TdxNumberingListStyleCollection;
    FDefaultTableProperties: TdxTableProperties;
    FDefaultTableRowProperties: TdxTableRowProperties;
    FDefaultTableCellProperties: TdxTableCellProperties;
    FNumberDecimalSeparator: string;
    FNumberGroupSeparator: string;
    FActivePieceTable: TdxPieceTable;
    FActiveSectionIndex: TdxSectionIndex;
    FLineNumberRun: TdxLineNumberCommonRun;
    FHeaders: TdxHeaderCollection;
    FFooters: TdxFooterCollection;

    FAbstractNumberingListIdProvider: TdxAbstractNumberingListIdProvider;
    FAbstractNumberingLists: TdxAbstractNumberingListCollection;
    FNumberingListIdProvider: TdxNumberingListIdProvider;
    FNumberingLists: TdxNumberingListCollection;

    FLastInsertedFloatingObjectAnchorRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo;


    FCopyPasteOptions: TdxCopyPasteOptions;
    FEditingOptions: TdxRichEditEditingOptions;
    FDocumentSaveOptions: TdxDocumentSaveOptions;
    FLayoutOptions: TdxRichEditLayoutOptions;
    FFieldOptions: TdxFieldOptions;
    FMailMergeOptions: TdxRichEditMailMergeOptions;
    FDocumentImportOptions: TdxRichEditDocumentImportOptions;
    FDocumentExportOptions: TdxRichEditDocumentExportOptions;
    FBookmarkOptions: TdxBookmarkOptions;
    FRangePermissionOptions: TdxRangePermissionOptions;
    FSearchOptions: TdxDocumentSearchOptions;
    FSpellCheckerOptions: TdxSpellCheckerOptions;

    FAuthenticationOptions: TdxAuthenticationOptions;
    FTableOptions: TdxTableOptions;
    FAutoCorrectOptions: TdxAutoCorrectOptions;
    FBehaviorOptions: TdxRichEditBehaviorOptions;
    FPrintingOptions: TdxPrintingOptions;
    FDocumentProperties: TdxDocumentProperties;
    FExtendedDocumentProperties: TdxExtendedDocumentProperties;

    FSafeEditor: TdxSafeDocumentModelEditor;
    FInternalAPI: TdxCustomInternalAPI;
    FCommandsCreationStrategy: TdxRichEditCommandsCreationStrategy;

    FDocumentFileName: string;
    FDocumentFormat: TdxRichEditDocumentFormat;
    FMailMergeProperties: TdxMailMergeProperties;
    FProtectionProperties: TdxDocumentProtectionProperties;
    FEncryptionProperties: TdxDocumentEncryptionProperties;
    FSearchParameters: TdxSearchParameters;
    FSearchContext: TdxSearchContext;

    FMailMergeDataController: TdxRichEditDataControllerAdapterBase;
    FTableBorderInfoRepository: TdxBorderInfoRepository;
    FFloatingObjectBorderInfoRepository: TdxBorderInfoRepository;
    FVariables: TdxDocumentVariableCollection;
    FFootNotes: TdxFootNoteCollection;
    FEndNotes: TdxEndNoteCollection;
    FSyntaxHighlightSuspendCount: Integer;
    FSuppressFieldsChangeNotification: Boolean;
    FIsLastSelectionInEmptySpecialParagraph: Boolean;
    FSpecialEmptyParagraphRunIndex: TdxRunIndex;
    FOnBeforeExport: TdxBeforeExportEventHandler;
    FOnAfterExport: TdxNotifyEventHandler;
    FOnBeforeImport: TdxBeforeImportEventHandler;
    FOnBeginDocumentUpdate: TdxEventHandler;
    FOnCalculateDocumentVariable: TdxCalculateDocumentVariableEventHandler;
    FOnDocumentCleared: TdxEventHandler;
    FOnEndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler;
    FOnBeforeEndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler;
    FOnAfterEndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler;
    FOnModifiedChanged: TdxEventHandler;
    FOnInnerContentChanged: TdxEventHandler;
    FOnInnerDocumentCleared: TdxEventHandler;
    FOnInvalidFormatException: TdxRichEditInvalidFormatExceptionEventHandler;
    FOnContentChanged: TdxEventHandler;
    FOnPageBackgroundChanged: TdxEventHandler;
    FOnSectionInserted: TdxSectionEventHandler;
    FOnSectionRemoved: TdxSectionEventHandler;
    FOnHyperlinkInfoDeleted: TdxHyperlinkInfoEventHandler;
    FOnHyperlinkInfoInserted: TdxHyperlinkInfoEventHandler;
    FOnMailMergeFinished: TdxMailMergeFinishedEventHandler;
    FOnMailMergeRecordStarted: TdxMailMergeRecordStartedEventHandler;
    FOnMailMergeRecordFinished: TdxMailMergeRecordFinishedEventHandler;
    FOnMailMergeStarted: TdxMailMergeStartedEventHandler;
    FOnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEventHandler;
    FOnEncryptionPasswordQuery: TdxEncryptionPasswordQueryEvent;

    FHtmlSettings: TdxWebSettings;
    FDisableCheckDocumentModelIntegrity: Boolean;

  {$REGION 'IdxTableStylesContainer'}
    function GetTableStyles: TdxTableStyleCollection;
    function GetTableCellStyles: TdxTableCellStyleCollection;
  {$ENDREGION}
  {$REGION 'IdxDocumentEncryptionPropertiesContainer'}
    function GetEncryptionProperties: TdxDocumentEncryptionProperties;
  {$ENDREGION}
  {$REGION 'IdxDefaultTablePropertiesContainer'}
    function GetDefaultTableProperties: TdxTableProperties;
    function GetDefaultTableCellProperties: TdxTableCellProperties;
    function GetDefaultTableRowProperties: TdxTableRowProperties;
  {$ENDREGION}

    procedure RestoreInvalidNumberingListStyles;
    procedure EnsureNumberingListStyleValid(AStyle: TdxNumberingListStyle; AMaxListIndex: TdxNumberingListIndex);

    function GetCache: TdxDocumentCache;
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    function GetDeferredChanges: TdxDocumentModelDeferredChanges;
    function GetDocumentCapabilities: TdxDocumentCapabilitiesOptions;
    function GetFormattingMarkVisibilityOptions: TdxFormattingMarkVisibilityOptions;
    function GetIsEmpty: Boolean;
    function GetPieceTable: TdxPieceTable;
    function GetSections: TdxSectionCollection;
    function GetSelection: TdxSelection;
    function GetSyntaxHighlightSuspended: Boolean;
    function GetEmptyBottomBorder: TdxBottomBorder;
    function GetEmptyTopBorder: TdxTopBorder;
    procedure SetDocumentCapabilities(const Value: TdxDocumentCapabilitiesOptions);
  protected
    constructor Create(ADpiX, ADpiY: Single); overload;

    function GetSeparateModelForApiExport: Boolean;
    function GetMainPart: TdxCustomPieceTable; override;
    function GetIsDocumentProtectionEnabled: Boolean; override;{$IFNDEF CPUX64} final;{$ENDIF}

    procedure RaisePageBackgroundChanged;

    procedure OnMailMergeCurrentRowChanged(Sender: TObject; E: TdxEventArgs);
    procedure OnMailMergeViewMergedDataChanged(Sender: TObject; E: TdxEventArgs);
    procedure OnMailMergeDataSourceChanged(Sender: TObject; E: TdxEventArgs);

    procedure ClearCore; override;
    procedure DisposeCore; override;{$IFNDEF CPUX64} final;{$ENDIF}

    function CreateAuthenticationOptions: TdxAuthenticationOptions;
    function CreateAutoCorrectOptions: TdxAutoCorrectOptions;
    function CreateBehaviorOptions: TdxRichEditBehaviorOptions;
    function CreateBookmarkOptions: TdxBookmarkOptions;
    function CreateCopyPasteOptions: TdxCopyPasteOptions;
    function CreateDocumentCapabilitiesOptions: TdxCustomDocumentCapabilitiesOptions; override;
    function CreateDocumentExportOptions: TdxRichEditDocumentExportOptions;
    function CreateDocumentImportOptions: TdxRichEditDocumentImportOptions;
    function CreateDocumentSaveOptions: TdxDocumentSaveOptions;
    function CreateEditingOptions: TdxRichEditEditingOptions;
    function CreateFieldOptions: TdxFieldOptions;
    function CreateFormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions; override;
    function CreateLayoutOptions: TdxRichEditLayoutOptions;
    function CreateMailMergeOptions: TdxRichEditMailMergeOptions;
    function CreatePrintingOptions: TdxPrintingOptions;
    function CreateRangePermissionOptions: TdxRangePermissionOptions;
    function CreateSearchOptions: TdxDocumentSearchOptions;
    function CreateSpellCheckerOptions: TdxSpellCheckerOptions;
    function CreateTableOptions: TdxTableOptions;

    function CreateDeferredChanges: TdxCustomDocumentModelDeferredChanges; override;
    function CreateSectionCollection: TdxCustomSectionCollection; override;

    procedure BeginClearDocument; override;
    procedure EndClearDocument; override;
    procedure ClearDocumentContent; override;
    procedure ClearDocumentDefaultPropertiesCore; override;
    procedure ClearDocumentProperties; override;
    procedure ClearDocumentStyles; override;

    procedure CreateOptions; override;
    procedure CreateDocumentObjects; override;
    procedure DestroyDocumentObjects; override;
    procedure Initialize; override;
    procedure SubscribeOptionsEvents; override;{$IFNDEF CPUX64} final;{$ENDIF}
    procedure SubscribeDocumentObjectsEvents; override;{$IFNDEF CPUX64} final;{$ENDIF}

    function UseFontSubstitution: Boolean; override;{$IFNDEF CPUX64} final;{$ENDIF}

    function CreateDocumentCache: TdxCustomDocumentCache; override;
    function CreateDocumentModelCopyOptions(AFrom: TdxDocumentLogPosition; ALength: Integer): TdxDocumentModelCopyOptions;
    procedure SetModelForExportCopyOptions(ACopyOptions: TdxDocumentModelCopyOptions);
    procedure CopyDocumentModelOptions(ADestinationModel: TdxDocumentModel);
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function GetActivePieceTableCore: TdxCustomPieceTable; override;

    function AddHistoryItem(AItem: TdxHistoryItem): TdxHistoryItem; override;
    function AutodetectDocumentFormat(const AFileName: string; AUseFormatFallback: Boolean = True): TdxRichEditDocumentFormat; override;{$IFNDEF CPUX64} final;{$ENDIF}

    //for internal use
    procedure ClearDataSources; virtual;


    procedure ClearDocumentCore; override;
    procedure Reinitialize; virtual;
    function CreateInternalAPI: TdxCustomInternalAPI;
    function CreateCommandCreationStrategy: TdxRichEditCommandsCreationStrategy;

    function CreateMailMergeDataController: TdxRichEditDataControllerAdapterBase;

    function CreateMainContentType: TdxSimpleMainContentType; override;
    function CreatePieceTable(AContentType: TdxContentTypeBase): TdxCustomPieceTable{TdxPieceTable}; override;
    procedure OnLayoutUnitChanged; override;
    function GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList; override;
    function GetBookmarks(AIncludeHiddenBookmarks: Boolean = True): TdxBookmarkList;
    function CreateHyphenator: TdxHyphenator;
    function CreateHyphenationService{(AContainer: IdxServiceContainer; AServiceType: TObject)}: IInterface;
    procedure AddServices; virtual;
    procedure AddDataServices; virtual;
    function CreateDocumentImportManagerService: IInterface; virtual;
    function CreateDocumentExportManagerService: IInterface; virtual;
    procedure SubscribeDocumentPropertiesEvents;
    procedure UnsubscribeDocumentPropertiesEvents;
    procedure SubscribeProtectionPropertiesEvents;
    procedure UnsubscribeProtectionPropertiesEvents;
    procedure SubscribeMailMergePropertiesEvents;
    procedure UnsubscribeMailMergePropertiesEvents;
    procedure SubscribeMailMergeDataControllerEvents;
    procedure UnubscribeMailMergeDataControllerEvents;
    procedure UpdateFields(AUpdateType: TdxUpdateFieldOperationType; AOptions: TdxFieldUpdateOnLoadOptions = nil);
    procedure SubscribeRichEditLayoutOptions;
    procedure SubscribeTableOptions;
    procedure UnsubscribeRichEditLayoutOptions;
    procedure OnTableOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
    procedure OnLayoutOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
    procedure OnDocumentPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure OnDocumentPropertiesPageBackgroundChanged(ASender: TObject; E: TdxEventArgs);
    procedure OnProtectionPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure OnHistoryOperationCompleted(ASender: TObject; E: TdxEventArgs); override;
    procedure OnHistoryModifiedChanged(ASender: TObject; E: TdxEventArgs); override;

    function ShouldFormatSpecialEmptyParagraph: Boolean;
    function IsSpecialEmptyParagraphAfterInnerTable(AParagraph: TdxParagraph; ACell: TdxTableCell): Boolean;
    function IsCursorInParagraph(AParagraph: TdxParagraph): Boolean;
    function IsTextSelectedOnly: Boolean;
    function CreateDocumentHistory: TdxDocumentHistory; override;
    procedure SwitchToNormalSelection;
    function CreateNormalSelection: TdxSelection;
    procedure SwitchToEmptySelection;
    procedure SwitchToSelectionCore(ANewSelection: TdxSimpleSelection); override;
    procedure BeginSetContent;
    function CreateDocumentModelForExport(const AInitializeEmptyDocumentModel: TdxAction<TdxDocumentModel>): TdxDocumentModel;
    procedure SubscribeEventsForExport(AModelForExport: TdxDocumentModel);
    procedure UnsubscribeEventsForExport;
    procedure RaiseAfterExportFromExportModel(ASender: TObject);
    procedure RaiseBeforeExportFromExportModel(ASender: TObject; E: TdxBeforeExportEventArgs);
    procedure RaiseCalculateDocumentVariableForExport(ASender: TObject; E: TdxCalculateDocumentVariableEventArgs);
    procedure RaiseBeforeExport(AFormat: TdxRichEditDocumentFormat; const AOptions: IdxExporterOptions);
    procedure InheritServicesForExport(ADocumentModel: TdxDocumentModel);
    procedure UpdateTableOfContents; virtual;
    procedure BeginSetContentForExport;
    procedure EndSetContentForExport(AChangeType: TdxDocumentModelChangeType; AUpdateFields: Boolean);
    procedure EndSetContent(AChangeType: TdxDocumentModelChangeType; AUpdateFields: Boolean; AUpdateOptions: TdxFieldUpdateOnLoadOptions); overload;
    procedure EndSetContent(AChangeType: TdxDocumentModelChangeType; AUpdateFields, APasteFromIe: Boolean;
      AUpdateOptions: TdxFieldUpdateOnLoadOptions; const AAfterPieceTablesEndSetContentAction: TdxAction = nil); overload;
    procedure BeginFieldsUpdate;
    procedure EndFieldsUpdate;
    procedure PreprocessContentBeforeInsert(ADestination: TdxPieceTable; APos: TdxDocumentLogPosition);
    procedure PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat);
    procedure InitializeDefaultProperties; override;
    procedure InitializeDefaultStyles;

    procedure OnFirstBeginUpdate; override;
    procedure OnBeginUpdate; override;
    procedure OnEndUpdate; override;
    procedure OnCancelUpdate; override;
    procedure OnLastEndUpdateCore; override;
    procedure ResetParagraphsOnLastEndUpdate;
    procedure ForceSyntaxHighlight; virtual;
    procedure NotifyContentChanged;
    procedure PerformSyntaxHighlight(AForced: Boolean);
    procedure PerformAutoCorrect;
    procedure ApplyChanges(APieceTable: TdxCustomPieceTable{TdxPieceTable}; AChangeType: TdxDocumentModelChangeType; AStartRunIndex, AEndRunIndex: TdxRunIndex); override;
    procedure ApplyChangesCore(APieceTable: TdxCustomPieceTable{TdxPieceTable}; AActions: TdxDocumentModelChangeActions; AStartRunIndex, AEndRunIndex: TdxRunIndex); override;
    procedure InvalidateDocumentLayout;
    procedure InvalidateDocumentLayoutFrom(ARunIndex: TdxRunIndex);
    procedure OnSectionInserted(ASectionIndex: TdxSectionIndex); override;
    procedure OnSectionRemoved(ASectionIndex: TdxSectionIndex); override;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); override;
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer); override;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer); override;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer); override;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); override;
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); override;
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable); override;
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable); override;
  {$ENDREGION}

    procedure RecalcSectionIndices(AFrom: TdxSectionIndex; ADeltaIndex: Integer);
    procedure RecalcSectionIndicesCore(AFrom: TdxSectionIndex; ADeltaIndex: Integer);
    function GetActiveSectionBySelectionEnd: TdxSection;
    procedure InsertSection(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False); overload;
    procedure ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase); override;
    procedure ApplySectionFormattingCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase);
    procedure DeleteDefaultNumberingList(ANumberingLists: TdxNumberingListCollection);
    function GetNumberingListIndex(ATarget: TdxCustomDocumentModel; ASourceListIndex, AMaxNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex; override;
    function CheckIsOverrideEquals(ATargetNumberingList, ASourceNumberingList: TdxNumberingList): Boolean;
    function GetAbstractNumberingListIndex(ATarget: TdxDocumentModel; ASourceListIndex: TdxNumberingListIndex): TdxAbstractNumberingListIndex;
    procedure CreateAbstractNumberingList(ATarget: TdxDocumentModel; ATargetList: TdxAbstractNumberingList; ANumberingListId: Integer; ASourceListIndex: TdxNumberingListIndex);
    procedure AddAbstractNumberingListUsingHistory(AAbstractList: TdxAbstractNumberingList);
    procedure AddNumberingListUsingHistory(ANumberingList: TdxNumberingList);
    function GetSelectionText: string;
    procedure ResetDocumentFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType); override;
    procedure ClearModelCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
    function GetDatabaseFieldNames: TArray<TdxMergeFieldName>;

    function CreateNumberingListCountersCalculator(AList: TdxAbstractNumberingList): TdxNumberingListCountersCalculator;
    function GetLastInsertedFloatingObjectAnchorRunInfo(APieceTable: TdxPieceTable): TdxLastInsertedFloatingObjectAnchorRunInfo;
    procedure OnGridLinesOptionsChanged;
    procedure OnLayoutTablesToExtendIntoMarginsOptionsChanged;
    procedure OnMatchHorizontalTableIndentsToTextEdgeOptionsChanged;
    function GetActiveSection: TdxSection;
    function GetActiveSectionIndex: TdxSectionIndex;
    procedure SetActivePieceTable(APieceTable: TdxCustomPieceTable); overload; override;
    procedure SetActivePieceTable(APieceTable: TdxPieceTable; ASection: TdxSection); overload;
    procedure SetActivePieceTableCore(APieceTable: TdxPieceTable; ASection: TdxSection);
    function CanEditSection(ASection: TdxSection): Boolean;
    procedure EnforceDocumentProtection(const APassword: string);
    function RemoveDocumentProtection(const APassword: string): Boolean;
    procedure ForceRemoveDocumentProtection;
    function CheckDocumentProtectionPassword(const APassword: string): Boolean; virtual;
    function CheckPasswordHash(const AHash, AExpectedHash: TArray<Byte>): Boolean; virtual;
    function CheckLegacyDocumentProtectionPassword(ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
    function CheckOpenXmlDocumentProtectionPassword(ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
    function CheckOpenOfficeDocumentProtectionPassword(ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
    function CheckRtfDocumentProtectionPassword(ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
    function CheckDocumentProtectionPasswordCore(ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
    procedure ResetParagraphs;
    procedure ResetAdditionalPieceTablesParagraphs(APieceTable: TdxPieceTableList);
    procedure InheritDataServices(ADocumentModel: TdxDocumentModel); virtual;
    procedure InheritUriProviderService(ADocumentModel: TdxDocumentModel); virtual;
    procedure ResetTemporaryLayout;
    procedure ToggleAllFieldCodes(AShowCodes: Boolean);
    function CreateNew: TdxDocumentModel; virtual;
    function CreateCopySectionOperation(ACopyManager: TdxCustomDocumentModelCopyManager): TdxSelectionBasedOperation;
    function CreateDocumentModelCopyCommand(ASourcePieceTable: TdxPieceTable; ATarget: TdxDocumentModel;
      AOptions: TdxDocumentModelCopyOptions): TObject;
    procedure EnsureImagesLoadComplete;
    procedure EnsurePieceTableImagesLoadComplete(APieceTable: TdxPieceTable);
    procedure SuspendSyntaxHighlight;
    procedure ResumeSyntaxHighlight;
    function CreateDocumentImportHelper: TObject; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function CreateDocumentExportHelper(ADocumentFormat: TdxRichEditDocumentFormat): TObject; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function CreateEmptySpellCheckerManager(APieceTable: TdxCustomPieceTable): TObject{TdxSpellCheckerManager}; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetImportManagerService: IInterface; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetExportManagerService: IInterface; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetMailMergeDataMode: TdxMailMergeDataMode;
    function GetFieldResultModel: TdxDocumentModel;
    function GetBoxEffectiveRotationAngle(ABox: TdxBox): Integer;
    function GetBoxEffectiveRotationAngleInDegrees(ABox: TdxBox): Single;
    procedure NormalizeZOrder;
    procedure BeforeFloatingObjectDrop(AOldPosition, ANewPosition: TdxDocumentLogPosition; APieceTable: TdxPieceTable);

    procedure LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
      AEncoding: TEncoding = nil); overload;

    procedure SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat;
      const ATargetUri: string = ''; AEncoding: TEncoding = nil); virtual;

    // for internal use
    function CreateSection: TdxCustomSection; override;
    procedure NotifyNumberingListParagraphAdded(AIndex: TdxNumberingListIndex); override;
    procedure NotifyNumberingListParagraphRemoved(AIndex: TdxNumberingListIndex); override;
    procedure RecreateLineNumberRun;
    function QueryEncryptionPassword(var APassword: string): Boolean; virtual;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; override;

    // Events
    function RaiseMailMergeStarted(const Args: TdxMailMergeStartedEventArgs): Boolean;
    function RaiseMailMergeRecordStarted(const Args: TdxMailMergeRecordStartedEventArgs): Boolean;
    function RaiseMailMergeRecordFinished(const Args: TdxMailMergeRecordFinishedEventArgs): Boolean;
    procedure RaiseMailMergeFinished(const Args: TdxMailMergeFinishedEventArgs);
    procedure RaiseMailMergeGetTargetDocument(const Args: TdxMailMergeGetTargetDocumentEventArgs);
    procedure RaiseBeginDocumentUpdate;
    procedure RaiseEndDocumentUpdate;
    procedure RaiseBeforeEndDocumentUpdate;
    procedure RaiseAfterEndDocumentUpdate;
    procedure RaiseInnerContentChanged;
    procedure RaiseContentChanged;
    procedure RaiseInvalidFormatException(E: Exception);
    procedure RaiseModifiedChanged;
    procedure RaiseAfterExport;
    function RaiseCalculateDocumentVariable(Args: TdxCalculateDocumentVariableEventArgs): Boolean;
    procedure RaiseSectionInserted(ASectionIndex: TdxSectionIndex);
    procedure RaiseSectionRemoved(ASectionIndex: TdxSectionIndex);
    procedure RaiseBeforeImport(AFormat: TdxRichEditDocumentFormat; const AOptions: IdxImporterOptions);
    procedure RaiseInnerDocumentCleared;
    procedure RaiseDocumentCleared;
    procedure RaiseHyperlinkInfoInserted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer); override;
    procedure RaiseHyperlinkInfoDeleted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer); override;

    class property DefaultStrikeoutRepository: TdxStrikeoutRepository read FDefaultStrikeoutRepository;
    class property DefaultUnderlineRepository: TdxUnderlineRepository read FDefaultUnderlineRepository;

    property ActivePieceTable: TdxPieceTable read FActivePieceTable;
    property BorderLineRepository: TdxBorderLineRepository read FBorderLineRepository;
    property Cache: TdxDocumentCache read GetCache;
    property CommandsCreationStrategy: TdxRichEditCommandsCreationStrategy read FCommandsCreationStrategy;
    property DefaultTableCellProperties: TdxTableCellProperties read FDefaultTableCellProperties;
    property DefaultTableProperties: TdxTableProperties read FDefaultTableProperties;
    property DefaultTableRowProperties: TdxTableRowProperties read FDefaultTableRowProperties;
    property DeferredChanges: TdxDocumentModelDeferredChanges read GetDeferredChanges;
    property DisableCheckDocumentModelIntegrity: Boolean read FDisableCheckDocumentModelIntegrity write FDisableCheckDocumentModelIntegrity;
    property DocumentFileName: string read FDocumentFileName write FDocumentFileName;
    property DocumentFormat: TdxRichEditDocumentFormat read FDocumentFormat write FDocumentFormat;
    property DocumentProperties: TdxDocumentProperties read FDocumentProperties;
    property EmptyBottomBorder: TdxBottomBorder read GetEmptyBottomBorder;
    property EmptyTopBorder: TdxTopBorder read GetEmptyTopBorder;
    property EncryptionProperties: TdxDocumentEncryptionProperties read FEncryptionProperties;
    property EndNotes: TdxEndNoteCollection read FEndNotes;
    property ExtendedDocumentProperties: TdxExtendedDocumentProperties read FExtendedDocumentProperties;
    property FieldResultModel: Boolean read FFieldResultModel write FFieldResultModel;
    property FloatingObjectBorderInfoRepository: TdxBorderInfoRepository read FFloatingObjectBorderInfoRepository;
    property Footers: TdxFooterCollection read FFooters;
    property FootNotes: TdxFootNoteCollection read FFootNotes;
    property Headers: TdxHeaderCollection read FHeaders;
    property IntermediateModel: Boolean read FIntermediateModel write FIntermediateModel;
    property InternalAPI: TdxCustomInternalAPI read FInternalAPI;
    property IsEmpty: Boolean read GetIsEmpty;
    property LineNumberRun: TdxLineNumberCommonRun read FLineNumberRun;
    property MailMergeDataController: TdxRichEditDataControllerAdapterBase read FMailMergeDataController;
    property MailMergeProperties: TdxMailMergeProperties read FMailMergeProperties;
    property MainPieceTable: TdxPieceTable read GetPieceTable;
    property Modified: Boolean read GetModified write SetModified;
    property NumberDecimalSeparator: string read FNumberDecimalSeparator write FNumberDecimalSeparator;
    property NumberGroupSeparator: string read FNumberGroupSeparator write FNumberGroupSeparator;
    property NumberingListStyles: TdxNumberingListStyleCollection read FNumberingListStyles;
    property ProtectionProperties: TdxDocumentProtectionProperties read FProtectionProperties;
    property SafeEditor: TdxSafeDocumentModelEditor read FSafeEditor;
    property SearchContext: TdxSearchContext read FSearchContext;
    property SearchParameters: TdxSearchParameters read FSearchParameters;
    property Sections: TdxSectionCollection read GetSections;
    property Selection: TdxSelection read GetSelection;
    property SeparateModelForApiExport: Boolean read GetSeparateModelForApiExport;
    property ShouldApplyAppearanceProperties: Boolean read FShouldApplyAppearanceProperties write FShouldApplyAppearanceProperties;
    property StrikeoutRepository: TdxStrikeoutRepository read FStrikeoutRepository;
    property SuppressFieldsChangeNotification: Boolean read FSuppressFieldsChangeNotification;
    property SyntaxHighlightSuspended: Boolean read GetSyntaxHighlightSuspended;
    property TableBorderInfoRepository: TdxBorderInfoRepository read FTableBorderInfoRepository;
    property TableCellStyles: TdxTableCellStyleCollection read FTableCellStyles;
    property TableStyles: TdxTableStyleCollection read FTableStyles;
    property UnderlineRepository: TdxUnderlineRepository read FUnderlineRepository;
    property Variables: TdxDocumentVariableCollection read FVariables;

    property AbstractNumberingListIdProvider: TdxAbstractNumberingListIdProvider read FAbstractNumberingListIdProvider;
    property AbstractNumberingLists: TdxAbstractNumberingListCollection read FAbstractNumberingLists;
    property NumberingListIdProvider: TdxNumberingListIdProvider read FNumberingListIdProvider;
    property NumberingLists: TdxNumberingListCollection read FNumberingLists;

    // options
    property AuthenticationOptions: TdxAuthenticationOptions read FAuthenticationOptions;
    property AutoCorrectOptions: TdxAutoCorrectOptions read FAutoCorrectOptions;
    property BehaviorOptions: TdxRichEditBehaviorOptions read FBehaviorOptions;
    property BookmarkOptions: TdxBookmarkOptions read FBookmarkOptions;
    property CopyPasteOptions: TdxCopyPasteOptions read FCopyPasteOptions;
    property DocumentCapabilities: TdxDocumentCapabilitiesOptions read GetDocumentCapabilities write SetDocumentCapabilities;
    property DocumentExportOptions: TdxRichEditDocumentExportOptions read FDocumentExportOptions;
    property DocumentImportOptions: TdxRichEditDocumentImportOptions read FDocumentImportOptions;
    property DocumentSaveOptions: TdxDocumentSaveOptions read FDocumentSaveOptions;
    property EditingOptions: TdxRichEditEditingOptions read FEditingOptions;
    property FieldOptions: TdxFieldOptions read FFieldOptions;
    property FormattingMarkVisibilityOptions: TdxFormattingMarkVisibilityOptions read GetFormattingMarkVisibilityOptions;
    property LayoutOptions: TdxRichEditLayoutOptions read FLayoutOptions;
    property MailMergeOptions: TdxRichEditMailMergeOptions read FMailMergeOptions;
    property PrintingOptions: TdxPrintingOptions read FPrintingOptions;
    property RangePermissionOptions: TdxRangePermissionOptions read FRangePermissionOptions;
    property SearchOptions: TdxDocumentSearchOptions read FSearchOptions;
    property SpellCheckerOptions: TdxSpellCheckerOptions read FSpellCheckerOptions;
    property TableOptions: TdxTableOptions read FTableOptions;
    property WebSettings: TdxWebSettings read FHtmlSettings;
    // event handlers
    property AfterEndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler read FOnAfterEndDocumentUpdate;
    property AfterExport: TdxNotifyEventHandler read FOnAfterExport;
    property BeforeEndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler read FOnBeforeEndDocumentUpdate;
    property BeforeExport: TdxBeforeExportEventHandler read FOnBeforeExport;
    property BeforeImport: TdxBeforeImportEventHandler read FOnBeforeImport;
    property BeginDocumentUpdate: TdxEventHandler read FOnBeginDocumentUpdate;
    property CalculateDocumentVariable: TdxCalculateDocumentVariableEventHandler read FOnCalculateDocumentVariable;
    property ContentChanged: TdxEventHandler read FOnContentChanged;
    property DocumentCleared: TdxEventHandler read FOnDocumentCleared;
    property EndDocumentUpdate: TdxDocumentUpdateCompleteEventHandler read FOnEndDocumentUpdate;
    property HyperlinkInfoDeleted: TdxHyperlinkInfoEventHandler read FOnHyperlinkInfoDeleted;
    property HyperlinkInfoInserted: TdxHyperlinkInfoEventHandler read FOnHyperlinkInfoInserted;
    property InnerContentChanged: TdxEventHandler read FOnInnerContentChanged;
    property InnerDocumentCleared: TdxEventHandler read FOnInnerDocumentCleared;
    property InvalidFormatException: TdxRichEditInvalidFormatExceptionEventHandler read FOnInvalidFormatException;
    property MailMergeFinished: TdxMailMergeFinishedEventHandler read FOnMailMergeFinished;
    property MailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEventHandler read FOnMailMergeGetTargetDocument;
    property MailMergeRecordFinished: TdxMailMergeRecordFinishedEventHandler read FOnMailMergeRecordFinished;
    property MailMergeRecordStarted: TdxMailMergeRecordStartedEventHandler read FOnMailMergeRecordStarted;
    property MailMergeStarted: TdxMailMergeStartedEventHandler read FOnMailMergeStarted;
    property ModifiedChanged: TdxEventHandler read FOnModifiedChanged;
    property PageBackgroundChanged: TdxEventHandler read FOnPageBackgroundChanged;
    property SectionInserted: TdxSectionEventHandler read FOnSectionInserted;
    property SectionRemoved : TdxSectionEventHandler read FOnSectionRemoved;
    property OnEncryptionPasswordQuery: TdxEncryptionPasswordQueryEvent read FOnEncryptionPasswordQuery write FOnEncryptionPasswordQuery;
  end;

  { TdxParagraphPropertyModifierBase }

  TdxParagraphPropertyModifierBase = class abstract
  public
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); virtual; abstract;
  end;


  { TdxNumberingListCalculatorCache }

  TdxNumberingListCalculatorCache = class
  strict private
    FCache: TObjectDictionary<TdxAbstractNumberingListIndex, TdxNumberingListCountersCalculator>;
    FDocumentModel: TdxDocumentModel;
    FLastProcessedParagraphIndex: TdxParagraphIndex;
  protected
    property DocumentModel: TdxDocumentModel read FDocumentModel;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    function GetRangeListCounters(AParagraph: TdxParagraph): TIntegerDynArray;
    procedure Clear;
    function GetCacheItem(AListIndex: TdxAbstractNumberingListIndex): TdxNumberingListCountersCalculator;
  end;

  { TdxPieceTable }

  TdxPieceTable = class(TdxSimplePieceTable,
    IdxDebugVisualizer)
  strict protected
    type
      TdxValidateFieldFunction = reference to function(AField: TdxField): Boolean;
  strict private
    FPrecalculatedNumberingListTexts: TDictionary<TdxParagraph, string>;
    FTables: TdxTableCollection;
    FComments: TdxCommentCollection;
    FBookmarks: TdxBookmarkCollection;
    FMyTables: TdxTableCellsManager;
    FRangePermissions: TdxRangePermissionCollection;
    FCustomMarks: TdxCustomMarkCollection;
    FTextBoxes: TdxTextBoxContentTypeList;
    FCalculatorCache: TdxNumberingListCalculatorCache;
    FNavigationVisibleTextFilter: TdxVisibleTextFilterBase;

    FLayoutDependentTextInserter: TdxObjectInserter;
    FFootNoteRunInserter: TdxObjectInserter;
    FEndNoteRunInserter: TdxObjectInserter;

    FFieldUpdater: TdxFieldUpdater;
    FSpellCheckerManager: TdxRichEditSpellCheckerManager;
    FSuppressTableIntegrityCheck: Boolean;
    FShouldForceUpdateIntervals: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetFields: TdxFieldCollection;
    function GetIsHeaderFooter: Boolean;
    function GetIsFooter: Boolean;
    function GetIsHeader: Boolean;
    function GetIsNote: Boolean;
    function GetIsFootNote: Boolean;
    function GetIsEndNote: Boolean;
    function GetIsTextBox: Boolean;
    function GetIsComment: Boolean;
    function GetIsReferenced: Boolean;
    function GetLastInsertedFloatingObjectAnchorRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo;
    function GetParagraphs: TdxParagraphCollection;
    procedure SetPrecalculatedNumberingListTexts(const Value: TDictionary<TdxParagraph, string>);
    procedure SetSpellCheckerManager(const Value: TdxRichEditSpellCheckerManager);

    procedure ResetTableAllUse(ATable: TdxTable);
  strict protected
    function CreateFieldCollection: TdxFieldCollectionBase; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function CreateParagraphCollection: TdxParagraphBaseCollection; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetDocumentStartLogPosition: TdxDocumentLogPosition; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetDocumentEndLogPosition: TdxDocumentLogPosition; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function CreateObjectInserter: TdxObjectInserter; override;{$IFNDEF CPUX64} final;{$ENDIF}
  protected
    function CreateTextRunsDeletedHistoryItem: TdxRichEditHistoryItem; override;
    function GetIsEmpty: Boolean; override;
    procedure InitializeUncheckedInterval; override;

    procedure DoParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphRemovedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure DoParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphInsertedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure DoParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphMergedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure DoRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer); override;
    procedure RunInsertedCore(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); override;
    procedure DoRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); override;
    procedure RunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); override;
    procedure DoRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure RunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure DoRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure RunUnmergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure DoRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); override;
    procedure RunSplitCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); override;
    procedure DoRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer); override;
    procedure RunJoinedCore(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer); override;
    procedure DoFieldInserted(AFieldIndex: Integer); override;
    procedure DoFieldRemoved(AFieldIndex: Integer); override;

    procedure BookmarksClear; virtual;
    function CollectFieldsToProcess(AParent: TdxField): TdxFieldList;
    procedure FixParagraphFramesInTablesCore; virtual;
    procedure RemoveFieldWithCode(AField: TdxField);

    function GetDebugVisualizerData: string;
    procedure WriteDebugVisualizerData(AWriter: TWriter);
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase); override;
    destructor Destroy; override;

    procedure AddPieceTables(AResult: TdxFastList; AIncludeUnreferenced: Boolean); override;{$IFNDEF CPUX64} final;{$ENDIF}
    procedure Clear; override;

    function CreateParagraph: TdxSimpleParagraph; override;
    function GetTableCore(AIndex: Integer): TdxCustomTable; override;

    function CreateFieldUpdater: TdxFieldUpdater;
    function CreateSpellCheckerManager: TdxRichEditSpellCheckerManager;

    procedure AddParagraphToList(AParagraphIndex: TdxParagraphIndex; ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer); override;

    function GetPlainText(const AStartPos, AEndPos: TdxDocumentModelPosition): string;
    function GetPlainText2(const AStartPos, AEndPos: TdxFormatterPosition): string; overload;
    function GetPlainText2(const AStartPos, AEndPos: TdxDocumentModelPosition): string; overload;
    function GetFilteredPlainText(const AStart, AEnd: TdxDocumentModelPosition; const APredicate: TdxPredicate<TdxRunIndex>): string;
    procedure GetNumberingListText(AIndex: TdxRunIndex; ASb: TStringBuilder);
    function FindParagraph(ALogPosition: TdxDocumentLogPosition): TdxParagraph;
    procedure FixParagraphFramesInTables;
    procedure AddFieldToTable(AField: TdxField; AIndex: Integer);
    procedure InsertParagraphCoreNoInheritParagraphRunStyle(APos: TdxInputPosition);
    procedure InsertSectionParagraphCore(APos: TdxInputPosition); overload;
    procedure InheritParagraphRunStyleCore(APos: TdxInputPosition; AParagraphRun: TdxTextRunBase);
    procedure InheritParagraphRunStyle(APos: TdxInputPosition; AParagraphRun: TdxTextRunBase);
    function InsertSectionParagraphCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean): TdxRunIndex; overload;
    procedure InsertParagraphCore(APos: TdxInputPosition); override;

    procedure InsertFieldSymbolResult(ALogPosition: TdxDocumentLogPosition; const ASymbol: Char); override;
    procedure InsertInsertFieldSymbolResultCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const ASymbol: Char; AForceVisible: Boolean);

    function InsertFloatingObjectAnchorCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition): TdxFloatingObjectAnchorRun; overload;
    function InsertFloatingObjectAnchorCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean): TdxFloatingObjectAnchorRun; overload;
    function InsertFloatingObjectAnchorCore(APos: TdxInputPosition): TdxFloatingObjectAnchorRun; overload;
    procedure ApplyListLevelIndexToParagraph(AParagraph: TdxParagraph; ALevelIndex: Integer);
    procedure RemoveParagraphFromList(AParagraphIndex: TdxParagraphIndex);
    procedure InsertLayoutDependentTextRun(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AFormatting: TdxFieldResultFormatting);
    function InsertFootNoteRun(APos: TdxInputPosition; ANoteIndex: Integer): TdxTextRunBase; overload;
    function InsertFootNoteRun(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer): TdxTextRunBase; overload;
    function InsertEndNoteRun(APos: TdxInputPosition; ANoteIndex: Integer): TdxTextRunBase; overload;
    function InsertEndNoteRun(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer): TdxTextRunBase; overload;
    procedure InsertFootNoteRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      AFormatting: TdxFieldResultFormatting; ANoteIndex: Integer; AInserter: TdxObjectInserter{TdxFootNoteRunInserterBase<T>});
    procedure InsertTextCoreNoResetMergeNoApplyFormatting(APos: TdxInputPosition; const AText: string; AForceVisible: Boolean);
    procedure SplitTextRunsByCharset;
    procedure AppendText(APos: TdxInputPosition; ACh: Char); overload;
    procedure AppendText(APos: TdxInputPosition; const AText: string); overload;
    procedure AppendImage(APos: TdxInputPosition; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single; AUseScreenDpi: Boolean = False); overload;
    procedure AppendImage(APos: TdxInputPosition; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single; AFillColor: TdxAlphaColor; AUseScreenDpi: Boolean); overload;
    procedure ApplyCharacterStyleCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxRunPropertyModifierBase);
    procedure ApplyParagraphStyleCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxRunPropertyModifierBase);
    procedure ChangeParagraphStyle(AParagraph: TdxParagraph; AModifier: TdxRunPropertyModifierBase);
    function ObtainMergedParagraphsProperties(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
      AModifier: TdxParagraphPropertyModifierBase): TdxMergedParagraphProperties;
    function ObtainMergedParagraphsTabFormattingInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
      AModifier: TdxParagraphPropertyModifierBase): TdxTabFormattingInfo;
    function GetLanguageInfo(const AStart, AEnd: TdxDocumentModelPosition): TdxNullableValue<TdxLangInfo>;
    function ShouldCheckWord(const AStart, AEnd: TdxDocumentModelPosition): Boolean;

    procedure ResetCharacterStyle(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
    procedure MultipleSplitTextRun(APositions: TdxIntegerList);
    procedure SetFont(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AFont: TFont);
    procedure SetForeColor(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AForeColor: TdxAlphaColor);
    procedure SetBackColor(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; ABackColor: TdxAlphaColor);
    procedure ApplyCharacterFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxRunPropertyModifierBase); overload;
    procedure ApplyParagraphFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxParagraphPropertyModifierBase); overload;
    procedure ApplyParagraphFormattingCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxParagraphPropertyModifierBase);
    procedure ApplyCharacterStyle(ALogPositionStart: TdxDocumentLogPosition; ALength, AStyleIndex: Integer; AResetProperties: Boolean); overload;
    procedure ApplyCharacterStyle(ALogPositionStart: TdxDocumentLogPosition; ALength, AStyleIndex: Integer); overload;
    procedure ApplyParagraphStyle(ALogPositionStart: TdxDocumentLogPosition; ALength, AStyleIndex: Integer);

    procedure ApplyTableStyleCore(ATables: TdxList<TdxTable>; AStyleIndex: Integer);
    function ShouldApplyStyleToParagraphs(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): Boolean;
    procedure ClearFontCacheIndices;
    procedure ClearRunFontCacheIndex(ARun: TdxTextRunBase);
    procedure ToggleFieldCodesFromCommandOrApi(AField: TdxField);
    procedure ProcessFieldsRecursive(AParent: TdxField; const AValidateField: TdxValidateFieldFunction);
    function GetEntireFieldsFromInterval(AStart: TdxRunIndex; AEnd: TdxRunIndex): TdxFieldList;
    function ShouldForceUpdateIntervals: Boolean;
    procedure UpdateIntervals;
    procedure OnBeginSetContent;
    procedure OnEndSetContent;
    function HasInlinePicture(ARunInfo: TdxRunInfo): Boolean;
    procedure ConvertParagraphsToTable(AFirstParagraphIndex: TdxParagraphIndex; ARowCount, ACellCount: Integer);
    procedure PerformTextRunSplit(ARunIndices: TdxSortedRunIndexCollection);
    procedure ApplyNumberingToInsertedParagraph(AParagraphIndex: TdxParagraphIndex); override;
    procedure DeleteNumerationFromParagraphAndChangeParagraphStyle(AParagraph: TdxParagraph; AParagraphStyleIndex: Integer);
    procedure CopyCharacterPropertiesToParagraphMark(ASourceParagraph, ATargetParagraph: TdxParagraph);
    procedure InsertFloatingObjectAnchor(ALogPosition: TdxDocumentLogPosition); overload;
    procedure InsertFloatingObjectAnchor(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); overload;
    function CreateTableCore(ASourceCell: TdxTableCell): TdxTable;
    function CreateTableRowCore(ATable: TdxTable): TdxTableRow; overload;
    function CreateTableRowCore(ATable: TdxTable; ARowIndex: Integer): TdxTableRow; overload;
    function CreateTableCellCore(ARow: TdxTableRow; AStart, AEnd: TdxParagraphIndex): TdxTableCell; overload;
    function CreateTableCellCore(ARow: TdxTableRow; AInsertedIndex: Integer; AStart, AEnd: TdxParagraphIndex): TdxTableCell; overload;
    procedure DeleteTableCore(ADeletedTable: TdxTable);
    procedure DeleteEmptyTableRowCore(ATableIndex, ARowIndex: Integer);
    procedure DeleteEmptyTableCellCore(ATableIndex, ARowIndex, ACellIndex: Integer);
    procedure ConvertParagraphsIntoTableRow(ARow: TdxTableRow; AIndex: TdxParagraphIndex; AParagraphCount: Integer);
    procedure ChangeCellEndParagraphIndex(ATableCell: TdxTableCell; AIndex: TdxParagraphIndex);
    procedure ChangeCellStartParagraphIndex(ATableCell: TdxTableCell; AIndex: TdxParagraphIndex);
    procedure InsertSeparator(ALogPosition: TdxDocumentLogPosition); overload;
    procedure InsertSeparator(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); overload;
    function GetFieldsInsideInterval(AFirstRunIndex, ALastRunIndex: TdxRunIndex): TdxFieldList;
    procedure UpdateTableOfContents(AOperationType: TdxUpdateFieldOperationType);
    function CalculateFieldResult(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode;
      AUpdateType: TdxUpdateFieldOperationType): TdxCalculateFieldResult;
    function ValidateBookmarkName(const AName: string): string;
    function CreateBookmarkCore(APosition: TdxDocumentLogPosition; ALength: Integer; const AName: string;
      AForceUpdateInterval: Boolean = False): TdxBookmark;
    procedure CreateBookmark(APosition: TdxDocumentLogPosition; ALength: Integer; const AName: string);
    procedure DeleteBookmark(ABookmarkIndex: Integer);
    procedure DeleteBookmarkCore(ABookmarkIndex: Integer);
    function GetEntireBookmarks(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
    function CreateCommentCore(APosition: TdxDocumentLogPosition; ALength: Integer;
      const AName, AId, AAuthor, ATime, ADate, AIdParent: string; AContent: TdxCommentContentType): TdxComment;
    procedure CreateComment(APosition: TdxDocumentLogPosition; ALength: Integer;
      const AName, AId, AAuthor, ATime, ADate, AIdParent: string; AContent: TdxCommentContentType);
    procedure DeleteComment(ACommentIndex: Integer);
    procedure DeleteCommentCore(ACommentIndex: Integer);
    function GetEntireComments(AStart: TdxDocumentLogPosition; ALength: Integer): TdxCommentList;
    function InsertCustomMark(ACustomMarkIndex: Integer; ACustomMark: TdxCustomMark): TdxCustomMark;
    procedure DeleteCustomMark(ACustomMarkIndex: Integer);
    procedure DeleteCustomMarkCore(ACustomMarkIndex: Integer);
    function CreateCustomMarkCore(APostion: TdxDocumentLogPosition; AUserData: TObject): TdxCustomMark;
    procedure CreateCustomMark(APosition: TdxDocumentLogPosition; AUserData: TObject);
    procedure ApplyDocumentPermission(AStart, AEnd: TdxDocumentLogPosition; AInfo: TdxRangePermissionInfo);
    procedure RemoveDocumentPermission(AStart, AEnd: TdxDocumentLogPosition; AInfo: TdxRangePermissionInfo);
    procedure DeleteRangePermission(APermission: TdxRangePermission);
    function ApplyDocumentPermissionCore(AStart, AEnd: TdxDocumentLogPosition; ARangePermissionInfoIndex: Integer): TdxRunInfo;
    function RemoveDocumentPermissionCore(AStart, AEnd: TdxDocumentLogPosition; ARangePermissionInfoIndex: Integer): TdxRunInfo;
    function CreateRangePermission(AStart, AEnd: TdxDocumentLogPosition; ARangePermissionInfoIndex: Integer): TdxRangePermission;
    function GetEntireRangePermissions(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
    function ExtractMergedRanges(ARangePermissionInfoIndex: Integer): TdxRangePermissionCollectionEx;
    procedure AppendMergedRanges(AMergedRanges: TdxRangePermissionCollectionEx);
    function CanContainCompositeContent: Boolean;
    function CanEditSelection: Boolean;
    function CanEditSelectionItems(AItems: TdxSelectionItemList): Boolean;
    function CanEditRangeLength(AStart: TdxDocumentLogPosition; ALength: Integer): Boolean;
    function CanEditRange(AStart, AEnd: TdxDocumentLogPosition): Boolean; override;
    function IsPermissionGranted(ARangePermission: TdxRangePermission): Boolean;
    function ObtainRangePermissionsMatchSelection: TdxRangePermissionCollection;
    procedure ObtainRangePermissionsMatchSelectionItem(ATarget: TdxRangePermissionCollection; AItem: TdxSelectionItem);
    function ObtainRangePermissionsWithSelectionInside: TdxRangePermissionCollection;
    procedure ObtainRangePermissionsWithSelectionItemInside(ATarget: TdxRangePermissionCollection; AItem: TdxSelectionItem);
    procedure AddNumberingListToParagraph(AParagraph: TdxSimpleParagraph; ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
    function IsNumberingListLevelIndexValid(AParagraph: TdxSimpleParagraph; ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer): Boolean;
    function IsValidNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex): Boolean;
    procedure DeleteNumerationFromParagraph(AParagraph: TdxParagraph);
    procedure WriteParagraphLeftIndent(AParagraph: TdxParagraph);
    procedure RemoveNumberingFromParagraph(AParagraph: TdxSimpleParagraph); override;
    function InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer): TdxTable; overload;
    function InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount, AFixedColumnWidths: Integer): TdxTable; overload;
    function InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer;
      AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths: Integer): TdxTable; overload;
    function InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer;
      AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths, AOuterColumnWidth: Integer; AForceVisible: Boolean): TdxTable; overload;
    function InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer; AAutoFitBehavior: TdxTableAutoFitBehaviorType;
      AFixedColumnWidths, AOuterColumnWidth: Integer; AForceVisible: Boolean; AMatchHorizontalTableIndentsToTextEdge: Boolean): TdxTable; overload;
    procedure ValidateTableIndent(ATable: TdxTable);
    function InsertParagraph(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxParagraph; reintroduce; overload;
    procedure InsertParagraphs(ALogPosition: TdxDocumentLogPosition; ACount: Integer; AForceVisible: Boolean);
    procedure InsertTableRowBelow(APatternRow: TdxTableRow; AForceVisible: Boolean);
    procedure InsertTableRowAbove(APatternRow: TdxTableRow; AForceVisible: Boolean);
    procedure MergeTableCellsHorizontally(ACell: TdxTableCell; ACount: Integer);
    procedure MergeTableCellsVertically(ACell: TdxTableCell; ACount: Integer);
    procedure SplitTable(ATableIndex, ARowIndex: Integer; AForceVisible: Boolean);
    procedure InsertColumnToTheLeft(APatternCell: TdxTableCell; AForceVisible: Boolean);
    procedure InsertColumnToTheRight(APatternCell: TdxTableCell; AForceVisible: Boolean);
    procedure DeleteTableColumns(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection}; const AServer: IdxInnerRichEditDocumentServerOwner);
    procedure InsertTableCellWithShiftToTheDown(APatternCell: TdxTableCell; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
    procedure DeleteTableCellWithContent(ADeletedCell: TdxTableCell; const AServer: IdxInnerRichEditDocumentServerOwner); overload;
    procedure DeleteTableCellWithContent(ADeletedCell: TdxTableCell; ACanNormalizeCellVerticalMerging: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner); overload;
    procedure DeleteTableCellsWithShiftToTheUp(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
    procedure InsertTableCellToTheRight(APatternCell: TdxTableCell; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
    procedure InsertTableCellToTheLeft(APatternCell: TdxTableCell; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
    procedure DeleteTableCellWithNestedTables(ATableIndex, ARowIndex, ACellIndex: Integer);
    procedure SplitTableCellsHorizontally(ACell: TdxTableCell; APartsCount: Integer; const AServer: IdxInnerRichEditDocumentServerOwner); overload;
    procedure SplitTableCellsHorizontally(ACell: TdxTableCell; APartsCount: Integer; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner); overload;
    procedure JoinTables(ATopTable, ABottomTable: TdxTable);
    procedure MoveTableRowToOtherTable(ATargetTable: TdxTable; ARow: TdxTableRow);
    procedure DeleteTableFromTableCollection(ADeletedTable: TdxTable);
    procedure SplitTableCellsVertically(APatternCell: TdxTableCell; APartsCount, AColumnsCount: Integer; AForceVisible: Boolean);
    procedure DeleteSelectedTables(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
    procedure DeleteTablesByNestedLevel(ACurrent: TdxTableCellNode; ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
    function ShouldDeleteTable(ATable: TdxTable; ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean;
    procedure DeleteTableWithContent(ADeletedTable: TdxTable);
    procedure DeleteTableRowWithContent(ADeletedRow: TdxTableRow);
    procedure DeleteBackContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer; ADocumentLastParagraphSelected: Boolean);
    procedure DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer; ADocumentLastParagraphSelected: Boolean); overload;
    procedure DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected, AAllowPartiallyDeletingField: Boolean); overload;
    procedure DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields: Boolean); overload;
    procedure DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields, ALeaveFieldIfResultIsRemoved: Boolean); overload;
    procedure DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields, ALeaveFieldIfResultIsRemoved, ABackspacePressed: Boolean); overload;

    function CreateNavigationVisibleTextFilter(AShowHiddenText: Boolean): TdxVisibleTextFilterBase;
    procedure SetShowHiddenText(AValue: Boolean); override;
    procedure InsertDocumentModelContent(ADocumentModel: TdxDocumentModel; APos: TdxDocumentLogPosition); overload;
    procedure InsertDocumentModelContent(ADocumentModel: TdxDocumentModel; APos: TdxDocumentLogPosition;
      ASuppressParentFieldsUpdate, ASuppressFieldsUpdate, ACopyBetweenInternalModels: Boolean); overload;
    procedure ForceCheckTablesIntegrity;
    function GetRunInfoByTableCell(ACell: TdxTableCell): TdxRunInfo;
    procedure FixLastParagraph;
    procedure FixLastParagraphCore;
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
    procedure RemoveLastParagraph(AOriginalParagraphCount: Integer);
    function ShouldFixLastParagraph: Boolean;
    procedure UnsafeRemoveLastSpaceSymbolRun;
    procedure FixTables;
    procedure ResetParagraphs(AFrom, ATo: TdxParagraphIndex); override;
    procedure ApplyAutoCorrect(APosition: TdxDocumentLogPosition;
      ALength: Integer; const AText: string; const ARule: IdxSpellCheckerAutoCorrectCustomRule);
    procedure ReplaceTextWithPicture(APosition: TdxDocumentLogPosition; ALength: Integer; AImage: TdxOfficeImageReference);
    procedure ReplaceTextWithMultilineText(APosition: TdxDocumentLogPosition; ALength: Integer; const AText: string);
    procedure MovePositionToNext(var APos: TdxDocumentModelPosition; AOffset: Integer);
    function GetTocFields: TdxFieldList;
    function GetFieldToken(AField: TdxField): IdxToken;
    function IsTocField(AField: TdxField): Boolean;
    procedure MergeCells(ASelectedCellsCollectionInstance: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
    procedure MergeCellsHorizontally(ASelectedCellsInstance: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
    function MergeCellsHorizontallyCore(AStartCell: TdxTableCell; AMergedCellsColumnSpan: Integer): Integer;
    function CreateDeleteContentOperation: TdxCustomDeleteContentOperation; override;
    function GetBookmarks(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
    procedure EnsureImagesLoadComplete;
    procedure EnsureImageLoadComplete(ARun: TdxTextRunBase);
    procedure PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat);
    function GetFloatingObjectList: TdxIZOrderedObjectList;
    function GetCopyManager(ASourcePieceTable: TdxPieceTable; AInsertOptions: TdxInsertOptions): TdxCustomDocumentModelCopyManager;
    function GetCopyManagerCore(ASourcePieceTable: TdxPieceTable;
      AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions; AFormattingCopyOptions: TdxFormattingCopyOptions): TdxCustomDocumentModelCopyManager;
    function GetRangeListCounters(AParagraph: TdxParagraph): TIntegerDynArray; virtual;

    property IsHeaderFooter: Boolean read GetIsHeaderFooter;
    property IsFooter: Boolean read GetIsFooter;
    property IsHeader: Boolean read GetIsHeader;
    property IsNote: Boolean read GetIsNote;
    property IsFootNote: Boolean read GetIsFootNote;
    property IsEndNote: Boolean read GetIsEndNote;
    property IsTextBox: Boolean read GetIsTextBox;
    property IsComment: Boolean read GetIsComment;
    property IsReferenced: Boolean read GetIsReferenced;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Paragraphs: TdxParagraphCollection read GetParagraphs;
    property Tables: TdxTableCollection read FTables;
    property Fields: TdxFieldCollection read GetFields;
    property Bookmarks: TdxBookmarkCollection read FBookmarks;
    property Comments: TdxCommentCollection read FComments;
    property RangePermissions: TdxRangePermissionCollection read FRangePermissions;
    property TableCellsManager: TdxTableCellsManager read FMyTables;
    property CustomMarks: TdxCustomMarkCollection read FCustomMarks;
    property TextBoxes: TdxTextBoxContentTypeList read FTextBoxes;
    property NavigationVisibleTextFilter: TdxVisibleTextFilterBase read FNavigationVisibleTextFilter;
    property FieldUpdater: TdxFieldUpdater read FFieldUpdater;
    property PrecalculatedNumberingListTexts: TDictionary<TdxParagraph, string> read FPrecalculatedNumberingListTexts write SetPrecalculatedNumberingListTexts;
    property SpellCheckerManager: TdxRichEditSpellCheckerManager read FSpellCheckerManager write SetSpellCheckerManager;
    property SuppressTableIntegrityCheck: Boolean read FSuppressTableIntegrityCheck write FSuppressTableIntegrityCheck;
    property LastInsertedFloatingObjectAnchorRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo read GetLastInsertedFloatingObjectAnchorRunInfo;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TdxPieceTableList = class(TdxFastList)
  private
    function GetItem(Index: Integer): TdxPieceTable;
  public
    function Contains(AItem: TdxPieceTable): Boolean;
    property Items[Index: Integer]: TdxPieceTable read GetItem; default;
  end;

  { TdxParagraph }

  TdxParagraph = class(TdxSimpleParagraph)
  strict private
    function GetContextualSpacingBefore: Integer;
    function GetContextualSpacingAfter: Integer;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  strict protected
    function CreateBoxCollection: TdxSimpleParagraphBoxCollection; override;

    function EqualsMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean; override;{$IFNDEF CPUX64} final;{$ENDIF}
    procedure DoUseMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult); override;{$IFNDEF CPUX64} final;{$ENDIF}
  public
    function GetCellCore: TdxCustomTableCell; override;
    function GetListLevelIndex: Integer; override;
    function GetNumberingListIndex: TdxNumberingListIndex; override;
    function IsInCell: Boolean; override;
    function GetTabs: TdxTabFormattingInfo; override;
    function GetParentTabs: TdxTabFormattingInfo;
    function GetListLevelTabs: TdxTabFormattingInfo;

    function GetCell: TdxTableCell;

    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxParagraph;
    procedure CopyFrom(ADocumentModel: TdxDocumentModel; AResultParagraph: TdxParagraph);
    procedure CopyNumberingListProperties(ATargetParagraph: TdxParagraph);
    procedure CreateNumberingList(ATarget: TdxDocumentModel; ANumberingListId: Integer);
    function ShouldExportNumbering: Boolean;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; overload; override;
    function GetMergedCharacterProperties(AUseSpecialTableStyle: Boolean; ATableStyle: TdxTableStyle): TdxMergedCharacterProperties; overload;

    function GetParentMergedParagraphProperties: TdxMergedParagraphProperties; override;
    function GetParentMergedWithTableStyleParagraphProperties(AUseSpecialTableStyle: Boolean;
      ATableStyle: TdxTableStyle): TdxMergedParagraphProperties;
    function GetTableStyleCharacterPropertiesIndex: Integer;
    function TryUseParentMergedCachedResult(ACachedResult: TdxParagraphMergedParagraphPropertiesCachedResult; ATableStyleIndex: Integer): Boolean;
    function TryUseParentMergedCachedResultCore(ACachedResult: TdxParagraphMergedParagraphPropertiesCachedResult;
      AOwnListLevelParagraphPropertiesIndex, ATableStyleParagraphPropertiesIndex: Integer): Boolean;
    function GetOwnListLevelParagraphPropertiesIndex: Integer;
    function GetTableStyleIndex: Integer;
    function GetOwnListLevelParagraphProperties: TdxParagraphProperties;
    function GetListLevelParagraphProperties: TdxParagraphProperties;

    function IsInList: Boolean; override;
    function GetNumberingListText: string; overload; override;
    function GetNumberingListText(const ACounters: TIntegerDynArray): string; overload;
    function GetListLevelSeparator: string;
    function GetNumerationCharacterProperties: TdxMergedCharacterProperties;
    function GetNumerationFontCacheIndex: Integer;
    function GetNumerationFontInfo: TdxFontInfo;
    class function Format(const AFormatString: string; AArgs: TIntegerDynArray;
      const ALevels: TdxListLevelCollection;
      ALanguageId: Word = TdxLanguageId.English;
      ADisplayAllLevelsUsingArabicNumerals: Boolean = False): string; static;
    procedure SetNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex);
    procedure SetListLevelIndex(AListLevelIndex: Integer);
    procedure ResetNumberingListIndex(AIndex: TdxNumberingListIndex);
    procedure ResetListLevelIndex;

    function GetAbstractNumberingList: TdxAbstractNumberingList;
    function GetAbstractNumberingListIndex: TdxAbstractNumberingListIndex; override;

    property ContextualSpacingBefore: Integer read GetContextualSpacingBefore;
    property ContextualSpacingAfter: Integer read GetContextualSpacingAfter;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

  { TdxParagraphList }

  TdxParagraphList = class(TdxSimpleParagraphList)
  strict private
    function GetItem(Index: Integer): TdxParagraph;
    procedure SetItem(Index: Integer; const Value: TdxParagraph);
  public
    function First: TdxParagraph; reintroduce;
    function Last: TdxParagraph; reintroduce;
    property Items[Index: Integer]: TdxParagraph read GetItem write SetItem; default;
  end;

  TdxParagraphCollection = class(TdxSimpleParagraphCollection)
  strict private
  private
    function GetItem(Index: Integer): TdxParagraph;
  public
    function First: TdxParagraph; reintroduce;
    function Last: TdxParagraph; reintroduce;
    property Items[Index: Integer]: TdxParagraph read GetItem; default;
  end;

  { TdxDocumentFormatsHelper }

  TdxDocumentFormatsHelper = class
  public
    class function ShouldExportSectionColumns(AColumns: TdxSectionColumns; ADocumentModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertHyperlink(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertHyperlinks(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertNumbering(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertMultiLevelNumbering(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertBulletedNumbering(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertPicture(AModel: TdxDocumentModel): Boolean; static;
    class function ShouldInsertSimpleNumbering(AModel: TdxDocumentModel): Boolean; static;
    class function NeedReplaceSimpleToBulletNumbering(AModel: TdxDocumentModel): Boolean; static;
    class function NeedReplaceBulletedLevelsToDecimal(AModel: TdxDocumentModel): Boolean; static;
  end;

  { TdxChangeableDocumentInterval }

  TdxChangeableDocumentInterval = class(TdxDocumentInterval)
  protected
    procedure SetEnd(const Value: TdxDocumentLogPosition); virtual;
    procedure SetStart(const Value: TdxDocumentLogPosition); virtual;
  public
    property Start: TdxDocumentLogPosition read GetStart write SetStart;
    property &End: TdxDocumentLogPosition read GetEnd write SetEnd;
  end;

  { TdxSelectionItem }

  TdxSelectionItem = class(TdxChangeableDocumentInterval,
    IdxBatchUpdateHandler)
  strict private
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FRightOffset: Integer;
    FIsCovered: Boolean;
    FIsSelectionInTable: Boolean;
    FLeftOffset: Integer;
    FGeneration: Integer;
    FChanged: Boolean;
    FUsePreviousBoxBounds: Boolean;
    FChangedEvent: TdxEventHandler;
    procedure ResetOffsets;

    function GetPieceTable: TdxPieceTable;
    function GetVirtualEnd: TdxDocumentLogPosition;
    function GetUsePreviousBoxBounds: Boolean;
    procedure SetUsePreviousBoxBounds(const Value: Boolean);
  protected
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;

    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;

    procedure OnChanged(AStartChanged, AEndChanged: Boolean); override;
    procedure RaiseChanged; virtual;
    function GetStart: TdxDocumentLogPosition; override;
    function GetEnd: TdxDocumentLogPosition; override;
    procedure SetStart(const Value: TdxDocumentLogPosition);  override;{$IFNDEF CPUX64} final;{$ENDIF}
    procedure SetEnd(const Value: TdxDocumentLogPosition); override;{$IFNDEF CPUX64} final;{$ENDIF}

    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property IsSelectionInTable: Boolean read FIsSelectionInTable write FIsSelectionInTable;
    property VirtualEnd: TdxDocumentLogPosition read GetVirtualEnd;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    function GetStartParagraphIndex: TdxParagraphIndex;
    function GetEndParagraphIndex: TdxParagraphIndex;
    function CalculateStartPosition(AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
    function CalculateEndPosition(AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;

    procedure OnChangedCore; override;

    property Generation: Integer read FGeneration write FGeneration;
    property IsChanged: Boolean read FChanged write FChanged;
    property IsCovered: Boolean read FIsCovered write FIsCovered;
    property LeftOffset: Integer read FLeftOffset write FLeftOffset;
		property RightOffset: Integer read FRightOffset write FRightOffset;
    property UsePreviousBoxBounds: Boolean read GetUsePreviousBoxBounds write SetUsePreviousBoxBounds;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property PieceTable: TdxPieceTable read GetPieceTable;

    property Changed: TdxEventHandler read FChangedEvent write FChangedEvent;
  end;

  TdxSelectionItemList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxSelectionItem;
  public
    function First: TdxSelectionItem; reintroduce;
    function Last: TdxSelectionItem; reintroduce;

    property Items[Index: Integer]: TdxSelectionItem read GetItem; default;
  end;

  { TdxSelectionItems }

  TdxSelectionItems = class(TdxSelectionItemList)
  public
    function GetRange(AIndex, ACount: Integer): TdxSelectionItemList;
  end;

  { TdxSelection }

  TdxSelection = class(TdxSimpleSelection,
    IdxDocumentModelStructureChangedListener)
  strict private
    FItems: TdxSelectionItems;
    FTableSelectionStucture: TdxSelectedTableStructureBase;
    FSelectionGeneration: Integer;
    function GetInterval: TdxRunInfo;
    function GetDocumentModel: TdxDocumentModel;
    function GetActiveSelection: TdxSelectionItem;
    function GetFirst: TdxSelectionItem;
    function GetPieceTable: TdxPieceTable;
    function GetVirtualEnd: TdxDocumentLogPosition;
    function GetUsePreviousBoxBounds: Boolean;
    procedure SetUsePreviousBoxBounds(const AValue: Boolean);
    function GetNormalizedVirtualEnd: TdxDocumentLogPosition;
    procedure SetTableSelectionStucture(AValue: TdxSelectedTableStructureBase);
  protected
    FMultipleRunSplitCount: Integer;

    function GetIsSelectionChanged: Boolean; override;
    procedure SetIsSelectionChanged(const AValue: Boolean); override;

    function GetNormalizedStart: TdxDocumentLogPosition; override;
    function GetNormalizedEnd: TdxDocumentLogPosition; override;
    function GetStart: TdxDocumentLogPosition; override;
    procedure SetStart(const AValue: TdxDocumentLogPosition); override;
    function GetEnd: TdxDocumentLogPosition; override;
    procedure SetEnd(const AValue: TdxDocumentLogPosition); override;
    function GetIsMultiSelection: Boolean;
    function GetLength: Integer; override;
    function InitialSelection(APieceTable: TdxCustomPieceTable): TdxSelectionItem; virtual;
    procedure RaiseChanged;
    procedure TryAndMergeSelectionEnd(AValue: TdxDocumentLogPosition);
    procedure TryAndMergeSelectionStart(AValue: TdxDocumentLogPosition);
    function IsSelectionInTable: Boolean;
    function IsValidSelectedCells: Boolean;
    procedure OnSelectionChanged(ASender: TObject; E: TdxEventArgs);

    procedure OnLastEndUpdate; override;
    procedure OnLastCancelUpdate; override;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell;
      AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable); overload;
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable); overload;
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ASplitOffset: Integer); overload;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer); overload;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); overload;
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); overload;
  {$ENDREGION}
    procedure OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnBeginMultipleRunSplit; overload;
    procedure OnEndMultipleRunSplit; overload;
    procedure OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer); overload;
    procedure OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset: Integer; ATailRunLength: Integer); overload;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer); overload;
    procedure OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer); overload;
    procedure OnFieldInserted(AFieldIndex: Integer); overload;
    procedure OnFieldRemoved(AFieldIndex: Integer); overload;
    procedure UpdateStartPosition;
    procedure UpdateEndPosition;
    function GetSelectionCollection: TdxSelectionRangeCollection;
    function GetEndParagraphIndex(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex;
    function GetStartPositionInTableRow(ARow: TdxTableRow): TdxDocumentLogPosition;
    function CalculateSelectionStart(ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
    function DetermineStartSelectedCell(ASelectionStart: TdxDocumentLogPosition): TdxTableCell;
    function IsCellSelectedFully(ATableCell: TdxTableCell; ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): Boolean;
    function GetCellEndPosition(AStartCell: TdxTableCell): TdxDocumentLogPosition;
    function DetermineEndCellByLogPosition(ASelectionStart: TdxDocumentLogPosition;
      ASelectionEnd: TdxDocumentLogPosition; AConsiderCellStart: Boolean): TdxTableCell;
    function DetermineEndCellCore(ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): TdxTableCell;
    function EnsureCellIsSelected(ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition;
      ANextSelectedCell: TdxTableCell; APrevSelectedCell: TdxTableCell; AConsiderCellStart: Boolean): TdxTableCell;
    function IsCellSelected(ACell: TdxTableCell; ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): Boolean;
    function GetPreviousTableCell(ACell: TdxTableCell; AIsLeftToRightDirection: Boolean): TdxTableCell;
    function IsColumnsSelected(APrevSelectedCell: TdxTableCell; ANextSelectedCell: TdxTableCell): Boolean;
    function GetLastSelectedCell: TdxTableCell;
    function IsRowsSelected(APrevSelectedCell: TdxTableCell; ANextSelectedCell: TdxTableCell): Boolean;
    function GetActualNestedLevelConsiderStartCell(AStartCell: TdxTableCell): Integer;
    procedure NormalizeTableCellsToMinNestedLevel(var AStartCell, AEndCell: TdxTableCell);
    function GetEndPositionInTableRow(AInitialEnd: TdxDocumentLogPosition; ACell: TdxTableCell; ANestedLevel: Integer): TdxDocumentLogPosition;
    function GetLastNonCoveredByVerticalMergingCell(ARow: TdxTableRow): TdxTableCell;
    function GetFirstNonCoveredByVerticalMergingCell(ARow: TdxTableRow): TdxTableCell;
    procedure UpdateSelectionBy(ASelectedCells: TdxSelectedTableStructureBase {TdxSelectedCellsCollection});
    function IsLeftToRightDirection(ASelectedCells: TdxSelectedTableStructureBase {TdxSelectedCellsCollection}): Boolean;
    procedure UpdateInTableSelectionItem(AItem: TdxSelectionItem; ACurrentCellsInterval: TdxSelectedCellsIntervalInRow; AIsLeftToRightDirection: Boolean);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    procedure ClearOutdatedItems;
    procedure SetInterval(AStart, AEnd: TdxDocumentLogPosition);
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure CancelUpdate;
    function GetSelectedTableRows: TdxTableRowList;
    function GetSelectedParagraphs: TdxParagraphList;

    procedure AddSelection(ANewSelection: TdxSelectionItem);
    procedure BeginMultiSelection(AActivePiecetable: TdxPieceTable); overload;
    procedure BeginMultiSelection(ANewSelection: TdxSelectionItem); overload;
    procedure ClearMultiSelection(AClearFrom: Integer = 0); override;
    procedure ClearSelectionInTable;
    procedure Delete(Index: Integer);
    function FirstAndSecondCellHaveCommonTableButSecondCellNotParentForFirstCell(AFirst: TdxTableCell; ASecond: TdxTableCell): Boolean;
    function FirstCellIsParentCellForSecondCellsTable(AFirstCell: TdxTableCell; ASecondCell: TdxTableCell): Boolean;
    function GetSortedSelectionCollection: TdxSelectionRangeCollection;
    function IsFloatingObjectSelected: Boolean;
    function IsInlinePictureSelected: Boolean;
    function IsSelectFieldPictureResult: Boolean;
    function IsWholeSelectionInOneTable: Boolean;
    procedure ManuallySetTableSelectionStructureAndChangeSelection(AStartCell: TdxTableCell; AEndCell: TdxTableCell; AIsColumnSelected: Boolean); overload;
    procedure ManuallySetTableSelectionStructureAndChangeSelection(AStartCell: TdxTableCell; AEndCell: TdxTableCell); overload;
    procedure RemoveIntersectedSelectionItems(ALastInsertedItemsCount: Integer);
    procedure SetStartCell(ALogPosition: TdxDocumentLogPosition);
    procedure TryMergeByActiveSelection;
    procedure Unselect(ARange: TdxSelectionItem);
    procedure UpdateTableSelectionStart(ALogPosition: TdxDocumentLogPosition);
    procedure UpdateTableSelectionEnd(ALogPosition: TdxDocumentLogPosition; AConsiderCellStart: Boolean = False);

    property Interval: TdxRunInfo read GetInterval;
    property IsMultiSelection: Boolean read GetIsMultiSelection;
    property NormalizedVirtualEnd: TdxDocumentLogPosition read GetNormalizedVirtualEnd;
    property VirtualEnd: TdxDocumentLogPosition read GetVirtualEnd;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ActiveSelection: TdxSelectionItem read GetActiveSelection;
    property Items: TdxSelectionItems read FItems;
    property First: TdxSelectionItem read GetFirst;
    property SelectedCells: TdxSelectedTableStructureBase read FTableSelectionStucture write SetTableSelectionStucture;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property SelectionGeneration: Integer read FSelectionGeneration write FSelectionGeneration;
    property UsePreviousBoxBounds: Boolean read GetUsePreviousBoxBounds write SetUsePreviousBoxBounds;
    property IsSelectionChanged: Boolean read GetIsSelectionChanged write SetIsSelectionChanged;
  end;

  { TdxEmptySelection }

  TdxEmptySelection = class(TdxSelection)
  protected
    function InitialSelection(APieceTable: TdxCustomPieceTable): TdxSelectionItem; override;
  end;

  { TdxEmptySelectionItem }

  TdxEmptySelectionItem = class(TdxSelectionItem)
  protected
    procedure ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer); override;
    procedure RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); override;
    procedure RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer); override;
    procedure RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure RaiseChanged; override;
    procedure UpdateStartPosition; override;

    function GetStart: TdxDocumentLogPosition; override;{$IFNDEF CPUX64} final;{$ENDIF}
    function GetEnd: TdxDocumentLogPosition; override;{$IFNDEF CPUX64} final;{$ENDIF}
  public
    procedure UpdateEndPosition; override;
  end;

  { TdxMeasurementAndDrawingStrategy }

  TdxMeasurementAndDrawingStrategy = class abstract
  strict private
    FDocumentModel: TdxDocumentModel;
    FMeasurer: TdxBoxMeasurer;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    procedure Initialize; virtual;
    function CreateBoxMeasurer: TdxBoxMeasurer; virtual; abstract;
    function CreateDocumentPainter(AGraphics: TdxGraphics): TObject; virtual; abstract;
    procedure OnLayoutUnitChanged; virtual;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property Measurer: TdxBoxMeasurer read FMeasurer;
  end;

  IdxDocumentExporterFactory = interface
  ['{56C7EB9D-365E-4E50-ADBA-B85098D999D3}']
    function CreateHtmlExporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentExporterOptions): TObject;
  end;

  IdxDocumentImporterFactory = interface
  ['{7D845116-E9F1-437B-9304-E38D846BABF3}']
    function CreateHtmlImporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentImporterOptions): TObject;
  end;

  { TdxParagraphEventArgs }

  TdxParagraphEventArgs = class(TdxEventArgs)
  strict private
    FPieceTable: TdxCustomPieceTable;
    FSectionIndex: TdxSectionIndex;
    FParagraphIndex: TdxParagraphIndex;
  public
    constructor Create(APieceTable: TdxCustomPieceTable;
      ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex);

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property SectionIndex: TdxSectionIndex read FSectionIndex;
  end;

  TdxParagraphEvent = procedure(Sender: TObject; E: TdxParagraphEventArgs) of object;
  TdxParagraphEventHandler = TdxMulticastMethod<TdxParagraphEvent>;

  { TdxFieldEventArgs }

  TdxFieldEventArgs = class(TdxEventArgs)
  strict private
    FPieceTable: TdxCustomPieceTable;
    FFieldIndex: Integer;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property FieldIndex: Integer read FFieldIndex;
  end;

  TdxFieldEvent = procedure(Sender: TObject; E: TdxFieldEventArgs) of object;
  TdxFieldEventHandler = TdxMulticastMethod<TdxFieldEvent>;

  { TdxInternalAPI }

  TdxCustomInternalAPI = class abstract(TcxIUnknownObject)
  protected
    procedure ClearAnchors; virtual; abstract;
    function GetHtmlText: string; virtual; abstract;
    procedure SetHtmlText(const Value: string); virtual; abstract;
    function GetText: string; virtual; abstract;
    procedure SetText(const Value: string); virtual; abstract;
    function GetRtfText: string; virtual; abstract;
    procedure SetRtfText(const Value: string); virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    procedure SetModified(const Value: Boolean); virtual; abstract;
    function GetExporterFactory: IdxDocumentExporterFactory; virtual; abstract;
    function GetImporterFactory: IdxDocumentImporterFactory; virtual; abstract;
    procedure SetExporterFactory(const Value: IdxDocumentExporterFactory); virtual; abstract;
    procedure SetImporterFactory(const Value: IdxDocumentImporterFactory); virtual; abstract;
    function GetConverter(Index: TdxMeasurementUnit): TdxUnitConverter; virtual; abstract;
  public
    procedure RegisterAnchor(APos: TdxDocumentModelPositionAnchor); virtual; abstract;
    procedure UnregisterAnchor(APos: TdxDocumentModelPositionAnchor); virtual; abstract;
    procedure CreateNewDocument; virtual; abstract;
    function GetDocumentRtfContent(AOptions: TdxRtfDocumentExporterOptions = nil;
      ALastParagraphRunNotSelected: Boolean = False; AKeepFieldCodeViewState: Boolean = False;
      AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): string; virtual; abstract;
    function GetDocumentPlainTextContent(AOptions: TdxPlainTextDocumentExporterOptions = nil): string; virtual; abstract;
    function GetDocumentOpenXmlContent(AOptions: TdxOpenXmlDocumentExporterOptions): TArray<Byte>; virtual; abstract;
    function GetDocumentDocContent(AOptions: TdxDocDocumentExporterOptions): TArray<Byte>; virtual; abstract;
    procedure LoadDocumentRtfContent(AStream: TStream; AOptions: TdxRtfDocumentImporterOptions); virtual; abstract;
    procedure LoadDocumentPlainTextContent(AStream: TStream; AOptions: TdxPlainTextDocumentImporterOptions); virtual; abstract;
    procedure SaveDocumentPlainTextContent(AStream: TStream; AOptions: TdxPlainTextDocumentExporterOptions); virtual; abstract;
    procedure SaveDocumentRtfContent(AStream: TStream; AOptions: TdxRtfDocumentExporterOptions); virtual; abstract;
    procedure LoadDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentImporterOptions); virtual; abstract;
    procedure SaveDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentExporterOptions); virtual; abstract;
    procedure LoadDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentImporterOptions); virtual; abstract;
    procedure SaveDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentExporterOptions); virtual; abstract;
    procedure LoadDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentImporterOptions); virtual; abstract;
    procedure SaveDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentExporterOptions); virtual; abstract;

    property Text: string read GetText write SetText;
    property RtfText: string read GetRtfText write SetRtfText;
    property HtmlText: string read GetHtmlText write SetHtmlText;
    property Modified: Boolean read GetModified write SetModified;
    property ExporterFactory: IdxDocumentExporterFactory read GetExporterFactory write SetExporterFactory;
    property ImporterFactory: IdxDocumentImporterFactory read GetImporterFactory write SetImporterFactory;
    property UnitConverters[Index: TdxMeasurementUnit]: TdxUnitConverter read GetConverter;
  end;

  { TdxRichEditCommandsCreationStrategy }

  TdxRichEditCommandsCreationStrategy = class(TcxIUnknownObject)
  public
    function CreateNumberingListIndexCalculator(AModel: TdxDocumentModel; ANumberingListType: TdxNumberingType): TdxNumberingListIndexCalculator; virtual;
  end;

  { TdxSpellCheckerInterval }

  TdxSpellCheckerInterval = class abstract(TdxChangeableDocumentInterval)
  public
    procedure OnChangedCore; override;
  end;

  { TdxSpellCheckerIntervalCollection }

  TdxSpellCheckerIntervalCollection<T: TdxSpellCheckerInterval> = class abstract
  strict private
    type

      TSpellCheckerIntervalAndLogPositionComparer = class(TcxIUnknownObject, IdxComparable<T>)
      strict private
        FPosition: TdxDocumentLogPosition;
      public
        constructor Create(APosition: TdxDocumentLogPosition);
        function CompareTo(const AOther: T): Integer;
      end;

      TSpellCheckerIntervalAndIntervalComparer = class(TcxIUnknownObject{?}, IdxComparable<T>)
      strict private
        FStart: TdxDocumentLogPosition;
        FEnd: TdxDocumentLogPosition;
      public
        constructor Create(AStart, AEnd: TdxDocumentLogPosition);
        function CompareTo(const AOther: T): Integer;
      end;

  strict private
    FPieceTable: TdxSimplePieceTable;
    FInnerList: TdxObjectList<T>;
    function GetCount: Integer;
  protected
    function BinarySearch(APosition: TdxDocumentLogPosition): Integer; overload;
    function BinarySearch(AStart, AEnd: TdxDocumentLogPosition): Integer; overload;
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); virtual;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); virtual;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); virtual;
    procedure OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer); virtual;
    procedure OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;

    procedure RemoveRangeCore(const ARange: TArray<T>; ARangeDestroy: Boolean);
    procedure RemoveCore(AInterval: T; AIntervalDestroy: Boolean);

    property InnerList: TdxObjectList<T> read FInnerList;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Add(ANewInterval: T); overload; virtual;
    function GetIntervals(AStart, AEnd: TdxDocumentLogPosition): TArray<T>; virtual;
    function FindInterval(APosition: TdxDocumentLogPosition): T; overload; virtual;
    function FindInterval(AStart, AEnd: TdxDocumentLogPosition): T; overload; virtual;

    procedure RemoveRange(const ARange: TArray<T>); overload; virtual;
    procedure RemoveRange(AStart, AEnd: TdxDocumentLogPosition); overload; virtual;
    procedure Remove(AInterval: T); overload; virtual;
    procedure Remove(APosition: TdxDocumentLogPosition); overload; virtual;

    procedure ExtractRange(const ARange: TArray<T>); overload; virtual;
    procedure ExtractRange(AStart, AEnd: TdxDocumentLogPosition); overload; virtual;
    procedure Extract(AInterval: T); overload; virtual;
    procedure Extract(APosition: TdxDocumentLogPosition); overload; virtual;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property Count: Integer read GetCount;
  end;

  { TdxMisspelledInterval }

  TdxMisspelledInterval = class(TdxSpellCheckerInterval)
  strict private
    FErrorType: TdxSpellingError;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AErrorType: TdxSpellingError); reintroduce;
    procedure OnChangedCore; override;

    property ErrorType: TdxSpellingError read FErrorType write FErrorType;
  end;

  TdxUncheckedInterval = class(TdxSpellCheckerInterval);

  { TdxIgnoredList }

  TdxIgnoredList = class(TdxSpellCheckerIntervalCollection<TdxMisspelledInterval>)
  strict private
    FIgnoreAllList: TdxStringList;
  protected
    property IgnoreAllList: TdxStringList read FIgnoreAllList;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    destructor Destroy; override;

    function Contains(AStart, AEnd: TdxDocumentLogPosition; const AWord: string): Boolean; overload; virtual;
    function Contains(const AWord: string): Boolean; overload; virtual;
    procedure Add(ANewInterval: TdxMisspelledInterval); overload; override;
    procedure Add(const AStart, AEnd: TdxDocumentModelPosition); overload;
    procedure Add(const AWord: string); overload; virtual;
    function Remove(const AWord: string): Boolean; overload; virtual;
    procedure Clear; override;
  end;

  { TdxMisspelledIntervalCollection }

  TdxMisspelledIntervalCollection = class(TdxSpellCheckerIntervalCollection<TdxMisspelledInterval>)
  public
    procedure AddIfNotExists(ANewInterval: TdxMisspelledInterval); virtual;
  end;

  { TdxUncheckedIntervalCollection }

  TdxUncheckedIntervalCollection = class(TdxSpellCheckerIntervalCollection<TdxUncheckedInterval>)
  protected
    function SplitInterval(AIndex: Integer; const APosition: TdxDocumentModelPosition): Boolean; virtual;
    procedure TryMergeIntervals(AIndex: Integer); virtual;
    function CanMerge(AFirstIndex: Integer; ALastIndex: Integer): Boolean;
    procedure Merge(AFirstIndex: Integer; ALastIndex: Integer);
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); override;
  public
    function ObtainIntervals(const AStart, AEnd: TdxDocumentModelPosition): TArray<TdxUncheckedInterval>; virtual;
    procedure Add(ANewInterval: TdxUncheckedInterval); override;
  end;

  { TdxRichEditSpellCheckerManager }

  TdxRichEditSpellCheckerManager = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  strict private
    FPieceTable: TdxSimplePieceTable;
    FMisspelledIntervals: TdxMisspelledIntervalCollection;
    FUncheckedIntervals: TdxUncheckedIntervalCollection;
    FIgnoredIntervals: TdxIgnoredList;
    FModifiedWordStart: TdxDocumentLogPosition;
    FModifiedWordEnd: TdxDocumentLogPosition;
    FShouldCheckDocument: Boolean;
  protected
    procedure Clear; virtual;
    procedure ClearIntervals; virtual;
    procedure ClearModifiedWordInfo; virtual;
    procedure ResetModification;
    procedure CalculateUncheckedInterval(const AStart, AEnd: TdxDocumentModelPosition); virtual;
    procedure CalculateModifiedWordInfo(const AEnd: TdxDocumentModelPosition);
    procedure HandleSelectionChanged;
    procedure UnhandleSelectionChanged;
    function IsModifiedWord(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
    procedure ProcessModifiedWord;
    function IsRangeEditable(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
    function IsRangeVisible(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
    procedure CalculateUncheckedIntervalCore(const AStart, AEnd: TdxDocumentModelPosition); virtual;
  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer); overload;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); overload;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
  {$ENDREGION}
    procedure OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload; virtual;
    procedure OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload; virtual;
    procedure OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload; virtual;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload; virtual;
    procedure OnRunInsertedCore(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer); overload; virtual;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload; virtual;
    procedure OnRunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); overload; virtual;
    procedure OnRunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); overload; virtual;
    procedure OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload; virtual;
    procedure OnSelectionChanged(ASender: TObject);
  public
    constructor Create(APieceTable: TdxSimplePieceTable);
    destructor Destroy; override;

    function CreateInstance(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager; virtual;

    function PopUncheckedIntervals(const AStart, AEnd: TdxDocumentModelPosition): TArray<TdxSpellCheckerInterval>; virtual;
    procedure RemoveMisspelledIntervals(AStart, AEnd: TdxDocumentLogPosition); virtual;
    procedure CreateMisspelledInterval(const AStart, AEnd: TdxDocumentModelPosition; AErrorType: TdxSpellingError); virtual;
    procedure InitializeUncheckedInterval; virtual;

    procedure Initialize; virtual;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property MisspelledIntervals: TdxMisspelledIntervalCollection read FMisspelledIntervals;
    property UncheckedIntervals: TdxUncheckedIntervalCollection read FUncheckedIntervals;
    property IgnoredList: TdxIgnoredList read FIgnoredIntervals;
    property ModifiedWordStart: TdxDocumentLogPosition read FModifiedWordStart;
    property ModifiedWordEnd: TdxDocumentLogPosition read FModifiedWordEnd;
    property ShouldCheckDocument: Boolean read FShouldCheckDocument write FShouldCheckDocument;
  end;

  { TdxEmptySpellCheckerManager }

  TdxEmptySpellCheckerManager = class(TdxRichEditSpellCheckerManager)
  protected
    procedure Clear; override;
    procedure OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); override;
    procedure OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer); override;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); override;
    procedure OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); override;
    procedure OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
  public
    function CreateInstance(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager; override;
    procedure Initialize; override;
  end;

  { TdxCustomMark }

  TdxCustomMark = class(TdxDocumentModelPositionAnchor)
  strict private
    FUserData: TObject;
  public
    constructor Create(const APos: PdxDocumentModelPosition; AUserData: TObject);

    property UserData: TObject read FUserData;
  end;

function IsFormatterPositionEquals(const APos1, APos2: TdxFormatterPosition): Boolean;

implementation

uses
  RTLConsts, Contnrs, Math, Variants, StrUtils, Character, IOUtils,
  cxVariants, dxTypeHelpers, dxHash, dxHashUtils, dxThreading,

  dxRichEdit.Commands,
  dxRichEdit.Control.AutoCorrect,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.VisibleTextFilter,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Commands.Simple,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.FieldDataService,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Fields.TocField,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.Export.DocumentExportHelper,
  dxRichEdit.Import.DocumentImportHelper,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxCharacters,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.Utils.UriStreamService,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.History.Paragraph,
  dxRichEdit.DocumentModel.History.Run,
  dxRichEdit.DocumentModel.History.Table,
  dxRichEdit.DocumentModel.History.Section,
  dxRichEdit.DocumentModel.History.BookmarkHistory,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.DocumentModel.History.Protection,

  dxRichEdit.DocumentModel.PieceTable.InternalAPI;

type
  { TdxDocumentModelChangeActionsCalculator }

  TdxDocumentModelChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxDocumentModelChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionParagraphIndexComparable }

  TdxSectionParagraphIndexComparable = class(TcxIUnknownObject, IdxComparable<TdxCustomSection>)
  strict private
    FParagraphIndex: TdxParagraphIndex;
  public
    constructor Create(AParagraphIndex: TdxParagraphIndex);
    function CompareTo(const ASection: TdxCustomSection): Integer;

    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
  end;

function IsFormatterPositionEquals(const APos1, APos2: TdxFormatterPosition): Boolean;
begin
  Result := (APos1.Offset = APos2.Offset) and (APos1.RunIndex = APos2.RunIndex);
end;

function CalculateFormattingRunIndex(ARuns: TdxTextRunCollection; ARunIndex: TdxRunIndex): TdxRunIndex;
var
  APrevRunIndex, ANextRunIndex: TdxRunIndex;
begin
  APrevRunIndex := ARunIndex - 1;
  ANextRunIndex := ARunIndex + 1;
  while True do
  begin
    if (APrevRunIndex < 0) or (ARuns[APrevRunIndex] is TdxParagraphRun) then
    begin
      if ARuns[ANextRunIndex] is TdxSeparatorTextRun then
        Inc(ANextRunIndex)
      else
        Exit(ANextRunIndex);
    end
    else
    begin
      if ARuns[APrevRunIndex] is TdxSeparatorTextRun then
        Dec(APrevRunIndex)
      else
        Exit(APrevRunIndex);
    end;
  end;
 // Result := -1;
end;

{ TdxDocumentUpdateCompleteEventArgs }

constructor TdxDocumentUpdateCompleteEventArgs.Create(ADeferredChanges: TdxDocumentModelDeferredChanges);
begin
  inherited Create;
  Assert(ADeferredChanges <> nil);
  FDeferredChanges := ADeferredChanges;
end;

{ TdxSectionParagraphIndexComparable }

constructor TdxSectionParagraphIndexComparable.Create(AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create;
  FParagraphIndex := AParagraphIndex;
end;

function TdxSectionParagraphIndexComparable.CompareTo(const ASection: TdxCustomSection): Integer;
begin
  if ASection.LastParagraphIndex < FParagraphIndex then
    Result := -1
  else
    if ASection.FirstParagraphIndex > FParagraphIndex then
      Result := 1
    else
      Result := 0;
end;

{ TdxSafeDocumentModelEditor }

constructor TdxSafeDocumentModelEditor.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel is TdxDocumentModel);
  FDocumentModel := ADocumentModel;
end;

function TdxSafeDocumentModelEditor.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.MainPieceTable;
end;

function TdxSafeDocumentModelEditor.GetUnsafeEditor: TdxUnsafeDocumentModelEditor;
begin
  Result := DocumentModel.UnsafeEditor;
end;

procedure TdxSafeDocumentModelEditor.InsertSectionCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition);
begin
  InsertSectionCore(AParagraphIndex, ALogPosition, False);
end;

procedure TdxSafeDocumentModelEditor.InsertSectionCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  ATransaction: TdxHistoryTransaction;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    PieceTable.InsertSectionParagraphCore(AParagraphIndex, ALogPosition, AForceVisible);
    PerformInsertSectionCore(AParagraphIndex);
  finally
    ATransaction.Free;
  end;
end;

procedure TdxSafeDocumentModelEditor.PerformInsertSectionCore(AParagraphIndex: TdxParagraphIndex);
var
  AItem: TdxSectionInsertedHistoryItem;
begin
  AItem := TdxSectionInsertedHistoryItem.Create(DocumentModel);
  AItem.ParagraphIndex := AParagraphIndex;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

{ TdxDocumentModel }

class constructor TdxDocumentModel.Initialize;
begin
  FDefaultUnderlineRepository := TdxUnderlineRepository.Create;
  FDefaultStrikeoutRepository := TdxStrikeoutRepository.Create;
end;

class destructor TdxDocumentModel.Finalize;
begin
  FreeAndNil(FDefaultUnderlineRepository);
  FreeAndNil(FDefaultStrikeoutRepository);
end;

constructor TdxDocumentModel.Create;
begin
  Create(TdxDocumentModelDpi.DpiX, TdxDocumentModelDpi.DpiY);
end;

constructor TdxDocumentModel.Create(ADpiX, ADpiY: Single);
begin
  inherited Create(ADpiX, ADpiY);
  FMailMergeDataController := CreateMailMergeDataController;
  SubscribeMailMergeDataControllerEvents;
  AddServices;
  Initialize;
end;

destructor TdxDocumentModel.Destroy;
begin
  TdxUIThreadSyncService.Unsubscribe(Self);
  FreeAndNil(FCommandsCreationStrategy);
  FreeAndNil(FInternalAPI);
  FreeAndNil(FSearchParameters);
  FreeAndNil(FSearchContext);
  FreeAndNil(FMailMergeDataController);
  inherited Destroy;
end;

function TdxDocumentModel.GetActivePieceTableCore: TdxCustomPieceTable;
begin
  Result := FActivePieceTable;
end;

procedure TdxDocumentModel.AddAbstractNumberingListUsingHistory(AAbstractList: TdxAbstractNumberingList);
var
  AHistoryItem: TdxAddAbstractNumberingListHistoryItem;
begin
  AHistoryItem := TdxAddAbstractNumberingListHistoryItem.Create(ActivePieceTable);
  AHistoryItem.AbstractList := AAbstractList;
  History.Add(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxDocumentModel.AddHistoryItem(AItem: TdxHistoryItem): TdxHistoryItem;
begin
  Assert(AItem is TdxHistoryItem);
  History.Add(AItem);
  Result := AItem;
end;

function TdxDocumentModel.AutodetectDocumentFormat(const AFileName: string; AUseFormatFallback: Boolean = True): TdxRichEditDocumentFormat;
var
  AImportHelper: TdxImportHelper<TdxRichEditDocumentFormat, Boolean>;
  AImportManagerService: IdxImportManagerService<TdxRichEditDocumentFormat, Boolean>;
  AImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  AImportHelper := TdxImportHelper<TdxRichEditDocumentFormat, Boolean>(CreateDocumentImportHelper);
  try
    if not Supports(GetImportManagerService, IdxImportManagerService<TdxRichEditDocumentFormat, Boolean>, AImportManagerService) then
      Exit(AImportHelper.UndefinedFormat);
    if AImportManagerService = nil then
      Exit(AImportHelper.UndefinedFormat);

    AImporter := AImportHelper.AutodetectImporter(AFileName, AImportManagerService, AUseFormatFallback);
    if AImporter = nil then
      Result := AImportHelper.UndefinedFormat
    else
      Result := AImporter.Format;
  finally
    AImportHelper.Free;
  end;
end;

procedure TdxDocumentModel.AddDataServices;
var
  AService: TdxMailMergeDataService;
begin
  AService := TdxMailMergeDataService.Create(MailMergeDataController);
  AddService(IdxMailMergeDataService, AService);
  AddService(IdxFieldDataService, AService);
end;

procedure TdxDocumentModel.AddNumberingListUsingHistory(ANumberingList: TdxNumberingList);
var
  AHistoryItem: TdxAddNumberingListHistoryItem;
begin
  AHistoryItem := TdxAddNumberingListHistoryItem.Create(ActivePieceTable);
  AHistoryItem.NumberingList := ANumberingList;
  History.Add(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxDocumentModel.AddServices;
begin
  AddService(IdxDocumentImportManagerService, CreateDocumentImportManagerService);
  AddService(IdxDocumentExportManagerService, CreateDocumentExportManagerService);
  AddService(IdxUriStreamService, TdxUriStreamService.Create);
  AddService(IdxUriProviderService, TdxUriProviderService.Create);
  AddDataServices;
  AddService(IdxFieldCalculatorService, TdxFieldCalculatorService.Create);
  AddService(IdxRichEditProgressIndicationService, TdxRichEditProgressIndicationService.Create(Self));
  AddService(IdxHyphenationService, CreateHyphenationService);
end;

procedure TdxDocumentModel.ApplyChanges(APieceTable: TdxCustomPieceTable{TdxPieceTable}; AChangeType: TdxDocumentModelChangeType;
  AStartRunIndex, AEndRunIndex: TdxRunIndex);
var
  AActions: TdxDocumentModelChangeActions;
begin
  Assert(IsUpdateLockedOrOverlapped);
  AActions := TdxDocumentModelChangeActionsCalculator.CalculateChangeActions(AChangeType);
  ApplyChangesCore(APieceTable, AActions, AStartRunIndex, AEndRunIndex);
end;

procedure TdxDocumentModel.ApplyChangesCore(APieceTable: TdxCustomPieceTable{TdxPieceTable}; AActions: TdxDocumentModelChangeActions;
  AStartRunIndex, AEndRunIndex: TdxRunIndex);
begin
  if AActions = [] then
    Exit;
  Assert(IsUpdateLocked or BatchUpdateHelper.OverlappedTransaction);
  DeferredChanges.ApplyChanges(TdxPieceTable(APieceTable), AActions, AStartRunIndex, AEndRunIndex);
end;

procedure TdxDocumentModel.ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxSectionPropertyModifierBase);
begin
  if ALogPositionStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPositionStart', ALogPositionStart);
  if ALength <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('length', ALength);

  ApplySectionFormattingCore(ALogPositionStart, ALength, AModifier);
end;

procedure TdxDocumentModel.ApplySectionFormattingCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxSectionPropertyModifierBase);
var
  I: Integer;
  ATransaction: TdxHistoryTransaction;
  ALastSectionIndex: TdxSectionIndex;
begin
  ATransaction := TdxHistoryTransaction.Create(History);
  try
    ALastSectionIndex := FindSectionIndex(ALogPositionStart + ALength - 1);
    for I := FindSectionIndex(ALogPositionStart) to ALastSectionIndex do
      AModifier.ModifySection(Sections[I], I);
  finally
    FreeAndNil(ATransaction);
  end;
end;

procedure TdxDocumentModel.BeforeFloatingObjectDrop(AOldPosition, ANewPosition: TdxDocumentLogPosition;
  APieceTable: TdxPieceTable);
begin
end;

procedure TdxDocumentModel.SaveDocument(AStream: TStream;
  ADocumentFormat: TdxRichEditDocumentFormat; const ATargetUri: string = ''; AEncoding: TEncoding = nil);
var
  AExportHelper: TdxExportHelper;
  AExportManagerService: IdxExportManagerService;
begin
  AExportHelper := CreateDocumentExportHelper(ADocumentFormat) as TdxExportHelper;
  try
    AExportManagerService := GetExportManagerService as IdxExportManagerService;
    if AExportManagerService = nil then
      AExportHelper.ThrowUnsupportedFormatException;
    AExportHelper.Export(AStream, ADocumentFormat, ATargetUri, AExportManagerService, AEncoding);
  finally
    AExportHelper.Free;
  end;
end;

procedure TdxDocumentModel.RaiseBeginDocumentUpdate;
begin
  if not FOnBeginDocumentUpdate.Empty then
    FOnBeginDocumentUpdate.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseEndDocumentUpdate;
var
  AArgs: TdxDocumentUpdateCompleteEventArgs;
begin
  if FOnEndDocumentUpdate.Empty then
    Exit;
  AArgs := TdxDocumentUpdateCompleteEventArgs.Create(DeferredChanges);
  try
    FOnEndDocumentUpdate.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentModel.RaiseHyperlinkInfoDeleted(APieceTable: TdxSimplePieceTable;
  AFieldIndex: Integer);
var
  AArgs: TdxHyperlinkInfoEventArgs;
begin
  if FOnHyperlinkInfoDeleted.Empty then
    Exit;
  AArgs := TdxHyperlinkInfoEventArgs.Create(APieceTable, AFieldIndex);
  try
    FOnHyperlinkInfoDeleted.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentModel.RaiseHyperlinkInfoInserted(
  APieceTable: TdxSimplePieceTable; AFieldIndex: Integer);
var
  Args: TdxHyperlinkInfoEventArgs;
begin
  if FOnHyperlinkInfoInserted.Empty then
    Exit;
  Args := TdxHyperlinkInfoEventArgs.Create(APieceTable, AFieldIndex);
  try
    FOnHyperlinkInfoInserted.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxDocumentModel.RaiseBeforeEndDocumentUpdate;
var
  AArgs: TdxDocumentUpdateCompleteEventArgs;
begin
  if FOnBeforeEndDocumentUpdate.Empty then
    Exit;
  AArgs := TdxDocumentUpdateCompleteEventArgs.Create(DeferredChanges);
  try
    FOnBeforeEndDocumentUpdate.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentModel.RaiseAfterEndDocumentUpdate;
var
  AArgs: TdxDocumentUpdateCompleteEventArgs;
begin
  if FOnAfterEndDocumentUpdate.Empty then
    Exit;
  AArgs := TdxDocumentUpdateCompleteEventArgs.Create(DeferredChanges);
  try
    FOnAfterEndDocumentUpdate.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentModel.RaiseInnerContentChanged;
begin
  if not FOnInnerContentChanged.Empty then
    FOnInnerContentChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseInnerDocumentCleared;
begin
  if not FOnInnerDocumentCleared.Empty then
    FOnInnerDocumentCleared.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseContentChanged;
begin
  if not FOnContentChanged.Empty then
    FOnContentChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseDocumentCleared;
begin
  if not FOnDocumentCleared.Empty then
    FOnDocumentCleared.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseModifiedChanged;
begin
  if not FOnModifiedChanged.Empty then
    FOnModifiedChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.RaiseAfterExport;
begin
  if not FOnAfterExport.Empty then
    FOnAfterExport.Invoke(Self);
end;

procedure TdxDocumentModel.RaisePageBackgroundChanged;
begin
  if not FOnPageBackgroundChanged.Empty then
    FOnPageBackgroundChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentModel.OnMailMergeCurrentRowChanged(Sender: TObject; E: TdxEventArgs);
begin
end;

procedure TdxDocumentModel.OnMailMergeViewMergedDataChanged(Sender: TObject; E: TdxEventArgs);
begin
  MailMergeOptions.ViewMergedData := MailMergeProperties.ViewMergedData;
  UpdateFields(TdxUpdateFieldOperationType.Normal);
end;

procedure TdxDocumentModel.OnMailMergeDataSourceChanged(Sender: TObject; E: TdxEventArgs);
begin
  MailMergeOptions.DataSource := MailMergeDataController.DataSource;
  UpdateFields(TdxUpdateFieldOperationType.Normal);
end;

procedure TdxDocumentModel.BeginFieldsUpdate;
begin
  DeferredChanges.IsSetContentMode := False;
  FSuppressFieldsChangeNotification := True;
end;

procedure TdxDocumentModel.BeginSetContent;
var
  I: Integer;
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
begin
  if IsUpdateLocked and DeferredChanges.IsSetContentMode then
    TdxRichEditExceptions.ThrowInternalException;
  APieceTables := GetPieceTables(True);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := APieceTables[I];
      APieceTable.OnBeginSetContent;
    end;
  finally
    APieceTables.Free;
  end;
  BeginUpdate;
  DeferredChanges.IsSetContentMode := True;
  InternalAPI.ClearAnchors;
  SwitchToEmptyHistory(True);
  ClearDocument;
  SetActivePieceTable(MainPieceTable, nil);
  SwitchToEmptySelection;
  RaiseInnerDocumentCleared;
  RaiseDocumentCleared;
end;

function TdxDocumentModel.CreateDocumentModelCopyOptions(AFrom: TdxDocumentLogPosition; ALength: Integer): TdxDocumentModelCopyOptions;
begin
  Result := TdxDocumentModelCopyOptions.Create(AFrom, ALength);
end;

procedure TdxDocumentModel.SetModelForExportCopyOptions(ACopyOptions: TdxDocumentModelCopyOptions);
begin
  ACopyOptions.DefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Always;
  ACopyOptions.CopyDocumentVariables := True;
end;

procedure TdxDocumentModel.CopyDocumentModelOptions(ADestinationModel: TdxDocumentModel);
begin
  ADestinationModel.FieldOptions.CopyFrom(FieldOptions);
  ADestinationModel.PrintingOptions.UpdateDocVariablesBeforePrint := PrintingOptions.UpdateDocVariablesBeforePrint;
end;


function TdxDocumentModel.CreateDocumentModelForExport(const AInitializeEmptyDocumentModel: TdxAction<TdxDocumentModel>): TdxDocumentModel;
var
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  ACopyOptions: TdxDocumentModelCopyOptions;
  AService: IdxDocumentLayoutService;
  ACommand: TdxDocumentModelCopyCommand;
begin
  Result := CreateNew;
  Result.DisableCheckDocumentModelIntegrity := DisableCheckDocumentModelIntegrity;
  Result.History.DisableHistory;
  Result.LayoutUnit := LayoutUnit;
  if Result.LayoutUnit = TdxDocumentLayoutUnit.Pixel then
    Result.LayoutUnit := TdxDocumentLayoutUnit.Document;
  AInitializeEmptyDocumentModel(Result);
  SubscribeEventsForExport(Result);
  Result.LayoutOptions.PrintLayoutView.AllowTablesToExtendIntoMargins := LayoutOptions.PrintLayoutView.AllowTablesToExtendIntoMargins;
  Result.BookmarkOptions.AllowNameResolution := BookmarkOptions.AllowNameResolution;
  AStartLogPosition := MainPieceTable.DocumentStartLogPosition;
  AEndLogPosition := MainPieceTable.DocumentEndLogPosition;
  ACopyOptions := CreateDocumentModelCopyOptions(AStartLogPosition, AEndLogPosition - AStartLogPosition + 1);
  try
    SetModelForExportCopyOptions(ACopyOptions);
    CopyDocumentModelOptions(Result);
    Result.BeginSetContentForExport;
    Result.InheritServicesForExport(Self);
    Result.ExtendedDocumentProperties.SetPages(ExtendedDocumentProperties.Pages);
    AService := GetService<IdxDocumentLayoutService>;
    if AService <> nil then
      AService.CreateService(Result);
    try
      ACommand := CreateDocumentModelCopyCommand(MainPieceTable, Result, ACopyOptions) as TdxDocumentModelCopyCommand;
      try
        ACommand.UpdateFieldOperationType := TdxUpdateFieldOperationType.CreateModelForExport;
        ACommand.UpdateIntervals := True;
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
      Result.UpdateTableOfContents;
      Result.MainPieceTable.FixLastParagraph;
    finally
      if AService <> nil then
        AService.RemoveService(Result);
      Result.EndSetContentForExport(TdxDocumentModelChangeType.LoadNewDocument, False);
    end;
    Result.DocumentProperties.CopyFrom(DocumentProperties.Info);
    Result.DocumentExportOptions.CopyFrom(DocumentExportOptions);

    Result.PrintingOptions.EnablePageBackgroundOnPrint := PrintingOptions.EnablePageBackgroundOnPrint;
    Result.History.EnableHistory;
  finally
    ACopyOptions.Free;
  end;
end;

procedure TdxDocumentModel.BeginSetContentForExport;
begin
  ModelForExport := True;
  BeginSetContent;
end;

function TdxDocumentModel.CanEditSection(ASection: TdxSection): Boolean;
var
  AStart, AEnd: TdxDocumentLogPosition;
  ALastParagraph: TdxParagraph;
begin
  AStart := MainPieceTable.Paragraphs[ASection.FirstParagraphIndex].LogPosition;
  ALastParagraph := MainPieceTable.Paragraphs[ASection.LastParagraphIndex];
  AEnd := ALastParagraph.LogPosition + ALastParagraph.Length;
  Result := MainPieceTable.CanEditRange(AStart, AEnd);
end;

function TdxDocumentModel.CheckDocumentProtectionPassword(const APassword: string): Boolean;
var
  ACalculatorClass: TdxPasswordHashCodeCalculatorClass;
begin
  ACalculatorClass := TdxPasswordHashCodeCalculator;
  case DocumentSaveOptions.CurrentFormat of
    TdxRichEditDocumentFormat.Rtf:
      Result := CheckRtfDocumentProtectionPassword(ACalculatorClass, APassword);
    TdxRichEditDocumentFormat.OpenXml:
      Result := CheckOpenXmlDocumentProtectionPassword(ACalculatorClass, APassword);
  else
    Result := CheckDocumentProtectionPasswordCore(ACalculatorClass, APassword);
  end;
end;

function TdxDocumentModel.CheckDocumentProtectionPasswordCore(
  ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
begin
 if (ProtectionProperties.OpenOfficePasswordHash <> nil) and (Length(ProtectionProperties.OpenOfficePasswordHash) > 0) then
    Result := CheckOpenOfficeDocumentProtectionPassword(ACalculator, APassword)
  else
    Result := CheckRtfDocumentProtectionPassword(ACalculator, APassword);
end;

function TdxDocumentModel.CheckIsOverrideEquals(ATargetNumberingList, ASourceNumberingList: TdxNumberingList): Boolean;
var
  I: Integer;
  ASourceLevels, ATargetLevels: TdxListLevelCollection;
begin
  ASourceLevels := ASourceNumberingList.Levels;
  ATargetLevels := ATargetNumberingList.Levels;

  if ASourceLevels.Count <> ATargetLevels.Count then
    Exit(False);

  for I := 0 to ASourceNumberingList.Levels.Count - 1 do
    if (ASourceLevels[I].OverrideStart <> ATargetLevels[I].OverrideStart) or
      (ASourceLevels[I].NewStart <> ATargetLevels[I].NewStart) then
        Exit(False);
  Result := True;
end;

function TdxDocumentModel.CheckLegacyDocumentProtectionPassword(
  ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
begin
  Result := Boolean(NotImplemented);
end;

function TdxDocumentModel.CheckOpenOfficeDocumentProtectionPassword(
  ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
var
  AHash: TArray<Byte>;
begin
  AHash := ACalculator.CalculateRichEditPasswordHash(APassword, ProtectionProperties.PasswordPrefix,
    ProtectionProperties.HashIterationCount, ProtectionProperties.HashAlgorithmType);
  Result := CheckPasswordHash(AHash, ProtectionProperties.PasswordHash);
end;

function TdxDocumentModel.CheckOpenXmlDocumentProtectionPassword(
  ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
var
  AHash: TArray<Byte>;
begin
  AHash := ACalculator.CalculateRichEditPasswordHash(APassword, ProtectionProperties.PasswordPrefix,
    ProtectionProperties.HashIterationCount, ProtectionProperties.HashAlgorithmType);
  Result := CheckPasswordHash(AHash, ProtectionProperties.PasswordHash);
end;

function TdxDocumentModel.CheckPasswordHash(const AHash, AExpectedHash: TArray<Byte>): Boolean;
begin
  if (AExpectedHash = nil) or (Length(AExpectedHash) <= 0) then
    Result := True
  else
    Result := TdxByteArray.Compare(AHash, AExpectedHash);
end;

function TdxDocumentModel.CheckRtfDocumentProtectionPassword(
  ACalculator: TdxPasswordHashCodeCalculatorClass; const APassword: string): Boolean;
begin
  if (ProtectionProperties.Word2003PasswordHash <> nil) and (Length(ProtectionProperties.Word2003PasswordHash) > 0) then
    Result := CheckLegacyDocumentProtectionPassword(ACalculator, APassword)
  else
    Result := CheckOpenXmlDocumentProtectionPassword(ACalculator, APassword);
end;

procedure TdxDocumentModel.ClearCore;
begin
  inherited ClearCore;

  FreeAndNil(FDefaultTableProperties);
  FreeAndNil(FDefaultTableRowProperties);
  FreeAndNil(FDefaultTableCellProperties);
  FreeAndNil(FEncryptionProperties);
  if FDocumentProperties <> nil then
  begin
    UnsubscribeDocumentPropertiesEvents;
    FreeAndNil(FDocumentProperties);
  end;
  if FMailMergeProperties <> nil then
  begin
    UnsubscribeMailMergePropertiesEvents;
    FreeAndNil(FMailMergeProperties);
  end;
  if FProtectionProperties <> nil then
  begin
    UnsubscribeProtectionPropertiesEvents;
    FreeAndNil(FProtectionProperties);
  end;
  if FLayoutOptions <> nil then
  begin
    UnsubscribeRichEditLayoutOptions;
    FreeAndNil(FLayoutOptions);
  end;

  FreeAndNil(FUnderlineRepository);
  FreeAndNil(FBorderLineRepository);
  FreeAndNil(FStrikeoutRepository);
end;

procedure TdxDocumentModel.ClearDataSources;
begin
//do nothing
end;

procedure TdxDocumentModel.BeginClearDocument;
begin
  inherited BeginClearDocument;
  UnsubscribeDocumentPropertiesEvents;
  UnsubscribeProtectionPropertiesEvents;
end;

procedure TdxDocumentModel.EndClearDocument;
begin
  UnsafeEditor.InsertFirstParagraph(MainPieceTable);
  UnsafeEditor.InsertFirstSection;

  RecreateLineNumberRun;
  inherited EndClearDocument;
  SubscribeDocumentPropertiesEvents;
  SubscribeProtectionPropertiesEvents;
end;

procedure TdxDocumentModel.ClearDocumentCore;
begin
  inherited ClearDocumentCore;
end;

procedure TdxDocumentModel.ClearDocumentContent;
begin
  inherited ClearDocumentContent;
  FAbstractNumberingListIdProvider.Reset;
  FNumberingListIdProvider.Reset;
  FAbstractNumberingLists.Clear;
  FNumberingLists.Clear;
  FHeaders.Clear;
  FFooters.Clear;
  FVariables.Clear;
  FFootNotes.Clear;
  FEndNotes.Clear;
  FreeAndNil(FEmptyBottomBorder);
  FreeAndNil(FEmptyTopBorder);
  FreeAndNil(FUnderlineRepository);
  FUnderlineRepository := TdxUnderlineRepository.Create;
  FreeAndNil(FBorderLineRepository);
  FBorderLineRepository := TdxBorderLineRepository.Create;
  FreeAndNil(FStrikeoutRepository);
  FStrikeoutRepository := TdxStrikeoutRepository.Create;
  FNumberDecimalSeparator := TdxCultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;
  FNumberGroupSeparator := TdxCultureInfo.CurrentCulture.NumberFormat.NumberGroupSeparator;
end;

procedure TdxDocumentModel.ClearDocumentStyles;
begin
  inherited ClearDocumentStyles;
  FTableStyles.Free;
  FTableStyles := TdxTableStyleCollection.Create(Self, True);
  FTableCellStyles.Free;
  FTableCellStyles := TdxTableCellStyleCollection.Create(Self, True);
  FNumberingListStyles.Free;
  FNumberingListStyles := TdxNumberingListStyleCollection.Create(Self);
  InitializeDefaultStyles;
end;

procedure TdxDocumentModel.ClearDocumentDefaultPropertiesCore;
begin
  inherited ClearDocumentDefaultPropertiesCore;
  FDefaultTableProperties.Free;
  FDefaultTableProperties := TdxTableProperties.Create(MainPieceTable);
  FDefaultTableRowProperties.Free;
  FDefaultTableRowProperties := TdxTableRowProperties.Create(MainPieceTable);
  FDefaultTableCellProperties.Free;
  FDefaultTableCellProperties := TdxTableCellProperties.Create(MainPieceTable, Self);
end;

procedure TdxDocumentModel.ClearDocumentProperties;
begin
  FEncryptionProperties.Free;
  FEncryptionProperties := TdxDocumentEncryptionProperties.Create(Self);
  FDocumentProperties.Free;
  FDocumentProperties := TdxDocumentProperties.Create(Self);
  FProtectionProperties.Free;
  FProtectionProperties := TdxDocumentProtectionProperties.Create(Self);
  FExtendedDocumentProperties.Free;
  FExtendedDocumentProperties := TdxExtendedDocumentProperties.Create;
  FHtmlSettings.Free;
  FHtmlSettings := TdxWebSettings.Create;
end;

procedure TdxDocumentModel.ResetDocumentFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
  inherited ResetDocumentFormattingCaches(AResetFormattingCacheType);
  ClearModelCachedIndices(AResetFormattingCacheType);
end;

procedure TdxDocumentModel.ClearModelCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
  if AResetFormattingCacheType in [TdxResetFormattingCacheType.Paragraph, TdxResetFormattingCacheType.All] then
    ParagraphStyles.ResetCachedIndices(AResetFormattingCacheType);
  if AResetFormattingCacheType in [TdxResetFormattingCacheType.Character, TdxResetFormattingCacheType.All] then
    CharacterStyles.ResetCachedIndices(AResetFormattingCacheType);
  TableStyles.ResetCachedIndices(AResetFormattingCacheType);

  LineNumberRun.ResetCachedIndices(AResetFormattingCacheType);
end;

function TdxDocumentModel.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = DefaultTableCellProperties);
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable as TdxPieceTable, AProperties);
end;

procedure TdxDocumentModel.CreateAbstractNumberingList(ATarget: TdxDocumentModel; ATargetList: TdxAbstractNumberingList;
  ANumberingListId: Integer; ASourceListIndex: TdxNumberingListIndex);
begin
  if ATargetList = nil then
    ATargetList := TdxAbstractNumberingList.Create(ATarget);
  ATarget.AddAbstractNumberingListUsingHistory(ATargetList);
  ATargetList.CopyFrom(NumberingLists[ASourceListIndex].AbstractNumberingList);
  ATargetList.SetId(ANumberingListId);
end;

function TdxDocumentModel.CreateCommandCreationStrategy: TdxRichEditCommandsCreationStrategy;
begin
  Result := TdxRichEditCommandsCreationStrategy.Create;
end;

function TdxDocumentModel.CreateCopySectionOperation(
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxSelectionBasedOperation;
begin
  Result := TdxCopySectionOperation.Create(TdxDocumentModelCopyManager(ACopyManager));
end;

function TdxDocumentModel.CreateDeferredChanges: TdxCustomDocumentModelDeferredChanges;
begin
  Result := TdxDocumentModelDeferredChanges.Create(Self);
end;

function TdxDocumentModel.CreateSectionCollection: TdxCustomSectionCollection;
begin
  Result := TdxSectionCollection.Create;
end;

function TdxDocumentModel.CreateDocumentCache: TdxCustomDocumentCache;
begin
  Result := TdxDocumentCache.Create;
end;

function TdxDocumentModel.CreateDocumentExportHelper(
  ADocumentFormat: TdxRichEditDocumentFormat): TObject;
begin
  Result := TdxDocumentExportHelper.Create(Self);
end;

function TdxDocumentModel.CreateEmptySpellCheckerManager(APieceTable: TdxCustomPieceTable): TObject;
begin
  Result := TdxEmptySpellCheckerManager.Create(TdxSimplePieceTable(APieceTable));
end;

function TdxDocumentModel.CreateDocumentExportManagerService: IInterface;
begin
  Result := TdxDocumentExportManagerService.Create;
end;

function TdxDocumentModel.CreateDocumentHistory: TdxDocumentHistory;
begin
  if DocumentCapabilities.UndoAllowed then
    Result := TdxRichEditDocumentHistory.Create(Self)
  else
    Result := TdxDisabledHistory.Create(Self);
end;

function TdxDocumentModel.CreateDocumentImportHelper: TObject;
begin
  Result := TdxDocumentImportHelper.Create(Self);
end;

function TdxDocumentModel.CreateDocumentImportManagerService: IInterface;
begin
  Result := TdxDocumentImportManagerService.Create;
end;

function TdxDocumentModel.CreateDocumentModelCopyCommand(ASourcePieceTable: TdxPieceTable;
  ATarget: TdxDocumentModel; AOptions: TdxDocumentModelCopyOptions): TObject;
begin
  Assert(ASourcePieceTable.DocumentModel = Self);
  Result := TdxDocumentModelCopyCommand.Create(ASourcePieceTable, ATarget, AOptions);
end;

procedure TdxDocumentModel.CreateDocumentObjects;
begin
  inherited CreateDocumentObjects;
  FSafeEditor := TdxSafeDocumentModelEditor.Create(Self);
  FHeaders := TdxHeaderCollection.Create;
  FFooters := TdxFooterCollection.Create;
  FLastInsertedFloatingObjectAnchorRunInfo := TdxLastInsertedFloatingObjectAnchorRunInfo.Create;
  FActivePieceTable := PieceTable;
  FActiveSectionIndex := -1;
  FLastInsertedFloatingObjectAnchorRunInfo.PieceTable := MainPieceTable;
  FNumberingLists := TdxNumberingListCollection.Create;
  FAbstractNumberingLists := TdxAbstractNumberingListCollection.Create;
  FAbstractNumberingListIdProvider := TdxAbstractNumberingListIdProvider.Create(Self);
  FNumberingListIdProvider := TdxNumberingListIdProvider.Create(Self);
  FTableBorderInfoRepository := TdxBorderInfoRepository.Create(UnitConverter);
  FFloatingObjectBorderInfoRepository := TdxBorderInfoRepository.Create(UnitConverter);
  FVariables := TdxDocumentVariableCollection.Create(Self);
  FFootNotes := TdxFootNoteCollection.Create;
  FEndNotes := TdxEndNoteCollection.Create;
  FMailMergeProperties := TdxMailMergeProperties.Create;
end;

procedure TdxDocumentModel.DestroyDocumentObjects;
begin
  FreeAndNil(FVariables);
  FreeAndNil(FMailMergeProperties);
  FreeAndNil(FEndNotes);
  FreeAndNil(FFootNotes);
  FreeAndNil(FHeaders);
  FreeAndNil(FFooters);

  FreeAndNil(FNumberingListStyles);
  FreeAndNil(FTableStyles);
  FreeAndNil(FTableCellStyles);

  FreeAndNil(FNumberingListIdProvider);
  FreeAndNil(FFloatingObjectBorderInfoRepository);
  FreeAndNil(FTableBorderInfoRepository);
  FreeAndNil(FExtendedDocumentProperties);
  FreeAndNil(FDocumentProperties);
  FreeAndNil(FProtectionProperties);
  FreeAndNil(FHtmlSettings);
  FreeAndNil(FLineNumberRun);

  FreeAndNil(FSpellCheckerOptions);
  FreeAndNil(FPrintingOptions);
  FreeAndNil(FFieldOptions);
  FreeAndNil(FCopyPasteOptions);
  FreeAndNil(FEditingOptions);
  FreeAndNil(FMailMergeOptions);
  FreeAndNil(FTableOptions);
  FreeAndNil(FLayoutOptions);
  FreeAndNil(FBookmarkOptions);
  FreeAndNil(FRangePermissionOptions);
  FreeAndNil(FDocumentImportOptions);
  FreeAndNil(FDocumentExportOptions);
  FreeAndNil(FDocumentSaveOptions);
  FreeAndNil(FBehaviorOptions);
  FreeAndNil(FSearchOptions);
  FreeAndNil(FAuthenticationOptions);
  FreeAndNil(FAutoCorrectOptions);

  FreeAndNil(FSafeEditor);
  FreeAndNil(FLastInsertedFloatingObjectAnchorRunInfo);
  FreeAndNil(FAbstractNumberingLists);
  FreeAndNil(FAbstractNumberingListIdProvider);
  FreeAndNil(FNumberingLists);
  inherited DestroyDocumentObjects;
end;

function TdxDocumentModel.CreateHyphenationService{(AContainer: IdxServiceContainer; AServiceType: TObject)}: IInterface;
begin
  Result := TdxHyphenationService.Create(CreateHyphenator);
end;

function TdxDocumentModel.CreateHyphenator: TdxHyphenator;
begin
  Result := TdxEmptyHyphenator.Create;
end;

function TdxDocumentModel.CreateInternalAPI: TdxCustomInternalAPI;
begin
  Result := TdxInternalAPI.Create(Self);
end;

function TdxDocumentModel.CreateMailMergeDataController: TdxRichEditDataControllerAdapterBase;
begin
  Result := TdxRichEditDataControllerAdapter.Create;
end;

function TdxDocumentModel.CreateMainContentType: TdxSimpleMainContentType;
begin
  Result := TdxMainContentType.Create(Self);
end;

function TdxDocumentModel.CreateNew: TdxDocumentModel;
begin
  Result := TdxDocumentModel.Create;
end;

function TdxDocumentModel.CreateNormalSelection: TdxSelection;
begin
  Result := TdxSelection.Create(ActivePieceTable);
end;

function TdxDocumentModel.CreateNumberingListCountersCalculator(
  AList: TdxAbstractNumberingList): TdxNumberingListCountersCalculator;
begin
  Result := TdxNumberingListCountersCalculator.Create(AList);
end;


function TdxDocumentModel.CreateAuthenticationOptions: TdxAuthenticationOptions;
begin
  Result := TdxAuthenticationOptions.Create;
end;

function TdxDocumentModel.CreateAutoCorrectOptions: TdxAutoCorrectOptions;
begin
  Result := TdxAutoCorrectOptions.Create;
end;

function TdxDocumentModel.CreateBehaviorOptions: TdxRichEditBehaviorOptions;
begin
  Result := TdxRichEditBehaviorOptions.Create;
end;

function TdxDocumentModel.CreateBookmarkOptions: TdxBookmarkOptions;
begin
  Result := TdxBookmarkOptions.Create;
end;

function TdxDocumentModel.CreateCopyPasteOptions: TdxCopyPasteOptions;
begin
  Result := TdxCopyPasteOptions.Create;
end;

function TdxDocumentModel.CreateDocumentCapabilitiesOptions: TdxCustomDocumentCapabilitiesOptions;
begin
  Result := TdxDocumentCapabilitiesOptions.Create;
end;

function TdxDocumentModel.CreateDocumentExportOptions: TdxRichEditDocumentExportOptions;
begin
  Result := TdxRichEditDocumentExportOptions.Create;
end;

function TdxDocumentModel.CreateDocumentImportOptions: TdxRichEditDocumentImportOptions;
begin
  Result := TdxRichEditDocumentImportOptions.Create;
end;

function TdxDocumentModel.CreateDocumentSaveOptions: TdxDocumentSaveOptions;
begin
  Result := TdxDocumentSaveOptions.Create;
end;

function TdxDocumentModel.CreateEditingOptions: TdxRichEditEditingOptions;
begin
  Result := TdxRichEditEditingOptions.Create;
end;

function TdxDocumentModel.CreateFieldOptions: TdxFieldOptions;
begin
  Result := TdxFieldOptions.Create;
end;

function TdxDocumentModel.CreateFormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions;
begin
  Result := TdxFormattingMarkVisibilityOptions.Create;
end;

function TdxDocumentModel.CreateLayoutOptions: TdxRichEditLayoutOptions;
begin
  Result := TdxRichEditLayoutOptions.Create;
end;

function TdxDocumentModel.CreateMailMergeOptions: TdxRichEditMailMergeOptions;
begin
  Result := TdxRichEditMailMergeOptions.Create;
end;

function TdxDocumentModel.CreatePrintingOptions: TdxPrintingOptions;
begin
  Result := TdxPrintingOptions.Create;
end;

function TdxDocumentModel.CreateRangePermissionOptions: TdxRangePermissionOptions;
begin
  Result := TdxRangePermissionOptions.Create;
end;

function TdxDocumentModel.CreateSearchOptions: TdxDocumentSearchOptions;
begin
  Result := TdxDocumentSearchOptions.Create;
end;

function TdxDocumentModel.CreateSpellCheckerOptions: TdxSpellCheckerOptions;
begin
  Result := TdxSpellCheckerOptions.Create;
end;

function TdxDocumentModel.CreateTableOptions: TdxTableOptions;
begin
  Result := TdxTableOptions.Create;
end;

procedure TdxDocumentModel.CreateOptions;
begin
  inherited CreateOptions;
  FEditingOptions := CreateEditingOptions;
  FCopyPasteOptions := CreateCopyPasteOptions;
  FFieldOptions := CreateFieldOptions;
  FMailMergeOptions := CreateMailMergeOptions;
  FDocumentSaveOptions := CreateDocumentSaveOptions;
  FLayoutOptions := CreateLayoutOptions;
  FDocumentImportOptions := CreateDocumentImportOptions;
  FDocumentExportOptions := CreateDocumentExportOptions;
  FBookmarkOptions := CreateBookmarkOptions;
  FRangePermissionOptions := CreateRangePermissionOptions;
  FSearchOptions := CreateSearchOptions;
  FAuthenticationOptions := CreateAuthenticationOptions;
  FTableOptions := CreateTableOptions;
  FAutoCorrectOptions := CreateAutoCorrectOptions;
  FBehaviorOptions := CreateBehaviorOptions;
  FPrintingOptions := CreatePrintingOptions;
  FSpellCheckerOptions := CreateSpellCheckerOptions;
end;

function TdxDocumentModel.CreatePieceTable(AContentType: TdxContentTypeBase): TdxCustomPieceTable{TdxPieceTable};
begin
  Result := TdxPieceTable.Create(Self, AContentType);
end;

procedure TdxDocumentModel.DeleteDefaultNumberingList(ANumberingLists: TdxNumberingListCollection);
begin
  if ANumberingLists.Count > 0 then
    FNumberingLists.Remove(ANumberingLists.First)
end;

procedure TdxDocumentModel.DisposeCore;
begin
  inherited DisposeCore;
  if FMailMergeDataController <> nil then
  begin
    UnubscribeMailMergeDataControllerEvents;
    FreeAndNil(FMailMergeDataController);
  end;
  UnsubscribeEventsForExport;
end;

procedure TdxDocumentModel.EndFieldsUpdate;
begin
  FSuppressFieldsChangeNotification := False;
  DeferredChanges.IsSetContentMode := True;
end;

procedure TdxDocumentModel.EndSetContent(AChangeType: TdxDocumentModelChangeType; AUpdateFields: Boolean;
  AUpdateOptions: TdxFieldUpdateOnLoadOptions);
begin
  EndSetContent(AChangeType, AUpdateFields, False, AUpdateOptions);
end;

procedure TdxDocumentModel.EndSetContent(AChangeType: TdxDocumentModelChangeType;
  AUpdateFields, APasteFromIe: Boolean;
  AUpdateOptions: TdxFieldUpdateOnLoadOptions; const AAfterPieceTablesEndSetContentAction: TdxAction = nil);
const
  UpdateFieldOperationTypeMap: array[Boolean] of TdxUpdateFieldOperationType = (TdxUpdateFieldOperationType.Load, TdxUpdateFieldOperationType.PasteFromIE);
var
  I: Integer;
  APieceTable: TdxPieceTable;
  APieceTables: TdxFastList;
begin
  RestoreInvalidNumberingListStyles;
  APieceTables := GetPieceTables(True);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := APieceTables[I];
      if DocumentCapabilities.Fields = TdxDocumentCapability.Disabled then
        APieceTable.ProcessFieldsRecursive(nil, function (AField: TdxField): Boolean
          begin
            Result := False;
          end);
      APieceTable.OnEndSetContent;
    end;
  finally
    APieceTables.Free;
  end;
  if Assigned(AAfterPieceTablesEndSetContentAction) then
    AAfterPieceTablesEndSetContentAction;

  TdxTableConditionalFormattingController.ResetTablesCachedProperties(Self);

  if AUpdateFields then
  begin
    BeginFieldsUpdate;
    try
      UpdateFields(UpdateFieldOperationTypeMap[APasteFromIe], AUpdateOptions);
    finally
      EndFieldsUpdate;
    end;
  end;

  SwitchToNormalHistory(True);
  SwitchToNormalSelection;
  MainPieceTable.ApplyChanges(AChangeType, 0, MainPieceTable.Runs.Count - 1);

  Selection.&End := 0;
  Selection.Start := 0;
  Selection.SetStartCell(0);

  DeferredChanges.IsSetContentMode := False;

  EndUpdate;
end;

procedure TdxDocumentModel.EndSetContentForExport(AChangeType: TdxDocumentModelChangeType; AUpdateFields: Boolean);
begin
  EndSetContent(AChangeType, AUpdateFields, nil);
end;

procedure TdxDocumentModel.EnforceDocumentProtection(const APassword: string);
begin
  BeginUpdate;
  try
    ProtectionProperties.EnforceProtection := True;
    ProtectionProperties.ProtectionType := TdxDocumentProtectionType.ReadOnly;
    if APassword = '' then
    begin
      ProtectionProperties.HashAlgorithmType := TdxHashAlgorithmType.None;
      ProtectionProperties.HashIterationCount := 0;
      ProtectionProperties.PasswordPrefix := nil;
      ProtectionProperties.PasswordHash := nil;
      ProtectionProperties.Word2003PasswordHash := nil;
      ProtectionProperties.OpenOfficePasswordHash := nil;
    end
    else
    begin
      ProtectionProperties.HashAlgorithmType := TdxHashAlgorithmType.Sha1;
      ProtectionProperties.HashIterationCount := 100000;
      ProtectionProperties.PasswordPrefix := dxGenerateSalt(16);
      ProtectionProperties.PasswordHash := TdxPasswordHashCodeCalculator.CalculateRichEditPasswordHash(APassword,
        ProtectionProperties.PasswordPrefix, ProtectionProperties.HashIterationCount, ProtectionProperties.HashAlgorithmType);
      ProtectionProperties.Word2003PasswordHash := TdxPasswordHashCodeCalculator.CalculateLegacyPasswordHash(APassword);
      ProtectionProperties.OpenOfficePasswordHash := TdxPasswordHashCodeCalculator.CalculateOpenOfficePasswordHash(APassword);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.EnsurePieceTableImagesLoadComplete(APieceTable: TdxPieceTable);
begin
  APieceTable.EnsureImagesLoadComplete;
end;

procedure TdxDocumentModel.EnsureImagesLoadComplete;
var
  I: Integer;
  APieceTables: TdxFastList;
begin
  APieceTables := GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
      EnsurePieceTableImagesLoadComplete(APieceTables[I]);
  finally
    APieceTables.Free;
  end;
end;

procedure TdxDocumentModel.ForceRemoveDocumentProtection;
begin
  BeginUpdate;
  try
    ProtectionProperties.EnforceProtection := False;
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.ForceSyntaxHighlight;
begin
  BeginUpdate;
  try
    DeferredChanges.ChangeActions := DeferredChanges.ChangeActions + [TdxDocumentModelChangeAction.ForceSyntaxHighlight];
    if not (TdxDocumentModelChangeAction.RaiseContentChanged in DeferredChanges.ChangeActions) then
      DeferredChanges.ChangeActions := DeferredChanges.ChangeActions + [TdxDocumentModelChangeAction.SuppressRaiseContentChangedCalculationByCurrentTransactionChanges];
  finally
    EndUpdate;
  end;
end;

function TdxDocumentModel.GetAbstractNumberingListIndex(ATarget: TdxDocumentModel;
  ASourceListIndex: TdxNumberingListIndex): TdxAbstractNumberingListIndex;
var
  ANumberingListId: Integer;
  I: TdxAbstractNumberingListIndex;
  ATargetList: TdxAbstractNumberingList;
begin
  ANumberingListId := NumberingLists[ASourceListIndex].AbstractNumberingList.Id;
  for I := 0 to ATarget.AbstractNumberingLists.Count - 1 do
  begin
    ATargetList := ATarget.AbstractNumberingLists[I];
    if ANumberingListId = ATargetList.Id then
    begin
      if ATargetList.Deleted then
        CreateAbstractNumberingList(ATarget, ATargetList, ANumberingListId, ASourceListIndex);
      Exit(I);
    end;
  end;
  CreateAbstractNumberingList(ATarget, nil, ANumberingListId, ASourceListIndex);
  Result := ATarget.AbstractNumberingLists.Count - 1;
end;

function TdxDocumentModel.GetActiveSection: TdxSection;
begin
  if FActiveSectionIndex < 0 then
    Result := nil
  else
    Result := Sections[FActiveSectionIndex];
end;

function TdxDocumentModel.GetActiveSectionBySelectionEnd: TdxSection;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  ASection: TdxSection;
  ASectionIndex: TdxSectionIndex;
begin
  AHeaderFooter := Safe<TdxSectionHeaderFooterBase>.Cast(Selection.PieceTable.ContentType);
  if AHeaderFooter <> nil then
  begin
    ASection := GetActiveSection;
    Exit(ASection);
  end;

  ASectionIndex := FindSectionIndex(Selection.&End);
  if ASectionIndex < 0 then
    Exit(nil);
  Result := Sections[ASectionIndex];
end;

function TdxDocumentModel.GetActiveSectionIndex: TdxSectionIndex;
begin
  Result := FActiveSectionIndex;
end;

function TdxDocumentModel.GetBookmarks(AIncludeHiddenBookmarks: Boolean = True): TdxBookmarkList;
var
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
  ABookmarks: TdxBookmarkList;
  I: Integer;
begin
  Result := TdxBookmarkList.Create;
  APieceTables := GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := APieceTables[I];
      ABookmarks := APieceTable.GetBookmarks(AIncludeHiddenBookmarks);
      try
        Result.AddRange(ABookmarks);
      finally
        ABookmarks.Free;
      end;
    end;
  finally
    APieceTables.Free;
  end;
end;

function TdxDocumentModel.GetBoxEffectiveRotationAngle(ABox: TdxBox): Integer;
var
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  if ABox is TdxFloatingObjectBox then
  begin
    ARun := TdxFloatingObjectBox(ABox).GetFloatingObjectRun;
    if ARun.Content is TdxTextBoxFloatingObjectContent then
    begin
      ATextBoxContent := TdxTextBoxFloatingObjectContent(ARun.Content);
      if ATextBoxContent.TextBox.PieceTable = ActivePieceTable then
        Exit(0);
    end;
    Result := ARun.Shape.Rotation;
  end
  else
    Result := 0;
end;

function TdxDocumentModel.GetBoxEffectiveRotationAngleInDegrees(ABox: TdxBox): Single;
begin
  Result := UnitConverter.ModelUnitsToDegreeF(GetBoxEffectiveRotationAngle(ABox));
end;

function TdxDocumentModel.GetEmptyBottomBorder: TdxBottomBorder;
begin
  Result := NotImplemented;
end;

function TdxDocumentModel.GetEmptyTopBorder: TdxTopBorder;
begin
  Result := NotImplemented;
end;

function TdxDocumentModel.GetEncryptionProperties: TdxDocumentEncryptionProperties;
begin
  Result := FEncryptionProperties;
end;


function TdxDocumentModel.GetExportManagerService: IInterface;
begin
  Result := GetService<IdxDocumentExportManagerService>;
end;

procedure TdxDocumentModel.SetDocumentCapabilities(const Value: TdxDocumentCapabilitiesOptions);
begin
  inherited DocumentCapabilities := Value;
end;

function TdxDocumentModel.GetFieldResultModel: TdxDocumentModel;
begin
  Result := CreateNew;
  Result.DisableCheckDocumentModelIntegrity := DisableCheckDocumentModelIntegrity;
  Result.ModelForExport := ModelForExport;
  Result.FieldOptions.Assign(FieldOptions);
  Result.FieldResultModel := True;
end;

function TdxDocumentModel.GetImportManagerService: IInterface;
begin
  Result := GetService<IdxDocumentImportManagerService>;
end;

function TdxDocumentModel.GetIsDocumentProtectionEnabled: Boolean;
begin
  Result := ProtectionProperties.EnforceProtection and
    (ProtectionProperties.ProtectionType = TdxDocumentProtectionType.ReadOnly);
end;

function TdxDocumentModel.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxDocumentModel.GetSections: TdxSectionCollection;
begin
  Result := TdxSectionCollection(inherited Sections);
end;

function TdxDocumentModel.GetSelection: TdxSelection;
begin
  Result := TdxSelection(inherited Selection);
end;

function TdxDocumentModel.GetIsEmpty: Boolean;
begin
  Result := MainPieceTable.IsEmpty and (Sections.Count <= 1) and not Sections.First.HasNonEmptyHeadersOrFooters;
end;

function TdxDocumentModel.GetLastInsertedFloatingObjectAnchorRunInfo(
  APieceTable: TdxPieceTable): TdxLastInsertedFloatingObjectAnchorRunInfo;
begin
  if APieceTable <> FLastInsertedFloatingObjectAnchorRunInfo.PieceTable then
    FLastInsertedFloatingObjectAnchorRunInfo.Reset(APieceTable);
  Result := FLastInsertedFloatingObjectAnchorRunInfo;
end;

function TdxDocumentModel.GetMailMergeDataMode: TdxMailMergeDataMode;
begin
  if MailMergeProperties.ViewMergedData then
    Result := TdxMailMergeDataMode.ViewMergedData
  else
    Result := TdxMailMergeDataMode.None;
end;

function TdxDocumentModel.GetDefaultTableProperties: TdxTableProperties;
begin
  Result := FDefaultTableProperties;
end;

function TdxDocumentModel.GetDefaultTableCellProperties: TdxTableCellProperties;
begin
  Result := FDefaultTableCellProperties;
end;

function TdxDocumentModel.GetDefaultTableRowProperties: TdxTableRowProperties;
begin
  Result := FDefaultTableRowProperties;
end;

procedure TdxDocumentModel.RestoreInvalidNumberingListStyles;
var
  I, ACount: Integer;
  AMaxListIndex: TdxNumberingListIndex;
  AStyle: TdxNumberingListStyle;
begin
  ACount := NumberingListStyles.Count;
  AMaxListIndex := NumberingLists.Count - 1;
  for I := 0 to ACount - 1 do
  begin
    AStyle := NumberingListStyles[I];
    EnsureNumberingListStyleValid(AStyle, AMaxListIndex);
  end;
end;

procedure TdxDocumentModel.EnsureNumberingListStyleValid(AStyle: TdxNumberingListStyle; AMaxListIndex: TdxNumberingListIndex);
var
  AIndices: TdxIntegerList;
  ANumberingList: TdxNumberingList;
  AAbstractListIndex: TdxAbstractNumberingListIndex;
  AAbstractList: TdxAbstractNumberingList;
begin
  if (AStyle.NumberingListIndex < NumberingListIndexMinValue) or (AStyle.NumberingListIndex > AMaxListIndex) then
    Exit;
  if not AStyle.NumberingList.AbstractNumberingList.UseStyleLevels then
    Exit;
  AIndices := TdxIntegerList.Create;
  try
    while AStyle <> nil do
    begin
      if (AStyle.NumberingListIndex < NumberingListIndexMinValue) or (AStyle.NumberingListIndex > AMaxListIndex) then
        Exit;
      if not AStyle.NumberingList.AbstractNumberingList.UseStyleLevels then
        Exit;
      ANumberingList := AStyle.NumberingList;
      AAbstractListIndex := ANumberingList.AbstractNumberingListIndex;
      AAbstractList := AbstractNumberingLists[AAbstractListIndex];
      if AIndices.Contains(AAbstractListIndex) then
      begin
        AAbstractList.SetStyleLinkIndex(AAbstractList.NumberingStyleReferenceIndex);
        Exit;
      end;
      if AIndices.IndexOf(AAbstractListIndex) = -1 then
        AIndices.Add(AAbstractListIndex);
      AStyle := AAbstractList.NumberingStyleReference;
    end;
  finally
    AIndices.Free;
  end;
end;

function TdxDocumentModel.GetCache: TdxDocumentCache;
begin
  Result := TdxDocumentCache(inherited Cache);
end;

function TdxDocumentModel.GetModified: Boolean;
begin
  Result := History.Modified;
end;

function TdxDocumentModel.GetNumberingListIndex(ATarget: TdxCustomDocumentModel;
  ASourceListIndex, AMaxNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex;
var
  I, AAbstractNumberingListId: Integer;
  ASourceNumberingList, ATargetNumberingList, ANumberingList: TdxNumberingList;
  ATargetAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
  ATargetModel: TdxDocumentModel absolute ATarget;
begin
  ATargetAbstractNumberingListIndex := GetAbstractNumberingListIndex(ATargetModel, ASourceListIndex);

  ASourceNumberingList := NumberingLists[ASourceListIndex];
  AAbstractNumberingListId := ASourceNumberingList.AbstractNumberingList.Id;
  AMaxNumberingListIndex := Min(AMaxNumberingListIndex, ATargetModel.NumberingLists.Count - 1);
  for I := 0 to AMaxNumberingListIndex do
  begin
    ATargetNumberingList := ATargetModel.NumberingLists[I];
    if (AAbstractNumberingListId = ATargetNumberingList.AbstractNumberingList.Id) and
      CheckIsOverrideEquals(ATargetNumberingList, ASourceNumberingList) then
      Exit(I);
  end;
  ANumberingList := TdxNumberingList.Create(ATargetModel, ATargetAbstractNumberingListIndex);
  ANumberingList.CopyFrom(NumberingLists[ASourceListIndex]);
  ATargetModel.AddNumberingListUsingHistory(ANumberingList);
  ANumberingList.SetId(ATargetModel.NumberingLists.Count);
  Result := ATargetModel.NumberingLists.Count - 1;
end;

function TdxDocumentModel.GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList;
var
  I: TdxSectionIndex;
begin
  Result := inherited GetPieceTables(AIncludeUnreferenced);
  for I := 0 to Sections.Count - 1 do
    Sections[I].AddPieceTables(Result, AIncludeUnreferenced);
  FootNotes.GetPieceTables(Result, AIncludeUnreferenced);
  EndNotes.GetPieceTables(Result, AIncludeUnreferenced);
end;

function TdxDocumentModel.GetSelectionText: string;
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  if Selection.Length = 0 then
    Exit('');
  AStart := Selection.Interval.NormalizedStart^;
  AEnd := Selection.Interval.NormalizedEnd^;
  Result := Selection.PieceTable.GetFilteredPlainText(AStart, AEnd, Selection.PieceTable.VisibleTextFilter.IsRunVisible);
end;

function TdxDocumentModel.GetSeparateModelForApiExport: Boolean;
begin
  Result := False;
end;

function TdxDocumentModel.GetMainPart: TdxCustomPieceTable;
begin
  Result := MainPieceTable;
end;

procedure TdxDocumentModel.RaiseInvalidFormatException(E: Exception);
var
  Args: TdxRichEditInvalidFormatExceptionEventArgs;
begin
  if not FOnInvalidFormatException.Empty then
  begin
    Args := TdxRichEditInvalidFormatExceptionEventArgs.Create(E);
    try
      FOnInvalidFormatException.Invoke(Self, Args);
    finally
      ARgs.Free;
    end;
  end;
end;

procedure TdxDocumentModel.RaiseSectionInserted(ASectionIndex: TdxSectionIndex);
var
  Args: TdxSectionEventArgs;
begin
  if FOnSectionInserted.Empty then
    Exit;
  Args := TdxSectionEventArgs.Create(ASectionIndex);
  try
    FOnSectionInserted.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxDocumentModel.RaiseSectionRemoved(ASectionIndex: TdxSectionIndex);
var
  Args: TdxSectionEventArgs;
begin
  if FOnSectionRemoved.Empty then
    Exit;
  Args := TdxSectionEventArgs.Create(ASectionIndex);
  try
    FOnSectionRemoved.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

function TdxDocumentModel.GetDatabaseFieldNames: TArray<TdxMergeFieldName>;
begin
  if MailMergeDataController.IsReady then
  begin
    Result := MailMergeDataController.GetColumnNames;
  end
  else
    Result := TArray<TdxMergeFieldName>.Create();
end;

function TdxDocumentModel.GetSyntaxHighlightSuspended: Boolean;
begin
  Result := FSyntaxHighlightSuspendCount > 0;
end;

function TdxDocumentModel.GetTableCellStyles: TdxTableCellStyleCollection;
begin
  Result := FTableCellStyles;
end;

function TdxDocumentModel.GetTableStyles: TdxTableStyleCollection;
begin
  Result := FTableStyles;
end;

procedure TdxDocumentModel.InheritDataServices(ADocumentModel: TdxDocumentModel);
begin
end;

procedure TdxDocumentModel.InheritServicesForExport(ADocumentModel: TdxDocumentModel);
begin
  InheritDataServices(ADocumentModel);
  InheritUriProviderService(ADocumentModel);
end;

procedure TdxDocumentModel.InheritUriProviderService(ADocumentModel: TdxDocumentModel);
begin
end;

procedure TdxDocumentModel.Initialize;
begin
  inherited Initialize;

  ClearDocument;

  SubscribeDocumentObjectsEvents;
  SubscribeOptionsEvents;

  SwitchToNormalHistory(True);
  SwitchToNormalSelection;
  FInternalAPI := CreateInternalAPI;
  FCommandsCreationStrategy := CreateCommandCreationStrategy;
  FSearchParameters := TdxSearchParameters.Create;
  FSearchContext := TdxSearchContext.Create(MainPieceTable);
end;

procedure TdxDocumentModel.InitializeDefaultProperties;
begin
  inherited InitializeDefaultProperties;
  FDefaultTableProperties.SetIndexInitial(TdxTablePropertiesOptionsCache.RootTableFormattingOptionsItem);
  FDefaultTableRowProperties.SetIndexInitial(TdxTableRowPropertiesOptionsCache.RootRowPropertiesOptionsItem);
  FDefaultTableCellProperties.SetIndexInitial(TdxTableCellPropertiesOptionsCache.RootCellPropertiesOptionsItem);
end;

procedure TdxDocumentModel.InitializeDefaultStyles;
begin
end;

procedure TdxDocumentModel.InsertSection(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False);
var
  ACommand: TdxDocumentModelInsertSectionAtLogPositionCommand;
begin
  ACommand := TdxDocumentModelInsertSectionAtLogPositionCommand.Create(Self, ALogPosition, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocumentModel.InvalidateDocumentLayout;
begin
  Assert(IsUpdateLocked);
  ActivePieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    0, MaxInt);
end;

procedure TdxDocumentModel.InvalidateDocumentLayoutFrom(ARunIndex: TdxRunIndex);
begin
  Assert(IsUpdateLocked);
  ActivePieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    ARunIndex, MaxInt);
end;

function TdxDocumentModel.IsCursorInParagraph(AParagraph: TdxParagraph): Boolean;
var
  APieceTable: TdxPieceTable;
  ASelection: TdxSelection;
begin
  APieceTable := AParagraph.PieceTable;
  ASelection := APieceTable.DocumentModel.Selection;
  Result := (APieceTable = ASelection.PieceTable) and (ASelection.Items.Count = 1) and (ASelection.Length = 0) and
    (ASelection.Start >= AParagraph.LogPosition) and (ASelection.Start <= AParagraph.EndLogPosition);
end;

function TdxDocumentModel.IsSpecialEmptyParagraphAfterInnerTable(AParagraph: TdxParagraph;
  ACell: TdxTableCell): Boolean;
var
  AParagraphIndex: TdxParagraphIndex;
  APieceTable: TdxPieceTable;
  APrevParagraphCell: TdxTableCell;
begin
  if not AParagraph.BoxCollection.IsValid then
    Exit(False);
  if (AParagraph.BoxCollection.Count > 1) or (ACell = nil) then
    Exit(False);
  AParagraphIndex := AParagraph.Index;
  if AParagraphIndex = 0 then
    Exit(False);
  if ACell.EndParagraphIndex <> AParagraphIndex then
    Exit(False);
  APieceTable := AParagraph.PieceTable;
  APrevParagraphCell := APieceTable.Paragraphs[AParagraphIndex - 1].GetCell;
  if APrevParagraphCell = nil then
    Exit(False);
  Exit(APrevParagraphCell.Table.ParentCell = ACell);
end;

function TdxDocumentModel.IsTextSelectedOnly: Boolean;
var
  I, AStartIndex, AEndIndex: TdxRunIndex;
  APieceTable: TdxPieceTable;
begin
  AStartIndex := Selection.Interval.NormalizedStart.RunIndex;
  AEndIndex := Selection.Interval.NormalizedEnd.RunIndex;
  APieceTable := Selection.PieceTable;
  for I := AStartIndex to AEndIndex do
  begin
    if not APieceTable.VisibleTextFilter.IsRunVisible(I) then
      Continue;
    if not (APieceTable.Runs[I] is TdxTextRun) then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxDocumentModel.LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
  AEncoding: TEncoding);
var
  AImportHelper: TdxImportHelper<TdxRichEditDocumentFormat, Boolean>;
  AImportManagerService: IdxImportManagerService<TdxRichEditDocumentFormat, Boolean>;
  AService: IInterface;
  APrevModified: Boolean;
begin
  AImportHelper := TdxImportHelper<TdxRichEditDocumentFormat, Boolean>(CreateDocumentImportHelper);
  try
    AService := GetImportManagerService;
    if not Supports(AService, IdxImportManagerService<TdxRichEditDocumentFormat, Boolean>, AImportManagerService) then
      AImportHelper.ThrowUnsupportedFormatException;
    APrevModified := Modified;
    AImportHelper.Import(AStream, ADocumentFormat, ASourceUri, AImportManagerService, AEncoding);
    if APrevModified <> Modified then
      RaiseModifiedChanged;
    Assert(not Modified);
  finally
    AImportHelper.Free;
  end;
end;

procedure TdxDocumentModel.NormalizeZOrder;
var
  I: Integer;
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
  AFloatingObjects: TdxIZOrderedObjectList;
  AZOrderManager: TdxZOrderManager;
  AList: TdxIZOrderedObjectList;
begin
  AFloatingObjects := TdxIZOrderedObjectList.Create;
  try
    APieceTables := GetPieceTables(False);
    try
      for I := 0 to APieceTables.Count - 1 do
      begin
        APieceTable := APieceTables[I];
        AList := APieceTable.GetFloatingObjectList;
        try
          AFloatingObjects.AddRange(AList);
        finally
          AList.Free;
        end;
      end;
    finally
      APieceTables.Free;
    end;
    AZOrderManager := TdxZOrderManager.Create;
    try
      AZOrderManager.Normalize(AFloatingObjects);
    finally
      AZOrderManager.Free;
    end;
  finally
    AFloatingObjects.Free;
  end;
end;

procedure TdxDocumentModel.NotifyContentChanged;
begin
end;

procedure TdxDocumentModel.OnLayoutOptionsChanged(ASender: TObject;
  E: TdxRichEditNotificationOptionsChangedArgs);
begin
  if TdxViewLayoutOptionsBase.TAction.AllowTablesToExtendIntoMargins in E.Actions then
    OnLayoutTablesToExtendIntoMarginsOptionsChanged;
  if TdxViewLayoutOptionsBase.TAction.MatchHorizontalTableIndentsToTextEdge in E.Actions then
    OnMatchHorizontalTableIndentsToTextEdgeOptionsChanged;
end;

procedure TdxDocumentModel.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  Assert(IsUpdateLockedOrOverlapped);
  if not DeferredChanges.IsSetContentMode then
    TdxDocumentModelStructureChangedNotifier.NotifyBeginMultipleRunSplit(Selection, APieceTable);
end;

procedure TdxDocumentModel.OnCancelUpdate;
begin
  Selection.CancelUpdate;
  inherited OnCancelUpdate;
end;

procedure TdxDocumentModel.OnDocumentPropertiesObtainAffectedRange(ASender: TObject;
  E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxDocumentModel.OnDocumentPropertiesPageBackgroundChanged(ASender: TObject; E: TdxEventArgs);
begin
  RaisePageBackgroundChanged;
end;

procedure TdxDocumentModel.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  Assert(IsUpdateLockedOrOverlapped);
  if not DeferredChanges.IsSetContentMode then
    TdxDocumentModelStructureChangedNotifier.NotifyEndMultipleRunSplit(Selection, APieceTable);
end;

procedure TdxDocumentModel.OnEndUpdate;
begin
  inherited OnEndUpdate;
  Selection.EndUpdate;
end;

procedure TdxDocumentModel.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode and not SuppressFieldsChangeNotification then
    TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(TdxInternalAPI(InternalAPI), APieceTable, AFieldIndex);
end;

procedure TdxDocumentModel.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if not DeferredChanges.IsSetContentMode and not SuppressFieldsChangeNotification then
    TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(TdxInternalAPI(InternalAPI), APieceTable, AFieldIndex);
end;

procedure TdxDocumentModel.OnFirstBeginUpdate;
begin
  RaiseBeginDocumentUpdate;
  inherited OnFirstBeginUpdate;
end;

procedure TdxDocumentModel.OnBeginUpdate;
begin
  inherited OnBeginUpdate;
  Selection.BeginUpdate;
end;

procedure TdxDocumentModel.OnGridLinesOptionsChanged;
begin
  OnLayoutTablesToExtendIntoMarginsOptionsChanged;
end;

procedure TdxDocumentModel.OnHistoryModifiedChanged(ASender: TObject; E: TdxEventArgs);
begin
  if IsUpdateLockedOrOverlapped then
    ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.RaiseModifiedChanged], dxRunIndexDontCare, dxRunIndexDontCare)
  else
    RaiseModifiedChanged;
end;

procedure TdxDocumentModel.OnHistoryOperationCompleted(ASender: TObject; E: TdxEventArgs);
begin
  RaiseInnerContentChanged;
  if IsUpdateLockedOrOverlapped then
    ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.RaiseContentChanged], dxRunIndexDontCare, dxRunIndexDontCare)
  else
    RaiseContentChanged;
end;

procedure TdxDocumentModel.OnLastEndUpdateCore;
var
  APieceTable: TdxCustomPieceTable;
  ADeferredChangesRunIndicesForSplit: TDictionary<TdxCustomPieceTable, TdxSortedRunIndexCollection>;
begin
  if TdxDocumentModelChangeAction.RaiseSelectionChanged in DeferredChanges.ChangeActions then
  begin
    if ShouldFormatSpecialEmptyParagraph then
    begin
      FSpecialEmptyParagraphRunIndex := Selection.Interval.Start.RunIndex;
      ActivePieceTable.ApplyChangesCore([
        TdxDocumentModelChangeAction.ResetPrimaryLayout,
        TdxDocumentModelChangeAction.ResetSecondaryLayout,
        TdxDocumentModelChangeAction.Redraw], FSpecialEmptyParagraphRunIndex, FSpecialEmptyParagraphRunIndex);
      FIsLastSelectionInEmptySpecialParagraph := True;
    end
    else
      if FIsLastSelectionInEmptySpecialParagraph then
      begin
        ActivePieceTable.ApplyChangesCore([
          TdxDocumentModelChangeAction.ResetPrimaryLayout,
          TdxDocumentModelChangeAction.ResetSecondaryLayout,
          TdxDocumentModelChangeAction.Redraw], FSpecialEmptyParagraphRunIndex, FSpecialEmptyParagraphRunIndex);
        FIsLastSelectionInEmptySpecialParagraph := False;
      end;
  end;

  ADeferredChangesRunIndicesForSplit := DeferredChanges.RunIndicesForSplit;
  for APieceTable in ADeferredChangesRunIndicesForSplit.Keys do
    TdxPieceTable(APieceTable).PerformTextRunSplit(ADeferredChangesRunIndicesForSplit[APieceTable]);

  if History.HasChangesInCurrentTransaction and
     not (TdxDocumentModelChangeAction.SuppressRaiseContentChangedCalculationByCurrentTransactionChanges in DeferredChanges.ChangeActions) then
    DeferredChanges.ChangeActions := DeferredChanges.ChangeActions + [TdxDocumentModelChangeAction.RaiseContentChanged];
  RaiseBeforeEndDocumentUpdate;

        History.EndTransaction;
  if TdxDocumentModelChangeAction.ApplyAutoCorrect in DeferredChanges.ChangeActions then
    PerformAutoCorrect;
  ResetParagraphsOnLastEndUpdate;

  NotifyContentChanged;
  RaiseEndDocumentUpdate;
  RaiseAfterEndDocumentUpdate;

  if not DeferredChanges.SuppressClearOutdatedSelectionItems then
    Selection.ClearOutdatedItems;
  DeleteDeferredChanges;
end;

procedure TdxDocumentModel.OnLayoutTablesToExtendIntoMarginsOptionsChanged;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler];
begin
  BeginUpdate;
  try
    MainPieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.OnMatchHorizontalTableIndentsToTextEdgeOptionsChanged;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler];
begin
  BeginUpdate;
  try
    MainPieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.OnLayoutUnitChanged;
var
  I: Integer;
  APieceTable: TdxPieceTable;
  APieceTables: TdxFastList;
begin
  inherited OnLayoutUnitChanged;
  FLineNumberRun.ResetFontCacheIndex;
  APieceTables := GetPieceTables(True);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := APieceTables[I];
      APieceTable.ClearFontCacheIndices;
    end;
  finally
    APieceTables.Free;
  end;
  ResetParagraphs;
end;

procedure TdxDocumentModel.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(DeferredChanges, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
    if not SuppressFieldsChangeNotification then
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(TdxInternalAPI(InternalAPI), APieceTable, ASectionIndex,
        AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(Selection, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(SearchContext, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  end;
  if APieceTable.IsMain then
  begin
    Assert(ASectionIndex >= 0);
    RecalcSectionIndices(ASectionIndex, 1);
  end;
end;

procedure TdxDocumentModel.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(DeferredChanges, APieceTable,
      ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    if not SuppressFieldsChangeNotification then
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(TdxInternalAPI(InternalAPI), APieceTable, ASectionIndex,
        AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(Selection, APieceTable,
      ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(SearchContext, APieceTable,
      ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  end;
  if APieceTable.IsMain then
  begin
    Assert(ASectionIndex >= 0);
    RecalcSectionIndices(ASectionIndex, -1);
  end;
end;

procedure TdxDocumentModel.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(DeferredChanges, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, AHistoryNotificationId);
    if not SuppressFieldsChangeNotification then
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(TdxInternalAPI(InternalAPI), APieceTable, ASectionIndex,
        AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(Selection, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(SearchContext, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, AHistoryNotificationId);
  end;
  if APieceTable.IsMain then
  begin
    Assert(ASectionIndex >= 0);
    RecalcSectionIndices(ASectionIndex, -1);
  end;
end;

procedure TdxDocumentModel.OnProtectionPropertiesObtainAffectedRange(ASender: TObject;E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxDocumentModel.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  Assert(ALength > 0);
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(DeferredChanges, APieceTable, AParagraphIndex,
      ANewRunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      ANewRunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(Selection, APieceTable, AParagraphIndex,
      ANewRunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(SearchContext, APieceTable, AParagraphIndex,
      ANewRunIndex, ALength, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModel.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  Assert(IsUpdateLockedOrOverlapped);
  Assert(ASplitOffset > 0);
  Assert(ATailRunLength > 0);
  if not DeferredChanges.IsSetContentMode then
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(DeferredChanges, APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(DeferredChanges, APieceTable, AParagraphIndex,
      AJoinedRunIndex, ASplitOffset, ATailRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      AJoinedRunIndex, ASplitOffset, ATailRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(Selection, APieceTable, AParagraphIndex,
      AJoinedRunIndex, ASplitOffset, ATailRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(SearchContext, APieceTable, AParagraphIndex,
      AJoinedRunIndex, ASplitOffset, ATailRunLength);
  end;
end;

procedure TdxDocumentModel.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(DeferredChanges, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(Selection, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(SearchContext, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
  end;
end;

procedure TdxDocumentModel.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  Assert(IsUpdateLockedOrOverlapped);
  Assert(ALength > 0);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(DeferredChanges, APieceTable, AParagraphIndex,
      ARunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      ARunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(Selection, APieceTable, AParagraphIndex,
      ARunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(SearchContext, APieceTable, AParagraphIndex,
      ARunIndex, ALength, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModel.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  Assert(IsUpdateLockedOrOverlapped);
  Assert(ASplitOffset > 0);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(DeferredChanges, APieceTable, AParagraphIndex,
      ARunIndex, ASplitOffset);
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      ARunIndex, ASplitOffset);
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(Selection, APieceTable, AParagraphIndex,
      ARunIndex, ASplitOffset);
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(SearchContext, APieceTable, AParagraphIndex,
      ARunIndex, ASplitOffset);
  end;
end;

procedure TdxDocumentModel.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  Assert(IsUpdateLocked);
  if not DeferredChanges.IsSetContentMode then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(DeferredChanges, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(TdxInternalAPI(InternalAPI), APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(Selection, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(SearchContext, APieceTable, AParagraphIndex,
      ARunIndex, ADeltaRunLength);
  end;
end;

procedure TdxDocumentModel.OnSectionInserted(ASectionIndex: TdxSectionIndex);
begin
  Assert(IsUpdateLocked);
  RaiseSectionInserted(ASectionIndex);
end;

procedure TdxDocumentModel.OnSectionRemoved(ASectionIndex: TdxSectionIndex);
begin
  Assert(IsUpdateLocked);
  RaiseSectionRemoved(ASectionIndex);
end;

procedure TdxDocumentModel.OnTableOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  if TdxTableOptions.TAction.GridLines in E.Actions then
    OnShowHiddenTextOptionsChanged;
end;

procedure TdxDocumentModel.PerformAutoCorrect;
var
  AService: IdxAutoCorrectService;
  AInfo: TdxAutoCorrectInfo;
begin
  AService := GetService<IdxAutoCorrectService>;
  if AService = nil then
    Exit;

  AInfo := AService.CalculateAutoCorrectInfo;
  if not AInfo.IsNull then
    AService.ApplyAutoCorrectInfo(AInfo);
end;

procedure TdxDocumentModel.PerformSyntaxHighlight(AForced: Boolean);
begin
  NotImplemented;
end;

procedure TdxDocumentModel.PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat);
var
  APieceTable: TdxPieceTable;
  APieceTables: TdxFastList;
  I, ACount: Integer;
begin
  APieceTables := GetPieceTables(False);
  try
    ACount := APieceTables.Count;
    for I := 0 to ACount - 1 do
    begin
      APieceTable := APieceTables[I];
      if DocumentCapabilities.Fields = TdxDocumentCapability.Disabled then
        APieceTable.ProcessFieldsRecursive(nil, function (AField: TdxField): Boolean
          begin
            Result := False;
          end);
      APieceTable.PreprocessContentBeforeExport(AFormat);
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxDocumentModel.PreprocessContentBeforeInsert(ADestination: TdxPieceTable;
  APos: TdxDocumentLogPosition);
begin
  InheritDataServices(ADestination.DocumentModel);
end;

procedure TdxDocumentModel.RaiseAfterExportFromExportModel(ASender: TObject);
begin
  RaiseAfterExport;
end;

procedure TdxDocumentModel.RaiseBeforeExportFromExportModel(ASender: TObject; E: TdxBeforeExportEventArgs);
begin
  RaiseBeforeExport(E.Format, E.Options);
end;

procedure TdxDocumentModel.RaiseBeforeImport(AFormat: TdxRichEditDocumentFormat;
  const AOptions: IdxImporterOptions);
var
  Args: TdxBeforeImportEventArgs;
begin
  if FOnBeforeImport.Empty then
    Exit;
  Args := TdxBeforeImportEventArgs.Create(AFormat, AOptions);
  try
    FOnBeforeImport.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

function TdxDocumentModel.RaiseCalculateDocumentVariable(
  Args: TdxCalculateDocumentVariableEventArgs): Boolean;
begin
  if not FOnCalculateDocumentVariable.Empty then
  begin
    FOnCalculateDocumentVariable.Invoke(Self, Args);
    Result := Args.Handled;
  end
  else
    Result := False;
end;

procedure TdxDocumentModel.RaiseCalculateDocumentVariableForExport(ASender: TObject;
  E: TdxCalculateDocumentVariableEventArgs);
begin
  RaiseCalculateDocumentVariable(E);
end;

procedure TdxDocumentModel.RaiseBeforeExport(AFormat: TdxRichEditDocumentFormat; const AOptions: IdxExporterOptions);
var
  Args: TdxBeforeExportEventArgs;
begin
  if FOnBeforeExport.Empty then
    Exit;
  Args := TdxBeforeExportEventArgs.Create(AFormat, AOptions);
  try
    FOnBeforeExport.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxDocumentModel.RecalcSectionIndices(AFrom: TdxSectionIndex; ADeltaIndex: Integer);
begin
  Sections[AFrom].LastParagraphIndex := Sections[AFrom].LastParagraphIndex + ADeltaIndex;
  RecalcSectionIndicesCore(AFrom + 1, ADeltaIndex);
end;

procedure TdxDocumentModel.RecalcSectionIndicesCore(AFrom: TdxSectionIndex; ADeltaIndex: Integer);
var
  I: Integer;
  ASection: TdxSection;
begin
  for I := AFrom to Sections.Count - 1 do
  begin
    ASection := Sections[I];
    ASection.FirstParagraphIndex := ASection.FirstParagraphIndex + ADeltaIndex;
    ASection.LastParagraphIndex := ASection.LastParagraphIndex + ADeltaIndex;
  end;
end;

function TdxDocumentModel.CreateSection: TdxCustomSection;
begin
  Result := TdxSection.Create(Self);
end;

procedure TdxDocumentModel.NotifyNumberingListParagraphAdded(AIndex: TdxNumberingListIndex);
begin
  TdxNumberingListNotifier.NotifyParagraphAdded(Self, AIndex);
end;

procedure TdxDocumentModel.NotifyNumberingListParagraphRemoved(AIndex: TdxNumberingListIndex);
begin
  TdxNumberingListNotifier.NotifyParagraphRemoved(Self, AIndex);
end;

procedure TdxDocumentModel.RecreateLineNumberRun;
var
  AStyleIndex: Integer;
begin
  FreeAndNil(FLineNumberRun);
  FLineNumberRun := TdxLineNumberCommonRun.Create(MainPieceTable.Paragraphs.First);
  AStyleIndex := CharacterStyles.GetStyleIndexByName(TdxCharacterStyleCollection.LineNumberingStyleName);
  if AStyleIndex < 0 then
    AStyleIndex := TdxCharacterStyleCollection.EmptyCharacterStyleIndex;
  FLineNumberRun.SetCharacterStyleIndexCore(AStyleIndex);
end;

function TdxDocumentModel.QueryEncryptionPassword(var APassword: string): Boolean;
begin
  APassword := EncryptionProperties.Password;
  if Assigned(FOnEncryptionPasswordQuery) then
    Result := FOnEncryptionPasswordQuery(Self, APassword)
  else
    Result := True;
end;

function TdxDocumentModel.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
var
  APredicate: TdxSectionParagraphIndexComparable;
begin
  APredicate := TdxSectionParagraphIndexComparable.Create(AParagraphIndex);
  try
    TdxAlgorithms1<TdxCustomSection>.BinarySearch(Sections, APredicate, Result);
  finally
    APredicate.Free;
  end;
end;

function TdxDocumentModel.RaiseMailMergeStarted(const Args: TdxMailMergeStartedEventArgs): Boolean;
begin
  if not FOnMailMergeStarted.Empty then
  begin
    FOnMailMergeStarted.Invoke(Self, Args);
    Result := not Args.Cancel;
  end
  else
    Result := True;
end;

function TdxDocumentModel.RaiseMailMergeRecordStarted(const Args: TdxMailMergeRecordStartedEventArgs): Boolean;
begin
  if not FOnMailMergeRecordStarted.Empty then
  begin
    FOnMailMergeRecordStarted.Invoke(Self, Args);
    Result := not Args.Cancel;
  end
  else
    Result := True;
end;

function TdxDocumentModel.RaiseMailMergeRecordFinished(const Args: TdxMailMergeRecordFinishedEventArgs): Boolean;
begin
  if not FOnMailMergeRecordFinished.Empty then
  begin
    FOnMailMergeRecordFinished.Invoke(Self, Args);
    Result := not Args.Cancel;
  end
  else
    Result := True;
end;

procedure TdxDocumentModel.RaiseMailMergeFinished(const Args: TdxMailMergeFinishedEventArgs);
begin
  if not FOnMailMergeFinished.Empty then
    FOnMailMergeFinished.Invoke(Self, Args);
end;

procedure TdxDocumentModel.RaiseMailMergeGetTargetDocument(const Args: TdxMailMergeGetTargetDocumentEventArgs);
begin
  if not FOnMailMergeGetTargetDocument.Empty then
    FOnMailMergeGetTargetDocument.Invoke(Self, Args);
end;

procedure TdxDocumentModel.Reinitialize;
begin
  ClearCore;
  Initialize;
end;

function TdxDocumentModel.RemoveDocumentProtection(const APassword: string): Boolean;
begin
  if not CheckDocumentProtectionPassword(APassword) then
    Exit(False);
  ForceRemoveDocumentProtection;
  Result := True;
end;

procedure TdxDocumentModel.ResetParagraphs;
var
  I: Integer;
  APieceTable: TdxPieceTable;
  AList: TdxFastList;
begin
  AList := GetPieceTables(True);
  try
    for I := 0 to AList.Count - 1 do
    begin
      APieceTable := AList[I];
      APieceTable.ResetParagraphs;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxDocumentModel.ResetAdditionalPieceTablesParagraphs(APieceTable: TdxPieceTableList);
var
  I: Integer;
begin
  for I := 0 to APieceTable.Count - 1 do
    APieceTable[I].ResetParagraphs(0, MaxInt);
end;

procedure TdxDocumentModel.ResetParagraphsOnLastEndUpdate;
var
  AAdditionalChangedPieceTables: TdxPieceTableList;
begin
  if TdxDocumentModelChangeAction.ResetAllPrimaryLayout in DeferredChanges.ChangeActions then
    ResetParagraphs
  else
  if (TdxDocumentModelChangeAction.ResetPrimaryLayout in DeferredChanges.ChangeActions) or
     (TdxDocumentModelChangeAction.ResetSecondaryLayout in DeferredChanges.ChangeActions) then
  begin
    MainPieceTable.ResetParagraphs(DeferredChanges.ChangeStart.ParagraphIndex, DeferredChanges.ChangeEnd.ParagraphIndex);
    AAdditionalChangedPieceTables := DeferredChanges.AdditionalChangedPieceTables;
    if AAdditionalChangedPieceTables <> nil then
      ResetAdditionalPieceTablesParagraphs(AAdditionalChangedPieceTables);
  end;
end;

procedure TdxDocumentModel.ResetTemporaryLayout;
var
  AService: IdxDocumentLayoutService;
begin
  AService := GetService<IdxDocumentLayoutService>;
  if AService <> nil then
    AService.ResetLayout;
end;

procedure TdxDocumentModel.ResumeSyntaxHighlight;
begin
  if FSyntaxHighlightSuspendCount > 0 then
    Dec(FSyntaxHighlightSuspendCount);
end;

procedure TdxDocumentModel.SetActivePieceTable(APieceTable: TdxCustomPieceTable);
begin
  SetActivePieceTable(TdxPieceTable(APieceTable), nil);
end;

procedure TdxDocumentModel.SetActivePieceTable(APieceTable: TdxPieceTable; ASection: TdxSection);
const
  ChangeActions = [
    TdxDocumentModelChangeAction.ActivePieceTableChanged,
    TdxDocumentModelChangeAction.RaiseSelectionChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler,
    TdxDocumentModelChangeAction.ValidateSelectionInterval,
    TdxDocumentModelChangeAction.ResetSecondaryLayout];
begin
  BeginUpdate;
  try
    SetActivePieceTableCore(APieceTable, ASection);
    MainPieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.SetActivePieceTableCore(APieceTable: TdxPieceTable; ASection: TdxSection);
var
  AContentType: TdxContentTypeBase;
begin
  if APieceTable.IsMain and (ASection <> nil) then
    TdxRichEditExceptions.ThrowArgumentException('section', ASection);
  if (APieceTable <> MainPieceTable) and APieceTable.IsHeaderFooter then
  begin
    Assert(ASection <> nil);
    AContentType := APieceTable.ContentType;
    if (ASection.InnerFirstPageHeader <> AContentType) and
      (ASection.InnerOddPageHeader <> AContentType) and
      (ASection.InnerEvenPageHeader <> AContentType) and
      (ASection.InnerFirstPageFooter <> AContentType) and
      (ASection.InnerOddPageFooter <> AContentType) and
      (ASection.InnerEvenPageFooter <> AContentType) then
      TdxRichEditExceptions.ThrowArgumentException('pieceTable', APieceTable);
  end;
  Assert(APieceTable <> nil);
  FActivePieceTable := APieceTable;
  if ASection = nil then
    FActiveSectionIndex := -1
  else
    FActiveSectionIndex := Sections.IndexOf(ASection);
  SwitchToNormalSelection;
end;

procedure TdxDocumentModel.SetModified(const Value: Boolean);
begin
  History.Modified := Value;
end;

function TdxDocumentModel.GetDeferredChanges: TdxDocumentModelDeferredChanges;
begin
  Result := TdxDocumentModelDeferredChanges(inherited DeferredChanges);
end;

function TdxDocumentModel.GetDocumentCapabilities: TdxDocumentCapabilitiesOptions;
begin
  Result := TdxDocumentCapabilitiesOptions(inherited DocumentCapabilities);
end;

function TdxDocumentModel.GetFormattingMarkVisibilityOptions: TdxFormattingMarkVisibilityOptions;
begin
  Result := TdxFormattingMarkVisibilityOptions(inherited FormattingMarkVisibilityOptions);
end;

function TdxDocumentModel.ShouldFormatSpecialEmptyParagraph: Boolean;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  if (Selection.Items.Count = 1) and (Selection.Length = 0) then
  begin
    AParagraphIndex := Selection.Interval.Start.ParagraphIndex;
    if (AParagraphIndex >= 0) and (AParagraphIndex < Selection.PieceTable.Paragraphs.Count) then
    begin
      AParagraph := Selection.PieceTable.Paragraphs[AParagraphIndex];
      Exit(IsSpecialEmptyParagraphAfterInnerTable(AParagraph, AParagraph.GetCell));
    end;
  end;
  Result := False;
end;

procedure TdxDocumentModel.SubscribeDocumentObjectsEvents;
begin
  inherited SubscribeDocumentObjectsEvents;
  SubscribeMailMergePropertiesEvents;
end;

function TdxDocumentModel.UseFontSubstitution: Boolean;
begin
  Result := BehaviorOptions.UseFontSubstitution;
end;

procedure TdxDocumentModel.SubscribeDocumentPropertiesEvents;
begin
  if FDocumentProperties <> nil then
  begin
    FDocumentProperties.OnObtainAffectedRange.Add(OnDocumentPropertiesObtainAffectedRange);
    FDocumentProperties.OnPageBackgroundChanged := OnDocumentPropertiesPageBackgroundChanged;
  end;
end;

procedure TdxDocumentModel.SubscribeEventsForExport(AModelForExport: TdxDocumentModel);
begin
  AModelForExport.CalculateDocumentVariable.Add(RaiseCalculateDocumentVariableForExport);
  AModelForExport.BeforeExport.Add(RaiseBeforeExportFromExportModel);
  AModelForExport.AfterExport.Add(RaiseAfterExportFromExportModel);
end;

procedure TdxDocumentModel.SubscribeMailMergeDataControllerEvents;
begin
  FMailMergeDataController.CurrentRowChanged.Add(OnMailMergeCurrentRowChanged);
  FMailMergeDataController.DataSourceChanged.Add(OnMailMergeDataSourceChanged);
end;

procedure TdxDocumentModel.SubscribeMailMergePropertiesEvents;
begin
  FMailMergeProperties.ViewMergedDataChanged.Add(OnMailMergeViewMergedDataChanged);
  FMailMergeProperties.DataSourceChanged.Add(OnMailMergeDataSourceChanged);
end;

procedure TdxDocumentModel.SubscribeOptionsEvents;
begin
  inherited SubscribeOptionsEvents;
  SubscribeRichEditLayoutOptions;
  SubscribeTableOptions;
end;

procedure TdxDocumentModel.SubscribeProtectionPropertiesEvents;
begin
  if FProtectionProperties <> nil then
    FProtectionProperties.OnObtainAffectedRange.Add(OnProtectionPropertiesObtainAffectedRange);
end;

procedure TdxDocumentModel.SubscribeRichEditLayoutOptions;
begin
  if FLayoutOptions <> nil then
    FLayoutOptions.Changed.Add(OnLayoutOptionsChanged);
end;

procedure TdxDocumentModel.SubscribeTableOptions;
begin
  if FTableOptions <> nil then
    FTableOptions.Changed.Add(OnTableOptionsChanged);
end;

procedure TdxDocumentModel.SuspendSyntaxHighlight;
begin
  Inc(FSyntaxHighlightSuspendCount);
end;

procedure TdxDocumentModel.SwitchToEmptySelection;
begin
  SwitchToSelectionCore(TdxEmptySelection.Create(MainPieceTable));
end;

procedure TdxDocumentModel.SwitchToNormalSelection;
begin
  SwitchToSelectionCore(CreateNormalSelection);
end;

procedure TdxDocumentModel.SwitchToSelectionCore(ANewSelection: TdxSimpleSelection);
begin
  inherited SwitchToSelectionCore(ANewSelection);
  if IsUpdateLocked then
    DeferredChanges.ResetSelectionChanged;
end;

procedure TdxDocumentModel.ToggleAllFieldCodes(AShowCodes: Boolean);
var
  ATables: TdxFastList;
  APieceTable: TdxPieceTable;
  I: Integer;
begin
  BeginUpdate;
  try
    ATables := GetPieceTables(False);
    try
      for I := 0 to ATables.Count - 1 do
      begin
        APieceTable := TdxPieceTable(ATables[I]);
        APieceTable.ToggleAllFieldCodes(AShowCodes);
      end;
      ResetParagraphs;
    finally
      ATables.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.UnsubscribeDocumentPropertiesEvents;
begin
  if FDocumentProperties <> nil then
  begin
    FDocumentProperties.OnObtainAffectedRange.Remove(OnDocumentPropertiesObtainAffectedRange);
    FDocumentProperties.OnPageBackgroundChanged := nil;
  end;
end;

procedure TdxDocumentModel.UnsubscribeEventsForExport;
begin
  FOnCalculateDocumentVariable.Remove(RaiseCalculateDocumentVariableForExport);
  FOnBeforeExport.Remove(RaiseBeforeExportFromExportModel);
  FOnAfterExport.Remove(RaiseAfterExportFromExportModel);
end;

procedure TdxDocumentModel.UnsubscribeMailMergePropertiesEvents;
begin
  FMailMergeProperties.ViewMergedDataChanged.Remove(OnMailMergeViewMergedDataChanged);
  FMailMergeProperties.DataSourceChanged.Remove(OnMailMergeDataSourceChanged);
end;

procedure TdxDocumentModel.UnsubscribeProtectionPropertiesEvents;
begin
  if ProtectionProperties <> nil then
    ProtectionProperties.OnObtainAffectedRange.Remove(OnProtectionPropertiesObtainAffectedRange);
end;

procedure TdxDocumentModel.UnsubscribeRichEditLayoutOptions;
begin
  if LayoutOptions <> nil then
    FLayoutOptions.Changed.Remove(OnLayoutOptionsChanged);
end;

procedure TdxDocumentModel.UnubscribeMailMergeDataControllerEvents;
begin
  FMailMergeDataController.CurrentRowChanged.Remove(OnMailMergeCurrentRowChanged);
  FMailMergeDataController.DataSourceChanged.Remove(OnMailMergeDataSourceChanged);
end;

procedure TdxDocumentModel.UpdateFields(AUpdateType: TdxUpdateFieldOperationType;
  AOptions: TdxFieldUpdateOnLoadOptions = nil);
var
  ACalculatorService: IdxFieldCalculatorService;
  APieceTable: TdxPieceTable;
  APieceTables: TdxFastList;
  I, ACount: Integer;
begin
  BeginUpdate;
  try
    ACalculatorService := GetService<IdxFieldCalculatorService>;
    if (ACalculatorService <> nil) and (AUpdateType = TdxUpdateFieldOperationType.Load) then
      ACalculatorService.BeginUpdateFieldsOnLoad(AOptions);
    try
      APieceTables := GetPieceTables(False);
      try
        ACount := APieceTables.Count;
        for I := 0 to ACount - 1 do
        begin
          APieceTable := APieceTables[I];
          APieceTable.FieldUpdater.UpdateFields(AUpdateType);
        end;
      finally
        APieceTables.Free;
      end;
    finally
      if (ACalculatorService <> nil) and (AUpdateType = TdxUpdateFieldOperationType.Load) then
        ACalculatorService.EndUpdateFieldsOnLoad;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentModel.UpdateTableOfContents;
begin
end;

{ TdxNumberingListCalculatorCache }

constructor TdxNumberingListCalculatorCache.Create(ADocumentModel: TdxDocumentModel);
begin
  FCache := TObjectDictionary<TdxAbstractNumberingListIndex, TdxNumberingListCountersCalculator>.Create([doOwnsValues]);
  FDocumentModel := ADocumentModel;
  FLastProcessedParagraphIndex := -1;
end;

destructor TdxNumberingListCalculatorCache.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TdxNumberingListCalculatorCache.GetRangeListCounters(AParagraph: TdxParagraph): TIntegerDynArray;
var
  AIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  ACurrentParagraph: TdxParagraph;
  AListIndex: TdxAbstractNumberingListIndex;
  ACalculator: TdxNumberingListCountersCalculator;
  I, AListLevelIndex: Integer;
begin
  Assert(AParagraph.IsInList);
  AIndex := AParagraph.Index;
  if AIndex <= FLastProcessedParagraphIndex then
    Clear;

  AParagraphs := AParagraph.PieceTable.Paragraphs;
  for I := FLastProcessedParagraphIndex + 1 to AIndex do
  begin
    ACurrentParagraph := AParagraphs[I];
    AListIndex := ACurrentParagraph.GetAbstractNumberingListIndex;
    if AListIndex < AbstractNumberingListIndexMinValue then
      Continue;
    ACalculator := GetCacheItem(AListIndex);
    AListLevelIndex := ACurrentParagraph.GetListLevelIndex;
    if ACalculator.ShouldAdvanceListLevelCounters(ACurrentParagraph, DocumentModel.AbstractNumberingLists[AListIndex]) then
      ACalculator.AdvanceListLevelCounters(ACurrentParagraph, AListLevelIndex);
    if ACurrentParagraph = AParagraph then
    begin
      FLastProcessedParagraphIndex := AIndex;
      Exit(ACalculator.GetActualRangeCounters(AListLevelIndex));
    end;
  end;
  Assert(False);
  Result := nil;
end;

procedure TdxNumberingListCalculatorCache.Clear;
begin
  FCache.Clear;
  FLastProcessedParagraphIndex := -1;
end;

function TdxNumberingListCalculatorCache.GetCacheItem(AListIndex: TdxAbstractNumberingListIndex): TdxNumberingListCountersCalculator;
var
  AList: TdxAbstractNumberingList;
begin
  if FCache.TryGetValue(AListIndex, Result) then
    Exit;
  AList := DocumentModel.AbstractNumberingLists[AListIndex];
  Result := DocumentModel.CreateNumberingListCountersCalculator(AList);
  Result.BeginCalculateCounters;
  FCache.Add(AListIndex, Result);
end;

{ TdxPieceTable }

constructor TdxPieceTable.Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase);
begin
  inherited Create(ADocumentModel, AContentType);
  FShouldForceUpdateIntervals := True;
  FTables := TdxTableCollection.Create;
  FBookmarks := TdxBookmarkCollection.Create(Self);
  FRangePermissions := TdxRangePermissionCollection.Create(Self);
  FMyTables := TdxTableCellsManager.Create(Self);
  FCustomMarks := TdxCustomMarkCollection.Create;
  FTextBoxes := TdxTextBoxContentTypeList.Create;
  FCalculatorCache := TdxNumberingListCalculatorCache.Create(DocumentModel);
  FSpellCheckerManager := CreateSpellCheckerManager;
  SetShowHiddenText(False);
  Clear;
  FLayoutDependentTextInserter := TdxLayoutDependentTextInserter.Create(Self);
  FFootNoteRunInserter := TdxFootNoteRunInserter.Create(Self);
  FEndNoteRunInserter := TdxEndNoteRunInserter.Create(Self);
  FFieldUpdater := CreateFieldUpdater;
end;

destructor TdxPieceTable.Destroy;
begin
  if DocumentModel.DeferredChanges <> nil then
    DocumentModel.DeferredChanges.RunIndicesForSplit.Remove(Self);
  FreeAndNil(FNavigationVisibleTextFilter);
  Clear;

  FreeAndNil(FPrecalculatedNumberingListTexts);
  FreeAndNil(FTables);
  FreeAndNil(FBookmarks);
  FreeAndNil(FComments);
  FreeAndNil(FRangePermissions);
  FreeAndNil(FMyTables);
  FreeAndNil(FMyTables);
  FreeAndNil(FCustomMarks);
  FreeAndNil(FTextBoxes);
  FreeAndNil(FSpellCheckerManager);
  FreeAndNil(FLayoutDependentTextInserter);
  FreeAndNil(FFootNoteRunInserter);
  FreeAndNil(FEndNoteRunInserter);
  FreeAndNil(FFieldUpdater);
  FreeAndNil(FSpellCheckerManager);
  FreeAndNil(FCalculatorCache);
  inherited Destroy;
end;

function TdxPieceTable.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxPieceTable.GetFields: TdxFieldCollection;
begin
  Result := TdxFieldCollection(inherited Fields);
end;

procedure TdxPieceTable.CreateBookmark(APosition: TdxDocumentLogPosition; ALength: Integer; const AName: string);
const
  ChangeActions = [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSecondaryLayout];
var
  ABookmark: TdxBookmark;
  ARunInfo: TdxRunInfo;
begin
  DocumentModel.BeginUpdate;
  try
    ABookmark := CreateBookmarkCore(APosition, ALength, AName);
    ARunInfo := ABookmark.Interval;
    ApplyChangesCore(ChangeActions, ARunInfo.NormalizedStart.RunIndex, ARunInfo.NormalizedEnd.RunIndex);
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxPieceTable.ValidateBookmarkName(const AName: string): string;
var
  I: Integer;
  APrefix: string;
  ABookmarkNames: TStringList;
begin
  Result := AName;
  if not DocumentModel.BookmarkOptions.AllowNameResolution then
    Exit;
  ABookmarkNames := TStringList.Create;
  try
    ABookmarkNames.Sorted := True;
    for I := 0 to Bookmarks.Count - 1 do
      ABookmarkNames.Add(Bookmarks[I].Name);
    if ABookmarkNames.IndexOf(AName) < 0 then
      Exit;
    I := 1;
    APrefix := AName;
    repeat
      Result := Format('%s_%d', [APrefix, I]);
      Inc(I);
    until ABookmarkNames.IndexOf(Result) < 0;
  finally
    ABookmarkNames.Free;
  end;
end;

function TdxPieceTable.CreateBookmarkCore(APosition: TdxDocumentLogPosition; ALength: Integer;
  const AName: string; AForceUpdateInterval: Boolean = False): TdxBookmark;
var
  AItem: TdxInsertBookmarkHistoryItem;
begin
  AItem := TdxInsertBookmarkHistoryItem.Create(Self);
  AItem.Position := APosition;
  AItem.Length := ALength;
  AItem.BookmarkName := ValidateBookmarkName(AName);
  AItem.ForceUpdateInterval := AForceUpdateInterval;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
  Result := Bookmarks[AItem.IndexToInsert];
end;

procedure TdxPieceTable.CreateComment(APosition: TdxDocumentLogPosition; ALength: Integer; const AName, AId,
  AAuthor, ATime, ADate, AIdParent: string; AContent: TdxCommentContentType);
begin
  NotImplemented;
end;

function TdxPieceTable.CreateCommentCore(APosition: TdxDocumentLogPosition; ALength: Integer; const AName, AId,
  AAuthor, ATime, ADate, AIdParent: string; AContent: TdxCommentContentType): TdxComment;
begin
  Result := NotImplemented;
end;

procedure TdxPieceTable.CreateCustomMark(APosition: TdxDocumentLogPosition; AUserData: TObject);
begin
  NotImplemented;
end;

function TdxPieceTable.CreateCustomMarkCore(APostion: TdxDocumentLogPosition;
  AUserData: TObject): TdxCustomMark;
begin
  Result := NotImplemented;
end;

function TdxPieceTable.CreateDeleteContentOperation: TdxCustomDeleteContentOperation;
begin
  Result := TdxDeleteContentOperation.Create(Self);
end;

procedure TdxPieceTable.DeleteBackContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected: Boolean);
begin
  DeleteContent(ALogPosition, ALength, ADocumentLastParagraphSelected, False, False, False, True);
end;

procedure TdxPieceTable.DeleteBookmark(ABookmarkIndex: Integer);
var
  ABookmark: TdxBookmark;
  AChangeActions: TdxDocumentModelChangeActions;
  ARunInfo: TdxRunInfo;
begin
	DocumentModel.BeginUpdate();
	try
    ABookmark := Bookmarks[ABookmarkIndex];
		DeleteBookmarkCore(ABookmarkIndex);

		AChangeActions := [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetSecondaryLayout];
		ARunInfo := ABookmark.Interval;
		ApplyChangesCore(AChangeActions, ARunInfo.NormalizedStart.RunIndex, ARunInfo.NormalizedEnd.RunIndex);
	finally
		DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.DeleteBookmarkCore(ABookmarkIndex: Integer);
var
  AItem: TdxDeleteBookmarkHistoryItem;
begin
	AItem := TdxDeleteBookmarkHistoryItem.Create(Self);
	AItem.DeletedBookmarkIndex := ABookmarkIndex;
	DocumentModel.History.Add(AItem);
	AItem.Execute;
end;

procedure TdxPieceTable.DeleteComment(ACommentIndex: Integer);
begin
  NotImplemented;
end;

procedure TdxPieceTable.DeleteCommentCore(ACommentIndex: Integer);
begin
  NotImplemented;
end;

procedure TdxPieceTable.DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected, AAllowPartiallyDeletingField: Boolean);
begin
  DeleteContent(ALogPosition, ALength, ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, False);
end;

procedure TdxPieceTable.DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected: Boolean);
begin
  DeleteContent(ALogPosition, ALength, ADocumentLastParagraphSelected, False);
end;

procedure TdxPieceTable.DeleteCustomMark(ACustomMarkIndex: Integer);
begin
  NotImplemented;
end;

procedure TdxPieceTable.DeleteCustomMarkCore(ACustomMarkIndex: Integer);
begin
  NotImplemented;
end;

procedure TdxPieceTable.DeleteEmptyTableCellCore(ATableIndex, ARowIndex, ACellIndex: Integer);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxDeleteEmptyTableCellHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxDeleteEmptyTableCellHistoryItem.Create(Self, ATableIndex, ARowIndex, ACellIndex);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.DeleteEmptyTableRowCore(ATableIndex, ARowIndex: Integer);
var
  ATransaction: TdxHistoryTransaction;
  ATable: TdxTable;
  ARows: TdxTableRowCollection;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ARunIndex: TdxRunIndex;
  ACellsCount, ARowCount, I, AColumnIndex: Integer;
  ACell, ANextRowCell, AAfterNextRowCell, APrevRowCell: TdxTableCell;
  AItem: TdxDeleteTableRowHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := Tables[ATableIndex];
    ARows := ATable.Rows;
    ARow := ARows[ARowIndex];
    ACells := ARow.Cells;
    ARunIndex := Paragraphs[ACells.First.StartParagraphIndex].FirstRunIndex;
    ACellsCount := ACells.Count;
    ARowCount := ARows.Count;
    for I := ACellsCount - 1 downto 0 do
    begin
      ACell := ACells[I];
      if ACell.VerticalMerging <> TdxMergingState.None then
      begin
        AColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ACell, False);
        if ARowIndex + 1 < ARowCount then
          ANextRowCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ARows[ARowIndex + 1], AColumnIndex, False)
        else
          ANextRowCell := nil;
        if ACell.VerticalMerging = TdxMergingState.Restart then
        begin
          if ANextRowCell <> nil then
          begin
            Assert(ANextRowCell.VerticalMerging = TdxMergingState.Continue);
            if ARowIndex + 2 < ARowCount then
              AAfterNextRowCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ARows[ARowIndex + 2], AColumnIndex, False)
            else
              AAfterNextRowCell := nil;
            if (AAfterNextRowCell <> nil) and (AAfterNextRowCell.VerticalMerging = TdxMergingState.Continue) then
              ANextRowCell.VerticalMerging := TdxMergingState.Restart
            else
              ANextRowCell.VerticalMerging := TdxMergingState.None;
          end;
        end
        else
        begin
          Assert(ARowIndex > 0);
          APrevRowCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ARows[ARowIndex - 1], AColumnIndex, False);
          if (APrevRowCell <> nil) and (APrevRowCell.VerticalMerging = TdxMergingState.Restart) then
          begin
            if (ANextRowCell = nil) or (ANextRowCell.VerticalMerging <> TdxMergingState.Continue) then
              APrevRowCell.VerticalMerging := TdxMergingState.None;
          end;
        end;
      end;
      DeleteTableCellWithNestedTables(ATableIndex, ARowIndex, I);
    end;
    AItem := TdxDeleteTableRowHistoryItem.Create(Self, ATableIndex, ARowIndex);
    DocumentModel.History.Add(AItem);
    AItem.RunIndex := ARunIndex;
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.DeleteNumerationFromParagraph(AParagraph: TdxParagraph);
begin
  Assert(AParagraph <> nil);
  if not AParagraph.IsInList then
    TdxRichEditExceptions.ThrowArgumentException('paragraph', AParagraph);
  WriteParagraphLeftIndent(AParagraph);
  RemoveNumberingFromParagraph(AParagraph);
end;

procedure TdxPieceTable.DeleteNumerationFromParagraphAndChangeParagraphStyle(AParagraph: TdxParagraph;
  AParagraphStyleIndex: Integer);
begin
  AParagraph.ResetRunsCharacterFormatting;
  AParagraph.ParagraphStyleIndex := AParagraphStyleIndex;
  if AParagraph.IsInList then
    AParagraph.PieceTable.RemoveNumberingFromParagraph(AParagraph);
  if AParagraph.GetOwnNumberingListIndex = NumberingListIndexNoNumberingList then
  begin
    AParagraph.ResetNumberingListIndex(NumberingListIndexListIndexNotSetted);
    AParagraph.ParagraphProperties.ResetUse(
      [TdxUsedParagraphFormattingOption.UseFirstLineIndent, TdxUsedParagraphFormattingOption.UseLeftIndent]);
  end;
end;

procedure TdxPieceTable.DeleteRangePermission(APermission: TdxRangePermission);
var
  AItem: TdxDeleteRangePermissionHistoryItem;
begin
  AItem := TdxDeleteRangePermissionHistoryItem.Create(Self);
  AItem.DeletedRangePermission := APermission.Clone;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxPieceTable.DeleteSelectedTables(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
var
  ARoot: TdxTableCellNode;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  if (ARunInfo.Start = ARunInfo.&End) and not ADocumentLastParagraphSelected then
    Exit;
  AStartParagraphIndex := ARunInfo.Start.ParagraphIndex;
  AEndParagraphIndex := ARunInfo.&End.ParagraphIndex;
  ARoot := TableCellsManager.GetCellSubTree(AStartParagraphIndex, AEndParagraphIndex, MinInt);
  try
    DeleteTablesByNestedLevel(ARoot, ARunInfo, ADocumentLastParagraphSelected);
  finally
    ARoot.Free;
  end;
end;

procedure TdxPieceTable.DeleteTablesByNestedLevel(ACurrent: TdxTableCellNode; ARunInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
var
  I: Integer;
  ATable: TdxTable;
  ACurrentNode: TdxTableCellNode;
begin
  if ACurrent = nil then
    Exit;
  for I := ACurrent.ChildNodes.Count - 1 downto 0 do
  begin
    ACurrentNode := ACurrent.ChildNodes[I];
    if ACurrentNode.ChildNodes <> nil then
      DeleteTablesByNestedLevel(ACurrentNode, ARunInfo, ADocumentLastParagraphSelected);
    ATable := ACurrentNode.Cell.Table;
    if ((ACurrentNode.Cell <> nil) and (ATable.FirstRow.FirstCell = ACurrentNode.Cell)) and
      ShouldDeleteTable(ATable, ARunInfo, ADocumentLastParagraphSelected) then
      DeleteTableCore(ATable);
  end;
end;

procedure TdxPieceTable.DeleteTableCellWithContent(ADeletedCell: TdxTableCell;
  const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  DeleteTableCellWithContent(ADeletedCell, True, AServer);
end;

procedure TdxPieceTable.DeleteTableCellsWithShiftToTheUp(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
var
  ACommand: TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand;
begin
  ACommand := TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand.Create(Self, TdxSelectedCellsCollection(ASelectedCells));
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableCellWithContent(ADeletedCell: TdxTableCell;
  ACanNormalizeCellVerticalMerging: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableDeleteTableCellWithContentCommand;
begin
  ACommand := TdxPieceTableDeleteTableCellWithContentCommand.Create(Self, ADeletedCell, AServer);
  try
    ACommand.CanNormalizeCellVerticalMerging := ACanNormalizeCellVerticalMerging;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableCellWithNestedTables(ATableIndex, ARowIndex, ACellIndex: Integer);
var
  ACommand: TdxPieceTableDeleteTableCellWithNestedTablesCommand;
begin
  ACommand := TdxPieceTableDeleteTableCellWithNestedTablesCommand.Create(Self, ATableIndex, ARowIndex, ACellIndex);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableColumns(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection};
  const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableDeleteTableColumnsCommand;
begin
  Assert(ASelectedCells is TdxSelectedCellsCollection);
  ACommand := TdxPieceTableDeleteTableColumnsCommand.Create(Self, TdxSelectedCellsCollection(ASelectedCells), AServer);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableCore(ADeletedTable: TdxTable);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxDeleteTableHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxDeleteTableHistoryItem.Create(Self, ADeletedTable);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableFromTableCollection(ADeletedTable: TdxTable);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxDeleteTableFromTableCollectionHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxDeleteTableFromTableCollectionHistoryItem.Create(Self, ADeletedTable);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableRowWithContent(ADeletedRow: TdxTableRow);
var
  ATransaction: TdxHistoryTransaction;
  AStart, AEnd: TdxDocumentLogPosition;
  AEndParagraph: TdxParagraph;
  ALength: Integer;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AStart := Paragraphs[ADeletedRow.Cells.First.StartParagraphIndex].LogPosition;
    AEndParagraph := Paragraphs[ADeletedRow.Cells.Last.EndParagraphIndex];
    AEnd := AEndParagraph.EndLogPosition;
    ALength := AEnd - AStart + 1;
    DeleteEmptyTableRowCore(ADeletedRow.Table.Index, ADeletedRow.IndexInTable);
    DeleteContent(AStart, ALength, AStart + ALength >= DocumentEndLogPosition);
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.DeleteTableWithContent(ADeletedTable: TdxTable);
var
  ATransaction: TdxHistoryTransaction;
  ATableStartParagraph, ATableEndParagraph: TdxParagraph;
  AStartLogPosition: TdxDocumentLogPosition;
  ALength: Integer;
  ARunInfo: TdxRunInfo;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATableStartParagraph := Paragraphs[ADeletedTable.StartParagraphIndex];
    ATableEndParagraph := Paragraphs[ADeletedTable.EndParagraphIndex];
    AStartLogPosition := ATableStartParagraph.LogPosition;
    ALength := ATableEndParagraph.EndLogPosition - AStartLogPosition + 1;
    ARunInfo := ObtainAffectedRunInfo(AStartLogPosition, ALength);
    try
      DeleteSelectedTables(ARunInfo, True);
    finally
      ARunInfo.Free;
    end;
    DeleteContent(AStartLogPosition, ALength, AStartLogPosition + ALength >= DocumentEndLogPosition);
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.EnsureImagesLoadComplete;
var
  I: Integer;
begin
  for I := 0 to Runs.Count - 1 do
    EnsureImageLoadComplete(Runs[I]);
end;

procedure TdxPieceTable.EnsureImageLoadComplete(ARun: TdxTextRunBase);
var
  AInlinePictureRun: TdxInlinePictureRun;
begin
  AInlinePictureRun := Safe<TdxInlinePictureRun>.Cast(ARun);
  if AInlinePictureRun <> nil then
    AInlinePictureRun.Image.EnsureLoadComplete;
end;

function TdxPieceTable.ExtractMergedRanges(ARangePermissionInfoIndex: Integer): TdxRangePermissionCollectionEx;
var
  I: Integer;
  ARangePermission: TdxRangePermission;
begin
  Result := TdxRangePermissionMergedCollection.Create(Self);

  for I := RangePermissions.Count - 1 downto 0 do
  begin
    ARangePermission := RangePermissions[I];
    if ARangePermission.Properties.Index = ARangePermissionInfoIndex then
    begin
      Result.Add(ARangePermission);
      RangePermissions.Delete(I, False);
    end;
  end;
end;

procedure TdxPieceTable.AddFieldToTable(AField: TdxField; AIndex: Integer);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxInsertFieldHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxInsertFieldHistoryItem.Create(Self);
    AItem.InsertedFieldIndex := AIndex;
    AItem.InsertedField := AField;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.AddNumberingListToParagraph(AParagraph: TdxSimpleParagraph;
  ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
begin
  Assert(IsValidNumberingListIndex(ANumberingListIndex) and (ANumberingListIndex < DocumentModel.NumberingLists.Count));
  Assert(IsNumberingListLevelIndexValid(AParagraph, ANumberingListIndex, AListLevelIndex));
  Assert(not AParagraph.IsInNonStyleList);
  DocumentModel.BeginUpdate;
  try
    AddParagraphToList(AParagraph.Index, ANumberingListIndex, AListLevelIndex);
    DocumentModel.InvalidateDocumentLayout;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.AddPieceTables(AResult: TdxFastList; AIncludeUnreferenced: Boolean);
var
  I, ACount: Integer;
begin
  if (AIncludeUnreferenced or IsReferenced) and not (AResult.IndexOf(Self) >= 0) then
    AResult.Add(Self);
  ACount := FTextBoxes.Count;
  for I := 0 to ACount - 1 do
    if Self <> FTextBoxes[I].PieceTable then
      FTextBoxes[I].PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
end;

function TdxPieceTable.CreateParagraph: TdxSimpleParagraph;
begin
  Result := TdxParagraph.Create(Self);
end;

function TdxPieceTable.GetTableCore(AIndex: Integer): TdxCustomTable;
begin
  if AIndex >= 0 then
    Result := Tables[AIndex]
  else
    Result := nil;
end;

procedure TdxPieceTable.AppendText(APos: TdxInputPosition; ACh: Char);
begin
  Assert(APos.PieceTable = Self);
  InsertTextCore(APos, ACh);
end;

procedure TdxPieceTable.AppendImage(APos: TdxInputPosition; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
  AUseScreenDpi: Boolean = False);
begin
  AppendImage(APos, AImage, AScaleX, AScaleY, TdxAlphaColors.Empty, AUseScreenDpi);
end;

procedure TdxPieceTable.AppendImage(APos: TdxInputPosition; AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
  AFillColor: TdxAlphaColor; AUseScreenDpi: Boolean);
var
  ALastInsertedInlinePictureRunInfo: TdxLastInsertedInlinePictureRunInfo;
  ARun: TdxInlinePictureRun;
begin
  Assert(APos.PieceTable = Self);
  InsertInlineImageCore(APos.ParagraphIndex, APos.LogPosition, AImage, AScaleX, AScaleY, AFillColor, AUseScreenDpi, False);
  ALastInsertedInlinePictureRunInfo := LastInsertedInlinePictureRunInfo;
  ARun := TdxInlinePictureRun(ALastInsertedInlinePictureRunInfo.Run);
  ARun.ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

procedure TdxPieceTable.AppendMergedRanges(AMergedRanges: TdxRangePermissionCollectionEx);
var
  I: Integer;
begin
  for I := 0 to AMergedRanges.Count - 1 do
    RangePermissions.Add(AMergedRanges[I]);
end;

procedure TdxPieceTable.AppendText(APos: TdxInputPosition; const AText: string);
begin
  Assert(APos.PieceTable = Self);
  InsertTextCore(APos, AText);
end;

procedure TdxPieceTable.ApplyCharacterFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxRunPropertyModifierBase);
begin
  if ALogPositionStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPositionStart', ALogPositionStart);
  if ALength <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('length', ALength);
  ApplyCharacterStyleCore(ALogPositionStart, ALength, AModifier);
end;

procedure TdxPieceTable.ApplyCharacterStyle(ALogPositionStart: TdxDocumentLogPosition; ALength,
  AStyleIndex: Integer; AResetProperties: Boolean);
var
  AModifier: TdxRunCharacterStyleModifier;
begin
  if ALogPositionStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPositionStart', ALogPositionStart);
  if ALength <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('length', ALength);
  AModifier := TdxRunCharacterStyleModifier.Create(AStyleIndex, AResetProperties);
  try
    ApplyCharacterStyleCore(ALogPositionStart, ALength, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.ApplyCharacterStyle(ALogPositionStart: TdxDocumentLogPosition; ALength,
  AStyleIndex: Integer);
begin
  ApplyCharacterStyle(ALogPositionStart, ALength, AStyleIndex, True);
end;

procedure TdxPieceTable.ApplyCharacterStyleCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxRunPropertyModifierBase);
var
  ATransaction: TdxHistoryTransaction;
  ARunInfo: TdxRunInfo;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ARunInfo := ObtainAffectedRunInfo(ALogPositionStart, ALength);
    try
      if (ARunInfo.Start.RunIndex >= 0) and (ARunInfo.&End.RunIndex >= ARunInfo.Start.RunIndex) then
      begin
        ChangeCharacterStyle(ARunInfo, AModifier);
        TryToJoinRuns(ARunInfo);
      end;
    finally
      ARunInfo.Free;
    end;
  finally
    FreeAndNil(ATransaction);
  end;
end;

procedure TdxPieceTable.ApplyDocumentPermission(AStart, AEnd: TdxDocumentLogPosition;
  AInfo: TdxRangePermissionInfo);
var
  AItem: TdxInsertRangePermissionHistoryItem;
begin
  if AEnd - AStart < 0 then
    Exit;

  DocumentModel.BeginUpdate;
  try
    AItem := TdxInsertRangePermissionHistoryItem.Create(Self);
    AItem.Position := AStart;
    AItem.Length := AEnd - AStart;
    AItem.IndexToInsert := DocumentModel.Cache.RangePermissionInfoCache.GetItemIndex(AInfo);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxPieceTable.ApplyDocumentPermissionCore(AStart, AEnd: TdxDocumentLogPosition;
  ARangePermissionInfoIndex: Integer): TdxRunInfo;
var
  AMergedRanges: TdxRangePermissionCollectionEx;
  ARangePermission: TdxRangePermission;
begin
  AMergedRanges := ExtractMergedRanges(ARangePermissionInfoIndex);
  try
    ARangePermission := CreateRangePermission(AStart, AEnd, ARangePermissionInfoIndex);
    AMergedRanges.Add(ARangePermission);
    AppendMergedRanges(AMergedRanges);

    if AMergedRanges.Count > 0 then
      Result := AMergedRanges[0].Interval
    else
      Result := nil;
  finally
    AMergedRanges.Free;
  end;
end;

procedure TdxPieceTable.ApplyListLevelIndexToParagraph(AParagraph: TdxParagraph; ALevelIndex: Integer);
begin
  NotImplemented;
end;

procedure TdxPieceTable.ApplyNumberingToInsertedParagraph(AParagraphIndex: TdxParagraphIndex);
var
  APieceTable: TdxPieceTable;
  AOriginalParagraph, ANewParagraph: TdxParagraph;
  ANormalStyleIndex: Integer;
begin
  AOriginalParagraph := Paragraphs[AParagraphIndex + 1];
  if AOriginalParagraph.IsInList then
  begin
    ANewParagraph := Paragraphs[AParagraphIndex];
    AddParagraphToList(AParagraphIndex, AOriginalParagraph.GetOwnNumberingListIndex, AOriginalParagraph.GetOwnListLevelIndex);
    if not AOriginalParagraph.IsEmpty or not ANewParagraph.IsEmpty then
      CopyCharacterPropertiesToParagraphMark(AOriginalParagraph, ANewParagraph)
    else
    begin
      APieceTable := AOriginalParagraph.PieceTable;
      if AOriginalParagraph.IsInNonStyleList then
      begin
        ANormalStyleIndex := APieceTable.DocumentModel.ParagraphStyles.DefaultItemIndex;
        DeleteNumerationFromParagraphAndChangeParagraphStyle(AOriginalParagraph, ANormalStyleIndex);
        DeleteNumerationFromParagraphAndChangeParagraphStyle(ANewParagraph, ANormalStyleIndex);
      end
      else
      begin
        APieceTable.RemoveParagraphFromList(AOriginalParagraph.Index);
        APieceTable.RemoveParagraphFromList(ANewParagraph.Index);
      end;
    end;
  end
  else
    if AOriginalParagraph.GetOwnNumberingListIndex = NumberingListIndexNoNumberingList then
      AddParagraphToList(AParagraphIndex, AOriginalParagraph.GetOwnNumberingListIndex, AOriginalParagraph.GetOwnListLevelIndex);
end;

procedure TdxPieceTable.ApplyParagraphFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxParagraphPropertyModifierBase);
begin
  if ALogPositionStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPositionStart', ALogPositionStart);
  if ALength <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('length', ALength);
  ApplyParagraphFormattingCore(ALogPositionStart, ALength, AModifier);
end;

procedure TdxPieceTable.ApplyParagraphFormattingCore(ALogPositionStart: TdxDocumentLogPosition;
  ALength: Integer; AModifier: TdxParagraphPropertyModifierBase);
var
  I: Integer;
  ATransaction: TdxHistoryTransaction;
  ARunInfo: TdxRunInfo;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ARunInfo := FindRunInfo(ALogPositionStart, ALength);
    try
      for I := ARunInfo.Start.ParagraphIndex to ARunInfo.&End.ParagraphIndex do
        AModifier.ModifyParagraph(Paragraphs[I], I);
    finally
      ARunInfo.Free;
    end;
  finally
    FreeAndNil(ATransaction);
  end;
end;

procedure TdxPieceTable.ApplyParagraphStyle(ALogPositionStart: TdxDocumentLogPosition; ALength,
  AStyleIndex: Integer);
var
  AModifier: TdxRunCharacterStyleModifier;
begin
  if ALogPositionStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPositionStart', ALogPositionStart);
  if ALength <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('length', ALength);
  AModifier := TdxRunCharacterStyleModifier.Create(AStyleIndex);
  try
    if ShouldApplyStyleToParagraphs(ALogPositionStart, ALength) then
      ApplyParagraphStyleCore(ALogPositionStart, ALength, AModifier)
    else
      ApplyCharacterStyleCore(ALogPositionStart, ALength, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.ApplyParagraphStyleCore(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxRunPropertyModifierBase);
var
  I: Integer;
  ARunInfo: TdxRunInfo;
  ATransaction: TdxHistoryTransaction;
  AEndParagraphIndex: TdxParagraphIndex;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ARunInfo := FindRunInfo(ALogPositionStart, ALength);
    try
      AEndParagraphIndex := ARunInfo.&End.ParagraphIndex;
      for I := ARunInfo.Start.ParagraphIndex to AEndParagraphIndex do
        ChangeParagraphStyle(Paragraphs[I], AModifier);
    finally
      ARunInfo.Free;
    end;
  finally
    FreeAndNil(ATransaction);
  end;
end;

procedure TdxPieceTable.ApplyTableStyleCore(ATables: TdxList<TdxTable>; AStyleIndex: Integer);
var
  ATransaction: TdxHistoryTransaction;
  ATable: TdxTable;
  I: Integer;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    for I := 0 to ATables.Count - 1 do
    begin
      ATable := ATables[I];
      ResetTableAllUse(ATable);
      ATable.StyleIndex := AStyleIndex;
    end;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTable.CalculateFieldResult(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode;
  AUpdateType: TdxUpdateFieldOperationType): TdxCalculateFieldResult;
var
  AFieldCalculatorService: IdxFieldCalculatorService;
begin
  AFieldCalculatorService := DocumentModel.GetService<IdxFieldCalculatorService>;
  if AFieldCalculatorService <> nil then
    Result := AFieldCalculatorService.CalculateField(Self, AField, AMailMergeDataMode, AUpdateType)
  else
    Result := TdxCalculateFieldResult.Create(TdxCalculatedFieldValue.NullValue, [TdxUpdateFieldOperationType.Normal]);
end;

function TdxPieceTable.CanContainCompositeContent: Boolean;
begin
  Result := not IsComment;
end;

function TdxPieceTable.CanEditRangeLength(AStart: TdxDocumentLogPosition; ALength: Integer): Boolean;
var
  AEnd: TdxDocumentLogPosition;
begin
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit(True);
  AEnd := AStart + ALength;
  Result := CanEditRange(AStart, AEnd);
end;

function TdxPieceTable.CanEditRange(AStart, AEnd: TdxDocumentLogPosition): Boolean;
var
  APermissions: TdxRangePermissionCollection;
  ACount, I: Integer;
  ARangePermission: TdxRangePermission;
begin
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit(True);

  APermissions := RangePermissions;
  ACount := APermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    ARangePermission := APermissions[I];
    if (((AStart = AEnd) and (AStart >= ARangePermission.Start) and (AStart <= ARangePermission.&End)) or
       ((AStart <> AEnd) and ARangePermission.Contains(AStart, AEnd))) and
       IsPermissionGranted(ARangePermission) then
      Exit(True);
  end;
  Result := False;
end;

function TdxPieceTable.CanEditSelection: Boolean;
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  if ASelection.PieceTable <> Self then
    Exit(False);
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit(True);
  Result := CanEditSelectionItems(ASelection.Items);
end;

function TdxPieceTable.CanEditSelectionItems(AItems: TdxSelectionItemList): Boolean;
var
  I: Integer;
  ASelectionItem: TdxSelectionItem;
begin
  Result := True;
  if not DocumentModel.IsDocumentProtectionEnabled then
    Exit;
  for I := 0 to AItems.Count - 1 do
  begin
    ASelectionItem := AItems[I];
    if not CanEditRange(ASelectionItem.NormalizedStart, ASelectionItem.NormalizedEnd) then
      Exit(False);
  end;
end;

procedure TdxPieceTable.ChangeCellEndParagraphIndex(ATableCell: TdxTableCell; AIndex: TdxParagraphIndex);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxChangeCellEndParagraphIndexHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxChangeCellEndParagraphIndexHistoryItem.Create(Self, ATableCell, AIndex);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.ChangeCellStartParagraphIndex(ATableCell: TdxTableCell; AIndex: TdxParagraphIndex);
var
  AItem: TdxChangeCellStartParagraphIndexHistoryItem;
  ATransaction: TdxHistoryTransaction;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxChangeCellStartParagraphIndexHistoryItem.Create(Self, ATableCell, AIndex);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.ChangeParagraphStyle(AParagraph: TdxParagraph; AModifier: TdxRunPropertyModifierBase);
var
  I: Integer;
begin
  for I := AParagraph.FirstRunIndex to AParagraph.LastRunIndex do
    AModifier.ModifyTextRun(Runs[I], I);
end;

procedure TdxPieceTable.Clear;
begin
  inherited Clear;
  FTables.Clear;
  FBookmarks.Clear;
  FRangePermissions.Clear;
  FMyTables.Clear;
  FTextBoxes.Clear;
  FSpellCheckerManager.Clear;
end;

procedure TdxPieceTable.ClearFontCacheIndices;
var
  I: Integer;
begin
  for I := 0 to Runs.Count - 1 do
    ClearRunFontCacheIndex(Runs[I]);
end;

procedure TdxPieceTable.ClearRunFontCacheIndex(ARun: TdxTextRunBase);
begin
  ARun.ResetFontCacheIndex;
end;

procedure TdxPieceTable.ConvertParagraphsIntoTableRow(ARow: TdxTableRow; AIndex: TdxParagraphIndex;
  AParagraphCount: Integer);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxConvertParagraphsIntoTableRowHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxConvertParagraphsIntoTableRowHistoryItem.Create(Self, ARow, AIndex, AParagraphCount);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTable.ConvertParagraphsToTable(AFirstParagraphIndex: TdxParagraphIndex; ARowCount,
  ACellCount: Integer);
var
  AItem: TdxCreateTableHistoryItem;
begin
  DocumentModel.BeginUpdate;
  try
    AItem := TdxCreateTableHistoryItem.Create(Self, AFirstParagraphIndex, ARowCount, ACellCount);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.CopyCharacterPropertiesToParagraphMark(ASourceParagraph,
  ATargetParagraph: TdxParagraph);
var
  ARuns: TdxTextRunCollection;
  ASourceRun, ATargetRun: TdxTextRunBase;
begin
  ARuns := ASourceParagraph.PieceTable.Runs;
  ASourceRun := ARuns[ASourceParagraph.LastRunIndex];
  ATargetRun := ARuns[ATargetParagraph.LastRunIndex];
  ATargetRun.CharacterStyleIndex := ASourceRun.CharacterStyleIndex;
  ATargetRun.CharacterProperties.CopyFrom(ASourceRun.CharacterProperties);
end;

function TdxPieceTable.CreateFieldUpdater: TdxFieldUpdater;
begin
  Result := TdxFieldUpdater.Create(Self);
end;

function TdxPieceTable.CreateNavigationVisibleTextFilter(AShowHiddenText: Boolean): TdxVisibleTextFilterBase;
begin
  if AShowHiddenText then
    Result := TdxEmptyTextFilterSkipFloatingObjects.Create(Self)
  else
    Result := TdxVisibleTextFilterSkipFloatingObjects.Create(Self);
end;

function TdxPieceTable.CreateRangePermission(AStart, AEnd: TdxDocumentLogPosition;
  ARangePermissionInfoIndex: Integer): TdxRangePermission;
begin
  Result := TdxRangePermission.Create(Self, AStart, AEnd);
  Result.Properties.SetIndexInitial(ARangePermissionInfoIndex);
end;

function TdxPieceTable.CreateSpellCheckerManager: TdxRichEditSpellCheckerManager;
begin
  Result := TdxRichEditSpellCheckerManager(ContentType.CreateSpellCheckerManager(Self));
end;

procedure TdxPieceTable.AddParagraphToList(AParagraphIndex: TdxParagraphIndex;
  ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxAddParagraphToListHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxAddParagraphToListHistoryItem.Create(Self);
    AItem.ParagraphIndex := AParagraphIndex;
    AItem.NumberingListIndex := ANumberingListIndex;
    AItem.ListLevelIndex := AListLevelIndex;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    FreeAndNil(ATransaction);
  end;
end;

function TdxPieceTable.CreateTableCellCore(ARow: TdxTableRow; AStart, AEnd: TdxParagraphIndex): TdxTableCell;
begin
  Result := CreateTableCellCore(ARow, ARow.Cells.Count, AStart, AEnd);
end;

function TdxPieceTable.CreateTableCellCore(ARow: TdxTableRow; AInsertedIndex: Integer; AStart,
  AEnd: TdxParagraphIndex): TdxTableCell;
var
  ACommand: TdxPieceTableCreateCellEmptyCommand;
begin
  ACommand := TdxPieceTableCreateCellEmptyCommand.Create(Self, ARow, AInsertedIndex, AStart, AEnd);
  try
    ACommand.Execute;
    Result := ACommand.InsertedCell;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTable.CreateTableCore(ASourceCell: TdxTableCell): TdxTable;
var
  ACommand: TdxPieceTableCreateEmptyTableCommand;
begin
  ACommand := TdxPieceTableCreateEmptyTableCommand.Create(Self, ASourceCell);
  try
    ACommand.Execute;
    Result := ACommand.NewTable;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTable.CreateTableRowCore(ATable: TdxTable; ARowIndex: Integer): TdxTableRow;
var
  ACommand: TdxPieceTableCreateRowEmptyCommand;
begin
  ACommand := TdxPieceTableCreateRowEmptyCommand.Create(Self, ATable, ARowIndex);
  try
    ACommand.Execute;
    Result := ACommand.InsertedRow;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTable.CreateTextRunsDeletedHistoryItem: TdxRichEditHistoryItem;
begin
  Result := TdxTextRunsDeletedHistoryItem.Create(Self);
end;

function TdxPieceTable.CreateTableRowCore(ATable: TdxTable): TdxTableRow;
begin
  Result := CreateTableRowCore(ATable, ATable.Rows.Count);
end;

function TdxPieceTable.FindParagraph(ALogPosition: TdxDocumentLogPosition): TdxParagraph;
begin
  Result := Paragraphs[FindParagraphIndex(ALogPosition)];
end;

procedure TdxPieceTable.FixParagraphFramesInTables;
begin
  FixParagraphFramesInTablesCore;
end;

procedure TdxPieceTable.FixLastParagraph;
begin
  if ShouldFixLastParagraph then
    FixLastParagraphCore;
  if not DocumentModel.DocumentCapabilities.ParagraphsAllowed then
    UnsafeRemoveLastSpaceSymbolRun;
end;

procedure TdxPieceTable.FixLastParagraphCore;
var
  ACount: Integer;
begin
  ACount := Paragraphs.Count;
  RemoveLastParagraph(ACount);
  FixLastParagraphOfLastSection(ACount);
end;

procedure TdxPieceTable.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
begin
  ContentType.FixLastParagraphOfLastSection(AOriginalParagraphCount);
end;

procedure TdxPieceTable.RemoveLastParagraph(AOriginalParagraphCount: Integer);
var
  AItem: TdxRemoveLastParagraphHistoryItem;
begin
  AItem := TdxRemoveLastParagraphHistoryItem.Create(Self);
  AItem.OriginalParagraphCount := AOriginalParagraphCount;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

function TdxPieceTable.ShouldFixLastParagraph: Boolean;
var
  ALastParagraph: TdxParagraph;
begin
  Result := Paragraphs.Count > 1;
  if Result then
  begin
    ALastParagraph := Paragraphs.Last;
    Result := ALastParagraph.IsEmpty;
    if Result then
      Result := (ALastParagraph.Index <= 0) or
        (Paragraphs[ALastParagraph.Index - 1].GetCell = nil);
  end;
end;

procedure TdxPieceTable.UnsafeRemoveLastSpaceSymbolRun;
var
  ARunIndex: TdxRunIndex;
  ARun: TdxTextRun;
begin
  Assert(not DocumentModel.DocumentCapabilities.ParagraphsAllowed);
  if Runs.Count <= 1 then
    Exit;
  ARunIndex := Runs.Count - 2;
  if Runs[ARunIndex] is TdxTextRun then
  begin
    ARun := TdxTextRun(Runs[ARunIndex]);
    if ARun.Length = 1 then
      DocumentModel.UnsafeEditor.DeleteRuns(Self, ARunIndex, 1)
    else
      DeleteContent(DocumentEndLogPosition - 1, 1, False);
  end;
end;

procedure TdxPieceTable.FixTables;
var
  I: Integer;
  ATable: TdxTable;
begin
  for I := 0 to Tables.Count - 1 do
  begin
    ATable := Tables[I];
    ATable.Normalize;
    ATable.NormalizeRows;
    ATable.NormalizeTableGrid;
    ATable.NormalizeCellColumnSpans;
  end;
end;

procedure TdxPieceTable.ForceCheckTablesIntegrity;
begin
end;

function TdxPieceTable.GetBookmarks(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
var
  I: Integer;
begin
  Result := TdxBookmarkList.Create;
  for I := 0 to Bookmarks.Count - 1 do
    if AIncludeHiddenBookmarks or not Bookmarks[I].IsHidden then
      Result.Add(Bookmarks[I]);
end;

function TdxPieceTable.GetCopyManager(ASourcePieceTable: TdxPieceTable;
  AInsertOptions: TdxInsertOptions): TdxCustomDocumentModelCopyManager;
begin
  case AInsertOptions of
    TdxInsertOptions.KeepSourceFormatting:
      Result := GetCopyManagerCore(ASourcePieceTable, TdxParagraphNumerationCopyOptions.CopyAlways, TdxFormattingCopyOptions.KeepSourceFormatting);
    TdxInsertOptions.MatchDestinationFormatting:
      Result := GetCopyManagerCore(ASourcePieceTable, TdxParagraphNumerationCopyOptions.CopyIfWholeSelected, TdxFormattingCopyOptions.UseDestinationStyles);
  else
    Result := nil;
  end;
end;

function TdxPieceTable.GetCopyManagerCore(ASourcePieceTable: TdxPieceTable;
  AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
  AFormattingCopyOptions: TdxFormattingCopyOptions): TdxCustomDocumentModelCopyManager;
begin
  Result := TdxDocumentModelCopyManager.Create(ASourcePieceTable, Self, AParagraphNumerationCopyOptions, AFormattingCopyOptions);
end;

function TdxPieceTable.GetDocumentEndLogPosition: TdxDocumentLogPosition;
var
  ALastParagraph: TdxParagraph;
begin
  ALastParagraph := Paragraphs.Last;
  Result := ALastParagraph.LogPosition + ALastParagraph.Length - 1;
end;

function TdxPieceTable.CreateObjectInserter: TdxObjectInserter;
begin
  Result := TdxTextInserter.Create(Self);
end;

function TdxPieceTable.GetDocumentStartLogPosition: TdxDocumentLogPosition;
begin
  Result := 0;
end;

function TdxPieceTable.GetEntireBookmarks(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
begin
  Result := Bookmarks.GetEntireBookmarksCore(AStart, ALength);
end;

function TdxPieceTable.GetEntireComments(AStart: TdxDocumentLogPosition; ALength: Integer): TdxCommentList;
begin
  Result := NotImplemented;
end;

function TdxPieceTable.GetEntireFieldsFromInterval(AStart, AEnd: TdxRunIndex): TdxFieldList;
var
  I, ACount: Integer;
  AField: TdxField;
begin
  Result := TdxFieldList.Create;
  ACount := Fields.Count;
  for I := 0 to ACount - 1 do
  begin
    AField := Fields[I];
    if AStart <= AField.FirstRunIndex then
    begin
      if AEnd >= AField.LastRunIndex then
        Result.Add(AField)
      else
        Break;
    end;
  end;
end;

function TdxPieceTable.GetEntireRangePermissions(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
begin
  Result := RangePermissions.GetEntireBookmarksCore(AStart, ALength);
end;

function TdxPieceTable.GetFieldsInsideInterval(AFirstRunIndex, ALastRunIndex: TdxRunIndex): TdxFieldList;
begin
  Result := NotImplemented;
end;

function TdxPieceTable.GetFieldToken(AField: TdxField): IdxToken;
var
  AIterator: TdxDocumentFieldIterator;
  AScanner: TdxFieldScanner;
begin
  AIterator := TdxDocumentFieldIterator.Create(Self, AField);
  try
    AScanner := TdxFieldScanner.Create(AIterator, DocumentModel.MaxFieldSwitchLength,
      DocumentModel.EnableFieldNames,
      SupportFieldCommonStringFormat);
    try
      Result := AScanner.Scan;
    finally
      AScanner.Free;
    end;
  finally
    AIterator.Free;
  end;
end;

function TdxPieceTable.GetFilteredPlainText(const AStart, AEnd: TdxDocumentModelPosition;
  const APredicate: TdxPredicate<TdxRunIndex>): string;
var
  ABuilder: TStringBuilder;
  AStartRun: TdxTextRunBase;
  I: TdxRunIndex;
begin
	if AEnd.LogPosition - AStart.LogPosition <= 0 then
		Exit('');
  AStartRun := Runs[AStart.RunIndex];
  if AStart.RunIndex = AEnd.RunIndex then
  begin
    if APredicate(AStart.RunIndex) then
      Result := AStartRun.GetPlainText(TextBuffer, AStart.RunOffset, AEnd.RunOffset - 1)
    else
      Result := '';
  end
  else
  begin
    ABuilder := TStringBuilder.Create;
    try
      if APredicate(AStart.RunIndex) then
        ABuilder.Append(AStartRun.GetPlainText(TextBuffer, AStart.RunOffset, AStartRun.Length - 1));
      for I := AStart.RunIndex + 1 to AEnd.RunIndex - 1 do
        if APredicate(I) then
          ABuilder.Append(GetRunPlainText(I));
      if (AEnd.RunOffset > 0) and APredicate(AEnd.RunIndex) then
        ABuilder.Append(Runs[AEnd.RunIndex].GetPlainText(TextBuffer, 0, AEnd.RunOffset - 1));
      Result := ABuilder.ToString;
    finally
      ABuilder.Free;
    end;
  end;
end;

type
  { TdxFloatingObjectZOrderComparer }

  TdxFloatingObjectZOrderComparer = class(TcxIUnknownObject, IComparer<IdxZOrderedObject>)
  public
    function Compare(const AFirst: IdxZOrderedObject; const ASecond: IdxZOrderedObject): Integer;
  end;

{ TdxFloatingObjectZOrderComparer }

function TdxFloatingObjectZOrderComparer.Compare(const AFirst: IdxZOrderedObject; const ASecond: IdxZOrderedObject): Integer;
begin
  Result := TdxComparer<Integer>.Default.Compare(AFirst.ZOrder, ASecond.ZOrder);
end;

function TdxPieceTable.GetFloatingObjectList: TdxIZOrderedObjectList;
var
  I: TdxRunIndex;
  AAnchorRun: TdxFloatingObjectAnchorRun;
  AComparer: TdxFloatingObjectZOrderComparer;
begin
  Result := TdxIZOrderedObjectList.Create;
  for I := 0 to Runs.Count - 1 do
  begin
    AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(Runs[I]);
    if AAnchorRun <> nil then
      Result.Add(AAnchorRun.FloatingObjectProperties);
  end;
  AComparer := TdxFloatingObjectZOrderComparer.Create;
  try
    Result.Sort(AComparer);
  finally
    AComparer.Free;
  end;
end;

function TdxPieceTable.GetIsComment: Boolean;
begin
  Result := ContentType.IsComment;
end;

function TdxPieceTable.GetIsEmpty: Boolean;
begin
  Result := DocumentEndLogPosition - DocumentStartLogPosition <= 0;
end;

procedure TdxPieceTable.InitializeUncheckedInterval;
begin
  SpellCheckerManager.InitializeUncheckedInterval;
end;

function TdxPieceTable.CreateFieldCollection: TdxFieldCollectionBase;
begin
  Result := TdxFieldCollection.Create(Self);
end;

function TdxPieceTable.CreateParagraphCollection: TdxParagraphBaseCollection;
begin
  Result := TdxParagraphCollection.Create;
end;

procedure TdxPieceTable.SetPrecalculatedNumberingListTexts(const Value: TDictionary<TdxParagraph, string>);
begin
  if FPrecalculatedNumberingListTexts = Value then
    Exit;
  FPrecalculatedNumberingListTexts.Free;
  FPrecalculatedNumberingListTexts := Value;
end;

procedure TdxPieceTable.SetSpellCheckerManager(const Value: TdxRichEditSpellCheckerManager);
begin
  if FSpellCheckerManager = Value then
    Exit;
  FSpellCheckerManager.Free;
  FSpellCheckerManager := Value;
end;

function TdxPieceTable.GetIsEndNote: Boolean;
begin
  Result := ContentType.IsEndNote;
end;

function TdxPieceTable.GetIsFooter: Boolean;
begin
  Result := ContentType.IsFooter;
end;

function TdxPieceTable.GetIsFootNote: Boolean;
begin
  Result := ContentType.IsFootNote;
end;

function TdxPieceTable.GetIsHeader: Boolean;
begin
  Result := ContentType.IsHeader;
end;

function TdxPieceTable.GetIsHeaderFooter: Boolean;
begin
  Result := ContentType.IsHeaderFooter;
end;

function TdxPieceTable.GetIsNote: Boolean;
begin
  Result := ContentType.IsNote;
end;

function TdxPieceTable.GetIsReferenced: Boolean;
begin
  Result := ContentType.IsReferenced;
end;

function TdxPieceTable.GetIsTextBox: Boolean;
begin
  Result := ContentType.IsTextBox;
end;

function TdxPieceTable.GetLastInsertedFloatingObjectAnchorRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo;
begin
  Result := DocumentModel.GetLastInsertedFloatingObjectAnchorRunInfo(Self);
end;

function TdxPieceTable.GetParagraphs: TdxParagraphCollection;
begin
  Result := TdxParagraphCollection(inherited Paragraphs);
end;

procedure TdxPieceTable.GetNumberingListText(AIndex: TdxRunIndex; ASb: TStringBuilder);
var
  AParagraph: TdxSimpleParagraph;
begin
  AParagraph := Runs[AIndex].Paragraph;
  if AParagraph.IsInList and (AIndex = AParagraph.FirstRunIndex) then
    ASb.Append(AParagraph.GetNumberingListText);
end;

function TdxPieceTable.GetPlainText(const AStartPos, AEndPos: TdxDocumentModelPosition): string;
var
  ARun: TdxTextRunBase;
  SB: TStringBuilder;
  I, AEndPosIndex: TdxRunIndex;
begin
  if AEndPos.LogPosition - AStartPos.LogPosition <= 0 then
    Exit('');
  ARun := Runs[AStartPos.RunIndex];
  if AStartPos.RunIndex = AEndPos.RunIndex then
    Exit(ARun.GetPlainText(textBuffer, AStartPos.RunOffset, AEndPos.RunOffset - 1));
  SB := TStringBuilder.Create;
  try
    if AStartPos.LogPosition = ARun.Paragraph.LogPosition then
      GetNumberingListText(AStartPos.RunIndex, SB);
    SB.Append(ARun.GetPlainText(textBuffer, AStartPos.RunOffset, ARun.Length - 1));
    AEndPosIndex := AEndPos.RunIndex;
    for I := AStartPos.RunIndex + 1 to AEndPosIndex - 1 do
    begin
      GetNumberingListText(I, SB);
      SB.Append(GetRunPlainText(I));
    end;
    if AEndPos.RunOffset > 0 then
    begin
      ARun := Runs[AEndPos.RunIndex];
      GetNumberingListText(AEndPos.RunIndex, SB);
      SB.Append(ARun.GetPlainText(TextBuffer, 0, AEndPos.RunOffset - 1));
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;


function TdxPieceTable.GetPlainText2(const AStartPos, AEndPos: TdxDocumentModelPosition): string;
begin
  NotImplemented;
end;

function TdxPieceTable.GetPlainText2(const AStartPos, AEndPos: TdxFormatterPosition): string;
begin
  NotImplemented;
end;

function TdxPieceTable.GetRunInfoByTableCell(ACell: TdxTableCell): TdxRunInfo;
var
  AStartParagraph, AEndParagraph: TdxParagraph;
  AStartLogPosition: TdxDocumentLogPosition;
  ALength: Integer;
begin
  AStartParagraph := Paragraphs[ACell.StartParagraphIndex];
  AEndParagraph := Paragraphs[ACell.EndParagraphIndex];
  AStartLogPosition := AStartParagraph.LogPosition;
  ALength := AEndParagraph.EndLogPosition - AStartLogPosition + 1;
  Result := FindRunInfo(AStartLogPosition, ALength);
end;

procedure TdxPieceTable.FixParagraphFramesInTablesCore;
var
  I: Integer;
  ATable: TdxTable;
begin
  for I := 0 to FTables.Count - 1 do
  begin
    ATable := Tables[I];
    if ATable.StartParagraphIndex < ATable.EndParagraphIndex then
      ATable.EnsureTableHasSameFrameProperties;
  end;
end;

procedure TdxPieceTable.RemoveFieldWithCode(AField: TdxField);
var
  AStartLogPosition: TdxDocumentLogPosition;
begin
  DocumentModel.BeginUpdate;
  try
    RemoveField(AField);
    DeleteContent(GetRunLogPosition(AField.LastRunIndex), 1, False);
    AStartLogPosition := GetRunLogPosition(AField.FirstRunIndex);
    DeleteContent(AStartLogPosition, GetRunLogPosition(AField.Result.Start) - AStartLogPosition, False);
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxPieceTable.GetRangeListCounters(AParagraph: TdxParagraph): TIntegerDynArray;
begin
  Assert(AParagraph.IsInList);
  Result := FCalculatorCache.GetRangeListCounters(AParagraph);
end;

procedure TdxPieceTable.BookmarksClear;
begin
end;

function TdxPieceTable.CollectFieldsToProcess(AParent: TdxField): TdxFieldList;
var
  ACount, I: Integer;
begin
  Result := TdxFieldList.Create;
  ACount := Fields.Count;
  for I := 0 to ACount - 1 do
  begin
    if Fields[I].Parent = AParent then
      Result.Add(Fields[I]);
  end;
end;

function TdxPieceTable.GetDebugVisualizerData: string;
var
  AStream: TMemoryStream;
  AWriter: TWriter;
begin
  if Runs.Count = 0 then
    Exit('');

  Result := TPath.GetTempPath + TPath.GetGUIDFileName;
  AStream := TMemoryStream.Create;
  try
    AWriter := TWriter.Create(AStream, $8000);
    try
      WriteDebugVisualizerData(AWriter);
    finally
      AWriter.Free;
    end;
    AStream.SaveToFile(Result);
  finally
    AStream.Free;
  end;
end;

procedure TdxPieceTable.WriteDebugVisualizerData(AWriter: TWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  for I := 0 to Runs.Count - 1 do
    Runs[I].WriteDebugVisualizerData(AWriter);
  AWriter.WriteListEnd;
  AWriter.FlushBuffer;
end;

function TdxPieceTable.GetTocFields: TdxFieldList;
var
  ACount, I: Integer;
begin
  Result := TdxFieldList.Create;
  ACount := Fields.Count;
  for I := 0 to ACount - 1 do
    if IsTocField(Fields[I]) then
      Result.Add(Fields[I]);
end;

function TdxPieceTable.HasInlinePicture(ARunInfo: TdxRunInfo): Boolean;
var
  I: Integer;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  Result := False;
  AStartRunIndex := ARunInfo.NormalizedStart.RunIndex;
  AEndRunIndex := ARunInfo.NormalizedEnd.RunIndex;
  for I := AStartRunIndex to AEndRunIndex - 1 do
    if Runs[I] is TdxInlinePictureRun then
      Exit(True);
end;

procedure TdxPieceTable.InheritParagraphRunStyle(APos: TdxInputPosition; AParagraphRun: TdxTextRunBase);
begin
  InheritParagraphRunStyleCore(APos, AParagraphRun);
  APos.LogPosition := APos.LogPosition + 1;
  APos.ParagraphIndex := APos.ParagraphIndex + 1;
end;

procedure TdxPieceTable.InheritParagraphRunStyleCore(APos: TdxInputPosition; AParagraphRun: TdxTextRunBase);
var
  ACharacterProperties: TdxCharacterProperties;
begin
  ACharacterProperties := AParagraphRun.CharacterProperties;
  ACharacterProperties.CopyFrom(APos.CharacterFormatting);
  AParagraphRun.CharacterStyleIndex := APos.CharacterStyleIndex;
end;

function TdxPieceTable.InsertFloatingObjectAnchorCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition): TdxFloatingObjectAnchorRun;
begin
  Result := InsertFloatingObjectAnchorCore(AParagraphIndex, ALogPosition, False);
end;

function TdxPieceTable.InsertFloatingObjectAnchorCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean): TdxFloatingObjectAnchorRun;
var
  AInserter: TdxFloatingObjectAnchorInserter;
begin
  TextBuffer.Append(TdxCharacters.FloatingObjectMark);
  AInserter := TdxFloatingObjectAnchorInserter.Create(Self);
  try
    InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
  Result := TdxFloatingObjectAnchorRun(LastInsertedFloatingObjectAnchorRunInfo.Run);
end;

procedure TdxPieceTable.InsertFloatingObjectAnchor(ALogPosition: TdxDocumentLogPosition);
begin
  InsertFloatingObjectAnchor(ALogPosition, false);
end;

procedure TdxPieceTable.InsertFloatingObjectAnchor(ALogPosition: TdxDocumentLogPosition;
  AForceVisible: Boolean);
var
  ARunInfo: TdxRunInfo;
  ACommand: TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand;
begin
  ARunInfo := FindRunInfo(ALogPosition, 1);
  try
    if Runs[ARunInfo.Start.RunIndex] is TdxFieldResultEndRun then
      Inc(ALogPosition);
  finally
    ARunInfo.Free;
  end;
  if ContentType.IsTextBox then
    TdxRichEditExceptions.ThrowInternalException;
  ACommand := TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand.Create(Self, ALogPosition, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTable.InsertFloatingObjectAnchorCore(APos: TdxInputPosition): TdxFloatingObjectAnchorRun;
var
  ALastInsertedFloatingObjectAnchorRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo;
begin
  Assert(APos.PieceTable = Self);
  InsertFloatingObjectAnchorCore(APos.ParagraphIndex, APos.LogPosition, False);
  ALastInsertedFloatingObjectAnchorRunInfo := LastInsertedFloatingObjectAnchorRunInfo;
  Result := TdxFloatingObjectAnchorRun(ALastInsertedFloatingObjectAnchorRunInfo.Run);
  Result.ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

function TdxPieceTable.InsertFootNoteRun(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer): TdxTextRunBase;
begin
  InsertFootNoteRunCore(AParagraphIndex, ALogPosition, TdxFootNoteNumberResultFormatting.Instance.Clone, ANoteIndex, FFootNoteRunInserter);
  Result := LastInsertedRunInfo.Run;
end;

function TdxPieceTable.InsertFootNoteRun(APos: TdxInputPosition; ANoteIndex: Integer): TdxTextRunBase;
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
  ARun: TdxFootNoteRun;
begin
  InsertFootNoteRun(APos.ParagraphIndex, APos.LogPosition, ANoteIndex);
  ALastInsertedRunInfo := LastInsertedRunInfo;
  ARun := TdxFootNoteRun(ALastInsertedRunInfo.Run);
  ARun.ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
  Result := ARun;
end;

procedure TdxPieceTable.InsertLayoutDependentTextRun(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AFormatting: TdxFieldResultFormatting);
begin
  TextBuffer.Append('#');
  FLayoutDependentTextInserter.TextLength := 1;
  TdxLayoutDependentTextInserter(FLayoutDependentTextInserter).FieldResultFormatting := AFormatting;
  InsertObjectCore(FLayoutDependentTextInserter, AParagraphIndex, ALogPosition);
end;

procedure TdxPieceTable.InsertParagraphCore(APos: TdxInputPosition);
var
  ARunIndex: TdxRunIndex;
begin
  Assert(APos.PieceTable = Self);
  ARunIndex := InsertParagraphCore(APos.ParagraphIndex, APos.LogPosition, False);
  InheritParagraphRunStyle(APos, TdxParagraphRun(Runs[ARunIndex]));
end;

procedure TdxPieceTable.InsertFieldSymbolResult(ALogPosition: TdxDocumentLogPosition; const ASymbol: Char);
var
  ACommand: TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand;
begin
  ACommand := TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand.Create(Self, ALogPosition, ASymbol);
  try
    ACommand.Execute;
  finally
    FreeAndNil(ACommand);
  end;
end;

procedure TdxPieceTable.InsertInsertFieldSymbolResultCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; const ASymbol: Char; AForceVisible: Boolean);
var
  AInserter: TdxFieldSymbolResultInserter;
begin
  TextBuffer.Append(ASymbol);
  AInserter := TdxFieldSymbolResultInserter.Create(Self);
  try
    AInserter.TextLength := 1;
    InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

procedure TdxPieceTable.InsertParagraphCoreNoInheritParagraphRunStyle(APos: TdxInputPosition);
begin
  Assert(APos.PieceTable = Self);
  InsertParagraphCore(APos.ParagraphIndex, APos.LogPosition, False);
  APos.LogPosition := APos.LogPosition + 1;
  APos.ParagraphIndex := APos.ParagraphIndex + 1;
end;

function TdxPieceTable.InsertParagraph(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxParagraph;
begin
  Result := TdxParagraph(inherited InsertParagraph(ALogPosition, AForceVisible));
end;

procedure TdxPieceTable.InsertParagraphs(ALogPosition: TdxDocumentLogPosition; ACount: Integer;
  AForceVisible: Boolean);
var
  I: Integer;
begin
  if ACount > 0 then
    InsertParagraph(ALogPosition, AForceVisible);
  for I := 0 to ACount - 2 do
    InsertParagraph(ALogPosition + I, AForceVisible);
end;

function TdxPieceTable.InsertSectionParagraphCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean): TdxRunIndex;
var
  AInserter: TdxSectionInserter;
begin
  TextBuffer.Append(TdxCharacters.SectionMark);
  AInserter := TdxSectionInserter.Create(Self);
  try
    Result := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

procedure TdxPieceTable.InsertSeparator(ALogPosition: TdxDocumentLogPosition);
begin
  InsertSeparator(ALogPosition, False);
end;

procedure TdxPieceTable.InsertSeparator(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableInsertSeparatorAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertSeparatorAtLogPositionCommand.Create(Self, ALogPosition, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertSectionParagraphCore(APos: TdxInputPosition);
var
  ARunIndex: TdxRunIndex;
begin
  Assert(APos.PieceTable = Self);
  ARunIndex := InsertSectionParagraphCore(APos.ParagraphIndex, APos.LogPosition, False);
  InheritParagraphRunStyle(APos, Runs[ARunIndex] as TdxParagraphRun);
end;

procedure TdxPieceTable.RemoveDocumentPermission(AStart, AEnd: TdxDocumentLogPosition;
  AInfo: TdxRangePermissionInfo);
var
  AItem: TdxRemoveRangePermissionHistoryItem;
begin
  if AEnd - AStart < 0 then
    Exit;

  DocumentModel.BeginUpdate;
  try
    AItem := TdxRemoveRangePermissionHistoryItem.Create(Self);
    AItem.Position := AStart;
    AItem.Length := AEnd - AStart;
    AItem.IndexToInsert := DocumentModel.Cache.RangePermissionInfoCache.GetItemIndex(AInfo);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxPieceTable.RemoveDocumentPermissionCore(AStart, AEnd: TdxDocumentLogPosition;
  ARangePermissionInfoIndex: Integer): TdxRunInfo;
var
  AMergedRanges: TdxRangePermissionCollectionEx;
  ARangePermission: TdxRangePermission;
begin
  AMergedRanges := ExtractMergedRanges(ARangePermissionInfoIndex);
  try
    if AMergedRanges.Count > 0 then
      Result := AMergedRanges[0].Interval.Clone
    else
      Result := nil;

    ARangePermission := CreateRangePermission(AStart, AEnd, ARangePermissionInfoIndex);
    AMergedRanges.Remove(ARangePermission);
    AppendMergedRanges(AMergedRanges);
  finally
    AMergedRanges.Free;
  end;
end;

procedure TdxPieceTable.RemoveNumberingFromParagraph(AParagraph: TdxSimpleParagraph);
var
  ALeftIndent: Integer;
  ATransaction: TdxHistoryTransaction;
begin
  Assert(AParagraph <> nil);
  if not AParagraph.IsInList then
    TdxRichEditExceptions.ThrowArgumentException('paragraph', AParagraph);
  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      if AParagraph.IsInNonStyleList then
        RemoveParagraphFromList(AParagraph.Index)
      else
      begin
        if AParagraph.GetOwnNumberingListIndex = NumberingListIndexListIndexNotSetted then
        begin
          ALeftIndent := AParagraph.LeftIndent;
          AParagraph.ParagraphProperties.LeftIndent := ALeftIndent;
        end;
        AddNumberingListToParagraph(AParagraph, NumberingListIndexNoNumberingList, 0);
      end;
    finally
      FreeAndNil(ATransaction);
    end;
    DocumentModel.InvalidateDocumentLayout;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.RemoveParagraphFromList(AParagraphIndex: TdxParagraphIndex);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxRemoveParagraphFromListHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxRemoveParagraphFromListHistoryItem.Create(Self);
    AItem.ParagraphIndex := AParagraphIndex;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    FreeAndNil(ATransaction);
  end;
end;

procedure TdxPieceTable.ApplyAutoCorrect(APosition: TdxDocumentLogPosition;
  ALength: Integer; const AText: string;
  const ARule: IdxSpellCheckerAutoCorrectCustomRule);
var
  ACommand: TdxPieceTableApplyAutoCorrectCommand;
begin
  ACommand := TdxPieceTableApplyAutoCorrectCommand.Create(Self, APosition, ALength, AText, ARule);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.ReplaceTextWithMultilineText(APosition: TdxDocumentLogPosition; ALength: Integer;
  const AText: string);
begin
  DocumentModel.BeginUpdate;
  try
    DeleteContent(APosition, ALength, False);
    InsertPlainText(APosition, AText);
    DocumentModel.Selection.SetStartCell(DocumentModel.Selection.Start);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.ReplaceTextWithPicture(APosition: TdxDocumentLogPosition; ALength: Integer;
  AImage: TdxOfficeImageReference);
begin
  DocumentModel.BeginUpdate;
  try
    DeleteContent(APosition, ALength, False);
    InsertInlinePicture(APosition, AImage);
    DocumentModel.Selection.SetStartCell(DocumentModel.Selection.Start);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.ResetCharacterStyle(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  AModifier: TdxRunCharacterStyleModifier;
begin
  AModifier := TdxRunCharacterStyleModifier.Create(-1);
  try
    AModifier.ModifyTextRun(ARun, ARunIndex);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.ResetParagraphs(AFrom, ATo: TdxParagraphIndex);
var
  AParagraphs: TdxParagraphCollection;
  I: Integer;
  AFromPosition: TdxDocumentModelPosition;
begin
  AParagraphs := Paragraphs;
  AFrom := Max(AFrom, 0);
  ATo := Min(ATo, AParagraphs.Count - 1);
  AFromPosition := TdxRichEditView.EnsurePositionVisibleWhenHiddenTextNotShown(DocumentModel, TdxDocumentModelPosition.FromParagraphStart(Self, AFrom));
  AFrom := AFromPosition.ParagraphIndex;
  for I := AFrom to ATo do
    AParagraphs[I].BoxCollection.InvalidateBoxes;
  TableCellsManager.ResetCachedTableLayoutInfo(AFrom, ATo);
  FCalculatorCache.Clear;
end;

procedure TdxPieceTable.ResetTableAllUse(ATable: TdxTable);
var
  ATableUseValue: Integer;
  AResetAllUse: TdxTableCellProcessorDelegate;
  AStart, AEnd: TdxRunIndex;
  AController: TdxTableConditionalFormattingController;
begin
  DocumentModel.BeginUpdate;
  try
    ATableUseValue := ATable.TableProperties.UseValue;
    ATableUseValue := ATableUseValue and (not TdxTablePropertiesOptions.MaskUsePreferredWidth);
    ATableUseValue := ATableUseValue and (not TdxTablePropertiesOptions.MaskUseTableLook);
    ATable.TableProperties.ResetUse(ATableUseValue);
    AResetAllUse :=
      procedure (ACell: TdxTableCell)
      var
        ACellUseValue: Integer;
      begin
        ACellUseValue := ACell.Properties.UseValue;
        ACellUseValue := ACellUseValue and (not TdxTableCellPropertiesOptions.MaskUsePreferredWidth);
        ACell.Properties.ResetUse(ACellUseValue);
      end;
    ATable.ForEachCell(AResetAllUse);
    AController.Init(ATable);
    AController.ResetCachedProperties(0);

    AStart := Paragraphs[ATable.FirstRow.FirstCell.StartParagraphIndex].FirstRunIndex;
    AEnd := Paragraphs[ATable.LastRow.LastCell.EndParagraphIndex].LastRunIndex;
    ApplyChangesCore(TdxTableChangeActionCalculator.CalculateChangeActions(TdxTableChangeType.TableStyle), AStart, AEnd);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.SetBackColor(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  ABackColor: TdxAlphaColor);
var
  AModifier: TdxRunBackColorModifier;
begin
  AModifier := TdxRunBackColorModifier.Create(ABackColor);
  try
    ApplyCharacterFormatting(ALogPositionStart, ALength, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.SetFont(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AFont: TFont);
var
  AModifier: TdxRunFontModifier;
begin
  AModifier := TdxRunFontModifier.Create(AFont);
  try
    ApplyCharacterFormatting(ALogPositionStart, ALength, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.SetForeColor(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AForeColor: TdxAlphaColor);
var
  AModifier: TdxRunForeColorModifier;
begin
  AModifier := TdxRunForeColorModifier.Create(AForeColor);
  try
    ApplyCharacterFormatting(ALogPositionStart, ALength, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxPieceTable.SetShowHiddenText(AValue: Boolean);
begin
  inherited SetShowHiddenText(AValue);
  FreeAndNil(FNavigationVisibleTextFilter);
  FNavigationVisibleTextFilter := CreateNavigationVisibleTextFilter(AValue);
end;

function TdxPieceTable.ShouldApplyStyleToParagraphs(ALogPositionStart: TdxDocumentLogPosition;
  ALength: Integer): Boolean;
var
  ARunInfo: TdxRunInfo;
  AStartRun, AEndRun: TdxTextRunBase;
  AIsStartRunIndexMatchStartOfParagraph, AIsEndPositionMatchEndOfParagraph: Boolean;
begin
  Result := False;
  ARunInfo := FindRunInfo(ALogPositionStart, ALength);
  try
    AStartRun := Runs[ARunInfo.Start.RunIndex];
    AEndRun := Runs[ARunInfo.&End.RunIndex];
    if AStartRun.Paragraph <> AEndRun.Paragraph then
      Exit(True);
    AIsStartRunIndexMatchStartOfParagraph := (ARunInfo.Start.RunIndex = 0) or
      (Runs[ARunInfo.Start.RunIndex - 1] is TdxParagraphRun);
    if AIsStartRunIndexMatchStartOfParagraph then
    begin
      if ARunInfo.Start.RunStartLogPosition = ALogPositionStart then
      begin
        if AEndRun is TdxParagraphRun then
          Exit(True);
        AIsEndPositionMatchEndOfParagraph := (ARunInfo.&End.RunEndLogPosition = ARunInfo.&End.LogPosition) and
          (Runs[ARunInfo.&End.RunIndex + 1] is TdxParagraphRun);
        if AIsEndPositionMatchEndOfParagraph then
          Exit(True);
      end;
    end;
  finally
    ARunInfo.Free;
  end;
end;

function TdxPieceTable.ShouldDeleteTable(ATable: TdxTable; ARunInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ATableStartParagraphIndex, ATableEndParagraphIndex: TdxParagraphIndex;
  ATableStartRunIndex, ATableEndRunIndex, ARunStartIndex, ARunEndIndex: TdxRunIndex;
begin
  ATableStartParagraphIndex := ATable.FirstRow.FirstCell.StartParagraphIndex;
  ATableEndParagraphIndex := ATable.LastRow.LastCell.EndParagraphIndex;
  ATableStartRunIndex := Paragraphs[ATableStartParagraphIndex].FirstRunIndex;
  ATableEndRunIndex := Paragraphs[ATableEndParagraphIndex].LastRunIndex;
  ARunStartIndex := ARunInfo.Start.RunIndex;
  ARunEndIndex := ARunInfo.&End.RunIndex;
  if (ARunStartIndex < ATableStartRunIndex) and ((ATableEndRunIndex < ARunEndIndex) or ADocumentLastParagraphSelected) then
    Exit(True);
  if (ARunStartIndex = ATableStartRunIndex) and ((ATableEndRunIndex < ARunEndIndex) or ADocumentLastParagraphSelected) then
    Exit(True);
  if (ARunStartIndex < ATableStartRunIndex) and (ATableEndRunIndex = ARunEndIndex) then
    Exit(True);
  Result := False;
end;

procedure TdxPieceTable.SplitTable(ATableIndex, ARowIndex: Integer; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableSplitTableCommand;
begin
  ACommand := TdxPieceTableSplitTableCommand.Create(Self, ATableIndex, ARowIndex, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.SplitTableCellsHorizontally(ACell: TdxTableCell; APartsCount: Integer;
  AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableSplitTableCellsHorizontally;
begin
  ACommand := TdxPieceTableSplitTableCellsHorizontally.Create(Self, ACell, APartsCount, AForceVisible, AServer);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.SplitTableCellsVertically(APatternCell: TdxTableCell; APartsCount,
  AColumnsCount: Integer; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableSplitTableCellsVertically;
begin
  ACommand := TdxPieceTableSplitTableCellsVertically.Create(Self, APatternCell, APartsCount, AColumnsCount, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.SplitTableCellsHorizontally(ACell: TdxTableCell; APartsCount: Integer;
  const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  SplitTableCellsHorizontally(ACell, APartsCount, False, AServer);
end;

procedure TdxPieceTable.MultipleSplitTextRun(APositions: TdxIntegerList);
begin
  NotImplemented;
end;

procedure TdxPieceTable.SplitTextRunsByCharset;
var
  I: Integer;
  ARun: TdxTextRunBase;
  ACachedResult: TdxRunMergedCharacterPropertiesCachedResult;
begin
  if not DocumentModel.UseFontSubstitution then
    Exit;

  ACachedResult := TdxRunMergedCharacterPropertiesCachedResult.Create;
  try
    for I := Runs.Count - 1 downto 0 do
    begin
      ARun := Runs[I];
      if ARun is TdxTextRun then
      begin
        ARun.EnsureMergedCharacterFormattingCacheIndexCalculated(ACachedResult);
        SplitTextRunByCharsetCore(I);
      end;
    end;
  finally
    ACachedResult.Free;
  end;
end;

procedure TdxPieceTable.ProcessFieldsRecursive(AParent: TdxField; const AValidateField: TdxValidateFieldFunction);
var
  AHasDeletedFields: Boolean;
  AFields: TdxFieldList;
  ACount, I: Integer;
begin
  Assert(Assigned(AValidateField));
  BookmarksClear;
  repeat
    AHasDeletedFields := False;
    AFields := CollectFieldsToProcess(AParent);
    try
      ACount := AFields.Count;
      for I := 0 to ACount - 1 do
      begin
        if AFields[I].DisableUpdate then
          Continue;
        if not AValidateField(AFields[I]) then
        begin
          RemoveFieldWithCode(AFields[I]);
          AHasDeletedFields := True;
        end
        else
          ProcessFieldsRecursive(AFields[I], AValidateField);
      end;
    finally
      AFields.Free;
    end;
  until not AHasDeletedFields;
end;

procedure TdxPieceTable.ToggleFieldCodesFromCommandOrApi(AField: TdxField);
begin
  ToggleFieldCodes(AField);
end;

procedure TdxPieceTable.UpdateTableOfContents(AOperationType: TdxUpdateFieldOperationType);
begin
end;

procedure TdxPieceTable.ValidateTableIndent(ATable: TdxTable);
var
  AStartParagraph, AParagraph: TdxParagraph;
  ALeftIndent, AFirstLineIndent: Integer;
  AFirstLineIndentType: TdxParagraphFirstLineIndent;
  I, ATableEndParagraphIndex, ATableStartParagraphIndex: TdxParagraphIndex;
begin
  ATableStartParagraphIndex := ATable.StartParagraphIndex;
  AStartParagraph := Paragraphs[ATableStartParagraphIndex];
  ALeftIndent := AStartParagraph.LeftIndent;
  AFirstLineIndent := AStartParagraph.FirstLineIndent;
  AFirstLineIndentType := AStartParagraph.FirstLineIndentType;
  if (ALeftIndent = 0) and (AFirstLineIndent = 0) and (AFirstLineIndentType = TdxParagraphFirstLineIndent.None) then
    Exit;
  if ALeftIndent <> 0 then
  begin
    ATable.TableProperties.TableIndent.&Type := TdxWidthUnitType.ModelUnits;
    ATable.TableProperties.TableIndent.Value := ALeftIndent;
  end;
  ATableEndParagraphIndex := ATable.EndParagraphIndex;
  for I := ATableStartParagraphIndex to ATableEndParagraphIndex do
  begin
    AParagraph := Paragraphs[I];
    AParagraph.LeftIndent := 0;
    AParagraph.FirstLineIndent := 0;
    AParagraph.FirstLineIndentType := TdxParagraphFirstLineIndent.None;
  end;
end;

procedure TdxPieceTable.WriteParagraphLeftIndent(AParagraph: TdxParagraph);
var
  ANumberingList: TdxNumberingList;
  ALevel: TdxAbstractListLevel;
begin
  ANumberingList := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex];
  ALevel := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex].Levels[AParagraph.GetListLevelIndex];
  if TdxNumberingListHelper.GetListType(ANumberingList) = TdxNumberingType.MultiLevel then
  begin
    if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
      AParagraph.LeftIndent := AParagraph.LeftIndent - AParagraph.FirstLineIndent
    else
      AParagraph.LeftIndent := AParagraph.LeftIndent - ALevel.ListLevelProperties.OriginalLeftIndent;
  end
  else
    AParagraph.LeftIndent := AParagraph.LeftIndent - ALevel.ListLevelProperties.OriginalLeftIndent;
  AParagraph.FirstLineIndentType := TdxParagraphFirstLineIndent.None;
  AParagraph.FirstLineIndent := 0;
  if AParagraph.LeftIndent < 0 then
    AParagraph.LeftIndent := 0;
end;

function TdxPieceTable.ObtainMergedParagraphsProperties(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxParagraphPropertyModifierBase): TdxMergedParagraphProperties;
var
  I: Integer;
  ARunInfo: TdxRunInfo;
  AValue, AOldValue, AParagraphValue: TdxMergedParagraphProperties;
  ATypedModifier: TdxMergedParagraphPropertyModifier<TdxMergedParagraphProperties>;
begin
  ATypedModifier := TdxMergedParagraphPropertyModifier<TdxMergedParagraphProperties>(AModifier);
  ARunInfo := FindRunInfo(ALogPositionStart, ALength);
  try
    AValue := ATypedModifier.GetParagraphPropertyValue(Paragraphs[ARunInfo.Start.ParagraphIndex]);
    for I := ARunInfo.Start.ParagraphIndex + 1 to ARunInfo.&End.ParagraphIndex do
    begin
      AParagraphValue := ATypedModifier.GetParagraphPropertyValue(Paragraphs[I]);
      try
        AOldValue := AValue;
        AValue := ATypedModifier.Merge(AOldValue, AParagraphValue);
        AOldValue.Free;
      finally
        AParagraphValue.Free;
      end;
    end;
  finally
    ARunInfo.Free;
  end;
  Result := AValue;
end;

function TdxPieceTable.ObtainMergedParagraphsTabFormattingInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer;
  AModifier: TdxParagraphPropertyModifierBase): TdxTabFormattingInfo;
var
  I: Integer;
  ARunInfo: TdxRunInfo;
  AValue, AOldValue, AParagraphValue: TdxTabFormattingInfo;
  ATypedModifier: TdxMergedParagraphPropertyModifier<TdxTabFormattingInfo>;
begin
  ATypedModifier := TdxMergedParagraphPropertyModifier<TdxTabFormattingInfo>(AModifier);
  ARunInfo := FindRunInfo(ALogPositionStart, ALength);
  try
    AValue := ATypedModifier.GetParagraphPropertyValue(Paragraphs[ARunInfo.Start.ParagraphIndex]);
    for I := ARunInfo.Start.ParagraphIndex + 1 to ARunInfo.&End.ParagraphIndex do
    begin
      AParagraphValue := ATypedModifier.GetParagraphPropertyValue(Paragraphs[I]);
      try
        AOldValue := AValue;
        AValue := ATypedModifier.Merge(AOldValue, AParagraphValue);
        AOldValue.Free;
      finally
        AParagraphValue.Free;
      end;
    end;
  finally
    ARunInfo.Free;
  end;
  Result := AValue;
end;

function TdxPieceTable.GetLanguageInfo(const AStart, AEnd: TdxDocumentModelPosition): TdxNullableValue<TdxLangInfo>;
begin
        Exit(TdxNullableValue<TdxLangInfo>.Null);
end;

function TdxPieceTable.ShouldCheckWord(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
var
  I: TdxRunIndex;
begin
  if not Runs[AStart.RunIndex].NoProof then
    Exit(True);
  if AStart.RunIndex <> AEnd.RunIndex then
  begin
    for I := AStart.RunIndex + 1 to AEnd.RunIndex - 1 do
    begin
      if not Runs[I].NoProof then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TdxPieceTable.InsertEndNoteRun(APos: TdxInputPosition; ANoteIndex: Integer): TdxTextRunBase;
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  InsertEndNoteRun(APos.ParagraphIndex, APos.LogPosition, ANoteIndex);
  ALastInsertedRunInfo := LastInsertedRunInfo;
  Result := ALastInsertedRunInfo.Run as TdxEndNoteRun;
  TdxEndNoteRun(Result).ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

procedure TdxPieceTable.InsertColumnToTheLeft(APatternCell: TdxTableCell; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableInsertColumnToTheLeft;
begin
  ACommand := TdxPieceTableInsertColumnToTheLeft.Create(Self, APatternCell, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertColumnToTheRight(APatternCell: TdxTableCell; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableInsertColumnToTheRight;
begin
  ACommand := TdxPieceTableInsertColumnToTheRight.Create(Self, APatternCell, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTable.InsertCustomMark(ACustomMarkIndex: Integer; ACustomMark: TdxCustomMark): TdxCustomMark;
begin
  Result := NotImplemented;
end;

procedure TdxPieceTable.InsertDocumentModelContent(ADocumentModel: TdxDocumentModel;
  APos: TdxDocumentLogPosition; ASuppressParentFieldsUpdate, ASuppressFieldsUpdate,
  ACopyBetweenInternalModels: Boolean);
var
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(Self, ADocumentModel, APos, False);
  try
    ACommand.CopyBetweenInternalModels := ACopyBetweenInternalModels;
    ACommand.SuppressParentFieldsUpdate := ASuppressParentFieldsUpdate;
    ACommand.SuppressFieldsUpdate := ASuppressFieldsUpdate;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertDocumentModelContent(ADocumentModel: TdxDocumentModel;
  APos: TdxDocumentLogPosition);
begin
  InsertDocumentModelContent(ADocumentModel, APos, False, False, False);
end;

function TdxPieceTable.InsertEndNoteRun(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; ANoteIndex: Integer): TdxTextRunBase;
begin
  InsertFootNoteRunCore(AParagraphIndex, ALogPosition, TdxEndNoteNumberResultFormatting.Instance.Clone, ANoteIndex, FEndNoteRunInserter);
  Result := LastInsertedRunInfo.Run;
end;

procedure TdxPieceTable.InsertFootNoteRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
  AFormatting: TdxFieldResultFormatting; ANoteIndex: Integer; AInserter: TdxObjectInserter{TdxFootNoteRunInserterBase<T>});
var
  ATypedInserter: TdxFootNoteRunInserter;
begin
  ATypedInserter := TdxFootNoteRunInserter(AInserter);
  Assert((AInserter is TdxFootNoteRunInserter) or (AInserter is TdxEndNoteRunInserter));
  TextBuffer.Append('#');
  ATypedInserter.TextLength := 1;
  ATypedInserter.FieldResultFormatting := AFormatting;
  ATypedInserter.NoteIndex := ANoteIndex;
  InsertObjectCore(ATypedInserter, AParagraphIndex, ALogPosition);
end;

function TdxPieceTable.IsNumberingListLevelIndexValid(AParagraph: TdxSimpleParagraph;
  ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer): Boolean;
var
  AActualNumberingListIndex: TdxNumberingListIndex;
begin
  Result := AListLevelIndex >= 0;
  if Result then
  begin
    AActualNumberingListIndex := ANumberingListIndex;
    if ANumberingListIndex < NumberingListIndexMinValue then
      AActualNumberingListIndex := AParagraph.ParagraphStyle.GetNumberingListIndex;
    if AActualNumberingListIndex < NumberingListIndexMinValue then
      Result := ANumberingListIndex = NumberingListIndexNoNumberingList
    else
      Result := AListLevelIndex < DocumentModel.NumberingLists[AActualNumberingListIndex].Levels.Count;
  end;
end;

function TdxPieceTable.IsPermissionGranted(ARangePermission: TdxRangePermission): Boolean;
var
  AOptions: TdxAuthenticationOptions;
begin
  AOptions := DocumentModel.AuthenticationOptions;
  if ((AOptions.UserName <> '') and SameText(ARangePermission.UserName, AOptions.UserName)) or
     ((AOptions.EMail <> '') and SameText(ARangePermission.UserName, AOptions.EMail)) then
    Exit(True);
  if SameText(ARangePermission.Group, 'Everyone') or
     ((AOptions.Group <> '') and SameText(ARangePermission.Group, AOptions.Group)) then
    Exit(True);

  Result := False;
end;

function TdxPieceTable.IsTocField(AField: TdxField): Boolean;
begin
  Result := GetFieldToken(AField).Value = TdxTocField.FieldType;
end;

function TdxPieceTable.IsValidNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex): Boolean;
begin
  Result := (ANumberingListIndex >= NumberingListIndexMinValue) or
    (ANumberingListIndex = NumberingListIndexNoNumberingList);
end;

procedure TdxPieceTable.JoinTables(ATopTable, ABottomTable: TdxTable);
var
  ACommand: TdxPieceTableJoinTablesCommand;
begin
  if ATopTable <> ABottomTable then
  begin
    ACommand := TdxPieceTableJoinTablesCommand.Create(Self, ATopTable, ABottomTable);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

procedure TdxPieceTable.MergeCells(ASelectedCellsCollectionInstance: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
var
  ANormalizedStartCell: TdxTableCell;
  ASelectedCellsCollection: TdxSelectedCellsCollection absolute ASelectedCellsCollectionInstance;
begin
  TdxSelectedCellsCollection.AddReference(ASelectedCellsCollection);
  try
    MergeCellsHorizontally(ASelectedCellsCollection);
    ANormalizedStartCell := ASelectedCellsCollection.NormalizedFirst.NormalizedStartCell;
    MergeTableCellsVertically(ANormalizedStartCell, TdxSelectedCellsCollection(ASelectedCellsCollection).RowsCount);
  finally
    TdxSelectedCellsCollection.Release(ASelectedCellsCollection);
  end;
end;

procedure TdxPieceTable.MergeCellsHorizontally(ASelectedCellsInstance: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
var
  ATopRowIndex, ABottomRowIndex, ADirection, ACount, I, AColumnSpan: Integer;
  ACellsInterval: TdxSelectedCellsIntervalInRow;
  AStartCell: TdxTableCell;
  ASelectedCells: TdxSelectedCellsCollection absolute ASelectedCellsInstance;
begin
  ATopRowIndex := ASelectedCells.GetNormalizedTopRowIndex;
  ABottomRowIndex := ASelectedCells.GetNormalizedBottomRowIndex;
  if ABottomRowIndex > ATopRowIndex then
    ADirection := 1
  else
    ADirection := -1;
  ACount := Abs(ABottomRowIndex - ATopRowIndex);
  I := 0;
  while I <= ACount do
  begin
    ACellsInterval := ASelectedCells[ATopRowIndex + I * ADirection];
    AStartCell := ACellsInterval.NormalizedStartCell;
    AColumnSpan := ACellsInterval.GetNormalizedColumnSpan;
    if AStartCell.VerticalMerging = TdxMergingState.Restart then
      Inc(I, MergeCellsHorizontallyCore(AStartCell, AColumnSpan));
    MergeTableCellsHorizontally(AStartCell, AColumnSpan);
    Inc(I);
  end;
end;

function TdxPieceTable.MergeCellsHorizontallyCore(AStartCell: TdxTableCell; AMergedCellsColumnSpan: Integer): Integer;
var
  ACells: TdxTableCellList;
  AColumnIndex, ACellsCount, I: Integer;
begin
  AColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(AStartCell, False);
  ACells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(AStartCell, AColumnIndex, False);
  try
    ACellsCount := ACells.Count;
    for I := 1 to ACellsCount - 1 do
      MergeTableCellsHorizontally(ACells[I], AMergedCellsColumnSpan);
    Result := ACellsCount - 1;
  finally
    ACells.Free;
  end;
end;

procedure TdxPieceTable.MergeTableCellsHorizontally(ACell: TdxTableCell; ACount: Integer);
var
  ACommand: TdxPieceTableMergeTableCellsHorizontallyCommand;
begin
  ACommand := TdxPieceTableMergeTableCellsHorizontallyCommand.Create(Self, ACell, ACount);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.MergeTableCellsVertically(ACell: TdxTableCell; ACount: Integer);
var
  ACommand: TdxPieceTableMergeTableCellsVerticallyCommand;
begin
  ACommand := TdxPieceTableMergeTableCellsVerticallyCommand.Create(Self, ACell, ACount);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.MovePositionToNext(var APos: TdxDocumentModelPosition; AOffset: Integer);
var
  I: Integer;
begin
  for I := 0 to AOffset - 1 do
    TdxDocumentModelPosition.MoveForwardCore(APos);
end;

procedure TdxPieceTable.MoveTableRowToOtherTable(ATargetTable: TdxTable; ARow: TdxTableRow);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxMoveTableRowToOtherTableHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxMoveTableRowToOtherTableHistoryItem.Create(Self, ATargetTable, ARow);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTable.ObtainRangePermissionsMatchSelection: TdxRangePermissionCollection;
var
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
begin
  Result := TdxOwnedRangePermissionMergedCollection.Create(Self);
  AItems := DocumentModel.Selection.Items;
  ACount := AItems.Count;
  for I := 0 to ACount - 1 do
    ObtainRangePermissionsMatchSelectionItem(Result, AItems[I]);
end;

procedure TdxPieceTable.ObtainRangePermissionsMatchSelectionItem(ATarget: TdxRangePermissionCollection;
  AItem: TdxSelectionItem);
var
  ACount, I: Integer;
  APermission: TdxRangePermission;
begin
  ACount := RangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    APermission := RangePermissions[I];
    if (APermission.Start = AItem.NormalizedStart) and (APermission.&End = AItem.NormalizedEnd) then
      ATarget.Add(APermission.Clone);
  end;
end;

function TdxPieceTable.ObtainRangePermissionsWithSelectionInside: TdxRangePermissionCollection;
var
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
begin
  Result := TdxRangePermissionCollection.Create(Self);
  AItems := DocumentModel.Selection.Items;
  ACount := AItems.Count;
  for I := 0 to ACount - 1 do
    ObtainRangePermissionsWithSelectionItemInside(Result, AItems[I]);
end;

procedure TdxPieceTable.ObtainRangePermissionsWithSelectionItemInside(ATarget: TdxRangePermissionCollection;
  AItem: TdxSelectionItem);
var
  ACount, I: Integer;
  APermission: TdxRangePermission;
begin
  ACount := RangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    APermission := RangePermissions[I];
    if (AItem.NormalizedStart >= APermission.Start) and (AItem.NormalizedEnd <= APermission.&End) then
      ATarget.Add(APermission.Clone);
  end;
end;

function TdxPieceTable.ShouldForceUpdateIntervals: Boolean;
begin
  Result := FShouldForceUpdateIntervals;
end;

procedure TdxPieceTable.UpdateIntervals;
begin
  Bookmarks.UpdateIntervals;
  RangePermissions.UpdateIntervals;
  FShouldForceUpdateIntervals := False;
end;

procedure TdxPieceTable.OnBeginSetContent;
begin
  FShouldForceUpdateIntervals := True;
end;

procedure TdxPieceTable.OnEndSetContent;
begin
  SplitTextRunsByCharset;
  UpdateIntervals;
  SpellCheckerManager.Initialize;
end;

procedure TdxPieceTable.DoFieldInserted(AFieldIndex: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(TableCellsManager, Self, AFieldIndex);
end;

procedure TdxPieceTable.DoFieldRemoved(AFieldIndex: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(TableCellsManager, Self, AFieldIndex);
end;

procedure TdxPieceTable.DoParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(TableCellsManager, Self, ASectionIndex,
    AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.ParagraphInsertedCore(ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(Bookmarks, Self, ASectionIndex, AParagraphIndex,
    ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(RangePermissions, Self, ASectionIndex, AParagraphIndex,
    ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(SpellCheckerManager, Self, ASectionIndex,
    AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.DoParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(TableCellsManager, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.ParagraphMergedCore(ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(Bookmarks, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(RangePermissions, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(SpellCheckerManager, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.DoParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(TableCellsManager, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.ParagraphRemovedCore(ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(Bookmarks, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(RangePermissions, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(SpellCheckerManager, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxPieceTable.DoRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(TableCellsManager, Self, AParagraphIndex, ANewRunIndex,
    ALength, AHistoryNotificationId);
end;

procedure TdxPieceTable.RunInsertedCore(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(Bookmarks, Self, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(RangePermissions, Self, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(SpellCheckerManager, Self, AParagraphIndex, ANewRunIndex,
    ALength, AHistoryNotificationId);
end;

procedure TdxPieceTable.DoRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset: Integer; ATailRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(TableCellsManager, Self, AParagraphIndex, AJoinedRunIndex,
    ASplitOffset, ATailRunLength);
end;

procedure TdxPieceTable.RunJoinedCore(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset, ATailRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(Bookmarks, Self, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(RangePermissions, Self, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(SpellCheckerManager, Self, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxPieceTable.DoRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(TableCellsManager, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxPieceTable.RunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(Bookmarks, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(RangePermissions, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(SpellCheckerManager, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxPieceTable.DoRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(TableCellsManager, Self, AParagraphIndex, ARunIndex,
    ALength, AHistoryNotificationId);
end;

procedure TdxPieceTable.RunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(Bookmarks, Self, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(RangePermissions, Self, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(SpellCheckerManager, Self, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxPieceTable.DoRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(TableCellsManager, Self, AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxPieceTable.RunSplitCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(Bookmarks, Self, AParagraphIndex, ARunIndex, ASplitOffset);
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(RangePermissions, Self, AParagraphIndex, ARunIndex, ASplitOffset);
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(SpellCheckerManager, Self, AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxPieceTable.DoRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(TableCellsManager, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxPieceTable.RunUnmergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(Bookmarks, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(RangePermissions, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(SpellCheckerManager, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxPieceTable.PerformTextRunSplit(ARunIndices: TdxSortedRunIndexCollection);
var
  ACount: Integer;
  I: Integer;
begin
  if not DocumentModel.UseFontSubstitution then
    Exit;
  ACount := ARunIndices.Count;
  while (ACount > 0) and (ARunIndices[ACount - 1] > Runs.Count) do
    Dec(ACount);
  for I := ACount - 1 downto 0 do
    SplitTextRunByCharset(ARunIndices[I]);
end;

procedure TdxPieceTable.PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat);
begin
end;

procedure TdxPieceTable.InsertTextCoreNoResetMergeNoApplyFormatting(APos: TdxInputPosition; const AText: string;
  AForceVisible: Boolean);
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  Assert(APos.PieceTable = Self);
  InsertTextCoreWithoutSplit(APos.ParagraphIndex, APos.LogPosition, AText, AForceVisible);
  ALastInsertedRunInfo := LastInsertedRunInfo;
  if not DocumentModel.DeferredChanges.IsSetContentMode then
    SplitTextRunByCharset(ALastInsertedRunInfo.RunIndex);
  APos.LogPosition := APos.LogPosition + Length(AText);
end;

function TdxPieceTable.InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer): TdxTable;
begin
  Result := InsertTable(ALogPosition, ARowCount, ACellCount, TdxTableAutoFitBehaviorType.AutoFitToContents,
    MinInt, MinInt, False);
end;

function TdxPieceTable.InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount, AFixedColumnWidths: Integer): TdxTable;
begin
  Result := InsertTable(ALogPosition, ARowCount, ACellCount, TdxTableAutoFitBehaviorType.FixedColumnWidth, AFixedColumnWidths,
    MinInt, False);
end;

function TdxPieceTable.InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer;
  AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths: Integer): TdxTable;
begin
  Result := InsertTable(ALogPosition, ARowCount, ACellCount, AAutoFitBehavior, AFixedColumnWidths, MinInt, False);
end;

function TdxPieceTable.InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer;
  AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths, AOuterColumnWidth: Integer; AForceVisible: Boolean): TdxTable;
begin
  Result := InsertTable(ALogPosition, ARowCount, ACellCount, AAutoFitBehavior, AFixedColumnWidths, MinInt, AForceVisible, False);
end;

function TdxPieceTable.InsertTable(ALogPosition: TdxDocumentLogPosition; ARowCount, ACellCount: Integer;
  AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths, AOuterColumnWidth: Integer;
  AForceVisible: Boolean; AMatchHorizontalTableIndentsToTextEdge: Boolean): TdxTable;
var
  ATargetParagraphIndex: TdxParagraphIndex;
  ATable: TdxTable;
  ATransaction: TdxHistoryTransaction;
  AParagraph: TdxParagraph;
  ANewParagraphCount: Integer;
begin
  if ARowCount <= 0 then
    raise EArgumentNilException.Create('ARowCount');
  if ACellCount <= 0 then
    raise EArgumentNilException.Create('ACellCount');
  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ATargetParagraphIndex := FindParagraphIndex(ALogPosition);
      AParagraph := Paragraphs[ATargetParagraphIndex];
      if AParagraph.LogPosition <> ALogPosition then
      begin
        InsertParagraph(ALogPosition, AForceVisible);
        Inc(ALogPosition);
        Inc(ATargetParagraphIndex);
      end;
      ANewParagraphCount := ARowCount * ACellCount;
      InsertParagraphs(ALogPosition, ANewParagraphCount, AForceVisible);
      ConvertParagraphsToTable(ATargetParagraphIndex, ARowCount, ACellCount);
      ATable := Paragraphs[ATargetParagraphIndex].GetCell.Table;
      ATable.InitializeColumnWidths(AAutoFitBehavior, AFixedColumnWidths, AOuterColumnWidth, AMatchHorizontalTableIndentsToTextEdge);
      ValidateTableIndent(ATable);
    finally
      ATransaction.Free;
    end;
    DocumentModel.InvalidateDocumentLayout;
    ATable.TableLook := [TdxTableLookType.ApplyFirstRow, TdxTableLookType.ApplyFirstColumn, TdxTableLookType.DoNotApplyColumnBanding];
    Result := ATable;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTable.InsertTableCellToTheLeft(APatternCell: TdxTableCell; AForceVisible: Boolean;
  const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableInsertTableCellToTheLeft;
begin
  ACommand := TdxPieceTableInsertTableCellToTheLeft.Create(Self, APatternCell, AForceVisible, AServer);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertTableCellToTheRight(APatternCell: TdxTableCell; AForceVisible: Boolean;
  const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableInsertTableCellToTheRight;
begin
  ACommand := TdxPieceTableInsertTableCellToTheRight.Create(Self, APatternCell, AForceVisible, AServer);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertTableCellWithShiftToTheDown(APatternCell: TdxTableCell; AForceVisible: Boolean;
  const AServer: IdxInnerRichEditDocumentServerOwner);
var
  ACommand: TdxPieceTableInsertTableCellWithShiftToTheDownCommand;
begin
  ACommand := TdxPieceTableInsertTableCellWithShiftToTheDownCommand.Create(Self, APatternCell, AForceVisible, AServer);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertTableRowAbove(APatternRow: TdxTableRow; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableInsertTableRowAboveCommand;
begin
  ACommand := TdxPieceTableInsertTableRowAboveCommand.Create(Self, APatternRow, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.InsertTableRowBelow(APatternRow: TdxTableRow; AForceVisible: Boolean);
var
  ACommand: TdxPieceTableInsertTableRowBelowCommand;
begin
  ACommand := TdxPieceTableInsertTableRowBelowCommand.Create(Self, APatternRow, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTable.DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields: Boolean);
begin
  DeleteContent(ALogPosition, ALength, ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields, False);
end;

procedure TdxPieceTable.DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields,
  ALeaveFieldIfResultIsRemoved: Boolean);
begin
  DeleteContent(ALogPosition, ALength, ADocumentLastParagraphSelected, AAllowPartiallyDeletingField,
    AForceRemoveInnerFields, ALeaveFieldIfResultIsRemoved, False);
end;

procedure TdxPieceTable.DeleteContent(ALogPosition: TdxDocumentLogPosition; ALength: Integer;
  ADocumentLastParagraphSelected, AAllowPartiallyDeletingField, AForceRemoveInnerFields, ALeaveFieldIfResultIsRemoved,
  ABackspacePressed: Boolean);
var
  ACommand: TdxPieceTableDeleteTextCommand;
begin
  ACommand := TdxPieceTableDeleteTextCommand.Create(Self, ALogPosition, ALength);
  try
    ACommand.AllowPartiallyDeletingField := AAllowPartiallyDeletingField;
    ACommand.DocumentLastParagraphSelected := ADocumentLastParagraphSelected;
    ACommand.ForceRemoveInnerFields := AForceRemoveInnerFields;
    ACommand.LeaveFieldIfResultIsRemoved := ALeaveFieldIfResultIsRemoved;
    ACommand.BackspacePressed := ABackspacePressed;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxPieceTableList }

function TdxPieceTableList.Contains(AItem: TdxPieceTable): Boolean;
begin
  Result := IndexOf(AItem) >= 0;
end;

function TdxPieceTableList.GetItem(Index: Integer): TdxPieceTable;
begin
  Result := TdxPieceTable(inherited Items[Index]);
end;

{ TdxParagraphList }

function TdxParagraphList.First: TdxParagraph;
begin
  Result := TdxParagraph(inherited First);
end;

function TdxParagraphList.Last: TdxParagraph;
begin
  Result := TdxParagraph(inherited Last);
end;

function TdxParagraphList.GetItem(Index: Integer): TdxParagraph;
begin
  Result := TdxParagraph(inherited Items[Index]);
end;

procedure TdxParagraphList.SetItem(Index: Integer; const Value: TdxParagraph);
begin
  inherited Items[Index] := Value;
end;

{ TdxParagraphCollection }

function TdxParagraphCollection.First: TdxParagraph;
begin
  Result := TdxParagraph(inherited First);
end;

function TdxParagraphCollection.Last: TdxParagraph;
begin
  Result := TdxParagraph(inherited Last);
end;

function TdxParagraphCollection.GetItem(Index: Integer): TdxParagraph;
begin
  Result := TdxParagraph(inherited Items[Index]);
end;

{ TdxParagraph }

function TdxParagraph.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxParagraph.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxParagraph;
var
  ATargetPieceTable: TdxPieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  AManager: TdxDocumentModelCopyManager absolute ACopyManager;
begin
  Assert(DocumentModel = AManager.SourceModel);
  ATargetPieceTable := AManager.TargetPieceTable;
  ATargetPosition := AManager.TargetPosition;
  if ATargetPieceTable.DocumentModel.DocumentCapabilities.ParagraphsAllowed then
    ATargetPieceTable.InsertParagraph(ATargetPosition.LogPosition);
  Result := ATargetPieceTable.Paragraphs[ATargetPosition.ParagraphIndex];
  CopyFrom(AManager.TargetModel, Result);
end;

procedure TdxParagraph.CopyFrom(ADocumentModel: TdxDocumentModel; AResultParagraph: TdxParagraph);
var
  AOptions: TdxDocumentCapabilitiesOptions;
begin
  AOptions := AResultParagraph.DocumentModel.DocumentCapabilities;
  if AOptions.ParagraphFormattingAllowed then
    AResultParagraph.ParagraphProperties.CopyFrom(ParagraphProperties.Info);
  if AOptions.ParagraphTabsAllowed then
    AResultParagraph.Tabs.CopyFrom(Tabs.Info);
  if AOptions.ParagraphStyleAllowed then
    AResultParagraph.ParagraphStyleIndex := ParagraphStyle.Copy(ADocumentModel);

  if IsInNonStyleList or (GetOwnNumberingListIndex = NumberingListIndexNoNumberingList) then
    CopyNumberingListProperties(AResultParagraph)
  else
    if (AResultParagraph.PieceTable.PrecalculatedNumberingListTexts <> nil) and
        (GetOwnNumberingListIndex = NumberingListIndexListIndexNotSetted) and (ParagraphStyle.GetNumberingListIndex >= NumberingListIndexMinValue) then
      AResultParagraph.PieceTable.PrecalculatedNumberingListTexts.Add(AResultParagraph, GetNumberingListText);

  if HasFrameProperties then
  begin
    AResultParagraph.CreateFrameProperties;
    AResultParagraph.FrameProperties.CopyFrom(FrameProperties.Info);
  end;
end;

procedure TdxParagraph.CopyNumberingListProperties(ATargetParagraph: TdxParagraph);
var
  ANumbering: TdxNumberingOptions;
  ATargetNumberingListIndex: TdxNumberingListIndex;
begin
  ANumbering := ATargetParagraph.DocumentModel.DocumentCapabilities.Numbering;
  if not ANumbering.BulletedAllowed or not ANumbering.SimpleAllowed or not ANumbering.MultiLevelAllowed then
    Exit;
  if ATargetParagraph.IsInNonStyleList then
    ATargetParagraph.PieceTable.RemoveNumberingFromParagraph(ATargetParagraph);

  if GetOwnNumberingListIndex = NumberingListIndexNoNumberingList then
  begin
    ATargetParagraph.PieceTable.AddNumberingListToParagraph(ATargetParagraph, GetOwnNumberingListIndex, GetListLevelIndex);
    Exit;
  end;
  ATargetNumberingListIndex := DocumentModel.GetNumberingListIndex(ATargetParagraph.DocumentModel, GetOwnNumberingListIndex, NumberingListIndexMaxValue);
  ATargetParagraph.PieceTable.AddNumberingListToParagraph(ATargetParagraph, ATargetNumberingListIndex, GetListLevelIndex);
  if ATargetParagraph.PieceTable.PrecalculatedNumberingListTexts <> nil then
    ATargetParagraph.PieceTable.PrecalculatedNumberingListTexts.Add(ATargetParagraph, GetNumberingListText);
end;

function TdxParagraph.CreateBoxCollection: TdxSimpleParagraphBoxCollection;
begin
  Result := TdxParagraphBoxCollection.Create;
end;

function TdxParagraph.EqualsMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean;
begin
  Result := inherited EqualsMergedCharacterCachedResult(ACachedResult) and (ACachedResult.TableCell = GetCell);
end;

procedure TdxParagraph.DoUseMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult);
begin
  inherited DoUseMergedCharacterCachedResult(ACachedResult);
  ACachedResult.TableCell := GetCell;
end;

procedure TdxParagraph.CreateNumberingList(ATarget: TdxDocumentModel; ANumberingListId: Integer);
begin
NotImplemented;
end;

function TdxParagraph.GetAbstractNumberingList: TdxAbstractNumberingList;
var
  AListIndex: TdxNumberingListIndex;
begin
  AListIndex := GetNumberingListIndex;
  if AListIndex < NumberingListIndexMinValue then
    Result := nil
  else
    Result := DocumentModel.NumberingLists[AListIndex].AbstractNumberingList;
end;

function TdxParagraph.GetAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
var
  AListIndex: TdxNumberingListIndex;
begin
  AListIndex := GetNumberingListIndex;
  if AListIndex < NumberingListIndexMinValue then
    Result := -1
  else
    Result := DocumentModel.NumberingLists[AListIndex].AbstractNumberingListIndex;
end;

function TdxParagraph.GetCell: TdxTableCell;
begin
  Result := PieceTable.TableCellsManager.GetCell(Self);
end;

function TdxParagraph.GetContextualSpacingAfter: Integer;
begin
  if Index + 1 >= PieceTable.Paragraphs.Count then
    Result := SpacingAfter
  else
    if ContextualSpacing and (PieceTable.Paragraphs[Index + 1].ParagraphStyleIndex = ParagraphStyleIndex) then
      Result := 0
    else
      Result := SpacingAfter;
end;

function TdxParagraph.GetContextualSpacingBefore: Integer;
begin
  if Index <= 0 then
    Result := SpacingBefore
  else
    if ContextualSpacing and (PieceTable.Paragraphs[Index - 1].ParagraphStyleIndex = ParagraphStyleIndex) then
      Result := 0
    else
      Result := SpacingBefore;
end;

function TdxParagraph.GetListLevelParagraphProperties: TdxParagraphProperties;
var
  ANumberingList: TdxNumberingList;
  ALevel: IdxListLevel;
begin
  if not IsInList then
    Result := nil
  else
  begin
    ANumberingList := DocumentModel.NumberingLists[GetNumberingListIndex];
    Supports(ANumberingList.Levels[GetListLevelIndex], IdxListLevel, ALevel);
    Result := ALevel.ParagraphProperties;
  end;
end;

function TdxParagraph.GetListLevelSeparator: string;
var
  ALevels: TdxListLevelCollection;
  ASeparator: Char;
begin
  ALevels := DocumentModel.NumberingLists[GetNumberingListIndex].Levels;
  ASeparator := ALevels[GetListLevelIndex].ListLevelProperties.Separator;
  if ASeparator <> #0 then
    Result := ASeparator
  else
    Result := '';
end;

function TdxParagraph.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := GetMergedCharacterProperties(False, nil);
end;

function TdxParagraph.GetMergedCharacterProperties(AUseSpecialTableStyle: Boolean; ATableStyle: TdxTableStyle): TdxMergedCharacterProperties;
var
  ACell: TdxTableCell;
  AProperties: TdxMergedCharacterProperties;
begin
  Result := ParagraphStyle.GetMergedCharacterProperties;
  ACell := GetCell;
  if ACell <> nil then
  begin
    AProperties := ACell.GetMergedCharacterProperties;
    try
      Result.Merge(AProperties);
    finally
      FreeAndNil(AProperties);
    end;
    if not AUseSpecialTableStyle then
      AProperties := ACell.Table.GetMergedCharacterProperties(ACell)
    else
      if ATableStyle <> nil then
        AProperties := ACell.Table.GetMergedCharacterProperties(ACell, ATableStyle);

    if AProperties <> nil then
    try
      Result.Merge(AProperties);
    finally
      AProperties.Free;
    end;
  end;
  Result.Merge(DocumentModel.DefaultCharacterProperties);
end;

function TdxParagraph.GetNumberingListText: string;
var
  ACounters: TIntegerDynArray;
begin
  ACounters := PieceTable.GetRangeListCounters(Self);
  try
    Result := GetNumberingListText(ACounters);
  finally
    System.SetLength(ACounters, 0);
  end;
end;

function TdxParagraph.GetNumberingListText(const ACounters: TIntegerDynArray): string;
var
  ALevels: TdxListLevelCollection;
  AListLevel: TdxAbstractListLevel;
  ALevelProperties: TdxListLevelProperties;
  AFormatString: string;
  ALanguageId: Word;
begin
  if PieceTable.PrecalculatedNumberingListTexts <> nil then
    if PieceTable.PrecalculatedNumberingListTexts.ContainsKey(Self) then
      Exit(PieceTable.PrecalculatedNumberingListTexts[Self]);
  ALevels := DocumentModel.NumberingLists[GetNumberingListIndex].Levels;
  AListLevel := ALevels[GetListLevelIndex];
  ALevelProperties := AListLevel.ListLevelProperties;
  AFormatString := ALevelProperties.DisplayFormatString;
  ALanguageId := TdxLanguageId.English;
  Result := Format(AFormatString, ACounters, ALevels, ALanguageId, ALevelProperties.ConvertPreviousLevelNumberingToDecimal);
  if AListLevel.BulletLevel then
    Result := System.Copy(Result, 1, TdxListLevel.BulletLevelDisplayFormatStringLength);
end;

function TdxParagraph.GetNumerationCharacterProperties: TdxMergedCharacterProperties;
var
  AListLevel: IdxListLevel;
  AProperties: TdxMergedCharacterProperties;
begin
  AListLevel := DocumentModel.NumberingLists[GetNumberingListIndex].Levels[GetListLevelIndex];
  Result := TdxMergedCharacterProperties.Create(AListLevel.CharacterProperties);
  AProperties := PieceTable.Runs[LastRunIndex].GetMergedCharacterProperties;
  try
    AProperties.Options.UseFontUnderlineType := False;
    if AListLevel.BulletLevel then
    begin
      AProperties.Options.UseFontBold := False;
      AProperties.Options.UseFontItalic := False;
    end;
    Result.Merge(AProperties);
  finally
    AProperties.Free;
  end;
end;

function TdxParagraph.GetNumerationFontCacheIndex: Integer;
var
  AInfo: TdxCharacterFormattingInfo;
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties;
  try
    AInfo := ACharacterProperties.Info;
    Result := DocumentModel.FontCache.CalcFontIndex(AInfo.FontName, AInfo.DoubleFontSize,
      SmallFontStylesMap[AInfo.FontBold, AInfo.FontItalic], AInfo.Script);
  finally
    FreeAndNil(ACharacterProperties);
  end;
end;

function TdxParagraph.GetNumerationFontInfo: TdxFontInfo;
var
  AFontCacheIndex: Integer;
begin
  AFontCacheIndex := GetNumerationFontCacheIndex;
  Result := DocumentModel.FontCache[AFontCacheIndex];
end;

class function TdxParagraph.Format(const AFormatString: string; AArgs: TIntegerDynArray;
  const ALevels: TdxListLevelCollection; ALanguageId: Word = TdxLanguageId.English;
  ADisplayAllLevelsUsingArabicNumerals: Boolean = False): string;

  procedure ConvertToConstArray(const AStringArray: TArray<string>; var AConstArray: TArray<TVarRec>);
  var
    I: Integer;
  begin
    System.SetLength(AConstArray, System.Length(AStringArray));
    for I := 0 to System.Length(AStringArray) - 1 do
    begin
      AConstArray[I].VType := vtUnicodeString;
      string(AConstArray[I].VUnicodeString) := AStringArray[I];
    end;
  end;

  procedure ClearConstArray(var AConstArray: TArray<TVarRec>);
  var
    I: Integer;
  begin
    for I := 0 to System.Length(AConstArray) - 1 do
      string(AConstArray[I].VUnicodeString) := '';
    System.SetLength(AConstArray, 0);
  end;

var
  I: Integer;
  AStringArray: TArray<string>;
  AFormat: TdxNumberingFormat;
  AObjArgs: TArray<TVarRec>;
  AConverter: TdxOrdinalBasedNumberConverter;
begin
  System.SetLength(AStringArray, System.Length(AArgs));
  for I := 0 to System.Length(AArgs) - 1 do
  begin
    AFormat := ALevels[I].ListLevelProperties.Format;
    if ADisplayAllLevelsUsingArabicNumerals and
      ((AFormat <> TdxNumberingFormat.Decimal) and (AFormat <> TdxNumberingFormat.DecimalZero)) then
      AFormat := TdxNumberingFormat.Decimal;
    AConverter := TdxOrdinalBasedNumberConverter.CreateConverter(AFormat, ALanguageId);
    try
      AStringArray[I] := AConverter.ConvertNumber(AArgs[I]);
    finally
      AConverter.Free;
    end;
  end;
  try
    ConvertToConstArray(AStringArray, AObjArgs);
    try
      Result := SysUtils.Format(AFormatString, AObjArgs);
    finally
      ClearConstArray(AObjArgs);
    end;
  except
    if System.Length(AStringArray) > 0 then
      Result := AStringArray[0]
    else
      Result := '';
  end;
end;

function TdxParagraph.GetOwnListLevelParagraphProperties: TdxParagraphProperties;
begin
  if not IsInList or not IsInNonStyleList then
    Result := nil
  else
    Result := GetListLevelParagraphProperties;
end;

function TdxParagraph.GetOwnListLevelParagraphPropertiesIndex: Integer;
var
  AOwnListLevelParagraphProperties: TdxParagraphProperties;
begin
  Result := -1;
  AOwnListLevelParagraphProperties := GetOwnListLevelParagraphProperties;
  if AOwnListLevelParagraphProperties <> nil then
    Result := AOwnListLevelParagraphProperties.Index;
end;

function TdxParagraph.GetParentMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := GetParentMergedWithTableStyleParagraphProperties(False, nil);
end;

function TdxParagraph.GetParentMergedWithTableStyleParagraphProperties(AUseSpecialTableStyle: Boolean;
  ATableStyle: TdxTableStyle): TdxMergedParagraphProperties;
var
  AListLevelParagraphProperties: TdxParagraphProperties;
  AMergedProperties: TdxMergedParagraphProperties;
  AOwnListLevelParagraphProperties: TdxParagraphProperties;
  ACell: TdxTableCell;
begin
  AOwnListLevelParagraphProperties := GetOwnListLevelParagraphProperties;
  if AOwnListLevelParagraphProperties <> nil then
  begin
    Assert(IsInNonStyleList);
    Result := TdxMergedParagraphProperties.Create(AOwnListLevelParagraphProperties);
    AMergedProperties := ParagraphStyle.GetMergedParagraphProperties;
    try
      Result.Merge(AMergedProperties);
    finally
      AMergedProperties.Free;
    end;
  end
  else
  begin
    Result := ParagraphStyle.GetMergedParagraphProperties;
    AListLevelParagraphProperties := GetListLevelParagraphProperties;
    if AListLevelParagraphProperties <> nil then
      Result.Merge(AListLevelParagraphProperties);
  end;
  ACell := GetCell;
  if ACell <> nil then
  begin
    AMergedProperties := ACell.GetMergedParagraphProperties;
    try
      Result.Merge(AMergedProperties);
    finally
      FreeAndNil(AMergedProperties);
    end;

    if not AUseSpecialTableStyle then
      AMergedProperties := ACell.Table.GetMergedParagraphProperties(ACell)
    else if ATableStyle <> nil then
      AMergedProperties := ACell.Table.GetMergedParagraphProperties(ATableStyle, ACell);

    if AMergedProperties <> nil then
    try
      Result.Merge(AMergedProperties);
    finally
      AMergedProperties.Free;
    end;
  end;
  Result.Merge(DocumentModel.DefaultParagraphProperties);
end;

function TdxParagraph.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxParagraph.GetTableStyleCharacterPropertiesIndex: Integer;
var
  ACell: TdxTableCell;
begin
  ACell := GetCell;
  if ACell <> nil then
    Result := ACell.Table.TableStyle.CharacterProperties.Index
  else
    Result := -1;
end;

function TdxParagraph.TryUseParentMergedCachedResult(ACachedResult: TdxParagraphMergedParagraphPropertiesCachedResult; ATableStyleIndex: Integer): Boolean;
begin
  Result := TryUseParentMergedCachedResultCore(ACachedResult, GetOwnListLevelParagraphPropertiesIndex, ATableStyleIndex);
end;

function TdxParagraph.TryUseParentMergedCachedResultCore(ACachedResult: TdxParagraphMergedParagraphPropertiesCachedResult;
  AOwnListLevelParagraphPropertiesIndex, ATableStyleParagraphPropertiesIndex: Integer): Boolean;
var
  AShouldUseExistingResult: Boolean;
begin
  AShouldUseExistingResult :=
    (ACachedResult.ParagraphPropertiesIndex < 0) and
    (ACachedResult.ParagraphStyleIndex = ParagraphStyleIndex) and
    (ACachedResult.OwnListLevelParagraphPropertiesIndex = AOwnListLevelParagraphPropertiesIndex) and
    (ACachedResult.TableStyleParagraphPropertiesIndex = ATableStyleParagraphPropertiesIndex);
  if AShouldUseExistingResult then
    Exit(True);
  ACachedResult.ParagraphPropertiesIndex := -1;
  ACachedResult.ParagraphStyleIndex := ParagraphStyleIndex;
  ACachedResult.OwnListLevelParagraphPropertiesIndex := AOwnListLevelParagraphPropertiesIndex;
  ACachedResult.TableStyleParagraphPropertiesIndex := ATableStyleParagraphPropertiesIndex;
  Result := False;
end;

function TdxParagraph.GetTableStyleIndex: Integer;
var
  ACell: TdxTableCell;
begin
  ACell := GetCell;
  if ACell <> nil then
    Result := ACell.Table.StyleIndex
  else
    Result := -1;
end;

function TdxParagraph.IsInList: Boolean;
var
  ANumListIndex: TdxNumberingListIndex;
  ANumList: TdxNumberingList;
begin
  ANumListIndex := GetNumberingListIndex;
  if ANumListIndex < 0 then
    Exit(False);
  ANumList := DocumentModel.NumberingLists[ANumListIndex];
  Result := ANumList.Levels[GetListLevelIndex].ListLevelProperties.Format <> TdxRichEditNumberingFormat.None; // T285071
end;

function TdxParagraph.GetCellCore: TdxCustomTableCell;
begin
  Result := GetCell;
end;

function TdxParagraph.GetListLevelIndex: Integer;
begin
  if GetOwnNumberingListIndex >= NumberingListIndexMinValue then
    Result := ListLevelIndex
  else
    Result := ParagraphStyle.GetListLevelIndex;
end;

function TdxParagraph.GetNumberingListIndex: TdxNumberingListIndex;
begin
  Result := inherited GetNumberingListIndex;
  if (Result >= NumberingListIndexMinValue) or (Result = NumberingListIndexNoNumberingList) then
    Exit
  else
    Result := ParagraphStyle.GetNumberingListIndex;
end;

function TdxParagraph.IsInCell: Boolean;
begin
  Result := PieceTable.TableCellsManager.IsInCell(Self);
end;

function TdxParagraph.GetTabs: TdxTabFormattingInfo;
var
  ATabs, AStyleTabs: TdxTabFormattingInfo;
begin
  ATabs := Tabs.GetTabs;
  try
    AStyleTabs := GetParentTabs;
    try
      Result := TdxTabFormattingInfo.Merge(ATabs, AStyleTabs);
    finally
      AStyleTabs.Free;
    end;
  finally
    ATabs.Free;
  end;
end;

function TdxParagraph.GetParentTabs: TdxTabFormattingInfo;
var
  AStyleTabs, ANumberingTabs: TdxTabFormattingInfo;
begin
  ANumberingTabs := GetListLevelTabs;
  try
    if (ANumberingTabs <> nil) and (ANumberingTabs.Count > 0) then
    begin
      AStyleTabs := ParagraphStyle.GetTabs;
      try
        Result := TdxTabFormattingInfo.Merge(ANumberingTabs, AStyleTabs)
      finally
        AStyleTabs.Free;
      end;
    end
    else
      Result := ParagraphStyle.GetTabs;
  finally
    ANumberingTabs.Free;
  end;
end;

function TdxParagraph.GetListLevelTabs: TdxTabFormattingInfo;
var
  ANumberingList: TdxNumberingList;
  ALevel: IdxListLevel;
begin
  if not IsInList then
    Exit(nil);
  ANumberingList := DocumentModel.NumberingLists[GetNumberingListIndex];
  ALevel := ANumberingList.Levels[GetListLevelIndex];
  Result := ALevel.Tabs.Info.Clone;
end;

procedure TdxParagraph.ResetListLevelIndex;
begin
  SetListLevelIndex(-1);
end;

procedure TdxParagraph.ResetNumberingListIndex(AIndex: TdxNumberingListIndex);
begin
  SetNumberingListIndex(AIndex);
end;

procedure TdxParagraph.SetNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex);
begin
  OwnNumberingListIndex := ANumberingListIndex;
  ResetCachedIndices(TdxResetFormattingCacheType.All);
  if Parent <> nil then
    PieceTable.ApplyChangesCore(TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(
      TdxParagraphFormattingChangeType.NumberingListIndex), FirstRunIndex, MaxInt);
end;

procedure TdxParagraph.SetListLevelIndex(AListLevelIndex: Integer);
begin
  OwnListLevelIndex := AListLevelIndex;
  if DocumentModel.IsUpdateLocked and (FirstRunIndex >= 0) then
      PieceTable.ApplyChangesCore(TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(
        TdxParagraphFormattingChangeType.NumberingListIndex), FirstRunIndex, MaxInt);
end;

function TdxParagraph.ShouldExportNumbering: Boolean;
var
  AOptions: TdxNumberingOptions;
  AListIndex: TdxNumberingListIndex;
  ANumberingType: TdxNumberingType;
  AStyleNumbering: TdxNumberingListIndex;
begin
  AOptions := DocumentModel.DocumentCapabilities.Numbering;
  AListIndex := GetNumberingListIndex;
  if AListIndex = NumberingListIndexNoNumberingList then
  begin
    AStyleNumbering := ParagraphStyle.GetNumberingListIndex;
    if AStyleNumbering >= NumberingListIndexMinValue then
      ANumberingType := TdxNumberingListHelper.GetListType(DocumentModel.NumberingLists[AStyleNumbering])
    else
      Exit(False);
  end
  else
    ANumberingType := TdxNumberingListHelper.GetListType(DocumentModel.NumberingLists[AListIndex]);
  case ANumberingType of
    TdxNumberingType.Bullet:
      Result := AOptions.BulletedAllowed;
    TdxNumberingType.Simple:
      Result := AOptions.SimpleAllowed;
    else
      Result := AOptions.MultiLevelAllowed;
  end;
end;

{ TdxMainContentType }

procedure TdxMainContentType.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
var
  AItem: TdxFixLastParagraphOfLastSectionHistoryItem;
begin
  AItem := TdxFixLastParagraphOfLastSectionHistoryItem.Create(PieceTable);
  AItem.OriginalParagraphCount := AOriginalParagraphCount;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxMainContentType.SetPageCount(APageCount: Integer);
begin
  TdxDocumentModel(DocumentModel).ExtendedDocumentProperties.SetPages(APageCount);
end;

{ TdxHyperlinkInfoEventArgs }

constructor TdxHyperlinkInfoEventArgs.Create(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FFieldIndex := AFieldIndex;
end;

{ TdxDocumentFormatsHelper }

class function TdxDocumentFormatsHelper.ShouldInsertNumbering(AModel: TdxDocumentModel): Boolean;
var
  AOptions: TdxNumberingOptions;
begin
  AOptions := AModel.DocumentCapabilities.Numbering;
  Result := AOptions.BulletedAllowed or AOptions.SimpleAllowed or ShouldInsertMultiLevelNumbering(AModel);
end;

class function TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.Numbering.MultiLevelAllowed;
end;

class function TdxDocumentFormatsHelper.ShouldInsertBulletedNumbering(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.Numbering.BulletedAllowed;
end;

class function TdxDocumentFormatsHelper.ShouldExportSectionColumns(AColumns: TdxSectionColumns; ADocumentModel: TdxDocumentModel): Boolean;
var
  ADefaultColumns: TdxColumnsInfo;
begin
  ADefaultColumns := ADocumentModel.Cache.ColumnsInfoCache.DefaultItem;
  Result :=
    (AColumns.EqualWidthColumns <> ADefaultColumns.EqualWidthColumns) or
    (AColumns.ColumnCount <> ADefaultColumns.ColumnCount) or
    (AColumns.Space <> ADefaultColumns.Space) or
    (AColumns.DrawVerticalSeparator <> ADefaultColumns.DrawVerticalSeparator);
end;

class function TdxDocumentFormatsHelper.ShouldInsertHyperlink(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.HyperlinksAllowed;
end;

class function TdxDocumentFormatsHelper.ShouldInsertHyperlinks(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.HyperlinksAllowed;
end;

class function TdxDocumentFormatsHelper.ShouldInsertPicture(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.InlinePicturesAllowed;
end;

class function TdxDocumentFormatsHelper.ShouldInsertSimpleNumbering(AModel: TdxDocumentModel): Boolean;
begin
  Result := AModel.DocumentCapabilities.Numbering.SimpleAllowed;
end;

class function TdxDocumentFormatsHelper.NeedReplaceSimpleToBulletNumbering(AModel: TdxDocumentModel): Boolean;
begin
  Result := not ShouldInsertSimpleNumbering(AModel) and ShouldInsertBulletedNumbering(AModel);
end;

class function TdxDocumentFormatsHelper.NeedReplaceBulletedLevelsToDecimal(AModel: TdxDocumentModel): Boolean;
begin
  Result := not ShouldInsertBulletedNumbering(AModel) and ShouldInsertSimpleNumbering(AModel);
end;

{ TdxExtendedDocumentProperties }

procedure TdxExtendedDocumentProperties.SetPages(APages: Integer);
begin
  FPages := APages;
end;

{ TdxDocumentModelDeferredChanges }

constructor TdxDocumentModelDeferredChanges.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FChangeStart := TdxDocumentModelPosition.FromParagraphEnd(ADocumentModel.MainPart,
    ADocumentModel.MainPart.Paragraphs.GetLastCore.Index);
  FChangeEnd := TdxDocumentModelPosition.FromParagraphStart(ADocumentModel.MainPart, 0);
  FStartAnchor := TdxDocumentModelPositionAnchor.Create(@ChangeStart);
  FEndAnchor := TdxDocumentModelPositionAnchor.Create(@ChangeEnd);
  ResetSelectionChanged;
end;

destructor TdxDocumentModelDeferredChanges.Destroy;
begin
  FreeAndNil(FStartAnchor);
  FreeAndNil(FEndAnchor);
  FreeAndNil(FAdditionalChangedPieceTables);
  inherited Destroy;
end;

procedure TdxDocumentModelDeferredChanges.ApplyChanges(APieceTable: TdxPieceTable; AActions: TdxDocumentModelChangeActions;
  AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if APieceTable.DocumentModel.SuppressPerformLayout then
    AActions := AActions - [
      TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout];

  FChangeActions := FChangeActions + AActions;
  if (TdxDocumentModelChangeAction.SplitRunByCharset in AActions) and not IsSetContentMode then
    GetRunIndicesForSplit(APieceTable).Add(AStartRunIndex);
  if ShouldUpdatePositions(AActions) then
  begin
    Assert(AStartRunIndex > dxRunIndexDontCare);
    Assert(AEndRunIndex > dxRunIndexDontCare);
    if (AStartRunIndex < ChangeStart.RunIndex) or ((AStartRunIndex = ChangeStart.RunIndex)
      and (ChangeStart.RunStartLogPosition <> ChangeStart.LogPosition)) then
      TdxDocumentModelPosition.SetRunStart(FChangeStart, AStartRunIndex);
    if AEndRunIndex >= ChangeEnd.RunIndex then
    begin
      AEndRunIndex := Min(AEndRunIndex, ChangeEnd.PieceTable.Runs.Count - 1);
      TdxDocumentModelPosition.SetRunEnd(FChangeEnd, AEndRunIndex);
    end;
  end;
  if not APieceTable.IsMain then
    AddAdditionalChangedPieceTable(APieceTable);
end;

procedure TdxDocumentModelDeferredChanges.AddAdditionalChangedPieceTable(AAdditionalChangedPieceTable: TdxPieceTable);
begin
  if FAdditionalChangedPieceTables = nil then
  begin
    FAdditionalChangedPieceTables := TdxPieceTableList.Create;
    FAdditionalChangedPieceTables.Add(AAdditionalChangedPieceTable);
    Exit;
  end;
  if FAdditionalChangedPieceTables.Contains(AAdditionalChangedPieceTable) then
    Exit;
  FAdditionalChangedPieceTables.Add(AAdditionalChangedPieceTable);
end;

function TdxDocumentModelDeferredChanges.ShouldUpdatePositions(const AActions: TdxDocumentModelChangeActions): Boolean;
begin
  Result :=
    [TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout] * AActions <> [];
end;

procedure TdxDocumentModelDeferredChanges.SetChangeParagraphStart(AParagraphIndex: TdxParagraphIndex);
begin
  TdxDocumentModelPosition.SetParagraphStart(FChangeStart, AParagraphIndex);
end;

procedure TdxDocumentModelDeferredChanges.SetChangeParagraphEnd(AParagraphIndex: TdxParagraphIndex);
begin
  TdxDocumentModelPosition.SetParagraphEnd(@FChangeEnd, AParagraphIndex);
end;

procedure TdxDocumentModelDeferredChanges.ResetSelectionChanged;
begin
  FOriginalSelectionUsePreviousBoxBounds := DocumentModel.Selection.UsePreviousBoxBounds;
  inherited ResetSelectionChanged;
  FSelectionChanged := False;
end;

procedure TdxDocumentModelDeferredChanges.RegisterSelectionChanged;
begin
  FSelectionChanged := True;
end;

procedure TdxDocumentModelDeferredChanges.OnParagraphInserted(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(GetRunIndicesForSplit(APieceTable),
      APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
      AActualParagraphIndex, AHistoryNotificationId);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(StartAnchor, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(EndAnchor, APieceTable, ASectionIndex,
      AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnParagraphRemoved(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(GetRunIndicesForSplit(APieceTable),
      APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(StartAnchor,
      APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(EndAnchor, APieceTable,
      ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnParagraphMerged(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(GetRunIndicesForSplit(APieceTable),
      APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(StartAnchor,
      APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(EndAnchor, APieceTable,
      ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnRunInserted(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(GetRunIndicesForSplit(APieceTable),
      APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(StartAnchor, APieceTable,
      AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(EndAnchor, APieceTable,
      AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(GetRunIndicesForSplit(APieceTable),
      APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(StartAnchor, APieceTable,
      AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(EndAnchor, APieceTable,
      AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentModelDeferredChanges.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentModelDeferredChanges.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(GetRunIndicesForSplit(APieceTable), APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(StartAnchor, APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
    TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(EndAnchor, APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(GetRunIndicesForSplit(APieceTable), APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(StartAnchor, APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(EndAnchor, APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(GetRunIndicesForSplit(APieceTable), APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(StartAnchor, APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(EndAnchor, APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnRunUnmerged(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(GetRunIndicesForSplit(APieceTable), APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  if APieceTable.IsMain then
  begin
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(StartAnchor, APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
    TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(EndAnchor, APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  end;
end;

procedure TdxDocumentModelDeferredChanges.OnFieldRemoved(APieceTable: TdxCustomPieceTable;
  AFieldIndex: Integer);
begin
end;

procedure TdxDocumentModelDeferredChanges.OnFieldInserted(APieceTable: TdxCustomPieceTable;
  AFieldIndex: Integer);
begin
end;

function TdxDocumentModelDeferredChanges.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDocumentModelDeferredChanges.GetSelectionChanged: Boolean;
begin
  Result := FSelectionChanged or
    (FOriginalSelectionUsePreviousBoxBounds <> DocumentModel.Selection.UsePreviousBoxBounds);
end;

{ TdxSelectionItems }

function TdxSelectionItems.GetRange(AIndex, ACount: Integer): TdxSelectionItemList;
begin
  Result := TdxSelectionItemList.Create(False);
  Result.Capacity := ACount;
  while ACount > 0 do
  begin
    Result.Add(Items[AIndex]);
    Inc(AIndex);
    Dec(ACount);
  end;
end;

{ TdxSelection }

constructor TdxSelection.Create(APieceTable: TdxCustomPieceTable);
var
  ASelectionItem: TdxSelectionItem;
begin
  inherited Create(APieceTable);
  FItems := TdxSelectionItems.Create;
  ASelectionItem := InitialSelection(APieceTable);
  ASelectionItem.Changed.Add(OnSelectionChanged);
  ASelectionItem.IsSelectionInTable := False;
  ASelectionItem.Generation := 0;
  FItems.Add(ASelectionItem);
  SelectedCells := TdxSelectedCellsCollection.Create;
end;

destructor TdxSelection.Destroy;
begin
  SelectedCells := nil;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxSelection.GetStart: TdxDocumentLogPosition;
begin
  Result := ActiveSelection.Start;
end;

procedure TdxSelection.SetStart(const AValue: TdxDocumentLogPosition);
begin
  ActiveSelection.Start := AValue;
  TryAndMergeSelectionStart(AValue);
end;

function TdxSelection.GetEnd: TdxDocumentLogPosition;
begin
  Result := ActiveSelection.&End;
end;

procedure TdxSelection.SetEnd(const AValue: TdxDocumentLogPosition);
begin
  ActiveSelection.&End := AValue;
  TryAndMergeSelectionEnd(AValue);
end;

function TdxSelection.GetInterval: TdxRunInfo;
begin
  Result := ActiveSelection.Interval;
end;

function TdxSelection.GetNormalizedStart: TdxDocumentLogPosition;
begin
  Result := Interval.NormalizedStart.LogPosition;
end;

function TdxSelection.GetNormalizedEnd: TdxDocumentLogPosition;
begin
  Result := Interval.NormalizedEnd.LogPosition;
end;

function TdxSelection.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxSelection.GetActiveSelection: TdxSelectionItem;
begin
  Result := FItems[FItems.Count - 1];
end;

function TdxSelection.GetFirst: TdxSelectionItem;
begin
  Result := FItems[0];
end;

function TdxSelection.GetLength: Integer;
begin
  Result := ActiveSelection.Length;
end;

function TdxSelection.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxSelection.GetVirtualEnd: TdxDocumentLogPosition;
begin
  Result := ActiveSelection.VirtualEnd;
end;

function TdxSelection.GetUsePreviousBoxBounds: Boolean;
begin
  Result := ActiveSelection.UsePreviousBoxBounds;
end;

procedure TdxSelection.SetUsePreviousBoxBounds(const AValue: Boolean);
begin
  if ActiveSelection.UsePreviousBoxBounds = AValue then
    Exit;
  ActiveSelection.UsePreviousBoxBounds := AValue;
end;

function TdxSelection.GetNormalizedVirtualEnd: TdxDocumentLogPosition;
begin
  Result := NormalizedEnd;
  if Length <> 0 then
    Dec(Result);
end;

function TdxSelection.GetIsMultiSelection: Boolean;
begin
  Result := Items.Count > 1;
end;

function TdxSelection.GetIsSelectionChanged: Boolean;
var
  ACount, I: Integer;
begin
  ACount := Items.Count;
  for I := 0 to ACount - 1 do
    if Items[I].IsChanged then
      Exit(True);
  Result := False;
end;

procedure TdxSelection.SetIsSelectionChanged(const AValue: Boolean);
var
  ACount, I: Integer;
begin
  ACount := Items.Count;
  for I := 0 to ACount - 1 do
    Items[I].IsChanged := AValue;
end;

procedure TdxSelection.SetTableSelectionStucture(AValue: TdxSelectedTableStructureBase);
begin
  if FTableSelectionStucture <> AValue then
  begin
    TdxSelectedTableStructureBase.Release(FTableSelectionStucture);
    FTableSelectionStucture := AValue;
    TdxSelectedTableStructureBase.AddReference(FTableSelectionStucture);
  end;
end;

procedure TdxSelection.SetInterval(AStart, AEnd: TdxDocumentLogPosition);
begin
  ActiveSelection.Start := AStart;
  ActiveSelection.&End := AEnd;
  TryMergeByActiveSelection;
end;

function TdxSelection.InitialSelection(APieceTable: TdxCustomPieceTable): TdxSelectionItem;
begin
  Result := TdxSelectionItem.Create(APieceTable);
end;

procedure TdxSelection.BeginUpdate;
begin
  ActiveSelection.BeginUpdate;
  inherited BeginUpdate;
end;

procedure TdxSelection.EndUpdate;
begin
  ActiveSelection.EndUpdate;
  inherited EndUpdate;
end;

procedure TdxSelection.CancelUpdate;
begin
  ActiveSelection.CancelUpdate;
  BatchUpdateHelper.CancelUpdate;
end;

procedure TdxSelection.RaiseChanged;
begin
  if not Changed.Empty then
    Changed.Invoke(Self, nil);
end;

procedure TdxSelection.TryMergeByActiveSelection;
begin
  TryAndMergeSelectionStart(ActiveSelection.Start);
  TryAndMergeSelectionEnd(ActiveSelection.&End);
end;

procedure TdxSelection.Unselect(ARange: TdxSelectionItem);
var
  I: Integer;
  AItem: TdxSelectionItem;
  ANewSelection: TdxSelectionItem;
begin
  BeginUpdate;
  try
    for I := Items.Count - 1 downto 0 do
    begin
      AItem := Items[I];
      if (AItem.NormalizedStart >= ARange.NormalizedStart) and (AItem.NormalizedEnd <= ARange.NormalizedEnd) then
      begin
        Delete(I);
        Continue;
      end;

      if (AItem.NormalizedStart < ARange.NormalizedStart) and (AItem.NormalizedEnd > ARange.NormalizedEnd) then
      begin
        ANewSelection := TdxSelectionItem.Create(PieceTable);
        ANewSelection.Start := ARange.NormalizedEnd;
        ANewSelection.&End := AItem.NormalizedEnd;
        AddSelection(ANewSelection);
        AItem.Start := AItem.NormalizedStart;
        AItem.&End := ARange.NormalizedStart;
        Continue;
      end;

      if (AItem.NormalizedStart < ARange.NormalizedStart) and (AItem.NormalizedEnd > ARange.NormalizedStart) then
      begin
        AItem.Start := AItem.NormalizedStart;
        AItem.&End := ARange.NormalizedStart;
        Continue;
      end;

      if (AItem.NormalizedStart >= ARange.NormalizedStart) and (AItem.NormalizedEnd > ARange.NormalizedStart) then
      begin
        AItem.&End := AItem.NormalizedEnd;
        AItem.Start := ARange.NormalizedEnd;
        Continue;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSelection.TryAndMergeSelectionEnd(AValue: TdxDocumentLogPosition);
var
  I: Integer;
  ACurrentItem: TdxSelectionItem;
begin
  for I := FItems.Count - 2 downto 0 do
  begin
    ACurrentItem := Items[I];
    if (AValue > ACurrentItem.NormalizedStart) and (AValue < ACurrentItem.NormalizedEnd) then
      FItems.Remove(ACurrentItem);
  end;
end;

procedure TdxSelection.TryAndMergeSelectionStart(AValue: TdxDocumentLogPosition);
var
  I: Integer;
  ACurrentItem: TdxSelectionItem;
begin
  for I := FItems.Count - 2 downto 0 do
  begin
    ACurrentItem := Items[I];
    if (ActiveSelection.NormalizedStart <= ACurrentItem.NormalizedStart) and (ActiveSelection.NormalizedEnd >= ACurrentItem.NormalizedEnd) then
      FItems.Remove(ACurrentItem);
  end;
end;

procedure TdxSelection.AddSelection(ANewSelection: TdxSelectionItem);
begin
  ANewSelection.Changed.Add(OnSelectionChanged);
  Items.Add(ANewSelection);
  OwnActiveSelectionChanged := True;
end;

procedure TdxSelection.BeginMultiSelection(AActivePiecetable: TdxPieceTable);
var
  ANewSelection: TdxSelectionItem;
begin
  ANewSelection := TdxSelectionItem.Create(AActivePiecetable);
  BeginMultiSelection(ANewSelection);
end;

procedure TdxSelection.BeginMultiSelection(ANewSelection: TdxSelectionItem);
begin
  AddSelection(ANewSelection);
  Inc(FSelectionGeneration);
  ANewSelection.Generation := FSelectionGeneration;
end;

procedure TdxSelection.ClearMultiSelection(AClearFrom: Integer = 0);
var
  I, AItemsCount, ARemoveCount: Integer;
begin
  AItemsCount := FItems.Count;
  ARemoveCount := AItemsCount - AClearFrom - 1;
  if ARemoveCount > 0 then
  begin
    for I := AClearFrom to ARemoveCount - 1 do
      FItems[I].Changed.Remove(OnSelectionChanged);
    FItems.DeleteRange(AClearFrom, ARemoveCount);
  end;
  AClearFrom := FItems.Count - 1;
  FItems[AClearFrom].IsSelectionInTable := False;
  FItems[AClearFrom].IsCovered := False;
  if ARemoveCount > 0 then
    OwnActiveSelectionChanged := True;
  FSelectionGeneration := 0;
  FItems[AClearFrom].Generation := 0;
end;

procedure TdxSelection.ClearOutdatedItems;
var
  AStart, I: Integer;
  AItem: TdxSelectionItem;
begin
  AStart := FItems.Count - 1;
  for I := AStart downto 0 do
  begin
    AItem := FItems[I];
    if (AItem.Length = 0) and (FItems.Count > 1) then
    begin
      OwnActiveSelectionChanged := True;
      FItems[I].Changed.Remove(OnSelectionChanged);
      FItems.Delete(I);
    end;
  end;
end;

procedure TdxSelection.ClearSelectionInTable;
var
  I: Integer;
  AItem: TdxSelectionItem;
begin
  for I := FItems.Count - 2 downto 0 do
  begin
    AItem := FItems[I];
    if AItem.IsSelectionInTable and (AItem.Generation = SelectionGeneration) then
    begin
      AItem.Changed.Remove(OnSelectionChanged);
      FItems.Delete(I);
    end;
  end;
end;

procedure TdxSelection.Delete(Index: Integer);
var
  AItem: TdxSelectionItem;
begin
  BeginUpdate;
  try
    OwnActiveSelectionChanged := Index = Items.Count - 1;
    AItem := FItems[Index];
    AItem.Changed.Remove(OnSelectionChanged);
    FItems.Delete(Index);
    IsSelectionChanged := True;
  finally
    EndUpdate;
  end;
end;

function TdxSelection.IsWholeSelectionInOneTable: Boolean;
begin
  Result := (SelectedCells is TdxSelectedCellsCollection) and SelectedCells.IsNotEmpty;
end;

function TdxSelection.IsFloatingObjectSelected: Boolean;
begin
  Result := (Length = 1) and (PieceTable.Runs[Interval.NormalizedStart.RunIndex] is TdxFloatingObjectAnchorRun);
end;

function TdxSelection.IsInlinePictureSelected: Boolean;
begin
  Result := (Length = 1) and (PieceTable.Runs[Interval.NormalizedStart.RunIndex] is TdxInlinePictureRun);
end;

function TdxSelection.IsSelectionInTable: Boolean;
var
  I: TdxDocumentLogPosition;
begin
  for I := NormalizedStart to NormalizedEnd do
    if PieceTable.FindParagraph(I).IsInCell then
      Exit(True);
  Result := False;
end;

function TdxSelection.IsValidSelectedCells: Boolean;

  function ValidateSelection(AItem: TdxSelectionItem; ASelectedCells: TdxSelectedCellsCollection): Boolean;
  var
    I: Integer;
    ARowPieceTable: TdxSimplePieceTable;
    AFirstCell, ALastCell: TdxTableCell;
    ARow: TdxSelectedCellsIntervalInRow;
    AParagraphs: TdxSimpleParagraphCollection;
    AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  begin
    Result := False;
    for I := 0 to ASelectedCells.RowsCount - 1 do
    begin
      ARow := ASelectedCells.Items[I];
      AFirstCell := ARow.StartCell;
      ALastCell := ARow.EndCell;

      ARowPieceTable := ARow.Row.PieceTable;
      if (PieceTable <> ARowPieceTable) or (AFirstCell = nil) or (ALastCell = nil) then
        Exit(False);
      AParagraphs := ARowPieceTable.Paragraphs;

      AStartLogPosition := AParagraphs[AFirstCell.StartParagraphIndex].LogPosition;
      AEndLogPosition := AParagraphs[ALastCell.EndParagraphIndex].EndLogPosition;

      if (AItem.Start >= AStartLogPosition) and (AItem.&End - 1 <= AEndLogPosition) then
        Exit(True);
    end;
  end;

var
  I: Integer;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  Result := True;
  if SelectedCells is TdxSelectedCellsCollection then
  begin
    ASelectedCells := TdxSelectedCellsCollection(SelectedCells);
    for I := 0 to Items.Count - 1 do
      if not ValidateSelection(Items[I], ASelectedCells) then
        Exit(False);
  end;
end;

procedure TdxSelection.OnSelectionChanged(ASender: TObject; E: TdxEventArgs);
begin
  if not Changed.Empty then
    Changed.Invoke(Self, E);
end;

procedure TdxSelection.OnLastEndUpdate;
begin
  if OwnActiveSelectionChanged then
    ActiveSelection.OnChangedCore;
end;

procedure TdxSelection.OnLastCancelUpdate;
begin
  if OwnActiveSelectionChanged then
    ActiveSelection.OnChangedCore;
end;

procedure TdxSelection.OnParagraphInserted(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell;
  AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnParagraphRemoved(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnParagraphMerged(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex:
  TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnRunInserted(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer;
  AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSelection.OnRunRemoved(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength:
  Integer; AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSelection.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnBeginMultipleRunSplit;
end;

procedure TdxSelection.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnEndMultipleRunSplit;
end;

procedure TdxSelection.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSelection.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxSelection.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSelection.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSelection.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnFieldInserted(AFieldIndex);
end;

procedure TdxSelection.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnFieldRemoved(AFieldIndex);
end;

procedure TdxSelection.OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].ParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell,
      AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].ParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].ParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSelection.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSelection.OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSelection.OnBeginMultipleRunSplit;
begin
  Inc(FMultipleRunSplitCount);
end;

procedure TdxSelection.OnEndMultipleRunSplit;
var
  ASelectionsCount, AIndex: Integer;
begin
  Dec(FMultipleRunSplitCount);
  if FMultipleRunSplitCount = 0 then
  begin
    ASelectionsCount := Items.Count;
    for AIndex := 0 to ASelectionsCount - 1 do
    begin
      Items[AIndex].UpdateStartPosition;
      Items[AIndex].UpdateEndPosition;
    end;
  end;
end;

procedure TdxSelection.OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  if FMultipleRunSplitCount > 0 then
    Exit;

  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSelection.OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxSelection.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSelection.OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].RunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSelection.OnFieldInserted(AFieldIndex: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].FieldInserted(AFieldIndex);
end;

procedure TdxSelection.OnFieldRemoved(AFieldIndex: Integer);
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].FieldRemoved(AFieldIndex);
end;

procedure TdxSelection.UpdateStartPosition;
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].UpdateStartPosition;
end;

procedure TdxSelection.UpdateEndPosition;
var
  ASelectionsCount, AIndex: Integer;
begin
  ASelectionsCount := Items.Count;
  for AIndex := 0 to ASelectionsCount - 1 do
    Items[AIndex].UpdateEndPosition;
end;

function TdxSelection.GetSelectionCollection: TdxSelectionRangeCollection;
var
  ACount, I: Integer;
  AItem: TdxSelectionItem;
begin
  Result := TdxSelectionRangeCollection.Create;
  ACount := Items.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := Items[I];
    Result.Add(TdxSelectionRange.Create(AItem.NormalizedStart, AItem.Length));
  end;
end;

function TdxSelection.GetSortedSelectionCollection: TdxSelectionRangeCollection;
begin
  Result := GetSelectionCollection;
  Result.Sort(RangeComparer);
end;

function TdxSelection.IsSelectFieldPictureResult: Boolean;
var
  ARuns: TdxTextRunCollection;
  ARunIndex, APrevIndex, ANextIndex: TdxRunIndex;
begin
  if Length <> 1 then
    Exit(False);

  if Items.Count <> 1 then
    Exit(False);

  ARuns := PieceTable.Runs;
  ARunIndex := Items[0].Interval.NormalizedStart.RunIndex;
  APrevIndex := ARunIndex - 1;
  ANextIndex := ARunIndex + 1;
  if (APrevIndex < 0) or (ANextIndex >= ARuns.Count) then
    Exit(False);

  Result := ((ARuns[ARunIndex] is TdxInlinePictureRun) and (ARuns[APrevIndex] is TdxFieldCodeEndRun)) and (ARuns[ANextIndex] is TdxFieldResultEndRun);
end;

procedure TdxSelection.SetStartCell(ALogPosition: TdxDocumentLogPosition);
var
  ASelectionCalculator: TdxTableStructureBySelectionCalculator;
begin
  ASelectionCalculator := TdxTableStructureBySelectionCalculator.Create(PieceTable);
  try
    SelectedCells := ASelectionCalculator.SetStartCell(ALogPosition);
  finally
    ASelectionCalculator.Free;
  end;
end;

procedure TdxSelection.ManuallySetTableSelectionStructureAndChangeSelection(AStartCell: TdxTableCell; AEndCell: TdxTableCell);
begin
  ManuallySetTableSelectionStructureAndChangeSelection(AStartCell, AEndCell, False);
end;

procedure TdxSelection.ManuallySetTableSelectionStructureAndChangeSelection(AStartCell: TdxTableCell; AEndCell: TdxTableCell; AIsColumnSelected: Boolean);
var
  ASelectionCalculator: TdxTableStructureBySelectionCalculator;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ASelectionCalculator := TdxTableStructureBySelectionCalculator.Create(PieceTable);
  try
    ASelectedCells := ASelectionCalculator.Calculate(AStartCell, AEndCell, AIsColumnSelected);
  finally
    ASelectionCalculator.Free;
  end;
  ASelectedCells.OriginalStartLogPosition := SelectedCells.OriginalStartLogPosition;
  UpdateSelectionBy(ASelectedCells);
  SelectedCells := ASelectedCells;
end;

function TdxSelection.GetEndParagraphIndex(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex;
begin
  Result := PieceTable.FindParagraphIndex(ALogPosition);
end;

procedure TdxSelection.UpdateTableSelectionStart(ALogPosition: TdxDocumentLogPosition);
var
  AStartCell, AEndCell, ACell, AMostParentForFirst, AMostParentForSecond: TdxTableCell;
  ANestedLevel: Integer;
begin
  AStartCell := SelectedCells.FirstSelectedCell;
  if AStartCell = nil then
    Exit;

  AEndCell := PieceTable.FindParagraph(ALogPosition).GetCell;
  if (FirstCellIsParentCellForSecondCellsTable(AEndCell, AStartCell)) or (AEndCell = nil) then
  begin
    if AEndCell <> nil then
      ANestedLevel := AEndCell.Table.NestedLevel + 1
    else
      ANestedLevel := 0;
    ACell := PieceTable.TableCellsManager.GetCellByNestingLevel(AStartCell.StartParagraphIndex, ANestedLevel);
    Start := GetStartPositionInTableRow(ACell.Row);
    Exit;
  end;
  AMostParentForFirst := PieceTable.TableCellsManager.GetCellByNestingLevel(AStartCell.StartParagraphIndex, 0);
  AMostParentForSecond := PieceTable.TableCellsManager.GetCellByNestingLevel(AEndCell.StartParagraphIndex, 0);
  if AMostParentForFirst.Table <> AMostParentForSecond.Table then
    Start := GetStartPositionInTableRow(AMostParentForFirst.Row);
end;

function TdxSelection.GetStartPositionInTableRow(ARow: TdxTableRow): TdxDocumentLogPosition;
var
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphs := PieceTable.Paragraphs;
  if Start < &End then
    Result := AParagraphs[ARow.FirstCell.StartParagraphIndex].LogPosition
  else
    Result := AParagraphs[ARow.LastCell.EndParagraphIndex].EndLogPosition + 1;
end;

procedure TdxSelection.UpdateTableSelectionEnd(ALogPosition: TdxDocumentLogPosition; AConsiderCellStart: Boolean = False);
var
  ASelectionStart: TdxDocumentLogPosition;
  AStartCell, AEndCell, AParentCellForFirst, AParentCellForSecond: TdxTableCell;
  AStartCellNestedLevel: Integer;
  AParagraphBeforeTable: Boolean;
  ARuns: TdxTextRunCollection;
  ARunIndex: TdxRunIndex;
begin
  ASelectionStart := CalculateSelectionStart(ALogPosition);

  AStartCell := SelectedCells.FirstSelectedCell;
  AEndCell := DetermineEndCellByLogPosition(ASelectionStart, ALogPosition, AConsiderCellStart);

  if ((AStartCell <> nil) and (AStartCell = AEndCell)) and not IsCellSelected(AStartCell, ASelectionStart, &End) then
  begin
    SelectedCells := TdxSelectedCellsCollection.Create(SelectedCells.FirstSelectedCell, SelectedCells.OriginalStartLogPosition);
    Start := ASelectionStart;
    ClearSelectionInTable;
    Exit;
  end;
  if (AEndCell = nil) or FirstCellIsParentCellForSecondCellsTable(AEndCell, AStartCell) then
  begin
    if SelectedCells is TdxSelectedCellsCollection then
      SelectedCells := TdxStartSelectedCellInTable.Create(SelectedCells);
    Exit;
  end;

  AParagraphBeforeTable := False;
  ARuns := DocumentModel.ActivePieceTable.Runs;
  ARunIndex := Interval.&End.RunIndex;
  if (ARunIndex > 0) and (ARuns[ARunIndex - 1].Paragraph.GetCellCore = nil) and
    (ARuns[ARunIndex].Paragraph.GetCellCore <> nil) and IsSelectionChanged then
    AParagraphBeforeTable := True;
  if not SelectedCells.IsNotEmpty or (FirstCellIsParentCellForSecondCellsTable(AStartCell, AEndCell)) then
  begin
    if SelectedCells is TdxSelectedCellsCollection then
      SelectedCells := TdxStartSelectedCellInTable.Create(SelectedCells);
    if not AParagraphBeforeTable then
    begin
      AStartCellNestedLevel := GetActualNestedLevelConsiderStartCell(AStartCell);
      &End := GetEndPositionInTableRow(ALogPosition, AEndCell, AStartCellNestedLevel);
    end;
    Exit;
  end;
  if ((AStartCell <> nil) and (AEndCell <> nil)) and (AStartCell.Table = AEndCell.Table) then
  begin
    ManuallySetTableSelectionStructureAndChangeSelection(AStartCell, AEndCell);
    Exit;
  end;
  if FirstAndSecondCellHaveCommonTableButSecondCellNotParentForFirstCell(AStartCell, AEndCell) then
  begin
    NormalizeTableCellsToMinNestedLevel(AStartCell, AEndCell);
    if AStartCell.Table = AEndCell.Table then
    begin
      ManuallySetTableSelectionStructureAndChangeSelection(AStartCell, AEndCell);
      Exit;
    end;
    AParentCellForFirst := AStartCell.Table.ParentCell;
    AParentCellForSecond := AEndCell.Table.ParentCell;
    while (AParentCellForFirst <> nil) and (AParentCellForSecond <> nil) do
    begin
      if AParentCellForFirst.Table = AParentCellForSecond.Table then
      begin
        ManuallySetTableSelectionStructureAndChangeSelection(AParentCellForFirst, AParentCellForSecond);
        Exit;
      end;
      AParentCellForFirst := AParentCellForFirst.Table.ParentCell;
      AParentCellForSecond := AParentCellForSecond.Table.ParentCell;
    end;
    Exit;
  end
  else
    if AStartCell.Table <> AEndCell.Table then
    begin
      if SelectedCells is TdxSelectedCellsCollection then
        SelectedCells := TdxStartSelectedCellInTable.Create(SelectedCells);
      if not AParagraphBeforeTable then
        &End := GetEndPositionInTableRow(ALogPosition, AEndCell, 0);
      Exit;
    end
    else
      Start := SelectedCells.OriginalStartLogPosition;
end;

function TdxSelection.CalculateSelectionStart(ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  AActualStart, ACellStartPos, ACellEndPos, AOriginalStart: TdxDocumentLogPosition;
  AFirstSelectedCell: TdxTableCell;
begin
  AActualStart := Items[0].Start;
  AFirstSelectedCell := SelectedCells.FirstSelectedCell;
  if (AFirstSelectedCell <> nil) and SelectedCells.SelectedOnlyOneCell then
  begin
    ACellStartPos := PieceTable.Paragraphs[AFirstSelectedCell.StartParagraphIndex].LogPosition;
    ACellEndPos := PieceTable.Paragraphs[AFirstSelectedCell.EndParagraphIndex].EndLogPosition + 1;
    AOriginalStart := SelectedCells.OriginalStartLogPosition;
    if (AOriginalStart >= ACellStartPos) and (AOriginalStart < ACellEndPos) then
      Exit(AOriginalStart);
    if (AActualStart > ALogPosition) and (AActualStart = ACellEndPos) then
      Dec(AActualStart);
  end;
  Result := AActualStart;
end;

function TdxSelection.DetermineStartSelectedCell(ASelectionStart: TdxDocumentLogPosition): TdxTableCell;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := PieceTable.FindParagraphIndex(ASelectionStart);
  Result := PieceTable.Paragraphs[AParagraphIndex].GetCell;
  if Result = nil then
    Result := SelectedCells.FirstSelectedCell;
end;

function TdxSelection.IsCellSelectedFully(ATableCell: TdxTableCell; ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): Boolean;
var
  ANormalizeStart, ANormalizeEnd, ACellStartPos, ACellEndPos: TdxDocumentLogPosition;
begin
  ANormalizeStart := Min(ASelectionStart, ASelectionEnd);
  ANormalizeEnd := Max(ASelectionStart, ASelectionEnd);
  ACellStartPos := PieceTable.Paragraphs[ATableCell.StartParagraphIndex].LogPosition;
  ACellEndPos := PieceTable.Paragraphs[ATableCell.EndParagraphIndex].EndLogPosition + 1;
  Result := (ANormalizeStart <= ACellStartPos) and (ANormalizeEnd >= ACellEndPos);
end;

function TdxSelection.GetCellEndPosition(AStartCell: TdxTableCell): TdxDocumentLogPosition;
var
  ACellEndPos, ACellStartPos: TdxDocumentLogPosition;
begin
  if Start <= &End then
  begin
    ACellEndPos := PieceTable.Paragraphs[AStartCell.EndParagraphIndex].EndLogPosition;
    Exit(Min(ACellEndPos, &End));
  end
  else
  begin
    ACellStartPos := PieceTable.Paragraphs[AStartCell.StartParagraphIndex].LogPosition;
    Exit(Max(ACellStartPos, &End));
  end;
end;

function TdxSelection.DetermineEndCellByLogPosition(ASelectionStart: TdxDocumentLogPosition;
  ASelectionEnd: TdxDocumentLogPosition; AConsiderCellStart: Boolean): TdxTableCell;
var
  AFirstSelectedCell, ANextSelectedCell, APrevSelectedCell: TdxTableCell;
  APrevSelectedColumnIndex, ANextSelectedColumnIndex, AColumnIndex: Integer;
begin
  AFirstSelectedCell := SelectedCells.FirstSelectedCell;
  if ASelectionStart = ASelectionEnd then
    Exit(AFirstSelectedCell);

  ANextSelectedCell := DetermineEndCellCore(ASelectionStart, ASelectionEnd);
  if ANextSelectedCell = nil then
    Exit(nil);

  if AFirstSelectedCell = ANextSelectedCell then
    Exit(AFirstSelectedCell);

  APrevSelectedCell := GetLastSelectedCell;
  if (APrevSelectedCell = nil) or (IsRowsSelected(APrevSelectedCell, ANextSelectedCell)) then
    Exit(ANextSelectedCell);

  ANextSelectedCell := EnsureCellIsSelected(ASelectionStart, ASelectionEnd, ANextSelectedCell, APrevSelectedCell,
    AConsiderCellStart);
  if IsColumnsSelected(APrevSelectedCell, ANextSelectedCell) then
    Exit(ANextSelectedCell);

  APrevSelectedColumnIndex := APrevSelectedCell.GetStartColumnIndexConsiderRowGrid;
  ANextSelectedColumnIndex := ANextSelectedCell.GetStartColumnIndexConsiderRowGrid;

  if ASelectionStart < ASelectionEnd then
    AColumnIndex := Max(ANextSelectedColumnIndex, APrevSelectedColumnIndex)
  else
    AColumnIndex := Min(ANextSelectedColumnIndex, APrevSelectedColumnIndex);
  Result := ANextSelectedCell.Table.GetCell(ANextSelectedCell.Row, AColumnIndex);
end;

function TdxSelection.DetermineEndCellCore(ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): TdxTableCell;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  ACell: TdxTableCell;
begin
  AParagraphIndex := PieceTable.FindParagraphIndex(ASelectionEnd);
  AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  ACell := AParagraph.GetCell;
  if ACell <> nil then
    Exit(ACell);

  if (ASelectionStart > ASelectionEnd) or (ASelectionEnd > AParagraph.LogPosition) then
    Exit(nil);

  Dec(AParagraphIndex);
  if AParagraphIndex < 0 then
    Exit(nil);

  Result := PieceTable.Paragraphs[AParagraphIndex].GetCell;
end;

function TdxSelection.EnsureCellIsSelected(ASelectionStart: TdxDocumentLogPosition;
  ASelectionEnd: TdxDocumentLogPosition; ANextSelectedCell: TdxTableCell; APrevSelectedCell: TdxTableCell;
  AConsiderCellStart: Boolean): TdxTableCell;
var
  ANormalizeEnd, ACellStartPos: TdxDocumentLogPosition;
  ASelectedCellChanged, ACellShouldBeUnselected, ACellIsNotSelected, AIsLeftToRightDirection: Boolean;
  APreviousCell: TdxTableCell;
begin
  ANormalizeEnd := Max(ASelectionStart, ASelectionEnd);
  ACellStartPos := PieceTable.Paragraphs[ANextSelectedCell.StartParagraphIndex].LogPosition;

  ASelectedCellChanged := ANextSelectedCell <> APrevSelectedCell;
  ACellShouldBeUnselected := not ASelectedCellChanged and not IsCellSelectedFully(ANextSelectedCell, ASelectionStart, ASelectionEnd);
  if AConsiderCellStart then
    ACellIsNotSelected := ANormalizeEnd < ACellStartPos
  else
    ACellIsNotSelected := ANormalizeEnd <= ACellStartPos;

  AIsLeftToRightDirection := ASelectionStart <= ASelectionEnd;
  if ACellShouldBeUnselected or ACellIsNotSelected then
  begin
    APreviousCell := GetPreviousTableCell(ANextSelectedCell, AIsLeftToRightDirection);
    if APreviousCell <> nil then
      Exit(APreviousCell);
  end;
  Result := ANextSelectedCell;
end;

function TdxSelection.IsCellSelected(ACell: TdxTableCell; ASelectionStart: TdxDocumentLogPosition; ASelectionEnd: TdxDocumentLogPosition): Boolean;
var
  ANormalizeStart, ANormalizeEnd, ACellEndPos: TdxDocumentLogPosition;
begin
  ANormalizeStart := Min(ASelectionStart, ASelectionEnd);
  ANormalizeEnd := Max(ASelectionStart, ASelectionEnd);
  ACellEndPos := PieceTable.Paragraphs[ACell.EndParagraphIndex].EndLogPosition + 1;
  Result := (ANormalizeStart < ACellEndPos) and (ANormalizeEnd >= ACellEndPos);
end;

function TdxSelection.GetPreviousTableCell(ACell: TdxTableCell; AIsLeftToRightDirection: Boolean): TdxTableCell;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  if AIsLeftToRightDirection then
  begin
    AParagraphIndex := ACell.StartParagraphIndex - 1;
    if AParagraphIndex < 0 then
      Exit(nil);
  end
  else
  begin
    AParagraphIndex := ACell.EndParagraphIndex + 1;
    if AParagraphIndex > PieceTable.Paragraphs.Count - 1 then
      Exit(nil);
  end;
  Result := PieceTable.Paragraphs[AParagraphIndex].GetCell;
end;

function TdxSelection.IsColumnsSelected(APrevSelectedCell: TdxTableCell; ANextSelectedCell: TdxTableCell): Boolean;
begin
  Result := ANextSelectedCell.Row = APrevSelectedCell.Row;
end;

function TdxSelection.GetLastSelectedCell: TdxTableCell;
var
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ASelectedCells := Safe<TdxSelectedCellsCollection>.Cast(SelectedCells);
  if (ASelectedCells <> nil) and ASelectedCells.IsNotEmpty then
    Result := ASelectedCells.Last.EndCell
  else
    Result := SelectedCells.FirstSelectedCell;
end;

function TdxSelection.IsRowsSelected(APrevSelectedCell: TdxTableCell; ANextSelectedCell: TdxTableCell): Boolean;
var
  APrevSelectedColumnIndex, ANextSelectedColumnIndex: Integer;
begin
  APrevSelectedColumnIndex := APrevSelectedCell.GetStartColumnIndexConsiderRowGrid;
  ANextSelectedColumnIndex := ANextSelectedCell.GetStartColumnIndexConsiderRowGrid;
  Result := (APrevSelectedCell <> ANextSelectedCell) and (APrevSelectedColumnIndex = ANextSelectedColumnIndex);
end;

function TdxSelection.GetActualNestedLevelConsiderStartCell(AStartCell: TdxTableCell): Integer;
begin
  if SelectedCells.IsNotEmpty then
    Result := AStartCell.Table.NestedLevel + 1
  else
    Result := 0;
end;

function TdxSelection.FirstAndSecondCellHaveCommonTableButSecondCellNotParentForFirstCell(AFirst: TdxTableCell; ASecond: TdxTableCell): Boolean;
var
  AParentCellForFirst, AParentCellForSecond: TdxTableCell;
begin
  NormalizeTableCellsToMinNestedLevel(AFirst, ASecond);

  if AFirst = ASecond then
    Exit(False);

  if AFirst.Table = ASecond.Table then
    Exit(True);

  AParentCellForFirst := AFirst.Table.ParentCell;
  AParentCellForSecond := ASecond.Table.ParentCell;
  while (AParentCellForFirst <> nil) and (AParentCellForSecond <> nil) do
  begin
    if AParentCellForFirst.Table = AParentCellForSecond.Table then
      Exit(True);

    AParentCellForFirst := AParentCellForFirst.Table.ParentCell;
    AParentCellForSecond := AParentCellForSecond.Table.ParentCell;
  end;
  Result := False;
end;

procedure TdxSelection.NormalizeTableCellsToMinNestedLevel(var AStartCell, AEndCell: TdxTableCell);
var
  AStartCellNestedLevel, AEndCellNestedLevel: Integer;
  ATableCellManager: TdxTableCellsManager;
begin
  AStartCellNestedLevel := AStartCell.Table.NestedLevel;
  AEndCellNestedLevel := AEndCell.Table.NestedLevel;
  if AStartCellNestedLevel = AEndCellNestedLevel then
    Exit;

  ATableCellManager := PieceTable.TableCellsManager;
  if AStartCellNestedLevel < AEndCellNestedLevel then
    AEndCell := ATableCellManager.GetCellByNestingLevel(AEndCell.StartParagraphIndex, AStartCellNestedLevel)
  else
    AStartCell := ATableCellManager.GetCellByNestingLevel(AStartCell.StartParagraphIndex, AEndCellNestedLevel);
end;

function TdxSelection.FirstCellIsParentCellForSecondCellsTable(AFirstCell: TdxTableCell; ASecondCell: TdxTableCell): Boolean;
var
  AParentTable: TdxTable;
begin
  if (AFirstCell = nil) or (ASecondCell = nil) or (ASecondCell.Table.ParentCell = nil) then
    Exit(False);

  AParentTable := ASecondCell.Table;
  while AParentTable.ParentCell <> nil do
  begin
    if AParentTable.ParentCell = AFirstCell then
      Exit(True);
    AParentTable := AParentTable.ParentCell.Table;
  end;
  Result := False;
end;

function TdxSelection.GetEndPositionInTableRow(AInitialEnd: TdxDocumentLogPosition; ACell: TdxTableCell; ANestedLevel: Integer): TdxDocumentLogPosition;
var
  ARow: TdxTableRow;
  AFirstCell, ALastCell: TdxTableCell;
  AStartParagraphIndexInRow, AEndParagraphIndexInRow: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  AStartLogPos, AEndLogPos: TdxDocumentLogPosition;
begin
  ARow := ACell.Row;
  while ARow.Table.NestedLevel > ANestedLevel do
    ARow := ARow.Table.ParentCell.Row;
  AFirstCell := GetFirstNonCoveredByVerticalMergingCell(ARow);
  ALastCell := GetLastNonCoveredByVerticalMergingCell(ARow);
  AStartParagraphIndexInRow := AFirstCell.StartParagraphIndex;
  AEndParagraphIndexInRow := ALastCell.EndParagraphIndex;
  AParagraphs := PieceTable.Paragraphs;
  AStartLogPos := AParagraphs[AStartParagraphIndexInRow].LogPosition;
  AEndLogPos := AParagraphs[AEndParagraphIndexInRow].EndLogPosition + 1;
  if AInitialEnd < Start then
    Result := Min(AStartLogPos, AEndLogPos)
  else
    Result := Max(AStartLogPos, AEndLogPos);
end;

function TdxSelection.GetLastNonCoveredByVerticalMergingCell(ARow: TdxTableRow): TdxTableCell;
var
  ARowCellsCount, ACellIndex: Integer;
begin
  ARowCellsCount := ARow.Cells.Count;
  Result := ARow.Cells.Last;
  ACellIndex := ARowCellsCount - 1;
  while (ACellIndex >= 0) and (Result.VerticalMerging = TdxMergingState.Continue) do
  begin
    Result := ARow.Cells[ACellIndex];
    Dec(ACellIndex);
  end;
end;

function TdxSelection.GetFirstNonCoveredByVerticalMergingCell(ARow: TdxTableRow): TdxTableCell;
var
  ARowCellsCount, ACellIndex: Integer;
begin
  ARowCellsCount := ARow.Cells.Count;
  Result := ARow.Cells.First;
  ACellIndex := 0;
  while (ACellIndex < ARowCellsCount) and (Result.VerticalMerging = TdxMergingState.Continue) do
  begin
    Result := ARow.Cells[ACellIndex];
    Inc(ACellIndex);
  end;
end;

procedure TdxSelection.UpdateSelectionBy(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection});
var
  AIsLeftToRightDirection: Boolean;
  ASelectedRowsCount, I: Integer;
  ACells: TdxSelectedCellsCollection absolute ASelectedCells;
begin
  ClearSelectionInTable;
  AIsLeftToRightDirection := IsLeftToRightDirection(ACells);
  ASelectedRowsCount := ACells.RowsCount;
  for I := 0 to ASelectedRowsCount - 1 do
  begin
    if I >= Items.Count then
      AddSelection(TdxSelectionItem.Create(PieceTable));
    UpdateInTableSelectionItem(ActiveSelection, ACells[I], AIsLeftToRightDirection);
  end;
  RemoveIntersectedSelectionItems(ASelectedRowsCount);
end;

function TdxSelection.IsLeftToRightDirection(ASelectedCells: TdxSelectedTableStructureBase{TdxSelectedCellsCollection}): Boolean;
var
  AFirstRowIndex, ALastRowIndex: Integer;
  ACells: TdxSelectedCellsCollection absolute ASelectedCells;
begin
  if ACells.SelectedOnlyOneCell then
    Exit(Start <= &End);
  AFirstRowIndex := ACells.First.Row.IndexInTable;
  ALastRowIndex := ACells.Last.Row.IndexInTable;
  if AFirstRowIndex <> ALastRowIndex then
    Result := AFirstRowIndex < ALastRowIndex
  else
    Result := ACells.First.StartCellIndex <= ACells.First.EndCellIndex;
end;

procedure TdxSelection.UpdateInTableSelectionItem(AItem: TdxSelectionItem; ACurrentCellsInterval: TdxSelectedCellsIntervalInRow; AIsLeftToRightDirection: Boolean);
var
  AStart, AEnd: TdxParagraphIndex;
begin
  AItem.IsSelectionInTable := True;
  AItem.Generation := DocumentModel.Selection.SelectionGeneration;

  if AIsLeftToRightDirection then
  begin
    AStart := ACurrentCellsInterval.StartCell.StartParagraphIndex;
    AEnd := ACurrentCellsInterval.EndCell.EndParagraphIndex;
    AItem.Start := PieceTable.Paragraphs[AStart].LogPosition;
    AItem.&End := PieceTable.Paragraphs[AEnd].EndLogPosition + 1;
  end
  else
  begin
    AStart := ACurrentCellsInterval.StartCell.EndParagraphIndex;
    AEnd := ACurrentCellsInterval.EndCell.StartParagraphIndex;
    AItem.Start := PieceTable.Paragraphs[AStart].EndLogPosition + 1;
    AItem.&End := PieceTable.Paragraphs[AEnd].LogPosition;
  end;
end;

procedure TdxSelection.RemoveIntersectedSelectionItems(ALastInsertedItemsCount: Integer);
var
  AGenerationsToRemove: TdxIntegerList;
  I, ALastAddedItemsCount, APreviouslyAddedCount, ALastGensIndex, ALastAddedItemIndex: Integer;
  ALastAddedItems, APreviouslyAdded: TdxSelectionItemList;
  AItem, ACurrentItem: TdxSelectionItem;
  AItemNormalizedEnd, ACurrentItemNormalizedStart, AItemNormalizedStart, ACurrentItemNormalizedEnd: TdxDocumentLogPosition;
  AIsItemNormEndInCurrentInterval, AIsItemNormStartInCurrentInterval, AIsCurrentIntervalBelongsItem: Boolean;
begin
  if Items.Count = 0 then
    Exit;

  AGenerationsToRemove := TdxIntegerList.Create;
  try
    ALastAddedItems := Items.GetRange(Items.Count - ALastInsertedItemsCount, ALastInsertedItemsCount);
    try
      ALastAddedItemsCount := ALastAddedItems.Count;
      APreviouslyAdded := Items.GetRange(0, Items.Count - ALastInsertedItemsCount);
      try
        for ALastAddedItemIndex := 0 to ALastAddedItemsCount - 1 do
        begin
          AItem := ALastAddedItems[ALastAddedItemIndex];
          APreviouslyAddedCount := APreviouslyAdded.Count;
          for i := 0 to APreviouslyAddedCount - 1 do
          begin
            ACurrentItem := APreviouslyAdded[i];
            ALastGensIndex := AGenerationsToRemove.Count - 1;
            if (AGenerationsToRemove.Count > 0) and (AGenerationsToRemove[ALastGensIndex] = ACurrentItem.Generation) then
              Continue;
            AItemNormalizedEnd := AItem.NormalizedEnd;
            ACurrentItemNormalizedStart := ACurrentItem.NormalizedStart;
            AItemNormalizedStart := AItem.NormalizedStart;
            ACurrentItemNormalizedEnd := ACurrentItem.NormalizedEnd;
            AIsItemNormEndInCurrentInterval := (AItemNormalizedEnd > ACurrentItemNormalizedStart) and (AItemNormalizedEnd < ACurrentItemNormalizedEnd);
            AIsItemNormStartInCurrentInterval := (AItemNormalizedStart > ACurrentItemNormalizedStart) and (AItemNormalizedStart < ACurrentItemNormalizedEnd);
            AIsCurrentIntervalBelongsItem := (AItemNormalizedStart <= ACurrentItemNormalizedStart) and (AItemNormalizedEnd >= ACurrentItemNormalizedEnd);
            if AIsItemNormEndInCurrentInterval or AIsItemNormStartInCurrentInterval or AIsCurrentIntervalBelongsItem then
            begin
              if not AGenerationsToRemove.Contains(ACurrentItem.Generation) then
                AGenerationsToRemove.Add(ACurrentItem.Generation);
            end;
          end;
        end;
      finally
        FreeAndNil(APreviouslyAdded);
      end;
    finally
      FreeAndNil(ALastAddedItems);
    end;
    if AGenerationsToRemove.Count = 0 then
      Exit;
    for I := Items.Count - ALastInsertedItemsCount - 1 downto 0 do
    begin
      AItem := Items[I];
      if AGenerationsToRemove.Contains(AItem.Generation) then
      begin
        AItem.Changed.Remove(OnSelectionChanged);
        Items.Delete(I);
      end;
    end;
  finally
    FreeAndNil(AGenerationsToRemove);
  end;
end;

function TdxSelection.GetSelectedTableRows: TdxTableRowList;
var
  ACells: TdxSelectedCellsCollection;
begin
  if not IsWholeSelectionInOneTable then
    Exit(TdxTableRowList.Create);
  ACells := TdxSelectedCellsCollection(SelectedCells);
  Result := ACells.GetSelectedTableRows;
end;

function TdxSelection.GetSelectedParagraphs: TdxParagraphList;
var
  AParagraphs: TdxParagraphCollection;
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AItem: TdxSelectionItem;
  AStart, AEnd, AParagraphIndex: TdxParagraphIndex;
begin
  Result := TdxParagraphList.Create(False);
  AParagraphs := PieceTable.Paragraphs;
  AItems := Items;
  ACount := AItems.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := AItems[I];
    AStart := AItem.GetStartParagraphIndex;
    AEnd := AItem.GetEndParagraphIndex;
    for AParagraphIndex := AStart to AEnd do
      Result.Add(AParagraphs[AParagraphIndex]);
  end;
end;

{ TdxSelectionItem }

constructor TdxSelectionItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
end;

destructor TdxSelectionItem.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

procedure TdxSelectionItem.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate();
end;

function TdxSelectionItem.CalculateEndPosition(AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
var
  AIterator: TdxWordsDocumentModelIterator;
begin
  if Start < &End then
    Result := Interval.&End
  else
    if Start > &End then
      Result := Interval.Start
    else
    begin
      if AAllowSelectionExpanding then
      begin
        AIterator := TdxWordsDocumentModelIterator.Create(PieceTable);
        try
          if not AIterator.IsInsideWord(Interval.&End) or AIterator.IsNewElement(Interval.&End) then
            Result := Interval.&End
          else
            Result := AIterator.MoveForward(Interval.&End);
        finally
          AIterator.Free;
        end;
      end
      else
        Result := Interval.Start;
    end;
end;

function TdxSelectionItem.CalculateStartPosition(AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
var
  AStart: TdxDocumentModelPosition;
  AIterator: TdxWordsDocumentModelIterator;
begin
  if Start < &End then
    Result := Interval.Start
  else if Start > &End then
    Result := Interval.&End
  else
  begin
    AStart := Interval.Start;
    if AAllowSelectionExpanding then
    begin
      AIterator := TdxWordsDocumentModelIterator.Create(PieceTable);
      try
        if not AIterator.IsInsideWord(AStart) or AIterator.IsNewElement(AStart) then
          Result := AStart
        else
          Result := AIterator.MoveBack(AStart);
      finally
        AIterator.Free;
      end;
    end
    else
      Result := AStart;
  end;
end;

procedure TdxSelectionItem.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxSelectionItem.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

function TdxSelectionItem.GetEnd: TdxDocumentLogPosition;
begin
  Result := inherited GetEnd;
end;

function TdxSelectionItem.GetEndParagraphIndex: TdxParagraphIndex;
begin
  if (NormalizedEnd > 0) and (NormalizedEnd <> NormalizedStart) then
    Result := PieceTable.FindParagraphIndex(NormalizedEnd - 1, False)
  else
    Result := PieceTable.FindParagraphIndex(NormalizedEnd, False);
end;

function TdxSelectionItem.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxSelectionItem.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxSelectionItem.GetStart: TdxDocumentLogPosition;
begin
  Result := inherited GetStart;
end;

function TdxSelectionItem.GetStartParagraphIndex: TdxParagraphIndex;
begin
  Result := Interval.NormalizedStart.ParagraphIndex;
end;

function TdxSelectionItem.GetUsePreviousBoxBounds: Boolean;
begin
  Result := FUsePreviousBoxBounds;
end;

function TdxSelectionItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxSelectionItem.GetVirtualEnd: TdxDocumentLogPosition;
begin
  if UsePreviousBoxBounds then
    Result := Max(0, &End - 1)
  else
    Result := &End;
end;

procedure TdxSelectionItem.OnBeginUpdate;
begin
end;

procedure TdxSelectionItem.OnCancelUpdate;
begin
end;

procedure TdxSelectionItem.OnLastCancelUpdate;
begin
  if FChanged then
    OnChangedCore;
end;

procedure TdxSelectionItem.OnChanged(AStartChanged, AEndChanged: Boolean);
begin
  if AStartChanged or AEndChanged then
  begin
    FUsePreviousBoxBounds := False;
    OnChangedCore;
  end;
end;

procedure TdxSelectionItem.OnChangedCore;
begin
  if IsUpdateLocked then
	  FChanged := True
  else
		RaiseChanged;
end;

procedure TdxSelectionItem.OnEndUpdate;
begin
end;

procedure TdxSelectionItem.OnFirstBeginUpdate;
begin
  FChanged := False;
end;

procedure TdxSelectionItem.OnLastEndUpdate;
begin
  if FChanged then
    OnChangedCore;
end;

procedure TdxSelectionItem.RaiseChanged;
begin
  if not FChangedEvent.Empty then
    FChangedEvent.Invoke(Self, nil);
end;

procedure TdxSelectionItem.ResetOffsets;
begin
  FLeftOffset := 0;
  FRightOffset := 0;
end;

procedure TdxSelectionItem.SetEnd(const Value: TdxDocumentLogPosition);
begin
  inherited SetEnd(Value);
  ResetOffsets;
end;

procedure TdxSelectionItem.SetStart(const Value: TdxDocumentLogPosition);
begin
  inherited SetStart(Value);
  ResetOffsets;
end;

procedure TdxSelectionItem.SetUsePreviousBoxBounds(const Value: Boolean);
begin
  if FUsePreviousBoxBounds <> value then
    FUsePreviousBoxBounds := Value;
end;

{ TdxSelectionItemList }

function TdxSelectionItemList.First: TdxSelectionItem;
begin
  Result := TdxSelectionItem(inherited First);
end;

function TdxSelectionItemList.Last: TdxSelectionItem;
begin
  Result := TdxSelectionItem(inherited Last);
end;

function TdxSelectionItemList.GetItem(Index: Integer): TdxSelectionItem;
begin
  Result := TdxSelectionItem(inherited Items[Index]);
end;

{ TdxEmptySelection }

function TdxEmptySelection.InitialSelection(APieceTable: TdxCustomPieceTable): TdxSelectionItem;
begin
  Result := TdxEmptySelectionItem.Create(APieceTable);
end;

{ TdxEmptySelectionItem }

function TdxEmptySelectionItem.GetEnd: TdxDocumentLogPosition;
begin
  Result := 0;
end;

function TdxEmptySelectionItem.GetStart: TdxDocumentLogPosition;
begin
  Result := 0;
end;

procedure TdxEmptySelectionItem.ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySelectionItem.ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySelectionItem.ParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySelectionItem.RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySelectionItem.RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset, ATailRunLength: Integer);
begin
end;

procedure TdxEmptySelectionItem.RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
end;

procedure TdxEmptySelectionItem.RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySelectionItem.RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
end;

procedure TdxEmptySelectionItem.RaiseChanged;
begin
end;

procedure TdxEmptySelectionItem.UpdateEndPosition;
begin
end;

procedure TdxEmptySelectionItem.UpdateStartPosition;
begin
end;

{ TdxChangeableDocumentInterval }

procedure TdxChangeableDocumentInterval.SetEnd(const Value: TdxDocumentLogPosition);
begin
  SetEndCore(Value);
end;

procedure TdxChangeableDocumentInterval.SetStart(const Value: TdxDocumentLogPosition);
begin
  SetStartCore(Value);
end;

{ TdxDocumentModelChangeActionsCalculator }

class function TdxDocumentModelChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxDocumentModelChangeType): TdxDocumentModelChangeActions;
const
  DocumentModelChangeActionsMap : array[TdxDocumentModelChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.RaiseContentChanged,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.RaiseContentChanged,
     TdxDocumentModelChangeAction.RaiseSelectionChanged,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ScrollToBeginOfDocument,
     TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler,
     TdxDocumentModelChangeAction.Fields],
    [TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.RaiseContentChanged,
     TdxDocumentModelChangeAction.RaiseSelectionChanged,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ScrollToBeginOfDocument,
     TdxDocumentModelChangeAction.RaiseDocumentLoaded,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler,
     TdxDocumentModelChangeAction.Fields],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.RaiseContentChanged,
     TdxDocumentModelChangeAction.RaiseSelectionChanged,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
    [TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
     TdxDocumentModelChangeAction.ResetIgnoredList,
     TdxDocumentModelChangeAction.ValidateSelectionInterval],
     [TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
    [TdxDocumentModelChangeAction.Fields]);
begin
  Result := DocumentModelChangeActionsMap[AChange];
end;

{ TdxMeasurementAndDrawingStrategy }

constructor TdxMeasurementAndDrawingStrategy.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

destructor TdxMeasurementAndDrawingStrategy.Destroy;
begin
  FreeAndNil(FMeasurer);
  inherited Destroy;
end;

procedure TdxMeasurementAndDrawingStrategy.Initialize;
begin
  FMeasurer := CreateBoxMeasurer;
end;

procedure TdxMeasurementAndDrawingStrategy.OnLayoutUnitChanged;
begin
  Measurer.OnLayoutUnitChanged;
end;

{ TdxParagraphEventArgs }

constructor TdxParagraphEventArgs.Create(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FSectionIndex := ASectionIndex;
  FParagraphIndex := AParagraphIndex;
end;

{ TdxFieldEventArgs }

constructor TdxFieldEventArgs.Create(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FFieldIndex := AFieldIndex;
end;

{ TdxDocumentModelCopyOptions }

constructor TdxDocumentModelCopyOptions.Create(ASelectionRanges: TdxSelectionRangeCollection);
begin
  inherited Create;
  FParagraphNumerationCopyOptions := TdxParagraphNumerationCopyOptions.CopyAlways;
  FDefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Never;
  FFormattingCopyOptions := TdxFormattingCopyOptions.UseDestinationStyles;
  Assert(ASelectionRanges.Count > 0);
  FSelectionRanges := ASelectionRanges;
end;

constructor TdxDocumentModelCopyOptions.Create(AFrom: TdxDocumentLogPosition; ALength: Integer);
var
  ASelectionRanges: TdxSelectionRangeCollection;
begin
  FOwnSelectionRanges := True;
  ASelectionRanges := TdxSelectionRangeCollection.Create;
  ASelectionRanges.Add(TdxSelectionRange.Create(AFrom, ALength));
  Create(ASelectionRanges);
end;

destructor TdxDocumentModelCopyOptions.Destroy;
begin
  if FOwnSelectionRanges then
    FreeAndNil(FSelectionRanges);
  inherited Destroy;
end;

function TdxDocumentModelCopyOptions.GetFrom: TdxDocumentLogPosition;
begin
  Result := SelectionRanges[0].From;
end;

function TdxDocumentModelCopyOptions.GetLength: Integer;
begin
  Result := SelectionRanges[0].Length;
end;

procedure TdxDocumentModelCopyOptions.SetFrom(const Value: TdxDocumentLogPosition);
begin
  SelectionRanges[0].From := Value;
end;

procedure TdxDocumentModelCopyOptions.SetLength(const Value: Integer);
begin
  SelectionRanges[0].Length := Value;
end;

{ TdxCalculateDocumentVariableEventRouter }

constructor TdxCalculateDocumentVariableEventRouter.Create(ATargetModel: TdxDocumentModel);
begin
  inherited Create;
  FTargetModel := ATargetModel;
end;

procedure TdxCalculateDocumentVariableEventRouter.OnCalculateDocumentVariable(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
var
  AServer: TdxInternalRichEditDocumentServer;
  AModel: TdxDocumentModel;
begin
  E.Value := FTargetModel.Variables.GetVariableValue(E.VariableName, E.Arguments);
  AServer := TdxInternalRichEditDocumentServer.TryConvertInternalRichEditDocumentServer(E.Value);
  AModel := Safe<TdxDocumentModel>.Cast(Sender);
  if (AServer <> nil) and (AModel <> nil) then
    AModel.MailMergeOptions.KeepLastParagraph := AServer.DocumentModel.MailMergeOptions.KeepLastParagraph;
  E.Handled := True;
end;

{ TdxSelectedCellsIntervalInRow }

constructor TdxSelectedCellsIntervalInRow.Create(ARow: TdxTableRow; AStartCellIndex: Integer; AEndCellIndex: Integer);
begin
  inherited Create;
  FRow := ARow;
  FStartCellIndex := AStartCellIndex;
  FEndCellIndex := AEndCellIndex;
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedStartCellIndex: Integer;
begin
  Result := Min(FStartCellIndex, FEndCellIndex);
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedEndCellIndex: Integer;
begin
  Result := Max(FStartCellIndex, FEndCellIndex);
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedStartCell: TdxTableCell;
begin
  Result := Row.Cells[NormalizedStartCellIndex];
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedEndCell: TdxTableCell;
begin
  Result := Row.Cells[NormalizedEndCellIndex];
end;

function TdxSelectedCellsIntervalInRow.GetStartCell: TdxTableCell;
begin
  if StartCellIndex < Row.Cells.Count then
    Result := Row.Cells[StartCellIndex]
  else
    Result := nil;
end;

function TdxSelectedCellsIntervalInRow.GetEndCell: TdxTableCell;
begin
  if EndCellIndex < Row.Cells.Count then
    Result := Row.Cells[EndCellIndex]
  else
    Result := nil;
end;

function TdxSelectedCellsIntervalInRow.GetIsContainsOnlyOneCell: Boolean;
begin
  Result := StartCellIndex = EndCellIndex;
end;

function TdxSelectedCellsIntervalInRow.GetLeftCell: TdxTableCell;
begin
  if StartCellIndex < EndCellIndex then
    Result := Row.Cells[StartCellIndex]
  else
    Result := Row.Cells[EndCellIndex];
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedLength: Integer;
begin
  Result := Abs(EndCellIndex - StartCellIndex);
end;

function TdxSelectedCellsIntervalInRow.GetTable: TdxTable;
begin
  Result := Row.Table;
end;

function TdxSelectedCellsIntervalInRow.GetNormalizedColumnSpan: Integer;
var
  AStartCellIndex, AEndCellIndex, I: Integer;
  ACells: TdxTableCellCollection;
begin
  AStartCellIndex := NormalizedStartCellIndex;
  AEndCellIndex := NormalizedEndCellIndex;
  Result := 0;
  ACells := FRow.Cells;
  for I := AStartCellIndex to AEndCellIndex do
    Inc(Result, ACells[I].ColumnSpan);
end;

function TdxSelectedCellsIntervalInRow.ContainsCell(ACell: TdxTableCell): Boolean;
var
  AEnd, I: Integer;
  ACells: TdxTableCellCollection;
begin
  AEnd := NormalizedEndCellIndex;
  ACells := Row.Cells;
  Result := False;
  for I := NormalizedStartCellIndex to AEnd do
    if ACells[I] = ACell then
    begin
      Result := True;
      Break;
    end;
end;

{ TdxFieldsOperation }

class function TdxFieldsOperation.IsEntireFieldAffected(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
begin
  Result := (ARunInfo.NormalizedStart.RunIndex <= AField.Code.Start) and
    (ARunInfo.NormalizedEnd.RunIndex >= AField.Result.&End);
end;

class function TdxFieldsOperation.IsFieldCodeTextAffectedOnly(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
begin
  Result := (ARunInfo.NormalizedStart.RunIndex > AField.Code.Start) and
    (ARunInfo.NormalizedEnd.RunIndex < AField.Code.&End);
end;

class function TdxFieldsOperation.IsFieldResultTextAffectedOnly(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
begin
  Result := (ARunInfo.NormalizedStart.RunIndex >= AField.Result.Start) and
     (ARunInfo.NormalizedEnd.RunIndex < AField.Result.&End);
end;

class function TdxFieldsOperation.IsEntireFieldResultAffected(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
var
  AStart, AEnd: TdxRunIndex;
begin
  if AField.IsCodeView then
    Exit(False);
  AStart := AField.Result.Start;
  AEnd := AField.Result.&End - 1;
  if AStart > AEnd then
    Exit(False);
  Result := (ARunInfo.NormalizedStart.RunIndex = AStart) and (ARunInfo.NormalizedEnd.RunIndex = AEnd);
end;

class procedure TdxFieldsOperation.EnsureIntervalContainsField(AInterval: TdxRunInfo; AField: TdxField);
var
  ANewStart, ANewEnd: TdxDocumentModelPosition;
begin
  ANewStart := TdxDocumentModelPosition.FromRunStart(AInterval.Start.PieceTable, AField.FirstRunIndex);
  if ANewStart.LogPosition < AInterval.Start.LogPosition then
    AInterval.Start.CopyFrom(ANewStart);
  ANewEnd := TdxDocumentModelPosition.FromRunStart(AInterval.&End.PieceTable, AField.LastRunIndex);
  if ANewEnd.LogPosition > AInterval.&End.LogPosition then
    AInterval.&End.CopyFrom(ANewEnd);
end;

{ TdxCopyFieldsOperation.TdxUpdateFieldsParameters }

constructor TdxCopyFieldsOperation.TdxUpdateFieldsParameters.Create(
  const AFields, AFieldsToDelete: TdxFieldList; AParent: TdxField);
begin
  inherited Create;
  FFields := AFields;
  FFieldsToDelete := AFieldsToDelete;
  FParent := AParent;
end;

destructor TdxCopyFieldsOperation.TdxUpdateFieldsParameters.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FFieldsToDelete);
  inherited Destroy;
end;

{ TdxCopyFieldsOperation }

constructor TdxCopyFieldsOperation.Create(ASourcePieceTable,
  ATargetPieceTable: TdxPieceTable);
begin
  inherited Create;
  FSourcePieceTable := ASourcePieceTable;
  FTargetPieceTable := ATargetPieceTable;
  FUpdateFieldOperationType := TdxUpdateFieldOperationType.Copy;
end;

destructor TdxCopyFieldsOperation.Destroy;
begin
  FreeAndNil(FUpdateFieldsParameters);
  inherited Destroy;
end;

function TdxCopyFieldsOperation.CalculateCopingFields(
  AOriginFields: TdxFieldList; ARunOffset: Integer): TdxFieldList;
var
  I, ACount: Integer;
  ANewField: TdxField;
begin
  Result := TdxFieldList.Create;
  ACount := AOriginFields.Count;
  for I := 0 to ACount - 1 do
  begin
    ANewField := AOriginFields[I].CloneToNewPieceTable(TargetPieceTable);
    ANewField.Code.ShiftRunIndex(ARunOffset);
    ANewField.Result.ShiftRunIndex(ARunOffset);
    Result.Add(ANewField);
  end;
end;

procedure TdxCopyFieldsOperation.CalculateFieldsHierarchy(AOriginFields,
  ANewFields: TdxFieldList; AParentFieldIndex: Integer);
var
  AParentField: TdxField;
  AOriginParent: TdxField;
  I, ACount: Integer;
  AParentIndex: Integer;
begin
  if AParentFieldIndex >= 0 then
    AParentField := TargetPieceTable.Fields[AParentFieldIndex]
  else
    AParentField := nil;
  ACount := AOriginFields.Count;
  for I := 0 to ACount - 1 do
  begin
    AOriginParent := AOriginFields[I].Parent;
    if (AOriginParent <> nil) and AOriginFields.Contains(AOriginParent) then
    begin
      AParentIndex := AOriginFields.IndexOf(AOriginParent);
      ANewFields[I].Parent := ANewFields[AParentIndex];
    end
    else
      ANewFields[I].Parent := AParentField;
  end;

end;

procedure TdxCopyFieldsOperation.ChangeRunInfo(ARunInfo: TdxRunInfo;
  AField: TdxField);
begin
  if ShouldExtendInterval(ARunInfo, AField) then
    EnsureIntervalContainsField(ARunInfo, AField);
  if AField.Parent <> nil then
    ChangeRunInfo(ARunInfo, AField.Parent);
end;

procedure TdxCopyFieldsOperation.Execute(ARunInfo: TdxRunInfo;
  ARunIndex: TdxRunIndex);
var
  ASourceFields: TdxFieldList;
  AFields: TdxFieldList;
  AOffset, I, ACount: Integer;
  AParentFieldIndex, AIndexToInsert: Integer;
  AFieldsToDelete: TdxFieldList;
  AParent: TdxField;
  AHyperlinksAllowed: Boolean;
  AHyperlinkInfo: TdxHyperlinkInfo;
begin
  FreeAndNil(FUpdateFieldsParameters);
  ASourceFields := SourcePieceTable.GetEntireFieldsFromInterval(ARunInfo.Start.RunIndex, ARunInfo.&End.RunIndex);
  try
    if ASourceFields.Count = 0 then
      Exit;

    AOffset := ARunIndex - ARunInfo.Start.RunIndex;
    AFields := CalculateCopingFields(ASourceFields, AOffset);
    AParentFieldIndex := TargetPieceTable.FindFieldIndexByRunIndex(ARunIndex);
    CalculateFieldsHierarchy(ASourceFields, AFields, AParentFieldIndex);
    AIndexToInsert := GetIndexToInsert(AParentFieldIndex, ARunIndex);
    ACount := AFields.Count;
    AFieldsToDelete := TdxFieldList.Create;

    if AParentFieldIndex >= 0 then
      AParent := TargetPieceTable.Fields[AParentFieldIndex]
    else
      AParent := nil;

    AHyperlinksAllowed := TargetModel.DocumentCapabilities.HyperlinksAllowed;
    for I := 0 to ACount - 1 do
    begin
      AFields[I].Index := AIndexToInsert;
      TargetPieceTable.AddFieldToTable(AFields[I], AIndexToInsert);
      if SourcePieceTable.HyperlinkInfos.TryGetHyperlinkInfo(ASourceFields[I].Index, AHyperlinkInfo) then
      begin
        if AHyperlinksAllowed then
          TargetPieceTable.InsertHyperlinkInfo(AIndexToInsert, AHyperlinkInfo.Clone)
        else
          AFieldsToDelete.Add(AFields[I]);
      end;
      Inc(AIndexToInsert);
    end;
    FUpdateFieldsParameters := TdxUpdateFieldsParameters.Create(AFields, AFieldsToDelete, AParent);
  finally
    ASourceFields.Free;
  end;
end;

function TdxCopyFieldsOperation.GetIndexToInsert(AParentFieldIndex: Integer;
  ARunIndex: TdxRunIndex): Integer;
var
  AFields: TdxFieldCollection;
  I: Integer;
begin
  if AParentFieldIndex < 0 then
    Exit(not AParentFieldIndex);
  AFields := TargetPieceTable.Fields;
  Result := AParentFieldIndex;
  for I := AParentFieldIndex - 1 downto 0 do
  begin
    if ARunIndex > AFields[I].LastRunIndex then
      Exit;
    Result := I;
  end;
end;

function TdxCopyFieldsOperation.GetSourceModel: TdxDocumentModel;
begin
  Result := SourcePieceTable.DocumentModel;
end;

function TdxCopyFieldsOperation.GetTargetModel: TdxDocumentModel;
begin
  Result := TargetPieceTable.DocumentModel;
end;

procedure TdxCopyFieldsOperation.RecalculateRunInfo(ARunInfo: TdxRunInfo);
var
  ALeftCopiedField, ARigthCopiedField: TdxField;
begin
  ARigthCopiedField := SourcePieceTable.FindFieldByRunIndex(ARunInfo.&End.RunIndex);
  if ARigthCopiedField <> nil then
    ChangeRunInfo(ARunInfo, ARigthCopiedField);
  ALeftCopiedField := SourcePieceTable.FindFieldByRunIndex(ARunInfo.Start.RunIndex);
  if ALeftCopiedField <> nil then
    ChangeRunInfo(ARunInfo, ALeftCopiedField);
end;

function TdxCopyFieldsOperation.ShouldExtendInterval(ARunInfo: TdxRunInfo;
  AField: TdxField): Boolean;
begin
  Result := False;
  if IsEntireFieldAffected(ARunInfo, AField) or IsFieldCodeTextAffectedOnly(ARunInfo, AField) then
    Exit;
  if IsFieldResultTextAffectedOnly(ARunInfo, AField) then
    Result := not AllowCopyWholeFieldResult and IsEntireFieldResultAffected(ARunInfo, AField)
  else
    Result := True;
end;

procedure TdxCopyFieldsOperation.UpdateCopiedFields;
var
  AFieldsToDelete: TdxFieldList;
  AFields: TdxFieldList;
  AField: TdxField;
  I: Integer;
begin
  if FUpdateFieldsParameters = nil then
    Exit;
  AFieldsToDelete := FUpdateFieldsParameters.FieldsToDelete;
  AFields := FUpdateFieldsParameters.Fields;
  for I := AFieldsToDelete.Count - 1 downto 0 do
  begin
    AField := AFieldsToDelete[I];
    AFields.Remove(AField);
    TargetPieceTable.DeleteFieldWithoutResult(AField);
  end;
  if not SuppressFieldsUpdate then
    TargetPieceTable.FieldUpdater.UpdateFields(AFields, FUpdateFieldsParameters.Parent, UpdateFieldOperationType);
end;

{ TdxRichEditCommandsCreationStrategy }


function TdxRichEditCommandsCreationStrategy.CreateNumberingListIndexCalculator(AModel: TdxDocumentModel; ANumberingListType: TdxNumberingType): TdxNumberingListIndexCalculator;
begin
  Result := TdxNumberingListIndexCalculator.Create(AModel, ANumberingListType);
end;

{ TdxBufferedRegexSearchResult }

constructor TdxBufferedRegexSearchResult.Create(const ARegEx: TRegex; const AMatch: TMatch; const AOffset: TdxDocumentModelPosition);
begin
  inherited Create;
  FAbsolutePosition := -1;
  FRegEx := ARegEx;
  FMatch := AMatch;
  FOffset := AOffset;
end;

{ TdxSearchContext }

constructor TdxSearchContext.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FLogicalAction := TdxSearchLogicalAction.None;
  Assert(Assigned(APieceTable));
  FPieceTable := APieceTable;

  FSearchInfo := TdxSearchContextInfo.Create;
end;

destructor TdxSearchContext.Destroy;
begin
  FreeAndNil(FSearchInfo);
  FreeAndNil(FStartSelectionPosition);
  FreeAndNil(FEndSelectionPosition);
  FreeAndNil(FMatchInfo);
  inherited Destroy;
end;

function TdxSearchContext.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxSearchContext.GetIsExecuteLocked: Boolean;
begin
  Result := FSuspendCount > 0;
end;

function TdxSearchContext.GetStartOfSearch: Boolean;
begin
  Result := SearchState = TdxSearchState.FindStart;
end;

function TdxSearchContext.GetEndOfSearch: Boolean;
begin
  Result := SearchState = TdxSearchState.FindFinish;
end;

function TdxSearchContext.GetMatchCount: Integer;
begin
  Result := SearchInfo.MatchCount;
end;

function TdxSearchContext.GetReplaceCount: Integer;
begin
  Result := SearchInfo.ReplaceCount;
end;

function TdxSearchContext.GetStartOfIntervalSearch: Boolean;
begin
  Result := SearchInfo.StartOfIntervalSearch;
end;

function TdxSearchContext.GetSearchState: TdxSearchState;
begin
  Result := SearchInfo.SearchState;
end;

function TdxSearchContext.GetSearchScope: TdxSearchScope;
begin
  Result := SearchInfo.SearchScope;
end;

function TdxSearchContext.GetStartAt: TdxDocumentLogPosition;
begin
  Result := SearchInfo.LastResult;
end;

function TdxSearchContext.GetStartSelection: TdxDocumentLogPosition;
begin
  Result := StartSelectionAnchor.Position.LogPosition;
end;

function TdxSearchContext.GetEndSelection: TdxDocumentLogPosition;
begin
  Result := EndSelectionAnchor.Position.LogPosition;
end;

function TdxSearchContext.GetSearchInfo: TdxSearchContextInfo;
begin
  Result := FSearchInfo;
end;

procedure TdxSearchContext.BeginSearch(AAction: TdxSearchAction; ADirection: TdxDirection);
var
  ACurrentLogicalAction: TdxSearchLogicalAction;
begin
  if not IsExecuteLocked then
  begin
    FAction := AAction;
    FDirection := ADirection;
    ACurrentLogicalAction := LogicalActionsTable[AAction][ADirection];
    if FLogicalAction <> ACurrentLogicalAction then
    begin
      Initialize;
      FLogicalAction := ACurrentLogicalAction;
    end;
    DisableHandling;
  end;
  Inc(FSuspendCount);
end;

procedure TdxSearchContext.EndSearch;
begin
  Dec(FSuspendCount);
  if FSuspendCount = 0 then
    if EndOfSearch then
      Clear
    else
      EnableHandling;
end;

procedure TdxSearchContext.EnableHandling;
begin
  if not IsExecuteLocked then
    DocumentModel.InnerSelectionChanged.Add(OnSelectionChanged);
end;

procedure TdxSearchContext.DisableHandling;
begin
  DocumentModel.InnerSelectionChanged.Remove(OnSelectionChanged);
end;

procedure TdxSearchContext.OnSelectionChanged(ASender: TObject);
begin
  Clear;
  DisableHandling;
end;

procedure TdxSearchContext.SetMatchCount(const Value: Integer);
begin
  SearchInfo.MatchCount := Value;
end;

procedure TdxSearchContext.SetMatchInfo(const Value: TdxBufferedRegexSearchResult);
begin
  if FMatchInfo = Value then
    Exit;
  FMatchInfo.Free;
  FMatchInfo := Value;
end;

procedure TdxSearchContext.SetReplaceCount(const Value: Integer);
begin
  SearchInfo.ReplaceCount := Value;
end;

procedure TdxSearchContext.SetSearchScope(const Value: TdxSearchScope);
begin
  SearchInfo.SearchScope := Value;
end;

procedure TdxSearchContext.SetSearchState(const Value: TdxSearchState);
begin
  SearchInfo.SearchState := Value;
end;

procedure TdxSearchContext.SetStartAt(const Value: TdxDocumentLogPosition);
begin
  SearchInfo.LastResult := Value;
end;

procedure TdxSearchContext.SetStartOfIntervalSearch(const Value: Boolean);
begin
  SearchInfo.StartOfIntervalSearch := Value;
end;

procedure TdxSearchContext.StopSearching;
begin
  SearchState := TdxSearchState.FindFinish;
end;

procedure TdxSearchContext.ClearCore;
begin
  ResetSearchInfo;
  FreeAndNil(FStartSelectionPosition);
  FreeAndNil(FEndSelectionPosition);
  MatchInfo := nil;
end;

procedure TdxSearchContext.Clear;
begin
  ClearCore;
  FLogicalAction := TdxSearchLogicalAction.None;
end;

procedure TdxSearchContext.ResetSearchInfo;
begin
  FSearchInfo.Free;
  FSearchInfo := TdxSearchContextInfo.Create;
end;

procedure TdxSearchContext.Initialize;
begin
  ClearCore;
  FStartPos := DocumentModel.Selection.Interval.NormalizedStart^;
  StartSelectionAnchor := TdxDocumentModelPositionAnchor.Create(@FStartPos);
  FEndPos := DocumentModel.Selection.Interval.NormalizedEnd^;
  EndSelectionAnchor := TdxDocumentModelPositionAnchor.Create(@FEndPos);
end;

function TdxSearchContext.CreateEventArgs(const ASearchString: string; const AReplaceString: string): TdxSearchCompleteEventArgs;
var
  AArgs: TdxSearchCompleteEventArgs;
begin
  AArgs := TdxSearchCompleteEventArgs.Create(Action, Direction, SearchScope);
  AArgs.SetMatchCount(MatchCount);
  AArgs.SetReplaceCount(ReplaceCount);
  AArgs.SetEntireDocument(SearchState = TdxSearchState.FindFinish);
  AArgs.SetSearchString(ASearchString);
  AArgs.SetReplaceString(AReplaceString);
  Result := AArgs;
end;

procedure TdxSearchContext.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnParagraphInserted(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell,
      AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnParagraphInserted(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell,
      AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxSearchContext.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnParagraphRemoved(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnParagraphRemoved(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSearchContext.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnParagraphMerged(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnParagraphMerged(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSearchContext.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunInserted(APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunInserted(APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSearchContext.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunRemoved(APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunRemoved(APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSearchContext.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxSearchContext.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxSearchContext.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunSplit(APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunSplit(APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSearchContext.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunJoined(APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunJoined(APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxSearchContext.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunMerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunMerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSearchContext.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnRunUnmerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnRunUnmerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSearchContext.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnFieldRemoved(APieceTable, AFieldIndex);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnFieldRemoved(APieceTable, AFieldIndex);
end;

procedure TdxSearchContext.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if StartSelectionAnchor <> nil then
    StartSelectionAnchor.OnFieldInserted(APieceTable, AFieldIndex);
  if EndSelectionAnchor <> nil then
    EndSelectionAnchor.OnFieldInserted(APieceTable, AFieldIndex);
end;

{ TdxSpellCheckerInterval }

procedure TdxSpellCheckerInterval.OnChangedCore;
begin
end;

{ TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndLogPositionComparer }

constructor TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndLogPositionComparer.Create(APosition: TdxDocumentLogPosition);
begin
  inherited Create;
  FPosition := APosition;
end;

function TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndLogPositionComparer.CompareTo(const AOther: T): Integer;
begin
  if FPosition < AOther.Start then
    Exit(1);
  if FPosition >= AOther.&End then
    Exit(-1);
  Result := 0;
end;

{ TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndIntervalComparer }

constructor TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndIntervalComparer.Create(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
begin
  inherited Create;
  FStart := AStart;
  FEnd := AEnd;
end;

function TdxSpellCheckerIntervalCollection<T>.TSpellCheckerIntervalAndIntervalComparer.CompareTo(const AOther: T): Integer;
begin
  if FEnd <= AOther.Start then
    Exit(1);
  if FStart >= AOther.&End then
    Exit(-1);
  Result := 0;
end;

{ TdxSpellCheckerIntervalCollection<T> }

constructor TdxSpellCheckerIntervalCollection<T>.Create(APieceTable: TdxSimplePieceTable);
begin
  Assert(APieceTable <> nil);
  inherited Create;
  FPieceTable := APieceTable;
  FInnerList := TdxObjectList<T>.Create;
end;

destructor TdxSpellCheckerIntervalCollection<T>.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxSpellCheckerIntervalCollection<T>.GetCount: Integer;
begin
  Result := InnerList.Count;
end;

procedure TdxSpellCheckerIntervalCollection<T>.Clear;
begin
  InnerList.Clear;
end;

procedure TdxSpellCheckerIntervalCollection<T>.Add(ANewInterval: T);
var
  AIndex: Integer;
begin
  Assert(PieceTable = ANewInterval.PieceTable);
  AIndex := BinarySearch(ANewInterval.Start);
  if AIndex >= 0 then
    TdxRichEditExceptions.ThrowInternalException;
  AIndex := not AIndex;
  InnerList.Insert(AIndex, ANewInterval);
end;

procedure TdxSpellCheckerIntervalCollection<T>.Remove(AInterval: T);
begin
  RemoveCore(AInterval, True);
end;

function TdxSpellCheckerIntervalCollection<T>.GetIntervals(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition): TArray<T>;
var
  AStartIndex, AEndIndex: Integer;
  AList: TList<T>;
  I: Integer;
begin
  AStartIndex := BinarySearch(AStart);
  if AStartIndex < 0 then
    AStartIndex := not AStartIndex;

  AEndIndex := BinarySearch(Max(AEnd - 1, 0));
  if AEndIndex < 0 then
    AEndIndex := (not AEndIndex) - 1;

  if AStartIndex > AEndIndex then
    Exit(nil);
  AList := TList<T>.Create;
  try
    for I := AStartIndex to AEndIndex do
      AList.Add(InnerList[I]);
    Result := AList.ToArray;
  finally
    AList.Free;
  end;
end;

function TdxSpellCheckerIntervalCollection<T>.FindInterval(APosition: TdxDocumentLogPosition): T;
var
  AIndex: Integer;
  AInterval: T;
begin
  AIndex := BinarySearch(APosition);
  if AIndex >= 0 then
    Exit(InnerList[AIndex]);
  AIndex := not AIndex;
  Dec(AIndex);
  if AIndex < 0 then
    Exit(nil);
  AInterval := InnerList[AIndex];
  if AInterval.&End = APosition then
    Exit(AInterval);
  Result := nil;
end;

function TdxSpellCheckerIntervalCollection<T>.FindInterval(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition): T;
var
  AIndex: Integer;
begin
  AIndex := BinarySearch(AStart, AEnd);
  if AIndex >= 0 then
    Result := InnerList[AIndex]
  else
    Result := nil;
end;

procedure TdxSpellCheckerIntervalCollection<T>.RemoveRange(const ARange: TArray<T>);
begin
  RemoveRangeCore(ARange, True);
end;

procedure TdxSpellCheckerIntervalCollection<T>.RemoveRange(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
var
  AIntervals: TArray<T>;
begin
  AIntervals := GetIntervals(AStart, AEnd);
  if AIntervals <> nil then
    RemoveRange(AIntervals);
end;

procedure TdxSpellCheckerIntervalCollection<T>.Remove(APosition: TdxDocumentLogPosition);
var
  AInterval: T;
begin
  AInterval := FindInterval(APosition);
  if AInterval <> nil then
    Remove(AInterval);
end;

procedure TdxSpellCheckerIntervalCollection<T>.ExtractRange(const ARange: TArray<T>);
begin
  RemoveRangeCore(ARange, False);
end;

procedure TdxSpellCheckerIntervalCollection<T>.ExtractRange(AStart, AEnd: TdxDocumentLogPosition);
var
  AIntervals: TArray<T>;
begin
  AIntervals := GetIntervals(AStart, AEnd);
  if AIntervals <> nil then
    ExtractRange(AIntervals);
end;

procedure TdxSpellCheckerIntervalCollection<T>.Extract(AInterval: T);
begin
  RemoveCore(AInterval, False);
end;

procedure TdxSpellCheckerIntervalCollection<T>.Extract(APosition: TdxDocumentLogPosition);
var
  AInterval: T;
begin
  AInterval := FindInterval(APosition);
  if AInterval <> nil then
    Extract(AInterval);
end;

function TdxSpellCheckerIntervalCollection<T>.BinarySearch(APosition: TdxDocumentLogPosition): Integer;
var
  AComparer: TSpellCheckerIntervalAndLogPositionComparer;
begin
  AComparer := TSpellCheckerIntervalAndLogPositionComparer.Create(APosition);
  try
    if not TdxAlgorithms1<T>.BinarySearch(InnerList, AComparer, Result) then
      Result := not Result;
  finally
    AComparer.Free;
  end;
end;

function TdxSpellCheckerIntervalCollection<T>.BinarySearch(AStart, AEnd: TdxDocumentLogPosition): Integer;
var
  AComparer: TSpellCheckerIntervalAndIntervalComparer;
begin
  AComparer := TSpellCheckerIntervalAndIntervalComparer.Create(AStart, AEnd);
  try
    if not TdxAlgorithms1<T>.BinarySearch(InnerList, AComparer, Result) then
      Result := not Result;
  finally
    AComparer.Free;
  end;
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).ParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).ParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).ParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxSpellCheckerIntervalCollection<T>.OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSpellCheckerInterval(InnerList[I]).RunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSpellCheckerIntervalCollection<T>.RemoveRangeCore(const ARange: TArray<T>; ARangeDestroy: Boolean);
var
  ACount, AIndex: Integer;
begin
  ACount := Length(ARange);
  for AIndex := 0 to ACount - 1 do
  if ARangeDestroy then
    InnerList.Remove(ARange[AIndex])
  else
    InnerList.Extract(ARange[AIndex])
end;

procedure TdxSpellCheckerIntervalCollection<T>.RemoveCore(AInterval: T; AIntervalDestroy: Boolean);
begin
  if AIntervalDestroy then
    InnerList.Remove(AInterval)
  else
    InnerList.Extract(AInterval);
end;

{ TdxMisspelledInterval }

constructor TdxMisspelledInterval.Create(APieceTable: TdxCustomPieceTable; AErrorType: TdxSpellingError);
begin
  inherited Create(APieceTable);
  FErrorType := AErrorType;
end;

procedure TdxMisspelledInterval.OnChangedCore;
begin
  inherited OnChangedCore;
  Assert(Start < &End);
end;

{ TdxIgnoredList }

constructor TdxIgnoredList.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FIgnoreAllList := TdxStringList.Create;
end;

destructor TdxIgnoredList.Destroy;
begin
  FreeAndNil(FIgnoreAllList);
  inherited Destroy;
end;

function TdxIgnoredList.Contains(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition; const AWord: string): Boolean;
begin
  if Contains(AWord) then
    Exit(True);
  Result := FindInterval(AStart, AEnd) <> nil;
end;

function TdxIgnoredList.Contains(const AWord: string): Boolean;
begin
  Result := IgnoreAllList.Contains(AWord);
end;

function TdxIgnoredList.Remove(const AWord: string): Boolean;
begin
  Result := IgnoreAllList.Remove(AWord) >= 0;
end;

procedure TdxIgnoredList.Add(ANewInterval: TdxMisspelledInterval);
begin
  inherited Add(ANewInterval);
end;

procedure TdxIgnoredList.Add(const AStart, AEnd: TdxDocumentModelPosition);
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := TdxMisspelledInterval.Create(PieceTable, seUnknown);
  AInterval.SetEndCore(AEnd);
  AInterval.SetStartCore(AStart);
  Add(AInterval);
end;

procedure TdxIgnoredList.Add(const AWord: string);
begin
  IgnoreAllList.Add(AWord);
end;

procedure TdxIgnoredList.Clear;
begin
  inherited Clear;
  FIgnoreAllList.Clear;
end;

{ TdxMisspelledIntervalCollection }

procedure TdxMisspelledIntervalCollection.AddIfNotExists(ANewInterval: TdxMisspelledInterval);
var
  AIndex: Integer;
  AOldInterval: TdxMisspelledInterval;
begin
  Assert(PieceTable = ANewInterval.PieceTable);
  AIndex := BinarySearch(ANewInterval.Start);
  if AIndex < 0 then
  begin
    AIndex := not AIndex;
    InnerList.Insert(AIndex, ANewInterval);
  end
  else
  begin
    AOldInterval := InnerList[AIndex];
    if (AOldInterval.Start <> ANewInterval.Start) or (AOldInterval.&End <> ANewInterval.&End) then
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

{ TdxUncheckedIntervalCollection }

function TdxUncheckedIntervalCollection.ObtainIntervals(const AStart, AEnd: TdxDocumentModelPosition): TArray<TdxUncheckedInterval>;
var
  AStartIndex, AEndIndex: Integer;
  AList: TdxList<TdxUncheckedInterval>;
  I: Integer;
begin
  AStartIndex := BinarySearch(AStart.LogPosition);
  if AStartIndex < 0 then
    AStartIndex := not AStartIndex
  else
    if SplitInterval(AStartIndex, AStart) then
      Inc(AStartIndex);

  AEndIndex := BinarySearch(Max(AEnd.LogPosition - 1, 0));
  if AEndIndex < 0 then
    AEndIndex := (not AEndIndex) - 1
  else
    SplitInterval(AEndIndex, AEnd);

  if AStartIndex > AEndIndex then
    Exit(nil);
  AList := TdxList<TdxUncheckedInterval>.Create;
  try
    for I := AStartIndex to AEndIndex do
      AList.Add(InnerList[I]);
    Result := AList.ToArray;
  finally
    AList.Free;
  end;
end;

function TdxUncheckedIntervalCollection.SplitInterval(AIndex: Integer; const APosition: TdxDocumentModelPosition): Boolean;
var
  AOldInterval, ANewInterval: TdxUncheckedInterval;
begin
  AOldInterval := InnerList[AIndex];
  if (AOldInterval.Start >= APosition.LogPosition) or (APosition.LogPosition >= AOldInterval.&End) then
    Exit(False);
  ANewInterval := TdxUncheckedInterval.Create(PieceTable);
  ANewInterval.SetEndCore(AOldInterval.Interval.&End);
  ANewInterval.SetStartCore(APosition);
  InnerList.Insert(AIndex + 1, ANewInterval);
  AOldInterval.SetEndCore(APosition);
  Result := True;
end;

procedure TdxUncheckedIntervalCollection.Add(ANewInterval: TdxUncheckedInterval);
var
  AIntervals: TArray<TdxUncheckedInterval>;
  AIndex: Integer;
begin
  Assert(PieceTable = ANewInterval.PieceTable);
  AIntervals := ObtainIntervals(ANewInterval.Interval.Start, ANewInterval.Interval.&End);
  if AIntervals <> nil then
    RemoveRange(AIntervals);
  AIndex := BinarySearch(ANewInterval.Start);
  if AIndex >= 0 then
    TdxRichEditExceptions.ThrowInternalException;
  AIndex := not AIndex;
  InnerList.Insert(AIndex, ANewInterval);
  TryMergeIntervals(AIndex);
end;

procedure TdxUncheckedIntervalCollection.TryMergeIntervals(AIndex: Integer);
begin
  if (AIndex > 0) and CanMerge(AIndex - 1, AIndex) then
  begin
    Merge(AIndex - 1, AIndex);
    Dec(AIndex);
  end;
  if (AIndex < Count - 1) and CanMerge(AIndex, AIndex + 1) then
    Merge(AIndex, AIndex + 1);
end;

function TdxUncheckedIntervalCollection.CanMerge(AFirstIndex: Integer; ALastIndex: Integer): Boolean;
begin
  Result := InnerList[AFirstIndex].&End = InnerList[ALastIndex].Start;
end;

procedure TdxUncheckedIntervalCollection.Merge(AFirstIndex: Integer; ALastIndex: Integer);
begin
  InnerList[AFirstIndex].SetEndCore(InnerList[ALastIndex].Interval.&End);
  InnerList.Delete(ALastIndex);
end;

procedure TdxUncheckedIntervalCollection.OnRunRemoved(AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    InnerList[I].RunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
    if InnerList[I].Length = 0 then
      InnerList.Delete(I);
  end;
end;

{ TdxRichEditSpellCheckerManager }

constructor TdxRichEditSpellCheckerManager.Create(APieceTable: TdxSimplePieceTable);
begin
  Assert(APieceTable <> nil);
  inherited Create;
  FPieceTable := APieceTable;
  FMisspelledIntervals := TdxMisspelledIntervalCollection.Create(APieceTable);
  FUncheckedIntervals := TdxUncheckedIntervalCollection.Create(APieceTable);
  FIgnoredIntervals := TdxIgnoredList.Create(PieceTable);
end;

destructor TdxRichEditSpellCheckerManager.Destroy;
begin
  if (PieceTable <> nil) and (PieceTable.DocumentModel <> nil) then
    UnhandleSelectionChanged;
  FreeAndNil(FMisspelledIntervals);
  FreeAndNil(FUncheckedIntervals);
  FreeAndNil(FIgnoredIntervals);
  inherited Destroy;
end;

function TdxRichEditSpellCheckerManager.CreateInstance(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager;
begin
  Result := TdxRichEditSpellCheckerManager.Create(APieceTable);
end;

procedure TdxRichEditSpellCheckerManager.Clear;
begin
  FShouldCheckDocument := True;

  ClearIntervals;
  ClearModifiedWordInfo;
end;

procedure TdxRichEditSpellCheckerManager.ClearIntervals;
begin
  MisspelledIntervals.Clear;
  UncheckedIntervals.Clear;
  IgnoredList.Clear;
end;

procedure TdxRichEditSpellCheckerManager.ClearModifiedWordInfo;
begin
  FModifiedWordStart := -1;
  FModifiedWordEnd := -1;
  UnhandleSelectionChanged;
end;

procedure TdxRichEditSpellCheckerManager.ResetModification;
begin
  if (ModifiedWordEnd > ModifiedWordStart) and (ModifiedWordEnd <= PieceTable.DocumentEndLogPosition) then
    ProcessModifiedWord;
  ClearModifiedWordInfo;
end;

procedure TdxRichEditSpellCheckerManager.Initialize;
begin
  InitializeUncheckedInterval;
end;

procedure TdxRichEditSpellCheckerManager.InitializeUncheckedInterval;
var
  AInterval: TdxUncheckedInterval;
begin
  UncheckedIntervals.Clear;
  AInterval := TdxUncheckedInterval.Create(PieceTable);
  AInterval.Start := PieceTable.DocumentStartLogPosition;
  AInterval.&End := PieceTable.DocumentEndLogPosition;
  UncheckedIntervals.Add(AInterval);
end;

function TdxRichEditSpellCheckerManager.PopUncheckedIntervals(const AStart, AEnd: TdxDocumentModelPosition): TArray<TdxSpellCheckerInterval>;
var
  AIntervals: TArray<TdxUncheckedInterval>;
  I: Integer;
begin
  AIntervals := UncheckedIntervals.ObtainIntervals(AStart, AEnd);
  if AIntervals <> nil then
    UncheckedIntervals.ExtractRange(AIntervals);
  SetLength(Result, Length(AIntervals));
  for I := 0 to Length(AIntervals) - 1 do
    Result[I] := AIntervals[I];
end;

procedure TdxRichEditSpellCheckerManager.RemoveMisspelledIntervals(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
var
  AIntervals: TArray<TdxMisspelledInterval>;
  ACount, I: Integer;
begin
  AIntervals := MisspelledIntervals.GetIntervals(AStart, AEnd);
  if (AIntervals = nil) or (Length(AIntervals) = 0) then
    Exit;
  if (AIntervals[0].Start = AStart) and (AIntervals[0].ErrorType = seRepeating) then
  begin
    ACount := Length(AIntervals);
    for I := 1 to ACount - 1 do
      MisspelledIntervals.Remove(AIntervals[I]);
  end
  else
    MisspelledIntervals.RemoveRange(AIntervals);
end;

procedure TdxRichEditSpellCheckerManager.CreateMisspelledInterval(const AStart, AEnd: TdxDocumentModelPosition; AErrorType: TdxSpellingError);
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := TdxMisspelledInterval.Create(PieceTable, AErrorType);
  AInterval.SetEndCore(AEnd);
  AInterval.SetStartCore(AStart);
  MisspelledIntervals.AddIfNotExists(AInterval);
end;

procedure TdxRichEditSpellCheckerManager.OnRunInserted(AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := TdxDocumentModelPosition.FromRunStart(PieceTable, ANewRunIndex);
  AEnd := TdxDocumentModelPosition.FromRunEnd(PieceTable, ANewRunIndex);

  MisspelledIntervals.Remove(AStart.LogPosition);
  IgnoredList.Remove(AStart.LogPosition);

  OnRunInsertedCore(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  CalculateUncheckedInterval(AStart, AEnd);
end;

procedure TdxRichEditSpellCheckerManager.OnRunInsertedCore(AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  MisspelledIntervals.OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  UncheckedIntervals.OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  IgnoredList.OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.CalculateUncheckedInterval(const AStart, AEnd: TdxDocumentModelPosition);
begin
  if IsRangeVisible(AStart, AEnd) then
  begin
    if IsRangeEditable(AStart, AEnd) then
    begin
      CalculateUncheckedIntervalCore(AStart, AEnd);
      CalculateModifiedWordInfo(AEnd);
    end;
  end
  else
    ResetModification;
end;

procedure TdxRichEditSpellCheckerManager.CalculateModifiedWordInfo(const AEnd: TdxDocumentModelPosition);
var
  AIterator: TdxSpellCheckerWordIterator;
  AWordStart, AWordEnd: TdxDocumentModelPosition;
  AIntervals: TArray<TdxUncheckedInterval>;
begin
  if PieceTable.DocumentModel.History.IsHistoryDisabled then
    Exit;
  AIterator := TdxSpellCheckerWordIterator.Create(PieceTable);
  try
    AWordStart := AEnd;
    AWordStart := AIterator.MoveToWordStart(AWordStart);
    AWordEnd := AIterator.MoveToWordEnd(AWordStart);
    FModifiedWordStart := AWordStart.LogPosition;
    FModifiedWordEnd := AWordEnd.LogPosition;
    AIntervals := UncheckedIntervals.ObtainIntervals(AWordStart, AWordEnd);
    if AIntervals <> nil then
    begin
      UncheckedIntervals.RemoveRange(AIntervals);
      HandleSelectionChanged;
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxRichEditSpellCheckerManager.HandleSelectionChanged;
begin
  UnhandleSelectionChanged;
  PieceTable.DocumentModel.InnerSelectionChanged.Add(OnSelectionChanged);
end;

procedure TdxRichEditSpellCheckerManager.UnhandleSelectionChanged;
begin
  PieceTable.DocumentModel.InnerSelectionChanged.Remove(OnSelectionChanged);
end;

function TdxRichEditSpellCheckerManager.IsModifiedWord(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
begin
  Result := (ModifiedWordStart = AStart.LogPosition) and (ModifiedWordEnd = AEnd.LogPosition);
end;

procedure TdxRichEditSpellCheckerManager.OnSelectionChanged(ASender: TObject);
var
  ACaretPosition: TdxDocumentLogPosition;
begin
  if PieceTable <> PieceTable.DocumentModel.Selection.PieceTable then
    Exit;
  ACaretPosition := PieceTable.DocumentModel.Selection.NormalizedEnd;
  if (PieceTable.DocumentModel.Selection.Length > 0) or (((ACaretPosition < ModifiedWordStart) or (ACaretPosition > ModifiedWordEnd))) then
  begin
    ProcessModifiedWord;
    ClearModifiedWordInfo;
  end;
end;

procedure TdxRichEditSpellCheckerManager.ProcessModifiedWord;

  function IsValid(ALogPosition: TdxDocumentLogPosition): boolean;
  begin
    Result := (ALogPosition >= FPieceTable.DocumentStartLogPosition) and
      (ALogPosition <= FPieceTable.DocumentEndLogPosition);
  end;

var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  if not IsValid(ModifiedWordStart) or not IsValid(ModifiedWordEnd) then
    Exit;

  AStart := TdxPositionConverter.ToDocumentModelPosition(PieceTable, ModifiedWordStart);
  AEnd := TdxPositionConverter.ToDocumentModelPosition(PieceTable, ModifiedWordEnd);

  if not IsRangeVisible(AStart, AEnd) or not IsRangeEditable(AStart, AEnd) then
    Exit;

  CalculateUncheckedIntervalCore(AStart, AEnd);
  PieceTable.DocumentModel.ResetSpellCheck(AStart.RunIndex, AEnd.RunIndex, True);
end;

function TdxRichEditSpellCheckerManager.IsRangeEditable(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
var
  APieceTable: TdxPieceTable;
begin
  APieceTable := Safe<TdxPieceTable>.Cast(PieceTable);
  Result := (APieceTable = nil) or APieceTable.CanEditRange(AStart.LogPosition, AEnd.LogPosition);
end;

function TdxRichEditSpellCheckerManager.IsRangeVisible(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
var
  ATextFilter: IdxVisibleTextFilter;
  AStartRunIndex, AEndRunIndex, AIndex: TdxRunIndex;
begin
  ATextFilter := PieceTable.VisibleTextFilter;
  AStartRunIndex := AStart.RunIndex;
  AEndRunIndex := AEnd.RunIndex;
  if AStartRunIndex = AEndRunIndex then
    Exit(ATextFilter.IsRunVisible(AStartRunIndex));
  if AEnd.RunOffset = 0 then
  begin
    Dec(AEndRunIndex);
    if AEndRunIndex = AStartRunIndex then
      Exit(ATextFilter.IsRunVisible(AStartRunIndex));
  end;
  for AIndex := AStartRunIndex to AEndRunIndex do
  begin
    if ATextFilter.IsRunVisible(AIndex) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxRichEditSpellCheckerManager.CalculateUncheckedIntervalCore(const AStart, AEnd: TdxDocumentModelPosition);
var
  AUncheckedInterval: TdxUncheckedInterval;
  AIterator: TdxSpellCheckerWordIterator;
  AIntervals: TArray<TdxUncheckedInterval>;
  APos: TdxDocumentModelPosition;
begin
  AUncheckedInterval := TdxUncheckedInterval.Create(PieceTable);
  AUncheckedInterval.SetEndCore(AEnd);
  AUncheckedInterval.SetStartCore(AStart);

  AIterator := TdxSpellCheckerWordIterator.Create(PieceTable);
  try
    APos := AUncheckedInterval.Interval.Start;
    AIterator.MoveToPrevWordStart(APos);
    AUncheckedInterval.Interval.Start.CopyFrom(APos);
    APos := AUncheckedInterval.Interval.&End;
    AIterator.MoveToNextWordEnd(APos);
    AUncheckedInterval.Interval.&End.CopyFrom(APos);
  finally
    AIterator.Free;
  end;

  AIntervals := UncheckedIntervals.ObtainIntervals(AUncheckedInterval.Interval.Start, AUncheckedInterval.Interval.&End);
  if AIntervals <> nil then
    UncheckedIntervals.RemoveRange(AIntervals);
  UncheckedIntervals.Add(AUncheckedInterval);
end;

procedure TdxRichEditSpellCheckerManager.OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  APosition: TdxDocumentModelPosition;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  APosition := TdxDocumentModelPosition.FromRunStart(PieceTable, ARunIndex);

  AStart := APosition.LogPosition;
  AEnd := AStart + ALength;
  MisspelledIntervals.RemoveRange(AStart, AEnd);
  IgnoredList.RemoveRange(AStart, AEnd);

  OnRunRemovedCore(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  CalculateUncheckedInterval(APosition, APosition);
end;

procedure TdxRichEditSpellCheckerManager.OnRunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  MisspelledIntervals.OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  UncheckedIntervals.OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  IgnoredList.OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := TdxDocumentModelPosition.FromRunStart(PieceTable, ARunIndex);
  AStart.LogPosition := AStart.LogPosition + (PieceTable.Runs[ARunIndex].Length - ADeltaRunLength);
  AEnd := TdxDocumentModelPosition.FromRunEnd(PieceTable, ARunIndex);

  MisspelledIntervals.Remove(AStart.LogPosition);
  IgnoredList.Remove(AStart.LogPosition);

  OnRunMergedCore(AParagraphIndex, ARunIndex, ADeltaRunLength);
  CalculateUncheckedInterval(AStart, AEnd);
end;

procedure TdxRichEditSpellCheckerManager.OnRunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  MisspelledIntervals.OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  UncheckedIntervals.OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  IgnoredList.OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  MisspelledIntervals.OnParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  UncheckedIntervals.OnParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  IgnoredList.OnParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  MisspelledIntervals.OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  UncheckedIntervals.OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  IgnoredList.OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  MisspelledIntervals.OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  UncheckedIntervals.OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
  IgnoredList.OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  MisspelledIntervals.OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
  UncheckedIntervals.OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
  IgnoredList.OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxRichEditSpellCheckerManager.OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  MisspelledIntervals.OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  UncheckedIntervals.OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  IgnoredList.OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxRichEditSpellCheckerManager.OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  APosition: TdxDocumentModelPosition;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  APosition := TdxDocumentModelPosition.FromRunStart(PieceTable, ARunIndex);

  AStart := APosition.LogPosition + FPieceTable.Runs[ARunIndex].Length;
  AEnd := AStart - ADeltaRunLength;

  MisspelledIntervals.RemoveRange(AStart, AEnd);
  IgnoredList.RemoveRange(AStart, AEnd);

  MisspelledIntervals.OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  IgnoredList.OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  UncheckedIntervals.OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);

  CalculateUncheckedInterval(APosition, APosition);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;
  OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRichEditSpellCheckerManager.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxRichEditSpellCheckerManager.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxRichEditSpellCheckerManager.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxRichEditSpellCheckerManager.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if APieceTable <> PieceTable then
    Exit;

  OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxRichEditSpellCheckerManager.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxRichEditSpellCheckerManager.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxRichEditSpellCheckerManager.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxRichEditSpellCheckerManager.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

{ TdxEmptySpellCheckerManager }

function TdxEmptySpellCheckerManager.CreateInstance(APieceTable: TdxPieceTable): TdxRichEditSpellCheckerManager;
begin
  Result := TdxEmptySpellCheckerManager.Create(APieceTable);
end;

procedure TdxEmptySpellCheckerManager.Clear;
begin
end;

procedure TdxEmptySpellCheckerManager.Initialize;
begin
end;

procedure TdxEmptySpellCheckerManager.OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
end;

procedure TdxEmptySpellCheckerManager.OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

{ TdxCustomMark }

constructor TdxCustomMark.Create(const APos: PdxDocumentModelPosition; AUserData: TObject);
begin
  inherited Create(APos);
  FUserData := AUserData;
end;

end.
