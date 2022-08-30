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

unit dxRichEdit.DocumentModel.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

{.$DEFINE DXLOGGING}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses, dxGDIPlusClasses,
  dxRichEdit.NativeApi,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Options.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitTwipsConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.DocumentsToLayoutPixelsConverter,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.ServiceManager;

type
  TdxContentTypeBase = class;
  TdxRunBase = class;
  TdxParagraphBase = class;
  TdxCustomPieceTable = class;
  TdxCustomDocumentModel = class;

  TdxCommentViewInfo = class(TObject);
  TdxCommentViewInfoList = TList<TdxCommentViewInfo>;
  TdxCommentBoxCalculator = class(TObject);
  TdxCommentColorer = class(TObject);
  TdxCommentSizeCalculator = class(TObject);
  TdxComment = class(TObject);
  TdxCommentList = class(TList);
  TdxCommentCollection = class(TObject);

  TdxCustomMarkCollection = class(TObject);
  IdxHoverLayoutItem = class(TObject);
  TdxCustomMarkBoxCalculator = class(TObject);

  TdxRunVisibility = (
    Hidden,
    Visible,
    ForceVisible
  );

  TdxParagraphIteratorResult = (
    Success,
    RunFinished,
    Finished
  );

  TdxDocumentLayoutDetailsLevel = (
    None = -1,
    Page = 0,
    PageArea = 1,
    Column = 2,
    TableRow = 3,
    TableCell = 4,
    Row = 5,
    Box = 6,
    Character = 7,
    Max = MaxInt
  );

  TdxResetFormattingCacheType = (
    Character,
    Paragraph,
    All
  );

  TdxHitTestAccuracy = Cardinal;

  { TdxFormatterPosition }

  TdxFormatterPosition = record
    BoxIndex: Integer;
    Offset: Integer;
    RunIndex: TdxRunIndex;
    constructor Create(ARunIndex: TdxRunIndex; AOffset, ABoxIndex: Integer);
    procedure Init(ARunIndex: TdxRunIndex; AOffset, ABoxIndex: Integer);
    procedure OffsetRunIndex(ADelta: Integer);

    class function MaxValue: TdxFormatterPosition; static;

    class operator Equal(const A, B: TdxFormatterPosition): Boolean;
    class operator NotEqual(const A, B: TdxFormatterPosition): Boolean;
    class operator LessThan(const A, B: TdxFormatterPosition): Boolean;
    class operator LessThanOrEqual(const A, B: TdxFormatterPosition): Boolean;
    class operator GreaterThan(const A, B: TdxFormatterPosition): Boolean;
    class operator GreaterThanOrEqual(const A, B: TdxFormatterPosition): Boolean;
  end;

  TdxDocumentModelChangeAction = (
    Redraw,
    ResetPrimaryLayout,
    ResetAllPrimaryLayout,
    ResetSecondaryLayout,
    ResetSelectionLayout,
    ResetCaretInputPositionFormatting,
    RaiseSelectionChanged,
    RaiseContentChanged,
    ScrollToBeginOfDocument,
    RaiseEmptyDocumentCreated,
    RaiseDocumentLoaded,
    RaiseModifiedChanged,
    ResetUncheckedIntervals,
    ResetIgnoredList,
    ForceResize,
    ValidateSelectionInterval,
    PerformActionsOnIdle,
    SplitRunByCharset,
    ResetRuler,
    SuppressBindingsNotifications,
    ActivePieceTableChanged,
    RaiseDocumentProtectionChanged,
    ForceResetHorizontalRuler,
    ForceResetVerticalRuler,
    ForceSyntaxHighlight,
    ApplyAutoCorrect,
    SuppressRaiseContentChangedCalculationByCurrentTransactionChanges,
    Fields
  );
  TdxDocumentModelChangeActions = set of TdxDocumentModelChangeAction;

  TdxIndexBasedObject = class(TcxIUnknownObject)
  protected
    function GetDocumentModelPart: TdxCustomPieceTable; virtual; abstract;
    function GetIndex: Integer; virtual; abstract;
  public
    procedure SetIndex(AIndex: Integer; const AChangeActions: TdxDocumentModelChangeActions); virtual; abstract;

    property DocumentModelPart: TdxCustomPieceTable read GetDocumentModelPart;
  end;

  { TdxRunBase }

  TdxRunBase = class(TcxIUnknownObject)
  strict private
    FFontCacheIndex: Integer;
    FParagraph: TdxParagraphBase;
    FStartIndex: Integer;
    function GetEndIndex: Integer;
    function GetFontCacheIndex: Integer;
    function GetPieceTable: TdxCustomPieceTable;
    procedure SetParagraph(const Value: TdxParagraphBase);
    procedure SetStartIndex(const Value: Integer);
  strict protected
    FLength: Integer;
    function CalculateFontIndexCore: Integer; overload; virtual; abstract;
    function CanSetLength(const Value: Integer): Boolean; virtual;
    function GetLength: Integer; virtual;
    procedure SetLength(const Value: Integer); virtual;

    procedure ResetCachedIndicesCore; virtual;
    property InnerFontCacheIndex: Integer read FFontCacheIndex write FFontCacheIndex;
  public
    constructor Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1); virtual;

    function GetRunIndex: TdxRunIndex;
    procedure ResetFontCacheIndex;

    property EndIndex: Integer read GetEndIndex;
    property FontCacheIndex: Integer read GetFontCacheIndex;
    property Length: Integer read GetLength write SetLength;
    property Paragraph: TdxParagraphBase read FParagraph write SetParagraph;
    property PieceTable: TdxCustomPieceTable read GetPieceTable;
    property StartIndex: Integer read FStartIndex write SetStartIndex;
  end;

  { TdxRunCollection }

  TdxRunCollection = class
  strict private
    FList: TdxFastObjectList;
    function GetItem(Index: TdxRunIndex): TdxRunBase;
    function GetCount: Integer;
    procedure SetItem(Index: TdxRunIndex; const Value: TdxRunBase);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AItem: TdxRunBase): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Extract(AIndex: Integer): TdxRunBase;
    function IndexOf(AItem: TdxRunBase): Integer;
    procedure Insert(AIndex: Integer; AItem: TdxRunBase);
    function First: TdxRunBase;
    function Last: TdxRunBase;

    property Count: Integer read GetCount;
    property Items[Index: TdxRunIndex]: TdxRunBase read GetItem write SetItem; default;
  end;

  { TdxParagraphBase }

  TdxParagraphBase = class abstract(TcxIUnknownObject)
  strict private
    FLength: Integer;
    FPieceTable: TdxCustomPieceTable;
    function GetIsEmpty: Boolean;
    function GetIsLast: Boolean;
  strict protected
    function GetFirstRunIndex: TdxRunIndex; virtual; abstract;
    function GetEndLogPosition: TdxDocumentLogPosition; virtual; abstract;
    function GetIndex: TdxParagraphIndex; virtual; abstract;
    function GetLastRunIndex: TdxRunIndex; virtual; abstract;
    function GetLogPosition: TdxDocumentLogPosition; virtual; abstract;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;

    function GetRunIndex(ARun: TdxRunBase): TdxRunIndex;
    function GetListLevelIndex: Integer; virtual;
    function IsInList: Boolean; virtual;

    property EndLogPosition: TdxDocumentLogPosition read GetEndLogPosition;
    property FirstRunIndex: TdxRunIndex read GetFirstRunIndex;
    property Index: TdxParagraphIndex read GetIndex;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsLast: Boolean read GetIsLast;
    property LastRunIndex: TdxRunIndex read GetLastRunIndex;
    property Length: Integer read FLength write FLength;
    property LogPosition: TdxDocumentLogPosition read GetLogPosition;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
  end;

  { TdxParagraphBaseCollection }

  TdxParagraphBaseCollection = class abstract
  strict protected
    function GetCount: Integer; virtual; abstract;
    function GetItemBase(Index: Integer): TdxParagraphBase; virtual; abstract;
  public
    procedure Clear; virtual; abstract;

    function SearchByLogPosition(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex; virtual; abstract;
    function GetFirstCore: TdxParagraphBase; virtual; abstract;
    function GetLastCore: TdxParagraphBase; virtual; abstract;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxParagraphBase read GetItemBase; default;
  end;

  { TdxDocumentModelPosition }

  PdxDocumentModelPosition = ^TdxDocumentModelPosition;

  TdxDocumentModelPosition = record
  strict private
    FLogPosition: TdxDocumentLogPosition;
    FParagraphIndex: TdxParagraphIndex;
    FPieceTable: TdxCustomPieceTable;
    FRunIndex: TdxRunIndex;
    FRunStartLogPosition: TdxDocumentLogPosition;
    function GetRunOffset: Integer;
    function GetIsValid: Boolean;
    function GetRunEndLogPosition: TdxDocumentLogPosition;
    procedure SetLogPosition(Value: TdxDocumentLogPosition);
    procedure SetParagraphIndex(Value: TdxParagraphIndex);
    procedure SetRunIndex(Value: TdxRunIndex);
    procedure SetRunStartLogPosition(Value: TdxDocumentLogPosition);
  public
    constructor Create(APieceTable: TdxCustomPieceTable);
    class operator Equal(const A, B: TdxDocumentModelPosition): Boolean;
    class operator NotEqual(const A, B: TdxDocumentModelPosition): Boolean;
    class operator LessThan(const A, B: TdxDocumentModelPosition): Boolean;
    class operator LessThanOrEqual(const A, B: TdxDocumentModelPosition): Boolean;
    class operator GreaterThan(const A, B: TdxDocumentModelPosition): Boolean;
    class operator GreaterThanOrEqual(const A, B: TdxDocumentModelPosition): Boolean;

    class function FromDocumentEnd(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; static;
    class function FromRunEnd(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex): TdxDocumentModelPosition; static;
    class function FromRunStart(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex): TdxDocumentModelPosition; static;
    class function FromParagraphEnd(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex): TdxDocumentModelPosition; static;
    class function FromParagraphStart(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex): TdxDocumentModelPosition; static;

    function GetHashCode: Integer;
    class procedure SetParagraphStart(var APos: TdxDocumentModelPosition; AParagraphIndex: TdxParagraphIndex); static;
    class procedure SetParagraphEnd(APos: PdxDocumentModelPosition; AParagraphIndex: TdxParagraphIndex); static;
    class procedure SetRunStart(var APos: TdxDocumentModelPosition; ARunIndex: TdxRunIndex); static;
    class procedure SetRunEnd(var APos: TdxDocumentModelPosition; ARunIndex: TdxRunIndex); static;
    class function MoveBackward(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; static;
    class procedure MoveBackwardCore(const APos: TdxDocumentModelPosition); static;
    class function MoveForward(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; static;
    class procedure MoveForwardCore(var APos: TdxDocumentModelPosition); static;
    procedure Update;

    procedure CopyFrom(const APos: TdxDocumentModelPosition);
    procedure Invalidate;
    class function Null: TdxDocumentModelPosition; static;
    class function MaxValue: TdxDocumentModelPosition; static;

    property IsValid: Boolean read GetIsValid;
    property LogPosition: TdxDocumentLogPosition read FLogPosition write SetLogPosition;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex write SetParagraphIndex;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property RunEndLogPosition: TdxDocumentLogPosition read GetRunEndLogPosition;
    property RunIndex: TdxRunIndex read FRunIndex write SetRunIndex;
    property RunOffset: Integer read GetRunOffset;
    property RunStartLogPosition: TdxDocumentLogPosition read FRunStartLogPosition write SetRunStartLogPosition;
  end;

  { TdxRunInfo }

  TdxRunInfo = class sealed
  private
    FStart: TdxDocumentModelPosition;
    FEnd: TdxDocumentModelPosition;
    function GetNormalizedStart: PdxDocumentModelPosition;
    function GetNormalizedEnd: PdxDocumentModelPosition;
  protected
    function CreateEmptyClone: TdxRunInfo;
    function CreateDocumentModelPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
    procedure SetStartAnchorCore(const AValue: TdxDocumentModelPosition);
    procedure SetEndAnchorCore(const AValue: TdxDocumentModelPosition);
  public
    constructor Create(APieceTable: TdxCustomPieceTable);

    function Clone: TdxRunInfo;
    procedure CopyFrom(AValue: TdxRunInfo);
    function Equals(Obj: TObject): Boolean; override; final;
    function GetHashCode: Integer; override; final;

    property Start: TdxDocumentModelPosition read FStart;
    property &End: TdxDocumentModelPosition read FEnd;
    property NormalizedStart: PdxDocumentModelPosition read GetNormalizedStart;
    property NormalizedEnd: PdxDocumentModelPosition read GetNormalizedEnd;
  end;

  { TdxPositionConverter }

  TdxPositionConverter = class
  public
    class function ToDocumentModelPosition(APieceTable: TdxCustomPieceTable; APos: TdxDocumentLogPosition): TdxDocumentModelPosition; overload; static;
    class function ToDocumentModelPosition(APieceTable: TdxCustomPieceTable; APos: TdxFormatterPosition): TdxDocumentModelPosition; overload; static;
    class function GetRunStartLogPosition(ARunIndex: TdxRunIndex; AParagraph: TdxParagraphBase): TdxDocumentLogPosition; static;
    class function ToFormatterPosition(const APos: TdxDocumentModelPosition): TdxFormatterPosition; overload; static;
    class function ToFormatterPosition(APieceTable: TdxCustomPieceTable; APos: TdxDocumentLogPosition): TdxFormatterPosition; overload; static;
  end;

  { TdxSectionPropertyModifierBase }

  TdxSectionPropertyModifierBase = class abstract
  public
    procedure ModifySection(ASection: TObject; ASectionIndex: TdxSectionIndex); virtual; abstract;
  end;

  { TdxCustomPieceTable }

  TdxCustomPieceTable = class abstract(TcxIUnknownObject)
  strict private
    FContentType: TdxContentTypeBase;
    FDocumentModel: TdxCustomDocumentModel;
    FParagraphs: TdxParagraphBaseCollection;
    FRuns: TdxRunCollection;
    function GetIsMain: Boolean;
    function GetParagraphCount: Integer;
  strict protected
    function CreateParagraphCollection: TdxParagraphBaseCollection; virtual; abstract;
    function CreateRuns: TdxRunCollection; virtual; abstract;

    function GetDocumentStartLogPosition: TdxDocumentLogPosition; virtual; abstract;
    function GetDocumentEndLogPosition: TdxDocumentLogPosition; virtual; abstract;
    function GetIsEmpty: Boolean; virtual; abstract;
  protected
    property ParagraphCount: Integer read GetParagraphCount;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase); virtual;
    destructor Destroy; override;

    procedure AddPieceTables(AResult: TdxFastList; AIncludeUnreferenced: Boolean); virtual; abstract;
    procedure ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex, AEndRunIndex: TdxRunIndex);
    procedure ApplyChangesCore(const Actions: TdxDocumentModelChangeActions; AStartRunIndex, AEndRunIndex: Integer);
    procedure ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase);
    function CalculateRunIndex(ARun: TdxRunBase; ADefaultResultWhenNotFound: TdxRunIndex = 0): TdxRunIndex;
    procedure Clear; virtual;
    function FindParagraphIndex(ALogPosition: TdxDocumentLogPosition; AStrictSearch: Boolean = True): TdxParagraphIndex;
    function FindRunStartLogPosition(AParagraph: TdxParagraphBase;
      ALogPosition: TdxDocumentLogPosition; out ARunIndex: TdxRunIndex): TdxDocumentLogPosition;
    function GetRunLogPosition(ARunIndex: TdxRunIndex): TdxDocumentLogPosition; overload;
    function GetRunLogPosition(ARun: TdxRunBase): TdxDocumentLogPosition; overload;

    procedure CalculateRunInfoStart(ALogPosition: TdxDocumentLogPosition; AResult: TdxRunInfo);
    procedure CalculateRunInfoEnd(ALogPosition: TdxDocumentLogPosition; AResult: TdxRunInfo);
    function FindRunInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxRunInfo;

    function CanEditRange(AStart, AEnd: TdxDocumentLogPosition): Boolean; virtual; abstract;
    procedure ResetParagraphs; overload;
    procedure ResetParagraphs(AFrom, ATo: TdxParagraphIndex); overload; virtual; abstract;

    function FindParagraphIndexCore(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;

    property ContentType: TdxContentTypeBase read FContentType;
    property DocumentStartLogPosition: TdxDocumentLogPosition read GetDocumentStartLogPosition;
    property DocumentEndLogPosition: TdxDocumentLogPosition read GetDocumentEndLogPosition;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property Paragraphs: TdxParagraphBaseCollection read FParagraphs;
    property Runs: TdxRunCollection read FRuns;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsMain: Boolean read GetIsMain;
  end;

  { TdxHistoryItem }

  TdxHistoryItem = class abstract
  private
    FPieceTable: TdxCustomPieceTable;
    function GetDocumentModel: TdxCustomDocumentModel;
  protected
    procedure UndoCore; virtual; abstract;
    procedure RedoCore; virtual; abstract;
    function GetChangeModified: Boolean; virtual;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;
    procedure Execute; virtual;
    function NeedStoreSelection: Boolean; virtual;
    procedure Undo;
    procedure Redo;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property ChangeModified: Boolean read GetChangeModified;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
  end;

  TdxHistoryItemList = TdxObjectList<TdxHistoryItem>;

  { TdxCompositeHistoryItem }

  TdxCompositeHistoryItem = class(TdxHistoryItem)
  private
    FItems: TdxHistoryItemList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxHistoryItem;
  protected
    procedure UndoCore; override;
    procedure Clear; virtual;
    procedure RedoCore; override;
    function GetChangeModified: Boolean; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;
    procedure AddItem(AItem: TdxHistoryItem);

    property Self[Index: Integer]: TdxHistoryItem read GetItem; default;
    property Items: TdxHistoryItemList read FItems;
    property Count: Integer read GetCount;
  end;

  { TdxNotificationIdGenerator }

  TdxNotificationIdGenerator = class
  public const
    EmptyId = 0;
  private
    FLastId: Integer;
  public
    constructor Create;
    function GenerateId: Integer; virtual;
  end;

  { TdxDocumentHistory }

  TdxDocumentHistory = class
  public const
    ForceModifiedIndex = -2;
  private
    FDocumentModel: TdxCustomDocumentModel;
    FUnmodifiedIndex: Integer;
    FCurrentIndex: Integer;
    FIdGenerator: TdxNotificationIdGenerator;
    FTransactionLevel: Integer;
    FDisableCount: Integer;
    FSuppressRaiseOperationComplete: Boolean;
    FItems: TdxHistoryItemList;
    FPreviousModifiedValue: Boolean;
    FTransaction: TdxCompositeHistoryItem;
    FLastDisabledHistoryItem: TdxHistoryItem;

    FOnOperationCompleted: TdxEventHandler;
    FOnModifiedChanged: TdxEventHandler;
    FOnDestroying: TdxEventHandler;

    function GetCurrent: TdxHistoryItem;
    function GetItem(Index: Integer): TdxHistoryItem;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetCount: Integer;
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    function GetIsHistoryDisabled: Boolean;
    procedure ClearCore(ADisposeOnlyCutOffItems: Boolean);
    function CommitTransaction: TdxHistoryItem;
    procedure InternalAdd(AItem: TdxHistoryItem);
    procedure CutOffHistory;
    function CheckIsTransactionChangeModifiedForward(AIndex, AStartInnerIndex: Integer): Boolean;
    function CheckIsTransactionChangeModifiedBackward(AIndex, AStartInnerIndex: Integer): Boolean;
    procedure DisposeContent(ACutOffItemsOnly: Boolean);
  protected
    procedure SetTransaction(AValue: TdxCompositeHistoryItem);
    procedure SetTransactionLevel(AValue: Integer);
    procedure SetModifiedTextAppended(FForceRaiseModifiedChanged: Boolean); virtual;
    function CreateIdGenerator: TdxNotificationIdGenerator; virtual;
    procedure BeginTrackModifiedChanged; virtual;
    procedure EndTrackModifiedChanged; virtual;
    procedure RaiseModifiedChanged; virtual;
    procedure OnEndUndoCore; virtual;
    procedure UndoCore; virtual;
    procedure BeginUndoCurrent; virtual;
    procedure EndUndoCurrent; virtual;
    procedure RedoCore; virtual;
    function CommitAsSingleItem: TdxHistoryItem; virtual;
    function CommitAsSingleItemCore(ASingleItem: TdxHistoryItem): TdxHistoryItem; virtual;
    procedure OnCutOffHistory; virtual;
    function CreateCompositeHistoryItem: TdxCompositeHistoryItem; virtual;

    property Current: TdxHistoryItem read GetCurrent;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property UnmodifiedIndex: Integer read FUnmodifiedIndex write FUnmodifiedIndex;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetNotificationId: Integer;
    procedure SmartClear;
    procedure Clear;
    function Add(AItem: TdxHistoryItem): TdxHistoryItem; virtual;
    function BeginSyntaxHighlight: TdxHistoryItem; virtual;
    procedure EndSyntaxHighlight; virtual;
    function HasChangesInCurrentTransaction: Boolean; virtual;
    function IsModified(AUnmodifiedIndex, AUnmodifiedTransactionIndex: Integer): Boolean; overload;
    function IsModified(AUnmodifiedIndex: Integer): Boolean; overload;
    procedure Undo;
    procedure Redo;
    procedure DisableHistory;
    procedure EnableHistory;
    function BeginTransaction: TdxHistoryItem; virtual;
    function EndTransaction: TdxHistoryItem; virtual;
    procedure RaiseOperationCompleted; virtual;

    property IsHistoryDisabled: Boolean read GetIsHistoryDisabled;
    property Items: TdxHistoryItemList read FItems;
    property Self[Index: Integer]: TdxHistoryItem read GetItem; default;
    property Count: Integer read GetCount;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property Modified: Boolean read GetModified write SetModified;
    property Transaction: TdxCompositeHistoryItem read FTransaction;
    property TransactionLevel: Integer read FTransactionLevel;
    property OperationCompleted: TdxEventHandler read FOnOperationCompleted;
    property SuppressRaiseOperationComplete: Boolean read FSuppressRaiseOperationComplete write FSuppressRaiseOperationComplete;
    property ModifiedChanged: TdxEventHandler read FOnModifiedChanged;
    property Destroying: TdxEventHandler read FOnDestroying;
  end;

  { TdxEmptyNotificationIdGenerator }

  TdxEmptyNotificationIdGenerator = class(TdxNotificationIdGenerator)
  public
    function GenerateId: Integer; override;
  end;

  { TdxEmptyHistory }

  TdxEmptyHistory = class(TdxDocumentHistory)
  strict private
    FTransactionItemCount: Integer;
    FItems: TdxFastObjectList;
  protected
    function CreateIdGenerator: TdxNotificationIdGenerator; override;
    property Items: TdxFastObjectList read FItems;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;
    function Add(AItem: TdxHistoryItem): TdxHistoryItem; override;
    function BeginTransaction: TdxHistoryItem; override;
    function HasChangesInCurrentTransaction: Boolean; override;
  end;

  { TdxDisabledHistory }

  TdxDisabledHistory = class(TdxEmptyHistory)
  strict private
    FTransactionCount: Integer;
  public
    function Add(AItem: TdxHistoryItem): TdxHistoryItem; override;
    function BeginTransaction: TdxHistoryItem; override;
    function EndTransaction: TdxHistoryItem; override;
    function HasChangesInCurrentTransaction: Boolean; override;
  end;

  { TdxHistoryTransaction }

  TdxHistoryTransaction = class
  private
    FHistory: TdxDocumentHistory;
    FSuppressRaiseOperationComplete: Boolean;

    procedure EndHistoryTransaction;
    procedure HistoryDestroyingHandler(Sender: TObject; Args: TdxEventArgs);
  public
    constructor Create(AHistory: TdxDocumentHistory);
    destructor Destroy; override;

    property SuppressRaiseOperationComplete: Boolean read FSuppressRaiseOperationComplete write FSuppressRaiseOperationComplete;
  end;

  { TdxWebSettings }

  TdxWebSettings = class
  strict private
    FLeftMargin: Integer;
    FTopMargin: Integer;
    FRightMargin: Integer;
    FBottomMargin: Integer;
  public
    function IsBodyMarginsSet: Boolean;

    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    property TopMargin: Integer read FTopMargin write FTopMargin;
    property RightMargin: Integer read FRightMargin write FRightMargin;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
  end;

  { TdxCustomDocumentCache }

  TdxCustomDocumentCache = class abstract
  public
    procedure Initialize(ADocumentModel: TdxCustomDocumentModel); virtual; abstract;
  end;

  { TdxCustomDocumentModelDeferredChanges }

  TdxCustomDocumentModelDeferredChanges = class abstract(TcxIUnknownObject)
  strict private
    FDocumentModel: TdxCustomDocumentModel;
    FIsSetContentMode: Boolean;
    FSuppressSyntaxHighlight: Boolean;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); virtual;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property IsSetContentMode: Boolean read FIsSetContentMode write FIsSetContentMode;
    property SuppressSyntaxHighlight: Boolean read FSuppressSyntaxHighlight write FSuppressSyntaxHighlight;
  end;

  { TdxCustomSection }

  TdxCustomSection = class abstract
  strict private
    FDocumentModel: TdxCustomDocumentModel;
    FFirstParagraphIndex: TdxParagraphIndex;
    FLastParagraphIndex: TdxParagraphIndex;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); virtual;

    procedure CopyFrom(ASection: TdxCustomSection); virtual;

    procedure SubscribeInnerObjectsEvents; virtual; abstract;
    procedure UnsubscribeInnerObjectsEvents; virtual; abstract;
    procedure SubscribeHeadersFootersEvents; virtual; abstract;
    procedure UnsubscribeHeadersFootersEvents; virtual; abstract;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property FirstParagraphIndex: TdxParagraphIndex read FFirstParagraphIndex write FFirstParagraphIndex;
    property LastParagraphIndex: TdxParagraphIndex read FLastParagraphIndex write FLastParagraphIndex;
  end;
  TdxCustomSectionCollection = class(TdxObjectList<TdxCustomSection>);

  { TdxCustomDocumentModel }

  TdxCustomDocumentModel = class abstract(TdxDpiSupport,
    IdxBatchUpdateable,
    IdxBatchUpdateHandler,
    IdxServiceProvider,
    IdxServiceContainer,
    IdxFontsContainer)
  public const
    DefaultLayoutUnit = TdxDocumentLayoutUnit.Document;
  strict private class var
    class function GetDpi: Single; static;
    class function GetDpiX: Single; static;
    class function GetDpiY: Single; static;
  strict private
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FCache: TdxCustomDocumentCache;
    FContentTypes: TdxObjectList<TdxContentTypeBase>;
    FDeferredChanges: TdxCustomDocumentModelDeferredChanges;
    FDocumentCapabilities: TdxCustomDocumentCapabilitiesOptions;
    FServiceManager: TdxServiceManager;
    FHistory: TdxDocumentHistory;
    FIsDestroying: Boolean;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FLayoutUnitConverter: TdxDocumentLayoutUnitConverter;
    FLayoutUnit: TdxDocumentLayoutUnit;
    FLayoutUnitChanged: TdxEventHandler;
    FSections: TdxCustomSectionCollection;
    FSuppressPerformLayoutCount: Integer;
    function GetEnableFieldNames: Boolean;
    function GetFontCache: TdxFontCache;
    function GetIsUpdateLockedOrOverlapped: Boolean;
    function GetMaxFieldSwitchLength: Integer;
    procedure SetDocumentCapabilities(const Value: TdxCustomDocumentCapabilitiesOptions);
    procedure SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
    procedure RaiseLayoutUnitChanged;

    procedure OnDocumentCapabilitiesChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
  protected
    function GetDefaultLayoutUnit: TdxDocumentLayoutUnit; virtual;

    function CreateDocumentModelUnitConverter: TdxDocumentModelUnitConverter; virtual;
    function CreateDeferredChanges: TdxCustomDocumentModelDeferredChanges; virtual; abstract;
    function CreateServiceManager: TdxServiceManager; virtual;
    function CreateSectionCollection: TdxCustomSectionCollection; virtual; abstract;
    procedure DeleteDeferredChanges;
    procedure UpdateLayoutUnitConverter;
    procedure OnLayoutUnitChanged; virtual;

    //IdxBatchUpdateable part I
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;

    //IdxServiceProvider
    function GetService(const AServiceType: TdxServiceType): IInterface; overload;

    //IdxServiceContainer
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType; APromote: Boolean); overload;

    procedure ClearCore; virtual;
    procedure DisposeCore; virtual;
    function GetMainPart: TdxCustomPieceTable; virtual; abstract;

    procedure CreateDocumentObjects; virtual;
    procedure DestroyDocumentObjects; virtual;
    procedure Initialize; virtual;
    procedure CreateOptions; virtual;

    procedure SubscribeDocumentCapabilitiesOptionsEvents;
    procedure UnsubscribeDocumentCapabilitiesOptionsEvents;
    procedure SubscribeOptionsEvents; virtual;
    procedure SubscribeDocumentObjectsEvents; virtual;

    function CreateDocumentCapabilitiesOptions: TdxCustomDocumentCapabilitiesOptions; virtual; abstract;
    function CreateDocumentImportHelper: TObject; virtual; abstract;
    function CreateDocumentExportHelper(ADocumentFormat: TdxRichEditDocumentFormat): TObject; virtual; abstract;
    function CreateEmptySpellCheckerManager(APieceTable: TdxCustomPieceTable): TObject{TdxSpellCheckerManager}; virtual; abstract;
    function GetImportManagerService: IInterface; virtual; abstract;
    function GetExportManagerService: IInterface; virtual; abstract;

    function CreateDocumentCache: TdxCustomDocumentCache; virtual; abstract;

    procedure BeginClearDocument; virtual;
    procedure EndClearDocument; virtual;
    procedure ClearDocumentContent; virtual;

    function CreateDocumentHistory: TdxDocumentHistory; virtual;
    procedure DisposeHistory; virtual;
    procedure SubscribeHistoryEvents; virtual;
    procedure UnsubscribeHistoryEvents; virtual;
    procedure OnHistoryOperationCompleted(ASender: TObject; E: TdxEventArgs); virtual; abstract;
    procedure OnHistoryModifiedChanged(ASender: TObject; E: TdxEventArgs); virtual; abstract;

    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property ContentTypes: TdxObjectList<TdxContentTypeBase> read FContentTypes;
  public
    constructor Create(ADpiX, ADpiY: Single); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function AddHistoryItem(AItem: TdxHistoryItem): TdxHistoryItem; virtual; abstract;
    function AutodetectDocumentFormat(const AFileName: string; AUseFormatFallback: Boolean = True): TdxRichEditDocumentFormat; virtual; abstract;
    procedure ApplyChanges(APieceTable: TdxCustomPieceTable; AChangeType: TdxDocumentModelChangeType; AStartRunIndex, AEndRunIndex: TdxRunIndex); virtual; abstract;
    procedure ApplyChangesCore(APieceTable: TdxCustomPieceTable; AActions: TdxDocumentModelChangeActions; AStartRunIndex, AEndRunIndex: TdxRunIndex); virtual; abstract;
    procedure ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase); virtual; abstract;
    function CreatePieceTable(AContentType: TdxContentTypeBase): TdxCustomPieceTable; virtual; abstract;
    procedure ResetMerging; virtual;
    procedure Redo; virtual;
    procedure Undo; virtual;
    //IdxBatchUpdateable part II
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
    //IdxBatchUpdateHandler
    procedure OnBeginUpdate; virtual;
    procedure OnEndUpdate; virtual;
    procedure OnCancelUpdate; virtual;
    procedure OnFirstBeginUpdate; virtual;
    procedure OnLastEndUpdate; virtual;
    procedure OnLastCancelUpdate; virtual;
    procedure OnLastEndUpdateCore; virtual;

    function GetService<T: IInterface>: T; overload;
    procedure ReplaceService<T: IInterface>(const ANewService: T);

    procedure BeginSuppressPerformLayout;
    procedure EndSuppressPerformLayout;
    function SuppressPerformLayout: Boolean;

    procedure ClearDocument;
    procedure ClearDocumentCore; virtual;
    function GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList; virtual;
    procedure ResetDocumentFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType); virtual; abstract;

    //for internal use
    function CreateSection: TdxCustomSection; virtual; abstract;
    function FindSectionIndex(ALogPosition: TdxDocumentLogPosition; AStrictSearch: Boolean = True): TdxSectionIndex;
    function GetActivePieceTableCore: TdxCustomPieceTable; virtual;
    procedure SetActivePieceTable(APieceTable: TdxCustomPieceTable); overload; virtual;
    procedure SwitchToEmptyHistory(ADisposeHistory: Boolean); virtual;
    procedure SwitchToNormalHistory(ADisposeHistory: Boolean); virtual;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; virtual; abstract;

    property Cache: TdxCustomDocumentCache read FCache;
    property DeferredChanges: TdxCustomDocumentModelDeferredChanges read FDeferredChanges;
    property DocumentCapabilities: TdxCustomDocumentCapabilitiesOptions read FDocumentCapabilities write SetDocumentCapabilities;
    class property Dpi: Single read GetDpi;
    class property DpiX: Single read GetDpiX;
    class property DpiY: Single read GetDpiY;
    property EnableFieldNames: Boolean read GetEnableFieldNames;
    property FontCache: TdxFontCache read GetFontCache;
    property History: TdxDocumentHistory read FHistory;
    property IsDestroying: Boolean read FIsDestroying;
    property IsUpdateLockedOrOverlapped: Boolean read GetIsUpdateLockedOrOverlapped;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property LayoutUnit: TdxDocumentLayoutUnit read FLayoutUnit write SetLayoutUnit;
    property LayoutUnitConverter: TdxDocumentLayoutUnitConverter read FLayoutUnitConverter;
    property MainPart: TdxCustomPieceTable read GetMainPart;
    property MaxFieldSwitchLength: Integer read GetMaxFieldSwitchLength;
    property Sections: TdxCustomSectionCollection read FSections;
    property ToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read FToDocumentLayoutUnitConverter;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;

    property LayoutUnitChanged: TdxEventHandler read FLayoutUnitChanged;
  end;

  { TdxRequestSectionIndexEventArgs }

  TdxRequestSectionIndexEventArgs = class(TdxEventArgs)
  strict private
    FSectionIndex: TdxSectionIndex;
  public
    constructor Create;

    property SectionIndex: TdxSectionIndex read FSectionIndex write FSectionIndex;
  end;

  TdxRequestSectionIndexEvent = procedure(ASender: TObject; AArgs: TdxRequestSectionIndexEventArgs) of object;
  TdxRequestSectionIndexEventHandler = TdxMulticastMethod<TdxRequestSectionIndexEvent>;

  { TdxContentTypeBase }

  TdxContentTypeBase = class abstract
  strict private
    FOwnPieceTable: Boolean;
    FPieceTable: TdxCustomPieceTable;
    function GetDocumentModel: TdxCustomDocumentModel;
  protected
    function GetIsMain: Boolean; virtual;
    function GetIsHeaderFooter: Boolean; virtual;
    function GetIsFooter: Boolean; virtual;
    function GetIsHeader: Boolean; virtual;
    function GetIsNote: Boolean; virtual;
    function GetIsFootNote: Boolean; virtual;
    function GetIsEndNote: Boolean; virtual;
    function GetIsTextBox: Boolean; virtual;
    function GetIsComment: Boolean; virtual;
    function GetIsReferenced: Boolean; virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); overload;
    constructor Create(APieceTable: TdxCustomPieceTable); overload;
    destructor Destroy; override;

    procedure ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); virtual;
    procedure ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); virtual;
    procedure ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase); virtual;
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); virtual; abstract;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; virtual;
    procedure SetPageCount(APageCount: Integer); virtual;

    //for internal use
    function CreateSpellCheckerManager(APieceTable: TdxCustomPieceTable): TObject{TdxSpellCheckerManager}; virtual;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property IsMain: Boolean read GetIsMain;
    property IsHeaderFooter: Boolean read GetIsHeaderFooter;
    property IsFooter: Boolean read GetIsFooter;
    property IsHeader: Boolean read GetIsHeader;
    property IsNote: Boolean read GetIsNote;
    property IsFootNote: Boolean read GetIsFootNote;
    property IsEndNote: Boolean read GetIsEndNote;
    property IsTextBox: Boolean read GetIsTextBox;
    property IsComment: Boolean read GetIsComment;
    property IsReferenced: Boolean read GetIsReferenced;
  end;

implementation

uses
  Graphics, Math, RTLConsts,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Graphics;

type

  { TdxSectionAndLogPositionComparable }

  TdxSectionAndLogPositionComparable = class(TcxIUnknownObject, IdxComparable<TdxCustomSection>)
  strict private
    FLogPosition: Integer;
  public
    constructor Create(ALogPosition: Integer);
    function CompareTo(const ASection: TdxCustomSection): Integer;
    property LogPosition: Integer read FLogPosition;
  end;

{ TdxSectionAndLogPositionComparable }

constructor TdxSectionAndLogPositionComparable.Create(ALogPosition: Integer);
begin
  inherited Create;
  FLogPosition := ALogPosition;
end;

function TdxSectionAndLogPositionComparable.CompareTo(const ASection: TdxCustomSection): Integer;
var
  AFirstParagraphLogPosition: TdxDocumentLogPosition;
  ALastParagraphLogPosition: TdxDocumentLogPosition;
  ALastParagraph: TdxParagraphBase;
begin
  AFirstParagraphLogPosition := ASection.DocumentModel.MainPart.Paragraphs[ASection.FirstParagraphIndex].LogPosition;
  if LogPosition < AFirstParagraphLogPosition then
    Result := 1
  else
    if LogPosition > AFirstParagraphLogPosition then
    begin
      ALastParagraph := ASection.DocumentModel.MainPart.Paragraphs[ASection.LastParagraphIndex];
      ALastParagraphLogPosition := ALastParagraph.LogPosition;
      if LogPosition < (ALastParagraphLogPosition + ALastParagraph.Length) then
        Result := 0
      else
        Result := -1
    end
    else
      Result := 0;
end;

{ TdxFormatterPosition }

constructor TdxFormatterPosition.Create(ARunIndex: TdxRunIndex; AOffset, ABoxIndex: Integer);
begin
  Init(ARunIndex, AOffset, ABoxIndex);
end;

procedure TdxFormatterPosition.OffsetRunIndex(ADelta: Integer);
begin
  Inc(RunIndex, ADelta);
end;

class function TdxFormatterPosition.MaxValue: TdxFormatterPosition;
begin
  Result.RunIndex := MaxInt;
  Result.Offset := MaxInt;
  Result.BoxIndex := 0;
end;

procedure TdxFormatterPosition.Init(ARunIndex: TdxRunIndex; AOffset, ABoxIndex: Integer);
begin
  RunIndex := ARunIndex;
  Offset := AOffset;
  BoxIndex := ABoxIndex;
end;

class operator TdxFormatterPosition.Equal(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := (A.Offset = B.Offset) and (A.RunIndex = B.RunIndex);
end;

class operator TdxFormatterPosition.NotEqual(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := not (A = B);
end;

class operator TdxFormatterPosition.LessThan(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := (A.RunIndex < B.RunIndex) or ((A.RunIndex = B.RunIndex) and (A.Offset < B.Offset));
end;

class operator TdxFormatterPosition.LessThanOrEqual(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := not (A > B);
end;

class operator TdxFormatterPosition.GreaterThan(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := (A.RunIndex > B.RunIndex) or ((A.RunIndex = B.RunIndex) and (A.Offset > B.Offset));
end;

class operator TdxFormatterPosition.GreaterThanOrEqual(const A, B: TdxFormatterPosition): Boolean;
begin
  Result := not (A < B);
end;

{ TdxDocumentModelPosition }

constructor TdxDocumentModelPosition.Create(APieceTable: TdxCustomPieceTable);
begin
  Assert(APieceTable <> nil, 'APieceTable = nil');
  FPieceTable := APieceTable;
  FLogPosition := 0;
  FRunStartLogPosition := 0;
  FParagraphIndex := 0;
  FRunIndex := 0;
end;

procedure TdxDocumentModelPosition.SetLogPosition(Value: TdxDocumentLogPosition);
begin
  FLogPosition := Value;
end;

procedure TdxDocumentModelPosition.SetParagraphIndex(Value: TdxParagraphIndex);
begin
  FParagraphIndex := Value;
end;

procedure TdxDocumentModelPosition.SetRunIndex(Value: TdxRunIndex);
begin
  FRunIndex := Value;
end;

procedure TdxDocumentModelPosition.SetRunStartLogPosition(Value: TdxDocumentLogPosition);
begin
  FRunStartLogPosition := Value;
end;

procedure TdxDocumentModelPosition.CopyFrom(const APos: TdxDocumentModelPosition);
begin
  FLogPosition := APos.LogPosition;
  FParagraphIndex := APos.ParagraphIndex;
  FRunIndex := APos.RunIndex;
  FRunStartLogPosition := APos.RunStartLogPosition;
end;

procedure TdxDocumentModelPosition.Update;
var
  ARunIndex: TdxRunIndex;
begin
  ParagraphIndex := PieceTable.FindParagraphIndex(LogPosition);
  RunStartLogPosition := PieceTable.FindRunStartLogPosition(PieceTable.Paragraphs[ParagraphIndex],
    LogPosition, ARunIndex);
	RunIndex := ARunIndex;
end;

procedure TdxDocumentModelPosition.Invalidate;
begin
  FLogPosition := MinInt;
end;

class function TdxDocumentModelPosition.Null: TdxDocumentModelPosition;
begin
  Result.FLogPosition := MinInt
end;

class function TdxDocumentModelPosition.MaxValue;
begin
  Result.FPieceTable  := nil;
  Result.FLogPosition := MaxInt;
end;

function TdxDocumentModelPosition.GetHashCode: Integer;
begin
  Result := FLogPosition;
end;

function TdxDocumentModelPosition.GetIsValid: Boolean;
begin
  Result := LogPosition <> MinInt;
end;

class function TdxDocumentModelPosition.FromParagraphEnd(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  SetParagraphEnd(@Result, AParagraphIndex);
end;

class operator TdxDocumentModelPosition.Equal(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition = B.LogPosition;
end;

class operator TdxDocumentModelPosition.NotEqual(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition <> B.LogPosition;
end;

class operator TdxDocumentModelPosition.LessThan(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition < B.LogPosition;
end;

class operator TdxDocumentModelPosition.LessThanOrEqual(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition <= B.LogPosition;
end;

class operator TdxDocumentModelPosition.GreaterThan(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition > B.LogPosition;
end;

class operator TdxDocumentModelPosition.GreaterThanOrEqual(const A, B: TdxDocumentModelPosition): Boolean;
begin
  Result := A.LogPosition >= B.LogPosition;
end;

class function TdxDocumentModelPosition.FromParagraphStart(APieceTable: TdxCustomPieceTable;
  AParagraphIndex: TdxParagraphIndex): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  SetParagraphStart(Result, AParagraphIndex);
end;

class function TdxDocumentModelPosition.FromDocumentEnd(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  Result.LogPosition := APieceTable.DocumentEndLogPosition;
  Result.ParagraphIndex := APieceTable.ParagraphCount - 1;
  Result.RunIndex := APieceTable.Runs.Count - 1;
  Result.RunStartLogPosition := APieceTable.DocumentEndLogPosition;
end;

class function TdxDocumentModelPosition.FromRunEnd(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  SetRunEnd(Result, ARunIndex);
end;

class function TdxDocumentModelPosition.FromRunStart(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  SetRunStart(Result, ARunIndex);
end;

function TdxDocumentModelPosition.GetRunEndLogPosition: TdxDocumentLogPosition;
begin
  if FLogPosition = MaxInt then
    Result := MaxInt
  else
    Result := RunStartLogPosition + PieceTable.Runs[RunIndex].Length - 1;
end;

function TdxDocumentModelPosition.GetRunOffset: Integer;
begin
  Result := LogPosition - RunStartLogPosition;
end;

class procedure TdxDocumentModelPosition.SetParagraphEnd(APos: PdxDocumentModelPosition;
  AParagraphIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraphBase;
  APieceTable: TdxCustomPieceTable;
begin
  APieceTable := APos.PieceTable;
  AParagraph := APieceTable.Paragraphs[AParagraphIndex];
  APos^.LogPosition := AParagraph.LogPosition + AParagraph.Length;
  APos^.ParagraphIndex := AParagraphIndex;
  APos^.RunIndex := AParagraph.LastRunIndex;
  APos^.RunStartLogPosition := APos^.LogPosition - APieceTable.Runs[AParagraph.LastRunIndex].Length;
end;

class procedure TdxDocumentModelPosition.SetRunStart(var APos: TdxDocumentModelPosition; ARunIndex: TdxRunIndex);
var
  I, AOffset: Integer;
  APieceTable: TdxCustomPieceTable;
  AParagraph: TdxParagraphBase;
begin
  APieceTable := APos.PieceTable;
  AParagraph := APieceTable.Runs[ARunIndex].Paragraph;
  AOffset := 0;
  for I := AParagraph.FirstRunIndex to ARunIndex - 1 do
    Inc(AOffset, APieceTable.Runs[I].Length);
  APos.LogPosition := AParagraph.LogPosition + AOffset;
  APos.ParagraphIndex := AParagraph.Index;
  APos.RunIndex := ARunIndex;
  APos.RunStartLogPosition := APos.LogPosition;
end;

class procedure TdxDocumentModelPosition.SetRunEnd(var APos: TdxDocumentModelPosition; ARunIndex: TdxRunIndex);
var
  I, AOffset: Integer;
  APieceTable: TdxCustomPieceTable;
  AParagraph: TdxParagraphBase;
begin
  APieceTable := APos.PieceTable;
  AParagraph := APieceTable.Runs[ARunIndex].Paragraph;
  AOffset := 0;
  for I := AParagraph.FirstRunIndex to ARunIndex do
    Inc(AOffset, APieceTable.Runs[I].Length);
  APos.LogPosition := AParagraph.LogPosition + AOffset;
  APos.ParagraphIndex := AParagraph.Index;
  if AParagraph.LastRunIndex = ARunIndex then
    APos.ParagraphIndex := APos.ParagraphIndex + 1;
  APos.RunIndex := ARunIndex + 1;
  APos.RunStartLogPosition := APos.LogPosition;
end;

class function TdxDocumentModelPosition.MoveBackward(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  MoveBackwardCore(Result);
end;

class procedure TdxDocumentModelPosition.MoveBackwardCore(const APos: TdxDocumentModelPosition);
var
  ARun: TdxRunBase;
begin
  APos.LogPosition := APos.LogPosition - 1;
  if APos.LogPosition < APos.RunStartLogPosition then
  begin
    if APos.RunIndex > 0 then
    begin
      APos.RunIndex := APos.RunIndex - 1;
      ARun := APos.PieceTable.Runs[APos.RunIndex];
      APos.ParagraphIndex := ARun.Paragraph.Index;
      APos.RunStartLogPosition := APos.LogPosition - ARun.Length + 1;
    end
    else
      APos.LogPosition := APos.RunStartLogPosition;
  end;
end;

class function TdxDocumentModelPosition.MoveForward(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  MoveForwardCore(Result);
end;

class procedure TdxDocumentModelPosition.MoveForwardCore(var APos: TdxDocumentModelPosition);
begin
  APos.LogPosition := APos.LogPosition + 1;
  if APos.LogPosition > APos.RunEndLogPosition then
  begin
    APos.RunIndex := APos.RunIndex + 1;
    APos.ParagraphIndex := APos.PieceTable.Runs[APos.RunIndex].Paragraph.Index;
    APos.RunStartLogPosition := APos.LogPosition;
  end;
end;

class procedure TdxDocumentModelPosition.SetParagraphStart(var APos: TdxDocumentModelPosition;
  AParagraphIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraphBase;
  APieceTable: TdxCustomPieceTable;
begin
  APieceTable := APos.PieceTable;
  AParagraph := APieceTable.Paragraphs[AParagraphIndex];
  APos.LogPosition := AParagraph.LogPosition;
  APos.ParagraphIndex := AParagraphIndex;
  APos.RunIndex := AParagraph.FirstRunIndex;
  APos.RunStartLogPosition := APos.LogPosition;
end;

{ TdxRunInfo }

constructor TdxRunInfo.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FStart := CreateDocumentModelPosition(APieceTable);
  FEnd := CreateDocumentModelPosition(APieceTable);
end;

function TdxRunInfo.Clone: TdxRunInfo;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateEmptyClone;
  Result.CopyFrom(Self);
end;

procedure TdxRunInfo.CopyFrom(AValue: TdxRunInfo);
begin
  Start.CopyFrom(AValue.Start);
  &End.CopyFrom(AValue.&End);
end;

function TdxRunInfo.CreateDocumentModelPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
end;

function TdxRunInfo.CreateEmptyClone: TdxRunInfo;
begin
  Result := TdxRunInfo.Create(Start.PieceTable);
end;

function TdxRunInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxRunInfo;
begin
  Result := Obj is TdxRunInfo;
  if Result then
  begin
    AInfo := TdxRunInfo(Obj);
    Result := (AInfo.Start = Start) and
      (AInfo.&End = &End);
  end;
end;

function TdxRunInfo.GetHashCode: Integer;
begin
  Result := Start.GetHashCode and &End.GetHashCode;
end;

function TdxRunInfo.GetNormalizedEnd: PdxDocumentModelPosition;
begin
  Result := @FStart;
  if FStart.LogPosition < FEnd.LogPosition then
    Result := @FEnd;
end;

function TdxRunInfo.GetNormalizedStart: PdxDocumentModelPosition;
begin
  Result := @FStart;
  if FStart.LogPosition > FEnd.LogPosition then
    Result := @FEnd;
end;

procedure TdxRunInfo.SetEndAnchorCore(const AValue: TdxDocumentModelPosition);
begin
  FEnd := AValue;
end;

procedure TdxRunInfo.SetStartAnchorCore(const AValue: TdxDocumentModelPosition);
begin
  FStart := AValue;
end;

{ TdxPositionConverter }

class function TdxPositionConverter.ToDocumentModelPosition(APieceTable: TdxCustomPieceTable; APos: TdxDocumentLogPosition): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  Result.LogPosition := APos;
  Result.Update;
end;

class function TdxPositionConverter.ToDocumentModelPosition(APieceTable: TdxCustomPieceTable; APos: TdxFormatterPosition): TdxDocumentModelPosition;
var
  ARunIndex: TdxRunIndex;
  AParagraph: TdxParagraphBase;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  ARunIndex := APos.RunIndex;
  Result.RunIndex := ARunIndex;
  AParagraph := APieceTable.Runs[ARunIndex].Paragraph;
  Result.ParagraphIndex := AParagraph.Index;
  Result.RunStartLogPosition := GetRunStartLogPosition(ARunIndex, AParagraph);
  Result.LogPosition := Result.RunStartLogPosition + APos.Offset;
end;

class function TdxPositionConverter.GetRunStartLogPosition(ARunIndex: TdxRunIndex; AParagraph: TdxParagraphBase): TdxDocumentLogPosition;
var
  ARuns: TdxRunCollection;
  AFirstRunIndex, I: TdxRunIndex;
begin
  ARuns := AParagraph.PieceTable.Runs;
  AFirstRunIndex := AParagraph.FirstRunIndex;
  Result := AParagraph.LogPosition;
  for I := AFirstRunIndex to ARunIndex - 1 do
    Inc(Result, ARuns[I].Length);
end;

class function TdxPositionConverter.ToFormatterPosition(const APos: TdxDocumentModelPosition): TdxFormatterPosition;
begin
  Result := TdxFormatterPosition.Create(APos.RunIndex, APos.RunOffset, 0);
end;

class function TdxPositionConverter.ToFormatterPosition(APieceTable: TdxCustomPieceTable; APos: TdxDocumentLogPosition): TdxFormatterPosition;
var
  AModelPos: TdxDocumentModelPosition;
begin
  AModelPos := ToDocumentModelPosition(APieceTable, APos);
  Result := ToFormatterPosition(AModelPos);
end;

{ TdxRunBase }

constructor TdxRunBase.Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1);
begin
  inherited Create;
  StartIndex := AStartIndex;
  FLength := ALength;
  FParagraph := AParagraph;
  FFontCacheIndex := -1;
end;

function TdxRunBase.GetRunIndex: TdxRunIndex;
begin
  Result := Paragraph.GetRunIndex(Self);
  if Result < 0 then
    Result := Paragraph.PieceTable.Runs.IndexOf(Self);
  if Result < 0 then
    TdxRichEditExceptions.ThrowInternalException;
end;

function TdxRunBase.CanSetLength(const Value: Integer): Boolean;
begin
  Result := Value >= 0;
end;

function TdxRunBase.GetEndIndex: Integer;
begin
  Result := StartIndex + Length - 1;
end;

function TdxRunBase.GetFontCacheIndex: Integer;
begin
  if FFontCacheIndex < 0 then
    FFontCacheIndex := CalculateFontIndexCore;
  Result := FFontCacheIndex;
end;

function TdxRunBase.GetPieceTable: TdxCustomPieceTable;
begin
  Result := FParagraph.PieceTable;
end;

function TdxRunBase.GetLength: Integer;
begin
  Result := FLength;
end;

procedure TdxRunBase.SetLength(const Value: Integer);
begin
  Assert(CanSetLength(Value));
  FLength := Value;
end;

procedure TdxRunBase.ResetFontCacheIndex;
begin
  FFontCacheIndex := -1;
end;

procedure TdxRunBase.ResetCachedIndicesCore;
begin
  ResetFontCacheIndex;
end;

procedure TdxRunBase.SetParagraph(const Value: TdxParagraphBase);
begin
  if Paragraph <> Value then
  begin
    FParagraph := Value;
    ResetCachedIndicesCore;
  end;
end;

procedure TdxRunBase.SetStartIndex(const Value: Integer);
begin
  Assert(Value >= 0);
  FStartIndex := Value;
end;

{ TdxRunCollection }

constructor TdxRunCollection.Create;
begin
  inherited Create;
  FList := TdxFastObjectList.Create(True, 1024 * 16);
end;

destructor TdxRunCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxRunCollection.First: TdxRunBase;
begin
  Result := TdxRunBase(FList.First);
end;

function TdxRunCollection.Last: TdxRunBase;
begin
  Result := TdxRunBase(FList.Last);
end;

function TdxRunCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TdxRunCollection.SetItem(Index: TdxRunIndex; const Value: TdxRunBase);
begin
  FList[Index] := Value;
end;

function TdxRunCollection.GetItem(Index: TdxRunIndex): TdxRunBase;
begin
  Result := TdxRunBase(FList[Index]);
end;

function TdxRunCollection.Add(AItem: TdxRunBase): Integer;
begin
  Result := FList.Add(AItem);
end;

procedure TdxRunCollection.Clear;
begin
  FList.Clear;
end;

procedure TdxRunCollection.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TdxRunCollection.Extract(AIndex: Integer): TdxRunBase;
begin
  Result := TdxRunBase(FList.ExtractByIndex(AIndex));
end;

function TdxRunCollection.IndexOf(AItem: TdxRunBase): Integer;
begin
  Result := FList.IndexOf(AItem, ldFromEnd);
end;

procedure TdxRunCollection.Insert(AIndex: Integer; AItem: TdxRunBase);
begin
  FList.Insert(AIndex, AItem);
end;

{ TdxParagraphBase }

constructor TdxParagraphBase.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FLength := -1;
end;

function TdxParagraphBase.GetRunIndex(ARun: TdxRunBase): TdxRunIndex;
var
  AStartIndex, AEndIndex: TdxRunIndex;
  ARuns: TdxRunCollection;
  I: Integer;
begin
  AStartIndex := FirstRunIndex;
  AEndIndex := LastRunIndex;
  ARuns := PieceTable.Runs;
  for I := AEndIndex downto AStartIndex do
    if ARuns[I] = ARun then
      Exit(I);
  Result := -1;
end;

function TdxParagraphBase.GetListLevelIndex: Integer;
begin
  dxAbstractError;
  Result := -1;
end;

function TdxParagraphBase.IsInList: Boolean;
begin
  Result := False;
end;

function TdxParagraphBase.GetIsEmpty: Boolean;
begin
  Result := Length <= 1;
end;

function TdxParagraphBase.GetIsLast: Boolean;
begin
  Result := Index = PieceTable.Paragraphs.Count - 1;
end;

{ TdxCustomPieceTable }

constructor TdxCustomPieceTable.Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FContentType := AContentType;
  FRuns := CreateRuns;
  FParagraphs := CreateParagraphCollection;
end;

destructor TdxCustomPieceTable.Destroy;
begin
  FreeAndNil(FRuns);
  FreeAndNil(FParagraphs);
  inherited Destroy;
end;

procedure TdxCustomPieceTable.ApplyChanges(
  AChangeType: TdxDocumentModelChangeType; AStartRunIndex, AEndRunIndex: TdxRunIndex);
begin
  FContentType.ApplyChanges(AChangeType, AStartRunIndex, AEndRunIndex);
end;

procedure TdxCustomPieceTable.ApplyChangesCore(const Actions: TdxDocumentModelChangeActions; AStartRunIndex, AEndRunIndex: Integer);
begin
  FContentType.ApplyChangesCore(Actions, AStartRunIndex, AEndRunIndex);
end;

procedure TdxCustomPieceTable.ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase);
begin
  FContentType.ApplySectionFormatting(ALogPositionStart, ALength, AModifier);
end;

function TdxCustomPieceTable.CalculateRunIndex(ARun: TdxRunBase; ADefaultResultWhenNotFound: TdxRunIndex = 0): TdxRunIndex;
var
  AParagraph: TdxParagraphBase;
  AMaxRunIndex, AFirstRunIndex, ALastRunIndex: TdxRunIndex;
  I: Integer;
begin
  Result := ADefaultResultWhenNotFound;
  AParagraph := ARun.Paragraph;
  AFirstRunIndex := AParagraph.FirstRunIndex;
  ALastRunIndex := AParagraph.LastRunIndex;
  if AFirstRunIndex = -1 then
    Exit;
  AMaxRunIndex := FRuns.Count - 1;
  if AFirstRunIndex > AMaxRunIndex then
    Exit;
  ALastRunIndex := Min(ALastRunIndex, AMaxRunIndex);
  for I := AFirstRunIndex to ALastRunIndex do
    if Runs[I] = ARun then
      Exit(I);
end;

procedure TdxCustomPieceTable.Clear;
begin
  FParagraphs.Clear;
  FRuns.Clear;
end;

function TdxCustomPieceTable.FindParagraphIndex(ALogPosition: TdxDocumentLogPosition; AStrictSearch: Boolean = True): TdxParagraphIndex;
begin
  Result := FindParagraphIndexCore(ALogPosition);
  if Result < 0 then
  begin
    if Result = not Paragraphs.Count then
      Result := Paragraphs.Count - 1
    else
    begin
      TdxRichEditExceptions.ThrowArgumentException('logPosition', ALogPosition);
      Result := -1;
    end;
  end;
end;

function TdxCustomPieceTable.FindRunStartLogPosition(AParagraph: TdxParagraphBase;
  ALogPosition: TdxDocumentLogPosition; out ARunIndex: TdxRunIndex): TdxDocumentLogPosition;
var
  ALastRunIndex: TdxRunIndex;
  I: TdxRunIndex;
  ARun: TdxRunBase;
  ANextPos: TdxDocumentLogPosition;
begin
  if AParagraph.EndLogPosition = ALogPosition then
  begin
    ARunIndex := AParagraph.LastRunIndex;
    Exit(ALogPosition);
  end;
  Result := AParagraph.LogPosition;
  ALastRunIndex := AParagraph.LastRunIndex;
  for I := AParagraph.FirstRunIndex to ALastRunIndex do
  begin
    ARun := Runs[I];
    ANextPos := Result + ARun.Length;
    if (ALogPosition >= Result) and (ALogPosition < ANextPos) then
    begin
      ARunIndex := I;
      Exit(Result);
    end;
    Result := ANextPos;
  end;
  Assert(False, 'LogPosititon');
  ARunIndex := -1;
  Result := -1;
end;

function TdxCustomPieceTable.GetRunLogPosition(ARunIndex: TdxRunIndex): TdxDocumentLogPosition;
var
  I: Integer;
  ARun: TdxRunBase;
begin
  ARun := Runs[ARunIndex];
  Result := ARun.Paragraph.LogPosition;
  for I := ARun.Paragraph.FirstRunIndex to ARunIndex - 1 do
    Inc(Result, Runs[I].Length);
end;

function TdxCustomPieceTable.GetRunLogPosition(ARun: TdxRunBase): TdxDocumentLogPosition;
var
  I: Integer;
  AParagraph: TdxParagraphBase;
  AEndRunIndex: TdxRunIndex;
begin
  AParagraph := ARun.Paragraph;
  Result := AParagraph.LogPosition;
  AEndRunIndex := AParagraph.LastRunIndex;
  for I := AParagraph.FirstRunIndex to AEndRunIndex do
    if FRuns[I] <> ARun then
      Inc(Result, FRuns[I].Length)
    else
      Break;
end;

procedure TdxCustomPieceTable.CalculateRunInfoStart(ALogPosition: TdxDocumentLogPosition; AResult: TdxRunInfo);
var
  ARunIndex: TdxRunIndex;
begin
  AResult.Start.LogPosition := ALogPosition;
  ALogPosition := Min(ALogPosition, DocumentEndLogPosition);
  AResult.Start.ParagraphIndex := FindParagraphIndex(ALogPosition, True);
  AResult.Start.RunStartLogPosition := FindRunStartLogPosition(Paragraphs[AResult.Start.ParagraphIndex], ALogPosition,
    ARunIndex);
  AResult.Start.RunIndex := ARunIndex;
end;

procedure TdxCustomPieceTable.CalculateRunInfoEnd(ALogPosition: TdxDocumentLogPosition; AResult: TdxRunInfo);
var
  ARunIndex: TdxRunIndex;
begin
  AResult.&End.LogPosition := ALogPosition;
  ALogPosition := Min(ALogPosition, DocumentEndLogPosition);
  AResult.&End.ParagraphIndex := FindParagraphIndex(ALogPosition, true);
  AResult.&End.RunStartLogPosition := FindRunStartLogPosition(Paragraphs[AResult.&End.ParagraphIndex], ALogPosition,
    ARunIndex);
  AResult.&End.RunIndex := ARunIndex;
end;

function TdxCustomPieceTable.FindRunInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxRunInfo;
begin
  Result := TdxRunInfo.Create(Self);
  CalculateRunInfoStart(ALogPositionStart, Result);
  CalculateRunInfoEnd(ALogPositionStart + ALength - 1, Result);
end;

procedure TdxCustomPieceTable.ResetParagraphs;
begin
  ResetParagraphs(0, Paragraphs.Count - 1);
end;

function TdxCustomPieceTable.FindParagraphIndexCore(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex;
begin
  Result := Paragraphs.SearchByLogPosition(ALogPosition);
end;

function TdxCustomPieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
begin
  Result := FContentType.LookupSectionIndexByParagraphIndex(AParagraphIndex);
end;

function TdxCustomPieceTable.GetIsMain: Boolean;
begin
  Result := ContentType.IsMain;
end;

function TdxCustomPieceTable.GetParagraphCount: Integer;
begin
  Result := Paragraphs.Count;
end;

{ TdxHistoryItem }

constructor TdxHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;


procedure TdxHistoryItem.Execute;
begin
  RedoCore;
end;

function TdxHistoryItem.NeedStoreSelection: Boolean;
begin
  Result := True;
end;

function TdxHistoryItem.GetChangeModified: Boolean;
begin
  Result := True;
end;

function TdxHistoryItem.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

procedure TdxHistoryItem.Redo;
begin
  RedoCore;
end;

procedure TdxHistoryItem.Undo;
begin
  UndoCore;
end;

{ TdxCompositeHistoryItem }

constructor TdxCompositeHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FItems := TdxHistoryItemList.Create;
end;

destructor TdxCompositeHistoryItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxCompositeHistoryItem.AddItem(AItem: TdxHistoryItem);
begin
  FItems.Add(AItem);
end;

procedure TdxCompositeHistoryItem.Clear;
begin
  FItems.Clear;
end;

function TdxCompositeHistoryItem.GetChangeModified: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if FItems[I].ChangeModified then
    begin
      Result := True;
      Break;
    end;
end;

function TdxCompositeHistoryItem.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxCompositeHistoryItem.GetItem(Index: Integer): TdxHistoryItem;
begin
  Result := FItems[Index];
end;

procedure TdxCompositeHistoryItem.RedoCore;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FItems[I].Redo;
  end;
end;

procedure TdxCompositeHistoryItem.UndoCore;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    FItems[I].Undo;
  end;
end;

{ TdxNotificationIdGenerator }

constructor TdxNotificationIdGenerator.Create;
begin
  inherited Create;
  FLastId := MinInt;
end;

function TdxNotificationIdGenerator.GenerateId: Integer;
begin
  Inc(FLastId);
  if FLastId = EmptyId then
    Inc(FLastId);
  Result := FLastId;
end;

{ TdxDocumentHistory }

constructor TdxDocumentHistory.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FUnmodifiedIndex := -1;
  FCurrentIndex := -1;
  FItems := TdxHistoryItemList.Create;
  FDocumentModel := ADocumentModel;
  FIdGenerator := CreateIdGenerator;
end;

destructor TdxDocumentHistory.Destroy;
begin
  FreeAndNil(FLastDisabledHistoryItem);
  DisposeContent(False);
  FreeAndNil(FIdGenerator);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxDocumentHistory.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if not FOnDestroying.Empty then
    FOnDestroying.Invoke(Self, TdxEventArgs.Empty);
end;

function TdxDocumentHistory.Add(AItem: TdxHistoryItem): TdxHistoryItem;
begin
  if TransactionLevel <> 0 then
    Transaction.AddItem(AItem)
  else
    InternalAdd(AItem);
  Result := AItem;
end;

function TdxDocumentHistory.BeginSyntaxHighlight: TdxHistoryItem;
begin
  Result := Transaction;
end;

procedure TdxDocumentHistory.BeginTrackModifiedChanged;
begin
  FPreviousModifiedValue := Modified;
end;

function TdxDocumentHistory.BeginTransaction: TdxHistoryItem;
begin
  if FTransactionLevel = 0 then
    FTransaction := CreateCompositeHistoryItem;
  Inc(FTransactionLevel);
  Result := Transaction;
end;

procedure TdxDocumentHistory.BeginUndoCurrent;
begin
end;

function TdxDocumentHistory.CheckIsTransactionChangeModifiedBackward(AIndex, AStartInnerIndex: Integer): Boolean;
var
  ATransaction: TdxCompositeHistoryItem;
  I: Integer;
begin
  if AIndex < 0 then
    Exit(False);

  Assert(Items[AIndex] is TdxCompositeHistoryItem);

  ATransaction := TdxCompositeHistoryItem(Items[AIndex]);
  for I := AStartInnerIndex downto 0 do
    if ATransaction.Items[I].ChangeModified then
      Exit(True);
  Result := False;
end;

procedure TdxDocumentHistory.DisposeContent(ACutOffItemsOnly: Boolean);
begin
  FreeAndNil(FTransaction);
  if ACutOffItemsOnly then
    CutOffHistory
  else
    Items.Clear;
end;

function TdxDocumentHistory.CheckIsTransactionChangeModifiedForward(AIndex, AStartInnerIndex: Integer): Boolean;
var
  ATransaction: TdxCompositeHistoryItem;
  I: Integer;
begin
  if AIndex < 0 then
    Exit(False);

  Assert(Items[AIndex] is TdxCompositeHistoryItem);

  ATransaction := TdxCompositeHistoryItem(Items[AIndex]);
  for I := AStartInnerIndex to ATransaction.Items.Count - 1 do
    if ATransaction.Items[I].ChangeModified then
      Exit(True);
  Result := False;
end;

procedure TdxDocumentHistory.Clear;
begin
  ClearCore(False);
end;

procedure TdxDocumentHistory.ClearCore(ADisposeOnlyCutOffItems: Boolean);
begin
  DisposeContent(ADisposeOnlyCutOffItems);
  FItems.Clear;
  BeginTrackModifiedChanged;
  try
    FCurrentIndex := -1;
    FUnmodifiedIndex := -1;
  finally
    EndTrackModifiedChanged;
  end;
end;

function TdxDocumentHistory.CommitAsSingleItem: TdxHistoryItem;
begin
  Assert(Transaction.Count = 1);
  Result := CommitAsSingleItemCore(Transaction[0]);
  if Result <> nil then
    Transaction.Items.Extract(Result);
end;

function TdxDocumentHistory.CommitAsSingleItemCore(ASingleItem: TdxHistoryItem): TdxHistoryItem;
begin
  Add(ASingleItem);
  Result := ASingleItem;
end;

function TdxDocumentHistory.CommitTransaction: TdxHistoryItem;
var
  AItemsCount: Integer;
begin
  if Transaction = nil then
    Exit(nil);
  AItemsCount := Transaction.Count;
  if AItemsCount > 1 then
    Result := Add(FTransaction)
  else
  begin
    if AItemsCount = 1 then
      Result := CommitAsSingleItem
    else
      Result := nil;
  end;
  if Result <> FTransaction then
    FTransaction.Free;
  FTransaction := nil;

end;

function TdxDocumentHistory.CreateCompositeHistoryItem: TdxCompositeHistoryItem;
begin
  Result := TdxCompositeHistoryItem.Create(FDocumentModel.MainPart);
end;

function TdxDocumentHistory.CreateIdGenerator: TdxNotificationIdGenerator;
begin
  Result := TdxNotificationIdGenerator.Create;
end;

procedure TdxDocumentHistory.CutOffHistory;
var
  AIndex: Integer;
begin
  AIndex := FCurrentIndex + 1;
  if AIndex < Count then
    OnCutOffHistory;
  while AIndex < Count do
    FItems.Delete(AIndex);
  if FUnmodifiedIndex > FCurrentIndex then
    FUnmodifiedIndex := ForceModifiedIndex;
end;

procedure TdxDocumentHistory.DisableHistory;
begin
  Inc(FDisableCount);
end;

procedure TdxDocumentHistory.EnableHistory;
begin
  if FDisableCount > 0 then
    Dec(FDisableCount);
  if not IsHistoryDisabled then
    FreeAndNil(FLastDisabledHistoryItem);
end;

procedure TdxDocumentHistory.EndSyntaxHighlight;
begin
end;

procedure TdxDocumentHistory.EndTrackModifiedChanged;
begin
  if FPreviousModifiedValue <> Modified then
    RaiseModifiedChanged;
end;

function TdxDocumentHistory.EndTransaction: TdxHistoryItem;
begin
  Result := FTransaction;
  if FTransactionLevel > 0 then
  begin
    Dec(FTransactionLevel);
    if FTransactionLevel = 0 then
      Result := CommitTransaction;
  end;
end;

procedure TdxDocumentHistory.EndUndoCurrent;
begin
end;

function TdxDocumentHistory.GetCanRedo: Boolean;
begin
  Result := (Count > 0) and (CurrentIndex < Count - 1);
end;

function TdxDocumentHistory.GetCanUndo: Boolean;
begin
  Result := CurrentIndex >= 0;
end;

function TdxDocumentHistory.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxDocumentHistory.GetCurrent: TdxHistoryItem;
begin
  Result := nil;
  if (CurrentIndex >= 0) and (CurrentIndex < Count) then
     Result := Items[CurrentIndex];
end;

function TdxDocumentHistory.GetIsHistoryDisabled: Boolean;
begin
  Result := FDisableCount > 0;
end;

function TdxDocumentHistory.GetItem(Index: Integer): TdxHistoryItem;
begin
  Result := FItems[Index];
end;

function TdxDocumentHistory.GetModified: Boolean;
begin
  Result := IsModified(FUnmodifiedIndex);
end;

function TdxDocumentHistory.GetNotificationId: Integer;
begin
  Result := FIdGenerator.GenerateId;
end;

function TdxDocumentHistory.HasChangesInCurrentTransaction: Boolean;
begin
  if (FTransactionLevel <= 0) or (FTransaction = nil) then
    Result := False
  else
    Result := FTransaction.Count > 0;
end;

function TdxDocumentHistory.IsModified(AUnmodifiedIndex, AUnmodifiedTransactionIndex: Integer): Boolean;
var
  I: Integer;
begin
  if CurrentIndex = AUnmodifiedIndex then
  begin
    Result := (AUnmodifiedTransactionIndex >= 0) and (CheckIsTransactionChangeModifiedBackward(CurrentIndex, AUnmodifiedTransactionIndex));
    Exit;
  end;
  if AUnmodifiedIndex < -1 then
    Exit(True);
  if AUnmodifiedIndex < CurrentIndex then
  begin
    Inc(AUnmodifiedIndex);
    if AUnmodifiedTransactionIndex >= 0 then
    begin
      if CheckIsTransactionChangeModifiedForward(AUnmodifiedIndex, AUnmodifiedTransactionIndex + 1) then
        Exit(True);
      Inc(AUnmodifiedIndex);
    end;
    I := AUnmodifiedIndex;
    while (I <= CurrentIndex) and (I < Count) do
    begin
      if FItems[I].ChangeModified then
        Exit(True);
      Inc(I);
    end;
  end
  else
  begin
    if AUnmodifiedTransactionIndex >= 0 then
    begin
      CheckIsTransactionChangeModifiedBackward(AUnmodifiedIndex, AUnmodifiedTransactionIndex);
      Dec(AUnmodifiedIndex);
    end;
    I := CurrentIndex + 1;
    while (I <= AUnmodifiedIndex) and (I < Count) do
    begin
      if Items[I].ChangeModified then
        Exit(True);
      Inc(I);
    end;
  end;
  Result := False;
end;

procedure TdxDocumentHistory.InternalAdd(AItem: TdxHistoryItem);
begin
  if not IsHistoryDisabled then
  begin
    CutOffHistory;
    FItems.Add(AItem);
    BeginTrackModifiedChanged;
    try
      Inc(FCurrentIndex);
    finally
      EndTrackModifiedChanged;
    end;
  end
  else
  begin
    FreeAndNil(FLastDisabledHistoryItem);
    FLastDisabledHistoryItem := AItem;
  end;
  if not FSuppressRaiseOperationComplete then
    RaiseOperationCompleted;
end;

function TdxDocumentHistory.IsModified(AUnmodifiedIndex: Integer): Boolean;
begin
  Result := IsModified(AUnmodifiedIndex, -1);
end;

procedure TdxDocumentHistory.OnCutOffHistory;
begin
end;

procedure TdxDocumentHistory.OnEndUndoCore;
begin
end;

procedure TdxDocumentHistory.RaiseModifiedChanged;
begin
  if not FOnModifiedChanged.Empty then
    FOnModifiedChanged.Invoke(Self, nil);
end;

procedure TdxDocumentHistory.RaiseOperationCompleted;
begin
  if not FOnOperationCompleted.Empty then
    FOnOperationCompleted.Invoke(Self, nil);
end;

procedure TdxDocumentHistory.Redo;
begin
  if CanRedo then
  begin
    DisableHistory;
    try
      FDocumentModel.BeginUpdate;
      try
        BeginTrackModifiedChanged;
        try
          RedoCore;
        finally
          EndTrackModifiedChanged;
        end;
      finally
      documentModel.EndUpdate;
      end;
    finally
      EnableHistory;
    end;
  end;
end;

procedure TdxDocumentHistory.RedoCore;
begin
  Inc(FCurrentIndex);
  Current.Redo;
  RaiseOperationCompleted;
end;

procedure TdxDocumentHistory.SetModified(const Value: Boolean);
begin
  if Value <> Modified then
  begin
    if Value then
      FUnmodifiedIndex := ForceModifiedIndex
    else
      FUnmodifiedIndex := CurrentIndex;
    RaiseModifiedChanged;
  end;
end;

procedure TdxDocumentHistory.SetModifiedTextAppended(FForceRaiseModifiedChanged: Boolean);
begin
  if not Modified then
  begin
    Dec(FUnmodifiedIndex);
    RaiseModifiedChanged;
  end;
end;

procedure TdxDocumentHistory.SetTransaction(AValue: TdxCompositeHistoryItem);
begin
  FTransaction := AValue;
end;

procedure TdxDocumentHistory.SetTransactionLevel(AValue: Integer);
begin
  FTransactionLevel := AValue;
end;

procedure TdxDocumentHistory.SmartClear;
begin
  ClearCore(True);
end;

procedure TdxDocumentHistory.Undo;
begin
  if CanUndo then
  begin
    DisableHistory;
    try
      FDocumentModel.BeginUpdate;
      try
        BeginTrackModifiedChanged;
        try
          UndoCore;
        finally
          EndTrackModifiedChanged;
        end;
      finally
        FDocumentModel.EndUpdate;
      end;
    finally
      EnableHistory;
    end;
  end;
end;

procedure TdxDocumentHistory.UndoCore;
begin
  BeginUndoCurrent;
  Current.Undo;
  Dec(FCurrentIndex);
  EndUndoCurrent;
  OnEndUndoCore;
  RaiseOperationCompleted;
end;

{ TdxEmptyNotificationIdGenerator }

function TdxEmptyNotificationIdGenerator.GenerateId: Integer;
begin
  Result := EmptyId;
end;

{ TdxEmptyHistory }

constructor TdxEmptyHistory.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FItems := TdxFastObjectList.Create(True, 1024);
end;

destructor TdxEmptyHistory.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TdxEmptyHistory.Add(AItem: TdxHistoryItem): TdxHistoryItem;
begin
  Inc(FTransactionItemCount);
  Result := AItem;
  FItems.Add(AItem);
end;

function TdxEmptyHistory.BeginTransaction: TdxHistoryItem;
begin
  if TransactionLevel = 0 then
    FTransactionItemCount := 0;
  Result := inherited BeginTransaction;
end;

function TdxEmptyHistory.HasChangesInCurrentTransaction: Boolean;
begin
  if TransactionLevel <= 0 then
    Result := inherited HasChangesInCurrentTransaction
  else
    Result := FTransactionItemCount <> 0;
end;

function TdxEmptyHistory.CreateIdGenerator: TdxNotificationIdGenerator;
begin
  Result := TdxEmptyNotificationIdGenerator.Create;
end;

{ TdxDisabledHistory }

function TdxDisabledHistory.Add(AItem: TdxHistoryItem): TdxHistoryItem;
begin
  Result := AItem;
  Items.Add(AItem);
  if TransactionLevel > 0 then
    Inc(FTransactionCount);
end;

function TdxDisabledHistory.BeginTransaction: TdxHistoryItem;
begin
  if TransactionLevel = 0 then
    FTransactionCount := 0;
  Result := inherited BeginTransaction;
end;

function TdxDisabledHistory.EndTransaction: TdxHistoryItem;
begin
  if TransactionLevel = 1 then
    FTransactionCount := 0;
  Result := inherited EndTransaction;
end;

function TdxDisabledHistory.HasChangesInCurrentTransaction: Boolean;
begin
  if TransactionLevel <= 0 then
    Result := False
  else
    Result := FTransactionCount > 0;
end;

{ TdxHistoryTransaction }

constructor TdxHistoryTransaction.Create(AHistory: TdxDocumentHistory);
begin
  inherited Create;
  Assert(AHistory <> nil);
  FHistory := AHistory;
  AHistory.BeginTransaction;
  AHistory.Destroying.Add(HistoryDestroyingHandler);
end;

destructor TdxHistoryTransaction.Destroy;
begin
  if FHistory <> nil then
    FHistory.Destroying.Remove(HistoryDestroyingHandler);
  EndHistoryTransaction;
  inherited Destroy;
end;

procedure TdxHistoryTransaction.EndHistoryTransaction;
var
  ATopLevelTransaction: Boolean;
begin
  if FHistory = nil then
    Exit;
  ATopLevelTransaction := FHistory.TransactionLevel = 1;
  if ATopLevelTransaction and SuppressRaiseOperationComplete then
  begin
    FHistory.SuppressRaiseOperationComplete := True;
    FHistory.EndTransaction;
    FHistory.SuppressRaiseOperationComplete := False;
  end
  else
    FHistory.EndTransaction;
end;

procedure TdxHistoryTransaction.HistoryDestroyingHandler(Sender: TObject; Args: TdxEventArgs);
begin
  if FHistory = Sender then
    FHistory := nil;
end;

{ TdxWebSettings }

function TdxWebSettings.IsBodyMarginsSet: Boolean;
begin
  Result := (LeftMargin <> 0) or (TopMargin <> 0) or (RightMargin <> 0) or (BottomMargin <> 0);
end;

{ TdxCustomDocumentModelDeferredChanges }

constructor TdxCustomDocumentModelDeferredChanges.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  Assert(ADocumentModel <> nil, 'documentModel = nil');
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

{ TdxCustomSection }

constructor TdxCustomSection.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FFirstParagraphIndex := -1;
  FLastParagraphIndex := -1;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

procedure TdxCustomSection.CopyFrom(ASection: TdxCustomSection);
begin
  FirstParagraphIndex := ASection.FirstParagraphIndex;
  LastParagraphIndex := ASection.LastParagraphIndex;
end;

{ TdxCustomDocumentModel }

constructor TdxCustomDocumentModel.Create(ADpiX, ADpiY: Single);
begin
  inherited Create(ADpiX, ADpiY);
  FLayoutUnit := GetDefaultLayoutUnit;
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
  FServiceManager := CreateServiceManager;
  FUnitConverter := CreateDocumentModelUnitConverter;
  UpdateLayoutUnitConverter;
end;

constructor TdxCustomDocumentModel.Create;
begin
  Create(DpiX, DpiY);
end;

destructor TdxCustomDocumentModel.Destroy;
begin
  DisposeCore;
  DestroyDocumentObjects;

  FreeAndNil(FServiceManager);
  FreeAndNil(FBatchUpdateHelper);
  FreeAndNil(FLayoutUnitConverter);
  FreeAndNil(FToDocumentLayoutUnitConverter);
  FreeAndNil(FUnitConverter);
  inherited Destroy;
end;

class function TdxCustomDocumentModel.GetDpi: Single;
begin
  Result := TdxDocumentModelDpi.Dpi;
end;

class function TdxCustomDocumentModel.GetDpiX: Single;
begin
  Result := TdxDocumentModelDpi.DpiX;
end;

class function TdxCustomDocumentModel.GetDpiY: Single;
begin
  Result := TdxDocumentModelDpi.DpiY;
end;

procedure TdxCustomDocumentModel.DestroyDocumentObjects;
begin
  FreeAndNil(FSections);
  FreeAndNil(FContentTypes);
end;

procedure TdxCustomDocumentModel.BeforeDestruction;
begin
  FIsDestroying := True;
  inherited BeforeDestruction;
end;

procedure TdxCustomDocumentModel.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxCustomDocumentModel.ClearCore;
begin
  if FHistory <> nil then
  begin
    UnsubscribeHistoryEvents;
    FreeAndNil(FHistory);
  end;
  FreeAndNil(FCache);
  if FDocumentCapabilities <> nil then
  begin
    UnsubscribeDocumentCapabilitiesOptionsEvents;
    FreeAndNil(FDocumentCapabilities);
  end;
end;

procedure TdxCustomDocumentModel.DisposeCore;
begin
  ClearCore;
end;

procedure TdxCustomDocumentModel.ResetMerging;
begin
end;

function TdxCustomDocumentModel.CreateDocumentHistory: TdxDocumentHistory;
begin
  Result := TdxDocumentHistory.Create(Self);
end;

procedure TdxCustomDocumentModel.CreateDocumentObjects;
begin
  FContentTypes := TdxObjectList<TdxContentTypeBase>.Create;
  FSections := CreateSectionCollection;
end;

function TdxCustomDocumentModel.FindSectionIndex(ALogPosition: TdxDocumentLogPosition; AStrictSearch: Boolean = True): TdxSectionIndex;
var
  APredicate: TdxSectionAndLogPositionComparable;
begin
  APredicate := TdxSectionAndLogPositionComparable.Create(ALogPosition);
  try
    if not TdxAlgorithms1<TdxCustomSection>.BinarySearch(Sections, APredicate, Result) then
    begin
      if Result = Sections.Count then
        Result := Sections.Count - 1
      else
      begin
        Assert(False, 'LogPosition');
        Result := -1;
      end;
    end;
  finally
    APredicate.Free;
  end;
end;

function TdxCustomDocumentModel.GetActivePieceTableCore: TdxCustomPieceTable;
begin
  Result := MainPart;
end;

procedure TdxCustomDocumentModel.SetActivePieceTable(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxCustomDocumentModel.SwitchToEmptyHistory(ADisposeHistory: Boolean);
begin
  if ADisposeHistory then
    DisposeHistory;
  FHistory.Free;
  FHistory := TdxEmptyHistory.Create(Self);
  SubscribeHistoryEvents;
end;

procedure TdxCustomDocumentModel.SwitchToNormalHistory(ADisposeHistory: Boolean);
begin
  if ADisposeHistory then
    DisposeHistory;
  FHistory.Free;
  FHistory := CreateDocumentHistory;
  ResetMerging;
  SubscribeHistoryEvents;
end;

procedure TdxCustomDocumentModel.DisposeHistory;
begin
  if History <> nil then
  begin
    UnsubscribeHistoryEvents;
    FreeAndNil(FHistory);
  end;
end;

procedure TdxCustomDocumentModel.SubscribeHistoryEvents;
begin
  History.OperationCompleted.Add(OnHistoryOperationCompleted);
  History.ModifiedChanged.Add(OnHistoryModifiedChanged);
end;

procedure TdxCustomDocumentModel.Undo;
begin
  History.Undo;
end;

procedure TdxCustomDocumentModel.UnsubscribeHistoryEvents;
begin
  History.OperationCompleted.Remove(OnHistoryOperationCompleted);
  History.ModifiedChanged.Remove(OnHistoryModifiedChanged);
end;

procedure TdxCustomDocumentModel.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxCustomDocumentModel.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxCustomDocumentModel.OnBeginUpdate;
begin
  History.BeginTransaction;
end;

procedure TdxCustomDocumentModel.OnEndUpdate;
begin
  History.EndTransaction;
end;

procedure TdxCustomDocumentModel.OnCancelUpdate;
begin
  History.EndTransaction;
end;

procedure TdxCustomDocumentModel.OnFirstBeginUpdate;
begin
  FDeferredChanges := CreateDeferredChanges;
  History.BeginTransaction;
end;

procedure TdxCustomDocumentModel.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxCustomDocumentModel.OnLastCancelUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxCustomDocumentModel.OnLastEndUpdateCore;
begin
  History.EndTransaction;
  DeleteDeferredChanges;
end;

function TdxCustomDocumentModel.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxCustomDocumentModel.GetEnableFieldNames: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentModel.GetService(const AServiceType: TdxServiceType): IInterface;
begin
  if FServiceManager <> nil then
    Result := FServiceManager.GetService(AServiceType)
  else
    Result := nil;
end;

procedure TdxCustomDocumentModel.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface);
begin
  if FServiceManager <> nil then
    FServiceManager.AddService(AServiceType, AServiceInstance);
end;

procedure TdxCustomDocumentModel.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean);
begin
  if FServiceManager <> nil then
    FServiceManager.AddService(AServiceType, AServiceInstance, APromote);
end;

procedure TdxCustomDocumentModel.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback);
begin
  if FServiceManager <> nil then
    FServiceManager.AddService(AServiceType, ACallback);
end;

procedure TdxCustomDocumentModel.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean);
begin
  if FServiceManager <> nil then
    FServiceManager.AddService(AServiceType, ACallback, APromote);
end;

procedure TdxCustomDocumentModel.RemoveService(const AServiceType: TdxServiceType);
begin
  if FServiceManager <> nil then
    FServiceManager.RemoveService(AServiceType);
end;

procedure TdxCustomDocumentModel.Redo;
begin
  History.Redo;
end;

procedure TdxCustomDocumentModel.RemoveService(const AServiceType: TdxServiceType; APromote: Boolean);
begin
  if FServiceManager <> nil then
    FServiceManager.RemoveService(AServiceType, APromote);
end;

function TdxCustomDocumentModel.GetService<T>: T;
begin
  Result := TdxServiceUtils<T>.GetService(Self);
end;

procedure TdxCustomDocumentModel.ReplaceService<T>(const ANewService: T);
begin
  TdxServiceUtils<T>.ReplaceService(Self, ANewService);
end;

procedure TdxCustomDocumentModel.BeginSuppressPerformLayout;
begin
  Inc(FSuppressPerformLayoutCount);
end;

procedure TdxCustomDocumentModel.EndSuppressPerformLayout;
begin
  Dec(FSuppressPerformLayoutCount);
end;

function TdxCustomDocumentModel.SuppressPerformLayout: Boolean;
begin
  Result := FSuppressPerformLayoutCount <> 0;
end;

procedure TdxCustomDocumentModel.ClearDocument;
begin
  BeginClearDocument;
  ClearDocumentCore;
  EndClearDocument;
end;

function TdxCustomDocumentModel.GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList;
begin
  Result := TdxFastList.Create;
end;

function TdxCustomDocumentModel.GetFontCache: TdxFontCache;
begin
  Result := TdxFontCacheManager.GetFontCache(LayoutUnit, Dpi);
end;

function TdxCustomDocumentModel.GetIsUpdateLockedOrOverlapped: Boolean;
begin
  Result := BatchUpdateHelper.IsUpdateLocked or BatchUpdateHelper.OverlappedTransaction;
end;

function TdxCustomDocumentModel.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxCustomDocumentModel.GetMaxFieldSwitchLength: Integer;
begin
  Result := 2;
end;

procedure TdxCustomDocumentModel.SetDocumentCapabilities(const Value: TdxCustomDocumentCapabilitiesOptions);
begin
  FDocumentCapabilities.Assign(Value);
end;

procedure TdxCustomDocumentModel.Initialize;
begin
  CreateOptions;
  CreateDocumentObjects;
end;

procedure TdxCustomDocumentModel.CreateOptions;
begin
  FDocumentCapabilities := CreateDocumentCapabilitiesOptions;
end;

function TdxCustomDocumentModel.GetDefaultLayoutUnit: TdxDocumentLayoutUnit;
begin
  Result := DefaultLayoutUnit;
end;

function TdxCustomDocumentModel.CreateDocumentModelUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := TdxDocumentModelUnitTwipsConverter.Create(ScreenDpiX, ScreenDpiY);
end;

function TdxCustomDocumentModel.CreateServiceManager: TdxServiceManager;
begin
  Result := TdxServiceManager.Create;
end;

procedure TdxCustomDocumentModel.DeleteDeferredChanges;
begin
  FreeAndNil(FDeferredChanges);
end;

procedure TdxCustomDocumentModel.SubscribeDocumentCapabilitiesOptionsEvents;
begin
  if FDocumentCapabilities <> nil then
    FDocumentCapabilities.Changed.Add(OnDocumentCapabilitiesChanged);
end;

procedure TdxCustomDocumentModel.UnsubscribeDocumentCapabilitiesOptionsEvents;
begin
  if DocumentCapabilities <> nil then
    FDocumentCapabilities.Changed.Remove(OnDocumentCapabilitiesChanged);
end;

procedure TdxCustomDocumentModel.SubscribeOptionsEvents;
begin
  SubscribeDocumentCapabilitiesOptionsEvents;
end;

procedure TdxCustomDocumentModel.SubscribeDocumentObjectsEvents;
begin
end;

procedure TdxCustomDocumentModel.BeginClearDocument;
begin
end;

procedure TdxCustomDocumentModel.EndClearDocument;
begin
end;

procedure TdxCustomDocumentModel.ClearDocumentContent;
begin
  FreeAndNil(FCache);
  FCache := CreateDocumentCache;
  FCache.Initialize(Self);
  FSections.Clear;
end;

procedure TdxCustomDocumentModel.ClearDocumentCore;
begin
  ClearDocumentContent;
end;

procedure TdxCustomDocumentModel.SetLayoutUnit(const Value: TdxDocumentLayoutUnit);
begin
  if FLayoutUnit = Value then
    Exit;
  FLayoutUnit := Value;
  OnLayoutUnitChanged;
end;

procedure TdxCustomDocumentModel.UpdateLayoutUnitConverter;
begin
  FreeAndNil(FToDocumentLayoutUnitConverter);
  FToDocumentLayoutUnitConverter := FUnitConverter.CreateConverterToLayoutUnits(LayoutUnit);
  FreeAndNil(FLayoutUnitConverter);
  FLayoutUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(LayoutUnit, ScreenDpi);
end;

procedure TdxCustomDocumentModel.OnDocumentCapabilitiesChanged(ASender: TObject;
  E: TdxRichEditNotificationOptionsChangedArgs);
begin
  if TdxCustomDocumentCapabilitiesOptions.TAction.Undo in E.Actions then
    SwitchToNormalHistory(False);
end;

procedure TdxCustomDocumentModel.OnLayoutUnitChanged;
begin
  UpdateLayoutUnitConverter;
  RaiseLayoutUnitChanged;
end;

procedure TdxCustomDocumentModel.RaiseLayoutUnitChanged;
begin
  if not FLayoutUnitChanged.Empty then
    FLayoutUnitChanged.Invoke(Self, TdxEventArgs.Empty);
end;

{ TdxRequestSectionIndexEventArgs }

constructor TdxRequestSectionIndexEventArgs.Create;
begin
  inherited Create;
  FSectionIndex := MaxInt;
end;

{ TdxContentTypeBase }

constructor TdxContentTypeBase.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  ADocumentModel.ContentTypes.Add(Self);
  FPieceTable := ADocumentModel.CreatePieceTable(Self);
  FOwnPieceTable := True;
end;

constructor TdxContentTypeBase.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

destructor TdxContentTypeBase.Destroy;
begin
  if DocumentModel.ContentTypes <> nil then
    DocumentModel.ContentTypes.Extract(Self);
  if FOwnPieceTable then
    FreeAndNil(FPieceTable);
  inherited Destroy;
end;

procedure TdxContentTypeBase.ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex,
  AEndRunIndex: TdxRunIndex);
begin
  DocumentModel.ApplyChanges(PieceTable, AChangeType, AStartRunIndex, AEndRunIndex);
end;

procedure TdxContentTypeBase.ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex,
  AEndRunIndex: TdxRunIndex);
begin
  DocumentModel.ApplyChangesCore(PieceTable, AActions, AStartRunIndex, AEndRunIndex);
end;

function TdxContentTypeBase.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxContentTypeBase.GetIsComment: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsEndNote: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsFooter: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsFootNote: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsHeader: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsHeaderFooter: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsMain: Boolean;
begin
  Result := True;
end;

function TdxContentTypeBase.GetIsNote: Boolean;
begin
  Result := False;
end;

function TdxContentTypeBase.GetIsReferenced: Boolean;
begin
  Result := True;
end;

procedure TdxContentTypeBase.SetPageCount(APageCount: Integer);
begin
end;

function TdxContentTypeBase.GetIsTextBox: Boolean;
begin
  Result := False;
end;

procedure TdxContentTypeBase.ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase);
begin
  DocumentModel.ApplySectionFormatting(ALogPositionStart, ALength, AModifier);
end;

function TdxContentTypeBase.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
begin
  Result := DocumentModel.LookupSectionIndexByParagraphIndex(AParagraphIndex);
end;

function TdxContentTypeBase.CreateSpellCheckerManager(APieceTable: TdxCustomPieceTable): TObject{TdxSpellCheckerManager};
begin
  Result := APieceTable.DocumentModel.CreateEmptySpellCheckerManager(APieceTable);
end;


end.
