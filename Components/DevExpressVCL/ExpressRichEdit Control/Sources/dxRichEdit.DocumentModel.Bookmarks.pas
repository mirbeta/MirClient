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

unit dxRichEdit.DocumentModel.Bookmarks;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.ProtectionFormatting;

type
  TdxBookmark = class;
  TdxRangePermission = class;
  TdxRangePermissionCollection = class;
  TdxRangePermissionCollectionEx = class;

  { IdxDocumentModelExporter }

  IdxDocumentModelExporter = interface(IdxSimpleDocumentModelExporter)
  ['{3EAE4BF5-24CB-4771-BB29-403A1BDAF0A8}']
    procedure ExportBookmarkEnd(ABookmark: TdxBookmark);
    procedure ExportBookmarkStart(ABookmark: TdxBookmark);
    procedure ExportRangePermissionStart(ARangePermission: TdxRangePermission);
    procedure ExportRangePermissionEnd(ARangePermission: TdxRangePermission);
  end;

  { TdxBookmarkBase }

  TdxBookmarkBase = class abstract(TdxVisitableDocumentInterval)
  protected type
  {$REGION 'protected type'}
    TdxGetDelegate = {reference to }function(): TdxIntegerList of object;
    TdxSetDelegate = {reference to }procedure(AValue: TdxIntegerList) of object;

    { TdxHistoryNotificationsAccessor }

    TdxHistoryNotificationsAccessor = class
    strict private
      function GetChangeNotificationIds: TdxIntegerList;
      procedure SetChangeNotificationIds(AValue: TdxIntegerList);
    protected
      FGetter: TdxGetDelegate;
      FSetter: TdxSetDelegate;
    public
      constructor Create(AGetter: TdxGetDelegate; ASetter: TdxSetDelegate);
      function ShouldChange(AHistoryNotificationId: Integer): Boolean;
      procedure AddChangeNotificationId(AHistoryNotificationId: Integer);

      property ChangeNotificationIds: TdxIntegerList read GetChangeNotificationIds write SetChangeNotificationIds;
    end;
  {$ENDREGION}
  strict private
    FCanExpand: Boolean;
    FForceUpdateInterval: Boolean;
    FStartChangeNotificationIds: TdxIntegerList;
    FEndChangeNotificationIds: TdxIntegerList;
    function GetPieceTable: TdxSimplePieceTable;
  protected
    procedure UpdateStartPosition; override;
    procedure UpdateInterval; virtual;
    function GetStartChangeNotificationIds: TdxIntegerList; virtual;
    procedure SetStartChangeNotificationIds(AValue: TdxIntegerList); virtual;
    function GetEndChangeNotificationIds: TdxIntegerList; virtual;
    procedure SetEndChangeNotificationIds(AValue: TdxIntegerList); virtual;
    procedure RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer;
      AHistoryNotificationId: Integer); overload; override;
    function RunInserted(APosition: PdxDocumentModelPosition; ANewRunIndex: TdxRunIndex; ALength: Integer;
      AAccessor: TdxHistoryNotificationsAccessor; AHistoryNotificationId: Integer): Boolean; overload;
    procedure RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer;
      AHistoryNotificationId: Integer); overload; override;
    function RunRemoved(APos: PdxDocumentModelPosition; ARunIndex: TdxRunIndex; ALength: Integer;
      AAccessor: TdxHistoryNotificationsAccessor; AHistoryNotificationId: Integer): Boolean; overload;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AStart, AEnd: TdxDocumentLogPosition;
      AForceUpdateInterval: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure UpdateEndPosition; override;
    procedure OnChangedCore; override;
    procedure Delete(AIndex: Integer); virtual; abstract;

    property CanExpand: Boolean read FCanExpand write FCanExpand;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  TdxBookmarkBaseList = class(TdxDocumentIntervalList)
  strict private
    function GetItem(Index: Integer): TdxBookmarkBase;
  public
    property Items[Index: Integer]: TdxBookmarkBase read GetItem; default;
  end;

  { TdxBookmarkInfo }

  TdxBookmarkInfo = class
  strict private
    FBookmark: TdxBookmarkBase;
    FStartPosition: TdxDocumentModelPosition;
    FEndPosition: TdxDocumentModelPosition;
  public
    constructor Create(ABookmark: TdxBookmarkBase);

    property Bookmark: TdxBookmarkBase read FBookmark;
    property StartPosition: TdxDocumentModelPosition read FStartPosition write FStartPosition;
    property EndPosition: TdxDocumentModelPosition read FEndPosition write FEndPosition;
  end;
  TdxBookmarkInfoList = class(TdxObjectList<TdxBookmarkInfo>);

  { TdxBookmark }

  TdxBookmark = class(TdxBookmarkBase)
  strict private
    FName: string;
    function GetIsHidden: Boolean;
    class function IsNameValidCore(const AName: string): Boolean; static;
  public
    procedure Visit(const AVisitor: IdxDocumentIntervalVisitor); override;
    class function IsNameValid(const AName: string): Boolean; static;
    procedure Delete(AIndex: Integer); override;

    property Name: string read FName write FName;
    property IsHidden: Boolean read GetIsHidden;
  end;

  TdxBookmarkList = class(TdxBookmarkBaseList)
  strict private
    function GetItem(Index: Integer): TdxBookmark;
  public
    property Items[Index: Integer]: TdxBookmark read GetItem; default;
  end;

  { TdxBookmarkBaseCollection }

  TdxBookmarkBaseCollection = class(TdxDocumentIntervalCollection)
  private
    function GetItem(Index: Integer): TdxBookmarkBase;
  public
    procedure Clear;
    procedure UpdateIntervals; virtual;

    function GetEntireBookmarksCore(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;

    property Items[Index: Integer]: TdxBookmarkBase read GetItem; default;
  end;

  { TdxBookmarkCollection }

  TdxBookmarkCollection = class(TdxBookmarkBaseCollection)
  private
    function GetItem(Index: Integer): TdxBookmark;
  public
    function FindByName(const AName: string): TdxBookmark;

    property Items[Index: Integer]: TdxBookmark read GetItem; default;
  end;

  { TdxBookmarkBoundaryOrder }

  TdxBookmarkBoundaryOrder = (Start, &End);

  { TdxVisitableDocumentIntervalBoundary }

  TdxVisitableDocumentIntervalBoundary = class abstract
  private
    FInterval: TdxVisitableDocumentInterval;
    FIntervalIndex: Integer;
  protected
    function GetOrder: TdxBookmarkBoundaryOrder; virtual; abstract;
    function GetPosition: PdxDocumentModelPosition; virtual; abstract;
  public
    constructor Create(AInterval: TdxVisitableDocumentInterval);
    function CreateBox: TdxVisitableDocumentIntervalBox; virtual; abstract;

    procedure Export(const AExporter: IdxDocumentModelExporter); virtual; abstract;

    property IntervalIndex: Integer read FIntervalIndex write FIntervalIndex;
    property Order: TdxBookmarkBoundaryOrder read GetOrder;
    property Position: PdxDocumentModelPosition read GetPosition;
    property VisitableInterval: TdxVisitableDocumentInterval read FInterval;
  end;

  { TdxBookmarkStartBoundary }

  TdxBookmarkStartBoundary = class(TdxVisitableDocumentIntervalBoundary)
  protected
    function GetPosition: PdxDocumentModelPosition; override;
    function GetOrder: TdxBookmarkBoundaryOrder; override;
  public
    function CreateBox: TdxVisitableDocumentIntervalBox; override;
    procedure Export(const AExporter: IdxDocumentModelExporter); override;
  end;

  { TdxBookmarkEndBoundary }

  TdxBookmarkEndBoundary = class(TdxVisitableDocumentIntervalBoundary)
  protected
    function GetPosition: PdxDocumentModelPosition; override;
    function GetOrder: TdxBookmarkBoundaryOrder; override;
  public
    function CreateBox: TdxVisitableDocumentIntervalBox; override;
    procedure Export(const AExporter: IdxDocumentModelExporter); override;
  end;

  { TdxVisitableDocumentIntervalBoundaryCollection }

  TdxVisitableDocumentIntervalBoundaryCollection = class(TdxObjectList<TdxVisitableDocumentIntervalBoundary>)
  protected
    procedure QuickSort(const AComparer: IComparer<TdxVisitableDocumentIntervalBoundary>; L, R: Integer);
  public
    function GetRange(AIndex, ALength: Integer): TdxList<TdxVisitableDocumentIntervalBoundary>;
    procedure Sort(AIndex, ACount: Integer; const AComparer: IComparer<TdxVisitableDocumentIntervalBoundary>); overload;
  end;

  { TdxVisitableDocumentIntervalStartBoundaryFactory }

  TdxVisitableDocumentIntervalStartBoundaryFactory = class(TcxIUnknownObject, IdxDocumentIntervalVisitor)
  strict private
    FBoundary: TdxVisitableDocumentIntervalBoundary;
  protected
    procedure SetBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary);
  public
    procedure Visit(AInterval: TdxVisitableDocumentInterval);

    property Boundary: TdxVisitableDocumentIntervalBoundary read FBoundary;
  end;

  { TdxVisitableDocumentIntervalEndBoundaryFactory }

  TdxVisitableDocumentIntervalEndBoundaryFactory = class(TcxIUnknownObject, IdxDocumentIntervalVisitor)
  strict private
    FBoundary: TdxVisitableDocumentIntervalBoundary;
  protected
    procedure SetBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary);
  public
    procedure Visit(AInterval: TdxVisitableDocumentInterval);
    property Boundary: TdxVisitableDocumentIntervalBoundary read FBoundary;
  end;

  { TdxVisitableDocumentIntervalBoundaryIterator }

  TdxVisitableDocumentIntervalBoundaryIterator = class
  strict private type
      TComparer = class(TdxComparer<TdxVisitableDocumentIntervalBoundary>)
      public
        //IComparer
        function Compare(const Left, Right: TdxVisitableDocumentIntervalBoundary): Integer; override;
      end;
  protected class var
    FComparer: TComparer;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  private
    FCurrentIndex: Integer;
    FPieceTable: TdxSimplePieceTable;
    FVisitableDocumentIntervalBoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
    FIncludeHiddenIntervals: Boolean;
    function InternalGetCurrent: TdxVisitableDocumentIntervalBoundary;
  protected
    procedure InitializeBoundaries;
    procedure PopulateBoundaries;
    procedure PopulateBookmarksCore(ABookmarks: TdxBookmarkList); virtual;
    procedure PopulateBoundariesCore; overload; virtual;
    procedure PopulateBoundariesCore(AIntervals: TObjectList); overload; virtual;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AIncludeHiddenIntervals: Boolean = True);
    destructor Destroy; override;

    function IsDone: Boolean; virtual;
    procedure MoveNext; virtual;
    procedure Reset; virtual;
    function CreateVisitableDocumentIntervalStartBoundaryFactory: TdxVisitableDocumentIntervalStartBoundaryFactory; virtual;
    function CreateVisitableDocumentIntervalEndBoundaryFactory: TdxVisitableDocumentIntervalEndBoundaryFactory; virtual;
    function IsVisibleInterval(AInterval: TdxVisitableDocumentInterval): Boolean; virtual;

    property Boundaries: TdxVisitableDocumentIntervalBoundaryCollection  read FVisitableDocumentIntervalBoundaries;
    property Current: TdxVisitableDocumentIntervalBoundary read InternalGetCurrent;
    property PieceTable: TdxSimplePieceTable read FPieceTable;
  end;

  { TdxVisitableDocumentIntervalBasedObjectBoundaryIterator }

  TdxVisitableDocumentIntervalBasedObjectBoundaryIterator = class(TdxVisitableDocumentIntervalBoundaryIterator)
  protected
    procedure PopulateBoundariesCore; overload; override;
    procedure PopulateBoundariesCore(AIntervals: TObjectList); overload; override;
  end;

  { TdxRangePermission }

  TdxRangePermission = class(TdxBookmarkBase)
  strict private
    type
      TBoundaryComparer = class(TdxComparer<TdxVisitableDocumentIntervalBoundary>)
        function Compare(const ALeft, ARight: TdxVisitableDocumentIntervalBoundary): Integer; override;
      end;

      TBoundaryOrderComparer = class(TdxComparer<TdxVisitableDocumentIntervalBoundary>)
        function Compare(const ALeft, ARight: TdxVisitableDocumentIntervalBoundary): Integer; override;
      end;
    class var
      FBoundaryComparer: TBoundaryComparer;
      FBoundaryOrderComparer: TBoundaryOrderComparer;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FProperties: TdxRangePermissionProperties;
    function GetUserName: string;
    procedure SetUserName(const AValue: string);
    function GetGroup: string;
    procedure SetGroup(const AValue: string);
  protected
    function IsGranted(APieceTable: TdxSimplePieceTable): Boolean;
    function OnRunInsertedStart(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer): Boolean;
    function OnRunMergedStart(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer): Boolean;
    function OnRunInsertedEnd(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer): Boolean;
    function OnRunMergedEnd(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer): Boolean;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; const AStart, AEnd: TdxDocumentLogPosition); overload;
    constructor Create(APieceTable: TdxSimplePieceTable; const AStart, AEnd: TdxDocumentLogPosition; APropertiesIndex: Integer); overload;
    destructor Destroy; override;
    function Clone: TdxRangePermission;
    procedure Delete(AIndex: Integer); override;
    procedure Visit(const AVisitor: IdxDocumentIntervalVisitor); override;
    function Subtract(AInterval: TdxRangePermission): TdxRangePermissionCollection;
    class function Union(AInterval1: TdxRangePermission; AInterval2: TdxRangePermission): TdxRangePermission; static;

    class property BoundaryComparer: TBoundaryComparer read FBoundaryComparer;
    class property BoundaryOrderComparer: TBoundaryOrderComparer read FBoundaryOrderComparer;
    property Properties: TdxRangePermissionProperties read FProperties;

    property UserName: string read GetUserName write SetUserName;
    property Group: string read GetGroup write SetGroup;
  end;

  TdxRangePermissionList = class(TdxBookmarkBaseList)
  strict private
    function GetItem(Index: Integer): TdxRangePermission;
  public
    property Items[Index: Integer]: TdxRangePermission read GetItem; default;
  end;

  { TdxRangePermissionCollection }

  TdxRangePermissionCollection = class(TdxBookmarkBaseCollection)
  strict private
    FBoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
    function GetBoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
    function GetItem(Index: Integer): TdxRangePermission;
  protected
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); override;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    function CreateBoundaryCollection: TdxVisitableDocumentIntervalBoundaryCollection;
    procedure OnDocumentIntervalInserted(AIndex: Integer); override;
    procedure OnDocumentIntervalRemoved(AIndex: Integer); override;

    property Boundaries: TdxVisitableDocumentIntervalBoundaryCollection read GetBoundaries;
  public
    destructor Destroy; override;

    property Items[Index: Integer]: TdxRangePermission read GetItem; default;
  end;

  { TdxRangePermissionCollectionEx }

  TdxRangePermissionCollectionEx = class(TdxRangePermissionCollection)
  protected
    function AddCore(AInterval: TdxDocumentInterval): Integer; override;
  public
    function Remove(AInterval: TdxRangePermission): Boolean; virtual;
    function Contains(AInterval: TdxRangePermission): Boolean; virtual;
    procedure RemoveCore(AToRemove: TdxRangePermissionList);
    procedure AddRange(AToAdd: TdxRangePermissionList); overload;
  end;

  { TdxRangePermissionMergedCollection }
  TdxRangePermissionMergedCollection = class(TdxRangePermissionCollectionEx)
  protected
    function CreateInnerList: TdxDocumentIntervalList; override;
  end;

  { TdxOwnedRangePermissionMergedCollection }

  TdxOwnedRangePermissionMergedCollection = class(TdxRangePermissionCollectionEx)
  protected
    function CreateInnerList: TdxDocumentIntervalList; override;
  end;

  { TdxRangePermissionBoundaryUpdateStrategy }

  TdxRangePermissionBoundaryUpdateStrategy = class abstract
  public
    function IsBoundaryAffected(ABoundaryPosition: PdxDocumentModelPosition; ANewRunIndex: TdxRunIndex): Boolean; virtual; abstract;
    procedure ShiftBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); virtual; abstract;
    procedure RemainBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength: Integer); virtual; abstract;
  end;

  { TdxRangePermissionBoundary }

  TdxRangePermissionBoundary = class abstract(TdxVisitableDocumentIntervalBoundary)
  protected
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); virtual; abstract;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); virtual; abstract;
  end;

  { TdxRangePermissionStartBoundary }

  TdxRangePermissionStartBoundary = class(TdxRangePermissionBoundary)
  protected
    function GetPosition: PdxDocumentModelPosition; override;
    function GetOrder: TdxBookmarkBoundaryOrder; override;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
  public
    function CreateBox: TdxVisitableDocumentIntervalBox; override;
    procedure Export(const AExporter: IdxDocumentModelExporter); override;
  end;

  { TdxRangePermissionEndBoundary }

  TdxRangePermissionEndBoundary = class(TdxRangePermissionBoundary)
  protected
    function GetPosition: PdxDocumentModelPosition; override;
    function GetOrder: TdxBookmarkBoundaryOrder; override;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
  public
    function CreateBox: TdxVisitableDocumentIntervalBox; override;
    procedure Export(const AExporter: IdxDocumentModelExporter); override;
  end;

  { TdxRangePermissionColorer }

  TdxRangePermissionColorer = class
  strict private
    FRangeColors: TdxStringColorDictionary;
  protected
    property RangeColors: TdxStringColorDictionary read FRangeColors;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function GetColor(ARangePermission: TdxRangePermission): TdxAlphaColor;
  end;

  { TdxRangePermissionBoundaryIterator }

  TdxRangePermissionBoundaryIterator = class(TdxVisitableDocumentIntervalBoundaryIterator)
  protected
    procedure PopulateBoundariesCore; overload; override;
    procedure PopulateBoundariesCore(AIntervals: TObjectList); overload; override;
  public
    function IsVisibleInterval(AInterval: TdxVisitableDocumentInterval): Boolean; override;
  end;

  { TdxBookmarkBoxCalculator }

  TdxBookmarkBoxCalculator = class
  strict private
    FMeasurerProvider: IdxBoxMeasurerProvider;
    FPieceTable: TdxSimplePieceTable;
    FBookmarkIteratorCache: TObjectDictionary<TdxSimplePieceTable, TdxVisitableDocumentIntervalBoundaryIterator>;
    FBookmarkIterator: TdxVisitableDocumentIntervalBoundaryIterator;
    FExportToPdf: Boolean;
    procedure SetPieceTable(const AValue: TdxSimplePieceTable);
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetMeasurer: TdxBoxMeasurer;
    procedure SetBookmarkIterator(const Value: TdxVisitableDocumentIntervalBoundaryIterator);
  protected
    function GetBookmarkBoxCollection(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection; virtual;
    procedure ClearBookmarkBoxCollection(ARow: TdxSimpleRow); virtual;
    procedure ChangeBookmarkIterator(AOldPieceTable, ANewPieceTable: TdxSimplePieceTable);
    procedure EnsureBookmarkIteratorIsInitialized;
    function ShouldCalculate: Boolean; virtual;

    function CalculateBoxes(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection;
    function GetHorizontalPosition(const APosition: TdxFormatterPosition; ABox: TdxBox): Integer;
    function GetPosition(ABoundary: TdxVisitableDocumentIntervalBoundary): TdxFormatterPosition;
    function GetPrevPosition(const APos: TdxFormatterPosition): TdxFormatterPosition;
    function MeasureBoxPart(const APosition: TdxFormatterPosition; ABox: TdxBox): Integer;
    function CreateBookmarkBoundaryIterator: TdxVisitableDocumentIntervalBoundaryIterator; virtual;
    function GetBoxColor: TdxAlphaColor; virtual;

    property BookmarkIterator: TdxVisitableDocumentIntervalBoundaryIterator read FBookmarkIterator write SetBookmarkIterator;
    property BookmarkIteratorCache: TObjectDictionary<TdxSimplePieceTable, TdxVisitableDocumentIntervalBoundaryIterator> read FBookmarkIteratorCache;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; const AMeasurerProvider: IdxBoxMeasurerProvider);
    destructor Destroy; override;
    procedure Calculate(ARow: TdxSimpleRow);
    procedure ResetFrom(const APosition: TdxDocumentModelPosition);
    procedure Reset;

    property ExportToPdf: Boolean read FExportToPdf write FExportToPdf;
    property PieceTable: TdxSimplePieceTable read FPieceTable write SetPieceTable;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property Measurer: TdxBoxMeasurer read GetMeasurer;
  end;

  { TdxRangePermissionBoxCalculator }

  TdxRangePermissionBoxCalculator = class(TdxBookmarkBoxCalculator)
  protected
    function CreateBookmarkBoundaryIterator: TdxVisitableDocumentIntervalBoundaryIterator; override;
    function ShouldCalculate: Boolean; override;
    function GetBoxColor: TdxAlphaColor; override;
    function GetBookmarkBoxCollection(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection; override;
    procedure ClearBookmarkBoxCollection(ARow: TdxSimpleRow); override;
  end;

implementation

uses
  Math, Character, RTLConsts,
  dxTypeHelpers,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Types,
  dxRichEdit.Options;

type
  { TdxRangePermissionBoundaryUpdater }

  TdxRangePermissionBoundaryUpdater = class
  strict private
    FPieceTable: TdxSimplePieceTable;
    FUpdateStrategy: TdxRangePermissionBoundaryUpdateStrategy;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; const AUpdateStrategy: TdxRangePermissionBoundaryUpdateStrategy);
    destructor Destroy; override;
    procedure Update(ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    function GetAdjoiningBoundariesCount(ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection; AIndex: Integer): Integer;
    procedure ReorderBoundaries(ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection; AIndex, ACount: Integer);
    procedure UpdateAdjoiningBoundaries(ABoundaries: TdxList<TdxVisitableDocumentIntervalBoundary>;
      AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    procedure ShiftBoundaries(ABoundaries: TdxList<TdxVisitableDocumentIntervalBoundary>; AIndex: Integer;
      AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    function ShouldUpdateBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary): Boolean;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property UpdateStrategy: TdxRangePermissionBoundaryUpdateStrategy read FUpdateStrategy;
  end;

  { TdxRunInsertedUpdateStrategy }

  TdxRunInsertedUpdateStrategy = class(TdxRangePermissionBoundaryUpdateStrategy)
  public
    function IsBoundaryAffected(ABoundaryPosition: PdxDocumentModelPosition; ANewRunIndex: TdxRunIndex): Boolean; override;
    procedure ShiftBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure RemainBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength: Integer); override;
  end;

  { TdxRunMergedUpdateStrategy }

  TdxRunMergedUpdateStrategy = class(TdxRangePermissionBoundaryUpdateStrategy)
  public
    function IsBoundaryAffected(ABoundaryPosition: PdxDocumentModelPosition; ANewRunIndex: TdxRunIndex): Boolean; override;
    procedure ShiftBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); override;
    procedure RemainBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength: Integer); override;
  end;

{ TdxRangePermissionBoundaryUpdater }

constructor TdxRangePermissionBoundaryUpdater.Create(APieceTable: TdxSimplePieceTable; const AUpdateStrategy: TdxRangePermissionBoundaryUpdateStrategy);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FUpdateStrategy := AUpdateStrategy;
end;

destructor TdxRangePermissionBoundaryUpdater.Destroy;
begin
  FUpdateStrategy.Free;
  inherited Destroy;
end;

procedure TdxRangePermissionBoundaryUpdater.Update(ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
var
  ACount, AIndex, ARangeLength: Integer;
  APosition: PdxDocumentModelPosition;
  ARange: TdxList<TdxVisitableDocumentIntervalBoundary>;
begin
  ACount := ABoundaries.Count;
  AIndex := 0;
  while AIndex < ACount do
  begin
    APosition := ABoundaries[AIndex].Position;
    if FUpdateStrategy.IsBoundaryAffected(APosition, ANewRunIndex) then
    begin
      ARangeLength := GetAdjoiningBoundariesCount(ABoundaries, AIndex);
      ReorderBoundaries(ABoundaries, AIndex, ARangeLength);
      ARange := ABoundaries.GetRange(AIndex, ARangeLength);
      try
        UpdateAdjoiningBoundaries(ARange, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
      finally
        ARange.Free;
      end;
      Inc(AIndex, ARangeLength);
    end
    else
    begin
      FUpdateStrategy.ShiftBoundary(ABoundaries[AIndex], AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
      Inc(AIndex);
    end;
  end;
end;

function TdxRangePermissionBoundaryUpdater.GetAdjoiningBoundariesCount(
  ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection; AIndex: Integer): Integer;
var
  APosition: TdxDocumentLogPosition;
  ACount, I: Integer;
begin
  APosition := ABoundaries[AIndex].Position.LogPosition;
  ACount := ABoundaries.Count;
  Result := 1;
  for I := AIndex + 1 to ACount - 1 do
  begin
    if ABoundaries[I].Position.LogPosition <> APosition then
      Break;
    Inc(Result);
  end;
end;

procedure TdxRangePermissionBoundaryUpdater.ReorderBoundaries(ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
  AIndex, ACount: Integer);
begin
  ABoundaries.Sort(AIndex, ACount, TdxRangePermission.BoundaryOrderComparer);
end;

procedure TdxRangePermissionBoundaryUpdater.UpdateAdjoiningBoundaries(ABoundaries: TdxList<TdxVisitableDocumentIntervalBoundary>;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
var
  ACount, AStartIndex, AIndex: Integer;
  ABoundary: TdxVisitableDocumentIntervalBoundary;
begin
  ACount := ABoundaries.Count;
  AStartIndex := 0;
  for AIndex := 0 to ACount - 1 do
  begin
    ABoundary := ABoundaries[AIndex];
    if (AIndex > 0) and (ABoundary.Order <> ABoundaries[AIndex - 1].Order) then
      AStartIndex := AIndex;
    if ShouldUpdateBoundary(ABoundary) then
    begin
      ShiftBoundaries(ABoundaries, AStartIndex, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
      Exit;
    end;
  end;
end;

procedure TdxRangePermissionBoundaryUpdater.ShiftBoundaries(ABoundaries: TdxList<TdxVisitableDocumentIntervalBoundary>;
  AIndex: Integer; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
var
  ACount, I: Integer;
begin
  ACount := ABoundaries.Count;
  for I := 0 to ACount - 1 do
  begin
    if I < AIndex then
      FUpdateStrategy.RemainBoundary(ABoundaries[I], AParagraphIndex, ANewRunIndex, ALength)
    else
      FUpdateStrategy.ShiftBoundary(ABoundaries[I], AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  end;
end;

function TdxRangePermissionBoundaryUpdater.ShouldUpdateBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary): Boolean;
var
  ARangePermission: TdxRangePermission;
begin
  if ABoundary.Order = TdxBookmarkBoundaryOrder.Start then
    Exit(False);
  if PieceTable.DocumentModel.IsDocumentProtectionEnabled then
  begin
    ARangePermission := TdxRangePermission(ABoundary.VisitableInterval);
    Result := ARangePermission.IsGranted(PieceTable);
  end
  else
    Result := True;
end;

{ TdxRunInsertedUpdateStrategy }

function TdxRunInsertedUpdateStrategy.IsBoundaryAffected(ABoundaryPosition: PdxDocumentModelPosition;
  ANewRunIndex: TdxRunIndex): Boolean;
begin
  Result := (ABoundaryPosition.RunIndex = ANewRunIndex) and (ABoundaryPosition.RunOffset = 0);
end;

procedure TdxRunInsertedUpdateStrategy.ShiftBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermissionBoundary(ABoundary).OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRunInsertedUpdateStrategy.RemainBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer);
begin
end;

{ TdxRunMergedUpdateStrategy }

function TdxRunMergedUpdateStrategy.IsBoundaryAffected(ABoundaryPosition: PdxDocumentModelPosition;
  ANewRunIndex: TdxRunIndex): Boolean;
begin
  Result := (ABoundaryPosition.RunIndex = ANewRunIndex + 1) and (ABoundaryPosition.RunOffset = 0);
end;

procedure TdxRunMergedUpdateStrategy.ShiftBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermissionBoundary(ABoundary).OnRunMerged(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRunMergedUpdateStrategy.RemainBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary;
  AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer);
var
  APos: PdxDocumentModelPosition;
  ARun: TdxRunBase;
begin
  APos := ABoundary.Position;
  APos.RunIndex := ANewRunIndex;
  ARun := APos.PieceTable.Runs[APos.RunIndex];
  APos.RunStartLogPosition := APos.LogPosition - ARun.Length + ALength;
  APos.ParagraphIndex := ARun.Paragraph.Index;
end;

{ TdxBookmarkBase.TdxHistoryNotificationsAccessor }

constructor TdxBookmarkBase.TdxHistoryNotificationsAccessor.Create(AGetter: TdxGetDelegate; ASetter: TdxSetDelegate);
begin
  inherited Create;
  FGetter := AGetter;
  FSetter := ASetter;
end;

function TdxBookmarkBase.TdxHistoryNotificationsAccessor.GetChangeNotificationIds: TdxIntegerList;
begin
  Result := FGetter();
end;

procedure TdxBookmarkBase.TdxHistoryNotificationsAccessor.SetChangeNotificationIds(AValue: TdxIntegerList);
begin
  FSetter(AValue);
end;

function TdxBookmarkBase.TdxHistoryNotificationsAccessor.ShouldChange(AHistoryNotificationId: Integer): Boolean;
var
  AIndex: Integer;
begin
  if (ChangeNotificationIds = nil) or (AHistoryNotificationId = TdxNotificationIdGenerator.EmptyId) then
    Result := False
  else
    Result := ChangeNotificationIds.BinarySearch(AHistoryNotificationId, AIndex);
end;

procedure TdxBookmarkBase.TdxHistoryNotificationsAccessor.AddChangeNotificationId(AHistoryNotificationId: Integer);
var
  AIndex: Integer;
begin
  if AHistoryNotificationId = TdxNotificationIdGenerator.EmptyId then
    Exit;

  if ChangeNotificationIds = nil then
    ChangeNotificationIds := TdxIntegerList.Create;
  if not ChangeNotificationIds.BinarySearch(AHistoryNotificationId, AIndex) then
    ChangeNotificationIds.Insert(AIndex, AHistoryNotificationId);
end;

{ TdxBookmarkBase }

constructor TdxBookmarkBase.Create(APieceTable: TdxCustomPieceTable; AStart, AEnd: TdxDocumentLogPosition;
  AForceUpdateInterval: Boolean = False);
begin
  inherited Create(APieceTable);
  FCanExpand := True;
  FForceUpdateInterval := AForceUpdateInterval;
  SetStartCore(AStart);
  SetEndCore(AEnd);
end;

destructor TdxBookmarkBase.Destroy;
begin
  FreeAndNil(FStartChangeNotificationIds);
  FreeAndNil(FEndChangeNotificationIds);
  inherited Destroy;
end;

procedure TdxBookmarkBase.UpdateStartPosition;
begin
  if not FForceUpdateInterval then
    if DocumentModel.IsUpdateLocked and DocumentModel.DeferredChanges.IsSetContentMode then
      Exit;
  inherited UpdateStartPosition;
end;

procedure TdxBookmarkBase.UpdateEndPosition;
begin
  if not FForceUpdateInterval then
    if DocumentModel.IsUpdateLocked and DocumentModel.DeferredChanges.IsSetContentMode then
      Exit;
  inherited UpdateEndPosition;
end;

procedure TdxBookmarkBase.UpdateInterval;
begin
  inherited UpdateStartPosition;
  inherited UpdateEndPosition;
end;

procedure TdxBookmarkBase.OnChangedCore;
begin
end;

function TdxBookmarkBase.GetStartChangeNotificationIds: TdxIntegerList;
begin
  Result := FStartChangeNotificationIds;
end;

procedure TdxBookmarkBase.SetStartChangeNotificationIds(AValue: TdxIntegerList);
begin
  if FStartChangeNotificationIds <> AValue then
  begin
    FreeAndNil(FStartChangeNotificationIds);
    FStartChangeNotificationIds := AValue;
  end;
end;

function TdxBookmarkBase.GetEndChangeNotificationIds: TdxIntegerList;
begin
  Result := FEndChangeNotificationIds;
end;

procedure TdxBookmarkBase.SetEndChangeNotificationIds(AValue: TdxIntegerList);
begin
  if FEndChangeNotificationIds <> AValue then
  begin
    FreeAndNil(FEndChangeNotificationIds);
    FEndChangeNotificationIds := AValue;
  end;
end;

procedure TdxBookmarkBase.RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer;
  AHistoryNotificationId: Integer);
var
  AStartAccessor, AEndAccessor: TdxHistoryNotificationsAccessor;
  AStartChanged, AEndChanged: Boolean;
begin
  AStartAccessor := TdxHistoryNotificationsAccessor.Create(GetStartChangeNotificationIds, SetStartChangeNotificationIds);
  try
    AEndAccessor := TdxHistoryNotificationsAccessor.Create(GetEndChangeNotificationIds, SetEndChangeNotificationIds);
    try
      AStartChanged := RunInserted(@Interval.Start, ANewRunIndex, ALength, AStartAccessor, AHistoryNotificationId);
      AEndChanged := RunInserted(@Interval.&End, ANewRunIndex, ALength, AEndAccessor, AHistoryNotificationId);
    finally
      AEndAccessor.Free;
    end;
  finally
    AStartAccessor.Free;
  end;
  OnChanged(AStartChanged, AEndChanged);
end;

function TdxBookmarkBase.RunInserted(APosition: PdxDocumentModelPosition; ANewRunIndex: TdxRunIndex; ALength: Integer;
  AAccessor: TdxHistoryNotificationsAccessor; AHistoryNotificationId: Integer): Boolean;
begin
  if (ANewRunIndex < APosition^.RunIndex) or ((ANewRunIndex = APosition^.RunIndex) and ((APosition^.RunOffset > 0) or
    (AAccessor.ShouldChange(AHistoryNotificationId)))) then
  begin
    APosition^.RunIndex := APosition^.RunIndex + 1;
    APosition^.RunStartLogPosition := APosition^.RunStartLogPosition + ALength;
    APosition^.LogPosition := APosition^.LogPosition + ALength;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdxBookmarkBase.RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer;
  AHistoryNotificationId: Integer);
var
  AStartAccessor, AEndAccessor: TdxHistoryNotificationsAccessor;
  AStartChanged, AEndChanged: Boolean;
begin
  AStartAccessor := TdxHistoryNotificationsAccessor.Create(GetStartChangeNotificationIds, SetStartChangeNotificationIds);
  try
    AEndAccessor := TdxHistoryNotificationsAccessor.Create(GetEndChangeNotificationIds, SetEndChangeNotificationIds);
    try
      AStartChanged := RunRemoved(@Interval.Start, ARunIndex, ALength, AStartAccessor, AHistoryNotificationId);
      AEndChanged := RunRemoved(@Interval.&End, ARunIndex, ALength, AEndAccessor, AHistoryNotificationId);
    finally
      AEndAccessor.Free;
    end;
  finally
    AStartAccessor.Free;
  end;
  OnChanged(AStartChanged, AEndChanged);
end;

function TdxBookmarkBase.RunRemoved(APos: PdxDocumentModelPosition; ARunIndex: TdxRunIndex; ALength: Integer;
  AAccessor: TdxHistoryNotificationsAccessor; AHistoryNotificationId: Integer): Boolean;
begin
  if ARunIndex = APos^.RunIndex then
  begin
    if APos^.RunOffset > 0 then
      APos^.LogPosition := APos^.LogPosition - APos^.RunOffset;
    Result := True;
  end
  else
    if ARunIndex < APos^.RunIndex then
    begin
      if (ARunIndex = (APos^.RunIndex - 1)) and (APos^.RunOffset = 0) then
        AAccessor.AddChangeNotificationId(AHistoryNotificationId);
      APos^.RunIndex := APos^.RunIndex - 1;
      APos^.RunStartLogPosition := APos^.RunStartLogPosition - ALength;
      APos^.LogPosition := APos^.LogPosition - ALength;
      Result := True;
    end
    else
      Result := False;
end;

function TdxBookmarkBase.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxBookmarkBaseList }

function TdxBookmarkBaseList.GetItem(Index: Integer): TdxBookmarkBase;
begin
  Result := TdxBookmarkBase(inherited Items[Index]);
end;

{ TdxBookmarkInfo }

constructor TdxBookmarkInfo.Create(ABookmark: TdxBookmarkBase);
begin
  inherited Create;
  Assert(ABookmark <> nil);
  FBookmark := ABookmark;
end;

{ TdxBookmark }

procedure TdxBookmark.Delete(AIndex: Integer);
begin
  TdxPieceTable(PieceTable).DeleteBookmarkCore(AIndex);
end;

function TdxBookmark.GetIsHidden: Boolean;
begin
  Result := (FName = '') or (FName[1] = '_')
end;

class function TdxBookmark.IsNameValid(const AName: string): Boolean;
begin
  Result := IsNameValidCore(Trim(AName));
end;

class function TdxBookmark.IsNameValidCore(const AName: string): Boolean;
var
  C: Char;
begin
  if (System.Length(AName) > 0) and not {$IFDEF DELPHIXE4}AName[1].IsLetter{$ELSE}TCharacter.IsLetter(AName[1]){$ENDIF} then
     Exit(False);
  for C in AName do
    if not {$IFDEF DELPHIXE4}C.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(C){$ENDIF} and (C <> '_') then
      Exit(False);
  Result := True;
end;

procedure TdxBookmark.Visit(const AVisitor: IdxDocumentIntervalVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxBookmarkList }

function TdxBookmarkList.GetItem(Index: Integer): TdxBookmark;
begin
  Result := TdxBookmark(inherited Items[Index]);
end;

{ TdxBookmarkBaseCollection }

procedure TdxBookmarkBaseCollection.Clear;
begin
  InnerList.Clear;
end;

procedure TdxBookmarkBaseCollection.UpdateIntervals;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Self[I].UpdateInterval;
end;

function TdxBookmarkBaseCollection.GetEntireBookmarksCore(AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
var
  I: Integer;
  AEnd: TdxDocumentLogPosition;
begin
  AEnd := AStart + ALength;
  Result := TdxBookmarkBaseList.Create;
  for I := 0 to Count - 1 do
    if (Self[I].Start >= AStart) and (Self[I].&End <= AEnd) and (Self[I].Start < AEnd) then
      Result.Add(Self[I]);
end;

function TdxBookmarkBaseCollection.GetItem(Index: Integer): TdxBookmarkBase;
begin
  Result := TdxBookmarkBase(inherited Items[Index]);
end;

{ TdxBookmarkCollection }

function TdxBookmarkCollection.FindByName(const AName: string): TdxBookmark;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Self[I].Name = AName then
    begin
      Result := Self[I];
      Break;
    end;
end;

function TdxBookmarkCollection.GetItem(Index: Integer): TdxBookmark;
begin
  Result := TdxBookmark(inherited Items[Index]);
end;

{ TdxVisitableDocumentIntervalBoundary }

constructor TdxVisitableDocumentIntervalBoundary.Create(
  AInterval: TdxVisitableDocumentInterval);
begin
  inherited Create;
  FIntervalIndex := -1;
  FInterval := AInterval;
end;

{ TdxBookmarkStartBoundary }

function TdxBookmarkStartBoundary.GetPosition: PdxDocumentModelPosition;
begin
  Result := @VisitableInterval.Interval.Start;
end;

function TdxBookmarkStartBoundary.GetOrder: TdxBookmarkBoundaryOrder;
begin
  Result := TdxBookmarkBoundaryOrder.Start;
end;

procedure TdxBookmarkStartBoundary.Export(const AExporter: IdxDocumentModelExporter);
begin
  AExporter.ExportBookmarkStart(TdxBookmark(VisitableInterval));
end;

function TdxBookmarkStartBoundary.CreateBox: TdxVisitableDocumentIntervalBox;
begin
  Result := TdxBookmarkStartBox.Create;
end;

{ TdxBookmarkEndBoundary }

function TdxBookmarkEndBoundary.GetPosition: PdxDocumentModelPosition;
begin
  Result := @VisitableInterval.Interval.&End;
end;

function TdxBookmarkEndBoundary.GetOrder: TdxBookmarkBoundaryOrder;
begin
  Result := TdxBookmarkBoundaryOrder.&End;
end;

procedure TdxBookmarkEndBoundary.Export(const AExporter: IdxDocumentModelExporter);
begin
  AExporter.ExportBookmarkEnd(TdxBookmark(VisitableInterval));
end;

function TdxBookmarkEndBoundary.CreateBox: TdxVisitableDocumentIntervalBox;
begin
  Result := TdxBookmarkEndBox.Create;
end;

{ TdxVisitableDocumentIntervalBoundaryCollection }

function TdxVisitableDocumentIntervalBoundaryCollection.GetRange(AIndex, ALength: Integer): TdxList<TdxVisitableDocumentIntervalBoundary>;
var
  I: Integer;
begin
  Result := TdxList<TdxVisitableDocumentIntervalBoundary>.Create;
  for I := AIndex to AIndex + ALength - 1 do
    Result.Add(Items[I]);
end;

procedure TdxVisitableDocumentIntervalBoundaryCollection.Sort(AIndex, ACount: Integer;
  const AComparer: IComparer<TdxVisitableDocumentIntervalBoundary>);
var
  AOwsItems: Boolean;
begin
  if (AIndex < 0) or ((AIndex >= Count) and (ACount > 0))
    or (AIndex + ACount - 1 >= Count) or (ACount < 0)
    or (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount <= 1 then
    Exit;
  AOwsItems := OwnsObjects;
  OwnsObjects := False;
  try
    QuickSort(AComparer, AIndex, AIndex + ACount - 1);
  finally
    OwnsObjects := AOwsItems;
  end;
end;

procedure TdxVisitableDocumentIntervalBoundaryCollection.QuickSort(
  const AComparer: IComparer<TdxVisitableDocumentIntervalBoundary>; L, R: Integer);
var
  I, J: Integer;
  APivot, ATemp: TdxVisitableDocumentIntervalBoundary;
begin
  if (Count = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    APivot := Items[L + (R - L) shr 1];
    repeat
      while AComparer.Compare(Items[I], APivot) < 0 do
        Inc(I);
      while AComparer.Compare(Items[J], APivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          ATemp := Items[I];
          Items[I] := Items[J];
          Items[J] := ATemp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AComparer, L, J);
    L := I;
  until I >= R;
end;


{ TdxVisitableDocumentIntervalStartBoundaryFactory }

procedure TdxVisitableDocumentIntervalStartBoundaryFactory.Visit(AInterval: TdxVisitableDocumentInterval);
begin
  if AInterval is TdxBookmark then
    FBoundary := TdxBookmarkStartBoundary.Create(AInterval)
  else if AInterval is TdxRangePermission then
    FBoundary := TdxRangePermissionStartBoundary.Create(AInterval)
end;

procedure TdxVisitableDocumentIntervalStartBoundaryFactory.SetBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary);
begin
  FBoundary := ABoundary;
end;

{ TdxVisitableDocumentIntervalEndBoundaryFactory }

procedure TdxVisitableDocumentIntervalEndBoundaryFactory.Visit(AInterval: TdxVisitableDocumentInterval);
begin
  if AInterval is TdxBookmark then
    FBoundary := TdxBookmarkEndBoundary.Create(AInterval)
  else if AInterval is TdxRangePermission then
    FBoundary := TdxRangePermissionEndBoundary.Create(AInterval)
end;

procedure TdxVisitableDocumentIntervalEndBoundaryFactory.SetBoundary(ABoundary: TdxVisitableDocumentIntervalBoundary);
begin
  FBoundary := ABoundary;
end;

{ TdxVisitableDocumentIntervalBoundaryIterator.TComparer }

function TdxVisitableDocumentIntervalBoundaryIterator.TComparer.Compare(const Left,
  Right: TdxVisitableDocumentIntervalBoundary): Integer;
begin
  Result := Left.Position.LogPosition - Right.Position.LogPosition;
  if Result = 0 then
  begin
    Result := Left.IntervalIndex - Right.IntervalIndex;
    if Result = 0 then
      Result := Ord(Left.Order) - Ord(Right.Order);
  end;
end;

{ TdxVisitableDocumentIntervalBoundaryIterator }

constructor TdxVisitableDocumentIntervalBoundaryIterator.Create(
  APieceTable: TdxSimplePieceTable; AIncludeHiddenIntervals: Boolean);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FIncludeHiddenIntervals := AIncludeHiddenIntervals;
  FVisitableDocumentIntervalBoundaries := TdxVisitableDocumentIntervalBoundaryCollection.Create;
  InitializeBoundaries;
end;

destructor TdxVisitableDocumentIntervalBoundaryIterator.Destroy;
begin
  FreeAndNil(FVisitableDocumentIntervalBoundaries);
  inherited Destroy;
end;

class constructor TdxVisitableDocumentIntervalBoundaryIterator.Initialize;
begin
  FComparer := TComparer.Create;
end;

class destructor TdxVisitableDocumentIntervalBoundaryIterator.Finalize;
begin
  FComparer.Free;
end;

function TdxVisitableDocumentIntervalBoundaryIterator.InternalGetCurrent: TdxVisitableDocumentIntervalBoundary;
begin
  Result := FVisitableDocumentIntervalBoundaries[FCurrentIndex];
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.InitializeBoundaries;
begin
  PopulateBoundaries;
  FVisitableDocumentIntervalBoundaries.Sort(FComparer);
end;

function TdxVisitableDocumentIntervalBoundaryIterator.IsDone: Boolean;
begin
  Result := FCurrentIndex >= FVisitableDocumentIntervalBoundaries.Count;
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.MoveNext;
begin
  Inc(FCurrentIndex);
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.Reset;
begin
  FCurrentIndex := 0;
end;

function TdxVisitableDocumentIntervalBoundaryIterator.CreateVisitableDocumentIntervalStartBoundaryFactory: TdxVisitableDocumentIntervalStartBoundaryFactory;
begin
   Result := TdxVisitableDocumentIntervalStartBoundaryFactory.Create;
end;

function TdxVisitableDocumentIntervalBoundaryIterator.CreateVisitableDocumentIntervalEndBoundaryFactory: TdxVisitableDocumentIntervalEndBoundaryFactory;
begin
   Result := TdxVisitableDocumentIntervalEndBoundaryFactory.Create;
end;

function TdxVisitableDocumentIntervalBoundaryIterator.IsVisibleInterval(AInterval: TdxVisitableDocumentInterval): Boolean;
begin
  Result := True;
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.PopulateBoundaries;
begin
  FVisitableDocumentIntervalBoundaries.Clear;
  PopulateBoundariesCore;
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.PopulateBookmarksCore(ABookmarks: TdxBookmarkList);
begin
  PopulateBoundariesCore(ABookmarks);
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.PopulateBoundariesCore;
var
  ABookmarks: TdxBookmarkList;
begin
  ABookmarks := TdxPieceTable(PieceTable).GetBookmarks(FIncludeHiddenIntervals);
  try
    PopulateBookmarksCore(ABookmarks);
  finally
    ABookmarks.Free;
  end;
end;

procedure TdxVisitableDocumentIntervalBoundaryIterator.PopulateBoundariesCore(
  AIntervals: TObjectList);
var
  I: Integer;
  AInterval: TdxVisitableDocumentInterval;
  AStartBoundary, AEndBoundary: TdxVisitableDocumentIntervalBoundary;
  AStartBoundaryFactory: TdxVisitableDocumentIntervalStartBoundaryFactory;
  AEndBoundaryFactory: TdxVisitableDocumentIntervalEndBoundaryFactory;
begin
  AStartBoundaryFactory := CreateVisitableDocumentIntervalStartBoundaryFactory;
  try
    AEndBoundaryFactory := CreateVisitableDocumentIntervalEndBoundaryFactory;
    try
      for I := 0 to AIntervals.Count - 1 do
      begin
        AInterval := AIntervals[I] as TdxVisitableDocumentInterval;
        if not IsVisibleInterval(AInterval) then
          Continue;

        AInterval.Visit(AStartBoundaryFactory);
        AStartBoundary := AStartBoundaryFactory.Boundary;
        AStartBoundary.IntervalIndex := I;
        FVisitableDocumentIntervalBoundaries.Add(AStartBoundary);

        AInterval.Visit(AEndBoundaryFactory);
        AEndBoundary := AEndBoundaryFactory.Boundary;
        AEndBoundary.IntervalIndex := I;
        FVisitableDocumentIntervalBoundaries.Add(AEndBoundary);
      end;
    finally
      AEndBoundaryFactory.Free;
    end;
  finally
    AStartBoundaryFactory.Free;
  end;
end;

{ TdxVisitableDocumentIntervalBasedObjectBoundaryIterator }

procedure TdxVisitableDocumentIntervalBasedObjectBoundaryIterator.PopulateBoundariesCore;
begin
  inherited PopulateBoundariesCore;
  PopulateBoundariesCore(TdxPieceTable(PieceTable).RangePermissions.InnerList);
end;

procedure TdxVisitableDocumentIntervalBasedObjectBoundaryIterator.PopulateBoundariesCore(AIntervals: TObjectList);
begin
  inherited PopulateBoundariesCore(AIntervals);
end;

{ TdxRangePermission.TBoundaryComparer }

function TdxRangePermission.TBoundaryComparer.Compare(const ALeft, ARight: TdxVisitableDocumentIntervalBoundary): Integer;
begin
  Result := ALeft.Position.LogPosition - ARight.Position.LogPosition;
end;

{ TdxRangePermission.TBoundaryOrderComparer }

function TdxRangePermission.TBoundaryOrderComparer.Compare(const ALeft, ARight: TdxVisitableDocumentIntervalBoundary): Integer;
begin
  Result := ALeft.VisitableInterval.NormalizedStart - ARight.VisitableInterval.NormalizedStart;
  if Result <> 0 then
    Exit(Result);
  Result := ALeft.VisitableInterval.NormalizedEnd - ARight.VisitableInterval.NormalizedEnd;
  if Result <> 0 then
    Exit(Result);
  Result := Ord(ALeft.Order) - Ord(ARight.Order);
end;

{ TdxRangePermission }

constructor TdxRangePermission.Create(APieceTable: TdxSimplePieceTable; const AStart, AEnd: TdxDocumentLogPosition; APropertiesIndex: Integer);
begin
  Create(APieceTable, AStart, AEnd);
  Properties.SetIndexInitial(APropertiesIndex);
end;

constructor TdxRangePermission.Create(APieceTable: TdxSimplePieceTable; const AStart, AEnd: TdxDocumentLogPosition);
begin
  inherited Create(APieceTable, AStart, AEnd);
  FProperties := TdxRangePermissionProperties.Create(PieceTable);
end;

destructor TdxRangePermission.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

class constructor TdxRangePermission.Initialize;
begin
  FBoundaryComparer := TBoundaryComparer.Create;
  FBoundaryOrderComparer := TBoundaryOrderComparer.Create;
end;

class destructor TdxRangePermission.Finalize;
begin
  FBoundaryComparer.Free;
  FBoundaryOrderComparer.Free;
end;

function TdxRangePermission.Clone: TdxRangePermission;
begin
  Result := TdxRangePermission.Create(PieceTable, Start, &End, Properties.Index);
  Result.Group := Group;
  Result.UserName := UserName;
end;

function TdxRangePermission.GetUserName: string;
begin
  Result := FProperties.UserName;
end;

procedure TdxRangePermission.SetUserName(const AValue: string);
begin
  FProperties.UserName := AValue;
end;

function TdxRangePermission.GetGroup: string;
begin
  Result := FProperties.Group;
end;

procedure TdxRangePermission.SetGroup(const AValue: string);
begin
  FProperties.Group := AValue;
end;

procedure TdxRangePermission.Visit(const AVisitor: IdxDocumentIntervalVisitor);
begin
  AVisitor.Visit(Self);
end;

function TdxRangePermission.Subtract(AInterval: TdxRangePermission): TdxRangePermissionCollection;
begin
  Result := TdxRangePermissionMergedCollection.Create(PieceTable);

  if (AInterval = nil) or not IntersectsWithExcludingBounds(AInterval) then
  begin
    Result.Add(Self);
    Exit;
  end;

  if AInterval.Contains(Self) then
  begin
    AInterval.Free;
    Exit;
  end;

  if Contains(AInterval) then
  begin
    if Start < AInterval.Start then
      Result.Add(TdxRangePermission.Create(PieceTable, Start, AInterval.Start, Properties.Index));
    if &End > AInterval.&End then
      Result.Add(TdxRangePermission.Create(PieceTable, AInterval.&End, &End, Properties.Index));
  end
  else
  begin
    if Start >= AInterval.Start then
      Result.Add(TdxRangePermission.Create(PieceTable, AInterval.&End, &End, Properties.Index))
    else
      Result.Add(TdxRangePermission.Create(PieceTable, Start, AInterval.Start, Properties.Index));
  end;
  AInterval.Free;
end;

class function TdxRangePermission.Union(AInterval1: TdxRangePermission; AInterval2: TdxRangePermission): TdxRangePermission;
var
  AStart, AEnd: TdxDocumentLogPosition;
begin
  AStart := Min(AInterval1.Start, AInterval2.Start);
  AEnd := Max(AInterval1.&End, AInterval2.&End);
  Result := TdxRangePermission.Create(AInterval1.PieceTable, AStart, AEnd, AInterval1.Properties.Index);
end;

function TdxRangePermission.IsGranted(APieceTable: TdxSimplePieceTable): Boolean;
begin
  Result := TdxPieceTable(APieceTable).IsPermissionGranted(Self);
end;

procedure TdxRangePermission.Delete(AIndex: Integer);
begin
  TdxPieceTable(PieceTable).DeleteRangePermission(Self);
end;

function TdxRangePermission.OnRunInsertedStart(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer): Boolean;
var
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  AAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AAnchor.OnRunInserted(PieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
    OnChanged(AAnchor.PositionChanged, False);
    Result := AAnchor.PositionChanged;
  finally
    AAnchor.Free;
  end;
end;

function TdxRangePermission.OnRunMergedStart(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer): Boolean;
var
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  AAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AAnchor.OnRunMerged(PieceTable, AParagraphIndex, ANewRunIndex, ALength);
    OnChanged(AAnchor.PositionChanged, False);
    Result := AAnchor.PositionChanged;
  finally
    AAnchor.Free;
  end;
end;

function TdxRangePermission.OnRunInsertedEnd(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer): Boolean;
var
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  AAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
  try
    AAnchor.OnRunInserted(PieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
    OnChanged(False, AAnchor.PositionChanged);
    Result := AAnchor.PositionChanged;
  finally
    AAnchor.Free;
  end;
end;

function TdxRangePermission.OnRunMergedEnd(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer): Boolean;
var
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  AAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
  try
    AAnchor.OnRunMerged(PieceTable, AParagraphIndex, ANewRunIndex, ALength);
    OnChanged(False, AAnchor.PositionChanged);
    Result := AAnchor.PositionChanged;
  finally
    AAnchor.Free;
  end;
end;

{ TdxRangePermissionList }

function TdxRangePermissionList.GetItem(Index: Integer): TdxRangePermission;
begin
  Result := TdxRangePermission(inherited Items[Index]);
end;

{ TdxRangePermissionCollection }

destructor TdxRangePermissionCollection.Destroy;
begin
  FBoundaries.Free;
  inherited Destroy;
end;

function TdxRangePermissionCollection.GetBoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
begin
  if FBoundaries = nil then
    FBoundaries := CreateBoundaryCollection;
  Result := FBoundaries;
end;

function TdxRangePermissionCollection.GetItem(
  Index: Integer): TdxRangePermission;
begin
  Result := TdxRangePermission(inherited Items[Index]);
end;

procedure TdxRangePermissionCollection.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  AUpdater: TdxRangePermissionBoundaryUpdater;
begin
  AUpdater := TdxRangePermissionBoundaryUpdater.Create(PieceTable, TdxRunInsertedUpdateStrategy.Create);
  try
    AUpdater.Update(Boundaries, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  finally
    AUpdater.Free;
  end;
end;

procedure TdxRangePermissionCollection.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  AUpdater: TdxRangePermissionBoundaryUpdater;
begin
  AUpdater := TdxRangePermissionBoundaryUpdater.Create(PieceTable, TdxRunMergedUpdateStrategy.Create);
  try
    AUpdater.Update(Boundaries, AParagraphIndex, ARunIndex, ADeltaRunLength, TdxNotificationIdGenerator.EmptyId);
  finally
    AUpdater.Free;
  end;
end;

function TdxRangePermissionCollection.CreateBoundaryCollection: TdxVisitableDocumentIntervalBoundaryCollection;
var
  I: Integer;
  APermission: TdxRangePermission;
begin
  Result := TdxVisitableDocumentIntervalBoundaryCollection.Create;
  for I := 0 to Count - 1 do
  begin
    APermission := Self[I];
    Result.Add(TdxRangePermissionStartBoundary.Create(APermission));
    Result.Add(TdxRangePermissionEndBoundary.Create(APermission));
  end;
  Result.Sort(TdxRangePermission.BoundaryComparer);
end;

procedure TdxRangePermissionCollection.OnDocumentIntervalInserted(AIndex: Integer);
begin
  inherited OnDocumentIntervalInserted(AIndex);
  FreeAndNil(FBoundaries);
end;

procedure TdxRangePermissionCollection.OnDocumentIntervalRemoved(AIndex: Integer);
begin
  inherited OnDocumentIntervalRemoved(AIndex);
  FreeAndNil(FBoundaries);
end;

{ TdxRangePermissionCollectionEx }

function TdxRangePermissionCollectionEx.AddCore(AInterval: TdxDocumentInterval): Integer;
var
  AToRemove: TdxRangePermissionList;
  ACount, I: Integer;
  APermission: TdxRangePermission absolute AInterval;
  AUnitedInterval: TdxRangePermission;
begin
  if Contains(APermission) then
  begin
    AInterval.Free;
    Exit(0);
  end;

  AUnitedInterval := APermission;
  AToRemove := TdxRangePermissionList.Create(True);
  try
    ACount := Count;
    for I := 0 to ACount - 1 do
    begin
      if AUnitedInterval.IntersectsWith(Self[I]) then
      begin
        APermission := TdxRangePermission.Union(Self[I], AUnitedInterval);
        AUnitedInterval.Free;
        AUnitedInterval := APermission;
        AToRemove.Add(InnerList[I]);
      end;
    end;

    RemoveCore(AToRemove);
  finally
    AToRemove.Free;
  end;

  InnerList.Add(AUnitedInterval);
  Result := Count - 1;
end;

function TdxRangePermissionCollectionEx.Remove(AInterval: TdxRangePermission): Boolean;
var
  AIndex, ACount, I, ASubtractResultCount, ASubtractResultIndex: Integer;
  AToRemove, AToAdd: TdxRangePermissionList;
  ASubtractResult: TdxRangePermissionCollection;
begin
  if AInterval = nil then
    Exit(False);

  AIndex := InnerList.IndexOf(AInterval);
  if AIndex >= 0 then
  begin
    InnerList.Delete(AIndex);
    AInterval.Free;
    Exit(True);
  end;

  AToRemove := TdxRangePermissionList.Create(True);
  AToAdd := TdxRangePermissionList.Create;
  try
    ACount := Count;
    for I := 0 to ACount - 1 do
    begin
      if AInterval.IntersectsWithExcludingBounds(Self[I]) then
      begin
        AToRemove.Add(Self[I]);
        ASubtractResult := Self[I].Subtract(AInterval.Clone);
        try
          ASubtractResultCount := ASubtractResult.Count;
          for ASubtractResultIndex := 0 to ASubtractResultCount - 1 do
            AToAdd.Add(ASubtractResult[ASubtractResultIndex]);
        finally
          ASubtractResult.Free;
        end;
      end;
    end;
    RemoveCore(AToRemove);
    AddRange(AToAdd);
  finally
    AToRemove.Free;
    AToAdd.Free;
    AInterval.Free;
  end;

  Result := True;
end;

function TdxRangePermissionCollectionEx.Contains(AInterval: TdxRangePermission): Boolean;
var
  ACount, I: Integer;
begin
  if InnerList.Contains(AInterval) then
    Exit(True);

  ACount := Count;
  for I := 0 to ACount - 1 do
    if Self[I].Contains(AInterval) then
      Exit(True);

  Result := False;
end;

procedure TdxRangePermissionCollectionEx.RemoveCore(AToRemove: TdxRangePermissionList);
var
  ACount, I: Integer;
begin
  ACount := AToRemove.Count;
  for I := 0 to ACount - 1 do
    InnerList.Remove(AToRemove[I]);
end;

procedure TdxRangePermissionCollectionEx.AddRange(AToAdd: TdxRangePermissionList);
var
  ACount, I: Integer;
begin
  ACount := AToAdd.Count;
  for I := 0 to ACount - 1 do
    InnerList.Add(AToAdd[I]);
end;

{ TdxRangePermissionMergedCollection }
function TdxRangePermissionMergedCollection.CreateInnerList: TdxDocumentIntervalList;
begin
  Result := TdxRangePermissionList.Create;
end;

{ TdxOwnedRangePermissionMergedCollection }

function TdxOwnedRangePermissionMergedCollection.CreateInnerList: TdxDocumentIntervalList;
begin
  Result := TdxRangePermissionList.Create(True);
end;

{ TdxRangePermissionStartBoundary }

function TdxRangePermissionStartBoundary.GetPosition: PdxDocumentModelPosition;
begin
  Result := @VisitableInterval.Interval.Start;
end;

function TdxRangePermissionStartBoundary.GetOrder: TdxBookmarkBoundaryOrder;
begin
  Result := TdxBookmarkBoundaryOrder.Start;
end;

procedure TdxRangePermissionStartBoundary.Export(const AExporter: IdxDocumentModelExporter);
begin
  AExporter.ExportRangePermissionStart(TdxRangePermission(VisitableInterval));
end;

function TdxRangePermissionStartBoundary.CreateBox: TdxVisitableDocumentIntervalBox;
begin
  Result := TdxBookmarkStartBox.Create;
end;

procedure TdxRangePermissionStartBoundary.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermission(VisitableInterval).OnRunInsertedStart(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRangePermissionStartBoundary.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermission(VisitableInterval).OnRunMergedStart(AParagraphIndex, ANewRunIndex, ALength);
end;

{ TdxRangePermissionEndBoundary }

function TdxRangePermissionEndBoundary.GetPosition: PdxDocumentModelPosition;
begin
  Result := @VisitableInterval.Interval.&End;
end;

function TdxRangePermissionEndBoundary.GetOrder: TdxBookmarkBoundaryOrder;
begin
  Result := TdxBookmarkBoundaryOrder.&End;
end;

procedure TdxRangePermissionEndBoundary.Export(const AExporter: IdxDocumentModelExporter);
begin
  AExporter.ExportRangePermissionEnd(TdxRangePermission(VisitableInterval));
end;

function TdxRangePermissionEndBoundary.CreateBox: TdxVisitableDocumentIntervalBox;
begin
  Result := TdxBookmarkEndBox.Create;
end;

procedure TdxRangePermissionEndBoundary.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermission(VisitableInterval).OnRunInsertedEnd(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxRangePermissionEndBoundary.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  TdxRangePermission(VisitableInterval).OnRunMergedEnd(AParagraphIndex, ANewRunIndex, ALength);
end;

{ TdxRangePermissionColorer }

constructor TdxRangePermissionColorer.Create;
begin
  FRangeColors := TdxStringColorDictionary.Create;
end;

destructor TdxRangePermissionColorer.Destroy;
begin
  FRangeColors.Free;
  inherited Destroy;
end;

procedure TdxRangePermissionColorer.Reset;
begin
  FRangeColors.Clear;
end;

function TdxRangePermissionColorer.GetColor(ARangePermission: TdxRangePermission): TdxAlphaColor;
var
  AId: string;
  AColor: TdxAlphaColor;
begin
  AId := ARangePermission.UserName;
  if ARangePermission.Group <> '' then
    AId := AId + Format(':%s', [ARangePermission.Group]);
  if RangeColors.ContainsKey(AId) then
    AColor := RangeColors[AId]
  else
  begin
    AColor := TdxRangePermissionOptions.GetColor;
    RangeColors.Add(AId, AColor);
  end;
  Result := AColor;
end;

{ TdxRangePermissionBoundaryIterator }

procedure TdxRangePermissionBoundaryIterator.PopulateBoundariesCore;
begin
  PopulateBoundariesCore(TdxPieceTable(PieceTable).RangePermissions.InnerList);
end;

procedure TdxRangePermissionBoundaryIterator.PopulateBoundariesCore(AIntervals: TObjectList);
begin
  inherited PopulateBoundariesCore(AIntervals);
end;

function TdxRangePermissionBoundaryIterator.IsVisibleInterval(AInterval: TdxVisitableDocumentInterval): Boolean;
var
  ARangePermission: TdxRangePermission;
begin
  ARangePermission := Safe<TdxRangePermission>.Cast(AInterval);
  if ARangePermission = nil then
    Exit(False);
  if not TdxDocumentModel(PieceTable.DocumentModel).ProtectionProperties.EnforceProtection then
    Exit(True);
  Result := ARangePermission.IsGranted(PieceTable);
end;

{ TdxBookmarkBoxCalculator }

constructor TdxBookmarkBoxCalculator.Create(APieceTable: TdxSimplePieceTable; const AMeasurerProvider: IdxBoxMeasurerProvider);
begin
  inherited Create;
  FExportToPdf := False;
  Assert(APieceTable <> nil);
  Assert(AMeasurerProvider <> nil);
  FPieceTable := APieceTable;
  FMeasurerProvider := AMeasurerProvider;
  FBookmarkIteratorCache := TObjectDictionary<TdxSimplePieceTable, TdxVisitableDocumentIntervalBoundaryIterator>.Create([doOwnsValues]);
end;

destructor TdxBookmarkBoxCalculator.Destroy;
begin
  FMeasurerProvider := nil;
  BookmarkIterator := nil;
  FreeAndNil(FBookmarkIteratorCache);
  inherited Destroy;
end;

function TdxBookmarkBoxCalculator.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxBookmarkBoxCalculator.GetBoxColor: TdxAlphaColor;
begin
  Result := TdxDocumentModel(DocumentModel).BookmarkOptions.Color;
end;

function TdxBookmarkBoxCalculator.GetMeasurer: TdxBoxMeasurer;
begin
  Result := FMeasurerProvider.Measurer;
end;

procedure TdxBookmarkBoxCalculator.SetBookmarkIterator(const Value: TdxVisitableDocumentIntervalBoundaryIterator);
begin
  if FBookmarkIterator <> Value then
  begin
    if not FBookmarkIteratorCache.ContainsValue(FBookmarkIterator) then
      FreeAndNil(FBookmarkIterator);
    FBookmarkIterator := Value;
  end;
end;

procedure TdxBookmarkBoxCalculator.SetPieceTable(const AValue: TdxSimplePieceTable);
begin
  if FPieceTable = AValue then
    Exit;
  Assert(AValue <> nil);
  ChangeBookmarkIterator(FPieceTable, AValue);
  FPieceTable := AValue;
end;

function TdxBookmarkBoxCalculator.GetBookmarkBoxCollection(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := ARow.GetBookmarkBoxesCore;
end;

procedure TdxBookmarkBoxCalculator.ClearBookmarkBoxCollection(ARow: TdxSimpleRow);
begin
  ARow.ClearBookmarkBoxes;
end;

procedure TdxBookmarkBoxCalculator.ChangeBookmarkIterator(AOldPieceTable, ANewPieceTable: TdxSimplePieceTable);
var
  AOldBookmarkIterator: TdxVisitableDocumentIntervalBoundaryIterator;
begin
  if BookmarkIterator <> nil then
    if not BookmarkIterator.IsDone then
    begin
      if not FBookmarkIteratorCache.TryGetValue(AOldPieceTable, AOldBookmarkIterator) or
        (AOldBookmarkIterator <> BookmarkIterator) then
        FBookmarkIteratorCache.AddOrSetValue(AOldPieceTable, BookmarkIterator);
    end
    else
    begin
      BookmarkIterator := nil;
      FBookmarkIteratorCache.Remove(AOldPieceTable);
    end;
  FBookmarkIteratorCache.TryGetValue(ANewPieceTable, FBookmarkIterator);
end;

procedure TdxBookmarkBoxCalculator.EnsureBookmarkIteratorIsInitialized;
begin
  if BookmarkIterator <> nil then
    Exit;
  BookmarkIterator := CreateBookmarkBoundaryIterator;
end;

procedure TdxBookmarkBoxCalculator.Calculate(ARow: TdxSimpleRow);
var
  ABoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if not ShouldCalculate then
    Exit;

  EnsureBookmarkIteratorIsInitialized;

  ABoxes := CalculateBoxes(ARow);
  try
    ABoxes.OwnsObjects := False;
    if ABoxes.Count > 0 then
    begin
      ClearBookmarkBoxCollection(ARow);
      GetBookmarkBoxCollection(ARow).AddRange(ABoxes);
    end;
  finally
    ABoxes.Free;
  end;
end;

function TdxBookmarkBoxCalculator.ShouldCalculate: Boolean;
begin
  Result := (TdxDocumentModel(DocumentModel).BookmarkOptions.Visibility = TdxRichEditBookmarkVisibility.Visible) or ExportToPdf;
end;

function TdxBookmarkBoxCalculator.CalculateBoxes(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection;
var
  ABoundary: TdxVisitableDocumentIntervalBoundary;
  APosition: TdxFormatterPosition;
  ABoxes: TdxBoxCollection;
  ABoxIndex: Integer;
  ABox: TdxVisitableDocumentIntervalBox;
  AComparable: TdxBoxAndFormatterPositionComparable;
begin
  Result := TdxVisitableDocumentIntervalBoxCollection.Create;
  while not BookmarkIterator.IsDone do
  begin
    ABoundary := BookmarkIterator.Current;
    APosition := GetPosition(ABoundary);
    if APosition > ARow.GetLastFormatterPosition then
      Break;

    if APosition < ARow.GetFirstFormatterPosition then
    begin
      BookmarkIterator.MoveNext;
      Continue;
    end;

    ABoxes := ARow.Boxes;
    AComparable := TdxBoxAndFormatterPositionComparable.Create(APosition);
    try
      if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(ABoxes, AComparable, ABoxIndex) then
      begin
        if ABoxIndex >= ABoxes.Count then
          Break;
      end;
    finally
      AComparable.Free;
    end;
    ABox := ABoundary.CreateBox;
    ABox.Interval := BookmarkIterator.Current.VisitableInterval;
    ABox.HorizontalPosition := GetHorizontalPosition(APosition, ABoxes[ABoxIndex]);
    ABox.Color := GetBoxColor;
    Result.Add(ABox);
    BookmarkIterator.MoveNext;
  end;
end;

function TdxBookmarkBoxCalculator.GetHorizontalPosition(const APosition: TdxFormatterPosition; ABox: TdxBox): Integer;
begin
  if ABox.StartPos.RunIndex <> APosition.RunIndex then
    Exit(ABox.Bounds.Left);
  if APosition = ABox.StartPos then
    Exit(ABox.Bounds.Left)
  else
    if GetPrevPosition(APosition) = ABox.EndPos then
      Exit(ABox.Bounds.Right);
  Result := ABox.Bounds.Left + MeasureBoxPart(APosition, ABox);
end;

function TdxBookmarkBoxCalculator.GetPosition(ABoundary: TdxVisitableDocumentIntervalBoundary): TdxFormatterPosition;
begin
  Result := TdxFormatterPosition.Create(ABoundary.Position.RunIndex, ABoundary.Position.RunOffset, 0);
end;

function TdxBookmarkBoxCalculator.GetPrevPosition(const APos: TdxFormatterPosition): TdxFormatterPosition;
var
  ARunIndex: TdxRunIndex;
  AOffset: Integer;
begin
  AOffset := APos.Offset - 1;
  if AOffset < 0 then
  begin
    ARunIndex := APos.RunIndex - 1;
    if ARunIndex < 0 then
      Exit(APos);

    AOffset := PieceTable.Runs[ARunIndex].Length - 1;
  end
  else
    ARunIndex := APos.RunIndex;

  Result := TdxFormatterPosition.Create(ARunIndex, AOffset, 0);
end;

function TdxBookmarkBoxCalculator.MeasureBoxPart(const APosition: TdxFormatterPosition; ABox: TdxBox): Integer;
var
  ABoxInfo: TdxBoxInfo;
  ABoxStartPos, APrevPosition: TdxFormatterPosition;
  AOldPieceTable: TdxCustomPieceTable;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxStartPos := ABox.StartPos;
    ABoxInfo.StartPos := TdxFormatterPosition.Create(ABoxStartPos.RunIndex, ABoxStartPos.Offset, -1);
    APrevPosition := GetPrevPosition(APosition);
    ABoxInfo.EndPos := TdxFormatterPosition.Create(APrevPosition.RunIndex, APrevPosition.Offset, -1);
    AOldPieceTable := Measurer.PieceTable;
    try
      Measurer.PieceTable := PieceTable;
      Measurer.MeasureText(ABoxInfo);
    finally
      Measurer.PieceTable := AOldPieceTable;
    end;
    Result := ABoxInfo.Size.Width;
  finally
    ABoxInfo.Free;
  end;
end;

procedure TdxBookmarkBoxCalculator.ResetFrom(const APosition: TdxDocumentModelPosition);
begin
  Reset;
end;

procedure TdxBookmarkBoxCalculator.Reset;
begin
  BookmarkIterator := nil;
  FBookmarkIteratorCache.Clear;
end;

function TdxBookmarkBoxCalculator.CreateBookmarkBoundaryIterator: TdxVisitableDocumentIntervalBoundaryIterator;
begin
  Result := TdxVisitableDocumentIntervalBoundaryIterator.Create(PieceTable, ExportToPdf);
end;

{ TdxRangePermissionBoxCalculator }

function TdxRangePermissionBoxCalculator.CreateBookmarkBoundaryIterator: TdxVisitableDocumentIntervalBoundaryIterator;
begin
  Result := TdxRangePermissionBoundaryIterator.Create(PieceTable);
end;

function TdxRangePermissionBoxCalculator.ShouldCalculate: Boolean;
begin
  Result := TdxDocumentModel(DocumentModel).RangePermissionOptions.Visibility <> TdxRichEditRangePermissionVisibility.Hidden;
end;

function TdxRangePermissionBoxCalculator.GetBoxColor: TdxAlphaColor;
begin
  if TdxDocumentModel(DocumentModel).ProtectionProperties.EnforceProtection then
    Exit(TdxDocumentModel(DocumentModel).RangePermissionOptions.HighlightBracketsColor);
  Result := TdxDocumentModel(DocumentModel).RangePermissionOptions.BracketsColor;
end;

function TdxRangePermissionBoxCalculator.GetBookmarkBoxCollection(ARow: TdxSimpleRow): TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := ARow.GetRangePermissionBoxesCore;
end;

procedure TdxRangePermissionBoxCalculator.ClearBookmarkBoxCollection(ARow: TdxSimpleRow);
begin
  ARow.ClearRangePermissionBoxes;
end;

end.
