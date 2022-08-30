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

unit dxRichEdit.DocumentModel.CopyManager;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Contnrs, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.TableCellsManager,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxDocumentModelCopyManager = class;

  { TdxTableCopyHelper }

  TdxTableCopyHelper = class
  public type
    TdxTableCopyInfo = record
      LastSourceTable: TdxTable;
      LastTargetTable: TdxTable;
      LastSourceRow: TdxTableRow;
      LastSourceCell: TdxTableCell;
      class function Create: TdxTableCopyInfo; static;
    end;
  strict private
    FTableCopyStack: TStack<TdxTableCopyInfo>;
    FOwner: TdxDocumentModelCopyManager;
    FCopiedProperties: TDictionary<TdxTableCellProperties, TdxTableCellProperties>;
    FTargetStartParagraphIndex: TdxParagraphIndex;
    FSuppressCopyTables: Boolean;
    FCopyFromNestedLevel: Integer;
    function GetTableCopyState: TdxTableCopyInfo;
    function GetLastSourceTable: TdxTable;
    function GetLastSourceRow: TdxTableRow;
    function GetLastTargetTable: TdxTable;
    function GetLastSourceCell: TdxTableCell;
    function GetSourcePieceTable: TdxPieceTable;
    function GetTargetPieceTable: TdxPieceTable;
    function GetSourceModel: TdxDocumentModel;
    function GetTargetModel: TdxDocumentModel;
    procedure SetLastSourceTable(const Value: TdxTable);
    procedure SetLastSourceCell(const Value: TdxTableCell);
    procedure SetLastSourceRow(const Value: TdxTableRow);
    procedure SetLastTargetTable(const Value: TdxTable);
    procedure SetTableCopyState(const Value: TdxTableCopyInfo);
  protected
    procedure ProcessTableCore(ANode: TdxTableCellNode;
      ATargetParentCell: TdxTableCell; AParagraphIndexOffset: Integer; ARunInfo: TdxRunInfo); virtual;
    function IsOneCellCopying(ARoot: TdxTableCellNode): Boolean;
    function GetTargetCell(ARoot: TdxTableCellNode; AParagraphIndexOffset: Integer): TdxTableCell;
    procedure FinalizeNestedTableCreation;
    function CopyCellAllowed(ACell: TdxTableCell; AInfo: TdxRunInfo): Boolean; virtual;

    property TableCopyState: TdxTableCopyInfo read GetTableCopyState write SetTableCopyState;
  public
    constructor Create(AOwner: TdxDocumentModelCopyManager);
    destructor Destroy; override;
    function CreateTable(APieceTable: TdxPieceTable; ATargetParentCell: TdxTableCell;
      ASourceCell: TdxTableCell): TdxTable;
    procedure CreateRow(ATable: TdxTable; ASourceRowProperties: TdxTableRowProperties;
      ASourceTablePropertiesException: TdxTableProperties);
    function CreateCell(AStartCellParagraphIndex: TdxParagraphIndex;
      AEndParagraphIndex: TdxParagraphIndex; ASourceCell: TdxTableCell): TdxTableCell;
    function IsNewRow(ATableRow: TdxTableRow): Boolean;
    function IsNewTable(ATable: TdxTable): Boolean;
    procedure CopyTables(ASourceRunInfo: TdxRunInfo);

    property SuppressCopyTables: Boolean read FSuppressCopyTables write FSuppressCopyTables;
    property LastSourceTable: TdxTable read GetLastSourceTable write SetLastSourceTable;
    property LastSourceRow: TdxTableRow read GetLastSourceRow write SetLastSourceRow;
    property LastTargetTable: TdxTable read GetLastTargetTable write SetLastTargetTable;
    property LastSourceCell: TdxTableCell read GetLastSourceCell write SetLastSourceCell;
    property SourcePieceTable: TdxPieceTable read GetSourcePieceTable;
    property TargetPieceTable: TdxPieceTable read GetTargetPieceTable;
    property SourceModel: TdxDocumentModel read GetSourceModel;
    property TargetModel: TdxDocumentModel read GetTargetModel;
    property TargetStartParagraphIndex: TdxParagraphIndex read FTargetStartParagraphIndex write FTargetStartParagraphIndex;
    property CopyFromNestedLevel: Integer read FCopyFromNestedLevel write FCopyFromNestedLevel;
  end;

  { TdxDocumentModelCopyManager }

  TdxDocumentModelCopyManager = class(TdxSimpleDocumentModelCopyManager)
  strict private
    FTableCopyHelper: TdxTableCopyHelper;
    function GetSourceModel: TdxDocumentModel;
    function GetSourcePieceTable: TdxPieceTable;
    function GetTargetModel: TdxDocumentModel;
    function GetTargetPieceTable: TdxPieceTable;
  public
    constructor Create(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable;
      AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
      AFormattingCopyOptions: TdxFormattingCopyOptions = TdxFormattingCopyOptions.UseDestinationStyles); reintroduce; virtual;
    destructor Destroy; override;

    procedure OnTargetSectionInserted(ASourceSection, ATargetSection: TdxSection);

    property SourceModel: TdxDocumentModel read GetSourceModel;
    property SourcePieceTable: TdxPieceTable read GetSourcePieceTable;
    property TargetModel: TdxDocumentModel read GetTargetModel;
    property TargetPieceTable: TdxPieceTable read GetTargetPieceTable;
    property TableCopyHelper: TdxTableCopyHelper read FTableCopyHelper;
  end;

  { TdxCopySectionOperation }

  TdxCopySectionOperation = class(TdxSelectionBasedOperation)
  strict private
    FCopyManager: TdxDocumentModelCopyManager;
    FShouldCopyBookmarks: Boolean;
    FFixLastParagraph: Boolean;
    FIsMergingTableCell: Boolean;
    FSuppressParentFieldsUpdate: Boolean;
    FAllowCopyWholeFieldResult: Boolean;
    FSuppressFieldsUpdate: Boolean;
    FSuppressCopySectionProperties: Boolean;
    FSuppressJoinTables: Boolean;
    FRemoveLeadingPageBreak: Boolean;
    FTransactionItemCountBeforeExecute: Integer;
    FUpdateFieldOperationType: TdxUpdateFieldOperationType;
    FUnfinishedCopyOperations: TdxObjectList<TdxCopyFieldsOperation>;
    FHistoryDisabled: Boolean;
    FOldUndoValue: TdxDocumentCapability;
  private
    function GetAffectsMainPieceTable: Boolean;
    function GetSourceModel: TdxDocumentModel;
    function GetTargetModel: TdxDocumentModel;
    function GetSourcePieceTable: TdxPieceTable;
    function GetTargetPieceTable: TdxPieceTable;
    function GetTargetPosition: PdxDocumentModelPosition;
    function SourceAndTargetModelAreDifferent: Boolean;
    function IsEmptySection(ATargetSection: TdxSection): Boolean;
    procedure ProcessSectionHeadCore(AInfo: TdxRunInfo; AStartSectionIndex: TdxSectionIndex;
      ADocumentLastParagraphSelected: Boolean);
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;

    function ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean; override;
    function ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean; override;
    procedure ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    procedure ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph,
      ADocumentLastParagraphSelected: Boolean); override;
    function ProcessContentSameParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph: Boolean;
      ADocumentLastParagraphSelected: Boolean): Boolean; override;
    function ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;
    function ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean; override;
    function ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;

    function CreateBookmarkCopyOperation: TdxCopySectionOperation{TdxCopyBookmarksOperation}; virtual;
    function CreateSectionCopy(ASourceSection: TdxSection): TdxSection;
    function IsTargetSectionLast: Boolean;
    procedure ProcessSections(AStartSectionIndex: TdxSectionIndex; ASectionCount: Integer);
    procedure ProcessSectionsCore(AIndex: TdxSectionIndex);
    function ShouldCopySection(ASourceSection, ATargetSection: TdxSection): Boolean;
    function TryCopyLastSection(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean;

    procedure JoinTables(ARunInfo: TdxRunInfo);

    property AffectsMainPieceTable: Boolean read GetAffectsMainPieceTable;
  public
    constructor Create(ACopyManager: TdxDocumentModelCopyManager);
    destructor Destroy; override;

    procedure AfterBookmarkCopied(AOperation: TdxCopyFieldsOperation); overload;
    procedure AfterBookmarkCopied; overload;
    function ExecuteCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean; override;

    property CopyManager: TdxDocumentModelCopyManager read FCopyManager;
    property ShouldCopyBookmarks: Boolean read FShouldCopyBookmarks write FShouldCopyBookmarks;
    property FixLastParagraph: Boolean read FFixLastParagraph write FFixLastParagraph;
    property IsMergingTableCell: Boolean read FIsMergingTableCell write FIsMergingTableCell;
    property SuppressParentFieldsUpdate: Boolean read FSuppressParentFieldsUpdate write FSuppressParentFieldsUpdate;
    property AllowCopyWholeFieldResult: Boolean read FAllowCopyWholeFieldResult write FAllowCopyWholeFieldResult;
    property SuppressFieldsUpdate: Boolean read FSuppressFieldsUpdate write FSuppressFieldsUpdate;
    property SuppressJoinTables: Boolean read FSuppressJoinTables write FSuppressJoinTables;
    property UpdateFieldOperationType: TdxUpdateFieldOperationType read FUpdateFieldOperationType write FUpdateFieldOperationType;

    property SourceModel: TdxDocumentModel read GetSourceModel;
    property TargetModel: TdxDocumentModel read GetTargetModel;
    property SourcePieceTable: TdxPieceTable read GetSourcePieceTable;
    property TargetPieceTable: TdxPieceTable read GetTargetPieceTable;
    property TargetPosition: PdxDocumentModelPosition read GetTargetPosition;
    property RemoveLeadingPageBreak: Boolean read FRemoveLeadingPageBreak write FRemoveLeadingPageBreak;
    property SuppressCopySectionProperties: Boolean read FSuppressCopySectionProperties write FSuppressCopySectionProperties;
  end;

  { TdxDeleteParagraphOperation }

  TdxDeleteParagraphOperation = class(TdxSelectionBasedOperation)
  strict private
    FAllowedDeleteLastParagraphInTableCell: Boolean;
    FIsProcessContentCrossParentExecute: Boolean;
    FIsDeletedSomeSections: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function ParagraphContentIsNumberingRunOnly(AParagraph: TdxParagraph): Boolean;
  protected
    function ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean; override;
    function ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean; override;
    procedure ProcessAllParagraphRuns(AParagraphIndex: TdxParagraphIndex);
    procedure ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean); override;
    procedure ProcessContentCrossParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    function ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;
    function ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean; override;
    procedure ProcessNumberingRun(AParagraph: TdxParagraph; AUseFirstParagraphStyle: Boolean);
    procedure ProcessParagraphsCore(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer);
    procedure ProcessParagraphs(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer);
    procedure ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    function ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;

    procedure DeleteRuns(AStartIndex: TdxRunIndex; ARunCount: Integer);
    function DeleteRunsInsideParagraphCore(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph: Boolean): Boolean;
    function IsDeletedLastParagraphInCell(AInfo: TdxRunInfo): Boolean;
    function IsMergeParagraphWithTable(AInfo: TdxRunInfo): Boolean;
    function IsNeedJoinTables(ATopParagraphIndex, ABottomParagraphIndex: TdxParagraphIndex): Boolean;
    function IsTheOnlyParagraph(AParagraphIndex: TdxParagraphIndex): Boolean;
    procedure JoinTables(AParagraphIndex: TdxParagraphIndex);
    procedure MergeParagraphWithNext(AParagraph: TdxParagraph);
    procedure ProcessContentCrossParentCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);

    function ProcessMiddleTable(AInfo: TdxRunInfo; AParagraphCount: Integer): Boolean;
    procedure ProcessTable(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); virtual;
    procedure ProcessContentCrossParentTable(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); virtual;
    procedure ProcessParagraphsInTable(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer); virtual;

    property IsProcessContentCrossParentExecute: Boolean read FIsProcessContentCrossParentExecute;
    property IsDeletedSomeSections: Boolean read FIsDeletedSomeSections write FIsDeletedSomeSections;
  public
    property AllowedDeleteLastParagraphInTableCell: Boolean read FAllowedDeleteLastParagraphInTableCell write FAllowedDeleteLastParagraphInTableCell;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxDeleteSectionOperation }

  TdxDeleteSectionOperation = class(TdxSelectionBasedOperation)
  strict private
    FIsDeletedSomeSections: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  private
    function AffectsMainPieceTable: Boolean;
    procedure DeleteTail(AInfo: TdxRunInfo; AEndSection: TdxSection; ADocumentLastParagraphSelected: Boolean);
  protected
    function ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean; override;
    function ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean; override;
    procedure ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean); override;
    procedure ProcessContentCrossParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    function ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;
    procedure ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    function ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean; override;
    function ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;
    procedure ProcessSections(AStartSectionIndex: TdxSectionIndex; ACount: Integer); virtual;
    procedure DeleteAllParagraphsInsideSection(AStartSectionIndex, AEndSectionIndex: TdxSectionIndex); virtual;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxDeleteContentWithFieldsOperation }

  TdxDeleteContentWithFieldsOperation = class(TdxDeleteContentOperationBase)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  protected
    procedure ExecuteCore(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    procedure DeleteFieldsWithinInterval(ARunInfo: TdxRunInfo); virtual;
    function CalculateDeletedFields(ARunInfo: TdxRunInfo): TdxFieldList; virtual;
    function CreateDeleteContentOperation: TdxSelectionBasedOperation; override;
    function IsFieldWithinInterval(ARunInfo: TdxRunInfo; AField: TdxField): Boolean; virtual;
    function IsParentShouldBeRemoved(AField: TdxField; ARunInfo: TdxRunInfo): Boolean; virtual;
    function IsFieldHidByParent(AField: TdxField): Boolean; virtual;
    procedure DeleteContentWithFieldsCore(const AStart, AEnd: TdxDocumentModelPosition; ADocumentLastParagraphSelected: Boolean); virtual;
    function ShouldSkipRun(ARunIndex: TdxRunIndex): Boolean; virtual;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxDeleteContentOperation }

  TdxCanDeleteBookmarkDelegate = function(AStart, AEnd: TdxDocumentLogPosition; ABookmark: TdxBookmarkBase): Boolean of object;

  TdxDeleteContentOperation = class(TdxSimpleDeleteContentOperation)
  strict private
    FSuppressFieldDelete: Boolean;
    procedure ExtendDeletedInterval(ARunInfo: TdxRunInfo; AField: TdxField);
    function GetPieceTable: TdxPieceTable;
    function ShouldDeleteField(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
  protected
    procedure BeforeExecute(ARunInfo: TdxRunInfo); override;
    procedure DeleteBookmarks(ARunInfo: TdxRunInfo); virtual;
    procedure DeleteBookmarksCore(ABookmarks: TObjectList; ARunInfo: TdxRunInfo;
      ACanDeleteBookmark: TdxCanDeleteBookmarkDelegate);
    function ShouldDeleteBookmark(AStart, AEnd: TdxDocumentLogPosition; ABookmark: TdxBookmarkBase): Boolean; virtual;
    function CanDeleteRangePermission(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition; ABookmark: TdxBookmarkBase): Boolean; virtual;
    function CreateDeleteContentOperation: TdxSelectionBasedOperation; override;
    function DeleteFields(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean; virtual;
    procedure ExecuteCore(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
  public
    property SuppressFieldDelete: Boolean read FSuppressFieldDelete write FSuppressFieldDelete;

    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

implementation

uses
  Classes,
  dxCoreClasses,
  dxRichEdit.DocumentModel.CopyParagraphOperation,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.ParagraphRange;

type
  { TdxDeleteTablesOptions }

  TdxDeleteTablesOptions = class
  strict private
    FDocumentLastParagraphSelected: Boolean;
    FIsDeletedSomeSections: Boolean;
    FBackspacePressed: Boolean;
  public
    constructor Create(ADocumentLastParagraphSelected: Boolean; AIsDeletedSomeSections: Boolean; ABackspacePressed: Boolean);

    property DocumentLastParagraphSelected: Boolean read FDocumentLastParagraphSelected write FDocumentLastParagraphSelected;
    property IsDeletedSomeSections: Boolean read FIsDeletedSomeSections write FIsDeletedSomeSections;
    property BackspacePressed: Boolean read FBackspacePressed write FBackspacePressed;
  end;

  { TdxDeleteTablesHelper }

  TdxDeleteTablesHelper = class
  public
    class function IsDeleteParagraphsInTable(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean; AIsDeletedSomeSections: Boolean): Boolean; static;
    class function IsSelectedEntireTableRows(AInfo: TdxRunInfo; AOptions: TdxDeleteTablesOptions): Boolean; static;
    class procedure GetSelectedRowsCore(ACurrent: TdxTableCellNode; AInfo: TdxRunInfo; AResult: TdxTableRowList); static;
    class function IsSelectedEntireTableRow(ARow: TdxTableRow; AInfo: TdxRunInfo): Boolean; static;
    class function GetSelectedRows(AInfo: TdxRunInfo): TdxTableRowList; static;
    class procedure DeleteSelectedTableRows(AInfo: TdxRunInfo; AOptions: TdxDeleteTablesOptions); overload; static;
    class procedure DeleteSelectedTableRows(ASelectedRows: TdxTableRowList); overload; static;
  end;

{ TdxDeleteTablesOptions }

constructor TdxDeleteTablesOptions.Create(ADocumentLastParagraphSelected: Boolean; AIsDeletedSomeSections: Boolean; ABackspacePressed: Boolean);
begin
  inherited Create;
  DocumentLastParagraphSelected := ADocumentLastParagraphSelected;
  IsDeletedSomeSections := AIsDeletedSomeSections;
  BackspacePressed := ABackspacePressed;
end;

{ TdxDeleteTablesHelper }

class function TdxDeleteTablesHelper.IsDeleteParagraphsInTable(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean; AIsDeletedSomeSections: Boolean): Boolean;
var
  APieceTable: TdxPieceTable;
  ARoot: TdxTableCellNode;
  AStart, AEnd: TdxParagraphIndex;
  AStartCell, AEndCell: TdxTableCell;
begin
  if AIsDeletedSomeSections then
    Exit(False);
  APieceTable := TdxPieceTable(AInfo.&End.PieceTable);
  ARoot := APieceTable.TableCellsManager.GetCellSubTree(AInfo.Start.ParagraphIndex, AInfo.&End.ParagraphIndex, 0);
  try
    if (ARoot <> nil) and (ARoot.ChildNodes.Count > 0) then
      Exit(True);
  finally
    ARoot.Free;
  end;
  AStart := AInfo.Start.ParagraphIndex;
  AEnd := AInfo.&End.ParagraphIndex;
  AStartCell := APieceTable.Paragraphs[AStart].GetCell;
  AEndCell := APieceTable.Paragraphs[AEnd].GetCell;
  Result := (AStartCell <> nil) or (AEndCell <> nil);
end;

class function TdxDeleteTablesHelper.IsSelectedEntireTableRows(AInfo: TdxRunInfo; AOptions: TdxDeleteTablesOptions): Boolean;
var
  APieceTable: TdxPieceTable;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  AStartCell, AEndCell: TdxTableCell;
  AInfoEndRunIndex, AInfoStartRunIndex: TdxRunIndex;
  ASelectWhollyFirstCell, ASelectWhollyLastCell, ADifferentTable: Boolean;
begin
  APieceTable := TdxPieceTable(AInfo.&End.PieceTable);
  AStartParagraphIndex := AInfo.NormalizedStart.ParagraphIndex;
  AEndParagraphIndex := AInfo.NormalizedEnd.ParagraphIndex;
  AParagraphs := APieceTable.Paragraphs;
  AStartCell := AParagraphs[AStartParagraphIndex].GetCell;
  AEndCell := AParagraphs[AEndParagraphIndex].GetCell;
  if (AStartCell = nil) and (AEndCell = nil) then
    Exit(False);
  if ((((AStartCell <> nil) and (AEndCell <> nil)) and (AStartCell.Table = AEndCell.Table)) and not AOptions.DocumentLastParagraphSelected) and not AOptions.IsDeletedSomeSections then
    Exit(False);
  AInfoEndRunIndex := AInfo.NormalizedEnd.RunIndex;
  AInfoStartRunIndex := AInfo.NormalizedStart.RunIndex;
  if AStartCell <> nil then
    ASelectWhollyFirstCell := AStartCell.IsFirstCellInRow and (AParagraphs[AStartCell.StartParagraphIndex].FirstRunIndex = AInfoStartRunIndex)
  else
    ASelectWhollyFirstCell := False;
  if AEndCell <> nil then
    ASelectWhollyLastCell := AEndCell.IsLastCellInRow and (AParagraphs[AEndCell.EndParagraphIndex].LastRunIndex = AInfoEndRunIndex)
  else
    ASelectWhollyLastCell := False;
  ADifferentTable := ((AStartCell <> nil) and (AEndCell <> nil)) and (AStartCell.Table <> AEndCell.Table);
  if ((AStartCell = nil) or (ADifferentTable)) and ASelectWhollyLastCell then
    Exit(True);
  if ((AEndCell = nil) or (ADifferentTable)) and ASelectWhollyFirstCell then
    Exit(True);
  if (((AStartCell <> nil) and (AEndCell <> nil)) and ASelectWhollyFirstCell) and ASelectWhollyLastCell then
    Exit(True);
  Result := False;
end;

class procedure TdxDeleteTablesHelper.GetSelectedRowsCore(ACurrent: TdxTableCellNode;
  AInfo: TdxRunInfo; AResult: TdxTableRowList);
var
  I: Integer;
  ACurrentNode: TdxTableCellNode;
  ARow: TdxTableRow;
begin
  if ACurrent = nil then
    Exit;
  for I := ACurrent.ChildNodes.Count - 1 downto 0 do
  begin
    ACurrentNode := ACurrent.ChildNodes[I];
    if ACurrentNode.ChildNodes <> nil then
      GetSelectedRowsCore(ACurrentNode, AInfo, AResult);
    ARow := ACurrentNode.Cell.Row;
    if not AResult.Contains(ARow) and IsSelectedEntireTableRow(ARow, AInfo) then
      AResult.Add(ARow);
  end;
end;

class function TdxDeleteTablesHelper.IsSelectedEntireTableRow(ARow: TdxTableRow; AInfo: TdxRunInfo): Boolean;
var
  ARowStartParagraphIndex, ARowEndParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  ARowStartRunIndex, ARowEndRunIndex: TdxRunIndex;
begin
  ARowStartParagraphIndex := ARow.FirstCell.StartParagraphIndex;
  ARowEndParagraphIndex := ARow.LastCell.EndParagraphIndex;
  AParagraphs := TdxParagraphCollection(ARow.PieceTable.Paragraphs);
  ARowStartRunIndex := AParagraphs[ARowStartParagraphIndex].FirstRunIndex;
  ARowEndRunIndex := AParagraphs[ARowEndParagraphIndex].LastRunIndex;
  if (ARowStartRunIndex >= AInfo.NormalizedStart.RunIndex) and (ARowEndRunIndex <= AInfo.NormalizedEnd.RunIndex) then
    Exit(True);
  Result := False;
end;

class function TdxDeleteTablesHelper.GetSelectedRows(AInfo: TdxRunInfo): TdxTableRowList;
var
  ARoot: TdxTableCellNode;
begin
  Result := TdxTableRowList.Create;
  ARoot := TdxPieceTable(AInfo.&End.PieceTable).TableCellsManager.GetCellSubTree(AInfo.NormalizedStart.ParagraphIndex, AInfo.NormalizedEnd.ParagraphIndex, -1);
  try
    GetSelectedRowsCore(ARoot, AInfo, Result);
  finally
    ARoot.Free;
  end;
end;

class procedure TdxDeleteTablesHelper.DeleteSelectedTableRows(AInfo: TdxRunInfo; AOptions: TdxDeleteTablesOptions);
var
  ASelectedRows: TdxTableRowList;
  ATable: TdxTable;
begin
  if not IsSelectedEntireTableRows(AInfo, AOptions) and not AOptions.BackspacePressed then
    Exit;
  ASelectedRows := GetSelectedRows(AInfo);
  try
    if ASelectedRows.Count > 0 then
    begin
      ATable := ASelectedRows[0].Table;
      DeleteSelectedTableRows(ASelectedRows);
      ATable.NormalizeCellColumnSpans;
    end;
  finally
    ASelectedRows.Free;
  end;
end;

class procedure TdxDeleteTablesHelper.DeleteSelectedTableRows(ASelectedRows: TdxTableRowList);
var
  APieceTable: TdxPieceTable;
  I: Integer;
  ACurrentRow: TdxTableRow;
begin
  APieceTable := TdxPieceTable(ASelectedRows[0].PieceTable);
  for I := ASelectedRows.Count - 1 downto 0 do
  begin
    ACurrentRow := ASelectedRows[I];
    APieceTable.DeleteEmptyTableRowCore(ACurrentRow.Table.Index, ACurrentRow.IndexInTable);
  end;
end;

{ TdxTableCopyHelper }

constructor TdxTableCopyHelper.Create(AOwner: TdxDocumentModelCopyManager);
begin
  inherited Create;
  FCopyFromNestedLevel := -1;
  FOwner := AOwner;
  FTargetStartParagraphIndex := AOwner.TargetPosition.ParagraphIndex;
  FTableCopyStack := TStack<TdxTableCopyInfo>.Create;
  FTableCopyStack.Push(TdxTableCopyInfo.Create);
  FCopiedProperties := TDictionary<TdxTableCellProperties, TdxTableCellProperties>.Create;
end;

destructor TdxTableCopyHelper.Destroy;
begin
  FreeAndNil(FCopiedProperties);
  FreeAndNil(FTableCopyStack);
  inherited Destroy;
end;

function TdxTableCopyHelper.GetTableCopyState: TdxTableCopyInfo;
begin
  Result := FTableCopyStack.Peek;
end;

function TdxTableCopyHelper.GetLastSourceTable: TdxTable;
begin
  Result := TableCopyState.LastSourceTable;
end;

function TdxTableCopyHelper.GetLastSourceRow: TdxTableRow;
begin
  Result := TableCopyState.LastSourceRow;
end;

function TdxTableCopyHelper.GetLastTargetTable: TdxTable;
begin
  Result := TableCopyState.LastTargetTable;
end;

function TdxTableCopyHelper.GetLastSourceCell: TdxTableCell;
begin
  Result := TableCopyState.LastSourceCell;
end;

function TdxTableCopyHelper.GetSourcePieceTable: TdxPieceTable;
begin
  Result := FOwner.SourcePieceTable;
end;

function TdxTableCopyHelper.GetTargetPieceTable: TdxPieceTable;
begin
  Result := FOwner.TargetPieceTable;
end;

function TdxTableCopyHelper.GetSourceModel: TdxDocumentModel;
begin
  Result := FOwner.SourcePieceTable.DocumentModel;
end;

function TdxTableCopyHelper.GetTargetModel: TdxDocumentModel;
begin
  Result := FOwner.TargetPieceTable.DocumentModel;
end;

function TdxTableCopyHelper.CreateTable(APieceTable: TdxPieceTable; ATargetParentCell: TdxTableCell; ASourceCell: TdxTableCell): TdxTable;
begin
  Result := APieceTable.CreateTableCore(ATargetParentCell);
  LastSourceTable := ASourceCell.Table;
  Result.CopyProperties(ASourceCell.Table);
end;

procedure TdxTableCopyHelper.CreateRow(ATable: TdxTable; ASourceRowProperties: TdxTableRowProperties; ASourceTablePropertiesException: TdxTableProperties);
var
  ALastTargetRow: TdxTableRow;
begin
  ALastTargetRow := TdxPieceTable(ATable.PieceTable).CreateTableRowCore(ATable);
  ALastTargetRow.Properties.CopyFrom(ASourceRowProperties);
  ALastTargetRow.TablePropertiesException.CopyFrom(ASourceTablePropertiesException);
end;

function TdxTableCopyHelper.CreateCell(AStartCellParagraphIndex: TdxParagraphIndex; AEndParagraphIndex: TdxParagraphIndex; ASourceCell: TdxTableCell): TdxTableCell;
begin
  Result := TargetPieceTable.CreateTableCellCore(LastTargetTable.LastRow, AStartCellParagraphIndex, AEndParagraphIndex);
  Result.CopyProperties(ASourceCell);
end;

function TdxTableCopyHelper.IsNewRow(ATableRow: TdxTableRow): Boolean;
begin
  Result := ATableRow <> LastSourceRow;
end;

function TdxTableCopyHelper.IsNewTable(ATable: TdxTable): Boolean;
begin
  Result := ATable <> LastSourceTable;
end;

procedure TdxTableCopyHelper.CopyTables(ASourceRunInfo: TdxRunInfo);
var
  AStart, AEnd: TdxParagraphIndex;
  AParagraphIndexOffset: Integer;
  ARoot: TdxTableCellNode;
  ATargetCell: TdxTableCell;
begin
  if not TargetModel.DocumentCapabilities.TablesAllowed or not TargetModel.DocumentCapabilities.ParagraphsAllowed then
    Exit;
  AStart := ASourceRunInfo.NormalizedStart.ParagraphIndex;
  AEnd := ASourceRunInfo.NormalizedEnd.ParagraphIndex;
  AParagraphIndexOffset := ASourceRunInfo.NormalizedStart.ParagraphIndex - TargetStartParagraphIndex;
  ARoot := SourcePieceTable.TableCellsManager.GetCellSubTree(AStart, AEnd, CopyFromNestedLevel);
  try
    ATargetCell := GetTargetCell(ARoot, AParagraphIndexOffset);
    if ATargetCell <> nil then
      CopyFromNestedLevel := ATargetCell.Table.NestedLevel + 1;
    ProcessTableCore(ARoot, ATargetCell, AParagraphIndexOffset, ASourceRunInfo);
  finally
    ARoot.Free;
  end;
end;

procedure TdxTableCopyHelper.ProcessTableCore(ANode: TdxTableCellNode;
  ATargetParentCell: TdxTableCell; AParagraphIndexOffset: Integer; ARunInfo: TdxRunInfo);
var
  I: Integer;
  ASourceCell: TdxTableCell;
  ATargetEnd, ATargetStart: TdxParagraphIndex;
  ANewTargetCell: TdxTableCell;
begin
  if (ANode = nil) or (ANode.ChildNodes = nil) or (ANode.ChildNodes.Count = 0) then
    Exit;
  if ATargetParentCell <> nil then
    FTableCopyStack.Push(TdxTableCopyInfo.Create);
  try
    for I := 0 to ANode.ChildNodes.Count - 1 do
    begin
      if ANode.ChildNodes[I].Cell = nil then
      begin
        ProcessTableCore(ANode.ChildNodes[I], ATargetParentCell, AParagraphIndexOffset, ARunInfo);
        Continue;
      end;
      if not CopyCellAllowed(ANode.ChildNodes[I].Cell, ARunInfo) then
      begin
        ProcessTableCore(ANode.ChildNodes[I], ATargetParentCell, AParagraphIndexOffset, ARunInfo);
        Exit;
      end;
      if IsNewTable(ANode.ChildNodes[I].Cell.Table) then
        LastTargetTable := CreateTable(TargetPieceTable, ATargetParentCell, ANode.ChildNodes[I].Cell);
      ASourceCell := ANode.ChildNodes[I].Cell;
      if IsNewRow(ASourceCell.Row) then
      begin
        LastSourceRow := ASourceCell.Row;
        CreateRow(LastTargetTable, LastSourceRow.Properties, LastSourceRow.TablePropertiesException);
      end;
      ATargetStart := ASourceCell.StartParagraphIndex - AParagraphIndexOffset;
      ATargetEnd := ASourceCell.EndParagraphIndex - AParagraphIndexOffset;
      ANewTargetCell := CreateCell(ATargetStart, ATargetEnd, ASourceCell);
      ProcessTableCore(ANode.ChildNodes[I], ANewTargetCell, AParagraphIndexOffset, ARunInfo);
    end;
  finally
    if ATargetParentCell <> nil then
      FinalizeNestedTableCreation;
  end;
end;

procedure TdxTableCopyHelper.SetLastSourceCell(const Value: TdxTableCell);
var
  AState: TdxTableCopyInfo;
begin
  AState := TableCopyState;
  AState.LastSourceCell := Value;
  TableCopyState := AState;
end;

procedure TdxTableCopyHelper.SetLastSourceRow(const Value: TdxTableRow);
var
  AState: TdxTableCopyInfo;
begin
  AState := TableCopyState;
  AState.LastSourceRow := Value;
  TableCopyState := AState;
end;

procedure TdxTableCopyHelper.SetLastSourceTable(const Value: TdxTable);
var
  AState: TdxTableCopyInfo;
begin
  AState := TableCopyState;
  AState.LastSourceTable := Value;
  TableCopyState := AState;
end;

procedure TdxTableCopyHelper.SetLastTargetTable(const Value: TdxTable);
var
  AState: TdxTableCopyInfo;
begin
  AState := TableCopyState;
  AState.LastTargetTable := Value;
  TableCopyState := AState;
end;

procedure TdxTableCopyHelper.SetTableCopyState(const Value: TdxTableCopyInfo);
begin
  FTableCopyStack.Pop;
  FTableCopyStack.Push(Value);
end;

function TdxTableCopyHelper.IsOneCellCopying(ARoot: TdxTableCellNode): Boolean;
begin
  Result := (ARoot <> nil) and (ARoot.ChildNodes.Count = 1);
end;

function TdxTableCopyHelper.GetTargetCell(ARoot: TdxTableCellNode;
  AParagraphIndexOffset: Integer): TdxTableCell;
var
  ATargetStart: TdxParagraphIndex;
begin
  if (ARoot = nil) or (ARoot.ChildNodes = nil) or (ARoot.ChildNodes.First = nil) or (ARoot.ChildNodes.First.Cell = nil) then
    Exit(nil);
  ATargetStart := ARoot.ChildNodes.First.Cell.StartParagraphIndex - AParagraphIndexOffset;
  if ATargetStart < 0 then
    ATargetStart := 0;
  if ATargetStart >= TargetPieceTable.Paragraphs.Last.Index then
    Exit(nil);
  Result := TargetPieceTable.Paragraphs[ATargetStart].GetCell;
end;

procedure TdxTableCopyHelper.FinalizeNestedTableCreation;
begin
  FTableCopyStack.Pop;
end;

function TdxTableCopyHelper.CopyCellAllowed(ACell: TdxTableCell; AInfo: TdxRunInfo): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  AStartParagraphFirstRunIndex, AEndParagraphLastRunIndex: TdxRunIndex;
begin
  if AInfo = nil then
    Exit(True);
  AParagraphs := FOwner.SourcePieceTable.Paragraphs;
  AStartParagraphFirstRunIndex := AParagraphs[ACell.StartParagraphIndex].FirstRunIndex;
  AEndParagraphLastRunIndex := AParagraphs[ACell.EndParagraphIndex].LastRunIndex;
  Result := (AInfo.Start.RunIndex <= AStartParagraphFirstRunIndex) and (AInfo.&End.RunIndex >= AEndParagraphLastRunIndex);
end;

{ TdxTableCopyHelper.TdxTableCopyInfo }

class function TdxTableCopyHelper.TdxTableCopyInfo.Create: TdxTableCopyInfo;
begin
  Result.LastSourceTable := nil;
  Result.LastTargetTable := nil;
  Result.LastSourceRow := nil;
  Result.LastSourceCell := nil;
end;

{ TdxDocumentModelCopyManager }

constructor TdxDocumentModelCopyManager.Create(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable;
  AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
  AFormattingCopyOptions: TdxFormattingCopyOptions = TdxFormattingCopyOptions.UseDestinationStyles);
begin
  inherited Create(ASourcePieceTable, ATargetPieceTable, AParagraphNumerationCopyOptions,
    AFormattingCopyOptions);
  FTableCopyHelper := TdxTableCopyHelper.Create(Self);
end;

destructor TdxDocumentModelCopyManager.Destroy;
begin
  FreeAndNil(FTableCopyHelper);
  inherited Destroy;
end;

procedure TdxDocumentModelCopyManager.OnTargetSectionInserted(ASourceSection, ATargetSection: TdxSection);
var
  ATargetRun, ASourceRun: TdxParagraphRun;
  ATargetSectionLastRunIndex, ASourceSectionLastRunIndex: TdxRunIndex;
begin
  ATargetSectionLastRunIndex := TargetPieceTable.Paragraphs[ATargetSection.LastParagraphIndex].LastRunIndex;
  ASourceSectionLastRunIndex := SourcePieceTable.Paragraphs[ASourceSection.LastParagraphIndex].LastRunIndex;
  FTargetPosition.ParagraphIndex := FTargetPosition.ParagraphIndex + 1;
  ATargetRun := TdxParagraphRun(TargetPieceTable.Runs[ATargetSectionLastRunIndex]);
  ASourceRun := TdxParagraphRun(SourcePieceTable.Runs[ASourceSectionLastRunIndex]);
  OnTargetRunInserted(ASourceRun, ATargetRun);
end;

function TdxDocumentModelCopyManager.GetSourceModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited SourceModel);
end;

function TdxDocumentModelCopyManager.GetSourcePieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited SourcePieceTable);
end;

function TdxDocumentModelCopyManager.GetTargetModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited TargetModel);
end;

function TdxDocumentModelCopyManager.GetTargetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited TargetPieceTable);
end;

{ TdxCopySectionOperation }

constructor TdxCopySectionOperation.Create(ACopyManager: TdxDocumentModelCopyManager);
begin
  inherited Create(ACopyManager.SourcePieceTable);
  FCopyManager := ACopyManager;
  FShouldCopyBookmarks := True;
  FUpdateFieldOperationType := TdxUpdateFieldOperationType.Copy;
end;

function TdxCopySectionOperation.CreateBookmarkCopyOperation: TdxCopySectionOperation{TdxCopyBookmarksOperation};
begin
  Result := TdxCopyBookmarksOperation.Create(CopyManager);
end;

destructor TdxCopySectionOperation.Destroy;
begin
  FreeAndNil(FUnfinishedCopyOperations);
  inherited Destroy;
end;

procedure TdxCopySectionOperation.AfterBookmarkCopied(AOperation: TdxCopyFieldsOperation);
begin
  AOperation.UpdateCopiedFields;
end;

procedure TdxCopySectionOperation.AfterBookmarkCopied;
var
  I: Integer;
begin
  if FUnfinishedCopyOperations = nil then
    Exit;
  for I := 0 to FUnfinishedCopyOperations.Count - 1 do
    FUnfinishedCopyOperations[I].UpdateCopiedFields;
  FreeAndNil(FUnfinishedCopyOperations);
end;

procedure TdxCopySectionOperation.AfterExecute;
var
  ATransaction: TdxCompositeHistoryItem;
  I: Integer;
  AItem: TdxHistoryItem;
begin
  ATransaction := SourceModel.History.Transaction;
  for I := ATransaction.Items.Count - 1 downto FTransactionItemCountBeforeExecute do
  begin
    AItem := ATransaction.Items[I];
    if AItem.PieceTable = SourcePieceTable then
    begin
      AItem.Undo;
      ATransaction.Items.Delete(I);
    end;
  end;
  if FHistoryDisabled then
  begin
    SourceModel.History.EndTransaction;
    SourceModel.DocumentCapabilities.Undo := FOldUndoValue;
  end;
  DocumentModel.EndSuppressPerformLayout;
end;

procedure TdxCopySectionOperation.BeforeExecute;
var
  ATransaction: TdxCompositeHistoryItem;
begin
  DocumentModel.BeginSuppressPerformLayout;
  FHistoryDisabled := SourceModel.History is TdxDisabledHistory;
  if FHistoryDisabled then
  begin
    FOldUndoValue := SourceModel.DocumentCapabilities.Undo;
    SourceModel.DocumentCapabilities.Undo := TdxDocumentCapability.Enabled;
    SourceModel.History.BeginTransaction;
  end;
  ATransaction := SourceModel.History.Transaction;
  FTransactionItemCountBeforeExecute := ATransaction.Items.Count;
end;

function TdxCopySectionOperation.ExecuteCore(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ABookmarksCopier: TdxCopyBookmarksOperation;
  AEventRouter: TdxCalculateDocumentVariableEventRouter;
  AOperation: TdxCopyFieldsOperation;
  APositionToInsert: TdxDocumentModelPosition;
begin
  SourceModel.BeginUpdate;
  try
    AEventRouter := TdxCalculateDocumentVariableEventRouter.Create(SourceModel);
    try
      TargetModel.BeginUpdate;
      try
        if SourceAndTargetModelAreDifferent and not SourceModel.IntermediateModel then
          TargetModel.CalculateDocumentVariable.Add(AEventRouter.OnCalculateDocumentVariable);
        AOperation := TdxCopyFieldsOperation.Create(SourcePieceTable, TargetPieceTable);
        try
          AOperation.AllowCopyWholeFieldResult := AllowCopyWholeFieldResult;
          AOperation.SuppressFieldsUpdate := SuppressFieldsUpdate;
          AOperation.UpdateFieldOperationType := UpdateFieldOperationType;
          AOperation.RecalculateRunInfo(AInfo);
          APositionToInsert := CopyManager.TargetPosition;
          Result := inherited ExecuteCore(AInfo, ADocumentLastParagraphSelected);
          if CopyManager.ParagraphWasInsertedBeforeTable or IsMergingTableCell then
          begin
            APositionToInsert.LogPosition := APositionToInsert.LogPosition + 1;
            APositionToInsert.RunIndex := APositionToInsert.RunIndex + 1;
          end;
          AOperation.Execute(AInfo, APositionToInsert.RunIndex);
        finally
          if ShouldCopyBookmarks then
          begin
            ABookmarksCopier := TdxCopyBookmarksOperation(CreateBookmarkCopyOperation);
            try
              ABookmarksCopier.CopyBookmarksToTargetModel(AInfo, APositionToInsert);
              AfterBookmarkCopied(AOperation);
            finally
              ABookmarksCopier.Free;
            end;
            AOperation.Free;
          end
          else
          begin
            if FUnfinishedCopyOperations = nil then
              FUnfinishedCopyOperations := TdxObjectList<TdxCopyFieldsOperation>.Create;
            FUnfinishedCopyOperations.Add(AOperation);
          end;
        end;
        if FixLastParagraph then
          TargetPieceTable.FixLastParagraph;
        if RemoveLeadingPageBreak then
          TargetPieceTable.Paragraphs.First.PageBreakBefore := False;
      finally
        TargetModel.EndUpdate;
        if SourceAndTargetModelAreDifferent and not SourceModel.IntermediateModel then
          TargetModel.CalculateDocumentVariable.Remove(AEventRouter.OnCalculateDocumentVariable);
      end;
    finally
      AEventRouter.Free;
    end;
  finally
    SourceModel.EndUpdate;
  end;
end;

function TdxCopySectionOperation.ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean;
var
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AStartParargraph, AEndParagraph: TdxParagraph;
begin
  if not AffectsMainPieceTable then
    Result := True
  else
  begin
    AStartSectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
    AEndSectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
    ASection := SourceModel.Sections[AStartSectionIndex];
    AStartParargraph := SourcePieceTable.Paragraphs[ASection.FirstParagraphIndex];
    AEndParagraph := SourcePieceTable.Paragraphs[ASection.LastParagraphIndex];
    Result := (AStartSectionIndex = AEndSectionIndex) and
      ((AInfo.Start.RunIndex <> AStartParargraph.FirstRunIndex) or
      (AInfo.&End.RunIndex <> AEndParagraph.LastRunIndex));
  end;
end;

function TdxCopySectionOperation.ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean;
var
  AEnd: TdxDocumentModelPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AEndRun: TdxTextRunBase;
  ALastSectionRun: TdxRunIndex;
begin
  if not AffectsMainPieceTable then
    Result := False
  else
  begin
    AEnd := AInfo.&End;
    ASectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
    ASection := SourceModel.Sections[ASectionIndex];
    AEndRun := SourcePieceTable.Runs[AEnd.RunIndex];
    ALastSectionRun := SourcePieceTable.Paragraphs[ASection.LastParagraphIndex].LastRunIndex;
    Result := (AEnd.RunIndex = ALastSectionRun) and (AEndRun is TdxSectionRun);
  end;
end;

procedure TdxCopySectionOperation.ProcessRunParent(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
var
  ASectionIndex: TdxSectionIndex;
  ASourceSection, ATargetSection: TdxSection;
  ASelectionContentInfo: TdxRunInfo;
begin
  if AffectsMainPieceTable then
  begin
    ASectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
    ASourceSection := SourceModel.Sections[ASectionIndex];
    ATargetSection := CreateSectionCopy(ASourceSection);
    ASelectionContentInfo := SourcePieceTable.ObtainAffectedRunInfo(AInfo.Start.LogPosition, AInfo.&End.LogPosition - AInfo.Start.LogPosition);
    try
      ProcessContentInsideParent(ASelectionContentInfo, True, ADocumentLastParagraphSelected);
    finally
      ASelectionContentInfo.Free;
    end;
    FCopyManager.OnTargetSectionInserted(ASourceSection, ATargetSection);
  end
  else
    ProcessContentInsideParent(AInfo, True, ADocumentLastParagraphSelected);
end;

procedure TdxCopySectionOperation.ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph,
  ADocumentLastParagraphSelected: Boolean);
var
  AParagraphOperation: TdxCopyParagraphOperation;
begin
  CopyManager.TableCopyHelper.TargetStartParagraphIndex := CopyManager.TargetPosition.ParagraphIndex;

  AParagraphOperation := TdxCopyParagraphOperation.Create(CopyManager);
  try
    AParagraphOperation.IsMergingTableCell := IsMergingTableCell;
    AParagraphOperation.ExecuteCore(AInfo, False);
  finally
    AParagraphOperation.Free;
  end;
  CopyManager.TableCopyHelper.CopyTables(AInfo);
  if not SuppressJoinTables then
    JoinTables(AInfo);
end;

function TdxCopySectionOperation.ProcessContentSameParent(AInfo: TdxRunInfo;
  AAllowMergeWithNextParagraph,
  ADocumentLastParagraphSelected: Boolean): Boolean;
begin
  if not SourceModel.FieldResultModel and TryCopyLastSection(AInfo, ADocumentLastParagraphSelected) then
  begin
    ProcessContentInsideParent(AInfo, True, False);
    Result := True
  end
  else
    Result := inherited ProcessContentSameParent(AInfo, AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected);
end;

function TdxCopySectionOperation.ProcessHead(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Integer;
var
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  ASectionCount: Integer;
begin
  AStartSectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  AEndSectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
  ASection := SourceModel.Sections[AStartSectionIndex];
  ASectionCount := AEndSectionIndex - AStartSectionIndex + 1;
  if SourcePieceTable.Paragraphs[ASection.FirstParagraphIndex].FirstRunIndex = AInfo.Start.RunIndex then
    Result := ASectionCount
  else
  begin
    ProcessSectionHeadCore(AInfo, AStartSectionIndex, ADocumentLastParagraphSelected);
    Result := ASectionCount - 1;
  end;
end;

function TdxCopySectionOperation.ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex;
  ALastRunIndex: TdxRunIndex;
begin
  AEndSectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
  AStartSectionIndex := AEndSectionIndex - AParagraphCount + 1;
  ALastRunIndex := SourcePieceTable.Paragraphs[SourceModel.Sections[AEndSectionIndex].LastParagraphIndex].LastRunIndex;

  if (AInfo.&End.RunIndex = ALastRunIndex) and (ALastRunIndex <> SourcePieceTable.Runs.Count - 1) then
  begin
    ProcessSections(AStartSectionIndex, AParagraphCount);
    Result := False;
  end
  else
  begin
    ProcessSections(AStartSectionIndex, AParagraphCount - 1);
    Result := True;
  end;
end;

function TdxCopySectionOperation.ProcessTail(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Integer;
var
  ASectionIndex: TdxSectionIndex;
  AStartLogPosition: TdxDocumentLogPosition;
  ASelectionContentInfo: TdxRunInfo;
begin
  ASectionIndex := SourcePieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
  AStartLogPosition := SourcePieceTable.Paragraphs[SourceModel.Sections[ASectionIndex].FirstParagraphIndex].LogPosition;
  TryCopyLastSection(AInfo, ADocumentLastParagraphSelected);
  ASelectionContentInfo := SourcePieceTable.ObtainAffectedRunInfo(AStartLogPosition, AInfo.&End.LogPosition - AStartLogPosition + 1);
  try
    ProcessContentInsideParent(ASelectionContentInfo, True, False);
  finally
    ASelectionContentInfo.Free;
  end;
  Result := 0;
end;

function TdxCopySectionOperation.CreateSectionCopy(ASourceSection: TdxSection): TdxSection;
begin
  Result := ASourceSection.Copy(CopyManager);
end;

function TdxCopySectionOperation.IsTargetSectionLast: Boolean;
var
  ATargetSectionIndex, ALastSectionIndex: TdxSectionIndex;
begin
  ATargetSectionIndex := TargetPieceTable.LookupSectionIndexByParagraphIndex(TargetPosition.ParagraphIndex);
  ALastSectionIndex := TargetModel.Sections.Count - 1;
  Result := ATargetSectionIndex = ALastSectionIndex;
end;

procedure TdxCopySectionOperation.JoinTables(ARunInfo: TdxRunInfo);
var
  ASourceFirstCell: TdxTableCell;
  APreviousTargetCell: TdxTableCell;
  ATargetStartParagraphIndex: TdxParagraphIndex;
  APreviousTargetTable: TdxTable;
  ACell: TdxTableCell;
begin
  ASourceFirstCell := SourcePieceTable.Paragraphs[ARunInfo.NormalizedStart.ParagraphIndex].GetCell;
  if ASourceFirstCell = nil then
    Exit;

  ATargetStartParagraphIndex := CopyManager.TableCopyHelper.TargetStartParagraphIndex;
  if ATargetStartParagraphIndex = 0 then
    Exit;

  APreviousTargetCell := TargetPieceTable.Paragraphs[ATargetStartParagraphIndex - 1].GetCell;
  if APreviousTargetCell = nil then
    Exit;

  APreviousTargetTable := APreviousTargetCell.Table;
  ACell := TargetPieceTable.Paragraphs[APreviousTargetTable.LastRow.LastCell.EndParagraphIndex + 1].GetCell;
  if (ACell = nil) or (APreviousTargetTable.NestedLevel <> ACell.Table.NestedLevel) then
    Exit;

  TargetPieceTable.JoinTables(APreviousTargetTable, ACell.Table);
end;

procedure TdxCopySectionOperation.ProcessSections(AStartSectionIndex: TdxSectionIndex; ASectionCount: Integer);
var
  AEndSectionIndex: TdxSectionIndex;
  ASections: TdxSectionCollection;
  I: Integer;
  ASourceSection: TdxSection;
  ATargetSection: TdxSection;
begin
  AEndSectionIndex := AStartSectionIndex + ASectionCount;
  ASections := SourceModel.Sections;
  for I := AStartSectionIndex to AEndSectionIndex - 1 do
  begin
    ASourceSection := ASections[I];
    ATargetSection := CreateSectionCopy(ASourceSection);
    ProcessSectionsCore(I);
    CopyManager.OnTargetSectionInserted(ASourceSection, ATargetSection);
  end;
end;

procedure TdxCopySectionOperation.ProcessSectionsCore(AIndex: TdxSectionIndex);
var
  ASection: TdxSection;
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  ASelectionContentInfo: TdxRunInfo;
begin
  ASection := SourceModel.Sections[AIndex];
  AStartLogPosition := SourcePieceTable.Paragraphs[ASection.FirstParagraphIndex].LogPosition;
  AEndLogPosition := SourcePieceTable.Paragraphs[ASection.LastParagraphIndex].EndLogPosition;
  if AEndLogPosition <> AStartLogPosition then
  begin
    ASelectionContentInfo := SourcePieceTable.ObtainAffectedRunInfo(AStartLogPosition, AEndLogPosition - AStartLogPosition);
    try
      ProcessContentInsideParent(ASelectionContentInfo, True, False);
    finally
      ASelectionContentInfo.Free;
    end;
  end;
end;

function TdxCopySectionOperation.ShouldCopySection(ASourceSection,
  ATargetSection: TdxSection): Boolean;
begin
  Result := not SuppressCopySectionProperties and
    (TdxDocumentModel(ATargetSection.DocumentModel).ModelForExport or IsEmptySection(ATargetSection));
end;

function TdxCopySectionOperation.TryCopyLastSection(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ALastRunIndex: TdxRunIndex;
  AIsLastParagraphMarkSelected: Boolean;
  ASourceSection, ATargetSection: TdxSection;
begin
  Result := False;
  if not AffectsMainPieceTable then
    Exit;
  ALastRunIndex := PieceTable.Runs.Count - 1;
  AIsLastParagraphMarkSelected := (AInfo.&End.RunIndex = ALastRunIndex) or ADocumentLastParagraphSelected;
  if not AIsLastParagraphMarkSelected or not IsTargetSectionLast then
    Exit;

  ASourceSection := SourceModel.Sections.Last;
  ATargetSection := TargetModel.Sections.Last;
  if not ShouldCopySection(ASourceSection, ATargetSection) then
    Exit;

  ATargetSection.CopyFromCore(ASourceSection);
  ATargetSection.Headers.CopyFrom(ASourceSection);
  ATargetSection.Footers.CopyFrom(ASourceSection);
  Result := True;
end;

function TdxCopySectionOperation.GetAffectsMainPieceTable: Boolean;
begin
  Result := SourcePieceTable = SourceModel.MainPieceTable;
end;

function TdxCopySectionOperation.GetSourceModel: TdxDocumentModel;
begin
  Result := FCopyManager.SourceModel;
end;

function TdxCopySectionOperation.GetTargetModel: TdxDocumentModel;
begin
  Result := FCopyManager.TargetModel;
end;

function TdxCopySectionOperation.GetSourcePieceTable: TdxPieceTable;
begin
  Result := FCopyManager.SourcePieceTable;
end;

function TdxCopySectionOperation.GetTargetPieceTable: TdxPieceTable;
begin
  Result := FCopyManager.TargetPieceTable;
end;

function TdxCopySectionOperation.GetTargetPosition: PdxDocumentModelPosition;
begin
  Result := @FCopyManager.TargetPosition;
end;

function TdxCopySectionOperation.SourceAndTargetModelAreDifferent: Boolean;
begin
  Result := CopyManager.SourceModel <> CopyManager.TargetModel;
end;

function TdxCopySectionOperation.IsEmptySection(ATargetSection: TdxSection): Boolean;
var
  AFirstParagraphIndex: TdxParagraphIndex;
begin
  Result := False;
  if ATargetSection.HasNonEmptyHeadersOrFooters then
    Exit;

  AFirstParagraphIndex := ATargetSection.FirstParagraphIndex;
  if AFirstParagraphIndex <> ATargetSection.LastParagraphIndex then
    Exit;

  Result := ATargetSection.DocumentModel.MainPart.Paragraphs[AFirstParagraphIndex].IsEmpty;
end;

procedure TdxCopySectionOperation.ProcessSectionHeadCore(AInfo: TdxRunInfo;
  AStartSectionIndex: TdxSectionIndex; ADocumentLastParagraphSelected: Boolean);
var
  ASection: TdxSection;
  AEndParagraph: TdxParagraph;
  ASelectionContentInfo: TdxRunInfo;
begin
  ASection := SourceModel.Sections[AStartSectionIndex];
  AEndParagraph := SourcePieceTable.Paragraphs[ASection.LastParagraphIndex];
  ASelectionContentInfo := SourcePieceTable.ObtainAffectedRunInfo(AInfo.Start.LogPosition,
    AEndParagraph.EndLogPosition - AInfo.Start.LogPosition + 1);
  try
    ProcessRunParent(ASelectionContentInfo, ADocumentLastParagraphSelected);
  finally
    ASelectionContentInfo.Free;
  end;
end;

{ TdxDeleteParagraphOperation }

function TdxDeleteParagraphOperation.ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean;
var
  AStart, AEnd: TdxDocumentModelPosition;
  AParagraph: TdxParagraphBase;
begin
  AStart := AInfo.Start;
  AEnd := AInfo.&End;
  AParagraph := PieceTable.Paragraphs[AStart.ParagraphIndex];
  Result := (AStart.RunIndex = AParagraph.LastRunIndex) and (AEnd.RunIndex = AParagraph.LastRunIndex);
end;

function TdxDeleteParagraphOperation.ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean;
var
  AStart, AEnd: TdxDocumentModelPosition;
  AParagraphs: TdxParagraphBaseCollection;
begin
  AStart := AInfo.Start;
  AEnd := AInfo.&End;
  AParagraphs := PieceTable.Paragraphs;
  Result := (AStart.ParagraphIndex = AEnd.ParagraphIndex) and
    ((AStart.RunIndex <> AParagraphs[AStart.ParagraphIndex].FirstRunIndex) or
    (AEnd.RunIndex <> AParagraphs[AStart.ParagraphIndex].LastRunIndex));
end;

procedure TdxDeleteParagraphOperation.ProcessAllParagraphRuns(AParagraphIndex: TdxParagraphIndex);
begin
  DocumentModel.UnsafeEditor.DeleteAllRunsInParagraph(PieceTable, AParagraphIndex);
end;

function TdxDeleteParagraphOperation.ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AStartParagraph: TdxParagraphBase;
  AStartInfo: TdxRunInfo;
begin
  AStartParagraph := PieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  AStartInfo := TdxRunInfo.Create(PieceTable);
  try
    AStartInfo.Start.CopyFrom(AInfo.Start);
    TdxDocumentModelPosition.SetParagraphEnd(@AStartInfo.&End, AStartParagraph.Index);
    ProcessContentSameParent(AStartInfo, DocumentModel.EditingOptions.MergeParagraphsContent,
      ADocumentLastParagraphSelected);
    Result := 0;
  finally
    AStartInfo.Free;
  end;
end;

function TdxDeleteParagraphOperation.ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  AStartParagraph: TdxParagraphBase;
begin
  AStartParagraph := PieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  Result := AInfo.Start.RunIndex <> AStartParagraph.FirstRunIndex;
  if Result then
    ProcessParagraphs(AStartParagraph.Index + 1, AParagraphCount)
  else
    ProcessParagraphs(AStartParagraph.Index, AParagraphCount + 1);
end;

procedure TdxDeleteParagraphOperation.ProcessNumberingRun(AParagraph: TdxParagraph; AUseFirstParagraphStyle: Boolean);
var
  ANextParagraph: TdxSimpleParagraph;
begin
  ANextParagraph := PieceTable.Paragraphs[AParagraph.Index + 1];
  if ANextParagraph.IsInList and AParagraph.IsInList and AUseFirstParagraphStyle then
    Exit;
  if AParagraph.IsInList and not AUseFirstParagraphStyle then
    PieceTable.RemoveNumberingFromParagraph(AParagraph);
  if ANextParagraph.IsInList then
  begin
    if not AUseFirstParagraphStyle then
      PieceTable.AddNumberingListToParagraph(AParagraph, ANextParagraph.GetNumberingListIndex,
        ANextParagraph.GetListLevelIndex);
    PieceTable.RemoveNumberingFromParagraph(ANextParagraph);
  end;
end;

procedure TdxDeleteParagraphOperation.ProcessParagraphsCore(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer);
var
  AEndParagraphIndex: TdxParagraphIndex;
  I: TdxParagraphIndex;
begin
  AEndParagraphIndex := AStartParagraphIndex + ACount;
  for I := AStartParagraphIndex to AEndParagraphIndex - 1 do
    ProcessAllParagraphRuns(I);
end;

procedure TdxDeleteParagraphOperation.ProcessParagraphs(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer);
var
  AIsNeedJoinTables: Boolean;
begin
  ProcessParagraphsCore(AStartParagraphIndex, ACount);
  AIsNeedJoinTables := IsNeedJoinTables(AStartParagraphIndex, AStartParagraphIndex + ACount - 1);
  DocumentModel.UnsafeEditor.DeleteParagraphs(PieceTable, AStartParagraphIndex, ACount, nil);
  if AIsNeedJoinTables then
    JoinTables(AStartParagraphIndex);
end;

procedure TdxDeleteParagraphOperation.ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
var
  AParagraph, AParagraphInTable: TdxParagraph;
  AParagraphIndex: TdxParagraphIndex;
  I, AFieldsCounter: Integer;
  ARun: TdxTextRunBase;
  AIsNeedJoinTables: Boolean;
  ACell, ANextCell: TdxTableCell;
begin
  if IsDeletedLastParagraphInCell(AInfo) then
    Exit;
  if PieceTable.Runs.Last = PieceTable.Runs[AInfo.Start.RunIndex] then
    Exit;
  AParagraph := PieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  AParagraphIndex := AParagraph.Index;
  if ParagraphContentIsNumberingRunOnly(AParagraph) then
    ProcessParagraphs(AParagraphIndex, 1)
  else
  begin
    AParagraphInTable := PieceTable.Paragraphs[AParagraphIndex + 1];
    ACell := AParagraph.GetCell;
    ANextCell := AParagraphInTable.GetCell;
    if (ANextCell <> nil) and ((ACell = nil) or (ACell.Table <> ANextCell.Table)) then
    begin
      AFieldsCounter := 0;
      for I := AParagraph.FirstRunIndex to AParagraph.LastRunIndex do
      begin
        ARun := PieceTable.Runs[i];
        if ARun is TdxFieldCodeStartRun then
          Inc(AFieldsCounter);
        if ARun is TdxFieldResultEndRun then
          Dec(AFieldsCounter);
      end;
      if AFieldsCounter <> 0 then
        Exit;
    end;
    ProcessNumberingRun(AParagraph, DocumentModel.EditingOptions.MergeUseFirstParagraphStyle);
    DeleteRuns(AParagraph.LastRunIndex, 1);
    AIsNeedJoinTables := IsNeedJoinTables(AParagraphIndex, AParagraphIndex);
    DocumentModel.UnsafeEditor.MergeParagraphs(PieceTable, AParagraph, AParagraphInTable, True, ACell);
    if AIsNeedJoinTables then
      JoinTables(AParagraphIndex);
  end;
end;

function TdxDeleteParagraphOperation.ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AEndParagraph: TdxParagraphBase;
  AStartRunIndex: TdxRunIndex;
  ARunCount: Integer;
begin
  AEndParagraph := PieceTable.Paragraphs[AInfo.&End.ParagraphIndex];
  AStartRunIndex := AEndParagraph.FirstRunIndex;
  Result := AEndParagraph.Index - AInfo.Start.ParagraphIndex;
  if AInfo.&End.RunIndex <> AEndParagraph.LastRunIndex then
  begin
    ARunCount := AInfo.&End.RunIndex - AStartRunIndex + 1;
    DeleteRuns(AStartRunIndex, ARunCount);
    Dec(Result);
  end;
end;

procedure TdxDeleteParagraphOperation.ProcessContentInsideParent(AInfo: TdxRunInfo;
  AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean);
var
  AShouldMergeParagraphs: Boolean;
begin
  AShouldMergeParagraphs := DeleteRunsInsideParagraphCore(AInfo, AAllowMergeWithNextParagraph);
  if AShouldMergeParagraphs then
    MergeParagraphWithNext(PieceTable.Paragraphs[AInfo.Start.ParagraphIndex]);
end;

procedure TdxDeleteParagraphOperation.ProcessContentCrossParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
begin
  if IsTheOnlyParagraph(AInfo.Start.ParagraphIndex) then
    Exit;
  FIsProcessContentCrossParentExecute := True;
  PieceTable.DeleteSelectedTables(AInfo, ADocumentLastParagraphSelected);
  if TdxDeleteTablesHelper.IsDeleteParagraphsInTable(AInfo, ADocumentLastParagraphSelected, IsDeletedSomeSections) then
    ProcessTable(AInfo, ADocumentLastParagraphSelected)
  else
    ProcessContentCrossParentCore(AInfo, ADocumentLastParagraphSelected);
end;

procedure TdxDeleteParagraphOperation.DeleteRuns(AStartIndex: TdxRunIndex; ARunCount: Integer);
begin
  DocumentModel.UnsafeEditor.DeleteRuns(PieceTable, AStartIndex, ARunCount);
end;

function TdxDeleteParagraphOperation.DeleteRunsInsideParagraphCore(
  AInfo: TdxRunInfo; AAllowMergeWithNextParagraph: Boolean): Boolean;
var
  AParagraph: TdxParagraphBase;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  AForbidMergeParagraph, AMergeParagraphs: Boolean;
  ARunCount: Integer;
begin
  AParagraph := PieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  AStartRunIndex := AInfo.Start.RunIndex;
  AEndRunIndex := AInfo.&End.RunIndex;
  AForbidMergeParagraph := False;
  AMergeParagraphs := (AEndRunIndex = AParagraph.LastRunIndex) and AAllowMergeWithNextParagraph;
  if AMergeParagraphs then
  begin
    AForbidMergeParagraph := (IsMergeParagraphWithTable(AInfo) and not IsProcessContentCrossParentExecute) or
      IsDeletedLastParagraphInCell(AInfo);
    AMergeParagraphs := AMergeParagraphs and not AForbidMergeParagraph;
  end;
  ARunCount := AEndRunIndex - AStartRunIndex + 1;
  if (AEndRunIndex = AParagraph.LastRunIndex) and not AAllowMergeWithNextParagraph or
    (AEndRunIndex = AParagraph.LastRunIndex) and AForbidMergeParagraph then
    Dec(ARunCount);
  DeleteRuns(AStartRunIndex, ARunCount);
  Result := AMergeParagraphs;
end;

function TdxDeleteParagraphOperation.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDeleteParagraphOperation.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxDeleteParagraphOperation.IsDeletedLastParagraphInCell(AInfo: TdxRunInfo): Boolean;
var
  AEndParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  ACell: TdxTableCell;
begin
  AEndParagraphIndex := AInfo.NormalizedEnd.ParagraphIndex;
  AParagraphs := PieceTable.Paragraphs;
  ACell := AParagraphs[AEndParagraphIndex].GetCell;
  if ACell = nil then
    Exit(False);
  Result := AParagraphs[ACell.EndParagraphIndex].LastRunIndex = AInfo.NormalizedEnd.RunIndex;
end;

function TdxDeleteParagraphOperation.IsMergeParagraphWithTable(AInfo: TdxRunInfo): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  AStartParagraphIndex: TdxParagraphIndex;
  AStartParagraph, ANextParagraph: TdxParagraph;
  AStartCell, ANextCell: TdxTableCell;
  ASelectOnlyParagraph: Boolean;
  AStartTable, ANextTable: TdxTable;
begin
  AParagraphs := PieceTable.Paragraphs;
  AStartParagraphIndex := AInfo.NormalizedStart.ParagraphIndex;
  if AStartParagraphIndex + 1 = AParagraphs.Count then
    Exit(False);
  AStartParagraph := AParagraphs[AStartParagraphIndex];
  AStartCell := AStartParagraph.GetCell;
  ANextParagraph := AParagraphs[AStartParagraphIndex + 1];
  ASelectOnlyParagraph := AStartParagraph.LastRunIndex = AInfo.NormalizedEnd.RunIndex;
  ANextCell := ANextParagraph.GetCell;
  if ((AStartCell = nil) and (ANextCell <> nil)) and ASelectOnlyParagraph then
    Exit(True);
  if AStartCell <> nil then
    AStartTable := AStartCell.Table
  else
    AStartTable := nil;
  if ANextCell <> nil then
    ANextTable := ANextCell.Table
  else
    ANextTable := nil;
  if (AStartTable <> nil) and (ANextTable <> nil) and (AStartTable <> ANextTable) and
    (AStartTable.NestedLevel <> ANextTable.NestedLevel) and ASelectOnlyParagraph then
    Exit(True);
  Result := False;
end;

function TdxDeleteParagraphOperation.IsNeedJoinTables(ATopParagraphIndex, ABottomParagraphIndex: TdxParagraphIndex): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  APreviousCell: TdxTableCell;
  APreviousTable: TdxTable;
  APreviousTableNestedLevel: Integer;
  ANextCell: TdxTableCell;
  ANextTable: TdxTable;
begin
  Result := False;
  AParagraphs := PieceTable.Paragraphs;
  if (ATopParagraphIndex = 0) or (ABottomParagraphIndex = AParagraphs.Count - 1) then
    Exit;
  APreviousCell := AParagraphs[ATopParagraphIndex - 1].GetCell;
  if APreviousCell = nil then
    Exit;
  APreviousTable := APreviousCell.Table;
  APreviousTableNestedLevel := APreviousTable.NestedLevel;
  ANextCell := PieceTable.TableCellsManager.GetCellByNestingLevel(ABottomParagraphIndex + 1,
    APreviousTableNestedLevel);
  if ANextCell = nil then
    Result := False
  else
  begin
    ANextTable := ANextCell.Table;
    if (APreviousTable <> ANextTable) and (APreviousTableNestedLevel = ANextTable.NestedLevel) then
      Result := APreviousTable.ParentCell = ANextTable.ParentCell;
  end;
end;

function TdxDeleteParagraphOperation.IsTheOnlyParagraph(AParagraphIndex: TdxParagraphIndex): Boolean;
begin
  Result := AParagraphIndex >= PieceTable.Paragraphs.Count - 1;
end;

procedure TdxDeleteParagraphOperation.JoinTables(AParagraphIndex: TdxParagraphIndex);
var
  AParagraphs: TdxParagraphCollection;
  APreviousParagraph: TdxParagraph;
  ATopTable, ABottomTable: TdxTable;
begin
  AParagraphs := PieceTable.Paragraphs;
  APreviousParagraph := AParagraphs[AParagraphIndex - 1];
  ATopTable := APreviousParagraph.GetCell.Table;
  ABottomTable := PieceTable.TableCellsManager.GetCellByNestingLevel(AParagraphIndex, ATopTable.NestedLevel).Table;
  ATopTable.NormalizeCellColumnSpans;
  ABottomTable.NormalizeCellColumnSpans;
  PieceTable.JoinTables(ATopTable, ABottomTable);
end;

procedure TdxDeleteParagraphOperation.MergeParagraphWithNext(AParagraph: TdxParagraph);
var
  AParagraphIndex, ANextParagraph: TdxParagraphIndex;
  ACell: TdxTableCell;
  AIsNeedJoinTables: Boolean;
begin
  AParagraphIndex := AParagraph.Index;
  ANextParagraph := AParagraphIndex + 1;
  ProcessNumberingRun(AParagraph, DocumentModel.EditingOptions.MergeUseFirstParagraphStyle);
  ACell := AParagraph.GetCell;
  AIsNeedJoinTables := IsNeedJoinTables(AParagraphIndex, AParagraphIndex);
  DocumentModel.UnsafeEditor.MergeParagraphs(PieceTable, AParagraph,
    PieceTable.Paragraphs[ANextParagraph], DocumentModel.EditingOptions.MergeUseFirstParagraphStyle, ACell);
  if AIsNeedJoinTables then
    JoinTables(AParagraphIndex);
end;

procedure TdxDeleteParagraphOperation.ProcessContentCrossParentCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
var
  AOptions: TdxDeleteTablesOptions;
  AParagraphCount: Integer;
  AShouldDeleteHead: Boolean;
begin
  AOptions := TdxDeleteTablesOptions.Create(ADocumentLastParagraphSelected, IsDeletedSomeSections, BackspacePressed);
  try
    TdxDeleteTablesHelper.DeleteSelectedTableRows(AInfo, AOptions);
    AParagraphCount := ProcessTail(AInfo, ADocumentLastParagraphSelected);
    AShouldDeleteHead := ProcessMiddle(AInfo, AParagraphCount, ADocumentLastParagraphSelected);
    if AShouldDeleteHead then
      ProcessHead(AInfo, ADocumentLastParagraphSelected);
  finally
    AOptions.Free;
  end;
end;

function TdxDeleteParagraphOperation.ProcessMiddleTable(AInfo: TdxRunInfo; AParagraphCount: Integer): Boolean;
var
  AStartParagraph: TdxParagraph;
begin
  AStartParagraph := PieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  if AInfo.Start.RunIndex = AStartParagraph.FirstRunIndex then
  begin
    ProcessParagraphsInTable(AStartParagraph.Index, AParagraphCount + 1);
    Exit(False);
  end
  else
  begin
    ProcessParagraphsInTable(AStartParagraph.Index + 1, AParagraphCount);
    Exit(True);
  end;
end;

procedure TdxDeleteParagraphOperation.ProcessTable(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
var
  AOptions: TdxDeleteTablesOptions;
begin
  AOptions := TdxDeleteTablesOptions.Create(ADocumentLastParagraphSelected, IsDeletedSomeSections, BackspacePressed);
  try
    TdxDeleteTablesHelper.DeleteSelectedTableRows(AInfo, AOptions);
    ProcessContentCrossParentTable(AInfo, ADocumentLastParagraphSelected);
  finally
    AOptions.Free;
  end;
end;

procedure TdxDeleteParagraphOperation.ProcessContentCrossParentTable(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
var
  AParagraphCount: Integer;
  AShouldDeleteHead: Boolean;
begin
  AParagraphCount := ProcessTail(AInfo, ADocumentLastParagraphSelected);
  AShouldDeleteHead := ProcessMiddleTable(AInfo, AParagraphCount);
  if AShouldDeleteHead then
    ProcessHead(AInfo, ADocumentLastParagraphSelected);
end;


procedure TdxDeleteParagraphOperation.ProcessParagraphsInTable(AStartParagraphIndex: TdxParagraphIndex; ACount: Integer);
var
  AEndParagraphIndex, I: TdxParagraphIndex;
  AIsNeedJoinTables: Boolean;
  ACurrentCell: TdxTableCell;
  AParagraph: TdxParagraph;
  ARunsCount: Integer;
begin
  AEndParagraphIndex := AStartParagraphIndex + ACount - 1;
  AIsNeedJoinTables := IsNeedJoinTables(AStartParagraphIndex, AEndParagraphIndex);
  for I := AEndParagraphIndex downto AStartParagraphIndex do
  begin
    ACurrentCell := PieceTable.Paragraphs[I].GetCell;
    if (ACurrentCell = nil) or (ACurrentCell.EndParagraphIndex <> I) or (AllowedDeleteLastParagraphInTableCell) then
    begin
      ProcessParagraphsCore(I, 1);
      DocumentModel.UnsafeEditor.DeleteParagraphs(PieceTable, I, 1, ACurrentCell);
    end
    else
    begin
      AParagraph := PieceTable.Paragraphs[I];
      ARunsCount := AParagraph.LastRunIndex - AParagraph.FirstRunIndex;
      if ARunsCount > 0 then
        DeleteRuns(AParagraph.FirstRunIndex, ARunsCount);
    end;
  end;
  if AIsNeedJoinTables then
    JoinTables(AStartParagraphIndex);
end;

function TdxDeleteParagraphOperation.ParagraphContentIsNumberingRunOnly(AParagraph: TdxParagraph): Boolean;
begin
  Result := AParagraph.IsInList and (AParagraph.Length <= 1);
end;

{ TdxDeleteSectionOperation }

function TdxDeleteSectionOperation.ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  ASections: TdxSectionCollection;
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex;
begin
  if not AffectsMainPieceTable then
    Exit(True);
  AParagraphs := PieceTable.Paragraphs;
  ASections := DocumentModel.Sections;
  AStartSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  AEndSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
  Result := (AStartSectionIndex = AEndSectionIndex) and
    ((AInfo.Start.RunIndex <> AParagraphs[ASections[AStartSectionIndex].FirstParagraphIndex].FirstRunIndex) or
    (AInfo.&End.RunIndex <> AParagraphs[ASections[AStartSectionIndex].LastParagraphIndex].LastRunIndex));
end;

function TdxDeleteSectionOperation.ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean;
var
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AEndRun: TdxTextRunBase;
  ALastSectionRun: TdxRunIndex;
begin
  if not AffectsMainPieceTable then
    Exit(False);
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  ASection := DocumentModel.Sections[ASectionIndex];
  AEndRun := PieceTable.Runs[AInfo.&End.RunIndex];
  ALastSectionRun := PieceTable.Paragraphs[ASection.LastParagraphIndex].LastRunIndex;
  Result := (AInfo.&End.RunIndex = ALastSectionRun) and (AEndRun is TdxSectionRun);
end;

procedure TdxDeleteSectionOperation.ProcessContentInsideParent(
  AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean);
var
  ADeleteParagraphOperation: TdxDeleteParagraphOperation;
begin
  ADeleteParagraphOperation := TdxDeleteParagraphOperation.Create(PieceTable);
  try
    ADeleteParagraphOperation.IsDeletedSomeSections := FIsDeletedSomeSections;
    ADeleteParagraphOperation.BackspacePressed := BackspacePressed;
    ADeleteParagraphOperation.ExecuteCore(AInfo, ADocumentLastParagraphSelected);
  finally
    ADeleteParagraphOperation.Free;
  end;
end;

procedure TdxDeleteSectionOperation.DeleteAllParagraphsInsideSection(
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex);
var
  I: TdxSectionIndex;
  ASection: TdxSection;
  AParagraphCount: Integer;
  ADeleteParagraph: TdxDeleteParagraphOperation;
  AParagraphs: TdxParagraphCollection;
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
begin
  for I := AStartSectionIndex to AEndSectionIndex - 1 do
  begin
    ASection := DocumentModel.Sections[I];
    AParagraphCount := ASection.LastParagraphIndex - ASection.FirstParagraphIndex + 1;
    ADeleteParagraph := TdxDeleteParagraphOperation.Create(PieceTable);
    try
      AParagraphs := PieceTable.Paragraphs;
      AStartLogPosition := AParagraphs[ASection.FirstParagraphIndex].LogPosition;
      AEndLogPosition := AParagraphs[ASection.FirstParagraphIndex + AParagraphCount - 1].EndLogPosition + 1;
      ADeleteParagraph.Execute(AStartLogPosition, AEndLogPosition - AStartLogPosition, AEndLogPosition = PieceTable.DocumentEndLogPosition);
    finally
      ADeleteParagraph.Free;
    end;
  end;
end;

procedure TdxDeleteSectionOperation.DeleteTail(AInfo: TdxRunInfo; AEndSection: TdxSection; ADocumentLastParagraphSelected: Boolean);
var
  AStartLogPosition: TdxDocumentLogPosition;
  ASectionContentInfo: TdxRunInfo;
  AStartSectionIndex, AEndSectionIndex: TdxSectionIndex;
begin
  AStartLogPosition := PieceTable.Paragraphs[AEndSection.FirstParagraphIndex].LogPosition;
  ASectionContentInfo := PieceTable.ObtainAffectedRunInfo(AStartLogPosition, AInfo.&End.LogPosition - AStartLogPosition + 1);
  try
    AStartSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.NormalizedStart.ParagraphIndex);
    AEndSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.NormalizedEnd.ParagraphIndex);
    FIsDeletedSomeSections := AStartSectionIndex <> AEndSectionIndex;
    ProcessContentInsideParent(ASectionContentInfo, True, ADocumentLastParagraphSelected);
  finally
    ASectionContentInfo.Free;
  end;
end;

function TdxDeleteSectionOperation.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDeleteSectionOperation.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

procedure TdxDeleteSectionOperation.ProcessContentCrossParent(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
var
  ASectionCount: Integer;
  AShouldDeleteHead: Boolean;
begin
  ASectionCount := ProcessTail(AInfo, ADocumentLastParagraphSelected);
  AShouldDeleteHead := ProcessMiddle(AInfo, ASectionCount, ADocumentLastParagraphSelected);
  if AShouldDeleteHead then
    ProcessHead(AInfo, ADocumentLastParagraphSelected);
end;

function TdxDeleteSectionOperation.ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AStartSectionIndex: TdxSectionIndex;
  AParagraph: TdxParagraph;
  ALength: Integer;
  ASectionContentInfo: TdxRunInfo;
begin
  AStartSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  AParagraph := PieceTable.Paragraphs[DocumentModel.Sections[AStartSectionIndex].LastParagraphIndex];
  ALength := AParagraph.LogPosition + AParagraph.Length - AInfo.Start.LogPosition;
  ASectionContentInfo := PieceTable.ObtainAffectedRunInfo(AInfo.Start.LogPosition, ALength);
  try
    DocumentModel.UnsafeEditor.DeleteSections(AStartSectionIndex, 1);
    ProcessContentInsideParent(ASectionContentInfo, True, ADocumentLastParagraphSelected);
  finally
    ASectionContentInfo.Free;
  end;
  Result := 0;
end;

procedure TdxDeleteSectionOperation.ProcessRunParent(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
var
  ASectionIndex: TdxSectionIndex;
begin
  if AffectsMainPieceTable then
  begin
    ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
    DocumentModel.UnsafeEditor.DeleteSections(ASectionIndex, 1);
  end;
  ProcessContentInsideParent(AInfo, True, ADocumentLastParagraphSelected);
end;

procedure TdxDeleteSectionOperation.ProcessSections(AStartSectionIndex:
  TdxSectionIndex; ACount: Integer);
var
  AEndSectionIndex: TdxSectionIndex;
begin
  AEndSectionIndex := AStartSectionIndex + ACount;
  DeleteAllParagraphsInsideSection(AStartSectionIndex, AEndSectionIndex);
  DocumentModel.UnsafeEditor.DeleteSections(AStartSectionIndex, ACount);
end;

function TdxDeleteSectionOperation.ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  AStartSectionIndex: TdxSectionIndex;
  AStartSection: TdxSection;
begin
  AStartSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  AStartSection := DocumentModel.Sections[AStartSectionIndex];
  if AInfo.Start.RunIndex = PieceTable.Paragraphs[AStartSection.FirstParagraphIndex].FirstRunIndex then
  begin
    ProcessSections(AStartSectionIndex, AParagraphCount + 1);
    Result := False;
  end
  else
  begin
    ProcessSections(AStartSectionIndex + 1, AParagraphCount);
    Result := True;
  end;
end;

function TdxDeleteSectionOperation.ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AEndSectionIndex: TdxSectionIndex;
  AStartSectionIndex: TdxSectionIndex;
  AEndSection: TdxSection;
  AParagraphCount: Integer;
begin
  AEndSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.&End.ParagraphIndex);
  AStartSectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(AInfo.Start.ParagraphIndex);
  AEndSection := DocumentModel.Sections[AEndSectionIndex];
  AParagraphCount := AEndSectionIndex - AStartSectionIndex;
  if AInfo.&End.RunIndex <> PieceTable.Paragraphs[AEndSection.LastParagraphIndex].LastRunIndex then
  begin
    DeleteTail(AInfo, AEndSection, ADocumentLastParagraphSelected);
    Result := AParagraphCount - 1;
  end
  else
    Result := AParagraphCount;
end;

function TdxDeleteSectionOperation.AffectsMainPieceTable: Boolean;
begin
  Result := PieceTable = DocumentModel.MainPieceTable;
end;

{ TdxDeleteContentWithFieldsOperation }

procedure TdxDeleteContentWithFieldsOperation.ExecuteCore(ARunInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean);
begin
  DeleteFieldsWithinInterval(ARunInfo);
  DeleteContentWithFieldsCore(ARunInfo.Start, ARunInfo.&End, ADocumentLastParagraphSelected);
end;

function TdxDeleteContentWithFieldsOperation.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDeleteContentWithFieldsOperation.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

procedure TdxDeleteContentWithFieldsOperation.DeleteFieldsWithinInterval(ARunInfo: TdxRunInfo);
var
  ADeletedFields: TdxFieldList;
  ACount, I: Integer;
begin
  ADeletedFields := CalculateDeletedFields(ARunInfo);
  try
    ACount := ADeletedFields.Count;
    for I := 0 to ACount - 1 do
      PieceTable.RemoveField(ADeletedFields[I]);
  finally
    ADeletedFields.Free;
  end;
end;

function TdxDeleteContentWithFieldsOperation.CalculateDeletedFields(ARunInfo: TdxRunInfo): TdxFieldList;
var
  AIndex: Integer;
  AField: TdxField;
  AFieldInInterval, AFieldShouldBeRemove: Boolean;
begin
  Result := TdxFieldList.Create;
  for AIndex := PieceTable.Fields.Count - 1 downto 0 do
  begin
    AField := PieceTable.Fields[AIndex];
    AFieldInInterval := IsFieldWithinInterval(ARunInfo, AField);
    AFieldShouldBeRemove := ForceRemoveInnerFields or not IsFieldHidByParent(AField) or
      IsParentShouldBeRemoved(AField, ARunInfo);
    if AFieldInInterval and AFieldShouldBeRemove then
      Result.Add(AField);
  end;
end;

function TdxDeleteContentWithFieldsOperation.IsFieldWithinInterval(ARunInfo: TdxRunInfo; AField: TdxField): Boolean;
var
  AStartIndex, AEndIndex: TdxRunIndex;
begin
  AStartIndex := ARunInfo.Start.RunIndex;
  AEndIndex := ARunInfo.&End.RunIndex;
  Result := (AField.FirstRunIndex >= AStartIndex) and (AField.LastRunIndex <= AEndIndex);
end;

function TdxDeleteContentWithFieldsOperation.IsParentShouldBeRemoved(AField: TdxField; ARunInfo: TdxRunInfo): Boolean;
begin
  Result := (AField.Parent <> nil) and IsFieldWithinInterval(ARunInfo, AField.Parent);
end;

function TdxDeleteContentWithFieldsOperation.IsFieldHidByParent(AField: TdxField): Boolean;
var
  AParent: TdxField;
  AInvisibleInterval: TdxFieldRunInterval;
begin
  AParent := AField.Parent;
  if AParent <> nil then
  begin
    if AParent.IsCodeView then
      AInvisibleInterval := AParent.Result
    else
      AInvisibleInterval := AParent.Code;
    Exit(AInvisibleInterval.Contains(AField));
  end;
  Result := False;
end;

function TdxDeleteContentWithFieldsOperation.CreateDeleteContentOperation: TdxSelectionBasedOperation;
begin
  Result := TdxDeleteSectionOperation.Create(PieceTable);
end;

procedure TdxDeleteContentWithFieldsOperation.DeleteContentWithFieldsCore(const AStart,
  AEnd: TdxDocumentModelPosition; ADocumentLastParagraphSelected: Boolean);
var
  ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition;
  ALength: Integer;
begin
  if AllowPartiallyDeletingField then
  begin
    DeleteContent(AStart, AEnd, ADocumentLastParagraphSelected);
    Exit;
  end;

  ARunIndex := AEnd.RunIndex;
  ALogPosition := AEnd.LogPosition;
  while ARunIndex >= AStart.RunIndex do
  begin
    ALength := 0;
    while (ARunIndex >= AStart.RunIndex) and not ShouldSkipRun(ARunIndex) do
    begin
      Inc(ALength, PieceTable.Runs[ARunIndex].Length);
      Dec(ARunIndex);
    end;
    if ALength > 0 then
    begin
      DeleteContent(ALogPosition - ALength + 1, ALength, ADocumentLastParagraphSelected);
      Dec(ALogPosition, ALength);
    end
    else
    begin
      Dec(ALogPosition, PieceTable.Runs[ARunIndex].Length);
      Dec(ARunIndex);
    end;
  end;
end;

function TdxDeleteContentWithFieldsOperation.ShouldSkipRun(ARunIndex: TdxRunIndex): Boolean;
var
  AField: TdxField;
  AIsFieldMarkRun: Boolean;
  AInvisibleInterval: TdxFieldRunInterval;
begin
  AField := PieceTable.FindFieldByRunIndex(ARunIndex);
  if AField = nil then
    Exit(False);
  AIsFieldMarkRun := (AField.Code.Start = ARunIndex) or (AField.Code.&End = ARunIndex) or
    (AField.Result.&End = ARunIndex);
  if AIsFieldMarkRun or IsFieldHidByParent(AField) then
    Exit(True);
  if AField.IsCodeView then
    AInvisibleInterval := AField.Result
  else
    AInvisibleInterval := AField.Code;
  Result := AInvisibleInterval.Contains(ARunIndex);
end;

{ TdxDeleteContentOperation }

function TdxDeleteContentOperation.CreateDeleteContentOperation: TdxSelectionBasedOperation;
begin
  Result := TdxDeleteSectionOperation.Create(PieceTable);
  TdxDeleteSectionOperation(Result).BackspacePressed := BackspacePressed;
end;

function TdxDeleteContentOperation.DeleteFields(ARunInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ALeftField, ARightField: TdxField;
  AOperation: TdxDeleteContentWithFieldsOperation;
begin
  ALeftField := PieceTable.FindFieldByRunIndex(ARunInfo.Start.RunIndex);
  ARightField := PieceTable.FindFieldByRunIndex(ARunInfo.&End.RunIndex);
  AOperation := TdxDeleteContentWithFieldsOperation.Create(PieceTable);
  try
    AOperation.AllowPartiallyDeletingField := AllowPartiallyDeletingField;
    AOperation.ForceRemoveInnerFields := ForceRemoveInnerFields;
    if (ALeftField <> nil) or (ARightField <> nil) then
    begin
      if (ALeftField = ARightField) and ShouldDeleteField(ARunInfo, ALeftField) then
        ExtendDeletedInterval(ARunInfo, ALeftField)
      else
      begin
        AOperation.ExecuteCore(ARunInfo, ADocumentLastParagraphSelected);
        Exit(True);
      end;
    end;
    AOperation.DeleteFieldsWithinInterval(ARunInfo);
    Result := False;
  finally
    AOperation.Free;
  end;
end;

procedure TdxDeleteContentOperation.ExecuteCore(ARunInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
begin
  if not SuppressFieldDelete then
    if DeleteFields(ARunInfo, ADocumentLastParagraphSelected) then
      Exit;
  DeleteContent(ARunInfo.Start, ARunInfo.&End, ADocumentLastParagraphSelected);
end;

procedure TdxDeleteContentOperation.ExtendDeletedInterval(ARunInfo: TdxRunInfo;
  AField: TdxField);
begin
  TdxFieldsOperation.EnsureIntervalContainsField(ARunInfo, AField);
end;

function TdxDeleteContentOperation.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

function TdxDeleteContentOperation.ShouldDeleteField(ARunInfo: TdxRunInfo;
  AField: TdxField): Boolean;
begin
  Result := TdxFieldsOperation.IsEntireFieldResultAffected(ARunInfo, AField) and
    not LeaveFieldIfResultIsRemoved;
end;

procedure TdxDeleteContentOperation.BeforeExecute(ARunInfo: TdxRunInfo);
begin
  inherited BeforeExecute(ARunInfo);
  DeleteBookmarks(ARunInfo);
end;

procedure TdxDeleteContentOperation.DeleteBookmarks(ARunInfo: TdxRunInfo);
begin
  DeleteBookmarksCore(PieceTable.Bookmarks.InnerList, ARunInfo, ShouldDeleteBookmark);
  DeleteBookmarksCore(PieceTable.RangePermissions.InnerList, ARunInfo, CanDeleteRangePermission);
end;

procedure TdxDeleteContentOperation.DeleteBookmarksCore(ABookmarks: TObjectList;
  ARunInfo: TdxRunInfo; ACanDeleteBookmark: TdxCanDeleteBookmarkDelegate);
var
  I: Integer;
  ABookmark: TdxBookmarkBase;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  AStart := ARunInfo.Start.LogPosition;
  AEnd := ARunInfo.&End.LogPosition + 1;
  for I := ABookmarks.Count - 1 downto 0 do
  begin
    ABookmark := TdxBookmarkBase(ABookmarks[I]);
    if ACanDeleteBookmark(AStart, AEnd, ABookmark) then
      ABookmark.Delete(I);
  end;
end;

function TdxDeleteContentOperation.ShouldDeleteBookmark(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition;
  ABookmark: TdxBookmarkBase): Boolean;
begin
  if ABookmark.CanExpand then
    Result := (ABookmark.Start >= AStart) and (ABookmark.&End <= AEnd)
  else
    Result := (ABookmark.Start > AStart) and (ABookmark.&End < AEnd);
end;

function TdxDeleteContentOperation.CanDeleteRangePermission(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition;
  ABookmark: TdxBookmarkBase): Boolean;
begin
  Result := (ABookmark.Start > AStart) and (ABookmark.&End < AEnd);
end;

end.
