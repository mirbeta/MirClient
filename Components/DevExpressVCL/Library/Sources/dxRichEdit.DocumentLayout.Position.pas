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

unit dxRichEdit.DocumentLayout.Position;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCoreClasses, cxClasses, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.FloatingObjectRange;

type

  { TdxDocumentLayoutPosition }

  TdxDocumentLayoutPosition = class(TdxCloneable)
  public type
    TdxIsIntersectedWithPrevBoxFunc = function (ABox: TdxBoxBase): Boolean of object;
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FPieceTable: TdxCustomPieceTable;
    FLogPosition: TdxDocumentLogPosition;
    FDetailsLevel: TdxDocumentLayoutDetailsLevel;
    FPage: TdxPage;
    FFloatingObjectBoxPage: TdxPage;
    FPageArea: TdxPageArea;
    FColumn: TdxColumn;
    FRow: TdxRow;
    FBox: TdxBox;
    FCharacter: TdxCharacterBox;
    FTableRow: TdxTableRowViewInfoBase;
    FTableCell: TdxTableCellViewInfo;
    FLeftOffset: Integer;
    FRightOffset: Integer;
    FSuppressSuspendFormatting: Boolean;
    function BinarySearchTableCell(ATableViewInfoCollection: TdxTableViewInfoCollection): TdxTableCellViewInfo;
    function GetTableViewInfo(ATableViewInfoCollection: TdxTableViewInfoCollection): TdxTableViewInfo;
    function GetDocumentModel: TdxDocumentModel;
    function IsTableCellVerticalMerged: Boolean;
    procedure SetCharacter(const Value: TdxCharacterBox);
    procedure SetRow(const Value: TdxRow);
    procedure SetTableCell(const Value: TdxTableCellViewInfo);
    procedure SetTableRow(const Value: TdxTableRowViewInfoBase);
  protected
    function GetPieceTable: TdxPieceTable; virtual;
    procedure EnsureFormattingComplete; virtual;
    procedure EnsurePageSecondaryFormattingComplete(APage: TdxPage); virtual;
    function UpdateCore(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdatePageRecursive(APages: TdxPageCollection; AStartIndex: Integer;
      ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; virtual;
    function LookupPage(APages: TdxPageCollection; AStartIndex: Integer): TdxPage; virtual;
    function UpdatePageAreaRecursive(APage: TdxPage; AStartIndex: Integer;
      ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    procedure CheckPageAreaPieceTable(APageArea: TdxPageArea); virtual;
    function LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea; virtual;
    function UpdateColumnRecursive(AColumns: TdxColumnCollection; AStartIndex: Integer;
      ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdateRowRecursive(ARows: TdxRowCollection; AStartIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdateBoxRecursive(ARow: TdxRow; AStartIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdateCharacterBoxRecursive(ARow: TdxRow; AStartIndex: Integer;
      ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; virtual;
    function CreateEmptyClone: TdxDocumentLayoutPosition; virtual;
    function GetPageIndex(APages: TdxPageCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetPageIndex(APages: TdxPageCollection): Integer; overload; virtual;
    function GetPageAreaIndex(AAreas: TdxPageAreaCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetPageAreaIndex(AAreas: TdxPageAreaCollection): Integer; overload; virtual;
    function GetColumnIndex(AColumns: TdxColumnCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetColumnIndex(AColumns: TdxColumnCollection): Integer; overload; virtual;

    function FindBoxIndex(ACollection: TdxBoxList; AExactComparable: TdxBoxComparable;
      const AIsIntersect: TdxIsIntersectedWithPrevBoxFunc;
      AStartIndex, AEndIndex: Integer): Integer; overload;
    function FindBoxIndex(ACollection: TdxBoxList; AComparable, AExactComparable: TdxBoxComparable;
      const AIsIntersect: TdxIsIntersectedWithPrevBoxFunc;
      AStartIndex, AEndIndex: Integer): Integer; overload;

    function IsIntersectedWithPrevArea(AArea: TdxBoxBase): Boolean;
    function IsIntersectedWithPrevColumn(AColumn: TdxBoxBase): Boolean;
    function IsIntersectedWithPrevPage(APage: TdxBoxBase): Boolean;

    function GetRowIndex(ARows: TdxRowCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetRowIndex(ARows: TdxRowCollection): Integer; overload; virtual;
    function GetBoxIndex(ABoxes: TdxBoxCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetBoxIndex(ABoxes: TdxBoxCollection): Integer; overload; virtual;
    function GetCharIndex(ACharacters: TdxCharacterBoxCollection; AStartIndex, AEndIndex: Integer): Integer; overload; virtual;
    function GetCharIndex(ACharacters: TdxCharacterBoxCollection): Integer; overload; virtual;
    function GetLogPosition: TdxDocumentLogPosition; virtual;
    function GetUseLastAvailablePosition: Boolean; virtual;
    procedure SetUseLastAvailablePosition(const Value: Boolean); virtual;

    property UseLastAvailablePosition: Boolean read GetUseLastAvailablePosition write SetUseLastAvailablePosition;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition); reintroduce; virtual;
    destructor Destroy; override;

    procedure SetLogPosition(AValue: TdxDocumentLogPosition);
    procedure IncreaseDetailsLevel(ADetailsLevel: TdxDocumentLayoutDetailsLevel);
    procedure Invalidate; virtual;
    function IsValid(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; virtual;
    function UpdateCellRecursive(AColumn: TdxColumn; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; virtual;
    function Update(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; overload; virtual;
    function Update(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel;
      AUseLastAvailablePosition: Boolean): Boolean; overload; virtual;

    function Clone: TdxCloneable; override;
    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(AValue: TdxDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel); reintroduce; overload; virtual;

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property DetailsLevel: TdxDocumentLayoutDetailsLevel read FDetailsLevel;
    property Page: TdxPage read FPage write FPage;
    property FloatingObjectBoxPage: TdxPage read FFloatingObjectBoxPage write FFloatingObjectBoxPage;
    property LeftOffset: Integer read FLeftOffset write FLeftOffset;
    property RightOffset: Integer read FRightOffset write FRightOffset;
    property LogPosition: TdxDocumentLogPosition read GetLogPosition;
    property PageArea: TdxPageArea read FPageArea write FPageArea;
    property Column: TdxColumn read FColumn write FColumn;
    property Row: TdxRow read FRow write SetRow;
    property Box: TdxBox read FBox write FBox;
    property Character: TdxCharacterBox read FCharacter write SetCharacter;
    property TableRow: TdxTableRowViewInfoBase read FTableRow write SetTableRow;
    property TableCell: TdxTableCellViewInfo read FTableCell write SetTableCell;
    property SuppressSuspendFormatting: Boolean read FSuppressSuspendFormatting write FSuppressSuspendFormatting;
  end;
  TdxDocumentLayoutPositionClass = class of TdxDocumentLayoutPosition;

  { TdxTextBoxDocumentLayoutPosition }

  TdxTextBoxDocumentLayoutPosition = class(TdxDocumentLayoutPosition)
  strict private
    FAnchorPieceTable: TdxPieceTable;
    FPreferredPageIndex: Integer;
    FUseLastAvailablePosition: Boolean;
    function IsIntersectedWithPrevBox(ABox: TdxBoxBase): Boolean;
  protected
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
    function GetPageIndex(APages: TdxPageCollection; AStartIndex, AEndIndex: Integer): Integer; override;
    function GetPieceTable: TdxPieceTable; override;
    procedure CheckPageAreaPieceTable(APageArea: TdxPageArea); override;
    function LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea; overload; override;
    function LookupPageArea(AFloatingObjects: TdxFloatingObjectBoxList; ARun: TdxFloatingObjectAnchorRun): TdxPageArea; reintroduce; overload; virtual;

    function GetUseLastAvailablePosition: Boolean; override;
    procedure SetUseLastAvailablePosition(const Value: Boolean); override;

    property AnchorPieceTable: TdxPieceTable read FAnchorPieceTable;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; ATextBoxContentType: TdxTextBoxContentType;
      ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer); reintroduce;

    property PreferredPageIndex: Integer read FPreferredPageIndex write FPreferredPageIndex;
  end;

  { TdxHeaderFooterDocumentLayoutPosition }

  TdxHeaderFooterDocumentLayoutPosition = class(TdxDocumentLayoutPosition)
  strict private
    FPreferredPageIndex: Integer;
    procedure SetPreferredPageIndex(const AValue: Integer);
  protected
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
    function LookupPage(APages: TdxPageCollection; AStartIndex: Integer): TdxPage; override;
    function LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea; override;
    function LookupPreferredPage(APages: TdxPageCollection): TdxPage; virtual;
    function IsPageMatch(APage: TdxPage): Boolean; virtual;
    function LookupPreferredPageBackward(APages: TdxPageCollection): TdxPage; virtual;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer); reintroduce;

    property PreferredPageIndex: Integer read FPreferredPageIndex write SetPreferredPageIndex;
  end;

implementation

uses
  SysUtils, Math,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Utils.Exceptions;

type

  { TdxTableCellViewInfoAndLogPositionComparable }

  TdxTableCellViewInfoAndLogPositionComparable = class(TcxIUnknownObject, IdxComparable<TdxTableCellViewInfo>)
  private
    FPieceTable: TdxPieceTable;
    FLogPosition: TdxDocumentLogPosition;
    function GetFirstPosition(ACell: TdxTableCellViewInfo): TdxDocumentLogPosition;
    function GetLastPosition(ACell: TdxTableCellViewInfo): TdxDocumentLogPosition;
  public
    constructor Create(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition);
    function CompareTo(const ACell: TdxTableCellViewInfo): Integer;

    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxTableViewInfoAndLogPositionComparable }

  TdxTableViewInfoAndLogPositionComparable = class(TcxIUnknownObject, IdxComparable<TdxTableViewInfo>)
  private
    FPieceTable: TdxPieceTable;
    FLogPosition: TdxDocumentLogPosition;
  public
    constructor Create(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition);
    function CompareTo(const ATable: TdxTableViewInfo): Integer;
    function GetFirstPosition(ATable: TdxTableViewInfo): TdxDocumentLogPosition;
    function GetLastPosition(ATable: TdxTableViewInfo): TdxDocumentLogPosition;

    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

{ TdxTableCellViewInfoAndLogPositionComparable }

constructor TdxTableCellViewInfoAndLogPositionComparable.Create(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FLogPosition := ALogPosition;
end;

function TdxTableCellViewInfoAndLogPositionComparable.CompareTo(const ACell: TdxTableCellViewInfo): Integer;
var
  AFirstPos, ALastPos: TdxDocumentLogPosition;
begin
  AFirstPos := GetFirstPosition(ACell);
  if FLogPosition < AFirstPos then
    Result := 1
  else
    if FLogPosition > AFirstPos then
    begin
      ALastPos := GetLastPosition(ACell);
      if FLogPosition <= ALastPos then
        Result := 0
      else
        Result := -1;
    end
    else
      Result := 0;
end;

function TdxTableCellViewInfoAndLogPositionComparable.GetFirstPosition(ACell: TdxTableCellViewInfo): TdxDocumentLogPosition;
begin
  Result := PieceTable.Paragraphs[ACell.Cell.StartParagraphIndex].LogPosition;
end;

function TdxTableCellViewInfoAndLogPositionComparable.GetLastPosition(ACell: TdxTableCellViewInfo): TdxDocumentLogPosition;
begin
  Result := PieceTable.Paragraphs[ACell.Cell.EndParagraphIndex].EndLogPosition;
end;

{ TdxTableViewInfoAndLogPositionComparable }

constructor TdxTableViewInfoAndLogPositionComparable.Create(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FLogPosition := ALogPosition;
end;

function TdxTableViewInfoAndLogPositionComparable.CompareTo(const ATable: TdxTableViewInfo): Integer;
var
  AFirstPos: TdxDocumentLogPosition;
begin
  AFirstPos := GetFirstPosition(ATable);
  if AFirstPos > LogPosition then
    Result := 1
  else
    Result := -1;
end;

function TdxTableViewInfoAndLogPositionComparable.GetFirstPosition(ATable: TdxTableViewInfo): TdxDocumentLogPosition;
begin
  Result := PieceTable.Paragraphs[ATable.Cells[0].Cell.StartParagraphIndex].LogPosition;
end;

function TdxTableViewInfoAndLogPositionComparable.GetLastPosition(ATable: TdxTableViewInfo): TdxDocumentLogPosition;
begin
  Result := PieceTable.Paragraphs[ATable.Cells[ATable.Cells.Count - 1].Cell.EndParagraphIndex].EndLogPosition;
end;

{ TdxDocumentLayoutPosition }

procedure TdxDocumentLayoutPosition.CheckPageAreaPieceTable(APageArea: TdxPageArea);
begin
  Assert(APageArea.PieceTable = PieceTable);
end;

constructor TdxDocumentLayoutPosition.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  FDetailsLevel := TdxDocumentLayoutDetailsLevel.None;
  Assert(ADocumentLayout <> nil);
  Assert(APieceTable <> nil);
  Assert(ADocumentLayout.DocumentModel = APieceTable.DocumentModel);
  FDocumentLayout := ADocumentLayout;
  FPieceTable := APieceTable;
  FLogPosition := ALogPosition;
  FSuppressSuspendFormatting := False;
end;

destructor TdxDocumentLayoutPosition.Destroy;
begin
  TableRow := nil;
  TableCell := nil;
  Row := nil;
  Character := nil;
  inherited Destroy;
end;

function TdxDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxDocumentLayoutPosition.Create(DocumentLayout, PieceTable, LogPosition);
end;

procedure TdxDocumentLayoutPosition.EnsureFormattingComplete;
begin
end;

procedure TdxDocumentLayoutPosition.EnsurePageSecondaryFormattingComplete(APage: TdxPage);
begin
end;

function TdxDocumentLayoutPosition.GetColumnIndex(AColumns: TdxColumnCollection; AStartIndex,
  AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxExactColumnAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    Result := FindBoxIndex(AColumns, AComparable, IsIntersectedWithPrevColumn, AStartIndex, AEndIndex);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetBoxIndex(ABoxes: TdxBoxCollection; AStartIndex, AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxAndLogPositionComparable;
begin
  AComparable := TdxBoxAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(ABoxes, AComparable, AStartIndex, AEndIndex, Result);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetBoxIndex(ABoxes: TdxBoxCollection): Integer;
begin
  Result := GetBoxIndex(ABoxes, 0, ABoxes.Count - 1);
end;

function TdxDocumentLayoutPosition.GetCharIndex(ACharacters: TdxCharacterBoxCollection; AStartIndex, AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxBoxAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(ACharacters, AComparable, AStartIndex, AEndIndex, Result);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetCharIndex(ACharacters: TdxCharacterBoxCollection): Integer;
begin
  Result := GetCharIndex(ACharacters, 0, ACharacters.Count - 1);
end;

function TdxDocumentLayoutPosition.GetColumnIndex(AColumns: TdxColumnCollection): Integer;
begin
  Result := GetColumnIndex(AColumns, 0, AColumns.Count - 1);
end;

function TdxDocumentLayoutPosition.FindBoxIndex(ACollection: TdxBoxList;
  AExactComparable: TdxBoxComparable;
  const AIsIntersect: TdxIsIntersectedWithPrevBoxFunc;
  AStartIndex, AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxBoxStartAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    Result := FindBoxIndex(ACollection, AComparable, AExactComparable, AIsIntersect, AStartIndex, AEndIndex);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.FindBoxIndex(ACollection: TdxBoxList;
  AComparable, AExactComparable: TdxBoxComparable;
  const AIsIntersect: TdxIsIntersectedWithPrevBoxFunc;
  AStartIndex, AEndIndex: Integer): Integer;
var
  ABoxIndex: Integer;
  ALastBoxIndex: Integer;
  AStartBoxIndex: Integer;
begin
  if TdxAlgorithms1<TdxBoxBase>.BinarySearch(ACollection, AComparable, AStartIndex, AEndIndex, ABoxIndex) then
    Exit(ABoxIndex);

  ALastBoxIndex := ABoxIndex - 1;
  if ALastBoxIndex < 0 then
  begin
    if ACollection.Count > 0 then
      Exit(0)
    else
      Exit(-1);
  end;

  AStartBoxIndex := ALastBoxIndex;
  while (AStartBoxIndex > 0) and AIsIntersect(ACollection[AStartBoxIndex]) do
    Dec(AStartBoxIndex);
  if TdxAlgorithms1<TdxBoxBase>.BinarySearch(ACollection, AExactComparable, AStartBoxIndex, ALastBoxIndex, ABoxIndex) then
    Exit(ABoxIndex);

  if ABoxIndex < ACollection.Count then
    Exit(ABoxIndex);

  if UseLastAvailablePosition then
    Result := ACollection.Count - 1
  else
    Result := -1;
end;

function TdxDocumentLayoutPosition.BinarySearchTableCell(ATableViewInfoCollection: TdxTableViewInfoCollection): TdxTableCellViewInfo;
var
  ATableViewInfo: TdxTableViewInfo;
  ACells: TdxTableCellViewInfoCollection;
  AStartIndex, AEndIndex, ACellIndex: Integer;
  AComparable1, AComparable2: TdxTableCellViewInfoAndLogPositionComparable;
  AParagraph: TdxParagraph;
  ACell, AFirstCell: TdxTableCell;
begin
  ATableViewInfo := GetTableViewInfo(ATableViewInfoCollection);
  if ATableViewInfo = nil then
    Exit(nil);

  ACells := ATableViewInfo.Cells;
  AComparable1 := TdxTableCellViewInfoAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    AStartIndex := 0;
    AEndIndex := ACells.Count - 1;
    if not TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ACells, AComparable1, AStartIndex, AEndIndex, ACellIndex) then
    begin
      AParagraph := PieceTable.FindParagraph(LogPosition);
      if AParagraph = nil then
        TdxRichEditExceptions.ThrowInternalException;

      ACell := AParagraph.GetCell;
      if ACell = nil then
        TdxRichEditExceptions.ThrowInternalException;

      AFirstCell := ACell.Table.GetFirstCellInVerticalMergingGroup(ACell);
      AComparable2 := TdxTableCellViewInfoAndLogPositionComparable.Create(PieceTable, PieceTable.Paragraphs[AFirstCell.StartParagraphIndex].LogPosition);
      try
        if not TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ACells, AComparable2, AStartIndex, AEndIndex, ACellIndex) then
          Exit(nil);
      finally
        AComparable2.Free;
      end;
    end;
    Result := ACells[ACellIndex];
  finally
    AComparable1.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetTableViewInfo(ATableViewInfoCollection: TdxTableViewInfoCollection): TdxTableViewInfo;
var
  ATableIndex: Integer;
  ATableViewInfo: TdxTableViewInfo;
  ATableViewInfoAndLogPositionComparable: TdxTableViewInfoAndLogPositionComparable;
begin
  ATableViewInfoAndLogPositionComparable := TdxTableViewInfoAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    for ATableIndex := ATableViewInfoCollection.Count - 1 downto 0 do
    begin
      ATableViewInfo := ATableViewInfoCollection[ATableIndex];
      if ATableViewInfo.Cells.Count = 0 then
        Continue;
      if (ATableViewInfoAndLogPositionComparable.GetFirstPosition(ATableViewInfo) <= LogPosition) and
        (ATableViewInfoAndLogPositionComparable.GetLastPosition(ATableViewInfo) >= LogPosition) then
        Exit(ATableViewInfo);
    end;
  finally
    ATableViewInfoAndLogPositionComparable.Free;
  end;
  Result := nil;
end;

function TdxDocumentLayoutPosition.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(DocumentLayout.DocumentModel);
end;

function TdxDocumentLayoutPosition.IsTableCellVerticalMerged: Boolean;
begin
  Result := TableCell.Cell.VerticalMerging = TdxMergingState.Restart;
end;

function TdxDocumentLayoutPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := FLogPosition;
end;

function TdxDocumentLayoutPosition.GetPageAreaIndex(AAreas: TdxPageAreaCollection; AStartIndex,
  AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxExactPageAreaAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    Result := FindBoxIndex(AAreas,
      AComparable, IsIntersectedWithPrevArea,
      AStartIndex, AEndIndex);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetPageAreaIndex(AAreas: TdxPageAreaCollection): Integer;
begin
  Result := GetPageAreaIndex(AAreas, 0, AAreas.Count - 1);
end;

function TdxDocumentLayoutPosition.GetPageIndex(APages: TdxPageCollection): Integer;
begin
  Result := GetPageIndex(APages, 0, APages.Count - 1);
end;

function TdxDocumentLayoutPosition.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FPieceTable);
end;

function TdxDocumentLayoutPosition.GetRowIndex(ARows: TdxRowCollection): Integer;
begin
  Result := GetRowIndex(ARows, 0, ARows.Count - 1);
end;

function TdxDocumentLayoutPosition.GetRowIndex(ARows: TdxRowCollection; AStartIndex, AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxBoxAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(ARows, AComparable, AStartIndex, AEndIndex, Result);
  finally
    AComparable.Free;
  end;

  if (Result >= ARows.Count) and (UseLastAvailablePosition or ((TableCell <> nil) and IsTableCellVerticalMerged)) then
      Result := ARows.Count - 1;
end;

function TdxDocumentLayoutPosition.GetPageIndex(APages: TdxPageCollection; AStartIndex, AEndIndex: Integer): Integer;
var
  AComparable: TdxBoxComparable;
begin
  AComparable := TdxExactPageAndLogPositionComparable.Create(PieceTable, LogPosition);
  try
    Result := FindBoxIndex(APages, AComparable, IsIntersectedWithPrevPage, AStartIndex, AEndIndex);
  finally
    AComparable.Free;
  end;
end;

function TdxDocumentLayoutPosition.GetUseLastAvailablePosition: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentLayoutPosition.IncreaseDetailsLevel(ADetailsLevel: TdxDocumentLayoutDetailsLevel);
begin
  if FDetailsLevel < ADetailsLevel then
    FDetailsLevel := ADetailsLevel;
end;

procedure TdxDocumentLayoutPosition.Invalidate;
begin
  FDetailsLevel := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxDocumentLayoutPosition.IsIntersectedWithPrevArea(AArea: TdxBoxBase): Boolean;
begin
  Assert(AArea is TdxPageArea);
  Result := IsIntersectedWithPrevColumn(TdxPageArea(AArea).Columns.First);
end;

function TdxDocumentLayoutPosition.IsIntersectedWithPrevColumn(AColumn: TdxBoxBase): Boolean;
begin
  Assert(AColumn is TdxColumn);
  Result := TdxColumn(AColumn).IsIntersectedWithPrevColumn;
end;

function TdxDocumentLayoutPosition.IsIntersectedWithPrevPage(APage: TdxBoxBase): Boolean;
begin
  Assert(APage is TdxPage);
  Result := IsIntersectedWithPrevArea(TdxPage(APage).Areas.First);
end;

function TdxDocumentLayoutPosition.IsValid(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := ADetailsLevel <= DetailsLevel;
end;

function TdxDocumentLayoutPosition.LookupPage(APages: TdxPageCollection; AStartIndex: Integer): TdxPage;
var
  APageIndex: Integer;
begin
  APageIndex := GetPageIndex(APages, AStartIndex, APages.Count - 1);
  if APageIndex >= 0 then
    Result := APages[APageIndex]
  else
    Result :=  nil;
end;

function TdxDocumentLayoutPosition.LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea;
var
  Areas: TdxPageAreaCollection;
  APageAreaIndex: Integer;
begin
  if (APage.Header <> nil) and (APage.Header.PieceTable = PieceTable) then
    Exit(APage.Header);
  if (APage.Footer <> nil) and (APage.Footer.PieceTable = PieceTable) then
    Exit(APage.Footer);

  Areas := APage.Areas;
  APageAreaIndex := GetPageAreaIndex(Areas, AStartIndex, Areas.Count - 1);
  if APageAreaIndex >= 0 then
    Result := Areas[APageAreaIndex]
  else
    Result := nil;
end;

procedure TdxDocumentLayoutPosition.SetCharacter(const Value: TdxCharacterBox);
begin
  if Character <> Value then
  begin
    TdxCharacterBox.Release(FCharacter);
    FCharacter := Value;
    TdxCharacterBox.AddReference(FCharacter);
  end;
end;

procedure TdxDocumentLayoutPosition.SetLogPosition(AValue: TdxDocumentLogPosition);
begin
  FLogPosition := AValue;
end;

procedure TdxDocumentLayoutPosition.SetRow(const Value: TdxRow);
begin
  if FRow <> Value then
  begin
    TdxRow.Release(FRow);
    FRow := Value;
    TdxRow.AddReference(FRow);
  end;
end;

procedure TdxDocumentLayoutPosition.SetTableCell(const Value: TdxTableCellViewInfo);
begin
  if FTableCell <> Value then
  begin
    TdxTableCellViewInfo.Release(FTableCell);
    FTableCell := Value;
    TdxTableCellViewInfo.AddReference(FTableCell);
  end;
end;

procedure TdxDocumentLayoutPosition.SetTableRow(const Value: TdxTableRowViewInfoBase);

  procedure LockTableViewInfo;
  var
    ATableViewInfo: TdxTableViewInfo;
  begin
    if FTableRow = nil then
      ATableViewInfo := nil
    else
      ATableViewInfo := FTableRow.TableViewInfo;
    TdxTableViewInfo.AddReference(ATableViewInfo);
  end;

  procedure UnlockTableViewInfo;
  var
    ATableViewInfo: TdxTableViewInfo;
  begin
    if FTableRow = nil then
      ATableViewInfo := nil
    else
      ATableViewInfo := FTableRow.TableViewInfo;
    TdxTableViewInfo.Release(ATableViewInfo);
  end;

begin
  if FTableRow <> Value then
  begin
    UnlockTableViewInfo;
    TdxTableRowViewInfoBase.Release(FTableRow);
    FTableRow := Value;
    TdxTableRowViewInfoBase.AddReference(FTableRow);
    LockTableViewInfo;
  end;
end;

procedure TdxDocumentLayoutPosition.SetUseLastAvailablePosition(const Value: Boolean);
begin
end;

function TdxDocumentLayoutPosition.Update(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel;
  AUseLastAvailablePosition: Boolean): Boolean;
begin
  UseLastAvailablePosition := AUseLastAvailablePosition;
  if not IsValid(ADetailsLevel) then
  begin
    FDetailsLevel := UpdateCore(APages, ADetailsLevel);
    Result := IsValid(ADetailsLevel);
  end
  else
    Result := True;
end;

function TdxDocumentLayoutPosition.Update(APages: TdxPageCollection;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := Update(APages, ADetailsLevel, False);
end;

function TdxDocumentLayoutPosition.UpdateBoxRecursive(ARow: TdxRow; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  ABoxIndex: Integer;
  ABoxes: TdxBoxCollection;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Box) then
  begin
    ABoxes := ARow.Boxes;
    ABoxIndex := GetBoxIndex(ABoxes, AStartIndex, ABoxes.Count - 1);
    if ABoxIndex >= ABoxes.Count then
    begin
      if UseLastAvailablePosition then
        ABoxIndex := ABoxes.Count - 1
      else
        Exit(TdxDocumentLayoutDetailsLevel.Row);
    end;
    FBox := ABoxes[ABoxIndex];
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.Box then
    Result := TdxDocumentLayoutDetailsLevel.Box
  else
    Result := UpdateCharacterBoxRecursive(ARow, 0, ADetailsLevel);
end;

function TdxDocumentLayoutPosition.UpdateCellRecursive(AColumn: TdxColumn;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  ARows: TdxRowCollection;
  ACell: TdxTableCellViewInfo;
  AColumnTables: TdxTableViewInfoCollection;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.TableCell) then
  begin
    AColumnTables := AColumn.InnerTables;
    if AColumnTables <> nil then
    begin
      ACell := BinarySearchTableCell(AColumnTables);
      TableCell := ACell;
      if ACell <> nil then
        Assert(ACell.TableViewInfo.Table.PieceTable = PieceTable);
    end
    else
      TableCell := nil;
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.TableCell then
    Exit(TdxDocumentLayoutDetailsLevel.TableCell);
  if TableCell <> nil then
    ARows := TableCell.GetRows(AColumn)
  else
    ARows := AColumn.Rows;
  try
    Result := UpdateRowRecursive(ARows, 0, ADetailsLevel);
  finally
    if ARows <> AColumn.Rows then
      ARows.Free;
  end;
end;

function TdxDocumentLayoutPosition.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateEmptyClone;
  Result.CopyFrom(Self);
end;

procedure TdxDocumentLayoutPosition.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxDocumentLayoutPosition absolute Source;
begin
  FDetailsLevel := ASource.DetailsLevel;
  FLogPosition := ASource.LogPosition;
  Page := ASource.Page;
  PageArea := ASource.PageArea;
  Column := ASource.Column;
  Row := ASource.Row;
  Box := ASource.Box;
  Character := ASource.Character;
  TableRow := ASource.TableRow;
  TableCell := ASource.TableCell;
  LeftOffset := ASource.LeftOffset;
  RightOffset := ASource.RightOffset;
end;

procedure TdxDocumentLayoutPosition.CopyFrom(AValue: TdxDocumentLayoutPosition;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel);
begin
  FDetailsLevel := ADetailsLevel;
  if IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    Page := AValue.Page;
  if IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    PageArea := AValue.PageArea;
  if IsValid(TdxDocumentLayoutDetailsLevel.Column) then
    Column := AValue.Column;
  if IsValid(TdxDocumentLayoutDetailsLevel.TableCell) then
    TableCell := AValue.TableCell;
  if IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    Row := AValue.Row;
  if IsValid(TdxDocumentLayoutDetailsLevel.Box) then
    Box := AValue.Box;
  if IsValid(TdxDocumentLayoutDetailsLevel.Character) then
    Character := AValue.Character;
end;

function TdxDocumentLayoutPosition.UpdateCharacterBoxRecursive(ARow: TdxRow; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  ACharIndex: Integer;
  ADetailRow: TdxDetailRow;
  ACharacters: TdxCharacterBoxCollection;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Character) then
  begin
    ADetailRow := DocumentLayout.CreateDetailRowForBox(ARow, Box, SuppressSuspendFormatting);
    try
      ACharacters := ADetailRow.Characters;
      ACharIndex := GetCharIndex(ACharacters, AStartIndex, ACharacters.Count - 1);
      if ACharIndex >= ADetailRow.Characters.Count then
      begin
        if UseLastAvailablePosition or ((TableCell <> nil) and IsTableCellVerticalMerged) then
          ACharIndex := ADetailRow.Characters.Count - 1
        else
          Exit(TdxDocumentLayoutDetailsLevel.Box);
      end;
      Character := ADetailRow.Characters[ACharIndex];
    finally
      ADetailRow.Free;
    end;
  end;
  Result := TdxDocumentLayoutDetailsLevel.Character;
end;

function TdxDocumentLayoutPosition.UpdateColumnRecursive(AColumns: TdxColumnCollection; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  AColumnIndex: Integer;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Column) then
  begin
    AColumnIndex := GetColumnIndex(AColumns, AStartIndex, AColumns.Count - 1);
    if AColumnIndex < 0 then
      Exit(TdxDocumentLayoutDetailsLevel.PageArea);
    FColumn := AColumns[AColumnIndex];
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.Column then
    Result := TdxDocumentLayoutDetailsLevel.Column
  else
    Result := UpdateCellRecursive(FColumn, ADetailsLevel);
end;

function TdxDocumentLayoutPosition.UpdateCore(APages: TdxPageCollection;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
begin
  if ADetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(TdxDocumentLayoutDetailsLevel.None);
  EnsureFormattingComplete;
  Result := UpdatePageRecursive(APages, 0, ADetailsLevel);
end;

function TdxDocumentLayoutPosition.UpdatePageAreaRecursive(APage: TdxPage; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
  begin
    FPageArea := LookupPageArea(APage, AStartIndex);
    if FPageArea = nil then
      Exit(TdxDocumentLayoutDetailsLevel.Page);
    CheckPageAreaPieceTable(FPageArea);
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.PageArea then
    Result := TdxDocumentLayoutDetailsLevel.PageArea
  else
    Result := UpdateColumnRecursive(FPageArea.Columns, 0, ADetailsLevel);
end;

function TdxDocumentLayoutPosition.UpdatePageRecursive(APages: TdxPageCollection; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  APage: TdxPage;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Page) then
  begin
    APage := LookupPage(APages, AStartIndex);
    if APage = nil then
      Exit(TdxDocumentLayoutDetailsLevel.None)
    else
      FPage := APage;
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.Page then
    Result := TdxDocumentLayoutDetailsLevel.Page
  else
    Result := UpdatePageAreaRecursive(FPage, 0, ADetailsLevel);
end;

function TdxDocumentLayoutPosition.UpdateRowRecursive(ARows: TdxRowCollection; AStartIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  ARowIndex: Integer;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Row) then
  begin
    EnsurePageSecondaryFormattingComplete(Page);
    ARowIndex := GetRowIndex(ARows, AStartIndex, ARows.Count - 1);
    if (ARowIndex < 0) or (ARowIndex >= ARows.Count) then
      Exit(TdxDocumentLayoutDetailsLevel.Column);
    Row := ARows[ARowIndex];
    Assert(FRow.Paragraph.PieceTable = PieceTable);
  end;
  if ADetailsLevel <= TdxDocumentLayoutDetailsLevel.Row then
    Result := TdxDocumentLayoutDetailsLevel.Row
  else
    Result := UpdateBoxRecursive(FRow, 0, ADetailsLevel);
end;

{ TdxTextBoxDocumentLayoutPosition }

procedure TdxTextBoxDocumentLayoutPosition.CheckPageAreaPieceTable(APageArea: TdxPageArea);
begin
end;

constructor TdxTextBoxDocumentLayoutPosition.Create(ADocumentLayout: TdxDocumentLayout;
  ATextBoxContentType: TdxTextBoxContentType; ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
begin
  inherited Create(ADocumentLayout, TdxPieceTable(ATextBoxContentType.PieceTable), ALogPosition);
  FPreferredPageIndex := APreferredPageIndex;
end;

function TdxTextBoxDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxTextBoxDocumentLayoutPosition.Create(DocumentLayout, TdxTextBoxContentType(PieceTable.ContentType),
    LogPosition, FPreferredPageIndex);
end;

function TdxTextBoxDocumentLayoutPosition.IsIntersectedWithPrevBox(ABox: TdxBoxBase): Boolean;
begin
  Result := TdxPage(ABox).Areas.First.Columns.First.IsIntersectedWithPrevColumn;
end;

function TdxTextBoxDocumentLayoutPosition.GetPageIndex(APages: TdxPageCollection; AStartIndex,
  AEndIndex: Integer): Integer;
var
  APieceTable: TdxPieceTable;
  AAnchorLogPosition: TdxDocumentLogPosition;
  ATextBoxPieceTable: TdxTextBoxContentType;
  AExactPageAndLogPositionComparable: TdxExactPageAndLogPositionComparable;
  ABoxStartAndLogPositionComparable: TdxBoxStartAndLogPositionComparable;
begin
  if LogPosition > PieceTable.DocumentEndLogPosition then
    Exit(-1);

  ATextBoxPieceTable := TdxTextBoxContentType(PieceTable.ContentType);
  APieceTable := TdxPieceTable(ATextBoxPieceTable.AnchorRun.PieceTable);
  if APieceTable.IsHeaderFooter then
    Exit(FPreferredPageIndex);

  FAnchorPieceTable := APieceTable;
  try
    AAnchorLogPosition := FAnchorPieceTable.GetRunLogPosition(ATextBoxPieceTable.AnchorRun);
    ABoxStartAndLogPositionComparable := TdxBoxStartAndLogPositionComparable.Create(FAnchorPieceTable, AAnchorLogPosition);
    try
      AExactPageAndLogPositionComparable := TdxExactPageAndLogPositionComparable.Create(FAnchorPieceTable, AAnchorLogPosition);
      try
        Result := FindBoxIndex(APages, ABoxStartAndLogPositionComparable, AExactPageAndLogPositionComparable,
          IsIntersectedWithPrevBox, AStartIndex, AEndIndex);
      finally
        AExactPageAndLogPositionComparable.Free;
      end;
    finally
      ABoxStartAndLogPositionComparable.Free;
    end;
  finally
    FAnchorPieceTable := nil;
  end;
end;

function TdxTextBoxDocumentLayoutPosition.GetPieceTable: TdxPieceTable;
begin
  if FAnchorPieceTable <> nil then
    Result :=  FAnchorPieceTable
  else
    Result :=  inherited GetPieceTable;
end;

function TdxTextBoxDocumentLayoutPosition.GetUseLastAvailablePosition: Boolean;
begin
  Result := FUseLastAvailablePosition;
end;

function TdxTextBoxDocumentLayoutPosition.LookupPageArea(AFloatingObjects: TdxFloatingObjectBoxList;
  ARun: TdxFloatingObjectAnchorRun): TdxPageArea;
var
  I: Integer;
begin
  if AFloatingObjects = nil then
    Exit(nil);
  for I := 0 to AFloatingObjects.Count - 1 do
    if AFloatingObjects[I].GetFloatingObjectRun = ARun then
      Exit(AFloatingObjects[I].DocumentLayout.Pages.First.Areas.First);
  Result := nil;
end;

procedure TdxTextBoxDocumentLayoutPosition.SetUseLastAvailablePosition(const Value: Boolean);
begin
  FUseLastAvailablePosition := Value;
end;

function TdxTextBoxDocumentLayoutPosition.LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea;
var
  AAnchorRun: TdxFloatingObjectAnchorRun;
  ATextBoxPieceTable: TdxTextBoxContentType;
begin
  ATextBoxPieceTable := TdxTextBoxContentType(PieceTable.ContentType);
  AAnchorRun := ATextBoxPieceTable.AnchorRun;

  Result := LookupPageArea(APage.InnerFloatingObjects, AAnchorRun);
  if Result <> nil then
    Exit;
  Result := LookupPageArea(APage.InnerForegroundFloatingObjects, AAnchorRun);
  if Result <> nil then
    Exit;
  Result := LookupPageArea(APage.InnerBackgroundFloatingObjects, AAnchorRun);
end;

{ TdxHeaderFooterDocumentLayoutPosition }

constructor TdxHeaderFooterDocumentLayoutPosition.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition; APreferredPageIndex: Integer);
begin
  inherited Create(ADocumentLayout, APieceTable, ALogPosition);
  Assert(APreferredPageIndex >= 0);
  FPreferredPageIndex := APreferredPageIndex;
end;

procedure TdxHeaderFooterDocumentLayoutPosition.SetPreferredPageIndex(const AValue: Integer);
begin
  Assert(AValue >= 0);
  FPreferredPageIndex := AValue;
end;

function TdxHeaderFooterDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterDocumentLayoutPosition.Create(DocumentLayout, PieceTable, LogPosition, PreferredPageIndex);
end;

function TdxHeaderFooterDocumentLayoutPosition.LookupPage(APages: TdxPageCollection; AStartIndex: Integer): TdxPage;
begin
  Result := LookupPreferredPage(APages);
end;

function TdxHeaderFooterDocumentLayoutPosition.LookupPageArea(APage: TdxPage; AStartIndex: Integer): TdxPageArea;
begin
  if (APage.Header <> nil) and (APage.Header.PieceTable = PieceTable) then
    Exit(APage.Header);
  if (APage.Footer <> nil) and (APage.Footer.PieceTable = PieceTable) then
    Exit(APage.Footer);
  Result := nil;
end;

function TdxHeaderFooterDocumentLayoutPosition.LookupPreferredPage(APages: TdxPageCollection): TdxPage;
var
  APage: TdxPage;
begin
  if LogPosition > PieceTable.DocumentEndLogPosition then
    Exit(nil);

  if PreferredPageIndex >= APages.Count then
    Exit(LookupPreferredPageBackward(APages));

  APage := APages[PreferredPageIndex];
  if IsPageMatch(APage) then
    Exit(APage);

  Result := LookupPreferredPageBackward(APages);
end;

function TdxHeaderFooterDocumentLayoutPosition.IsPageMatch(APage: TdxPage): Boolean;
begin
  if (APage.Header <> nil) and (APage.Header.PieceTable = PieceTable) then
    Exit(True);
  if (APage.Footer <> nil) and (APage.Footer.PieceTable = PieceTable) then
    Exit(True);
  Result := False;
end;

function TdxHeaderFooterDocumentLayoutPosition.LookupPreferredPageBackward(APages: TdxPageCollection): TdxPage;
var
  I: Integer;
begin
  for I := APages.Count - 1 downto 0 do
  begin
    if IsPageMatch(APages[I]) then
      Exit(APages[I]);
  end;
  Result := nil;
end;

end.
