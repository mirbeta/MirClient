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

unit dxRichEdit.DocumentModel.Exporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, ZLIB,
  Generics.Defaults, Generics.Collections, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Export.Core,
  dxRichEdit.Utils.Types,
  dxEncoding,
  dxGenerics,
  dxRichEdit.Utils.OfficeImage,
  dxXMLWriter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentFormats.DocumentFormatUtils;

type
  TdxDocumentModelExporter = class;

  { TdxFootNoteExportInfo }

  TdxFootNoteExportInfo = record
  strict private
    FNumberText: string;
    FNumber: Integer;
    FNote: TdxPieceTable;
    FId: Integer;
  public
    constructor Create(ANote: TdxPieceTable; ANumber: Integer; const ANumberText: string);

    property NumberText: string read FNumberText write FNumberText;
    property Number: Integer read FNumber;
    property Note: TdxPieceTable read FNote;
    property Id: Integer read FId write FId;
  end;

  { TdxPieceTableNumberingListCountersManager }

  TdxPieceTableNumberingListCountersManager = class
  private
    FCalculators: TObjectDictionary<TdxAbstractNumberingList, TdxNumberingListCountersCalculator>;
  public
    procedure BeginCalculateCounters;
    procedure EndCalculateCounters;
    function CalculateNextCounters(ACurrentParagraph: TdxParagraph): TIntegerDynArray;
  end;

  { TdxDocumentModelExporter }

  TdxExportPieceTableDelegate = procedure of object;

  TdxDocumentModelExporter = class abstract(TdxCustomDocumentModelExporter,
    IdxSimpleDocumentModelExporter,
    IdxDocumentModelExporter)
  strict private
    FCurrentSection: TdxSection;
    FFieldLevel: Integer;
    FFootNoteNumber: Integer;
    FEndNoteNumber: Integer;
    FFootNoteExportInfos: TList<TdxFootNoteExportInfo>;
    FEndNoteExportInfos: TList<TdxFootNoteExportInfo>;
    FExportedParagraphCount: Integer;
    FPieceTable: TdxPieceTable;
    FPieceTableNumberingListCounters: TdxPieceTableNumberingListCountersManager;
    FTableBackgroundColorStack: TStack<TdxAlphaColor>;
    FVisitableDocumentIntervalsIteratorStack: TdxObjectStack<TdxVisitableDocumentIntervalBoundaryIterator>;
    function GetDocumentModel: TdxDocumentModel;
    function GetVisitableDocumentIntervalsIterator: TdxVisitableDocumentIntervalBoundaryIterator;
    function GetPieceTableNumberingListCounters: TdxPieceTableNumberingListCountersManager;
  protected
    procedure ExportCell(ACell: TdxTableCell; ATableInfo: TdxTableInfo); virtual;
    procedure ExportDocument; virtual;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); virtual;
    procedure ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); virtual;
    procedure ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); virtual;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); virtual;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); virtual;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); virtual;
    procedure ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); virtual;
    procedure ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); virtual;
    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); virtual;
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); virtual;
    procedure ExportImageReference(ARun: TdxFloatingObjectAnchorRun); overload; virtual;
    procedure ExportImageReference(ARun: TdxInlinePictureRun); overload; virtual;
    procedure ExportInlineObjectRun(ARun: TdxInlineObjectRun); virtual;
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); virtual;
    procedure ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); virtual;
    procedure ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); virtual;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; virtual;
    function ExportParagraphFiltered(AParagraph: TdxParagraph): TdxParagraphIndex;
    procedure ExportParagraphRun(ARun: TdxParagraphRun); virtual;
    procedure ExportParagraphRuns(AParagraph: TdxParagraph);
    procedure ExportParagraphs(AFrom, ATo: TdxParagraphIndex); virtual;
    procedure ExportPieceTable; virtual;
    function ExportRootTable(ATable: TdxTable): TdxParagraphIndex;
    procedure ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo); virtual;
    procedure ExportRun(I: TdxRunIndex); virtual;
    procedure ExportSection(const ASection: TdxSection); virtual;
    procedure ExportSectionFiltered(const ASection: TdxSection);
    procedure ExportSectionHeadersFooters(ASection: TdxSection); virtual;
    procedure ExportSectionHeadersFootersCore(ASection: TdxSection); virtual;
    procedure ExportSectionRun(ARun: TdxSectionRun); virtual;
    procedure ExportSeparatorTextRun(ARun: TdxSeparatorTextRun); virtual;
    function ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex; virtual;
    procedure ExportTextRun(ARun: TdxTextRun); virtual;
    function CreateVisitableDocumentIntervalBoundaryIterator: TdxVisitableDocumentIntervalBasedObjectBoundaryIterator; virtual;
    function GetAllowSkipParagraphInCell: Boolean; virtual;
    function GetNumberingListText(AParagraph: TdxParagraph): string; virtual;

    procedure PullVisitableDocumentIntervalBoundaries;
    function ShouldExportRun(ARun: TdxTextRunBase): Boolean;
    function ShouldExportHiddenText: Boolean; virtual;
    function ShouldExportSection(ASection: TdxSection): Boolean;
    function ShouldSplitRuns: Boolean; virtual;
    function ShouldCalculateFootNoteAndEndNoteNumbers: Boolean;
    function ShouldExportParagraph(AParagraph: TdxParagraph): Boolean;
    function ShouldUseCustomSaveTableMethod: Boolean; virtual;
    function ShouldExportInlinePicture(ARun: TdxInlinePictureRun): Boolean; virtual;
    procedure SplitRunsByBookmarkBoundaries;
    procedure SplitRunByModelPosition(const APos: TdxDocumentModelPosition);
    function CalculateTotalParagraphCount: Integer; virtual;
    function CalculateSectionParagraphCount(ASection: TdxSection): Integer; virtual;
    function CalculatePieceTableParagraphCount(AHeaderFooter: TdxSectionHeaderFooterBase): Integer; virtual;
    procedure CalculateFootNoteNumber(ASection: TdxSection); virtual;
    procedure CalculateEndNoteNumber(ASection: TdxSection); virtual;
    function CalculateFootNoteNumberCore(AProperties: TdxSectionFootNote; AValue: Integer): Integer; virtual;
    function CreateFootNoteExportInfo(ANote: TdxFootNoteRun): TdxFootNoteExportInfo; overload; virtual;
    function FindFootNoteExportInfoByNote(AList: TList<TdxFootNoteExportInfo>; APieceTable: TdxPieceTable; out AInfo: TdxFootNoteExportInfo): Boolean; virtual;
    function CreateFootNoteExportInfo(ANote: TdxEndNoteRun): TdxFootNoteExportInfo; overload; virtual;

    procedure TryToExportBookmarks(ARunIndex: TdxRunIndex; ARunOffset: Integer); virtual;

    procedure InvalidateParagraphsBoxes;
    function PrepareModelForExport(ADocumentModel: TdxCustomDocumentModel): TdxCustomDocumentModel;
    procedure PerformExportPieceTable(APieceTable: TdxPieceTable; APieceTableExporter: TdxExportPieceTableDelegate);
    procedure SplitRuns;
    procedure PopVisitableDocumentIntervalBoundaryIterator; virtual;
    procedure PushVisitableDocumentIntervalBoundaryIterator; virtual;

    function CurrentTableBackgroundColor: TdxAlphaColor;
    procedure PopCurrentTableBackgroundColor;
    procedure PushCurrentTableBackgroundColor(AColor: TdxAlphaColor);

    // IdxDocumentModelExporter
    procedure Export(ARun: TdxTextRunBase); overload;
    procedure ExportBookmarkEnd(ABookmark: TdxBookmark); virtual;
    procedure ExportBookmarkStart(ABookmark: TdxBookmark); virtual;
    procedure ExportRangePermissionStart(ARangePermission: TdxRangePermission); virtual;
    procedure ExportRangePermissionEnd(ARangePermission: TdxRangePermission); virtual;

    property AllowSkipParagraphInCell: Boolean read GetAllowSkipParagraphInCell;
    property FootNoteExportInfos: TList<TdxFootNoteExportInfo> read FFootNoteExportInfos;
    property EndNoteExportInfos: TList<TdxFootNoteExportInfo> read FEndNoteExportInfos;
    property ExportedParagraphCount: Integer read FExportedParagraphCount write FExportedParagraphCount;
    property FieldLevel: Integer read FFieldLevel;
    property PieceTableNumberingListCounters: TdxPieceTableNumberingListCountersManager read GetPieceTableNumberingListCounters;
    property VisitableDocumentIntervalsIterator: TdxVisitableDocumentIntervalBoundaryIterator read GetVisitableDocumentIntervalsIterator;
    property VisitableDocumentIntervalsIteratorStack: TdxObjectStack<TdxVisitableDocumentIntervalBoundaryIterator> read FVisitableDocumentIntervalsIteratorStack;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); override;
    destructor Destroy; override;

    procedure Export; overload; virtual;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

implementation

uses
  Contnrs, dxCore,
  dxRichEdit.Strs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.NumberingFormatting;

{ TdxFootNoteExportInfo }

constructor TdxFootNoteExportInfo.Create(ANote: TdxPieceTable; ANumber: Integer; const ANumberText: string);
begin
  FId := 0;
  FNote := ANote;
  FNumber := ANumber;
  FNumberText := ANumberText;
end;

{ TdxDocumentModelExporter }

constructor TdxDocumentModelExporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions);
var
  AExportDocumentModel: TdxCustomDocumentModel;
begin
  AExportDocumentModel := PrepareModelForExport(ADocumentModel);
  inherited Create(AExportDocumentModel, AOptions);
  FPieceTable := DocumentModel.MainPieceTable;
  FVisitableDocumentIntervalsIteratorStack := TdxObjectStack<TdxVisitableDocumentIntervalBoundaryIterator>.Create(False);
  FTableBackgroundColorStack := TStack<TdxAlphaColor>.Create;
  FFootNoteExportInfos := TList<TdxFootNoteExportInfo>.Create;
  FEndNoteExportInfos := TList<TdxFootNoteExportInfo>.Create;
end;

destructor TdxDocumentModelExporter.Destroy;
begin
  FreeAndNil(FFootNoteExportInfos);
  FreeAndNil(FEndNoteExportInfos);
  FreeAndNil(FTableBackgroundColorStack);
  FreeAndNil(FPieceTableNumberingListCounters);
  FreeAndNil(FVisitableDocumentIntervalsIteratorStack);
  inherited Destroy;
end;

function TdxDocumentModelExporter.CurrentTableBackgroundColor: TdxAlphaColor;
begin
  if FTableBackgroundColorStack.Count <= 0 then
    Result := TdxAlphaColors.Empty
  else
    Result := FTableBackgroundColorStack.Peek;
end;

procedure TdxDocumentModelExporter.Export(ARun: TdxTextRunBase);
begin
  if ARun.InheritsFrom(TdxFieldCodeStartRun) then
    ExportFieldCodeStartRun(TdxFieldCodeStartRun(ARun))
  else
  if ARun.InheritsFrom(TdxFieldCodeEndRun) then
    ExportFieldCodeEndRun(TdxFieldCodeEndRun(ARun))
  else
  if ARun.InheritsFrom(TdxFieldResultEndRun) then
    ExportFieldResultEndRun(TdxFieldResultEndRun(ARun))
  else
  if ARun.InheritsFrom(TdxFootNoteRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportFootNoteRun(TdxFootNoteRun(ARun));
  end
  else
  if ARun.InheritsFrom(TdxEndNoteRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportEndNoteRun(TdxEndNoteRun(ARun));
  end
  else
  if ARun.InheritsFrom(TdxTextRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportTextRun(TdxTextRun(ARun));
  end
  else
  if ARun.InheritsFrom(TdxSectionRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportSectionRun(TdxSectionRun(ARun))
  end
  else
  if ARun.InheritsFrom(TdxParagraphRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportParagraphRun(TdxParagraphRun(ARun))
  end
  else
  if ARun.InheritsFrom(TdxFloatingObjectAnchorRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportFloatingObjectAnchorRun(TdxFloatingObjectAnchorRun(ARun))
  end
  else
  if ARun.InheritsFrom(TdxInlinePictureRun) then
  begin
    if ShouldExportInlinePicture(TdxInlinePictureRun(ARun)) then
      ExportInlinePictureRun(TdxInlinePictureRun(ARun))
  end
  else
  if ARun.InheritsFrom(TdxSeparatorTextRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportSeparatorTextRun(TdxSeparatorTextRun(ARun));
  end
  else
  if ARun.InheritsFrom(TdxInlineObjectRun) then
  begin
    if ShouldExportRun(ARun) then
      ExportInlineObjectRun(TdxInlineObjectRun(ARun));
  end
end;

procedure TdxDocumentModelExporter.Export;
begin
  ProgressIndication.&Begin(cxGetResourceString(@sdxRichEditMsg_Saving), 0, CalculateTotalParagraphCount, 0);
  try
    PerformExportPieceTable(DocumentModel.MainPieceTable, ExportDocument);
  finally
    ProgressIndication.&End;
  end;
end;

procedure TdxDocumentModelExporter.ExportCell(ACell: TdxTableCell;
  ATableInfo: TdxTableInfo);
var
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  I: Integer;
  AParagraphCell: TdxTableCell;
  AParagraph: TdxParagraph;
  AParagraphTable: TdxTableInfo;
  AParentTableInfo: TdxTableInfo;
  AAfterNestedTable: Boolean;
  ASkipParagraph: Boolean;
begin
  PushCurrentTableBackgroundColor(ACell.BackgroundColor);

  AStartParagraphIndex := ACell.StartParagraphIndex;
  AEndParagraphIndex := ACell.EndParagraphIndex;
  AAfterNestedTable := False;
  I := AStartParagraphIndex;
  while I <= AEndParagraphIndex do
  begin
    AParagraphCell := PieceTable.Paragraphs[I].GetCell;
    if AParagraphCell.Table.NestedLevel > ATableInfo.NestedLevel then
    begin
      AParagraphTable := TdxTableInfo.Create(AParagraphCell.Table);
      try
        AParentTableInfo := TdxTableInfo.Create(AParagraphTable.GetParentTable(ATableInfo.NestedLevel + 1));
        try
          I := ExportTable(AParentTableInfo);
          AAfterNestedTable := True;
        finally
          AParentTableInfo.Free;
        end;
      finally
        AParagraphTable.Free;
      end;
    end
    else
    begin
      AParagraph := PieceTable.Paragraphs[I];
      ASkipParagraph := AllowSkipParagraphInCell and
        AAfterNestedTable and (AParagraphCell.EndParagraphIndex = I) and AParagraph.IsEmpty;
      if not ASkipParagraph then
        ExportParagraph(AParagraph);
      AAfterNestedTable := False;
    end;
    Inc(I);
  end;
  PopCurrentTableBackgroundColor;
end;

procedure TdxDocumentModelExporter.ExportDocument;
var
  I: Integer;
  ASections: TdxSectionCollection;
begin
  ASections := DocumentModel.Sections;
  for I := 0 to ASections.Count - 1 do
    ExportSectionFiltered(ASections[I]);
  TryToExportBookmarks(PieceTable.Runs.Count - 1, 1);
end;

procedure TdxDocumentModelExporter.ExportPieceTable;
begin
  ExportParagraphs(0, PieceTable.Paragraphs.Count - 1);
  TryToExportBookmarks(PieceTable.Runs.Count - 1, 1);
end;

procedure TdxDocumentModelExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
begin
  Inc(FEndNoteNumber);
end;

procedure TdxDocumentModelExporter.ExportFieldCodeEndRun(
  ARun: TdxFieldCodeEndRun);
begin
  Dec(FFieldLevel);
end;

procedure TdxDocumentModelExporter.ExportFieldCodeStartRun(
  ARun: TdxFieldCodeStartRun);
begin
  Inc(FFieldLevel);
end;

procedure TdxDocumentModelExporter.ExportFieldResultEndRun(
  ARun: TdxFieldResultEndRun);
begin
// do nothing
end;

procedure TdxDocumentModelExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
begin
end;

procedure TdxDocumentModelExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
begin
  Inc(FFootNoteNumber);
end;

procedure TdxDocumentModelExporter.ExportImageReference(ARun: TdxFloatingObjectAnchorRun);
begin
end;

procedure TdxDocumentModelExporter.ExportImageReference(ARun: TdxInlinePictureRun);
begin
end;

function TdxDocumentModelExporter.ExportParagraph(
  AParagraph: TdxParagraph): TdxParagraphIndex;
begin
  ExportParagraphRuns(AParagraph);
  Result := AParagraph.Index;
end;

function TdxDocumentModelExporter.ExportParagraphFiltered(
  AParagraph: TdxParagraph): TdxParagraphIndex;
var
  AParagraphCell: TdxTableCell;
begin
  Inc(FExportedParagraphCount);
  ProgressIndication.SetProgress(ExportedParagraphCount);
  if not ShouldExportParagraph(AParagraph) then
    Result := AParagraph.Index
  else
  begin
    AParagraphCell := AParagraph.GetCell;
    if ShouldUseCustomSaveTableMethod or (AParagraphCell = nil) or
        not DocumentModel.DocumentCapabilities.TablesAllowed then
      Result := ExportParagraph(AParagraph)
    else
      Result := ExportRootTable(AParagraphCell.Table);
  end;
end;

procedure TdxDocumentModelExporter.ExportParagraphRun(ARun: TdxParagraphRun);
begin
// do nothing
end;

procedure TdxDocumentModelExporter.ExportInlineObjectRun(
  ARun: TdxInlineObjectRun);
begin
// do nothing
end;

procedure TdxDocumentModelExporter.ExportInlinePictureRun(ARun: TdxInlinePictureRun);
begin
// do nothing
end;

function TdxDocumentModelExporter.GetNumberingListText(AParagraph: TdxParagraph): string;
begin
  Result := AParagraph.GetNumberingListText(FPieceTableNumberingListCounters.CalculateNextCounters(AParagraph));
end;

procedure TdxDocumentModelExporter.ExportParagraphRuns(
  AParagraph: TdxParagraph);
var
  ALastRunIndex: TdxRunIndex;
  I: Integer;
begin
  ALastRunIndex := AParagraph.LastRunIndex;
  for I := AParagraph.FirstRunIndex to ALastRunIndex do
  begin
    TryToExportBookmarks(I, 0);
    ExportRun(I);
  end;
end;

procedure TdxDocumentModelExporter.ExportParagraphs(AFrom,
  ATo: TdxParagraphIndex);
var
  I: TdxParagraphIndex;
begin
  I := AFrom;
  while I <= ATo do
  begin
    I := ExportParagraphFiltered(PieceTable.Paragraphs[I]);
    Inc(I);
  end;
end;

function TdxDocumentModelExporter.ExportRootTable(
  ATable: TdxTable): TdxParagraphIndex;
var
  ATableInfo: TdxTableInfo;
  ARootTable: TdxTable;
  ARootTableInfo: TdxTableInfo;
begin
  ATableInfo := TdxTableInfo.Create(ATable);
  try
    if ATable.NestedLevel > 0 then
    begin
      ARootTable := ATableInfo.GetRootTable;
      ARootTableInfo := TdxTableInfo.Create(ARootTable);
      try
        Result := ExportTable(ARootTableInfo);
      finally
        ARootTableInfo.Free;
      end;
    end
    else
      Result := ExportTable(ATableInfo);
  finally
    ATableInfo.Free;
  end;
end;

procedure TdxDocumentModelExporter.ExportRow(ARow: TdxTableRow;
  ATableInfo: TdxTableInfo);
var
  ACount, ACellIndex: Integer;
begin
  ACount := ARow.Cells.Count;
  for ACellIndex := 0 to ACount - 1 do
    ExportCell(ARow.Cells[ACellIndex], ATableInfo);
end;

procedure TdxDocumentModelExporter.ExportRun(I: TdxRunIndex);
begin
  PieceTable.Runs[I].Export(Self);
end;

procedure TdxDocumentModelExporter.ExportSection(const ASection: TdxSection);
begin
  FCurrentSection := ASection;
  if ShouldCalculateFootNoteAndEndNoteNumbers then
  begin
    CalculateFootNoteNumber(ASection);
    CalculateEndNoteNumber(ASection);
  end;
  ExportSectionHeadersFooters(ASection);
  ExportParagraphs(ASection.FirstParagraphIndex, ASection.LastParagraphIndex);
end;

procedure TdxDocumentModelExporter.ExportSectionHeadersFooters(ASection: TdxSection);
begin
  ExportSectionHeadersFootersCore(ASection);
end;

procedure TdxDocumentModelExporter.ExportSectionHeadersFootersCore(ASection: TdxSection);
begin
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  if ASection.InnerFirstPageHeader <> nil then
    ExportFirstPageHeader(ASection.InnerFirstPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.First));
  if ASection.InnerOddPageHeader <> nil then
    ExportOddPageHeader(ASection.InnerOddPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Odd));
  if ASection.InnerEvenPageHeader <> nil then
    ExportEvenPageHeader(ASection.InnerEvenPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Even));
  if ASection.InnerFirstPageFooter <> nil then
    ExportFirstPageFooter(ASection.InnerFirstPageFooter, ASection.Footers.IsLinkedToPrevious(TdxHeaderFooterType.First));
  if ASection.InnerOddPageFooter <> nil then
    ExportOddPageFooter(ASection.InnerOddPageFooter, ASection.Footers.IsLinkedToPrevious(TdxHeaderFooterType.Odd));
  if ASection.InnerEvenPageFooter <> nil then
    ExportEvenPageFooter(ASection.InnerEvenPageFooter, ASection.Footers.IsLinkedToPrevious(TdxHeaderFooterType.Even));
end;

procedure TdxDocumentModelExporter.ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionHeader.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionHeader.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionHeader.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionFooter.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionFooter.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  PerformExportPieceTable(TdxPieceTable(ASectionFooter.PieceTable), ExportPieceTable);
end;

procedure TdxDocumentModelExporter.ExportSectionFiltered(const ASection: TdxSection);
begin
  if ShouldExportSection(ASection) then
    ExportSection(ASection)
  else
  begin
    ExportedParagraphCount := ExportedParagraphCount + CalculateSectionParagraphCount(ASection);
    ProgressIndication.SetProgress(ExportedParagraphCount);
  end;
end;

procedure TdxDocumentModelExporter.ExportSectionRun(ARun: TdxSectionRun);
begin
end;

procedure TdxDocumentModelExporter.ExportSeparatorTextRun(
  ARun: TdxSeparatorTextRun);
begin
end;

function TdxDocumentModelExporter.ExportTable(
  ATableInfo: TdxTableInfo): TdxParagraphIndex;
var
  ARowsCount: Integer;
  I: Integer;
begin
  PushCurrentTableBackgroundColor(ATableInfo.Table.BackgroundColor);

  ARowsCount := ATableInfo.Table.Rows.Count;
  for I := 0 to ARowsCount - 1 do
    ExportRow(ATableInfo.Table.Rows[I], ATableInfo);

  PopCurrentTableBackgroundColor;
  Result := ATableInfo.Table.EndParagraphIndex;
end;

procedure TdxDocumentModelExporter.ExportTextRun(ARun: TdxTextRun);
begin
end;

function TdxDocumentModelExporter.CreateVisitableDocumentIntervalBoundaryIterator: TdxVisitableDocumentIntervalBasedObjectBoundaryIterator;
begin
  Result := TdxVisitableDocumentIntervalBasedObjectBoundaryIterator.Create(PieceTable);
end;

function TdxDocumentModelExporter.GetAllowSkipParagraphInCell: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentModelExporter.ExportBookmarkStart(ABookmark: TdxBookmark);
begin
end;

procedure TdxDocumentModelExporter.ExportBookmarkEnd(ABookmark: TdxBookmark);
begin
end;

procedure TdxDocumentModelExporter.ExportRangePermissionStart(ARangePermission: TdxRangePermission);
begin
end;

procedure TdxDocumentModelExporter.ExportRangePermissionEnd(ARangePermission: TdxRangePermission);
begin
end;

function TdxDocumentModelExporter.GetPieceTableNumberingListCounters: TdxPieceTableNumberingListCountersManager;
begin
  if FPieceTableNumberingListCounters = nil then
    FPieceTableNumberingListCounters := TdxPieceTableNumberingListCountersManager.Create;
  Result := FPieceTableNumberingListCounters;
end;

function TdxDocumentModelExporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDocumentModelExporter.GetVisitableDocumentIntervalsIterator: TdxVisitableDocumentIntervalBoundaryIterator;
begin
  if FVisitableDocumentIntervalsIteratorStack.Count > 0 then
    Result := FVisitableDocumentIntervalsIteratorStack.Peek
  else
    Result := nil;
end;

procedure TdxDocumentModelExporter.InvalidateParagraphsBoxes;
var
  AParagraph: TdxParagraph;
  I: Integer;
begin
  for I := 0 to FPieceTable.Paragraphs.Count - 1 do
  begin
    AParagraph := FPieceTable.Paragraphs[I];
    AParagraph.BoxCollection.InvalidateBoxes;
  end;
end;

procedure TdxDocumentModelExporter.PerformExportPieceTable(
  APieceTable: TdxPieceTable; APieceTableExporter: TdxExportPieceTableDelegate);
var
  AOriginalPieceTable: TdxPieceTable;
  AOriginalPieceTableNumberingListCounters: TdxPieceTableNumberingListCountersManager;
  ATransaction: TdxCompositeHistoryItem;
  ATransactionItemCountBeforeExecute: Integer;
  I: Integer;
begin
  AOriginalPieceTable := FPieceTable;
  AOriginalPieceTableNumberingListCounters := PieceTableNumberingListCounters;
  FPieceTable := APieceTable;
  FPieceTableNumberingListCounters := TdxPieceTableNumberingListCountersManager.Create;
  FPieceTableNumberingListCounters.BeginCalculateCounters;
  try
    PushVisitableDocumentIntervalBoundaryIterator;
    if not ShouldSplitRuns then
      APieceTableExporter
    else
    begin
      DocumentModel.BeginUpdate;
      try
        ATransaction := DocumentModel.History.Transaction;
        ATransactionItemCountBeforeExecute := ATransaction.Items.Count;
        SplitRuns;
        APieceTableExporter;
        PullVisitableDocumentIntervalBoundaries;
        if not DocumentModel.ModelForExport then
        begin
          for I := ATransaction.Items.Count - 1 downto ATransactionItemCountBeforeExecute do
          begin
            ATransaction.Items[I].Undo;
            ATransaction.Items.Delete(I);
          end;
        end;
        InvalidateParagraphsBoxes;
      finally
        DocumentModel.EndUpdate;
      end;
    end;
  finally
    PopVisitableDocumentIntervalBoundaryIterator;
    FPieceTableNumberingListCounters.EndCalculateCounters;
    if AOriginalPieceTable = nil then
      FPieceTable := DocumentModel.MainPieceTable
    else
      FPieceTable := AOriginalPieceTable;

    FreeAndNil(FPieceTableNumberingListCounters);
    FPieceTableNumberingListCounters := AOriginalPieceTableNumberingListCounters;
  end;
end;

procedure TdxDocumentModelExporter.PopCurrentTableBackgroundColor;
begin
  FTableBackgroundColorStack.Pop;
end;

procedure TdxDocumentModelExporter.PopVisitableDocumentIntervalBoundaryIterator;
begin
  Assert(FVisitableDocumentIntervalsIteratorStack.Count > 0);
  FVisitableDocumentIntervalsIteratorStack.Extract.Free;
end;

function TdxDocumentModelExporter.PrepareModelForExport(
  ADocumentModel: TdxCustomDocumentModel): TdxCustomDocumentModel;
begin
  Result := ADocumentModel;
end;

procedure TdxDocumentModelExporter.PushCurrentTableBackgroundColor(
  AColor: TdxAlphaColor);
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    AColor := CurrentTableBackgroundColor;
  FTableBackgroundColorStack.Push(AColor);
end;

procedure TdxDocumentModelExporter.PushVisitableDocumentIntervalBoundaryIterator;
begin
  FVisitableDocumentIntervalsIteratorStack.Push(CreateVisitableDocumentIntervalBoundaryIterator);
end;

procedure TdxDocumentModelExporter.PullVisitableDocumentIntervalBoundaries;
begin
  if VisitableDocumentIntervalsIterator = nil then
    Exit;
  while not VisitableDocumentIntervalsIterator.IsDone do
  begin
    if VisitableDocumentIntervalsIterator.Current <> nil then
      VisitableDocumentIntervalsIterator.Current.Export(Self);
    VisitableDocumentIntervalsIterator.MoveNext;
  end;
end;

function TdxDocumentModelExporter.ShouldExportRun(
  ARun: TdxTextRunBase): Boolean;
begin
  Result := ShouldExportHiddenText;
  if not Result then
  begin
    if FFieldLevel > 0 then
      Result := False
    else
      Result := not ARun.Hidden;
  end;
end;

function TdxDocumentModelExporter.ShouldExportSection(
  ASection: TdxSection): Boolean;
begin
  Result := ShouldExportHiddenText or not ASection.IsHidden;
end;

function TdxDocumentModelExporter.ShouldSplitRuns: Boolean;
begin
  Result := VisitableDocumentIntervalsIterator.Boundaries.Count > 0;
end;

function TdxDocumentModelExporter.ShouldUseCustomSaveTableMethod: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentModelExporter.SplitRuns;
begin
  SplitRunsByBookmarkBoundaries;
  InvalidateParagraphsBoxes;
end;

procedure TdxDocumentModelExporter.TryToExportBookmarks(ARunIndex: TdxRunIndex;
  ARunOffset: Integer);
var
  ABoundary: TdxVisitableDocumentIntervalBoundary;
begin
  if VisitableDocumentIntervalsIterator = nil then
    Exit;

  while not VisitableDocumentIntervalsIterator.IsDone do
  begin
    ABoundary := VisitableDocumentIntervalsIterator.Current;
    if (ABoundary.Position.RunIndex <> ARunIndex) or (ABoundary.Position.RunOffset <> ARunOffset) then
      Break;
    ABoundary.Export(Self);

    VisitableDocumentIntervalsIterator.MoveNext;
  end;
end;

function TdxDocumentModelExporter.ShouldCalculateFootNoteAndEndNoteNumbers: Boolean;
begin
  Result := False;
end;

function TdxDocumentModelExporter.ShouldExportHiddenText: Boolean;
begin
  Result := False;
end;

function TdxDocumentModelExporter.ShouldExportInlinePicture(
  ARun: TdxInlinePictureRun): Boolean;
begin
  Result := not ARun.Image.SuppressStore and ShouldExportRun(ARun);
end;

procedure TdxDocumentModelExporter.SplitRunsByBookmarkBoundaries;
var
  ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
  ACount, I: Integer;
begin
  ABoundaries := VisitableDocumentIntervalsIterator.Boundaries;
  ACount := ABoundaries.Count;
  for I := 0 to ACount - 1 do
    SplitRunByModelPosition(ABoundaries[I].Position^);
end;

procedure TdxDocumentModelExporter.SplitRunByModelPosition(const APos: TdxDocumentModelPosition);
begin
  if (APos.RunOffset <> 0) and (APos.LogPosition <= PieceTable.DocumentEndLogPosition) then
    PieceTable.SplitTextRun(APos.ParagraphIndex, APos.RunIndex, APos.RunOffset);
end;

function TdxDocumentModelExporter.CalculateTotalParagraphCount: Integer;
var
  ASections: TdxSectionCollection;
  ACount, I: TdxSectionIndex;
begin
  Result := 0;
  ASections := DocumentModel.Sections;
  ACount := ASections.Count;
  for I := 0 to ACount - 1 do
    Inc(Result, CalculateSectionParagraphCount(ASections[I]));
end;

function TdxDocumentModelExporter.CalculateSectionParagraphCount(ASection: TdxSection): Integer;
begin
  Result := ASection.LastParagraphIndex - ASection.FirstParagraphIndex + 1;
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerFirstPageHeader));
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerOddPageHeader));
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerEvenPageHeader));
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerFirstPageFooter));
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerOddPageFooter));
  Inc(Result, CalculatePieceTableParagraphCount(ASection.InnerEvenPageFooter));
end;

function TdxDocumentModelExporter.CalculatePieceTableParagraphCount(AHeaderFooter: TdxSectionHeaderFooterBase): Integer;
begin
  if AHeaderFooter = nil then
    Result := 0
  else
    Result := TdxPieceTable(AHeaderFooter.PieceTable).Paragraphs.Count;
end;

procedure TdxDocumentModelExporter.CalculateFootNoteNumber(ASection: TdxSection);
begin
  FFootNoteNumber := CalculateFootNoteNumberCore(ASection.FootNote, FFootNoteNumber);
end;

procedure TdxDocumentModelExporter.CalculateEndNoteNumber(ASection: TdxSection);
begin
  FEndNoteNumber := CalculateFootNoteNumberCore(ASection.EndNote, FFootNoteNumber);
end;

function TdxDocumentModelExporter.CalculateFootNoteNumberCore(AProperties: TdxSectionFootNote; AValue: Integer): Integer;
begin
  if AProperties.NumberingRestartType = TdxLineNumberingRestart.NewSection then
    Result := AProperties.StartingNumber - 1
  else
    Result := AValue;
end;

function TdxDocumentModelExporter.CreateFootNoteExportInfo(ANote: TdxFootNoteRun): TdxFootNoteExportInfo;
begin
  Result := TdxFootNoteExportInfo.Create(TdxPieceTable(ANote.Note.PieceTable), FFootNoteNumber, FCurrentSection.FootNote.FormatCounterValue(FFootNoteNumber));
end;

function TdxDocumentModelExporter.FindFootNoteExportInfoByNote(AList: TList<TdxFootNoteExportInfo>;
  APieceTable: TdxPieceTable; out AInfo: TdxFootNoteExportInfo): Boolean;
var
  ACount, I: Integer;
begin
  ACount := AList.Count;
  for I := 0 to ACount - 1 do
    if AList[I].Note = APieceTable then
    begin
      AInfo := AList[I];
      Exit(True);
    end;

  Result := False;
end;

function TdxDocumentModelExporter.CreateFootNoteExportInfo(ANote: TdxEndNoteRun): TdxFootNoteExportInfo;
begin
  Result := TdxFootNoteExportInfo.Create(TdxPieceTable(ANote.Note.PieceTable), FEndNoteNumber, FCurrentSection.EndNote.FormatCounterValue(FEndNoteNumber));
end;

function TdxDocumentModelExporter.ShouldExportParagraph(AParagraph: TdxParagraph): Boolean;
begin
  Result := ShouldExportHiddenText or not AParagraph.IsHidden;
end;

{ TdxPieceTableNumberingListCountersManager }

procedure TdxPieceTableNumberingListCountersManager.BeginCalculateCounters;
begin
  FCalculators := TObjectDictionary<TdxAbstractNumberingList, TdxNumberingListCountersCalculator>.Create([doOwnsValues]);
end;

function TdxPieceTableNumberingListCountersManager.CalculateNextCounters(
  ACurrentParagraph: TdxParagraph): TIntegerDynArray;
var
  AbstractNumberingList: TdxAbstractNumberingList;
  ACalculator: TdxNumberingListCountersCalculator;
begin
  Result := nil;
  AbstractNumberingList := ACurrentParagraph.GetAbstractNumberingList;
  if AbstractNumberingList <> nil then
  begin
    if not FCalculators.TryGetValue(AbstractNumberingList, ACalculator) then
    begin
      ACalculator := TdxNumberingListCountersCalculator.Create(AbstractNumberingList);
      ACalculator.BeginCalculateCounters;
      FCalculators.Add(AbstractNumberingList, ACalculator);
    end;
    Result := ACalculator.CalculateNextCounters(ACurrentParagraph);
  end;
end;

procedure TdxPieceTableNumberingListCountersManager.EndCalculateCounters;
var
  AKey: TdxAbstractNumberingList;
begin
  for AKey in FCalculators.Keys do
    FCalculators[AKey].EndCalculateCounters;
  FreeAndNil(FCalculators);
end;

end.
