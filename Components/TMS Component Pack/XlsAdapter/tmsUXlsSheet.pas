unit tmsUXlsSheet;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords, tmsUXlsChart,
     tmsUXlsSST, tmsXlsMessages, tmsUXlsSections, tmsUXlsRowColEntries, tmsUXlsEscher,
     tmsUXlsWorkbookGlobals, tmsUXlsNotes, tmsUXlsBaseList,
     tmsUFlxMessages, tmsUXlsPageBreaks,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     {$IFNDEF FLX_CROSSPLAT}Windows, {$ENDIF} //On linux resources is defined on system
     {$IFDEF FLX_NEEDSTYPES} Types,{$ENDIF} //Delphi 6 or above
     

     tmsUXlsColInfo, tmsUXlsHyperLink, tmsUOle2Impl;

const
     RESOURCE_EMPTYSHEET = 'TMSFLXEMPTYSHEET';

type
  TSheet= class (TBaseSection)
  private
    function GetShowGridLines: boolean;
    procedure SetShowGridLines(const Value: boolean);
    function GetShowGridHeaders: boolean;
    procedure SetShowGridHeaders(const Value: boolean);
    function GetPrintGridLines: boolean;
    procedure SetPrintGridLines(const Value: boolean);
    function GetPageFooter: UTF16String;
    function GetPageHeader: UTF16String;
    procedure SetPageFooter(const Value: UTF16String);
    procedure SetPageHeader(const Value: UTF16String);
    function GetMargins: TXlsMargins;
    procedure SetMargins(const Value: TXlsMargins);
    procedure AddMargin(var Margin: TMarginRecord; const aId: integer; const Value: extended);
    function GetSheetZoom: integer;
    procedure SetSheetZoom(const Value: integer);
    function GetCodeName: UTF16String;
    procedure SetCodeName(const Value: UTF16String);
    function GetIsR1C1: boolean;
    procedure SetIsR1C1(const Value: boolean);
    function GetWindow2Frozen: Boolean;
    procedure SetWindow2Frozen(const value: Boolean);
  protected
    FWorkbookGlobals: TWorkbookGlobals;
    FWindow2: TWindow2Record;
    FPageHeader: TPageHeaderRecord;
    FPageFooter: TPageFooterRecord;
    FPrintGridLines: TPrintGridLinesRecord;
    FLeftMargin, FRightMargin, FTopMargin, FBottomMargin: TMarginRecord;
    FSetup: TSetupRecord;
    FPrinterDriverSettings: TPlsRecord;
    FWSBool: TWSBoolRecord;
    FHCenter, FVCenter: TPrintCenteredRecord;
    FZoom: TSCLRecord;
    FGuts: TGutsRecord;
    FPane: TPaneRecord;
    FAutoFilterInfo: TAutoFilterInfoRecord;


    FRefMode: TRefModeRecord;

    FPrintRecords: TBaseRecordList;

    FCodeName: TCodeNameRecord;

    procedure LoadCachePointers(const R: TBaseRecord);
    procedure FixCachePointers; virtual;
    function GetSelected: boolean;
    procedure SetSelected(const Value: boolean);
    procedure SetPageHeaderFooter(const P: TPageHeaderFooterRecord; const s: UTF16String);virtual;abstract;
    procedure AddZoomRecord; virtual;abstract;
    procedure AddOrRemovePane(const Add: boolean);virtual;

    property Window2Frozen: boolean read GetWindow2Frozen write SetWindow2Frozen;

  public
    OriginalDimensions: TDimensionsRec;

    function CopyTo: TSheet; //This method can't be virtual
    function DoCopyTo: TSheet; virtual;
    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);virtual;abstract;
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word;const SheetInfo: TSheetInfo);virtual; abstract;
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer ; const SheetInfo: TSheetInfo);virtual;abstract;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo); virtual;

    procedure Clear; override;

    procedure DeleteHPageBreak(const aRow: word);virtual;
    procedure DeleteVPageBreak(const aCol: word);virtual;
    procedure InsertHPageBreak(const aRow: word);virtual;
    procedure InsertVPageBreak(const aCol: word);virtual;

    constructor Create(const aWorkbookGlobals: TWorkbookGlobals);virtual;

    property Selected: boolean read GetSelected write SetSelected;
    property ShowGridLines: boolean read GetShowGridLines write SetShowGridLines;
    property ShowGridHeaders: boolean read GetShowGridHeaders write SetShowGridHeaders;
    property PrintGridLines: boolean read GetPrintGridLines write SetPrintGridLines;

    property PageHeader: UTF16String read GetPageHeader write SetPageHeader;
    property PageFooter: UTF16String read GetPageFooter write SetPageFooter;
    property Margins: TXlsMargins read GetMargins write SetMargins; //Margins are in inches

    property SheetZoom: integer read GetSheetZoom write SetSheetZoom;

    property IsR1C1: boolean read GetIsR1C1 write SetIsR1C1;

    procedure FreezePanes(const row, col: integer);
    procedure GetFrozenPanes(out row, col: integer);
    procedure GetSplitWindow(out xOffset, yOffset: integer);
    procedure SplitWindow(const xOffset, yOffset: integer);

    property CodeName: UTF16String read GetCodeName write SetCodeName;

    function TotalSize:int64; override;
    function FixTotalSize(const NeedsRecalc: boolean): int64;virtual;
    function TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange):int64; override;

   	procedure RestoreObjectCoords; virtual;

    procedure FixRows; virtual;
  end;

  ClassOfTSheet= class of TSheet;

  TFlxChart = class (TSheet)
  private
    FChartRecords: TChartRecordList;
    procedure LoadSubChart(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const SST: TSST; const Level: integer);
  protected
    procedure SetPageHeaderFooter(const P: TPageHeaderFooterRecord; const s: UTF16String);override;
    procedure AddZoomRecord; override;
    procedure FixCachePointers; override;

  public
    RemainingData: TBaseRecord;

    constructor Create(const aWorkbookGlobals: TWorkbookGlobals);override;
    destructor Destroy;override;
    function DoCopyTo: TSheet; override;

    function TotalSize:int64; override;
    function TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange):int64; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBOFRecord; const SST: TSST);override;
    procedure SaveToStream(const DataStream: TOle2File);override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer; const CellRange: TXlsCellRange);override;
    procedure Clear; override;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);override;
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);override;
    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);override;
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);override;

  end;

  TSheetRecordList=class(TBaseRecordList)
  end;

  TChartList = class(TBaseList) //records are TFlxChart
    {$INCLUDE TChartListHdr.inc}
    procedure SaveToStream(const DataStream: TOle2File);
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
  end;

  TFlxUnsupportedSheet = class (TSheet)
  private
    FSheetRecords: TSheetRecordList;
  protected
    procedure SetPageHeaderFooter(const P: TPageHeaderFooterRecord; const s: UTF16String);override;
    procedure AddZoomRecord; override;
    procedure FixCachePointers; override;

  public

    constructor Create(const aWorkbookGlobals: TWorkbookGlobals);override;
    destructor Destroy;override;
    function DoCopyTo: TSheet; override;

    function TotalSize:int64; override;
    function TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange):int64; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBOFRecord; const SST: TSST);override;
    procedure SaveToStream(const DataStream: TOle2File);override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer; const CellRange: TXlsCellRange);override;
    procedure Clear; override;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);override;
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);override;
    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);override;
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);override;

  end;

  TWorkSheet = class (TSheet)
  private
    FMiscRecords1: TBaseRecordList;
    FMiscRecords2: TBaseRecordList;
    FHPageBreaks: THPageBreakList;
    FVPageBreaks: TVPageBreakList;
    FDrawing: TDrawing;
    FCells: TCells;
    FRanges: TRangeList;
    FNotes: TNoteList;
    FColumns: TColInfoList;
    FHLinks: THLinkList;

    FDefRowHeight: Longint;
    FDefColWidth:  integer;

    function GetDrawingRow(index: integer): integer;
    function GetDrawingName(index: integer): UTF16String;
    function GetPrintNumberOfHorizontalPages: word;
    function GetPrintNumberOfVerticalPages: word;
    function GetPrintScale: integer;
    function GetPrintToFit: boolean;
    procedure SetPrintNumberOfHorizontalPages(const Value: word);
    procedure SetPrintNumberOfVerticalPages(const Value: word);
    procedure SetPrintScale(const Value: integer);
    procedure SetPrintToFit(const Value: boolean);
    function GetPrintOptions: word;
    procedure SetPrintOptions(const Value: word);

    function GetPrintCopies: integer;
    function GetPrinterDriverSettings: TPrinterDriverSettings;
    function GetPrintPaperSize: TExcelPaperSize;
    function GetPrintXResolution: integer;
    function GetPrintYResolution: integer;
    procedure SetPrintCopies(const Value: integer);
    procedure SetPrinterDriverSettings(
      const Value: TPrinterDriverSettings);
    procedure SetPrintPaperSize(const Value: TExcelPaperSize);
    procedure SetPrintXResolution(const Value: integer);
    procedure SetPrintYResolution(const Value: integer);

    function GetColOutlineLevel(col: integer): integer;
    function GetRowOutlineLevel(row: integer): integer;
    procedure SetColOulineLevel(col: integer; const Value: integer);
    procedure SetRowOulineLevel(row: integer; const Value: integer);

    procedure EnsureGuts;
    procedure AddPrinterDriverRecord(const aPlsRecord: TPlsRecord; const FRecords: TBaseRecordList);
    procedure RemovePrinterDriverRecord;overload;
    procedure RemovePrinterDriverRecord(const FRecords: TBaseRecordList);overload;
    function GetOutlineSummaryColsRightOfDetail: boolean;
    function GetOutlineSummaryRowsBelowDetail: boolean;
    function GetOutlineAutomaticStyles: boolean;
    procedure SetOutlineSummaryColsRightOfDetail(const value: boolean);
    procedure SetOutlineSummaryRowsBelowDetail(const value: boolean);
    procedure SetOutlineAutomaticStyles(const value: boolean);
    function GetPrintHCentered: boolean;
    function GetPrintVCentered: boolean;
    procedure SetPrintHCentered(const Value: boolean);
    procedure SetPrintVCentered(const Value: boolean);
  protected
    procedure AddZoomRecord; override;
    procedure SetPageHeaderFooter(const P: TPageHeaderFooterRecord; const s: UTF16String);override;
    procedure FixCachePointers; override;
    procedure AddOrRemovePane(const Add: boolean);override;
  public
    constructor Create(const aWorkbookGlobals: TWorkbookGlobals);override;
    constructor CreateFromData(const aWorkbookGlobals: TWorkbookGlobals; const SST: TSST);
    destructor Destroy;override;
    function DoCopyTo: TSheet; override;

    function TotalSize:int64; override;
    function TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64;override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBOFRecord; const SST: TSST);override;
    procedure SaveToStream(const DataStream: TOle2File);override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer; const CellRange: TXlsCellRange);override;
    procedure Clear; override;

    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);override;
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);override;

    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);override;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo); override;

    procedure ClearValues;

    property Notes: TNoteList read FNotes;
    property Cells: TCells read FCells;
    property HLinks: THLinkList read FHLinks;

    function DrawingCount: integer;
    procedure AssignDrawing(const Index: integer; const Data: ByteArray; const DataType: TXlsImgTypes);
    procedure GetDrawingFromStream(const Index: integer; const Data: TStream; var DataType: TXlsImgTypes);
    property DrawingRow[index: integer]: integer read GetDrawingRow;
    property DrawingName[index: integer]: UTF16String read GetDrawingName;
    function GetAnchor(const Index: integer): TClientAnchor;
    procedure SetAnchor(const Index: integer; const aAnchor: TClientAnchor);

    procedure DeleteImage(const Index: integer);
    procedure ClearImage(const Index: integer);
    procedure AddImage(const Data: ByteArray; const DataType: TXlsImgTypes; const Properties: TImageProperties;const Anchor: TFlxAnchorType);

    procedure AddNewComment(const Row, Col: integer; const Txt: UTF16String; const Properties: TImageProperties);

    procedure DeleteHPageBreak(const aRow: word);override;
    procedure DeleteVPageBreak(const aCol: word);override;
    procedure InsertHPageBreak(const aRow: word);override;
    procedure InsertVPageBreak(const aCol: word);override;

    function GetRowHeight(const aRow: integer): integer;overload;
    function GetRowHeight(const aRow: integer; const HiddenIsZero: boolean): integer;overload;
    function GetColWidth(const aCol: Word): integer;overload;
    function GetColWidth(const aCol: Word; const HiddenIsZero: boolean): integer;overload;
    procedure SetRowHeight(const aRow: integer; const Value: integer);
    procedure SetColWidth(const aCol: Word; const Value: integer);

    function GetRowHidden(const aRow: integer): boolean;
    function GetColHidden(const aCol: Word): boolean;
    procedure SetRowHidden(const aRow: integer; const Value: boolean);
    procedure SetColHidden(const aCol: Word; const Value: boolean);


    property DefRowHeight: Longint read FDefRowHeight;
    property DefColWidth:  integer read FDefColWidth;

    function GetRowFormat(const aRow: integer): integer;
    function GetColFormat(const aCol: integer): integer;
    procedure SetRowFormat(const aRow: integer; const Value: integer);
    procedure SetColFormat(const aCol: integer; const Value: integer);

    function CellMergedBounds(const aRow, aCol: integer): TXlsCellRange;
    function CellMergedList(const index: integer): TXlsCellRange;
    function CellMergedListCount: integer;
    procedure MergeCells(aRow1, aCol1, aRow2, aCol2: integer);
    procedure UnMergeCells(aRow1, aCol1, aRow2, aCol2: integer);

    function HasHPageBreak(const Row: integer): boolean;
    function HasVPageBreak(const Col: integer): boolean;

    property PrintToFit: boolean read GetPrintToFit write SetPrintToFit;
    property PrintHCentered: boolean read GetPrintHCentered write SetPrintHCentered;
    property PrintVCentered: boolean read GetPrintVCentered write SetPrintVCentered;

    property PrintScale: integer read GetPrintScale write SetPrintScale;
    property PrintNumberOfHorizontalPages: word read GetPrintNumberOfHorizontalPages write SetPrintNumberOfHorizontalPages;
    property PrintNumberOfVerticalPages: word read GetPrintNumberOfVerticalPages write SetPrintNumberOfVerticalPages;
    property PrintOptions: word read GetPrintOptions write SetPrintOptions;

    property PrintPaperSize: TExcelPaperSize read GetPrintPaperSize write SetPrintPaperSize;
    property PrintCopies: integer read GetPrintCopies write SetPrintCopies;
    property PrintXResolution: integer read GetPrintXResolution write SetPrintXResolution;
    property PrintYResolution: integer read GetPrintYResolution write SetPrintYResolution;

    property PrinterDriverSettings: TPrinterDriverSettings read GetPrinterDriverSettings write SetPrinterDriverSettings;

    procedure DoCreateFromData(const SST: TSST);

    property RowOutlineLevel[row: integer]: integer read GetRowOutlineLevel write SetRowOulineLevel;
    property ColOutlineLevel[col: integer]: integer read GetColOutlineLevel write SetColOulineLevel;
    property OutlineSummaryRowsBelowDetail: boolean read GetOutlineSummaryRowsBelowDetail write SetOutlineSummaryRowsBelowDetail;
    property OutlineSummaryColsRightOfDetail: boolean read GetOutlineSummaryColsRightOfDetail write SetOutlineSummaryColsRightOfDetail;
    property OutlineAutomaticStyles: boolean read GetOutlineAutomaticStyles write SetOutlineAutomaticStyles;

  	procedure RestoreObjectCoords; override;

    procedure RecalcRowHeights(const Workbook: pointer; const Row1, Row2: Integer; const Forced: Boolean; const KeepAutoFit: Boolean; const Adjustment: extended);
    procedure RecalcColWidths(const Workbook: pointer; const Col1, Col2: Integer; const IgnoreStrings: Boolean; const Adjustment: extended);

    procedure SetAutoFilter(const SheetIndex: Int32; const Row: Int32; const Col1: Int32; const Col2: Int32);
    procedure RemoveAutoFilter();
    function HasAutoFilter(): Boolean;overload;
    function HasAutoFilter(const sheet: Int32; const row: Int32; const col: Int32): Boolean;overload;
    function GetAutoFilterRange(const Sheet: Int32): TXlsCellRange;

    procedure FixRows; override;
    function FixTotalSize(const NeedsRecalc: boolean): int64;override;

  end;

implementation
uses tmsUXlsCellRecords, tmsUXlsFormula, tmsUXlsCondFmt, tmsUXlsRangeRecords;

{$INCLUDE TChartListImp.inc}

{ TSheet }

function TSheet.CopyTo: TSheet;
begin
  if Self= nil then Result:=nil else Result:= DoCopyTo;
end;

constructor TSheet.Create(const aWorkbookGlobals: TWorkbookGlobals);
begin
  FWorkbookGlobals:=aWorkbookGlobals;
end;

function TSheet.DoCopyTo: TSheet;
begin
  Result:= ClassOfTSheet(ClassType).Create(FWorkbookGlobals);
  Result.sBOF:= sBOF.CopyTo as TBOFRecord;
  Result.sEOF:= sEOF.CopyTo as TEOFRecord;
  Result.FCodeName:= nil;
end;

function TSheet.GetSelected: boolean;
begin
  if (FWindow2<>nil) then Result:=FWindow2.Selected else Result:=false;
end;

procedure TSheet.SetSelected(const Value: boolean);
begin
  if (FWindow2<>nil) then FWindow2.Selected:=value;
end;

procedure TSheet.DeleteHPageBreak(const aRow: word);
begin
  //Nothing in TSheet
end;

procedure TSheet.DeleteVPageBreak(const aCol: word);
begin
  //Nothing in TSheet
end;

procedure TSheet.InsertHPageBreak(const aRow: word);
begin
  //Nothing in TSheet
end;

procedure TSheet.InsertVPageBreak(const aCol: word);
begin
  //Nothing in TSheet
end;

procedure TSheet.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  //Nothing in TSheet
end;

function TSheet.GetShowGridLines: boolean;
begin
  if (FWindow2<>nil) then Result:=FWindow2.ShowGridLines else Result:=true;
end;

procedure TSheet.SetShowGridLines(const Value: boolean);
begin
  if (FWindow2<>nil) then FWindow2.ShowGridLines:=value;
end;

function TSheet.GetShowGridHeaders: boolean;
begin
  if (FWindow2<>nil) then Result:=FWindow2.ShowGridHeaders else Result:=true;
end;

procedure TSheet.SetShowGridHeaders(const Value: boolean);
begin
  if (FWindow2<>nil) then FWindow2.ShowGridHeaders:=value;
end;

function TSheet.GetPrintGridLines: boolean;
begin
  if (FPrintGridLines<>nil) then Result:=FPrintGridLines.Value else Result:=true;
end;

procedure TSheet.SetPrintGridLines(const Value: boolean);
begin
  if (FPrintGridLines<>nil) then FPrintGridLines.Value:=value;
end;

function TSheet.GetPageFooter: UTF16String;
begin
  if (FPageFooter<>nil) then Result:=FPageFooter.Text else Result:='';
end;

function TSheet.GetPageHeader: UTF16String;
begin
  if (FPageHeader<>nil) then Result:=FPageHeader.Text else Result:='';
end;

procedure TSheet.SetPageFooter(const Value: UTF16String);
begin
  SetPageHeaderFooter(FPageFooter, Value);
end;

procedure TSheet.SetPageHeader(const Value: UTF16String);
begin
  SetPageHeaderFooter(FPageHeader, Value);
end;

function TSheet.GetMargins: TXlsMargins;
var
  t, l: double;
const
  l0: array[0..7]of byte=($FC, $FD, $7E, $BF, $DF, $EF, $E7, $3F);
  t0: array[0..7]of byte=($E0, $EF, $F7, $FB, $FD, $7E, $EF, $3F);
begin
  FillChar(Result, SizeOf(Result), 0); //No dynamic fields like huge strings in Result.
  move(t0, t ,SizeOf(t));
  move(l0, l ,SizeOf(t));

  if FLeftMargin<>nil then Result.Left:=FLeftMargin.Value else Result.Left:=l;
  if FRightMargin<>nil then Result.Right:=FRightMargin.Value else Result.Right:=l;
  if FTopMargin<>nil then Result.Top:=FTopMargin.Value else Result.Top:=t;
  if FBottomMargin<>nil then Result.Bottom:=FBottomMargin.Value else Result.Bottom:=t;

  if FSetup<> nil then
  begin
    Result.Header:= FSetup.HeaderMargin;
    Result.Footer:= FSetup.FooterMargin;
  end;

end;

procedure TSheet.SetMargins(const Value: TXlsMargins);
begin
  if FLeftMargin=nil then AddMargin(FLeftMargin, xlr_LEFTMARGIN, Value.Left) else FLeftMargin.Value:=Value.Left;
  if FRightMargin=nil then AddMargin(FRightMargin, xlr_RIGHTMARGIN, Value.Right) else FRightMargin.Value:=Value.Right;
  if FTopMargin=nil then AddMargin(FTopMargin, xlr_TOPMARGIN, Value.Top) else FTopMargin.Value:=Value.Top;
  if FBottomMargin=nil then AddMargin(FBottomMargin, xlr_BOTTOMMARGIN, Value.Bottom) else FBottomMargin.Value:=Value.Bottom;

  if FSetup<> nil then
  begin
    FSetup.HeaderMargin:=Value.Header;
    FSetup.FooterMargin:=Value.Footer;
  end;

end;

procedure TSheet.AddMargin(var Margin: TMarginRecord; const aId: integer;
  const Value: extended);
const
  DataSize=SizeOf(Double);
var
  Data: PArrayOfByte;
  i,k, RId: integer;
begin
  //Search for the best position...
  k:=FPrintRecords.Count-1;
  for i:=FPrintRecords.Count-1 downto 0 do
  begin
    RId:=(FPrintRecords[i] as TBaseRecord).Id;
    if (RId=xlr_LEFTMARGIN) or(RId=xlr_RIGHTMARGIN)
       or(RId=xlr_TOPMARGIN) or(RId=xlr_BOTTOMMARGIN)
       or (RId=xlr_VCENTER)or (RId=xlr_DEFAULTROWHEIGHT)then
    begin
      k:=i;
      break;
    end;
  end;
  GetMem(Data, DataSize);
  try
    FPrintRecords.Insert(k +1,TMarginRecord.Create(aId, Data, DataSize));
  except
    FreeMem(Data);
    raise;
  end; //Except
  Margin:=(FPrintRecords[k+1] as TMarginRecord);
  Margin.Value:=Value;
end;

function TSheet.GetSheetZoom: integer;
begin
  if FZoom<>nil then Result:=FZoom.Zoom else Result:=100;
end;

function TSheet.GetIsR1C1: boolean;
begin
  if FRefMode<>nil then Result:=FRefMode.IsR1C1 else Result:=false;
end;

procedure TSheet.SetIsR1C1(const Value: boolean);
begin
  if FRefMode<>nil then FRefMode.IsR1C1:=Value;
end;


procedure TSheet.SetSheetZoom(const Value: integer);
begin
  if FZoom=nil then AddZoomRecord;
  if FZoom=nil then exit;
  FZoom.Zoom:=Value;
end;

procedure TSheet.FreezePanes(const row: integer; const col: integer);
var
  Frost: Boolean;
  row1: integer;
  col1: integer;
begin
  Frost := (row > 0) or (col > 0);
  Window2Frozen := Frost;
  if FWindow2 <> nil then
    FWindow2.IsFrozenButNoSplit := Frost;

  AddOrRemovePane(Frost);
  if FPane = nil then
    exit;

  if row < 1 then row1 := 1 else row1 := row;
  if col < 1 then col1 := 1 else col1 := col;
  FPane.RowSplit := row;
  if FPane.FirstVisibleRow < row1 then
    FPane.FirstVisibleRow := row1;

  FPane.ColSplit := col;
  if FPane.FirstVisibleCol < col1 then
    FPane.FirstVisibleCol := col1;

  FPane.EnsureSelectedVisible;
end;

procedure TSheet.SplitWindow(const xOffset: integer; const yOffset: integer);
var
  Frost: Boolean;
begin
  Frost := (xOffset > 0) or (yOffset > 0);
  Window2Frozen := false;
  if FWindow2 <> nil then
    FWindow2.IsFrozenButNoSplit := false;

  AddOrRemovePane(Frost);
  if FPane = nil then
    exit;

  FPane.RowSplit := yOffset;
  FPane.ColSplit := xOffset;
  if FPane.FirstVisibleRow < 1 then
    FPane.FirstVisibleRow := 1;

  if FPane.FirstVisibleCol < 1 then
    FPane.FirstVisibleCol := 1;

  FPane.EnsureSelectedVisible;
end;

procedure TSheet.GetFrozenPanes(out row, col: integer);
begin
  Row := 0;
  Col := 0;
  if (FPane <> nil) and Window2Frozen then
  begin
    Row := FPane.RowSplit;
    Col := FPane.ColSplit;
  end;
end;

procedure TSheet.GetSplitWindow(out xOffset: integer; out yOffset: integer);
begin
  xOffset := 0;
  yOffset := 0;
  if (FPane <> nil) and not Window2Frozen then
  begin
    xOffset:=FPane.ColSplit;
    yOffset:=FPane.RowSplit;
  end;
end;

function TSheet.GetWindow2Frozen(): Boolean;
begin
  Result := (FWindow2 = nil) or FWindow2.IsFrozen;
end;

procedure TSheet.SetWindow2Frozen(const value: Boolean);
begin
  if FWindow2 <> nil then
  FWindow2.IsFrozen := value;

end;


procedure TSheet.FixCachePointers;
begin
  //Nothing here.
end;

procedure TSheet.FixRows;
begin

end;

function TSheet.FixTotalSize(const NeedsRecalc: boolean): int64;
begin
  Result := TotalSize;
end;

procedure TSheet.LoadCachePointers(const R: TBaseRecord);

begin
  if R = nil then exit;

  if R.Id=xlr_WINDOW2 then FWindow2:=R as TWindow2Record else
  if R.Id=xlr_SCL then FZoom:=R as TSCLRecord else
  if R.Id=xlr_FOOTER then FPageFooter:=R as TPageFooterRecord else
  if R.Id=xlr_HEADER then FPageHeader:=R as TPageHeaderRecord else
  if R.Id=xlr_PRINTGRIDLINES then FPrintGridLines:=R as TPrintGridLinesRecord else

  if R.Id=xlr_LEFTMARGIN then FLeftMargin:=R as TMarginRecord else
  if R.Id=xlr_RIGHTMARGIN then FRightMargin:=R as TMarginRecord else
  if R.Id=xlr_TOPMARGIN then FTopMargin:=R as TMarginRecord else
  if R.Id=xlr_BOTTOMMARGIN then FBottomMargin:=R as TMarginRecord else

  if R.Id=xlr_SETUP then FSetup:=R as TSetupRecord else
  if R.Id=xlr_PLS then FPrinterDriverSettings:=R as TPlsRecord else
  if R.Id=xlr_WSBool then FWsBool:=R as TWsBoolRecord else

  if R.Id=xlr_HCENTER then FHCenter := R as TPrintCenteredRecord else
  if R.Id=xlr_VCENTER then FVCenter := R as TPrintCenteredRecord;

  if R.Id=xlr_Pane then FPane := R as TPaneRecord else
  if R.Id=xlr_GUTS then FGuts:=R as TGutsRecord else
  if R.Id=xlr_REFMODE then FRefMode:=R as TRefModeRecord;
  if R.Id=xlr_AUTOFILTERINFO then FAutoFilterInfo := R as TAutoFilterInfoRecord;

end;

function TSheet.TotalRangeSize(const SheetIndex: integer;
  const CellRange: TXlsCellRange): int64;
begin
  Result:= inherited TotalRangeSize(SheetIndex, CellRange);
  if (FCodeName<>nil) then inc(Result, FCodeName.TotalSize);
end;

function TSheet.TotalSize: int64;
begin
  Result:= inherited TotalSize;
  if (FCodeName<>nil) then inc(Result, FCodeName.TotalSize);
end;

function TSheet.GetCodeName: UTF16String;
begin
  if FCodeName<>nil then Result:=FCodeName.SheetName else Result:='';
end;

procedure TSheet.SetCodeName(const Value: UTF16String);
begin
  FreeAndNil(FCodeName);
  FCodeName:=TCodeNameRecord.CreateNew(Value);
end;


procedure TSheet.Clear;
begin
  inherited;
  //don't FWorkbookGlobals:=nil;
  FWindow2:=nil;
  FPageHeader:=nil;
  FPageFooter:=nil;
  FPrintGridLines:=nil;
  FLeftMargin:=nil; FRightMargin:=nil; FTopMargin:=nil; FBottomMargin:=nil;
  FSetup:=nil;
  FPrinterDriverSettings:=nil;
  FWSBool:=nil;
  FHCenter := nil;
  FVCenter := nil;
  FZoom:=nil;
  FGuts:=nil;
  FRefMode:=nil;
  FPane:=nil;
  FAutoFilterInfo := nil;

  //don't FPrintRecords:=nil;
  //don't FCodeName:=nil;
end;

procedure TSheet.AddOrRemovePane(const Add: boolean);
begin

end;

procedure TSheet.RestoreObjectCoords;
begin

end;

{ TFlxChart }

procedure TFlxChart.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
begin
  FChartRecords.ArrangeCopyRowsAndCols(RowOffset, ColOffset);
end;

procedure TFlxChart.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  FChartRecords.ArrangeCopySheet(SheetInfo);
end;

procedure TFlxChart.InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
begin
  //Nothing, we never insert rows in a chart sheet
end;

procedure TFlxChart.DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);
begin
  //Nothing, we never delete rows in a chart sheet
end;

procedure TFlxChart.Clear;
begin
  inherited;
  if FChartRecords<>nil then FChartRecords.Clear;
  FreeAndNil(RemainingData);
  //We don't clear CodeName.
end;

function TFlxChart.DoCopyTo: TSheet;
begin
  Result:= inherited DoCopyTo;
  (Result as TFlxChart).FChartRecords.CopyFrom(FChartRecords);
  (Result as TFlxChart).FPrintRecords:=(Result as TFlxChart).FChartRecords;
  Result.FixCachePointers;
  (Result as TFlxChart).RemainingData := nil; //we will not copy this
end;

constructor TFlxChart.Create(const aWorkbookGlobals: TWorkbookGlobals);
begin
  inherited;
  FChartRecords:= TChartRecordList.Create;
  FPrintRecords:=FChartRecords;
end;

destructor TFlxChart.Destroy;
begin
  FreeAndNil(FChartRecords);
  FreeAndNil(FCodeName);
  FreeAndNil(RemainingData);
  inherited;
end;


procedure TFlxChart.LoadSubChart(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const SST: TSST; const Level: integer);
var
  R: TBaseRecord;
  RecordId: integer;
begin
  repeat
    RecordId := RecordHeader.Id;
    R:=LoadRecords(DataStream, RecordHeader);
    try
      if (Level = 0) then LoadCachePointers(R); //CachePointers are only on level 0, as they refer to the sheet

      if (R is TBofRecord) then raise Exception.Create(ErrExcelInvalid);
      if (R is TLabelSSTRecord) then (R as TLabelSSTRecord).AttachToSST(SST)
      else if (R is TIgnoreRecord) then FreeAndNil(R)
      else if (R is TDimensionsRecord) then begin; OriginalDimensions:=(R as TDimensionsRecord).Dim^; FreeAndNil(R);end
      else if (R is TEOFRecord) then sEOF:=(R as TEOFRecord)
      else if (R is TCodeNameRecord) then begin; FreeAndNil(FCodeName); FCodeName:=(R as TCodeNameRecord); end
      else if (R is TBeginRecord)then begin; FChartRecords.Add(R); LoadSubChart(DataStream, RecordHeader, SST, Level + 1); end

      else FChartRecords.Add(R) ;



      if (R<>nil) and (R.Id = xlr_EOF) then
      begin
        RemainingData := R.Continue;
        R.Continue := nil;
      end;

    except
      FreeAndNil(R);
      Raise;
    end; //Finally

  until (RecordId = xlr_EOF) or (RecordId = xlr_END);

end;

procedure TFlxChart.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TBOFRecord; const SST: TSST);
begin
  Clear;
  LoadSubChart(DataStream, RecordHeader, SST, 0);
  sBOF:=First; //Last statement
end;

procedure TFlxChart.SaveToStream(const DataStream: TOle2File);
begin
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);
  sBOF.SaveToStream(DataStream);
  FChartRecords.SaveToStream(DataStream);
  if (FCodeName<>nil) then FCodeName.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

function TFlxChart.TotalSize: int64;
begin
  Result:= inherited TotalSize+
    FChartRecords.TotalSize;
end;

procedure TFlxChart.SaveRangeToStream(const DataStream: TOle2File;
  const SheetIndex: integer; const CellRange: TXlsCellRange);
begin
  //Can't save a chart range
  SaveToStream(DataStream);
end;

function TFlxChart.TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64;
begin
  //Can't save a chart range
  Result:=TotalSize;
end;

procedure TFlxChart.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
begin
  FChartRecords.ArrangeInsertRowsAndCols( InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
end;

procedure TFlxChart.SetPageHeaderFooter(const P: TPageHeaderFooterRecord;
  const s: UTF16String);
var
  OldSize: integer;
begin
  if P=nil then exit;
  OldSize:=P.DataSize;
  P.Text:=s;
  FChartRecords.AdaptSize(P.DataSize-OldSize);
end;

procedure TFlxChart.AddZoomRecord;
begin
  FZoom:=FChartRecords[FChartRecords.Add(TSCLRecord.CreateFromData(100))] as TSCLRecord;
end;

procedure TFlxChart.FixCachePointers;
var
  i: integer;
begin
  inherited;
  for i:=0 to FChartRecords.Count-1 do
    LoadCachePointers(FChartRecords[i] as TBaseRecord);
end;

{ TChartList }

procedure TChartList.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer;
  const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  for i:=0 to Count -1 do Items[i].ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
end;

procedure TChartList.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveToStream(DataStream);
end;

{ TWorkSheet }

procedure TWorkSheet.Clear;
begin
  inherited;
  //Dont Clear CodeName
  if FRanges<>nil then FRanges.Clear;
  if FCells<>nil then FCells.Clear;
  if FNotes<>nil then FNotes.Clear;
  if FColumns<>nil then FColumns.Clear;
  if FHLinks<>nil then FHLinks.Clear;
  //FDrawing should be freed after notes
  if FDrawing<>nil then FDrawing.Clear;
  if FVPageBreaks<>nil then FVPageBreaks.Clear;
  if FHPageBreaks<>nil then FHPageBreaks.Clear;
  if FMiscRecords1<>nil then FMiscRecords1.Clear;
  if FMiscRecords2<>nil then FMiscRecords2.Clear;
end;

procedure TWorkSheet.AddOrRemovePane(const Add: Boolean);
var
  i: integer;
  PaneData: PArrayOfByte;
  DataSize: integer;
begin
  if Add then
  begin
    if FPane = nil then
      if FMiscRecords2.Count > 1 then
      begin
        DataSize := 10;
        GetMem(PaneData, DataSize);
        FillChar(PaneData[0], DataSize, 0);
        try
          FPane := TPaneRecord.Create(xlr_PANE, PaneData, DataSize);
          FMiscRecords2.Insert(1, FPane);
        except
          FreeMem(PaneData);
          FPane := nil;
          raise;
        end; //Except
      end
  end
  else
  begin
    if FPane = nil then exit;

    for i := FMiscRecords2.Count - 1 downto 0 do
    if FMiscRecords2[i] = FPane then
    begin
      FMiscRecords2.Delete(i);
      FPane := nil;
      exit;
    end;
  end;
end;


function TWorkSheet.DoCopyTo: TSheet;
begin
  Result:= inherited DoCopyTo;
  (Result as TWorkSheet).FMiscRecords1.CopyFrom(FMiscRecords1);
  (Result as TWorkSheet).FMiscRecords2.CopyFrom(FMiscRecords2);
  (Result as TWorkSheet).FHPageBreaks.CopyFrom(FHPageBreaks);
  (Result as TWorkSheet).FVPageBreaks.CopyFrom(FVPageBreaks);
  (Result as TWorkSheet).FDrawing.CopyFrom(FDrawing, self);
  (Result as TWorkSheet).FCells.CopyFrom(FCells);
  (Result as TWorkSheet).FRanges.CopyFrom(FRanges);
  (Result as TWorkSheet).FNotes.CopyFrom(FNotes);
  (Result as TWorkSheet).FColumns.CopyFrom(FColumns);
  (Result as TWorkSheet).FHLinks.CopyFrom(FHLinks);

  (Result as TWorkSheet).FNotes.FixDwgIds((Result as TWorkSheet).FDrawing);

  (Result as TWorkSheet).FDefColWidth:=FDefColWidth;
  (Result as TWorkSheet).FDefRowHeight:=FDefRowHeight;
  Result.FixCachePointers;
end;

constructor TWorkSheet.Create(const aWorkbookGlobals: TWorkbookGlobals);
begin
  inherited;
  FMiscRecords1:= TBaseRecordList.Create;
  FMiscRecords2:= TBaseRecordList.Create;
  FHPageBreaks:=THPageBreakList.Create;
  FVPageBreaks:=TVPageBreakList.Create;
  FDrawing:= TDrawing.Create(FWorkbookGlobals.DrawingGroup);
  FColumns:= TColInfoList.Create;
  FCells:= TCells.Create(aWorkbookGlobals, FColumns);
  FRanges :=TRangeList.Create;
  FNotes:= TNoteList.Create;
  FHLinks:= THLinkList.Create;

  FPrintRecords:=FMiscRecords1;

  FDefRowHeight:=$FF;
  FDefColWidth:=$0A*DefColWidthAdapt;
end;

procedure TWorkSheet.DoCreateFromData(const SST: TSST);
var
  P: Pointer;
  H: THandle;
  MemStream: TMemoryStream;
  DataStream : TOle2File;
  R: TBaseRecord;
  BOF: TBOFRecord;
  RId: integer;
  RecordHeader: TRecordHeader;
begin
  H:=FindResource(HINSTANCE, RESOURCE_EMPTYSHEET, RT_RCDATA);
  try
    P:=LockResource(LoadResource(HINSTANCE, H));
    try
      MemStream:=TMemoryStream.Create;
      try
        MemStream.Write(P^, SizeofResource(HINSTANCE, H));
        MemStream.Position:=0;
        DataStream := TOle2File.Create(MemStream);
        try
          DataStream.SelectStream(WorkbookStrS);
          //skip workbook
          DataStream.ReadMem(RecordHeader, sizeof(RecordHeader)); //initialize the first time.
          repeat
            R := LoadRecords(DataStream, RecordHeader);
            try
              RId := R.Id;
            finally
              FreeAndNil(R);
            end;
          until RId = xlr_EOF;

          R := LoadRecords(DataStream, RecordHeader);
          try
            if (R is TBOFRecord) then
            begin
              BOF := TBOFRecord(R);
              LoadFromStream(DataStream, RecordHeader, BOF, SST);
            end
            else raise Exception.Create(ErrExcelInvalid);
          except
             FreeAndNil(R);
             raise;
          end;
        finally
          FreeAndNil(DataStream);
        end; //finally
      finally
        FreeAndNil(MemStream);
      end; //finally

    finally
      //UnlockResource(P); obsolete in win32
    end;
  finally
    //FreeResource(H); obsolete in win32
  end;
end;

constructor TWorkSheet.CreateFromData(const aWorkbookGlobals: TWorkbookGlobals; const SST: TSST);
begin
  Create(aWorkbookGlobals);
  DoCreateFromData(SST);
end;

destructor TWorkSheet.Destroy;
begin
  FreeAndNil(FRanges);
  FreeAndNil(FCells);
  FreeAndNil(FNotes);
  FreeAndNil(FColumns);
  FreeAndNil(FHLinks);
  //FDrawing should be freed after notes
  FreeAndNil(FDrawing);
  FreeAndNil(FVPageBreaks);
  FreeAndNil(FHPageBreaks);
  FreeAndNil(FMiscRecords1);
  FreeAndNil(FMiscRecords2);
  FreeAndNil(FCodeName);
  inherited;
end;

procedure TWorkSheet.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TBOFRecord; const SST: TSST);
var
  R: TBaseRecord;
  MiscRecords: TBaseRecordList;
  FShrFmlas: TShrFmlaRecordList;
  LastFormula: TFormulaRecord;
  HasStandardWidthRec: boolean;
  StdW: integer;
  RecordId: integer;
begin
  Clear;
  MiscRecords:=FMiscRecords1;
  FShrFmlas:= TShrFmlaRecordList.Create;
  LastFormula:=nil;
  HasStandardWidthRec:=false;
  StdW:=8;
  try
    repeat
      RecordId := RecordHeader.Id;
      R:=LoadRecords(DataStream, RecordHeader);
      try
        if RecordId=xlr_WINDOW2 then
        begin
          MiscRecords:=FMiscRecords2;
          FWindow2:=R as TWindow2Record;
        end;

        LoadCachePointers(R);

        if (R is TFormulaRecord) then LastFormula:=R as TFormulaRecord;

        //It looks like standardwidth is used <-> DefColwidth=8
        if (R is TDefColWidthRecord) and (not HasStandardWidthRec or ((R as TDefColWidthRecord).Width<>8)) then begin; StdW:=(R as TDefColWidthRecord).Width;FDefColWidth:= StdW*DefColWidthAdapt;end;
        if (R is TStandardWidthRecord) and (StdW=8) then begin HasStandardWidthRec:=true; FDefColWidth:= (R as TStandardWidthRecord).Width;end;
        if (R is TDefRowHeightRecord) then FDefRowHeight:= (R as TDefRowHeightRecord).Height;

        if (R is TLabelSSTRecord) then (R as TLabelSSTRecord).AttachToSST(SST);
        if (R is TBofRecord) then raise Exception.Create(ErrExcelInvalid)
        else if (R is TDrawingRecord) then FDrawing.LoadFromStream(DataStream, RecordHeader, R as TDrawingRecord, SST)
        else if (R is TIgnoreRecord) then FreeAndNil(R)
        else if (R is TDimensionsRecord) then begin; OriginalDimensions:=(R as TDimensionsRecord).Dim^; FreeAndNil(R);end
        else if (R is TNoteRecord) then FNotes.AddRecord(R as TNoteRecord, (R as TNoteRecord).Row)
        else if (R is TColInfoRecord) then FColumns.AddRecord(R as TColInfoRecord)
        else if (R is TCellRecord) then FCells.AddCell(R as TCellRecord, (R as TCellRecord).Row)
        else if (R is TMultipleValueRecord) then begin FCells.AddMultipleCells(R as TMultipleValueRecord);FreeAndNil(R);end
        else if (R is TRowRecord) then FCells.AddRow(R as TRowRecord)
        else if (R is TCondFmtRecord) then FRanges[FRanges.Add(TCondFmt.Create)].LoadFromStream(DataStream, RecordHeader, R as TCondFmtRecord)
        else if (R is TCellMergingRecord) then FRanges[FRanges.Add(TMergedCells.Create)].LoadFromStream(DataStream, RecordHeader, R as TCellMergingRecord)
        else if (R is THLinkRecord) then FHLinks.Add(R as THLinkRecord)
        else if (R is TShrFmlaRecord) then begin if LastFormula=nil then raise Exception.Create(ErrExcelInvalid) else begin; (R as TShrFmlaRecord).Key:=LastFormula.Row+LastFormula.Column shl 16; FShrFmlas.Add(R as TShrFmlaRecord);end end
        else if (R is THPageBreakRecord) then FHPageBreaks.AddRecord(R as THPageBreakRecord)
        else if (R is TVPageBreakRecord) then FVPageBreaks.AddRecord(R as TVPageBreakRecord)
        else if (R is TStringRecord) then begin if LastFormula=nil then raise Exception.Create(ErrExcelInvalid) else LastFormula.SetFormulaValue((R as TStringRecord).Value);FreeAndNil(R);end
        else if (R is TArrayRecord) then begin if LastFormula=nil then raise Exception.Create(ErrExcelInvalid) else LastFormula.ArrayRecord:=R as TArrayRecord;end
        else if (R is TEOFRecord) then sEOF:=(R as TEOFRecord)
        else if (R is TCodeNameRecord) then begin; FreeAndNil(FCodeName); FCodeName:=(R as TCodeNameRecord); end
        else MiscRecords.Add(R) ;

      except
        FreeAndNil(R);
        Raise;
      end; //Finally

    until RecordId = xlr_EOF;

    FNotes.FixDwgIds(FDrawing);
    FCells.CellList.FixFormulas(FShrFmlas);
		FDrawing.SaveObjectCoords(self);
  finally
    FreeAndNil(FShrFmlas);
  end; //finally

  FCells.FixRows; //if the file was invalid.

  //this must be the last statment, so if there is an exception, we dont take First
  sBOF:= First;
end;

procedure TWorkSheet.SaveToStream(const DataStream: TOle2File);
begin
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);
  sBOF.SaveToStream(DataStream);

  if (FGuts<>nil) and FGuts.RecalcNeeded then
  begin
    FCells.RowList.CalcGuts(FGuts);
    FColumns.CalcGuts(FGuts);
    FGuts.RecalcNeeded:=false;
  end;

  FMiscRecords1.SaveToStream(DataStream);
  FHPageBreaks.SaveToStream(DataStream);
  FVPageBreaks.SaveToStream(DataStream);
  FColumns.SaveToStream(DataStream);
  FCells.SaveToStream(DataStream);
  FDrawing.SaveToStream(DataStream);
  FNotes.SaveToStream(DataStream);
  FMiscRecords2.SaveToStream(DataStream);
  FRanges.SaveToStream(DataStream);
  FHLinks.SaveToStream(DataStream);
  if (FCodeName<>nil) then FCodeName.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

procedure TWorkSheet.SaveRangeToStream(const DataStream: TOle2File;
  const SheetIndex: integer; const CellRange: TXlsCellRange);
begin
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);
  sBOF.SaveToStream(DataStream);
  FMiscRecords1.SaveToStream(DataStream);
  FHPageBreaks.SaveRangeToStream(DataStream, CellRange);
  FVPageBreaks.SaveRangeToStream(DataStream, CellRange);
  FColumns.SaveRangeToStream(DataStream, CellRange);
  FCells.SaveRangeToStream(DataStream, CellRange);
  //Excel doesnt save drawings to the clipboard
  //FDrawing.SaveToStream(DataStream);
  FNotes.SaveRangeToStream(DataStream, CellRange);
  FMiscRecords2.SaveToStream(DataStream);
  FRanges.SaveRangeToStream(DataStream, CellRange);
  FHLinks.SaveRangeToStream(DataStream, CellRange);
  if (FCodeName<>nil) then FCodeName.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

procedure TWorkSheet.InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
var
  r: TXlsCellRange;
begin
  if (aRowCount>0) then
  begin
    FCells.InsertAndCopyRows(FirstRow, LastRow, DestRow, aRowCount, SheetInfo, OnlyFormulas);
    FRanges.InsertAndCopyRowsOrCols(FirstRow, LastRow, DestRow, aRowCount, SheetInfo, false);
    FNotes.InsertAndCopyRows(FirstRow, LastRow, DestRow, aRowCount, SheetInfo, false);
    FHPageBreaks.InsertRows(DestRow, aRowCount);

    r.Left:=0;r.Right:=Max_Columns;r.Top:=FirstRow; r.Bottom:=LastRow;
    FHLinks.InsertAndCopyRange(r, DestRow, 0, aRowCount, 0, SheetInfo);
  end;
  if (aColCount>0) then
  begin
    FCells.InsertAndCopyCols(FirstCol, LastCol, DestCol, aColCount, SheetInfo, OnlyFormulas);
    FRanges.InsertAndCopyRowsOrCols(FirstCol, LastCol, DestCol, aColCount, SheetInfo, true);
    FNotes.InsertAndCopyCols(FirstCol, LastCol, DestCol, aColCount, SheetInfo, false);
    FVPageBreaks.InsertCols(DestCol, aColCount);
    r.Top:=0;r.Bottom:=Max_Rows;r.Left:=FirstCol; r.Right:=LastCol;
    FHLinks.InsertAndCopyRange(r, 0, DestCol, 0, aColCount, SheetInfo);
  end;
  FDrawing.InsertAndCopyRowsAndCols(FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount, SheetInfo, self);
end;

procedure TWorkSheet.DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);
var
  r: TXlsCellRange;
begin
  if (aRowCount>0) then
  begin
    FCells.DeleteRows(aRow, aRowCount, SheetInfo);
    FDrawing.DeleteRows(aRow, aRowCount, SheetInfo, self);
    FRanges.DeleteRowsOrCols(aRow, aRowCount, SheetInfo, false);
    FNotes.DeleteRows(aRow, aRowCount, SheetInfo);
    FHPageBreaks.DeleteRows(aRow, aRowCount);
    r.Left:=0;r.Right:=Max_Columns;r.Top:=aRow; r.Bottom:=aRow+aRowCount;
    FHLinks.DeleteRange(r, 1, 0, SheetInfo);
  end;
  if (aColCount>0) then
  begin
    FCells.DeleteCols(aCol, aColCount, SheetInfo);
    FDrawing.DeleteCols(aCol, aColCount, SheetInfo, self);
    FRanges.DeleteRowsOrCols(aCol, aColCount, SheetInfo, true);
    FNotes.DeleteCols(aCol, aColCount, SheetInfo);
    FVPageBreaks.DeleteCols(aCol, aColCount);
    r.Top:=0;r.Bottom:=Max_Rows;r.Left:=aCol; r.Right:=aCol+aColCount;
    FHLinks.DeleteRange(r, 0, 1, SheetInfo);
  end;

end;


procedure TWorkSheet.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
begin
  //PENDING: Optimize this
  FCells.ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
  FDrawing.ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo, self);
end;



procedure TWorkSheet.AssignDrawing(const Index: integer; const Data: ByteArray;
  const DataType: TXlsImgTypes);
begin
  FDrawing.AssignDrawing( Index, Data, DataType);
end;

procedure TWorkSheet.GetDrawingFromStream(const Index: integer; const Data: TStream;
  var DataType: TXlsImgTypes);
begin
  FDrawing.GetDrawingFromStream( Index, Data, DataType);
end;

function TWorkSheet.DrawingCount: integer;
begin
  Result:= FDrawing.DrawingCount;
end;

function TWorkSheet.GetDrawingRow(index: integer): integer;
begin
  Result:= FDrawing.DrawingRow[index];
end;

function TWorkSheet.GetDrawingName(index: integer): UTF16String;
begin
  Result:= FDrawing.DrawingName[index];
end;

procedure TWorkSheet.DeleteHPageBreak(const aRow: word);
begin
  inherited;
  FHPageBreaks.DeleteBreak(aRow);
end;

procedure TWorkSheet.DeleteVPageBreak(const aCol: word);
begin
  inherited;
  FVPageBreaks.DeleteBreak(aCol);
end;

procedure TWorkSheet.InsertHPageBreak(const aRow: word);
begin
  inherited;
  FHPageBreaks.AddBreak(aRow);
end;

procedure TWorkSheet.InsertVPageBreak(const aCol: word);
begin
  inherited;
  FVPageBreaks.AddBreak(aCol);
end;

procedure TWorkSheet.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  inherited;
  FCells.ArrangeInsertSheet(SheetInfo);
  FDrawing.ArrangeCopySheet(SheetInfo);
end;

function TWorkSheet.GetColWidth(const aCol: Word): integer;
var
  index: integer;
begin
  if not FColumns.Find(aCol, Index) then Result:=DefColWidth else Result:=FColumns[Index].Width;
end;

function TWorkSheet.GetColWidth(const aCol: Word; const HiddenIsZero: boolean): integer;
var
  index: integer;
begin
  if not FColumns.Find(aCol, Index) then Result:=DefColWidth
  else
  begin
    if HiddenIsZero and (FColumns[Index].Options and $01 = $01) then Result := 0
      else Result:=FColumns[Index].Width;
  end;
end;

function TWorkSheet.GetRowHeight(const aRow: integer): integer;
begin
  if not FCells.RowList.HasRow(aRow) then Result:=DefRowHeight else
  Result:= FCells.RowList.RowHeight(aRow);
end;

function TWorkSheet.GetRowHeight(const aRow: integer; const HiddenIsZero: boolean): integer;
begin
  if not FCells.RowList.HasRow(aRow) then Result:=DefRowHeight
  else
  begin
    if HiddenIsZero and FCells.RowList[aRow].IsHidden then Result := 0
      else Result:=FCells.RowList.RowHeight(aRow);
  end;
end;


procedure TWorkSheet.SetColWidth(const aCol: Word; const Value: integer);
var
  Index: integer;
begin
  if FColumns.Find(aCol, Index) then
    FColumns[Index].Width:=Value
  else
    FColumns.Insert(Index, TColInfo.Create(aCol, Value, 15, 0));
end;

procedure TWorkSheet.SetRowHeight(const aRow, Value: integer);
begin
  FCells.RowList.SetRowHeight(aRow, Value);
end;

function TWorkSheet.GetColHidden(const aCol: Word): boolean;
var
  index: integer;
begin
  if not FColumns.Find(aCol, Index) then Result:=false else Result:=FColumns[Index].Options and $1 = $1;
end;

function TWorkSheet.GetRowHidden(const aRow: integer): boolean;
begin
  if not FCells.RowList.HasRow(aRow) then Result:=false else
  Result:= FCells.RowList[aRow].IsHidden;
end;

procedure TWorkSheet.SetColHidden(const aCol: Word; const Value: boolean);
var
  Index: integer;
begin
  if FColumns.Find(aCol, Index) then
  begin
    if Value then
    begin
      FColumns[Index].Options:=FColumns[Index].Options or $1;
    end else
    begin
      if FColumns[Index].Width=0 then FColumns[Index].Width:=DefColWidth;
      FColumns[Index].Options:=FColumns[Index].Options and not $1;
    end;
  end
  else
    if Value then
      FColumns.Insert(Index, TColInfo.Create(aCol, DefColWidth, 15, $1));
end;

procedure TWorkSheet.SetRowHidden(const aRow: integer;const Value: boolean);
begin
  FCells.RowList.AddRow(aRow);
  FCells.RowList[aRow].Hide(Value);
end;


function TWorkSheet.GetColFormat(const aCol: integer): integer;
var
  index: integer;
begin
  if not FColumns.Find(aCol, Index) then Result:=-1 else Result:=FColumns[Index].XF;
end;

function TWorkSheet.GetRowFormat(const aRow: integer): integer;
begin
  if not FCells.RowList.HasRow(aRow) or not FCells.RowList[aRow].IsFormatted then Result:=-1 else
  Result:= FCells.RowList[aRow].XF;
end;

procedure TWorkSheet.SetColFormat(const aCol: integer; const Value: integer);
var
  Index: integer;
  i: integer;
begin
  if FColumns.Find(aCol, Index) then
    FColumns[Index].XF:=Value
  else
    FColumns.Insert(Index, TColInfo.Create(aCol, DefColWidth, Value, 0));

  //Reset all cells in column to format XF
  for i:=0 to FCells.CellList.Count-1 do
    if FCells.CellList[i].Find(aCol, Index) then FCells.CellList[i][Index].XF:=Value;
end;

procedure TWorkSheet.SetRowFormat(const aRow, Value: integer);
var
  i: integer;
begin
  FCells.RowList.AddRow(aRow);
  FCells.RowList[aRow].XF:= Value;

  //Reset all cells in row to format XF
  if(aRow>=0) and (aRow<FCells.CellList.Count) then
    for i:=0 to FCells.CellList[aRow].Count-1 do FCells.CellList[aRow][i].XF:=Value;

end;

function TWorkSheet.GetAnchor(const Index: integer): TClientAnchor;
begin
  Result:= FDrawing.GetAnchor(Index);
end;

procedure TWorkSheet.SetAnchor(const Index: integer; const aAnchor: TClientAnchor);
begin
  FDrawing.SetAnchor(Index, aAnchor, self);
end;


function TWorkSheet.CellMergedBounds(const aRow, aCol: integer): TXlsCellRange;
var
  i: integer;
begin
  //Find the cell into the MergedCells array
  Result.Left:=aCol;
  Result.Right:=aCol;
  Result.Top:=aRow;
  Result.Bottom:=aRow;
  for i:=0 to FRanges.Count-1 do
    if FRanges[i] is TMergedCells then
      if (FRanges[i] as TMergedCells).CheckCell(aRow, aCol, Result) then exit;
end;

function TWorkSheet.CellMergedList(const index: integer): TXlsCellRange;
var
  i, p, k: integer;
begin
  //Find the cell into the MergedCells array
  Result.Left:=0;
  Result.Right:=0;
  Result.Top:=0;
  Result.Bottom:=0;
  if index<0 then exit;
  p:=0;
  for i:=0 to FRanges.Count-1 do
    if FRanges[i] is TMergedCells then
    begin
      k:=(FRanges[i] as TMergedCells).MergedCount;
      if index<p+k then
      begin
        Result:=(FRanges[i] as TMergedCells).MergedCell(index-p);
        exit;
      end;
      inc (p, k);
    end;
end;

function TWorkSheet.CellMergedListCount: integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to FRanges.Count-1 do
    if FRanges[i] is TMergedCells then
      inc(Result, (FRanges[i] as TMergedCells).MergedCount);
end;

procedure TWorkSheet.UnMergeCells(aRow1, aCol1, aRow2, aCol2: integer);
var
  x: integer;
  i: integer;
begin
  if aRow1>aRow2 then begin x:=aRow2;aRow2:=aRow1; aRow1:=x;end;
  if aCol1>aCol2 then begin x:=aCol2;aCol2:=aCol1; aCol1:=x;end;

  for i:=0 to FRanges.Count-1 do
    if FRanges[i] is TMergedCells then
    begin
      (FRanges[i] as TMergedCells).UnMergeCells(aRow1, aCol1, aRow2, aCol2);
    end;
end;

procedure TWorkSheet.MergeCells(aRow1, aCol1, aRow2, aCol2: integer);
var
  x: integer;
  Mc: TMergedCells;
  i: integer;
  bRow1, bCol1, bRow2, bCol2: integer;
begin
  if aRow1>aRow2 then begin x:=aRow2;aRow2:=aRow1; aRow1:=x;end;
  if aCol1>aCol2 then begin x:=aCol2;aCol2:=aCol1; aCol1:=x;end;

  //We have to take all existing included merged cells

  Mc:=nil;
  repeat
    bRow1:=aRow1; bRow2:=aRow2; bCol1:=aCol1; bCol2:=aCol2;
    for i:=0 to FRanges.Count-1 do
      if FRanges[i] is TMergedCells then
      begin
        Mc:=(FRanges[i] as TMergedCells);
        Mc.PreMerge(aRow1, aCol1, aRow2, aCol2)
      end;
  until (aRow1=bRow1) and (aRow2=bRow2) and (aCol1=bCol1) and (aCol2=bCol2);

  if Mc=nil then Mc:=FRanges[FRanges.Add(TMergedCells.Create)] as TMergedCells;
  Mc.MergeCells(aRow1, aCol1, aRow2, aCol2);
end;


function TWorkSheet.TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64;
begin
  Result:= inherited TotalRangeSize(SheetIndex, CellRange)+
    FMiscRecords1.TotalSize +
    FHPageBreaks.TotalRangeSize(CellRange) +
    FVPageBreaks.TotalRangeSize(CellRange) +
    FCells.TotalRangeSize(CellRange)+
    FRanges.TotalRangeSize(CellRange) +
    FDrawing.TotalSize +
    FMiscRecords2.TotalSize+
    FNotes.TotalRangeSize(CellRange)+
    FColumns.TotalRangeSize(CellRange)+
    FHLinks.TotalRangeSize(CellRange);
end;

function TWorkSheet.TotalSize: int64;
begin
  Result := FixTotalSize(false);
end;

function TWorkSheet.FixTotalSize(const NeedsRecalc: boolean): int64;
begin
  Result:= inherited TotalSize+
    FMiscRecords1.TotalSize +
    FHPageBreaks.TotalSize +
    FVPageBreaks.TotalSize +
    FCells.FixTotalSize(NeedsRecalc) +
    FRanges.TotalSize +
    FDrawing.TotalSize +
    FMiscRecords2.TotalSize+
    FNotes.TotalSize+
    FColumns.TotalSize+
    FHLinks.TotalSize;
end;

procedure TWorkSheet.SetPageHeaderFooter(const P: TPageHeaderFooterRecord;
  const s: UTF16String);
var
  OldSize: integer;
begin
  if P=nil then exit;
  OldSize:=P.DataSize;
  P.Text:=s;
  FMiscRecords1.AdaptSize(P.DataSize-OldSize);
end;

function TWorkSheet.HasHPageBreak(const Row: integer): boolean;
begin
  Result:=FHPageBreaks.HasPageBreak(Row);
end;

function TWorkSheet.HasVPageBreak(const Col: integer): boolean;
begin
  Result:=FVPageBreaks.HasPageBreak(Col);
end;


function TWorkSheet.GetPrintNumberOfHorizontalPages: word;
begin
  if FSetup= nil then Result:=1 else
    Result:= FSetup.FitWidth;
end;

function TWorkSheet.GetPrintNumberOfVerticalPages: word;
begin
  if FSetup= nil then Result:=1 else
    Result:= FSetup.FitHeight;
end;

function TWorkSheet.GetPrintScale: integer;
begin
  if FSetup= nil then Result:=100 else
    Result:= FSetup.Scale;
end;

function TWorkSheet.GetPrintToFit: boolean;
begin
  if FWsBool= nil then Result:=false else
    Result:= FWsBool.FitToPage;
end;

function TWorkSheet.GetPrintVCentered: boolean;
begin
  if FVCenter = nil then Result := false
    else Result := FVCenter.Centered;
end;

function TWorkSheet.GetOutlineSummaryRowsBelowDetail: boolean;
begin
  if FWsBool= nil then Result:=true else
    Result:= FWsBool.OutlineSummaryRowsBelowDetail;
end;

function TWorkSheet.GetOutlineSummaryColsRightOfDetail: boolean;
begin
  if FWsBool= nil then Result:=true else
    Result:= FWsBool.OutlineSummaryColsRightOfDetail;
end;

function TWorkSheet.GetOutlineAutomaticStyles: boolean;
begin
  if FWsBool= nil then Result:=false else
    Result:= FWsBool.OutlineAutomaticStyles;
end;

procedure TWorkSheet.SetPrintNumberOfHorizontalPages(const Value: word);
begin
  if FSetup<>nil then FSetup.FitWidth:=Value;
end;

procedure TWorkSheet.SetPrintNumberOfVerticalPages(const Value: word);
begin
  if FSetup<>nil then FSetup.FitHeight:=Value;
end;

procedure TWorkSheet.SetPrintScale(const Value: integer);
begin
  if (Value<Low(Word))or (Value>High(Word)) then
    raise Exception.CreateFmt(ErrXlsIndexOutBounds, [Value, 'PrintScale', Low(Word), High(Word)]);
  if FSetup<>nil then FSetup.Scale:=Value;
end;

procedure TWorkSheet.SetPrintToFit(const Value: boolean);
begin
  if FWSBool<>nil then FWsBool.FitToPage:=value;
end;

procedure TWorkSheet.SetPrintVCentered(const Value: boolean);
begin
  if (FVCenter = nil) then exit;
  FVCenter.Centered := Value;
end;

procedure TWorkSheet.SetOutlineSummaryRowsBelowDetail(const value: boolean);
begin
  if FWsBool <> nil then FWsBool.OutlineSummaryRowsBelowDetail:=value;
end;

procedure TWorkSheet.SetOutlineSummaryColsRightOfDetail(const value: boolean);
begin
  if FWsBool <> nil then FWsBool.OutlineSummaryColsRightOfDetail:=value;
end;

procedure TWorkSheet.SetOutlineAutomaticStyles(const value: boolean);
begin
  if FWsBool <> nil then FWsBool.OutlineAutomaticStyles:=value;
end;

procedure TWorkSheet.AddImage(const Data: ByteArray; const DataType: TXlsImgTypes; const Properties: TImageProperties;const Anchor: TFlxAnchorType);
begin
  FDrawing.AddImage(Data, DataType, Properties, Anchor, self);
end;

procedure TWorkSheet.ClearImage(const Index: integer);
begin
  FDrawing.ClearImage(Index);
end;

procedure TWorkSheet.DeleteImage(const Index: integer);
begin
  FDrawing.DeleteImage(Index);
end;

procedure TWorkSheet.AddZoomRecord;
begin
  if FMiscRecords2.Count>1 then
  begin
    FMiscRecords2.Insert(1,TSCLRecord.CreateFromData(100));
    FZoom:=FMiscRecords2[1] as TSCLRecord;
  end;
end;

procedure TWorkSheet.AddNewComment(const Row, Col: integer;
  const Txt: UTF16String; const Properties: TImageProperties);
begin
  FNotes.AddNewComment(Row, Col, Txt, FDrawing, Properties, self);
end;


procedure TWorkSheet.ClearValues;
begin
  Clear;
  DoCreateFromData(FWorkbookGlobals.SST);
end;

function TWorkSheet.GetPrintOptions: word;
begin
  if FSetup= nil then Result:=0 else
    Result:= FSetup.PrintOptions;
end;

procedure TWorkSheet.SetPrintOptions(const Value: word);
begin
  if FSetup<>nil then FSetup.PrintOptions:=Value;
end;

procedure TWorkSheet.FixCachePointers;
var
  i: integer;
begin
  inherited;
  for i:=0 to FMiscRecords1.Count-1 do
    LoadCachePointers(FMiscRecords1[i] as TBaseRecord);
  for i:=0 to FMiscRecords2.Count-1 do
    LoadCachePointers(FMiscRecords2[i] as TBaseRecord);
end;


procedure TWorkSheet.FixRows;
begin
  inherited;
  Cells.FixRows;
end;

function TWorkSheet.GetColOutlineLevel(col: integer): integer;
var
  Index: integer;
begin
  if not FColumns.Find(col, Index) then
    Result:= 0
  else
    Result:= FColumns[Index].GetColOutlineLevel;
end;

function TWorkSheet.GetRowOutlineLevel(row: integer): integer;
begin
  if not FCells.RowList.HasRow(row) then
    Result:=0
  else
    Result:= FCells.RowList[row].Options and $07;
end;

procedure TWorkSheet.SetColOulineLevel(col: integer; const Value: integer);
var
  Index: integer;
begin
  EnsureGuts();
  FGuts.RecalcNeeded:=true;
  Index:=-1;
  if (not FColumns.Find(col, Index)) then
    FColumns.Insert(Index, TColInfo.Create(col, DefColWidth, 15, 0));

  FColumns[Index].SetColOutlineLevel(Value);
end;

procedure TWorkSheet.SetRowOulineLevel(row: integer; const Value: integer);
begin
  EnsureGuts();
  FGuts.RecalcNeeded:=true;
  FCells.RowList.AddRow(row);
  FCells.RowList[row].SetRowOutlineLevel(Value);
end;

procedure TWorkSheet.EnsureGuts;
var
  aPos: integer;
  i: integer;
  pdata: PArrayOfByte;
begin
  if (FGuts<>nil) then exit;

  aPos:=FMiscRecords1.Count;
  for i:=0 to FMiscRecords1.Count do
  begin
    if (FMiscRecords1[i] is TDefRowHeightRecord) then
    begin
      aPos:=i;
      break;
    end;
  end;

  GetMem(pdata, 0);
  FillChar(pdata^,8,0);
  FGuts:= TGutsRecord.Create(xlr_GUTS, pdata, 8);
  FMiscRecords1.Insert(aPos, FGuts);
end;

function TWorkSheet.GetPrintCopies: integer;
begin
  if FSetup= nil then Result:=1 else
    Result:= FSetup.PrintCopies;
end;

function TWorkSheet.GetPrinterDriverSettings: TPrinterDriverSettings;
begin
  if FPrinterDriverSettings = nil then
  begin
    Result.OperatingEnviroment:=0;
    SetLength(Result.Data, 0);
    exit;
  end;

  Result:=FPrinterDriverSettings.PrinterData;
end;

function TWorkSheet.GetPrintHCentered: boolean;
begin
  if FHCenter = nil then Result := false
    else Result := FHCenter.Centered;
end;

function TWorkSheet.GetPrintPaperSize: TExcelPaperSize;
begin
  if FSetup= nil then Result:=1 else
    Result:= FSetup.PrintPaperSize;
end;

function TWorkSheet.GetPrintXResolution: integer;
begin
  if FSetup= nil then Result:=600 else
    Result:= FSetup.PrintXResolution;
end;

function TWorkSheet.GetPrintYResolution: integer;
begin
  if FSetup= nil then Result:=600 else
    Result:= FSetup.PrintXResolution;
end;

procedure TWorkSheet.SetPrintCopies(const Value: integer);
begin
  if (Value<0) or (Value>$FFFF) then raise Exception.CreateFmt(ErrIndexOutBounds,[Value,'PrintCopies',0,$FFFF]);
  if FSetup<>nil then FSetup.PrintCopies:=Value;
end;

procedure TWorksheet.AddPrinterDriverRecord(const aPlsRecord:TPlsRecord; const FRecords: TBaseRecordList);
var
  idx,i: integer;
begin
  if FPrinterDriverSettings<>nil then RemovePrinterDriverRecord;
  idx:=FRecords.Count;
  if FSetup<>nil then
  for i:=0 to FRecords.Count-1 do
    if (FRecords[i]=FSetup) then
    begin
      idx:=i;
      break;
    end;

    FRecords.Insert(idx, aPlsRecord);
    FPrinterDriverSettings:=aPlsRecord;
end;

procedure TWorksheet.RemovePrinterDriverRecord;
begin
  RemovePrinterDriverRecord(FMiscRecords1);
  RemovePrinterDriverRecord(FMiscRecords1);
end;

procedure TWorksheet.RemovePrinterDriverRecord(const FRecords: TBaseRecordList);
var
  i: integer;
begin
  for i:=FRecords.Count-1 downto 0 do
    if FRecords[i] is TPlsRecord then
      FRecords.Delete(i);
end;

procedure TWorkSheet.SetPrinterDriverSettings(
  const Value: TPrinterDriverSettings);
begin
  if Length(value.Data) = 0 then
    RemovePrinterDriverRecord
  else
    begin
      AddPrinterDriverRecord(TPlsRecord.CreateFromData(Value), FMiscRecords1);
      PrintOptions:=PrintOptions and not word($4);
      //Not needed. If you want to copy the PaperSize, just use PaperSize property.
      //PaperSize=(TPaperSize)BitOps.GetWord(aData, 80);
    end;
end;

procedure TWorkSheet.SetPrintHCentered(const Value: boolean);
begin
  if (FHCenter = nil) then exit;
  FHCenter.Centered := Value;
end;

procedure TWorkSheet.SetPrintPaperSize(const Value: TExcelPaperSize);
begin
  if (Value<0) or (Value>$FFFF) then raise Exception.CreateFmt(ErrIndexOutBounds,[Value,'PrintPaperSize',0,$FFFF]);
  if FSetup<>nil then FSetup.PrintPaperSize:=Value;
end;

procedure TWorkSheet.SetPrintXResolution(const Value: integer);
begin
  if (Value<0) or (Value>$FFFF) then raise Exception.CreateFmt(ErrIndexOutBounds,[Value,'PrintXResolution',0,$FFFF]);
  if FSetup<>nil then FSetup.PrintXResolution:=Value;
end;

procedure TWorkSheet.SetPrintYResolution(const Value: integer);
begin
  if (Value<0) or (Value>$FFFF) then raise Exception.CreateFmt(ErrIndexOutBounds,[Value,'PrintYResolution',0,$FFFF]);
  if FSetup<>nil then FSetup.PrintYResolution:=Value;
end;

procedure TWorkSheet.RestoreObjectCoords;
begin
	FDrawing.RestoreObjectCoords(Self);
end;

procedure TWorkSheet.RecalcRowHeights(const Workbook: pointer; const Row1, Row2: Integer; const Forced: Boolean; const KeepAutoFit: Boolean; const Adjustment: extended);
begin
  Cells.CellList.RecalcRowHeights(Workbook, Row1, Row2, Forced, KeepAutoFit, Adjustment);
end;

procedure TWorkSheet.RecalcColWidths(const Workbook: pointer; const Col1, Col2: Integer; const IgnoreStrings: Boolean; const Adjustment: extended);
begin
  Cells.CellList.RecalcColWidths(Workbook, Col1, Col2, IgnoreStrings, Adjustment);
end;

procedure TWorkSheet.SetAutoFilter(const SheetIndex: Int32; const Row: Int32; const Col1: Int32; const Col2: Int32);
var
  aCount: Int32;
  aPos: Int32;
  i: Int32;
  myData: PArrayOfByte;
  AutoFilterName: TXlsNamedRange;
begin
  if Col2 < Col1 then
  begin
    RemoveAutoFilter;
    exit;
  end;

  if FAutoFilterInfo = nil then
  begin
    aCount := FMiscRecords1.Count;
    aPos := aCount;
    for i := 0 to aCount - 1 do
    begin
      if (FMiscRecords1[i] is TDefColWidthRecord) or (FMiscRecords1[i] is TDimensionsRecord) then
      begin
        aPos := i;
        break;
      end;
    end;


    GetMem(myData, 2);
    try
      FillChar(MyData^, 2, 0);
      FAutoFilterInfo := TAutoFilterInfoRecord.Create(xlr_AutoFilterINFO, myData, 2);
    Except
      FreeAndNil(MyData);
      raise;
    end;
    FMiscRecords1.Insert(aPos, FAutoFilterInfo);
  end;

  FAutoFilterInfo.DropDownCount := Abs((Col2 - Col1) + 1);
  FDrawing.RemoveAutoFilter;
  FDrawing.AddAutoFilter(Row, Col1, Col2, Self);
  InitializeNamedRange(AutoFilterName);
  AutoFilterName.Name := InternalNameRange_Filter_DataBase;
  AutoFilterName.NameSheetIndex := SheetIndex;
  AutoFilterName.OptionFlags := $21;

  AutoFilterName.RangeFormula := '=$' + EncodeColumn(Col1 + 1) + '$' + IntToStr(Row+ 1)+ ':$' + EncodeColumn(Col2+1) + '$' + IntToStr(Row + 1);
  FWorkbookGlobals.AddName(AutoFilterName, Cells.CellList);
end;

procedure TWorkSheet.RemoveAutoFilter();
var
  i: Int32;
  r: TBaseRecord;
begin
  for i := FMiscRecords1.Count - 1 downto 0 do
  begin
    r:=nil;
    if (FMiscRecords1[i] is TBaseRecord) then r := TBaseRecord(FMiscRecords1[i]);
    if (r <> nil) and (((r.Id = xlr_AutoFilter) or (r.Id = xlr_AutoFilterINFO)) or (r.Id = xlr_FILTERMODE)) then
    begin
      FMiscRecords1.Delete(i);
    end;
  end;

  FAutoFilterInfo := nil;
  FDrawing.RemoveAutoFilter;
end;

function TWorkSheet.HasAutoFilter(): Boolean;
begin
  Result := FAutoFilterInfo <> nil;
end;

function TWorkSheet.HasAutoFilter(const sheet: Int32; const row: Int32; const col: Int32): Boolean;
var
  R: TXlsCellRange;
begin
  if FAutoFilterInfo = nil then
    begin Result := false; exit; end;

  R := GetAutoFilterRange(sheet);

  Result := (((row >= R.Top) and (row <= R.Bottom)) and (col >= R.Left)) and (col <= R.Right);
end;

function TWorkSheet.GetAutoFilterRange(const Sheet: Int32): TXlsCellRange;
var
  Name: TNameRecord;
begin
  if FAutoFilterInfo = nil then
    begin Result.Left:= -1; Result.Top := -1; Result.Right := -1; Result.Bottom := -1; exit; end;

  Name := FWorkbookGlobals.GetName(Sheet, InternalNameRange_Filter_DataBase);
  if Name = nil then
    begin Result.Left := -1; Result.Top := -1; Result.Right := -1; Result.Bottom := -1; exit; end;

  Result.Top := Name.GetR1;
  Result.Left := Name.GetC1;
  Result.Bottom := Name.GetR2;
  Result.Right := Name.GetC2;
end;



{ TFlxUnsupportedSheet }

procedure TFlxUnsupportedSheet.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
end;

procedure TFlxUnsupportedSheet.InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount,FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
begin
end;

procedure TFlxUnsupportedSheet.DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);
begin
end;

procedure TFlxUnsupportedSheet.Clear;
begin
  inherited;
  //Dont Clear FCodeName
  if FSheetRecords<>nil then FSheetRecords.Clear;
end;

function TFlxUnsupportedSheet.DoCopyTo: TSheet;
begin
  Result:= inherited DoCopyTo;
  (Result as TFlxUnsupportedSheet).FSheetRecords.CopyFrom(FSheetRecords);
  Result.FixCachePointers;
end;

constructor TFlxUnsupportedSheet.Create(const aWorkbookGlobals: TWorkbookGlobals);
begin
  inherited;
  FSheetRecords:= TSheetRecordList.Create;
  FPrintRecords:=FSheetRecords;
end;

destructor TFlxUnsupportedSheet.Destroy;
begin
  FreeAndNil(FSheetRecords);
  FreeAndNil(FCodeName);
  inherited;
end;

procedure TFlxUnsupportedSheet.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TBOFRecord; const SST: TSST);
var
  R: TBaseRecord;
  RecordId: integer;
begin
  Clear;
  repeat
    RecordId := RecordHeader.Id;
    R:=LoadRecords(DataStream, RecordHeader);
    try
      LoadCachePointers(R);

      if (R is TLabelSSTRecord) then (R as TLabelSSTRecord).AttachToSST(SST);
      if (R is TBofRecord) then raise Exception.Create(ErrExcelInvalid)
      else if (R is TIgnoreRecord) then FreeAndNil(R)
      else if (R is TDimensionsRecord) then begin; OriginalDimensions:=(R as TDimensionsRecord).Dim^; FreeAndNil(R);end
      else if (R is TEOFRecord) then sEOF:=(R as TEOFRecord)
      else if (R is TCodeNameRecord) then begin; FreeAndNil(FCodeName); FCodeName:=(R as TCodeNameRecord); end
      else FSheetRecords.Add(R) ;
    except
      FreeAndNil(R);
      Raise;
    end; //Finally

  until RecordId = xlr_EOF;
  sBOF:=First; //Last statement
end;

procedure TFlxUnsupportedSheet.SaveToStream(const DataStream: TOle2File);
begin
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);
  sBOF.SaveToStream(DataStream);
  FSheetRecords.SaveToStream(DataStream);
  if (FCodeName<>nil) then FCodeName.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

function TFlxUnsupportedSheet.TotalSize: int64;
begin
  Result:= inherited TotalSize+
    FSheetRecords.TotalSize;
end;

procedure TFlxUnsupportedSheet.SaveRangeToStream(const DataStream: TOle2File;
  const SheetIndex: integer; const CellRange: TXlsCellRange);
begin
  //Can't save a range
  SaveToStream(DataStream);
end;

function TFlxUnsupportedSheet.TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64;
begin
  //Can't save a range
  Result:=TotalSize;
end;

procedure TFlxUnsupportedSheet.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
begin
end;

procedure TFlxUnsupportedSheet.SetPageHeaderFooter(const P: TPageHeaderFooterRecord;
  const s: UTF16String);
var
  OldSize: integer;
begin
  if P=nil then exit;
  OldSize:=P.DataSize;
  P.Text:=s;
  FSheetRecords.AdaptSize(P.DataSize-OldSize);
end;

procedure TFlxUnsupportedSheet.AddZoomRecord;
begin
end;


procedure TFlxUnsupportedSheet.FixCachePointers;
var
  i: integer;
begin
  inherited;
  for i:=0 to FSheetRecords.Count-1 do
    LoadCachePointers(FSheetRecords[i] as TBaseRecord);
end;

end.
