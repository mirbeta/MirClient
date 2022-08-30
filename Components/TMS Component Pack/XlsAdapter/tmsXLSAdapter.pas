unit tmsXLSAdapter;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

//Note: Excel uses 1-Based arrays, and that's the interface we present to our users.
// but, TExcelWorkbook uses 0-Based arrays, to be consistent with the file format (made in C)
//So here we have to add and substract 1 everywere to be consistent.

interface
uses
  SysUtils, Classes,
  tmsUExcelAdapter, tmsXlsBaseTemplateStore, tmsUFlxMessages, tmsUExcelRecords, tmsXlsMessages,
  tmsUFlxRowComments, tmsUOle2Impl,
  {$IFNDEF FIREMONKEY}
  {$IFDEF FLX_VCL}Clipbrd,{$ENDIF}
  {$IFDEF FLX_CLX}QClipbrd, {$ENDIF}
  {$ENDIF}
  {$IFNDEF FLX_CROSSPLAT}Windows,{$ENDIF}
  

  {$IFDEF FLX_NEEDSVARIANTS}variants,{$ENDIF} //Delphi 6 or above
  {$IFDEF FLX_NEEDSTYPES}Types,{$ENDIF}
  tmsUXlsSheet, tmsUFlxFormats, tmsUXlsRowColEntries,
  
  tmsUXlsXF;


  {$RESOURCE tmsEmptySheet.res}

type
  TExcelSaveFormatNative= (
    snXLS, snCSVComma, snCSVSemiColon, snTabDelimited
    );

  TSetOfExcelSaveFormatNative = Set Of TExcelSaveFormatNative;


type
  TXLSAdapter = class(TExcelAdapter)
  private
    FTemplateStore: TXlsBaseTemplateStore;
    FSaveFormat: TSetOfExcelSaveFormatNative;
    FAllowOverwritingFiles: boolean;
    procedure SetTemplateStore(const Value: TXLSBaseTemplateStore);
    { Private declarations }
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Protected declarations }
  public
    constructor Create(AOwner:TComponent);override;
    function GetWorkbook: TExcelFile;override;
    { Public declarations }
  published
    property SaveFormat: TSetOfExcelSaveFormatNative read FSaveFormat write FSaveFormat default [snXLS];
    property TemplateStore: TXLSBaseTemplateStore read FTemplateStore write SetTemplateStore;
    property AllowOverwritingFiles:boolean read FAllowOverwritingFiles write FAllowOverwritingFiles;

    { Published declarations }
  end;

  TXLSFile = class(TExcelFile)
  private
    FAdapter: TXLSAdapter;
    FActiveSheet: integer;

    FWorkbook: TWorkbook;
    FOtherStreams : ByteArray; 

    FirstColumn,LastColumn: integer;
    AllowOverwritingFiles: boolean;

    IsFileModified: boolean;

    RowPictures: TRowComments;
    procedure ParsePictures;
    procedure OpenStream(const aStream: TStream);
{$IFNDEF FIREMONKEY}
    procedure PasteFromBiff8(const Row, Col: integer);
    procedure PasteFromText(const Row, Col: integer);
{$ENDIF}

    procedure SaveAsXls(const aFileName: string; const aStream: TStream); overload;
    procedure SaveAsXls(const aStream: TStream); overload;
    procedure SaveAsTextDelimited(const FileName: string; const DataStream: TStream; const Delim: Char);

    procedure InternalSetCellString(const aRow, aCol: integer; const Text: UTF16String; const Fm: PFlxFormat; const DateFormat, TimeFormat: UTF16String; const FormatSettings: PFormatSettings);
    procedure SetCellValueAndFmt(const aRow, aCol: integer; const v: variant; const Fm: PFlxFormat);
    function SkipThousands(const s: string; const FormatSettings: PFormatSettings): string;
    procedure RestoreObjectSizes;
    procedure GetPercentAndCurrencyFormat(const aRow, aCol: integer; var Fmt1: TFlxFormat; const HasExp,
      HasPercent, HasCurrency, NeedsDecimals: boolean; const FormatSettings: PFormatSettings);

  protected
    function GetActiveSheet: integer; override;
    procedure SetActiveSheet(const Value: integer); override;
    function GetActiveSheetName: UTF16String; override;
    procedure SetActiveSheetName(const Value: UTF16String); override;
    function GetActiveSheetCodeName: UTF16String; override;
    procedure SetActiveSheetCodeName(const Value: UTF16String); override;
    function GetActiveSheetVisible: TXlsSheetVisible; override;
    procedure SetActiveSheetVisible(const Value: TXlsSheetVisible); override;

    function GetColumnWidth(aCol: integer): integer;override;
    function GetColumnWidthHiddenIsZero(aCol: integer): integer;override;
    function GetRowHeight(aRow: integer): integer;override;
    function GetRowHeightHiddenIsZero(aRow: integer): integer;override;
    procedure SetColumnWidth(aCol: integer; const Value: integer);override;
    procedure SetRowHeight(aRow: integer; const Value: integer);override;

    function GetRowHidden(const aRow: integer): boolean;override;
    function GetColumnHidden(const aCol: integer): boolean;override;
    procedure SetRowHidden(const aRow: integer; const Value: boolean);override;
    procedure SetColumnHidden(const aCol: integer; const Value: boolean);override;

    function GetDefaultColWidth: integer;override;
    function GetDefaultRowHeight: integer;override;

    function GetCommentsCount(Row: integer): integer; override;
    function GetCommentText(Row, aPos: integer): UTF16String; override;
    function GetCommentColumn(Row, aPos: integer): integer; override;
    function GetPictureName(Row, aPos: integer): UTF16String;  override; //use row < 0 to return all
    function GetPicturesCount(Row: integer): integer;  override; //use row < 0 to return all

    function GetCellValue(aRow, aCol: integer): Variant; override;
    procedure SetCellValue(aRow, aCol: integer; const Value: Variant); override;
    function GetCellValueX(aRow, aCol: integer): TXlsCellValue; override;
    procedure SetCellValueX(aRow, aCol: integer; const Value: TXlsCellValue); override;

    function GetCellFormula(aRow, aCol: integer): UTF16String; override;
    procedure SetCellFormula(aRow, aCol: integer; const Value: UTF16String); override;

    function GetAutoRowHeight(Row: integer): boolean;override;
    procedure SetAutoRowHeight(Row: integer; const Value: boolean);override;

    function GetColumnFormat(aColumn: integer): integer; override;
    function GetRowFormat(aRow: integer): integer;override;
    procedure SetColumnFormat(aColumn: integer; const Value: integer);override;
    procedure SetRowFormat(aRow: integer; const Value: integer);override;

    function GetCellFormat(aRow, aCol: integer): integer; override;
    procedure SetCellFormat(aRow, aCol: integer; const Value: integer); override;

    function GetColorPalette(Index: TColorPaletteRange): LongWord; override;
    procedure SetColorPalette(Index: TColorPaletteRange; const Value: LongWord); override;

    function GetFontList(index: integer): TFlxFont; override;
    procedure SetFontList(index: integer; Value : TFlxFont); override;

    function GetFormatList(index: integer): TFlxFormat; override;
    procedure SetFormatList(index: integer; Value : TFlxFormat); override;

    function GetPageFooter: UTF16String;override;
    function GetPageHeader: UTF16String;override;
    procedure SetPageFooter(const Value: UTF16String);override;
    procedure SetPageHeader(const Value: UTF16String);override;

    function GetShowGridLines: boolean; override;
    procedure SetShowGridLines(const Value: boolean); override;
    function GetShowGridHeaders: boolean; override;
    procedure SetShowGridHeaders(const Value: boolean); override;
    function GetPrintGridLines: boolean; override;
    procedure SetPrintGridLines(const Value: boolean); override;

    function GetPrintHCentered: boolean;override;
    function GetPrintVCentered: boolean;override;
    procedure SetPrintHCentered(const Value: boolean);override;
    procedure SetPrintVCentered(const Value: boolean);override;

    function GetSheetZoom: integer;override;
    procedure SetSheetZoom(const Value: integer);override;

    function GetMargins: TXlsMargins;override;
    procedure SetMargins(const Value: TXlsMargins);override;

    function GetPrintNumberOfHorizontalPages: word;override;
    function GetPrintNumberOfVerticalPages: word;override;
    function GetPrintScale: integer;override;
    function GetPrintOptions: byte;override;
    function GetPrintToFit: boolean;override;
    procedure SetPrintNumberOfHorizontalPages(const Value: word);override;
    procedure SetPrintNumberOfVerticalPages(const Value: word);override;
    procedure SetPrintScale(const Value: integer);override;
    procedure SetPrintToFit(const Value: boolean);override;
    procedure SetPrintOptions(const Value: byte);override;

    function GetPrintCopies: integer; override;
    function GetPrinterDriverSettings: TPrinterDriverSettings; override;
    function GetPrintPaperSize: TExcelPaperSize; override;
    function GetPrintXResolution: integer; override;
    function GetPrintYResolution: integer; override;
    procedure SetPrintCopies(const Value: integer); override;
    procedure SetPrinterDriverSettings(const Value: TPrinterDriverSettings); override;
    procedure SetPrintPaperSize(const Value: TExcelPaperSize); override;
    procedure SetPrintXResolution(const Value: integer); override;
    procedure SetPrintYResolution(const Value: integer); override;


    function GetCellMergedBounds(aRow, aCol: integer): TXlsCellRange; override;
    function GetCellMergedList(index: integer): TXlsCellRange; override;

    function GetOptions1904Dates: boolean;override;
    function GetOptionsR1C1: boolean;override;
    function GetOptionsSaveExternalLinkValues: boolean;override;
    procedure SetOptions1904Dates(const Value: boolean);override;
    procedure SetOptionsR1C1(const Value: boolean);override;
    procedure SetOptionsSaveExternalLinkValues(const Value: boolean);override;
    function GetOptionsPrecisionAsDisplayed: boolean;override;
    procedure SetOptionsPrecisionAsDisplayed(const Value: boolean);override;

    function GetOutlineSummaryColsRightOfDetail: boolean; override;
    function GetOutlineSummaryRowsBelowDetail: boolean; override;
    function GetOutlineAutomaticStyles: boolean;override;
    procedure SetOutlineSummaryColsRightOfDetail(const Value: boolean); override;
    procedure SetOutlineSummaryRowsBelowDetail(const Value: boolean); override;
    procedure SetOutlineAutomaticStyles(const Value: boolean);override;

    function GetInvalidateFormulas: boolean; override;
    procedure SetInvalidateFormulas(const Value: boolean); override;

    function GetIsXltTemplate: boolean; override;
    procedure SetIsXltTemplate(const Value: boolean); override;


  public
    constructor Create(const aAdapter: TXLSAdapter);overload;
    constructor Create(const aAdapter: TXLSAdapter; const aAllowOverwritingFiles: boolean);overload;
    destructor Destroy; override;

    procedure Connect;override;
    procedure Disconnect;override;

    function GetTWorkbook: TWorkbook;

    procedure NewFile(const SheetCount: integer=3);override;
    procedure OpenFile(const FileName: TFileName);override;
    procedure OpenFileAndSearch(const FileName: TFileName);override;
    procedure OpenFileAndOrSearch(const FileName: TFileName; const Search: boolean);
    procedure LoadFromStream(const aStream: TStream);override;
    procedure CloseFile; override;

    procedure InsertAndCopySheets (const CopyFrom, InsertBefore, SheetCount: integer);override;
    procedure ClearSheet;override;
    procedure DeleteSheet(aSheetCount: integer);override;
    function SheetCount: integer;override;
    procedure SelectSheet(const SheetNo:integer); override;

    procedure DeleteMarkedRows(const Mark: UTF16String);override;
    procedure RefreshChartRanges(const VarStr: UTF16String);override;
    procedure MakePageBreaks(const Mark: UTF16String);override;
    procedure InsertHPageBreak(const Row: integer); override;
    procedure InsertVPageBreak(const Col: integer); override;
    procedure DeleteHPageBreak(const Row: integer); override;
    procedure DeleteVPageBreak(const Col: integer); override;
    function HasHPageBreak(const Row: integer): boolean;override;
    function HasVPageBreak(const Col: integer): boolean;override;
    procedure RefreshPivotTables;override;

    procedure Save(const AutoClose: boolean; const FileName: string; const OnGetFileName: TOnGetFileNameEvent; const OnGetOutStream: TOnGetOutStreamEvent=nil; const DataStream: TStream=nil);override;

    procedure InsertAndCopyRows(const FirstRow, LastRow, DestRow, aCount: integer; const OnlyFormulas: boolean);override;
    procedure InsertAndCopyCols(const FirstCol, LastCol, DestCol, aCount: integer; const OnlyFormulas: boolean);override;
    procedure DeleteRows(const aRow, aCount: integer);override;
    procedure DeleteCols(const aCol, aCount: integer);override;

    procedure BeginSheet;override;
    procedure EndSheet(const RowOffset: integer);override;

    function CanOptimizeRead: boolean; override;


    function GetExcelNameCount: integer;  override;
    function GetRangeName(index: integer): UTF16String;  override;
    function GetRangeR1(index: integer): integer; override;
    function GetRangeR2(index: integer): integer; override;
    function GetRangeC1(index: integer): integer; override;
    function GetRangeC2(index: integer): integer; override;

    procedure SetRangeR1(index: integer; value: integer); override;
    procedure SetRangeR2(index: integer; value: integer); override;
    procedure SetRangeC1(index: integer; value: integer); override;
    procedure SetRangeC2(index: integer; value: integer); override;

    function GetRangeSheet(index: integer): integer; override;

    procedure AddRange(var NamedRange: TXlsNamedRange);override;

    procedure AssignPicture(const Row, aPos: integer; const Pic: ByteArray; const PicType: TXlsImgTypes); overload; override; //use row < 0 to return all
    procedure AssignPicture(const Row, aPos: integer; const Pic: ByteArray; const PicType: TXlsImgTypes; const Props: TImageProperties; const Anchor: TFlxAnchorType=at_MoveAndDontResize);overload; override;
    procedure AssignPictureProperties(const Row, aPos: integer; const Props: TImageProperties; const Anchor: TFlxAnchorType=at_MoveAndDontResize);override;
    procedure GetPicture(const Row, aPos: integer; const Pic: TStream; out PicType: TXlsImgTypes; out Anchor: TClientAnchor); override; //use row < 0 to return all

    procedure DeleteImage(const Index: integer);override;
    procedure ClearImage(const Index: integer);override;
    procedure AddImage(const Data: ByteArray; const DataType: TXlsImgTypes; const Properties: TImageProperties;const Anchor: TFlxAnchorType);override;

    procedure AssignComment(const Row, aPos: integer; const Comment: UTF16String); override;
    function GetCellComment(Row, Col: integer): UTF16String; override;
    procedure SetCellComment(Row, Col: integer; const Value: UTF16String; const Properties: TImageProperties); override;

    function CellMergedListCount: integer; override;
    procedure MergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer); override;
    procedure UnMergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer); override;

    function CellCount(const aRow: integer): integer;override;
    function GetCellData(const aRow, aColOffset: integer): variant;override;
    function GetCellDataX(const aRow, aColOffset: integer): TXlsCellValue;override;
    procedure GetCellDataX2(const aRow, aColOffset: integer;out v: TXlsCellValue; out RTFRuns: TRTFRunList);override;
    procedure AssignCellData(const aRow, aColOffset: integer; const Value: variant);override;
    procedure AssignCellDataX(const aRow, aColOffset: integer; const Value: TXlsCellValue);override;
    procedure AssignCellDataX2(const aRow, aColOffset: integer; const Value: TXlsCellValue; const RTFRuns: TRTFRunList);override;

    procedure GetCellValueX2(aRow, aCol: integer; out v: TXlsCellValue; out RTFRuns: TRTFRunList); override;
    procedure AssignCellValueX2(aRow, aCol: integer; const Value: TXlsCellValue; const RTFRuns: TRTFRunList); override;

    procedure SetCellFormulaX(aRow, aCol: integer; const Formula: UTF16String; const Value: variant); override;
    procedure SetCellString(const aRow, aCol: integer; const Text: UTF16String; const DateFormat: UTF16String=''; const TimeFormat: UTF16String='');overload;override;
    procedure SetCellString(const aRow, aCol: integer; const Text: UTF16String; const Fm: TFlxFormat; const DateFormat: UTF16String=''; const TimeFormat: UTF16String='');overload;override;
    function MaxRow: integer; override;
    function MaxCol: integer; override;
    function IsEmptyRow(const aRow: integer): boolean; override;

    function ColByIndex(const Row, ColIndex: integer): integer;override;
    function ColIndexCount(const Row: integer): integer; override;
    function ColIndex(const Row, Col: integer): integer;override;

    procedure SetBounds(const aRangePos: integer);override;
    function GetFirstColumn: integer; override;

    procedure PrepareBlockData(const R1,C1,R2,C2: integer);override;
    procedure AssignBlockData(const Row,Col: integer; const v: variant);override;
    procedure PasteBlockData;override;

    function IsWorksheet(const index: integer): boolean; override;

    function FontListCount: integer;override;
    function FormatListCount: integer;override;
    function AddFont (const Fmt: TFlxFont): integer; override;
    function AddFormat (const Fmt: TFlxFormat): integer; override;

    procedure CopyToClipboardFormat(const Range: TXlsCellRange;
      out textString: string; const xlsStream: TStream);
    procedure CopyToClipboard; overload; override;
    procedure CopyToClipboard(const Range: TXlsCellRange);overload;override;
    procedure PasteFromClipboard(const Row, Col: integer);override;
    procedure PasteFromXlsClipboardFormat(const Row, Col: integer; const Stream: TStream);
    procedure PasteFromTextClipboardFormat(const Row, Col: integer; const Data: string);

    procedure ParseComments; override;

    function HyperLinkCount: integer; override;
    function GetHyperLink(const HyperLinkIndex:integer):THyperLink; override;
    procedure SetHyperLink(const HyperLinkIndex:integer; const value: THyperLink); override;
    function GetHyperLinkCellRange(const HyperLinkIndex: integer):TXlsCellRange; override;
    procedure SetHyperLinkCellRange(const HyperLinkIndex: integer; const CellRange:TXlsCellRange ); override;
    procedure AddHyperLink(const CellRange: TXlsCellRange; const value: THyperLink); override;
    procedure DeleteHyperLink(const HyperLinkIndex: integer); override;

    function GetRowOutlineLevel(const aRow: integer): integer; override;
    procedure SetRowOutlineLevel(const FirstRow, LastRow, Level: integer); override;
    function GetColOutlineLevel(const aCol: integer):integer; override;
    procedure SetColOutlineLevel(const FirstCol, LastCol, Level: integer); override;

    function GetUsedPaletteColors: BooleanArray; override;

    procedure FreezePanes(const Row, Col: integer);override;
    procedure GetFrozenPanes(out Row, Col: integer);override;
    procedure SplitWindow(const xOffset, yOffset: integer);override;
    procedure GetSplitWindow(out xOffset, yOffset: integer);override;

    procedure AutofitRow(const row1, row2: integer; const AutofitNotAutofittingRows: Boolean; const keepHeightAutomatic: Boolean; const adjustment: extended);override;
    procedure AutofitCol(const Col1, Col2: integer; const IgnoreStrings: Boolean; const Adjustment: extended);override;
    procedure AutofitRowsOnWorkbook(const AutofitNotAutofittingRows: Boolean; const KeepSizesAutomatic: Boolean; const Adjustment: extended);override;

    procedure SetAutoFilter(const row: Int32; const col1: Int32; const col2: Int32);override;
    procedure RemoveAutoFilter();override;
    function HasAutoFilter(): Boolean;overload; override;
    function HasAutoFilter(const row: Int32; const col: Int32): Boolean;overload; override;
    function GetAutoFilterRange(): TXlsCellRange;override;

    function DefaultFormatId: integer; override;
    function FormatValue1904(const CellValue: Variant; const Format: UTF16String; var Color: Integer): UTF16String; override;

  end;

implementation
uses tmsUXlsBaseRecordLists, tmsUXlsBaseRecords, tmsUXlsNotes, tmsUXlsHyperLink, tmsUFlxNumberFormat,
  {$IFDEF TRIAL}Dialogs,{$ENDIF}
  tmsUXlsWorkbookGlobals;
{ TXLSAdapter }

constructor TXLSAdapter.Create(AOwner: TComponent);
begin
  inherited;
  FSaveFormat:=[snXLS];
end;

function TXLSAdapter.GetWorkbook: TExcelFile;
begin
  Result:= TXLSFile.Create(Self, AllowOverwritingFiles);
end;

procedure TXLSAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FTemplateStore then
        FTemplateStore:= nil;
  end;
end;

procedure TXLSAdapter.SetTemplateStore(const Value: TXLSBaseTemplateStore);
begin
  FTemplateStore := Value;
end;

{ TXLSFile }

procedure TXLSFile.RestoreObjectSizes;
begin
	FWorkbook.RestoreObjectCoords(FActiveSheet - 1);
end;

procedure TXLSFile.AssignCellData(const aRow, aColOffset: integer; const Value: variant);
var
  V: TXlsCellValue;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
    V.Value:=Value; V.XF:=-1;
    FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(aRow-1, FirstColumn + aColOffset, V, nil, Options1904Dates);
    IsFileModified := true;
end;

procedure TXLSFile.SetCellValueAndFmt(const aRow, aCol: integer; const v: variant; const Fm: PFlxFormat);
var
  Value: TXlsCellValue;
begin
  if Fm=nil then SetCellValue(ARow, ACol, v) else
  begin
    Value.XF:=AddFormat(Fm^);
    Value.Value:=v;
    Value.IsFormula:=False;
    SetCellValueX(aRow, aCol, Value);
  end;
end;

function TXlsFile.SkipThousands(const s: string; const FormatSettings: PFormatSettings): string;
var
  s1: string;
  i,L: integer;
begin

  // on german locales, "11.11.02" is a valid date, and it could be a number too. So we *Must* check thousands come on groups of 3.
  i:= pos(FormatSettings.DecimalSeparator, s);
  if i>0 then
    s1:=copy(s,1,i-1)
  else s1:=s;

  if (i>0) and (pos(FormatSettings.ThousandSeparator, copy(s,i,length(s)))>0) then   //No thousand separators after decimalseparator.
  begin
    result:=s;
    exit;
  end;

  if (length(s)>0) and (s[1]=FormatSettings.ThousandSeparator) then   //No numbers like ",000.3"  .
  begin
    result:=s;
    exit;
  end;

  i:=3;
  L:=Length(s1);
  while i<L do
  begin
    if (s1[L-i]<>FormatSettings.ThousandSeparator) and (s1[L-i]<>'-')then
    begin
      result:=s;
      exit;
    end;
    inc(i,4);
  end;

  result:=StringReplace(s,FormatSettings.ThousandSeparator,'', [rfReplaceAll]);
end;

procedure CheckPercentAndCurrency(var s: string; out HasExp, HasPercent, HasCurrency: boolean; const FormatSettings: PFormatSettings);
begin
 HasExp := false; HasPercent := false; HasCurrency := false;
 if pos ('e', LowerCase(s)) > 1 then HasExp := true;

 if (Length(s) > 0) and (s[1] = '%') then
 begin
   s := copy(s, 2, Length(s));
   HasPercent := true;
   exit;
 end;

 if (Length(s) > 1) and (s[Length(s)] = '%') then
 begin
   s := copy(s, 1, Length(s) - 1);
   HasPercent := true;
   exit;
 end;

 if (Length(s) > 0) and (copy(s, 1, Length(FormatSettings.CurrencyString)) = FormatSettings.CurrencyString) then
 begin
   s := copy(s, 1 + Length(FormatSettings.CurrencyString), Length(s));
   HasCurrency := true;
   exit;
 end;

 if (Length(s) > 0) and (copy(s, Length(s) - Length(FormatSettings.CurrencyString), Length(FormatSettings.CurrencyString)) = FormatSettings.CurrencyString) then
 begin
   s := copy(s, 1, Length(s) - Length(FormatSettings.CurrencyString));
   HasCurrency := true;
   exit;
 end;
end;

procedure TXLSFile.GetPercentAndCurrencyFormat(const aRow, aCol: integer; var Fmt1: TFlxFormat; const HasExp, HasPercent, HasCurrency, NeedsDecimals: boolean; const FormatSettings: PFormatSettings);
const
  Sp = '\ ';
  NegSep = ';[Red]';
  NegSep2 = '_)' + NegSep;
  Op = '\(';
  Cp = '\)';
  NegativeSign = '-';
var
  FmtIndex: integer;
  n: string;
  CurrFmt: string;
  CurrStr: string;
begin
  CurrStr := FormatSettings.CurrencyString;
  if (not HasExp) and (not HasPercent) and (not HasCurrency) then exit;

  if HasExp then
  begin
    Fmt1.Format:= XlsBuiltInFormats(11);
    exit;
  end;

  if HasPercent then
  begin
    FmtIndex := 9;
    if NeedsDecimals then FmtIndex := 10;
    Fmt1.Format:= XlsBuiltInFormats(FmtIndex);
    exit;
  end;

  if HasCurrency then
  begin
    //This has to be entered with the current currency locale, not what is in the file (in a NewFile we will see Euro)
    n := '###,###';
    if (NeedsDecimals) then n := n + '.' + StringOfChar('0', FormatSettings.CurrencyDecimals);

    case FormatSettings.CurrencyFormat of
       1: CurrFmt := n + CurrStr;
       2: CurrFmt := CurrStr + Sp + n;
       3: CurrFmt := n + Sp + CurrStr;
       else CurrFmt := CurrStr + n;
    end;

    case FormatSettings.NegCurrFormat of
        1: CurrFmt := CurrFmt +  NegSep + NegativeSign + CurrStr + n;
        2: CurrFmt := CurrFmt +  NegSep + CurrStr + NegativeSign + n;
        3: CurrFmt := CurrFmt +  NegSep + CurrStr + n + NegativeSign;
        4: CurrFmt := CurrFmt +  NegSep2 + Op + n + CurrStr + Cp;
        5: CurrFmt := CurrFmt +  NegSep + NegativeSign + n + CurrStr;
        6: CurrFmt := CurrFmt +  NegSep + n + NegativeSign + CurrStr;
        7: CurrFmt := CurrFmt +  NegSep + n + CurrStr + NegativeSign;

        8: CurrFmt := CurrFmt +  NegSep + NegativeSign + n + Sp + CurrStr;
        9: CurrFmt := CurrFmt +  NegSep + NegativeSign + CurrStr + Sp + n;
        10: CurrFmt := CurrFmt +  NegSep + n + Sp + CurrStr + NegativeSign;
        11: CurrFmt := CurrFmt +  NegSep + CurrStr + Sp + n + NegativeSign;
        12: CurrFmt := CurrFmt +  NegSep + CurrStr + Sp + NegativeSign + n;
        13: CurrFmt := CurrFmt +  NegSep + n + NegativeSign + Sp + CurrStr;
        14: CurrFmt := CurrFmt +  NegSep2 + Op + CurrStr + Sp + n + Cp;
        15: CurrFmt := CurrFmt +  NegSep2 + Op + n + Sp + CurrStr + Cp;

        else CurrFmt := CurrFmt + NegSep2 + Op + CurrStr + n + Cp;

    end;
    Fmt1.Format := CurrFmt;
    exit;
  end;

end;


procedure TXLSFile.InternalSetCellString(const aRow, aCol: integer; const Text: UTF16String; const Fm: PFlxFormat; const DateFormat, TimeFormat: UTF16String; const FormatSettings: PFormatSettings);
var
  e:extended;
  d:double;
  ok: boolean;
  s, s1: string;
  dt: TDateTime;
  dFormat: UTF16String;
  Fmt: TFlxFormat;
  HasTime, HasDate: boolean;
  HasExp, HasPercent, HasCurrency: boolean;
begin
  //try to convert to number
    s:=Trim(Text); //for if value is a UTF16String
    s1 := s;
    // if TextToFloat(PChar(StringReplace(s,ThousandSeparator,'', [rfReplaceAll])), e, fvExtended) then  //Dont use val because it doesnt handle locales
    ok:=false; d:=0;
    CheckPercentAndCurrency(s1, HasExp, HasPercent, HasCurrency, FormatSettings);
    if TextToFloat(PChar(SkipThousands(s1, FormatSettings)), e, fvExtended) then  //Dont use val because it doesnt handle locales
    begin
      try
        d:=e;
        if HasPercent then d:= d/100.0;
         if Fm=nil then
          begin
            Fmt:=GetFormatList(CellFormat[ARow, ACol]);
          end else Fmt := Fm^;
        GetPercentAndCurrencyFormat(aRow, aCol, Fmt, HasExp, HasPercent, HasCurrency, Pos(FormatSettings.DecimalSeparator, s1) > 1, FormatSettings);
        ok:=true;
      except
      end; //except
    end;
    if ok then SetCellValueAndFmt(ARow, ACol, d, @Fmt) else
  //try to convert to boolean
    if UpperCase(s)=TxtFalse then SetCellValueAndFmt(ARow, ACol, false, Fm)  else
    if UpperCase(s)=TxtTrue then SetCellValueAndFmt(ARow, ACol, true, Fm) else
  //try to convert to a date
    if FlxTryStrToDateTime(s, dt, dFormat, HasDate, HasTime, DateFormat, TimeFormat, FormatSettings) then
    begin
      if Fm=nil then Fmt:=GetFormatList(CellFormat[ARow, ACol]) else Fmt:=Fm^;
      Fmt.Format:=dFormat;
      SetCellValueAndFmt(ARow, ACol, double(dt), @Fmt)
    end else
      SetCellValueAndFmt(ARow, ACol, Text, Fm);
end;

procedure TXLSFile.SetCellString(const aRow, aCol: integer; const Text: UTF16String; const DateFormat: UTF16String; const TimeFormat: UTF16String);
begin
  InternalSetCellString(aRow, aCol, Text, nil, DateFormat, TimeFormat, GetDefaultLocaleFormatSettings);
end;

procedure TXLSFile.SetCellString(const aRow, aCol: integer; const Text: UTF16String; const Fm: TFlxFormat; const DateFormat: UTF16String; const TimeFormat: UTF16String);
begin
  InternalSetCellString(aRow, aCol, Text, @Fm, DateFormat, TimeFormat, GetDefaultLocaleFormatSettings);
end;


procedure TXLSFile.AssignCellDataX(const aRow, aColOffset: integer; const Value: TXlsCellValue);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(aRow-1, FirstColumn + aColOffset, Value, nil, Options1904Dates);
  IsFileModified := true;
end;

procedure TXLSFile.AssignComment(const Row, aPos: integer;
  const Comment: UTF16String);
begin
  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
  begin
    if Comment='' then FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1].Delete(aPos) else
    FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1][aPos].Text:= Comment;
  end;
end;

procedure TXLSFile.AssignPicture(const Row, aPos: integer; const Pic: ByteArray; const PicType: TXlsImgTypes);
var
  MyPos: integer;
begin
  if Row>0 then MyPos:=RowPictures[Row][aPos] else MyPos:=aPos;
  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
    FWorkbook.WorkSheets[ActiveSheet-1].AssignDrawing(MyPos, Pic, PicType);
end;

procedure TXLSFile.AssignPicture(const Row, aPos: integer; const Pic: ByteArray;
  const PicType: TXlsImgTypes; const Props: TImageProperties; const Anchor: TFlxAnchorType);
begin
  AssignPicture(Row, aPos, Pic, PicType);
  AssignPictureProperties(Row, aPos, Props, Anchor);
end;

procedure TXLSFile.AssignPictureProperties(const Row, aPos: integer; const Props: TImageProperties; const Anchor: TFlxAnchorType);
var
  MyPos: integer;
  ClientAnchor: TClientAnchor;
begin
  if Row>0 then MyPos:=RowPictures[Row][aPos] else MyPos:=aPos;

  case Anchor of
    at_MoveAndResize: ClientAnchor.Flag:=00;
    at_DontMoveAndDontResize: ClientAnchor.Flag:=03;
    else ClientAnchor.Flag:=02;
  end; //case

  ClientAnchor.Col1:=Props.Col1-1;
  ClientAnchor.Dx1:=Props.dx1;
  ClientAnchor.Col2:=Props.Col2-1;
  ClientAnchor.Dx2:=Props.dx2;
  ClientAnchor.Row1:=Props.Row1-1;
  ClientAnchor.Dy1:=Props.dy1;
  ClientAnchor.Row2:=Props.Row2-1;
  ClientAnchor.Dy2:=Props.dy2;

  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
    FWorkbook.WorkSheets[ActiveSheet-1].SetAnchor(MyPos, ClientAnchor);

end;

procedure TXLSFile.GetPicture(const Row, aPos: integer; const Pic: TStream;
  out PicType: TXlsImgTypes; out Anchor: TClientAnchor);
var
  MyPos: integer;
begin
  if Row>0 then MyPos:=RowPictures[Row][aPos] else MyPos:=aPos;
  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
  begin
    if (Pic<>nil) then FWorkbook.WorkSheets[ActiveSheet-1].GetDrawingFromStream(MyPos, Pic, PicType);
    Anchor:=FWorkbook.WorkSheets[ActiveSheet-1].GetAnchor(MyPos);
    inc(Anchor.Col1);
    inc(Anchor.Col2);
    inc(Anchor.Row1);
    inc(Anchor.Row2);
  end else
  begin
    FillChar(Anchor, SizeOf(Anchor), 0) //TClientAnchor doesn't have any dynamic fields like strings. If it did, Result is not a safe plaec to use FillChar.
  end;
end;

procedure TXLSFile.ParsePictures;
var
  i:integer;

begin
  FreeAndNil(RowPictures);
  RowPictures:= TRowComments.Create;
  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
    for i:=0 to FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount-1 do
      RowPictures.Add(FWorkbook.WorkSheets[ActiveSheet-1].DrawingRow[i]+1, i);
end;


procedure TXLSFile.BeginSheet;
begin
  ParsePictures;
end;

function TXLSFile.CellCount(const aRow: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0; exit; end;
  if aRow-1<FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Count then
    Result:=LastColumn-FirstColumn+1
  else Result:=0;
end;

procedure TXLSFile.CloseFile;
begin
  //Nothing
end;

procedure TXLSFile.Connect;
begin
  FWorkbook:= TWorkbook.Create;
end;


constructor TXLSFile.Create(const aAdapter: TXLSAdapter; const aAllowOverwritingFiles: boolean);
begin
  inherited Create;
  FAdapter:= aAdapter;
  AllowOverwritingFiles:=aAllowOverwritingFiles;
  IsFileModified := false;
end;

constructor TXLSFile.Create(const aAdapter: TXLSAdapter);
begin
  Create(aAdapter, false);
end;

procedure TXLSFile.DeleteMarkedRows(const Mark: UTF16String);
var
  i:integer;
  s: UTF16String;
  Cl: TCellList;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Cl:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList;
  IsFileModified := true;
  for i:=Cl.Count -1 downto 0 do
  try
    s:= Cl.Value[i,0].Value;
    if (s=Mark) then
      FWorkbook.DeleteRowsAndCols(FActiveSheet-1, i, 1,0,0);
  except
    //nothing
  end;//except
end;

procedure TXLSFile.MakePageBreaks(const Mark: UTF16String);
var
  i:integer;
  s: UTF16String;
  V: TXlsCellValue;
  Cl: TCellList;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  V.Value:=Unassigned; V.XF:=-1;
  Cl:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList;
  for i:=Cl.Count -1 downto 0 do
  try
    s:= Cl.Value[i,0].Value;
    if (s=Mark) then
    begin
      FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(i,0,V,nil,Options1904Dates);
      FWorkbook.InsertHPageBreak(FActiveSheet-1, i);
    end;
  except
    //nothing
  end;//except
end;

procedure TXLSFile.DeleteRows(const aRow, aCount: integer);
begin
  FWorkbook.DeleteRowsAndCols(FActiveSheet-1, aRow-1, aCount,0,0);
  IsFileModified := true;
end;

function TXLSFile.DefaultFormatId: integer;
begin
  Result := 15;
end;

procedure TXLSFile.DeleteCols(const aCol, aCount: integer);
begin
  FWorkbook.DeleteRowsAndCols(FActiveSheet-1, 0, 0, aCol-1, aCount);
  IsFileModified := true;
 end;

destructor TXLSFile.Destroy;
begin
  FreeAndNil(RowPictures);
  inherited;
end;

procedure TXLSFile.Disconnect;
begin
  FreeAndNil(FWorkbook);
end;

procedure TXLSFile.EndSheet(const RowOffset: integer);
begin
  //Nothing
end;

function TXLSFile.GetActiveSheet: integer;
begin
  Result:= FActiveSheet;
end;

function TXLSFile.GetActiveSheetName: UTF16String;
begin
  Result:= FWorkbook.Globals.SheetName[FActiveSheet-1];
end;

function TXLSFile.GetActiveSheetCodeName: UTF16String;
begin
  Result:= FWorkbook.Sheets[FActiveSheet-1].CodeName;
end;

function TXLSFile.GetCellData(const aRow, aColOffset: integer): variant;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=unassigned; exit; end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Value[aRow-1,FirstColumn+aColOffset].Value;
end;

function TXLSFile.GetCellDataX(const aRow, aColOffset: integer): TXlsCellValue;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result.Value:=unassigned; Result.XF:=-1; exit; end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Value[aRow-1,FirstColumn+aColOffset];
end;

function TXLSFile.GetCommentsCount(Row: integer): integer;
begin
  if FWorkbook.IsWorkSheet(ActiveSheet-1) then
    if Row-1<FWorkbook.WorkSheets[ActiveSheet-1].Notes.Count then
      Result:=FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1].Count
    else
      Result:=0
  else
    Result:=0;
end;

function TXLSFile.GetCommentText(Row, aPos: integer): UTF16String;
begin
  if FWorkbook.IsWorkSheet(ActiveSheet-1)
    and (Row-1<FWorkbook.WorkSheets[ActiveSheet-1].Notes.Count) then
      Result:=FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1][aPos].Text
    else
      Result:='';
end;

function TXLSFile.GetExcelNameCount: integer;
begin
  Result:=FWorkbook.Globals.Names.Count;
end;

function TXLSFile.GetPictureName(Row, aPos: integer): UTF16String;
var
  MyPos: integer;
begin
  if Row>0 then MyPos:=RowPictures[Row][aPos] else MyPos:=aPos;
  Result:= '';
  if not FWorkbook.IsWorksheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].DrawingName[MyPos];
end;

function TXLSFile.GetPicturesCount(Row: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorksheet(FActiveSheet-1) then exit;
  if Row>0 then Result:=RowPictures[Row].Count else
    Result:= FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount;
end;

function TXLSFile.GetRangeName(index: integer): UTF16String;
begin
  Result:= FWorkbook.Globals.Names[index-1].Name;
end;

function TXLSFile.GetRangeR1(index: integer): integer;
begin
  Result:= FWorkbook.Globals.Names[index-1].GetR1+1;
end;

function TXLSFile.GetRangeR2(index: integer): integer;
begin
  Result:= FWorkbook.Globals.Names[index-1].GetR2+1;
end;

function TXLSFile.GetRangeC1(index: integer): integer;
begin
  Result:= FWorkbook.Globals.Names[index-1].GetC1+1;
end;

function TXLSFile.GetRangeC2(index: integer): integer;
begin
  Result:= FWorkbook.Globals.Names[index-1].GetC2+1;
end;

function TXLSFile.GetRangeSheet(index: integer): integer;
begin
  Result:= FWorkbook.Globals.Names[index-1].RefersToSheet(FWorkbook.Globals.References.GetSheet)+1;
end;

procedure TXLSFile.InsertAndCopyRows(const FirstRow, LastRow, DestRow,
  aCount: integer; const OnlyFormulas: boolean);
begin
  FWorkbook.InsertAndCopyRowsAndCols(FActiveSheet-1, FirstRow-1, LastRow-1, DestRow-1, aCount, 0,0,0,0, OnlyFormulas);
  IsFileModified := true;
end;

procedure TXLSFile.InsertAndCopyCols(const FirstCol, LastCol, DestCol,
  aCount: integer; const OnlyFormulas: boolean);
begin
  FWorkbook.InsertAndCopyRowsAndCols(FActiveSheet-1, 0,0,0,0, FirstCol-1, LastCol-1, DestCol-1, aCount, OnlyFormulas);
  IsFileModified := true;
end;

procedure TXLSFile.InsertAndCopySheets(const CopyFrom, InsertBefore,
  SheetCount: integer);
begin
  FWorkbook.InsertSheets(CopyFrom-1, InsertBefore-1, SheetCount);
  IsFileModified := true;
end;

procedure TXLSFile.OpenStream(const aStream: TStream);
var
  DataStream: TOle2File;
  MemStream: TMemoryStream;
  DeleteStorages: StringArray;
begin
{$IFDEF TRIAL}
{$WARNINGS OFF}
  if DebugHook = 0 then ShowMessage('This is an unregistered version of FlexCel');
{$WARNINGS ON}
{$ENDIF}
  MemStream := TMemoryStream.Create;
  try
    DataStream := TOle2File.Create(aStream);
    try
      DataStream.SelectStream(WorkbookStrS);
      FWorkbook.LoadFromStream(DataStream);
      SetLength(DeleteStorages, 0);
      DataStream.PrepareForWrite(MemStream, WorkbookStrS, DeleteStorages); //Saves all the other streams.
    finally
      FreeAndNil(DataStream);
    end;

    SetLength(FOtherStreams, MemStream.Size);
    MemStream.Position := 0;
    MemStream.ReadBuffer(FOtherStreams[0], Length(FOtherStreams));
  finally
    FreeAndNil(MemStream);
  end;

  FActiveSheet:=FWorkbook.ActiveSheet+1;
  IsFileModified := false;
end;

procedure TXLSFile.OpenFile(const FileName: TFileName);
begin
  OpenFileAndOrSearch(FileName, false);
end;

procedure TXLSFile.OpenFileAndSearch(const FileName: TFileName);
begin
  OpenFileAndOrSearch(FileName, true);
end;

procedure TXLSFile.OpenFileAndOrSearch(const FileName: TFileName; const Search: boolean);
var
  fs: TStream;
  TemplateData: ByteArray;
  SearchFileName: string;
begin
  TemplateData := nil; //Avoid stupid warning in D7.
  fs := nil;
  try
    if (FAdapter<>nil) and (FAdapter.TemplateStore<>nil) then
    begin
      fs := TMemoryStream.Create;
      TemplateData := FAdapter.TemplateStore.StoredFile[FileName];
      fs.Write(TemplateData[0], Length(TemplateData));
      fs.Seek(0, soFromBeginning)
    end
    else
    begin
      if Search and (FAdapter <> nil) then
      begin
        SearchFileName := SearchPathStr(FAdapter.BasePathToOpen, FileName);
      end
      else SearchFileName := FileName;

      fs := TFileStream.Create(SearchFileName, fmOpenRead or fmShareDenyNone);
    end;

    OpenStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TXLSFile.LoadFromStream(const aStream: TStream);
begin
  OpenStream(aStream);
end;

procedure TXLSFile.RefreshPivotTables;
begin
  //Nothing
end;


procedure TXLSFile.RemoveAutoFilter;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].RemoveAutoFilter;
end;

procedure TXLSFile.SaveAsXls(const aFileName: string; const aStream: TStream);
var
  f: TFileStream;
  CreateMode: word;
begin
  if aStream <> nil then SaveAsXls(aStream)
  else
  begin
    CreateMode := fmCreate;
    f := TFileStream.Create(aFileName, CreateMode);
    try
    try
      SaveAsXls(f);
    except
      SysUtils.DeleteFile(aFileName);
      raise;
    end;
    finally
      FreeAndNil(f);
    end;
  end;
end;

procedure TXLSFile.SaveAsXls(const aStream: TStream);
var
  MemStream: TMemoryStream;
  DataStream: TOle2File;
  DeleteStreams: StringArray;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.WriteBuffer(FOtherStreams[0], Length(FOtherStreams));
    MemStream.Position := 0;
    DataStream := TOle2File.Create(MemStream);
    try
      SetLength(DeleteStreams, 0);
      DataStream.PrepareForWrite(aStream, WorkbookStrS, DeleteStreams);
      FWorkbook.SaveToStream(DataStream, IsFileModified);
    finally
      FreeAndNil(DataStream);
    end;
  finally
    FreeAndNil(MemStream);
  end;

end;

procedure TXLSFile.SaveAsTextDelimited(const FileName: string;
  const DataStream: TStream; const Delim: Char);
begin
end;


procedure TXLSFile.Save(const AutoClose: boolean; const FileName: string; const OnGetFileName: TOnGetFileNameEvent; const OnGetOutStream: TOnGetOutStreamEvent=nil; const DataStream: TStream=nil);
var
  aFileName: TFileName;
  OutStream: TStream;
  SF: TExcelSaveFormatNative;
begin
  for SF:=Low(TExcelSaveFormatNative) to High(TExcelSaveFormatNative) do
    if ((FAdapter<>nil) and (SF in FAdapter.SaveFormat))or
       ((FAdapter=nil) and (SF = snXLS)) then
    begin
      aFileName:=Filename;
      OutStream:=nil;
      if Assigned(DataStream) then
      begin
        //Save to stream
        OutStream:=DataStream;
      end
      else
      if Assigned (OnGetOutStream) then
      begin
        //SaveToStream
        OnGetOutStream(Self,integer(SF),OutStream);
      end else
      begin
        //SaveToFile
        if Assigned (OnGetFileName) then OnGetFileName(Self,integer(SF),aFilename);
        if (not AllowOverwritingFiles) and FileExists(aFileName) then raise Exception.CreateFmt(ErrCantWriteToFile, [aFileName]); 
      end;

      case SF of
        snXLS: SaveAsXls(aFileName, OutStream);
        snCSVComma: SaveAsTextDelimited(aFileName, OutStream, ',');
        snCSVSemiColon: SaveAsTextDelimited(aFileName, OutStream, ';');
        snTabDelimited: SaveAsTextDelimited(aFileName, OutStream, #9);
        else raise Exception.Create(ErrInternal);
      end; //case
    end;
end;

procedure TXLSFile.SelectSheet(const SheetNo:integer);
begin
  FWorkbook.ActiveSheet:=SheetNo-1;
end;

procedure TXLSFile.SetActiveSheet(const Value: integer);
begin
  FActiveSheet:=Value;
end;

procedure TXLSFile.SetActiveSheetName(const Value: UTF16String);
begin
  FWorkbook.Globals.SheetName[FActiveSheet-1]:= Value;
end;

procedure TXLSFile.SetActiveSheetCodeName(const Value: UTF16String);
var
  i: integer;
begin
  for i:=0 to FWorkbook.Sheets.Count-1 do
  begin
    if FWorkbook.Sheets[i].CodeName= Value then raise Exception.CreateFmt(ErrDuplicatedSheetName,[Value]);
  end;

  FWorkbook.Sheets[FActiveSheet-1].CodeName:= Value;
end;

procedure TXLSFile.SetBounds(const aRangePos: integer);
begin
  FirstColumn:=FWorkbook.Globals.Names[aRangePos-1].GetC1;
  LastColumn:=FWorkbook.Globals.Names[aRangePos-1].GetC2;
end;

function TXLSFile.SheetCount: integer;
begin
  Result:=FWorkbook.Globals.SheetCount;
end;

procedure TXLSFile.AssignBlockData(const Row, Col: integer; const v: variant);
begin
  AssignCellData(Row, Col, v);
end;

procedure TXLSFile.PasteBlockData;
begin
  // Nothing
end;

procedure TXLSFile.PrepareBlockData(const R1, C1, R2, C2: integer);
begin
  // Nothing
end;

function TXLSFile.MaxRow: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].Cells.RowList.Count;
end;

function TXLSFile.MaxCol: integer;
var
  i: integer;
  Cells: TCellList;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;

  Cells := FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList;
    for i:=0 to Cells.Count-1 do
      if (Cells[i] <> nil) and (Cells[i].Count > 0) then
        if Cells[i][Cells[i].Count - 1].Column + 1> Result then Result:= Cells[i][Cells[i].Count - 1].Column + 1; //1 based
end;


function TXLSFile.GetCellValue(aRow, aCol: integer): Variant;
begin
  Result:= GetCellData(aRow, aCol-FirstColumn-1);
end;

procedure TXLSFile.SetCellValue(aRow, aCol: integer; const Value: Variant);
begin
  AssignCellData(aRow, aCol-FirstColumn-1, Value);
end;

function TXLSFile.IsEmptyRow(const aRow: integer): boolean;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=true;exit;end;
  Result:=
    (aRow-1<0) or (aRow-1>= FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Count) or
    not FWorkbook.WorkSheets[FActiveSheet-1].Cells.RowList.HasRow(aRow-1);
end;


function TXLSFile.CanOptimizeRead: boolean;
begin
  Result:=true;
end;

procedure TXLSFile.RefreshChartRanges(const VarStr: UTF16String);
begin
  //not implemented
end;

function TXLSFile.IsWorksheet(const index: integer): boolean;
begin
  Result:= FWorkbook.Sheets[index-1] is TWorkSheet;
end;


function TXLSFile.GetColumnWidth(aCol: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetColWidth(aCol-1);
end;

function TXLSFile.GetColumnWidthHiddenIsZero(aCol: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetColWidth(aCol-1, true);
end;

function TXLSFile.GetRowHeight(aRow: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetRowHeight(aRow-1);
end;

function TXLSFile.GetRowHeightHiddenIsZero(aRow: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=0;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetRowHeight(aRow-1, true);
end;

procedure TXLSFile.SetColumnWidth(aCol: integer; const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetColWidth(aCol-1, Value);
	RestoreObjectSizes();
end;

procedure TXLSFile.SetRowHeight(aRow: integer; const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetRowHeight(aRow-1, Value);
	RestoreObjectSizes();
end;

function TXLSFile.GetColumnHidden(const aCol: integer): boolean;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=false;exit;end;
  if (aCol<1)or (aCol>Max_Columns+1) then begin; result:=false;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetColHidden(aCol-1);
end;

function TXLSFile.GetRowHidden(const aRow: integer): boolean;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=false;exit;end;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].GetRowHidden(aRow-1);
end;

procedure TXLSFile.SetColumnHidden(const aCol: integer; const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if (aCol<1)or (aCol>Max_Columns+1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetColHidden(aCol-1, Value);
	RestoreObjectSizes();
end;

procedure TXLSFile.SetRowHidden(const aRow: integer; const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetRowHidden(aRow-1, Value);
	RestoreObjectSizes();
end;

function TXLSFile.GetFirstColumn: integer;
begin
  Result:=FirstColumn+1;
end;

function TXLSFile.GetCellValueX(aRow, aCol: integer): TXlsCellValue;
begin
  Result:= GetCellDataX(aRow, aCol-FirstColumn-1);
end;

procedure TXLSFile.SetCellValueX(aRow, aCol: integer;
  const Value: TXlsCellValue);
begin
  AssignCellDataX(aRow, aCol-FirstColumn-1, Value);
end;

function TXLSFile.GetAutoFilterRange: TXlsCellRange;
begin
  Result.Left := -1; Result.Top := -1; Result.Bottom := -1; Result.Right := -1;;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result := FWorkbook.WorkSheets[FActiveSheet-1].GetAutoFilterRange(FActiveSheet - 1);
  inc(Result.Left);
  inc(Result.Top);
  inc(Result.Right);
  inc(Result.Bottom);
end;

function TXLSFile.GetAutoRowHeight(Row: integer): boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.RowList.IsAutoRowHeight(Row-1);
end;

procedure TXLSFile.SetAutoFilter(const row, col1, col2: Int32);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetAutoFilter(FActiveSheet - 1, row - 1, col1 - 1, col2 - 1);
end;

procedure TXLSFile.SetAutoRowHeight(Row: integer; const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.RowList.AutoRowHeight(Row-1, Value);
end;

function TXLSFile.GetColorPalette(Index: TColorPaletteRange): LongWord;
begin
  Result:=FWorkbook.Globals.ColorPalette[Index-1];
end;

function TXlsFile.GetUsedPaletteColors: BooleanArray;
begin
  Result := FWorkbook.Globals.XF.GetUsedColors(56 + 1, FWorkbook.Globals.Fonts);
end;

procedure TXLSFile.SetColorPalette(Index: TColorPaletteRange;
  const Value: LongWord);
begin
  FWorkbook.Globals.ColorPalette[Index-1]:=Value;
end;

function TXLSFile.GetColumnFormat(aColumn: integer): integer;
begin
  Result:=-1;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].GetColFormat(aColumn-1);
end;

function TXLSFile.GetRowFormat(aRow: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].GetRowFormat(aRow-1);
end;

procedure TXLSFile.SetColumnFormat(aColumn: integer; const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetColFormat(aColumn-1, Value);
end;

procedure TXLSFile.SetRowFormat(aRow: integer; const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SetRowFormat(aRow-1, Value);
end;

function TXLSFile.FormatListCount: integer;
begin
  Result:=FWorkbook.Globals.XF.Count;
end;

function TXLSFile.FormatValue1904(const CellValue: Variant;
  const Format: UTF16String; var Color: Integer): UTF16String;
begin
  Result := XlsFormatValue1904(CellValue, Format, Options1904Dates, Color);
end;

function TXLSFile.GetFormatList(index: integer): TFlxFormat;
begin
  if (Index<0) or (Index>=FWorkbook.Globals.XF.Count) then Index:=0;
  Result:=FWorkbook.Globals.XF[index].FlxFormat(FWorkbook.Globals.Fonts, FWorkbook.Globals.Formats);
end;

procedure TXLSFile.SetFormatList(index: integer; Value: TFlxFormat);
begin
  if (Index<0) or (Index>=FWorkbook.Globals.XF.Count) then Index:=0;
  FWorkbook.Globals.XF[Index]:=TXFRecord.CreateFromFormat(Value, FWorkbook.Globals.Fonts, FWorkbook.Globals.Formats);;
end;

function TXLSFile.AddFont(const Fmt: TFlxFont): integer;
begin
  Result:=FWorkbook.Globals.Fonts.AddFont(Fmt);
end;

function TXLSFile.AddFormat(const Fmt: TFlxFormat): integer;
var
  XF: TXFRecord;
begin
  XF:= TXFRecord.CreateFromFormat(Fmt, FWorkbook.Globals.Fonts, FWorkbook.Globals.Formats);
  try
    if FWorkbook.Globals.XF.FindFormat(XF, Result) then
    begin
      FreeAndNil(XF);
      exit;
    end;

    Result:=FWorkbook.Globals.XF.Add(XF);
  except
    FreeAndNil(XF);
    raise;
  end; //Except

end;

function TXLSFile.ColByIndex(const Row, ColIndex: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if IsEmptyRow(Row) then exit;
  if (ColIndex<=0) or (ColIndex>FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList[Row-1].Count) then exit;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList[Row-1][ColIndex-1].Column+1;
end;

function TXLSFile.ColIndexCount(const Row: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if IsEmptyRow(Row) then exit;
  Result:= FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList[Row-1].Count;
end;

function TXLSFile.ColIndex(const Row, Col: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if IsEmptyRow(Row) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList[Row-1].Find(Col - 1, Result);
  inc(Result);
end;

function TXLSFile.GetDefaultColWidth: integer;
begin
  Result:=$A;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].DefColWidth;
end;

function TXLSFile.GetDefaultRowHeight: integer;
begin
  Result:=$FF;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].DefRowHeight;
end;


function TXLSFile.GetShowGridLines: boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].ShowGridLines;
end;

procedure TXLSFile.SetShowGridLines(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].ShowGridLines:=value;
end;

function TXLSFile.GetShowGridHeaders: boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].ShowGridHeaders;
end;

procedure TXLSFile.SetShowGridHeaders(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].ShowGridHeaders:=value;
end;

function TXLSFile.GetPrintGridLines: boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintGridLines;
end;

function TXLSFile.GetPrintHCentered: boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintHCentered;
end;

procedure TXLSFile.SetPrintGridLines(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintGridLines:=value;
end;


procedure TXLSFile.SetPrintHCentered(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintHCentered := value;
end;

function TXLSFile.GetCellMergedBounds(aRow, aCol: integer): TXlsCellRange;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then
  begin
    //Result := TXlsCellRange.Create(aRow, aCol, aRow, aCol);
    exit;
  end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].CellMergedBounds(aRow-1, aCol-1);
  inc(Result.Left);
  inc(Result.Top);
  inc(Result.Right);
  inc(Result.Bottom);
end;

function TXLSFile.GetCellMergedList(index: integer): TXlsCellRange;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then
  begin
    //Result := TxlsCellRange.Create();
    exit;
  end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].CellMergedList(index);
  inc(Result.Left);
  inc(Result.Top);
  inc(Result.Right);
  inc(Result.Bottom);
end;

function TXLSFile.CellMergedListCount: integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].CellMergedListCount;
end;

procedure TXLSFile.CopyToClipboardFormat(const Range: TXlsCellRange; out textString: string; const xlsStream: TStream);
begin
end;



{$IFDEF FLX_WINDOWSCLIPBOARD}
procedure TXLSFile.CopyToClipboard(const Range: TXlsCellRange);
begin
end;

{$ELSE}

procedure TXLSFile.CopyToClipboard(const Range: TXlsCellRange);
var
  Text: String;
  AsText: TStringStream;
  AsBiff8: TMemoryStream;
begin

  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;

  AsBiff8 := TMemoryStream.Create;
  try
    CopyToClipboardFormat(Range, Text, AsBiff8);
    AsBiff8.Position := 0;

    AsText := TStringStream.Create(Text);
    try
      ClipBoard.SetFormat(RegisterClipboardFormat('Biff8'), AsBiff8);
      ClipBoard.AddFormat(RegisterClipboardFormat('text/plain'), AsText)
    finally
      FreeAndNil(AsText);
    end;
  finally
    FreeAndNil(AsBiff8);
  end;
end;
{$ENDIF}

procedure TXLSFile.CopyToClipboard;
var
  Range: TXlsCellRange;
begin
  Range.Left:=1;
  Range.Top:=1;
  Range.Right:= MaxCol;
  Range.Bottom:= MaxRow;
  CopyToClipboard(Range);
end;


procedure TXlsFile.PasteFromXlsClipboardFormat(const Row, Col: integer; const Stream: TStream);
var
  TempWorkbook: TXlsFile;
  r,c: integer;
  Value: TXlsCellValue;
  XF: TXFRecord;
  d: TDimensionsRec;
  RTFRuns: TRTFRunList;
  fnt: TFlxFont;
  i, Index: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;

  TempWorkbook:=TXlsFile.Create;
  try
    TempWorkbook.Connect;
    try
      TempWorkbook.OpenStream(Stream);

      if (TempWorkbook.SheetCount<=0) or (not TempWorkbook.IsWorksheet(1)) then exit; //Biff8 only pastes one sheet
      d:=TempWorkbook.FWorkbook.WorkSheets[0].OriginalDimensions;
      IsFileModified := true;
      for r:= d.FirstRow to d.LastRow-1 do
        for c:= d.FirstCol to d.LastCol-1 do
        begin
          TempWorkbook.FWorkbook.WorkSheets[0].Cells.CellList.GetValueX2(r,c, Value, RTFRuns);
          if Value.XF<0 then XF:=TempWorkbook.FWorkbook.Globals.XF[0] else XF:= TempWorkbook.FWorkbook.Globals.XF[Value.XF];
          if not FWorkbook.Globals.XF.FindFormat( XF, Value.XF) then
            Value.XF:=FWorkbook.Globals.XF.Add(TXFRecord.CreateFromFormat(XF.FlxFormat(TempWorkbook.FWorkbook.Globals.Fonts, TempWorkbook.FWorkbook.Globals.Formats), FWorkbook.Globals.Fonts, FWorkbook.Globals.Formats));

          if (Length(RTFRuns) > 0) then
          begin
            for i:= 0 to Length(RTFRuns) - 1 do
            begin
              Index := RTFRuns[i].FontIndex;
              if Index=4 then Index:=0;  //font 4 does not exists
              if Index>4 then Index:=Index-1;
              if (Index<0) or (Index>= Tempworkbook.FWorkbook.Globals.Fonts.Count) then Index:=0;
              fnt := TempWorkbook.FWorkbook.Globals.Fonts[Index].FlxFont;
              RTFRuns[i].FontIndex := FWorkbook.Globals.Fonts.AddFont(fnt)
            end;
          end;
          FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(int64(Row)-1+r-d.FirstRow, int64(Col)-1+c-d.FirstCol, Value, RTFRuns, Options1904Dates);
        end;
    finally
      TempWorkbook.Disconnect;
    end;
  finally
    FreeAndNil(TempWorkbook);
  end; //Finally
end;

{$IFNDEF FIREMONKEY}
{$IFDEF FLX_WINDOWSCLIPBOARD}
procedure TXlsFile.PasteFromBiff8(const Row, Col: integer);
begin

end;
{$ENDIF}

procedure TXlsFile.PasteFromText(const Row, Col: integer);
begin
{$IFNDEF FIREMONKEY}
  PasteFromTextClipboardFormat(Row, Col, ClipBoard.AsText);
{$ENDIF}
end;
{$ENDIF}

procedure TXlsFile.PasteFromTextClipboardFormat(const Row, Col: integer; const Data: string);
begin
end;

procedure TXLSFile.PasteFromClipboard(const Row, Col: integer);
begin
  {$IFNDEF FIREMONKEY}
  if Clipboard.HasFormat(RegisterClipboardFormat('Biff8')) then PasteFromBiff8(Row, Col) else
  if Clipboard.HasFormat(CF_TEXT) then PasteFromText(Row, Col);
  {$ENDIF}
end;

{$IFNDEF FLX_WINDOWSCLIPBOARD}
procedure TXlsFile.PasteFromBiff8(const Row, Col: integer);
var
  MemStream: TMemoryStream;
begin
  MemStream:=TMemoryStream.Create;
  try
    ClipBoard.GetFormat(RegisterClipboardFormat('Biff8'), MemStream);
    MemStream.Position:=0;
    PasteFromXlsClipboardFormat(Row, Col, MemStream);
  finally
    FreeAndNil(MemStream);
  end; //finally
end;

{$ENDIF}


function TXLSFile.GetCellFormat(aRow, aCol: integer): integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=-1; exit; end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Value[aRow-1,aCol-1].XF;
end;

procedure TXLSFile.SetCellFormat(aRow, aCol: integer; const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetFormat(aRow-1, aCol-1,Value);
end;

procedure TXLSFile.NewFile(const SheetCount: integer=3);
var
  P: Pointer;
  H: THandle;
  MemStream: TMemoryStream;
begin
  H:=FindResource(HINSTANCE, RESOURCE_EMPTYSHEET, RT_RCDATA);
  P:=LockResource(LoadResource(HINSTANCE, H));
  MemStream:=TMemoryStream.Create;
  try
    MemStream.Write(P^, SizeofResource(HINSTANCE, H));
    MemStream.Position:=0;
    OpenStream(MemStream);
  finally
    FreeAndNil(MemStream);
  end; //finally
  InsertAndCopySheets(-1, 2, SheetCount-1);
end;

procedure TXLSFile.DeleteHPageBreak(const Row: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].DeleteHPageBreak(Row);
end;

procedure TXLSFile.DeleteVPageBreak(const Col: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].DeleteVPageBreak(Col);
end;

procedure TXLSFile.InsertHPageBreak(const Row: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].InsertHPageBreak(Row);
end;

procedure TXLSFile.InsertVPageBreak(const Col: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].InsertVPageBreak(Col);
end;


function TXLSFile.HasAutoFilter: Boolean;
begin
  Result := false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result := FWorkbook.WorkSheets[FActiveSheet-1].HasAutoFilter;
end;

function TXLSFile.HasAutoFilter(const row, col: Int32): Boolean;
begin
  Result := false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result := FWorkbook.WorkSheets[FActiveSheet-1].HasAutoFilter(FActiveSheet - 1, row - 1, col - 1);
end;

function TXLSFile.HasHPageBreak(const Row: integer): boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].HasHPageBreak(Row); //Page break arrays are 1-based
end;

function TXLSFile.HasVPageBreak(const Col: integer): boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].HasVPageBreak(Col); //Page break arrays are 1-based
end;

function TXLSFile.GetMargins: TXlsMargins;
begin
  Result:=FWorkbook.Sheets[FActiveSheet-1].Margins;
end;

procedure TXLSFile.SetMargins(const Value: TXlsMargins);
begin
  FWorkbook.Sheets[FActiveSheet-1].Margins:=Value;
end;


function TXLSFile.GetPrintNumberOfHorizontalPages: word;
begin
  Result:=1;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintNumberOfHorizontalPages;
end;

function TXLSFile.GetPrintNumberOfVerticalPages: word;
begin
  Result:=1;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintNumberOfVerticalPages;
end;

function TXLSFile.GetPrintScale: integer;
begin
  Result:=100;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintScale;
end;

function TXLSFile.GetPrintToFit: boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintToFit;
end;

function TXLSFile.GetPrintVCentered: boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintVCentered;
end;

procedure TXLSFile.SetPrintNumberOfHorizontalPages(const Value: word);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintNumberOfHorizontalPages:=Value;
end;

procedure TXLSFile.SetPrintNumberOfVerticalPages(const Value: word);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintNumberOfVerticalPages:=Value;
end;

procedure TXLSFile.SetPrintScale(const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintScale:=Value;
end;

procedure TXLSFile.SetPrintToFit(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintToFit:=Value;
end;

procedure TXLSFile.SetPrintVCentered(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintVCentered := value;
end;

function TXLSFile.GetPageFooter: UTF16String;
begin
  Result:=FWorkbook.Sheets[FActiveSheet-1].PageFooter;
end;

function TXLSFile.GetPageHeader: UTF16String;
begin
  Result:=FWorkbook.Sheets[FActiveSheet-1].PageHeader;
end;

procedure TXLSFile.SetPageFooter(const Value: UTF16String);
begin
  FWorkbook.Sheets[FActiveSheet-1].PageFooter:=Value;
end;

procedure TXLSFile.SetPageHeader(const Value: UTF16String);
begin
  FWorkbook.Sheets[FActiveSheet-1].PageHeader:=Value;
end;

function TXLSFile.GetCellFormula(aRow, aCol: integer): UTF16String;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; Result:=unassigned; exit; end;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Formula[aRow-1,aCol-1];
end;

procedure TXLSFile.SetCellFormula(aRow, aCol: integer;
  const Value: UTF16String);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
    FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.Formula[aRow-1, aCol-1]:=Value;
    IsFileModified := true;
end;

procedure TXLSFile.AddImage(const Data: ByteArray; const DataType: TXlsImgTypes;
  const Properties: TImageProperties;const Anchor: TFlxAnchorType);
var
  Props: TImageProperties;
begin
  Props:=Properties;
  dec(Props.Col1);
  dec(Props.Col2);
  dec(Props.Row1);
  dec(Props.Row2);

  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.Globals.DrawingGroup.EnsureDwgGroup;
  FWorkbook.WorkSheets[FActiveSheet-1].AddImage(Data, DataType, Props, Anchor);
end;

procedure TXLSFile.ClearImage(const Index: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if (Index<0) or (Index>=FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount) then
    raise exception.CreateFmt(ErrIndexOutBounds,[Index, 'ImageIndex', 0, FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount]);
  FWorkbook.WorkSheets[FActiveSheet-1].ClearImage(Index);
end;

procedure TXLSFile.DeleteImage(const Index: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if (Index<0) or (Index>=FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount) then
    raise exception.CreateFmt(ErrIndexOutBounds,[Index, 'ImageIndex', 0, FWorkbook.WorkSheets[ActiveSheet-1].DrawingCount]);
  FWorkbook.WorkSheets[FActiveSheet-1].DeleteImage(Index);
end;

function TXLSFile.GetCellComment(Row, Col: integer): UTF16String;
var
  Index: integer;
begin
  Result:='';
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  if not (Row-1<FWorkbook.WorkSheets[ActiveSheet-1].Notes.Count) then exit;

  if FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1].Find(Col-1, Index) then
    Result:=FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1][Index].Text;
end;

function TXLSFile.GetCommentColumn(Row, aPos: integer): integer;
begin
  Result:=1;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1)
  or not (Row-1<FWorkbook.WorkSheets[ActiveSheet-1].Notes.Count)
    then exit;

  Result:=FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1][aPos].Column+1;
end;

procedure TXLSFile.SetCellComment(Row, Col: integer;
  const Value: UTF16String; const Properties: TImageProperties);
var
  Index: integer;
  Found: boolean;
  Prop:TImageProperties;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Found:= (Row-1<FWorkbook.WorkSheets[ActiveSheet-1].Notes.Count) and
           FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1].Find(Col-1, Index);

  if Value='' then
    if found then FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1].Delete(Index) else
  else
    if Found then
      FWorkbook.WorkSheets[ActiveSheet-1].Notes[Row-1][Index].Text:= Value
    else
    begin
      Prop:=Properties;
      dec(Prop.Row1);dec(Prop.Row2);dec(Prop.Col1);dec(Prop.Col2);
      FWorkbook.WorkSheets[ActiveSheet-1].AddNewComment(Row-1, Col-1, Value, Prop);
    end;
end;

function TXLSFile.GetSheetZoom: integer;
begin
  //This doesn't have to be a worksheet
  Result:=FWorkbook.Sheets[FActiveSheet-1].SheetZoom;
end;

procedure TXLSFile.SetSheetZoom(const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].SheetZoom:=Value;
end;

procedure TXLSFile.MergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].MergeCells(FirstRow-1, FirstCol-1, LastRow-1, LastCol-1);
end;

procedure TXLSFile.UnMergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].UnMergeCells(FirstRow-1, FirstCol-1, LastRow-1, LastCol-1);
end;

procedure TXLSFile.ParseComments;
begin
  //Nothing
end;


procedure TXLSFile.SetCellFormulaX(aRow, aCol: integer;
  const Formula: UTF16String; const Value: variant);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.AssignFormulaX(aRow-1, aCol-1, Formula, Value, Options1904Dates);
end;

function TXLSFile.GetActiveSheetVisible: TXlsSheetVisible;
begin
  Result:= FWorkbook.Globals.SheetVisible[FActiveSheet-1];
end;

procedure TXLSFile.SetActiveSheetVisible(const Value: TXlsSheetVisible);
begin
  FWorkbook.Globals.SheetVisible[FActiveSheet-1]:= Value;
end;

procedure TXLSFile.AssignCellDataX2(const aRow, aColOffset: integer;
  const Value: TXlsCellValue; const RTFRuns: TRTFRunList);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(aRow-1, FirstColumn + aColOffset, Value, RTFRuns, Options1904Dates);
  IsFileModified := true;
end;

procedure TXLSFile.GetCellDataX2(const aRow, aColOffset: integer;
  out v: TXlsCellValue; out RTFRuns: TRTFRunList);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; v.Value:=unassigned; v.XF:=-1; SetLength(RTFRuns, 0); exit; end;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.GetValueX2(aRow-1,FirstColumn+aColOffset,v,RTFRuns);
end;

procedure TXLSFile.ClearSheet;
begin
  if FWorkbook.IsWorkSheet(FActiveSheet-1) then
  begin;
    FWorkbook.WorkSheets[FActiveSheet-1].ClearValues;
    SelectSheet(FActiveSheet);
    IsFileModified := true;
  end;
end;

procedure TXLSFile.DeleteSheet(aSheetCount: integer);
begin
  if ((SheetCount<=aSheetCount) or(SheetCount<0)) then raise Exception.Create(ErrNoSheetVisible);

  FWorkbook.DeleteSheets(FActiveSheet-1, aSheetCount);
  if (FActiveSheet>= SheetCount) then ActiveSheet:=SheetCount-1;  //Guarantee that ActiveSheet remains valid.
  IsFileModified := true;
end;

function TXLSFile.GetPrintOptions: byte;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=byte(FWorkbook.WorkSheets[FActiveSheet-1].PrintOptions);
end;

procedure TXLSFile.SetPrintOptions(const Value: byte);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintOptions:=Value;
end;

function TXLSFile.GetFontList(index: integer): TFlxFont;
begin
  if Index=4 then Index:=0;  //font 4 does not exists
  if Index>4 then Index:=Index-1;
  if (Index<0) or (Index>=FWorkbook.Globals.Fonts.Count) then Index:=0;
  Result:=FWorkbook.Globals.Fonts[Index].FlxFont;
end;

procedure TXLSFile.SetFontList(index: integer; Value: TFlxFont);
begin
  if Index=4 then exit;  //font 4 does not exists
  if Index>4 then Index:=Index-1;
  if (Index<0) or (Index>=FWorkbook.Globals.Fonts.Count) then Index:=0;
  FWorkbook.Globals.Fonts[Index]:=TFontRecord.CreateFromFlxFont(Value);
end;

function TXLSFile.FontListCount: integer;
begin
  Result:=FWorkbook.Globals.Fonts.Count+1; //Font 4 does not exists!!!
end;

procedure TXLSFile.AssignCellValueX2(aRow, aCol: integer; const Value: TXlsCellValue; const RTFRuns: TRTFRunList);
var
  i: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  for i:=Low(RTFRuns) to High(RTFRuns) do dec(RTFRuns[i].FirstChar);
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.SetValueX2(aRow-1, aCol-1, Value, RTFRuns, Options1904Dates);
  IsFileModified := true;
end;

procedure TXLSFile.GetCellValueX2(aRow, aCol: integer; out v: TXlsCellValue; out RTFRuns: TRTFRunList);
var
  i: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then begin; v.Value:=unassigned; v.XF:=-1; SetLength(RTFRuns, 0); exit; end;
  FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList.GetValueX2(aRow-1,aCol-1,v,RTFRuns);
  for i:=Low(RTFRuns) to High(RTFRuns) do inc(RTFRuns[i].FirstChar);
end;

procedure TXLSFile.AddHyperLink(const CellRange: TXlsCellRange; const value: THyperLink);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].HLinks.Add(THLinkRecord.CreateNew(OffsetRange(CellRange,-1,-1), value));
end;

procedure TXLSFile.DeleteHyperLink(const HyperLinkIndex: integer);
begin
  if (HyperLinkIndex<1) or (HyperLinkIndex>FWorkbook.WorkSheets[ActiveSheet-1].HLinks.Count) then
    raise exception.CreateFmt(ErrIndexOutBounds,[HyperLinkIndex, 'HyperLinkIndex', 1, FWorkbook.WorkSheets[ActiveSheet-1].Hlinks.Count]);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].HLinks.Delete(HyperLinkIndex-1);
end;

function TXLSFile.GetHyperLink(const HyperLinkIndex: integer): THyperLink;
begin
  if (HyperLinkIndex<1) or (HyperLinkIndex>FWorkbook.WorkSheets[ActiveSheet-1].HLinks.Count) then
    raise exception.CreateFmt(ErrIndexOutBounds,[HyperLinkIndex, 'HyperLinkIndex', 1, FWorkbook.WorkSheets[ActiveSheet-1].Hlinks.Count]);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].HLinks[HyperLinkIndex-1].GetProperties;
end;

function TXLSFile.GetHyperLinkCellRange(const HyperLinkIndex: integer): TXlsCellRange;
begin
  if (HyperLinkIndex<1) or (HyperLinkIndex>FWorkbook.WorkSheets[ActiveSheet-1].HLinks.Count) then
    raise exception.CreateFmt(ErrIndexOutBounds,[HyperLinkIndex, 'HyperLinkIndex', 1, FWorkbook.WorkSheets[ActiveSheet-1].Hlinks.Count]);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=OffsetRange(FWorkbook.WorkSheets[FActiveSheet-1].HLinks[HyperLinkIndex-1].GetCellRange, 1,1);
end;

function TXLSFile.HyperLinkCount: integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].HLinks.Count;
end;

procedure TXLSFile.SetHyperLink(const HyperLinkIndex: integer; const value: THyperLink);
begin
  if (HyperLinkIndex<1) or (HyperLinkIndex>FWorkbook.WorkSheets[ActiveSheet-1].HLinks.Count) then
    raise exception.CreateFmt(ErrIndexOutBounds,[HyperLinkIndex, 'HyperLinkIndex', 1, FWorkbook.WorkSheets[ActiveSheet-1].Hlinks.Count]);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].HLinks[HyperLinkIndex-1].SetProperties(value);
end;

procedure TXLSFile.SetHyperLinkCellRange(const HyperLinkIndex: integer; const CellRange: TXlsCellRange);
begin
  if (HyperLinkIndex<1) or (HyperLinkIndex>FWorkbook.WorkSheets[ActiveSheet-1].HLinks.Count) then
    raise exception.CreateFmt(ErrIndexOutBounds,[HyperLinkIndex, 'HyperLinkIndex', 1, FWorkbook.WorkSheets[ActiveSheet-1].Hlinks.Count]);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].HLinks[HyperLinkIndex-1].SetCellRange(OffsetRange(CellRange,-1,-1));
end;

function TXLSFile.GetTWorkbook: TWorkbook;
begin
  Result:=FWorkbook;
end;

function TXLSFile.GetColOutlineLevel(const aCol: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].ColOutlineLevel[aCol-1];
end;

function TXLSFile.GetRowOutlineLevel(const aRow: integer): integer;
begin
  Result:=0;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].RowOutlineLevel[aRow-1];
end;

procedure TXLSFile.SetColOutlineLevel(const FirstCol, LastCol,
  Level: integer);
var
  i: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  for i:= FirstCol-1 to LastCol-1 do
  begin
    FWorkbook.WorkSheets[FActiveSheet-1].ColOutlineLevel[i]:= Level;
  end;
end;

procedure TXLSFile.SetRowOutlineLevel(const FirstRow, LastRow,
  Level: integer);
var
  i: integer;
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  for i:= FirstRow-1 to LastRow-1 do
  begin
    FWorkbook.WorkSheets[FActiveSheet-1].RowOutlineLevel[i]:= Level;
  end;
end;

procedure TXLSFile.SetRangeC1(index, value: integer);
begin
  inherited;
  FWorkbook.Globals.Names[index-1].SetC1(value-1);
end;

procedure TXLSFile.SetRangeC2(index, value: integer);
begin
  inherited;
  FWorkbook.Globals.Names[index-1].SetC2(value-1);
end;

procedure TXLSFile.SetRangeR1(index, value: integer);
begin
  inherited;
  FWorkbook.Globals.Names[index-1].SetR1(value-1);
end;

procedure TXLSFile.SetRangeR2(index, value: integer);
begin
  inherited;
  FWorkbook.Globals.Names[index-1].SetR2(value-1);
end;

function TXLSFile.GetOptions1904Dates: boolean;
begin
  Result:=FWorkbook.Globals.Is1904;
end;

function TXLSFile.GetOptionsR1C1: boolean;
var
  i: integer;
begin
  for i:=0 to FWorkbook.Sheets.Count-1 do
    if FWorkbook.IsWorkSheet(i) then
    begin
      Result:=FWorkbook.WorkSheets[i].IsR1C1;
      exit;
    end;
    Result:=false;
end;

function TXLSFile.GetOptionsPrecisionAsDisplayed: boolean;
begin
  Result:=FWorkbook.Globals.PrecisionAsDisplayed;
end;

function TXLSFile.GetOptionsSaveExternalLinkValues: boolean;
begin
  Result:=FWorkbook.Globals.SaveExternalLinkValues;
end;

procedure TXLSFile.SetOptions1904Dates(const Value: boolean);
begin
  FWorkbook.Globals.Is1904:=Value;
end;

procedure TXLSFile.SetOptionsR1C1(const Value: boolean);
var
  i: integer;
begin
  //Even when it is stored on each sheet, it is a global setting that has to be changed on each.
  for i:=0 to FWorkbook.Sheets.Count-1 do
    if FWorkbook.IsWorkSheet(i) then
      FWorkbook.WorkSheets[i].IsR1C1:=Value;
end;

procedure TXLSFile.SetOptionsPrecisionAsDisplayed(const Value: boolean);
begin
  FWorkbook.Globals.PrecisionAsDisplayed:=Value;
end;

procedure TXLSFile.SetOptionsSaveExternalLinkValues(const Value: boolean);
begin
  FWorkbook.Globals.SaveExternalLinkValues:=Value;
end;

function TXLSFile.GetPrintCopies: integer;
begin
  Result:=1;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintCopies;
end;

function TXLSFile.GetPrinterDriverSettings: TPrinterDriverSettings;
begin
  FillChar(Result.Data,SizeOf(Result.Data),0);
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrinterDriverSettings;
end;

function TXLSFile.GetPrintPaperSize: TExcelPaperSize;
begin
  Result:=TExcelPaperSize_Undefined;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintPaperSize;
end;

function TXLSFile.GetPrintXResolution: integer;
begin
  Result:=600;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintXResolution;
end;

function TXLSFile.GetPrintYResolution: integer;
begin
  Result:=600;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].PrintYResolution;
end;

procedure TXLSFile.SetPrintCopies(const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintCopies:=Value;
end;

procedure TXLSFile.SetPrinterDriverSettings(
  const Value: TPrinterDriverSettings);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrinterDriverSettings:=Value;
end;

procedure TXLSFile.SetPrintPaperSize(const Value: TExcelPaperSize);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintPaperSize:=Value;
end;

procedure TXLSFile.SetPrintXResolution(const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintXResolution:=Value;
end;

procedure TXLSFile.SetPrintYResolution(const Value: integer);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].PrintYResolution:=Value;
end;

procedure TXLSFile.FreezePanes(const Row, Col: integer);
begin
  FWorkbook.Sheets[FActiveSheet-1].FreezePanes(Row - 1, Col - 1);
end;

procedure TXLSFile.GetFrozenPanes(out Row, Col: integer);
begin
  FWorkbook.Sheets[FActiveSheet-1].GetFrozenPanes(Row, Col);
  inc(Row);
  inc(Col);
end;

procedure TXLSFile.GetSplitWindow(out xOffset, yOffset: integer);
begin
  FWorkbook.Sheets[FActiveSheet-1].GetSplitWindow(xOffset, yOffset);
end;

procedure TXLSFile.SplitWindow(const xOffset, yOffset: integer);
begin
  FWorkbook.Sheets[FActiveSheet-1].SplitWindow(xOffset, yOffset);
end;

procedure TXLSFile.AddRange(var NamedRange: TXlsNamedRange);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  dec(NamedRange.NameSheetIndex);
  try
    FWorkbook.Globals.AddName(NamedRange, FWorkbook.WorkSheets[FActiveSheet-1].Cells.CellList);
  finally
    inc(NamedRange.NameSheetIndex);
  end;
end;

function TXLSFile.GetOutlineSummaryColsRightOfDetail: boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].OutlineSummaryColsRightOfDetail;
end;

function TXLSFile.GetOutlineSummaryRowsBelowDetail: boolean;
begin
  Result:=true;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].OutlineSummaryRowsBelowDetail;
end;

function TXLSFile.GetOutlineAutomaticStyles: boolean;
begin
  Result:=false;
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  Result:=FWorkbook.WorkSheets[FActiveSheet-1].OutlineAutomaticStyles;
end;

procedure TXLSFile.SetOutlineSummaryColsRightOfDetail(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].OutlineSummaryColsRightOfDetail:=Value;
end;

procedure TXLSFile.SetOutlineSummaryRowsBelowDetail(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].OutlineSummaryRowsBelowDetail:=Value;
end;

procedure TXLSFile.SetOutlineAutomaticStyles(const Value: boolean);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].OutlineAutomaticStyles:=Value;
end;

procedure TXLSFile.AutofitCol(const Col1, Col2: integer;
  const IgnoreStrings: Boolean; const Adjustment: extended);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].RecalcColWidths(self, col1 - 1, col2 - 1, IgnoreStrings, adjustment);
  RestoreObjectSizes;
end;

procedure TXLSFile.AutofitRow(const row1, row2: integer;
  const AutofitNotAutofittingRows, keepHeightAutomatic: Boolean;
  const adjustment: extended);
begin
  if not FWorkbook.IsWorkSheet(FActiveSheet-1) then exit;
  FWorkbook.WorkSheets[FActiveSheet-1].RecalcRowHeights(self, row1 - 1, row2 - 1, AutofitNotAutofittingRows, keepHeightAutomatic, adjustment);
  RestoreObjectSizes;
end;

procedure TXLSFile.AutofitRowsOnWorkbook(const AutofitNotAutofittingRows,
  KeepSizesAutomatic: Boolean; const Adjustment: extended);
var
  i: integer;
  ws: TWorkSheet;
begin
  for i := 0 to SheetCount - 1 do
  begin
    if FWorkbook.IsWorksheet(i) then
    begin
      ws:=FWorkbook.WorkSheets[i];
      ws.Cells.CellList.RecalcRowHeights(self, 0, ws.Cells.RowList.Count - 1, AutofitNotAutofittingRows, KeepSizesAutomatic, Adjustment);
      ws.RestoreObjectCoords;

    end;
  end;
end;

function TXLSFile.GetInvalidateFormulas: boolean;
begin
  Result := IsFileModified;
end;


function TXLSFile.GetIsXltTemplate: boolean;
begin
  Result := FWorkbook.IsXltTemplate;
end;

procedure TXLSFile.SetInvalidateFormulas(const Value: boolean);
begin
  IsFileModified := value;
end;


procedure TXLSFile.SetIsXltTemplate(const Value: boolean);
begin
  FWorkbook.IsXltTemplate := Value;
end;

end.
