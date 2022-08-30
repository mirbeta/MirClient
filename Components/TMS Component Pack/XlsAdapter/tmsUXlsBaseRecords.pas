unit tmsUXlsBaseRecords;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses Sysutils, Contnrs, Classes,
    {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF}
     tmsXlsMessages, tmsUFlxMessages, tmsUOle2Impl;

type
  TContinueRecord=class;

  TBaseRecord = class (TObject)
  public
    Id: word;
    Data: PArrayOfByte;
    DataSize: word;

    Continue: TContinueRecord;

    procedure SaveDataToStream(const Workbook: TOle2File; const aData: PArrayOfByte);
  protected
    function DoCopyTo: TBaseRecord; virtual;
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);virtual;
    destructor Destroy; override;
    procedure AddContinue(const aContinue: TContinueRecord);

    procedure SaveToStream(const Workbook: TOle2File); virtual;
    function CopyTo: TBaseRecord;  //this should be non-virtual
    function TotalSize: integer;virtual;
    function TotalSizeNoHeaders: integer;virtual;
  end;

  ClassOfTBaseRecord= Class of TBaseRecord;

  TContinueRecord=class(TBaseRecord)
  end;

  TIgnoreRecord = class (TBaseRecord)
    function TotalSize: integer; override;
    procedure SaveToStream(const Workbook: TOle2File); override;
  end;

  TSubListRecord = class (TBaseRecord)  //This is a "virtual" record used to save sublists to stream
  private
    FSubList: TObjectList;
  protected
    function DoCopyTo: TBaseRecord; override;

  public
    constructor  CreateAndAssign(const aSubList: TObjectList);
    function TotalSize: integer; override;
    procedure SaveToStream(const Workbook: TOle2File); override;
  end;

  TBaseRowColRecord = class(TBaseRecord)
  private
    function GetColumn: word;
    function GetRow: word;
    procedure SetColumn( Value: word );
    procedure SetRow( Value: word );
  public
    property Row: word read GetRow write SetRow;
    property Column: word read GetColumn write SetColumn;

    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);virtual;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);virtual;
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    function FixTotalSize(const NeedsRecalc: boolean): int64; virtual;
 end;

  TCellRecord=class(TBaseRowColRecord)
  private
    function GetXF: word;
    procedure SetXF(const Value: word);
  protected
    function GetValue: Variant; virtual;
    procedure SetValue(const Value: Variant); virtual;
  public
    property XF: word read GetXF write SetXF;
    property Value:Variant read GetValue write SetValue;
    constructor CreateFromData(const aId, aDataSize, aRow, aCol, aXF: word);

    function CanJoinNext(const NextRecord: TCellRecord; const MaxCol: integer): boolean;virtual;
    procedure SaveFirstMul(const Workbook: TOle2File; const JoinedRecordSize: Word);virtual;
    procedure SaveMidMul(const Workbook: TOle2File);virtual;
    procedure SaveLastMul(const Workbook: TOle2File);virtual;
    function TotalSizeFirst: integer; virtual;
    function TotalSizeMid: integer; virtual;
    function TotalSizeLast: integer;virtual;

  end;

  TRowRecord=class(TBaseRowColRecord)
  private
    function GetHeight: word;
    function GetMaxCol: word;
    function GetMinCol: word;
    function GetXF: word;
    procedure SetHeight(const Value: word);
    procedure SetMaxCol(const Value: word);
    procedure SetMinCol(const Value: word);
    procedure SetXF(const Value: word);
    function GetOptions: word;
    procedure SetOptions(const Value: word);
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateStandard(const Row: word);
    function GetRow: Word;

    property MaxCol: word read GetMaxCol write SetMaxCol;
    property MinCol: word read GetMinCol write SetMinCol;
    property Height: word read GetHeight write SetHeight;
    property XF: word read GetXF write SetXF;
    function IsFormatted: boolean;
    function IsModified: boolean;
    property Options: word read GetOptions write SetOptions;

    procedure ManualHeight;
    procedure AutoHeight;
    procedure Hide(const value: boolean);
    function IsAutoHeight: boolean;
    function IsHidden: boolean;
    procedure SaveRangeToStream(const DataStream: TOle2File; const aMinCol, aMaxCol: integer);

    procedure SetRowOutlineLevel(const Level: integer);
  end;

  TDimensionsRec=packed record
    FirstRow, LastRow: LongWord;
    FirstCol, LastCol: Word;
    Extra: word;
  end;
  PDimensionsRec=^TDimensionsRec;
  
  TDimensionsRecord=class(TBaseRecord)
    function Dim: PDimensionsRec;
  end;

  TStringRecord=class(TBaseRecord)
  public
    procedure SaveToStream(const Workbook: TOle2File); override;
    function TotalSize: integer; override;
    function Value: UTF16String;
  end;

  TWindow1Record=class(TBaseRecord)
  private
    function GetActiveSheet: integer;
    procedure SetActiveSheet(const Value: integer);
    function GetFirstSheetVisible: integer;
    procedure SetFirstSheetVisible(const Value: integer);
    procedure MakeMoreThan1(const p: integer);
  public
    property ActiveSheet: integer read GetActiveSheet write SetActiveSheet;
    property FirstSheetVisible: integer read GetFirstSheetVisible write SetFirstSheetVisible;
    procedure SaveToStream(const Workbook: TOle2File); override;
  end;

  TWindow2Record=class(TBaseRecord)
  private
    function GetSelected: boolean;
    procedure SetSelected(const Value: boolean);
    function GetShowGridLines: boolean;
    procedure SetShowGridLines(const Value: boolean);
    function GetShowGridHeaders: boolean;
    procedure SetShowGridHeaders(const Value: boolean);
    procedure SetSheetZoom(const Value: integer);
    function GetSheetZoom: integer;
    function GetIsFrozen: Boolean;
    function GetIsFrozenButNoSplit: Boolean;
    procedure SetIsFrozen(const value: Boolean);
    procedure SetIsFrozenButNoSplit(const value: Boolean);
  protected
    function DoCopyTo: TBaseRecord; override;
  public
    property Selected: boolean read GetSelected write SetSelected;
    property ShowGridLines: boolean read GetShowGridLines write SetShowGridLines;
    property ShowGridHeaders: boolean read GetShowGridHeaders write SetShowGridHeaders;
    property SheetZoom: integer read GetSheetZoom write SetSheetZoom;

    property IsFrozen: Boolean read GetIsFrozen write SetIsFrozen;
    property IsFrozenButNoSplit: Boolean read GetIsFrozenButNoSplit write SetIsFrozenButNoSplit;

  end;

  TSCLRecord=class(TBaseRecord)
  private
    function GetZoom: integer;
    procedure SetZoom(const Value: integer);
  public
    constructor CreateFromData(const aZoom: integer);
    property Zoom: integer read GetZoom write SetZoom;
  end;

  TDefColWidthRecord = class(TBaseRecord)
  public
    function Width: Word;
    procedure SaveToStream(const Workbook: TOle2File); override;
  end;

  TStandardWidthRecord = class(TBaseRecord)
  public
    function Width: Word;
  end;

  TDefRowHeightRecord = class(TBaseRecord)
  public
    function Height: Word;
  end;

  TPageHeaderFooterRecord = class(TBaseRecord)
  private
    function GetText: UTF16String;
    procedure SetText(const Value: UTF16String);
  public
    property Text: UTF16String read GetText write SetText;
  end;

  TPageHeaderRecord = class(TPageHeaderFooterRecord)
  end;

  TPageFooterRecord = class(TPageHeaderFooterRecord)
  end;

  TPrintGridLinesRecord = class(TBaseRecord)
  private
    function GetValue: boolean;
    procedure SetValue(const Value: boolean);
  public
    property Value: boolean read GetValue write SetValue;
  end;


  TMarginRecord=class(TBaseRecord)
  private
    function GetValue: double;
    procedure SetValue(const Value: double);
  public
    property Value: double read GetValue write SetValue;
  end;

  TSetupRec=packed record
    PaperSize: word;
    Scale: word;
    PageStart: word;
    FitWidth: word;
    FitHeight: word;
    GrBit: word;
    Resolution: word;
    VResolution: word;
    HeaderMargin: double;
    FooterMargin: double;
    Copies: word;
  end;
  PSetupRec=^TSetupRec;

  TSetupRecord=class(TBaseRecord)
  private
    function GetValue: TSetupRec;
    procedure SetValue(const Value: TSetupRec);
    function GetScale: word;
    procedure SetScale(const Value: word);
    function GetFitHeight: word;
    function GetFitWidth: word;
    procedure SetFitHeight(const Value: word);
    procedure SetFitWidth(const Value: word);
    function GetFooterMargin: extended;
    function GetHeaderMargin: extended;
    procedure SetFooterMargin(const Value: extended);
    procedure SetHeaderMargin(const Value: extended);
    function GetPrintOptions: word;
    procedure SetPrintOptions(const Value: word);
    function GetPrintCopies: integer;
    function GetPrintPaperSize: TExcelPaperSize;
    function GetPrintXResolution: integer;
    function GetPrintYResolution: integer;
    procedure SetPrintCopies(const Value: integer);
    procedure SetPrintPaperSize(const Value: TExcelPaperSize);
    procedure SetPrintXResolution(const Value: integer);
    procedure SetPrintYResolution(const Value: integer);
  public
    property Value: TSetupRec read GetValue write SetValue;
    property Scale: word read GetScale write SetScale;
    property PrintOptions: word read GetPrintOptions write SetPrintOptions;
    property FitWidth: word read GetFitWidth write SetFitWidth;
    property FitHeight: word read GetFitHeight write SetFitHeight;

    property PrintPaperSize: TExcelPaperSize read GetPrintPaperSize write SetPrintPaperSize;
    property PrintCopies: integer read GetPrintCopies write SetPrintCopies;
    property PrintXResolution: integer read GetPrintXResolution write SetPrintXResolution;
    property PrintYResolution: integer read GetPrintYResolution write SetPrintYResolution;

    property HeaderMargin: extended read GetHeaderMargin write SetHeaderMargin;
    property FooterMargin: extended read GetFooterMargin write SetFooterMargin;
  end;

  TPlsRecord=class(TBaseRecord)
  private
    function GetPrinterDriverSettings: TPrinterDriverSettings;
    procedure SetPrinterDriverSettings(
      const Value: TPrinterDriverSettings);
  public
    constructor CreateFromData(aPrinterData: TPrinterDriverSettings);

    property PrinterData: TPrinterDriverSettings read GetPrinterDriverSettings write SetPrinterDriverSettings;
  end;

  TPrintCenteredRecord = class(TBaseRecord)
  private
    function GetCentered: boolean;
    procedure SetCentered(const Value: boolean);
    public
    property Centered: boolean read GetCentered write SetCentered;
  end;

  TWsBoolRecord=class(TBaseRecord)
  private
    function GetValue: word;
    procedure SetValue(const Value: word);
    function GetFitToPage: boolean;
    procedure SetFitToPage(const Value: boolean);
    function GetOutlineSummaryColsRightOfDetail: boolean;
    function GetOutlineSummaryRowsBelowDetail: boolean;
    function GetOutlineAutomaticStyles: boolean;
    procedure SetOutlineRightOfDetail(const Value: boolean);
    procedure SetOutlineSummaryRowsBelowDetail(const Value: boolean);
    procedure SetOutlineAutomaticStyles(const Value: boolean);
  public
    property Value: word read GetValue write SetValue;
    property FitToPage: boolean read GetFitToPage write SetFitToPage;
    property OutlineSummaryRowsBelowDetail: boolean read GetOutlineSummaryRowsBelowDetail write SetOutlineSummaryRowsBelowDetail;
    property OutlineSummaryColsRightOfDetail: boolean read GetOutlineSummaryColsRightOfDetail write SetOutlineRightOfDetail;
    property OutlineAutomaticStyles: boolean read GetOutlineAutomaticStyles write SetOutlineAutomaticStyles;
  end;

  T1904Record = class(TBaseRecord)
  private
    function GetIs1904: boolean;
    procedure SetIs1904(const Value: boolean);
  public
    property Is1904: boolean read GetIs1904 write SetIs1904;
  end;

  TRefModeRecord = class(TBaseRecord)
  private
    function GetIsR1C1: boolean;
    procedure SetIsR1C1(const Value: boolean);
  public
    property IsR1C1: boolean read GetIsR1C1 write SetIsR1C1;
  end;

  TPrecisionRecord = class(TBaseRecord)
  private
    function GetPrecisionAsDisplayed: boolean;
    procedure SetPrecisionAsDisplayed(const Value: boolean);
  public
    property PrecisionAsDisplayed: boolean read GetPrecisionAsDisplayed write SetPrecisionAsDisplayed;
  end;

  TBookBoolRecord = class(TBaseRecord)
  private
    function GetSaveExternalLinkValues: boolean;
    procedure SetSaveExternalLinkValues(const Value: boolean);
  public
    property SaveExternalLinkValues: boolean read GetSaveExternalLinkValues write SetSaveExternalLinkValues;
  end;

  /// <summary>
  /// AutoFilter Information
  /// </summary>
  TAutoFilterInfoRecord = class (TBaseRecord)
  private
    function Get_DropDownCount(): Int32;
    procedure Set_DropDownCount(const value: Int32);

  public
    property DropDownCount: Int32 read Get_DropDownCount write Set_DropDownCount;
  end;




//------------------------------------ Utility functions
  function LoadRecords(const DataStream: TOle2File; var RecordHeader: TRecordHeader): TBaseRecord;
  procedure ReadMem(var aRecord: TBaseRecord; var aPos: integer; const aSize: integer; const pResult: pointer);
  procedure ReadStr(var aRecord: TBaseRecord; var aPos: integer; var ShortData: AnsiString; var WideData: UTF16String; var OptionFlags, ActualOptionFlags: byte; var DestPos: integer; const StrLen: integer );

implementation
uses tmsUXlsFormula, tmsUXlsOtherRecords, tmsUXlsSST, tmsUXlsReferences, tmsUXlsCondFmt, tmsUXlsChart, tmsUXlsEscher,
     tmsUXlsNotes, tmsUXlsCellRecords, tmsUXlsPageBreaks, tmsUXlsStrings, tmsUXlsColInfo, tmsUXlsXF,
     tmsUXlsBaseRecordLists, tmsUXlsPalette, tmsUXlsHyperLink;

//------------------------------------ Utility functions

procedure ReadMem(var aRecord: TBaseRecord; var aPos: integer; const aSize: integer; const pResult: pointer);
//Read memory taking in count "Continue" Records
var
  l: integer;
begin
  l:= aRecord.DataSize-aPos;

  if l<0 then raise Exception.Create(ErrReadingRecord);
  if (l=0) and (aSize>0) then
  begin
    aPos:=0;
    aRecord:=aRecord.Continue;
    if aRecord=nil then raise Exception.Create(ErrReadingRecord);
  end;

  l:= aRecord.DataSize-aPos;

  if aSize<=l then
  begin
    if pResult<>nil then Move(aRecord.Data^[aPos], pResult^, aSize);
    inc(aPos, aSize);
  end else
  begin
    ReadMem(aRecord, aPos, l, pResult);
    if pResult<>nil then ReadMem(aRecord, aPos, aSize-l, PAddress(pResult)+ l)
    else ReadMem(aRecord, aPos, aSize-l, nil);
  end
end;

procedure ReadStr(var aRecord: TBaseRecord; var aPos: integer; var ShortData: AnsiString; var WideData: UTF16String; var OptionFlags, ActualOptionFlags: byte; var DestPos: integer; const StrLen: integer );
//Read a string taking in count "Continue" Records
var
  l,i: integer;
  pResult: pointer;
  aSize, CharSize: integer;
begin
  l:= aRecord.DataSize-aPos;

  if l<0 then raise Exception.Create(ErrReadingRecord);
  if (l=0) and (StrLen>0) then
  // This is not a valid Excel thing, but if it is (f.i. on JET exported files), the optionflags will be repeated.
    {if DestPos=0 then  //we are beginning the record
    begin
      aPos:=0;
      if aRecord.Continue=nil then raise Exception.Create(ErrReadingRecord);
      aRecord:=aRecord.Continue;
    end else }
    begin       //We are in the middle of a string
      aPos:=1;
      if aRecord.Continue=nil then raise Exception.Create(ErrReadingRecord);
      aRecord:=aRecord.Continue;
      ActualOptionFlags:=aRecord.Data[0];
      if (ActualOptionFlags=1) and ((OptionFlags and 1)=0 ) then
      begin
        WideData:=StringToWideStringNoCodePage(ShortData);
        OptionFlags:= OptionFlags or 1;
      end;
    end;

  l:= aRecord.DataSize-aPos;

  if (ActualOptionFlags and 1)=0 then
  begin
    aSize:= StrLen-DestPos;
    pResult:= @ShortData[DestPos+1];
    CharSize:=1;
  end else
  begin
    aSize:= (StrLen-DestPos)*2;
    pResult:= @WideData[DestPos+1];
    CharSize:=2;
  end;

  if aSize<=l then
  begin
    if (ActualOptionFlags and 1=0) and (OptionFlags and 1=1) then
      //We have to move result to widedata
      for i:=0 to aSize div CharSize -1 do WideData[DestPos+1+i]:=UTF16Char(aRecord.Data^[aPos+i])
      //We are either reading widedata or shortdata
      else Move(aRecord.Data^[aPos], pResult^, aSize);

    inc(aPos, aSize);
    inc(DestPos, aSize div CharSize);
  end else
  begin
    if (ActualOptionFlags and 1=0) and (OptionFlags and 1=1) then
      //We have to move result to widedata
      for i:=0 to l div CharSize -1 do WideData[DestPos+1+i]:=UTF16Char(aRecord.Data^[aPos+i])
      //We are either reading widedata or shortdata
      else  Move(aRecord.Data^[aPos], pResult^, l);
    inc(aPos, l);
    inc(DestPos, l div CharSize);
    ReadStr(aRecord, aPos, ShortData, WideData, OptionFlags, ActualOptionFlags, DestPos ,StrLen);
  end
end;


procedure LoadContinues(const DataStream: TOle2File; const Master: TBaseRecord; var RecordHeader: TRecordHeader);
var
  LastRecord: TBaseRecord;
  Id: Int32;
  Data: PArrayOfByte;
  R: TContinueRecord;
begin
  LastRecord := Master;
  repeat
    Id := RecordHeader.Id;
    Assert(Id = Int32(xlr_CONTINUE));

    GetMem (Data, RecordHeader.Size);
    try
      DataStream.ReadMem(Data[0], RecordHeader.Size);

      R := TContinueRecord.Create(Id, Data, RecordHeader.Size);
    except
      FreeMem(Data);
      raise;
    end;
    LastRecord.AddContinue(R);
    LastRecord := R;
    if not DataStream.NextEof(3) then
    begin
      DataStream.ReadMem(RecordHeader, SizeOf(RecordHeader));
    end
    else
    begin
       //Array.Clear(RecordHeader.Data,0,RecordHeader.Length);
      RecordHeader.Id := Int32(xlr_EOF);  //Return EOFs, so in case of bad formed files we don't get on an infinite loop.
      RecordHeader.Size := 0;
    end;

  until RecordHeader.Id <> Int32(xlr_CONTINUE);
end;


function LoadRecords(const DataStream: TOle2File; var RecordHeader: TRecordHeader): TBaseRecord;
var
  Data: PArrayOfByte;
  R: TBaseRecord;
begin
  GetMem(Data, RecordHeader.Size);
  try
    DataStream.ReadMem(Data^, RecordHeader.Size);
  except
    FreeMem(Data);
    raise;
  end; //except

  //From here, if there is an exception, the mem will be freed by the object
  case RecordHeader.Id of
    xlr_BOF         : R:= TBOFRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_EOF         : R:= TEOFRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_FORMULA     : R:= TFormulaRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_SHRFMLA     : R:= TShrFmlaRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_OBJ         : R:= TObjRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_MSODRAWING  : R:= TDrawingRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_MSODRAWINGGROUP
                    : R:= TDrawingGroupRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_TEMPLATE    : R:= TTemplateRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_TXO         : R:= TTXORecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_NOTE        : R:= TNoteRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_RECALCID,   //So the workbook gets recalculated
    xlr_EXTSST,     // We will have to generate this again
    xlr_DBCELL,     //To find rows in blocks... we need to calculate it again
    xlr_INDEX,      //Same as DBCELL
    xlr_MSODRAWINGSELECTION,   // Object selection. We do not need to select any drawing
//    ,xlr_OBPROJ  If we want to disable macros.
    xlr_TABID       //Office 2010 won't like it if we don't generate it right.
                    : R:= TIgnoreRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_DIMENSIONS  //Used range of a sheet
                    : R:= TDimensionsRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_SST         : R:= TSSTRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_BoundSheet  : R:= TBoundSheetRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_CODENAME    : R:= TCodeNameRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_OBPROJ      : R:= TObProjRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_Array       : R:= TArrayRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Blank       : R:= TBlankRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_BoolErr     : R:= TBoolErrRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Number      : R:= TNumberRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_MulBlank    : R:= TMulBlankRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_MulRK       : R:= TMulRKRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_RK          : R:= TRKRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_STRING      : R:= TStringRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);//String record saves the result of a formula

    xlr_XF          : R:= TXFRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_FONT        : R:= TFontRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_FORMAT      : R:= TFormatRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Palette     : R:= TPaletteRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Style       : R:= TStyleRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_LabelSST    : R:= TLabelSSTRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Label       : R:= TLabelRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_RSTRING     : R:= TRStringRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Row         : R:= TRowRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_NAME        : R:= TNameRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_TABLE       : R:= TTableRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_CELLMERGING : R:= TCellMergingRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_CONDFMT     : R:= TCondFmtRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_CF          : R:= TCFRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_DVAL        : R:= TDValRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_HLINK       : R:= THLinkRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_SCREENTIP   : R:= TScreenTipRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Continue    : R:= TContinueRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_FOOTER      : R:= TPageFooterRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_HEADER      : R:= TPageHeaderRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_PRINTGRIDLINES : R:= TPrintGridLinesRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_LEFTMARGIN,
    xlr_RIGHTMARGIN,
    xlr_TOPMARGIN,
    xlr_BOTTOMMARGIN: R:= TMarginRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_SETUP       : R:= TSetupRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_PLS         : R:= TPlsRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_WSBOOL      : R:= TWsBoolRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_HCENTER,
    xlr_VCENTER     : R:= TPrintCenteredRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_XCT,        // Cached values of a external workbook... not supported yet
    xlr_CRN         // Cached values also
                    : R:=TIgnoreRecord.Create(RecordHeader.Id, Data, RecordHeader.Size); //raise Exception.Create (ErrExtRefsNotSupported);
    xlr_SUPBOOK     : R:= TSupBookRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_EXTERNSHEET : R:= TExternSheetRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_EXTERNNAME,
    xlr_EXTERNNAME2
                    : R:= TExternNameRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_ChartAI     : R:= TChartAIRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Window1     : R:= TWindow1Record.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_Window2     : R:= TWindow2Record.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_1904        : R:= T1904Record.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_REFMODE     : R:= TRefModeRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_PRECISION   : R:= TPrecisionRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_BOOKBOOL    : R:= TBookBoolRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_PANE        : R:= TPaneRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_AUTOFILTERINFO : R:= TAutoFilterInfoRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_SCL         : R:= TSCLRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_GUTS        : R:= TGutsRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_HORIZONTALPAGEBREAKS: R:= THPageBreakRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_VERTICALPAGEBREAKS  : R:= TVPageBreakRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_COLINFO     : R:= TColInfoRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_DEFCOLWIDTH : R:= TDefColWidthRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_STANDARDWIDTH:R:= TStandardWidthRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_DEFAULTROWHEIGHT: R:= TDefRowHeightRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_FILEPASS: raise Exception.Create(ErrFileIsPasswordProtected);

    xlr_BEGIN: R:= TBeginRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
    xlr_END: R:= TEndRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);

    xlr_ChartFbi: R:=TIgnoreRecord.Create(RecordHeader.Id, Data, RecordHeader.Size); //charfbi might be dangerous if copied.

    else              R:= TBaseRecord.Create(RecordHeader.Id, Data, RecordHeader.Size);
  end; //case

  //Peek at the next record...
  if not DataStream.NextEof(3) then
  begin
    DataStream.ReadMem(RecordHeader, SizeOf(RecordHeader));
    if RecordHeader.Id = xlr_Continue then LoadContinues(DataStream, R, RecordHeader)
    else if RecordHeader.Id=xlr_Table then
      if (R is TFormulaRecord) then
      begin
        (R as TFormulaRecord).TableRecord:=LoadRecords(DataStream, RecordHeader) as TTableRecord;
      end
      else Exception.Create(ErrExcelInvalid)
    else if RecordHeader.Id=xlr_Array then
      if (R is TFormulaRecord) then
      begin
        (R as TFormulaRecord).ArrayRecord:=LoadRecords(DataStream, RecordHeader) as TArrayRecord;
      end
      else Exception.Create(ErrExcelInvalid)

    else if RecordHeader.Id=xlr_ScreenTip then
      if (R is THLinkRecord) then
      begin
        (R as THLinkRecord).AddHint(LoadRecords(DataStream, RecordHeader) as TScreenTipRecord);
      end
      else Exception.Create(ErrExcelInvalid)
    else
    begin
      if RecordHeader.Id = xlr_String then
      begin
        if not (R is TFormulaRecord) and not (R is TShrFmlaRecord) and not (R is TArrayRecord) and not (R is TTableRecord) then raise Exception.Create(ErrExcelInvalid);
      end;
    end;
  end else
  begin
    RecordHeader.Id := xlr_EOF;
    RecordHeader.Size := 0;
  end;

  Result:=R;
end;

{ TBaseRecord }

procedure TBaseRecord.AddContinue(const aContinue: TContinueRecord);
begin
  if Continue<>nil then raise Exception.Create(ErrInvalidContinue);
  Continue:=aContinue;
end;

function TBaseRecord.CopyTo: TBaseRecord;
begin
  if Self=nil then Result:= nil   //for this to work, this cant be a virtual method
  else Result:=DoCopyTo;
end;

constructor TBaseRecord.Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited Create;
  Id := aId;
  Data := aData;
  DataSize := aDataSize;
end;

destructor TBaseRecord.Destroy;
begin
  if Data<>nil then FreeMem(Data);
  FreeAndNil(Continue);
  inherited;
end;

function TBaseRecord.DoCopyTo: TBaseRecord;
var
  NewData: PArrayOfByte;
begin
  GetMem(NewData, DataSize);
  try
    Move(Data^, NewData^, DataSize);
    Result:= ClassOfTBaseRecord(ClassType).Create(Id, NewData, DataSize);
  except
    FreeMem(NewData);
    raise;
  end;
  if Continue<>nil then Result.Continue:= Continue.CopyTo as TContinueRecord;
end;

procedure TBaseRecord.SaveDataToStream(const Workbook: TOle2File;
  const aData: PArrayOfByte);
begin
  Workbook.WriteMem(Id, Sizeof(Id));
  Workbook.WriteMem(DataSize, Sizeof(DataSize));
  if DataSize > 0 then
    Workbook.WriteMem(aData^, DataSize);
end;

procedure TBaseRecord.SaveToStream(const Workbook: TOle2File);
begin
  SaveDataToStream(Workbook, Data);
  if Continue<>nil then Continue.SaveToStream(Workbook);
end;

function TBaseRecord.TotalSize: integer;
begin
  Result:=SizeOf(TRecordHeader)+ DataSize;
  if Continue<>nil then Result:=Result+Continue.TotalSize;
end;

function TBaseRecord.TotalSizeNoHeaders: integer;
begin
  Result:=DataSize;
  if Continue<>nil then Result:=Result+Continue.TotalSizeNoHeaders;
end;

{ TBaseRowColRecord }

procedure TBaseRowColRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
begin
  if DataSize<4 then raise Exception.CreateFmt(ErrWrongExcelRecord,[Id]);
  if (SheetInfo.InsSheet<0) or (SheetInfo.FormulaSheet<> SheetInfo.InsSheet) then exit;
  if aRowPos<= Row then IncWord(Data, 0, aRowCount, Max_Rows);  //row;
  //Issues if we have 256 columns.
  if aColPos<= Column then IncWord(Data, 2, aColCount, Max_Columns);  //col;
end;

constructor TBaseRowColRecord.Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited;
  if DataSize<4 then raise Exception.CreateFmt(ErrWrongExcelRecord,[Id]);
end;

function TBaseRowColRecord.FixTotalSize(const NeedsRecalc: boolean): int64;
begin
  Result := TotalSize;
end;

procedure TBaseRowColRecord.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
begin
  if DataSize<4 then raise Exception.CreateFmt(ErrWrongExcelRecord,[Id]);
  SetWord(Data, 0, Row+RowOffset);  //row;
  SetWord(Data, 2, Column+ColOffset);  //col;
end;

function TBaseRowColRecord.GetColumn: word;
begin
  GetColumn:=GetWord(Data,2);
end;

function TBaseRowColRecord.GetRow: word;
begin
  GetRow:=GetWord(Data,0);
end;

procedure TBaseRowColRecord.SetColumn(Value: word);
begin
  SetWord(Data,2,Value);
end;

procedure TBaseRowColRecord.SetRow(Value: word);
begin
  SetWord(Data,0,Value);
end;

{ TIgnoreRecord }

procedure TIgnoreRecord.SaveToStream(const Workbook: TOle2File);
begin
  //nothing
end;

function TIgnoreRecord.TotalSize: integer;
begin
  Result:=0;
end;

{ TStringRecord }
//We won't write out this record

procedure TStringRecord.SaveToStream(const Workbook: TOle2File);
begin
  //Nothing.
end;

function TStringRecord.TotalSize: integer;
begin
  Result:=0;
end;

function TStringRecord.Value: UTF16String;
var
  xs: TExcelString;
  Myself: TBaseRecord;
  Ofs: integer;
begin
  Myself:=Self;Ofs:=0;
  xs:=TExcelString.Create(2, Myself, Ofs);
  try
    Result:=Xs.Value;
  finally
    freeAndNil(xs);
  end;
end;


{ TRowRecord }

constructor TRowRecord.Create(const aId: word; const aData: PArrayOfByte;
  const aDataSize: integer);
begin
  inherited;
  //Set irwMac=0
  SetWord(Data, 8, 0);
end;

constructor TRowRecord.CreateStandard(const Row: word);
var
  MyData: PArrayOfByte;
begin
  GetMem(myData, 16);
  FillChar(myData^,16, 0);
  SetWord(myData, 0, Row);
  SetWord(myData, 6, $FF);
  myData[13]:=1;
  myData[14]:=$0F; //Default format.
  inherited Create(xlr_ROW, myData, 16);
end;

function TRowRecord.GetHeight: word;
begin
  Result:=GetWord(Data, 6);
end;

function TRowRecord.GetMaxCol: word;
begin
  Result:=GetWord(Data, 4);
end;

function TRowRecord.GetMinCol: word;
begin
  Result:=GetWord(Data, 2);
end;

function TRowRecord.GetXF: word;
begin
  if IsFormatted then Result:=GetWord(Data, 14) and $FFF else Result:=15;
end;

function TRowRecord.GetRow: Word;
begin
  Result:= GetWord(Data, 0);
end;

procedure TRowRecord.SetHeight(const Value: word);
begin
  SetWord( Data, 6, Value);
end;

procedure TRowRecord.SetMaxCol(const Value: word);
begin
  SetWord( Data, 4, Value);
end;

procedure TRowRecord.SetMinCol(const Value: word);
begin
  SetWord( Data, 2, Value);
end;

procedure TRowRecord.ManualHeight;
begin
  Data[12]:= Data[12] or $40;
end;

procedure TRowRecord.AutoHeight;
begin
  Data[12]:= Data[12] and not $40;
end;

procedure TRowRecord.Hide(const value: boolean);
begin
  if Value then Data[12]:= Data[12] or $20 else Data[12]:= Data[12] and not $20;
end;

function TRowRecord.IsAutoHeight: boolean;
begin
  Result:=  not (Data[12] and $40 = $40);
end;

function TRowRecord.IsHidden: boolean;
begin
  Result:= (Data[12] and $20 = $20);
end;

procedure TRowRecord.SetXF(const Value: word);
begin
  Data[12]:= Data[12] or $80;
  Data[13]:= Data[13] or $01;
  SetWord(Data, 14, Value);
end;

procedure TRowRecord.SaveRangeToStream(const DataStream: TOle2File; const aMinCol, aMaxCol: integer);
var
  sMinCol, sMaxCol: integer;
begin
  sMinCol:=MinCol;
  sMaxCol:=MaxCol;
  try
    if sMinCol<aMinCol then MinCol:=aMinCol;
    if sMaxCol>aMaxCol+1 then MaxCol:=aMaxCol+1;
    inherited SaveToStream(DataStream);
  finally
    MinCol:=sMinCol;
    MaxCol:=sMaxCol;
  end; //Finally

end;

function TRowRecord.IsFormatted: boolean;
begin
  Result:=Data[12] and $80= $80;
end;

function TRowRecord.IsModified: boolean;
begin
  Result:=(Data[12]<>0) or (Data[13]<>1);
end;

function TRowRecord.GetOptions: word;
begin
  Result:=GetWord(Data, 12);
end;

procedure TRowRecord.SetOptions(const Value: word);
begin
  SetWord(Data, 12, Value);
end;

procedure TRowRecord.SetRowOutlineLevel(const Level: integer);
begin
  Data[12]:=(Data[12] and not 7) or (Level and 7);
end;

{ TCellRecord }

function TCellRecord.CanJoinNext(const NextRecord: TCellRecord;
  const MaxCol: integer): boolean;
begin
  Result:=false;
end;

constructor TCellRecord.CreateFromData(const aId, aDataSize, aRow, aCol, aXF: word);
var
  aData: pointer;
begin
  GetMem(aData, aDataSize);
  FillChar(aData^, aDataSize, 0);
  Create(aId, aData, aDataSize);
  Row:=aRow;
  Column:=aCol;
  XF:=aXF;
end;

function TCellRecord.GetValue: Variant;
begin
  Result:=unassigned;
end;

function TCellRecord.GetXF: word;
begin
  Result:= GetWord(Data, 4);
end;

procedure TCellRecord.SaveFirstMul(const Workbook: TOle2File;
  const JoinedRecordSize: Word);
begin
  SaveToStream(Workbook);
end;

procedure TCellRecord.SaveLastMul(const Workbook: TOle2File);
begin

end;

procedure TCellRecord.SaveMidMul(const Workbook: TOle2File);
begin

end;

procedure TCellRecord.SetValue(const Value: Variant);
begin
  //Nothing
end;

procedure TCellRecord.SetXF(const Value: word);
begin
  SetWord(Data, 4, Value);
end;

function TCellRecord.TotalSizeFirst: integer;
begin
  Result:=TotalSize;
end;

function TCellRecord.TotalSizeLast: integer;
begin
  Result:=TotalSize;
end;

function TCellRecord.TotalSizeMid: integer;
begin
  Result:=TotalSize;
end;

{ TWindow1Record }

function TWindow1Record.GetActiveSheet: integer;
begin
  Result:= GetWord(Data, 10);
end;

function TWindow1Record.GetFirstSheetVisible: integer;
begin
  Result:= GetWord(Data, 12);
end;

procedure TWindow1Record.SetActiveSheet(const Value: integer);
begin
  SetWord(Data, 10, Value);
  SetWord(Data, 12, 0);
  SetWord(Data, 14, 1);
end;

procedure TWindow1Record.SetFirstSheetVisible(const Value: integer);
begin
  SetWord(Data, 12, Value);
end;

procedure TWindow1Record.SaveToStream(const Workbook: TOle2File);
begin
  MakeMoreThan1(4);
  MakeMoreThan1(6);
  inherited;
end;

procedure TWindow1Record.MakeMoreThan1(const p: integer);
var
  i: integer;
begin
  i := GetWord(Data, p);
  if (i < 1) then SetWord(Data, p, 1);
  
end;

{ TWindow2Record }


function TWindow2Record.DoCopyTo: TBaseRecord;
begin
  Result:= inherited DoCopyTo;
  (Result as TWindow2Record).Selected:=False;
end;

function TWindow2Record.GetSelected: boolean;
begin
  Result:=GetWord(Data, 0) and (1 shl 9) = (1 shl 9);
end;

function TWindow2Record.GetSheetZoom: integer;
begin
  Result:=GetWord(Data, 12);
end;

function TWindow2Record.GetShowGridLines: boolean;
begin
  Result:=GetWord(Data, 0) and $2 = $2;
end;

function TWindow2Record.GetShowGridHeaders: boolean;
begin
  Result:=GetWord(Data, 0) and $4 = $4;
end;

procedure TWindow2Record.SetSelected(const Value: boolean);
begin
  if Value then SetWord(Data, 0, GetWord(Data, 0) or (3 shl 9)) //Selected=true, showing on window=true
  else SetWord(Data, 0, GetWord(Data, 0) and not (3 shl 9)); //Selected=false, showing on window=false
end;

procedure TWindow2Record.SetSheetZoom(const Value: integer);
begin
  if Value<10 then SetWord(Data, 12, 10) else
    if Value>400 then SetWord(Data, 12, 400)else
    SetWord(Data, 12, Value);
end;

procedure TWindow2Record.SetShowGridLines(const Value: boolean);
begin
  if Value then SetWord(Data, 0, GetWord(Data, 0) or $2) //GridLines=true
  else SetWord(Data, 0, GetWord(Data, 0) and not $2); //GridLines=false
end;

procedure TWindow2Record.SetShowGridHeaders(const Value: boolean);
begin
  if Value then SetWord(Data, 0, GetWord(Data, 0) or $4) //GridHeaders=true
  else SetWord(Data, 0, GetWord(Data, 0) and not $4); //GridHeaders=false
end;

function TWindow2Record.GetIsFrozen(): Boolean;
begin
  Result := (GetWord(Data, 0) and 8) <> 0;
end;

procedure TWindow2Record.SetIsFrozen(const value: Boolean);
begin
  if value then
    SetWord(Data, 0, GetWord(Data, 0) or 8) else
    SetWord(Data, 0, GetWord(Data, 0) and not 8);

end;

function TWindow2Record.GetIsFrozenButNoSplit(): Boolean;
begin
  Result := (GetWord(Data, 0) and 256) <> 0;
end;

procedure TWindow2Record.SetIsFrozenButNoSplit(const value: Boolean);
begin
  if value then
    SetWord(Data, 0, GetWord(Data, 0) or 256) else
    SetWord(Data, 0, GetWord(Data, 0) and not 256);

end;

{ TDefColWidthRecord }

function TDefColWidthRecord.Width: Word;
begin
  Result:= GetWord(Data, 0);
end;

procedure TDefColWidthRecord.SaveToStream(const Workbook: TOle2File);
var
  i: integer;
begin
  i:=GetWord(data, 0);
  if i > 255 then SetWord(Data, 0, 255);
  inherited;
end;

{ TDefRowHeightRecord }

function TDefRowHeightRecord.Height: Word;
begin
  Result:= GetWord(Data, 2);
end;

{ TSubListRecord }

constructor TSubListRecord.CreateAndAssign(const aSubList: TObjectList);
begin
  inherited Create(0,nil,0);
  FSubList:=aSubList;
end;

function TSubListRecord.DoCopyTo: TBaseRecord;
begin
  Assert(true, 'Sublist record can''t be copied'); //To copy, it should change the reference to FList
  Result:=inherited DoCopyTo;
end;

procedure TSubListRecord.SaveToStream(const Workbook: TOle2File);
begin
  (FSubList as TBaseRecordList).SaveToStream(Workbook);
end;

function TSubListRecord.TotalSize: integer;
begin
  Result:=0;
end;

{ TDimensionsRecord }

function TDimensionsRecord.Dim: PDimensionsRec;
begin
  Result:=PDimensionsRec(Data);
end;

{ TPageHeaderFooterRecord }

function TPageHeaderFooterRecord.GetText: UTF16String;
var
  Xs: TExcelString;
  MySelf: TBaseRecord;
  Ofs: integer;
begin
  if Data=nil then
  begin
    Result:='';
    exit;
  end;
  MySelf:=Self;
  Ofs:= 0;
  Xs:=TExcelString.Create(2, MySelf, Ofs );
  try
    Result:=Xs.Value;
  finally
    FreeAndNil(Xs);
  end; //finally
end;

procedure TPageHeaderFooterRecord.SetText(const Value: UTF16String);
  //Important: This method changes the size of the record without notifying it's parent list
  //It's necessary to adapt the Totalsize in the parent list.
var
  Xs: TExcelString;
  NewDataSize: integer;
begin
  Xs:=TExcelString.Create(2, Value);
  try
    NewDataSize:=Xs.TotalSize;
    ReallocMem(Data, NewDataSize);
    DataSize:=NewDataSize;
    Xs.CopyToPtr( Data, 0 );
  finally
    FreeAndNil(Xs);
  end;  //finally
end;

{ TPrintGridLinesRecord }

function TPrintGridLinesRecord.GetValue: boolean;
begin
  Result:=GetWord(Data,0)=1;
end;

procedure TPrintGridLinesRecord.SetValue(const Value: boolean);
begin
  if Value then SetWord(Data,0,1) else SetWord(Data,0,0)
end;

{ TMarginRecord }

function TMarginRecord.GetValue: double;
begin
  move(Data[0], Result, SizeOf(Result));
end;

procedure TMarginRecord.SetValue(const Value: double);
begin
  Assert(SizeOf(Value)=DataSize,'Error in Margin Record');
  move(Value,Data[0],sizeof(Value));
end;

{ TSetupRecord }

function TSetupRecord.GetFitHeight: word;
begin
  Result:=PSetupRec(Data).FitHeight;
end;

function TSetupRecord.GetFitWidth: word;
begin
  Result:=PSetupRec(Data).FitWidth;
end;

function TSetupRecord.GetFooterMargin: extended;
begin
  Result:=PSetupRec(Data).FooterMargin;
end;

function TSetupRecord.GetHeaderMargin: extended;
begin
  Result:=PSetupRec(Data).HeaderMargin;
end;

function TSetupRecord.GetPrintCopies: integer;
begin
  if PrintOptions and $04 = $04 then Result:=1
  else
    Result:=PSetupRec(Data).Copies;
end;

function TSetupRecord.GetPrintOptions: word;
begin
  Result:=PSetupRec(Data).GrBit;
end;

function TSetupRecord.GetPrintPaperSize: TExcelPaperSize;
begin
  if PrintOptions and $04 = $04 then Result:=0
  else
    Result:=PSetupRec(Data).PaperSize;
end;

function TSetupRecord.GetPrintXResolution: integer;
begin
  if PrintOptions and $04 = $04 then Result:=100
  else
    Result:=PSetupRec(Data).Resolution;
end;

function TSetupRecord.GetPrintYResolution: integer;
begin
  if PrintOptions and $04 = $04 then Result:=100
  else
    Result:=PSetupRec(Data).VResolution;
end;

function TSetupRecord.GetScale: word;
begin
  if (PSetupRec(Data).GrBit and $4)=$4 then Result:=100 else
  Result:=PSetupRec(Data).Scale;
end;

function TSetupRecord.GetValue: TSetupRec;
begin
  move(Data[0], Result, SizeOf(Result));
end;

procedure TSetupRecord.SetFitHeight(const Value: word);
begin
  PSetupRec(Data).FitHeight:=Value;
end;

procedure TSetupRecord.SetFitWidth(const Value: word);
begin
  PSetupRec(Data).FitWidth:=Value;
end;

procedure TSetupRecord.SetFooterMargin(const Value: extended);
begin
  PSetupRec(Data).FooterMargin:=Value;
end;

procedure TSetupRecord.SetHeaderMargin(const Value: extended);
begin
  PSetupRec(Data).HeaderMargin:=Value;
end;

procedure TSetupRecord.SetPrintCopies(const Value: integer);

begin
  PrintOptions:= PrintOptions and not Word($04);
  PSetupRec(Data).Copies:=Value;
end;

procedure TSetupRecord.SetPrintOptions(const Value: word);
begin
  PSetupRec(Data).GrBit:=Value and $FF;
end;

procedure TSetupRecord.SetPrintPaperSize(const Value: TExcelPaperSize);
begin
  PrintOptions:= PrintOptions and not Word($04);
  PSetupRec(Data).PaperSize:=Value;
end;

procedure TSetupRecord.SetPrintXResolution(const Value: integer);
begin
  PrintOptions:= PrintOptions and not Word($04);
  PSetupRec(Data).Resolution:=Value;
end;

procedure TSetupRecord.SetPrintYResolution(const Value: integer);
begin
  PrintOptions:= PrintOptions and not Word($04);
  PSetupRec(Data).VResolution:=Value;
end;

procedure TSetupRecord.SetScale(const Value: word);
begin
  PSetupRec(Data).GrBit:=PSetupRec(Data).GrBit and not Word($4);
  PSetupRec(Data).Scale:=Value;
end;

procedure TSetupRecord.SetValue(const Value: TSetupRec);
begin
  Assert(SizeOf(Value)=DataSize,'Error in Setup Record');
  move(Value, Data[0], SizeOf(Value));
end;

{ TWsBoolRecord }

function TWsBoolRecord.GetFitToPage: boolean;
begin
  Result:= Data[1] and 1=1;
end;

function TWsBoolRecord.GetOutlineSummaryColsRightOfDetail: boolean;
begin
  Result:= Data[0] and $80 <> 0;
end;

function TWsBoolRecord.GetOutlineSummaryRowsBelowDetail: boolean;
begin
  Result:= Data[0] and $40 <> 0;
end;

function TWsBoolRecord.GetOutlineAutomaticStyles: boolean;
begin
  Result:= Data[0] and $20 <> 0;
end;

function TWsBoolRecord.GetValue: word;
begin
  Result:=GetWord(Data,0);
end;

procedure TWsBoolRecord.SetFitToPage(const Value: boolean);
begin
  if Value then Data[1]:=Data[1] or 1 else Data[1]:=Data[1] and ($FF-1);
end;

procedure TWsBoolRecord.SetOutlineRightOfDetail(const Value: boolean);
begin
  if Value then Data[0]:=Data[0] or $80 else Data[0]:=Data[0] and not $80;
end;

procedure TWsBoolRecord.SetOutlineSummaryRowsBelowDetail(const Value: boolean);
begin
  if Value then Data[0]:=Data[0] or $40 else Data[0]:=Data[0] and not $40;
end;

procedure TWsBoolRecord.SetOutlineAutomaticStyles(const Value: boolean);
begin
  if Value then Data[0]:=Data[0] or $20 else Data[0]:=Data[0] and not $20;
end;

procedure TWsBoolRecord.SetValue(const Value: word);
begin
  SetWord(Data, 0, Value);
end;

{ TSCLRecord }

constructor TSCLRecord.CreateFromData(const aZoom: integer);
var
  aData:pointer;
begin
  GetMem(aData, 4);
  Create(xlr_SCL, aData, 4);
  SetZoom(aZoom);
end;

function TSCLRecord.GetZoom: integer;
begin
  if GetWord(Data,2)= 0 then Result:=100 else
    Result:=Round(100*GetWord(Data,0)/GetWord(Data,2));
end;

procedure TSCLRecord.SetZoom(const Value: integer);
var
  v: integer;
begin
  if Value<10 then v:=10 else if Value>400 then v:=400 else v:=Value;
  SetWord(Data,0,v);
  SetWord(Data,2,100);
end;

{ TStandardWidthRecord }

function TStandardWidthRecord.Width: Word;
begin
  Result:= GetWord(Data, 0);
end;

{ TRefModeRecord }

function TRefModeRecord.GetIsR1C1: boolean;
begin
  Result:=GetWord(Data,0)<>1;
end;

procedure TRefModeRecord.SetIsR1C1(const Value: boolean);
begin
  if Value then SetWord(Data, 0, 0) else SetWord(Data, 0, 1);
end;

{ T1904Record }

function T1904Record.GetIs1904: boolean;
begin
  Result:=GetWord(Data,0)=1;
end;

procedure T1904Record.SetIs1904(const Value: boolean);
begin
  if Value then SetWord(Data, 0, 1) else SetWord(Data, 0, 0);
end;

{ TPrecisionRecord }

function TPrecisionRecord.GetPrecisionAsDisplayed: boolean;
begin
  Result:=GetWord(Data,0)=0;
end;

procedure TPrecisionRecord.SetPrecisionAsDisplayed(const Value: boolean);
begin
  if Value then SetWord(Data, 0, 0) else SetWord(Data, 0, 1);
end;

{ TBookBoolRecord }

function TBookBoolRecord.GetSaveExternalLinkValues: boolean;
begin
  Result:=GetWord(Data,0)<>1;
end;

procedure TBookBoolRecord.SetSaveExternalLinkValues(const Value: boolean);
begin
  if Value then SetWord(Data, 0, 0) else SetWord(Data, 0, 1);
end;

{ TPlsRecord }

constructor TPlsRecord.CreateFromData(
  aPrinterData: TPrinterDriverSettings);
var
  aData:pointer;
begin
  GetMem(aData, SizeOf(aPrinterData.OperatingEnviroment)+Length(aPrinterData.Data));
  Create(xlr_PLS, aData, SizeOf(aPrinterData.OperatingEnviroment)+Length(aPrinterData.Data));
  SetPrinterDriverSettings(aPrinterData);
end;

function TPlsRecord.GetPrinterDriverSettings: TPrinterDriverSettings;
begin
  Result.OperatingEnviroment:= GetWord(Data,0);
  SetLength(Result.Data, DataSize-2);
  move (Data[2], Result.Data[0], Length(Result.Data));
end;

procedure TPlsRecord.SetPrinterDriverSettings(
  const Value: TPrinterDriverSettings);
var
  NewDataSize: integer;
begin
  NewDataSize:=SizeOf(Value.OperatingEnviroment)+Length(Value.Data);
  if DataSize <> NewDataSize then
  begin
    FreeAndNil(Data);
    GetMem(Data, NewDataSize);
  end;

  SetWord(Data,0, Value.OperatingEnviroment);
  move(Value.Data[0], Data[2], DataSize-2);
end;

{ TAutoFilterInfoRecord }

function TAutoFilterInfoRecord.Get_DropDownCount(): Int32;
begin
  Result := GetWord(Data, 0);
end;

procedure TAutoFilterInfoRecord.Set_DropDownCount(const value: Int32);
begin
  SetWord(Data, 0, value);
end;

{ TPrintCenteredRecord }

function TPrintCenteredRecord.GetCentered: boolean;
begin
  Result := GetWord(Data, 0) = 1;
end;

procedure TPrintCenteredRecord.SetCentered(const Value: boolean);
begin
  if Value then SetWord(Data, 0, 1) else SetWord(Data, 0, 0);
  
end;

end.
