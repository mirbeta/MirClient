unit tmsUExcelRecords;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses SysUtils, Contnrs, Classes, tmsXlsMessages, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists,
     tmsUXlsOtherRecords, tmsUXlsSST, tmsUXlsReferences, tmsUSheetNameList, tmsUXlsFormula,
     tmsUXlsEscher, tmsUXlsClientData, tmsUXlsSheet, tmsUXlsWorkbookGlobals, tmsUXlsBaseList, tmsUFlxMessages,
     tmsUOle2Impl;

type

  TSheetList = class(TBaseList) //records are TSheet
    {$INCLUDE TSheetListHdr.inc}
    procedure SaveToStream(const DataStream: TOle2File);
    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; SheetInfo: TSheetInfo);
    procedure DeleteSheets(const SheetIndex: integer; const SheetCount: integer);
  end;

  TWorkbook = class
  private
    FGlobals: TWorkbookGlobals;
    FSheets: TSheetList;

    procedure FixBoundSheetsOffset(const NeedsRecalc: boolean);
    procedure FixRows;
    function GetWorkSheets(index: integer): TWorksheet;
    function GetActiveSheet: integer;
    procedure SetActiveSheet(const Value: integer);
    procedure FixRangeBoundSheetsOffset(const SheetIndex: integer; const CellRange: TXlsCellRange; const NeedsRecalc: boolean);
    procedure FixCodeNames;
    function GetIsXltTemplate: boolean;
    procedure SetIsXltTemplate(const Value: boolean);
  public
    property Globals: TWorkbookGlobals read FGlobals write FGlobals;
    property Sheets: TSheetList read FSheets write FSheets;

    function IsWorksheet(const Index: integer): boolean;
    property WorkSheets[index:integer]: TWorksheet read GetWorkSheets;

    property IsXltTemplate: boolean read GetIsXltTemplate write SetIsXltTemplate;

    property ActiveSheet: integer read GetActiveSheet write SetActiveSheet;

    constructor Create;
    destructor Destroy;override;

    procedure LoadFromStream( const DataStream: TOle2File);
    procedure SaveToStream(const DataStream: TOle2File; const NeedsRecalc: boolean);
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer; const CellRange: TXlsCellRange; const NeedsRecalc: boolean);

    //Manipulating Methods
    procedure InsertAndCopyRowsAndCols(const SheetNo, FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; const OnlyFormulas: boolean);
    procedure DeleteRowsAndCols(const SheetNo: integer; const aRow, aRowCount, aCol, aColCount: word);
    procedure InsertSheets(const CopyFrom, InsertBefore: integer; SheetCount: integer);
    procedure DeleteSheets(const SheetPos, SheetCount: integer);

    procedure InsertHPageBreak(const SheetNo: integer; const aRow: word);
    procedure InsertVPageBreak(const SheetNo: integer; const aCol: word);

    procedure RestoreObjectCoords(dSheet: integer);
  end;

implementation

{ TSheetList }
{$INCLUDE TSheetListImp.inc}

procedure TSheetList.InsertAndCopyRowsAndCols(const FirstRow, LastRow,
  DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
var
  i:integer;
begin
  Items[SheetInfo.InsSheet].InsertAndCopyRowsAndCols(FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount, SheetInfo, OnlyFormulas);
  for i:=0 to Count -1 do if i<>SheetInfo.InsSheet then
  begin
    SheetInfo.FormulaSheet:=i;
    Items[i].ArrangeInsertRowsAndCols(DestRow, (LastRow-FirstRow+1)*aRowCount, DestCol, (LastCol-FirstCol+1)*aColCount, SheetInfo);
  end;
end;

procedure TSheetList.DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word;
  SheetInfo: TSheetInfo);
var
  i:integer;
begin
  Items[SheetInfo.InsSheet].DeleteRowsAndCols(aRow, aRowCount, aCol, aColCount, SheetInfo);
  for i:=0 to Count -1 do if i<>SheetInfo.InsSheet then
  begin
    SheetInfo.FormulaSheet:=i;
    Items[i].ArrangeInsertRowsAndCols(aRow, -aRowCount, aCol, -aColCount, SheetInfo);
  end;
end;

procedure TSheetList.DeleteSheets(const SheetIndex, SheetCount: integer);
var
  i: integer;
begin
  for i:=0 to SheetCount-1 do
  begin
    if (SheetIndex>= Count) then exit;
    if (Items[SheetIndex] is TWorkSheet) then (Items[SheetIndex] as TWorkSheet).Clear;  //Images are not cleared when destroyng. thats why we need to clear.
    Delete(SheetIndex);
  end;
end;

procedure TSheetList.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveToStream(DataStream);
end;

{ TWorkbook }

constructor TWorkbook.Create;
begin
  inherited;
  FGlobals:= TWorkbookGlobals.Create;
  FSheets := TSheetList.Create;
end;

procedure TWorkbook.DeleteRowsAndCols(const SheetNo: integer; const aRow, aRowCount, aCol, aColCount: word);
var
  SheetInfo: TSheetInfo;
begin
  if(SheetNo>= Sheets.Count) then raise Exception.CreateFmt(ErrInvalidSheetNo, [SheetNo, 0, Sheets.Count-1]);

  SheetInfo.InsSheet:=SheetNo;
  SheetInfo.FormulaSheet:=SheetNo;
  SheetInfo.GetSheet:=Globals.References.GetSheet;
  SheetInfo.SetSheet:=Globals.References.SetSheet;
  SheetInfo.Names:=nil;

  FSheets.DeleteRowsAndCols(aRow, aRowCount, aCol, aColCount, SheetInfo);
  Globals.DeleteRowsAndCols(aRow, aRowCount, aCol, aColCount, SheetInfo);
end;

  //PENDING: DVal (data validation)
  //PENDING: HLINKS // SCREENTIP
  //PENDING: LabelRanges
  //MADE: TABLE
  //PENDING: Index /dbcell
  //PENDING: property LoadValuesOnly
  //PENDING: String records    Ver como arreglamos esto y dbcells
  //MADE: Dimensions
  //PENDING: eliminar mensaje excel grabado con version anterior
destructor TWorkbook.Destroy;
begin
  FreeAndNil(FSheets);
  //Order is important. Globals should be freed after sheets
  FreeAndNil(FGlobals);
  inherited;
end;

procedure TWorkbook.FixRangeBoundSheetsOffset(const SheetIndex: integer; const CellRange: TXlsCellRange; const NeedsRecalc: boolean);
var
  TotalOfs: int64;
begin
  Globals.SST.FixRefs;
  TotalOfs:=Globals.TotalRangeSize(SheetIndex, CellRange);  //Includes the EOF on workbook Globals
  if Globals.SheetCount<> Sheets.Count then raise Exception.Create(ErrExcelInvalid);

  Sheets[SheetIndex].FixTotalSize(NeedsRecalc); 
  Globals.SheetSetOffset(SheetIndex, TotalOfs);
end;

procedure TWorkbook.FixBoundSheetsOffset(const NeedsRecalc: boolean);
var
  i: integer;
  TotalOfs: int64;
begin
  Globals.SST.FixRefs;
  TotalOfs:=Globals.TotalSize;  //Includes the EOF on workbook Globals
  if Globals.SheetCount<> Sheets.Count then raise Exception.Create(ErrExcelInvalid);

  for i:=0 to Globals.SheetCount-1 do
  begin
    Globals.SheetSetOffset(i,TotalOfs);
    TotalOfs:=TotalOfs+(Sheets[i].FixTotalSize(NeedsRecalc));
  end;
end;

procedure TWorkbook.FixRows;
var
  i: integer;
begin
  for i:=0 to Globals.SheetCount-1 do
  begin
    Sheets[i].FixRows;
  end;
end;

function TWorkbook.GetActiveSheet: integer;
begin
  Result:= Globals.ActiveSheet;
end;


function TWorkbook.GetIsXltTemplate: boolean;
begin
  Result := FGlobals.IsXltTemplate;
end;

function TWorkbook.GetWorkSheets(index: integer): TWorksheet;
begin
  Result:= Sheets[index] as TWorkSheet;
end;

procedure TWorkbook.InsertAndCopyRowsAndCols(const SheetNo, FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; const OnlyFormulas: boolean);
var
  SheetInfo: TSheetInfo;
begin
  //Some error handling
  if (FirstRow>LastRow) or (FirstRow<0) or (LastRow> Max_Rows) or
  ((FirstRow<DestRow) and (DestRow<=LastRow)) or (DestRow+(LastRow-FirstRow+1)*aRowCount>Max_Rows)
  or (DestRow<0)
  then raise Exception.Create(ErrBadCopyRows);   

  if (FirstCol>LastCol) or (FirstCol<0) or (LastCol> Max_Columns) or
  ((FirstCol<DestCol) and (DestCol<=LastCol)) or (DestCol+(LastCol-FirstCol+1)*aColCount>Max_Columns)
  or (DestCol<0)
  then raise Exception.Create(ErrBadCopyRows);

  if (SheetNo<0) or (SheetNo>= Sheets.Count) then raise Exception.CreateFmt(ErrInvalidSheetNo, [SheetNo, 0, Sheets.Count-1]);

  SheetInfo.InsSheet:=SheetNo;
  SheetInfo.FormulaSheet:=SheetNo;
  SheetInfo.GetSheet:=Globals.References.GetSheet;
  SheetInfo.SetSheet:=Globals.References.SetSheet;
  SheetInfo.Names:=nil;

  FSheets.InsertAndCopyRowsAndCols(FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount, SheetInfo, OnlyFormulas);
  Globals.InsertAndCopyRowsAndCols(FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount, SheetInfo);
end;

procedure TWorkbook.DeleteSheets(const SheetPos, SheetCount: integer);
begin
   if  (SheetPos> Sheets.Count) then raise Exception.CreateFmt(ErrInvalidSheetNo, [SheetPos, 0, Sheets.Count]);
   Globals.DeleteSheets(SheetPos, SheetCount);
   FSheets.DeleteSheets(SheetPos, SheetCount);

end;

procedure TWorkbook.InsertSheets(const CopyFrom, InsertBefore: integer; SheetCount: integer);
var
  i:integer;
  aSheet: TSheet;
  OptionFlags: Word;
  SheetInfo: TSheetInfo;
begin
  if  (CopyFrom>= Sheets.Count) then raise Exception.CreateFmt(ErrInvalidSheetNo, [CopyFrom, -1, Sheets.Count-1]);
  if  (InsertBefore> Sheets.Count) then raise Exception.CreateFmt(ErrInvalidSheetNo, [InsertBefore, 0, Sheets.Count]);

  if CopyFrom>=0 then
  begin
    aSheet:= Sheets[CopyFrom];
    OptionFlags := Globals.SheetOptionFlags[CopyFrom];
  end else
  begin
    aSheet:=nil;
    OptionFlags := 0;
  end;

  Globals.InsertSheets( CopyFrom, InsertBefore, OptionFlags, BaseSheetName, SheetCount);

  SheetInfo.GetSheet:=Globals.References.GetSheet;
  SheetInfo.SetSheet:=Globals.References.SetSheet;
  SheetInfo.Names:= Globals.Names;
  for i:=0 to SheetCount-1 do
  begin
    SheetInfo.InsSheet:=InsertBefore+SheetCount-1-i;
    SheetInfo.FormulaSheet:=CopyFrom;

    if aSheet=nil then
      Sheets.Insert( InsertBefore , TWorkSheet.CreateFromData(Globals,Globals.SST))
    else
    begin
      Sheets.Insert( InsertBefore , aSheet.CopyTo);
      Sheets[InsertBefore].ArrangeCopySheet(SheetInfo);
    end;
  end;
end;

procedure TWorkbook.InsertHPageBreak(const SheetNo: integer; const aRow: word);
begin
  Sheets[SheetNo].InsertHPageBreak(aRow);
end;

procedure TWorkbook.InsertVPageBreak(const SheetNo: integer; const aCol: word);
begin
  Sheets[SheetNo].InsertVPageBreak(aCol);
end;

function TWorkbook.IsWorksheet(const Index: integer): boolean;
begin
  Result:= Sheets[index] is TWorkSheet;
end;


procedure TWorkbook.SetIsXltTemplate(const Value: boolean);
begin
  FGlobals.IsXltTemplate := Value;
end;

procedure TWorkbook.LoadFromStream(const DataStream: TOle2File);
var
  RecordHeader: TRecordHeader;
  R: TBaseRecord;
  RecordId: integer;
begin
  Sheets.Clear;
  Globals.Clear;

  DataStream.ReadMem(RecordHeader, sizeof(RecordHeader)); //initialize the first time.

  while (not DataStream.NextEof(3)) and (RecordHeader.id<>0) do
  begin
    RecordId := RecordHeader.Id;
    R:=LoadRecords(DataStream, RecordHeader);
    try
      if (RecordId = xlr_BOF) then
      case (R as TBOFRecord).BOFType of
        xlb_Globals   : Globals.LoadFromStream(DataStream, RecordHeader, R as TBOFRecord, Globals.SST);
        xlb_Worksheet : FSheets[FSheets.Add(TWorkSheet.Create(Globals))].LoadFromStream(DataStream, RecordHeader, R as TBOFRecord, Globals.SST) ;
        xlb_Chart     : FSheets[FSheets.Add(TFlxChart.Create(Globals))].LoadFromStream(DataStream, RecordHeader, R as TBOFRecord, Globals.SST) ;
        else FSheets[FSheets.Add(TFlxUnsupportedSheet.Create(Globals))].LoadFromStream(DataStream, RecordHeader, R as TBOFRecord, Globals.SST) ;
      end //case
      else
        if (RecordId = xlr_EOF) then FreeAndNil(R) //There can be 2 eof at the end of the file
        else raise Exception.Create(ErrExcelInvalid);

      if (Globals.SheetCount > 0) and (Globals.SheetCount <= FSheets.Count) then break; //There shouldn't be any garbage here, but some weird non-created-by-excel files might have it, and Excel will load them fine.
    except
      FreeAndNil(R);
      raise;
    end; //except
  end; //while

  // References from LABELSST to SST have been loaded, we can sort
  Globals.SST.Sort;
  //now we can safely sort, all BSEs are pointers, no integers
  if Globals.DrawingGroup.RecordCache.BStore <> nil then Globals.DrawingGroup.RecordCache.BStore.ContainedRecords.Sort;

end;

procedure TWorkbook.FixCodeNames;
var
  Names: TStringList;
  i,k: integer;
  s, SheetName: UTF16String;
  Index: integer;
begin
  if not FGlobals.HasMacro then exit;
  Names:=TStringList.Create;
  try
    Names.Sorted:=true;
    Names.Duplicates:= dupIgnore;
    Names.Add(FGlobals.CodeName);
    for i:=0 to FSheets.Count-1 do
    begin
      s:=FSheets[i].CodeName;
      if s<>'' then Names.Add(WideUpperCase98(s));
    end;
    for i:=0 to FSheets.Count-1 do
    begin
      if FSheets[i].CodeName='' then
      begin
        SheetName:=FGlobals.SheetName[i];
        k:=Length(SheetName);
        while (k>0) and (SheetName[k]<#255) and (AnsiChar(SheetName[k])in ['0'..'9']) do dec(k);
        s:=copy(SheetName,k+1,length(s));
        SheetName:=Copy(SheetName,1,k);
        if s='' then k:=0 else k:=StrToInt(s);
        while Names.Find(WideUpperCase98(SheetName)+s, Index) do
        begin
          inc(k);
          s:=IntToStr(k);
        end;

        FSheets[i].CodeName:=SheetName+s;
        Names.Add(WideUpperCase98(SheetName)+s);
      end;
    end;
  finally
    FreeAndNil(Names);
  end; //finally
end;

procedure TWorkbook.SaveToStream(const DataStream: TOle2File; const NeedsRecalc: boolean);
var
  i: integer;
  FirstSheetVisible: integer;
begin
  FixCodeNames; //before fixing offsets.
  FixRows;
  FixBoundSheetsOffset(NeedsRecalc);

  FirstSheetVisible:=-1;
  for i:=FSheets.Count-1 downto 0 do
  begin
    if (FGlobals.SheetVisible[i]=sv_Visible) then FirstSheetVisible:=i
    else
      if FSheets[i].Selected then raise Exception.Create(ErrHiddenSheetSelected);
  end;

  if FirstSheetVisible=-1 then raise Exception.Create(ErrNoSheetVisible);
  FGlobals.SetFirstSheetVisible(FirstSheetVisible);

  FGlobals.SaveToStream(DataStream);
  FSheets.SaveToStream(DataStream);
end;

procedure TWorkbook.SetActiveSheet(const Value: integer);
var
  i: integer;
begin
//  if (Globals.ActiveSheet>=0) and (Globals.ActiveSheet< Sheets.Count) then  //Active sheet might become invalid if we delete sheets.
//    Sheets[Globals.ActiveSheet].Selected:=false;
  //We have to loop on ALL sheets, because copying might copy selected sheets.
  for i:=0 to Sheets.Count-1 do Sheets[i].Selected:=false;

  Globals.ActiveSheet:=Value;
  Sheets[Value].Selected:=true;
end;

procedure TWorkbook.SaveRangeToStream(const DataStream: TOle2File;
  const SheetIndex: integer; const CellRange: TXlsCellRange; const NeedsRecalc: boolean);
begin
  FixCodeNames;//before fixing offsets.
  FixRows;
  FixRangeBoundSheetsOffset(SheetIndex, CellRange, NeedsRecalc);
  FGlobals.SaveRangeToStream(DataStream, SheetIndex, CellRange);
  //we dont have to check SheetIndex is ok. this was done on FGlobals.SaveRangetoStream
  FSheets[SheetIndex].SaveRangeToStream(DataStream, SheetIndex, CellRange);
end;

procedure TWorkbook.RestoreObjectCoords(dSheet: integer);
begin
	FSheets[dSheet].RestoreObjectCoords;
end;
end.

