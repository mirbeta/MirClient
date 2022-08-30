unit tmsUXlsRowColEntries;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords,
     tmsXlsMessages, tmsUXlsRangeRecords, tmsUXlsBaseList, tmsUXlsCellRecords, tmsUXlsFormula,
     {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF}
     {$IFDEF FLX_NEEDSTYPES} Types,{$ENDIF} //Delphi 6 or above
     {$IFDEF DELPHIXE3UP} System.UITypes, {$ENDIF}
      {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
    tmsUXlsSST, tmsUFlxMessages, tmsUXlsColInfo, tmsUXlsReferences, tmsUXlsWorkbookGlobals, tmsUXlsTokenArray, tmsXlsFormulaMessages,tmsUFlxNumberFormat,

  {$IFDEF FIREMONKEY}
   FMX.Types,
  {$ELSE}
  {$IFDEF FLX_VCL}
    Graphics,
  {$ENDIF}
  {$IFDEF FLX_CLX}
    QGraphics,
  {$ENDIF}
  {$ENDIF}

   tmsUFlxFormats, tmsUOle2Impl;

type
  TListClass= class of TBaseRowColRecordList;

  TFlxFontArray = array of TFlxFont;
  TIntegerArray = array of integer;

  TColWidthCalc = class
  private
{$IFNDEF FIREMONKEY}
    XFFonts: TFlxFontArray;
    Wg: TWorkbookGlobals;
    bmp: TBitmap;
    Canvas: TCanvas;

    procedure InitXF();
{$ENDIF}
  public
    constructor Create(const aWg: TWorkbookGlobals);
    function CalcCellWidth(const Row: integer; const Col: integer; const val: TRichString; const XF: integer; const Workbook: pointer; const RowMultDisplay: Extended; const ColMultDisplay: Extended): integer;
    destructor Destroy;override;
  end;


  TBaseRowColList = class(TBaseList) //records are TBaseRowColRecordList
    {$INCLUDE TBaseRowColListHdr.inc}
  protected
    ListClass: TListClass;
  public
    procedure AddRecord(const aRecord: TBaseRowColRecord; const aRow: integer);

    procedure CopyFrom(const aList: TBaseRowColList);

    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure InsertAndCopyRows(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
    procedure InsertAndCopyCols(const FirstCol, LastCol, DestCol, aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean); virtual;
    procedure DeleteRows(const aRow, aCount: word; const SheetInfo: TSheetInfo);
    procedure DeleteCols(const aCol, aCount: word; const SheetInfo: TSheetInfo);
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo); virtual;

    constructor Create(const aListClass: TListClass);
  end;

  TCellList = class (TBaseRowColList)//records are TCellRecordList
  private
    FGlobals: TWorkbookGlobals;
    FRowRecordList: TRowRecordList;
    FColInfoList: TColInfoList;

    function GetValue(Row, Col: integer): TXlsCellValue;
    procedure FixFormulaTokens(const Formula: TFormulaRecord; const ShrFmlas: TShrFmlaRecordList);
    function GetFormula(Row, Col: integer): UTF16String;
    procedure SetFormula(Row, Col: integer; const Value: UTF16String);
    procedure AutofitColumn(const Workbook: pointer; const Column: integer;
      const ColCalc: TColWidthCalc; const RowMultDisplay,
      ColMultDisplay: Extended; const IgnoreStrings: Boolean;
      const Adjustment: Extended);

    {$INCLUDE TCellListHdr.inc}
  public
    constructor Create(const aGlobals: TWorkbookGlobals; const aRowRecordList: TRowRecordList; const aColInfoList: TColInfoList);
    property Value[Row,Col:integer]:TXlsCellValue  read GetValue;
    procedure SetValueX2(const Row, Col: integer; const Value: TXlsCellValue; const RTFRuns: TRTFRunList; const Options1904: boolean);
    procedure GetValueX2(const Row, Col: integer; out V: TXlsCellValue; out RTFRuns: TRTFRunList);
    procedure SetFormat(const Row, Col: integer; const XF: integer);
    property Formula[Row,Col: integer]: UTF16String read GetFormula write SetFormula;
    procedure AssignFormulaX(const Row, Col: integer; const Formula: UTF16String; const Value: variant; const Options1904: boolean);
    function ArrayFormula(const Row, Col: integer): PArrayOfByte;
    function TableFormula(const Row, Col: integer): PArrayOfByte;
    procedure FixFormulas(const ShrFmlas: TShrFmlaRecordList);

    function GetSheetName(const SheetNumber: integer): UTF16String;
    function AddExternSheet(const FirstSheet, LastSheet: Integer): Integer;
    function FindSheet(SheetName: UTF16String; out SheetIndex: Integer): Boolean;

    procedure InsertAndCopyCols(const FirstCol, LastCol, DestCol, aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean); override;
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo); override;

    procedure ArrangeInsertSheet(const SheetInfo: TSheetInfo);
    function GetName(const ExternSheet, NameId: integer): UTF16String;

    procedure RecalcColWidths(const Workbook: pointer; const Col1, Col2: integer; const IgnoreStrings: boolean; const Adjustment: Extended);
    procedure RecalcRowHeights(const Workbook: pointer; const Row1, Row2: integer; const Forced, KeepAutofit: Boolean; const Adjustment: Extended);

    function FixTotalSize(const NeedsRecalc: boolean): int64;
  end;

  TCells = class
  private
    FRowList: TRowRecordList;
    FCellList: TCellList;
    procedure WriteDimensions(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function DimensionsSize: integer;
    procedure CalcUsedRange(var CellRange: TXlsCellRange);
    procedure ArrangeCols;
  public
    constructor Create(const aGlobals: TWorkbookGlobals; const aColInfoList: TColInfoList);
    destructor Destroy; override;

    procedure Clear;
    procedure CopyFrom(const aList: TCells);

    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function FixTotalSize(const NeedsRecalc: boolean): int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure FixRows;

    procedure InsertAndCopyRows(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
    procedure InsertAndCopyCols(const FirstCol, LastCol, DestCol, aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
    procedure DeleteRows(const aRow, aCount: word; const SheetInfo: TSheetInfo);
    procedure DeleteCols(const aCol, aCount: word; const SheetInfo: TSheetInfo);
    procedure ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
    procedure ArrangeInsertSheet(const SheetInfo: TSheetInfo);

    procedure AddRow(const aRecord: TRowRecord);
    procedure AddCell(const aRecord: TCellRecord;  const aRow: integer);
    procedure AddMultipleCells(const aRecord: TMultipleValueRecord);

    property CellList: TCellList read FCellList;
    property RowList: TRowRecordList read FRowList;
  end;


  TRangeList = class(TBaseList) //records are TRangeEntry
    {$INCLUDE TRangeListHdr.inc}
    procedure CopyFrom(const aRangeList: TRangeList);

    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean);
    procedure DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);

  end;

implementation
uses tmsUXlsFormulaParser, tmsUXlsEncodeFormula, tmsUXlsXF, tmsUExcelAdapter
  ,Math;

{$INCLUDE TBaseRowColListImp.inc}
{$INCLUDE TRangeListImp.inc}
{$INCLUDE TCellListImp.inc}

type

  /// <summary>
  /// Class for calculating the automatic row heights.
  /// This is a tricky thing because we are coupling GDI calls with
  /// non-graphic code, but there is no other way to do it.
  /// </summary>
  TRowHeightCalc = class
  private
{$IFNDEF FIREMONKEY}
    XFHeight: TIntegerArray;
    XFFonts: TFlxFontArray;
    Wg: TWorkbookGlobals;
    Canvas: TCanvas;
    bmp: TBitmap;

    procedure InitXF();
{$ENDIF}

  public
    constructor Create(const aWg: TWorkbookGlobals);
    destructor Destroy;override;
    function CalcCellHeight(const Row: integer; const Col: integer; const val: TRichString; const XF: integer; const Workbook: pointer; const RowMultDisplay: Extended; const ColMultDisplay: Extended): integer;
  end;

{ TBaseRowColList }
procedure TBaseRowColList.AddRecord(const aRecord: TBaseRowColRecord; const aRow: integer);
var
  i:integer;
begin
  for i:= Count to aRow do Add(ListClass.Create);
  Items[aRow].Add(aRecord);
end;

procedure TBaseRowColList.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeInsertRowsAndCols(InsRowPos, InsRowCount,InsColPos,InsColCount, SheetInfo);
end;

procedure TBaseRowColList.CopyFrom(const aList: TBaseRowColList);
var
  i: integer;
  Tr: TBaseRowColRecordList;
begin
  for i:=0 to aList.Count - 1 do
  begin
    Tr:= ListClass.Create;
    Tr.CopyFrom(aList[i]);
    Add(Tr);
  end;
end;

constructor TBaseRowColList.Create(const aListClass: TListClass);
begin
  inherited Create(true);
  ListClass:=aListClass;
end;

procedure TBaseRowColList.DeleteRows(const aRow, aCount: word; const SheetInfo: TSheetInfo);
var
  i, Max: integer;
begin
  Max:=aRow+aCount ; if Max>Count then Max:= Count;
  for i:= Max-1 downto aRow do Delete(i);
  //Delete the cells. we have to look at all the formulas, not only those below arow
  ArrangeInsertRowsAndCols(aRow, -aCount, 0, 0, SheetInfo);

end;

procedure TBaseRowColList.DeleteCols(const aCol, aCount: word; const SheetInfo: TSheetInfo);
var
  Index: integer;
  r,c: integer;
begin
  for r:=0 to Count-1 do
    for c:= aCol to ACol+aCount-1 do
      if Items[r].Find(c, Index) then Items[r].Delete(Index);
  //Delete the cells. we have to look at all the formulas, not only those below arow
  ArrangeInsertRowsAndCols(0, 0, aCol, -aCount, SheetInfo);

end;

procedure TBaseRowColList.InsertAndCopyRows(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
var
  i, k, z, a, CopyOffs, MyDestRow: integer;
  aRecordList: TBaseRowColRecordList;
begin
  // Insert the cells. we have to look at all the formulas, not only those below destrow
  ArrangeInsertRowsAndCols(DestRow, aCount*(LastRow-FirstRow+1), 0,0, SheetInfo);

  //Copy the cells
  MyDestRow:=DestRow;
  CopyOffs:=0;
  for k:=1 to aCount do
    for i:=FirstRow to LastRow do
    begin
      aRecordList:= ListClass.Create;
      try
        //Will only copy the cells if copyfrom < recordcount. This allows us to only insert, and not copy.
        if i+CopyOffs<Count then
        begin
          if OnlyFormulas then
          begin
            for a:=0 to Items[i+CopyOffs].Count-1 do
              if (Items[i+CopyOffs][a] is TFormulaRecord) then
                aRecordList.Add(Items[i+CopyOffs][a].CopyTo as TBaseRowColRecord);
          end else aRecordList.CopyFrom(Items[i+CopyOffs]);

          if (aRecordList.Count>0) then aRecordList.ArrangeCopyRowsAndCols(MyDestRow-aRecordList[0].Row,0);
        end;
        for z:= Count to MyDestRow-1 do Add(ListClass.Create);
        Insert(MyDestRow, aRecordList);
        aRecordList:=nil;
      finally
        FreeAndNil(aRecordList);
      end; //finally
      Inc(MyDestRow);
      if FirstRow>=DestRow then Inc(CopyOffs);
    end;

end;

procedure TBaseRowColList.InsertAndCopyCols(const FirstCol, LastCol, DestCol,
  aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
var
  i, k, r, CopyOffs, MyDestCol: integer;
  Index: integer;
  Rec: TBaseRowColRecord;
begin
  // Insert the cells. we have to look at all the formulas, not only those at the left from destcol
  ArrangeInsertRowsAndCols(0,0,DestCol, aCount*(LastCol-FirstCol+1), SheetInfo);

  //Copy the cells
  MyDestCol:=DestCol;
  if (DestCol<=FirstCol) then CopyOffs:=aCount*(LastCol-FirstCol+1) else CopyOffs:=0;

  for k:=1 to aCount do
    for i:=FirstCol to LastCol do
    begin
      for r:=0 to Count-1 do
      begin
        if Items[r].Find(i+CopyOffs, Index)
          and  ( not OnlyFormulas or (Items[r][Index] is TFormulaRecord)) then
          begin
            Rec:=(Items[r][Index].CopyTo as TBaseRowColRecord);
            try
              Rec.ArrangeCopyRowsAndCols(0,MyDestCol-Rec.Column);
            except
              FreeAndNil(Rec);
              raise;
            end; //except
            Items[r].Find(Rec.Column, Index);
            Items[r].Insert(Index, Rec);
          end;

      end;
      Inc(MyDestCol);
    end;
end;

procedure TBaseRowColList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveRangeToStream(DataStream, CellRange);
end;

procedure TBaseRowColList.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveToStream(DataStream);
end;

function TBaseRowColList.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  i: integer;
begin
  Result:=0;
  for i:=CellRange.Top to CellRange.Bottom do Result:=Result+Items[i].TotalRangeSize(CellRange, false);
end;

function TBaseRowColList.TotalSize: int64;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do Result:=Result+Items[i].TotalSize;
end;

{ TCellList }

constructor TCellList.Create(const aGlobals: TWorkbookGlobals; const aRowRecordList: TRowRecordList; const aColInfoList: TColInfoList);
begin
  inherited Create(TCellRecordList);
  FGlobals:= aGlobals;
  FRowRecordList:=aRowRecordList;
  FColInfoList:=aColInfoList;
end;

procedure TCellList.GetValueX2(const Row, Col: integer;
  out V: TXlsCellValue; out RTFRuns: TRTFRunList);
var
  Index: integer;
  Rs: TRichString;
begin
  if (Row<0) or (Row>Max_Rows) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);
  SetLength(RTFRuns,0);
  if Row>=Count then begin; V.Value:=Unassigned; V.XF:=-1; V.IsFormula:=false; exit; end;
  if Items[Row].Find(Col,Index) then
  begin
    V.XF:=Items[Row][Index].XF;
    V.IsFormula:=Items[Row][Index] is TFormulaRecord;
    if Items[Row][Index] is TLabelSSTRecord then
    begin
      Rs:=(Items[Row][Index] as TLabelSSTRecord).AsRichString;
      V.Value:=Rs.Value;
      RTFRuns:= Copy(Rs.RTFRuns);
    end else
    if Items[Row][Index] is TRStringRecord then
    begin
      Rs:=(Items[Row][Index] as TRStringRecord).AsRichString;
      V.Value:=Rs.Value;
      RTFRuns:= Copy(Rs.RTFRuns);
    end else V.Value:=Items[Row][Index].Value;

  end else
  begin
    V.Value:=Unassigned;
    V.XF:=-1;
    V.IsFormula:=false;
  end;
end;

function TCellList.GetValue(Row, Col: integer): TXlsCellValue;
var
  RTFRuns: TRTFRunList;
begin
  GetValueX2(Row, Col, Result, RTFRuns);
end;

procedure TCellList.SetValueX2(const Row, Col: integer; const Value: TXlsCellValue; const RTFRuns: TRTFRunList; const Options1904: boolean);
var
  Index, k: integer;
  XF, DefaultXF: integer;
  Found: boolean;
  Cell: TCellRecord;
  ValueType: integer;
  Rs: TRichString;
  RealValue: variant;
begin
  if (Row<0) or (Row>Max_Rows) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);

  FRowRecordList.AddRow(Row);

  if FRowRecordList[Row].IsFormatted then DefaultXF:=FRowRecordList[Row].XF
  else if FColInfoList.Find(Col, Index) then DefaultXF:=FColInfoList[Index].XF
  else DefaultXF:=15;

  Cell:=nil;
  Found:=(Row<Count) and Items[Row].Find(Col,Index);
  XF:=DefaultXF;
  if Found then XF:=Items[Row][Index].XF;
  if Value.XF>=0 then XF:=Value.XF;


  RealValue:= Value.Value;
  ValueType:= VarType(RealValue);

  {$IFDEF FLX_HASCUSTOMVARIANTS}
  //Check for Custom Variants
  if (ValueType>=$010F) and (ValueType<=$0FFF) then
  begin
    ValueType:=VarDouble; //should be VarType(OleVariant(Value.Value)), but this converts numbers to strings
  end;
  {$ENDIF} //Delphi 6 or above

  if (ValueType = varDate) and (Options1904) then RealValue := double(RealValue) - Date1904Diff;

  case ValueType of
    varEmpty,
    varNull      : if (XF<>DefaultXF) then Cell:= TBlankRecord.CreateFromData(Row,Col,XF);

    varByte,
    varSmallint,
    varInteger,
    varSingle,
    varDouble,
    {$IFDEF FLX_HASCUSTOMVARIANTS}
      varShortInt, VarWord, VarLongWord, varInt64,
    {$ENDIF} //Delphi 6 or above
    varDate,
    varCurrency : if IsRK(RealValue) then Cell:= TRKRecord.CreateFromData(Row,Col,XF)
                                 else Cell:= TNumberRecord.CreateFromData(Row,Col,XF);

    varOleStr,
    varStrArg,
    {$IFDEF DELPHI2008UP}
    varUString,
    {$ENDIF}
    varString   : if (RealValue='') then
                  begin
                    if (XF<>DefaultXF) then Cell:= TBlankRecord.CreateFromData(Row,Col,XF);
                  end
                  else Cell:= TLabelSSTRecord.CreateFromData(Row,Col,XF,FGlobals.SST);

    varBoolean	: Cell:= TBoolErrRecord.CreateFromData(Row,Col,XF);
  end; //case

  if Found then Items[Row].Delete(Index);


  if Found and (Cell=nil) then  //We are deleting a cell
  begin
    if (Row>=Count) or (Items[Row]=nil)or(Items[Row].Count=0)then //Row emptied
      if (not FRowRecordList[Row].IsModified)  then     //Row always exists... it is added at the top
        FRowRecordList[Row]:=nil  //this frees the object
      else
      begin
        FRowRecordList[Row].MinCol:= 0;
        FRowRecordList[Row].MaxCol:= 0;
      end
    else
    begin
      FRowRecordList[Row].MinCol:= Items[Row][0].Column;
      FRowRecordList[Row].MaxCol:= Items[Row][Items[Row].Count-1].Column+1;
    end;
  end;

  //Remove all empty Rows at the end.
  k:=FRowRecordList.Count-1;
  while ((k>Row) or (Cell=nil)) and
        (k>=0) and (not FRowRecordList.HasRow(k) or (not FRowRecordList[k].IsModified)) and
        ((k>=Count) or (Items[k]=nil) or (Items[k].Count=0)) do
  begin
    FRowRecordList.Delete(k);
    if k<Count then Delete(k);
    dec(k);
  end;

  if Cell=nil then exit;

  if Col+1> FRowRecordList[Row].MaxCol then FRowRecordList[Row].MaxCol:=Col+1;
  if Col< FRowRecordList[Row].MinCol then FRowRecordList[Row].MinCol:=Col;

  if (Cell is TLabelSSTRecord) and (Length(RTFRuns)>0) then
  begin
    Rs.Value:=RealValue;
    Rs.RTFRuns:=Copy(RTFRuns);
    (Cell as TLabelSSTRecord).AsRichString:=Rs;
  end else
  Cell.Value:=RealValue;
  if Row>=Count then AddRecord(Cell, Row) else Items[Row].Insert(Index, Cell);
end;

procedure TCellList.FixFormulaTokens(const Formula: TFormulaRecord; const ShrFmlas: TShrFmlaRecordList);
var
  Key: Cardinal;
  Index: integer;
begin
  if not Formula.IsExp(Key) then exit;
  if ShrFmlas.Find(Key, Index) then
    Formula.MixShared(ShrFmlas[Index].Data, ShrFmlas[Index].DataSize)
  else //Array formula
  begin
    //nothing, it's ok
    //raise Exception.Create(ErrShrFmlaNotFound);
  end;
end;

function TCellList.FixTotalSize(const NeedsRecalc: boolean): int64;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do Result:=Result+Items[i].FixTotalSize(NeedsRecalc);

end;

procedure TCellList.FixFormulas(const ShrFmlas: TShrFmlaRecordList);
var
  i, k: integer;
  it: TCellRecordList;
  OldFormulaSize: integer;
begin
  for i:=0 to Count-1 do
  begin
    it:=Items[i];
    for k:=0 to it.Count-1 do
      if it.Items[k] is TFormulaRecord then
      begin
        OldFormulaSize:=(it.Items[k] as TFormulaRecord).DataSize;
        FixFormulaTokens(it.Items[k] as TFormulaRecord, ShrFmlas);
        it.AdaptSize((it.Items[k] as TFormulaRecord).DataSize-OldFormulaSize);
      end;
  end;
end;

function TCellList.GetFormula(Row, Col: integer): UTF16String;
var
  Index: integer;
begin
  if (Row<0) or (Row>Max_Rows) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);
  if Row>=Count then begin; Result:=''; exit; end;
  if Items[Row].Find(Col,Index) and (Items[Row][Index] is TFormulaRecord) then
  begin
    Result:=RPNToString(Items[Row][Index].Data, 22, Self);
  end else
  begin
    Result:='';
  end;
end;

procedure TCellList.SetFormula(Row, Col: integer; const Value: UTF16String);
begin
  AssignFormulaX(Row, Col, Value, unassigned , false); //Options1904 doesn't matter here.
end;

function TCellList.ArrayFormula(const Row, Col: integer): PArrayOfByte;
var
  Index: integer;
  Fmla: TFormulaRecord;
begin
  if (Row<0) or (Row>=Count) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);
  if Items[Row].Find(Col,Index) and (Items[Row][Index] is TFormulaRecord) then
  begin
    Fmla:=(Items[Row][Index] as TFormulaRecord);
    if Fmla.ArrayRecord=nil then raise Exception.CreateFmt(ErrBadFormula,[Row, Col,1]);
    Result:=Fmla.ArrayRecord.Data;
  end else
  begin
    raise Exception.Create(ErrShrFmlaNotFound);
  end;
end;

function TCellList.TableFormula(const Row, Col: integer): PArrayOfByte;
var
  Index: integer;
  Fmla: TFormulaRecord;
begin
  if (Row<0) or (Row>=Count) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);
  if Items[Row].Find(Col,Index) and (Items[Row][Index] is TFormulaRecord) then
  begin
    Fmla:=(Items[Row][Index] as TFormulaRecord);
    if Fmla.TableRecord=nil then raise Exception.CreateFmt(ErrBadFormula,[Row, Col,1]);
    Result:=(Items[Row][Index] as TFormulaRecord).TableRecord.Data;
  end else
  begin
    raise Exception.Create(ErrShrFmlaNotFound);
  end;
end;

function TCellList.GetName(const ExternSheet, NameId: integer): UTF16String;
begin
  Result := FGlobals.References.GetName(ExternSheet, NameId, FGlobals);
end;

function TCellList.GetSheetName(const SheetNumber: integer): UTF16String;
begin
  Result:= FGlobals.References.GetSheetName(SheetNumber, FGlobals);
end;

function TCellList.FindSheet(SheetName: UTF16String; out SheetIndex: Integer): Boolean;
var
  i: Integer;
begin
  SheetName:=WideUpperCase98(SheetName);
  for i:=0 to FGlobals.SheetCount-1 do
  begin
    if SheetName= WideUpperCase98(FGlobals.SheetName[i]) then
    begin
      SheetIndex := i;
      Result := True;
      exit;
    end;
  end;
  SheetIndex := -1;
  Result := False;
end;

function TCellList.AddExternSheet(const FirstSheet: Integer; const LastSheet: Integer): Integer;
begin
  Result := FGlobals.References.AddSheet(FGlobals.SheetCount, FirstSheet, LastSheet);
end;



procedure TCellList.AssignFormulaX(const Row, Col: integer; const Formula: UTF16String; const Value: variant; const Options1904: boolean);
var
  Cell: TCellRecord;
  ds: integer;
  Ps: TParseString;
  Index, k: integer;
  XF, DefaultXF: integer;
  Found: boolean;
begin
  if (Row<0) or (Row>Max_Rows) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);
  FRowRecordList.AddRow(Row);
  if FRowRecordList[Row].IsFormatted then DefaultXF:=FRowRecordList[Row].XF
  else if FColInfoList.Find(Col, Index) then DefaultXF:=FColInfoList[Index].XF
  else DefaultXF:=15;

  Cell:=nil;
  Found:=(Row<Count) and Items[Row].Find(Col,Index);
  XF:=DefaultXF;
  if Found then XF:=Items[Row][Index].XF;
  //if Formula.XF>=0 then XF:=Formula.XF;

  if Formula='' then Cell:=nil else
  begin
    Ps:=TParseString.Create(Formula, Self, fmValue);
    try
      Ps.Parse;
      ds:= Ps.TotalSize+20;
      Cell:= TFormulaRecord.CreateFromData(xlr_FORMULA, ds, Row, Col, XF, Value, Options1904);
      Ps.CopyToPtr(Cell.Data, 20);
    finally
      FreeAndNil(Ps);
    end;
  end;

  try
    if Found then Items[Row].Delete(Index);

    if Found and (Cell=nil) then  //We are deleting a cell
    begin
      if (Row>=Count) or (Items[Row]=nil)or(Items[Row].Count=0)then //Row emptied
        if (not FRowRecordList[Row].IsModified)  then     //Row always exists... it is added at the top
          FRowRecordList[Row]:=nil  //this frees the object
        else
        begin
          FRowRecordList[Row].MinCol:= 0;
          FRowRecordList[Row].MaxCol:= 0;
        end
      else
      begin
        FRowRecordList[Row].MinCol:= Items[Row][0].Column;
        FRowRecordList[Row].MaxCol:= Items[Row][Items[Row].Count-1].Column+1;
      end;
    end;

    //Remove all empty Rows at the end.
    k:=FRowRecordList.Count-1;
    while ((k>Row) or (Cell=nil)) and
          (k>=0) and (not FRowRecordList.HasRow(k) or (not FRowRecordList[k].IsModified)) and
          ((k>=Count) or (Items[k]=nil) or (Items[k].Count=0)) do
    begin
      FRowRecordList.Delete(k);
      if k<Count then Delete(k);
      dec(k);
    end;

    if Cell=nil then exit;

    if Col+1> FRowRecordList[Row].MaxCol then FRowRecordList[Row].MaxCol:=Col+1;
    if Col< FRowRecordList[Row].MinCol then FRowRecordList[Row].MinCol:=Col;
    if Row>=Count then AddRecord(Cell, Row) else Items[Row].Insert(Index, Cell);
  except
    FreeAndNil(Cell);
    raise;
  end; //except
end;

procedure TCellList.SetFormat(const Row, Col, XF: integer);
var
  Index: integer;
  Value: TXlsCellValue;
begin
  if (Row<0) or (Row>Max_Rows) then raise Exception.CreateFmt(ErrInvalidRow,[Row]);
  if (Col>Max_Columns)or (Col<0) then raise Exception.CreateFmt(ErrInvalidCol,[Col]);

  if FRowRecordList.HasRow(Row) and (Row<Count) and (Row>=0) and Items[Row].Find(Col,Index) then
    Items[Row][Index].XF:=XF else
  begin
    Value.Value:=null;
    Value.XF:=XF;
    SetValueX2(Row,Col,Value, nil, false); //options1904 doesn't matter here since value is null.
  end;
end;

procedure TCellList.ArrangeInsertSheet(const SheetInfo: TSheetInfo);
var
  Data: PArrayOfByte;
  i, k: integer;
  it: TCellRecordList;
begin
  for i:=0 to Count-1 do
  begin
    it:=Items[i];
    for k:=0 to it.Count-1 do
      if it.Items[k] is TFormulaRecord then
      begin
        Data:= it.Items[k].Data;
        UXlsTokenArray_ArrangeInsertSheets(Data, 22, 22 + GetWord(Data, 20), SheetInfo);
      end;
  end;
end;

procedure TCellList.RecalcRowHeights(const Workbook: pointer; const Row1: integer; const Row2: integer; const Forced: Boolean; const KeepAutofit: Boolean; const Adjustment: Extended);
var
  RowCalc: TRowHeightCalc;
  RowMultDisplay: Extended;
  ColMultDisplay: Extended;
  i: integer;
  Row: TRowRecord;
  MaxCellHeight: integer;
  Columns: TCellRecordList;
  cCount: integer;
  c: integer;
  Cell: TXlsCellValue;
  rx: TRichString;
  CellHeight: integer;
  Color, index: integer;
  XF: integer;
begin
  //For autofitting all the workoobk:
  //Row2 should be = FRowRecordList.Count - 1;
  //Row1 should be 0.
  RowCalc := TRowHeightCalc.Create(FGlobals);
  try
    RowMultDisplay := RowMult;
    ColMultDisplay := ColMult;
    for i := Row1 to Row2 do
    begin
      if not FRowRecordList.HasRow(i) then
        continue;

      Row := FRowRecordList[i];
      if Row = nil then
        continue;

      if not Forced and not Row.IsAutoHeight then
        continue;

      rx.Value:='';
      SetLength(rx.RTFRuns, 0);
      MaxCellHeight := RowCalc.CalcCellHeight(i + 1, -1, rx, Row.XF, Workbook, RowMultDisplay, ColMultDisplay);
      if i < Count then
      begin
        Columns := Self[i];
        cCount := Columns.Count;
        for c := 0 to cCount - 1 do
        begin
          GetValueX2(i, Columns[c].Column, Cell, Rx.RTFRuns);
          XF:= Cell.XF;
          if XF<0 then
          begin
            XF:=FRowRecordList[i].XF;
            if (XF<=0) and (FColInfoList.Find(Columns[c].Column, index)) then XF:=  FColInfoList[index].XF;
          end;
          if (XF<0) then XF:=15;

          rx.Value:= XlsFormatValue1904(Cell.Value, TExcelFile(Workbook).FormatList[XF].Format, TExcelFile(Workbook).Options1904Dates, Color);

          CellHeight := RowCalc.CalcCellHeight(i + 1, Columns[c].Column + 1, rx, XF, Workbook, RowMultDisplay, ColMultDisplay);
          if CellHeight > MaxCellHeight then MaxCellHeight := CellHeight;
        end;

      end;

      if (Adjustment <> 1) and (Adjustment >= 0) then
        MaxCellHeight := Round(MaxCellHeight * Adjustment);

      if MaxCellHeight > $7FFF then
        MaxCellHeight := $7FFF;

      Row.Height := word(MaxCellHeight);
      if not KeepAutofit then
        Row.ManualHeight;

    end;
  finally
    FreeAndNil(RowCalc);
  end;
end;

procedure TCellList.AutofitColumn(const Workbook: pointer; const Column: integer; const ColCalc: TColWidthCalc; const RowMultDisplay: Extended; const ColMultDisplay: Extended; const IgnoreStrings: Boolean; const Adjustment: Extended);
var
  MaxWidth: integer;
  r: integer;
  CellWidth: integer;

  Cell: TXlsCellValue;
  rx: TRichString;
  Color, index: integer;
  XF: integer;

begin
  MaxWidth := 0;
  for r :=  FRowRecordList.Count - 1 downto 0 do
  begin
    GetValueX2(r, Column, Cell, Rx.RTFRuns);
    XF:= Cell.XF;
    if XF<0 then
    begin
      if (FRowRecordList.HasRow(r)) then XF:=FRowRecordList[r].XF;
      if (XF<=0) and (FColInfoList.Find(Column, index)) then XF:=  FColInfoList[index].XF;
    end;
    if (XF<0) then XF:=15;

    rx.Value:= XlsFormatValue1904(Cell.Value, TExcelFile(Workbook).FormatList[XF].Format, TExcelFile(Workbook).Options1904Dates, Color);

    if IgnoreStrings then
    begin
      if (Items[r].Find(Column,Index)) and (Items[r][index] is TLabelSSTRecord) then continue;
    end;

    CellWidth := ColCalc.CalcCellWidth(r + 1, Column + 1, rx, XF, Workbook, RowMultDisplay, ColMultDisplay);
    if CellWidth > MaxWidth then
      MaxWidth := CellWidth;

  end;

  if (Adjustment <> 1) and (Adjustment >= 0) then
    MaxWidth := Round(MaxWidth * Adjustment) ;

  if MaxWidth > $FFFF then
    MaxWidth := $FFFF;

  if MaxWidth > 0 then
    TExcelFile(Workbook).ColumnWidth[Column + 1]:= MaxWidth;

end;


procedure TCellList.RecalcColWidths(const Workbook: pointer; const Col1, Col2: integer; const IgnoreStrings: Boolean; const Adjustment: Extended);
var
  ColCalc: TColWidthCalc;
  RowMultDisplay: Extended;
  ColMultDisplay: Extended;
  c: integer;
begin
    ColCalc := TColWidthCalc.Create(FGlobals);
    try
      RowMultDisplay := RowMult;
      ColMultDisplay := ColMult;
      for c := Col1 to Col2 do
      begin
        AutofitColumn(Workbook, c,  ColCalc, RowMultDisplay, ColMultDisplay, IgnoreStrings, Adjustment);
      end;
    finally
      FreeAndNil(ColCalc);
    end;
end;


procedure TCellList.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount,
  InsColPos, InsColCount: integer; const SheetInfo: TSheetInfo);
begin
  inherited ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
  if (InsColCount > 0) then
    FColInfoList.ArrangeInsertCols(InsColPos, InsColCount, SheetInfo);
end;

procedure TCellList.InsertAndCopyCols(const FirstCol, LastCol, DestCol,
  aCount: integer; const SheetInfo: TSheetInfo;
  const OnlyFormulas: boolean);
var
  NewFirstCol: integer;
  NewLastCol: integer;
begin
  inherited InsertAndCopyCols(FirstCol, LastCol, DestCol, aCount, SheetInfo, OnlyFormulas);
  if (aCount > 0) then
  begin
    NewFirstCol := FirstCol;
    NewLastCol := LastCol;
    if (DestCol <= FirstCol) then
    begin
      NewFirstCol := FirstCol + (LastCol - FirstCol + 1) * aCount;
      NewLastCol := LastCol + (LastCol - FirstCol + 1) * aCount;
    end;

    FColInfoList.CopyCols(NewFirstCol, NewLastCol, DestCol, aCount, SheetInfo);
  end;

end;

{ TCells }

procedure TCells.AddCell(const aRecord: TCellRecord; const aRow: integer);
begin
  FCellList.AddRecord(aRecord, aRow);
end;

procedure TCells.AddMultipleCells(const aRecord: TMultipleValueRecord);
var
  OneRec: TCellRecord;
begin
  while not aRecord.Eof do
  begin
    OneRec:=aRecord.ExtractOneRecord;
    FCellList.AddRecord( OneRec, OneRec.Row);
  end;
end;

procedure TCells.AddRow(const aRecord: TRowRecord);
begin
  FRowList.AddRecord(aRecord);
end;

procedure TCells.ArrangeInsertRowsAndCols(const InsRowPos, InsRowCount, InsColPos, InsColCount: integer;
  const SheetInfo: TSheetInfo);
begin
  FRowList.ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
  FCellList.ArrangeInsertRowsAndCols(InsRowPos, InsRowCount, InsColPos, InsColCount, SheetInfo);
end;

procedure TCells.Clear;
begin
  if FRowList<>nil then FRowList.Clear;
  if FCellList<>nil then FCellList.Clear;
end;

procedure TCells.CopyFrom(const aList: TCells);
begin
  FRowList.CopyFrom(aList.FRowList);
  FCellList.CopyFrom(aList.FCellList);
end;

constructor TCells.Create(const aGlobals: TWorkbookGlobals; const aColInfoList: TColInfoList);
begin
  inherited Create;
  FRowList:=TRowRecordList.Create;
  FCellList:=TCellList.Create(aGlobals, FRowList, aColInfoList);
end;

procedure TCells.DeleteRows(const aRow, aCount: word; const SheetInfo: TSheetInfo);
begin
  FRowList.DeleteRows(aRow, aCount, SheetInfo);
  FCellList.DeleteRows(aRow, aCount, SheetInfo);
end;

procedure TCells.DeleteCols(const aCol, aCount: word; const SheetInfo: TSheetInfo);
begin
  FCellList.DeleteCols(aCol, aCount, SheetInfo);
  ArrangeCols;
end;

destructor TCells.Destroy;
begin
  FreeAndNil(FRowList);
  FreeAndNil(FCellList);
  inherited;
end;

procedure TCells.InsertAndCopyRows(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
begin
  FRowList.InsertAndCopyRows(FirstRow, LastRow, DestRow, aCount, SheetInfo);
  FCellList.InsertAndCopyRows(FirstRow, LastRow, DestRow, aCount, SheetInfo, OnlyFormulas);
end;

procedure TCells.InsertAndCopyCols(const FirstCol, LastCol, DestCol,
  aCount: integer; const SheetInfo: TSheetInfo; const OnlyFormulas: boolean);
begin
  FCellList.InsertAndCopyCols(FirstCol, LastCol, DestCol, aCount, SheetInfo, OnlyFormulas);
  ArrangeCols;
end;

procedure TCells.ArrangeCols;
var
  i: integer;
begin
  for i:= 0 to FRowList.Count-1 do
    if (FRowList.HasRow(i)) then
    begin
      if ((i<FCellList.Count) and (FCellList[i]<>nil) and (FCellList[i].Count>0)) then
      begin
        FRowList[i].MinCol:= FCellList[i][0].Column;
        FRowList[i].MaxCol:= FCellList[i][FCellList[i].Count-1].Column+1;
      end
      else
      begin
        FRowList[i].MinCol:= 0;
        FRowList[i].MaxCol:= 0;
      end;
    end;
end;

function TCells.DimensionsSize: integer;
begin
  Result:= SizeOf(TDimensionsRec)+SizeOf(TRecordHeader);
end;

procedure TCells.CalcUsedRange(var CellRange: TXlsCellRange);
var
  i: integer;
begin
  CellRange.Top:=0;
  while (int64(CellRange.Top)<RowList.Count) and not RowList.HasRow(CellRange.Top) do inc(CellRange.Top);
  CellRange.Bottom:=RowList.Count-1;
  CellRange.Left:=0;
  CellRange.Right:=0;
  for i:=CellRange.Top to RowList.Count-1 do
    if RowList.HasRow(i) then
    begin
      if RowList[i].MaxCol>CellRange.Right then CellRange.Right:=RowList[i].MaxCol;
      if RowList[i].MinCol<CellRange.Left then CellRange.Left:=RowList[i].MinCol;
    end;
  if CellRange.Right>0 then Dec(CellRange.Right); //MaxCol is the max col+1
end;

procedure TCells.WriteDimensions(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  DimRec: TDimensionsRecord;
  DimRecDat: PDimensionsRec;
begin
  GetMem(DimRecDat, SizeOf(TDimensionsRec));
  try
    DimRecDat.FirstRow:=CellRange.Top;
    DimRecDat.LastRow:=CellRange.Bottom+1; //This adds an extra row. Dimensions do from firstrow to lastrow+1
    DimRecDat.FirstCol:=CellRange.Left;
    DimRecDat.LastCol:=CellRange.Right+1;
    DimRecDat.Extra:=0;
    DimRec:=TDimensionsRecord.Create(xlr_DIMENSIONS, PArrayOfByte(DimRecDat), SizeOf(TDimensionsRec));
  except
    FreeMem(DimRecDat);
    raise;
  end;
  try
    DimRec.SaveToStream(DataStream);
  finally
    FreeAndNil(DimRec);
  end; //Finally
end;

procedure TCells.SaveToStream(const DataStream: TOle2File);
var
  CellRange: TXlsCellRange;
begin
  CalcUsedRange(CellRange);
  SaveRangetoStream(DataStream, CellRange);
end;

function TCells.TotalSize: int64;
begin
  Result := FixTotalSize(false);
end;

function TCells.FixTotalSize(const NeedsRecalc: boolean): int64;
begin
  Result:= DimensionsSize + FRowList.TotalSize + FCellList.FixTotalSize(NeedsRecalc);
end;

procedure TCells.FixRows;
var
  i: integer;
begin
  if FRowList.Count>= FCellList.Count then exit;
  for i:=0 to FCellList.Count - 1 do
    if (not FRowList.HasRow(i) and (FCellList[i].Count>0)) then FRowList.AddRow(i);

  if (FCellList.Count >0) then FRowList.AddRow(FCellList.Count-1);
end;

procedure TCells.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  i,k,j, Written :integer;
begin
  WriteDimensions(DataStream, CellRange);
  i:=CellRange.Top;
  while (i<=CellRange.Bottom) do
  begin
    k:=0;Written:=0;
    while (Written<32) and (k+i<=CellRange.Bottom) do
    begin
      if FRowList.HasRow(k+i) then
      begin
        FRowList[k+i].SaveRangeToStream(DataStream, CellRange.Left, CellRange.Right);
        //inc(Written);  //We want 32 records in total, counting blanks. that's why not here
      end;
      inc(Written);
      inc(k);
    end;

    for j:= i to k+i-1 do
      if (j<=CellRange.Bottom) and (j<FCellList.Count) then FCellList[j].SaveRangeToStream(DataStream, CellRange);

    inc(i, k);
  end;

end;


function TCells.TotalRangeSize(const CellRange: TXlsCellRange): int64;
begin
  TotalRangeSize:= DimensionsSize + FRowList.TotalRangeSize(CellRange, false) + FCellList.TotalRangeSize(CellRange);
end;

procedure TCells.ArrangeInsertSheet(const SheetInfo: TSheetInfo);
begin
  FCellList.ArrangeInsertSheet(SheetInfo);
end;

{ TRangeList }

procedure TRangeList.CopyFrom(const aRangeList: TRangeList);
var
  i: integer;
begin
  for i:=0 to aRangeList.Count - 1 do
    Add(aRangeList.Items[i].CopyTo);
end;

procedure TRangeList.DeleteRowsOrCols(const aRow, aCount: word;
  const SheetInfo: TSheetInfo; const UseCols: boolean);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].DeleteRowsOrCols(aRow, aCount, SheetInfo, UseCols);
end;

procedure TRangeList.InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].InsertAndCopyRowsOrCols(FirstRow, LastRow, DestRow, aCount, SheetInfo, UseCols);
end;

procedure TRangeList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveRangeToStream(DataStream, CellRange);
end;

procedure TRangeList.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
begin
  for i:=0 to Count-1 do Items[i].SaveToStream(DataStream);
end;

function TRangeList.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do Result:=Result+Items[i].TotalRangeSize(CellRange);
end;

function TRangeList.TotalSize: int64;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do Result:=Result+Items[i].TotalSize;
end;


{ TRowHeightCalc }
constructor TRowHeightCalc.Create(const aWg: TWorkbookGlobals);
begin
{$IFNDEF FIREMONKEY}
  inherited Create;
  Wg := aWg;
  bmp := TBitmap.Create;
  bmp.Height := 1;
  bmp.Width := 1;
  Canvas := bmp.Canvas;
  InitXF;
{$ENDIF}
end;

{$IFNDEF FIREMONKEY}
procedure TRowHeightCalc.InitXF();
var
  i: integer;
  xf: TXFRecord;
  FontIndex: integer;
  Fr: TFontRecord;
begin
  SetLength (XFHeight, Wg.XF.Count);
  FillChar(XFHeight[0], Length(XFHeight), 0);
  SetLength (XFFonts, Length(XFHeight));
  FillChar(XFFonts[0], Length(XFFonts), 0);
  for i := 0 to Length(XFHeight) - 1 do
  begin
    xf := Wg.XF[i];
    FontIndex := xf.GetActualFontIndex(Wg.Fonts);
    Fr := Wg.Fonts[FontIndex];
    XFFonts[i] := Fr.FlxFont;
    Canvas.Lock;
    try
      Canvas.Font.Name := XFFonts[i].Name;
      Canvas.Font.Size := Round(XFFonts[i].Size20 / 20);
      XFHeight[i] := Ceil(Canvas.TextHeight('Mg') * RowMult);
    finally
      Canvas.Unlock;
    end;
  end;
end;
{$ENDIF}


function CalcAngle(const ExcelRotation: integer; var Vertical: boolean): extended;
begin
  Vertical:=ExcelRotation=255;
  if ExcelRotation<0 then Result:=0
  else if ExcelRotation<=90 then Result:=ExcelRotation*2*pi/360
  else if ExcelRotation<=180 then Result:=(90-ExcelRotation)*2*pi/360
  else Result:=0;
end;


function TRowHeightCalc.CalcCellHeight(const Row: integer; const Col: integer; const val: TRichString; const XF: integer; const Workbook: pointer; const RowMultDisplay: Extended; const ColMultDisplay: Extended): integer;
begin
  Result := 255;
end;

destructor TRowHeightCalc.Destroy;
begin
{$IFNDEF FIREMONKEY}
  FreeAndNil(bmp);
{$ENDIF}
  inherited;
end;

{ TColWidthCalc }
constructor TColWidthCalc.Create(const aWg: TWorkbookGlobals);
begin
{$IFNDEF FIREMONKEY}

  inherited Create;
  Wg := aWg;
  bmp := TBitmap.Create;
  bmp.Height := 1;
  bmp.Width := 1;
  Canvas := bmp.Canvas;
  InitXF;
{$ENDIF}
end;

{$IFNDEF FIREMONKEY}
procedure TColWidthCalc.InitXF();
var
  i: integer;
  xf: TXFRecord;
  FontIndex: integer;
  Fr: TFontRecord;
begin
  SetLength (XFFonts, Wg.XF.Count);
  FillChar(XFFonts[0], Length(XFFonts), 0);
  for i := 0 to Length(XFFonts) - 1 do
  begin
    xf := Wg.XF[i];
    FontIndex := xf.GetActualFontIndex(Wg.Fonts);
    Fr := Wg.Fonts[FontIndex];
    XFFonts[i] := Fr.FlxFont;
  end;
end;
{$ENDIF}


function TColWidthCalc.CalcCellWidth(const Row: integer; const Col: integer; const val: TRichString; const XF: integer; const Workbook: pointer; const RowMultDisplay: Extended; const ColMultDisplay: Extended): integer;
begin
  Result := 0;
end;

destructor TColWidthCalc.Destroy;
begin
{$IFNDEF FIREMONKEY}
  FreeAndNil(bmp);
{$ENDIF}
  inherited;
end;


end.
