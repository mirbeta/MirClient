unit tmsUXlsBaseRecordLists;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses SysUtils, Contnrs, Classes, tmsXlsMessages,
     tmsUXlsBaseRecords, tmsUXlsOtherRecords, tmsUXlsFormula, tmsUXlsBaseList,
     tmsUFlxMessages, tmsUOle2Impl;

type

  TBaseRecordList = class(TBaseList) //Records are TBaseRecord
    {$INCLUDE TBaseRecordListHdr.inc}
  private
    FTotalSize: int64;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    function GetTotalSize: int64; virtual;
  public
    procedure AdaptSize(Delta: integer);

    procedure CopyFrom(const aBaseRecordList: TBaseRecordList);
    property TotalSize: int64 read GetTotalSize;
    procedure SaveToStream(const DataStream: TOle2File); virtual;
  end;

  TBaseRowColRecordList = class(TBaseRecordList) //Records are TBaseRowColRecord
    {$INCLUDE TBaseRowColRecordListHdr.inc}
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);virtual;

    function TotalRangeSize(const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;virtual;
  end;

  TNameRecordList = class(TBaseRecordList) //Records are TNameRecord
    {$INCLUDE TNameRecordListHdr.inc}
    procedure ArrangeInsertRowsAndCols(const InsRow, aRowCount, InsCol, aColCount: integer; const SheetInfo: TSheetInfo);
    procedure InsertSheets(const CopyFrom, BeforeSheet:integer;  SheetCount: integer; SheetInfo: TSheetInfo);
    procedure DeleteSheets(const SheetIndex, SheetCount: integer);
  end;

  TBoundSheetRecordList = class (TBaseRecordList)
  private
    function GetSheetName(index: integer): UTF16String;
    procedure SetSheetName(index: integer; const Value: UTF16String);
    function GetSheetVisible(index: integer): TXlsSheetVisible;
    procedure SetSheetVisible(index: integer; const Value: TXlsSheetVisible);
  public
    {$INCLUDE TBoundSheetRecordListHdr.inc}
    property SheetName[index: integer]: UTF16String read GetSheetName write SetSheetName;
    property SheetVisible[index: integer]: TXlsSheetVisible read GetSheetVisible write SetSheetVisible;
  end;

  TCellRecordList = class (TBaseRowColRecordList)
    {$INCLUDE TCellRecordListHdr.inc}

    private
      procedure GoNext(var i: integer; const aCount: integer; var it: TCellRecord; var NextRec: TCellRecord);
    function SaveAndCalcRange(const DataStream: TOle2File;
      const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;
    public
      procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange); override;
      function TotalRangeSize(const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64; override;
      procedure SaveToStream(const DataStream: TOle2File); override;
      function GetTotalSize: int64;override;
      function FixTotalSize(const NeedsRecalc: boolean): int64;

  end;

  TRowRecordList = class (TBaseRowColRecordList)
  private
    function GetItems(index: integer): TRowRecord;
    procedure SetItems(index: integer; const Value: TRowRecord);
  public
    function AddRecord(aRecord: TRowRecord): integer;
    property Items[index: integer]: TRowRecord read GetItems write SetItems; default;
    function HasRow(const Index: integer): boolean;
    procedure AddRow(const Index: word);
    procedure InsertAndCopyRows(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo);
    procedure DeleteRows(const aRow, aRowCount: word; const SheetInfo: TSheetInfo);

    function RowHeight(const aRow: integer): integer;
    procedure SetRowHeight(const aRow: integer; const Height: word);

    procedure AutoRowHeight(const aRow: integer; const Value: boolean);
    function IsAutoRowHeight(const aRow: integer): boolean;

    function TotalRangeSize(const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64; override;

    procedure CalcGuts(const Guts: TGutsRecord);
  end;

  TShrFmlaRecordList=class(TBaseRecordList)
    {$INCLUDE TShrFmlaRecordListHdr.inc}
  end;

implementation
{$INCLUDE TShrFmlaRecordListImp.inc}
{$INCLUDE TBaseRecordListImp.inc}
{$INCLUDE TBaseRowColRecordListImp.inc}
{$INCLUDE TNameRecordListImp.inc}
{$INCLUDE TBoundSheetRecordListImp.inc}
{$INCLUDE TCellRecordListImp.inc}


{ TBaseRecordList }

procedure TBaseRecordList.AdaptSize(Delta: integer);
begin
  Inc(FTotalSize, Delta);
end;

procedure TBaseRecordList.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
  it: TBaseRecord;
begin
  for i:=0 to Count-1 do
  begin
    it:=(Items[i] as TBaseRecord);
    if it<>nil then it.SaveToStream(DataStream);
  end;
end;

procedure TBaseRecordList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Ptr<>nil then
  begin
    if Action = lnDeleted then FTotalSize:= FTotalSize-TBaseRecord(Ptr).TotalSize;
    if Action = lnAdded then FTotalSize:= FTotalSize+TBaseRecord(Ptr).TotalSize;
  end;
  inherited Notify(Ptr, Action);
end;

procedure TBaseRecordList.CopyFrom(const aBaseRecordList: TBaseRecordList);
var
  i:integer;
begin
  if aBaseRecordList=nil then exit;
  for i:=0 to aBaseRecordList.Count-1 do Add((aBaseRecordList[i] as TBaseRecord).CopyTo);
end;

function TBaseRecordList.GetTotalSize: int64;
begin
  Result:= FTotalSize;
end;

{ TBaseRowColRecordList }
procedure TBaseRowColRecordList.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
var
  i: integer;
  it: TBaseRowColRecord;
begin
  for i:=0 to Count-1 do
  begin
    it:= Items[i];
    if it<>nil then it.ArrangeCopyRowsAndCols(RowOffset, ColOffset);
  end;
end;

procedure TBaseRowColRecordList.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo);
var
  i: integer;
  it: TBaseRowColRecord;
begin
  for i:=0 to Count-1 do
  begin
    it:=Items[i];
    if it<>nil then it.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo);
  end;
end;

procedure TBaseRowColRecordList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  i, r, c:integer;
  it: TBaseRowColRecord;
begin
  for i:=0 to Count-1 do
  begin
    it:=(Items[i] as TBaseRowColRecord);
    if (it<>nil) then
    begin
      r:=it.Row;c:=it.Column;
      if  (r>=CellRange.Top) and (r<=CellRange.Bottom)
         and (c>=CellRange.Left) and (c<=CellRange.Right) then
           it.SaveToStream(DataStream);
    end;
  end;
end;

function TBaseRowColRecordList.TotalRangeSize(const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;
var
  i:integer;
  it: TBaseRowColRecord;
begin
  Result:=0;
  for i:=0 to Count-1 do
  begin
    it:=(Items[i] as TBaseRowColRecord);
    if (it<>nil)and (it.Row>=CellRange.Top) and (it.Row<=CellRange.Bottom)
       and (it.Column>=CellRange.Left) and (it.Column<=CellRange.Right) then
       Result:=Result+it.FixTotalSize(NeedsRecalc);
  end;
end;


{ TNameRecordList }

procedure TNameRecordList.ArrangeInsertRowsAndCols(const InsRow, aRowCount, InsCol, aColCount: integer; const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeInsertRowsAndCols(InsRow, aRowCount, InsCol, aColCount, SheetInfo);
end;

procedure TNameRecordList.DeleteSheets(const SheetIndex,
  SheetCount: integer);
var
  i: integer;
begin
  for i:=Count-1 downto 0 do
  begin
    if (Items[i].RangeSheet>=SheetIndex) and (Items[i].RangeSheet<SheetIndex+SheetCount) then
      Delete(i)
    else
      Items[i].ArrangeInsertSheets(SheetIndex, -SheetCount);
  end;
end;

procedure TNameRecordList.InsertSheets(const CopyFrom, BeforeSheet:integer;
  SheetCount: integer; SheetInfo: TSheetInfo);
var
  i, k, MyCount: integer;
begin
  MyCount:=Count;
  for i:=0 to MyCount-1 do
  begin
    if (CopyFrom>=0) and
    (
      (Items[i].RangeSheet=CopyFrom) or
      ((Items[i].RangeSheet=-1) and (Items[i].RefersToSheet(SheetInfo.GetSheet)=SheetInfo.FormulaSheet))
    )then
    begin
       for k:=0 to SheetCount-1 do
       begin
         SheetInfo.InsSheet:=BeforeSheet+k;
         Add((Items[i].CopyTo as TNameRecord).ArrangeCopySheet(SheetInfo));
       end;
    end;

    Items[i].ArrangeInsertSheets(BeforeSheet, SheetCount);
  end;
end;


{ TRowRecordList }
function TRowRecordList.AddRecord(aRecord: TRowRecord):integer;
var
  i:integer;
begin
  if aRecord.GetRow<Count then
  begin
    if inherited Items[aRecord.GetRow]=nil then Items[aRecord.GetRow]:=aRecord; //else Raise Exception.Create(ErrDupRow);
    Result:= aRecord.GetRow;
  end
  else
  begin
    for i:=Count to aRecord.GetRow-1 do inherited Add(nil);
    Result:=inherited Add(aRecord);
  end;
end;

function TRowRecordList.GetItems(index: integer): TRowRecord;
begin
  Result := inherited Items[Index] as TRowRecord;
  if Result=nil then raise Exception.CreateFmt(ErrRowMissing,[Index]);
end;

function TRowRecordList.HasRow(const Index: integer): boolean;
begin
 Result:= (Index>=0) and(Index<Count) and (inherited Items[Index]<>nil);
end;

procedure TRowRecordList.DeleteRows(const aRow, aRowCount: word; const SheetInfo: TSheetInfo);
var
  i, Max: integer;
begin
  Max:=aRow+aRowCount ; if Max>Count then Max:= Count;
  for i:= Max-1 downto aRow do Delete(i);
  //Delete the cells. we can look only for those below arow
  for i:=aRow to Count-1 do if HasRow(i) then Items[i].ArrangeInsertRowsAndCols(aRow, -aRowCount, 0, 0, SheetInfo);

end;


procedure TRowRecordList.InsertAndCopyRows(const FirstRow, LastRow,
  DestRow, aCount: integer; const SheetInfo: TSheetInfo);
var
  i, k, z, CopyOffs, MyDestRow: integer;
  aRecord: TRowRecord;
begin
  //Insert the cells. we can look only for those below destrow
  for i:=DestRow to Count-1 do if HasRow(i) then Items[i].ArrangeInsertRowsAndCols(DestRow, aCount*(LastRow-FirstRow+1), 0, 0, SheetInfo);
  //Copy the cells
  MyDestRow:=DestRow;
  CopyOffs:=0;
  for k:=1 to aCount do
    for i:=FirstRow to LastRow do
    begin
      aRecord:=nil;
      try
        if (i+CopyOffs<Count) and HasRow(i+CopyOffs) then
        begin
          aRecord:=Items[i+CopyOffs].CopyTo as TRowRecord;
          aRecord.ArrangeCopyRowsAndCols(MyDestRow-aRecord.Row,0);
        end;
        for z:= Count to MyDestRow-1 do Add(nil);
        Insert(MyDestRow, aRecord);
        aRecord:=nil;
      finally
        FreeAndNil(aRecord);
      end; //finally
      Inc(MyDestRow);
      if FirstRow>=DestRow then Inc(CopyOffs);
    end;
end;

procedure TRowRecordList.SetItems(index: integer; const Value: TRowRecord);
begin
  inherited Items[Index] := Value;
end;

procedure TRowRecordList.AddRow(const Index: word);
var
  aRecord: TRowRecord;
begin
  if HasRow(Index) then exit;
  aRecord:= TRowRecord.CreateStandard(Index);
  AddRecord(aRecord);
end;

function TRowRecordList.RowHeight(const aRow: integer): integer;
begin
  if not HasRow(aRow) then Result:=0 else Result:=Items[aRow].Height;
end;

procedure TRowRecordList.SetRowHeight(const aRow: integer; const Height: word);
begin
  AddRow(aRow);
  Items[aRow].Height:=Height;
  Items[aRow].ManualHeight;
end;

procedure TRowRecordList.AutoRowHeight(const aRow: integer;const Value: boolean);
begin
  if HasRow(aRow) then
    if Value then Items[aRow].AutoHeight else Items[aRow].ManualHeight;
end;


function TRowRecordList.IsAutoRowHeight(const aRow: integer): boolean;
begin
  if HasRow(aRow) then
    Result:= Items[aRow].IsAutoHeight else Result:=True;
end;

function TRowRecordList.TotalRangeSize(const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;
var
  i: integer;
begin
  Result:=0;
  for i:= CellRange.Top to CellRange.Bottom do Result:=Result+Items[i].FixTotalSize(NeedsRecalc);
end;

procedure TRowRecordList.CalcGuts(const Guts: TGutsRecord);
var
  MaxGutsLevel: integer;
  GutsLevel: integer;
  i: integer;
begin
  MaxGutsLevel:=0;
  for i:=0 to Count-1 do
  begin
    if HasRow(i) then
    begin
      GutsLevel:=items[i].Options and $07;
      if GutsLevel>MaxGutsLevel then MaxGutsLevel:=GutsLevel;
    end;
  end;
  Guts.RowLevel:=MaxGutsLevel;
end;

{ TBoundSheetRecordList }

function TBoundSheetRecordList.GetSheetName(index: integer): UTF16String;
begin
  Result:= Items[index].SheetName;
end;

function TBoundSheetRecordList.GetSheetVisible(index: integer): TXlsSheetVisible;
begin
  //Wrong docs? the byte to hide a sheet is the low, not the high on grbit
  case byte(Items[index].OptionFlags) and $3 of
    1: Result:=sv_Hidden;
    2: Result:=sv_VeryHidden;
    else Result:=sv_Visible;
  end; //case
end;

procedure TBoundSheetRecordList.SetSheetName(index: integer;
  const Value: UTF16String);
var
  OldSize: integer;
begin
  OldSize:=Items[index].TotalSize;
  Items[index].SheetName:=Value;
  AdaptSize(Items[index].TotalSize-OldSize);
end;

procedure TBoundSheetRecordList.SetSheetVisible(index: integer;
  const Value: TXlsSheetVisible);
var
  w: word;
  p: PArrayOfByte;
begin
  //Wrong docs? the byte to hide a sheet is the low, not the high on grbit

  w:=Items[index].OptionFlags;
  p:=@w;
  p[0]:=p[0] and ($FF-$3); //clear the 2 first bytes;
  case Value of
    sv_Hidden: p[0]:=p[0] or $1;
    sv_VeryHidden: p[0]:=p[0] or $2;
  end; //case
  Items[index].OptionFlags:=w;
end;

{ TCellRecordList }

procedure TCellRecordList.GoNext(var i: integer; const aCount: integer;
  var it: TCellRecord; var NextRec: TCellRecord);
begin
  it:=NextRec;
  inc(i);
  if (i<aCount) then NextRec:=Items[i] else NextRec:=nil;
end;

function TCellRecordList.SaveAndCalcRange(const DataStream: TOle2File;
  const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;
var
  aCount: integer;
  it, it2: TCellRecord;
  NextRec, NextRec2: TCellRecord;
  i, c, i2: integer;
  JoinSize: integer;
begin
  Result:=0;
  aCount:=Count;
  if  (aCount<=0) or ((Items[0].Row<CellRange.Top) or (Items[0].Row>CellRange.Bottom)) then exit;
  if (aCount<=0) then exit;

  NextRec:=Items[0];
  it:=nil;
  i:=0;
  while (i<aCount) do
  begin
    GoNext(i, aCount, it, NextRec);
    if (it<>nil) then
    begin
      c:=it.Column;
      if ((c>=CellRange.Left) and (c<=CellRange.Right)) then
      begin
        //Search for MulRecords. We need 2 blanks together for this to work.
        if (it.CanJoinNext(NextRec, CellRange.Right)) then
        begin
          //Calc Total.
          i2:=i; it2:=it; NextRec2:=NextRec;
          JoinSize:=it2.TotalSizeFirst;
          GoNext(i2, aCount, it2, NextRec2);

          while (it2.CanJoinNext(NextRec2, CellRange.Right)) do
          begin
            inc(JoinSize, it2.TotalSizeMid);
            GoNext(i2, aCount, it2, NextRec2);
          end;
          inc(JoinSize, it2.TotalSizeLast);
          inc(Result, JoinSize);

          if (DataStream <> nil) then
          begin
            it.SaveFirstMul(DataStream, JoinSize);
            GoNext(i, aCount, it, NextRec);

            while (it.CanJoinNext(NextRec, CellRange.Right)) do
            begin
              it.SaveMidMul(DataStream);
              GoNext(i, aCount, it, NextRec);
            end;
            it.SaveLastMul(DataStream);
          end else
          begin
            i:=i2;
            it:=it2;
            NextRec:=NextRec2;
          end;
        end
          else
          begin
            if (DataStream <> nil) then it.SaveToStream(DataStream);
            inc(Result, it.FixTotalSize(NeedsRecalc));
          end;
      end;
    end;
  end;
end;

procedure TCellRecordList.SaveToStream(const DataStream: TOle2File);
const
  CellRange: TXlsCellRange=(Left: 0; Top: 0; Right: Max_Columns; Bottom: Max_Rows);
begin
  SaveRangeToStream (DataStream, CellRange);
end;

function TCellRecordList.TotalRangeSize(
  const CellRange: TXlsCellRange; const NeedsRecalc: boolean): int64;
begin
  Result:=SaveAndCalcRange(nil, CellRange, NeedsRecalc);
end;

function TCellRecordList.FixTotalSize(const NeedsRecalc: boolean): int64;
const
  CellRange: TXlsCellRange=(Left: 0; Top: 0; Right: Max_Columns; Bottom: Max_Rows);
begin
  Result:= TotalRangeSize(CellRange, NeedsRecalc);
end;

function TCellRecordList.GetTotalSize: int64;
begin
  Result:= FixTotalSize(false);
end;

procedure TCellRecordList.SaveRangeToStream(const DataStream: TOle2File;
  const CellRange: TXlsCellRange);
begin
  SaveAndCalcRange(DataStream, CellRange, false);
end;

end.

