unit tmsUXlsRangeRecords;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords,
     tmsXlsMessages, Classes, SysUtils, tmsUFlxMessages, Math, tmsUOle2Impl;

type
  TExcelRange= packed record
    R1, R2, C1, C2: word;
  end;
  PExcelRange= ^TExcelRange;

  TRangeValuesList= class(TList) //Items are TExcelRange
  private
    FOtherDataLen :word;
    MaxRangesPerRecord: integer;
    procedure CopyIntersectRange(const R, Rx: PExcelRange; const NewFirstRow, NewLastRow, DestRow, aCount: integer; var MinR1, MaxR2: Word);
    function NextInRange(const Range: TXlsCellRange; const k: integer): integer;
  protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
      constructor Create(const aMaxRangesPerRecord: integer; const aOtherDataLen: word);

      procedure Load(const aRecord: TBaseRecord; const aPos: integer);

      //these methods are to split the record repeating it
      procedure SaveToStreamR(const DataStream: TOle2File; const Line: integer);
      procedure SaveRangeToStreamR(const DataStream: TOle2File; const Line: integer; const aCount: integer; const Range: TXlsCellRange);
      function TotalSizeR(const aCount: integer): int64;
      function RepeatCountR(const aCount: integer): integer;
      function RecordSizeR(const Line: integer; const aCount:integer): integer;
      function CountRangeRecords(const Range: TXlsCellRange): integer;

      procedure CopyFrom( const RVL: TRangeValuesList);

      procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer);

     //Formats are copied if the range intersects with the original. (Merged cells need all the range to be inside the original)
      procedure CopyRowsInclusive(const FirstRow, LastRow, DestRow, aCount: integer; var MinR1, MaxR2: Word; const UseCols: boolean);
      procedure CopyRowsExclusive(const FirstRow, LastRow, DestRow, aCount: integer; const UseCols: boolean);
      procedure DeleteRows(const aRow, aCount: integer; const Allow1Cell: boolean; const UseCols: boolean);
      procedure PreAddNewRange(var R1,C1,R2,C2: integer);
      procedure AddNewRange(const FirstRow, FirstCol, LastRow, LastCol: integer);
  end;

  TRangeEntry = class
  private
  protected
    RangeValuesList: TRangeValuesList;
    function DoCopyTo: TRangeEntry; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function CopyTo: TRangeEntry;

    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TRangeRecord);virtual;abstract;
    procedure SaveToStream(const DataStream: TOle2File);virtual;abstract;
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);virtual;abstract;
    function TotalSize: int64;virtual; abstract;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;virtual; abstract;

    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);virtual;
    procedure InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean); virtual;
    procedure DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);virtual;
  end;

  //Merged cells can't be continued. We have to write independent records.
  TMergedCells = class (TRangeEntry)
  private
  public
    constructor Create; override;

    procedure Clear;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TRangeRecord); override;
    procedure SaveToStream(const DataStream: TOle2File); override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);override;
    function TotalSize: int64; override;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;override;

    procedure InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean); override;
    procedure DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);override;

    function CheckCell(const aRow, aCol: integer; var CellBounds: TXlsCellRange): boolean;

    function MergedCount: integer;
    function MergedCell(const i: integer): TXlsCellRange;

    procedure PreMerge(var R1,C1,R2,C2: integer);
    procedure MergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
    procedure UnMergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
  end;

  ClassOfTRangeEntry = class of TRangeEntry;

implementation

{ TRangeValuesList }

procedure TRangeValuesList.CopyFrom( const RVL: TRangeValuesList);
var
  i: integer;
  R: PExcelRange;
begin
  for i:=0 to RVL.Count-1 do
  begin
    New(R);
    try
      R^:=PExcelRange(RVL[i])^;
      Add(R);
    except
      FreeAndNil(R);
      raise;
    end; //except
  end;
end;

constructor TRangeValuesList.Create(const aMaxRangesPerRecord: integer; const aOtherDataLen: word);
begin
  inherited Create;
  MaxRangesPerRecord := aMaxRangesPerRecord;
  FOtherDataLen:= aOtherDataLen;
end;


procedure TRangeValuesList.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer);
var
  i:integer;
begin
  for i:=Count -1 downto 0 do
  begin
    if PExcelRange(Items[i]).R1>= aRowPos then IncMaxMin( PExcelRange(Items[i]).R1, aRowCount, Max_Rows, aRowPos);
    if PExcelRange(Items[i]).R2>= aRowPos then IncMaxMin( PExcelRange(Items[i]).R2, aRowCount, Max_Rows, PExcelRange(Items[i]).R1);

    if PExcelRange(Items[i]).C1>= aColPos then IncMaxMin( PExcelRange(Items[i]).C1, aColCount, Max_Columns, aColPos);
    if PExcelRange(Items[i]).C2>= aColPos then IncMaxMin( PExcelRange(Items[i]).C2, aColCount, Max_Columns, PExcelRange(Items[i]).C1);
  end;
end;

procedure TRangeValuesList.CopyIntersectRange(const R, Rx: PExcelRange; const NewFirstRow, NewLastRow, DestRow, aCount: integer; var MinR1, MaxR2: Word);
var
  NewRange, NewRangex: PExcelRange;
  k, Lc: integer;
begin
  Lc:=(NewLastRow-NewFirstRow+1)* aCount;

  if (Rx.R1<=NewFirstRow) and (Rx.R2>=NewLastRow) then // Just copy one big range
  begin
    New(NewRange);
    try
      NewRangex:=PExcelRange(PAddress(NewRange)+(PAddress(Rx)-PAddress(R)));
      NewRange^:=R^;
      NewRangex.R1:=DestRow;
      NewRangex.R2:=DestRow+Lc-1;
      Add(NewRange);
      if NewRangex.R1< MinR1 then MinR1:=NewRangex.R1;
      if NewRangex.R2> MaxR2 then MaxR2:=NewRangex.R2;
    except
      Dispose(NewRange);
      raise;
    end; //Except
  end else // We have to copy one small range for each aCount
  begin
    for k:=0 to aCount -1 do
    begin
      New(NewRange);
      try
        NewRangex:=PExcelRange(PAddress(NewRange)+(PAddress(Rx)-PAddress(R)));
        NewRange^:=R^;
        NewRangex.R1:=DestRow+(NewLastRow-NewFirstRow+1)*k;
        if Rx.R1>NewFirstRow then inc(NewRangex.R1, Rx.R1-NewFirstRow);
        NewRangex.R2:=DestRow+(NewLastRow-NewFirstRow+1)*(k+1)-1;
        if Rx.R2<NewLastRow then dec(NewRangex.R2, NewLastRow-Rx.R2);

        Add(NewRange);
        if NewRangex.R1< MinR1 then MinR1:=NewRangex.R1;
        if NewRangex.R2> MaxR2 then MaxR2:=NewRangex.R2;
      except
        Dispose(NewRange);
        raise;
      end; //Except
    end;
  end;
end;

procedure TRangeValuesList.CopyRowsInclusive(const FirstRow, LastRow,
  DestRow, aCount: integer; var MinR1, MaxR2: word; const UseCols: boolean);
var
  i, Lc:integer;
  R, Rx: PExcelRange;
  NewFirstRow, NewLastRow: integer;
begin
  Lc:=(LastRow-FirstRow+1)* aCount;

  if FirstRow<DestRow then NewFirstRow:=FirstRow else NewFirstRow:=FirstRow+ Lc;
  if LastRow<DestRow then NewLastRow:=LastRow else NewLastRow:=LastRow+Lc;

  for i:=0 to Count-1 do
  begin
    R:=PExcelRange(Items[i]);
    if UseCols then Rx:=PExcelRange(PAddress(R)+2*SizeOf(Word)) else Rx:=R;  //when using cols, we fool the record so R1 really means C1. We can't use C1 there.
    if (Rx.R1<= NewLastRow) and
       (Rx.R2>= NewFirstRow) then
    begin
      //First Case, Block copied is above the original

      if (FirstRow>=DestRow) then
        if (Rx.R1<DestRow + Lc) then //nothing, range is automatically expanded
        else if (Rx.R1=DestRow + Lc) and( Rx.R2 >=NewLastRow) then //expand the range to include inserted rows
        begin
          Dec(Rx.R1, Lc);
          if Rx.R1< MinR1 then MinR1:=Rx.R1;
        end
        else CopyIntersectRange(R, Rx, NewFirstRow, NewLastRow, DestRow, aCount, MinR1, MaxR2) //We have to Copy the intersecting range, and clip the results

      //Second Case, Block copied is below the original

      else
        if (Rx.R2>DestRow-1) then //nothing, range is automatically expanded
        else if (Rx.R2=DestRow -1) and (Rx.R1<=NewFirstRow) then //expand the range to include inserted rows
        begin
          Inc(Rx.R2, Lc);
          if Rx.R2> MaxR2 then MaxR2:=Rx.R2;
        end
        else CopyIntersectRange(R, Rx, NewFirstRow, NewLastRow, DestRow, aCount, MinR1, MaxR2); //We have to Copy the intersecting range, and clip the results

    end;
  end;
end;

procedure TRangeValuesList.CopyRowsExclusive(const FirstRow,
  LastRow, DestRow, aCount: integer; const UseCols: boolean);
var
  i, k, Lc:integer;
  R, Rx, NewRange, NewRangex: PExcelRange;
  NewFirstRow, NewLastRow, z: integer;
  xMaxRows: integer;
begin
  Lc:=(LastRow-FirstRow+1)* aCount;

  if FirstRow<DestRow then NewFirstRow:=FirstRow else NewFirstRow:=FirstRow+ Lc;
  if LastRow<DestRow then NewLastRow:=LastRow else NewLastRow:=LastRow+Lc;

  if UseCols then xMaxRows:=Max_Columns else xMaxRows:=Max_Rows;
  for i:=0 to Count-1 do
  begin
    R:=PExcelRange(Items[i]);
    if UseCols then Rx:=PExcelRange(PAddress(R)+2*SizeOf(Word)) else Rx:=R;  //when using cols, we fool the record so R1 really means C1. We can't use C1 there.
    if (Rx.R1>= NewFirstRow) and
       (Rx.R2<= NewLastRow) then

      for k:=0 to aCount-1 do
      begin
        New(NewRange);
        try
          NewRangex:=PExcelRange(PAddress(NewRange)+(PAddress(Rx)-PAddress(R)));
          NewRange^:=R^;
          if (FirstRow>=DestRow) then z:=k+1 else z:=-k;

          IncMax(NewRangex.R1, DestRow - FirstRow -(LastRow-FirstRow+1)*z, xMaxRows);
          IncMax(NewRangex.R2, DestRow - FirstRow -(LastRow-FirstRow+1)*z, xMaxRows);
          Add(NewRange);
        except
          Dispose(NewRange);
          raise;
        end; //Except
      end;
  end;
end;

procedure TRangeValuesList.DeleteRows(const aRow, aCount: integer; const Allow1Cell: boolean; const UseCols: boolean);
var
  i:integer;
  R:PExcelRange;
  ColsEqual: boolean;
begin
  for i:=Count-1 downto 0 do
  begin
    if UseCols then R:=PExcelRange(PAddress(Items[i])+2*SizeOf(Word)) else R:=PExcelRange(Items[i]);  //when using cols, we fool the record so R1 really means C1. We can't use C1 there.
    if UseCols then ColsEqual:=PExcelRange(Items[i]).R1=PExcelRange(Items[i]).R2 else ColsEqual:=(R.C1=R.C2);
    if (R.R1>= aRow) and
      ((R.R2< aRow+aCount) or (not Allow1Cell and (R.R2=aRow+aCount) and ColsEqual)) then
        Delete(i);
  end;
end;

type
  //Just to avoid including windows.pas on d5
  TRect1 = packed record
    Left, Top, Right, Bottom: Longint;
  end;

procedure TRangeValuesList.PreAddNewRange(var R1,C1,R2,C2: integer);
var
  i: integer;
  OutRect: TRect1;
  R: PExcelRange;
begin
  //Check ranges are valid
  if (R1<0) or (R2<R1) or (R2>Max_Rows) or
     (C1<0) or (C2<C1) or (C2>Max_Columns) then exit;

  if (R1=R2)and(C1=C2) then exit;

  for i:=Count-1 downto 0 do
  begin
    R:=PExcelRange(Items[i]);
    OutRect.Left:=Max(R.C1, C1);
    OutRect.Top:=Max(R.R1, R1);
    OutRect.Right:=Min(R.C2, C2);
    OutRect.Bottom:=Min(R.R2, R2);
    if (OutRect.Left<=OutRect.Right)and(OutRect.Top<=OutRect.Bottom) then //found
    begin
      R1:=Min(R.R1, R1);
      R2:=Max(R.R2, R2);
      C1:=Min(R.C1, C1);
      C2:=Max(R.C2, C2);
      Delete(i);
    end;
  end;

end;

//We always have to call PreAddNewRange to verify it doesn't exist
procedure TRangeValuesList.AddNewRange(const FirstRow, FirstCol, LastRow, LastCol: integer);
var
  NewRange: PExcelRange;
begin
  //Check ranges are valid
  if (FirstRow<0) or (LastRow<FirstRow) or (LastRow>Max_Rows) or
     (FirstCol<0) or (LastCol<FirstCol) or (LastCol>Max_Columns) then exit;

  if (FirstRow=LastRow)and(FirstCol=LastCol) then exit;

  New(NewRange);
  try
    NewRange.R1:=FirstRow;
    NewRange.R2:=LastRow;
    NewRange.C1:=FirstCol;
    NewRange.C2:=LastCol;
    add(NewRange);
  except
    Dispose(NewRange);
    raise;
  end; //Except
end;


procedure TRangeValuesList.Load(const aRecord: TBaseRecord; const aPos: integer);
var
  i: integer;
  n: word;
  MyPos: integer;
  MyRecord: TBaseRecord;
  ExcelRange: PExcelRange;
begin
  MyPos:= aPos;
  MyRecord:= aRecord;
  ReadMem(MyRecord, MyPos, SizeOf(n), @n);
  for i:=0 to n-1 do
  begin
    New(ExcelRange);
    try
      ReadMem(MyRecord, MyPos, SizeOf(TExcelRange), ExcelRange);
      Add(ExcelRange);
      ExcelRange:=nil;
    finally
      Dispose(ExcelRange);
    end; //finally
  end;
end;

procedure TRangeValuesList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then Dispose(PExcelRange(Ptr));
  inherited Notify(Ptr, Action);
end;


//------------ methods with "R" at the end add new records and don't use continue ---------------//
function TRangeValuesList.RepeatCountR(const aCount: integer): integer;
const
  Rl = SizeOf(TExcelRange);
var
  OneRecCount: integer;
begin
  OneRecCount := MaxRangesPerRecord;
  if aCount>0 then Result:= (aCount-1) div OneRecCount +1 else Result:=1;
end;

procedure TRangeValuesList.SaveToStreamR(const DataStream: TOle2File; const Line: integer);
const
  Rl = SizeOf(TExcelRange);
var
  OneRecCount, i: integer;
  myCount: word;
begin
  OneRecCount := MaxRangesPerRecord ;
  if (Line+1)*OneRecCount >Count then MyCount:=Count-Line*OneRecCount else MyCount:=OneRecCount;
  DataStream.WriteMem(MyCount, SizeOf(MyCount));
  for i:=Line*OneRecCount to Line*OneRecCount+myCount-1 do DataStream.WriteMem(PExcelRange(Items[i])^, Rl);
end;

function TRangeValuesList.NextInRange(const Range: TXlsCellRange; const k: integer): integer;
var
  i: integer;
begin
  Result:=-1;
  for i:=k+1 to Count-1 do
    if (Range.Top<= PExcelRange(Items[i]).R1 ) and
       (Range.Bottom>= PExcelRange(Items[i]).R2 ) and
       (Range.Left<= PExcelRange(Items[i]).C1 ) and
       (Range.Right>= PExcelRange(Items[i]).C2 )  then
       begin
         Result:=i;
         exit;
       end;

end;

procedure TRangeValuesList.SaveRangeToStreamR(const DataStream: TOle2File; const Line: integer; const aCount: integer; const Range: TXlsCellRange);
const
  Rl = SizeOf(TExcelRange);
var
  OneRecCount, i, k: integer;
  myCount: word;
begin
  OneRecCount := MaxRangesPerRecord ;
  if (Line+1)*OneRecCount >aCount then MyCount:=aCount-Line*OneRecCount else MyCount:=OneRecCount;
  DataStream.WriteMem(MyCount, SizeOf(MyCount));
  k:=NextInRange(Range, -1);
  for i:=Line*OneRecCount to Line*OneRecCount+myCount-1 do
  begin
    DataStream.WriteMem(PExcelRange(Items[k])^, Rl);
    k:=NextInRange(Range, k);
  end;
end;

function TRangeValuesList.TotalSizeR(const aCount:integer): int64;
const
  Rl = SizeOf(TExcelRange);
begin
  Result := (SizeOf(TRecordHeader)+ 2+ FOtherDataLen)* RepeatCountR(aCount)    //Base data
            + Rl*aCount;                               // Registers
end;

function TRangeValuesList.RecordSizeR(const Line: integer; const aCount:integer): integer;
const
  Rl = SizeOf(TExcelRange);
var
  OneRecCount, MyCount: integer;
begin
  OneRecCount := MaxRangesPerRecord;
  if (Line+1)*OneRecCount >aCount then MyCount:=aCount-Line*OneRecCount else MyCount:=OneRecCount;
  Result:= 2+ FOtherDataLen+MyCount*Rl;
end;

function TRangeValuesList.CountRangeRecords(const Range: TXlsCellRange): integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if (Range.Top<= PExcelRange(Items[i]).R1 ) and
       (Range.Bottom>= PExcelRange(Items[i]).R2 ) and
       (Range.Left<= PExcelRange(Items[i]).C1 ) and
       (Range.Right>= PExcelRange(Items[i]).C2 )  then Inc(Result);
end;

{ TRangeEntry }

function TRangeEntry.CopyTo: TRangeEntry;
begin
  if Self=nil then Result:= nil   //for this to work, this cant be a virtual method
  else Result:=DoCopyTo;
end;

constructor TRangeEntry.Create;
begin
  inherited;
end;

destructor TRangeEntry.Destroy;
begin
  FreeAndNil(RangeValuesList);
  inherited;
end;

function TRangeEntry.DoCopyTo: TRangeEntry;
begin
  Result:= ClassOfTRangeEntry(ClassType).Create;
  Result.RangeValuesList.CopyFrom(RangeValuesList);
end;

procedure TRangeEntry.DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);
begin
  if UseCols then
    ArrangeInsertRowsAndCols(0, 0, aRow, -aCount, SheetInfo)
  else
    ArrangeInsertRowsAndCols(aRow, -aCount, 0, 0, SheetInfo);
end;


procedure TRangeEntry.InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean);
begin
  if UseCols then
    ArrangeInsertRowsAndCols(0,0,DestRow, (LastRow-FirstRow+1)* aCount, SheetInfo)
  else
    ArrangeInsertRowsAndCols(DestRow, (LastRow-FirstRow+1)* aCount,0,0, SheetInfo);
end;


procedure TRangeEntry.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
begin
  RangeValuesList.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount);
end;

{ TMergedCells }

function TMergedCells.CheckCell(const aRow, aCol: integer; var CellBounds: TXlsCellRange): boolean;
var
  i: integer;
begin
  Result:=false;
  for i:=0 to RangeValuesList.Count-1 do
    if (PExcelRange(RangeValuesList[i]).R1<=aRow) and
       (PExcelRange(RangeValuesList[i]).R2>=aRow) and
       (PExcelRange(RangeValuesList[i]).C1<=aCol) and
       (PExcelRange(RangeValuesList[i]).C2>=aCol) then
       begin
         CellBounds.Left:= PExcelRange(RangeValuesList[i]).C1;
         CellBounds.Top:= PExcelRange(RangeValuesList[i]).R1;
         CellBounds.Right:= PExcelRange(RangeValuesList[i]).C2;
         CellBounds.Bottom:= PExcelRange(RangeValuesList[i]).R2;
         Result:=true;
         exit;
       end;

end;

procedure TMergedCells.Clear;
begin
  if RangeValuesList<>nil then RangeValuesList.Clear;
end;

constructor TMergedCells.Create;
begin
  inherited;
  RangeValuesList:= TRangeValuesList.Create(513, 0);
end;

procedure TMergedCells.DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);
begin
  RangeValuesList.DeleteRows(aRow, aCount, false, UseCols);
  inherited;
end;

procedure TMergedCells.InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean);
begin
  inherited;
  RangeValuesList.CopyRowsExclusive(FirstRow, LastRow, DestRow, aCount, UseCols);
end;

procedure TMergedCells.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TRangeRecord);
var
  aPos: integer;
begin
  Clear;
  aPos:=0;
  RangeValuesList.Load(First, aPos);

  First.Free;
end;

procedure TMergedCells.UnMergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
var
  i: integer;
begin
  for i:=RangeValuesList.Count-1 downto 0 do
    if (PExcelRange(RangeValuesList[i]).R1=FirstRow) and
       (PExcelRange(RangeValuesList[i]).R2=LastRow) and
       (PExcelRange(RangeValuesList[i]).C1=FirstCol) and
       (PExcelRange(RangeValuesList[i]).C2=LastCol) then
       begin
         RangeValuesList.Delete(i);
       end;
end;

//Always call premergecell first...
procedure TMergedCells.MergeCells(const FirstRow, FirstCol, LastRow, LastCol: integer);
begin
  RangeValuesList.AddNewRange(FirstRow, FirstCol, LastRow, LastCol);
end;

procedure TMergedCells.PreMerge(var R1, C1, R2, C2: integer);
begin
  RangeValuesList.PreAddNewRange(R1, C1, R2, C2);
end;

procedure TMergedCells.SaveRangeToStream(const DataStream: TOle2File;
  const CellRange: TXlsCellRange);
var
  RecordHeader: TRecordHeader;
  i: integer;
  Rc: integer;
begin
  Rc:=RangeValuesList.CountRangeRecords(CellRange);
  if Rc=0 then exit; //don't save empty MergedCells
  RecordHeader.Id:= xlr_CELLMERGING;
  for i:=0 to RangeValuesList.RepeatCountR(Rc)-1 do
  begin
    RecordHeader.Size:=RangeValuesList.RecordSizeR(i,Rc);
    DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
    RangeValuesList.SaveRangeToStreamR(DataStream, i, Rc, CellRange);
  end;
end;

procedure TMergedCells.SaveToStream(const DataStream: TOle2File);
var
  RecordHeader: TRecordHeader;
  i: integer;
begin
  if RangeValuesList.Count=0 then exit; //don't save empty MergedCells
  RecordHeader.Id:= xlr_CELLMERGING;
  for i:=0 to RangeValuesList.RepeatCountR(RangeValuesList.Count)-1 do
  begin
    RecordHeader.Size:=RangeValuesList.RecordSizeR(i, RangeValuesList.Count);
    DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
    RangeValuesList.SaveToStreamR(DataStream, i);
  end;
end;

function TMergedCells.TotalRangeSize(const CellRange: TXlsCellRange): int64;
begin
  if RangeValuesList.Count=0 then Result:=0 else Result:= RangeValuesList.TotalSizeR(RangeValuesList.CountRangeRecords(CellRange)) ;
end;

function TMergedCells.TotalSize: int64;
begin
  if RangeValuesList.Count=0 then TotalSize:=0 else TotalSize:= RangeValuesList.TotalSizeR(RangeValuesList.Count);
end;

function TMergedCells.MergedCount: integer;
begin
  Result:=RangeValuesList.Count;
end;

function TMergedCells.MergedCell(const i: integer): TXlsCellRange;
begin
  Result.Left:=PExcelRange(RangeValuesList[i]).C1;
  Result.Top:=PExcelRange(RangeValuesList[i]).R1;
  Result.Right:=PExcelRange(RangeValuesList[i]).C2;
  Result.Bottom:=PExcelRange(RangeValuesList[i]).R2;
end;

end.
