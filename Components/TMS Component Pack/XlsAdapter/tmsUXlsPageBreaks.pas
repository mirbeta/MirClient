unit tmsUXlsPageBreaks;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUXlsBaseRecords, tmsUXlsBaseList, tmsXlsMessages,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     Classes, SysUtils, tmsUFlxMessages, tmsUOle2Impl;

type
  THBreakData= packed record
    Row: Word;
    Col1: Word;
    Col2: Word;
  end;

  TVBreakData= packed record
    Col: Word;
    Row1: Word;
    Row2: Word;
  end;

  THPageBreakRecord = class (TBaseRecord)
  public
    function Count: Word;
    function BreakData(const index: integer): THBreakData;
  end;

  TVPageBreakRecord = class (TBaseRecord)
  public
    function Count: Word;
    function BreakData(const index: integer): TVBreakData;
  end;

  THPageBreak=class
  public
    BreakData: THBreakData;

    constructor Create(const aBreakData: THBreakData);
    function CopyTo: THPageBreak;
  end;

  TVPageBreak=class
  public
    BreakData: TVBreakData;

    constructor Create(const aBreakData: TVBreakData);
    function CopyTo: TVPageBreak;
  end;

  THPageBreakList= class (TBaseList)
  private
    procedure SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
    procedure CalcIncludedRangeRecords(const CellRange: TXlsCellRange; out FirstRecord, RecordCount: integer);
    function TotalSizeExt(const RecordCount:integer): int64;
  {$INCLUDE THPageBreakListHdr.inc}
  public
    procedure AddRecord(const aRecord: THPageBreakRecord);
    procedure AddBreak(const aRow: Longint);
    procedure DeleteBreak(const aRow: Integer);
    procedure CopyFrom(const aBreakList: THPageBreakList);
    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure InsertRows(const DestRow: integer; const aCount: integer);
    procedure DeleteRows(const DestRow: integer; const aCount: integer);

    function HasPageBreak( const Row: integer): boolean;
  end;

  TVPageBreakList= class (TBaseList)
  private
    procedure SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
    procedure CalcIncludedRangeRecords(const CellRange: TXlsCellRange; out FirstRecord, RecordCount: integer);
    function TotalSizeExt(const RecordCount:integer): int64;
  {$INCLUDE TVPageBreakListHdr.inc}
  public
    procedure AddRecord(const aRecord: TVPageBreakRecord);
    procedure AddBreak(const aCol: Longint);
    procedure DeleteBreak(const aCol: Integer);
    procedure CopyFrom(const aBreakList: TVPageBreakList);
    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure InsertCols(const DestCol: integer; const aCount: integer);
    procedure DeleteCols(const DestCol: integer; const aCount: integer);

    function HasPageBreak( const Col: integer): boolean;
  end;

implementation
{$INCLUDE THPageBreakListImp.inc}
{$INCLUDE TVPageBreakListImp.inc}

{ THPageBreakRecord }

function THPageBreakRecord.BreakData(const index: integer): THBreakData;
begin
  Move(Data[2+index*SizeOf(THBreakData)],Result, SizeOf(THBreakData));
end;

function THPageBreakRecord.Count: Word;
begin
  Result:= GetWord(Data, 0);
end;

{ THPageBreak }

function THPageBreak.CopyTo: THPageBreak;
begin
  Result:= THPageBreak.Create(BreakData);
end;

constructor THPageBreak.Create(const aBreakData: THBreakData);
begin
  inherited Create;
  BreakData:= aBreakData;
end;

{ THPageBreakList }

procedure THPageBreakList.AddBreak(const aRow: Integer);
var
  Index: integer;
  BreakData: THBreakData;
begin
  BreakData.Row:=aRow;
  BreakData.Col1:=0;
  BreakData.Col2:=$FF;
  if Count>MaxHPageBreaks then raise Exception.Create(ErrTooManyPageBreaks);
  if not Find(aRow, Index) then Insert(Index, THPageBreak.Create(BreakData));
end;

procedure THPageBreakList.DeleteBreak(const aRow: Integer);
var
  Index: integer;
begin
  if Find(aRow, Index) then Delete(Index);
end;

procedure THPageBreakList.AddRecord(const aRecord: THPageBreakRecord);
var
  i, Index: integer;
begin
  for i:=0 to aRecord.Count - 1 do
    if not Find(aRecord.BreakData(i).Row, Index) then Insert(Index, THPageBreak.Create(aRecord.BreakData(i)));
  aRecord.Free;
end;

procedure THPageBreakList.CalcIncludedRangeRecords(const CellRange: TXlsCellRange; out FirstRecord, RecordCount: integer);
var
  LastRecord, i: integer;
begin
  Sort; //just in case
  FirstRecord:=-1;
  LastRecord:=-1;
  for i:=0 to Count-1 do
  begin
    if (FirstRecord<0) and (Items[i].BreakData.Row>=CellRange.Top) then FirstRecord:=i;
    if Items[i].BreakData.Row<=CellRange.Bottom then LastRecord:=i;
  end;
  if (FirstRecord>=0) and (LastRecord>=0) and (FirstRecord<=LastRecord) then
    RecordCount:=LastRecord-FirstRecord+1
  else
  begin
    FirstRecord:=0;
    RecordCount:=0;
  end;
end;

procedure THPageBreakList.CopyFrom(const aBreakList: THPageBreakList);
var
  i:integer;
begin
  if aBreakList=nil then exit;
  for i:=0 to aBreakList.Count-1 do Add(aBreakList[i].CopyTo);
end;


procedure THPageBreakList.DeleteRows(const DestRow, aCount: integer);
var
  Index: integer;
  i: integer;
begin
  Find(DestRow, Index);
  for i:=Count-1 downto Index do
    if Items[i].BreakData.Row<DestRow+aCount then Delete(i) else dec(Items[i].BreakData.Row, aCount);
end;

procedure THPageBreakList.InsertRows(const DestRow, aCount: integer);
var
  Index: integer;
  i: integer;
begin
  Find(DestRow, Index);
  for i:=Index to Count-1 do IncMax(Items[i].BreakData.Row, aCount, $FFFF);
end;

procedure THPageBreakList.SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
var
  RecordHeader: TRecordHeader;
  MyRecordCount: word;
  i: integer;
begin
  if RecordCount > 0 then
  begin
    Sort; //just in case...
    MyRecordCount:=RecordCount;
    RecordHeader.Id:= xlr_HORIZONTALPAGEBREAKS;
    RecordHeader.Size:= SizeOf(MyRecordCount)+RecordCount*SizeOf(THBreakData);

    DataStream.WriteMem(RecordHeader, Sizeof(RecordHeader));
    DataStream.WriteMem(MyRecordCount, Sizeof(MyRecordCount));
    for i:=FirstRecord to FirstRecord+RecordCount-1 do
    begin
      DataStream.WriteMem(Items[i].BreakData, SizeOf(Items[i].BreakData));
    end;
  end;
end;

procedure THPageBreakList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  SaveToStreamExt(DataStream, FirstRecord, RecordCount);
end;

procedure THPageBreakList.SaveToStream(const DataStream: TOle2File);
begin
  SaveToStreamExt(DataStream, 0, Count);
end;

function THPageBreakList.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  Result:=TotalSizeExt(RecordCount);
end;

function THPageBreakList.TotalSize: int64;
begin
  Result:=TotalSizeExt(Count);
end;

function THPageBreakList.TotalSizeExt(const RecordCount: integer): int64;
begin
  if RecordCount=0 then Result:=0
  else Result:=sizeOf(TRecordHeader)+SizeOf(Word)+RecordCount*SizeOf(THBreakData);
end;

function THPageBreakList.HasPageBreak(const Row: integer): boolean;
var
  Index: integer;
begin
  Result:=Find(Row, Index);
end;

//---------------------------- VERTICAL PAGE BREAKS ----------------------//
//------------------------------------------------------------------------//

{ TVPageBreakRecord }

function TVPageBreakRecord.BreakData(const index: integer): TVBreakData;
begin
  Move(Data[2+index*SizeOf(TVBreakData)],Result, SizeOf(TVBreakData));
end;

function TVPageBreakRecord.Count: Word;
begin
  Result:= GetWord(Data, 0);
end;

{ TVPageBreak }

function TVPageBreak.CopyTo: TVPageBreak;
begin
  Result:= TVPageBreak.Create(BreakData);
end;

constructor TVPageBreak.Create(const aBreakData: TVBreakData);
begin
  inherited Create;
  BreakData:= aBreakData;
end;

{ TVPageBreakList }

procedure TVPageBreakList.AddBreak(const aCol: Integer);
var
  Index: integer;
  BreakData: TVBreakData;
begin
  BreakData.Col:=aCol;
  BreakData.Row1:=0;
  BreakData.Row2:=$FFFF;
  if Count>MaxVPageBreaks then raise Exception.Create(ErrTooManyPageBreaks);
  if not Find(aCol, Index) then Insert(Index, TVPageBreak.Create(BreakData));
end;

procedure TVPageBreakList.DeleteBreak(const aCol: Integer);
var
  Index: integer;
begin
  if Find(aCol, Index) then Delete(Index);
end;

procedure TVPageBreakList.AddRecord(const aRecord: TVPageBreakRecord);
var
  i, Index: integer;
begin
  for i:=0 to aRecord.Count - 1 do
    if not Find(aRecord.BreakData(i).Col, Index) then Insert(Index, TVPageBreak.Create(aRecord.BreakData(i)));
  aRecord.Free;
end;

procedure TVPageBreakList.CalcIncludedRangeRecords(const CellRange: TXlsCellRange; out FirstRecord, RecordCount: integer);
var
  LastRecord, i: integer;
begin
  Sort; //just in case
  FirstRecord:=-1;
  LastRecord:=-1;
  for i:=0 to Count-1 do
  begin
    if (FirstRecord<0) and (Items[i].BreakData.Col>=CellRange.Left) then FirstRecord:=i;
    if Items[i].BreakData.Col<=CellRange.Right then LastRecord:=i;
  end;
  if (FirstRecord>=0) and (LastRecord>=0) and (FirstRecord<=LastRecord) then
    RecordCount:=LastRecord-FirstRecord+1
  else
  begin
    FirstRecord:=0;
    RecordCount:=0;
  end;
end;

procedure TVPageBreakList.CopyFrom(const aBreakList: TVPageBreakList);
var
  i:integer;
begin
  if aBreakList=nil then exit;
  for i:=0 to aBreakList.Count-1 do Add(aBreakList[i].CopyTo);
end;


procedure TVPageBreakList.DeleteCols(const DestCol, aCount: integer);
var
  Index: integer;
  i: integer;
begin
  Find(DestCol, Index);
  for i:=Count-1 downto Index do
    if Items[i].BreakData.Col<DestCol+aCount then Delete(i) else dec(Items[i].BreakData.Col, aCount);
end;

procedure TVPageBreakList.InsertCols(const DestCol, aCount: integer);
var
  Index: integer;
  i: integer;
begin
  Find(DestCol, Index);
  for i:=Index to Count-1 do IncMax(Items[i].BreakData.Col, aCount, $FF);
end;

procedure TVPageBreakList.SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
var
  RecordHeader: TRecordHeader;
  MyRecordCount: word;
  i: integer;
begin
  if RecordCount > 0 then
  begin
    Sort; //just in case...
    MyRecordCount:=RecordCount;
    RecordHeader.Id:= xlr_VERTICALPAGEBREAKS;
    RecordHeader.Size:= SizeOf(MyRecordCount)+RecordCount*SizeOf(TVBreakData);

    DataStream.WriteMem(RecordHeader, Sizeof(RecordHeader));
    DataStream.WriteMem(MyRecordCount, Sizeof(MyRecordCount));
    for i:=FirstRecord to FirstRecord+RecordCount-1 do
    begin
      DataStream.WriteMem(Items[i].BreakData, SizeOf(Items[i].BreakData));
    end;
  end;
end;

procedure TVPageBreakList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  SaveToStreamExt(DataStream, FirstRecord, RecordCount);
end;

procedure TVPageBreakList.SaveToStream(const DataStream: TOle2File);
begin
  SaveToStreamExt(DataStream, 0, Count);
end;

function TVPageBreakList.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  Result:=TotalSizeExt(RecordCount);
end;

function TVPageBreakList.TotalSize: int64;
begin
  Result:=TotalSizeExt(Count);
end;

function TVPageBreakList.TotalSizeExt(const RecordCount: integer): int64;
begin
  if RecordCount=0 then Result:=0
  else Result:=sizeOf(TRecordHeader)+SizeOf(Word)+RecordCount*SizeOf(TVBreakData);
end;

function TVPageBreakList.HasPageBreak(const Col: integer): boolean;
var
  Index: integer;
begin
  Result:=Find(Col, Index);
end;




end.
