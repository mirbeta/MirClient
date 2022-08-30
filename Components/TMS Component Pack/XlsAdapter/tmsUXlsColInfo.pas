unit tmsUXlsColInfo;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsUXlsBaseList, tmsXlsMessages, tmsUFlxMessages, tmsUXlsOtherRecords, tmsUOle2Impl;

type
  TColInfoDat=packed record
    FirstColumn: word;
    LastColumn: word;
    Width: word;
    XF: word;
    Options: word;
    Reserved: Word;
  end;
  PColInfoDat=^TColInfoDat;

  TColInfo=class
  public
    Column: word;
    Width: Word;
    XF: Word;
    Options: Word;

    constructor Create (const aColumn, aWidth, aXF, aOptions: word);

    function IsEqual(const aColInfo: TColInfo): boolean;

    procedure SetColOutlineLevel(Level: integer);
    function GetColOutlineLevel: integer;
  end;

  TColInfoRecord=class(TBaseRecord)
    function D: TColInfoDat;
  end;

  TColInfoList= class(TBaseList)  //Items are TColInfo
  {$INCLUDE TColInfoListHdr.inc}
  private
    procedure SaveOneRecord(const i, k: integer; const DataStream: TOle2File);
    procedure SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
    procedure CalcIncludedRangeRecords(const CellRange: TXlsCellRange; var FirstRecord, RecordCount: integer);
    function TotalSizeExt(const FirstRecord, RecordCount:integer): int64;
  public
    procedure CopyFrom(const aColInfoList: TColInfoList);

    procedure AddRecord(const R: TColInfoRecord);
    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
    function TotalSize: int64;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;

    procedure ArrangeInsertCols(const DestCol: integer; const aColCount: integer; const SheetInfo: TSheetInfo);
    procedure CopyCols(const FirstCol, LastCol: integer;  const DestCol: integer; const aColCount: integer; const SheetInfo: TSheetInfo);

    procedure CalcGuts(const Guts: TGutsRecord);
  end;

implementation
{$INCLUDE TColInfoListImp.inc}

{ TColInfoList }

procedure TColInfoList.AddRecord(const R: TColInfoRecord);
var
  i: integer;
begin
  for i:=R.D.FirstColumn to R.D.LastColumn do
    Add(TColInfo.Create(i, R.D.Width, R.D.XF, R.D.Options ));
  R.Free;
end;

procedure TColInfoList.CalcIncludedRangeRecords(
  const CellRange: TXlsCellRange; var FirstRecord, RecordCount: integer);
var
  LastRecord, i: integer;
begin
  Sort; //just in case...
  FirstRecord:=-1;
  LastRecord:=-1;
  for i:=0 to Count-1 do
  begin
    if (FirstRecord<0) and (Items[i].Column>=CellRange.Left) then FirstRecord:=i;
    if Items[i].Column<=CellRange.Right then LastRecord:=i;
  end;
  if (FirstRecord>=0) and (LastRecord>=0) and (FirstRecord<=LastRecord) then
    RecordCount:=LastRecord-FirstRecord+1
  else
  begin
    FirstRecord:=0;
    RecordCount:=0;
  end;
end;

procedure TColInfoList.CopyFrom(const aColInfoList: TColInfoList);
var
  i: integer;
begin
  Clear;
  for i:=0 to aColInfoList.Count-1 do Add(TColInfo.Create(aColInfoList[i].Column, aColInfoList[i].Width, aColInfoList[i].XF, aColInfoList[i].Options));
end;

procedure TColInfoList.SaveOneRecord(const i,k: integer; const DataStream: TOle2File);
var
  RecordHeader: TRecordHeader;
  Info: TColInfoDat;
begin
  RecordHeader.Id:= xlr_COLINFO;
  RecordHeader.Size:=SizeOf(TColInfoDat);
  DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
  Info.FirstColumn:=Items[i].Column;
  Info.LastColumn:=Items[k].Column;

  if Info.LastColumn > Max_Columns + 1 then Info.LastColumn := Max_Columns + 1; //LastColumn must not be bigger than 256.

  Info.Width:=Items[i].Width;
  Info.XF:=Items[i].XF;
  Info.Options:=Items[i].Options;
  Info.Reserved:=0;
  DataStream.WriteMem(Info, SizeOf(Info));
end;

procedure TColInfoList.SaveToStreamExt(const DataStream: TOle2File; const FirstRecord, RecordCount: integer);
var
  i,k: integer;
begin
  //Mix similar columns
  Sort;
  i:=FirstRecord;
  while i<RecordCount do
  begin
    k:=i+1;
    while (k<FirstRecord+RecordCount) and Items[i].IsEqual(Items[k]) and (Items[k].Column=Items[k-1].Column+1) do inc(k);

   	//We need to ensure this[i] is not bigger than Maxcolumns. this[k] can be=Maxcolumns+1.
    if Items[i].Column > Max_Columns then exit;

    SaveOneRecord(i, k-1,DataStream);
    i:=k;
  end;
end;

procedure TColInfoList.SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  SaveToStreamExt(DataStream, FirstRecord, RecordCount);
end;

procedure TColInfoList.SaveToStream(const DataStream: TOle2File);
begin
  SaveToStreamExt(DataStream, 0, Count);
end;

function TColInfoList.TotalSize: int64;
var
  i,k: integer;
begin
  Sort; //just in case

  Result:=0;
  //Mix similar columns
  i:=0;
  while i<Count do
  begin
    k:=i+1;
    while (k<Count) and Items[i].IsEqual(Items[k]) and (Items[k].Column=Items[k-1].Column+1) do inc(k);
   	//We need to ensure this[i] is not bigger than Maxcolumns. this[k] can be=Maxcolumns+1.
    if Items[i].Column > Max_Columns then exit;
    inc(Result, SizeOf(TRecordHeader)+SizeOf(TColInfoDat));
    i:=k;
  end;
end;

function TColInfoList.TotalSizeExt(const FirstRecord, RecordCount: integer): int64;
var
  i,k: integer;
begin
  Sort; //just in case
  Result:=0;
  //Mix similar columns
  i:=FirstRecord;
  while i<FirstRecord+RecordCount do
  begin
    k:=i+1;
    while (k<Count) and Items[i].IsEqual(Items[k])and (Items[k].Column=Items[k-1].Column+1) do inc(k);
   	//We need to ensure this[i] is not bigger than Maxcolumns. this[k] can be=Maxcolumns+1.
    if Items[i].Column > Max_Columns then exit;
    inc(Result, SizeOf(TRecordHeader)+SizeOf(TColInfoDat));
    i:=k;
  end;
end;

function TColInfoList.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  FirstRecord, RecordCount: integer;
begin
  CalcIncludedRangeRecords(CellRange, FirstRecord, RecordCount);
  Result:=TotalSizeExt(FirstRecord, RecordCount);
end;

procedure TColInfoList.CalcGuts(const Guts: TGutsRecord);
var
  MaxGutsLevel: integer;
  GutsLevel: integer;
  i: integer;
begin
  MaxGutsLevel:=0;
  for i:=0 to Count-1 do
  begin
    if (Items[i]<>nil) then
    begin
      GutsLevel:=items[i].GetColOutlineLevel;
      if GutsLevel>MaxGutsLevel then MaxGutsLevel:=GutsLevel;
    end;
  end;
  Guts.ColLevel:=MaxGutsLevel;
end;

procedure TColInfoList.ArrangeInsertCols(const DestCol,
  aColCount: integer; const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  if SheetInfo.FormulaSheet <> SheetInfo.InsSheet then exit;
  Sort; //just in case
  for i := Count - 1 downto 0 do
  begin
    if (Items[i].Column >= DestCol) then inc (Items[i].Column, aColCount)
    else break;
  end;
end;


procedure TColInfoList.CopyCols(const FirstCol, LastCol, DestCol,
  aColCount: integer; const SheetInfo: TSheetInfo);
var
  k: integer;
  i: integer;
  C: TColInfo;
  Index: integer;
  NewCol: integer;
begin
  //ArrangeInsertCols(SourceRange.Offset(SourceRange.Top, DestCol), aColCount, SheetInfo);  //This has already been called.
  for k := 0 to aColCount - 1 do
  begin
    i := 0;
    while i < Count do
    begin
      try
        C := Items[i];
        if (C = nil) then continue;
        if (C.Column < FirstCol) then continue;
        if (C.Column > LastCol) then break;
        NewCol := ((DestCol + C.Column) - FirstCol) + (k * (LastCol - FirstCol + 1));
        if ((NewCol >= 0)) and (NewCol <> C.Column) and (NewCol <= (Max_Columns + 1)) then
        begin
          Index := -1;
          if Find(NewCol, Index) then Delete(Index) else if Index <= i then inc(i);
          Insert(Index, TColInfo.Create(NewCol, C.Width, C.XF, C.Options));
        end;
      finally
        inc(i);
      end;
    end;
  end;
end;

{ TColInfoRecord }

function TColInfoRecord.D: TColInfoDat;
begin
  Result:= PColInfoDat(Data)^;
end;

{ TColInfo }

constructor TColInfo.Create(const aColumn, aWidth, aXF, aOptions: word);
begin
  inherited Create;
  Column:=aColumn;
  Width:=aWidth;
  XF:=aXF;
  Options:=aOptions;
end;

function TColInfo.GetColOutlineLevel: integer;
begin
  Result:=hi(word(Options)) and 7;
end;

function TColInfo.IsEqual(const aColInfo: TColInfo): boolean;
begin
  Result:= // don't compare the column .... (Column = aColInfo.Column) and
           (Width  = aColInfo.Width)  and
           (XF     = aColInfo.XF)     and
           (Options= acolInfo.Options);
end;

procedure TColInfo.SetColOutlineLevel(Level: integer);
begin
  Options:= (Options and not (7 shl 8)) or ((Level and 7) shl 8);
end;

end.
