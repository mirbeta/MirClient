unit tmsUXlsCondFmt;
{$INCLUDE ..\FLXCOMPILER.INC}
interface

uses tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsXlsMessages, Classes, SysUtils, tmsUXlsRangeRecords, tmsUXlsTokenArray, tmsUFlxMessages, tmsUOle2Impl;

type

  TCondFmtRecord= class(TRangeRecord)
  end;

  TCFRecord = class(TBaseRecord)
  private
    CfType, Op: byte;
    Cce1, Cce2: word;

    procedure ArrangeTokensInsertRowsAndCols(const  atPos, fPos, InsRowPos, InsRowOffset, CopyRowOffset,InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);

  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
  end;

  TCFRecordList = class (TBaseRecordList)
    {$INCLUDE TCFRecordListHdr.inc}
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
  end;


  TCondFmt = class (TRangeEntry)
  private
    Flag: word;
    AllRange: TExcelRange;
    CFs: TCFRecordList;
  protected
    function DoCopyTo: TRangeEntry; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TRangeRecord); override;
    procedure SaveToStream(const DataStream: TOle2File); override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const CellRange: TXlsCellRange);override;
    function TotalSize: int64; override;
    function TotalRangeSize(const CellRange: TXlsCellRange): int64;override;

    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);override;
    procedure InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow, aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean); override;
    procedure DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);override;
  end;

implementation
{$INCLUDE TCFRecordListImp.inc}

{ TCondFmt }

procedure TCondFmt.Clear;
begin
  if CFs<>nil then CFs.Clear;
  if RangeValuesList<>nil then RangeValuesList.Clear;
end;

constructor TCondFmt.Create;
begin
  inherited;
  RangeValuesList:= TRangeValuesList.Create(513, 4+SizeOf(TExcelRange));
  CFs:= TCFRecordList.Create;
end;

destructor TCondFmt.Destroy;
begin
  FreeAndNil(CFs);
  inherited;
end;

function TCondFmt.DoCopyTo: TRangeEntry;
begin
  Result:=inherited DoCopyTo;
  (Result as TCondFmt).Flag:=Flag;
  (Result as TCondFmt).AllRange:=AllRange;
  (Result as TCondFmt).CFs.CopyFrom(CFs);
end;

procedure TCondFmt.DeleteRowsOrCols(const aRow, aCount: word; const SheetInfo: TSheetInfo; const UseCols: boolean);
begin
  RangeValuesList.DeleteRows(aRow, aCount, true, UseCols);
  inherited;
end;

procedure TCondFmt.InsertAndCopyRowsOrCols(const FirstRow, LastRow, DestRow,
  aCount: integer; const SheetInfo: TSheetInfo; const UseCols: boolean);
var
  RangeIntersects: boolean;
begin
  if UseCols then
  begin
    RangeIntersects:=(AllRange.C1<= LastRow) and(AllRange.C2>= FirstRow);
    inherited;
    if RangeIntersects then
      RangeValuesList.CopyRowsInclusive( FirstRow, LastRow, DestRow, aCount, AllRange.C1, AllRange.C2, UseCols);
  end else
  begin
    RangeIntersects:=(AllRange.R1<= LastRow) and(AllRange.R2>= FirstRow);
    inherited;
    if RangeIntersects then
      RangeValuesList.CopyRowsInclusive( FirstRow, LastRow, DestRow, aCount, AllRange.R1, AllRange.R2, UseCols);
  end;
end;

procedure TCondFmt.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
begin
  if (AllRange.R2>= aRowPos) or (AllRange.C2>=aColPos) then inherited;

  if (AllRange.R2>= aRowPos) then
  begin
    if AllRange.R1>= aRowPos then IncMaxMin(AllRange.R1, aRowCount, Max_Rows, aRowPos);
    IncMaxMin(AllRange.R2, aRowCount, Max_Rows, AllRange.R1);
  end;

  if (AllRange.C2>= aColPos) then
  begin
    if AllRange.C1>= aColPos then IncMaxMin(AllRange.C1, aColCount, Max_Columns, aColPos);
    IncMaxMin(AllRange.C2, aColCount, Max_Columns, AllRange.C1);
  end;

  CFs.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo );
end;

procedure TCondFmt.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TRangeRecord);
var
  MyRecord: TBaseRecord;
  aPos, CFCount, i: integer;

  R: TBaseRecord;
begin
  Clear;
  MyRecord:= First;

  CFCount:=GetWord(First.Data, 0);
  Flag:=GetWord(First.Data,2);
  aPos:=4;
  ReadMem(MyRecord, aPos, SizeOf(TExcelRange), @AllRange);
  RangeValuesList.Load(First, aPos);

  //Load corresponding CFs
  for i:=0 to CFCount-1 do
  begin
    R:=LoadRecords(DataStream, RecordHeader);
    try
      if not (R is TCFRecord) then raise Exception.Create(ErrInvalidCF);
      CFs.Add(R as TCFRecord);
    except
      FreeAndNil(R);
      raise;
    end; //Except
  end;

  First.Free;  //to be consistent with the other LoadFromStream, we should take ownership of the record if there are no exceptions

end;

procedure TCondFmt.SaveToStream(const DataStream: TOle2File);
var
  RecordHeader: TRecordHeader;
  CFCount: Word;
  i: integer;
begin
  if RangeValuesList.Count=0 then exit; //Don't save empty CF's
  RecordHeader.Id:= xlr_CONDFMT;
  for i:=0 to RangeValuesList.RepeatCountR(RangeValuesList.Count)-1 do
  begin
    RecordHeader.Size:=RangeValuesList.RecordSizeR(i, RangeValuesList.Count);
    DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));

    CFCount:= CFs.Count;
    DataStream.WriteMem(CFCount, SizeOf(CFCount));
    DataStream.WriteMem(Flag, SizeOf(Flag));
    DataStream.WriteMem(AllRange, SizeOf(AllRange));

    RangeValuesList.SaveToStreamR(DataStream, i);
    CFs.SaveToStream(DataStream);
  end;

end;

function TCondFmt.TotalSize: int64;
begin
  if RangeValuesList.Count=0 then TotalSize:=0 else
    TotalSize:=RangeValuesList.TotalSizeR(RangeValuesList.Count) + CFs.TotalSize*RangeValuesList.RepeatCountR(RangeValuesList.Count);
end;

procedure TCondFmt.SaveRangeToStream(const DataStream: TOle2File;
  const CellRange: TXlsCellRange);
var
  RecordHeader: TRecordHeader;
  CFCount: Word;
  i: integer;
  Rc: integer;
begin
  Rc:=RangeValuesList.CountRangeRecords(CellRange);
  if Rc=0 then exit; //Don't save empty CF's
  RecordHeader.Id:= xlr_CONDFMT;
  for i:=0 to RangeValuesList.RepeatCountR(Rc)-1 do
  begin
    RecordHeader.Size:=RangeValuesList.RecordSizeR(i, Rc);
    DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));

    CFCount:= CFs.Count;
    DataStream.WriteMem(CFCount, SizeOf(CFCount));
    DataStream.WriteMem(Flag, SizeOf(Flag));
    DataStream.WriteMem(AllRange, SizeOf(AllRange));

    RangeValuesList.SaveRangeToStreamR(DataStream, i, Rc, CellRange );
    CFs.SaveToStream(DataStream);
  end;
end;

function TCondFmt.TotalRangeSize(const CellRange: TXlsCellRange): int64;
var
  i: integer;
begin
  i:= RangeValuesList.CountRangeRecords(CellRange);
  if RangeValuesList.Count=0 then Result:=0 else
    Result:=RangeValuesList.TotalSizeR(i)
            + CFs.TotalSize*RangeValuesList.RepeatCountR(i);
end;

{ TCFRecord }

procedure TCFRecord.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
begin
//  No need to arrange nothing... ranges are relative to the cells
end;

procedure TCFRecord.ArrangeTokensInsertRowsAndCols(const atPos, fPos, InsRowPos, InsRowOffset, CopyRowOffset,InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
begin
  try
    UXlsTokenArray_ArrangeInsertRowsAndCols(Data, atPos, fPos, InsRowPos, InsRowOffset, CopyRowOffset,InsColPos, InsColOffset, CopyColOffset, SheetInfo, true);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadCF,[e.Token]);
    else raise;
  end; //Except
end;

constructor TCFRecord.Create(const aId: word;
  const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited;
  CfType:= Data[0];
  Op:=Data[1];
  Cce1:=GetWord(Data, 2);
  Cce2:=GetWord(Data, 4);
end;

procedure TCFRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
begin
  inherited;
  if Cce1>0 then ArrangeTokensInsertRowsAndCols(DataSize-Cce1-Cce2 , DataSize-Cce2,  aRowPos, aRowCount, 0, aColPos, aColCount,0,  SheetInfo);
  if Cce2>0 then ArrangeTokensInsertRowsAndCols(DataSize-Cce2 , DataSize,  aRowPos, aRowCount, 0, aColPos, aColCount,0,  SheetInfo);
end;

{ TCFRecordList }


procedure TCFRecordList.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount,SheetInfo);
end;

end.
