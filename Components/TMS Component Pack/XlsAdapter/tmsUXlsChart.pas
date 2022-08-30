unit tmsUXlsChart;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsXlsMessages, tmsUXlsTokenArray, Classes, SysUtils, tmsUXlsBaseList;

type
  TChartRecord = class (TBaseRecord)
  end;

  TBeginRecord = class(TChartRecord)
  end;

  TEndRecord = class(TChartRecord)
  end;

  TChartAIRecord = class (TChartRecord)
  private
    Flags, FLen: word;

    procedure ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);

  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
  end;

  TChartAIRecordCache = class (TBaseList)
    {$INCLUDE TChartAIRecordCacheHdr.inc}
    constructor Create;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);
  end;

  TChartRecordList = class (TBaseRecordList)
  private
    AICache: TChartAIRecordCache;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification);override;
  public
    constructor Create;
    destructor Destroy;override;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);
  end;

implementation

{ TChartAIRecordCache }
{$INCLUDE TChartAIRecordCacheImp.inc}

constructor TChartAIRecordCache.Create;
begin
  inherited Create(False) //We don't own the objects
end;

procedure TChartAIRecordCache.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeCopyRowsAndCols(RowOffset, ColOffset);
end;

procedure TChartAIRecordCache.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer;
  const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo);
end;

procedure TChartAIRecordCache.ArrangeCopySheet(const SheetInfo: TSheetInfo);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].ArrangeCopySheet(SheetInfo);
end;

{ TChartAIRecord }

//This shouldn't make sense... all ranges in charts are absolute. This is to support RelativeCharts
procedure TChartAIRecord.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
const
  SheetInfo: TSheetInfo=(InsSheet:-1;FormulaSheet:-1;GetSheet:nil;SetSheet:nil;Names:nil);
begin
  if FLen>0 then ArrangeTokensInsertRowsAndCols(0, 0, RowOffset, 0, 0, ColOffset, SheetInfo); //Sheet info doesn't have meaninig on copy
end;

procedure TChartAIRecord.ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
begin
  try
    UXlsTokenArray_ArrangeInsertRowsAndCols(Data, 8, 8+FLen, InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, False);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadChartFormula,[e.Token]);
    else raise;
  end; //Except
end;

constructor TChartAIRecord.Create(const aId: word; const aData: PArrayOfByte;
  const aDataSize: integer);
begin
  inherited;
  Flags:=GetWord(Data, 0);
  FLen:=GetWord(Data, 6);
end;

procedure TChartAIRecord.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  if FLen=0 then exit;
  try
    UXlsTokenArray_ArrangeInsertSheets(Data, 8, 8+FLen, SheetInfo);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadChartFormula,[e.Token]);
    else raise;
  end; //Except
end;

procedure TChartAIRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer;
  const SheetInfo: TSheetInfo);
begin
  if FLen>0 then ArrangeTokensInsertRowsAndCols(aRowPos, aRowCount, 0, aColPos, aColCount, 0, SheetInfo);
end;

{ TChartRecordList }

constructor TChartRecordList.Create;
begin
  inherited;
  AICache:= TChartAIRecordCache.Create;
end;

destructor TChartRecordList.Destroy;
begin
  FreeAndNil(AICache);
  inherited;
end;

procedure TChartRecordList.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
begin
  AICache.ArrangeCopyRowsAndCols(RowOffset, ColOffset);
end;

procedure TChartRecordList.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer;
  const SheetInfo: TSheetInfo);
begin
  AICache.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo);
end;

procedure TChartRecordList.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  AICache.ArrangeCopySheet(SheetInfo);
end;

procedure TChartRecordList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (AICache<>nil) then if (TBaseRecord(Ptr) is TChartAIRecord) then
    AICache.Delete(AICache.IndexOf(TBaseRecord(Ptr)));
  if Action = lnAdded then if (TBaseRecord(Ptr) is TChartAIRecord) then
    AICache.Add(TChartAIRecord(Ptr));
  inherited Notify(Ptr, Action);
end;


end.
