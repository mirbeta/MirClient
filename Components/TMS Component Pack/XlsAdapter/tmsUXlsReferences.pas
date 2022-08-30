unit tmsUXlsReferences;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, Sysutils, tmsXlsMessages, tmsUXlsBaseRecords,
     tmsUXlsBaseRecordLists, tmsUxlsBaseList, tmsUXlsStrings,
     tmsXlsFormulaMessages,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
    tmsUFlxMessages, tmsUOle2Impl;
type

  TExternNameRecord = class(TBaseRecord)
  public
    function Name: UTF16String;
    function NameLength: byte;
  end;

  TExternNameRecordList=class(TBaseRecordList)
  end;

  TSupBookRecord  = class(TBaseRecord)
  private
    FExternNameList: TExternNameRecordList;
  public
    function IsLocal: boolean;
    function IsAddIn: boolean;
    procedure InsertSheets(const SheetCount: integer);
    function BookName: UTF16String;
    function SheetName(const SheetIndex: integer; const Globals: TObject): UTF16String;

    procedure AddExternName(const ExternNameRecord: TExternNameRecord);

  //TBaseRecord functionality
  protected
    function DoCopyTo: TBaseRecord; override;
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateEmpty(const SheetCount: integer);
    destructor Destroy; override;

    procedure SaveToStream(const Workbook: TOle2File); override;
    function TotalSize: integer;override;
    function TotalSizeNoHeaders: integer;override;
  end;

  TExternSheetRecord = class(TBaseRecord)
  end;


  TExternRef= class
  public
    SupBookRecord: Word;
    FirstSheet, LastSheet: Word;
    constructor Create(const aSupBookRecord, aFirstSheet, aLastSheet: word);
    procedure SaveToStream(const DataStream: TOle2File);
  end;

  TSupBookRecordList = class(TBaseRecordList)
  {$INCLUDE TSupBookRecordListHdr.inc}
  function TotalSize: int64;
  end;



  TExternRefList= class(TBaseList)
  {$INCLUDE TExternRefListHdr.inc}
    procedure Load(const aRecord: TExternSheetRecord);
    procedure SaveToStream(const DataStream: TOle2File);
    function TotalSize: int64;

    procedure InsertSheets(const BeforeSheet, SheetCount:integer; LocalSupBook: integer);
  end;

  TReferences = class
  private
    FSupBooks: TSupBookRecordList;
    FExternRefs: TExternRefList;
    LocalSupBook: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function TotalSize:int64;
    procedure Clear;
    procedure SaveToStream(const DataStream: TOle2File);

    procedure AddSupBook(const aRecord: TSupBookRecord);
    procedure AddExternRef(const aRecord: TExternSheetRecord);
    procedure AddExternName(const aRecord: TExternNameRecord);

    procedure InsertSheets(const BeforeSheet, SheetCount: integer);
    function GetSheet(const SheetRef: word): integer;
    function SetSheet(const Sheet: word): integer;

    function AddSheet(SheetCount, FirstSheet, LastSheet: Integer): Integer;

    function GetSheetName(const SheetRef: word; const Globals: TObject): UTF16String;
    function GetName(const SheetRef: integer; const NameIndex: integer; const Globals: TObject): UTF16String;
  end;

implementation
uses tmsUXlsWorkbookGlobals;
{$INCLUDE TExternRefListImp.inc}
{$INCLUDE TSupBookRecordListImp.inc}

{ TExternRefList }


procedure TExternRefList.InsertSheets(const BeforeSheet, SheetCount:integer; LocalSupBook: integer);
var
  i:integer;
begin
  for i:=0 to Count-1 do
    if Items[i].SupBookRecord= LocalSupBook then
    begin
      //Handling of deleted references for Sheetcount<0
      if ((Items[i].FirstSheet>=BeforeSheet) and (Items[i].FirstSheet<BeforeSheet-SheetCount)) then // we will delete the reference
      begin
          Items[i].FirstSheet:=$FFFF;
      end;
      if ((Items[i].LastSheet>=BeforeSheet)and(Items[i].LastSheet<BeforeSheet-SheetCount))  then // we will delete the reference
      begin
          Items[i].LastSheet:=$FFFF;
      end;
      if (Items[i].FirstSheet<$FFFE) and (Items[i].FirstSheet>=BeforeSheet) then IncMax(Items[i].FirstSheet, SheetCount, MaxSheets);
      if (Items[i].LastSheet<$FFFE) and (Items[i].LastSheet>=BeforeSheet) then IncMax(Items[i].LastSheet, SheetCount, MaxSheets);
    end;
end;

procedure TExternRefList.Load(const aRecord: TExternSheetRecord);
var
  n: word;
  i: integer;
  aPos: integer;
  MyRecord: TBaseRecord;
  Index, Fs, Ls: word;
begin
  n:=GetWord(aRecord.Data, 0);
  aPos:=2; MyRecord:= aRecord;
  for i:=0 to n-1 do
  begin
    ReadMem(MyRecord, aPos, 2, @Index);
    ReadMem(MyRecord, aPos, 2, @Fs);
    ReadMem(MyRecord, aPos, 2, @Ls);
    Add(TExternRef.Create(Index,Fs,Ls));
  end;
end;

procedure TExternRefList.SaveToStream(const DataStream: TOle2File);
var
  RecordHeader: TRecordHeader;
  i, k, Lines, CountRecords:integer;
  MyCount: word;
begin
  MyCount:=Count;
  if Count =0 then
  begin
    //RecordHeader.Id:= xlr_EXTERNSHEET;
    //RecordHeader.Size:=2;
    //DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
    //DataStream.WriteMem(MyCount, SizeOf(MyCount));
    exit;
  end;

  Lines:=(6* Count-1) div MaxExternSheetDataSize;
  for i:= 0 to Lines do
  begin
    if i<Lines then CountRecords:= MaxExternSheetDataSize div 6
      else CountRecords:=((6* Count-1) mod MaxExternSheetDataSize + 1) div 6 ;
    RecordHeader.Size:= CountRecords*6;

    if i= 0 then
    begin
      RecordHeader.Id:= xlr_EXTERNSHEET;
      inc(RecordHeader.Size,2);
    end
    else RecordHeader.Id:= xlr_CONTINUE;

    DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
    if i=0 then DataStream.WriteMem( MyCount, SizeOf (MyCount));

    for k:= i*(MaxExternSheetDataSize div 6) to i*(MaxExternSheetDataSize div 6)+CountRecords-1 do
     Items[k].SaveToStream(DataStream);
  end;
end;

function TExternRefList.TotalSize: int64;
begin
  //Take in count Continues...
  if Count=0 then Result:=0 else //2+SizeOf(TRecordHeader) else
  Result:=2+ (((6* Count-1) div MaxExternSheetDataSize)+1)* SizeOf(TRecordHeader)  //header + continues
          + 6*Count;
end;

{ TReferences }

procedure TReferences.AddExternName(const aRecord: TExternNameRecord);
begin
  if FSupBooks.Count<=0 then raise Exception.Create(ErrExcelInvalid);
  FSupBooks[FSupBooks.Count-1].AddExternName(aRecord);
end;

procedure TReferences.AddExternRef(const aRecord: TExternSheetRecord);
begin
  FExternRefs.Load(aRecord);
end;

procedure TReferences.AddSupBook(const aRecord: TSupBookRecord);
begin
  FSupBooks.Add(aRecord);
  if aRecord.IsLocal then LocalSupBook:= FSupBooks.Count-1;
end;

procedure TReferences.Clear;
begin
  if FSupbooks<>nil then FSupBooks.Clear;
  if FExternRefs<>nil then FExternRefs.Clear;
  LocalSupBook:=-1;
end;

constructor TReferences.Create;
begin
  inherited;
  FSupBooks:=TSupBookRecordList.Create;
  FExternRefs:= TExternRefList.Create;
  LocalSupBook:=-1;
end;

destructor TReferences.Destroy;
begin
  FreeAndNil(FSupBooks);
  FreeAndNil(FExternRefs);
  inherited;
end;

function TReferences.GetSheet(const SheetRef: word): integer;
begin
  if (SheetRef>=FExternRefs.Count) then raise
    Exception.CreateFmt(ErrIndexOutBounds, [SheetRef,'Sheet Reference',0,FExternRefs.Count]);
  if (FExternRefs[SheetRef].SupBookRecord = LocalSupBook) and
     (FExternRefs[SheetRef].FirstSheet = FExternRefs[SheetRef].LastSheet) then

    Result:=FExternRefs[SheetRef].FirstSheet else Result:=-1;
end;

function TReferences.AddSheet(SheetCount: Integer; FirstSheet: Integer; LastSheet: Integer): Integer;
var
  i: Integer;
begin
  if (LocalSupBook < 0) then
    AddSupBook(TSupBookRecord.CreateEmpty(SheetCount));

  for i:=0 to FExternRefs.Count-1 do
  begin
    if (FExternRefs[i].SupBookRecord = LocalSupBook) and
       (FExternRefs[i].FirstSheet= FirstSheet) and
       (FExternRefs[i].LastSheet= LastSheet) then
        begin
          Result := i;
          exit;
        end;
  end;
  FExternRefs.Add(TExternRef.Create(LocalSupBook, FirstSheet, LastSheet));
  Result := (FExternRefs.Count - 1);
end;

function TReferences.GetName(const SheetRef: integer; const NameIndex: integer; const Globals: TObject): UTF16String;
var
  idx: integer;
begin
  idx := LocalSupBook;
  if (SheetRef >= 0) then
  begin
    if (SheetRef>=FExternRefs.Count) then raise
      Exception.CreateFmt(ErrIndexOutBounds, [SheetRef,'Sheet Reference',0,FExternRefs.Count - 1]);
    idx:=FExternRefs[SheetRef].SupBookRecord;
  end;

  if (idx = LocalSupBook) then
  begin
    if (NameIndex< 0) or (NameIndex >=(Globals as TWorkbookGlobals).Names.Count) then raise
      Exception.CreateFmt(ErrIndexOutBounds, [NameIndex,'Name Index',0,(Globals as TWorkbookGlobals).Names.Count - 1]);
    Result:= (Globals as TWorkbookGlobals).Names[NameIndex].Name;
    exit;
  end;

  if (idx< 0) or (idx >=FSupBooks.Count) then raise
    Exception.CreateFmt(ErrIndexOutBounds, [idx,'idx',0,FSupBooks.Count - 1]);
  if (NameIndex< 0) or (NameIndex >=FSupBooks[idx].FExternNameList.Count) then raise
    Exception.CreateFmt(ErrIndexOutBounds, [NameIndex,'Name Index',0,FSupBooks[idx].FExternNameList.Count - 1]);
  Result := (FSupBooks[idx].FExternNameList[NameIndex] as TExternNameRecord).Name;
end;


function TReferences.GetSheetName(const SheetRef: word; const Globals: TObject): UTF16String;
var
  idx: integer;
  Sh1: UTF16String;
  Ld: integer;
begin
  if (SheetRef>=FExternRefs.Count) then raise
    Exception.CreateFmt(ErrIndexOutBounds, [SheetRef,'Sheet Reference',0,FExternRefs.Count]);
  idx:=FExternRefs[SheetRef].SupBookRecord;
  Sh1:=FSupBooks[idx].SheetName(FExternRefs[SheetRef].FirstSheet, Globals);
  if FExternRefs[SheetRef].FirstSheet<>FExternRefs[SheetRef].LastSheet then
    Sh1:=Sh1+fmRangeSep+FSupBooks[idx].SheetName(FExternRefs[SheetRef].LastSheet, Globals);
  if idx = LocalSupBook then Result:='' else
  begin
    Result:= FSupBooks[idx].BookName;
    if Sh1<>'' then
    begin
      Ld:= LastDelimiter('\:',Result);
      if Ld>0 then Insert(fmWorkbookOpen, Result, Ld+1) else Result:=fmWorkbookOpen+Result;
      Result:=Result+fmWorkbookClose;
    end;
  end;
  Result:=Result+Sh1;
  if Result<>'' then Result:=''''+Result+''''+fmExternalRef;
end;

procedure TReferences.InsertSheets(const BeforeSheet, SheetCount: integer);
begin
  FExternRefs.InsertSheets(BeforeSheet, SheetCount, LocalSupBook);
  if LocalSupBook>=0 then FSupBooks[LocalSupBook].InsertSheets(SheetCount);
end;

procedure TReferences.SaveToStream(const DataStream: TOle2File);
begin
  FSupBooks.SaveToStream(DataStream);
  FExternRefs.SaveToStream(DataStream);
end;

function TReferences.SetSheet(const Sheet: word): integer;
var
  i:integer;
begin
  for i:=0 to FExternRefs.Count-1 do
    if (FExternRefs[i].SupBookRecord = LocalSupBook) and
       (FExternRefs[i].FirstSheet = FExternRefs[i].LastSheet) and
       (FExternRefs[i].FirstSheet = Sheet) then
       begin
         Result:=i;
         exit;
       end;

  //Ref doesnt exits...
  FExternRefs.Add(TExternRef.Create(LocalSupBook, Sheet, Sheet));
  Result:=FExternRefs.Count-1;

end;

function TReferences.TotalSize: int64;
begin
  Result:= FSupBooks.TotalSize+ FExternRefs.TotalSize;
end;

{ TExternRef }

constructor TExternRef.Create(const aSupBookRecord, aFirstSheet, aLastSheet: word);
begin
  inherited Create;
  SupBookRecord:=aSupBookRecord;
  FirstSheet:=aFirstSheet;
  LastSheet:=aLastSheet;
end;

procedure TExternRef.SaveToStream(const DataStream: TOle2File);
begin
  DataStream.WriteMem(SupBookRecord, SizeOf(SupBookRecord));
  DataStream.WriteMem(FirstSheet, SizeOf(FirstSheet));
  DataStream.WriteMem(LastSheet, SizeOf(LastSheet));
end;

{ TSupBookRecord }

function DecodeFileName(const s: UTF16String): UTF16String;
var
  i: integer;
begin
  Result:=''; i:=1;
  while i <= Length(s) do
  begin
    if s[i]=#1 then
    begin
      inc(i);
      if s[i]='@' then
      begin
        Result := Result + '\\';
      end else
      begin
        Result := Result + s[i] +':\';
      end
    end else
    if s[i]=#2 then
    begin
        Result := Result + '\';
    end else
    if s[i]=#3 then
    begin
        Result := Result + '\';
    end else
    if s[i]=#4 then
    begin
      Result := Result + '..\';
    end
    else
      Result := Result + s[i];

    inc(i);
  end;
end;

procedure TSupBookRecord.AddExternName(const ExternNameRecord: TExternNameRecord);
begin
  FExternNameList.Add(ExternNameRecord);
end;

function TSupBookRecord.BookName: UTF16String;
var
  Xs: TExcelString;
  MySelf: TBaseRecord;
  MyPos: integer;
begin
  if IsLocal or IsAddIn then begin; Result:= ''; exit; end;
  MySelf:=Self;
  MyPos:=2;
  Xs:=TExcelString.Create(2, MySelf, MyPos);
  try
    Result:=Xs.Value;
    if Length(Result)>0 then
    begin
      if Result[1]=#0 then Result:='' else
      if Result[1]=#1 then Result:=DecodeFileName(copy(Result,2,Length(Result))) else
      if Result[1]=#2 then Result:='';
    end;
  finally
    FreeAndNil(Xs);
  end; //finally
end;

constructor TSupBookRecord.Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited;
  FExternNameList:=TExternNameRecordList.Create;
end;

constructor TSupBookRecord.CreateEmpty(const SheetCount: integer);
var
  MyData: PArrayOfByte;
begin
  GetMem(MyData, 4);
  SetWord(MyData, 0, SheetCount);
  MyData[2]:=$01;
  MyData[3]:=$04;
  Create(xlr_SUPBOOK, myData, 4);
end;

destructor TSupBookRecord.Destroy;
begin
  FreeAndNil(FExternNameList);
  inherited;
end;

function TSupBookRecord.DoCopyTo: TBaseRecord;
begin
  Result:=inherited DoCopyTo;
  FreeAndNil((Result as TSupBookRecord).FExternNameList);
  (Result as TSupBookRecord).FExternNameList:= TExternNameRecordList.Create;
  (Result as TSupBookRecord).FExternNameList.CopyFrom(FExternNameList);
end;

procedure TSupBookRecord.InsertSheets(const SheetCount: integer);
begin
  if not IsLocal then raise Exception.Create(ErrExcelInvalid);
  IncWord(Data, 0, SheetCount, MaxSheets);
end;

function TSupBookRecord.IsLocal: boolean;
begin
  IsLocal:= (DataSize = 4)and (GetWord (Data, 2)= $0401);
end;

function TSupBookRecord.IsAddIn: boolean;
begin
  Result:= (DataSize = 4)and (GetWord (Data, 2)= $3A01);
end;

procedure TSupBookRecord.SaveToStream(const Workbook: TOle2File);
begin
  inherited;
  FExternNameList.SaveToStream(Workbook);
end;

function TSupBookRecord.SheetName(const SheetIndex: integer; const Globals: TObject): UTF16String;
var
  n: integer;
  i, tpos: integer;
  Xs: TExcelString;
  MySelf: TBaseRecord;
begin
  n:=GetWord(Data, 0);
  if (SheetIndex<0) or (SheetIndex>=n) then  //this might happen... on range references to another workbook
  begin
    Result:='';
    exit;
  end;

  if GetWord(Data,2)= $0401 then //current sheet
  begin
    Result:=(Globals as TWorkbookGlobals).SheetName[SheetIndex];
    exit;
  end;
  //A little slow... but it shouldn't be called much.
  //I don't think it justifies a cache.
  MySelf:=Self;
  tPos:=2;
  for i:=0 to SheetIndex do   //0 stands for the first unicode string, the book name.
  begin
    Xs:=TExcelString.Create(2, MySelf, tPos);
    try
    finally
      FreeAndNil(Xs);
    end; //finally
  end;

  Xs:=TExcelString.Create(2, MySelf, tPos);
  try
    Result:=Xs.Value;
  finally
    FreeAndNil(Xs);
  end; //finally
end;

function TSupBookRecord.TotalSize: integer;
begin
  Result:=inherited TotalSize+ FExternNameList.TotalSize;
end;

function TSupBookRecord.TotalSizeNoHeaders: integer;
var
  i:integer;
begin
  Result:=inherited TotalSizeNoHeaders;
  for i:=0 to FExternNameList.Count-1 do Result:=Result+ (FExternNameList[i] as TBaseRecord).TotalSizeNoHeaders;
end;



{ TSupBookRecordList }

function TSupBookRecordList.TotalSize: int64;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to Count-1 do Result:=Result+Items[i].TotalSize;
end;

{ TExternNameRecord }

function TExternNameRecord.Name: UTF16String;
var
  s: AnsiString;
begin
  if (Data[7] and 1)=1 then
  begin
    SetLength(Result, NameLength);
    Move(Data[8], Result[1], NameLength*2);
  end else
  begin
    SetLength(s, NameLength);
    Move(Data[8], s[1], NameLength);
    Result:= StringToWideStringNoCodePage(s);
  end;
end;

function TExternNameRecord.NameLength: byte;
begin
  Result:= Data[6];
end;

end.
