unit tmsUXlsClientData;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords, tmsUXlsChart,
     tmsUXlsSST, tmsXlsMessages, tmsUXlsSheet, tmsUXlsBaseClientData, tmsUFlxMessages, tmsUOle2Impl;
type
  TMsObj = class(TBaseClientData)
  private
    FObjRecord: TObjRecord;
    FChart: TFlxChart;
    FImData: TBaseRecord;
    HasPictFmla: boolean;
    function Get_IsAutoFilter(): Boolean;
  protected
    function GetId: Word; override;
    procedure SetId(const Value: Word); override;
    procedure ScanRecord( myRecord: TBaseRecord);
  public
    procedure  ArrangeId(var MaxId: word);override;

    constructor Create;
    constructor CreateEmpty(var MaxId: word; const data: array of byte);
    class function CreateEmptyImg(var MaxId: word): TMsObj;
    class function CreateEmptyNote(var MaxId: word): TMsObj;
    class function CreateEmptyAutoFilter(var MaxId: word): TMsObj;
    destructor Destroy; override;
    procedure Clear; override;
    function CopyTo: TBaseClientData; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBaseRecord; const SST: TSST); override;
    procedure SaveToStream(const DataStream: TOle2File); override;
    function TotalSize: int64;override;

    procedure ArrangeCopyRowsAndCols(const RowOfs, ColOfs: integer);override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);override;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);override;

    class function ObjRecord: ClassOfTBaseRecord; override;

    property IsAutoFilter: Boolean read Get_IsAutoFilter;

  end;

  TTXO= class (TBaseClientData)
  private
    FTXO: TTXORecord;
    function GetValue: UTF16String;
    procedure SetValue(const aValue: UTF16String);
    procedure ScanRecord(var Value: UTF16String; var RTFRuns: TRTFRunList);
  public
    constructor Create;
    constructor CreateFromData(const Dummy: integer=1);
    destructor Destroy; override;
    procedure Clear; override;
    function CopyTo: TBaseClientData; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBaseRecord; const SST: TSST); override;
    procedure SaveToStream(const DataStream: TOle2File); override;
    function TotalSize: int64;override;

    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);override;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);override;
    class function ObjRecord: ClassOfTBaseRecord; override;

    property Value: UTF16String read GetValue write SetValue;
  end;

implementation

{ TMsObj }

procedure TMsObj.Clear;
begin
  FreeAndNil(FObjRecord);
  FreeAndNil(FChart);
  FreeAndNil(FImData);
  FreeAndNil(RemainingData);
end;

function TMsObj.CopyTo: TBaseClientData;
begin
  if HasPictFmla then Raise Exception.Create(ErrCantCopyPictFmla);
  Result:= TMsObj.Create;
  (Result as TMsObj).FObjRecord:= FObjRecord.CopyTo as TObjRecord;
  if FChart<>nil then (Result as TMsObj).FChart:= FChart.CopyTo as TFlxChart;
  if FImData<>nil then (Result as TMsObj).FImData:= FImData.CopyTo;
end;

constructor TMsObj.Create;
begin
  inherited;
end;

destructor TMsObj.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMsObj.ArrangeCopyRowsAndCols(const RowOfs, ColOfs: integer);
begin
  if FChart<>nil then FChart.ArrangeCopyRowsAndCols(RowOfs, ColOfs);
end;

procedure TMsObj.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo);
begin
  if FChart<>nil then FChart.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo);
end;

procedure TMsObj.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  if FChart<>nil then FChart.ArrangeCopySheet(SheetInfo);
end;


procedure TMsObj.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBaseRecord; const SST: TSST);
var
  R: TBaseRecord;
begin
  Clear;
  if ((First as TObjRecord).ObjId= ftCmo) and ((First as TObjRecord).CmoId = xlCmo_Chart) then
  begin
    R:=LoadRecords(DataStream, RecordHeader);
    try
      if not(R is TBOFRecord) then raise Exception.Create(ErrExcelInvalid);
      FChart:= TFlxChart.Create(nil);
      try
        FChart.LoadFromStream(DataStream, RecordHeader, R as TBOFRecord, SST);
      except
        FreeAndNil(FChart);
        raise
      end; //except
    except
      FreeAndNil(R);
      raise
    end; //Except
  end else
  if ((First as TObjRecord).ObjId= ftCmo) and ((First as TObjRecord).CmoId = xlcmo_Picture) then
  begin
    if RecordHeader.Id=xlr_IMDATA then FImData:=LoadRecords(DataStream, RecordHeader); //Here recordHeader.Id has the next record.
  end;

  ScanRecord(First);

  if (FChart <> nil) and (RemainingData = nil) then
  begin
    RemainingData := FChart.RemainingData;
    FChart.RemainingData := nil;
  end;

  //this must be the last statment, so if there is an exception, we dont take First
  FObjRecord:= First as TObjRecord;

end;

procedure TMsObj.SaveToStream(const DataStream: TOle2File);
begin
  if FObjRecord=nil then raise Exception.Create(ErrExcelInvalid);
  FObjRecord.SaveToStream(DataStream);
  if FImData<>nil then FImData.SaveToStream(DataStream);
  if FChart<>nil then FChart.SaveToStream(DataStream);
end;

function TMsObj.TotalSize: int64;
begin
  if FObjRecord=nil then raise Exception.Create(ErrExcelInvalid);
  Result:=FObjRecord.TotalSize;
  if FChart<>nil then Result:=Result+FChart.TotalSize;
  if FImData<>nil then Result:=Result+FImData.TotalSize;
end;



class function TMsObj.ObjRecord: ClassOfTBaseRecord;
begin
  Result:= TObjRecord;
end;

function TMsObj.GetId: Word;
begin
  if FObjRecord<>nil then GetId:=GetWord( FObjRecord.Data, 6) else GetId:=0;
end;

function TMsObj.Get_IsAutoFilter: Boolean;
begin
  Result := (((((FObjRecord.DataSize = 70)
     and (FObjRecord.Data[0] = 21))
     and (FObjRecord.Data[22] = 12))
     and (FObjRecord.Data[46] = 19))
     and (FObjRecord.Data[56] = 1))
     and (FObjRecord.Data[57] = 3);
end;

procedure TMsObj.SetId(const Value: Word);
begin
  if FObjRecord<>nil then SetWord( FObjRecord.Data, 6, Value);
end;

procedure TMsObj.ArrangeId(var MaxId: word);
begin
  inherited;
  inc(MaxId);
  Id:=MaxId;
end;

procedure TMsObj.ScanRecord( myRecord: TBaseRecord);
var
  RHeader: TRecordHeader;
  aPos: integer;
begin
  aPos:=0;
  repeat
    ReadMem(myRecord, aPos, SizeOf(RHeader), @RHeader);
    if RHeader.Id= 9 then HasPictFmla:=true;
    if (Rheader.Size+aPos>myRecord.DataSize) then //This shouldn't be really necessary, but Obj records sometimes don't count right. So, we have to ensure all is in the same Obj record. I assume there are no continues here.
    begin
      //Longer than expected???
      RemainingData:=myRecord.Continue;
      MyRecord.Continue:=nil;
      exit;
    end;

    try
      ReadMem(myRecord, aPos, RHeader.Size, nil);
    except
      //Longer than expected???
      RemainingData:=nil;
      exit;
    end;

  until RHeader.Id=0; // ftEnd
  RemainingData:=myRecord.Continue;
  myRecord.Continue:=nil;
end;

const
  EmptyMsObjImg: array[0..37] of byte=
    ($15, $00, $12, $00, $08, $00, $01, $00, $11, $60, $00, $00, $00, $00, //Note that here ObjId=1. This has to be changed later on code
     $00, $00, $00, $00, $00, $00, $00, $00, // ftCmo
     $07, $00, $02, $00, $FF, $FF, //ftCf
     $08, $00, $02 ,$00 ,$01, //ftPioGrbit
     $00, $00, $00, $00, $00); //ftEnd

  EmptyMsObjNote: array[0..51] of byte=
    ($15, $00, $12, $00, $19, $00, $01, $00, $11, $40, $00, $00, $00, $00, //Note that here ObjId=1. This has to be changed later on code
     $00, $00, $00, $00, $00, $00, $00, $00, // ftCmo
     $0D, $00, $16, $00,
     $00, $12, $23, $8C, $6C, $50, $8B, $4D, $A8, $C5, $7B, $64, $FF, $A8, $C5,
     $A3, $00, $00, $10, $00, $00,  //ftNts
     $00, $00, $00, $00, $00); //ftEnd

	EmptyMsObjAutoFilter: array[0.. 69] of byte =
			(
				$15, $00, $12, $00, $14, $00, $01, $00, $01, $21, $00, $00, $00, $00, //Note that here ObjId=1. This has to be changed later on code
				$00, $00, $00, $00, $00, $00, $00, $00, // ftCmo
				$0C, $00, $14, $00, $00, $00, $00, $00, $00, $00, $00, $00, $64, $00, $01, $00, $0A, $00, $00, $00, $10, $00, $01, $00, //ftSbs  Scroll bar data
				$13, $00, $EE, $1F, $00, $00, $00, $00, $04, $00, $01, $03, $00, $00, $02, $00, $08, $00, $6C, $00, $00, $00, $00, $00  //List box data. this record has a negative length, but Excel writes it this way.
			);


constructor TMsObj.CreateEmpty(var MaxId: word; const data: array of byte);
var
  MyData: PArrayOfByte;
  MyDataSize: integer;
begin
  Create;
  MyDataSize:=SizeOf(data);
  GetMem(MyData, MyDataSize);
  try
    Move(data, MyData^, MyDataSize);
    FObjRecord:=TObjRecord.Create(xlr_OBJ, MyData, MyDataSize);
    ArrangeId(MaxId);
  except
    FreeAndNil(MyData);
    raise;
  end; //Except
end;

class function TMsObj.CreateEmptyImg(var MaxId: word): TMsObj;
begin
    Result := TMsObj.CreateEmpty(MaxId, EmptyMsObjImg);
end;

class function TMsObj.CreateEmptyNote(var MaxId: word): TMsObj;
begin
  Result := TMsObj.CreateEmpty(MaxId, EmptyMsObjNote);
end;

class function TMsObj.CreateEmptyAutoFilter(var MaxId: word): TMsObj;
begin
  Result := TMsObj.CreateEmpty(MaxId, EmptyMsObjAutoFilter);
end;

{ TTXO }

procedure TTXO.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  //nothing
end;

procedure TTXO.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo);
begin
end;

procedure TTXO.Clear;
begin
  FreeAndNil(FTXO);
  FreeAndNil(RemainingData);
end;

function TTXO.CopyTo: TBaseClientData;
begin
  Result:= TTXO.Create;
  if FTXO <>nil then (Result as TTXO).FTXO:= FTXO.CopyTo as TTXORecord;
end;

constructor TTXO.Create;
begin
  inherited;
end;

constructor TTXO.CreateFromData(const Dummy: integer=1);
begin
  Create;
  FTXO:=TTXORecord.CreateFromData;
  //No need for continue recods
end;

destructor TTXO.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTXO.ScanRecord(var Value: UTF16String; var RTFRuns: TRTFRunList);
var
  i, DestPos, Len: integer;
  s: AnsiString;
  w:UTF16String;
  OptionFlags, ActualOptionFlags: byte;
  TxtRec: TBaseRecord;
  aPos: integer;
  r: array[0..3]of word;
begin
  Value:='';SetLength(RTFRuns,0);
  if FTXO.Continue=nil then exit;
  Len:= GetWord(FTXO.Data, 10);

  if (Len=0) then
  begin
    RemainingData.Free;
    RemainingData:=FTXO.Continue;
    FTXO.Continue:=nil;
    exit;
  end;
  TxtRec:=FTXO.Continue;
  aPos:=1; DestPos:=0;
  if TxtRec<>nil then
  begin
    OptionFlags:=TxtRec.Data[0] and 1; ActualOptionFlags:=OptionFlags;
    SetLength(s, Len);
    SetLength(w, Len);
    ReadStr(TxtRec, aPos, s, w, OptionFlags, ActualOptionFlags, DestPos, Len);
    if (OptionFlags and $1) = 0 then Value:=StringToWideStringNoCodePage(s) else Value:=w;
  end else Value:='';

  Len:= GetWord(FTXO.Data, 12) div 8;
  SetLength(RTFRuns, Len);
  if (TxtRec<>nil) then TxtRec:=TxtRec.Continue;
  aPos:=0;
  for i:=0 to Len-1 do
  begin
    ReadMem(TxtRec, aPos, 8, @r);
    RTFRuns[i].FirstChar:=r[0];
    RTFRuns[i].FontIndex:=r[1];
  end;
  if (TxtRec<>nil) and (TxtRec.Continue<>nil) then
  begin
    RemainingData.Free;
    RemainingData:=TxtRec.Continue;
    TxtRec.Continue:=nil;
  end else FreeAndNil(RemainingData);

end;

function TTXO.GetValue: UTF16String;
var
  RTFRuns: TRTFRunList;
begin
  ScanRecord(Result, RTFRuns);
end;

procedure TTXO.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TBaseRecord; const SST: TSST);
begin
  FTXO:=First as TTXORecord;
  //We have to search for any Continues that do not apply here.
  GetValue;
end;

class function TTXO.ObjRecord: ClassOfTBaseRecord;
begin
  Result:= TTXORecord;
end;

procedure TTXO.SaveToStream(const DataStream: TOle2File);
begin
  if FTXO<>nil then FTXO.SaveToStream(DataStream);
end;

procedure TTXO.SetValue(const aValue: UTF16String);
var
  Len: integer;
  Dat: PArrayOfByte;
  s: AnsiString;
begin
  Len:=Length(aValue);
  SetWord(FTXO.Data, 10, Len); //length of text
  if Len>0 then SetWord(FTXO.Data, 12, 16) else SetWord(FTXO.Data, 12, 0); //length of formatting runs
  FreeAndNil(FTXO.Continue);
  if Len>0 then
  begin
    if IsWide(aValue) then
      begin
        GetMem(Dat, Len*2+1);
        Dat[0]:=1;
        move(aValue[1], Dat[1], Len*2);
        FTXO.Continue:=TContinueRecord.Create(xlr_CONTINUE, Dat, Len*2+1);
      end else
      begin
        GetMem(Dat, Len+1);
        Dat[0]:=0;
        s:= WideStringToStringNoCodePage(aValue);
        move(s[1], Dat[1], Len);
        FTXO.Continue:=TContinueRecord.Create(xlr_CONTINUE, Dat, Len+1);
      end;

      Len:= 2*8;
      GetMem(Dat, Len);
      FillChar(Dat^, Len, 0);
      SetWord(Dat, 8, Length(aValue));

      FTXO.Continue.Continue:= TContinueRecord.Create(xlr_CONTINUE, Dat, Len);
  end;

end;

function TTXO.TotalSize: int64;
begin
  Result:= FTXO.TotalSize;
end;

end.
