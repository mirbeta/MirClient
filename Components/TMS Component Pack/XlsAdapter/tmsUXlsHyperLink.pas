unit tmsUXlsHyperLink;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsXlsMessages,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsUFlxMessages, tmsUXlsBaseList, tmsUOle2Impl;

type
  TScreenTipRecord=class;

  THLinkRecord = class(TBaseRecord)
  private
    Hint: TScreenTipRecord;

    function ReadString(var Pos: Integer; const OptMask: Integer; const ByteSize: Integer): UTF16String;
    function ReadLocalFile(var Pos: Integer): UTF16String;
    procedure SetString(var Pos: Integer; const OptMask: Integer; const value: UTF16String);
    procedure SetString2(var Pos: Integer; const GUID: PArrayOfByte; const value: UTF16String; const ByteCount: Integer);
    procedure SetLocalFile(var Pos: Integer; const value: UTF16String);
    function IsUrl(pos: Integer): Boolean;
    function IsFile(pos: Integer): Boolean;
    function IsUNC(pos: Integer): Boolean;
    function GetText(var pos: Integer; var HType: THyperLinkType): UTF16String;
    procedure SetText(var pos: Integer; Text: UTF16String; HType: THyperLinkType);
    procedure ClearData;

    function GetFirstRow: Integer;
    function GetLastRow: Integer;
    function GetFirstCol: Integer;
    function GetLastCol: Integer;
    function GetOptionFlags: Integer;
    procedure SetFirstRow(Value: Integer);
    procedure SetLastRow(Value: Integer);
    procedure SetFirstCol(Value: Integer);
    procedure SetLastCol(Value: Integer);
    procedure SetOptionFlags(Value: Integer);

  protected
    function DoCopyTo: TBaseRecord; override;
  public
    procedure AddHint(const aHint: TScreenTipRecord);
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateNew(const CellRange: TXlsCellRange; const HLink: THyperLink);
    destructor Destroy;override;

    property FirstRow: Integer read GetFirstRow write SetFirstRow;
    property LastRow: Integer read GetLastRow write SetLastRow;
    property FirstCol: Integer read GetFirstCol write SetFirstCol;
    property LastCol: Integer read GetLastCol write SetLastCol;
    property OptionFlags: Integer read GetOptionFlags write SetOptionFlags;

    procedure SaveToStream(const Workbook: TOle2File);override;
    procedure SaveRangeToStream(const Workbook: TOle2File; const CellRange: TXlsCellRange);
    function TotalRangeSize(const CellRange: TXlsCellRange): Integer;

    function TotalSize: Integer;override;
    function TotalSizeNoHeaders: Integer;override;

    function GetProperties: THyperLink;
    procedure SetProperties(value: THyperLink);
    function GetCellRange: TXlsCellRange;
    procedure SetCellRange(CellRange: TXlsCellRange);

    procedure ArrangeInsertRange(CellRange: TXlsCellRange; aRowCount: Integer; aColCount: Integer; SheetInfo: TSheetInfo);
    function Offset(DeltaRow: Integer; DeltaCol: Integer): THLinkRecord;

  end;

  TScreenTipRecord = class(TBaseRecord)
  protected
    function GetFirstRow: Integer;
    function GetLastRow: Integer;
    function GetFirstCol: Integer;
    function GetLastCol: Integer;
    function GetText: UTF16String;
    procedure SetFirstRow(Value: Integer);
    procedure SetLastRow(Value: Integer);
    procedure SetFirstCol(Value: Integer);
    procedure SetLastCol(Value: Integer);
    procedure SetText(Value: UTF16String);

  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);  override;
    constructor CreateNew(aDescription: UTF16String);

    property FirstRow: Integer read GetFirstRow write SetFirstRow;
    property LastRow: Integer read GetLastRow write SetLastRow;
    property FirstCol: Integer read GetFirstCol write SetFirstCol;
    property LastCol: Integer read GetLastCol write SetLastCol;
    property Text: UTF16String read GetText write SetText;
  end;

  THLinkList = class(TBaseList)
  {$INCLUDE THLinkListHdr.inc}
  public
    procedure CopyFrom(aHLinkList: THLinkList);
    procedure CopyObjectsFrom(aHLinkList: THLinkList; CopyRange: TXlsCellRange; RowOfs: Integer; ColOfs: Integer);
    procedure SaveToStream(DataStream: TOle2File);
    procedure SaveRangeToStream(DataStream: TOle2File; CellRange: TXlsCellRange);
    function TotalSize: Int64;
    function TotalRangeSize(CellRange: TXlsCellRange): Int64;
    procedure InsertAndCopyRange(SourceRange: TXlsCellRange; DestRow: Integer;
      DestCol: Integer; aRowCount: Integer; aColCount: Integer; SheetInfo: TSheetInfo);
    procedure DeleteRange(CellRange: TXlsCellRange; aRowCount: Integer; aColCount: Integer;
      SheetInfo: TSheetInfo);
  end;

implementation
{$INCLUDE THLinkListImp.inc}

const
  FILEGUID: array [0..15] of byte=($03, $03, $00, $00, $00, $00, $00, $00, $C0, $00, $00, $00, $00, $00, $00, $46);
  URLGUID: array [0..15] of byte=($E0, $C9, $EA, $79, $F9, $BA, $CE, $11, $8C, $82, $00, $AA, $00, $4B, $A9, $0B);

constructor THLinkRecord.Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited Create(aId, aData, aDataSize);
  Hint := nil;
end;

function THLinkRecord.GetFirstRow: Integer;
begin
  Result := GetWord(Data, 0);
end;

function THLinkRecord.GetLastRow: Integer;
begin
  Result := GetWord(Data, 2);
end;

function THLinkRecord.GetFirstCol: Integer;
begin
  Result := GetWord(Data, 4);
end;

function THLinkRecord.GetLastCol: Integer;
begin
  Result := GetWord(Data, 6);
end;

function THLinkRecord.GetOptionFlags: Integer;
begin
  Result := GetWord(Data, 28);
end;

procedure THLinkRecord.SetFirstRow(Value: Integer);
begin
  SetWord(Data, 0, value);
end;

procedure THLinkRecord.SetLastRow(Value: Integer);
begin
  SetWord(Data, 2, value);
end;

procedure THLinkRecord.SetFirstCol(Value: Integer);
begin
  SetWord(Data, 4, value);
end;

procedure THLinkRecord.SetLastCol(Value: Integer);
begin
  SetWord(Data, 6, value);
end;

procedure THLinkRecord.SetOptionFlags(Value: Integer);
begin
  SetWord(Data, 28, value);
end;

constructor THLinkRecord.CreateNew(const CellRange: TXlsCellRange; const HLink: THyperLink);
var
  TmpData: PArrayOfByte;
begin
  GetMem(TmpData, 32);
  FillChar(TmpData[0], 32, 0);
  try
    inherited Create(xlr_HLINK, TmpData, 32);
    TmpData:=nil;

    FirstRow := CellRange.Top;
    FirstCol := CellRange.Left;
    LastRow := CellRange.Bottom;
    LastCol := CellRange.Right;
    SetLongWord(Data, 8, $79EAC9D0);
    SetLongWord(Data, 12, $11CEBAF9);
    SetLongWord(Data, 16, $AA00828C);
    SetLongWord(Data, 20, $0BA94B00);
    SetWord(Data, 24, 2);
    FillChar(Data[26],6,0);
    SetProperties(HLink);
  finally
    FreeAndNil(TmpData);
  end; //finally
end;

procedure THLinkRecord.SaveToStream(const Workbook: TOle2File);
begin
  inherited SaveToStream(Workbook);
  if (Hint <> nil) then
  begin
    Hint.FirstRow := FirstRow;
    Hint.FirstCol := FirstCol;
    Hint.LastRow := LastRow;
    Hint.LastCol := LastCol;
    Hint.SaveToStream(Workbook);
  end;
end;

procedure THLinkRecord.SaveRangeToStream(const Workbook: TOle2File; const CellRange: TXlsCellRange);
begin
  if ((((Self.FirstRow > CellRange.Bottom) or (Self.LastRow < CellRange.Top))
    or (Self.FirstCol > CellRange.Right)) or (Self.LastCol < CellRange.Left)) then
    exit;
  SaveToStream(Workbook);
end;

function THLinkRecord.TotalRangeSize(const CellRange: TXlsCellRange): Integer;
begin
  if ((((Self.FirstRow > CellRange.Bottom) or (Self.LastRow < CellRange.Top))
    or (Self.FirstCol > CellRange.Right)) or (Self.LastCol < CellRange.Left)) then
    begin
      Result := 0;
      exit;
    end;
  Result := TotalSize;
end;

function THLinkRecord.DoCopyTo: TBaseRecord;
begin
  Result := inherited DoCopyTo;
  if (Hint <> nil) then
    (Result as THLinkRecord).Hint := Hint.CopyTo as TScreenTipRecord;
end;

function THLinkRecord.TotalSize: Integer;
begin
  Result := inherited TotalSize;
  if (Hint <> nil) then
    inc(Result, Hint.TotalSize);
end;

function THLinkRecord.TotalSizeNoHeaders: Integer;
begin
  Result := inherited TotalSizeNoHeaders;
  if (Hint <> nil) then
    inc(Result, Hint.TotalSizeNoHeaders);
end;

function THLinkRecord.ReadString(var Pos: Integer; const OptMask: Integer; const ByteSize: Integer): UTF16String;
var
  OldPos: Integer;
  p: integer;
begin
  if ((OptionFlags and OptMask) <> OptMask) then
  begin
    Result := '';
  end
  else
  begin
    OldPos := Pos;
    inc(Pos, 4+Integer(GetLongWord(Data, Pos))*ByteSize);
    SetLength(Result, (Pos-(OldPos+4)-2) div 2); //00 Terminated
    move(Data[OldPos+4], Result[1], Length(Result)*2);
    p := System.Pos(#0, Result);
    if (p > 0) then Delete(Result, p, Length(Result)); //string might have a 0 inside. In this case we need to cut it.
  end;
end;

function THLinkRecord.ReadLocalFile(var Pos: Integer): UTF16String;
var
  XLen: Integer;
  RLen: Integer;
  s8: AnsiString;
  s16: UTF16String;
  StrLen: Integer;
  i: Integer;
  DirUp: Integer;
begin
  Result := '';
  DirUp := GetWord(Data, Pos);

  for i:=0 to DirUp-1 do
    Result:=Result+ IncludeTrailingPathDelimiter('..');

  inc(Pos,2);
  StrLen := GetLongWord(Data, Pos);
  if (StrLen > 1) then Dec(StrLen);

  SetLength(s8, StrLen);
  move(Data[Pos+4], s8[1], StrLen);
  inc(Pos, 4+StrLen+1+24);

  RLen := GetLongWord(Data, Pos);
  inc(Pos,4);
  if (RLen = 0) then
  begin
    Result:=Result+ StringToWideStringNoCodePage(s8);
    Exit;
  end;

  XLen := GetLongWord(Data, Pos);
  inc(Pos, 4+2);
  SetLength(s16, XLen div 2);
  move(Data[Pos], s16[1], XLen);
  inc(Pos, XLen);
  Result := Result+s16;
end;

procedure THLinkRecord.SetString(var Pos: Integer; const OptMask: Integer; const value: UTF16String);
var
  Len: Integer;
begin
  if (value= '') then
  begin
    if ((OptionFlags and OptMask) <> OptMask) then Exit; //Already empty
    OptionFlags:=OptionFlags and not OptMask;

    DataSize:=DataSize-4-Integer(GetLongWord(Data, Pos))*2;
    ReallocMem(Data, DataSize);
  end
  else
  begin
    Len := 0;
    if ((Self.OptionFlags and OptMask) = OptMask) then Len := 4 + GetLongWord(Data, Pos) * 2;
    DataSize:=DataSize-Len+4+Length(Value)*2+2;
    ReallocMem(Data, DataSize);
    if (Pos+1<DataSize) then FillChar(Data[Pos+1], DataSize-Pos-1, 0);

    SetLongWord(Data, Pos, Length(Value) + 1);
    move(value[1], Data[Pos+4] , Length(Value)* 2);
    inc(Pos, 4+Length(Value)*2+2);
    SetWord(Data, Pos-2, 0);

    OptionFlags := OptionFlags or OptMask;
  end;
end;

procedure THLinkRecord.SetString2(var Pos: Integer; const GUID: PArrayOfByte; const value: UTF16String; const ByteCount: Integer);
var
  GuidSize: integer;
begin
  if GUID=nil then GuidSize:=0 else GuidSize:=16;
  DataSize:=DataSize +GuidSize+ 4+ Length(Value)*2 + 2;
  ReallocMem(Data, DataSize);
  if (Pos+1<DataSize) then FillChar(Data[Pos+1], DataSize-Pos-1, 0);

  if (GUID<>nil) then
  begin
    move(GUID[0], Data[Pos], 16);
    inc(Pos, 16);
  end;
  SetLongWord(Data, Pos, ((Length(value)*2 + 2) div ByteCount));
  move(value[1], Data[Pos+4] , Length(Value)* 2);
  inc(Pos, 4+ Length(Value)*2+2);
  SetWord(Data, Pos-2 ,0);
end;

procedure THLinkRecord.SetLocalFile(var Pos: Integer; const value: UTF16String);
var
  WideDataLen: Integer;
  IsCompressed: Boolean;
  i: Integer;
  NewValue: UTF16String;
begin
  i := 0;
  while copy(value,i+1, 3) = IncludeTrailingPathDelimiter('..') do inc(i,3);

  NewValue := copy(value, i+1, Length(Value));
  IsCompressed := not IsWide(NewValue);
  WideDataLen := 0;
  if (not IsCompressed) then
  begin
    WideDataLen := 4+4+2+Length(NewValue)*2;
  end;

  DataSize:=DataSize+16+2+4+Length(NewValue)+1+24+4+WideDataLen;
  ReallocMem(Data, DataSize);
  if (Pos+1<DataSize) then FillChar(Data[Pos+1], DataSize-Pos-1, 0);
  move(FILEGUID[0], Data[Pos], 16);
  inc(Pos,16);
  SetWord(Data, Pos, (i div 3));
  inc (Pos, 2);
  SetLongWord(Data, Pos, Length(NewValue) + 1);
  inc(Pos,4);
  CompressBestUnicode(NewValue, Data, Pos);
  inc(Pos, Length(NewValue) + 1);
  Data[Pos-1]:=0;

  FillChar(Data[Pos], 24, 0);
  SetLongWord(Data, Pos, $DEADFFFF);
  inc(Pos,24);
  if IsCompressed then
  begin
    Exit;
  end;

  SetLongWord(Data, Pos, ((4 + 2) + Length(NewValue)*2));
  inc(Pos,4);
  SetLongWord(Data, Pos, Length(NewValue)*2);
  inc(Pos,4);
  SetWord(Data, Pos, 3);
  inc(Pos,2);
  move(NewValue[1], Data[Pos] , Length(NewValue)* 2);
  inc(Pos, Length(NewValue)* 2);
end;

function THLinkRecord.IsUrl(pos: Integer): Boolean;
begin
  if ((((OptionFlags and 3) = $03) and ((OptionFlags and $60) = 0)) and
    ((Self.OptionFlags and $100) = 0)) then
    begin
      Result := CompareMem(@URLGUID, PAddress(Data)+ pos,16);
      Exit;
    end;
  Result := False;
end;

function THLinkRecord.IsFile(pos: Integer): Boolean;
begin
  if ((((Self.OptionFlags and 1) = $01) and ((Self.OptionFlags and $60) = 0)) and
    ((Self.OptionFlags and $100) = 0)) then
    begin
      Result := CompareMem(@FILEGUID, PAddress(Data)+ pos,16);
      Exit;
    end;
  Result := False;
end;

function THLinkRecord.IsUNC(pos: Integer): Boolean;
begin
  Result := ((((Self.OptionFlags and $03) = $03) and ((Self.OptionFlags and $60) =0))
             and ((Self.OptionFlags and $100) <> 0));
end;

function THLinkRecord.GetText(var pos: Integer; var HType: THyperLinkType): UTF16String;
begin
  if IsUrl(pos) then
  begin
    HType := hl_URL;
    inc(Pos,16);
    Result := ReadString(pos, 0, 1);
    Exit;
  end;
  if IsFile(pos) then
  begin
    HType := hl_LocalFile;
    inc(Pos,16);
    Result := ReadLocalFile(pos);
    Exit;
  end;
  if IsUNC(pos) then
  begin
    HType := hl_UNC;
    Result := ReadString(pos, 0, 2);
    Exit;
  end;
  HType := hl_CurrentWorkbook;
  Result := '';
end;

procedure THLinkRecord.SetText(var pos: Integer; Text: UTF16String; HType: THyperLinkType);
begin
  case (HType) of
    hl_URL:
    begin
        OptionFlags:=(OptionFlags or $03) and not $160;
        SetString2(pos, @URLGUID, Text,1);
    end;

    hl_LocalFile:
    begin
        OptionFlags:=(OptionFlags or $01) and not $160;
        SetLocalFile(pos, Text);
    end;

    hl_UNC:
    begin
        OptionFlags:=(OptionFlags or $103) and not $60;
        SetString2(pos, nil, Text,2);
    end;

    hl_CurrentWorkbook:
    begin
        OptionFlags:=(OptionFlags) and not $163;
        //No SetString2(pos, nil, '', 1);
    end;
  end; //case
end;

function THLinkRecord.GetProperties: THyperLink;
var
  TmpLinkType: THyperLinkType;
  pos: Integer;
begin
  pos := 32;
  Result.Description := ReadString(pos, $14, 2);
  Result.TargetFrame := ReadString(pos, $80, 2);
  TmpLinkType := hl_CurrentWorkbook;
  Result.Text := GetText(pos, TmpLinkType);
  Result.LinkType := TmpLinkType;
  Result.TextMark := ReadString(pos, $8, 2);
  if (Hint = nil) then
    Result.Hint := ''
  else
    Result.Hint := Hint.Text;
end;

procedure THLinkRecord.ClearData;
begin
  DataSize:=32;
  ReallocMem(Data,DataSize);
  FillChar(Data[28],4,0);
end;

procedure THLinkRecord.SetProperties(value: THyperLink);
var
  pos: Integer;
begin
  ClearData;
  pos := 32;
  SetString(pos, $14, value.Description);
  SetString(pos, $80, value.TargetFrame);
  SetText(pos, value.Text, value.LinkType);
  SetString(pos, $8, value.TextMark);
  if (value.Hint = '') then
    FreeAndNil(Hint)
  else
    if (Hint = nil) then
      Hint := TScreenTipRecord.CreateNew(value.Hint)
    else
      Hint.Text := value.Hint;
end;

function THLinkRecord.GetCellRange: TXlsCellRange;
begin
  Result.Top:=FirstRow;
  Result.Left:=FirstCol;
  Result.Bottom:=LastRow;
  Result.Right:=LastCol;
end;

procedure THLinkRecord.SetCellRange(CellRange: TXlsCellRange);
begin
  FirstRow := CellRange.Top;
  FirstCol := CellRange.Left;
  LastRow := CellRange.Bottom;
  LastCol := CellRange.Right;
end;

procedure THLinkRecord.ArrangeInsertRange(CellRange: TXlsCellRange; aRowCount: Integer;
  aColCount: Integer; SheetInfo: TSheetInfo);
begin
  //Hyperlink data doesn't move when you insert/copy cells or sheets. It is a static text.

  if ((SheetInfo.InsSheet < 0) or (SheetInfo.FormulaSheet <> SheetInfo.InsSheet)) then
    Exit;
  if (((aRowCount <> 0) and (FirstCol >= CellRange.Left)) and (LastCol <= CellRange.Right)) then
  begin
    if (FirstRow >= CellRange.Top) then
      IncWord(Data, 0, (aRowCount * (CellRange.Bottom-CellRange.Top+1)), Max_Rows);
    if (LastRow >= CellRange.Top) then
      IncWord(Data, 2, (aRowCount * (CellRange.Bottom-CellRange.Top+1)), Max_Rows);
  end;
  if (((aColCount <> 0) and (FirstRow >= CellRange.Top)) and (LastRow <= CellRange.Bottom)) then
  begin
    if (FirstCol >= CellRange.Left) then
      IncWord(Data, 4, (aColCount * (CellRange.Right-CellRange.Left+1)), Max_Columns);
    if (Self.LastCol >= CellRange.Left) then
      IncWord(Data, 6, (aColCount * (CellRange.Right-CellRange.Left+1)), Max_Columns);
  end;
end;

function THLinkRecord.Offset(DeltaRow: Integer; DeltaCol: Integer): THLinkRecord;
begin
  FirstRow:= FirstRow + DeltaRow;
  LastRow:= LastRow + DeltaRow;
  FirstCol:= FirstCol + DeltaCol;
  LastCol:= LastCol + DeltaCol;

  Result := Self;
end;

{ TScreenTipRecord }

constructor TScreenTipRecord.Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited Create(aId, aData, aDataSize);
end;

constructor TScreenTipRecord.CreateNew(aDescription: UTF16String);
begin
   Create(xlr_SCREENTIP, nil, 0);
   Text := aDescription;
end;


function TScreenTipRecord.GetFirstRow: Integer;
begin
  Result := GetWord(Data, 2);
end;

function TScreenTipRecord.GetLastRow: Integer;
begin
  Result := GetWord(Data, 4);
end;

function TScreenTipRecord.GetFirstCol: Integer;
begin
  Result := GetWord(Data, 6);
end;

function TScreenTipRecord.GetLastCol: Integer;
begin
  Result := GetWord(Data, 8);
end;

function TScreenTipRecord.GetText: UTF16String;
begin
  SetLength(Result, (DataSize-10-2)div 2);
  move(Data[10], Result[1], Length(Result)*2);
end;

procedure TScreenTipRecord.SetFirstRow(Value: Integer);
begin
  SetWord(Data, 2, value);
end;

procedure TScreenTipRecord.SetLastRow(Value: Integer);
begin
  SetWord(Data, 4, value);
end;

procedure TScreenTipRecord.SetFirstCol(Value: Integer);
begin
  SetWord(Data, 6, value);
end;

procedure TScreenTipRecord.SetLastCol(Value: Integer);
begin
  SetWord(Data, 8, value);
end;

procedure TScreenTipRecord.SetText(Value: UTF16String);
begin
  FreeAndNil(Data);
  DataSize:=12+Length(Value)*2;
  GetMem(Data, DataSize);
  FillChar(Data[0],DataSize,0);
  move(Value[1], Data[10], Length(Value)*2);
  SetWord(Data, 0, 2048);
end;

{ THLinkList }
procedure THLinkList.CopyFrom(aHLinkList: THLinkList);
var
  i: Integer;
begin
  for i := 0 to aHLinkList.Count-1 do
  begin
    Add(aHLinkList[i].CopyTo as THLinkRecord);
  end;
end;

procedure THLinkList.CopyObjectsFrom(aHLinkList: THLinkList; CopyRange: TXlsCellRange;
  RowOfs: Integer; ColOfs: Integer);
var
  r: THLinkRecord;
  i: Integer;
begin
  if (aHLinkList = nil) then exit;
  for i := 0 to aHLinkList.Count-1 do
  begin
    r := aHLinkList[i];
    if ((((r.FirstCol >= CopyRange.Left) and (r.LastCol <= CopyRange.Right))
    and (r.FirstRow >= CopyRange.Top)) and (r.LastRow <= CopyRange.Bottom)) then
        Add((THLinkRecord(r.CopyTo)).Offset(RowOfs, ColOfs));
  end;
end;

procedure THLinkList.SaveToStream(DataStream: TOle2File);
var
  i: Integer;
begin
  Sort;
  for i := 0 to Count-1 do
  begin
    Items[i].SaveToStream(DataStream);
  end;
end;

procedure THLinkList.SaveRangeToStream(DataStream: TOle2File; CellRange: TXlsCellRange);
var
  i: Integer;
begin
  Sort;
  for i := 0 to Count-1 do
  begin
    Items[i].SaveRangeToStream(DataStream, CellRange);
  end;
end;

function THLinkList.TotalSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    inc(Result, Items[i].TotalSize);
end;

function THLinkList.TotalRangeSize(CellRange: TXlsCellRange): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    inc(Result, Items[i].TotalRangeSize(CellRange));
end;

procedure THLinkList.InsertAndCopyRange(SourceRange: TXlsCellRange; DestRow: Integer;
  DestCol: Integer; aRowCount: Integer; aColCount: Integer; SheetInfo: TSheetInfo);
var
  cc: Integer;
  rc: Integer;
  r: THLinkRecord;
  i: Integer;
  RBottom: Integer;
  RRight: Integer;
  RLeft: Integer;
  RTop: Integer;
  SourceRangeRowCount: integer;
  SourceRangeColCount: integer;
begin
  SourceRangeRowCount:=SourceRange.Bottom-SourceRange.Top+1;
  SourceRangeColCount:=SourceRange.Right-SourceRange.Left+1;

  for i:=0 to Count-1 do
  begin
    Items[i].ArrangeInsertRange(OffsetRange(SourceRange, DestRow, DestCol), aRowCount, aColCount, SheetInfo);
  end;

  //Pending:  if (CopyMode = TRangeCopyMode.None) then exit;
  RTop := SourceRange.Top;
  RLeft := SourceRange.Left;
  if (DestRow <= SourceRange.Top) then inc(RTop, aRowCount* SourceRangeRowCount);
  if (DestCol <= SourceRange.Left) then inc(RLeft, aColCount* SourceRangeColCount);
  RRight := ((RLeft + SourceRangeColCount) - 1);
  RBottom := ((RTop + SourceRangeRowCount) - 1);

  if ((aRowCount > 0) or (aColCount > 0)) then
    begin
      for i:=0 to Count-1 do
      begin
        r := Items[i];
        if (r.FirstCol >= RLeft) and (r.LastCol <= RRight) and (r.FirstRow>= RTop) and (r.LastRow <= RBottom) then
        begin
          for rc:=0 to aRowCount-1 do
          begin
            Add(THLinkRecord(r.CopyTo).Offset(DestRow - RTop + rc * SourceRangeRowCount, DestCol - RLeft));
          end;
          for cc:=0 to aColCount-1 do
          begin
            Add(THLinkRecord(r.CopyTo).Offset(DestRow - RTop,DestCol - RLeft + cc * SourceRangeColCount));
          end;
        end;
      end;
    end;
end;

procedure THLinkList.DeleteRange(CellRange: TXlsCellRange; aRowCount: Integer;
  aColCount: Integer; SheetInfo: TSheetInfo);
var
  bColCount: Integer;
  bRowCount: Integer;
  r: THLinkRecord;
  i: Integer;
  CellRangeRowCount: integer;
  CellRangeColCount: integer;
begin
  CellRangeRowCount:=CellRange.Bottom-CellRange.Top+1;
  CellRangeColCount:=CellRange.Right-CellRange.Left+1;
  for i:= Count-1 downto 0 do
  begin
    r := Items[i];
    bRowCount := (aRowCount - 1);if (bRowCount < 0) then bRowCount := 0;
    bColCount := (aColCount - 1);if (bColCount < 0) then bColCount := 0;

    if (r.FirstRow >= CellRange.Top) and (r.LastRow <= CellRange.Bottom +
      CellRangeRowCount * bRowCount) and (r.FirstCol >= CellRange.Left)
      and (r.LastCol <= CellRange.Right + CellRangeColCount * bColCount) then
      Delete(i)
    else
    begin
      r.ArrangeInsertRange(CellRange, -aRowCount, -aColCount, SheetInfo);
      if (r.LastRow < r.FirstRow) then
        Delete(i);
    end;
  end;
end;

procedure THLinkRecord.AddHint(const aHint: TScreenTipRecord);
begin
  FreeAndNil(Hint);
  Hint:=aHint;
end;

destructor THLinkRecord.Destroy;
begin
  FreeAndNil(Hint);
  inherited;
end;

end.

