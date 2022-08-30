unit tmsUXlsCellRecords;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUXlsBaseRecords, tmsXlsMessages, tmsUFlxMessages,
     {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF}
     SysUtils, Classes, tmsUOle2Impl;


//MADE: generar mulblanks and mulrks al grabar
type
  TBlankRecord=class(TCellRecord)
  public
    constructor CreateFromData(const aRow, aCol, aXF: word);

    function CanJoinNext(const NextRecord: TCellRecord; const MaxCol: integer): boolean;override;
    procedure SaveFirstMul(const Workbook: TOle2File; const JoinedRecordSize: word);override;
    procedure SaveMidMul(const Workbook: TOle2File);override;
    procedure SaveLastMul(const Workbook: TOle2File);override;
    function TotalSizeFirst: integer; override;
    function TotalSizeMid: integer; override;
    function TotalSizeLast: integer;override;

  end;

  TBoolErrRecord=class(TCellRecord)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;

    function ErrcodeToString(const ErrCode: integer): UTF16String;
    function StringToErrcode(const ErrStr: UTF16String): integer;
  public
    constructor CreateFromData(const aRow, aCol, aXF: word);
  end;

  TNumberRecord=class(TCellRecord)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor CreateFromData(const aRow, aCol, aXF: word);
  end;

  TRKRecord=class(TCellRecord)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor CreateFromData(const aRow, aCol, aXF: word);

    function CanJoinNext(const NextRecord: TCellRecord; const MaxCol: integer): boolean;override;
    procedure SaveFirstMul(const Workbook: TOle2File; const JoinedRecordSize: word);override;
    procedure SaveMidMul(const Workbook: TOle2File);override;
    procedure SaveLastMul(const Workbook: TOle2File);override;
    function TotalSizeFirst: integer; override;
    function TotalSizeMid: integer; override;
    function TotalSizeLast: integer;override;

  end;

  TMultipleValueRecord= class(TBaseRecord)
    function Eof: boolean; virtual;abstract;
    function ExtractOneRecord: TCellRecord; virtual; abstract;
  end;

  TMulBlankRecord=class(TMultipleValueRecord)
  private
    CurrentCol: integer;
  public
    function Eof: boolean; override;
    function ExtractOneRecord: TCellRecord; override;
  end;

  TMulRKRecord=class(TMultipleValueRecord)
  private
    CurrentCol: integer;
  public
    function Eof: boolean; override;
    function ExtractOneRecord: TCellRecord; override;
  end;

  function EncodeRK(Value: double; out RK: Longint): boolean;
  function IsRK(const Value: double): boolean;
implementation

{ TBoolErrRecord }
constructor TBoolErrRecord.CreateFromData(const aRow, aCol, aXF: word);
begin
  inherited CreateFromData(xlr_BOOLERR, 8, aRow, aCol, aXF);
end;

function TBoolErrRecord.ErrcodeToString(const ErrCode: integer): UTF16String;
begin
  case ErrCode of
    xlerrcode_Null  : Result:=xlerr_Null;
    xlerrcode_Div0  : Result:=xlerr_Div0;
    xlerrcode_Value : Result:=xlerr_Value;
    xlerrcode_Ref   : Result:=xlerr_Ref;
    xlerrcode_Name  : Result:=xlerr_Name;
    xlerrcode_Num   : Result:=xlerr_Num;
    xlerrcode_NA    : Result:=xlerr_NA;
    else Result:=xlerr_Null;
  end; //case
end;

function TBoolErrRecord.GetValue: Variant;
begin
  if Data[7]=0 then if Data[6]=0 then Result:=false else Result:=true else
    Result:= ErrcodeToString(Data[6]);
end;

procedure TBoolErrRecord.SetValue(const Value: Variant);
begin
  case VarType(Value) of
    varBoolean:
    begin
      Data[7]:=0;
      if Value then Data[6]:=1 else Data[6]:=0;
    end;
    varOleStr,
    {$IFDEF DELPHI2008UP}
    varUString,
    {$ENDIF}
    varString:
    begin
      Data[7]:=1;
      Data[6]:=StringToErrcode(Value);
    end;
    varNull:
      raise Exception.CreateFmt(ErrInvalidCellValue, ['']);

    else raise Exception.CreateFmt(ErrInvalidCellValue, [VarAsType(Value, varString)]);
  end; //case
end;

function TBoolErrRecord.StringToErrcode(const ErrStr: UTF16String): integer;
begin
  if ErrStr = xlerr_Null  then Result:=xlerrcode_Null else
  if ErrStr = xlerr_Div0  then Result:=xlerrcode_Div0 else
  if ErrStr = xlerr_Value then Result:=xlerrcode_Value else
  if ErrStr = xlerr_Ref   then Result:=xlerrcode_Ref else
  if ErrStr = xlerr_Name  then Result:=xlerrcode_Name else
  if ErrStr = xlerr_Num   then Result:=xlerrcode_Num else
  if ErrStr = xlerr_NA    then Result:=xlerrcode_NA else
  raise Exception.CreateFmt(ErrInvalidErrStr,[ErrStr]);
end;

{ TNumberRecord }

constructor TNumberRecord.CreateFromData(const aRow, aCol, aXF: word);
begin
  inherited CreateFromData(xlr_NUMBER, 14, aRow, aCol, aXF);
end;

function TNumberRecord.GetValue: Variant;
var
  d: double;
begin
  move(Data[6],d,sizeof(d));
  Result:=d;
end;

procedure TNumberRecord.SetValue(const Value: Variant);
var
  d: double;
begin
  d:=Value;
  move(d,Data[6],sizeof(d));
end;

{ TRKRecord }

function TRKRecord.CanJoinNext(const NextRecord: TCellRecord;
  const MaxCol: integer): boolean;
begin
  Result:= (NextRecord is TRKRecord) and (NextRecord.Column=Column+1) and (NextRecord.Column<=MaxCol);
end;

constructor TRKRecord.CreateFromData(const aRow, aCol, aXF: word);
begin
  inherited CreateFromData(xlr_RK, 10, aRow, aCol, aXF);
end;

function TRKRecord.GetValue: Variant;
var
  Rk: ^LongInt;
  d: double;
  pd:^Longint;
begin
  //Warning: Be careful with sign. Shl/Shr works different that ">>" and "<<" in c
  //when using negative numbers. Shr will add 0 at the left, ">>" will preserve sign.

  Rk:=@(Data[6]);
  if Rk^ and $2 = $2 then //integer
    if Rk^ and (1 shl 31) <> 0 then //Negative
      d:=not(not(Rk^) shr 2) else
      d:=Rk^ shr 2 else
  begin
    pd:=@d;
    pd^:=0;
    inc(pd);
    pd^:= Rk^ and $fffffffc;
  end;

  Result:=d;
  if Rk^ and $1 = $1 then Result:=Result/100;
end;

function EncodeRK(Value: double; out RK: Longint): boolean;
var
  d: double;
  pd, pd1:^Longint;
  Mask: int64;
  i: integer;
begin
  if (Value > 1e300) or (Value < -1e300) then begin; Result := false; exit; end; // when we multiply by 100 below, we could raise an exception. If the number is so big, let's just avoid the RK optimization.
  if Value = 0 then  //a IEEE number might have the negative bit set, and we want 0 to be 0, not -0.
    Value := 0;

  Result:=true;
  for i:=0 to 1 do
  begin
    d:=Value*(1+99*i);
    pd:=@d;
    pd1:=pd;
    inc(pd1);
    if (pd^=0) and (pd1^ and 3=0) then    //Type 0-2   30 bits IEEE float
    begin
      RK:=pd1^ + i;
      exit;
    end;

    Mask:= $1FFFFFFF;  //29 bits
    if (Int(d)=d) and (d<=Mask) and (d>=-Mask-1) then   //Type 1-3: 30 bits integer
      begin
        RK:= Round(d) shl 2 + i+2;
        exit;
      end;
  end;

  Result:=False;
end;

procedure TRKRecord.SaveFirstMul(const Workbook: TOle2File;
  const JoinedRecordSize: word);
var
  x: word;
  r: PArrayOfByte;
begin
  x:=xlr_MULRK; Workbook.WriteMem(x, Sizeof(x));
  x:=JoinedRecordSize-SizeOf(TRecordHeader); Workbook.WriteMem(x, Sizeof(x));
  x:=Row; Workbook.WriteMem(x, Sizeof(x));
  x:=Column; Workbook.WriteMem(x, Sizeof(x));
  r:= @Data[4]; Workbook.WriteMem(r^, 6);
end;

procedure TRKRecord.SaveLastMul(const Workbook: TOle2File);
var
  x: word;
  r: PArrayOfByte;
begin
  inherited;
  r:= @Data[4]; Workbook.WriteMem(r^, 6);
  x:=Column; Workbook.WriteMem(x, Sizeof(x));
end;

procedure TRKRecord.SaveMidMul(const Workbook: TOle2File);
var
  r: PArrayOfByte;
begin
  inherited;
  r:= @Data[4]; Workbook.WriteMem(r^, 6);
end;

procedure TRKRecord.SetValue(const Value: Variant);
var
  Rk:^Longint;
begin
  Rk:=@(Data[6]);
  if not EncodeRK(Value, Rk^) then raise Exception.CreateFmt(ErrInvalidCellValue,[FloatToStr(Value)]);

end;

function IsRK(const Value: double): boolean;
var
  Dummy: Longint;
begin
  Result:=EncodeRK(Value, Dummy);
end;

function TRKRecord.TotalSizeFirst: integer;
begin
  Result:=TotalSize;
end;

function TRKRecord.TotalSizeLast: integer;
begin
  Result:=4+4;
end;

function TRKRecord.TotalSizeMid: integer;
begin
  Result:=2+4;
end;

{ TMulBlankRecord }

function TMulBlankRecord.ExtractOneRecord: TCellRecord;
var
  NewData: PArrayOfByte;
  NewDataSize: integer;
begin
  NewDataSize:=6;
  GetMem(NewData, NewDataSize);
  try
    SetWord(NewData, 0, GetWord(Data, 0));
    SetWord(NewData, 2, GetWord(Data, 2)+CurrentCol);
    SetWord(NewData, 4, GetWord(Data, 4 + CurrentCol*SizeOf(Word) ));

    Result:=TBlankRecord.Create( xlr_BLANK, NewData, NewDataSize);
    inc(CurrentCol);
  except
    FreeMem(NewData);
    raise;
  end;
end;

function TMulBlankRecord.Eof: boolean;
begin
  Result:=4+(CurrentCol+1)*SizeOf(Word)>=DataSize;
end;

{ TMulRKRecord }

type
TRKRec= packed record
  XF: Word;
  RK: Longint;
end;
PRKRec=^TRKRec;

function TMulRKRecord.ExtractOneRecord: TCellRecord;
var
  NewData: PArrayOfByte;
  NewDataSize: integer;
  Rec1, Rec2: PRKRec;
begin
  NewDataSize:=10;
  GetMem(NewData, NewDataSize);
  try
    SetWord(NewData, 0, GetWord(Data, 0));
    SetWord(NewData, 2, GetWord(Data, 2)+CurrentCol);
    Rec1:=PRKRec(@(Data[4+CurrentCol*SizeOf(TRKRec)]));
    Rec2:=PRKRec(@(NewData[4]));
    Rec2^:=Rec1^;

    Result:=TRKRecord.Create( xlr_RK, NewData, NewDataSize);
    inc(CurrentCol);
  except
    FreeMem(NewData);
    raise;
  end;
end;

function TMulRKRecord.Eof: boolean;
begin
  Result:=4+(CurrentCol+1)*SizeOf(TRKRec)>=DataSize;
end;

{ TBlankRecord }

constructor TBlankRecord.CreateFromData(const aRow, aCol, aXF: word);
begin
  inherited CreateFromData(xlr_BLANK, 6, aRow, aCol, aXF);
end;

function TBlankRecord.CanJoinNext(const NextRecord: TCellRecord;
  const MaxCol: integer): boolean;
begin
  Result:= (NextRecord is TBlankRecord) and (NextRecord.Column=Column+1) and (NextRecord.Column<=MaxCol);
end;

procedure TBlankRecord.SaveFirstMul(const Workbook: TOle2File;
  const JoinedRecordSize: word);
var
  x: word;
begin
  x:=xlr_MULBLANK; Workbook.WriteMem(x, Sizeof(x));
  x:=JoinedRecordSize-SizeOf(TRecordHeader); Workbook.WriteMem(x, Sizeof(x));
  x:=Row; Workbook.WriteMem(x, Sizeof(x));
  x:=Column; Workbook.WriteMem(x, Sizeof(x));
  x:=XF; Workbook.WriteMem(x, Sizeof(x));
end;

procedure TBlankRecord.SaveLastMul(const Workbook: TOle2File);
var
  x: word;
begin
  x:=XF; Workbook.WriteMem(x, Sizeof(x));
  x:=Column; Workbook.WriteMem(x, Sizeof(x));
end;

procedure TBlankRecord.SaveMidMul(const Workbook: TOle2File);
var
  x: word;
begin
  x:=XF; Workbook.WriteMem(x, Sizeof(x));
end;

function TBlankRecord.TotalSizeFirst: integer;
begin
  Result:=TotalSize;
end;

function TBlankRecord.TotalSizeLast: integer;
begin
  Result:=4;
end;

function TBlankRecord.TotalSizeMid: integer;
begin
  Result:=2;
end;


end.
