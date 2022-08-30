unit tmsUXlsSST;
{$INCLUDE ..\FLXCOMPILER.INC}
//This is a unit to optimize the SST for a big number of strings.
//Optimizations:
  //We use records, no objects to store the strings (4 bytes of VMT per string and avoid calling create/destroy)
  //We don't use Widestrings or Strings to store them (8+6 bytes / string and avoid double allocation, one for the record and one for the string)
  //PENDING: Hash array to locate strings

interface
uses SysUtils, tmsXlsMessages, tmsUXlsBaseRecords,
     tmsUXlsOtherRecords, tmsUXlsStrings, Classes, tmsUFlxMessages,
      {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
    {$IFDEF FLX_GENERICS} Generics.Collections, {$ENDIF}
     tmsUOle2Impl;
type
  TExtraData=Packed Record
    Refs: word;
    AbsStreamPos: LongWord;
    RecordStreamPos: Word;
    PosInTable:LongWord;
  end;

  PExtraData=^TExtraData;

  TiSSTEntry   = integer;  //offset to the array
  PiSSTEntry   = PArrayOfByte;  //Pointer to internal calcs. Never store it, because MemData.Buffer can be realocated

  TMemSST=record
     UsedSize: integer;
     Buffer: Array of Byte;
  end;

  {$IFDEF FLX_GENERICS}
  TSST = class(TList<TiSSTEntry>)
  private
  {$ELSE}
  TSST = class (TList)
    {$INCLUDE TiSSTHdr.inc}
  {$ENDIF}

  private
    MemSST: TMemSST;
    procedure QuickSort(L, R: Integer);
    function SSTRecordSize: int64;
    function ExtSSTRecordSize: int64;
  public
    constructor Create;
    function Find(const s:PiSSTEntry; var Index: integer): boolean;
    procedure Load(const aSSTRecord: TSSTRecord);
    procedure SaveToStream(const DataStream: TOle2File);
    procedure WriteExtSST(const DataStream: TOle2File);
    function AddString(const s:UTF16String; const RTFRuns: TRTFRunList):integer;
    procedure Sort;
    function TotalSize: int64;
    procedure FixRefs;

    function GetEntry(const aEntry: TiSSTEntry): PiSSTEntry;
  end;

  TLabelSSTRecord= class(TCellRecord)
  private
    pSSTEntry: TiSSTEntry;
    SST: TSST;
    function GetAsString: UTF16String;
    procedure SetAsString(const Value: UTF16String);
    function GetAsRTF: UTF16String;
    procedure SetAsRTF(const Value: UTF16String);
    function GetAsRichString: TRichString;
    procedure SetAsRichString(const Value: TRichString);
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function DoCopyTo: TBaseRecord; override;
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateFromData(const aRow, aCol, aXF: word; const aSST: TSST);

    procedure AttachToSST(const aSST: TSST);
    procedure SaveToStream(const Workbook: TOle2File); override;

    destructor Destroy;override;

    property AsString: UTF16String read GetAsString write SetAsString;
    property AsRichString: TRichString read GetAsRichString write SetAsRichString;
    property AsRTF: UTF16String read GetAsRTF write SetAsRTF;
  end;

  TLabelRecord=class(TCellRecord)
    function GetValue: Variant; override;
    //We dont implement writing to a label. All writing should go to a LabelSST
  end;

  TRStringRecord=class(TCellRecord)
  private
    function GetAsRichString: TRichString;
  public
    function GetValue: Variant; override;
    property AsRichString: TRichString read GetAsRichString;
    //We dont implement writing to a label. All writing should go to a LabelSST
  end;

implementation
{$IFNDEF FLX_GENERICS}
{$INCLUDE TiSSTImp.inc}
{$ENDIF}

const
  MemSSTDeltaSize=8096*1024; {4M}

type
  TRecordBuff= array [0..MaxRecordDataSize+4] of byte;
    
procedure CreateSSTEntryFromString(var MemSST: TMemSST; const s: UTF16String; const RTFRuns: TRTFRunList; var Entry: TiSSTEntry);
var
  OptionFlags: byte;
  Wide: byte;
  Lb, OldSize, Posi, i: integer;
  pEntry: PArrayOfByte;
begin
  if IsWide(s) then Wide := 1 else Wide:=0;
  OptionFlags := Wide;
  if Length(RTFRuns)>0 then OptionFlags:=OptionFlags or $8;

{  GetMem(Entry, SizeOf(TExtraData)+ //Extra data not to be saved
                SizeOf(Word) + // String Length
                SizeOf(byte) + // OptionsFlag
                Length(s)*(1+OptionFlags));
}
  OldSize:=MemSST.UsedSize;
  inc( MemSST.UsedSize, SizeOf(TExtraData)+ //Extra data not to be saved
                        SizeOf(Word) + // String Length
                        SizeOf(byte) + // OptionsFlag
                        Length(s)*(1+Wide));

  if Length(RTFRuns)>0 then
    inc( MemSST.UsedSize, Length(RTFRuns)*4+2 ); //RTF Info

  Lb:=Length(MemSST.Buffer);
  if MemSST.UsedSize>=Lb then
    SetLength(MemSST.Buffer, Lb+ MemSSTDeltaSize); //A string can't be longer than 8192 bytes;

  Entry:=OldSize;
  pEntry:=@MemSST.Buffer[Entry];
  PExtraData(pEntry).Refs:=0;
  PExtraData(pEntry).AbsStreamPos:=0;
  PExtraData(pEntry).RecordStreamPos:=0;
  PExtraData(pEntry).PosInTable:=0;

  SetWord(pEntry, SizeOf(TExtraData), Length(s));
  pEntry[2+SizeOf(TExtraData)]:=OptionFlags;

  Posi:=3+SizeOf(TExtraData);
  if Length(RTFRuns)>0 then
  begin
    SetWord(pEntry, Posi, Length(RTFRuns));
    inc(Posi,2);
  end;

  if Wide = 1 then
  begin
    System.Move(s[1], pEntry^[Posi], Length(s)*2);
    inc(Posi, Length(s)*2);
  end else
  begin
    System.Move(WideStringToStringNoCodePage(s)[1], pEntry^[Posi], Length(s));
    inc(Posi, Length(s));
  end;

  for i:=0 to Length(RTFRuns)-1 do
  begin
    SetWord(pEntry, Posi, RTFRuns[i].FirstChar);
    SetWord(pEntry, Posi+2, RTFRuns[i].FontIndex);
    Inc(Posi, 4);
  end;

end;

procedure CreateSSTEntryFromRecord(var MemSST: TMemSST; var aSSTRecord: TBaseRecord; var Ofs: integer; var Entry: TiSSTEntry);
var
  Xs: TExcelString;
  Lb, OldSize: integer;
  pEntry: PArrayOfByte;
begin
  Xs:=TExcelString.Create(2, aSSTRecord, Ofs); //Ok, we use TExcelString... This could be done without creating an object, but I don't think there is a difference
                                             // and it's complicated, because it has to handle all continues and char-widechar issues
  try
    {GetMem(Entry, SizeOf(TExtraData)+Xs.TotalSize);}
    OldSize:=MemSST.UsedSize;
    inc( MemSST.UsedSize, SizeOf(TExtraData)+Xs.TotalSize);

    Lb:=Length(MemSST.Buffer);
    if MemSST.UsedSize>=Lb then
      SetLength(MemSST.Buffer, Lb+ MemSSTDeltaSize); //A string can't be longer than 8192 bytes;

    Entry:=OldSize;
    pEntry:=@MemSST.Buffer[OldSize];

    PExtraData(pEntry).Refs:=0;
    PExtraData(pEntry).AbsStreamPos:=0;
    PExtraData(pEntry).RecordStreamPos:=0;
    PExtraData(pEntry).PosInTable:=0;
    Xs.CopyToPtr(pEntry, SizeOf(TExtraData));
  finally
    FreeAndNil(Xs);
  end;

end;

function SSTLength(const S: PiSSTEntry): int64;
var
  OptionFlags: byte;
  Ofs: integer;
begin
    Ofs:=0;
    OptionFlags:=S[2+SizeOf(TExtraData)];
    Result:=SizeOf(TExtraData)+
            2+ //Length
            SizeOf(OptionFlags);
    if OptionFlags and $1 = 0 then Result:=Result+GetWord(S, SizeOf(TExtraData))
        else Result:= Result+GetWord(S, SizeOf(TExtraData))*2;

    //Rich text
    if OptionFlags and $8 = $8 {HasRichText} then
    begin
      Result:=Result + 2+ 4* GetWord(S,3+SizeOf(TExtraData));
      Ofs:=2;
    end;

    //FarEast
    if OptionFlags and $4 = $4 {HasFarInfo} then
      Result:=Result+ 4 + GetLongWord(S, 3+SizeOf(TExtraData)+Ofs);
end;

{function SSTRealLength(const S: PiSSTEntry): int64;
begin
  Result:=SSTLength(S)-SizeOf(TExtraData);
end;
}

function CompareSSTEntry(const S1, S2: PiSSTEntry): integer;
var
  i:integer;
  L1, L2: integer;
begin
  Result:=0;
  L1:= SSTLength(S1);
  L2:= SSTLength(S2);
  if L1<L2 then Result:=-1 else if L1>L2 then Result:=1
  else
  for i:=SizeOf(TExtraData) to L1-1 do
  begin
    if S1[i]=S2[i] then continue
    else if S1[i]<S2[i] then Result:=-1 else Result:=1;
    exit;
  end;
end;

function CompareSSTEntries(Item1, Item2: Pointer): Integer;
begin
  CompareSSTEntries:= CompareSSTEntry(PiSSTEntry(Item1),PiSSTEntry(Item2));
end;


procedure AddSSTRef(const Entry: PiSSTEntry);
begin
  Inc(PExtraData(Entry).Refs);
end;

procedure DecSSTRef(const Entry: PiSSTEntry);
begin
  Dec(PExtraData(Entry).Refs);
end;

function SSTRefs(const Entry: PiSSTEntry): word;
begin
  Result:=PExtraData(Entry).Refs;
end;

procedure AddContinue (const DataStream: TOle2File; var Buffer: TRecordBuff; var BufferPos: integer; var BeginRecordPos: LongWord; var TotalSize: int64);
begin
	if DataStream<>nil then
  begin
	  SetWord(PArrayOfByte(@Buffer), 2, BufferPos - 4);  //Adapt the record size before writing it.
		DataStream.WriteMem(Buffer, BufferPos);

  	BeginRecordPos:=DataStream.Position;
    SetWord(PArrayOfByte(@Buffer), 0, xlr_CONTINUE);
 		Buffer[4]:=0; Buffer[5]:=0; //Clear the OptionFlags.
  end;

  inc(TotalSize, BufferPos);
  BufferPos:= 4;
end;

function Min(const a, b: integer): integer;
begin
  if a<b then Result:=a else Result:=b;
end;

procedure WriteArray(const DataToWrite: PArrayOfByte; const DataLength: integer; DataStream: TOle2File; var Buffer: TRecordBuff;
  var BufferPos: Integer; var BeginRecordPos: LongWord; var TotalSize: Int64);
var
  Chars: Integer;
  BytesLeft: Integer;
  StPos: Integer;
begin
  StPos := 0;
  while (StPos < DataLength) do
  begin
    BytesLeft := (Length(Buffer) - BufferPos) div 4 * 4; //A string can not be splitted between formatting runs.
    Chars := Min((DataLength - StPos), BytesLeft);
    System.Move(DataToWrite[StPos], Buffer[BufferPos], Chars);
    inc(BufferPos, Chars);
    inc(StPos, Chars);
    if (StPos < DataLength) then
      AddContinue(DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
  end;
end;



procedure SaveSSTToStream(const Entry: PiSSTEntry; const DataStream: TOle2File;
          var BeginRecordPos: LongWord; var Buffer: TRecordBuff; var BufferPos: Integer;  var TotalSize: Int64);

var
  i: Integer;
  b: Byte;
  CanCompress: Boolean;
  CharsUncompressed: Integer;
  CharsCompressed: Integer;
  StPos: Integer;
  OpFlagsPos: Integer;
  BytesLeft: Integer;
  aLen: word;
  OptionFlags: byte;
  p: integer;
  FarEastLen: LongWord;
  RTFRuns: word;
  Data: PArrayOfByte;
  CharSize: integer;
begin
  //First, see if we can write the header of this string on the current record, or if we have to create a new one
  BytesLeft := (Length(Buffer) - BufferPos);
  if (BytesLeft < 32) then    //12 is really the required, but we play it safe. Anyway, starting a new continue does no harm.
  begin
    AddContinue(DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
    BytesLeft := (Length(Buffer) - BufferPos);
  end;

  if (DataStream <> nil) then
  begin
    PExtraData(Entry).AbsStreamPos := (DataStream.Position + BufferPos);
    PExtraData(Entry).RecordStreamPos := PExtraData(Entry).AbsStreamPos - BeginRecordPos;
  end;

  Assert((BytesLeft >= 32));
  aLen:=GetWord(Entry, SizeOf(TExtraData));
  if (DataStream <> nil) then System.Move(aLen , Buffer[BufferPos], 2);
  inc(BufferPos,2);

  OpFlagsPos := BufferPos;
  OptionFlags:= Entry[2+SizeOf(TExtraData)];
  Buffer[BufferPos] := OptionFlags;
  inc(BufferPos);

  p:=3;
  if  OptionFlags and $8 = $8 then //HasRichText then
  begin
    RTFRuns:=GetWord(Entry,p+SizeOf(TExtraData));

    if (DataStream <> nil) then
    begin
      System.Move(RTFRuns , Buffer[BufferPos], 2);
    end;
    inc(p,2);
    inc(BufferPos, 2);
  end;

  if OptionFlags and $4 = $4 then //HasFarInfo then
  begin
    FarEastLen:=GetLongWord(Entry,p+SizeOf(TExtraData));
    if (DataStream <> nil) then
    begin
      System.Move(FarEastLen , Buffer[BufferPos], 4);
    end;
    inc(p,4);
    inc(BufferPos, 4);
  end;

	// Write the actual string. It might span multiple continues
  StPos := 0;
  Data:= PArrayOfByte(@Entry[p+SizeOf(TExtraData)]);
  CharSize:=(OptionFlags and 1)+1;
  while (StPos < aLen) do  //If aLen==0, we won't write this string.
  begin
    BytesLeft := (Length(Buffer) - BufferPos);

		//Find if we can compress the unicode on this part.
		//If the number of chars we can write using compressed is bigger or equal than using uncompressed, we compress...
    CharsCompressed := Min((aLen - StPos), BytesLeft);
    CharsUncompressed := Min((aLen - StPos), (BytesLeft div 2));
    if (CharSize <> 1) then              //if charsize=1, string is already compressed.
    begin
      for i:= 0 to CharsCompressed-1 do
      begin
        if (Data[(StPos*CharSize + i*2+1)] <> 0) then
        begin
          CharsCompressed := i;
          break;
        end;
      end;
    end;

    CanCompress := (CharsCompressed >= CharsUncompressed);
    if CanCompress then
    begin
      b := $FE;
      Buffer[OpFlagsPos] := Buffer[OpFlagsPos] and b;
      if (DataStream <> nil) then
      begin
        for i := 0 to CharsCompressed-1 do
        begin
           Buffer[BufferPos] := Data[(StPos + i)*CharSize];
           inc(BufferPos);
        end;
      end
        else inc(BufferPos, CharsCompressed);

      inc( StPos, CharsCompressed);

      if (StPos < aLen) then
      begin
        AddContinue(DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
        OpFlagsPos := BufferPos;
        inc(BufferPos);
      end;
    end
      else
    begin
      b := 1;
      Buffer[OpFlagsPos] := Buffer[OpFlagsPos] or b;
      if (DataStream <> nil) then
      begin
        System.Move(Data[StPos*2], Buffer[BufferPos], 2*CharsUncompressed);
      end;

      inc (BufferPos, CharsUncompressed*2);
      inc (StPos, CharsUncompressed);

      if (StPos < aLen) then
      begin
        AddContinue(DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
        OpFlagsPos := BufferPos;
        inc(BufferPos);
      end;
    end;
  end;

  inc(p, aLen*CharSize);

  if  OptionFlags and $8 = $8 then //HasRichText then
  begin
     Data:= PArrayOfByte(@Entry[p+SizeOf(TExtraData)]);
     WriteArray(Data, RTFRuns*4, DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
     p:=p+RTFRuns*4;
  end;

  if OptionFlags and $4 = $4 then //HasFarInfo then
  begin
     Data:= PArrayOfByte(@Entry[p+SizeOf(TExtraData)]);
     WriteArray(Data, FarEastLen, DataStream, Buffer, BufferPos, BeginRecordPos, TotalSize);
  end;

end;

function GetSSTValue(const Entry: PiSSTEntry; var RTFRunList: TRTFRunList): UTF16String;
var
  OptionFlags: byte;
  Ini: integer;
  RTFRunCount: integer;
  i: integer;
  St: AnsiString;
begin
    OptionFlags:=Entry[2+SizeOf(TExtraData)];
    Ini:=SizeOf(TExtraData)+
            2+ //Length
            SizeOf(OptionFlags);

    //Rich text
    RTFRunCount:=0;
    if OptionFlags and $8 = $8 {HasRichText} then
    begin
      RTFRunCount:=GetWord(Entry, Ini);
      Inc(Ini, 2);
    end;

    //FarEast
    if OptionFlags and $4 = $4 {HasFarInfo} then
      Inc(Ini, 4);

    if OptionFlags and $1 = 0 then
    begin
      SetLength(St, GetWord(Entry, SizeOf(TExtraData)));
      Move(Entry[Ini], St[1], Length(St));
      Inc(Ini, Length(St));
      Result:=StringToWideStringNoCodePage(St);
    end else
    begin
      SetLength(Result, GetWord(Entry, SizeOf(TExtraData)));
      Move(Entry[Ini], Result[1], Length(Result)*2);
      Inc(Ini, Length(Result)*2);
    end;

    SetLength(RTFRunList, RTFRunCount);
    for i:=0 to RTFRunCount-1 do
    begin
      RTFRunList[i].FirstChar:=GetWord(Entry, Ini);
      RTFRunList[i].FontIndex:=GetWord(Entry, Ini+2);
      inc(Ini,4);
    end;
end;

//**************************************************************
{ TSST }
function TSST.AddString(const s: UTF16String; const RTFRuns: TRTFRunList): integer;
var
  es: TiSSTEntry;
  pEs: PiSSTEntry;
  LastMem: integer;
begin
  LastMem:=MemSST.UsedSize;
  CreateSSTEntryFromString(MemSST, s, RTFRuns, es);
  try
    pEs:=@MemSST.Buffer[es];
    if Find(pEs, Result) then
    begin
      AddSSTRef(@MemSST.Buffer[self[Result]]);
      MemSST.UsedSize:=LastMem; //No need to add space.
    end else
    begin
      Insert(Result, es);
      AddSSTRef(pEs);
      //es:=nil;  //so we dont free it
    end;
  finally
    //No need to free. if es<>nil then Freemem(es);
  end;
end;

function TSST.Find(const S: PiSSTEntry; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareSSTEntry(@MemSST.Buffer[self[I]],S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TSST.Load(const aSSTRecord: TSSTRecord);
var
  i, Ofs:integer;
  Es: TiSSTEntry;
  TmpSSTRecord: TBaseRecord;
begin
  Ofs:=8;
  TmpSSTRecord:= aSSTRecord;
  for i:=0 to aSSTRecord.Count-1 do
  begin
    CreateSSTEntryFromRecord(MemSST, TmpSSTRecord, Ofs, Es);
    try
      Add(Es);
      //Es:=nil;
    finally
      //No need to free. if es<>nil then Freemem(Es);
    end; //Finally
  end;
  //We can't sort now, this should be done after all the LABELSST records have been loaded
end;

procedure TSST.FixRefs;
var
  i: integer;
begin
  for i:=count-1 downto 0 do
    if SSTRefs(@MemSST.Buffer[self[i]])<=0 then Delete(i);
end;

procedure TSST.SaveToStream(const DataStream: TOle2File);
var
  i:integer;
  TotalRefs, aCount: LongWord;
  BeginRecordPos: LongWord;
  Se: PiSSTEntry;
  Buffer: TRecordBuff;
  BufferPos: integer;
  TotalSize: int64;
  w:word;
begin
  BeginRecordPos:=DataStream.Position;
  w:=xlr_SST;
  System.move(w, Buffer[0], 2);

  //Renum the items
  i:=0; TotalRefs:=0;
  while i< Count do
  begin
    Se:=@MemSST.Buffer[self[i]];
    Assert(SSTRefs(Se)>0,'Refs should be >0');
    PExtraData(Se).PosInTable:=i;
    TotalRefs:=TotalRefs+LongWord(SSTRefs(Se));
    inc(i);
   end;

  System.move(TotalRefs, Buffer[4], 4);
  aCount:=Count;
  System.move(aCount, Buffer[8], 4);


  BufferPos:=4+8;
  TotalSize:=0;

  for i:= 0 to Count-1 do
  begin
    SaveSSTToStream(@MemSST.Buffer[Self[i]], DataStream, BeginRecordPos, Buffer, BufferPos, TotalSize);
  end;

  //Flush the buffer.
  SetWord(PArrayOfByte(@Buffer), 2, BufferPos - 4);  //Adapt the record size before writing it.
  DataStream.WriteMem(Buffer, BufferPos);

  WriteExtSST(DataStream);
end;

procedure TSST.WriteExtSST(const DataStream: TOle2File);
var
  n, nBuckets, Dummy: Word;
  i: integer;
  RecordHeader: TRecordHeader;
begin
  // Calc number of strings per hash bucket
  n:=Count div 128+1;
  if n<8 then n:=8;

  if Count=0 then nBuckets:=0 else nBuckets:= (Count-1) div n + 1;

  RecordHeader.Id:= xlr_EXTSST;
  RecordHeader.Size:= 2+8*nBuckets;
  DataStream.WriteMem(RecordHeader, SizeOf(RecordHeader));
  DataStream.WriteMem(n, SizeOf(n));
  i:= 0; Dummy:=0;
  while i<Count do
  begin
    DataStream.WriteMem(PExtraData(@MemSST.Buffer[Self[i]]).AbsStreamPos, SizeOf(PExtraData(nil).AbsStreamPos));
    DataStream.WriteMem(PExtraData(@MemSST.Buffer[Self[i]]).RecordStreamPos, SizeOf(PExtraData(nil).RecordStreamPos));
    DataStream.WriteMem(Dummy, SizeOf(Dummy));
    inc(i,n);
  end;

end;


procedure TSST.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: Pointer;
  T: integer;
begin
  repeat
    I := L;
    J := R;
    P := @MemSST.Buffer[self[(L + R) shr 1]];
    repeat
      while CompareSSTEntries(@MemSST.Buffer[Self[I]], P) < 0 do
        Inc(I);
      while CompareSSTEntries(@MemSST.Buffer[Self[J]], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := Self[I];
        Self[I] := Self[J];
        Self[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TSST.Sort;
begin
  if (Count > 0) then
    QuickSort(0, Count - 1);
end;

function TSST.ExtSSTRecordSize: int64;
var
  n, nBuckets: word;
begin
  n:=Count div 128+1;
  if n<8 then n:=8;

  if Count=0 then nBuckets:=0 else nBuckets:= (Count-1) div n + 1;
  Result:= 2+8*nBuckets+SizeOf(TRecordHeader);
end;

//Simulates a write to know how much it takes.
function TSST.SSTRecordSize: int64;
//Has to handle continue records
var
  BeginRecordPos:LongWord;
  Buffer: TRecordBuff;
  BufferPos: integer;
  TotalSize: int64;
  i: integer;
begin
  BeginRecordPos:=0;
  BufferPos:=4+8;
  TotalSize:=0;
  for i:=0 to Count-1 do
  begin
    SaveSSTToStream(@MemSST.Buffer[Self[i]], nil, BeginRecordPos, Buffer, BufferPos, TotalSize);
  end;

  Result:=TotalSize+BufferPos;
end;


function TSST.TotalSize: int64;
begin
  Result:= SSTRecordSize + ExtSSTRecordSize;
end;

constructor TSST.Create;
begin
  inherited;
  MemSST.UsedSize:=0;
  SetLength(MemSST.Buffer, MemSSTDeltaSize);
end;

function TSST.GetEntry(const aEntry: TiSSTEntry): PiSSTEntry;
begin
  Result:=@MemSST.Buffer[aEntry];
end;

{ TLabelSSTRecord }

constructor TLabelSSTRecord.Create(const aId: word;
  const aData: PArrayOfByte; const aDataSize: integer);
begin
  inherited Create(aId, aData, aDataSize);
end;

procedure TLabelSSTRecord.AttachToSST(const aSST: TSST);
var
  a:int64;
begin
  SST:=aSST;
  a:=GetLongWord(Data,6);
  if a>= SST.Count then raise Exception.Create(ErrExcelInvalid);
  pSSTEntry:= SST[a];
  AddSSTRef(SST.GetEntry(pSSTEntry));
end;

destructor TLabelSSTRecord.Destroy;
begin
    if (pSSTEntry>=0) and (SST <> nil) then DecSSTRef(SST.GetEntry(pSSTEntry));
  inherited;
end;

procedure TLabelSSTRecord.SaveToStream(const Workbook: TOle2File);
begin
  SetLongWord(Data, 6, PExtraData(SST.GetEntry(pSSTEntry)).PosInTable);
  inherited;
end;

function TLabelSSTRecord.DoCopyTo: TBaseRecord;
begin
  Result:= inherited DoCopyTo;
  (Result as TLabelSSTRecord).SST:= SST;
  (Result as TLabelSSTRecord).pSSTEntry:= pSSTEntry;
  AddSSTRef(SST.GetEntry((Result as TLabelSSTRecord).pSSTEntry));

end;

function TLabelSSTRecord.GetValue: Variant;
begin
  Result:=GetAsString;
end;

procedure TLabelSSTRecord.SetValue(const Value: Variant);
begin
  SetAsString(Value);
end;

function TLabelSSTRecord.GetAsString: UTF16String;
var
  RTFRuns: TRTFRunList;
begin
  Result:=GetSSTValue(SST.GetEntry(pSSTEntry), RTFRuns);
end;

procedure TLabelSSTRecord.SetAsString(const Value: UTF16String);
var
  OldpSSTEntry: TiSSTEntry;
begin
  OldpSSTEntry:=pSSTEntry;
  pSSTEntry:= SST[SST.AddString(Value, nil)];
  if OldpSSTEntry>=0 then DecSSTRef(SST.GetEntry(OldpSSTEntry));
end;

function TLabelSSTRecord.GetAsRichString: TRichString;
begin
  Result.Value:=GetSSTValue(SST.GetEntry(pSSTEntry), Result.RTFRuns);
end;

procedure TLabelSSTRecord.SetAsRichString(const Value: TRichString);
var
  OldpSSTEntry: TiSSTEntry;
begin
  OldpSSTEntry:=pSSTEntry;
  pSSTEntry:= SST[SST.AddString(Value.Value, Value.RTFRuns)];
  if OldpSSTEntry>=0 then DecSSTRef(SST.GetEntry(OldpSSTEntry));
end;

constructor TLabelSSTRecord.CreateFromData(const aRow, aCol, aXF: word; const aSST: TSST);
begin
  inherited CreateFromData(xlr_LABELSST, 10, aRow, aCol, aXF);
  SST:=aSST;
  pSSTEntry:=-1;
end;


function TLabelSSTRecord.GetAsRTF: UTF16String;
begin
  //Todo:
end;

procedure TLabelSSTRecord.SetAsRTF(const Value: UTF16String);
//var
//  OldpSSTEntry: TiSSTEntry;
begin
{TODO:
  OldpSSTEntry:=pSSTEntry;
  pSSTEntry:= SST[SST.AddString(Value)];
  if OldpSSTEntry>=0 then DecSSTRef(SST.GetEntry(OldpSSTEntry));
}
end;


{ TLabelRecord }

function TLabelRecord.GetValue: Variant;
var
  XS: TExcelString;
  MySelf: TBaseRecord;
  MyOfs: integer;
begin
  MySelf:=Self;
  MyOfs:=6;
  XS:=TExcelString.Create(2, Myself, MyOfs);
  try
    Result:= XS.Value;
  finally
    FreeAndNil(XS);
  end;
end;

{ TRStringRecord }

function TRStringRecord.GetAsRichString: TRichString;
var
  XS: TExcelString;
  MySelf: TBaseRecord;
  MyOfs: integer;
  d: array[0..1] of byte;
  i: integer;
begin
  MySelf:=Self;
  MyOfs:=6;
  XS:=TExcelString.Create(2, Myself, MyOfs);
  try
    Result.Value:= XS.Value;
  finally
    FreeAndNil(XS);
  end;

  ReadMem(MySelf, MyOfs, 2, @d);
  SetLength(Result.RTFRuns, GetWord(@d, 0));
  for i := 0 to Length(Result.RTFRuns) - 1 do
  begin
    ReadMem(MySelf, MyOfs, 2, @d);
    Result.RTFRuns[i].FirstChar := GetWord(@d,0);
    ReadMem(MySelf, MyOfs, 2, @d);
    Result.RTFRuns[i].FontIndex := GetWord(@d,0);
  end;

end;

function TRStringRecord.GetValue: Variant;
var
  XS: TExcelString;
  MySelf: TBaseRecord;
  MyOfs: integer;
begin
  MySelf:=Self;
  MyOfs:=6;
  XS:=TExcelString.Create(2, Myself, MyOfs);
  try
    Result:= XS.Value;
  finally
    FreeAndNil(XS);
  end;
end;

end.
