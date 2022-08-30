unit tmsUXlsFormula;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses
  {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF}
  Classes, SysUtils, tmsUXlsBaseRecords, tmsXlsMessages, tmsUXlsTokenArray, tmsXlsFormulaMessages, tmsUFlxMessages,
  tmsUXlsStrings, tmsUOle2Impl;

type
  TTableRecord = class(TBaseRecord)
  private
    procedure IncRowToMax( const Pdata: PArrayOfByte; const rowPos, colPos: integer; const Offset: integer; const Max: integer);
    procedure IncColToMax( const Pdata: PArrayOfByte; const rowPos, colPos: integer; const Offset: integer; const Max: integer);
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer);
    procedure ArrangeCopyRowsAndCols(const DeltaRow, DeltaCol: integer);
  end;

  TArrayRecord=class(TBaseRecord)
  public
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer);
    procedure ArrangeCopyRowsAndCols(const DeltaRow, DeltaCol: integer);
  end;

  TFormulaRecord = class(TCellRecord)
  private
    FormulaValue: variant;
    FTableRecord: TTableRecord;
    FArrayRecord: TArrayRecord;

    procedure ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset, CopyRowOffset,InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
    procedure ArrangeSharedTokens;
    procedure SetTableRecord(const Value: TTableRecord);
    procedure SetArrayRecord(const Value: TArrayRecord);
    procedure ClearResult;
    function SaveStringRecord(const Workbook: TOle2File; const IncludeHeaders: boolean): integer;
  protected
    function DoCopyTo: TBaseRecord; override;

  public

    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateFromData(const aId, aDataSize, aRow, aCol, aXF: word; const aValue: variant; const Options1904: boolean);
    destructor Destroy;override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);override;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);override;
    procedure SaveToStream(const Workbook: TOle2File); override;

    function TotalSize: integer;override;
    function FixTotalSize(const NeedsRecalc: boolean): int64; override;
    function TotalSizeNoHeaders: integer;override;

    property TableRecord: TTableRecord read FTableRecord write SetTableRecord;
    property ArrayRecord: TArrayRecord read FArrayRecord write SetArrayRecord;

    function IsExp(var Key: LongWord): boolean;
    procedure MixShared(const PData: PArrayOfByte; const aDataSize: integer);
    function GetValue: Variant; override;
    procedure SetFormulaValue(const v: variant);
  end;

  TNameRecord =  class (TBaseRecord)
  private
    procedure ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset, CopyRowOffset,InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
    function NameLength: byte;
    function NameSize: integer;
    function NameOptionFlags: byte;

    procedure ChangeRefToArea;
  public
    constructor Create(const aId: word; const aData: PArrayOfByte; const aDataSize: integer);override;
    constructor CreateFromData(const Range: TXlsNamedRange; const Globals: pointer; const CellList: pointer);

    procedure ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
    procedure ArrangeInsertSheets(const FirstSheet, SheetCount: integer);

    function ArrangeCopySheet(const SheetInfo: TSheetInfo): TNameRecord;

    function RangeSheet: integer;
    function RefersToSheet(const GetSheet:TGetSheet) : integer;
    function Name:UTF16String;
    function GetR1: integer;
    function GetR2: integer;
    function GetC1: integer;
    function GetC2: integer;

    procedure SetR1(value: integer);
    procedure SetR2(value: integer);
    procedure SetC1(value: integer);
    procedure SetC2(value: integer);
  end;

  TShrFmlaRecord=class(TBaseRecord)
  public
    Key: LongWord;  //This is the last formula Row+ last cormula col shr 16. Used to know which cell this is attached to.

    //We don't really need to implement a CopyTo, since this record is used temporary only. But, for consistency:
    protected
        function DoCopyTo: TBaseRecord; override;
    public
    function FirstRow: integer;
    function LastRow: integer;
    function FirstCol: integer;
    function LastCol: integer;
  end;


implementation
uses tmsUXlsEncodeFormula, tmsUXlsWorkbookGlobals;

{ TFormulaRecord }

procedure TFormulaRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);
begin
  inherited;
  ArrangeTokensInsertRowsAndCols(aRowPos, aRowCount, 0, aColPos,aColCount,0, SheetInfo);
  if (FTableRecord<>nil) and (SheetInfo.FormulaSheet=SheetInfo.InsSheet) then FTableRecord.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount);
  if (FArrayRecord<>nil) and (SheetInfo.FormulaSheet=SheetInfo.InsSheet) then FArrayRecord.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount);
end;

constructor TFormulaRecord.Create(const aId: word;
  const aData: PArrayOfByte; const aDataSize: integer);
var
  d: double;
  b: byte;
begin
  inherited;
  ArrayRecord:=nil;
  //Save the formula result
  FormulaValue:=unassigned;
  if GetWord(Data,12)<> $FFFF then //it's a number
  begin
    move(Data[6], d, sizeof(d));
    FormulaValue:=d;
  end else
  begin
    case Data[6] of
      0: FormulaValue:=''; //It's a string. We will fill it later when we read the string record
      1: FormulaValue:=data[8]=1; //boolean
      //2 is error. we can't codify this on a variant.
      2:
      begin
           b:= Data[8];
           if b= fmiErrNull  then FormulaValue:=fmErrNull else
           if b= fmiErrDiv0  then FormulaValue:=fmErrDiv0 else
           if b= fmiErrValue then FormulaValue:=fmErrValue else
           if b= fmiErrRef   then FormulaValue:=fmErrRef else
           if b= fmiErrName  then FormulaValue:=fmErrName else
           if b= fmiErrNum   then FormulaValue:=fmErrNum else
           if b= fmiErrNA    then FormulaValue:=fmErrNA;
      end;
    end; //case
  end;

  // For automatic recalc on Excel97...
  Data^[14]:=Data^[14] or 2;
end;

constructor TFormulaRecord.CreateFromData(const aId, aDataSize, aRow, aCol, aXF: word; const aValue: variant; const Options1904: boolean);
var
  d: double;
begin
  inherited CreateFromData(aId, aDataSize, aRow, aCol, aXF);

  FormulaValue:=unassigned;
  case VarType(aValue) of
    varEmpty,
    varNull      : begin FormulaValue:=unassigned; end;

    varByte,
    varSmallint,
    varInteger,
    varSingle,
    varDouble,
    VarDate,
    {$IFDEF FLX_HASCUSTOMVARIANTS}
      varShortInt, VarWord, VarLongWord, varInt64,
    {$ENDIF} //Delphi 6 or above
    varCurrency :
    begin
      d:= aValue;
      if Options1904 and (VarType(aValue) = varDate) then d := d - Date1904Diff;

      move(d, Data[6],  sizeof(d));
      FormulaValue:=d;
    end;

    varOleStr,
    varStrArg,
    {$IFDEF DELPHI2008UP}
    varUString,
    {$ENDIF}
    varString   :
    begin
      Data[6]:=0;
      SetWord(Data, 12, $FFFF);
      //need to create a string record.
      FormulaValue:=aValue;
    end;

    varBoolean	:
    begin
      Data[6]:=1;
      Data[7]:=0;
      if aValue then Data[8]:=1 else Data[8]:=0;
      //no need to set 0s really. Formula result has been cleared on inherited constructor.
      SetWord(Data, 12, $FFFF);

      FormulaValue:=aValue;
    end;
  end; //case

 end;

procedure TFormulaRecord.ClearResult;
begin
  FillChar(Data^[6],8,0); //clear result
  Data^[6]:=2; //error value
  SetWord(Data,12,$FFFF);
  FillChar(Data^[16],4,0); //clear chn

  FormulaValue := unassigned;
end;

procedure TFormulaRecord.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
const
  SheetInfo: TSheetInfo=(InsSheet:-1;FormulaSheet:-1;GetSheet:nil;SetSheet:nil;Names:nil);
begin
  ArrangeTokensInsertRowsAndCols( 0, 0, RowOffset, 0, 0, ColOffset, SheetInfo); //Sheet info doesn't have meaninig on copy
  if (FTableRecord<>nil) and (SheetInfo.FormulaSheet=SheetInfo.InsSheet) then FTableRecord.ArrangeCopyRowsAndCols(RowOffset, ColOffset);
  if (FArrayRecord<>nil) and (SheetInfo.FormulaSheet=SheetInfo.InsSheet) then FArrayRecord.ArrangeCopyRowsAndCols(RowOffset, ColOffset);
  inherited;   //should be last, so we dont modify Row or Col
end;

procedure TFormulaRecord.ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset,
  CopyRowOffset, InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
begin
  try
    UXlsTokenArray_ArrangeInsertRowsAndCols(Data, 22, 22+GetWord(Data,20), InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, true);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadFormula,[ Row+1, Column+1, e.Token]);
    else raise;
  end; //Except
end;

procedure TFormulaRecord.ArrangeSharedTokens;
begin
  try
    UXlsTokenArray_ArrangeSharedFormulas(Data, 22, 22+GetWord(Data,20), Row, Column);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadFormula,[ Row+1, Column+1, e.Token]);
    else raise;
  end; //Except
end;

function TFormulaRecord.IsExp(var Key: LongWord): boolean;
begin
  Result:= (DataSize=27) and (GetWord(Data,20)=5) and (Data[22]=1);
  if Result then Key:=GetWord(Data,23) or (GetWord(Data,25) shl 16);
end;

procedure TFormulaRecord.MixShared(const PData: PArrayOfByte; const aDataSize: integer);
var
  NewDataSize: integer;
begin
  //Important: This method changes the size of the record without notifying it's parent list
  //It's necessary to adapt the Totalsize in the parent list.
  NewDataSize:=20+aDataSize-8;  //DataSize - 5+ aDataSize-8 ;
  ReallocMem(Data, NewDataSize);
  //Now is safe to change DataSize
  DataSize:=NewDataSize;
  Move(PData[8], Data[20], aDataSize-8);
  ArrangeSharedTokens;
end;

function TFormulaRecord.GetValue: Variant;
begin
  Result:=FormulaValue;
end;

function TFormulaRecord.SaveStringRecord(const Workbook: TOle2File; const IncludeHeaders: boolean): integer;
var
  xs: TExcelString;
  sdata: PArrayOfByte;
  posi, TotalSize, FinalSize: integer;
begin
  Result := 0;
  if (GetWord(Data,12)<> $FFFF) or (Data[6] <> 0) or VarIsClear(FormulaValue) then exit;

  xs:=TExcelString.Create(2, FormulaValue);
  try
    TotalSize := xs.TotalSize;
    GetMem(sData, TotalSize);
    try
      xs.CopyToPtr(sdata, 0);

      if TotalSize > MaxRecordDataSize - 4 then FinalSize := MaxRecordDataSize - 4 else FinalSize := TotalSize;

      if IncludeHeaders then inc(Result, 2 + 2);
      inc(Result, FinalSize);
      if Workbook <> nil then
      begin
        Workbook.Write16(xlr_STRING);
        Workbook.Write16(FinalSize);
        Workbook.WriteMem(sdata[0], FinalSize);
      end;

      Posi := FinalSize;
      while posi < TotalSize do
      begin
        if TotalSize -Posi > MaxRecordDataSize - 4 - 1 then FinalSize := MaxRecordDataSize - 4 - 1 else FinalSize := TotalSize - posi;

        if IncludeHeaders then inc(Result, 2 + 2);
        inc(Result, 1 + FinalSize);
        if Workbook <> nil then
        begin
          Workbook.Write16(xlr_CONTINUE);
          Workbook.Write16(FinalSize + 1);
          Workbook.WriteMem(sData[2], 1); //option flags.
          Workbook.WriteMem(sdata[posi], FinalSize);
        end;

        inc(posi, MaxRecordDataSize - 4 - 1); //4 is for the record header, 1 for the optionflags.
      end;


    finally
      FreeMem(sData);
    end;
  finally
    freeAndNil(xs);
  end;
end;

procedure TFormulaRecord.SaveToStream(const Workbook: TOle2File);
begin
  inherited;
  if FArrayRecord<>nil then FArrayRecord.SaveToStream(Workbook);
  if FTableRecord<>nil then FTableRecord.SaveToStream(Workbook);
  SaveStringRecord(Workbook, true);
end;

procedure TFormulaRecord.SetFormulaValue(const v: variant);
begin
  FormulaValue:=v;
end;

function TFormulaRecord.DoCopyTo: TBaseRecord;
begin
  Result:=inherited DoCopyTo;
  (Result as TFormulaRecord).TableRecord:= (FTableRecord.CopyTo as TTableRecord);
  (Result as TFormulaRecord).ArrayRecord:= (FArrayRecord.CopyTo as TArrayRecord);
  (Result as TFormulaRecord).ClearResult;
end;

function TFormulaRecord.FixTotalSize(const NeedsRecalc: boolean): int64;
begin
  if (NeedsRecalc) then ClearResult;

  Result:=inherited TotalSize;
  if FTableRecord<>nil then inc(Result, FTableRecord.TotalSize);
  if FArrayRecord<>nil then inc(Result, FArrayRecord.TotalSize);
  inc (Result, SaveStringRecord(nil, true));

end;

function TFormulaRecord.TotalSize: integer;
begin
  Result := FixTotalSize(false);
end;

function TFormulaRecord.TotalSizeNoHeaders: integer;
begin
  Result:=inherited TotalSizeNoHeaders;
  if FTableRecord<>nil then inc(Result, FTableRecord.TotalSizeNoHeaders);
  if FArrayRecord<>nil then inc(Result, FArrayRecord.TotalSizeNoHeaders);
  inc (Result, SaveStringRecord(nil, false));
end;

destructor TFormulaRecord.Destroy;
begin
  FreeAndNil(FTableRecord);
  FreeAndNil(FArrayRecord);
  inherited;
end;

procedure TFormulaRecord.SetTableRecord(const Value: TTableRecord);
begin
  if FTableRecord<>nil then FreeAndNil(FTableRecord);
  FTableRecord := Value;
end;

procedure TFormulaRecord.SetArrayRecord(const Value: TArrayRecord);
begin
  if FArrayRecord<>nil then FreeAndNil(FArrayRecord);
  FArrayRecord := Value;
end;


{ TNameRecord }

procedure TNameRecord.ArrangeInsertSheets(const FirstSheet, SheetCount: integer);
begin
  if (RangeSheet<>$FFFF) and (RangeSheet>=FirstSheet) then IncWord(Data, 8, SheetCount, MaxSheets+1); //NewSheet is 0 based, Data[8] is one-based;
end;

procedure TNameRecord.ArrangeTokensInsertRowsAndCols(const InsRowPos, InsRowOffset,
  CopyRowOffset, InsColPos, InsColOffset, CopyColOffset: integer; const SheetInfo: TSheetInfo);
begin
  try
    UXlsTokenArray_ArrangeInsertRowsAndCols(Data, 14+ NameSize,14+ NameSize+GetWord(Data,4), InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, true);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadName,[ Name, e.Token]);
    else raise;
  end; //Except
end;

constructor TNameRecord.Create(const aId: word; const aData: PArrayOfByte;
  const aDataSize: integer);
begin
  inherited;

end;

procedure TNameRecord.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo);
begin
  ArrangeTokensInsertRowsAndCols( aRowPos, aRowCount, 0, aColPos, aColCount, 0, SheetInfo);
end;

function TNameRecord.Name: UTF16String;
var
  s: AnsiString;
begin
  if (NameOptionFlags and 1)=1 then
  begin
    SetLength(Result, NameLength);
    Move(Data[15], Result[1], NameLength*2);
  end else
  begin
    SetLength(s, NameLength);
    Move(Data[15], s[1], NameLength);
    Result:= StringToWideStringNoCodePage(s);
  end;
end;

function TNameRecord.NameLength: byte;
begin
  Result:= Data[3];
end;

function TNameRecord.NameSize: integer;
begin
  Result:= GetStrLen(false , Data, 14, true, NameLength);
end;

function TNameRecord.NameOptionFlags: byte;
begin
  Result:= Data[14];
end;

function TNameRecord.RangeSheet: integer;
begin
  Result:=GetWord(Data,8)-1;
end;

function TNameRecord.ArrangeCopySheet(const SheetInfo: TSheetInfo): TNameRecord;
begin
  try
    UXlsTokenArray_ArrangeInsertSheets(Data, 14+ NameSize,14+ NameSize+GetWord(Data,4), SheetInfo);
  except
    on e: ETokenException do raise Exception.CreateFmt(ErrBadName,[ Name, e.Token]);
    else raise;
  end; //Except

  SetWord(Data, 8, SheetInfo.InsSheet+1); //InsSheet is 0 based, Data[8] is one-based;
  Result:=Self;
end;

function TNameRecord.GetR1: integer;
begin
  if GetWord(Data,4)<=0 then Result:=-1
  else if Data[14+ NameSize] in tk_Area3d then Result:= GetWord(Data, 15+2+NameSize)
  else if Data[14+ NameSize] in tk_Ref3d then Result:= GetWord(Data, 15+2+NameSize)
  else Result:=-1;
end;

function TNameRecord.GetR2: integer;
begin
  if GetWord(Data,4)<=0 then Result:=-1
  else if Data[14+ NameSize] in tk_Area3d then Result:= GetWord(Data, 15+4+NameSize)
  else if Data[14+ NameSize] in tk_Ref3d then Result:= GetWord(Data, 15+2+NameSize)
  else Result:=-1;
end;

function TNameRecord.RefersToSheet(const GetSheet:TGetSheet): integer;
begin
  if GetWord(Data,4)<=0 then Result:=-1
  else if Data[14+ NameSize] in tk_Area3d then Result:= GetSheet(GetWord(Data, 15+NameSize))
  else if Data[14+ NameSize] in tk_Ref3d then Result:= GetSheet(GetWord(Data, 15+NameSize))
  else Result:=-1;
end;


function TNameRecord.GetC1: integer;
begin
  if GetWord(Data,4)<=0 then Result:=-1
  else if Data[14+ NameSize] in tk_Area3d then Result:= GetWord(Data, 15+6+NameSize) and $FF
  else if Data[14+ NameSize] in tk_Ref3d then Result:= GetWord(Data, 15+4+NameSize) and $FF
  else Result:=-1;
end;

function TNameRecord.GetC2: integer;
begin
  if GetWord(Data,4)<=0 then Result:=-1
  else if Data[14+ NameSize] in tk_Area3d then Result:= GetWord(Data, 15+8+NameSize) and $FF
  else if Data[14+ NameSize] in tk_Ref3d then Result:= GetWord(Data, 15+4+NameSize) and $FF
  else Result:=-1;
end;


procedure TNameRecord.SetC1(value: integer);
begin
  if GetWord(Data,4)<=0 then exit;
  if Data[14+ NameSize] in tk_Ref3d then ChangeRefToArea;
  if Data[14+ NameSize] in tk_Area3d then SetWord(Data, 15+6+NameSize, value and $FF)
end;

procedure TNameRecord.SetC2(value: integer);
begin
  if GetWord(Data,4)<=0 then exit;
  if Data[14+ NameSize] in tk_Ref3d then ChangeRefToArea;
  if Data[14+ NameSize] in tk_Area3d then SetWord(Data, 15+8+NameSize, value and $FF)
end;

procedure TNameRecord.SetR1(value: integer);
begin
  if GetWord(Data,4)<=0 then exit;
  if Data[14+ NameSize] in tk_Ref3d then ChangeRefToArea;
  if Data[14+ NameSize] in tk_Area3d then SetWord(Data, 15+2+NameSize, value and $FFFF)
end;

procedure TNameRecord.SetR2(value: integer);
begin
  if GetWord(Data,4)<=0 then exit;
  if Data[14+ NameSize] in tk_Ref3d then ChangeRefToArea;
  if Data[14+ NameSize] in tk_Area3d then SetWord(Data, 15+4+NameSize, value and $FFFF)
end;

procedure TNameRecord.ChangeRefToArea;
var
  NewDataSize: integer;
begin
  //Important: This method changes the size of the record without notifying it's parent list
  //It's necessary to adapt the Totalsize in the parent list.
  NewDataSize:=DataSize+4;
  ReallocMem(Data, NewDataSize);
  DataSize:=NewDataSize;

  Data[14+ NameSize]:=Data[14+ NameSize]+1; //Convert to area.
  SetWord(Data, 4, GetWord(Data, 4)+4); //Length of name record.
  System.Move(Data[14+NameSize+6], Data[14+NameSize+10], DataSize-14-NameSize-10);
  SetWord(Data, 14+NameSize+6, GetWord(Data, 14+NameSize+4));
  SetWord(Data, 14+NameSize+8, GetWord(Data, 14+NameSize+4));
  SetWord(Data, 14+NameSize+4, GetWord(Data, 14+NameSize+2));
end;

constructor TNameRecord.CreateFromData(const Range: TXlsNamedRange;
  const Globals: pointer; const CellList: pointer);
var
  es: TExcelString;
  Fmla: array of byte;
  DefaultSheet: integer;
  DefaultSheetName: UTF16String;
  Ps: TParseString;
  sht: integer;
begin
  Create(xlr_NAME, nil, 0);
  es := TExcelString.Create(1, Range.Name);
  try
    Fmla := nil;
    if (Length(Trim(Range.RangeFormula)) <= 0) then exit;

    DefaultSheet := 0;
    if Range.NameSheetIndex >= 0 then
      DefaultSheet := Range.NameSheetIndex;

    DefaultSheetName := TWorkbookGlobals(Globals).SheetName[DefaultSheet];
    Ps := TParseString.CreateExt(Range.RangeFormula, CellList, true, DefaultSheetName, fmRef);
    try
      Ps.Parse;
      SetLength(Fmla, Ps.TotalSize - 2);
      Ps.CopyToPtrNoLen(PArrayOfByte(Fmla), 0);
    finally
      FreeAndNil(Ps);
    end; //finally

    DataSize := ((es.TotalSize - 1) + 14) + Length(Fmla);
    GetMem (Data, DataSize);
    FillChar(Data[0], DataSize, 0);
    SetWord(Data, 0, Range.OptionFlags and 65535);
    Data[3] := Byte(Length(Range.Name));
    SetWord(Data, 4, Length(Fmla));
    if Range.NameSheetIndex >= 0 then
    begin
      sht := Range.NameSheetIndex + 1; // SetSheet(Range.NameSheetIndex)+1; .Despite what the docs say, this is the sheetindex+1 not the externsheetindex.
      SetWord(Data, 6, sht);
      SetWord(Data, 8, sht);
    end;

    es.CopyToPtr(Data, 14, false);
    move(Fmla[0], Data[(14 + es.TotalSize) - 1], Length(Fmla));
  finally
    FreeAndNil(es);
  end;
end;

{ TShrFmlaRecord }
function TShrFmlaRecord.FirstRow: integer;
begin
  Result:=GetWord(Data,0);
end;

function TShrFmlaRecord.LastRow: integer;
begin
  Result:=GetWord(Data,2);
end;

function TShrFmlaRecord.FirstCol: integer;
begin
  Result:=Data[4];
end;

function TShrFmlaRecord.LastCol: integer;
begin
  Result:=Data[5];
end;

function TShrFmlaRecord.DoCopyTo: TBaseRecord;
begin
  Result:=inherited DoCopyTo;
  (Result as TShrFmlaRecord).Key:=Key;
end;


{ TTableRecord }

procedure TTableRecord.ArrangeCopyRowsAndCols(const DeltaRow, DeltaCol: integer);
begin
  if (GetWord(Data,0) <>$FFFF) then IncWord(Data, 0, DeltaRow, Max_Rows); //Here we raise an error, can't insert past the bound of a sheet.
  if (GetWord(Data,2) <>$FFFF) then IncWord(Data, 2, DeltaRow, Max_Rows);
  if (Data[4] <>$FF) then IncWord(Data, 4, DeltaCol, Max_Columns);
  if (Data[5] <>$FF) then IncWord(Data, 5, DeltaCol, Max_Columns);

  if (GetWord(Data,8) <>$FFFF) then IncRowToMax(Data, 8, 10, DeltaRow, Max_Rows);  //here, we create an invalid ref
  if (GetWord(Data,12) <>$FFFF) then IncRowToMax(Data, 12, 14, DeltaRow, Max_Rows);
  if (Data[10] <>$FF) then IncColToMax(Data, 8, 10, DeltaCol, Max_Columns);
  if (Data[14] <>$FF) then IncColToMax(Data, 12, 14, DeltaCol, Max_Columns);
end;

procedure TTableRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer);
begin
  //Increment the position of the table. Here we give an error if we pass the maximum value, or wi would be loosing data
  if (GetWord(Data,0) >=aRowPos)and (GetWord(Data,0) <>$FFFF) then IncWord(Data, 0, aRowCount, Max_Rows);
  if (GetWord(Data,2) >=aRowPos)and (GetWord(Data,2) <>$FFFF) then IncWord(Data, 2, aRowCount, Max_Rows);
  if (Data[4] >=aColPos) and (Data[4] <>$FF) then IncByte(Data, 4, aColCount, Max_Columns);
  if (Data[5] >=aColPos) and (Data[5] <>$FF) then IncByte(Data, 5, aColCount, Max_Columns);

  //Increment the entry cells. If they go out of limits, we should replace them with #ref
  if (GetWord(Data,8) >=aRowPos)and (GetWord(Data,8) <>$FFFF) then IncRowToMax(Data, 8, 10, aRowCount, Max_Rows);
  if (GetWord(Data,12) >=aRowPos)and (GetWord(Data,12) <>$FFFF) then IncRowToMax(Data, 12, 14, aRowCount, Max_Rows);
  if (Data[10] >=aColPos) and (Data[10] <>$FF) then IncColToMax(Data, 8, 10, aColCount, Max_Columns);
  if (Data[14] >=aColPos) and (Data[14] <>$FF) then IncColToMax(Data, 12, 14, aColCount, Max_Columns);
end;

constructor TTableRecord.Create(const aId: word; const aData: PArrayOfByte;
  const aDataSize: integer);
begin
  inherited;
  SetWord(Data, 6, GetWord(Data, 6) or 3); // Calc on load...
end;

procedure TTableRecord.IncColToMax(const Pdata: PArrayOfByte; const rowPos, colPos,
  Offset, Max: integer);
var
  v: int64;
begin
  v:=Pdata[colPos];
  v:=v+Offset;
  if (v>Max) or (v<0) then begin; SetWord(PData,rowPos,$FFFF); SetWord(PData,colPos,$FFFF); end  //Invalid ref
  else
  begin
    Pdata[colPos]:=v;
  end;
end;

procedure TTableRecord.IncRowToMax(const Pdata: PArrayOfByte; const rowPos, colPos,
  Offset, Max: integer);
var
  v: int64;
begin
  v:=GetWord(Pdata,rowPos);
  v:=v+Offset;
  if (v>Max) or (v<0) then begin; SetWord(PData,rowPos,$FFFF); SetWord(PData,colPos,$FFFF); end  //Invalid ref
    else SetWord(Pdata,RowPos,v);
end;

{ TArrayRecord }

procedure TArrayRecord.ArrangeCopyRowsAndCols(const DeltaRow, DeltaCol: integer);
begin
  //Pending:
end;

procedure TArrayRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer);
begin
  //Pending:
end;

end.

