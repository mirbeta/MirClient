unit tmsUXlsTokenArray;
{$INCLUDE ..\FLXCOMPILER.INC}

interface

uses tmsXlsMessages, SysUtils, Math;

type
  ETokenException= class (EExcelException)
  public
    Token: integer;
    constructor Create(const aToken: integer;const aDummy1: integer; const ADummy2: integer);
  end;


//*************************************************************
{**} procedure UXlsTokenArray_ArrangeInsertRowsAndCols(const Data: PArrayOfByte; const atPos, afPos: integer;
                            const InsRowPos, InsRowOffset, CopyRowOffset: integer;
                            const InsColPos, InsColOffset, CopyColOffset: integer;
                            const SheetInfo: TSheetInfo; const AllowedAbsolute: boolean);

{**} procedure UXlsTokenArray_ArrangeInsertSheets(const Data: PArrayOfByte; const atPos, afPos: integer;
                                   const SheetInfo: TSheetInfo);
{**} procedure UXlsTokenArray_ArrangeSharedFormulas(const Data: PArrayOfByte; const atPos, afPos: integer;
                              const SharedRow, SharedCol: integer);
//*************************************************************

implementation
uses tmsUXlsFormula, tmsUXlsBaseRecordLists;
procedure CreateInvalidRef(var Token: byte);
begin
  if Token in tk_Ref then Inc(Token, tk_RefToRefErr) else
  if Token in tk_Area then Inc(Token, tk_AreaToAreaErr) else
  if Token in tk_Ref3D then Inc(Token, tk_Ref3DToRef3DErr) else
  if Token in tk_Area3D then Inc(Token, tk_Area3DToArea3DErr) else
  if not (Token in tk_RefErr + tk_Ref3DErr + tk_AreaErr + tk_Area3DErr)
    then raise ETokenException.Create(Token,0,0);
end;

procedure IncWordRef( const Pdata: PArrayOfByte; const tPos: integer; const InsPos, Offset: integer; const Max: integer; const tkPos: integer; const CheckInside: boolean);
var
  w: int64;
begin
  w:=Pdata^[tPos] or (PData^[tPos+1] shl 8);

  //Handle deletes...
  if CheckInside and (Offset<0) and (InsPos >=0) and (w>= InsPos) and (w<Inspos - Offset) then
  begin
    CreateInvalidRef( PData[tkPos]);
    exit;
  end;

  inc(w, Offset);

  if (w<0) or (w>Max) then
  begin
    CreateInvalidRef( PData[tkPos]);
    exit;
  end;

  Pdata^[tPos]:= byte(w);
  Pdata^[tPos+1]:= hi(word(w));
end;


procedure IncByteRef( const Pdata: PArrayOfByte; const tPos: integer; const InsPos, Offset: integer; const Max: integer; const tkPos: integer; const CheckInside: boolean);
var
  w: int64;
begin
  w:=Pdata^[tPos];

  //Handle deletes...
  if CheckInside and (Offset<0) and (InsPos >=0) and (w>= InsPos) and (w<Inspos - Offset) then
  begin
    CreateInvalidRef( PData[tkPos]);
    exit;
  end;

  inc(w, Offset);

  if (w<0) or (w>Max) then
  begin
    CreateInvalidRef( PData[tkPos]);
    exit;
  end;

  Pdata^[tPos]:= w;
end;

procedure IncW(const pData: PArrayOfByte; const tPos: integer; const Offset: integer);
var
  w: ^Word;
begin
  w:=@(PData[tPos]);
  inc(w^, Offset);
end;

procedure DeleteRowsArea(const tPos: Integer; const InsRowPos, InsRowOffset, r1, r2: integer;
                         const Data: PArrayOfByte; const tkPos: Integer);
begin
  if (r1 >= InsRowPos) then
    if (r2 < InsRowPos - InsRowOffset) then //range is all inside the deleted range
      CreateInvalidRef(Data[tkPos])
    else
    begin
      //Do NOT DELETE full columns sum(a:a). should always remain this. (Except when deleting the full range on another sheet);
      //A funny one: Sum(a65536:a65536), when deleting a row, will convert to sum(a65535:65536). Only second row will wrap.
      IncWordRef(Data, tPos, InsRowPos, Max((InsRowPos - r1), (InsRowOffset)), Max_Rows, tkPos, false);
      if (r2<Max_Rows) then
        IncWordRef(Data, tPos + 2, InsRowPos, InsRowOffset, Max_Rows, tkPos, false);
    end
  else
  begin
    if (r2>=Max_Rows) then exit;
    if (r2 >= InsRowPos) then
      IncWordRef(Data, tPos + 2, InsRowPos, Max((InsRowPos - r2) - 1, InsRowOffset), Max_Rows, tkPos, false);
  end;
end;

procedure DeleteColsArea(const tPos: Integer; const InsRowPos, InsRowOffset, r1, r2: integer;
                         const Data: PArrayOfByte; const tkPos: Integer);
begin
  //Not implemented yet, until we adapt this to ranges.
end;


procedure ArrangeTokenOperand(Token: integer;const Data: ParrayOfByte;
                              var tPos: integer;
                              const InsRowPos, InsRowOffset, CopyRowOffset: integer;
                              const InsColPos, InsColOffset, CopyColOffset: integer;
                              const SheetInfo: TSheetInfo; const InsertingSheet: boolean;
                              const SharedRow, SharedCol: integer; const AllowedAbsolute: boolean);
var
  AbsoluteRowRef, AbsoluteColRef: boolean;
  LocalSheet: integer;
  tkPos: integer;
  Name, NewName: TNameRecord;
  NameList: TNameRecordList;
  i: integer;
begin
  // Shared formulas
  if (SharedRow>=0) and (Token in tk_RefN+ tk_AreaN) then
  begin
    Inc(Data[tPos],$24-$2C);
    Token:=Data[tPos];
  end;

  tkPos:=tPos;
  inc(tPos);
  LocalSheet:= SheetInfo.FormulaSheet;

  if Token in tk_Ref3D+ tk_Area3D+ tk_Ref3DErr + tk_Area3DErr then
  begin
    if Assigned(SheetInfo.GetSheet) then
    begin
      LocalSheet:=SheetInfo.GetSheet(GetWord(Data, tPos));
      if InsertingSheet and (LocalSheet=SheetInfo.FormulaSheet) and Assigned(SheetInfo.SetSheet) then
        SetWord(Data, tPos, SheetInfo.SetSheet(SheetInfo.InsSheet));        //this copies external refs to the old sheet to the new sheet
    end;
    inc(tPos, 2);
  end;


  if Token in tk_Array then
  begin
    inc(tPos,7);
    //Values are stored outside the formula
  end
  else if (Token in tk_Name) or (Token in tk_NameX) then
  begin
    if (Token in tk_NameX) then inc(tPos, 2);

    if (SheetInfo.Names <> nil) then
    begin
      NameList := SheetInfo.Names as TNameRecordList;
      Name := NameList[GetWord(Data, tPos) - 1];
      LocalSheet:=Name.RangeSheet;
      if InsertingSheet and ((LocalSheet=SheetInfo.FormulaSheet) or (LocalSheet<0)) then
      begin
        for i :=0 to NameList.Count - 1 do
        begin
          NewName := NameList[i];
          if (NewName.RangeSheet = SheetInfo.InsSheet) and (NewName.Name = Name.Name) then
          begin
            SetWord(Data, tPos, i+1);
            break;
          end;
        end;
      end;
    end;

    inc(tPos, 4);
  end
  else if Token in tk_Ref + tk_RefErr + tk_Ref3D + tk_Ref3DErr then
  begin
    if (LocalSheet= SheetInfo.InsSheet) then
    begin
      if GetWord(Data, tPos)>= InsRowPos then IncWordRef(Data, tPos, InsRowPos, InsRowOffset, Max_Rows, tkpos, true);
      if Data[tPos+2]>= InsColPos then IncByteRef(Data, tPos+2, InsColPos, InsColOffset, Max_Columns, tkpos, true);
    end;
    AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+2) and $8000) <> $8000);
    if not AbsoluteRowRef then IncWordRef(Data, tPos, -1, CopyRowOffset, Max_Rows, tkpos, true);
    AbsoluteColRef:= AllowedAbsolute and ((GetWord(Data, tPos+2) and $4000) <> $4000);
    if not AbsoluteColRef then IncByteRef(Data, tPos+2, -1, CopyColOffset, Max_Columns, tkpos, true);

    if SharedRow>=0 then
    begin
      AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+2) and $8000) <> $8000);
      if not AbsoluteRowRef then IncW(Data, tPos,   SharedRow);
      AbsoluteColRef:= AllowedAbsolute and ((GetWord(Data, tPos+2) and $4000) <> $4000);
      if not AbsoluteColRef then IncW(Data, tPos+2, SharedCol);
    end;

    inc(tPos,4);
  end
  else if Token in tk_RefN then inc(tPos,4)//This is used in conditional formats, besides shared formulas

  else if Token in tk_Area + tk_AreaErr + tk_Area3D + tk_Area3DErr then
  begin
    if (LocalSheet= SheetInfo.InsSheet) then
    begin
      if InsRowOffset<0 then
        DeleteRowsArea(tpos, InsRowPos, InsRowOffset, GetWord(Data, tPos), GetWord(Data, tPos+2), Data, tkpos)
      else
      begin
        if GetWord(Data, tPos)>= InsRowPos then IncWordRef(Data, tPos, InsRowPos, InsRowOffset, Max_Rows, tkpos, true);
        if (GetWord(Data, tPos+2)>= InsRowPos) and (GetWord(Data, tPos+2)<>Max_Rows) then IncWordRef(Data, tPos+2, InsRowPos, InsRowOffset, Max_Rows, tkpos, true);
      end;

      if InsColOffset<0 then
        DeleteColsArea(tpos+4, InsColPos, InsColOffset, GetWord(Data, tPos+4), GetWord(Data, tPos+6), Data, tkpos)
      else
      begin
        if Data[tPos+4]>= InsColPos then IncByteRef(Data, tPos+4, InsColPos, InsColOffset, Max_Columns, tkpos, true);
        if (Data[tPos+6]>= InsColPos) and (Data[tPos+6]<>Max_Columns) then IncByteRef(Data, tPos+6, InsColPos, InsColOffset, Max_Columns, tkpos, true);
      end;
    end;

    AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+4) and $8000) <> $8000);
    if not AbsoluteRowRef then IncWordRef(Data, tPos, -1, CopyRowOffset, Max_Rows, tkpos, true);
    AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+6) and $8000) <> $8000);
    if not AbsoluteRowRef then IncWordRef(Data, tPos+2, -1, CopyRowOffset, Max_Rows, tkpos, true);

    AbsoluteColRef:= AllowedAbsolute and ((GetWord(Data, tPos+4) and $4000) <> $4000);
    if not AbsoluteColRef then IncByteRef(Data, tPos+4, -1, CopyColOffset, Max_Columns, tkpos, true);
    AbsoluteColRef:= AllowedAbsolute and ((GetWord(Data, tPos+6) and $4000) <> $4000);
    if not AbsoluteColRef then IncByteRef(Data, tPos+6, -1, CopyColOffset, Max_Columns, tkpos, true);

    if SharedRow>=0 then
    begin
      AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+4) and $8000) <> $8000);
      if not AbsoluteRowRef then IncW(Data, tPos, SharedRow);
      AbsolutecolRef:= AllowedAbsolute and ((GetWord(Data, tPos+4) and $4000) <> $4000);
      if not AbsoluteColRef then IncW(Data, tPos+4, SharedCol);

      AbsoluteRowRef:= AllowedAbsolute and ((GetWord(Data, tPos+6) and $8000) <> $8000);
      if not AbsoluteRowRef then IncW(Data, tPos+2, SharedRow);
      AbsoluteColRef:= AllowedAbsolute and ((GetWord(Data, tPos+6) and $4000) <> $4000);
      if not AbsoluteColRef then IncW(Data, tPos+6, SharedCol);
    end;

    inc(tPos,8);
  end
  else if Token in tk_AreaN then inc(tPos,8)  //PENDING: Arreglar inserts en CFs

  else raise ETokenException.Create(Token,0,0);
end;

procedure ArrangeTokenTableAndArray(const Data: ParrayOfByte;
                              var tPos: integer;
                              const InsRowPos, InsRowOffset, CopyRowOffset: integer;
                              const InsColPos, InsColOffset, CopyColOffset: integer;
                              const SheetInfo: TSheetInfo; const InsertingSheet: boolean;
                              const SharedRow, SharedCol: integer; const AllowedAbsolute: boolean);
begin
  inc(tpos,5);
  if (SheetInfo.FormulaSheet<>SheetInfo.InsSheet)or
     (SharedRow>0) or (SharedCol>0) or InsertingSheet then exit;

  if GetWord(Data, tpos-4)>=InsRowPos then IncWordRef(Data, tPos-4, InsRowPos, InsRowOffset, Max_Rows, tPos-5, true);
  IncWordRef(Data, tPos-4, -1, CopyRowOffset, Max_Rows, tpos-5, true);
  if Data[tPos-2]>=InsColPos then IncByteRef(Data, tPos-2, InsColPos, InsColOffset, Max_Columns, tPos-5, true);
	IncByteRef(Data, tPos-2, -1, CopyColOffset, Max_Columns, tPos-5, true);
end;

procedure ArrangeTokenArray(const Data: PArrayOfByte; const atPos, afPos: integer;
                            const InsRowPos, InsRowOffset, CopyRowOffset: integer;
                            const InsColPos, InsColOffset, CopyColOffset: integer;
                            const SheetInfo: TSheetInfo; const InsertingSheet: boolean;
                            const SharedRow, SharedCol: integer; const AllowedAbsolute: boolean);
var
  tPos, fPos: integer;
  Token: byte;
begin;
  tPos:=atPos;
  fPos:=afPos;

  while tPos<fPos do
  begin
    Token:= Data[tPos];
    if Token in tk_UnaryOps + tk_BinaryOps + [tk_MissArg] then inc(tPos)
    else if Token = tk_Str then inc(tPos,1 + GetStrLen(false, Data,tPos+1, False, 0))
    else if Token in [tk_Err, tk_Bool] then inc(tPos,1+1)
    else if Token in [tk_Int]+ tk_Func then inc(tPos,1+2)
    else if Token in tk_FuncVar then inc(tPos,1+3)
    else if Token in [tk_Num] then inc(tPos,1+8)
    else if Token=tk_Attr then
    begin
      if (Data[tPos+1] and $04)=$04 then inc(tPos, (GetWord(Data, tPos+2)+1)*2);
      inc(tPos, 1+3);
    end

    else if Token in tk_Operand then ArrangeTokenOperand(Token, Data, tPos, InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, InsertingSheet, SharedRow, SharedCol, AllowedAbsolute)
    else if Token=tk_Table then ArrangeTokenTableAndArray(Data, tPos, InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, InsertingSheet, SharedRow, SharedCol, AllowedAbsolute)
    else if Token=tk_MemFunc then inc(tPos, 2+ 1) //GetWord(Data, tPos+1))
    else if Token=tk_ArrayFormula then ArrangeTokenTableAndArray(Data, tPos, InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, InsertingSheet, SharedRow, SharedCol, AllowedAbsolute)
    else raise ETokenException.Create(Token,0,0);

  end;

end;

procedure UXlsTokenArray_ArrangeInsertRowsAndCols(const Data: PArrayOfByte; const atPos, afPos: integer;
                            const InsRowPos, InsRowOffset, CopyRowOffset: integer;
                            const InsColPos, InsColOffset, CopyColOffset: integer;
                            const SheetInfo: TSheetInfo; const AllowedAbsolute: boolean);
begin
  ArrangeTokenArray(Data, atPos, afPos, InsRowPos, InsRowOffset, CopyRowOffset, InsColPos, InsColOffset, CopyColOffset, SheetInfo, false, -1, -1, AllowedAbsolute);
end;

procedure UXlsTokenArray_ArrangeInsertSheets(const Data: PArrayOfByte; const atPos, afPos: integer;
                              const SheetInfo: TSheetInfo);
begin
  ArrangeTokenArray(Data, atPos, afPos, 0, 0, 0, 0, 0, 0, SheetInfo, true, -1, -1, true);
end;

procedure UXlsTokenArray_ArrangeSharedFormulas(const Data: PArrayOfByte; const atPos, afPos: integer;
                              const SharedRow, SharedCol: integer);
var
  SheetInfo: TSheetInfo;
begin
  SheetInfo.InsSheet:=0;
  SheetInfo.FormulaSheet:=0;
  SheetInfo.GetSheet:=nil;
  SheetInfo.SetSheet:=nil;
  SheetInfo.Names:=nil;
  ArrangeTokenArray(Data, atPos, afPos, 0, 0, 0, 0, 0, 0, SheetInfo, false, SharedRow, SharedCol, true);
end;


{ ETokenException }

constructor ETokenException.Create(const aToken: integer;const aDummy1: integer; const ADummy2: integer);
begin
  Token:= aToken;
  inherited CreateFmt(ErrBadToken, [Token]);
end;

end.
