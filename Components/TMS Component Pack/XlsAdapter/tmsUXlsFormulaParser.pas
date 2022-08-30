//This unit doesn't handle continue records.
//I don't know a way to input a formula larger than one record.

unit tmsUXlsFormulaParser;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsXlsMessages, tmsXlsFormulaMessages, tmsUFlxStack, tmsUXlsBaseRecords,
     SysUtils, tmsUXlsBaseRecordLists, tmsUFlxMessages, tmsUXlsReferences, tmsUXlsRowColEntries;
//************************************************************
  function RPNToString(const RPN: PArrayOfByte; const atPos: integer; const CellList: TCellList): UTF16String;
//************************************************************
implementation

//Returns an string token
function GetString(const Length16Bit: boolean; const RPN: PArrayOfByte; const tpos: integer; var sl: integer): UTF16String;
begin
  GetSimpleString(Length16Bit, RPN, tpos, false, 0, Result, sl);
  Result:=StringReplace(Result,'"','""',  [rfReplaceAll]);
end;

function GetErrorText(const Err: byte): UTF16String;
begin
  case Err of
    fmiErrNull: Result:=fmErrNull;
    $07: Result:=fmErrDiv0;
    $0F: Result:=fmErrValue;
    $17: Result:=fmErrRef;
    $1D: Result:=fmErrName;
    $24: Result:=fmErrNum;
    $2A: Result:=fmErrNA;
    else Result:=fmErrUnknown;
  end; //case
end;

function GetBoolText(const Bool: byte): UTF16String;
begin
  if Bool=1 then Result:=fmTrue else Result:=fmFalse;
end;

function GetDouble(const RPN: PArrayOfByte; const tpos: integer): double;
var
  d: double;
begin
  move(RPN[tpos],d,sizeof(d));
  Result:=d;
end;

function ReadCachedValueTxt(const RPN:PArrayOfByte; var ArrayPos: integer):UTF16String;
var
  ValueType: byte;
  sl: integer;
begin
  ValueType:=RPN[ArrayPos];
  inc(ArrayPos);
  case ValueType of
    $01: begin; Result:=FmFloatToStr(GetDouble(RPN, ArrayPos));inc(ArrayPos, SizeOf(Double)); end;
    $02: begin; Result:=fmStr+GetString(true, RPN, ArrayPos, sl)+fmStr; inc(ArrayPos, sl); end;
    $04: begin; Result:=GetBoolText(RPN[ArrayPos]);inc(ArrayPos, 8); end;
    $10: begin; Result:=GetErrorText(RPN[ArrayPos]);inc(ArrayPos, 8); end;
    else raise Exception.CreateFmt(ErrBadToken,[ValueType]);
  end; //case
end;

function GetArrayText(const RPN: PArrayOfByte; const tpos: integer; var ArrayPos: integer):UTF16String;
var
  Columns: integer;
  Rows: integer;
  r,c:integer;
  Sep: UTF16String;
begin
  Columns:= RPN[ArrayPos]+1;
  Rows:=GetWord(RPN, ArrayPos+1)+1;
  inc(ArrayPos,3);
  Result:=fmOpenArray;
  Sep:='';
  for r:=1 to Rows do
  begin
    for c:=1 to Columns do
    begin
      Result:=Result+Sep+ReadCachedValueTxt(RPN,ArrayPos);
      Sep:=fmArrayColSep;
    end;
    Sep:=fmArrayRowSep;
  end;
  Result:=Result+fmCloseArray;
end;

function Get1Ref(const Row, Col: integer): UTF16String;
begin
  if Col and $4000=0 then Result:=fmAbsoluteRef else Result:='';
  //Result:= Result+EncodeColumn(Col and $3FFF );
  Result:= Result+EncodeColumn(((Col-1) and $FF)+1); //Error on excel docs!!! This is "and $FF" not "and $3FFF"
  if Col and $8000=0 then Result:= Result+fmAbsoluteRef;
  Result:= Result+IntToStr(Row);
end;

function GetRef(const RPN: PArrayOfByte; const tpos: integer):UTF16String;
var
  Row, Col: integer;
begin
  Row:=GetWord(RPN, tpos)+1;
  Col:=GetWord(RPN, tpos+2)+1;
  Result:=Get1Ref(Row, Col);
end;

function GetRowRange(const Row1, Row2: integer; const Abs1, Abs2: boolean): UTF16String;
begin
  if Abs1 then Result:=fmAbsoluteRef else Result:='';
  Result:=Result+IntToStr(Row1)+fmRangeSep;
  if Abs2 then Result:=Result+fmAbsoluteRef;
  Result:=Result+IntToStr(Row2);
end;

function GetColRange(const Col1, Col2: integer): UTF16String;
begin
  if Col1 and $4000=0 then Result:=fmAbsoluteRef else Result:='';
  Result:=Result+EncodeColumn(((Col1-1) and $FF)+1)+fmRangeSep;
  if Col2 and $4000=0 then Result:=Result+fmAbsoluteRef;
  Result:=Result+EncodeColumn(((Col2-1) and $FF)+1);
end;

function GetArea(const RPN: PArrayOfByte; const tpos: integer):UTF16String;
var
  Row1, Col1, Row2, Col2: integer;
begin
  Row1:=GetWord(RPN, tpos  )+1;
  Row2:=GetWord(RPN, tpos+2)+1;
  Col1:=GetWord(RPN, tpos+4)+1;
  Col2:=GetWord(RPN, tpos+6)+1;
  if (Col1 and $FF=1) and (Col2 and $FF =Max_Columns+1) then Result:=GetRowRange(Row1, Row2, Col1 and $8000=0, Col2 and $8000=0) else
  if (Row1=1) and (Row2=Max_Rows+1) then Result:=GetColRange(Col1, Col2) else
  Result:=Get1Ref(Row1, Col1)+fmRangeSep+Get1Ref(Row2, Col2);
end;

function GetName(const RPN: PArrayOfByte; const ExternSheet: integer; const tpos: integer; const CellList: TCellList):UTF16String;
var
  Idx: integer;
begin
 Idx:=GetWord(RPN, tPos);
 Result := CellList.GetName(ExternSheet, Idx - 1);
end;

function GetNameX(const RPN: PArrayOfByte; const tpos: integer; const CellList: TCellList):UTF16String;
var
  Idx: integer;
begin
  Assert(CellList<>nil,'References must not be nil');
  Idx:=GetWord(RPN, tPos);
  Result:= CellList.GetSheetName(Idx)+GetName(RPN, Idx,  tPos+2, CellList);
end;

procedure ReadMemArea(const RPN: PArrayOfByte; const tpos: integer; var ArrayPos: integer);
var
  Len: integer;
begin
  Len:=GetWord(RPN, ArrayPos);
  inc(ArrayPos, 2+8*(Len));
end;

procedure ReadMemErr(const RPN: PArrayOfByte; const tpos: integer; var ArrayPos: integer);
begin
  //Nothing
end;

function GetRef3D(const RPN: PArrayOfByte; const tpos: integer; const CellList: TCellList):UTF16String;
var
  Idx: integer;
  Row, Col: integer;
begin
  Idx:=GetWord(RPN, tPos);
  Row:=GetWord(RPN, tpos+2)+1;
  Col:=GetWord(RPN, tpos+4)+1;
  Result:= CellList.GetSheetName(Idx)+Get1Ref(Row, Col);
end;

function GetArea3D(const RPN: PArrayOfByte; const tpos: integer; const CellList: TCellList):UTF16String;
var
  Idx: integer;
  Row1, Col1, Row2, Col2: integer;
begin
  Idx:=GetWord(RPN, tPos);
  Row1:=GetWord(RPN, tpos+2  )+1;
  Row2:=GetWord(RPN, tpos+4)+1;
  Col1:=GetWord(RPN, tpos+6)+1;
  Col2:=GetWord(RPN, tpos+8)+1;
  Result:=CellList.GetSheetName(Idx)+Get1Ref(Row1, Col1)+fmRangeSep+Get1Ref(Row2, Col2);
end;

function GetFuncName(const RPN: PArrayOfByte; const tPos: integer; var minnp, maxnp: integer): UTF16String;
var
  index: integer;
begin
  index:=GetWord(RPN, tPos);
  if (index<Low(FuncNameArray)) or (index>High(FuncNameArray)) then raise Exception.CreateFmt(ErrIndexOutBounds, [index, 'Function name', Low(FuncNameArray), High(FuncNameArray)]);
  minnp:=FuncNameArray[index].MinArgCount;
  maxnp:=FuncNameArray[index].MaxArgCount;
  Result:=FuncNameArray[index].Name;
end;

function GetFuncNameVar(const RPN: PArrayOfByte; const tPos: integer; var np: integer; out IsAddIn: boolean): UTF16String;
var
  index: integer;
begin
  index:=GetWord(RPN, tPos+1);
  IsAddin := index = $FF;

  if (index<Low(FuncNameArray)) or (index>High(FuncNameArray)) then raise Exception.CreateFmt(ErrIndexOutBounds, [index, 'Function name', Low(FuncNameArray), High(FuncNameArray)]);
  np:=RPN[tPos]and $7F;

  if (IsAddIn) then
  begin
    Result := '';
    dec(np);
  end
  else Result:=FuncNameArray[index].Name;
end;

function GetTableText(const RPN: pArrayOfByte): UTF16String;
begin
  Result:=GetRef(RPN,8)+ fmUnion; //Yes.. Excel leaves an empty comma at the end for one entry tables
  if RPN[6] and $08= $08 then //two entry table
    Result:=Result+GetRef(RPN,12);
end;

function CalcBaseToken(const RealToken: byte): byte;
begin
     if (RealToken and $40)=$40 then Result:= (RealToken or $20) and $3F
     else Result := RealToken and $3F;
end;

procedure ProcessAttr(const RPN: PArrayOfByte; const tPos: integer; var AttrLen: integer; const ParsedStack: TFormulaStack);
var
  s: UTF16String;
begin
  AttrLen:=3;
  if (RPN[tPos] and $04)=$04 then inc(AttrLen, (GetWord(RPN, tPos+1)+1)*2) else
  if (RPN[tPos] and $10)=$10 then
  begin //optimized sum
    ParsedStack.Pop(s);
    ParsedStack.Push(ParsedStack.FmSpaces+FuncNameArray[4].Name+fmOpenParen+s+fmCloseParen)
  end else

  if RPN[tPos] and $40= $40  then //Spaces
    case RPN[tPos+1] of
      attr_bitFSpace: ParsedStack.FmSpaces:= StringOfChar(' ', RPN[tPos+2]);
      attr_bitFEnter: ParsedStack.FmSpaces:= StringOfChar(#13, RPN[tPos+2]);
      attr_bitFPreSpace: ParsedStack.FmPreSpaces:= StringOfChar(' ', RPN[tPos+2]);
      attr_bitFPreEnter: ParsedStack.FmPreSpaces:= StringOfChar(#13, RPN[tPos+2]);
      attr_bitFPostSpace: ParsedStack.FmPostSpaces:= StringOfChar(' ', RPN[tPos+2]);
      attr_bitFPostEnter: ParsedStack.FmPostSpaces:= StringOfChar(#13, RPN[tPos+2]);
      attr_bitFPreFmlaSpace: begin end;//not handled;
    end; //case
end;

function RPNToString(const RPN: PArrayOfByte; const atPos: integer; const CellList: TCellList): UTF16String;
var
  tPos, fPos, arrayPos: integer;
  BaseToken, RealToken: byte;
  ParsedStack: TFormulaStack;
  s1, s2, s3, s4: UTF16String;
  IsAddin: boolean;
  sl, np, i, AttrLen: integer;
  StartFormula:UTF16String;
begin
  Result:='';
  StartFormula:=fmStartFormula; //Formulas do not always begin with "=". Array formulas begin with "{", and they override this var to empty.
  tPos:=atPos;
  fPos:=atPos+GetWord(RPN,atPos-2);
  arrayPos:=fPos;
  ParsedStack:=TFormulaStack.Create;
  try
    while tPos<fPos do
    begin
      RealToken:= RPN[tPos];
      BaseToken:=CalcBaseToken(RealToken);
      case BaseToken of
        {$INCLUDE XlsDoFormula.inc}
        else raise Exception.CreateFmt(ErrBadToken,[RealToken]);
      end; //case
      inc(tPos);
    end; //while
    ParsedStack.Pop(Result);
    Result:=StartFormula+Result;
  finally
    FreeAndNil(ParsedStack);
  end; //finally
end;

end.
