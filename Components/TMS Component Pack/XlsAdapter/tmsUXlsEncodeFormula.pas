unit tmsUXlsEncodeFormula;
{$INCLUDE ..\FLXCOMPILER.INC}

interface

uses
  tmsXlsFormulaMessages, tmsXlsMessages, tmsUXlsFormulaParser,
  tmsUFlxStack, tmsUXlsStrings, SysUtils, tmsUXlsBaseRecordLists, tmsUXlsRowColEntries,
{$IFDEF DELPHI2008UP}
  Character,
{$ENDIF}
  tmsUFlxMessages;

type
  TParseString= class
  private
    ParsePos: integer;
    Fw: UTF16String;
    FParsedData: array of byte;
    FParsedArrayData: array of byte;
    MaxErrorLen: integer;
    DirectlyInFormula: UTF16String;

    LastRefOp: integer;

    FCellList: TCellList;

    StackWs: TWhiteSpaceStack;

    Default3DExternSheet: UTF16String;
    Force3d: boolean; //Named ranges
    InitialRefMode: TFmReturnType;

    function IsNumber(const c: UTF16Char): boolean;
    function IsAlpha(const c: UTF16Char): boolean;
    function IsAZ(const c: UTF16Char): boolean;
    function ATo1(const c: UTF16Char): integer;

    function NextChar: boolean;
    function PeekChar(out c: UTF16Char): boolean;
    function Peek2Char(out c: UTF16Char): boolean;
    function PeekCharWs(out c: UTF16Char): boolean;

    procedure GetNumber;
    procedure GetString;
    procedure GetAlpha;
    procedure GetArray;

    procedure GetFormulaArgs(const Index: integer; out ArgCount: integer);
    procedure GetFormula(const s: UTF16String);
    function  GetBool(const s: UTF16String): boolean;
    function IsErrorCode(const s: UTF16String; out b: byte): boolean;
    procedure GetError;
    procedure GetOneReference(out RowAbs, ColAbs: boolean;out Row, Col: integer; out IsFullRowRange: Boolean; out IsFullColRange: Boolean);
    function  GetReference(const OnlyPeek: boolean): boolean;

    procedure Factor;     // [Whitespace]* Function | Number | String | Cell Reference | 3d Ref | (Expression) | NamedRange | Boolean | Err | Array
    procedure RefTerm;    // Factor [ : | ' ' | , Factor]
    procedure NegTerm;    // [-]* RefTerm
    procedure PerTerm;    // NegTerm [%]*
    procedure ExpTerm;    // PerTerm [ ^ PerTerm]*
    procedure MulTerm;    // ExpTerm [ *|/ ExpTerm ]*
    procedure AddTerm;    // MulTerm [ +|- MulTerm]*
    procedure AndTerm;    // AddTerm [ & AddTerm]*
    procedure ComTerm;    // AndTerm [ = | < | > | <= | >= | <>  AndTerm]*
    procedure Expression;

    procedure SkipWhiteSpace;
    procedure UndoSkipWhiteSpace(const SaveParsePos: integer);
    procedure PopWhiteSpace;
    procedure AddParsed(const s: array of byte; const PopWs: boolean=true);
    procedure AddParsedArray(const s: array of byte);
    function FindComTerm(var Ptg: byte): boolean;
    procedure GetGeneric3dRef(const ExternSheet: UTF16String);
    procedure GetQuotedRef3d;
    procedure GetRef3d(const s: UTF16String);
    function GetExternSheet(const ExternSheet: UTF16String): word;
    procedure ConvertLastRefValueType(const RefMode: TFmReturnType);
    function GetLastRefOp: byte;
    class function GetPtgMode(const aptg: byte): TFmReturnType;
    procedure SetLastRefOp(const aptg: byte; const RefMode: TFmReturnType);
    procedure ConvertLastRefValueTypeOnce(const RefMode: TFmReturnType; var First: boolean);
    function IsDirectlyInFormula: boolean;
    procedure DiscardNormalWhiteSpace;
    procedure MakeLastWhitespaceNormal;
    function GetSecondAreaPart(const ExternSheet: UTF16String;
      const OnlyPeek: Boolean; Row1, Col1: Int32; const RowAbs1, ColAbs1,
      IsFullRowRange1, IsFullColRange1: Boolean): Boolean;
    procedure DoExternNamedRange(const ExternSheet: UTF16String);
    procedure AddParsedArea(const Rw1, Rw2, grBit1, grBit2: Int32);
    procedure AddParsed3dArea(const ExternSheet: UTF16String; const Rw1, Rw2,
      grBit1, grBit2: Int32);
    procedure AddParsed3dRef(const ExternSheet: UTF16String; const Rw1,
      grBit1: Int32);
    procedure AddParsedRef(const Rw1, grBit1: Int32);
    procedure AddParsedExternName(const ExternSheet, ExternName: UTF16String);
  public
    constructor Create(const aw: UTF16String; const aCellList: TCellList; const ReturnType: TFmReturnType);
    constructor CreateExt(const aw: UTF16String;
      const aCellList: TCellList;
      const aForce3D: Boolean; const aDefault3DExternSheet: UTF16String;
      const ReturnType: TFmReturnType);

    destructor Destroy; override;
    procedure Parse;

    function TotalSize: integer;
    procedure CopyToPtr(const Ptr: PArrayOfByte; const aPos: integer);
    procedure CopyToPtrNoLen(const Ptr: PArrayOfByte; const destIndex: integer);
  end;

implementation

function GetRealPtg(const PtgBase: word; const ReturnType: TFmReturnType): word;
begin
  case ReturnType of
    fmArray: Result:=PtgBase+$40;
    fmRef  : Result:=PtgBase;
    else     Result:=PtgBase+$20;
  end; //case
end;

{ TParseString }

constructor TParseString.Create(const aw: UTF16String; const aCellList: TCellList; const ReturnType: TFmReturnType);
begin
  inherited Create;
  Fw:= aw;
  ParsePos:=1;
  StackWs:=TWhiteSpaceStack.Create;
  FCellList:=aCellList;
  Force3D := false;
  InitialRefMode:=ReturnType;
  MaxErrorLen:=Length(fmErrNull);
  if MaxErrorLen<Length(  fmErrDiv0 ) then MaxErrorLen:=Length(fmErrDiv0 );
  if MaxErrorLen<Length(  fmErrValue) then MaxErrorLen:=Length(fmErrValue);
  if MaxErrorLen<Length(  fmErrRef  ) then MaxErrorLen:=Length(fmErrRef  );
  if MaxErrorLen<Length(  fmErrName ) then MaxErrorLen:=Length(fmErrName );
  if MaxErrorLen<Length(  fmErrNum  ) then MaxErrorLen:=Length(fmErrNum  );
  if MaxErrorLen<Length(  fmErrNA   ) then MaxErrorLen:=Length(fmErrNA   );
end;

constructor TParseString.CreateExt(const aw: UTF16String; const aCellList: TCellList;
                     const aForce3D: Boolean; const aDefault3DExternSheet: UTF16String; const ReturnType: TFmReturnType);
begin
  Create(aw, aCellList, ReturnType);
  Default3DExternSheet := aDefault3DExternSheet;
  Force3D := aForce3d;
end;

destructor TParseString.Destroy;
begin
  FreeAndNil(StackWs);
  inherited;
end;

function TParseString.GetLastRefOp: byte;
begin
  Result:= FParsedData[LastRefOp];
end;

procedure TParseString.SetLastRefOp(const aptg: byte; const RefMode: TFmReturnType);
var
  newptg: Byte;
begin
  newptg := Byte(aptg);
  if (Byte(aptg) and 96) <> 0 then
  begin
    case RefMode of
      fmRef:
          newptg := Byte((newptg and 159) or 32);
      fmValue:
          newptg := Byte((newptg and 159) or 64);
      fmArray:
          newptg := Byte(newptg or 96);
    end; //case
  end;
  FParsedData[LastRefOp] :=newptg;
end;

class function TParseString.GetPtgMode(const aptg: byte): TFmReturnType;
var
  PtgMode: TFmReturnType;
begin
  PtgMode := fmValue;
  if ((aptg = ptgRange) or (aptg = ptgIsect)) or (aptg = ptgUnion) then PtgMode := fmRef;

  case aptg and 96 of
    32:
      PtgMode := fmRef;
    96:
      PtgMode := fmArray;
  end; //case
  
  Result := PtgMode;
end;

procedure TParseString.ConvertLastRefValueType(const RefMode: TFmReturnType);
var
  aptg: byte;
  PtgMode: TFmReturnType;
begin
  if LastRefOp < 0 then
    raise Exception.Create(ErrInternal);
  
  aptg := GetLastRefOp;
  PtgMode := GetPtgMode(aptg);
  case RefMode of
    fmValue:
    begin
      if PtgMode <> fmArray then
        SetLastRefOp(aptg, fmValue);
    end;
    fmArray:
    begin
      SetLastRefOp(aptg, fmArray);
    end;
  end;
end;

procedure TParseString.ConvertLastRefValueTypeOnce(const RefMode: TFmReturnType; var First: boolean);
begin
  if (First) then ConvertLastRefValueType(RefMode);
  First:=false;
end;

procedure TParseString.GetRef3d(const s: UTF16String);
var
  c: UTF16Char;
begin
  c := ' ';
  if not PeekChar(c) or (c <> fmExternalRef) then
    raise Exception.CreateFmt(ErrUnexpectedChar, [c, ParsePos, Fw]);
  NextChar;
  GetGeneric3dRef(s);
end;

procedure TParseString.GetQuotedRef3d;
var
  e: UTF16Char;
  d: UTF16Char;
  More: Boolean;
  sq: UTF16String;
  c: UTF16Char;
  s: UTF16String;
begin
  SkipWhiteSpace;
  s := '';
  c := ' ';
  sq := fmSingleQuote;
  if not PeekChar(c) or (c <> sq) then
    raise Exception.CreateFmt(ErrUnexpectedChar, [c, ParsePos, Fw]);
  NextChar;

  repeat
    More := False;
    if PeekChar(c) and (c <> sq) then
    begin
      s:=s+c;
      NextChar;
      More := True;
    end
    else
    begin
      d := ' ';
      e := ' ';
      if PeekChar(d) and (d = sq) and Peek2Char(e) and (e = sq) then
      begin
        s:=s+sq;
        NextChar;
        NextChar;
        More := True;
      end;
    end;
  until not More;
  if not PeekChar(c) or (c <> sq) then
    raise Exception.CreateFmt(ErrUnterminatedString, [Fw]);
  NextChar;
  GetRef3d(s);
end;

procedure TParseString.Factor;
var
  c: UTF16Char;
begin
  if PeekCharWs(c) then
  begin
    if ord(c)>255 then raise Exception.CreateFmt(ErrUnexpectedChar, [AnsiChar(c), ParsePos, Fw]);
    if c= fmOpenParen then
    begin
      SkipWhiteSpace;
      NextChar;

			DirectlyInFormula := DirectlyInFormula + '0';
			try
        Expression;
      finally
				Delete(DirectlyInFormula, Length(DirectlyInFormula), 1);
      end;

      if not (PeekCharWs(c)) or (c<>fmCloseParen) then raise Exception.CreateFmt(ErrMissingParen, [Fw]);
      SkipWhiteSpace;
      NextChar;
      PopWhiteSpace;
      AddParsed([ptgParen]);
    end
    else if c=fmStr then GetString
    else if c=fmOpenArray then GetArray
    else if c=fmErrStart then GetError
    else if not GetReference(false) then
		  if (IsNumber(c) or (c = fmFormulaDecimal)) then GetNumber //Is number must go after getreference, to handle things like =sum(1:2)
      else if IsAlpha(c) then GetAlpha
      else if c=fmSingleQuote then GetQuotedRef3d();
  end
  else
    raise Exception.CreateFmt(ErrUnexpectedEof, [Fw]);
end;

function TParseString.IsDirectlyInFormula: boolean;
begin
  if (Length(DirectlyInFormula) <=0) then Result:= false
  else
  begin
    Result := DirectlyInFormula[Length(DirectlyInFormula)]='1';
  end;
end;

procedure TParseString.RefTerm;
// Factor [ : | ' ' | , Factor]
var
  c: UTF16Char;
  b: byte;
  First: boolean;
begin
  First:=true;
  Factor;
  //Pending: see how to fix intersect (on popwhitespace, if there are two references, is an intersect).
  //Union is only valid if we are not inside a function. For example A2:A3,B5 is ok. But HLookup(0,A2:A3,B5,1, true) is not ok.

  while PeekCharWS(c) and (((c=fmUnion) and not IsDirectlyInFormula)   or (c=fmRangeSep) {or (c=fmIntersect)}) do
  begin
	  ConvertLastRefValueTypeOnce(fmRef, First);
    SkipWhiteSpace;
    NextChar;
    Factor;
	  ConvertLastRefValueType(fmRef);

    if (c=fmUnion) then b:=ptgUnion else
    if (c=fmRangeSep) then b:=ptgRange else
    if (c=fmIntersect) then b:=ptgIsect else
    raise Exception.Create(ErrInternal);
    AddParsed(b);
  end;
end;

procedure TParseString.NegTerm;
//[-]* RefTerm
var
  c: UTF16Char;
  i: integer;
  s: UTF16String;
begin
  s:='';
  while PeekCharWs(c) and ((c=fmMinus) or (c=fmPlus))do
  begin
    SkipWhiteSpace;
    NextChar;
    s:=s+c;
  end;
  RefTerm;
  if Length(s)>0 then
  begin
    ConvertLastRefValueType(fmValue);
    for i:=1 to Length(s) do
      if (s[i] = fmMinus) then AddParsed([ptgUminus]) else AddParsed([ptgUplus]);
  end;
end;

procedure TParseString.PerTerm;
// NegTerm [%]*
var
  c: UTF16Char;
  First: boolean;
begin
  First:=true;
  NegTerm;
  while PeekCharWs(c) and (c=fmPercent) do
  begin
	  ConvertLastRefValueTypeOnce(fmValue, First);
    SkipWhiteSpace;
    NextChar;
    AddParsed([ptgPercent]);
  end;
end;

procedure TParseString.ExpTerm;
// PerTerm [ ^ PerTerm]*
var
  c: UTF16Char;
  First: boolean;
begin
  First:=true;
  PerTerm;
  while PeekCharWs(c) and (c=fmPower) do
  begin
	  ConvertLastRefValueTypeOnce(fmValue, First);
    SkipWhiteSpace;
    NextChar;
    PerTerm;
	  ConvertLastRefValueType(fmValue);
    AddParsed([ptgPower]);
  end;
end;

procedure TParseString.MulTerm;
// ExpTerm [ *|/ ExpTerm ]*
var
  c: UTF16Char;
  First: boolean;
begin
  First:=true;
  ExpTerm;
  while PeekCharWs(c) and ((c=fmMul) or (c=fmDiv)) do
  begin
	  ConvertLastRefValueTypeOnce(fmValue, First);
    SkipWhiteSpace;
    NextChar;
    ExpTerm;
	  ConvertLastRefValueType(fmValue);
    if (c=fmMul) then AddParsed([ptgMul]) else AddParsed([ptgDiv]);
  end;
end;

procedure TParseString.AddTerm;
// MulTerm [ +|- MulTerm]*
var
  c: UTF16Char;
  First: boolean;
begin
  First:=true;
  MulTerm;
  while PeekCharWs(c) and ((c=fmPlus) or (c=fmMinus)) do
  begin
	  ConvertLastRefValueTypeOnce(fmValue, First);
    SkipWhiteSpace;
    NextChar;
    MulTerm;
	  ConvertLastRefValueType(fmValue);
    if (c=fmPlus) then AddParsed([ptgAdd]) else AddParsed([ptgSub]);
  end;
end;

procedure TParseString.AndTerm;
// AddTerm [ & AddTerm]*
var
  c: UTF16Char;
  First: boolean;
begin
  First:=true;
  AddTerm;
  while PeekCharWs(c) and (c=fmAnd) do
  begin
	  ConvertLastRefValueTypeOnce(fmValue, First);
    SkipWhiteSpace;
    NextChar;
    AddTerm;
	  ConvertLastRefValueType(fmValue);
    AddParsed([ptgConcat]);
  end;
end;

function TParseString.FindComTerm(var Ptg: byte): boolean;
var
  c,d:UTF16Char;
  s: UTF16String;
  One: boolean;
begin
  Result:= PeekCharWs(c) and ((c=fmEQ) or (c=fmLT) or (c=fmGT));
  if Result then
  begin
    One:=true;
    SkipWhiteSpace; //Already granted we will add a ptg
    NextChar;
    if PeekChar(d)and((d=fmEQ) or (d=fmGT)) then
    begin
      s:=c; s:=s+d; One:=False;
      if s = fmGE then begin; NextChar; Ptg:=ptgGE; end else
      if s = fmLE then begin; NextChar; Ptg:=ptgLE; end else
      if s = fmNE then begin; NextChar; Ptg:=ptgNE; end else
      One:=True;
    end;
    If One then
      if c= fmEQ then Ptg:=ptgEQ else
      if c= fmLT then Ptg:=ptgLT else
      if c= fmGT then Ptg:=ptgGT else
      raise Exception.Create(ErrInternal);
  end;
end;

procedure TParseString.ComTerm;
// AndTerm [ = | < | > | <= | >= | <>  AndTerm]*
var
  c: UTF16Char;
  Ptg: byte;
  First: boolean;
begin
  First:=true;
  AndTerm;
  while PeekCharWs(c) and FindComTerm(Ptg) do
  begin
    //no NextChar or SkipWhitespace here. It is added by FindComTerm
	  ConvertLastRefValueTypeOnce(fmValue, First);
    AndTerm;
	  ConvertLastRefValueType(fmValue);
    AddParsed([Ptg]);
  end;
end;

procedure TParseString.Expression;
begin
  ComTerm;
end;

procedure TParseString.GetNumber;
var
  c: UTF16Char;
  d: double;
  w: word;
  ab: array[0..7] of byte;
  start: integer;
begin
  SkipWhiteSpace;
  start:=ParsePos;
  while PeekChar(c) and (IsNumber(c)or (c=fmFormulaDecimal)) do NextChar;
  if PeekChar(c) and ((c='e')or (c='E')) then //numbers like 1e+23
  begin
    NextChar;
    if PeekChar(c) and ((c=fmPlus)or (c=fmMinus)) then NextChar;
    while PeekChar(c) and IsNumber(c) do NextChar; //no decimals allowed here
  end;

  d:=fmStrToFloat(copy(FW, start, ParsePos-Start));

  if (round(d)=d) and (d<=$FFFF)and (d>=0) then
  begin
    w:=round(d);
    AddParsed([ptgInt, byte(w), hi(word(w))]);
  end else
  begin
    move(d, ab[0], length(ab));
    AddParsed([ptgNum, ab[0], ab[1], ab[2], ab[3], ab[4], ab[5], ab[6], ab[7]]);
  end;
end;

procedure TParseString.GetString;
var
  c,d,e: UTF16Char;
  s: UTF16String;
  Xs: TExcelString;
  St: array of byte;
  More: boolean;
begin
  s:='';
  SkipWhiteSpace;
  if not PeekChar(c) or (c<>fmStr) then raise Exception.Create(ErrNotAString);
  NextChar;

  repeat
    More:=false;
    if PeekChar(c) and (c<>fmStr) then
    begin
      s:=s+c;
      NextChar;
      More:=true;
    end
    else
    begin
      if PeekChar(d) and (d=fmStr) and Peek2Char(e) and (e=fmStr) then
      begin
        s:=s+fmStr;
        NextChar;
        NextChar;
        More:=true;
      end;
    end;
   until not more;

   if not PeekChar(c) then raise Exception.CreateFmt(ErrUnterminatedString,[Fw]);
   NextChar;

   Xs:=TExcelString.Create(1,s);
   try
     SetLength(St, Xs.TotalSize+1);
     St[0]:=ptgStr;
     Xs.CopyToPtr(PArrayOfByte(St),1);
     AddParsed(St);
   finally
     FreeAndNil(Xs);
   end; //finally
end;

procedure TParseString.GetAlpha;
// Possibilities:
{ 1 -> Formula - We know by the "(" at the end
  2 -> Boolean - We just see if text is "true" or "false"
  3 -> Error   - No, we already cached this
  4 -> Reference - Look if it is one of the strings between A1..IV65536 (and $A$1) As it might start with $, we don't look at it here.
  5 -> 3d Ref    - Search for a '!'  As it migh start with "'" we don't look at it here.
  6 -> Named Range - if it isn't anything else...
}
var
  Start: integer;
  s: string; //no need for widestring
  c: UTF16Char;
begin
  SkipWhiteSpace;
  start:=ParsePos;
  while PeekChar(c) and ( IsAlpha(c) or IsNumber(c) or (c='.')or (c=':')) do NextChar;
  s:=UpperCase(copy(FW, start, ParsePos-Start));

  if PeekChar(c) and (c=fmOpenParen) then GetFormula(s) else
  if PeekChar(c) and (c=fmExternalRef) then GetRef3d(s) else
  if not GetBool(s) then
  raise Exception.CreateFmt(ErrUnexpectedId,[s,Fw]);


end;

function TParseString.GetBool(const s: UTF16String): boolean;
var
  b: byte;
begin
  if s=fmTrue then b:=1 else
  if s=fmFalse then b:=0 else
  begin
    Result:=false;
    exit;
  end;

  AddParsed([ptgBool, b]);
  Result:=true;
end;

procedure TParseString.GetOneReference(out RowAbs, ColAbs: boolean; out  Row, Col: integer; out IsFullRowRange: Boolean; out IsFullColRange: Boolean);
var
  c: UTF16Char;
begin
  RowAbs:=false; ColAbs:=false;
  IsFullColRange := true;  //Something like 'B:B'
  IsFullRowRange := true;  //something like '1:3'

  if PeekChar(c) and (c=fmAbsoluteRef) then
  begin
    ColAbs:=true;
    NextChar;
  end;

  Col:=0;
  while (PeekChar(c) and IsAZ(c)) and (Col <= (Max_Columns + 1)) do
  begin
    IsFullRowRange := false;
    NextChar;
    Col := (Col * ATo1('Z')) + ATo1(c);
  end;

  if ColAbs and IsFullRowRange then
  begin
    ColAbs := false;
    RowAbs := true;
  end
  else
  begin
    if PeekChar(c) and (c=fmAbsoluteRef) then
    begin
      RowAbs:=true;
      NextChar;
    end;
  end;

  Row:=0;
  while PeekChar(c) and IsNumber(c) and (Row<=Max_Rows+1) do
  begin
    NextChar;
    IsFullColRange := false;
    Row:=Row*10+(ord(c)-ord('0'));
  end;

end;

function TParseString.GetExternSheet(const ExternSheet: UTF16String): word;
var
  i: integer;
  SheetName: UTF16String;
  Sheet1, Sheet2: integer;
begin
  i:= pos (fmRangeSep, ExternSheet);
  if (i>0) then SheetName:=Copy(ExternSheet,1, i-1) else SheetName:=ExternSheet;

  if not FCellList.FindSheet(SheetName, Sheet1) then raise Exception.CreateFmt(ErrInvalidSheet, [SheetName]);

  if (i>0) then
  begin
    SheetName:=Copy(ExternSheet,i+1, Length(ExternSheet));
    if not FCellList.FindSheet(SheetName, Sheet2) then raise Exception.CreateFmt(ErrInvalidSheet, [SheetName]);
  end
    else Sheet2:=Sheet1;

  Result:=FCellList.AddExternSheet(Sheet1, Sheet2);
end;

procedure TParseString.AddParsedRef(const Rw1: Int32; const grBit1: Int32);
begin
  if Force3D then
  begin
    AddParsed3dRef(Default3DExternSheet, Rw1, grBit1 and not $0C000);
    exit;
  end;

  AddParsed([GetRealPtg(ptgRef,fmRef) , byte(Rw1), hi(word(Rw1)), byte(grBit1), hi(word(grBit1))]);
end;

procedure TParseString.AddParsed3dRef(const ExternSheet: UTF16String; const Rw1: Int32; const grBit1: Int32);
var
  ESheet: word;
begin
  ESheet:=GetExternSheet(ExternSheet);
  AddParsed([GetRealPtg(ptgRef3d,fmRef) ,byte(ESheet), hi(word(ESheet)), byte(Rw1), hi(word(Rw1)), byte(grBit1), hi(word(grBit1))]);
end;

procedure TParseString.AddParsedArea(const Rw1: Int32; const Rw2: Int32; const grBit1: Int32; const grBit2: Int32);
begin
  if Force3D then
  begin
    AddParsed3dArea(Default3DExternSheet, Rw1, Rw2, grBit1 and not $0C000, grBit2 and not $0C000);
    exit;
  end;
  AddParsed([GetRealPtg(ptgArea,fmRef) , byte(Rw1), hi(word(Rw1)), byte(Rw2), hi(word(Rw2)), byte(grBit1), hi(word(grBit1)), byte(grBit2), hi(word(grBit2))]);
end;

procedure TParseString.AddParsed3dArea(const ExternSheet: UTF16String; const Rw1: Int32; const Rw2: Int32; const grBit1: Int32; const grBit2: Int32);
var
  ESheet: word;
begin
  ESheet:=GetExternSheet(ExternSheet);
  AddParsed([GetRealPtg(ptgArea3d,fmRef) ,byte(ESheet), hi(word(ESheet)), byte(Rw1), hi(word(Rw1)), byte(Rw2), hi(word(Rw2)), byte(grBit1), hi(word(grBit1)), byte(grBit2), hi(word(grBit2))]);
end;

procedure TParseString.AddParsedExternName(const ExternSheet: UTF16String; const ExternName: UTF16String);
begin
  raise Exception.Create('External names are not supported: ' + ExternName);
end;


function TParseString.GetSecondAreaPart(const ExternSheet: UTF16String; const OnlyPeek: Boolean; Row1: Int32; Col1: Int32; const RowAbs1: Boolean; const ColAbs1: Boolean; const IsFullRowRange1: Boolean; const IsFullColRange1: Boolean): Boolean;
var
  RowAbs2: Boolean;
  ColAbs2: Boolean;
  Row2: Int32;
  Col2: Int32;
  ActualPos: Int32;
  IsFullRowRange2: Boolean;
  IsFullColRange2: Boolean;
  rw1: Int32;
  grBit1: Int32;
  rw2: Int32;
  grBit2: Int32;
begin
  RowAbs2 := false;
  ColAbs2 := false;
  Row2 := 0;
  Col2 := 0;
  ActualPos := ParsePos;
  NextChar;
  GetOneReference(RowAbs2, ColAbs2, Row2, Col2, IsFullRowRange2, IsFullColRange2);
  if IsFullRowRange1 and IsFullRowRange2 then
  begin
    Col1 := 1;
    Col2 := Max_Columns + 1;
  end;

  if IsFullColRange1 and IsFullColRange2 then
  begin
    Row1 := 1;
    Row2 := Max_Rows + 1;
  end;

  if (((Row2 > (Max_Rows + 1)) or (Row2 <= 0)) or (Col2 <= 0)) or (Col2 > (Max_Columns + 1)) then
  begin
    ParsePos := ActualPos;
    begin Result := false; exit; end;
  end;

   rw1 := Row1 - 1;
   grBit1 := (Col1 - 1) and $FF;
   if not RowAbs1 then
     grBit1 := (grBit1 or $8000);
   if not ColAbs1 then
     grBit1 := (grBit1 or $4000);
   rw2 := Row2 - 1;
   grBit2 := (Col2 - 1) and $FF;
   if not RowAbs2 then
     grBit2 := (grBit2 or $8000);
   if not ColAbs2 then
     grBit2 := (grBit2 or $4000);

  if not OnlyPeek then
  begin
    if ExternSheet <> '' then
      AddParsed3dArea(ExternSheet, rw1, rw2, grBit1, grBit2) else
      AddParsedArea(rw1, rw2, grBit1, grBit2);
  end;

  Result := true;
end;

procedure TParseString.DoExternNamedRange(const ExternSheet: UTF16String);
var
  start: Int32;
  c: UTF16Char;
  s: UTF16String;
begin
  start := ParsePos;
  c := ' ';
  while PeekChar(c) and (((IsAlpha(c) or IsNumber(c)) or (c = '.')) or (c = ':')) do
  begin
    NextChar;
  end;
  s := UpperCase(Copy(Fw, start, ParsePos - start));
  AddParsedExternName(ExternSheet, s);
end;

procedure TParseString.GetGeneric3dRef(const ExternSheet: UTF16String);
var
  RowAbs1: Boolean;
  ColAbs1: Boolean;
  Row1: Int32;
  Col1: Int32;
  IsFullRowRange1: Boolean;
  IsFullColRange1: Boolean;
  SavedPos: Int32;
  d: UTF16Char;
  c: UTF16Char;
  IsArea: Boolean;
  rw1: Int32;
  grBit1: Int32;
begin
  RowAbs1 := False;
  ColAbs1 := False;
  Row1 := 0;
  Col1 := 0;

  SavedPos := ParsePos;
  d := ' ';
  GetOneReference(RowAbs1, ColAbs1, Row1, Col1, IsFullRowRange1, IsFullColRange1);

  if ((((Row1 <= 0) and (Col1 <= 0)) or (Row1 > (Max_Rows + 1))) or (Col1 > (Max_Columns + 1))) or (PeekChar(d) and IsAlpha(d)) then
  begin  //something like "a3a"
  //Wasn't a valid reference. It might be a name
    ParsePos := SavedPos;
    DoExternNamedRange(ExternSheet);
    exit;
  end;

  if not IsFullRowRange1 and not IsFullColRange1 then
  begin
  if (Row1 > Max_Rows + 1) or (Row1 <= 0) or (Col1 <= 0) or (Col1 > Max_Columns + 1) then
    raise Exception.CreateFmt(ErrUnexpectedId, [IntToStr(Row1)+ ', '+ IntToStr(Col1), Fw]);
  end;

  c := ' ';
  IsArea := false;
  if PeekChar(c) and (c = fmRangeSep) then
  begin
    IsArea := GetSecondAreaPart(ExternSheet, false, Row1, Col1, RowAbs1, ColAbs1, IsFullRowRange1, IsFullColRange1);
  end;

  if not IsArea then
  begin
    if IsFullColRange1 or IsFullRowRange1 then
    begin
      raise Exception.CreateFmt(ErrUnexpectedId, [IntToStr(Row1) + ', ' + IntToStr(Col1), Fw]);
    end;
    rw1 := Row1 - 1;
    grBit1 := (Col1 - 1) and $FF;
    if not RowAbs1 then
      grBit1 := (grBit1 or $8000);
    if not ColAbs1 then
      grBit1 := (grBit1 or $4000);

    AddParsed3dRef(ExternSheet, rw1, grBit1);
  end;
end;


function TParseString.GetReference(const OnlyPeek: Boolean): Boolean;
var
  SaveParsePos: Int32;
  RowAbs1: Boolean;
  ColAbs1: Boolean;
  Row1: Int32;
  Col1: Int32;
  IsFullRowRange1: Boolean;
  IsFullColRange1: Boolean;
  c: UTF16Char;
  IsArea: Boolean;
  rw1: Int32;
  grBit1: Int32;
begin
  SaveParsePos := ParsePos;
  SkipWhiteSpace;
  RowAbs1 := false;
  ColAbs1 := false;
  Row1 := 0;
  Col1 := 0;
  GetOneReference(RowAbs1, ColAbs1, Row1, Col1, IsFullRowRange1, IsFullColRange1);
  if not IsFullRowRange1 and not IsFullColRange1 then
  begin
    if (Row1>Max_Rows+1) or (Row1<=0) or (Col1<=0) or (Col1>Max_Columns+1) then
    begin
      UndoSkipWhiteSpace(SaveParsePos);
      Result := false;
      exit;
    end;
  end;

  IsArea := false;
  if PeekChar(c) and (c = fmRangeSep) then
  begin
    IsArea := GetSecondAreaPart('', OnlyPeek, Row1, Col1, RowAbs1, ColAbs1, IsFullRowRange1, IsFullColRange1);
  end;

  if not IsArea then
  begin
    if IsFullColRange1 or IsFullRowRange1 then
    begin
      UndoSkipWhiteSpace(SaveParsePos);
      begin Result := false; exit; end;
    end;

    rw1 := Row1 - 1;
    grBit1 := (Col1-1) and $FF;
    if not RowAbs1 then grBit1 := grBit1 or $8000;
    if not ColAbs1 then grBit1 := grBit1 or $4000;

    if not OnlyPeek then
      AddParsedRef(rw1, grBit1);
  end;

  if OnlyPeek then
  begin
    UndoSkipWhiteSpace(SaveParsePos);
  end;

  Result := true;
end;


function TParseString.IsErrorCode(const s: UTF16String; out b: byte): boolean;
begin
  Result:=true;
  b:=0;
  if s= fmErrNull  then b:=fmiErrNull else
  if s= fmErrDiv0  then b:=fmiErrDiv0 else
  if s= fmErrValue then b:=fmiErrValue else
  if s= fmErrRef   then b:=fmiErrRef else
  if s= fmErrName  then b:=fmiErrName else
  if s= fmErrNum   then b:=fmiErrNum else
  if s= fmErrNA    then b:=fmiErrNA else Result:=false;
end;

procedure TParseString.GetError;
var
  b: byte;
  Start: integer;
  s: UTF16String;
  c: UTF16Char;
begin
  SkipWhiteSpace;
  start:=ParsePos;

  while PeekChar(c) do
  begin
    NextChar;
    s:=WideUpperCase98(copy(FW, start, ParsePos-Start));
    if IsErrorCode(s, b) then
    begin
      AddParsed([ptgErr, b]);
      exit;
    end;

    if Length(s)>MaxErrorLen then break;
  end;

  raise Exception.CreateFmt(ErrUnexpectedId,[s,Fw]);

end;

function FindFormula(const s: UTF16String; var Index: integer): boolean;
var
  i:integer;
begin
  //Pending: optimize this to be binary search
  for i:=low(FuncNameArray) to High(FuncNameArray) do
    if FuncNameArray[i].Name=s then
    begin
      Result:=true;
      Index:=i;
      exit;
    end;
  Result:=False;
end;

function FuncParamType(const Index: integer; Position: integer): TFmReturnType;
begin
	if (Position+1 > Length(FuncNameArray[Index].ParamType) - 1) then Position := Length(FuncNameArray[Index].ParamType)-1;
	case (FuncNameArray[Index].ParamType[Position+1]) of
			'A': Result:= fmArray;
			'R': Result:= fmRef;
			'V': Result:= fmValue;
			'-': Result:= fmValue; //Missing Arg.
      else 	raise Exception.Create(ErrInternal);
  end; //case
end;

procedure TParseString.GetFormulaArgs(const Index: integer; out ArgCount: integer);
var
  c: UTF16Char;
  MoreToCome: boolean;
  ActualPos: integer;
begin
  ArgCount:=0;
  NextChar; //skip parenthesis
  c:= ' ';
  MoreToCome:=true;
  while MoreToCome do
  begin
    ActualPos := ParsePos;
    Expression;

    if (ParsePos = ActualPos) then //missing parameter.
    begin
      SkipWhiteSpace;
      if (ArgCount > 0) or (PeekChar(c) and (c=fmFunctionSep)) then
      begin
			  MakeLastWhitespaceNormal; //No real need to call this here, but this way it will behave the same as Excel. (An space before the closing parenthesis on a missing arg is not a post whitespace but a normal space)
				AddParsed([ptgMissArg]);
      end
      else
      begin
				PopWhiteSpace();
        dec(ArgCount);  //This is not a real argument, as in PI()
      end;
    end else
    begin
			ConvertLastRefValueType(FuncParamType(Index, ArgCount));
			SkipWhiteSpace();
			DiscardNormalWhiteSpace();  //No space is allowed before a ",". We only keep the whitespace if it is for closing a parenthesis.
    end;

    if PeekCharWs(c) then
    begin
			//We should not call SkipWhitespace here, as it was already called.
      if c=fmFunctionSep then
      begin
        NextChar;
				if (not PeekChar(c)) then
					raise Exception.CreateFmt(ErrUnexpectedEof, [Fw]);
      end else
      if c = fmCloseParen then
      begin
        MoreTocome:=false;
      end else raise Exception.CreateFmt(ErrUnexpectedChar, [char(c), ParsePos, Fw]);
    end else raise Exception.CreateFmt(ErrUnexpectedEof, [Fw]);


    inc(ArgCount);
  end;

  if not PeekChar(c) then raise Exception.CreateFmt(ErrMissingParen, [Fw]);
  NextChar;

  if (ArgCount < FuncNameArray[Index].MinArgCount) or (ArgCount > FuncNameArray[Index].MaxArgCount) then
    raise Exception.CreateFmt(ErrInvalidNumberOfParams,[FuncNameArray[Index].Name, FuncNameArray[Index].MinArgCount,ArgCount]);
end;

procedure TParseString.GetFormula(const s: UTF16String);
var
  Index, ArgCount: integer;
  Ptg: byte;
begin
  if not FindFormula(s, Index) then
    raise Exception.CreateFmt(ErrFunctionNotFound,[s,Fw]);

			DirectlyInFormula := DirectlyInFormula + '1';
			try
			  GetFormulaArgs(Index, Argcount);
      finally
				Delete(DirectlyInFormula, Length(DirectlyInFormula), 1);
      end;

  if FuncNameArray[Index].MinArgCount <> FuncNameArray[Index].MaxArgCount then
  begin
    Ptg:=GetRealPtg(ptgFuncVar, FuncNameArray[Index].ReturnType);
    AddParsed([Ptg, ArgCount, byte(FuncNameArray[Index].Index), hi(word(FuncNameArray[Index].Index))]);
  end else
  begin
    Ptg:=GetRealPtg(ptgFunc, FuncNameArray[Index].ReturnType);
    AddParsed([Ptg, byte(FuncNameArray[Index].Index), hi(word(FuncNameArray[Index].Index))]);
  end;

end;

procedure TParseString.GetArray;
var
  Rows, Cols: integer;
  c: UTF16Char;
begin
  raise exception.Create('Writing array formulas is not yet supported');
  SkipWhiteSpace;
  if not PeekChar(c) or (c<>fmOpenArray) then raise Exception.CreateFmt(ErrUnexpectedChar, [char(c), ParsePos, Fw]);
  NextChar;
  while PeekChar(c) and (c<>fmCloseArray) do
  begin
    NextChar;
    if c=fmArrayRowSep then inc(Rows) else
    if c=fmArrayColSep then inc(Cols);
  end;
  AddParsedArray([byte(Cols-1), byte(Rows-1), hi(word(Rows-1))]);
  //pending: add the data to array.


  if not PeekChar(c) then raise Exception.CreateFmt(ErrMissingParen, [Fw]);

  AddParsed([ptgArray, 0, 0, 0, 0, 0, 0, 0]);
end;

function TParseString.NextChar: boolean;
begin
  Result:=ParsePos<=Length(Fw);
  if Result then
  begin
    inc(ParsePos);
    if ParsePos>1024 then raise Exception.CreateFmt(ErrFormulaTooLong,[Fw]);
  end;
end;

function TParseString.PeekChar(out c: UTF16Char): boolean;
begin
  Result:=ParsePos<=Length(Fw);
  if Result then
  begin
    c:=Fw[ParsePos];
  end;
end;

function TParseString.Peek2Char(out c: UTF16Char): boolean;
begin
  Result:=ParsePos+1<=Length(Fw);
  if Result then
  begin
    c:=Fw[ParsePos+1];
  end;
end;

function TParseString.PeekCharWs(out c: UTF16Char): boolean;
var
  aParsePos: integer;
begin
  aParsePos:= ParsePos;
  while (aParsePos<=Length(Fw)) and (Fw[aParsePos] =' ') do begin inc(aParsePos); end;

  Result:=aParsePos<=Length(Fw);
  if Result then
  begin
    c:=Fw[aParsePos];
  end;

end;

procedure TParseString.SkipWhiteSpace;
var
  Ws: TWhitespace;
  c: UTF16Char;
begin
  Ws.Count:=0;
  while PeekChar(c) and (c =' ') do begin NextChar; if (Ws.Count<255) then inc(Ws.Count); end;

  if ParsePos<=Length(Fw) then
  begin
    c:=Fw[ParsePos];
    if (c=fmOpenParen) then  Ws.Kind:=attr_bitFPreSpace else
    if (c=fmCloseParen) then  Ws.Kind:=attr_bitFPostSpace
    else Ws.Kind:= attr_bitFSpace;
    StackWs.Push(Ws);
  end;

end;

procedure TParseString.UndoSkipWhiteSpace(const SaveParsePos: integer);
var
  Ws: TWhiteSpace;
begin
  StackWs.Pop(Ws);
  ParsePos:=SaveParsePos;
end;

procedure TParseString.Parse;
var
  c: UTF16Char;
  Ptr: PArrayOfByte;
begin
  LastRefOp := -1;
  DirectlyInFormula := '';
  SetLength(FParsedData,0);
  SetLength(FParsedArrayData,0);
  if not PeekChar(c) or (c<>fmStartFormula) then raise Exception.CreateFmt(ErrFormulaStart,[Fw]);
  NextChar;
  Expression;

	ConvertLastRefValueType(InitialRefMode);

  if PeekChar(c) then raise Exception.CreateFmt(ErrUnexpectedChar,[char(c), ParsePos, Fw]);
  if StackWs.Count<>0 then raise Exception.Create(ErrInternal);

  //Try to decode what we encoded
  //something like "= >" will be encoded nicely, but will crash when decoded

  GetMem(Ptr, TotalSize);
  try
    CopyToPtr(Ptr, 0);
    try
      RPNToString(Ptr, 2, FCellList);
    except
      raise Exception.CreateFmt(ErrFormulaInvalid,[Fw]);
    end;
  finally
    FreeMem(Ptr);
  end; //finally
end;

procedure TParseString.PopWhiteSpace;
var
  Ws: TWhiteSpace;
begin
  StackWs.Pop(Ws);
  if Ws.Count>0 then
    AddParsed([ptgAttr,$40,Ws.Kind, Ws.Count], false);
end;

procedure TParseString.DiscardNormalWhiteSpace;
var
  Ws: TWhiteSpace;
begin
	StackWs.Pop(Ws);
	if (Ws.Count>0) and (Ws.Kind <> attr_bitFSpace) then
    AddParsed([ptgAttr,$40,Ws.Kind, Ws.Count], false);
end;

procedure TParseString.MakeLastWhitespaceNormal;
var
  Ws: TWhiteSpace;
begin
  StackWs.Peek(Ws);
  Ws.Kind := attr_bitFSpace;
end;


procedure TParseString.AddParsed(const s: array of byte; const PopWs: boolean=true);
begin
  if Length(s)= 0 then exit;
  if PopWs then PopWhiteSpace;

  if (s[0] <> ptgParen) and (s[0] <> ptgAttr) then //Those are "transparent" for reference ops.
  begin
		LastRefOp := Length(FParsedData);
	end;

  SetLength(FParsedData, Length(FParsedData)+ Length(s));
  move(s[0], FParsedData[Length(FParsedData)-Length(s)], Length(s));
end;

procedure TParseString.AddParsedArray(const s: array of byte);
begin
  if Length(s)= 0 then exit;
  SetLength(FParsedArrayData, Length(FParsedArrayData)+ Length(s));
  move(s[0], FParsedArrayData[Length(FParsedArrayData)-Length(s)], Length(s));
end;

function TParseString.TotalSize: integer;
begin
  Result:=2+Length(FParsedData)+Length(FParsedArrayData);
end;

procedure TParseString.CopyToPtr(const Ptr: PArrayOfByte; const aPos: integer);
var
  w: word;
begin
  w:=Length(FParsedData)+Length(FParsedArrayData);
  Move(w,ptr[aPos],2);
  Move(FParsedData[0],ptr[aPos+2], Length(FParsedData));
  Move(FParsedArrayData[0],ptr[aPos+Length(FParsedData)+2], Length(FParsedArrayData));
end;

procedure TParseString.CopyToPtrNoLen(const Ptr: PArrayOfByte; const destIndex: integer);
begin
  Move(FParsedData[0],ptr[destIndex], Length(FParsedData));
  Move(FParsedArrayData[0],ptr[destIndex+Length(FParsedData)], Length(FParsedArrayData));
end;

function TParseString.IsNumber(const c: UTF16Char): boolean;
begin
  Result:=(ord(c)<255) and (AnsiChar(c) in ['0'..'9'])
end;

function TParseString.IsAlpha(const c: UTF16Char): boolean;
begin
{$IFDEF DELPHIXE4UP}
  Result := c.IsLetter or (c = '_') or (c = '\');
{$ELSE}
{$IFDEF DELPHI2008UP}
  Result := TCharacter.IsLetter(c) or (c = '_') or (c = '\');
{$ELSE}
  Result:=(ord(c)<255) and (AnsiChar(c) in ['A'..'Z','_','\','a'..'z'])
{$ENDIF}
{$ENDIF}
end;

function TParseString.IsAZ(const c: UTF16Char): boolean;
begin
  Result:=(ord(c)<255) and (AnsiChar(c) in ['A'..'Z','a'..'z'])
end;

function TParseString.ATo1(const c: UTF16Char): integer;
begin
  Result:= ord(UpCase(AnsiChar(c)))-Ord('A')+1;
end;

end.
