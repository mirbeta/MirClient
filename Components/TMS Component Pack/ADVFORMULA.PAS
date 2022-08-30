{ ************************************************************************** }
{ TAdvFormula component                                                      }
{ for Delphi & C++Builder                                                    }
{                                                                            }
{ written by                                                                 }
{ TMS Software                                                               }
{ copyright © 2000 - 2014                                                    }
{ Email : info@tmssoftware.com                                               }
{ Web : http://www.tmssoftware.com                                           }
{                                                                            }
{ The source code is given as is. The author is not responsible              }
{ for any possible damage done due to the use of this code.                  }
{ The component can be freely used in any application. The complete          }
{ source code remains property of the author and may not be distributed,     }
{ published, given or sold in any form as such. No parts of the source       }
{ code can be included in any other component or application without         }
{ written authorization of the author.                                       }
{ ************************************************************************** }

unit AdvFormula;
{$I TMSDEFS.INC}

interface

uses
  Classes, SysUtils, Windows, StdCtrls;

const
  MAXVARS = 100;

type
  TokenType = (Delimiter, Non, variable, Digit, endExpr, Error, Func);
  TokenPtr = ^TokenRec;

  TokenRec = Record
    Next: TokenPtr;
    Start, Close: smallint;
  end;

  TIsCustomFunction = procedure(sender: tobject; var Func: string; var match: boolean) of object;
  TCalcCustomFunction = procedure(sender: tobject; var Func: string; var param: double) of object;
  TIsCompRefEvent = procedure(sender: tobject; compname: string; compobj: TCustomEdit) of object;

  TExpressionVariables = array [0..MAXVARS] of double;

  TAdvFormula = class(tobject)
  private
    i: integer; { scan position }
    varlen: integer;
    fOwner: TComponent;
    fExpression: string;
    fErrorPosition: integer;
    fIsCustomFunction: TIsCustomFunction;
    fCalcCustomFunction: TCalcCustomFunction;
    fIsCompRefEvent: TIsCompRefEvent;
    function ReadWord: string;
    function ReadNumber: double;
    procedure SkipBlanks;
    function GetToken: TokenType;
    function MatchFunc(match: string; var Res: double; var n: TokenType): boolean;
    function MatchToken(match: string): boolean;
    function doPI(var r: double): boolean;
    function doE(var r: double): boolean;
    function DoNow(var r: double): boolean;
    function DoToday(var r: double): boolean;
    function DoFrac(var Res: double; var n: TokenType): boolean;
    function DoFact(var Res: double; var n: TokenType): boolean;
    function DoInt(var Res: double; var n: TokenType): boolean;
    function DoChs(var Res: double; var n: TokenType): boolean;
    function DoSin(var Res: double; var n: TokenType): boolean;
    function DoCos(var Res: double; var n: TokenType): boolean;
    function DoTan(var Res: double; var n: TokenType): boolean;
    function DoCoTan(var Res: double; var n: TokenType): boolean;
    function DoSinh(var Res: double; var n: TokenType): boolean;
    function DoCosh(var Res: double; var n: TokenType): boolean;
    function DoTanh(var Res: double; var n: TokenType): boolean;
    function DoCoTanh(var Res: double; var n: TokenType): boolean;
    function DoMonth(var Res: double; var n: TokenType): boolean;
    function DoYear(var Res: double; var n: TokenType): boolean;
    function DoDay(var Res: double; var n: TokenType): boolean;
    function DoDayOfWeek(var Res: double; var n: TokenType): boolean;
    function DoMinute(var Res: double; var n: TokenType): boolean;
    function DoSecond(var Res: double; var n: TokenType): boolean;
    function DoHour(var Res: double; var n: TokenType): boolean;
    function DoExp(var Res: double; var n: TokenType): boolean;
    function DoLn(var Res: double; var n: TokenType): boolean;
    function DoLog10(var Res: double; var n: TokenType): boolean;
    function DoLog2(var Res: double; var n: TokenType): boolean;
    function DoAbs(var Res: double; var n: TokenType): boolean;
    function DoArcTan(var Res: double; var n: TokenType): boolean;
    function DoArcCoTan(var Res: double; var n: TokenType): boolean;
    function DoRound(var Res: double; var n: TokenType): boolean;
    function DoTrunc(var Res: double; var n: TokenType): boolean;
    function DoDegrees(var Res: double; var n: TokenType): boolean;
    function DoRadians(var Res: double; var n: TokenType): boolean;
    function DoRand(var Res: double; var n: TokenType): boolean;
    function DoSqr(var Res: double; var n: TokenType): boolean;
    function DoCube(var Res: double; var n: TokenType): boolean;
    function DoSqrt(var Res: double; var n: TokenType): boolean;
    function DoArcSin(var Res: double; var n: TokenType): boolean;
    function DoArcCos(var Res: double; var n: TokenType): boolean;
    procedure DoFunc(var Res: double; var n: TokenType);
    procedure Primitive(var Res: double; var n: TokenType);
    function Sign(Number: double): double;
    procedure Level6(var Res: double; var n: TokenType);
    procedure Level5(var Res: double; var n: TokenType);
    procedure Level4(var Res: double; var n: TokenType);
    procedure Level3(var Res: double; var n: TokenType);
    procedure Level2(var Res: double; var n: TokenType);
    procedure Level1(var Res: double; var n: TokenType);
    function GetExpr(var Valid: boolean): double;
    procedure DoErr(var n: TokenType);
    procedure GetNextVar(var full, expr, c, r: string);
    function IsExprVar: boolean;
    function GetExprVar: double;
    function IsCompRefFunc(match: string): boolean;
    function SolveCompRefFunc(match: string; var Res: double): boolean;
  public
    V: TExpressionVariables; { fExpression program variables }
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;
    property Expression: string read fExpression write fExpression;
    property ErrorPosition: integer read fErrorPosition write fErrorPosition;
    function Calc(var r: double): boolean;

    property IsCustomFunction: TIsCustomFunction read fIsCustomFunction write fIsCustomFunction;
    property CalcCustomFunction: TCalcCustomFunction read fCalcCustomFunction write fCalcCustomFunction;
    property OnIsCompRef: TIsCompRefEvent read fIsCompRefEvent write fIsCompRefEvent;
  end;

implementation

{$IFNDEF DELPHI_UNICODE}
type
  TCharSet = set of char;

function CharInSet(c: char; s: TCharSet): boolean;
begin
  Result := c in s;
end;
{$ENDIF}

{$I DELPHIXE.INC}

function VarPos(su, s: string; var vp: integer): integer;
begin
  vp := Pos(su, s);
  result := vp;
end;

function CvtText(s: string): string;
var
  vp: integer;
begin
  while (VarPos(ThousandSeparator, s, vp) > 0) do
    Delete(s, vp, 1);

  Result := '';
  for vp := 1 to length(s) do
  begin
    if (s[vp] = DecimalSeparator) then
      Result := Result + '.';

    if CharInSet(s[vp],['0' .. '9']) then
      Result := Result + s[vp];
  end;
end;

(* *****************************************************************************
  *                                    DoErr                                    *
  ***************************************************************************** *)
procedure TAdvFormula.DoErr(var n: TokenType);
begin
  n := Error;
  FErrorPosition := i; { globl err pos }
end; { doErr }

(* *****************************************************************************
  *                                  ReadWord                                   *
  ***************************************************************************** *)
function TAdvFormula.ReadWord: string;
var
  WordIn: string;
begin
  WordIn := '';
  while (i <= length(FExpression)) and (not(CharInSet(FExpression[i],[' ', '\', ';', '*', '/', '^', '+', '=', '-', '%', '(', ')']))) do
  begin
    WordIn := WordIn + UpCase(fExpression[i]);
    Inc(i);
  end;
  ReadWord := WordIn;
end; { readWord }

(* *****************************************************************************
  *                                 ReadNumber                                  *
  ***************************************************************************** *)
function TAdvFormula.ReadNumber: double;
var
  Number: double;
  Code: integer;
  StrNum: string;
begin
  StrNum := ReadWord;
  if StrNum[1] = '.' then
    StrNum := '0' + StrNum;
  Val(StrNum, Number, Code);
  if Code <> 0 then
    Number := 0;
  ReadNumber := Number;
end; { readNumber }

(* *****************************************************************************
  *                                 SkipBlanks                                  *
  ***************************************************************************** *)
procedure TAdvFormula.SkipBlanks;
begin
  while fExpression[i] = ' ' do
    Inc(i);
end; { skipBlanks }

function TAdvFormula.IsCompRefFunc(match: string): boolean;
var
  mc: TComponent;
begin
  result := false;
  mc := fOwner.FindComponent(match);
  if (mc <> nil) then
    result := (mc is TCustomEdit) or (mc is TLabel);
{$IFDEF TMSDEBUG}
  if result then
    outputdebugstring(pchar('Found match: ' + mc.name));
{$ENDIF}
  if result then
  begin
    if (mc is TCustomEdit) then
      if assigned(fIsCompRefEvent) then
        fIsCompRefEvent(self, mc.name, (mc as TCustomEdit));
  end;

end;

function TAdvFormula.SolveCompRefFunc(match: string; var Res: double): boolean;
var
  err: integer;
  mc: TComponent;
begin
  result := false;
  mc := fOwner.FindComponent(match);
  if (mc <> nil) then
  begin
    if (mc is TCustomEdit) then
    begin
      Val(CvtText((mc as TCustomEdit).Text), Res, err);
      if err = 0 then
        result := true;
    end;

    if (mc is TLabel) then
    begin
      Val(CvtText((mc as TLabel).Caption), Res, err);
      if err = 0 then
        result := true;
    end;
  end;
end;

procedure TAdvFormula.GetNextVar(var full, expr, c, r: string);
var
  j: smallint;
  allow: set of ansichar;
  gotb: boolean;

begin
  j := i;
  allow := ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '(', ')', ':', '[', ']'];
  full := '';
  expr := '';
  c := '';
  r := '';
  gotb := false;

  while (j <= length(FExpression)) and (CharInSet(FExpression[j], Allow)) do
  begin
    if not (CharInSet(FExpression[j],[']', ')'])) then
      full := full + FExpression[j];

    if (CharInSet(FExpression[j],['(', ')'])) then
      gotb := true;

    if (CharInSet(FExpression[j], [']', ')'])) then
      allow := [];

    if (CharInSet(FExpression[j], ['a' .. 'z', 'A' .. 'Z'])) and not gotb then
      expr := expr + FExpression[j];

    if (CharInSet(FExpression[j], ['0' .. '9'])) and not gotb then
    begin
      if expr <> '' then
        c := expr;
      expr := '';
      r := r + FExpression[j];
    end;

    Inc(j);
  end;

  if (r = '') then
    c := '';

end;

function TAdvFormula.IsExprVar: boolean;
var
  j: smallint;
  r, c, f, a: string;
  varidx, Error: integer;

begin
  Result := false;
  j := i;
  if not(CharInSet(FExpression[j], ['a' .. 'z', 'A' .. 'Z', '(', ')', '[', ']'])) then
    Exit;

  GetNextVar(a, f, c, r);
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('getnextvar :' + a + ' - ' + f + ' - ' + c + ' - ' + r));
  {$ENDIF}
  if IsCompRefFunc(a) then
  begin
    varlen := length(a) - 2;
    result := true;
    exit;
  end
  else
  begin
    Val(r, varidx, Error);
    result := (c = 'V') and (varidx in [0 .. 99]) and (Error = 0);
    if result then
      varlen := length(f) + length(c) + length(r) - 1
    else
      varlen := 0;
  end;
end;

function TAdvFormula.GetExprVar: double;
var
  r, c, f, a: string;
  varidx, Error: integer;

begin
  r := '';
  c := '';
  f := '';

  GetNextVar(a, f, c, r);

  Val(r, varidx, Error);
{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('getcellvar :' + a + ' - ' + f + ' - ' + c + ' - ' + r));
{$ENDIF}
  if IsCompRefFunc(a) then
  begin
    SolveCompRefFunc(a, result);
    i := i + length(a);
  end
  else
  begin
    if (c = 'V') and (varidx in [0 .. 99]) and (Error = 0) then
    begin
      result := V[varidx];
      i := i + length(f) + length(c) + length(r);
    end
    else
      result := 0;
  end;
end;

(* *****************************************************************************
  *                                  GetToken                                   *
  ***************************************************************************** *)
function TAdvFormula.GetToken: TokenType;
var
  n: TokenType;

begin
  SkipBlanks;
  if CharInSet(FExpression[i], ['+', '-', '/', '*', '=', '^', '%', '(', ')', '[', ']']) then
    n := Delimiter
    // else if (fExpression [i] in ['v','V']) then begin
  else
  if IsExprVar then
  begin
    n := variable;
  end
  else if CharInSet(FExpression[i], ['0' .. '9', '.']) then
    n := Digit
  else if FExpression[i] = ';' then
    n := endExpr
  else if CharInSet(FExpression[i], ['a' .. 'z', 'A' .. 'Z']) then
    n := Func
  else
    n := Error;

  GetToken := n;
end; { getToken }

(* *****************************************************************************
  *                                  MatchFunc                                  *
  ***************************************************************************** *)
function TAdvFormula.MatchFunc(match: string; var Res: double; var n: TokenType): boolean;
var
  j: smallint;
begin
  j := i; { restore i if no match }
  if (ReadWord = match) then
  begin
    MatchFunc := true;
    SkipBlanks;
    if (fExpression[i] <> '(') then
      DoErr(n)
    else
    begin
      Inc(i);
      n := GetToken;
      Level1(Res, n);
      SkipBlanks; { Reach closing parenthasis }
      if fExpression[i] <> ')' then
        DoErr(n);
      Inc(i);
      SkipBlanks;
    end;
  end
  else
  begin
    MatchFunc := false;
    i := j; { no Func Match, restore }
  end;
end; { matchFunc }

(* *****************************************************************************
  *                                 MatchToken                                  *
  ***************************************************************************** *)
function TAdvFormula.MatchToken(match: string): boolean;
var
  j: smallint;
begin
  j := i;
  if (ReadWord = match) then
    MatchToken := true
  else
  begin
    MatchToken := false;
    i := j;
  end; { else }
end; { matchToken }

(* *****************************************************************************
  *                                    doPI                                     *
  ***************************************************************************** *)
function TAdvFormula.doPI(var r: double): boolean;
begin
  doPI := MatchToken('PI');
  r := pi;
end; { doPI }

(* *****************************************************************************
  *                                     doE                                     *
  ***************************************************************************** *)
function TAdvFormula.doE(var r: double): boolean;
begin
  doE := MatchToken('E');
  r := exp(1.0);
end; { doE }

(* *****************************************************************************
  *                                     doToday                                 *
  ***************************************************************************** *)
function TAdvFormula.DoToday(var r: double): boolean;
var
  da, mo, ye: word;
begin
{$IFDEF DELPHI3_LVL}
  DoToday := MatchToken('TODAY');
  decodedate(now, ye, mo, da);
  r := double(encodedate(ye, mo, da));
{$ELSE}
  r := 0;
{$ENDIF}
end; { doE }

(* *****************************************************************************
  *                                     doNow                                   *
  ***************************************************************************** *)
function TAdvFormula.DoNow(var r: double): boolean;
begin
{$IFDEF DELPHI3_LVL}
  DoNow := MatchToken('NOW');
  r := double(now);
{$ELSE}
  r := 0;
{$ENDIF}
end; { doE }

(* *****************************************************************************
  *                                    DoSin                                    *
  ***************************************************************************** *)
function TAdvFormula.DoSin(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('SIN', Res, n);
  Res := sin(Res);
  DoSin := r;
end; { doSin }

(* *****************************************************************************
  *                                    DoSinh                                    *
  ***************************************************************************** *)
function TAdvFormula.DoSinh(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('SINH', Res, n);
  Res := 0.5 * (exp(Res) - exp(-Res));
  DoSinh := r;
end; { doSinh }

(* *****************************************************************************
  *                                    DoCosh                                    *
  ***************************************************************************** *)
function TAdvFormula.DoCosh(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('COSH', Res, n);
  Res := 0.5 * (exp(Res) + exp(-Res));
  DoCosh := r;
end; { doCosh }

(* *****************************************************************************
  *                                    DoTanh                                    *
  ***************************************************************************** *)
function TAdvFormula.DoTanh(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  ep, en: double;
begin
  r := MatchFunc('TANH', Res, n);
  ep := exp(Res);
  en := exp(-Res);
  Res := (ep - en) / (ep + en);
  DoTanh := r;
end; { doSin }

(* *****************************************************************************
  *                                    DoCotanh                                    *
  ***************************************************************************** *)
function TAdvFormula.DoCoTanh(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  ep, en: double;

begin
  r := MatchFunc('COTANH', Res, n);
  ep := exp(Res);
  en := exp(-Res);
  Res := (ep + en) / (ep - en);
  DoCoTanh := r;
end; { doSin }

(* *****************************************************************************
  *                                    DoExp                                    *
  ***************************************************************************** *)
function TAdvFormula.DoExp(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('EXP', Res, n);
  Res := exp(Res);
  DoExp := r;
end; { doSin }

(* *****************************************************************************
  *                                    DoCos                                    *
  ***************************************************************************** *)
function TAdvFormula.DoCos(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('COS', Res, n);
  Res := cos(Res);
  DoCos := r;
end; { doCos }

(* *****************************************************************************
  *                                    DoLn                                     *
  ***************************************************************************** *)
function TAdvFormula.DoLn(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('LN', Res, n);
  if (Res > 0.0) then
    Res := ln(Res)
  else
    DoErr(n);
  DoLn := r;
end; { doLn }

(* *****************************************************************************
  *                                   DoLog10                                   *
  ***************************************************************************** *)
function TAdvFormula.DoLog10(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('LOG10', Res, n);
  if (Res > 0.0) then
    Res := ln(Res) / ln(10.0)
  else
    DoErr(n);
  DoLog10 := r;
end; { doLog10 }

(* *****************************************************************************
  *                                   DoLog2                                    *
  ***************************************************************************** *)
function TAdvFormula.DoLog2(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('LOG2', Res, n);
  if (Res > 0.0) then
    Res := ln(Res) / ln(2.0)
  else
    DoErr(n);
  DoLog2 := r;
end; { doLog2 }

(* *****************************************************************************
  *                                    DoAbs                                    *
  ***************************************************************************** *)
function TAdvFormula.DoAbs(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ABS', Res, n);
  Res := Abs(Res);
  DoAbs := r;
end; { doAbs }

(* *****************************************************************************
  *                                  DoArcTan                                   *
  ***************************************************************************** *)
function TAdvFormula.DoArcTan(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ATAN', Res, n);
  Res := ArcTan(Res);
  DoArcTan := r;
end; { doArcTan }

(* *****************************************************************************
  *                                  DoArcCoTan                                   *
  ***************************************************************************** *)
function TAdvFormula.DoArcCoTan(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ACOTAN', Res, n);
  if (Res <> 0.0) then
    Res := ArcTan(1 / Res)
  else
    DoErr(n);
  DoArcCoTan := r;
end; { doArcCoTan }

(* *****************************************************************************
  *                                    DoSqr                                    *
  ***************************************************************************** *)
function TAdvFormula.DoSqr(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('SQR', Res, n);
  Res := Sqr(Res);
  DoSqr := r;
end; { doSqr }

(* *****************************************************************************
  *                                   DoSqrt                                    *
  ***************************************************************************** *)
function TAdvFormula.DoSqrt(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('SQRT', Res, n);
  Res := Sqrt(Res);
  DoSqrt := r;
end; { doSqrt }

(* *****************************************************************************
  *                                    DoCube                                   *
  ***************************************************************************** *)
function TAdvFormula.DoCube(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('CUBE', Res, n);
  Res := Sqr(Res) * Res;
  DoCube := r;
end; { doCube }

(* *****************************************************************************
  *                                    DoChs                                    *
  ***************************************************************************** *)
function TAdvFormula.DoChs(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('CHS', Res, n);
  Res := -Res;
  DoChs := r;
end; { doChs }

(* *****************************************************************************
  *                                    DoTan                                    *
  ***************************************************************************** *)
function TAdvFormula.DoTan(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('TAN', Res, n);
  if (cos(Res) <> 0) then
    Res := sin(Res) / cos(Res)
  else
    DoErr(n);
  DoTan := r;
end; { doTan }

(* *****************************************************************************
  *                                   DoCoTan                                   *
  ***************************************************************************** *)
function TAdvFormula.DoCoTan(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('COTAN', Res, n);
  if (sin(Res) <> 0) then
    Res := cos(Res) / sin(Res)
  else
    DoErr(n);
  DoCoTan := r;
end; { doCoTan }

(* *****************************************************************************
  *                                  DoArcSin                                   *
  ***************************************************************************** *)
function TAdvFormula.DoArcSin(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ASIN', Res, n);
  if (Abs(Res) < 1.0) then
    Res := ArcTan(Res / Sqrt(1 - Res * Res))
  else
    DoErr(n);
  DoArcSin := r;
end; { doArcSin }

(* *****************************************************************************
  *                                  DoArcCos                                   *
  ***************************************************************************** *)
function TAdvFormula.DoArcCos(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ACOS', Res, n);
  if ((Res <> 0.0) and (Res < 1.0)) then
    Res := ArcTan(Sqrt(1 - Res * Res) / Res)
  else
    DoErr(n);
  DoArcCos := r;
end; { doArcCos }

(* *****************************************************************************
  *                                  DoInt                                      *
  ***************************************************************************** *)
function TAdvFormula.DoInt(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('INT', Res, n);
  Res := int(Res);
  DoInt := r;
end; { doInt }

(* *****************************************************************************
  *                                  DoFrac                                     *
  ***************************************************************************** *)
function TAdvFormula.DoFrac(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('FRAC', Res, n);
  Res := frac(Res);
  DoFrac := r;
end; { doFrac }

(* *****************************************************************************
  *                                  DoFact                                     *
  ***************************************************************************** *)
function TAdvFormula.DoFact(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  i, j: integer;

begin
  r := MatchFunc('FACT', Res, n);
  j := round(Res);
  Res := 1;
  for i := 1 to j do
    Res := Res * i;
  DoFact := r;
end; { doFact }

(* *****************************************************************************
  *                                  DoRound                                     *
  ***************************************************************************** *)
function TAdvFormula.DoRound(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('ROUND', Res, n);
  Res := round(Res);
  DoRound := r;
end; { doFrac }

(* *****************************************************************************
  *                                  DoTrunc                                     *
  ***************************************************************************** *)
function TAdvFormula.DoTrunc(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('TRUNC', Res, n);
  Res := trunc(Res);
  DoTrunc := r;
end; { doTrunc }

(* *****************************************************************************
  *                                  DoRand                                     *
  ***************************************************************************** *)
function TAdvFormula.DoRand(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('RAND', Res, n);
  Res := random(round(Res));
  DoRand := r;
end; { doRand }

(* *****************************************************************************
  *                                  DoRadians                                     *
  ***************************************************************************** *)
function TAdvFormula.DoRadians(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('RADIANS', Res, n);
  while (Res > 360) do
    Res := Res - 360;
  while (Res < -360) do
    Res := Res + 360;
  Res := Res / 180 * pi;
  DoRadians := r;
end; { doFrac }

function TAdvFormula.DoDegrees(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  r := MatchFunc('DEGREES', Res, n);
  while (Res > 2 * pi) do
    Res := Res - 2 * pi;
  while (Res < -2 * pi) do
    Res := Res + 2 * pi;
  Res := Res / pi * 180;
  DoDegrees := r;
end; { doFrac }

function TAdvFormula.DoYear(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  da, mo, ye: word;
begin
  DoYear := false;
  r := MatchFunc('YEAR', Res, n);
  if r then
    try
      decodedate(tdatetime(Res), ye, mo, da);
      Res := ye;
      DoYear := true;
    except
      DoYear := false;
    end;
end; { doInt }

function TAdvFormula.DoMonth(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  da, mo, ye: word;
begin
  DoMonth := false;
  r := MatchFunc('MONTH', Res, n);
  if r then
    try
      decodedate(tdatetime(Res), ye, mo, da);
      Res := mo;
      DoMonth := true;
    except
      DoMonth := false;
    end;
end; { doInt }

function TAdvFormula.DoDay(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  da, mo, ye: word;
begin
  DoDay := false;
  r := MatchFunc('DAY', Res, n);
  if r then
    try
      decodedate(tdatetime(Res), ye, mo, da);
      Res := da;
      DoDay := true;
    except
      DoDay := false;
    end;
end; { doDay }

function TAdvFormula.DoDayOfWeek(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
begin
  DoDayOfWeek := false;
  r := MatchFunc('WEEKDAY', Res, n);
  if r then
    try
      Res := dayofweek(Res);
      DoDayOfWeek := true;
    except
      DoDayOfWeek := false;
    end;
end; { doInt }

function TAdvFormula.DoHour(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  ho, mi, se, se100: word;
begin
  DoHour := false;
  r := MatchFunc('HOUR', Res, n);
  if r then
    try
      decodetime(tdatetime(Res), ho, mi, se, se100);
      Res := ho;
      DoHour := true;
    except
      DoHour := false;
    end;

end; { doInt }

function TAdvFormula.DoMinute(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  ho, mi, se, se100: word;
begin
  DoMinute := false;
  r := MatchFunc('MINUTE', Res, n);
  if r then
    try
      decodetime(tdatetime(Res), ho, mi, se, se100);
      Res := mi;
      DoMinute := r;
    except
      DoMinute := false;
    end;
end; { doInt }

function TAdvFormula.DoSecond(var Res: double; var n: TokenType): boolean;
var
  r: boolean;
  ho, mi, se, se100: word;
begin
  DoSecond := false;
  r := MatchFunc('SECOND', Res, n);
  if r then
    try
      decodetime(tdatetime(Res), ho, mi, se, se100);
      Res := se;
      DoSecond := true;
    except
      DoSecond := false;
    end;
end; { doInt }

(* *****************************************************************************
  *                                   DoFunc                                    *
  ***************************************************************************** *)
procedure TAdvFormula.DoFunc(var Res: double; var n: TokenType);
var
  match: boolean;
  s: string;
begin
  if Assigned(fIsCustomFunction) and assigned(fCalcCustomFunction) then
  begin
    s := copy(FExpression, i, $FFFF);
    match := false;
    FIsCustomFunction(self, s, match);
    if match then
    begin
      if MatchFunc(s, Res, n) then
      begin
        FCalcCustomFunction(self, s, Res);
        Exit;
      end;
    end;
  end;

  case fExpression[i] of
    'c', 'C':
      begin
        if not(DoCosh(Res, n)) then
          if not(DoCos(Res, n)) then
            if not(DoCoTanh(Res, n)) then
              if not(DoCoTan(Res, n)) then
                if not DoCube(Res, n) then
                  if not DoChs(Res, n) then
                    DoErr(n);
      end;
    'l', 'L':
      begin
        if not(DoLn(Res, n)) then
          if not(DoLog10(Res, n)) then
            if not(DoLog2(Res, n)) then
              DoErr(n);
      end;
    'a', 'A':
      begin
        if not(DoAbs(Res, n)) then
          if not(DoArcTan(Res, n)) then
            if not(DoArcSin(Res, n)) then
              if not(DoArcCos(Res, n)) then
                if not(DoArcCoTan(Res, n)) then
                  DoErr(n);
      end;
    'd', 'D':
      begin
        if not(DoDegrees(Res, n)) then
          if not(DoDay(Res, n)) then
            DoErr(n);
      end;
    'e', 'E':
      begin
        if not(DoExp(Res, n)) then
          if not(doE(Res)) then
            DoErr(n);
      end;
    'f', 'F':
      begin
        if not(DoFrac(Res, n)) then
          if not DoFact(Res, n) then
            DoErr(n);
      end;
    'h', 'H':
      begin
        if not(DoHour(Res, n)) then
          DoErr(n);
      end;
    'i', 'I':
      begin
        if not(DoInt(Res, n)) then
          DoErr(n);
      end;
    'm', 'M':
      begin
        if not(DoMinute(Res, n)) then
          if not(DoMonth(Res, n)) then
            DoErr(n);
      end;
    'n', 'N':
      begin
        if not(DoNow(Res)) then
          DoErr(n);
      end;
    'p', 'P':
      begin
        if not(doPI(Res)) then
          DoErr(n);
      end;
    'r', 'R':
      begin
        if not(DoRound(Res, n)) then
          if not DoRand(Res, n) then
            if not DoRadians(Res, n) then
              DoErr(n);
      end;
    's', 'S':
      begin
        if not(DoSinh(Res, n)) then
          if not(DoSin(Res, n)) then
            if not(DoSqr(Res, n)) then
              if not(DoSqrt(Res, n)) then
                if not(DoSecond(Res, n)) then
                  DoErr(n);
      end;
    't', 'T':
      begin
        if not(DoTanh(Res, n)) then
          if not(DoTan(Res, n)) then
            if not DoTrunc(Res, n) then
              if not DoToday(Res) then
                DoErr(n);
      end;
    'w', 'W':
      begin
        if not(DoDayOfWeek(Res, n)) then
          DoErr(n);
      end;
    'y', 'Y':
      begin
        if not(DoYear(Res, n)) then
          DoErr(n);
      end;

  else
    DoErr(n);
  end; { case }
end;

procedure TAdvFormula.Primitive(var Res: double; var n: TokenType);
var
  s:string;
begin
  s := copy(FExpression, i, length(FExpression));
  if n = variable then
  begin
    // Inc(i);
    SkipBlanks;
    Res := GetExprVar;
    {
      if fExpression[i] in ['0'..'9'] then begin
      res := V [fExpression [i]];
      Inc(i);
      end else DoErr(n);
    }
  end
  else
    if n = Digit then
      Res := ReadNumber
    else if (n = Func) then
      DoFunc(Res, n);

  SkipBlanks;
end;

procedure TAdvFormula.Level6(var Res: double; var n: TokenType);
{ deal with parenthsis }
begin
  if (n = Delimiter) and (FExpression[i] = '(') then
  begin
    Inc(i);
    n := GetToken;
    Level1(Res, n);
    SkipBlanks; { Reach closing parenthasis }
    if fExpression[i] <> ')' then
      DoErr(n);
    Inc(i);
    SkipBlanks;
  end
  else
    Primitive(Res, n);
end;

procedure TAdvFormula.Level5(var Res: double; var n: TokenType);
{ Unary +,- }
var
  op: char;

begin
  op := FExpression[i];
  if CharInSet(op, ['-', '+']) then
    Inc(i);
  n := GetToken;
  Level6(Res, n);
  if op = '-' then
    Res := -(Res);
end;

function TAdvFormula.Sign(Number: double): double;
{ -1 if Number < 0 , 1 otherwise }
begin
  if (Number < 0.0) then
    Sign := -1.0
  else
    Sign := 1.0;
end;

procedure TAdvFormula.Level4(var Res: double; var n: TokenType);

var
  Hold: double;

begin
  Level5(Res, n);
  if (n <> Error) then
    if fExpression[i] = '^' then
    begin
      Inc(i);
      n := GetToken;
      Level4(Hold, n);
      if (Res = 0.0) then
        if (Hold = 0.0) then
          Res := 1.0
        else
          Res := 0.0
        else
          Res := Sign(Res) * exp(Hold * ln(Abs(Res)));
      { mimic power }
      SkipBlanks;
    end;
end;

procedure TAdvFormula.Level3(var Res: double; var n: TokenType);

{ Multipy / divide 2 factors }

var
  Hold: double;
  op: char;

begin
  Level4(Res, n);
  if (n <> Error) then
  begin
    SkipBlanks;
    while CharInSet(FExpression[i], ['*', '/', '%']) do
    begin
      op := FExpression[i];
      Inc(i);
      n := GetToken;
      Level4(Hold, n);
      if op = '*' then
        Res := Res * Hold
      else
      begin
        if (Hold = 0.0) then
          DoErr(n)
        else if op = '/' then
          Res := Res / Hold
        else
          Res := trunc(Res) mod trunc(Hold);
      end;
      SkipBlanks;
    end;
  end; { not error }
end;

procedure TAdvFormula.Level2(var Res: double; var n: TokenType);
{ add/sub 2 terms }
var
  Hold: double;
  op: char;

begin
  Level3(Res, n);
  if (n <> Error) then
  begin
    SkipBlanks;
    While (CharInSet(FExpression[i], ['+', '-'])) do
    begin
      op := fExpression[i];
      Inc(i);
      n := GetToken;
      Level3(Hold, n);
      if op = '+' then
        Res := Res + Hold
      else
        Res := Res - Hold;
      SkipBlanks;
    end; { while }
  end; { not error }
end;

procedure TAdvFormula.Level1(var Res: double; var n: TokenType);
var
  // Slot:Char;
  m: TokenType;
  j: smallint;

begin
  if n = variable then
  begin
    j := i; { save  i }
    Inc(i, varlen);
    SkipBlanks;
    // Slot:=#0;
    // if (fExpression [i] in ['0'..'9']) then
    // Slot := fExpression [i];
    // else
    // DoErr(n);

    if (n <> Error) then
    begin
      Inc(i);
      m := GetToken;
      if ((m = Delimiter) and (fExpression[i] = '=') and (i <= length(fExpression))) then
      begin
        Inc(i);
        n := GetToken;
        Level2(Res, n);
        V[100] := Res;
      end
      else
      begin
        i := j; { restore .. }
        Level2(Res, n);
      end; { not a vx = ... }
    end { n <> error }
  end
  { variable case } else
    Level2(Res, n);
end;

{ Deal with assinment here }
function TAdvFormula.GetExpr(var Valid: boolean): double;
var
  Res: double;
  n: TokenType;
begin
  i := 1;
  Res := 0; { if no res returned }
  n := GetToken;

  if not (n in [endExpr, Non]) then
    Level1(Res, n);

  if (n <> endExpr) and (i < length(fExpression)) then
    Dec(i);

  GetExpr := Res;
  Valid := not (n = Error);
end;

{ TAdvFormula }
constructor TAdvFormula.Create(AOwner: TComponent);
var
  c: smallint;
begin
  inherited Create;
  for c := 0 to 100 do
    V[c] := 0.0;
  FOwner := AOwner;
end;

function TAdvFormula.Calc(var r: double): boolean;
begin
  r := GetExpr(Result);
end;

destructor TAdvFormula.Destroy;
begin
  inherited;
end;

end.
