{*************************************************************************}
{ TGRIDFORMULA component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{    TMS Software                                                         }
{    copyright © 1996 - 2015                                              }
{    Email:info@tmssoftware.com                                           }
{    Web:http://www.tmssoftware.com                                       }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
                           
unit AdvPars;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Dialogs, SysUtils, Grids, BaseGrid, AdvGrid, AdvUtil, Variants
  {$IFDEF TMSDEBUG}
  ,TMSUtil
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.Types
  {$ENDIF}
  ;

const
  Error_NoError               =  0;
  Error_NoFormula             =  1;
  Error_DivisionByZero        =  2;
  Error_InvalidValue          =  3;
  Error_InvalidCellRef        =  4;
  Error_InvalidRangeRef       =  5;
  Error_InvalidGridRef        =  6;
  Error_InvalidNrOfParams     =  7;
  Error_CircularReference     =  8;
  Error_NoOpenParenthesis     =  9;
  Error_NoCloseParenthesis    = 10;
  Error_PrematureEndOfFormula = 11;
  Error_UnknownError          = 12;
  Error_InvalidQualifier      = 13;
  Error_InvalidTokenAtPosition= 14;
  Error_Overflow              = 15;
  Error_Underflow             = 16;
  Error_CircularRange         = 17;
  Error_NoDataSource          = 18;
  Error_NoDataSet             = 19;
  Error_NoDataSetActive       = 20;

  Error_MaxErrorNumber        = 100;

  {$IFNDEF DELPHI_UNICODE}
  varUString = $0102;
  {$ENDIF}

type
  TokenType = (Delimiter,Non,Variable,Digit,EndExpr,Error,Func,Operator,Text);

  TokenPtr = ^TokenRec;
  TokenRec = record
    Next: TokenPtr;
    Start,Close: Smallint;
  end;                                

  TIsCustomFunction = procedure(sender:TObject;var func:string;var match:boolean) of object;
  TCalcCustomFunction = procedure(sender:TObject;var func:string;var param: double) of object;

  TErrorEvent = procedure(Sender: TObject;ACol,ARow: Integer; ErrType,ErrPos,ErrParam:Integer; ErrStr:string) of object;

  TDoubleItem = class(TObject)
  private
    FValue: Double;
    property Value: Double read FValue write FValue;
  end;

  {
  TParamList = class(TObject)
  private
    FList: Pointer;
    FCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Double);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Double read GetItem write SetItem;
  end;
  }

  TParamList = class(TList)
  private
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
  public
    procedure Add(Item: Double);
    procedure Clear; override;
    property Items[Index: Integer]: Double read GetItem write SetItem;
  end;

  TMathLib = class(TComponent)
  private
  public
    function HandlesConstant(Constant:string): Boolean; virtual;
    function GetConstant(Constant:string): double; virtual;
    function HandlesFunction(FuncName:string): Boolean; virtual;
    function HandlesStrFunction(FuncName:string): Boolean; virtual;
    function CalcFunction(FuncName:string;Params: TParamList;var ErrType,ErrParam:Integer): double; virtual;
    function CalcStrFunction(FuncName:string;Params: TStringList;var ErrType,ErrParam:Integer): string; virtual;
    function GetErrorMessage(ErrType:Integer):string;
    function GetEditHint(FuncName:string; ParamIndex: Integer):string; virtual;
  end;

  TGridBinderItem = class(TCollectionItem)
  private
    FGrid: TBaseGrid;
    FName: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TBaseGrid read FGrid write FGrid;
    property Name: string read FName write FName;
  end;

  TGridBinder = class;

  TGridBinderItems = class(TCollection)
  private
    FOwner: TGridBinder;
    function GetItem(Index: Integer): TGridBinderItem;
    procedure SetItem(Index: Integer; const Value: TGridBinderItem);
    function GetGridByName(Name: string): TBaseGrid;
  public
    function Add: TGridBinderItem;
    constructor Create(AOwner: TGridBinder);
    function GetOwner: TPersistent; override;
    property Items[Index: Integer]:TGridBinderItem read GetItem write SetItem;
    property GridByName[Name: string]: TBaseGrid read GetGridByName;
  end;

  TGridBinder = class(TComponent)
  private
    FGrids: TGridBinderItems;
    procedure SetGrids(const Value: TGridBinderItems);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grids: TGridBinderItems read FGrids write SetGrids;
  end;

  TLibBinderItem = class(TCollectionItem)
  private
    FMathLib: TMathLib;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MathLib: TMathLib read FMathLib write FMathLib;
  end;

  TLibBinder = class;

  TLibBinderItems = class(TCollection)
  private
    FOwner: TLibBinder;
    function GetItem(Index: Integer): TLibBinderItem;
    procedure SetItem(Index: Integer; const Value: TLibBinderItem);
  public
    function Add: TLibBinderItem;
    constructor Create(AOwner: TLibBinder);
    function GetOwner: TPersistent; override;
    property Items[Index: Integer]:TLibBinderItem read GetItem write SetItem;
  end;

  TLibBinder = class(TComponent)
  private
    FLibs: TLibBinderItems;
    procedure SetLibs(const Value: TLibBinderItems);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Libs: TLibBinderItems read FLibs write SetLibs;
  end;

  TGridFormula = class(TObject)
  private
    i: Integer; //scan position
    Varlen: Integer;
    FFirstError: Boolean;
    FOrigExpression: string;
    FExpression: string;
    FCol,FRow: Integer;
    FErrorPosition: Integer;
    FStringGrid: TAdvStringGrid;
    FCellNameList: TStringList;
    FIsCustomFunction:TIsCustomFunction;
    FCalcCustomFunction:TCalcCustomFunction;
    FUseRCNames:boolean;
    FMathLib: TLibBinder;
    FBinder: TGridBinder;
    FOnError: TErrorEvent;
    FTokenEnd: set of ansichar;
    FParamEnd: set of ansichar;
    function ReadWord:string;
    function ReadWordEx:string;
    function ReadStr:string;
    function ReadNumber: double;
    function GetToken: TokenType;
    function MatchFunc(Match:string; var Res: double; var n:TokenType):Boolean;
    function MatchFuncStr(Match:string; var Res: string; var n:TokenType):Boolean;
    function MatchDualParamFunc(Match:string; var Res1,Res2: double; var n:TokenType):Boolean;
    function MatchTriParamFunc(Match:string; var Res1,Res2,Res3: double; var n:TokenType):Boolean;
    function MatchVarParamFunc(Match:string; pl: TParamList; var n:TokenType; SkipEmptyCells: boolean = false):Boolean;
    function MatchVarStrParamFunc(Match:string; pl: TStringList; var n:TokenType):Boolean;
    function MatchRangeDualNumFunc(Match:string; pl: TStringList; var n:TokenType):Boolean;
    function MatchStrNumFunc(Match:string; var resstr:string; var resnum:double;var n:TokenType):Boolean;
    function MatchStrDualNumFunc(Match:string; var resstr:string; var resnum1,resnum2:double;var n:TokenType):Boolean;
    function MatchToken(Match:string):boolean;
    function DoPI(var r: double):boolean;
    function DoE(var r: double):boolean;
    function DoTrue(var r: double):boolean;
    function DoFalse(var r: double):boolean;
    function DoCol(var r: double):boolean;
    function DoRow(var r: double):boolean;
    function DoNow(var r: double):Boolean;
    function DoToday(var r: double):Boolean;
    function DoChoose(var Res: double; var n:TokenType):Boolean;
    function DoLt(var Res: double; var n:TokenType):Boolean;
    function DoSt(var Res: double; var n:TokenType):Boolean;
    function DoEq(var Res: double; var n:TokenType):Boolean;
    function DoFrac(var Res: double; var n:TokenType):Boolean;
    function DoFact(var Res: double; var n:TokenType):Boolean;
    function DoInt(var Res: double; var n:TokenType):Boolean;
    function DoIndex(var res:double;var resstr:string; var n:TokenType):Boolean;
    function DoMatch(var Res: double; var n:TokenType):Boolean;
    function DoLookup(var ResStr: string; var n:TokenType):Boolean;    
    function DoChs(var Res: double; var n:TokenType):Boolean;
    function DoSin(var Res: double; var n:TokenType):Boolean;
    function DoCos(var Res: double; var n:TokenType):Boolean;
    function DoTan(var res: double; var n:TokenType):Boolean;
    function DoCoTan(var res: double; var n:TokenType):Boolean;
    function DoSinh(var Res: double; var n:TokenType):Boolean;
    function DoCosh(var Res: double; var n:TokenType):Boolean;
    function DoTanh(var res: double; var n:TokenType):Boolean;
    function DoCoTanh(var res: double; var n:TokenType):Boolean;
    function DoMonth(var res: double; var n:TokenType):Boolean;
    function DoYear(var res: double; var n:TokenType):Boolean;
    function DoDay(var res: double; var n:TokenType):Boolean;
    function DoDayOfWeek(var res: double; var n:TokenType):Boolean;
    function DoMinute(var res: double; var n:TokenType):Boolean;
    function DoSecond(var res: double; var n:TokenType):Boolean;
    function DoHour(var res: double; var n:TokenType):Boolean;
    function DoExp(var Res: double; var n:TokenType):Boolean;
    function DoLn(var res: double; var n:TokenType):Boolean;
    function DoLog10(var res: double; var n:TokenType):Boolean;
    function DoLog2(var res: double; var n:TokenType):Boolean;
    function DoAbs(var res: double; var n:TokenType):Boolean;
    function DoArcTan(var res: double; var n:TokenType):Boolean;
    function DoArcCoTan(var res: double; var n:TokenType):Boolean;
    function DoRound(var res: double; var n:TokenType):Boolean;
    function DoCeiling(var res: double; var n:TokenType):Boolean;
    function DoTrunc(var res: double; var n:TokenType):Boolean;
    function DoDegrees(var res: double; var n:TokenType):Boolean;
    function DoRadians(var res: double; var n:TokenType):Boolean;
    function DoRand(var res: double; var n:TokenType):Boolean;
    function DoSqr(var res: double; var n:TokenType):Boolean;
    function DoCube(var res: double; var n:TokenType):Boolean;
    function DoSqrt(var res: double; var n:TokenType):Boolean;
    function DoArcSin(var res: double; var n:TokenType):Boolean;
    function DoArcCos(var res: double; var n:TokenType):Boolean;
    function DoOR(var res: double; var n:TokenType):Boolean;
    function DoAND(var res: double; var n:TokenType):Boolean;
    function DoNOR(var res: double; var n:TokenType):Boolean;
    function DoXOR(var res: double; var n:TokenType):Boolean;
    function DoNAND(var res: double; var n:TokenType):Boolean;
    function DoNOT(var res: double; var n:TokenType):Boolean;
    function DoSum(var res: double; var n:TokenType):Boolean;
    function DoProduct(var res: double; var n:TokenType):Boolean;
    function DoAverage(var res: double; var n:TokenType):Boolean;
    function DoCount(var res: double; var n:TokenType):Boolean;
    function DoCountA(var res: double; var n:TokenType):Boolean;
    function DoCountBlank(var res: double; var n:TokenType):Boolean;
    function DoCountIF(var res: double; var n:TokenType):Boolean;    
    function DoMin(var res: double; var n:TokenType):Boolean;
    function DoMax(var res: double; var n:TokenType):Boolean;
    function DoStDev(var res: double; var n:TokenType):Boolean;
    function DoDevSQ(var res: double; var n:TokenType):Boolean;
    function DoStDevP(var res: double; var n:TokenType):Boolean;
    function DoPower(var res: double; var n:TokenType):Boolean;
    function DoVar(var res: double; var n:TokenType):Boolean;
    function DoUpper(var res:string; var n:TokenType):Boolean;
    function DoLower(var res:string; var n:TokenType):Boolean;
    function DoTrim(var res:string; var n:TokenType):Boolean;
    function DoLen(var res:double;var resstr:string; var n:TokenType):Boolean;
    function DoSearch(var res:double;var resstr:string; var n:TokenType):Boolean;
    function DoConcatenate(var res:string; var n: TokenType):Boolean;
    function DoSubstitute(var res:string; var n: TokenType):Boolean;
    function DoLeft(var res:string; var n: TokenType):Boolean;
    function DoRight(var res:string; var n: TokenType):Boolean;
    function DoMid(var res:string; var n: TokenType):Boolean;

    procedure DoFunc(var res: double; var ResStr: string; var n:TokenType);
    procedure Primitive(var Res: double;var ResStr: string; var n:TokenType);
    function Sign(Number: double): double;
    procedure Level6(var res: double;var resstr:string; var n:TokenType);
    procedure Level5(var res: double;var resstr:string; var n:TokenType);
    procedure Level4(var res: double;var resstr:string; var n:TokenType);
    procedure Level3(var res: double;var resstr:string; var n:TokenType);
    procedure Level2(var res: double;var resstr:string; var n:TokenType);
    procedure Level1(var res: double;var resstr:string; var n:TokenType);

    function GetExpr(var Valid: Boolean): variant;
    procedure DoErr(var n: TokenType;ErrPos,ErrType: Integer; ErrStr:string);
    function GetNextVar(var Full,Expr,c,r:string): Integer;
    function IsCellVar: Boolean;
    function GetCellRange(Sheet,Range:string; Params: TParamList; SkipEmptyCells: boolean = false): Boolean;
    function GetCellRangeCoord(Range: string;var gr: TGridRect): Boolean;
    function GetCellStrRange(Sheet,Range:string; Params: TStringList): Boolean;
    function GetCellVar: variant;
    function GetCellVal(Sheet:string; ACol, ARow: Integer): variant;
    function GetCellValStr(Sheet:string; ACol, ARow: Integer): variant;    
    function IsCellNameRef(Sheet, Match:string): Integer;
    function SolveCellNameRef(Sheet, Match:string;var Res: double): Boolean;
    function GetCoord(rng:ansistring): TPoint;
    function GetExpression: string;
    procedure SetExpression(const Value: string);
  public
    constructor Create(Grid: TAdvStringGrid);
    destructor Destroy; override;
    property Expression:string read GetExpression write SetExpression;
    property Row: Integer read FRow write FRow;
    property Col: Integer read FCol write FCol;
    property ErrorPosition: Integer read FErrorPosition write FErrorPosition;
    function Calc(var r: variant): Boolean;
    property CellNameList: TStringList read FCellNameList;
    property UseRCNames: Boolean read FUseRCNames write FUseRCNames;

    property Binder: TGridBinder read FBinder write FBinder;
    property Libs: TLibBinder read FMathLib write FMathLib;
    property IsCustomFunction: TIsCustomFunction read FIsCustomFunction write FIsCustomFunction;
    property CalcCustomFunction: TCalcCustomFunction read FCalcCustomFunction write FCalcCustomFunction;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation

const
  OrdA = ord('A');

{$I DELPHIXE.INC}

type
  TAdvStringGridEx = class(TAdvStringGrid)

  end;

function StripThousandSep(s:string):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if s[i] = DecimalSeparator then
      Result := Result + '.' else
      if s[i] <> ThousandSeparator then
        Result := Result + s[i];
  end;
end;


(******************************************************************************
*                                    DoErr                                    *
******************************************************************************)
procedure TGridFormula.DoErr(var n:TokenType;ErrPos,ErrType: Integer; ErrStr:string);
var
  k: Integer;

begin
  if not FFirstError then Exit;

  n := Error;
  FErrorPosition := ErrPos;
  FFirstError := False;

  if ErrType in [Error_InvalidQualifier] then
  begin
    ErrStr := ReadWord;
  end;

  k := 1;

  while k <= FErrorPosition do
  begin
    if FOrigExpression[k] = ' ' then
      Inc(FErrorPosition);
    Inc(k);
  end;

  if Assigned(FOnError) then
    FOnError(Self,FCol,FRow,ErrType,FErrorPosition,0,ErrStr);
end;

(******************************************************************************
*                                  ReadWord                                   *
******************************************************************************)
function TGridFormula.ReadWord:string;
var
  WordIn: string;
  FTokenEndTemp: set of AnsiChar;

begin
  WordIn := '';

  FTokenEndTemp := FTokenEnd;


  while (i <= Length(FExpression)) and (not(AnsiChar(FExpression[i]) in FTokenEndTemp)) do
  begin
    WordIn := WordIn + string(UpCase(AnsiChar(FExpression[i])));
    Inc(i);

    if (WordIn = '"') then
    begin
      FTokenEndTemp := ['"'];
    end;

  end;
  ReadWord := WordIn;
end;

(******************************************************************************
*                                  ReadWordEx                                 *
******************************************************************************)
function TGridFormula.ReadWordEx:string;
var
  WordIn: string;
  GotExp: Boolean;
  GotNum: Boolean;
begin
  GotExp := False; 
  GotNum := False;
  WordIn := '';
  while (i <= Length(FExpression)) and (not(AnsiChar(FExpression[i]) in FTokenEnd)) do
  begin
    if AnsiChar(FExpression[i]) in ['0'..'9'] then
    if GotExp then
    begin
      FTokenEnd := FTokenEnd + ['-','+'];
    end
    else
      GotNum := true;

    if (WordIn <> '') and GotNum then
    begin
      if (UpCase(AnsiChar(FExpression[i])) = 'E') then
      begin
        GotExp := true;
        FTokenEnd := FTokenEnd - ['-','+']; // ignore - sign in exponent
      end;
    end;

    WordIn := WordIn + string(UpCase(AnsiChar(FExpression[i])));
    Inc(i);
  end;
  ReadWordEx := WordIn;
end;


(******************************************************************************
*                                  ReadStr                                    *
******************************************************************************)
function TGridFormula.ReadStr:string;
var
  WordIn: string;
  Tokens: set of ansichar;
  m: Integer;
begin
  WordIn := '';
  Tokens := FTokenEnd;

  m := i;

  while (i <= Length(FExpression)) and (not(AnsiChar(FExpression[i]) in Tokens)) do
  begin
    if FExpression[i] <> '"' then
      WordIn := WordIn + FExpression[i]
    else
      if m = i then
        Tokens := ['"'];
    Inc(i);
  end;

  if (i <= Length(FExpression)) then
    if FExpression[i] = '"' then Inc(i);

  ReadStr := WordIn;
end;


(******************************************************************************
*                                 ReadNumber                                  *
******************************************************************************)
function TGridFormula.ReadNumber: double;
var
  Number: Double;
  Code: Integer;
  StrNum: string;
begin
  StrNum := ReadWordEx;
  if Length(StrNum) > 0 then
    if StrNum[1] = DecimalSeparator then
      StrNum := '0' + StrNum;

  StrNum := StripThousandSep(StrNum);

  Val(StrNum,Number,Code);
  if Code <> 0 then
    Number := 0;
  Result := Number;
end; 

function TGridFormula.IsCellNameRef(Sheet, Match: string): Integer;
var
  AGrid: TAdvStringGridEx;
begin
  AGrid := TAdvStringGridEx(FStringGrid);

  if (Sheet <> '') and Assigned(Binder) then
     AGrid := TAdvStringGridEx(Binder.Grids.GridByName[Sheet]);

  Result := AGrid.CellNameList.IndexOf(Match);
end;

function TGridFormula.SolveCellNameRef(Sheet, Match:string;var Res: Double): Boolean;
var
  l: Integer;
  c,r: Integer;
  v: variant;
  AGrid: TAdvStringGridEx;
begin
  AGrid := TAdvStringGridEx(FStringGrid);

  if (Sheet <> '') and Assigned(Binder) then
     AGrid := TAdvStringGridEx(Binder.Grids.GridByName[Sheet]);

  l := AGrid.CellNameList.IndexOf(Match);

  Result := False;
  res := 0;

  if l >= 0 then
  begin
    c := Integer(AGrid.CellNameList.Objects[l]);
    r := (c and $FFFF0000) shr 16;
    c := c and $FFFF;

    v := GetCellVal(sheet,c,r);

    if Vartype(v) = varDouble then
      Res := v;

    Result := True;
  end;
end;

function TGridFormula.GetCoord(rng:ansistring): TPoint;
var
  l,Err,vp: Integer;
  c,r: ansistring;
  cs,rs: string;
  rngs: string;
begin
  c := '';
  r := '';
  Result := Point(0,0);

  rngs := string(rng);

  if FUseRCNames then
  begin
    Delete(rngs,1,1); // del leading R
    VarCharPosNC('C',rngs,vp);
    rs := Copy(rngs,1,vp - 1);
    cs := Copy(rngs,vp + 1,length(rngs));
    Val(rs,result.y,Err);
    Result.y := Result.y - 1 + FStringGrid.FixedRows;
    Val(cs,result.x,err);
    Result.x := Result.x - 1 + FStringGrid.FixedCols;
  end
  else
  begin
    for l := 1 to Length(rng) do
      if (rng[l] in ['0'..'9']) then
        r := r + rng[l]
      else
        if rng[l] <> '$' then
        c := c + rng[l];

    Val(string(r),Result.y,Err);

    case Length(c) of
    1:Result.x := Ord(Upcase(c[1])) - OrdA;
    2:Result.x := (Ord(Upcase(c[2])) - OrdA) + (26 * (Ord(Upcase(c[1])) - OrdA + 1));
    end;
    Result.y := Result.y - 1 + FStringGrid.FixedRows;
    Result.x := Result.x + FStringGrid.FixedCols;
  end;
end;


function TGridFormula.GetCellVal(Sheet: string; ACol,ARow: Integer): variant;
var
  cg: TCellGraphic;
  err,pq,ps: Integer;
  s: string;
  AGrid : TAdvStringGrid;
  n: TokenType;
  res: Double;

begin
  AGrid := nil;
  Result := 0.0;

  if Sheet = '' then
    AGrid := FStringGrid
  else
  begin
    if Assigned(Binder) then
      AGrid := TAdvStringGrid(Binder.Grids.GridByName[Sheet]);

    if not Assigned(AGrid) then
    begin
      n := Error;
      DoErr(n,i,Error_InvalidGridRef,Sheet);
      Exit;
    end;
  end;

  ARow := FStringGrid.DisplRowIndex(ARow);

  ARow := ARow - (FStringGrid.FixedRows - AGrid.FixedRows);
  ACol := ACol - (FStringGrid.FixedCols - AGrid.FixedCols);


  if AGrid is TAdvStringGrid then
  begin
    if (ACol >= AGrid.ColCount) or (ARow >= AGrid.RowCount) then
    begin
      n := Error;
      DoErr(n,i,Error_InvalidCellRef,'');
      Exit;
    end;

    cg := TCellGraphic(AGrid.GraphicObjects[ACol,ARow]);

    if Assigned(cg) then
    begin
      if cg.CellBoolean and not (cg.CellType = ctRadio) and not (cg.CellType = ctCheckBox) then
      begin
        Result := cg.CellVar;
        Exit;
      end;
    end;
  end;

  s := AGrid.Cells[ACol,ARow];

  if (Length(s) > 3) then
  begin
    if (s[1] = '|') and (s[2] = '\') then
    begin
      s := AGrid.WideCells[ACol,ARow];
      if (Pos('=',s) = 0) then
      begin
        val(s,res,err);
        if (err <> 0) then // a string value is found
        begin
          if (res = 0) then    // avoid unnecessary compiler hint
            s := AGrid.Cells[ACol,ARow]; // restore unicode string
          if (res <> 0) then
            s := AGrid.Cells[ACol,ARow]; // restore unicode string          
        end;  
      end;
    end;
  end;

  s := StripThousandSep(s);

  {$IFDEF DELPHI3_LVL}
  if Pos('=',s) = 0 then
  begin
    ps := NumCharInStr('/',s);
    pq := Pos(':',s);

    if (pq > 0) and (ps = 2) then
    begin
      try
        Result := Double(StrtoDateTime(s));
      except
        //Result := AGrid.Floats[ACol,ARow]
      end
    end
    else
      if (ps = 2) and (pq = 0) then
      begin
        try
          Result := Double(StrToDate(s));
        except
          Result := s;
          //Result := AGrid.Floats[ACol,ARow];
        end
      end
      else
        if (ps = 0) and (pq > 0) then
        begin
          try
            Result := Double(StrToTime(s));
          except
            Result := s;
        end
      end
      else
      begin
        if s = '' then
          s := '0';
        Val(s,res,Err);
        if Err = 0 then
          Result := res
        else
          Result := s;
      end;
  end
  else
  {$ENDIF}
  begin
     if s = '' then
       s := '0';

     Val(s,res,err);
     if err = 0 then
       Result := res
     else
       Result := s;
  end;

end;


function TGridFormula.GetCellValStr(Sheet: string; ACol,ARow: Integer): variant;
var
  cg: TCellGraphic;
  err,pq,ps: Integer;
  s: string;
  AGrid : TAdvStringGrid;
  n: TokenType;
  res: Double;

begin
  AGrid := nil;
  Result := 0.0;

  if Sheet = '' then
    AGrid := FStringGrid
  else
  begin
    if Assigned(Binder) then
      AGrid := TAdvStringGrid(Binder.Grids.GridByName[Sheet]);

    if not Assigned(AGrid) then
    begin
      n := Error;
      DoErr(n,i,Error_InvalidGridRef,Sheet);
      Exit;
    end;
  end;

  ARow:= ARow - (FStringGrid.FixedRows - AGrid.FixedRows);
  ACol:= ACol - (FStringGrid.FixedCols - AGrid.FixedCols);

  if AGrid is TAdvStringGrid then
  begin
    if (ACol >= AGrid.ColCount) or (ARow >= AGrid.RowCount) then
    begin
      n := Error;
      DoErr(n,i,Error_InvalidCellRef,'');
      Exit;
    end;

    cg := TCellGraphic(AGrid.GraphicObjects[ACol,ARow]);

    if Assigned(cg) then
    if cg.CellBoolean then
    begin
      Result := cg.CellVar;
      Exit;
    end;
  end;

  s := AGrid.Cells[ACol,ARow];
  s := StripThousandSep(s);

  {$IFDEF DELPHI3_LVL}
  if Pos('=',s) = 0 then
  begin
    ps := NumCharInStr('/',s);
    pq := Pos(':',s);

    if (pq > 0) and (ps = 2) then
    begin
      try
        Result := Double(StrtoDateTime(s));
      except
        //Result := AGrid.Floats[ACol,ARow]
      end
    end
    else
      if (ps = 2) and (pq = 0) then
      begin
        try
          Result := Double(StrToDate(s));
        except
          Result := s;
          //Result := AGrid.Floats[ACol,ARow];
        end
      end
      else
        if (ps = 0) and (pq > 0) then
        begin
          try
            Result := Double(StrToTime(s));
          except
            Result := s;
            //Result := AGrid.Floats[ACol,ARow];
        end
      end
      else
      begin
        Val(s,res,Err);
        if Err = 0 then
          Result := res
        else
          Result := s;
      end;
  end
  else
  {$ENDIF}
  begin
     Val(s,res,err);
     if err = 0 then
       Result := res
     else
       Result := s;
  end;
end;


function TGridFormula.GetNextVar(var Full,Expr,c,r:string): Integer;
var
  j: SmallInt;
  allow: set of AnsiChar;
  gotr,gotc,gotb: Boolean;
  vp: Integer;
  VarName,Sheet: string;
  ac: AnsiChar;
begin
  j := i;
  Allow := ['a'..'z','A'..'Z','0'..'9','(',')',':','[',']','!','$'];
  full := '';
  expr := '';
  c := '';
  r := '';
  gotr := False;
  gotc := False;
  gotb := False;

  Full := ReadWord;

  i := j;

  VarName := Full;

  Result := Length(Full);

  Sheet := '';

  if VarCharPos('!',VarName,vp) > 0 then
  begin
    Sheet := Copy(VarName, 1, vp);
    Delete(VarName,1,vp);
  end;

  j := 1;

  while (j <= Length(VarName)) and (AnsiChar(VarName[j]) in Allow) do
  begin
    ac := AnsiChar(VarName[j]);
    if (ac in ['a'..'z','A'..'Z']) and not gotb then
       Expr := Expr + VarName[j];

    if FUseRCNames then
    begin
      if (ac in ['r','R']) and (j = 1) then gotr := true;
      if (ac in ['c','C']) then gotc := true;
      if (ac in ['0'..'9']) and gotr and not gotc then
        r := r + VarName[j];
      if (ac in ['0'..'9']) and gotr and gotc then
        c := c + VarName[j];
    end
    else
    begin
      if (ac in ['0'..'9']) then
      begin
        if (Expr <> '') and (Length(Expr) <= 2) then
          c := Expr;
        Expr := '';
        r := r + VarName[j];
      end;
    end;
    Inc(j);
  end;

  if r = '' then
    c := '';
end;

function TGridFormula.GetCellRangeCoord(Range: string;var gr: TGridRect): Boolean;
var
  vp: Integer;
  c1,c2: string;
  rc1,rc2: TPoint;
begin
  Result := False;
  if VarCharPos(':',Range,vp) = 0 then
    Exit;

  Result := True;

  c1 := Copy(Range,1,vp - 1);
  c2 := Copy(Range,vp + 1,255);

  rc1 := GetCoord(AnsiString(c1));
  rc2 := GetCoord(AnsiString(c2));

  gr.Left := rc1.X;
  gr.Top := rc1.Y;
  gr.Right := rc2.X;
  gr.Bottom := rc2.Y;
end;

function TGridFormula.GetCellRange(Sheet,Range:string; Params: TParamList; SkipEmptyCells: boolean = false): Boolean;
var
  vp,m,n: Integer;
  c1,c2,cv: string;
  rc1,rc2: TPoint;
  d: variant;
  dbl: double;
  err: Integer;
begin
  Result := False;
  if VarCharPos(':',Range,vp) = 0 then
    Exit;

  c1 := Copy(Range,1,vp - 1);
  c2 := Copy(Range,vp + 1,255);

  rc1 := GetCoord(AnsiString(c1));
  rc2 := GetCoord(AnsiString(c2));

  for m := rc1.x to rc2.x do
    for n := rc1.y to rc2.y do
    begin
      cv := GetCellValStr(Sheet,m,n);
      if not SkipEmptyCells or (cv <> '') then
      begin
        d := StripThousandSep(GetCellVal(Sheet,m,n));
        Val(d,dbl,err);
        Params.Add(dbl);
      end;
   end;
end;

function TGridFormula.GetCellStrRange(Sheet,Range:string; Params: TStringList): Boolean;
var
  vp,vp2,m,n: Integer;
  c1,c2: string;
  rc1,rc2: TPoint;

begin
  Result := False;

  if VarCharPos(':',Range,vp) = 0 then
    Exit;

  c1 := Copy(Range,1,vp - 1);
//  c2 := Copy(Range,vp + 1,255);

  vp2 := vp + 1;

  while (vp2 < length(Range)) and (Ansichar(Range[vp2]) in ['$','A'..'Z','a'..'z','0'..'9']) do
    inc(vp2);

  c2 := Copy(Range,vp + 1, vp2 - vp + 1);


  rc1 := GetCoord(AnsiString(c1));
  rc2 := GetCoord(AnsiString(c2));

  for m := rc1.x to rc2.x do
    for n := rc1.y to rc2.y do
    begin
      Params.Add(GetCellValStr(Sheet,m,n));
    end;  
end;


function TGridFormula.IsCellVar: Boolean;
var
  j: Smallint;
  r,c,f,a,sheet: string;
  vl,vp: Integer;

begin
  Result := False;
  j := i;
  if not (AnsiChar(FExpression[j]) in ['a'..'z','A'..'Z','(',')','[',']','$']) then
    Exit;

  vl := GetNextVar(a,f,c,r);

  {$IFDEF TMSDEBUG}
  DbgMsg('getnextvar :'+a+' - '+f+' - '+c+' - '+r);
  {$ENDIF}

  Sheet := '';

  if VarCharPos('!',a,vp)>0 then
  begin
    Sheet := copy (a,1,vp - 1);
    Delete(a,1,vp);
  end;

  if IsCellNameRef(Sheet, f)>-1 then
  begin
    VarLen := Length(f)-1;
    Result := True;
  end
  else
  begin
    Result := (r <> '') and (c <> '');
    if Result then
      Varlen := vl
    else
      Varlen := 0;
  end;
end;


function TGridFormula.GetCellVar: variant;
var
  r,c,f,a,Sheet: string;
  ri,ci,vp,vl,Err: Integer;
  d: double;

begin
  r := '';
  c := '';
  f := '';

  vl := GetNextVar(a,f,c,r);

  Sheet := '';

  if VarCharPos('!',a,vp)>0 then
  begin
    Sheet := copy (a,1,vp - 1);
    Delete(a,1,vp);
  end;

  if IsCellNameRef(Sheet, f) > -1 then
  begin
    SolveCellNameRef(Sheet,f,d);
    Result := d;
    i := i + Length(f);
    Exit;
  end
  else
  begin
    Val(r,ri,err);
    ri := ri - 1 + FStringGrid.FixedRows;
    if FUseRCNames then
    begin
      Val(c,ci,err);
      ci := ci - 1 + FStringGrid.FixedCols;
    end
    else
    begin
      case Length(c) of
      1:ci := Ord(UpCase(c[1])) - OrdA + FStringGrid.FixedCols;
      2:begin
         ci := (Ord(UpCase(c[2])) - OrdA + FStringGrid.FixedCols)+
               (26 * (Ord(UpCase(c[1])) - OrdA + 1));
        end
      else ci := 0;
      end;
    end;

    Result := GetCellVal(Sheet,ci,ri);
    i := i + vl;
  end;
end;

(******************************************************************************
*                                  GetToken                                   *
******************************************************************************)
function TGridFormula.GetToken: TokenType;
var
  n: TokenType;
begin
  if AnsiChar(FExpression[i]) in ['+','-','/','*','=','^','%','(',')','[',']'] then
    n := Delimiter
  else
    if IsCellVar then
      n := Variable
    else
      {$IFDEF TMSDOTNET}
      if (FExpression[i] in ['0'..'9']) or (FExpression[i] = DecimalSeparator) then
      {$ENDIF}
      {$IFNDEF TMSDOTNET}
      if AnsiChar(FExpression[i]) in ['0'..'9',DecimalSeparator] then
      {$ENDIF}
        n := Digit
      else
        if FExpression[i] = '"' then
          n := Text
        else
          if FExpression[i] = ';' then
            n := EndExpr
          else
            if AnsiChar(FExpression[i]) in ['a'..'z','A'..'Z'] then
              n := Func
            else
              n := Error;
  GetToken := n;
end;

(******************************************************************************
*                                  MatchFunc                                  *
******************************************************************************)
function TGridFormula.MatchFunc(Match:string; var Res: double; var n:TokenType):Boolean;
var
  j: SmallInt;
  StrRes: string;
begin
  j := i; //restore i if no match
  if ReadWord = Match then
  begin
    MatchFunc := True;
    if FExpression[i] <> '(' then DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      n := GetToken;
      Level1(Res,StrRes,n);
      if FExpression[i] <> ')' then DoErr(n,i,Error_NoCloseParenthesis,'');
      Inc(i);
    end;
  end
  else
  begin
     MatchFunc := False;
     i := j;
  end;
end;

(******************************************************************************
*                                  MatchFuncStr                               *
******************************************************************************)
function TGridFormula.MatchFuncStr(Match:string; var Res: string; var n:TokenType):Boolean;
var
  j: SmallInt;
  d: double;
begin
  j := i; //restore i if no match
  if ReadWord = Match then
  begin
    MatchFuncStr := True;
    if FExpression[i] <> '(' then DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      n := GetToken;

      Level1(d,Res,n);

      if (Res = '') then
      begin
        if (d <> 0.0) then
          Res := Format('%g',[d])
        else
          Res := #0;
      end;

      if FExpression[i] <> ')' then
        DoErr(n,i,Error_NoCloseParenthesis,'');
      Inc(i);
    end;
  end
  else
  begin
     MatchFuncStr := False;
     i := j;
  end;
end;



(******************************************************************************
*                            MatchDualParamFunc                              *
******************************************************************************)
function TGridFormula.MatchDualParamFunc(Match:string; var Res1,Res2: double; var n:TokenType):Boolean;
var
  j,k: SmallInt;
  StrRes: string;
begin
  j := i;  // restore i if no match
  if ReadWord = Match then
  begin
    MatchDualParamFunc := True;
    //SkipBlanks;
    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      k := i;
      n := GetToken;
      Level1(Res1,StrRes,n);
      
      //SkipBlanks; {Reach closing parenthasis}
      if not (AnsiChar(FExpression[i]) in FParamEnd) then
        DoErr(n,k,Error_PrematureEndOfFormula,'');
      Inc(i);
      k := i;
      n := GetToken;
      Level1(Res2,StrRes,n);
      //SkipBlanks;
      if FExpression[i] <> ')' then
        DoErr(n,k,Error_NoCloseParenthesis,'');
      Inc(i);
    end;
  end
  else
  begin
    MatchDualParamFunc := False;
    i := j; // no Func Match, restore
  end;
end;

(******************************************************************************
*                            MatchStrNumFunc                                  *
******************************************************************************)
function TGridFormula.MatchStrNumFunc(Match:string; var resstr:string; var resnum:double;var n:TokenType):Boolean;
var
  j,k: SmallInt;
  TmpStr: string;
  TmpD: Double;
begin
  j := i;  // restore i if no match
  if ReadWord = Match then
  begin
    MatchStrNumFunc := True;
    //SkipBlanks;
    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      k := i;
      n := GetToken;
      Level1(TmpD,resstr,n);

      if (ResStr = '') then
      begin
        if (TmpD <> 0.0) then
          ResStr := Format('%g',[TmpD])
        else
          ResStr := #0;
      end;

      //SkipBlanks; {Reach closing parenthasis}

      if not (AnsiChar(FExpression[i]) in FParamEnd) then
        DoErr(n,k,Error_PrematureEndOfFormula,'');

      Inc(i);
      k := i;
      n := GetToken;

      Level1(resnum,TmpStr,n);

      //SkipBlanks;
      if FExpression[i] <> ')' then
        DoErr(n,k,Error_NoCloseParenthesis,'');
      Inc(i);
    end;
  end
  else
  begin
    MatchStrNumFunc := False;
    i := j; // no Func Match, restore
  end;
end;

(******************************************************************************
*                            MatchStrDualNumFunc                              *
******************************************************************************)
function TGridFormula.MatchStrDualNumFunc(Match:string; var resstr:string; var resnum1,resnum2:double;var n:TokenType):Boolean;
var
  j,k: SmallInt;
  TmpStr: string;
  TmpD: Double;
begin
  j := i;  // restore i if no match
  if ReadWord = Match then
  begin
    MatchStrDualNumFunc := True;
    //SkipBlanks;
    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      k := i;
      n := GetToken;
      Level1(TmpD,ResStr,n);

      if (ResStr = '') then
      begin
        if (TmpD <> 0.0) then
          ResStr := Format('%g',[TmpD])
        else
          ResStr := #0;
      end;

      //SkipBlanks; {Reach closing parenthasis}

      if not (AnsiChar(FExpression[i]) in FParamEnd) then
        DoErr(n,k,Error_PrematureEndOfFormula,'');

      Inc(i);
      k := i;
      n := GetToken;

      Level1(resnum1,TmpStr,n);

      if not (AnsiChar(FExpression[i]) in FParamEnd) then
        DoErr(n,k,Error_PrematureEndOfFormula,'');

      Inc(i);
      k := i;
      n := GetToken;

      Level1(resnum2,TmpStr,n);

      //SkipBlanks;
      if FExpression[i] <> ')' then
        DoErr(n,k,Error_NoCloseParenthesis,'');

      Inc(i);
    end;
  end
  else
  begin
    MatchStrDualNumFunc := False;
    i := j; // no Func Match, restore
  end;
end;


(******************************************************************************
*                            MatchTriParamFunc                                *
******************************************************************************)
function TGridFormula.MatchTriParamFunc(Match:string; var Res1,Res2,Res3: double; var n:TokenType): Boolean;
var
  j,k: SmallInt;
  StrRes: string;
begin
  j := i;  // restore i if no match
  if ReadWord = Match then
  begin
    MatchTriParamFunc := True;

    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      Inc(i);
      k := i;
      n := GetToken;
      Level1(Res1,StrRes,n);
      if not (AnsiChar(FExpression[i]) in FParamEnd) then DoErr(n,k,Error_PrematureEndOfFormula,'');
      Inc(i);
      k := i;
      n := GetToken;
      Level1(Res2,StrRes,n);
      if not(AnsiChar(FExpression[i]) in FParamEnd) then DoErr(n,k,Error_PrematureEndOfFormula,'');
      Inc(i);
      k := i;
      n := GetToken;
      Level1(Res3,StrRes,n);
      if FExpression[i] <> ')' then DoErr(n,k,Error_NoCloseParenthesis,'');
      Inc(I);
    end;
  end
  else
  begin
    MatchTriParamFunc := False;
    i := j;  //no Func Match, restore
  end;
end;


(******************************************************************************
*                            MatchVarParamFunc                                *
******************************************************************************)
function TGridFormula.MatchVarParamFunc(Match:string; pl: TParamList; var n:TokenType; SkipEmptyCells: boolean = false): Boolean;
var
  j,k: SmallInt;
  D: double;
  s, Sheet: string;
  vp: Integer;
  StrRes: string;
begin
  j := i; // restore i if no match
  if ReadWord = Match then
  begin
    MatchVarParamFunc := True;

    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      repeat
        if i < Length(FExpression) then
          Inc(i);

        k := i;
        s := ReadWord;

        if VarCharPos(':',s,vp) > 0 then
        begin
          if VarCharPos('!',s,vp) > 0 then
          begin
            Sheet := Copy(s,1,vp-1);
            Delete(s,1,vp);
          end;
          GetCellRange(Sheet,s,pl, SkipEmptyCells)
        end
        else
        begin
          i := k;
          n := GetToken;
          Level1(D,StrRes,n);
          pl.Add(D);
        end;
      until (AnsiChar(FExpression[i]) in [')']) or (i >= Length(FExpression));

      if pl.Count = 0 then
        DoErr(n,k,Error_InvalidNrOfParams,'');

      Inc(i);
    end;
  end
  else
  begin
    MatchVarParamFunc := False;
    i := j; //no Func Match, restore}
  end;
end;

function TGridFormula.MatchRangeDualNumFunc(Match:string; pl: TStringList; var n:TokenType):Boolean;
var
  j,k: SmallInt;
  D: double;
  s: string;
  vp: Integer;
  StrRes: string;
begin
  j := i; // restore i if no match
  if ReadWord = Match then
  begin
    MatchRangeDualNumFunc := True;

    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      repeat
        StrRes := '';
        d := 0.0;

        if i < Length(FExpression) then
          Inc(i);

        k := i;
        s := ReadWord;

        if VarCharPos(':',s,vp) > 0 then
        begin
          pl.Add(s);
        end
        else
        begin
          i := k;
          n := GetToken;
          Level1(D,StrRes,n);

          if StrRes = '' then
          begin
            if (d <> 0.0) then
              StrRes := Format('%g',[d])
            else
              StrRes := #0;
          end;

          pl.Add(StrRes);
        end;
      until (AnsiChar(FExpression[i]) in [')']) or (i >= Length(FExpression));

      if pl.Count = 0 then
        DoErr(n,k,Error_InvalidNrOfParams,'');

      Inc(i);
    end;
  end
  else
  begin
    MatchRangeDualNumFunc := False;
    i := j; //no Func Match, restore}
  end;
end;


(******************************************************************************
*                            MatchVarStrParamFunc                             *
******************************************************************************)
function TGridFormula.MatchVarStrParamFunc(Match:string; pl: TStringList; var n:TokenType): Boolean;
var
  j,k: SmallInt;
  D: double;
  s, Sheet: string;
  vp: Integer;
  StrRes: string;
begin
  j := i; // restore i if no match
  if ReadWord = Match then
  begin
    MatchVarStrParamFunc := True;

    if FExpression[i] <> '(' then
      DoErr(n,i,Error_NoOpenParenthesis,'')
    else
    begin
      repeat
        StrRes := '';
        d := 0.0;

        if i < Length(FExpression) then
          Inc(i);

        k := i;
        s := ReadWord;

        if (VarCharPos(':',s,vp)>0) and (pos('"',s) <> 1) then
        begin
          if VarCharPos('!',s,vp) > 0 then
          begin
            Sheet := Copy(s,1,vp-1);
            Delete(s,1,vp);
          end;
          GetCellStrRange(Sheet,s,pl)
        end
        else
        begin
          i := k;
          n := GetToken;
          Level1(D,StrRes,n);

          if StrRes = '' then
          begin
            if (d <> 0.0) then
              StrRes := Format('%g',[d])
            else
              StrRes := #0;
          end;

          pl.Add(StrRes);
        end;
      until (AnsiChar(FExpression[i]) in [')']) or (i >= Length(FExpression));

      if pl.Count = 0 then
        DoErr(n,k,Error_InvalidNrOfParams,'');

      Inc(i);
    end;
  end
  else
  begin
    MatchVarStrParamFunc := False;
    i := j; //no Func Match, restore}
  end;
end;


(******************************************************************************
*                                 MatchToken                                  *
******************************************************************************)
function TGridFormula.MatchToken(Match: string): Boolean;
var
  j: SmallInt;
begin
  j := i;
  if ReadWord = match then
    MatchToken:=True
  else
  begin
    MatchToken := False;
    i := j;
  end;
end;

function TGridFormula.DoPI(var r: double):boolean;
begin
  doPI := MatchToken('PI');

  // Allow also 'PI()'
  if Result and (i < Length(fExpression)) and
                (fExpression[i] = '(') and
                (fExpression[succ(i)] = ')') then
    Inc(i,2);

  r := pi;
end;

function TGridFormula.DoFalse(var r: double):boolean;
begin
  Result := MatchToken('FALSE');
  r := 0;
end;

function TGridFormula.DoTrue(var r: double):boolean;
begin
  Result := MatchToken('TRUE');
  r := 1;
end;

function TGridFormula.DoE(var r: double):boolean;
begin
  doE := MatchToken('E');
  r := Exp(1.0);
end;

(******************************************************************************
*                                     doToday                                 *
******************************************************************************)
function TGridFormula.DoToday(var r: double):boolean;
var
  da,mo,ye: word;
begin
  {$IFDEF DELPHI3_LVL}
  DoToday := MatchToken('TODAY');
  DecodeDate(now,ye,mo,da);
  r := double(EncodeDate(ye,mo,da));
  {$ELSE}
  r := 0;
  {$ENDIF}
end;


(******************************************************************************
*                                     doNow                                   *
******************************************************************************)
function TGridFormula.doNow(var r: double):boolean;
begin
  {$IFDEF DELPHI3_LVL}
  doNow := MatchToken('NOW');
  r := double(now);
  {$ELSE}
  r := 0;
  {$ENDIF}
end;

(******************************************************************************
*                                     doRow                                     *
******************************************************************************)
function TGridFormula.doRow(var r: double):boolean;
begin
  Result := MatchToken('ROW');
  r := FRow;
end;

(******************************************************************************
*                                     doCol                                   *
******************************************************************************)
function TGridFormula.doCol(var r: double):boolean;
begin
  Result := MatchToken('COL');
  r := FCol;
end;

(******************************************************************************
*                                     doChoose                                *
******************************************************************************)
function TGridFormula.DoChoose(var Res: double; var n:TokenType):Boolean;
var
 r1,r2,r3: double;
begin
  Result := MatchTriParamFunc('CHOOSE',r1,r2,r3,n);
  if r1 <> 0.0 then
    res := r3
  else
    res := r2;
end; 

(******************************************************************************
*                                     doEQ                                    *
******************************************************************************)
function TGridFormula.DoEq(var Res: double; var n:TokenType):Boolean;
var
  r1,r2: double;
begin
  Result := MatchDualParamFunc('EQ',r1,r2,n);
  if r1 = r2 then
    res := 1
  else
    res := 0;
end;

(******************************************************************************
*                                     doLt                                    *
******************************************************************************)
function TGridFormula.DoLt(var Res: double; var n:TokenType):Boolean;
var
  r1,r2: double;
begin
  Result := MatchDualParamFunc('LT',r1,r2,n);
  if r1 > r2 then
    res := 1
  else
    res := 0;
end;

(******************************************************************************
*                                     doSt                                    *
******************************************************************************)
function TGridFormula.DoSt(var Res: double; var n:TokenType):Boolean;
var
 r1,r2: double;
begin
  Result := MatchDualParamFunc('ST',r1,r2,n);
  if r1 < r2 then
    res := 1
  else
    res := 0;
end;


(******************************************************************************
*                                    DoSin                                    *
******************************************************************************)
function TGridFormula.DoSin(var Res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('SIN',Res,n);
  if Result then
    Res := Sin(Res);
end; 

(******************************************************************************
*                                    DoSinh                                    *
******************************************************************************)
function TGridFormula.DoSinh(var Res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('SINH',Res,n);
  if Result then
    Res := 0.5*(Exp(Res)-Exp(-Res));
end;

(******************************************************************************
*                                    DoCosh                                    *
******************************************************************************)
function TGridFormula.DoCosh(var Res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('COSH',Res,n);
  if Result then
    Res := 0.5*(Exp(Res) + Exp(-Res));
end;

(******************************************************************************
*                                    DoTanh                                    *
******************************************************************************)
function TGridFormula.DoTanh(var Res: double; var n:TokenType):Boolean;
var
  ep,en: double;
begin
  Result := MatchFunc('TANH',Res,n);
  if Result then
  begin
    ep := Exp(Res);
    en := Exp(-Res);
    Res := (ep - en)/(ep + en);
  end;
end;

(******************************************************************************
*                                    DoCotanh                                    *
******************************************************************************)
function TGridFormula.DoCotanh(var Res: double; var n:TokenType):Boolean;
var
  ep,en: double;
begin
  Result := MatchFunc('COTANH',Res,n);
  if Result then
  begin
    ep := Exp(Res);
    en := Exp(-Res);
    Res:=(ep + en)/(ep - en);
  end;
end;

(******************************************************************************
*                                    DoExp                                    *
******************************************************************************)
function TGridFormula.DoExp(var Res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('EXP',Res,n);
  if Result then
    Res := Exp(Res);
end;

(******************************************************************************
*                                    DoCos                                    *
******************************************************************************)
function TGridFormula.DoCos(var Res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('COS',Res,n);
  if Result then
    Res := Cos(Res);
end; 

(******************************************************************************
*                                    DoLn                                     *
******************************************************************************)
function TGridFormula.DoLn(var Res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('LN',Res,n);
  if Result then
  begin
    if Res > 0.0 then
      Res := ln(Res)
    else
      DoErr(n,k + 3,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                   DoLog10                                   *
******************************************************************************)
function TGridFormula.DoLog10(var Res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('LOG10',res,n);
  if Result then
  begin
   if Res > 0.0 then
     Res := ln(Res)/ln(10.0)
   else
     DoErr(n,k + 6,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                   DoLog2                                    *
******************************************************************************)
function TGridFormula.DoLog2(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('LOG2',res,n);
  if Result then
  begin
    if Res > 0.0 then
      Res := ln(Res)/ln(2.0)
    else
      DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                    DoAbs                                    *
******************************************************************************)
function TGridFormula.DoAbs(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('ABS',res,n);
  if Result then
    Res := Abs(Res);
end;

(******************************************************************************
*                                  DoArcTan                                   *
******************************************************************************)
function TGridFormula.DoArcTan(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('ATAN',res,n);
  if Result then
    Res := ArcTan(Res);
end; 

(******************************************************************************
*                                  DoArcCoTan                                   *
******************************************************************************)
function TGridFormula.DoArcCoTan(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('ACOTAN',Res,n);
  if Result then
  begin
    if Res <> 0.0 then
      Res := ArcTan(1/Res)
    else
      DoErr(n,K + 7,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                    DoSqr                                    *
******************************************************************************)
function TGridFormula.DoSqr(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('SQR',Res,n);
  if Result then
    Res := Sqr(Res);
end;

(******************************************************************************
*                                   DoSqrt                                    *
******************************************************************************)
function TGridFormula.DoSqrt(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('SQRT',res,n);
  if Result then
  begin
    if Res > 0.0 then
      Res := Sqrt(Res)
    else
      DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                    DoCube                                   *
******************************************************************************)
function TGridFormula.DoCube(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('CUBE',res,n);
  if Result then
    Res := Sqr(Res) * Res;
end;

(******************************************************************************
*                                    DoChs                                    *
******************************************************************************)
function TGridFormula.DoChs(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('CHS',res,n);
  Res := -Res;
end;

(******************************************************************************
*                                    DoTan                                    *
******************************************************************************)
function TGridFormula.DoTan(var res: double; var n:TokenType):Boolean;
var
  c: double;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('TAN',res,n);
  if Result then
  begin
    c := Cos(Res);
    if c <> 0 then
      Res := Sin(Res)/c
    else
      DoErr(n,k + 4,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                   DoCoTan                                   *
******************************************************************************)
function TGridFormula.DoCoTan(var res: double; var n:TokenType):Boolean;
var
  s: double;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('COTAN',res,n);
  if Result then
  begin
    s := Sin(Res);
    if s <> 0 then
      Res := Cos(Res)/s
    else
      DoErr(n,k + 6,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoArcSin                                   *
******************************************************************************)
function TGridFormula.DoArcSin(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('ASIN',res,n);
  if Result then
  begin
   if Abs(Res) <= 1.0 then
   begin
     if Res = 1.0 then
       Res := pi/2
     else
       if Res = -1.0 then
         Res := -pi/2
       else
         Res := ArcTan(Res / Sqrt(1 - Res * Res))
   end
   else
     DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoArcCos                                   *
******************************************************************************)
function TGridFormula.DoArcCos(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('ACOS',res,n);
  if Result then
  begin
    if Res <= 1.0 then
    begin
      if Res = 0 then
        Res := pi/2
      else
        Res := ArcTan(Sqrt(1 - Res * Res) / Res);
    end
    else
      DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoInt                                      *
******************************************************************************)
function TGridFormula.DoInt(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('INT',Res,n);
  if Result then
    Res := Int(Res);
end;

(******************************************************************************
*                                  DoFrac                                     *
******************************************************************************)
function TGridFormula.DoFrac(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('FRAC',Res,n);
  if Result then
    Res := Frac(Res);
end;

(******************************************************************************
*                                  DoFact                                     *
******************************************************************************)
function TGridFormula.DoFact(var res: double; var n:TokenType):Boolean;
var
  i,j: Integer;
begin
  Result := MatchFunc('FACT',Res,n);
  if Result then
  begin
    j := Round(Res);
    Res := 1;
    for i := 1 to j do
      Res := Res * i;
  end;
end;

(******************************************************************************
*                                  DoRound                                     *
******************************************************************************)
function TGridFormula.DoRound(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('ROUND',res,n);
  if Result then
    Res := Round(Res);
end;

(******************************************************************************
*                                  DoCeiling                                  *
******************************************************************************)
function TGridFormula.DoCeiling(var res: double; var n:TokenType):Boolean;
var
  r1,r2: double;
begin
  Result := MatchDualParamFunc('CEILING',r1,r2,n);
  if Result then
  begin
    if (r2 <> 0) then
    begin
      r1 := r1 / r2;
      if Frac(r1) > 0 then
        r1 := Trunc(r1 + 1)
      else
        r1 := Int(r1);

      res := r1 * r2;
    end
    else
      DoErr(n,i,Error_InvalidValue,'');
  end;  
end;


(******************************************************************************
*                                  DoTrunc                                     *
******************************************************************************)
function TGridFormula.DoTrunc(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('TRUNC',res,n);
  if Result then
    Res := Trunc(Res);
end;

(******************************************************************************
*                                  DoRand                                     *
******************************************************************************)
function TGridFormula.DoRand(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('RAND',Res,n);
  if Result then
    Res := Random(Round(Res));
end;

(******************************************************************************
*                                  DoRadians                                  *
******************************************************************************)
function TGridFormula.DoRadians(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('RADIANS',res,n);
  if Result then
  begin
    while Res > 360 do
      Res := Res - 360;
    while Res < -360 do
      Res := Res + 360;
    Res := Res / 180 * PI;
  end;
end;

(******************************************************************************
*                                  DoDegrees                                  *
******************************************************************************)
function TGridFormula.DoDegrees(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('DEGREES',res,n);
  if Result then
  begin
    while Res > 2*PI do
      Res := Res - 2 * PI;
    while Res < -2 * PI do
      Res := Res + 2 * PI;
    Res := Res / PI * 180;
  end;
end;

(******************************************************************************
*                                  DoYear                                    *
******************************************************************************)
function TGridFormula.DoYear(var res: double; var n:TokenType):Boolean;
var
  da,mo,ye: Word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('YEAR',res,n);
  if Result then
  try
    DecodeDate(TDateTime(res),ye,mo,da);
    Res := ye;
  except
    DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoMonth                                    *
******************************************************************************)
function TGridFormula.DoMonth(var res: double; var n:TokenType):Boolean;
var
  da,mo,ye: Word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('MONTH',res,n);
  if Result then
  try
    Decodedate(TDateTime(Res),ye,mo,da);
    Res := mo;
  except
    DoErr(n,k + 6,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoDay                                      *
******************************************************************************)
function TGridFormula.DoDay(var res: double; var n:TokenType):Boolean;
var
  da,mo,ye: Word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('DAY',Res,n);
  if Result then
  try
    DecodeDate(TDateTime(Res),ye,mo,da);
    Res := da;
  except
    DoErr(n,k + 4,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoDayofWeek                               *
******************************************************************************)
function TGridFormula.DoDayOfWeek(var res: double; var n:TokenType):Boolean;
var
  k: Integer;
begin
  k := i;
  Result := MatchFunc('WEEKDAY',Res,n);
  if Result then
  try
    Res := DayOfWeek(res);
  except
    DoErr(n,k + 8,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoHour                                     *
******************************************************************************)
function TGridFormula.DoHour(var res: double; var n:TokenType):Boolean;
var
  ho,mi,se,se100: Word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('HOUR',res,n);
  if Result then
  try
    DecodeTime(TDateTime(Res),ho,mi,se,se100);
    Res := ho;
  except
    DoErr(n,k + 5,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoMinute                                   *
******************************************************************************)
function TGridFormula.DoMinute(var res: double; var n:TokenType):Boolean;
var
  ho,mi,se,se100: Word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('MINUTE',res,n);
  if Result then
  try
    DecodeTime(TDateTime(Res),ho,mi,se,se100);
    Res := mi;
  except
    DoErr(n,k + 7,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                  DoSecond                                   *
******************************************************************************)
function TGridFormula.DoSecond(var res: double; var n:TokenType):Boolean;
var
  ho,mi,se,se100:word;
  k: Integer;
begin
  k := i;
  Result := MatchFunc('SECOND',res,n);
  if Result then
  try
    DecodeTime(TDateTime(Res),ho,mi,se,se100);
    Res := se;
  except
    DoErr(n,k + 7,Error_InvalidValue,'');
  end;
end;

(******************************************************************************
*                                 DoPower                                     *
******************************************************************************)
function TGridFormula.DoPower(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  k: Integer;
begin
  k := i;
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('POWER',pl,n) then
  begin
    if pl.Count <> 2 then
    begin
      DoErr(n, k, Error_InvalidNrOfParams,'');
      pl.Free;
      Exit;
    end;

    if pl.Items[0] < 0 then
    begin
      DoErr(n, k, Error_InvalidValue,'');
      pl.Free;
      Exit;
    end;

    if pl.Items[0] > 0.0 then
      res := Exp(pl.Items[1] * ln(pl.Items[0]))
    else
      res := 0.0;
    Result := True;
      
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoOR                                       *
******************************************************************************)
function TGridFormula.DoOR(var res: double; var n: TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('OR',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
      if pl.Items[i - 1] = 1 then
        res := 1;
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoXOR                                      *
******************************************************************************)
function TGridFormula.DoXOR(var res: double; var n: TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
  gotor: boolean;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('XOR',pl,n) then
  begin
    Result := True;
    res := 0;
    gotor := false;
    for i := 1 to pl.Count do
      if pl.Items[i - 1] = 1 then
      begin
        if res = 0 then
          res := 1
        else
          gotor := true;
      end;
    if gotor and (res = 1) then
      res := 0;  
  end;
  pl.Free;
end;


(******************************************************************************
*                                  DoAND                                      *
******************************************************************************)
function TGridFormula.DoAND(var res: double; var n: TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('AND',pl,n) then
  begin
    Result := True;
    res := 1;
    for i := 1 to pl.Count do
      if pl.Items[i - 1] = 0 then
        res := 0;
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoNOR                                      *
******************************************************************************)
function TGridFormula.DoNOR(var res: double; var n: TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('NOR',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
      if pl.Items[i - 1] = 1 then
        res := 1;
  end;
  if res = 1 then
    res := 0
  else
    res := 1;
  pl.Free;
end;

(******************************************************************************
*                                  DoNAND                                     *
******************************************************************************)
function TGridFormula.DoNAND(var res: double; var n: TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('NAND',pl,n) then
  begin
    Result := True;
    res := 1;
    for i := 1 to pl.Count do
      if pl.Items[i - 1] = 0 then
        res := 0;
  end;
  if res = 1 then
    res := 0
  else
    res := 1;
  pl.Free;
end;

(******************************************************************************
*                                  DoNOT                                      *
******************************************************************************)
function TGridFormula.DoNOT(var res: double; var n:TokenType):Boolean;
begin
  Result := MatchFunc('NOT',res,n);
  if Result then
    if Res = 1 then
      Res := 0
    else
      Res := 1;
end;

(******************************************************************************
*                                  DoSum                                      *
******************************************************************************)
function TGridFormula.DoSum(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('SUM',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
      res := res + pl.Items[i - 1];
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoProduct                                  *
******************************************************************************)
function TGridFormula.DoProduct(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('PRODUCT',pl,n) then
  begin
    Result := True;
    res := 1;
    for i := 1 to pl.Count do
      res := res * pl.Items[i - 1];
  end;
  pl.Free;
end;


(******************************************************************************
*                                  DoAverage                                  *
******************************************************************************)
function TGridFormula.DoAverage(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  j,k: Integer;
begin
  Result := False;
  k := i;
  pl := TParamList.Create;
  if MatchVarParamFunc('AVERAGE',pl,n) then
  begin
    Result := True;
    res := 0;
    for j := 1 to pl.Count do
      res := res + pl.Items[j - 1];
    if pl.Count > 0 then
      res := res / pl.Count
    else
      DoErr(n, k + 8, Error_InvalidNrOfParams,'');
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoCount                                    *
******************************************************************************)
{$HINTS OFF}
function TGridFormula.DoCount(var res: double; var n:TokenType):Boolean;
var
  pl: TStringList;
  s: string;
  err,i: Integer;
  v: double;

begin
  Result := False;
  pl := TStringList.Create;
  if MatchVarStrParamFunc('COUNT',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
    begin
      s := pl.Strings[i - 1];
      val(s,v,err);
      if (err = 0) then
        res := res + 1;
    end;
  end;
  pl.Free;
end;

function TGridFormula.DoCountA(var res: double; var n:TokenType):Boolean;
var
  pl: TStringList;
  s: string;
  err,i: Integer;
  v: double;

begin
  Result := False;
  pl := TStringList.Create;
  if MatchVarStrParamFunc('COUNTA',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
    begin
      s := pl.Strings[i - 1];
      if (s <> '') then
        res := res + 1;
    end;
  end;
  pl.Free;
end;

function TGridFormula.DoCountBlank(var res: double; var n:TokenType):Boolean;
var
  pl: TStringList;
  s: string;
  err,i: Integer;
  v: double;

begin
  Result := False;
  pl := TStringList.Create;
  if MatchVarStrParamFunc('COUNTBLANK',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count do
    begin
      s := pl.Strings[i - 1];
      if (s = '') then
        res := res + 1;
    end;
  end;
  pl.Free;
end;

function TGridFormula.DoCountIF(var res: double; var n:TokenType):Boolean;
var
  pl: TStringList;
  s: string;
  err,i: Integer;
  v: double;

begin
  Result := False;
  pl := TStringList.Create;
  if MatchVarStrParamFunc('COUNTIF',pl,n) then
  begin
    Result := True;
    res := 0;
    for i := 1 to pl.Count - 1 do
    begin
      s := pl.Strings[i - 1];
      if MatchStr(pl.Strings[pl.Count - 1],s,false) then
        res := res + 1;
    end;
  end;
  pl.Free;
end;


{$HINTS ON}

(******************************************************************************
*                                  DoMin                                      *
******************************************************************************)
function TGridFormula.DoMin(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;

begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('MIN', pl, n, true) then
  begin
    Result := True;
    res := 0;
    if pl.Count > 0 then
      res := pl.Items[0];

    for i := 2 to pl.Count do
      if pl.Items[i - 1] < res then
        res := pl.Items[i - 1];
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoMax                                      *
******************************************************************************)
function TGridFormula.DoMax(var Res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;

begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('MAX', pl, n, true) then
  begin
    Result := True;
    Res := 0;
    if pl.Count > 0 then
      Res := pl.Items[0];

    for i := 2 to pl.Count do
      if pl.Items[i - 1] > res then
        Res := pl.Items[i - 1];
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoStDev                                    *
******************************************************************************)
function TGridFormula.DoStDev(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  j,k: Integer;
  d1,d2: double;
begin
  Result := False;
  k := i;
  pl := TParamList.Create;
  if MatchVarParamFunc('STDEV',pl,n) then
  begin
    Result := True;

    d1 := 0;
    d2 := 0;

    for j := 1 to pl.Count do
    begin
      d1 := d1 + pl.Items[j - 1] * pl.Items[j - 1];
      d2 := d2 + pl.Items[j - 1];
    end;
    if pl.Count > 1 then
    begin
      res := sqrt ((pl.Count * d1 - d2*d2) / (pl.Count * (pl.Count - 1)));
    end
    else
      DoErr(n, k + 6, Error_InvalidNrOfParams,'');
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoStDevP                                   *
******************************************************************************)
function TGridFormula.DoStDevP(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
  d1,d2: double;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('STDEVP',pl,n) then
  begin
    Result := True;

    d1 := 0;
    d2 := 0;

    for i := 1 to pl.Count do
    begin
      d1 := d1 + pl.Items[i - 1] * pl.Items[i - 1];
      d2 := d2 + pl.Items[i - 1];
    end;
    res := sqrt ((pl.Count * d1 - d2*d2) / (pl.Count * pl.Count) );
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoDevSQ                                    *
******************************************************************************)
function TGridFormula.DoDevSQ(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  i: Integer;
  avg: double;
begin
  Result := False;
  pl := TParamList.Create;
  if MatchVarParamFunc('DEVSQ',pl,n) then
  begin
    Result := True;

    avg := 0;

    for i := 1 to pl.Count do
      avg := avg + pl.Items[i - 1];

    avg := avg / pl.Count;

    res := 0;

    for i := 1 to pl.Count do
      res := res + (avg - pl.Items[i - 1])*(avg - pl.Items[i - 1]);
  end;
  pl.Free;
end;

(******************************************************************************
*                                  DoStDevP                                   *
******************************************************************************)
function TGridFormula.DoVar(var res: double; var n:TokenType):Boolean;
var
  pl: TParamList;
  j,k: Integer;
  d1,d2: double;
begin
  Result := False;
  pl := TParamList.Create;
  k := i;
  if MatchVarParamFunc('VAR',pl,n) then
  begin
    Result := True;

    d1 := 0;
    d2 := 0;

    for j := 1 to pl.Count do
    begin
      d1 := d1 + pl.Items[j - 1] * pl.Items[j - 1];
      d2 := d2 + pl.Items[j - 1];
    end;
    if pl.Count > 0 then
      res := (pl.Count * d1 - d2*d2) / (pl.Count * (pl.Count - 1))
    else
      DoErr(n, k, Error_InvalidNrOfParams,'');
  end;
  pl.Free;
end;

(******************************************************************************
*                                   DoTrim                                    *
******************************************************************************)
function TGridFormula.DoTrim(var res:string; var n: TokenType): Boolean;
begin
  Result := MatchFuncStr('TRIM',Res,n);
  Res := Trim(res);
end;


(******************************************************************************
*                                   DoUpper                                   *
******************************************************************************)
function TGridFormula.DoUpper(var res:string; var n: TokenType): Boolean;
begin
  Result := MatchFuncStr('UPPER',Res,n);
  Res := Uppercase(Res);
end;

(******************************************************************************
*                                   DoLower                                   *
******************************************************************************)
function TGridFormula.DoLower(var res:string; var n: TokenType): Boolean;
begin
  Result := MatchFuncStr('LOWER',Res,n);
  Res := Lowercase(res);
end;

(******************************************************************************
*                                   DoLen                                     *
******************************************************************************)
function TGridFormula.DoLen(var res:double;var resstr:string; var n: TokenType): Boolean;
var
  vp: Integer;
begin
  Result := MatchFuncStr('LEN',ResStr,n);

  if Result then
  begin
    while VarCharPos(#0,ResStr,vp) > 0 do
      Delete(ResStr,vp,1);
      
    Res := Length(ResStr);
    ResStr := '';
  end;
end;

(******************************************************************************
*                                   DoSearch                                  *
******************************************************************************)
function TGridFormula.DoSearch(var res:Double;var resstr:string; var n: TokenType): Boolean;
var
  sl:TStringList;
  k: Integer;
begin
  sl := TStringList.Create;
  k := i;

  Result := MatchVarStrParamFunc('SEARCH',sl,n);

  if Result then
  begin
    if sl.Count <> 2 then
      DoErr(n,k,Error_InvalidNrOfParams,'')
    else
    begin
      Res := Pos(sl.Strings[0],sl.Strings[1]);
      ResStr := '';
    end;
  end;
  sl.Free;
end;

(******************************************************************************
*                                   DoMatch                                  *
******************************************************************************)
function TGridFormula.DoMatch(var res: double; var n: TokenType): Boolean;
var
  sl: TStringList;
  k: Integer;
  x,vp:Integer;
  Sheet,s,ws,m: string;
begin
  k := i;
  sl := TStringList.Create;
  Result := MatchRangeDualNumFunc('MATCH',sl,n);

  if Result then
  begin

    if (sl.Count <> 2) then
      DoErr(n,k,Error_InvalidNrOfParams,'')
    else
    begin
      res := -1;
      m := sl.Strings[0];
      while VarCharPos('"',m,vp) > 0 do
        delete(m,vp,1);
      s := sl.Strings[1];
      sl.Clear;
      if VarCharPos(':',s,vp)>0 then
      begin
        if VarCharPos('!',s,vp) > 0 then
        begin
          Sheet := Copy(s,1,vp-1);
          Delete(s,1,vp);
        end;
        GetCellStrRange(Sheet,s,sl);
        for x := 1 to sl.Count do
        begin
          ws := sl.Strings[x - 1];

          if pos('|\',ws) > 0 then
            ws := DecodeWideStr(ws);

          if MatchStr(m,ws,false) then
          begin
            res := x;
            Break;
          end;
        end;
      end
    end;
  end;
  sl.Free;
end;

(******************************************************************************
*                                   DoLookup                                  *
******************************************************************************)
function TGridFormula.DoLookup(var resstr: string; var n: TokenType): Boolean;
var
  sl: TStringList;
  k: Integer;
  x,vp,res:Integer;
  Sheet,s,m,lk,ws: string;
begin
  k := i;
  sl := TStringList.Create;
  Result := MatchRangeDualNumFunc('LOOKUP',sl,n);

  if Result then
  begin
    if (sl.Count <> 3) then
      DoErr(n,k,Error_InvalidNrOfParams,'')
    else
    begin
      res := -1;
      m := sl.Strings[0];
      while VarCharPos('"',m,vp) > 0 do
        delete(m,vp,1);
      s := sl.Strings[1];
      lk := sl.Strings[2];
      sl.Clear;

      if VarCharPos(':',s,vp)>0 then
      begin
        if VarCharPos('!',s,vp) > 0 then
        begin
          Sheet := Copy(s,1,vp-1);
          Delete(s,1,vp);
        end;
        GetCellStrRange(Sheet,s,sl);
        for x := 1 to sl.Count do
        begin
          ws := sl.Strings[x - 1];
          if pos('|\',ws) > 0 then
          begin
            ws := DecodeWideStr(ws);
          end;

          if MatchStr(m,ws,false) then
          begin
            res := x - 1;
            Break;
          end;
        end;

        if res > -1 then
        begin
          if VarCharPos(':',lk,vp)>0 then
          begin
            if VarCharPos('!',lk,vp) > 0 then
            begin
              Sheet := Copy(lk,1,vp-1);
              Delete(lk,1,vp);
            end;
            sl.Clear;
            GetCellStrRange(Sheet,lk,sl);
            if res < sl.Count then
              resstr := sl.Strings[res];
          end;
        end;
      end
    end;
  end;
  sl.Free;
end;


(******************************************************************************
*                                   DoIndex                                   *
******************************************************************************)
function TGridFormula.DoIndex(var res:Double;var resstr:string; var n: TokenType): Boolean;
var
  sl:TStringList;
  k: Integer;
  gr: TGridRect;
  d1,d2,err,vp:Integer;
  Sheet,s: string;
begin
  k := i;
  sl := TStringList.Create;
  Result := MatchRangeDualNumFunc('INDEX',sl,n);

  if Result then
  begin
    if (sl.Count <> 3) then
      DoErr(n,k,Error_InvalidNrOfParams,'')
    else
    begin
      s := sl.Strings[0];
      if VarCharPos('!',s,vp) > 0 then
      begin
        Sheet := Copy(s,1,vp - 1);
        Delete(s,1,vp);
      end;

      val(sl.Strings[1],d1,err);
      val(sl.Strings[2],d2,err);

      if GetCellRangeCoord(s,gr) then
      begin
        ResStr := GetCellVal(Sheet,d1 - 1 + gr.Left,d2 - 1 + gr.Top);
      end;
    end;
  end;
  sl.Free;
end;

(******************************************************************************
*                                   DoRight                                   *
******************************************************************************)
function TGridFormula.DoRight(var res:string; var n: TokenType): Boolean;
var
  d: double;
begin
  Result := MatchStrNumFunc('RIGHT',Res,d,n);
  if Result then
  begin
    res := copy(res,length(res)-trunc(d)+1,255);
  end;
end;

(******************************************************************************
*                                   DoLeft                                    *
******************************************************************************)
function TGridFormula.DoLeft(var res:string; var n: TokenType): Boolean;
var
  d: double;
begin
  Result := MatchStrNumFunc('LEFT',Res,d,n);
  if Result then
  begin
    res := copy(res,1,trunc(d));
  end;
end;

(******************************************************************************
*                                   DoMid                                     *
******************************************************************************)
function TGridFormula.DoMid(var res:string; var n: TokenType): Boolean;
var
  d1,d2: double;
begin
  Result := MatchStrDualNumFunc('MID',Res,d1,d2,n);
  if Result then
  begin
    res := copy(res,trunc(d1),trunc(d2));
  end;
end;


(******************************************************************************
*                                   DoConcatenate                             *
******************************************************************************)
function TGridFormula.DoConcatenate(var res:string; var n: TokenType): Boolean;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  Result := MatchVarStrParamFunc('CONCATENATE',sl,n);

  if Result then
  begin
    res := '';
    for i := 1 to sl.Count do
      res := res + sl.Strings[i - 1];
  end;

  sl.Free;
end;


function ReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := Pos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;


(******************************************************************************
*                                   DoConcatenate                             *
******************************************************************************)
function TGridFormula.DoSubstitute(var res:string; var n: TokenType): Boolean;
var
  sl: TStringList;
  k: Integer;
begin
  k := i;
  sl := TStringList.Create;

  Result := MatchVarStrParamFunc('SUBSTITUTE',sl,n);

  if Result then
  begin
    if sl.Count <> 3 then
      DoErr(n,k,Error_InvalidNrOfParams,'')
    else
    begin
      res := sl.Strings[0];
      repeat
      until ReplaceString(sl.Strings[1],sl.Strings[2],res) = false;
    end;  
  end;

  sl.Free;
end;



(******************************************************************************
*                                   DoFunc                                    *
******************************************************************************)
procedure TGridFormula.DoFunc(var Res: Double; var ResStr: string; var n:TokenType);
var
  Match: Boolean;
  FuncName: string;
  fn,ac: AnsiChar;
  j,k,e: Integer;
  pl: TParamList;
  sl: TStringList;
  ErrType,ErrParam: Integer;
  ErrStr: string;
  v: real;


begin
  j := i;
  k := i;
  FuncName := ReadWord;

  i := j;

  if Assigned(FMathLib) then
  begin
    for j := 1 to FMathLib.Libs.Count do
    begin
      if Assigned(FMathLib.Libs.Items[j - 1]) then

      if FMathLib.Libs.Items[j - 1].MathLib.HandlesConstant(FuncName) then
      begin
        FuncName := ReadWord;
        res := FMathLib.Libs.Items[j - 1].MathLib.GetConstant(FuncName);
        Exit;
      end;

      if FMathLib.Libs.Items[j - 1].MathLib.HandlesFunction(FuncName) then
      begin
        pl := TParamList.Create;
        if MatchVarParamFunc(FuncName,pl,n) then
        begin
          res := FMathLib.Libs.Items[j - 1].MathLib.CalcFunction(FuncName,pl,ErrType,ErrParam);
          if ErrType <> Error_NoError then
          begin
            ErrStr := '';
            if ErrType > Error_MaxErrorNumber then
              ErrStr := FMathLib.Libs.Items[j - 1].MathLib.GetErrorMessage(ErrType);

            DoErr(n, ErrType, k, ErrStr);
          end;
        end;
        pl.Free;
        Exit;
      end;

      if FMathLib.Libs.Items[j - 1].MathLib.HandlesStrFunction(FuncName) then
      begin
        sl := TStringList.Create;
        if MatchVarStrParamFunc(FuncName,sl,n) then
        begin
          resstr := FMathLib.Libs.Items[j - 1].MathLib.CalcStrFunction(FuncName,sl,ErrType,ErrParam);
          if ErrType <> Error_NoError then
          begin
            ErrStr := '';
            if ErrType > Error_MaxErrorNumber then
              ErrStr := FMathLib.Libs.Items[j - 1].MathLib.GetErrorMessage(ErrType);
            DoErr(n, ErrType, k, ErrStr);
          end;

          val(resstr,v,e);
          if (e = 0) then
          begin
            res := v;
            ResStr := '';
          end;
        end;
        sl.Free;
        Exit;
      end;

    end;
  end;

  if Assigned(FIsCustomFunction) and
     Assigned(FCalcCustomFunction) then
  begin
    Match := False;
    FIsCustomFunction(Self,FuncName,Match);
    if Match then
    begin
      if MatchFunc(FuncName,res,n) then
      begin
        FCalcCustomFunction(Self,FuncName,res);
        Exit;
      end;
    end;
  end;

//  fn := AnsiUpper(FExpression[i]);
  ac := AnsiChar(FExpression[i]);
  fn := UpCase(ac);

  case fn of
  'C':
    begin
      if not DoCountA(res,n) then
        if not DoCountBlank(res,n) then
          if not DoCountIf(res,n) then
            if not DoCount(res,n) then
              if not DoCosh(res,n) then
                if not DoCos(res,n) then
                  if not DoCoTanh(res,n) then
                    if not DoCoTan(res,n) then
                      if not DoCol(res) then
                        if not DoCube(res,n) then
                          if not DoChs(res,n) then
                            if not DoChoose(res,n) then
                              if not DoConcatenate(resstr,n) then
                                if not DoCeiling(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'L':
    begin
      if not DoLn(res,n) then
      	if not DoLog10(res,n) then
          if not DoLog2(res,n) then
            if not DoLt(res,n) then
              if not DoLower(resstr,n) then
                if not DoLen(res,resstr,n) then
                  if not DoLeft(resstr,n) then
                    if not DoLookup(resstr,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'A':
    begin
      if not DoAverage(res,n) then
        if not DoAbs(res,n) then
          if not DoArcTan(res,n) then
            if not DoArcSin(res,n) then
              if not DoArcCos(res,n) then
  	        if not DoArcCoTan(res,n) then
                  if not DoAND(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
 'D':
    begin
      if not DoDegrees(res,n) then
        if not DoDay(res,n) then
          if not DoDevSQ(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'E':
    begin
      if not DoExp(res,n) then
        if not DoE(res) then
          if not DoEQ(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'F':
    begin
      if not DoFrac(res,n) then
        if not DoFact(res,n) then
          if not DoFalse(res) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'H':
    begin
      if not DoHour(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'I':
    begin
        if not DoInt(res,n) then
          if not DoIndex(res,resstr,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'M':
    begin
      if not DoMax(res,n) then
        if not DoMin(res,n) then
          if not DoMinute(res,n) then
            if not DoMonth(res,n) then
              if not DoMid(resstr,n) then
                if not DoMatch(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'N':
    begin
      if not DoNow(res) then
        if not DoNOR(res,n) then
          if not DoNOT(res,n) then
            if not DoNAND(res,n) then DoErr(n,i,Error_InvalidQualifier,'')
    end;
  'O':
    begin
      if not DoOR(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'P':
    begin
      if not DoPI(res) then
        if not DoPower(res,n) then
          if not DoProduct(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'R':
    begin
      if not DoRound(res,n) then
        if not DoRand(res,n) then
          if not DoRadians(res,n) then
            if not DoRow(res) then
              if not DoRight(resstr,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'S':
    begin
      if not DoSum(res,n) then
        if not DoSinh(res,n) then
          if not DoSin(res,n) then
            if not DoSqr(res,n) then
              if not DoSqrt(res,n) then
                if not DoSecond(res,n) then
                  if not DoSt(res,n) then
                    if not DoStDev(res,n) then
                      if not DoStDevP(res,n) then
                        if not DoSubStitute(resstr,n) then
                          if not DoSearch(res,resstr,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'T':
    begin
      if not DoTanh(res,n) then
        if not DoTan(res,n) then
          if not DoTrunc(res,n) then
            if not DoToday(res) then
              if not DoTrue(res) then
                if not DoTrim(resstr,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'U':
    begin
      if not DoUpper(ResStr,n) then
        DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'V':
    begin
      if not DoVar(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;

  'W':
    begin
      if not DoDayOfWeek(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  'X':
    begin
      if not DoXOR(res,n) then DoErr(n,i,Error_InvalidQualifier,'');    
    end;
  'Y':
    begin
      if not DoYear(res,n) then DoErr(n,i,Error_InvalidQualifier,'');
    end;
  else
    DoErr(n,i,Error_InvalidQualifier,'');
  end;
end;

//-1 if Number < 0 , 1 otherwise
function TGridFormula.Sign(Number: double): double;
begin
  if Number < 0.0 then
    Sign := -1.0
  else
    Sign := 1.0;
end;

procedure TGridFormula.Primitive(var res: double;var ResStr:string; var n:TokenType);
var
  v: variant;
  vt: TVarType;
begin
  if n = Variable then
  begin
    v := GetCellVar;
    vt := VarType(v);
    if (vt = varString) or (vt = varUString) then
      resstr := v
    else
      res := v;
//    Res := GetCellVar
  end
  else
    if n = Text then
      ResStr := ReadStr
    else
      if n = Digit then
        Res := ReadNumber
      else
      begin
        if n = Func then
          DoFunc(res,resstr,n);
      end;
end;

//deal with parenthesis
procedure TGridFormula.Level6(var res: double;var resstr:string; var n:TokenType);
begin
  if n = Delimiter then
  begin
    if (i < Length(FExpression)) and (FExpression[i] = '(') then
    begin
      Inc(i);
      n := GetToken;
      Level1(res,resstr,n);
      if FExpression[i] <> ')' then
        DoErr(n,i,Error_NoCloseParenthesis,'');
      Inc(i);
    end
    else
      DoErr(n,i,Error_InvalidTokenAtPosition,'');
  end
  else
    Primitive(res,resstr,n);
end;

//Unary +,-
procedure TGridFormula.Level4(var res: double;var resstr:string; var n:TokenType);
var
  op: AnsiChar;
begin
  op := AnsiChar(FExpression[i]);
  if op in ['-','+'] then
    Inc(i);
  n := GetToken;
  Level5(Res,resstr,n);
  if op = '-' then
    Res := -Res;
end;

//Power
procedure TGridFormula.Level5(var res: double;var resstr:string; var n:TokenType);
var
  Hold: double;

begin
  Level6(res,resstr,n);
  if n <> Error then
    if (i < Length(FExpression)) and (FExpression[i] = '^') then
    begin
      Inc(i);
      n := GetToken;
      Level4(Hold,resstr,n);
      if Res = 0.0 then
        if Hold = 0.0 then
          Res := 1.0
        else
          Res := 0.0
      else
        try
          Res := Sign(Res) * Exp(Hold * Ln(Abs(Res)));
          if (res < 0) and (hold > 1) then
            if trunc(hold/2) = hold/2 then
              res:=-res;

        except

        end;
    end;
end;

procedure TGridFormula.Level3(var res: double;var resstr:string; var n:TokenType);
{Multipy / divide 2 factors}
var
  Hold: double;
  op: AnsiChar;
begin
  Level4(res,resstr,n);
  if n <> Error then
  begin
    while (i < Length(FExpression)) and (AnsiChar(FExpression[i]) in ['*','/','%']) do
    begin
      op := AnsiChar(FExpression[i]);
      Inc(i);
      n := GetToken;
      Level4(Hold,resstr,n);
      if op = '*' then
      begin
        try
          Res := Res * Hold
        except
          DoErr(n,i,Error_Overflow,'')
        end;
      end
      else
      begin
        if Hold = 0.0 then
          DoErr(n,i,Error_DivisionByZero,'')
        else
          if op = '/' then
            Res := Res/Hold
          else
            Res := Trunc(Res) mod Trunc(Hold);
      end;
    end;
  end;
end;

//add/sub 2 terms
procedure TGridFormula.Level2(var res: double;var resstr:string; var n:TokenType);
var
  Hold: double;
  op: AnsiChar;
begin
  Level3(Res,resstr,n);
  if n <> Error then
  begin
    while (i < Length(FExpression)) and (AnsiChar(FExpression[i]) in ['+','-']) do
    begin
      op := AnsiChar(FExpression[i]);
      Inc(i);
      n := GetToken;
      Level3(Hold,resstr,n);
      if op = '+' then
        Res := Res + Hold
      else
        Res := Res - Hold;
    end;
  end;
end;

procedure TGridFormula.Level1(var res: double;var resstr:string; var n:TokenType);
begin
  Level2(Res,resstr,n);
end;


function TGridFormula.GetExpr(var Valid:Boolean): Variant;
var
  Res: double;
  n: TokenType;
  ResStr: string;
  p: Integer;
begin
  if DecimalSeparator <> ',' then
    FTokenEnd := [' ','\',';','*','/','^','+','=','-','%','(',')',',']
  else
    FTokenEnd := [' ','\',';','*','/','^','+','=','-','%','(',')'];

  if DecimalSeparator <> ',' then
    FParamEnd := [';',',']
  else
    FParamEnd := [';'];

  FFirstError := True;
  i := 1;
  Res := 0;
  n := GetToken;
  if not (n in [EndExpr,Non]) then
    Level2(Res,ResStr,n);

  if (n <> endExpr) and (i < Length(FExpression)) then
    Dec(i);

  if ResStr = '' then
    GetExpr := Res
  else
  begin
    if (Length(ResStr) > 3) then
    begin
      if not((ResStr[1] = '|') and (ResStr[2] = '\')) then
      begin
        while VarCharPos(#0,ResStr,p) > 0 do
          Delete(ResStr,p,1);
        GetExpr := ResStr;
      end
      else
      begin
        GetExpr := DecodeWideStr(ResStr);
      end;
    end
    else
    begin
      while VarCharPos(#0,ResStr,p) > 0 do
        Delete(ResStr,p,1);
      GetExpr := ResStr;
    end;
  end;

  Valid := not (n = Error);

  if not Valid and FFirstError then
    DoErr(n,i,Error_InvalidTokenAtPosition,'');
end;

{ TGridFormula }
constructor TGridFormula.Create(Grid: TAdvStringGrid);
begin
  inherited Create;
  FStringGrid := Grid;
  FCellNameList := TStringList.Create;
end;

function TGridFormula.Calc(var r: variant): boolean;
begin
  r := GetExpr(Result);
end;

destructor TGridFormula.Destroy;
begin
  FCellNameList.Free;
  inherited;
end;


function TGridFormula.GetExpression: string;
begin
  Result := FOrigExpression;
end;

procedure TGridFormula.SetExpression(const Value: string);
var
  i: Integer;
  InQuotedString: boolean;
begin
  InQuotedString := False;
  FOrigExpression := Value;
  FExpression := '';
  for i := 1 to Length(Value) do
  begin
    if Value[i] = '"' then
    begin
      FExpression := FExpression + Value[i];
      InQuotedString := not InQuotedString;
    end
    else
    begin
      if InQuotedString then
        FExpression := FExpression + Value[i]
      else
        if Value[i] <> ' ' then
          FExpression := FExpression + Value[i];
    end; // if-else
  end; //
end;

{ TMathLib }

function TMathLib.CalcFunction(FuncName: string; Params: TParamList;
  var ErrType,ErrParam: Integer): double;
begin
  Result := 0.0;
  ErrType := 0;
end;

function TMathLib.CalcStrFunction(FuncName: string; Params: TStringList;
  var ErrType, ErrParam: Integer): string;
begin
  Result := '';
  ErrType := 0;
end;

function TMathLib.GetConstant(Constant: string): double;
begin
  Result := 0.0;
end;

function TMathLib.GetEditHint(FuncName: string;
  ParamIndex: Integer): string;
begin

end;

function TMathLib.GetErrorMessage(ErrType: Integer): string;
begin
  Result := 'No error';
end;

function TMathLib.HandlesConstant(Constant: string): Boolean;
begin
  Result := False;
end;

function TMathLib.HandlesFunction(FuncName: string): boolean;
begin
  Result := False;
end;

function TMathLib.HandlesStrFunction(FuncName: string): Boolean;
begin
  Result := False;
end;

{ TParamList }
(*
procedure TParamList.Add(Item: Double);
var
  PD: PDouble;
begin
  ReAllocMem(FList,(FCount + 1) * SizeOf(Double));
  PD := Pointer( Integer(FList) + FCount * Sizeof(Double) );
  PD^ := Item;
  Inc(FCount);
end;

procedure TParamList.Clear;
begin
  FreeMem(FList);
end;

constructor TParamList.Create;
begin
  FList := Nil;
  FCount := 0;
end;

destructor TParamList.Destroy;
begin
  Clear;
end;

function TParamList.GetCount: Integer;
begin
  Result := FCount;
end;

function TParamList.GetItem(Index: Integer): double;
var
  PD: Pdouble;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('List index out of bounds');
  PD := pointer( integer(FList) + Index * Sizeof(double) );
  Result := PD^;
end;

procedure TParamList.SetItem(Index: Integer; const Value: double);
var
  PD: Pdouble;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('List index out of bounds');
  PD := Pointer( integer(FList) + Index * Sizeof(double) );
  PD^ := Value;
end;
*)


function TParamList.GetItem(Index: Integer): double;
begin
  Result := TDoubleItem(inherited Items[Index]).Value;
end;

procedure TParamList.SetItem(Index: Integer; const Value: double);
begin
  TDoubleItem(inherited Items[Index]).Value := Value;
end;

procedure TParamList.Add(Item: Double);
var
  NewItem: TDoubleItem;
begin
  NewItem := TDoubleItem.Create;
  NewItem.Value := Item;
  {$IFDEF TMSDOTNET}
  inherited Add(NewItem);
  {$ENDIF}
  {$IFNDEF TMSDOTNET}
  inherited Add(Pointer(NewItem));
  {$ENDIF}
end;

procedure TParamList.Clear;
var
  i: Integer;
begin
  for i := 1 to Count do
    TDoubleItem(inherited Items[i - 1]).Free;

  inherited Clear;
end;


{ TGridBinderItems }

function TGridBinderItems.Add: TGridBinderItem;
begin
  Result := TGridBinderItem(inherited Add);
end;

constructor TGridBinderItems.Create(AOwner: TGridBinder);
begin
  inherited Create(TGridBinderItem);
  FOwner := AOwner;
end;

function TGridBinderItems.GetGridByName(Name: string): TBaseGrid;
var
  i: Integer;
begin
  Result := nil;

  for i := 1 to Count do
  begin
    {$IFDEF TMSDOTNET}
    if (UpperCase(Items[i - 1].Name) = UpperCase(Name)) then
    {$ENDIF}
    {$IFNDEF TMSDOTNET}
    if StrIComp(PChar(Items[i - 1].Name),PChar(Name)) = 0 then
    {$ENDIF}
    begin
      Result := Items[i - 1].Grid;
      Break;
    end;
  end;
end;

function TGridBinderItems.GetItem(Index: Integer): TGridBinderItem;
begin
  Result := TGridBinderItem(inherited Items[Index]);
end;

function TGridBinderItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGridBinderItems.SetItem(Index: Integer;
  const Value: TGridBinderItem);
begin
  inherited SetItem(Index,Value);
end;

{ TGridBinder }

constructor TGridBinder.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FGrids := TGridBinderItems.Create(Self);
end;

destructor TGridBinder.Destroy;
begin
  FGrids.Free;
  inherited;
end;

procedure TGridBinder.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    i := FGrids.Count - 1;
    while (i >= 0) do
    begin
      if FGrids.Items[i].FGrid = AComponent then
        FGrids.Items[i].Free;
      Dec(i);
    end;
  end;
  inherited;
end;

procedure TGridBinder.SetGrids(const Value: TGridBinderItems);
begin
  FGrids.Assign(Value);
end;

{ TGridBinderItem }

procedure TGridBinderItem.Assign(Source: TPersistent);
begin
  FGrid.Assign( (Source as TGridBinderItem).Grid);
  FName := (Source as TGridBinderItem).Name;
end;

constructor TGridBinderItem.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TGridBinderItem.Destroy;
begin
  inherited;
end;

{ TLibBinderItems }

function TLibBinderItems.Add: TLibBinderItem;
begin
  Result := TLibBinderItem(inherited Add);
end;

constructor TLibBinderItems.Create(AOwner: TLibBinder);
begin
  inherited Create(TLibBinderItem);
  FOwner := AOwner;
end;

function TLibBinderItems.GetItem(Index: Integer): TLibBinderItem;
begin
  Result := TLibBinderItem(inherited Items[Index]);
end;

function TLibBinderItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TLibBinderItems.SetItem(Index: Integer;
  const Value: TLibBinderItem);
begin
  inherited SetItem(Index,Value);
end;

{ TLibBinder }

constructor TLibBinder.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FLibs := TLibBinderItems.Create(Self);
end;

destructor TLibBinder.Destroy;
begin
  FLibs.Free;
  inherited;
end;

procedure TLibBinder.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin

  if AOperation = opRemove then
  begin
    i := 1;
    while i <= FLibs.Count do
    begin
      if FLibs.Items[i - 1].FMathLib = AComponent then
        FLibs.Items[i - 1].Free
      else
        Inc(i);
    end;
  end;

  inherited;
end;

procedure TLibBinder.SetLibs(const Value: TLibBinderItems);
begin
  FLibs.Assign(Value);
end;

{ TLibBinderItem }

procedure TLibBinderItem.Assign(Source: TPersistent);
begin
  FMathLib.Assign( (Source as TLibBinderItem).MathLib);
end;

constructor TLibBinderItem.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TLibBinderItem.Destroy;
begin
  inherited;
end;


end.
