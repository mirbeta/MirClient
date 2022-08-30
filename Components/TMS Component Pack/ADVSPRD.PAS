{***************************************************************************}
{ TADVSPREADGRID component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996-2015                                          }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvSprd;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Messages, SysUtils, Grids, BaseGrid, AdvGrid, AdvPars,
  AdvUtil, Forms, Graphics, AdvObj, Variants, Types
  {$IFDEF TMSDEBUG}
  , TMSUtil
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Aug, 2015'; // Month version

  // version history
  // 2.1.1.1 : Fix with CellValue[] for unicode cells
  // 2.1.1.2 : Fix for ANSI string use in formulas
  // 2.1.1.3 : Fix for AutoHeaders with insert/delete rows
  // 2.1.1.4 : Fix for AutoHeaders with LoadFromXML
  // 2.1.1.5 : Fix for smart clipboard copy with functions LOG10,LOG2
  // 2.1.1.6 : Fix for AllowSmartClipboard formula modification during clipboard actions
  // 2.1.1.7 : Fix for recalculation with deep nested cell references
  // 2.1.1.8 : Fixed issue with multisheet recalculation
  // 2.1.1.9 : Fixed issue with multisheet references to same cell
  // 2.1.1.10: Fixed issue in parser with string formulas
  // 2.1.2.0 : Improved smart clipboard recalculation speed
  // 2.1.3.0 : New : function FindCellName() added
  // 2.1.4.0 : Improved InsertRows, RemoveRows with cell names
  // 2.2.0.0 : New : support for Delphi 2009 & C++Builder 2009
  // 2.2.0.1 : Improved : unicode compatibility
  // 2.2.0.2 : Fixed : issue with hide/unhide rows
  // 2.2.0.3 : Fixed : issue with string function Search()
  // 2.2.1.0 : New : function HasError() added
  //         : Fixed : memory leak issue with AutoNumAlign = true
  // 2.2.2.0 : Improved : SmartClipboard handling
  // 2.2.2.1 : Improved : SmartClipboard handling
  // 2.2.3.0 : New : Support for RAD Studio XE added
  // 2.2.4.0 : New : OnBeforeRecalc, OnAfterRecalc events added
  // 2.2.5.0 : New : Auto cell reference renaming has support for multisheet cell references
  //         : Improved : Skip empty cells for Min/Max functions
  // 2.2.5.1 : Fixed : Issue with string literals & auto formula rearranging
  // 2.3.0.0 : New : Inter grid support for cell name references
  // 2.3.0.1 : Fixed : Issue with Selection resizer function and checkboxes
  // 2.3.1.0 : New : TDBMathLib sample library demonstrating DB access
  // 2.3.2.0 : New : grid.SaveFormula controls export of value or formula to PDF

type
  TAdvSpreadGrid = class;

  TFormulaErrorEvent = procedure(Sender: TObject;ACol,ARow: Integer; S:string) of object;

  TFormulaErrorInfoEvent = procedure(Sender: TObject;ACol,ARow: Integer; ErrType,ErrPos,ErrParam:Integer; ErrStr:string) of object;

  TCellFormatEvent = procedure (Sender: TObject;ACol,Arow: Integer;var Format:string;var DateType: Boolean) of object;

  PCellRef = ^TCellRef;
  TCellRef = record
    Src: TGridCoord;
    Dst: TGridCoord;
    SrcGrd: TAdvSpreadGrid;
    DstGrd: TAdvSpreadGrid;
    Dirty: Boolean;
  end;

  TCellRefList = class(TList)
  private
    function GetCellRef(Index: Integer): TCellRef;
    procedure ShowCellRefs;
  public
    constructor Create;
    destructor Destroy; override;
    property CellRefs[index: Integer]: TCellRef read GetCellRef; default;
    procedure Add(Value: TCellRef);
    procedure Delete(Index: Integer);
    procedure DeleteRef(AGrid:TAdvSpreadGrid;gc:TGridCoord);
    procedure DeleteAll;
    procedure ClearAll;
    procedure ClearCell(ACol,ARow: Integer);
  end;

  TCellNameMode = (nmA1,nmRC);

  TCellCalcState = (csNoCalc,csCalcOk,csCalcErr);

  TErrorDisplay = (edFormula, edErrorText);

  TAdvSpreadGrid = class(TAdvStringGrid)
  private
    FGridFormula: TGridFormula;
    FCalcBusy: Boolean;
    FShowFormula: Boolean;
    FAutoRecalc: Boolean;
    FEditHint: Boolean;
    FCellFormat: string;
    FOnCellFormat: TCellFormatEvent;
    FOnFormulaError : TFormulaErrorEvent;
    FCellRefList: TCellRefList;
    FErrorText:string;
    FCellNameMode: TCellNameMode;
    FAutoHeaders: Boolean;
    FMathLib: TLibBinder;
    FBinder: TGridBinder;
    FOnFormulaErrorInfo: TFormulaErrorInfoEvent;
    FPrintFormula: Boolean;
    FErrorDisplay: TErrorDisplay;
    FFormulaCellClick: Boolean;
    FSetCV:Boolean;
    FOnBeforeRecalc: TNotifyEvent;
    FOnAfterRecalc: TNotifyEvent;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure SetShowFormula(const Value:boolean);
    procedure SetCellFormat(const Value:string);
    procedure CalcFormula(ACol,ARow: Integer);
    procedure DelCellRef(ACol,ARow: Integer);
    procedure AddCellRef(ACol,ARow: Integer;s:string);
    procedure CalcCellRef(ACol,ARow: Integer);
    procedure ClearCellRefs;
    procedure UpdCellRef;
    function HasCellRef(ACol,ARow: integer): boolean;

    function GetIsCustomFunction:TIsCustomFunction;
    procedure SetIsCustomFunction(value:tIsCustomFunction);
    function GetCalcCustomFunction:TCalcCustomFunction;
    procedure SetCalcCustomFunction(value:tCalcCustomFunction);
    function GetCellValue(c,r: Integer): Variant;
    function GetCellName(c, r: Integer): string;
    procedure SetCellName(c, r: Integer; const Value: string);
    procedure SetCellNameMode(const Value: TCellNameMode);
    procedure SetAutoHeaders(const Value: boolean);
    function GetFixedCols: integer;
    function GetFixedRows: integer;
    procedure SetFixedCols(const Value: integer);
    procedure SetFixedRows(const Value: integer);
    function GetCellsEx(c, r: integer): string;
    procedure SetCellEx(c, r: integer; const Value: string);
    function GetColCountEx: Integer;
    function GetRowCountEx: Integer;
    procedure SetColCountEx(const Value: Integer);
    procedure SetRowCountEx(const Value: Integer);
    procedure SetCellVal(ACol,ARow: Integer; r:variant);
    procedure SetCellError(ACol,ARow,ErrPos,ErrLen: Integer);
    //procedure RemoveCellError(ACol,ARow: Integer);
    function GetCellVal(ACol,ARow: Integer): variant;
    procedure ClearCellVal(ACol,ARow: Integer);
    function GetCellState(ACol,ARow: Integer): TCellCalcState;
    function GetCalculatedValue(c, r: Integer): Variant;
  protected
    procedure DoBeforeRecalc; virtual;
    procedure DoAfterRecalc; virtual;
    function CellNameList: TStringList; override;
    function GetCellText(ACol,ARow: Integer; Formula: Boolean):string;
    function CalcCell(ACol,ARow: Integer):string; override;
    procedure GetCellAlign(ACol,ARow: Integer;var HAlign: TAlignment;var VAlign: TVAlignment); override;
    procedure UpdateCell(ACol,ARow: Integer); override;
    procedure PasteStart; override;
    procedure PasteDone; override;
    procedure PasteNotify(Orig: TPoint;gr: TGridRect;lastop: TClipOperation); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function CheckRange(c1,r1,c2,r2: Integer): Boolean;
    function ModifyRange(s, absc1, absr1, absc2, absr2:string;ptx,pty,ofsx,ofsy,rngx,rngy: Integer):string;
    function ModifyName(s, absc,absr:string;ptx,pty,ofsx,ofsy: Integer):string;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ErrorHandler(Sender: TObject; ACol,ARow: Integer; ErrType,ErrPos,ErrParam: Integer; ErrStr:string);
    procedure EditProgress(Value: string; pt: TPoint; SelPos: Integer); override;
    function GetEditHint(Func:string; ParamNr: Integer):string;
    procedure Loaded; override;
    procedure CellsLoaded; override;
    procedure RemoveRowsInternal(RowIndex, RCount: Integer); override;
    procedure CellToNameEx(Col,Row: Integer;var ColName, RowName: string);
    procedure RangeToNameEx(gr: TGridRect; var ColName1,RowName1,ColName2,RowName2: string);
    procedure CellSelect(c,r: Integer); override;
    procedure SetCellSelectMode(const Value: Boolean); override;
    function ModifyFormula(s:string;ptx,pty,ofsx,ofsy,rngx,rngy: Integer):string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; override;
    function GetVersionString:string; override;
    function ValidateCell(const NewValue: string): Boolean; override;
    procedure ClearRect(ACol1,ARow1,ACol2,ARow2: Integer); override;
    procedure ShowCellRefs;
    procedure AutoSpreadHeaders;
    procedure RecalcCell(ACol,ARow: Integer);
    procedure Recalc;

    function CellToName(Col,Row: Integer):string;
    function SaveCell(ACol,ARow: Integer):string; override;
    function ExportCell(ACol, ARow: integer): string; override;

    function FindCellName(value: string): TGridCoord;
    function NameToCell(s:string): TGridCoord;
    function RangeToName(gr:TGridRect):string;
    function NameToRange(s:string): TGridRect;
    procedure Group(Colindex: Integer); override;
    procedure UnGroup; override;

    procedure InsertRows(RowIndex, RCount: Integer; UpdateCellControls: boolean = true); override;
    procedure RemoveRows(RowIndex, RCount: Integer); override;
    procedure RemoveCols(ColIndex, CCount: Integer); override;
    procedure InsertCols(ColIndex, CCount: Integer); override;

    procedure MoveFormula(FromCell, ToCell: TGridCoord);
    function HasFormula(ACol,ARow: integer): boolean;
    function HasError(ACol,ARow: integer): boolean;

    procedure HandleError(ACol,ARow,ErrType,ErrPos,ErrParam: Integer); virtual;
    function ErrorToString(ErrType: Integer): string; virtual;

    property CellValue[c,r: Integer]: Variant read GetCellValue;
    property CellName[c,r: Integer]: string read GetCellName write SetCellName;
    property Cells[c,r: Integer]: string read GetCellsEx write SetCellEx;
    property PrintFormula: Boolean read FPrintFormula write FPrintFormula;
    property CalculatedValue[c,r: Integer]: Variant read GetCalculatedValue;
    property SaveFormula;
  published
    property Binder: TGridBinder read FBinder write FBinder;
    property EditHint: Boolean read FEditHint write FEditHint;
    property Libs: TLibBinder read FMathLib write FMathLib;
    property ShowFormula: Boolean read FShowFormula write SetShowFormula;
    property FormulaCellClick: Boolean read FFormulaCellClick write FFormulaCellClick default true;
    property AutoRecalc: Boolean read FAutoRecalc write FAutoRecalc;
    property AutoHeaders: Boolean read FAutoHeaders write SetAutoHeaders default true;
    property ErrorText:string read FErrorText write FErrorText;
    property ErrorDisplay: TErrorDisplay read FErrorDisplay write FErrorDisplay;    
    property FixedCols: Integer read GetFixedCols write SetFixedCols;
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property CellFormat: string read fCellFormat write SetCellFormat;
    property CellNameMode: TCellNameMode read fCellNameMode write SetCellNameMode default nmA1;
    property ColCount: Integer read GetColCountEx write SetColCountEx;
    property RowCount: Integer read GetRowCountEx write SetRowCountEx;
    property OnAfterRecalc: TNotifyEvent read FOnAfterRecalc write FOnAfterRecalc;
    property OnBeforeRecalc: TNotifyEvent read FOnBeforeRecalc write FOnBeforeRecalc;
    property OnCellFormat: TCellFormatEvent read FOnCellFormat write FOnCellFormat;
    property OnFormulaError: TFormulaErrorEvent read FOnFormulaError write FOnFormulaError;
    property OnFormulaErrorInfo: TFormulaErrorInfoEvent read FOnFormulaErrorInfo write FOnFormulaErrorInfo;
    property OnIsCustomFunction: TIsCustomFunction read GetIsCustomFunction write SetIsCustomFunction;
    property OnCalcCustomFunction: TCalcCustomFunction read GetCalcCustomFunction write SetCalcCustomFunction;
  end;

  function GridCoord(x,y: Integer): TGridCoord;


implementation

{$I DELPHIXE.INC}

function GridCoord(x,y: Integer): TGridCoord;
begin
  Result.X := X;
  Result.Y := Y;
end;

function IsFormula(s:string): Boolean;
begin
  Result := False;
  if (Length(s) > 0) then
  begin
    Result := (s[1] = '=');

    if Length(s) > 3 then
    begin
      if (s[1] = '|') and (s[2] = '\') and (ord(s[4]) = 61) then
      begin
        Result := true;
      end;
    end;
  end;
end;

{ TAdvSpreadGrid }

function TAdvSpreadGrid.GetCalculatedValue(c, r: Integer): Variant;
var
  s: string;
  Res: Double;
  Err: Integer;
begin
  if IsFormula(GridCells[c,r]) then
  begin
    // new inserted formula did not yet cause a recalculation
    if Assigned(GraphicObjects[c,r]) then
      Result := TCellGraphic(GraphicObjects[c,r]).CellVar
    else
      Result := 0;

    {$IFDEF TMSDEBUG}
    DbgVar('get calc val',Result);
    {$ENDIF}
  end
  else
  begin
    s := RemoveSeps(GridCells[c,r]);

    if s = '' then
      s := '0';

    Val(s,Res,Err);
    if Err <> 0 then
      raise EAdvGridError.Create('Cell does not contain a float value');
    Result := Res;
  end;
end;


function TAdvSpreadGrid.GetCellValue(c,r: Integer): Variant;
var
  s: string;
  Res: Double;
  Err: Integer;
  {$IFDEF DELPHI6_LVL}
  vt: TVarType;
  {$ENDIF}
  {$IFNDEF DELPHI6_LVL}
  vt: integer;
  {$ENDIF}
begin
  if IsFormula(GridCells[c,r]) then
  begin
    // new inserted formula did not yet cause a recalculation
    if GetCellState(c,r) = csNoCalc then
      CalcCell(c,r);

    if Assigned(GraphicObjects[c,r]) then
    begin
      Result := TCellGraphic(GraphicObjects[c,r]).CellVar;

      vt := VarType(Result);
      if (vt = varString) or (vt = VarUString) then
      begin
        s := Result;
        if (length(s) > 3) and (s[1] = '|') and (s[2] = '\') then
        begin
          Result := DecodeWideStr(s);
        end;
      end;
    end
    else
      Result := 0;

    {$IFDEF TMSDEBUG}
    DbgVar('get cell value',Result);
    {$ENDIF}
  end
  else
  begin
    s := GridCells[c,r];
    if (IsType(s) in [atFloat, atNumeric]) then
    begin
      s := RemoveSeps(s);

      if s = '' then
        s := '0';

      Val(s,Res,Err);
      if Err <> 0 then
        raise EAdvGridError.Create('Cell does not contain a float value');
      Result := Res;
    end
    else
    begin
      Result := s;
      raise EAdvGridError.Create('Cell does not contain a float value');
    end;
  end;
end;

function TAdvSpreadGrid.ModifyRange(s, absc1, absr1, absc2, absr2:string;ptx,pty,ofsx,ofsy,rngx,rngy: Integer):string;
var
  gr: TGridRect;
  col1,col2,row1,row2: string;
begin
  gr := NameToRange(s);

  if gr.left > ptx then
  begin
    if gr.left + ofsx > 0 then
      gr.left := gr.left + ofsx
    else
      gr.left := 1;
  end;

  if gr.right >= ptx then
  begin
    if gr.right + rngx > 0 then
      gr.right := gr.right + rngx
    else
      gr.right := 1;
  end;

  if gr.top >= pty then
  begin
    if gr.top + ofsy > 0 then
      gr.top := gr.top + ofsy
    else
      gr.top := 1;
  end;

  if gr.bottom >= pty then
  begin
    if gr.bottom + rngy > 0 then
      gr.bottom := gr.bottom + rngy
    else
      gr.bottom := 1;
  end;

  RangeToNameEx(gr,col1,row1,col2,row2);

  if absc1 <> '' then
    col1 := absc1;

  if absc2 <> '' then
    col2 := absc2;

  if absr1 <> '' then
    row1 := absr1;

  if absr2 <> '' then
    row2 := absr2;

  if CellNameMode = nmA1 then
    Result := col1 + row1 + ':' + col2 + row2
  else
    Result := row1 + col1 + ':' + row2 + col2;
end;

function TAdvSpreadGrid.ModifyName(s,absc,absr:string;ptx,pty,ofsx,ofsy: Integer):string;
var
  gc:TGridCoord;
  colname,rowname: string;
begin
  gc := NameToCell(s);
  if gc.x >= ptx then
  begin
    if gc.x + ofsx >= 0 then
      gc.x := gc.x + ofsx
    else
      gc.x := 0;
  end;

  if gc.y >= pty then
  begin
    if gc.y + ofsy >= 0 then
      gc.y := gc.y + ofsy
    else
      gc.y := 0;
  end;

  CellToNameEx(gc.x,gc.y,colname,rowname);

  if absc <> '' then
    colname := absc;
  if absr <> '' then
    rowname := absr;

  if CellNameMode = nmA1 then
    Result := colname + rowname
  else
    Result := rowname + colname;
end;

function TAdvSpreadGrid.ModifyFormula(s:string;ptx,pty,ofsx,ofsy,rngx,rngy: Integer):string;
var
  i,j: Integer;
  su,sf: string;
  absc1,absr1,absc2,absr2, gotc1, gotr1, gotc2: boolean;
  abscs1, absrs1, abscs2, absrs2: string;
  str_literal: boolean;
begin
  su := '';

  if Length(s) > 3 then
  begin
    if (s[1] = '|') and (s[2] = '\') and (ord(s[4]) = 61) then
      s := DecodeWideStr(s);
  end;

  {$IFDEF TMSDEBUG}
  DbgMsg('cvt',inttostr(ptx)+':'+inttostr(pty)+'->'+inttostr(ofsx)+':'+inttostr(ofsy)+' '+s);
  {$ENDIF}

  i := 1;
  str_literal := false;

  //if VarPos('(',s,v) > 0 then
  //begin
  //  su := copy(s,1,v);
  //  delete(s,1,v);
  //end;

  // skip a first sheetname
  if Pos ('!', s) <> 0 then
  begin
    i := pos ('!', s);
    su := copy (s,1,i);
    inc (i);
  end;

  while (i <= Length(s)) do
  begin
    absr1 := false;
    absr2 := false;
    absc2 := false;
    gotc1 := false;
    gotc2 := false;
    gotr1 := false;

    if (s[i] = '"') then
      str_literal := not str_literal;

    {$IFDEF DELPHI_UNICODE}
    if CharInSet(s[i], ['A'..'Z','a'..'z','$']) and not str_literal then
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    if (s[i] in ['A'..'Z','a'..'z','$']) and not str_literal then
    {$ENDIF}
    begin
      absc1 := s[i] = '$';

      if s[i] <> '$' then
        sf := s[i]
      else
        sf := '';

      abscs1 := s[i];
      absrs1 := '';

      j := i + 1;
      while (j <= Length(s)) do
      {$IFDEF DELPHI_UNICODE}
        if (CharInSet(s[j], ['A'..'Z','a'..'z','0'..'9',':','$'])) and not str_literal then
        begin
          if CharInSet(s[j],['0'..'9',':','$']) then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
        if (s[j] in ['A'..'Z','a'..'z','0'..'9',':','$']) and not str_literal then
        begin
          if s[j] in ['0'..'9',':','$'] then
      {$ENDIF}
          begin
            if gotr1 then
            begin
              if abscs2 <> '' then
                gotc2 := true
            end
            else
              gotc1 := true;
          end;

          if s[j] = ':' then  // 2nd part found
            gotr1 := true
          else
          begin
            if not gotc1 then
              abscs1 := abscs1 + s[j];

            if gotc1 and not gotr1 then
              absrs1 := absrs1 + s[j];

            if gotr1 and not gotc2 then
              abscs2 := abscs2 + s[j];

            if gotr1 and gotc2 then
              absrs2 := absrs2 + s[j];

            if not absr1 and not gotr1 then
              absr1 := s[j] = '$';

            if gotr1 and not gotc2 and not absc2 then
              absc2 := s[j] = '$';

            if gotr1 and gotc2 and not absr2 then
              absr2 := s[j] = '$';
          end;

          if s[j] <> '$' then
            sf := sf + s[j];
          inc(j);
        end
        else
          Break;

      dec(j);
      {$IFDEF DELPHI_UNICODE}
      if ((Length(sf) > 1) and (CharInSet(sf[2], ['0'..'9']))) or
         ((Length(sf) > 2) and (CharInSet(sf[3], ['0'..'9']))) then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if ((Length(sf) > 1) and (sf[2] in ['0'..'9'])) or
         ((Length(sf) > 2) and (sf[3] in ['0'..'9'])) then
      {$ENDIF}
      begin
        {$IFDEF TMSDEBUG}
        DbgMsg(sf);
        {$ENDIF}

        if not absc1 then
          abscs1 := '';
        if not absr1 then
          absrs1 := '';

        if not absc2 then
          abscs2 := '';
        if not absr2 then
          absrs2 := '';

        if (pos(':',sf)>0) then
          su := su + ModifyRange(sf,abscs1,absrs1, abscs2, absrs2, ptx,pty,ofsx,ofsy,rngx,rngy)
        else
          su := su + ModifyName(sf,abscs1,absrs1,ptx,pty,ofsx,ofsy);
        i := j;
      end
      else su := su + s[i];
    end
    else
      su := su + s[i];

    inc(i);

    if Pos('!', copy (s, i, length (s))) <> 0 then
    begin
      while (i < Length (s)) and (copy (s, i, 1) <> '!') do
      begin
        su := su + s[i];
        inc (i);
      end;
    end;

  end;

  Result := su;
end;

function TAdvSpreadGrid.CellToName(Col,Row: Integer):string;
var
  cs: string;
begin
  if FCellNameMode = nmA1 then
  begin
    if (col - FixedCols >= 26) then
      cs := chr(((Col - FixedCols) div 26) + 64)+ chr(((col-FixedCols) mod 26) + 65)
    else
      cs := chr((Col - FixedCols) + 65);

    Result := cs + IntToStr(Row - FixedRows + 1);
  end
  else
  begin
    Result := 'R' + IntToStr(Row - FixedRows + 1)+'C'+IntTostr(Col - FixedCols + 1);
  end;
end;

procedure TAdvSpreadGrid.CellToNameEx(Col,Row: Integer;var ColName, RowName:string);
var
  cs: string;
begin
  if FCellNameMode = nmA1 then
  begin
    if (col - FixedCols >= 26) then
      cs := chr(((Col - FixedCols) div 26) + 64)+ chr(((col-FixedCols) mod 26) + 65)
    else
      cs := chr((Col - FixedCols) + 65);

    ColName := cs;
    RowName := IntToStr(Row - FixedRows + 1);
  end
  else
  begin
    ColName := 'C'+IntTostr(Col - FixedCols + 1);
    RowName := 'R' + IntToStr(Row - FixedRows + 1);
  end;
end;


function TAdvSpreadGrid.NameToCell(s:string): TGridCoord;
var
  l,err,vp: Integer;
  c,r: string;
begin
  c := '';
  r := '';
  Result.x := -1;
  Result.y := -1;

  if FCellNameMode = nmA1 then
  begin
    for l := 1 to Length(s) do
    {$IFDEF DELPHI_UNICODE}
      if CharInSet(s[l], ['0'..'9']) then
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
      if (s[l] in ['0'..'9']) then
    {$ENDIF}
        r := r + s[l]
      else
        c := c + s[l];

    Val(r,Result.y,err);
    Result.y := Result.y - 1 + FixedRows;

    case Length(c) of
    1:Result.x := Ord(UpCase(c[1])) - 65 + FixedCols;
    2:begin
        Result.x := (Ord(UpCase(c[2])) - 65 + FixedCols) + (26 * (ord(UpCase(c[1])) - 64));
      end;
    end;
  end
  else
  begin
    Delete(s,1,1); {del leading R}
    VarCharPosNC('C',s,vp);
    r := Copy(s,1,vp - 1);
    c := Copy(s,vp + 1,Length(s));
    Val(r,Result.y,err);
    Result.y := Result.y - 1 + FixedRows;
    Val(c,Result.x,err);
    Result.x := Result.x - 1 + FixedCols;
  end;
end;

function TAdvSpreadGrid.NameToRange(s:string): TGridRect;
var
  vp: Integer;
begin
  if VarCharPos(':',s,vp) > 0 then
    Result.TopLeft := NameToCell(Copy(s,1,vp-1));

  if VarCharPos(':',s,vp) > 0 then
    Result.BottomRight := NameToCell(Copy(s,vp+1,255));
end;

function TAdvSpreadGrid.RangeToName(gr:TGridRect):string;
begin
  Result := CellToName(gr.left,gr.top) + ':' + CellToName(gr.right,gr.bottom);
end;

procedure TAdvSpreadGrid.RangeToNameEx(gr:TGridRect; var ColName1, RowName1, ColName2, RowName2:string);
begin
  CellToNameEx(gr.left,gr.top,ColName1,RowName1);
  CellToNameEx(gr.right,gr.bottom,ColName2,RowName2);
end;


procedure TAdvSpreadGrid.AutoSpreadHeaders;
var
  i: Integer;
begin
  if not AutoHeaders then
    Exit;

  if FixedCols > 0 then
  begin
    for i := 1 to FixedRows do
      Cells[0,i - 1] := '';
    for i := FixedRows + 1 to RowCount do
      Cells[0,i - 1] := IntToStr(i - FixedRows);
  end;

  if Fixedrows > 0 then
  for i := FixedCols to ColCount - 1 do
  begin
    if FCellNameMode = nmA1 then
    begin
      if i - Fixedcols >= 26 then
      begin
        Cells[i,0] := chr( ((i - Fixedcols) div 26) + 64)+ chr(((i - FixedCols) mod 26) + 65);
      end
      else
        Cells[i,0] := chr(i + 65 - FixedCols);
    end
    else
    begin
      Ints[i,0] := i - FixedCols + 1;
    end;
  end;
end;

// cell contents changed-> so mark for recalc
procedure TAdvSpreadGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  cs: string;
begin
  inherited SetEditText(Acol,Arow,Value);

  cs := CellToName(ACol,ARow);

  if Assigned(GraphicObjects[ACol,ARow]) then
  begin
    TCellGraphic(GraphicObjects[ACol,ARow]).CellBoolean := False;
    TCellGraphic(GraphicObjects[ACol,ARow]).CellErrFrom := 0;
    TCellGraphic(GraphicObjects[ACol,ARow]).CellErrLen := 0;
  end;
end;

procedure TAdvSpreadGrid.CalcFormula(ACol,ARow: Integer);
begin
  ClearCellVal(ACol,ARow);
  CalcCell(ACol,ARow);
  InvalidateCell(ACol,ARow);  // is causing a cell calc
  CalcCellRef(ACol,ARow);
end;

procedure TAdvSpreadGrid.ReCalc;
var
  i,j: Integer;
  err: boolean;
  cnt: integer;
label
  redocalc;

begin
  // fully rebuilds the cell reference list.

  DoBeforeRecalc;

  UpdCellRef;

  // there are no formulas, so skip recalculation
  if FCellRefList.Count = 0 then
    Exit;

  FCellRefList.ClearAll;
  err := false;
  cnt := 0;

redocalc:

  for i := 1 to ColCount do
    for j := 1 to RowCount do
    begin
      if IsFormula(Cells[i - 1,j - 1]) then
      begin
        CalcFormula(i - 1,j - 1);
         case GetCellState(i - 1,j - 1) of
        csNoCalc: CalcCell(i - 1,j - 1);
        csCalcErr: err := true;
        end;
       end;
    end;

  if err then
  begin
    err := false;
    for i := ColCount downto 1 do
      for j := RowCount downto 1 do
      begin
        if IsFormula(Cells[i - 1,j - 1]) then
        begin
          CalcFormula(i - 1,j - 1);
          case GetCellState(i - 1,j - 1) of
          csNoCalc: CalcCell(i - 1,j - 1);
          csCalcErr: err := true;
          end;
        end;
      end;

    if err and (cnt = 0) then
    begin
      inc(cnt);
      goto redocalc;
    end;
  end;

  DoAfterRecalc;

  CellsChanged(Rect(0,0,ColCount - 1,RowCount - 1));
end;


function TAdvSpreadGrid.GetCellText(ACol, ARow: Integer; Formula: Boolean): string;
var
  s: string;
  r: Double;
  v: variant;
  CellFmt: string;
  DateFmt,FloatFmt: Boolean;
  {$IFDEF DELPHI6_LVL}
  vt: TVarType;
  {$ENDIF}
  {$IFNDEF DELPHI6_LVL}
  vt: integer;
  {$ENDIF}
begin
  s := GridCells[ACol,ARow];

  Result := s;

  if Formula then
    Exit;

  if (Length(s) > 3) then
  begin
    if (s[1] = '|') and (s[2] = '\') and (ord(s[4]) = 61) then
      s := WideCells[ACol,ARow];
  end;

  if not IsFormula(s) then
    Exit;

  case GetCellState(ACol,ARow) of
  csNoCalc:
    begin
      Delete(s,1,1);
      if Length(s) = 0 then
      begin
        HandleError(ACol,ARow,Error_NoFormula,1,1);

        SetCellError(ACol,ARow,1,1);
        Exit;
      end;

      FGridFormula.Expression := s;
      FGridFormula.Col := ACol;
      FGridFormula.Row := ARow;
      FGridFormula.Libs := FMathLib;
      FGridFormula.Binder := FBinder;

      if FGridFormula.Calc(v) then
      begin
        SetCellVal(ACol,ARow,v);
        CellFmt := FCellFormat;
        DateFmt := False;

        if Assigned(FOnCellFormat) then
          FOnCellFormat(self,ACol,ARow,CellFmt,DateFmt);

        if Assigned(OnGetFloatFormat) then
          OnGetFloatFormat(self,ACol,ARow,FloatFmt,CellFmt);

        if DateFmt then
        begin
          r := v;
          Result := FormatDateTime(CellFmt,TDateTime(r))
        end
        else
        begin
          vt := VarType(v);
          if not ((vt = varString) or (vt = varOLEStr) or (vt = varUString)) then
          begin
            r := v;
            Result := Format(CellFmt,[r]);
          end
          else
          begin
            {$IFDEF DELPHI_UNICODE}
            Result := v;
            {$ENDIF}
            {$IFNDEF DELPHI_UNICODE}
            if (VarType(v) = varOleStr) then
              Result := EncodeWideStr(v)
            else
              Result := v;
            {$ENDIF}
          end;
        end;
      end;
    end;
  csCalcOk:
    begin
      CellFmt := FCellFormat;
      DateFmt := False;
      v := GetCellVal(ACol,ARow);
      vt := VarType(v);
      if not ( (vt = varString) or (vt = varOleStr) or (vt = varUString)) then
      begin
        r := v;
        if Assigned(FOnCellFormat) then
          FOnCellFormat(self,ACol,ARow,cellfmt,datefmt);

        if Assigned(OnGetFloatFormat) then
          OnGetFloatFormat(self,ACol,ARow,FloatFmt,CellFmt);

        if Datefmt then
          Result := FormatDateTime(cellfmt,TDateTime(r))
        else
          Result := Format(CellFmt,[r]);
      end
      else
      begin
        {$IFDEF DELPHI_UNICODE}
        Result := v;
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
        if (VarType(v) = varOleStr) then
          Result := EncodeWideStr(v)
        else
          Result := v;
        {$ENDIF}          
      end;
    end;
  csCalcErr:
    begin
      if ErrorDisplay = edErrorText then
        Result := ErrorText
      else
        Result := s;
    end;
  end;
end;

function TAdvSpreadGrid.ExportCell(ACol, ARow: integer): string;
begin
  if IsFormula(Cells[ACol,ARow]) then
    Result := GetCellText(ACol,ARow,SaveFormula)
  else
    Result := Cells[ACol,ARow];
end;


function TAdvSpreadGrid.SaveCell(ACol, ARow: integer): string;
begin
  if IsFormula(Cells[ACol,ARow]) then
    Result := GetCellText(ACol,ARow,SaveFormula)
  else
    Result := Cells[ACol,ARow];
end;

function TAdvSpreadGrid.CalcCell(ACol, ARow: integer): string;
begin
  Result := GetCellText(ACol,ARow,FShowFormula);
end;

constructor TAdvSpreadGrid.Create(aOwner: tComponent);
begin
  inherited Create(aOwner);
  FGridFormula := TGridFormula.Create(self);
  FGridFormula.OnError := ErrorHandler;
  FCalcBusy := False;
  FShowFormula := False;
  FAutoRecalc := True;
  FCellFormat := '%.2f';
  FCellRefList := TCellRefList.Create;
  FCellNameMode := nmA1;
  AutoHeaders := True;
  FErrorText := 'Error';
  {$IFDEF DELPHI4_LVL}
  DoubleBuffered := False;
  {$ENDIF}
  SaveFormula := True;
  FPrintFormula := False;
  {$IFDEF DELPHI6_LVL}
  ActiveCellColor := clCream;
  {$ENDIF}
  Options := Options + [goEditing, goColSizing, goRowSizing];
  FFormulaCellClick := true;
end;

destructor TAdvSpreadGrid.Destroy;
begin
  FGridFormula.Free;
  FCellRefList.Free;
  inherited Destroy;
end;

procedure TAdvSpreadGrid.SetShowFormula(const Value: boolean);
begin
  if FShowFormula <> Value then
  begin
    FShowFormula := Value;
    Invalidate;
    if FloatingFooter.Visible then
      UpdateFooter;
  end;
end;

procedure TAdvSpreadGrid.SetCellFormat(const Value: string);
begin
  if FCellFormat <> Value then
  begin
    FCellFormat := Value;
    Invalidate;
  end;
end;

procedure TAdvSpreadGrid.InsertCols(ColIndex, CCount: Integer);
var
  i,j: Integer;
begin
  FCellRefList.DeleteAll;
  inherited InsertCols(ColIndex,CCount);
  for i := FixedCols + 1 to Colcount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i-1,j-1]) then
        Cells[i - 1,j - 1] := ModifyFormula(Cells[i - 1,j - 1],ColIndex,0,CCount,0,CCount,0);
    end;
  UpdCellRef;
  if FAutoRecalc then
    Recalc;
  AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.RemoveRows(RowIndex, RCount: Integer);
var
  i,j: Integer;
  o: Integer;
  GC: TGridCoord;
begin
  if FIsGrouping then
  begin
    inherited;
    Exit;
  end;

  for i := 0 to FGridFormula.CellNameList.Count - 1 do
  begin
    o := integer(FGridFormula.CellNameList.Objects[i]);
    gc := GridCoord( (o and $FFFF), ((o shr 16) and $FFFF));

    if gc.Y >= RowIndex + RCount then
    begin
      gc.Y := gc.Y - RCount;
      FGridFormula.CellNameList.Objects[i] := TObject(makelong(GC.x,GC.y));
    end;
  end;


  FCellRefList.DeleteAll;
  inherited RemoveRows(RowIndex,RCount);

  for i := FixedCols + 1 to Colcount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i-1,j-1]) then
        Cells[i-1,j-1] := ModifyFormula(Cells[i-1,j-1],0,RowIndex,0,-RCount,0,-RCount);
    end;
  UpdCellRef;
  if FAutoRecalc then
    Recalc;
  AutoSpreadHeaders;
end;


procedure TAdvSpreadGrid.InsertRows(RowIndex, RCount: Integer; UpdateCellControls: boolean = true);
var
  i,j,o: Integer;
  GC: TGridCoord;
begin
  if FIsGrouping then
  begin
    inherited;
    Exit;
  end;

  FCellRefList.DeleteAll;

  for i := 0 to FGridFormula.CellNameList.Count - 1 do
  begin
    o := integer(FGridFormula.CellNameList.Objects[i]);
    gc := GridCoord( (o and $FFFF), ((o shr 16) and $FFFF));

    if gc.Y > RowIndex then
    begin
      gc.Y := gc.Y + RCount;
      FGridFormula.CellNameList.Objects[i] := TObject(makelong(GC.x,GC.y));
    end;
  end;


  inherited InsertRows(RowIndex,RCount);

  if FIsGrouping then
    Exit;

  for i := FixedCols + 1 to Colcount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i-1,j-1]) then
        Cells[i-1,j-1] := ModifyFormula(Cells[i-1,j-1],0,RowIndex,0,RCount,0,RCount);
    end;

  UpdCellRef;


  if FAutoRecalc then
    Recalc;

  AutoSpreadHeaders;
end;

function TAdvSpreadGrid.HasCellRef(ACol, ARow: integer): boolean;
var
  i: integer;
  p: PCellRef;

begin
  Result := false;
  i := 1;

  while (i <= FCellRefList.Count) do
  begin
    p := FCellRefList.Items[i - 1];
    if (p^.src.x = ACol) and (p^.src.y = ARow) and p^.Dirty then
    begin
      Result := true;
      Exit;
    end;
    inc(i);
  end;

end;

function TAdvSpreadGrid.HasError(ACol, ARow: integer): boolean;
begin
  Result := false;
  if Assigned(GraphicObjects[ACol,ARow]) then
  begin
    Result := TCellGraphic(GraphicObjects[ACol,ARow]).CellErrFrom <> 0;
  end;

end;

function TAdvSpreadGrid.HasFormula(ACol, ARow: integer): boolean;
begin
  Result := IsFormula(Cells[ACol,ARow]);
end;

procedure TAdvSpreadGrid.RemoveCols(ColIndex, CCount: Integer);
var
  i,j: Integer;
begin
  FCellRefList.DeleteAll;
  inherited RemoveCols(ColIndex,CCount);

  for i := FixedCols + 1 to Colcount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i-1,j-1]) then
        Cells[i-1,j-1] := ModifyFormula(cells[i-1,j-1],ColIndex,0,-CCount,0,-CCount,0);
    end;

  UpdCellRef;
  if FAutoRecalc then
    Recalc;
  AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.RemoveRowsInternal(RowIndex, RCount: Integer);
var
  i,j: Integer;

begin
  if FIsGrouping then
  begin
    inherited;
    Exit;
  end;

  FCellRefList.DeleteAll;
  inherited RemoveRows(RowIndex,RCount);
  for i := FixedCols + 1 to Colcount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i-1,j-1]) then
        Cells[i-1,j-1] := ModifyFormula(Cells[i-1,j-1],0,RowIndex,0,-RCount,0,-RCount);
    end;
  UpdCellRef;
  if FAutoRecalc then
    Recalc;
end;


function TAdvSpreadGrid.CheckRange(c1,r1,c2,r2: Integer): Boolean;
begin
  Result := False;
  if (c1 = c2) and (r1 = r2) then
  begin
    HandleError(c2,r2,Error_CircularRange,1,1);
    Exit;
  end;
  if (c1 >= ColCount) or (r1 >= RowCount) then
  begin
    HandleError(c2,r2,Error_InvalidRangeRef,1,1);
    Exit;
  end;
  Result := True;
end;


procedure TAdvSpreadGrid.AddCellRef(ACol,ARow: Integer;s: string);
var
  i,j,k,l,vp: Integer;
  su,sf,Sheet: string;
  gr: TGridRect;
  gc: TGridcoord;
  cr: TCellref;
  AGrid: TAdvSpreadGrid;

begin
  su := '';

  //do add to cellrefs too ...

  i := 1;
  while i <= Length(s) do
  begin
    {$IFDEF DELPHI_UNICODE}
    if CharInSet(s[i], ['A'..'Z','a'..'z','$']) then
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    if s[i] in ['A'..'Z','a'..'z','$'] then
    {$ENDIF}
    begin
      if s[i] <> '$' then
        sf := s[i]
      else
        sf := '';

      j := i + 1;
      while j <= Length(s) do
        {$IFDEF DELPHI_UNICODE}
        if (CharInSet(s[j], ['A'..'Z','a'..'z','0'..'9',':','!','$'])) then
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
        if (s[j] in ['A'..'Z','a'..'z','0'..'9',':','!','$']) then
        {$ENDIF}
        begin
          if s[j] <> '$' then
            sf := sf + s[j];
          inc(j);
        end
        else
          Break;

      Dec(j);
      {
      l := FGridFormula.CellNameList.IndexOf(sf);

      if l > -1 then
      begin
        cr.src.x := Integer(FGridFormula.CellNameList.Objects[l]) and $FFFF;
        cr.src.y := Integer(FGridFormula.CellNameList.Objects[l]) and $FFFF0000 shr 16;
        cr.dst.x := ACol;
        cr.dst.y := ARow;
        cr.SrcGrd := Self;
        cr.DstGrd := Self;

        if (cr.dst.x = cr.src.x) and (cr.dst.y = cr.src.y) then
          HandleError(ACol,ARow,Error_CircularReference,1,1)
        else
          FCellRefList.Add(cr);

        i := j;
      end;
      }

      AGrid := Self;

      if (VarCharPos('!',sf,vp) > 0) and Assigned(Binder) then
      begin
        Sheet := Copy(sf,1,vp - 1);
        Delete(sf,1,vp);

        AGrid := TAdvSpreadGrid(Binder.Grids.GridByName[Sheet]);

        if not Assigned(AGrid) then
          AGrid := Self;
      end;

      l := AGrid.CellNameList.IndexOf(sf);

      if l > -1 then
      begin
        cr.src.x := Integer(AGrid.CellNameList.Objects[l]) and $FFFF;
        cr.src.y := Integer(AGrid.CellNameList.Objects[l]) and $FFFF0000 shr 16;
        cr.dst.x := ACol;
        cr.dst.y := ARow;
        cr.SrcGrd := AGrid;
        cr.DstGrd := Self;

        if (cr.dst.x = cr.src.x) and (cr.dst.y = cr.src.y) then
          HandleError(ACol,ARow,Error_CircularReference,1,1)
        else
          AGrid.FCellRefList.Add(cr);

        i := j;
      end;

      {$IFDEF DELPHI_UNICODE}
      if ((Length(sf) > 1) and (CharInSet(sf[2], ['0'..'9']))) or
         ((Length(sf) > 2) and (CharInSet(sf[3], ['0'..'9']))) then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if ((Length(sf) > 1) and (sf[2] in ['0'..'9'])) or
         ((Length(sf) > 2) and (sf[3] in ['0'..'9'])) then
      {$ENDIF}
      begin
        {$IFDEF TMSDEBUG}
        DbgMsg(sf);
        {$ENDIF}

        if VarCharPos(':',sf,vp) > 0 then
        begin
          gr := NameToRange(sf);
          for k := gr.Left to gr.Right do
            for l := gr.Top to gr.Bottom do
            begin
              cr.Src.x := k;
              cr.Src.y := l;
              cr.Dst.x := ACol;
              cr.Dst.y := ARow;
              cr.SrcGrd := AGrid;
              cr.DstGrd := Self;

              if (cr.dst.x = cr.src.x) and (cr.dst.y = cr.src.y) and (cr.SrcGrd = cr.DstGrd) then
                HandleError(ACol,ARow,Error_CircularReference,1,1)
              else
                AGrid.FCellRefList.Add(cr);
            end;

        end
        else
        begin
          gc := NameToCell(sf);
          cr.Src.x := gc.x;
          cr.Src.y := gc.y;
          cr.Dst.x := ACol;
          cr.Dst.y := ARow;
          cr.SrcGrd := AGrid;
          cr.DstGrd := Self;

          if (cr.dst.x = cr.src.x) and (cr.dst.y = cr.src.y) and (cr.SrcGrd = cr.DstGrd) then
            HandleError(ACol,ARow,Error_CircularReference,1,1)
          else
            AGrid.FCellRefList.Add(cr);

          AGrid.ShowCellRefs;
        end;
        i := j;
      end
      else
        su := su + s[i];
    end
    else
      su := su + s[i];
    Inc(i);
  end;

  FCellRefList.ShowCellRefs;
end;

procedure TAdvSpreadGrid.DelCellRef(acol,arow: Integer);
var
  gc: TGridCoord;
begin
  gc.x := ACol;
  gc.y := ARow;
  FCellRefList.DeleteRef(Self,gc);
  FCellRefList.ShowCellRefs;
end;

procedure TAdvSpreadGrid.UpdCellRef;
var
  i,j: Integer;
  p: PCellRef;
begin
  if (csDestroying in ComponentState) then
    Exit;

  //  FCellRefList.DeleteAll;
  for i := FCellRefList.Count - 1 downto 0 do
  begin
    p := FCellRefList.Items[i];

    if (p^.DstGrd = Self) then
      FCellRefList.Delete(i);
  end;

  for i := FixedCols + 1 to ColCount do
    for j := FixedRows + 1 to RowCount do
    begin
      if IsFormula(Cells[i - 1,j - 1]) then
        AddCellRef(i - 1 ,j - 1,WideCells[i - 1,j - 1]);
    end;
end;

function TAdvSpreadGrid.ValidateCell(const NewValue: string): Boolean;
var
  hasref: boolean;
begin
  hasref := HasCellRef(Col,Row);
  if hasref then
    DoBeforeRecalc;

  Result := inherited ValidateCell(NewValue);
  if hasref then
    DoAfterRecalc;
end;

procedure TAdvSpreadGrid.ReCalcCell(ACol,ARow: Integer);
begin
//  FCellRefList.ClearAll;
  CalcFormula(ACol,ARow);
end;

procedure TAdvSpreadGrid.CalcCellRef(ACol,ARow: Integer);
var
  i,j: Integer;
  p, k: PCellRef;
begin
  i := 1;
  {$IFDEF TMSDEBUG}
  DbgPoint('calc cell ref',point(ACol,ARow));
  {$ENDIF}


  while (i <= FCellRefList.Count) do
  begin
    p := FCellRefList.Items[i - 1];
    if (p^.src.x = ACol) and (p^.src.y = ARow) and p^.Dirty then
    begin
      {$IFDEF TMSDEBUG}
      DbgPoint('circular',point(p^.src.x,p^.src.y));
      {$ENDIF}
      HandleError(ACol,ARow,Error_CircularReference,1,1);
    end;

    if (p^.Src.x = ACol) and (p^.Src.y = ARow) and
       (p^.SrcGrd = Self) and not p^.Dirty then

    begin
      if (p^.DstGrd = p^.SrcGrd) then
        p^.Dirty := True;

      {$IFDEF TMSDEBUG}
      DbgPoint('calc',point(p^.dst.x,p^.dst.y));
      {$ENDIF}

      p^.DstGrd.ClearCellVal(p^.Dst.x,p^.Dst.y);
      p^.DstGrd.CalcCell(p^.Dst.x,p^.Dst.y);
      p^.DstGrd.InvalidateCell(p^.Dst.x,p^.Dst.y);

      if Assigned(Binder) then
      begin
        if (Binder.Grids.Count > 1) then
          for j := 0 to p^.DstGrd.FCellRefList.Count - 1 do
          begin
            k := p^.DstGrd.FCellRefList.Items[j];

            if (k^.SrcGrd = p^.DstGrd) and (k^.Src.X = p^.Dst.x) and (k^.Src.Y = p^.Dst.y) then
              k^.Dirty := false;
          end;
      end;

      p^.DstGrd.CalcCellRef(p^.Dst.x,p^.Dst.y);

      {$IFDEF TMSDEBUG}
      DbgMsg('done recurs?');
      {$ENDIF}

      if (p^.Dst.Y = RowCount - 1) and FloatingFooter.Visible then
        UpdateFooter;
    end;
    Inc(i);
  end;
end;

procedure TAdvSpreadGrid.UnGroup;
begin
  inherited;
  AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.UpdateCell(ACol,ARow: Integer);
var
  i: Integer;
  Formula: Boolean;
begin
  {$IFDEF TMSDEBUG}
  DbgPoint('in updatecell : ',point(acol,arow));
  {$ENDIF}

  DelCellRef(ACol,ARow);

  inherited UpdateCell(ACol,ARow);

  Formula := IsFormula(Cells[ACol,ARow]);

  if Formula then
  begin
    AddCellRef(ACol,ARow,WideCells[ACol,ARow]);
    CalcFormula(ACol,ARow);
  end;

  if FAutoRecalc then
  begin
    if Assigned(Binder) then
    begin
      for i := 1 to Binder.Grids.Count do
      begin
        with Binder.Grids.Items[i - 1].Grid as TAdvSpreadGrid do
        begin
          ClearCellRefs;
          CalcCellRef(ACol,ARow);
        end;
      end;
    end
    else
    begin
      ClearCellRefs;
      CalcCellRef(ACol,ARow);
    end;
  end;
end;

procedure TAdvSpreadGrid.ClearRect(ACol1, ARow1, ACol2, ARow2: Integer);
var
  r,c : Integer;
  cg: TCellGraphic;
begin
  FCellRefList.DeleteAll;

  inherited ClearRect(acol1,arow1,acol2,arow2);

  for r := ARow1 to ARow2 do
    for c := ACol1 to ACol2 do
    begin
      if Assigned(GraphicObjects[c,r]) then
      begin
        cg := TCellGraphic(GraphicObjects[c,r]);
        cg.Free;
        GraphicObjects[c,r] := nil;
      end;
    end;

  if not (csDestroying in ComponentState) then
  begin
    UpdCellRef;
    if FAutoRecalc then Recalc;
  end;  
end;

function TAdvSpreadGrid.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSpreadGrid.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;

procedure TAdvSpreadGrid.Group(Colindex: Integer);
begin
  inherited;
  AutoSpreadHeaders;
end;

function TAdvSpreadGrid.GetIsCustomFunction:TIsCustomFunction;
begin
  Result := FGridFormula.IsCustomFunction;
end;

procedure TAdvSpreadGrid.SetIsCustomFunction(Value:TIsCustomFunction);
begin
  FGridFormula.IsCustomFunction := Value;
end;

function TAdvSpreadGrid.GetCalcCustomFunction:TCalcCustomFunction;
begin
  Result := FGridFormula.CalcCustomFunction;
end;

procedure TAdvSpreadGrid.SetCalcCustomFunction(Value:TCalcCustomFunction);
begin
  FGridFormula.CalcCustomFunction := Value;
end;

procedure TAdvSpreadGrid.MoveFormula(FromCell, ToCell: TGridCoord);
begin
  Cells[ToCell.X ,ToCell.Y] := ModifyFormula(Cells[FromCell.X,FromCell.Y],0,0,ToCell.X - FromCell.x,ToCell.Y - FromCell.y,ToCell.X - FromCell.x,ToCell.Y - FromCell.y);
end;

procedure TAdvSpreadGrid.PasteStart;
begin

end;

procedure TAdvSpreadGrid.PasteDone;
begin
  Recalc;
end; 

procedure TAdvSpreadGrid.PasteNotify(orig: TPoint; gr: TGridRect;lastop: TClipOperation);
var
  i,j,k: Integer;
  ol: TList;
  cg: TCellGraphic;
begin
  //transfer object contents as well

  if (orig.X = -1) and (orig.Y = -1) then
  begin
    Recalc;
    Exit;
  end;

  ol := TList.Create;

  for i := gr.left to gr.right do
    for j := gr.top to gr.bottom do
    begin
      ol.Add(Pointer(GraphicObjects[orig.x + i - gr.left,orig.y + j - gr.top]));
      if lastop = coCut then
        GraphicObjects[orig.x + i - gr.left,orig.y + j - gr.top] := nil;
    end;

  for i := gr.left to gr.right do
    for j := gr.top to gr.bottom do
    begin
      if IsFormula(Cells[i,j]) then
      begin
        Cells[i,j] := ModifyFormula(Cells[orig.x + i - gr.Left,orig.y + j - gr.Top],0,0,gr.left - orig.x,gr.top - orig.y,gr.left - orig.x,gr.top - orig.y);
      end;
   end;

  //this clears the objects
  k := 0;
  for i := gr.left to gr.right do
    for j := gr.top to gr.bottom do
    begin
      if lastop = coCopy then
      begin
        // if object already exists in this cell, free it

        if Assigned(GraphicObjects[i,j]) and not HasCheckbox(i,j) then
        begin
          if (GraphicObjects[i,j] is TCellGraphic) then
          begin
            (GraphicObjects[i,j] as TCellGraphic).Free;
            GraphicObjects[i,j] := nil;
          end;
        end;

        if Assigned(TObject(ol.Items[k])) then
        begin
          cg := TCellGraphic.Create;
          cg.CellType := TCellGraphic(ol.Items[k]).CellType;
          cg.CellBitmap := TCellGraphic(ol.Items[k]).CellBitmap;
          cg.CellIcon := TCellGraphic(ol.Items[k]).CellIcon;
          cg.CellVAlign := TCellGraphic(ol.Items[k]).CellVAlign;
          cg.CellHAlign := TCellGraphic(ol.Items[k]).CellHAlign;
          cg.CellIndex := TCellGraphic(ol.Items[k]).CellIndex;
          cg.CellTransparent := TCellGraphic(ol.Items[k]).CellTransparent;
          cg.CellCreated := TCellGraphic(ol.Items[k]).CellCreated;
          cg.CellBoolean := TCellGraphic(ol.Items[k]).CellBoolean;
          cg.CellAngle := TCellGraphic(ol.Items[k]).CellAngle;
          if cg.CellAngle > 0 then
          begin
            cg.CellAngle := FGridFormula.CellNameList.Add(FGridFormula.CellNameList.Strings[TCellGraphic(ol.Items[k]).CellAngle-1])+1;
            FGridFormula.CellNameList.Objects[cg.CellAngle - 1] := TObject(makelong(i,j));
          end;

          GraphicObjects[i,j] := cg;
       end
     end
    else
    begin
      GraphicObjects[i,j] := TObject(ol.Items[k]);
      {convert references here}
      if (GraphicObjects[i,j] is TCellGraphic) then
      begin
         if (GraphicObjects[i,j] as TCellGraphic).CellAngle > 0 then
         begin
           FGridFormula.CellNameList.Objects[(Objects[i,j] as TCellGraphic).CellAngle - 1] := TObject(makelong(i,j));
         end;
      end;
    end;
    Inc(k);
  end;
  ol.Free;
  
  //Recalc;
end;

procedure TAdvSpreadGrid.WMLButtonDown(var Msg: TWMLButtonDown);
var
  r,c: longint;
  s: string;
  ss: Integer;
begin
  if (GetKeyState(VK_CONTROL) and $8000 = $8000) then
  begin
    MouseToCell(msg.XPos,msg.YPos,c,r);
    if (c >= fixedcols) and (r >= fixedrows) then
    begin
      if Assigned(NormalEdit) then
      begin
        if NormalEdit.Visible then
        begin
          s := NormalEdit.Text;
          ss := NormalEdit.SelStart;
          Delete(s,normaledit.SelStart + 1, NormalEdit.SelLength);
          Insert(CellToName(c,r),s,normaledit.selstart+1);
          Normaledit.Text := s;
          Normaledit.Selstart := ss;
          Normaledit.Sellength := Length(CellToName(c,r));
          Msg.Result := 0;
          Exit;
        end;
      end;
    end;
  end;
  inherited;
end;

function TAdvSpreadGrid.GetCellName(c, r: integer): string;
var
  cg: TCellGraphic;
begin
  if Assigned(GraphicObjects[c,r]) then
  begin
    cg := TCellGraphic(GraphicObjects[c,r]);
    if cg.CellAngle > 0 then
      Result := FGridFormula.CellNameList.Strings[cg.CellAngle - 1];
  end
  else
    Result := '';
end;

function TAdvSpreadGrid.FindCellName(value: string): TGridCoord;
var
  i,o: integer;
begin
  Result := GridCoord(-1,-1);

  i := FGridFormula.CellNameList.IndexOf(value);

  if (i >= 0) then
  begin
    o := integer(FGridFormula.CellNameList.Objects[i]);
    Result := GridCoord( (o and $FFFF), ((o shr 16) and $FFFF));
  end;

end;

procedure TAdvSpreadGrid.SetCellName(c, r: integer; const Value: string);
var
  cg: TCellGraphic;
begin
  if Assigned(GraphicObjects[c,r]) then
  begin
    cg := TCellGraphic(GraphicObjects[c,r]);

    if cg.CellAngle > 0 then
    begin
      FGridFormula.CellNameList.Strings[cg.CellAngle - 1] := Value
    end
    else
    begin
      cg.CellAngle := FGridFormula.CellNameList.Add(Value) + 1;
      FGridFormula.CellNameList.Objects[cg.CellAngle - 1] := TObject(makelong(c,r));
    end;
  end
  else
  begin
    cg := TCellGraphic.Create;
    cg.Celltype := ctNone;
    cg.CellAngle := FGridFormula.CellNameList.Add(Value) + 1;
    FGridFormula.CellNameList.Objects[cg.CellAngle - 1] := TObject(makelong(c,r));
    GraphicObjects[c,r] := cg;
  end;
end;

procedure TAdvSpreadGrid.SetCellNameMode(const Value: TCellNameMode);
begin
  FCellNameMode := Value;
  if FAutoHeaders then
  begin
    AutoSpreadHeaders;
    Invalidate;
  end;
  FGridFormula.UseRCNames := value = nmRC;
end;

procedure TAdvSpreadGrid.SetAutoHeaders(const Value: boolean);
begin
  FAutoHeaders := Value;
  if not (csLoading in ComponentState) then
  begin
    if FAutoHeaders then AutoSpreadHeaders else
    begin
      if FixedRows > 0 then ClearRect(0,0,ColCount - 1,0);
      if FixedCols > 0 then ClearRect(0,0,0,RowCount - 1);
    end;
  end;
end;

procedure TAdvSpreadGrid.GetCellAlign(ACol, ARow: integer;
  var HAlign: TAlignment;var VAlign: TVAlignment);
begin
  if FAutoHeaders then
    if (ACol = 0) or (ARow = 0) then HAlign := taCenter;
  inherited;
end;

function TAdvSpreadGrid.GetFixedCols: integer;
begin
  Result := inherited FixedCols;
end;

function TAdvSpreadGrid.GetFixedRows: integer;
begin
  Result := inherited FixedRows;
end;

procedure TAdvSpreadGrid.SetFixedCols(const Value: integer);
begin
  inherited FixedCols := Value;
  if FAutoHeaders and (Value = 0) then
    ClearRect(0,0,0,RowCount - 1);
  if not (csLoading in ComponentState) then
    AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.SetFixedRows(const Value: integer);
begin
  inherited FixedRows := Value;
  if FAutoHeaders and (Value = 0) then
    ClearRect(0,0,ColCount - 1,0);
  if not (csLoading in ComponentState) then
    AutoSpreadHeaders;
end;

function TAdvSpreadGrid.GetCellsEx(c, r: integer): string;
begin
  Result := inherited Cells[c,r];
end;

procedure TAdvSpreadGrid.SetCellEx(c, r: integer; const Value: string);
begin
  inherited Cells[c,r] := Value;
  if FAutoRecalc and IsFormula(Value) then
  begin
    FCellRefList.ClearAll;
    RecalcCell(c,r);
  end;
end;

procedure TAdvSpreadGrid.Loaded;
begin
  inherited;
  if FAutoHeaders then
    AutoSpreadHeaders;
end;

function TAdvSpreadGrid.GetColCountEx: Integer;
begin
  Result := inherited ColCount;
end;

function TAdvSpreadGrid.GetRowCountEx: Integer;
begin
  Result := inherited RowCount;
end;

procedure TAdvSpreadGrid.SetColCountEx(const Value: Integer);
begin
  inherited ColCount := Value;
  if AutoHeaders then
    AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.SetRowCountEx(const Value: Integer);
begin
  inherited RowCount := Value;
  if AutoHeaders then
    AutoSpreadHeaders;
end;

procedure TAdvSpreadGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FBinder) then
    FBinder := nil;

  if (AOperation = opRemove) and (AComponent = FMathLib) then
    FMathLib := nil;
end;

procedure TAdvSpreadGrid.ClearCellRefs;
begin
  FCellRefList.ClearAll;
end;

procedure TAdvSpreadGrid.ShowCellRefs;
begin
  {$IFDEF TMSDEBUG}
  DbgMsg('cell ref list from',Name);
  {$ENDIF}
  FCellRefList.ShowCellRefs;
end;

procedure TAdvSpreadGrid.ErrorHandler(Sender: TObject; ACol, ARow, ErrType,
  ErrPos, ErrParam: Integer; ErrStr: string);
var
  ErrLen: Integer;
begin
  if ErrType in [Error_InvalidQualifier,Error_InvalidGridRef] then
    ErrLen := Max(1,Length(ErrStr))
  else
    ErrLen := 1;

  SetCellError(ACol,ARow,ErrPos,ErrLen);

  HandleError(ACol,ARow,ErrType,ErrPos,ErrParam);
end;

function TAdvSpreadGrid.ErrorToString(ErrType: Integer): string;
begin
  case ErrType of
  Error_NoFormula             : Result := 'No formula';
  Error_DivisionByZero        : Result := 'Division by zero';
  Error_InvalidValue          : Result := 'Invalid value';
  Error_InvalidCellRef        : Result := 'Invalid cell reference';
  Error_InvalidRangeRef       : Result := 'Invalid range reference';
  Error_InvalidGridRef        : Result := 'Invalid grid reference';
  Error_InvalidNrOfParams     : Result := 'Invalid nr. of parameters';
  Error_CircularReference     : Result := 'Circular reference';
  Error_NoOpenParenthesis     : Result := 'No opening parenthesis';
  Error_NoCloseParenthesis    : Result := 'No closing parenthesis';
  Error_PrematureEndOfFormula : Result := 'Premature end of formula';
  Error_UnknownError          : Result := 'Unknown error';
  Error_InvalidQualifier      : Result := 'Invalid qualifier';
  Error_InvalidTokenAtPosition: Result := 'Invalid token';
  Error_Overflow              : Result := 'Overflow';
  Error_Underflow             : Result := 'Underflow';
  Error_CircularRange         : Result := 'Circular range reference in formula'; 
  end;
end;

procedure TAdvSpreadGrid.ClearCellVal(ACol, ARow: Integer);
var
  cg: TCellGraphic;
begin
  if Assigned(GraphicObjects[ACol,ARow]) then
  begin
    cg := TCellGraphic(GraphicObjects[ACol,ARow]);
    cg.CellBoolean := False;
    cg.CellErrFrom := 0;
    cg.CellErrLen := 0;
  end;
end;

function TAdvSpreadGrid.GetCellState(ACol, ARow: Integer): TCellCalcState;
var
  cg: TCellGraphic;
begin
  Result := csNoCalc;

  if Assigned(GraphicObjects[ACol,ARow]) then
  begin
    {$IFDEF TMSDEBUG_LVL1}
    DbgMsg('get cell state',inttostr(ACol)+':'+inttostr(ARow));
    {$ENDIF}
    cg := TCellGraphic(GraphicObjects[ACol,ARow]);
    if cg.CellErrLen <> 0 then
      Result := csCalcErr
    else
      if cg.CellBoolean then
        Result := csCalcOK;
  end;
end;

procedure TAdvSpreadGrid.SetCellVal(ACol, ARow: Integer; r: variant);
var
  w: widestring;
begin
  if FSetCV then
    Exit;

  FSetCV := true;
  if not Assigned(GraphicObjects[ACol,ARow]) then
  begin
    GraphicObjects[ACol,ARow] := TCellGraphic.Create;
  end;

  FSetCV := false;

  with TCellGraphic(GraphicObjects[ACol,ARow]) do
  begin
    {$IFDEF TMSDEBUG_LVL1}
    DbgVar('set cell val',r);
    {$ENDIF}
    Celltype := ctValue;

    if VarType(r) = varOleStr then
    begin
      w := r;
      CellVar := EncodeWideStr(w);
    end
    else
      CellVar := r;
      
    CellBoolean := True;
  end;
end;

function TAdvSpreadGrid.GetCellVal(ACol, ARow: Integer): Variant;
begin
  if Assigned(GraphicObjects[ACol,ARow]) then
    Result := TCellGraphic(GraphicObjects[ACol,ARow]).CellVar
  else
    Result := 0.0;

  {$IFDEF TMSDEBUG_LVL1}
  DbgVar('get cell val',Result);
  {$ENDIF}

end;

procedure TAdvSpreadGrid.SetCellError(ACol, ARow, ErrPos, ErrLen: Integer);
begin
  if FSetCV then
    Exit;

  FSetCV := true;
  if not Assigned(GraphicObjects[ACol,ARow]) then
  begin
    GraphicObjects[ACol,ARow] := TCellGraphic.Create;
    TCellGraphic(GraphicObjects[ACol,ARow]).CellType := ctEmpty;
  end;

  with TCellGraphic(GraphicObjects[ACol,ARow]) do
  begin
    CellErrFrom := ErrPos;
    CellErrLen := ErrLen;
  end;
  FSetCV := false;
end;

{
procedure TAdvSpreadGrid.RemoveCellError(ACol, ARow: Integer);
var
  cg: TCellGraphic;
begin
  cg := TCellGraphic(Objects[ACol,ARow]);

  if not Assigned(cg) then
    Exit;

  cg.CellErrFrom := 0;
  cg.CellErrLen := 0;
end;
}

procedure TAdvSpreadGrid.EditProgress(Value: string; pt: TPoint; SelPos: Integer);
var
  r: TRect;
  func:string;
  param:integer;
  isfunc:boolean;
  ch: char;

begin
  if not FEditHint then Exit;

  if (SelPos = -1) or (Value = '') and FScrollHintShow then
  begin
    FScrollHintWnd.ReleaseHandle;
    FScrollHintShow := False;
    Exit;
  end;

  Value := Copy(Value,1,SelPos);

  func := '';

  if (System.Pos('(',value) > 0) and IsFormula(Value) then
  begin
    param := 0;
    func := '';
    isfunc := False;

    while Length(value) > 0 do
    begin
      ch := value[Length(value)];

      {$IFDEF DELPHI_UNICODE}
      if (CharInSet(ch, [',',';'])) and (ch <> DecimalSeparator) then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if (ch in [',',';']) and (ch <> DecimalSeparator) then
      {$ENDIF}
        inc(param);

      if (ch = ')') and (func = '') then
      begin
        func := '';
        value := '';
      end;

      if (ch = '(') and (func = '') then
        isfunc := true
      else
      begin
        {$IFDEF DELPHI_UNICODE}
        if isfunc and (CharInSet(ch, ['a'..'z','A'..'Z','0'..'9'])) then
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
        if isfunc and (ch in ['a'..'z','A'..'Z','0'..'9']) then
        {$ENDIF}
          func := ch + func
        else
          isfunc := false;
      end;

      delete(value,length(value),1);
    end;

    if func <> '' then
      func := GetEditHint(UpperCase(func),param);

    if func <> '' then
    begin
      pt := NormalEdit.ClientToScreen(pt);

      r := FScrollHintWnd.CalcHinTRect(600,func,Nil);
      FScrollHintWnd.Caption := Value;
      FScrollHintWnd.Color := self.HintColor;

      r.Left := r.Left + pt.x;
      r.Right := r.Right + pt.x;
      r.Top := r.Top + pt.y - (R.Bottom - R.Top) - 8;
      r.Bottom := r.Bottom + pt.y;
      FScrollHintWnd.ActivateHint(r,func);
      FScrollHintShow := True;
    end
    else
    begin
      FScrollHintWnd.ReleaseHandle;
      FScrollHintShow := False;
    end;
  end
  else
  begin
    FScrollHintWnd.ReleaseHandle;
    FScrollHintShow := False;
  end;
end;


function TAdvSpreadGrid.GetEditHint(Func: string;
  ParamNr: Integer): string;
var
  j: Integer;
begin
  Result := '';

  if Assigned(FMathLib) then
  begin
    for j := 1 to FMathLib.Libs.Count do
    begin
      if Assigned(FMathLib.Libs.Items[j - 1]) then
      begin
        if FMathLib.Libs.Items[j - 1].MathLib.HandlesFunction(Func) then
          Result := FMathLib.Libs.Items[j - 1].MathLib.GetEditHint(Func,ParamNr);
      end;
    end;
  end;

  if Result = '' then
  begin
    if Func = 'CHOOSE' then
    case paramnr of
    0: Result := 'Choose(<b>Selector</b>;Value 1; Value 2: Double):Double<HR><i>If Selector &lt;&gt; 0, returns Value 2 otherwise Value 1</i>';
    1: Result := 'Choose(Selector;<b>Value 1</b>; Value 2: Double):Double<HR><i>If Selector &lt;&gt; 0, returns Value 2 otherwise Value 1</i>';
    2: Result := 'Choose(Selector;Value 1; <b>Value 2: Double</b>):Double<HR><i>If Selector &lt;&gt; 0, returns Value 2 otherwise Value 1</i>';
    end;

    if Func = 'LT' then
    case paramnr of
    0: Result := 'Lt(<b>Value 1</b>; Value 2: Double):Double<HR><i>Returns 1 if Value 1 &lt; Value 2 or 0 otherwise</i>';
    1: Result := 'Lt(Value 1; <b>Value 2: Double</b>):Double<HR><i>Returns 1 if Value 1 &gt; Value 2 or 0 otherwise</i>';
    end;

    if Func = 'GT' then
    case paramnr of
    0: Result := 'Gt(<b>Value 1</b>; Value 2: Double):Double<HR><i>Returns 1 if Value 1 &gt; Value 2 or 0 otherwise</i>';
    1: Result := 'Gt(Value 1; <b>Value 2: Double</b>):Double<HR><i>Returns 1 if Value 1 &lt; Value 2 or 0 otherwise</i>';
    end;

    if Func = 'EQ' then
    case paramnr of
    0: Result := 'Eq(<b>Value 1</b>; Value 2: Double):Double<HR><i>Returns 1 if Value 1 = Value 2 or 0 otherwise</i>';
    1: Result := 'Eq(Value 1; <b>Value 2: Double</b>):Double<HR><i>Returns 1 if Value 1 = Value 2 or 0 otherwise</i>';
    end;

  end;

end;

procedure TAdvSpreadGrid.CellsLoaded;
begin
  inherited;
  if FAutoHeaders then
    AutoSpreadHeaders;

  if FAutoRecalc then
    Recalc;
end;


procedure TAdvSpreadGrid.HandleError(ACol, ARow, ErrType, ErrPos,
  ErrParam: Integer);
begin
  if Assigned(FOnFormulaError) then
    FOnFormulaError(Self,ACol,ARow,ErrorToString(ErrType));

  if Assigned(FOnFormulaErrorInfo) then
    FOnFormulaErrorInfo(Self,ACol,ARow,ErrType,ErrPos,ErrParam,ErrorToString(ErrType));
end;

procedure TAdvSpreadGrid.DoAfterRecalc;
begin
  if Assigned(OnAfterRecalc) then
    OnAfterRecalc(Self);
end;

procedure TAdvSpreadGrid.DoBeforeRecalc;
begin
  if Assigned(OnBeforeRecalc) then
    OnBeforeRecalc(Self);
end;

function TAdvSpreadGrid.CellNameList: TStringList;
begin
  Result := FGridFormula.CellNameList;
end;

procedure TAdvSpreadGrid.CellSelect(c, r: Integer);
begin
  if (c >= FixedCols) and (r >= FixedRows) then
  begin
    NormalEdit.Text := NormalEdit.Text + CellToName(c,r);
    NormalEdit.SelStart := Length(NormalEdit.Text);
    SetEditText(Col,Row,NormalEdit.Text);
  end;
end;

procedure TAdvSpreadGrid.SetCellSelectMode(const Value: Boolean);
begin
  CellSelectorMode := value and FFormulaCellClick;
end;

{TCellRefList}

constructor TCellRefList.Create;
begin
  inherited Create;
end;

procedure TCellRefList.Add(Value: TCellRef);
var
  p: PCellRef;
begin
  if (Value.Src.X = Value.Dst.X) and
     (Value.Src.Y = Value.Dst.Y) and
     (Value.SrcGrd = Value.DstGrd) then
    Exit;

  New(p);
  p^ := Value;
  inherited Add(Pointer(p));
end;

procedure TCellRefList.Delete(Index: Integer);
var
  p:PCellRef;
begin
  p := Items[index];
  Dispose(p);
  inherited Delete(Index);
end;

destructor TCellRefList.Destroy;
begin
  ShowCellRefs;
  DeleteAll;
  inherited Destroy;
end;

function TCellRefList.GetCellRef(Index: Integer): TCellRef;
var
  p: PCellRef;
begin
  p := PCellRef(Items[index]);
  Result := p^;
end;

procedure TCellRefList.ShowCellRefs;
{$IFDEF TMSDEBUG}
var
  i: Integer;
  p: PCellRef;
{$ENDIF}

begin
 {$IFDEF TMSDEBUG}
  for i := 1 to Count do
  begin
    p := Items[i-1];
    OutputdebugString(pchar(p^.SrcGrd.Name+' Src ('+inttostr(p^.Src.X)+':'+inttostr(p^.Src.Y)+') -> Dst '+p^.DstGrd.Name+' ('+
                               inttostr(p^.Dst.X)+':'+inttostr(p^.Dst.Y)+')'));
  end;
{$ENDIF}
end;

procedure TCellRefList.DeleteRef(AGrid:TAdvSpreadGrid;gc: TGridCoord);
var
 i: Integer;
 p: PCellRef;

begin
  i := 1;
  while  i <= Count do
  begin
    p := Items[i - 1];
    if (p^.Dst.x  = gc.x) and (p^.Dst.y = gc.y) and (p^.DstGrd = AGrid) then
      Delete(i - 1)
    else
      Inc(i);
  end;
end;

procedure TCellRefList.DeleteAll;
begin
  while Count > 0 do
    Delete(Count - 1);
end;

procedure TCellRefList.ClearAll;
var
  i: Integer;
  p: PCellRef;
begin
  for i := 1 to Count do
  begin
    p := Items[i - 1];
    p^.Dirty := False;
  end;
end;

procedure TCellRefList.ClearCell(ACol,ARow: Integer);
var
  i: Integer;
  p: PCellRef;
begin
  for i := 1 to Count do
  begin
    p := Items[i - 1];
    if (p^.src.X <> ACol) or (p^.src.Y <> ARow) then
      p^.Dirty := False;
  end;
end;

initialization
  {$IFDEF ISDELPHI}
  try
    RegisterClass(TAdvSpreadGrid);
  except
  end;
  {$ENDIF}

end.
