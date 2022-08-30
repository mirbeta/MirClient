{*************************************************************************}
{ TMATHLIBS components                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{    TMS Software                                                         }
{    copyright © 2002 - 2015                                              }
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

unit MiscMathLib;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AdvPars, DB;

type
  TMiscMathLib = class(TMathLib)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function HandlesFunction(FuncName:string):Boolean; override;
    function CalcFunction(FuncName:string;Params:TParamList;var ErrType,ErrParam: Integer):Double; override;
    function GetEditHint(FuncName:string;ParamIndex: Integer):string; override;
  published
    { Published declarations }
  end;

  TFinanceMathLib = class(TMathLib)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function HandlesFunction(FuncName:string):Boolean; override;
    function CalcFunction(FuncName:string;Params:TParamList;var ErrType,ErrParam: Integer):Double; override;
  published
    { Published declarations }
  end;

  TConversionMathLib = class(TMathLib)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function HandlesFunction(FuncName:string):Boolean; override;
    function CalcFunction(FuncName:string;Params:TParamList;var ErrType,ErrParam: Integer):Double; override;
  published
    { Published declarations }
  end;

  TStringMathLib = class(TMathLib)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function HandlesStrFunction(FuncName:string):Boolean; override;
    function CalcStrFunction(FuncName:string;Params:TStringList;var ErrType,ErrParam: Integer):string; override;
  published
    { Published declarations }
  end;

  TDBMathLib = class(TMathLib)
  private
    { Private declarations }
    FDataSource: TDataSource;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    { Public declarations }
    function HandlesStrFunction(FuncName:string):Boolean; override;
    function CalcStrFunction(FuncName:string;Params:TStringList;var ErrType,ErrParam: Integer):string; override;
  published
    { Published declarations }
    property DataSource: TDataSource read FDataSource write FDataSource;
  end;


implementation


{ TMiscMathLib }

function TMiscMathLib.CalcFunction(FuncName: string; Params: TParamList;
  var ErrType,ErrParam: Integer): Double;
var
  k: Integer;
  d: Double;

begin
  Result := 0.0;

  ErrType := Error_NoError;

  if FuncName = 'HARMEAN' then
  begin
    d := 0;
    for k := 1 to Params.Count do
    begin
      if Params.Items[k - 1] <> 0 then
      begin
        d := d + (1 / Params.Items[k - 1]);
      end
      else
      begin
        ErrType := Error_DivisionByZero;
        ErrParam := k - 1;
      end;
      Result := 1/d * Params.Count;
    end;
    Exit;
  end;

  if FuncName = 'GEOMEAN' then
  begin
    d := 1;
    for k := 1 to Params.Count do
    begin
      d := d * Params.Items[k - 1];
    end;
    if Params.Count > 0 then
    begin
      Result := exp(1/Params.Count * ln(d));
    end
    else
    begin
      ErrType := Error_DivisionByZero;
      ErrParam := 0;
    end;
  end;

  if FuncName = 'ROUNDUP' then
  begin
    if params.Count = 2 then
    begin
      d := Exp(params.Items[1] * ln(10));
      Result := round((params.Items[0] * d) + 0.5) / d;
    end
    else
    begin
      ErrType := Error_InvalidNrOfParams;
      ErrParam := 0;
    end;
  end;
end;

function TMiscMathLib.GetEditHint(FuncName: string;
  ParamIndex: Integer): string;
begin
  if FuncName = 'HARMEAN' then
    Result := 'HarMean(<b>Param1,..ParamN: Double</b>):Double<HR>Calculates the harmonic mean of parameters';
  if FuncName = 'GEOMEAN' then
    Result := 'GeoMean(<b>Param1,..ParamN: Double</b>):Double<HR>Calculates the geometric mean of parameters';
  if FuncName = 'ROUNDUP' then
    Result := 'RoundUp(<b>Value,NumberOfDigits: Double</b>):Double<HR>Rounds up a value to the specified nr. of digits';
end;

function TMiscMathLib.HandlesFunction(FuncName: string): boolean;
begin
  Result := (FuncName = 'HARMEAN') or
            (FuncName = 'GEOMEAN') or
            (FuncName = 'ROUNDUP');
end;

{ TFinanceMathLib }

function Power(number, exponent : Double) : Double;
begin
  if number > 0.0 then
    Power := Exp(exponent * ln(number))
  else
    Power := 0.0
end;

// number of compounding periods for initial amount "PresentValue" to
// accumulate into amount "FutureValue" at interest "Rate"
function Cterm(Rate : Double; FutureValue, PresentValue : Double) : Double;
begin
  Cterm := ln ((FutureValue) / (PresentValue)) / ln(1 + Rate);
end;

// number of compounding periods required to accumulate "FutureValue" by making
// periodic deposits of "Payment" with interest accumulating at "rate" per period
function Term(Payment : Double; Rate : Double; FutureValue : Double) : Double;
begin
  Term := (ln(1 + (FutureValue) * (Rate / (Payment))) / ln(1 + Rate));
end;

// payment amount per interval on loan or annuity of initial value "Pincipal"
// with payments spread out over "nper" intervals and with interest accruing
// at "rate" per interval
function Pmt(Principal : Double; Rate : Double; Term : Double) : Double;
begin
  Pmt := (Principal) * (Rate / (1 - Power(1 + Rate, - Term)));
end;

// determines interest rate per interval when initial amount "PresentValue"
// accumulates into amount "FutureValue" by compounding over "nper" intervals
function Rate(FutureValue, PresentValue : Double; Term : Double) : Double;
begin
  Rate := Power((FutureValue) / (PresentValue), 1 / Term) - 1;
end;

// initial value of loan or annuity that can be paid off by making "Term"
// payments of "Payment" which interest on the unpaid amount accrues at
// "rate" per interval }
function Pv(Payment : Double; Rate : Double; Term : Double) : Double;
begin
  Pv := ((Payment) * (1 - Power(1 + Rate, - Term)) / Rate);
end;

function TFinanceMathLib.CalcFunction(FuncName: string; Params: TParamList;
  var ErrType,ErrParam: Integer): Double;
var
  p,k: Integer;
  d: Double;

begin
  Result := 0.0;
  ErrType := 0;

  if FuncName = 'CUMINT' then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams;
    if ErrType = 0 then
    begin
      p := Round(Params.Items[2]);
      d := Params.Items[0];
      for k := 1 to p do
         d := d + (d * Params.Items[1]/100);
      Result := d;
    end;
  end;

  if (FuncName = 'CTERM') then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams
    else
    begin
      if Params.Items[1] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 2;
        Exit;
      end;
      if Params.Items[2] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 3;
        Exit;
      end;

      Result := CTerm(Params.Items[0],Params.Items[1],Params.Items[2]);
    end;
  end;

  if (FuncName = 'TERM') then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams
    else
    begin
      if Params.Items[1] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 2;
        Exit;
      end;
      if Params.Items[2] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 3;
        Exit;
      end;
      Result := Term(Params.Items[0],Params.Items[1],Params.Items[2]);
    end;
  end;

  if (FuncName = 'RATE') then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams
    else
    begin
      if Params.Items[1] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 2;
        Exit;
      end;
      if Params.Items[2] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 3;
        Exit;
      end;
      Result := Rate(Params.Items[0],Params.Items[1],Params.Items[2]);
    end;
  end;

  if (FuncName = 'PV') then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams
    else
    begin
      if Params.Items[1] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 2;
        Exit;
      end;
      if Params.Items[2] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 3;
        Exit;
      end;
      Result := Pv(Params.Items[0],Params.Items[1],Params.Items[2]);
    end;

  end;

  if (FuncName = 'PMT') then
  begin
    if Params.Count <> 3 then
      ErrType := Error_InvalidNrOfParams
    else
    begin
      if Params.Items[1] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 2;
        Exit;
      end;
      if Params.Items[2] <= 0 then
      begin
        ErrType := Error_InvalidValue;
        ErrParam := 3;
        Exit;
      end;
      Result := Pmt(Params.Items[0],Params.Items[1],Params.Items[2]);
    end;
  end;
end;

function TFinanceMathLib.HandlesFunction(FuncName: string): Boolean;
begin
  Result := (FuncName = 'CUMINT') or
            (FuncName = 'CTERM') or
            (FuncName = 'TERM') or
            (FuncName = 'RATE') or
            (FuncName = 'PV') or
            (FuncName = 'PMT');
end;

{ TConversionMathLib }


function CelsToFahr(Value : Double) : Double;
begin
  CelsToFahr := 9 / 5 * Value + 32;
end;

function FahrToCels(Value : Double) : Double;
begin
  FahrToCels := 5 / 9 * (Value - 32);
end;

function GalToL(Value : Double) : Double;
begin
  GalToL := Value * 3.785411784;
end;

function LToGal(Value : Double) : Double;
begin
  LToGal := Value / 3.785411784;
end;

function InchToCm(Value : Double) : Double;
begin
  InchToCm := Value * 2.54;
end;

function CmToInch(Value : Double) : Double;
begin
  CmToInch := Value / 2.54;
end;

function LbToKg(Value : Double) : Double;
begin
  LbToKg := Value * 0.45359237;
end;

function KgToLb(Value : Double) : Double;
begin
  KgToLb := Value / 0.45359237;
end;


function TConversionMathLib.CalcFunction(FuncName: string;
  Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
  Result := 0.0;
  ErrType := 0;

  if Params.Count > 1 then
  begin
    ErrType := Error_InvalidNrOfParams;
  end;

  if (FuncName = 'CELSTOFAHR') then
  begin
    Result := CelsToFahr(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'FAHRTOCELS') then
  begin
    Result := FahrToCels(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'GALTOL') then
  begin
    Result := GalToL(Params.Items[0]);
  end;


  if (FuncName = 'LTOGAL') then
  begin
    Result := LToGal(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'INCHTOCM') then
  begin
    Result := InchToCm(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'CMTOINCH') then
  begin
    Result := CmToInch(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'LBTOKG') then
  begin
    Result := LbToKg(Params.Items[0]);
    Exit;
  end;

  if (FuncName = 'KGTOLB') then
  begin
    Result := KgToLb(Params.Items[0]);
    Exit;
  end;



end;

function TConversionMathLib.HandlesFunction(FuncName: string): Boolean;
begin
  Result := (FuncName = 'CELSTOFAHR') or
            (FuncName = 'FAHRTOCELS') or
            (FuncName = 'GALTOL') or
            (FuncName = 'LTOGAL') or
            (FuncName = 'INCHTOCM') or
            (FuncName = 'CMTOINCH') or
            (FuncName = 'LBTOKG') or
            (FuncName = 'KGTOLB');
end;





{ TStringMathLib }

function TStringMathLib.CalcStrFunction(FuncName: string;
  Params: TStringList; var ErrType, ErrParam: Integer): String;
var
  i: Integer;
  s: string;
begin
  Result := '';
  ErrType := 0;

  if Params.Count = 0 then
  begin
    ErrType := Error_InvalidNrOfParams;
    Exit;
  end;

  Result := '';

  if FuncName = 'REVERSE' then
  begin
    s := Params.Strings[0];
    for i := 1 to Length(s) do
      Result := Result + s[Length(s)- i + 1];
  end;

  if FuncName = 'CAPITALIZE' then
  begin
    s := Params.Strings[0];
    for i := 1 to Length(s) do
      if (i = 1) or ( (i > 1) and (s[i - 1]=' ')) then
        Result := Result + upcase(s[i])
      else
        Result := Result + s[i];
  end;

  if FuncName = 'IF' then
  begin
    s := Params.Strings[0];
    if s = '1' then
      Result := Params.Strings[1]
    else
      if Params.count > 2 then Result := Params.Strings[2];
  end;

  if FuncName = 'GETVAR' then
  begin
    s := Params.Strings[0];

    if s='A' then
      Result := '1';
    if s='B' then
      Result := '2';
    if s='C' then
      Result := '3';
  end;

end;

function TStringMathLib.HandlesStrFunction(FuncName: string): Boolean;
begin
  Result := (FuncName = 'REVERSE') or
            (FuncName = 'CAPITALIZE') or
            (FuncName = 'IF') or
            (FuncName = 'GETVAR');
end;

{ TDBMathLib }

function TDBMathLib.CalcStrFunction(FuncName: string; Params: TStringList;
  var ErrType, ErrParam: Integer): string;
var
  s: string;
  n,e: integer;
  fld: TField;
begin
  if (FuncName = 'DBV') then
  begin
    if Params.Count <> 2 then
    begin
      ErrType := Error_InvalidNrOfParams;
      Exit;
    end;

    if not Assigned(DataSource) then
    begin
      ErrType := Error_NoDataSource;
      Exit;
    end;

    if not Assigned(DataSource.DataSet) then
    begin
      ErrType := Error_NoDataSet;
      Exit;
    end;

    if not DataSource.DataSet.Active then
    begin
      ErrType := Error_NoDataSetActive;
      Exit;
    end;

    s := Params.Strings[0]; // DB FIELD value

    fld := DataSource.DataSet.FieldByName(s);

    if not Assigned(fld) then
    begin
      ErrType := Error_InvalidValue;
      ErrParam := 1;
    end
    else
    begin
      val(Params.Strings[1],n, e);

      DataSource.DataSet.First;
      DataSource.DataSet.MoveBy(n);

      Result := fld.AsString;
    end;
  end;
end;

function TDBMathLib.HandlesStrFunction(FuncName: string): Boolean;
begin
  Result := FuncName = 'DBV';
end;

procedure TDBMathLib.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FDataSource) then
    FDataSource := nil;
end;

end.
