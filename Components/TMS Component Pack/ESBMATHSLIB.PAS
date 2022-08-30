{*************************************************************************}
{ TESBMATHsLIB component for TMS Software's TAdvSpreadGrid                }
{ for Borland Delphi 3.0, 4.0, 5.0, 6.0 & C++ Builder 3.0, 4.0 & 5.0      }
{ version 1.0, 14 June 2001                                                }
{                                                                         }
{ written by                                                              }
{    ESB Consultancy                                                      }
{    copyright © 2001                                                     }
{    Email:support@esbconsult.com.au                                      }
{    Web:http://www.esbconsult.com.au                                     }
{                                                                         }
{ Based on work by                                                        }
{    TMS Software                                                         }
{    copyright © 2001                                                     }
{    Email:info@tmssoftware.com                                           }
{    Web:http://www.tmssoftware.com                                       }
{                                                                         }
{ Requires the Freeware ESBMaths available from:                          }
{    http://www.esbconsult.com.au                                         }
{                                                                         }
{ and naturally also requires TAdvSpreadGrid from:                        }
{    http://www.tmssoftware.com.au                                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{$IFDEF VER120}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER125}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER130}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE D4andAbove}
{$ENDIF}

{$J-} // Constants from here are not assignable
unit ESBMathsLib;

interface

uses
	Messages, Classes, Graphics, Controls, Forms, AdvPars;

{$IFNDEF D4andAbove}
type
	LongWord = Cardinal;
{$ENDIF}

type
  TESBMathsLib = class(TMathLib)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function HandlesConstant(Constant:string): Boolean; override;
    function GetConstant(Constant:string): Double; override;
    function HandlesFunction(FuncName:string):Boolean; override;
    function CalcFunction(FuncName:string;Params:TParamList;var ErrType,ErrParam: Integer):Double; override;
    function GetEditHint(FuncName:string;ParamIndex: Integer):string; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
	ESBMaths;


procedure Register;
begin
	RegisterComponents('TMS Grids', [TESBMathsLib]);
end;

{ TESBMathsLib }

function TESBMathsLib.GetConstant(Constant: string): Double;
begin
	Result := 0.0;
	
	if Constant = 'SQRT2' then
		Result := Sqrt2
	else	if Constant = 'SQRT3' then
		Result := Sqrt3
	else	if Constant = 'SQRT5' then
		Result := Sqrt5
	else	if Constant = 'SQRT10' then
		Result := Sqrt10
	else	if Constant = 'SQRTPI' then
		Result := SqrtPi
	else	if Constant = 'CBRT2' then
		Result := Cbrt2
	else	if Constant = 'CBRT3' then
		Result := Cbrt3
	else	if Constant = 'CBRT10' then
		Result := Cbrt10
	else	if Constant = 'CBRT100' then
		Result := Cbrt100
	else	if Constant = 'CBRTPI' then
		Result := CbrtPi
	else	if Constant = 'INVSQRT2' then
		Result := InvSqrt2
	else	if Constant = 'INVSQRT3' then
		Result := InvSqrt3
	else	if Constant = 'INVSQRT5' then
		Result := InvSqrt5
	else	if Constant = 'INVSQRTPI' then
		Result := InvSqrtPI
	else	if Constant = 'INVCBRTPI' then
		Result := InvCbrtPi
	else	if Constant = 'ESBE' then
		Result := ESBe
	else	if Constant = 'ESBE2' then
		Result := ESBe2
	else	if Constant = 'ESBEPI' then
		Result := ESBePi
	else	if Constant = 'ESBEPION2' then
		Result := ESBePiOn2
	else	if Constant = 'ESBEPION4' then
		Result := ESBePiOn4
	else if Constant = 'LN2' then
	    Result := Ln2
	else if Constant = 'LN10' then
	    Result := Ln10
	else if Constant = 'LNPI' then
	    Result := LnPi
	else if Constant = 'LOG10BASE2' then
	    Result := Log10Base2
	else if Constant = 'LOG2BASE10' then
	    Result := Log2Base10
	else if Constant = 'LOG3BASE10' then
	    Result := Log3Base10
	else if Constant = 'LOGPIBASE10' then
	    Result := LogPiBase10
	else if Constant = 'LOGEBASE10' then
	    Result := LogEBase10
	else if Constant = 'ESBPI' then
	    Result := ESBPi
	else if Constant = 'INVPI' then
	    Result := InvPi
	else if Constant = 'TWOPI' then
	    Result := TwoPi
	else if Constant = 'THREEPI' then
	    Result := ThreePi
	else if Constant = 'PI2' then
	    Result := Pi2
	else if Constant = 'PITOE' then
	    Result := PiToE
	else if Constant = 'PION2' then
	    Result := PiOn2
	else if Constant = 'PION3' then
	    Result := PiOn3
	else if Constant = 'PION4' then
	    Result := PiOn4
	else if Constant = 'THREEPION2' then
	    Result := ThreePiOn2
	else if Constant = 'FOURPION3' then
	    Result := FourPiOn3
	else if Constant = 'TWOTO63' then
	    Result := TwoToPower63
	else if Constant = 'ONERADIAN' then
	    Result := OneRadian
	else if Constant = 'ONEDEGREE' then
	    Result := OneDegree
	else if Constant = 'ONEMINUTE' then
	    Result := OneMinute
	else if Constant = 'ONESECOND' then
	    Result := OneSecond
	else if Constant = 'ESBGAMMA' then
	    Result := ESBGamma
	else if Constant = 'LNRT2PI' then
	    Result := LnRt2Pi;
end;

function TESBMathsLib.HandlesConstant(Constant: string): Boolean;
begin
	Result := (Constant = 'SQRT2') or
		(Constant = 'SQRT3') or
		(Constant = 'SQRT5') or
		(Constant = 'SQRT10') or
		(Constant = 'SQRTPI') or
		(Constant = 'CBRT2') or
		(Constant = 'CBRT3') or
		(Constant = 'CBRT10') or
		(Constant = 'CBRT100') or
		(Constant = 'CBRTPI') or
		(Constant = 'INVSQRT2') or
		(Constant = 'INVSQRT3') or
		(Constant = 'INVSQRT5') or
		(Constant = 'INVSQRTPI') or
		(Constant = 'INVCBRTPI') or
		(Constant = 'ESBE') or
		(Constant = 'ESBE2') or
		(Constant = 'ESBEPI') or
		(Constant = 'ESBEPION2') or
		(Constant = 'ESBEPION4') or
		(Constant = 'LN2') or
		(Constant = 'LN10') or
		(Constant = 'LNPI') or
		(Constant = 'LOG10BASE2') or
		(Constant = 'LOG2BASE10') or
		(Constant = 'LOG3BASE10') or
		(Constant = 'LOGPIBASE10') or
		(Constant = 'LOGEBASE10') or
		(Constant = 'ESBPI') or
		(Constant = 'INVPI') or
		(Constant = 'TWOPI') or
		(Constant = 'THREEPI') or
		(Constant = 'PI2') or
		(Constant = 'PITOE') or
		(Constant = 'PION2') or
		(Constant = 'PION3') or
		(Constant = 'PION4') or
		(Constant = 'THREEPION2') or
		(Constant = 'FOURPION3') or
		(Constant = 'TWOTO63') or
		(Constant = 'ONERADIAN') or
		(Constant = 'ONEDEGREE') or
		(Constant = 'ONEMINUTE') or
		(Constant = 'ONESECOND') or
		(Constant = 'ESBGAMMA') or
		(Constant = 'LNRT2PI');
end;

function GCDFunction (Params: TParamList; var ErrType, ErrParam: Integer): LongWord;
var
	X, Y: LongWord;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= 0) and (Params.Items [0] <= 1.0 * High (Longword)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if (Params.Items [1] >= 0) and (Params.Items [1] <= 1.0 * High (Longword)) then
		Y := Round (Params.Items [1])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	Result := GCD (X, Y);
end;

{$IFDEF D4andAbove}
function LCMFunction (Params: TParamList; var ErrType, ErrParam: Integer): Int64;
{$ELSE}
function LCMFunction (Params: TParamList; var ErrType, ErrParam: Integer): LongInt;
{$ENDIF}
var
	X, Y: LongInt;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= Low (Longint)) and (Params.Items [0] <= 1.0 * High (LongInt)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if (Params.Items [1] >= Low (Longint)) and (Params.Items [1] <= 1.0 * High (LongInt)) then
		Y := Round (Params.Items [1])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	Result := LCM (X, Y);
end;

function RelPrimeFunction (Params: TParamList; var ErrType, ErrParam: Integer): Boolean;
var
	X, Y: LongWord;
begin
	Result := False;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= 0) and (Params.Items [0] <= 1.0 * High (Longword)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if (Params.Items [1] >= 0) and (Params.Items [1] <= 1.0 * High (Longword)) then
		Y := Round (Params.Items [1])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	Result := RelativePrime (X, Y);
end;

function DistanceFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 4 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := Distance (Params.Items [0], Params.Items [1], Params.Items [2],
		Params.Items [3]);
end;

function ExtModFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if FloatIsZero (Params.Items [2]) then
	begin
		ErrType := Error_DivisionByZero;
		ErrParam := 1;
		Exit;
	end;

	Result := ExtMod (Params.Items [0], Params.Items [1]);
end;

function ExtRemFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if FloatIsZero (Params.Items [2]) then
	begin
		ErrType := Error_DivisionByZero;
		ErrParam := 1;
		Exit;
	end;

	Result := ExtRem (Params.Items [0], Params.Items [1]);
end;

function ESBFactFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
var
	X: LongWord;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= 0) and (Params.Items [0] <= 1.0 * High (Longword)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	Result := FactorialX (X);
end;

function ESBPermFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
var
	X, Y: LongWord;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= 0) and (Params.Items [0] <= 1.0 * High (Longword)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if (Params.Items [1] >= 0) and (Params.Items [1] <= 1.0 * X) then
		Y := Round (Params.Items [1])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	Result := PermutationX (X, Y);
end;

function ESBCombFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
var
	X, Y: LongWord;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] >= 0) and (Params.Items [0] <= 1.0 * High (Longword)) then
		X := Round (Params.Items [0])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if (Params.Items [1] >= 0) and (Params.Items [1] <= 1.0 * X) then
		Y := Round (Params.Items [1])
	else
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	Result := BinomialCoeff (X, Y);
end;

function ESBTanFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if Abs (Params.Items [0]) >= TwoToPower63 then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	Result := ESBTan (Params.Items [0]);
end;

function ESBCotFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if Abs (Params.Items [0]) >= TwoToPower63 then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	Result := ESBCot (Params.Items [0]);
end;

function ESBArcTanFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := ESBArcTan (Params.Items [0], Params.Items [1]);
end;

function ESBArcCosFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := ESBArcCos (Params.Items [0]);
end;

function ESBArcSinFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := ESBArcSin (Params.Items [0]);
end;

function ESBGammaFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := Gamma (Params.Items [0]);
end;

function ESBInvGammaFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := InverseGamma (Params.Items [0]);
end;

function ESBLnGammaFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 1 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if FloatIsZero (Params.Items [0]) or (Params.Items [0] < 0) then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;

	Result := LnGamma (Params.Items [0]);
end;

function ESBBetaFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 2 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	Result := Beta (Params.Items [0], Params.Items [1]);
end;

function ESBInCompBetaFunction (Params: TParamList; var ErrType, ErrParam: Integer): Double;
begin
	Result := 0;
	if Params.Count < 3 then
	begin
		ErrType := Error_InvalidNrOfParams;
		Exit;
	end;
	if (Params.Items [0] < 0) or (Params.Items [0] > 1) then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 0;
		Exit;
	end;
	if FloatIsZero (Params.Items [1]) or (Params.Items [1] < 0) then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 1;
		Exit;
	end;
	if FloatIsZero (Params.Items [2]) or (Params.Items [2] < 0) then
	begin
		ErrType := Error_InvalidValue;
		ErrParam := 2;
		Exit;
	end;
	Result := InCompleteBeta (Params.Items [0], Params.Items [1], Params.Items [2]);
end;

function TESBMathsLib.CalcFunction (FuncName: string; Params: TParamList;
	var ErrType,ErrParam: Integer): Double;
begin
	Result := 0.0;
	ErrType := 0;

	if FuncName = 'GCD' then
		Result := GCDFunction (Params, ErrType, ErrParam)
	else if FuncName = 'LCM' then
		Result := LCMFunction (Params, ErrType, ErrParam)
	else if FuncName = 'RELPRIME' then
	begin
		if RelPrimeFunction (Params, ErrType, ErrParam) then
			Result := 1.0
	end
	else if FuncName = 'DISTANCE' then
		Result := DistanceFunction (Params, ErrType, ErrParam)
	else if FuncName = 'EXTMOD' then
		Result := ExtModFunction (Params, ErrType, ErrParam)
	else if FuncName = 'EXTREM' then
		Result := ExtRemFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBFACT' then
		Result := ESBFactFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBPERM' then
		Result := ESBPermFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBCOMB' then
		Result := ESBCombFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBTAN' then
		Result := ESBTanFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBCOT' then
		Result := ESBCotFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBARCTAN' then
		Result := ESBArcTanFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBARCSIN' then
		Result := ESBArcSinFunction (Params, ErrType, ErrParam)
	else if FuncName = 'ESBARCCOS' then
		Result := ESBArcCosFunction (Params, ErrType, ErrParam)
	else if FuncName = 'GAMMA' then
		Result := ESBGammaFunction (Params, ErrType, ErrParam)
	else if FuncName = 'INVGAMMA' then
		Result := ESBInvGammaFunction (Params, ErrType, ErrParam)
	else if FuncName = 'LNGAMMA' then
		Result := ESBLNGammaFunction (Params, ErrType, ErrParam)
	else if FuncName = 'BETA' then
		Result := ESBBetaFunction (Params, ErrType, ErrParam)
	else if FuncName = 'INCOMPBETA' then
		Result := ESBInCompBetaFunction (Params, ErrType, ErrParam)
end;

function TESBMathsLib.HandlesFunction(FuncName: string): boolean;
begin
  Result := (FuncName = 'GCD')
		or (FuncName = 'LCM')
		or (FuncName = 'RELPRIME')
		or (FuncName = 'DISTANCE')
		or (FuncName = 'EXTMOD')
		or (FuncName = 'EXTREM')
		or (FuncName = 'ESBFACT')
		or (FuncName = 'ESBPERM')
		or (FuncName = 'ESBCOMB')
		or (FuncName = 'ESBTAN')
		or (FuncName = 'ESBCOT')
		or (FuncName = 'ESBARCTAN')
		or (FuncName = 'ESBARCSIN')
		or (FuncName = 'ESBARCCOS')
		or (FuncName = 'GAMMA')
		or (FuncName = 'INVGAMMA')
		or (FuncName = 'LNGAMMA')
		or (FuncName = 'BETA')
		or (FuncName = 'INCOMPBETA')
end;

function TESBMathsLib.GetEditHint (FuncName: string; ParamIndex: Integer): string;
begin
	if FuncName = 'GCD' then
		Result := 'GCD(<b>X, Y: LongWord</b>):LongWord<HR>'
			+ 'Returns the Greatest Common Divisor of X and Y<BR>'
			+ '0 &lt;= X &lt;= High (LongWord), X will be rounded if not integral<BR>'
			+ '0 &lt;= Y &lt;= High (LongWord), Y will be rounded if not integral'
	else if FuncName = 'LCM' then
		Result := 'LCM(<b>X, Y: LongWord</b>):INT64<HR>'
			+ 'Returns the Lowest Common Multiplier of X and Y<BR>'
			+ 'Low (LongInt) &lt;= X &lt;= High (LongInt), X will be rounded if not integral<BR>'
			+ 'Low (LongInt) &lt;= Y &lt;= High (LongInt), Y will be rounded if not integral'
	else if FuncName = 'RELPRIME' then
		Result := 'RelPrime(<b>X, Y: LongWord</b>):Boolean<HR>'
			+ 'Returns 1 if X and Y are Relatively Prime, otherwise 0<BR>'
			+ '0 &lt;= X &lt;= High (LongWord), X will be rounded if not integral<BR>'
			+ '0 &lt;= Y &lt;= High (LongWord), Y will be rounded if not integral'
	else if FuncName = 'DISTANCE' then
		Result := 'Distance (<b>X1, Y1, X2, Y2: Double</b>): Double<HR>'
			+ 'Returns the straight line distance between  (X1, Y1) and (X2, Y2)'
	else if FuncName = 'EXTMOD' then
		Result := 'ExtMod (<b>X, Y: double</b>): Double<HR>'
			+ 'Returns Floating Point Modulus<BR>'
			+ 'ExtMod := X - Floor ( X / Y ) * Y<BR>'
			+ 'Y &lt;&gt; 0'
	else if FuncName = 'EXTREM' then
		Result := 'ExtRem (<b>X, Y: double</b>): Double<HR>'
			+ 'Returns Floating Point Remainder<BR>'
			+ 'ExtMod := X - Int ( X / Y ) * Y<BR>'
			+ 'Y &lt;&gt; 0'
	else if FuncName = 'ESBFACT' then
		Result := 'ESBFact (<b>X: LongWord</b>): Double<HR>'
			+ 'Returns Factorial X<BR>'
			+ '0 &lt;= X &lt;= 1754, X will be rounded if not integral'
	else if FuncName = 'ESBPERM' then
		Result := 'ESBPerm (<b>N, R: LongWord</b>): Double<HR>'
			+ 'Returns the number of Permutations of R Objects from N<BR>'
			+ '0 &lt;= N &lt;= 1754, N will be rounded if not integral<BR>'
			+ '0 &lt;= R &lt;= N, R will be rounded if not integral'
	else if FuncName = 'ESBCOMB' then
		Result := 'ESBComb (<b>N, R: LongWord</b>): Double<HR>'
			+ 'Returns the number of Combinations of R Objects from N<BR>'
			+ '0 &lt;= N &lt;= 1754, N will be rounded if not integral<BR>'
			+ '0 &lt;= R &lt;= N, R will be rounded if not integral'
	else if FuncName = 'ESBTAN' then
		Result := 'ESBTan (<b>X: Double</b>): Double<HR>'
			+ 'Returns the Tangent of Angle X, given in Radians<BR>'
			+ 'abs (x) &lt; 2^63'
	else if FuncName = 'ESBCOT' then
		Result := 'ESBCot (<b>X: Double</b>): Double<HR>'
			+ 'Returns the CoTangent of Angle X, given in Radians<BR>'
			+ 'abs (x) &lt; 2^63'
	else if FuncName = 'ESBARCTAN' then
		Result := 'ESBArcTan (<b>X, Y: Double</b>): Double<HR>'
			+ 'ArcTangent of Y / X - Result is in Radians'
	else if FuncName = 'ESBARCSIN' then
		Result := 'ESBArcSin (<b>X: Double</b>): Double<HR>'
			+ 'ArcSine of X - Result is in Radians'
	else if FuncName = 'ESBARCCOS' then
		Result := 'ESBArcCos (<b>X: Double</b>): Double<HR>'
			+ 'ArcCosine of X - Result is in Radians'
	else if FuncName = 'GAMMA' then
		Result := 'Gamma (<b>X: Double</b>): Double<HR>'
			+ 'Returns Gamma (X)'
	else if FuncName = 'INVGAMMA' then
		Result := 'InvGamma (<b>X: Double</b>): Double<HR>'
			+ 'Returns 1 / Gamma (X)'
	else if FuncName = 'LNGAMMA' then
		Result := 'LnGamma (<b>X: Double</b>): Double<HR>'
			+ 'Returns the Natural Logarithm of Gamma (X)'
	else if FuncName = 'BETA' then
		Result := 'Beta (<b>X, Y: Double</b>): Double<HR>'
			+ 'Returns Beta (X, Y)'
	else if FuncName = 'INCOMPBETA' then
		Result := 'InCompBeta (<b>X, P, Q: Double</b>): Double<HR>'
			+ 'Returns Incomplete Beta Ix (P, Q)<BR>'
			+ '0 &lt;= X &lt;= 1, P &gt; 0, Q &gt; 0'
end;


end.
