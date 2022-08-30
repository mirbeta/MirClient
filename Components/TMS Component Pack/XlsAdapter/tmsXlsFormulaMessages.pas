unit tmsXlsFormulaMessages;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses tmsUFlxMessages;
  //Resources on this unit are not localized, to avoid having different
  //interfaces to formulas for different languages.

  //Of course, you are free to translate them to your language,
  //so a user can read or write the formula text the same way
  //he does it on his Excel version. But keep in mind that if you
  //write a formula in your code, you will have to write it in your language,
  //and if you later compile your app on other language, it will not work.

const
  fmiErrNull = $00;
  fmiErrDiv0  = $07;
  fmiErrValue = $0F;
  fmiErrRef   = $17;
  fmiErrName  = $1D;
  fmiErrNum   = $24;
  fmiErrNA    = $2A;

resourcestring
    fmErrNull='#NULL!';
    fmErrDiv0='#DIV/0!';
    fmErrValue='#VALUE!';
    fmErrRef='#REF!';
    fmErrName='#NAME?';
    fmErrNum='#NUM!';
    fmErrNA='#N/A';
    fmErrUnknown='#FLEXCEL_UNKNOWN';

    fmErrStart='#';

    fmTrue='TRUE';
    fmFalse='FALSE';

    fmStartFormula='='; //as in =a1*2 or {=1+2}

    fmFormulaDecimal='.'; //as in '1.2'
    fmFormulaThousands=','; //as in '1,300'

    fmFunctionSep=',';  //Argument separator on a function.
                       //For example, if fmFunctionSep=';' we should
                       //write "Max(a1;a2)"
                       //If you want to localize this, you could use fmFunctionSep=ListSeparator

    fmUnion      =',';  //as in "=a1, b2"
    fmIntersect  =' ';  //as in a1 a2

    fmSingleQuote='''';
    fmOpenArray='{';
    fmCloseArray='}';
    fmOpenParen='(';
    fmCloseParen=')';
    fmArrayRowSep=';';  // Separates 2 rows on an array. It is '\' in spanish
    fmArrayColSep=',';  // Separates 2 columns on an array. Ex: {1,2}. It is ';' in spanish
                        //If you want to localize this, you could use fmArrayColSep=ListSeparator

    fmAbsoluteRef='$'; //as in $A$3
    fmRangeSep=':';   //as in A1:A3

    fmExternalRef='!'; //as in Sheet1!a1
    fmWorkbookOpen='['; //as in c:\[book1.xls]Sheet1!a1
    fmWorkbookClose=']';

    fmTableText='TABLE';

    //those here  shouldnt change
    fmPlus='+';
    fmMinus='-';
    fmMul='*';
    fmDiv='/';
    fmPower='^';
    fmPercent='%';
    const
    fmStr='"';    //this will cause a bug when localizing an app.
    resourcestring

    fmAnd='&';

    fmLT='<';
    fmLE='<=';
    fmEQ='=';
    fmGE='>=';
    fmGT='>';
    fmNE='<>';



    function FmFloatToStr(const Value: Extended): string;
    function FmStrToFloat(const Value:string ): Extended;
const
  attr_bitFSpace=$00;
  attr_bitFEnter=$01;
  attr_bitFPreSpace=$02;
  attr_bitFPreEnter=$03;
  attr_bitFPostSpace=$04;
  attr_bitFPostEnter=$05;
  attr_bitFPreFmlaSpace=$06;
 

type
  TFmReturnType=(fmValue, fmRef, fmArray);
  TCellFunctionData=record
    Index: integer;
    Name: UTF16String;
    MinArgCount: integer;
    MaxArgCount: integer;
    Volatile: boolean;
    ReturnType: TFmReturnType;
    ParamType: UTF16String;
  end;
const
    {$INCLUDE XlsFunctionNames.inc}
    {$INCLUDE XlsPtgs.inc}
implementation

uses SysUtils;

function FmFloatToStr(const Value: Extended): string;
  {
  This is a non-localized version of FloatToStr
  It will always use "." as decimal separator.
  If you are localizing this unit to your language, change this function
  to be:
    function FmFloatToStr(const Value: Extended): string;
    begin
      Result:=FloatToStr(Value);
    end;

  And it will use your current locale to get the decimal separator.
  Just remember that if you for example use "," as decimal sep,
  you should also change fmArrayColSep, fmFunctionSep and all vars with value=","
  }

  //we cant use TFormatSettings as it does not work on D5 or kylix;
begin
{$IFDEF NOFORMATSETTINGS}
  Result:=FloatToStr(Value);
  if ThousandSeparator<>fmFormulaThousands then Result:=StringReplace(Result, ThousandSeparator, '',[rfReplaceAll]);
  if DecimalSeparator<> fmFormulaDecimal then Result:=StringReplace(Result, DecimalSeparator, fmFormulaDecimal,[rfReplaceAll]);
{$ELSE}
  Result:=FloatToStr(Value, InvariantFormatSettings^);
{$ENDIF}
end;

function FmStrToFloat(const Value:string ): Extended;
  {
  This is a non-localized version of StrToFloat
  It will always use "." as decimal separator.
  If you are localizing this unit to your language, change this function
  to be:
    function FmStrToFloat(const Value:string ): Extended;
    begin
      Result:=StrToFloat(Value);
    end;

  And it will use your current locale to get the decimal separator.
  Just remember that if you for example use "," as decimal sep,
  you should also change fmArrayColSep, fmFunctionSep and all vars with value=","
  }

  //we cant use TFormatSettings as it does not work on D5 or kylix;
{$IFDEF NOFORMATSETTINGS}
var
  v: string;
begin
  v:=Value;
  if ThousandSeparator<>fmFormulaThousands then v:=StringReplace(v, fmFormulaThousands, '',[rfReplaceAll]);
  if DecimalSeparator<> fmFormulaDecimal then v:=StringReplace(v, fmFormulaDecimal, DecimalSeparator,[rfReplaceAll]);
  Result:=StrToFloat(v);
end;
{$ELSE}
begin
  Result:=StrtoFloat(Value, InvariantFormatSettings^);
end;
{$ENDIF}

end.
