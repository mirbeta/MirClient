{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxEMF.Utils.CriteriaParser;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, TypInfo, Generics.Defaults, Generics.Collections, Rtti,
  dxCoreClasses,
  dxEMF.Utils,
  dxEMF.Types,
  dxEMF.DB.Criteria;

type

  IdxyyInput = interface
    function Advance: Boolean;
    function Token: Integer;
    function Value: TValue;
  end;

  { TdxCriteriaLexer }

  TdxCriteriaLexer = class(TcxIUnknownObject, IdxyyInput)
  strict private type
{$REGION 'Token''s'}
      TToken = (
        &AND,
        &OR,
        &TRUE,
        &FALSE,
        &NOT,
        &IS,
        NULL,
        ISNULL,
        ISNULLOREMPTY,
        TRIM,
        LEN,
        SUBSTRING,
        UPPERCASE,
        LOWERCASE,
        CUSTOM,
        CUSTOMNONDETERMINISTIC,
        CONCAT,
        IIF, IFTHEN,
        ABS,
        ARCCOS,
        INCDAY,
        INCHOUR,
        INCMILLISECOND,
        INCMINUTE,
        INCMONTH,
        INCSECOND,
        INCTICK,
        ADDTIMESPAN,
        INCYEAR,
        ASCII,
        ARCSIN,
        ARCTAN,
        ARCTAN2,
        BIGMUL,
        CEIL,
        CHAR,
        CHARINDEX, POS,
        COS,
        COSH,
        EXP,
        FLOOR,
        DATEOF,
        DAYOF,
        DAYOFTHEWEEK,
        DAYOFTHEYEAR,
        HOUROF,
        MILLISECONDOF,
        MINUTEOF,
        MONTHOF,
        SECONDOF,
        GETTIMEOFDAY,
        YEAROF,
        DAYSBETWEEN,
        HOURSBETWEEN,
        MILLISECONDSBETWEEN,
        MINUTESBETWEEN,
        MONTHSBETWEEN,
        SECONDSBETWEEN,
        DATEDIFFTICKS,
        YEARSBETWEEN,
        LOG,
        LOG10,
        NOW,
        UTCNOW,
        PADLEFT,
        PADRIGHT,
        POWER,
        REMOVE,
        REPLACE,
        REVERSE,
        RANDOM,
        ROUND,
        SIGN,
        SIN,
        SINH,
        SQR,
        TOSTRING,
        INSERT,
        TAN,
        TANH,
        TODAY,
        TOINTEGER,
        TOINT64,
        TOSINGLE,
        TODOUBLE,
        TODECIMAL,
        STARTSWITH,
        ENDSWITH,
        CONTAINS,
        BETWEEN,
        &IN,
        EXISTS,
        COUNT,
        MIN,
        MAX,
        SINGLE,
        AVG,
        SUM,
        LOCALDATETIMETHISYEAR,
        LOCALDATETIMETHISMONTH,
        LOCALDATETIMELASTWEEK,
        LOCALDATETIMETHISWEEK,
        LOCALDATETIMEYESTERDAY,
        LOCALDATETIMETODAY,
        LOCALDATETIMENOW,
        LOCALDATETIMETOMORROW,
        LOCALDATETIMEDAYAFTERTOMORROW,
        LOCALDATETIMENEXTWEEK,
        LOCALDATETIMETWOWEEKSAWAY,
        LOCALDATETIMENEXTMONTH,
        LOCALDATETIMENEXTYEAR,
        LOCALDATETIMETWOMONTHSAWAY,
        LOCALDATETIMETWOYEARSAWAY,
        LOCALDATETIMELASTMONTH,
        LOCALDATETIMELASTYEAR,
        LOCALDATETIMEYEARBEFORETODAY,
        ISOUTLOOKINTERVALBEYONDTHISYEAR,
        ISOUTLOOKINTERVALLATERTHISYEAR,
        ISOUTLOOKINTERVALLATERTHISMONTH,
        ISOUTLOOKINTERVALNEXTWEEK,
        ISOUTLOOKINTERVALLATERTHISWEEK,
        ISOUTLOOKINTERVALTOMORROW,
        ISOUTLOOKINTERVALTODAY,
        ISOUTLOOKINTERVALYESTERDAY,
        ISOUTLOOKINTERVALEARLIERTHISWEEK,
        ISOUTLOOKINTERVALLASTWEEK,
        ISOUTLOOKINTERVALEARLIERTHISMONTH,
        ISOUTLOOKINTERVALEARLIERTHISYEAR,
        ISOUTLOOKINTERVALPRIORTHISYEAR,
        ISTHISWEEK,
        ISTHISMONTH,
        ISTHISYEAR,
        ISNEXTMONTH,
        ISNEXTYEAR,
        ISLASTMONTH,
        ISLASTYEAR,
        ISYEARTODATE,
        ISSAMEDAY,
        ISJANUARY,
        ISFEBRUARY,
        ISMARCH,
        ISAPRIL,
        ISMAY,
        ISJUNE,
        ISJULY,
        ISAUGUST,
        ISSEPTEMBER,
        ISOCTOBER,
        ISNOVEMBER,
        ISDECEMBER,
        ASC,
        ASCENDING,
        DESC,
        DESCENDING
      );

      TNumericValueToken = (
        m,
        f,
        i,
        s,
        l,
        b,
        u,
        ui,
        iu,
        sb,
        bs,
        us,
        su,
        ul,
        lu
      );
{$ENDREGION}
  strict private class var
    FTokens: TDictionary<string, TToken>;
    FNumericValueTokens: TDictionary<string, TNumericValueToken>;
  strict private
    FInputReader: TTextReader;
    FCurrentToken: Integer;
    FCurrentValue: TdxSmartValue;
    FIsAfterColumn: Boolean;
    FLine: Integer;
    FColumn: Integer;
    FCurrentLine: Integer;
    FCurrentColumn: Integer;
    FPosition: Integer;
    FCurrentTokenPosition: Integer;
    FRecognizeSorting: Boolean;
    FWasChar: Char;
    class function GetToken(const AStr: string; out AToken: TToken): Boolean; static; inline;
    class function GetNumericValueTokens(const AStr: string; out AToken: TNumericValueToken): Boolean; static; inline;
    class procedure PopulateTokens; static;
    class procedure PopulateNumericValueTokens; static;
    class destructor Destroy;
  protected
    class function CanStartColumn(AValue: Char): Boolean; static;
    class function CanContinueColumn(AValue: Char): Boolean; static;
    class function ExtractUserValue(const ATag: string; const AData: string): TObject; static;
    class procedure ToTokenAndValue(const AStr: string; out ACurrentToken: Integer;
      out ACurrentValue: TdxSmartValue; AAllowSorting: Boolean = False); static;

    procedure CatchAll(AFirstChar: Char);
    procedure CheckFunctionArgumentsCount(const AOperator: IdxFunctionOperator);
    procedure DoAtColumn;
    procedure DoConstGuid;
    procedure DoDateTimeConst;
    procedure DoDotOrNumber;
    procedure DoEnclosedColumn;
    procedure DoParam;
    procedure DoString;
    procedure DoNumber(AFirstSymbol: Char);
    procedure DoUserObject;
    function ExtractNumericValue(const AStr: string; const ANumericCode: string): TValue;
    function GetNumericCode: string;

    function ReadNextChar: Integer;
    function PeekNextChar: Char; inline;
    function ReadToLoneSharp: string;
    procedure SkipBlanks;

    // IdxyyInput
    function token: Integer;
    function value: TValue;
    function Advance: Boolean;

    procedure YyError(const AMessage: string; const AArgs: array of const);
  public
    constructor Create(AInputReader: TTextReader);
    class function IsGoodUnescapedName(const AFunctionName: string): Boolean; static;

    property InputReader: TTextReader read FInputReader;
    property CurrentToken: Integer read FCurrentToken;
    property CurrentValue: TdxSmartValue read FCurrentValue;

    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property Position: Integer read FPosition;
    property CurrentTokenPosition: Integer read FCurrentTokenPosition;
    property RecognizeSorting: Boolean read FRecognizeSorting write FRecognizeSorting;
  end;

  { TdxCriteriaParser }

  TdxCriteriaParser = class
  strict private const
{$REGION 'Parser consts'}
      yyLhs: array[0..79] of ShortInt = (-1,
      0,    0,    1,    1,    1,    2,    2,    2,    4,    4,
      4,    5,    6,    6,    6,    7,    7,    8,    8,    8,
      8,    8,    8,   10,    9,    9,    9,    9,    9,    9,
      9,    9,    9,    9,   11,   12,    3,    3,    3,    3,
      3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
      3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
      3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
      3,    3,    3,    3,    3,   13,   13,   14,   14);
      yyLen: array[0..79] of ShortInt = (2,
      1,    2,    1,    3,    3,    1,    2,    2,    1,    3,
      3,    4,    1,    1,    1,    1,    3,    3,    6,    5,
      4,    3,    1,    1,    1,    1,    3,    3,    4,    4,
      3,    4,    2,    2,    3,    3,    1,    1,    1,    1,
      1,    3,    3,    3,    3,    3,    3,    3,    3,    2,
      2,    2,    3,    3,    3,    3,    3,    3,    3,    4,
      2,    3,    3,    3,    3,    4,    3,    7,    4,    6,
      2,    2,    2,    4,    4,    3,    2,    1,    3);
      yyDefRed: array[0 .. 159] of ShortInt = ( 0,
     37,    0,    0,    0,    0,    0,    0,    0,   39,    0,
      0,    0,    0,    0,   38,    0,   15,    0,    0,    0,
      1,    0,    0,    3,    0,   14,   16,    0,   41,   24,
     23,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,   72,    0,   71,   73,    0,    0,    9,    0,    0,
     50,   51,    2,    0,    0,    7,    8,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     33,    0,   34,    0,   28,   27,    0,    0,    0,    0,
     31,    0,   77,    0,    0,    0,   64,    0,    0,    0,
      4,    5,    0,    0,    0,    0,   65,    0,    0,    0,
      0,    0,    0,    0,   67,    0,    0,    0,    0,    0,
      0,   42,   43,   46,   13,   17,   18,    0,    0,    0,
      0,    0,    0,   29,   30,   32,   76,    0,   69,    0,
     10,   12,   11,    0,   66,    0,    0,    0,   74,   75,
      0,    0,    0,   20,    0,   70,    0,   19,   68);
    yyDGoTo  : array[0 .. 14] of ShortInt = (22,
     23,   24,   25,   49,   26,   27,   28,   29,   30,   31,
     32,   33,   42,   95);
    yyFinal = 22;
    yySIndex : array[0 .. 159] of SmallInt = (799,
      0,  -34,  -18,  -14,  -12,   -9,   -5,   -3,    0,   15,
     16,   15, 1303, 1557,    0, -239,    0, 1557, 1557, 1557,
      0,    0,    9,    0,  635,    0,    0,  -41,    0,    0,
      0,  -33,   -8,    8,   25, 1557, 1557, 1557, 1557, 1328,
   1370,    0, 1557,    0,    0,  840, 1260,    0,  -43,   79,
      0,    0,    0, 1557, 1557,    0,    0, 1557, 1557, -237,
   -258, 1557, 1557, 1557, 1557, 1557, 1557, 1557,   15,   51,
   1557, 1557, 1557, 1557, 1557, 1557, 1557, 1557,  257, 1513,
      0, 1557,    0, 1557,    0,    0, 1235, 1235,  854,  885,
      0,  899,    0, 1235,   30,  826,    0, -157, -155, -154,
      0,    0, 1248, 1260, 1557, -160,    0,  221,  221,  221,
   1271, 1271, 1271, 1271,    0, 1557,  -24,  -13,   79,  -35,
    -35,    0,    0,    0,    0,    0,    0,   82,   90,   91,
    914,  928,  945,    0,    0,    0,    0, 1557,    0, 1557,
      0,    0,    0,  221,    0,  959,  -47,   92,    0,    0,
   1235, 1207, 1557,    0,  -47,    0, 1221,    0,    0
    );
    yyRIndex: array[0 .. 159] of SmallInt = (0,
      0,   20,   40,    0,    0,    0,    0,    0,    0,    1,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,   10,    0,    0,   59,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,  194,    0,    0,  437,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,   45,   49,    0,    0,
      0,    0,    0,   64,    0,    0,    0,    0,    0,    0,
      0,    0,  202,  566,    0,    0,    0,   88,  117,  149,
    510,  524,  538,  552,    0,    0,  491,  470,  454,  162,
    415,    0,    0,    0,    0,    0,    0,    0,    0,   98,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,  577,    0,    0,    0,  130,    0,    0,
     66,    0,    0,    0,    0,    0,    0,    0,    0
    );
    yyGIndex: array[0..14] of SmallInt = (0,
      0,   21, 1649,    0,    0,   67,    0,    0,  -75,    0,
    -28,  -27,    4,    0
    );
    yyTable: array[0 .. 1837] of SmallInt = (100,
     13,   78,   98,  127,   79,   34,   76,   81,   53,    6,
     82,   77,   78,   73,  106,   44,  107,   76,   75,   26,
     74,   35,   77,   78,   73,   36,   48,   37,   76,   75,
     38,   74,   83,   77,   39,   84,   40,   13,   13,   25,
    105,   13,   13,   13,   13,   13,   13,   13,   85,   80,
    128,  129,   55,    6,   41,   43,   26,   26,   40,   13,
     26,   26,   26,   26,   26,   86,   26,   54,    6,   72,
    137,  154,  115,  138,  101,  102,   25,   25,   26,  158,
     25,   25,   25,   25,   25,   35,   25,   53,   35,   36,
    116,   13,   36,   13,   13,   40,   40,   22,   25,   40,
     40,   40,   40,   40,   78,   40,   79,   78,  141,   79,
    142,  143,   26,   26,  145,   78,   54,   40,  128,  129,
     76,   75,   81,   74,   13,   77,  128,  129,   53,   21,
     83,   53,   25,   25,   22,   22,  147,  155,   22,   22,
     22,   22,   22,   26,   22,  126,   53,    0,   59,    0,
      0,   40,   40,    0,    0,    0,   22,   54,    0,    0,
     54,   45,    0,   25,    0,    0,   21,   21,    0,    0,
     21,   21,   21,   21,   21,   54,   21,    0,    0,    0,
     53,    0,   40,    0,    0,    0,    0,    0,   21,   59,
     22,   22,   59,   61,    0,    0,    0,    0,    0,   45,
      0,   63,   45,    0,   45,   45,   45,   59,    0,   54,
      2,    3,    4,    5,    6,    7,    8,    0,    0,    0,
     45,   22,   21,   21,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,   61,   99,    0,   61,    0,    0,
      0,   59,   63,    0,    0,   63,    0,    0,    0,    0,
      0,    0,   61,   21,   45,   45,    0,   78,   73,    0,
     63,    0,   76,   75,    0,   74,    0,   77,    0,   13,
     13,   13,   13,   13,   13,    0,   13,   13,   13,   13,
     13,   13,   13,   13,   13,   45,   61,    0,   26,   26,
     26,   26,   26,   26,   63,   26,   26,   26,   26,   26,
     26,   26,   26,   26,    0,    0,    0,    0,   25,   25,
     25,   25,   25,   25,   72,   25,   25,   25,   25,   25,
     25,   25,   25,   25,    0,    0,    0,   40,   40,   40,
     40,   40,   40,    0,   40,   40,   40,   40,   40,   40,
     40,   40,   40,    0,   71,    0,    0,    0,    0,    0,
     17,    0,    0,    0,    0,    0,   53,   53,   53,   53,
     53,   53,    0,   53,   53,   53,   22,   22,   22,   22,
     22,   22,    0,   22,   22,   22,   22,   22,   22,   22,
     22,   22,    0,    0,    0,   54,   54,   54,   54,   54,
     54,    0,   54,   54,   54,    0,    0,    0,   21,   21,
     21,   21,   21,   21,    0,   21,   21,   21,   21,   21,
     21,   21,   21,   21,   44,    0,    0,   59,   59,   59,
     59,   59,   59,    0,   59,   59,   59,    0,    0,    0,
     45,   45,   45,   45,   45,   45,   52,   45,   45,   45,
     45,   45,   45,   45,   45,   45,    0,    0,    0,    0,
      0,    0,   44,   48,    0,   44,    0,   44,   44,   44,
      0,    0,   61,   61,   61,   61,    0,    0,    0,   49,
     63,   63,   63,   44,   52,    0,    0,   52,    0,    0,
     52,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     47,   48,    0,    0,   48,   52,    0,   48,    0,   65,
     66,   67,   68,   69,   70,    0,    0,   44,   44,   55,
     49,    0,   48,   49,    2,    3,    4,    5,    6,    7,
      8,    0,  125,   56,    0,    0,    0,    0,   49,   52,
     52,   47,    0,    0,   47,    0,   16,   57,   44,    0,
      0,    0,    0,    0,    0,    0,   48,   48,    0,   47,
     55,   58,    0,   55,    0,    0,    0,    0,    0,    0,
     52,    0,   49,   49,   56,   62,    0,   56,   55,    0,
      0,    0,    0,    0,    0,    0,   60,   48,   57,    0,
      0,   57,   56,   47,    0,    0,    0,    0,    0,    0,
      0,    0,   58,   49,    0,   58,   57,    0,    0,    0,
      0,    0,   55,    0,    0,    0,   62,    0,    0,   62,
     58,    0,    0,    0,   47,    0,   56,   60,    0,    0,
     60,    0,    0,    0,   62,    0,    0,    0,    0,    0,
     57,    0,    0,    0,    0,   60,    0,    0,    0,    0,
      0,    0,    0,    0,   58,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,   62,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,   60,
      0,   78,   73,    0,    0,    0,   76,   75,    0,   74,
      0,   77,    0,   44,   44,   44,   44,   44,   44,    0,
     44,   44,   44,   44,   44,   44,   44,   44,   44,    0,
      0,    0,    0,    0,    0,   52,   52,   52,   52,   52,
     52,    0,   52,   52,   52,   52,   52,   52,   52,   52,
     52,    0,   48,   48,   48,   48,   48,   48,   72,   48,
     48,   48,   48,   48,   48,   48,   48,   48,   49,   49,
     49,   49,   49,   49,    0,   49,   49,   49,   49,   49,
     49,   49,   49,   49,    0,    0,    0,    0,   71,   47,
     47,   47,   47,   47,   47,    0,   47,   47,   47,   47,
     47,   47,   47,   47,   47,    0,    0,    0,   55,   55,
     55,   55,   55,   55,    0,   55,   55,   55,   55,   55,
     55,   55,   56,   56,   56,   56,   56,   56,   21,   56,
     56,   56,   56,   56,   56,   56,   57,   57,   57,   57,
     57,   57,    0,   57,   57,   57,   57,   57,   57,   57,
     58,   58,   58,   58,   58,   58,    0,   58,   58,   58,
     58,   58,   58,   58,   62,   62,   62,   62,   13,    0,
      0,   20,    0,   19,    0,   60,   60,   60,   60,   60,
     60,    0,   60,   60,   60,    0,    0,    0,    0,    0,
      0,    0,   78,   73,    0,    0,  139,   76,   75,  140,
     74,    0,   77,    0,    0,    0,   78,   73,    0,    0,
     97,   76,   75,    0,   74,    0,   77,    0,    0,    0,
     78,   73,   17,    0,  134,   76,   75,    0,   74,    0,
     77,    0,    0,   56,   57,   58,   59,   60,   61,    0,
     62,   63,   64,   65,   66,   67,   68,   69,   70,   72,
      0,   78,   73,    0,   18,  135,   76,   75,    0,   74,
      0,   77,    0,   72,    0,   78,   73,    0,    0,  136,
     76,   75,    0,   74,    0,   77,    0,   72,    0,   71,
     78,   73,    0,    0,    0,   76,   75,    0,   74,    0,
     77,    0,    0,   71,   78,   73,    0,    0,  149,   76,
     75,    0,   74,    0,   77,    0,    0,   71,   72,    0,
      0,   78,   73,    0,    0,  150,   76,   75,    0,   74,
      0,   77,   72,    0,    0,   78,   73,    0,    0,    0,
     76,   75,  153,   74,    0,   77,  148,   72,   71,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,   72,   71,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,   71,   72,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,   71,   72,    0,    0,    1,    2,    3,    4,    5,
      6,    7,    8,    9,   10,   11,   12,    0,   71,    0,
      0,   14,    0,   15,    0,    0,    0,    0,   16,    0,
      0,    0,   71,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,   58,   59,   60,   61,
      0,   62,   63,   64,   65,   66,   67,   68,   69,   70,
     58,   59,   60,   61,    0,   62,   63,   64,   65,   66,
     67,   68,   69,   70,   58,   59,   60,   61,    0,   62,
     63,   64,   65,   66,   67,   68,   69,   70,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,   58,   59,   60,   61,    0,
     62,   63,   64,   65,   66,   67,   68,   69,   70,   58,
     59,   60,   61,    0,   62,   63,   64,   65,   66,   67,
     68,   69,   70,    0,   58,   59,   60,   61,    0,   62,
     63,   64,   65,   66,   67,   68,   69,   70,   58,   59,
     60,   61,    0,   62,   63,   64,   65,   66,   67,   68,
     69,   70,    0,    0,    0,   58,   59,   60,   61,    0,
     62,   63,   64,   65,   66,   67,   68,   69,   70,   58,
     59,   60,   61,    0,   62,   63,   64,   65,   66,   67,
     68,   69,   70,   78,   73,    0,    0,  156,   76,   75,
      0,   74,    0,   77,    0,    0,    0,   78,   73,    0,
      0,  159,   76,   75,    0,   74,    0,   77,    0,    0,
      0,   78,   73,    0,    0,    0,   76,   75,    0,   74,
      0,   77,    0,    0,   78,   73,    0,    0,    0,   76,
     75,    0,   74,    0,   77,    0,   78,   73,    0,    0,
     72,   76,   75,    0,   74,    0,   77,   78,   73,    0,
      0,    0,   76,   75,   72,   74,    0,   77,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,   72,    0,
     71,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,   72,   13,   45,   71,   20,    0,   19,    0,    0,
      0,    0,    0,   72,    0,    0,    0,    0,   71,    0,
      0,    0,    0,    0,   72,    0,    0,   13,   91,    0,
     20,   71,   19,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,   71,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,   71,    0,   17,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,   13,
     93,    0,   20,    0,   19,    0,    0,    0,    0,    0,
      0,   17,    0,    0,    0,    0,    0,    0,   18,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,   18,    0,    0,    0,    0,    0,    0,
      0,    0,    0,   17,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,   58,   59,   60,
     61,    0,   62,   63,   64,   65,   66,   67,   68,   69,
     70,   58,   59,   60,   61,   18,   62,   63,   64,   65,
     66,   67,   68,   69,   70,   58,   59,   60,   61,    0,
     62,   63,   64,   65,   66,   67,   68,   69,   70,   59,
     60,   61,    0,   62,   63,   64,   65,   66,   67,   68,
     69,   70,   60,   61,    0,   62,   63,   64,   65,   66,
     67,   68,   69,   70,    0,    0,    0,    0,    0,    0,
      0,    0,   13,   69,   70,   20,    0,   19,    0,    1,
      2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
     12,    0,    0,    0,    0,   14,    0,   15,    0,    0,
      0,    0,   16,    0,    1,    2,    3,    4,    5,    6,
      7,    8,    9,   10,   11,   12,   13,    0,    0,   20,
     14,   19,   15,    0,    0,  130,   17,   16,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    1,    2,    3,    4,
      5,    6,    7,    8,    9,   10,   11,   12,   18,    0,
      0,    0,   14,    0,   15,    0,    0,    0,    0,   16,
     17,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,   46,   47,    0,    0,    0,   50,   51,   52,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,   18,    0,   87,   88,   89,   90,   92,   94,
      0,   96,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,  103,  104,    0,    0,
    108,  109,  110,  111,  112,  113,  114,    0,    0,  117,
    118,  119,  120,  121,  122,  123,  124,    0,  131,    0,
    132,    0,  133,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,  144,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    0,  146,    0,    0,    0,    0,    1,
      2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
     12,    0,    0,    0,    0,   14,  151,   15,  152,    0,
      0,    0,   16,    0,    0,    0,    0,    0,    0,    0,
      0,  157,    0,    0,    0,    0,    0,    0,    0,    0,
      0,    0,    0,    1,    2,    3,    4,    5,    6,    7,
      8,    9,   10,   11,   12,    0,    0,    0,    0,   14,
      0,   15,    0,    0,    0,    0,   16
    );
    yyCheck: array[0 .. 1837] of SmallInt = (43,
      0,   37,   46,   79,   46,   40,   42,   41,    0,    0,
     44,   47,   37,   38,  273,   12,  275,   42,   43,    0,
     45,   40,   47,   37,   38,   40,  266,   40,   42,   43,
     40,   45,   41,   47,   40,   44,   40,   37,   38,    0,
    278,   41,   42,   43,   44,   45,   46,   47,   41,   91,
     79,   79,   44,   44,   40,   40,   37,   38,    0,   59,
     41,   42,   43,   44,   45,   41,   47,   59,   59,   94,
     41,  147,   69,   44,   54,   55,   37,   38,   59,  155,
     41,   42,   43,   44,   45,   41,   47,    0,   44,   41,
     40,   91,   44,   93,   94,   37,   38,    0,   59,   41,
     42,   43,   44,   45,   41,   47,   41,   44,  266,   44,
    266,  266,   93,   94,  275,   37,    0,   59,  147,  147,
     42,   43,   41,   45,  124,   47,  155,  155,   41,    0,
     41,   44,   93,   94,   37,   38,   46,   46,   41,   42,
     43,   44,   45,  124,   47,   79,   59,   -1,    0,   -1,
     -1,   93,   94,   -1,   -1,   -1,   59,   41,   -1,   -1,
     44,    0,   -1,  124,   -1,   -1,   37,   38,   -1,   -1,
     41,   42,   43,   44,   45,   59,   47,   -1,   -1,   -1,
     93,   -1,  124,   -1,   -1,   -1,   -1,   -1,   59,   41,
     93,   94,   44,    0,   -1,   -1,   -1,   -1,   -1,   38,
     -1,    0,   41,   -1,   43,   44,   45,   59,   -1,   93,
    258,  259,  260,  261,  262,  263,  264,   -1,   -1,   -1,
     59,  124,   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   41,  279,   -1,   44,   -1,   -1,
     -1,   93,   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,
     -1,   -1,   59,  124,   93,   94,   -1,   37,   38,   -1,
     59,   -1,   42,   43,   -1,   45,   -1,   47,   -1,  269,
    270,  271,  272,  273,  274,   -1,  276,  277,  278,  279,
    280,  281,  282,  283,  284,  124,   93,   -1,  269,  270,
    271,  272,  273,  274,   93,  276,  277,  278,  279,  280,
    281,  282,  283,  284,   -1,   -1,   -1,   -1,  269,  270,
    271,  272,  273,  274,   94,  276,  277,  278,  279,  280,
    281,  282,  283,  284,   -1,   -1,   -1,  269,  270,  271,
    272,  273,  274,   -1,  276,  277,  278,  279,  280,  281,
    282,  283,  284,   -1,  124,   -1,   -1,   -1,   -1,   -1,
     94,   -1,   -1,   -1,   -1,   -1,  269,  270,  271,  272,
    273,  274,   -1,  276,  277,  278,  269,  270,  271,  272,
    273,  274,   -1,  276,  277,  278,  279,  280,  281,  282,
    283,  284,   -1,   -1,   -1,  269,  270,  271,  272,  273,
    274,   -1,  276,  277,  278,   -1,   -1,   -1,  269,  270,
    271,  272,  273,  274,   -1,  276,  277,  278,  279,  280,
    281,  282,  283,  284,    0,   -1,   -1,  269,  270,  271,
    272,  273,  274,   -1,  276,  277,  278,   -1,   -1,   -1,
    269,  270,  271,  272,  273,  274,    0,  276,  277,  278,
    279,  280,  281,  282,  283,  284,   -1,   -1,   -1,   -1,
     -1,   -1,   38,    0,   -1,   41,   -1,   43,   44,   45,
     -1,   -1,  269,  270,  271,  272,   -1,   -1,   -1,    0,
    269,  270,  271,   59,   38,   -1,   -1,   41,   -1,   -1,
     44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
      0,   38,   -1,   -1,   41,   59,   -1,   44,   -1,  279,
    280,  281,  282,  283,  284,   -1,   -1,   93,   94,    0,
     41,   -1,   59,   44,  258,  259,  260,  261,  262,  263,
    264,   -1,  266,    0,   -1,   -1,   -1,   -1,   59,   93,
     94,   41,   -1,   -1,   44,   -1,  280,    0,  124,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   93,   94,   -1,   59,
     41,    0,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,
    124,   -1,   93,   94,   41,    0,   -1,   44,   59,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,    0,  124,   41,   -1,
     -1,   44,   59,   93,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   41,  124,   -1,   44,   59,   -1,   -1,   -1,
     -1,   -1,   93,   -1,   -1,   -1,   41,   -1,   -1,   44,
     59,   -1,   -1,   -1,  124,   -1,   93,   41,   -1,   -1,
     44,   -1,   -1,   -1,   59,   -1,   -1,   -1,   -1,   -1,
     93,   -1,   -1,   -1,   -1,   59,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,
     -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,
     -1,   47,   -1,  269,  270,  271,  272,  273,  274,   -1,
    276,  277,  278,  279,  280,  281,  282,  283,  284,   -1,
     -1,   -1,   -1,   -1,   -1,  269,  270,  271,  272,  273,
    274,   -1,  276,  277,  278,  279,  280,  281,  282,  283,
    284,   -1,  269,  270,  271,  272,  273,  274,   94,  276,
    277,  278,  279,  280,  281,  282,  283,  284,  269,  270,
    271,  272,  273,  274,   -1,  276,  277,  278,  279,  280,
    281,  282,  283,  284,   -1,   -1,   -1,   -1,  124,  269,
    270,  271,  272,  273,  274,   -1,  276,  277,  278,  279,
    280,  281,  282,  283,  284,   -1,   -1,   -1,  269,  270,
    271,  272,  273,  274,   -1,  276,  277,  278,  279,  280,
    281,  282,  269,  270,  271,  272,  273,  274,    0,  276,
    277,  278,  279,  280,  281,  282,  269,  270,  271,  272,
    273,  274,   -1,  276,  277,  278,  279,  280,  281,  282,
    269,  270,  271,  272,  273,  274,   -1,  276,  277,  278,
    279,  280,  281,  282,  269,  270,  271,  272,   40,   -1,
     -1,   43,   -1,   45,   -1,  269,  270,  271,  272,  273,
    274,   -1,  276,  277,  278,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,
     45,   -1,   47,   -1,   -1,   -1,   37,   38,   -1,   -1,
     41,   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,
     37,   38,   94,   -1,   41,   42,   43,   -1,   45,   -1,
     47,   -1,   -1,  269,  270,  271,  272,  273,  274,   -1,
    276,  277,  278,  279,  280,  281,  282,  283,  284,   94,
     -1,   37,   38,   -1,  126,   41,   42,   43,   -1,   45,
     -1,   47,   -1,   94,   -1,   37,   38,   -1,   -1,   41,
     42,   43,   -1,   45,   -1,   47,   -1,   94,   -1,  124,
     37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
     47,   -1,   -1,  124,   37,   38,   -1,   -1,   41,   42,
     43,   -1,   45,   -1,   47,   -1,   -1,  124,   94,   -1,
     -1,   37,   38,   -1,   -1,   41,   42,   43,   -1,   45,
     -1,   47,   94,   -1,   -1,   37,   38,   -1,   -1,   -1,
     42,   43,   44,   45,   -1,   47,   93,   94,  124,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   94,  124,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   94,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,  124,   94,   -1,   -1,  257,  258,  259,  260,  261,
    262,  263,  264,  265,  266,  267,  268,   -1,  124,   -1,
     -1,  273,   -1,  275,   -1,   -1,   -1,   -1,  280,   -1,
     -1,   -1,  124,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,  271,  272,  273,  274,
     -1,  276,  277,  278,  279,  280,  281,  282,  283,  284,
    271,  272,  273,  274,   -1,  276,  277,  278,  279,  280,
    281,  282,  283,  284,  271,  272,  273,  274,   -1,  276,
    277,  278,  279,  280,  281,  282,  283,  284,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,  271,  272,  273,  274,   -1,
    276,  277,  278,  279,  280,  281,  282,  283,  284,  271,
    272,  273,  274,   -1,  276,  277,  278,  279,  280,  281,
    282,  283,  284,   -1,  271,  272,  273,  274,   -1,  276,
    277,  278,  279,  280,  281,  282,  283,  284,  271,  272,
    273,  274,   -1,  276,  277,  278,  279,  280,  281,  282,
    283,  284,   -1,   -1,   -1,  271,  272,  273,  274,   -1,
    276,  277,  278,  279,  280,  281,  282,  283,  284,  271,
    272,  273,  274,   -1,  276,  277,  278,  279,  280,  281,
    282,  283,  284,   37,   38,   -1,   -1,   41,   42,   43,
     -1,   45,   -1,   47,   -1,   -1,   -1,   37,   38,   -1,
     -1,   41,   42,   43,   -1,   45,   -1,   47,   -1,   -1,
     -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,
     -1,   47,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,
     43,   -1,   45,   -1,   47,   -1,   37,   38,   -1,   -1,
     94,   42,   43,   -1,   45,   -1,   47,   37,   38,   -1,
     -1,   -1,   42,   43,   94,   45,   -1,   47,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   94,   -1,
    124,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   94,   40,   41,  124,   43,   -1,   45,   -1,   -1,
     -1,   -1,   -1,   94,   -1,   -1,   -1,   -1,  124,   -1,
     -1,   -1,   -1,   -1,   94,   -1,   -1,   40,   41,   -1,
     43,  124,   45,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,  124,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,  124,   -1,   94,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   40,
     41,   -1,   43,   -1,   45,   -1,   -1,   -1,   -1,   -1,
     -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,  271,  272,  273,
    274,   -1,  276,  277,  278,  279,  280,  281,  282,  283,
    284,  271,  272,  273,  274,  126,  276,  277,  278,  279,
    280,  281,  282,  283,  284,  271,  272,  273,  274,   -1,
    276,  277,  278,  279,  280,  281,  282,  283,  284,  272,
    273,  274,   -1,  276,  277,  278,  279,  280,  281,  282,
    283,  284,  273,  274,   -1,  276,  277,  278,  279,  280,
    281,  282,  283,  284,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   40,  283,  284,   43,   -1,   45,   -1,  257,
    258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
    268,   -1,   -1,   -1,   -1,  273,   -1,  275,   -1,   -1,
     -1,   -1,  280,   -1,  257,  258,  259,  260,  261,  262,
    263,  264,  265,  266,  267,  268,   40,   -1,   -1,   43,
    273,   45,  275,   -1,   -1,   93,   94,  280,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,  257,  258,  259,  260,
    261,  262,  263,  264,  265,  266,  267,  268,  126,   -1,
     -1,   -1,  273,   -1,  275,   -1,   -1,   -1,   -1,  280,
     94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   13,   14,   -1,   -1,   -1,   18,   19,   20,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,  126,   -1,   36,   37,   38,   39,   40,   41,
     -1,   43,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   -1,   -1,
     62,   63,   64,   65,   66,   67,   68,   -1,   -1,   71,
     72,   73,   74,   75,   76,   77,   78,   -1,   80,   -1,
     82,   -1,   84,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,  105,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,   -1,  116,   -1,   -1,   -1,   -1,  257,
    258,  259,  260,  261,  262,  263,  264,  265,  266,  267,
    268,   -1,   -1,   -1,   -1,  273,  138,  275,  140,   -1,
     -1,   -1,  280,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,  153,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     -1,   -1,   -1,  257,  258,  259,  260,  261,  262,  263,
    264,  265,  266,  267,  268,   -1,   -1,   -1,   -1,  273,
     -1,  275,   -1,   -1,   -1,   -1,  280
    );
{$ENDREGION}
  strict private
    FResult: TArray<IdxCriteriaOperator>;
    FResultParameters: TList<IdxOperandValue>;
    FYyMax: Integer;
    FLexer: TdxCriteriaLexer;
    procedure FreeResults;
  protected
    procedure ClearResults;
    function YyParse(const AYyLex: IdxyyInput): TValue;
    procedure YyError(const AMessage: string); overload;
    procedure YyError(const AMessage: string; const AExpected: TArray<string>); overload;

    class function JoinOrAggregate(const ACollectionProperty: IdxOperandProperty; const ACondition: IdxCriteriaOperator;
      AType: TdxAggregateFunctionType; const AAggregated: IdxCriteriaOperator): IdxCriteriaOperator; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const AQuery: string; AAllowSort: Boolean = False); overload;
    class function Parse(const AStringCriteria: string; out ACriteriaParametersList: TArray<IdxOperandValue>): IdxCriteriaOperator; overload; static;
    class function ParseList(const ACriteriaList: string; out ACriteriaParametersList: TArray<IdxOperandValue>; AAllowSorting: Boolean): TArray<IdxCriteriaOperator>; static;

    property Result: TArray<IdxCriteriaOperator> read FResult;
    property ResultParameters: TList<IdxOperandValue> read FResultParameters;
  end;

  { TdxToken }

  TdxToken = class sealed
  public const
    &CONST = 257;
    AGG_EXISTS = 258;
    AGG_COUNT = 259;
    AGG_MIN = 260;
    AGG_MAX = 261;
    AGG_AVG = 262;
    AGG_SUM = 263;
    AGG_SINGLE = 264;
    PARAM = 265;
    COL = 266;
    FN_ISNULL = 267;
    &FUNCTION = 268;
    SORT_ASC = 269;
    SORT_DESC = 270;
    &OR = 271;
    &AND = 272;
    &NOT = 273;
    &IS = 274;
    NULL = 275;
    OP_EQ = 276;
    OP_NE = 277;
    OP_LIKE = 278;
    OP_GT = 279;
    OP_LT = 280;
    OP_GE = 281;
    OP_LE = 282;
    OP_IN = 283;
    OP_BETWEEN = 284;
    NEG = 285;
    YyErrorCode = 256;
  end;

implementation

uses
  Character, TimeSpan,
  dxCore, dxTypeHelpers, dxStringHelper,
  dxEMF.Utils.Exceptions,
  dxEMF.Utils.Expressions,
  dxEMF.Strs;

{ TdxCriteriaLexer }

constructor TdxCriteriaLexer.Create(AInputReader: TTextReader);
begin
  inherited Create;
  FLine := -1;
  FColumn := -1;
  FCurrentTokenPosition := -1;
  FInputReader := AInputReader;
end;

class destructor TdxCriteriaLexer.Destroy;
begin
  FreeAndNil(FTokens);
  FreeAndNil(FNumericValueTokens);
end;

function TdxCriteriaLexer.PeekNextChar: Char;
begin
  Result := Char(InputReader.Peek);
end;

class procedure TdxCriteriaLexer.PopulateNumericValueTokens;
var
  I: TNumericValueToken;
begin
  for I := Low(TNumericValueToken) to High(TNumericValueToken) do
    FNumericValueTokens.Add(GetEnumName(TypeInfo(TNumericValueToken), Integer(I)), I);
end;

class procedure TdxCriteriaLexer.PopulateTokens;
var
  I: TToken;
begin
  for I := Low(TToken) to High(TToken) do
    FTokens.Add(GetEnumName(TypeInfo(TToken), Integer(I)), I);
end;

function TdxCriteriaLexer.token: Integer;
begin
  Result := CurrentToken;
end;

function TdxCriteriaLexer.value: TValue;
begin
  Result := CurrentValue;
end;

function TdxCriteriaLexer.Advance: Boolean;
var
  ANextInt: Integer;
  ANextChar: Char;
begin
  SkipBlanks;
  FLine := FCurrentLine;
  FColumn := FCurrentColumn;
  FCurrentTokenPosition := FPosition;
  FCurrentToken := 0;
  FCurrentValue := nil;
  ANextInt := ReadNextChar;
  if ANextInt = -1 then
    Exit(False);
  ANextChar := Char(ANextInt);
  case ANextChar of
    '?', ':':
      DoParam;
    '^',
    '+',
    '*',
    '/',
    '%',
    '(',
    ')',
    ']',
    ',',
    '~',
    '-',
    ';':
      FCurrentToken := Integer(ANextChar);
    '.':
      DoDotOrNumber;
    '0'..'9':
      DoNumber(ANextChar);
    '!':
      if PeekNextChar = '=' then
      begin
        ReadNextChar;
        FCurrentToken := TdxToken.OP_NE;
      end
      else
        FCurrentToken := TdxToken.NOT;
    '=':
      begin
        FCurrentToken := TdxToken.OP_EQ;
        if PeekNextChar = '=' then
          ReadNextChar;
      end;
    '<':
      if PeekNextChar = '>' then
      begin
        ReadNextChar;
        FCurrentToken := TdxToken.OP_NE;
      end
      else
        if PeekNextChar = '=' then
        begin
          ReadNextChar;
          FCurrentToken := TdxToken.OP_LE;
        end
        else
          FCurrentToken := TdxToken.OP_LT;
    '>':
      if PeekNextChar = '=' then
      begin
        ReadNextChar;
        FCurrentToken := TdxToken.OP_GE;
      end
      else
        FCurrentToken := TdxToken.OP_GT;
    '|':
      if PeekNextChar = '|' then
      begin
        ReadNextChar;
        FCurrentToken := TdxToken.OR;
      end
      else
        FCurrentToken := Integer(ANextChar);
    '&':
      if PeekNextChar = '&' then
      begin
        ReadNextChar;
        FCurrentToken := TdxToken.AND;
      end
      else
        FCurrentToken := Integer(ANextChar);
    '[':
      if FIsAfterColumn then
        FCurrentToken := Integer(ANextChar)
      else
        DoEnclosedColumn;
    '{':
      DoConstGuid;
    #$27:
      DoString;
    '@':
      DoAtColumn;
    '#':
      if PeekNextChar = '#' then
      begin
        ReadNextChar;
        DoUserObject;
      end
      else
        DoDateTimeConst;
    else
      CatchAll(ANextChar);
  end;
  FIsAfterColumn := CurrentToken = TdxToken.COL;
  Result := True;
end;

function TdxCriteriaLexer.ReadNextChar: Integer;
var
  ANextInt: Integer;
  ANextIntChar: Char absolute ANextInt;
begin
  ANextInt := InputReader.Read;
  if ANextInt = -1 then
    FWasChar := #0
  else
    if ANextIntChar = #10 then
    begin
      if FWasChar = #13 then
      begin
        FWasChar := #0;
        Inc(FPosition);
      end
      else
      begin
        FWasChar := #10;
        Inc(FPosition);
        Inc(FCurrentLine);
        FCurrentColumn := 0;
      end;
    end
    else
      if ANextIntChar = #13 then
      begin
        if FWasChar = #10 then
        begin
          FWasChar := #0;
          Inc(FPosition);
        end
        else
        begin
          FWasChar := #13;
          Inc(FPosition);
          Inc(FCurrentLine);
          FCurrentColumn := 0;
        end;
      end
      else
      begin
        Inc(FPosition);
        Inc(FCurrentColumn);
      end;
  Result := ANextInt;
end;

procedure TdxCriteriaLexer.SkipBlanks;
var
  APeeked: Integer;
  APeekedCategory: TUnicodeCategory;
begin
  while True do
  begin
    APeeked := Integer(PeekNextChar);

    APeekedCategory := {$IFDEF DELPHIXE4}Char(APeeked).GetUnicodeCategory{$ELSE}GetUnicodeCategory(Char(APeeked)){$ENDIF};
    if (APeekedCategory <> TUnicodeCategory.ucSpaceSeparator) and (APeekedCategory <> TUnicodeCategory.ucControl) then
      Exit;
    ReadNextChar;
  end;
end;

procedure TdxCriteriaLexer.DoAtColumn;
var
  AColumnName: TStringBuilder;
begin
  AColumnName := TStringBuilder.Create;
  try
    while True do
    begin
      if CanContinueColumn(Char(PeekNextChar)) then
        AColumnName.Append(Char(ReadNextChar))
      else
        Break;
    end;
    FCurrentToken := TdxToken.COL;
    FCurrentValue := TdxOperandProperty.Create(AColumnName.ToString);
  finally
    AColumnName.Free;
  end;
end;

procedure TdxCriteriaLexer.DoParam;
var
  AParamName: TStringBuilder;
begin
  AParamName := TStringBuilder.Create;
  try
    while True do
    begin
      if CanContinueColumn(Char(PeekNextChar)) then
        AParamName.Append(Char(ReadNextChar))
      else
        Break;
    end;
    FCurrentToken := TdxToken.PARAM;
    FCurrentValue := AParamName.ToString;
  finally
    AParamName.Free;
  end;
end;

procedure TdxCriteriaLexer.DoEnclosedColumn;
var
  AName: TStringBuilder;
  ANextInt: Integer;
  ANextChar: Char;
begin
  AName := TStringBuilder.Create;
  try
    FCurrentToken := TdxToken.COL;
    try
      while True do
      begin
        ANextInt := ReadNextChar;
        if ANextInt = -1 then
        begin
          YyError(sdxFilteringExceptionsTextLexerNonClosedElement, [sdxFilteringExceptionsTextLexerElementPropertyName, ']']);
          Exit;
        end;
        ANextChar := Char(ANextInt);
        if ANextChar = ']' then
          Exit;
        if ANextChar = '\' then
        begin
          ANextInt := ReadNextChar;
          if ANextInt = -1 then
          begin
            YyError(sdxFilteringExceptionsTextLexerNonClosedElement, [sdxFilteringExceptionsTextLexerElementPropertyName, ']']);
            Exit;
          end;
          ANextChar := Char(ANextInt);
          case ANextChar of
            'n':
              AName.Append(#10);
            'r':
              AName.Append(#13);
            't':
              AName.Append(#9);
            else
              AName.Append(ANextChar);
          end;
        end
        else
          AName.Append(ANextChar);
      end;
    finally
      FCurrentValue := TdxOperandProperty.Create(AName.ToString);
    end;
  finally
    AName.Free;
  end;
end;

procedure TdxCriteriaLexer.DoString;
var
  AStr: TStringBuilder;
  ANextInt: Integer;
  ANextChar, APossibleSuffix: Char;
begin
  FCurrentToken := TdxToken.CONST;
  AStr := TStringBuilder.Create;
  try
    while True do
    begin
      ANextInt := ReadNextChar;
      if ANextInt = -1 then
      begin
        FCurrentValue := TdxConstantValue.Create(AStr.ToString);
        YyError(sdxFilteringExceptionsTextLexerNonClosedElement, [sdxFilteringExceptionsTextLexerElementStringLiteral, #$27]);
        Exit;
      end;
      ANextChar := Char(ANextInt);
      if ANextChar = #$27 then
      begin
        if PeekNextChar <> #$27 then
        begin
          FCurrentValue := TdxConstantValue.Create(AStr.ToString);
          if AStr.Length = 1 then
          begin
            APossibleSuffix := PeekNextChar;
            if (APossibleSuffix = 'c') or (APossibleSuffix = 'C') then
            begin
              ReadNextChar;
              FCurrentValue := TdxConstantValue.Create(AStr.Chars[0]);
            end;
          end;
          Exit;
        end;
        ReadNextChar;
      end;
      AStr.Append(ANextChar);
    end;
  finally
    AStr.Free;
  end;
end;

function TdxCriteriaLexer.ReadToLoneSharp: string;
var
  AStr: TStringBuilder;
  ANextInt: Integer;
  ANextChar, APeek: Char;
begin
  AStr := TStringBuilder.Create;
  try
    while True do
    begin
      ANextInt := ReadNextChar;
      if ANextInt = -1 then
      begin
        FCurrentValue := TdxConstantValue.Create(AStr);
        YyError(sdxFilteringExceptionsTextLexerNonClosedElement, [sdxFilteringExceptionsTextLexerElementDateTimeOrUserTypeLiteral, '#']);
        Exit(AStr.ToString);
      end;
      ANextChar := Char(ANextInt);
      if ANextChar = '#' then
      begin
        APeek := PeekNextChar;
        if APeek = '#' then
          ReadNextChar
        else
          Break;
      end;
      AStr.Append(ANextChar);
    end;
    Result := AStr.ToString;
  finally
    AStr.Free;
  end;
end;

procedure TdxCriteriaLexer.DoUserObject;
var
  ATag, AData: string;
begin
  FCurrentToken := TdxToken.CONST;
  FCurrentValue := TdxConstantValue.Create;
  ATag := ReadToLoneSharp;
  AData := ReadToLoneSharp;
  try
    FCurrentValue := TdxConstantValue.Create(ExtractUserValue(ATag, AData));
  except
    on E: Exception do
    begin
      {$IFDEF DELPHIXE3}
      FCurrentValue := TdxConstantValue.Create(ATag.Replace('#', '##') + '#' + AData.Replace('#', '##'));
      {$ELSE}
      FCurrentValue := TdxConstantValue.Create(TdxStringHelper.Replace(ATag, '#', '##') + '#' + TdxStringHelper.Replace(AData, '#', '##'));
      {$ENDIF}
      YyError(sdxFilteringExceptionsTextLexerCantRestoreUserObject, [ATag, AData, E.Message]);
    end
  end;
end;

class function TdxCriteriaLexer.ExtractUserValue(const ATag: string; const AData: string): TObject;
begin
  Result := NotImplemented;
end;

class function TdxCriteriaLexer.GetToken(const AStr: string; out AToken: TToken): Boolean;
begin
  if FTokens = nil then
  begin
    FTokens := TDictionary<string, TToken>.Create;
    PopulateTokens;
  end;
  Result := FTokens.TryGetValue(UPPERCASE(AStr), AToken);
end;

class function TdxCriteriaLexer.GetNumericValueTokens(const AStr: string; out AToken: TNumericValueToken): Boolean;
begin
  if FNumericValueTokens = nil then
  begin
    FNumericValueTokens := TDictionary<string, TNumericValueToken>.Create;
    PopulateNumericValueTokens;
  end;
  Result := FNumericValueTokens.TryGetValue(LOWERCASE(AStr), AToken);
end;

procedure TdxCriteriaLexer.DoDateTimeConst;
var
  AStr: string;
  ATs: TTimeSpan;
begin
  FCurrentToken := TdxToken.CONST;
  AStr := ReadToLoneSharp;
  if TTimeSpan.TryParse(AStr, ATs) then
  begin
    FCurrentValue := TdxConstantValue.Create(ATs);
    Exit;
  end;
  try
    FCurrentValue := TdxConstantValue.Create(StrToDateTime(AStr));
    Exit;
  except
  end;
  FCurrentValue := TdxConstantValue.Create(AStr);
  YyError(sdxFilteringExceptionsTextLexerInvalidElement, [sdxFilteringExceptionsTextLexerElementDateTimeLiteral, AStr]);
end;

procedure TdxCriteriaLexer.DoConstGuid;
var
  AStr: TStringBuilder;
  S: string;
  ANextInt: Integer;
  ANextChar: Char;
begin
  FCurrentToken := TdxToken.CONST;
  AStr := TStringBuilder.Create;
  try
    while True do
    begin
      ANextInt := ReadNextChar;
      if ANextInt = -1 then
      begin
        FCurrentValue := TdxConstantValue.Create(AStr.ToString);
        YyError(sdxFilteringExceptionsTextLexerNonClosedElement, [sdxFilteringExceptionsTextLexerElementGuidLiteral, '}']);
        Exit;
      end;
      ANextChar := Char(ANextInt);
      if ANextChar = '}' then
        Break;
      AStr.Append(ANextChar);
    end;
    S := AStr.ToString;
    try
      FCurrentValue := TdxConstantValue.Create(StringToGUID('{' + S + '}'));
      Exit;
    except
    end;
    FCurrentValue := TdxConstantValue.Create(S);
    YyError(sdxFilteringExceptionsTextLexerInvalidElement, [sdxFilteringExceptionsTextLexerElementGuidLiteral, S]);
  finally
    AStr.Free;
  end;
end;

procedure TdxCriteriaLexer.CatchAll(AFirstChar: Char);
var
  AStr: TStringBuilder;
  ANextInt: Integer;
  ANextChar: Char;
begin
  if not CanStartColumn(AFirstChar) then
  begin
    FCurrentToken := TdxToken.YyErrorCode;
    FCurrentValue := AFirstChar;
    YyError(sdxFilteringExceptionsTextLexerInvalidInputCharacter, [AFirstChar]);
    Exit;
  end;
  AStr := TStringBuilder.Create;
  AStr.Append(AFirstChar);
  try
    while True do
    begin
      ANextInt := Integer(PeekNextChar);
      if ANextInt = -1 then
        Break;
      ANextChar := Char(ANextInt);
      if not CanContinueColumn(ANextChar) then
        Break;
      ReadNextChar;
      AStr.Append(ANextChar);
    end;
    ToTokenAndValue(AStr.ToString, FCurrentToken, FCurrentValue, RecognizeSorting);
  finally
    AStr.Free;
  end;
end;

class procedure TdxCriteriaLexer.ToTokenAndValue(const AStr: string; out ACurrentToken: Integer;
  out ACurrentValue: TdxSmartValue; AAllowSorting: Boolean = False);
var
  AToken: TToken;
begin
  ACurrentValue := TValue.Empty;
  if not GetToken(AStr, AToken) then
  begin
    ACurrentToken := TdxToken.COL;
    ACurrentValue := TdxOperandProperty.Create(AStr);
    Exit;
  end;
  case AToken of
    TToken.&AND:
      ACurrentToken := TdxToken.AND;
    TToken.&OR:
      ACurrentToken := TdxToken.OR;
    TToken.&TRUE:
      begin
        ACurrentToken := TdxToken.CONST;
        ACurrentValue := TdxConstantValue.Create(True);
      end;
    TToken.&FALSE:
      begin
        ACurrentToken := TdxToken.CONST;
        ACurrentValue := TdxConstantValue.Create(False);
      end;
    TToken.&NOT:
      ACurrentToken := TdxToken.NOT;
    TToken.&IS:
      ACurrentToken := TdxToken.IS;
    TToken.NULL:
      ACurrentToken := TdxToken.NULL;
    TToken.ISNULL:
      ACurrentToken := TdxToken.FN_ISNULL;
    TToken.ISNULLOREMPTY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsNullOrEmpty);
      end;
    TToken.TRIM:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Trim);
      end;
    TToken.LEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Len);
      end;
    TToken.SUBSTRING:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Substring);
      end;
    TToken.UPPERCASE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.UpperCase);
      end;
    TToken.LOWERCASE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LowerCase);
      end;
    TToken.CUSTOM:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Custom);
      end;
    TToken.CUSTOMNONDETERMINISTIC:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.CustomNonDeterministic);
      end;
    TToken.CONCAT:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Concat);
      end;
    TToken.IIF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Iif);
      end;
    TToken.ABS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Abs);
      end;
    TToken.ARCCOS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ArcCos);
      end;
    TToken.INCDAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncDay);
      end;
    TToken.INCHOUR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncHour);
      end;
    TToken.INCMILLISECOND:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncMillisecond);
      end;
    TToken.INCMINUTE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncMinute);
      end;
    TToken.INCMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncMonth);
      end;
    TToken.INCSECOND:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncSecond);
      end;
    TToken.INCTICK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncTick);
      end;
    TToken.ADDTIMESPAN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.AddTimeSpan);
      end;
    TToken.INCYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IncYear);
      end;
    TToken.ASCII:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Ascii);
      end;
    TToken.ARCSIN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ArcSin);
      end;
    TToken.ARCTAN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ArcTan);
      end;
    TToken.ARCTAN2:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ArcTan2);
      end;
    TToken.BIGMUL:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.BigMul);
      end;
    TToken.CEIL:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Ceil);
      end;
    TToken.CHAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Char);
      end;
    TToken.CHARINDEX, TToken.POS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.CharIndex);
      end;
    TToken.COS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Cos);
      end;
    TToken.COSH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Cosh);
      end;
    TToken.EXP:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Exp);
      end;
    TToken.FLOOR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Floor);
      end;
    TToken.DATEOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DateOf);
      end;
    TToken.DAYOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DayOf);
      end;
    TToken.DAYOFTHEWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DayOfTheWeek);
      end;
    TToken.DAYOFTHEYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DayOfTheYear);
      end;
    TToken.HOUROF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.HourOf);
      end;
    TToken.MILLISECONDOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MillisecondOf);
      end;
    TToken.MINUTEOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MinuteOf);
      end;
    TToken.MONTHOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MonthOf);
      end;
    TToken.SECONDOF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.SecondOf);
      end;
    TToken.GETTIMEOFDAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.GetTimeOfDay);
      end;
    TToken.YEAROF:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.YearOf);
      end;
    TToken.DAYSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DaysBetween);
      end;
    TToken.HOURSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.HoursBetween);
      end;
    TToken.MILLISECONDSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MillisecondsBetween);
      end;
    TToken.MINUTESBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MinutesBetween);
      end;
    TToken.MONTHSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.MonthsBetween);
      end;
    TToken.SECONDSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.SecondsBetween);
      end;
    TToken.DATEDIFFTICKS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.DateDiffTicks);
      end;
    TToken.YEARSBETWEEN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.YearsBetween);
      end;
    TToken.LOG:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Log);
      end;
    TToken.LOG10:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Log10);
      end;
    TToken.NOW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Now);
      end;
    TToken.UTCNOW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.UtcNow);
      end;
    TToken.PADLEFT:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.PadLeft);
      end;
    TToken.PADRIGHT:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.PadRight);
      end;
    TToken.POWER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Power);
      end;
    TToken.REMOVE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Remove);
      end;
    TToken.REPLACE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Replace);
      end;
    TToken.REVERSE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Reverse);
      end;
    TToken.RANDOM:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Random);
      end;
    TToken.ROUND:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Round);
      end;
    TToken.SIGN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Sign);
      end;
    TToken.SIN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Sin);
      end;
    TToken.SINH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Sinh);
      end;
    TToken.SQR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Sqr);
      end;
    TToken.TOSTRING:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToString);
      end;
    TToken.INSERT:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Insert);
      end;
    TToken.TAN:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Tan);
      end;
    TToken.TANH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Tanh);
      end;
    TToken.TODAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Today);
      end;
    TToken.TOINTEGER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToInteger);
      end;
    TToken.TOINT64:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToInt64);
      end;
    TToken.TOSINGLE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToSingle);
      end;
    TToken.TODOUBLE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToDouble);
      end;
    TToken.TODECIMAL:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.ToDecimal);
      end;
    TToken.STARTSWITH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.StartsWith);
      end;
    TToken.ENDSWITH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.EndsWith);
      end;
    TToken.CONTAINS:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.Contains);
      end;
    TToken.BETWEEN:
      ACurrentToken := TdxToken.OP_BETWEEN;
    TToken.IN:
      ACurrentToken := TdxToken.OP_IN;
    TToken.EXISTS:
      ACurrentToken := TdxToken.AGG_EXISTS;
    TToken.COUNT:
      ACurrentToken := TdxToken.AGG_COUNT;
    TToken.MIN:
      ACurrentToken := TdxToken.AGG_MIN;
    TToken.MAX:
      ACurrentToken := TdxToken.AGG_MAX;
    TToken.SINGLE:
      ACurrentToken := TdxToken.AGG_SINGLE;
    TToken.AVG:
      ACurrentToken := TdxToken.AGG_AVG;
    TToken.SUM:
      ACurrentToken := TdxToken.AGG_SUM;
    TToken.LOCALDATETIMETHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeThisYear);
      end;
    TToken.LOCALDATETIMETHISMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeThisMonth);
      end;
    TToken.LOCALDATETIMELASTWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeLastWeek);
      end;
    TToken.LOCALDATETIMETHISWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeThisWeek);
      end;
    TToken.LOCALDATETIMEYESTERDAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeYesterday);
      end;
    TToken.LOCALDATETIMETODAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeToday);
      end;
    TToken.LOCALDATETIMENOW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeNow);
      end;
    TToken.LOCALDATETIMETOMORROW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeTomorrow);
      end;
    TToken.LOCALDATETIMEDAYAFTERTOMORROW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow);
      end;
    TToken.LOCALDATETIMENEXTWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeNextWeek);
      end;
    TToken.LOCALDATETIMETWOWEEKSAWAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway);
      end;
    TToken.LOCALDATETIMENEXTMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeNextMonth);
      end;
    TToken.LOCALDATETIMENEXTYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeNextYear);
      end;
    TToken.LOCALDATETIMETWOMONTHSAWAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeTwoMonthsAway);
      end;
    TToken.LOCALDATETIMETWOYEARSAWAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeTwoYearsAway);
      end;
    TToken.LOCALDATETIMELASTMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeLastMonth);
      end;
    TToken.LOCALDATETIMELASTYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeLastYear);
      end;
    TToken.LOCALDATETIMEYEARBEFORETODAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.LocalDateTimeYearBeforeToday);
      end;
    TToken.ISOUTLOOKINTERVALBEYONDTHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear);
      end;
    TToken.ISOUTLOOKINTERVALLATERTHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear);
      end;
    TToken.ISOUTLOOKINTERVALLATERTHISMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth);
      end;
    TToken.ISOUTLOOKINTERVALNEXTWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalNextWeek);
      end;
    TToken.ISOUTLOOKINTERVALLATERTHISWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek);
      end;
    TToken.ISOUTLOOKINTERVALTOMORROW:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalTomorrow);
      end;
    TToken.ISOUTLOOKINTERVALTODAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalToday);
      end;
    TToken.ISOUTLOOKINTERVALYESTERDAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalYesterday);
      end;
    TToken.ISOUTLOOKINTERVALEARLIERTHISWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek);
      end;
    TToken.ISOUTLOOKINTERVALLASTWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalLastWeek);
      end;
    TToken.ISOUTLOOKINTERVALEARLIERTHISMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth);
      end;
    TToken.ISOUTLOOKINTERVALEARLIERTHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear);
      end;
    TToken.ISOUTLOOKINTERVALPRIORTHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear);
      end;
    TToken.ISTHISWEEK:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsThisWeek);
      end;
    TToken.ISTHISMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsThisMonth);
      end;
    TToken.ISTHISYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsThisYear);
      end;
    TToken.ISNEXTMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsNextMonth);
      end;
    TToken.ISNEXTYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsNextYear);
      end;
    TToken.ISLASTMONTH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsLastMonth);
      end;
    TToken.ISLASTYEAR:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsLastYear);
      end;
    TToken.ISYEARTODATE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsYearToDate);
      end;
    TToken.ISSAMEDAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsSameDay);
      end;
    TToken.ISJANUARY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsJanuary);
      end;
    TToken.ISFEBRUARY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsFebruary);
      end;
    TToken.ISMARCH:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsMarch);
      end;
    TToken.ISAPRIL:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsApril);
      end;
    TToken.ISMAY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsMay);
      end;
    TToken.ISJUNE:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsJune);
      end;
    TToken.ISJULY:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsJuly);
      end;
    TToken.ISAUGUST:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsAugust);
      end;
    TToken.ISSEPTEMBER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsSeptember);
      end;
    TToken.ISOCTOBER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsOctober);
      end;
    TToken.ISNOVEMBER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsNovember);
      end;
    TToken.ISDECEMBER:
      begin
        ACurrentToken := TdxToken.FUNCTION;
        ACurrentValue := TValue.From<TdxFunctionOperatorType>(TdxFunctionOperatorType.IsDecember);
      end;
    TToken.ASC,
    TToken.ASCENDING:
      if AAllowSorting then
        ACurrentToken := TdxToken.SORT_ASC
      else
      begin
        ACurrentToken := TdxToken.COL;
        ACurrentValue := TdxOperandProperty.Create(AStr);
      end;
    TToken.DESC,
    TToken.DESCENDING:
      if AAllowSorting then
        ACurrentToken := TdxToken.SORT_DESC
      else
      begin
        ACurrentToken := TdxToken.COL;
        ACurrentValue := TdxOperandProperty.Create(AStr);
      end;
  end;
end;

procedure TdxCriteriaLexer.DoNumber(AFirstSymbol: Char);
var
  AStringBuilder: TStringBuilder;
  ANumericCode: string;
  ANextInt: Integer;
  ANextChar: Char;
begin
  AStringBuilder := TStringBuilder.Create;
  try
    AStringBuilder.Append(AFirstSymbol);
    while True do
    begin
      ANextInt := Integer(PeekNextChar);
      ANextChar := Char(ANextInt);
      case ANextChar of
        '0'..'9',
        '.':
          begin
            ReadNextChar;
            AStringBuilder.Append(ANextChar);
          end;
        'e',
        'E':
          begin
            ReadNextChar;
            AStringBuilder.Append(ANextChar);
            ANextInt := ReadNextChar;
            if ANextInt = -1 then
            begin
              YyError(sdxFilteringExceptionsTextLexerInvalidElement, [sdxFilteringExceptionsTextLexerElementNumberLiteral, AStringBuilder]);
              Break;
            end;
            ANextChar := Char(ANextInt);
            AStringBuilder.Append(ANextChar);
          end;
        else
          begin
            FCurrentToken := TdxToken.CONST;
            ANumericCode := GetNumericCode;
            try
              FCurrentValue := TdxConstantValue.Create(ExtractNumericValue(AStringBuilder.ToString, ANumericCode));
            except
              FCurrentValue := TdxConstantValue.Create(AStringBuilder.ToString + ANumericCode);
              YyError(sdxFilteringExceptionsTextLexerInvalidElement, [sdxFilteringExceptionsTextLexerElementNumberLiteral, AStringBuilder.ToString + ANumericCode]);
            end;
            Exit;
          end;
      end;
    end;
  finally
    AStringBuilder.Free;
  end;
end;

function TdxCriteriaLexer.ExtractNumericValue(const AStr: string; const ANumericCode: string): TValue;
var
  AToken: TNumericValueToken;
begin
  if GetNumericValueTokens(ANumericCode, AToken) then
    case AToken of
      TNumericValueToken.m:
        NotImplemented;
      TNumericValueToken.f:
        Exit(TValue.From<Single>(StrToFloat(AStr, InvariantCulture)));
      TNumericValueToken.i:
        Exit(StrToInt(AStr));
      TNumericValueToken.s:
        Exit(TValue.From<SmallInt>(StrToInt(AStr)));
      TNumericValueToken.l:
        Exit(StrToInt64(AStr));
      TNumericValueToken.b:
        Exit(TValue.From<Byte>(StrToInt(AStr)));
      TNumericValueToken.u,
      TNumericValueToken.ui,
      TNumericValueToken.iu:
        Exit(TValue.From<Cardinal>(StrToInt(AStr)));
      TNumericValueToken.sb,
      TNumericValueToken.bs:
        Exit(TValue.From<ShortInt>(StrToInt(AStr)));
      TNumericValueToken.us,
      TNumericValueToken.su:
        Exit(TValue.From<Word>(StrToInt(AStr)));
      TNumericValueToken.ul,
      TNumericValueToken.lu:
        Exit(TValue.From<UInt64>(StrToInt64(AStr)));
    end
  else
    if AStr <> '' then
    begin
      {$IFDEF DELPHIXE3}
      if AStr.IndexOfAny(['.', 'e', 'E']) >= 0 then
      {$ELSE}
      if TdxStringHelper.IndexOfAny(AStr, ['.', 'e', 'E']) >= 0 then
      {$ENDIF}
        Exit(StrToFloat(AStr, InvariantCulture));
      try
        Exit(StrToInt(AStr));
      except
      end;
      try
        Exit(StrToInt64(AStr));
      except
      end;
      Result := StrToFloat(AStr, InvariantCulture);
    end
    else
      raise EInvalidOperation.Create(sdxFilteringExceptionsTextInvalidTypeCode);
end;

function TdxCriteriaLexer.GetNumericCode: string;
var
  APeeked: Integer;
  ACh, ACh2: Char;
begin
  APeeked := Integer(PeekNextChar);
  if APeeked = -1 then
    Exit('');
  ACh := Char(APeeked);
  case ACh of
    'm',
    'M',
    'f',
    'F':
      begin
        ReadNextChar;
        Exit(ACh);
      end;
    'b',
    's',
    'i',
    'l',
    'u',
    'B',
    'S',
    'I',
    'L',
    'U':
    else
      Exit('');
  end;
  ReadNextChar;
  APeeked := Integer(PeekNextChar);
  if APeeked <> -1 then
  begin
    ACh2 := Char(APeeked);
    case ACh2 of
      'b',
      's',
      'i',
      'l',
      'u',
      'B',
      'S',
      'I',
      'L',
      'U':
      begin
        ReadNextChar;
        Exit(ACh + ACh2);
      end;
    end;
  end;
  Result := ACh;
end;

procedure TdxCriteriaLexer.DoDotOrNumber;
begin
  case PeekNextChar of
    '0'..'9':
      DoNumber('.');
    else
      FCurrentToken := Integer('.');
  end;
end;

class function TdxCriteriaLexer.CanStartColumn(AValue: Char): Boolean;
begin
  case {$IFDEF DELPHIXE4}AValue.GetUnicodeCategory{$ELSE}GetUnicodeCategory(AValue){$ENDIF} of
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter,
    TUnicodeCategory.ucConnectPunctuation:
      Result := True;
    else
      Result := False;
  end;
end;

class function TdxCriteriaLexer.CanContinueColumn(AValue: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}AValue.GetUnicodeCategory{$ELSE}GetUnicodeCategory(AValue){$ENDIF} in
    [TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter,
    TUnicodeCategory.ucConnectPunctuation,
    TUnicodeCategory.ucDecimalNumber,
    TUnicodeCategory.ucLetterNumber,
    TUnicodeCategory.ucOtherNumber];
end;

class function TdxCriteriaLexer.IsGoodUnescapedName(const AFunctionName: string): Boolean;
var
  I, AToken: Integer;
  AValue: TdxSmartValue;
begin
  if AFunctionName = '' then
    Exit(False);
  if not CanStartColumn(PChar(AFunctionName)[0]) then
    Exit(False);
  for I := 1 to Length(AFunctionName) - 1 do
    if not CanContinueColumn(AFunctionName[I]) then
      Exit(False);
  ToTokenAndValue(AFunctionName, AToken, AValue);
  Result := AToken = TdxToken.COL;
end;

procedure TdxCriteriaLexer.YyError(const AMessage: string; const AArgs: array of const);
var
  AFullMessage: string;
begin
  AFullMessage := Format(AMessage, AArgs);
  raise EdxCriteriaParserException.Create(AFullMessage);
end;

procedure TdxCriteriaLexer.CheckFunctionArgumentsCount(const AOperator: IdxFunctionOperator);
var
  AFunctionType: TdxFunctionOperatorType;
  AArgumentCount, I: Integer;
  AArgumentCountArray: TArray<Integer>;
begin
  AFunctionType := AOperator.OperatorType;
  AArgumentCount := AOperator.Operands.Count;
  if AFunctionType in [TdxFunctionOperatorType.Custom, TdxFunctionOperatorType.CustomNonDeterministic] then
  begin
    if (AArgumentCount > 0) and TdxFunctionOperatorHelper.IsValidCustomFunctionArgumentCount(
      (TdxOperandValue(AOperator.Operands[0])).Value.ToString, AArgumentCount - 1) then
      Exit;
  end
  else
  begin
    AArgumentCountArray := TdxFunctionOperatorHelper.GetFunctionArgumentCount(AFunctionType);
    if AArgumentCountArray = nil then
      YyError(sdxFilteringExceptionsTextLexerWrongFunction,
        [GetEnumName(TypeInfo(TdxFunctionOperatorType), Ord(AFunctionType))]);
    for I := 0 to Length(AArgumentCountArray) - 1 do
    begin
      if AArgumentCountArray[I] = AArgumentCount then
        Exit;
      if AArgumentCountArray[I] < 0 then
      begin
        if (-AArgumentCountArray[I]) <= AArgumentCount then
          Exit;
      end;
    end;
  end;
  YyError(sdxFilteringExceptionsTextLexerWrongArgumentCount,
    [AArgumentCount, GetEnumName(TypeInfo(TdxFunctionOperatorType), Ord(AFunctionType))]);
end;

{ TdxCriteriaParser }

procedure TdxCriteriaParser.ClearResults;
begin
  FResult := nil;
  FResultParameters.Clear;
end;

constructor TdxCriteriaParser.Create;
begin
  inherited Create;
  FResultParameters := TList<IdxOperandValue>.Create;
end;

destructor TdxCriteriaParser.Destroy;
begin
  FreeResults;
  FreeAndNil(FResultParameters);
  FreeAndNil(FLexer);
  inherited Destroy;
end;

procedure TdxCriteriaParser.FreeResults;
begin
  ClearResults;
end;

class function TdxCriteriaParser.JoinOrAggregate(const ACollectionProperty: IdxOperandProperty;
  const ACondition: IdxCriteriaOperator; AType: TdxAggregateFunctionType; const AAggregated: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if (ACollectionProperty = nil) or (Length(ACollectionProperty.PropertyName) < 2) or
    (PChar(ACollectionProperty.PropertyName)[0] <> '<') or
    (PChar(ACollectionProperty.PropertyName)[Length(ACollectionProperty.PropertyName) - 1] <> '>') then
    Result := TdxAggregateOperand.Create(ACollectionProperty, AAggregated, AType, ACondition)
  else
    Result := TdxJoinOperand.Create(
      {$IFDEF DELPHIXE3}
        ACollectionProperty.PropertyName.Substring(1, Length(ACollectionProperty.PropertyName) - 2)
      {$ELSE}
        TdxStringHelper.Substring(ACollectionProperty.PropertyName, 1, Length(ACollectionProperty.PropertyName) - 2)
      {$ENDIF}, ACondition, AType, AAggregated);
end;

function IInc(var X: Integer; N: Integer): Integer; inline;
begin
  Inc(X, N);
  Result := X;
end;

function TdxCriteriaParser.YyParse(const AYyLex: IdxyyInput): TValue;
label
  skip, yyLoop, yyDiscarded;
var
  AYyState, AYyToken, AYyErrorFlag, AYyTop, AYyN, AYyV, AYyM: Integer;
  AYyStates, I: TArray<Integer>;
  AYyVal: TdxSmartValue;
  AYyValues, O: TArray<TdxSmartValue>;
  AProp1, AProp2, AProp3, AProp4: IdxOperandProperty;
  AAggregateOperand: IdxAggregateOperand;
  AParamName: string;
  V: IdxOperandValue;
  AParam, AOperand: IdxOperandValue;
  AParamNotFound: Boolean;
  P: IdxOperandParameter;
  AFunctionOperator: IdxFunctionOperator;
  ALst: TdxCriteriaOperatorList;
begin
  if FYyMax <= 0 then
    FYyMax := 256;
  AYyState := 0;
  SetLength(AYyStates, FYyMax);
  AYyVal := TValue.Empty;
  SetLength(AYyValues, FYyMax);
  AYyToken := -1;
  AYyErrorFlag := 0;

  AYyTop := 0;
  goto skip;

yyLoop:
  Inc(AYyTop);

skip:
  while True do
  begin
    if AYyTop >= Length(AYyStates) then
    begin
      SetLength(I, Length(AYyStates) + FYyMax);
      TArray.Copy<Integer>(AYyStates, I, 0);
      AYyStates := I;
      SetLength(O, Length(AYyValues) + FYyMax);
      TArray.Copy<TdxSmartValue>(AYyValues, O, 0);
      AYyValues := O;
    end;
    AYyStates[AYyTop] := AYyState;
    AYyValues[AYyTop] := AYyVal;

yyDiscarded:
    while True do
    begin
      AYyN := YyDefRed[AYyState];
      if AYyN = 0 then
      begin
        if AYyToken < 0 then
          if AYyLex.advance then
            AYyToken := AYyLex.token
          else
            AYyToken := 0;
        AYyN := yySIndex[AYyState];
        if (((AYyN <> 0) and ((IInc(AYyN, AYyToken)) >= 0)) and (AYyN < Length(YyTable))) and (YyCheck[AYyN] = AYyToken) then
        begin
          AYyState := YyTable[AYyN];
          AYyVal := AYyLex.value;
          AYyToken := -1;
          if AYyErrorFlag > 0 then
            Dec(AYyErrorFlag);
          goto yyLoop;
        end;
        AYyN := yyRIndex[AYyState];
        if (((AYyN <> 0) and ((IInc(AYyN, AYyToken)) >= 0)) and (AYyN < Length(YyTable))) and (YyCheck[AYyN] = AYyToken) then
          AYyN := YyTable[AYyN]
        else
        begin
          if AYyErrorFlag = 0 then
          begin
            YyError(sdxFilteringExceptionsTextSyntaxError);
            AYyErrorFlag := 1;
          end;
          case AYyErrorFlag of
            1, 2:
              begin
                AYyErrorFlag := 3;
                repeat
                  AYyN := yySIndex[AYyStates[AYyTop]];
                  if (((AYyN <> 0) and
                    ((IInc(AYyN, TdxToken.YyErrorCode)) >= 0)) and (AYyN < Length(YyTable))) and (YyCheck[AYyN] = TdxToken.YyErrorCode) then
                  begin
                    AYyState := YyTable[AYyN];
                    AYyVal := AYyLex.value;
                    goto yyLoop;
                  end;
                  Dec(AYyTop)
                until not (AYyTop >= 0);
                YyError(sdxFilteringExceptionsTextIrrecoverableSyntaxError);
                goto yyDiscarded;
              end;
            3:
              begin
                if AYyToken = 0 then
                  YyError(sdxFilteringExceptionsTextIrrecoverableSyntaxErrorAtEnd);
                AYyToken := -1;
                goto yyDiscarded;
              end;
          end;
        end;
      end;
      AYyV := AYyTop + 1 - YyLen[AYyN];
      if AYyV > AYyTop then
        AYyVal := nil
      else
        AYyVal := AYyValues[AYyV];
      case AYyN of
        1:
          SetLength(FResult, 0);
        2:
          FResult := TdxCriteriaOperatorList(AYyValues[-1 + AYyTop].AsObject).ToArray;
        3:
          begin
            ALst := TdxCriteriaOperatorList.Create;
            AYyVal := ALst;
            ALst.Add((AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator));
          end;
        4:
          begin
            AYyVal := AYyValues[-2 + AYyTop];
            TdxCriteriaOperatorList(AYyVal.AsObject).Add(AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
          end;
        5:
          begin
            AYyVal := AYyValues[-2 + AYyTop];
            TdxCriteriaOperatorList(AYyVal.AsObject).Add(AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
          end;
        6:
          AYyVal := AYyValues[0 + AYyTop];
        7:
          AYyVal := AYyValues[-1 + AYyTop];
        8:
          begin
            NotImplemented;
          end;
        9:
          AYyVal := AYyValues[0 + AYyTop];
        10:
          begin
            AProp1 := AYyValues[-2 + AYyTop].AsInterface as IdxOperandProperty;
            AProp3 := AYyValues[0 + AYyTop].AsInterface as IdxOperandProperty;
            AYyVal := TdxOperandProperty.Create(AProp1.PropertyName + '.' + AProp3.PropertyName);
          end;
        11:
          begin
            AProp1 := AYyValues[-2 + AYyTop].AsInterface as IdxOperandProperty;
            AProp3 := AYyValues[0 + AYyTop].AsInterface as IdxOperandProperty;
            AYyVal := TdxOperandProperty.Create(AProp1.PropertyName + '+' + AProp3.PropertyName);
          end;
        12:
          begin
            AProp2 := AYyValues[-2 + AYyTop].AsInterface as IdxOperandProperty;
            AProp4 := AYyValues[0 + AYyTop].AsInterface as IdxOperandProperty;
            AYyVal := TdxOperandProperty.Create('<' + AProp2.PropertyName + '>' + AProp4.PropertyName);
          end;
        13:
          AYyVal := AYyValues[0 + AYyTop];
        14:
          AYyVal := AYyValues[0 + AYyTop];
        15:
          AYyVal := TdxOperandProperty.Create('^');
        16:
          AYyVal := AYyValues[0 + AYyTop];
        17:
          begin
            AProp1 := AYyValues[-2 + AYyTop].AsInterface as IdxOperandProperty;
            AProp3 := AYyValues[0 + AYyTop].AsInterface as IdxOperandProperty;
            (AProp1 as TdxOperandProperty).PropertyName := AProp1.PropertyName + '.' + AProp3.PropertyName;
            AYyVal := AProp1;
          end;
        18:
          begin
            AAggregateOperand := AYyValues[0 + AYyTop].AsInterface as IdxAggregateOperand;
            AYyVal := JoinOrAggregate(AYyValues[-2 + AYyTop].AsInterface as TdxOperandProperty, nil, AAggregateOperand.AggregateFunctionType, AAggregateOperand.AggregatedExpression);
          end;
        19:
          begin
            AAggregateOperand := AYyValues[0 + AYyTop].AsInterface as IdxAggregateOperand;
            AYyVal := JoinOrAggregate(AYyValues[-5 + AYyTop].AsInterface as IdxOperandProperty,
              AYyValues[-3 + AYyTop].AsInterface as IdxCriteriaOperator, AAggregateOperand.AggregateFunctionType, AAggregateOperand.AggregatedExpression);
          end;
        20:
          begin
            AAggregateOperand := AYyValues[0 + AYyTop].AsInterface as IdxAggregateOperand;
            AYyVal := JoinOrAggregate(AYyValues[-4 + AYyTop].AsInterface as IdxOperandProperty, nil, AAggregateOperand.AggregateFunctionType, AAggregateOperand.AggregatedExpression);
          end;
        21:
          AYyVal := JoinOrAggregate(AYyValues[-3 + AYyTop].AsInterface as IdxOperandProperty, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Exists, nil);
        22:
          AYyVal := JoinOrAggregate(AYyValues[-2 + AYyTop].AsInterface as IdxOperandProperty, nil, TdxAggregateFunctionType.Exists, nil);
        25:
          AYyVal := TdxAggregateOperand.Create(nil, nil, TdxAggregateFunctionType.Count, nil);
        26:
          AYyVal := TdxAggregateOperand.Create(nil, nil, TdxAggregateFunctionType.Exists, nil);
        27:
          AYyVal := TdxAggregateOperand.Create(nil, nil, TdxAggregateFunctionType.Count, nil);
        28:
          AYyVal := TdxAggregateOperand.Create(nil, nil, TdxAggregateFunctionType.Exists, nil);
        29:
          AYyVal := TdxAggregateOperand.Create(nil, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Avg, nil);
        30:
          AYyVal := TdxAggregateOperand.Create(nil, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Sum, nil);
        31:
          AYyVal := TdxAggregateOperand.Create(nil, TdxOperandProperty.Create('This'), TdxAggregateFunctionType.Single, nil);
        32:
          AYyVal := TdxAggregateOperand.Create(nil, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Single, nil);
        33:
          AYyVal := AYyValues[-1 + AYyTop];
        34:
          AYyVal := AYyValues[-1 + AYyTop];
        35:
          AYyVal := TdxAggregateOperand.Create(nil, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Min, nil);
        36:
          AYyVal := TdxAggregateOperand.Create(nil, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxAggregateFunctionType.Max, nil);
        37:
          AYyVal := AYyValues[0 + AYyTop];
        38:
          AYyVal := TdxConstantValue.Create(nil);
        39:
          begin
            AParamName := AYyValues[0 + AYyTop].Value.AsString;
            if AParamName = '' then
            begin
              AParam := TdxOperandValue.Create;
              FResultParameters.Add(AParam);
              AYyVal := AParam;
            end
            else
            begin
              AParamNotFound := True;
              for V in FResultParameters do
              begin
                P := Safe<TdxOperandParameter>.Cast(V as TdxCriteriaOperator);
                if P = nil then
                  Continue;
                if P.ParameterName <> AParamName then
                  Continue;
                AParamNotFound := False;
                FResultParameters.Add(V);
                AYyVal := V;
                Break;
              end;
              if AParamNotFound then
              begin
                AParam := TdxOperandParameter.Create(AParamName);
                FResultParameters.Add(AParam);
                AYyVal := AParam;
              end;
            end;
          end;
        40:
          AYyVal := AYyValues[0 + AYyTop];
        41:
          AYyVal := AYyValues[0 + AYyTop];
        42:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Multiply);
        43:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Divide);
        44:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Plus);
        45:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Minus);
        46:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Modulo);
        47:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.BitwiseOr);
        48:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.BitwiseAnd);
        49:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator,
            AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.BitwiseXor);
        50:
          begin
            AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.Minus, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
            try
              if AYyValues[0 + AYyTop].AsInterface is TdxOperandValue then
              begin
                AOperand := AYyValues[0 + AYyTop].AsInterface as IdxOperandValue;
                case AOperand.Value.TypeInfo.Kind of
                  tkInteger:
                    if AOperand.Value.IsType<SmallInt> then
                    begin
                      AOperand.Value := - AOperand.Value.AsType<SmallInt>;
                      AYyVal := AOperand;
                    end
                    else
                      if AOperand.Value.IsType<ShortInt> then
                      begin
                        AOperand.Value := - AOperand.Value.AsType<ShortInt>;
                        AYyVal := AOperand;
                      end
                    else
                      begin
                        AOperand.Value := - AOperand.Value.AsInteger;
                        AYyVal := AOperand;
                      end;
                  tkFloat:
                    if AOperand.Value.IsDouble then
                    begin
                      AOperand.Value := - AOperand.Value.AsDouble;
                      AYyVal := AOperand;
                    end
                    else
                      if AOperand.Value.IsSingle then
                      begin
                        AOperand.Value := -AOperand.Value.AsSingle;
                        AYyVal := AOperand;
                      end;
                  tkInt64:
                    begin
                      AOperand.Value := - AOperand.Value.AsInt64;
                      AYyVal := AOperand;
                    end;
                end;
              end;
            except

            end;
          end;
        51:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.Plus, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
        52:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.BitwiseNot, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
        53:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Equal);
        54:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.NotEqual);
        55:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Greater);
        56:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.Less);
        57:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.GreaterOrEqual);
        58:
          AYyVal := TdxBinaryOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator, TdxBinaryOperatorType.LessOrEqual);
        59:
          NotImplemented;
        60:
          NotImplemented;
        61:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.Not, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
        62:
          AYyVal := TdxGroupOperator.And(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
        63:
          AYyVal := TdxGroupOperator.Or(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
        64:
          AYyVal := AYyValues[-1 + AYyTop];
        65:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.IsNull, AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator);
        66:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.Not, TdxUnaryOperator.Create(TdxUnaryOperatorType.IsNull, AYyValues[-3 + AYyTop].AsInterface as IdxCriteriaOperator));
        67:
          AYyVal := TdxInOperator.Create(AYyValues[-2 + AYyTop].AsInterface as IdxCriteriaOperator, TdxCriteriaOperatorList(AYyValues[0 + AYyTop].AsObject).ToArray);
        68:
          AYyVal := TdxBetweenOperator.Create(AYyValues[-6 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[-3 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator);
        69:
          AYyVal := TdxUnaryOperator.Create(TdxUnaryOperatorType.IsNull, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator);
        70:
          AYyVal := TdxFunctionOperator.Create(TdxFunctionOperatorType.IsNull, [AYyValues[-3 + AYyTop].AsInterface as IdxCriteriaOperator, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator]);
        71:
          begin
            AFunctionOperator := TdxFunctionOperator.Create(AYyValues[-1 + AYyTop].&As<TdxFunctionOperatorType>, TdxCriteriaOperatorList(AYyValues[0 + AYyTop].AsObject).ToArray);
            AYyVal := AFunctionOperator;
            FLexer.CheckFunctionArgumentsCount(AFunctionOperator);
          end;
        72:
          begin
            NotImplemented;
          end;
        73:
          AYyVal := nil;
        74:
          begin
            AAggregateOperand := AYyValues[-3 + AYyTop].AsInterface as IdxAggregateOperand;
            AYyVal := TdxFunctionOperator.Create(TdxFunctionOperatorType.Min, [AAggregateOperand.AggregatedExpression, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator]);
            (AAggregateOperand as TdxAggregateOperand).AggregatedExpression := nil;
          end;
        75:
          begin
            AAggregateOperand := AYyValues[-3 + AYyTop].AsInterface as IdxAggregateOperand;
            AYyVal := TdxFunctionOperator.Create(TdxFunctionOperatorType.Max, [AAggregateOperand.AggregatedExpression, AYyValues[-1 + AYyTop].AsInterface as IdxCriteriaOperator]);
            (AAggregateOperand as TdxAggregateOperand).AggregatedExpression := nil;
          end;
        76:
          AYyVal := AYyValues[-1 + AYyTop];
        77:
          AYyVal := TdxCriteriaOperatorList.Create;
        78:
          begin
            ALst := TdxCriteriaOperatorList.Create;
            AYyVal := ALst;
            ALst.Add(AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
          end;
        79:
          begin
            ALst := TdxCriteriaOperatorList(AYyValues[-2 + AYyTop].AsObject);
            if AYyVal <> ALst then
              AYyVal := ALst;
            ALst.Add(AYyValues[0 + AYyTop].AsInterface as IdxCriteriaOperator);
          end;
      end;
      Dec(AYyTop, YyLen[AYyN]);
      AYyState := AYyStates[AYyTop];
      AYyM := YyLhs[AYyN];
      if (AYyState = 0) and (AYyM = 0) then
      begin
        AYyState := YyFinal;
        if AYyToken < 0 then
          if AYyLex.advance then
            AYyToken := AYyLex.token
          else
            AYyToken := 0;
        if AYyToken = 0 then
          Exit(AYyVal);
        goto yyLoop;
      end;
      AYyN := yyGIndex[AYyM];
      if (((AYyN <> 0) and ((IInc(AYyN, AYyState)) >= 0)) and (AYyN < Length(YyTable))) and (YyCheck[AYyN] = AYyState) then
        AYyState := YyTable[AYyN]
      else
        AYyState := YyDGoTo[AYyM];
      goto yyLoop;
    end;
  end;
end;

procedure TdxCriteriaParser.YyError(const AMessage: string);
begin
  YyError(AMessage, nil);
end;

procedure TdxCriteriaParser.YyError(const AMessage: string; const AExpected: TArray<string>);
var
  ABuf: TStringBuilder;
  AMsg: string;
  N: Integer;
begin
  ABuf := TStringBuilder.Create(AMessage);
  try
    if (AExpected <> nil) and (Length(AExpected) > 0) then
    begin
      ABuf.Append(', expecting'#10);
      N := 0;
      while N < Length(AExpected) do
      begin
        ABuf.Append(' ').Append(AExpected[N]);
        Inc(N);
      end;
      ABuf.Append(#10);
    end;
    AMsg := ABuf.ToString;
  finally
    ABuf.Free;
  end;
  raise EdxCriteriaParserException.Create(AMsg);
end;

procedure TdxCriteriaParser.Parse(const AQuery: string; AAllowSort: Boolean = False);
var
  AStringReader: TStringReader;
  AMalformedQuery: string;
begin
  AStringReader := TStringReader.Create(AQuery);
  try
    FLexer := TdxCriteriaLexer.Create(AStringReader);
    FLexer.RecognizeSorting := AAllowSort;
    try
      YyParse(FLexer);
    except
      on E: EdxCriteriaParserException do
      begin
        AMalformedQuery := AQuery;
        if FLexer.Line = 0 then
        begin
          try
            {$IFDEF DELPHIXE3}
            AMalformedQuery := AMalformedQuery.Substring(0, FLexer.Column) + sdxFilteringExceptionsTextErrorPointer +
              AMalformedQuery.Substring(FLexer.Column);
            {$ELSE}
            AMalformedQuery := TdxStringHelper.Substring(AMalformedQuery, 0, FLexer.Column) + sdxFilteringExceptionsTextErrorPointer +
              TdxStringHelper.Substring(AMalformedQuery, FLexer.Column);
            {$ENDIF}
          except
          end;
        end;
        raise EdxCriteriaParserException.Create(Format(sdxFilteringExceptionsTextGrammarCatchAllErrorMessage, [FLexer.Line, FLexer.Column, E.Message, AMalformedQuery]));
      end
    end;
  finally
    AStringReader.Free;
  end;
end;

class function TdxCriteriaParser.ParseList(const ACriteriaList: string;
  out ACriteriaParametersList: TArray<IdxOperandValue>; AAllowSorting: Boolean): TArray<IdxCriteriaOperator>;
var
  AParser: TdxCriteriaParser;
begin
  AParser := TdxCriteriaParser.Create;
  try
    AParser.Parse(ACriteriaList, AAllowSorting);
    ACriteriaParametersList := {$IFDEF DELPHIXE}AParser.ResultParameters.ToArray{$ELSE}ToArray(AParser.ResultParameters){$ENDIF};
    Result := AParser.Result;
    AParser.ClearResults;
  finally
    AParser.Free;
  end;
end;

class function TdxCriteriaParser.Parse(const AStringCriteria: string;
  out ACriteriaParametersList: TArray<IdxOperandValue>): IdxCriteriaOperator;
var
  AParser: TdxCriteriaParser;
begin
  Result := nil;
  if AStringCriteria = '' then
  begin
    ACriteriaParametersList := nil;
    Exit;
  end;
  AParser := TdxCriteriaParser.Create;
  try
    AParser.Parse(AStringCriteria);
    case Length(AParser.Result) of
      0:
        Result := nil;
      1:
        begin
          Result := AParser.Result[0];
        end
      else
        raise EArgumentException.Create(sdxFilteringExceptionsTextSingleCriterionExpected);
    end;
    ACriteriaParametersList := {$IFDEF DELPHIXE}AParser.ResultParameters.ToArray{$ELSE}ToArray(AParser.ResultParameters){$ENDIF};
    AParser.ClearResults;
  finally
    AParser.Free;
  end;
end;

end.
