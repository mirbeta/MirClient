{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxCalendarUtils;

{$I cxVer.inc}

interface

uses
  Windows, cxDateUtils;

const
  CAL_GREGORIAN                   = 1;  //Gregorian (localized) calendar
  {$EXTERNALSYM CAL_GREGORIAN}
  CAL_GREGORIAN_US                = 2;  //Gregorian (U.S.) calendar
  {$EXTERNALSYM CAL_GREGORIAN_US}
  CAL_JAPAN                       = 3;  // Japanese Emperor Era calendar
  {$EXTERNALSYM CAL_JAPAN}
  CAL_TAIWAN                      = 4;  // Republic of China Era calendar
  {$EXTERNALSYM CAL_TAIWAN}
  CAL_KOREA                       = 5;  // Korean Tangun Era calendar
  {$EXTERNALSYM CAL_KOREA}
  CAL_HIJRI                       = 6;  // Hijri (Arabic Lunar) calendar
  {$EXTERNALSYM CAL_HIJRI}
  CAL_THAI                        = 7;  // Thai calendar
  {$EXTERNALSYM CAL_THAI}
  CAL_HEBREW                      = 8;  // Hebrew calendar
  {$EXTERNALSYM CAL_HEBREW}
  CAL_GREGORIAN_ME_FRENCH         = 9;  // Gregorian Middle East French calendar
  {$EXTERNALSYM CAL_GREGORIAN_ME_FRENCH}
  CAL_GREGORIAN_ARABIC            = 10; // Gregorian Arabic calendar
  {$EXTERNALSYM CAL_GREGORIAN_ARABIC}
  CAL_GREGORIAN_XLIT_ENGLISH      = 11; // Gregorian Transliterated English calendar
  {$EXTERNALSYM CAL_GREGORIAN_XLIT_ENGLISH}
  CAL_GREGORIAN_XLIT_FRENCH       = 12; // Gregorian Transliterated French calendar
  {$EXTERNALSYM CAL_GREGORIAN_XLIT_FRENCH}
  CAL_JULIAN                      = 13; // Julian calendar
  {$EXTERNALSYM CAL_JULIAN}
  CAL_JAPANESELUNISOLAR           = 14; // Japanes Lunar/Solar calendar
  {$EXTERNALSYM CAL_JAPANESELUNISOLAR}
  CAL_CHINESELUNISOLAR            = 15; // Chinese Lunar/Solar calendar
  {$EXTERNALSYM CAL_CHINESELUNISOLAR}
  CAL_SAKA                        = 16; // reserved to match Office but not implemented in our code
  {$EXTERNALSYM CAL_SAKA}
  CAL_LUNAR_ETO_CHN               = 17; // reserved to match Office but not implemented in our code
  {$EXTERNALSYM CAL_LUNAR_ETO_CHN}
  CAL_LUNAR_ETO_KOR               = 18; // reserved to match Office but not implemented in our code
  {$EXTERNALSYM CAL_LUNAR_ETO_KOR}
  CAL_LUNAR_ETO_ROKUYOU           = 19; // reserved to match Office but not implemented in our code
  {$EXTERNALSYM CAL_LUNAR_ETO_ROKUYOU}
  CAL_KOREANLUNISOLAR             = 20; // Korean Lunar/Solar calendar
  {$EXTERNALSYM CAL_KOREANLUNISOLAR}
  CAL_TAIWANLUNISOLAR             = 21; // Taiwan Lunar/Solar calendar
  {$EXTERNALSYM CAL_TAIWANLUNISOLAR}
  CAL_PERSIAN                     = 22; // Persian calendar
  {$EXTERNALSYM CAL_PERSIAN}
  CAL_UMALQURA                    = 23; // UmAlQura Hijri (Arabic Lunar) calendar }
  {$EXTERNALSYM CAL_UMALQURA}

  CAL_SSHORTESTDAYNAME1   = $00000060;  // Windows Vista or later: Short native name of the first day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME1}
  CAL_SSHORTESTDAYNAME2   = $00000061;  // Windows Vista or later: Short native name of the second day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME2}
  CAL_SSHORTESTDAYNAME3   = $00000062;  // Windows Vista or later: Short native name of the third day of the week.\
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME3}
  CAL_SSHORTESTDAYNAME4   = $00000063;  // Windows Vista or later: Short native name of the fourth day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME4}
  CAL_SSHORTESTDAYNAME5   = $00000064;  // Windows Vista or later: Short native name of the fifth day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME5}
  CAL_SSHORTESTDAYNAME6   = $00000065;  // Windows Vista or later: Short native name of the sixth day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME6}
  CAL_SSHORTESTDAYNAME7   = $00000066;  // Windows Vista or later: Short native name of the seventh day of the week.
  {$EXTERNALSYM CAL_SSHORTESTDAYNAME7}

  CAL_RETURN_NUMBER     = $20000000;  // Windows 98/Me, Windows 2000 and later: Returns the result from GetCalendarInfo as a number instead of a string. This is only valid for CALTYPES beginning with CAL_I.
  {$EXTERNALSYM CAL_RETURN_NUMBER}
  CAL_ITWODIGITYEARMAX  = $00000030;  // Windows 98/Me, Windows 2000 and later: An integer value indicating the upper boundary of the two-digit year range.
  {$EXTERNALSYM CAL_ITWODIGITYEARMAX}
  CAL_SYEARMONTH        = $0000002F;  // Windows 98/Me, Windows 2000 and later: Enumerates the year/month formats for the specified calendars.
  {$EXTERNALSYM CAL_SYEARMONTH}

type
  { TcxGregorianCalendarTable }

  TcxGregorianCalendarTable = class(TcxCustomCalendarTable)
  private
    FDefaultEra: TcxEra;
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMaxSupportedDate: TDateTime; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function FromDateTime(ADate: TDateTime): TcxDateTime; overload; override;
    function GetFirstWeekDay: Byte; override;
    function GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
      AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal; overload; override;
    function GetDaysInMonth(AEra: Integer; AYear, AMonth: Cardinal): Cardinal; override;
    function GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function GetFullWeeksInYear(AYear: Cardinal): Cardinal; override;
    function GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function IsLeapDay(AEra: Integer; AYear, AMonth, ADay: Cardinal): Boolean; override;
    function IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean; override;
    function IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean; override;
    function ToDateTime(ADateTime: TcxDateTime): TDateTime; overload; override;
  end;

  { TcxGregorianUSEnglishCalendarTable }

  TcxGregorianUSEnglishCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarID: CALID; override;
  end;

  { TcxGregorianMiddleEastFrenchCalendarTable }

  TcxGregorianMiddleEastFrenchCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarID: CALID; override;
  public
    function GetFirstWeekDay: Byte; override;
  end;

  { TcxGregorianArabicCalendarTable }

  TcxGregorianArabicCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarID: CALID; override;
  public
    function GetFirstWeekDay: Byte; override;
  end;

  { TcxGregorianTransliteratedEnglishCalendarTable }

  TcxGregorianTransliteratedEnglishCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarID: CALID; override;
  public
    function GetFirstWeekDay: Byte; override;
  end;

  { TcxGregorianTransliteratedFrenchCalendarTable }

  TcxGregorianTransliteratedFrenchCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarID: CALID; override;
  public
    function GetFirstWeekDay: Byte; override;
  end;

  { TcxJapaneseCalendarTable }

  TcxJapaneseCalendarTable = class(TcxGregorianCalendarTable)
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMaxSupportedDate: TDateTime; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;

    function FromDateTime(ADate: TDateTime): TcxDateTime; overload; override;
    function GetFirstWeekDay: Byte; override;
    function ToDateTime(ADateTime: TcxDateTime): TDateTime; overload; override;
  end;

  { TcxTaiwanCalendarTable }

  TcxTaiwanCalendarTable = class(TcxJapaneseCalendarTable)
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
  end;

  { TcxKoreanCalendarTable }

  TcxKoreanCalendarTable = class(TcxJapaneseCalendarTable)
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
  end;

  { TcxHijriCalendarTable }

  TcxHijriCalendarTable = class(TcxCustomCalendarTable)
  private
    FDefaultEra: TcxEra;
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMaxSupportedDate: TDateTime; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function FromDateTime(ADate: TDateTime): TcxDateTime; overload; override;
    function GetFirstWeekDay: Byte; override;
    function GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
      AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal; overload; override;
    function GetDaysInMonth(AEra: Integer; AYear, AMonth: Cardinal): Cardinal; override;
    function GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function GetFullWeeksInYear(AYear: Cardinal): Cardinal; override;
    function GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function IsLeapDay(AEra: Integer; AYear, AMonth, ADay: Cardinal): Boolean; override;
    function IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean; override;
    function IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean; override;
    function ToDateTime(ADateTime: TcxDateTime): TDateTime; overload; override;

    function GetMonthNumber(AYear: Integer; const S: string): Integer; override;
  end;

  { TcxThaiCalendarTable }

  TcxThaiCalendarTable = class(TcxJapaneseCalendarTable)
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
  end;

  { TcxHebrewCalendar }

  TcxHebrewCalendarTableTypeYear = (hctyDeficient = 1, hctyNormal = 2, hctyPerfect = 3);

  TcxHebrewCalendarTable = class(TcxCustomCalendarTable)
  private
    FDefaultEra: TcxEra;
    function GetDayDifference(ALunarYearType, AMonth, ADay,
      ALunarMonth, ALunarDay: Integer): Integer;
    function HebrewNumber(const S: string): Integer;
    procedure GetLunarMonthDay(AYear: Integer; var ADate: TcxDate);
  protected
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; override;
    function GetCalendarID: TcxCALID; override;
    function GetDefaultEra: TcxEra; override;
    function GetMaxSupportedDate: TDateTime; override;
    function GetMinSupportedDate: TDateTime; override;
    function GetMaxSupportedYear: Integer; override;
    function GetMinSupportedYear: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function FromDateTime(ADate: TDateTime): TcxDateTime; overload; override;
    function GetFirstWeekDay: Byte; override;
    function GetYearType(AYear: Cardinal): TcxHebrewCalendarTableTypeYear;
    function GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
      AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal; overload; override;
    function GetDaysInMonth(AEra: Integer; AYear, AMonth: Cardinal): Cardinal; override;
    function GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function GetFullWeeksInYear(AYear: Cardinal): Cardinal; override;
    function GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal; override;
    function IsLeapDay(AEra: Integer; AYear, AMonth, ADay: Cardinal): Boolean; override;
    function IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean; override;
    function IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean; override;
    function ToDateTime(ADateTime: TcxDateTime): TDateTime; overload; override;

    function GetDayNumber(const S: string): Integer; override;
    function GetMonthNumber(AYear: Integer; const S: string): Integer; override;
    function GetYearNumber(const S: string): Integer; override;
  end;

function cxGetCalendarClass(ACalendType: CALTYPE): TcxCustomCalendarTableClass;
function cxIsGregorianCalendar(ACalendar: TcxCustomCalendarTable = nil): Boolean;

implementation

uses
  SysUtils, Classes, DateUtils, dxCore, dxCoreClasses;

const
  cxHebrewTable: array [0..1315] of Integer =
   (7,3,17,3,                 // 1583-1584  (Hebrew year: 5343 - 5344)
    0,4,11,2,21,6,1,3,13,2,   // 1585-1589
    25,4,5,3,16,2,27,6,9,1,   // 1590-1594
    20,2,0,6,11,3,23,4,4,2,   // 1595-1599
    14,3,27,4,8,2,18,3,28,6,  // 1600
    11,1,22,5,2,3,12,3,25,4,  // 1605
    6,2,16,3,26,6,8,2,20,1,   // 1610
    0,6,11,2,24,4,4,3,15,2,   // 1615
    25,6,8,1,19,2,29,6,9,3,   // 1620
    22,4,3,2,13,3,25,4,6,3,   // 1625
    17,2,27,6,7,3,19,2,31,4,  // 1630
    11,3,23,4,5,2,15,3,25,6,  // 1635
    6,2,19,1,29,6,10,2,22,4,  // 1640
    3,3,14,2,24,6,6,1,17,3,   // 1645
    28,5,8,3,20,1,32,5,12,3,  // 1650
    22,6,4,1,16,2,26,6,6,3,   // 1655
    17,2,0,4,10,3,22,4,3,2,   // 1660
    14,3,24,6,5,2,17,1,28,6,  // 1665
    9,2,19,3,31,4,13,2,23,6,  // 1670
    3,3,15,1,27,5,7,3,17,3,   // 1675
    29,4,11,2,21,6,3,1,14,2,  // 1680
    25,6,5,3,16,2,28,4,9,3,   // 1685
    20,2,0,6,12,1,23,6,4,2,   // 1690
    14,3,26,4,8,2,18,3,0,4,   // 1695
    10,3,21,5,1,3,13,1,24,5,  // 1700
    5,3,15,3,27,4,8,2,19,3,   // 1705
    29,6,10,2,22,4,3,3,14,2,  // 1710
    26,4,6,3,18,2,28,6,10,1,  // 1715
    20,6,2,2,12,3,24,4,5,2,   // 1720
    16,3,28,4,8,3,19,2,0,6,   // 1725
    12,1,23,5,3,3,14,3,26,4,  // 1730
    7,2,17,3,28,6,9,2,21,4,   // 1735
    1,3,13,2,25,4,5,3,16,2,   // 1740
    27,6,9,1,19,3,0,5,11,3,   // 1745
    23,4,4,2,14,3,25,6,7,1,   // 1750
    18,2,28,6,9,3,21,4,2,2,   // 1755
    12,3,25,4,6,2,16,3,26,6,  // 1760
    8,2,20,1,0,6,11,2,22,6,   // 1765
    4,1,15,2,25,6,6,3,18,1,   // 1770
    29,5,9,3,22,4,2,3,13,2,   // 1775
    23,6,4,3,15,2,27,4,7,3,   // 1780
    19,2,31,4,11,3,21,6,3,2,  // 1785
    15,1,25,6,6,2,17,3,29,4,  // 1790
    10,2,20,6,3,1,13,3,24,5,  // 1795
    4,3,16,1,27,5,7,3,17,3,   // 1800
    0,4,11,2,21,6,1,3,13,2,   // 1805
    25,4,5,3,16,2,29,4,9,3,   // 1810
    19,6,30,2,13,1,23,6,4,2,  // 1815
    14,3,27,4,8,2,18,3,0,4,   // 1820
    11,3,22,5,2,3,14,1,26,5,  // 1825
    6,3,16,3,28,4,10,2,20,6,  // 1830
    30,3,11,2,24,4,4,3,15,2,  // 1835
    25,6,8,1,19,2,29,6,9,3,   // 1840
    22,4,3,2,13,3,25,4,7,2,   // 1845
    17,3,27,6,9,1,21,5,1,3,   // 1850
    11,3,23,4,5,2,15,3,25,6,  // 1855
    6,2,19,1,29,6,10,2,22,4,  // 1860
    3,3,14,2,24,6,6,1,18,2,   // 1865
    28,6,8,3,20,4,2,2,12,3,   // 1870
    24,4,4,3,16,2,26,6,6,3,   // 1875
    17,2,0,4,10,3,22,4,3,2,   // 1880
    14,3,24,6,5,2,17,1,28,6,  // 1885
    9,2,21,4,1,3,13,2,23,6,   // 1890
    5,1,15,3,27,5,7,3,19,1,   // 1895
    0,5,10,3,22,4,2,3,13,2,   // 1900
    24,6,4,3,15,2,27,4,8,3,   // 1905
    20,4,1,2,11,3,22,6,3,2,   // 1910
    15,1,25,6,7,2,17,3,29,4,  // 1915
    10,2,21,6,1,3,13,1,24,5,  // 1920
    5,3,15,3,27,4,8,2,19,6,   // 1925
    1,1,12,2,22,6,3,3,14,2,   // 1930
    26,4,6,3,18,2,28,6,10,1,  // 1935
    20,6,2,2,12,3,24,4,5,2,   // 1940
    16,3,28,4,9,2,19,6,30,3,  // 1945
    12,1,23,5,3,3,14,3,26,4,  // 1950
    7,2,17,3,28,6,9,2,21,4,   // 1955
    1,3,13,2,25,4,5,3,16,2,   // 1960
    27,6,9,1,19,6,30,2,11,3,  // 1965
    23,4,4,2,14,3,27,4,7,3,   // 1970
    18,2,28,6,11,1,22,5,2,3,  // 1975
    12,3,25,4,6,2,16,3,26,6,  // 1980
    8,2,20,4,30,3,11,2,24,4,  // 1985
    4,3,15,2,25,6,8,1,18,3,   // 1990
    29,5,9,3,22,4,3,2,13,3,   // 1995
    23,6,6,1,17,2,27,6,7,3,   // 2000 - 2004
    20,4,1,2,11,3,23,4,5,2,   // 2005 - 2009
    15,3,25,6,6,2,19,1,29,6,  // 2010
    10,2,20,6,3,1,14,2,24,6,  // 2015
    4,3,17,1,28,5,8,3,20,4,   // 2020
    1,3,12,2,22,6,2,3,14,2,   // 2025
    26,4,6,3,17,2,0,4,10,3,   // 2030
    20,6,1,2,14,1,24,6,5,2,   // 2035
    15,3,28,4,9,2,19,6,1,1,   // 2040
    12,3,23,5,3,3,15,1,27,5,  // 2045
    7,3,17,3,29,4,11,2,21,6,  // 2050
    1,3,12,2,25,4,5,3,16,2,   // 2055
    28,4,9,3,19,6,30,2,12,1,  // 2060
    23,6,4,2,14,3,26,4,8,2,   // 2065
    18,3,0,4,10,3,22,5,2,3,   // 2070
    14,1,25,5,6,3,16,3,28,4,  // 2075
    9,2,20,6,30,3,11,2,23,4,  // 2080
    4,3,15,2,27,4,7,3,19,2,   // 2085
    29,6,11,1,21,6,3,2,13,3,  // 2090
    25,4,6,2,17,3,27,6,9,1,   // 2095
    20,5,30,3,10,3,22,4,3,2,  // 2100
    14,3,24,6,5,2,17,1,28,6,  // 2105
    9,2,21,4,1,3,13,2,23,6,   // 2110
    5,1,16,2,27,6,7,3,19,4,   // 2115
    30,2,11,3,23,4,3,3,14,2,  // 2120
    25,6,5,3,16,2,28,4,9,3,   // 2125
    21,4,2,2,12,3,23,6,4,2,   // 2130
    16,1,26,6,8,2,20,4,30,3,  // 2135
    11,2,22,6,4,1,14,3,25,5,  // 2140
    6,3,18,1,29,5,9,3,22,4,   // 2145
    2,3,13,2,23,6,4,3,15,2,   // 2150
    27,4,7,3,20,4,1,2,11,3,   // 2155
    21,6,3,2,15,1,25,6,6,2,   // 2160
    17,3,29,4,10,2,20,6,3,1,  // 2165
    13,3,24,5,4,3,17,1,28,5,  // 2170
    8,3,18,6,1,1,12,2,22,6,   // 2175
    2,3,14,2,26,4,6,3,17,2,   // 2180
    28,6,10,1,20,6,1,2,12,3,  // 2185
    24,4,5,2,15,3,28,4,9,2,   // 2190
    19,6,33,3,12,1,23,5,3,3,  // 2195
    13,3,25,4,6,2,16,3,26,6,  // 2200
    8,2,20,4,30,3,11,2,24,4,  // 2205
    4,3,15,2,25,6,8,1,18,6,   // 2210
    33,2,9,3,22,4,3,2,13,3,   // 2215
    25,4,6,3,17,2,27,6,9,1,   // 2220
    21,5,1,3,11,3,23,4,5,2,   // 2225
    15,3,25,6,6,2,19,4,33,3,  // 2230
    10,2,22,4,3,3,14,2,24,6,  // 2235
    6,1);                     // 2240 (Hebrew year: 6000)

  cxHebrewLunarMonthLen: array [0..6,0..13] of Integer = (
    (0,00,00,00,00,00,00,00,00,00,00,00,00,0),
    (0,30,29,29,29,30,29,30,29,30,29,30,29,0),     // 3 common year variations
    (0,30,29,30,29,30,29,30,29,30,29,30,29,0),
    (0,30,30,30,29,30,29,30,29,30,29,30,29,0),
    (0,30,29,29,29,30,30,29,30,29,30,29,30,29),    // 3 leap year variations
    (0,30,29,30,29,30,30,29,30,29,30,29,30,29),
    (0,30,30,30,29,30,30,29,30,29,30,29,30,29));

  cxHebrewYearOf1AD = 3760;
  cxHebrewFirstGregorianTableYear = 1583;
  cxHebrewLastGregorianTableYear = 2239;
  cxHebrewTableYear = cxHebrewLastGregorianTableYear - cxHebrewFirstGregorianTableYear;

{ TcxGregorianCalendarTable }

constructor TcxGregorianCalendarTable.Create;
begin
  inherited Create;
  FDefaultEra := TcxEra.Create(-1, NullDate, 0, 1, 9999);
end;

destructor TcxGregorianCalendarTable.Destroy;
begin
  FreeAndNil(FDefaultEra);
  inherited Destroy;
end;

function TcxGregorianCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catSolarCalendar;
end;

function TcxGregorianCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_GREGORIAN;
end;

function TcxGregorianCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := FDefaultEra;
end;

function TcxGregorianCalendarTable.GetMaxSupportedDate: TDateTime;
begin
  Result := MaxDateTime;
end;

function TcxGregorianCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := MinDateTime;
end;

function TcxGregorianCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := dxMaxYear;
end;

function TcxGregorianCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := dxMinYear;
end;

function TcxGregorianCalendarTable.FromDateTime(ADate: TDateTime): TcxDateTime;
var
  Y, M, D: Word;
  H, MN, S, MS: Word;
begin
  DecodeDateTime(ADate, Y, M, D, H, MN, S, MS);
  with Result do
  begin
    Year := Y;
    Month := M;
    Day := D;
    Hours := H;
    Minutes := MN;
    Seconds := S;
    Milliseconds := MS;
  end;
end;

function TcxGregorianCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := Byte(dSunday);
end;

function TcxGregorianCalendarTable.GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
  AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal;
begin
  Result := dxGetWeekNumber(ToDateTime(ADate), AStartOfWeek, AFirstWeekOfYear);
end;

function TcxGregorianCalendarTable.GetDaysInMonth(AEra: Integer; AYear,
  AMonth: Cardinal): Cardinal;
begin
  case AMonth of
    2:
      begin
        if IsLeapYear(AEra, AYear) then
          Result := 29
        else
          Result := 28;
      end;
    4, 6, 9, 11:
      Result := 30;
    else
      Result := 31;
  end;
end;

function TcxGregorianCalendarTable.GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  if IsLeapYear(AEra, AYear) then
    Result := 366
  else
    Result := 365;
end;

function TcxGregorianCalendarTable.GetFullWeeksInYear(AYear: Cardinal): Cardinal;
var
  ADate: TcxDateTime;
  ADay: Integer;
begin
  Result := 52;
  ADate.Year := AYear;
  ADate.Month := 1;
  ADate.Day := 1;
  ADate.Hours := 0;
  ADate.Minutes := 0;
  ADate.Seconds := 0;
  ADate.Milliseconds := 0;
  ADay := GetWeekDay(ADate) - GetFirstWeekDay;
  if ADay < 0 then
    Inc(ADay, 7);
  if (IsLeapYear(AYear) and (ADay >= 5)) or (ADay >= 6) then
    Result := 53;
end;

function TcxGregorianCalendarTable.GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  Result := 12;
end;

function TcxGregorianCalendarTable.IsLeapDay(AEra: Integer; AYear, AMonth,
  ADay: Cardinal): Boolean;
begin
  Result := IsLeapMonth(AEra, AYear, ADay) and (ADay = 29);
end;

function TcxGregorianCalendarTable.IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean;
begin
  Result := IsLeapYear(AEra, AYear) and (AMonth = 2);
end;

function TcxGregorianCalendarTable.IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean;
begin
  YearToGregorianYear(AYear, AEra);
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function TcxGregorianCalendarTable.ToDateTime(ADateTime: TcxDateTime): TDateTime;
begin
  with ADateTime do
    Result := EncodeDateTime(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds);
end;

{ TcxGregorianUSEnglishCalendarTable }

function TcxGregorianUSEnglishCalendarTable.GetCalendarID: CALID;
begin
  Result := CAL_GREGORIAN_US;
end;

{ TcxGregorianMiddleEastFrenchCalendarTable }

function TcxGregorianMiddleEastFrenchCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := Byte(dMonday);
end;

function TcxGregorianMiddleEastFrenchCalendarTable.GetCalendarID: CALID;
begin
  Result := CAL_GREGORIAN_ME_FRENCH;
end;

{ TcxGregorianArabicCalendarTable }

function TcxGregorianArabicCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := Byte(dSaturday);
end;

function TcxGregorianArabicCalendarTable.GetCalendarID: CALID;
begin
  Result := CAL_GREGORIAN_ARABIC;
end;

{ TcxGregorianTransliteratedEnglishCalendarTable }

function TcxGregorianTransliteratedEnglishCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := Byte(dMonday);
end;

function TcxGregorianTransliteratedEnglishCalendarTable.GetCalendarID: CALID;
begin
  Result := CAL_GREGORIAN_XLIT_ENGLISH;
end;

{ TcxGregorianTransliteratedFrenchCalendarTable }

function TcxGregorianTransliteratedFrenchCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := Byte(dMonday);
end;

function TcxGregorianTransliteratedFrenchCalendarTable.GetCalendarID: CALID;
begin
  Result := CAL_GREGORIAN_XLIT_FRENCH;
end;

{ TcxJapaneseCalendarTable }

constructor TcxJapaneseCalendarTable.Create;
begin
  FEras := TcxEras.Create;
  FEras.Add(TcxEra.Create(1, EncodeDate(1868, 9, 8), 1867, 1, 1912 - 1867));    // Meiji
  FEras.Add(TcxEra.Create(2, EncodeDate(1912, 7, 30), 1911, 1, 1926 - 1911));   // Taisho
  FEras.Add(TcxEra.Create(3, EncodeDate(1926, 12, 25), 1925, 1, 1989 - 1925));  // Showa
  FEras.Add(TcxEra.Create(4, EncodeDate(1989, 1, 8), 1988, 1, 9999 - 1988));    // Heisei. Most recent
end;

function TcxJapaneseCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catSolarCalendar;
end;

function TcxJapaneseCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_JAPAN;
end;

function TcxJapaneseCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := Eras[3];
end;

function TcxJapaneseCalendarTable.GetMaxSupportedDate: TDateTime;
begin
  Result := EncodeDate(9999, 12, 31);
end;

function TcxJapaneseCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := EncodeDate(1868, 09, 08);
end;

function TcxJapaneseCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := 8011; // Heisei 8011/12/31
end;

function TcxJapaneseCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := 1;
end;

function TcxJapaneseCalendarTable.FromDateTime(ADate: TDateTime): TcxDateTime;
var
  Y, M, D, H, Mn, S, MS: Word;
begin
  DecodeDate(ADate, Y, M, D);
  DecodeTime(ADate, H, Mn, S, MS);
  with Result do
  begin
    Era := GetEra(Y, M, D);
    if Era = -1 then
      Year := Y - DefaultEra.YearOffset
    else
      Year := Y - Eras[Era].YearOffset;
    Month := M;
    Day := D;
    Hours := H;
    Minutes := Mn;
    Seconds := S;
    Milliseconds := MS;
  end;
end;

function TcxJapaneseCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := 0;
end;

function TcxJapaneseCalendarTable.ToDateTime(ADateTime: TcxDateTime): TDateTime;
var
  AYear: Cardinal;
begin
  with ADateTime do
  begin
    if IsNotValid(ADateTime, Result) then
      Exit;
    AYear := Year;
    YearToGregorianYear(AYear, Era);
    Result := EncodeDate(AYear, Month, Day) +
      EncodeTime(Hours, Minutes, Seconds, Milliseconds);
  end;
end;

{ TcxTaiwanCalendarTable }

constructor TcxTaiwanCalendarTable.Create;
begin
  FEras := TcxEras.Create;
  FEras.Add(TcxEra.Create(1, EncodeDate(1912, 1, 1), 1911, 1, 9999 - 1911)); //
end;

function TcxTaiwanCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catSolarCalendar;
end;

function TcxTaiwanCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_TAIWAN;
end;

function TcxTaiwanCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := FEras[0];
end;

function TcxTaiwanCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := DefaultEra.StartDate;
end;

function TcxTaiwanCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := DefaultEra.MaxEraYear;
end;

function TcxTaiwanCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := DefaultEra.MinEraYear;
end;

{ TcxKoreanCalendarTable }

constructor TcxKoreanCalendarTable.Create;
begin
  FEras := TcxEras.Create;
  FEras.Add(TcxEra.Create(0, EncodeDate(1, 1, 1), -2333, 2334, 9999 + 2333));
end;

function TcxKoreanCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catSolarCalendar;
end;

function TcxKoreanCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_KOREA;
end;

function TcxKoreanCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := Eras[0];
end;

function TcxKoreanCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := DefaultEra.StartDate;
end;

function TcxKoreanCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := DefaultEra.MaxEraYear;
end;

function TcxKoreanCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := DefaultEra.MinEraYear;
end;

{ TcxHijriCalendarTable }

constructor TcxHijriCalendarTable.Create;
begin
  inherited Create;
  FDefaultEra := TcxEra.Create(-1, NullDate, 0, 1, 9999);
end;

destructor TcxHijriCalendarTable.Destroy;
begin
  FreeAndNil(FDefaultEra);
  inherited Destroy;
end;

function TcxHijriCalendarTable.FromDateTime(
  ADate: TDateTime): TcxDateTime;
var
  I: Integer;
  H, M, S, MS: Word;
  ACountDays: Integer;
  ACurrentYear: Integer;
begin
  with Result do
  begin
    DecodeTime(ADate, H, M, S, MS);
    Hours := H;
    Minutes := M;
    Seconds := S;
    Milliseconds := MS;
    ACountDays := Trunc(ADate - EncodeDate(1, 1, 1) + 1) - 227013;
    Year := ((ACountDays * 30) div 10631) + 1;
    ACurrentYear := Year;
    ACountDays := ACountDays - (ACurrentYear - 1) * 354;
    ACountDays := ACountDays - ((ACurrentYear - 1) div 30) * 11;
    for I := 1 to (ACurrentYear - 1) mod 30 do
      if IsLeapYear(I) then
        ACountDays := ACountDays - 1;
    Year := ACurrentYear;
    Month := 1;
    while ACountDays > Integer(GetDaysInMonth(Year, Month)) do
    begin
      ACountDays := ACountDays - Integer(GetDaysInMonth(Year, Month));
      Month := Month + 1;
      if Month > GetMonthsInYear(Year) then
      begin
        Month := 1;
        Year := Year + 1;
      end;
    end;
    if ACountDays = 0 then
    begin
      Year := Year - 1;
      Month := GetMonthsInYear(Year);
      Day := GetDaysInMonth(Year, Month);
    end
    else
      Day := ACountDays;
  end;
end;

function TcxHijriCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catLunarCalendar;
end;

function TcxHijriCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_HIJRI;
end;

function TcxHijriCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := FDefaultEra;
end;

function TcxHijriCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := 6;
end;

function TcxHijriCalendarTable.GetDaysInMonth(AEra: Integer; AYear,
  AMonth: Cardinal): Cardinal;
begin
  if IsLeapMonth(AYear, AMonth) then
    Result := 30
  else
    if AMonth in [2, 4, 6, 8, 10, 12] then
      Result := 29
    else
      Result := 30;
end;

function TcxHijriCalendarTable.GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  if IsLeapYear(AYear) then
    Result := 355
  else
    Result := 354;
end;

function TcxHijriCalendarTable.GetFullWeeksInYear(
  AYear: Cardinal): Cardinal;
begin
  Result := 50;
end;

function TcxHijriCalendarTable.GetMaxSupportedDate: TDateTime;
begin
  Result := EncodeDate(9666, 4, 3);
end;

function TcxHijriCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := EncodeDate(0622, 7, 16);
end;

function TcxHijriCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := 9666;
end;

function TcxHijriCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := 1;
end;

function TcxHijriCalendarTable.GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  Result := 12;
end;

function TcxHijriCalendarTable.GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
  AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal;
var
  AStartWeekDate: TDateTime;
  AStart: TDateTime;
  ATmpDate: TcxDateTime;
begin
  if AFirstWeekOfYear = fwySystem then
    AFirstWeekOfYear := TcxFirstWeekOfYear(
      StrToInt(dxGetLocaleInfo(GetThreadLocale, LOCALE_IFIRSTWEEKOFYEAR, '0')) + 1);
  with ATmpDate do
  begin
    Year := ADate.Year;
    Month := 1;
    Day := 1;
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
    Milliseconds := 0;
  end;
  AStart := GetFirstDayOfWeek(ToDateTime(ATmpDate));
  case AFirstWeekOfYear of
    fwyFirstFourDays:
      if FromDateTime(AStart + 3).Year < ADate.Year then AStart := AStart + 7;
    fwyFirstFullWeek:
      if FromDateTime(AStart).Year < ADate.Year then AStart := AStart + 7;
  end;
  Result := Trunc(Trunc(ToDateTime(ADate)) - AStart) div 7 + 1;
  if ADate.Month = GetMonthsInYear(ADate.Year) then
  begin
    AStartWeekDate := ToDateTime(GetFirstDayOfWeek(ADate));
    case AFirstWeekOfYear of
      fwyJan1:
        if FromDateTime(AStartWeekDate + 6).Month = 1 then
          Result := 1;
      fwyFirstFourDays:
        if FromDateTime(AStartWeekDate + 3).Month = 1 then
          Result := 1;
    end;
  end;
end;

function TcxHijriCalendarTable.IsLeapDay(AEra: Integer; AYear, AMonth,
  ADay: Cardinal): Boolean;
begin
  Result := IsLeapMonth(AYear, AMonth) and (ADay = 30);
end;

function TcxHijriCalendarTable.IsLeapMonth(AEra: Integer; AYear,
  AMonth: Cardinal): Boolean;
begin
  Result := IsLeapYear(AYear) and (AMonth = 12);
end;

function TcxHijriCalendarTable.IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean;
begin
  Result := (AYear mod 30) in [2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29];
end;

function TcxHijriCalendarTable.ToDateTime(
  ADateTime: TcxDateTime): TDateTime;
var
  I: Integer;
begin
  Result := 227013;
  Result := Result + (ADateTime.Year - 1) * 354;
  Result := Result + ((ADateTime.Year - 1) div 30) * 11;
  for I := 1 to (ADateTime.Year - 1) mod 30 do
    if IsLeapYear(I) then
      Result := Result + 1;
  Result := Result + GetDayOfYear(ADateTime);
  Result := Result + EncodeDate(1, 1, 1) - 1;
  Result := Result + EncodeTime(ADateTime.Hours, ADateTime.Minutes, ADateTime.Seconds, ADateTime.Milliseconds);
end;

function TcxHijriCalendarTable.GetMonthNumber(AYear: Integer; const S: string): Integer;
var
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    if AnsiCompareText(S, cxGetLocalMonthName(AYear, I, Self)) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := inherited GetMonthNumber(AYear, S);
end;

{ TcxThaiCalendarTable }

constructor TcxThaiCalendarTable.Create;
begin
  FEras := TcxEras.Create;
  FEras.Add(TcxEra.Create(0, EncodeDate(1, 1, 1), -543, 544, 9999 + 543));
end;

function TcxThaiCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catSolarCalendar;
end;

function TcxThaiCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_THAI;
end;

function TcxThaiCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := Eras[0];
end;

function TcxThaiCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := DefaultEra.StartDate;
end;

function TcxThaiCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := DefaultEra.MaxEraYear;
end;

function TcxThaiCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := DefaultEra.MinEraYear;
end;

{ TcxHebrewCalendarTable }

constructor TcxHebrewCalendarTable.Create;
begin
  inherited Create;
  FDefaultEra := TcxEra.Create(-1, NullDate, 0, 1, 9999);
end;

destructor TcxHebrewCalendarTable.Destroy;
begin
  FreeAndNil(FDefaultEra);
  inherited Destroy;
end;

function TcxHebrewCalendarTable.FromDateTime(ADate: TDateTime): TcxDateTime;
var
  AYear, AMonth, ADay: Word;
  H, M, S, MS: Word;
  ADays: Integer;
begin
  if ADate > MaxSupportedDate then
  begin
    Result := FromDateTime(MaxSupportedDate);
    Exit;
  end;
  if ADate < MinSupportedDate then
  begin
    Result := FromDateTime(MinSupportedDate);
    Exit;
  end;
  DecodeDateTime(ADate, AYear, AMonth, ADay, H, M, S, MS);
  AYear := AYear + 3760;
  AMonth := 1;
  ADays := Trunc(ADate - ToDateTime(AYear, AMonth, 1)) + 1;
  with Result do
  begin
    Year := AYear;
    Month := AMonth;
    Day := 1;
    Hours := H;
    Minutes := M;
    Seconds := S;
    Milliseconds := MS;
  end;
  if ADays = 0 then
    Exit;
  while (ADays < 0) or (ADays > Integer(GetDaysInMonth(AYear, AMonth))) do
  begin
    if ADays < 0 then
    begin
      Dec(AMonth);
      if AMonth <= 0 then
      begin
        Dec(AYear);
        AMonth := GetMonthsInYear(AYear);
      end;
    end
    else
    begin
      Inc(AMonth);
      if AMonth > GetMonthsInYear(AYear) then
      begin
        AMonth := 1;
        Inc(AYear);
      end;
    end;
    ADays := Trunc(ADate - ToDateTime(AYear, AMonth, 1)) + 1;
  end;
  with Result do
  begin
    Year := AYear;
    Month := AMonth;
    Day := ADays;
  end;
end;

function TcxHebrewCalendarTable.GetCalendarAlgorithmType: TcxCalendarAlgorithmType;
begin
  Result := catLunarSolarCalendar;
end;

function TcxHebrewCalendarTable.GetCalendarID: TcxCALID;
begin
  Result := CAL_HEBREW;
end;

function TcxHebrewCalendarTable.GetDefaultEra: TcxEra;
begin
  Result := FDefaultEra;
end;

function TcxHebrewCalendarTable.GetDaysInMonth(AEra: Integer; AYear,
  AMonth: Cardinal): Cardinal;
var
  AYearIndex: Integer;
begin
  if not IsValidMonth(AYear, AMonth) then
  begin
    Result := 0;
    Exit;
  end;
  AYearIndex := Integer(GetYearType(AYear));
  if IsLeapYear(AYear) then
    Inc(AYearIndex, 3);
  Result := cxHebrewLunarMonthLen[AYearIndex, AMonth];
end;

function TcxHebrewCalendarTable.GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  if not IsValidYear(AYear) then
  begin
    Result := 0;
    Exit;
  end;
  Result := 353;
  case GetYearType(AYear) of
    hctyNormal:
      Result := 354;
    hctyPerfect:
      Result := 355;
  end;
  if IsLeapYear(AYear) then
    Inc(Result, 30);
end;

function TcxHebrewCalendarTable.GetFirstWeekDay: Byte;
begin
  Result := 0;
end;

function TcxHebrewCalendarTable.GetYearType(
  AYear: Cardinal): TcxHebrewCalendarTableTypeYear;
var
  AIndex: Integer;
  ATypeYear: Integer;
begin
  AIndex := AYear - cxHebrewFirstGregorianTableYear - cxHebrewYearOf1AD;
  if (AIndex < 0) or (AIndex > cxHebrewTableYear) then
  begin
    Result := TcxHebrewCalendarTableTypeYear(0);
    Exit;
  end;
  AIndex := AIndex * 2 + 1;
  ATypeYear := cxHebrewTable[AIndex];
  if IsLeapYear(AYear) then
    Dec(ATypeYear, 3);
  Result := TcxHebrewCalendarTableTypeYear(ATypeYear);
end;

function TcxHebrewCalendarTable.GetFullWeeksInYear(AYear: Cardinal): Cardinal;
begin
  Result := GetDaysInYear(AYear) div 7;
end;

function TcxHebrewCalendarTable.GetMaxSupportedDate: TDateTime;
begin
  Result := EncodeDateTime(2239, 9, 29, 23, 59, 59, 999);
end;

function TcxHebrewCalendarTable.GetMinSupportedDate: TDateTime;
begin
  Result := EncodeDate(1583, 1, 1);
end;

function TcxHebrewCalendarTable.GetMaxSupportedYear: Integer;
begin
  Result := 5999;
end;

function TcxHebrewCalendarTable.GetMinSupportedYear: Integer;
begin
  Result := 5343;
end;

function TcxHebrewCalendarTable.GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal;
begin
  if IsLeapYear(AYear) then
    Result := 13
  else
    Result := 12;
end;

function TcxHebrewCalendarTable.GetWeekNumber(ADate: TcxDateTime;
  AStartOfWeek: TDay; AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal;
var
  AStartWeekDate: TDateTime;
  AStart: TDateTime;
  ATmpDate: TcxDateTime;
begin
  if AFirstWeekOfYear = fwySystem then
    AFirstWeekOfYear := TcxFirstWeekOfYear(
      StrToInt(dxGetLocaleInfo(GetThreadLocale, LOCALE_IFIRSTWEEKOFYEAR, '0')) + 1);
  with ATmpDate do
  begin
    Year := ADate.Year;
    Month := 1;
    Day := 1;
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
    Milliseconds := 0;
  end;
  AStart := GetFirstDayOfWeek(ToDateTime(ATmpDate));
  case AFirstWeekOfYear of
    fwyFirstFourDays:
      if FromDateTime(AStart + 3).Year < ADate.Year then AStart := AStart + 7;
    fwyFirstFullWeek:
      if FromDateTime(AStart).Year < ADate.Year then AStart := AStart + 7;
  end;
  Result := Trunc(Trunc(ToDateTime(ADate)) - AStart) div 7 + 1;
  if ADate.Month = GetMonthsInYear(ADate.Year) then
  begin
    AStartWeekDate := ToDateTime(GetFirstDayOfWeek(ADate));
    case AFirstWeekOfYear of
      fwyJan1:
        if FromDateTime(AStartWeekDate + 6).Month = 1 then
          Result := 1;
      fwyFirstFourDays:
        if FromDateTime(AStartWeekDate + 3).Month = 1 then
          Result := 1;
    end;
  end;
end;

function TcxHebrewCalendarTable.IsLeapDay(AEra: Integer; AYear, AMonth,
  ADay: Cardinal): Boolean;
begin
  Result := IsValidDay(AYear, AMonth, ADay) and IsLeapMonth(AYear, AMonth);
end;

function TcxHebrewCalendarTable.IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean;
begin
  Result := IsValidMonth(AYear, AMonth) and IsLeapYear(AYear) and (AMonth = 7);
end;

function TcxHebrewCalendarTable.IsLeapYear(AEra: Integer; AYear: Cardinal): Boolean;
var
  AIndex: Integer;
begin
  AIndex := AYear - cxHebrewFirstGregorianTableYear - cxHebrewYearOf1AD;
  if (AIndex < 0) or (AIndex > cxHebrewTableYear) then
  begin
    Result := False;
    Exit;
  end;
  AIndex := AIndex * 2 + 1;
  Result := cxHebrewTable[AIndex] >= 4;
end;

function TcxHebrewCalendarTable.ToDateTime(ADateTime: TcxDateTime): TDateTime;
var
  AYear, ADays: Integer;
  ALunarDate: TcxDate;
  ALunarYearType: Integer;
begin
  if IsNotValid(ADateTime, Result) then
    Exit;
  ALunarYearType := Integer(GetYearType(ADateTime.Year));
  if IsLeapYear(ADateTime.Year) then
    Inc(ALunarYearType, 3);
  AYear := ADateTime.Year - cxHebrewYearOf1AD;
  GetLunarMonthDay(AYear, ALunarDate);
  Result := EncodeDateTime(AYear, 1, 1, ADateTime.Hours, ADateTime.Minutes,
    ADateTime.Seconds, ADateTime.Milliseconds);
  if (ADateTime.Month = ALunarDate.Month) and (ADateTime.Day = ALunarDate.Day) then
    Exit;
  ADays := GetDayDifference(ALunarYearType, ADateTime.Month, ADateTime.Day,
    ALunarDate.Month, ALunarDate.Day);
  Result := Result + ADays;
end;

function TcxHebrewCalendarTable.GetDayNumber(const S: string): Integer;
begin
  Result := HebrewNumber(S);
  if Result = 0 then
    Result := inherited GetDayNumber(S);
end;

function TcxHebrewCalendarTable.GetMonthNumber(AYear: Integer; const S: string): Integer;
var
  I: Integer;
begin
  Result := HebrewNumber(S);
  if IsValidMonth(AYear, Result) then
    Exit;
  for I := 1 to 13 do
  begin
    if AnsiCompareText(S, cxGetLocalMonthName(AYear, I, Self)) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := inherited GetMonthNumber(AYear, S);
end;

function TcxHebrewCalendarTable.GetYearNumber(const S: string): Integer;
begin
  Result := HebrewNumber(S);
  if Result = 0 then
    Result := inherited GetYearNumber(S)
  else
    Result := Result + 5000;
end;

function TcxHebrewCalendarTable.GetDayDifference(ALunarYearType, AMonth, ADay,
  ALunarMonth, ALunarDay: Integer): Integer;
var
  ASwap: Boolean;
  ATmpDay, ATmpMonth: Integer;
begin
  if AMonth = ALunarMonth then
  begin
    Result := ADay - ALunarDay;
    Exit;
  end;
  ASwap := AMonth > ALunarMonth;
  if ASwap then
  begin
    ATmpMonth := AMonth;
    AMonth := ALunarMonth;
    ALunarMonth := ATmpMonth;
    ATmpDay := ADay;
    ADay := ALunarDay;
    ALunarDay := ATmpDay;
  end;
  Result := cxHebrewLunarMonthLen[ALunarYearType, AMonth] - ADay;
  Inc(AMonth);
  while AMonth < ALunarMonth do
  begin
    Result := Result + cxHebrewLunarMonthLen[ALunarYearType, AMonth];
    Inc(AMonth);
  end;
  Result := Result + ALunarDay;
  if not ASwap then
    Result := - Result;
end;

function TcxHebrewCalendarTable.HebrewNumber(const S: string): Integer;
var
  I, AOrd: Integer;
  ACh: Char;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    ACh := S[I];
    AOrd := Ord(ACh);
    AOrd := AOrd - 223;
    if AOrd <= 0 then
      AOrd := 0;
    case AOrd of
      12:
        AOrd := 20;
      13:
        AOrd := 30;
      14, 15:
        AOrd := 40;
      16, 17:
        AOrd := 50;
      18:
        AOrd := 60;
      19:
        AOrd := 70;
      21, 22:
        AOrd := 80;
      23:
        AOrd := 90;
      24:
        AOrd := 100;
      25:
        AOrd := 200;
      26:
        AOrd := 300;
      27:
        AOrd := 400;
      else
        if AOrd > 10 then
          AOrd := 0;
    end;
    Result := Result + AOrd;
  end;
end;

procedure TcxHebrewCalendarTable.GetLunarMonthDay(AYear: Integer; var ADate: TcxDate);
var
  AIndex: Integer;
begin
  AIndex := AYear - cxHebrewFirstGregorianTableYear;
  AIndex := AIndex * 2;
  ADate.Day := cxHebrewTable[AIndex];
  case (ADate.Day) of
    0:
      begin
        ADate.Day := 1;
        ADate.Month := 5;
      end;
    30:
      ADate.Month := 3;
    31:
      begin
        ADate.Day := 2;
        ADate.Month := 5;
      end;
    32:
      begin
        ADate.Day := 3;
        ADate.Month := 5;
      end;
    33:
      begin
        ADate.Day := 29;
        ADate.Month := 3;
      end;
    else
      ADate.Month := 4;
  end;
end;

function cxGetCalendarClass(ACalendType: CALTYPE): TcxCustomCalendarTableClass;
begin
  case ACalendType of
    CAL_GREGORIAN: Result := TcxGregorianCalendarTable;
    CAL_GREGORIAN_US: Result := TcxGregorianUSEnglishCalendarTable;
    CAL_GREGORIAN_ME_FRENCH: Result := TcxGregorianMiddleEastFrenchCalendarTable;
    CAL_GREGORIAN_ARABIC: Result := TcxGregorianArabicCalendarTable;
    CAL_GREGORIAN_XLIT_ENGLISH: Result := TcxGregorianTransliteratedEnglishCalendarTable;
    CAL_GREGORIAN_XLIT_FRENCH: Result := TcxGregorianTransliteratedFrenchCalendarTable;
    CAL_JAPAN: Result := TcxJapaneseCalendarTable;
    CAL_TAIWAN: Result := TcxTaiwanCalendarTable;
    CAL_KOREA: Result := TcxKoreanCalendarTable;
    CAL_HIJRI: Result := TcxHijriCalendarTable;
    CAL_THAI: Result := TcxThaiCalendarTable;
    CAL_HEBREW: Result := TcxHebrewCalendarTable;
    else
      Result := TcxGregorianCalendarTable;
  end;
end;

function cxIsGregorianCalendar(ACalendar: TcxCustomCalendarTable = nil): Boolean;
var
  ACalType: CALID;
begin
  if ACalendar = nil then
    ACalType := cxGetLocalCalendar.CalendarID
  else
    ACalType := ACalendar.CalendarID;
  Result := ACalType in [CAL_GREGORIAN, CAL_GREGORIAN_US, CAL_GREGORIAN_ME_FRENCH,
    CAL_GREGORIAN_ARABIC, CAL_GREGORIAN_XLIT_ENGLISH, CAL_GREGORIAN_XLIT_FRENCH];
end;

end.
