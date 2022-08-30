{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxCultureInfo;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Generics.Defaults, Generics.Collections,
  cxDateUtils;

type
  PdxCultureInfo = ^TdxCultureInfo;
  PdxFormatSettings = ^TFormatSettings;

  { TdxNumberFormatInfo }

  TdxNumberFormatInfo = record
  strict private
    FFormatSettings: TFormatSettings;
    FNumberGroupSizes: TArray<Integer>;
    function GetNumberDecimalSeparator: Char;
    function GetNumberGroupSeparator: Char;
    procedure Initialize;
    procedure SetNumberDecimalSeparator(const Value: Char);
    procedure SetNumberGroupSeparator(const Value: Char);
  public
    constructor Create(const AFormatSettings: TFormatSettings); overload;

    property FormatSettings: TFormatSettings read FFormatSettings;
    property NumberDecimalSeparator: Char read GetNumberDecimalSeparator write SetNumberDecimalSeparator;
    property NumberGroupSeparator: Char read GetNumberGroupSeparator write SetNumberGroupSeparator;
    property NumberGroupSizes: TArray<Integer> read FNumberGroupSizes write FNumberGroupSizes;
  end;
  PdxNumberFormatInfo = ^TdxNumberFormatInfo;

  { TdxCultureInfo }

  TdxCultureInfo = record
  strict private
    FLCID: Integer;
    FNumberFormatInfo: TdxNumberFormatInfo;
    function GetParent: TdxCultureInfo;
  private
    function GetCalendar: TcxCustomCalendarTable;
    function GetEnglishName: string;
    function GetFormatSettings: TFormatSettings;
    class function GetInvariantCulture: TdxCultureInfo; static;
  public
    constructor Create(const LCID: Integer; AUseUserOverride: Boolean = False); overload;
    constructor Create(const AName: string; AUseUserOverride: Boolean = False); overload;

    class operator Equal(const A, B: TdxCultureInfo): Boolean;

    function Clone: TdxCultureInfo;
    class function CurrentCulture: TdxCultureInfo; static;
    class function InvalidCulture: TdxCultureInfo; static;

    function IsValid: Boolean;

    property Calendar: TcxCustomCalendarTable read GetCalendar;
    property EnglishName: string read GetEnglishName;
    property FormatSettings: TFormatSettings read GetFormatSettings;
    class property InvariantCulture: TdxCultureInfo read GetInvariantCulture;
    property LCID: Integer read FLCID;
    property NumberFormat: TdxNumberFormatInfo read FNumberFormatInfo;
    property Parent: TdxCultureInfo read GetParent;
  end;

  { TdxLangInfo }

  TdxLangInfo = record
  strict private
    FLatin: TdxCultureInfo;
    FBidi: TdxCultureInfo;
    FEastAsia: TdxCultureInfo;
  public
    constructor Create(const ALatin, ABidi, AEastAsia: TdxCultureInfo);
    class operator Equal(const A, B: TdxLangInfo): Boolean;

    function Equals(const Value: TdxLangInfo): Boolean;

    function IsValid: Boolean;
    class function InvalidLangInfo: TdxLangInfo; static;

    property Latin: TdxCultureInfo read FLatin;
    property Bidi: TdxCultureInfo read FBidi;
    property EastAsia: TdxCultureInfo read FEastAsia;
  end;

  { TdxCultureInfoHelper }

  TdxCultureInfoHelper = class
  strict private
    class var
      FDictionary: TDictionary<Integer, string>;
  private
    class procedure Initialize; static;
    class procedure Finalize; static;
  public
    class function CreateCultureInfo(AKey: Integer): TdxCultureInfo; static;
  end;

implementation

uses
  dxCore, cxClasses;

{$IFNDEF DELPHIXE}
type
  TLocaleNameToLCIDFunc = function(AName: PWideChar; AFlags: DWORD): Integer; stdcall;

var
  FLocaleNameToLCID: TLocaleNameToLCIDFunc = nil;

function LocaleNameToLCID(AName: PWideChar; AFlags: DWORD): Integer; stdcall;
begin
  if not Assigned(FLocaleNameToLCID) then
    @FLocaleNameToLCID := GetProcAddress(GetModuleHandle(kernel32), 'LocaleNameToLCID');
  if Assigned(FLocaleNameToLCID) then
    Result := FLocaleNameToLCID(AName, AFlags)
  else
    raise EdxException.Create('Unexpected');
end;
{$EXTERNALSYM LocaleNameToLCID}
{$ENDIF}

{ TdxNumberFormatInfo }

constructor TdxNumberFormatInfo.Create(const AFormatSettings: TFormatSettings);
begin
  FFormatSettings := AFormatSettings;
  Initialize;
end;

function TdxNumberFormatInfo.GetNumberDecimalSeparator: Char;
begin
  Result := FormatSettings.DecimalSeparator;
end;

function TdxNumberFormatInfo.GetNumberGroupSeparator: Char;
begin
  Result := FormatSettings.ThousandSeparator;
end;

procedure TdxNumberFormatInfo.Initialize;
begin
  FNumberGroupSizes := TArray<Integer>.Create(3);
end;

procedure TdxNumberFormatInfo.SetNumberDecimalSeparator(const Value: Char);
begin
  FFormatSettings.DecimalSeparator := Value;
end;

procedure TdxNumberFormatInfo.SetNumberGroupSeparator(const Value: Char);
begin
  FFormatSettings.ThousandSeparator := Value;
end;

{ TdxCultureInfo }

constructor TdxCultureInfo.Create(const LCID: Integer; AUseUserOverride: Boolean = False);
var
  AFormatSettings: TFormatSettings;
begin
  FLCID := LCID;
  dxGetLocaleFormatSettings(LCID, AFormatSettings);
  FNumberFormatInfo := TdxNumberFormatInfo.Create(AFormatSettings);
end;

function TdxCultureInfo.Clone: TdxCultureInfo;
begin
  Result := TdxCultureInfo.Create(LCID);
end;

constructor TdxCultureInfo.Create(const AName: string; AUseUserOverride: Boolean = False);

  function AdjustLocaleName(const ALocaleName: string): string;
  const
    CLookup = '_';
    CReplace = '-';
  var
    P: PChar;
  begin
    Result := ALocaleName;
    P := PChar(Result);
    while P^ <> #0 do
    begin
      if P^ = CLookup then
      begin
        P^ := CReplace;
        Break;
      end;
      Inc(P);
    end;
  end;

var
  ALocale: Integer;
begin
  if AName <> '' then
  begin
    if Win32MajorVersion >= 6 then
      ALocale := LocaleNameToLCID(PChar(AdjustLocaleName(AName)), 0)
    else
      ALocale := dxLanguages.LocaleID[dxLanguages.IndexOf(AdjustLocaleName(AName))];
  end
  else
    ALocale := GetThreadLocale;

  Create(ALocale, AUseUserOverride);
end;

class operator TdxCultureInfo.Equal(const A, B: TdxCultureInfo): Boolean;
begin
  Result := A.LCID = B.LCID;
end;

class function TdxCultureInfo.CurrentCulture: TdxCultureInfo;
begin
  Result := TdxCultureInfo.Create(GetUserDefaultLCID);
end;

class function TdxCultureInfo.InvalidCulture: TdxCultureInfo;
begin
  Result.FLCID := -1;
end;

function TdxCultureInfo.IsValid: Boolean;
begin
  Result := LCID >= 0;
end;

function TdxCultureInfo.GetCalendar: TcxCustomCalendarTable;
begin
  Result := cxGetDefaultCalendar(LCID);
end;

function TdxCultureInfo.GetEnglishName: string;
begin
  Result := dxGetLocaleInfo(LCID, LOCALE_SENGLANGUAGE);
end;

function TdxCultureInfo.GetFormatSettings: TFormatSettings;
begin
  Result := NumberFormat.FormatSettings;
end;

class function TdxCultureInfo.GetInvariantCulture: TdxCultureInfo;
begin
  Result := TdxCultureInfo.Create($7F, False);
end;

function TdxCultureInfo.GetParent: TdxCultureInfo;
const
  LOCALE_SPARENT = $0000006D;
var
  AParentName: string;
begin
  AParentName := dxGetLocaleInfo(LCID, LOCALE_SPARENT);
  if AParentName <> '' then
    Result := TdxCultureInfo.Create(AParentName)
  else
    Result := TdxCultureInfo.InvariantCulture;
end;

{ TdxLangInfo }

constructor TdxLangInfo.Create(const ALatin, ABidi, AEastAsia: TdxCultureInfo);
begin
  FLatin := ALatin;
  FBidi := ABidi;
  FEastAsia := AEastAsia;
end;

class operator TdxLangInfo.Equal(const A, B: TdxLangInfo): Boolean;
begin
  Result := (A.Latin = B.Latin) and (A.Bidi = B.Bidi) and (A.EastAsia = B.EastAsia);
end;

function TdxLangInfo.Equals(const Value: TdxLangInfo): Boolean;
begin
  Result := Self = Value;
end;

function TdxLangInfo.IsValid: Boolean;
begin
  Result := Latin.IsValid;
end;

class function TdxLangInfo.InvalidLangInfo: TdxLangInfo;
begin
  Result := TdxLangInfo.Create(TdxCultureInfo.InvalidCulture, TdxCultureInfo.InvalidCulture, TdxCultureInfo.InvalidCulture);
end;

{ TdxCultureInfoHelper }

class procedure TdxCultureInfoHelper.Initialize;
begin
  FDictionary := TDictionary<Integer, string>.Create;

  FDictionary.Add(1, 'ar');
  FDictionary.Add(2, 'bg');
  FDictionary.Add(3, 'ca');
  FDictionary.Add(4, 'zh-Hans');
  FDictionary.Add(5, 'cs');
  FDictionary.Add(6, 'da');
  FDictionary.Add(7, 'de');
  FDictionary.Add(8, 'el');
  FDictionary.Add(9, 'en');
  FDictionary.Add(10, 'es');
  FDictionary.Add(11, 'fi');
  FDictionary.Add(12, 'fr');
  FDictionary.Add(13, 'he');
  FDictionary.Add(14, 'hu');
  FDictionary.Add(15, 'is');
  FDictionary.Add(16, 'it');
  FDictionary.Add(17, 'ja');
  FDictionary.Add(18, 'ko');
  FDictionary.Add(19, 'nl');
  FDictionary.Add(20, 'no');
  FDictionary.Add(21, 'pl');
  FDictionary.Add(22, 'pt');
  FDictionary.Add(23, 'rm');
  FDictionary.Add(24, 'ro');
  FDictionary.Add(25, 'ru');
  FDictionary.Add(26, 'hr');
  FDictionary.Add(27, 'sk');
  FDictionary.Add(28, 'sq');
  FDictionary.Add(29, 'sv');
  FDictionary.Add(30, 'th');
  FDictionary.Add(31, 'tr');
  FDictionary.Add(32, 'ur');
  FDictionary.Add(33, 'id');
  FDictionary.Add(34, 'uk');
  FDictionary.Add(35, 'be');
  FDictionary.Add(36, 'sl');
  FDictionary.Add(37, 'et');
  FDictionary.Add(38, 'lv');
  FDictionary.Add(39, 'lt');
  FDictionary.Add(40, 'tg');
  FDictionary.Add(41, 'fa');
  FDictionary.Add(42, 'vi');
  FDictionary.Add(43, 'hy');
  FDictionary.Add(44, 'az');
  FDictionary.Add(45, 'eu');
  FDictionary.Add(46, 'hsb');
  FDictionary.Add(47, 'mk');
  FDictionary.Add(50, 'tn');
  FDictionary.Add(52, 'xh');
  FDictionary.Add(53, 'zu');
  FDictionary.Add(54, 'af');
  FDictionary.Add(55, 'ka');
  FDictionary.Add(56, 'fo');
  FDictionary.Add(57, 'hi');
  FDictionary.Add(58, 'mt');
  FDictionary.Add(59, 'se');
  FDictionary.Add(60, 'ga');
  FDictionary.Add(62, 'ms');
  FDictionary.Add(63, 'kk');
  FDictionary.Add(64, 'ky');
  FDictionary.Add(65, 'sw');
  FDictionary.Add(66, 'tk');
  FDictionary.Add(67, 'uz');
  FDictionary.Add(68, 'tt');
  FDictionary.Add(69, 'bn');
  FDictionary.Add(70, 'pa');
  FDictionary.Add(71, 'gu');
  FDictionary.Add(72, 'or');
  FDictionary.Add(73, 'ta');
  FDictionary.Add(74, 'te');
  FDictionary.Add(75, 'kn');
  FDictionary.Add(76, 'ml');
  FDictionary.Add(77, 'as');
  FDictionary.Add(78, 'mr');
  FDictionary.Add(79, 'sa');
  FDictionary.Add(80, 'mn');
  FDictionary.Add(81, 'bo');
  FDictionary.Add(82, 'cy');
  FDictionary.Add(83, 'km');
  FDictionary.Add(84, 'lo');
  FDictionary.Add(86, 'gl');
  FDictionary.Add(87, 'kok');
  FDictionary.Add(89, 'sd');
  FDictionary.Add(90, 'syr');
  FDictionary.Add(91, 'si');
  FDictionary.Add(92, 'chr');
  FDictionary.Add(93, 'iu');
  FDictionary.Add(94, 'am');
  FDictionary.Add(95, 'tzm');
  FDictionary.Add(97, 'ne');
  FDictionary.Add(98, 'fy');
  FDictionary.Add(99, 'ps');
  FDictionary.Add(100, 'fil');
  FDictionary.Add(101, 'dv');
  FDictionary.Add(103, 'ff');
  FDictionary.Add(104, 'ha');
  FDictionary.Add(106, 'yo');
  FDictionary.Add(107, 'quz');
  FDictionary.Add(108, 'nso');
  FDictionary.Add(109, 'ba');
  FDictionary.Add(110, 'lb');
  FDictionary.Add(111, 'kl');
  FDictionary.Add(112, 'ig');
  FDictionary.Add(115, 'ti');
  FDictionary.Add(117, 'haw');
  FDictionary.Add(120, 'ii');
  FDictionary.Add(122, 'arn');
  FDictionary.Add(124, 'moh');
  FDictionary.Add(126, 'br');
  FDictionary.Add(127, '');
  FDictionary.Add(128, 'ug');
  FDictionary.Add(129, 'mi');
  FDictionary.Add(130, 'oc');
  FDictionary.Add(131, 'co');
  FDictionary.Add(132, 'gsw');
  FDictionary.Add(133, 'sah');
  FDictionary.Add(134, 'qut');
  FDictionary.Add(135, 'rw');
  FDictionary.Add(136, 'wo');
  FDictionary.Add(140, 'prs');
  FDictionary.Add(145, 'gd');
  FDictionary.Add(146, 'ku');
  FDictionary.Add(1025, 'ar-SA');
  FDictionary.Add(1026, 'bg-BG');
  FDictionary.Add(1027, 'ca-ES');
  FDictionary.Add(1028, 'zh-TW');
  FDictionary.Add(1029, 'cs-CZ');
  FDictionary.Add(1030, 'da-DK');
  FDictionary.Add(1031, 'de-DE');
  FDictionary.Add(1032, 'el-GR');
  FDictionary.Add(1033, 'en-US');
  FDictionary.Add(1035, 'fi-FI');
  FDictionary.Add(1036, 'fr-FR');
  FDictionary.Add(1037, 'he-IL');
  FDictionary.Add(1038, 'hu-HU');
  FDictionary.Add(1039, 'is-IS');
  FDictionary.Add(1040, 'it-IT');
  FDictionary.Add(1041, 'ja-JP');
  FDictionary.Add(1042, 'ko-KR');
  FDictionary.Add(1043, 'nl-NL');
  FDictionary.Add(1044, 'nb-NO');
  FDictionary.Add(1045, 'pl-PL');
  FDictionary.Add(1046, 'pt-BR');
  FDictionary.Add(1047, 'rm-CH');
  FDictionary.Add(1048, 'ro-RO');
  FDictionary.Add(1049, 'ru-RU');
  FDictionary.Add(1050, 'hr-HR');
  FDictionary.Add(1051, 'sk-SK');
  FDictionary.Add(1052, 'sq-AL');
  FDictionary.Add(1053, 'sv-SE');
  FDictionary.Add(1054, 'th-TH');
  FDictionary.Add(1055, 'tr-TR');
  FDictionary.Add(1056, 'ur-PK');
  FDictionary.Add(1057, 'id-ID');
  FDictionary.Add(1058, 'uk-UA');
  FDictionary.Add(1059, 'be-BY');
  FDictionary.Add(1060, 'sl-SI');
  FDictionary.Add(1061, 'et-EE');
  FDictionary.Add(1062, 'lv-LV');
  FDictionary.Add(1063, 'lt-LT');
  FDictionary.Add(1064, 'tg-Cyrl-TJ');
  FDictionary.Add(1065, 'fa-IR');
  FDictionary.Add(1066, 'vi-VN');
  FDictionary.Add(1067, 'hy-AM');
  FDictionary.Add(1068, 'az-Latn-AZ');
  FDictionary.Add(1069, 'eu-ES');
  FDictionary.Add(1070, 'hsb-DE');
  FDictionary.Add(1071, 'mk-MK');
  FDictionary.Add(1074, 'tn-ZA');
  FDictionary.Add(1076, 'xh-ZA');
  FDictionary.Add(1077, 'zu-ZA');
  FDictionary.Add(1078, 'af-ZA');
  FDictionary.Add(1079, 'ka-GE');
  FDictionary.Add(1080, 'fo-FO');
  FDictionary.Add(1081, 'hi-IN');
  FDictionary.Add(1082, 'mt-MT');
  FDictionary.Add(1083, 'se-NO');
  FDictionary.Add(1086, 'ms-MY');
  FDictionary.Add(1087, 'kk-KZ');
  FDictionary.Add(1088, 'ky-KG');
  FDictionary.Add(1089, 'sw-KE');
  FDictionary.Add(1090, 'tk-TM');
  FDictionary.Add(1091, 'uz-Latn-UZ');
  FDictionary.Add(1092, 'tt-RU');
  FDictionary.Add(1093, 'bn-IN');
  FDictionary.Add(1094, 'pa-IN');
  FDictionary.Add(1095, 'gu-IN');
  FDictionary.Add(1096, 'or-IN');
  FDictionary.Add(1097, 'ta-IN');
  FDictionary.Add(1098, 'te-IN');
  FDictionary.Add(1099, 'kn-IN');
  FDictionary.Add(1100, 'ml-IN');
  FDictionary.Add(1101, 'as-IN');
  FDictionary.Add(1102, 'mr-IN');
  FDictionary.Add(1103, 'sa-IN');
  FDictionary.Add(1104, 'mn-MN');
  FDictionary.Add(1105, 'bo-CN');
  FDictionary.Add(1106, 'cy-GB');
  FDictionary.Add(1107, 'km-KH');
  FDictionary.Add(1108, 'lo-LA');
  FDictionary.Add(1110, 'gl-ES');
  FDictionary.Add(1111, 'kok-IN');
  FDictionary.Add(1114, 'syr-SY');
  FDictionary.Add(1115, 'si-LK');
  FDictionary.Add(1116, 'chr-Cher-US');
  FDictionary.Add(1117, 'iu-Cans-CA');
  FDictionary.Add(1118, 'am-ET');
  FDictionary.Add(1121, 'ne-NP');
  FDictionary.Add(1122, 'fy-NL');
  FDictionary.Add(1123, 'ps-AF');
  FDictionary.Add(1124, 'fil-PH');
  FDictionary.Add(1125, 'dv-MV');
  FDictionary.Add(1128, 'ha-Latn-NG');
  FDictionary.Add(1130, 'yo-NG');
  FDictionary.Add(1131, 'quz-BO');
  FDictionary.Add(1132, 'nso-ZA');
  FDictionary.Add(1133, 'ba-RU');
  FDictionary.Add(1134, 'lb-LU');
  FDictionary.Add(1135, 'kl-GL');
  FDictionary.Add(1136, 'ig-NG');
  FDictionary.Add(1139, 'ti-ET');
  FDictionary.Add(1141, 'haw-US');
  FDictionary.Add(1144, 'ii-CN');
  FDictionary.Add(1146, 'arn-CL');
  FDictionary.Add(1148, 'moh-CA');
  FDictionary.Add(1150, 'br-FR');
  FDictionary.Add(1152, 'ug-CN');
  FDictionary.Add(1153, 'mi-NZ');
  FDictionary.Add(1154, 'oc-FR');
  FDictionary.Add(1155, 'co-FR');
  FDictionary.Add(1156, 'gsw-FR');
  FDictionary.Add(1157, 'sah-RU');
  FDictionary.Add(1158, 'qut-GT');
  FDictionary.Add(1159, 'rw-RW');
  FDictionary.Add(1160, 'wo-SN');
  FDictionary.Add(1164, 'prs-AF');
  FDictionary.Add(1169, 'gd-GB');
  FDictionary.Add(1170, 'ku-Arab-IQ');
  FDictionary.Add(2049, 'ar-IQ');
  FDictionary.Add(2051, 'ca-ES-valencia');
  FDictionary.Add(2052, 'zh-CN');
  FDictionary.Add(2055, 'de-CH');
  FDictionary.Add(2057, 'en-GB');
  FDictionary.Add(2058, 'es-MX');
  FDictionary.Add(2060, 'fr-BE');
  FDictionary.Add(2064, 'it-CH');
  FDictionary.Add(2067, 'nl-BE');
  FDictionary.Add(2068, 'nn-NO');
  FDictionary.Add(2070, 'pt-PT');
  FDictionary.Add(2074, 'sr-Latn-CS');
  FDictionary.Add(2077, 'sv-FI');
  FDictionary.Add(2092, 'az-Cyrl-AZ');
  FDictionary.Add(2094, 'dsb-DE');
  FDictionary.Add(2098, 'tn-BW');
  FDictionary.Add(2107, 'se-SE');
  FDictionary.Add(2108, 'ga-IE');
  FDictionary.Add(2110, 'ms-BN');
  FDictionary.Add(2115, 'uz-Cyrl-UZ');
  FDictionary.Add(2117, 'bn-BD');
  FDictionary.Add(2118, 'pa-Arab-PK');
  FDictionary.Add(2121, 'ta-LK');
  FDictionary.Add(2128, 'mn-Mong-CN');
  FDictionary.Add(2137, 'sd-Arab-PK');
  FDictionary.Add(2141, 'iu-Latn-CA');
  FDictionary.Add(2143, 'tzm-Latn-DZ');
  FDictionary.Add(2151, 'ff-Latn-SN');
  FDictionary.Add(2155, 'quz-EC');
  FDictionary.Add(2163, 'ti-ER');
  FDictionary.Add(3073, 'ar-EG');
  FDictionary.Add(3076, 'zh-HK');
  FDictionary.Add(3079, 'de-AT');
  FDictionary.Add(3081, 'en-AU');
  FDictionary.Add(3082, 'es-ES');
  FDictionary.Add(3084, 'fr-CA');
  FDictionary.Add(3098, 'sr-Cyrl-CS');
  FDictionary.Add(3131, 'se-FI');
  FDictionary.Add(3179, 'quz-PE');
  FDictionary.Add(4097, 'ar-LY');
  FDictionary.Add(4100, 'zh-SG');
  FDictionary.Add(4103, 'de-LU');
  FDictionary.Add(4105, 'en-CA');
  FDictionary.Add(4106, 'es-GT');
  FDictionary.Add(4108, 'fr-CH');
  FDictionary.Add(4122, 'hr-BA');
  FDictionary.Add(4155, 'smj-NO');
  FDictionary.Add(4191, 'tzm-Tfng-MA');
  FDictionary.Add(5121, 'ar-DZ');
  FDictionary.Add(5124, 'zh-MO');
  FDictionary.Add(5127, 'de-LI');
  FDictionary.Add(5129, 'en-NZ');
  FDictionary.Add(5130, 'es-CR');
  FDictionary.Add(5132, 'fr-LU');
  FDictionary.Add(5146, 'bs-Latn-BA');
  FDictionary.Add(5179, 'smj-SE');
  FDictionary.Add(6145, 'ar-MA');
  FDictionary.Add(6153, 'en-IE');
  FDictionary.Add(6154, 'es-PA');
  FDictionary.Add(6156, 'fr-MC');
  FDictionary.Add(6170, 'sr-Latn-BA');
  FDictionary.Add(6203, 'sma-NO');
  FDictionary.Add(7169, 'ar-TN');
  FDictionary.Add(7177, 'en-ZA');
  FDictionary.Add(7178, 'es-DO');
  FDictionary.Add(7194, 'sr-Cyrl-BA');
  FDictionary.Add(7227, 'sma-SE');
  FDictionary.Add(8193, 'ar-OM');
  FDictionary.Add(8201, 'en-JM');
  FDictionary.Add(8202, 'es-VE');
  FDictionary.Add(8218, 'bs-Cyrl-BA');
  FDictionary.Add(8251, 'sms-FI');
  FDictionary.Add(9217, 'ar-YE');
  FDictionary.Add(9225, 'en-029');
  FDictionary.Add(9226, 'es-CO');
  FDictionary.Add(9242, 'sr-Latn-RS');
  FDictionary.Add(9275, 'smn-FI');
  FDictionary.Add(10241, 'ar-SY');
  FDictionary.Add(10249, 'en-BZ');
  FDictionary.Add(10250, 'es-PE');
  FDictionary.Add(10266, 'sr-Cyrl-RS');
  FDictionary.Add(11265, 'ar-JO');
  FDictionary.Add(11273, 'en-TT');
  FDictionary.Add(11274, 'es-AR');
  FDictionary.Add(11290, 'sr-Latn-ME');
  FDictionary.Add(12289, 'ar-LB');
  FDictionary.Add(12297, 'en-ZW');
  FDictionary.Add(12298, 'es-EC');
  FDictionary.Add(12314, 'sr-Cyrl-ME');
  FDictionary.Add(13313, 'ar-KW');
  FDictionary.Add(13321, 'en-PH');
  FDictionary.Add(13322, 'es-CL');
  FDictionary.Add(14337, 'ar-AE');
  FDictionary.Add(14346, 'es-UY');
  FDictionary.Add(15361, 'ar-BH');
  FDictionary.Add(15370, 'es-PY');
  FDictionary.Add(16385, 'ar-QA');
  FDictionary.Add(16393, 'en-IN');
  FDictionary.Add(16394, 'es-BO');
  FDictionary.Add(17417, 'en-MY');
  FDictionary.Add(17418, 'es-SV');
  FDictionary.Add(18441, 'en-SG');
  FDictionary.Add(18442, 'es-HN');
  FDictionary.Add(19466, 'es-NI');
  FDictionary.Add(20490, 'es-PR');
  FDictionary.Add(21514, 'es-US');
  FDictionary.Add(25626, 'bs-Cyrl');
  FDictionary.Add(26650, 'bs-Latn');
  FDictionary.Add(27674, 'sr-Cyrl');
  FDictionary.Add(28698, 'sr-Latn');
  FDictionary.Add(28731, 'smn');
  FDictionary.Add(29740, 'az-Cyrl');
  FDictionary.Add(29755, 'sms');
  FDictionary.Add(30724, 'zh');
  FDictionary.Add(30740, 'nn');
  FDictionary.Add(30746, 'bs');
  FDictionary.Add(30764, 'az-Latn');
  FDictionary.Add(30779, 'sma');
  FDictionary.Add(30787, 'uz-Cyrl');
  FDictionary.Add(30800, 'mn-Cyrl');
  FDictionary.Add(30813, 'iu-Cans');
  FDictionary.Add(30815, 'tzm-Tfng');
  FDictionary.Add(31748, 'zh-Hant');
  FDictionary.Add(31764, 'nb');
  FDictionary.Add(31770, 'sr');
  FDictionary.Add(31784, 'tg-Cyrl');
  FDictionary.Add(31790, 'dsb');
  FDictionary.Add(31803, 'smj');
  FDictionary.Add(31811, 'uz-Latn');
  FDictionary.Add(31814, 'pa-Arab');
  FDictionary.Add(31824, 'mn-Mong');
  FDictionary.Add(31833, 'sd-Arab');
  FDictionary.Add(31836, 'chr-Cher');
  FDictionary.Add(31837, 'iu-Latn');
  FDictionary.Add(31839, 'tzm-Latn');
  FDictionary.Add(31847, 'ff-Latn');
  FDictionary.Add(31848, 'ha-Latn');
  FDictionary.Add(31890, 'ku-Arab');
end;

class procedure TdxCultureInfoHelper.Finalize;
begin
  FreeAndNil(FDictionary);
end;

class function TdxCultureInfoHelper.CreateCultureInfo(AKey: Integer): TdxCultureInfo;
var
  ANameLang: string;
begin
  if FDictionary.TryGetValue(AKey, ANameLang) then
    Result := TdxCultureInfo.Create(ANameLang)
  else
    Result := TdxCultureInfo.InvalidCulture;
end;

initialization
  TdxCultureInfoHelper.Initialize;

finalization
  TdxCultureInfoHelper.Finalize;

end.
