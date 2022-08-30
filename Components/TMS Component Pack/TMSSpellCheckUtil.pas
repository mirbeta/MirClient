{***************************************************************************}
{ TMS Spell Check component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit TMSSpellCheckUtil;

{$I TMSDEFS.INC}

interface

uses
  Math,Classes, SysUtils, XMLDoc, XMLDom, XMLIntf,
{$IFDEF FMXLIB}
  FMX.Forms, FMX.Controls
{$ENDIF}
{$IFNDEF FMXLIB}
  Forms, Controls
{$ENDIF}
  ;

type
  TTMSLangauages = (lcCustom, lcAfrikaans, // af	af
    lcAlbanian, // sq	sq	1052		1250
    lcAmharic, // am	am	1118
    lcArabic, // - Algeria	ar	ar-dz	5121	1401	1256
    lcArmenian, // hy	hy	1067
    lcAssamese, // as	as	1101
    lcAzeriCyrillic, // - Cyrillic	az	az-az	2092		1251
    lcAzeriLatin, // - Latin	az	az-az	1068		1254
    lcBasque, // eu	eu	1069		1252
    lcBelarusian, // be	be	1059	423	1251
    lcBengali, // - Bangladesh	bn	bn	2117	845
    lcBosnian, // bs	bs	5146
    lcBulgarian, // bg	bg	1026	402	1251
    lcBurmese, // my	my	1109	455
    lcCatalan, // ca	ca	1027	403	1252
    lcChineseSimplified, // - China	zh	zh-cn	2052	804
    lcChineseTraditional, // - Hong Kong SAR	zh	zh-hk	3076
    lcCroatian, // hr	hr	1050		1250
    lcCzech, // cs	cs	1029	405	1250
    lcDanish, // da	da	1030	406	1252
    lcDivehi, // ; Dhivehi; Maldivian	dv	dv	1125	465
    lcDutch, // - Netherlands	nl	nl-nl	1043	413	1252
    lcEdo, // 1126	466
    lcEnglish, // - Australia	en	en-au	3081		1252
    lcEstonian, // et	et	1061	425	1257
    lcFaroese, // fo	fo	1080	438	1252
    lcPersian, // - Persian	fa	fa	1065	429	1256
    lcFilipino, // 1124	464
    lcFinnish, // fi	fi	1035		1252
    lcFrench, // - Belgium	fr	fr-be	2060		1252
    lcFrisian, // - Netherlands			1122	462
    lcFYRO, // Macedonia	mk	mk	1071		1251
    lcGaelic, // - Ireland	gd	gd-ie	2108
    lcGalician, // gl		1110	456	1252
    lcGeorgian, // ka		1079	437
    lcGerman, // - Germany	de	de-de	1031	407	1252
    lcGreek, // el	el	1032	408	1253
    lcGuarani, // - Paraguay	gn	gn	1140	474
    lcGujarati, // gu	gu	1095	447
    lcHebrew, // he	he	1037		1255
    lcHID, // (Human Interface Device)			1279
    lcHindi, // hi	hi	1081	439
    lcHungarian, // hu	hu	1038		1250
    lcIcelandic, // is	is	1039		1252
    lcIgbo, // - Nigeria			1136	470
    lcIndonesian, // id	id	1057	421	1252
    lcItalian, // - Italy	it	it-it	1040	410	1252
    lcJapanese, // ja	ja	1041	411
    lcKannada, // kn	kn	1099
    lcKashmiri, // ks	ks	1120	460
    lcKazakh, // kk	kk	1087		1251
    lcKhmer, // km	km	1107	453
    lcKonkani, // 1111	457
    lcKorean, // ko	ko	1042	412
    lcKyrgyz, // - Cyrillic			1088	440	1251
    lcLao, // lo	lo	1108	454
    lcLatin, // la	la	1142	476
    lcLatvian, // lv	lv	1062	426	1257
    lcLithuanian, // lt	lt	1063	427	1257
    lcMalay, // - Brunei	ms	ms-bn	2110		1252
    lcMalayalam, // ml	ml	1100
    lcMaltese, // mt	mt	1082
    lcManipuri, // 1112	458
    lcMaori, // mi	mi	1153	481
    lcMarathi, // mr	mr	1102
    lcMongolianStandard, // mn	mn	2128	850
    lcMongolian, // mn	mn	1104	450	1251
    lcNepali, // ne	ne	1121	461
    lcNorwegian, // - Bokml	nb	no-no	1044	414	1252
    lcOriya, // or	or	1096	448
    lcPolish, // pl	pl	1045	415	1250
    lcPortuguese, // - Brazil	pt	pt-br	1046	416	1252
    lcPunjabi, // pa	pa	1094	446
    lcRaeto_Romance, // rm	rm	1047	417
    lcRomanian, // - Moldova	ro	ro-mo	2072	818
    lcRussian, // ru	ru	1049	419	1251
    lcSami, // Lappish			1083
    lcSanskrit, // sa	sa	1103
    lcSerbian, // - Cyrillic	sr	sr-sp	3098		1251
    lcSesotho, // (Sutu)			1072	430
    lcSetsuana, // tn	tn	1074	432
    lcSindhi, // sd	sd	1113	459
    lcSinhala, // ; Sinhalese	si	si	1115
    lcSlovak, // sk	sk	1051		1250
    lcSlovenian, // sl	sl	1060	424	1250
    lcSomali, // so	so	1143	477
    lcSorbian, // sb	sb	1070
    lcSpanish, // - Argentina	es	es-ar	11274		1252
    lcSwahili, // sw	sw	1089	441	1252
    lcSwedish, // - Sweden	sv	sv-se	1053		1252
    lcSyriac, // 1114
    lcTajik, // tg	tg	1064	428
    lcTamil, // ta	ta	1097	449
    lcTatar, // tt	tt	1092	444	1251
    lcTelugu, // te	te	1098
    lcThai, // th	th	1054
    lcTibetan, // bo	bo	1105	451
    lcTsonga, // ts	ts	1073	431
    lcTurkish, // tr	tr	1055		1254
    lcTurkmen, // tk	tk	1090	442
    lcUkrainian, // uk	uk	1058	422	1251
    lcUnicode, // UTF-8	0
    lcUrdu, // ur	ur	1056	420	1256
    lcUzbek, // - Cyrillic	uz	uz-uz	2115	843	1251
    lcVenda, // 1075	433
    lcVietnamese, // vi	vi	1066		1258
    lcWelsh, // cy	cy	1106	452
    lcXhos, // a	xh	xh	1076	434
    lcYiddish, // yi	yi	1085
    lcZulu); // zu	zu	1077	435

function Base64Encode(const Input: string): string;
function Base64Decode(const Input: string): string;
function Unescape(const s: string): string;
function FindNode(Node: IXMLNode; Name: string): IXMLNode;
function LanguageEnumToString(Value: TTMSLangauages): string;
function FixWithExtension(Filename, Extension: string): string;
function StringToLanguageEnum(Value: string): TTMSLangauages;
function FixFileName(Value:String):string;
procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings);
function FixCap(Word,Value:string):string;
function CharInStr(s: string; Index: Integer): Char;
{$IFNDEF DELPHIXE5_LVL}
function CharInSetX(ch: char; charset: TSysCharSet): boolean;
{$ENDIF}
{$IFDEF DELPHIXE5_LVL}
function CharInSetX(ch: char; charset: array of char): boolean;
{$ENDIF}
procedure SetCharInStr(var s: string; index: integer; ch: char);


implementation

{$IFDEF DELPHIXE5_LVL}
uses
  Character;
{$ENDIF}

function CharInStr(s: string; Index: Integer): Char;
begin
  Result := #0;
  if (Index > 0) and (Index <= Length(s)) then
  begin
    {$IFDEF DELPHI_LLVM}
    dec(Index);
    {$ENDIF}
    Result := s[Index]
  end;
end;

{$IFNDEF DELPHIXE5_LVL}
function CharInSetX(ch: char; charset: TSysCharSet): boolean;
{$ENDIF}
{$IFDEF DELPHIXE5_LVL}
function CharInSetX(ch: char; charset: array of char): boolean;
{$ENDIF}
begin
{$IFNDEF DELPHIXE5_LVL}
  Result := CharInSet(ch, charset);
{$ENDIF}

{$IFDEF DELPHIXE5_LVL}
  Result := ch.IsInArray(charset);
{$ENDIF}
end;


procedure SetCharInStr(var s: string; index: integer; ch: char);
begin
  if (Index > 0) and (Index <= Length(s)) then
  begin
    {$IFDEF DELPHI_LLVM}
    dec(Index);
    {$ENDIF}
    s[Index] := ch;
  end;
end;

function OctToDec(OctStr: string): string;
var
  DecNum: Real;
  i: Integer;
  Error: Boolean;
begin
  DecNum := 0;
  Error := False;

  for i := Length(OctStr) downto 1 do
  begin
    if not CharInSetX(CharInStr(OctStr, i), ['0','1','2','3','4','5','6','7']) then
    begin
      Error := True;
      Break;
    end;
    DecNum := DecNum + StrToInt(CharInStr(OctStr,i))*Power(8, Length(OctStr)-i);
  end;

  if not Error then
    Result := FloatToStr(DecNum)
  else
    Result := '';
end;

function Unescape(const s: string): string;
var
  i: Integer;
  j: Integer;
  c: Integer;
  x: Word;
  su: string;
  ch: Char;
begin
  // Make result at least large enough. This prevents too many reallocs
  SetLength(Result, Length(s));
  i := 1;
  j := 1;
  while i <= Length(s) do
  begin
    if s[i] = '\' then
    begin
      if i < Length(s) then
      begin
        ch := CharInStr(s,i + 1);

        // escaped backslash?
        if ch = '\' then
        begin
          Result[j] := '\';
          inc(i, 2);
        end
        else
        if ch = ':' then
        begin
          Result[j] := '"';
          inc(i, 2);
        end
        else
        if ch = '"' then
        begin
          Result[j] := '"';
          inc(i, 2);
        end
        else
        if ch = '''' then
        begin
          Result[j] := '''';
          inc(i, 2);
        end
        else
        if CharInSetX(ch, ['0','1','2','3','4','5','6','7','8','9']) and (i+3 <= Length(s)) then
        begin
          su := string(Copy(s,i+1,3));
          x := StrToInt(OctToDec(su));
          Result[j] := Chr(x);
          //Result[j] := AnsiChar(PByteArray(@x)[0]);
          //Inc(j);
          inc(i, 4);
        end
        // convert hex number to WideChar
        else
        if (ch = 'u') and (i + 1 + 4 <= Length(s))
                and TryStrToInt('$' + string(Copy(s, i + 2, 4)), c) then
        begin
          inc(i, 6);
          Result[j] := Char(c);
        end
        else
        begin
//          Result[j] := Char(s[i + 1]);
          inc(i, 2);
          ;//raise Exception.CreateFmt('Invalid code at position %d', [i]);
        end;
      end
      else
      begin
        raise Exception.Create('Unexpected end of string');
      end;
    end
    else
    begin
      Result[j] := CharInStr(s,i);
      inc(i);
    end;
    inc(j);
  end;

  // Trim result in case we reserved too much space
  SetLength(Result, j - 1);
end;

function FixCap(Word,Value: string): string;
var
  fsc,falc:boolean;

  procedure GetCapsInfo;
  var
    S1, S2: string;
    I: integer;
  begin
    S1 := UpperCase(Word, TLocaleOptions.loUserLocale);
    S2 := LowerCase(Word, TLocaleOptions.loUserLocale);
    fsc := false;
    falc := false;
    if CharInStr(S1,1) = CharInStr(Word, 1) then
    begin
      fsc := true;
      falc := true;
      for I := 1 to Length(S2) do
      begin
        if CharInStr(Word,I) = CharInStr(S2,I) then
        begin
          falc := false;
          Exit;
        end;
      end;
    end;
  end;

begin
  GetCapsInfo;
  if falc then
    Exit(UpperCase(Value,TLocaleOptions.loUserLocale));
  if fsc then
    Exit(UpperCase(Copy(Value,1,1),TLocaleOptions.loUserLocale)+LowerCase(Copy(Value,2,65535),TLocaleOptions.loUserLocale));
  Exit(Value);
end;

procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;

  while pos(delimiter, input) > 0 do
  begin
    strings.Add(copy(input, 1, pos(delimiter, input) - 1));
    system.delete(input, 1, pos(delimiter, input));
  end;

  Strings.Add(input);
end;

function FixWithExtension(Filename, Extension: string): string;
begin
  if ExtractFileExt(Filename) = '' then
    Exit(Filename + Extension);
  Exit(Filename + Extension);
end;

function LanguageEnumToString(Value: TTMSLangauages): string;
begin
  Result := 'Custom';
  case Value of
    lcCustom:
      Result := 'Custom';
    lcAfrikaans:
      Result := 'Afrikaans';
    lcAlbanian:
      Result := 'Albanian';
    lcAmharic:
      Result := 'Amharic';
    lcArabic:
      Result := 'Arabic';
    lcArmenian:
      Result := 'Armenian';
    lcAssamese:
      Result := 'Assamese';
    lcAzeriCyrillic:
      Result := 'AzeriCyrillic';
    lcAzeriLatin:
      Result := 'AzeriLatin';
    lcBasque:
      Result := 'Basque';
    lcBelarusian:
      Result := 'Belarusian';
    lcBengali:
      Result := 'Bengali';
    lcBosnian:
      Result := 'Bosnian';
    lcBulgarian:
      Result := 'Bulgarian';
    lcBurmese:
      Result := 'Burmese';
    lcCatalan:
      Result := 'Catalan';
    lcChineseSimplified:
      Result := 'ChineseSimplified';
    lcChineseTraditional:
      Result := 'ChineseTraditional';
    lcCroatian:
      Result := 'Croatian';
    lcCzech:
      Result := 'Czech';
    lcDanish:
      Result := 'Danish';
    lcDivehi:
      Result := 'Divehi';
    lcDutch:
      Result := 'Dutch';
    lcEdo:
      Result := 'Edo';
    lcEnglish:
      Result := 'English';
    lcEstonian:
      Result := 'Estonian';
    lcFaroese:
      Result := 'Faroese';
    lcPersian:
      Result := 'Persian';
    lcFilipino:
      Result := 'Filipino';
    lcFinnish:
      Result := 'Finnish';
    lcFrench:
      Result := 'French';
    lcFrisian:
      Result := 'Frisian';
    lcFYRO:
      Result := 'FYRO';
    lcGaelic:
      Result := 'Gaelic';
    lcGalician:
      Result := 'Galician';
    lcGeorgian:
      Result := 'Georgian';
    lcGerman:
      Result := 'German';
    lcGreek:
      Result := 'Greek';
    lcGuarani:
      Result := 'Guarani';
    lcGujarati:
      Result := 'Gujarati';
    lcHebrew:
      Result := 'Hebrew';
    lcHID:
      Result := 'HID';
    lcHindi:
      Result := 'Hindi';
    lcHungarian:
      Result := 'Hungarian';
    lcIcelandic:
      Result := 'Hungarian';
    lcIgbo:
      Result := 'Igbo';
    lcIndonesian:
      Result := 'Indonesian';
    lcItalian:
      Result := 'Italian';
    lcJapanese:
      Result := 'Japanese';
    lcKannada:
      Result := 'Kannada';
    lcKashmiri:
      Result := 'Kashmiri';
    lcKazakh:
      Result := 'Kazakh';
    lcKhmer:
      Result := 'Khmer';
    lcKonkani:
      Result := 'Konkani';
    lcKorean:
      Result := 'Korean';
    lcKyrgyz:
      Result := 'Kyrgyz';
    lcLao:
      Result := 'Lao';
    lcLatin:
      Result := 'Latin';
    lcLatvian:
      Result := 'Latvian';
    lcLithuanian:
      Result := 'Lithuanian';
    lcMalay:
      Result := 'Malay';
    lcMalayalam:
      Result := 'Malayalam';
    lcMaltese:
      Result := 'Maltese';
    lcMaori:
      Result := 'Maori';
    lcMarathi:
      Result := 'Marathi';
    lcMongolianStandard:
      Result := 'MongolianStandard';
    lcMongolian:
      Result := 'Mongolian';
    lcNepali:
      Result := 'Nepali';
    lcNorwegian:
      Result := 'Norwegian';
    lcOriya:
      Result := 'Oriya';
    lcPolish:
      Result := 'Polish';
    lcPortuguese:
      Result := 'Portuguese';
    lcPunjabi:
      Result := 'Punjabi';
    lcRaeto_Romance:
      Result := 'Raeto_Romance';
    lcRomanian:
      Result := 'Romanian';
    lcRussian:
      Result := 'Russian';
    lcSami:
      Result := 'Sami';
    lcSanskrit:
      Result := 'Sanskrit';
    lcSerbian:
      Result := 'Serbian';
    lcSesotho:
      Result := 'Sesotho';
    lcSetsuana:
      Result := 'Setsuana';
    lcSindhi:
      Result := 'Sindhi';
    lcSinhala:
      Result := 'Sinhala';
    lcSlovak:
      Result := 'Slovak';
    lcSlovenian:
      Result := 'Slovenian';
    lcSomali:
      Result := 'Somali';
    lcSorbian:
      Result := 'Sorbian';
    lcSpanish:
      Result := 'Spanish';
    lcSwahili:
      Result := 'Swahili';
    lcSwedish:
      Result := 'Swedish';
    lcSyriac:
      Result := 'Syriac';
    lcTajik:
      Result := 'Tajik';
    lcTamil:
      Result := 'Tamil';
    lcTatar:
      Result := 'Tatar';
    lcTelugu:
      Result := 'Telugu';
    lcThai:
      Result := 'Thai';
    lcTibetan:
      Result := 'Tibetan';
    lcTsonga:
      Result := 'Tsonga';
    lcTurkish:
      Result := 'Turkish';
    lcTurkmen:
      Result := 'Turkmen';
    lcUkrainian:
      Result := 'Ukrainian';
    lcUnicode:
      Result := 'Unicode';
    lcUrdu:
      Result := 'Urdu';
    lcUzbek:
      Result := 'Uzbek';
    lcVenda:
      Result := 'Venda';
    lcVietnamese:
      Result := 'Vietnamese';
    lcWelsh:
      Result := 'Welsh';
    lcXhos:
      Result := 'Xhos';
    lcYiddish:
      Result := 'Yiddish';
    lcZulu:
      Result := 'Zulu';
  end;
end;

function FindNode(Node:IXMLNode;Name:string):IXMLNode;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Node.ChildNodes.Count-1 do
    if Node.ChildNodes[I].LocalName = Name then
      Exit(Node.ChildNodes[I]);
end;

function FixFileName(Value:String):string;
begin
  if Copy(Value,2,1) = ':' then
    Exit(Value);
  if Copy(Value,1,1) = '/' then
    Exit(Value);

  {$IFNDEF FMXLIB}
  Exit(ExtractFilePath(Application.ExeName) + Value);
  {$ENDIF}
  {$IFDEF FMXLIB}
  Exit(Value);
  {$ENDIF}
end;

function StringToLanguageEnum(Value: string): TTMSLangauages;
begin
  Result := lcCustom;
  Value := Uppercase(Copy(Value,1,1))+LowerCase(Copy(Value,2,100));
  if Value = 'Custom' then
    Result := lcCustom;
  if Value = 'Afrikaans' then
    Result := lcAfrikaans;
  if Value = 'Afrikaans' then
    Result := lcAfrikaans;
  if Value = 'Albanian' then
    Result := lcAlbanian;
  if Value = 'Amharic' then
    Result := lcAmharic;
  if Value = 'Arabic' then
    Result := lcArabic;
  if Value = 'Armenian' then
    Result := lcArmenian;
  if Value = 'Assamese' then
    Result := lcAssamese;
  if Value = 'AzeriCyrillic' then
    Result := lcAzeriCyrillic;
  if Value = 'AzeriLatin' then
    Result := lcAzeriLatin;
  if Value = 'Basque' then
    Result := lcBasque;
  if Value = 'Belarusian' then
    Result := lcBelarusian;
  if Value = 'Bengali' then
    Result := lcBengali;
  if Value = 'Bosnian' then
    Result := lcBosnian;
  if Value = 'Bulgarian' then
    Result := lcBulgarian;
  if Value = 'Burmese' then
    Result := lcBurmese;
  if Value = 'Catalan' then
    Result := lcCatalan;
  if Value = 'ChineseSimplified' then
    Result := lcChineseSimplified;
  if Value = 'ChineseTraditional' then
    Result := lcChineseTraditional;
  if Value = 'Croatian' then
    Result := lcCroatian;
  if Value = 'Czech' then
    Result := lcCzech;
  if Value = 'Danish' then
    Result := lcDanish;
  if Value = 'Divehi' then
    Result := lcDivehi;
  if Value = 'Dutch' then
    Result := lcDutch;
  if Value = 'Edo' then
    Result := lcEdo;
  if Value = 'English' then
    Result := lcEnglish;
  if Value = 'Estonian' then
    Result := lcEstonian;
  if Value = 'Faroese' then
    Result := lcFaroese;
  if Value = 'Persian' then
    Result := lcPersian;
  if Value = 'Filipino' then
    Result := lcFilipino;
  if Value = 'Finnish' then
    Result := lcFinnish;
  if Value = 'French' then
    Result := lcFrench;
  if Value = 'Frisian' then
    Result := lcFrisian;
  if Value = 'FYRO' then
    Result := lcFYRO;
  if Value = 'Gaelic' then
    Result := lcGaelic;
  if Value = 'Galician' then
    Result := lcGalician;
  if Value = 'Georgian' then
    Result := lcGeorgian;
  if Value = 'German' then
    Result := lcGerman;
  if Value = 'Greek' then
    Result := lcGreek;
  if Value = 'Guarani' then
    Result := lcGuarani;
  if Value = 'Gujarati' then
    Result := lcGujarati;
  if Value = 'Hebrew' then
    Result := lcHebrew;
  if Value = 'HID' then
    Result := lcHID;
  if Value = 'Hindi' then
    Result := lcHindi;
  if Value = 'Hungarian' then
    Result := lcHungarian;
  if Value = 'Hungarian' then
    Result := lcIcelandic;
  if Value = 'Igbo' then
    Result := lcIgbo;
  if Value = 'Indonesian' then
    Result := lcIndonesian;
  if Value = 'Italian' then
    Result := lcItalian;
  if Value = 'Japanese' then
    Result := lcJapanese;
  if Value = 'Kannada' then
    Result := lcKannada;
  if Value = 'Kashmiri' then
    Result := lcKashmiri;
  if Value = 'Kazakh' then
    Result := lcKazakh;
  if Value = 'Khmer' then
    Result := lcKhmer;
  if Value = 'Konkani' then
    Result := lcKonkani;
  if Value = 'Korean' then
    Result := lcKorean;
  if Value = 'Kyrgyz' then
    Result := lcKyrgyz;
  if Value = 'Lao' then
    Result := lcLao;
  if Value = 'Latin' then
    Result := lcLatin;
  if Value = 'Latvian' then
    Result := lcLatvian;
  if Value = 'Lithuanian' then
    Result := lcLithuanian;
  if Value = 'Malay' then
    Result := lcMalay;
  if Value = 'Malayalam' then
    Result := lcMalayalam;
  if Value = 'Maltese' then
    Result := lcMaltese;
  if Value = 'Maori' then
    Result := lcMaori;
  if Value = 'Marathi' then
    Result := lcMarathi;
  if Value = 'MongolianStandard' then
    Result := lcMongolianStandard;
  if Value = 'Mongolian' then
    Result := lcMongolian;
  if Value = 'Nepali' then
    Result := lcNepali;
  if Value = 'Norwegian' then
    Result := lcNorwegian;
  if Value = 'Oriya' then
    Result := lcOriya;
  if Value = 'Polish' then
    Result := lcPolish;
  if Value = 'Portuguese' then
    Result := lcPortuguese;
  if Value = 'Punjabi' then
    Result := lcPunjabi;
  if Value = 'Raeto_Romance' then
    Result := lcRaeto_Romance;
  if Value = 'Romanian' then
    Result := lcRomanian;
  if Value = 'Russian' then
    Result := lcRussian;
  if Value = 'Sami' then
    Result := lcSami;
  if Value = 'Sanskrit' then
    Result := lcSanskrit;
  if Value = 'Serbian' then
    Result := lcSerbian;
  if Value = 'Sesotho' then
    Result := lcSesotho;
  if Value = 'Setsuana' then
    Result := lcSetsuana;
  if Value = 'Sindhi' then
    Result := lcSindhi;
  if Value = 'Sinhala' then
    Result := lcSinhala;
  if Value = 'Slovak' then
    Result := lcSlovak;
  if Value = 'Slovenian' then
    Result := lcSlovenian;
  if Value = 'Somali' then
    Result := lcSomali;
  if Value = 'Sorbian' then
    Result := lcSorbian;
  if Value = 'Spanish' then
    Result := lcSpanish;
  if Value = 'Swahili' then
    Result := lcSwahili;
  if Value = 'Swedish' then
    Result := lcSwedish;
  if Value = 'Syriac' then
    Result := lcSyriac;
  if Value = 'Tajik' then
    Result := lcTajik;
  if Value = 'Tamil' then
    Result := lcTamil;
  if Value = 'Tatar' then
    Result := lcTatar;
  if Value = 'Telugu' then
    Result := lcTelugu;
  if Value = 'Thai' then
    Result := lcThai;
  if Value = 'Tibetan' then
    Result := lcTibetan;
  if Value = 'Tsonga' then
    Result := lcTsonga;
  if Value = 'Turkish' then
    Result := lcTurkish;
  if Value = 'Turkmen' then
    Result := lcTurkmen;
  if Value = 'Ukrainian' then
    Result := lcUkrainian;
  if Value = 'Unicode' then
    Result := lcUnicode;
  if Value = 'Urdu' then
    Result := lcUrdu;
  if Value = 'Uzbek' then
    Result := lcUzbek;
  if Value = 'Venda' then
    Result := lcVenda;
  if Value = 'Vietnamese' then
    Result := lcVietnamese;
  if Value = 'Welsh' then
    Result := lcWelsh;
  if Value = 'Xhos' then
    Result := lcXhos;
  if Value = 'Yiddish' then
    Result := lcYiddish;
  if Value = 'Zulu' then
    Result := lcZulu;
end;


var
  EncodeTable: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz' +
    '0123456789+/';

  DecodeTable: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
    64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
    64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of Char);
  end;

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];

  if NumChars < 2 then
    OutBuf[2] := '='
  else
    OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];

  if NumChars < 3 then
    OutBuf[3] := '='
  else
    OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

function Base64Encode(const Input: string): string;
var
  I, K, J: Integer;
  Packet: TPacket;
begin
  Result := '';
  I := (Length(Input) div 3) * 4;
  if Length(Input) mod 3 > 0 then Inc(I, 4);
  SetLength(Result, I);
  J := 1;
  for I := 1 to Length(Input) div 3 do
  begin
    Packet.i := 0;
    Packet.a[0] := Byte(Input[(I - 1) * 3 + 1]);
    Packet.a[1] := Byte(Input[(I - 1) * 3 + 2]);
    Packet.a[2] := Byte(Input[(I - 1) * 3 + 3]);
    EncodePacket(Packet, 3, PChar(@Result[J]));
    Inc(J, 4);
  end;
  K := 0;
  Packet.i := 0;
  for I := Length(Input) - (Length(Input) mod 3) + 1 to Length(Input) do
  begin
    Packet.a[K] := Byte(Input[I]);
    Inc(K);
    if I = Length(Input) then
      EncodePacket(Packet, Length(Input) mod 3, PChar(@Result[J]));
  end;
end;

function DecodePacket(InBuf: PChar; var nChars: Integer): TPacket;
begin
  Result.a[0] := (DecodeTable[InBuf[0]] shl 2) or
    (DecodeTable[InBuf[1]] shr 4);
  NChars := 1;
  if InBuf[2] <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := (DecodeTable[InBuf[1]] shl 4) or (DecodeTable[InBuf[2]] shr 2);
  end;
  if InBuf[3] <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := (DecodeTable[InBuf[2]] shl 6) or DecodeTable[InBuf[3]];
  end;
end;

function Base64Decode(const Input: string): string;
var
  I, J, K: Integer;
  Packet: TPacket;
begin
  Result := '';
  for I := 1 to Length(Input) div 4 do
  begin
    Packet := DecodePacket(PChar(@Input[(I - 1) * 4 + 1]), J);
    K := 0;
    while J > 0 do
    begin
      Result := Result + Packet.c[K];
      Inc(K);
      Dec(J);
    end;
  end;
end;

end.
