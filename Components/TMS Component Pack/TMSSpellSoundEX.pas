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

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN WIDECHAR_REDUCED OFF}

unit TMSSpellSoundEX;

{$I TMSDEFS.INC}

{$IFDEF IOS}
  {$DEFINE DELPHI_LLVM}
{$ENDIF}

{$IFDEF ANDROID}
  {$DEFINE DELPHI_LLVM}
{$ENDIF}

interface

uses
  Classes, Math, StrUtils, SysUtils, Types, Generics.Collections,
{$IFNDEF FMXLIB}
  Dialogs
{$ENDIF}
{$IFDEF FMXLIB}
  FMX.Dialogs
{$ENDIF}
  ;

var
  // Alphs: array [0 .. 65535] of Char;
  Alphs: array ['A' .. 'Z'] of Char = (
    '0',
    '1',
    '2',
    '3',
    '0',
    '1',
    '2',
    '0',
    '0',
    '2',
    '2',
    '4',
    '5',
    '5',
    '0',
    '1',
    '2',
    '6',
    '2',
    '3',
    '0',
    '1',
    '0',
    '2',
    '0',
    '2'
  );

type
  TTMSSoundexProcess = (spStandard, spGerman, spFrench, spSpanish,spItalian,spDutch);

  TTMSSoundexContainer = Class(TCollectionItem)
  private
    FCharacters: string;
    FValue: Char;
  protected
  public
    property Value: Char read FValue write FValue;
    property Characters: string read FCharacters write FCharacters;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TTMSSoundexContainerCollection = class(TCollection)
  private
    FName: String;
    FEnabled: boolean;
  protected
    function GetItem(Index: integer): TTMSSoundexContainer;
  public
    property Enabled: boolean read FEnabled write FEnabled;
    property Name: String read FName write FName;
    property Items[Index: integer]: TTMSSoundexContainer read GetItem; default;
    function Add: TTMSSoundexContainer;
    function Insert(Index: integer): TTMSSoundexContainer;
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSoundexManager = class(TObject)
  private
    FList: TList<TTMSSoundexContainerCollection>;
  protected
    function GetCount: integer;
    function GetItem(Index: integer): TTMSSoundexContainerCollection;
  public
    AlphabetValues: array [0 .. 65535] of Char;
    property Items[index: integer]: TTMSSoundexContainerCollection read GetItem; default;
    property Count: integer read GetCount;
    function IndexOf(SoundexTableName: string): integer;
    procedure Reset();
    procedure Clear;
    function AddSoundexCollection(Name: string): TTMSSoundexContainerCollection;
    procedure AddSondexData(Value: string);
    procedure RemoveSoundex(SoundexTableName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Names: string; Stream: TStream);
    procedure SaveToFile(Names: String; Filename: string);
    procedure LoadFromFile(Filename: string);
    procedure LoadFromResource(Instance: HINST; Name: String);
    // function FrenchSoundex(InputStr: string): string;
    function Soundex(Process: TTMSSoundexProcess; InputStr: string): String;
    function SoundexValidation(InputStr: String): integer;
    procedure Apply(Name: string = '');
    constructor Create;
    destructor Destroy; override;
  end;

  // Returns the Soundex code for the specified string.
function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
function DutchSoundex(var Alphs: array of Char;InputStr: string): string;
function ItalianSoundex(var Alphs: array of Char; InputStr:String):string;
function OExSoundex(const InpStr: String): string;
function ExSoundex(var Alphs: array of Char; InpStr: String): string;
function EditDistance(Str1, Str2: String;NoRemoveAccent:boolean=false): Integer;

//function EditDistance(Word1, Word2: String): integer;
function GermanSoundex(InputStr: string): string;
function FrenchSoundex(var Alphs: array of Char; InputStr: string): string;
function FixTextForDict(Value: string): string;
function SpanishSoundex(var Alphs: array of Char; InputStr:String):string;
function GetFlagsDict(Value: string): string;
function GetLangIndex(Value: string): integer;

{$R TMSSpellCheckSoundex.res}

implementation

{ TTMSSoundexContainer }

uses
  TMSSpellCheckUtil;

const
  acset1='çäöüéèêàáâëÑÁÉÍÓÚÀÈÌÒÙÜîáéíóúÁÉÍÓÚaeioôuôîÿûÎãûùùÅÅÈåÅïé';
  acset2='caoueeeaaaeNAEIOUAEIOUUiaeiouaeiouaeioouoiyuiauuuaaeaaie';

var
  aclow,achigh,aclow2,achigh2:string;
  acInit:boolean=false;

function RemoveAccent(Value:string):string;
var
  res,I: Integer;
begin
  if not acInit then
  begin
    aclow := LowerCase(acset1,TLocaleOptions.loUserLocale);
    achigh := UpperCase(acset1,TLocaleOptions.loUserLocale);
    aclow2 := LowerCase(acset2,TLocaleOptions.loUserLocale);
    achigh2 := UpperCase(acset2,TLocaleOptions.loUserLocale);
    acInit := true;
  end;

  Result := ReplaceStr(Value,'ß','sS');

  for I := 1 to Length(Value) do
  begin
    res := Pos(Value[I],acLow);
    if res <> 0 then
    begin
      Result[I] := aclow2[res];
      continue;
    end;

    res := Pos(Value[I],acHigh);
    if res<>0 then
    begin
      Result[I] := achigh2[res];
      continue;
    end;
  end;
end;

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

function FixTextForDict(Value: string): string;
var
  I: integer;
begin
  for I := 1 to Length(Value) do
  begin
    if (CharInSetX(CharInStr(Value, I), ['0','1','2','3','4','5','6','7','8','9'])) or (CharInStr(Value, I) < #32) then
    begin
      Exit(Trim(Copy(Value, 1, I - 1)));
    end;
    if Copy(Value, I,2)='%%' then
    begin
      Exit(Trim(Copy(Value, 1, I - 1)));
    end;
    if CharInSetX(CharInStr(Value, I), ['/']) then
    begin
      Exit(Trim(Copy(Value, 1, I - 1)));
    end;
  end;
  Exit(Trim(Value));
end;

function GetFlagsDict(Value: string): string;
begin
  if Pos('%%',Value)<>0 then
   Value:=Copy(Value,1,Pos('%%',Value)-1);
  if Pos('/',Value)<>0 then
    Exit(Trim(Copy(Value,Pos('/',Value)+1,65535)))
  else
    Exit('');
end;

function GetLangIndex(Value: string): integer;
begin
  Result:=0;
  if Pos('%%',Value)<>0 then
   Result:=Word(Copy(Value,Pos('%%',Value)+2,65535)[1])-32;
end;

constructor TTMSSoundexContainer.Create(Collection: TCollection);
begin
  Inherited;
end;

destructor TTMSSoundexContainer.Destroy;
begin
  Inherited;
end;

{ TTMSSoundexContainerCollection }
function TTMSSoundexContainerCollection.GetItem(Index: integer)
  : TTMSSoundexContainer;
begin
  Exit(TTMSSoundexContainer(inherited Items[Index]));
end;

function TTMSSoundexContainerCollection.Add: TTMSSoundexContainer;
begin
  Exit(TTMSSoundexContainer(Inherited Add));
end;

function TTMSSoundexContainerCollection.Insert(Index: integer)
  : TTMSSoundexContainer;
begin
  Exit(TTMSSoundexContainer(Inherited Insert(Index)));
end;

constructor TTMSSoundexContainerCollection.Create;
begin
  inherited Create(TTMSSoundexContainer);
  // FEnabled:=true;
end;

destructor TTMSSoundexContainerCollection.Destroy;
begin
  Inherited;
end;

{ TTMSoundexManager }
function TTMSoundexManager.GetCount: integer;
begin
  Exit(FList.Count);
end;

function TTMSoundexManager.GetItem(Index: integer)
  : TTMSSoundexContainerCollection;
begin
  Exit(TTMSSoundexContainerCollection(FList[Index]));
end;

function TTMSoundexManager.IndexOf(SoundexTableName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if LowerCase(Self[I].Name) = LowerCase(SoundexTableName) then
      Exit(I)
  end;
end;

procedure TTMSoundexManager.Clear;
var
  I: integer;
  c: TTMSSoundexContainerCollection;
begin
  for I := 0 to Count - 1 do
  begin
    c := Items[I];
    c.Free;
  end;
  FList.Clear;
end;

procedure TTMSoundexManager.Reset();
var
  I: integer;
begin
  LoadFromResource(HInstance, 'SOUNDEXDEFAULT');
  for I := 0 to Length(AlphabetValues) - 1 do
    AlphabetValues[I] := #255;
  for I := 0 to Count - 1 do
    Items[I].Enabled := false;
  AlphabetValues[Word('.')] := '0';
  AlphabetValues[Word('-')] := '0';
  AlphabetValues[Word('–')] := '0';
  AlphabetValues[Word('''')] := '0';
  AlphabetValues[Word(' ')] := '0';
end;

function TTMSoundexManager.SoundexValidation(InputStr: String): integer;
var
  T1, T2: boolean;
  I: integer;
begin
  InputStr:=Uppercase(InputStr,TLocaleOptions.loUserLocale);
  T1 := false;
  T2 := false;
  InputStr := UpperCase(InputStr);
  for I := 1 to Length(InputStr) do
  begin
    if AlphabetValues[Word(CharInStr(InputStr, I))] = #255 then
      T2 := true;
    if AlphabetValues[Word(CharInStr(InputStr, I))] <> #255 then
      T1 := true;
  end;
  if T1 and T2 then
    Exit(0);

  if T1 and not T2 then
    Exit(1);

  if Not T1 and T2 then
    Exit(2);
  Exit(0);
end;

function SpanishSoundex(var Alphs: array of Char; InputStr:String):string;
var
  s1:string;
  rep,s2:string;
  i:integer;

begin
  s1 := 'ÑÁÉÍÓÚÀÈÌÒÙÜ';
  s2 := 'NAEIOUAEIOUU';

  InputStr := Trim(UpperCase(InputStr,TLocaleOptions.loUserLocale));

  if Copy(Inputstr,1,1) = 'H' then
    InputStr:=Copy(Inputstr,2,65535);
  if InputStr = '' then
    Exit('');

  for I := 1 to Length(InputStr) do
    if Pos(InputStr[I],S1)<>0 then
      InputStr[I]:=s2[Pos(InputStr[I],S1)];

  s1 := Copy(InputStr,1,1);
  s2 := Copy(inputStr,2,1);
  rep := s1;
  if s1 = 'V' then
    rep := 'B';
  if (s1 = 'Z') or (s1 = 'X') then
    rep := 'S';
  if (S1 = 'G') and ((S2 = 'E') or (S2 = 'I')) then
    rep := 'J';

  if (S1 = 'C') and ((S2 = 'H') or (S2 = 'E') or (S2 = 'I')) then
    rep := 'K';
  InputStr := rep + Copy(Inputstr,2,65535);
	InputStr := ReplaceStr(InputStr,'CH','V');
	InputStr := ReplaceStr(InputStr,'QU','K');
	InputStr := ReplaceStr(InputStr,'LL','J');
	InputStr := ReplaceStr(InputStr,'CE','S');
	InputStr := ReplaceStr(InputStr,'CI','S');
	InputStr := ReplaceStr(InputStr,'YA','J');
	InputStr := ReplaceStr(InputStr,'YE','J');
	InputStr := ReplaceStr(InputStr,'YI','J');
	InputStr := ReplaceStr(InputStr,'YO','J');
	InputStr := ReplaceStr(InputStr,'YU','J');
	InputStr := ReplaceStr(InputStr,'GE','J');
	InputStr := ReplaceStr(InputStr,'GI','J');
	InputStr := ReplaceStr(InputStr,'NY','N');
  Exit(ExSoundex(Alphs, InputStr));
end;

function ItalianSoundex(var Alphs: array of Char; InputStr:String): string;
var
  s1: string;
  s2: string;
  i: integer;
begin
  s1 := 'ÑÁÉÍÓÚÀÈÌÒÙÜ';
  s2 := 'NAEIOUAEIOUU';
  InputStr := Trim(UpperCase(InputStr,TLocaleOptions.loUserLocale));
  if Copy(Inputstr,1,1)='H' then
    InputStr := Copy(Inputstr,2,65535);
  if InputStr = '' then
    Exit('');
  for I := 1 to Length(InputStr) do
    if Pos(InputStr[I],S1) <> 0 then
      InputStr[I] := s2[Pos(InputStr[I],S1)];

  InputStr := ReplaceStr(InputStr,'GLI','LLI');
  InputStr := ReplaceStr(InputStr,'GN','N');
  InputStr := ReplaceStr(InputStr,'X','SS');
  InputStr := ReplaceStr(InputStr,'CS','SS');
  InputStr := ReplaceStr(InputStr,'KS','SS');
  InputStr := ReplaceStr(InputStr,'GS','SS');
  InputStr := ReplaceStr(InputStr,'JS','SS');
  InputStr := ReplaceStr(InputStr,'QS','SS');
  InputStr := ReplaceStr(InputStr,'CZ','SS');
  InputStr := ReplaceStr(InputStr,'KZ','SS');
  InputStr := ReplaceStr(InputStr,'GZ','SS');
  InputStr := ReplaceStr(InputStr,'JZ','SS');
  InputStr := ReplaceStr(InputStr,'QZ','SS');
  Exit(ExSoundex(Alphs, InputStr));
end;

function DutchSoundex(var Alphs: array of Char;InputStr: string): string;
begin
  InputStr := Trim(Uppercase(InputStr,TLocaleOptions.loUserLocale));
  if InputStr = '' then
    Exit('');

  InputStr := ReplaceStr(InputStr,'QU','KW');
  InputStr := ReplaceStr(InputStr,'SCH','SEE');
  InputStr := ReplaceStr(InputStr,'KS','XX');
  InputStr := ReplaceStr(InputStr,'KX','XX');
  InputStr := ReplaceStr(InputStr,'KC','KK');
  InputStr := ReplaceStr(InputStr,'CK','KK');
  InputStr := ReplaceStr(InputStr,'DT','TT');
  InputStr := ReplaceStr(InputStr,'TD','TT');
  InputStr := ReplaceStr(InputStr,'CH','GG');
  InputStr := ReplaceStr(InputStr,'SZ','SS');
  InputStr := ReplaceStr(InputStr,'IJ','YY');
  Exit(ExSoundex(Alphs, InputStr));
end;

function GermanSoundex(InputStr: string): string;
var
  code, char0, char1, char2: string;
  I: integer;
begin
  InputStr := LowerCase(InputStr);

  // Umwandlung: v->f, w->f, j->i, y->i, ph->f, ä->a, ö->o, ü->u, ß->ss, é->e, è->e, ê->e, à->a, á->a, â->a, ë->e

  InputStr := ReplaceStr(InputStr, 'ç', 'c');
  InputStr := ReplaceStr(InputStr, 'v', 'f');
  InputStr := ReplaceStr(InputStr, 'w', 'f');
  InputStr := ReplaceStr(InputStr, 'j', 'i');
  InputStr := ReplaceStr(InputStr, 'y', 'i');
  InputStr := ReplaceStr(InputStr, 'ph', 'f');
  InputStr := ReplaceStr(InputStr, 'ä', 'a');
  InputStr := ReplaceStr(InputStr, 'ö', 'o');
  InputStr := ReplaceStr(InputStr, 'ü', 'u');

  InputStr := ReplaceStr(InputStr, 'ß', 'ss');
  InputStr := ReplaceStr(InputStr, 'é', 'e');
  InputStr := ReplaceStr(InputStr, 'è', 'e');
  InputStr := ReplaceStr(InputStr, 'ê', 'e');

  InputStr := ReplaceStr(InputStr, 'à', 'a');
  InputStr := ReplaceStr(InputStr, 'á', 'a');
  InputStr := ReplaceStr(InputStr, 'â', 'a');
  InputStr := ReplaceStr(InputStr, 'ë', 'e');

  // Nur Buchstaben (keine Zahlen, keine Sonderzeichen)
  // word = Regex.Replace(word, @'[^A-Za-z ]+', '');

  // Wir hängen bei 1-buchstabigen Strings ein Leerzeichen an, sonst funktioniert die Anlautprüfung auf den zweiten Buchstaben nicht.
  // if (word.Length == 1)  word += ' '; }

  // Sonderfälle bei Wortanfang (Anlaut)

  if (Copy(InputStr, 1, 1) = 'c') then
  begin
    // vor a,h,k,l,o,q,r,u,x
    case (CharInStr(InputStr, 2)) of
      'a', 'h', 'k', 'l', 'o', 'q', 'r', 'u', 'x':
        code := '4'
    else
      code := '8'
    end;
    I := 2;
  end
  else
    I := 1;

  // * Umwandlungstabelle:
  // * ============================================
  // * Buchstabe      Kontext                  Code
  // * -------------  -----------------------  ----
  // * A,E,I,J,O,U,Y                            0
  // * H                                        -
  // * B                                        1
  // * P              nicht vor H               1
  // * D,T            nicht vor C,S,Z           2
  // * F,V,W                                    3
  // * P              vor H                     3
  // * G,K,Q                                    4
  // * C              im Wortanfang
  // *                vor A,H,K,L,O,Q,R,U,X     4
  // * C              vor A,H,K,O,Q,U,X
  // *                ausser nach S,Z           4
  // * X              nicht nach C,K,Q         48
  // * L                                        5
  // * M,N                                      6
  // * R                                        7
  // * S,Z                                      8
  // * C              nach S,Z                  8
  // * C              im Wortanfang ausser vor
  // *                A,H,K,L,O,Q,R,U,X         8
  // * C              nicht vor A,H,K,O,Q,U,X   8
  // * D,T            vor C,S,Z                 8
  // * X              nach C,K,Q                8
  // * --------------------------------------------

  while (I <= Length(InputStr)) do
  begin
    char1 := Copy(InputStr, I, 1);
    char0 := Copy(InputStr, I - 1, 1);
    char2 := Copy(InputStr, I + 1, 1);
    if char0 = '' then
      char0 := #0;
    if char1 = '' then
      char1 := #0;
    if char2 = '' then
      char2 := #0;
    if (CharInSetX(CharInStr(char1, 1), ['a', 'e', 'i', 'o', 'u'])) then
      code := code + '0';
    if (CharInSetX(CharInStr(char1, 1), ['b', 'p'])) then
      code := code + '1';
    if (CharInSetX(CharInStr(char1, 1), ['d', 't'])) then
    begin
      if CharInSetX(CharInStr(char2, 1), ['c', 's', 'z']) then
        code := code + '8'
      else
        code := code + '2';
    end;
    if (CharInSetX(CharInStr(char1, 1), ['f'])) then
      code := code + '3';
    if (CharInSetX(CharInStr(char1, 1), ['g', 'k', 'q'])) then
      code := code + '4';
    if (CharInSetX(CharInStr(char1, 1), ['c'])) then
    begin
      if CharInSetX(CharInStr(char2, 1), ['a', 'h', 'k', 'o', 'q', 'u', 'x']) then
      begin
        if CharInSetX(CharInStr(char2, 1), ['s', 'z']) then
          code := code + '8'
        else
          code := code + '4';
      end
      else
        code := code + '8';
    end;
    if (CharInSetX(CharInStr(char1, 1), ['x'])) then
    begin
      if (I > 1) then
      begin
        if (I > 1) and (CharInSetX(CharInStr(char0, 1), ['c', 'x', 'q'])) then
          code := code + '8'
        else
          code := code + '48';
      end;
    end;
    if (CharInSetX(CharInStr(char1, 1), ['l'])) then
      code := code + '5';
    if (CharInSetX(CharInStr(char1, 1), ['m', 'n'])) then
      code := code + '6';
    if (CharInSetX(CharInStr(char1, 1), ['r'])) then
      code := code + '7';
    if (CharInSetX(CharInStr(char1, 1), ['s', 'z'])) then
    begin
      code := code + '8';
    end;
    inc(I);
  end;

  Result := Copy(code, 1, 1);
  code := ReplaceStr(code, '0', '');
  char0 := '';
  for I := 2 to Length(code) do
  begin
    char1 := Copy(code, I, 1);
    if char0 <> char1 then
      Result := Result + char1;
    char0 := char1;
  end;
  Result := Result;//{ UpperCase(Copy(InputStr, 1, 1) + }Copy(Result, 1, 4) + Copy('0000', 1,
//    4 - Length(Copy(Result, 1, 3)));
end;

function FrenchSoundex(var Alphs: array of Char; InputStr: string): string;
begin
  InputStr := UpperCase(InputStr);
  InputStr := ReplaceStr(InputStr, 'GUI', 'KI');
  InputStr := ReplaceStr(InputStr, 'GUE', 'KE');
  InputStr := ReplaceStr(InputStr, 'GO', 'KO');
  InputStr := ReplaceStr(InputStr, 'GA', 'KA');
  InputStr := ReplaceStr(InputStr, 'GU', 'K');
  InputStr := ReplaceStr(InputStr, 'CA', 'KA');
  InputStr := ReplaceStr(InputStr, 'CO', 'KO');
  InputStr := ReplaceStr(InputStr, 'CU', 'KU');
  InputStr := ReplaceStr(InputStr, 'Q', 'K');
  InputStr := ReplaceStr(InputStr, 'CC', 'K');
  InputStr := ReplaceStr(InputStr, 'CK', 'K');

  if Copy(InputStr, 1, 3) = 'MAC' then
    InputStr := 'MCC' + Copy(InputStr, 4, 65535);
  if Copy(InputStr, 1, 3) = 'ASA' then
    InputStr := 'AZA' + Copy(InputStr, 4, 65535);
  if Copy(InputStr, 1, 3) = 'KN' then
    InputStr := 'NN' + Copy(InputStr, 4, 65535);
  if Copy(InputStr, 1, 3) = 'PF' then
    InputStr := 'FF' + Copy(InputStr, 4, 65535);
  if Copy(InputStr, 1, 3) = 'SCH' then
    InputStr := 'SSS' + Copy(InputStr, 4, 65535);
  if Copy(InputStr, 1, 3) = 'PH' then
    InputStr := 'FF' + Copy(InputStr, 4, 65535);

  Exit(ExSoundex(Alphs, InputStr));
end;

function TTMSoundexManager.Soundex(Process: TTMSSoundexProcess;
  InputStr: string): String;
begin
  InputStr:=Uppercase(InputStr,TLocaleOptions.loUserLocale);
  case Process of
    spStandard:
      Exit(ExSoundex(AlphabetValues, InputStr));
    spGerman:
      Exit(GermanSoundex(InputStr));
    spFrench:
      Exit(FrenchSoundex(AlphabetValues, InputStr));
    spSpanish:
      Exit(SpanishSoundex(AlphabetValues, InputStr));
    spItalian:
      Exit(ItalianSoundex(AlphabetValues, InputStr));
    spDutch:
      Exit(DutchSoundex(AlphabetValues, InputStr));
  end;
end;

function TTMSoundexManager.AddSoundexCollection(Name: string): TTMSSoundexContainerCollection;
var
  I: integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
    Exit(Items[I]);

  Result := TTMSSoundexContainerCollection.Create;
  Result.Name := Name;
  FList.Add(Result);
end;

procedure TTMSoundexManager.AddSondexData(Value: string);
var
  s: string;
  st: TStringList;
  cont: TTMSSoundexContainer;
  I: integer;
  cs: TTMSSoundexContainerCollection;
begin
  if Copy(Value, 1, 1) = #$FEFF then
    Value := Copy(Value, 2, Length(Value));
  st := TStringList.Create;
  st.Text := Value;
  try
    cs := nil;
    for I := 0 to st.Count - 1 do
    begin
      if Trim(st[I]) = '' then
        continue;
      if Copy(st[I], 1, 1) = '[' then
      begin
        s := ReplaceStr(st[I], ']', '');
        s := Trim(ReplaceStr(s, '[', ''));
        if s = '' then
          raise Exception.Create('Bad Soundex Source Format ' + st[I]);
        cs := AddSoundexCollection(s);
        cs.Clear;
        continue;
      end;
      if cs = nil then
        raise Exception.Create('Bad Soundex Source Format');

      if (Pos(',', st[I]) = 0) and (Pos('=', st[I]) = 0) then
        raise Exception.Create('Bad Soundex Source Format ' + st[I]);
      if (Pos('=', st[I]) <> 0) then
      begin
        s := Copy(st[I], 1, Pos('=', st[I]) - 1);
        if s = 'LIKE' then
        begin
          s := Copy(st[I], Pos('=', st[I]) + 1, 65535);
          if s = 'GERMAN' then;
          if s = 'FRENCH' then;
        end;
        continue;
      end;
      s := Copy(st[I], 1, Pos(',', st[I]) - 1);
      s := UpperCase(Trim(ReplaceStr(s, '''', '')));
      s := UpperCase(Trim(ReplaceStr(s, ' ', '')));
      cont := cs.Add;
      cont.Characters := Uppercase(s,TLocaleOptions.loUserLocale);
      s := Copy(st[I], Pos(',', st[I]) + 1, 65535);
      cont.Value := Char(StrToInt(s) + 48);
    end;
  finally
    st.Free;
  end;
end;

procedure TTMSoundexManager.RemoveSoundex(SoundexTableName: string);
var
  I: integer;
  c: TTMSSoundexContainer;
begin
  I := IndexOf(SoundexTableName);
  if I <> -1 then
  begin
    c := TTMSSoundexContainer(FList[I]);
    c.Free;
    FList.Delete(I);
  end;
end;

procedure TTMSoundexManager.LoadFromResource(Instance: HINST; Name: String);
var
  Stream: TResourceStream;
begin
  try
    Stream := TResourceStream.Create(HInstance, Name, RT_RCDATA);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    on e: Exception do
      ShowMessage(e.Message)
  end;
end;

procedure TTMSoundexManager.Apply(Name: string = '');
var
  I: integer;
  C: integer;
  X: integer;
begin
  if NAME = 'STANDARD' then
    NAME := 'ENGLISH';
  for I := 0 to Count - 1 do
  begin
    if (Items[I].Enabled and (Name = '')) or
      (UpperCase(Self[I].Name) = UpperCase(Name)) or
      (UpperCase(Self[I].Name) = UpperCase(Name)) then
      for C := 0 to Items[I].Count - 1 do
      begin
        for X := 1 to Length(Items[I].Items[C].Characters) do
          AlphabetValues[Word(CharInStr(Items[I].Items[C].FCharacters, X))] :=
            Char(Items[I].Items[C].Value);
      end;
  end;

  AlphabetValues[Word('.')] := '0';
  AlphabetValues[Word('-')] := '0';
  AlphabetValues[Word('–')] := '0';
  AlphabetValues[Word('''')] := '0';
  AlphabetValues[Word(' ')] := '0';

  { for I := 65 to Word('Z') do
    AlphabetValues[I] := Alphs[Char(I)]; }
end;

procedure TTMSoundexManager.LoadFromStream(Stream: TStream);
var
  str: TStringStream;
begin
  Stream.Position := 0;
  str := TStringStream.Create('', TEncoding.Unicode);
  str.CopyFrom(Stream, Stream.Size);
  try
    AddSondexData(str.DataString);
  finally
    str.Free;
  end;
end;

procedure TTMSoundexManager.SaveToStream(Names: string; Stream: TStream);
begin

end;

procedure TTMSoundexManager.SaveToFile(Names: String; Filename: string);
var
  str: TFileStream;
begin
  str := TFileStream.Create(Filename, fmOpenRead);
  try
    SaveToStream(Names, str);
  finally
    str.Free;
  end;
end;

procedure TTMSoundexManager.LoadFromFile(Filename: string);
var
  str: TFileStream;
begin
  str := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(str);
  finally
    str.Free;
  end;
end;

constructor TTMSoundexManager.Create;
begin
  inherited;
  FList := TList<TTMSSoundexContainerCollection>.Create;
  Reset;
end;

destructor TTMSoundexManager.Destroy;
var
  I: integer;
  c: TTMSSoundexContainerCollection;
begin
  for I := 0 to Count - 1 do
  begin
    c := Items[I];
    c.Free;
  end;

  FList.Free;
  inherited;
end;

{ [ARABIC]
  'اأإآحعغشويه',0
  'فب' , 1
  'چخجزسصظقكگش', 2
  'تثدذضط', 3
  'ل' ,4
  'من' ,5
  'ر', 6
  [HEBREW]
  'בוףפ' ,1
  'גזחךכסקש', 2
  'דטת', 3
  'ץצ', 32
  'ל', 4
  'םמןנ', 5
  'ר', 6
  [ENGLISH]
  'aeiouhwy',0
  'bfpv',1
  'cgjkqsxz',2
  'dt',3
  'l',4
  'dt',5
  'r',6
  [GREEK]
  'ΒΠΦ', 1
  'Ψ',12
  'ΓΖΚΞΣΧ',2
  'ΔΘΤ',3
  'Λ',4
  'ΜΝ',5
  'Ρ',6
  'ΑΕΗΙΟΥΩ'
  [RUSSIAN]
  'БВПФ',1
  'ГЖЗКСХЧШЩ', 2
  'ДТ' 3
  'Ц',32
  'Л',4
  'МН',5
  'Р', 6
  'АЕЁИЙОУЪЫЬЭЮЯ' }

{function EditDistance(Word1, Word2: String): integer;
var
  lev: array of array of integer;
  I, j: integer;
begin
  // If the words are identical, do nothing
  Word1:=Trim(Uppercase(Word1,TLocaleOptions.loUserLocale));
  Word2:=Trim(Uppercase(Word2,TLocaleOptions.loUserLocale));
  if Word1 = Word2 then
  begin
    Result := 0;
    Exit;
  end;

  SetLength(lev, Length(Word1) + 1);
  for I := low(lev) to high(lev) do
    SetLength(lev[I], Length(Word2) + 1);

  for I := low(lev) to high(lev) do
    lev[I][0] := I;
  for j := low(lev[low(lev)]) to high(lev[low(lev)]) do
    lev[0][j] := j;

  for I := low(lev) + 1 to high(lev) do
    for j := low(lev[I]) + 1 to high(lev[I]) do
      lev[I][j] := min(min(lev[I - 1][j] + 1, lev[I][j - 1] + 1),
        lev[I - 1][j - 1] + ifthen(Word1[I] = Word2[j], 0, 1));

  Result := lev[Length(Word1)][Length(Word2)];
end;}

function EditDistance( Str1, Str2: String;NoRemoveAccent:boolean=false): Integer;

  function Min(const A, B, C: Integer): Integer;
  begin
    Result := A;
    if B < Result then
      Result := B;
    if C < Result then
      Result := C;
  end;

var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  if not NoRemoveAccent then  begin
    Str1:=RemoveAccent(Str1);
    Str2:=RemoveAccent(Str2);
  end;

  Str2:=UpperCase(Str2);
  Str1:=UpperCase(Str1);
  Str1:=ReplaceStr(Str1,'-','');
  Str1:=ReplaceStr(Str1,'"','');
  Str1:=ReplaceStr(Str1,'''','');
  Str1:=ReplaceStr(Str1,'.','');

  Str2:=ReplaceStr(Str2,'-','');
  Str2:=ReplaceStr(Str2,'"','');
  Str2:=ReplaceStr(Str2,'''','');
  Str2:=ReplaceStr(Str2,'.','');

  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // save a bit memory by making the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;
    pStr1 := PChar(Str2);
    pStr2 := PChar(Str1);
  end
  else
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
  end;

  // bypass leading identical characters
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // bypass trailing identical characters
  while (LenStr2 <> 0) and ((pStr1 + LenStr1 - 1)^ = (pStr2 + LenStr2 - 1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // is the shorter string empty? so, the edit distance is length of the longer one
  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;

  // calculate the edit distance
  GetMem(D, (LenStr2 + 1) * SizeOf(Integer));

  for I := 0 to LenStr2 do
    D[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I - 1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + Min(Cost, PrevCost, D[J]);
      PrevCost := D[J];
      D[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D[LenStr2   ];
  FreeMem(D);
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    if IgnoreCase then
      Distance := EditDistance(LowerCase(Str1), LowerCase(Str2))
    else
      Distance := EditDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
end;

function OExSoundex(const InpStr: String): string;
var
  vStr: String;
  vCh1: Char;
  I: Word;
begin
  // Store the given InpStr in local variable in uppercase
  vStr := '';
  for I := 1 to Length(InpStr) do
    vStr := vStr + UpCase(CharInStr(InpStr, I));

  // Replace all occurances of "PH" with "F"
  I := Pos('PH', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 2);
    Insert('F', vStr, I);
    I := Pos('PH', vStr);
  end;

  // Replace all occurances of "CHR" with "CR"
  I := Pos('CHR', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 3);
    Insert('CR', vStr, I);
    I := Pos('CHR', vStr);
  end;

  // Replace all occurances of "Z" with "S"
  for I := 1 to Length(vStr) do
    if (CharInStr(vStr, I) = 'Z') then
      vStr[I] := 'S';

  // Replace all occurances of "X" with "KS"
  I := Pos('X', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 1);
    Insert('KS', vStr, I);
    I := Pos('X', vStr);
  end;

  // Remove all adjacent duplicates
  I := 2;
  while (I <= Length(vStr)) do
    if (CharInStr(vStr,I) = CharInStr(vStr, I - 1)) then
      Delete(vStr, I, 1)
    else
      inc(I);

  // Starting from 2nd char, remove all chars mapped to '0' in Alphs table
  // Remove all adjacent duplicates from assembled Soundex string
  I := 2;
  while (I <= Length(vStr)) do
    if (CharInStr(vStr, I) = CharInStr(vStr, I - 1)) then
      Delete(vStr, I, 1)
    else
      inc(I);

  I := 2;
  while (I <= Length(vStr)) do
    if (Alphs[CharInStr(vStr, I)] = '0') then
      Delete(vStr, I, 1)
    else
      inc(I);

  // Assemble Soundex string from Alphs table
  vCh1 := vStr[1];
  for I := 1 to Length(vStr) do
    SetCharInStr(vStr, I, Alphs[CharInStr(vStr, I)]);


  // Final assembly of Soundex string
  vStr := vCh1 + Copy(vStr, 2, 255);
  for I := Length(vStr) to 3 do
    vStr := vStr + '0';
  OExSoundex := string(UTF8Encode(vStr));
end;

function ExSoundex(var Alphs: array of Char;  InpStr: String): string;
var
  vStr: String;
  vCh1: Char;
  I: Word;

begin
  // Store the given InpStr in local variable in uppercase

  vStr := '';
  for I := 1 to Length(InpStr) do
    vStr := vStr + UpCase(CharInStr(InpStr,I));

  for I := 1 to Length(vStr) do
    if Alphs[Word(CharInStr(vStr,I))] = #255 then
      Exit('');

//  SetCharInStr(vStr, 1, Alphs[Word(CharInStr(vStr,1))]);
  vCh1 := Alphs[Word(CharInStr(vStr,1))];

  // Replace all occurances of "PH" with "F"
  I := Pos('PH', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 2);
    Insert('F', vStr, I);
    I := Pos('PH', vStr);
  end;

  // Replace all occurances of "CHR" with "CR"
  I := Pos('CHR', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 3);
    Insert('CR', vStr, I);
    I := Pos('CHR', vStr);
  end;

  // Replace all occurances of "Z" with "S"
  for I := 1 to Length(vStr) do
    if (CharInStr(vStr,I) = 'Z') then
      SetCharInStr(vStr,I, 'S');

  // Replace all occurances of "X" with "KS"
  I := Pos('X', vStr);
  while (I <> 0) do
  begin
    Delete(vStr, I, 1);
    Insert('KS', vStr, I);
    I := Pos('X', vStr);
  end;

  // Remove all adjacent duplicates
  I := 2;
  while (I <= Length(vStr)) do
    if (CharInStr(vStr,I) = CharInStr(vStr, I - 1)) then
      Delete(vStr, I, 1)
    else
      inc(I);

  // Starting from 2nd char, remove all chars mapped to '0' in Alphs table
  I := 2;
  while (I <= Length(vStr)) do
    if (Alphs[Word(CharInStr(vStr,I))] = '0') then
      Delete(vStr, I, 1)
    else
      inc(I);

  // Assemble Soundex string from Alphs table
  for I := 1 to Length(vStr) do
    SetCharInStr(vStr, I ,Alphs[Word(CharInStr(vStr,I))]);

  // Remove all adjacent duplicates from assembled Soundex string
  I := 2;
  while (I <= Length(vStr)) do
    if (CharInStr(vStr,I) = CharInStr(vStr,I - 1)) then
      Delete(vStr, I, 1)
    else
      inc(I);

  // Final assembly of Soundex string
  vStr := vCh1 + Copy(vStr, 2, 255);
  {for I := Length(vStr) to 3 do
    vStr := vStr + '0';}


  ExSoundex := string(UTF8Encode(vStr));
end;

end.
