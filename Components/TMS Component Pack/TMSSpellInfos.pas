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

unit TMSSpellInfos;

interface

uses
  Classes, Math, StrUtils, SysUtils, Types, Generics.Collections;

type
  TTMSSpellinfo = class;
  TTMSSpellInfoFlag = class;
  TTMSAffixFlagType = (attPrefix, attSuffix);

  TTMSAlternates = class(TCollectionitem)
  private
    FValue, FFirstString, FSecondString: string;
  protected
    procedure Setvalue(Value: string);
  public
    property Value: string read FValue write Setvalue;
    property FirstString: string read FFirstString write FFirstString;
    property SecondString: string read FSecondString write FSecondString;
    constructor Create(Collection: TCollection);override;
    destructor Destroy; override;
  end;

  TTMSAlternateCollection = class(TCollection)
  private
    FTypes: TStringList;
    FFormat: string;
  protected
    function GetItems(Index: integer): TTMSAlternates;
    procedure SetFormats(Value: string);
  public
    property Formats: string read FFormat write SetFormats;
    property Types: TStringList read FTypes write FTypes;
    property Items[Index: integer]: TTMSAlternates read GetItems; default;
    function Add: TTMSAlternates;
    function Insert(Index: integer): TTMSAlternates;
    function Supports(AType: string): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSAlternateFormats = class
  private
    FList: TList<TTMSAlternateCollection>;
  protected
    function GetItems(Index: Integer): TTMSAlternateCollection;
    function getByFormat(Name: string): TTMSAlternateCollection;
  public
    property Items[Index: Integer]: TTMSAlternateCollection
      read GetItems; default;
    property ByFormat[Name: string]: TTMSAlternateCollection read getByFormat;
    procedure Remove(Index: Integer);
    function Count: integer;
    procedure Clear;
    function Add: TTMSAlternateCollection;
    function Insert(Index: integer): TTMSAlternateCollection;
    function GetIndexByFormat(Value: string): TTMSAlternateCollection;
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSWordList = class
  private
    FNegative: boolean;
    FWordStr: string;
  public
    property Negative: boolean read FNegative write FNegative;
    property WordStr: string read FWordStr write FWordStr;
    constructor Create;
    destructor Destroy; override;
  end;

  TTMSFlagData = class(TCollectionitem)
  private
    FList: TList<TTMSWordList>;
    FValue, FConditions, FToReplace: string;
    FChoice: TStringList;
    procedure Setvalue(Value: string);
    procedure InterpretReplace(Value: string);
    procedure ClearAll;
    procedure Add(Value: string; Negative: boolean);
    procedure SetConditions(Value: string);
  protected
  public
    property Conditions: string read FConditions write SetConditions;
    property ToReplace: string read FToReplace write InterpretReplace;
    property Value: string read FValue write Setvalue;
    function Flag: TTMSSpellInfoFlag;
    function CompareAffix(Word: string): boolean;
    function DefaultChoice: String;
    function Suggestions(Word: string): String;
    function CompareSuffix(Word: string): boolean;
    function RemoveAffix(Word: string): string;
    function AddAffix(Word: string): string;
    function GetIsSupported(Word: String): boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TTMSFlagDataCollection = class(TCollection)
  private
    FOwner: TTMSSpellInfoFlag;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(Index: integer): TTMSFlagData;
  public
    property Items[Index: integer]: TTMSFlagData read GetItems; default;
    function Add: TTMSFlagData;
    function Insert(Index: integer): TTMSFlagData;
    Constructor Create(Owner: TTMSSpellInfoFlag);
    destructor Destroy; override;
  end;

  TTMSSpellInfoFlag = class(TCollectionitem)
  private
    FName: string;
    FType: TTMSAffixFlagType;
    FlagData: TTMSFlagDataCollection;
    FCrossProduct: boolean;
    procedure SetName(Value: string);
  protected
  public
    property CrossProduct: boolean read FCrossProduct write FCrossProduct;
    property Name: string read FName write SetName;
    property FlagType: TTMSAffixFlagType read FType write FType;
    property Data: TTMSFlagDataCollection read FlagData write FlagData;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TTMSSpellInfoFlagCollection = class(TCollection)
  private
    FOwner: TTMSSpellinfo;
    function GetIndexByFlagName(Value: String): integer;
    function GetFlagByname(Value: string): TTMSSpellInfoFlag;
    function GetItem(Value: integer): TTMSSpellInfoFlag;
  protected
    function GetOwner: TPersistent; override;
  public
    property ByName[Name: string]: TTMSSpellInfoFlag read GetFlagByname;
    property Items[Index: integer]: TTMSSpellInfoFlag read GetItem; default;
    function Add: TTMSSpellInfoFlag;
    function Insert(Index: integer): TTMSSpellInfoFlag;
    constructor Create(Owner: TTMSSpellinfo);
    destructor Destroy; override;
  end;

  TTMSSpellinfo = class(TPersistent)
  private
    LI, LC: integer;
    FSType: TTMSAffixFlagType;
    FSWord: string;
    FOwner: TPersistent;
    FFlags: TTMSSpellInfoFlagCollection;
    FAlternateFormats: TTMSAlternateFormats;
  protected
    function GetOwner: TPersistent; override;
  public
    function Encode(Format, Word: string): string;
    function Decode(Format, Word: string): string;
    function GetSuggestionsFor(Word: string; FlagsList: string): string;
    function GetPrefixSuggestionsFor(Word: string; FlagsList: string): string;
    function FindFirst(Word: string; AType: TTMSAffixFlagType): TTMSFlagData;
    function FindNext: TTMSFlagData;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(Filename: string);
    constructor Create(Owner: TPersistent);
    destructor Destroy; override;
  published
    property AlternateFormats: TTMSAlternateFormats read FAlternateFormats
      write FAlternateFormats;
    property Flags: TTMSSpellInfoFlagCollection read FFlags write FFlags;
  end;

implementation

uses
  TMSSpellCheckUtil;

{ TTMSWordList }

constructor TTMSWordList.Create;
begin
  inherited;
end;

destructor TTMSWordList.Destroy;
begin
  inherited;
end;

{ TTMSFlagData }

procedure TTMSFlagData.SetConditions(Value: string);
begin
  FChoice.Clear;
  Value := Trim(Value);
  Split(',', Value, FChoice);
  FConditions := Value;
end;

procedure TTMSFlagData.Add(Value: string; Negative: boolean);
var
  ta: TTMSWordList;
begin
  if Trim(Value) = '' then
    Exit;
  ta := TTMSWordList.Create;
  ta.FNegative := Negative;
  ta.FWordStr := Value;
  FList.Add(ta);
end;

procedure TTMSFlagData.InterpretReplace(Value: string);
var
  ist, i: integer;
  Negative: boolean;
  g, s: string;
begin
  s := Value;
  i := 1;
  // ig:=false;
  ist := -1;
  ClearAll;
  Negative := false;
  g := '';
  while i <= Length(s) do
  begin
    while (i <= Length(s)) and (CharInSetX(CharInStr(s,i), [#32, #9])) do
      Inc(i);
    if CharInStr(s,i) = '[' then
    begin
      ist := i + 1;
      Inc(i);
      continue;
    end;
    if (ist <> -1) and (CharInStr(s,i) = ']') then
    begin
      Add(Copy(s, ist, i - ist), Negative);
      Negative := false;
      ist := -1;
      Inc(i);
      continue;
    end;
    if (ist <> -1) and (CharInStr(s,i) = '^') then
    begin
      Negative := true;
      ist := ist + 1;
      Inc(i);
      continue;
    end;
    if (ist = -1) then
    begin
      ist := i;
      Negative := false;
      while (i <= Length(s)) and not CharInSetX(CharInStr(s,i), [#32, #9]) do
        Inc(i);
      Add(Copy(s, ist, i - ist), false);
      ist := -1;
      continue;
    end;
    Inc(i);
  end;
  FToReplace := Value;
end;

procedure TTMSFlagData.Setvalue(Value: string);
var
  s1, s2: string;
begin
  if Pos('>', Value) <> 0 then
  begin
    s1 := Copy(Value, 1, Pos('>', Value) - 1);
    s2 := Copy(Value, Pos('>', Value) + 1, 65535);
    ToReplace := s1;
    Conditions := s2;
  end;
end;

procedure TTMSFlagData.ClearAll;
var
  i: integer;
  w: TTMSWordList;
begin
  for i := 0 to FList.Count - 1 do
  begin
    w := TTMSWordList(FList[i]);
    w.Free;
  end;
  FList.Clear;
end;

function TTMSFlagData.Flag: TTMSSpellInfoFlag;
begin
  Exit(TTMSFlagDataCollection(Collection).FOwner);
end;

function TTMSFlagData.CompareAffix(Word: string): boolean;
var
  i: integer;
  w: char;
begin
  if CompareSuffix(Word) then
    Exit(true);
  if FList.Count = 0 then
    Exit(false);
  if CompareSuffix(Word) then
    Exit(true);
  if Flag.FlagType = attPrefix then
  begin
    Word := Trim(UpperCase(Word, TLocaleOptions.loUserLocale));
    Word := Copy(Word, 1, FList.Count);
  end
  else
  begin
    Word := Trim(UpperCase(Word, TLocaleOptions.loUserLocale));
    Word := Copy(Word, (Length(Word) - FList.Count) + 1, 65535);
  end;
  if Length(Word) <> FList.Count then
    Exit(false);
  for i := 1 to Length(Word) do
  begin
    w := Word[i];
    if (TTMSWordList(FList[i - 1]).WordStr = '.') then
      continue;
    if TTMSWordList(FList[i - 1]).Negative and
      (Pos(w, TTMSWordList(FList[i - 1]).WordStr) <> 0) then
      Exit(false);
    if Not TTMSWordList(FList[i - 1]).Negative and
      not(Pos(w, TTMSWordList(FList[i - 1]).WordStr) <> 0) then
      Exit(false);
  end;
  Exit(true);
end;

function TTMSFlagData.CompareSuffix(Word: string): boolean;
var
  i: integer;
  w, s, e: string;
  // w: char;
begin
  if Trim(Word) = '' then
    Exit(false);
  s := Trim(UpperCase(Word, TLocaleOptions.loUserLocale));
  for i := 0 to FChoice.Count - 1 do
  begin
    e := Trim(UpperCase(ReplaceStr(FChoice[i], '-', ''),
      TLocaleOptions.loUserLocale));
    { if Copy(FChoice[i],1,1)='-' then
      e:=''; }

    if Copy(s, (Length(s) - Length(e)) + 1, 65535) = e then
    begin
      if Copy(FChoice[i], 1, 1) = '-' then
        e := '';
      w := Copy(s, Length(s) - ((FList.Count - Length(e))) + 1,
        FList.Count - Length(e) );
      if e = '' then
        w := '';
      // Copy(s,[Length(s) - (FList.Count- Length(e)),FList.Count];
      // if DefaultChoice='' then begin
//      Result := true;
{      for x := 1 to Length(w) do
      begin
        if (TTMSWordList(FList[x - 1]).WordStr = '.') then
        begin
          continue;
        end;
        if TTMSWordList(FList[x - 1]).Negative and
          (Pos(w[x], TTMSWordList(FList[x - 1]).WordStr) = 0) then
        begin
          continue;
        end;
        if Not TTMSWordList(FList[x - 1]).Negative and
          (Pos(w[x], TTMSWordList(FList[x - 1]).WordStr) <> 0) then
        begin
          continue;
        end;
        Result := false;
        break;
      end;
      if Result then}
        Exit(true);
    end;
    // end;
  end;
  Exit(false);
end;

function TTMSFlagData.DefaultChoice: String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FChoice.Count - 1 do
    if Copy(Trim(FChoice[i]), 1, 1) = '-' then
      Exit(LowerCase(Copy(FChoice[i], 2, 65536), TLocaleOptions.loUserLocale));
end;

function TTMSFlagData.RemoveAffix(Word: string): string;
var
  i: integer;
  s, e: string;
 // w: char;
begin
  if Trim(Word) = '' then
    Exit('');
  s := Trim(UpperCase(Word, TLocaleOptions.loUserLocale));
  for i := 0 to FChoice.Count - 1 do
  begin
    e := Trim(UpperCase(ReplaceStr(FChoice[i], '-', ''),
      TLocaleOptions.loUserLocale));
    if Copy(s, (Length(s) - Length(e)) + 1, 65535) = e then
    begin
//      w := s[Length(s) - Length(e)];
//      if DefaultChoice = '' then
{        if (TTMSWordList(FList[0]).WordStr = '.') then
        begin
          if TTMSWordList(FList[0]).Negative and
            (Pos(w, TTMSWordList(FList[0]).WordStr) <> 0) then
            continue;
          if Not TTMSWordList(FList[0]).Negative and
            not(Pos(w, TTMSWordList(FList[0]).WordStr) <> 0) then
            continue;
        end;}
      if DefaultChoice <> '' then
        Exit(FixCap(Word, Copy(Word, 1, (Length(s) - Length(e))) +
          DefaultChoice))
      else
        Exit(FixCap(Word, Copy(Word, 1, (Length(s) - Length(e)))));
    end;
  end;
  Exit(FixCap(Word, Word));
end;

function TTMSFlagData.AddAffix(Word: string): string;
begin

end;

function TTMSFlagData.GetIsSupported(Word: String): boolean;
begin
  Exit(false);
end;

Constructor TTMSFlagData.Create(Collection: TCollection);
begin
  Inherited;
  FList := TList<TTMSWordList>.Create;
  FChoice := TStringList.Create;
end;

destructor TTMSFlagData.Destroy;
begin
  ClearAll;
  FreeAndNil(FChoice);
  FreeAndNil(FList);
  Inherited;
end;

function TTMSFlagData.Suggestions(Word: string): String;
var
  r: TStringList;
  i: integer;
  Value: string;

  procedure Add(AValue: string);
  begin
    if UpperCase(Value, TLocaleOptions.loUserLocale) <>
      UpperCase(AValue, TLocaleOptions.loUserLocale) then
      r.Add(AValue);
  end;

var
  default: string;
begin
  Result := '';
  if CompareAffix(Word) and (Flag.FlagType = attSuffix) then
  begin
    Value := RemoveAffix(Word);
    r := TStringList.Create;
    try
      r.Add(Value);
      default := DefaultChoice;
      if default <> '' then
        default := default;
      for i := 0 to FChoice.Count - 1 do
        if Copy(FChoice[i], 1, 1) = '-' then
          Add(FixCap(Word, Copy(Word, 1, (Length(Value) - Length(default))) +
            Trim(LowerCase(Copy(FChoice[i], 2, 65535),
            TLocaleOptions.loUserLocale))))
        else
          Add(FixCap(Word, Copy(Word, 1, (Length(Value) - Length(default))) +
            Trim(LowerCase(FChoice[i], TLocaleOptions.loUserLocale))));
    finally
      Result := r.Text;
      r.Free;
    end;
    Exit(Result);
  end;
  if CompareAffix(Word) and (Flag.FlagType = attPrefix) then
  begin
    r := TStringList.Create;
    try
      r.Add(Value);
      default := DefaultChoice;
      if default <> '' then
        default := default;
      for i := 0 to FChoice.Count - 1 do
        Add(FixCap(Word, Trim(LowerCase(FChoice[i], TLocaleOptions.loUserLocale)
          + Copy(Word, 1, (Length(Word) - Length(default))))));
    finally
      Result := r.Text;
      r.Free;
    end;
  end;
end;

{ TTMSFlagDataCollection }
function TTMSFlagDataCollection.GetItems(Index: integer): TTMSFlagData;
begin
  Exit(TTMSFlagData(Inherited Items[index]));
end;

function TTMSFlagDataCollection.GetOwner: TPersistent;
begin
  Exit(FOwner);
end;

function TTMSFlagDataCollection.Add: TTMSFlagData;
begin
  Exit(TTMSFlagData(Inherited Add));
end;

function TTMSFlagDataCollection.Insert(Index: integer): TTMSFlagData;
begin
  Exit(TTMSFlagData(Inherited Insert(Index)));
end;

Constructor TTMSFlagDataCollection.Create(Owner: TTMSSpellInfoFlag);
begin
  Inherited Create(TTMSFlagData);
  FOwner := Owner;
end;

destructor TTMSFlagDataCollection.Destroy;
begin
  Inherited;
end;
{ TTMSSpellInfoFlag }

procedure TTMSSpellInfoFlag.SetName(Value: string);
begin
  Value := Trim(Value);
  if Pos(':', Value) <> 0 then
    Value := Copy(Value, 1, Pos(':', Value) - 1);
  if Copy(Value, 1, 1) = '*' then
  begin
    Value := Copy(Value, 2, 65535);
    FCrossProduct := true;
  end;
  FName := Value;
end;

Constructor TTMSSpellInfoFlag.Create(Collection: TCollection);
begin
  Inherited;
  FlagData := TTMSFlagDataCollection.Create(Self);
end;

destructor TTMSSpellInfoFlag.Destroy;
begin
  FreeAndNil(FlagData);
  Inherited;
end;

{ TTMSSpellInfoFlagCollection }
function TTMSSpellInfoFlagCollection.GetIndexByFlagName(Value: String): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if UpperCase(Value, TLocaleOptions.loUserLocale) = Self[i].Name then
      Exit(i);
  end;
end;

function TTMSSpellInfoFlagCollection.GetItem(Value: integer): TTMSSpellInfoFlag;
begin
  Exit(TTMSSpellInfoFlag(Inherited Items[Value]));
end;

function TTMSSpellInfoFlagCollection.GetFlagByname(Value: string)
  : TTMSSpellInfoFlag;
var
  i: integer;
begin
  i := GetIndexByFlagName(Value);
  Result := nil;
  if i <> -1 then
    Exit(Self[i]);
end;

function TTMSSpellInfoFlagCollection.GetOwner: TPersistent;
begin
  Exit(FOwner);
end;

function TTMSSpellInfoFlagCollection.Add: TTMSSpellInfoFlag;
begin
  Exit(TTMSSpellInfoFlag(Inherited Add));
end;

function TTMSSpellInfoFlagCollection.Insert(Index: integer): TTMSSpellInfoFlag;
begin
  Exit(TTMSSpellInfoFlag(Inherited Insert(Index)));
end;

constructor TTMSSpellInfoFlagCollection.Create(Owner: TTMSSpellinfo);
begin
  Inherited Create(TTMSSpellInfoFlag);
  FOwner := Owner;
end;

destructor TTMSSpellInfoFlagCollection.Destroy;
begin
  Inherited;
end;

{ TTMSSpellinfo }
function TTMSSpellinfo.GetOwner: TPersistent;
begin
  Exit(FOwner);
end;

function TTMSSpellinfo.GetPrefixSuggestionsFor(Word: string;
  FlagsList: string): string;
var
  sr, st: TStringList;
  i: integer;
  cf: TTMSSpellInfoFlag;
  C: integer;
  e: integer;

  function Exists(Word: string): boolean;
  var
    x: integer;
  begin
    for x := 0 to st.Count - 1 do
      if Word = st[x] then
        Exit(true);
    Exit(false);
  end;

  procedure Add(Word: String);
  var
    x: integer;
    sts: TStringList;
  begin
    sts := TStringList.Create;
    sts.Text := Word;
    try
      for x := 0 to sts.Count - 1 do
      begin
        if Not Exists(sts[x]) then
          st.Add(sts[x]);
      end;
    finally
      sts.Free;
    end;
  end;

begin
  st := TStringList.Create;
  sr := TStringList.Create;
  try
    Add(Word + '/' + FlagsList);
    for i := 1 to Length(FlagsList) do
    begin
      cf := Flags.ByName[FlagsList[i]];
      if (cf <> nil) and (cf.FlagType = attPrefix) then
      begin
        for C := 0 to cf.Data.Count - 1 do
        begin
          if cf.Data[C].CompareAffix(Word) then
          begin
            sr.Text := cf.Data[C].Suggestions(Encode('dictionary',Word));
            for e := 0 to sr.Count - 1 do
            begin
              if cf.CrossProduct then
                Add(sr[e] + '/' + ReplaceStr(FlagsList, FlagsList[i], ''))
              else
                Add(sr[e]);
            end;
          end;
        end;
      end;
    end;
  finally
    Result := st.Text;
    st.Free;
    sr.Free;
  end;
end;

function TTMSSpellinfo.Encode(Format, Word: string): string;
var
  a: TTMSAlternateCollection;
  i: integer;
  {$IFDEF FMXLIB}
  x: RawByteString;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  x: string;
  {$ENDIF}
begin
  a := AlternateFormats.ByFormat['dictionary'];
  Result := Word;
  x := UTF8Encode(Word);
  Result := x;
  if a <> nil then
    for i := 0 to a.Count - 1 do
    begin
      Result := ReplaceStr(Result, UpperCase(a[i].FirstString,
        TLocaleOptions.loUserLocale), UpperCase(a[i].SecondString,
        TLocaleOptions.loUserLocale));
      Result := ReplaceStr(Result, LowerCase(a[i].FirstString,
        TLocaleOptions.loUserLocale), LowerCase(a[i].SecondString,
        TLocaleOptions.loUserLocale));
    end;
end;

function TTMSSpellinfo.Decode(Format, Word: string): string;
var
  a: TTMSAlternateCollection;
  i: integer;
begin
  a := AlternateFormats.ByFormat['dictionary'];
  Result := Word;
  if a <> nil then
    for i := 0 to a.Count - 1 do
    begin
      Result := ReplaceStr(Result, a[i].SecondString, a[i].FirstString);
    end;
end;

function TTMSSpellinfo.GetSuggestionsFor(Word: string;
  FlagsList: string): string;
var
  st: TStringList;
  i: integer;
  cf: TTMSSpellInfoFlag;
  C: integer;

  function Exists(Word: string): boolean;
  var
    x: integer;
  begin
    for x := 0 to st.Count - 1 do
      if Word = st[x] then
        Exit(true);
    Exit(false);
  end;

  procedure Add(Word: String);
  var
    x: integer;
    sts: TStringList;
  begin
    sts := TStringList.Create;
    sts.Text := Word;
    try
      for x := 0 to sts.Count - 1 do
      begin
        if Not Exists(sts[x]) then
          st.Add(sts[x]);
      end;
    finally
      sts.Free;
    end;
  end;

begin
  st := TStringList.Create;
  try
    // if FlagsList = '' then
    begin
      { for I := 0 to Flags.Count-1 do begin
        for C := 0 to Flags[I].Data.Count-1 do begin
        if Flags[I].Data[C].CompareAffix(Word) then
        Add(Flags[I].Data[C].Suggestions(Word));
        end;
        end; }
      st.Add(Word);
    end;
    for i := 1 to Length(FlagsList) do
    begin
      cf := Flags.ByName[CharInStr(FlagsList,i)];
      if cf <> nil then
      begin
        for C := 0 to cf.Data.Count - 1 do
        begin
          if (cf.Data[C].CompareAffix(Word)) and (cf.FlagType = attSuffix) then
            Add(cf.Data[C].Suggestions(Word));
        end;
      end;
    end;
  finally
    Result := st.Text;
    st.Free;
  end;
end;

function TTMSSpellinfo.FindFirst(Word: string; AType: TTMSAffixFlagType)
  : TTMSFlagData;
var
  i: integer;
  C: integer;
begin
  FSWord := Word;
  FSType := AType;
  for i := 0 to Flags.Count - 1 do
  begin
    if Flags[i].FlagType <> AType then
      continue;
    for C := 0 to Flags[i].Data.Count - 1 do
    begin
      if Flags[i].Data.Items[C].CompareSuffix(Word) then
      begin
        LC := C + 1;
        LI := i;
        Exit(Flags[i].Data.Items[C]);
      end;
    end;
  end;
  LI := -1;
  LC := -1;
  Exit(nil);
end;

function TTMSSpellinfo.FindNext: TTMSFlagData;
var
  i: integer;
  C: integer;
begin
  if LI = -1 then
    Exit(nil);
  for i := LI to Flags.Count - 1 do
  begin
    if Flags[i].FlagType <> FSType then
      continue;
    for C := LC to Flags[i].Data.Count - 1 do
    begin
      if Flags[i].Data.Items[C].CompareSuffix(FSWord) then
      begin
        LC := C + 1;
        LI := i;
        Exit(Flags[i].Data.Items[C]);
      end;
    end;
    LC := 0;
  end;
  LI := -1;
  LC := -1;
  Exit(nil);
end;

procedure TTMSSpellinfo.LoadFromStream(Stream: TStream);
var
  st: TStringList;
  i: integer;
  ct: TTMSAffixFlagType;
  vl: string;
  cf: TTMSSpellInfoFlag;
  cfd: TTMSFlagData;
  ac: TTMSAlternateCollection;
  aa: TTMSAlternates;
begin
  FFlags.Clear;
  Stream.Position := 0;
  st := TStringList.Create;
  AlternateFormats.Clear;
  Flags.Clear;
  try
    st.LoadFromStream(Stream);
    cf := nil;
//    cfd := nil;
    ct:=attPrefix;
    ac:=nil;
    for i := 0 to st.Count - 1 do
    begin
      if Pos('#', st[i]) <> 0 then
        vl := Copy(st[i], 1, Pos('#', st[i]) - 1)
      else
        vl := st[i];
      vl := Trim(ReplaceStr(vl, #9, ' '));
      if vl = '' then
        continue;
      if Trim(Copy(vl, 1, Pos(' ', vl) - 1)) = 'altstringtype' then
      begin
        ac := AlternateFormats.Add;
        ac.Formats := Trim(Copy(vl, Pos(' ', vl) + 1, 65535));
        cf := nil;
      end;
      if (Copy(vl, 1, Pos(' ', vl) - 1) = 'altstringchar') and (ac <> nil) then
      begin
        aa := ac.Add;
        aa.Value := Copy(vl, Pos(' ', vl));
      end;
      if Trim(st[i]) = 'suffixes' then
      begin
        ct := TTMSAffixFlagType.attSuffix;
        continue;
      end;
      if Trim(st[i]) = 'prefixes' then
      begin
        ct := TTMSAffixFlagType.attPrefix;
        continue;
      end;

      if Copy(Trim(vl), 1, 4) = 'flag' then
      begin
        ac := nil;
        cf := FFlags.Add;
        cf.Name := Copy(Trim(vl), 5, 50);
        cf.FlagType := ct;
        continue;
      end;
      if cf <> nil then
      begin
        if Pos('>', vl) <> 0 then
        begin
          cfd := cf.Data.Add;
          cfd.Value := vl;
        end;
        continue;
      end;

    end;
  finally
    st.Free;
  end;
end;

procedure TTMSSpellinfo.LoadFromFile(Filename: string);
var
  st: TFileStream;
begin
  st := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(st);
  finally
    st.Free;
  end;
end;

constructor TTMSSpellinfo.Create(Owner: TPersistent);
begin
  Inherited Create;
  FFlags := TTMSSpellInfoFlagCollection.Create(Self);
  FAlternateFormats := TTMSAlternateFormats.Create;
  LI := -1;
  LC := -1;
end;

destructor TTMSSpellinfo.Destroy;
begin
  FreeAndNil(FFlags);
  FreeAndNil(FAlternateFormats);
  Inherited;
end;

{ TTMSAlternates }

procedure TTMSAlternates.Setvalue(Value: string);
begin
  Value := Trim(Value);
  FirstString := UTF8ToWideString
    (Unescape(Trim(Copy(Value, 1, Pos(' ', Value) - 1))));
  SecondString := Unescape(Trim(Copy(Value, Pos(' ', Value) + 1, 65535)));
  FValue := Value;
end;

constructor TTMSAlternates.Create(Collection: TCollection);
begin
  Inherited Create(Collection);
end;

destructor TTMSAlternates.Destroy;
begin
  Inherited;
end;

{ TTMSAlternateCollection }
procedure TTMSAlternateCollection.SetFormats(Value: string);
var
  si, i: integer;
  fo: boolean;
  s: TStringList;
begin
  s := TStringList.Create;
  try
    si := 1;
    fo := false;
    Value := Trim(Value);
    for i := 1 to Length(Value) do
    begin
      if not fo and (CharInStr(Value,i) = '"') then
      begin
        fo := true;
        si := i;
        continue;
      end;
      if fo and (CharInStr(Value,i) = '"') then
      begin
        s.Add(Copy(Value, si, i - si));
        fo := false;
        continue;
      end;
    end;
    FTypes.Clear;
    for i := 0 to s.Count - 1 do
      FTypes.Add(ReplaceStr(s[i], '"', ''));
  finally
    s.Free;
  end;
end;

function TTMSAlternateCollection.GetItems(Index: integer): TTMSAlternates;
begin
  Exit(TTMSAlternates(Inherited Items[Index]));
end;

function TTMSAlternateCollection.Add: TTMSAlternates;
begin
  Exit(TTMSAlternates(Inherited Add));
end;

function TTMSAlternateCollection.Insert(Index: integer): TTMSAlternates;
begin
  Exit(TTMSAlternates(Inherited Add));
end;

function TTMSAlternateCollection.Supports(AType: string): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to FTypes.Count - 1 do
    if LowerCase(FTypes[i]) = LowerCase(AType) then
      Exit(true);
end;

constructor TTMSAlternateCollection.Create;
begin
  Inherited Create(TTMSAlternates);
  FTypes := TStringList.Create;
end;

destructor TTMSAlternateCollection.Destroy;
begin
  FreeAndNil(FTypes);
  Inherited;
end;

{ TTMSAlternateFormats }
function TTMSAlternateFormats.GetItems(Index: integer): TTMSAlternateCollection;
begin
  Exit(TTMSAlternateCollection(FList[Index]));
end;

function TTMSAlternateFormats.getByFormat(Name: string)
  : TTMSAlternateCollection;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Self[i].Supports(name) then
      Exit(Self[i]);
end;

procedure TTMSAlternateFormats.Remove(Index: integer);
var
  c: TTMSAlternateCollection;
begin
  c := TTMSAlternateCollection(FList[Index]);
  c.Free;
  FList.Delete(Index);
end;

function TTMSAlternateFormats.Count: integer;
begin
  Exit(FList.Count);
end;

procedure TTMSAlternateFormats.Clear;
var
  i: integer;
  c: TTMSAlternateCollection;
begin
  for i := 0 to FList.Count - 1 do
  begin
    c := TTMSAlternateCollection(FList[i]);
    c.Free;
  end;
  FList.Clear;
end;

function TTMSAlternateFormats.Add: TTMSAlternateCollection;
begin
  Result := TTMSAlternateCollection.Create();
  FList.Add(Result);
  Exit(Result);
end;

function TTMSAlternateFormats.GetIndexByFormat(Value: string)
  : TTMSAlternateCollection;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Self[i].Supports(Value) then
      Exit(Self[i]);
  end;
end;

function TTMSAlternateFormats.Insert(Index: Integer): TTMSAlternateCollection;
begin
  Result := TTMSAlternateCollection.Create();
  FList.Insert(Index, Result);
  Exit(Result);
end;

constructor TTMSAlternateFormats.Create;
begin
  Inherited;
  FList := TList<TTMSAlternateCollection>.Create;
end;

destructor TTMSAlternateFormats.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  Inherited;
end;

end.
