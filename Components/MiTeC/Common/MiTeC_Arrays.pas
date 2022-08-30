{*******************************************************}
{                   MiTeC Arrays                        }
{                  Common routines                      }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Arrays;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Classes, System.SysUtils, System.Math;
     {$ELSE}
     Windows, Classes, SysUtils, Math;
     {$ENDIF}

type
  TIntegerArray = record
    Items: array of integer;
    procedure Clear;
    function Add(AValue: integer): integer;
    procedure Delete(AIndex: integer);
    function Count: integer;
    function GetCommaText: string;
    procedure SetCommaText(const AValue: string);
    procedure Sort(Descending: Boolean = False);
  end;

  TFloatArray = record
    Items: array of double;
    procedure Clear;
    function Add(AValue: double): integer;
    procedure Delete(AIndex: integer);
    function Count: integer;
    procedure Sort(Descending: Boolean = False);
  end;

  TStringArray = record
    Items: array of string;
    procedure Clear;
    function Add(const AValue: string): integer;
    procedure Delete(AIndex: integer);
    function Count: integer;
    function GetDelimitedText(const ADelimiter: char = ','): string;
    procedure SetDelimitedText(const AValue: string; const ADelimiter: char = ',');
    procedure Sort(Descending: Boolean = False);
  end;

implementation

{ TIntegerArray }

function TIntegerArray.Add(AValue: integer): integer;
begin
  SetLength(Items,Length(Items)+1);
  Items[High(Items)]:=AValue;
  Result:=High(Items);
end;

procedure TIntegerArray.Clear;
begin
  Finalize(Items);
end;

function TIntegerArray.Count: integer;
begin
  Result:=Length(Items);
end;

procedure TIntegerArray.Delete(AIndex: integer);
var
  i: Integer;
begin
  for i:=AIndex to High(Items)-1 do
    Items[i]:=Items[i+1];
  SetLength(Items, High(Items));
end;

function TIntegerArray.GetCommaText: string;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i in Items do
      sl.Add(IntToStr(i));
    Result:=sl.CommaText;
  finally
    sl.Free;
  end;
end;

procedure TIntegerArray.SetCommaText(const AValue: string);
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    sl.CommaText:=AValue;
    SetLength(Items,sl.Count);
    for i:=0 to sl.Count-1 do
      Items[i]:=StrToIntDef(sl[i],0);
  finally
    sl.Free;
  end;
end;

procedure TIntegerArray.Sort(Descending: Boolean);

  function _Compare(AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
  begin
    Result:=CompareValue(Items[AIndex1],Items[AIndex2]);
    if ADescending then
      Result:=-Result;
  end;

  procedure _Swap(AIndex1, AIndex2: integer);
  var
    r: integer;
  begin
    r:=Items[AIndex1];
    Items[AIndex1]:=Items[AIndex2];
    Items[AIndex2]:=r;
  end;

  procedure _QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while _Compare(Lo,Mid,Descending)<0 do
          Inc(Lo);
        while _Compare(Hi,Mid,Descending)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            _Swap(Lo,Hi);
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        _QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  _QuickSort(0,High(Items));
end;

{ TStringArray }

function TStringArray.Add(const AValue: string): integer;
begin
  SetLength(Items,Length(Items)+1);
  Items[High(Items)]:=AValue;
  Result:=High(Items);
end;

procedure TStringArray.Clear;
begin
  Finalize(Items);
end;

function TStringArray.Count: integer;
begin
  Result:=Length(Items);
end;

procedure TStringArray.Delete(AIndex: integer);
var
  i: Integer;
begin
  for i:=AIndex to High(Items)-1 do
    Items[i]:=Items[i+1];
  SetLength(Items, High(Items));
end;

function TStringArray.GetDelimitedText(const ADelimiter: char): string;
var
  sl: TStringList;
  s: string;
begin
  sl:=TStringList.Create;
  try
    sl.Delimiter:=ADelimiter;
    sl.StrictDelimiter:=True;
    for s in Items do
      sl.Add(s);
    Result:=sl.DelimitedText;
  finally
    sl.Free;
  end;
end;

procedure TStringArray.SetDelimitedText(const AValue: string;
  const ADelimiter: char);
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    sl.Delimiter:=ADelimiter;
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=AValue;
    SetLength(Items,sl.Count);
    for i:=0 to sl.Count-1 do
      Items[i]:=sl[i];
  finally
    sl.Free;
  end;
end;


procedure TStringArray.Sort(Descending: Boolean);

function _Compare(AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
  begin
    Result:=AnsiCompareStr(Items[AIndex1],Items[AIndex2]);
    if ADescending then
      Result:=-Result;
  end;

  procedure _Swap(AIndex1, AIndex2: integer);
  var
    r: string;
  begin
    r:=Items[AIndex1];
    Items[AIndex1]:=Items[AIndex2];
    Items[AIndex2]:=r;
  end;

  procedure _QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while _Compare(Lo,Mid,Descending)<0 do
          Inc(Lo);
        while _Compare(Hi,Mid,Descending)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            _Swap(Lo,Hi);
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        _QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  _QuickSort(0,High(Items));
end;

{ TFloatArray }

function TFloatArray.Add(AValue: double): integer;
begin
  SetLength(Items,Length(Items)+1);
  Items[High(Items)]:=AValue;
  Result:=High(Items);
end;

procedure TFloatArray.Clear;
begin
  Finalize(Items);
end;

function TFloatArray.Count: integer;
begin
  Result:=Length(Items);
end;

procedure TFloatArray.Delete(AIndex: integer);
var
  i: Integer;
begin
  for i:=AIndex to High(Items)-1 do
    Items[i]:=Items[i+1];
  SetLength(Items, High(Items));
end;

procedure TFloatArray.Sort(Descending: Boolean);

  function _Compare(AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
  begin
    Result:=CompareValue(Items[AIndex1],Items[AIndex2]);
    if ADescending then
      Result:=-Result;
  end;

  procedure _Swap(AIndex1, AIndex2: integer);
  var
    r: double;
  begin
    r:=Items[AIndex1];
    Items[AIndex1]:=Items[AIndex2];
    Items[AIndex2]:=r;
  end;

  procedure _QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while _Compare(Lo,Mid,Descending)<0 do
          Inc(Lo);
        while _Compare(Hi,Mid,Descending)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            _Swap(Lo,Hi);
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        _QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  _QuickSort(0,High(Items));
end;

end.
