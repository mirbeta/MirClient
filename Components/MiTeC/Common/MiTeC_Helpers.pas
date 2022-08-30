{*******************************************************}
{               MiTeC TStrings helper                   }
{                                                       }
{          Copyright (c) 1997-2016 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Helpers;

interface

uses {$IFDEF RAD9PLUS}
     System.Variants, System.SysUtils, System.Classes, WinAPI.Windows, System.Win.Registry,
     System.Generics.Collections, Vcl.ComCtrls;
     {$ELSE}
     Variants, SysUtils, Classes, Windows, Registry, ComCtrls;
     {$ENDIF}

{$IFDEF RAD5PLUS}
type
  TStringsHelper = class helper for TStrings
  public
    function IndexOfNameEx(const Name: string; Idx: Integer): Integer; inline;
    function IndexOfEx(const S: string; Idx: Integer): Integer; inline;

    function TrimIndexOf(const AString: string): Integer; overload; inline;
    function TrimIndexOf(const AString: string; Idx: Integer): Integer; overload; inline;
    {case sensitive version of indexof}
    function CaseIndexOf(const AString: string): Integer; overload; inline;
    function CaseIndexOf(const AString: string; Idx: Integer): Integer; overload; inline;
    {like as caseindexof but can search substring}
    function IndexOfSubString(const SubString: string): Integer; overload; inline;
    {like as caseindexof but can search substring on next occurence}
    function IndexOfSubString(const SubString: string; Idx: Integer): Integer; overload; inline;
    function IndexOfValue(const ValueString: string): Integer; overload; inline;

    procedure CopyLines(ASource: TStrings; ACount: integer); overload; inline;
    procedure DeleteLines(ACount: integer); overload; inline;

    function LastItem: string; overload; inline;
  end;

  TRegistryHelper = class helper for TRegistry
  public
    function ReadDWORD(const Name: string): Cardinal;
  end;

  TListViewHelper = class helper for TListView
  public
    function GetCheckedCount: Integer;
    procedure CheckAll(AState: boolean);
    procedure ToggleCheckAll;
  end;
{$ENDIF}

function RectHeight(ARect: TRect): Integer;
function RectWidth(ARect: TRect): Integer;

implementation

uses {$IFDEF RAD9PLUS}
     System.RTLConsts,
     {$ELSE}
     RTLConsts,
     {$ENDIF}
     MiTeC_StrUtils;

function RectHeight;
begin
  Result:=ARect.Bottom-ARect.Top;
end;

function RectWidth;
begin
  Result:=ARect.Right-ARect.Left;
end;


{$IFDEF RAD5PLUS}

{ TStringsHelper }

function TStringsHelper.IndexOfEx(const S: string; Idx: Integer): Integer;
begin
  for Result := Idx to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TStringsHelper.IndexOfNameEx(const Name: string;
  Idx: Integer): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := Idx to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TStringsHelper.TrimIndexOf(const AString: string;
 Idx: Integer): Integer;
var
  i: Integer;
begin
  if (Count > 0) and (Idx <= (Count - 1)) then
    for i:=Idx to Count - 1 do
      if SameText(AString, TrimAll(Strings[i])) then
        begin
          Result:=i;
          Exit;
        end;
  Result:=-1;
end;

function TStringsHelper.TrimIndexOf(const AString: string): Integer;
begin
  Result:=TrimIndexOf(AString, 0);
end;

function TStringsHelper.CaseIndexOf(const AString: string): Integer;
begin
  Result:=CaseIndexOf(AString, 0);
end;

function TStringsHelper.CaseIndexOf(const AString: string;
 Idx: Integer): Integer;
var
  i: Integer;
begin
  if (Count > 0) and (Idx <= (Count - 1)) then
    for i:=Idx to Count - 1 do
      if AnsiCompareStr(AString, Strings[i]) = 0 then
        begin
          Result:=i;
          Exit;
        end;
  Result:=-1;
end;

function TStringsHelper.IndexOfSubString(const SubString: string): Integer;
begin
  Result:=IndexOfSubString(SubString, 0);
end;

function TStringsHelper.IndexOfSubString(const SubString: string;
   Idx: Integer): Integer;
var
  i: integer;
begin
  if (Count > 0) and (Idx <= (Count - 1)) then
    for i:=Idx to Count - 1 do
      if Pos(Uppercase(SubString), Uppercase(Strings[i])) > 0 then
        begin
          Result:=i;
          Exit;
        end;
  Result:=-1;
end;

function TStringsHelper.IndexOfValue(const ValueString: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if SameText(ValueString,ValueFromIndex[i]) then begin
      Result:=i;
      Break;
    end;
end;

procedure TStringsHelper.CopyLines(ASource: TStrings; ACount: integer);
var
  i: integer;
begin
  if ACount>ASource.Count then
    ACount:=ASource.Count;
  for i:=0 to ACount-1 do
    Add(ASource[i]);
end;

procedure TStringsHelper.DeleteLines(ACount: integer);
var
  i: Integer;
begin
  if ACount>=Count then
    Clear
  else begin
    i:=0;
    while i<ACount do begin
      Delete(0);
      Inc(i);
    end;
  end;
end;

function TStringsHelper.LastItem: string;
begin
  if Count>0 then
    Result:=Strings[Count-1]
  else
    Result:='';
end;

{ TRegistryHelper }

function TRegistryHelper.ReadDWORD(const Name: string): Cardinal;
var
  RegData: TRegDataType;
  DataType: Integer;
  BufSize: Integer;
begin
  DataType:=REG_NONE;
  BufSize:=SizeOf(Result);
  if RegQueryValueEx(CurrentKey,PChar(Name),nil,@DataType,PByte(@Result),@BufSize)<>ERROR_SUCCESS then
    raise ERegistryException.CreateResFmt(@SRegGetDataFailed,[Name]);
  if DataType=REG_SZ then
    RegData:=rdString
  else if DataType=REG_EXPAND_SZ then
    RegData:=rdExpandString
  else if DataType=REG_DWORD then
    RegData:=rdInteger
  else if DataType=REG_BINARY then
    RegData:=rdBinary
  else
    RegData:=rdUnknown;
  if RegData<>rdInteger then
    raise ERegistryException.CreateResFmt(@SInvalidRegType,[Name]);
end;

{ TListViewHelper }

procedure TListViewHelper.CheckAll(AState: boolean);
var
  i: integer;
begin
  with Self, Items do
    for i:=0 to Count-1 do
      Items[i].Checked:=AState;
end;

function TListViewHelper.GetCheckedCount: Integer;
var
  i: integer;
begin
  Result:=0;
  with Self, Items do
    for i:=0 to Count-1 do
      if Items[i].Checked then
        Inc(Result);
end;

procedure TListViewHelper.ToggleCheckAll;
var
  i: integer;
begin
  with Self, Items do
    for i:=0 to Count-1 do
      Items[i].Checked:=not Items[i].Checked;
end;

{$ENDIF}

end.
