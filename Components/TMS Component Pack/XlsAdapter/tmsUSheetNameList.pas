unit tmsUSheetNameList;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsXlsMessages, Contnrs, tmsUFlxMessages;

type
  TWideContainer= record
    S: UTF16String;
    n: integer;
  end;
  PWideContainer= ^TWideContainer;

  TSheetNameList=class(TList) //Items are TWideContainer
  private
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetFullName(const S: UTF16String; const N: integer): UTF16String;
  public
    procedure Add(const aName: UTF16String); //Error if duplicated entry
    function AddUniqueName(const aName: UTF16String): UTF16String;
    class function MakeValidSheetName(const aName: UTF16String): UTF16String; 

    procedure DeleteSheet(const SheetName: UTF16String);
    procedure Rename(const OldName, NewName: UTF16String);

    function FindRootString(const S: UTF16String; out Index: Integer): Boolean; virtual;
    function FindFullString(const S: UTF16String; out Index: Integer): Boolean; virtual;
  end;

implementation

{ TSheetNameList }

procedure TSheetNameList.Add(const aName: UTF16String);
var
  InsPos: integer;
  Itm: PWideContainer;
begin
  if FindFullString(AName, InsPos) then raise Exception.CreateFmt(ErrDuplicatedSheetName, [string(aName)]);
  New(Itm);
  Itm.S:=aName;
  Itm.n:=0;
  Insert( InsPos, Itm );
end;

class function TSheetNameList.MakeValidSheetName(const aName: UTF16String): UTF16String;
var
  Min: integer;
begin
  Min:=Length(aName);
  if Min>31 then Min:=31;
  Result:=Copy(aName, 1, Min);
  Result:= StringReplace(Result, '/','_', [rfReplaceAll]);
  Result:= StringReplace(Result, '\','_', [rfReplaceAll]);
  Result:= StringReplace(Result, '?','_', [rfReplaceAll]);
  Result:= StringReplace(Result, '[','_', [rfReplaceAll]);
  Result:= StringReplace(Result, ']','_', [rfReplaceAll]);
  Result:= StringReplace(Result, '*','_', [rfReplaceAll]);
  Result:= StringReplace(Result, ':','.', [rfReplaceAll]);
end;

function TSheetNameList.AddUniqueName(const aName: UTF16String): UTF16String;
var
  InsPos: integer;
  Itm: PWideContainer;
  n:integer;
begin
  n:=0;
  if FindRootString(aName, InsPos) then
  begin
    n:=PWideContainer(Items[InsPos]).n+1;
    while FindFullString(GetFullName(aName, n), InsPos) do inc(n);
  end;
  New(Itm);
  Itm.S:=aName;
  Itm.n:=n;
  Insert( InsPos, Itm );
  Result:=GetFullName(aName,n);
end;

function MyCompareWideStrings(const s1,s2: UTF16String): integer;
var
  i:integer;
begin
  Result:=0;
  if Length(S1)<Length(S2) then Result:=-1 else if Length(S1)>Length(S2) then Result:=1
  else
  for i:=1 to Length(S1) do
  begin
    if S1[i]=S2[i] then continue
    else if S1[i]<S2[i] then Result:=-1 else Result:=1;
    exit;
  end;
end;

procedure TSheetNameList.DeleteSheet(const SheetName: UTF16String);
var
  Idx1: integer;
begin
  if not FindFullString(SheetName, Idx1) then raise Exception.CreateFmt(ErrDuplicatedSheetName, [SheetName]);
  Delete(Idx1);
end;

function TSheetNameList.FindFullString(const S: UTF16String;
  out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := MyCompareWideStrings(GetFullName(PWideContainer(Items[I]).S, PWideContainer(Items[I]).N), S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
      end;
    end;
  end;
  Index := L;
end;

function TSheetNameList.FindRootString(const S: UTF16String;
  out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := MyCompareWideStrings(PWideContainer(Items[I]).S, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
      end;
    end;
  end;
  Index := L;
end;


function TSheetNameList.GetFullName(const S: UTF16String; const N: integer): UTF16String;
begin
  if n=0 then Result:= S else Result:= S+IntToStr(n);
end;

procedure TSheetNameList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then Dispose(PWideContainer(Ptr));
  inherited Notify(Ptr, Action);
end;

procedure TSheetNameList.Rename(const OldName, NewName: UTF16String);
var
  Idx1, Idx2: integer;
begin
  if OldName=NewName then exit;
  if not FindFullString(OldName, Idx1) then raise Exception.CreateFmt(ErrDuplicatedSheetName, [OldName]);
  if FindFullString(NewName, Idx2) then raise Exception.CreateFmt(ErrDuplicatedSheetName, [string(NewName)]);
  Delete(Idx1);
  Add(NewName);
end;

end.
