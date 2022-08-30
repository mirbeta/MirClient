unit tmsUFlxRowComments;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses SysUtils, Classes,
     {$IFDEF FLX_GENERICS} Generics.Collections, {$ENDIF}
     Contnrs;
type
{$IFDEF FLX_GENERICS}
  TCommentRowPos = class (TList<integer>)
  public
    Row: integer;
  end;
{$ELSE}
  TCommentRowPos= class(TList)
  private
    function GetItems(index: integer): integer;
    procedure SetItems(index: integer; const Value: integer);
  public
    Row: integer;

    procedure Add(const i: integer);
    property Items[index: integer]: integer read GetItems write SetItems; default;
  end;
{$ENDIF}

  TRowComments= class(TObjectList) //Items are TCommentRowPos
  private
    EmptySlot: TCommentRowPos;

    function GetItems(aRow: integer): TCommentRowPos;
    function Find(const aRow: integer; out Index: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const aRow, aPos: integer);
    property Items[aRow: integer]: TCommentRowPos read GetItems; default;
    procedure Delete(const aRow, aCol: integer);
  end;
implementation

{ TCommentRowPos }


{ TRowComments }

procedure TRowComments.Add(const aRow, aPos: integer);
var
  i: integer;
begin
  if not Find(aRow, i) then
  begin;
    Insert( i, TCommentRowPos.Create);
    (inherited Items[i] as TCommentRowPos).Row:=aRow;
  end;
  (inherited Items[i] as TCommentRowPos).Add(aPos);
end;

constructor TRowComments.Create;
begin
  inherited Create(True);
  EmptySlot:= TCommentRowPos.Create;
end;

destructor TRowComments.Destroy;
begin
  FreeAndNil(EmptySlot);
  inherited;
end;

function TRowComments.GetItems(aRow: integer): TCommentRowPos;
var
  i:integer;
begin
  if Find(aRow, i) then Result:= inherited Items[i] as TCommentRowPos else Result:=EmptySlot;
end;

function TRowComments.Find(const aRow: integer ; out Index: integer): boolean;
Var
 L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if (inherited Items[i] as TCommentRowPos).Row < aRow then C:=-1 else if (inherited Items[i] as TCommentRowPos).Row>aRow then C:=1 else C:=0;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      If C = 0 Then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TRowComments.Delete(const aRow, aCol: integer);
var
  i,k:integer;
  Limit: integer;
  CRp: TCommentRowPos;
begin
  if Find(aRow, i) then
  begin
    CRp:=(inherited Items[i] as TCommentRowPos);
    Limit:=CRp[aCol];
    CRp.Delete(aCol);
    for i:=0 to Count-1 do
    begin
     CRp:=(inherited Items[i] as TCommentRowPos);
     for k:=0 to CRp.Count-1 do if CRp[k]>Limit then CRp[k]:=CRp[k]-1;
    end;
  end;
end;

{$IFDEF FLX_GENERICS}
{$ELSE}
{ TCommentRowPos }
procedure TCommentRowPos.Add(const i: integer);
begin
  inherited Add(Pointer(i));
end;

function TCommentRowPos.GetItems(index: integer): integer;
begin
  Result:=Integer(inherited Items[Index]);
end;

procedure TCommentRowPos.SetItems(index: integer; const Value: integer);
begin
  inherited Items[Index]:=Pointer(Value);
end;
{$ENDIF}
end.


