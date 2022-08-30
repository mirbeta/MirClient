//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TVPageBreakList.GetItems(index: integer): TVPageBreak;
begin
  Result := TVPageBreak (inherited Items[Index]);
end;

procedure TVPageBreakList.SetItems(index: integer; const Value: TVPageBreak);
begin
  inherited Items[Index] := Value;
end;

function TVPageBreakList.Add(aRecord: TVPageBreak):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TVPageBreakList.Insert(Index: Integer; ARecord:TVPageBreak);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareVPageBreaks(Item1, Item2: Pointer): Integer;
begin
  if TVPageBreak(Item1).BreakData.Col < TVPageBreak(Item2).BreakData.Col then Result:=-1 else if TVPageBreak(Item1).BreakData.Col > TVPageBreak(Item2).BreakData.Col then Result:=1 else Result:=0;
end;

procedure TVPageBreakList.Sort;
begin
  inherited Sort(CompareVPageBreaks);
  Sorted:=true;
end;

function TVPageBreakList.Find(const aItem:integer ; var Index: integer): boolean;
Var
 L, H, I, C: Integer;
 V: integer;
begin
  if not Sorted then Sort;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    V:=Items[i].BreakData.Col;
    if V < aItem then C:=-1 else if V > aItem then C:=1 else C:=0;
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

