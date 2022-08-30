//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function THPageBreakList.GetItems(index: integer): THPageBreak;
begin
  Result := THPageBreak (inherited Items[Index]);
end;

procedure THPageBreakList.SetItems(index: integer; const Value: THPageBreak);
begin
  inherited Items[Index] := Value;
end;

function THPageBreakList.Add(aRecord: THPageBreak):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure THPageBreakList.Insert(Index: Integer; ARecord:THPageBreak);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareHPageBreaks(Item1, Item2: Pointer): Integer;
begin
  if THPageBreak(Item1).BreakData.Row < THPageBreak(Item2).BreakData.Row then Result:=-1 else if THPageBreak(Item1).BreakData.Row > THPageBreak(Item2).BreakData.Row then Result:=1 else Result:=0;
end;

procedure THPageBreakList.Sort;
begin
  inherited Sort(CompareHPageBreaks);
  Sorted:=true;
end;

function THPageBreakList.Find(const aItem:integer ; var Index: integer): boolean;
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
    V:=Items[i].BreakData.Row;
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

