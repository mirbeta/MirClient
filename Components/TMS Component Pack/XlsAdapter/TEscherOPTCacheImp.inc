//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherOPTCache.GetItems(index: integer): TEscherOPTRecord;
begin
  Result := TEscherOPTRecord (inherited Items[Index]);
end;

procedure TEscherOPTCache.SetItems(index: integer; const Value: TEscherOPTRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherOPTCache.Add(aRecord: TEscherOPTRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TEscherOPTCache.Insert(Index: Integer; ARecord:TEscherOPTRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareOPT(Item1, Item2: Pointer): Integer;
begin
  if TEscherOPTRecord(Item1).Row < TEscherOPTRecord(Item2).Row then Result:=-1 else if TEscherOPTRecord(Item1).Row > TEscherOPTRecord(Item2).Row then Result:=1 else Result:=0;
end;

procedure TEscherOPTCache.Sort;
begin
  inherited Sort(CompareOPT);
  Sorted:=true;
end;

function TEscherOPTCache.Find(const aItem:Integer ; var Index: integer): boolean;
Var
 L, H, I, C: Integer;
 V: Integer;
begin
  if not Sorted then Sort;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    V:=Items[i].Row;
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

