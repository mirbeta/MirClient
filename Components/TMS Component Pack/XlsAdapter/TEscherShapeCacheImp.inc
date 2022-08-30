//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherShapeCache.GetItems(index: integer): TEscherSPRecord;
begin
  Result := TEscherSPRecord (inherited Items[Index]);
end;

procedure TEscherShapeCache.SetItems(index: integer; const Value: TEscherSPRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherShapeCache.Add(aRecord: TEscherSPRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TEscherShapeCache.Insert(Index: Integer; ARecord:TEscherSPRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareShapes(Item1, Item2: Pointer): Integer;
begin
  if TEscherSPRecord(Item1).ShapeId^ < TEscherSPRecord(Item2).ShapeId^ then Result:=-1 else if TEscherSPRecord(Item1).ShapeId^ > TEscherSPRecord(Item2).ShapeId^ then Result:=1 else Result:=0;
end;

procedure TEscherShapeCache.Sort;
begin
  inherited Sort(CompareShapes);
  Sorted:=true;
end;

function TEscherShapeCache.Find(const aItem:Cardinal ; var Index: integer): boolean;
Var
 L, H, I, C: Integer;
 V: Cardinal;
begin
  if not Sorted then Sort;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    V:=Items[i].ShapeId^;
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

