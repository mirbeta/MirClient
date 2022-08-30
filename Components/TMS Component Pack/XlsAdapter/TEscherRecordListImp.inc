//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherRecordList.GetItems(index: integer): TEscherRecord;
begin
  Result := TEscherRecord (inherited Items[Index]);
end;

procedure TEscherRecordList.SetItems(index: integer; const Value: TEscherRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherRecordList.Add(aRecord: TEscherRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TEscherRecordList.Insert(Index: Integer; ARecord:TEscherRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareRecs(Item1, Item2: Pointer): Integer;
begin
  Result:= TEscherRecord(Item1).CompareRec(TEscherRecord(Item2));
end;

procedure TEscherRecordList.Sort;
begin
  inherited Sort(CompareRecs);
  Sorted:=true;
end;

function TEscherRecordList.Find(const aItem:TEscherRecord ; var Index: integer): boolean;
Var
 L, H, I, C: Integer;
begin
  if not Sorted then Sort;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C:=Items[i].CompareRec(aItem);
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

