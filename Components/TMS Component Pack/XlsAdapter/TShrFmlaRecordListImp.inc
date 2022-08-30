//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TShrFmlaRecordList.GetItems(index: integer): TShrFmlaRecord;
begin
  Result := TShrFmlaRecord (inherited Items[Index]);
end;

procedure TShrFmlaRecordList.SetItems(index: integer; const Value: TShrFmlaRecord);
begin
  inherited Items[Index] := Value;
end;

function TShrFmlaRecordList.Add(aRecord: TShrFmlaRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TShrFmlaRecordList.Insert(Index: Integer; ARecord:TShrFmlaRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareFormulaKey(Item1, Item2: Pointer): Integer;
begin
  if TShrFmlaRecord(Item1).Key < TShrFmlaRecord(Item2).Key then Result:=-1 else if TShrFmlaRecord(Item1).Key > TShrFmlaRecord(Item2).Key then Result:=1 else Result:=0;
end;

procedure TShrFmlaRecordList.Sort;
begin
  inherited Sort(CompareFormulaKey);
  Sorted:=true;
end;

function TShrFmlaRecordList.Find(const aItem:Cardinal ; var Index: integer): boolean;
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
    V:=Items[i].Key;
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

