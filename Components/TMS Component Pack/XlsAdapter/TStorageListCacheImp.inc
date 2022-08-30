//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TStorageListCache.GetItems(index: integer): TNamedStorageList;
begin
  Result := TNamedStorageList (inherited Items[Index]);
end;

procedure TStorageListCache.SetItems(index: integer; const Value: TNamedStorageList);
begin
  inherited Items[Index] := Value;
end;

function TStorageListCache.Add(aRecord: TNamedStorageList):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TStorageListCache.Insert(Index: Integer; ARecord:TNamedStorageList);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareStorages(Item1, Item2: Pointer): Integer;
begin
  if TNamedStorageList(Item1).Name < TNamedStorageList(Item2).Name then Result:=-1 else if TNamedStorageList(Item1).Name > TNamedStorageList(Item2).Name then Result:=1 else Result:=0;
end;

procedure TStorageListCache.Sort;
begin
  inherited Sort(CompareStorages);
  Sorted:=true;
end;

function TStorageListCache.Find(const aItem:string ; var Index: integer): boolean;
Var
 L, H, I, C: Integer;
 V: string;
begin
  if not Sorted then Sort;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    V:=Items[i].Name;
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

