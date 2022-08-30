//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TColInfoList.GetItems(index: integer): TColInfo;
begin
  Result := TColInfo (inherited Items[Index]);
end;

procedure TColInfoList.SetItems(index: integer; const Value: TColInfo);
begin
  inherited Items[Index] := Value;
end;

function TColInfoList.Add(aRecord: TColInfo):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TColInfoList.Insert(Index: Integer; ARecord:TColInfo);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareColumns(Item1, Item2: Pointer): Integer;
begin
  if TColInfo(Item1).Column < TColInfo(Item2).Column then Result:=-1 else if TColInfo(Item1).Column > TColInfo(Item2).Column then Result:=1 else Result:=0;
end;

procedure TColInfoList.Sort;
begin
  inherited Sort(CompareColumns);
  Sorted:=true;
end;

function TColInfoList.Find(const aItem:integer ; var Index: integer): boolean;
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
    V:=Items[i].Column;
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

