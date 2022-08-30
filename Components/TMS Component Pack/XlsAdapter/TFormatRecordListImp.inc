//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TFormatRecordList.GetItems(index: integer): TFormatRecord;
begin
  Result := TFormatRecord (inherited Items[Index]);
end;

procedure TFormatRecordList.SetItems(index: integer; const Value: TFormatRecord);
begin
  inherited Items[Index] := Value;
end;

function TFormatRecordList.Add(aRecord: TFormatRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TFormatRecordList.Insert(Index: Integer; ARecord:TFormatRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareFormat(Item1, Item2: Pointer): Integer;
begin
  if TFormatRecord(Item1).FormatId < TFormatRecord(Item2).FormatId then Result:=-1 else if TFormatRecord(Item1).FormatId > TFormatRecord(Item2).FormatId then Result:=1 else Result:=0;
end;

procedure TFormatRecordList.Sort;
begin
  inherited Sort(CompareFormat);
  Sorted:=true;
end;

function TFormatRecordList.Find(const aItem:integer ; var Index: integer): boolean;
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
    V:=Items[i].FormatId;
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

