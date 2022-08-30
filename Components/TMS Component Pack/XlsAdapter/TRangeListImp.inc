//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TRangeList.GetItems(index: integer): TRangeEntry;
begin
  Result := TRangeEntry (inherited Items[Index]);
end;

procedure TRangeList.SetItems(index: integer; const Value: TRangeEntry);
begin
  inherited Items[Index] := Value;
end;

function TRangeList.Add(aRecord: TRangeEntry):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TRangeList.Insert(Index: Integer; ARecord:TRangeEntry);
begin
  inherited Insert(Index, ARecord);
end;

