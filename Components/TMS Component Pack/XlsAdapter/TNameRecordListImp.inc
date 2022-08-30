//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TNameRecordList.GetItems(index: integer): TNameRecord;
begin
  Result := TNameRecord (inherited Items[Index]);
end;

procedure TNameRecordList.SetItems(index: integer; const Value: TNameRecord);
begin
  inherited Items[Index] := Value;
end;

function TNameRecordList.Add(aRecord: TNameRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TNameRecordList.Insert(Index: Integer; ARecord:TNameRecord);
begin
  inherited Insert(Index, ARecord);
end;

