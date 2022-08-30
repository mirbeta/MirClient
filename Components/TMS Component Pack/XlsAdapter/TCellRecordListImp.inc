//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TCellRecordList.GetItems(index: integer): TCellRecord;
begin
  Result := TCellRecord (inherited Items[Index]);
end;

procedure TCellRecordList.SetItems(index: integer; const Value: TCellRecord);
begin
  inherited Items[Index] := Value;
end;

function TCellRecordList.Add(aRecord: TCellRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TCellRecordList.Insert(Index: Integer; ARecord:TCellRecord);
begin
  inherited Insert(Index, ARecord);
end;

