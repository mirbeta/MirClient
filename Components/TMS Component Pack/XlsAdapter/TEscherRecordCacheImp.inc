//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherRecordCache.GetItems(index: integer): TEscherRecord;
begin
  Result := TEscherRecord (inherited Items[Index]);
end;

procedure TEscherRecordCache.SetItems(index: integer; const Value: TEscherRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherRecordCache.Add(aRecord: TEscherRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TEscherRecordCache.Insert(Index: Integer; ARecord:TEscherRecord);
begin
  inherited Insert(Index, ARecord);
end;

