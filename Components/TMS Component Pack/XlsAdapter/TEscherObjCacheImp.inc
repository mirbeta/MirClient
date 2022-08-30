//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherObjCache.GetItems(index: integer): TEscherClientDataRecord;
begin
  Result := TEscherClientDataRecord (inherited Items[Index]);
end;

procedure TEscherObjCache.SetItems(index: integer; const Value: TEscherClientDataRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherObjCache.Add(aRecord: TEscherClientDataRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TEscherObjCache.Insert(Index: Integer; ARecord:TEscherClientDataRecord);
begin
  inherited Insert(Index, ARecord);
end;

