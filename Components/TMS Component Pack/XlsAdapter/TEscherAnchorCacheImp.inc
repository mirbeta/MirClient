//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TEscherAnchorCache.GetItems(index: integer): TEscherClientAnchorRecord;
begin
  Result := TEscherClientAnchorRecord (inherited Items[Index]);
end;

procedure TEscherAnchorCache.SetItems(index: integer; const Value: TEscherClientAnchorRecord);
begin
  inherited Items[Index] := Value;
end;

function TEscherAnchorCache.Add(aRecord: TEscherClientAnchorRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TEscherAnchorCache.Insert(Index: Integer; ARecord:TEscherClientAnchorRecord);
begin
  inherited Insert(Index, ARecord);
end;

