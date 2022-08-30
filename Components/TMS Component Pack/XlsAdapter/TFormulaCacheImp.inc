//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TFormulaCache.GetItems(index: integer): TFormulaRecord;
begin
  Result := inherited Items[Index] as TFormulaRecord;
end;

procedure TFormulaCache.SetItems(index: integer; const Value: TFormulaRecord);
begin
  inherited Items[Index] := Value;
end;

function TFormulaCache.Add(aRecord: TFormulaRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TFormulaCache.Insert(Index: Integer; ARecord:TFormulaRecord);
begin
  inherited Insert(Index, ARecord);
end;

