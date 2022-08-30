//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Modified by hand to not include Items, because here it doesnt make sense
//  (this is the base) and this can be called millons of times, we can save some seconds...                                                             //
//************************************************************************************//

{function TBaseRecordList.GetItems(index: integer): TBaseRecord;
begin
  Result := inherited Items[Index] as TBaseRecord;
end;

procedure TBaseRecordList.SetItems(index: integer; const Value: TBaseRecord);
begin
  inherited Items[Index] := Value;
end;
}

function TBaseRecordList.Add(aRecord: TBaseRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TBaseRecordList.Insert(Index: Integer; ARecord:TBaseRecord);
begin
  inherited Insert(Index, ARecord);
end;

