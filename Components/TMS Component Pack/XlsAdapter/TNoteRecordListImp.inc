//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TNoteRecordList.GetItems(index: integer): TNoteRecord;
begin
  Result := TNoteRecord (inherited Items[Index]);
end;

procedure TNoteRecordList.SetItems(index: integer; const Value: TNoteRecord);
begin
  inherited Items[Index] := Value;
end;

function TNoteRecordList.Add(aRecord: TNoteRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TNoteRecordList.Insert(Index: Integer; ARecord:TNoteRecord);
begin
  inherited Insert(Index, ARecord);
end;

