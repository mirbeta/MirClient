//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TNoteList.GetItems(index: integer): TNoteRecordList;
begin
  Result := TNoteRecordList (inherited Items[Index]);
end;

procedure TNoteList.SetItems(index: integer; const Value: TNoteRecordList);
begin
  inherited Items[Index] := Value;
end;

function TNoteList.Add(aRecord: TNoteRecordList):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TNoteList.Insert(Index: Integer; ARecord:TNoteRecordList);
begin
  inherited Insert(Index, ARecord);
end;

