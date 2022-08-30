//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TRowRecordList.GetItems(index: integer): TRowRecord;
begin
  Result := TRowRecord (inherited Items[Index]);
end;

procedure TRowRecordList.SetItems(index: integer; const Value: TRowRecord);
begin
  inherited Items[Index] := Value;
end;

function TRowRecordList.Add(aRecord: TRowRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TRowRecordList.Insert(Index: Integer; ARecord:TRowRecord);
begin
  inherited Insert(Index, ARecord);
end;

