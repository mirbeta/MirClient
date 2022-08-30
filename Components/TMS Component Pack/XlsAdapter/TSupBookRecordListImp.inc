//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TSupBookRecordList.GetItems(index: integer): TSupBookRecord;
begin
  Result := TSupBookRecord (inherited Items[Index]);
end;

procedure TSupBookRecordList.SetItems(index: integer; const Value: TSupBookRecord);
begin
  inherited Items[Index] := Value;
end;

function TSupBookRecordList.Add(aRecord: TSupBookRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TSupBookRecordList.Insert(Index: Integer; ARecord:TSupBookRecord);
begin
  inherited Insert(Index, ARecord);
end;

