//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TCFRecordList.GetItems(index: integer): TCFRecord;
begin
  Result := TCFRecord (inherited Items[Index]);
end;

procedure TCFRecordList.SetItems(index: integer; const Value: TCFRecord);
begin
  inherited Items[Index] := Value;
end;

function TCFRecordList.Add(aRecord: TCFRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TCFRecordList.Insert(Index: Integer; ARecord:TCFRecord);
begin
  inherited Insert(Index, ARecord);
end;

