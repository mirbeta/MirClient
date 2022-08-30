//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TXFRecordList.GetItems(index: integer): TXFRecord;
begin
  Result := TXFRecord (inherited Items[Index]);
end;

procedure TXFRecordList.SetItems(index: integer; const Value: TXFRecord);
begin
  inherited Items[Index] := Value;
end;

function TXFRecordList.Add(aRecord: TXFRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TXFRecordList.Insert(Index: Integer; ARecord:TXFRecord);
begin
  inherited Insert(Index, ARecord);
end;

