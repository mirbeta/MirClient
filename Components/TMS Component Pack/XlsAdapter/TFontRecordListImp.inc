//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TFontRecordList.GetItems(index: integer): TFontRecord;
begin
  Result := TFontRecord (inherited Items[Index]);
end;

procedure TFontRecordList.SetItems(index: integer; const Value: TFontRecord);
begin
  inherited Items[Index] := Value;
end;

function TFontRecordList.Add(aRecord: TFontRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TFontRecordList.Insert(Index: Integer; ARecord:TFontRecord);
begin
  inherited Insert(Index, ARecord);
end;

