//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TBoundSheetRecordList.GetItems(index: integer): TBoundSheetRecord;
begin
  Result := TBoundSheetRecord (inherited Items[Index]);
end;

procedure TBoundSheetRecordList.SetItems(index: integer; const Value: TBoundSheetRecord);
begin
  inherited Items[Index] := Value;
end;

function TBoundSheetRecordList.Add(aRecord: TBoundSheetRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TBoundSheetRecordList.Insert(Index: Integer; ARecord:TBoundSheetRecord);
begin
  inherited Insert(Index, ARecord);
end;

