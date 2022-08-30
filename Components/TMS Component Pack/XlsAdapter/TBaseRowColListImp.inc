//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TBaseRowColList.GetItems(index: integer): TBaseRowColRecordList;
begin
  Result := TBaseRowColRecordList (inherited Items[Index]);
end;

procedure TBaseRowColList.SetItems(index: integer; const Value: TBaseRowColRecordList);
begin
  inherited Items[Index] := Value;
end;

function TBaseRowColList.Add(aRecord: TBaseRowColRecordList):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TBaseRowColList.Insert(Index: Integer; ARecord:TBaseRowColRecordList);
begin
  inherited Insert(Index, ARecord);
end;

