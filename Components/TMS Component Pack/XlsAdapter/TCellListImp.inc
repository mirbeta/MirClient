//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TCellList.GetItems(index: integer): TCellRecordList;
begin
  Result := TCellRecordList (inherited Items[Index]);
end;

procedure TCellList.SetItems(index: integer; const Value: TCellRecordList);
begin
  inherited Items[Index] := Value;
end;

function TCellList.Add(aRecord: TCellRecordList):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TCellList.Insert(Index: Integer; ARecord:TCellRecordList);
begin
  inherited Insert(Index, ARecord);
end;

