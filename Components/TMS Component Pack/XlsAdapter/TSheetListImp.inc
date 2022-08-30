//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TSheetList.GetItems(index: integer): TSheet;
begin
  Result := TSheet (inherited Items[Index]);
end;

procedure TSheetList.SetItems(index: integer; const Value: TSheet);
begin
  inherited Items[Index] := Value;
end;

function TSheetList.Add(aRecord: TSheet):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TSheetList.Insert(Index: Integer; ARecord:TSheet);
begin
  inherited Insert(Index, ARecord);
end;

