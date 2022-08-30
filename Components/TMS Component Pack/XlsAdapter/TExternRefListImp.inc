//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TExternRefList.GetItems(index: integer): TExternRef;
begin
  Result := TExternRef (inherited Items[Index]);
end;

procedure TExternRefList.SetItems(index: integer; const Value: TExternRef);
begin
  inherited Items[Index] := Value;
end;

function TExternRefList.Add(aRecord: TExternRef):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TExternRefList.Insert(Index: Integer; ARecord:TExternRef);
begin
  inherited Insert(Index, ARecord);
end;

