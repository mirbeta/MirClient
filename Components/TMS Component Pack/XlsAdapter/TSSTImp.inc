//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TSST.GetItems(index: integer): TSSTEntry;
begin
  Result := TSSTEntry (inherited Items[Index]);
end;

procedure TSST.SetItems(index: integer; const Value: TSSTEntry);
begin
  inherited Items[Index] := Value;
end;

function TSST.Add(aRecord: TSSTEntry):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TSST.Insert(Index: Integer; ARecord:TSSTEntry);
begin
  inherited Insert(Index, ARecord);
end;

