//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Modified by hand                                                            //
//************************************************************************************//

function TSST.GetItems(index: integer): TiSSTEntry;
begin
  Result := TiSSTEntry (inherited Items[Index]);
end;

procedure TSST.SetItems(index: integer; const Value: TiSSTEntry);
begin
  inherited Items[Index] := Pointer(Value);
end;

function TSST.Add(aRecord: TiSSTEntry):integer;
begin
  Result:=inherited Add(Pointer(aRecord));
end;

procedure TSST.Insert(Index: Integer; ARecord:TiSSTEntry);
begin
  inherited Insert(Index, Pointer(ARecord));
end;

