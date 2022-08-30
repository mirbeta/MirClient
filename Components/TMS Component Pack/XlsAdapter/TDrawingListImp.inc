//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TDrawingList.GetItems(index: integer): TDrawing;
begin
  Result := TDrawing (inherited Items[Index]);
end;

procedure TDrawingList.SetItems(index: integer; const Value: TDrawing);
begin
  inherited Items[Index] := Value;
end;

function TDrawingList.Add(aRecord: TDrawing):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TDrawingList.Insert(Index: Integer; ARecord:TDrawing);
begin
  inherited Insert(Index, ARecord);
end;

