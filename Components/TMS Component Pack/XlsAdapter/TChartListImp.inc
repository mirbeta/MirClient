//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TChartList.GetItems(index: integer): TFlxChart;
begin
  Result := TFlxChart (inherited Items[Index]);
end;

procedure TChartList.SetItems(index: integer; const Value: TFlxChart);
begin
  inherited Items[Index] := Value;
end;

function TChartList.Add(aRecord: TFlxChart):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TChartList.Insert(Index: Integer; ARecord:TFlxChart);
begin
  inherited Insert(Index, ARecord);
end;

