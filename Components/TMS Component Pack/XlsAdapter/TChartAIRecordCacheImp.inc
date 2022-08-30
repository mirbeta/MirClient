//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function TChartAIRecordCache.GetItems(index: integer): TChartAIRecord;
begin
  Result := TChartAIRecord (inherited Items[Index]);
end;

procedure TChartAIRecordCache.SetItems(index: integer; const Value: TChartAIRecord);
begin
  inherited Items[Index] := Value;
end;

function TChartAIRecordCache.Add(aRecord: TChartAIRecord):integer;
begin
  Result:=inherited Add(aRecord);
end;

procedure TChartAIRecordCache.Insert(Index: Integer; ARecord:TChartAIRecord);
begin
  inherited Insert(Index, ARecord);
end;

