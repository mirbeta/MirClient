//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

function THLinkList.GetItems(index: integer): THLinkRecord;
begin
  Result := THLinkRecord (inherited Items[Index]);
end;

procedure THLinkList.SetItems(index: integer; const Value: THLinkRecord);
begin
  inherited Items[Index] := Value;
end;

function THLinkList.Add(aRecord: THLinkRecord):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure THLinkList.Insert(Index: Integer; ARecord:THLinkRecord);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareHLink(Item1, Item2: Pointer): Integer;
var
  i1, i2: THLinkRecord;
begin
  i1:=THLinkRecord(Item1);
  i2:=THLinkRecord(Item2);
  if i1.FirstRow < i2.FirstRow then Result:=-1 else if i1.FirstRow > i2.FirstRow then Result:=1 else
  if i1.FirstCol < i2.FirstCol then Result:=-1 else if i1.FirstCol > i2.FirstCol then Result:=1
  else Result:=0;
end;

procedure THLinkList.Sort;
begin
  inherited Sort(CompareHLink);
  Sorted:=true;
end;


