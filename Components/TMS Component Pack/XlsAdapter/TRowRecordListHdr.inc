//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TRowRecord;
  procedure SetItems(index: integer; const Value: TRowRecord);
public
  property Items[index: integer]: TRowRecord read GetItems write SetItems;  default;
  function Add(aRecord: TRowRecord):integer;
  procedure Insert(Index: integer; aRecord: TRowRecord);
