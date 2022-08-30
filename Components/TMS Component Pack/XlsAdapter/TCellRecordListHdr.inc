//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TCellRecord;
  procedure SetItems(index: integer; const Value: TCellRecord);
public
  property Items[index: integer]: TCellRecord read GetItems write SetItems;  default;
  function Add(aRecord: TCellRecord):integer;
  procedure Insert(Index: integer; aRecord: TCellRecord);
