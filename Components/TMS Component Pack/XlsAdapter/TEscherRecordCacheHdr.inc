//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TEscherRecord;
  procedure SetItems(index: integer; const Value: TEscherRecord);
public
  property Items[index: integer]: TEscherRecord read GetItems write SetItems;  default;
  function Add(aRecord: TEscherRecord):integer;
  procedure Insert(Index: integer; aRecord: TEscherRecord);
