//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TNameRecord;
  procedure SetItems(index: integer; const Value: TNameRecord);
public
  property Items[index: integer]: TNameRecord read GetItems write SetItems;  default;
  function Add(aRecord: TNameRecord):integer;
  procedure Insert(Index: integer; aRecord: TNameRecord);
