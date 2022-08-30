//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TRangeEntry;
  procedure SetItems(index: integer; const Value: TRangeEntry);
public
  property Items[index: integer]: TRangeEntry read GetItems write SetItems;  default;
  function Add(aRecord: TRangeEntry):integer;
  procedure Insert(Index: integer; aRecord: TRangeEntry);
