//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TShrFmlaRecord;
  procedure SetItems(index: integer; const Value: TShrFmlaRecord);
public
  property Items[index: integer]: TShrFmlaRecord read GetItems write SetItems;  default;
  function Add(aRecord: TShrFmlaRecord):integer;
  procedure Insert(Index: integer; aRecord: TShrFmlaRecord);
private
  Sorted: boolean;
public
  function Find(const aItem:Cardinal ; var Index: integer): boolean;
  procedure Sort;
