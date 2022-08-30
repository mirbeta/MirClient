//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TEscherOPTRecord;
  procedure SetItems(index: integer; const Value: TEscherOPTRecord);
public
  property Items[index: integer]: TEscherOPTRecord read GetItems write SetItems;  default;
  function Add(aRecord: TEscherOPTRecord):integer;
  procedure Insert(Index: integer; aRecord: TEscherOPTRecord);
private
  Sorted: boolean;
public
  function Find(const aItem:Integer ; var Index: integer): boolean;
  procedure Sort;
