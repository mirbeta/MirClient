//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TEscherSPRecord;
  procedure SetItems(index: integer; const Value: TEscherSPRecord);
public
  property Items[index: integer]: TEscherSPRecord read GetItems write SetItems;  default;
  function Add(aRecord: TEscherSPRecord):integer;
  procedure Insert(Index: integer; aRecord: TEscherSPRecord);
private
  Sorted: boolean;
public
  function Find(const aItem:Cardinal ; var Index: integer): boolean;
  procedure Sort;
