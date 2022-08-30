//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TNamedStorageList;
  procedure SetItems(index: integer; const Value: TNamedStorageList);
public
  property Items[index: integer]: TNamedStorageList read GetItems write SetItems;  default;
  function Add(aRecord: TNamedStorageList):integer;
  procedure Insert(Index: integer; aRecord: TNamedStorageList);
private
  Sorted: boolean;
public
  function Find(const aItem:string ; var Index: integer): boolean;
  procedure Sort;
