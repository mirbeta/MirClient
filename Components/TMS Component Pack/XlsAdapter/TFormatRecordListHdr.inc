//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TFormatRecord;
  procedure SetItems(index: integer; const Value: TFormatRecord);
public
  property Items[index: integer]: TFormatRecord read GetItems write SetItems; 
  function Add(aRecord: TFormatRecord):integer;
  procedure Insert(Index: integer; aRecord: TFormatRecord);
private
  Sorted: boolean;
public
  function Find(const aItem:integer ; var Index: integer): boolean;
  procedure Sort;
