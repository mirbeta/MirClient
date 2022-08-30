//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TNoteRecord;
  procedure SetItems(index: integer; const Value: TNoteRecord);
public
  property Items[index: integer]: TNoteRecord read GetItems write SetItems;  default;
  function Add(aRecord: TNoteRecord):integer;
  procedure Insert(Index: integer; aRecord: TNoteRecord);
