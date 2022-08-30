//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TSSTEntry;
  procedure SetItems(index: integer; const Value: TSSTEntry);
public
  property Items[index: integer]: TSSTEntry read GetItems write SetItems;  default;
  function Add(aRecord: TSSTEntry):integer;
  procedure Insert(Index: integer; aRecord: TSSTEntry);
