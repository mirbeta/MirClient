//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Modified by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TiSSTEntry;
  procedure SetItems(index: integer; const Value: TiSSTEntry);
public
  property Items[index: integer]: TiSSTEntry read GetItems write SetItems;  default;
  function Add(aRecord: TiSSTEntry):integer;
  procedure Insert(Index: integer; aRecord: TiSSTEntry);
