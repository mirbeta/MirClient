//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TSheet;
  procedure SetItems(index: integer; const Value: TSheet);
public
  property Items[index: integer]: TSheet read GetItems write SetItems;  default;
  function Add(aRecord: TSheet):integer;
  procedure Insert(Index: integer; aRecord: TSheet);
