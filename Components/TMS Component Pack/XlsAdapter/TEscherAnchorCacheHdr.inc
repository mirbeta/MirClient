//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TEscherClientAnchorRecord;
  procedure SetItems(index: integer; const Value: TEscherClientAnchorRecord);
public
  property Items[index: integer]: TEscherClientAnchorRecord read GetItems write SetItems;  default;
  function Add(aRecord: TEscherClientAnchorRecord):integer;
  procedure Insert(Index: integer; aRecord: TEscherClientAnchorRecord);
