//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): THLinkRecord;
  procedure SetItems(index: integer; const Value: THLinkRecord);
public
  property Items[index: integer]: THLinkRecord read GetItems write SetItems;  default;
  function Add(aRecord: THLinkRecord):integer;
  procedure Insert(Index: integer; aRecord: THLinkRecord);
private
  Sorted: boolean;
public
  procedure Sort;
