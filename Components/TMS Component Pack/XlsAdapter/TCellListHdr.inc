//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TCellRecordList;
  procedure SetItems(index: integer; const Value: TCellRecordList);
public
  property Items[index: integer]: TCellRecordList read GetItems write SetItems;  default;
  function Add(aRecord: TCellRecordList):integer;
  procedure Insert(Index: integer; aRecord: TCellRecordList);
