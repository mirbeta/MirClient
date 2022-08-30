//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TBaseRowColRecordList;
  procedure SetItems(index: integer; const Value: TBaseRowColRecordList);
public
  property Items[index: integer]: TBaseRowColRecordList read GetItems write SetItems;  default;
  function Add(aRecord: TBaseRowColRecordList):integer;
  procedure Insert(Index: integer; aRecord: TBaseRowColRecordList);
