//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TNoteRecordList;
  procedure SetItems(index: integer; const Value: TNoteRecordList);
public
  property Items[index: integer]: TNoteRecordList read GetItems write SetItems;  default;
  function Add(aRecord: TNoteRecordList):integer;
  procedure Insert(Index: integer; aRecord: TNoteRecordList);
