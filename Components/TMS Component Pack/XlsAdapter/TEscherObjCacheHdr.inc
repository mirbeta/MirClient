//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TEscherClientDataRecord;
  procedure SetItems(index: integer; const Value: TEscherClientDataRecord);
public
  property Items[index: integer]: TEscherClientDataRecord read GetItems write SetItems;  default;
  function Add(aRecord: TEscherClientDataRecord):integer;
  procedure Insert(Index: integer; aRecord: TEscherClientDataRecord);
