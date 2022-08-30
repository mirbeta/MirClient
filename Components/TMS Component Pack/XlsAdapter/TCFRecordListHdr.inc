//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TCFRecord;
  procedure SetItems(index: integer; const Value: TCFRecord);
public
  property Items[index: integer]: TCFRecord read GetItems write SetItems;  default;
  function Add(aRecord: TCFRecord):integer;
  procedure Insert(Index: integer; aRecord: TCFRecord);
