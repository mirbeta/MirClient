//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TXFRecord;
  procedure SetItems(index: integer; const Value: TXFRecord);
public
  property Items[index: integer]: TXFRecord read GetItems write SetItems;  default;
  function Add(aRecord: TXFRecord):integer;
  procedure Insert(Index: integer; aRecord: TXFRecord);
