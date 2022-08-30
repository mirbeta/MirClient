//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TSupBookRecord;
  procedure SetItems(index: integer; const Value: TSupBookRecord);
public
  property Items[index: integer]: TSupBookRecord read GetItems write SetItems;  default;
  function Add(aRecord: TSupBookRecord):integer;
  procedure Insert(Index: integer; aRecord: TSupBookRecord);
