//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TBoundSheetRecord;
  procedure SetItems(index: integer; const Value: TBoundSheetRecord);
public
  property Items[index: integer]: TBoundSheetRecord read GetItems write SetItems;  default;
  function Add(aRecord: TBoundSheetRecord):integer;
  procedure Insert(Index: integer; aRecord: TBoundSheetRecord);
