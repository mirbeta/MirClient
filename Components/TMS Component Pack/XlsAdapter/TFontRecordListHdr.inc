//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TFontRecord;
  procedure SetItems(index: integer; const Value: TFontRecord);
public
  property Items[index: integer]: TFontRecord read GetItems write SetItems;  default;
  function Add(aRecord: TFontRecord):integer;
  procedure Insert(Index: integer; aRecord: TFontRecord);
