//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TFormulaRecord;
  procedure SetItems(index: integer; const Value: TFormulaRecord);
public
  property Items[index: integer]: TFormulaRecord read GetItems write SetItems; default;
  function Add(aRecord: TFormulaRecord):integer;
  procedure Insert(Index: integer; aRecord: TFormulaRecord);
