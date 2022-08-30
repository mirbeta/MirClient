//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TChartAIRecord;
  procedure SetItems(index: integer; const Value: TChartAIRecord);
public
  property Items[index: integer]: TChartAIRecord read GetItems write SetItems;  default;
  function Add(aRecord: TChartAIRecord):integer;
  procedure Insert(Index: integer; aRecord: TChartAIRecord);
