//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TFlxChart;
  procedure SetItems(index: integer; const Value: TFlxChart);
public
  property Items[index: integer]: TFlxChart read GetItems write SetItems;  default;
  function Add(aRecord: TFlxChart):integer;
  procedure Insert(Index: integer; aRecord: TFlxChart);
