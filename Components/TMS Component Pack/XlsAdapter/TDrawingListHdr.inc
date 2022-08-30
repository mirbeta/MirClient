//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TDrawing;
  procedure SetItems(index: integer; const Value: TDrawing);
public
  property Items[index: integer]: TDrawing read GetItems write SetItems;  default;
  function Add(aRecord: TDrawing):integer;
  procedure Insert(Index: integer; aRecord: TDrawing);
