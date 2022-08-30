//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): THPageBreak;
  procedure SetItems(index: integer; const Value: THPageBreak);
public
  property Items[index: integer]: THPageBreak read GetItems write SetItems;  default;
  function Add(aRecord: THPageBreak):integer;
  procedure Insert(Index: integer; aRecord: THPageBreak);
private
  Sorted: boolean;
public
  function Find(const aItem:integer ; var Index: integer): boolean;
  procedure Sort;
