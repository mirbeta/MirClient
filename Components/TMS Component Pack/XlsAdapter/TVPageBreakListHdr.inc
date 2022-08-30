//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TVPageBreak;
  procedure SetItems(index: integer; const Value: TVPageBreak);
public
  property Items[index: integer]: TVPageBreak read GetItems write SetItems;  default;
  function Add(aRecord: TVPageBreak):integer;
  procedure Insert(Index: integer; aRecord: TVPageBreak);
private
  Sorted: boolean;
public
  function Find(const aItem:integer ; var Index: integer): boolean;
  procedure Sort;
