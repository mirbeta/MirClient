//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TColInfo;
  procedure SetItems(index: integer; const Value: TColInfo);
public
  property Items[index: integer]: TColInfo read GetItems write SetItems;  default;
  function Add(aRecord: TColInfo):integer;
  procedure Insert(Index: integer; aRecord: TColInfo);
private
  Sorted: boolean;
public
  function Find(const aItem:integer ; var Index: integer): boolean;
  procedure Sort;
