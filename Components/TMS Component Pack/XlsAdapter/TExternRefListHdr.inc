//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

private
  function GetItems(index: integer): TExternRef;
  procedure SetItems(index: integer; const Value: TExternRef);
public
  property Items[index: integer]: TExternRef read GetItems write SetItems;  default;
  function Add(aRecord: TExternRef):integer;
  procedure Insert(Index: integer; aRecord: TExternRef);
