//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Modified by hand to not include Items, because here it doesnt make sense
//  (this is the base) and this can be called millons of times, we can save some seconds...                                                             //
//************************************************************************************//

//private
//  function GetItems(index: integer): TBaseRecord;
//  procedure SetItems(index: integer; const Value: TBaseRecord);
public
//  property Items[index: integer]: TBaseRecord read GetItems write SetItems; default;
  function Add(aRecord: TBaseRecord):integer;
  procedure Insert(Index: integer; aRecord: TBaseRecord);
