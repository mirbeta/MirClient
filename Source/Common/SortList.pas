unit SortList;

interface

uses
  SysUtils, Windows,uSyncObj;

const
  MaxListSize = Maxint div 16;

type
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (const Item1, Item2: Pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);
  TDuplicates = (dupIgnore, dupAccept, dupError);

  ESortListError = class(Exception);

  TSortList = class;
  
  TSortListEnumerator = class
  private
    FIndex: Integer;
    FList: TSortList;
  public
    constructor Create(AList: TSortList);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TSortList = class(TObject)
  private
    FList     : PPointerList;
    FCount    : Integer;
    FCapacity : Integer;
    FOnCompare: TListSortCompare;
    FSorted   : Boolean;    
    FDuplicates: TDuplicates;
    procedure SetSorted(const Value: Boolean);
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function Find(const Item: Pointer):Integer;
    procedure InsertItem(const Index: Integer; const Item: Pointer);
  public
    constructor Create();virtual;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure AddList(AList: TSortList);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);      
    procedure DeleteEx(Index, ACount: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TSortList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TSortListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfItem(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    procedure Trunc(const ACount: Integer);
    procedure Pack;
    procedure Sort();
    procedure Assign(ListA: TSortList; AOperator: TListAssignOp = laCopy; ListB: TSortList = nil);
    function Search(const Ident: Pointer; Compare: TListSortCompare):Integer;
    function SearchItem(const Ident: Pointer; Compare: TListSortCompare):Pointer;
    property Pointers: PPointerList read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
    property OnCompare: TListSortCompare read FOnCompare write FOnCompare;
    property Sorted : Boolean read FSorted write SetSorted;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  TThreadSortList = class
  private
    FList: TSortList;
    FLock: TFixedCriticalSection;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TSortList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

implementation

uses  RTLConsts;

resourcestring
    ESortedListNotAllowSetItem = 'SortedList does not allow change indexed item';
    ESortedListNotAllowInsert  = 'SortedList does not allow insert';
    ESortedListNotAllowMove    = 'SortedList does not allow move';
    ESortedListNotAllowExchange= 'SortedList does not allow exchange';   
    ESortedInvalidCompare      = 'Invalid SortList Comapre function';
    ESortedListItemDuplicates  = 'Try to add a Duplicates item on SortedList'; 


procedure QuickSort(Items: PPointerList; Compare: TListSortCompare; nLow, nHigh: Integer);
var
  I, J, P: Integer; 
  Temp: Pointer;
begin
  repeat
    I := nLow;
    J := nHigh;
    P := (nLow + nHigh) shr 1;
    repeat
      while Compare(Items^[I], Items^[P]) < 0 do Inc(I);
      while Compare(Items^[J], Items^[P]) > 0 do Dec(J);
      if I <= J then
      begin
        Temp := Items^[I];
        Items^[I] := Items^[J];
        Items^[J] := Temp;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if nLow < J then QuickSort(Items, Compare, nLow, J);
    nLow := I;
  until I >= nHigh;
end;

(*
procedure BinarySort(Items: PPointerList; Compare:TListSortCompare; const Count: Integer);
var
  I, Index, Value: Integer;
  Temp: Pointer;
begin
  I := 1;
  while I < Count  do
  begin
    Index := I - 1;
    Value := Compare( Items^[I], Items^[Index] );
    if Value <> 0 then
    begin
      if Value < 0 then
      begin
        if Index > 0 then
        begin
          repeat
            Dec( Index );
          until (Index < 0) or (Compare( Items^[I], Items^[Index] ) >= 0);
          Inc( Index );
        end;
      end
      else begin
        if Index < Count - 1 then
        begin     
          Inc( Index );
          repeat
            Inc( Index );
          until (Index >= Count) or (Compare( Items^[I], Items^[Index] ) <= 0);
          Dec( Index );
        end;
      end;  
      if I <> Index then
      begin
        Temp := Items^[Index];
        Items^[Index] := Items^[i];
        Items^[i] := Temp;
      end
      else Inc( I );
    end
    else Inc( I );
  end;
end;
*)

function BinarySearch(Items: PPointerList; Item: Pointer;
  Compare:TListSortCompare; AFrom, ATo: Integer;
  var nInsert: Integer):Integer;
var
  Index, Value: Integer;
begin
  Result := -1;
  Index  := 0;

  while AFrom <= ATo do
  begin
    Index := (AFrom + ATo) shr 1;
    Value := Compare( Item, Items^[Index] );
    if Value <> 0 then
    begin
      if Value < 0 then
        ATo := Index - 1
      else begin
        AFrom := Index + 1;
        Index := AFrom;
      end;
    end
    else begin
      Result := Index;
      break;
    end;
  end;

  if @nInsert <> nil then
    nInsert := Index;
end;

function SortListCompareByAddress(const Item1, Item2: Pointer):Integer;
begin
  if Item1 = Item2 then
    Result := 0
  else if Integer(Item1) < Integer(Item2) then
    Result := -1
  else Result := 1;
end;

{ TSortList }

destructor TSortList.Destroy;
begin
  Clear;
end;

function TSortList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  if not FSorted then
  begin
    Result := FCount;  
    FList^[Result] := Item;
  end
  else begin
    BinarySearch( FList, Item, FOnCompare, 0, FCount -1, Result );
    if (FDuplicates <> dupAccept) and (Result < FCount) then
    begin
      if FOnCompare( Item, FList^[Result] ) = 0 then
      begin
        if FDuplicates = dupError then
          Error( ESortedListItemDuplicates, Result )
        else begin
          Result := -1;
          Exit;
        end;
      end;
    end;
    InsertItem( Result, Item );
  end;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TSortList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

constructor TSortList.Create;
begin
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupAccept;
  FOnCompare := SortListCompareByAddress;
end;

procedure TSortList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := FList^[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TSortList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise ESortListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TSortList.Error(Msg: PResStringRec; Data: Integer);
begin
  TSortList.Error(LoadResString(Msg), Data);
end;

procedure TSortList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if not FSorted then
  begin
    if (Index1 < 0) or (Index1 >= FCount) then
      Error(@SListIndexError, Index1);
    if (Index2 < 0) or (Index2 >= FCount) then
      Error(@SListIndexError, Index2);
    Item := FList^[Index1];
    FList^[Index1] := FList^[Index2];
    FList^[Index2] := Item;
  end
  else Error( @ESortedListNotAllowExchange, Index2 );
end;

function TSortList.Expand: TSortList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TSortList.Find(const Item: Pointer): Integer;
var
  Index, Value, AFrom, ATo: Integer;
begin
  Result := -1;
  AFrom  := 0;
  ATo    := FCount - 1;

  while AFrom <= ATo do
  begin
    Index := (AFrom + ATo) shr 1;
    Value := FOnCompare( Item, FList^[Index] );
    if Value <> 0 then
    begin
      if Value < 0 then
        ATo := Index - 1
      else AFrom := Index + 1;
    end
    else begin
      Result := Index;
      break;
    end;
  end;
end;

function TSortList.First: Pointer;
begin
  Result := Get(0);
end;

function TSortList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

function TSortList.GetEnumerator: TSortListEnumerator;
begin
  Result := TSortListEnumerator.Create(Self);
end;

procedure TSortList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else begin
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(FCapacity + Delta);
end;

function TSortList.IndexOf(Item: Pointer): Integer;
begin
  if FSorted then
    Result := Find( Item )
  else begin
    Result := 0;
    while (Result < FCount) and (FList^[Result] <> Item) do
      Inc(Result);
    if Result = FCount then
      Result := -1;
  end;
end;

function TSortList.IndexOfItem(Item: Pointer): Integer;
var
  nIndex, i: Integer;
begin
  if FSorted then
  begin
    nIndex := Find( Item );
    if (nIndex < 0) or (FList^[nIndex] = Item) then
      Result := nIndex
    else
    begin
      for i := nIndex + 1 to Count - 1 do
      begin
        if FList^[i] = Item then
        begin
          Result := i;
          Exit;
        end;
      end;
      
      for i := nIndex - 1 downto 0 do
      begin
        if FList^[i] = Item then
        begin
          Result := i;
          Exit;
        end;
      end;
      Result := -1;
    end;
  end
  else begin
    Result := 0;
    while (Result < FCount) and (FList^[Result] <> Item) do
      Inc(Result);
    if Result = FCount then
      Result := -1;
  end;
end;

procedure TSortList.Insert(Index: Integer; Item: Pointer);
begin
  if not FSorted then
  begin
    if (Index < 0) or (Index > FCount) then
      Error(@SListIndexError, Index);
    InsertItem( Index, Item );  
    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);
  end
  else Error( @ESortedListNotAllowInsert, Index );
end;

procedure TSortList.InsertItem(const Index: Integer; const Item: Pointer);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
end;

function TSortList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TSortList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if not FSorted then
  begin
    if CurIndex <> NewIndex then
    begin
      if (NewIndex < 0) or (NewIndex >= FCount) then
        Error(@SListIndexError, NewIndex);
      Item := Get(CurIndex);
      FList^[CurIndex] := nil;
      Delete(CurIndex);
      Insert(NewIndex, nil);
      FList^[NewIndex] := Item;
    end;
  end
  else Error( @ESortedListNotAllowMove, NewIndex );
end;

procedure TSortList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if not FSorted then
  begin
    if (Index < 0) or (Index >= FCount) then
      Error(@SListIndexError, Index);
    if Item <> FList^[Index] then
    begin
      Temp := FList^[Index];
      FList^[Index] := Item;
      if Temp <> nil then
        Notify(Temp, lnDeleted);
      if Item <> nil then
        Notify(Item, lnAdded);
    end;
  end
  else Error( @ESortedListNotAllowSetItem, Index );
end;

function TSortList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TSortList.Pack;
var
  PackedCount : Integer;
  StartIndex : Integer;
  EndIndex : Integer;
begin

  if FCount = 0 then
    Exit;

  PackedCount := 0;
  StartIndex := 0;
  Repeat
    // Locate the first/next non-nil element in the list
    While (FList^[StartIndex] = Nil) and (StartIndex < FCount) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
      begin
        // Locate the next nil pointer
        EndIndex := StartIndex;
        While (FList^[EndIndex] <> Nil) and (EndIndex < FCount) do
          Inc(EndIndex);
        Dec(EndIndex);

        // Move this block of non-null items to the index recorded in PackedToCount:
        // If this is a contiguous non-nil block at the start of the list then
        // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
        if StartIndex > PackedCount then
          System.Move(FList^[StartIndex],
                      FList^[PackedCount],
                      (EndIndex - StartIndex + 1) * SizeOf(Pointer));

        // Set the PackedToCount to reflect the number of items in the list
        // that have now been packed.
        Inc(PackedCount, EndIndex - StartIndex + 1);

        // Reset StartIndex to the element following EndIndex
        StartIndex := EndIndex + 1;
      end;
  Until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

function TSortList.Search(const Ident: Pointer; Compare: TListSortCompare):Integer;
var
  Index, Value, AFrom, ATo: Integer;
begin
  Result := -1;

  if FSorted then
  begin
    if not Assigned(Compare) then
      Error( @ESortedInvalidCompare, 0 );
      
    AFrom  := 0;
    ATo    := FCount - 1;

    while AFrom <= ATo do
    begin
      Index := (AFrom + ATo) shr 1;
      Value := Compare( FList^[Index], Ident );
      if Value <> 0 then
      begin
        if Value > 0 then
          ATo := Index - 1
        else AFrom := Index + 1;
      end
      else begin
        Result := Index;
        break;
      end;
    end;
  end
  else begin
    for Index := 0 to FCount - 1 do
    begin
      if Compare( FList^[Index], Ident ) = 0 then
      begin
        Result := Index;
        break;
      end;
    end;
  end;
end;

function TSortList.SearchItem(const Ident: Pointer;
  Compare: TListSortCompare): Pointer;
var
  Index: Integer;
begin
  Index := Search( Ident, Compare );
  if Index > -1 then
    Result := FList^[Index]
  else
    Result := nil;
end;

procedure TSortList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TSortList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TSortList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin          
    if Value then Sort();
    FSorted := Value;
  end;
end;

procedure TSortList.Sort();
begin
  if not FSorted and (FCount > 1) and Assigned(FOnCompare) then
  begin
    //BinarySort(FList, FOnCompare, Count);
    QuickSort( FList, FOnCompare, 0, FCount - 1 );
  end;
end;

function TSortList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TSortList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

procedure TSortList.Assign(ListA: TSortList; AOperator: TListAssignOp; ListB: TSortList);
var
  I: Integer;
  LTemp, LSource: TSortList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(FList^[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TSortList.Create; // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(FList^[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(FList^[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TSortList.Create;
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

procedure TSortList.Trunc(const ACount: Integer);
begin
  if ACount < FCount then
    FCount := ACount;
end;

procedure TSortList.AddList(AList: TSortList);
begin
  if AList.Count > 0 then
  begin
    while FCapacity - FCount < AList.Count do
    begin
      Grow();
    end;
    System.Move( AList.FList^[0], FList^[FCount], AList.Count * sizeof(FList^[0]) );
    Inc( FCount, AList.Count );
    if Sorted then
    begin
      FSorted := False;
      Sort();
      FSorted := True;
    end;
  end;
end;

procedure TSortList.DeleteEx(Index, ACount: Integer);
begin
  if (Index < 0) or (Index + ACount > FCount) then
    Error(@SListIndexError, Index);
  System.Move( FList^[Index + ACount], FList^[Index], (FCount - Index - ACount) * SizeOf(Pointer) );  
  Dec(FCount, ACount);
end;

{ TSortListEnumerator }

constructor TSortListEnumerator.Create(AList: TSortList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TSortListEnumerator.GetCurrent: Pointer;
begin
  Result := FList[FIndex];
end;

function TSortListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;


{ TThreadSortList }

constructor TThreadSortList.Create;
begin
  inherited Create;
  FLock := TFixedCriticalSection.Create;
  FList := TSortList.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadSortList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

procedure TThreadSortList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, Integer(Item));
  finally
    UnlockList;
  end;
end;

procedure TThreadSortList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadSortList.LockList: TSortList;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TThreadSortList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadSortList.UnlockList;
begin
  FLock.Leave;
end;

end.

