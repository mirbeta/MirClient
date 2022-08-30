unit uSyncObj;
//修复TCriticalSection 区块太小可能造成单CPU缓存的问。随云 2016-12-13
interface
uses SyncObjs,Classes,RTLConsts;
type
   TFixedCriticalSection = class(TCriticalSection)
      private
         FDummy : array [0..95] of Byte;
   end;

{ TFixedThreadList class }

  TFixedThreadList = class
  private
    FList: TList;
    FLock: TFixedCriticalSection;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function LockList: TList;
    procedure Remove(Item: Pointer); inline;
    procedure RemoveItem(Item: Pointer; Direction: TList.TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;


implementation

{ TFixedThreadList }

constructor TFixedThreadList.Create;
begin
  inherited Create;
  FLock := TFixedCriticalSection.Create;
  FList := TList.Create;
  FDuplicates := dupIgnore;
end;

destructor TFixedThreadList.Destroy;
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

procedure TFixedThreadList.Add(Item: Pointer);
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

procedure TFixedThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TFixedThreadList.LockList: TList;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TFixedThreadList.Remove(Item: Pointer);
begin
  RemoveItem(Item, TList.TDirection.FromBeginning);
end;

procedure TFixedThreadList.RemoveItem(Item: Pointer; Direction: TList.TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TFixedThreadList.UnlockList;
begin
  FLock.Leave;
end;

end.
