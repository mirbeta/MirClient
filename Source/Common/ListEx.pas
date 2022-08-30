unit ListEx;

interface

uses
  Windows, SysUtils, SortList, Classes,uSyncObj;

type
  TLockSortList = class (TSortList)
  private  
    m_Lock: TFixedCriticalSection;
  public
    constructor Create();override;
    destructor Destroy();override;
    procedure Lock();
    procedure Unlock();
  end;
  
  TDoubleList = class(TLockSortList)
  private
    m_AppendList: TSortList;
    function GetAppendCount: Integer;
  public
    constructor Create();override;
    destructor Destroy();override;
    procedure Append(P: Pointer);  
    procedure Flush();

    property AppendCount: Integer read GetAppendCount;
    property AppendList: TSortList read m_AppendList;
  end;

  TLockStringList = class(TStringList)
  private
    m_Lock: TFixedCriticalSection;
  public
    constructor Create();virtual;
    destructor Destroy();override;
    procedure Lock();
    procedure Unlock();
  end;

  TDoubleStringList = class(TLockStringList)    
    m_AppendStrings: TStringList;
    function GetAppendCount: Integer;
  private
  public
    constructor Create();override;
    destructor Destroy();override;
    procedure Append(S: string);
    procedure AppendObject(s: string; AObject: TObject);  
    procedure Flush();

    property AppendCount: Integer read GetAppendCount;
    property AppendList: TStringList read m_AppendStrings;
  end;

implementation

{ TDoubleList }

procedure TDoubleList.Append(P: Pointer);
begin    
 m_Lock.Enter;
  try
    m_AppendList.Add(P);
  finally
    m_Lock.Leave;
  end;
end;

constructor TDoubleList.Create;
begin
  inherited Create;
  m_AppendList := TSortList.Create;
end;

destructor TDoubleList.Destroy;
begin
  m_AppendList.Free;
  inherited;
end;

procedure TDoubleList.Flush;
begin
  m_Lock.Enter;
  try
    AddList( m_AppendList );
    m_AppendList.Trunc(0);
  finally
    m_Lock.Leave;
  end;
end;

function TDoubleList.GetAppendCount: Integer;
begin
  result := m_AppendList.Count;
end;

{ TLockSortList }

constructor TLockSortList.Create;
begin
  inherited Create();
  m_Lock := TFixedCriticalSection.Create;
end;

destructor TLockSortList.Destroy;
begin
  m_Lock.Free;
  inherited;
end;

procedure TLockSortList.Lock;
begin
  m_Lock.Enter;
end;

procedure TLockSortList.Unlock;
begin
  m_Lock.Leave;
end;

{ TLockStringList }

constructor TLockStringList.Create;
begin
  inherited Create;
  m_Lock := TFixedCriticalSection.Create;
end;

destructor TLockStringList.Destroy;
begin
  m_Lock.Free;
  inherited;
end;

procedure TLockStringList.Lock;
begin
  m_Lock.Enter;
end;

procedure TLockStringList.Unlock;
begin
  m_Lock.Leave;
end;

{ TDoubleStringList }

procedure TDoubleStringList.Append(S: string);
begin
  m_Lock.Enter;
  try
    m_AppendStrings.Add( S );
  finally
    m_Lock.Leave;
  end;
end;

procedure TDoubleStringList.AppendObject(s: string; AObject: TObject);
begin
  m_Lock.Enter;
  try
    m_AppendStrings.AddObject( S, AObject );
  finally
    m_Lock.Leave;
  end;
end;

constructor TDoubleStringList.Create;
begin
  inherited Create;
  m_AppendStrings := TStringList.Create;
end;

destructor TDoubleStringList.Destroy;
begin
  m_AppendStrings.Free;
  inherited;
end;

procedure TDoubleStringList.Flush;
begin
  m_Lock.Enter;
  try
    AddStrings( m_AppendStrings );
    m_AppendStrings.Clear();
  finally
    m_Lock.Leave;
  end;
end;

function TDoubleStringList.GetAppendCount: Integer;
begin
  Result := m_AppendStrings.Count;
end;

end.
