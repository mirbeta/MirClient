{
  "eXtended Pooled List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memXObjList;

{$INCLUDE rtcDefs.inc}

interface

uses
  memPtrPool, rtcTypes;

type
  infoType=TObject;

type
  pnode=^tnode;
  tnode=record
    info:infoType;
    prior,
    next:pnode;
    end;

  pnodearr=^tnodearr;
  tnodearr=array of tnode;

  tXObjList=class(tObject)
  private
    myPoolSize:longint;
    myPools:array of pnodearr;
    pool:tPtrPool;
    cnt:cardinal;

    Ffirst,
    Flast:pnode;

    procedure del_node(node:pnode);
    function new_node(const i:infoType; const pri,nex:pnode):pnode;

  public
    constructor Create(size:integer);
    destructor Destroy; override;

    function empty:boolean;

    function Count:cardinal;

    procedure PoolSize(size:integer);

    function First:infoType;
    function Last:infoType;

    procedure addFirst(const info:infoType);
    procedure addLast(const info:infoType);

    procedure removeFirst;
    procedure removeLast;

    procedure removeThis(const info:infoType);

    procedure removeall;
    end;

implementation

const
  infoNil:infoType=nil;

function tXObjList.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tXObjList.New_Node(const i:infoType; const pri,nex:pnode):pnode;
  var
    a:longint;
    p:pnodearr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(pnodearr);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,MyPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(tnode));
  FillChar(Result^,SizeOf(tnode),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tXObjList.PoolSize(size:integer);
// PoolSize;
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tXObjList.Del_Node(node:pnode);
// del_node
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tXObjList.Create(size:integer);
// Create
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tPtrPool.Create;
  end;

procedure tXObjList.RemoveAll;
// RemoveAll
  var
    x:pnode;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=infoNil;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tXObjList.Destroy;
// Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  pool.destroy;

  inherited;
  end;

function tXObjList.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tXObjList.addFirst(const info: infoType);
  var
    nn:pnode;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tXObjList.addLast(const info: infoType);
  var
    nn:pnode;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tXObjList.First: infoType;
  begin
  if fFirst=nil then
    Result:=infoNil
  else
    Result:=fFirst^.info;
  end;

function tXObjList.Last: infoType;
  begin
  if fLast=nil then
    Result:=infoNil
  else
    Result:=fLast^.info;
  end;

procedure tXObjList.removeFirst;
  var
    x:pnode;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=infoNil;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tXObjList.removeLast;
  var
    x:pnode;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=infoNil;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tXObjList.removeThis(const info: infoType);
  var
    x:pnode;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=infoNil;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

end.
