{
  @html(<b>)
  Client Connection Pool classes
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit implements Client connection Pools with the abbility to use sticky Sessions,
  which are used by the RTC Load Balancer and simplify handling of Connections with sticky Sessions.
}
unit rtcCliPool;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes,
  rtcTypes, rtcConn, rtcDataCli, rtcInfo, rtcSyncObjs,
  memXObjList, memStrIntList, memStrList,
  rtcFastStrings;

type
  { @abstract(Pool of Client connections towards one Server) }
  TRtcClientPool=class(TObject)
  protected
    FID:integer;
    FIdle:TXObjList;
    FCount:integer;
    FSessExp:tStrIntList; // Sessions sorted by Expiring date+time & Session ID
    FSessList:tStrList; // Sessions sorted by Session ID

    function GetIdleCount: integer;

    { This method needs to be implemented by a descendant class.
      The purpose of this method is to create a new TRtcDataClient component
      and set all its properties so that a connection can be made to the Server. @html(<br><br>)

      If a connection component can NOT be created at this point (for example,
      the Server is too busy and can not handle new connections yet), return NIL. @html(<br><br>)

      If the Server can accept a new connection, then create a new TRtcHttpClient component
      and set all its properties required for opening a connection to the Server. }
    function NewClient:TRtcDataClient; virtual;

  public
    // Standard constructor
    constructor Create(StackSize:integer); virtual;
    // Standard destructor
    destructor Destroy; override;

    { Calls "Disconnect" on all idle Client connections inside the Pool. }
    procedure Close;

    { If there are idle DataRequest components inside the Pool, returns the topmost
      idle DataRequest component from the Pool and removes that component from the Pool,
      incrementing the value of "ActiveClients" and decrementing the value of "IdleClients".

      If the Pool is empty, calls "NewClient" to create a new connection. If "NewClient"
      returns a valid connection component, a DataRequest component is created and returned,
      incrementing the value of "ActiveClients" without changing the value of "IdleClients".

      If the Pool is empty but "NewClient" returns NIL, GetDataRequest will also return NIL,
      without changing either "ActiveClients" nor "IdleClients" values. }
    function GetDataRequest:TRtcDataRequest;

    { Places a DataRequest component received from GetDataRequest back to the top of our Pool,
      incrementing the value of "IdleClients" and decrementing the value of "ActiveClients".
      Returns the Pool index where the component was placed or -1 if the Pool does not exist. }
    procedure PutDataRequest(DataRequest:TRtcDataRequest; ToBottom:boolean=False);

    { If Session "SID" already exists, update its expiration time.
      If Session "SID" does not exist, open a new session "SID" and set its expiration time. }
    procedure OpenOrUpdateSession(const SID:RtcString; ExpireTime:TDateTime);

    { If Session "SID" already exists, return FALSE.
      If Session "SID" does NOT yet exist, Open new Session "SID",
        set Sessions expiration time to "ExpireTime" and return TRUE. }
    function OpenSession(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" does NOT exist, return FALSE.
      If Session "SID" exists, update its expiration time to "ExpireTime" and return TRUE. }
    function UpdateSession(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" does NOT exist, return FALSE.
      If Session "SID" exists and expires before "BeforeTime",
      update its expiration time to "ExpireTime" and return TRUE. }
    function UpdateSessionIfExpiresBefore(const SID:RtcString; BeforeTime,ExpireTime:TDateTime):boolean;

    { If Session "SID" exists and it would expire before "ExpireTime", return TRUE.
      If Session "SID" does not exist or if it would expire after "ExpireTime", return FALSE. }
    function SessionExpiresBefore(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" exist, return TRUE.
      If Session "SID" does NOT exist, return FALSE. }
    function HaveSession(const SID:RtcString):boolean;

    { If Session "SID" exist, Close the Session and return TRUE.
      If Session "SID" did NOT exist, return FALSE. }
    function CloseSession(const SID:RtcString):boolean;

    { Total number of active Sessions in this Pool. }
    function ActiveSessions:integer;

    { Remove all Sessions with expiration time before "ExpireTime".
      Returns the number of Sessions removed. }
    function RemoveExpiredSessions(ExpireTime:TDateTime):integer;

    { If this Pool is inside a MultiClientPool,
      this ID is the "index" where it can be found in the MultiClientPool.
      For a stand-alone Pool (not part of a MultiClientPool), ID will be -1. }
    property ID:integer read FID;

    { Total number of Clients currently Active (in use). }
    property ActiveClients:integer read FCount;

    { Total number of Clients currently idle (NOT in use). }
    property IdleClients:integer read GetIdleCount;
    end;

  { @abstract(Multiple Pools of Client connections, each Pool pointing at a different Server) }
  TRtcMultiClientPool=class(TObject)
  protected
    FPools:array of TRtcClientPool;
    FPoolCnt:integer;

    function GetActiveClients: integer;
    function GetIdleClients: integer;
    function GetPool(index: integer): TRtcClientPool;

  public
    // Standard constructor
    constructor Create; virtual;
    // Standard destructor
    destructor Destroy; override;

    { Add a new Client Pool to our "Multi Client Pool".
      Returns the index at which the new Pool is now. }
    function AddPool(NewPool:TRtcClientPool):integer;

    { Extract "Pool Index" from Client connection component }
    function GetPoolIndex(Con:TRtcDataClient):integer;

    // Close Client connections in all Pools
    procedure CloseAll;

    { Places a DataRequest component received from GetDataRequest back to the originating Pool,
      incrementing the value of "IdleClients" and decrementing the value of "ActiveClients". }
    function PutDataRequest(DataRequest:TRtcDataRequest; ToBottom:boolean=False):integer;

    { Update Session "SID" to expire after "ExpireTime".
      Returns the index of the Pool where the Session was updated,
      or -1 if Session "SID" was not found. }
    function UpdateSession(const SID:RtcString; ExpireTime:TDateTime):integer;

    { If Session "SID" exists and it would expire before "ExpireTime", return the Pool index where the Session is.
      If Session "SID" does not exist or if it would expire after "ExpireTime", return -1. }
    function SessionExpiresBefore(const SID:RtcString; ExpireTime:TDateTime):integer;

    { If Session "SID" does NOT exist or if it would expire after "BeforeTime", return -1.
      If Session "SID" exists and would expire before "BeforeTime",
      update its expiration time to "ExpireTime" and return the Pool index where the Session is. }
    function UpdateSessionIfExpiresBefore(const SID:RtcString; BeforeTime,ExpireTime:TDateTime):integer;

    { Check if Session "SID" exists in one of the Pools.
      Returns the index of the Pool where the Session was found,
      or -1 if Session "SID" could not be found. }
    function HaveSession(const SID:RtcString):integer;

    { Close Session "SID".
      Returns the index of the Pool where the Session was found,
      or -1 if the Session "SID" could not be found. }
    function CloseSession(const SID:RtcString):integer;

    { Returns the total number of active Sessions (sum of all Pools). }
    function ActiveSessions:integer;

    { Removes all Sessions with expiration time before "ExpireTime" (from all Pools). }
    function RemoveExpiredSessions(ExpireTime:TDateTime):integer;

    { Total number of active Clients (all Pools). }
    property ActiveClients:integer read GetActiveClients;

    { Total number of idle Clients (all Pools). }
    property IdleClients:integer read GetIdleClients;

    { Number of Pools in this MultiClientPool }
    property PoolCount:integer read FPoolCnt;

    { Pool at position "index" (starting at 0, max = PoolCount-1) }
    property Pool[index:integer]:TRtcClientPool read GetPool;
    end;

implementation

function SessionTimeToStr(v:TDateTime):RtcString;
  var
    y,m,d,hh,mm,ss,ms:word;
    p:integer;
    str:RtcString;
    len:word;
  begin
  Result:='00000000000000000';

  DecodeDate(v, y,m,d);
  DecodeTime(v, hh,mm,ss,ms);

  p:=1;
  str:=Int2Str(y); len:=length(str);
  Inc(p,4-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(m); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(d); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(hh); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(mm); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(ss); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(ms); len:=length(str);
  Inc(p,3-len); Move(str[1],Result[p],len*SizeOf(RtcChar));
  end;

{ TRtcClientPool }

constructor TRtcClientPool.Create(StackSize:integer);
  begin
  inherited Create;
  FCount:=0;
  FID:=-1;
  FIdle:=tXObjList.Create(StackSize); // Idle TRtcDataRequest components
  FSessExp:=tStrIntList.Create(StackSize); // Sessions sorted by Expiring date+time & Session ID
  FSessList:=tStrList.Create(StackSize); // Sessions sorted by Session ID
  end;

destructor TRtcClientPool.Destroy;
  var
    a:integer;
    req:TRtcDataRequest;
    cli:TRtcDataClient;
  begin
  for a:=1 to FIdle.Count do
    begin
    req:=TRtcDataRequest(FIdle.First);
    FIdle.removeFirst;

    cli:=req.Client;
    req.Client:=nil;
    cli.Free;
    req.Free;
    end;
  RtcFreeAndNil(FIdle);
  RtcFreeAndNil(FSessExp);
  RtcFreeAndNil(FSessList);
  inherited;
  end;

procedure TRtcClientPool.Close;
  var
    a:integer;
    req:TRtcDataRequest;
    cli:TRtcDataClient;
  begin
  for a:=1 to FIdle.Count do
    begin
    req:=TRtcDataRequest(FIdle.First);
    FIdle.removeFirst;
    FIdle.addLast(req);
    cli:=req.Client;
    cli.AutoConnect:=False;
    cli.Disconnect;
    end;
  end;

function TRtcClientPool.GetIdleCount: integer;
  begin
  Result:=FIdle.Count;
  end;

function TRtcClientPool.GetDataRequest: TRtcDataRequest;
  var
    Cli:TRtcDataClient;
  begin
  if FIdle.Count>0 then
    begin
    Result:=TRtcDataRequest(FIdle.First);
    FIdle.removeFirst;
    Inc(FCount);
    end
  else
    begin
    Cli:=NewClient;
    if assigned(Cli) then
      begin
      Result:=TRtcDataRequest.Create(nil);
      Result.Tag:=FID+1;
      Result.Client:=Cli;
      Cli.Tag:=FID+1;
      Inc(FCount);
      end
    else
      Result:=nil;
    end;
  end;

procedure TRtcClientPool.PutDataRequest(DataRequest: TRtcDataRequest; ToBottom:boolean=False);
  begin
  if ToBottom then
    FIdle.addLast(DataRequest)
  else
    FIdle.addFirst(DataRequest);
  Dec(FCount);
  end;

function TRtcClientPool.NewClient: TRtcDataClient;
  begin
  // Descendant class needs to implement this
  // to return a fully set up Client component
  Result:=nil;
  end;

function TRtcClientPool.ActiveSessions: integer;
  begin
  Result:=FSessList.Count;
  end;

procedure TRtcClientPool.OpenOrUpdateSession(const SID: RtcString; ExpireTime: TDateTime);
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp='' then
    begin
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.insert(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    end
  else
    begin
    FSessExp.remove(exp+'-'+SID);
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.change(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    end;
  end;

function TRtcClientPool.OpenSession(const SID: RtcString; ExpireTime: TDateTime):boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp='' then
    begin
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.insert(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.UpdateSession(const SID: RtcString; ExpireTime: TDateTime):boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    FSessExp.remove(exp+'-'+SID);
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.change(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.CloseSession(const SID: RtcString): boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    FSessExp.remove(exp+'-'+SID);
    FSessList.remove(SID);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.HaveSession(const SID: RtcString): boolean;
  begin
  Result:=FSessList.search(SID)<>'';
  end;

function TRtcClientPool.RemoveExpiredSessions(ExpireTime: TDateTime):integer;
  var
    exp,exp_sid,sid:RtcString;
    loc,i:integer;
  begin
  Result:=0;
  exp:=SessionTimeToStr(ExpireTime);

  exp_sid:=FSessExp.search_min(i);
  while (exp_sid<>'') and (exp_sid<exp) do
    begin
    Inc(Result);
    loc:=PosEx('-',exp_sid);
    sid:=Copy(exp_sid, loc+1, length(exp_sid)-loc);

    FSessList.remove(sid);
    FSessExp.remove(exp_sid);

    exp_sid:=FSessExp.search_min(i);
    end;
  end;

function TRtcClientPool.SessionExpiresBefore(const SID: RtcString; ExpireTime: TDateTime): boolean;
  var
    exp,exp2:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    exp2:=SessionTimeToStr(ExpireTime);
    Result:=exp<exp2;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.UpdateSessionIfExpiresBefore(const SID: RtcString; BeforeTime, ExpireTime: TDateTime): boolean;
  var
    exp,exp2:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    exp2:=SessionTimeToStr(BeforeTime);
    if exp<exp2 then
      begin
      FSessExp.remove(exp+'-'+SID);
      exp:=SessionTimeToStr(ExpireTime);
      FSessList.change(SID,exp);
      FSessExp.insert(exp+'-'+SID,1);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

{ TRtcMultiClientPool }

constructor TRtcMultiClientPool.Create;
  begin
  inherited;
  FPoolCnt:=0;
  SetLength(FPools,0);
  end;

destructor TRtcMultiClientPool.Destroy;
  var
    a:integer;
  begin
  for a:=0 to FPoolCnt-1 do
    RtcFreeAndNil(FPools[a]);
  SetLength(FPools,0);
  FPoolCnt:=0;
  inherited;
  end;

function TRtcMultiClientPool.AddPool(NewPool: TRtcClientPool):integer;
  begin
  SetLength(FPools,FPoolCnt+1);
  FPools[FPoolCnt]:=NewPool;
  NewPool.FID:=FPoolCnt;
  Result:=FPoolCnt;
  Inc(FPoolCnt);
  end;

procedure TRtcMultiClientPool.CloseAll;
  var
    a:integer;
  begin
  for a:=0 to FPoolCnt-1 do
    FPools[a].Close;
  end;

function TRtcMultiClientPool.GetActiveClients: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].ActiveClients;
  end;

function TRtcMultiClientPool.GetIdleClients: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].IdleClients;
  end;

function TRtcMultiClientPool.GetPool(index: integer): TRtcClientPool;
  begin
  if (index>=0) and (index<FPoolCnt) then
    Result:=FPools[index]
  else
    Result:=nil;
  end;

function TRtcMultiClientPool.ActiveSessions: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].ActiveSessions;
  end;

function TRtcMultiClientPool.RemoveExpiredSessions(ExpireTime: TDateTime):integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].RemoveExpiredSessions(ExpireTime);
  end;

function TRtcMultiClientPool.CloseSession(const SID: RtcString): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].CloseSession(SID) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.HaveSession(const SID: RtcString): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].HaveSession(SID) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.UpdateSession(const SID: RtcString; ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].UpdateSession(SID,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.SessionExpiresBefore(const SID: RtcString; ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].SessionExpiresBefore(SID,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.UpdateSessionIfExpiresBefore(const SID: RtcString; BeforeTime, ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].UpdateSessionIfExpiresBefore(SID,BeforeTime,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.PutDataRequest(DataRequest: TRtcDataRequest; ToBottom:boolean=False):integer;
  var
    id:integer;
  begin
  id:=DataRequest.Tag-1;
  if (id<0) or (id>=FPoolCnt) then
    Result:=-1
  else
    begin
    FPools[id].PutDataRequest(DataRequest,ToBottom);
    Result:=id;
    end
  end;

function TRtcMultiClientPool.GetPoolIndex(Con: TRtcDataClient): integer;
  begin
  if (Con.Tag>0) and (Con.Tag<=FPoolCnt) then
    Result:=Con.Tag-1
  else
    Result:=-1;
  end;

end.
