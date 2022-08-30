{
  "Threaded Connection Provider wrapper"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br>)

  @exclude
}

unit rtcThrConnProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  rtcTypes,
  rtcThrPool,
  rtcConnProv;

type
  TRtcThrClientProvider = class(TRtcBasicClientProvider)
  protected
    function GetClientThread:TRtcThread; virtual; abstract;

  public
    procedure Release; override;

    function GetThread:TRtcThread; override;

    function inMainThread:boolean; override;
    function SyncEvent(Event:TRtcBasicEvent):boolean; override;
    function inThread:boolean; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  protected
    procedure Enter; override;
    procedure Leave; override;

    function GetClientThread:TRtcThread; override;

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

    <- *** end ***)
    end;

  TRtcNoThrClientProvider = class(TRtcBasicClientProvider)
  public
    function inMainThread:boolean; override;
    function SyncEvent(Event:TRtcBasicEvent):boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  protected
    procedure Enter; override;
    procedure Leave; override;

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

    <- *** end ***)
    end;

  TRtcThrServerProvider = class(TRtcBasicServerProvider)
  protected
    function GetServerThread:TRtcThread; virtual; abstract;
    function GetClientThread:TRtcThread; virtual; abstract;

  public
    function SyncEvent(Event:TRtcBasicEvent):boolean; override;

    function inMainThread:boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

    function GetClientThread:TRtcThread; override;
    function GetServerThread:TRtcThread; override;

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

  *** end ***)
    end;

  TRtcNoThrServerProvider = class(TRtcBasicServerProvider)
  public
    function SyncEvent(Event:TRtcBasicEvent):boolean; override;

    function inMainThread:boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

  *** end ***)
    end;

implementation

{ TRtcThrClientProvider }

function TRtcThrClientProvider.inMainThread: boolean;
  begin
  Result:=InsideMainThread;
  end;

function TRtcThrClientProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.InsideThread
  else if GetMultiThreaded then
    Result:=inMainThread
  else
    Result:=True;
  end;

function TRtcThrClientProvider.PostJob(Job:TObject; HighPriority: boolean): boolean;
  begin
  if Job=nil then
    Result:=True
  else if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded {and inMainThread} then
    begin
    if TRtcJob(Job).Run(nil) then TRtcJob(Job).Kill;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrClientProvider.SyncEvent(Event: TRtcBasicEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else if GetClientThread<>nil then
      begin
      GetClientThread.Sync(Event);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

procedure TRtcThrClientProvider.Release;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

function TRtcThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  end;

{ TRtcNoThrClientProvider }

function TRtcNoThrClientProvider.inMainThread: boolean;
  begin
  Result := InsideMainThread;
  end;

function TRtcNoThrClientProvider.inThread: boolean;
  begin
  Result:=True; // inMainThread;
  end;

function TRtcNoThrClientProvider.PostJob(Job:TObject; HighPriority: boolean): boolean;
  begin
  if Job=nil then
    Result:=True
  else if Job is TRtcJob then
    begin
    if TRtcJob(Job).Run(nil) then TRtcJob(Job).Kill;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcNoThrClientProvider.SyncEvent(Event: TRtcBasicEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcNoThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

{ TRtcThrServerProvider }

function TRtcThrServerProvider.inMainThread: boolean;
  begin
  Result:=InsideMainThread;
  end;

function TRtcThrServerProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.InsideThread
  else if GetServerThread<>nil then
    Result:=GetServerThread.InsideThread
  else if GetMultiThreaded then
    Result:=inMainThread
  else
    Result:=True;
  end;

function TRtcThrServerProvider.PostJob(Job:TObject; HighPriority: boolean): boolean;
  begin
  if Job=nil then
    Result:=True
  else if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority);
    end
  else if GetServerThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetServerThread,Job,HighPriority);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded {and inMainThread} then
    begin
    if TRtcJob(Job).Run(nil) then TRtcJob(Job).Kill;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.SyncEvent(Event: TRtcBasicEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else if GetClientThread<>nil then
      begin
      GetClientThread.Sync(Event);
      Result:=True;
      end
    else if GetServerThread<>nil then
      begin
      GetServerThread.Sync(Event);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  if not assigned(Result) then
    Result:=GetServerThread;
  end;

{ TRtcNoThrServerProvider }

function TRtcNoThrServerProvider.inMainThread: boolean;
  begin
  Result:=InsideMainThread;
  end;

function TRtcNoThrServerProvider.inThread: boolean;
  begin
  Result:=True; // inMainThread;
  end;

function TRtcNoThrServerProvider.PostJob(Job:TObject; HighPriority: boolean): boolean;
  begin
  if Job=nil then
    Result:=True
  else if Job is TRtcJob then
    begin
    if TRtcJob(Job).Run(nil) then TRtcJob(Job).Kill;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcNoThrServerProvider.SyncEvent(Event: TRtcBasicEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcNoThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

end.

