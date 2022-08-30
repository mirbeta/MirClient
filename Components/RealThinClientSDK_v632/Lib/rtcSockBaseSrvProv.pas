{
  "Server Socket Connection Provider wrapper"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}
unit rtcSockBaseSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcLog,
  rtcFastStrings,
  rtcSyncObjs,

  rtcPlugins,
  rtcThrConnProv, // Threaded connection provider wrapper

  rtcConn,
  rtcConnProv; // Basic connection provider wrapper

const
  LOG_REFUSED_CONNECTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  TRtcBaseSockServerProvider = class(TRtcThrServerProvider)
  private
    FCS:TRtcCritSec;
    FCryptPlugin: TRtcCryptPlugin;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

  protected
    FCryptObject:TObject;

    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider); virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    property CryptPlugin        : TRtcCryptPlugin   read FCryptPlugin
                                                    write FCryptPlugin;
    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;

    property CryptObject:TObject read FCryptObject;
    end;

implementation

{ TRtcBaseSockServerProvider }

constructor TRtcBaseSockServerProvider.Create;
  begin
  inherited;

  FCryptObject:=nil;

  FCS:=TRtcCritSec.Create;

  FPeerPort:='';
  FPeerAddr:='0.0.0.0';
  FLocalPort:='';
  FLocalAddr:='0.0.0.0';
  end;

destructor TRtcBaseSockServerProvider.Destroy;
  begin
  try
    RtcFreeAndNil(FCS);

    FPeerPort:='';
    FPeerAddr:='';
    FLocalPort:='';
    FLocalAddr:='';

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcBaseSockServerProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcBaseSockServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  FCryptPlugin:=TRtcBaseSockServerProvider(Dup).CryptPlugin;
  end;

procedure TRtcBaseSockServerProvider.Enter;
  begin
  FCS.Acquire;
  end;

procedure TRtcBaseSockServerProvider.Leave;
  begin
  FCS.Release;
  end;

end.
