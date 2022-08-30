{
  "Client Socket Connection Provider wrapper"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}
unit rtcSockBaseCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSyncObjs,

  rtcLog,
  rtcConn,
  rtcConnProv, // Basic connection provider wrapper

  rtcPlugins,
  rtcThrConnProv; // Threaded connection provider wrapper

type
  TRtcBaseSockClientProvider = class(TRtcThrClientProvider)
  private
    FCS:TRtcCritSec;
    FCryptPlugin: TRtcCryptPlugin;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

  protected
    FCryptObject:TObject;

    procedure Enter; override;
    procedure Leave; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property CryptPlugin        : TRtcCryptPlugin   read FCryptPlugin
                                                    write FCryptPlugin;

    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;

    property CryptObject:TObject read FCryptObject;
    end;

implementation

{ TRtcBaseSockClientProvider }

constructor TRtcBaseSockClientProvider.Create;
  begin
  inherited;

  FCryptObject:=nil;

  FCS:=TRtcCritSec.Create;

  FPeerPort:='';
  FPeerAddr:='0.0.0.0';
  FLocalPort:='';
  FLocalAddr:='0.0.0.0';
  end;

destructor TRtcBaseSockClientProvider.Destroy;
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
        Log('TRtcBaseSockClientProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcBaseSockClientProvider.Enter;
  begin
  FCS.Acquire;
  end;

procedure TRtcBaseSockClientProvider.Leave;
  begin
  FCS.Release;
  end;

end.
