program OverbyteIcsSvcTcp;

uses
  SvcMgr,
  OverbyteIcsSvcTcp1 in 'OverbyteIcsSvcTcp1.pas' {IcsTcpSvc: TService},
  OverbyteIcsTcpCmd in 'OverbyteIcsTcpCmd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTcpSvc, IcsTcpSvc);
  Application.Run;
end.
