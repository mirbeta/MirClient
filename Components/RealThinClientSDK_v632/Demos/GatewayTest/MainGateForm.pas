unit MainGateForm;

interface

{$include rtcDeploy.inc}

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  rtcInfo, rtcConn,

  rtcLog,
  rtcDataSrv,
  rtcHttpSrv,
  rtcGateSrv,
  rtcGateConst,

  ExtCtrls, Spin;

type
  TGateForm = class(TForm)
    Server: TRtcHttpServer;
    Panel1: TPanel;
    Label1: TLabel;
    shGateway: TShape;
    Label2: TLabel;
    Label3: TLabel;
    ePort: TEdit;
    btnStart: TButton;
    eSendSpeed: TSpinEdit;
    lblStatus: TLabel;
    lblConnect: TLabel;
    StatusTimer: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    eRecvSpeed: TSpinEdit;
    Label6: TLabel;
    MyGate: TRtcGateway;
    procedure btnStartClick(Sender: TObject);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure ServerException(Sender: TRtcConnection; E: Exception);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GateForm: TGateForm;

implementation

{$R *.dfm}

procedure TGateForm.btnStartClick(Sender: TObject);
  begin
  if not Server.isListening then
    begin
    SetupConnectionSpeed(eRecvSpeed.Value,eSendSpeed.Value);

    Server.ServerPort:=ePort.Text;
    Server.Listen();
    end
  else
    Server.StopListenNow;
  end;

procedure TGateForm.ServerListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStart)
  else
    begin
    shGateway.Brush.Color:=clGreen;
    lblStatus.Caption:='Listening on Port '+Sender.LocalPort;
    btnStart.Caption:='STOP';
    end;
  end;

procedure TGateForm.ServerListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStop)
  else
    begin
    shGateway.Brush.Color:=clRed;
    lblStatus.Caption:='Stopped listening';
    btnStart.Caption:='START';
    end;
  end;

procedure TGateForm.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenError,E)
  else
    lblStatus.Caption:=E.Message;
  end;

procedure TGateForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  Server.StopListenNow;
  end;

procedure TGateForm.FormCreate(Sender: TObject);
  begin
  StatusTimer.Enabled:=True;
  end;

procedure TGateForm.FormDestroy(Sender: TObject);
  begin
  StatusTimer.Enabled:=False;
  end;

procedure TGateForm.StatusTimerTimer(Sender: TObject);
  begin
  lblConnect.Caption:=IntToStr(Server.TotalConnectionCount);
  end;

procedure TGateForm.ServerException(Sender: TRtcConnection; E: Exception);
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' ERROR '+E.ClassName+':'+E.Message,'ERROR');
  end;

procedure TGateForm.ServerInvalidRequest(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' Invalid: '+Srv.Request.Method+' '+Srv.Request.URI,'HTTP');
  if Srv.Request.Complete then
    begin
    Srv.Response.Status(404,'*');
    Srv.Write;
    end
  else
    Srv.Disconnect;
  end;

procedure TGateForm.ServerRequestNotAccepted(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' NOT Accepted: '+Srv.Request.Method+' '+Srv.Request.URI+' ('+Srv.Request['CONTENT-LENGTH']+' bytes)','HTTP');
  if Srv.Request.Complete then
    begin
    Srv.Response.Status(404,'*');
    Srv.Write;
    end
  else
    Srv.Disconnect;
  end;

end.
