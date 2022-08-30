object WeblServerForm: TWeblServerForm
  Left = 339
  Top = 282
  Caption = 'Web Application Server '
  ClientHeight = 437
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 587
    Height = 41
    Align = alTop
    TabOrder = 0
    object StartButton: TButton
      Left = 29
      Top = 8
      Width = 57
      Height = 21
      Caption = '&Start'
      Default = True
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 111
      Top = 8
      Width = 57
      Height = 21
      Caption = 'S&top'
      Enabled = False
      TabOrder = 1
      OnClick = StopButtonClick
    end
    object RecheckCertsButton: TButton
      Left = 184
      Top = 8
      Width = 102
      Height = 21
      Caption = 'Recheck Ssl Certs'
      TabOrder = 2
      OnClick = RecheckCertsButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 405
      Top = 10
      Width = 146
      Height = 17
      Caption = 'Display HTTP Protocol'
      TabOrder = 3
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 587
    Height = 396
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        'Please note that this sample takes all it'#39's settings from an INI' +
        ' file.  '
      ''
      
        'The INI file needs to be manually edited to add multiple Host se' +
        'ctions to '
      'define the web server behaviour.  ')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 320
    Top = 5
  end
  object SslHttpAppSrv1: TSslHttpAppSrv
    ListenBacklog = 5
    MultiListenSockets = <>
    Port = '80'
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    MaxClients = 0
    DocDir = 'c:\wwwroot'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 0
    Options = []
    KeepAliveTimeSec = 10
    KeepAliveTimeXferSec = 300
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    MaxBlkSize = 8192
    BandwidthLimit = 0
    BandwidthSampling = 1000
    ServerHeader = 'Server: ICS-HttpServer-8.57'
    OnServerStarted = SslHttpAppSrv1ServerStarted
    OnServerStopped = SslHttpAppSrv1ServerStopped
    OnClientConnect = SslHttpAppSrv1ClientConnect
    OnGetDocument = SslHttpAppSrv1GetDocument
    OnHeadDocument = SslHttpAppSrv1HeadDocument
    OnPostDocument = SslHttpAppSrv1PostDocument
    OnOptionsDocument = SslHttpAppSrv1OptionsDocument
    OnPutDocument = SslHttpAppSrv1PutDocument
    OnDeleteDocument = SslHttpAppSrv1DeleteDocument
    OnTraceDocument = SslHttpAppSrv1TraceDocument
    OnPatchDocument = SslHttpAppSrv1PatchDocument
    OnBeforeProcessRequest = SslHttpAppSrv1BeforeProcessRequest
    OnAfterAnswer = SslHttpAppSrv1AfterAnswer
    OnHttpMimeContentType = SslHttpAppSrv1HttpMimeContentType
    OnAuthGetPassword = SslHttpAppSrv1AuthGetPassword
    OnAuthResult = SslHttpAppSrv1AuthResult
    OnAuthGetType = SslHttpAppSrv1AuthGetType
    OnAuthNtlmBeforeValidate = SslHttpAppSrv1AuthNtlmBeforeValidate
    AuthTypes = []
    AuthRealm = 'ics'
    OnBgException = SslHttpAppSrv1BgException
    SocketErrs = wsErrFriendly
    ExclusiveAddr = True
    onWellKnownDir = SslHttpAppSrv1WellKnownDir
    SessionTimeout = 300
    MaxSessions = 100
    OnDeleteSession = SslHttpAppSrv1DeleteSession
    OnVirtualException = SslHttpAppSrv1VirtualException
    OnDisplay = SslHttpAppSrv1Display
    SslEnable = False
    IcsHosts = <>
    SslCliCertMethod = sslCliCertNone
    SslCertAutoOrder = False
    CertExpireDays = 30
    SslX509Certs = IcsSslX509Certs
    OnSslHandshakeDone = SslHttpAppSrv1SslHandshakeDone
    Left = 365
    Top = 5
  end
  object IcsSslX509Certs: TSslX509Certs
    AcmeAccKeyType = PrivKeyRsa2048
    AutoOrderComplete = False
    CertSubAltNames = <>
    CertCsrOrigin = CsrOriginProps
    CertOutFmts = []
    CertSerNumType = SerNumRandom
    CertSignDigestType = Digest_sha256
    CertValidity = 365
    DebugLevel = DebugConn
    DomWebSrvIP = '0.0.0.0'
    LogJson = False
    LogPkeys = False
    OAAuthType = OAuthTypeWeb
    OARefreshAuto = False
    OARefrMinsPrior = 120
    OAWebSrvIP = '127.0.0.1'
    OAWebSrvPort = '8080'
    PrivKeyCipher = PrivKeyEncNone
    PrivKeyType = PrivKeyRsa2048
    SeqOrderNum = 0
    SuppCertChallenge = ChallNone
    SupplierProto = SuppProtoNone
    OnCertProg = IcsSslX509CertsCertProg
    OnNewCert = IcsSslX509CertsNewCert
    OnOAuthAuthUrl = IcsSslX509CertsOAuthAuthUrl
    OnChallengeDNS = IcsSslX509CertsChallengeDNS
    Left = 545
    Top = 5
  end
end
