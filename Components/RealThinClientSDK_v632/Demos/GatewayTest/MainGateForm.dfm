object GateForm: TGateForm
  Left = 221
  Top = 117
  Width = 363
  Height = 150
  AutoSize = True
  Caption = 'Simple Gateway'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 105
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 24
      Height = 16
      Caption = 'Port'
    end
    object shGateway: TShape
      Left = 272
      Top = 60
      Width = 61
      Height = 37
      Brush.Color = clRed
      Shape = stRoundRect
    end
    object Label2: TLabel
      Left = 172
      Top = 4
      Width = 46
      Height = 16
      Caption = 'Send at'
    end
    object Label3: TLabel
      Left = 236
      Top = 28
      Width = 22
      Height = 16
      Caption = 'Kbit'
    end
    object lblStatus: TLabel
      Left = 4
      Top = 60
      Width = 78
      Height = 16
      Caption = 'Click START'
    end
    object lblConnect: TLabel
      Left = 92
      Top = 80
      Width = 16
      Height = 16
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 4
      Top = 80
      Width = 77
      Height = 16
      Caption = 'Connections:'
    end
    object Label5: TLabel
      Left = 76
      Top = 4
      Width = 65
      Height = 16
      Caption = 'Receive at'
    end
    object Label6: TLabel
      Left = 140
      Top = 28
      Width = 22
      Height = 16
      Caption = 'Kbit'
    end
    object ePort: TEdit
      Left = 4
      Top = 24
      Width = 61
      Height = 24
      TabOrder = 0
      Text = '80'
    end
    object btnStart: TButton
      Left = 272
      Top = 8
      Width = 61
      Height = 45
      Caption = 'START'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object eSendSpeed: TSpinEdit
      Left = 172
      Top = 24
      Width = 61
      Height = 26
      MaxValue = 4096
      MinValue = 1
      TabOrder = 2
      Value = 48
    end
    object eRecvSpeed: TSpinEdit
      Left = 76
      Top = 24
      Width = 61
      Height = 26
      MaxValue = 4096
      MinValue = 1
      TabOrder = 3
      Value = 48
    end
  end
  object Server: TRtcHttpServer
    MultiThreaded = True
    OnException = ServerException
    RestartOn.ListenLost = True
    RestartOn.ListenError = True
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    OnListenError = ServerListenError
    FixupRequest.RemovePrefix = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    MaxHeaderSize = 2048
    OnInvalidRequest = ServerInvalidRequest
    TimeoutsOfAPI.SendTimeout = 240
    TimeoutsOfAPI.ReceiveTimeout = 240
    Left = 136
    Top = 64
  end
  object StatusTimer: TTimer
    Interval = 500
    OnTimer = StatusTimerTimer
    Left = 228
    Top = 64
  end
  object MyGate: TRtcGateway
    Server = Server
    GateFileName = '/'
    Left = 168
    Top = 64
  end
end
