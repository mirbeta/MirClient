object SenderForm: TSenderForm
  Left = 160
  Top = 100
  Caption = 'Sender'
  ClientHeight = 341
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 559
    Height = 73
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object Label2: TLabel
      Left = 144
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label3: TLabel
      Left = 12
      Top = 40
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object Label4: TLabel
      Left = 356
      Top = 12
      Width = 35
      Height = 13
      Caption = 'Repeat'
    end
    object Label5: TLabel
      Left = 236
      Top = 12
      Width = 56
      Height = 13
      Caption = 'Line Length'
    end
    object CountLabel: TLabel
      Left = 396
      Top = 40
      Width = 28
      Height = 13
      Caption = 'Count'
    end
    object ServerEdit: TEdit
      Left = 48
      Top = 8
      Width = 89
      Height = 21
      TabOrder = 0
      Text = 'ServerEdit'
    end
    object PortEdit: TEdit
      Left = 168
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 1
      Text = 'PortEdit'
    end
    object DataEdit: TEdit
      Left = 48
      Top = 36
      Width = 177
      Height = 21
      TabOrder = 2
      Text = 'DataEdit'
    end
    object RepeatEdit: TEdit
      Left = 400
      Top = 8
      Width = 37
      Height = 21
      TabOrder = 5
      Text = 'RepeatEdit'
    end
    object ContCheckBox: TCheckBox
      Left = 460
      Top = 6
      Width = 65
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Continous'
      TabOrder = 6
      OnClick = ContCheckBoxClick
    end
    object ActionButton: TButton
      Left = 240
      Top = 36
      Width = 45
      Height = 21
      Caption = '&Start'
      TabOrder = 3
      OnClick = ActionButtonClick
    end
    object LengthEdit: TEdit
      Left = 300
      Top = 8
      Width = 41
      Height = 21
      TabOrder = 4
      Text = 'LengthEdit'
    end
    object DisplayDataCheckBox: TCheckBox
      Left = 444
      Top = 22
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Data'
      TabOrder = 7
      OnClick = DisplayDataCheckBoxClick
    end
    object UseDataSentCheckBox: TCheckBox
      Left = 428
      Top = 38
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Use onDataSent'
      TabOrder = 8
      OnClick = UseDataSentCheckBoxClick
    end
    object PauseButton: TButton
      Left = 292
      Top = 36
      Width = 49
      Height = 21
      Caption = '&Pause'
      TabOrder = 9
      Visible = False
      OnClick = PauseButtonClick
    end
    object AutoStartButton: TButton
      Left = 348
      Top = 36
      Width = 41
      Height = 21
      Caption = '&Auto'
      TabOrder = 10
      OnClick = AutoStartButtonClick
    end
    object LingerCheckBox: TCheckBox
      Left = 477
      Top = 55
      Width = 48
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Linger'
      TabOrder = 11
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 73
    Width = 559
    Height = 268
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 1
    ExplicitTop = 69
    ExplicitHeight = 272
  end
  object WSocket1: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    OnDataAvailable = WSocket1DataAvailable
    OnDataSent = WSocket1DataSent
    OnSessionClosed = WSocket1SessionClosed
    OnSessionConnected = WSocket1SessionConnected
    OnDnsLookupDone = WSocket1DnsLookupDone
    Left = 140
    Top = 92
  end
end
