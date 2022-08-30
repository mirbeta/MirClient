object SocksTestForm: TSocksTestForm
  Left = 175
  Top = 163
  Width = 511
  Height = 252
  Caption = 'Socks Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 109
    Width = 503
    Height = 109
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 109
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 12
      Width = 82
      Height = 13
      Caption = 'Target Hostname'
    end
    object Label2: TLabel
      Left = 20
      Top = 40
      Width = 53
      Height = 13
      Caption = 'Target Port'
    end
    object Label3: TLabel
      Left = 255
      Top = 12
      Width = 64
      Height = 13
      Caption = 'Socks Server'
    end
    object Label4: TLabel
      Left = 267
      Top = 36
      Width = 52
      Height = 13
      Caption = 'Socks Port'
    end
    object Label5: TLabel
      Left = 240
      Top = 60
      Width = 79
      Height = 13
      Caption = 'Socks Usercode'
    end
    object Label6: TLabel
      Left = 240
      Top = 84
      Width = 79
      Height = 13
      Caption = 'Socks Password'
    end
    object ConnectButton: TButton
      Left = 8
      Top = 80
      Width = 65
      Height = 25
      Caption = '&Connect'
      TabOrder = 0
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 80
      Top = 80
      Width = 65
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 1
      OnClick = DisconnectButtonClick
    end
    object TargetHostEdit: TEdit
      Left = 104
      Top = 8
      Width = 125
      Height = 21
      TabOrder = 2
      Text = 'TargetHostEdit'
    end
    object TargetPortEdit: TEdit
      Left = 104
      Top = 36
      Width = 125
      Height = 21
      TabOrder = 3
      Text = 'TargetPortEdit'
    end
    object SocksServerEdit: TEdit
      Left = 332
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'SocksServerEdit'
    end
    object SocksPortEdit: TEdit
      Left = 332
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 5
      Text = 'SocksPortEdit'
    end
    object SocksUsercodeEdit: TEdit
      Left = 332
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 6
      Text = 'SocksUsercodeEdit'
    end
    object SocksPasswordEdit: TEdit
      Left = 332
      Top = 80
      Width = 121
      Height = 21
      TabOrder = 7
      Text = 'SocksPasswordEdit'
    end
    object SocksAuthCheckBox: TCheckBox
      Left = 139
      Top = 59
      Width = 90
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Authentication'
      TabOrder = 8
    end
    object ClearButton: TButton
      Left = 152
      Top = 80
      Width = 65
      Height = 25
      Caption = 'C&lear'
      TabOrder = 9
      OnClick = ClearButtonClick
    end
    object Socks4RadioButton: TRadioButton
      Left = 8
      Top = 59
      Width = 57
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Socks 4'
      TabOrder = 10
    end
    object Socks5RadioButton: TRadioButton
      Left = 72
      Top = 59
      Width = 57
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Socks 5'
      Checked = True
      TabOrder = 11
      TabStop = True
    end
  end
  object WSocket1: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ComponentOptions = []
    OnDataAvailable = WSocket1DataAvailable
    OnSessionClosed = WSocket1SessionClosed
    OnSessionConnected = WSocket1SessionConnected
    OnSocksConnected = WSocket1SocksConnected
    OnSocksError = WSocket1SocksError
    OnSocksAuthState = WSocket1SocksAuthState
    Left = 188
    Top = 124
  end
end
