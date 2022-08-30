object ServerForm: TServerForm
  Left = 269
  Top = 236
  Width = 568
  Height = 306
  Caption = 'ServerForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object PortLabel: TLabel
    Left = 16
    Top = 240
    Width = 26
    Height = 16
    Caption = 'Port'
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 560
    Height = 225
    Align = alTop
    Lines.Strings = (
      'Memo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object QuitButton: TButton
    Left = 448
    Top = 236
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Quit'
    TabOrder = 1
    OnClick = QuitButtonClick
  end
  object AboutButton: TButton
    Left = 360
    Top = 236
    Width = 75
    Height = 25
    Caption = '&About'
    TabOrder = 2
    OnClick = AboutButtonClick
  end
  object PortEdit: TEdit
    Left = 56
    Top = 236
    Width = 49
    Height = 24
    TabOrder = 3
    Text = 'telnet'
  end
  object ChangePortButton: TButton
    Left = 120
    Top = 236
    Width = 75
    Height = 25
    Caption = 'Change'
    TabOrder = 4
    OnClick = ChangePortButtonClick
  end
  object SrvSocket: TWSocket
    LineEnd = #13#10
    Addr = '0.0.0.0'
    Port = 'telnet'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ComponentOptions = []
    OnSessionClosed = SrvSocketSessionClosed
    OnSessionAvailable = SrvSocketSessionAvailable
    Left = 28
    Top = 36
  end
end
