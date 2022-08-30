object ServerMain: TServerMain
  Left = 301
  Top = 223
  Width = 260
  Height = 163
  Caption = 'RTC MSG Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 20
    Top = 18
    Width = 67
    Height = 16
    Caption = 'Server Port'
  end
  object btnStart: TButton
    Left = 20
    Top = 49
    Width = 92
    Height = 31
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 133
    Top = 49
    Width = 92
    Height = 31
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = btnStopClick
  end
  object pInfo: TPanel
    Left = 0
    Top = 89
    Width = 242
    Height = 29
    Align = alBottom
    BevelOuter = bvLowered
    Caption = 'Server not listening.'
    TabOrder = 2
  end
  object ePort: TEdit
    Left = 103
    Top = 12
    Width = 90
    Height = 24
    TabOrder = 3
    Text = '80'
  end
  object Server: TRtcHttpServer
    MultiThreaded = True
    Timeout.AfterConnecting = 40
    ServerPort = '80'
    RestartOn.ListenLost = True
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    FixupRequest.RemovePrefix = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    MaxRequestSize = 64000
    MaxHeaderSize = 16000
    Left = 8
    Top = 50
  end
end
