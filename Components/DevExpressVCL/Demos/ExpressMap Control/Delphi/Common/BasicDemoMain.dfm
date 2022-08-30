object frmBasicDemoMain: TfrmBasicDemoMain
  Left = 0
  Top = 0
  Caption = 'frmBasicDemoMain'
  ClientHeight = 464
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cxGroupBox2: TcxGroupBox
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    TabOrder = 0
    Height = 464
    Width = 993
    object dxMapControl1: TdxMapControl
      Left = 2
      Top = 2
      Width = 989
      Height = 460
      Align = alClient
      TabOrder = 0
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 648
    Top = 176
  end
  object mmMain: TMainMenu
    Left = 416
    Top = 240
    object miOptions: TMenuItem
      Caption = 'Options'
    end
  end
end
