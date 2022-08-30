object Form3: TForm3
  Left = 163
  Top = 104
  Width = 435
  Height = 300
  Caption = 'DBGrid'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 232
    Width = 427
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      Left = 4
      Top = 8
      Width = 240
      Height = 25
      DataSource = Form1.DS1
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      Left = 428
      Top = 9
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 1
      Kind = bkClose
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 427
    Height = 232
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object DBGrid1: TDBGrid
      Left = 1
      Top = 1
      Width = 425
      Height = 230
      Align = alClient
      DataSource = Form1.DS1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
end
