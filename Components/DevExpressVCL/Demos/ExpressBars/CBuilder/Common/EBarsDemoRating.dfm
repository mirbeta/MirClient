object EBarsDemoRatingForm: TEBarsDemoRatingForm
  Left = 399
  Top = 220
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo rating '
  ClientHeight = 254
  ClientWidth = 306
  Color = 15921902
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 256
    Height = 13
    Caption = 'How would you rate the quality of this demo?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Poor'
  end
  object Label3: TLabel
    Left = 252
    Top = 48
    Width = 43
    Height = 13
    Caption = 'Excellent'
  end
  object Label4: TLabel
    Left = 8
    Top = 80
    Width = 263
    Height = 13
    Caption = 'Tell us your opinion about this demo (optional)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object memRateDescrip: TMemo
    Left = 8
    Top = 96
    Width = 289
    Height = 121
    Color = 15921902
    TabOrder = 1
  end
  object btnSend: TButton
    Left = 224
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Email...'
    Enabled = False
    TabOrder = 2
    OnClick = btnSendClick
  end
  object rgRate: TRadioGroup
    Left = 44
    Top = 32
    Width = 201
    Height = 38
    Columns = 5
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5')
    TabOrder = 0
    OnClick = rgRateChange
  end
end
