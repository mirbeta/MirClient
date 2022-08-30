object DemoRatingForm: TDemoRatingForm
  Left = 316
  Top = 224
  Width = 323
  Height = 292
  Caption = 'The Demo rating '
  Color = 15451300
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
    Left = 248
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
  object rgRate: TcxRadioGroup
    Left = 48
    Top = 36
    Width = 193
    Height = 32
    Properties.Columns = 5
    Properties.Items = <
      item
        Caption = '1'
      end
      item
        Caption = '2'
      end
      item
        Caption = '3'
      end
      item
        Caption = '4'
      end
      item
        Caption = '5'
      end>
    Properties.OnChange = rgRatePropertiesChange
    Style.BorderStyle = ebsNone
    Style.Edges = []
    Style.LookAndFeel.NativeStyle = True
    TabOrder = 0
    Caption = ''
  end
  object memRateDescrip: TcxMemo
    Left = 8
    Top = 96
    Width = 297
    Height = 121
    Properties.ScrollBars = ssVertical
    Style.Color = 16247513
    Style.LookAndFeel.NativeStyle = True
    TabOrder = 1
  end
  object btnSend: TcxButton
    Left = 232
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Send'
    Enabled = False
    ModalResult = 1
    TabOrder = 2
    OnClick = btnSendClick
    LookAndFeel.NativeStyle = True
  end
end
