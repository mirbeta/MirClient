object frmPostionSize: TfrmPostionSize
  Left = 323
  Top = 251
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #25511#20214#22823#23567#20462#27491
  ClientHeight = 180
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 147
    Width = 355
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 192
      Top = 1
      Width = 75
      Height = 25
      Caption = #30830#23450'(&O)'
      Default = True
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 272
      Top = 1
      Width = 75
      Height = 25
      Caption = #21462#28040'(&C)'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 355
    Height = 147
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object rgHeight: TRadioGroup
      Left = 184
      Top = 8
      Width = 169
      Height = 129
      Caption = #39640
      ItemIndex = 0
      Items.Strings = (
        #26080#21464#21270
        #20462#27491#21040#26368#23567#39640#24230
        #20462#27491#21040#26368#22823#39640#24230
        #20462#27491#21040#25351#23450#39640#24230)
      TabOrder = 0
    end
    object rgWidth: TRadioGroup
      Left = 8
      Top = 8
      Width = 169
      Height = 129
      Caption = #23485
      ItemIndex = 0
      Items.Strings = (
        #26080#21464#21270
        #20462#27491#21040#26368#23567#23485#24230
        #20462#27491#21040#26368#22823#23485#24230
        #20462#27491#21040#25351#23450#23485#24230)
      TabOrder = 1
    end
    object Edit1: TEdit
      Left = 120
      Top = 108
      Width = 49
      Height = 20
      NumbersOnly = True
      TabOrder = 2
      Text = '0'
      OnKeyPress = Edit1KeyPress
    end
    object Edit2: TEdit
      Left = 296
      Top = 108
      Width = 49
      Height = 20
      NumbersOnly = True
      TabOrder = 3
      Text = '0'
      OnKeyPress = Edit1KeyPress
    end
  end
end
