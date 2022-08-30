object SelectStorage: TSelectStorage
  Left = 307
  Top = 203
  BorderStyle = bsDialog
  Caption = 'Select storage'
  ClientHeight = 115
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 6
    Top = 8
    Width = 180
    Height = 65
    Caption = 'Save to'
    TabOrder = 0
    object rbDBStorage: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'DB Storage'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbUnboundStorage: TRadioButton
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Unbound storage'
      TabOrder = 1
    end
  end
  object Button1: TButton
    Left = 24
    Top = 85
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 111
    Top = 85
    Width = 75
    Height = 23
    Caption = 'Default'
    ModalResult = 2
    TabOrder = 2
  end
end
