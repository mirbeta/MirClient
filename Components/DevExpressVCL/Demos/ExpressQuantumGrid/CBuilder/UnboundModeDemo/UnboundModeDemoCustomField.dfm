object UnboundModeDemoCustomFieldForm: TUnboundModeDemoCustomFieldForm
  Left = 290
  Top = 153
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsSingle
  Caption = 'Custom Field'
  ClientHeight = 161
  ClientWidth = 253
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbHeight: TLabel
    Left = 24
    Top = 37
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object lbWidth: TLabel
    Left = 24
    Top = 69
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object lbMineCount: TLabel
    Left = 26
    Top = 102
    Width = 28
    Height = 13
    Caption = 'Mines'
  end
  object edtHeight: TEdit
    Left = 64
    Top = 32
    Width = 41
    Height = 21
    Color = 16247513
    MaxLength = 5
    TabOrder = 0
    OnKeyPress = edtKeyPress
  end
  object edtWidth: TEdit
    Left = 64
    Top = 64
    Width = 41
    Height = 21
    Color = 16247513
    MaxLength = 5
    TabOrder = 1
    OnKeyPress = edtKeyPress
  end
  object edtMineCount: TEdit
    Left = 64
    Top = 96
    Width = 41
    Height = 21
    Color = 16247513
    MaxLength = 5
    TabOrder = 2
    OnKeyPress = edtKeyPress
  end
  object btnOK: TcxButton
    Left = 144
    Top = 32
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object bntCancel: TcxButton
    Left = 144
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    Cancel = True
  end
end
