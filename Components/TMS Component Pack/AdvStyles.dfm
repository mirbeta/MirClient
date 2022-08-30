object AdvStyleForm: TAdvStyleForm
  Left = 133
  Top = 267
  BorderStyle = bsDialog
  Caption = 'Styles'
  ClientHeight = 366
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 18
    Top = 8
    Width = 161
    Height = 305
    ItemIndex = 0
    Items.Strings = (
      'Office 2003 (Blue)'
      'Office 2003 (Olive)'
      'Office 2003 (Silver)'
      'Office 2003 (Classic)'
      'Office 2007 (Luna)'
      'Office 2007 (Obsidian)'
      'Office 2007 (Silver)'
      'Office 2010 (Blue)'
      'Office 2010 (Silver)'
      'Office 2010 (Black)'
      'Windows XP'
      'Windows Vista'
      'Windows 7'
      'Terminal')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 18
    Top = 330
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 102
    Top = 330
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
