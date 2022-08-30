object PlanStyleForm: TPlanStyleForm
  Left = 133
  Top = 267
  BorderStyle = bsDialog
  Caption = 'Styles'
  ClientHeight = 314
  ClientWidth = 376
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
  object Button1: TButton
    Left = 109
    Top = 279
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 195
    Top = 279
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 13
    Top = 8
    Width = 355
    Height = 265
    Columns = 2
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
      'Terminal'
      'Windows8'
      'Office 2013 (White)'
      'Office 2013 (Light Gray)'
      'Office 2013 (Gray)'
      'Windows 10'
      'Office 2016 (White)'
      'Office 2016 (Gray)'
      'Office 2016 (Black)')
    TabOrder = 2
  end
end
