object IssueListStylesForm: TIssueListStylesForm
  Left = 381
  Top = 255
  BorderStyle = bsDialog
  Caption = 'Predefined stylesheets'
  ClientHeight = 353
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbPredefinedStyleSheets: TcxListBox
    Left = 8
    Top = 8
    Width = 193
    Height = 338
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbPredefinedStyleSheetsClick
  end
  object btnEdit: TcxButton
    Left = 207
    Top = 37
    Width = 81
    Height = 23
    Caption = 'Edit...'
    TabOrder = 1
    OnClick = btnEditClick
  end
  object cxButton1: TcxButton
    Left = 207
    Top = 66
    Width = 81
    Height = 23
    Caption = 'Load...'
    TabOrder = 2
    OnClick = cxButton1Click
  end
  object cxButton2: TcxButton
    Left = 207
    Top = 95
    Width = 81
    Height = 23
    Caption = 'Save...'
    TabOrder = 3
    OnClick = cxButton2Click
  end
  object btnClear: TcxButton
    Left = 207
    Top = 8
    Width = 81
    Height = 23
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btnClearClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 24
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 56
    Top = 16
  end
end
