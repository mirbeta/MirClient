object WorldWeatherChangeVisibilityDialogForm: TWorldWeatherChangeVisibilityDialogForm
  Left = 0
  Top = 0
  ActiveControl = cxCheckListBox1
  BorderStyle = bsDialog
  Caption = 'Show/Hide Cities'
  ClientHeight = 306
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxCheckListBox1: TcxCheckListBox
    Left = 5
    Top = 8
    Width = 254
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    Items = <>
    Sorted = True
    TabOrder = 0
  end
  object cxButton1: TcxButton
    Left = 103
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cxButton2: TcxButton
    Left = 184
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
