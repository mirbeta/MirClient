object WorldWeatherDemoAddCityDialogForm: TWorldWeatherDemoAddCityDialogForm
  Left = 0
  Top = 0
  ActiveControl = cxTextEdit1
  BorderStyle = bsDialog
  Caption = 'Add City'
  ClientHeight = 65
  ClientWidth = 288
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
  object cxTextEdit1: TcxTextEdit
    Left = 8
    Top = 8
    TabOrder = 0
    Width = 273
  end
  object cxButton1: TcxButton
    Left = 125
    Top = 35
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cxButton2: TcxButton
    Left = 206
    Top = 35
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
