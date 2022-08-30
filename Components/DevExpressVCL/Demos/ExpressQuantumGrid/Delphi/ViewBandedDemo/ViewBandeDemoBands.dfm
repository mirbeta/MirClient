object ViewBandeDemoBandsForm: TViewBandeDemoBandsForm
  Left = 250
  Top = 166
  ActiveControl = lbBands
  BorderStyle = bsDialog
  Caption = 'Remove Bands'
  ClientHeight = 282
  ClientWidth = 284
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
  object lbBands: TcxListBox
    Left = 8
    Top = 8
    Width = 265
    Height = 233
    ItemHeight = 13
    MultiSelect = True
    Style.Color = 16247513
    TabOrder = 0
  end
  object btnOK: TcxButton
    Left = 118
    Top = 248
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TcxButton
    Left = 198
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
