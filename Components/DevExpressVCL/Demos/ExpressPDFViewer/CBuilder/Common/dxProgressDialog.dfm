object frmProgress: TfrmProgress
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Exporting...'
  ClientHeight = 63
  ClientWidth = 441
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
  object pbProgress: TcxProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 23
    Align = alTop
    Properties.PeakValue = 100.000000000000000000
    TabOrder = 0
    Width = 435
  end
  object lbTitle: TcxLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Please wait while the document is exported...'
    Transparent = True
  end
end
