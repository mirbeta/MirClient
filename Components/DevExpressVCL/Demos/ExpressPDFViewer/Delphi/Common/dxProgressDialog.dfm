object frmProgress: TfrmProgress
  Left = 408
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Exporting...'
  ClientHeight = 65
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Padding.Left = 3
  Padding.Top = 3
  Padding.Right = 3
  Padding.Bottom = 3
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pbProgress: TcxProgressBar
    AlignWithMargins = True
    Left = 6
    Top = 26
    Align = alTop
    Properties.PeakValue = 100.000000000000000000
    TabOrder = 0
    Width = 429
  end
  object lbTitle: TcxLabel
    AlignWithMargins = True
    Left = 6
    Top = 6
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Please wait while the document is exported...'
    Transparent = True
  end
end
