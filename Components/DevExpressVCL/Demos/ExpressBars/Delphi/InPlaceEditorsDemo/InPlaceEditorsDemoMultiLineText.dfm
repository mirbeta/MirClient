inherited frmMultiLineTextEditors: TfrmMultiLineTextEditors
  Tag = 1
  Left = 461
  Top = 230
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frmMultiLineTextEditors'
  ClientHeight = 395
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Height = 395
    AutoSnap = False
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 289
    Height = 395
    Align = alLeft
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 292
    Top = 0
    Width = 309
    Height = 395
    Align = alClient
    TabOrder = 1
  end
end
