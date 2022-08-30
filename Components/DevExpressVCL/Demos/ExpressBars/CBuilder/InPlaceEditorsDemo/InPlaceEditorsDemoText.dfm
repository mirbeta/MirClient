inherited frmTextEditors: TfrmTextEditors
  Tag = 2
  Left = 380
  Top = 190
  BorderIcons = []
  Caption = 'frmTextEditors'
  ClientHeight = 398
  ClientWidth = 642
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblFrameDescription: TLabel
    Width = 642
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 16
    Width = 642
    Height = 382
    Align = alClient
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnPaint = PaintBox1Paint
  end
end
