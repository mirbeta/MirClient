inherited frmComboBoxes: TfrmComboBoxes
  Tag = 4
  Left = 354
  Top = 168
  BorderIcons = []
  Caption = 'frmComboBoxes'
  ClientHeight = 368
  ClientWidth = 619
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblFrameDescription: TLabel
    Width = 619
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 16
    Width = 619
    Height = 352
    Align = alClient
    OnPaint = PaintBox1Paint
  end
end
