object AdvPreviewForm: TAdvPreviewForm
  Left = 308
  Top = 185
  Caption = 'Preview'
  ClientHeight = 375
  ClientWidth = 413
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 413
    Height = 41
    Align = alTop
    TabOrder = 0
    object Next: TButton
      Left = 88
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Next'
      TabOrder = 0
      OnClick = NextClick
    end
    object Previous: TButton
      Left = 8
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Previous'
      TabOrder = 1
      OnClick = PreviousClick
    end
    object PrintBtn: TButton
      Left = 168
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Print'
      Default = True
      TabOrder = 2
      OnClick = PrintBtnClick
    end
    object CloseBtn: TButton
      Left = 248
      Top = 8
      Width = 73
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 3
      OnClick = CloseBtnClick
    end
  end
  object Panel2: TPanel
    Left = 16
    Top = 48
    Width = 393
    Height = 329
    Color = clWhite
    TabOrder = 1
    object PreviewPaintBox: TPaintBox
      Left = 1
      Top = 1
      Width = 391
      Height = 327
      Align = alClient
      OnPaint = PreviewPaintBoxPaint
    end
  end
end
