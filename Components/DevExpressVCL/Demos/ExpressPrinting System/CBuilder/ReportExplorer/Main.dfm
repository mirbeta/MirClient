object fmLauncher: TfmLauncher
  Left = 1145
  Top = 772
  Caption = 'fmLauncher'
  ClientHeight = 95
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object dxPSFileBasedExplorer1: TdxPSFileBasedExplorer
    OnItemDataLoadError = dxPSFileBasedExplorer1ItemDataLoadError
    Options = [eoLoadAll, eoShowIOErrors]
    Left = 220
    Top = 13
  end
  object dxComponentPrinter1: TdxComponentPrinter
    Explorer = dxPSFileBasedExplorer1
    PreviewOptions.PreviewBoundsRect = {0000000000000000B603000058020000}
    Version = 0
    Left = 41
    Top = 12
  end
  object dxPSEngineController1: TdxPSEngineController
    Active = True
    Left = 131
    Top = 64
  end
end
