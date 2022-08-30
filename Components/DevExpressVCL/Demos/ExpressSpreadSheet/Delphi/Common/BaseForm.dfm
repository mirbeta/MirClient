object fmBaseForm: TfmBaseForm
  Left = 420
  Top = 223
  Caption = 'fmBaseForm'
  ClientHeight = 376
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object sbMain: TStatusBar
    Left = 0
    Top = 357
    Width = 623
    Height = 19
    AutoHint = True
    Panels = <>
    ParentShowHint = False
    ShowHint = True
  end
  object mmMain: TMainMenu
    Left = 136
    Top = 24
    object miFile: TMenuItem
      Caption = '&File'
      object miSaveAs: TMenuItem
        Caption = 'Save As'
        OnClick = miSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'E&xit'
        Hint = 'Press to quit the demo-program'
        ShortCut = 32856
        OnClick = miExitClick
      end
    end
    object miOptions: TMenuItem
      Caption = 'Options'
      object miShowFormulas: TMenuItem
        AutoCheck = True
        Caption = 'Show Formulas'
        OnClick = miShowFormulasClick
      end
    end
    object miAbout: TMenuItem
      Caption = '&About this demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 64
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xlsx'
    Filter = 'MS Excel 2007-2013|*.xlsx|MS Excel 97-2003|*.xls'
    Options = [ofOverwritePrompt]
    Left = 304
    Top = 200
  end
end
