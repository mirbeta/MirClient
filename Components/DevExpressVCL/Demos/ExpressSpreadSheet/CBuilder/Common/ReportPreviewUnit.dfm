object frmPreview: TfrmPreview
  Left = 0
  Top = 0
  Caption = 'Report Preview'
  ClientHeight = 650
  ClientWidth = 989
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ssResult: TdxSpreadSheet
    Left = 0
    Top = 0
    Width = 989
    Height = 650
    Align = alClient
    Data = {
      D501000044585353763242460900000042465320000000000000000001000101
      010100000000000001004246532000000000424653200200000001000000200B
      00000007000000430061006C0069006200720069000000000000200000002000
      000000200000000020000000002000000000200007000000470045004E004500
      520041004C0000000000000200000000000000000101000000200B0000000700
      0000430061006C00690062007200690000000000002000000020000000002000
      00000020000000002000000000200007000000470045004E004500520041004C
      0000000000000200000000000000000142465320010000004246532017000000
      5400640078005300700072006500610064005300680065006500740054006100
      62006C00650056006900650077000600000053006800650065007400310001FF
      FFFFFFFFFFFFFF64000000020000000200000002000000550000001400000002
      0000000200000000020000000000000100000000000101000042465320550000
      0000000000424653200000000042465320140000000000000042465320000000
      0000000000000000000100000000000000000000000000000000000000424653
      20000000000000000000000000424653200000000000000000}
  end
  object MainMenu1: TMainMenu
    Left = 448
    Top = 288
    object File1: TMenuItem
      Caption = 'File'
      object miSaveAs: TMenuItem
        Caption = 'Save As'
        OnClick = miSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object miShowFormulas: TMenuItem
        AutoCheck = True
        Caption = 'Show Formulas'
        OnClick = miShowFormulasClick
      end
    end
  end
  object sveDialog: TSaveDialog
    Left = 520
    Top = 288
  end
end
