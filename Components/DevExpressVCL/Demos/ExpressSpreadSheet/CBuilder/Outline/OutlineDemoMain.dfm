inherited frmOutline: TfrmOutline
  Left = 0
  Top = 0
  Caption = 'DX SpreadSheet - Outline Demo'
  ClientHeight = 406
  ClientWidth = 845
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbMain: TStatusBar
    Top = 387
    Width = 845
  end
  object SpreadSheet: TdxSpreadSheet [1]
    AlignWithMargins = True
    Left = 3
    Top = 37
    Width = 839
    Height = 347
    Align = alClient
    Data = {
      A402000044585353763242460C00000042465320000000000000000001000101
      010100000000000001004246532000000000424653200200000001000000200B
      00000007000000430061006C0069006200720069000000000000002000000020
      00000000200000000020000000002000000000200007000000470045004E0045
      00520041004C0000000000000200000000000000000101000000200B00000007
      000000430061006C006900620072006900000000000000200000002000000000
      200000000020000000002000000000200007000000470045004E004500520041
      004C000000000000020000000000000000014246532001000000424653201700
      0000540064007800530070007200650061006400530068006500650074005400
      610062006C006500560069006500770006000000530068006500650074003100
      01FFFFFFFFFFFFFFFF6400000002000000020000000200000055000000140000
      0002000000020000000002000000000000010000000000010100004246532055
      0000000000000042465320000000004246532014000000000000004246532000
      0000000000000000000000010000000000000000000000000000000000000042
      4653200000000002020000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000064000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000020002020002000000
      0000000000000000000000000000020000000000000000000000000000000000
      0000000000000000000000000000000000000202000000000000000042465320
      0000000000000000}
  end
  object FormulaBar: TdxSpreadSheetFormulaBar [2]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 839
    Height = 20
    Align = alTop
    SpreadSheet = SpreadSheet
    TabOrder = 2
  end
  object FormulaBarSplitter: TcxSplitter [3]
    Left = 0
    Top = 26
    Width = 845
    Height = 8
    AlignSplitter = salTop
    Control = FormulaBar
  end
  inherited mmMain: TMainMenu
    object miGrouping: TMenuItem [1]
      Caption = 'Grouping'
      object miGroupColumns: TMenuItem
        Caption = 'Group Columns'
        OnClick = miGroupColumnsClick
      end
      object miGroupRows: TMenuItem
        Caption = 'Group Rows'
        OnClick = miGroupRowsClick
      end
      object miLine2: TMenuItem
        Caption = '-'
      end
      object miUngroupColumns: TMenuItem
        Caption = 'Ungroup Columns'
        OnClick = miUngroupColumnsClick
      end
      object miUngroupRows: TMenuItem
        Caption = 'Ungroup Rows'
        OnClick = miUngroupRowsClick
      end
    end
    inherited miOptions: TMenuItem
      object GroupExpandButtonPosition1: TMenuItem [0]
        Caption = 'Expand Button Position'
        object miGroupStart: TMenuItem
          AutoCheck = True
          Caption = 'Group Start'
          RadioItem = True
          OnClick = miGroupFinishClick
        end
        object miGroupFinish: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Group Finish'
          RadioItem = True
          OnClick = miGroupFinishClick
        end
      end
      object N2: TMenuItem [1]
        Caption = '-'
      end
    end
  end
end
