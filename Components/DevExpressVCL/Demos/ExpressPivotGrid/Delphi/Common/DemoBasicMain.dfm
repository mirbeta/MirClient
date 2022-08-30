object frmDemoBasicMain: TfrmDemoBasicMain
  Left = 162
  Top = 128
  Caption = 'Express PivotGrid DemoBasic form'
  ClientHeight = 544
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescrip: TLabel
    Left = 0
    Top = 0
    Width = 807
    Height = 32
    Align = alTop
    Caption = 'Brief purpose of the demo'#10#13'That'#39's it'
    Color = 4707838
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object mmMain: TMainMenu
    Left = 624
    Top = 24
    object miFile: TMenuItem
      Caption = '&File'
      object miExport: TMenuItem
        Caption = 'Export'
        object miExportToXLSX: TMenuItem
          Tag = 5
          Caption = 'Excel'
          object miDataOnly1: TMenuItem
            Tag = 51
            Caption = 'Data Only'
            OnClick = miExportToClick
          end
          object miWYSIWYG1: TMenuItem
            Tag = 52
            Caption = 'WYSIWYG'
            OnClick = miExportToClick
          end
        end
        object miExportToExcel: TMenuItem
          Tag = 1
          Caption = 'Excel 97-2003'
          object miDataOnly2: TMenuItem
            Tag = 11
            Caption = 'Data Only'
            OnClick = miExportToClick
          end
          object miWYSIWYG2: TMenuItem
            Tag = 12
            Caption = 'WYSIWYG'
            OnClick = miExportToClick
          end
        end
        object miExportToHtml: TMenuItem
          Tag = 3
          Caption = 'HTML'
          OnClick = miExportToClick
        end
        object miExportToText: TMenuItem
          Tag = 2
          Caption = 'Text'
          OnClick = miExportToClick
        end
        object miExportToXml: TMenuItem
          Tag = 4
          Caption = 'XML'
          OnClick = miExportToClick
        end
      end
      object Separator1: TMenuItem
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
      object miTotalsPosition: TMenuItem
        Caption = 'Totals Location'
        object miColumnTotalsPosition: TMenuItem
          Caption = 'Column'
          object miColumnTotalsPositionFar: TMenuItem
            AutoCheck = True
            Caption = 'Far'
            Checked = True
            GroupIndex = 4
            RadioItem = True
            OnClick = miTotalsLocationClick
          end
          object miColumnTotalsPositionNear: TMenuItem
            Tag = 1
            AutoCheck = True
            Caption = 'Near'
            GroupIndex = 4
            RadioItem = True
            OnClick = miTotalsLocationClick
          end
        end
        object miRowTotalsPosition: TMenuItem
          Caption = 'Row'
          object miRowTotalsPositionFar: TMenuItem
            Tag = 2
            AutoCheck = True
            Caption = 'Far'
            Checked = True
            GroupIndex = 5
            RadioItem = True
            OnClick = miTotalsLocationClick
          end
          object miRowTotalsPositionNear: TMenuItem
            Tag = 3
            AutoCheck = True
            Caption = 'Near'
            GroupIndex = 5
            RadioItem = True
            OnClick = miTotalsLocationClick
          end
          object miRowTotalsPositionTree: TMenuItem
            Tag = 4
            AutoCheck = True
            Caption = 'Tree'
            GroupIndex = 5
            RadioItem = True
            OnClick = miTotalsLocationClick
          end
        end
      end
      object miTotalsVisibility: TMenuItem
        Caption = 'Totals Visibility'
        object miShowColumnTotals: TMenuItem
          AutoCheck = True
          Caption = 'ShowColumnTotals'
          OnClick = miTotalsVisibilityClick
        end
        object miShowRowTotals: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'ShowRowTotals'
          OnClick = miTotalsVisibilityClick
        end
        object N1: TMenuItem
          Caption = '-'
        end
        object miShowColumnGrandTotals: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'ShowColumnGrandTotals'
          OnClick = miTotalsVisibilityClick
        end
        object miShowRowGrandTotals: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'ShowRowGrandTotals'
          OnClick = miTotalsVisibilityClick
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object miShowTotalsForSingleValues: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'ShowTotalsForSingleValues'
          OnClick = miTotalsVisibilityClick
        end
        object miShowGrandTotalsForSingleValues: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = 'ShowGrandTotalsForSingleValues'
          OnClick = miTotalsVisibilityClick
        end
      end
      object miElementsVisibility: TMenuItem
        Caption = 'Elements Visibility'
        object miShowColumnFields: TMenuItem
          AutoCheck = True
          Caption = 'ShowColumnFields'
          OnClick = miElementsVisibilityClick
        end
        object miShowDataFields: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'ShowDataFields'
          OnClick = miElementsVisibilityClick
        end
        object miShowFilterFields: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'ShowFilterFields'
          OnClick = miElementsVisibilityClick
        end
        object miShowFilterSeparator: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'ShowFilterSeparator'
          OnClick = miElementsVisibilityClick
        end
        object miShowRowFields: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'ShowRowFields'
          OnClick = miElementsVisibilityClick
        end
      end
      object miSelection: TMenuItem
        Caption = 'Selection'
        object miIncludeCells: TMenuItem
          Caption = 'Include Cells'
          object miCrossCells: TMenuItem
            AutoCheck = True
            Caption = 'Cross Cells'
            Checked = True
            OnClick = IncludeCellsClick
          end
          object miGrandTotalsCells: TMenuItem
            Tag = 1
            AutoCheck = True
            Caption = 'Grand Totals Cells'
            Checked = True
            OnClick = IncludeCellsClick
          end
          object miTotalsCells: TMenuItem
            Tag = 2
            AutoCheck = True
            Caption = 'Totals Cells'
            Checked = True
            OnClick = IncludeCellsClick
          end
        end
        object miMultiSelect: TMenuItem
          AutoCheck = True
          Caption = 'Multi select'
          OnClick = miMultiSelectClick
        end
        object miHideFocusRect: TMenuItem
          AutoCheck = True
          Caption = 'Hide Focus Rect when Inactive'
          OnClick = miHideFocusRectClick
        end
        object miHideSelection: TMenuItem
          AutoCheck = True
          Caption = 'Hide Selection when Inactive'
          OnClick = miHideSelectionClick
        end
      end
      object miCopyToClipboard: TMenuItem
        Caption = 'Copy To Clipboard'
        object miCopyToClipboardIncludeHeaders: TMenuItem
          AutoCheck = True
          Caption = 'Include Headers'
          Checked = True
          OnClick = miCopyToClipboardIncludeHeadersClick
        end
        object miCopyToClipboardColumnHeaders: TMenuItem
          Caption = 'Column Headers'
          object miCopyToClipboardColumnHeadersAll: TMenuItem
            Caption = 'All'
            Checked = True
            GroupIndex = 12
            RadioItem = True
          end
          object InnermostOnly1: TMenuItem
            Caption = 'Innermost Only'
            GroupIndex = 12
            RadioItem = True
          end
        end
        object miCopyToClipboardRowHeaders: TMenuItem
          Caption = 'Row Headers'
          object miCopyToClipboardRowHeadersAll: TMenuItem
            Caption = 'All'
            Checked = True
            GroupIndex = 11
            RadioItem = True
          end
          object miAllCopyToClipboardRowInnermostOnly: TMenuItem
            Caption = 'Innermost Only'
            GroupIndex = 11
            RadioItem = True
          end
        end
      end
      object miTouchMode: TMenuItem
        AutoCheck = True
        Caption = 'Touch Mode'
        OnClick = miTouchModeClick
      end
    end
    object miAbout: TMenuItem
      Caption = '&About this demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
  object SaveDialog: TSaveDialog
    Left = 680
    Top = 8
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 488
    Top = 200
  end
  object ActionList1: TActionList
    Left = 576
    object Action1: TAction
      Caption = 'Action1'
      ShortCut = 16451
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = 'Action2'
      ShortCut = 16429
      OnExecute = Action1Execute
    end
  end
end
