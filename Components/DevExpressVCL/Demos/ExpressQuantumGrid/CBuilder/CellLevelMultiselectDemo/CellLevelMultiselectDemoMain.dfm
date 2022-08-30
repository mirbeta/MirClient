inherited CellLevelMultiselectDemoMainForm: TCellLevelMultiselectDemoMainForm
  Left = 107
  Top = 75
  Caption = 'ExpressQuantumGrid CellLevelMultiselect Demo'
  ClientHeight = 465
  ClientWidth = 714
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 714
    Caption = 
      'Experiment with cell selection. Click '#39'About this demo'#39' for more' +
      ' information.'
  end
  inherited sbMain: TStatusBar
    Top = 405
    Width = 714
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 424
    Width = 714
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = 16247513
    TabOrder = 1
    object Label1: TcxLabel
      Left = 4
      Top = 4
      AutoSize = False
      Caption = 'Selected rows:'
      Transparent = True
      Height = 17
      Width = 74
    end
    object Label2: TcxLabel
      Left = 4
      Top = 20
      AutoSize = False
      Caption = 'Selected columns:'
      Transparent = True
      Height = 17
      Width = 91
    end
    object Label3: TcxLabel
      Left = 160
      Top = 4
      AutoSize = False
      Caption = 'Selected Cells:'
      Transparent = True
      Height = 17
      Width = 74
    end
    object Label4: TcxLabel
      Left = 160
      Top = 20
      AutoSize = False
      Caption = 'Selected Summary:'
      Transparent = True
      Height = 17
      Width = 95
    end
    object lblSelectedRows: TcxLabel
      Left = 96
      Top = 4
      AutoSize = False
      Caption = 'lblSelectedRows'
      Transparent = True
      Height = 17
      Width = 58
    end
    object lblSelectedColumns: TcxLabel
      Left = 96
      Top = 20
      AutoSize = False
      Caption = 'lblSelectedColumns'
      Transparent = True
      Height = 17
      Width = 58
    end
    object lblSelectedCells: TcxLabel
      Left = 259
      Top = 4
      AutoSize = False
      Caption = 'lblSelectedCells'
      Transparent = True
      Height = 17
      Width = 78
    end
    object lblSelectedSummary: TcxLabel
      Left = 258
      Top = 20
      AutoSize = False
      Caption = 'lblSelectedSummary'
      Transparent = True
      Height = 17
      Width = 79
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 16
    Width = 714
    Height = 389
    Align = alClient
    TabOrder = 2
    object TableView: TcxGridTableView
      OnMouseDown = TableViewMouseDown
      OnSelectionChanged = TableViewSelectionChanged
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnMoving = False
      OptionsCustomize.ColumnSorting = False
      OptionsSelection.InvertSelect = False
      OptionsSelection.MultiSelect = True
      OptionsSelection.CellMultiSelect = True
      OptionsView.GroupByBox = False
      OptionsView.Indicator = True
      OptionsView.IndicatorWidth = 40
      Styles.OnGetHeaderStyle = TableViewStylesGetHeaderStyle
      OnCustomDrawIndicatorCell = TableViewCustomDrawIndicatorCell
    end
    object Level: TcxGridLevel
      GridView = TableView
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 336
    Top = 88
    PixelsPerInch = 96
    object styleSelected: TcxStyle [24]
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clRed
    end
    object styleNormal: TcxStyle [25]
      AssignedValues = [svColor]
      Color = clBtnFace
    end
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
end
