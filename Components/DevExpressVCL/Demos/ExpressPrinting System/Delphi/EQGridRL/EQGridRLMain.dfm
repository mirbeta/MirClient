inherited EQGridRLMainForm: TEQGridRLMainForm
  Left = 620
  Top = 180
  Caption = 'Report Link Demo - ExpressQuantumGrid'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This example demonstates the ExpressQuantumGrid printing capabil' +
      'ities.'
  end
  inherited ToolBar1: TToolBar
    object tbtnFullCollapse: TToolButton
      Left = 123
      Top = 0
      Action = actFullCollapse
      ParentShowHint = False
      ShowHint = True
    end
    object tbtnFullExpand: TToolButton
      Left = 146
      Top = 0
      Action = actFullExpand
      ParentShowHint = False
      ShowHint = True
    end
  end
  object cxGrid: TcxGrid [3]
    Left = 0
    Top = 41
    Width = 789
    Height = 428
    Align = alClient
    TabOrder = 2
    object tvPlanets: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.IncSearch = True
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsSelection.MultiSelect = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.HeaderAutoHeight = True
      Styles.StyleSheet = tvssDevExpress
      object tvPlanetsNAME: TcxGridColumn
        Caption = 'Name'
        HeaderAlignmentHorz = taCenter
        Width = 100
      end
      object tvPlanetsNO: TcxGridColumn
        Caption = '#'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
        Width = 40
      end
      object tvPlanetsORBITS: TcxGridColumn
        Caption = 'Orbits'
        RepositoryItem = edrepCenterText
        GroupIndex = 0
        HeaderAlignmentHorz = taCenter
        SortIndex = 0
        SortOrder = soAscending
      end
      object tvPlanetsDISTANCE: TcxGridColumn
        Caption = 'Distance (000km)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
        SortIndex = 1
        SortOrder = soAscending
        Width = 80
      end
      object tvPlanetsPERIOD: TcxGridColumn
        Caption = 'Period (days)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
        Width = 80
      end
      object tvPlanetsDISCOVERER: TcxGridColumn
        Caption = 'Discoverer'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
      end
      object tvPlanetsDATE: TcxGridColumn
        Caption = 'Date'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
      end
      object tvPlanetsRADIUS: TcxGridColumn
        Caption = 'Radius (km)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
      end
    end
    object lvPlanets: TcxGridLevel
      GridView = tvPlanets
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miFullCollapsing: TMenuItem [0]
        Action = actFullCollapse
      end
      object miFullExpand: TMenuItem [1]
        Action = actFullExpand
      end
      object N3: TMenuItem [2]
        Caption = '-'
      end
    end
  end
  inherited sty: TActionList
    object actFullExpand: TAction
      Category = 'Options'
      Caption = 'Full &Expand'
      Hint = 'Full expand'
      ImageIndex = 8
      OnExecute = actFullExpandExecute
    end
    object actFullCollapse: TAction
      Category = 'Options'
      Caption = 'Full &Collapse'
      Hint = 'Full collapse'
      ImageIndex = 7
      OnExecute = actFullCollapseExecute
    end
  end
  inherited dxComponentPrinter: TdxComponentPrinter
    CurrentLink = dxComponentPrinterLink1
    object dxComponentPrinterLink1: TdxGridReportLink
      Component = cxGrid
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      BuiltInReportLink = True
    end
  end
  inherited dxPSEngineController1: TdxPSEngineController
    Active = True
  end
  inherited ilMain: TcxImageList
    FormatVersion = 1
  end
  object edrepMain: TcxEditRepository
    Left = 152
    Top = 99
    object edrepCenterText: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taCenter
    end
    object edrepRightText: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taRightJustify
    end
  end
  object StyleRepository: TcxStyleRepository
    Left = 120
    Top = 99
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = 14872561
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
    end
    object cxStyle8: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle9: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle11: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle12: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16777088
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlue
    end
    object cxStyle14: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object tvssDevExpress: TcxGridTableViewStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle2
      Styles.ContentEven = cxStyle3
      Styles.ContentOdd = cxStyle4
      Styles.FilterBox = cxStyle5
      Styles.Inactive = cxStyle10
      Styles.IncSearch = cxStyle11
      Styles.Selection = cxStyle14
      Styles.Footer = cxStyle6
      Styles.Group = cxStyle7
      Styles.GroupByBox = cxStyle8
      Styles.Header = cxStyle9
      Styles.Indicator = cxStyle12
      Styles.Preview = cxStyle13
      BuiltIn = True
    end
  end
end
