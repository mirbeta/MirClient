inherited frmResearch: TfrmResearch
  Width = 1076
  Height = 537
  Font.Name = 'Segoe UI'
  ParentFont = False
  object pgResearch: TcxDBPivotGrid
    Left = 0
    Top = 0
    Width = 685
    Height = 537
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DMRealtorWorld.dsHouseSales
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Groups = <>
    OptionsDataField.Area = dfaColumn
    OptionsView.FilterFields = False
    OptionsView.TotalsForSingleValues = True
    ParentFont = False
    PopupMenus.FieldHeaderMenu.UseBuiltInMenu = False
    PopupMenus.GroupValueMenu.UseBuiltInMenu = False
    PopupMenus.HeaderAreaMenu.UseBuiltInMenu = False
    TabOrder = 0
    object pgfYear: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Year'
      DataBinding.FieldName = 'Date1'
      Options.Filtering = False
      Options.Moving = False
      GroupInterval = giDateYear
      SortOrder = soDescending
      Visible = True
      UniqueName = 'Year'
    end
    object pgfCount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Count'
      DataBinding.FieldName = 'Count1'
      Options.Filtering = False
      Options.Moving = False
      Visible = True
      UniqueName = 'Count'
    end
    object pgfRegion: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 2
      IsCaptionAssigned = True
      Caption = 'Region'
      DataBinding.FieldName = 'Region'
      Options.Filtering = False
      Options.Moving = False
      Visible = True
      Width = 62
      UniqueName = 'Region'
    end
    object pgfSeasonallyAdjusted: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Seasonally Adjusted'
      DataBinding.FieldName = 'SeasonallyAdjusted'
      Options.Filtering = False
      Options.Moving = False
      Visible = True
      UniqueName = 'Seasonally Adjusted'
    end
    object pgfStatus: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Status'
      DataBinding.FieldName = 'Type1'
      Options.Filtering = False
      Options.Moving = False
      SortOrder = soDescending
      Visible = True
      UniqueName = 'Status'
    end
    object pgfMonth: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Month'
      DataBinding.FieldName = 'Date1'
      Options.Filtering = False
      Options.Moving = False
      GroupInterval = giDateMonth
      Visible = True
      UniqueName = 'Month'
    end
  end
  object cxSplitter1: TcxSplitter
    Left = 685
    Top = 0
    Width = 4
    Height = 537
    InvertDirection = True
    Control = pgResearch
    OnBeforeClose = cxSplitter1BeforeClose
  end
  object cxGroupBox1: TcxGroupBox
    Left = 689
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    TabOrder = 2
    Height = 537
    Width = 387
    object cxgChart: TcxGrid
      Left = 2
      Top = 64
      Width = 383
      Height = 399
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = [beTop, beBottom]
      TabOrder = 0
      object cvChart: TcxGridDBChartView
        Categories.DataBinding.FieldName = 'Date'
        Categories.DisplayText = 'Price'
        DataController.DataSource = DMRealtorWorld.dsResearchChart
        DiagramLine.Active = True
        DiagramLine.AxisCategory.GridLines = False
        DiagramLine.AxisCategory.TickMarkKind = tmkNone
        DiagramLine.AxisCategory.TickMarkLabels = False
        DiagramLine.AxisCategory.Title.Text = ' '
        DiagramLine.EmptyPointsDisplayMode = epdmGap
        DiagramLine.Values.VaryColorsByCategory = True
        DiagramLine.Values.LineWidth = 2
        Legend.Border = lbSingle
        Legend.Orientation = cpoVertical
        Legend.Position = cppBottom
        OptionsCustomize.DataDrillUpMethod = ddumNone
        OptionsCustomize.DataGroupMoving = False
        OptionsCustomize.OptionsCustomization = False
        OptionsCustomize.SeriesCustomization = False
        object cxGridDBChartDataGroup6: TcxGridDBChartDataGroup
          DataBinding.FieldName = 'State'
        end
        object cxGridDBChartDataGroup5: TcxGridDBChartDataGroup
          DataBinding.FieldName = 'SeasonallyAdjusted'
          DisplayText = 'Seasonally Adjusted'
        end
        object cxGridDBChartSeries11: TcxGridDBChartSeries
          DataBinding.FieldName = 'Mid-West'
        end
        object cxGridDBChartSeries12: TcxGridDBChartSeries
          DataBinding.FieldName = 'North-East'
        end
        object cxGridDBChartSeries13: TcxGridDBChartSeries
          DataBinding.FieldName = 'South'
        end
        object cxGridDBChartSeries14: TcxGridDBChartSeries
          DataBinding.FieldName = 'West'
        end
      end
      object cxGridLevel3: TcxGridLevel
        GridView = cvChart
      end
    end
  end
end
