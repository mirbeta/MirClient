inherited frmSales: TfrmSales
  Width = 1224
  Height = 678
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 1224
    Height = 558
    object cbxYears: TcxComboBox [0]
      Left = 426
      Top = 17
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = cbxYearsPropertiesEditValueChanged
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 0
      Width = 152
    end
    object cxGridChart: TcxGrid [1]
      Left = 302
      Top = 56
      Width = 400
      Height = 485
      TabOrder = 1
      object cxGridChartDBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        object cxGridChartDBTableView1RecId: TcxGridDBColumn
          DataBinding.FieldName = 'RecId'
          Visible = False
        end
        object cxGridChartDBTableView1Year: TcxGridDBColumn
          DataBinding.FieldName = 'Year'
        end
        object cxGridChartDBTableView1Category: TcxGridDBColumn
          DataBinding.FieldName = 'Category'
        end
        object cxGridChartDBTableView1Total: TcxGridDBColumn
          DataBinding.FieldName = 'Total'
        end
      end
      object gvChartView: TcxGridDBChartView
        Categories.DataBinding.FieldName = 'CategoryName'
        DataController.DataSource = DM.dsYearSales
        DataController.KeyFieldNames = 'Category'
        DiagramPie.Active = True
        DiagramPie.Legend.Alignment = cpaStart
        DiagramPie.Legend.Position = cppTop
        DiagramPie.Values.CaptionPosition = pdvcpCenter
        object gvChartViewSeries1: TcxGridDBChartSeries
          DataBinding.FieldName = 'Total'
        end
      end
      object cxGridChartLevel1: TcxGridLevel
        GridView = gvChartView
      end
    end
    object cxGridSales: TcxGrid [2]
      Left = 59
      Top = 17
      Width = 233
      Height = 524
      TabOrder = 2
      object gvSales: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        FindPanel.DisplayMode = fpdmAlways
        FindPanel.Position = fppBottom
        OnCellDblClick = gvSalesCellDblClick
        DataController.DataSource = DM.dsOrders
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <
          item
            Format = '$,0.00;-$,0.00'
            Kind = skSum
            FieldName = 'TotalAmount'
            Column = gvSalesTotalAmount
          end>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsData.CancelOnExit = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 40
        OptionsView.Footer = True
        OptionsView.FooterAutoHeight = True
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Footer = DM.cxStyle4
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        object gvSalesInvoiceNumber: TcxGridDBColumn
          Caption = 'Invoice'
          DataBinding.FieldName = 'InvoiceNumber'
          OnGetFilterValues = gvSalesNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          SortIndex = 0
          SortOrder = soDescending
          Width = 136
        end
        object gvSalesOrderDate: TcxGridDBColumn
          Caption = 'Order Date'
          DataBinding.FieldName = 'OrderDate'
          PropertiesClassName = 'TcxDateEditProperties'
          Properties.Alignment.Horz = taCenter
          Properties.DisplayFormat = 'mm/dd/yyyy'
          OnGetFilterValues = gvSalesNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 117
        end
        object gvSalesName: TcxGridDBColumn
          Caption = 'Name'
          DataBinding.FieldName = 'CustomerId'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'Name'
            end>
          Properties.ListSource = DM.dsCustomersHelper
          OnGetFilterValues = gvSalesNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 230
        end
        object gvSalesCity: TcxGridDBColumn
          Caption = 'City'
          DataBinding.FieldName = 'StoreId'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'Address_Full'
            end>
          Properties.ListSource = DM.dsCustomerStoresHelper
          OnGetFilterValues = gvSalesNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 230
        end
        object gvSalesTotalAmount: TcxGridDBColumn
          Caption = 'Total Amount'
          DataBinding.FieldName = 'TotalAmount'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0.00;$-,0.00'
          OnGetFilterValues = gvSalesNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 170
        end
      end
      object cxGridSalesLevel1: TcxGridLevel
        GridView = gvSales
      end
    end
    object cxDBPivotGrid1: TcxDBPivotGrid [3]
      Left = 712
      Top = 46
      Width = 495
      Height = 495
      DataSource = DM.dsQuotes
      Groups = <>
      OptionsView.ColumnFields = False
      OptionsView.ColumnGrandTotals = False
      OptionsView.DataFields = False
      OptionsView.FilterFields = False
      OptionsView.HeaderFilterButtonShowMode = pgfbmSmartTag
      OptionsView.RowGrandTotalWidth = 20
      Styles.ColumnHeader = DM.cxStyle1
      Styles.ColumnMaximumValue = DM.cxStyle1
      Styles.ColumnMinimumValue = DM.cxStyle1
      Styles.Content = DM.cxStyle1
      Styles.FieldHeader = DM.cxStyle1
      Styles.FilterHeaderArea = DM.cxStyle1
      Styles.FilterSeparator = DM.cxStyle1
      Styles.Inactive = DM.cxStyle1
      Styles.MaximumValue = DM.cxStyle1
      Styles.MinimumValue = DM.cxStyle1
      Styles.Prefilter = DM.cxStyle1
      Styles.RowHeader = DM.cxStyle1
      Styles.Total = DM.cxStyle1
      TabOrder = 3
      object cxDBPivotGrid1Total: TcxDBPivotGridField
        Area = faData
        AreaIndex = 0
        IsCaptionAssigned = True
        Caption = 'Opportunities'
        DataBinding.FieldName = 'Total'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Properties.DisplayFormat = '$,0.00;$-,0.00'
        SortOrder = soAscending
        Visible = True
        Width = 130
        UniqueName = 'Total'
      end
      object cxDBPivotGrid1Opportunity: TcxDBPivotGridField
        Area = faData
        AreaIndex = 1
        IsCaptionAssigned = True
        Caption = 'Percentage'
        DataBinding.FieldName = 'Percent'
        PropertiesClassName = 'TcxProgressBarProperties'
        Properties.BeginColor = 10258176
        Properties.EndColor = clNavy
        Properties.OverloadBeginColor = clWhite
        Properties.OverloadEndColor = clWhite
        Properties.PeakValue = 100.000000000000000000
        SummaryType = stAverage
        Visible = True
        Width = 110
        UniqueName = 'Opportunity'
      end
      object cxDBPivotGrid1State: TcxDBPivotGridField
        Area = faRow
        AreaIndex = 0
        IsCaptionAssigned = True
        Caption = 'State'
        DataBinding.FieldName = 'OfficeState'
        Visible = True
        Width = 120
        UniqueName = 'State'
      end
      object cxDBPivotGrid1City: TcxDBPivotGridField
        Area = faRow
        AreaIndex = 1
        IsCaptionAssigned = True
        Caption = 'City'
        DataBinding.FieldName = 'OfficeCity'
        Visible = True
        Width = 110
        UniqueName = 'City'
      end
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      LayoutLookAndFeel = DM.dxLayoutCxLookAndFeelNavy
    end
    inherited dxLayoutGroup2: TdxLayoutGroup
      LayoutDirection = ldHorizontal
    end
    inherited lgBackButton: TdxLayoutGroup
      Visible = False
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahCenter
      AlignVert = avTop
      CaptionOptions.Text = 'SALES'
      CaptionOptions.Visible = False
      Control = cbxYears
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 152
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cxGridChart
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 400
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cxGridSales
      ControlOptions.OriginalHeight = 381
      ControlOptions.OriginalWidth = 549
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liOpportunities: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = 'OPPORTUNITIES'
      CaptionOptions.Layout = clTop
      Control = cxDBPivotGrid1
      ControlOptions.OriginalHeight = 264
      ControlOptions.OriginalWidth = 495
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 558
    Width = 1224
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 1220
      object btnView: TcxButton [0]
        Left = 517
        Top = 17
        Width = 80
        Height = 82
        Caption = 'View'
        OptionsImage.ImageIndex = 19
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = btnViewClick
      end
      object btnPrint: TcxButton [1]
        Left = 623
        Top = 17
        Width = 80
        Height = 82
        Caption = 'Print'
        Enabled = False
        OptionsImage.ImageIndex = 18
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnView
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem8: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnPrint
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Enabled = False
        Index = 2
      end
      object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Separator'
        Index = 1
      end
    end
  end
end
