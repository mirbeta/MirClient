inherited frmAgents: TfrmAgents
  Left = 0
  Top = 0
  Width = 965
  Height = 515
  OnCanResize = AA
  object tcAgents: TdxTileControl
    Left = 0
    Top = 0
    Width = 224
    Height = 515
    Align = alLeft
    AutoSize = True
    BorderStyle = cxcbsDefault
    LookAndFeel.NativeStyle = False
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsBehavior.ItemMoving = False
    OptionsView.IndentHorz = 8
    OptionsView.IndentVert = 8
    OptionsView.ItemSize = 170
    OptionsView.GroupLayout = glVertical
    OptionsView.GroupMaxRowCount = 1024
    TabOrder = 0
  end
  object pnDetailSite: TPanel
    Left = 224
    Top = 0
    Width = 741
    Height = 515
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object cxgDetailInfo: TcxGrid
      Left = 0
      Top = 0
      Width = 741
      Height = 285
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object cxGridDBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        Navigator.Buttons.First.Visible = True
        Navigator.Buttons.PriorPage.Visible = True
        Navigator.Buttons.Prior.Visible = True
        Navigator.Buttons.Next.Visible = True
        Navigator.Buttons.NextPage.Visible = True
        Navigator.Buttons.Last.Visible = True
        Navigator.Buttons.Insert.Visible = True
        Navigator.Buttons.Append.Visible = False
        Navigator.Buttons.Delete.Visible = True
        Navigator.Buttons.Edit.Visible = True
        Navigator.Buttons.Post.Visible = True
        Navigator.Buttons.Cancel.Visible = True
        Navigator.Buttons.Refresh.Visible = True
        Navigator.Buttons.SaveBookmark.Visible = True
        Navigator.Buttons.GotoBookmark.Visible = True
        Navigator.Buttons.Filter.Visible = True
        DataController.DataSource = DMRealtorWorld.dsHomesDetail
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsSelection.HideFocusRectOnExit = False
        OptionsSelection.HideSelection = True
        OptionsSelection.UnselectFocusedRecordOnExit = False
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        object cxGridDBTableView1Photo: TcxGridDBColumn
          DataBinding.FieldName = 'Photo'
          PropertiesClassName = 'TcxImageProperties'
          Properties.FitMode = ifmProportionalStretch
          Properties.GraphicClassName = 'TdxSmartImage'
          Width = 121
        end
        object cxGridDBTableView1Address: TcxGridDBColumn
          DataBinding.FieldName = 'Address'
          PropertiesClassName = 'TcxMemoProperties'
          Width = 268
        end
        object cxGridDBTableView1Beds: TcxGridDBColumn
          Caption = 'Bedrooms'
          DataBinding.FieldName = 'Beds'
          Width = 68
        end
        object cxGridDBTableView1Baths: TcxGridDBColumn
          Caption = 'Bathrooms'
          DataBinding.FieldName = 'Baths'
          Width = 68
        end
        object cxGridDBTableView1HouseSize: TcxGridDBColumn
          Caption = 'House Size'
          DataBinding.FieldName = 'HouseSize'
          Width = 91
        end
        object cxGridDBTableView1Price: TcxGridDBColumn
          DataBinding.FieldName = 'Price'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Width = 106
        end
      end
      object cxGridLevel1: TcxGridLevel
        GridView = cxGridDBTableView1
      end
    end
    object cxgDetailChart: TcxGrid
      Left = 0
      Top = 289
      Width = 741
      Height = 226
      Align = alClient
      TabOrder = 1
      object cxgDetailChartDBChartView1: TcxGridDBChartView
        Categories.DataBinding.FieldName = 'Date'
        DataController.DataSource = dsChart
        DiagramColumn.Active = True
        Legend.Alignment = cpaStart
        Legend.Border = lbSingle
        OptionsView.CategoriesPerPage = 3
        object cxgNorthEastSeries: TcxGridDBChartSeries
          DataBinding.FieldName = 'North-East'
          DisplayText = 'North-East'
        end
        object cxgMidWestSeries: TcxGridDBChartSeries
          DataBinding.FieldName = 'Mid-West'
          DisplayText = 'Mid-West'
        end
        object cxgSouthSeries: TcxGridDBChartSeries
          DataBinding.FieldName = 'South'
          DisplayText = 'South'
        end
        object cxgWestSeries: TcxGridDBChartSeries
          DataBinding.FieldName = 'West'
          DisplayText = 'West'
        end
      end
      object cxgDetailChartLevel1: TcxGridLevel
        GridView = cxgDetailChartDBChartView1
      end
    end
    object cxSplitter1: TcxSplitter
      Left = 0
      Top = 285
      Width = 741
      Height = 4
      AlignSplitter = salTop
      Control = cxgDetailInfo
      OnBeforeClose = cxSplitter2BeforeClose
    end
  end
  object dsChart: TDataSource
    DataSet = cdsChart
    Left = 520
    Top = 344
  end
  object cdsChart: TClientDataSet
    Active = True
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 560
    Top = 344
    Data = {
      760000009619E0BD0100000018000000060000000000030000007600084D6964
      2D5765737404000100000000000A4E6F7274682D456173740400010000000000
      05536F757468040001000000000004576573740400010000000000074167656E
      7449440400010000000000044461746504000100000000000000}
    object cdsChartMidWest: TIntegerField
      FieldName = 'Mid-West'
    end
    object cdsChartNorthEast: TIntegerField
      FieldName = 'North-East'
    end
    object cdsChartSouth: TIntegerField
      FieldName = 'South'
    end
    object cdsChartWest: TIntegerField
      FieldName = 'West'
    end
    object cdsChartAgentID: TIntegerField
      FieldName = 'AgentID'
    end
    object cdsChartDate: TIntegerField
      FieldName = 'Date'
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clRed
    end
  end
end
