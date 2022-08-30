inherited frmStatistic: TfrmStatistic
  Width = 1118
  Height = 648
  inherited tcHomePhotos: TdxTileControl
    Height = 648
    TabOrder = 1
  end
  object cxGroupBox1: TcxGroupBox
    Left = 249
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.Edges = []
    TabOrder = 0
    Height = 648
    Width = 869
    object cxGroupBox2: TcxGroupBox
      Left = 2
      Top = 2
      Align = alLeft
      PanelStyle.Active = True
      TabOrder = 0
      Height = 644
      Width = 385
      object cxGrid2: TcxGrid
        Left = 2
        Top = 2
        Width = 381
        Height = 640
        Align = alClient
        BorderStyle = cxcbsNone
        TabOrder = 0
        object cxGridDBTableView2: TcxGridDBTableView
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
          DataController.DataSource = DMRealtorWorld.dsHouseRating
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
        end
        object cxGridDBChartView2: TcxGridDBChartView
          Categories.DataBinding.FieldName = 'RegionName'
          DataController.DataSource = DMRealtorWorld.dsHouseRating
          DiagramLine.AxisValue.MinMaxValues = mmvCustom
          DiagramLine.AxisValue.MinValue = 200.000000000000000000
          DiagramLine.AxisValue.MaxValue = 2000.000000000000000000
          DiagramLine.Values.LineWidth = 3
          DiagramPie.Active = True
          DiagramPie.Legend.Orientation = cpoHorizontal
          DiagramPie.Legend.Position = cppBottom
          DiagramPie.Styles.ValueCaptions = cxStyle2
          DiagramPie.Values.CaptionPosition = pdvcpCenter
          DiagramPie.Values.CaptionItems = [pdvciCategory, pdvciValue, pdvciPercentage]
          Legend.Border = lbSingle
          Legend.Position = cppRight
          OptionsView.CategoriesPerPage = 15
          object cxGridDBChartView2Series1: TcxGridDBChartSeries
            DataBinding.FieldName = 'Value'
            DisplayText = 'Housing Type Rating by Region'
          end
        end
        object cxGridLevel2: TcxGridLevel
          GridView = cxGridDBChartView2
        end
      end
    end
    object cxGroupBox3: TcxGroupBox
      Left = 391
      Top = 2
      Align = alClient
      PanelStyle.Active = True
      TabOrder = 1
      Height = 644
      Width = 476
      object grHousePrice: TcxGrid
        Left = 2
        Top = 317
        Width = 472
        Height = 325
        Align = alClient
        BorderStyle = cxcbsNone
        TabOrder = 0
        object grHousePriceDBTableView1: TcxGridDBTableView
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
          DataController.DataSource = DMRealtorWorld.dsHousesSimular
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
        end
        object grHousePriceDBChartView1: TcxGridDBChartView
          Categories.DataBinding.FieldName = 'Year'
          DataController.DataSource = DMRealtorWorld.dsHousesSimular
          DiagramColumn.Active = True
          DiagramLine.AxisValue.MinMaxValues = mmvCustom
          DiagramLine.AxisValue.MinValue = 200.000000000000000000
          DiagramLine.AxisValue.MaxValue = 2000.000000000000000000
          DiagramLine.Values.LineWidth = 3
          Legend.Border = lbSingle
          Legend.Position = cppTop
          OptionsView.CategoriesPerPage = 15
          Title.Text = 'The Demand and Supply for Simular House'
          object grHousePriceDBChartView1Series1: TcxGridDBChartSeries
            DataBinding.FieldName = 'Proposals'
          end
          object grHousePriceDBChartView1Series2: TcxGridDBChartSeries
            DataBinding.FieldName = 'Sold'
          end
        end
        object grHousePriceLevel1: TcxGridLevel
          GridView = grHousePriceDBChartView1
        end
      end
      object cxGrid1: TcxGrid
        Left = 2
        Top = 2
        Width = 472
        Height = 311
        Align = alTop
        BorderStyle = cxcbsNone
        TabOrder = 1
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
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
        end
        object cxGridDBChartView1: TcxGridDBChartView
          Categories.DataBinding.FieldName = 'Date'
          DataController.DataSource = DMRealtorWorld.dsHousePrice
          DiagramLine.Active = True
          DiagramLine.AxisCategory.GridLines = False
          DiagramLine.AxisCategory.TickMarkKind = tmkNone
          DiagramLine.AxisCategory.TickMarkLabels = False
          DiagramLine.AxisValue.MinMaxValues = mmvAuto
          DiagramLine.Values.LineWidth = 3
          Legend.Border = lbSingle
          Legend.Position = cppNone
          Title.Text = 'House Price Over Time'
          object cxGridDBChartSeries1: TcxGridDBChartSeries
            DataBinding.FieldName = 'Price'
          end
        end
        object cxGridLevel1: TcxGridLevel
          GridView = cxGridDBChartView1
        end
      end
      object cxSplitter3: TcxSplitter
        Left = 2
        Top = 313
        Width = 472
        Height = 4
        AlignSplitter = salTop
        Control = cxGrid1
        OnBeforeClose = cxSplitter2BeforeClose
      end
    end
    object cxSplitter2: TcxSplitter
      Left = 387
      Top = 2
      Width = 4
      Height = 644
      Control = cxGroupBox2
      OnBeforeClose = cxSplitter2BeforeClose
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clRed
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clWhite
    end
  end
end
