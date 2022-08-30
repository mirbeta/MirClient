inherited frmMortgageRate: TfrmMortgageRate
  Width = 883
  Height = 429
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    PanelStyle.Active = True
    TabOrder = 0
    Height = 429
    Width = 521
    object cxGrid2: TcxGrid
      Left = 2
      Top = 2
      Width = 517
      Height = 425
      Align = alClient
      BorderStyle = cxcbsNone
      TabOrder = 0
      object cxGrid2DBTableView1: TcxGridDBTableView
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
      object cxGrid2DBChartView1: TcxGridDBChartView
        Categories.DataBinding.FieldName = 'Date1'
        DataController.DataSource = DMRealtorWorld.dsMortgage
        DiagramLine.Active = True
        DiagramLine.AxisValue.MinMaxValues = mmvCustom
        DiagramLine.AxisValue.MinValue = 2.500000000000000000
        DiagramLine.AxisValue.MaxValue = 7.000000000000000000
        DiagramLine.Values.LineWidth = 3
        Legend.Border = lbSingle
        OptionsView.CategoriesPerPage = 15
        Title.Text = 'Mortgage Rate Data'
        object cxGrid2DBChartView1Series1: TcxGridDBChartSeries
          DataBinding.FieldName = 'FRM30'
          DisplayText = '30 year Fixed'
        end
        object cxGrid2DBChartView1Series2: TcxGridDBChartSeries
          DataBinding.FieldName = 'FRM15'
          DisplayText = '15 year Fixed'
        end
        object cxGrid2DBChartView1Series3: TcxGridDBChartSeries
          DataBinding.FieldName = 'ARM1'
          DisplayText = '1 year ARM'
        end
      end
      object cxGrid2Level1: TcxGridLevel
        GridView = cxGrid2DBChartView1
      end
    end
  end
  object cxGroupBox2: TcxGroupBox
    Left = 525
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    TabOrder = 1
    Height = 429
    Width = 358
    object cxGrid1: TcxGrid
      Left = 2
      Top = 2
      Width = 354
      Height = 425
      Align = alClient
      BorderStyle = cxcbsNone
      TabOrder = 0
      object cxGrid1DBTableView1: TcxGridDBTableView
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
        DataController.DataSource = DMRealtorWorld.dsMortgage
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsCustomize.ColumnHidingOnGrouping = False
        OptionsCustomize.ColumnMoving = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsSelection.HideFocusRectOnExit = False
        OptionsSelection.HideSelection = True
        OptionsSelection.UnselectFocusedRecordOnExit = False
        OptionsView.ColumnAutoWidth = True
        object cxGrid1DBTableView1Date1: TcxGridDBColumn
          Caption = 'Date'
          DataBinding.FieldName = 'Date1'
          PropertiesClassName = 'TcxDateEditProperties'
          Visible = False
          DateTimeGrouping = dtgByMonth
          GroupIndex = 0
          Options.Filtering = False
          SortIndex = 0
          SortOrder = soDescending
        end
        object cxGrid1DBTableView1FRM30: TcxGridDBColumn
          Caption = '30 year Fixed'
          DataBinding.FieldName = 'FRM30'
          Options.Filtering = False
        end
        object cxGrid1DBTableView1FRM15: TcxGridDBColumn
          Caption = '30 year Fixed'
          DataBinding.FieldName = 'FRM15'
          Options.Filtering = False
        end
        object cxGrid1DBTableView1ARM1: TcxGridDBColumn
          Caption = '1 year ARM'
          DataBinding.FieldName = 'ARM1'
          Options.Filtering = False
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1DBTableView1
      end
    end
  end
  object cxSplitter1: TcxSplitter
    Left = 521
    Top = 0
    Width = 4
    Height = 429
    Control = cxGroupBox1
    OnBeforeClose = cxSplitter1BeforeClose
  end
end
