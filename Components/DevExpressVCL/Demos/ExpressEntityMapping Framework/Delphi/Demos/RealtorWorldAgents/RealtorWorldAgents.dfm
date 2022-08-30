object frmAgents: TfrmAgents
  Left = 0
  Top = 0
  ClientHeight = 456
  ClientWidth = 949
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescription: TLabel
    Left = 0
    Top = 0
    Width = 949
    Height = 32
    Align = alTop
    Caption = 
      'This demo shows how to use the ExpressQuantumGrid control and th' +
      'e TdxEMFDataSet component shipped with the ExpressEntityMapping ' +
      'Framework. '
    Color = 12937777
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    WordWrap = True
  end
  object tcAgents: TdxTileControl
    Left = 0
    Top = 32
    Width = 224
    Height = 424
    Align = alLeft
    AutoSize = True
    BorderStyle = cxcbsDefault
    LookAndFeel.NativeStyle = False
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsBehavior.ItemMoving = False
    OptionsView.GroupLayout = glVertical
    OptionsView.GroupMaxRowCount = 1024
    OptionsView.IndentHorz = 8
    OptionsView.IndentVert = 8
    OptionsView.ItemHeight = 170
    OptionsView.ItemWidth = 170
    TabOrder = 0
  end
  object pnDetailSite: TPanel
    Left = 224
    Top = 32
    Width = 725
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object cxgDetailInfo: TcxGrid
      Left = 0
      Top = 0
      Width = 725
      Height = 210
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
      Top = 214
      Width = 725
      Height = 210
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
          DataBinding.FieldName = 'NorthEast'
          DisplayText = 'North-East'
        end
        object cxgMidWestSeries: TcxGridDBChartSeries
          DataBinding.FieldName = 'MidWest'
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
      Top = 210
      Width = 725
      Height = 4
      AlignSplitter = salTop
      Control = cxgDetailInfo
      OnBeforeClose = cxSplitter2BeforeClose
    end
  end
  object dsChart: TDataSource
    DataSet = DMRealtorWorld.edsChart
    Left = 520
    Top = 344
  end
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clRed
    end
  end
  object mmMain: TMainMenu
    Left = 904
    Top = 56
    object miAbout: TMenuItem
      Caption = '&About this demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
end
