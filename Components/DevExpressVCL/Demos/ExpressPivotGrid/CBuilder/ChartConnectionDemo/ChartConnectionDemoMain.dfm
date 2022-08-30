inherited frmChartConnection: TfrmChartConnection
  Left = 193
  Top = 145
  Caption = 'PivotGrid - Chart Connection Demo'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo shows you how to visualize pivot grid data using the E' +
      'xpressQuantumGrid'#39's Chart View. The ExpressQuantumGrid is not a ' +
      'part of the ExpressPivotGrid Suite, and must be purchased separa' +
      'tely.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 32
    Width = 807
    Height = 248
    Align = alClient
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    OptionsSelection.MultiSelect = True
    TabOrder = 0
    object pgfPurchaseDate: TcxDBPivotGridField
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Purchase Date'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateMonth
      Visible = True
    end
    object pgfPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
      OnGetGroupImageIndex = pgfPaymentTypeGetGroupImageIndex
    end
    object pgfQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      DataBinding.FieldName = 'Quantity'
      DisplayFormat = '0'
      Visible = True
      Width = 49
    end
    object pgfCarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      DataBinding.FieldName = 'Car Name'
      Visible = True
      Width = 77
    end
    object pgfUnitPrice: TcxDBPivotGridField
      AreaIndex = 0
      DataBinding.FieldName = 'Unit Price'
      Visible = True
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      Visible = True
      Width = 131
    end
    object pgfPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      Visible = True
      Width = 90
    end
  end
  object cxGrid: TcxGrid [2]
    Left = 0
    Top = 280
    Width = 807
    Height = 271
    Align = alBottom
    TabOrder = 1
    object cxGridDBTableView1: TcxGridDBTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
    end
    object cxGridChartView: TcxGridChartView
      DiagramColumn.Active = True
      ToolBox.CustomizeButton = True
      ToolBox.DiagramSelector = True
      OnActiveDiagramChanged = cxGridChartViewActiveDiagramChanged
    end
    object cxGridLevel: TcxGridLevel
      GridView = cxGridChartView
    end
  end
  inherited mmMain: TMainMenu
    object miChartOptions: TMenuItem [1]
      Caption = 'Chart Options'
      object miSourceData: TMenuItem
        Caption = 'Source Data '
        object miSelectedCells: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Selected Cells'
          RadioItem = True
          OnClick = miVisibleCellsClick
        end
        object miVisibleCells: TMenuItem
          AutoCheck = True
          Caption = 'Visible Cells'
          Checked = True
          RadioItem = True
          OnClick = miVisibleCellsClick
        end
      end
      object miSourceForCategories: TMenuItem
        Caption = 'Source For Categories'
        object miSourceColumn: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Columns'
          RadioItem = True
          OnClick = miSourceColumnClick
        end
        object miSourceRow: TMenuItem
          AutoCheck = True
          Caption = 'Rows'
          Checked = True
          RadioItem = True
          OnClick = miSourceColumnClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miChartType: TMenuItem
        Caption = 'Diagram Type'
      end
    end
    inherited miOptions: TMenuItem
      inherited miElementsVisibility: TMenuItem
        inherited miShowColumnFields: TMenuItem
          Caption = 'Show Column Fields'
        end
        inherited miShowDataFields: TMenuItem
          Caption = 'Show Data Fields'
        end
        inherited miShowFilterFields: TMenuItem
          Caption = 'Show Filter Fields'
        end
        inherited miShowFilterSeparator: TMenuItem
          Caption = 'Show Filter Separator'
        end
        inherited miShowRowFields: TMenuItem
          Caption = 'Show Row Fields'
        end
      end
      inherited miSelection: TMenuItem
        inherited miMultiSelect: TMenuItem
          Checked = True
        end
      end
    end
  end
  object cxPivotGridChartConnection: TcxPivotGridChartConnection
    GridChartView = cxGridChartView
    PivotGrid = DBPivotGrid
    Left = 712
  end
end
