inherited frmDataSets: TfrmDataSets
  Left = 129
  Top = 55
  Caption = 'PivotGrid - DataSets'
  ClientHeight = 712
  ClientWidth = 964
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 964
    Caption = 
      'This demo shows you how to link Summary and DrillDown datasets t' +
      'o a pivot grid, to display its data using the ExpressQuantumGrid' +
      #39's Table Views and Chart Views. The ExpressQuantumGrid is not a ' +
      'part of the ExpressPivotGrid Suite, and must be purchased separa' +
      'tely.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 32
    Width = 964
    Height = 254
    Align = alTop
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    OptionsSelection.MultiSelect = True
    TabOrder = 0
    object pgfPurchaseDate: TcxDBPivotGridField
      AreaIndex = 1
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateMonth
      Visible = True
      UniqueName = 'PurchaseDate'
    end
    object pgfPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
      UniqueName = 'Payment Type'
    end
    object pgfQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      DataBinding.FieldName = 'Quantity'
      DisplayFormat = '0'
      Visible = True
      Width = 49
      UniqueName = 'Quantity'
    end
    object pgfCarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      DataBinding.FieldName = 'Car Name'
      Visible = True
      Width = 77
      UniqueName = 'Car Name'
    end
    object pgfUnitPrice: TcxDBPivotGridField
      AreaIndex = 0
      DataBinding.FieldName = 'Unit Price'
      Visible = True
      UniqueName = 'Unit Price'
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      Visible = True
      Width = 131
      UniqueName = 'Company Name'
    end
    object pgfPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      PropertiesClassName = 'TdxSparklineProperties'
      Properties.Series = <
        item
        end>
      SummaryType = stCustom
      Visible = True
      Width = 90
      OnCalculateCustomSummary = pgfPaymentAmountCalculateCustomSummary
      UniqueName = 'Payment Amount'
    end
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 286
    Width = 964
    Height = 426
    Align = alClient
    TabOrder = 1
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = GridActiveTabChanged
    object SummaryChartView: TcxGridDBChartView
      DataController.DataSource = PivotGridSummaryDataSource
      DiagramPie.Active = True
      ToolBox.CustomizeButton = True
      ToolBox.DiagramSelector = True
    end
    object SummaryTableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = PivotGridSummaryDataSource
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.Footer = True
    end
    object DrillDownTableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = PivotGridDrillDownDataSource
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.Footer = True
    end
    object SummaryTableLevel: TcxGridLevel
      Caption = 'Summary Table'
      GridView = SummaryTableView
    end
    object SummaryChartLevel: TcxGridLevel
      Tag = 1
      Caption = 'Summary Chart'
      GridView = SummaryChartView
    end
    object DrillDownTableLevel: TcxGridLevel
      Tag = 2
      Caption = 'DrillDown Table'
      GridView = DrillDownTableView
    end
  end
  object PivotGridSummaryDataSet: TcxPivotGridSummaryDataSet
    PivotGrid = DBPivotGrid
    SynchronizeData = True
    OnCreateField = PivotGridDataSetCreateField
    OnDataChanged = PivotGridDataSetDataChanged
    Left = 760
    Top = 528
  end
  object PivotGridSummaryDataSource: TDataSource
    DataSet = PivotGridSummaryDataSet
    Left = 808
    Top = 544
  end
  object PivotGridDrillDownDataSet: TcxPivotGridDrillDownDataSet
    PivotGrid = DBPivotGrid
    OnCreateField = PivotGridDataSetCreateField
    OnDataChanged = PivotGridDataSetDataChanged
    Left = 752
    Top = 464
  end
  object PivotGridDrillDownDataSource: TDataSource
    DataSet = PivotGridDrillDownDataSet
    Left = 808
    Top = 480
  end
end
