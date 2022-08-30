inherited GridModeDemoMainForm: TGridModeDemoMainForm
  Left = 55
  Top = 52
  Caption = 'ExpressQuantumGrid Grid Mode Demo'
  ClientHeight = 529
  ClientWidth = 856
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 856
    Caption = 
      'This demo loads and processes 100,000 records using gridmode. Cl' +
      'ick ''About this demo'' for more information.'#13#10'Note: Support is provided for BDE 32-bit (BDE 64-bit is not supported).'
  end
  inherited sbMain: TStatusBar
    Top = 510
    Width = 856
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 56
    Width = 856
    Height = 454
    Align = alClient
    TabOrder = 1
    object tvCars: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataModeController.GridMode = True
      DataController.DataSource = GridModeDemoDataDM.dsCars
      DataController.Filter.OnGetValueList = tvDataControllerFilterGetValueList
      DataController.Filter.AutoDataSetFilter = True
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          FieldName = 'Trademark'
          Column = tvCarsTrademark
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.FocusCellOnTab = True
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnSorting = False
      OptionsView.GroupByBox = False
      OptionsView.Indicator = True
      Preview.MaxLineCount = 1
      Preview.RightIndent = 10
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvCarsTrademark: TcxGridDBColumn
        DataBinding.FieldName = 'Trademark'
        Width = 100
      end
      object tvCarsModel: TcxGridDBColumn
        DataBinding.FieldName = 'Model'
        Width = 180
      end
      object tvCarshp: TcxGridDBColumn
        DataBinding.FieldName = 'hp'
        PropertiesClassName = 'TcxSpinEditProperties'
        Properties.Increment = 10.000000000000000000
      end
      object tvCarsliter: TcxGridDBColumn
        DataBinding.FieldName = 'liter'
        PropertiesClassName = 'TcxSpinEditProperties'
        Properties.Increment = 0.100000000000000000
      end
      object tvCarscyl: TcxGridDBColumn
        DataBinding.FieldName = 'cyl'
        PropertiesClassName = 'TcxSpinEditProperties'
      end
      object tvCarsTransmissSpeedCount: TcxGridDBColumn
        Caption = 'Transmiss Speed Count'
        DataBinding.FieldName = 'TransmissSpeedCount'
        PropertiesClassName = 'TcxSpinEditProperties'
        Width = 130
      end
      object tvCarsTransmissAutomatic: TcxGridDBColumn
        Caption = 'Automatic Transmission'
        DataBinding.FieldName = 'TransmissAutomatic'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Properties.ValueChecked = 'Yes'
        Properties.ValueUnchecked = 'No'
        Width = 135
      end
      object tvCarsMPG_City: TcxGridDBColumn
        DataBinding.FieldName = 'MPG_City'
        Width = 70
      end
      object tvCarsMPG_Highway: TcxGridDBColumn
        DataBinding.FieldName = 'MPG_Highway'
        Width = 100
      end
      object tvCarsCategory: TcxGridDBColumn
        DataBinding.FieldName = 'Category'
        Width = 70
      end
      object tvCarsDescription: TcxGridDBColumn
        DataBinding.FieldName = 'Description'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Properties.MemoScrollBars = ssVertical
      end
      object tvCarsHyperlink: TcxGridDBColumn
        DataBinding.FieldName = 'Hyperlink'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Width = 150
      end
      object tvCarsPrice: TcxGridDBColumn
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Width = 80
      end
    end
    object tvOrders: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataModeController.GridMode = True
      DataController.DataSource = GridModeDemoDataDM.dsOrders
      DataController.DetailKeyFieldNames = 'ProductID'
      DataController.Filter.OnGetValueList = tvDataControllerFilterGetValueList
      DataController.Filter.AutoDataSetFilter = True
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          FieldName = 'CustomerID'
          Column = tvOrdersCustomerID
        end
        item
          Kind = skSum
          FieldName = 'PaymentAmount'
          Column = tvOrdersPaymentAmount
        end
        item
          Kind = skMax
          FieldName = 'PurchaseDate'
          Column = tvOrdersPurchaseDate
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.FocusCellOnTab = True
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnSorting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      OptionsView.Indicator = True
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvOrdersCustomerID: TcxGridDBColumn
        Caption = 'Company'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Company'
          end>
        Properties.ListOptions.AnsiSort = True
        Properties.ListOptions.CaseInsensitive = True
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = GridModeDemoDataDM.dsCustomers
        Properties.MaxLength = 50
        Options.Sorting = False
        Width = 100
      end
      object tvOrdersPurchaseDate: TcxGridDBColumn
        DataBinding.FieldName = 'PurchaseDate'
        PropertiesClassName = 'TcxDateEditProperties'
        Width = 60
      end
      object tvOrdersPaymentType: TcxGridDBColumn
        DataBinding.FieldName = 'PaymentType'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Vert = taVCenter
        Properties.Images = GridModeDemoDataDM.PaymentTypeImages
        Properties.Items = <
          item
            Description = 'Am. Express'
            ImageIndex = 3
            Value = 'AmEx'
          end
          item
            Description = 'Cash'
            ImageIndex = 0
            Value = 'Cash'
          end
          item
            Description = 'Master'
            ImageIndex = 2
            Value = 'Master'
          end
          item
            Description = 'Visa'
            ImageIndex = 1
            Value = 'Visa'
          end>
      end
      object tvOrdersPaymentAmount: TcxGridDBColumn
        DataBinding.FieldName = 'PaymentAmount'
        PropertiesClassName = 'TcxCalcEditProperties'
        Properties.Alignment.Horz = taRightJustify
      end
    end
    object lvCars: TcxGridLevel
      GridView = tvCars
      MaxDetailHeight = 300
      object lvOrders: TcxGridLevel
        GridView = tvOrders
      end
    end
  end
  object pnlPopulate: TPanel [3]
    Left = 0
    Top = 16
    Width = 856
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    Color = 4707838
    TabOrder = 2
    object btnPopulate: TcxButton
      Left = 8
      Top = 8
      Width = 153
      Height = 25
      Hint = 'Click here to populate the Orders table (100000 records)'
      Caption = 'Add about 100,000 orders'
      Colors.Default = 16247513
      Colors.Normal = 16247513
      TabOrder = 0
      OnClick = btnPopulateClick
    end
    object pnlProgress: TPanel
      Left = 176
      Top = 8
      Width = 681
      Height = 25
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'Please wait ...'
      ParentColor = True
      TabOrder = 1
      Visible = False
      object ProgressBar: TcxProgressBar
        Left = 77
        Top = 3
        Properties.Max = 1000.000000000000000000
        TabOrder = 0
        Width = 596
      end
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miCustomizeViews: TMenuItem
        Caption = 'Customize &Views'
        object miCalculateSummaries: TMenuItem
          AutoCheck = True
          Caption = 'Calculate &Summaries'
          Hint = 'Summary calculation. If checked, the demo will work more slowly'
          OnClick = miCalculateSummariesClick
        end
        object miEnableSorting: TMenuItem
          AutoCheck = True
          Caption = 'Enable S&orting'
          Hint = 'Enable Sorting. If checked, the demo will work more slowly'
          OnClick = miEnableSortingClick
        end
        object miEnableFiltering: TMenuItem
          AutoCheck = True
          Caption = 'Enable &Filtering'
          Hint = 'Enable Filtering. If checked, the demo will work more slowly'
          OnClick = miEnableFilteringClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miRecreateDB: TMenuItem
        Caption = 'Re&create the database'
        OnClick = miRecreateDBClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
end
