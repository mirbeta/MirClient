inherited SummaryGroupDemoMainForm: TSummaryGroupDemoMainForm
  Left = 300
  Top = 120
  Caption = 'ExpressQuantumGrid Summary Group Demo'
  ClientHeight = 546
  ClientWidth = 792
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 792
    Caption = 
      '  This demo shows summaries calculated for groups and presents d' +
      'ifferent ways to organize them visually.'
  end
  inherited sbMain: TStatusBar
    Top = 527
    Width = 792
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 792
    Height = 511
    Align = alClient
    TabOrder = 0
    object tvOrders: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = SummaryGroupDemoDataDM.dsOrders
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <
        item
          Links = <
            item
              Column = tvOrdersCustomerID
            end>
          SummaryItems = <
            item
              Kind = skSum
              Position = spFooter
              Column = tvOrdersPaymentAmount
            end
            item
              Format = '0'
              Kind = skCount
            end
            item
              Format = 'Last order: '
              Kind = skMax
              Column = tvOrdersPurchaseDate
            end
            item
              Kind = skCount
              Position = spFooter
              Column = tvOrdersProductID
            end
            item
              Format = '$,0.00'
              Kind = skSum
              Column = tvOrdersPaymentAmount
            end
            item
              Kind = skSum
              Position = spFooter
              Column = tvOrdersQuantity
            end
            item
              Kind = skMax
              Position = spFooter
              Column = tvOrdersPurchaseDate
            end>
        end
        item
          Links = <
            item
              Column = tvOrdersProductID
            end>
          SummaryItems = <
            item
              Format = 'Quantity: 0'
              Kind = skSum
              Column = tvOrdersQuantity
            end
            item
              Format = 'First: '
              Kind = skMin
              Column = tvOrdersPurchaseDate
            end
            item
              Format = 'Last: '
              Kind = skMax
              Column = tvOrdersPurchaseDate
            end
            item
              Kind = skSum
              Position = spFooter
              Column = tvOrdersPaymentAmount
            end
            item
              Format = '0'
              Kind = skSum
              Position = spFooter
              Column = tvOrdersQuantity
            end
            item
              Format = 'Average: $,0.00'
              Kind = skAverage
              Column = tvOrdersPaymentAmount
            end>
        end
        item
          Links = <
            item
              Column = tvOrdersPurchaseDate
            end
            item
              Column = tvOrdersPaymentType
            end>
          SummaryItems = <
            item
              Kind = skCount
            end
            item
              Format = 'Quantity: 0'
              Kind = skSum
              Column = tvOrdersQuantity
            end
            item
              Kind = skAverage
              Position = spFooter
              Column = tvOrdersPaymentAmount
            end>
        end>
      OptionsBehavior.FocusCellOnTab = True
      OptionsSelection.MultiSelect = True
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupFooters = gfVisibleWhenExpanded
      OptionsView.GroupSummaryLayout = gslAlignWithColumns
      OptionsView.Indicator = True
      Styles.Footer = SummaryGroupDemoDataDM.stGreyLight
      Styles.Group = SummaryGroupDemoDataDM.stBlueLight
      Styles.GroupByBox = SummaryGroupDemoDataDM.stBlueSky
      Styles.GroupSummary = SummaryGroupDemoDataDM.stClear
      object tvOrdersCustomerID: TcxGridDBColumn
        Caption = 'Company'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Company'
          end>
        Properties.ListSource = SummaryGroupDemoDataDM.dsCustomers
        Visible = False
        GroupIndex = 1
        SortIndex = 0
        SortOrder = soAscending
        Width = 100
      end
      object tvOrdersProductID: TcxGridDBColumn
        Caption = 'Car'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'CarName'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = SummaryGroupDemoDataDM.dsCars
        Visible = False
        GroupIndex = 0
        SortIndex = 1
        SortOrder = soAscending
        Width = 80
      end
      object tvOrdersDescription: TcxGridDBColumn
        DataBinding.FieldName = 'Description'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Options.Grouping = False
        Width = 214
      end
      object tvOrdersPurchaseDate: TcxGridDBColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
        PropertiesClassName = 'TcxDateEditProperties'
        DateTimeGrouping = dtgByMonth
        Width = 225
      end
      object tvOrdersPaymentAmount: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        PropertiesClassName = 'TcxCalcEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Options.Grouping = False
        Styles.GroupSummary = SummaryGroupDemoDataDM.stRed
        Width = 165
      end
      object tvOrdersPaymentType: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Vert = taVCenter
        Properties.Images = SummaryGroupDemoDataDM.PaymentTypeImages
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
        Width = 93
      end
      object tvOrdersQuantity: TcxGridDBColumn
        DataBinding.FieldName = 'Quantity'
        PropertiesClassName = 'TcxSpinEditProperties'
        Options.Grouping = False
        Width = 98
      end
    end
    object lvOrders: TcxGridLevel
      GridView = tvOrders
      MaxDetailHeight = 200
    end
  end
  inherited mmMain: TMainMenu
    Left = 504
    Top = 8
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miSummaries: TMenuItem
        Caption = 'Su&mmary'
        object miSelectedRecordsOnly: TMenuItem
          Caption = 'Calculate on &selected records only'
          Checked = True
          OnClick = miSelectedRecordsOnlyClick
        end
        object miIgnoreNullValues: TMenuItem
          Caption = '&Ignore Null Values when calculating'
          Checked = True
          OnClick = miIgnoreNullValuesClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miGroupSummaryLayout: TMenuItem
        Caption = 'Group Summary Layout'
        object miGroupSummaryLayoutStandard: TMenuItem
          Caption = 'gslStandard'
          GroupIndex = 2
          RadioItem = True
          OnClick = miGroupSummaryLayoutClick
        end
        object miGroupSummaryLayoutAlignWithColumns: TMenuItem
          Tag = 1
          Caption = 'gslAlignWithColumns'
          GroupIndex = 2
          RadioItem = True
          OnClick = miGroupSummaryLayoutClick
        end
        object miGroupSummaryLayoutAlignWithColumnsAndDistribute: TMenuItem
          Tag = 2
          Caption = 'gslAlignWithColumnsAndDistribute'
          GroupIndex = 2
          RadioItem = True
          OnClick = miGroupSummaryLayoutClick
        end
      end
      object miGroupFooter: TMenuItem
        Caption = 'Group Footers'
        object miGroupFooterNeverShow: TMenuItem
          Caption = 'gfInvisible'
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupFooterShowClick
        end
        object miGroupFooterShowWhenExpand: TMenuItem
          Tag = 1
          Caption = 'gfVisibleWhenExpanded'
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupFooterShowClick
        end
        object miGroupFooterAlwaysShow: TMenuItem
          Tag = 2
          Caption = 'gfAlwaysVisible'
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupFooterShowClick
        end
        object N3: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object miGroupFootersAtCarLevel: TMenuItem
          Tag = 1
          Caption = 'At Car Level'
          Checked = True
          GroupIndex = 1
          OnClick = miGroupFootersAtLevelClick
        end
        object miGroupFootersAtCompanyLevel: TMenuItem
          Tag = 2
          Caption = 'At Company level'
          Checked = True
          GroupIndex = 1
          OnClick = miGroupFootersAtLevelClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miMultiSelect: TMenuItem
        Caption = '&Multi Select'
        Checked = True
        OnClick = miMultiSelectClick
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
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = Grid
    PopupMenus = <>
    Left = 440
    Top = 8
  end
end
