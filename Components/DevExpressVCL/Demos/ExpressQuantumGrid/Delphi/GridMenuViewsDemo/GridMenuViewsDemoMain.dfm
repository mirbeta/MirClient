inherited GridMenuViewsDemoMainForm: TGridMenuViewsDemoMainForm
  Left = 88
  Top = 63
  Caption = 'ExpressQuantumGrid Grid Menu Views Demo'
  ClientHeight = 570
  ClientWidth = 848
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 848
    Caption = 
      'This demo shows use of popup menus with the grid. Click '#39'About t' +
      'his demo'#39' for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 551
    Width = 848
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 848
    Height = 535
    Align = alClient
    TabOrder = 1
    object tvOrders: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = GridMenuViewsDemoDataDM.dsOrders
      DataController.DetailKeyFieldNames = 'ProductID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skSum
          Position = spFooter
          Column = tvOrdersPaymentAmount
        end
        item
          Kind = skCount
          Column = tvOrdersDescription
        end
        item
          Format = 'Last purchase date: '
          Kind = skMax
          Column = tvOrdersPurchaseDate
        end
        item
          Kind = skCount
          Position = spFooter
          Column = tvOrdersProductID
        end
        item
          Format = 'Payment Amount = $,0.00'
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
        end
        item
          Kind = skCount
          Position = spFooter
          Column = tvOrdersDescription
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skMax
          Column = tvOrdersPaymentAmount
        end
        item
          Kind = skCount
          Column = tvOrdersCustomerID
        end
        item
          Kind = skCount
          Column = tvOrdersDescription
        end
        item
          Kind = skCount
          Column = tvOrdersProductID
        end
        item
          Kind = skMin
          Column = tvOrdersPurchaseDate
        end
        item
          Kind = skAverage
          Column = tvOrdersQuantity
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.FocusCellOnTab = True
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.GroupFooters = gfAlwaysVisible
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
        Properties.ListSource = GridMenuViewsDemoDataDM.dsCustomers
        Visible = False
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
            FieldName = 'FullName'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = dmGridCars.dsModels
        Visible = False
        GroupIndex = 0
        SortIndex = 1
        SortOrder = soAscending
        Width = 80
      end
      object tvOrdersPurchaseDate: TcxGridDBColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
        PropertiesClassName = 'TcxDateEditProperties'
        Width = 60
      end
      object tvOrdersPurchaseMonth: TcxGridDBColumn
        Caption = 'Purchase Month'
        DataBinding.FieldName = 'PurchaseMonth'
      end
      object tvOrdersPaymentType: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = GridMenuViewsDemoDataDM.PaymentTypeImages
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
        Width = 60
      end
      object tvOrdersPaymentAmount: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        PropertiesClassName = 'TcxCalcEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Options.Grouping = False
      end
      object tvOrdersDescription: TcxGridDBColumn
        DataBinding.FieldName = 'Description'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Options.Grouping = False
      end
      object tvOrdersQuantity: TcxGridDBColumn
        DataBinding.FieldName = 'Quantity'
        PropertiesClassName = 'TcxSpinEditProperties'
        Options.Grouping = False
        Width = 40
      end
    end
    object lvOrders: TcxGridLevel
      GridView = tvOrders
      MaxDetailHeight = 200
    end
  end
  inherited mmMain: TMainMenu
    Left = 472
    Top = 24
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object CustomizePopupmenus1: TMenuItem
        Caption = 'Customize Popup menus'
        object miUseBuiltInPopupMenu: TMenuItem
          AutoCheck = True
          Caption = 'Use Built-In Popup Menu'
          Checked = True
          Hint = 'Use Built-In Popup menu '
          OnClick = miUseBuiltInPopupMenuClick
        end
        object miAddCopyToClipboard: TMenuItem
          AutoCheck = True
          Caption = 'Add '#39'Copy to clipboard'#39' '
          Checked = True
          Hint = 'Add '#39'Copy to clipboard'#39' menu item to summary footer popup menus'
          OnClick = miAddCopyToClipboardClick
        end
        object miUseCustomPopupMenu: TMenuItem
          AutoCheck = True
          Caption = 'Use Custom Popup Menu'
          Checked = True
          Hint = 'Use User Popup Menu on the records'
          OnClick = miUseCustomPopupMenuClick
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object GridPopupMenu: TcxGridPopupMenu
    Grid = Grid
    PopupMenus = <
      item
        GridView = tvOrders
        HitTypes = [gvhtCell, gvhtRecord, gvhtRowIndicator]
        Index = 0
        OnPopup = GridMenuPopup
        PopupMenu = PopupMenu
      end>
    Left = 440
    Top = 24
  end
  object PopupMenu: TPopupMenu
    Left = 504
    Top = 24
    object miInsert: TMenuItem
      Caption = 'Insert'
      OnClick = miInsertClick
    end
    object miDelete: TMenuItem
      Caption = 'Delete'
      OnClick = miDeleteClick
    end
  end
end
