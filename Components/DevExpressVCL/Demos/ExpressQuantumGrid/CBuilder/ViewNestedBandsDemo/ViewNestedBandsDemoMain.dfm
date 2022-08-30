inherited ViewNestedBandsDemoMainForm: TViewNestedBandsDemoMainForm
  Left = 34
  Top = 41
  Caption = 'ExpressQuantumGrid ViewNestedBandsDemo'
  ClientHeight = 575
  ClientWidth = 948
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 948
    Caption = 
      'Use Nested Bands with the BandedView. Experiment by changing the' +
      ' Options. Click on '#39'About this demo'#39' for more information.'
  end
  object Grid: TcxGrid [1]
    Left = 0
    Top = 16
    Width = 948
    Height = 540
    Align = alClient
    TabOrder = 0
    object bvOrders: TcxGridDBBandedTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ViewNestedBandsDemoDataDM.dsOrders
      DataController.MultiThreadedOptions.Sorting = bFalse
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnsQuickCustomization = True
      OptionsCustomize.BandsQuickCustomization = True
      OptionsView.FocusRect = False
      Styles.BandHeader = ViewNestedBandsDemoDataDM.cxStyle31
      Bands = <
        item
          Caption = 'Order'
          Styles.Header = ViewNestedBandsDemoDataDM.cxStyle35
        end
        item
          Caption = 'Car'
          Styles.Header = ViewNestedBandsDemoDataDM.cxStyle35
          Width = 605
        end
        item
          Caption = 'Customer'
          Styles.Header = ViewNestedBandsDemoDataDM.cxStyle35
          Width = 774
        end
        item
          Caption = 'MPG'
          Position.BandIndex = 1
          Position.ColIndex = 3
          Width = 108
        end
        item
          Caption = 'Transmission'
          Position.BandIndex = 1
          Position.ColIndex = 2
          Width = 138
        end
        item
          Caption = 'Engine'
          Position.BandIndex = 1
          Position.ColIndex = 1
          Width = 137
        end
        item
          Caption = 'Model'
          Position.BandIndex = 1
          Position.ColIndex = 0
          Width = 222
        end
        item
          Caption = 'Contact'
          Position.BandIndex = 2
          Position.ColIndex = 1
          Width = 377
        end
        item
          Caption = 'Primary Info'
          Position.BandIndex = 2
          Position.ColIndex = 0
          Width = 397
        end>
      object clnCustomerID: TcxGridDBBandedColumn
        Caption = 'Customer'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.ImmediatePost = True
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Name'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Options.CellMerging = True
        Options.SortByDisplayText = isbtOn
        SortIndex = 2
        SortOrder = soAscending
        Width = 173
        Position.BandIndex = 8
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnCustomerCompany: TcxGridDBBandedColumn
        Caption = 'Company'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Company'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 124
        Position.BandIndex = 8
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnCustomerAddres: TcxGridDBBandedColumn
        Caption = 'Address'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Address'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 160
        Position.BandIndex = 7
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
      object clnCustomerFax: TcxGridDBBandedColumn
        Caption = 'Fax'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'FaxPhone'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 96
        Position.BandIndex = 7
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnCustomerPhone: TcxGridDBBandedColumn
        Caption = 'Phone'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'HomePhone'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 89
        Position.BandIndex = 7
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnCustomerOccupation: TcxGridDBBandedColumn
        Caption = 'Occupation'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Occupation'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 100
        Position.BandIndex = 8
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object clnCustomerZipCode: TcxGridDBBandedColumn
        Caption = 'ZipCode'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'ZipCode'
          end>
        Properties.ListSource = ViewNestedBandsDemoDataDM.dsCustomers
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 77
        Position.BandIndex = 7
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object clnOrdersProductID: TcxGridDBBandedColumn
        Caption = 'Car'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.ImmediatePost = True
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'FullName'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Options.CellMerging = True
        Options.SortByDisplayText = isbtOn
        SortIndex = 1
        SortOrder = soAscending
        Position.BandIndex = 6
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnCarTorque: TcxGridDBBandedColumn
        Caption = 'Torque'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Torque'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Position.BandIndex = 5
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnCarCyl: TcxGridDBBandedColumn
        Caption = 'Cyl'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Cilinders'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Position.BandIndex = 5
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnCarHP: TcxGridDBBandedColumn
        Caption = 'HP'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Horsepower'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Position.BandIndex = 5
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object clnCarMPG_City: TcxGridDBBandedColumn
        Caption = 'City'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'MPG City'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 44
        Position.BandIndex = 3
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnCarMPG_Highway: TcxGridDBBandedColumn
        Caption = 'Highway'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'MPG Highway'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Editing = False
        Options.CellMerging = True
        Width = 64
        Position.BandIndex = 3
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnCarTransmissSpeedCount: TcxGridDBBandedColumn
        Caption = 'Speed Count'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Transmission Speeds'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Filtering = False
        Options.CellMerging = True
        Width = 76
        Position.BandIndex = 4
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnCarTransMissAuto: TcxGridDBBandedColumn
        Caption = 'Automatic'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'TransmissionTypeName'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Properties.ReadOnly = True
        Options.Filtering = False
        Options.CellMerging = True
        Width = 80
        Position.BandIndex = 4
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnPurchaseDate: TcxGridDBBandedColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
        PropertiesClassName = 'TcxDateEditProperties'
        Options.CellMerging = True
        SortIndex = 0
        SortOrder = soAscending
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object clnPaymentType: TcxGridDBBandedColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = ViewNestedBandsDemoDataDM.PaymentTypeImages
        Properties.Items = <
          item
            Description = 'Cash'
            ImageIndex = 0
            Value = 'Cash'
          end
          item
            Description = 'Visa'
            ImageIndex = 1
            Value = 'Visa'
          end
          item
            Description = 'Master'
            ImageIndex = 2
            Value = 'Master'
          end
          item
            Description = 'American Express'
            ImageIndex = 3
            Value = 'AmEx'
          end>
        Options.CellMerging = True
        SortIndex = 3
        SortOrder = soAscending
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object clnQuantity: TcxGridDBBandedColumn
        DataBinding.FieldName = 'Quantity'
        Position.BandIndex = 0
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object clnPaymentAmount: TcxGridDBBandedColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        Position.BandIndex = 0
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
    end
    object lvOrders: TcxGridLevel
      GridView = bvOrders
    end
  end
  inherited sbMain: TStatusBar
    Top = 556
    Width = 948
  end
  inherited mmMain: TMainMenu
    Left = 504
    Top = 24
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miNestedBands: TMenuItem
        AutoCheck = True
        Caption = '&Nested Bands'
        Checked = True
        Hint = 'Enables nested bands'
        OnClick = miNestedBandsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miBandsQuickCustomization: TMenuItem
        AutoCheck = True
        Caption = '&Bands quick customization'
        Checked = True
        Hint = 
          'Enables  bands to be shown or hidden in the grid by clicking on ' +
          'items in the list (click the indicator located to the left of th' +
          'e band headers area)'
        OnClick = miBandsQuickCustomizationClick
      end
      object miColumnsQuickCustomization: TMenuItem
        AutoCheck = True
        Caption = '&Columns quick customization'
        Checked = True
        Hint = 
          'Enables  columns to be shown or hidden in the grid by clicking o' +
          'n items in the list (click the indicator located to the left of ' +
          'the column headers area)'
        OnClick = miColumnsQuickCustomizationClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miCellMerging: TMenuItem
        AutoCheck = True
        Caption = 'Cell &merging'
        Checked = True
        Hint = 'Merges cells with the same data'
        OnClick = miCellMergingClick
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
end
