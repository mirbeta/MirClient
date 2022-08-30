inherited EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm
  Left = 55
  Top = 101
  Caption = 'ExpressQuantumGrid EditorsInPlaceDemo'
  ClientHeight = 573
  ClientWidth = 959
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 959
    Caption = 
      'This demo shows various column editors. Click '#39'About this demo'#39' ' +
      'for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 554
    Width = 959
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 959
    Height = 538
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailFrameColor = 15451300
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnFocusedViewChanged = GridFocusedViewChanged
    object cvCustomers: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = EditorsInPlaceDemoDataDM.dsCustomers
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CellAutoHeight = True
      OptionsView.SeparatorColor = 12937777
      Styles.StyleSheet = GridCardViewStyleSheetDevExpress
      object cvCustomersCompany: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Company'
        Position.BeginsLayer = True
      end
      object cvCustomersState: TcxGridDBCardViewRow
        DataBinding.FieldName = 'State'
        PropertiesClassName = 'TcxMRUEditProperties'
        Properties.LookupItems.Strings = (
          'VA'
          'NY'
          'IL'
          'GA'
          'CA'
          'OK'
          'OH'
          'CT'
          'MI')
        Properties.OnButtonClick = cvCustomersStatePropertiesButtonClick
        Position.BeginsLayer = True
      end
      object cvCustomersCity: TcxGridDBCardViewRow
        DataBinding.FieldName = 'City'
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Default = True
          end
          item
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = cvCustomersCityPropertiesButtonClick
        Position.BeginsLayer = True
      end
      object cvCustomersPrefix: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Prefix'
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Ms.'
          'Mr.'
          'Mrs.')
        Position.BeginsLayer = True
      end
      object cvCustomersFirstName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FirstName'
        Position.BeginsLayer = True
      end
      object cvCustomersLastName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LastName'
        Position.BeginsLayer = True
      end
      object cvCustomersCustomer: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Customer'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Properties.ValueChecked = 'Y'
        Properties.ValueUnchecked = 'N'
        Position.BeginsLayer = True
      end
      object cvCustomersZipCode: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ZipCode'
        PropertiesClassName = 'TcxMaskEditProperties'
        Properties.MaskKind = emkRegExprEx
        Properties.EditMask = '\d\d\d\d\d'
        Position.BeginsLayer = True
      end
      object cvCustomersAddress: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Address'
        Position.BeginsLayer = True
      end
      object cvCustomersFaxPhone: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FaxPhone'
        PropertiesClassName = 'TcxMaskEditProperties'
        Properties.MaskKind = emkRegExprEx
        Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - \d\d - \d\d'
        Position.BeginsLayer = True
      end
      object cvCustomersDescription: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Description'
        PropertiesClassName = 'TcxMemoProperties'
        Properties.WantReturns = False
        Position.BeginsLayer = True
      end
    end
    object cvOrders: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = EditorsInPlaceDemoDataDM.dsOrders
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CellAutoHeight = True
      OptionsView.SeparatorColor = 12937777
      Styles.StyleSheet = GridCardViewStyleSheetDevExpress
      object cvOrdersCustomerID: TcxGridDBCardViewRow
        Caption = 'Customer'
        DataBinding.FieldName = 'CustomerID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Company'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = EditorsInPlaceDemoDataDM.dsCustomers
        Position.BeginsLayer = True
      end
      object cvOrdersProductID: TcxGridDBCardViewRow
        Caption = 'Car'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Car'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = EditorsInPlaceDemoDataDM.dsCars
        Position.BeginsLayer = True
      end
      object cvOrdersPurchaseDate: TcxGridDBCardViewRow
        DataBinding.FieldName = 'PurchaseDate'
        Position.BeginsLayer = True
      end
      object cvOrdersTime: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Time'
        PropertiesClassName = 'TcxTimeEditProperties'
        Position.BeginsLayer = True
      end
      object cvOrdersPaymentType: TcxGridDBCardViewRow
        DataBinding.FieldName = 'PaymentType'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = EditorsInPlaceDemoDataDM.PaymentTypeImages
        Properties.Items = <
          item
            Description = 'American Express'
            ImageIndex = 3
            Value = 'AmEx'
          end
          item
            Description = 'Master Card'
            ImageIndex = 2
            Value = 'Master'
          end
          item
            Description = 'Visa Card'
            ImageIndex = 1
            Value = 'Visa'
          end
          item
            Description = 'Cash'
            ImageIndex = 0
            Value = 'Cash'
          end>
        Position.BeginsLayer = True
      end
      object cvOrdersQuantity: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Quantity'
        PropertiesClassName = 'TcxSpinEditProperties'
        Position.BeginsLayer = True
      end
      object cvOrdersPaymentAmount: TcxGridDBCardViewRow
        DataBinding.FieldName = 'PaymentAmount'
        PropertiesClassName = 'TcxCalcEditProperties'
        Position.BeginsLayer = True
      end
      object cvOrdersDescription: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Description'
        PropertiesClassName = 'TcxMemoProperties'
        Position.BeginsLayer = True
      end
    end
    object cvCars: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      OnCustomDrawCell = cvCarsCustomDrawCell
      DataController.DataSource = dmGridCars.dsModels
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 400
      OptionsView.CellAutoHeight = True
      OptionsView.SeparatorColor = 12937777
      Styles.StyleSheet = GridCardViewStyleSheetDevExpress
      object cvCarsCategory: TcxGridDBCardViewRow
        Caption = 'Category'
        DataBinding.FieldName = 'CategoryID'
        RepositoryItem = dmGridCars.EditRepositoryCategoryLookup
        Position.BeginsLayer = True
      end
      object cvCarsCar: TcxGridDBCardViewRow
        Caption = 'Car'
        DataBinding.FieldName = 'FullName'
        Position.BeginsLayer = True
      end
      object cvCarsPicture: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Image'
        RepositoryItem = dmGridCars.EditRepositoryImage
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvBarCode: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Hyperlink'
        PropertiesClassName = 'TdxBarCodeProperties'
        Properties.BarCodeSymbologyClassName = 'TdxBarCodeQRCodeSymbology'
        Properties.FitMode = ifmProportionalStretch
        Properties.ShowText = False
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Position.Width = 100
      end
      object cvCarsInfo: TcxGridDBCardViewRow
        Caption = 'Info'
        PropertiesClassName = 'TcxPopupEditProperties'
        Properties.PopupControl = EditorsInPlaceDemoCarsForm.pnlCarInfo
        Properties.PopupSizeable = False
        Position.BeginsLayer = True
      end
      object cvCarsHyperlink: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Hyperlink'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BeginsLayer = True
      end
    end
    object lvCustomers: TcxGridLevel
      Caption = 'Customers'
      GridView = cvCustomers
    end
    object lvOrders: TcxGridLevel
      Caption = 'Orders'
      GridView = cvOrders
    end
    object lvCars: TcxGridLevel
      Caption = 'Cars'
      GridView = cvCars
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miShowEditButtons: TMenuItem
        Caption = 'Show Edit Buttons'
        object miEditButtonsNever: TMenuItem
          Caption = 'Focused &Field Only'
          Checked = True
          Hint = 
            'The editor buttons are displayed only for the focused column in ' +
            'a table view.'
          RadioItem = True
          OnClick = miEditButtonsClick
        end
        object miEditButtonsForFocusedRecord: TMenuItem
          Tag = 1
          Caption = 'Focused &Card Only'
          Hint = 'The editor buttons are visible for the focused record.'
          RadioItem = True
          OnClick = miEditButtonsClick
        end
        object miEditButtonsAlways: TMenuItem
          Tag = 2
          Caption = '&Always'
          Hint = 'The editor buttons are always visible within the current view'
          RadioItem = True
          OnClick = miEditButtonsClick
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
end
