inherited StylesSimpleDemoMainForm: TStylesSimpleDemoMainForm
  Left = 347
  Top = 208
  Caption = 'ExpressVerticalGrid StylesSimple Demo'
  ClientHeight = 566
  ClientWidth = 709
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 709
    Caption = 
      'Change the style of various ExpressVerticalGrid display elements' +
      '. See Help/About on how to use the Styles Dialog'
  end
  inherited sbMain: TStatusBar
    Top = 547
    Width = 709
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      
        'This demo provides a non-modal Styles Dialog that allows you to ' +
        'change the styles of an ExpressVerticalGrid'#39's display elements.'
      ''
      'The dialog can be used in two ways:'
      ''
      
        '1) to assign a style to a grid element. Select a Style Name cell' +
        ' in the dialog and use its dropdown combo.'
      ''
      
        '2) change one of the styles. Select a Style Name cell and use it' +
        's ellipsis button. Note: this will change the look of every grid' +
        ' element assigned to the style.'
      ''
      
        'Use the '#39'Restore Defaults'#39' button to reverse all your changes, t' +
        'hus allowing you to experiment safely.'
      ''
      '')
  end
  object cxDBVerticalGrid: TcxDBVerticalGrid [3]
    Left = 0
    Top = 32
    Width = 709
    Height = 515
    Align = alClient
    LayoutStyle = lsBandsView
    OptionsView.GridLineColor = 14869218
    OptionsView.RowHeaderWidth = 204
    OptionsBehavior.AlwaysShowEditor = False
    OptionsBehavior.CellHints = False
    OptionsBehavior.BandSizing = False
    OptionsBehavior.IncSearch = True
    Styles.StyleSheet = StylesSimpleDemoDataDM.UserStyleSheet
    TabOrder = 2
    DataController.DataSource = StylesSimpleDemoDataDM.dsOrders
    Version = 1
    object cxDBVerticalGridOrderInfo: TcxCategoryRow
      Properties.Caption = 'Order Info'
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridPurchaseDate: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'PurchaseDate'
      ID = 1
      ParentID = 0
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridTime: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxTimeEditProperties'
      Properties.DataBinding.FieldName = 'Time'
      ID = 2
      ParentID = 0
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridPaymentType: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'PaymentType'
      ID = 3
      ParentID = 0
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridPaymentAmount: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'PaymentAmount'
      ID = 4
      ParentID = 0
      Index = 3
      Version = 1
    end
    object cxDBVerticalGridQuantity: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Quantity'
      ID = 5
      ParentID = 0
      Index = 4
      Version = 1
    end
    object cxDBVerticalGridCustomerInfo: TcxCategoryRow
      Properties.Caption = 'Customer Info'
      ID = 6
      ParentID = -1
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridCommonCustomerInfo: TcxCategoryRow
      Properties.Caption = 'Common Customer Info'
      ID = 7
      ParentID = 6
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridFirstName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'FirstName'
      ID = 8
      ParentID = 7
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridLastName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'LastName'
      ID = 9
      ParentID = 7
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridSpouse: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Spouse'
      ID = 10
      ParentID = 7
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridPrefix: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Prefix'
      ID = 11
      ParentID = 7
      Index = 3
      Version = 1
    end
    object cxDBVerticalGridTitle: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Title'
      ID = 12
      ParentID = 7
      Index = 4
      Version = 1
    end
    object cxDBVerticalGridCustomerContacts: TcxCategoryRow
      Properties.Caption = 'Customer Contacts'
      ID = 13
      ParentID = 6
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridPhonesAndFaxes: TcxCategoryRow
      Properties.Caption = 'Phones&Faxes'
      ID = 14
      ParentID = 13
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridFaxPhone: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'FaxPhone'
      ID = 15
      ParentID = 14
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridHomePhone: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'HomePhone'
      ID = 16
      ParentID = 14
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridCategoryAddress: TcxCategoryRow
      Properties.Caption = 'Address'
      ID = 17
      ParentID = 13
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridState: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'State'
      ID = 18
      ParentID = 17
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridCity: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'City'
      ID = 19
      ParentID = 17
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridAddress: TcxDBEditorRow
      Properties.Caption = 'Street'
      Properties.DataBinding.FieldName = 'Address'
      ID = 20
      ParentID = 17
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridZipCode: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'ZipCode'
      ID = 21
      ParentID = 17
      Index = 3
      Version = 1
    end
    object cxDBVerticalGridEmail: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Email'
      ID = 22
      ParentID = 13
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridOccupation: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Occupation'
      ID = 23
      ParentID = 6
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridCustomer: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Customer'
      ID = 24
      ParentID = 6
      Index = 3
      Version = 1
    end
    object cxDBVerticalGridCompany: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Company'
      ID = 25
      ParentID = 6
      Index = 4
      Version = 1
    end
    object cxDBVerticalGridCarInfo: TcxCategoryRow
      Properties.Caption = 'CarInfo'
      ID = 26
      ParentID = -1
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridCar: TcxCategoryRow
      Properties.Caption = 'Car'
      ID = 27
      ParentID = 26
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridTrademark: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Trademark'
      ID = 28
      ParentID = 27
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridModel: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Model'
      ID = 29
      ParentID = 27
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridMPG: TcxCategoryRow
      Properties.Caption = 'MPG'
      ID = 30
      ParentID = 26
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridMPG_City: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'MPG_City'
      ID = 31
      ParentID = 30
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridMPG_Highway: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'MPG_Highway'
      ID = 32
      ParentID = 30
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridEngine: TcxCategoryRow
      Properties.Caption = 'Engine'
      ID = 33
      ParentID = 26
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridHP: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'HP'
      ID = 34
      ParentID = 33
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridLiter: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Liter'
      ID = 35
      ParentID = 33
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridCyl: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Cyl'
      ID = 36
      ParentID = 33
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridNotes: TcxCategoryRow
      Properties.Caption = 'Notes'
      ID = 37
      ParentID = 26
      Index = 3
      Version = 1
    end
    object cxDBVerticalGridCars_Description: TcxDBEditorRow
      Properties.Caption = 'Description'
      Properties.DataBinding.FieldName = 'Cars_Description'
      ID = 38
      ParentID = 37
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridTransmission: TcxCategoryRow
      Properties.Caption = 'Transmission'
      ID = 39
      ParentID = 26
      Index = 4
      Version = 1
    end
    object cxDBVerticalGridTransmissSpeedCount: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'TransmissSpeedCount'
      ID = 40
      ParentID = 39
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridTransmissAutomatic: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'TransmissAutomatic'
      ID = 41
      ParentID = 39
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridOthers: TcxCategoryRow
      Properties.Caption = 'Others'
      ID = 42
      ParentID = 26
      Index = 5
      Version = 1
    end
    object cxDBVerticalGridCategory: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Category'
      ID = 43
      ParentID = 42
      Index = 0
      Version = 1
    end
    object cxDBVerticalGridHyperlink: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxHyperLinkEditProperties'
      Properties.EditProperties.SingleClick = True
      Properties.DataBinding.FieldName = 'Hyperlink'
      ID = 44
      ParentID = 42
      Index = 1
      Version = 1
    end
    object cxDBVerticalGridPrice: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Price'
      ID = 45
      ParentID = 42
      Index = 2
      Version = 1
    end
    object cxDBVerticalGridPicture: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxBlobEditProperties'
      Properties.EditProperties.BlobEditKind = bekPict
      Properties.EditProperties.PictureGraphicClassName = 'TJPEGImage'
      Properties.DataBinding.FieldName = 'Picture'
      ID = 46
      ParentID = 42
      Index = 3
      Version = 1
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object ShowStyleDialog1: TMenuItem [0]
        Action = actShowStyleDialog
      end
    end
  end
  inherited alMain: TActionList
    object actShowStyleDialog: TAction
      Category = 'Options'
      Caption = 'Show &StyleDialog...'
      Checked = True
      OnExecute = actShowStyleDialogExecute
    end
  end
end
