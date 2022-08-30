inherited StylesMultiDemoMainForm: TStylesMultiDemoMainForm
  Left = 216
  Top = 123
  Caption = 'ExpressVerticalGrid StylesMulti Demo'
  ClientHeight = 570
  ClientWidth = 798
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 798
    Height = 16
    Caption = 
      'Experiment using StyleSheets. See Help/About for more informatio' +
      'n'
  end
  object Splitter: TSplitter [1]
    Left = 217
    Top = 16
    Width = 2
    Height = 535
    MinSize = 4
  end
  inherited sbMain: TStatusBar
    Top = 551
    Width = 798
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      'In this demo you can:'
      ''
      
        '- switch between different style sheets (via the StyleSheets gri' +
        'd to the left of the mainform)'
      
        '- use a different pre-defined style sheets (just select any item' +
        ' in the Predefined Style Sheets branch of the Style Sheets grid)' +
        '.'
      ''
      
        '- select a user defined style sheet via the combobox (just selec' +
        't any item in the User Defined Style Sheets branch of the Style ' +
        'Sheets grid).'
      '- modify one or more fonts and/or colors using the Edit button'
      
        '- load/save user defined style sheets from/to ini-files using op' +
        'enfile/savefile dialogs'
      '')
  end
  object pnlLeft: TPanel [4]
    Left = 0
    Top = 16
    Width = 217
    Height = 535
    Align = alLeft
    Anchors = [akLeft]
    BevelInner = bvLowered
    BevelOuter = bvNone
    Color = 15451300
    Constraints.MinWidth = 215
    TabOrder = 0
    object gbUserDefined: TGroupBox
      Left = 1
      Top = 419
      Width = 215
      Height = 115
      Align = alBottom
      Caption = 'User Defined Style Sheets'
      TabOrder = 2
      TabStop = True
      object btnLoad: TcxButton
        Left = 11
        Top = 49
        Width = 193
        Height = 25
        Action = actLoadFromFile
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object btnSave: TcxButton
        Left = 11
        Top = 76
        Width = 193
        Height = 25
        Action = actSaveToFile
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object btnEdit: TcxButton
        Left = 11
        Top = 22
        Width = 193
        Height = 25
        Action = actEditStyleSheet
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object gbPredefined: TGroupBox
      Left = 1
      Top = 36
      Width = 215
      Height = 383
      Align = alClient
      Caption = 'Predefined Style Sheets'
      TabOrder = 1
      TabStop = True
      object vgStyleSheets: TcxVerticalGrid
        Left = 2
        Top = 15
        Width = 211
        Height = 366
        Align = alClient
        OptionsView.RowHeaderWidth = 24
        OptionsBehavior.HeaderSizing = False
        Styles.StyleSheet = StylesMultiDemoDataDM.cxVerticalGridStyleSheetDevExpress
        Styles.OnGetCategoryStyle = vgStyleSheetsStylesGetCategoryStyle
        TabOrder = 0
        OnDrawRowHeader = vgStyleSheetsDrawRowHeader
        OnItemChanged = vgStyleSheetsItemChanged
        Version = 1
        object vgStyleSheetsNone: TcxCategoryRow
          Properties.Caption = 'None'
          ID = 0
          ParentID = -1
          Index = 0
          Version = 1
        end
        object vgStyleSheetsUserDefinedStyleSheets: TcxCategoryRow
          Tag = -1
          Properties.Caption = 'User defined style sheets'
          ID = 1
          ParentID = -1
          Index = 1
          Version = 1
        end
        object vgStyleSheetsPredefinedStyleSheets: TcxCategoryRow
          Tag = -1
          Properties.Caption = 'Predefined style sheets'
          ID = 2
          ParentID = -1
          Index = 2
          Version = 1
        end
      end
    end
    object pnlCurrentStyleSheet: TPanel
      Left = 1
      Top = 1
      Width = 215
      Height = 35
      Align = alTop
      BevelOuter = bvLowered
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object cxDBVerticalGrid: TcxDBVerticalGrid [5]
    Left = 219
    Top = 16
    Width = 579
    Height = 535
    Align = alClient
    OptionsView.RowHeaderWidth = 268
    OptionsBehavior.AlwaysShowEditor = False
    OptionsBehavior.CellHints = False
    OptionsBehavior.BandSizing = False
    OptionsBehavior.RowTracking = False
    OptionsBehavior.IncSearch = True
    OptionsData.Deleting = False
    OptionsData.Inserting = False
    TabOrder = 3
    DataController.DataSource = StylesMultiDemoDataDM.dsOrders
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
  end
  inherited alMain: TActionList
    object actEditStyleSheet: TAction
      Category = 'Options'
      Caption = '&Edit Style Sheet'
      OnExecute = actEditStyleSheetExecute
      OnUpdate = actEditAndSaveStyleSheetUpdate
    end
    object actLoadFromFile: TAction
      Category = 'Options'
      Caption = '&LoadFromFile...'
      OnExecute = actLoadFromFileExecute
    end
    object actSaveToFile: TAction
      Category = 'Options'
      Caption = '&SaveToFile...'
      OnExecute = actSaveToFileExecute
      OnUpdate = actEditAndSaveStyleSheetUpdate
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 536
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 568
    Top = 8
  end
end
