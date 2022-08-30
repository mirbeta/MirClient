inherited frmProductEdit: TfrmProductEdit
  Width = 1078
  Height = 679
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 1078
    Height = 559
    object edStartDate: TcxDBDateEdit [0]
      Left = 256
      Top = 17
      DataBinding.DataField = 'ProductionStart'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 0
      Width = 198
    end
    object edAvailable: TcxDBCheckBox [1]
      Left = 256
      Top = 56
      DataBinding.DataField = 'Available'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 1
      Transparent = True
    end
    object edSupport: TcxDBLookupComboBox [2]
      Left = 256
      Top = 88
      DataBinding.DataField = 'SupportId'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsEmployeesHelper
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Width = 198
    end
    object edEngineer: TcxDBLookupComboBox [3]
      Left = 256
      Top = 127
      DataBinding.DataField = 'EngineerId'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.KeyFieldNames = 'Id'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsEmployeesHelper
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 3
      Width = 198
    end
    object edCategory: TcxDBLookupComboBox [4]
      Left = 256
      Top = 166
      DataBinding.DataField = 'Category'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Category'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsCategoriesSpr
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 4
      Width = 198
    end
    object edCurrentInventory: TcxDBSpinEdit [5]
      Left = 256
      Top = 225
      DataBinding.DataField = 'CurrentInventory'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.MaxValue = 1000000.000000000000000000
      Properties.SpinButtons.Position = sbpHorzRight
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 5
      Width = 198
    end
    object edBackorder: TcxDBSpinEdit [6]
      Left = 256
      Top = 264
      DataBinding.DataField = 'Backorder'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.MaxValue = 1000000.000000000000000000
      Properties.SpinButtons.Position = sbpHorzRight
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 6
      Width = 198
    end
    object edCost: TcxDBCurrencyEdit [7]
      Left = 256
      Top = 303
      DataBinding.DataField = 'Cost'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.DisplayFormat = '$,0.00;$-,0.00'
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 7
      Width = 198
    end
    object edSalePrice: TcxDBCurrencyEdit [8]
      Left = 256
      Top = 342
      DataBinding.DataField = 'SalePrice'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.DisplayFormat = '$,0.00;$-,0.00'
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 8
      Width = 198
    end
    object edRetailPrice: TcxDBCurrencyEdit [9]
      Left = 256
      Top = 381
      DataBinding.DataField = 'RetailPrice'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.DisplayFormat = '$,0.00;$-,0.00'
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 9
      Width = 198
    end
    object edDescription: TcxDBRichEdit [10]
      Left = 59
      Top = 449
      DataBinding.DataField = 'Description'
      DataBinding.DataSource = DM.dsProduct
      ParentFont = False
      Properties.ScrollBars = ssVertical
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 10
      Height = 93
      Width = 395
    end
    object pdfProduct: TdxPDFViewer [11]
      Left = 488
      Top = 17
      Width = 573
      Height = 525
      LookAndFeel.NativeStyle = False
      OptionsFindPanel.DisplayMode = fpdmNever
      OnZoomFactorChanged = pdfProductZoomFactorChanged
    end
    inherited dxLayoutGroup2: TdxLayoutGroup
      LayoutDirection = ldHorizontal
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 395
      ButtonOptions.Buttons = <>
      ScrollOptions.Vertical = smAuto
      ShowBorder = False
      Index = 0
    end
    object liProductionStartDate: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'PRODUCTION START DATE'
      Control = edStartDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 218
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liAvailableForSale: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'AVAILABLE FOR SALE'
      Control = edAvailable
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liSupportEngineer: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'SUPPORT ENGINEER'
      Control = edSupport
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liProductEngineer: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'PRODUCT ENGINEER'
      Control = edEngineer
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liCategory: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'CATEGORY'
      Control = edCategory
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 5
    end
    object liCurrentInventory: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'CURRENT INVENTORY'
      Control = edCurrentInventory
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object liBackorders: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'BACKORDERS'
      Control = edBackorder
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object liCost: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'COST'
      Control = edCost
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object liSalePrice: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'SALE PRICE'
      Control = edSalePrice
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 263
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object liRetailPrice: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'RETAIL PRICE'
      Control = edRetailPrice
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 263
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object liDescription: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignVert = avClient
      CaptionOptions.Text = 'DESCRIPTION'
      CaptionOptions.Layout = clTop
      Control = edDescription
      ControlOptions.MinHeight = 70
      ControlOptions.OriginalHeight = 173
      ControlOptions.OriginalWidth = 460
      ControlOptions.ShowBorder = False
      Index = 11
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Height = 10
      SizeOptions.Width = 14
      Index = 1
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      Control = pdfProduct
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 605
      ControlOptions.OriginalWidth = 448
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 559
    Width = 1078
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 1074
      object btnSave: TcxButton [0]
        Left = 344
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Save'
        ModalResult = 1
        OptionsImage.ImageIndex = 25
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 0
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnSaveClick
      end
      object btnCancel: TcxButton [1]
        Left = 439
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Cancel'
        ModalResult = 2
        OptionsImage.ImageIndex = 21
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 1
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnCancelClick
      end
      object btnZoomIn: TcxButton [2]
        Left = 550
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Zoom In'
        OptionsImage.ImageIndex = 27
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnZoomInClick
      end
      object btnZoomOut: TcxButton [3]
        Left = 645
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Zoom Out'
        OptionsImage.ImageIndex = 28
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 3
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnZoomOutClick
      end
      inherited dxLayoutControl2Group_Root: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup4: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = btnSave
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem18: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = btnCancel
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem19: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = btnZoomIn
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 3
      end
      object dxLayoutItem20: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = btnZoomOut
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 4
      end
      object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Separator'
        Index = 2
      end
    end
  end
end
