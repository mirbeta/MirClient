inherited frmCustomers: TfrmCustomers
  Width = 1099
  Height = 690
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 1099
    Height = 570
    object cxGridCustomers: TcxGrid [0]
      Left = 59
      Top = 46
      Width = 1006
      Height = 180
      TabOrder = 0
      object gvCustomers: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        FindPanel.DisplayMode = fpdmManual
        OnCellDblClick = gvCustomersCellDblClick
        OnFocusedRecordChanged = gvCustomersFocusedRecordChanged
        DataController.DataSource = DM.dsCustomers
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <
          item
            Kind = skCount
            OnGetText = gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText
            FieldName = 'Id'
            Column = gvCustomersName
          end
          item
            Format = '$,0.00;-$,0.00'
            Kind = skSum
            OnGetText = gvCustomersTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems1GetText
            FieldName = 'Total2015'
            Column = gvCustomersMonthlySales
          end>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsData.CancelOnExit = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 40
        OptionsView.Footer = True
        OptionsView.FooterAutoHeight = True
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Footer = DM.cxStyle1
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        OnCustomDrawFooterCell = gvCustomersCustomDrawFooterCell
        object gvCustomersName: TcxGridDBColumn
          DataBinding.FieldName = 'Name'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 150
        end
        object gvCustomersHomeOffice_Line: TcxGridDBColumn
          Caption = 'Address'
          DataBinding.FieldName = 'HomeOffice_Line'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 138
        end
        object gvCustomersHomeOffice_City: TcxGridDBColumn
          Caption = 'City'
          DataBinding.FieldName = 'HomeOffice_City'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 142
        end
        object gvCustomersHomeOffice_State: TcxGridDBColumn
          Caption = 'State'
          DataBinding.FieldName = 'HomeOffice_State'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.Alignment.Horz = taCenter
          Properties.IncrementalFiltering = False
          Properties.KeyFieldNames = 'ID'
          Properties.ListColumns = <
            item
              FieldName = 'ShortName'
            end>
          Properties.ListSource = DM.dsStatesSpr
          Properties.ReadOnly = True
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 55
        end
        object gvCustomersHomeOffice_ZipCode: TcxGridDBColumn
          Caption = 'Zip Code'
          DataBinding.FieldName = 'HomeOffice_ZipCode'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 97
        end
        object gvCustomersFax: TcxGridDBColumn
          DataBinding.FieldName = 'Fax'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 95
        end
        object gvCustomersPhone: TcxGridDBColumn
          DataBinding.FieldName = 'Phone'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 97
        end
        object gvCustomersMonthlySales: TcxGridDBColumn
          Caption = 'Monthly Sales (FY2015)'
          DataBinding.FieldName = 'Id'
          PropertiesClassName = 'TdxLookupSparklineProperties'
          Properties.Series.MinMaxRangeType = mmrtZeroBasedAuto
          Properties.Series = <
            item
              DataBinding.FieldName = 'Total'
              Color = 10834223
              SeriesType = stBar
            end>
          Properties.LookupDataSource = DM.dsCustomerSales2015
          Properties.LookupKeyFieldName = 'CustomerID'
          OnGetFilterValues = gvCustomersNameGetFilterValues
          BestFitMaxWidth = 200
          HeaderAlignmentHorz = taCenter
          Width = 183
        end
      end
      object cxGridCustomersLevel1: TcxGridLevel
        GridView = gvCustomers
      end
    end
    object lbSelectedCustomer: TcxLabel [1]
      Left = 59
      Top = 258
      Caption = 'lbSelectedCustomer'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -19
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.TextColor = clNavy
      Style.IsFontAssigned = True
      Properties.ShowAccelChar = False
      Transparent = True
    end
    object cmbEmployeesView: TcxComboBox [2]
      Left = 241
      Top = 256
      ParentFont = False
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Contacts'
        'Stores')
      Properties.OnChange = cmbEmployeesViewPropertiesChange
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -19
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Text = 'Contacts'
      Width = 114
    end
    object cxGridEmployees: TcxGrid [3]
      Left = 59
      Top = 299
      Width = 1006
      Height = 300
      TabOrder = 3
      object gvEmployees: TcxGridDBLayoutView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsCustomerEmployees
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsBehavior.ItemHotTrack = False
        OptionsBehavior.RecordScrollMode = rsmByRecord
        OptionsData.CancelOnExit = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsView.NavigatorOffset = 20
        OptionsView.RecordCaption.Visible = False
        OptionsView.ViewMode = lvvmSingleRow
        object gvEmployeesItemPicture: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'Picture'
          PropertiesClassName = 'TcxImageProperties'
          Properties.FitMode = ifmProportionalStretch
          Properties.GraphicClassName = 'TdxSmartImage'
          LayoutItem = cxGridLayoutItem1
          Options.Filtering = False
          Options.Focusing = False
        end
        object gvEmployeesItemFullName: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'FullName'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          LayoutItem = gvEmployeesLayoutItem2
          Options.Filtering = False
          Options.Focusing = False
        end
        object gvEmployeesItemPosition: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'Position'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          LayoutItem = gvEmployeesLayoutItem3
          Options.Filtering = False
          Options.Focusing = False
        end
        object gvEmployeesItemPhone: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'MobilePhone'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          LayoutItem = gvEmployeesLayoutItem4
          Options.Filtering = False
          Options.Focusing = False
        end
        object gvEmployeesItemCity: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'CustomerStoreId'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.Alignment.Horz = taCenter
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'Address_City'
            end>
          Properties.ListSource = DM.dsCustomerStores
          LayoutItem = gvEmployeesLayoutItem5
        end
        object gvEmployeesGroup_Root: TdxLayoutGroup
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Template Card'
          ButtonOptions.Buttons = <>
          Hidden = True
          ShowBorder = False
          Index = -1
        end
        object cxGridLayoutItem1: TcxGridLayoutItem
          Parent = gvEmployeesGroup_Root
          AlignHorz = ahClient
          AlignVert = avClient
          CaptionOptions.Visible = False
          SizeOptions.Height = 130
          SizeOptions.Width = 100
          Index = 0
        end
        object gvEmployeesLayoutItem2: TcxGridLayoutItem
          Parent = gvEmployeesGroup_Root
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Visible = False
          Index = 1
        end
        object gvEmployeesLayoutItem3: TcxGridLayoutItem
          Parent = gvEmployeesGroup_Root
          CaptionOptions.Visible = False
          Index = 2
        end
        object gvEmployeesLayoutItem4: TcxGridLayoutItem
          Parent = gvEmployeesGroup_Root
          CaptionOptions.Visible = False
          Index = 3
        end
        object gvEmployeesLayoutItem5: TcxGridLayoutItem
          Parent = gvEmployeesGroup_Root
          CaptionOptions.Visible = False
          Index = 4
        end
      end
      object gvStores: TcxGridDBLayoutView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsCustomerStores
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsBehavior.ItemHotTrack = False
        OptionsBehavior.RecordScrollMode = rsmByPixel
        OptionsView.NavigatorOffset = 20
        OptionsView.RecordCaption.Visible = False
        OptionsView.ViewMode = lvvmSingleRow
        object gvStoresAddress_City: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'Address_City'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          LayoutItem = gvStoresLayoutItem4
          Options.Editing = False
          Options.Focusing = False
          Styles.Content = DM.cxStyle1
        end
        object gvStoresAddress_Full: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'Address_Full'
          PropertiesClassName = 'TcxMemoProperties'
          LayoutItem = gvStoresLayoutItem16
          Options.Editing = False
          Options.Focusing = False
          Styles.Content = DM.cxStyle2
        end
        object gvStoresCrestId: TcxGridDBLayoutViewItem
          DataBinding.FieldName = 'CrestId'
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.Images = DM.ilCrests
          Properties.ImmediateDropDownWhenKeyPressed = False
          Properties.Items = <
            item
              ImageIndex = 0
              Value = 1
            end
            item
              ImageIndex = 1
              Value = 2
            end
            item
              ImageIndex = 2
              Value = 3
            end
            item
              ImageIndex = 3
              Value = 4
            end
            item
              ImageIndex = 4
              Value = 5
            end
            item
              ImageIndex = 5
              Value = 6
            end
            item
              ImageIndex = 6
              Value = 7
            end
            item
              ImageIndex = 7
              Value = 8
            end
            item
              ImageIndex = 8
              Value = 9
            end
            item
              ImageIndex = 9
              Value = 10
            end
            item
              ImageIndex = 10
              Value = 11
            end
            item
              ImageIndex = 11
              Value = 12
            end
            item
              ImageIndex = 12
              Value = 13
            end
            item
              ImageIndex = 13
              Value = 14
            end
            item
              ImageIndex = 14
              Value = 15
            end
            item
              ImageIndex = 15
              Value = 16
            end
            item
              ImageIndex = 16
              Value = 17
            end
            item
              ImageIndex = 17
              Value = 18
            end
            item
              ImageIndex = 18
              Value = 19
            end
            item
              ImageIndex = 19
              Value = 20
            end>
          Properties.PopupAlignment = taCenter
          Properties.ReadOnly = True
          Properties.ShowDescriptions = False
          LayoutItem = cxGridLayoutItem2
          Options.Editing = False
          Options.Focusing = False
        end
        object gvStoresGroup_Root: TdxLayoutGroup
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Template Card'
          ButtonOptions.Buttons = <>
          Hidden = True
          ShowBorder = False
          Index = -1
        end
        object gvStoresLayoutItem4: TcxGridLayoutItem
          Parent = gvStoresGroup_Root
          CaptionOptions.Visible = False
          SizeOptions.Width = 127
          Index = 1
        end
        object gvStoresLayoutItem16: TcxGridLayoutItem
          Parent = gvStoresGroup_Root
          CaptionOptions.Visible = False
          SizeOptions.Height = 51
          SizeOptions.Width = 130
          Index = 2
        end
        object cxGridLayoutItem2: TcxGridLayoutItem
          Parent = gvStoresGroup_Root
          CaptionOptions.Visible = False
          SizeOptions.Width = 126
          Index = 0
        end
      end
      object cxGridEmployeesLevel1: TcxGridLevel
        GridView = gvEmployees
      end
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      LayoutLookAndFeel = DM.dxLayoutCxLookAndFeelNavy
      ItemIndex = 1
    end
    inherited dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'CUSTOMERS'
      CaptionOptions.Visible = True
      Index = 0
    end
    inherited lgBackButton: TdxLayoutGroup
      Visible = False
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Index = 1
      AutoCreated = True
    end
    object liCustomers: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'CUSTOMERS'
      CaptionOptions.Layout = clTop
      Control = cxGridCustomers
      ControlOptions.MinHeight = 180
      ControlOptions.OriginalHeight = 202
      ControlOptions.OriginalWidth = 830
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = lbSelectedCustomer
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 172
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avCenter
      Control = cmbEmployeesView
      ControlOptions.OriginalHeight = 33
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avBottom
      Control = cxGridEmployees
      ControlOptions.OriginalHeight = 300
      ControlOptions.OriginalWidth = 809
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 570
    Width = 1099
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 1095
      object btnNew: TcxButton [0]
        Left = 457
        Top = 17
        Width = 85
        Height = 82
        Caption = 'New'
        Enabled = False
        OptionsImage.ImageIndex = 26
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnEdit: TcxButton [1]
        Left = 552
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Edit'
        OptionsImage.ImageIndex = 22
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = btnEditClick
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnNew
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Enabled = False
        Index = 0
      end
      object dxLayoutItem8: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnEdit
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
    end
  end
end
