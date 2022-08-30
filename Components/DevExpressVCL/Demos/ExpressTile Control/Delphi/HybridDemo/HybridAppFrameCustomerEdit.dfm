inherited frmCustomerEdit: TfrmCustomerEdit
  Width = 946
  Height = 650
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 946
    Height = 530
    object edName: TcxDBTextEdit [0]
      Left = 195
      Top = 17
      DataBinding.DataField = 'Name'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 0
      Width = 271
    end
    object edHomeAddress: TcxDBTextEdit [1]
      Left = 195
      Top = 56
      DataBinding.DataField = 'HomeOffice_Line'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 1
      Width = 271
    end
    object edHomeCity: TcxDBTextEdit [2]
      Left = 195
      Top = 95
      DataBinding.DataField = 'HomeOffice_City'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Width = 147
    end
    object edHomeState: TcxDBLookupComboBox [3]
      Left = 403
      Top = 95
      DataBinding.DataField = 'HomeOffice_State'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      ParentShowHint = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'ShortName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsStatesSpr
      ShowHint = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 3
      Width = 63
    end
    object edFax: TcxDBTextEdit [4]
      Left = 195
      Top = 232
      DataBinding.DataField = 'Fax'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 6
      Width = 271
    end
    object edBillingAddress: TcxDBTextEdit [5]
      Left = 195
      Top = 291
      DataBinding.DataField = 'BillingAddress_Line'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 7
      Width = 271
    end
    object edBillingCity: TcxDBTextEdit [6]
      Left = 195
      Top = 330
      DataBinding.DataField = 'BillingAddress_City'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 8
      Width = 147
    end
    object edBillingZipCode: TcxDBTextEdit [7]
      Left = 195
      Top = 369
      DataBinding.DataField = 'BillingAddress_ZipCode'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 10
      Width = 271
    end
    object edBillingState: TcxDBLookupComboBox [8]
      Left = 403
      Top = 330
      DataBinding.DataField = 'BillingAddress_State'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'ShortName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsStatesSpr
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 9
      Width = 63
    end
    object edProfile: TcxDBRichEdit [9]
      Left = 59
      Top = 437
      DataBinding.DataField = 'PersonalProfile'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.ScrollBars = ssVertical
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 11
      Height = 76
      Width = 407
    end
    object edHomeZipCode: TcxDBTextEdit [10]
      Left = 195
      Top = 134
      DataBinding.DataField = 'HomeOffice_ZipCode'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 4
      Width = 271
    end
    object edPhone: TcxDBTextEdit [11]
      Left = 195
      Top = 193
      DataBinding.DataField = 'Phone'
      DataBinding.DataSource = DM.dsCustomers
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 5
      Width = 271
    end
    object dxMapControl1: TdxMapControl [12]
      Left = 487
      Top = 17
      Width = 442
      Height = 496
      NavigationPanel.Style.CoordinateFont.Charset = DEFAULT_CHARSET
      NavigationPanel.Style.CoordinateFont.Color = clWindowText
      NavigationPanel.Style.CoordinateFont.Height = -21
      NavigationPanel.Style.CoordinateFont.Name = 'Tahoma'
      NavigationPanel.Style.CoordinateFont.Style = []
      NavigationPanel.Style.ScaleFont.Charset = DEFAULT_CHARSET
      NavigationPanel.Style.ScaleFont.Color = clWindowText
      NavigationPanel.Style.ScaleFont.Height = -16
      NavigationPanel.Style.ScaleFont.Name = 'Tahoma'
      NavigationPanel.Style.ScaleFont.Style = []
      TabOrder = 12
      ZoomLevel = 5.000000000000000000
      object dxMapControl1ImageTileLayer1: TdxMapImageTileLayer
        ProviderClassName = 'TdxMapControlOpenStreetMapImageryDataProvider'
        Provider.Subdomains.Strings = (
          'a'
          'b'
          'c')
        Provider.UrlTemplate = 'http://[subdomain].tile.openstreetmap.org/[z]/[x]/[y].png'
      end
      object mcItemLayer: TdxMapItemLayer
        ProjectionClassName = 'TdxMapControlSphericalMercatorProjection'
        ItemStyle.AssignedValues = [mcsvColor, mcsvBorderColor]
        ItemStyle.BorderColor = -3247872
        ItemStyle.Color = -3247872
        ItemStyleHot.AssignedValues = [mcsvColor, mcsvBorderColor]
        ItemStyleHot.BorderColor = -16777216
        ItemStyleHot.Color = -3247872
        ItemStyleSelected.AssignedValues = [mcsvColor, mcsvBorderColor]
        ItemStyleSelected.BorderColor = -13676123
        ItemStyleSelected.Color = -13676123
        object mcItemLayerDot1: TdxMapDot
        end
      end
    end
    inherited dxLayoutGroup2: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      ScrollOptions.Vertical = smAuto
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 407
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object liName: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'NAME'
      Control = edName
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liAddress: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'ADDRESS'
      Control = edHomeAddress
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liCity: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'CITY'
      Control = edHomeCity
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 177
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liState: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'STATE'
      Control = edHomeState
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 63
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup3
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object liFax: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'FAX'
      Control = edFax
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 4
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 7
    end
    object liBillingAddress: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'BILLING ADDRESS'
      Control = edBillingAddress
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object liBillingCity: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'CITY'
      Control = edBillingCity
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 177
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liBillingZipCode: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'ZIP CODE'
      Control = edBillingZipCode
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object liBillingState: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'STATE'
      Control = edBillingState
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 63
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup3
      LayoutDirection = ldHorizontal
      Index = 9
      AutoCreated = True
    end
    object liProfile: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'PROFILE'
      CaptionOptions.Layout = clTop
      Control = edProfile
      ControlOptions.MinHeight = 70
      ControlOptions.OriginalHeight = 39
      ControlOptions.OriginalWidth = 437
      ControlOptions.ShowBorder = False
      Index = 11
    end
    object liZipCode: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'ZIP CODE'
      Control = edHomeZipCode
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liPhone: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'PHONE'
      Control = edPhone
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 301
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutSplitterItem1: TdxLayoutSplitterItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 1
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      Control = dxMapControl1
      ControlOptions.OriginalHeight = 479
      ControlOptions.OriginalWidth = 297
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 530
    Width = 946
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 942
      object btnSave: TcxButton [0]
        Left = 381
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Save'
        OptionsImage.ImageIndex = 25
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnSaveClick
      end
      object btnCancel: TcxButton [1]
        Left = 476
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Cancel'
        OptionsImage.ImageIndex = 21
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnCancelClick
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnSave
        ControlOptions.OriginalHeight = 70
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem17: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnCancel
        ControlOptions.OriginalHeight = 70
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
    end
  end
  object stRepository: TdxScreenTipRepository
    Left = 296
    Top = 336
    PixelsPerInch = 96
    object stRepositoryScreenTip1: TdxScreenTip
      Header.PlainText = False
      Header.Text = 'stRepositoryScreenTip1'
    end
  end
  object cxHintStyleController1: TcxHintStyleController
    HintStyleClassName = 'TcxHintStyle'
    HintStyle.Animate = cxhaNone
    HintStyle.CaptionFont.Charset = DEFAULT_CHARSET
    HintStyle.CaptionFont.Color = clWindowText
    HintStyle.CaptionFont.Height = -11
    HintStyle.CaptionFont.Name = 'Tahoma'
    HintStyle.CaptionFont.Style = []
    HintStyle.Font.Charset = DEFAULT_CHARSET
    HintStyle.Font.Color = clWindowText
    HintStyle.Font.Height = -11
    HintStyle.Font.Name = 'Tahoma'
    HintStyle.Font.Style = []
    HintStyle.Rounded = True
    Left = 400
    Top = 336
  end
end
