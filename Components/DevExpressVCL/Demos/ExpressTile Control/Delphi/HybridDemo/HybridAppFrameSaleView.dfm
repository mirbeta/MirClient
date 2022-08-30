inherited frmSaleView: TfrmSaleView
  Width = 1008
  Height = 669
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 1008
    Height = 549
    object lbInvoice: TcxLabel [0]
      Left = 200
      Top = 17
      ParentFont = False
      Style.HotTrack = False
      Style.StyleController = DM.cxEditStyleController1
      Transparent = True
    end
    object edOrderDate: TcxDBDateEdit [1]
      Left = 200
      Top = 52
      DataBinding.DataField = 'OrderDate'
      DataBinding.DataSource = DM.dsOrders
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Properties.DisplayFormat = 'mm/dd/yyyy'
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 1
      Width = 404
    end
    object edCompany: TcxTextEdit [2]
      Left = 200
      Top = 91
      ParentFont = False
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Width = 404
    end
    object edStore: TcxTextEdit [3]
      Left = 200
      Top = 130
      ParentFont = False
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 3
      Width = 404
    end
    object edPONumber: TcxDBTextEdit [4]
      Left = 200
      Top = 208
      DataBinding.DataField = 'PONumber'
      DataBinding.DataSource = DM.dsOrders
      ParentFont = False
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 5
      Width = 404
    end
    object edAddress: TcxTextEdit [5]
      Left = 200
      Top = 169
      ParentFont = False
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 4
      Width = 404
    end
    object cxGrid1: TcxGrid [6]
      Left = 59
      Top = 267
      Width = 545
      Height = 253
      TabOrder = 6
      object cxGrid1DBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsOrderItems
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <
          item
            Format = '$,0.00;-$,0.00'
            Kind = skSum
            FieldName = 'Total'
            Column = cxGrid1DBTableView1Discount
            DisplayText = 'awewqer'
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
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Footer = DM.cxStyle1
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        object cxGrid1DBTableView1Name: TcxGridDBColumn
          DataBinding.FieldName = 'Name'
          PropertiesClassName = 'TcxTextEditProperties'
          HeaderAlignmentHorz = taCenter
          Options.Filtering = False
          Width = 226
        end
        object cxGrid1DBTableView1ProductUnits: TcxGridDBColumn
          Caption = 'Quantity'
          DataBinding.FieldName = 'ProductUnits'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          HeaderAlignmentHorz = taCenter
          Options.Filtering = False
          Width = 89
        end
        object cxGrid1DBTableView1ProductPrice: TcxGridDBColumn
          Caption = 'Unit Price'
          DataBinding.FieldName = 'ProductPrice'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0.00;-$,0.00'
          HeaderAlignmentHorz = taCenter
          Options.Filtering = False
          Width = 90
        end
        object cxGrid1DBTableView1Discount: TcxGridDBColumn
          DataBinding.FieldName = 'Discount'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0.00;-$,0.00'
          HeaderAlignmentHorz = taCenter
          Options.Filtering = False
          Width = 114
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1DBTableView1
      end
    end
    object edShipping: TcxDBCurrencyEdit [7]
      Left = 489
      Top = 530
      DataBinding.DataField = 'ShippingAmount'
      DataBinding.DataSource = DM.dsOrders
      ParentFont = False
      Properties.DisplayFormat = '$,0.00;$-,0.00'
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 7
      Width = 115
    end
    object edGrandTotal: TcxDBCurrencyEdit [8]
      Left = 488
      Top = 569
      DataBinding.DataField = 'TotalAmount'
      DataBinding.DataSource = DM.dsOrders
      ParentFont = False
      Properties.DisplayFormat = '$,0.00;$-,0.00'
      Properties.ReadOnly = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = [fsBold]
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 8
      Width = 116
    end
    object dxMapControl1: TdxMapControl [9]
      Left = 614
      Top = 17
      Width = 343
      Height = 581
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
      TabOrder = 9
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
    object lgOrder: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 545
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object liSale: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'SALE'
      Control = lbInvoice
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liOrderDate: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'ORDER DATE'
      Control = edOrderDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liCompany: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'COMPANY'
      Control = edCompany
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 138
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liStore: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'STORE'
      Control = edStore
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 138
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liPurchaseOrder: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'PURCHASE ORDER'
      Control = edPONumber
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liAddress: TdxLayoutItem
      Parent = lgOrder
      CaptionOptions.Text = 'ADDRESS'
      Control = edAddress
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 382
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = lgOrder
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'cxGrid1'
      CaptionOptions.Visible = False
      Control = cxGrid1
      ControlOptions.OriginalHeight = 253
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lgOrder
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 6
    end
    object liShipping: TdxLayoutItem
      Parent = lgOrder
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'SHIPPING'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 192
      Control = edShipping
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 115
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object liGrandTotal: TdxLayoutItem
      Parent = lgOrder
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'GRAND TOTAL'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 228
      Control = edGrandTotal
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 116
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      Control = dxMapControl1
      ControlOptions.OriginalHeight = 526
      ControlOptions.OriginalWidth = 432
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 549
    Width = 1008
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 1004
      object btnClose: TcxButton [0]
        Left = 459
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Close'
        OptionsImage.ImageIndex = 21
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnCloseClick
      end
      object dxLayoutItem12: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Visible = False
        Control = btnClose
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
end
