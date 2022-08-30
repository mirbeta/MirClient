inherited cxGridWizardTableViewOptionsInterfaceElementsPageFrame: TcxGridWizardTableViewOptionsInterfaceElementsPageFrame
  inherited lcMain: TdxLayoutControl
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 345
      Height = 430
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object lbInterfaceElements: TcxLabel [1]
      Left = 361
      Top = 10
      Caption = 'Interface elements'
      Style.HotTrack = False
      Transparent = True
    end
    object chbFooter: TcxCheckBox [2]
      Left = 377
      Top = 56
      Caption = 'Footer'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 253
    end
    object chbGroupBox: TcxCheckBox [3]
      Left = 377
      Top = 129
      Caption = 'Group box'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 253
    end
    object chbHeader: TcxCheckBox [4]
      Left = 377
      Top = 33
      Caption = 'Header'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 253
    end
    object chbIndicator: TcxCheckBox [5]
      Left = 377
      Top = 198
      Caption = 'Indicator'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      OnClick = chbIndicatorClick
      Width = 253
    end
    object chbNavigator: TcxCheckBox [6]
      Left = 377
      Top = 267
      Caption = 'Navigator'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      OnClick = chbGroupFooterClick
      Width = 253
    end
    object chbNewItemRow: TcxCheckBox [7]
      Left = 377
      Top = 175
      Caption = 'New item row'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 253
    end
    object chbPreview: TcxCheckBox [8]
      Left = 377
      Top = 317
      Caption = 'Preview column'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
      OnClick = chbPreviewClick
      Width = 253
    end
    object cbPreviewColumn: TcxComboBox [9]
      Tag = -1
      Left = 393
      Top = 340
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 15
      Width = 235
    end
    object chbColumnsQuickCustomization: TcxCheckBox [10]
      Left = 377
      Top = 244
      Caption = 'Columns quick customization'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Width = 253
    end
    object chbBandsQuickCustomization: TcxCheckBox [11]
      Left = 377
      Top = 221
      Caption = 'Bands quick customization'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Width = 253
    end
    object chbGroupFooter: TcxCheckBox [12]
      Left = 377
      Top = 79
      Caption = 'Group Footer'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      OnClick = chbGroupFooterClick
      Width = 253
    end
    object cbGroupFooterMode: TcxComboBox [13]
      Tag = -1
      Left = 393
      Top = 102
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 5
      Width = 235
    end
    object chbInplaceEditForm: TcxCheckBox [14]
      Left = 377
      Top = 367
      Caption = 'Inplace edit form'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      OnClick = chbGroupFooterClick
      Width = 253
    end
    object chbHideCurrentRow: TcxCheckBox [15]
      Left = 393
      Top = 390
      Caption = 'Hide current row'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Transparent = True
      Width = 237
    end
    object chbFilterRow: TcxCheckBox [16]
      Left = 377
      Top = 152
      Caption = 'Filter row'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Width = 253
    end
    object chcbNavigatorButtons: TcxCheckComboBox [17]
      Left = 434
      Top = 290
      AutoSize = False
      Properties.Items = <>
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 13
      Height = 21
      Width = 121
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      Index = -1
    end
    object lciPreviewGrid: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Panel1'
      CaptionOptions.Visible = False
      Control = pnPreviewGrid
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcInterfaceElementsPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciInterfaceElements: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup5
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbInterfaceElements
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFooter: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbFooter
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciGroupBox: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbGroupBox
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lciHeader: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbHeader
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciIndicator: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chbIndicator
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object lciNavigator: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = chbNavigator
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object lciNewItemRow: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = chbNewItemRow
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lciPreview: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbPreview
      ControlOptions.ShowBorder = False
      Index = 12
    end
    object lciPreviewColumn: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      CaptionOptions.Layout = clTop
      Offsets.Left = 16
      Control = cbPreviewColumn
      ControlOptions.ShowBorder = False
      Index = 13
    end
    object lcInterfaceElementsPageGroup3: TdxLayoutGroup
      Parent = lcInterfaceElementsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcInterfaceElementsPageGroup5: TdxLayoutGroup
      Parent = lcInterfaceElementsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcInterfaceElementsPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup1
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lciColumnsQuickCustomization: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbColumnsQuickCustomization
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object lciBandsQuickCustomization: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbBandsQuickCustomization
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      CaptionOptions.Visible = False
      Control = chbGroupFooter
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciGroupFooter: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      Offsets.Left = 16
      Control = cbGroupFooterMode
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciInplaceEditForm: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbInplaceEditForm
      ControlOptions.ShowBorder = False
      Index = 14
    end
    object lciHideCurrentRow: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbHideCurrentRow
      ControlOptions.ShowBorder = False
      Index = 15
    end
    object lciFilterRow: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbFilterRow
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcInterfaceElementsPageGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciNavigatorButtons: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup3
      CaptionOptions.Text = 'Buttons'
      Offsets.Left = 16
      Control = chcbNavigatorButtons
      ControlOptions.ShowBorder = False
      Index = 11
    end
  end
end
