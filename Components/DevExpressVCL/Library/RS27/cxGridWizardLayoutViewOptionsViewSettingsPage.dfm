inherited cxGridWizardLayoutViewOptionsViewSettingsPageFrame: TcxGridWizardLayoutViewOptionsViewSettingsPageFrame
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
    object lbSingleRecordStretch: TcxLabel [1]
      Left = 361
      Top = 206
      Caption = 'Single record stretch'
      Style.HotTrack = False
      Transparent = True
    end
    object lbViewMode: TcxLabel [2]
      Left = 361
      Top = 10
      Caption = 'View mode'
      Style.HotTrack = False
      Transparent = True
    end
    object rbNone: TcxRadioButton [3]
      Left = 377
      Top = 229
      Width = 250
      Height = 17
      Caption = 'None'
      TabOrder = 10
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbHorizontal: TcxRadioButton [4]
      Left = 377
      Top = 252
      Width = 250
      Height = 17
      Caption = 'Horizontal'
      TabOrder = 11
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbVertical: TcxRadioButton [5]
      Left = 377
      Top = 275
      Width = 250
      Height = 17
      Caption = 'Vertical'
      TabOrder = 12
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbClient: TcxRadioButton [6]
      Left = 377
      Top = 298
      Width = 250
      Height = 17
      Caption = 'Client'
      TabOrder = 13
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbSingleRecord: TcxRadioButton [7]
      Left = 377
      Top = 33
      Width = 250
      Height = 17
      Caption = 'Single record'
      TabOrder = 2
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbSingleRow: TcxRadioButton [8]
      Left = 377
      Top = 56
      Width = 250
      Height = 17
      Caption = 'Single row'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbMultiRow: TcxRadioButton [9]
      Left = 377
      Top = 79
      Width = 250
      Height = 17
      Caption = 'Multi row'
      TabOrder = 4
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbSingleColumn: TcxRadioButton [10]
      Left = 377
      Top = 102
      Width = 250
      Height = 17
      Caption = 'Single column'
      TabOrder = 5
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbMultiColumn: TcxRadioButton [11]
      Left = 377
      Top = 125
      Width = 250
      Height = 17
      Caption = 'Multi column'
      TabOrder = 6
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbCarousel: TcxRadioButton [12]
      Left = 377
      Top = 148
      Width = 250
      Height = 17
      Caption = 'Carousel'
      TabOrder = 7
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object chbCenterRecords: TcxCheckBox [13]
      Left = 377
      Top = 183
      Caption = 'Center records'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 250
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
    object lcViewSettingsPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lciSingleRecordStretch: TdxLayoutItem
      Parent = lcViewSettingsPageGroup4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbSingleRecordStretch
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciViewMode: TdxLayoutItem
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbViewMode
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciNone: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbNone
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciHorizontal: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbHorizontal
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciVertical: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbVertical
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciClient: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbClient
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcViewSettingsPageGroup2: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcViewSettingsPageGroup4: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcViewSettingsPageGroup5: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup5
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcViewSettingsPageGroup7: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup7
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageGroup3: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciSingleRecord: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbSingleRecord
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciSingleRow: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbSingleRow
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciMultiRow: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbMultiRow
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciSingleColumn: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbSingleColumn
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciMultiColumn: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton5'
      CaptionOptions.Visible = False
      Control = rbMultiColumn
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lciCarousel: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton6'
      CaptionOptions.Visible = False
      Control = rbCarousel
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup3
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 6
    end
    object lciCenterRecords: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCenterRecords
      ControlOptions.ShowBorder = False
      Index = 7
    end
  end
end
