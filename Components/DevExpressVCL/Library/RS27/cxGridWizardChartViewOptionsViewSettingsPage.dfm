inherited cxGridWizardChartViewOptionsViewSettingsPageFrame: TcxGridWizardChartViewOptionsViewSettingsPageFrame
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
    object chbDataDrillDown: TcxCheckBox [1]
      Left = 377
      Top = 194
      Caption = 'Data drill down'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Width = 95
    end
    object chbDataGroupHiding: TcxCheckBox [2]
      Left = 377
      Top = 33
      Caption = 'Data group hiding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 109
    end
    object chbDataGroupMoving: TcxCheckBox [3]
      Left = 377
      Top = 56
      Caption = 'Data group moving'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 122
    end
    object chbOptionsCustomization: TcxCheckBox [4]
      Left = 377
      Top = 102
      Caption = 'Options customization'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 134
    end
    object chbSeriesCustomization: TcxCheckBox [5]
      Left = 377
      Top = 125
      Caption = 'Series customization'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 130
    end
    object chbAntialiasing: TcxCheckBox [6]
      Left = 377
      Top = 217
      Caption = 'Antialiasing'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Width = 88
    end
    object chbTransparentCaptions: TcxCheckBox [7]
      Left = 377
      Top = 240
      Caption = 'Transparent captions'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Width = 129
    end
    object chbCustomizeButton: TcxCheckBox [8]
      Left = 377
      Top = 148
      Caption = 'Customize button'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Width = 109
    end
    object lbDataGroupOptions: TcxLabel [9]
      Left = 361
      Top = 10
      Caption = 'Data group options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbCustomizationOptions: TcxLabel [10]
      Left = 361
      Top = 79
      Caption = 'Customization options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbOther: TcxLabel [11]
      Left = 361
      Top = 171
      Caption = 'Other'
      Style.HotTrack = False
      Transparent = True
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
    object lciDataDrillDown: TdxLayoutItem
      Parent = lcViewSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbDataDrillDown
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDataGroupHiding: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbDataGroupHiding
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDataGroupMoving: TdxLayoutItem
      Parent = lcViewSettingsPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbDataGroupMoving
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciOptionsCustomization: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chbOptionsCustomization
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciSeriesCustomization: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = chbSeriesCustomization
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAntialiasing: TdxLayoutItem
      Parent = lcViewSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = chbAntialiasing
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciTransparentCaptions: TdxLayoutItem
      Parent = lcViewSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox7'
      CaptionOptions.Visible = False
      Control = chbTransparentCaptions
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciCustomizeButton: TdxLayoutItem
      Parent = lcViewSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCustomizeButton
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcViewSettingsPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciDataGroupOptions: TdxLayoutItem
      Parent = lcViewSettingsPageGroup5
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbDataGroupOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCustomizationOptions: TdxLayoutItem
      Parent = lcViewSettingsPageGroup7
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbCustomizationOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciOther: TdxLayoutItem
      Parent = lcViewSettingsPageGroup9
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbOther
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcViewSettingsPageGroup2: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageGroup3: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcViewSettingsPageGroup4: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcViewSettingsPageGroup5: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcViewSettingsPageGroup6: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcViewSettingsPageGroup7: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcViewSettingsPageGroup8: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup8
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcViewSettingsPageGroup9: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcViewSettingsPageGroup10: TdxLayoutGroup
      Parent = lcViewSettingsPageGroup9
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcViewSettingsPageSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lcViewSettingsPageGroup10
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
  end
end
