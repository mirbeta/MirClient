inherited cxGridWizardLayoutViewOptionsCarouselModePageFrame: TcxGridWizardLayoutViewOptionsCarouselModePageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 345
      Height = 430
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object seAnimationInterval: TcxSpinEdit [1]
      Left = 468
      Top = 286
      Properties.Increment = 100.000000000000000000
      Properties.LargeIncrement = 1000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 10
      Value = 100
      Width = 75
    end
    object seRadius: TcxSpinEdit [2]
      Left = 468
      Top = 313
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 11
      Width = 75
    end
    object trbBackgroundRecordAlphaLevel: TcxTrackBar [3]
      Left = 468
      Top = 33
      Properties.Frequency = 10
      Properties.Max = 255
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Height = 30
      Width = 160
    end
    object trbBackgroundRecordStartScale: TcxTrackBar [4]
      Left = 468
      Top = 69
      Properties.Frequency = 5
      Properties.Max = 100
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Height = 30
      Width = 160
    end
    object trbBackgroundRecordEndScale: TcxTrackBar [5]
      Left = 468
      Top = 105
      Properties.Frequency = 5
      Properties.Max = 100
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Height = 30
      Width = 160
    end
    object seRecordCount: TcxSpinEdit [6]
      Left = 468
      Top = 340
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 12
      Value = 1
      Width = 75
    end
    object trbRollAngle: TcxTrackBar [7]
      Left = 468
      Top = 164
      Properties.Frequency = 20
      Properties.Max = 360
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Height = 30
      Width = 160
    end
    object trbPitchAngle: TcxTrackBar [8]
      Left = 468
      Top = 200
      Properties.Frequency = 10
      Properties.Max = 180
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Height = 30
      Width = 160
    end
    object chbAutoPitchAngle: TcxCheckBox [9]
      Left = 468
      Top = 236
      Caption = 'Auto'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 8
      Transparent = True
      Width = 117
    end
    object lbBackgroundRecordOptions: TcxLabel [10]
      Left = 361
      Top = 10
      Caption = 'Background record options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbAngleOptions: TcxLabel [11]
      Left = 361
      Top = 141
      Caption = 'Angle options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbOther: TcxLabel [12]
      Left = 361
      Top = 263
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
    object lciAnimationInterval: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Animation interval'
      Control = seAnimationInterval
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRadius: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Radius'
      Control = seRadius
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciBackgroundRecordAlphaLevel: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup2
      CaptionOptions.Text = 'Alpha level'
      Control = trbBackgroundRecordAlphaLevel
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciBackgroundRecordStartScale: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup2
      CaptionOptions.Text = 'Start scale'
      Control = trbBackgroundRecordStartScale
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciBackgroundRecordEndScale: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup2
      CaptionOptions.Text = 'End scale'
      Control = trbBackgroundRecordEndScale
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciRecordCount: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Record count'
      Control = seRecordCount
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciRollAngle: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup3
      CaptionOptions.Text = 'Roll angle'
      Control = trbRollAngle
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciPitchAngle: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup3
      CaptionOptions.Text = 'Pitch angle'
      Control = trbPitchAngle
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAutoPitchAngle: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = ' '
      Control = chbAutoPitchAngle
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcCarouselModeSettingsPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      Offsets.Right = 2
      SizeOptions.Width = 267
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciBackgroundRecordOptions: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup5
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbBackgroundRecordOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciAngleOptions: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup7
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbAngleOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciOther: TdxLayoutItem
      Parent = lcCarouselModeSettingsPageGroup9
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbOther
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcCarouselModeSettingsPageGroup2: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcCarouselModeSettingsPageGroup3: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcCarouselModeSettingsPageGroup4: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcCarouselModeSettingsPageGroup5: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcCarouselModeSettingsPageGroup6: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcCarouselModeSettingsPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcCarouselModeSettingsPageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcCarouselModeSettingsPageGroup7: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcCarouselModeSettingsPageGroup8: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcCarouselModeSettingsPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcCarouselModeSettingsPageGroup8
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcCarouselModeSettingsPageGroup9: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcCarouselModeSettingsPageGroup10: TdxLayoutGroup
      Parent = lcCarouselModeSettingsPageGroup9
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcCarouselModeSettingsPageSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lcCarouselModeSettingsPageGroup10
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
  end
end
