inherited cxGridWizardTableViewOptionsInplaceEditFormPageFrame: TcxGridWizardTableViewOptionsInplaceEditFormPageFrame
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
    object rbClient: TcxRadioButton [1]
      Left = 377
      Top = 33
      Width = 253
      Height = 17
      Caption = 'Client'
      TabOrder = 2
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbVertical: TcxRadioButton [2]
      Left = 377
      Top = 102
      Width = 253
      Height = 17
      Caption = 'Vertical'
      TabOrder = 5
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object lbStretch: TcxLabel [3]
      Left = 361
      Top = 10
      Caption = 'Stretch'
      Style.HotTrack = False
      Transparent = True
    end
    object rbHorizontal: TcxRadioButton [4]
      Left = 377
      Top = 56
      Width = 253
      Height = 17
      Caption = 'Horizontal'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbNone: TcxRadioButton [5]
      Left = 377
      Top = 79
      Width = 253
      Height = 17
      Caption = 'None'
      TabOrder = 4
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbShowEditForm: TcxRadioButton [6]
      Left = 377
      Top = 148
      Width = 253
      Height = 17
      Caption = 'Show Edit form'
      TabOrder = 7
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbSwitchExpandedState: TcxRadioButton [7]
      Left = 377
      Top = 171
      Width = 253
      Height = 17
      Caption = 'Switch expanded state'
      TabOrder = 8
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object chbUseDefaultLayout: TcxCheckBox [8]
      Left = 361
      Top = 194
      Caption = 'Use default layout'
      Properties.OnChange = chbUseDefaultLayoutPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Width = 269
    end
    object lbMasterRowDblClickAction: TcxLabel [9]
      Left = 361
      Top = 125
      Caption = 'Master row double click action'
      Style.HotTrack = False
      Transparent = True
    end
    object seDefaultColumnCount: TcxSpinEdit [10]
      Left = 483
      Top = 217
      Properties.LargeIncrement = 5.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 10
      Value = 2
      Width = 75
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
    object lciClient: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbClient
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciVertical: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbVertical
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lciStretch: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbStretch
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciHorizontal: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbHorizontal
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciNone: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbNone
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciShowEditForm: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbShowEditForm
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciSwitchExpandedState: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbSwitchExpandedState
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciUseDefaultLayout: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = chbUseDefaultLayout
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcMainGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup1
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lciMasterRowDblClickAction: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbMasterRowDblClickAction
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDefaultColumnCount: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Default column count'
      Offsets.Left = 16
      Control = seDefaultColumnCount
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup6
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcMainGroup7
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup6: TdxLayoutGroup
      Parent = lcMainGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
  end
end
