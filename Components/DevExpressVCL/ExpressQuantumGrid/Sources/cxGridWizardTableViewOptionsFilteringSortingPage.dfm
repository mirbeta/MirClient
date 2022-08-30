inherited cxGridWizardTableViewOptionsFilteringSortingPageFrame: TcxGridWizardTableViewOptionsFilteringSortingPageFrame
  inherited lcMain: TdxLayoutControl
    object chbIncSearch: TcxCheckBox [0]
      Left = 377
      Top = 290
      Caption = 'Incremental Search'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Width = 248
    end
    object chbColumnSorting: TcxCheckBox [1]
      Left = 377
      Top = 267
      Caption = 'Column sorting'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Width = 248
    end
    object pnPreviewGrid: TPanel [2]
      Left = 10
      Top = 10
      Width = 345
      Height = 430
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object rbAlways: TcxRadioButton [3]
      Left = 377
      Top = 198
      Width = 248
      Height = 17
      Caption = 'Always'
      TabOrder = 9
      OnClick = RefreshPreviewGrid
      GroupIndex = 2
      Transparent = True
    end
    object rbNonEmpty: TcxRadioButton [4]
      Left = 377
      Top = 221
      Width = 248
      Height = 17
      Caption = 'Non empty'
      TabOrder = 10
      OnClick = RefreshPreviewGrid
      GroupIndex = 2
      Transparent = True
    end
    object lbOther: TcxLabel [5]
      Left = 361
      Top = 244
      Caption = 'Other'
      Style.HotTrack = False
      Transparent = True
    end
    object lbFilterBox: TcxCheckBox [6]
      Left = 361
      Top = 171
      Caption = 'Filter Box Visibility'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 8
      Transparent = True
      Width = 111
    end
    object chbColumnFiltering: TcxCheckBox [7]
      Left = 361
      Top = 10
      Caption = 'Column filtering'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Width = 98
    end
    object lbHeaderFilterButtonShowMode: TcxLabel [8]
      Left = 377
      Top = 33
      Caption = 'Filter button show mode'
      Style.HotTrack = False
      Transparent = True
    end
    object rbShowModeButton: TcxRadioButton [9]
      Left = 393
      Top = 56
      Width = 232
      Height = 17
      Caption = 'Button'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbShowWhenSelected: TcxRadioButton [10]
      Left = 393
      Top = 148
      Width = 232
      Height = 17
      Caption = 'When selected'
      TabOrder = 7
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object lbShowColumnFilterButtons: TcxLabel [11]
      Left = 377
      Top = 102
      Caption = 'Filter button visible'
      Style.HotTrack = False
      Transparent = True
    end
    object rbShowModeSmartTag: TcxRadioButton [12]
      Left = 393
      Top = 79
      Width = 232
      Height = 17
      Caption = 'Smart tag'
      TabOrder = 4
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbShowAlways: TcxRadioButton [13]
      Left = 393
      Top = 125
      Width = 232
      Height = 17
      Caption = 'Always'
      TabOrder = 6
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      Index = -1
    end
    object lciIncSearch: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbIncSearch
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciColumnSorting: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbColumnSorting
      ControlOptions.ShowBorder = False
      Index = 0
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
    object lciAlways: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbAlways
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciNonEmpty: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbNonEmpty
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcFilteringSortingPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciOther: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup8
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbOther
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFilterBox: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup6
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbFilterBox
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciColumnFiltering: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbColumnFiltering
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciHeaderFilterButtonShowMode: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbHeaderFilterButtonShowMode
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciShowModeButton: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = rbShowModeButton
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciShowWhenSelected: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = rbShowWhenSelected
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lciShowColumnFilterButtons: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbShowColumnFilterButtons
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciShowModeSmartTag: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = rbShowModeSmartTag
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciShowAlways: TdxLayoutItem
      Parent = lciColumnFilteringGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = rbShowAlways
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcFilteringSortingPageGroup2: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lciColumnFilteringGroup: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'Hidden Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcFilteringSortingPageGroup3: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcFilteringSortingPageGroup4: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcFilteringSortingPageGroup5: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcFilteringSortingPageSeparatorItem: TdxLayoutSeparatorItem
      Parent = lcFilteringSortingPageGroup5
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcFilteringSortingPageGroup6: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcFilteringSortingPageGroup7: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcFilteringSortingPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcFilteringSortingPageGroup7
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcFilteringSortingPageGroup8: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcFilteringSortingPageGroup9: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcFilteringSortingPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcFilteringSortingPageGroup9
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
  end
end
