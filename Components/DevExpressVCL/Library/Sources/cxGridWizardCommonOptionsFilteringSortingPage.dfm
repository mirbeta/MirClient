inherited cxGridWizardCommonOptionsFilteringSortingPageFrame: TcxGridWizardCommonOptionsFilteringSortingPageFrame
  inherited lcMain: TdxLayoutControl
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 367
      Height = 407
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object chbFilterButton: TcxCheckBox [1]
      Left = 361
      Top = 83
      Caption = 'Filter Button Visibility'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 120
    end
    object chbFilterBox: TcxCheckBox [2]
      Left = 361
      Top = 10
      Caption = 'Filter Box Visibility'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
      Width = 108
    end
    object rbShowButtonAlways: TcxRadioButton [3]
      Left = 377
      Top = 106
      Width = 59
      Height = 17
      Caption = 'Always'
      TabOrder = 5
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbShowButtonWhenSelected: TcxRadioButton [4]
      Left = 377
      Top = 129
      Width = 99
      Height = 17
      Caption = 'When selected'
      TabOrder = 6
      OnClick = RefreshPreviewGrid
      GroupIndex = 1
      Transparent = True
    end
    object rbShowAlways: TcxRadioButton [5]
      Left = 377
      Top = 37
      Width = 253
      Height = 17
      Caption = 'Always'
      TabOrder = 2
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbShowNonEmpty: TcxRadioButton [6]
      Left = 377
      Top = 60
      Width = 253
      Height = 17
      Caption = 'Non empty'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
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
    object lcFilteringSortingPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lciRowFiltering: TdxLayoutItem
      Parent = lcFilteringSortingPageGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbFilterButton
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcFilteringSortingPageSeparatorItem: TdxLayoutSeparatorItem
      Parent = lcFilteringSortingPageGroup3
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcFilteringSortingPageGroup3: TdxLayoutGroup
      Parent = lcFilteringSortingPageGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'lbFilterBox'
      CaptionOptions.Visible = False
      Control = chbFilterBox
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup3
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcgRowFilteringGroup: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lciShowButtonAlways: TdxLayoutItem
      Parent = lcgRowFilteringGroup
      CaptionOptions.Text = 'rbShowButtonAlways'
      CaptionOptions.Visible = False
      Control = rbShowButtonAlways
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciShowButtonWhenSelected: TdxLayoutItem
      Parent = lcgRowFilteringGroup
      CaptionOptions.Text = 'rbShowButtonWhenSelected'
      CaptionOptions.Visible = False
      Control = rbShowButtonWhenSelected
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'rbShowAlways'
      CaptionOptions.Visible = False
      Control = rbShowAlways
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'rbShowNonEmpty'
      CaptionOptions.Visible = False
      Control = rbShowNonEmpty
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
end
