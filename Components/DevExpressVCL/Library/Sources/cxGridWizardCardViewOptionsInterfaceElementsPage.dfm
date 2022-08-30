inherited cxGridWizardCardViewOptionsInterfaceElementsPageFrame: TcxGridWizardCardViewOptionsInterfaceElementsPageFrame
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
    object chbNavigator: TcxCheckBox [1]
      Left = 377
      Top = 194
      Caption = 'Navigator'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      OnClick = chbNavigatorClick
      Width = 253
    end
    object chbEmptyRows: TcxCheckBox [2]
      Left = 377
      Top = 171
      Caption = 'Empty rows'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 253
    end
    object lbCardExpandButtonAlignment: TcxLabel [3]
      Left = 361
      Top = 10
      Caption = 'Expand button alignment'
      Style.HotTrack = False
      Transparent = True
    end
    object lbOther: TcxLabel [4]
      Left = 361
      Top = 148
      Caption = 'Other'
      Style.HotTrack = False
      Transparent = True
    end
    object rbLayoutHorizontal: TcxRadioButton [5]
      Left = 377
      Top = 102
      Width = 253
      Height = 17
      Caption = 'Horizontal'
      TabOrder = 5
      OnClick = RefreshPreviewGrid
      GroupIndex = 2
      Transparent = True
    end
    object rbLayoutVertical: TcxRadioButton [6]
      Left = 377
      Top = 125
      Width = 253
      Height = 17
      Caption = 'Vertical'
      TabOrder = 6
      OnClick = RefreshPreviewGrid
      GroupIndex = 2
      Transparent = True
    end
    object lbRowLayout: TcxLabel [7]
      Left = 361
      Top = 79
      Caption = 'Row Layout'
      Style.HotTrack = False
      Transparent = True
    end
    object rbAlignmentLeft: TcxRadioButton [8]
      Left = 377
      Top = 33
      Width = 253
      Height = 17
      Caption = 'Left'
      TabOrder = 2
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbAlignmentRight: TcxRadioButton [9]
      Left = 377
      Top = 56
      Width = 253
      Height = 17
      Caption = 'Right'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object chcbNavigatorButtons: TcxCheckComboBox [10]
      Left = 434
      Top = 217
      AutoSize = False
      Properties.Items = <>
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 10
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
    object lciNavigator: TdxLayoutItem
      Parent = lcMainGroup6
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbNavigator
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciEmptyRows: TdxLayoutItem
      Parent = lcMainGroup6
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chbEmptyRows
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCardExpandButtonAlignment: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbCardExpandButtonAlignment
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcInterfaceElementsPageGroup6: TdxLayoutGroup
      Parent = lcMainGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcInterfaceElementsPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcInterfaceElementsPageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup4: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
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
    object lcMainGroup6: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcInterfaceElementsPageGroup12: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lciOther: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup12
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbOther
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcInterfaceElementsPageGroup13: TdxLayoutGroup
      Parent = lcInterfaceElementsPageGroup12
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcInterfaceElementsPageSeparatorItem4: TdxLayoutSeparatorItem
      Parent = lcInterfaceElementsPageGroup13
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'rbLayoutHorizontal'
      CaptionOptions.Visible = False
      Control = rbLayoutHorizontal
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'rbLayoutVertical'
      CaptionOptions.Visible = False
      Control = rbLayoutVertical
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbRowLayout
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup1
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcInterfaceElementsPageGroup2: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciAlignmentLeft: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup2
      CaptionOptions.Text = 'cxRadioButton5'
      CaptionOptions.Visible = False
      Control = rbAlignmentLeft
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciAlignmentRight: TdxLayoutItem
      Parent = lcInterfaceElementsPageGroup2
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbAlignmentRight
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciNavigatorButtons: TdxLayoutItem
      Parent = lcMainGroup6
      CaptionOptions.Text = 'Buttons'
      Offsets.Left = 16
      Control = chcbNavigatorButtons
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
end
