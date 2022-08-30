inherited cxGridWizardTableViewOptionsBehaviorPageFrame: TcxGridWizardTableViewOptionsBehaviorPageFrame
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
    object lbFocusingOptions: TcxLabel [1]
      Left = 361
      Top = 10
      Caption = 'Focusing options'
      Style.HotTrack = False
      Style.TransparentBorder = True
      Transparent = True
    end
    object chbFocusCellOnCycle: TcxCheckBox [2]
      Left = 377
      Top = 33
      Caption = 'Focus cell on cycle'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 253
    end
    object chbFocusCellOnTab: TcxCheckBox [3]
      Left = 377
      Top = 56
      Caption = 'Focus cell on Tab'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 253
    end
    object chbGoToNextCellOnEnter: TcxCheckBox [4]
      Left = 377
      Top = 79
      Caption = 'Go to next cell on Enter'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 253
    end
    object lbSelectionOptions: TcxLabel [5]
      Left = 361
      Top = 102
      Caption = 'Selection options'
      Style.HotTrack = False
      Style.TransparentBorder = True
      Transparent = True
    end
    object chbCellSelect: TcxCheckBox [6]
      Left = 377
      Top = 148
      Caption = 'Cell select'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Width = 253
    end
    object chbCellMultiSelect: TcxCheckBox [7]
      Left = 377
      Top = 125
      Caption = 'Cell multiselect'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 253
    end
    object chbRecordMultiSelect: TcxCheckBox [8]
      Left = 377
      Top = 171
      Caption = 'Record multiselect'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 253
    end
    object chbHideSelection: TcxCheckBox [9]
      Left = 377
      Top = 194
      Caption = 'Hide selection'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Width = 253
    end
    object lbGridLines: TcxLabel [10]
      Left = 361
      Top = 217
      Caption = 'Grid lines'
      Style.HotTrack = False
      Style.TransparentBorder = True
      Transparent = True
    end
    object chbHorizontal: TcxCheckBox [11]
      Left = 377
      Top = 240
      Caption = 'Horizontal'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Width = 253
    end
    object chbVertical: TcxCheckBox [12]
      Left = 377
      Top = 263
      Caption = 'Vertical'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Width = 253
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
    object lciSelectionSizingPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciFocusingOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup3
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbFocusingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFocusCellOnCycle: TdxLayoutItem
      Parent = lcSelectionPageGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbFocusCellOnCycle
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFocusCellOnTab: TdxLayoutItem
      Parent = lcSelectionPageGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbFocusCellOnTab
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciGoToNextCellOnEnter: TdxLayoutItem
      Parent = lcSelectionPageGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbGoToNextCellOnEnter
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciSelectionOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup5
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbSelectionOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCellSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCellSelect
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciCellMultiSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbCellMultiSelect
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRecordMultiSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbRecordMultiSelect
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcSelectionPageGroup1: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageGroup2: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcSelectionPageGroup3: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup4: TdxLayoutGroup
      Parent = lcSelectionPageGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup4
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup5: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcSelectionPageGroup6: TdxLayoutGroup
      Parent = lcSelectionPageGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'chbHideSelection'
      CaptionOptions.Visible = False
      Control = chbHideSelection
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciGridLines: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbGridLines
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciHorizontal: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbHorizontal
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciVertical: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbVertical
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lciSelectionSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
  end
end
