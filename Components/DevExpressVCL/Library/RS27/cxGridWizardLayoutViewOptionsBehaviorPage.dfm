inherited cxGridWizardLayoutViewOptionsBehaviorPageFrame: TcxGridWizardLayoutViewOptionsBehaviorPageFrame
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
    object chbItemHotTrack: TcxCheckBox [1]
      Left = 377
      Top = 355
      Caption = 'Item hot track'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      Width = 253
    end
    object lbExpandingOptions: TcxLabel [2]
      Left = 361
      Top = 10
      Caption = 'Expanding options'
      Style.HotTrack = False
      Transparent = True
    end
    object chbCellEndEllipsis: TcxCheckBox [3]
      Left = 377
      Top = 332
      Caption = 'Cell end ellipsis'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Width = 253
    end
    object lbOther: TcxLabel [4]
      Left = 361
      Top = 309
      Caption = 'Other'
      Style.HotTrack = False
      Transparent = True
    end
    object chbCellSelect: TcxCheckBox [5]
      Left = 377
      Top = 217
      Caption = 'Cell select'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Width = 253
    end
    object chbMultiSelect: TcxCheckBox [6]
      Left = 377
      Top = 240
      Caption = 'Record multiselect'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Width = 253
    end
    object chbHideSelection: TcxCheckBox [7]
      Left = 377
      Top = 263
      Caption = 'Hide selection'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Width = 253
    end
    object chbIncSearch: TcxCheckBox [8]
      Left = 377
      Top = 286
      Caption = 'Incremental search'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Width = 253
    end
    object lbSelectionOptions: TcxLabel [9]
      Left = 361
      Top = 194
      Caption = 'Selection options'
      Style.HotTrack = False
      Transparent = True
    end
    object chbFocusCellOnTab: TcxCheckBox [10]
      Left = 377
      Top = 125
      Caption = 'Focus cell on tab'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 253
    end
    object chbFocusFirstCellOnNewRecord: TcxCheckBox [11]
      Left = 377
      Top = 148
      Caption = 'Focus first cell on new record'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Width = 253
    end
    object chbGoToNextCellOnEnter: TcxCheckBox [12]
      Left = 377
      Top = 171
      Caption = 'Go to next cell on Enter'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 253
    end
    object lbFocusingOptions: TcxLabel [13]
      Left = 361
      Top = 102
      Caption = 'Focusing options'
      Style.HotTrack = False
      Transparent = True
    end
    object chbRecordExpanding: TcxCheckBox [14]
      Left = 377
      Top = 33
      Caption = 'Record expanding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 253
    end
    object chbGroupExpanding: TcxCheckBox [15]
      Left = 377
      Top = 56
      Caption = 'Group expanding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 253
    end
    object chbExpandRecordOnDblClick: TcxCheckBox [16]
      Left = 377
      Top = 79
      Caption = 'Expand record on double click'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
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
    object lciItemHotTrack: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbItemHotTrack
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciExpandingOptions: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbExpandingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup6: TdxLayoutGroup
      Parent = lcMainGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Visible = False
      Control = chbCellEndEllipsis
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 7
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 6
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Visible = False
      Control = lbOther
      ControlOptions.ShowBorder = False
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
    object lcSelectionPageGroup4: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lciCellSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCellSelect
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMultiSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbMultiSelect
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Visible = False
      Control = chbHideSelection
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Visible = False
      Control = chbIncSearch
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcSelectionPageGroup9: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lciSelectionOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup9
      CaptionOptions.Text = 'cxLabel3'
      CaptionOptions.Visible = False
      Control = lbSelectionOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup10: TdxLayoutGroup
      Parent = lcSelectionPageGroup9
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup10
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup3: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lciFocusCellOnTab: TdxLayoutItem
      Parent = lcSelectionPageGroup3
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chbFocusCellOnTab
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFocusFirstCellOnNewRecord: TdxLayoutItem
      Parent = lcSelectionPageGroup3
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = chbFocusFirstCellOnNewRecord
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciGoToNextCellOnEnter: TdxLayoutItem
      Parent = lcSelectionPageGroup3
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = chbGoToNextCellOnEnter
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcSelectionPageGroup7: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lciFocusingOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup7
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbFocusingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup8: TdxLayoutGroup
      Parent = lcSelectionPageGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup8
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup2: TdxLayoutGroup
      Parent = lcMainGroup4
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciRecordExpanding: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox7'
      CaptionOptions.Visible = False
      Control = chbRecordExpanding
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciGroupExpanding: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox8'
      CaptionOptions.Visible = False
      Control = chbGroupExpanding
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciExpandRecordOnDblClick: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox9'
      CaptionOptions.Visible = False
      Control = chbExpandRecordOnDblClick
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
end
