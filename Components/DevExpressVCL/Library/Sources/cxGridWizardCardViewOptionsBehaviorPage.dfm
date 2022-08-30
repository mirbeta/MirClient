inherited cxGridWizardCardViewOptionsBehaviorPageFrame: TcxGridWizardCardViewOptionsBehaviorPageFrame
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
    object chbCardExpanding: TcxCheckBox [1]
      Left = 377
      Top = 33
      Caption = 'Card expanding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 253
    end
    object chbRowExpanding: TcxCheckBox [2]
      Left = 377
      Top = 56
      Caption = 'Row expanding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 253
    end
    object chbExpandRowOnDblClick: TcxCheckBox [3]
      Left = 377
      Top = 79
      Caption = 'Expand row on double click'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 253
    end
    object chbFocusCellOnTab: TcxCheckBox [4]
      Left = 377
      Top = 125
      Caption = 'Focus cell on Tab'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 253
    end
    object chbFocusFirstCellOnNewRecord: TcxCheckBox [5]
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
    object chbGoToNextCellOnEnter: TcxCheckBox [6]
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
    object chbCellSelect: TcxCheckBox [7]
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
    object chbMultiSelect: TcxCheckBox [8]
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
    object chbHideSelection: TcxCheckBox [9]
      Left = 377
      Top = 263
      Caption = 'Hide selection'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Width = 253
    end
    object chbRowHiding: TcxCheckBox [10]
      Left = 377
      Top = 332
      Caption = 'Row hiding'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Width = 253
    end
    object chbRowMoving: TcxCheckBox [11]
      Left = 377
      Top = 355
      Caption = 'Row moving'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      Width = 253
    end
    object lbExpandingOptions: TcxLabel [12]
      Left = 361
      Top = 10
      Caption = 'Expanding options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbRowOptions: TcxLabel [13]
      Left = 361
      Top = 309
      Caption = 'Row options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbSelectionOptions: TcxLabel [14]
      Left = 361
      Top = 194
      Caption = 'Selection options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbFocusingOptions: TcxLabel [15]
      Left = 361
      Top = 102
      Caption = 'Focusing options'
      Style.HotTrack = False
      Transparent = True
    end
    object cbIncSearch: TcxCheckBox [16]
      Left = 377
      Top = 286
      Caption = 'Incremental Search'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
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
    object lciCardExpanding: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCardExpanding
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRowExpanding: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Visible = False
      Control = chbRowExpanding
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciExpandRowOnDblClick: TdxLayoutItem
      Parent = lcSelectionPageGroup2
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Visible = False
      Control = chbExpandRowOnDblClick
      ControlOptions.ShowBorder = False
      Index = 2
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
    object lciCellSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Text = 'cxCheckBox7'
      CaptionOptions.Visible = False
      Control = chbCellSelect
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMultiSelect: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Text = 'cxCheckBox8'
      CaptionOptions.Visible = False
      Control = chbMultiSelect
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciHideSelection: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Text = 'cxCheckBox9'
      CaptionOptions.Visible = False
      Control = chbHideSelection
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciRowHiding: TdxLayoutItem
      Parent = lcSelectionPageGroup5
      CaptionOptions.Text = 'cxCheckBox10'
      CaptionOptions.Visible = False
      Control = chbRowHiding
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRowMoving: TdxLayoutItem
      Parent = lcSelectionPageGroup5
      CaptionOptions.Text = 'cxCheckBox11'
      CaptionOptions.Visible = False
      Control = chbRowMoving
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcSelectionPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciExpandingOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup6
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbExpandingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRowOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup12
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbRowOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciSelectionOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup10
      CaptionOptions.Text = 'cxLabel3'
      CaptionOptions.Visible = False
      Control = lbSelectionOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFocusingOptions: TdxLayoutItem
      Parent = lcSelectionPageGroup8
      CaptionOptions.Text = 'cxLabel4'
      CaptionOptions.Visible = False
      Control = lbFocusingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup2: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageGroup3: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcSelectionPageGroup4: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 5
    end
    object lcSelectionPageGroup5: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 7
    end
    object lcSelectionPageGroup6: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcSelectionPageGroup7: TdxLayoutGroup
      Parent = lcSelectionPageGroup6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup7
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup8: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcSelectionPageGroup9: TdxLayoutGroup
      Parent = lcSelectionPageGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup9
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup10: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcSelectionPageGroup11: TdxLayoutGroup
      Parent = lcSelectionPageGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup11
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSelectionPageGroup12: TdxLayoutGroup
      Parent = lcSelectionPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 6
    end
    object lcSelectionPageGroup13: TdxLayoutGroup
      Parent = lcSelectionPageGroup12
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSelectionPageSeparatorItem4: TdxLayoutSeparatorItem
      Parent = lcSelectionPageGroup13
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcSelectionPageGroup4
      CaptionOptions.Visible = False
      Control = cbIncSearch
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
end
