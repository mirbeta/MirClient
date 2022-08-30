inherited cxGridWizardTableViewOptionsSizingPageFrame: TcxGridWizardTableViewOptionsSizingPageFrame
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
    object chbCellAutoheight: TcxCheckBox [1]
      Left = 377
      Top = 33
      Caption = 'Cell autoheight'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbCellAutoheightClick
      Width = 250
    end
    object chbColumnAutowidth: TcxCheckBox [2]
      Left = 377
      Top = 79
      Caption = 'Column autowidth'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 250
    end
    object chbDataRowSizing: TcxCheckBox [3]
      Left = 377
      Top = 171
      Caption = 'Data row sizing'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 250
    end
    object chbFooterAutoheight: TcxCheckBox [4]
      Left = 377
      Top = 102
      Caption = 'Footer autoheight'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 250
    end
    object chbGroupRowSizing: TcxCheckBox [5]
      Left = 377
      Top = 194
      Caption = 'Group row sizing'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Width = 250
    end
    object chbHeaderAutoheight: TcxCheckBox [6]
      Left = 377
      Top = 125
      Caption = 'Header autoheight'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 250
    end
    object chbCellEndEllipsis: TcxCheckBox [7]
      Left = 377
      Top = 56
      Caption = 'Cell end ellipsis'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 250
    end
    object lbAutoSizingOptions: TcxLabel [8]
      Left = 361
      Top = 10
      Caption = 'Auto sizing options'
      Style.HotTrack = False
      Transparent = True
    end
    object lbManualSizingOptions: TcxLabel [9]
      Left = 361
      Top = 148
      Caption = 'Manual sizing options'
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
    object lciCellAutoheight: TdxLayoutItem
      Parent = lcSizingPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCellAutoheight
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciColumnAutowidth: TdxLayoutItem
      Parent = lcSizingPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbColumnAutowidth
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciDataRowSizing: TdxLayoutItem
      Parent = lcSizingPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbDataRowSizing
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFooterAutoheight: TdxLayoutItem
      Parent = lcSizingPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chbFooterAutoheight
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciGroupRowSizing: TdxLayoutItem
      Parent = lcSizingPageGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = chbGroupRowSizing
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciHeaderAutoheight: TdxLayoutItem
      Parent = lcSizingPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = chbHeaderAutoheight
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lciCellEndEllipsis: TdxLayoutItem
      Parent = lcSizingPageGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbCellEndEllipsis
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcSizingPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciAutoSizingOptions: TdxLayoutItem
      Parent = lcSizingPageGroup4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbAutoSizingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciManualSizingOptions: TdxLayoutItem
      Parent = lcSizingPageGroup6
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      Control = lbManualSizingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcSizingPageGroup2: TdxLayoutGroup
      Parent = lcSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcSizingPageGroup3: TdxLayoutGroup
      Parent = lcSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lcSizingPageGroup4: TdxLayoutGroup
      Parent = lcSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcSizingPageGroup5: TdxLayoutGroup
      Parent = lcSizingPageGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcSizingPageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcSizingPageGroup5
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSizingPageSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcSizingPageGroup7
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lcSizingPageGroup6: TdxLayoutGroup
      Parent = lcSizingPageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcSizingPageGroup7: TdxLayoutGroup
      Parent = lcSizingPageGroup6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
  end
end
