inherited cxGridWizardCardViewOptionsSizingPageFrame: TcxGridWizardCardViewOptionsSizingPageFrame
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
    object chbCardSizing: TcxCheckBox [1]
      Left = 377
      Top = 171
      Caption = 'Card sizing'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Width = 253
    end
    object lbAutoSizingOptions: TcxLabel [2]
      Left = 361
      Top = 10
      Caption = 'Auto Sizing Options'
      Style.HotTrack = False
    end
    object lbManualSizingOptions: TcxLabel [3]
      Left = 361
      Top = 148
      Caption = 'Manual Sizing Options'
      Style.HotTrack = False
    end
    object chbCellAutoHeight: TcxCheckBox [4]
      Left = 377
      Top = 33
      Caption = 'Cell auto height'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 253
    end
    object chbCellEndEllipsis: TcxCheckBox [5]
      Left = 377
      Top = 56
      Caption = 'Cell end ellipsis'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 253
    end
    object chbRowCaptionAutoHeight: TcxCheckBox [6]
      Left = 377
      Top = 79
      Caption = 'Row caption auto height'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 253
    end
    object chbRowCaptionEndEllipsis: TcxCheckBox [7]
      Left = 377
      Top = 102
      Caption = 'Row caption end ellipsis'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 253
    end
    object chbCardAutoWidth: TcxCheckBox [8]
      Left = 377
      Top = 125
      Caption = 'Card auto width'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
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
    object lciCardSizing: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbCardSizing
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbAutoSizingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup4: TdxLayoutGroup
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
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
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup6: TdxLayoutGroup
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup6
      CaptionOptions.Visible = False
      Control = lbManualSizingOptions
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcMainGroup5
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lciSizingPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciCellAutoHeight: TdxLayoutItem
      Parent = lciSizingPageGroup1
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbCellAutoHeight
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcCellEndEllipsis: TdxLayoutItem
      Parent = lciSizingPageGroup1
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbCellEndEllipsis
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRowCaptionAutoHeight: TdxLayoutItem
      Parent = lciSizingPageGroup1
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbRowCaptionAutoHeight
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcRowCaptionEndEllipsis: TdxLayoutItem
      Parent = lciSizingPageGroup1
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbRowCaptionEndEllipsis
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciCardAutoWidth: TdxLayoutItem
      Parent = lciSizingPageGroup1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Control = chbCardAutoWidth
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
end
