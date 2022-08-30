inherited cxGridWizardTableViewOptionsSummaryPageFrame: TcxGridWizardTableViewOptionsSummaryPageFrame
  inherited lcMain: TdxLayoutControl
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 127
      Height = 159
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object chbNullIgnore: TcxCheckBox [1]
      Left = 361
      Top = 91
      Caption = 'Null ignore'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 79
    end
    object chbSelectedRecords: TcxRadioButton [2]
      Left = 361
      Top = 33
      Width = 125
      Height = 17
      Caption = 'For selected records'
      TabOrder = 2
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object chbMultipleSelectedRecords: TcxRadioButton [3]
      Left = 361
      Top = 56
      Width = 165
      Height = 17
      Caption = 'For multiple selected records'
      TabOrder = 3
      OnClick = RefreshPreviewGrid
      Transparent = True
    end
    object rbAllRecords: TcxRadioButton [4]
      Left = 361
      Top = 10
      Width = 125
      Height = 17
      Caption = 'For all records'
      TabOrder = 1
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
    object lciNullIgnore: TdxLayoutItem
      Parent = lcSummaryCustomizationPageGroup1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbNullIgnore
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciSelectedRecords: TdxLayoutItem
      Parent = lcSummaryCustomizationPageGroup1
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chbSelectedRecords
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMultipleSelectedRecords: TdxLayoutItem
      Parent = lcSummaryCustomizationPageGroup1
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbMultipleSelectedRecords
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcSummaryCustomizationPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcSummaryCustomizationPageGroup1
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 2
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Visible = False
      Control = rbAllRecords
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
  end
  object pupmSummaryCustomization: TcxGridPopupMenu
    PopupMenus = <
      item
        HitTypes = [gvhtGridNone, gvhtGridTab, gvhtNone, gvhtTab, gvhtCell, gvhtExpandButton, gvhtRecord, gvhtNavigator, gvhtPreview, gvhtColumnHeader, gvhtColumnHeaderFilterButton, gvhtFilter, gvhtGroupByBox, gvhtIndicator, gvhtRowIndicator, gvhtRowLevelIndent, gvhtBand, gvhtBandHeader, gvhtRowCaption, gvhtSeparator]
        Index = 0
      end>
    Left = 36
    Top = 4
  end
end
