inherited cxGridWizardDBViewsDataLoadingSettingsPageFrame: TcxGridWizardDBViewsDataLoadingSettingsPageFrame
  inherited lcMain: TdxLayoutControl
    CustomizeFormTabbedView = True
    object chbSmartRefresh: TcxCheckBox [0]
      Left = 361
      Top = 78
      Caption = 'Smart refresh'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 95
    end
    object chbSyncMode: TcxCheckBox [1]
      Left = 361
      Top = 101
      Caption = 'Sync mode'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 87
    end
    object pnPreviewGrid: TPanel [2]
      Left = 10
      Top = 10
      Width = 185
      Height = 41
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object chbSynchronization: TcxCheckBox [3]
      Left = 361
      Top = 124
      Caption = 'Synchronization'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 107
    end
    object cbMultiThreadedOptionsFiltering: TcxComboBox [4]
      Left = 361
      Top = 165
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Disabled'
        'Enabled'
        'Default')
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 6
      Width = 121
    end
    object cbMultiThreadedOptionsSorting: TcxComboBox [5]
      Left = 361
      Top = 210
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Disabled'
        'Enabled'
        'Default')
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 7
      Width = 121
    end
    object chbGridMode: TcxCheckBox [6]
      Left = 361
      Top = 10
      Caption = 'Grid mode'
      Properties.OnChange = chbGridModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Width = 74
    end
    object seGridModeBufferCount: TcxSpinEdit [7]
      Left = 379
      Top = 51
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 2
      Value = 1
      Width = 49
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Text = 'Hidden Group'
      LayoutDirection = ldHorizontal
      Index = -1
    end
    object lciSmartRefresh: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Visible = False
      Control = chbSmartRefresh
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciSyncMode: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbSyncMode
      ControlOptions.ShowBorder = False
      Index = 3
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
    object lciSynchronization: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbSynchronization
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcLoadSettingsPageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciMultiThreadedOptionsFiltering: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'MultiThreaded Filtering'
      CaptionOptions.Layout = clTop
      Control = cbMultiThreadedOptionsFiltering
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lciMultiThreadedOptionsSorting: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'MultiThreaded Sorting'
      CaptionOptions.Layout = clTop
      Control = cbMultiThreadedOptionsSorting
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lciGridMode: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Visible = False
      Control = chbGridMode
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciGridModeBufferCount: TdxLayoutItem
      Parent = lcLoadSettingsPageGroup1
      CaptionOptions.Text = 'Grid mode buffer count'
      CaptionOptions.Layout = clTop
      Offsets.Left = 18
      Visible = False
      Control = seGridModeBufferCount
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
end
