inherited cxGridWizardDBViewsDataSourcePageFrame: TcxGridWizardDBViewsDataSourcePageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    object cbDataSource: TcxComboBox [0]
      Left = 28
      Top = 52
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbDataSourcePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 215
    end
    object cbKeyField: TcxCheckComboBox [1]
      Left = 28
      Top = 98
      Properties.EditValueFormat = cvfStatesString
      Properties.Items = <>
      Properties.OnChange = cbKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 215
    end
    object lbCommon: TcxLabel [2]
      Left = 10
      Top = 10
      Caption = 'Common'
      Style.HotTrack = False
      Transparent = True
    end
    object chbDetail: TcxCheckBox [3]
      Left = 10
      Top = 141
      Caption = 'Is detail view'
      Properties.OnChange = chbDetailPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 85
    end
    object cbMasterView: TcxComboBox [4]
      Left = 28
      Top = 183
      Enabled = False
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbMasterViewPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 215
    end
    object cbDetailKeyField: TcxCheckComboBox [5]
      Left = 28
      Top = 275
      Enabled = False
      Properties.EditValueFormat = cvfStatesString
      Properties.Items = <>
      Properties.OnChange = cbDetailKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Width = 215
    end
    object cbMasterKeyField: TcxCheckComboBox [6]
      Left = 28
      Top = 229
      Enabled = False
      Properties.EditValueFormat = cvfStatesString
      Properties.Items = <>
      Properties.OnChange = cbMasterKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 215
    end
    object lbDataSource: TcxLabel [7]
      Left = 28
      Top = 33
      Caption = 'Data Source:'
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbKeyFields: TcxLabel [8]
      Left = 28
      Top = 79
      Caption = 'Key Fields:'
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbMasterView: TcxLabel [9]
      Left = 28
      Top = 164
      Caption = 'Master View:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbMasterKeyFields: TcxLabel [10]
      Left = 28
      Top = 210
      Caption = 'Master Key Fields:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbDetailKeyField: TcxLabel [11]
      Left = 28
      Top = 256
      Caption = 'Detail Key Field:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = -1
    end
    object lcDataSourcePageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lciDataSource: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      Control = cbDataSource
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciKeyField: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      Control = cbKeyField
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciCommon: TdxLayoutItem
      Parent = lcgCommonCaptionGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbCommon
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDetail: TdxLayoutItem
      Parent = lcgIsDetailGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbDetail
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgIsDetailGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object lcDataSourcePageGroup3: TdxLayoutGroup
      Parent = lcgIsDetailGroup
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcDataSourcePageSeparatorItem: TdxLayoutSeparatorItem
      Parent = lcDataSourcePageGroup3
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lciMasterView: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbMasterView
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciDetailKeyField: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbDetailKeyField
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcgCommonGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcgDetailGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Enabled = False
      Hidden = True
      ShowBorder = False
      Index = 4
    end
    object lcgCommonCaptionGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcDataSourcePageGroup6: TdxLayoutGroup
      Parent = lcgCommonCaptionGroup
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcDataSourcePageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcDataSourcePageGroup6
      AlignVert = avCenter
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object lciMasterKeyField: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbMasterKeyField
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcgCommonGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbDataSource
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcgCommonGroup
      CaptionOptions.Visible = False
      Control = lbKeyFields
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbMasterView
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbMasterKeyFields
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbDetailKeyField
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'Empty Space Item'
      Visible = False
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
  end
end
