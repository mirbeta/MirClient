inherited cxGridWizardServerModeViewsDataSourcePageFrame: TcxGridWizardServerModeViewsDataSourcePageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    CustomizeFormTabbedView = True
    object cbDataSource: TcxComboBox [0]
      Left = 28
      Top = 33
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbDataSourcePropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 215
    end
    object btnTestConnection: TcxButton [1]
      Left = 28
      Top = 60
      Width = 215
      Height = 25
      Caption = 'Test connection'
      TabOrder = 2
      OnClick = btnTestConnectionClick
    end
    object lbDataSource: TcxLabel [2]
      Left = 28
      Top = 10
      Caption = 'Data Source:'
      Style.HotTrack = False
      Transparent = True
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = -1
    end
    object lciDataSource: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      Control = cbDataSource
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgCommonGroup: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lciTestConnection: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnTestConnection
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcgCommonGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbDataSource
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
end
