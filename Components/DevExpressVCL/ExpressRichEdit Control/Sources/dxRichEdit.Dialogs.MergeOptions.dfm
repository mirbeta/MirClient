inherited dxRichEditMergeOptionsDialogForm: TdxRichEditMergeOptionsDialogForm
  Caption = 'Merge Options'
  ClientHeight = 197
  ClientWidth = 234
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 234
    Height = 197
    object rbMergeAllRecords: TcxRadioButton [0]
      Left = 18
      Top = 30
      Width = 113
      Height = 17
      Caption = '&All'
      TabOrder = 0
      OnClick = rbMergeRecordsClick
      Transparent = True
    end
    object rbMergeSelectedRecords: TcxRadioButton [1]
      Tag = 2
      Left = 18
      Top = 53
      Width = 113
      Height = 17
      Caption = '&Selected'
      TabOrder = 1
      OnClick = rbMergeRecordsClick
      Transparent = True
    end
    object btnOk: TcxButton [2]
      Left = 68
      Top = 119
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object btnCancel: TcxButton [3]
      Left = 149
      Top = 119
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object rbMergeToWindow: TcxRadioButton [4]
      Left = 18
      Top = 96
      Width = 96
      Height = 17
      Caption = '&Window'
      TabOrder = 2
      OnClick = rbMergeToClick
      GroupIndex = 1
      Transparent = True
    end
    object rbMergeToFile: TcxRadioButton [5]
      Left = 120
      Top = 96
      Width = 96
      Height = 17
      Caption = '&File'
      TabOrder = 3
      OnClick = rbMergeToClick
      GroupIndex = 1
      Transparent = True
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      CaptionOptions.Visible = False
      Index = -1
    end
    object liMergeRecords: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Merge records'
      CaptionOptions.Visible = True
      Index = 0
    end
    object liMergeTo: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Merge to'
      CaptionOptions.Visible = True
      Index = 2
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbMergeAllRecords
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbMergeSelectedRecords
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbMergeToWindow
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbMergeToFile
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 128
  end
end
