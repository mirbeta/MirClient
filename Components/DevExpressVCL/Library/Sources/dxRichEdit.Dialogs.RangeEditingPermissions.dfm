inherited dxRangeEditingPermissionsForm: TdxRangeEditingPermissionsForm
  Caption = 'Editing Permissions'
  ClientHeight = 265
  ClientWidth = 481
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 481
    Height = 265
    object ListUserGroups: TcxCheckListBox [0]
      Left = 236
      Top = 28
      Width = 220
      Height = 184
      Items = <>
      TabOrder = 1
    end
    object ListUsers: TcxCheckListBox [1]
      Left = 10
      Top = 28
      Width = 220
      Height = 184
      Items = <>
      TabOrder = 0
    end
    object btnOk: TcxButton [2]
      Left = 219
      Top = 218
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 3
      OnClick = btnApplyClick
    end
    object btnCancel: TcxButton [3]
      Left = 300
      Top = 218
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnApply: TcxButton [4]
      Left = 381
      Top = 218
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 5
      OnClick = btnApplyClick
    end
    object btnMoreUsers: TButton [5]
      Left = 10
      Top = 218
      Width = 75
      Height = 25
      Caption = 'More users...'
      TabOrder = 2
      OnClick = btnMoreUsersClick
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object liGroups: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Groups:'
      CaptionOptions.Layout = clTop
      Control = ListUserGroups
      ControlOptions.OriginalHeight = 184
      ControlOptions.OriginalWidth = 220
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liUsers: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Users:'
      CaptionOptions.Layout = clTop
      Control = ListUsers
      ControlOptions.OriginalHeight = 184
      ControlOptions.OriginalWidth = 220
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Button1'
      CaptionOptions.Visible = False
      Control = btnMoreUsers
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
end
