object cxSchedulerEventEditorForm: TcxSchedulerEventEditorForm
  Left = 313
  Top = 267
  ClientHeight = 477
  ClientWidth = 442
  Color = clBtnFace
  Constraints.MinHeight = 410
  Constraints.MinWidth = 458
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 442
    Height = 477
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    HighlightRoot = False
    object cxGroupBox1: TcxGroupBox
      Left = 10
      Top = 10
      Alignment = alCenterCenter
      Anchors = [akLeft, akTop, akRight]
      ParentBackground = False
      ParentColor = False
      Style.Color = clInfoBk
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 23
      Width = 296
      object lbInformation: TcxLabel
        Left = 4
        Top = 4
        AutoSize = False
        Caption = 'Conflicts with another event in your schedule.'
        Style.TransparentBorder = False
        Transparent = True
        Height = 15
        Width = 290
      end
    end
    object btnFindTime: TcxButton
      Left = 312
      Top = 10
      Width = 120
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Find available time'
      TabOrder = 1
      OnClick = btnFindTimeClick
    end
    object teSubject: TcxTextEdit
      Left = 87
      Top = 51
      Anchors = [akLeft, akTop, akRight]
      Properties.OnChange = OnChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 345
    end
    object teLocation: TcxTextEdit
      Left = 87
      Top = 76
      Properties.OnChange = OnChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 184
    end
    object icbLabel: TcxImageComboBox
      Left = 311
      Top = 76
      Properties.Items = <>
      Properties.OnChange = OnChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 121
    end
    object deStart: TcxDateEdit
      Left = 87
      Top = 113
      Properties.DateButtons = [btnToday]
      Properties.InputKind = ikStandard
      Properties.OnChange = OnChanged
      Properties.OnEditValueChanged = StartDateChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 121
    end
    object teStart: TcxTimeEdit
      Left = 214
      Top = 113
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = OnEventTimeChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Width = 121
    end
    object cbAllDayEvent: TcxCheckBox
      Left = 341
      Top = 113
      Caption = 'All day event'
      Properties.OnChange = cbAllDayEventPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
    end
    object deEnd: TcxDateEdit
      Left = 87
      Top = 140
      Properties.DateButtons = [btnToday]
      Properties.InputKind = ikStandard
      Properties.OnChange = OnChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Width = 121
    end
    object teEnd: TcxTimeEdit
      Left = 214
      Top = 140
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = OnEventTimeChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 121
    end
    object seTaskComplete: TcxSpinEdit
      Left = 87
      Top = 177
      Properties.AssignedValues.EditFormat = True
      Properties.AssignedValues.MinValue = True
      Properties.Increment = 25.000000000000000000
      Properties.LargeIncrement = 25.000000000000000000
      Properties.MaxValue = 100.000000000000000000
      Properties.OnChange = seTaskCompleteChange
      Properties.OnEditValueChanged = seTaskCompleteChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Width = 121
    end
    object cbxTaskStatus: TcxComboBox
      Left = 278
      Top = 177
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxTaskStatusChange
      Properties.OnEditValueChanged = cbxTaskStatusChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Width = 121
    end
    object lbRecurrencePattern: TcxLabel
      Left = 87
      Top = 214
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'PatternInfo'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.WordWrap = True
      Transparent = True
      Height = 14
      Width = 345
    end
    object cbResources: TcxCheckComboBox
      Left = 87
      Top = 234
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownAutoWidth = False
      Properties.EditValueFormat = cvfIndices
      Properties.Items = <>
      Properties.OnChange = OnResourceIDChanged
      Properties.OnClickCheck = cbResourcesPropertiesClickCheck
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Width = 345
    end
    object cbReminder: TcxCheckBox
      Left = 10
      Top = 271
      Caption = 'Reminder:'
      Properties.OnChange = OnChanged
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 14
      Transparent = True
      OnClick = cbReminderClick
    end
    object cbReminderMinutesBeforeStart: TcxComboBox
      Left = 86
      Top = 271
      Properties.ImmediateDropDownWhenKeyPressed = False
      Properties.ImmediatePost = True
      Properties.IncrementalSearch = False
      Properties.OnChange = OnChanged
      Properties.OnPopup = cbReminderMinutesBeforeStartPropertiesPopup
      Properties.OnValidate = cbReminderMinutesBeforeStartPropertiesValidate
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Width = 121
    end
    object icbShowTimeAs: TcxImageComboBox
      Left = 285
      Top = 271
      Properties.Items = <>
      Properties.OnChange = OnEventTimeChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Width = 147
    end
    object meMessage: TcxMemo
      Left = 10
      Top = 308
      Anchors = [akLeft, akTop, akRight, akBottom]
      Properties.OnChange = OnChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Height = 130
      Width = 422
    end
    object btnOk: TcxButton
      Left = 114
      Top = 444
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 18
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton
      Left = 195
      Top = 444
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 19
      OnClick = btnCancelClick
    end
    object btnDelete: TcxButton
      Left = 276
      Top = 444
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 20
      OnClick = btnDeleteClick
    end
    object btnRecurrence: TcxButton
      Left = 357
      Top = 444
      Width = 75
      Height = 23
      Caption = '&Recurrence'
      TabOrder = 21
      OnClick = btnRecurrenceClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 6
      ShowBorder = False
      Index = -1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = pnlInformation
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object pnlInformation: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = pnlInformation
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxGroupBox1'
      CaptionOptions.Visible = False
      Control = cxGroupBox1
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 570
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'btnFindTime'
      CaptionOptions.Visible = False
      Control = btnFindTime
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlCaption: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lbSubject: TdxLayoutItem
      Parent = pnlCaption
      CaptionOptions.Text = 'Subject:'
      Control = teSubject
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 641
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = pnlCaption
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lbLocation: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'Location:'
      Control = teLocation
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbLabel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'La&bel:'
      Control = icbLabel
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlTime: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 2
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = pnlTime
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = pnlTime
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lbStartTime: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Start time:'
      Control = deStart
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liStart: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      Control = teStart
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbAllDayEvent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 84
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = pnlTime
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lbEndTime: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'End time:'
      Control = deEnd
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liFinish: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      Control = teEnd
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlTaskComplete: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 3
    end
    object dxLayoutSeparatorItem3: TdxLayoutSeparatorItem
      Parent = pnlTaskComplete
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = pnlTaskComplete
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lbTaskComplete: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Task complete:'
      Control = seTaskComplete
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbTaskStatus: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Text = 'Task status:'
      Control = cbxTaskStatus
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlRecurrenceInfo: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 4
    end
    object dxLayoutSeparatorItem4: TdxLayoutSeparatorItem
      Parent = pnlRecurrenceInfo
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object lbRecurrence: TdxLayoutItem
      Parent = pnlRecurrenceInfo
      CaptionOptions.Text = 'Recurrence:'
      Control = lbRecurrencePattern
      ControlOptions.OriginalHeight = 14
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlResource: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 5
    end
    object lbResource: TdxLayoutItem
      Parent = pnlResource
      CaptionOptions.Text = 'Resource(s):'
      Control = cbResources
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object pnlPlaceHolder: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 6
    end
    object dxLayoutSeparatorItem5: TdxLayoutSeparatorItem
      Parent = pnlPlaceHolder
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = pnlPlaceHolder
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object pnlReminder: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = pnlReminder
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = cbReminder
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = pnlReminder
      Control = cbReminderMinutesBeforeStart
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlShowTimeAs: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lbShowTimeAs: TdxLayoutItem
      Parent = pnlShowTimeAs
      AlignHorz = ahClient
      CaptionOptions.Text = 'Show time as:'
      Control = icbShowTimeAs
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object pnlMessage: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 7
    end
    object dxLayoutSeparatorItem6: TdxLayoutSeparatorItem
      Parent = pnlMessage
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = pnlMessage
      AlignVert = avClient
      Control = meMessage
      ControlOptions.OriginalHeight = 101
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlThreeButtons: TdxLayoutGroup
      Parent = pnlButtons
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = pnlThreeButtons
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = pnlThreeButtons
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = pnlThreeButtons
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object pnlRecurrence: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnRecurrence
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlButtons: TdxLayoutGroup
      Parent = pnlMessage
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 816
    Top = 112
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
