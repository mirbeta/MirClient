object cxSchedulerRecurrenceEventEditorForm: TcxSchedulerRecurrenceEventEditorForm
  Left = 277
  Top = 263
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Event recurrence'
  ClientHeight = 369
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 561
    Height = 369
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object btnOk: TcxButton
      Left = 103
      Top = 321
      Width = 110
      Height = 23
      Caption = '&Ok'
      Default = True
      TabOrder = 38
      OnClick = btnOkClick
    end
    object btnRemove: TcxButton
      Left = 335
      Top = 321
      Width = 110
      Height = 23
      Caption = '&Remove recurrence'
      Enabled = False
      ModalResult = 3
      TabOrder = 40
    end
    object btnCancel: TcxButton
      Left = 219
      Top = 321
      Width = 110
      Height = 23
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 39
    end
    object deStart: TcxDateEdit
      Left = 55
      Top = 216
      Properties.DateButtons = [btnToday]
      Properties.InputKind = ikStandard
      Properties.OnChange = DoChange
      Properties.OnEditValueChanged = deStartPropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 32
      Width = 135
    end
    object rbNoEndDate: TcxRadioButton
      Left = 196
      Top = 216
      Width = 113
      Height = 17
      Caption = 'No end date'
      Checked = True
      TabOrder = 33
      TabStop = True
      OnClick = rbNoEndDateClick
      GroupIndex = 1
      Transparent = True
    end
    object rbEndAfter: TcxRadioButton
      Left = 196
      Top = 241
      Width = 113
      Height = 17
      Caption = 'End after:'
      TabOrder = 34
      OnClick = DoChange
      GroupIndex = 1
      Transparent = True
    end
    object meEndAfter: TcxMaskEdit
      Tag = 11452416
      Left = 315
      Top = 239
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = meEndAfterPropertiesChange
      Properties.OnEditValueChanged = meEndAfterPropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 35
      OnExit = meEndAfterExit
      Width = 46
    end
    object rbEndBy: TcxRadioButton
      Left = 196
      Top = 268
      Width = 113
      Height = 17
      Caption = 'End by:'
      TabOrder = 36
      OnClick = DoChange
      GroupIndex = 1
      Transparent = True
    end
    object deEndBy: TcxDateEdit
      Tag = 11453468
      Left = 315
      Top = 266
      Properties.DateButtons = [btnToday]
      Properties.InputKind = ikStandard
      Properties.OnChange = deEndByPropertiesChange
      Properties.OnEditValueChanged = deEndByPropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 37
      Width = 109
    end
    object rbYearly: TcxRadioButton
      Tag = 3
      Left = 22
      Top = 156
      Width = 80
      Height = 17
      Caption = 'Yearly'
      TabOrder = 6
      OnClick = SelectPeriodicityClick
      GroupIndex = 2
      Transparent = True
    end
    object rbDaily: TcxRadioButton
      Left = 22
      Top = 87
      Width = 80
      Height = 17
      Caption = 'Daily'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = SelectPeriodicityClick
      GroupIndex = 2
      Transparent = True
    end
    object rbWeekly: TcxRadioButton
      Tag = 1
      Left = 22
      Top = 110
      Width = 80
      Height = 17
      Caption = 'Weekly'
      TabOrder = 4
      OnClick = SelectPeriodicityClick
      GroupIndex = 2
      Transparent = True
    end
    object rbMonthly: TcxRadioButton
      Tag = 2
      Left = 22
      Top = 133
      Width = 80
      Height = 17
      Caption = 'Monthly'
      TabOrder = 5
      OnClick = SelectPeriodicityClick
      GroupIndex = 2
      Transparent = True
    end
    object rbDay: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 60
      Height = 17
      Caption = 'Day'
      Checked = True
      TabOrder = 7
      TabStop = True
      Visible = False
      OnClick = DoChange
      GroupIndex = 3
      Transparent = True
    end
    object meNumOfDay: TcxMaskEdit
      Tag = 11533468
      Left = 10000
      Top = 10000
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = SetDayRadioButtonChecked
      Style.HotTrack = False
      TabOrder = 8
      Visible = False
      OnExit = ValidateNumber
      Width = 32
    end
    object meNumMonth: TcxMaskEdit
      Tag = 11533468
      Left = 10000
      Top = 10000
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = SetDayRadioButtonChecked
      Style.HotTrack = False
      TabOrder = 9
      Visible = False
      OnExit = ValidateNumber
      Width = 32
    end
    object rbThe: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 60
      Height = 17
      Caption = 'The'
      TabOrder = 10
      Visible = False
      OnClick = DoChange
      GroupIndex = 3
      Transparent = True
    end
    object cbWeek: TcxComboBox
      Tag = 11534460
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = SetTheRadioButtonChecked
      Style.HotTrack = False
      TabOrder = 11
      Visible = False
      Width = 65
    end
    object cbDay: TcxComboBox
      Tag = 11534460
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.DropDownRows = 10
      Properties.OnChange = SetTheRadioButtonChecked
      Style.HotTrack = False
      TabOrder = 12
      Visible = False
      Width = 95
    end
    object meNumMonth1: TcxMaskEdit
      Tag = 11534460
      Left = 10000
      Top = 10000
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = SetTheRadioButtonChecked
      Style.HotTrack = False
      TabOrder = 13
      Visible = False
      OnExit = ValidateNumber
      Width = 32
    end
    object rbEvery: TcxRadioButton
      Left = 134
      Top = 101
      Width = 91
      Height = 17
      Caption = 'Every'
      Checked = True
      TabOrder = 14
      TabStop = True
      OnClick = DoChange
      GroupIndex = 4
      Transparent = True
    end
    object meDay: TcxMaskEdit
      Left = 231
      Top = 99
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = meDayPropertiesChange
      Style.HotTrack = False
      TabOrder = 15
      OnExit = ValidateNumber
      Width = 40
    end
    object rbEveryWeekday: TcxRadioButton
      Left = 134
      Top = 126
      Width = 160
      Height = 17
      Caption = 'Every weekday'
      TabOrder = 16
      OnClick = rbEveryWeekdayClick
      GroupIndex = 4
      Transparent = True
    end
    object meNumOfWeek: TcxMaskEdit
      Left = 10000
      Top = 10000
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = DoChange
      Style.HotTrack = False
      TabOrder = 17
      Visible = False
      OnExit = ValidateNumber
      Width = 40
    end
    object rbEvery1: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 60
      Height = 17
      Caption = 'Every'
      Checked = True
      TabOrder = 25
      TabStop = True
      Visible = False
      OnClick = DoChange
      GroupIndex = 5
      Transparent = True
    end
    object cbMonths: TcxComboBox
      Tag = 11588656
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbMonthsPropertiesChange
      Style.HotTrack = False
      TabOrder = 26
      Visible = False
      Width = 95
    end
    object meDayOfMonth: TcxMaskEdit
      Tag = 11588656
      Left = 10000
      Top = 10000
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{0,4}'
      Properties.MaxLength = 0
      Properties.OnChange = cbMonthsPropertiesChange
      Style.HotTrack = False
      TabOrder = 27
      Visible = False
      OnExit = ValidateNumber
      Width = 32
    end
    object rbThe1: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 60
      Height = 17
      Caption = 'The'
      TabOrder = 28
      Visible = False
      GroupIndex = 5
      Transparent = True
    end
    object cbWeek1: TcxComboBox
      Tag = 11589716
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbWeek1PropertiesChange
      Style.HotTrack = False
      TabOrder = 29
      Visible = False
      Width = 65
    end
    object cbDay1: TcxComboBox
      Tag = 11589716
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.DropDownRows = 10
      Properties.OnChange = cbWeek1PropertiesChange
      Style.HotTrack = False
      TabOrder = 30
      Visible = False
      Width = 95
    end
    object cbMonths1: TcxComboBox
      Tag = 11589716
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbWeek1PropertiesChange
      Style.HotTrack = False
      TabOrder = 31
      Visible = False
      Width = 95
    end
    object cbDayOfWeek1: TcxCheckBox
      Tag = 1
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Sunday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek2: TcxCheckBox
      Tag = 2
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Monday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek3: TcxCheckBox
      Tag = 3
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Tuesday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek4: TcxCheckBox
      Tag = 4
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Wednesday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek5: TcxCheckBox
      Tag = 5
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Thursday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek6: TcxCheckBox
      Tag = 6
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Friday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object cbDayOfWeek7: TcxCheckBox
      Tag = 7
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Saturday'
      Properties.OnChange = DoChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      Height = 17
      Width = 90
    end
    object teStart: TcxTimeEdit
      Left = 55
      Top = 28
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = DoChange
      Properties.OnEditValueChanged = StartTimeChanged
      Style.HotTrack = False
      TabOrder = 0
      Width = 78
    end
    object teEnd: TcxTimeEdit
      Left = 166
      Top = 28
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = DoChange
      Properties.OnEditValueChanged = EndTimeChanged
      Style.HotTrack = False
      TabOrder = 1
      Width = 78
    end
    object cbDuration: TcxComboBox
      Left = 300
      Top = 28
      Properties.ImmediateDropDownWhenKeyPressed = False
      Properties.ImmediatePost = True
      Properties.IncrementalSearch = False
      Properties.OnChange = DoChange
      Properties.OnPopup = cbDurationPropertiesPopup
      Properties.OnValidate = cbDurationPropertiesValidate
      Style.HotTrack = False
      TabOrder = 2
      Width = 121
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnRemove
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbStart1: TdxLayoutItem
      Parent = gbRange
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Start:'
      Control = deStart
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 135
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbNoEndDate
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbEndAfter
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbOccurrences: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'occurrences'
      CaptionOptions.Layout = clRight
      Control = meEndAfter
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbEndBy
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      Control = deEndBy
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 109
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 3
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = pnlPeriodicity
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbYearly
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = pnlPeriodicity
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbDaily
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = pnlPeriodicity
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbWeekly
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = pnlPeriodicity
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbMonthly
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object gbRange: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Range of recurrence'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = gbRange
      AlignHorz = ahLeft
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object gbPattern: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Recurrence pattern'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object pnlPeriodicity: TdxLayoutGroup
      Parent = gbPattern
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Bottom = 2
      Offsets.Top = 2
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = gbPattern
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object pnlViewsHost: TdxLayoutGroup
      Parent = gbPattern
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 400
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.HideTabs = True
      Index = 2
    end
    object pnlMonthly: TdxLayoutGroup
      Parent = pnlViewsHost
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = pnlMonthlyDay
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clRight
      Control = rbDay
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbOfEvery: TdxLayoutItem
      Parent = pnlMonthlyDay
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'of every'
      CaptionOptions.Layout = clRight
      Control = meNumOfDay
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbMonths: TdxLayoutItem
      Parent = pnlMonthlyDay
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'month(s)'
      CaptionOptions.Layout = clRight
      Control = meNumMonth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object pnlMonthlyDay: TdxLayoutGroup
      Parent = pnlMonthly
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object pnlMonthlyThe: TdxLayoutGroup
      Parent = pnlMonthly
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = pnlMonthlyThe
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbThe
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = pnlMonthlyThe
      AlignVert = avCenter
      Control = cbWeek
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 65
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbOfEvery1: TdxLayoutItem
      Parent = pnlMonthlyThe
      AlignVert = avCenter
      CaptionOptions.Text = 'of every'
      CaptionOptions.Layout = clRight
      Control = cbDay
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lbMonths1: TdxLayoutItem
      Parent = pnlMonthlyThe
      AlignVert = avCenter
      CaptionOptions.Text = 'month(s)'
      CaptionOptions.Layout = clRight
      Control = meNumMonth1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object pnlDaily: TdxLayoutGroup
      Parent = pnlViewsHost
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbEvery
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 91
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbDay: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'day'
      CaptionOptions.Layout = clRight
      Control = meDay
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup5
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = rbEveryWeekday
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = pnlDaily
      Index = 0
      AutoCreated = True
    end
    object pnlWeekly: TdxLayoutGroup
      Parent = pnlViewsHost
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = pnlWeekly
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object lbRecurEvery: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Rec&ur every'
      Control = meNumOfWeek
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbWeeksOn: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignVert = avClient
      CaptionOptions.Text = 'week(s) on:'
      Index = 1
    end
    object pnlYearly: TdxLayoutGroup
      Parent = pnlViewsHost
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 3
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbEvery1
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avCenter
      Control = cbMonths
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignVert = avCenter
      Control = meDayOfMonth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = rbThe1
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = pnlYearly
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignVert = avCenter
      Control = cbWeek1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 65
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignVert = avCenter
      Control = cbDay1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lbOf: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignVert = avCenter
      CaptionOptions.Text = 'of'
      Control = cbMonths1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = cbDayOfWeek1
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = cbDayOfWeek2
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbDayOfWeek3
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbDayOfWeek4
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = pnlWeekly
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = pnlWeekly
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = cbDayOfWeek5
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbDayOfWeek6
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbDayOfWeek7
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object gbTime: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Event Time'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object lbStart: TdxLayoutItem
      Parent = gbTime
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Start:'
      Control = teStart
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbEnd: TdxLayoutItem
      Parent = gbTime
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'End:'
      Control = teEnd
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbDuration: TdxLayoutItem
      Parent = gbTime
      AlignVert = avClient
      CaptionOptions.Text = 'Duration:'
      Control = cbDuration
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 504
    Top = 8
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
    end
  end
end
