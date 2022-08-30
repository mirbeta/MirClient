object cxSchedulerReminderForm: TcxSchedulerReminderForm
  Left = 288
  Top = 89
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 329
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 440
    Height = 329
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object pbImage: TPaintBox
      Left = 10
      Top = 10
      Width = 16
      Height = 16
      OnPaint = DrawIcon
    end
    object lvItems: TcxListView
      Left = 10
      Top = 48
      Width = 420
      Height = 200
      Anchors = [akLeft, akTop, akRight]
      ColumnClick = False
      Columns = <
        item
          Width = 290
        end
        item
          Width = 120
        end>
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      Style.HotTrack = False
      TabOrder = 1
      ViewStyle = vsReport
      OnDblClick = lvItemsDblClick
      OnKeyDown = lvItemsKeyDown
      OnSelectItem = lvItemsSelectItem
    end
    object btnDismissAll: TcxButton
      Left = 10
      Top = 254
      Width = 90
      Height = 23
      Caption = 'Dismiss &All'
      TabOrder = 2
      OnClick = ButtonClick
    end
    object btnOpenItem: TcxButton
      Tag = 1
      Left = 244
      Top = 254
      Width = 90
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Open Item'
      TabOrder = 3
      OnClick = ButtonClick
    end
    object btnDismiss: TcxButton
      Tag = 2
      Left = 340
      Top = 254
      Width = 90
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Dismiss'
      TabOrder = 4
      OnClick = ButtonClick
    end
    object cbSnoozeTime: TcxComboBox
      Left = 10
      Top = 283
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 5
      Width = 324
    end
    object btnSnooze: TcxButton
      Tag = 3
      Left = 340
      Top = 283
      Width = 90
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Snooze'
      Default = True
      TabOrder = 6
      OnClick = ButtonClick
    end
    object lbEventCaption: TcxLabel
      Left = 32
      Top = 10
      Caption = 'lbEventCaption'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.HotTrack = False
      Style.TransparentBorder = False
      Style.IsFontAssigned = True
      Properties.Alignment.Vert = taVCenter
      Transparent = True
      AnchorY = 20
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object liImage: TdxLayoutItem
      Parent = dxLayoutGroup1
      Control = pbImage
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 16
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object dxLayoutLabeledItem2: TdxLayoutLabeledItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      Control = lvItems
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 437
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'btnDismissAll'
      CaptionOptions.Visible = False
      Control = btnDismissAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liOpenItem: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnOpenItem'
      CaptionOptions.Visible = False
      Control = btnOpenItem
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDismiss'
      CaptionOptions.Visible = False
      Control = btnDismiss
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      Control = cbSnoozeTime
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 338
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnSnooze'
      CaptionOptions.Visible = False
      Control = btnSnooze
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbEventCaption
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbEventStartTime: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
  end
  object tmUpdate: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = tmUpdateTimer
    Left = 48
    Top = 64
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 64
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
    end
  end
end
