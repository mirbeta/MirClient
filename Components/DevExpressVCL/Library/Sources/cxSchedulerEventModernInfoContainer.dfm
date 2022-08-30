object cxSchedulerEventModernInfoContainer: TcxSchedulerEventModernInfoContainer
  Left = -500
  Top = -500
  AutoSize = True
  BorderStyle = bsNone
  ClientHeight = 209
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 257
    Height = 209
    Align = alTop
    ParentBackground = True
    TabOrder = 0
    Transparent = True
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object pbEvent: TPaintBox
      Left = 10
      Top = 10
      Width = 237
      Height = 47
      OnPaint = pbEventPaint
    end
    object pbTaskComplete: TcxProgressBar
      Left = 73
      Top = 165
      TabOrder = 0
      Width = 174
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object liEventCaption: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'PaintBox1'
      CaptionOptions.Visible = False
      Control = pbEvent
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 47
      ControlOptions.OriginalWidth = 237
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liStartCaption: TdxLayoutLabeledItem
      Parent = lgStartAndEndCaptions
      CaptionOptions.Text = 'Start:'
      LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
      Index = 0
    end
    object liEndCaption: TdxLayoutLabeledItem
      Parent = lgStartAndEndCaptions
      CaptionOptions.Text = 'End:'
      LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
      Index = 1
    end
    object liStart: TdxLayoutLabeledItem
      Parent = lgStartAndEndFields
      AlignHorz = ahLeft
      CaptionOptions.Text = 'liStart'
      Index = 0
    end
    object liEnd: TdxLayoutLabeledItem
      Parent = lgStartAndEndFields
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'liEnd'
      Index = 1
    end
    object lsiSpace1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      AlignVert = avTop
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 5
      SizeOptions.Width = 10
      Index = 1
    end
    object liLocationCaption: TdxLayoutLabeledItem
      Parent = lgLocation
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Location:'
      LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
      Index = 0
    end
    object liReminderCaption: TdxLayoutLabeledItem
      Parent = lgReminder
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Reminder:'
      LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
      Index = 0
    end
    object liLocation: TdxLayoutLabeledItem
      Parent = lgLocation
      AlignHorz = ahClient
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 'liLocation'
      CaptionOptions.WordWrap = True
      Index = 2
    end
    object liReminder: TdxLayoutLabeledItem
      Parent = lgReminder
      AlignHorz = ahClient
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = 'liReminder'
      CaptionOptions.WordWrap = True
      Index = 1
    end
    object lgStartAndEnd: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lgStartAndEndCaptions: TdxLayoutGroup
      Parent = lgStartAndEnd
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lgStartAndEndFields: TdxLayoutGroup
      Parent = lgStartAndEnd
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lgLocationAndReminder: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 4
    end
    object liTaskComplete: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avTop
      CaptionOptions.Text = 'Complete:'
      LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
      Control = pbTaskComplete
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lsiSpace2: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 5
      SizeOptions.Width = 10
      Index = 3
    end
    object lgLocation: TdxLayoutGroup
      Parent = lgLocationAndReminder
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgReminder: TdxLayoutGroup
      Parent = lgLocationAndReminder
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object esiLocationSpace: TdxLayoutEmptySpaceItem
      Parent = lgLocation
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = False
      SizeOptions.Height = 10
      SizeOptions.Width = 1
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 192
    Top = 120
    object dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clWindowText
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Tahoma'
      ItemOptions.CaptionOptions.Font.Style = [fsBold]
      ItemOptions.CaptionOptions.UseDefaultFont = False
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
