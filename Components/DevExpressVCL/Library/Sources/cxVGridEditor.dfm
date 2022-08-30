inherited cxVerticalGridEditor: TcxVerticalGridEditor
  Left = 591
  Top = 125
  Caption = 'VerticalGrid - rows editor'
  ClientHeight = 322
  ClientWidth = 384
  Constraints.MinHeight = 360
  Constraints.MinWidth = 400
  PopupMenu = PopupMenu
  OnActivate = FormActivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 384
    Height = 322
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btEditor: TcxButton
      Left = 286
      Top = 10
      Width = 88
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Add editor'
      TabOrder = 1
      OnClick = btEditorClick
    end
    object btCategory: TcxButton
      Left = 286
      Top = 41
      Width = 88
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Add category'
      TabOrder = 2
      OnClick = btCategoryClick
    end
    object btMultiEditor: TcxButton
      Left = 286
      Top = 72
      Width = 88
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Add multieditor'
      TabOrder = 3
      OnClick = btMultiEditorClick
    end
    object btDelete: TcxButton
      Left = 286
      Top = 103
      Width = 88
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Delete'
      TabOrder = 4
      OnClick = btDeleteClick
    end
    object btCreateAll: TcxButton
      Left = 286
      Top = 134
      Width = 88
      Height = 24
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Create all items'
      TabOrder = 5
      OnClick = btCreateAllClick
    end
    object btClear: TcxButton
      Left = 286
      Top = 164
      Width = 88
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Clear all'
      TabOrder = 6
      OnClick = btClearClick
    end
    object btLayoutEditor: TcxButton
      Left = 286
      Top = 195
      Width = 88
      Height = 24
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Layout editor...'
      TabOrder = 7
      OnClick = btLayoutEditorClick
    end
    object btClose: TcxButton
      Left = 286
      Top = 288
      Width = 88
      Height = 24
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Close'
      TabOrder = 8
      OnClick = btCloseClick
    end
    object lbRows: TcxListBox
      Left = 10
      Top = 10
      Width = 270
      Height = 302
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      ItemHeight = 13
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbRowsClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btEditor'
      CaptionOptions.Visible = False
      Control = btEditor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btCategory'
      CaptionOptions.Visible = False
      Control = btCategory
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btMultiEditor'
      CaptionOptions.Visible = False
      Control = btMultiEditor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btDelete'
      CaptionOptions.Visible = False
      Control = btDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liCreateAll: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btCreateAll'
      CaptionOptions.Visible = False
      Control = btCreateAll
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btClear'
      CaptionOptions.Visible = False
      Control = btClear
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btLayoutEditor'
      CaptionOptions.Visible = False
      Control = btLayoutEditor
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'btClose'
      CaptionOptions.Visible = False
      Control = btClose
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbRows
      ControlOptions.OriginalHeight = 342
      ControlOptions.OriginalWidth = 148
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object PopupMenu: TPopupMenu
    Left = 128
    Top = 16
    object miEditor: TMenuItem
      Caption = 'Add &editor'
      ShortCut = 45
      OnClick = miEditorClick
    end
    object miCategory: TMenuItem
      Caption = 'Add &category'
      OnClick = miCategoryClick
    end
    object miMultieditor: TMenuItem
      Caption = 'Add &multieditor'
      OnClick = miMultieditorClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Caption = '&Delete row'
      Enabled = False
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object miClearAll: TMenuItem
      Caption = 'C&lear all'
      Enabled = False
      OnClick = miClearAllClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Caption = 'Select &All'
      Enabled = False
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
    end
  end
end
