inherited dxBarCustomizationForm: TdxBarCustomizationForm
  ClientHeight = 381
  ClientWidth = 410
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl [0]
    Left = 0
    Top = 0
    Width = 385
    Height = 381
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = lcMainLookAndFeel
    object BClose: TcxButton
      Left = 283
      Top = 387
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = BCloseClick
    end
    object BHelp: TcxButton
      Left = 10
      Top = 387
      Width = 25
      Height = 25
      TabOrder = 0
      OnClick = BHelpClick
    end
    object BBarRename: TcxButton
      Left = 269
      Top = 93
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 3
    end
    object lbBarsList: TcxListBox
      Left = 24
      Top = 62
      Width = 239
      Height = 305
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      ListStyle = lbOwnerDrawFixed
      Style.TransparentBorder = False
      TabOrder = 6
    end
    object tvKeyTips: TTreeView
      Left = 10000
      Top = 10000
      Width = 347
      Height = 321
      BevelInner = bvNone
      BevelOuter = bvSpace
      BevelKind = bkFlat
      BorderStyle = bsNone
      Indent = 19
      TabOrder = 12
      Visible = False
    end
    object BBarReset: TcxButton
      Left = 269
      Top = 155
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 5
    end
    object BBarNew: TcxButton
      Left = 269
      Top = 62
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
    object BBarDelete: TcxButton
      Left = 269
      Top = 124
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 4
    end
    object Label3: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Personalized Menus and Toolbars  '
      Style.HotTrack = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 362
      AnchorY = 10008
    end
    object CBMenusShowRecentItemsFirst: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Me&nus show recently used commands first'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      OnClick = CBMenusShowRecentItemsFirstClick
    end
    object CBShowFullMenusAfterDelay: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Show f&ull menus after a short delay'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = CBShowFullMenusAfterDelayClick
    end
    object BResetUsageData: TcxButton
      Left = 10000
      Top = 10000
      Width = 133
      Height = 25
      Caption = '&Reset my usage data'
      TabOrder = 20
      Visible = False
      OnClick = BResetUsageDataClick
    end
    object Label2: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Other  '
      Style.HotTrack = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 362
      AnchorY = 10008
    end
    object CBLargeIconsEx: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = '&Large icons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      OnClick = CBLargeIconsClick
    end
    object CBHint1Ex: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Show Tool&Tips on toolbars'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = CBHint1ExClick
    end
    object CBHint2Ex: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Show s&hortcut keys in ToolTips'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = CBHint2Click
    end
    object ComboBoxMenuAnimationsEx: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 25
      Visible = False
      OnClick = ComboBoxMenuAnimationsClick
      Width = 121
    end
    object CBLargeIcons: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = '&Large icons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Visible = False
      OnClick = CBLargeIconsClick
    end
    object CBHint1: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Show Tool&Tips on toolbars'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
      Visible = False
      OnClick = CBHint1Click
    end
    object CBHint2: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Show s&hortcut keys in ToolTips'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = CBHint2Click
    end
    object ComboBoxMenuAnimations: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsEditFixedList
      Style.HotTrack = False
      TabOrder = 16
      Visible = False
      OnClick = ComboBoxMenuAnimationsClick
      Width = 121
    end
    object LabelDescription: TcxLabel
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Description  '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 320
      AnchorY = 10008
    end
    object CategoriesPopupButtonPlace: TcxButton
      Left = 10000
      Top = 10000
      Width = 96
      Height = 18
      DropDownMenu = CategoriesPopupMenu
      Kind = cxbkDropDown
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 8
      Visible = False
    end
    object lbCategories: TcxListBox
      Left = 10000
      Top = 10000
      Width = 157
      Height = 271
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 9
      Visible = False
    end
    object CommandsPopupButtonPlace: TcxButton
      Left = 10000
      Top = 10000
      Width = 96
      Height = 18
      Anchors = [akTop, akRight]
      DropDownMenu = CommandsPopupMenu
      Kind = cxbkDropDown
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 10
      Visible = False
    end
    object lbItems: TcxListBox
      Left = 10000
      Top = 10000
      Width = 157
      Height = 271
      Touch.InteractiveGestureOptions = [igoPanSingleFingerVertical, igoPanInertia, igoPanGutter]
      ItemHeight = 13
      ListStyle = lbOwnerDrawVariable
      Style.TransparentBorder = False
      TabOrder = 11
      Visible = False
      OnDrawItem = lbItemsDrawItem
    end
    object LAllCommands: TcxListBox
      Left = 10000
      Top = 10000
      Width = 362
      Height = 301
      ItemHeight = 16
      ListStyle = lbOwnerDrawFixed
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 26
      Visible = False
    end
    object CBShowCommandsWithShortCut: TcxCheckBox
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akBottom]
      Caption = 'Show commands that may have a shortcut'
      State = cbsChecked
      Style.TransparentBorder = False
      TabOrder = 27
      Visible = False
      OnClick = CBShowCommandsWithShortCutClick
    end
    object lbGroups: TcxListBox
      Left = 10000
      Top = 10000
      Width = 136
      Height = 262
      ItemHeight = 16
      ListStyle = lbOwnerDrawVariable
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 32
      Visible = False
    end
    object btnAddGroup: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actAddGroup
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 28
      Visible = False
    end
    object btnDeleteGroup: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actDeleteGroup
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 29
      Visible = False
    end
    object btnMoveGroupUp: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actMoveGroupUp
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 30
      Visible = False
    end
    object btnMoveGroupDown: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actMoveGroupDown
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 31
      Visible = False
    end
    object lbGroupsItems: TcxListBox
      Left = 10000
      Top = 10000
      Width = 136
      Height = 262
      ItemHeight = 16
      ListStyle = lbOwnerDrawVariable
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 37
      Visible = False
    end
    object btnAddGroupItem: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actAddGroupItem
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 33
      Visible = False
    end
    object btnDeleteGroupItem: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actDeleteGroupItem
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 34
      Visible = False
    end
    object btnMoveGroupItemDown: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actMoveGroupItemDown
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 36
      Visible = False
    end
    object btnMoveGroupItemUp: TcxButton
      Left = 10000
      Top = 10000
      Width = 25
      Height = 25
      Action = actMoveGroupItemUp
      OptionsImage.Images = imgGroups
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 35
      Visible = False
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = BClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liBHelp: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = BHelp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Visible = False
      Control = BBarRename
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object LabelToolbars: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Toolb&ars:'
      CaptionOptions.Layout = clTop
      Control = lbBarsList
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object tsToolbars: TdxLayoutGroup
      Parent = PageControl
      AlignVert = avClient
      CaptionOptions.Text = 'Toolbars'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object PageControl: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = tsToolbars
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = PageControl
      AlignVert = avClient
      Visible = False
      Control = tvKeyTips
      ControlOptions.OriginalHeight = 204
      ControlOptions.OriginalWidth = 121
      Index = 2
    end
    object liBBarReset: TdxLayoutItem
      Parent = dxLayoutGroup2
      Control = BBarReset
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = ' '
      CaptionOptions.Layout = clTop
      Control = BBarNew
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Visible = False
      Control = BBarDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object EnhancedOptionsPanel: TdxLayoutGroup
      Parent = tsOptions
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Control = Label3
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Control = CBMenusShowRecentItemsFirst
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 226
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = CBShowFullMenusAfterDelay
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 190
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = BResetUsageData
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 133
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Control = Label2
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 4
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Control = CBLargeIconsEx
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Control = CBHint1Ex
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = CBHint2Ex
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object Label1: TdxLayoutItem
      Parent = EnhancedOptionsPanel
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Menu animations:'
      Control = ComboBoxMenuAnimationsEx
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object tsOptions: TdxLayoutGroup
      Parent = PageControl
      AlignVert = avClient
      CaptionOptions.Text = 'Options'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      TabbedOptions.HideTabs = True
      Index = 3
    end
    object StandardOptionsPanel: TdxLayoutGroup
      Parent = tsOptions
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = StandardOptionsPanel
      CaptionOptions.Visible = False
      Control = CBLargeIcons
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = StandardOptionsPanel
      CaptionOptions.Visible = False
      Control = CBHint1
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 145
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = StandardOptionsPanel
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = CBHint2
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 167
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object LabelMenuAnimations: TdxLayoutItem
      Parent = StandardOptionsPanel
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Menu animations:'
      Control = ComboBoxMenuAnimations
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object tsItems: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Commands'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tsItems
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = LabelDescription
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object DescriptionLabel: TdxLayoutLabeledItem
      Parent = tsItems
      AlignVert = avBottom
      CaptionOptions.WordWrap = True
      Index = 1
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = tsItems
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup7
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object LabelCategories: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Cate&gories:'
      Control = CategoriesPopupButtonPlace
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignVert = avClient
      Control = lbCategories
      ControlOptions.OriginalHeight = 92
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = dxLayoutGroup7
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object LabelCommands: TdxLayoutItem
      Parent = dxLayoutGroup6
      CaptionOptions.Text = 'Comman&ds:'
      Control = CommandsPopupButtonPlace
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignVert = avClient
      Control = lbItems
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object tsCommands: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'All commands'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 4
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tsCommands
      AlignVert = avClient
      Control = LAllCommands
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = tsCommands
      CaptionOptions.Visible = False
      Control = CBShowCommandsWithShortCut
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 97
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object tsGroups: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Groups'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 5
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = tsGroups
      AlignHorz = ahClient
      CaptionOptions.Text = 'Groups'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = tsGroups
      AlignHorz = ahClient
      CaptionOptions.Text = 'Items'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbGroups
      ControlOptions.OriginalHeight = 100
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnAddGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnDeleteGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup3
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'btnMoveGroupUp'
      CaptionOptions.Visible = False
      Control = btnMoveGroupUp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnMoveGroupDown
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbGroupsItems
      ControlOptions.OriginalHeight = 100
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnAddGroupItem
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnDeleteGroupItem
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnMoveGroupItemDown
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnMoveGroupItemUp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup4
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
  end
  inherited alCustomize: TActionList
    inherited actAddGroup: TAction
      ImageIndex = 0
    end
  end
  inherited BarManager1: TdxBarManager
    ImageOptions.Images = imgGroups
    Style = bmsUseLookAndFeel
    PixelsPerInch = 96
    inherited CategoriesAdd: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 0
    end
    inherited CategoriesDelete: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 1
    end
    inherited CommandsAdd: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 0
    end
    inherited CommandsDelete: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 1
    end
    inherited CommandsMoveUp: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 2
    end
    inherited CommandsMoveDown: TdxBarButton
      Glyph.Data = {}
      ImageIndex = 3
    end
  end
  inherited CategoriesPopupMenu: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  inherited CommandsPopupMenu: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  inherited imgGroups: TcxImageList
    FormatVersion = 1
  end
  object lcMainLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 272
    Top = 280
    object lcMainLookAndFeel: TdxLayoutCxLookAndFeel
    end
  end
end
