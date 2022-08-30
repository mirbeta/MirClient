object dxLayoutControlCustomizeForm: TdxLayoutControlCustomizeForm
  Left = 93
  Top = 249
  ActiveControl = btnClose
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  ClientHeight = 463
  ClientWidth = 461
  Color = clBtnFace
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShortCut = FormShortCut
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 461
    Height = 463
    Align = alClient
    TabOrder = 0
    HighlightRoot = False
    object tvVisibleItems: TcxTreeView
      Left = 22
      Top = 84
      Width = 183
      Height = 326
      PopupMenu = pmTreeViewActions
      Style.TransparentBorder = False
      TabOrder = 12
      OnKeyDown = FormKeyDown
      OnMouseDown = tvVisibleItemsMouseDown
      OnMouseMove = tvVisibleItemsMouseMove
      OnMouseUp = tvVisibleItemsMouseUp
      OnContextPopup = tvVisibleItemsContextPopup
      HideSelection = False
      Images = ilItems
      MultiSelect = True
      MultiSelectStyle = [msControlSelect, msShiftSelect, msVisibleOnly]
      RightClickSelect = True
      OnChange = tvVisibleItemsChange
      OnChanging = tvVisibleItemsChanging
      OnCustomDrawItem = tvVisibleItemsCustomDrawItem
      OnDeletion = tvVisibleItemsDeletion
      OnEditing = tvVisibleItemsEditing
      OnEdited = tvVisibleItemsEdited
    end
    object tvAvailableItems: TcxTreeView
      Left = 243
      Top = 84
      Width = 196
      Height = 326
      PopupMenu = pmAvailableItemsActions
      Style.TransparentBorder = False
      TabOrder = 21
      OnKeyDown = FormKeyDown
      OnMouseDown = tvVisibleItemsMouseDown
      OnMouseMove = tvVisibleItemsMouseMove
      OnMouseUp = tvVisibleItemsMouseUp
      OnContextPopup = tvAvailableItemsContextPopup
      HideSelection = False
      Images = ilItems
      MultiSelect = True
      MultiSelectStyle = [msControlSelect, msShiftSelect, msVisibleOnly]
      RightClickSelect = True
      OnChange = tvVisibleItemsChange
      OnChanging = tvVisibleItemsChanging
      OnCustomDrawItem = tvVisibleItemsCustomDrawItem
      OnDeletion = tvVisibleItemsDeletion
      OnEditing = tvVisibleItemsEditing
      OnEdited = tvVisibleItemsEdited
    end
    object btnClose: TcxButton
      Left = 376
      Top = 428
      Width = 75
      Height = 25
      Action = acClose
      TabOrder = 23
    end
    object cbTabbedView: TcxCheckBox
      Left = 10
      Top = 432
      Action = acTabbedView
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
    end
    object btnShowDesignSelectors: TcxButton
      Left = 131
      Top = 10
      Width = 23
      Height = 22
      Action = acShowDesignSelectors
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 5
    end
    object btnHighlightRoot: TcxButton
      Left = 108
      Top = 10
      Width = 23
      Height = 22
      Action = acHighlightRoot
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 4
    end
    object btnRestore: TcxButton
      Left = 85
      Top = 10
      Width = 23
      Height = 22
      Action = acRestore
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 3
    end
    object btnStore: TcxButton
      Left = 62
      Top = 10
      Width = 23
      Height = 22
      Action = acStore
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 2
    end
    object btnRedo: TcxButton
      Left = 33
      Top = 10
      Width = 23
      Height = 22
      Action = acRedo
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 1
    end
    object btnUndo: TcxButton
      Left = 10
      Top = 10
      Width = 23
      Height = 22
      Action = acUndo
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 0
    end
    object btnAlignBy: TcxButton
      Left = 126
      Top = 56
      Width = 23
      Height = 22
      Action = acAlignBy
      DropDownMenu = pmAlign
      Kind = cxbkDropDown
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 11
    end
    object btnTreeViewItemsDelete: TcxButton
      Left = 74
      Top = 56
      Width = 23
      Height = 22
      Action = acTreeViewItemsDelete
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 9
    end
    object btnTreeViewCollapseAll: TcxButton
      Left = 45
      Top = 56
      Width = 23
      Height = 22
      Action = acTreeViewCollapseAll
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 8
    end
    object btnTreeViewExpandAll: TcxButton
      Left = 22
      Top = 56
      Width = 23
      Height = 22
      Action = acTreeViewExpandAll
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 7
    end
    object btnAvailableItemsViewAsList: TcxButton
      Left = 416
      Top = 56
      Width = 23
      Height = 22
      Action = acAvailableItemsViewAsList
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 20
    end
    object btnAvailableItemsDelete: TcxButton
      Left = 364
      Top = 56
      Width = 23
      Height = 22
      Action = acAvailableItemsDelete
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 18
    end
    object btnAddItem: TcxButton
      Left = 341
      Top = 56
      Width = 23
      Height = 22
      Action = acAddCustomItem
      DropDownMenu = pmAddCustomItem
      Kind = cxbkDropDown
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 17
    end
    object btnAddGroup: TcxButton
      Left = 295
      Top = 56
      Width = 23
      Height = 22
      Action = acAddGroup
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 15
    end
    object btnAvailableItemsCollapseAll: TcxButton
      Left = 266
      Top = 56
      Width = 23
      Height = 22
      Action = acAvailableItemsCollapseAll
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 14
    end
    object btnAvailableItemsExpandAll: TcxButton
      Left = 243
      Top = 56
      Width = 23
      Height = 22
      Action = acAvailableItemsExpandAll
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 13
    end
    object cxButton1: TcxButton
      Left = 318
      Top = 56
      Width = 23
      Height = 22
      Action = acAddItem
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 16
    end
    object cxButton2: TcxButton
      Left = 97
      Top = 56
      Width = 23
      Height = 22
      Action = acVisibleItemsMakeFloat
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 10
    end
    object cxButton3: TcxButton
      Left = 387
      Top = 56
      Width = 23
      Height = 22
      Action = acAvailableItemsMakeFloat
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 19
    end
    object cxButton4: TcxButton
      Left = 154
      Top = 10
      Width = 23
      Height = 22
      Action = acShowItemNames
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 6
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      UseIndent = False
      Index = 0
    end
    object liUndo: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnUndo
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liRedo: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnRedo
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lsSeparator4: TdxLayoutSeparatorItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 2
    end
    object liStore: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStore
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liRestore: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnRestore
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liHighlightRoot: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnHighlightRoot
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liShowDesignSelectors: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnShowDesignSelectors
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcgTreeView: TdxLayoutGroup
      Parent = lcMainGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = '&Layout Tree View'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lgTreeView: TdxLayoutGroup
      Parent = lcgTreeView
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      UseIndent = False
      Index = 0
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lgTreeView
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnTreeViewExpandAll
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem9: TdxLayoutItem
      Parent = lgTreeView
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnTreeViewCollapseAll
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lgTreeView
      CaptionOptions.Text = 'Separator Item'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 2
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lgTreeView
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnTreeViewItemsDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lsAlignBy: TdxLayoutSeparatorItem
      Parent = lgTreeView
      CaptionOptions.Text = 'Separator Item'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 5
    end
    object liAlignBy: TdxLayoutItem
      Parent = lgTreeView
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAlignBy
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcgTreeView
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxTreeView1'
      CaptionOptions.Visible = False
      Control = tvVisibleItems
      ControlOptions.OriginalHeight = 326
      ControlOptions.OriginalWidth = 194
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgAvailableItems: TdxLayoutGroup
      Parent = lcMainGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = '&Available Items'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object lgAvailableItems: TdxLayoutGroup
      Parent = lcgAvailableItems
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      UseIndent = False
      Index = 0
    end
    object lcMainItem15: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAvailableItemsExpandAll
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem14: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAvailableItemsCollapseAll
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lgAvailableItems
      CaptionOptions.Text = 'Separator Item'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 2
    end
    object lcMainItem13: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAddGroup
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liAddItem: TdxLayoutItem
      Parent = lgAvailableItems
      CaptionOptions.Visible = False
      Control = cxButton1
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liAddCustomItem: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAddItem
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcMainItem11: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAvailableItemsDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lcMainSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lgAvailableItems
      CaptionOptions.Text = 'Separator Item'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 8
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnAvailableItemsViewAsList
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcgAvailableItems
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxTreeView2'
      CaptionOptions.Visible = False
      Control = tvAvailableItems
      ControlOptions.OriginalHeight = 326
      ControlOptions.OriginalWidth = 193
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbTabbedView
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object siMainSplitter: TdxLayoutSplitterItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 1
    end
    object liVisibleItemsMakeFloat: TdxLayoutItem
      Parent = lgTreeView
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liAvailableItemsMakeFloat: TdxLayoutItem
      Parent = lgAvailableItems
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = cxButton3
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object liShowItemNames: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Visible = False
      Control = cxButton4
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 7
    end
  end
  object pmAlign: TPopupMenu
    Left = 48
    Top = 360
    object Left1: TMenuItem
      Action = acAlignLeftSide
    end
    object Right1: TMenuItem
      Tag = 2
      Action = acAlignRightSide
    end
    object miSeparator3: TMenuItem
      Caption = '-'
    end
    object op1: TMenuItem
      Tag = 1
      Action = acAlignTopSide
    end
    object Bottom1: TMenuItem
      Tag = 3
      Action = acAlignBottomSide
    end
    object miSeparator4: TMenuItem
      Caption = '-'
    end
    object None1: TMenuItem
      Tag = -1
      Action = acAlignNone
    end
  end
  object alMain: TActionList
    Images = ilActions
    Left = 16
    Top = 360
    object acAddGroup: TAction
      Category = 'Buttons'
      Caption = 'Add &Group'
      Hint = 'Add Group'
      ImageIndex = 6
      ShortCut = 32839
      OnExecute = acAddGroupExecute
    end
    object acAddCustomItem: TAction
      Category = 'Buttons'
      Caption = 'Add Item'
      Hint = 'Add Item'
      ImageIndex = 18
      OnExecute = acAddCustomItemExecute
    end
    object acAddItem: TAction
      Category = 'Buttons'
      Caption = 'Add &Item'
      Hint = 'Add Item'
      ImageIndex = 5
      ShortCut = 32841
      OnExecute = acAddItemExecute
    end
    object acAddEmptySpaceItem: TAction
      Category = 'Buttons'
      Caption = 'Add &Empty Space Item'
      Hint = 'Add Empty Space Item'
      ImageIndex = 14
      OnExecute = acAddEmptySpaceItemExecute
    end
    object acAddLabeledItem: TAction
      Category = 'Buttons'
      Caption = 'Add &Label'
      Hint = 'Add Label'
      ImageIndex = 15
      OnExecute = acAddLabeledItemExecute
    end
    object acAddImage: TAction
      Category = 'Buttons'
      Caption = 'Add &Image'
      ImageIndex = 22
      OnExecute = acAddImageExecute
    end
    object acAddSeparator: TAction
      Category = 'Buttons'
      Caption = 'Add &Separator'
      Hint = 'Add Separator'
      ImageIndex = 16
      OnExecute = acAddSeparatorExecute
    end
    object acAddSplitter: TAction
      Category = 'Buttons'
      Caption = 'Add S&plitter'
      Hint = 'Add Splitter'
      ImageIndex = 17
      OnExecute = acAddSplitterExecute
    end
    object acAvailableItemsDelete: TAction
      Category = 'Buttons'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 2
      OnExecute = acAvailableItemsDeleteExecute
    end
    object acTreeViewItemsDelete: TAction
      Category = 'Buttons'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 2
      OnExecute = acTreeViewItemsDeleteExecute
    end
    object acClose: TAction
      Category = 'Buttons'
      Caption = '&Close'
      Hint = 'Close'
      ShortCut = 27
      OnExecute = acCloseExecute
    end
    object acAvailableItemsExpandAll: TAction
      Category = 'Buttons'
      Caption = 'Expand All'
      Hint = 'Expand All'
      ImageIndex = 3
      OnExecute = acAvailableItemsExpandAllExecute
    end
    object acAvailableItemsCollapseAll: TAction
      Category = 'Buttons'
      Caption = 'Callapse All'
      Hint = 'Collapse All'
      ImageIndex = 1
      OnExecute = acAvailableItemsCollapseAllExecute
    end
    object acTreeViewExpandAll: TAction
      Category = 'Buttons'
      Caption = 'Expand All'
      Hint = 'Expand All'
      ImageIndex = 3
      OnExecute = acTreeViewExpandAllExecute
    end
    object acTreeViewCollapseAll: TAction
      Category = 'Buttons'
      Caption = 'Callapse All'
      Hint = 'Collapse All'
      ImageIndex = 1
      OnExecute = acTreeViewCollapseAllExecute
    end
    object acAlignLeftSide: TAction
      Category = 'ItemAligns'
      Caption = 'Left Side'
      OnExecute = AlignExecute
    end
    object acAlignRightSide: TAction
      Tag = 2
      Category = 'ItemAligns'
      Caption = 'Right Side'
      OnExecute = AlignExecute
    end
    object acAlignTopSide: TAction
      Tag = 1
      Category = 'ItemAligns'
      Caption = 'Top Side'
      OnExecute = AlignExecute
    end
    object acAlignBottomSide: TAction
      Tag = 3
      Category = 'ItemAligns'
      Caption = 'Bottom Side'
      OnExecute = AlignExecute
    end
    object acAlignNone: TAction
      Tag = -1
      Category = 'ItemAligns'
      Caption = 'None'
      OnExecute = AlignExecute
    end
    object acAvailableItemsViewAsList: TAction
      Category = 'Buttons'
      AutoCheck = True
      Caption = 'ViewAsList'
      Hint = 'ViewAsList'
      ImageIndex = 8
      OnExecute = acAvailableItemsViewAsListExecute
    end
    object acTabbedView: TAction
      Category = 'Buttons'
      AutoCheck = True
      Caption = 'Tabbed View'
      Hint = 'Tabbed View'
      ImageIndex = 9
      OnExecute = acTabbedViewExecute
    end
    object acShowItemNames: TAction
      Category = 'Buttons'
      AutoCheck = True
      Caption = 'acShowItemNames'
      Checked = True
      Hint = 'Display Item Names/Captions'
      ImageIndex = 24
      OnExecute = acShowItemNamesExecute
    end
    object acHighlightRoot: TAction
      Category = 'Buttons'
      AutoCheck = True
      Caption = 'acHighlightRoot'
      Hint = 'Highlight Root'
      ImageIndex = 4
      OnExecute = acHighlightRootExecute
    end
    object acShowDesignSelectors: TAction
      Category = 'Buttons'
      AutoCheck = True
      Caption = 'acHighlightControlSelectors'
      Hint = 'Show Design Selectors'
      ImageIndex = 7
      OnExecute = acShowDesignSelectorsExecute
    end
    object acStore: TAction
      Category = 'Buttons'
      Caption = 'Store'
      ImageIndex = 9
      OnExecute = acStoreExecute
    end
    object acRestore: TAction
      Category = 'Buttons'
      Caption = 'acRestore'
      ImageIndex = 10
      OnExecute = acRestoreExecute
    end
    object acTreeViewItemRename: TAction
      Category = 'Buttons'
      Caption = 'Rename'
      Hint = 'Rename'
      ImageIndex = 13
      ShortCut = 113
      OnExecute = acTreeViewItemRenameExecute
    end
    object acAvailableItemRename: TAction
      Category = 'Buttons'
      Caption = 'Rename'
      Hint = 'Rename'
      ImageIndex = 13
      OnExecute = acAvailableItemRenameExecute
    end
    object acUndo: TAction
      Category = 'Buttons'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 11
      OnExecute = acUndoExecute
    end
    object acRedo: TAction
      Category = 'Buttons'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 12
      OnExecute = acRedoExecute
    end
    object acAlignBy: TAction
      Category = 'ItemAligns'
      Caption = 'acAlignBy'
      Hint = 'Align By'
      ImageIndex = 0
      OnExecute = acAlignByExecute
    end
    object acHAlignLeft: TAction
      Category = 'HAlign'
      Caption = 'ahLeft'
      GroupIndex = 4
      OnExecute = acHAlignExecute
    end
    object acHAlignCenter: TAction
      Tag = 1
      Category = 'HAlign'
      Caption = 'ahCenter'
      GroupIndex = 4
      OnExecute = acHAlignExecute
    end
    object acHAlignRight: TAction
      Tag = 2
      Category = 'HAlign'
      Caption = 'ahRight'
      GroupIndex = 4
      OnExecute = acHAlignExecute
    end
    object acHAlignClient: TAction
      Tag = 3
      Category = 'HAlign'
      Caption = 'ahClient'
      GroupIndex = 4
      OnExecute = acHAlignExecute
    end
    object acHAlignParent: TAction
      Tag = 4
      Category = 'HAlign'
      Caption = 'ahParentManaged'
      GroupIndex = 4
      OnExecute = acHAlignExecute
    end
    object acVAlignTop: TAction
      Category = 'VAlign'
      Caption = 'acVAlignTop'
      GroupIndex = 5
      OnExecute = acVAlignExecute
    end
    object acVAlignCenter: TAction
      Tag = 1
      Category = 'VAlign'
      Caption = 'acVAlignCenter'
      GroupIndex = 5
      OnExecute = acVAlignExecute
    end
    object acVAlignBottom: TAction
      Tag = 2
      Category = 'VAlign'
      Caption = 'acVAlignBottom'
      GroupIndex = 5
      OnExecute = acVAlignExecute
    end
    object acVAlignClient: TAction
      Tag = 3
      Category = 'VAlign'
      Caption = 'acVAlignClient'
      GroupIndex = 5
      OnExecute = acVAlignExecute
    end
    object acVAlignParent: TAction
      Tag = 4
      Category = 'VAlign'
      Caption = 'acVAlignParent'
      GroupIndex = 5
      OnExecute = acVAlignExecute
    end
    object acDirectionHorizontal: TAction
      Category = 'Directions'
      Caption = 'acDirectionHorizontal'
      GroupIndex = 6
      ImageIndex = 42
      OnExecute = acDirectionsExecute
    end
    object acDirectionVertical: TAction
      Tag = 1
      Category = 'Directions'
      Caption = 'acDirectionVertical'
      GroupIndex = 6
      ImageIndex = 41
      OnExecute = acDirectionsExecute
    end
    object acDirectionTabbed: TAction
      Tag = 2
      Category = 'Directions'
      Caption = 'acDirectionTabbed'
      GroupIndex = 6
      ImageIndex = 25
      OnExecute = acDirectionsExecute
    end
    object acBorder: TAction
      Category = 'Buttons'
      Caption = 'acShowBorder'
      GroupIndex = 7
      OnExecute = acBorderExecute
    end
    object acExpandButton: TAction
      Category = 'Buttons'
      Caption = 'Show Expand Button'
      OnExecute = acExpandButtonExecute
    end
    object acTextPositionLeft: TAction
      Category = 'TextPosition'
      Caption = 'acTextPositionLeft'
      GroupIndex = 8
      OnExecute = acTextPositionExecute
    end
    object acTextPositionTop: TAction
      Tag = 1
      Category = 'TextPosition'
      Caption = 'acTextPositionTop'
      GroupIndex = 8
      OnExecute = acTextPositionExecute
    end
    object acTextPositionRight: TAction
      Tag = 2
      Category = 'TextPosition'
      Caption = 'acTextPositionRight'
      GroupIndex = 8
      OnExecute = acTextPositionExecute
    end
    object acTextPositionBottom: TAction
      Tag = 3
      Category = 'TextPosition'
      Caption = 'acTextPositionBottom'
      GroupIndex = 8
      OnExecute = acTextPositionExecute
    end
    object acCaptionAlignHorzLeft: TAction
      Category = 'CaptionAlignHorz'
      Caption = 'acCaptionAlignHorzLeft'
      GroupIndex = 9
      OnExecute = acCaptionAlignHorzExecute
    end
    object acCaptionAlignHorzCenter: TAction
      Tag = 2
      Category = 'CaptionAlignHorz'
      Caption = 'acCaptionAlignHorzCenter'
      GroupIndex = 9
      OnExecute = acCaptionAlignHorzExecute
    end
    object acCaptionAlignHorzRight: TAction
      Tag = 1
      Category = 'CaptionAlignHorz'
      Caption = 'acCaptionAlignHorzRight'
      GroupIndex = 9
      OnExecute = acCaptionAlignHorzExecute
    end
    object acCaption: TAction
      Caption = 'acCaption'
      OnExecute = acCaptionExecute
    end
    object acCaptionAlignVertTop: TAction
      Category = 'CaptionAlignVert'
      Caption = 'acCaptionAlignVertTop'
      GroupIndex = 10
      OnExecute = acCaptionAlignVertExecute
    end
    object acCaptionAlignVertCenter: TAction
      Tag = 1
      Category = 'CaptionAlignVert'
      Caption = 'acCaptionAlignVertCenter'
      GroupIndex = 10
      OnExecute = acCaptionAlignVertExecute
    end
    object acCaptionAlignVertBottom: TAction
      Tag = 2
      Category = 'CaptionAlignVert'
      Caption = 'acCaptionAlignVertBottom'
      GroupIndex = 10
      OnExecute = acCaptionAlignVertExecute
    end
    object acGroup: TAction
      Caption = 'acGroup'
      ImageIndex = 20
      OnExecute = acGroupExecute
    end
    object acUngroup: TAction
      Caption = 'acUngroup'
      ImageIndex = 21
      OnExecute = acUngroupExecute
    end
    object acVisibleItemsMakeFloat: TAction
      Category = 'Buttons'
      Caption = 'Make/Stop Float'
      Hint = 'Make/Stop Float'
      ImageIndex = 23
      OnExecute = acVisibleItemsMakeFloatExecute
    end
    object acAvailableItemsMakeFloat: TAction
      Category = 'Buttons'
      Caption = 'Make/Stop Float'
      Hint = 'Make/Stop Float'
      ImageIndex = 23
      OnExecute = acAvailableItemsFloatExecute
    end
    object acCollapsible: TAction
      Caption = 'acCollapsible'
      OnExecute = acCollapsibleExecute
    end
  end
  object ilActions: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 25690128
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000302017FF00000002000000090000000F00000011000000120000000D0000
          0003000000000000000000000000000000000000000000000000000000000000
          0000332319FF0000000650433ABE6C594CFF675447FF645143FF46382FC30000
          000C000000000000000000000000000000000000000000000000000000000000
          000036251BFF00000006766257FFF4E3D3FFE5C3A0FFE2BE9BFF635044FF0000
          0010000000000000000000000000000000000000000000000000000000000000
          000039291FFF000000047B675CFFFAEEE2FFF9EBDFFFF3E1D0FF675447FF0000
          000D000000000000000000000000000000000000000000000000000000000000
          00003E2C22FF000000015E4F47BF7A675CFF766256FF715D51FF504139C30000
          0007000000000000000000000000000000000000000000000000000000000000
          0000423125FF0000000000000003000000070000000A0000000E0000000F0000
          000F0000000F0000001100000013000000150000000F00000004000000000000
          0000463529FF000000005C4F47B87D695EFF79655AFF766256FF725E52FF6E5B
          4FFF6B584CFF685548FF655244FF634F42FF46382FC60000000F000000000000
          00004B392DFF00000000836F65FFF6E6D6FFEBCBA9FFE9C7A6FFE7C5A3FFE5C2
          A0FFE3BF9CFFE1BD98FFDFBB96FFDEB893FF635043FF00000014000000000000
          00004F3D31FF00000000867267FFFCF0E4FFFBEFE2FFFAEDE1FFF9ECDFFFF7EA
          DDFFF6E8DAFFF5E6D8FFF3E5D6FFEFDDCAFF655245FF00000011000000000000
          0000544135FF0000000065574FBD867268FF837065FF806C61FF7D695EFF7966
          5BFF766256FF715E53FF6E5B4EFF6B584BFF4D3F36C60000000A000000000000
          0000574438FF000000000000000300000007000000090000000C000000100000
          001300000017000000140000000B000000090000000700000002000000000000
          00005B483BFF00000001584A41B9766257FF725E52FF6E5A4EFF6A564AFF6653
          46FF645043FF46382EC50000000E000000000000000000000000000000000000
          00005E4A3EFF000000007F6B61FFF6E6D6FFEBCBA9FFE9C8A6FFE7C4A3FFE4C1
          9EFFE2BE99FF645144FF00000012000000000000000000000000000000000000
          0000614D40FF00000000836F65FFFBF0E4FFFBEFE3FFFAEEE1FFF9ECDEFFF7EA
          DCFFF1E0CEFF685548FF0000000F000000000000000000000000000000000000
          0000634F42FF0000000064554EBD847066FF816C62FF7D695EFF796559FF7461
          55FF705C50FF504239C300000008000000000000000000000000000000000000
          0000634F42FF0000000000000000000000000000000000000000000000010000
          0003000000040000000400000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000ACCBBFFF6C9E7DFF357846FF0F5C20FF0F5B1FFF3575
          46FF6C9B7DFFACC9BFFF00000000000000000000000000000000000000000000
          00000000000081B093FF2C7B3DFF197B2BFF138023FF188B26FF178825FF117A
          1FFF0A6818FF236C31FF81AA93FF000000000000000000000000000000000000
          000081B193FF237D36FF2A973FFF1F9E31FF1D9E2FFF1C992DFF1B9729FF1992
          27FF188B25FF10771DFF0E621DFF81AA93FF000000000000000000000000ACCE
          BFFF2E8340FF3BA653FF23A939FF21A536FF1FA131FF1D9F2FFF1C9C2DFF1B99
          29FF1A9327FF188D25FF10771DFF236C32FFACC9BFFF00000000000000006CA7
          7EFF36994DFF3EBD5AFF25AD3DFF71BF7FFFE7E7E8FF79C484FF7BC385FFFDFC
          FDFF7BC383FF1A9328FF188C25FF0B6818FF6C9B7DFF00000000000000003687
          46FF59BB73FF30B74DFF26B041FF219736FF75AD7FFFFBFBFAFFFFFFFFFF78AE
          80FF198829FF1B9829FF199126FF117A1FFF367546FF00000000000000001073
          20FF72D38FFF33BC52FF29B446FF26AF40FF209535FF79AD82FF7AAF84FF1C8D
          2CFF1D9D2FFF1B9C2DFF1B972AFF178825FF0F5C1FFF00000000000000001074
          20FF72D48FFF45C565FF2CB94CFF75C686FFE4E4E5FF66AD72FF66AD74FFF9F9
          F9FF7DC988FF1D9F2FFF1B992DFF188B26FF0F5E20FF0000000000000000368C
          47FF59BD73FF5BD17CFF2FBC51FF26A142FF77B083FFF9FAF8FFFDFDFDFF7AB1
          84FF1D9130FF1FA231FF1D9E2FFF138223FF357946FF00000000000000006CAC
          7EFF379F4DFF74DA93FF48C96BFF2EBA4FFF269F41FF7BB186FF7CB287FF2096
          35FF22A739FF20A635FF1FA030FF197C2CFF6C9F7DFF0000000000000000ACD1
          BFFF2E8E40FF56BB6FFF6CD78CFF48CA6AFF2EBB4FFF269F41FF239B3DFF26AF
          41FF25AE3DFF23A93AFF29983FFF2C7C3DFFACCBBFFF00000000000000000000
          000081B994FF268E39FF56BB6FFF74DA93FF5BD07CFF45C564FF33BB52FF30B7
          4DFF3EBD5BFF3BA753FF237F36FF81B093FF0000000000000000000000000000
          00000000000081B994FF2E8E40FF379F4DFF59BD73FF72D48FFF72D38FFF59BC
          73FF37994DFF2E8340FF81B293FF000000000000000000000000000000000000
          00000000000000000000ACD1BFFF6CAC7EFF368D46FF0F7620FF107420FF3689
          46FF6CA87EFFACCEBFFF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000100000003000000010000000000000000000000000000
          0000000000010000000400000001000000000000000000000000000000000000
          00000000000300000011040315480000000F0000000200000000000000000000
          0003000000130303135000000019000000050000000000000000000000000000
          0003000000140C0B41AA14136EFD060624780000001100000002000000020000
          0014050522820E0D64FD080738B6000000210000000500000000000000010000
          000D0C0C42A6151573FF071EC9FF14136EFD0606247800000013000000140505
          237E0F0F67FD061BC7FF0E0D65FF080738B60000001800000001000000020504
          173F181675FA0B28CEFF0625DCFF071EC9FF121479FD0606247E060624801011
          74FD061BC7FF041DD8FF061BC5FF0E0D63FC0302134F00000004000000010000
          00080808286B181774FB0A28CEFF0525DCFF071ECAFF121479FE121378FE061B
          C7FF041DD8FF061BC6FF100E66FC0504227F0000001200000001000000000000
          0001000000090808276A171A7EFB0A28CEFF0625DCFF061ECBFF061CCAFF041D
          D8FF061BC6FF101171FB0505237C000000120000000200000000000000000000
          0000000000010000000A0908286D171A7FFC0929CFFF0627DDFF0420DAFF061C
          C9FF121376FD0605247E00000012000000020000000000000000000000000000
          000000000001000000080908286A181B82FE0C2ED4FF072CE0FF0625DCFF071E
          CCFF121479FE0606247A00000011000000020000000000000000000000000000
          00010000000609092A631A1E86FD0E3CDAFF0B39E7FF6379E3FF6175E0FF0625
          DCFF071ECAFF121478FD060524740000000F0000000200000000000000000000
          00030A0A2B5E1D1C7EFC1348DFFF0F49EFFF6580E7FF181B80FC161A7FFC6175
          DFFF0625DCFF071ECAFF14136FFD060624730000000C00000001000000000504
          13281E1E83FC4E7BEBFF1356F4FF6789EAFF191D84FA090828680808276A1719
          7EFA6275DFFF0625DCFF071EC9FF14136EFD0302103700000002000000000000
          00020E0D3A751F1E85FF88A7F1FF1B2087FA09092A6100000007000000080808
          286717197EFA6175DFFF161573FF090832870000000C00000001000000000000
          0000000000030E0D3A751C238CFA0A092B5D0000000500000001000000010000
          00070808286517197EFA0A0933820000000D0000000200000000000000000000
          0000000000000000000204041327000000030000000000000000000000000000
          0001000000050404112E00000008000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000ACCBBFFF6C9E7DFF357846FF0F5C20FF0F5B1FFF3575
          46FF6C9B7DFFACC9BFFF00000000000000000000000000000000000000000000
          00000000000081B093FF2C7B3DFF197B2BFF138023FF188B26FF178825FF117A
          1FFF0A6818FF236C31FF81AA93FF000000000000000000000000000000000000
          000081B193FF237D36FF2A973FFF1F9E31FF1D9E2FFF1C992DFF1B9729FF1992
          27FF188B25FF10771DFF0E621DFF81AA93FF000000000000000000000000ACCE
          BFFF2E8340FF3BA653FF23A939FF21A536FF1FA131FF7BC385FF7DC486FF1B99
          29FF1A9327FF188D25FF10771DFF236C32FFACC9BFFF00000000000000006CA7
          7EFF36994DFF3EBD5AFF25AD3DFF22A939FF79C585FFFBFBFAFFFFFFFFFF7BC4
          85FF1B9929FF1A9328FF188C25FF0B6818FF6C9B7DFF00000000000000003687
          46FF59BB73FF30B74DFF26B141FF73C180FFE7E7E8FF76AA7EFF78AC81FFFDFC
          FDFF7BC485FF1B9929FF199126FF117A1FFF367546FF00000000000000001073
          20FF72D38FFF33BC52FF29B346FF219A39FF1B7B2DFF7BBC87FF7DBC88FF1876
          27FF1A8C2BFF1B9B2DFF1B972AFF178825FF0F5C1FFF00000000000000001074
          20FF72D48FFF45C565FF2CB94CFF29B446FF7CCB8BFFFBFBFAFFFFFFFFFF7EC8
          8AFF1FA232FF1D9F2FFF1B992DFF188B26FF0F5E20FF0000000000000000368C
          47FF59BD73FF5BD17CFF2FBD51FF77C889FFE7E7E8FF77AD82FF7AAF84FFFDFC
          FDFF7ECA8AFF1FA331FF1D9E2FFF138223FF357946FF00000000000000006CAC
          7EFF379F4DFF74DA93FF48C86AFF29A345FF1F8235FF239A3CFF219739FF1B7C
          2DFF1E9434FF20A535FF1FA030FF197C2CFF6C9F7DFF0000000000000000ACD1
          BFFF2E8E40FF56BB6FFF6CD78CFF48C969FF2EBB4FFF2CB84BFF29B346FF26AF
          41FF25AD3DFF23A93AFF29983FFF2C7C3DFFACCBBFFF00000000000000000000
          000081B994FF268E39FF56BB6FFF74DA93FF5BD07CFF45C665FF33BC52FF30B7
          4DFF3EBD5BFF3BA753FF237F36FF81B093FF0000000000000000000000000000
          00000000000081B994FF2E8E40FF379F4DFF59BD73FF72D48FFF72D38FFF59BC
          73FF37994DFF2E8340FF81B293FF000000000000000000000000000000000000
          00000000000000000000ACD1BFFF6CAC7EFF368D46FF0F7620FF107420FF3689
          46FF6CA87EFFACCEBFFF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000030201036638196C000000000B06030C6F3D
          1C75000000000000000000000000000000000000000000000000000000000000
          000000000000301A0C3300000000140B0515EE823BFC391F0E3C7D441F84CC70
          33D80000000028160A2A44251148000000000000000000000000000000000000
          0000000000009C5527A5965225A01F110828402310530F080425100904283C21
          0F5228160A35CC7033DB4F2B1454000000000000000000000000000000000000
          0000000000003A200E3F48271259321C06735B330CBE764410ED744310EE5530
          0CC32D1A067D4425115D0F08041422130824412410450000000000000000AD5F
          2BB7AF602CBA301A0C3D48290A92A3662CFFC88952FFE9A975FFEAAC76FFC88B
          56FF9C632DFF3E24099F3B200F53D17234E04124104500000000000000000905
          02096035186A351F076AA6662AFFD58E58FFEA9B5FFFEE9957FFF09B59FFF0A0
          64FFD7935DFF9A602BFF2D1A067B3C210F490000000000000000090502094425
          11482213092A633A0DB4BF7940FFDD864AFFE18646FFE38B4CFFE68D4CFFE58A
          4BFFE38C4FFFBB7944FF56310CC1321B0D46914F2499603518666F3D1C75E07B
          38ED130B051A874F12E9CA7A43FFD3763AFFD97C3EFFDC7F41FFDB8344FFDC81
          41FFD87B3EFFCE7E46FF744311ED120A05279652259F1F110821000000000603
          01060A06030F894F11E8C46930FFD1753EFFDC8952FFE49561FFE59661FFDF8B
          54FFD57B42FFC56E34FF764411EC0C06031F0B06030C00000000000000007741
          1E7E43251149663B0DAFB15D20FFDF9D76FFEAB999FFEBB999FFEBB99AFFEABA
          9AFFE09F79FFAC5B22FF59330CBB40231051DA7836E73C210F3F361E0D39914F
          2499522D145737200760A25918FFDFB296FFF2D9CBFFF2D9CBFFF2D9CBFFF2D9
          CBFFDFB396FF985417FF321C076F341C0D3D301A0C3311090412000000000000
          00001C10071E4828124E4D2C0A83B57D48FFD8B194FFEFDACDFFEFDACDFFD7B0
          94FFB17948FF4729098E48281257613518680000000000000000000000000000
          00008E4E23969A5426A3201108233720075F673C0EAE894F11E8884E12E8633B
          0DB2351E0766190D06225E331765BB662FC61A0E061B00000000000000000000
          00002D190B3000000000321B0C35874A228F23130926120A051611090416150B
          051A8448218E67381A6D00000000000000000000000000000000000000000000
          00000000000000000000412410459C5527A503020103A45A29AE9C5527A50000
          00004A28124E8046208700000000000000000000000000000000000000000000
          000000000000000000000B06030C0000000000000000391F0E3C3E2210420000
          0000000000000302010300000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000004D0FFF004D0FFF004D0FFF000000000000000000000000000000000000
          000000000001000000050000000E0000001800000020000000210000001B0000
          001100550FFF40D077FF00550FFF000000000000000000000000000000000000
          000100000009100B05383E29129465431ED2825626F36B461FF6005E10FF005D
          10FF005D0FFF41D179FF005D10FF005E10FF015D10FF00000000000000000000
          00082015095462401CC8AD6F36FFC87F41FFDC8B49FFAD6D39FF016610FF43D4
          7EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF0000000000000003100A
          053162411BC4B77538FFE08C49FFE08C49FFE08C4AFFBA743DFF016F11FF016F
          10FF017011FF47D781FF016F10FF016F11FF016F10FF00000000000000073F28
          1186AB6B31FFDB8545FFDB8645FFDB8544FFDA8644FFD18042FFB66F39FF8753
          2AFF017611FF48DB86FF017711FF0000001100000000000000000000000A6942
          1AC4BF7336FFD67F40FFD57F40FFD67F40FFD67F40FFD57F3FFFD67F40FFB16A
          34FF017C11FF017C11FF017C11FF0000001A00000000000000000000000B8855
          20EECA7538FFCF783AFFD78348FFDE8C54FFE3935DFFE3935DFFDE8C54FFCD7D
          45FFAD6430FF9E5C2CFF71451CF50000001F0000000000000000000000098954
          20EDC46F32FFD78751FFE9A678FFE9A678FFE9A578FFE9A679FFE9A678FFE9A5
          77FFD78751FFC46F33FF8A541FF30000001E0000000000000000000000066C41
          17BFB5662AFFE6AC86FFEFBD9BFFEFBD9BFFEFBC9AFFEFBD9BFFEEBC9BFFEFBC
          9BFFE6AC85FFB5652AFF6D4118CF000000160000000000000000000000034327
          0E79A75E23FFEABFA3FFF5D6C1FFF6D6C1FFF5D6C1FFF6D6C1FFF5D6C1FFF5D6
          C1FFEABFA3FFA75E23FF43280E910000000C000000000000000000000001120A
          03226B3F15B6B97B4BFFFAECE3FFFBEDE4FFFBEDE4FFFBEDE3FFFBEDE4FFFAEC
          E3FFB97B4AFF6B3E15C5120A0335000000040000000000000000000000000000
          00012314073E6C3E14B6B06F3AFFCFA381FFEFDFD2FFEFDED2FFCFA381FFAF6F
          3AFF6C3E14C12314065000000008000000000000000000000000000000000000
          000000000001120A032146280C77724013BD915319ED915219ED724114C24627
          0C81120A032D0000000600000001000000000000000000000000000000000000
          0000000000000000000000000002000000040000000700000008000000080000
          0005000000020000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000002000000080000000E000000140000001400000010000000090000
          000200000000004D0FFF004D0FFF004D0FFF0000000000000000000000000000
          00020000000F01200774023A0DBE034D11ED034B11ED02380DC0011E07780000
          00130000000300550FFF40D077FF00550FFF0000000000000000000000010000
          000D022F0A93167326FF2F943FFF44B054FF44AF54FF2F923FFF125B20FF005E
          10FF005D10FF005D0FFF41D179FF005D10FF005E10FF015D10FF000000050123
          066D167726FF32A544FF29AF3DFF18A92EFF18A92EFF29AF3DFF278035FF0166
          10FF43D47EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF000000070243
          0CB7259035FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF168726FF016F
          11FF016F10FF017011FF47D781FF016F10FF016F11FF016F10FF00000009035C
          11EA229530FF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0C8A1BFF1C7A
          27FF07320CFE017611FF48DB86FF017711FF000000030000000000000008035E
          10E911861CFF1B942CFF34AA4DFF48BD68FF48BD68FF34AB4DFF1C952CFF1182
          1CFF104A16FF017C11FF017C11FF017C11FF0100001300000002000000050247
          0CB3077211FF64B774FF8DD1A0FF8DD2A0FF8DD2A0FF8DD1A0FF65B774FF076C
          11FF3C5E23FFBE8053FFAA764DFF83552BFF2F1D0A7800000009000000020127
          0665046C11FF8AC091FFC4E3CAFFC4E3CAFFC4E3CAFFC4E3CAFF89BF91FF0461
          11FF7F6F34FFDF8B4FFFE39156FFBE7E4BFF593512C000000010000000000000
          000602360989398B43FF88BA8DFFC7E0C9FFC7DFCAFF88B98DFF398544FF5E66
          28FFCF8147FFDB8547FFDA8046FFD0824DFF794819ED00000014000000000000
          0001000000060126076502470CB50F6513FD136515FF3E6620FF807439FFD991
          60FFE49864FFE08E59FFD78049FFC8743CFF7B4918ED00000013000000000000
          000000000000000000020000000A663E12B5AE6028FFD99C77FFE6B798FFEBB9
          9BFFEBBB9DFFEBBB9DFFE1A37DFFB0622BFF5D3812BD0000000E000000000000
          000000000000000000000000000239220A65A66021FFE0B59AFFF2DACCFFF2DA
          CCFFF2DACCFFF3DACCFFE0B59AFF9D5A20FF341F0A7200000007000000000000
          0000000000000000000000000000010000064F2F0E88B9824FFFDAB498FFF0DC
          CFFFF0DCCFFFD9B398FFB47F4FFF492C0E920100000E00000002000000000000
          0000000000000000000000000000000000010100000639230A64694014B38D55
          1AE98B551BEA663F13B636210A6C0100000C0000000200000000000000000000
          0000000000000000000000000000000000000000000000000002000000050000
          0007000000080000000700000004000000010000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000030000
          000E0000001400000016000000180000001A0000001C0000001E000000200000
          002200000024000000260000002700000029000000210000000A0000000B036B
          0EFF02690EFF03680EFF03670EFF02650EFF03640EFF02630EFF02600EFF025F
          0EFF025E0EFF025E0EFF035C0EFF035B0EFF025A0EFF000000210000000B036C
          0EFF60C27EFF2BAE54FF219945FF27AB4FFF25AB4DFF1A943EFF20A948FF1FA8
          45FF158E37FF1AA640FF18A43FFF0F8830FF025A0EFF0000002900000009026D
          0EFF68D08BFF2BAB53FF33BE5FFF31BD5DFF25A44CFF2CBB58FF29BA55FF1E9C
          44FF24B750FF22B54CFF17953BFF18A43EFF025C0EFF0000002700000008026F
          0EFF61C380FF39C166FF36C063FF2AA951FF31BD5DFF2FBC5AFF22A249FF28B9
          54FF27B852FF1B9A41FF22B54CFF1BA540FF025E0EFF00000025000000060371
          0EFF6CD18EFF3CC268FF2EAD56FF36C063FF33BF5FFF27A64EFF2EBC5AFF2CBA
          58FF209F46FF26B852FF24B750FF158D37FF025E0EFF00000023000000050371
          0EFF6ED290FF32B15AFF3CC268FF39C166FF2CAB54FF33BE5FFF31BD5EFF25A4
          4CFF2CBB58FF29B954FF1E9C44FF1FA745FF025F0EFF00000020000000040373
          0EFF66C785FF3FC56EFF3EC36BFF30AF58FF39C165FF35BF63FF29A951FF31BD
          5DFF2EBB5BFF22A249FF28BA55FF21A848FF03620EFF0000001E000000030374
          0EFF71D494FF41C570FF34B35DFF3DC36BFF3CC268FF2EAD56FF36BF63FF33BE
          5FFF26A74EFF2EBC5BFF2CBB58FF1A943EFF03630EFF0000001C000000030375
          0EFF73D495FF37B660FF42C56FFF40C46EFF32B15AFF3BC268FF39C166FF2BAB
          53FF34BE60FF31BD5DFF24A54BFF25AB4DFF02640EFF0000001A000000020377
          0EFF69CA88FF47C774FF45C772FF35B45FFF3FC46EFF3EC46BFF30B058FF39C1
          66FF35C063FF29A951FF31BD5EFF28AC4FFF02650EFF00000017000000010378
          0EFF75D598FF48C775FF38B862FF45C772FF42C670FF34B35CFF3EC36AFF3BC2
          68FF2DAE56FF35BF64FF34BF60FF219A45FF02670EFF00000015000000000379
          0EFF77D79AFF3AB964FF49C776FF47C774FF37B660FF42C56FFF40C56EFF32B1
          5BFF3BC268FF39C166FF2BAB53FF2BAE54FF03680EFF00000013000000000379
          0EFF6ACA8AFF77D69AFF75D698FF69CA88FF72D495FF71D494FF65C685FF6DD2
          90FF6BD18EFF62C280FF68D08BFF5FC17EFF03690EFF0000001000000000037B
          0EFF03790EFF03790EFF03780EFF03770EFF03750EFF03740EFF03730EFF0371
          0EFF02710EFF03700EFF036D0EFF026C0EFF026C0EFF0000000B000000000000
          0000000000000000000000000000000000010000000100000002000000030000
          0003000000040000000500000006000000080000000800000003}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000021732
          68BB1E4190FF1B3E8DFF193D8AFF183989FF102861C30000000C000000000000
          0000000000000000000000000000000000000000000000000000000000022148
          94FF86D8FBFF43BFF5FF39B9F2FF31B4EEFF183989FF0000000F000000000000
          0000000000005C5C5CFF565656FF515051FF4B4B4AFF464645FF414140FF244A
          97FF8CDCFEFF4DC5F9FF52C4F7FF79D0F6FF193B8AFF0000000D000000000000
          000000000000636262FF0000000000000000000000000000000000000000264E
          98FF90DEFFFF70B5E3FF204592FF1D4190FF142E69C300000007000000000000
          000000000000686969FF00000000000000000000000000000000000000001E3D
          73BD264E99FF1B3970BE00000004000000040000000400000001000000000000
          0000000000006F6F6FFF00000000000000000000000000000000000000010000
          00070000000E0000001200000016000000180000001200000005000000000000
          000000000000767575FF00000000000000000000000000000000000000031731
          68BC1E4190FF1B3E8DFF193D8AFF183989FF102761C900000012000000000000
          0000000000007C7B7BFF00000000000000000000000000000000000000022148
          94FF86D8FBFF43BFF5FF39B9F2FF31B4EEFF183989FF00000017000000000000
          000000000000828282FF7C7C7CFF757675FF6F6F6FFF686868FF636263FF244A
          97FF8CDCFEFF4DC5F9FF52C4F7FF79D0F6FF193B8AFF00000014000000000000
          000000000000888888FF0000000000000000000000000000000000000000264E
          98FF90DEFFFF70B5E3FF204592FF1D4190FF142E68C60000000B000000010000
          00070000000E8E8E8EFF00000016000000180000001200000005000000001E3D
          73BD264E99FF1B3970BE00000006000000060000000600000002000000031731
          68BC1E4190FF1B3E8DFF193D8AFF183989FF102761C900000012000000000000
          0000000000000000000000000000000000000000000000000000000000022148
          94FF86D8FBFF43BFF5FF39B9F2FF31B4EEFF183989FF00000017000000000000
          000000000000000000000000000000000000000000000000000000000000244A
          97FF8CDCFEFF4DC5F9FF52C4F7FF79D0F6FF193B8AFF00000014000000000000
          000000000000000000000000000000000000000000000000000000000000264E
          98FF90DEFFFF70B5E3FF204592FF1D4190FF142E68C60000000B000000000000
          0000000000000000000000000000000000000000000000000000000000001E3D
          73BD264E99FF1B3970BE00000006000000060000000600000002000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000020000
          0010000000220000002C0000002F0000002F0000003100000032000000320000
          00330000003400000035000000340000002A00000014000000030000000D744E
          26CA9F6B33FF9D6932FF4C453FFF453E38FF3C352FFF332C27FF2A231EFF211B
          16FF1F1914FF1F1914FF8A5824FF895723FF613E18D0000000120000001CA46F
          37FFF1CBA2FFEAB77FFF5D564FFFE4AC70FFE2AA6DFF433C36FF37302BFF2A24
          1FFF211B16FF1F1914FFD99E5FFFD99D5EFF885723FF0000002900000021A671
          39FFFCD7B1FFFAC790FF6E665FFFF9BF81FFF8BD7FFF564F48FF49423CFF3B35
          2FFF2E2722FF221C17FFF3B575FFDA9E60FF8A5824FF000000330000001EA873
          3AFFFCDAB4FFFBC992FF79716AFFF9C184FFF9BF81FF655E57FF5A534CFF4C45
          3FFF3C3630FF2E2722FFF4B673FFDCA063FF8C5A25FF000000320000001BAB75
          3BFFFEDCB8FFFCCB95FF877F77FF7D756DFF787069FF716A62FF686059FF5B54
          4DFF4C453FFF3C3530FFF4B777FFDCA264FF8D5C26FF0000003000000018AD77
          3DFFFEDEBCFFFDCC98FFFCC589FFFBC387FFFAC285FFF9C084FFF9BF81FFF8BD
          7FFFF6BC7DFFF6B97AFFF5B878FFDFA466FF8F5D28FF0000002E00000016AF79
          3FFFFEE0BFFFFBCB97FFF1BB80FFEAB57BFFE7B176FFE4AE73FFE2AB71FFE1A9
          6EFFDFA76CFFE3AB6EFFF0B477FFE1A669FF915F29FF0000002C00000014B17A
          40FFFFE1C1FFF6C694FFBE8D57FFB98955FFB48552FFAF814FFFAB7C4BFFA678
          48FFA17444FF9E7141FFE3AB6EFFE4A96CFF93612AFF0000002900000012B37C
          41FFFFE3C5FFF6C795FFC1905AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFA37544FFDFA76CFFE5AD6FFF96632CFF0000002600000010B47E
          42FFFFE5C8FFF8CB9BFFC4915CFFFFFFFFFFA9A9A8FFA3A2A3FF9B9B9BFF9393
          93FFFFFFFFFFA87A49FFE1AA6EFFE9B073FF98652EFF000000230000000EB57F
          44FFFFE6CAFFF9CD9EFFC4915CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFAF7F4EFFE2AC6FFFECB376FF9A6730FF000000200000000DB780
          44FFFFE7CDFFFBD5ACFFC4915CFFFFFFFFFFA8A8A8FFA3A3A2FF9B9B9BFF9494
          93FFFFFFFFFFB58551FFE4B27BFFEFBC86FF9D6931FF0000001C00000009B881
          45FFFFEBD6FFFCE4C9FFC4915CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFBA8B56FFE7C199FFF6D0A8FF9F6B33FF00000014000000038860
          33C1B88246FFB68044FFC4915CFFC4915CFFC4915CFFC4915CFFC4915CFFC491
          5CFFC3905BFFBF8E59FFA26E35FFA47037FF775128C600000008000000000000
          0003000000070000000A0000000B0000000C0000000D0000000E0000000F0000
          00100000001100000011000000110000000E0000000600000001}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000020000000700000010000000170000001A0000
          0017000000120000000900000002000000000000000000000000000000000000
          000000000000000000020000000F2E1C0C61693F1BB68F5625E5A5632AFC8C54
          22E5653C19B9341F0C7300000015000000050000000100000000000000000000
          0000000000010100000B4E2F158AA6672EF4AB682CFFA7642AFFAA672CFFAE6A
          2EFFB06C2FFFA76429FE553112AA0D0803300000000500000000000000000000
          0000000000031F1308408C5628D23D240F790F0904350100001F211308544C2D
          1299995B25F1B26C2FFFA8642AFF502C0FAA0000001500000002000000000000
          00000402010B553215861E1107400000000B0000000300000002000000050000
          0011331D0B73935621F1AD692DFF985720FE2C1707730000000A000000010000
          00070000000D000000140000001400000014000000130000000B000000020000
          00020000001146260D989D5A21FFA15D24FF552C0EB80000001100000009C170
          2DFFB76828FFAE5F22FFA2571DFF9C521AFF975018FF48270C8D000000060000
          0000000000061C0F0553904D19FFA66228FF784016E4000000160000000CC776
          32FFD58D4DFFC57636FFBE6E2FFFB16428FF5D310FAB00000011000000020000
          0000000000040F0702398C4815FFA96429FF955523FC000000190000000BCC7B
          36FFDD9552FFD0823EFFC77635FFB46527FF2F19085F00000007000000000000
          0000000000062311055C934F1CFFA76329FF8B5527E3000000150000000AD07F
          39FFDF9B55FFDB914CFFD28844FFC17332FF6135139D0000000E000000050000
          00050000001147230A99A35D26FFA9662DFF6D4725B40000000F00000009D182
          3EFFD98F48FFDA914DFFDD9654FFD38746FFBB7031F66236139C2A1608542815
          0656562D0FA0A15D28F5B26C32FFB37239FE3D29166C0000000800000007D185
          44FF67401F8B955D2BBDD78E49FFDF9856FFD78D4BFFC77839FFB8692AFFB163
          28FFB76C32FFC2783FFFC0783FFF684424A00000000F00000001000000046844
          258400000008130C052071461F95D18540FDD68B49FFD98F51FFD99053FFD388
          4BFFC97E44FFBF783EFE6742219D120C07270000000300000000000000010000
          00020000000100000001000000064329125C7D4B21A5A8622ADCBE6E2FFBA15F
          2ADD734620A93D2713620000000B000000030000000100000000000000000000
          0000000000000000000000000000000000020000000500000007000000080000
          0008000000070000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000080000001E00000027000000200000000900000000000000000000
          0000000000000000000000000002000000040000000200000000000000000000
          00000000001C176126FF176126FF176026FF0000001F00000000000000000000
          000000000000000000020000000D071E0C600000000A00000000000000000000
          00020104012F1A6529FF4CA967FF175F26FD0000002400000000000000000000
          0000000000010000000A071F0B68176426FF0000001400000000000000000000
          0009071B0A6F2C7A3EFF31974DFF135120E90000001E00000000000000000000
          000100000008071F0C632E8142FF176526FF0000001900000003000000080103
          012310411ABF2C8C46FF1A8236FF0E3B17BD0000001500000000000000010000
          0006071F0B5E2F8443FF37B461FF176526FF061C0B6601040126061C0B651141
          1ABB30904AFF229A47FF247838FE07200C770000000B0000000000000004071F
          0C59308644FF3DBA67FF39B763FF176626FF176526FF186728FF1E7733FF2A90
          46FF41AC61FF388F4EFF0D3615A900000015000000020000000007200C52378A
          4AFF7ED39BFF69CB8BFF3CB866FF39B562FF76CB92FF72C68EFF63B77CFF4D9E
          63FF338146FE0C3615A40209032D000000040000000000000000000000030720
          0B5536894AFF7DD29BFF69C98AFF176726FF176626FE176425FB135520DF0E3E
          17AE07210C690000000F00000004000000000000000000000000000000000000
          000307200C5436894AFF7DD29AFF176825FF000000120000000B0000000B0000
          0009000000050000000100000000000000000000000000000000000000000000
          00000000000307200B54368949FF176825FF0000000800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000207200B54176826FF0000000600000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000207200B500000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000040000
          000F000000140000001100000005000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000C1766
          25FF176526FF176525FF00000011000000000000000000000000000000020000
          00050000000200000000000000000000000000000000000000000000000C1764
          25FB69C88AFF196828FF010401210000000200000000000000000000000B071D
          0C63000000110000000200000000000000000000000000000000000000091357
          20DD60BC7EFF267F3BFF071C0B5D000000060000000000000000000000151762
          26FF071D0B730000001100000002000000000000000000000000000000050E40
          17A64DA666FF43A25EFF0F421AB20103011900000006000000030000001A1762
          26FF2B7B3EFF071D0B7300000011000000020000000000000000000000020722
          0C5B348848FD6CCC8DFF3C9F58FF0F431AB1071C0A5D01040223061B0B671763
          25FF59B675FF1A732FFF071D0B73000000110000000200000000000000000000
          00050D381494469F5FFF6BCA8CFF41A05DFF257C3BFF186728FF176326FF1763
          26FF27A24DFF229E47FF19722FFF071D0C720000001000000002000000000000
          00010209031C0D381494338847FD4AA363FF5CB879FF64C283FF63C181FF60BE
          7EFF5DBB7AFF26A14BFF229D46FF257738FF071D0C6300000005000000000000
          0000000000010000000407210C5B0E3E17A5135620DC176325FB176526FE1764
          25FF5EBD7CFF28A34EFF26793AFF071E0B6C0000000E00000002000000000000
          000000000000000000000000000200000004000000070000000A000000161764
          26FF61BE7EFF287C3CFF071E0B680000000C0000000200000000000000000000
          00000000000000000000000000000000000000000000000000000000000B1765
          26FF2E8141FF071E0C630000000A000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000071765
          26FF071F0B5D0000000800000001000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000003071F
          0B53000000050000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000442717FF4427
          17FF140B074D422516FF422516FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000442717FF0000000000000000000000000000000000000000000000070000
          000B0000000E0000000F00000011000000120000001400000016000000170000
          0019452819FF0000001C0000001E0000001E0000001400000005883C0EBDBA53
          13FFB95012FFB74F11FFB44E10FFB24C10FFB14A0FFFAE490EFFAC480EFFAB46
          0DFF4B2B1AFF937967FF907866FF8F7665FF665447CC00000014BF5614FFD68D
          48FFD38B47FFD28945FFD08743FFCE8541FFCE8440FFCC833EFFCB823DFFCA80
          3CFF4C2E1EFFF8F2EAFFF7F0E7FFF7EFE4FF8F7463FF0000001DC05816FFD78E
          4AFFFFFFFFFFD48B46FFD28844FFDEAB7CFFFFFFFFFFFFFFFFFFFFFFFFFFCA81
          3DFF503120FFF8F3ECFFF8F2EAFFF7F0E7FF8F7663FF0000001DC25916FFD88F
          4BFFFFFFFFFFD58B47FFD38A46FFFFFFFFFFD08742FFCE8541FFCC833FFFCB82
          3DFF553424FFF9F5EEFFF8F3EBFFF8F2EAFF907866FF0000001CC45A16FFDA91
          4CFFFFFFFFFFD68D49FFD58B47FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCD83
          3FFF593727FFF9F5F0FFF8F5EEFFF8F2EBFF927967FF0000001AC55B18FFDB91
          4EFFFFFFFFFFFFFFFFFFE2AF80FFFFFFFFFFD28944FFD18743FFFFFFFFFFCD84
          40FF5C3C2AFFFAF6F2FFF9F5F0FFF9F4EEFF937969FF00000018C75D18FFDB93
          4FFFFFFFFFFFD98F4CFFFFFFFFFFE2AF7FFFFFFFFFFFFFFFFFFFDEAB7CFFCE85
          41FF603D2EFFFBF8F3FFFAF6F2FFF9F5F0FF947A69FF00000016C85F19FFDD94
          50FFDC924EFFDA914DFFD88F4BFFD68D49FFD48B47FFD38A46FFD18844FFD087
          42FF654231FFFBF8F5FFFAF7F3FFFAF6F2FF947C6AFF00000013964613BDC85E
          19FFC75B18FFC45A17FFC05816FFC05815FFBD5414FFBC5413FFB95213FFB750
          11FF674534FF9D8474FF998171FF98806EFF6F5C50C80000000C000000000000
          0000000000000000000100000002000000020000000400000005000000060000
          00086B4737FF0000000B0000000C0000000D0000000A00000003000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00006E4B3AFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000724E3DFF724E
          3DFF2218124D704B3CFF6E4B3AFF000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00030000000E0000001500000012000000060000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000B004D0FFF004D0FFF004D0FFF000000130000000000000000000000060000
          0008000000090000000A0000000B0000000C0000000D0000000F000000150000
          002200550FFF40D077FF00550FFF000000280000001300000006A69082FFA590
          81FFA48D7FFFA38C7EFFA18B7CFFA0897AFF9E8878FFF4EDE4FF005E10FF005D
          10FF005D0FFF41D179FF005D10FF005E10FF015D10FF00000013A79284FFFFFF
          FFFFEFE7DFFFEFE7DFFFEEE6DDFFEEE6DCFFEEE6DCFFF4EEE6FF016610FF43D4
          7EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF00000016A99486FFFFFF
          FFFFAC703AFFAB6F38FFF8F3ECFFA86A33FFA76930FFF7F0E9FF016F11FF016F
          10FF017011FF47D781FF016F10FF016F11FF016F10FF0000000FAA9487FFFFFF
          FFFFAD723CFFFAF3EEFFF9F3ECFFF9F3EBFFF9F3EBFFF9F3ECFFF6F1E9FFF2EB
          E3FF017611FF48DB86FF017711FF000000230000000B00000004AB9688FFFFFF
          FFFFF9F5EFFFF9F4EDFFF9F4EDFFF9F4ECFFF8F3ECFFF9F4ECFFF8F3ECFFF7F0
          E8FF017C11FF017C11FF017C11FF000000160000000000000000AC9789FFFFFF
          FFFFB07640FFF9F5EFFFF9F4EEFFF9F3EDFFF9F4ECFFF9F4EEFFF9F3EDFFF9F3
          ECFFF7F0E8FFF4EEE6FFF3EDE5FF0000000F0000000000000000AD988BFFFFFF
          FFFFB27742FFFAF5F0FFF9F5EFFFF9F4EDFFF9F4EDFFF9F3ECFFF9F3EBFFF8F3
          EBFFA76930FFEEE5DBFF9E8779FF0000000D0000000000000000AE998CFFFFFF
          FFFFB37945FFFAF5F0FFF9F5EFFFFAF5EFFFF9F4EEFFFAF4EDFFF9F3ECFFF9F3
          ECFFA86B33FFEEE6DCFF9F897AFF0000000C0000000000000000AF9B8DFFFFFF
          FFFFFAF6F2FFFAF6F1FFFAF5F0FFF9F5EFFFF9F5EFFFFAF4EEFFF9F4EDFFF8F4
          ECFFF8F3EBFFEEE6DDFFA18A7BFF0000000B0000000000000000AF9B8EFFFFFF
          FFFFB47C48FFFBF7F2FFFAF6F1FFFAF5F0FFF9F5EFFFFAF4EEFFF9F5EEFFF9F4
          EDFFAB6F37FFEEE7DEFFA28C7EFF0000000A0000000000000000B09B8FFFFFFF
          FFFFB67D4AFFB57C48FFFAF6F2FFB37944FFB27743FFB07641FFF9F4EEFFAD72
          3CFFAD703AFFEFE8DFFFA48D7FFF000000090000000000000000B09C8FFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFA58F81FF000000080000000000000000B09C8FFFB09C
          8FFFB09C8FFFB09B8EFFAE9B8DFFAE9A8CFFAD988BFFAC978AFFAB9688FFAA95
          87FFA99386FFA89284FFA69182FF000000060000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00030000000E0000001500000012000000060000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000B004D0FFF004D0FFF004D0FFF000000130000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000070000
          001400550FFF40D077FF00550FFF000000260000001300000006000000050000
          000E0000001200000014000000140000000E0000000400000009005E10FF005D
          10FF005D0FFF41D179FF005D10FF005E10FF015D10FF00000013160E072CA668
          31FFA6672FFFA5652EFFA4642CFF77471FC4000000090000000F016610FF43D4
          7EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF00000016000000040000
          0011704520B89B5F2CF20000001F0000000D0000000300000006016F11FF016F
          10FF017011FF47D781FF016F10FF016F11FF016F10FF0000000F000000000000
          00020B070322AC6D35FF211409460000000400000000000000000000000F5F39
          17B2017611FF48DB86FF017711FF0000001B0000000B00000004000000000000
          000000000008704621B4643E1DA70000001400000010000000120B070330A665
          2DFF017C11FF017C11FF017C11FF000000090000000000000000000000000000
          00000000000338241162C28149FFA66830FFA6662FFFA5652DFFA86830FFEFA4
          69FFB47138FF2B190A600000000C000000020000000000000000000000000000
          000000000001000000099D6331F03823116800000011000000198E5827E4E29A
          60FF824F22D60000001100000001000000000000000000000000000000000000
          000000000000000000044F321980704723B10000000B38231064C07E45FFC481
          48FF412712790000000700000000000000000000000000000000000000000000
          00000000000000000001170E072AA56A38F00B070320794D27B9DB995FFFA165
          2FF20B0703200000000200000000000000000000000000000000000000000000
          00000000000000000000000000047D5028BF5A3B1F8ACE8E55FFD39057FF5837
          1A94000000070000000000000000000000000000000000000000000000000000
          0000000000000000000000000002452C166BCA8B53FFFBB77DFFB3743DFF160E
          072D000000020000000000000000000000000000000000000000000000000000
          0000000000000000000000000000170F0825AB6F38FFAA6D36FF714824AF0000
          0005000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000003000000030000
          0001000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00030000000E0000001500000012000000060000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000B004D0FFF004D0FFF004D0FFF0000001300000000000000000000000C0000
          0011000000130000001500000016000000180000001A0000001D000000230000
          003100550FFF40D077FF00550FFF0000002D00000013000000069A8E86FF998D
          85FF978B83FF958981FF938780FF91857DFF90837BFF8D7F76FF005E10FF005D
          10FF005D0FFF41D179FF005D10FF005E10FF015D10FF000000139B8F89FFEFE6
          DCFFE5D6C5FFE3D3C3FFE2D1C0FFE1D0BEFFE0CFBCFFDCCAB7FF016610FF43D4
          7EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF000000169D918BFFFCF4
          EBFFF9EBDBFFF8E9DAFFF7E8D8FFF7E7D6FF7E684FFFFDFDFDFF016F11FF016F
          10FF017011FF47D781FF016F10FF016F11FF016F10FF0000000F9E938DFFFCF5
          EEFFFAECDDFFF9EADCFFF9EADAFFF8E8D8FF806B50FFFFFFFFFFF4E2D1FFEEDC
          CAFF017611FF48DB86FF017711FF000000320000000B00000004A0958EFFFDF7
          F0FFFAEEDFFFFAECDDFFF9EBDBFFF9E9DAFF816D54FFFFFFFFFFF6E6D4FFF4E3
          D1FF017C11FF017C11FF017C11FF000000240000000000000000A19690FFFDF8
          F2FFFBEEE0FFFAEEDFFFFAECDDFFF9EBDCFF847059FFFFFFFFFFF6E7D7FFF6E6
          D5FFF3E3D1FFDBCAB7FF8C8077FF0000001D0000000000000000A29792FFFDF9
          F4FFFBEFE1FFFAEEE0FFFAEDDFFFF9EDDEFF87745AFFFFFFFFFFF7E9D9FFF7E7
          D7FFF6E6D5FFE0D0BDFF91847CFF0000001A0000000000000000A49994FFFEF9
          F5FFFBEFE2FFFBEFE2FFFAEEE1FFFAEDE0FF89755DFFFFFFFFFFF8EADAFFF8E8
          D8FFF6E7D6FFE1D1BEFF93867FFF000000180000000000000000A49B94FFFEFA
          F6FFFBEFE2FFFBEFE2FFFBEFE1FFFAEEE0FF8C7860FFFFFFFFFFF9EBDBFFF8E9
          DAFFF7E9D8FFE1D2C1FF958880FF000000160000000000000000A59B96FFFEFB
          F8FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE1FF8D7A61FFFFFFFFFFFAECDDFFF9EB
          DCFFF8EAD9FFE3D3C3FF968A83FF000000150000000000000000A69C97FFFEFC
          F9FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE2FF8E7B64FFFFFFFFFFFAEDDFFFFAEC
          DDFFF8EBDCFFE4D6C5FF978B85FF000000130000000000000000A79D98FFFEFD
          FBFFFEFCFAFFFEFBF9FFFEFBF7FFFEFAF7FFFEFAF6FFFEF9F5FFFDF8F3FFFDF7
          F1FFFDF6F0FFF2EBE3FF998C87FF000000110000000000000000A79D98FFA79D
          98FFA69C98FFA59B96FFA59A94FFA49A94FFA39892FFA29791FFA19690FF9F94
          8EFF9D928CFF9C908AFF9B8F89FF0000000C0000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000030000
          000E000000150000001200000006000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000B004D
          0FFF004D0FFF004D0FFF00000013000000000000000000000000000000000000
          00000000000000000000000000030000000C000000140000001C000000290055
          0FFF40D077FF00550FFF00000026000000130000000600000000000000000000
          000000000000000000000000000A665B56C38A7C73FF005E10FF005D10FF005D
          0FFF41D179FF005D10FF005E10FF015D10FF0000001300000000000000000000
          000000000000000000000000000D8F8179FFF3E9E1FF016610FF43D47EFF43D4
          7EFF44D47DFF43D47EFF43D47EFF006610FF0000001600000000000000000000
          000000000000000000000000000C91837CFFF5EDE3FF016F11FF016F10FF0170
          11FF47D781FF016F10FF016F11FF016F10FF0000000F00000000000000000000
          000000000000000000000000000A92857DFFF8EFE6FFF0DAC2FFE4CAB1FF0176
          11FF48DB86FF017711FF000000160000000B0000000400000000000000000000
          0000000000000000000000000009948780FFFBF4EBFFD3BDA4FFF7EFE7FF017C
          11FF017C11FF017C11FF00000008000000000000000000000000000000000000
          0000000000005130138100000008958982FFFCF5ECFF725D44FFD1B89DFF8E80
          78FF000000105131168300000002000000000000000000000000000000000000
          000053331781A26229FF00000008978B84FFF8F1EAFFF2DCC3FFEBD0B6FF9083
          7BFF0000000AA76831FF52331681000000000000000000000000000000005637
          1B81B97D4DFFA5652DFF00000007998C86FFF8F2EBFFD4BDA4FFF9F1E9FF9285
          7DFF00000009AA6C36FFB87D4DFF5232168100000000000000005A3C2083BC83
          53FFCB946AFFA76932FF000000069B8F89FFF8F3ECFF725D44FFCFB69BFF9487
          7FFF00000008AE713BFFCB946AFFB87D4CFF5433178300000000000000005739
          1F7EBC8352FFAA6D36FF000000069C918AFFF8F3EDFFF2DCC4FFECD0B7FF9689
          82FF00000008B17541FFBC8354FF54351B7E0000000000000000000000000000
          000056391F7EAD703AFF000000059E928CFFF9F4EFFFF3DAC4FFECD0B6FF978B
          84FF00000007B37A46FF573A207E000000000000000000000000000000000000
          00000000000056391F7E000000049F948EFFF9F4EFFFFCF6F0FFF6EEE7FF9A8C
          86FF00000006593D247E00000000000000000000000000000000000000000000
          0000000000000000000000000002766F6AC09F948EFF9E938CFF9D918AFF736A
          65C0000000030000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000600000019000000230000001D000000090000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0015004D0FFF004D0FFF004D0FFF0000001D0000000000000000000000000000
          000000000001000000050000000E0000001800000020000000240000002A0000
          003900550FFF40D077FF00550FFF0000003E0000001D00000009000000000000
          000100000009030710380A193C94112962D216347EF3153278F5005E10FF005D
          10FF005D0FFF41D179FF005D10FF005E10FF015D10FF0000001D000000000000
          0008050D1F5410285EC82C68AFFF3E90D0FF4BADEAFF47A4DDFF016610FF43D4
          7EFF43D47EFF44D47DFF43D47EFF43D47EFF006610FF00000023000000030307
          103111285FC43279BEFF4AB7F3FF4AB7F3FF4AB7F3FF48B1ECFF016F11FF016F
          10FF017011FF47D781FF016F10FF016F11FF016F10FF00000019000000070B1A
          3D862967B0FF44B3F2FF44B3F3FF43B3F3FF43B5F3FF44B2F1FF42ADEAFF3DA4
          DDFF017611FF48DB86FF017711FF0000003900000015000000060000000A132D
          65C4318AD1FF3CB0F2FF3CAFF2FF3CAFF2FF3CAFF2FF3CAFF2FF3CAFF2FF3AAA
          EAFF017C11FF017C11FF017C11FF0000002900000000000000000000000B1B3B
          83EE33A2E8FF35ADF2FF4AB7F1FF53C1F7FF5EC9F9FF5EC9F9FF53C1F7FF49B6
          EFFF34A6E9FF3099DBFF153277F3000000230000000000000000000000091D3D
          85ED2D9EE8FF55BBF3FF81D7FBFF82D6FAFF82D7FBFF82D6FBFF82D6FAFF81D7
          FBFF55BBF3FF2E9DE7FF16347EF30000001E0000000000000000000000061732
          69BF2786D2FF8DD6FAFFA6E2FCFFA6E2FCFFA6E2FCFFA6E2FCFFA6E2FCFFA6E2
          FCFF8DD6FAFF2381CFFF112962CF000000160000000000000000000000030F20
          4179236AB7FFA9DFFBFFCAEEFDFFCAEEFDFFCAEEFEFFCAEEFDFFCAEEFEFFCBED
          FDFFA9DFFBFF1D5FAEFF0B193C910000000C0000000000000000000000010408
          1122193568B64F89C7FFE7F7FEFFE9F8FEFFEAF8FEFFE9F8FEFFE9F8FEFFE8F7
          FEFF4981C1FF10275FC503061035000000040000000000000000000000000000
          00010811223E1A366AB64176BAFF85ADD9FFD5E4F3FFD5E4F3FF83AAD7FF3B6D
          B3FF122A60C1050D1F5000000008000000000000000000000000000000000000
          000000000001040911211021427719356BBD204088ED1D3E85ED152E67C20C1B
          3D810307102D0000000600000001000000000000000000000000000000000000
          0000000000000000000000000002000000040000000700000008000000080000
          0005000000020000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000898989FF878787FF858585FF848484FF828282FF8181
          81FF7F7F7FFF7E7E7EFF00000000000000000000000000000000000000000000
          000000000000000000008B8B8BFFE0E0E0FFE0E0E0FFDFDFDFFFDFDFDFFFDFDF
          DFFFDEDEDEFF808080FF00000000000000000000000000000000000000000000
          000000000000000000008E8E8EFFE1E1E1FFE1E1E1FFE1E1E1FFE0E0E0FFE0E0
          E0FFDFDFDFFF838383FF00000000000000000000000000000000000000009797
          97FF959595FF939393FF2F2F2FFF2A2A2AFF262626FF212121FF1D1D1DFF1919
          19FF151515FF111111FF838383FF828282FF808080FF00000000000000009A9A
          9AFFE5E5E5FFE5E5E5FF373737FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF171717FFE0E0E0FFE0E0E0FF838383FF00000000000000009D9D
          9DFFE6E6E6FFE6E6E6FF404040FFFFFFFFFFFFFFFFFFDA7C29FFD57824FFFFFF
          FFFFFFFFFFFF1E1E1EFFE1E1E1FFE1E1E1FF868686FF00000000000000009F9F
          9FFFE7E7E7FFE7E7E7FF484848FFFFFFFFFFFFFFFFFFE0822EFFDB7D29FFFFFF
          FFFFFFFFFFFF262626FFE2E2E2FFE2E2E2FF888888FF0000000000000000A2A2
          A2FFE9E9E9FFE8E8E8FF505050FFFFFFFFFFFFFFFFFFE68732FFE0822EFFFFFF
          FFFFFFFFFFFF2D2D2DFFE4E4E4FFE3E3E3FF8B8B8BFF0000000000000000A5A5
          A5FFEAEAEAFFE9E9E9FF595959FFFFFFFFFFF0903BFFEB8C37FFE68833FFE183
          2FFFFFFFFFFF353535FFE5E5E5FFE4E4E4FF8E8E8EFF0000000000000000A7A7
          A7FFEBEBEBFFEBEBEBFF616161FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF3D3D3DFFE6E6E6FFE5E5E5FF919191FF0000000000000000AAAA
          AAFFA8A8A8FFA7A7A7FF6A6A6AFF656565FF5F5F5FFF5A5A5AFF555555FF5050
          50FF4B4B4BFF464646FF989898FF969696FF949494FF00000000000000000000
          00000000000000000000A8A8A8FFECECECFFEBEBEBFFEBEBEBFFEAEAEAFFEAEA
          EAFFE9E9E9FF9C9C9CFF00000000000000000000000000000000000000000000
          00000000000000000000AAAAAAFFEDEDEDFFEDEDEDFFECECECFFECECECFFEBEB
          EBFFEBEBEBFF9F9F9FFF00000000000000000000000000000000000000000000
          00000000000000000000ACACACFFABABABFFAAAAAAFFA8A8A8FFA7A7A7FFA5A5
          A5FFA4A4A4FFA2A2A2FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000F0F
          0FFF0C0C0CFF0A0A0AFF00000000000000000000000000000000000000000000
          00000000000000000000000000FF000000FF000000FF00000000000000001414
          14FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FF00000000000000001A1A
          1AFF00000000C8702EFFC46C29FFC06726FFBD6422FFB9601FFFB85F1DFFB65D
          1CFF00000000000000000000000000000000000000FF00000000000000000000
          000000000000D07934FFFFD0A1FFFFCE9CFFFFCB97FFFFC892FFFFC58DFFB960
          1EFF000000000000000000000000000000000000000000000000000000000000
          000000000000D7813CFFFFD7B0FFFFC78EFFFFC387FFFFBF80FFFFCD9BFFBF67
          24FF000000000000000000000000000000000000000000000000000000000000
          000000000000DE8943FFFFDEBDFFFFD1A0FFFFCD99FFFFC993FFFFD4A9FFC56D
          2BFF000000000000000000000000000000000000000000000000000000000000
          000000000000E6914AFFFFE5C9FFFFD9B2FFFFD6ABFFFFD2A5FFFFDBB6FFCD76
          33FF049530FF049330FF04922FFF000000000000000000000000000000000000
          000000000000EC9950FFFFEBD4FFFFE9D1FFFFE6CCFFFFE5C8FFFFE2C4FFD57E
          3AFF7EEBB4FF73E9ADFF049430FF000000000000000000000000000000000000
          000000000000F19F55FFEE9C52FFEB984FFFE8944CFFE49049FFE08B44FFDD87
          41FF6DE8ACFF85ECB9FF059833FF000000000000000000000000000000000000
          000000000000000000000000000000000000000000001BB753FFBEF6DEFF99F0
          C8FF88EDBDFF99EFC5FF0CA13CFF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000025C061FFCEF9E9FFC4F8
          E2FFB9F5DAFFADF3D2FF13AB47FF000000000000000000000000000000005B5B
          5BFF00000000000000000000000000000000000000002FC96EFF2BC669FF27C2
          64FF23BE5FFF20BA59FF1CB654FF000000002C2C2CFF00000000000000006464
          64FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000323232FF00000000000000006C6C
          6CFF686868FF636363FF00000000000000000000000000000000000000000000
          000000000000000000003D3D3DFF3B3B3BFF383838FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000C870
          2EFFC46C29FFC06726FFBD6422FFB9601FFFB85F1DFFB65D1CFF000000000000
          000000000000000000000909B2FF00000000000000000000000000000000D079
          34FFFFD0A1FFFFCE9CFFFFCB97FFFFC892FFFFC58DFFB9601EFF000000000000
          000000000000000000000909B3FF00000000000000000000000000000000D781
          3CFFFFD7B0FFFFC78EFFFFC387FFFFBF80FFFFCD9BFFBF6724FF000000000000
          000000000000000000000A0AB5FF0A0AB4FF0909B4FF0000000000000000DE89
          43FFFFDEBDFFFFD1A0FFFFCD99FFFFC993FFFFD4A9FFC56D2BFF000000000000
          000000000000000000000000000000000000000000000000000000000000E691
          4AFFFFE5C9FFFFD9B2FFFFD6ABFFFFD2A5FFFFDBB6FFCD7633FF000000000000
          000000000000000000000000000000000000000000000000000000000000EC99
          50FFFFEBD4FFFFE9D1FFFFE6CCFFFFE5C8FFFFE2C4FFD57E3AFF000000000000
          000000000000000000000000000000000000000000000000000000000000F19F
          55FFEE9C52FFEB984FFFE8944CFFE49049FFE08B44FFDD8741FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000069C
          33FF049931FF049731FF049530FF049330FF04922FFF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000BA4
          3BFF98EFC5FF8CEDBCFF7EEBB4FF73E9ADFF049430FF00000000000000000000
          00000000000000000000000000000000000000000000000000000000000012AC
          46FFADF3D2FF7FEBB8FF6DE8ACFF85ECB9FF059833FF00000000000000001010
          D4FF0F0FD3FF0F0FD2FF00000000000000000000000000000000000000001BB7
          53FFBEF6DEFF99F0C8FF88EDBDFF99EFC5FF0CA13CFF00000000000000000000
          0000000000001010D4FF000000000000000000000000000000000000000025C0
          61FFCEF9E9FFC4F8E2FFB9F5DAFFADF3D2FF13AB47FF00000000000000000000
          0000000000001010D6FF00000000000000000000000000000000000000002FC9
          6EFF2BC669FF27C264FF23BE5FFF20BA59FF1CB654FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00030000000E0000001500000012000000060000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000B004D0FFF004D0FFF004D0FFF000000130000000000000000000000060000
          000A0000000B0000000B0000000B0000000B0000000B0000000C000000120000
          001F00550FFF4ADC87FF00550FFF000000270000001300000006635852BD8A7B
          73FF8A7C72FF8A7B72FF897B72FF897B72FF897B72FF867870FF005E10FF005D
          10FF005D0FFF4BDC89FF005D10FF005E10FF015D10FF000000138B7E76FFF8EF
          E6FFF2DFCDFFF2DFCCFFF3DFCCFFF3DFCBFFF2DFCBFFEEDCC8FF016610FF4DDF
          8EFF4DDF8EFF4FDF8DFF4DDF8EFF4DDF8EFF006610FF000000168D8078FFF9F1
          E8FF26488FFF264990FF6888BAFFA8C5E4FF6585B8FF234189FF016F11FF016F
          10FF017011FF52E191FF016F10FF016F11FF016F10FF0000000F90827AFFFAF2
          EAFF2E579EFF31599FFF7899C8FFBDD9F0FF7797C6FF2B5097FF284992FF2242
          87FF017611FF53E596FF017711FF0000001E0000000B0000000491857DFFFBF4
          EDFF3865AAFF3C67ACFF86A8D2FFCCE6F8FF85A6D0FF375FA5FF30589FFF2B50
          96FF017C11FF017C11FF017C11FF000000100000000000000000938780FFFCF5
          EFFF4AB8F4FF52BBF5FF69C5F6FF73C8F6FF68C3F6FF4FBAF3FF45B4F3FF3EB0
          F2FF37A9EFFFF1DFCEFF8E827AFF0000000A0000000000000000968983FFFCF6
          F1FF52BDF5FF6DC7F7FFB1E1FBFFE0F2FDFFB0E1FBFF69C5F6FF4BB9F4FF42B4
          F4FF3EAEF2FFF5E4D3FF93867FFF000000080000000000000000988C86FFFCF7
          F3FF57C2F7FF78CEF9FFDBF2FDFFFFFFFFFFDBF2FDFF76CCF8FF50BDF6FF49B9
          F5FF43B3F4FFF5E6D5FF958982FF0000000700000000000000009A8E89FFFDF9
          F5FF5BC5F8FF74CFF8FFB1E4FBFFDFF3FDFFB0E3FBFF6FCCF8FF54C1F6FF4EBC
          F5FF48B7F5FFF6E6D7FF978B85FF0000000700000000000000009C918BFFFDF9
          F6FF5BC7F9FF62CAF9FF74D1FAFF7CD3F9FF74CFF9FF5EC7F8FF56C3F8FF51BF
          F6FF4BBAF5FFF7E8D9FF998E88FF0000000600000000000000009F938EFFFEFA
          F7FFFDFAF7FFFDFAF7FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9F5FFFDF8F4FFFDF8
          F4FFFCF8F3FFFCF8F3FF9C908BFF00000006000000000000000086807DC0B5AC
          A9FFB5ACA8FFB4ACA8FFB4ABA8FFB5ACA7FFB4ABA6FFB4ABA7FFB4ABA6FFB4AB
          A6FFB4ABA6FFB4ABA6FF857E7BC0000000030000000000000000000000020000
          0004000000040000000400000004000000040000000400000004000000050000
          0005000000050000000400000003000000010000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000001A120D811A120D810000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000001B120D813A281EFF39271EFF1A12
          0D81000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000001C140F813C2A20FF402D23FF3F2C22FF3A28
          20FF1B120D810000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000423025FF413024FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001F171281000000000000000000000000443227FF433226FF0000
          000000000000000000001B140F81000000000000000000000000000000000000
          000021181381443327FF000000000000000000000000483429FF463428FF0000
          000000000000000000003F2D22FF1C151081000000000000000000000000221A
          148148372BFF4D3B2EFF423126FF413026FF412F24FF4A372BFF49372AFF3F2E
          23FF3E2D23FF3E2D22FF463428FF402F24FF1D1511810000000000000000251D
          177E513D32FF4F3D31FF4E3D31FF4E3B30FF4D3B30FF4D3A2FFF4C392DFF4C38
          2CFF4B372CFF4A362BFF49362BFF49352AFF2118127E00000000000000000000
          00002A201A7E533F34FF000000000000000000000000503C31FF4F3C30FF0000
          000000000000000000004C382DFF251C167E0000000000000000000000000000
          0000000000002A211B7E000000000000000000000000523F33FF513E32FF0000
          00000000000000000000271E187E000000000000000000000000000000000000
          00000000000000000000000000000000000000000000564236FF554135FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000271E187E4E3B31FF584438FF584338FF4B39
          2FFF251C177E0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000002E241E7E5B483CFF5B473AFF2C23
          1D7E000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000002E251F7E2E251E7E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000BEBEBEC0B8B8
          B8C1B7B7B7C2B8B8B8C1BBBBBBC0B8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B6B6
          B6C2B6B6B6C2B6B6B6C2B7B7B7C2AEAEAEB68585858928282829BBBBBBC14C30
          27FF4B3027FF4B3027FFB6B6B6C24B3026FF4B3026FF4B2F26FF4A2F26FF4A30
          26FF4A2F26FF4A2F26FF50352DFC66534CEE9D9895D0AFAFAFB3BDBDBDC0AEAC
          ABC76A5248F8A5A2A1CAB5B5B5C3A29E9DCD60463BFE644B40FCA09B99D05F44
          3AFF60453AFEAEADADC5958D8AD64F342AFF51372FFBACACACB410101010A6A6
          A6AA897D7ADC4C3128FF4F342BFF61463CFF60453AFF7B6D69E4B5B5B5C36045
          3AFF4C3127FFB2B1B1C598908CD55E4338FF584239F7ADADADB4000000005151
          5152B6B5B4C47F6D66EBB2B2B2C3705A52F4593E34FEABA8A7C9B8B8B8C16146
          3BFF4C3128FF52372DFF63493EFD81736DE5ABA8A7C9A3A3A3A5000000000909
          09097D7D7D807A6C66DF958C87D963483DFF73615BEAB9B9B9BEBABABAC16247
          3CFF674D42FCA29E9BCE583D32FF796A65E5BBBBBBC016161616000000000000
          000067676768A7A4A3C0694D44FC5C4137FFA59F9ECDBDBDBDC0ADAAA8CA6248
          3CFF573D34FCA29D9AD0593E33FF4C3128FFB7B6B5C207070707000000000000
          000016161616B4B4B4B782706AEA6C5850F0BDBDBDC0BDBDBDC0654A3FFF6349
          3EFF52372DFF664C41FE745E55F3887C78DDBDBDBDC007070707000000000000
          0000000000007E7E7E7E8F8F8F918F8F8F918F8F8F90BEBEBEBFBDBDBDC0BCBC
          BCC0BCBCBCC0BCBCBCC0B9B9B9BC9898989A5555555500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
  object ilItems: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 23593072
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000100B051D3E29127065431DB7815526EA815526EA6543
          1DB73E291270100B051D00000000000000000000000000000000000000000000
          0000000000002015093962401CB0AD6F36FFC87F41FFDC8B49FFDC8B49FFC87F
          41FFAD7035FF62401CB020150939000000000000000000000000000000000000
          0000100A051D63401BB0B77538FFE08C49FFE08C49FFE08C4AFFE08B49FFE08C
          4AFFE08B4AFFB87438FF63401CB0100B051D0000000000000000000000000000
          00003F281170AB6B31FFDB8545FFDB8645FFDB8544FFDA8644FFDB8645FFDB86
          44FFDB8644FFDA8645FFAB6B31FF402911700000000000000000000000000000
          000069421AB7BF7336FFD67F40FFD57F40FFD67F40FFD67F40FFD57F3FFFD67F
          40FFD57F3FFFD57F3FFFBF7336FF69421BB70000000000000000000000000000
          0000885420EACA7538FFCF783AFFD78348FFDE8C54FFE3935DFFE3935DFFDE8C
          54FFD78348FFD0783AFFCA7538FF885421EA0000000000000000000000000000
          000089541FEAC46F32FFD78751FFE9A678FFE9A678FFE9A578FFE9A679FFE9A6
          78FFE9A577FFD78751FFC46F33FF8A541FEA0000000000000000000000000000
          00006C4117B7B5662AFFE6AC86FFEFBD9BFFEFBD9BFFEFBC9AFFEFBD9BFFEEBC
          9BFFEFBC9BFFE6AC85FFB5652AFF6D4118B70000000000000000000000000000
          000043280E70A75E23FFEABFA3FFF5D6C1FFF6D6C1FFF5D6C1FFF6D6C1FFF5D6
          C1FFF5D6C1FFEABFA3FFA75E23FF43280E700000000000000000000000000000
          0000120A031D6B3F15B0B97B4BFFFAECE3FFFBEDE4FFFBEDE4FFFBEDE3FFFBED
          E4FFFAECE3FFB97B4AFF6B3E15B0120A031D0000000000000000000000000000
          000000000000231406396C3E14B0B06F3AFFCFA381FFEFDFD2FFEFDED2FFCFA3
          81FFAF6F3AFF6C3E13B023140639000000000000000000000000000000000000
          00000000000000000000120A031D46280C70724013B7915319EA915219EA7241
          13B746280C70120A031D00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000030000
          000A0000000D0000000E0000000F0000000D00000004000000020000000B0000
          0012000000150000001600000017000000160000000E00000003160E0729A668
          31FFA6672FFFA5652EFFA4642CFFA3622AFF0000000900000006603917A39F5E
          25FF9F5D23FF9E5D22FF9E5C22FF9D5B21FF5E3613A800000008000000030000
          000B643E1CA5AF6D36FF160D06390000000C0000000300000002000000090000
          001C8B5120E6DC8E52FFBF763CFF4A2A10920000001200000003000000000000
          00010B07031DA26531F121140941000000030000000000000000000000010000
          0012955923F2D3874CFF744419CA000000140000000200000000000000000000
          000000000005643F1EA14E301781000000060000000000000000000000053621
          0D68B97339FFBD753BFF40250F7C000000080000000000000000000000000000
          0000000000022D1C0E4D865428D00000000F0000000B0000000C000000136D42
          1CB8D1864CFF9C5D28F30B060224000000020000000000000000000000000000
          00000000000000000006925C2DE0A86932FFA76730FFA6662FFFA4652DFFD289
          4FFFC88146FF5634169900000009000000000000000000000000000000000000
          000000000000000000035032197E59381C910000000D0000000E4D301588C57F
          46FFAD6B33FF160D053300000003000000000000000000000000000000000000
          00000000000000000001170F07289E6533EF0000000800000009905A29E1D890
          55FF784A20C40000000A00000000000000000000000000000000000000000000
          000000000000000000000000000488572CCF3824125D2D1D0E4FBA783FFFC27E
          45FF422913720000000400000000000000000000000000000000000000000000
          000000000000000000000000000251341A7B94633ABF8A5B33BAD18C53FF9D62
          2EF00000000A0000000100000000000000000000000000000000000000000000
          00000000000000000000000000000C070415A56C38F0D38F56FFCE8A51FF5A38
          1B90000000040000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000027E522ABDAB6F38FFAA6C36FF170E
          0728000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000200000003000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000100000
          0016000000180000001A0000001B0000001D0000001F00000020000000220000
          002300000025000000260000002800000029000000210000000A968A83FF9588
          82FF94877FFF92857DFF90837AFF8F8178FF8D7F76FF8B7D74FF897B72FF887A
          70FF86776EFF84756BFF83746AFF817167FF7F6F64FF00000021988C84FFEDE3
          D7FFE4D4C3FFE4D2C1FFE2D1BFFFE0D0BCFFDFCEBBFFDECCB9FFDDCAB6FFDBC8
          B4FFDBC7B1FFD9C5AFFFD8C3ADFFD7C1AAFF817167FF000000299A8E86FFFAF1
          E7FFF7E9D8FFF7E7D6FFF6E6D4FFF6E5D2FFF4E2D0FFF3E1CEFFF3DFCCFFF1DD
          C9FFF1DCC6FFF0DBC4FFEFD9C3FFD8C3ADFF82736AFF000000289B8F89FFFBF2
          E9FFF9EADAFFF8E8D8FFF7E6D6FFF6E5D3FFF6E5D2FFF5E3D0FFF4E1CDFFF3DF
          CBFFF2DEC9FFF0DCC7FFEFDAC4FFD9C5AFFF84756CFF000000269D918BFFFCF4
          EBFFF9EBDBFFF8E9DAFFF7E8D8FFF7E7D6FFF6E6D4FFF5E4D3FFF5E3D1FFF3E1
          CEFFF2DFCCFFF2DEC9FFF1DCC6FFDBC7B1FF86776DFF000000259E938DFFFCF5
          EEFFFAECDDFFF9EADCFFF9EADAFFF8E8D8FFF7E7D6FFF7E6D4FFF6E4D3FFF5E3
          D0FFF4E2CEFFF3E0CBFFF1DEC9FFDCC8B3FF88786FFF00000023A0958EFFFDF7
          F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFDDCAB6FF8B7C72FF00000022A19690FFFDF8
          F2FF7A644AFF7B674CFF7E684FFF806B50FF816D54FF847059FF87745AFF8975
          5DFF8C7860FF8D7A61FF8E7B64FFDECCB9FF8C7E74FF00000020A29792FFFDF9
          F4FFFBEFE1FFFAEEE0FFFAEDDFFFF9EDDEFFF9EBDCFFF9E9DAFFF7E9D9FFF7E7
          D7FFF6E6D5FFF5E5D2FFF4E3D0FFE0CEBBFF8D8077FF0000001FA49994FFFEF9
          F5FFFBEFE2FFFBEFE2FFFAEEE1FFFAEDE0FFF9ECDEFFF9ECDCFFF8EADAFFF8E8
          D8FFF6E7D6FFF7E6D3FFF6E4D1FFE1CFBCFF8F827AFF0000001DA49B94FFFEFA
          F6FFFBEFE2FFFBEFE2FFFBEFE1FFFAEEE0FFFAEDDFFFF9ECDDFFF9EBDBFFF8E9
          DAFFF7E9D8FFF6E7D6FFF6E5D3FFE2D1BFFF91847CFF0000001BA59B96FFFEFB
          F8FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE1FFFAEEE1FFFAEDDFFFFAECDDFFF9EB
          DCFFF8EAD9FFF7E8D8FFF7E7D5FFE3D2C1FF93867EFF0000001AA69C97FFFEFC
          F9FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE2FFFBEEE1FFFAEDDFFFFAEC
          DDFFF8EBDCFFF8EADAFFF7E8D8FFE5D5C4FF958881FF00000018A79D98FFFEFD
          FBFFFEFCFAFFFEFBF9FFFEFBF7FFFEFAF7FFFEFAF6FFFEF9F5FFFDF8F3FFFDF7
          F1FFFDF6F0FFFCF5EEFFFBF4EBFFF1E8DFFF968982FF00000016A79D98FFA79D
          98FFA69C98FFA59B96FFA59A94FFA49A94FFA39892FFA29791FFA19690FF9F94
          8EFF9D928CFF9C908AFF9B8F89FF998E87FF978C84FF00000010}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000100000
          0016000000180000001A0000001B0000001D0000001F00000020000000220000
          002300000025000000260000002800000029000000210000000A968A83FF9588
          82FF94877FFF92857DFF90837AFF8F8178FF8D7F76FF8B7D74FF897B72FF887A
          70FF86776EFF84756BFF83746AFF817167FF7F6F64FF00000021988C84FFEDE3
          D7FFE4D4C3FFE4D2C1FFE2D1BFFFE0D0BCFFDFCEBBFFDECCB9FFDDCAB6FFDBC8
          B4FFDBC7B1FFD9C5AFFFD8C3ADFFD7C1AAFF817167FF000000299A8E86FFFAF1
          E7FFF7E9D8FFF7E7D6FFF6E6D4FFF6E5D2FFF4E2D0FF7A644AFFFFFFFFFFF1DD
          C9FFF1DCC6FFF0DBC4FFEFD9C3FFD8C3ADFF82736AFF000000289B8F89FFFBF2
          E9FFF9EADAFFF8E8D8FFF7E6D6FFF6E5D3FFF6E5D2FF7B674CFFFFFFFFFFF3DF
          CBFFF2DEC9FFF0DCC7FFEFDAC4FFD9C5AFFF84756CFF000000269D918BFFFCF4
          EBFFF9EBDBFFF8E9DAFFF7E8D8FFF7E7D6FFF6E6D4FF7E684FFFFFFFFFFFF3E1
          CEFFF2DFCCFFF2DEC9FFF1DCC6FFDBC7B1FF86776DFF000000259E938DFFFCF5
          EEFFFAECDDFFF9EADCFFF9EADAFFF8E8D8FFF7E7D6FF806B50FFFFFFFFFFF5E3
          D0FFF4E2CEFFF3E0CBFFF1DEC9FFDCC8B3FF88786FFF00000023A0958EFFFDF7
          F0FFFAEEDFFFFAECDDFFF9EBDBFFF9E9DAFFF7E8D9FF816D54FFFFFFFFFFF6E5
          D3FFF5E3D0FFF3E2CEFFF2E0CCFFDDCAB6FF8B7C72FF00000022A19690FFFDF8
          F2FFFBEEE0FFFAEEDFFFFAECDDFFF9EBDCFFF9EADAFF847059FFFFFFFFFFF6E6
          D5FFF5E5D3FFF5E3D0FFF4E1CEFFDECCB9FF8C7E74FF00000020A29792FFFDF9
          F4FFFBEFE1FFFAEEE0FFFAEDDFFFF9EDDEFFF9EBDCFF87745AFFFFFFFFFFF7E7
          D7FFF6E6D5FFF5E5D2FFF4E3D0FFE0CEBBFF8D8077FF0000001FA49994FFFEF9
          F5FFFBEFE2FFFBEFE2FFFAEEE1FFFAEDE0FFF9ECDEFF89755DFFFFFFFFFFF8E8
          D8FFF6E7D6FFF7E6D3FFF6E4D1FFE1CFBCFF8F827AFF0000001DA49B94FFFEFA
          F6FFFBEFE2FFFBEFE2FFFBEFE1FFFAEEE0FFFAEDDFFF8C7860FFFFFFFFFFF8E9
          DAFFF7E9D8FFF6E7D6FFF6E5D3FFE2D1BFFF91847CFF0000001BA59B96FFFEFB
          F8FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE1FFFAEEE1FF8D7A61FFFFFFFFFFF9EB
          DCFFF8EAD9FFF7E8D8FFF7E7D5FFE3D2C1FF93867EFF0000001AA69C97FFFEFC
          F9FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE2FFFBEFE2FF8E7B64FFFFFFFFFFFAEC
          DDFFF8EBDCFFF8EADAFFF7E8D8FFE5D5C4FF958881FF00000018A79D98FFFEFD
          FBFFFEFCFAFFFEFBF9FFFEFBF7FFFEFAF7FFFEFAF6FFFEF9F5FFFDF8F3FFFDF7
          F1FFFDF6F0FFFCF5EEFFFBF4EBFFF1E8DFFF968982FF00000016A79D98FFA79D
          98FFA69C98FFA59B96FFA59A94FFA49A94FFA39892FFA29791FFA19690FF9F94
          8EFF9D928CFF9C908AFF9B8F89FF998E87FF978C84FF00000010}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000522F1283000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000052321681B37643FF4E2D117E0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000056371B81B87D4DFFC78F65FFB37643FF4E2D
          117E000000000000000000000000000000000000000000000000000000000000
          00000000000000000000593B2181AD713BFFAA6D36FFA76831FFA4632BFFA160
          27FF4F2D117E00000000000000000000000000000000000000000000000B0000
          0011000000120000001200000013000000140000001500000016000000180000
          00190000001A0000001C0000001D0000001D00000015000000065F544DC18677
          6DFF87796FFF887A71FF8B7C73FF8B7D75FF8D8077FF8F8179FF90837BFF9285
          7DFF94877FFF968982FF978B84FF9A8C86FF736A65CF0000001386776DFFFAFA
          F9FFEBD0B6FFEBD0B5FFCEB69BFFFCF6F0FFE9D0B6FFCEB69BFFFCF6F0FFEBD1
          B7FFCFB69BFFFCF6F0FFECD0B6FFEAD0B6FF9D918AFF0000001787786FFFFFFF
          FFFFF3DCC3FFF3DCC3FF725D44FFD3BDA4FFF2DCC3FF725D44FFD3BDA4FFF3DC
          C3FF725D44FFD3BDA4FFF3DAC4FFF3DCC4FF9E938CFF00000014897A71FFFAFA
          FAFFFAFAFAFFFBFAFAFFFBFAFAFFFBFAFAFFFBFAFAFFFBFAFAFFFBFAFAFFFBFA
          FAFFFBFBFAFFFBFBFAFFFBFBFAFFFBFBFAFF9F948EFF0000000E635953B98B7D
          75FF8E7F77FF8F8179FF91837CFF92857DFF948780FF958982FF978B84FF998C
          86FF9B8F89FF9C918AFF9E928CFF9F948EFF776F6AC200000007000000010000
          0001000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000030000000300000001000000000000
          000000000000000000005A3C2281AF743FFFAD703BFFAA6D36FFA76932FFA564
          2DFF5030147E0000000000000000000000000000000000000000000000000000
          00000000000000000000000000005A3C2281BE8657FFCC966CFFBA804FFF5333
          187E000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000005A3C2281BE8656FF55371D7E0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000005B3E2283000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000030000000F000000180000001A0000001A0000
          0012000000040000000000000000000000000000000000000000000000000000
          000000000000000000000000000D635952C6897A71FF87786FFF86776DFF5F54
          4DC9000000110000000000000000000000000000000000000000000000000000
          00000000000000000000000000118B7D75FFF6ECE1FFF2DCC3FFE9CFB5FF8677
          6DFF000000180000000000000000000000000000000000000000000000000000
          00000000000000000000000000108E7F77FFF8ECE3FFF3DCC3FFEBD0B6FF8779
          6FFF000000180000000000000000000000000000000000000000000000000000
          000000000000000000000000000E8F8179FFF7EDE4FFD4BDA4FFF9F0E8FF887A
          71FF000000150000000000000000000000000000000000000000000000000000
          000000000000502F12810000000C91837CFFF7EFE5FF725D44FFCEB69BFF8B7C
          73FF00000013502F118100000000000000000000000000000000000000000000
          000052321581A15F26FF0000000A92857DFFF8EFE6FFF2DCC3FFEBD0B6FF8B7D
          75FF00000010A05E25FF502F1281000000000000000000000000000000005536
          1A81B77C4BFFA36229FF00000009948780FFF7F0E7FFD3BDA4FFF8F0E8FF8D80
          77FF0000000EA2622AFFB47644FF502F12810000000000000000593B1F83BB82
          52FFCA9368FFA5662EFF00000008958982FFF8F1E8FF725D44FFCEB69BFF8F81
          79FF0000000CA5662EFFC78F64FFB37644FF522F128300000000000000005638
          1E7EBB8050FFA86932FF00000008978B84FFF8F1EAFFF2DCC3FFEBD0B6FF9083
          7BFF0000000AA96B34FFB77C4AFF5131157E0000000000000000000000000000
          000056381E7EAB6D36FF00000007998C86FFF8F2EBFFD4BDA4FFF9F1E9FF9285
          7DFF00000009AC6F39FF54351A7E000000000000000000000000000000000000
          00000000000055381D7E000000069B8F89FFF8F3ECFF725D44FFCFB69BFF9487
          7FFF0000000856391F7E00000000000000000000000000000000000000000000
          00000000000000000000000000069C918AFFF8F3EDFFF2DCC4FFECD0B7FF9689
          82FF000000080000000000000000000000000000000000000000000000000000
          00000000000000000000000000059E928CFFF9F4EFFFF3DAC4FFECD0B6FF978B
          84FF000000070000000000000000000000000000000000000000000000000000
          00000000000000000000000000049F948EFFF9F4EFFFFCF6F0FFF6EEE7FF9A8C
          86FF000000060000000000000000000000000000000000000000000000000000
          0000000000000000000000000002766F6AC09F948EFF9E938CFF9D918AFF736A
          65C0000000030000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000060000
          0008000000090000000A0000000B0000000C0000000D0000000E0000000E0000
          000F000000100000001100000012000000130000001000000004A69183FFA58F
          81FFA38D7FFFA28C7DFFA18B7CFFA0897AFF9E8778FF9D8676FF9B8575FF9B83
          73FF988171FF988070FF967E6FFF957D6DFF947B6BFF00000010A79284FFFFFF
          FFFFECE4DEFFECE4DDFFEBE3DDFFEBE2DBFFEBE2DAFFEAE2DAFFEAE1D9FFEAE1
          D9FFE9E0D9FFE8E0D8FFE8E0D8FFE8E0D7FF957D6DFF00000013A99486FFFFFF
          FFFFAA6D35FFA96B33FFF5F0EBFFF5EFE9FFA5652DFFA3642AFFA26329FFF3ED
          E6FFF3ECE5FF9F5F24FF9F5E23FFE8E0D8FF967F6FFF00000012A99587FFFFFF
          FFFFAB6F37FFF7F1ECFFF7F0EBFFF6F0EAFFF5EFEAFFF4EFE9FFF4EFE8FFF4ED
          E7FFF3EDE7FFF3EDE5FF9F5F24FFE9E0D8FF97806FFF00000011AB9689FFFFFF
          FFFFF8F2EEFFF7F1EDFFF7F1ECFFF6F1EBFFF5F0EBFFF5EFEAFFF5EEE8FFF4EE
          E8FFF4EDE7FFF4EDE6FFF4EDE6FFE9E0D9FF998172FF00000010AC9789FFFFFF
          FFFFF8F3EFFFF7F2EEFFF7F1EDFFF7F1ECFFF6F0EBFFF5EFEAFFF5EFEAFFF5EF
          E8FFF5EFE7FFF4EEE7FFF4EDE6FFE9E1D9FF9A8373FF0000000FAD988BFFFFFF
          FFFFAF743EFFF8F2EEFFF7F2EDFFF7F1ECFFF6F1ECFFF6F0EBFFF6F0EBFFF6EF
          E9FFF4EEE8FFF4EFE8FFA26229FFEAE1D9FF9C8475FF0000000EAE998DFFFFFF
          FFFFB07640FFF8F3EFFFF8F3EEFFF7F2EDFFF7F2ECFFF7F1ECFFF6F0EBFFF6F0
          EAFFF5F0E9FFF5EFE9FFA3642BFFEAE2DAFF9D8676FF0000000EAE9A8EFFFFFF
          FFFFB27742FFF8F4F0FFF9F4F0FFF8F2EFFFF7F2EEFFF7F2EDFFF7F1ECFFF6F0
          EBFFF6F0EBFFF5EFE9FFA5662CFFEAE2DAFF9E8878FF0000000DB09B8EFFFFFF
          FFFFF9F4F2FFF9F4F1FFF9F4F0FFF8F4EFFFF8F3EEFFF8F2EEFFF7F1EDFFF7F1
          ECFFF6F1ECFFF5F0EBFFF5EFEAFFEBE2DBFF9F897AFF0000000CB09B8FFFFFFF
          FFFFFAF5F2FFF9F5F2FFF9F4F1FFF8F4F1FFF8F4EFFFF8F3EFFFF7F2EEFFF7F2
          EDFFF7F1ECFFF6F1EBFFF6F0EAFFEBE3DDFFA08A7BFF0000000BB09C8FFFFFFF
          FFFFB47C48FFFAF5F2FFFAF5F1FFF9F4F1FFF9F4F1FFF8F3EFFFF8F3EFFFF8F2
          EEFFF7F2EDFFF6F1ECFFA86B32FFECE4DDFFA28C7DFF0000000AB09B8FFFFFFF
          FFFFB67D4AFFB57C48FFFAF5F2FFF9F5F1FFB27743FFB07641FFAF743EFFF7F2
          EFFFF7F2EEFFAB6F38FFA96D35FFECE5DEFFA48E7FFF00000009B09C8FFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA58F81FF00000008B09C8FFFB09C
          8FFFB09C8EFFB09C8FFFB09C8EFFB09B8EFFAF9B8DFFAE998DFFAD998BFFAC97
          8AFFAB9689FFAA9487FFA89485FFA89284FFA79183FF00000006}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          00010120075C023A0DAB034E11E6034C11E602380DAB021E075D301D09695D37
          12AC7B4A18E6794A18E6583512AB2F1D095C010000010000000000000001022F
          0A80167326FF2F943FFF44B054FF44AF54FF2F923FFF166D26FF42531EFFBB84
          53FFE8AC7AFFEAAF7CFFCA905CFFA06935FF41280E80010000010123065C1677
          26FF32A544FF29AF3DFF18A92EFF18A92EFF29AF3DFF32A344FF166D26FF7D72
          3CFFE7975BFFF09F60FFF1A36AFFD99762FF9D6634FF2F1D0A5C02430CAB2590
          35FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF1BA22EFF258B35FF3959
          1FFFD6874EFFE79153FFE68F52FFE39156FFBE7E4BFF593612AB035C10E62295
          30FF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0D911CFF22932FFF1254
          16FFCB7942FFDC884BFFDD8648FFDA8046FFD0824DFF794919E6035E10E61186
          1CFF1B942CFF34AA4DFF48BD68FF48BD68FF34AB4DFF1C952CFF11821CFF1256
          17FFD28B5DFFE69965FFE08E59FFD78049FFC8743CFF7B4A18E602470CAB0772
          11FF64B774FF8DD1A0FF8DD2A0FF8DD2A0FF8DD1A0FF65B774FF076C11FF3B67
          32FFDDAE92FFEBBB9DFFEBBB9DFFE1A37DFFB0622BFF5D3812AB0127065C046C
          11FF8AC091FFC4E3CAFFC4E3CAFFC4E3CAFFC4E3CAFF89BF91FF046111FF8093
          72FFE9D2C5FFF2DACCFFF3DACCFFE0B59AFF9D5A20FF341F0A5C000000010236
          0980398B43FF88BA8DFFC7E0C9FFC7DFCAFF88B98DFF398544FF496128FFCAA7
          8DFFEEDACDFFF0DCCFFFD9B398FFB47F4FFF492C0E8001000001000000000000
          00010127075C02470CAB035E11E6035C10E602430DAB0223075D35210969693F
          14AC8D5519E68B551AE6663F13AB36210A5C0100000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000010120075C023A0DAB034E11E6034C11E60238
          0DAB011E075C0000000100000000000000000000000000000000000000000000
          00000000000000000001022F0A80167326FF2F943FFF44B054FF44AF54FF2F92
          3FFF166D26FF022A0A8000000001000000000000000000000000000000000000
          0000000000000123065C167726FF32A544FF29AF3DFF18A92EFF18A92EFF29AF
          3DFF32A344FF166D26FF011E075C000000000000000000000000000000000000
          00000000000002430CAB259035FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E
          23FF1BA22EFF258B35FF02380DAB000000000000000000000000000000000000
          000000000000035C10E6229530FF0D931DFF0D931CFF0D911CFF0D931CFF0D91
          1CFF0D911CFF22932FFF044C11E6000000000000000000000000000000000000
          000000000000035E10E611861CFF1B942CFF34AA4DFF48BD68FF48BD68FF34AB
          4DFF1C952CFF11821CFF034D11E6000000000000000000000000000000000000
          00000000000002470CAB077211FF64B774FF8DD1A0FF8DD2A0FF8DD2A0FF8DD1
          A0FF65B774FF076C11FF023A0DAB000000000000000000000000000000000000
          0000000000000227075D046C11FF8AC091FFC4E3CAFFC4E3CAFFC4E3CAFFC4E3
          CAFF89BF91FF046111FF0120075D000000000000000000000000000000000000
          00000000000036220A5D43601CFF398B43FF88BA8DFFC7E0C9FFC7DFCAFF88B9
          8DFF398544FF3F571EFF2F1D0A5D000000000000000000000000000000000000
          000000000000663E13ABB37543FF766F31FF39691FFF126717FF126616FF3A66
          21FF796F34FFB07546FF593612AB000000000000000000000000000000000000
          0000000000008B551AE6CB7E49FFCD7740FFCC7942FFCB7942FFCA7D45FFCF7E
          43FFD27B44FFCE814CFF794919E6000000000000000000000000000000000000
          0000000000008D5519E6C67037FFD37A45FFDE8D57FFE59865FFE69965FFE08E
          59FFD78049FFC8743CFF7B4A18E6000000000000000000000000000000000000
          000000000000694013ABB46329FFE0A17BFFEBBB9BFFECBA9CFFEBBB9DFFEBBB
          9DFFE1A37DFFB0622BFF5D3812AB000000000000000000000000000000000000
          00000000000039230A5CA66021FFE0B59AFFF2DACCFFF2DACCFFF2DACCFFF3DA
          CCFFE0B59AFF9D5A20FF341F0A5C000000000000000000000000000000000000
          000000000000010000014F2F0F80B9824FFFDAB498FFF0DCCFFFF0DCCFFFD9B3
          98FFB47F4FFF492C0E8001000001000000000000000000000000000000000000
          000000000000000000000100000139220A5C694013AB8D5519E68B551AE6663F
          13AB36210A5C0100000100000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000301F10489A62
          30E7A86A33FFA66731FDA5662FFFA5642CFFA3632AFFA26229FFA15F27FFA05F
          25FF9F5E23FF9E5C22FF9D5B21FF9D5A20FF96571FF32D1A0A489B6434E4DCA4
          75FFE7A065FFE59D62FFE39A5FFFE0975CFFDE9559FFDD9257FFDA8F54FFD88C
          51FFD78A4FFFD5884CFFD38649FFD18548FFC2773BFF8D511EE4AD723CFFF6C1
          96FFF3AB71FFF1A96EFFEFA66BFFEEA469FFECA166FFEB9F64FFE99D61FFE89A
          5FFFE6985CFFE5965AFFE49458FFDE8F52FFD18447FF9D5A20FFAF733EFFF8C5
          9BFFF4AD73FFF3AA70FFF0A86EFFF0A56BFFEEA368FFECA166FFEB9E63FFE99C
          61FFE89A5FFFE6985CFFE5955AFFE39458FFD38549FF9D5B21FFB17641FFF9C8
          9EFFF5AE74FFF4AC72FFF2AA70FFF1A86EFFEFA66BFFEEA368FFECA165FFEB9E
          63FFE99D60FFE7995EFFE6975CFFE49659FFD4874CFF9E5C22FFB27842FFFACB
          A3FFF6B077FFF5AF74FFF3AC72FFF2AA6FFFF1A86DFFEFA56AFFEDA368FFECA0
          65FFEB9F62FFE99B60FFE79A5EFFE6975BFFD6894EFF9F5D23FFB37944FFFBCC
          A5FFF7B279FFF6B076FFF5AF74FFF4AC72FFF2A970FFF0A76DFFEFA46AFFEEA3
          68FFECA066FFEA9E62FFE99C60FFE7995DFFD88B50FFA05E25FFB47B47FFFCCF
          A8FFF8B47BFFF7B278FFF6B076FFF4AE74FFF3AB71FFF2A96FFFF0A76CFFEFA4
          6AFFEDA367FFEBA065FFEA9D62FFE99B60FFDA8E52FFA15F27FFB57C48FFFCD0
          ACFFFAB57DFFF8B47AFFF8B278FFF6B076FFF4AE73FFF6C399FFF5C095FFF4BD
          92FFF3BB8EFFF1B78AFFEFB485FFEEB282FFD29463FF935925E7B67D4AFFFDD3
          AFFFFBB87EFFFAB67CFFF8B47AFFF7B278FFAF743EFFAD723CFFAD703AFFAB6E
          38FFAA6C35FFA86A33FFA76830FFA5662EFF915828E028180A3FB77F4CFFFDD5
          B1FFFCBF8BFFFBB77EFFFAB57CFFF8B37AFFB07540FFDFDEDEFFCDCDCDFFBCBE
          BEFF8A8A8AFF6A6A6AFF00000000000000000000000000000000A67446E7ECBF
          9AFFFED5B2FFFDD3B0FFFCD1ADFFE9B990FFB58050FFEFEFEFFFE7E7E7FFD6D6
          D6FFA5A5A4FF616162E7000000000000000000000000000000002D20133F9166
          3DC9B77F4CFFB67E4AFFB57C48FFA27041E86F6153D66F6F70FF6E6E6FFF6E6E
          6EFF606160E01B1B1B3F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000B1D34
          68C327488FFF27468FFF25448EFF25438CFF24428BFF192F63C3000000000000
          0000000000000000000000000000000000000000000000000000000000112A4A
          92FFC7E6FAFF6AB9EEFF64B3EAFF5EACE5FF58A5E0FF24428BFF000000000000
          0000000000000000000000000000000000000000000000000000000000122A4B
          93FFD0EBFCFF71BFF1FF1E3D81FF63B3E9FF5EACE6FF25448CFF000000000000
          0000000000000000000000000000000000000000000000000000000000122B4D
          94FFD8EFFDFF77C5F5FF102B6DFF6AB8EEFF63B3EAFF25448DFF000000000000
          0002000000080000000F00000014000000120000000B00000005000000182C4E
          95FFDFF2FDFFD7EEFCFFD0EBFCFFC6E6F9FFBBDFF7FF26458EFF000000020001
          0010022A088703480FD9035713FF03450FDB012709910202002A3E250B8E3D4D
          79F72C4D95FF2B4D93FF2A4B92FF2A4A92FF284890FF1D356AC800030114024B
          0ECD167A26FF219C34FF18A92EFF139728FF0F7220FF144D12F794632FFFD29B
          6DFF534236FFDBCDBFFF49392EFFD7C6B3FF38271FFA0000000F0234098D1479
          24FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF0B6F1CFF566129FFDB93
          60FF816A5AFFA6978AFFF8E9D9FFA08F80FF4C3C31E70000000C035910E12092
          2EFF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0B871AFF175717FFCB7D
          49FFD79971FF866D5DFF584538FF7D6556FF896848EA00000013036612F91389
          1DFF1D962FFF3BB157FF4CC06DFF3CB157FF1D962FFF0B8516FF075713FFCE86
          55FFE79D6BFFE69E6DFFE1986AFFD78958FF8E5924FB00000014035C10E00875
          11FF67B977FF8DD1A0FF8DD2A0FF8DD2A0FF68B877FF087511FF1A5E1CFFDDB0
          92FFECBA9CFFEBBC9DFFE2A680FFC06932FF7C4A17E5000000100134087F046C
          11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF8EC395FF046411FF687E50FFEAD3
          C6FFF2DACCFFF2DACCFFE6BAA0FFA25D22FF452A0D8D00000008000100050251
          0EC23F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF3F8A49FF1C5613F2B07F50FFE2C2
          ACFFF3E2D9FFE3C3ADFFB98355FF704415C80201001000000002000000000001
          00050130087502540FCF036713FC02520FD0012D087C02020111452A0C767E4C
          18CF9A5D1CFC7B4B17D0442A0D7C0201000C0000000200000000000000000000
          0000000000010000000200000004000000040000000300000001000000010000
          0002000000040000000400000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000002000000080000000F000000140000001C1E3568C62748
          8FFF27468FFF25448EFF25438CFF24428BFF192F63C30000000B000000000000
          00000000000200010010022A088703480FD9035713FF3F6A48DD2A4A92FFC7E6
          FAFF6AB9EEFF64B3EAFF5EACE5FF58A5E0FF24428BFF00000011000000000000
          000100030114024B0ECD167A26FF219C34FF18A92EFF58AB65FF2A4B93FFD0EB
          FCFF71BFF1FF1E3D81FF63B3E9FF5EACE6FF25448CFF00000012000000000000
          00030234098D147924FF1BA32DFF0F9D22FF0F9E22FF56B063FF2B4D94FFD8EF
          FDFF77C5F5FF102B6DFF6AB8EEFF63B3EAFF25448DFF00000012000000000000
          0005035910E120922EFF0D931DFF0D931CFF0D911CFF55AA5EFF2C4E95FFDFF2
          FDFFD7EEFCFFD0EBFCFFC6E6F9FFBBDFF7FF26458EFF00000011000000000000
          0004036612F913891DFF1D962FFF3BB157FF4CC06DFF5CBB75FF3A648BFF2C4D
          95FF2B4D93FF2A4B92FF2A4A92FF284890FF1D356AC80000000B000000000000
          0002035C10E0087511FF67B977FF8DD1A0FF8DD2A0FF8BD09EFF7ABC88FF5041
          35FFDBCDBFFF49392EFFD7C6B3FF38271FF90000000B00000003000000000000
          00010134087F046C11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF9CCBA3FF6764
          52FFA6978AFFF8E9D9FFA08F80FF32251DC00000000000000000000000000000
          0001050501191E6213F33F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF489052FF4C6F
          3FF73F3128BF523F34F9362922B10806051B0000000000000000000000000000
          00034F300E8DAA6C32FF797A34FF2D6F1DFF056814FF2D6C1EFF787733FF9E63
          2CFF452A0E9F0000000C00000000000000000000000000000000000000000000
          000586521AE1C97D47FFD47B42FFD88045FFDA8247FFD8864AFFDA8447FFCB78
          3EFF774818E70000001300000000000000000000000000000000000000000000
          0004985C1BF9CB723AFFD47B47FFE0915CFFE69A67FFE3935EFFDA834CFFD077
          3EFF89521BFB0000001400000000000000000000000000000000000000000000
          0002895319E0BD652DFFE1A27DFFEBBB9BFFECBA9CFFEBBC9DFFE2A680FFC069
          32FF7C4A17E50000001000000000000000000000000000000000000000000000
          00014C2E0D7FA76022FFE4B89FFFF2DACCFFF2DACCFFF2DACCFFE6BAA0FFA25D
          22FF452A0D8D0000000800000000000000000000000000000000000000000000
          000002010005764716C2BC8655FFE4C3ADFFF3E2D9FFE3C3ADFFB98355FF7044
          15C8020100100000000200000000000000000000000000000000000000000000
          00000000000002010005462A0C757E4C18CF9A5D1CFC7B4B17D0442A0D7C0201
          000C000000020000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000B1D34
          68C327488FFF27468FFF25448EFF25438CFF24428BFF192F63C3000000000000
          0000000000000000000000000000000000000000000000000000000000112A4A
          92FFC7E6FAFF6AB9EEFF64B3EAFF5EACE5FF58A5E0FF24428BFF000000030000
          00070000000A0000000B0000000B0000000C0000000C0000000D0000001F2A4B
          93FFD0EBFCFF71BFF1FF1E3D81FF63B3E9FF5EACE6FF25448CFF7B4F28BBAA6C
          35FFA86A33FFA76931FFA6672EFFA4642CFFA3632BFFA26229FFA6754AFF2B4D
          94FFD8EFFDFF77C5F5FF102B6DFF6AB8EEFF63B3EAFF25448DFFAC703AFFEEB8
          8DFFE69F64FFE39B61FFE2995EFFDF965BFFDE9459FFDB9155FFCE9364FF2C4E
          95FFDFF2FDFFD7EEFCFFD0EBFCFFC6E6F9FFBBDFF7FF26458EFFAE723CFFF7C3
          98FFF3AA70FFF0A86EFFF0A56BFFEEA368FFECA166FFEB9E63FFE3A77AFF5460
          8AFF2C4D95FF2B4D93FF2A4B92FF2A4A92FF284890FF1D356AC8AF743EFFF8C5
          9BFFF4AC72FFF2AA70FFF1A86EFFEFA66BFFEEA368FFECA165FFEBAA78FFE3A7
          79FF523F32FFDBCDBFFF49392EFFD7C6B3FF38271FF90000000BB07541FFF9C7
          9FFFF5AF74FFF3AC72FFF2AA6FFFF1A86DFFEFA56AFFEDA368FFECA065FFEEAC
          78FF80614BFFA6978AFFF8E9D9FFA08F80FF32251CC300000000B17742FFF9CA
          A3FFF6B076FFF5AF74FFF4AC72FFF2A970FFF0A76DFFEFA46AFFEEA368FFEDA4
          6CFFDFA578FF86654EFF584437FF715643FF0806052800000000B37944FFFACC
          A6FFF7B278FFF6B076FFF4AE74FFF3AB71FFF2A96FFFF0A76CFFEFA46AFFEDA3
          67FFECA36AFFEDAA76FFECAA78FFB88458FF0000000D00000000B37A46FFFCCE
          A9FFF8B47AFFF8B278FFF6B076FFF4AE73FFF6C399FFF5C095FFF4BD92FFF3BB
          8EFFF1B78AFFEFB485FFEEB282FFA3632BFF0000000A00000000B57D48FFFCD1
          ACFFFAB67CFFF8B47AFFF7B278FFAF743EFFAD723CFFAD703AFFAB6E38FFAA6C
          35FFA86A33FFA76830FFA5662EFF7A4A21C20000000400000000B67D4AFFFDD2
          AFFFFBBE8AFFFAB57CFFF8B37AFFB07540FFCDCDCDFFBCBEBEFF8A8A8AFF6A6A
          6AFF000000100000000900000008000000040000000100000000B77F4BFFFED5
          B2FFFDD3B0FFFCD1ADFFFCCFAAFFB27743FFE7E7E7FFD6D6D6FFB9B8B8FF6B6B
          6CFF000000070000000000000000000000000000000000000000885F39BEB77F
          4CFFB67E4AFFB57C48FFB47B46FFA17650FF6E6E6FFF6E6E6EFF6D6E6DFF5150
          50C0000000030000000000000000000000000000000000000000000000010000
          0002000000030000000400000005000000050000000600000006000000050000
          0002000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000080000
          000C0000000D0000000D0000000D0000000D0000000D0000000D0000000D0000
          000E0000000E0000000E0000000E0000000D0000000900000002625852BF887A
          71FF887A70FF887A71FF877970FF887970FF877970FF87796FFF87786FFF8878
          6FFF87786FFF87786FFF87776FFF87786FFF615550C0000000088A7B73FFF9EE
          E3FFF2DECBFFF2DFCAFFF2DECAFFF2DECAFFF1DDCAFFF2DEC9FFF2DEC9FFF1DE
          C9FFF2DDC9FFF1DEC9FFF2DDC9FFF1DDC9FF877970FF0000000C8B7E76FFF8EF
          E6FF264990FF274A92FF6586B9FFA5C3E3FF6484B8FF25448CFF214188FF203C
          84FF1D397EFF1A3479FF183074FFF2DECAFF897A72FF0000000C8D8078FFF9F1
          E8FF2E579FFF2F5AA1FF789ACAFFBCD8F0FF7798C7FF2D539BFF2B4F96FF274A
          91FF23458BFF203F84FF1D3A7FFFF3DFCCFF8B7C74FF0000000C90827AFFFAF2
          EAFF3966ADFF3B69AEFF86A8D3FFCBE5F8FF84A6D1FF3863A8FF345DA5FF2F59
          9FFF2B5099FF264A91FF22448AFFF3E0CDFF8D7E77FF0000000B91857DFFFBF4
          EDFF4EBAF5FF52BCF5FF54BEF5FF54BDF5FF52BDF5FF4EBAF4FF48B7F4FF43B3
          F3FF3DADF2FF37A8F0FF31A1EFFFF3E1CEFF8E8179FF0000000B938780FFFCF5
          EFFF57C1F6FF5EC3F7FF72CCF8FF7ACEF8FF71CAF7FF5BC0F6FF52BDF5FF4AB8
          F5FF42B3F3FF3CADF2FF36A7F0FFF4E2D1FF90837BFF0000000A968983FFFCF6
          F1FF5EC6F7FF77CFF9FFB4E4FBFFE0F4FDFFB4E4FBFF74CCF8FF58C1F6FF50BD
          F5FF49B8F4FF41B1F3FF3BABF1FFF5E3D3FF92857EFF00000009988C86FFFCF7
          F3FF64C9F7FF83D4F9FFDDF3FDFFFFFFFFFFDDF3FDFF80D1F9FF5FC4F7FF56C1
          F6FF4EBBF5FF46B6F4FF3FB0F3FFF6E5D5FF948882FF000000099A8E89FFFDF9
          F5FF66CBF8FF7FD3FAFFB8E7FCFFE2F5FEFFB6E6FCFF7BD1F9FF62C7F7FF5AC3
          F7FF52BFF6FF4AB8F5FF43B3F4FFF6E6D6FF978B84FF000000089C918BFFFDF9
          F6FF66CCF9FF6ECFF9FF80D4FAFF88D7FAFF7ED4FAFF6CCCF9FF62C9F8FF5BC5
          F9FF53C0F7FF4CBBF6FF45B5F4FFF7E7D9FF998D87FF000000089F938EFFFEFA
          F7FFFDFAF7FFFDFAF7FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9F5FFFDF8F4FFFDF8
          F4FFFCF8F3FFFCF8F3FFFDF7F2FFFCF7F2FF9B8F8AFF0000000786807DC0B5AC
          A9FFB5ACA8FFB4ACA8FFB4ABA8FFB5ACA7FFB4ABA6FFB4ABA7FFB4ABA6FFB4AB
          A6FFB4ABA6FFB4ABA6FFB4AAA6FFB4AAA6FF857E7BC100000004000000030000
          0004000000050000000500000005000000050000000500000005000000050000
          0005000000050000000600000006000000050000000400000001}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002000000080000000F00000014000000120000000B00000005000000080000
          000F00000014000000120000000B000000030000000000000000000000020001
          0010022A088703480FD9035713FF03450FDB012709910202002A40270C897346
          16D98C531BFF6F4215DB3E260C900101001B000000060000000000030114024B
          0ECD167A26FF219C34FF18A92EFF139728FF0F7220FF144D12F7966430FFD48D
          53FFEF9D5EFFDA8E51FFAA6C34FF694016D705030122000000040234098D1479
          24FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF0B6F1CFF566129FFD784
          49FFE49053FFE79153FFE68F52FFA6672EFF452A0E9E0000000C035910E12092
          2EFF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0B871AFF175717FFCA77
          41FFDD8448FFDC884BFFDD8648FFCC783EFF774818E700000013036612F91389
          1DFF1D962FFF3BB157FF4CC06DFF3CB157FF1D962FFF0B8516FF075713FFCE86
          55FFE69A67FFE3935EFFDA834CFFD0773EFF89521BFB00000014035C10E00875
          11FF67B977FF8DD1A0FF8DD2A0FF8DD2A0FF68B877FF087511FF1D601FFFEDD4
          C4FFFBF2ECFFFFFFFFFFFAEEE7FFDDAE90FF7D4B1AE5000000100134087F046C
          11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF8EC395FF0D6919FFDDE2D8FFFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFD8D2CCE709090911000100050251
          0EC23F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF3F8A49FF91AE8DF9FFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF84848485000000000001
          00050130087502540FCF036713FC02520FD0012D087CDEDEDEE0ECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDE000000000000
          00000000000100000002000000040000000400000003F9F9F9F94B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9000000000000
          00000000000000000000000000000000000000000000DEDEDEDEEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE000000000000
          000000000000000000000000000000000000000000007B7B7B7BFFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000000000
          0000000000000000000000000000000000000000000003030303C0C0C0C0FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          0000000000000000000000000000000000000000000000000000030303037272
          7272CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000200010010022A088703480FD9035713FF03450FDB012608900001001B0000
          0006000000000000000000000000000000000000000000000000000000010003
          0114024B0ECD167A26FF219C34FF18A92EFF139728FF0F7220FF03420FD70003
          0122000000040000000000000000000000000000000000000000000000030234
          098D147924FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF0B6F1CFF022B
          0A9E0000000C0000000000000000000000000000000000000000000000050359
          10E120922EFF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0B871AFF034B
          10E7000000130000000000000000000000000000000000000000000000040366
          12F913891DFF1D962FFF3BB157FF4CC06DFF3CB157FF1D962FFF0B8516FF0355
          12FB00000014000000000000000000000000000000000000000000000002035C
          10E0087511FF67B977FF8DD1A0FF8DD2A0FF8DD2A0FF68B877FF087511FF034D
          0FE5000000100000000000000000000000000000000000000000000000010134
          087F046C11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF8EC395FF046411FF022C
          088F000000080000000000000000000000000000000000000000000000010505
          01191E6213F33F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF3F8A49FF1C5716F67777
          758ECFCFCFD0FFFFFFFFCFCFCFCF757575750303030300000000000000034F30
          0E8DAA6C32FF797A34FF2D6F1DFF056814FF2D6C1EFF7D7B3AFFE9DCD0FFFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFC9C9C9C909090909000000058652
          1AE1C97D47FFD47B42FFD88045FFDA8247FFD8864AFFEDC4A6FFFFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF8484848400000004985C
          1BF9CB723AFFD47B47FFE0915CFFE69A67FFE3935EFFFAEFE8FFECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDE000000028953
          19E0BD652DFFE1A27DFFEBBB9BFFECBA9CFFEBBC9DFFFEFDFCFF4B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9000000014C2E
          0D7FA76022FFE4B89FFFF2DACCFFF2DACCFFF2DACCFFFCF6F3FFEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE000000000201
          0005764716C2BC8655FFE4C3ADFFF3E2D9FFE3C3ADFFDBBFA7FFFFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000000000
          000002010005462A0C757E4C18CF9A5D1CFC7B4B17D0462C0F7EC1C0C0C3FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          0000000000000000000100000002000000040000000400000003030303047272
          7272CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000030000
          00070000000A0000000B0000000B0000000C0000000C0000000D0000000E0000
          000E0000000F0000000F0000000D0000000600000001000000007B4F28BBAA6C
          35FFA86A33FFA76931FFA6672EFFA4642CFFA3632BFFA26229FFA16027FFA05E
          25FF9F5D23FF9E5C22FF9E5B21FF704017BF0000000600000000AC703AFFEEB8
          8DFFE69F64FFE39B61FFE2995EFFDF965BFFDE9459FFDB9155FFDA8E53FFD78C
          50FFD68A4EFFD4874CFFD28549FF9E5B21FF0000000C00000000AE723CFFF7C3
          98FFF3AA70FFF0A86EFFF0A56BFFEEA368FFECA166FFEB9E63FFE99C61FFE89A
          5FFFE6985CFFE5955AFFDF9055FF9E5C23FF0000000F00000000AF743EFFF8C5
          9BFFF4AC72FFF2AA70FFF1A86EFFEFA66BFFEEA368FFECA165FFEB9E63FFE99D
          60FFE7995EFFE6975CFFE49659FF9F5E24FF0000000F00000000B07541FFF9C7
          9FFFF5AF74FFF2AA6FFFF1A86DFFEFA56AFFEDA368FFECA065FFECA065FFEB9F
          62FFE99B60FFE79A5EFFE6975BFFA05E25FF0000000E00000000B17742FFF9CA
          A3FFF6B076FFF4AC72FFF2A970FFF0A76DFFEFA46AFFEEA368FFEEA368FFECA0
          66FFEA9E62FFE99C60FFE7995DFFA26027FF0000000E00000000B37944FFFACC
          A6FFF7B278FFF4AE74FFF3AB71FFF2A96FFFF0A76CFFEFA46AFFEFA56CFFF5CD
          ADFFFBEDE2FFFFFFFFFFFBECE1FFCDA98BFF0303031000000000B37A46FFFCCE
          A9FFF8B47AFFF6B076FFF4AE73FFF6C399FFF5C095FFF4BF96FFFDF0E7FFFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFC9C9C9CB09090909B57D48FFFCD1
          ACFFFAB67CFFF7B278FFAF743EFFAD723CFFAD703AFFD6B99FFFFFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF84848484B67D4AFFFDD2
          AFFFFBBE8AFFF8B37AFFB07540FFCDCDCDFFBCBEBEFFF0F0F0FFECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDEB77F4BFFFED5
          B2FFFDD3B0FFFCCFAAFFB27743FFE7E7E7FFD6D6D6FFFDFDFDFF4B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9885F39BEB77F
          4CFFB67E4AFFB47B46FFA17650FF6E6E6FFF6E6E6EFFECECECFFEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE000000010000
          000200000003000000090000000A0000000B0000000C7B7B7B81FFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000000000
          0000000000000000000000000000000000000000000003030303C0C0C0C0FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          0000000000000000000000000000000000000000000000000000030303037272
          7272CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000001E35
          69B7284991FF284890FF26468FFF25458EFF1B3164B700000000000000000000
          0000000000000000000000000000000000000000000000000000000000002A4B
          93FFCDE9FBFF68BAF0FF63B5ECFF5DAEE8FF25458DFF00000000000000000000
          0002000000080000000F00000014000000120000000B00000005000000082B4D
          94FFD5EDFCFF6EC0F4FF67BAEFFF63B4EDFF27468EFF00000000000000020001
          0010022A088703480FD9035713FF03450FDB012709910202002A40270C892C4E
          95FFDEF1FDFFD6EDFCFFCEEAFBFFC4E4F9FF28478FFF0000000000030114024B
          0ECD167A26FF219C34FF18A92EFF139728FF0F7220FF144D12F7AC845BFF5D6B
          97FF2C4D95FF2B4D93FF2A4B92FF2A4A92FF1F366BC6000000040234098D1479
          24FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF0B6F1CFF6B7544FFEBC1
          A3FF5E4A3DFFECEAE9FFECEAE8FF4D392DFF492E13A00000000C035910E12092
          2EFF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF0B871AFF175717FFD89B
          74FFA0816CFF634E40FF5F493BFF947763FF896037EA00000013036612F91389
          1DFF1D962FFF3BB157FF4CC06DFF3CB157FF1D962FFF0B8516FF0A5916FFE5BE
          A4FFFBF1EAFFFFFFFFFFFBF2ECFFECC8B2FF8B5620FB00000014035C10E00875
          11FF67B977FF8DD1A0FF8DD2A0FF8DD2A0FF68B877FF117A19FFCCDBCCFFFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFE4D9CEF9090909180134087F046C
          11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF8EC395FF86B48CFFFFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF84848488000100050251
          0EC23F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF3F8A49FFE2E9E0FDECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDE000000000001
          00050130087502540FCF036713FC02520FD0012D087CF9F9F9F94B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9000000000000
          00000000000100000002000000040000000400000003DEDEDEDEEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE000000000000
          000000000000000000000000000000000000000000007B7B7B7BFFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000000000
          0000000000000000000000000000000000000000000003030303C0C0C0C0FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          0000000000000000000000000000000000000000000000000000030303037272
          7272CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000002000000080000000F00000014000000120000000B000000031E35
          69B7284991FF284890FF26468FFF25458EFF1B3164B700000000000000000000
          000200010010022A088703480FD9035713FF03450FDB012608900203021D2A4B
          93FFCDE9FBFF68BAF0FF63B5ECFF5DAEE8FF25458DFF00000000000000010003
          0114024B0ECD167A26FF219C34FF18A92EFF139728FF0F7220FF668C6EE72B4D
          94FFD5EDFCFF6EC0F4FF67BAEFFF63B4EDFF27468EFF00000000000000030234
          098D147924FF1BA32DFF0F9D22FF0F9E22FF0F9D23FF0F9E23FF85B78EFF2C4E
          95FFDEF1FDFFD6EDFCFFCEEAFBFFC4E4F9FF28478FFF00000000000000050359
          10E120922EFF0D931DFF0D931CFF0D911CFF0D931CFF0D911CFF7ABE82FF3E62
          8EFB2C4D95FF2B4D93FF2A4B92FF2A4A92FF1E356BBD00000000000000040366
          12F913891DFF1D962FFF3BB157FF4CC06DFF3CB157FF1D962FFF2A9433FF7DA7
          84FD4A3A2FECECEAE9FFECEAE8FF3D2C23EA000000000000000000000002035C
          10E0087511FF67B977FF8DD1A0FF8DD2A0FF8DD2A0FF68B877FF087511FF3D76
          47EB2D231D8C49392EE446352AE4251C16840000000000000000000000010134
          087F046C11FF8FC395FFC4E3CAFFC4E3CAFFC4E3CAFF8EC395FF076614FF778D
          7BC3CFCFCFD1FFFFFFFFCFCFCFCF757575750303030300000000000000010505
          01191E6213F33F8F49FF9FC9A3FFD2E6D4FF9FC8A3FF468E4FFFCCD9CAFDFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFC9C9C9C909090909000000034F30
          0E8DAA6C32FF797A34FF2D6F1DFF056814FF2D6C1EFFBEBD9DFFFFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF84848484000000058652
          1AE1C97D47FFD47B42FFD88045FFDA8247FFD8864AFFFAEFE7FFECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDE00000004985C
          1BF9CB723AFFD47B47FFE0915CFFE69A67FFE3935EFFFEFCFBFF4B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9000000028953
          19E0BD652DFFE1A27DFFEBBB9BFFECBA9CFFEBBC9DFFFBF3EFFFEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE000000014C2E
          0D7FA76022FFE4B89FFFF2DACCFFF2DACCFFF2DACCFFF2DBCEFFFFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000000201
          0005764716C2BC8655FFE4C3ADFFF3E2D9FFE3C3ADFFBA8457FFDBD1C6F1FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          000002010005462A0C757E4C18CF9A5D1CFC7B4B17D0442A0D7C0504030F7272
          7273CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000001E35
          69B7284991FF284890FF26468FFF25458EFF1B3164B700000000000000030000
          00070000000A0000000B0000000B0000000C0000000C0000000D0000000E2A4B
          93FFCDE9FBFF68BAF0FF63B5ECFF5DAEE8FF25458DFF000000007B4F28BBAA6C
          35FFA86A33FFA76931FFA6672EFFA4642CFFA3632BFFA26229FFCCA889FF2B4D
          94FFD5EDFCFF6EC0F4FF67BAEFFF63B4EDFF27468EFF00000000AC703AFFEEB8
          8DFFE69F64FFE39B61FFE2995EFFDF965BFFDE9459FFDB9155FFEBC1A1FF2C4E
          95FFDEF1FDFFD6EDFCFFCEEAFBFFC4E4F9FF28478FFF00000000AE723CFFF7C3
          98FFF3AA70FFF0A86EFFF0A56BFFEEA368FFECA166FFEB9E63FFF3C9A9FF6170
          9CFF2C4D95FF2B4D93FF2A4B92FF2A4A92FF1E366BC100000000AF743EFFF8C5
          9BFFF4AC72FFF2AA70FFF1A86EFFEFA66BFFEEA368FFECA165FFEEAA77FFF4CD
          AFFF5E4B3DFFECEAE9FFECEAE8FF4E3A2FFF0000000F00000000B07541FFF9C7
          9FFFF5AF74FFF3AC72FFF2AA6FFFF1A86DFFEFA56AFFEDA368FFECA065FFF0B9
          8CFFA38672FF634F41FF5F4A3CFF8A705DFF0000000E00000000B17742FFF9CA
          A3FFF5AF74FFF4AC72FFF2A970FFF0A76DFFEFA46AFFEFA46AFFEEA46AFFF5CC
          ADFFFCF1E9FFFFFFFFFFFDF4EEFFDABFA8FF0303031100000000B37944FFFACC
          A6FFF6B076FFF4AE74FFF3AB71FFF2A96FFFF0A76CFFF1AA71FFFBEBDEFFFFFF
          FFFFEAE9E8FF35241BFFEAE9E8FFFFFFFFFFC9C9C9CC09090909B37A46FFFCCE
          A9FFF8B278FFF6B076FFF4AE73FFF6C399FFF5C095FFFAE1CCFFFFFFFFFFFFFF
          FFFF4F4037FF39281FFF4C3E35FFFFFFFFFFFFFFFFFF84848484B57D48FFFCD1
          ACFFF8B47AFFF7B278FFAF743EFFAD723CFFAD703AFFF4ECE6FFECEAE9FF5546
          3CFFFFFFFFFF402E25FFFFFFFFFF504239FFEBE9E8FFDEDEDEDEB67D4AFFFDD2
          AFFFFAB57CFFF8B37AFFB07540FFCDCDCDFFBCBEBEFFFDFDFDFF4B392DFF4938
          2CFF47352BFF46342AFF443329FF433127FF423026FFF9F9F9F9B77F4BFFFED5
          B2FFFCD1ADFFFCCFAAFFB27743FFE7E7E7FFD6D6D6FFFAFAFAFFEDEBEAFF6251
          47FFFFFFFFFF4C3B2FFFFFFFFFFF5C4C42FFECEBE9FFDEDEDEDE885F39BEB77F
          4CFFB57C48FFB47B46FFA17650FF6E6E6FFF6E6E6EFFB4B4B4FFFFFFFFFFFFFF
          FFFF66554AFF534035FF645349FFFFFFFFFFFFFFFFFF7B7B7B7B000000010000
          000200000007000000090000000A0000000B0000000C03030309C0C0C0C1FFFF
          FFFFEEECEBFF59463AFFEEECEBFFFFFFFFFFC0C0C0C003030303000000000000
          0000000000000000000000000000000000000000000000000000030303037272
          7272CCCCCCCCFCFCFCFCCCCCCCCC727272720303030300000000}
      end>
  end
  object pmTreeViewActions: TPopupMenu
    Images = ilActions
    OnPopup = pmTreeViewActionsPopup
    Left = 80
    Top = 360
    object Undo1: TMenuItem
      Action = acUndo
    end
    object Redo1: TMenuItem
      Action = acRedo
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miExpandAll: TMenuItem
      Action = acTreeViewExpandAll
    end
    object miCallapseAll: TMenuItem
      Action = acTreeViewCollapseAll
    end
    object miSeparator1: TMenuItem
      Caption = '-'
    end
    object miTreeViewDelete: TMenuItem
      Action = acTreeViewItemsDelete
    end
    object miSeparator2: TMenuItem
      Caption = '-'
    end
    object miAlignBy: TMenuItem
      Action = acAlignBy
    end
    object miAlignHorz: TMenuItem
      Caption = 'AlignHorz'
      object miHLeft: TMenuItem
        Action = acHAlignLeft
      end
      object miHCenter: TMenuItem
        Action = acHAlignCenter
      end
      object miHRight: TMenuItem
        Action = acHAlignRight
      end
      object miHClient: TMenuItem
        Action = acHAlignClient
      end
      object miHParentManaged: TMenuItem
        Action = acHAlignParent
      end
    end
    object miAlignVert: TMenuItem
      Caption = 'miAlignVert'
      object miVAlignTop: TMenuItem
        Action = acVAlignTop
      end
      object miVAlignCenter: TMenuItem
        Action = acVAlignCenter
      end
      object miVAlignBottom: TMenuItem
        Action = acVAlignBottom
      end
      object miVAlignClient: TMenuItem
        Action = acVAlignClient
      end
      object miVAlignParent: TMenuItem
        Action = acVAlignParent
      end
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object miDirection: TMenuItem
      Caption = 'miDirection'
      object miDirectionHorizontal: TMenuItem
        Action = acDirectionHorizontal
      end
      object miDirectionVertical: TMenuItem
        Action = acDirectionVertical
      end
      object miDirectionTabbed: TMenuItem
        Action = acDirectionTabbed
      end
    end
    object miBorder: TMenuItem
      Action = acBorder
    end
    object miExpandButton: TMenuItem
      Action = acExpandButton
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miGroup: TMenuItem
      Action = acGroup
    end
    object miUngroup: TMenuItem
      Action = acUngroup
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miCollapsible: TMenuItem
      Action = acCollapsible
    end
    object miCaption: TMenuItem
      Action = acCaption
    end
    object miTextPosition: TMenuItem
      Caption = 'TextPosition'
      ImageIndex = 19
      object miTextPositionLeft: TMenuItem
        Action = acTextPositionLeft
      end
      object miTextPositionTop: TMenuItem
        Action = acTextPositionTop
      end
      object miTextPositionRight: TMenuItem
        Action = acTextPositionRight
      end
      object miTextPositionBottom: TMenuItem
        Action = acTextPositionBottom
      end
    end
    object miCaptionAlignHorz: TMenuItem
      Caption = 'CaptionAlignHorz'
      object miCaptionAlignHorzLeft: TMenuItem
        Action = acCaptionAlignHorzLeft
      end
      object miCaptionAlignHorzCenter: TMenuItem
        Action = acCaptionAlignHorzCenter
      end
      object miCaptionAlignHorzRight: TMenuItem
        Action = acCaptionAlignHorzRight
      end
    end
    object miCaptionAlignVert: TMenuItem
      Caption = 'CaptionAlignVert'
      object miCaptionAlignVertTop: TMenuItem
        Action = acCaptionAlignVertTop
      end
      object miCaptionAlignVertCenter: TMenuItem
        Action = acCaptionAlignVertCenter
      end
      object miCaptionAlignVertBottom: TMenuItem
        Action = acCaptionAlignVertBottom
      end
    end
    object miTreeViewItemRename: TMenuItem
      Action = acTreeViewItemRename
    end
  end
  object pmAvailableItemsActions: TPopupMenu
    Images = ilActions
    OnPopup = pmAvailableItemsActionsPopup
    Left = 78
    Top = 392
    object Undo2: TMenuItem
      Action = acUndo
    end
    object Redo2: TMenuItem
      Action = acRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ExpandAll1: TMenuItem
      Action = acAvailableItemsExpandAll
    end
    object CallapseAll1: TMenuItem
      Action = acAvailableItemsCollapseAll
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object AddGroup1: TMenuItem
      Action = acAddGroup
    end
    object AddItem1: TMenuItem
      Action = acAddItem
    end
    object AddEmptySpaceItem1: TMenuItem
      Action = acAddEmptySpaceItem
    end
    object AddLabel1: TMenuItem
      Action = acAddLabeledItem
    end
    object AddImage2: TMenuItem
      Action = acAddImage
    end
    object acAddSeparator1: TMenuItem
      Action = acAddSeparator
    end
    object AddSplitter1: TMenuItem
      Action = acAddSplitter
    end
    object Delete1: TMenuItem
      Action = acAvailableItemsDelete
      SubMenuImages = ilActions
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Rename2: TMenuItem
      Action = acAvailableItemRename
      ShortCut = 113
    end
  end
  object ilHelper: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 25690224
  end
  object pmAddCustomItem: TPopupMenu
    Images = ilActions
    Left = 152
    Top = 360
    object AddEmptySpaceItem2: TMenuItem
      Action = acAddEmptySpaceItem
    end
    object acAddLabeledItem1: TMenuItem
      Action = acAddLabeledItem
    end
    object AddImage1: TMenuItem
      Action = acAddImage
    end
    object acAddSeparator2: TMenuItem
      Action = acAddSeparator
    end
    object acAddSplitter1: TMenuItem
      Action = acAddSplitter
    end
  end
end
