object dxfmCompositionDesignWindow: TdxfmCompositionDesignWindow
  Left = 380
  Top = 267
  BorderStyle = bsDialog
  Caption = 'Composition Designer'
  ClientHeight = 330
  ClientWidth = 462
  Color = clBtnFace
  Constraints.MinHeight = 334
  Constraints.MinWidth = 462
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 462
    Height = 330
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lvItems: TcxListView
      Left = 21
      Top = 44
      Width = 318
      Height = 213
      ColumnClick = False
      Columns = <>
      DragMode = dmAutomatic
      MultiSelect = True
      PopupMenu = pmItems
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvItemsChange
      OnDblClick = DesignerClick
      OnDragDrop = lvItemsDragDrop
      OnDragOver = lvItemsDragOver
      OnEdited = lvItemsEdited
      OnEditing = lvItemsEditing
      OnResize = lvItemsResize
      OnStartDrag = lvItemsStartDrag
    end
    object btnAdd: TcxButton
      Left = 345
      Top = 44
      Width = 96
      Height = 23
      Caption = '&Add...'
      TabOrder = 1
      OnClick = AddClick
    end
    object btnDelete: TcxButton
      Left = 345
      Top = 73
      Width = 96
      Height = 23
      Caption = '&Delete...'
      TabOrder = 2
      OnClick = DeleteClick
    end
    object btnDesign: TcxButton
      Left = 345
      Top = 102
      Width = 96
      Height = 23
      Caption = 'Desi&gn...'
      TabOrder = 3
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = DesignerClick
    end
    object btnMoveUp: TcxButton
      Left = 345
      Top = 131
      Width = 96
      Height = 23
      Caption = 'Move Up'
      TabOrder = 4
      OnClick = MoveUpClick
    end
    object btnMoveDown: TcxButton
      Left = 345
      Top = 160
      Width = 96
      Height = 23
      Caption = 'Move Down'
      TabOrder = 5
      OnClick = MoveDownClick
    end
    object cbStartEachItemFromNewPage: TcxCheckBox
      Left = 21
      Top = 263
      Caption = '&Start each item from new page'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = cbStartEachItemFromNewPageClick
    end
    object btnClose: TcxButton
      Left = 276
      Top = 297
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 7
    end
    object btnHelp: TcxButton
      Left = 367
      Top = 297
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 8
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tbsItems: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Items'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = tbsItems
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvItems
      ControlOptions.OriginalHeight = 199
      ControlOptions.OriginalWidth = 310
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnDesign'
      CaptionOptions.Visible = False
      Control = btnDesign
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveUp'
      CaptionOptions.Visible = False
      Control = btnMoveUp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveDown'
      CaptionOptions.Visible = False
      Control = btnMoveDown
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = tbsItems
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'cbStartEachItemFromNewPage'
      CaptionOptions.Visible = False
      Control = cbStartEachItemFromNewPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 168
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbbtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object pnlNoItems: TcxLabel
    Left = 44
    Top = 130
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'There are no Items to dispay'
    ParentColor = False
    ParentFont = False
    Style.TextColor = clGray
    Style.TextStyle = [fsBold]
    Properties.Alignment.Horz = taCenter
    Properties.Alignment.Vert = taVCenter
    Transparent = True
    Height = 54
    Width = 269
    AnchorX = 179
    AnchorY = 157
  end
  object pmItems: TPopupMenu
    Images = ilItems
    OnPopup = pmItemsPopup
    Left = 36
    Top = 72
    object miAdd: TMenuItem
      Caption = '&Add...'
      ImageIndex = 14
      ShortCut = 45
      OnClick = AddClick
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ImageIndex = 5
      ShortCut = 46
      OnClick = DeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miRename: TMenuItem
      Caption = 'Rena&me'
      ShortCut = 113
      OnClick = RenameClick
    end
    object miSelectAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAllClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miDesign: TMenuItem
      Caption = 'Desi&gn...'
      Default = True
      ImageIndex = 6
      ShortCut = 16397
      OnClick = DesignerClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miMoveUp: TMenuItem
      Caption = 'Move &Up'
      ImageIndex = 10
      ShortCut = 16422
      OnClick = MoveUpClick
    end
    object miMoveDown: TMenuItem
      Caption = 'Move &Down'
      ImageIndex = 11
      ShortCut = 16424
      OnClick = MoveDownClick
    end
  end
  object ilItems: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 3670144
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 112
    Top = 56
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
