object frmPivotGridDesigner: TfrmPivotGridDesigner
  Left = 253
  Top = 245
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'PivotGrid Designer'
  ClientHeight = 368
  ClientWidth = 442
  Color = clBtnFace
  Constraints.MinHeight = 402
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 442
    Height = 368
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lbFields: TcxListBox
      Left = 20
      Top = 44
      Width = 296
      Height = 273
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      MultiSelect = True
      PopupMenu = pmFields
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbFieldsClick
      OnDragDrop = lbFieldsDragDrop
      OnDragOver = lbFieldsDragOver
      OnEndDrag = lbFieldsEndDrag
    end
    object btnAdd: TcxButton
      Left = 322
      Top = 44
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add '
      TabOrder = 1
      OnClick = btnFieldsPageClick
    end
    object btnDelete: TcxButton
      Tag = 1
      Left = 322
      Top = 75
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnFieldsPageClick
    end
    object btnMoveUp: TcxButton
      Tag = 2
      Left = 322
      Top = 106
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Move &Up'
      TabOrder = 3
      OnClick = btnFieldsPageClick
    end
    object btnMoveDown: TcxButton
      Tag = 3
      Left = 322
      Top = 137
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Move &Down'
      TabOrder = 4
      OnClick = btnFieldsPageClick
    end
    object btnRetrieveFields: TcxButton
      Tag = 4
      Left = 322
      Top = 168
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Retrieve &Fields'
      TabOrder = 5
      OnClick = btnFieldsPageClick
    end
    object tvGroups: TcxTreeView
      Left = 10000
      Top = 10000
      Width = 156
      Height = 255
      Anchors = [akLeft, akTop, akRight, akBottom]
      PopupMenu = pmGroups
      Style.TransparentBorder = False
      TabOrder = 6
      Visible = False
      OnClick = tvGroupsClick
      OnDblClick = tvGroupsDblClick
      OnDragDrop = tvGroupsDragDrop
      OnDragOver = tvGroupsDragOver
      OnKeyUp = tvGroupsKeyUp
      OnMouseDown = tvGroupsMouseDown
      OnChange = tvGroupsChange
      OnCustomDrawItem = tvGroupsCustomDrawItem
      OnEditing = tvGroupsEditing
      OnEdited = tvGroupsEdited
    end
    object btnAddGroup: TcxButton
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = '&Add'
      TabOrder = 7
      Visible = False
      OnClick = btnGroupClick
    end
    object btnDeleteGroup: TcxButton
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = '&Delete'
      TabOrder = 8
      Visible = False
      OnClick = btnGroupClick
    end
    object btnUnlink: TcxButton
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = '>'
      TabOrder = 9
      Visible = False
      OnClick = btnGroupClick
    end
    object btnLink: TcxButton
      Tag = 3
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = '<'
      TabOrder = 10
      Visible = False
      OnClick = btnGroupClick
    end
    object btnMoveUpGroup: TcxButton
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = 'Move &Up'
      TabOrder = 11
      Visible = False
      OnClick = miMoveInGroupClick
    end
    object btnMoveDownGroup: TcxButton
      Tag = 3
      Left = 10000
      Top = 10000
      Width = 78
      Height = 25
      Anchors = [akTop]
      Caption = 'Move &Down'
      TabOrder = 12
      Visible = False
      OnClick = miMoveInGroupClick
    end
    object lbUnlinkedFields: TcxListBox
      Left = 10000
      Top = 10000
      Width = 156
      Height = 255
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 13
      Visible = False
      OnClick = lbUnlinkedFieldsClick
      OnDblClick = lbUnlinkedFieldsDblClick
    end
    object btnClose: TcxButton
      Left = 352
      Top = 333
      Width = 80
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 1
      TabOrder = 14
      OnClick = btnCloseClick
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object pcDesigner: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 0
    end
    object tbsFields: TdxLayoutGroup
      Parent = pcDesigner
      CaptionOptions.Text = '&Fields'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tbsFields
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbFields
      ControlOptions.OriginalHeight = 281
      ControlOptions.OriginalWidth = 306
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = tbsFields
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnMoveUp'
      CaptionOptions.Visible = False
      Control = btnMoveUp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnMoveDown'
      CaptionOptions.Visible = False
      Control = btnMoveDown
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liRetrieveFields: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnRetrieveFields'
      CaptionOptions.Visible = False
      Control = btnRetrieveFields
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object tbsGroups: TdxLayoutGroup
      Parent = pcDesigner
      CaptionOptions.Text = '&Groups'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = tbsGroups
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Groups:'
      CaptionOptions.Layout = clTop
      Control = tvGroups
      ControlOptions.OriginalHeight = 265
      ControlOptions.OriginalWidth = 160
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = tbsGroups
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = ' '
      CaptionOptions.Layout = clTop
      Control = btnAddGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnDeleteGroup'
      CaptionOptions.Visible = False
      Control = btnDeleteGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnUnlink'
      CaptionOptions.Visible = False
      Control = btnUnlink
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnLink'
      CaptionOptions.Visible = False
      Control = btnLink
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveUpGroup'
      CaptionOptions.Visible = False
      Control = btnMoveUpGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveDownGroup'
      CaptionOptions.Visible = False
      Control = btnMoveDownGroup
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tbsGroups
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Unlinked fields:'
      CaptionOptions.Layout = clTop
      Control = lbUnlinkedFields
      ControlOptions.OriginalHeight = 265
      ControlOptions.OriginalWidth = 160
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object pmFields: TPopupMenu
    Left = 36
    Top = 336
    object miAdd: TMenuItem
      Caption = 'Add'
      ShortCut = 45
      OnClick = btnFieldsPageClick
    end
    object miDelete: TMenuItem
      Tag = 1
      Caption = 'Delete'
      ShortCut = 46
      OnClick = btnFieldsPageClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miMoveUp: TMenuItem
      Tag = 2
      Caption = 'Move Up'
      ShortCut = 16422
      OnClick = btnFieldsPageClick
    end
    object miMoveDown: TMenuItem
      Tag = 3
      Caption = 'Move Down'
      ShortCut = 16424
      OnClick = btnFieldsPageClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Tag = 5
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = btnFieldsPageClick
    end
  end
  object pmGroups: TPopupMenu
    Left = 76
    Top = 336
    object miAddGroup: TMenuItem
      Caption = 'Add'
      ShortCut = 45
      OnClick = btnGroupClick
    end
    object miDeleteGroup: TMenuItem
      Tag = 1
      Caption = 'Delete'
      ShortCut = 46
      OnClick = btnGroupClick
    end
    object MenuItem3: TMenuItem
      Caption = '-'
    end
    object miMoveUpInGroup: TMenuItem
      Tag = 2
      Caption = 'Move Up'
      ShortCut = 16422
      OnClick = miMoveInGroupClick
    end
    object miMoveDownInGroup: TMenuItem
      Tag = 3
      Caption = 'Move Down'
      ShortCut = 16424
      OnClick = miMoveInGroupClick
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 56
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
