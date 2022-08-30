inherited cxCustomTableViewEditor: TcxCustomTableViewEditor
  Left = 556
  Top = 316
  Caption = 'cxCustomTableViewEditor'
  ClientHeight = 300
  ClientWidth = 390
  ParentFont = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited PViewEditor: TPanel
    Width = 390
    Height = 300
    Constraints.MinHeight = 300
    Constraints.MinWidth = 390
    inherited lcMain: TdxLayoutControl
      Width = 390
      Height = 300
      AutoSize = True
      object LBColumns: TcxListBox [0]
        Left = 20
        Top = 44
        Width = 224
        Height = 236
        DragMode = dmAutomatic
        ItemHeight = 13
        ListStyle = lbOwnerDrawFixed
        MultiSelect = True
        PopupMenu = PMColumns
        Style.TransparentBorder = False
        TabOrder = 0
        OnClick = LBColumnsClick
        OnDragDrop = LBColumnsDragDrop
        OnDragOver = LBColumnsDragOver
        OnDrawItem = LBColumnsDrawItem
        OnEndDrag = LBColumnsEndDrag
        OnKeyPress = FormKeyPress
        OnStartDrag = LBColumnsStartDrag
      end
      object BColumnAdd: TcxButton [1]
        Left = 250
        Top = 44
        Width = 120
        Height = 24
        Caption = '&Add'
        TabOrder = 1
        OnClick = BColumnAddClick
      end
      object BColumnDelete: TcxButton [2]
        Left = 250
        Top = 74
        Width = 120
        Height = 24
        Caption = 'Delete'
        TabOrder = 2
        OnClick = BColumnDeleteClick
      end
      object BColumnRestore: TcxButton [3]
        Left = 250
        Top = 104
        Width = 120
        Height = 24
        Caption = '&Restore Defaults'
        TabOrder = 3
        OnClick = BColumnRestoreClick
      end
      object BColumnMoveUp: TcxButton [4]
        Left = 250
        Top = 150
        Width = 120
        Height = 24
        Caption = 'Move &Up'
        TabOrder = 4
        OnClick = BColumnMoveUpClick
      end
      object BColumnMoveDown: TcxButton [5]
        Left = 250
        Top = 180
        Width = 120
        Height = 24
        Caption = 'Move &Down'
        TabOrder = 5
        OnClick = BColumnMoveDownClick
      end
      object BColumnAddAll: TcxButton [6]
        Left = 250
        Top = 226
        Width = 120
        Height = 24
        Caption = 'Retrieve &Fields'
        TabOrder = 6
        OnClick = BColumnAddAllClick
      end
      object BColumnAddMissing: TcxButton [7]
        Left = 250
        Top = 256
        Width = 120
        Height = 24
        Caption = 'Retrieve &Missing Fields'
        TabOrder = 7
        OnClick = BColumnAddMissingClick
      end
      object BAddFooterSummaryItem: TcxButton [8]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Add'
        TabOrder = 9
        Visible = False
        OnClick = BAddFooterSummaryItemClick
      end
      object BDeleteFooterSummaryItem: TcxButton [9]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Delete'
        TabOrder = 10
        Visible = False
        OnClick = BDeleteFooterSummaryItemClick
      end
      object BDefaultGroupSummaryAdd: TcxButton [10]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Add'
        TabOrder = 12
        Visible = False
        OnClick = BDefaultGroupSummaryAddClick
      end
      object BDefaultGroupSummaryDelete: TcxButton [11]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Delete'
        TabOrder = 13
        Visible = False
        OnClick = BDefaultGroupSummaryDeleteClick
      end
      object LBSummaryGroups: TcxListBox [12]
        Left = 10000
        Top = 10000
        Width = 162
        Height = 72
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = PMSummaryGroups
        Style.TransparentBorder = False
        TabOrder = 14
        Visible = False
        OnClick = LBSummaryGroupsClick
        OnKeyPress = FormKeyPress
      end
      object LBGroupSummaryItems: TcxListBox [13]
        Left = 10000
        Top = 10000
        Width = 161
        Height = 72
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = PMGroupSummaryItems
        Style.TransparentBorder = False
        TabOrder = 17
        Visible = False
        OnClick = LBGroupSummaryItemsClick
        OnKeyPress = FormKeyPress
      end
      object LBUnlinkedColumns: TcxListBox [14]
        Left = 10000
        Top = 10000
        Width = 153
        Height = 74
        ItemHeight = 13
        MultiSelect = True
        Style.TransparentBorder = False
        TabOrder = 20
        Visible = False
        OnClick = LBUnlinkedColumnsClick
        OnKeyPress = FormKeyPress
      end
      object BColumnLink: TcxButton [15]
        Left = 10000
        Top = 10000
        Width = 25
        Height = 25
        Caption = '>'
        TabOrder = 21
        Visible = False
        OnClick = BColumnLinkClick
      end
      object BColumnUnlink: TcxButton [16]
        Left = 10000
        Top = 10000
        Width = 25
        Height = 25
        Caption = '<'
        TabOrder = 22
        Visible = False
        OnClick = BColumnUnlinkClick
      end
      object LBLinkedColumns: TcxListBox [17]
        Left = 10000
        Top = 10000
        Width = 139
        Height = 74
        ItemHeight = 13
        MultiSelect = True
        Style.TransparentBorder = False
        TabOrder = 23
        Visible = False
        OnClick = LBUnlinkedColumnsClick
        OnKeyPress = FormKeyPress
      end
      object LBFooterSummary: TcxListBox [18]
        Left = 10000
        Top = 10000
        Width = 183
        Height = 144
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = PMFooterSummary
        Style.TransparentBorder = False
        TabOrder = 8
        Visible = False
        OnClick = LBFooterSummaryClick
        OnKeyPress = FormKeyPress
      end
      object LBDefaultGroupsSummary: TcxListBox [19]
        Left = 10000
        Top = 10000
        Width = 183
        Height = 170
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = PMDefaultGroupsSummary
        Style.TransparentBorder = False
        TabOrder = 11
        Visible = False
        OnClick = LBDefaultGroupsSummaryClick
        OnKeyPress = FormKeyPress
      end
      object BSummaryGroupAdd: TcxButton [20]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Add'
        TabOrder = 15
        Visible = False
        OnClick = BSummaryGroupAddClick
      end
      object BSummaryGroupDelete: TcxButton [21]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Delete'
        TabOrder = 16
        Visible = False
        OnClick = BSummaryGroupDeleteClick
      end
      object BGroupSummaryItemAdd: TcxButton [22]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Add'
        TabOrder = 18
        Visible = False
        OnClick = BGroupSummaryItemAddClick
      end
      object BGroupSummaryItemDelete: TcxButton [23]
        Left = 10000
        Top = 10000
        Width = 75
        Height = 24
        Caption = '&Delete'
        TabOrder = 19
        Visible = False
        OnClick = BGroupSummaryItemDeleteClick
      end
      inherited lcMainGroup_Root: TdxLayoutGroup
        LayoutDirection = ldTabbed
      end
      object lgItems: TdxLayoutGroup
        Parent = lcMainGroup_Root
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        LayoutDirection = ldHorizontal
        Index = 0
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = lgItems
        AlignHorz = ahClient
        AlignVert = avClient
        Control = LBColumns
        ControlOptions.OriginalHeight = 100
        ControlOptions.OriginalWidth = 100
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup5: TdxLayoutGroup
        Parent = lgItems
        AlignHorz = ahRight
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ItemIndex = 3
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem7: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnAdd'
        CaptionOptions.Visible = False
        Control = BColumnAdd
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem3: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnDelete'
        CaptionOptions.Visible = False
        Control = BColumnDelete
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem4: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnRestore'
        CaptionOptions.Visible = False
        Control = BColumnRestore
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 2
      end
      object dxLayoutItem5: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnMoveUp'
        CaptionOptions.Visible = False
        Control = BColumnMoveUp
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 4
      end
      object dxLayoutItem6: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnMoveDown'
        CaptionOptions.Visible = False
        Control = BColumnMoveDown
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 5
      end
      object liColumnAddAll: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnAddAll'
        CaptionOptions.Visible = False
        Control = BColumnAddAll
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 7
      end
      object liColumnAddMissing: TdxLayoutItem
        Parent = dxLayoutGroup5
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnAddMissing'
        CaptionOptions.Visible = False
        Control = BColumnAddMissing
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 8
      end
      object lgSummary: TdxLayoutGroup
        Parent = lcMainGroup_Root
        CaptionOptions.Text = '   Summary   '
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 1
      end
      object dxLayoutGroup7: TdxLayoutGroup
        Parent = lgSummary
        AlignHorz = ahClient
        AlignVert = avClient
        ButtonOptions.Buttons = <>
        LayoutDirection = ldTabbed
        ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup8: TdxLayoutGroup
        Parent = dxLayoutGroup7
        CaptionOptions.Text = '   Footer   '
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 0
      end
      object dxLayoutGroup9: TdxLayoutGroup
        Parent = dxLayoutGroup8
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup15: TdxLayoutGroup
        Parent = dxLayoutGroup9
        AlignHorz = ahLeft
        AlignVert = avBottom
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem10: TdxLayoutItem
        Parent = dxLayoutGroup15
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BAddFooterSummaryItem'
        CaptionOptions.Visible = False
        Control = BAddFooterSummaryItem
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem11: TdxLayoutItem
        Parent = dxLayoutGroup15
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BDeleteFooterSummaryItem'
        CaptionOptions.Visible = False
        Control = BDeleteFooterSummaryItem
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup18: TdxLayoutGroup
        Parent = dxLayoutGroup7
        CaptionOptions.Text = '   Default For Groups   '
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 1
      end
      object dxLayoutGroup19: TdxLayoutGroup
        Parent = dxLayoutGroup18
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup25: TdxLayoutGroup
        Parent = dxLayoutGroup19
        AlignHorz = ahLeft
        AlignVert = avBottom
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem13: TdxLayoutItem
        Parent = dxLayoutGroup25
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BDefaultGroupSummaryAdd'
        CaptionOptions.Visible = False
        Control = BDefaultGroupSummaryAdd
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem14: TdxLayoutItem
        Parent = dxLayoutGroup25
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BDefaultGroupSummaryDelete'
        CaptionOptions.Visible = False
        Control = BDefaultGroupSummaryDelete
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup2: TdxLayoutGroup
        Parent = dxLayoutGroup7
        CaptionOptions.Text = '   Groups   '
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        Index = 2
      end
      object dxLayoutGroup3: TdxLayoutGroup
        Parent = dxLayoutGroup2
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup4: TdxLayoutGroup
        Parent = dxLayoutGroup3
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 0
      end
      object dxLayoutItem15: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Groups:'
        CaptionOptions.Layout = clTop
        Control = LBSummaryGroups
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup11: TdxLayoutGroup
        Parent = dxLayoutGroup3
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem18: TdxLayoutItem
        Parent = dxLayoutGroup11
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Items:'
        CaptionOptions.Layout = clTop
        Control = LBGroupSummaryItems
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup13: TdxLayoutGroup
        Parent = dxLayoutGroup2
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ItemIndex = 2
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem21: TdxLayoutItem
        Parent = dxLayoutGroup13
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Unlinked Columns:'
        CaptionOptions.Layout = clTop
        Control = LBUnlinkedColumns
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup16: TdxLayoutGroup
        Parent = dxLayoutGroup13
        AlignHorz = ahLeft
        AlignVert = avCenter
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem22: TdxLayoutItem
        Parent = dxLayoutGroup16
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnLink'
        CaptionOptions.Visible = False
        Control = BColumnLink
        ControlOptions.OriginalHeight = 25
        ControlOptions.OriginalWidth = 25
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem23: TdxLayoutItem
        Parent = dxLayoutGroup16
        AlignHorz = ahLeft
        CaptionOptions.Text = 'BColumnUnlink'
        CaptionOptions.Visible = False
        Control = BColumnUnlink
        ControlOptions.OriginalHeight = 25
        ControlOptions.OriginalWidth = 25
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem24: TdxLayoutItem
        Parent = dxLayoutGroup13
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Linked Columns:'
        CaptionOptions.Layout = clTop
        Control = LBLinkedColumns
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 80
        ControlOptions.ShowBorder = False
        Index = 2
      end
      object dxLayoutItem25: TdxLayoutItem
        Parent = dxLayoutGroup9
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Items:'
        CaptionOptions.Layout = clTop
        Control = LBFooterSummary
        ControlOptions.OriginalHeight = 100
        ControlOptions.OriginalWidth = 183
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem9: TdxLayoutItem
        Parent = dxLayoutGroup19
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'Items:'
        CaptionOptions.Layout = clTop
        Control = LBDefaultGroupsSummary
        ControlOptions.OriginalHeight = 216
        ControlOptions.OriginalWidth = 183
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup10: TdxLayoutGroup
        Parent = dxLayoutGroup4
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ItemIndex = 1
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem16: TdxLayoutItem
        Parent = dxLayoutGroup10
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BSummaryGroupAdd'
        CaptionOptions.Visible = False
        Control = BSummaryGroupAdd
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem17: TdxLayoutItem
        Parent = dxLayoutGroup10
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BSummaryGroupDelete'
        CaptionOptions.Visible = False
        Control = BSummaryGroupDelete
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup12: TdxLayoutGroup
        Parent = dxLayoutGroup11
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem19: TdxLayoutItem
        Parent = dxLayoutGroup12
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BGroupSummaryItemAdd'
        CaptionOptions.Visible = False
        Control = BGroupSummaryItemAdd
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem20: TdxLayoutItem
        Parent = dxLayoutGroup12
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BGroupSummaryItemDelete'
        CaptionOptions.Visible = False
        Control = BGroupSummaryItemDelete
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 75
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
        Parent = dxLayoutGroup5
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        Index = 3
      end
      object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
        Parent = dxLayoutGroup5
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        Index = 6
      end
    end
  end
  inherited dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 200
    inherited dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object PMColumns: TPopupMenu
    Left = 228
    Top = 6
    object MIColumnAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BColumnAddClick
    end
    object MIColumnDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BColumnDeleteClick
    end
    object MIColumnRestore: TMenuItem
      Caption = 'Rest&ore Defaults'
      OnClick = BColumnRestoreClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MIColumnMoveUp: TMenuItem
      Caption = 'Move Up'
      ShortCut = 16422
      OnClick = BColumnMoveUpClick
    end
    object MIColumnMoveDown: TMenuItem
      Caption = 'Move Down'
      ShortCut = 16424
      OnClick = BColumnMoveDownClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MIColumnSelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MIColumnSelectAllClick
    end
  end
  object PMFooterSummary: TPopupMenu
    Left = 262
    Top = 2
    object MIFooterSummaryAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BAddFooterSummaryItemClick
    end
    object MIFooterSummaryDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BDeleteFooterSummaryItemClick
    end
    object MenuItem4a: TMenuItem
      Caption = '-'
    end
    object MIFooterSummarySelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MIFooterSummarySelectAllClick
    end
  end
  object PMDefaultGroupsSummary: TPopupMenu
    Left = 278
    Top = 65530
    object MIDefaultGroupSummaryAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BDefaultGroupSummaryAddClick
    end
    object MIDefaultGroupSummaryDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BDefaultGroupSummaryDeleteClick
    end
    object MenuItem3b: TMenuItem
      Caption = '-'
    end
    object MIDefaultGroupSummarySelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MIDefaultGroupSummarySelectAllClick
    end
  end
  object PMSummaryGroups: TPopupMenu
    Left = 286
    Top = 10
    object MISummaryGroupAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BSummaryGroupAddClick
    end
    object MISummaryGroupDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BSummaryGroupDeleteClick
    end
    object MenuItem5d: TMenuItem
      Caption = '-'
    end
    object MISummaryGroupSelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MISummaryGroupSelectAllClick
    end
  end
  object PMGroupSummaryItems: TPopupMenu
    Left = 310
    Top = 10
    object MIGroupSummaryItemsAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BGroupSummaryItemAddClick
    end
    object MIGroupSummaryItemsDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BGroupSummaryItemDeleteClick
    end
    object MenuItem6c: TMenuItem
      Caption = '-'
    end
    object MIGroupSummaryItemsSelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MIGroupSummaryItemsSelectAllClick
    end
  end
end
