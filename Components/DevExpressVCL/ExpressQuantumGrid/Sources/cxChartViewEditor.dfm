inherited cxChartViewEditor: TcxChartViewEditor
  Left = 543
  Top = 157
  Caption = 'cxChartViewEditor'
  ClientHeight = 240
  ClientWidth = 300
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited PViewEditor: TPanel
    Width = 300
    Height = 240
    Constraints.MinHeight = 240
    Constraints.MinWidth = 300
    inherited lcMain: TdxLayoutControl
      Width = 300
      Height = 240
      AutoSize = True
      object tcMain: TcxTabControl [0]
        Left = 10
        Top = 10
        Width = 280
        Height = 25
        TabOrder = 0
        Properties.CustomButtons.Buttons = <>
        Properties.Tabs.Strings = (
          '  Series  '
          '  DataGroups  ')
        OnChange = tcMainChange
        OnChanging = tcMainChanging
        ClientRectBottom = 25
        ClientRectRight = 280
        ClientRectTop = 24
      end
      object lbItems: TcxListBox [1]
        Left = 20
        Top = 44
        Width = 134
        Height = 183
        DragMode = dmAutomatic
        ItemHeight = 13
        ListStyle = lbOwnerDrawFixed
        MultiSelect = True
        PopupMenu = pmItems
        Style.TransparentBorder = False
        TabOrder = 1
        OnClick = lbItemsClick
        OnDragDrop = lbItemsDragDrop
        OnDragOver = lbItemsDragOver
        OnDrawItem = lbItemsDrawItem
        OnEndDrag = lbItemsEndDrag
        OnKeyPress = FormKeyPress
        OnStartDrag = lbItemsStartDrag
      end
      object btnItemAdd: TcxButton [2]
        Left = 160
        Top = 44
        Width = 120
        Height = 24
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnItemAddClick
      end
      object btnItemDelete: TcxButton [3]
        Left = 160
        Top = 74
        Width = 120
        Height = 24
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btnItemDeleteClick
      end
      object btnItemMoveUp: TcxButton [4]
        Left = 160
        Top = 120
        Width = 120
        Height = 24
        Caption = 'Move &Up'
        TabOrder = 4
        OnClick = btnItemMoveUpClick
      end
      object btnItemMoveDown: TcxButton [5]
        Left = 160
        Top = 150
        Width = 120
        Height = 24
        Caption = 'Move &Down'
        TabOrder = 5
        OnClick = btnItemMoveDownClick
      end
      object btnItemSelectAll: TcxButton [6]
        Left = 160
        Top = 196
        Width = 120
        Height = 24
        Caption = 'Select All'
        TabOrder = 6
        OnClick = btnItemSelectAllClick
      end
      inherited lcMainGroup_Root: TdxLayoutGroup
        ItemIndex = 1
      end
      object dxLayoutItem7: TdxLayoutItem
        Parent = lcMainGroup_Root
        Control = tcMain
        ControlOptions.AutoColor = True
        ControlOptions.OriginalHeight = 25
        ControlOptions.OriginalWidth = 324
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup2: TdxLayoutGroup
        Parent = lcMainGroup_Root
        AlignVert = avClient
        CaptionOptions.Text = 'New Group'
        Offsets.Bottom = 3
        Offsets.Left = 10
        Offsets.Right = 10
        Offsets.Top = 3
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup2
        AlignHorz = ahClient
        AlignVert = avClient
        Control = lbItems
        ControlOptions.OriginalHeight = 100
        ControlOptions.OriginalWidth = 113
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup1: TdxLayoutGroup
        Parent = dxLayoutGroup2
        AlignHorz = ahRight
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ItemIndex = 5
        ShowBorder = False
        Index = 1
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        CaptionOptions.Text = 'btnItemAdd'
        CaptionOptions.Visible = False
        Control = btnItemAdd
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem3: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        CaptionOptions.Text = 'btnItemDelete'
        CaptionOptions.Visible = False
        Control = btnItemDelete
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutItem4: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        CaptionOptions.Text = 'btnItemMoveUp'
        CaptionOptions.Visible = False
        Control = btnItemMoveUp
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 3
      end
      object dxLayoutItem5: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        CaptionOptions.Text = 'btnItemMoveDown'
        CaptionOptions.Visible = False
        Control = btnItemMoveDown
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 4
      end
      object dxLayoutItem6: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        CaptionOptions.Text = 'btnItemSelectAll'
        CaptionOptions.Visible = False
        Control = btnItemSelectAll
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 6
      end
      object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
        Parent = dxLayoutGroup1
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        Index = 2
      end
      object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
        Parent = dxLayoutGroup1
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        Index = 5
      end
    end
  end
  inherited dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 16
    Top = 48
    inherited dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object pmItems: TPopupMenu
    Left = 140
    Top = 74
    object miItemAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = btnItemAddClick
    end
    object miItemDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = btnItemDeleteClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miItemMoveUp: TMenuItem
      Caption = 'Move Up'
      ShortCut = 16422
      OnClick = btnItemMoveUpClick
    end
    object miItemMoveDown: TMenuItem
      Caption = 'Move Down'
      ShortCut = 16424
      OnClick = btnItemMoveDownClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miItemSelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = btnItemSelectAllClick
    end
  end
  object pmItemsAdd: TPopupMenu
    Left = 140
    Top = 112
  end
end
