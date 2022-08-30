inherited cxBandedTableViewEditor: TcxBandedTableViewEditor
  Left = 477
  Caption = 'cxBandedTableViewEditor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PViewEditor: TPanel
    inherited lcMain: TdxLayoutControl
      object BAddBand: TcxButton [24]
        Left = 250
        Top = 44
        Width = 120
        Height = 24
        Caption = '&Add'
        TabOrder = 1
        OnClick = BAddBandClick
      end
      object BBandMoveDown: TcxButton [25]
        Left = 250
        Top = 150
        Width = 120
        Height = 24
        Caption = 'Move &Down'
        TabOrder = 4
        OnClick = BBandMoveDownClick
      end
      object BDeleteBand: TcxButton [26]
        Left = 250
        Top = 74
        Width = 120
        Height = 24
        Caption = 'Delete'
        TabOrder = 2
        OnClick = BDeleteBandClick
      end
      object LBBands: TcxListBox [27]
        Left = 20
        Top = 44
        Width = 224
        Height = 236
        DragMode = dmAutomatic
        ItemHeight = 16
        ListStyle = lbOwnerDrawFixed
        MultiSelect = True
        PopupMenu = PMBands
        Style.TransparentBorder = False
        TabOrder = 0
        OnClick = LBBandsClick
        OnDragDrop = LBBandsDragDrop
        OnDragOver = LBBandsDragOver
        OnDrawItem = LBBandsDrawItem
        OnEndDrag = LBBandsEndDrag
        OnKeyPress = FormKeyPress
        OnStartDrag = LBBandsStartDrag
      end
      object BBandMoveUp: TcxButton [28]
        Left = 250
        Top = 120
        Width = 120
        Height = 24
        Caption = 'Move &Up'
        TabOrder = 3
        OnClick = BBandMoveUpClick
      end
      inherited lcMainGroup_Root: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited lgItems: TdxLayoutGroup
        ItemIndex = 1
        Index = 1
      end
      inherited dxLayoutGroup5: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited lgSummary: TdxLayoutGroup
        Index = 2
      end
      inherited dxLayoutGroup7: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup9: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup15: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup19: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup25: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup3: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup4: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup11: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup13: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup16: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup10: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      inherited dxLayoutGroup12: TdxLayoutGroup
        CaptionOptions.Visible = False
      end
      object lgBands: TdxLayoutGroup
        Parent = lcMainGroup_Root
        CaptionOptions.Text = 'Bands'
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        LayoutDirection = ldHorizontal
        Index = 0
      end
      object dxLayoutItem26: TdxLayoutItem
        Parent = dxLayoutAutoCreatedGroup1
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'BAddBand'
        CaptionOptions.Visible = False
        Control = BAddBand
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 120
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem27: TdxLayoutItem
        Parent = dxLayoutAutoCreatedGroup1
        CaptionOptions.Text = 'BBandMoveDown'
        CaptionOptions.Visible = False
        Control = BBandMoveDown
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 4
      end
      object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
        Parent = lgBands
        AlignHorz = ahRight
        Index = 1
        AutoCreated = True
      end
      object dxLayoutItem29: TdxLayoutItem
        Parent = dxLayoutAutoCreatedGroup1
        CaptionOptions.Text = 'BDeleteBand'
        CaptionOptions.Visible = False
        Control = BDeleteBand
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
        Parent = dxLayoutAutoCreatedGroup1
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        Index = 2
      end
      object dxLayoutItem30: TdxLayoutItem
        Parent = lgBands
        AlignHorz = ahClient
        AlignVert = avClient
        Control = LBBands
        ControlOptions.OriginalHeight = 100
        ControlOptions.OriginalWidth = 100
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem28: TdxLayoutItem
        Parent = dxLayoutAutoCreatedGroup1
        CaptionOptions.Text = 'BBandMoveUp'
        CaptionOptions.Visible = False
        Control = BBandMoveUp
        ControlOptions.OriginalHeight = 24
        ControlOptions.OriginalWidth = 110
        ControlOptions.ShowBorder = False
        Index = 3
      end
    end
  end
  inherited dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object PMBands: TPopupMenu
    Left = 262
    Top = 82
    object MIBandsAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BAddBandClick
    end
    object MIBandsDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BDeleteBandClick
    end
    object MenuItem4: TMenuItem
      Caption = '-'
    end
    object MIBandsMoveUp: TMenuItem
      Caption = 'Move Up'
      ShortCut = 16422
      OnClick = BBandMoveUpClick
    end
    object MIBandsMoveDown: TMenuItem
      Caption = 'Move Down'
      ShortCut = 16424
      OnClick = BBandMoveDownClick
    end
    object MenuItem7: TMenuItem
      Caption = '-'
    end
    object MIBandsSelectAll: TMenuItem
      Caption = '&Select All'
      ShortCut = 16449
      OnClick = MIBandsSelectAllClick
    end
  end
end
