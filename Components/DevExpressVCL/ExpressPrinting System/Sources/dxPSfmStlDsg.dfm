object dxfmPrintStylesDesignWindow: TdxfmPrintStylesDesignWindow
  Left = 395
  Top = 187
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Print Styles'
  ClientHeight = 250
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000FFFFFFFFFF000000F00F00000F000000FFF
    FFFFFFF000000F00F00000F000000FFFFFFFFFF000000FFFFFFF0FF000000F00
    FFF080F000000F080F08080000440FF080808088804400000808088888440000
    008088888844000000088888804400000000000000440000000000000000FFFF
    0000000F0000000F0000000F0000000F0000000F0000000F0000000F0000000F
    0000000400000000000000000000F8000000FC000000FE040000FFFF0000}
  KeyPreview = True
  OldCreateOrder = True
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 400
    Height = 250
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lbxStyles: TcxListBox
      Left = 10
      Top = 10
      Width = 264
      Height = 230
      DragMode = dmAutomatic
      ItemHeight = 36
      ListStyle = lbOwnerDrawFixed
      MultiSelect = True
      ParentFont = False
      PopupMenu = pmStyles
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbxStylesClick
      OnDblClick = PageSetupClick
      OnDragDrop = lbxStylesDragDrop
      OnDragOver = lbxStylesDragOver
      OnDrawItem = lbxStylesDrawItem
      OnEndDrag = lbxStylesEndDrag
      OnKeyPress = lbxStylesKeyPress
      OnStartDrag = lbxStylesStartDrag
    end
    object btnAdd: TcxButton
      Left = 280
      Top = 10
      Width = 110
      Height = 22
      Caption = '&Add...'
      TabOrder = 1
      OnClick = AddClick
    end
    object btnDelete: TcxButton
      Tag = 4
      Left = 280
      Top = 38
      Width = 110
      Height = 22
      Caption = '&Delete'
      TabOrder = 2
      OnClick = EditClick
    end
    object btnSelectAll: TcxButton
      Tag = 6
      Left = 280
      Top = 66
      Width = 110
      Height = 22
      Caption = 'Se&lect All'
      TabOrder = 3
      OnClick = EditClick
    end
    object btnMoveUp: TcxButton
      Left = 280
      Top = 94
      Width = 110
      Height = 22
      Caption = 'Move &Up'
      TabOrder = 4
      OnClick = MoveUpClick
    end
    object btnMoveDown: TcxButton
      Tag = 1
      Left = 280
      Top = 122
      Width = 110
      Height = 22
      Caption = 'Move Dow&n'
      TabOrder = 5
      OnClick = MoveDownClick
    end
    object btnPageSetup: TcxButton
      Tag = 1
      Left = 280
      Top = 150
      Width = 110
      Height = 22
      Caption = 'Pa&ge Setup...'
      TabOrder = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = PageSetupClick
    end
    object btnRestoreDefaults: TcxButton
      Left = 280
      Top = 178
      Width = 110
      Height = 22
      Caption = 'Rest&ore Defaults'
      TabOrder = 7
      OnClick = RestoreDefaultsClick
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
    object dxLayoutItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbxStyles
      ControlOptions.OriginalHeight = 191
      ControlOptions.OriginalWidth = 228
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object pnlButtons: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnSelectAll'
      CaptionOptions.Visible = False
      Control = btnSelectAll
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveUp'
      CaptionOptions.Visible = False
      Control = btnMoveUp
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnMoveDown'
      CaptionOptions.Visible = False
      Control = btnMoveDown
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnPageSetup'
      CaptionOptions.Visible = False
      Control = btnPageSetup
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnRestoreDefaults'
      CaptionOptions.Visible = False
      Control = btnRestoreDefaults
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 6
    end
  end
  object pmStyles: TPopupMenu
    Images = ilMenu
    OnPopup = pmStylesPopup
    Left = 13
    Top = 9
    object miAdd: TMenuItem
      Caption = '&Add...'
      ImageIndex = 0
      ShortCut = 45
      OnClick = AddClick
    end
    object miAddStandard: TMenuItem
      Caption = 'Add'
      ShortCut = 32813
      OnClick = AddStandardClick
    end
    object miLine5: TMenuItem
      Caption = '-'
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      object miCut: TMenuItem
        Tag = 2
        Caption = 'Cu&t'
        ImageIndex = 3
        ShortCut = 16472
        OnClick = EditClick
      end
      object miCopy: TMenuItem
        Tag = 1
        Caption = '&Copy'
        ImageIndex = 2
        ShortCut = 16451
        OnClick = EditClick
      end
      object miPaste: TMenuItem
        Tag = 3
        Caption = '&Paste'
        ImageIndex = 4
        ShortCut = 16470
        OnClick = EditClick
      end
      object miDelete: TMenuItem
        Tag = 4
        Caption = '&Delete'
        ImageIndex = 5
        ShortCut = 46
        OnClick = EditClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miSelectAll: TMenuItem
        Tag = 6
        Caption = 'Se&lect All'
        ShortCut = 16449
        OnClick = EditClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miMoveUp: TMenuItem
      Caption = 'Move &Up'
      ImageIndex = 10
      ShortCut = 16422
      OnClick = MoveUpClick
    end
    object miMoveDown: TMenuItem
      Caption = 'Move Dow&n'
      ImageIndex = 11
      ShortCut = 16424
      OnClick = MoveDownClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miPageSetup: TMenuItem
      Tag = 1
      Caption = 'Pa&ge Setup...'
      Default = True
      ImageIndex = 7
      ShortCut = 16397
      OnClick = PageSetupClick
    end
    object miSetAsCurrent: TMenuItem
      Caption = '&Make Current'
      ShortCut = 16416
      OnClick = miSetAsCurrentClick
    end
    object miLine: TMenuItem
      Caption = '-'
    end
    object miRestoreDefaults: TMenuItem
      Caption = 'Rest&ore Defaults'
      ShortCut = 16463
      OnClick = RestoreDefaultsClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miBackground: TMenuItem
      Caption = 'Bac&kground'
      object miBackgroundEffects: TMenuItem
        Tag = 4
        Caption = 'E&ffects...'
        ImageIndex = 12
        OnClick = BackgroundClick
      end
      object miBackgroundClear: TMenuItem
        Tag = 5
        Caption = 'Clear'
        OnClick = ClearBackgroundClick
      end
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miShowButtons: TMenuItem
      Caption = 'Show &Button Bar'
      Checked = True
      OnClick = miShowButtonsClick
    end
  end
  object ilMenu: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 589865
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 72
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
