object dxfmReportLinkDesignWindow: TdxfmReportLinkDesignWindow
  Left = 524
  Top = 253
  BorderStyle = bsDialog
  Caption = 'Report Links'
  ClientHeight = 360
  ClientWidth = 540
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
    Width = 540
    Height = 360
    Align = alClient
    Constraints.MinHeight = 360
    Constraints.MinWidth = 520
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lbxLinks: TcxListBox
      Left = 10
      Top = 10
      Width = 393
      Height = 340
      DragMode = dmAutomatic
      ItemHeight = 13
      ListStyle = lbOwnerDrawVariable
      MultiSelect = True
      PopupMenu = pmLinks
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbxLinksClick
      OnDblClick = lbxLinksDblClick
      OnDragDrop = lbxLinksDragDrop
      OnDragOver = lbxLinksDragOver
      OnDrawItem = lbxLinksDrawItem
      OnEndDrag = lbxLinksEndDrag
      OnKeyPress = lbxLinksKeyPress
      OnMeasureItem = lbxLinksMeasureItem
      OnStartDrag = lbxLinksStartDrag
    end
    object btnAdd: TcxButton
      Left = 409
      Top = 10
      Width = 121
      Height = 22
      Caption = '&Add...'
      DropDownMenu = pmAdd
      Kind = cxbkDropDownButton
      TabOrder = 1
      OnClick = AddClick
    end
    object btnDelete: TcxButton
      Tag = 4
      Left = 409
      Top = 38
      Width = 121
      Height = 22
      Caption = '&Delete'
      TabOrder = 2
      OnClick = EditClick
    end
    object btnSelectAll: TcxButton
      Tag = 6
      Left = 409
      Top = 66
      Width = 121
      Height = 22
      Caption = 'Se&lect All'
      TabOrder = 3
      OnClick = EditClick
    end
    object btnMoveUp: TcxButton
      Left = 409
      Top = 94
      Width = 121
      Height = 22
      Caption = 'Move &Up'
      TabOrder = 4
      OnClick = MoveUpClick
    end
    object btnMoveDown: TcxButton
      Tag = 1
      Left = 409
      Top = 122
      Width = 121
      Height = 22
      Caption = 'Move Dow&n'
      TabOrder = 5
      OnClick = MoveDownClick
    end
    object btnShowDesigner: TcxButton
      Left = 409
      Top = 150
      Width = 121
      Height = 22
      Caption = 'Show D&esigner...'
      TabOrder = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = LinkDesignClick
    end
    object btnChangeComponent: TcxButton
      Tag = 3
      Left = 409
      Top = 178
      Width = 121
      Height = 22
      Caption = 'C&hange Component...'
      TabOrder = 7
      OnClick = LinkChangeComponentClick
    end
    object btnRestoreDefaults: TcxButton
      Left = 409
      Top = 206
      Width = 121
      Height = 22
      Caption = 'Rest&ore Defaults'
      TabOrder = 8
      OnClick = RestoreDefaultsClick
    end
    object btnRestoreOriginal: TcxButton
      Left = 409
      Top = 234
      Width = 121
      Height = 22
      Caption = 'Rest&ore Or&iginal'
      TabOrder = 9
      OnClick = RestoreOriginalClick
    end
    object btnPageSetup: TcxButton
      Tag = 1
      Left = 409
      Top = 262
      Width = 121
      Height = 22
      Caption = 'Pa&ge Setup...'
      TabOrder = 10
      OnClick = PageSetupClick
    end
    object btnPrintPreview: TcxButton
      Tag = 2
      Left = 409
      Top = 290
      Width = 121
      Height = 22
      Caption = 'Pre&view...'
      TabOrder = 11
      OnClick = PrintPreviewClick
    end
    object btnPrint: TcxButton
      Tag = 3
      Left = 409
      Top = 318
      Width = 121
      Height = 22
      Caption = 'Print...'
      TabOrder = 12
      OnClick = PrintClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avParentManaged
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
      Control = lbxLinks
      ControlOptions.OriginalHeight = 331
      ControlOptions.OriginalWidth = 364
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
      ControlOptions.OriginalWidth = 121
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
      ControlOptions.OriginalWidth = 121
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
      ControlOptions.OriginalWidth = 121
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
      ControlOptions.OriginalWidth = 121
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
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnShowDesigner'
      CaptionOptions.Visible = False
      Control = btnShowDesigner
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnChangeComponent'
      CaptionOptions.Visible = False
      Control = btnChangeComponent
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnRestoreDefaults'
      CaptionOptions.Visible = False
      Control = btnRestoreDefaults
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnRestoreOriginal'
      CaptionOptions.Visible = False
      Control = btnRestoreOriginal
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnPageSetup'
      CaptionOptions.Visible = False
      Control = btnPageSetup
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnPrintPreview'
      CaptionOptions.Visible = False
      Control = btnPrintPreview
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = pnlButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnPrint'
      CaptionOptions.Visible = False
      Control = btnPrint
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 11
    end
  end
  object pmLinks: TPopupMenu
    Images = ilLinks
    OnPopup = pmLinksPopup
    Left = 7
    Top = 6
    object miAdd: TMenuItem
      Caption = '&Add...'
      ImageIndex = 0
      ShortCut = 45
      OnClick = AddClick
    end
    object miAddComposition: TMenuItem
      Caption = 'Add Composition...'
      ImageIndex = 1
      OnClick = AddCompositionClick
    end
    object miLine7: TMenuItem
      Caption = '-'
    end
    object miAddExisting: TMenuItem
      Caption = 'Add Existing'
      ShortCut = 32813
      OnClick = AddExistingClick
    end
    object miAddStandard: TMenuItem
      Caption = 'Add Standard Link...'
      ShortCut = 16429
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
      object N3: TMenuItem
        Caption = '-'
      end
      object miSelectAll: TMenuItem
        Tag = 6
        Caption = 'Se&lect All'
        ShortCut = 16449
        OnClick = EditClick
      end
    end
    object N2: TMenuItem
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
    object miShowDesigner: TMenuItem
      Caption = 'Show Designer...'
      Default = True
      ImageIndex = 6
      ShortCut = 16397
      OnClick = LinkDesignClick
    end
    object miSetAsCurrent: TMenuItem
      Caption = '&Make Current'
      ShortCut = 16416
      OnClick = SetAsCurrentClick
    end
    object miChangeComponent: TMenuItem
      Caption = 'C&hange Component...'
      OnClick = LinkChangeComponentClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miRestoreDefaults: TMenuItem
      Caption = 'Rest&ore Defaults'
      ShortCut = 16463
      OnClick = RestoreDefaultsClick
    end
    object miRestoreOriginal: TMenuItem
      Caption = 'Rest&ore Or&iginal'
      ShortCut = 16457
      OnClick = RestoreOriginalClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miPageSetup: TMenuItem
      Tag = 1
      Caption = 'Pa&ge Setup...'
      ImageIndex = 7
      OnClick = PageSetupClick
    end
    object miPrintPreview: TMenuItem
      Tag = 2
      Caption = 'Print Pre&view...'
      ImageIndex = 8
      OnClick = PrintPreviewClick
    end
    object miPrint: TMenuItem
      Tag = 3
      Caption = '&Print...'
      ImageIndex = 9
      ShortCut = 16464
      OnClick = PrintClick
    end
    object N1: TMenuItem
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
    object miLine: TMenuItem
      Caption = '-'
    end
    object miShowButtons: TMenuItem
      Caption = 'Show &Button Bar'
      Checked = True
      OnClick = ShowButtonsClick
    end
  end
  object pmAdd: TPopupMenu
    Images = ilLinks
    OnPopup = pmLinksPopup
    Left = 35
    Top = 6
    object miAdd1: TMenuItem
      Caption = '&Add...'
      Default = True
      ImageIndex = 0
      ShortCut = 45
      OnClick = AddClick
    end
    object miAddComposition1: TMenuItem
      Caption = 'Add Composition...'
      Hint = 'miAddComposition'
      ImageIndex = 1
      OnClick = AddCompositionClick
    end
    object miLine6: TMenuItem
      Caption = '-'
    end
    object miAddExisting1: TMenuItem
      Caption = 'Add Existing'
      ShortCut = 32813
      OnClick = AddExistingClick
    end
    object miAddStandard1: TMenuItem
      Caption = 'Add Standard...'
      ShortCut = 16429
      OnClick = AddStandardClick
    end
  end
  object ilLinks: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 393279
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 96
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
