object appRBE: TappRBE
  Left = 321
  Top = 229
  Caption = 'ROM BIOS Explorer'
  ClientHeight = 654
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GridPanel: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 355
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 2
    Caption = 'Loading...please wait...'
    TabOrder = 0
    ExplicitHeight = 234
    object sghex: TStringGrid
      Left = 3
      Top = 3
      Width = 401
      Height = 349
      Align = alLeft
      BorderStyle = bsNone
      Color = clWhite
      ColCount = 16
      DefaultColWidth = 20
      DefaultRowHeight = 15
      DrawingStyle = gdsClassic
      RowCount = 2
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goDrawFocusSelected, goTabs, goThumbTracking]
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnEnter = sgEnter
      OnExit = sgExit
      OnKeyDown = sghexKeyDown
      OnSelectCell = sghexSelectCell
      OnTopLeftChanged = sghexTopLeftChanged
      ExplicitHeight = 228
      ColWidths = (
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20)
    end
    object sgchar: TStringGrid
      Left = 404
      Top = 3
      Width = 244
      Height = 349
      Align = alClient
      BorderStyle = bsNone
      Color = clWhite
      ColCount = 15
      DefaultColWidth = 14
      DefaultRowHeight = 15
      DrawingStyle = gdsClassic
      FixedCols = 0
      RowCount = 2
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goDrawFocusSelected, goTabs, goThumbTracking]
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      OnEnter = sgEnter
      OnExit = sgExit
      OnKeyDown = sgcharKeyDown
      OnSelectCell = sgcharSelectCell
      OnTopLeftChanged = sgcharTopLeftChanged
      ExplicitHeight = 228
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 355
    Width = 651
    Height = 280
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Panel29: TPanel
      Left = 230
      Top = 0
      Width = 421
      Height = 280
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 2
      TabOrder = 0
      ExplicitHeight = 258
      object lvTables: TListView
        Left = 3
        Top = 21
        Width = 415
        Height = 256
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Table Type'
            Width = 240
          end
          item
            Alignment = taRightJustify
            Caption = 'Length'
            Width = 45
          end
          item
            Alignment = taRightJustify
            Caption = 'Handle'
            Width = 45
          end
          item
            Alignment = taRightJustify
            Caption = 'Address'
            Width = 65
          end>
        ColumnClick = False
        FlatScrollBars = True
        HideSelection = False
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvTablesDblClick
        OnEnter = lvTablesEnter
        OnExit = lvTablesExit
        ExplicitHeight = 234
      end
      object stPanel: TPanel
        Left = 3
        Top = 3
        Width = 415
        Height = 18
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '  Structure Tables'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 1
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 230
      Height = 280
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 2
      Caption = ' '
      TabOrder = 1
      ExplicitHeight = 258
      object Panel4: TPanel
        Left = 3
        Top = 3
        Width = 224
        Height = 18
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '  Inspector'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object sgVals: TStringGrid
        Left = 3
        Top = 21
        Width = 224
        Height = 256
        Align = alClient
        BorderStyle = bsNone
        Color = clWhite
        ColCount = 2
        DefaultColWidth = 100
        DefaultRowHeight = 18
        RowCount = 14
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goThumbTracking]
        ScrollBars = ssNone
        TabOrder = 1
        OnDblClick = sgValsDblClick
        OnEnter = sgEnter
        OnExit = sgExit
        ExplicitHeight = 234
        RowHeights = (
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18
          18)
      end
    end
  end
  object sb: TStatusBar
    Left = 0
    Top = 635
    Width = 651
    Height = 19
    Panels = <
      item
        Text = 'BIOS Size'
        Width = 125
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
    ExplicitTop = 492
  end
  object MainMenu: TMainMenu
    Left = 337
    Top = 91
    object File1: TMenuItem
      Caption = 'File'
      object mmOpen: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = mmOpenClick
      end
      object mmSave: TMenuItem
        Caption = 'Save...'
        ShortCut = 16467
        OnClick = cmSaveDump
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mmSaveRawMemory: TMenuItem
        Caption = 'Save Raw Memory...'
        OnClick = mmSaveRawMemoryClick
      end
      object mmXML: TMenuItem
        Caption = 'Save to Storage...'
        OnClick = mmXMLClick
      end
      object N1: TMenuItem
        Caption = '-'
        Visible = False
      end
      object ReloadLocalMemory1: TMenuItem
        Caption = 'Reload Local Memory'
        ShortCut = 16466
        OnClick = ReloadLocalMemory1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Details1: TMenuItem
        Caption = 'Details...'
        ShortCut = 16452
        OnClick = Details1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Search1: TMenuItem
      Caption = 'Search'
      object GotoAddress1: TMenuItem
        Caption = 'Go to Address...'
        ShortCut = 16455
        OnClick = GotoAddress1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mmFindText: TMenuItem
        Caption = 'Find Text...'
        ShortCut = 16454
        OnClick = mmFindTextClick
      end
      object mmFindSeq: TMenuItem
        Caption = 'Find Sequence...'
        ShortCut = 16465
        OnClick = mmFindSeqClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Findnext1: TMenuItem
        Caption = 'Find next'
        ShortCut = 114
        OnClick = Findnext1Click
      end
    end
    object About1: TMenuItem
      Caption = 'About...'
      OnClick = About1Click
    end
  end
  object fd: TFindDialog
    OnShow = fdShow
    Options = [frDown, frHideWholeWord, frHideUpDown]
    OnFind = fdFind
    Left = 152
    Top = 113
  end
  object od: TOpenDialog
    DefaultExt = 'smbios'
    Filter = 'SMBIOS dumps|*.smbios|Raw Memory dumps|*.bin|All files|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 74
    Top = 142
  end
  object ed: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'MiTeC System Information Storage files|*.sif|All files|*.*'
    Left = 169
    Top = 61
  end
  object sd: TSaveDialog
    DefaultExt = 'smbios'
    Filter = 'SMBIOS dumps|*.smbios|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 30
    Top = 73
  end
  object rd: TSaveDialog
    DefaultExt = 'bin'
    Filter = 'Raw Memory dumps|*.bin|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 93
    Top = 71
  end
end
