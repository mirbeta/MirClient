object dxServerModeDataSourceEditor: TdxServerModeDataSourceEditor
  Left = 206
  Top = 368
  Width = 153
  Height = 240
  object lbMain: TcxCustomizeListBox
    Left = 0
    Top = 0
    Width = 137
    Height = 202
    Align = alClient
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = pmMain
    TabOrder = 0
    OnClick = lbMainClick
    OnDragDrop = lbMainDragDrop
    OnDragOver = lbMainDragOver
    OnEndDrag = lbMainEndDrag
    OnStartDrag = lbMainStartDrag
  end
  object pmMain: TPopupMenu
    Left = 42
    Top = 32
    object miAdd: TMenuItem
      Caption = '&Add fields...'
      ShortCut = 16449
      OnClick = miAddClick
    end
    object miNew: TMenuItem
      Caption = '&New field...'
      ShortCut = 16462
      Visible = False
      OnClick = miNewClick
    end
    object miAddAll: TMenuItem
      Caption = 'Add all &fields'
      ShortCut = 16454
      OnClick = miAddAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miCut: TMenuItem
      Caption = 'Cu&t'
      ShortCut = 16472
      OnClick = miCutClick
    end
    object miCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = miCopyClick
    end
    object miPaste: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = miPasteClick
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object miSelectAll: TMenuItem
      Caption = 'Se&lect all'
      ShortCut = 16460
      OnClick = miSelectAllClick
    end
  end
end
