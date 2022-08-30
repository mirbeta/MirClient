inherited cxStyleRepositoryEditor: TcxStyleRepositoryEditor
  Left = 333
  Top = 185
  Caption = 'StyleRepository editor'
  ClientHeight = 374
  ClientWidth = 338
  Constraints.MinHeight = 350
  Constraints.MinWidth = 300
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 322
    Height = 358
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = tsStyles
    Align = alClient
    TabOrder = 0
    object tsStyles: TTabSheet
      Caption = 'Styles'
      object lbStyles: TListBox
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 199
        Height = 314
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmStyles
        TabOrder = 0
        OnClick = lbStylesClick
      end
      object pnlStyles: TPanel
        AlignWithMargins = True
        Left = 215
        Top = 8
        Width = 91
        Height = 314
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object btStyleAdd: TButton
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 91
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Add'
          TabOrder = 0
          OnClick = btStyleAddClick
        end
        object btStyleDelete: TButton
          AlignWithMargins = True
          Left = 0
          Top = 31
          Width = 91
          Height = 25
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Delete'
          TabOrder = 1
          OnClick = btStyleDeleteClick
        end
        object btClose: TButton
          Left = 0
          Top = 288
          Width = 91
          Height = 26
          Align = alBottom
          Caption = '&Close'
          TabOrder = 2
          OnClick = btCloseClick
        end
      end
    end
    object tsStyleSheets: TTabSheet
      Caption = 'Style Sheets'
      ImageIndex = 1
      object lbStyleSheets: TListBox
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 190
        Height = 314
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmStyleSheets
        TabOrder = 0
        OnClick = lbStyleSheetsClick
      end
      object pnlStyleSheets: TPanel
        AlignWithMargins = True
        Left = 206
        Top = 8
        Width = 100
        Height = 314
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object btStyleSheetAdd: TButton
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Add...'
          TabOrder = 0
          OnClick = btStyleSheetAddClick
        end
        object btStyleSheetDelete: TButton
          AlignWithMargins = True
          Left = 0
          Top = 31
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Delete'
          TabOrder = 1
          OnClick = btStyleSheetDeleteClick
        end
        object Button3: TButton
          Left = 0
          Top = 288
          Width = 100
          Height = 26
          Align = alBottom
          Caption = '&Close'
          TabOrder = 2
          OnClick = btCloseClick
        end
        object btnStyleSheetEdit: TButton
          AlignWithMargins = True
          Left = 0
          Top = 62
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Edit...'
          TabOrder = 3
          OnClick = btnStyleSheetEditClick
        end
        object btnStyleSheetsSave: TButton
          AlignWithMargins = True
          Left = 0
          Top = 102
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Top = 12
          Margins.Right = 0
          Align = alTop
          Caption = '&Save to ini...'
          TabOrder = 4
          OnClick = btnStyleSheetsSaveClick
        end
        object btnStyleSheetsLoad: TButton
          AlignWithMargins = True
          Left = 0
          Top = 133
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          Caption = '&Load from ini...'
          TabOrder = 5
          OnClick = btnStyleSheetsLoadClick
        end
        object btnStyleSheetsPredefine: TButton
          AlignWithMargins = True
          Left = 0
          Top = 173
          Width = 100
          Height = 25
          Margins.Left = 0
          Margins.Top = 12
          Margins.Right = 0
          Align = alTop
          Caption = '&Predefined...'
          TabOrder = 6
          OnClick = btnStyleSheetsPredefineClick
        end
      end
    end
  end
  object pmStyles: TPopupMenu
    Left = 40
    Top = 88
    object miStyleAdd: TMenuItem
      Caption = 'Add'
      ShortCut = 45
      OnClick = btStyleAddClick
    end
    object miStyleDelete: TMenuItem
      Caption = 'Delete'
      Enabled = False
      ShortCut = 46
      OnClick = btStyleDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miStyleSelectAll: TMenuItem
      Caption = 'Select all'
      Enabled = False
      ShortCut = 16449
      OnClick = miStyleSelectAllClick
    end
  end
  object pmStyleSheets: TPopupMenu
    Left = 72
    Top = 168
    object miStyleSheetAdd: TMenuItem
      Caption = 'Add...'
      ShortCut = 45
      OnClick = btStyleSheetAddClick
    end
    object miStyleSheetDelete: TMenuItem
      Caption = 'Delete'
      Enabled = False
      ShortCut = 46
      OnClick = btStyleSheetDeleteClick
    end
    object imStyleSheetEdit: TMenuItem
      Caption = 'Edit...'
      OnClick = btnStyleSheetEditClick
    end
    object MenuItem3: TMenuItem
      Caption = '-'
    end
    object miStyleSheetSelectAll: TMenuItem
      Caption = 'Select all'
      Enabled = False
      ShortCut = 16449
      OnClick = miStyleSheetSelectAllClick
    end
  end
  object pmAddStyleSheet: TPopupMenu
    Left = 128
    Top = 48
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'ini'
    FileName = 'cxstyles.ini'
    Filter = 'Ini files|*.ini'
    Left = 160
    Top = 120
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ini'
    FileName = 'cxstyles.ini'
    Filter = 'Ini files|*.ini'
    Left = 160
    Top = 176
  end
end
