inherited cxEditRepositoryEditor: TcxEditRepositoryEditor
  Left = 363
  Top = 194
  Caption = 'EditRepository editor'
  ClientHeight = 378
  ClientWidth = 337
  Constraints.MinHeight = 200
  Constraints.MinWidth = 350
  PopupMenu = PopupMenu1
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LBItems: TListBox
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 221
    Height = 358
    Margins.Left = 10
    Margins.Top = 10
    Margins.Bottom = 10
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 0
    OnClick = LBItemsClick
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 237
    Top = 10
    Width = 90
    Height = 358
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btAdd: TButton
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 90
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      Caption = '&Add...'
      TabOrder = 0
      OnClick = btAddClick
    end
    object btDelete: TButton
      AlignWithMargins = True
      Left = 0
      Top = 31
      Width = 90
      Height = 25
      Margins.Left = 0
      Margins.Right = 0
      Align = alTop
      Caption = '&Delete'
      TabOrder = 1
      OnClick = btDeleteClick
    end
    object btClose: TButton
      AlignWithMargins = True
      Left = 0
      Top = 333
      Width = 90
      Height = 25
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      Caption = '&Close'
      TabOrder = 2
      OnClick = btCloseClick
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 16
    object miAdd: TMenuItem
      Caption = 'Add'
      ShortCut = 45
      OnClick = miAddClick
    end
    object miDelete: TMenuItem
      Caption = 'Delete'
      Enabled = False
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Caption = 'Select all'
      Enabled = False
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
end
