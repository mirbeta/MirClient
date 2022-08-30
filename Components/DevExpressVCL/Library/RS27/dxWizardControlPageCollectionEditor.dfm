object dxWizardControlPageCollectionEditorForm: TdxWizardControlPageCollectionEditorForm
  Left = 219
  Top = 160
  Caption = 'Page Collection Editor'
  ClientHeight = 393
  ClientWidth = 402
  Color = clBtnFace
  Constraints.MinHeight = 340
  Constraints.MinWidth = 310
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbPages: TListBox
    Left = 10
    Top = 8
    Width = 301
    Height = 375
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = pmPagesMenu
    TabOrder = 0
    OnClick = lbPagesClick
  end
  object btnOK: TButton
    Left = 317
    Top = 327
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 317
    Top = 358
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object btnMoveUp: TButton
    Left = 317
    Top = 76
    Width = 75
    Height = 25
    Action = acMoveUp
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object btnMoveDown: TButton
    Left = 317
    Top = 107
    Width = 75
    Height = 25
    Action = acMoveDown
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object btnAdd: TButton
    Left = 317
    Top = 8
    Width = 75
    Height = 25
    Action = acAdd
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object btnDelete: TButton
    Left = 317
    Top = 39
    Width = 75
    Height = 25
    Action = acDelete
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object pmPagesMenu: TPopupMenu
    Left = 16
    Top = 32
    object miAdd: TMenuItem
      Action = acAdd
    end
    object miDelete: TMenuItem
      Action = acDelete
    end
    object miSeparator: TMenuItem
      Caption = '-'
    end
    object miMoveUp: TMenuItem
      Action = acMoveUp
    end
    object miMoveDown: TMenuItem
      Action = acMoveDown
    end
  end
  object alActions: TActionList
    Left = 48
    Top = 32
    object acAdd: TAction
      Caption = '&Add'
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = '&Delete'
      Enabled = False
      OnExecute = acDeleteExecute
    end
    object acMoveUp: TAction
      Caption = 'Move &Up'
      Enabled = False
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move Dow&n'
      Enabled = False
      OnExecute = acMoveDownExecute
    end
  end
end
