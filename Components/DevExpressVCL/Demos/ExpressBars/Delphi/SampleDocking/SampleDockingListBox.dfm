object SampleDockingListBoxFrame: TSampleDockingListBoxFrame
  Left = 391
  Top = 298
  BorderStyle = bsNone
  ClientHeight = 319
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 8
    Top = 48
    Width = 137
    Height = 105
    ItemHeight = 13
    Items.Strings = (
      'Item1'
      'Item2'
      'Item3')
    TabOrder = 0
  end
  object Edit: TEdit
    Left = 9
    Top = 17
    Width = 136
    Height = 21
    TabOrder = 1
    Text = 'Item'
  end
  object btnAdd: TButton
    Left = 152
    Top = 16
    Width = 75
    Height = 25
    Action = actAdd
    TabOrder = 2
  end
  object btnDelete: TButton
    Left = 152
    Top = 48
    Width = 75
    Height = 25
    Action = actDelete
    TabOrder = 3
  end
  object btnClear: TButton
    Left = 152
    Top = 80
    Width = 75
    Height = 25
    Action = actClear
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 152
    Top = 120
    object actAdd: TAction
      Caption = 'Add'
      OnExecute = actAddExecute
      OnUpdate = actAddUpdate
    end
    object actDelete: TAction
      Caption = 'Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actClear: TAction
      Caption = 'Clear'
      OnExecute = actClearExecute
    end
  end
end
