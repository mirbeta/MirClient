object StylesSimpleDemoAssignForm: TStylesSimpleDemoAssignForm
  Left = 396
  Top = 164
  BorderStyle = bsDialog
  Caption = 'Assign Styles'
  ClientHeight = 384
  ClientWidth = 223
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Content'
  end
  object Label2: TLabel
    Left = 8
    Top = 80
    Width = 58
    Height = 13
    Caption = 'Background'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 40
    Height = 13
    Caption = 'FilterBox'
  end
  object Label4: TLabel
    Left = 8
    Top = 152
    Width = 29
    Height = 13
    Caption = 'Group'
  end
  object Label5: TLabel
    Left = 8
    Top = 176
    Width = 59
    Height = 13
    Caption = 'GroupByBox'
  end
  object Label6: TLabel
    Left = 8
    Top = 200
    Width = 35
    Height = 13
    Caption = 'Header'
  end
  object Label7: TLabel
    Left = 8
    Top = 224
    Width = 41
    Height = 13
    Caption = 'Indicator'
  end
  object Label8: TLabel
    Left = 8
    Top = 248
    Width = 38
    Height = 13
    Caption = 'Inactive'
  end
  object Label9: TLabel
    Left = 8
    Top = 320
    Width = 42
    Height = 13
    Caption = 'Selected'
  end
  object Label10: TLabel
    Left = 8
    Top = 128
    Width = 30
    Height = 13
    Caption = 'Footer'
  end
  object Label11: TLabel
    Left = 8
    Top = 296
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object Label12: TLabel
    Left = 8
    Top = 56
    Width = 57
    Height = 13
    Caption = 'ContentOdd'
  end
  object Label14: TLabel
    Left = 8
    Top = 32
    Width = 62
    Height = 13
    Caption = 'ContentEven'
  end
  object Label13: TLabel
    Left = 8
    Top = 274
    Width = 49
    Height = 13
    Caption = 'IncSearch'
  end
  object ComboBox1: TComboBox
    Left = 72
    Top = 8
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnChange = ComboBoxChange
  end
  object ComboBox2: TComboBox
    Tag = 3
    Left = 72
    Top = 80
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnChange = ComboBoxChange
  end
  object ComboBox3: TComboBox
    Tag = 4
    Left = 72
    Top = 104
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 4
    OnChange = ComboBoxChange
  end
  object ComboBox4: TComboBox
    Tag = 6
    Left = 72
    Top = 152
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 6
    OnChange = ComboBoxChange
  end
  object ComboBox5: TComboBox
    Tag = 7
    Left = 72
    Top = 176
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 7
    OnChange = ComboBoxChange
  end
  object ComboBox6: TComboBox
    Tag = 8
    Left = 72
    Top = 200
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 8
    OnChange = ComboBoxChange
  end
  object ComboBox7: TComboBox
    Tag = 9
    Left = 72
    Top = 224
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 9
    OnChange = ComboBoxChange
  end
  object ComboBox8: TComboBox
    Tag = 10
    Left = 72
    Top = 248
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 10
    OnChange = ComboBoxChange
  end
  object ComboBox9: TComboBox
    Tag = 12
    Left = 72
    Top = 296
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 12
    OnChange = ComboBoxChange
  end
  object ComboBox10: TComboBox
    Tag = 5
    Left = 72
    Top = 128
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 5
    OnChange = ComboBoxChange
  end
  object btnRestore: TButton
    Left = 8
    Top = 352
    Width = 209
    Height = 25
    Caption = 'Restore Default'
    TabOrder = 14
    OnClick = btnRestoreClick
  end
  object ComboBox11: TComboBox
    Tag = 13
    Left = 72
    Top = 320
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 13
    OnChange = ComboBoxChange
  end
  object ComboBox12: TComboBox
    Tag = 2
    Left = 72
    Top = 56
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    OnChange = ComboBoxChange
  end
  object ComboBox14: TComboBox
    Tag = 1
    Left = 72
    Top = 32
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnChange = ComboBoxChange
  end
  object ComboBox13: TComboBox
    Tag = 11
    Left = 72
    Top = 272
    Width = 145
    Height = 19
    Style = csOwnerDrawFixed
    Color = 16247513
    ItemHeight = 13
    Sorted = True
    TabOrder = 11
    OnChange = ComboBoxChange
  end
end
