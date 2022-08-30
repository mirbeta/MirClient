object ReplaceDialogForm: TReplaceDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Replace'
  ClientHeight = 418
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Find what:'
  end
  object SpeedButton1: TSpeedButton
    Left = 335
    Top = 12
    Width = 20
    Height = 22
    Visible = False
  end
  object Label2: TLabel
    Left = 16
    Top = 206
    Width = 51
    Height = 13
    Caption = 'Find what:'
  end
  object Label3: TLabel
    Left = 16
    Top = 43
    Width = 65
    Height = 13
    Caption = 'Replace with:'
  end
  object SpeedButton2: TSpeedButton
    Left = 335
    Top = 39
    Width = 20
    Height = 22
    Visible = False
  end
  object Label4: TLabel
    Left = 18
    Top = 310
    Width = 65
    Height = 13
    Caption = 'Replace with:'
  end
  object ComboBox1: TComboBox
    Left = 87
    Top = 13
    Width = 242
    Height = 21
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object FindBtn: TButton
    Left = 358
    Top = 11
    Width = 75
    Height = 25
    Caption = '&Find'
    Default = True
    TabOrder = 2
    OnClick = FindBtnClick
  end
  object ReplaceBtn: TButton
    Left = 358
    Top = 42
    Width = 75
    Height = 25
    Caption = '&Replace'
    TabOrder = 3
    OnClick = ReplaceBtnClick
  end
  object ReplaceAllBtn: TButton
    Left = 358
    Top = 73
    Width = 75
    Height = 25
    Caption = 'Replace &All'
    TabOrder = 4
    OnClick = ReplaceAllBtnClick
  end
  object CloseBtn: TButton
    Left = 358
    Top = 174
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    TabOrder = 11
    OnClick = CloseBtnClick
  end
  object MoreBtn: TButton
    Left = 296
    Top = 174
    Width = 59
    Height = 25
    Caption = 'Mor&e'
    TabOrder = 10
    OnClick = MoreBtnClick
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 70
    Width = 130
    Height = 17
    Caption = '&Case sensitive'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 93
    Width = 130
    Height = 17
    Caption = 'Regular e&xpression'
    TabOrder = 6
    OnClick = CheckBox2Click
  end
  object CheckBox4: TCheckBox
    Left = 152
    Top = 70
    Width = 138
    Height = 17
    Caption = '&Whole word Only'
    TabOrder = 7
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 152
    Top = 93
    Width = 138
    Height = 17
    Caption = 'Wrap at the end of file'
    TabOrder = 8
    OnClick = CheckBox5Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 224
    Width = 415
    Height = 84
    TabOrder = 12
    OnChange = Memo1Change
  end
  object ComboBox2: TComboBox
    Left = 87
    Top = 40
    Width = 242
    Height = 21
    TabOrder = 1
    OnChange = ComboBox2Change
  end
  object RangeGroup: TRadioGroup
    Left = 16
    Top = 162
    Width = 274
    Height = 37
    Caption = 'Ran&ge'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Curre&nt file'
      '&Selection'
      'All &open files')
    TabOrder = 9
    OnClick = RangeGroupClick
  end
  object Memo2: TMemo
    Left = 18
    Top = 328
    Width = 415
    Height = 84
    TabOrder = 13
  end
  object DirGroup: TRadioGroup
    Left = 16
    Top = 116
    Width = 274
    Height = 40
    Caption = '&Direction'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Down'
      'Up')
    TabOrder = 14
  end
  object PopupMenu1: TPopupMenu
    Left = 312
    Top = 40
    object abCharacter1: TMenuItem
      Caption = 'Tab Character'
    end
    object NewLine1: TMenuItem
      Caption = 'New Line'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AnyCharacter1: TMenuItem
      Caption = 'Any Character'
    end
    object CharacterinRange1: TMenuItem
      Caption = 'Character in Range'
    end
    object CharacternotinRange1: TMenuItem
      Caption = 'Character not in Range'
    end
    object BeginningofLine1: TMenuItem
      Caption = 'Beginning of Line'
    end
    object EndofLine1: TMenuItem
      Caption = 'End of Line'
    end
    object aggedExpression1: TMenuItem
      Caption = 'Tagged Expression'
    end
    object Or1: TMenuItem
      Caption = 'Or'
    end
    object N0or1matches1: TMenuItem
      Caption = '0 or More Matches'
    end
    object N1orMoreMatches1: TMenuItem
      Caption = '1 or More Matches'
    end
    object N0or1Matches2: TMenuItem
      Caption = '0 or 1 Matches'
    end
  end
end
