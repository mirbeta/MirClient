object FindDialogForm: TFindDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 287
  ClientWidth = 445
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
    Caption = 'F&ind what:'
    FocusControl = ComboBox1
  end
  object SpeedButton1: TSpeedButton
    Left = 335
    Top = 12
    Width = 20
    Height = 22
    Visible = False
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 16
    Top = 174
    Width = 51
    Height = 13
    Caption = 'Find w&hat:'
    FocusControl = Memo1
  end
  object ComboBox1: TComboBox
    Left = 80
    Top = 13
    Width = 249
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
    TabOrder = 1
    OnClick = FindBtnClick
  end
  object PrevBtn: TButton
    Left = 358
    Top = 38
    Width = 75
    Height = 25
    Caption = '&Previous'
    TabOrder = 2
    OnClick = PrevBtnClick
  end
  object Marker: TButton
    Left = 358
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Set &Marker'
    TabOrder = 3
    OnClick = MarkerClick
  end
  object CloseBtn: TButton
    Left = 358
    Top = 90
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    TabOrder = 11
    OnClick = CloseBtnClick
  end
  object MoreBtn: TButton
    Left = 296
    Top = 90
    Width = 59
    Height = 25
    Caption = 'Mor&e'
    TabOrder = 10
    OnClick = MoreBtnClick
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 46
    Width = 130
    Height = 17
    Caption = '&Case sensitive'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 69
    Width = 130
    Height = 17
    Caption = 'Regular e&xpression'
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 92
    Width = 130
    Height = 17
    Caption = 'Cl&ose if found'
    TabOrder = 6
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 152
    Top = 46
    Width = 138
    Height = 17
    Caption = '&Whole word Only'
    TabOrder = 7
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 152
    Top = 69
    Width = 138
    Height = 17
    Caption = 'Wrap at the end of file'
    TabOrder = 8
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 152
    Top = 92
    Width = 138
    Height = 17
    Caption = 'Continue to next file'
    TabOrder = 9
    OnClick = CheckBox6Click
  end
  object Memo1: TMemo
    Left = 14
    Top = 192
    Width = 415
    Height = 84
    TabOrder = 12
    OnChange = Memo1Change
  end
  object DirGroup: TRadioGroup
    Left = 16
    Top = 121
    Width = 274
    Height = 40
    Caption = '&Direction'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Down'
      'Up')
    TabOrder = 13
    OnClick = DirGroupClick
  end
  object PopupMenu1: TPopupMenu
    Left = 312
    Top = 40
    object abCharacter1: TMenuItem
      Caption = 'Tab Character'
      OnClick = abCharacter1Click
    end
    object NewLine1: TMenuItem
      Caption = 'New Line'
      OnClick = NewLine1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AnyCharacter1: TMenuItem
      Caption = 'Any Character'
      OnClick = AnyCharacter1Click
    end
    object CharacterinRange1: TMenuItem
      Caption = 'Character in Range'
      OnClick = CharacterinRange1Click
    end
    object CharacternotinRange1: TMenuItem
      Caption = 'Character not in Range'
      OnClick = CharacternotinRange1Click
    end
    object BeginningofLine1: TMenuItem
      Caption = 'Beginning of Line'
      OnClick = BeginningofLine1Click
    end
    object EndofLine1: TMenuItem
      Caption = 'End of Line'
      OnClick = EndofLine1Click
    end
    object aggedExpression1: TMenuItem
      Caption = 'Tagged Expression'
      OnClick = aggedExpression1Click
    end
    object Or1: TMenuItem
      Caption = 'Or'
      OnClick = Or1Click
    end
    object N0or1matches1: TMenuItem
      Caption = '0 or More Matches'
      OnClick = N0or1matches1Click
    end
    object N1orMoreMatches1: TMenuItem
      Caption = '1 or More Matches'
      OnClick = N1orMoreMatches1Click
    end
    object N0or1Matches2: TMenuItem
      Caption = '0 or 1 Matches'
      OnClick = N0or1Matches2Click
    end
  end
end
