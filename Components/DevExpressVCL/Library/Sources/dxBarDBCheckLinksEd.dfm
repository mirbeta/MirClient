object dxBarDBCheckLinksEditor: TdxBarDBCheckLinksEditor
  Left = 233
  Top = 127
  BorderStyle = bsDialog
  Caption = 'ExpressBars DBCheckLinks Editor'
  ClientHeight = 278
  ClientWidth = 412
  Color = clBtnFace
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 171
    Height = 231
    Caption = ' CheckLinks '
    TabOrder = 0
    object ListBox: TListBox
      Left = 10
      Top = 17
      Width = 151
      Height = 172
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxClick
    end
    object Button1: TButton
      Left = 16
      Top = 198
      Width = 65
      Height = 23
      Caption = 'Add'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 90
      Top = 198
      Width = 65
      Height = 23
      Caption = 'Delete'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 184
    Top = 6
    Width = 221
    Height = 231
    Caption = ' Properties '
    TabOrder = 1
    object Label1: TLabel
      Left = 13
      Top = 24
      Width = 20
      Height = 13
      Caption = 'Item'
    end
    object Label2: TLabel
      Left = 12
      Top = 56
      Width = 62
      Height = 13
      Caption = 'EnableTypes'
    end
    object Bevel1: TBevel
      Left = 80
      Top = 62
      Width = 129
      Height = 9
      Shape = bsTopLine
    end
    object ComboBox: TComboBox
      Left = 42
      Top = 20
      Width = 167
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 78
      Width = 97
      Height = 17
      Caption = 'dxdbtCanModify'
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 138
      Width = 116
      Height = 17
      Caption = 'dxdbtHasRecords'
      TabOrder = 4
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 98
      Width = 97
      Height = 17
      Caption = 'dxdbtNotEOF'
      TabOrder = 2
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 158
      Width = 116
      Height = 17
      Caption = 'dxdbtIsModified'
      TabOrder = 5
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 178
      Width = 116
      Height = 17
      Caption = 'dxdbtIsNotModified'
      TabOrder = 6
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 118
      Width = 97
      Height = 17
      Caption = 'dxbdbtNotBOF'
      TabOrder = 3
    end
  end
  object Button3: TButton
    Left = 252
    Top = 248
    Width = 72
    Height = 23
    Caption = 'OK'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 334
    Top = 248
    Width = 72
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button4Click
  end
end
