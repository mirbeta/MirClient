object cxEditMaskEditorDlg: TcxEditMaskEditorDlg
  Left = 353
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Input Mask Editor'
  ClientHeight = 322
  ClientWidth = 543
  Color = clBtnFace
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Mask kind'
    Transparent = True
  end
  object Bevel1: TBevel
    Left = 7
    Top = 37
    Width = 266
    Height = 2
  end
  object cxMaskKindPickEdit1: TcxComboBox
    Tag = 4
    Left = 64
    Top = 4
    Properties.DropDownListStyle = lsFixedList
    Properties.Items.Strings = (
      'Standard'
      'Regular expression'
      'Extended regular expression')
    Properties.OnChange = cxMaskKindPickEdit1PropertiesChange
    TabOrder = 0
    Width = 193
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 281
    Width = 543
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object CancelButton: TcxButton
      Left = 440
      Top = 5
      Width = 93
      Height = 25
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = CancelButtonClick
    end
    object OKButton: TcxButton
      Left = 336
      Top = 5
      Width = 93
      Height = 25
      Caption = 'OK'
      TabOrder = 1
      OnClick = OKButtonClick
    end
    object MasksButton: TcxButton
      Left = 8
      Top = 5
      Width = 93
      Height = 25
      Caption = 'Masks...'
      TabOrder = 0
      OnClick = MasksButtonClick
    end
  end
  object RegExprMaskPanel: TPanel
    Left = 0
    Top = 42
    Width = 543
    Height = 239
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object Label2: TLabel
      Left = 4
      Top = 12
      Width = 55
      Height = 13
      Caption = 'Input mask:'
      Transparent = True
    end
    object Label3: TLabel
      Left = 4
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Test input:'
      Transparent = True
    end
    object Label4: TLabel
      Left = 12
      Top = 96
      Width = 38
      Height = 13
      Caption = 'Sample:'
      Transparent = True
    end
    object Label5: TLabel
      Left = 192
      Top = 96
      Width = 56
      Height = 13
      Caption = 'Description:'
      Transparent = True
    end
    object Bevel2: TBevel
      Left = 8
      Top = 80
      Width = 529
      Height = 2
    end
    object cxEditMaskEdit: TcxTextEdit
      Tag = 7
      Left = 64
      Top = 8
      TabOrder = 0
      OnKeyDown = cxEditMaskEditKeyDown
      Width = 473
    end
    object cxMaskEdit1: TcxMaskEdit
      Tag = 4
      Left = 64
      Top = 44
      Properties.IgnoreMaskBlank = True
      Properties.MaskKind = emkRegExprEx
      Properties.MaxLength = 0
      TabOrder = 1
      OnEnter = cxMaskEdit1Enter
      Width = 473
    end
    object Memo1: TcxMemo
      Left = 192
      Top = 112
      TabOrder = 2
      Height = 121
      Width = 345
    end
    object ListBox1: TcxListBox
      Left = 8
      Top = 112
      Width = 177
      Height = 121
      ItemHeight = 13
      TabOrder = 3
      OnClick = ListBox1Click
      OnExit = ListBox1Exit
    end
  end
  object StandardMaskPanel: TPanel
    Left = 0
    Top = 42
    Width = 543
    Height = 239
    BevelOuter = bvNone
    TabOrder = 1
    object Bevel3: TBevel
      Left = 4
      Top = 188
      Width = 261
      Height = 2
    end
    object Label6: TLabel
      Left = 8
      Top = 4
      Width = 55
      Height = 13
      Caption = 'Input mask:'
      Transparent = True
    end
    object Label7: TLabel
      Left = 8
      Top = 196
      Width = 51
      Height = 13
      Caption = 'Test Input:'
      Transparent = True
    end
    object Label8: TLabel
      Left = 272
      Top = 4
      Width = 66
      Height = 13
      Caption = 'Sample mask:'
      Transparent = True
    end
    object Label9: TLabel
      Left = 140
      Top = 64
      Width = 98
      Height = 13
      Caption = 'Character for blanks:'
      Transparent = True
    end
    object cxTextEdit1: TcxTextEdit
      Tag = 21
      Left = 8
      Top = 20
      TabOrder = 0
      OnExit = cxTextEdit1Exit
      OnKeyDown = cxTextEdit1KeyDown
      Width = 253
    end
    object cxMaskEdit2: TcxMaskEdit
      Tag = 22
      Left = 8
      Top = 212
      Properties.IgnoreMaskBlank = True
      TabOrder = 3
      Width = 253
    end
    object cxTextEdit2: TcxTextEdit
      Tag = 23
      Left = 244
      Top = 60
      Properties.MaxLength = 1
      Properties.OnChange = cxTextEdit2PropertiesChange
      TabOrder = 1
      OnExit = cxTextEdit2Exit
      Width = 17
    end
    object cxCheckBox1: TcxCheckBox
      Left = 4
      Top = 160
      Caption = 'Save literal characters'
      Properties.OnChange = cxCheckBox1PropertiesChange
      TabOrder = 2
      Transparent = True
      Width = 153
    end
    object ListView1: TListView
      Left = 272
      Top = 20
      Width = 261
      Height = 213
      Columns = <
        item
          AutoSize = True
          Caption = 'Description'
        end
        item
          AutoSize = True
          Caption = 'Sample'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 4
      ViewStyle = vsReport
      OnSelectItem = ListView1SelectItem
    end
  end
end
