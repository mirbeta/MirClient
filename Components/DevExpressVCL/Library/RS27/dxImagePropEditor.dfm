object frmdxImagePropEditor: TfrmdxImagePropEditor
  Left = 251
  Top = 167
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Express Image Items editor: '
  ClientHeight = 321
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 207
    Top = 0
    Width = 75
    Height = 208
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object bAdd: TButton
      Left = 2
      Top = 5
      Width = 72
      Height = 22
      Caption = '&Add'
      TabOrder = 0
      OnClick = bAddClick
    end
    object bInsert: TButton
      Left = 2
      Top = 30
      Width = 72
      Height = 22
      Caption = '&Insert'
      TabOrder = 1
      OnClick = bInsertClick
    end
    object bDelete: TButton
      Left = 2
      Top = 54
      Width = 72
      Height = 22
      Caption = 'De&lete'
      TabOrder = 2
      OnClick = bDeleteClick
    end
    object bUp: TButton
      Left = 2
      Top = 102
      Width = 72
      Height = 22
      Caption = 'Move &up'
      TabOrder = 4
      OnClick = bUpClick
    end
    object bDown: TButton
      Left = 2
      Top = 126
      Width = 72
      Height = 22
      Caption = 'Move d&own'
      TabOrder = 5
      OnClick = bDownClick
    end
    object bClear: TButton
      Left = 2
      Top = 78
      Width = 72
      Height = 22
      Caption = 'Clear'
      TabOrder = 3
      OnClick = bClearClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 207
    Height = 208
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object ListBox: TdxImageListBox
      Left = 4
      Top = 4
      Width = 199
      Height = 200
      Alignment = taLeftJustify
      ImageAlign = dxliLeft
      ItemHeight = 0
      MultiLines = True
      VertAlignment = tvaCenter
      Align = alClient
      DragMode = dmAutomatic
      TabOrder = 0
      OnClick = ListBoxClick
      OnDragDrop = ListBoxDragDrop
      OnDragOver = ListBoxDragOver
      SaveStrings = ()
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 208
    Width = 282
    Height = 113
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LabelText: TLabel
      Left = 6
      Top = 8
      Width = 21
      Height = 13
      Caption = 'Text'
    end
    object LabelValue: TLabel
      Left = 7
      Top = 32
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object LabelImageIndex: TLabel
      Left = 6
      Top = 60
      Width = 58
      Height = 13
      Caption = 'Image Index'
    end
    object Edit1: TEdit
      Left = 40
      Top = 5
      Width = 239
      Height = 21
      Enabled = False
      TabOrder = 0
      OnExit = Edit1Exit
    end
    object Edit3: TEdit
      Left = 40
      Top = 30
      Width = 168
      Height = 21
      TabOrder = 1
      OnExit = Edit3Exit
    end
    object Edit2: TEdit
      Left = 93
      Top = 57
      Width = 115
      Height = 21
      Enabled = False
      TabOrder = 2
      OnExit = Edit2Exit
      OnKeyPress = Edit2KeyPress
    end
    object BOk: TButton
      Left = 51
      Top = 85
      Width = 72
      Height = 22
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object bCancel: TButton
      Left = 129
      Top = 85
      Width = 72
      Height = 22
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object bHelp: TButton
      Left = 207
      Top = 85
      Width = 72
      Height = 22
      Caption = '&Help'
      TabOrder = 6
      OnClick = bHelpClick
    end
    object SpinImage: TdxSpinImage
      Left = 213
      Top = 30
      Width = 66
      Height = 49
      AutoSize = False
      BorderStyle = bsSingle
      DefaultImages = True
      ImageHAlign = hsiCenter
      ImageVAlign = vsiCenter
      Items = <>
      ItemIndex = -1
      ReadOnly = False
      Stretch = True
      UpDownAlign = udaRight
      UpDownOrientation = siVertical
      UpDownWidth = 16
      UseDblClick = True
      OnChange = SpinImageChange
      Color = clWhite
      TabOrder = 3
    end
  end
end
