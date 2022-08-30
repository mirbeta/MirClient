object frmSpinImagePropEditor: TfrmSpinImagePropEditor
  Left = 249
  Top = 149
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Express SpinImage Items editor: '
  ClientHeight = 330
  ClientWidth = 279
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
    Left = 203
    Top = 0
    Width = 76
    Height = 199
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
      Top = 55
      Width = 72
      Height = 22
      Caption = 'De&lete'
      TabOrder = 2
      OnClick = bDeleteClick
    end
    object bUp: TButton
      Left = 2
      Top = 105
      Width = 72
      Height = 22
      Caption = 'Move &up'
      TabOrder = 4
      OnClick = bUpClick
    end
    object bDown: TButton
      Left = 2
      Top = 130
      Width = 72
      Height = 22
      Caption = 'Move d&own'
      TabOrder = 5
      OnClick = bDownClick
    end
    object bClear: TButton
      Left = 2
      Top = 80
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
    Width = 203
    Height = 199
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object ListBox: TdxImageListBox
      Left = 4
      Top = 4
      Width = 195
      Height = 191
      Alignment = taLeftJustify
      ImageAlign = dxliLeft
      ItemHeight = 32
      MultiLines = False
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
    Top = 199
    Width = 279
    Height = 131
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LabelHint: TLabel
      Left = 9
      Top = 60
      Width = 19
      Height = 13
      Caption = 'Hint'
    end
    object LabelValue: TLabel
      Left = 7
      Top = 34
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object LabelImageIndex: TLabel
      Left = 6
      Top = 5
      Width = 58
      Height = 13
      Caption = 'Image Index'
    end
    object Edit1: TEdit
      Left = 42
      Top = 31
      Width = 159
      Height = 21
      Enabled = False
      TabOrder = 1
      OnExit = Edit1Exit
    end
    object Edit2: TEdit
      Left = 67
      Top = 4
      Width = 133
      Height = 21
      Enabled = False
      TabOrder = 0
      OnExit = Edit2Exit
      OnKeyPress = Edit2KeyPress
    end
    object BOk: TButton
      Left = 47
      Top = 102
      Width = 72
      Height = 22
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object bCancel: TButton
      Left = 124
      Top = 102
      Width = 72
      Height = 22
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object bHelp: TButton
      Left = 202
      Top = 102
      Width = 72
      Height = 22
      Caption = '&Help'
      TabOrder = 6
    end
    object SpinImage: TdxSpinImage
      Left = 206
      Top = 4
      Width = 69
      Height = 50
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
    object Edit3: TMemo
      Left = 42
      Top = 57
      Width = 233
      Height = 38
      TabOrder = 2
      OnExit = Edit3Exit
    end
  end
end
