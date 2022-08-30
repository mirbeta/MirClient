object OptionsForm: TOptionsForm
  Left = 351
  Top = 244
  BorderStyle = bsDialog
  Caption = 'OrgChart Options'
  ClientHeight = 232
  ClientWidth = 373
  Color = clBtnFace
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 356
    Height = 185
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 239
    Top = 25
    Width = 41
    Height = 13
    Caption = 'Indent X'
    Transparent = True
  end
  object Label2: TLabel
    Left = 239
    Top = 53
    Width = 41
    Height = 13
    Caption = 'Indent Y'
    Transparent = True
  end
  object Label3: TLabel
    Left = 239
    Top = 81
    Width = 48
    Height = 13
    Caption = 'Line width'
    Transparent = True
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 15
    Width = 217
    Height = 93
    Caption = ' Edit Mode '
    TabOrder = 0
    object cbLeft: TCheckBox
      Left = 9
      Top = 16
      Width = 100
      Height = 17
      Caption = 'Left'
      TabOrder = 0
    end
    object cbCenter: TCheckBox
      Left = 9
      Top = 32
      Width = 100
      Height = 17
      Caption = 'Center'
      TabOrder = 1
    end
    object cbRight: TCheckBox
      Left = 9
      Top = 48
      Width = 100
      Height = 17
      Caption = 'Right'
      TabOrder = 2
    end
    object cbVCenter: TCheckBox
      Left = 9
      Top = 64
      Width = 100
      Height = 17
      Caption = 'Vert Center'
      TabOrder = 3
    end
    object cbWrap: TCheckBox
      Left = 112
      Top = 16
      Width = 100
      Height = 17
      Caption = 'Wrap'
      TabOrder = 4
    end
    object cbUpper: TCheckBox
      Left = 112
      Top = 32
      Width = 100
      Height = 17
      Caption = 'Upper'
      TabOrder = 5
    end
    object cbLower: TCheckBox
      Left = 112
      Top = 48
      Width = 100
      Height = 17
      Caption = 'Lower'
      TabOrder = 6
    end
    object cbGrow: TCheckBox
      Left = 112
      Top = 64
      Width = 100
      Height = 17
      Caption = 'Grow'
      TabOrder = 7
    end
  end
  object BitBtn1: TButton
    Left = 289
    Top = 199
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BitBtn2: TButton
    Left = 208
    Top = 199
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object cbSelect: TCheckBox
    Left = 25
    Top = 112
    Width = 100
    Height = 17
    Caption = 'Show Select'
    TabOrder = 6
  end
  object cbFocus: TCheckBox
    Left = 25
    Top = 128
    Width = 100
    Height = 17
    Caption = 'Show Focus'
    TabOrder = 7
  end
  object cbButtons: TCheckBox
    Left = 25
    Top = 144
    Width = 100
    Height = 17
    Caption = 'Show Buttons'
    TabOrder = 8
  end
  object cbCanDrag: TCheckBox
    Left = 128
    Top = 112
    Width = 100
    Height = 17
    Caption = 'Can Drag'
    TabOrder = 9
  end
  object cbShowDrag: TCheckBox
    Left = 128
    Top = 128
    Width = 100
    Height = 17
    Caption = 'Show Drag'
    TabOrder = 10
  end
  object cbInsDel: TCheckBox
    Left = 128
    Top = 144
    Width = 100
    Height = 17
    Caption = 'Insert, Delete'
    TabOrder = 11
  end
  object cbEdit: TCheckBox
    Left = 25
    Top = 160
    Width = 100
    Height = 17
    Caption = 'Edit'
    TabOrder = 12
  end
  object cbShowImages: TCheckBox
    Left = 128
    Top = 160
    Width = 100
    Height = 17
    Caption = 'Show Images'
    TabOrder = 13
  end
  object seX: TSpinEdit
    Left = 291
    Top = 21
    Width = 61
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object seY: TSpinEdit
    Left = 291
    Top = 49
    Width = 61
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object seLineWidth: TSpinEdit
    Left = 291
    Top = 77
    Width = 61
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 1
  end
end
