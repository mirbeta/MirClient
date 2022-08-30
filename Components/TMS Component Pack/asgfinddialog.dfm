object AsgFindDlg: TAsgFindDlg
  Left = 257
  Top = 362
  BorderStyle = bsDialog
  Caption = 'Find text'
  ClientHeight = 235
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Text to find :'
  end
  object Options: TGroupBox
    Left = 184
    Top = 32
    Width = 161
    Height = 161
    Caption = 'Options'
    TabOrder = 3
    object Docase: TCheckBox
      Left = 8
      Top = 16
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object Whole: TCheckBox
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object MatchFirst: TCheckBox
      Left = 8
      Top = 64
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Match from first char'
      TabOrder = 2
    end
    object IgnoreHTML: TCheckBox
      Left = 8
      Top = 88
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Ignore HTML tags'
      TabOrder = 3
    end
    object Fixed: TCheckBox
      Left = 8
      Top = 112
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Find in fixed cells'
      TabOrder = 4
    end
    object Wildcards: TCheckBox
      Left = 8
      Top = 136
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Match with &wildchards'
      TabOrder = 5
    end
  end
  object TextToFind: TComboBox
    Left = 104
    Top = 4
    Width = 241
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = TextToFindChange
  end
  object Scope: TRadioGroup
    Left = 8
    Top = 135
    Width = 169
    Height = 92
    Caption = 'Scope'
    ItemIndex = 0
    Items.Strings = (
      'All cells'
      'Current row only'
      'Current column only'
      'Selected cells')
    TabOrder = 2
    OnClick = ScopeClick
  end
  object OkBtn: TButton
    Left = 184
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 271
    Top = 199
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = CancelBtnClick
  end
  object gbDirection: TGroupBox
    Left = 8
    Top = 32
    Width = 169
    Height = 97
    Caption = 'Direction'
    TabOrder = 1
    object cbForwardTB: TCheckBox
      Left = 10
      Top = 17
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Forward (top to bottom)'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbForwardTBClick
      OnMouseDown = cbForwardTBMouseDown
    end
    object cbForwardLR: TCheckBox
      Left = 10
      Top = 35
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Forward (left to right)'
      TabOrder = 1
      OnClick = cbForwardLRClick
      OnMouseDown = cbForwardLRMouseDown
    end
    object cbBackwardBT: TCheckBox
      Left = 10
      Top = 53
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Backward (bottom to top)'
      TabOrder = 2
      OnClick = cbBackwardBTClick
      OnMouseDown = cbBackwardBTMouseDown
    end
    object cbBackwardRL: TCheckBox
      Left = 10
      Top = 71
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Backward (right to left)'
      TabOrder = 3
      OnClick = cbBackwardRLClick
      OnMouseDown = cbBackwardRLMouseDown
    end
  end
end
