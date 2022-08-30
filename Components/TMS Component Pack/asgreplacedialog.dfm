object AsgReplaceDialog: TAsgReplaceDialog
  Left = 298
  Top = 226
  BorderStyle = bsDialog
  Caption = 'Replace text'
  ClientHeight = 294
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
  DesignSize = (
    354
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Text to find :'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 77
    Height = 13
    Caption = 'Text to replace :'
  end
  object Options: TGroupBox
    Left = 183
    Top = 56
    Width = 161
    Height = 169
    Caption = 'Options'
    TabOrder = 4
    DesignSize = (
      161
      169)
    object Docase: TCheckBox
      Left = 8
      Top = 24
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object Whole: TCheckBox
      Left = 8
      Top = 48
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object MatchFirst: TCheckBox
      Left = 8
      Top = 72
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Match from first char'
      TabOrder = 2
    end
    object IgnoreHTML: TCheckBox
      Left = 8
      Top = 96
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Ignore HTML tags'
      TabOrder = 3
    end
    object Fixed: TCheckBox
      Left = 8
      Top = 120
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Find in fixed cells'
      TabOrder = 4
    end
    object Prompt: TCheckBox
      Left = 8
      Top = 144
      Width = 150
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Prompt on replace'
      TabOrder = 5
    end
  end
  object TextToFind: TComboBox
    Left = 104
    Top = 5
    Width = 242
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = TextToFindChange
  end
  object Scope: TRadioGroup
    Left = 8
    Top = 159
    Width = 169
    Height = 98
    Caption = 'Scope'
    ItemIndex = 0
    Items.Strings = (
      'All cells'
      'Current column only'
      'Current row only'
      'Selected cells')
    TabOrder = 3
    OnClick = ScopeClick
  end
  object OkBtn: TButton
    Left = 112
    Top = 261
    Width = 75
    Height = 25
    Anchors = [akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 272
    Top = 261
    Width = 75
    Height = 25
    Anchors = [akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
    OnClick = CancelBtnClick
  end
  object gbDirection: TGroupBox
    Left = 8
    Top = 56
    Width = 169
    Height = 97
    Caption = 'Direction'
    TabOrder = 2
    DesignSize = (
      169
      97)
    object cbForwardTB: TCheckBox
      Left = 10
      Top = 17
      Width = 156
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
      Width = 151
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
      Width = 151
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
      Width = 156
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Backward (right to left)'
      TabOrder = 3
      OnClick = cbBackwardRLClick
      OnMouseDown = cbBackwardRLMouseDown
    end
  end
  object TextToReplace: TComboBox
    Left = 104
    Top = 28
    Width = 241
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = TextToFindChange
  end
  object ReplaceAll: TButton
    Left = 192
    Top = 261
    Width = 75
    Height = 25
    Anchors = [akRight]
    Caption = 'Replace all'
    TabOrder = 6
    OnClick = ReplaceAllClick
  end
end
