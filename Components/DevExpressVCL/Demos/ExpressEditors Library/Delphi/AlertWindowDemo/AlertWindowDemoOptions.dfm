object FormOptions: TFormOptions
  Left = 518
  Top = 287
  BorderIcons = [biSystemMenu, biMinimize, biHelp]
  BorderStyle = bsSingle
  Caption = 'Alert options'
  ClientHeight = 318
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TcxButton
    Left = 166
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = btOkClick
  end
  object btCancel: TcxButton
    Left = 248
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btCancelClick
  end
  object gbOptionsManager: TcxGroupBox
    Left = 8
    Top = 8
    Caption = 'Manager options'
    TabOrder = 2
    Height = 121
    Width = 201
    object lbPosition: TcxLabel
      Left = 8
      Top = 24
      Caption = 'Pop-up position:'
      Transparent = True
    end
    object cbPosition: TcxComboBox
      Left = 104
      Top = 21
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Auto'
        'TopLeft'
        'TopRight'
        'BottomLeft'
        'BottomRight')
      Properties.ReadOnly = False
      TabOrder = 0
      Text = 'Auto'
      Width = 88
    end
    object lbMaxAlertShownAtOnce: TcxLabel
      Left = 8
      Top = 51
      AutoSize = False
      Caption = 'Max alerts shown at once:'
      Properties.WordWrap = True
      Transparent = True
      Height = 30
      Width = 81
    end
    object seMaxInDisplay: TcxSpinEdit
      Left = 104
      Top = 62
      Properties.AssignedValues.MinValue = True
      TabOrder = 2
      Width = 88
    end
    object cbCollapseEmptySlots: TcxCheckBox
      Left = 8
      Top = 91
      Caption = 'Collapse Empty Slots'
      State = cbsChecked
      TabOrder = 4
      Transparent = True
      Width = 129
    end
  end
  object gbOptionsSize: TcxGroupBox
    Left = 215
    Top = 8
    Caption = 'Size options'
    TabOrder = 3
    Height = 145
    Width = 189
    object lbWidth: TcxLabel
      Left = 9
      Top = 23
      Caption = 'Width:'
      Transparent = True
    end
    object lbHeight: TcxLabel
      Left = 9
      Top = 51
      Caption = 'Height:'
      Transparent = True
    end
    object seWidth: TcxSpinEdit
      Left = 93
      Top = 21
      Properties.MaxValue = 600.000000000000000000
      Properties.MinValue = 200.000000000000000000
      TabOrder = 0
      Value = 200
      Width = 88
    end
    object seHeight: TcxSpinEdit
      Left = 93
      Top = 49
      Properties.MaxValue = 400.000000000000000000
      Properties.MinValue = 100.000000000000000000
      TabOrder = 1
      Value = 100
      Width = 88
    end
    object cbAutoWidth: TcxCheckBox
      Left = 9
      Top = 74
      Caption = 'Auto width'
      State = cbsChecked
      TabOrder = 2
      Transparent = True
      Width = 107
    end
    object cbAutoHeight: TcxCheckBox
      Left = 9
      Top = 93
      Caption = 'Auto height'
      State = cbsChecked
      TabOrder = 3
      Transparent = True
      Width = 107
    end
    object cbAutoSizeAdjustment: TcxCheckBox
      Left = 9
      Top = 112
      Caption = 'Auto Size Adjustment'
      TabOrder = 6
      Transparent = True
      Width = 168
    end
  end
  object gbOptionsBehavior: TcxGroupBox
    Left = 216
    Top = 160
    Caption = 'Behavior options'
    TabOrder = 4
    Height = 113
    Width = 189
    object lbTime: TcxLabel
      Left = 8
      Top = 20
      Caption = 'Display time, ms:'
      Transparent = True
    end
    object seTime: TcxSpinEdit
      Left = 93
      Top = 20
      Properties.MinValue = 10.000000000000000000
      TabOrder = 1
      Value = 7000
      Width = 88
    end
    object cbCloseOnRightClick: TcxCheckBox
      Left = 8
      Top = 47
      Caption = 'Close on right click'
      State = cbsChecked
      TabOrder = 2
      Transparent = True
      Width = 121
    end
    object cbHotTrack: TcxCheckBox
      Left = 8
      Top = 66
      Caption = 'Hot-tracking'
      State = cbsChecked
      TabOrder = 3
      Transparent = True
      Width = 121
    end
  end
  object gbOptionsAnimation: TcxGroupBox
    Left = 8
    Top = 136
    Caption = 'Animation options'
    TabOrder = 5
    Height = 137
    Width = 201
    object lbShowingDirection: TcxLabel
      Left = 8
      Top = 51
      Caption = 'Direction to show:'
      Transparent = True
    end
    object cbShowingDirection: TcxComboBox
      Left = 104
      Top = 49
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Auto'
        'Left'
        'Right'
        'Top'
        'Down')
      Properties.ReadOnly = False
      TabOrder = 1
      Text = 'Auto'
      Width = 88
    end
    object lbShowingAnimation: TcxLabel
      Left = 8
      Top = 23
      Caption = 'Animation to show:'
      Transparent = True
    end
    object cbShowingAnimation: TcxComboBox
      Left = 104
      Top = 21
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'Slide'
        'Fade'
        'Move')
      TabOrder = 3
      Text = 'Slide'
      Width = 88
    end
    object lbHidingAnimation: TcxLabel
      Left = 8
      Top = 79
      Caption = 'Animation to hide:'
      Transparent = True
    end
    object cbHidingAnimation: TcxComboBox
      Left = 104
      Top = 77
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'Slide'
        'Fade'
        'Move')
      TabOrder = 5
      Text = 'Fade'
      Width = 88
    end
    object cbHidingDirection: TcxComboBox
      Left = 104
      Top = 105
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Auto'
        'Left'
        'Right'
        'Top'
        'Down')
      Properties.ReadOnly = False
      TabOrder = 6
      Text = 'Auto'
      Width = 88
    end
    object lbHidingDirection: TcxLabel
      Left = 8
      Top = 107
      Caption = 'Direction to hide:'
      Transparent = True
    end
  end
  object btnApply: TcxButton
    Left = 330
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Apply'
    ModalResult = 1
    TabOrder = 6
    OnClick = btnApplyClick
  end
end
