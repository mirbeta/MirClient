object PeriodPlannerItemEditForm: TPeriodPlannerItemEditForm
  Left = 337
  Top = 179
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'PeriodPlannerItemEditForm'
  ClientHeight = 274
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_subject: TLabel
    Left = 13
    Top = 31
    Width = 36
    Height = 13
    Caption = '&Subject'
    FocusControl = EdSubject
  end
  object lbl_start: TLabel
    Left = 13
    Top = 59
    Width = 44
    Height = 13
    Caption = '&Start time'
  end
  object lbl_notes: TLabel
    Left = 13
    Top = 113
    Width = 28
    Height = 13
    Caption = '&Notes'
  end
  object lbl_shape: TLabel
    Left = 13
    Top = 211
    Width = 31
    Height = 13
    Caption = '&Shape'
    FocusControl = CBShape
  end
  object lbl_color: TLabel
    Left = 165
    Top = 211
    Width = 24
    Height = 13
    Caption = '&Color'
    FocusControl = CBShape
  end
  object lbl_end: TLabel
    Left = 13
    Top = 88
    Width = 41
    Height = 13
    Caption = 'En&d time'
  end
  object Shape1: TShape
    Left = 207
    Top = 207
    Width = 50
    Height = 22
    OnMouseDown = Shape1MouseDown
  end
  object StartTime: TDateTimePicker
    Left = 161
    Top = 54
    Width = 88
    Height = 21
    Date = 37415.499305555600000000
    Time = 37415.499305555600000000
    Kind = dtkTime
    TabOrder = 2
  end
  object EndTime: TDateTimePicker
    Left = 159
    Top = 86
    Width = 90
    Height = 21
    Date = 37415.499305555600000000
    Time = 37415.499305555600000000
    Kind = dtkTime
    TabOrder = 4
  end
  object CBShape: TComboBox
    Left = 61
    Top = 207
    Width = 89
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    Items.Strings = (
      'Rectangular'
      'Rounded'
      'Hexagon'
      'Custom')
  end
  object EdSubject: TEdit
    Left = 62
    Top = 27
    Width = 331
    Height = 21
    TabOrder = 0
  end
  object Notes: TMemo
    Left = 61
    Top = 112
    Width = 331
    Height = 89
    Lines.Strings = (
      '')
    TabOrder = 5
  end
  object StartDate: TDateTimePicker
    Left = 62
    Top = 54
    Width = 87
    Height = 21
    Date = 37416.510760300900000000
    Time = 37416.510760300900000000
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 20
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Warning: this appointment occurs in the past'
    Color = clInfoBk
    TabOrder = 7
    Visible = False
    object WarningPanel: TPanel
      Left = 0
      Top = 0
      Width = 405
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Warning: this appointment occurs in the past'
      Color = clInfoBk
      TabOrder = 0
      Visible = False
    end
  end
  object ButtonBottomPanel: TPanel
    Left = 0
    Top = 233
    Width = 405
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 8
    object ButtonBottomRightPanel: TPanel
      Left = 228
      Top = 0
      Width = 177
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OKBtn: TButton
        Left = 6
        Top = 10
        Width = 75
        Height = 25
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object CancBtn: TButton
        Left = 94
        Top = 10
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object EndDate: TDateTimePicker
    Left = 62
    Top = 86
    Width = 87
    Height = 21
    Date = 37416.510760300900000000
    Time = 37416.510760300900000000
    TabOrder = 3
  end
  object ColorDialog: TColorDialog
    Left = 12
    Top = 149
  end
end
