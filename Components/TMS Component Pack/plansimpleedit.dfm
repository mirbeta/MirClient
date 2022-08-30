object SimplePlannerItemEditForm: TSimplePlannerItemEditForm
  Left = 805
  Top = 157
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SimplePlannerItemEditForm'
  ClientHeight = 245
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
  object lbl_subj: TLabel
    Left = 13
    Top = 31
    Width = 36
    Height = 13
    Caption = '&Subject'
    FocusControl = EdSubject
  end
  object lbl_from: TLabel
    Left = 155
    Top = 58
    Width = 23
    Height = 13
    Caption = '&From'
    FocusControl = StartTime
  end
  object lbl_date: TLabel
    Left = 13
    Top = 58
    Width = 23
    Height = 13
    Caption = '&Date'
  end
  object lbl_notes: TLabel
    Left = 13
    Top = 81
    Width = 28
    Height = 13
    Caption = '&Notes'
  end
  object lbl_to: TLabel
    Left = 283
    Top = 58
    Width = 13
    Height = 13
    Caption = '&To'
    FocusControl = EndTime
  end
  object lbl_shape: TLabel
    Left = 13
    Top = 179
    Width = 31
    Height = 13
    Caption = '&Shape'
    FocusControl = CBShape
  end
  object lbl_color: TLabel
    Left = 165
    Top = 179
    Width = 24
    Height = 13
    Caption = '&Color'
    FocusControl = CBShape
  end
  object Shape1: TShape
    Left = 198
    Top = 175
    Width = 47
    Height = 22
    OnMouseDown = Shape1MouseDown
  end
  object StartTime: TDateTimePicker
    Left = 185
    Top = 54
    Width = 92
    Height = 21
    Date = 37415.499305555600000000
    Time = 37415.499305555600000000
    Kind = dtkTime
    TabOrder = 3
  end
  object EndTime: TDateTimePicker
    Left = 303
    Top = 54
    Width = 90
    Height = 21
    Date = 37415.499305555600000000
    Time = 37415.499305555600000000
    Kind = dtkTime
    TabOrder = 4
  end
  object CBShape: TComboBox
    Left = 61
    Top = 175
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
    TabOrder = 1
  end
  object Notes: TMemo
    Left = 62
    Top = 81
    Width = 331
    Height = 89
    Lines.Strings = (
      '')
    TabOrder = 5
  end
  object PlanDate: TDateTimePicker
    Left = 62
    Top = 54
    Width = 87
    Height = 21
    Date = 37416.510760300900000000
    Time = 37416.510760300900000000
    TabOrder = 2
  end
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
    object Panel1: TPanel
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
    Top = 204
    Width = 405
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 7
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
        Caption = 'C&ancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object ColorDialog: TColorDialog
    Left = 12
    Top = 101
  end
end
