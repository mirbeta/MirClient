object EditorsStylesDemoBaseFrame: TEditorsStylesDemoBaseFrame
  Left = 0
  Top = 0
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 466
  ClientWidth = 692
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnResize = FrameResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDescription: TPanel
    Left = 24
    Top = 56
    Width = 129
    Height = 81
    BevelOuter = bvNone
    Caption = 'pnlDescription'
    ParentColor = True
    TabOrder = 0
    Visible = False
    object memDescrip: TcxMemo
      Left = 0
      Top = 0
      Width = 129
      Height = 81
      Align = alClient
      Lines.Strings = (
        'memDescrip')
      ParentColor = True
      Properties.ScrollBars = ssVertical
      Properties.WordWrap = False
      Style.BorderStyle = ebsNone
      TabOrder = 0
    end
  end
  object cxEditStyleController: TcxEditStyleController
    Left = 80
    Top = 8
  end
  object cxPropertiesStore: TcxPropertiesStore
    Active = False
    Components = <>
    StorageName = 'cxPropertiesStore1'
    Left = 12
    Top = 8
  end
  object cxLabelStyleController: TcxEditStyleController
    Left = 120
    Top = 8
  end
  object FlickerTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = FlickerTimerTimer
    Left = 192
    Top = 8
  end
end
