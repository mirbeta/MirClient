object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Monitor View'
  ClientHeight = 247
  ClientWidth = 736
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object scb: TScrollBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 730
    Height = 241
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    ExplicitWidth = 686
  end
end
