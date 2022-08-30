object cxViewEditor: TcxViewEditor
  Left = 742
  Top = 104
  Caption = 'cxViewEditor'
  ClientHeight = 104
  ClientWidth = 136
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PViewEditor: TPanel
    Left = 0
    Top = 0
    Width = 136
    Height = 104
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object lcMain: TdxLayoutControl
      Left = 0
      Top = 0
      Width = 136
      Height = 104
      Align = alClient
      TabOrder = 0
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      object lcMainGroup_Root: TdxLayoutGroup
        AlignHorz = ahClient
        AlignVert = avClient
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 8
    Top = 8
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
