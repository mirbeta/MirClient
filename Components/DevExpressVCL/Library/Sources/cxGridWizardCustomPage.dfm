object cxGridWizardCustomPageFrame: TcxGridWizardCustomPageFrame
  Left = 0
  Top = 0
  Width = 640
  Height = 450
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 640
    Height = 450
    Align = alClient
    ParentBackground = True
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
    end
  end
end
