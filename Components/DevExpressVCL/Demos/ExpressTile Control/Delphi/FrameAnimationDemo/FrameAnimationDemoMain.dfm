object fmFrameAnimationMain: TfmFrameAnimationMain
  Left = 428
  Top = 168
  Width = 1068
  Height = 653
  Caption = 'FrameAnimationDemoMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tcMain: TdxTileControl
    Left = 0
    Top = 0
    Width = 1052
    Height = 595
    Constraints.MinWidth = 116
    OnContextPopup = tcMainContextPopup
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsView.CenterContentHorz = True
    OptionsView.CenterContentVert = True
    OptionsView.ItemSize = 170
    OptionsView.GroupMaxRowCount = 5
    TabOrder = 0
    object dxTiledxTileControlGroup1: TdxTileControlGroup
      Index = 0
    end
    object dxTiledxTileControlGroup2: TdxTileControlGroup
      Index = 1
    end
    object tiHouses: TdxTileControlItem
      AnimationInterval = 2000
      DetailOptions.Caption = 'Listing'
      GroupIndex = 0
      IndexInGroup = 0
      IsLarge = True
      RowCount = 2
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = 536870912
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
    end
    object tiInteriors: TdxTileControlItem
      Tag = 2
      AnimationInterval = 5000
      DetailOptions.Caption = 'Agents'
      GroupIndex = 1
      IndexInGroup = 0
      IsLarge = True
      Style.BorderColor = 543581798
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = 536870912
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.GradientBeginColor = 543581798
      Style.GradientEndColor = clNone
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
    end
    object tiAgents: TdxTileControlItem
      Tag = 3
      GroupIndex = 1
      IndexInGroup = 1
      IsLarge = True
      Style.BorderColor = 548433021
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = 536870912
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.GradientBeginColor = 547508838
      Style.GradientEndColor = clNone
      Glyph.AlignWithText = itaLeft
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
    end
  end
  object mmMain: TMainMenu
    Left = 104
    Top = 88
    object miFile: TMenuItem
      Caption = '&File'
      object miExit: TMenuItem
        Caption = '&Exit'
        ShortCut = 32856
        OnClick = miExitClick
      end
    end
    object miOptions: TMenuItem
      Caption = '&Options'
      object miScrollMode: TMenuItem
        Caption = 'Scroll Mode'
      end
      object miCenterContent: TMenuItem
        Caption = 'Center Content'
        object miCenterContentHorz: TMenuItem
          Caption = 'Horizontally'
          OnClick = miCenterContentHorzClick
        end
        object miCenterContentVert: TMenuItem
          Tag = 1
          Caption = 'Vertically'
          OnClick = miCenterContentVertClick
        end
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
    end
  end
  object pmItemAnimate: TPopupMenu
    OnPopup = pmItemAnimatePopup
    Left = 48
    Top = 216
    object pmAnimationInterval: TMenuItem
      Caption = 'Animation Interval (ms)'
    end
    object pmAnimationMode: TMenuItem
      Caption = 'Animation Mode'
    end
    object pmAnimateText: TMenuItem
      Caption = 'Animate Text with Frames'
      OnClick = pmAnimateTextClick
    end
  end
  object cxLookAndFeelController: TcxLookAndFeelController
    Left = 392
    Top = 8
  end
end
