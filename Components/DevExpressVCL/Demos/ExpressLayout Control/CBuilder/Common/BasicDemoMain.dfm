object frmBasicDemoMain: TfrmBasicDemoMain
  Left = 165
  Top = 126
  AutoScroll = False
  Caption = 'Editors Demo'
  ClientHeight = 634
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 782
    Height = 634
    Align = alClient
    TabOrder = 0
    CustomizeFormTabbedView = True
    HighlightRoot = False
    object lcMainGroup_Root1: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
  end
  object mmMain: TMainMenu
    Left = 680
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Customization1: TMenuItem
        Caption = '&Customization'
        ShortCut = 113
        OnClick = Customization1Click
      end
      object Autosize1: TMenuItem
        Action = aAutosize
        AutoCheck = True
      end
    end
    object miStyle: TMenuItem
      Caption = '&Style'
      object Standard1: TMenuItem
        Action = acLayoutStandard
        AutoCheck = True
        GroupIndex = 1
      end
      object Office1: TMenuItem
        Tag = 1
        Action = acLayoutOffice
        AutoCheck = True
        GroupIndex = 1
      end
      object Web1: TMenuItem
        Tag = 2
        Action = acLayoutWeb
        AutoCheck = True
        GroupIndex = 1
      end
      object UsecxLookAndFeel1: TMenuItem
        Tag = 3
        Caption = 'Use cxLookAndFeel'
        GroupIndex = 1
        object UltraFlat1: TMenuItem
          Tag = 2
          Action = acUltraFlat
          AutoCheck = True
          GroupIndex = 2
        end
        object Flat1: TMenuItem
          Action = acFlat
          AutoCheck = True
          GroupIndex = 2
        end
        object Standard2: TMenuItem
          Tag = 1
          Action = acStandard
          AutoCheck = True
          GroupIndex = 2
        end
        object Office111: TMenuItem
          Tag = 3
          Action = acOffice11
          AutoCheck = True
          GroupIndex = 2
        end
        object Native1: TMenuItem
          Tag = 4
          Action = acNative
          AutoCheck = True
          GroupIndex = 2
        end
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object DeveloperExpressProducts1: TMenuItem
        Caption = 'Developer Express &Products'
        OnClick = DeveloperExpressProducts1Click
      end
      object DeveloperExpressDownloads1: TMenuItem
        Tag = 1
        Caption = 'Developer Express &Downloads'
        OnClick = DeveloperExpressProducts1Click
      end
      object DeveloperExpressontheWeb1: TMenuItem
        Tag = 2
        Caption = 'Developer Express on the &Web'
        OnClick = DeveloperExpressProducts1Click
      end
      object SupportCenter1: TMenuItem
        Tag = 3
        Caption = 'Support &Center'
        OnClick = DeveloperExpressProducts1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Aboutthisdemo1: TMenuItem
        Caption = '&About this demo'
        ShortCut = 112
        OnClick = Aboutthisdemo1Click
      end
    end
  end
  object alMain: TActionList
    Left = 648
    Top = 40
    object acLayoutStandard: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Standard'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acLayoutOffice: TAction
      Tag = 1
      Category = 'View'
      AutoCheck = True
      Caption = 'Office'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acLayoutWeb: TAction
      Tag = 2
      Category = 'View'
      AutoCheck = True
      Caption = 'Web'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acFlat: TAction
      Tag = 3
      Category = 'View'
      AutoCheck = True
      Caption = 'Flat'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acStandard: TAction
      Tag = 4
      Category = 'View'
      AutoCheck = True
      Caption = 'Standard'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acUltraFlat: TAction
      Tag = 5
      Category = 'View'
      AutoCheck = True
      Caption = 'Ultra Flat'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acOffice11: TAction
      Tag = 6
      Category = 'View'
      AutoCheck = True
      Caption = 'Office11'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object acNative: TAction
      Tag = 7
      Category = 'View'
      AutoCheck = True
      Caption = 'Native Style'
      GroupIndex = 1
      OnExecute = LayoutStyleExecute
    end
    object aAutosize: TAction
      AutoCheck = True
      Caption = '&Autosize'
      OnExecute = aAutosizeExecute
    end
  end
end
