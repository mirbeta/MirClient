inherited cxGridWizardCommonCustomizeItemsPageFrame: TcxGridWizardCommonCustomizeItemsPageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 415
      Height = 415
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnCustomizationForm: TPanel [1]
      Left = 402
      Top = 10
      Width = 255
      Height = 415
      BevelOuter = bvNone
      TabOrder = 2
    end
    object SplitterCustomizeItemsPage: TcxSplitter [2]
      Left = 394
      Top = 10
      Width = 8
      Height = 20
      HotZoneClassName = 'TcxXPTaskBarStyle'
      HotZone.SizePercent = 100
      HotZone.Visible = False
      AlignSplitter = salRight
      MinSize = 100
      Control = pnCustomizationForm
      OnMoved = SplitterCustomizeItemsPageMoved
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      Index = -1
    end
    object lcPreviewGrid: TdxLayoutItem
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Panel1'
      CaptionOptions.Visible = False
      Parent = lcMainGroup_Root
      Control = pnPreviewGrid
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCustomizationForm: TdxLayoutItem
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Panel1'
      CaptionOptions.Visible = False
      Parent = lcMainGroup_Root
      Control = pnCustomizationForm
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciSplitterCustomizeItemsPage: TdxLayoutItem
      AlignVert = avClient
      CaptionOptions.Text = 'cxSplitter1'
      CaptionOptions.Visible = False
      Parent = lcMainGroup_Root
      Control = SplitterCustomizeItemsPage
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
end
