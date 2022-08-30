inherited cxGridWizardLayoutViewOptionsCustomizeItemsPageFrame: TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame
  inherited lcMain: TdxLayoutControl
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 710
      Height = 435
      BevelOuter = bvNone
      TabOrder = 0
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = -1
    end
    object lcMainItem1: TdxLayoutItem
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'pnPreviewGrid'
      CaptionOptions.Visible = False
      Parent = lcMainGroup_Root
      Control = pnPreviewGrid
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
end
