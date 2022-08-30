inherited cxGridWizardCommonSelectViewPageFrame: TcxGridWizardCommonSelectViewPageFrame
  inherited lcMain: TdxLayoutControl
    object lvGridViews: TdxGalleryControl [0]
      Left = 10
      Top = 10
      Width = 272
      Height = 186
      Images = ilGridViewImages
      OptionsBehavior.ItemCheckMode = icmSingleRadio
      OptionsView.ColumnAutoWidth = True
      OptionsView.Item.Image.ShowFrame = False
      OptionsView.Item.Text.AlignHorz = taLeftJustify
      OptionsView.Item.Text.AlignVert = vaCenter
      OptionsView.Item.Text.Position = posRight
      OptionsView.Item.Text.WordWrap = False
      OnDblClick = lvGridViewsDblClick
      OnItemClick = lvGridViewsItemClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      AlignVert = avParentManaged
      Index = -1
    end
    object lciGridViews: TdxLayoutItem
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'lvAvailableGridViews'
      CaptionOptions.Visible = False
      Parent = lcMainGroup_Root
      Control = lvGridViews
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object ilGridViewImages: TcxImageList
    Height = 32
    Width = 32
    FormatVersion = 1
    DesignInfo = 262180
  end
end
