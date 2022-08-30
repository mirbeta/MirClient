inherited cxGridWizardLayoutViewOptionsInterfaceElementsPageFrame: TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame
  inherited lcMain: TdxLayoutControl
    object pnPreviewGrid: TPanel [0]
      Left = 10
      Top = 10
      Width = 345
      Height = 430
      BevelOuter = bvNone
      Caption = 'pnPreviewGrid'
      TabOrder = 0
    end
    object chbRecordCaption: TcxCheckBox [1]
      Left = 361
      Top = 10
      Caption = 'Record caption'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Width = 248
    end
    object chbNavigator: TcxCheckBox [2]
      Left = 361
      Top = 33
      Caption = 'Navigator'
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbNavigatorClick
      Width = 248
    end
    object chcbNavigatorButtons: TcxCheckComboBox [3]
      Left = 418
      Top = 56
      AutoSize = False
      Properties.Items = <>
      Properties.OnChange = RefreshPreviewGrid
      Style.HotTrack = False
      TabOrder = 3
      Height = 21
      Width = 121
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      Index = -1
    end
    object lciPreviewGrid: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Panel1'
      CaptionOptions.Visible = False
      Control = pnPreviewGrid
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRecordCaption: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbRecordCaption
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciNavigator: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chbNavigator
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      SizeOptions.Width = 269
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lciNavigatorButtons: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Buttons'
      Offsets.Left = 16
      Control = chcbNavigatorButtons
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
end
