inherited frmEMFDeleteEntityOneToMany: TfrmEMFDeleteEntityOneToMany
  Caption = ''
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited pnlData: TPanel
      Top = 147
      Height = 538
      TabOrder = 1
      inherited mResults: TcxMemo
        Height = 538
      end
    end
    object btnDeleteCascade: TcxButton [1]
      Left = 10
      Top = 72
      Width = 199
      Height = 25
      Caption = 'Cascade Delete Entity Objects'
      TabOrder = 0
      OnClick = btnDeleteCascadeClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnDeleteCascade
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 199
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited lcTutorialInfo: TdxLayoutControl
    inherited cxLabel1: TcxLabel
      Style.IsFontAssigned = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    inherited dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
