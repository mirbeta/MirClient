inherited frmEMFUpdateEntity: TfrmEMFUpdateEntity
  Caption = ''
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited pnlData: TPanel
      Top = 151
      Height = 530
      TabOrder = 2
      inherited mResults: TcxMemo
        Height = 530
      end
    end
    object btnModifyLocalEntity: TcxButton [1]
      Left = 14
      Top = 72
      Width = 140
      Height = 25
      Caption = 'Update Entity Objects Locally'
      TabOrder = 0
      OnClick = btnModifyLocalEntityClick
    end
    object btnModifyDBEntity: TcxButton [2]
      Left = 160
      Top = 72
      Width = 140
      Height = 25
      Caption = 'Update Entity Objects in Database'
      TabOrder = 1
      OnClick = btnModifyDBEntityClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnModifyLocalEntity
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnModifyDBEntity
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 1
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
