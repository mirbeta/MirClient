inherited frmEMFQueryEntities: TfrmEMFQueryEntities
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited pnlData: TPanel
      Top = 151
      Height = 530
      TabOrder = 4
      inherited mResults: TcxMemo
        Height = 530
      end
    end
    object btnGetCollection: TcxButton [1]
      Left = 14
      Top = 72
      Width = 140
      Height = 25
      Caption = 'Query All Table Records'
      TabOrder = 0
      OnClick = btnGetCollectionClick
    end
    object btnGetByCriteria: TcxButton [2]
      Left = 160
      Top = 72
      Width = 140
      Height = 25
      Caption = 'Criteria Operator Query'
      TabOrder = 1
      OnClick = btnGetByCriteriaClick
    end
    object btnGetByPredicate: TcxButton [3]
      Left = 306
      Top = 72
      Width = 140
      Height = 25
      Caption = 'Predicate Query'
      TabOrder = 2
      OnClick = btnGetByPredicateClick
    end
    object btnGetByLINQ: TcxButton [4]
      Left = 452
      Top = 72
      Width = 140
      Height = 25
      Caption = 'LINQ Expression Query'
      TabOrder = 3
      OnClick = btnGetByLINQClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetCollection
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
      Control = btnGetByCriteria
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetByPredicate
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetByLINQ
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 3
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
