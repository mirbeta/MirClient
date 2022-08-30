inherited frmEMFQueryEntity: TfrmEMFQueryEntity
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
    object btnGetByKey: TcxButton [1]
      Left = 14
      Top = 72
      Width = 100
      Height = 25
      Caption = 'Key Value Query'
      TabOrder = 0
      OnClick = btnGetByKeyClick
    end
    object btnGetByCriteria: TcxButton [2]
      Left = 120
      Top = 72
      Width = 100
      Height = 25
      Caption = 'Criteria Operator Query'
      TabOrder = 1
      OnClick = btnGetByCriteriaClick
    end
    object btnGetByPredicate: TcxButton [3]
      Left = 226
      Top = 72
      Width = 100
      Height = 25
      Caption = 'Predicate Query'
      TabOrder = 2
      OnClick = btnGetByPredicateClick
    end
    object btnGetByLINQ: TcxButton [4]
      Left = 332
      Top = 72
      Width = 100
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
      Control = btnGetByKey
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetByCriteria
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetByPredicate
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnGetByLINQ
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 100
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
