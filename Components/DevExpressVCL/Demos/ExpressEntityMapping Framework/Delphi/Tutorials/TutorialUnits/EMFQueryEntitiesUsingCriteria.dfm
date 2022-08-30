inherited frmEMFQueryEntitiesUsingCriteria: TfrmEMFQueryEntitiesUsingCriteria
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited pnlData: TPanel
      Top = 151
      Height = 530
      TabOrder = 6
      inherited mResults: TcxMemo
        Height = 530
      end
    end
    object btnSelectTop: TcxButton [1]
      Left = 176
      Top = 72
      Width = 75
      Height = 25
      Caption = 'TOP Clause Query'
      TabOrder = 2
      OnClick = btnSelectTopClick
    end
    object btnSelectTopAndSkip: TcxButton [2]
      Left = 257
      Top = 72
      Width = 75
      Height = 25
      Caption = 'TOP and SKIP Clauses'#39' Query'
      TabOrder = 3
      OnClick = btnSelectTopAndSkipClick
    end
    object btnSelectOrderBy: TcxButton [3]
      Left = 95
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Sorted Query'
      TabOrder = 1
      OnClick = btnSelectOrderByClick
    end
    object btnCriteriaParser: TcxButton [4]
      Left = 14
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Parsed Criteria String Query'
      TabOrder = 0
      OnClick = btnCriteriaParserClick
    end
    object btnBetweenOperator: TcxButton [5]
      Left = 338
      Top = 72
      Width = 75
      Height = 25
      Caption = 'BETWEEN Operator Query'
      TabOrder = 4
      OnClick = btnBetweenOperatorClick
    end
    object btnInOperator: TcxButton [6]
      Left = 419
      Top = 72
      Width = 75
      Height = 25
      Caption = 'IN Operator Query'
      TabOrder = 5
      OnClick = btnInOperatorClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 5
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnSelectTop
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnSelectTopAndSkip
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnSelectOrderBy
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnCriteriaParser
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnBetweenOperator
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnInOperator
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 5
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
