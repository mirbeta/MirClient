inherited frmEMFQueryEntitiesUsingLINQ: TfrmEMFQueryEntitiesUsingLINQ
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited pnlData: TPanel
      Top = 151
      Height = 530
      TabOrder = 5
      inherited mResults: TcxMemo
        Height = 530
      end
    end
    object btnSelect: TcxButton [1]
      Left = 14
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Query All Table Records'
      TabOrder = 0
      OnClick = btnSelectClick
    end
    object btnComplexCondition: TcxButton [2]
      Left = 176
      Top = 72
      Width = 112
      Height = 25
      Caption = 'Criteria-Based Query'
      TabOrder = 2
      OnClick = btnComplexConditionClick
    end
    object btnTake: TcxButton [3]
      Left = 294
      Top = 72
      Width = 112
      Height = 25
      Caption = 'TAKE Function Query'
      TabOrder = 3
      OnClick = btnTakeClick
    end
    object btnTakeAndSkip: TcxButton [4]
      Left = 412
      Top = 72
      Width = 112
      Height = 25
      Caption = 'TAKE and SKIP Functions'#39' Query'
      TabOrder = 4
      OnClick = btnTakeAndSkipClick
    end
    object btnSorted: TcxButton [5]
      Left = 95
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Sorted Query'
      TabOrder = 1
      OnClick = btnSortedClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 4
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnSelect
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnComplexCondition
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnTake
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnTakeAndSkip
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnSorted
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
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
