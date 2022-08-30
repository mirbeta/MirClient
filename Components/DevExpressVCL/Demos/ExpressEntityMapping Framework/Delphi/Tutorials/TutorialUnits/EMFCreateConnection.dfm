inherited frmEMFCreateConnection: TfrmEMFCreateConnection
  Caption = ''
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
    object btnStep1: TcxButton [1]
      Tag = 1
      Left = 14
      Top = 72
      Width = 70
      Height = 25
      Caption = 'Step 1: Create a Connection'
      TabOrder = 0
      OnClick = btnStep1Click
    end
    object btnStep2: TcxButton [2]
      Tag = 2
      Left = 90
      Top = 72
      Width = 70
      Height = 25
      Caption = 'Step 2: Create a Data Provider'
      TabOrder = 1
      OnClick = btnStep2Click
    end
    object btnStep3: TcxButton [3]
      Tag = 3
      Left = 166
      Top = 72
      Width = 70
      Height = 25
      Caption = 'Step 3: Create a Session'
      TabOrder = 2
      OnClick = btnStep3Click
    end
    object btnStep4: TcxButton [4]
      Tag = 4
      Left = 242
      Top = 72
      Width = 70
      Height = 25
      Caption = 'Step 4: Retrieve Data'
      TabOrder = 3
      OnClick = btnStep4Click
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited liDataGrid: TdxLayoutItem
      ControlOptions.OriginalHeight = 302
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 3
    end
    object liShowData: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnStep1
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnStep2
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnStep3
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnStep4
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 70
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
