inherited frmEMFCreateEntity: TfrmEMFCreateEntity
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
    object btnAddFilm: TcxButton [1]
      Left = 14
      Top = 72
      Width = 156
      Height = 25
      Caption = 'Add Film to Database'
      TabOrder = 0
      OnClick = btnAddFilmClick
    end
    object btnAddMail: TcxButton [2]
      Left = 176
      Top = 72
      Width = 156
      Height = 25
      Caption = 'Add Mail to Database'
      TabOrder = 1
      OnClick = btnAddMailClick
    end
    object btnAddToCollection: TcxButton [3]
      Left = 338
      Top = 72
      Width = 156
      Height = 25
      Caption = 'Add Film to Collection'
      TabOrder = 2
      OnClick = btnAddToCollectionClick
    end
    object btnRemoveFromCollection: TcxButton [4]
      Left = 500
      Top = 72
      Width = 156
      Height = 25
      Caption = 'Remove Film from Collection'
      TabOrder = 3
      OnClick = btnRemoveFromCollectionClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited liDescription: TdxLayoutLabeledItem
      CaptionOptions.Text = 
        'This tutorial shows how to persist entities with auto-generated ' +
        '(Film) and manually generated (Mail) keys, and manage entity obj' +
        'ects in a collection.'
    end
    inherited lgActions: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited lgConnect: TdxLayoutGroup
      ItemIndex = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnAddFilm
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 156
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnAddMail
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 156
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnAddToCollection
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 156
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnRemoveFromCollection
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 156
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
