inherited frmEMFUsingDataset: TfrmEMFUsingDataset
  Caption = ''
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    ParentShowHint = False
    ShowHint = True
    inherited pnlData: TPanel
      Top = 151
      Height = 530
      TabOrder = 2
      inherited mResults: TcxMemo
        TabOrder = 1
        Height = 530
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 987
        Height = 530
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Title'
            Title.Alignment = taCenter
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Year'
            Title.Alignment = taCenter
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Runtime'
            Title.Alignment = taCenter
            Width = 300
            Visible = True
          end>
      end
    end
    object btnRefresh: TcxButton [1]
      Left = 290
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Refresh Dataset'
      TabOrder = 1
      OnClick = btnRefreshClick
    end
    object cxDBNavigator1: TcxDBNavigator [2]
      Left = 14
      Top = 72
      Width = 270
      Height = 25
      Buttons.CustomButtons = <>
      DataSource = DataSource1
      TabOrder = 0
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
      Control = btnRefresh
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgConnect
      AlignVert = avClient
      Control = cxDBNavigator1
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 270
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
  object DataSource1: TDataSource
    DataSet = dxEMFDataSet1
    Left = 888
    Top = 256
  end
  object dxEMFDataSet1: TdxEMFDataSet
    Left = 888
    Top = 208
  end
end
