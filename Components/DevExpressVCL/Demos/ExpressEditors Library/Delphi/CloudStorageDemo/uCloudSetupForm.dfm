object fmCloudSetupWizard: TfmCloudSetupWizard
  Left = 0
  Top = 0
  Caption = 'Specify Authorization Settings'
  ClientHeight = 559
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl2: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 528
    Height = 559
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object reGoogleApi: TcxRichEdit
      Left = 10000
      Top = 10000
      Properties.AutoURLDetect = True
      Properties.ReadOnly = True
      Properties.RichEditClass = recRichEdit20
      Properties.ScrollBars = ssVertical
      Properties.OnURLClick = reURLClick
      Style.HotTrack = False
      TabOrder = 4
      Visible = False
      Height = 281
      Width = 480
    end
    object teGoogleApiClientSecret: TcxTextEdit
      Left = 10000
      Top = 10000
      Properties.EchoMode = eemPassword
      Properties.OnChange = teChange
      Style.HotTrack = False
      TabOrder = 6
      Visible = False
      Width = 410
    end
    object teGoogleApiClientID: TcxTextEdit
      Left = 10000
      Top = 10000
      Properties.OnChange = teChange
      Style.HotTrack = False
      TabOrder = 5
      Visible = False
      Width = 410
    end
    object reMSGraph: TcxRichEdit
      Left = 24
      Top = 169
      Properties.AutoURLDetect = True
      Properties.ReadOnly = True
      Properties.RichEditClass = recRichEdit20
      Properties.ScrollBars = ssVertical
      Properties.OnURLClick = reURLClick
      Style.HotTrack = False
      TabOrder = 1
      Height = 281
      Width = 480
    end
    object teMSGraphClientID: TcxTextEdit
      Left = 94
      Top = 456
      Properties.OnChange = teChange
      Style.HotTrack = False
      TabOrder = 2
      Width = 410
    end
    object teMSGraphClientSecret: TcxTextEdit
      Left = 94
      Top = 483
      Properties.EchoMode = eemPassword
      Properties.OnChange = teChange
      Style.HotTrack = False
      TabOrder = 3
      Width = 410
    end
    object btnStart: TcxButton
      Left = 362
      Top = 524
      Width = 75
      Height = 25
      Caption = 'Start Demo'
      Enabled = False
      ModalResult = 1
      TabOrder = 7
    end
    object btnCancel: TcxButton
      Left = 443
      Top = 524
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 8
    end
    object reAbout: TcxRichEdit
      Left = 10
      Top = 10
      ParentFont = False
      Properties.ReadOnly = True
      Properties.RichEditClass = recRichEdit20
      Properties.ScrollBars = ssVertical
      Lines.Strings = (
        
          'This demo uses OAuth 2.0 authorization to access calendar data a' +
          'nd basic user '
        
          'information using web service APIs. The authorization requires t' +
          'hat you specify '
        
          'client application credentials (client ID and secret) for each w' +
          'eb service API '
        
          'you'#39're going to use. You also need to authenticate yourself as a' +
          ' user to the web '
        'service to obtain any information from it.')
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -15
      Style.Font.Name = 'Calibri'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 0
      Height = 119
      Width = 508
    end
    object dxLayoutGroup1: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      Control = reGoogleApi
      ControlOptions.OriginalHeight = 161
      ControlOptions.OriginalWidth = 299
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Client Secret:'
      Control = teGoogleApiClientSecret
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Client ID:'
      Control = teGoogleApiClientID
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Google API Authorization'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'Microsoft Graph API Authorization'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignVert = avClient
      Control = reMSGraph
      ControlOptions.OriginalHeight = 134
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Client ID:'
      Control = teMSGraphClientID
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Client Secret:'
      Control = teMSGraphClientSecret
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnStart
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup1
      Control = reAbout
      ControlOptions.OriginalHeight = 119
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
