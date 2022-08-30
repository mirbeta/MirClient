inherited frmEMFCustomTutorial: TfrmEMFCustomTutorial
  Caption = 'Custom Tutorial'
  ClientHeight = 695
  ClientWidth = 1015
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 1015
    Height = 695
    LayoutLookAndFeel = dxLayoutCxLookAndFeel2
    object pnlData: TPanel [0]
      Left = 14
      Top = 131
      Width = 987
      Height = 550
      BevelOuter = bvNone
      Caption = 'pnlData'
      ParentBackground = False
      TabOrder = 0
      object mResults: TcxMemo
        Left = 0
        Top = 0
        Align = alClient
        Properties.ScrollBars = ssBoth
        TabOrder = 0
        Height = 550
        Width = 987
      end
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ItemIndex = 2
    end
    inherited lgContent: TdxLayoutGroup
      CaptionOptions.Visible = False
      LayoutDirection = ldHorizontal
      Index = 2
    end
    inherited liDescription: TdxLayoutLabeledItem
      AlignVert = avTop
      CaptionOptions.Text = 'Tutorial description'
      Visible = True
      Index = 0
    end
    inherited lgTutorialContent: TdxLayoutGroup
      CaptionOptions.Text = 'Steps'
      CaptionOptions.Visible = False
      LayoutDirection = ldTabbed
    end
    object liDataGrid: TdxLayoutItem
      Parent = lgTutorialContent
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Results'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = pnlData
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 205
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgActions: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 1
    end
    object lgConnect: TdxLayoutGroup
      Parent = lgActions
      CaptionOptions.Text = 'Actions'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      WrapItemsMode = wmImmediateChildren
      Index = 0
    end
  end
  object lcTutorialInfo: TdxLayoutControl
    Left = 57
    Top = 194
    Width = 583
    Height = 445
    ParentBackground = True
    TabOrder = 1
    Transparent = True
    Visible = False
    object reStepSources: TcxRichEdit
      Left = 24
      Top = 87
      Properties.ScrollBars = ssBoth
      Lines.Strings = (
        'reTutorialSources')
      Style.Color = clWhite
      Style.HotTrack = False
      TabOrder = 1
      Height = 303
      Width = 535
    end
    object btnCopy: TcxButton
      Left = 498
      Top = 410
      Width = 75
      Height = 25
      Caption = 'Copy'
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object cxLabel1: TcxLabel
      Left = 10
      Top = 10
      Caption = 'Description:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.HotTrack = False
      Style.IsFontAssigned = True
      Transparent = True
    end
    object lcTutorialInfoGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object liActionSources: TdxLayoutItem
      Parent = lgSources
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Code'
      CaptionOptions.Visible = False
      Control = reStepSources
      ControlOptions.OriginalHeight = 205
      ControlOptions.OriginalWidth = 761
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgSources: TdxLayoutGroup
      Parent = lcTutorialInfoGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = lcTutorialInfoGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnCopy
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liDescriptionLabel: TdxLayoutItem
      Parent = lcTutorialInfoGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = cxLabel1
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lilActionDescription: TdxLayoutLabeledItem
      Parent = lcTutorialInfoGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Label'
      CaptionOptions.WordWrap = True
      Index = 1
    end
  end
  object cdsFilms: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 600
    Top = 24
    object cdsFilmsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsFilmsCAPTION: TStringField
      FieldName = 'CAPTION'
      Size = 50
    end
    object cdsFilmsYEAR: TIntegerField
      FieldName = 'YEAR'
    end
    object cdsFilmsTAGLINE: TStringField
      FieldName = 'TAGLINE'
      Size = 250
    end
    object cdsFilmsPLOTOUTLINE: TStringField
      FieldName = 'PLOTOUTLINE'
      Size = 200
    end
    object cdsFilmsRUNTIME: TIntegerField
      FieldName = 'RUNTIME'
    end
    object cdsFilmsCOLOR: TStringField
      FieldName = 'COLOR'
      Size = 50
    end
    object cdsFilmsPHOTO: TBlobField
      FieldName = 'PHOTO'
      Size = 10
    end
    object cdsFilmsICON: TBlobField
      FieldName = 'ICON'
      Size = 10
    end
    object cdsFilmsWEBSITE: TStringField
      FieldName = 'WEBSITE'
      Size = 200
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 200
    Top = 128
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      Offsets.RootItemsAreaOffsetHorz = 0
      Offsets.RootItemsAreaOffsetVert = 0
      PixelsPerInch = 96
    end
  end
  object dxUIAdornerManager1: TdxUIAdornerManager
    OnActiveChanged = dxUIAdornerManager1ActiveChanged
    OnAdornerCustomDraw = dxUIAdornerManager1AdornerCustomDraw
    OnGuideGetCalloutPopupControl = dxUIAdornerManager1GuideGetCalloutPopupControl
    Left = 752
    Top = 208
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 752
    Top = 280
  end
  object dxLayoutLookAndFeelList2: TdxLayoutLookAndFeelList
    Left = 336
    Top = 176
  end
end
