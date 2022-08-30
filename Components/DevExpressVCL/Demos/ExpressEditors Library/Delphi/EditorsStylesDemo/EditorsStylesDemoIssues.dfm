inherited EditorsStylesDemoIssuesFrame: TEditorsStylesDemoIssuesFrame
  Left = 321
  Top = 118
  Color = 15063997
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel [0]
    Left = 0
    Top = 41
    Width = 425
    Height = 425
    Align = alLeft
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    object lblDBIssueID: TcxDBLabel
      Left = 65
      Top = 0
      Width = 10
      Height = 17
      Hint = 'cxLabel'
      AutoSize = True
      DataBinding.DataField = 'ID'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxEditStyleController
    end
    object chcbIDEs: TcxDBCheckComboBox
      Left = 65
      Top = 297
      Width = 355
      Height = 21
      Hint = 'cxDBCheckComboBox'
      DataBinding.DataField = 'IDES'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentFont = False
      Properties.Delimiter = '; '
      Properties.DropDownSizeable = True
      Properties.Items = <
        item
          Description = 'Delphi 4'
        end
        item
          Description = 'Delphi 5'
        end
        item
          Description = 'Delphi 6'
        end
        item
          Description = 'CBuilder 4'
        end
        item
          Description = 'CBuilder 5'
        end
        item
          Description = 'CBuilder 5'
        end>
      Properties.ReadOnly = False
      Style.StyleController = cxEditStyleController
      TabOrder = 1
      Text = 'Delphi 5; CBuilder 4'
    end
    object lbProject: TcxLabel
      Left = 8
      Top = 40
      Width = 46
      Height = 17
      Hint = 'cxLabel'
      Caption = ' Project: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object lblIssueID: TcxLabel
      Left = 8
      Top = 0
      Width = 52
      Height = 17
      Hint = 'cxLabel'
      Caption = ' Issue ID: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object edProject: TcxDBLookupComboBox
      Left = 65
      Top = 40
      Width = 188
      Height = 21
      Hint = 'cxDBLookupCombobox'
      DataBinding.DataField = 'PROJECTID'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentFont = False
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'NAME'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListSource = EditorsStylesDemoDataDM.dsProjects
      Style.StyleController = cxEditStyleController
      TabOrder = 4
    end
    object lbCreator: TcxLabel
      Left = 8
      Top = 80
      Width = 47
      Height = 17
      Hint = 'cxLabel'
      Caption = ' Creator: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object lbOwner: TcxLabel
      Left = 8
      Top = 120
      Width = 44
      Height = 17
      Hint = 'cxLabel'
      Caption = ' Owner: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object edCreator: TcxDBLookupComboBox
      Left = 65
      Top = 80
      Width = 188
      Height = 21
      Hint = 'cxDBLookupCombobox'
      DataBinding.DataField = 'CREATORID'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentFont = False
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'UserName'
        end>
      Properties.ListSource = EditorsStylesDemoDataDM.dsUsers
      Style.StyleController = cxEditStyleController
      TabOrder = 7
    end
    object edOwner: TcxDBLookupComboBox
      Left = 65
      Top = 120
      Width = 188
      Height = 21
      Hint = 'cxDBLookupCombobox'
      DataBinding.DataField = 'OWNERID'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentFont = False
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'UserName'
        end>
      Properties.ListSource = EditorsStylesDemoDataDM.dsUsers
      Style.StyleController = cxEditStyleController
      TabOrder = 8
    end
    object gbStatus: TcxGroupBox
      Left = 296
      Top = 0
      Width = 121
      Height = 153
      Hint = 'cxGroupBox'
      Alignment = alLeftCenter
      Caption = 'Status'
      ParentFont = False
      TabOrder = 9
      LookAndFeel.Kind = lfStandard
      object tbStatus: TcxDBTrackBar
        Left = 16
        Top = 24
        Width = 30
        Height = 121
        Hint = 'cxTrackBar'
        AutoSize = False
        DataBinding.DataField = 'STATUS'
        DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
        ParentColor = False
        ParentFont = False
        Properties.Min = 1
        Properties.Max = 4
        Properties.Orientation = tboVertical
        Properties.TrackColor = 16512239
        Properties.ThumbHeight = 10
        Properties.ThumbWidth = 5
        Style.StyleController = cxEditStyleController
        TabOrder = 2
      end
      object lbNew: TcxLabel
        Left = 45
        Top = 26
        Width = 26
        Height = 17
        Caption = 'New'
        ParentFont = False
        Style.Edges = []
      end
      object lbPostponed: TcxLabel
        Left = 46
        Top = 60
        Width = 55
        Height = 17
        Caption = 'Postponed'
        ParentFont = False
        Style.Edges = []
      end
      object lbFixed: TcxLabel
        Left = 45
        Top = 90
        Width = 29
        Height = 17
        Caption = 'Fixed'
        ParentFont = False
        Style.Edges = []
      end
      object lbRejected: TcxLabel
        Left = 45
        Top = 121
        Width = 47
        Height = 17
        Caption = 'Rejected'
        ParentFont = False
        Style.Edges = []
      end
    end
    object gbProgress: TcxGroupBox
      Left = 8
      Top = 159
      Width = 409
      Height = 130
      Hint = 'cxGroupBox'
      Caption = 'Progress'
      ParentFont = False
      TabOrder = 10
      LookAndFeel.Kind = lfStandard
      object pgbProgress: TcxDBProgressBar
        Left = 8
        Top = 17
        Width = 392
        Height = 21
        Hint = 'cxProgressBar'
        DataBinding.DataField = 'PROGRESS'
        DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
        ParentColor = False
        ParentFont = False
        Properties.BeginColor = 10719578
        Properties.EndColor = 10719578
        Properties.ShowText = False
        Properties.OverloadValue = 10.000000000000000000
        Properties.ShowOverload = True
        Properties.OverloadBeginColor = 13944994
        Properties.OverloadEndColor = 13944994
        Properties.PeakValue = 86.000000000000000000
        Properties.ShowPeak = True
        Style.StyleController = cxEditStyleController
        TabOrder = 0
      end
      object seProgress: TcxDBSpinEdit
        Left = 294
        Top = 54
        Width = 42
        Height = 21
        Hint = 'cxSpinEdit'
        DataBinding.DataField = 'PROGRESS'
        DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
        ParentFont = False
        Properties.ImmediatePost = True
        Properties.MaxValue = 100.000000000000000000
        Properties.SpinButtons.Position = sbpHorzRight
        Properties.OnChange = seProgressPropertiesChange
        Style.StyleController = cxEditStyleController
        TabOrder = 1
      end
      object lbCheckProgress: TcxLabel
        Left = 8
        Top = 54
        Width = 94
        Height = 17
        Caption = ' Checked Progress'
        ParentColor = False
        ParentFont = False
        Style.Color = 10719578
      end
      object lbProgress: TcxLabel
        Left = 227
        Top = 54
        Width = 48
        Height = 17
        Caption = ' Progress'
        ParentColor = False
        ParentFont = False
        Style.Color = 13944994
      end
      object seCheckProgress: TcxDBSpinEdit
        Left = 127
        Top = 54
        Width = 41
        Height = 21
        Hint = 'cxSpinEdit'
        DataBinding.DataField = 'CHECKPROGRESS'
        DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
        ParentFont = False
        Properties.ImmediatePost = True
        Properties.MaxValue = 100.000000000000000000
        Properties.OnChange = seCheckProgressPropertiesChange
        Style.Color = 12562821
        Style.StyleController = cxEditStyleController
        TabOrder = 4
      end
      object cxLabel1: TcxLabel
        Left = 8
        Top = 92
        Width = 57
        Height = 17
        Hint = 'cxLabel'
        Caption = 'First Target'
        ParentColor = False
        ParentFont = False
        Style.StyleController = cxLabelStyleController
      end
      object seFirstTarget: TcxDBSpinEdit
        Left = 72
        Top = 92
        Width = 73
        Height = 21
        Hint = 'cxSpinEdit'
        DataBinding.DataField = 'FIRSTTARGET'
        DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
        ParentFont = False
        Properties.ImmediatePost = True
        Properties.MaxValue = 99.000000000000000000
        Properties.MinValue = 1.000000000000000000
        Properties.SpinButtons.ShowFastButtons = True
        Properties.UseCtrlIncrement = True
        Properties.OnChange = seFirstTargetPropertiesChange
        Style.StyleController = cxEditStyleController
        TabOrder = 6
      end
    end
    object lbIDEs: TcxLabel
      Left = 8
      Top = 297
      Width = 33
      Height = 17
      Hint = 'cxLabel'
      Caption = ' IDEs:'
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object cxDBNavigator1: TcxDBNavigator
      Left = 96
      Top = 339
      Width = 285
      Height = 25
      Hint = 'cxDBNavigator'
      DataSource = EditorsStylesDemoDataDM.dsItems
      TabOrder = 12
    end
  end
  object Panel1: TPanel [1]
    Left = 0
    Top = 0
    Width = 692
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object lbIssue: TcxLabel
      Left = 8
      Top = 6
      Width = 38
      Height = 17
      Hint = 'cxLabel'
      Caption = ' Issue: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object edIssue: TcxDBTextEdit
      Left = 65
      Top = 6
      Width = 355
      Height = 21
      Hint = 'cxDBTextEdit'
      DataBinding.DataField = 'NAME'
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      ParentFont = False
      Style.StyleController = cxEditStyleController
      TabOrder = 1
    end
  end
  object pnlNotification: TPanel [2]
    Left = 425
    Top = 41
    Width = 267
    Height = 425
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlNotification'
    ParentColor = True
    TabOrder = 2
    object lbNotification: TcxLabel
      Left = 0
      Top = 0
      Width = 267
      Height = 17
      Hint = 'cxLabel'
      Align = alTop
      Caption = ' Send notification: '
      ParentColor = False
      ParentFont = False
      Style.StyleController = cxLabelStyleController
    end
    object chlbUsers: TcxDBCheckListBox
      Left = 0
      Top = 17
      Width = 267
      Height = 408
      Hint = 'cxDBCheckListBox'
      Align = alClient
      DataBinding.DataSource = EditorsStylesDemoDataDM.dsItems
      DataBinding.DataField = 'NOTIFICATIONS'
      Items = <>
      ParentColor = False
      ParentFont = False
      ReadOnly = False
      Style.Color = clWhite
      TabOrder = 1
    end
  end
  inherited pnlDescription: TPanel
    inherited memDescrip: TcxMemo
      Lines.Strings = (
        'An example of a typical form using data-aware controls.'
        ''
        
          'If you are running under XP, see the difference made to the prog' +
          'ress bar if you set native style to true (main menu: Style | Loo' +
          'k&Feel). Under XP, it is not possible to show two areas in the o' +
          'ne control. Note also that XP does not permit a vertical caption' +
          ' for the Status groupbox. '
        ''
        
          'Note the text shown by the IDE'#39's combobox and how it changes as ' +
          'you check/uncheck items using its dropdown.'
        ''
        
          'See the caption of the Status group box (left-centered, one of n' +
          'ine possible positions).'
        ''
        
          'Explore the buttons on the data-aware navigator. In particular, ' +
          'try using the three rightmost. The filter builder should be easy' +
          ' to use, but there are full instructions in the help file if you' +
          ' have any difficulty.')
    end
  end
  inherited cxEditStyleController: TcxEditStyleController
    Style.BorderColor = 10979707
    Style.ButtonStyle = btsFlat
    Style.Color = 12562821
    Style.Font.Color = clWhite
    Style.Shadow = True
  end
  inherited cxPropertiesStore: TcxPropertiesStore
    Components = <
      item
        Component = chlbUsers
        Properties.Strings = (
          'Style.BorderColor'
          'Style.BorderStyle'
          'Style.Color'
          'Style.Font'
          'Style.Shadow')
      end
      item
        Component = cxEditStyleController
        Properties.Strings = (
          'Style.AssignedValues'
          'Style.BorderColor'
          'Style.BorderStyle'
          'Style.ButtonStyle'
          'Style.ButtonTransparency'
          'Style.Color'
          'Style.Edges'
          'Style.Font'
          'Style.HotTrack'
          'Style.PopupBorderStyle'
          'Style.Shadow')
      end
      item
        Component = cxLabelStyleController
        Properties.Strings = (
          'Style.AssignedValues'
          'Style.BorderColor'
          'Style.BorderStyle'
          'Style.ButtonStyle'
          'Style.ButtonTransparency'
          'Style.Color'
          'Style.Edges'
          'Style.Font'
          'Style.HotTrack'
          'Style.PopupBorderStyle'
          'Style.Shadow')
      end
      item
        Component = Owner
        Properties.Strings = (
          'Color')
      end
      item
        Component = gbProgress
        Properties.Strings = (
          'Font'
          'LookAndFeel.AssignedValues'
          'LookAndFeel.Kind')
      end
      item
        Component = gbStatus
        Properties.Strings = (
          'Font'
          'LookAndFeel.AssignedValues'
          'LookAndFeel.Kind')
      end
      item
        Component = lbCheckProgress
        Properties.Strings = (
          'Style.Color'
          'Style.Font')
      end
      item
        Component = lbProgress
        Properties.Strings = (
          'Style.Color'
          'Style.Font')
      end
      item
        Component = pgbProgress
        Properties.Strings = (
          'Properties.BeginColor'
          'Properties.EndColor'
          'Properties.OverloadBeginColor'
          'Properties.OverloadEndColor')
      end
      item
        Component = seCheckProgress
        Properties.Strings = (
          'Style.Color'
          'Style.Font')
      end
      item
        Component = tbStatus
        Properties.Strings = (
          'Properties.SelectionColor'
          'Properties.ThumbColor'
          'Properties.ThumbHighlightColor'
          'Properties.TickColor'
          'Properties.TrackColor')
      end>
    StorageName = 'StylesFrmIssues\StyleRainyDay.ini'
  end
  inherited cxLabelStyleController: TcxEditStyleController
    Style.Color = 15063997
    Style.Edges = []
    Style.Font.Color = clBlack
    Left = 48
  end
end
