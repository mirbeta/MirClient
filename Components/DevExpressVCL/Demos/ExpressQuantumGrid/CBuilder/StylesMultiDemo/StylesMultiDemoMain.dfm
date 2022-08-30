inherited StylesMultiDemoMainForm: TStylesMultiDemoMainForm
  Left = 270
  Top = 120
  Caption = 'ExpressQuantumGrid StylesMulti Demo'
  ClientHeight = 508
  ClientWidth = 793
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 793
    Caption = 
      'Practice using Style Sheets. Click '#39'About this demo'#39' for more in' +
      'formation.'
  end
  object Splitter: TSplitter [1]
    Left = 177
    Top = 16
    Width = 2
    Height = 473
    MinSize = 4
  end
  inherited sbMain: TStatusBar
    Top = 489
    Width = 793
  end
  object cxGrid: TcxGrid [3]
    Left = 179
    Top = 16
    Width = 614
    Height = 473
    Align = alClient
    TabOrder = 2
    object tvProjects: TcxGridDBTableView
      DataController.DataSource = StylesMultiDemoMainDM.dsProjects
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvProjectsNAME
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.Indicator = True
      object tvProjectsID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvProjectsNAME: TcxGridDBColumn
        DataBinding.FieldName = 'NAME'
        Width = 200
      end
      object tvProjectsMANAGERID: TcxGridDBColumn
        DataBinding.FieldName = 'MANAGERID'
        RepositoryItem = StylesMultiDemoMainDM.edrepUserInfo
        Width = 200
      end
    end
    object tvTeam: TcxGridDBTableView
      DataController.DataSource = StylesMultiDemoMainDM.dsTeam
      DataController.DetailKeyFieldNames = 'PROJECTID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvTeamFUNCTION
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.Indicator = True
      object tvTeamPROJECTID: TcxGridDBColumn
        DataBinding.FieldName = 'PROJECTID'
        Visible = False
      end
      object tvTeamFUNCTION: TcxGridDBColumn
        DataBinding.FieldName = 'FUNCTION'
        Width = 150
      end
      object tvTeamUSERID: TcxGridDBColumn
        DataBinding.FieldName = 'USERID'
        RepositoryItem = StylesMultiDemoMainDM.edrepUserInfo
        Width = 200
      end
    end
    object lvProjects: TcxGridLevel
      GridView = tvProjects
      object lvTeam: TcxGridLevel
        GridView = tvTeam
      end
    end
  end
  object pnlLeft: TPanel [4]
    Left = 0
    Top = 16
    Width = 177
    Height = 473
    Align = alLeft
    Anchors = [akLeft]
    BevelInner = bvLowered
    BevelOuter = bvNone
    Color = 15451300
    TabOrder = 1
    object gbUserDefined: TcxGroupBox
      Left = 1
      Top = 316
      TabStop = True
      Align = alBottom
      Caption = 'User Defined Style Sheets'
      Enabled = False
      TabOrder = 3
      Height = 156
      Width = 175
      object cbUserStyleSheets: TcxComboBox
        Left = 10
        Top = 31
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        Properties.DropDownListStyle = lsEditFixedList
        TabOrder = 0
        OnClick = cbUserStyleSheetsChange
        Width = 154
      end
      object btnLoad: TcxButton
        Left = 11
        Top = 89
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&LoadFromFile...'
        Enabled = False
        TabOrder = 2
        OnClick = btnLoadClick
      end
      object btnSave: TcxButton
        Left = 11
        Top = 116
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&SaveToFile...'
        Enabled = False
        TabOrder = 3
        OnClick = btnSaveClick
      end
      object btnEdit: TcxButton
        Left = 11
        Top = 62
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Edit Style Sheet'
        Enabled = False
        TabOrder = 1
        OnClick = btnEditClick
      end
    end
    object RadioGroup: TcxRadioGroup
      Left = 1
      Top = 36
      Align = alTop
      Caption = 'Use styles'
      Properties.Items = <
        item
          Caption = 'None'
        end
        item
          Caption = 'Predefined'
        end
        item
          Caption = 'User defined'
        end>
      ItemIndex = 1
      TabOrder = 1
      OnClick = RadioGroupClick
      Height = 88
      Width = 175
    end
    object gbPredefined: TcxGroupBox
      Left = 1
      Top = 124
      TabStop = True
      Align = alClient
      Caption = 'Predefined Style Sheets'
      TabOrder = 2
      Height = 192
      Width = 175
      object lbPredefinedStyleSheets: TcxListBox
        Left = 2
        Top = 18
        Width = 171
        Height = 172
        Align = alClient
        ItemHeight = 13
        Style.Color = 16247513
        TabOrder = 0
        OnClick = lbPredefinedStyleSheetsClick
      end
    end
    object pnlCurrentStyleSheet: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 35
      Align = alTop
      BevelOuter = bvLowered
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid
    PopupMenus = <>
    Left = 440
    Top = 8
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 536
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 568
    Top = 8
  end
end
