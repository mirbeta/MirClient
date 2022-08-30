inherited MasterDetailTableDemoMainForm: TMasterDetailTableDemoMainForm
  Left = 100
  Top = 40
  Caption = 'ExpressQuantumGrid Master Detail Table Demo'
  ClientHeight = 573
  ClientWidth = 887
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 887
    Height = 64
    Caption = 
      'This demo shows how to use the ExpressQuantumGrid control and th' +
      'e TdxEMFDataSet component shipped with the ExpressEntityMapping ' +
      'Framework to visualize master-detail relationships. The demo is ' +
      'equivalent to the MasterDetailTableDemo shipped with the Express' +
      'QuantumGrid Suite but replaces TClientDataSet components with Td' +
      'xEMFDataSet components to bind grid Views to data. Click '#39'About ' +
      'this demo'#39' for more information.'
  end
  object Bevel1: TBevel [1]
    Left = 0
    Top = 92
    Width = 887
    Height = 5
    Align = alTop
    Shape = bsTopLine
  end
  object Splitter: TSplitter [2]
    Left = 0
    Top = 288
    Width = 887
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    Visible = False
  end
  inherited sbMain: TStatusBar
    Top = 290
    Width = 887
  end
  object lblStyle: TcxLabel [4]
    Left = 0
    Top = 64
    Align = alTop
    Caption = 'Standard master-detail style'
    ParentColor = False
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -21
    Style.Font.Name = 'Arial'
    Style.Font.Style = []
    Style.TextColor = clNavy
    Style.IsFontAssigned = True
    Properties.Alignment.Horz = taCenter
    Transparent = True
    AnchorX = 444
  end
  object lblMaster: TcxLabel [5]
    Left = 0
    Top = 97
    Align = alTop
    Caption = 'Master : Films'
    ParentColor = False
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -19
    Style.Font.Name = 'Arial'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    Transparent = True
  end
  object pnlDetail: TPanel [6]
    Left = 0
    Top = 309
    Width = 887
    Height = 264
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'pnlDetail'
    TabOrder = 0
    object lblDetail: TcxLabel
      Left = 0
      Top = 0
      Align = alTop
      Caption = 'Detail: People involved with the film'
      ParentColor = False
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -19
      Style.Font.Name = 'Arial'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Transparent = True
    end
    object GridDetail: TcxGrid
      Left = 0
      Top = 26
      Width = 887
      Height = 238
      Align = alClient
      TabOrder = 0
      RootLevelOptions.DetailFrameColor = 16247513
      object tvFilmsPersonsStaff: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = FilmsDemoDM.dsFilmsPersons
        DataController.KeyFieldNames = 'ID'
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        NewItemRow.Visible = True
        OptionsBehavior.FocusCellOnTab = True
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.Indicator = True
        Styles.StyleSheet = GridTableViewStyleSheetDevExpress
        object tvFilmsPersonsStaffPERSONLINEID: TcxGridDBColumn
          Caption = 'Occupation'
          DataBinding.FieldName = 'PersonLineID'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'ID'
          Properties.ListColumns = <
            item
              FieldName = 'NAME'
            end>
          Properties.ListOptions.GridLines = glNone
          Properties.ListOptions.ShowHeader = False
          Properties.ListSource = FilmsDemoDM.dsPersonLines
        end
        object tvFilmsPersonsStaffPERSONID: TcxGridDBColumn
          Caption = 'Person'
          DataBinding.FieldName = 'PersonID'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'ID'
          Properties.ListColumns = <
            item
              FieldName = 'NAME'
            end>
          Properties.ListOptions.GridLines = glNone
          Properties.ListOptions.ShowHeader = False
          Properties.ListSource = FilmsDemoDM.dsPersons
        end
        object tvFilmsPersonsStaffDESCRIPTION: TcxGridDBColumn
          Caption = 'Description'
          DataBinding.FieldName = 'DESCRIPTION'
          Width = 300
        end
      end
      object lvDetail: TcxGridLevel
        GridView = tvFilmsPersonsStaff
      end
    end
  end
  object Grid: TcxGrid [7]
    Left = 0
    Top = 123
    Width = 887
    Height = 165
    Align = alClient
    TabOrder = 1
    object tvFilms: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataModeController.SmartRefresh = True
      DataController.DataSource = FilmsDemoDM.dsFilms
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      NewItemRow.Visible = True
      OptionsBehavior.FocusCellOnTab = True
      OptionsView.CellAutoHeight = True
      OptionsView.Indicator = True
      Preview.Column = tvFilmsPLOTOUTLINE
      Preview.Visible = True
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvFilmsCAPTION: TcxGridDBColumn
        Caption = 'Caption'
        DataBinding.FieldName = 'CAPTION'
      end
      object tvFilmsYEAR: TcxGridDBColumn
        Caption = 'Year'
        DataBinding.FieldName = 'YEAR'
      end
      object tvFilmsRUNTIME: TcxGridDBColumn
        Caption = 'Runtime'
        DataBinding.FieldName = 'RUNTIME'
      end
      object tvFilmsPHOTO: TcxGridDBColumn
        Caption = 'Photo'
        DataBinding.FieldName = 'PHOTO'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekPict
        Properties.PictureGraphicClassName = 'TdxSmartImage'
      end
      object tvFilmsTAGLINE: TcxGridDBColumn
        Caption = 'Tag Line'
        DataBinding.FieldName = 'TAGLINE'
        Width = 350
      end
      object tvFilmsPLOTOUTLINE: TcxGridDBColumn
        Caption = 'PlotOutline'
        DataBinding.FieldName = 'PLOTOUTLINE'
        Width = 350
      end
    end
    object lvFilms: TcxGridLevel
      GridView = tvFilms
      MaxDetailHeight = 200
      object lvFilmsPersonsStaff: TcxGridLevel
        Visible = False
      end
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miGrid: TMenuItem
        AutoCheck = True
        Caption = 'ExpressQuantumGrid Master-Detail display style'
        Hint = 
          'ExpressQuantumGrid master-detail display style Combines two sepa' +
          'rate grid views into the master-detail representation'
        OnClick = miGridClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
end
