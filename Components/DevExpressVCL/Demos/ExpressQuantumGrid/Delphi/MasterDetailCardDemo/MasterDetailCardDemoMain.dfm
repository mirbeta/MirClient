inherited MasterDetailCardDemoMainForm: TMasterDetailCardDemoMainForm
  Left = 87
  Top = 90
  Caption = 'ExpressQuantumGrid Master Detail Card Demo'
  ClientHeight = 618
  ClientWidth = 891
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 891
    Height = 32
    Caption = 
      'Initially displays as separate master and detail data. Use the O' +
      'ptions in order to show both tables in a single grid. Click '#39'Abo' +
      'ut this demo'#39' for more information.'
  end
  object Bevel1: TBevel [1]
    Left = 0
    Top = 60
    Width = 891
    Height = 5
    Align = alTop
    Shape = bsTopLine
  end
  inherited sbMain: TStatusBar
    Top = 318
    Width = 891
  end
  object lblMaster: TcxLabel [3]
    Left = 0
    Top = 65
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
  object lblStyle: TcxLabel [4]
    Left = 0
    Top = 32
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
    AnchorX = 446
  end
  object Grid: TcxGrid [5]
    Left = 0
    Top = 91
    Width = 891
    Height = 227
    Align = alClient
    TabOrder = 0
    object tvFilms: TcxGridDBTableView
      Navigator.Buttons.Insert.Visible = False
      Navigator.Buttons.Delete.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Post.Visible = False
      Navigator.Buttons.Cancel.Visible = False
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.Buttons.Filter.Visible = False
      Navigator.InfoPanel.Visible = True
      Navigator.InfoPanel.Width = 80
      DataController.DataModeController.SmartRefresh = True
      DataController.DataSource = FilmsDemoDM.dsFilms
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.FocusCellOnTab = True
      OptionsView.NavigatorOffset = 0
      OptionsView.CellAutoHeight = True
      OptionsView.Indicator = True
      Preview.Column = colFilmsPlotOutline
      Preview.Visible = True
      object colFilmsCaption: TcxGridDBColumn
        DataBinding.FieldName = 'Caption'
      end
      object colFilmsYear: TcxGridDBColumn
        DataBinding.FieldName = 'Year'
      end
      object colFilmsRuntime: TcxGridDBColumn
        DataBinding.FieldName = 'Runtime'
      end
      object colFilmsPhoto: TcxGridDBColumn
        DataBinding.FieldName = 'Photo'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekPict
        Properties.PictureGraphicClassName = 'TdxSmartImage'
      end
      object colFilmsTagline: TcxGridDBColumn
        DataBinding.FieldName = 'Tagline'
        PropertiesClassName = 'TcxMemoProperties'
        Width = 350
      end
      object colFilmsPlotOutline: TcxGridDBColumn
        DataBinding.FieldName = 'PlotOutline'
      end
    end
    object cvFilmsPersons: TcxGridDBCardView
      Navigator.Buttons.Insert.Visible = False
      Navigator.Buttons.Delete.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Post.Visible = False
      Navigator.Buttons.Cancel.Visible = False
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.Buttons.Filter.Visible = False
      Navigator.InfoPanel.Visible = True
      Navigator.InfoPanel.Width = 80
      Navigator.Visible = True
      DataController.DataSource = FilmsDemoDM.dsFilmsPersons
      DataController.DetailKeyFieldNames = 'FilmID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.FocusCellOnTab = True
      OptionsView.NavigatorOffset = 0
      OptionsView.CardIndent = 7
      OptionsView.CellAutoHeight = True
      OptionsView.SeparatorColor = 12937777
      object cvFilmsPersonsName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Name'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsPersonLineID: TcxGridDBCardViewRow
        Caption = 'PersonLine'
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
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsFIRSTNAME: TcxGridDBCardViewRow
        Caption = 'FirstName'
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsSECONDNAME: TcxGridDBCardViewRow
        Caption = 'SecondName'
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsNICKNAME: TcxGridDBCardViewRow
        Caption = 'NickName'
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsDATEOFBIRTH: TcxGridDBCardViewRow
        Caption = 'DateOfBirth'
        DataBinding.FieldName = 'DATEOFBIRTH'
        PropertiesClassName = 'TcxDateEditProperties'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow
        Caption = 'LocationOfBirth'
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsBIRTHNAME: TcxGridDBCardViewRow
        Caption = 'BirthName'
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsBIOGRAPHY: TcxGridDBCardViewRow
        Caption = 'Biography'
        DataBinding.FieldName = 'BIOGRAPHY'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Properties.BlobPaintStyle = bpsText
        Properties.MemoScrollBars = ssVertical
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsGender: TcxGridDBCardViewRow
        Caption = 'Male'
        DataBinding.FieldName = 'Gender'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Position.BeginsLayer = True
      end
      object cvFilmsPersonsHOMEPAGE: TcxGridDBCardViewRow
        Caption = 'HomePage'
        DataBinding.FieldName = 'HOMEPAGE'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BeginsLayer = True
      end
    end
    object lvFilms: TcxGridLevel
      GridView = tvFilms
      Options.DetailFrameColor = clHighlight
      Options.DetailFrameWidth = 0
      object lvFilmsPersons: TcxGridLevel
        GridView = cvFilmsPersons
        Visible = False
      end
    end
  end
  object pnlDetail: TPanel [6]
    Left = 0
    Top = 337
    Width = 891
    Height = 281
    Align = alBottom
    Color = 15451300
    TabOrder = 1
    object lblDetail: TcxLabel
      Left = 1
      Top = 1
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
    object Panel1: TPanel
      Left = 1
      Top = 27
      Width = 889
      Height = 253
      Align = alClient
      BevelOuter = bvNone
      Color = 15451300
      TabOrder = 0
      object Label1: TcxLabel
        Left = 24
        Top = 7
        Caption = '&Occupation:'
        FocusControl = cbOccupation
        Transparent = True
      end
      object Label2: TcxLabel
        Left = 24
        Top = 45
        Caption = 'First &Name:'
        FocusControl = edFirstName
        Transparent = True
      end
      object Label8: TcxLabel
        Left = 24
        Top = 85
        Caption = '&Second Name:'
        FocusControl = edSecondName
        Transparent = True
      end
      object Label9: TcxLabel
        Left = 24
        Top = 125
        Caption = '&Nick Name:'
        FocusControl = edNickName
        Transparent = True
      end
      object Label7: TcxLabel
        Left = 24
        Top = 165
        Caption = 'Home &Page'
        FocusControl = edHomePage
        Transparent = True
      end
      object Label6: TcxLabel
        Left = 264
        Top = 86
        Caption = 'Birth N&ame:'
        FocusControl = edBirthName
        Transparent = True
      end
      object Label4: TcxLabel
        Left = 264
        Top = 46
        Caption = '&Location of Birth:'
        FocusControl = edLocationOfBirth
        Transparent = True
      end
      object Label3: TcxLabel
        Left = 264
        Top = 6
        Caption = 'Date of &Birth:'
        FocusControl = deDateOfBirth
        Transparent = True
      end
      object Label5: TcxLabel
        Left = 520
        Top = 21
        Caption = 'Bio&graphy'
        FocusControl = meBiography
        Transparent = True
      end
      object cbOccupation: TcxDBLookupComboBox
        Left = 24
        Top = 21
        DataBinding.DataField = 'PersonLineID'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        ParentFont = False
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListSource = FilmsDemoDM.dsPersonLines
        Style.StyleController = cxEditStyleController1
        TabOrder = 0
        Width = 185
      end
      object edFirstName: TcxDBTextEdit
        Left = 24
        Top = 61
        DataBinding.DataField = 'FIRSTNAME'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 1
        Width = 185
      end
      object edSecondName: TcxDBTextEdit
        Left = 24
        Top = 101
        DataBinding.DataField = 'SECONDNAME'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 2
        Width = 185
      end
      object edNickName: TcxDBTextEdit
        Left = 24
        Top = 141
        DataBinding.DataField = 'NICKNAME'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 3
        Width = 185
      end
      object edHomePage: TcxDBHyperLinkEdit
        Left = 24
        Top = 181
        DataBinding.DataField = 'HOMEPAGE'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        ParentFont = False
        Style.StyleController = cxEditStyleController1
        TabOrder = 4
        Width = 473
      end
      object DBNavigator1: TcxDBNavigator
        Left = 24
        Top = 213
        Width = 326
        Height = 25
        DataSource = FilmsDemoDM.dsFilmsPersons
        InfoPanel.Font.Charset = DEFAULT_CHARSET
        InfoPanel.Font.Color = 536870912
        InfoPanel.Font.Height = -11
        InfoPanel.Font.Name = 'MS Sans Serif'
        InfoPanel.Font.Style = []
        InfoPanel.Visible = True
        TabOrder = 5
      end
      object edBirthName: TcxDBTextEdit
        Left = 264
        Top = 102
        DataBinding.DataField = 'BIRTHNAME'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 6
        Width = 233
      end
      object edLocationOfBirth: TcxDBTextEdit
        Left = 264
        Top = 62
        DataBinding.DataField = 'LOCATIONOFBIRTH'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 7
        Width = 233
      end
      object deDateOfBirth: TcxDBDateEdit
        Left = 264
        Top = 22
        DataBinding.DataField = 'DATEOFBIRTH'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 8
        Width = 121
      end
      object meBiography: TcxDBMemo
        Left = 520
        Top = 45
        DataBinding.DataField = 'BIOGRAPHY'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Properties.ScrollBars = ssVertical
        Style.StyleController = cxEditStyleController1
        TabOrder = 9
        Height = 156
        Width = 353
      end
      object chbMale: TcxDBCheckBox
        Left = 264
        Top = 136
        Caption = 'Male'
        DataBinding.DataField = 'Gender'
        DataBinding.DataSource = FilmsDemoDM.dsFilmsPersons
        Style.StyleController = cxEditStyleController1
        TabOrder = 10
        Transparent = True
        Width = 121
      end
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'O&ptions'
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
  object cxEditStyleController1: TcxEditStyleController
    Style.Color = 16247513
    Left = 521
    Top = 31
    PixelsPerInch = 96
  end
end
