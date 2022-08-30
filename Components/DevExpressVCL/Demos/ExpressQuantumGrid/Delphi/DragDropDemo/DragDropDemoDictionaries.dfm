object DragDropDemoDictionariesForm: TDragDropDemoDictionariesForm
  Left = 337
  Top = 236
  Width = 625
  Height = 429
  Caption = 'Dictionaries'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbDesc: TLabel
    Left = 0
    Top = 0
    Width = 617
    Height = 32
    Align = alTop
    Caption = 
      'Drag a row from the grid below and drop it into the current cate' +
      'gory displayed by the main form. Click '#39'About this demo'#39' for mor' +
      'e information.'
    Color = 12937777
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object SourceGrid: TcxGrid
    Left = 0
    Top = 32
    Width = 617
    Height = 363
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpTop
    object tvCompaniesList: TcxGridDBTableView
      DragMode = dmAutomatic
      DataController.DataSource = FilmsDemoDM.dsCompanies
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvCompaniesListID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvCompaniesListCOMPANYTYPEID: TcxGridDBColumn
        Caption = 'Company Type'
        DataBinding.FieldName = 'ID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListOptions.AnsiSort = True
        Properties.ListOptions.CaseInsensitive = True
        Properties.ListSource = FilmsDemoDM.dsCompanyTypes
        Properties.MaxLength = 50
        Width = 144
      end
      object tvCompaniesListCOUNTRYID: TcxGridDBColumn
        Caption = 'Country'
        DataBinding.FieldName = 'COUNTRYID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListOptions.AnsiSort = True
        Properties.ListOptions.CaseInsensitive = True
        Properties.ListSource = FilmsDemoDM.dsCountries
        Properties.MaxLength = 60
        Width = 110
      end
      object tvCompaniesListCOMPANYNAME: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'COMPANYNAME'
        Width = 264
      end
      object tvCompaniesListCOMPANYWEBSITE: TcxGridDBColumn
        Caption = 'Company Web Site'
        DataBinding.FieldName = 'COMPANYWEBSITE'
        Visible = False
      end
    end
    object tvFilmsList: TcxGridDBTableView
      DragMode = dmAutomatic
      DataController.DataSource = FilmsDemoDM.dsFilms
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvFilmsListID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvFilmsListCAPTION: TcxGridDBColumn
        Caption = 'Caption'
        DataBinding.FieldName = 'CAPTION'
        Width = 165
      end
      object tvFilmsListYEAR: TcxGridDBColumn
        Caption = 'Year'
        DataBinding.FieldName = 'YEAR'
        Width = 39
      end
      object tvFilmsListTAGLINE: TcxGridDBColumn
        Caption = 'Tagline'
        DataBinding.FieldName = 'TAGLINE'
        Width = 194
      end
      object tvFilmsListPLOTOUTLINE: TcxGridDBColumn
        Caption = 'Plot Outline'
        DataBinding.FieldName = 'PLOTOUTLINE'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Width = 41
      end
      object tvFilmsListRUNTIME: TcxGridDBColumn
        Caption = 'Runtime'
        DataBinding.FieldName = 'RUNTIME'
        Width = 31
      end
      object tvFilmsListCOLOR: TcxGridDBColumn
        Caption = 'Color'
        DataBinding.FieldName = 'COLOR'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Width = 48
      end
      object tvFilmsListPHOTO: TcxGridDBColumn
        Caption = 'Photo'
        DataBinding.FieldName = 'PHOTO'
        Visible = False
      end
      object tvFilmsListICON: TcxGridDBColumn
        Caption = 'Icon'
        DataBinding.FieldName = 'ICON'
        Visible = False
      end
      object tvFilmsListWEBSITE: TcxGridDBColumn
        Caption = 'Website'
        DataBinding.FieldName = 'WEBSITE'
        Visible = False
        Width = 87
      end
    end
    object cvPersonsList: TcxGridDBCardView
      DragMode = dmAutomatic
      DataController.DataSource = FilmsDemoDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      object cvPersonsListID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsListFIRSTNAME: TcxGridDBCardViewRow
        Caption = 'First Name'
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsListSECONDNAME: TcxGridDBCardViewRow
        Caption = 'Second Name'
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsListGENDER: TcxGridDBCardViewRow
        Caption = 'Gender'
        DataBinding.FieldName = 'GENDER'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Items = <
          item
            Description = 'Female'
            Value = False
          end
          item
            Description = 'Male'
            Value = True
          end>
        Position.BeginsLayer = True
      end
      object cvPersonsListBIRTHNAME: TcxGridDBCardViewRow
        Caption = 'Birthname'
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsListDATEOFBIRTH: TcxGridDBCardViewRow
        Caption = 'Date of Birth'
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvPersonsListBIRTHCOUNTRY: TcxGridDBCardViewRow
        Caption = 'Birth Country'
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListSource = FilmsDemoDM.dsCountries
        Position.BeginsLayer = True
      end
      object cvPersonsListLOCATIONOFBIRTH: TcxGridDBCardViewRow
        Caption = 'Location of Birth'
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvPersonsListBIOGRAPHY: TcxGridDBCardViewRow
        Caption = 'Biography'
        DataBinding.FieldName = 'BIOGRAPHY'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Position.BeginsLayer = True
      end
      object cvPersonsListNICKNAME: TcxGridDBCardViewRow
        Caption = 'Nick Name'
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsListHOMEPAGE: TcxGridDBCardViewRow
        DataBinding.FieldName = 'HOMEPAGE'
        Visible = False
        Position.BeginsLayer = True
      end
    end
    object glFilmsList: TcxGridLevel
      Caption = 'Films'
      GridView = tvFilmsList
    end
    object glPersonsList: TcxGridLevel
      Caption = 'Persons'
      GridView = cvPersonsList
    end
    object glCompaniesList: TcxGridLevel
      Caption = 'Companies'
      GridView = tvCompaniesList
    end
  end
end
