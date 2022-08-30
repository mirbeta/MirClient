inherited MasterDetailDemoMainForm: TMasterDetailDemoMainForm
  Left = 36
  Top = 37
  Caption = 'ExpressQuantumGrid Master Detail Demo'
  ClientHeight = 626
  ClientWidth = 924
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 924
    Caption = 'Click '#39'About this demo'#39' for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 607
    Width = 924
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 924
    Height = 591
    Align = alClient
    TabOrder = 1
    object tvFilms: TcxGridDBTableView
      DataController.DataSource = FilmsDemoDM.dsFilms
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvFilmsID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvFilmsCAPTION: TcxGridDBColumn
        DataBinding.FieldName = 'CAPTION'
      end
      object tvFilmsYEAR: TcxGridDBColumn
        DataBinding.FieldName = 'YEAR'
      end
      object tvFilmsTAGLINE: TcxGridDBColumn
        DataBinding.FieldName = 'TAGLINE'
        Visible = False
      end
      object tvFilmsPLOTOUTLINE: TcxGridDBColumn
        DataBinding.FieldName = 'PLOTOUTLINE'
        Visible = False
      end
      object tvFilmsRUNTIME: TcxGridDBColumn
        DataBinding.FieldName = 'RUNTIME'
      end
      object tvFilmsCOLOR: TcxGridDBColumn
        DataBinding.FieldName = 'COLOR'
        Visible = False
      end
      object tvFilmsPHOTO: TcxGridDBColumn
        DataBinding.FieldName = 'PHOTO'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekPict
        Properties.PictureGraphicClassName = 'TdxSmartImage'
      end
      object tvFilmsICON: TcxGridDBColumn
        DataBinding.FieldName = 'ICON'
        Visible = False
      end
      object tvFilmsWEBSITE: TcxGridDBColumn
        DataBinding.FieldName = 'WEBSITE'
        Visible = False
      end
    end
    object cvPeople: TcxGridDBCardView
      DataController.DataSource = FilmsDemoDM.dsFilmsPersons
      DataController.DetailKeyFieldNames = 'FilmID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      Styles.StyleSheet = GridCardViewStyleSheetDevExpress
      object cvPeopleName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Name'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvPeoplePersonLineID: TcxGridDBCardViewRow
        Caption = 'Occupation'
        DataBinding.FieldName = 'PersonLineID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListSource = FilmsDemoDM.dsPersonLines
        Position.BeginsLayer = True
      end
      object cvPeopleFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvPeopleSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvPeopleNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
      object cvPeopleBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvPeopleDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvPeopleLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvPeopleBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Position.BeginsLayer = True
      end
      object cvPeopleHOMEPAGE: TcxGridDBCardViewRow
        DataBinding.FieldName = 'HOMEPAGE'
        Position.BeginsLayer = True
      end
      object cvPeopleID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPeopleFilmID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FilmID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPeopleBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPeopleGender: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Gender'
        Visible = False
        Position.BeginsLayer = True
      end
    end
    object tvCompanies: TcxGridDBTableView
      DataController.DataSource = FilmsDemoDM.dsFilmsCompanies
      DataController.DetailKeyFieldNames = 'FILMID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvCompaniesName: TcxGridDBColumn
        DataBinding.FieldName = 'CompanyName'
      end
      object tvCompaniesType: TcxGridDBColumn
        DataBinding.FieldName = 'Type'
      end
      object tvCompaniesCountry: TcxGridDBColumn
        DataBinding.FieldName = 'Country'
      end
      object tvCompaniesWebSite: TcxGridDBColumn
        DataBinding.FieldName = 'WebSite'
        Visible = False
      end
      object tvCompaniesID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvCompaniesFILMID: TcxGridDBColumn
        DataBinding.FieldName = 'FILMID'
        Visible = False
      end
    end
    object cvPhotos: TcxGridDBCardView
      DataController.DataSource = FilmsDemoDM.dsFilmsScreens
      DataController.DetailKeyFieldNames = 'FILMID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CellAutoHeight = True
      Styles.StyleSheet = GridCardViewStyleSheetDevExpress
      object cvPhotosID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPhotosFILMID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FILMID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPhotosSCREEN: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SCREEN'
        PropertiesClassName = 'TcxImageProperties'
        Properties.GraphicClassName = 'TJPEGImage'
        Properties.Stretch = True
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvPhotosICON: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ICON'
        Visible = False
        Position.BeginsLayer = True
      end
    end
    object lvFilms: TcxGridLevel
      GridView = tvFilms
      Options.DetailTabsPosition = dtpTop
      object lvPeople: TcxGridLevel
        Caption = 'People'
        GridView = cvPeople
      end
      object lvCompanies: TcxGridLevel
        Caption = 'Companies'
        GridView = tvCompanies
      end
      object lvPhotos: TcxGridLevel
        Caption = 'Photos'
        GridView = cvPhotos
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 456
    Top = 32
    object miOptions: TMenuItem [1]
      Caption = 'O&ptions'
      object miTabsPosition: TMenuItem
        Caption = 'Tabs Position'
        GroupIndex = 1
        object miLeftTabsPosition: TMenuItem
          AutoCheck = True
          Caption = 'Left'
          GroupIndex = 2
          Hint = 'Tabs are arranged on the Left of the master level'
          RadioItem = True
          OnClick = miTabsPositionClick
        end
        object miTopTabsPosition: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Top'
          Checked = True
          GroupIndex = 2
          Hint = 'Tabs are arranged on the Top of the master level'
          RadioItem = True
          OnClick = miTabsPositionClick
        end
      end
      object miDetailViewsSynchronization: TMenuItem
        AutoCheck = True
        Caption = '&Synchronize Detail Views'
        Checked = True
        GroupIndex = 1
        Hint = 
          'Synchronize any changes across detail views (e.g. sorting, filte' +
          'ring, grouping)'
        OnClick = miDetailViewsSynchronizationClick
      end
      object miSeparator1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miShowPreviewData: TMenuItem
        AutoCheck = True
        Caption = 'Show Preview Data'
        GroupIndex = 1
        Hint = 'Show preview data for each films'
        OnClick = miShowPreviewDataClick
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
