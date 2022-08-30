inherited DragDropDemoMainForm: TDragDropDemoMainForm
  Left = 119
  Top = 100
  Caption = 'ExpressQuantumGrid DragDropDemo'
  ClientHeight = 568
  ClientWidth = 763
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 763
    Height = 80
    Caption = 
      'This demo shows how to use the ExpressQuantumGrid control and th' +
      'e TdxEMFDataSet component shipped with the ExpressEntityMapping ' +
      'Framework to visualize master-detail relationships and copy reco' +
      'rds using drag and drop. The demo is equivalent to the DragDropD' +
      'emo shipped with the ExpressQuantumGrid Suite but replaces TClie' +
      'ntDataSet components with TdxEMFDataSet components to bind grid ' +
      'Views to data. Click '#39'About this demo'#39' for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 549
    Width = 763
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 80
    Width = 763
    Height = 469
    Align = alClient
    TabOrder = 1
    RootLevelOptions.DetailTabsPosition = dtpLeft
    RootLevelStyles.OnGetTabStyle = GridRootLevelStylesGetTabStyle
    OnActiveTabChanged = GridActiveTabChanged
    object cvPersons: TcxGridDBCardView
      OnDragDrop = cvPersonsDragDrop
      OnDragOver = cvPersonsDragOver
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = FilmsDemoDM.dsFilmsPersons
      DataController.DetailKeyFieldNames = 'FILMID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'FILMID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.DragFocusing = dfDragOver
      OptionsData.Inserting = False
      OptionsView.CardIndent = 7
      object cvPersonsName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Name'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvPersonsPersonLineID: TcxGridDBCardViewRow
        Caption = 'Occupation'
        DataBinding.FieldName = 'PersonLineID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.DropDownAutoSize = True
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Position.BeginsLayer = True
      end
      object cvPersonsFIRSTNAME: TcxGridDBCardViewRow
        Caption = 'First Name'
        DataBinding.FieldName = 'FIRSTNAME'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsSECONDNAME: TcxGridDBCardViewRow
        Caption = 'Second Name'
        DataBinding.FieldName = 'SECONDNAME'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsNICKNAME: TcxGridDBCardViewRow
        Caption = 'Nick Name'
        DataBinding.FieldName = 'NICKNAME'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsBIRTHNAME: TcxGridDBCardViewRow
        Caption = 'Birth Name'
        DataBinding.FieldName = 'BIRTHNAME'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsDATEOFBIRTH: TcxGridDBCardViewRow
        Caption = 'Date of Birth'
        DataBinding.FieldName = 'DATEOFBIRTH'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow
        Caption = 'Location of Birth'
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Options.Editing = False
        Position.BeginsLayer = True
      end
      object cvPersonsBIOGRAPHY: TcxGridDBCardViewRow
        Caption = 'Biography'
        DataBinding.FieldName = 'BIOGRAPHY'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Properties.ReadOnly = True
        Position.BeginsLayer = True
      end
      object cvPersonsID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsFilmID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FilmID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsBIRTHCOUNTRY: TcxGridDBCardViewRow
        Caption = 'Birth Country'
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsGender: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Gender'
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
        Options.Editing = False
        Position.BeginsLayer = True
      end
    end
    object tvCompanies: TcxGridDBTableView
      OnDragDrop = tvCompaniesDragDrop
      OnDragOver = tvCompaniesDragOver
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = FilmsDemoDM.dsFilmsCompanies
      DataController.DetailKeyFieldNames = 'FILMID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'FILMID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.DragFocusing = dfDragOver
      OptionsData.Inserting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object tvCompaniesID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object tvCompaniesFilmID: TcxGridDBColumn
        DataBinding.FieldName = 'FilmID'
        Visible = False
      end
      object tvCompaniesCompanyID: TcxGridDBColumn
        DataBinding.FieldName = 'CompanyID'
        Visible = False
      end
      object tvCompaniesCompanyName: TcxGridDBColumn
        Caption = 'Company Name'
        DataBinding.FieldName = 'CompanyName'
        Options.Editing = False
      end
    end
    object cvPhotos: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = FilmsDemoDM.dsFilmsScreens
      DataController.DetailKeyFieldNames = 'FILMID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'FILMID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CellAutoHeight = True
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
        Properties.FitMode = ifmProportionalStretch
        Properties.GraphicClassName = 'TdxSmartImage'
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvPhotosICON: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ICON'
        Visible = False
        Position.BeginsLayer = True
      end
    end
    object bvFilms: TcxGridDBBandedTableView
      OnDragDrop = bvFilmsDragDrop
      OnDragOver = bvFilmsDragOver
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = FilmsDemoDM.dsFilmsGenres
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.DragFocusing = dfDragOver
      OptionsData.Inserting = False
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      OptionsView.Header = False
      OptionsView.Indicator = True
      Preview.Column = bvFilmsCAPTION
      Preview.Place = ppTop
      Preview.Visible = True
      Bands = <
        item
          Caption = 'Film'
          FixedKind = fkLeft
          Width = 57
        end
        item
          Caption = 'Info'
          Width = 447
        end>
      object bvFilmsCAPTION: TcxGridDBBandedColumn
        DataBinding.FieldName = 'CAPTION'
        Width = 404
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object bvFilmsPHOTO: TcxGridDBBandedColumn
        DataBinding.FieldName = 'PHOTO'
        PropertiesClassName = 'TcxImageProperties'
        Properties.FitMode = ifmProportionalStretch
        Properties.GraphicClassName = 'TdxSmartImage'
        Width = 50
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.LineCount = 5
        Position.RowIndex = 1
      end
      object bvFilmsYEAR: TcxGridDBBandedColumn
        DataBinding.FieldName = 'YEAR'
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object bvFilmsRUNTIME: TcxGridDBBandedColumn
        DataBinding.FieldName = 'RUNTIME'
        Position.BandIndex = 1
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object bvFilmsPLOTOUTLINE: TcxGridDBBandedColumn
        DataBinding.FieldName = 'PLOTOUTLINE'
        PropertiesClassName = 'TcxMemoProperties'
        Properties.MaxLength = 200
        Properties.ScrollBars = ssVertical
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.LineCount = 3
        Position.RowIndex = 1
      end
      object bvFilmsWEBSITE: TcxGridDBBandedColumn
        DataBinding.FieldName = 'WEBSITE'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 2
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 504
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miShowDictionaries: TMenuItem
        Caption = 'Show Dictionaries'
        Hint = 'Shows the form on which the source grid is located'
        OnClick = miShowDictionariesClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miGenreTabPosition: TMenuItem
        Caption = 'Genre Tab &Position'
        object miTabPositionLeft: TMenuItem
          Tag = 1
          Caption = '&Left'
          Checked = True
          GroupIndex = 2
          Hint = 'Moves the tab panel to the left of the grid'
          RadioItem = True
          OnClick = miTabPositionClick
        end
        object miTabPositionTop: TMenuItem
          Tag = 2
          Caption = '&Top'
          GroupIndex = 2
          Hint = 'Moves the tab panel to the top of the grid'
          RadioItem = True
          OnClick = miTabPositionClick
        end
        object miTabPositionNone: TMenuItem
          Caption = '&None'
          GroupIndex = 2
          Hint = 'Hides the tab panel'
          RadioItem = True
          OnClick = miTabPositionClick
        end
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
