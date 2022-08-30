inherited ViewTableSimpleDemoMainForm: TViewTableSimpleDemoMainForm
  Left = 349
  Top = 254
  Caption = 'ExpressQuantumGrid ViewTableSimple Demo'
  ClientHeight = 586
  ClientWidth = 896
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 896
    Caption = 
      'Use the new Table View. Experiment by changing the Options above' +
      '. Click '#39'About this demo'#39' for more information.'
  end
  object Splitter: TSplitter [1]
    Left = 217
    Top = 16
    Width = 2
    Height = 551
  end
  object pnGenres: TPanel [2]
    Left = 0
    Top = 16
    Width = 217
    Height = 551
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object pnGenresCaption: TPanel
      Left = 0
      Top = 0
      Width = 217
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Categories'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object cxgGenras: TcxGrid
      Left = 0
      Top = 33
      Width = 217
      Height = 518
      Align = alClient
      TabOrder = 1
      object cxgGenrasDBTableView: TcxGridDBTableView
        DataController.DataSource = ViewTableSimpleDemoMainDM.dsGENRES
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsData.Deleting = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.InvertSelect = False
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        object cxgGenrasDBTableViewNAME: TcxGridDBColumn
          Caption = 'Name'
          DataBinding.FieldName = 'NAME'
          Options.Editing = False
          Options.Filtering = False
        end
      end
      object cxgGenrasLevel: TcxGridLevel
        GridView = cxgGenrasDBTableView
      end
    end
  end
  object pnFilms: TPanel [3]
    Left = 219
    Top = 16
    Width = 677
    Height = 551
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnFilmsCaption: TPanel
      Left = 0
      Top = 0
      Width = 677
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Films'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object cxgFilms: TcxGrid
      Left = 0
      Top = 33
      Width = 677
      Height = 518
      Align = alClient
      TabOrder = 1
      object cxgFilmsDBTableView: TcxGridDBTableView
        Navigator.Visible = True
        DataController.DataSource = ViewTableSimpleDemoMainDM.dsFilms
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        Preview.Column = cxgFilmsDBTableViewTAGLINE
        Preview.Place = ppTop
        Preview.Visible = True
        object cxgFilmsDBTableViewICON: TcxGridDBColumn
          Caption = 'Icon'
          DataBinding.FieldName = 'ICON'
          PropertiesClassName = 'TcxImageProperties'
          Properties.GraphicClassName = 'TdxSmartImage'
          Properties.Stretch = True
          Width = 98
        end
        object cxgFilmsDBTableViewCAPTION: TcxGridDBColumn
          Caption = 'Caption'
          DataBinding.FieldName = 'CAPTION'
          PropertiesClassName = 'TcxTextEditProperties'
          Width = 74
        end
        object cxgFilmsDBTableViewYEAR: TcxGridDBColumn
          Caption = 'Year'
          DataBinding.FieldName = 'YEAR'
          Visible = False
          GroupIndex = 0
          SortIndex = 0
          SortOrder = soAscending
        end
        object cxgFilmsDBTableViewTAGLINE: TcxGridDBColumn
          Caption = 'Tagline'
          DataBinding.FieldName = 'TAGLINE'
          PropertiesClassName = 'TcxMemoProperties'
        end
        object cxgFilmsDBTableViewPLOTOUTLINE: TcxGridDBColumn
          Caption = 'Plotoutline'
          DataBinding.FieldName = 'PLOTOUTLINE'
          PropertiesClassName = 'TcxMemoProperties'
          Width = 288
        end
        object cxgFilmsDBTableViewRUNTIME: TcxGridDBColumn
          Caption = 'Runtime'
          DataBinding.FieldName = 'RUNTIME'
          PropertiesClassName = 'TcxTextEditProperties'
          Width = 55
        end
        object cxgFilmsDBTableViewCOLOR: TcxGridDBColumn
          Caption = 'Color'
          DataBinding.FieldName = 'COLOR'
          PropertiesClassName = 'TcxCheckBoxProperties'
          Width = 61
        end
        object cxgFilmsDBTableViewPHOTO: TcxGridDBColumn
          Caption = 'Photo'
          DataBinding.FieldName = 'PHOTO'
          PropertiesClassName = 'TcxBlobEditProperties'
          Properties.BlobEditKind = bekPict
          Properties.PictureGraphicClassName = 'TdxSmartImage'
          Width = 36
        end
        object cxgFilmsDBTableViewWEBSITE: TcxGridDBColumn
          Caption = 'Website'
          DataBinding.FieldName = 'WEBSITE'
          PropertiesClassName = 'TcxHyperLinkEditProperties'
          Width = 46
        end
      end
      object cxgFilmsLevel: TcxGridLevel
        GridView = cxgFilmsDBTableView
      end
    end
  end
  inherited sbMain: TStatusBar
    Top = 567
    Width = 896
  end
  inherited mmMain: TMainMenu
    Left = 600
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miMultiSelect: TMenuItem
        AutoCheck = True
        Caption = '&Multi Select'
        Hint = 'Allows you to select several rows'
        OnClick = miMultiSelectClick
      end
      object miShowIndicator: TMenuItem
        AutoCheck = True
        Caption = 'Show &Indicator'
        Hint = 'Determines whether the row indicator is visible'
        OnClick = miShowIndicatorClick
      end
      object miSeparator5: TMenuItem
        Caption = '-'
      end
      object miIncSearch: TMenuItem
        AutoCheck = True
        Caption = 'Incremental &Search'
        OnClick = miIncSearchClick
      end
      object miImmediateEditor: TMenuItem
        AutoCheck = True
        Caption = 'Immediate &Editor'
        Checked = True
        Hint = 
          'Determines whether the editor of a specific column is activated ' +
          'immediately as a user enters an appropriate cell'
        OnClick = miImmediateEditorClick
      end
      object miFocusCellOnTab: TMenuItem
        AutoCheck = True
        Caption = '&Focus Cell On Tab'
        Hint = 'Tab key changes cell focus'
        OnClick = miFocusCellOnTabClick
      end
      object miShowPreviewRow: TMenuItem
        AutoCheck = True
        Caption = 'Show &Preview Row'
        Checked = True
        Hint = 'Shows/Hides preview column'
        OnClick = miShowPreviewRowClick
      end
      object miShowNavigator: TMenuItem
        AutoCheck = True
        Caption = 'Show &Navigator'
        Checked = True
        Hint = 'Displays the inplace navigator'
        OnClick = miShowNavigatorClick
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
