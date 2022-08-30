inherited CustomRowHeightDemoMainForm: TCustomRowHeightDemoMainForm
  Left = 115
  Top = 47
  Width = 767
  Height = 654
  Caption = 'ExpressQuantumGrid CustomRowHeight Demo'
  Color = clBtnFace
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Left = 0
    Top = 0
    Height = 33
    Align = alTop
    AutoSize = False
    Caption = 
      'Experiment by zooming in or out on the photo (see the Options). ' +
      'Click '#39'About this demo'#39' for more information.'
  end
  object Grid: TcxGrid
    Left = 0
    Top = 33
    Width = 759
    Height = 548
    Align = alClient
    TabOrder = 1
    object tvFilms: TcxGridDBTableView
      DataController.DataSource = FilmsDemoDM.dsFilms
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      NavigatorButtons.ConfirmDelete = False
      OnGetCellHeight = tvFilmsGetCellHeight
      OptionsCustomize.DataRowSizing = True
      OptionsCustomize.GroupRowSizing = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object tvFilmsCAPTION: TcxGridDBColumn
        Caption = 'Caption'
        DataBinding.FieldName = 'CAPTION'
        Width = 157
      end
      object tvFilmsPLOTOUTLINE: TcxGridDBColumn
        Caption = 'Description'
        DataBinding.FieldName = 'PLOTOUTLINE'
        PropertiesClassName = 'TcxMemoProperties'
        Width = 222
      end
      object tvFilmsPHOTO: TcxGridDBColumn
        Caption = 'Photo'
        DataBinding.FieldName = 'PHOTO'
        PropertiesClassName = 'TcxImageProperties'
        Properties.GraphicClassName = 'TdxSmartImage'
        Properties.Stretch = True
        Width = 366
      end
    end
    object lvFilms: TcxGridLevel
      GridView = tvFilms
    end
  end
  inherited mmMain: TMainMenu
    Left = 504
    Top = 24
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miPictureZoom: TMenuItem
        Caption = 'Picture &Zoom'
        object miZoom100perc: TMenuItem
          Caption = '100%'
          RadioItem = True
          OnClick = miZoomClick
        end
        object miZoom75perc: TMenuItem
          Tag = 1
          Caption = '75%'
          RadioItem = True
          OnClick = miZoomClick
        end
        object miZoom50perc: TMenuItem
          Tag = 2
          Caption = '50%'
          Checked = True
          RadioItem = True
          OnClick = miZoomClick
        end
        object miZoom25perc: TMenuItem
          Tag = 3
          Caption = '25%'
          RadioItem = True
          OnClick = miZoomClick
        end
      end
    end
  end	
end
