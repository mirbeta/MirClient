object ColumnSimpleDemoCarsForm: TColumnSimpleDemoCarsForm
  Left = 327
  Top = 204
  Caption = 'ColumnSimpleDemoCarsForm'
  ClientHeight = 339
  ClientWidth = 805
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCarInfo: TPanel
    Left = 8
    Top = 31
    Width = 673
    Height = 268
    Caption = 'pnlCarInfo'
    TabOrder = 0
    object GridCars: TcxGrid
      Left = 1
      Top = 1
      Width = 671
      Height = 266
      Align = alClient
      TabOrder = 0
      object bvCars: TcxGridDBBandedTableView
        Navigator.Buttons.CustomButtons = <>
        OnTopRecordIndexChanged = bvCarsTopRecordIndexChanged
        DataController.DataSource = dmGridCars.dsModels
        DataController.KeyFieldNames = 'ID'
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.BandMoving = False
        OptionsCustomize.BandSizing = False
        OptionsView.CellAutoHeight = True
        OptionsView.GridLines = glHorizontal
        OptionsView.GroupByBox = False
        OptionsView.Header = False
        Bands = <
          item
            Caption = 'Info'
          end
          item
            Caption = 'Description'
          end>
        object bvCarsID: TcxGridDBBandedColumn
          DataBinding.FieldName = 'ID'
          Position.BandIndex = 0
          Position.ColIndex = 3
          Position.RowIndex = 0
        end
        object bvCarsTrademark: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Trademark'
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 0
        end
        object bvCarsModel: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Name'
          Width = 100
          Position.BandIndex = 0
          Position.ColIndex = 1
          Position.RowIndex = 0
        end
        object bvCarsPrice: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Price'
          Width = 60
          Position.BandIndex = 0
          Position.ColIndex = 2
          Position.RowIndex = 0
        end
        object bvCarsPicture: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Photo'
          PropertiesClassName = 'TcxImageProperties'
          Properties.FitMode = ifmProportionalStretch
          Properties.GraphicClassName = 'TdxSmartImage'
          Width = 150
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.LineCount = 7
          Position.RowIndex = 2
        end
        object bvCarsLargePicture: TcxGridDBBandedColumn
          Caption = 'LargePicture'
          DataBinding.FieldName = 'Photo'
          PropertiesClassName = 'TcxBlobEditProperties'
          Properties.BlobEditKind = bekPict
          Properties.PictureGraphicClassName = 'TdxSmartImage'
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 3
        end
        object bvCarsCategory: TcxGridDBBandedColumn
          DataBinding.FieldName = 'CategoryID'
          RepositoryItem = dmGridCars.EditRepositoryCategoryLookup
          Width = 50
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 1
        end
        object bvCarsHP: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Horsepower'
          Width = 50
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 4
        end
        object bvCarsCyl: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Cilinders'
          Width = 20
          Position.BandIndex = 0
          Position.ColIndex = 1
          Position.RowIndex = 4
        end
        object bvCarsTransmissSpeedCount: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Transmission Speeds'
          Width = 40
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 5
        end
        object bvCarsTransmissAutomatic: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Transmission Type'
          RepositoryItem = dmGridCars.EditRepositoryTransmissionTypeLookup
          Width = 150
          Position.BandIndex = 0
          Position.ColIndex = 1
          Position.RowIndex = 5
        end
        object bvCarsMPG_City: TcxGridDBBandedColumn
          DataBinding.FieldName = 'MPG City'
          Position.BandIndex = 0
          Position.ColIndex = 2
          Position.RowIndex = 5
        end
        object bvCarsMPG_Highway: TcxGridDBBandedColumn
          DataBinding.FieldName = 'MPG Highway'
          Position.BandIndex = 0
          Position.ColIndex = 3
          Position.RowIndex = 5
        end
        object bvCarsDescription: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Description'
          PropertiesClassName = 'TcxMemoProperties'
          Properties.ScrollBars = ssVertical
          Width = 300
          Position.BandIndex = 1
          Position.ColIndex = 0
          Position.LineCount = 12
          Position.RowIndex = 0
        end
        object bvCarsHyperlink: TcxGridDBBandedColumn
          DataBinding.FieldName = 'Hyperlink'
          PropertiesClassName = 'TcxHyperLinkEditProperties'
          Properties.SingleClick = True
          Width = 150
          Position.BandIndex = 0
          Position.ColIndex = 0
          Position.RowIndex = 6
        end
      end
      object lvCars: TcxGridLevel
        GridView = bvCars
      end
    end
  end
end
