object EditorsInPlaceDemoCarInfoForm: TEditorsInPlaceDemoCarInfoForm
  Left = 292
  Top = 98
  Caption = 'EditorsInPlaceDemoCarInfoForm'
  ClientHeight = 369
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCarInfo: TPanel
    Left = 21
    Top = 6
    Width = 545
    Height = 288
    BevelOuter = bvNone
    Caption = 'pnlCarInfo'
    TabOrder = 0
    object tlCarInfo: TcxDBTreeList
      Left = 0
      Top = 0
      Width = 545
      Height = 253
      Align = alClient
      Bands = <
        item
          Caption.Text = 'Band + 1'
          Width = 316
        end
        item
          Caption.Text = 'Band + 2'
          Width = 210
        end>
      DataController.DataSource = dmCars.dsModels
      DataController.ParentField = 'ID'
      DataController.KeyField = 'ID'
      Navigator.Buttons.CustomButtons = <>
      OptionsData.Inserting = True
      OptionsSelection.HideSelection = True
      OptionsSelection.InvertSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GridLineColor = clGray
      OptionsView.GridLines = tlglHorz
      OptionsView.Headers = False
      OptionsView.ShowRoot = False
      RootValue = 255
      Styles.Background = EditorsInPlaceDemoDataDM.cxStyle6
      Styles.Content = EditorsInPlaceDemoDataDM.cxStyle6
      Styles.Selection = EditorsInPlaceDemoDataDM.cxStyle6
      TabOrder = 0
      OnTopRecordIndexChanged = tlCarInfoTopRecordIndexChanged
      object tlCarInfoTrademark: TcxDBTreeListColumn
        PropertiesClassName = 'TcxMRUEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Properties.LookupItems.Strings = (
          'Mercedes-Benz'
          'BMW'
          'Rolls-Royce'
          'Jaguar'
          'Cadillac '
          'Lexus'
          'Ford'
          'Dodge'
          'GMC'
          'Nissan'
          'Toyota')
        Properties.OnButtonClick = tlCarInfoTrademarkPropertiesButtonClick
        DataBinding.FieldName = 'Trademark'
        Width = 105
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoModel: TcxDBTreeListColumn
        Caption.Text = 'Model'
        DataBinding.FieldName = 'Name'
        Width = 121
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoPrice: TcxDBTreeListColumn
        DataBinding.FieldName = 'Price'
        Width = 75
        Position.ColIndex = 3
        Position.RowIndex = 3
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoCategory: TcxDBTreeListColumn
        RepositoryItem = dmCars.EditRepositoryCategoryLookup
        Caption.Text = 'Category'
        DataBinding.FieldName = 'CategoryID'
        Width = 226
        Position.ColIndex = 1
        Position.RowIndex = 1
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoPicture: TcxDBTreeListColumn
        PropertiesClassName = 'TcxImageProperties'
        Properties.FitMode = ifmProportionalStretch
        Properties.GraphicClassName = 'TJPEGImage'
        DataBinding.FieldName = 'Photo'
        Width = 316
        Position.ColIndex = 0
        Position.LineCount = 7
        Position.RowIndex = 2
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoBlobImage: TcxDBTreeListColumn
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekPict
        Properties.PictureGraphicClassName = 'TJPEGImage'
        DataBinding.FieldName = 'Photo'
        Width = 61
        Position.ColIndex = 1
        Position.RowIndex = 3
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoHP: TcxDBTreeListColumn
        DataBinding.FieldName = 'Horsepower'
        Width = 76
        Position.ColIndex = 1
        Position.RowIndex = 4
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoTorque: TcxDBTreeListColumn
        DataBinding.FieldName = 'Torque'
        Width = 74
        Position.ColIndex = 2
        Position.RowIndex = 4
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoCyl: TcxDBTreeListColumn
        Caption.Text = 'Cyl'
        DataBinding.FieldName = 'Cilinders'
        Width = 76
        Position.ColIndex = 3
        Position.RowIndex = 4
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoTransmissSpeedCount: TcxDBTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.Alignment.Horz = taLeftJustify
        DataBinding.FieldName = 'Transmission Speeds'
        Width = 78
        Position.ColIndex = 1
        Position.RowIndex = 5
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoTransmissAutomatic: TcxDBTreeListColumn
        RepositoryItem = dmCars.EditRepositoryTransmissionTypeCheckBox
        DataBinding.FieldName = 'Transmission Type'
        Width = 20
        Position.ColIndex = 2
        Position.RowIndex = 5
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoMPG_City: TcxDBTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.Alignment.Horz = taLeftJustify
        DataBinding.FieldName = 'MPG City'
        Width = 102
        Position.ColIndex = 1
        Position.RowIndex = 6
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoMPG_Highway: TcxDBTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.Alignment.Horz = taLeftJustify
        DataBinding.FieldName = 'MPG Highway'
        Width = 124
        Position.ColIndex = 2
        Position.RowIndex = 6
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoDescription: TcxDBTreeListColumn
        PropertiesClassName = 'TcxMemoProperties'
        Properties.ScrollBars = ssVertical
        DataBinding.FieldName = 'Description'
        Width = 210
        Position.ColIndex = 0
        Position.LineCount = 14
        Position.RowIndex = 0
        Position.BandIndex = 1
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoHyperlink: TcxDBTreeListColumn
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        DataBinding.FieldName = 'Hyperlink'
        Width = 316
        Position.ColIndex = 0
        Position.RowIndex = 7
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlCarInfoCategoryCaption: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'Category: '
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 1
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoCarCaption: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'Car: '
        DataBinding.FieldName = 'FullName'
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoLargePictureCaption: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'LargePicture: '
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 3
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoEngineCaption: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'Engine: '
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 4
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoTransmissCaption: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'Transmission: '
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 5
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoMPG: TcxDBTreeListColumn
        Styles.Content = EditorsInPlaceDemoDataDM.styCaption
        Caption.Text = 'MPG: '
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 0
        Position.RowIndex = 6
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoPriceCaption: TcxDBTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.Alignment.Horz = taRightJustify
        Caption.Text = 'Price: '
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Width = 90
        Position.ColIndex = 2
        Position.RowIndex = 3
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
      object tlCarInfoTransmissAutomatCaption: TcxDBTreeListColumn
        Caption.Text = 'Automatic'
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.IncSearch = False
        Options.Moving = False
        Width = 128
        Position.ColIndex = 3
        Position.RowIndex = 5
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlCarInfoGetCaptionDisplayText
      end
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 253
      Width = 545
      Height = 35
      Align = alBottom
      BevelOuter = bvNone
      Color = 15784893
      TabOrder = 1
      object btnOK: TcxButton
        Left = 368
        Top = 5
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'OK'
        Colors.Default = 15784893
        Colors.Normal = 15784893
        Colors.Hot = 15188621
        Colors.Pressed = 14659432
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TcxButton
        Left = 456
        Top = 5
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Cancel'
        Colors.Default = 15784893
        Colors.Normal = 15784893
        Colors.Hot = 15188621
        Colors.Pressed = 14659432
        TabOrder = 1
        OnClick = btnCancelClick
      end
    end
  end
end
