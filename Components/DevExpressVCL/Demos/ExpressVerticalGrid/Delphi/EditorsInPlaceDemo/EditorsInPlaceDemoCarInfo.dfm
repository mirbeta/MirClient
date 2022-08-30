object EditorsInPlaceDemoCarInfoForm: TEditorsInPlaceDemoCarInfoForm
  Left = 197
  Top = 24
  Caption = 'CarInfo'
  ClientHeight = 388
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCarInfo: TPanel
    Left = 8
    Top = 8
    Width = 649
    Height = 329
    BevelOuter = bvNone
    TabOrder = 0
    object vgCarInfo: TcxDBVerticalGrid
      Left = 0
      Top = 0
      Width = 649
      Height = 294
      Align = alClient
      LayoutStyle = lsBandsView
      OptionsView.RowHeaderWidth = 242
      OptionsView.ShowHeaders = False
      OptionsView.GridLines = vglNone
      OptionsBehavior.CellHints = False
      Navigator.Buttons.CustomButtons = <>
      Styles.OnGetContentStyle = vgCarInfoStylesGetContentStyle
      Styles.StyleSheet = EditorsInPlaceDemoDataDM.cxVerticalGridStyleSheetDevExpress
      TabOrder = 0
      OnLeftVisibleRecordIndexChanged = vgCarInfoLeftVisibleRecordIndexChanged
      DataController.DataSource = dmCars.dsModels
      Version = 1
      object vgCarInfoCar: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'Car'
            EditPropertiesClassName = 'TcxTextEditProperties'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxMRUEditProperties'
            EditProperties.LookupItems.Strings = (
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
            EditProperties.OnButtonClick = OnEditPropertiesButtonClick
            Width = 100
            DataBinding.FieldName = 'Trademark'
          end
          item
            Caption = 'Model'
            DataBinding.FieldName = 'Name'
          end>
        Properties.Fixed = True
        ID = 0
        ParentID = -1
        Index = 0
        Version = 1
      end
      object vgCarInfoCategory: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'Category'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxRadioGroupProperties'
            EditProperties.Columns = 3
            EditProperties.Items = <
              item
                Caption = 'Sports'
                Value = 'SPORTS'
              end
              item
                Caption = 'Saloon'
                Value = 'SALOON'
              end
              item
                Caption = 'Truck'
                Value = 'TRUCK'
              end>
            RepositoryItem = dmCars.EditRepositoryCategoryLookup
            Width = 90
            DataBinding.FieldName = 'CategoryID'
          end>
        Properties.Fixed = True
        ID = 1
        ParentID = -1
        Index = 1
        Version = 1
      end
      object vgCarInfoImage: TcxDBEditorRow
        Height = 64
        Properties.EditPropertiesClassName = 'TcxImageProperties'
        Properties.EditProperties.FitMode = ifmProportionalStretch
        Properties.EditProperties.GraphicClassName = 'TJPEGImage'
        Properties.DataBinding.FieldName = 'Image'
        ID = 2
        ParentID = -1
        Index = 2
        Version = 1
      end
      object vgCarInfoLargeImageAndPrice: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'LargePicture:'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxBlobEditProperties'
            EditProperties.BlobEditKind = bekPict
            EditProperties.PictureGraphicClassName = 'TJPEGImage'
            Width = 110
            DataBinding.FieldName = 'Photo'
          end
          item
            Caption = 'Price:'
            Width = 40
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxCurrencyEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            DataBinding.FieldName = 'Price'
          end>
        Properties.Fixed = True
        ID = 3
        ParentID = -1
        Index = 3
        Version = 1
      end
      object vgCarInfoEngine: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'Engine:'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 80
            DataBinding.FieldName = 'Horsepower'
          end
          item
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 80
            DataBinding.FieldName = 'Torque'
          end
          item
            Caption = 'Cylinders'
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 60
            DataBinding.FieldName = 'Cilinders'
          end>
        Properties.Fixed = True
        ID = 4
        ParentID = -1
        Index = 4
        Version = 1
      end
      object vgCarInfoTransmission: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'Transmission:'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 80
            DataBinding.FieldName = 'Transmission Speeds'
          end
          item
            RepositoryItem = dmCars.EditRepositoryTransmissionTypeCheckBox
            Width = 30
            DataBinding.FieldName = 'Transmission Type'
          end
          item
            Caption = 'Automatic'
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end>
        Properties.Fixed = True
        ID = 5
        ParentID = -1
        Index = 5
        Version = 1
      end
      object vgCarInfoMPG: TcxDBMultiEditorRow
        Properties.Editors = <
          item
            Caption = 'MPG:'
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 100
            DataBinding.FieldName = 'MPG City'
          end
          item
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            DataBinding.FieldName = 'MPG Highway'
          end>
        Properties.Fixed = True
        ID = 6
        ParentID = -1
        Index = 6
        Version = 1
      end
      object vgCarInfoWebSite: TcxDBMultiEditorRow
        Height = 17
        Properties.Editors = <
          item
            Caption = 'WebSite:'
            EditPropertiesClassName = 'TcxTextEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            Width = 80
            Options.Editing = False
            OnGetDisplayText = GetDisplayText
          end
          item
            EditPropertiesClassName = 'TcxHyperLinkEditProperties'
            EditProperties.Alignment.Horz = taLeftJustify
            DataBinding.FieldName = 'Hyperlink'
          end>
        Properties.Fixed = True
        ID = 7
        ParentID = -1
        Index = 7
        Version = 1
      end
      object vgCarInfoDescription: TcxDBEditorRow
        Height = 186
        Properties.EditPropertiesClassName = 'TcxMemoProperties'
        Properties.EditProperties.ScrollBars = ssVertical
        Properties.EditProperties.WordWrap = False
        Properties.DataBinding.FieldName = 'Description'
        ID = 8
        ParentID = -1
        Index = 8
        Version = 1
      end
      object vgCarInfoID: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'ID'
        Visible = False
        ID = 9
        ParentID = -1
        Index = 9
        Version = 1
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 294
      Width = 649
      Height = 35
      Align = alBottom
      BevelOuter = bvNone
      Color = 15784893
      TabOrder = 1
      object btnOk: TcxButton
        Tag = 1
        Left = 448
        Top = 5
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&OK'
        Colors.Default = 15784893
        Colors.Normal = 15784893
        Colors.Hot = 15188621
        Colors.Pressed = 14659432
        TabOrder = 0
        OnClick = cxButtonClick
      end
      object btnCancel: TcxButton
        Left = 536
        Top = 5
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Cancel'
        Colors.Default = 15784893
        Colors.Normal = 15784893
        Colors.Hot = 15188621
        Colors.Pressed = 14659432
        TabOrder = 1
        OnClick = cxButtonClick
      end
    end
  end
end
