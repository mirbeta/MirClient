inherited ServerModeDemoMainForm: TServerModeDemoMainForm
  Left = 112
  Top = 111
  Caption = 'ExpressQuantumGrid ServerModeQuery Demo'
  ClientHeight = 552
  ClientWidth = 791
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 791
    Transparent = False
  end
  inherited sbMain: TStatusBar
    Top = 533
    Width = 791
  end
  object cxGrid1: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 791
    Height = 517
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = cxGrid1ActiveTabChanged
    object cxGrid1ServerModeTableView1: TcxGridServerModeTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.Insert.Visible = False
      Navigator.Buttons.Delete.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Post.Visible = False
      Navigator.Buttons.Cancel.Visible = False
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.InfoPanel.Visible = True
      Navigator.InfoPanel.Width = 100
      Navigator.Visible = True
      DataController.DataSource = ServerModeDemoDataDM.ServerModeQueryDataSource
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          FieldName = 'OID'
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Format = 'Count: #,###'
          Kind = skCount
          FieldName = 'OrderDate'
          Column = cxGrid1ServerModeTableView1OrderDate
        end>
      DataController.Summary.SummaryGroups = <>
      DateTimeHandling.Grouping = dtgByMonth
      OptionsBehavior.IncSearch = True
      OptionsCustomize.ColumnsQuickCustomization = True
      OptionsData.Deleting = False
      OptionsData.DeletingConfirmation = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.FooterAutoHeight = True
      OptionsView.FooterMultiSummaries = True
      OptionsView.GroupFooterMultiSummaries = True
      OptionsView.Indicator = True
      object cxGrid1ServerModeTableView1OrderDate: TcxGridServerModeColumn
        Caption = 'Order Date'
        DataBinding.FieldName = 'OrderDate'
        PropertiesClassName = 'TcxDateEditProperties'
        Properties.DisplayFormat = 'yyyy/mm/dd'
        Options.Filtering = False
        Options.FilteringFilteredItemsList = False
        Options.FilteringMRUItemsList = False
        Options.FilteringPopup = False
        Options.FilteringPopupMultiSelect = False
        SortIndex = 0
        SortOrder = soDescending
        Width = 113
      end
      object cxGrid1ServerModeTableView1Model: TcxGridServerModeColumn
        DataBinding.FieldName = 'Model'
        Width = 78
      end
      object cxGrid1ServerModeTableView1Trademark: TcxGridServerModeColumn
        DataBinding.FieldName = 'Trademark'
        Width = 70
      end
      object cxGrid1ServerModeTableView1Category: TcxGridServerModeColumn
        DataBinding.FieldName = 'Category'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Horz = taCenter
        Properties.Images = ImageList
        Properties.Items = <
          item
            ImageIndex = 0
            Value = 'SALOON'
          end
          item
            ImageIndex = 1
            Value = 'SPORTS'
          end
          item
            ImageIndex = 2
            Value = 'TRUCK'
          end>
        Properties.ShowDescriptions = False
        Width = 52
      end
      object cxGrid1ServerModeTableView1Price: TcxGridServerModeColumn
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxSpinEditProperties'
        Properties.DisplayFormat = '$#,###'
        Width = 103
      end
      object cxGrid1ServerModeTableView1TransmissAutomatic: TcxGridServerModeColumn
        Caption = 'Automatic Transmission'
        DataBinding.FieldName = 'TransmissAutomatic'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Width = 28
      end
      object cxGrid1ServerModeTableView1HP: TcxGridServerModeColumn
        DataBinding.FieldName = 'HP'
        Visible = False
      end
      object cxGrid1ServerModeTableView1TransmissSpeedCount: TcxGridServerModeColumn
        DataBinding.FieldName = 'TransmissSpeedCount'
        Visible = False
      end
      object cxGrid1ServerModeTableView1FirstName: TcxGridServerModeColumn
        Caption = 'First Name'
        DataBinding.FieldName = 'FirstName'
        Width = 54
      end
      object cxGrid1ServerModeTableView1LastName: TcxGridServerModeColumn
        Caption = 'Last Name'
        DataBinding.FieldName = 'LastName'
        Width = 53
      end
      object cxGrid1ServerModeTableView1Company: TcxGridServerModeColumn
        DataBinding.FieldName = 'Company'
        Visible = False
        GroupIndex = 0
      end
      object cxGrid1ServerModeTableView1Prefix: TcxGridServerModeColumn
        DataBinding.FieldName = 'Prefix'
        Visible = False
      end
      object cxGrid1ServerModeTableView1Title: TcxGridServerModeColumn
        DataBinding.FieldName = 'Title'
        Visible = False
        Width = 25
      end
      object cxGrid1ServerModeTableView1Address: TcxGridServerModeColumn
        DataBinding.FieldName = 'Address'
        Visible = False
      end
      object cxGrid1ServerModeTableView1City: TcxGridServerModeColumn
        DataBinding.FieldName = 'City'
        Width = 76
      end
      object cxGrid1ServerModeTableView1State: TcxGridServerModeColumn
        DataBinding.FieldName = 'State'
        Visible = False
      end
      object cxGrid1ServerModeTableView1Source: TcxGridServerModeColumn
        DataBinding.FieldName = 'Source'
        Width = 50
      end
      object cxGrid1ServerModeTableView1Customer: TcxGridServerModeColumn
        DataBinding.FieldName = 'Customer'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Visible = False
        Width = 30
      end
      object cxGrid1ServerModeTableView1HomePhone: TcxGridServerModeColumn
        DataBinding.FieldName = 'HomePhone'
        Visible = False
        Width = 68
      end
      object cxGrid1ServerModeTableView1Description: TcxGridServerModeColumn
        DataBinding.FieldName = 'Description'
        Visible = False
        Options.Filtering = False
        Options.FilteringFilteredItemsList = False
        Options.FilteringPopup = False
        Options.GroupFooters = False
        Options.Grouping = False
        Width = 53
      end
      object cxGrid1ServerModeTableView1Email: TcxGridServerModeColumn
        DataBinding.FieldName = 'Email'
        Visible = False
      end
    end
    object cxGrid1ServerModeBandedTableView1: TcxGridServerModeBandedTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.Insert.Visible = False
      Navigator.Buttons.Delete.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Post.Visible = False
      Navigator.Buttons.Cancel.Visible = False
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.InfoPanel.Visible = True
      Navigator.InfoPanel.Width = 100
      Navigator.Visible = True
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          FieldName = 'OID'
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Format = 'Count: #,###'
          Kind = skCount
          FieldName = 'OrderDate'
          Column = cxGrid1ServerModeBandedTableView1OrderDate
        end>
      DataController.Summary.SummaryGroups = <>
      DateTimeHandling.Grouping = dtgByMonth
      OptionsBehavior.IncSearch = True
      OptionsCustomize.ColumnsQuickCustomization = True
      OptionsData.Deleting = False
      OptionsData.DeletingConfirmation = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.FooterAutoHeight = True
      OptionsView.FooterMultiSummaries = True
      OptionsView.GroupFooterMultiSummaries = True
      OptionsView.Indicator = True
      Bands = <
        item
          Caption = 'Orders'
        end
        item
          Caption = 'Customer Info'
        end>
      object cxGrid1ServerModeBandedTableView1OrderDate: TcxGridServerModeBandedColumn
        Caption = 'Order Date'
        DataBinding.FieldName = 'OrderDate'
        PropertiesClassName = 'TcxDateEditProperties'
        Properties.DisplayFormat = 'yyyy/mm/dd'
        Options.Filtering = False
        Options.FilteringFilteredItemsList = False
        Options.FilteringMRUItemsList = False
        Options.FilteringPopup = False
        Options.FilteringPopupMultiSelect = False
        SortIndex = 0
        SortOrder = soDescending
        Width = 111
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Price: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxSpinEditProperties'
        Properties.DisplayFormat = '$#,###'
        Width = 101
        Position.BandIndex = 0
        Position.ColIndex = 5
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Model: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Model'
        Width = 77
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Trademark: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Trademark'
        Width = 68
        Position.BandIndex = 0
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Category: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Category'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Horz = taCenter
        Properties.Images = ImageList
        Properties.Items = <
          item
            ImageIndex = 0
            Value = 'SALOON'
          end
          item
            ImageIndex = 1
            Value = 'SPORTS'
          end
          item
            ImageIndex = 2
            Value = 'TRUCK'
          end>
        Properties.ShowDescriptions = False
        Width = 51
        Position.BandIndex = 0
        Position.ColIndex = 4
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1TransmissAutomatic: TcxGridServerModeBandedColumn
        Caption = 'Automatic Transmission'
        DataBinding.FieldName = 'TransmissAutomatic'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Width = 28
        Position.BandIndex = 0
        Position.ColIndex = 6
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1HP: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'HP'
        Visible = False
        Position.BandIndex = 0
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1FirstName: TcxGridServerModeBandedColumn
        Caption = 'First Name'
        DataBinding.FieldName = 'FirstName'
        Width = 54
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1LastName: TcxGridServerModeBandedColumn
        Caption = 'Last Name'
        DataBinding.FieldName = 'LastName'
        Width = 60
        Position.BandIndex = 1
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Company: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Company'
        Visible = False
        GroupIndex = 0
        Position.BandIndex = 1
        Position.ColIndex = 5
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Prefix: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Prefix'
        Visible = False
        Position.BandIndex = 1
        Position.ColIndex = 6
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Title: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Title'
        Visible = False
        Position.BandIndex = 1
        Position.ColIndex = 7
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Address: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Address'
        Visible = False
        Position.BandIndex = 0
        Position.ColIndex = 7
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1City: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'City'
        Width = 77
        Position.BandIndex = 1
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1State: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'State'
        Visible = False
        Position.BandIndex = 1
        Position.ColIndex = 10
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Source: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Source'
        Width = 50
        Position.BandIndex = 1
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Customer: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Customer'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Visible = False
        Width = 30
        Position.BandIndex = 1
        Position.ColIndex = 4
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1HomePhone: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'HomePhone'
        Visible = False
        Width = 68
        Position.BandIndex = 1
        Position.ColIndex = 8
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Description: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Description'
        Visible = False
        Options.Filtering = False
        Options.FilteringFilteredItemsList = False
        Options.FilteringPopup = False
        Options.GroupFooters = False
        Options.Grouping = False
        Width = 53
        Position.BandIndex = 1
        Position.ColIndex = 11
        Position.RowIndex = 0
      end
      object cxGrid1ServerModeBandedTableView1Email: TcxGridServerModeBandedColumn
        DataBinding.FieldName = 'Email'
        Visible = False
        Position.BandIndex = 1
        Position.ColIndex = 9
        Position.RowIndex = 0
      end
    end
    object cxGrid1Level1: TcxGridLevel
      Caption = 'Table View'
      GridView = cxGrid1ServerModeTableView1
    end
    object cxGrid1Level2: TcxGridLevel
      Caption = 'Banded Table View'
      GridView = cxGrid1ServerModeBandedTableView1
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
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid1
    PopupMenus = <>
    Left = 160
    Top = 192
  end
  object ImageList: TImageList
    Left = 128
    Top = 32
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D4C3B800BA9F8D00C4AD
      9E00594436006B52410059443600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DCC1B10053392700E8D4
      C700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000F0C0A004C3A2E00DFD3
      CA00C8B3A400070605000706050057433500CFBDB000CFBDB0009F7A60000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD977D00533927006546
      3200D9BFAD00D9BFAD00BF987F00986C4F00986C4F0053392700533927008F64
      4A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000082644F00D8C9BF00F5F1EF00E5DC
      D500C7B1A2003C2E2500372B2200745946008B6A54000101010081634E00AA89
      7200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0F0F00053392700533927005339
      2700533927005339270053392700533927005339270053392700533927005339
      27009F7053005339270053392700BD977D0000000000000000002F221400C196
      67002F2214000000000000000000000000000000000000000000000000000000
      00002F221400C19667002F22140000000000A8866F00E4DAD3006B524100382B
      220095725A004F3D300000000000755A4700C4AD9E00100C0A00403127004D3B
      2F00C4AD9E00A9887100C9B4A600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A4755600E0CFC500E0CFC500C49E
      8900B8907600C5A18B00C19B8200A4755600E0CFC500E0CFC50053392700B68C
      6F007E5840005339270053392700533927002F2214002F221400C1966700E6D5
      C200E6D5C2002F2214002F2214002F2214002F2214002F2214002F2214002F22
      1400C1966700E6D5C200E6D5C2002F2214009F7A6000654D3D00070605006E55
      4300D6C7BC008465500016110E0070564400C0A8970056423400191410005F49
      3A001E171300523F32004B3A2E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A8775900A6765800B0846700B387
      6B00B3876B00B3876B00B58B6E00B3876B00B58B6E00AF82650094694D009C6F
      51007F5941008A6147006B4A3400533927002F221400AD7C47002F221400E6D5
      C200E6D5C2002F2214002F221400AD7C4700AD7C4700AD7C4700AD7C47002F22
      14002F221400E6D5C2002F2214002F2214009B775E00D2C1B500CEBBAE00F2ED
      EA00ECE5E000D0BEB100564234002F241D00876852005B463700745946008C6B
      55004B3A2E000F0C0A00372B2200614A3B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EEDED400C19B8200B58B
      6E00B58B6E00B58B6E00B58B6E00B58B6E00AA7A5B00B58B6E00AA7A5B00A373
      5500A27255009F70530092674B00E2CABA002F2214002F2214002F2214002F22
      14002F2214002F2214002F221400AD7C4700AD7C4700AD7C4700AD7C47002F22
      1400AD7C47002F2214002F2214002F221400A6846C00C6B0A100EDE6E200D3C2
      B600A27E650032271F0043342900A07B6200AA897200544033005B4637005E48
      3900735846002D231C00362A210033271F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000074513C00AD7E6000AD7E6000AD7E6000AD7E6000AD7E6000AD7E
      60006B4A3400A2725500EFEEE80000000000000000002F2214002F2214002F22
      14002F221400D8BDA000D8BDA000D8BDA000D8BDA000D8BDA0002F2214002F22
      14002F2214002F2214002F2214002F221400000000009B775E00413228004132
      28003C2E250096735B00CEBBAE00F1ECE800D1BFB3002D231C00110D0B005440
      3300644D3D005E4839007E604C00403127000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DCC1B1009F705300AD7E6000AD7E6000AD7E6000996D
      500074503A00C29D8500000000000000000000000000000000002F221400D8BD
      A000D8BDA000D8BDA000D8BDA000D8BDA000D8BDA000D8BDA000D8BDA000D8BD
      A0000000000000000000000000000000000000000000B2948000695140006951
      4000CEBBAE00E9E1DB00F4F0ED00F4F0EE00DACCC200B79B8800866751003E30
      260045352A006E5543007056440032271F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002F22
      1400D8BDA0002F2214002F2214002F2214002F2214002F221400D8BDA0000000
      00000000000000000000000000000000000000000000D2C1B50095725A00A07B
      6100FAF8F700F0EBE700D3C2B7009C775E00916F5800C2AA9A00D0BEB200BBA1
      8F007E604C00513E31008868520046362B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000785C
      4900E2D7D000BEA59400A7856D00D3C2B600C5AE9F0089695300B4978300C6B0
      A100CFBDB000BBA18F009A765D003B2D24000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CAB5
      A700695140008E6D5600B89C8900B4978300A988710095725A00A5826A00AD8D
      7700B5998500C7B1A200D8C9BF004F3D30000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C9B4A6007257450082644F00AC8C7600AC8B7500A07B6100A27E
      6500AF907B00AD8D7700654D3D00A07B61000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C9B4A600725745008E6D5600B2947F00A37F
      6700644D3D009F7A600000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C9B4A600755A47009673
      5B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFF81FF0000
      FF8FFFFF801F0000800FFFFF000F00000000C7F1000100000000000000010000
      00000000000000008000000000000000F801800080000000FC03C00F80000000
      FFFFE01F80000000FFFFFFFFE0000000FFFFFFFFE0000000FFFFFFFFF8000000
      FFFFFFFFFE030000FFFFFFFFFF8F000000000000000000000000000000000000
      000000000000}
  end
end
