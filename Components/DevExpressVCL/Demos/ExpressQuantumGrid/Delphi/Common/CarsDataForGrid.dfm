inherited dmGridCars: TdmGridCars
  OldCreateOrder = True
  inherited mdModels: TdxMemData
    Persistent.Data = {
      5665728FC2F5285C8FFE3F140000000400000003000300494400040000000300
      0C0054726164656D61726B494400FF000000140005004E616D6500FF00000014
      000D004D6F64696669636174696F6E000400000003000B0043617465676F7279
      494400220000000800060050726963650004000000030009004D504720436974
      79000400000003000C004D50472048696768776179000400000003000600446F
      6F7273000400000003000C00426F64795374796C654944000400000003000A00
      43696C696E6465727300FF00000014000B00486F727365706F77657200FF0000
      0014000700546F7271756500FF000000140014005472616E736D697373696F6E
      205370656564730004000000030012005472616E736D697373696F6E20547970
      65000000000019000C004465736372697074696F6E00000000000D000600496D
      61676500000000000D00060050686F746F00080000000B000E0044656C697665
      72792044617465000200000005000800496E53746F636B00}
  end
  inherited mdBodyStyle: TdxMemData
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000300494400FF0000001400
      05004E616D6500}
  end
  inherited mdCategory: TdxMemData
    Persistent.Data = {
      5665728FC2F5285C8FFE3F030000000400000003000300494400FF0000001400
      05004E616D6500000000000D0008005069637475726500}
  end
  inherited mdTrademark: TdxMemData
    Persistent.Data = {
      5665728FC2F5285C8FFE3F050000000400000003000300494400FF0000001400
      05004E616D6500FF000000140005005369746500000000000D0005004C6F676F
      000000000019000C004465736372697074696F6E00}
  end
  inherited mdTransmissionType: TdxMemData
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000300494400FF0000001400
      05004E616D6500}
  end
  inherited EditRepository: TcxEditRepository
    object EditRepositoryTrademarkLookup: TcxEditRepositoryExtLookupComboBoxItem
      Properties.DropDownAutoSize = True
      Properties.FocusPopup = True
      Properties.View = GridViewRepositoryDBBandedTableView
      Properties.KeyFieldNames = 'ID'
      Properties.ListFieldItem = GridViewRepositoryDBBandedTableViewName
    end
  end
  object GridViewRepository: TcxGridViewRepository
    Left = 216
    Top = 72
    object GridViewRepositoryDBBandedTableView: TcxGridDBBandedTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = dsTrademark
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.GroupByBox = False
      OptionsView.Header = False
      OptionsView.BandHeaders = False
      Bands = <
        item
          Width = 178
        end
        item
          Width = 379
        end>
      object GridViewRepositoryDBBandedTableViewRecId: TcxGridDBBandedColumn
        DataBinding.FieldName = 'RecId'
        Visible = False
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 1
      end
      object GridViewRepositoryDBBandedTableViewID: TcxGridDBBandedColumn
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 1
      end
      object GridViewRepositoryDBBandedTableViewName: TcxGridDBBandedColumn
        DataBinding.FieldName = 'Name'
        Width = 108
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object GridViewRepositoryDBBandedTableViewSite: TcxGridDBBandedColumn
        DataBinding.FieldName = 'Site'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Width = 184
        Position.BandIndex = 1
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object GridViewRepositoryDBBandedTableViewLogo: TcxGridDBBandedColumn
        DataBinding.FieldName = 'Logo'
        RepositoryItem = EditRepositoryImage
        Width = 302
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.LineCount = 5
        Position.RowIndex = 0
      end
      object GridViewRepositoryDBBandedTableViewDescription: TcxGridDBBandedColumn
        DataBinding.FieldName = 'Description'
        RepositoryItem = EditRepositoryMemo
        Width = 360
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.LineCount = 4
        Position.RowIndex = 1
      end
    end
  end
end
