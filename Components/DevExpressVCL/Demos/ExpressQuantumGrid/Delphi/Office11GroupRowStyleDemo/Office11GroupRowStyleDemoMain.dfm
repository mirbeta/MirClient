inherited Office11GroupRowStyleDemoMainForm: TOffice11GroupRowStyleDemoMainForm
  Left = 300
  Top = 120
  Caption = 'ExpressQuantumGrid Office11GroupRowStyle Demo'
  ClientHeight = 546
  ClientWidth = 792
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 792
    Height = 32
    Caption = 
      '  Use Outlook 2003 GroupRow style. Experiment by changing indivi' +
      'dual Options. Try different DateTime grouping kinds.'
  end
  inherited sbMain: TStatusBar
    Top = 527
    Width = 792
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 32
    Width = 792
    Height = 495
    Align = alClient
    TabOrder = 0
    object tvMail: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DateTimeHandling.Filters = [dtfRelativeDayPeriods, dtfRelativeMonths]
      DateTimeHandling.IgnoreTimeForFiltering = True
      DateTimeHandling.Grouping = dtgRelativeToToday
      OptionsView.ColumnAutoWidth = True
      OptionsView.GridLines = glHorizontal
      OptionsView.GroupRowStyle = grsOffice11
      Styles.OnGetContentStyle = tvMailStylesGetContentStyle
      object tvMailImportance: TcxGridColumn
        DataBinding.ValueType = 'Integer'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = imgImportance
        Properties.Items = <
          item
            Description = 'Low'
            ImageIndex = 0
            Value = 0
          end
          item
            Description = 'Medium'
            Value = 1
          end
          item
            Description = 'High'
            ImageIndex = 1
            Value = 2
          end>
        Properties.ShowDescriptions = False
        HeaderGlyph.Data = {
          3A010000424D3A010000000000003600000028000000060000000D0000000100
          1800000000000401000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
          0000FFFFFFFFFFFF000000000000000000FFFFFF0000FFFFFFFFFFFFFFFFFF00
          0000FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFF000000FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF000000FFFF
          FFFFFFFF0000FFFFFFFFFFFF000000000000000000FFFFFF0000FFFFFFFFFFFF
          000000000000000000FFFFFF0000FFFFFFFFFFFF000000000000000000FFFFFF
          0000FFFFFFFFFFFF000000000000000000FFFFFF0000FFFFFFFFFFFFFFFFFF00
          0000FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        HeaderGlyphAlignmentHorz = taCenter
        MinWidth = 35
        Options.Filtering = False
        Options.HorzSizing = False
        Width = 35
        IsCaptionAssigned = True
      end
      object tvMailIcon: TcxGridColumn
        DataBinding.ValueType = 'Integer'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = imgImportance
        Properties.Items = <
          item
            Description = 'Unread'
            ImageIndex = 3
            Value = 0
          end
          item
            Description = 'Read'
            ImageIndex = 2
            Value = 1
          end>
        Properties.ShowDescriptions = False
        HeaderGlyph.Data = {
          8E020000424D8E0200000000000036000000280000000D0000000F0000000100
          1800000000005802000000000000000000000000000000000000D8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          EC00D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9EC00D8E9ECD8E9EC00000000000000000000000000000000
          0000000000000000000000D8E9ECD8E9EC00D8E9ECD8E9EC000000D8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000D8E9ECD8E9EC00D8E9ECD8E9EC
          000000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000D8E9ECD8E9
          EC00D8E9ECD8E9EC000000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          000000D8E9ECD8E9EC00D8E9ECD8E9EC000000D8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9EC000000D8E9ECD8E9EC00D8E9ECD8E9EC000000D8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000D8E9ECD8E9EC00D8E9ECD8E9EC
          000000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000D8E9ECD8E9
          EC00D8E9ECD8E9EC000000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          000000D8E9ECD8E9EC00D8E9ECD8E9EC000000D8E9ECD8E9ECD8E9ECD8E9EC00
          0000000000000000000000D8E9ECD8E9EC00D8E9ECD8E9EC000000D8E9ECD8E9
          ECD8E9ECD8E9EC000000D8E9EC000000D8E9ECD8E9ECD8E9EC00D8E9ECD8E9EC
          000000D8E9ECD8E9ECD8E9ECD8E9EC000000000000D8E9ECD8E9ECD8E9ECD8E9
          EC00D8E9ECD8E9EC000000000000000000000000000000000000D8E9ECD8E9EC
          D8E9ECD8E9ECD8E9EC00D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00}
        HeaderGlyphAlignmentHorz = taCenter
        MinWidth = 35
        Options.Filtering = False
        Options.HorzSizing = False
        Width = 35
        IsCaptionAssigned = True
      end
      object tvMailAttachment: TcxGridColumn
        DataBinding.ValueType = 'Integer'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = imgImportance
        Properties.Items = <
          item
            ImageIndex = 4
            Value = 0
          end
          item
            Value = 1
          end>
        HeaderGlyph.Data = {
          9E010000424D9E010000000000003600000028000000080000000F0000000100
          1800000000006801000000000000000000000000000000000000D8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000000000000000000000D8E9ECD8
          E9ECD8E9EC000000000000D8E9ECD8E9EC000000000000D8E9ECD8E9EC000000
          D8E9ECD8E9ECD8E9ECD8E9EC000000D8E9ECD8E9EC000000D8E9EC0000000000
          00D8E9EC000000D8E9ECD8E9EC000000000000D8E9ECD8E9EC000000000000D8
          E9ECD8E9EC000000000000D8E9ECD8E9EC000000000000D8E9ECD8E9EC000000
          000000D8E9ECD8E9EC000000000000D8E9ECD8E9EC000000000000D8E9ECD8E9
          EC000000000000D8E9ECD8E9ECD8E9EC000000D8E9ECD8E9ECD8E9EC000000D8
          E9ECD8E9ECD8E9EC000000D8E9ECD8E9ECD8E9EC000000D8E9ECD8E9ECD8E9EC
          D8E9EC000000000000000000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9EC}
        HeaderGlyphAlignmentHorz = taCenter
        MinWidth = 35
        Options.Editing = False
        Options.Filtering = False
        Options.HorzSizing = False
        Width = 35
      end
      object tvMailFrom: TcxGridColumn
        Caption = 'From'
        Width = 132
      end
      object tvMailSubject: TcxGridColumn
        Caption = 'Subject'
        Width = 355
      end
      object tvMailReceived: TcxGridColumn
        Caption = 'Received'
        DataBinding.ValueType = 'DateTime'
        SortIndex = 1
        SortOrder = soDescending
        Width = 98
      end
      object tvMailSent: TcxGridColumn
        Caption = 'Sent'
        DataBinding.ValueType = 'DateTime'
        Visible = False
        GroupIndex = 0
        SortIndex = 0
        SortOrder = soDescending
        Width = 154
      end
    end
    object lvMail: TcxGridLevel
      GridView = tvMail
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miOffice11GroupRowStyle: TMenuItem
        Caption = '&Outlook 2003 GroupRow Style'
        Checked = True
        OnClick = miOffice11GroupRowStyleClick
      end
      object miGroupBySorting: TMenuItem
        Caption = '&Group by Sorting'
        Checked = True
        OnClick = miGroupBySortingClick
      end
      object miAlwaysExpandedGroups: TMenuItem
        Caption = 'Always &Expanded Groups'
        Checked = True
        OnClick = miAlwaysExpandedGroupsClick
      end
    end
    object miDateTimeGrouping: TMenuItem [2]
      Caption = 'DateTime Grouping'
      object miDateTimeGroupingByDateAndTime: TMenuItem
        Caption = 'dtgByDateAndTime'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
      object miDateTimeGroupingRelativeToToday: TMenuItem
        Tag = 1
        Caption = 'dtgRelativeToToday'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
      object miDateTimeGroupingByHour: TMenuItem
        Tag = 2
        Caption = 'dtgByHour'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
      object miDateTimeGroupingByDate: TMenuItem
        Tag = 3
        Caption = 'dtgByDate'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
      object miDateTimeGroupingByMonth: TMenuItem
        Tag = 4
        Caption = 'dtgByMonth'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
      object miDateTimeGroupingByYear: TMenuItem
        Tag = 5
        Caption = 'dtgByYear'
        GroupIndex = 1
        RadioItem = True
        OnClick = miDateTimeGroupingClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object imgImportance: TImageList
    Height = 12
    Width = 12
    Left = 408
    Top = 24
    Bitmap = {
      494C0101050009000C000C000C00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000001800000001002000000000000012
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000DBD5CF006048300060504000C9C0B7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007058
      4000E4DFDB00D2C8C300E1DEDB00E4DEDB006048300000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007058
      4000000000007050400060484000000000006048300000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008068
      5000000000007060500070504000000000006050400000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009078
      6000000000008070600080605000000000007058400000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A080
      7000000000009078700090706000000000008068500000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A080
      7000000000009080700090706000000000008070600000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DBD2C900A088700090807000D5CFC90000000000000000000000
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
      0000F9F4F100A0503000A0502000F3E9E3000000000000000000000000000000
      000000000000000000000000000000000000F2F4FB00657BC8002F4FB100F0F2
      F9000000000000000000000000000000000000000000C0908000C0908000C080
      8000B0808000B0787000B0787000B0707000B0707000A0706000A06060000000
      00000000000080A8C00040709000306880003068800030608000305870003058
      700030587000305060003048600000000000000000000000000000000000F9F4
      F100B0603000E0805000D0785000A0502000F3E9E30000000000000000000000
      000000000000000000000000000000000000EDF0F9006080F0002048D000D2D6
      EA000000000000000000000000000000000000000000C0909000FFF8D000FFF0
      C000FFE8B000FFD8A000FFC09000F0B08000F0A87000E0906000A06860000000
      00000000000080A8C000A0E8FF0080E0F00080D8F00070D0F00070C8F00070C8
      F00070C8F00060C8F00030506000000000000000000000000000F9F4F100B058
      3000F0A07000F0A07000E0805000D0704000B0583000F9F4F100000000000000
      00000000000000000000000000000000000000000000C3CCE700A4B2E3000000
      00000000000000000000000000000000000000000000D0989000D0908000F0F0
      E000FFFFF000FFF8E000FFF0D000FFE8B000F0D0A000C0806000A07070000000
      00000000000080A8C000B0F8FF0090F0FF0090F0FF0090F0FF0090F0FF0090F0
      FF0090F0FF0080E0FF0030506000000000000000000000000000E9B29C00E5A4
      8A00E0907000FFA88000F0885000B0603000A0502000A0502000000000000000
      000000000000000000000000000000000000000000009DB2EA008F9FDF000000
      00000000000000000000000000000000000000000000D0A09000F0D8B000C0A0
      A000F0F0E000FFF0E000F0E8D000F0D0B000C0787000F0D0B000A07870000000
      00000000000080B0C00090E0F00090F8FF0090F0FF0080E8FF0080E8FF0080E8
      FF0090F0FF0070D8F00040586000000000000000000000000000000000000000
      0000E0906000FFA88000F0906000B05830000000000000000000000000000000
      000000000000000000000000000000000000F3F5FC004970E5002F5CD800DFE4
      F1000000000000000000000000000000000000000000D0A8A000D0989000A0F8
      FF00C0A09000C0989000C0888000B090900070D8FF00B0989000B08080000000
      00000000000090B0C00060B8D00080E8FF0070E0F00050B0E0003090C00070D0
      F00070D8F0004098C00040587000000000000000000000000000000000000000
      0000E0907000FFB09000FF906000A05020000000000000000000000000000000
      0000000000000000000000000000000000009FB3E7003060F0000040FF009FB1
      E7000000000000000000000000000000000000000000D0A8A000C0FFFF00C0FF
      FF00B0F8FF00B0F8FF0090E8FF0080E0FF0070D8FF0070D8FF00A07870000000
      00000000000090C0D00090E8F00060B8E00080E0F000A0F8FF0090F0FF0090E8
      FF0050A8D00080E0F00040607000000000000000000000000000000000000000
      0000E0907000FFB89000FF986000A05020000000000000000000000000000000
      0000000000000000000000000000000000006383E5004B78F0000048FF001F50
      D5000000000000000000000000000000000000000000D0A8A000A0E8FF00A0E8
      FF00A0E8FF0090E0FF0080D8F00070D0F00070C8F00060C8F000B47F87000000
      00000000000090C0D00060C0E00090E0F000A0F8FF00A0F8FF00A0F8FF0090F0
      FF0090E8FF0050A0C00050687000000000000000000000000000000000000000
      0000E0987000FFB89000FF987000A05020000000000000000000000000000000
      0000000000000000000000000000000000005078E0006088FF003060FF000038
      D0000000000000000000000000000000000000000000F6EEED00E0FFFF00E0FF
      FF00F0FFFF00F0FFFF00C0F8FF00A0F0FF0080E8FF0080E0FF00F3EBEA000000
      00000000000090C8D000C0F0FF00C0FFFF00C0F8FF00B0F8FF00B0F8FF00A0F8
      FF0090F0FF0090E8FF0050687000000000000000000000000000000000000000
      0000F0A08000FFC0A000FFB89000A05020000000000000000000000000000000
      0000000000000000000000000000000000007088E00090A8FF006088FF002050
      D000000000000000000000000000000000000000000000000000F6EEED00CBA4
      A400F0FFFF00F0FFFF00E0FFFF00B0F8FF00B47F8700F3EBEA00000000000000
      00000000000090C8D000E0FFFF00E0FFFF00E0FFFF00E0FFFF00E0FFFF00D0FF
      FF00D0FFFF00B0F8FF0050708000000000000000000000000000000000000000
      0000F0A57800E1A57800E19E7800D28769000000000000000000000000000000
      000000000000000000000000000000000000D2D9F000788FE100697FE100DBE2
      F60000000000000000000000000000000000000000000000000000000000F6EE
      ED00C7A49D00C3A5A500C0989000C0989000EFE5E30000000000000000000000
      00000000000090C8D00090C8D00090C8D00090C0D00090C0D00090B8C00080B0
      C00080B0C00080A8C00080A8C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000030000000180000000100010000000000C00000000000000000000000
      000000000000000000000000FFFFFF00FFF0000000000000FFF0000000000000
      F0F0000000000000E070000000000000E970000000000000E970000000000000
      E970000000000000E970000000000000E970000000000000F870000000000000
      FFF0000000000000FFF0000000000000FFFFFFFFFFFF0000F0FF0F8018010000
      E07F0F8018010000C03F9F8018010000C03F9F8018010000F0FF0F8018010000
      F0FF0F8018010000F0FF0F8018010000F0FF0F8018010000F0FF0FC038010000
      F0FF0FE078010000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object dsPersons: TDataSource
    DataSet = mdPersons
    Left = 288
    Top = 188
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 440
    Top = 24
    PixelsPerInch = 96
    object UnreadStyle: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
  end
  object mdPersons: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0D0000001900000001000600464E414D4500190000
      00010006004C4E414D4500040000000C00030049440014000000010006004D4E
      414D45000F00000001000800434F554E545259000A00000001000B00504F5354
      414C434F4445000F0000000100050043495459003C0000000100080041444452
      45535300180000000100060050484F4E45001800000001000400464158003200
      000001000600454D41494C003200000001000900484F4D455041474500040000
      0003000D004445504152544D454E54494400}
    SortOptions = []
    OnCalcFields = mdPersonsCalcFields
    Left = 320
    Top = 187
    object mdPersonsFNAME: TStringField
      FieldName = 'FNAME'
      Size = 25
    end
    object mdPersonsLNAME: TStringField
      FieldName = 'LNAME'
      Size = 25
    end
    object mdPersonsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdPersonsMNAME: TStringField
      FieldName = 'MNAME'
    end
    object mdPersonsCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 15
    end
    object mdPersonsPOSTALCODE: TStringField
      FieldName = 'POSTALCODE'
      Size = 10
    end
    object mdPersonsCITY: TStringField
      FieldName = 'CITY'
      Size = 15
    end
    object mdPersonsADDRESS: TStringField
      FieldName = 'ADDRESS'
      Size = 60
    end
    object mdPersonsPHONE: TStringField
      FieldName = 'PHONE'
      Size = 24
    end
    object mdPersonsFAX: TStringField
      FieldName = 'FAX'
      Size = 24
    end
    object mdPersonsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 50
    end
    object mdPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 50
    end
    object mdPersonsDEPARTMENTID: TIntegerField
      FieldName = 'DEPARTMENTID'
    end
    object mdPersonsFullName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'FullName'
      Calculated = True
    end
  end
end
