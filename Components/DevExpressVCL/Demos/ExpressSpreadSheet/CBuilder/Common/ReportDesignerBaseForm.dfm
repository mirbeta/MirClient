inherited frmReportDesignerBase: TfrmReportDesignerBase
  Left = 0
  Top = 0
  Caption = 'ReportDesigner Base Form'
  ClientHeight = 461
  ClientWidth = 887
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbMain: TStatusBar
    Top = 442
    Width = 887
  end
  object ReportDesigner: TdxSpreadSheetReportDesigner [1]
    Left = 0
    Top = 0
    Width = 549
    Height = 442
    DataBinding.DataGroups = <>
    DataBinding.SortedFields = <>
    Align = alClient
    OnSelectionChanged = ReportDesignerSelectionChanged
    Data = {
      E101000044585353763242460900000042465320000000000000000001000101
      010100000000000001004246532000000000424653200200000001000000200B
      00000007000000430061006C0069006200720069000000000000200000002000
      000000200000000020000000002000000000200007000000470045004E004500
      520041004C0000000000000200000000000000000101000000200B0000000700
      0000430061006C00690062007200690000000000002000000020000000002000
      00000020000000002000000000200007000000470045004E004500520041004C
      000000000000020000000000000000014246532001000000424653201D000000
      5400640078005300700072006500610064005300680065006500740052006500
      70006F00720074005400610062006C0065005600690065007700060000005300
      6800650065007400310001FFFFFFFFFFFFFFFF64000000020000000200000002
      0000005500000014000000020000000200000000020000000000000100000000
      0001010000424653205500000000000000424653200000000042465320140000
      0000000000424653200000000000000000000000000100000000000000000000
      0000000000000000004246532000000000000000000000000042465320000000
      0000000000}
  end
  object cxSplitter1: TcxSplitter [2]
    Left = 549
    Top = 0
    Width = 8
    Height = 442
    Control = Panel1
  end
  object Panel1: TPanel [3]
    Left = 557
    Top = 0
    Width = 330
    Height = 442
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object cxSplitter2: TcxSplitter
      Left = 0
      Top = 164
      Width = 330
      Height = 4
      AlignSplitter = salBottom
      Control = cxgFilter
    end
    object cxgFieldChooserSite: TcxGroupBox
      Left = 0
      Top = 0
      Align = alClient
      Caption = ' Field Chooser '
      TabOrder = 1
      Height = 164
      Width = 330
    end
    object cxgFilter: TcxGroupBox
      Left = 0
      Top = 168
      Align = alBottom
      Caption = ' Filter Options'
      TabOrder = 2
      Height = 274
      Width = 330
      object cbxUseFilter: TcxCheckBox
        Left = 14
        Top = 24
        Caption = 'Use Filter'
        TabOrder = 0
        Transparent = True
        OnClick = FilterChanged
      end
      object Filter: TcxFilterControl
        Left = 16
        Top = 51
        Width = 299
        Height = 166
        Anchors = [akLeft, akTop, akRight, akBottom]
        LinkComponent = ReportDesigner
        TabOrder = 1
      end
      object btnApply: TButton
        Left = 40
        Top = 232
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 2
        OnClick = btnApplyClick
      end
      object btnClear: TButton
        Left = 176
        Top = 232
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 3
        OnClick = btnClearClick
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 376
    Top = 40
    inherited miOptions: TMenuItem
      Caption = 'Report'
      object miDesignView: TMenuItem [0]
        AutoCheck = True
        Caption = 'Design View'
        Checked = True
        OnClick = miDesignViewClick
      end
      object asd1: TMenuItem
        Caption = '-'
      end
      object miSections: TMenuItem
        Caption = 'Sections'
        object miHeader: TMenuItem
          AutoCheck = True
          Caption = 'Header'
          OnClick = OnSectionClick
        end
        object miDetail: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Detail'
          OnClick = OnSectionClick
        end
        object miFooter: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Footer'
          OnClick = OnSectionClick
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object miRemove: TMenuItem
          Caption = 'Remove'
          OnClick = miRemoveClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miPreview: TMenuItem
        Caption = 'Preview'
        ShortCut = 116
        OnClick = miPreviewClick
      end
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 296
    Top = 40
  end
  inherited SaveDialog: TSaveDialog
    Left = 216
    Top = 40
  end
end
