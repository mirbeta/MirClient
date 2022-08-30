inherited TeeChartRLMainForm: TTeeChartRLMainForm
  Left = 382
  Top = 143
  Caption = 'Report Links Demo - TeeChart & DBTeeChart'
  ClientHeight = 432
  ClientWidth = 707
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 707
    Caption = 'This example demonstrates the TeeChart printing capabilities.'
  end
  inherited sbMain: TStatusBar
    Top = 413
    Width = 707
  end
  inherited ToolBar1: TToolBar
    Width = 707
  end
  object PageControl1: TPageControl [3]
    Left = 0
    Top = 41
    Width = 707
    Height = 372
    ActivePage = tsTeeChart
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object tsTeeChart: TTabSheet
      Caption = 'TeeChart'
      object TeeChart: TChart
        Left = 0
        Top = 0
        Width = 699
        Height = 344
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        BackWall.Color = 8454143
        BackWall.Pen.Color = clRed
        BottomWall.Size = 4
        LeftWall.Color = 16777088
        LeftWall.Size = 4
        Title.Alignment = taRightJustify
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -16
        Title.Font.Name = 'Arial'
        Title.Font.Style = []
        Title.Text.Strings = (
          'American Charting Suppliers, Inc.'
          '1997 Production Volume')
        BackColor = 8454143
        Frame.Color = clRed
        LeftAxis.Grid.Visible = False
        LeftAxis.Title.Caption = 'Millions of $'
        LeftAxis.Title.Font.Charset = DEFAULT_CHARSET
        LeftAxis.Title.Font.Color = clBlack
        LeftAxis.Title.Font.Height = -15
        LeftAxis.Title.Font.Name = 'Arial'
        LeftAxis.Title.Font.Style = []
        Legend.Alignment = laTop
        Legend.ColorWidth = 22
        Legend.DividingLines.Color = clSilver
        Legend.DividingLines.Visible = True
        Legend.ShadowColor = clGray
        Legend.ShadowSize = 5
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Series5: TBarSeries
          Marks.ArrowLength = 20
          Marks.Visible = False
          SeriesColor = clNavy
          Title = 'San Francisco'
          MultiBar = mbNone
          XValues.DateTime = False
          XValues.Name = 'X'
          XValues.Multiplier = 1.000000000000000000
          XValues.Order = loAscending
          YValues.DateTime = False
          YValues.Name = 'Bar'
          YValues.Multiplier = 1.000000000000000000
          YValues.Order = loNone
        end
        object Series7: TBarSeries
          Marks.ArrowLength = 20
          Marks.BackColor = 16777088
          Marks.Visible = False
          SeriesColor = clYellow
          Title = 'Hong Kong'
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Multiplier = 1.000000000000000000
          XValues.Order = loAscending
          YValues.DateTime = False
          YValues.Name = 'Bar'
          YValues.Multiplier = 1.000000000000000000
          YValues.Order = loNone
        end
        object Series6: TBarSeries
          Marks.ArrowLength = 20
          Marks.BackColor = 65408
          Marks.Visible = True
          SeriesColor = clBlue
          Title = 'Paris'
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Multiplier = 1.000000000000000000
          XValues.Order = loAscending
          YValues.DateTime = False
          YValues.Name = 'Bar'
          YValues.Multiplier = 1.000000000000000000
          YValues.Order = loNone
        end
      end
    end
    object tsDBTeeChart: TTabSheet
      Caption = 'DBTeeChart'
      object DBChart: TDBChart
        Left = 0
        Top = 0
        Width = 699
        Height = 344
        AllowPanning = pmNone
        AllowZoom = False
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        BackWall.Pen.Visible = False
        Foot.Font.Charset = DEFAULT_CHARSET
        Foot.Font.Color = clRed
        Foot.Font.Height = -15
        Foot.Font.Name = 'Arial'
        Foot.Font.Style = [fsBold]
        Foot.Frame.Color = clScrollBar
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -16
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold, fsItalic]
        Title.Frame.Color = clScrollBar
        Title.Text.Strings = (
          'Pie Chart built from dxMemData: DBTreeChart.dat')
        AxisVisible = False
        BottomAxis.Grid.Color = clScrollBar
        ClipPoints = False
        Frame.Visible = False
        LeftAxis.Grid.Color = clScrollBar
        Legend.Alignment = laLeft
        Legend.Color = clAqua
        Legend.Font.Charset = DEFAULT_CHARSET
        Legend.Font.Color = clBlack
        Legend.Font.Height = -16
        Legend.Font.Name = 'Arial'
        Legend.Font.Style = [fsBold, fsItalic]
        Legend.Frame.Color = clTeal
        Legend.Frame.Width = 4
        Legend.ShadowColor = clGray
        Legend.ShadowSize = 11
        Legend.TextStyle = ltsLeftPercent
        RightAxis.Grid.Color = clScrollBar
        TopAxis.Grid.Color = clScrollBar
        View3DOptions.Elevation = 315
        View3DOptions.Orthogonal = False
        View3DOptions.Perspective = 0
        View3DOptions.Rotation = 360
        View3DWalls = False
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object PieSeries1: TPieSeries
          Cursor = crDrag
          Marks.Arrow.Color = clYellow
          Marks.Arrow.Width = 2
          Marks.ArrowLength = 11
          Marks.BackColor = clWhite
          Marks.Font.Charset = DEFAULT_CHARSET
          Marks.Font.Color = clRed
          Marks.Font.Height = -13
          Marks.Font.Name = 'Arial'
          Marks.Font.Style = [fsItalic]
          Marks.Frame.Color = clNavy
          Marks.Frame.Width = 3
          Marks.Style = smsLabelValue
          Marks.Visible = True
          DataSource = mdTeeChart
          SeriesColor = clRed
          XLabelsSource = 'NAME'
          Circled = True
          OtherSlice.Text = 'Other'
          PieValues.DateTime = False
          PieValues.Name = 'Y'
          PieValues.Multiplier = 1.000000000000000000
          PieValues.Order = loNone
          PieValues.ValueSource = 'SIZE'
          Left = 187
          Top = 6
        end
      end
    end
  end
  inherited sty: TActionList
    inherited actDesigner: TAction
      Enabled = False
    end
  end
  inherited dxComponentPrinter: TdxComponentPrinter
    CurrentLink = dxComponentPrinterLink1
    object dxComponentPrinterLink1: TdxTeeChartReportLink
      Component = TeeChart
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      BuiltInReportLink = True
    end
    object dxComponentPrinterLink2: TdxDBTeeChartReportLink
      Component = DBChart
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      BuiltInReportLink = True
    end
  end
  inherited dxPSEngineController1: TdxPSEngineController
    Active = True
  end
  inherited ilMain: TcxImageList
    FormatVersion = 1
  end
  object DataSource1: TDataSource
    DataSet = mdTeeChart
    Left = 527
    Top = 64
  end
  object mdTeeChart: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F030000000A000000010005004E414D450002000000
      0200050053495A4500020000000200070057454947485400010A000000416E67
      656C20466973680102000102000103000000426F61010A000108000108000000
      4372697474657273011E000114000109000000486F75736520436174010A0001
      050001060000004F63656C6F740128000123000106000000506172726F740105
      000105000106000000546574726173010200010200}
    SortOptions = []
    Left = 492
    Top = 65
    object mdTeeChartNAME: TStringField
      FieldName = 'NAME'
      Size = 10
    end
    object mdTeeChartSIZE: TSmallintField
      FieldName = 'SIZE'
    end
    object mdTeeChartWEIGHT: TSmallintField
      FieldName = 'WEIGHT'
    end
  end
end
