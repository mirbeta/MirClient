object QRListForm: TQRListForm
  Left = 81
  Top = 136
  Width = 652
  Height = 212
  HorzScrollBar.Range = 1200
  VertScrollBar.Range = 2000
  AutoScroll = False
  Caption = 'QuickReport List'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object QuickReport: TQuickRep
    Left = -22
    Top = -26
    Width = 794
    Height = 1123
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    AfterPrint = QuickReportAfterPrint
    BeforePrint = QuickReportBeforePrint
    DataSet = FMain.DBTreePrintDataSet
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Options = [FirstPageHeader, LastPageFooter]
    Page.Columns = 1
    Page.Orientation = poPortrait
    Page.PaperSize = A4
    Page.Values = (
      100.000000000000000000
      2970.000000000000000000
      100.000000000000000000
      2100.000000000000000000
      100.000000000000000000
      100.000000000000000000
      0.000000000000000000)
    PrintIfEmpty = False
    PrinterSettings.Copies = 1
    PrinterSettings.Duplex = False
    PrinterSettings.FirstPage = 0
    PrinterSettings.LastPage = 0
    PrinterSettings.OutputBin = First
    SnapToGrid = True
    Units = MM
    Zoom = 100
    object PageHeader: TQRBand
      Left = 38
      Top = 38
      Width = 718
      Height = 24
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Frame.Width = 0
      AlignToBottom = False
      Color = clWhite
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        63.500000000000000000
        1899.708333333333000000)
      BandType = rbPageHeader
      object PageNumber: TQRSysData
        Left = 5
        Top = 4
        Width = 10
        Height = 17
        Frame.Color = clBlack
        Frame.DrawTop = False
        Frame.DrawBottom = False
        Frame.DrawLeft = False
        Frame.DrawRight = False
        Size.Values = (
          44.979166666666670000
          1873.250000000000000000
          10.583333333333330000
          26.458333333333330000)
        Alignment = taRightJustify
        AlignToBand = True
        AutoSize = False
        Color = clWhite
        Data = qrsPageNumber
        Text = 'Page number '
        Transparent = False
        FontSize = 10
      end
    end
    object Title: TQRBand
      Left = 38
      Top = 62
      Width = 718
      Height = 37
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Frame.Width = 0
      AlignToBottom = False
      Color = clWhite
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        97.895833333333340000
        1899.708333333333000000)
      BandType = rbTitle
      object Image2: TImage
        Left = 8
        Top = 0
        Width = 57
        Height = 33
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000003333330000000000
          000000000000333BBBBBBBB3330000000000000000033BBBBBBBBBBB33300000
          000000000003BBBBBBBBBBB3B3300000000000000003BBBBFFF3333333300000
          0000000000033333BBBBBBB3303000000000000000033BBBBBB7777777777777
          777777700003BBBBFFFFFFFFFFFFFFFFFFFFFF7000033333BBBF7FFFFFDDDFFF
          FFFFFF7000033BBBBBBF7F7070DDDFFFFFFFFF700003BBBBFFFF7F0FFFDDDFFF
          FFFFFF7000033333BBBF7F7FFFFFFFFFFFFFFF7000033BBBBBBF7F0FFFFFFFFF
          FFFFFF700003BBBBFFFF7F7FFFCCCFFFFFFFFF7000033333BFFF7F0707CCCFFF
          FFFFFF70000333B3BBFF7F7FFFCCCFFFFFFFFF700003333B3BBF7F0FFFFFFFFF
          FFFFFF70000333B3B3BF7F7FFFFFFFF999FFFF70000033333B3F7F0FFFF07079
          99FFFF7000000000333F7F7FFFF7FFF999FFFF7000000000000F7F0FFFF0FFFF
          FFFFFF7000000000000F7F7FFF222FFFFFFFFF7000000000000F7F0707222FFF
          FFFFFF7000000000000F7F7FFF222FFFFFFFFF7000000000000F7F0FFFFFFFFF
          FFFFFF7000000000000F7FFFFFFFFFFFFFFFFF7000000000000F777777777777
          7777777000000000000FFFFFFFFFFFFFFFFFFFF0000000000000000000000000
          00000000FFFFFFFFFFFFFFFFFF03FFFFF0003FFFE0001FFFC0000FFFC0000FFF
          C0000FFFC0000000C0000000C0000000C0000000C0000000C0000000C0000000
          C0000000C0000000C0000000C0000000C0000000E0000000F0000000FF000000
          FFE00000FFE00000FFE00000FFE00000FFE00000FFE00000FFE00001FFE00001
          FFFFFFFF}
      end
      object QRLabel1: TQRLabel
        Left = 176
        Top = 8
        Width = 265
        Height = 25
        Frame.Color = clBlack
        Frame.DrawTop = False
        Frame.DrawBottom = False
        Frame.DrawLeft = False
        Frame.DrawRight = False
        Size.Values = (
          66.145833333333340000
          465.666666666666700000
          21.166666666666670000
          701.145833333333400000)
        Alignment = taCenter
        AlignToBand = False
        AutoSize = False
        AutoStretch = False
        Caption = 'DBTreeView print example'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        WordWrap = True
        FontSize = 12
      end
    end
    object Detail: TQRBand
      Left = 38
      Top = 99
      Width = 718
      Height = 26
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Frame.Width = 0
      AfterPrint = DetailAfterPrint
      AlignToBottom = False
      BeforePrint = DetailBeforePrint
      Color = clWhite
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        68.791666666666660000
        1899.708333333333000000)
      BandType = rbDetail
      object QText: TQRDBText
        Left = 52
        Top = 3
        Width = 53
        Height = 17
        Frame.Color = clBlack
        Frame.DrawTop = False
        Frame.DrawBottom = False
        Frame.DrawLeft = False
        Frame.DrawRight = False
        Size.Values = (
          44.979166666666670000
          137.583333333333300000
          7.937500000000000000
          140.229166666666700000)
        Alignment = taLeftJustify
        AlignToBand = False
        AutoSize = True
        AutoStretch = False
        Color = clWhite
        DataSet = FMain.DBTreePrintDataSet
        DataField = 'Pr_name'
        Transparent = False
        WordWrap = True
        FontSize = 10
      end
      object Image: TQRImage
        Left = 31
        Top = 4
        Width = 16
        Height = 16
        Frame.Color = clBlack
        Frame.DrawTop = False
        Frame.DrawBottom = False
        Frame.DrawLeft = False
        Frame.DrawRight = False
        Size.Values = (
          42.333333333333330000
          82.020833333333340000
          10.583333333333330000
          42.333333333333330000)
        AutoSize = True
      end
      object ImageRect: TQRImage
        Left = 13
        Top = 7
        Width = 9
        Height = 9
        Frame.Color = clBlack
        Frame.DrawTop = False
        Frame.DrawBottom = False
        Frame.DrawLeft = False
        Frame.DrawRight = False
        Size.Values = (
          23.812500000000000000
          34.395833333333330000
          18.520833333333330000
          23.812500000000000000)
        Picture.Data = {
          07544269746D6170BE000000424DBE0000000000000076000000280000000900
          0000090000000100040000000000480000000000000000000000100000001000
          000000000000000080000080000000808000800000008000800080800000C0C0
          C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
          FF0088888888800000008FFFFFFF800000008FFFFFFF800000008FFFFFFF8000
          00008F00000F800000008FFFFFFF800000008FFFFFFF800000008FFFFFFF8000
          00008888888880000000}
      end
    end
  end
end
