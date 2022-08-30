object dxfmStdPreview: TdxfmStdPreview
  Left = 205
  Top = 190
  ActiveControl = Preview
  Caption = 'Preview'
  ClientHeight = 446
  ClientWidth = 954
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000FFFFFFFFFF000000FFFFFFF000070000FFF
    FFF0788707000FFFFF0788E770000FFFFF08888780000FFFFF08E88780000FFF
    FF07EE8770000FFFFFF0788700000FFFFFFF000000000FFFFFFFFFF000000FFF
    FFFF000000000FFFFFFF080000000FFFFFFF000000000000000000000000FFFF
    0000000C00000008000000010000000300000003000000030000000300000003
    000000070000000F0000000F0000000F0000001F0000003F0000007F0000}
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 954
    Height = 34
    AutoSize = True
    BorderWidth = 2
    ButtonWidth = 28
    EdgeBorders = [ebTop, ebBottom]
    Images = ilToolBarSmall
    Indent = 4
    ParentShowHint = False
    PopupMenu = pmToolBar
    ShowHint = True
    TabOrder = 0
    Wrapable = False
    object tbReportDesigner: TToolButton
      Left = 4
      Top = 0
      Hint = 'Ctrl+D'
      Caption = 'Design'
      ImageIndex = 0
      OnClick = DesignClick
    end
    object tbFileLoad: TToolButton
      Left = 32
      Top = 0
      Hint = 'Ctrl+O'
      Caption = 'tbFileLoad'
      ImageIndex = 23
      OnClick = FileLoadClick
    end
    object tbFileUnload: TToolButton
      Left = 60
      Top = 0
      Caption = 'tbFileUnload'
      ImageIndex = 24
      OnClick = FileCloseClick
    end
    object tbFileSave: TToolButton
      Left = 88
      Top = 0
      Hint = 'Ctrl+S'
      Caption = 'tbFileSave'
      ImageIndex = 19
      OnClick = FileSaveClick
    end
    object tbSeparator1: TToolButton
      Left = 116
      Top = 0
      Width = 8
      Caption = 'tbSeparator1'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbPrint: TToolButton
      Left = 124
      Top = 0
      Caption = 'Print'
      ImageIndex = 1
      OnClick = PrintClick
    end
    object tbPrintDialog: TToolButton
      Tag = 1
      Left = 152
      Top = 0
      Hint = 'Ctrl+P'
      Caption = 'PrintDialog'
      ImageIndex = 2
      OnClick = PrintClick
    end
    object tbExportToPdf: TToolButton
      Left = 180
      Top = 0
      Caption = 'Export To Pdf'
      ImageIndex = 32
      MenuItem = miExportToPdf
    end
    object tbPageSetup: TToolButton
      Tag = 2
      Left = 208
      Top = 0
      Caption = 'PageSetup'
      DropdownMenu = pmPrintStyles
      ImageIndex = 3
      Style = tbsDropDown
      OnClick = PageSetupClick
    end
    object tbSeparator3: TToolButton
      Left = 251
      Top = 0
      Width = 8
      Caption = 'tbSeparator3'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbViewExplorer: TToolButton
      Left = 259
      Top = 0
      Caption = 'tbViewExplorer'
      ImageIndex = 30
      Style = tbsCheck
      OnClick = ViewExplorerClick
    end
    object tbViewThumbnails: TToolButton
      Left = 287
      Top = 0
      Caption = 'tbViewThumbnails'
      ImageIndex = 31
      Style = tbsCheck
      OnClick = ViewThumbnailsClick
    end
    object tbSeparator2: TToolButton
      Left = 315
      Top = 0
      Width = 8
      Caption = 'tbSeparator2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbShrinkToPageWidth: TToolButton
      Left = 323
      Top = 0
      AllowAllUp = True
      Caption = 'tbShrinkToPageWidth'
      Grouped = True
      ImageIndex = 12
      OnClick = ShrinkToPageWidthClick
    end
    object tbPageBackground: TToolButton
      Left = 351
      Top = 0
      Hint = 'Ctrl+K'
      Caption = 'PageBackground'
      ImageIndex = 4
      OnClick = PageBackgroundClick
    end
    object ToolButton5: TToolButton
      Left = 379
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 19
      Style = tbsSeparator
    end
    object tbPercent100: TToolButton
      Left = 387
      Top = 0
      Hint = 'Ctrl+\'
      Caption = '100 percent'
      ImageIndex = 5
      OnClick = ZoomClick
    end
    object tbPageWidth: TToolButton
      Tag = 1
      Left = 415
      Top = 0
      Hint = 'Ctrl+0'
      Caption = 'Page Width'
      ImageIndex = 6
      OnClick = ZoomClick
    end
    object tbOnePage: TToolButton
      Tag = 2
      Left = 443
      Top = 0
      Hint = 'Ctrl+1'
      Caption = 'Whole Page'
      ImageIndex = 7
      OnClick = ZoomClick
    end
    object tbTwoPage: TToolButton
      Tag = 3
      Left = 471
      Top = 0
      Hint = 'Ctrl+2'
      Caption = 'Two Pages'
      ImageIndex = 8
      OnClick = ZoomClick
    end
    object tbFourPage: TToolButton
      Tag = 4
      Left = 499
      Top = 0
      Hint = 'Ctrl+4'
      Caption = 'Four Pages'
      ImageIndex = 9
      OnClick = ZoomClick
    end
    object tbMultiplePages: TToolButton
      Left = 527
      Top = 0
      Caption = 'tbMultiplePages'
      ImageIndex = 10
      OnClick = tbMultiplePagesClick
    end
    object tbWidenToSourceWidth: TToolButton
      Tag = 5
      Left = 555
      Top = 0
      Hint = 'Ctrl+W'
      Caption = 'Source Width'
      ImageIndex = 11
      OnClick = ZoomClick
    end
    object tbSeparator4: TToolButton
      Left = 583
      Top = 0
      Width = 8
      Caption = 'tbSeparator4'
      ImageIndex = 18
      Style = tbsSeparator
    end
    object pnlPredefinedZoom: TcxGroupBox
      Left = 591
      Top = 0
      PanelStyle.Active = True
      ParentBackground = False
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 22
      Width = 140
      object cbxPredefinedZoom: TcxComboBox
        Left = 0
        Top = 0
        Align = alLeft
        Properties.OnCloseUp = cbxPredefinedZoomPropertiesCloseUp
        Properties.OnDrawItem = cbxPredefinedZoomPropertiesDrawItem
        TabOrder = 0
        OnClick = cbxPredefinedZoomClick
        OnExit = cbxPredefinedZoomClick
        OnKeyDown = cbxPredefinedZoomKeyDown
        Width = 140
      end
    end
    object tbSeparator5: TToolButton
      Left = 731
      Top = 0
      Width = 8
      Caption = 'tbSeparator5'
      ImageIndex = 19
      Style = tbsSeparator
    end
    object tbGotoFirstPage: TToolButton
      Left = 739
      Top = 0
      Caption = 'First Page'
      ImageIndex = 13
      OnClick = GoToPageClick
    end
    object tbGotoPrevPage: TToolButton
      Tag = 1
      Left = 767
      Top = 0
      Caption = 'Prev Page'
      ImageIndex = 14
      OnClick = GoToPageClick
    end
    object ToolButton2: TToolButton
      Left = 795
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 20
      Style = tbsSeparator
    end
    object pnlActivePage: TcxGroupBox
      Left = 803
      Top = 0
      PanelStyle.Active = True
      ParentBackground = False
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
      TabOrder = 1
      Height = 22
      Width = 63
      object seActivePage: TcxSpinEdit
        Left = 0
        Top = 0
        Align = alLeft
        Properties.AssignedValues.MinValue = True
        TabOrder = 0
        Value = 1
        OnExit = seActivePageExit
        OnKeyPress = seKeyPress
        Width = 63
      end
    end
    object ToolButton1: TToolButton
      Left = 866
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 19
      Style = tbsSeparator
    end
    object tbGotoNextPage: TToolButton
      Tag = 2
      Left = 874
      Top = 0
      Caption = 'Next Page'
      ImageIndex = 15
      OnClick = GoToPageClick
    end
    object tbGotoLastPage: TToolButton
      Tag = 3
      Left = 902
      Top = 0
      Caption = 'Last Page'
      ImageIndex = 16
      OnClick = GoToPageClick
    end
    object tbSeparator8: TToolButton
      Left = 930
      Top = 0
      Width = 8
      Caption = 'tbSeparator8'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbHelp: TToolButton
      Left = 938
      Top = 0
      Hint = 'F1'
      Caption = 'Help'
      ImageIndex = 17
      OnClick = HelpClick
    end
    object tbClose: TToolButton
      Left = 966
      Top = 0
      Caption = 'Close'
      ImageIndex = 18
      OnClick = CloseClick
    end
  end
  object Preview: TdxPSPreviewWindow
    Left = 0
    Top = 34
    Width = 954
    Height = 412
    Align = alClient
    BorderStyle = cxcbsNone
    PreviewPopupMenu = pmPreview
    OnAddExplorerCommand = PreviewAddExplorerCommand
    OnInitContent = PreviewInitContent
    OnLoadProperties = PreviewLoadProperties
    OnSaveProperties = PreviewSaveProperties
    OnStyleListChanged = PreviewStyleListChanged
    OnUpdateControls = PreviewUpdateControls
    OnUpdateExplorerCommands = PreviewUpdateExplorerCommands
    OnZoomFactorChanged = PreviewZoomFactorChanged
    OnZoomModeChanged = PreviewZoomModeChanged
  end
  object pmToolBar: TPopupMenu
    OnPopup = pmToolBarPopup
    Left = 47
    Top = 56
    object pmiFlatBtns: TMenuItem
      Caption = '&Flat Buttons'
      Checked = True
      GroupIndex = 2
      OnClick = pmiFlatBtnsClick
    end
    object pmiLargeBtns: TMenuItem
      Caption = '&Large Buttons'
      Checked = True
      GroupIndex = 2
      OnClick = pmiLargeBtnsClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 19
    Top = 56
    object miFile: TMenuItem
      Caption = '&File'
      object miFileDesign: TMenuItem
        Caption = '&Design...'
        ShortCut = 16452
        OnClick = DesignClick
      end
      object miFileRebuild: TMenuItem
        Caption = 'Rebuild'
        ShortCut = 16500
        OnClick = miFileRebuildClick
      end
      object miLine32: TMenuItem
        Caption = '-'
      end
      object miFileLoad: TMenuItem
        Caption = 'Load...'
        ShortCut = 16463
        OnClick = FileLoadClick
      end
      object miFileClose: TMenuItem
        Caption = 'Unload'
        ShortCut = 16499
        OnClick = FileCloseClick
      end
      object miLine30: TMenuItem
        Caption = '-'
      end
      object miFileSave: TMenuItem
        Caption = '&Save...'
        ShortCut = 16467
        OnClick = FileSaveClick
      end
      object miLine21: TMenuItem
        Caption = '-'
      end
      object miFilePrint: TMenuItem
        Tag = 1
        Caption = '&Print...'
        ShortCut = 16464
        OnClick = PrintClick
      end
      object miFilePageSetup: TMenuItem
        Tag = 2
        Caption = 'Page Set&up...'
        OnClick = PageSetupClick
      end
      object miFilePrintStyles: TMenuItem
        Caption = 'Print Styles'
      end
      object miLine2: TMenuItem
        Caption = '-'
      end
      object miExportToPdf: TMenuItem
        Caption = 'Export To Pdf'
        ImageIndex = 32
        OnClick = miExportToPdfClick
      end
      object miLine3: TMenuItem
        Caption = '-'
      end
      object miFilePreferences: TMenuItem
        Caption = 'Pre&ferences...'
        OnClick = OptionsClick
      end
      object miLine1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Caption = '&Close'
        OnClick = CloseClick
      end
    end
    object miExplorer: TMenuItem
      Caption = 'E&xplorer'
      object miExplorerCreateNewFolder: TMenuItem
        Caption = 'New &Folder'
        ShortCut = 32821
        OnClick = ExplorerCreateNewFolderClick
      end
      object miLine31: TMenuItem
        Caption = '-'
      end
      object miExplorerDelete: TMenuItem
        Caption = '&Delete...'
        ShortCut = 46
        OnClick = ExplorerDeleteClick
      end
      object miExplorerRename: TMenuItem
        Caption = '&Rename'
        ShortCut = 113
        OnClick = ExplorerRenameClick
      end
      object miLine39: TMenuItem
        Caption = '-'
      end
      object miExplorerProperties: TMenuItem
        Caption = 'P&roperties...'
        ShortCut = 32889
        OnClick = ExplorerPropertiesClick
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      Visible = False
      object miEditFind: TMenuItem
        Caption = '&Find ...'
        ShortCut = 16454
      end
      object miEditFindNext: TMenuItem
        Caption = 'Find Ne&xt'
        ShortCut = 114
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miEditReplace: TMenuItem
        Caption = '&Replace ...'
        ShortCut = 16466
      end
    end
    object miView: TMenuItem
      Caption = '&View'
      object miViewMargins: TMenuItem
        Caption = '&Margins'
        Checked = True
        ShortCut = 16461
        OnClick = miViewMarginsClick
      end
      object miLine4: TMenuItem
        Caption = '-'
      end
      object miViewFlatTBtns: TMenuItem
        Caption = '&Flat toolbar buttons'
        Checked = True
        OnClick = pmiFlatBtnsClick
      end
      object miViewLargeTBtns: TMenuItem
        Caption = '&Large toolbar buttons'
        Checked = True
        OnClick = pmiLargeBtnsClick
      end
      object miLine5: TMenuItem
        Caption = '-'
      end
      object miViewToolBar: TMenuItem
        Caption = '&Toolbar'
        Checked = True
        Visible = False
      end
      object miViewMarginBar: TMenuItem
        Caption = '&MarginBar'
        Checked = True
        OnClick = miViewMarginBarClick
      end
      object miViewStatusBar: TMenuItem
        Caption = '&StatusBar'
        Checked = True
        OnClick = miViewStatusBarClick
      end
      object miViewExplorer: TMenuItem
        Caption = 'E&xplorer'
        ShortCut = 16472
        OnClick = ViewExplorerClick
      end
      object miViewThumbnails: TMenuItem
        Caption = 'Th&umbnails'
        ShortCut = 16469
        OnClick = ViewThumbnailsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miViewZoom: TMenuItem
        Caption = '&Zoom'
        object miZoomPercent100: TMenuItem
          Caption = '&Percent 100'
          ShortCut = 16604
          OnClick = ZoomClick
        end
        object miLine6: TMenuItem
          Caption = '-'
        end
        object miZoomPageWidth: TMenuItem
          Tag = 1
          Caption = 'Page &Width'
          ShortCut = 16432
          OnClick = ZoomClick
        end
        object miZoomWholePage: TMenuItem
          Tag = 2
          Caption = 'W&hole Page'
          ShortCut = 16433
          OnClick = ZoomClick
        end
        object miZoomTwoPages: TMenuItem
          Tag = 3
          Caption = '&Two Pages'
          ShortCut = 16434
          OnClick = ZoomClick
        end
        object miZoomFourPages: TMenuItem
          Tag = 4
          Caption = '&Four Pages '
          ShortCut = 16436
          OnClick = ZoomClick
        end
        object miLine7: TMenuItem
          Caption = '-'
        end
        object miZoomWidenToSourceWidth: TMenuItem
          Tag = 5
          Caption = '&Widen to source width'
          ShortCut = 16471
          OnClick = ZoomClick
        end
        object miLine20: TMenuItem
          Caption = '-'
        end
        object miZoomSetup: TMenuItem
          Caption = '&Setup ...'
          OnClick = miZoomSetupClick
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miViewPageHeaders: TMenuItem
        Caption = 'Page &Headers'
        Checked = True
        OnClick = miViewPageHeadersClick
      end
      object miViewPageFooters: TMenuItem
        Caption = '&Page Footers'
        Checked = True
        OnClick = miViewPageFootersClick
      end
    end
    object miFormat: TMenuItem
      Caption = '&Format'
      object miFormatAutoText: TMenuItem
        Caption = '&AutoText...'
        OnClick = miFormatAutoTextClick
      end
      object miLine14: TMenuItem
        Caption = '-'
      end
      object miFormatTitle: TMenuItem
        Caption = 'Title...'
        OnClick = miFormatTitleClick
      end
      object miFormatFootnotes: TMenuItem
        Caption = 'Footnotes...'
        ImageIndex = 33
        OnClick = miFormatFootnotesClick
      end
      object N7: TMenuItem
        Caption = '-'
        Hint = 'miLine38'
      end
      object miFormatDateTime: TMenuItem
        Caption = 'Date And &Time ...'
        OnClick = miFormatDateTimeClick
      end
      object miFormatPageNumbering: TMenuItem
        Caption = 'Page &Numbering ...'
        OnClick = miFormatPageNumberingClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miFormatShrinkToPageWidth: TMenuItem
        Caption = '&Shrink To Page'
        Checked = True
        OnClick = ShrinkToPageWidthClick
      end
      object miLine13: TMenuItem
        Caption = '-'
      end
      object miFormatPageBackground: TMenuItem
        Caption = 'Page bac&kground ...'
        ShortCut = 16459
        OnClick = PageBackgroundClick
      end
    end
    object miGoToPage: TMenuItem
      Caption = '&Go'
      object miGoToFirstPage: TMenuItem
        Caption = '&First Page'
        ShortCut = 36
        OnClick = GoToPageClick
      end
      object miGoToPrevPage: TMenuItem
        Tag = 1
        Caption = '&Previous Page'
        ShortCut = 33
        OnClick = GoToPageClick
      end
      object miLine8: TMenuItem
        Caption = '-'
      end
      object miGoToNextPage: TMenuItem
        Tag = 2
        Caption = '&Next Page'
        ShortCut = 34
        OnClick = GoToPageClick
      end
      object miGoToLastPage: TMenuItem
        Tag = 3
        Caption = '&Last Page'
        ShortCut = 35
        OnClick = GoToPageClick
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object miHelpTopics: TMenuItem
        Caption = '&Help Topics ...'
        OnClick = HelpClick
      end
      object N3: TMenuItem
        Caption = '-'
        Visible = False
      end
      object miHelpAbout: TMenuItem
        Caption = '&About ..'
        Visible = False
      end
    end
  end
  object pmPreview: TPopupMenu
    OnPopup = pmPreviewPopup
    Left = 75
    Top = 56
    object pmiReportDesign: TMenuItem
      Caption = '&Design ...'
      Default = True
      ShortCut = 16452
      OnClick = DesignClick
    end
    object miLine11: TMenuItem
      Caption = '-'
    end
    object pmiPageSetup: TMenuItem
      Caption = 'Page Set&up ...'
      OnClick = PageSetupClick
    end
    object pmiFilePrintStyles: TMenuItem
      Caption = 'Print Styles'
    end
    object pmiReportShrinkToPageWidth: TMenuItem
      Caption = '&Shrink To Page'
      Checked = True
      OnClick = ShrinkToPageWidthClick
    end
    object miLine10: TMenuItem
      Caption = '-'
    end
    object pmiZoom: TMenuItem
      Caption = '&Zoom '
      object pmiZoomPercent100: TMenuItem
        Caption = '&Percent 100'
        ShortCut = 16604
        OnClick = ZoomClick
      end
      object miLine12: TMenuItem
        Caption = '-'
      end
      object pmiZoomPageWidth: TMenuItem
        Tag = 1
        Caption = 'Page &Width'
        ShortCut = 16432
        OnClick = ZoomClick
      end
      object pmiZoomWholePage: TMenuItem
        Tag = 2
        Caption = 'W&hole Page'
        ShortCut = 16433
        OnClick = ZoomClick
      end
      object pmiZoomTwoPages: TMenuItem
        Tag = 3
        Caption = '&Two Pages'
        ShortCut = 16434
        OnClick = ZoomClick
      end
      object pmiZoomFourPages: TMenuItem
        Tag = 4
        Caption = '&Four Pages'
        ShortCut = 16436
        OnClick = ZoomClick
      end
      object miLine9: TMenuItem
        Caption = '-'
      end
      object pmiZoomWidenToSourceWidth: TMenuItem
        Tag = 5
        Caption = 'Widen to &source width'
        ShortCut = 16471
        OnClick = ZoomClick
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmiGoToFirstPage: TMenuItem
      Caption = 'First Page'
      ShortCut = 36
      OnClick = GoToPageClick
    end
    object pmiGoToPrevPage: TMenuItem
      Tag = 1
      Caption = 'Previous Page'
      ShortCut = 33
      OnClick = GoToPageClick
    end
    object pmiGoToNextPage: TMenuItem
      Tag = 2
      Caption = 'Next Page'
      ShortCut = 34
      OnClick = GoToPageClick
    end
    object pmiGoToLastPage: TMenuItem
      Tag = 3
      Caption = 'Last Page'
      ShortCut = 35
      OnClick = GoToPageClick
    end
  end
  object pmPrintStyles: TPopupMenu
    OnPopup = pmPrintStylesPopup
    Left = 103
    Top = 56
  end
  object pmDesigners: TPopupMenu
    Left = 131
    Top = 56
  end
  object pmExplorer: TPopupMenu
    Images = ilToolBarSmall
    OnPopup = pmExplorerPopup
    Left = 159
    Top = 56
    object pmiExplorerLoadData: TMenuItem
      Caption = 'Load'
      Default = True
      ShortCut = 16397
      OnClick = FileLoadClick
    end
    object pmiExplorerUnloadData: TMenuItem
      Caption = 'Unload'
      ShortCut = 16499
      OnClick = FileCloseClick
    end
    object miLine33: TMenuItem
      Caption = '-'
    end
    object pmiExplorerCreateFolder: TMenuItem
      Caption = 'New Folder'
      ShortCut = 32821
      OnClick = ExplorerCreateNewFolderClick
    end
    object miLine34: TMenuItem
      Caption = '-'
    end
    object pmiExplorerDelete: TMenuItem
      Caption = '&Delete...'
      ShortCut = 46
      OnClick = ExplorerDeleteClick
    end
    object pmiExplorerRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = ExplorerRenameClick
    end
    object miLine40: TMenuItem
      Caption = '-'
    end
    object pmiExplorerProperties: TMenuItem
      Caption = '&Properties...'
      ShortCut = 32781
      OnClick = ExplorerPropertiesClick
    end
  end
  object pmThumbnails: TPopupMenu
    OnPopup = pmThumbnailsPopup
    Left = 187
    Top = 56
    object pmiSmallThumbnails: TMenuItem
      Caption = '&Small Thumbnails'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = pmiThumbnailsSizeClick
    end
    object pmiLargeThumbnails: TMenuItem
      Tag = 1
      Caption = '&Large Thumbnails'
      GroupIndex = 1
      RadioItem = True
      OnClick = pmiThumbnailsSizeClick
    end
  end
  object ilToolBarSmall: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 7012401
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00848484008484840084848400FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000008484
          8400FF00FF008484840084848400FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0084848400FF00FF00FF00FF000000000000000000000000000000
          0000FF00FF00000000000000000084848400FF00FF00FF00FF00FF00FF00FF00
          FF00848484000000000000000000FF00FF00C6C6C600C6C6C600C6C6C6000000
          00000000000000000000000000000000000084848400FF00FF00FF00FF008484
          840000000000C6C6C600848484000000000000000000C6C6C600C6C6C6000000
          000084848400C6C6C600000000000000000084848400FF00FF00FF00FF00FF00
          FF0084848400C6C6C60084848400C6C6C600C6C6C6008484840000000000C6C6
          C60084848400848484008484840084848400FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000C6C6C600C6C6C60000000000FFFFFF00FFFFFF000000
          0000C6C6C6000000000000000000000000000000000084848400848484000000
          0000C6C6C60000000000C6C6C60000000000C6C6C600C6C6C600C6C6C600FFFF
          FF008484840000000000C6C6C600C6C6C6000000000084848400848484000000
          0000C6C6C600C6C6C600FFFFFF00000000008484840000000000C6C6C600FFFF
          FF0084848400C6C6C600C6C6C600C6C6C6000000000084848400848484000000
          00000000000000000000FFFFFF00000000008484840084848400C6C6C6000000
          0000C6C6C600000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000C6C6C600FFFFFF00000000000000000000000000C6C6
          C600C6C6C60000000000000000008484840000000000FF00FF00FF00FF00FF00
          FF0084848400C6C6C60084848400C6C6C600FFFFFF00FFFFFF00C6C6C600C6C6
          C6008484840084848400848484000000000084848400FF00FF00FF00FF008484
          840000000000C6C6C600848484000000000000000000C6C6C600000000000000
          0000C6C6C600C6C6C6000000000084848400FF00FF00FF00FF00FF00FF00FF00
          FF00848484000000000084848400FF00FF00C6C6C600C6C6C600C6C6C6000000
          0000848484000000000084848400FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0084848400FF00FF00FF00FF000000000000000000000000008484
          8400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00848484008484840084848400FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF000000
          0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
          C600C6C6C60000000000C6C6C60000000000FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000C6C6C60000000000FF00FF0000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000FFFF0000FFFF0000FF
          FF00C6C6C600C6C6C600000000000000000000000000FF00FF0000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60084848400848484008484
          8400C6C6C600C6C6C60000000000C6C6C60000000000FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000C6C6C600C6C6C6000000000000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
          C600C6C6C60000000000C6C6C60000000000C6C6C60000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000C6C6C60000000000C6C6C6000000000000000000FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000C6C6C60000000000C6C6C60000000000FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00000000000000000000000000000000000000
          0000FFFFFF0000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFFFF000000000000000000000000000000
          000000000000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF000000
          0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
          C600C6C6C60000000000C6C6C60000000000FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000C6C6C60000000000FF00FF0000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000FFFF0000FFFF0000FF
          FF00C6C6C600C6C6C600000000000000000000000000FF00FF0000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60084848400848484008484
          8400C6C6C600C6C6C60000000000C6C6C60000000000FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000C6C6C600C6C6C6000000000000000000C6C6
          C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
          C600C6C6C60000000000C6C6C60000000000C6C6C60000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000C6C6C60000000000C6C6C6000000000000000000FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000C6C6C60000000000C6C6C60084848400FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0084840000FFFF0000FFFF0000FFFF000000000000FF00FF008484
          8400000000000000000000000000000000000000000000000000000000000000
          000000000000FFFF000084840000000000000000000084848400FF00FF000000
          0000FFFF000084840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF008484
          8400000000000000000000000000000000000000000000000000000000000000
          000000000000FFFF000084840000000000000000000084848400FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF008484840084840000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          0000000000008484840000000000000000000000000084848400}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000FFFFFF008484840000000000FFFFFF000000
          00000000000000000000000000000000000000000000FF00FF00000000008484
          8400FFFFFF00FFFFFF00FFFFFF00848484008484840000000000FFFFFF00FFFF
          FF008484840084848400848484008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF008484840084848400848484008484840000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF008484840000000000FF00FF00000000008484
          8400FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF0084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
          FF00FFFFFF0000FFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0084848400FFFFFF000000000000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
          FF00FFFFFF00FFFFFF00FFFFFF0000FFFF0000000000FF00FF00FF00FF00FF00
          FF0084848400000000008484840000000000FFFFFF0000FFFF00FFFFFF00FFFF
          FF00FFFFFF0000FFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF000000000084848400848484008484840000000000FFFFFF00FFFFFF0000FF
          FF0084848400FFFFFF00FFFFFF0000FFFF0000000000FF00FF00FF00FF000000
          0000C6C6C600C6C6C600C6C6C600848484008484840000000000FFFFFF00FFFF
          FF008400000084848400FFFFFF00FFFFFF0000000000FF00FF0000000000FFFF
          FF00C6C6C600C6C6C600C6C6C600C6C6C60084848400848484000000000000FF
          FF008400000084000000FFFFFF0000FFFF0000000000FF00FF00C6C6C600FFFF
          FF00FFFFFF00C6C6C600C6C6C600C6C6C600C6C6C60084848400848484008400
          00008400000084000000FFFFFF00FFFFFF0000000000FF00FF0000000000C6C6
          C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600C6C6C600848484008400
          00008400000084000000FFFFFF0000FFFF0000000000FF00FF00FF00FF000000
          0000C6C6C600FFFFFF00000000008400000000000000C6C6C600840000008400
          00008400000084848400FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000C6C6C600FFFFFF0084000000C6C6C60084848400840000008400
          000084848400FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF008400000000000000C6C6C60084000000848484000000000084848400FFFF
          FF00FFFFFF0000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF0084000000C6C6C60000000000840000000000000084848400FFFFFF0000FF
          FF00FFFFFF0084848400C6C6C60000000000FF00FF00FF00FF00FF00FF00FF00
          FF0084000000C6C6C600C6C6C600840000008484840000FFFF00FFFFFF00FFFF
          FF00FFFFFF008484840000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00848484008400000084000000848484008484840084848400848484008484
          84008484840084848400FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
          000000000000FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
          FF0000000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FF00000000000000FFFFFF0000000000000000000000
          0000FF000000FF00000000000000FFFFFF0000000000FF000000FF0000000000
          0000FFFFFF0000000000FF000000FF000000000000000000000000000000FFFF
          FF0000000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FF00000000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
          000000000000FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000848484000000000000000000000000000000000000000000000000000000
          0000000000000000000084848400848400008484840000000000000000008484
          84008484000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000848484008484000000000000000000008484
          00008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000848400008484840000000000000000008484
          84008484000000000000FFFFFF00840000008400000084000000840000008400
          000084000000FFFFFF0000000000848484008484000000000000000000008484
          00008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000848400008484840000000000000000008484
          84008484000000000000FFFFFF0084000000840000008400000084000000FFFF
          FF00FFFFFF00FFFFFF0000000000848484008484000000000000000000008484
          00008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000848400008484840000000000000000008484
          84008484000000000000FFFFFF00840000008400000084000000840000008400
          000084000000FFFFFF0000000000848484008484000000000000000000008484
          00008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000848400008484840000000000000000008484
          8400848400000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000848484008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000000000000000000000000000000000000000000084840000848484000000
          0000000000000000000000000000000000008484840000000000000000008484
          840000000000FFFFFF00FFFFFF00FFFFFF000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          0000FFFFFF000000000000000000000000008484840000000000000000008484
          8400000000000000000000000000FFFFFF000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          00000000000000000000FFFFFF00000000008484840000000000000000008484
          840000000000FFFFFF0000000000000000000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          0000FFFFFF0000000000FFFFFF00000000008484840000000000000000008484
          840000000000FFFFFF0000000000FFFFFF000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000008484
          8400000000000000000000000000000000000000000084848400848400000000
          0000000000000000000000000000000000008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000000000000000000000000000000000000000000084840000848484000000
          0000000000000000000000000000000000008484840000000000000000008484
          840000000000FFFFFF00FFFFFF00FFFFFF000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          0000000000000000000000000000FFFFFF000000000084840000848484000000
          00000000000000000000FFFFFF00000000008484840000000000000000008484
          840000000000FFFFFF00FFFFFF00FFFFFF000000000084848400848400000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484000000000000000000008484
          0000000000000000000000000000000000000000000084840000848484000000
          0000000000000000000000000000000000008484840000000000000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000000000008484
          8400000000000000000000000000000000000000000084848400848400000000
          0000000000000000000000000000000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000008484
          8400000000000000000000000000FFFFFF000000000084848400848400000000
          00000000000000000000FFFFFF00000000008484000000000000000000008484
          000000000000FFFFFF00FFFFFF00FFFFFF000000000084840000848484000000
          0000FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000008484
          8400000000000000000000000000000000000000000084848400848400000000
          0000000000000000000000000000000000008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000000000008484
          8400000000000000000000000000848484000000000000000000000000008484
          0000000000000000000000000000848484008484000000000000000000008484
          000000000000FFFFFF00000000008484000000000000FFFFFF00000000008484
          840000000000FFFFFF0000000000848400008484840000000000000000008484
          840000000000FFFFFF00000000008484840000000000FFFFFF00000000008484
          000000000000FFFFFF0000000000848484008484000000000000000000008484
          0000000000000000000000000000848400000000000000000000000000008484
          0000000000000000000000000000848400008484840000000000000000008484
          8400848400008484840084840000848484008484000084848400848400008484
          8400848400008484840084840000848484008484000000000000000000008484
          0000000000000000000000000000848400000000000000000000000000008484
          0000000000000000000000000000848400008484840000000000000000008484
          840000000000FFFFFF00000000008484840000000000FFFFFF00000000008484
          840000000000FFFFFF0000000000848484008484000000000000000000008484
          000000000000FFFFFF00000000008484000000000000FFFFFF00000000008484
          000000000000FFFFFF0000000000848400008484840000000000000000008484
          8400000000000000000000000000848484000000000000000000000000008484
          8400000000000000000000000000848484008484000000000000000000008484
          0000848484008484000084848400848400008484840084840000848484008484
          0000848484008484000084848400848400008484840000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000FF00FF000000
          00000000000000000000000000000000000000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF000000000000000000FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000848484000000
          0000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FF000000000000008484840000000000FF00
          0000FF00000000000000FFFFFF00FF00000000000000FF00FF0000000000FFFF
          FF00FF000000FFFFFF0000000000FF000000FF00000000000000848484000000
          0000FF000000FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00FF000000000000008484840000000000FFFF
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000FF00FF000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF00FF00
          0000FF000000FF000000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000FFFFFF00FF00
          0000FF000000FF000000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF0000000000C6C6C600C6C6C600C6C6C600C6C6C60000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00000000000000
          000000000000C6C6C600FF000000FF000000FF00000000000000FFFFFF00FF00
          0000FF000000FF000000FFFFFF0000000000FF00FF00FF00FF00000000008484
          000000000000C6C6C600C6C6C600C6C6C600C6C6C60000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00000000008484
          000000000000C6C6C600FF000000FF000000FF00000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00000000008484
          000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000008484
          000000000000C6C6C600FF000000FF000000FF000000C6C6C60000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0084000000FF00FF00000000008484
          000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000FF00
          FF00FF00FF00FF00FF00FF00FF00840000008400000084000000000000008484
          000000000000000000000000000000000000000000000000000000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0084000000FF00FF00000000008484
          00008484000084840000848400008484000000000000FF00FF00FF00FF00FF00
          FF0084000000FF00FF00FF00FF00FF00FF0084000000FF00FF00000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00840000008400000084000000FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFF000000000000FF00FF0000000000FFFF00000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFF0000FFFF00000000000084848400FFFF0000FFFF00000000
          0000000000000000000000000000000000000000000000000000FF00FF000000
          0000FFFF0000FFFF0000FFFF000000000000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF
          0000FFFF0000FFFF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FF00FF000000
          0000FFFF0000FFFF0000FFFF000000000000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00
          FF0000000000FFFF0000FFFF00000000000084848400FFFF0000FFFF00000000
          0000000000000000000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF0000000000FFFF000000000000FF00FF0000000000FFFF00000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FFFF000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFF0000FFFF000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFF0000FFFF000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FFFF000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000000000000000000000000000000000000000000000000000FFFF
          0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000000000000000000000000000000000000000000000000000FFFF
          0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
          FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00
          FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF000000000000FF00
          FF0000000000FFFF000000000000FF00FF00FF00FF00FF00FF00000000000000
          00000000000000000000000000000000000000000000FFFF0000FFFF00008484
          840000000000FFFF0000FFFF000000000000FF00FF00FF00FF0000000000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          000000000000FFFF0000FFFF0000FFFF000000000000FF00FF0000000000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000FFFF000000000000FFFF0000FFFF0000FFFF00000000000000000000FFFF
          0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          000000000000FFFF0000FFFF0000FFFF000000000000FF00FF00000000000000
          00000000000000000000000000000000000000000000FFFF0000FFFF00008484
          840000000000FFFF0000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF000000000000FF00
          FF0000000000FFFF000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00
          FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
          FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF008484840084848400848484008484840084848400848484000000
          000000FFFF000000000084848400FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00000000000000000084848400FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFF
          FF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF008400000084848400FFFF
          FF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF008484840084000000C6C6
          C600FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00848484008400
          00008484840000FFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF008400000084848400FFFFFF00FFFFFF008400
          000084000000FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF0000FFFF00FFFFFF00840000008400000000FFFF00C6C6C6008400
          00008400000000FFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF00C6C6C6008400000084000000840000008400
          0000C6C6C600FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
          FF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
          0000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000008484840000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000848484008484840000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000008484840084848400848484000000000000000000000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000008484840084848400848484000000000000FFFF0000FFFF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000008484840084848400000000000000000000FFFF0000FFFF000000
          0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000008484840084848400848484000000000000FFFF0000FFFF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000008484840084848400848484000000000000FFFF0000FFFF008484
          8400000000000000000000000000848484008484840084848400FF00FF00FF00
          FF00000000008484840084848400848484000000000000FFFF0000FFFF000000
          0000000000000000000000000000000000000000000000000000FF00FF00FF00
          FF00000000008484840084848400848484000000000000FFFF0000FFFF008484
          8400000000000000000000000000848484008484840084848400FF00FF00FF00
          FF000000000084848400848484000000000000FFFF0000FFFF0000FFFF000000
          0000FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000848484000000000000FFFF0000FFFF0000FFFF0000FFFF000000
          0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF000000
          0000008080000080800000000000000000000000000000000000000000000000
          0000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          0000008080000080800000000000000000000000000000000000000000000000
          0000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          0000008080000080800000000000000000000000000000000000000000000000
          0000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          0000008080000080800000000000000000000000000000000000000000000000
          00000000000000000000000000000080800000000000FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          80000080800000808000008080000080800000000000FF00FF00FF00FF000000
          0000008080000080800000000000000000000000000000000000000000000000
          00000000000000000000008080000080800000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF000000
          00000080800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000C0C0C00000000000FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF008080800000FFFF00FFFF
          FF0000FFFF008080800000FFFF0000FFFF0000FFFF008080800000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF008080800000FF
          FF0000FFFF008080800000FFFF00FFFFFF008080800000FFFF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF008080
          800000FFFF0080808000FFFFFF008080800000FFFF00FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0080808000808080008080
          800080808000FFFFFF0080808000808080008080800080808000000000000000
          0000000000000000000000000000000000000000000000000000FFFFFF0000FF
          FF008080800000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF00FF000000
          000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF008080
          800000FFFF008080800000FFFF008080800000FFFF00FF00FF00FF00FF00FF00
          FF000000000000000000000000000000000000000000FF00FF008080800000FF
          FF00FF00FF0080808000FFFFFF00FF00FF008080800000FFFF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008080800000FFFF00FF00
          FF00FF00FF008080800080808000FF00FF00FF00FF0080808000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0080808000FFFFFF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
          000000000000FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF000000000000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF000000000000000000000000000000000000000000FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF00000000000000000000000000FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF0000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
          000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00000000000000000000000000FFFFFF00FF00FF00FF00FF00FF00
          FF000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000000000000000000FFFFFF00FF00FF000000
          000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000000000000000000FFFFFF00FF00FF000000
          0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000000000000000000000000000000000FFFFFF00FF00FF00FF00FF00FF00
          FF000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF000000000000000000FFFFFF00FF00FF00FF00FF00FF00FF000000
          000000000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000000000FFFFFF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000008080000080800000808000008080000080800000808000008080000080
          80000080800000000000FF00FF00FF00FF00FF00FF00FF00FF000000000000FF
          FF00000000000080800000808000008080000080800000808000008080000080
          8000008080000080800000000000FF00FF00FF00FF00FF00FF0000000000FFFF
          FF0000FFFF000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF000000000000FF
          FF00FFFFFF0000FFFF0000000000008080000080800000808000008080000080
          80000080800000808000008080000080800000000000FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
          FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
          FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000FF
          FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00
          FF00FF00FF00FF00FF0000000000FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF000000
          0000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000000000FF00FF00FF00FF00FF00FF008080
          8000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000080808000FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00
          FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0080000000FF00FF0080000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF008000000080000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00800000008000000080000000FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008000000080000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF008000000080000000800000008000
          00008000000080000000FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008000000080000000FFFF
          FF00FFFFFF00FFFFFF0080000000FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080000000FFFF
          FF00FFFFFF00FFFFFF0080000000FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF0080000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00800000008000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00800000008000000080000000800000008000
          000080000000FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00800000008000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080000000FFFF
          FF00FFFFFF0000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF008080800080808000808080008080800080808000808080008080
          80008080800080808000808080008080800080808000FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF00000000000000000000000000000000000000
          0000000000000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF000000000000000000FFFFFF00000000000000000000000000000000000000
          0000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF000000000000000000FFFFFF00000000000000000000000000000000000000
          0000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000C0C0C0000000
          0000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF0000000000C0C0C00000000000FFFFFF0000000000C0C0C00000000000C0C0
          C000000000000000000000000000FF00FF00800000008000000000000000FFFF
          FF00FFFFFF0000000000C0C0C00000000000C0C0C00000000000C0C0C0000000
          0000C0C0C000C0C0C000C0C0C000000000008000000080000000000000000000
          0000000000000000000000000000C0C0C00000000000C0C0C00000000000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C0008000000080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000C0C0C00000000000C0C0C000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C0008000000080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000C0C0C000000000008000000080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
          0000000000000000000000000000FF00FF008000000080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000FFFF00FFFF
          FF0000000000FFFFFF0000FFFF0000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000FFFF00FFFFFF0000FF
          FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF0000FFFF00FFFF
          FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000FFFFFF0000FF
          FF000000000000000000000000000000000000000000FF00FF00800000008000
          00008000000080000000800000008000000000000000FFFFFF0000FFFF00FFFF
          FF0000000000FFFFFF0000FFFF00FFFFFF0000000000FF00FF0080000000FFFF
          FF00C0C0C000FFFFFF00C0C0C000FFFFFF000000000000FFFF00FFFFFF0000FF
          FF000000000000FFFF00FFFFFF0000FFFF0000000000FF00FF0080000000C0C0
          C000FFFFFF0000008000FFFFFF0000008000000080000000000000FFFF00FFFF
          FF0000000000FFFFFF0000FFFF0000000000FF00FF00FF00FF0080000000FFFF
          FF00C0C0C00000008000C0C0C00000008000C0C0C000FFFFFF00000000000000
          0000000000000000000000000000FF00FF00FF00FF00FF00FF0080000000C0C0
          C0000000800000008000FFFFFF00C0C0C00000008000C0C0C000800000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FFFF
          FF00C0C0C00000008000C0C0C0000000800000008000FFFFFF00800000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000C0C0
          C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000800000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000008000
          0000800000008000000080000000800000008000000080000000800000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00
          000080000000FF000000FF000000FF00000080000000FF000000800000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000008000
          000080000000800000008000000080000000800000008000000080000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF0080000000FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00800000008000000080000000800000008000000080000000FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF0080000000FFFFFF0080000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00800000008000000080000000800000008000000080000000FFFF
          FF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF0080000000FFFFFF00FFFF
          FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF000000
          0000C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF0000000000C0C0
          C00000000000C0C0C0000000000000000000FF00FF0080000000000000000000
          00000000000000000000FFFFFF0000000000C0C0C00000000000C0C0C0000000
          0000C0C0C00000000000C0C0C000C0C0C0000000000080000000FF00FF000000
          0000FFFFFF0000000000FFFFFF00FFFFFF0000000000C0C0C00000000000C0C0
          C00000000000C0C0C000C0C0C000C0C0C000C0C0C00080000000FF00FF00FF00
          FF000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000C0C0C0000000
          0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00080000000FF00FF00FF00
          FF00FF00FF00000000000000000000000000000000000000000000000000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C0000000000080000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          000000000000000000000000000000000000FF00FF0080000000}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF000000FFFF000080000000FF00FF00FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FF000000FFFF0000800000000000000000000000808080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000FF000000FFFF00008000000000808000008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00FF00
          0000FFFF00008000000000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000FFFF00C0C0C000000000000000000000000000C0C0
          C0008000000000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF00C0C0C00000000000FFFF0000C0C0C000FFFF00000000
          0000C0C0C00000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000000000FFFF0000C0C0C000FFFF0000C0C0C000FFFF
          00000000000000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000000000C0C0C000FFFF0000C0C0C000FFFF0000C0C0
          C0000000000000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000000000FFFF0000C0C0C000FFFF0000C0C0C000FFFF
          00000000000000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF00C0C0C00000000000FFFF0000C0C0C000FFFF00000000
          0000C0C0C00000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF0000FFFF0000FFFF0000FFFF00C0C0C000000000000000000000000000C0C0
          C00000FFFF0000FFFF0000FFFF0000FFFF00008080000000000080808000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000080800000000000808080008080
          8000808080008080800080808000808080008080800080808000808080008080
          80008080800080808000808080008080800000000000FF00FF00FF00FF008080
          800000FFFF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF008080800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000FFFFFF0080808000FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FF00FF000000
          0000808080008080800080808000808080008080800080808000808080008080
          8000808080008080800080808000808080008080800000000000FF00FF000000
          0000808080000000000000000000000000008080800000000000000000000000
          0000808080000000000000000000000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          0000808080008080800080808000808080008080800080808000808080008080
          8000808080008080800080808000808080008080800000000000FF00FF000000
          0000808080000000000000000000000000008080800000000000000000000000
          0000808080000000000000000000000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          000080808000FFFFFF00FFFFFF000000000080808000FFFFFF00FFFFFF000000
          000080808000FFFFFF00FFFFFF00000000008080800000000000FF00FF000000
          0000808080008080800080808000808080008080800080808000808080008080
          8000808080008080800080808000808080008080800000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FF00FF000000
          0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
          0000FF000000FF000000FF0000000000000000FFFF0000000000FF00FF000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000000000000000000000
          000000000000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000000000000000000000
          000000000000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000000000000000000000
          000000000000FF00FF00FF00FF00000000000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000000000000000000000
          0000000000000000000000000000000000000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000808000008080000080
          8000008080000080800000808000008080000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008080000080800000000000000000000000
          0000000000000000000000000000008080000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00
          FF00FF00FF00FF00FF00000000000080800000000000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000808000000000001B1BC2001B1B
          C2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1B
          C2001B1BC2001B1BC2001B1BC2000000000000000000000000001B1BC200FFFF
          FF001B1BC2001B1BC2001B1BC200FFFFFF00FFFFFF001B1BC2001B1BC200FFFF
          FF001B1BC2001B1BC2001B1BC20000000000C0C0C000000000001B1BC200FFFF
          FF001B1BC2001B1BC2001B1BC200FFFFFF001B1BC200FFFFFF001B1BC200FFFF
          FF001B1BC2001B1BC2001B1BC2000000000000000000000000001B1BC200FFFF
          FF00FFFFFF001B1BC2001B1BC200FFFFFF001B1BC200FFFFFF001B1BC200FFFF
          FF00FFFFFF001B1BC2001B1BC200FF00FF00FF00FF00FF00FF001B1BC200FFFF
          FF001B1BC200FFFFFF001B1BC200FFFFFF001B1BC200FFFFFF001B1BC200FFFF
          FF001B1BC2001B1BC2001B1BC200FF00FF00FF00FF00FF00FF001B1BC200FFFF
          FF00FFFFFF001B1BC2001B1BC200FFFFFF00FFFFFF001B1BC2001B1BC200FFFF
          FF00FFFFFF00FFFFFF001B1BC200FF00FF00FF00FF00FF00FF001B1BC2001B1B
          C2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1BC2001B1B
          C2001B1BC2001B1BC2001B1BC200FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF008080800080808000808080008080800080808000808080008080
          80008080800080808000808080008080800080808000FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF00000000000000000000000000000000000000
          0000000000000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF000000FF000000FF000000FF000000FF000000FF000000
          FF000000FF000000FF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000FFFFFF00FFFFFF00C0C0C000FFFFFF00C0C0
          C000FFFFFF00C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
          C000C0C0C000C0C0C000FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF00FF00FF00FF00FF00
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end>
  end
  object ilStub: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 7012371
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
          00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000000000000000000000FF00FF008484840000000000000000000000
          00000000000084848400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000000000000000000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000084848400FFFF0000FFFF0000FF00FF00FF00
          FF00FF00FF00FF00FF008484840000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF008484840000000000FF00FF00FFFF0000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000084848400FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FFFF0000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FFFF0000FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FFFF0000FF00FF0000000000FF00FF00FF00FF00FF00
          FF00FF00FF008484840000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FFFF0000FFFF00000000000084848400FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000084848400FF00FF00FF00FF00FF00FF00FFFF
          0000FFFF0000FFFF00008484840000000000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00
          FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF008484840000000000000000000000
          00000000000084848400FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF008484
          8400848484008484840084848400848484008484840084848400848484008484
          8400848484008484840084848400848484008484840084848400000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00000000000000000000000000FFFFFF00000000000000000000000000FFFF
          FF00000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF0000000000C6C6C600FFFFFF00C6C6C60000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00C6C6C600000000000000000000000000C6C6C600FFFFFF00FFFF
          FF00000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00848484008484840084848400FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000FFFFFF00000000008484840000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FF00FF00}
        MaskColor = clFuchsia
      end>
  end
end
