object dxfmPreviewWdxBar: TdxfmPreviewWdxBar
  Left = 197
  Top = 188
  ActiveControl = Preview
  Caption = 'Print Preview'
  ClientHeight = 434
  ClientWidth = 885
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
  OldCreateOrder = True
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Preview: TdxPSPreviewWindow
    Left = 0
    Top = 51
    Width = 885
    Height = 383
    Align = alClient
    BorderStyle = cxcbsNone
    PreviewPopupMenu = pmPreview
    OnAddExplorerCommand = PreviewAddExplorerCommand
    OnCanShowMarginHint = PreviewCanShowMarginHint
    OnHFTextEntriesChanged = PreviewHFTextEntriesChanged
    OnInitContent = PreviewInitContent
    OnLoadProperties = PreviewLoadProperties
    OnPreviewDblClick = PreviewPreviewDblClick
    OnSaveProperties = PreviewSaveProperties
    OnStyleListChanged = PreviewStyleListChanged
    OnUpdateControls = PreviewUpdateControls
    OnUpdateExplorerCommands = PreviewUpdateExplorerCommands
    OnZoomFactorChanged = PreviewZoomFactorChanged
    OnZoomModeChanged = PreviewZoomFactorChanged
  end
  object dxBarManager: TdxBarManager
    AutoHideEmptyBars = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'File'
      'Explorer'
      'Edit'
      'Insert'
      'View'
      'Format'
      'Zoom'
      'Tools'
      'Go'
      'Help'
      'Built-in Menus'
      'Shortcut Menus'
      'New Menu')
    Categories.ItemsVisibles = (
      2
      2
      0
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      False
      True)
    ImageOptions.Images = ilToolBar
    ImageOptions.LargeImages = ilToolBar
    ImageOptions.StretchGlyphs = False
    MenusShowRecentItemsFirst = False
    PopupMenuLinks = <
      item
        PopupMenu = pmPreview
      end
      item
        PopupMenu = pmThumbnails
      end>
    ShowShortCutInHint = True
    StoreInRegistry = True
    Style = bmsFlat
    UseSystemFont = True
    OnBarVisibleChange = dxBarManagerBarVisibleChange
    OnHideCustomizingForm = dxBarManagerHideCustomizingForm
    OnShowCustomizingForm = dxBarManagerShowCustomizingForm
    Left = 63
    Top = 197
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      51
      0)
    object dxBarManagerBar1: TdxBar
      Caption = 'MenuBar'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 0
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbFile'
        end
        item
          Visible = True
          ItemName = 'bbExplorer'
        end
        item
          Visible = True
          ItemName = 'bbEdit'
        end
        item
          Visible = True
          ItemName = 'bbView'
        end
        item
          Visible = True
          ItemName = 'bbInsert'
        end
        item
          Visible = True
          ItemName = 'bbFormat'
        end
        item
          Visible = True
          ItemName = 'bbGoToPage'
        end
        item
          Visible = True
          ItemName = 'bbTools'
        end
        item
          Visible = True
          ItemName = 'bbHelp'
        end>
      MultiLine = True
      OldName = 'Build-In Menus'
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManagerBar2: TdxBar
      Caption = 'Standard'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 25
      DockingStyle = dsTop
      FloatLeft = 332
      FloatTop = 321
      FloatClientWidth = 554
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbFileDesign'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFileLoad'
        end
        item
          Visible = True
          ItemName = 'bbFileClose'
        end
        item
          Visible = True
          ItemName = 'bbFileSave'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFilePrint'
        end
        item
          Visible = True
          ItemName = 'bbFilePrintDialog'
        end
        item
          Visible = True
          ItemName = 'bbExportToPdf'
        end
        item
          Visible = True
          ItemName = 'bbFilePageSetup'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewExplorer'
        end
        item
          Visible = True
          ItemName = 'bbViewThumbnails'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatTitle'
        end
        item
          Visible = True
          ItemName = 'bbFormatFootnotes'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbZoomPercent100'
        end
        item
          Visible = True
          ItemName = 'bbZoomPageWidth'
        end
        item
          Visible = True
          ItemName = 'bbZoomWholePage'
        end
        item
          Visible = True
          ItemName = 'bbZoomTwoPages'
        end
        item
          Visible = True
          ItemName = 'bbZoomMultiplePages'
        end
        item
          Visible = True
          ItemName = 'cbxPredefinedZoom'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatPageBackground'
        end
        item
          Visible = True
          ItemName = 'bbFormatShrinkToPageWidth'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbGoToFirstPage'
        end
        item
          Visible = True
          ItemName = 'bbGoToPrevPage'
        end
        item
          UserDefine = [udWidth]
          UserWidth = 63
          Visible = True
          ItemName = 'seActivePage'
        end
        item
          Visible = True
          ItemName = 'bbGoToNextPage'
        end
        item
          Visible = True
          ItemName = 'bbGoToLastPage'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFileExit'
        end>
      OldName = 'Standard'
      OneOnRow = True
      Row = 1
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManagerBar3: TdxBar
      AllowClose = False
      Caption = 'Header and Footer'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 50
      DockedTop = 0
      DockingStyle = dsNone
      FloatLeft = 523
      FloatTop = 228
      FloatClientWidth = 601
      FloatClientHeight = 22
      Hidden = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bsiInsertAutoText'
        end
        item
          BeginGroup = True
          UserDefine = [udPaintStyle]
          Visible = True
          ItemName = 'bbInsertHFPageNumber'
        end
        item
          UserDefine = [udPaintStyle]
          Visible = True
          ItemName = 'bbInsertHFTotalPages'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFPageOfPages'
        end
        item
          Visible = True
          ItemName = 'bbFormatPageNumbering'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbInsertHFDateTime'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFDate'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFTime'
        end
        item
          Visible = True
          ItemName = 'bbFormatDateTime'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbInsertHFUserName'
        end
        item
          UserDefine = [udPaintStyle]
          Visible = True
          ItemName = 'bbInsertHFMachineName'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatHFClear'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatHFBackground'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFilePageSetup'
        end
        item
          Visible = True
          ItemName = 'bbViewHFSwitchHeaderFooter'
        end
        item
          Visible = True
          ItemName = 'bbViewSwitchToLeftPart'
        end
        item
          Visible = True
          ItemName = 'bbViewSwitchToCenterPart'
        end
        item
          Visible = True
          ItemName = 'bbViewSwitchToRightPart'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewHFClose'
        end>
      OldName = 'Header and Footer'
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object dxBarManagerBar4: TdxBar
      Caption = 'Shortcut Menus'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsNone
      FloatLeft = 293
      FloatTop = 319
      FloatClientWidth = 188
      FloatClientHeight = 19
      Hidden = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bsiShortcutPreview'
        end
        item
          Visible = True
          ItemName = 'bsiShortCutExplorer'
        end
        item
          Visible = True
          ItemName = 'bsiShortcutThumbnails'
        end>
      NotDocking = [dsLeft, dsTop, dsRight, dsBottom]
      OldName = 'Shortcut Menus'
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object dxBarManagerBar5: TdxBar
      Caption = 'AutoText'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsNone
      FloatLeft = 460
      FloatTop = 288
      FloatClientWidth = 124
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbInsertEditAutoText'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bsiInsertAutoText'
        end>
      OldName = 'AutoText'
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object dxBarManagerBar6: TdxBar
      Caption = 'Explorer'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 49
      DockingStyle = dsTop
      FloatLeft = 461
      FloatTop = 349
      FloatClientWidth = 23
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbExplorerCreateNewFolder'
        end
        item
          Visible = True
          ItemName = 'bbExplorerDelete'
        end
        item
          Visible = True
          ItemName = 'bbExplorerProperties'
        end>
      OldName = 'Explorer'
      OneOnRow = True
      Row = 2
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object bbFileDesign: TdxBarButton
      Caption = '&Design...'
      Category = 0
      Hint = 'ReportDesign|'
      Visible = ivNever
      ImageIndex = 0
      ShortCut = 16452
      OnClick = DesignClick
    end
    object bbFileRebuild: TdxBarButton
      Caption = 'Rebuild'
      Category = 0
      Hint = 'Rebuild'
      Visible = ivAlways
      ShortCut = 16500
      OnClick = bbFileRebuildClick
    end
    object bbFileLoad: TdxBarButton
      Caption = '&Load...'
      Category = 0
      Hint = 'Load'
      Visible = ivAlways
      ImageIndex = 43
      ShortCut = 16463
      OnClick = ExplorerLoadDataClick
    end
    object bbFileSave: TdxBarButton
      Caption = '&Save...'
      Category = 0
      Hint = 'Save'
      Visible = ivAlways
      ImageIndex = 38
      ShortCut = 16467
      OnClick = FileSaveClick
    end
    object bbFileClose: TdxBarButton
      Caption = '&Unload'
      Category = 0
      Hint = 'Unload'
      Visible = ivAlways
      ImageIndex = 44
      ShortCut = 16499
      OnClick = bbFileCloseClick
    end
    object bbFilePrint: TdxBarButton
      Caption = 'Print'
      Category = 0
      Hint = 'Print'
      Visible = ivNever
      ImageIndex = 1
      OnClick = PrintClick
    end
    object bbFilePrintDialog: TdxBarButton
      Tag = 1
      Caption = '&Print...'
      Category = 0
      Hint = 'Print Dialog'
      Visible = ivNever
      ImageIndex = 2
      ShortCut = 16464
      OnClick = PrintClick
    end
    object bbFilePageSetup: TdxBarButton
      Tag = 2
      Caption = 'Page set&up...'
      Category = 0
      Hint = 'Page Setup '
      Visible = ivNever
      ButtonStyle = bsDropDown
      DropDownMenu = pmPrintStyles
      ImageIndex = 3
      OnClick = PageSetupClick
    end
    object bliPrintStyles: TdxBarListItem
      Caption = 'Print Styles'
      Category = 0
      Visible = ivAlways
      OnGetData = bliPrintStylesGetData
      ShowCheck = True
      ShowNumbers = False
    end
    object bbDefinePrintStyles: TdxBarButton
      Caption = 'Define Print Styles...'
      Category = 0
      Hint = 'Define Print Styles'
      Visible = ivAlways
    end
    object bbFileExit: TdxBarButton
      Caption = '&Close'
      Category = 0
      Hint = 'Close'
      Visible = ivAlways
      OnClick = CloseClick
    end
    object bbExportToPdf: TdxBarButton
      Caption = 'Export To Pdf'
      Category = 0
      Visible = ivAlways
      ImageIndex = 50
      OnClick = bbExportToPdfClick
    end
    object bbFormatFootnotes: TdxBarButton
      Caption = 'Footnotes...'
      Category = 0
      Hint = 'Footnotes'
      Visible = ivAlways
      ImageIndex = 51
      OnClick = bbFormatFootnotesClick
    end
    object bbExplorerCreateNewFolder: TdxBarButton
      Caption = 'Create &Folder...'
      Category = 1
      Hint = 'Create Folder'
      Visible = ivAlways
      ImageIndex = 40
      ShortCut = 32821
      OnClick = ExplorerCreateNewFolderClick
    end
    object bbExplorerDelete: TdxBarButton
      Caption = '&Delete...'
      Category = 1
      Hint = 'Delete'
      Visible = ivAlways
      ImageIndex = 39
      ShortCut = 46
      OnClick = ExplorerDeleteItemClick
    end
    object bbExplorerRename: TdxBarButton
      Caption = '&Rename...'
      Category = 1
      Hint = 'Rename'
      Visible = ivAlways
      ShortCut = 113
      OnClick = ExplorerRenameItemClick
    end
    object bbExplorerProperties: TdxBarButton
      Caption = '&Properties...'
      Category = 1
      Hint = 'P&roperties'
      Visible = ivAlways
      ImageIndex = 46
      ShortCut = 32781
      OnClick = bbExplorerPropertiesClick
    end
    object bbEditFind: TdxBarButton
      Caption = 'Find...'
      Category = 2
      Hint = 'Find'
      Visible = ivNever
      ShortCut = 16454
    end
    object bbEditFindNext: TdxBarButton
      Caption = 'Find &Next'
      Category = 2
      Hint = 'Find Next'
      Visible = ivNever
      ShortCut = 114
    end
    object bbEditReplace: TdxBarButton
      Caption = '&Replace...'
      Category = 2
      Hint = 'Replace'
      Visible = ivNever
      ShortCut = 16466
    end
    object bsiInsertHFAutoText: TdxBarSubItem
      Caption = 'AutoText'
      Category = 3
      Visible = ivAlways
      Detachable = True
      DetachingBar = 4
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbInsertEditAutoText'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bliInsertAutoTextEntries'
        end>
    end
    object bbInsertEditAutoText: TdxBarButton
      Caption = 'AutoTe&xt...'
      Category = 3
      Hint = 'AutoText'
      Visible = ivAlways
      ImageIndex = 33
    end
    object bsiInsertAutoText: TdxBarSubItem
      Caption = 'Insert AutoText'
      Category = 3
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bliInsertAutoTextEntries'
        end>
    end
    object bliInsertAutoTextEntries: TdxBarListItem
      Caption = 'List of AutoText Entries'
      Category = 3
      Visible = ivAlways
      ShowNumbers = False
    end
    object bbInsertHFPageNumber: TdxBarButton
      Caption = '&Page Number'
      Category = 3
      Hint = 'Insert Page Number'
      Visible = ivAlways
      ImageIndex = 19
      ShortCut = 41040
      OnClick = InsertHFClick
    end
    object bbInsertHFTotalPages: TdxBarButton
      Tag = 1
      Caption = '&Number of Pages'
      Category = 3
      Hint = 'Insert Number of Pages'
      Visible = ivAlways
      ImageIndex = 21
      OnClick = InsertHFClick
    end
    object bbInsertHFPageOfPages: TdxBarButton
      Tag = 2
      Caption = 'Page Number Of Pages'
      Category = 3
      Hint = 'Insert Page Number Of Pages'
      Visible = ivAlways
      ImageIndex = 20
      OnClick = InsertHFClick
    end
    object bbInsertHFDateTime: TdxBarButton
      Tag = 3
      Caption = 'Date and Time'
      Category = 3
      Hint = 'Insert Date and Time'
      Visible = ivAlways
      ImageIndex = 23
      OnClick = InsertHFClick
    end
    object bbInsertHFDate: TdxBarButton
      Tag = 4
      Caption = '&Date'
      Category = 3
      Hint = 'Insert Date'
      Visible = ivAlways
      ImageIndex = 24
      ShortCut = 41028
      OnClick = InsertHFClick
    end
    object bbInsertHFTime: TdxBarButton
      Tag = 5
      Caption = '&Time'
      Category = 3
      Hint = 'Insert Time'
      Visible = ivAlways
      ImageIndex = 25
      ShortCut = 41044
      OnClick = InsertHFClick
    end
    object bbInsertHFUserName: TdxBarButton
      Tag = 6
      Caption = '&User Name'
      Category = 3
      Hint = 'Insert User Name'
      Visible = ivAlways
      ImageIndex = 27
      OnClick = InsertHFClick
    end
    object bbInsertHFMachineName: TdxBarButton
      Tag = 7
      Caption = '&Machine Name'
      Category = 3
      Hint = 'Insert Machine Name'
      Visible = ivAlways
      ImageIndex = 28
      OnClick = InsertHFClick
    end
    object bbViewMargins: TdxBarButton
      Caption = '&Margins'
      Category = 4
      Hint = 'Margins'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      ShortCut = 16461
      OnClick = bbViewMarginsClick
    end
    object bbViewMarginBar: TdxBarButton
      Caption = 'MarginBar'
      Category = 4
      Hint = 'Margin Bar'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = bbViewMarginBarClick
    end
    object bbViewStatusBar: TdxBarButton
      Caption = 'StatusBar'
      Category = 4
      Hint = 'StatusBar'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = bbViewStatusBarClick
    end
    object bbViewExplorer: TdxBarButton
      Caption = 'E&xplorer'
      Category = 4
      Hint = 'Explorer'
      Visible = ivAlways
      ButtonStyle = bsChecked
      ImageIndex = 48
      ShortCut = 16472
      OnClick = bbViewExplorerClick
    end
    object bbViewThumbnails: TdxBarButton
      Caption = 'Th&umbnails'
      Category = 4
      Hint = 'Thumbnails'
      Visible = ivAlways
      ButtonStyle = bsChecked
      ImageIndex = 49
      ShortCut = 16469
      OnClick = bbViewThumbnailsClick
    end
    object bbThumbnailsSmall: TdxBarButton
      Caption = '&Small Thumbnails'
      Category = 4
      Hint = 'Small Thumbnails'
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = bbThumbnailsSizeClick
    end
    object bbThumbnailsLarge: TdxBarButton
      Tag = 1
      Caption = '&Large Thumbnails'
      Category = 4
      Hint = 'Large Thumbnails'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = bbThumbnailsSizeClick
    end
    object bbViewToolbars: TdxBarToolbarsListItem
      Caption = '&Toolbars'
      Category = 4
      Visible = ivAlways
    end
    object bbViewPageHeaders: TdxBarButton
      Caption = 'Page &Headers'
      Category = 4
      Hint = 'Page Headers'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = bbViewPageHeadersClick
    end
    object bbViewPageFooters: TdxBarButton
      Caption = 'Page &Footers'
      Category = 4
      Hint = 'Page Footers'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = bbViewPageFootersClick
    end
    object bbViewSwitchToLeftPart: TdxBarButton
      Caption = 'Switch To Left Part'
      Category = 4
      Hint = 'Switch To Left Part'
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      Down = True
      ImageIndex = 30
      OnClick = SwitchPartClick
    end
    object bbViewSwitchToCenterPart: TdxBarButton
      Tag = 1
      Caption = 'Switch To Center Part'
      Category = 4
      Hint = 'Switch To Center Part'
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 31
      OnClick = SwitchPartClick
    end
    object bbViewSwitchToRightPart: TdxBarButton
      Tag = 2
      Caption = 'Switch To Right Part'
      Category = 4
      Hint = 'Switch To Right Part'
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 32
      OnClick = SwitchPartClick
    end
    object bbViewHFSwitchHeaderFooter: TdxBarButton
      Caption = '&Show Header/Footer'
      Category = 4
      Hint = 'Show Header/Footer'
      Visible = ivAlways
      AllowAllUp = True
      ButtonStyle = bsChecked
      ImageIndex = 29
      OnClick = bbViewHFSwitchHeaderFooterClick
    end
    object bbViewHFClose: TdxBarButton
      Caption = '&Close'
      Category = 4
      Hint = 'Close Header and Footer'
      Visible = ivAlways
      OnClick = bbViewHFCloseClick
    end
    object bbFormatHeaderAndFooter: TdxBarButton
      Caption = '&Header and Footer'
      Category = 5
      Hint = 'Header and Footer'
      Visible = ivAlways
      AllowAllUp = True
      ButtonStyle = bsChecked
      ImageIndex = 36
      OnClick = bbFormatHeaderAndFooterClick
    end
    object bbFormatDateTime: TdxBarButton
      Caption = 'Date and &Time...'
      Category = 5
      Hint = 'Date and Time'
      Visible = ivAlways
      ImageIndex = 26
      OnClick = bbFormatDateTimeClick
    end
    object bbFormatPageNumbering: TdxBarButton
      Caption = 'Page &Numbering...'
      Category = 5
      Hint = 'Page Numbering'
      Visible = ivAlways
      ImageIndex = 22
      OnClick = bbFormatPageNumbersClick
    end
    object bbFormatPageBackground: TdxBarButton
      Caption = 'Page Bac&kground...'
      Category = 5
      Hint = 'Background'
      Visible = ivAlways
      ImageIndex = 4
      ShortCut = 16459
      OnClick = PageBackgroundClick
    end
    object bbFormatShrinkToPageWidth: TdxBarButton
      Caption = '&Shrink To Page'
      Category = 5
      Hint = 'Shrink To Page'
      Visible = ivAlways
      AllowAllUp = True
      ButtonStyle = bsChecked
      ImageIndex = 12
      OnClick = bbFormatShrinkToPageWidthClick
    end
    object bbFormatHFBackground: TdxBarButton
      Caption = 'Header and Footer Background ...'
      Category = 5
      Hint = 'Header and Footer Background '
      Visible = ivAlways
      ImageIndex = 34
      OnClick = bbFormatHFBackgroundClick
    end
    object bbFormatHFClear: TdxBarButton
      Caption = 'Clea&r Text'
      Category = 5
      Hint = 'Clear Text'
      Visible = ivAlways
      OnClick = bbFormatHFClearClick
    end
    object bbFormatTitle: TdxBarButton
      Caption = 'Title...'
      Category = 5
      Hint = 'Title'
      Visible = ivAlways
      ImageIndex = 45
      OnClick = bbFormatTitleClick
    end
    object bbZoomPercent100: TdxBarButton
      Caption = '&Percent 100'
      Category = 6
      Hint = 'Zoom 100 %'
      Visible = ivAlways
      ImageIndex = 5
      ShortCut = 16604
      OnClick = ZoomClick
    end
    object bbZoomPageWidth: TdxBarButton
      Tag = 1
      Caption = '&Fit To Window'
      Category = 6
      Hint = 'Zoom Page Width'
      Visible = ivAlways
      ImageIndex = 6
      ShortCut = 16432
      OnClick = ZoomClick
    end
    object bbZoomWholePage: TdxBarButton
      Tag = 2
      Caption = '&One Page'
      Category = 6
      Hint = 'Zoom One Page'
      Visible = ivAlways
      ImageIndex = 7
      ShortCut = 16433
      OnClick = ZoomClick
    end
    object bbZoomTwoPages: TdxBarButton
      Tag = 3
      Caption = '&Two Page'
      Category = 6
      Hint = 'Zoom Two Page'
      Visible = ivAlways
      ImageIndex = 8
      ShortCut = 16434
      OnClick = ZoomClick
    end
    object bbZoomFourPages: TdxBarButton
      Tag = 4
      Caption = 'Four Page'
      Category = 6
      Hint = 'Zoom Four Page'
      Visible = ivAlways
      ImageIndex = 9
      ShortCut = 16436
      OnClick = ZoomClick
    end
    object bbZoomMultiplePages: TdxBarButton
      Caption = '&Multiple Pages'
      Category = 6
      Hint = 'Multiple Pages'
      Visible = ivAlways
      ImageIndex = 10
      OnClick = bbZoomMultiplePagesClick
    end
    object bbZoomWidenToSourceWidth: TdxBarButton
      Tag = 5
      Caption = 'Widen To Source Width'
      Category = 6
      Hint = 'Widen to source width'
      Visible = ivAlways
      ImageIndex = 11
      OnClick = ZoomClick
    end
    object cbxPredefinedZoom: TcxBarEditItem
      Caption = '&Zoom :'
      Category = 6
      Hint = 'Zoom :'
      Visible = ivAlways
      OnChange = cbxPredefinedZoomChange
      Glyph.SourceDPI = 96
      Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E00000000000000000000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000FF000000FFC0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000FF000000FF0000
        00FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000FF0000
        00FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
        00FF000000FF000000FFC0C0C000808080FF000000FF000000FF000000FF0000
        00FF808080FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000000000FF000000FF000000FF000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000000000FF808080FFFFFF00FFFFFF00FFC0C0C000C0C0C000C0C0
        C000C0C0C000808080FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000808080FF000000FFC0C0C000FFFF00FFC0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000000000FF808080FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FFC0C0C000FFFF00FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000FFFF00FFC0C0C000000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000FFFF00FFC0C0C000000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000808080FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000FFFF00FFFFFF00FF000000FF808080FFC0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000000000FF808080FFC0C0C000C0C0C000C0C0C000FFFF00FFFFFF
        00FFFFFF00FF808080FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000000000FF000000FFC0C0C000C0C0C000C0C0C000C0C0
        C000000000FF000000FFC0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000808080FF000000FF000000FF000000FF0000
        00FF808080FFC0C0C000C0C0C000C0C0C000C0C0C000}
      PropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.DefaultImageIndex = 55
      Properties.DropDownRows = 13
      Properties.Images = ilToolBar
      Properties.ImmediatePost = True
      Properties.Items = <
        item
          Description = '500%'
          ImageIndex = 55
          Value = '500%'
        end
        item
          Description = '200%'
          ImageIndex = 55
          Value = '200%'
        end
        item
          Description = '150%'
          ImageIndex = 55
          Value = '150%'
        end
        item
          Description = '100%'
          ImageIndex = 5
          Value = '100%'
        end
        item
          Description = '75%'
          ImageIndex = 55
          Value = '75%'
        end
        item
          Description = '50%'
          ImageIndex = 55
          Value = '50%'
        end
        item
          Description = '25%'
          ImageIndex = 55
          Value = '25%'
        end
        item
          Description = '10%'
          ImageIndex = 55
          Value = '10%'
        end
        item
          Description = 'Page Width'
          ImageIndex = 6
          Value = 'Page Width'
        end
        item
          Description = 'Whole Page'
          ImageIndex = 7
          Value = 'Whole Page'
        end
        item
          Description = 'Two Pages'
          ImageIndex = 8
          Value = 'Two Pages'
        end
        item
          Description = 'Four Pages'
          ImageIndex = 9
          Value = 'Four Pages'
        end
        item
          Description = 'Widen To Source Width'
          ImageIndex = 11
          Value = 'Widen To Source Width'
        end>
      Properties.OnChange = cbxPredefinedZoomChange
    end
    object bbZoomSetup: TdxBarButton
      Caption = '&Setup ...'
      Category = 6
      Hint = 'Setup '
      Visible = ivAlways
      PaintStyle = psCaption
      OnClick = bbZoomSetupClick
    end
    object bbToolsOptions: TdxBarButton
      Caption = '&Options...'
      Category = 7
      Hint = 'Options'
      Visible = ivAlways
      OnClick = bbToolsOptionsClick
    end
    object bbToolsCustomize: TdxBarButton
      Caption = '&Customize...'
      Category = 7
      Hint = 'Customize'
      Visible = ivAlways
      OnClick = bbToolsCustomizeClick
    end
    object bbGoToFirstPage: TdxBarButton
      Caption = '&First Page'
      Category = 8
      Hint = 'Go to First Page'
      Visible = ivAlways
      ImageIndex = 13
      ShortCut = 36
      OnClick = GoToPageClick
    end
    object bbGoToPrevPage: TdxBarButton
      Tag = 1
      Caption = '&Previous Page'
      Category = 8
      Hint = 'Go to Prev Page'
      Visible = ivAlways
      ImageIndex = 14
      ShortCut = 33
      OnClick = GoToPageClick
    end
    object bbGoToNextPage: TdxBarButton
      Tag = 2
      Caption = '&Next Page'
      Category = 8
      Hint = 'Go to Next Page'
      Visible = ivAlways
      ImageIndex = 15
      ShortCut = 34
      OnClick = GoToPageClick
    end
    object bbGoToLastPage: TdxBarButton
      Tag = 3
      Caption = '&Last Page'
      Category = 8
      Hint = 'Go to Last Page'
      Visible = ivAlways
      ImageIndex = 16
      ShortCut = 35
      OnClick = GoToPageClick
    end
    object seActivePage: TcxBarEditItem
      Caption = '&Active Page :'
      Category = 8
      Hint = 'Active Page :'
      Visible = ivAlways
      OnChange = seActivePageChange
      PropertiesClassName = 'TcxSpinEditProperties'
      Properties.ImmediatePost = True
    end
    object bbHelpTopics: TdxBarButton
      Caption = '&Help Topics...'
      Category = 9
      Hint = 'Help'
      Visible = ivAlways
      ImageIndex = 17
      OnClick = HelpClick
    end
    object bbHelpAbout: TdxBarButton
      Caption = '&About...'
      Category = 9
      Hint = 'About'
      Visible = ivNever
    end
    object bbFile: TdxBarSubItem
      Caption = '&File'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbFileDesign'
        end
        item
          Visible = True
          ItemName = 'bbFileRebuild'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFileLoad'
        end
        item
          Visible = True
          ItemName = 'bbFileClose'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFileSave'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFilePrintDialog'
        end
        item
          Visible = True
          ItemName = 'bbFilePageSetup'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbExportToPdf'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFileExit'
        end>
    end
    object bbExplorer: TdxBarSubItem
      Caption = 'E&xplorer'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbExplorerCreateNewFolder'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbExplorerDelete'
        end
        item
          Visible = True
          ItemName = 'bbExplorerRename'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbExplorerProperties'
        end>
    end
    object bbEdit: TdxBarSubItem
      Caption = '&Edit'
      Category = 10
      Visible = ivNever
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbEditFind'
        end
        item
          Visible = True
          ItemName = 'bbEditFindNext'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbEditReplace'
        end>
    end
    object bbInsert: TdxBarSubItem
      Caption = '&Insert'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bsiInsertHFAutoText'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbInsertHFPageNumber'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFTotalPages'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFPageOfPages'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbInsertHFDateTime'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFDate'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFTime'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbInsertHFUserName'
        end
        item
          Visible = True
          ItemName = 'bbInsertHFMachineName'
        end>
    end
    object bbView: TdxBarSubItem
      Caption = '&View'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbViewMargins'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewMarginBar'
        end
        item
          Visible = True
          ItemName = 'bbViewStatusBar'
        end
        item
          Visible = True
          ItemName = 'bbViewExplorer'
        end
        item
          Visible = True
          ItemName = 'bbViewThumbnails'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewToolbars'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatHeaderAndFooter'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewZoom'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbViewPageHeaders'
        end
        item
          Visible = True
          ItemName = 'bbViewPageFooters'
        end>
    end
    object bbViewZoom: TdxBarSubItem
      Caption = 'Zoom'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbZoomPercent100'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbZoomPageWidth'
        end
        item
          Visible = True
          ItemName = 'bbZoomWholePage'
        end
        item
          Visible = True
          ItemName = 'bbZoomTwoPages'
        end
        item
          Visible = True
          ItemName = 'bbZoomFourPages'
        end
        item
          Visible = True
          ItemName = 'bbZoomMultiplePages'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbZoomWidenToSourceWidth'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbZoomSetup'
        end>
    end
    object bbViewPages: TdxBarSubItem
      Caption = 'Pages'
      Category = 10
      Visible = ivNever
      ItemLinks = <>
    end
    object bbFormat: TdxBarSubItem
      Caption = '&Format'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbFormatTitle'
        end
        item
          Visible = True
          ItemName = 'bbFormatFootnotes'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatPageNumbering'
        end
        item
          Visible = True
          ItemName = 'bbFormatDateTime'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatShrinkToPageWidth'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFormatPageBackground'
        end>
    end
    object bbGoToPage: TdxBarSubItem
      Caption = '&Go'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbGoToFirstPage'
        end
        item
          Visible = True
          ItemName = 'bbGoToPrevPage'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'seActivePage'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbGoToNextPage'
        end
        item
          Visible = True
          ItemName = 'bbGoToLastPage'
        end>
    end
    object bbTools: TdxBarSubItem
      Caption = '&Tools'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbToolsCustomize'
        end
        item
          Visible = True
          ItemName = 'bbToolsOptions'
        end>
    end
    object bbHelp: TdxBarSubItem
      Caption = '&Help'
      Category = 10
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbHelpTopics'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbHelpAbout'
        end>
    end
    object bsiShortcutPreview: TdxBarSubItem
      Caption = 'Preview'
      Category = 11
      Visible = ivInCustomizing
      ItemLinks = <>
    end
    object bsiShortCutExplorer: TdxBarSubItem
      Caption = 'Explorer'
      Category = 11
      Visible = ivAlways
      ItemLinks = <>
    end
    object bsiShortcutThumbnails: TdxBarSubItem
      Caption = 'Thumbnails'
      Category = 11
      Visible = ivAlways
      ItemLinks = <>
    end
    object bsiNewMenuNewMenu: TdxBarSubItem
      Caption = 'New Item'
      Category = 12
      Visible = ivAlways
      ItemLinks = <>
    end
  end
  object pmPreview: TdxBarPopupMenu
    BarManager = dxBarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbFileDesign'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbFilePageSetup'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbFormatShrinkToPageWidth'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'cbxPredefinedZoom'
      end
      item
        Visible = True
        ItemName = 'bbZoomWholePage'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbGoToFirstPage'
      end
      item
        Visible = True
        ItemName = 'bbGoToPrevPage'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'seActivePage'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbGoToNextPage'
      end
      item
        Visible = True
        ItemName = 'bbGoToLastPage'
      end>
    UseOwnFont = False
    Left = 91
    Top = 197
    PixelsPerInch = 96
  end
  object pmPrintStyles: TdxBarPopupMenu
    BarManager = dxBarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bliPrintStyles'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbDefinePrintStyles'
      end>
    UseOwnFont = False
    Left = 119
    Top = 197
    PixelsPerInch = 96
  end
  object pmExplorer: TdxBarPopupMenu
    BarManager = dxBarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbFileLoad'
      end
      item
        Visible = True
        ItemName = 'bbFileClose'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbExplorerCreateNewFolder'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbExplorerDelete'
      end
      item
        Visible = True
        ItemName = 'bbExplorerRename'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbExplorerProperties'
      end>
    UseOwnFont = False
    OnPopup = pmExplorerPopup
    Left = 147
    Top = 197
    PixelsPerInch = 96
  end
  object pmThumbnails: TdxBarPopupMenu
    BarManager = dxBarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbThumbnailsSmall'
      end
      item
        Visible = True
        ItemName = 'bbThumbnailsLarge'
      end>
    UseOwnFont = False
    Left = 175
    Top = 197
    PixelsPerInch = 96
  end
  object ilToolBar: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 12910627
  end
end
