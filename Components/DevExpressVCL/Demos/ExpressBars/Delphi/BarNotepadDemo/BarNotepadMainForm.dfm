inherited frmBarsNotepadMain: TfrmBarsNotepadMain
  Caption = 'ExpressBars BarNotepadDemo'
  PixelsPerInch = 96
  TextHeight = 13
  inherited cxLargeImages: TcxImageList
    FormatVersion = 1
  end
  inherited cxSmallImages: TcxImageList
    FormatVersion = 1
  end
  inherited dxBarManager: TdxBarManager
    Categories.Strings = (
      'Default'
      'File'
      'Edit'
      'Format')
    Categories.ItemsVisibles = (
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True)
    DockControlHeights = (
      0
      0
      77
      24)
    object dxbMain: TdxBar [0]
      Caption = 'Main Menu'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'siFile'
        end
        item
          Visible = True
          ItemName = 'siEdit'
        end
        item
          Visible = True
          ItemName = 'siView'
        end
        item
          Visible = True
          ItemName = 'siFormat'
        end
        item
          Visible = True
          ItemName = 'bsHelp'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object dxbFile: TdxBar [1]
      Caption = 'File'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 25
      DockingStyle = dsTop
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbNew'
        end
        item
          Visible = True
          ItemName = 'bbOpen'
        end
        item
          Visible = True
          ItemName = 'bbSave'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbCut'
        end
        item
          Visible = True
          ItemName = 'bbCopy'
        end
        item
          Visible = True
          ItemName = 'bbPaste'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbUndo'
        end
        item
          Visible = True
          ItemName = 'bbRedo'
        end>
      OneOnRow = False
      Row = 1
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxbStatusBar: TdxBar [2]
      BorderStyle = bbsNone
      Caption = 'Status Bar'
      CaptionButtons = <>
      DockedDockingStyle = dsBottom
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsBottom
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbCursorLine'
        end
        item
          Visible = True
          ItemName = 'bbCursorColumn'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbLocked'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbModified'
        end
        item
          Visible = True
          ItemName = 'bsZoom'
        end>
      OneOnRow = True
      Row = 0
      ShowMark = False
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = True
    end
    object dxbFont: TdxBar [3]
      Caption = 'Font'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 51
      DockingStyle = dsTop
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          UserDefine = [udWidth]
          UserWidth = 134
          Visible = True
          ItemName = 'beFontName'
        end
        item
          UserDefine = [udWidth]
          UserWidth = 46
          Visible = True
          ItemName = 'beFontSize'
        end
        item
          Visible = True
          ItemName = 'bbFontColor'
        end>
      OneOnRow = False
      Row = 2
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxbFormat: TdxBar [4]
      Caption = 'Format'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 224
      DockedTop = 51
      DockingStyle = dsTop
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbBold'
        end
        item
          Visible = True
          ItemName = 'bbItalic'
        end
        item
          Visible = True
          ItemName = 'bbUnderline'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbAlignLeft'
        end
        item
          Visible = True
          ItemName = 'bbAlignCenter'
        end
        item
          Visible = True
          ItemName = 'bbAlignRight'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbBullets'
        end>
      OneOnRow = False
      Row = 2
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxbView: TdxBar [5]
      Caption = 'View'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 243
      DockedTop = 25
      DockingStyle = dsTop
      FloatLeft = 923
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbTabbedView'
        end
        item
          Visible = True
          ItemName = 'bsLookAndFeel'
        end>
      OneOnRow = False
      Row = 1
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    inherited bbModified: TdxBarButton
      PaintStyle = psCaptionGlyph
    end
    inherited scgiLookAndFeel: TdxSkinChooserGalleryItem
      Caption = '&Look And Feel'
    end
    object siView: TdxBarSubItem
      Caption = '&View'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbTabbedView'
        end
        item
          Visible = True
          ItemName = 'bsLookAndFeel'
        end
        item
          UserCaption = '&Zoom'
          UserDefine = [udCaption]
          Visible = True
          ItemName = 'bsZoom'
        end>
    end
    object bsHelp: TdxBarSubItem
      Caption = 'Help'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbBarsHelp'
        end
        item
          Visible = True
          ItemName = 'bbDockingHelp'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbDXOnWeb'
        end
        item
          Visible = True
          ItemName = 'bbDXSupport'
        end
        item
          Visible = True
          ItemName = 'bbDXProducts'
        end
        item
          Visible = True
          ItemName = 'bbDXDownloads'
        end
        item
          Visible = True
          ItemName = 'bbMyDX'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbAbout'
        end>
    end
    object bbBarsHelp: TdxBarButton
      Action = dmCommonData.actBarsHelp
      Category = 0
      ImageIndex = 18
    end
    object bbDockingHelp: TdxBarButton
      Action = dmCommonData.actDockingHelp
      Category = 0
      ImageIndex = 18
    end
    object bbAbout: TdxBarButton
      Action = dmCommonData.actAbout
      Category = 0
    end
    object bbDXOnWeb: TdxBarButton
      Action = dmCommonData.actDXOnTheWeb
      Category = 0
      ImageIndex = 20
    end
    object bbDXSupport: TdxBarButton
      Action = dmCommonData.actSupport
      Category = 0
      ImageIndex = 20
    end
    object bbDXProducts: TdxBarButton
      Action = dmCommonData.actProducts
      Category = 0
      ImageIndex = 20
    end
    object bbDXDownloads: TdxBarButton
      Action = dmCommonData.actDownloads
      Category = 0
      ImageIndex = 20
    end
    object bbMyDX: TdxBarButton
      Action = dmCommonData.actMyDX
      Category = 0
      ImageIndex = 20
    end
    object bsZoom: TdxBarSubItem
      Align = iaRight
      Caption = 'Zoom'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'liZoom'
        end>
    end
    object liZoom: TdxBarListItem
      Caption = 'Zoom'
      Category = 0
      Visible = ivAlways
      OnClick = liZoomClick
      ShowCheck = True
      ShowNumbers = False
    end
    object liRecentDocuments: TdxBarListItem
      Caption = 'Recent'
      Category = 0
      Visible = ivAlways
      OnClick = liRecentDocumentsClick
    end
    object bbTabbedView: TdxBarButton
      Caption = '&Tabbed View'
      Category = 0
      Hint = 'Tabbed View'
      Visible = ivAlways
      ButtonStyle = bsChecked
      ImageIndex = 41
      OnClick = bbTabbedViewClick
    end
    object bsLookAndFeel: TdxBarSubItem
      Caption = '&Look And Feel'
      Category = 0
      Visible = ivAlways
      ImageIndex = 37
      ItemLinks = <
        item
          Visible = True
          ItemName = 'scgiLookAndFeel'
        end>
    end
    object siFile: TdxBarSubItem
      Caption = '&File'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbNew'
        end
        item
          Visible = True
          ItemName = 'bbOpen'
        end
        item
          Visible = True
          ItemName = 'bbSave'
        end
        item
          Visible = True
          ItemName = 'bbSaveAs'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'liRecentDocuments'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbExit'
        end>
    end
    object bbNew: TdxBarButton
      Action = acNew
      Category = 1
      LargeImageIndex = 0
    end
    object bbOpen: TdxBarButton
      Action = acOpen
      Category = 1
      LargeImageIndex = 1
    end
    object bbSave: TdxBarButton
      Action = acSave
      Category = 1
    end
    object bbSaveAs: TdxBarButton
      Action = acSaveAs
      Category = 1
    end
    object bbExit: TdxBarButton
      Action = acExit
      Category = 1
    end
    object bbPrint: TdxBarButton
      Action = acPrint
      Category = 1
    end
    object siEdit: TdxBarSubItem
      Caption = '&Edit'
      Category = 2
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbUndo'
        end
        item
          Visible = True
          ItemName = 'bbRedo'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbCut'
        end
        item
          Visible = True
          ItemName = 'bbCopy'
        end
        item
          Visible = True
          ItemName = 'bbPaste'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbSelectAll'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbFind'
        end
        item
          Visible = True
          ItemName = 'bbReplace'
        end>
    end
    object bbCut: TdxBarButton
      Action = acCut
      Category = 2
    end
    object bbCopy: TdxBarButton
      Action = acCopy
      Category = 2
    end
    object bbPaste: TdxBarButton
      Action = acPaste
      Category = 2
    end
    object bbSelectAll: TdxBarButton
      Action = acSelectAll
      Category = 2
    end
    object bbFind: TdxBarButton
      Action = acFind
      Category = 2
    end
    object bbReplace: TdxBarButton
      Action = acReplace
      Category = 2
    end
    object bbUndo: TdxBarButton
      Action = acUndo
      Category = 2
    end
    object bbRedo: TdxBarButton
      Action = acRedo
      Category = 2
    end
    object siFormat: TdxBarSubItem
      Caption = 'F&ormat'
      Category = 3
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbFont'
        end
        item
          Visible = True
          ItemName = 'bbFontColor'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbAlignLeft'
        end
        item
          Visible = True
          ItemName = 'bbAlignCenter'
        end
        item
          Visible = True
          ItemName = 'bbAlignRight'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'bbBullets'
        end
        item
          BeginGroup = True
          UserCaption = 'Protected'
          UserDefine = [udCaption]
          Visible = True
          ItemName = 'bbLocked'
        end>
    end
    object bbFont: TdxBarButton
      Action = acFont
      Category = 3
    end
    object bbFontColor: TdxBarButton
      Action = acFontColor
      Category = 3
    end
    object bbBold: TdxBarButton
      Action = acBold
      Category = 3
      ButtonStyle = bsChecked
    end
    object bbItalic: TdxBarButton
      Action = acItalic
      Category = 3
      ButtonStyle = bsChecked
    end
    object bbUnderline: TdxBarButton
      Action = acUnderline
      Category = 3
      ButtonStyle = bsChecked
    end
    object bbAlignLeft: TdxBarButton
      Action = acAlignLeft
      Category = 3
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbAlignCenter: TdxBarButton
      Action = acAlignCenter
      Category = 3
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbAlignRight: TdxBarButton
      Action = acAlignRight
      Category = 3
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbBullets: TdxBarButton
      Action = acBullets
      Category = 3
      ButtonStyle = bsChecked
    end
  end
  inherited dxTabbedMDIManager1: TdxTabbedMDIManager
    Active = True
  end
  inherited alActions: TActionList
    inherited acExit: TAction
      ShortCut = 0
    end
    inherited acSaveAs: TAction
      ImageIndex = -1
    end
  end
  inherited FindDialog: TFindDialog
  end
  object pmEditor: TdxBarPopupMenu
    BarManager = dxBarManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbCut'
      end
      item
        Visible = True
        ItemName = 'bbCopy'
      end
      item
        Visible = True
        ItemName = 'bbPaste'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'bbFont'
      end
      item
        Visible = True
        ItemName = 'bbBullets'
      end>
    UseOwnFont = False
    Left = 632
    Top = 8
  end
end
