inherited SpreadSheetRLForm: TSpreadSheetRLForm
  Left = 182
  Top = 159
  Caption = 'Report Links Demo - ExpressSpreadSheet'
  ClientHeight = 531
  ClientWidth = 874
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Top = 54
    Width = 874
    Caption = 
      'This example demonstrates the ExpressSpreadSheet printing capabi' +
      'lities.'
  end
  inherited sbMain: TStatusBar
    Top = 512
    Width = 874
    Visible = False
  end
  inherited ToolBar1: TToolBar
    Width = 874
    object tbsOpen: TToolButton
      Left = 123
      Top = 0
      Action = actOpen
    end
    object ToolButton2: TToolButton
      Left = 146
      Top = 0
      Action = actSave
    end
    object ToolButton3: TToolButton
      Left = 169
      Top = 0
      Action = actCut
    end
    object ToolButton4: TToolButton
      Left = 192
      Top = 0
      Action = actCopy
    end
    object ToolButton5: TToolButton
      Left = 215
      Top = 0
      Action = actPaste
    end
  end
  object ToolBar2: TToolBar [3]
    Left = 0
    Top = 25
    Width = 874
    Height = 29
    ButtonHeight = 24
    Caption = 'ToolBar2'
    EdgeBorders = [ebTop, ebBottom]
    TabOrder = 2
    object pnCellsRect: TPanel
      Left = 0
      Top = 0
      Width = 81
      Height = 24
      BevelInner = bvLowered
      BevelOuter = bvNone
      Caption = 'A1'
      Color = clBtnHighlight
      ParentBackground = False
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 81
      Top = 0
      Width = 56
      Height = 24
      Alignment = taRightJustify
      BevelOuter = bvNone
      Caption = 'Cell text = '
      ParentBackground = False
      TabOrder = 0
    end
    object edtCellEdit: TEdit
      Left = 137
      Top = 0
      Width = 571
      Height = 24
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 2
      OnChange = edtCellEditChange
    end
  end
  object dxSpreadSheet1: TdxSpreadSheet [4]
    Left = 0
    Top = 70
    Width = 874
    Height = 442
    Align = alClient
    BorderStyle = cxcbsNone
    OnActiveCellChanging = dxSpreadSheet1ActiveCellChanging
    Data = {
      BA01000044585353763242460500000042465320000000000000000001000101
      01010000000000004246532000000000424653200200000001000000200B0000
      0007000000430061006C00690062007200690000000000002000000020000000
      00200000000020000000002000000000200007000000470045004E0045005200
      41004C0000000000000200000000000000000101000000200B00000007000000
      430061006C006900620072006900000000000020000000200000000020000000
      0020000000002000000000200007000000470045004E004500520041004C0000
      0000000002000000000000000001424653200100000042465320170000005400
      6400780053007000720065006100640053006800650065007400540061006200
      6C00650056006900650077000600000053006800650065007400310001FFFFFF
      FFFFFFFFFF640000000200000002000000020000005500000014000000020000
      0002000000000200000042465320550000000000000042465320000000004246
      5320140000000000000042465320000000000000000000000000010000000000
      000000000000000000000000000000000000000000004246532000000000}
  end
  inherited mmMain: TMainMenu
    inherited miFile: TMenuItem
      object LoadData1: TMenuItem [0]
        Action = actOpen
      end
      object miSaveSpreadSheet: TMenuItem [1]
        Tag = 2
        Action = actSave
      end
      object MenuItem6: TMenuItem [2]
        Caption = '-'
      end
      object PrintArea1: TMenuItem [3]
        Caption = 'Print &Area'
        object SetPrintArea1: TMenuItem
          Action = actSetPrintArea
        end
        object ClearPrintArea1: TMenuItem
          Action = actClearPrintArea
        end
      end
    end
    object mnuEdit: TMenuItem [1]
      Caption = '&Edit'
      object miCut: TMenuItem
        Tag = 3
        Action = actCut
      end
      object miCopy: TMenuItem
        Tag = 2
        Action = actCopy
      end
      object miPaste: TMenuItem
        Tag = 4
        Action = actPaste
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miCells: TMenuItem
        Caption = 'C&ells'
        object miFormat: TMenuItem
          Tag = 5
          Action = actFormatCells
        end
        object miDeletecells: TMenuItem
          Tag = 12
          Action = actDeleteCells
        end
        object Insertcells1: TMenuItem
          Tag = 13
          Action = actInsertCells
        end
      end
    end
  end
  inherited sty: TActionList
    Left = 80
    object actDeleteCells: TAction
      Category = 'Cells'
      Caption = 'Delete...'
      OnExecute = actDeleteCellsExecute
      OnUpdate = AlwaysEnabled
    end
    object actSave: TAction
      Category = 'File'
      Caption = '&Save'
      ImageIndex = 14
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actInsertCells: TAction
      Tag = 1
      Category = 'Cells'
      Caption = 'Insert...'
      OnExecute = actInsertCellsExecute
      OnUpdate = AlwaysEnabled
    end
    object actCut: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut'
      ImageIndex = 15
      ShortCut = 16472
      OnExecute = actCutExecute
      OnUpdate = AlwaysEnabled
    end
    object actCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy'
      ImageIndex = 16
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = AlwaysEnabled
    end
    object actPaste: TAction
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste'
      ImageIndex = 17
      ShortCut = 16470
      OnExecute = actPasteExecute
      OnUpdate = AlwaysEnabled
    end
    object actFormatCells: TAction
      Category = 'Cells'
      Caption = 'Format Cells...'
      OnExecute = actFormatCellsExecute
      OnUpdate = AlwaysEnabled
    end
    object actOpen: TAction
      Category = 'File'
      Caption = '&Open'
      ImageIndex = 19
      OnExecute = actOpenExecute
      OnUpdate = AlwaysEnabled
    end
    object actSetPrintArea: TAction
      Caption = '&Set Print Area'
      OnExecute = actSetPrintAreaExecute
      OnUpdate = AlwaysEnabled
    end
    object actClearPrintArea: TAction
      Caption = '&Clear Print Area'
      OnExecute = actClearPrintAreaExecute
      OnUpdate = AlwaysEnabled
    end
  end
  inherited dxComponentPrinter: TdxComponentPrinter
    CurrentLink = dxComponentPrinterLink1
    object dxComponentPrinterLink1: TdxSpreadSheetReportLnk
      Component = dxSpreadSheet1
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 200
      PrinterPage.GrayShading = True
      PrinterPage.Header = 200
      PrinterPage.PageSize.X = 8500
      PrinterPage.PageSize.Y = 11000
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 1
      BuiltInReportLink = True
    end
  end
  inherited dxPSEngineController1: TdxPSEngineController
    Active = True
    Left = 608
    Top = 120
  end
  inherited ilMain: TcxImageList
    FormatVersion = 1
    DesignInfo = 6815872
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xls'
    Filter = 'Spreadsheet files (*.xls)|*.xls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 440
    Top = 89
  end
  object OpenDialog: TOpenDialog
    Left = 296
    Top = 264
  end
end
