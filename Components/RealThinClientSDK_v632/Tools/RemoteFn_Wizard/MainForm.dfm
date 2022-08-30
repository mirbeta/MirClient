object formMain: TformMain
  Left = 228
  Top = 210
  Width = 800
  Height = 600
  Caption = 'RTC Remote Functions Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 784
    Height = 564
    ActivePage = tabSetup
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = PagesChange
    object tabSetup: TTabSheet
      Caption = 'Setup'
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 82
        Height = 16
        Caption = 'Unit Filename'
      end
      object Label2: TLabel
        Left = 352
        Top = 32
        Width = 80
        Height = 16
        Caption = 'Unit Directory'
      end
      object Label3: TLabel
        Left = 24
        Top = 136
        Width = 74
        Height = 16
        Caption = 'Class Name'
      end
      object edtUnitFileName: TEdit
        Left = 104
        Top = 24
        Width = 209
        Height = 24
        Hint = 'Unit File Name'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 0
      end
      object edtUnitPath: TEdit
        Left = 440
        Top = 24
        Width = 185
        Height = 24
        Hint = 'Path To Unit'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 1
      end
      object btnOpenFile: TButton
        Left = 208
        Top = 64
        Width = 105
        Height = 25
        Caption = 'Open File'
        TabOrder = 2
        OnClick = btnOpenFileClick
      end
      object edtClassName: TEdit
        Left = 104
        Top = 128
        Width = 209
        Height = 24
        Hint = 'Class Name Used by Unit'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object btnNewFile: TButton
        Left = 352
        Top = 64
        Width = 105
        Height = 25
        Caption = 'New File'
        TabOrder = 4
        OnClick = btnNewFileClick
      end
      object btnSetFont: TButton
        Left = 296
        Top = 240
        Width = 75
        Height = 25
        Caption = 'Set Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = btnSetFontClick
      end
      object chkFuncLbx: TCheckBox
        Left = 312
        Top = 272
        Width = 153
        Height = 17
        Caption = 'Function List'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = chkAutoSaveClick
      end
      object chkFuncVarMmo: TCheckBox
        Left = 312
        Top = 296
        Width = 177
        Height = 17
        Caption = 'Function Variables Memo'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = chkAutoSaveClick
      end
      object chkFuncCodeMmo: TCheckBox
        Left = 312
        Top = 320
        Width = 217
        Height = 17
        Caption = 'Function Implementation Memo'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = chkAutoSaveClick
      end
      object chkFuncSigEdt: TCheckBox
        Left = 312
        Top = 344
        Width = 193
        Height = 17
        Caption = 'Function Signature Edit box'
        Checked = True
        State = cbChecked
        TabOrder = 9
        OnClick = chkAutoSaveClick
      end
      object chkDiscardOptions: TCheckBox
        Left = 32
        Top = 376
        Width = 153
        Height = 17
        Caption = 'Discard all options'
        TabOrder = 10
      end
      object chkAsTmp: TCheckBox
        Left = 64
        Top = 304
        Width = 97
        Height = 17
        Caption = 'As TMP'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = chkAsPasClick
      end
      object chkAsPas: TCheckBox
        Left = 64
        Top = 280
        Width = 97
        Height = 17
        Caption = 'As PAS'
        TabOrder = 12
        OnClick = chkAsPasClick
      end
      object chkAutoSave: TCheckBox
        Left = 32
        Top = 256
        Width = 153
        Height = 17
        Caption = 'AutoSave on Close'
        Checked = True
        State = cbChecked
        TabOrder = 13
        OnClick = chkAutoSaveClick
      end
      object chkPreserveReturnedValue: TCheckBox
        Left = 32
        Top = 224
        Width = 193
        Height = 17
        Caption = 'Preserve Returned Values'
        Checked = True
        State = cbChecked
        TabOrder = 14
      end
      object btnEditFuncs: TButton
        Left = 504
        Top = 64
        Width = 105
        Height = 25
        Caption = 'Edit Functions'
        TabOrder = 15
        OnClick = btnEditFuncsClick
      end
      object chkNewOnEnter: TCheckBox
        Left = 32
        Top = 344
        Width = 249
        Height = 17
        Caption = 'Insert New Function on Enter key'
        Checked = True
        State = cbChecked
        TabOrder = 16
      end
    end
    object tabFunctions: TTabSheet
      Caption = 'Functions'
      ImageIndex = 1
      object HSplit: TSplitter
        Left = 384
        Top = 57
        Width = 4
        Height = 476
        Align = alRight
        Color = clTeal
        ParentColor = False
      end
      object pnlSig: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 57
        Align = alTop
        Color = clWhite
        TabOrder = 0
        DesignSize = (
          776
          57)
        object Label5: TLabel
          Left = 12
          Top = 16
          Width = 56
          Height = 16
          Caption = 'Function: '
        end
        object edtSig: TEdit
          Left = 72
          Top = 16
          Width = 689
          Height = 28
          Hint = 'Current Function Signature'
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          PopupMenu = popFuncList
          ShowHint = True
          TabOrder = 0
          OnChange = edtSigChange
          OnKeyDown = edtSigKeyDown
        end
      end
      object pnlRight: TPanel
        Left = 388
        Top = 57
        Width = 388
        Height = 476
        Align = alRight
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        object VSplit: TSplitter
          Left = 1
          Top = 145
          Width = 386
          Height = 4
          Cursor = crVSplit
          Align = alTop
          Color = clTeal
          ParentColor = False
        end
        object mmoSigVars: TMemo
          Left = 1
          Top = 1
          Width = 386
          Height = 144
          Hint = 'Error/Variable Display Memo'
          TabStop = False
          Align = alTop
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
        end
        object mmoCode: TMemo
          Left = 1
          Top = 149
          Width = 386
          Height = 326
          Hint = 'Function Implementation Memo'
          TabStop = False
          Align = alClient
          ParentShowHint = False
          ReadOnly = True
          ScrollBars = ssBoth
          ShowHint = True
          TabOrder = 1
        end
      end
      object pnlLeft: TPanel
        Left = 0
        Top = 57
        Width = 384
        Height = 476
        Align = alClient
        TabOrder = 2
        DesignSize = (
          384
          476)
        object lbxSignatures: TListBox
          Left = 1
          Top = 1
          Width = 382
          Height = 366
          Hint = 'Signature List Box'
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 16
          ParentFont = False
          ParentShowHint = False
          PopupMenu = popFuncList
          ShowHint = True
          TabOrder = 0
          OnClick = lbxSignaturesClick
        end
        object btnSaveToTmp: TButton
          Left = 8
          Top = 441
          Width = 129
          Height = 25
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Save To .TMP file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = btnSaveToTmpClick
        end
        object btnSaveToPAS: TButton
          Left = 224
          Top = 441
          Width = 153
          Height = 25
          Hint = 'Overwrite original .PAS file'
          Anchors = [akLeft, akBottom]
          Caption = 'Save To .PAS file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = btnSaveToPASClick
        end
        object btnAddFunction: TButton
          Left = 8
          Top = 393
          Width = 129
          Height = 25
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Add New Function'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = miNewSigClick
        end
        object btnDeleteFunction: TButton
          Left = 224
          Top = 393
          Width = 161
          Height = 25
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Delete Current Function'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = miDeleteClick
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'pas'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 180
    Top = 384
  end
  object popFuncList: TPopupMenu
    Left = 216
    Top = 384
    object miNewSig: TMenuItem
      Caption = 'New Function'
      ShortCut = 16462
      OnClick = miNewSigClick
    end
    object miDelete: TMenuItem
      Caption = 'Delete Function'
      ShortCut = 16452
      OnClick = miDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
      ShortCut = 189
    end
    object SaveTMPFile1: TMenuItem
      Caption = 'Save TMP File'
      ShortCut = 16468
      OnClick = btnSaveToTmpClick
    end
    object SavePASfile1: TMenuItem
      Caption = 'Save PAS file'
      ShortCut = 16464
      OnClick = btnSaveToPASClick
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 256
    Top = 384
  end
end
