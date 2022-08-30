object AsgImportForm: TAsgImportForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Import Settings'
  ClientHeight = 306
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gpbDelimited: TGroupBox
    Left = 144
    Top = 8
    Width = 465
    Height = 59
    TabOrder = 0
    object rdbDel1: TRadioButton
      Tag = 1
      Left = 34
      Top = 11
      Width = 113
      Height = 17
      Caption = '( ; )'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = DelimiterSettingsChanged
    end
    object rdbDel2: TRadioButton
      Tag = 2
      Left = 34
      Top = 34
      Width = 113
      Height = 17
      Caption = '( - )'
      TabOrder = 1
      OnClick = DelimiterSettingsChanged
    end
    object rdbDel3: TRadioButton
      Tag = 3
      Left = 186
      Top = 11
      Width = 113
      Height = 17
      Caption = '( , )'
      TabOrder = 2
      OnClick = DelimiterSettingsChanged
    end
    object rdbDel4: TRadioButton
      Tag = 4
      Left = 186
      Top = 34
      Width = 113
      Height = 17
      Caption = '( _ )'
      TabOrder = 3
      OnClick = DelimiterSettingsChanged
    end
    object rdbDel5: TRadioButton
      Tag = 6
      Left = 330
      Top = 34
      Width = 58
      Height = 17
      Caption = 'Custom'
      TabOrder = 4
      OnClick = DelimiterSettingsChanged
    end
    object txtCustom: TEdit
      Left = 394
      Top = 32
      Width = 33
      Height = 21
      Enabled = False
      MaxLength = 1
      TabOrder = 5
      OnKeyPress = txtCustomKeyPress
    end
    object rdbAutomatic: TRadioButton
      Tag = 5
      Left = 330
      Top = 11
      Width = 113
      Height = 17
      Caption = 'Automatic'
      TabOrder = 6
      OnClick = DelimiterSettingsChanged
    end
  end
  object btnOK: TButton
    Left = 453
    Top = 276
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 534
    Top = 276
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object rdbDelimited: TRadioButton
    Tag = 1
    Left = 8
    Top = 8
    Width = 113
    Height = 17
    Caption = 'Delimited'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = ImportSettingsChanged
  end
  object rdbFixed: TRadioButton
    Tag = 2
    Left = 8
    Top = 71
    Width = 265
    Height = 17
    Caption = 'Fixed (click the column header to set split positions)'
    TabOrder = 3
    OnClick = ImportSettingsChanged
  end
  object ScrollBox1: TScrollBox
    Left = 8
    Top = 93
    Width = 601
    Height = 177
    VertScrollBar.Visible = False
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 5
    object GridPreview: TAdvStringGrid
      Left = 0
      Top = 48
      Width = 597
      Height = 113
      Cursor = crDefault
      BorderStyle = bsNone
      Ctl3D = True
      DefaultRowHeight = 21
      DrawingStyle = gdsClassic
      FixedCols = 0
      FixedRows = 0
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Options = [goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 0
      HoverRowCells = [hcNormal, hcSelected]
      ActiveCellFont.Charset = DEFAULT_CHARSET
      ActiveCellFont.Color = clWindowText
      ActiveCellFont.Height = -11
      ActiveCellFont.Name = 'Tahoma'
      ActiveCellFont.Style = [fsBold]
      ControlLook.FixedGradientHoverFrom = clGray
      ControlLook.FixedGradientHoverTo = clWhite
      ControlLook.FixedGradientDownFrom = clGray
      ControlLook.FixedGradientDownTo = clSilver
      ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
      ControlLook.DropDownHeader.Font.Color = clWindowText
      ControlLook.DropDownHeader.Font.Height = -11
      ControlLook.DropDownHeader.Font.Name = 'Tahoma'
      ControlLook.DropDownHeader.Font.Style = []
      ControlLook.DropDownHeader.Visible = True
      ControlLook.DropDownHeader.Buttons = <>
      ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
      ControlLook.DropDownFooter.Font.Color = clWindowText
      ControlLook.DropDownFooter.Font.Height = -11
      ControlLook.DropDownFooter.Font.Name = 'Tahoma'
      ControlLook.DropDownFooter.Font.Style = []
      ControlLook.DropDownFooter.Visible = True
      ControlLook.DropDownFooter.Buttons = <>
      Filter = <>
      FilterDropDown.Font.Charset = DEFAULT_CHARSET
      FilterDropDown.Font.Color = clWindowText
      FilterDropDown.Font.Height = -11
      FilterDropDown.Font.Name = 'Tahoma'
      FilterDropDown.Font.Style = []
      FilterDropDown.Height = 200
      FilterDropDown.TextChecked = 'Checked'
      FilterDropDown.TextUnChecked = 'Unchecked'
      FilterDropDown.Width = 200
      FilterDropDownClear = '(All)'
      FilterEdit.TypeNames.Strings = (
        'Starts with'
        'Ends with'
        'Contains'
        'Not contains'
        'Equal'
        'Not equal'
        'Clear')
      FixedColWidth = 20
      FixedFont.Charset = ANSI_CHARSET
      FixedFont.Color = clWindowText
      FixedFont.Height = -12
      FixedFont.Name = 'Courier New'
      FixedFont.Style = [fsBold]
      FloatFormat = '%.2f'
      HoverButtons.Buttons = <>
      HoverButtons.Position = hbLeftFromColumnLeft
      PrintSettings.DateFormat = 'dd/mm/yyyy'
      PrintSettings.Font.Charset = DEFAULT_CHARSET
      PrintSettings.Font.Color = clWindowText
      PrintSettings.Font.Height = -11
      PrintSettings.Font.Name = 'Tahoma'
      PrintSettings.Font.Style = []
      PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
      PrintSettings.FixedFont.Color = clWindowText
      PrintSettings.FixedFont.Height = -11
      PrintSettings.FixedFont.Name = 'Tahoma'
      PrintSettings.FixedFont.Style = []
      PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
      PrintSettings.HeaderFont.Color = clWindowText
      PrintSettings.HeaderFont.Height = -11
      PrintSettings.HeaderFont.Name = 'Tahoma'
      PrintSettings.HeaderFont.Style = []
      PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
      PrintSettings.FooterFont.Color = clWindowText
      PrintSettings.FooterFont.Height = -11
      PrintSettings.FooterFont.Name = 'Tahoma'
      PrintSettings.FooterFont.Style = []
      PrintSettings.PageNumSep = '/'
      SearchFooter.FindNextCaption = 'Find &next'
      SearchFooter.FindPrevCaption = 'Find &previous'
      SearchFooter.Font.Charset = DEFAULT_CHARSET
      SearchFooter.Font.Color = clWindowText
      SearchFooter.Font.Height = -11
      SearchFooter.Font.Name = 'Tahoma'
      SearchFooter.Font.Style = []
      SearchFooter.HighLightCaption = 'Highlight'
      SearchFooter.HintClose = 'Close'
      SearchFooter.HintFindNext = 'Find next occurence'
      SearchFooter.HintFindPrev = 'Find previous occurence'
      SearchFooter.HintHighlight = 'Highlight occurences'
      SearchFooter.MatchCaseCaption = 'Match case'
      SortSettings.DefaultFormat = ssAutomatic
      Version = '7.4.3.4'
      ColWidths = (
        20
        20
        20
        20
        20)
    end
    object AdvColumnSetter1: TAdvColumnSetter
      Left = 2
      Top = 0
      Width = 595
      Height = 49
      Offset = 0
      MaxLength = 150
      Grid = GridPreview
      OnColumnSetterInsert = AdvColumnSetter1ColumnSetterInsert
      OnColumnSetterChanged = AdvColumnSetter1ColumnSetterChanged
      OnColumnSetterChanging = AdvColumnSetter1ColumnSetterChanging
      OnColumnSetterDeleted = AdvColumnSetter1ColumnSetterDeleted
    end
  end
  object chkColumnHeaders: TCheckBox
    Left = 8
    Top = 276
    Width = 209
    Height = 17
    Caption = 'First row headers'
    TabOrder = 6
  end
  object ScrollTimer: TTimer
    Interval = 50
    OnTimer = ScrollTimerTimer
    Left = 272
    Top = 264
  end
end
