object frmSpreadSheetConditionalFormattingRuleEditDialog: TfrmSpreadSheetConditionalFormattingRuleEditDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'frmSpreadSheetConditionalFormattingRuleEditDialog'
  ClientHeight = 442
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 624
    Height = 442
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutcxLookAndFeel
    HighlightRoot = False
    object pbPreview: TPaintBox
      Left = 11
      Top = 393
      Width = 209
      Height = 50
      Color = clBtnFace
      ParentColor = False
      OnPaint = pbPreviewPaint
    end
    object tlRuleType: TcxTreeList
      Left = 10
      Top = 28
      Width = 587
      Height = 20
      Bands = <
        item
        end>
      Navigator.Buttons.CustomButtons = <>
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Headers = False
      OptionsView.ShowRoot = False
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 0
      OnFocusedNodeChanged = tlRuleTypeFocusedNodeChanged
      object tlRuleTypeColumn1: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        SortOrder = soAscending
        SortIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object cbbOperator: TcxComboBox
      Left = 10
      Top = 74
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        '1'
        '2'
        '3')
      Properties.OnChange = ccbOperatorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 210
    end
    object teExpression1: TcxMaskEdit
      Left = 226
      Top = 74
      Properties.MaskKind = emkRegExprEx
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 183
    end
    object teExpression2: TcxMaskEdit
      Left = 415
      Top = 74
      Properties.MaskKind = emkRegExprEx
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 182
    end
    object btnOk: TcxButton
      Left = 421
      Top = 473
      Width = 85
      Height = 25
      Caption = 'btnOk'
      Default = True
      TabOrder = 23
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton
      Left = 512
      Top = 473
      Width = 85
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 24
    end
    object cbbAverageComparisonOperator: TcxComboBox
      Left = 10
      Top = 99
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        '1'
        '2'
        '3')
      Properties.OnChange = ccbOperatorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 587
    end
    object cbbTopBottomValues: TcxComboBox
      Left = 10
      Top = 124
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        '1'
        '2'
        '3')
      Properties.OnChange = ccbOperatorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 210
    end
    object cbTopBottomValuesPercents: TcxCheckBox
      Left = 318
      Top = 124
      Caption = 'cbTopBottomValuesPercents'
      Style.HotTrack = False
      TabOrder = 7
      Transparent = True
    end
    object seTopBottomValuesRank: TcxSpinEdit
      Left = 226
      Top = 124
      Properties.MaxValue = 100.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Value = 1
      Width = 86
    end
    object meScaleMinStopValue: TcxMaskEdit
      Left = 10
      Top = 196
      Properties.OnEditValueChanged = meScaleStopValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 191
    end
    object ccbColorScaleMinStopColor: TcxColorComboBox
      Left = 10
      Top = 221
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Properties.OnChange = ccbColorScaleStopColorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Width = 191
    end
    object cbbScaleMidStopValueType: TcxComboBox
      Tag = 1
      Left = 207
      Top = 171
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbbScaleStopValueTypePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Width = 192
    end
    object meScaleMidStopValue: TcxMaskEdit
      Tag = 1
      Left = 207
      Top = 196
      Properties.OnEditValueChanged = meScaleStopValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Width = 192
    end
    object ccbColorScaleMidStopColor: TcxColorComboBox
      Tag = 1
      Left = 207
      Top = 221
      ColorValue = clSilver
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Properties.OnChange = ccbColorScaleStopColorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Width = 192
    end
    object cbbScaleMaxStopValueType: TcxComboBox
      Tag = 2
      Left = 405
      Top = 171
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbbScaleStopValueTypePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Width = 192
    end
    object meScaleMaxStopValue: TcxMaskEdit
      Tag = 2
      Left = 405
      Top = 196
      Properties.OnEditValueChanged = meScaleStopValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Width = 192
    end
    object ccbColorScaleMaxStopColor: TcxColorComboBox
      Tag = 2
      Left = 405
      Top = 221
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Properties.OnChange = ccbColorScaleStopColorPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Width = 192
    end
    object cbbScaleMinStopValueType: TcxComboBox
      Left = 10
      Top = 171
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbbScaleStopValueTypePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Width = 191
    end
    object btnFormat: TcxButton
      Left = 227
      Top = 392
      Width = 85
      Height = 26
      Caption = 'btnFormat'
      TabOrder = 21
      OnClick = btnFormatClick
    end
    object tlIconSet: TcxTreeList
      Left = 10
      Top = 246
      Width = 587
      Height = 92
      Bands = <
        item
        end>
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.Sorting = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.ColumnCustomizing = False
      OptionsCustomizing.ColumnHorzSizing = False
      OptionsCustomizing.ColumnMoving = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsCustomizing.NestedBands = False
      OptionsCustomizing.StackedColumns = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Headers = False
      OptionsView.ShowRoot = False
      TabOrder = 17
      OnEdited = tlIconSetEdited
      OnEditing = tlIconSetEditing
      object tlIconSetColumnIcon: TcxTreeListColumn
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.ImmediatePost = True
        Properties.Items = <>
        Properties.ShowDescriptions = False
        Properties.OnEditValueChanged = tlIconSetColumnIconChanged
        DataBinding.ValueType = 'String'
        Width = 48
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlIconSetColumnInfo: TcxTreeListColumn
        PropertiesClassName = 'TcxLabelProperties'
        DataBinding.ValueType = 'String'
        Width = 200
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlIconSetColumnComparisonOperation: TcxTreeListColumn
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.ImmediatePost = True
        Properties.OnEditValueChanged = tlIconSetColumnComparisonOperationChanged
        DataBinding.ValueType = 'String'
        Width = 48
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlIconSetColumnGetDisplayText
      end
      object tlIconSetColumnStopValue: TcxTreeListColumn
        PropertiesClassName = 'TcxMaskEditProperties'
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlIconSetColumnGetDisplayText
        OnGetEditingProperties = tlIconSetColumnStopValueGetEditingProperties
      end
      object tlIconSetColumnStopValueType: TcxTreeListColumn
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.ImmediatePost = True
        Properties.OnEditValueChanged = tlIconSetColumnStopValueTypeChanged
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 4
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetDisplayText = tlIconSetColumnGetDisplayText
      end
    end
    object icbIconStyle: TcxImageComboBox
      Left = 67
      Top = 344
      AutoSize = False
      Properties.Items = <>
      Properties.ShowDescriptions = False
      Properties.OnEditValueChanged = icbIconStyleChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Height = 22
      Width = 134
    end
    object cbReverseIconOrder: TcxCheckBox
      Left = 207
      Top = 344
      Caption = 'cbReverseIconOrder'
      Properties.OnEditValueChanged = cbReverseIconOrderChanged
      Style.HotTrack = False
      TabOrder = 19
      Transparent = True
    end
    object cbShowIconOnly: TcxCheckBox
      Left = 337
      Top = 344
      Caption = 'cbShowIconOnly'
      Properties.OnEditValueChanged = cbShowIconOnlyChanged
      Style.HotTrack = False
      TabOrder = 20
      Transparent = True
    end
    object cbShowBarOnly: TcxCheckBox
      Left = 10
      Top = 450
      Caption = 'cbShowBarOnly'
      Properties.OnEditValueChanged = cbShowBarOnlyChanged
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lciRuleType: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'Rule Type:'
      CaptionOptions.Layout = clTop
      Control = tlRuleType
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 587
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lclRuleDescription: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'Format only cells with:'
      Index = 1
    end
    object lciOperators: TdxLayoutItem
      Parent = lcgRuleExpression
      Control = cbbOperator
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 210
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgRuleExpression: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcgRuleExpression
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciExpression1: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      Control = teExpression1
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 183
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciExpression2: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      Control = teExpression2
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 182
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 9
      AutoCreated = True
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAboveOrBelowAverage: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      Control = cbbAverageComparisonOperator
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 587
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcgTopBottomValues
      AlignHorz = ahLeft
      AlignVert = avClient
      Control = cbbTopBottomValues
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 210
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgTopBottomValues: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcgTopBottomValues
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbTopBottomValuesPercents
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcgTopBottomValues
      AlignVert = avClient
      Control = seTopBottomValuesRank
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 86
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgScale: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object lcgScaleMinStop: TdxLayoutGroup
      Parent = lcgScale
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcgScaleMinStop
      Control = meScaleMinStopValue
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciColorScaleMinStopColor: TdxLayoutItem
      Parent = lcgScaleMinStop
      Control = ccbColorScaleMinStopColor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcgScaleMidStop: TdxLayoutGroup
      Parent = lcgScale
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lcgScaleMidStop
      Control = cbbScaleMidStopValueType
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem11: TdxLayoutItem
      Parent = lcgScaleMidStop
      Control = meScaleMidStopValue
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciColorScaleMidStopColor: TdxLayoutItem
      Parent = lcgScaleMidStop
      Control = ccbColorScaleMidStopColor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcgScaleMaxStop: TdxLayoutGroup
      Parent = lcgScale
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object lcMainItem13: TdxLayoutItem
      Parent = lcgScaleMaxStop
      Control = cbbScaleMaxStopValueType
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem14: TdxLayoutItem
      Parent = lcgScaleMaxStop
      Control = meScaleMaxStopValue
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciColorScaleMaxStopColor: TdxLayoutItem
      Parent = lcgScaleMaxStop
      Control = ccbColorScaleMaxStopColor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcgScaleMinStop
      Control = cbbScaleMinStopValueType
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lclScaleMaxStop: TdxLayoutLabeledItem
      Parent = lcgScaleMaxStop
      CaptionOptions.Text = 'Label'
      Index = 0
    end
    object lclScaleMidStop: TdxLayoutLabeledItem
      Parent = lcgScaleMidStop
      CaptionOptions.Text = 'Label'
      Index = 0
    end
    object lclScaleMinStop: TdxLayoutLabeledItem
      Parent = lcgScaleMinStop
      CaptionOptions.Text = 'Label'
      Index = 0
    end
    object lcgPreview: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 7
    end
    object lclPreview: TdxLayoutLabeledItem
      Parent = lcgPreview
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Preview:'
      Index = 0
    end
    object lcMainGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcgPreview
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lciPreview: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = pbPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 50
      ControlOptions.OriginalWidth = 209
      Index = 0
    end
    object lciFormatButton: TdxLayoutItem
      Parent = lcMainGroup2
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnFormat
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem15: TdxLayoutItem
      Parent = lcgIconSet
      AlignVert = avBottom
      Control = tlIconSet
      ControlOptions.OriginalHeight = 92
      ControlOptions.OriginalWidth = 587
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgIconSet: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 6
    end
    object lciIconStyle: TdxLayoutItem
      Parent = lcMainGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'I&con Style:'
      Control = icbIconStyle
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 134
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem16: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Visible = False
      Control = cbReverseIconOrder
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 124
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcgIconSet
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lcMainItem17: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = cbShowIconOnly
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 104
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciShowBarOnly: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = cbShowBarOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 8
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 584
    Top = 8
    object dxLayoutcxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object EditRepository: TcxEditRepository
    Left = 552
    Top = 8
    PixelsPerInch = 96
    object EditRepositoryPercentsMask: TcxEditRepositoryMaskItem
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d+'
    end
    object EditRepositoryFloatMask: TcxEditRepositoryMaskItem
      Properties.MaskKind = emkRegExpr
    end
  end
end
