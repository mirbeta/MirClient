object dxSpreadSheetInsertFunctionDialogForm: TdxSpreadSheetInsertFunctionDialogForm
  Left = 0
  Top = 0
  ActiveControl = lbFunctions
  BorderIcons = [biSystemMenu]
  Caption = 'Insert Function'
  ClientHeight = 361
  ClientWidth = 424
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 440
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
    Width = 424
    Height = 361
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object btnCancel: TcxButton
      Left = 329
      Top = 326
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnOk: TcxButton
      Left = 238
      Top = 326
      Width = 85
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object cbCategory: TcxComboBox
      Left = 64
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbCategoryPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 350
    end
    object lbFunctions: TcxListBox
      Left = 10
      Top = 53
      Width = 404
      Height = 155
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 3
      OnClick = lbFunctionsClick
      OnDblClick = lbFunctionsDblClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liCategory: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Category:'
      Control = cbCategory
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liFunctions: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'Select a function:'
      CaptionOptions.Layout = clTop
      Control = lbFunctions
      ControlOptions.OriginalHeight = 97
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lliFunctionDescription: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.WordWrap = True
      SizeOptions.Height = 74
      Padding.Top = 8
      Padding.AssignedValues = [lpavTop]
      Index = 4
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 5
    end
    object lliFunctionDefinition: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      LayoutLookAndFeel = dxLayoutCxLookAndFeelBold
      SizeOptions.Height = 14
      Index = 3
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 152
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeelBold: TdxLayoutCxLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clWindowText
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Tahoma'
      ItemOptions.CaptionOptions.Font.Style = [fsBold]
      ItemOptions.CaptionOptions.UseDefaultFont = False
      PixelsPerInch = 96
    end
  end
end
