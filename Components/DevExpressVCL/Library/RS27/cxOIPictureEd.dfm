object cxfmPictureEditor: TcxfmPictureEditor
  Left = 295
  Top = 158
  BorderIcons = [biSystemMenu]
  Caption = 'Picture Editor'
  ClientHeight = 326
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 287
    Width = 368
    Height = 4
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 384
    Height = 326
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object Image: TcxImage
      Left = 10
      Top = 10
      Anchors = [akLeft, akTop, akRight, akBottom]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 266
      Width = 279
    end
    object btnLoad: TcxButton
      Left = 295
      Top = 10
      Width = 79
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Load...'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnSave: TcxButton
      Left = 295
      Top = 39
      Width = 79
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Save...'
      TabOrder = 2
      OnClick = btnSaveClick
    end
    object btnCopy: TcxButton
      Left = 295
      Top = 68
      Width = 79
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Copy'
      TabOrder = 3
      OnClick = btnCopyClick
    end
    object btnPaste: TcxButton
      Left = 295
      Top = 97
      Width = 79
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Paste'
      TabOrder = 4
      OnClick = btnPasteClick
    end
    object btnClear: TcxButton
      Left = 295
      Top = 126
      Width = 79
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'C&lear'
      TabOrder = 5
      OnClick = btnClearClick
    end
    object btnOk: TcxButton
      Left = 207
      Top = 294
      Width = 80
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 6
    end
    object btnCancel: TcxButton
      Left = 293
      Top = 294
      Width = 81
      Height = 22
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 7
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      Control = Image
      ControlOptions.OriginalHeight = 273
      ControlOptions.OriginalWidth = 268
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnLoad'
      CaptionOptions.Visible = False
      Control = btnLoad
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnSave'
      CaptionOptions.Visible = False
      Control = btnSave
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCopy'
      CaptionOptions.Visible = False
      Control = btnCopy
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnPaste'
      CaptionOptions.Visible = False
      Control = btnPaste
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnClear'
      CaptionOptions.Visible = False
      Control = btnClear
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avTop
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
