inherited dxRichEditHyperlinkDialogForm: TdxRichEditHyperlinkDialogForm
  Caption = 'Hyperlink'
  ClientHeight = 244
  ClientWidth = 401
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 401
    Height = 244
    object edtText: TcxTextEdit [0]
      Left = 90
      Top = 10
      Style.HotTrack = False
      TabOrder = 0
      Width = 296
    end
    object edtTooltip: TcxTextEdit [1]
      Left = 90
      Top = 37
      Style.HotTrack = False
      TabOrder = 1
      Width = 296
    end
    object cmbTargetFrame: TcxComboBox [2]
      Left = 90
      Top = 64
      Style.HotTrack = False
      TabOrder = 2
      Width = 296
    end
    object rbLinkToWebPage: TcxRadioButton [3]
      Left = 90
      Top = 91
      Width = 296
      Height = 20
      Caption = 'Existing file or web page'
      TabOrder = 3
      Transparent = True
    end
    object rbLinkToDocument: TcxRadioButton [4]
      Left = 90
      Top = 117
      Width = 296
      Height = 17
      Caption = 'Place in this document'
      TabOrder = 4
      Transparent = True
    end
    object cmblBookmark: TcxComboBox [5]
      Left = 90
      Top = 140
      Style.HotTrack = False
      TabOrder = 5
      Width = 296
    end
    object btnOk: TcxButton [6]
      Left = 230
      Top = 206
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 7
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [7]
      Left = 311
      Top = 206
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 8
    end
    object edtEditAddress: TcxButtonEdit [8]
      Left = 90
      Top = 167
      Properties.Buttons = <
        item
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = edtEditAddressPropertiesButtonClick
      Style.HotTrack = False
      TabOrder = 6
      Width = 296
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object liText: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = '&Text to display:'
      Control = edtText
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liTooltip: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'ScreenTi&p:'
      Control = edtTooltip
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liTarget: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Tar&get frame:'
      Control = cmbTargetFrame
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object lilLinkTo: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Link to:'
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'RadioButton1'
      CaptionOptions.Visible = False
      Control = rbLinkToWebPage
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'RadioButton2'
      CaptionOptions.Visible = False
      Control = rbLinkToDocument
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      Index = 1
      AutoCreated = True
    end
    object lilBookmark: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'B&ookmark:'
      Visible = False
      Control = cmblBookmark
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 7
      AutoCreated = True
    end
    object liAddress: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Address:'
      Control = edtEditAddress
      ControlOptions.AlignHorz = ahRight
      ControlOptions.Opaque = True
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1SeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 360
    Top = 8
  end
end
