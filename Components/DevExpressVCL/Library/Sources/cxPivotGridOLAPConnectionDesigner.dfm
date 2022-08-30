object frmConnectionDesigner: TfrmConnectionDesigner
  Left = 289
  Top = 153
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'OLAP Connection'
  ClientHeight = 193
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 400
    Height = 193
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object rbAnalysisServer: TcxRadioButton
      Tag = 1
      Left = 95
      Top = 28
      Width = 113
      Height = 17
      Caption = 'Analysis server'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbAnalysisServerClick
      Transparent = True
    end
    object rbCubeFile: TcxRadioButton
      Left = 214
      Top = 28
      Width = 113
      Height = 17
      Caption = 'Cube file'
      TabOrder = 1
      OnClick = rbAnalysisServerClick
      Transparent = True
    end
    object edtServer: TcxButtonEdit
      Left = 95
      Top = 51
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = edtServerPropertiesButtonClick
      Properties.OnEditValueChanged = edtServerChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 249
    end
    object edtDatabase: TcxComboBox
      Left = 95
      Top = 76
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = edtDatabaseChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 249
    end
    object edtCube: TcxComboBox
      Left = 95
      Top = 101
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 249
    end
    object btnOk: TcxButton
      Left = 180
      Top = 138
      Width = 85
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object btnCancel: TcxButton
      Left = 271
      Top = 138
      Width = 85
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lbConnectType: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Connect using'
      Control = rbAnalysisServer
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'rbCubeFile'
      CaptionOptions.Visible = False
      Control = rbCubeFile
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 1
    end
    object lbServer: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Server'
      Control = edtServer
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 249
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbDataBase: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Database'
      Control = edtDatabase
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 249
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbCube: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Cube'
      Control = edtCube
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 249
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Cube files|*.cub|All files|*.*'
    Left = 288
    Top = 13
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 320
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
