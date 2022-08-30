object fmTaskEdit: TfmTaskEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'EDIT TASK'
  ClientHeight = 362
  ClientWidth = 1076
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 1076
    Height = 362
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DM.dxLayoutCxLookAndFeel1
    OptionsItem.AutoControlTabOrders = False
    OptionsItem.SizableHorz = True
    OptionsItem.SizableVert = True
    object edHomePhone: TcxDBTextEdit
      Left = 484
      Top = 17
      DataBinding.DataField = 'Subject'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 0
      Width = 570
    end
    object edOwner: TcxDBLookupComboBox
      Left = 123
      Top = 17
      DataBinding.DataField = 'OwnerId'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsEmployeesHelper
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 1
      Width = 204
    end
    object edAssigned: TcxDBLookupComboBox
      Left = 123
      Top = 56
      DataBinding.DataField = 'AssignedEmployeeId'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.KeyFieldNames = 'Id'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsEmployeesHelper
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Width = 204
    end
    object edStartDate: TcxDBDateEdit
      Left = 123
      Top = 115
      DataBinding.DataField = 'StartDate'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 3
      Width = 204
    end
    object edDueDate: TcxDBDateEdit
      Left = 123
      Top = 154
      DataBinding.DataField = 'DueDate'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 4
      Width = 204
    end
    object edPriority: TcxDBImageComboBox
      Left = 123
      Top = 252
      DataBinding.DataField = 'Priority'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.Images = DM.ilPriority
      Properties.Items = <
        item
          Description = 'Low'
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Normal'
          ImageIndex = 1
          Value = 1
        end
        item
          Description = 'High'
          ImageIndex = 2
          Value = 2
        end
        item
          Description = 'Urgent'
          ImageIndex = 3
          Value = 3
        end>
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 5
      Width = 204
    end
    object edStatus: TcxDBLookupComboBox
      Left = 123
      Top = 213
      DataBinding.DataField = 'Status'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'StatusName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsTaskStatus
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 6
      Width = 204
    end
    object edProfile: TcxDBRichEdit
      Left = 484
      Top = 56
      DataBinding.DataField = 'Description'
      DataBinding.DataSource = DM.dsTasks
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 10
      Height = 153
      Width = 570
    end
    object edComplete: TcxTrackBar
      Left = 484
      Top = 219
      Properties.Max = 100
      Properties.ShowPositionHint = True
      Properties.TrackSize = 10
      Style.HotTrack = False
      TabOrder = 11
      Transparent = True
      Height = 76
      Width = 570
    end
    object btnSave: TcxButton
      Left = 809
      Top = 305
      Width = 124
      Height = 40
      Caption = 'Save'
      ModalResult = 1
      OptionsImage.ImageIndex = 30
      OptionsImage.Images = DM.ilButtons
      OptionsImage.Spacing = 15
      TabOrder = 12
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnCancel: TcxButton
      Left = 943
      Top = 305
      Width = 111
      Height = 40
      Caption = 'Cancel'
      ModalResult = 2
      OptionsImage.ImageIndex = 31
      OptionsImage.Images = DM.ilButtons
      OptionsImage.Spacing = 10
      TabOrder = 13
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      SizeOptions.Height = 200
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object liSubject: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'SUBJECT'
      Control = edHomePhone
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 570
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 236
      SizeOptions.Width = 32
      Index = 1
    end
    object liOwner: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'OWNER'
      SizeOptions.Width = 230
      Control = edOwner
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 204
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group1
      AlignHorz = ahLeft
      Index = 0
      AutoCreated = True
    end
    object liAssignedTo: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'ASSIGNED TO'
      Control = edAssigned
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liStartDate: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'START DATE'
      SizeOptions.Width = 310
      Control = edStartDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
    object liDueDate: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'DUE DATE'
      Control = edDueDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 5
    end
    object liPriority: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'PRIORITY'
      SizeOptions.Width = 223
      Control = edPriority
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 153
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object liStatus: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'STATUS'
      SizeOptions.Width = 225
      Control = edStatus
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 158
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object liDescription: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'DESCRIPTION'
      SizeOptions.Height = 119
      SizeOptions.Width = 642
      Control = edProfile
      ControlOptions.OriginalHeight = 119
      ControlOptions.OriginalWidth = 506
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      Index = 2
      AutoCreated = True
    end
    object liComplete: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = '% COMPLETE'
      Control = edComplete
      ControlOptions.OriginalHeight = 76
      ControlOptions.OriginalWidth = 196
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      SizeOptions.Width = 124
      Control = btnSave
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 124
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      SizeOptions.Width = 111
      Control = btnCancel
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 111
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      Index = 0
      AutoCreated = True
    end
  end
end
