object RzTabStopEditDlg: TRzTabStopEditDlg
  Left = 718
  Top = 119
  Caption = 'Tab Stop Editor'
  ClientHeight = 362
  ClientWidth = 481
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 497
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    481
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object grpPreview: TRzGroupBox
    Left = 8
    Top = 8
    Width = 464
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Preview'
    TabOrder = 0
    DesignSize = (
      464
      151)
    object lstPreview: TRzTabbedListBox
      Left = 9
      Top = 20
      Width = 446
      Height = 121
      Anchors = [akLeft, akTop, akRight, akBottom]
      FrameVisible = True
      HorzScrollBar = True
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object RzPanel1: TRzPanel
    Left = 0
    Top = 163
    Width = 481
    Height = 199
    Align = alBottom
    BorderOuter = fsNone
    TabOrder = 1
    DesignSize = (
      481
      199)
    object btnCancel: TRzButton
      Left = 399
      Top = 164
      Cancel = True
      ModalResult = 2
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      Color = 15791348
      HotTrack = True
      TabOrder = 0
    end
    object btnOK: TRzButton
      Left = 318
      Top = 164
      Default = True
      ModalResult = 1
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Color = 15791348
      HotTrack = True
      TabOrder = 1
    end
    object grpTabStops: TRzGroupBox
      Left = 8
      Top = 54
      Width = 465
      Height = 97
      Caption = 'Edit Tab Stops'
      TabOrder = 2
      object lblMin: TRzLabel
        Left = 167
        Top = 41
        Width = 22
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
        ParentColor = False
      end
      object lblMax: TRzLabel
        Left = 435
        Top = 41
        Width = 22
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '200'
        ParentColor = False
      end
      object Label3: TRzLabel
        Left = 272
        Top = 41
        Width = 64
        Height = 13
        Caption = 'Tab Stop #'
        ParentColor = False
      end
      object lblTabNum: TRzLabel
        Left = 336
        Top = 41
        Width = 7
        Height = 13
        Caption = '0'
        ParentColor = False
      end
      object lstTabs: TRzListBox
        Left = 88
        Top = 20
        Width = 69
        Height = 69
        FrameVisible = True
        ItemHeight = 13
        TabOrder = 0
        OnClick = lstTabsClick
      end
      object btnAdd: TRzButton
        Left = 8
        Top = 20
        Width = 70
        Caption = 'Add'
        Color = 15791348
        HotTrack = True
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnDelete: TRzButton
        Left = 8
        Top = 52
        Width = 70
        Caption = 'Delete'
        Color = 15791348
        Enabled = False
        HotTrack = True
        TabOrder = 3
        OnClick = btnDeleteClick
      end
      object chkRightAligned: TRzCheckBox
        Left = 176
        Top = 18
        Width = 94
        Height = 15
        Caption = 'Right Aligned'
        HotTrack = True
        State = cbUnchecked
        TabOrder = 1
        OnClick = chkRightAlignedClick
      end
      object trkTabPos: TRzTrackBar
        Left = 169
        Top = 59
        Width = 288
        Height = 28
        Position = 0
        ShowTicks = False
        TrackOffset = 10
        OnChange = trkTabPosChange
        Enabled = False
        TabOrder = 4
      end
    end
    object grpTabStopsMode: TRzRadioGroup
      Left = 8
      Top = 5
      Width = 465
      Height = 41
      Caption = 'TabStops Mode'
      Columns = 2
      ItemHotTrack = True
      Items.Strings = (
        'Manual'
        'Automatic')
      TabOrder = 3
      OnClick = grpTabStopsModeClick
    end
  end
end
