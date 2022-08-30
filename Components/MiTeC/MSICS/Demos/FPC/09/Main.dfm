object wnd_dm_Main: Twnd_dm_Main
  Left = 331
  Top = 253
  Caption = 'Directory Monitor'
  ClientHeight = 613
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sb: TStatusBar
    Left = 0
    Top = 594
    Width = 862
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object List: TListView
    Left = 0
    Top = 0
    Width = 862
    Height = 569
    Align = alClient
    Columns = <
      item
        Caption = 'Timestamp'
        Width = 120
      end
      item
        Caption = 'Action'
        Width = 100
      end
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Location'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'Size [B]'
        Width = 75
      end
      item
        Caption = 'Modified'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'Attrs'
        Width = 75
      end>
    ColumnClick = False
    FullDrag = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
    OnDblClick = cmProps
  end
  object Panel1: TPanel
    Left = 0
    Top = 569
    Width = 862
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object Label1: TLabel
      Left = 31
      Top = 6
      Width = 16
      Height = 13
      Caption = 'File'
    end
    object Label2: TLabel
      Left = 87
      Top = 6
      Width = 44
      Height = 13
      Caption = 'Directory'
    end
    object Label3: TLabel
      Left = 174
      Top = 6
      Width = 74
      Height = 13
      Caption = 'Removed entry'
    end
    object Panel2: TPanel
      Left = 9
      Top = 6
      Width = 18
      Height = 13
      Caption = ' '
      Color = clBlue
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 65
      Top = 6
      Width = 18
      Height = 13
      Caption = ' '
      Color = clGreen
      TabOrder = 1
    end
    object Panel4: TPanel
      Left = 152
      Top = 6
      Width = 18
      Height = 13
      Caption = ' '
      Color = clRed
      TabOrder = 2
    end
  end
  object MainMenu: TMainMenu
    OnChange = MainMenuChange
    Left = 124
    Top = 100
    object File1: TMenuItem
      Caption = 'File'
      object mmActive: TMenuItem
        AutoCheck = True
        Caption = 'Active'
        OnClick = mmActiveClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mmSetdirectory: TMenuItem
        Caption = 'Set directory...'
        OnClick = mmSetdirectoryClick
      end
      object Clearlog1: TMenuItem
        Caption = 'Clear log'
        OnClick = Clearlog1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmProps: TMenuItem
        Caption = 'Entry properties...'
        OnClick = cmProps
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mmExit: TMenuItem
        Caption = 'Exit'
        OnClick = mmExitClick
      end
    end
    object Filter1: TMenuItem
      Caption = 'Filter'
      object mmWS: TMenuItem
        AutoCheck = True
        Caption = 'Watch Subtree'
        OnClick = mmWSClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Actions1: TMenuItem
        Caption = 'Actions'
        object Add1: TMenuItem
          AutoCheck = True
          Caption = 'Add'
          Checked = True
          OnClick = cmAction
        end
        object Remove1: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Remove'
          Checked = True
          OnClick = cmAction
        end
        object Modify1: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Modify'
          Checked = True
          OnClick = cmAction
        end
        object RenameOld1: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'Rename (Old)'
          Checked = True
          OnClick = cmAction
        end
        object RenameNew1: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'Rename (New)'
          Checked = True
          OnClick = cmAction
        end
      end
      object Notifications1: TMenuItem
        Caption = 'Notifications'
        object Filename1: TMenuItem
          AutoCheck = True
          Caption = 'File name'
          Checked = True
          OnClick = cmNotify
        end
        object Directoryname1: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Directory name'
          Checked = True
          OnClick = cmNotify
        end
        object Attributes1: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Attributes'
          Checked = True
          OnClick = cmNotify
        end
        object Size1: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'Size'
          Checked = True
          OnClick = cmNotify
        end
        object Lastwrite1: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'Last write'
          Checked = True
          OnClick = cmNotify
        end
        object Lastaccess1: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = 'Last access'
          Checked = True
          OnClick = cmNotify
        end
        object Creation1: TMenuItem
          Tag = 6
          AutoCheck = True
          Caption = 'Creation'
          Checked = True
          OnClick = cmNotify
        end
        object Security1: TMenuItem
          Tag = 7
          AutoCheck = True
          Caption = 'Security'
          Checked = True
          OnClick = cmNotify
        end
      end
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 265
    Top = 121
    object pmProps: TMenuItem
      Caption = 'Properties...'
      OnClick = cmProps
    end
  end
end
