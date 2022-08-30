object fmFilterControlDialog: TfmFilterControlDialog
  Left = 360
  Top = 200
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'fmFilterControlDialog'
  ClientHeight = 316
  ClientWidth = 552
  Color = clBtnFace
  Constraints.MinHeight = 235
  Constraints.MinWidth = 560
  Icon.Data = {
    0000010001001010000000001800680300001600000028000000100000002000
    0000010018000000000000030000000000000000000000000000000000000000
    0000000000000000000040A0E00060C00060C000000000000000204000000000
    00000000000000000000000000000000000000000000000000000060C000A0E0
    00A0E0000000000000C0DCC04020400000000000000000000000000000000000
    000000000000000000000060C000C0E000C0E0002040C0DCC000000080E0E000
    20400000000000000000000000000000000000000000000000000060C000C0E0
    00C0E00060A040406080E0E080E0E040C0E00020400000000000000000000000
    000000000000000000000060C000C0E000C0E00060A000000000406080C0E0C0
    A080C060A08020600000000000000000000000000000000000000060C000C0E0
    00C0E00060A0000000000000004060C080A0C080E0C060C08020800000000000
    000000000000000060C040E0E040E0E000C0E000C0E00060A000000000000080
    20A0C080E0C0A0E0C080E08020800000000000000080C080E0E040E0E040E0E0
    00C0E000C0E000C0E00060A00000000000008040A0C0A0E0C060C08060A00000
    000080E080E0E080E0E080E0E040E0E040E0E000C0E000C0E000A0E00060A000
    00000000008040A0C060C00000000080E040E0E080E0E080E0E080E0E040E0E0
    40E0E000C0E000C0E000C0E000A0E00060A000000000000000000000000000A0
    E040E0E080E0E0F0FBFF80E0E080E0E040E0E000C0E000C0E000C0E000A0E000
    60A000000000000000000000000000A0E080E0E0F0FBFFF0FBFFF0FBFFF0FBFF
    F0FBFF80E0E080E0E040C0E000A0E00060C000000000000000000000000000A0
    E0F0FBFFC0C0C080A0C04060C00060A00060A04080C040A0E040C0E040C0E000
    60C000000000000000000000000000A0E0F0FBFF4060A00040A00040A00060A0
    0060C00060C00080C000A0E040C0E00060C000000000000000000000000040C0
    E040A0E040A0E04080C04080C00060C00080C04080E040A0E000A0E00080C040
    A0E000000000000000000000000000000080E0E080C0E040C0E040A0E00080E0
    0080E04080E040A0E040A0E080C0E0000000000000000000000000000000F03F
    0000F09F0000F04F0000F0070000F0830000F0C10000E0600000C03000008019
    0000000F0000000F0000000F0000000F0000000F0000000F0000801F0000}
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 552
    Height = 316
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutSkinLookAndFeel
    object btOpen: TcxButton
      Left = 10
      Top = 283
      Width = 100
      Height = 23
      Hint = 'Open|Opens an existing filter'
      Caption = '&Open...'
      TabOrder = 0
      OnClick = acOpenExecute
    end
    object btSave: TcxButton
      Left = 116
      Top = 283
      Width = 100
      Height = 23
      Hint = 'Save As|Saves the active filter with a new name'
      Caption = 'Save &As...'
      TabOrder = 1
      OnClick = acSaveExecute
    end
    object btApply: TcxButton
      Left = 442
      Top = 283
      Width = 100
      Height = 23
      Caption = 'Apply'
      TabOrder = 4
      OnClick = acApplyExecute
    end
    object btCancel: TcxButton
      Left = 336
      Top = 283
      Width = 100
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btOk: TcxButton
      Left = 230
      Top = 283
      Width = 100
      Height = 23
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 2
      OnClick = acOkExecute
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object lgButtons: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object libtOpen: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahLeft
      Control = btOpen
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object libtSave: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahLeft
      Control = btSave
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtOk: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      Control = btOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtCancel: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      Control = btCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object libtApply: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      Control = btApply
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liFilterControl: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Index = 1
    end
  end
  object OpenDialog: TOpenDialog
    Left = 240
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 272
    Top = 8
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 256
    Top = 56
    object dxLayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel
      PixelsPerInch = 96
    end
  end
end
