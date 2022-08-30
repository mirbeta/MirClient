object PreviewMenuEditor: TPreviewMenuEditor
  Left = 101
  Top = 128
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'PreviewMenuEditor'
  ClientHeight = 494
  ClientWidth = 793
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnl_Items: TPanel
    Left = 384
    Top = 2
    Width = 391
    Height = 487
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object pg_Items: TPageControl
      Left = 3
      Top = 3
      Width = 385
      Height = 446
      ActivePage = ts_MenuItems
      TabOrder = 0
      object ts_MenuItems: TTabSheet
        Caption = 'MenuItems'
        object lbl_ShortCutHint: TLabel
          Left = 8
          Top = 73
          Width = 60
          Height = 13
          Caption = 'ShortCutHint'
        end
        object lbl_Caption: TLabel
          Left = 8
          Top = 21
          Width = 36
          Height = 13
          Caption = 'Caption'
        end
        object lbl_SubMenuCaption: TLabel
          Left = 8
          Top = 47
          Width = 85
          Height = 13
          Caption = 'SubMenu Caption'
        end
        object lbl_ShortCutSubHint: TLabel
          Left = 8
          Top = 100
          Width = 99
          Height = 13
          Caption = 'ShortCutSubItemHint'
        end
        object lbl_ImageIndex: TLabel
          Left = 8
          Top = 238
          Width = 55
          Height = 13
          Caption = 'ImageIndex'
        end
        object Label1: TLabel
          Left = 8
          Top = 153
          Width = 105
          Height = 13
          Caption = 'SubMenuItemSpacing'
        end
        object lbl_Tag: TLabel
          Left = 8
          Top = 210
          Width = 19
          Height = 13
          Caption = 'Tag'
        end
        object lbl_Action: TLabel
          Left = 8
          Top = 126
          Width = 30
          Height = 13
          Caption = 'Action'
        end
        object lbl_SubMenuHeight: TLabel
          Left = 8
          Top = 181
          Width = 97
          Height = 13
          Caption = 'SubMenuItemHeight'
        end
        object Label2: TLabel
          Left = 173
          Top = 181
          Width = 46
          Height = 13
          Caption = '(0 = Auto)'
        end
        object edt_Caption: TEdit
          Left = 123
          Top = 17
          Width = 121
          Height = 21
          TabOrder = 0
          OnKeyUp = edt_CaptionKeyUp
        end
        object edt_SubMenuCaption: TEdit
          Left = 123
          Top = 43
          Width = 121
          Height = 21
          TabOrder = 1
          OnKeyUp = edt_SubMenuCaptionKeyUp
        end
        object edt_ShortCutHint: TEdit
          Left = 123
          Top = 69
          Width = 121
          Height = 21
          TabOrder = 2
          OnKeyUp = edt_ShortCutHintKeyUp
        end
        object edt_ShortCutSubHint: TEdit
          Left = 123
          Top = 95
          Width = 121
          Height = 21
          TabOrder = 3
          OnKeyUp = edt_ShortCutSubHintKeyUp
        end
        object spn_Tag: TSpinEdit
          Left = 123
          Top = 205
          Width = 50
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = spn_TagChange
        end
        object spn_SubMenuSpacing: TSpinEdit
          Left = 123
          Top = 148
          Width = 50
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = spn_SubMenuSpacingChange
        end
        object cmb_ImageIndex: TComboBox
          Left = 123
          Top = 233
          Width = 97
          Height = 19
          Style = csOwnerDrawFixed
          ItemHeight = 13
          TabOrder = 6
          OnChange = cmb_ImageIndexChange
          OnDrawItem = cmb_ImageIndexDrawItem
        end
        object chk_Separator: TCheckBox
          Left = 256
          Top = 85
          Width = 80
          Height = 17
          Caption = 'Separator'
          TabOrder = 7
          OnClick = chk_SeparatorClick
        end
        object chk_Enabled: TCheckBox
          Left = 256
          Top = 61
          Width = 80
          Height = 17
          Caption = 'Enabled'
          TabOrder = 8
          OnClick = chk_EnabledClick
        end
        object chk_Visible: TCheckBox
          Left = 256
          Top = 37
          Width = 80
          Height = 17
          Caption = 'Visible'
          TabOrder = 9
          OnClick = chk_VisibleClick
        end
        object chk_CanSelect: TCheckBox
          Left = 256
          Top = 13
          Width = 80
          Height = 17
          Caption = 'CanSelect'
          TabOrder = 10
          OnClick = chk_CanSelectClick
        end
        object btn_Picture: TButton
          Left = 256
          Top = 125
          Width = 87
          Height = 25
          Caption = 'Picture'
          TabOrder = 11
          OnClick = btn_PictureClick
        end
        object btn_DisabledPic: TButton
          Left = 256
          Top = 149
          Width = 87
          Height = 25
          Caption = 'Disabled Picture'
          TabOrder = 12
          OnClick = btn_DisabledPicClick
        end
        object gb_OfficeHint: TGroupBox
          Left = 8
          Top = 272
          Width = 361
          Height = 113
          Caption = 'OfficeHint'
          TabOrder = 13
          object lbl_HintTitle: TLabel
            Left = 8
            Top = 19
            Width = 20
            Height = 13
            Caption = 'Title'
          end
          object lbl_HintNotes: TLabel
            Left = 8
            Top = 37
            Width = 28
            Height = 13
            Caption = 'Notes'
          end
          object btn_HintPicture: TButton
            Left = 278
            Top = 59
            Width = 75
            Height = 25
            Caption = 'Picture'
            TabOrder = 0
          end
          object chk_HintShowHelp: TCheckBox
            Left = 278
            Top = 19
            Width = 78
            Height = 17
            Caption = 'Show Help'
            TabOrder = 1
            OnClick = chk_HintShowHelpClick
          end
          object mem_Notes: TMemo
            Left = 9
            Top = 51
            Width = 259
            Height = 55
            ScrollBars = ssVertical
            TabOrder = 2
            OnChange = mem_NotesChange
          end
          object edt_HintTitle: TEdit
            Left = 61
            Top = 15
            Width = 203
            Height = 21
            TabOrder = 3
            OnKeyUp = edt_HintTitleKeyUp
          end
        end
        object btn_Remove: TButton
          Left = 67
          Top = 390
          Width = 60
          Height = 25
          Caption = 'Remove'
          TabOrder = 14
          OnClick = btn_RemoveClick
        end
        object btn_Add: TButton
          Left = 8
          Top = 390
          Width = 60
          Height = 25
          Caption = 'Add'
          TabOrder = 15
          OnClick = btn_AddClick
        end
        object cmb_Action: TComboBox
          Left = 123
          Top = 122
          Width = 120
          Height = 21
          TabOrder = 16
          OnChange = cmb_ActionChange
        end
        object spn_SubMenuHeight: TSpinEdit
          Left = 123
          Top = 176
          Width = 50
          Height = 22
          Hint = '0 = Auto'
          MaxValue = 0
          MinValue = 0
          TabOrder = 17
          Value = 0
          OnChange = spn_SubMenuHeightChange
        end
      end
      object ts_SubMenuItems: TTabSheet
        Caption = 'SubMenuItems'
        ImageIndex = 1
        object lbl_Title: TLabel
          Left = 8
          Top = 13
          Width = 20
          Height = 13
          Caption = 'Title'
        end
        object lbl_SubShortCutHint: TLabel
          Left = 8
          Top = 38
          Width = 60
          Height = 13
          Caption = 'ShortCutHint'
        end
        object lbl_SubImageIndex: TLabel
          Left = 8
          Top = 141
          Width = 55
          Height = 13
          Caption = 'ImageIndex'
        end
        object lbl_SubTag: TLabel
          Left = 8
          Top = 89
          Width = 19
          Height = 13
          Caption = 'Tag'
        end
        object lbl_SubNotes: TLabel
          Left = 8
          Top = 169
          Width = 28
          Height = 13
          Caption = 'Notes'
        end
        object lbl_SubAction: TLabel
          Left = 8
          Top = 64
          Width = 30
          Height = 13
          Caption = 'Action'
        end
        object lbl_SubMenu: TLabel
          Left = 8
          Top = 115
          Width = 46
          Height = 13
          Caption = 'SubMenu'
        end
        object chk_SubSeparator: TCheckBox
          Left = 208
          Top = 37
          Width = 70
          Height = 17
          Caption = 'Separator'
          TabOrder = 0
          OnClick = chk_SubSeparatorClick
        end
        object edt_Title: TEdit
          Left = 72
          Top = 8
          Width = 121
          Height = 21
          TabOrder = 1
          OnKeyUp = edt_TitleKeyUp
        end
        object edt_SubShortCutHint: TEdit
          Left = 72
          Top = 33
          Width = 121
          Height = 21
          TabOrder = 2
          OnKeyUp = edt_SubShortCutHintKeyUp
        end
        object cmb_SubImageIndex: TComboBox
          Left = 72
          Top = 136
          Width = 97
          Height = 19
          Style = csOwnerDrawFixed
          ItemHeight = 13
          TabOrder = 3
          OnChange = cmb_SubImageIndexChange
          OnDrawItem = cmb_SubImageIndexDrawItem
        end
        object spn_SubTag: TSpinEdit
          Left = 72
          Top = 84
          Width = 98
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = spn_SubTagChange
        end
        object mem_SubNotes: TMemo
          Left = 9
          Top = 183
          Width = 360
          Height = 55
          ScrollBars = ssVertical
          TabOrder = 5
          OnChange = mem_SubNotesChange
        end
        object chk_SubEnabled: TCheckBox
          Left = 208
          Top = 21
          Width = 70
          Height = 17
          Caption = 'Enabled'
          TabOrder = 6
          OnClick = chk_SubEnabledClick
        end
        object chk_SubVisible: TCheckBox
          Left = 208
          Top = 5
          Width = 70
          Height = 17
          Caption = 'Visible'
          TabOrder = 7
          OnClick = chk_SubVisibleClick
        end
        object btn_SubPicture: TButton
          Left = 208
          Top = 69
          Width = 87
          Height = 25
          Caption = 'Picture'
          TabOrder = 8
          OnClick = btn_SubPictureClick
        end
        object btn_SubDisabledPic: TButton
          Left = 209
          Top = 93
          Width = 87
          Height = 25
          Caption = 'Disabled Picture'
          TabOrder = 9
          OnClick = btn_SubDisabledPicClick
        end
        object gb_SubOfficeHint: TGroupBox
          Left = 8
          Top = 249
          Width = 361
          Height = 121
          Caption = 'OfficeHint'
          TabOrder = 10
          object lbl_SubHintTile: TLabel
            Left = 9
            Top = 24
            Width = 20
            Height = 13
            Caption = 'Title'
          end
          object lbl_SubHintNotes: TLabel
            Left = 8
            Top = 44
            Width = 28
            Height = 13
            Caption = 'Notes'
          end
          object mem_SubHintNotes: TMemo
            Left = 9
            Top = 58
            Width = 259
            Height = 55
            ScrollBars = ssVertical
            TabOrder = 0
            OnChange = mem_SubHintNotesChange
          end
          object edt_SubHintTitle: TEdit
            Left = 59
            Top = 20
            Width = 206
            Height = 21
            TabOrder = 1
            OnKeyUp = edt_SubHintTitleKeyUp
          end
          object btn_SubHintPicture: TButton
            Left = 275
            Top = 56
            Width = 75
            Height = 25
            Caption = 'Picture'
            TabOrder = 2
          end
          object chk_SubShowHelp: TCheckBox
            Left = 273
            Top = 24
            Width = 76
            Height = 17
            Caption = 'Show Help'
            TabOrder = 3
            OnClick = chk_SubShowHelpClick
          end
        end
        object btn_SubItemRemove: TButton
          Left = 67
          Top = 384
          Width = 60
          Height = 25
          Caption = 'Remove'
          TabOrder = 11
          OnClick = btn_SubItemRemoveClick
        end
        object btn_SubItemAdd: TButton
          Left = 8
          Top = 384
          Width = 60
          Height = 25
          Caption = 'Add'
          TabOrder = 12
          OnClick = btn_SubItemAddClick
        end
        object btn_RemoveDefault: TButton
          Left = 271
          Top = 384
          Width = 98
          Height = 25
          Caption = 'Remove Default'
          TabOrder = 13
          OnClick = btn_RemoveDefaultClick
        end
        object btn_AddDefault: TButton
          Left = 184
          Top = 384
          Width = 88
          Height = 25
          Caption = 'Add Default'
          TabOrder = 14
          OnClick = btn_AddDefaultClick
        end
        object cmb_SubAction: TComboBox
          Left = 72
          Top = 59
          Width = 97
          Height = 21
          TabOrder = 15
          OnChange = cmb_SubActionChange
        end
        object cmb_SubMenu: TComboBox
          Left = 72
          Top = 110
          Width = 98
          Height = 21
          TabOrder = 16
          OnChange = cmb_SubMenuChange
        end
      end
    end
    object btn_Ok: TButton
      Left = 234
      Top = 456
      Width = 75
      Height = 25
      Hint = 'Ok'
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object Button1: TButton
      Left = 308
      Top = 456
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object pnl_Left: TPanel
    Left = 3
    Top = 458
    Width = 369
    Height = 32
    BevelOuter = bvNone
    TabOrder = 1
    object btn_Up: TButton
      Left = 6
      Top = 6
      Width = 49
      Height = 25
      Caption = 'Up'
      TabOrder = 0
      OnClick = btn_UpClick
    end
    object btn_Down: TButton
      Left = 54
      Top = 6
      Width = 49
      Height = 25
      Caption = 'Down'
      TabOrder = 1
      OnClick = btn_DownClick
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.gif;*.bmp;*.png)|*.jpg;*.jpeg;*.gif;*.bmp;*.' +
      'png|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpe' +
      'g|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|PNG files (*.png' +
      ')|*.png'
    Left = 592
  end
end
