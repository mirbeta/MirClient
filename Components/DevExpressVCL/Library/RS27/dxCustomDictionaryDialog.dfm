object fmCustomDictionaryForm: TfmCustomDictionaryForm
  Left = 0
  Top = 0
  Caption = 'Custom Dictionary'
  ClientHeight = 335
  ClientWidth = 301
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000004004000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00013F
    8B0001408BFF01408CFF01408DFF01408DFF01408DFF02408CFF02408CFF013F
    8AFF013E89FF023E8800013C8600013C8500013C8300023A8100013A7F000140
    8DFF8AA1BDFFF5F3F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFF0EEE7FF013F8BFFECE9DF00F3F1EA00F9F8F500FEFEFD00023B82000345
    91FFE7E2DDFFF5F3F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFC4B59FFFCDC3ACFF03418EFFEAE8DC00F1F0E800F8F7F200013D8500074E
    9AFFDDD7D0FFEAE7E2FFF3F1EFFFF3F1EFFFF3F1EFFFF3F1EFFFF3F1EFFFF3F1
    EFFF9B7C59FFC6BDA5FFD9D5C2FF024290FFE8E6D900F0EEE500023F89000D5D
    A7FF7297B6F4DBD4CCFFE2DDD6FFE2DDD6FFE2DDD6FFE2DDD6FFE2DDD6FFE2DD
    D6FF9F8464FFB6AA92FFC8C0A9FFD8D2BFFF024493FFE7E4D70003408E001570
    B900054E98FF054E98FF054D97FF044C96FF034994FF024691FF02428FFF0141
    8EFF01408DFFA5967BFFB5A990FFC6BEA7FFD6D1BCFF034596FF01408D001F88
    CD000B63A9000B63A9FF0C96E4FF0C96E4FF0C96E4FF0C96E4FF0C96E4FF0C97
    E4FF0D96E4FF02418EFFA4957BFFB4A990FFC5BCA4FFD4CFBAFF034798FFFFFF
    FF00127BBC00127BBC00127BBCFF12A3ECFF12A3ECFF12A3ECFF12A3ECFF12A3
    ECFF12A3ECFF12A3ECFF034793FFA4947AFFB3A78DFFC4BCA39B01408D00FFFF
    FF001994D0001994D0001994D0001994D0FF1AB2F4FF1AB2F4FF1AB2F4FF1AB2
    F4FF1AB2F4FF1AB2F4FF1AB2F4FF0857A0FFA39479FFB2A68C3701408D00FFFF
    FF0020ACE30020ACE30020ACE30020ACE30020ACE3FF22C1FBFF22C1FBFF22C2
    FBFF22C2FBFF22C2FBFF22C1FBFF22C2FBFF1077B9FFA393783704499400FFFF
    FF0026C1F40026C1F40026C1F40026C1F40026C1F40026C1F4FF2ACFFFFF2ACF
    FFFF2ACFFFFF2ACFFFFF2ACFFFFF2ACFFFFF2ACFFFFF1DA1DAFF127BBC00FFFF
    FF002ACFFF002ACFFF002ACFFF002ACFFF002ACFFF002ACFFF002ACFFFFF2ACF
    FFFF2ACFFFFF2ACFFFFF2ACFFFFF2ACFFFFF2ACFFFFF2ACFFFFF25C0F3FFFFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FFFFFFFFFFFF803FFFFF001FFFFF000FFFFF0007FFFF0003FFFF8001FFFFC000
    FFFFE001FFFFF001FFFFF801FFFFFC01FFFFFE00FFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 301
    Height = 335
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object meDictionary: TcxMemo
      Left = 10
      Top = 10
      Anchors = [akLeft, akTop, akRight, akBottom]
      Properties.ScrollBars = ssVertical
      Properties.WordWrap = False
      Properties.OnChange = meDictionaryPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 286
      Width = 281
    end
    object btnOk: TcxButton
      Left = 115
      Top = 302
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TcxButton
      Left = 206
      Top = 302
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Control = meDictionary
      ControlOptions.OriginalHeight = 252
      ControlOptions.OriginalWidth = 306
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
