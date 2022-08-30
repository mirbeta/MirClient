object ColumnsMultiEditorsDemoPopupForm: TColumnsMultiEditorsDemoPopupForm
  Left = 240
  Top = 88
  Width = 521
  Height = 375
  Caption = 'ColumnsMultiEditorsDemoPopupForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPopup: TPanel
    Left = 40
    Top = 24
    Width = 185
    Height = 193
    BevelOuter = bvNone
    Caption = 'pnlPopup'
    TabOrder = 0
    object tlPopup: TcxTreeList
      Left = 0
      Top = 0
      Width = 185
      Height = 193
      Styles.Background = stlContenet
      Styles.Content = stlContenet
      Styles.HotTrack = stlHotRoot
      Styles.OnGetHotTrackStyle = tlPopupStylesGetHotTrackStyle
      Align = alClient
      Bands = <
        item
          Caption.AlignHorz = taCenter
          Caption.Text = 'Band1'
          MinWidth = 30
          Width = 250
        end>
      BufferedPaint = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Images = ilPoupuImages
      LookAndFeel.Kind = lfFlat
      OptionsBehavior.GoToNextCellOnTab = True
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.AutomateLeftMostIndent = False
      OptionsBehavior.DragCollapse = False
      OptionsBehavior.ExpandOnIncSearch = True
      OptionsBehavior.HotTrack = True
      OptionsBehavior.Sorting = False
      OptionsBehavior.MultiSort = False
      OptionsBehavior.ShowHourGlass = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnCustomizing = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsSelection.CellSelect = False
      OptionsSelection.HideSelection = True
      OptionsSelection.InvertSelect = False
      OptionsView.CellTextMaxLineCount = -1
      OptionsView.ShowEditButtons = ecsbFocused
      OptionsView.ColumnAutoWidth = True
      OptionsView.Headers = False
      ParentColor = False
      ParentFont = False
      Preview.MaxLineCount = 2
      TabOrder = 0
      OnGetNodeImageIndex = tlPopupGetNodeImageIndex
      OnHotTrackNode = tlPopupHotTrackNode
      OnClick = tlPopupClick
      OnKeyDown = tlPopupKeyDown
      Data = {
        00000400610200000F00000044617461436F6E74726F6C6C6572310100000012
        000000546378537472696E6756616C7565547970651000000000000B00000050
        61796D656E745479706500000400000043617368000004000000566973610000
        060000004D617374657200000B000000416D2E20457870726573730000080000
        00416C69676D656E7400000B0000004C6566744A75737469667900000C000000
        52696768744A75737469667900000600000043656E7465720000060000004765
        6E6465720000040000004D616C6500000600000046656D616C65000005000000
        5374617465000009000000556E636865636B6564000007000000436865636B65
        6400000600000047726179656404000000000000000400000004000000000000
        0001000000FFFFFFFF01000000100000000000000000000000FFFFFFFF020000
        00100000000000000000000000FFFFFFFF030000001000000000000000000000
        00FFFFFFFF04000000100000000000000000000000FFFFFFFF05000000040000
        00030000000000000001000000FFFFFFFF060000001000000000000000000000
        00FFFFFFFF07000000100000000000000000000000FFFFFFFF08000000100000
        000000000000000000FFFFFFFF09000000040000000200000000000000010000
        00FFFFFFFF0A000000100000000000000000000000FFFFFFFF0B000000100000
        000000000000000000FFFFFFFF0C000000040000000300000000000000010000
        00FFFFFFFF0D000000100000000000000000000000FFFFFFFF0E000000100000
        000000000000000000FFFFFFFF0F000000100000000000000000000000FFFFFF
        FF}
      object clText: TcxTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.Alignment.Horz = taLeftJustify
        Properties.MaxLength = 0
        Properties.ReadOnly = False
        Caption.Text = 'Name'
        DataBinding.ValueType = 'String'
        Options.CellEndEllipsis = False
        Options.Customizing = False
        Options.Editing = False
        Options.Moving = False
        Options.ShowEditButtons = eisbNever
        Options.Sorting = False
        Options.TabStop = False
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Width = 166
      end
    end
  end
  object ilPoupuImages: TImageList
    DrawingStyle = dsTransparent
    Left = 40
    Top = 14
    Bitmap = {
      494C010107000A00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001800000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008400000000
      000000000000000000000000000000000000000000000000848484C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00848484848484848484C6C6C684848400000000000000000000000000000000
      000000000000000000000000000000000000840000840000840000FF00008400
      000000000000000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484848484
      84FFFFFFC6C6C6FFFFFFC6C6C684848484848400000000000000000000000000
      0000000000000000000000008400008400FFFFFF00FF00FFFFFF00FF00008400
      008400000000000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484C6C6C6FFFF
      FFC6C6C6FFFFFFC6C6C6C6C6C684848484848484848400000000000000000000
      000000000000000000840000FF00FFFFFF00FF00FFFFFF00FF0000FF00008400
      008400008400000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484FFFFFFC6C6
      C6FFFFFFC6C6C6FFFFFFC6C6C684848484848484848484848400000000000000
      0000000000000000008400FFFFFF00FF00FFFFFF00FF00FFFFFF00FF00008400
      008400008400008400000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484C6C6C6FFFF
      FFFFFFFFC6C6C6C6C6C6C6C6C684848484848484848484848400000000000000
      000000000000000000840000FF00FFFFFFFFFFFF00FF0000FF0000FF00008400
      008400008400008400000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484FFFFFFC6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684848484848484848400000000000000
      0000000000000000008400FFFFFF00FF0000FF0000FF0000FF0000FF0000FF00
      008400008400008400000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684848484848400000000000000
      000000000000000000840000FF0000FF0000FF0000FF0000FF0000FF0000FF00
      00FF00008400008400000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000848484C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684848484848400000000000000
      000000000000000000840000FF0000FF0000FF0000FF0000FF0000FF0000FF00
      00FF00008400008400000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000848484C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684848484848400000000
      000000000000000000000000840000FF0000FF0000FF0000FF0000FF0000FF00
      00FF0000FF00008400008400000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684848484848400000000000000000000
      000000000000000000000000000000840000FF0000FF0000FF0000FF0000FF00
      008400008400000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00848484C6C6C6C6C6C684848484848400000000000000000000000000000000
      000000000000000000000000000000000000840000FF0000FF00008400008400
      000000000000000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FFFFFF84848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084848484848400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008400008400000000000000
      000000000000000000000000000000000000000000000000848484FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C684848400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008484848484848484
      8484848484848484848484848484848484848400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008C8C8CC6C6C6C6C6C6C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600000000
      00000000000000008C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C
      8C8C8C8C8C8C8C8C8C8C8C8C0000000000009C9C00CECE63CECE63CECE63CECE
      63CECE63CECE63CECE63CECE63CECE63CECE63CECE63CECE63CECE6300000000
      00000000000000009C9C009C9C009C9C009C9C009C9C009C9C009C9C009C9C00
      9C9C009C9C009C9C009C9C000000000000008C8C8CFFFFFFD6D6D6F7F7F7D6D6
      D6F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6D6D6D6D6D6D6C6C6C600000000
      00000000000000008C8C8CFFFFFFD6D6D6F7F7F7D6D6D6F7F7F7D6D6D6D6D6D6
      D6D6D6D6D6D6C6C6C68C8C8C0000000000009C9C00FFFFCEFFCE9CFFFF9CFFCE
      9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CFFCE9CFFCE9CCECE6300000000
      00000000000000009C9C00FFFFFFFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CFFCE9C
      FFCE9CFFCE9CCECE639C9C000000000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6D6D6D6C6C6C600000000
      00000000008C8C8CFFFFFFF7F7F7F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6
      D6D6D6D6D6D6C6C6C60000008C8C8C0000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CFFCE9CCECE6300000000
      00000000009C9C00FFFFFFFFFF9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9C
      FFCE9CFFCE9CCECE630000009C9C000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6C6C6C600000000
      00000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7
      D6D6D6D6D6D68C8C8C0000008C8C8C0000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CCECE6300000000
      00000000009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9C
      FFCE9CFFCE9C9C9C000000009C9C000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7C6C6C600000000
      00008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7F7F7F7D6D6D6
      F7F7F7C6C6C6000000C6C6C6C6C6C60000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFFF9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CCECE6300000000
      00009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9CFFFF9CFFCE9C
      FFFF9CCECE63000000CECE63CECE630000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7D6D6D6C6C6C600000000
      00008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7
      D6D6D6C6C6C6000000C6C6C6C6C6C60000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9CFFCE9CCECE6300000000
      00009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9C
      FFCE9CCECE63000000CECE63CECE630000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7F7F7F7F7F7F7D6D6D6F7F7F7D6D6D6F7F7F7C6C6C600000000
      00008C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C
      8C8C8C8C8C8CC6C6C6F7F7F7C6C6C60000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CFFFF9CFFCE9CFFFF9CCECE6300000000
      00009C9C009C9C009C9C009C9C009C9C009C9C009C9C009C9C009C9C009C9C00
      9C9C009C9C00CECE63FFFF9CCECE630000008C8C8CFFFFFFF7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7D6D6D6C6C6C600000000
      00000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F7F7F7F7F7F7F7F7F7F7F7F7C6C6C60000009C9C00FFFFCEFFFF9CFFFF9CFFFF
      9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFCE9CCECE6300000000
      00000000009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9C
      FFFF9CFFFF9CFFFF9CFFFF9CCECE630000008C8C8CFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7C6C6C600000000
      00000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      FFFFFFFFFFFFFFFFFFFFFFFFC6C6C60000009C9C00FFFFCEFFFFCEFFFFCEFFFF
      CEFFFFCEFFFFCEFFFFCEFFFFCEFFFFCEFFFFCEFFFFCEFFFF9CCECE6300000000
      00000000009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9C
      FFFFFFFFFFFFFFFFFFFFFFFFCECE630000008C8C8CC6C6C6C6C6C6C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C68C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C00000000
      00000000008C8C8CFFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FFFFFF8C8C8C
      8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C0000009C9C00CECE63CECE63CECE63CECE
      63CECE63CECE63CECE639C9C009C9C009C9C009C9C009C9C009C9C0000000000
      00000000009C9C00FFFFFFFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFFFF9C9C00
      9C9C009C9C009C9C009C9C009C9C000000000000008C8C8CF7F7F7FFFFFFFFFF
      FFF7F7F7F7F7F78C8C8C00000000000000000000000000000000000000000000
      00000000000000008C8C8CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8C8C8C000000
      0000000000000000000000000000000000000000009C9C00F7F7F7FFFFCEFFFF
      CEFFFF9CFFFF9C9C9C0000000000000000000000000000000000000000000000
      00000000000000009C9C00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9C9C00000000
      0000000000000000000000000000000000000000000000008C8C8C8C8C8C8C8C
      8C8C8C8C8C8C8C00000000000000000000000000000000000000000000000000
      00000000000000000000008C8C8C8C8C8C8C8C8C8C8C8C8C8C8C000000000000
      0000000000000000000000000000000000000000000000009C9C009C9C009C9C
      009C9C009C9C0000000000000000000000000000000000000000000000000000
      00000000000000000000009C9C009C9C009C9C009C9C009C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFF0000FFFFFFFFC0030000
      FF3FFF3FC0030000F81FF81FC0030000E00FE00FC0030000C007C007C0030000
      C003C003C0030000C003C003C0030000C003C003C0030000C003C003C0030000
      C003C003C0030000E003E003C0030000F00FF00FC0030000F83FF83FC0070000
      FCFFFCFFC00F0000FFFFFFFFC01F0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      8001E0008001E0000001C0000001C0000001C0000001C0000001800000018000
      0001800000018000000100000001000000010000000100000001000000010000
      000180000001800000018000000180000003800100038001807FC07F807FC07F
      C0FFE0FFC0FFE0FFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 288
    Top = 32
    object stlHotRoot: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16578029
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      TextColor = clNavy
    end
    object stlContenet: TcxStyle
      AssignedValues = [svColor]
      Color = 16578029
    end
    object stlHotItem: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14917719
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      TextColor = clNavy
    end
  end
end
