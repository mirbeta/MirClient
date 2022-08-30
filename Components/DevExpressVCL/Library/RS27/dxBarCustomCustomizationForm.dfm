object dxBarCustomCustomizationForm: TdxBarCustomCustomizationForm
  Left = 707
  Top = 171
  BorderIcons = [biSystemMenu]
  Caption = 'Customize'
  ClientHeight = 306
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object alCustomize: TActionList
    Left = 136
    Top = 280
    object aAddItem: TAction
      Category = 'Items'
      Caption = 'Add Item'
      ImageIndex = 4
      OnExecute = aAddItemExecute
    end
    object aNewToolBar: TAction
      Category = 'ToolBars'
      Caption = '&New...'
      OnExecute = aNewToolBarExecute
    end
    object aRenameToolBar: TAction
      Category = 'ToolBars'
      Caption = 'R&ename...'
      OnExecute = aRenameToolBarExecute
    end
    object aDeleteToolBar: TAction
      Category = 'ToolBars'
      Caption = '&Delete'
      OnExecute = aDeleteToolBarExecute
    end
    object aResetToolBar: TAction
      Category = 'ToolBars'
      Caption = '&Reset...'
      OnExecute = aResetToolBarExecute
    end
    object aDeleteItem: TAction
      Category = 'Items'
      Caption = 'Delete Item'
      ImageIndex = 5
      OnExecute = aDeleteItemExecute
    end
    object aMoveUpItem: TAction
      Tag = -1
      Category = 'Items'
      Caption = 'Move Up'
      ImageIndex = 2
      OnExecute = aMoveItemExecute
    end
    object aMoveDownItem: TAction
      Tag = 1
      Category = 'Items'
      Caption = 'Move Down'
      ImageIndex = 3
      OnExecute = aMoveItemExecute
    end
    object aSubMenuEditor: TAction
      Category = 'Items'
      Caption = 'SubMenu Editor...'
      OnExecute = aSubMenuEditorExecute
    end
    object aClearItemList: TAction
      Category = 'Items'
      Caption = 'Clear'
      OnExecute = aClearItemListExecute
    end
    object actAddGroup: TAction
      Category = 'Groups'
      Caption = 'Add Group'
      Hint = 'Add Group (Ins)'
      OnExecute = actAddGroupExecute
    end
    object actDeleteGroup: TAction
      Category = 'Groups'
      Caption = 'Delete Group'
      Hint = 'Delete Group (Del)'
      ImageIndex = 1
      OnExecute = actDeleteGroupExecute
    end
    object actMoveGroupUp: TAction
      Tag = -1
      Category = 'Groups'
      Caption = 'Move Up Group'
      Hint = 'Move Up Group'
      ImageIndex = 2
      OnExecute = actMoveGroupExecute
    end
    object actMoveGroupDown: TAction
      Tag = 1
      Category = 'Groups'
      Caption = 'Move Down Group'
      Hint = 'Move Down Group'
      ImageIndex = 3
      OnExecute = actMoveGroupExecute
    end
    object actAddGroupItem: TAction
      Category = 'GroupItems'
      Caption = '&Add...'
      ImageIndex = 0
      OnExecute = actAddGroupItemExecute
    end
    object actDeleteGroupItem: TAction
      Category = 'GroupItems'
      Caption = 'Delete'
      ImageIndex = 1
      OnExecute = actDeleteGroupItemExecute
    end
    object actMoveGroupItemUp: TAction
      Tag = -1
      Category = 'GroupItems'
      Caption = 'Move Up Item'
      ImageIndex = 2
      OnExecute = actMoveGroupItemExecute
    end
    object actMoveGroupItemDown: TAction
      Tag = 1
      Category = 'GroupItems'
      Caption = 'Move Down Item'
      ImageIndex = 3
      OnExecute = actMoveGroupItemExecute
    end
  end
  object BarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Categories'
      'Commands')
    Categories.ItemsVisibles = (
      2
      2)
    Categories.Visibles = (
      True
      True)
    ImageOptions.StretchGlyphs = False
    PopupMenuLinks = <>
    UseSystemFont = False
    Left = 170
    Top = 278
    DockControlHeights = (
      0
      0
      0
      0)
    object CategoriesAdd: TdxBarButton
      Caption = 'Add...'
      Category = 0
      Hint = 'Add'
      Visible = ivAlways
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888888888888888888800000000000888880FFFFFFFFF0888880FFFFFFFFF
        0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFFFFF
        0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFF0000888880FFFFFF0F0
        8888880FFFFFF008888888000000008888888888888888888888}
      OnClick = CategoriesAddClick
    end
    object CategoriesInsert: TdxBarButton
      Caption = 'Insert...'
      Category = 0
      Hint = 'Insert'
      Visible = ivAlways
      ShortCut = 45
      OnClick = CategoriesInsertClick
    end
    object CategoriesRename: TdxBarButton
      Caption = 'Rename...'
      Category = 0
      Hint = 'Rename'
      Visible = ivAlways
      OnClick = CategoriesRenameClick
    end
    object CategoriesVisible: TdxBarButton
      Caption = 'Visible'
      Category = 0
      Hint = 'Visible'
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = CategoriesVisibleClick
    end
    object CategoriesItemsVisible: TdxBarCombo
      Caption = 'Items Visible:'
      Category = 0
      Hint = 'Items Visible:'
      Visible = ivAlways
      OnChange = CategoriesItemsVisibleChange
      ShowEditor = False
      Items.Strings = (
        'Never'
        'InCustomizing'
        'Always')
      ItemIndex = -1
    end
    object CategoriesDelete: TdxBarButton
      Caption = 'Delete'
      Category = 0
      Hint = 'Delete'
      Visible = ivAlways
      Glyph.Data = {
        EE000000424DEE000000000000007600000028000000100000000F0000000100
        0400000000007800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888888888888880F88880F8888888888888000F8888880F8888000F888880
        F888888000F88800F8888888000F800F88888888800000F88888888888000F88
        88888888800000F888888888000F80F88888880000F88800F88880000F888880
        0F88800F8888888800F88888888888888888}
      ShortCut = 46
      OnClick = CategoriesDeleteClick
    end
    object CommandsAdd: TdxBarButton
      Caption = 'Add...'
      Category = 1
      Hint = 'Add'
      Visible = ivAlways
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888888888888888888800000000000888880FFFFFFFFF0888880FFFFFFFFF
        0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFFFFF
        0888880FFFFFFFFF0888880FFFFFFFFF0888880FFFFFF0000888880FFFFFF0F0
        8888880FFFFFF008888888000000008888888888888888888888}
      ShortCut = 45
      OnClick = CommandsAddClick
    end
    object CommandsDelete: TdxBarButton
      Caption = 'Delete'
      Category = 1
      Hint = 'Delete'
      Visible = ivAlways
      Glyph.Data = {
        EE000000424DEE000000000000007600000028000000100000000F0000000100
        0400000000007800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888888888888880F88880F8888888888888000F8888880F8888000F888880
        F888888000F88800F8888888000F800F88888888800000F88888888888000F88
        88888888800000F888888888000F80F88888880000F88800F88880000F888880
        0F88800F8888888800F88888888888888888}
      ShortCut = 46
      OnClick = CommandsDeleteClick
    end
    object CommandsClear: TdxBarButton
      Caption = 'Clear'
      Category = 1
      Hint = 'Clear'
      Visible = ivAlways
      OnClick = CommandsClearClick
    end
    object CommandsMoveUp: TdxBarButton
      Caption = 'Move Up'
      Category = 1
      Hint = 'Move Up'
      Visible = ivAlways
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888888888888888800000888888888880666088
        8888888880666088888888888066608888888800006660000888888066666660
        8888888806666608888888888066608888888888880608888888888888808888
        8888888888888888888888888888888888888888888888888888}
      ShortCut = 16422
      OnClick = CommandsMoveUpClick
    end
    object CommandsMoveDown: TdxBarButton
      Caption = 'Move Down'
      Category = 1
      Hint = 'Move Down'
      Visible = ivAlways
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888888888888888888088888888888888060888
        8888888880666088888888880666660888888880666666608888880000666000
        0888888880666088888888888066608888888888806660888888888880000088
        8888888888888888888888888888888888888888888888888888}
      ShortCut = 16424
      OnClick = CommandsMoveDownClick
    end
    object CommandsSubMenuEditor: TdxBarButton
      Caption = 'SubMenu Editor...'
      Category = 1
      Hint = 'SubMenu Editor'
      Visible = ivAlways
      OnClick = CommandsSubMenuEditorClick
    end
  end
  object CategoriesPopupMenu: TdxBarPopupMenu
    BarManager = BarManager1
    ItemLinks = <
      item
        Visible = True
        ItemName = 'CategoriesAdd'
      end
      item
        Visible = True
        ItemName = 'CategoriesInsert'
      end
      item
        Visible = True
        ItemName = 'CategoriesRename'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'CategoriesVisible'
      end
      item
        Visible = True
        ItemName = 'CategoriesItemsVisible'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'CategoriesDelete'
      end>
    UseOwnFont = False
    OnPopup = CategoriesPopupMenuPopup
    Left = 204
    Top = 278
  end
  object CommandsPopupMenu: TdxBarPopupMenu
    BarManager = BarManager1
    ItemLinks = <
      item
        Visible = True
        ItemName = 'CommandsAdd'
      end
      item
        Visible = True
        ItemName = 'CommandsDelete'
      end
      item
        Visible = True
        ItemName = 'CommandsClear'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'CommandsMoveUp'
      end
      item
        Visible = True
        ItemName = 'CommandsMoveDown'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'CommandsSubMenuEditor'
      end>
    UseOwnFont = False
    OnPopup = CommandsPopupMenuPopup
    Left = 236
    Top = 278
  end
  object imgGroups: TcxImageList
    FormatVersion = 1
    DesignInfo = 18350184
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000020000000A170D0738542D1894814626D193502AEA924F2AE87F45
          25D0522C17931209053000000009000000010000000000000000000000000000
          00030201011159311B97A96239FAC58957FFD6A36DFFDDAF75FFDDAF74FFD6A4
          6BFFC58956FFA46137F53C2112730000000F0000000300000000000000020201
          0110744226B9BC7C4DFFDDAE77FFDEB076FFE2B782FFE3BB87FFE3BC86FFE1B7
          82FFDEAF74FFDBAB72FFBD7E4EFF6F3E24B50000001000000002000000085C36
          2095BE8053FFE0B37CFFDFB076FFDEB177FFB78254FFAA7144FFAB7245FFBC88
          59FFDFB279FFDFB277FFDEB077FFC08253FF55321D920000000A190F0932B070
          47FADFB27DFFDFB27AFFE0B37BFFE0B57DFFA56B3FFFF5EFEAFFF8F3EEFFAB72
          45FFE2B67EFFE0B47CFFE0B47BFFDEB079FFB3734AFB130B072F613C2795CD9B
          6FFFE2B780FFE5BD89FFE7C291FFE8C393FFA56B3FFFF1E6DEFFF9F5F1FFAA71
          44FFE8C494FFE8C393FFE5BF8CFFE1B77FFFD09C6EFF5434218B935E3DD2DCB3
          83FFE3B781FFBA8659FFA97043FFAB7245FFAC7346FFF5EDE6FFFAF6F3FFAD75
          47FFB0784AFFB17A4BFFC29162FFE4B983FFDEB17EFF8E5B3BD0B0744CF2E3BF
          8FFFE4BB84FFA56B3FFFF3EBE6FFFAF6F3FFF6EFE8FFF7F0EAFFFBF7F5FFFAF7
          F4FFFAF7F3FFFAF6F2FFAB7245FFE5BD87FFE5BE8BFFAB714CEEAE764FECE9C9
          A0FFE5BE89FFA56B3FFFE0D2CAFFE1D3CCFFE3D5CFFFF2EAE4FFF8F3EFFFEADF
          D9FFE6DAD4FFE9DED9FFAA7144FFE7C08CFFEACA9DFFAE764FEE9A6A49D0E9CD
          ACFFEAC796FFB78456FFA56B3FFFA56B3FFFA56B3FFFF1EAE5FFFAF6F3FFA56B
          3FFFA56B3FFFA56B3FFFB78457FFEACA99FFEBD1ADFF996A49D46E4E3697DDBB
          9DFFEED3A9FFEECFA2FFEED2A5FFF0D6A9FFA56B3FFFF0EAE7FFFDFCFBFFA56B
          3FFFF1D6AAFFF0D5A8FFEED2A5FFEFD4A7FFE0C2A2FF6246318F1C140E2BC794
          6CFCF5E8CCFFEFD6ABFFF1D8AEFFF2DAB0FFA56B3FFFDECFC9FFDFD1CBFFA56B
          3FFFF3DCB2FFF1DBB0FFF1D8ADFFF7EACDFFC69470FA1A120D2E000000036F52
          3C92D7B08CFFF8EFD3FFF3E0B9FFF3DFB7FFB98A5FFFA56B3FFFA56B3FFFBA8A
          5FFFF4E1B9FFF4E2BDFFFAF1D5FFD9B390FF664B368C00000006000000010202
          0107906C4EB8D9B38FFFF7EDD3FFF8EED0FFF7EBC9FFF6E8C4FFF6E8C5FFF7EC
          CAFFF8EED0FFF4E8CDFFD7AF8BFF88664AB30202010B00000001000000000000
          00010202010770543F8FCFA078FCE2C4A2FFEBD7B8FFF4E9CDFFF4EACEFFECD8
          B9FFE3C5A3FFC59973F24C392A67000000060000000100000000000000000000
          000000000001000000022019122C6C543E89A47E5FCCC59770F1C19570EEA47E
          60CD6C543F8B16110D2200000003000000010000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000020000000A00000010000000090000000200000000000000000000
          00020000000A000000120000000C000000030000000000000000000000000000
          00020000000F0F0742921D0F7EEF0603347A0000000E00000002000000020000
          000F0804347C1D0F7EF00F084194000000120000000200000000000000000000
          0008120B47923233AFFF3648CCFF1D1EA5FF0603357A0000000F0000000F0703
          357C1F20A5FF3747CCFF2D2FAEFF120B46950000000B00000000000000000000
          000C281C8DF1596CD8FF3B51D3FF3A4FD2FF1E22A6FF0602347D0502357E2022
          A6FF3A50D3FF3A50D3FF4C5FD4FF291D8CF10000001000000000000000000000
          0006130F3C734D4FBAFF667EE0FF415AD6FF415AD7FF1F24A7FF2529A8FF415A
          D7FF415AD7FF5B72DEFF484AB8FF130F3C790000000900000000000000000000
          00010000000A16123F73585CC1FF758DE6FF4A64DBFF4A65DBFF4A65DBFF4A64
          DBFF6983E3FF5356C0FF16123F780000000C0000000200000000000000000000
          0000000000010000000A191643755D63C7FF6783E5FF5774E2FF5774E2FF5774
          E2FF565CC6FF1916437A0000000D000000020000000000000000000000000000
          00000000000100000009100E3D734A50BEFF7492EBFF6383E7FF6483E7FF6383
          E7FF3840B6FF0B0839780000000C000000020000000000000000000000000000
          0001000000071413416E555CC5FF85A1EFFF7897EDFF9CB6F4FF9DB7F5FF7997
          EEFF7796EDFF414ABCFF0E0C3C730000000A0000000100000000000000000000
          00041818456B636CCFFF93AFF3FF83A1F1FFA6BFF7FF676DCAFF7E87DDFFAFC7
          F8FF83A3F2FF83A1F1FF5058C4FF121040710000000600000000000000000000
          00065759C3EFAFC6F6FF8EADF4FFABC4F8FF6F76D0FF1817456F24244F70868E
          E1FFB5CCF9FF8DACF4FFA1B8F4FF5758C3EF0000000900000000000000000000
          000331326B8695A0EAFFC0D3F9FF7880D7FF1C1C496B00000006000000072527
          526C8B93E6FFC1D3F9FF949EE9FF303168870000000500000000000000000000
          00010000000431336B825E62CBEC1F204D680000000500000001000000010000
          00052728536B5E62CBEC31326883000000070000000100000000000000000000
          0000000000000000000200000004000000020000000100000000000000000000
          0001000000030000000500000004000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000020000000A0000000F00000010000000100000
          000F0000000A0000000200000000000000000000000000000000000000000000
          00000000000000000000000000097E5237C1A8663FFFA7633DFFA6623BFFA560
          3AFF754429C10000000900000000000000000000000000000000000000000000
          000000000000000000000000000CB57852FFE6C69AFFE2BE8EFFE2BE8EFFE2BE
          8DFFA6613CFF0000000D00000000000000000000000000000000000000000000
          000000000000000000000000000CB67B55FFE6C79DFFDAAD73FFD9AD73FFE3C0
          8FFFA8653EFF0000000D00000000000000000000000000000000000000000000
          0002000000080000000B00000013B87F59FFE7C79DFFDBAF77FFDBAE77FFE4C1
          91FFA96842FF000000150000000C0000000A0000000300000000000000000000
          0007B77D56FFB67B54FFB47953FFB37751FFE5C495FFDCB279FFDCB17AFFE4C2
          95FFAB6B46FFAA6A44FFAA6842FFA86640FF0000000800000000000000000000
          00057E573DB5E7D3C4FFEACEA8FFE6C69AFFE5C598FFDDB37DFFDDB37DFFE5C4
          97FFE5C597FFE7C79CFFDDC5B7FF673923B60000000600000000000000000000
          0002140E0A24B78869F2F2E5D2FFE6C799FFE7C997FFEACF9FFFEACF9EFFE7C9
          97FFE3C18FFFEFDFCBFF9D6549F3100906260000000200000000000000000000
          0000000000034E372871D7B7A0FFF5E9CDFFEEDAABFFEDD7A8FFEDD7A8FFEED7
          A8FFF4E5C4FFC7A08CFF41261872000000040000000000000000000000000000
          0000000000010000000590684BC7EEE0D3FFF3E5BFFFF1DFB2FFF1DFB2FFF2E2
          B7FFE8D8CBFF7B4C30C700000006000000010000000000000000000000000000
          000000000000000000011D15102DC4997AFAFBF4E4FFF5E8C0FFF4E6BBFFF9F2
          E0FFB07D5FFA19100A2F00000001000000000000000000000000000000000000
          00000000000000000000000000025F453383DFC5B1FFFBF2D7FFFAF2D4FFD3B6
          A2FF533523830000000300000000000000000000000000000000000000000000
          000000000000000000000000000005030309A37C5CD8F4EBDFFFF1E6DAFF9362
          45D80403020A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000012E231A3FCFAB8EFDC09679FD291C
          1340000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000174584298674731950000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003000000090000000A0000
          0003000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000D552C19A64E2916A40000
          000D000000010000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000623130B54A77053FD9E6443FD2011
          0A54000000060000000000000000000000000000000000000000000000000000
          000000000000000000000000000204020117814A2EDEDFBF9BFFD4AC7EFF7942
          26DF030201180000000200000000000000000000000000000000000000000000
          00000000000000000001000000094D2D1C90C69D7FFFDFB987FFDFB785FFBA87
          5FFF472617910000000A00000001000000000000000000000000000000000000
          00000000000000000004180F0A3AA87252FBE5C89FFFDCB27AFFDCB17BFFE0BC
          8EFF9C6140FB160C073B00000004000000000000000000000000000000000000
          0000000000010000000C7E4E32CCDDBFA0FFDFB783FFDDB37DFFDDB37DFFDFB6
          82FFD2AB80FF714027CD0000000D000000020000000000000000000000000000
          000000000006452C1E78C69D7FFFE4C394FFDEB680FFDFB680FFDEB67FFFDEB5
          7FFFE3BF8FFFB8855FFF3E241679000000070000000000000000000000000000
          0002120C0828AA7758F3E9CEAAFFE4C290FFE5C794FFE8CA98FFE8CA99FFE6C5
          93FFE3C18DFFE2C193FF996141F31009062A0000000300000000000000000000
          0006795339B6E0C7ADFFF9F1E0FFFAF2E2FFFAF2E2FFEBD3A3FFECD2A3FFF1DD
          B7FFF0DDB7FFF9F1E0FFDBBEA6FF6B4028B80000000700000000000000000000
          0006C1906AFFC18E68FFC08C66FFBE8A64FFFBF5E5FFEFDAACFFEFD9ACFFF3E2
          BEFFB9805AFFB77E58FFB77C55FFB57A54FF0000000800000000000000000000
          000200000006000000070000000DC08D68FFFBF7E9FFF2E1B4FFF2E1B4FFF5E9
          C5FFB27753FF0000001000000009000000070000000200000000000000000000
          0000000000000000000000000007C3926CFFFCF8EBFFF5E8BCFFF5E7BCFFF8EE
          CCFFB47C56FF0000000800000000000000000000000000000000000000000000
          0000000000000000000000000005C59570FFFDFAEDFFFDFAEDFFFDFAEDFFFDFA
          EDFFB67F5AFF0000000600000000000000000000000000000000000000000000
          000000000000000000000000000393755CBACDA37EFFCBA17DFFCAA07BFFC99D
          79FF8C6A50BB0000000400000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000400000004000000040000
          0004000000030000000100000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00020000000A0000000F00000011000000110000001200000012000000130000
          00130000001300000014000000130000000D0000000300000000000000000000
          000981594BC1B37B67FFB27B66FFB27A66FFB27864FFB17965FFB17965FFB177
          65FFB17763FFB07664FFB07664FF7E5548C50000000C00000000000000000000
          000DB57D6BFFFDFBF9FFFBF6F2FFFBF5F2FFFAF5F1FFFBF4EFFFF9F3EEFFF9F2
          EEFFFAF2ECFFF8F0EBFFF9F0EAFFB17866FF0000001200000000000000000000
          000DB7816DFFFDFBFAFFF7EEE7FFF8EDE7FFF7EDE7FFF7EDE6FFF6ECE5FFF6EC
          E5FFF8EBE4FFF6EBE4FFF9F1ECFFB37A68FF0000001300000000000000000000
          000DB98472FFFDFCFBFFF8EFE8FFF7EEE8FFF7EEE8FFF8EEE7FFF7EEE7FFF7EC
          E6FFF7ECE5FFF6ECE5FFFAF2EEFFB57D6BFF0000001200000000000000000000
          000CBB8775FFFEFCFBFFF9F0EAFFF8F0EAFFF8EFE9FFF8EFE8FFF8EEE9FFF8EE
          E7FFF8EEE7FFF8EDE7FFFAF4EFFFB7816EFF0000001100000000000000000000
          000BBE8A79FFFEFDFCFFF9F2EDFFF9F2EDFFF9F0EBFFF9F0EAFFF8F0EAFFF8F0
          E9FFF8EFE9FFF8EFE8FFFAF5F1FFBA8571FF0000001000000000000000000000
          000AC08F7EFFFEFDFDFFFAF3EFFFFAF4EEFFFAF3EEFFFAF1ECFFF9F1EBFFF9F0
          EBFFF9F1EBFFF8F0EAFFFBF6F3FFBB8975FF0000000F00000000000000000000
          0009C49380FFFEFEFDFFFBF5F1FFFBF5F0FFFBF4F0FFFAF3EFFFFAF3EFFFF9F3
          EDFFF9F2EDFFF9F1EBFFFCF7F4FFBE8B79FF0000000F00000000000000000000
          0009C69686FFFEFEFDFFFAF5F3FFFBF6F2FFFBF5F1FFFBF5F0FFFBF5F0FFFAF4
          EFFFFAF4EEFFFAF3EFFFFDF9F7FFC18E7DFF0000000E00000000000000000000
          0008C99B8AFFFEFEFEFFFBF6F4FFFBF6F4FFFCF6F3FFFCF6F3FFFCF4F2FFFBF5
          F1FFFBF5F0FFFAF5F0FFFDFAF8FFC39382FF0000000D00000000000000000000
          0007C99E8DFFFFFEFEFFFCF8F6FFFCF7F5FFFCF7F5FFFBF6F4FFFBF6F4FFFCF6
          F3FFFCF6F2FFFBF6F1FFFDFBF9FFC69786FF0000000C00000000000000000000
          0006CEA190FFFFFFFEFFFDF9F7FFFDF9F7FFFCF8F7FFFCF8F6FFFCF7F5FFFBF7
          F5FFFBF7F4FFFCF7F3FFFDFCFAFFC89B8AFF0000000B00000000000000000000
          0006CEA393FFFFFFFFFFFDFAF9FFFDFAF8FFFDFAF8FFFDF9F7FFFCF8F7FFFBF8
          F6FFFBF7F6FFFCF7F5FFFEFCFCFFCB9D8DFF0000000B00000000000000000000
          0005D0A696FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFEFFFEFEFEFFFFFE
          FEFFFEFEFEFFFEFEFEFFFEFEFDFFCDA191FF0000000900000000000000000000
          00039C7C71C0D2A898FFD1A897FFD1A897FFD1A797FFD0A696FFD0A696FFD0A6
          95FFD0A595FFCFA595FFCFA494FF98796EC20000000600000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000030000
          0007000000060000000300000006000000080000000400000001000000000000
          00000000000000000000000000000000000000000000000000030B082C4D2619
          99EA110A58A2010003160F0855A3160C7EEA0603244F00000004000000000000
          0001000000060000000A0000000B0000000B0000000B000000112E23A2EA6F85
          EAFF4150CBFF1F1689E63B48C9FF5C74E4FF180E82E700000007000000000000
          000680574CBDB37B69FFB37A68FFB37A68FFB27968FFDCC6BEFF756BC0FF7780
          DDFF6D8BEFFF5872E5FF6381EDFF6972D8FF1A13659F00000005000000000000
          0008B57D6BFFFBF7F3FFFBF6F3FFFBF6F3FFFBF5F2FFFAF5F3FFEFECEFFF4D46
          BDFF6A85EBFF7494F2FF6079E7FF262094E40101041300000002000000000000
          0008B67F6DFFFCF8F5FFF8EFECFFF7EEEAFFF7EEEAFFF9F5F3FF807DD4FF6B7E
          E2FF93B0F6FFA0B3F2FF8AA6F4FF5D6EDBFF1C186AA000000004000000000000
          0008B8826FFFFCF9F6FFF8F0ECFFF8F0ECFFF7EFECFFFAF8F7FF6261D8FFB1C3
          F6FF8D99EAFF5F5DD2FF8995E7FFA6B8F3FF3B35AEE300000004000000000000
          0007BA8473FFFDF9F8FFF8F1EEFFF8F0EDFFF8F0ECFFFAF5F3FFCECDEEFF6564
          DEFF9291E2FFF2F1F3FF8982D3FF4340BCE71212334600000002000000000000
          0007BB8776FFFDFBF9FFF9F1EFFFF9F2EEFFF8F1EEFFF8F0EDFFFAF5F3FFFAF8
          F7FFFAF7F6FFFCF9F8FFE3CFC9FF0000000C0000000200000000000000000000
          0006BD8A78FFFDFBFAFFF9F2F0FFF9F2F0FFF8F2EFFFF9F1EFFFF8F1EEFFF9F1
          EEFFF8F0EDFFFDFAF8FFBB8675FF000000080000000000000000000000000000
          0006BF8D7BFFFEFCFBFFFAF4F1FFFAF4F1FFFAF2F1FFFAF2F0FFF9F2EFFFF9F2
          EEFFF8F1EEFFFDFBF9FFBD8978FF000000080000000000000000000000000000
          0005C39381FFFEFDFDFFFBF6F4FFFBF5F4FFFBF4F2FFFAF4F2FFFAF4F1FFF9F3
          F1FFFAF3F1FFFEFCFBFFC18F7EFF000000070000000000000000000000000000
          0004C69887FFFFFEFEFFFBF7F6FFFCF6F6FFFBF6F5FFFBF6F4FFFBF5F4FFFAF5
          F3FFFAF5F3FFFEFDFDFFC59684FF000000060000000000000000000000000000
          0003C99B8AFFFFFEFEFFFBF7F6FFFCF7F6FFFCF6F5FFFBF6F5FFFCF6F5FFFBF5
          F5FFFBF6F4FFFFFEFEFFC79887FF000000050000000000000000000000000000
          0003CA9E8DFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFE
          FEFFFFFEFEFFFFFEFEFFC99B8AFF000000040000000000000000000000000000
          00029B7F74BFD0AB9CFFD0AB9CFFD0AA9CFFCFA99BFFCFA99AFFCFA999FFCFA8
          99FFCEA899FFCFA898FF997B71C0000000030000000000000000}
      end>
  end
end
