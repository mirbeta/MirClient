object dxUIAdornerTargetElementPathEditor: TdxUIAdornerTargetElementPathEditor
  Left = 0
  Top = 0
  Caption = 'Select Target Element'
  ClientHeight = 294
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object tvTargetElements: TTreeView
    Left = 0
    Top = 0
    Width = 307
    Height = 258
    Align = alClient
    Images = ilIcons
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvTargetElementsChange
    OnGetImageIndex = tvTargetElementsGetImageIndex
    OnGetSelectedIndex = tvTargetElementsGetImageIndex
  end
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 307
    Height = 36
    Align = alBottom
    TabOrder = 1
    object btnSelect: TButton
      Left = 142
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Select'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnSelectClick
    end
    object btnCancel: TButton
      Left = 223
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Cancel'
      Default = True
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ilIcons: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 4718672
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000001294944415478DACD523B8A8440109D8BEC41E60246825F3034311841
          5003D1400551CCE60C6A6668A682C730F502EA1D6AFBC922B8D333E9AEF0A8A2
          DEEB576575DF6EFFEE1304E14B96654F92A49EC595C5555114E41EB88F874551
          BCB3039D6DDB5455152DCB7200396AE0A079DB9975EA8220A06DDB68DFF70B50
          030713EE2418D1300C9AA689E679E6021C34D0BE18E03F4DD3A4B66D691CC703
          C3309C1100070DB43C83F5F178509665D434CD81BAAE2F3938EC42D3B4F5C500
          45D775C9F77D8AE398CAB2A4E7F37900396AE0A0E11AA8AADA3B8E4351145D10
          86E1190168A0E51978966551922494A6E9197F031A68B9D7C8880E1D8AA23891
          E7F9197FBA77EC2AF90F8A2DF20E019689717108408E1A38683EBE46B86344B6
          A85ED7F515408EDADBCE7FFA7D038D4A2F1D6D743C310000000049454E44AE42
          6082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA864000001B349444154384FCD52CD
          2B047118DEBF63D7D7FA1C9C10EDEE05658B75526A9B9D1DB97071D82964E520
          C93A986DF3B564B775100EC85C34144939C865B565B7106AA54CD813070EAFF7
          9DFD69C23AF3D4D3F3CCFB3ECFEF374D63FA7F30BB63F916615932F331D5E289
          69163EA6993DE869863B16CB0D331FB5E7F151C5E153606C250E899B0C24AE33
          BA77F8B6C1823BCAB0F857D0E9054244691FD983F4E31BDC3DBDEB4C7F2ACE5C
          B82BE0234ACE37291416A592AE755839D4E028F9827CFDA62FFA8E3294653503
          459EB05AD5BB0381CD7B881C64742EED6735B2FF8CCCC004EEAA7A76C0EA5950
          59CD805598D76A7DC7D0319902FFDA4396AB0F30CCFC107ADAD561C62A843556
          33502CCC69B6C15370F8E3D03A9E824EF91AC4D9B44EF23473F8CF8032259865
          3503A5C28CDAD07F084DA349E439D32434324F4AA44C196659CD40B910926AFA
          36C0397109CEC025B4A0B6042EB29ED189CF942917A77F7EC46AB79C5FE10D29
          B64115DAE45B9D2ED2A9ACBA506D03BBC061A6BA5BCEFD437162C8CE79834A83
          6F0B9A474FB07CA5937CBDB485E5A0421916CF0D3A9D13831227CA6AA557D688
          E469F6EBCD7F0893E90328A740A9A328B10B0000000049454E44AE426082}
      end>
  end
end
