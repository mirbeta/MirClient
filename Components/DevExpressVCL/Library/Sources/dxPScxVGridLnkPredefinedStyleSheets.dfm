object cxdmPScxVGridLnkPredefinedStyles: TcxdmPScxVGridLnkPredefinedStyles
  OldCreateOrder = False
  Left = 435
  Top = 202
  Height = 150
  Width = 215
  object StyleRepository: TcxStyleRepository
    Left = 63
    Top = 28
    PixelsPerInch = 96
    object styleProfessionalCategory: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object styleProfessionalHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 7566195
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object styleGrayCategory: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6579300
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object styleGrayHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 8559005
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object styleNoneContent: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object styleTransparentCategory: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object styleTransparentHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object styleTransparentContent: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object ssProfessional: TcxVerticalGridReportLinkStyleSheet
      Caption = 'Professional'
      Styles.Category = styleProfessionalCategory
      Styles.Content = styleNoneContent
      Styles.Header = styleProfessionalHeader
      BuiltIn = True
    end
    object ssGray: TcxVerticalGridReportLinkStyleSheet
      Caption = 'Gray'
      Styles.Category = styleGrayCategory
      Styles.Content = styleNoneContent
      Styles.Header = styleGrayHeader
      BuiltIn = True
    end
    object ssTransparent: TcxVerticalGridReportLinkStyleSheet
      Caption = 'Transparent'
      Styles.Category = styleTransparentCategory
      Styles.Content = styleTransparentContent
      Styles.Header = styleTransparentHeader
      BuiltIn = True
    end
  end
end
