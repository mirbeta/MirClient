{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichEdit.Export.Html.Types;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Rtti, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics, dxCultureInfo,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage;

type

  TdxWebBorderStyle = (
    NotSet,
    None,
    Dotted,
    Dashed,
    Solid,
    Double,
    Groove,
    Ridge,
    Inset,
    Outset);

  TdxWebUnitType = (
    Cm = 6,
    Em = 8,
    Ex = 9,
    Inch = 4,
    Mm = 5,
    Percentage = 7,
    Pica = 3,
    Pixel = 1,
    Point = 2);

  TdxWebControlState = (
    Constructed,
    FrameworkInitialized,
    ChildrenInitialized,
    Initialized,
    ViewStateLoaded,
    Loaded,
    PreRendered);

  TdxHtmlTextWriterTag = (
    Unknown,
    A,
    Acronym,
    Address,
    Area,
    B,
    Base,
    Basefont,
    Bdo,
    Bgsound,
    Big,
    Blockquote,
    Body,
    Br,
    Button,
    Caption,
    Center,
    Cite,
    Code,
    Col,
    Colgroup,
    Dd,
    Del,
    Dfn,
    Dir,
    &Div,
    Dl,
    Dt,
    Em,
    Embed,
    Fieldset,
    Font,
    Form,
    Frame,
    Frameset,
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
    Head,
    Hr,
    Html,
    I,
    Iframe,
    Img,
    Input,
    Ins,
    Isindex,
    Kbd,
    &Label,
    Legend,
    Li,
    Link,
    Map,
    Marquee,
    Menu,
    Meta,
    Nobr,
    Noframes,
    Noscript,
    &Object,
    Ol,
    Option,
    P,
    Param,
    Pre,
    Q,
    Rt,
    Ruby,
    S,
    Samp,
    Script,
    Select,
    Small,
    Span,
    Strike,
    Strong,
    Style,
    Sub,
    Sup,
    Table,
    Tbody,
    Td,
    Textarea,
    Tfoot,
    Th,
    Thead,
    Title,
    Tr,
    Tt,
    U,
    Ul,
    &Var,
    Wbr,
    Xml);

  TdxHtmlTextWriterAttribute = (
    Invalid,

    Accesskey,
    Align,
    Alt,
    Background,
    Bgcolor,
    Border,
    Bordercolor,
    Cellpadding,
    Cellspacing,
    Checked,
    &Class,
    Cols,
    Colspan,
    Disabled,
    &For,
    Height,
    Href,
    Id,
    Maxlength,
    Multiple,
    Name,
    Nowrap,
    Onchange,
    Onclick,
    ReadOnly,
    Rows,
    Rowspan,
    Rules,
    Selected,
    Size,
    Src,
    Style,
    Tabindex,
    Target,
    Title,
    &Type,
    Valign,
    Value,
    Width,
    Wrap,
    Abbr,
    AutoComplete,
    Axis,
    Content,
    Coords,
    DesignerRegion,
    Dir,
    Headers,
    Longdesc,
    Rel,
    Scope,
    Shape,
    Line,
    Usemap,
    VCardName);

  TdxHtmlTextWriterStyle = (
    Invalid,

    BackgroundColor,
    BackgroundImage,
    BorderCollapse,
    BorderColor,
    BorderStyle,
    BorderWidth,
    Color,
    FontFamily,
    FontSize,
    FontStyle,
    FontWeight,
    Height,
    TextDecoration,
    Width,
    ListStyleImage,
    ListStyleType,
    Cursor,
    Direction,
    Display,
    Filter,
    FontVariant,
    Left,
    Margin,
    MarginBottom,
    MarginLeft,
    MarginRight,
    MarginTop,
    Overflow,
    OverflowX,
    OverflowY,
    Padding,
    PaddingBottom,
    PaddingLeft,
    PaddingRight,
    PaddingTop,
    Position,
    TextAlign,
    VerticalAlign,
    TextOverflow,
    Top,
    Visibility,
    WhiteSpace,
    ZIndex);

  TdxWebFontSize = (
    NotSet,
    AsUnit,
    Smaller,
    Larger,
    XXSmall,
    XSmall,
    Small,
    Medium,
    Large,
    XLarge,
    XXLarge
  );

  TdxWebHorizontalAlign = (
    NotSet,
    Left,
    Center,
    Right,
    Justify
  );


  TdxWebRenderStyle = record
    Name: string;
    Value: string;
    Key: TdxHtmlTextWriterStyle;
  end;

  IdxScriptContainer = interface
  ['{4BE00F5A-6FFB-45E0-8059-FA195B275F67}']
    function IsClientScriptBlockRegistered(const AKey: string): Boolean;
    procedure RegisterClientScriptBlock(const AKey, AScript: string);
    function RegisterCssClass(const AStyle: string): string;
    procedure RegisterCommonCssStyle(const AStyle, ATagName: string);
  end;

  IdxOfficeImageRepository = interface
    function GetImageSource(AImg: TdxOfficeImageReference): string;
  end;

implementation

end.
