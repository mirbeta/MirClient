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

unit dxRichEdit.Api.Formatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles;

type
  { TdxNativeCharacterPropertiesBase }

  TdxNativeCharacterPropertiesBase = class(TInterfacedObject, IdxRichEditCharacterPropertiesBase)
  strict private
    function GetAllCaps: TdxNullableBoolean;
    function GetBackColor: TdxNullableValue<TdxAlphaColor>;
    function GetFontSize: TdxNullableSingle;
    function GetBold: TdxNullableBoolean;
    function GetForeColor: TdxNullableValue<TdxAlphaColor>;
    function GetItalic: TdxNullableBoolean;
    function GetFontName: TdxNullableString;
    function GetUnderline: TdxNullableValue<TdxRichEditUnderlineType>;
    function GetStrikeout: TdxNullableValue<TdxRichEditStrikeoutType>;
    function GetHidden: TdxNullableBoolean;
    function GetSubscript: TdxNullableBoolean;
    function GetSuperscript: TdxNullableBoolean;
    function GetStrikeoutColor: TdxNullableValue<TdxAlphaColor>;
    function GetUnderlineColor: TdxNullableValue<TdxAlphaColor>;

    procedure SetAllCaps(const Value: TdxNullableBoolean);
    procedure SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetFontSize(const Value: TdxNullableSingle);
    procedure SetBold(const Value: TdxNullableBoolean);
    procedure SetForeColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetItalic(const Value: TdxNullableBoolean);
    procedure SetFontName(const Value: TdxNullableString);
    procedure SetUnderline(const Value: TdxNullableValue<TdxRichEditUnderlineType>);
    procedure SetStrikeout(const Value: TdxNullableValue<TdxRichEditStrikeoutType>);
    procedure SetHidden(const Value: TdxNullableBoolean);
    procedure SetSubscript(const Value: TdxNullableBoolean);
    procedure SetSuperscript(const Value: TdxNullableBoolean);
    procedure SetStrikeoutColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetUnderlineColor(const Value: TdxNullableValue<TdxAlphaColor>);
  protected
    function GetInfo: TdxCharacterFormattingInfo; virtual; abstract;
    function GetOptions: PdxCharacterFormattingOptions; virtual; abstract;
    function GetResetOptions: PdxCharacterFormattingOptions; virtual;

    procedure SetAllCapsCore(const Value: Boolean); virtual;
    procedure SetBackColorCore(const Value: TdxAlphaColor); virtual;
    procedure SetDoubleFontSizeCore(const Value: Integer); virtual;
    procedure SetBoldCore(const Value: Boolean); virtual;
    procedure SetForeColorCore(const Value: TdxAlphaColor); virtual;
    procedure SetItalicCore(const Value: Boolean); virtual;
    procedure SetFontNameCore(const Value: string); virtual;
    procedure SetStrikeoutCore(const Value: TdxRichEditStrikeoutType); virtual;
    procedure SetHiddenCore(const Value: Boolean); virtual;
    procedure SetScriptCore(const Value: TdxCharacterFormattingScript); virtual;
    procedure SetStrikeoutColorCore(const Value: TdxAlphaColor); virtual;
    procedure SetUnderlineCore(const Value: TdxRichEditUnderlineType); virtual;
    procedure SetUnderlineColorCore(const Value: TdxAlphaColor); virtual;

    procedure Reset; overload; virtual;
    procedure Reset(const AMask: TdxRichEditCharacterPropertiesMask); overload; virtual;

    property Info: TdxCharacterFormattingInfo read GetInfo;
    property Options: PdxCharacterFormattingOptions read GetOptions;
    property ResetOptions: PdxCharacterFormattingOptions read GetResetOptions;
  end;

  { TdxNativeSimpleCharacterProperties }

  TdxNativeSimpleCharacterProperties = class(TdxNativeCharacterPropertiesBase)
  strict private
    FProperties: TdxCharacterProperties;
  protected
    procedure SetAllCapsCore(const Value: Boolean); override;
    procedure SetBackColorCore(const Value: TdxAlphaColor); override;
    procedure SetDoubleFontSizeCore(const Value: Integer); override;
    procedure SetBoldCore(const Value: Boolean); override;
    procedure SetForeColorCore(const Value: TdxAlphaColor); override;
    procedure SetItalicCore(const Value: Boolean); override;
    procedure SetFontNameCore(const Value: string); override;
    procedure SetStrikeoutCore(const Value: TdxRichEditStrikeoutType); override;
    procedure SetHiddenCore(const Value: Boolean); override;
    procedure SetScriptCore(const Value: TdxCharacterFormattingScript); override;
    procedure SetStrikeoutColorCore(const Value: TdxAlphaColor); override;
    procedure SetUnderlineCore(const Value: TdxRichEditUnderlineType); override;
    procedure SetUnderlineColorCore(const Value: TdxAlphaColor); override;

    function GetInfo: TdxCharacterFormattingInfo; override;
    function GetOptions: PdxCharacterFormattingOptions; override;
  public
    constructor Create(AProperties: TdxCharacterProperties); reintroduce;
  end;

  { TdxNativeCharacterProperties }

  TdxNativeCharacterProperties = class(TdxNativeCharacterPropertiesBase,
    IdxRichEditCharacterProperties{, IdxDocumentModelModifier})
  strict private
    FDocument: TdxNativeSubDocument;
    FFrom: TdxDocumentLogPosition;
    FLength: Integer;
    FProperties: TdxMergedCharacterProperties;
    FResetOptions: TdxCharacterFormattingOptions;
    function GetDocumentModel: TdxDocumentModel;
    function GetStyle: IdxRichEditCharacterStyle;
    procedure SetStyle(const Value: IdxRichEditCharacterStyle);
  protected
    function GetInfo: TdxCharacterFormattingInfo; override;
    function GetOptions: PdxCharacterFormattingOptions; override;
    function GetResetOptions: PdxCharacterFormattingOptions; override;

    function CreateProperties: TdxMergedCharacterProperties;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      AFrom: TdxDocumentLogPosition; ALength: Integer); reintroduce;
    destructor Destroy; override;

    procedure Apply;
  end;

  { TdxNativeCharacterStyle }

  TdxNativeCharacterStyle = class(TInterfacedObject, IdxRichEditCharacterStyle)
  strict private
    FDocument: TdxNativeDocument;
    FInnerStyle: TdxCharacterStyle;
    FCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetDocumentModel: TdxDocumentModel;
    function GetIsDeleted: Boolean;
    function GetLinkedStyle: IdxRichEditParagraphStyle;
    function GetName: string;
    function GetParent: IdxRichEditCharacterStyle;
    function GetPrimary: Boolean;
    procedure SetLinkedStyle(const Value: IdxRichEditParagraphStyle);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: IdxRichEditCharacterStyle);
    procedure SetPrimary(const Value: Boolean);

    function GetAllCaps: TdxNullableBoolean;
    function GetBackColor: TdxNullableValue<TdxAlphaColor>;
    function GetFontSize: TdxNullableSingle;
    function GetBold: TdxNullableBoolean;
    function GetForeColor: TdxNullableValue<TdxAlphaColor>;
    function GetItalic: TdxNullableBoolean;
    function GetFontName: TdxNullableString;
    function GetUnderline: TdxNullableValue<TdxRichEditUnderlineType>;
    function GetStrikeout: TdxNullableValue<TdxRichEditStrikeoutType>;
    function GetStrikeoutColor: TdxNullableValue<TdxAlphaColor>;
    function GetHidden: TdxNullableBoolean;
    function GetSubscript: TdxNullableBoolean;
    function GetSuperscript: TdxNullableBoolean;
    function GetUnderlineColor: TdxNullableValue<TdxAlphaColor>;
    procedure SetAllCaps(const Value: TdxNullableBoolean);
    procedure SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetFontSize(const Value: TdxNullableSingle);
    procedure SetBold(const Value: TdxNullableBoolean);
    procedure SetForeColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetItalic(const Value: TdxNullableBoolean);
    procedure SetFontName(const Value: TdxNullableString);
    procedure SetUnderline(const Value: TdxNullableValue<TdxRichEditUnderlineType>);
    procedure SetStrikeout(const Value: TdxNullableValue<TdxRichEditStrikeoutType>);
    procedure SetStrikeoutColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetHidden(const Value: TdxNullableBoolean);
    procedure SetSubscript(const Value: TdxNullableBoolean);
    procedure SetSuperscript(const Value: TdxNullableBoolean);
    procedure SetUnderlineColor(const Value: TdxNullableValue<TdxAlphaColor>);
  protected
    property Document: TdxNativeDocument read FDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ADocument: TdxNativeDocument; AInnerStyle: TdxCharacterStyle); reintroduce;

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditCharacterPropertiesMask); overload;

    property InnerStyle: TdxCharacterStyle read FInnerStyle;
  end;

  { TdxNativeParagraphPropertiesBase }

  TdxNativeParagraphPropertiesBase = class abstract(TInterfacedObject, IdxRichEditParagraphPropertiesBase)
  strict private
    FDocument: TdxNativeSubDocument;
    function GetAlignment: TdxNullableValue<TdxRichEditParagraphAlignment>;
    function GetBackColor: TdxNullableValue<TdxAlphaColor>;
    function GetContextualSpacing: TdxNullableBoolean;
    function GetFirstLineIndent: TdxNullableSingle;
    function GetFirstLineIndentType: TdxNullableValue<TdxRichEditParagraphFirstLineIndent>;
    function GetKeepLinesTogether: TdxNullableBoolean;
    function GetLeftIndent: TdxNullableSingle;
    function GetLineSpacing: TdxNullableSingle;
    function GetLineSpacingMultiplier: TdxNullableSingle;
    function GetLineSpacingRule: TdxNullableValue<TdxRichEditParagraphLineSpacing>;
    function GetOutlineLevel: TdxNullableInteger;
    function GetPageBreakBefore: TdxNullableBoolean;
    function GetRightIndent: TdxNullableSingle;
    function GetSpacingAfter: TdxNullableSingle;
    function GetSpacingBefore: TdxNullableSingle;
    function GetSuppressHyphenation: TdxNullableBoolean;
    function GetSuppressLineNumbers: TdxNullableBoolean;
    procedure SetAlignment(const Value: TdxNullableValue<TdxRichEditParagraphAlignment>);
    procedure SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetContextualSpacing(const Value: TdxNullableBoolean);
    procedure SetFirstLineIndent(const Value: TdxNullableSingle);
    procedure SetFirstLineIndentType(const Value: TdxNullableValue<TdxRichEditParagraphFirstLineIndent>);
    procedure SetKeepLinesTogether(const Value: TdxNullableBoolean);
    procedure SetLeftIndent(const Value: TdxNullableSingle);
    procedure SetLineSpacing(const Value: TdxNullableSingle);
    procedure SetLineSpacingMultiplier(const Value: TdxNullableSingle);
    procedure SetLineSpacingRule(const Value: TdxNullableValue<TdxRichEditParagraphLineSpacing>);
    procedure SetOutlineLevel(const Value: TdxNullableInteger);
    procedure SetPageBreakBefore(const Value: TdxNullableBoolean);
    procedure SetRightIndent(const Value: TdxNullableSingle);
    procedure SetSpacingAfter(const Value: TdxNullableSingle);
    procedure SetSpacingBefore(const Value: TdxNullableSingle);
    procedure SetSuppressHyphenation(const Value: TdxNullableBoolean);
    procedure SetSuppressLineNumbers(const Value: TdxNullableBoolean);
  protected
    function GetInfo: TdxParagraphFormattingInfo; virtual; abstract;
    function GetOptions: PdxParagraphFormattingOptions; virtual; abstract;
    function GetResetOptions: PdxParagraphFormattingOptions; virtual;

    procedure SetAlignmentCore(const Value: TdxRichEditParagraphAlignment); virtual;
    procedure SetBackColorCore(const Value: TdxAlphaColor); virtual;
    procedure SetContextualSpacingCore(const Value: Boolean); virtual;
    procedure SetFirstLineIndentCore(const Value: Integer); virtual;
    procedure SetFirstLineIndentTypeCore(const Value: TdxRichEditParagraphFirstLineIndent); virtual;
    procedure SetKeepLinesTogetherCore(const Value: Boolean); virtual;
    procedure SetLeftIndentCore(const Value: Integer); virtual;
    procedure SetLineSpacingCore(const Value: Single); virtual;
    procedure SetLineSpacingRuleCore(const Value: TdxRichEditParagraphLineSpacing); virtual;
    procedure SetOutlineLevelCore(const Value: Integer); virtual;
    procedure SetPageBreakBeforeCore(const Value: Boolean); virtual;
    procedure SetRightIndentCore(const Value: Integer); virtual;
    procedure SetSpacingAfterCore(const Value: Integer); virtual;
    procedure SetSpacingBeforeCore(const Value: Integer); virtual;
    procedure SetSuppressHyphenationCore(const Value: Boolean); virtual;
    procedure SetSuppressLineNumbersCore(const Value: Boolean); virtual;

    procedure Reset; overload; virtual;
    procedure Reset(const AMask: TdxRichEditParagraphPropertiesMask); overload; virtual;

    property Document: TdxNativeSubDocument read FDocument;
    property Info: TdxParagraphFormattingInfo read GetInfo;
    property Options: PdxParagraphFormattingOptions read GetOptions;
    property ResetOptions: PdxParagraphFormattingOptions read GetResetOptions;
  public
    constructor Create(ADocument: TdxNativeSubDocument);
  end;

  { TdxNativeSimpleParagraphProperties }

  TdxNativeSimpleParagraphProperties = class(TdxNativeParagraphPropertiesBase)
  strict private
    FModelParagraphProperties: TdxParagraphProperties;
  protected
    function GetInfo: TdxParagraphFormattingInfo; override;
    function GetOptions: PdxParagraphFormattingOptions; override;

    procedure SetAlignmentCore(const Value: TdxRichEditParagraphAlignment); override;
    procedure SetBackColorCore(const Value: TdxAlphaColor); override;
    procedure SetContextualSpacingCore(const Value: Boolean); override;
    procedure SetFirstLineIndentCore(const Value: Integer); override;
    procedure SetFirstLineIndentTypeCore(const Value: TdxRichEditParagraphFirstLineIndent); override;
    procedure SetKeepLinesTogetherCore(const Value: Boolean); override;
    procedure SetLeftIndentCore(const Value: Integer); override;
    procedure SetLineSpacingCore(const Value: Single); override;
    procedure SetLineSpacingRuleCore(const Value: TdxRichEditParagraphLineSpacing); override;
    procedure SetOutlineLevelCore(const Value: Integer); override;
    procedure SetPageBreakBeforeCore(const Value: Boolean); override;
    procedure SetRightIndentCore(const Value: Integer); override;
    procedure SetSpacingAfterCore(const Value: Integer); override;
    procedure SetSpacingBeforeCore(const Value: Integer); override;
    procedure SetSuppressHyphenationCore(const Value: Boolean); override;
    procedure SetSuppressLineNumbersCore(const Value: Boolean); override;
  public
    constructor Create(ADocument: TdxNativeDocument;
      AParagraphProperties: TdxParagraphProperties); reintroduce;
  end;

  { TdxNativeStyleParagraphProperties }

  TdxNativeStyleParagraphProperties = class(TdxNativeSimpleParagraphProperties,
    IdxRichEditParagraphProperties,
    IdxRichEditParagraphPropertiesBase)
  strict private
    FTabs: TdxTabProperties;
  protected
    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);
  public
    constructor Create(ADocument: TdxNativeDocument;
      ATabs: TdxTabProperties; AParagraphProperties: TdxParagraphProperties); reintroduce;
  end;

  { TdxNativeParagraphStyle }

  TdxNativeParagraphStyle = class(TInterfacedObject, IdxRichEditParagraphStyle)
  strict private
    FDocument: TdxNativeDocument;
    FInnerStyle: TdxParagraphStyle;
    FParagraphProperties: IdxRichEditParagraphProperties;
    FCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetDocumentModel: TdxDocumentModel;

    function GetIsDeleted: Boolean;
    function GetLinkedStyle: IdxRichEditCharacterStyle;
    function GetListLevelIndex: Integer;
    function GetName: string;
    function GetNextStyle: IdxRichEditParagraphStyle;
    function GetNumberingListIndex: Integer;
    function GetParent: IdxRichEditParagraphStyle;
    function GetPrimary: Boolean;
    procedure SetLinkedStyle(const Value: IdxRichEditCharacterStyle);
    procedure SetListLevelIndex(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetNextStyle(const Value: IdxRichEditParagraphStyle);
    procedure SetNumberingListIndex(const Value: Integer);
    procedure SetParent(const Value: IdxRichEditParagraphStyle);
    procedure SetPrimary(const Value: Boolean);

    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);

    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
  protected
    property Document: TdxNativeDocument read FDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ADocument: TdxNativeDocument; AInnerStyle: TdxParagraphStyle);

    procedure ValidateStyleProperties;
    procedure ValidateNumberingListIndex(AValue: Integer);

    property InnerStyle: TdxParagraphStyle read FInnerStyle;
    property NumberingListIndex: Integer read GetNumberingListIndex write SetNumberingListIndex;
  end;

  { TdxNativeParagraphProperties }

  TdxNativeParagraphProperties = class(TdxNativeParagraphPropertiesBase, IdxRichEditParagraphProperties)
  private
    FResetOptions: TdxParagraphFormattingOptions;
    FProperties: TdxMergedParagraphProperties;
    FFrom: Integer;
    FLength: Integer;
  protected
    function GetInfo: TdxParagraphFormattingInfo; override;
    function GetOptions: PdxParagraphFormattingOptions; override;
    function GetResetOptions: PdxParagraphFormattingOptions; override;

    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);

    function CreateProperties: TdxMergedParagraphProperties;

    property From: Integer read FFrom;
    property Length: Integer read FLength;
  public
    constructor Create(ADocument: TdxNativeSubDocument; AFrom, ALength: Integer); reintroduce;
    destructor Destroy; override;

    procedure Apply;

    class function CreateTabInfoCollection(ADocument: TdxNativeSubDocument; ATabs: TdxTabFormattingInfo): IdxRichEditTabInfoCollection; static;
    class function CreateModelTabInfoCollection(ADocument: TdxNativeSubDocument; const ATabs: IdxRichEditTabInfoCollection): TdxTabFormattingInfo; static;
    class function CreateTabInfo(ADocument: TdxNativeSubDocument; const ATabInfo: TdxTabInfo): IdxRichEditTabInfo; static;
    class function CreateModelTabInfo(ADocument: TdxNativeSubDocument; const ATabInfo: IdxRichEditTabInfo): TdxTabInfo; static;
  end;

  { TdxNativeTabInfoCollection }

  TdxNativeTabInfoCollection = class(TdxIUnknownList<IdxRichEditTabInfo>, IdxRichEditTabInfoCollection)
  strict private
    function GetCount: Integer;
  public
    function CreateNew: IdxRichEditTabInfo;
  end;

  { TdxNativeTabInfo }

  TdxNativeTabInfo = class(TInterfacedObject, IdxRichEditTabInfo)
  strict private
    FAlignment: TdxRichEditTabAlignmentType;
    FDeleted: Boolean;
    FLeader: TdxRichEditTabLeaderType;
    FPosition: Single;
    function GetAlignment: TdxRichEditTabAlignmentType;
    function GetDeleted: Boolean;
    function GetLeader: TdxRichEditTabLeaderType;
    function GetPosition: Single;
    procedure SetAlignment(const Value: TdxRichEditTabAlignmentType);
    procedure SetDeleted(const Value: Boolean);
    procedure SetLeader(const Value: TdxRichEditTabLeaderType);
    procedure SetPosition(const Value: Single);
  end;

  { TdxNativeTableCellPropertiesBase }

  TdxNativeTableCellPropertiesBase = class abstract(TInterfacedObject, IdxRichEditTableCellPropertiesBase)
  public const
    VerticalAlignmentMap: array [TdxVerticalAlignment] of TdxRichEditTableCellVerticalAlignment =
      (TdxRichEditTableCellVerticalAlignment.Top, TdxRichEditTableCellVerticalAlignment.Top,
      TdxRichEditTableCellVerticalAlignment.Center, TdxRichEditTableCellVerticalAlignment.Bottom);
    ModelVerticalAlignmentMap: array[TdxRichEditTableCellVerticalAlignment] of TdxVerticalAlignment =
      (TdxVerticalAlignment.Top, TdxVerticalAlignment.Center, TdxVerticalAlignment.Bottom);
  strict private
    FDocument: TdxNativeDocument;
    FTableCellBorders: IdxRichEditTableCellBorders;
    function GetCellBackgroundColor: TdxNullableValue<TdxAlphaColor>;
    function GetCellBottomPadding: TdxNullableSingle;
    function GetCellLeftPadding: TdxNullableSingle;
    function GetCellRightPadding: TdxNullableSingle;
    function GetCellTopPadding: TdxNullableSingle;
    function GetNoWrap: TdxNullableBoolean;
    function GetTableCellBorders: IdxRichEditTableCellBorders;
    function GetVerticalAlignment: TdxNullableValue<TdxRichEditTableCellVerticalAlignment>;
    procedure SetCellBackgroundColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetCellBottomPadding(const Value: TdxNullableSingle);
    procedure SetCellLeftPadding(const Value: TdxNullableSingle);
    procedure SetCellRightPadding(const Value: TdxNullableSingle);
    procedure SetCellTopPadding(const Value: TdxNullableSingle);
    procedure SetNoWrap(const Value: TdxNullableBoolean);
    procedure SetVerticalAlignment(const Value: TdxNullableValue<TdxRichEditTableCellVerticalAlignment>);
  protected
    function CreateTableCellBorders: IdxRichEditTableCellBorders; virtual; abstract;
    function GetProperties: TdxTableCellProperties; virtual; abstract;

    property Document: TdxNativeDocument read FDocument;
    property Properties: TdxTableCellProperties read GetProperties;
  public
    constructor Create(ADocument: TdxNativeDocument);

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditTableCellPropertiesMask); overload;

    property CellBackgroundColor: TdxNullableValue<TdxAlphaColor> read GetCellBackgroundColor write SetCellBackgroundColor;
    property CellBottomPadding: TdxNullableSingle read GetCellBottomPadding write SetCellBottomPadding;
    property CellLeftPadding: TdxNullableSingle read GetCellLeftPadding write SetCellLeftPadding;
    property CellRightPadding: TdxNullableSingle read GetCellRightPadding write SetCellRightPadding;
    property CellTopPadding: TdxNullableSingle read GetCellTopPadding write SetCellTopPadding;
    property NoWrap: TdxNullableBoolean read GetNoWrap write SetNoWrap;
    property TableCellBorders: IdxRichEditTableCellBorders read GetTableCellBorders;
    property VerticalAlignment: TdxNullableValue<TdxRichEditTableCellVerticalAlignment> read GetVerticalAlignment write SetVerticalAlignment;
  end;

  { TdxNativeBorderBase }

  TdxNativeBorderBase = class abstract(TInterfacedObject)
  strict private
    FDocument: TdxNativeSubDocument;
  protected
    function GetLineColorCore: TdxAlphaColor; virtual; abstract;
    function GetLineStyleCore: TdxBorderLineStyle; virtual; abstract;
    function GetLineThicknessCore: Integer; virtual; abstract;
    procedure SetLineColorCore(const Value: TdxAlphaColor); virtual; abstract;
    procedure SetLineStyleCore(const Value: TdxBorderLineStyle); virtual; abstract;
    procedure SetLineThicknessCore(const Value: Integer); virtual; abstract;

    function GetLineColor: TdxAlphaColor;
    function GetLineStyle: TdxRichEditTableBorderLineStyle;
    function GetLineThickness: Single;
    procedure SetLineColor(const Value: TdxAlphaColor);
    procedure SetLineStyle(const Value: TdxRichEditTableBorderLineStyle);
    procedure SetLineThickness(const Value: Single);
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property LineStyle: TdxRichEditTableBorderLineStyle read GetLineStyle write SetLineStyle;
    property LineThickness: Single read GetLineThickness write SetLineThickness;
  end;

  { TdxNativeStyleBorderBase }

  TdxNativeStyleBorderBase = class abstract(TdxNativeBorderBase)
  strict private
    FBorder: TdxBorderBase;
  protected
    function GetLineColorCore: TdxAlphaColor; override;
    function GetLineStyleCore: TdxBorderLineStyle; override;
    function GetLineThicknessCore: Integer; override;
    procedure SetLineColorCore(const Value: TdxAlphaColor); override;
    procedure SetLineStyleCore(const Value: TdxBorderLineStyle); override;
    procedure SetLineThicknessCore(const Value: Integer); override;
  public
    constructor Create(ADocument: TdxNativeSubDocument; ABorder: TdxBorderBase); reintroduce;

    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property LineStyle: TdxRichEditTableBorderLineStyle read GetLineStyle write SetLineStyle;
    property LineThickness: Single read GetLineThickness write SetLineThickness;
  end;

  { TdxNativeStyleTableCellBorder }

  TdxNativeStyleTableCellBorder = class(TdxNativeStyleBorderBase, IdxRichEditTableCellBorder);

  { TdxNativeStyleTableBorder }

  TdxNativeStyleTableBorder = class(TdxNativeStyleBorderBase, IdxRichEditTableBorder);

  { TdxNativeTableCellBordersBase }

  TdxNativeTableCellBordersBase = class abstract(TInterfacedObject, IdxRichEditTableCellBorders)
  strict private
    FDocument: TdxNativeSubDocument;

    FBottom: IdxRichEditTableCellBorder;
    FLeft: IdxRichEditTableCellBorder;
    FRight: IdxRichEditTableCellBorder;
    FTop: IdxRichEditTableCellBorder;
  protected
    function CreateBottom: IdxRichEditTableCellBorder; virtual; abstract;
    function CreateLeft: IdxRichEditTableCellBorder; virtual; abstract;
    function CreateRight: IdxRichEditTableCellBorder; virtual; abstract;
    function CreateTop: IdxRichEditTableCellBorder; virtual; abstract;
    function GetBottom: IdxRichEditTableCellBorder;
    function GetLeft: IdxRichEditTableCellBorder;
    function GetRight: IdxRichEditTableCellBorder;
    function GetTop: IdxRichEditTableCellBorder;

    property Document: TdxNativeSubDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    property Bottom: IdxRichEditTableCellBorder read GetBottom;
    property Left: IdxRichEditTableCellBorder read GetLeft;
    property Right: IdxRichEditTableCellBorder read GetRight;
    property Top: IdxRichEditTableCellBorder read GetTop;
  end;

  { TdxNativeStyleTableCellBorders }

  TdxNativeStyleTableCellBorders = class(TdxNativeTableCellBordersBase)
  strict private
    FBorders: TdxTableCellBorders;
  protected
    function CreateBottom: IdxRichEditTableCellBorder; override;
    function CreateLeft: IdxRichEditTableCellBorder;  override;
    function CreateRight: IdxRichEditTableCellBorder;  override;
    function CreateTop: IdxRichEditTableCellBorder; override;

    property Borders: TdxTableCellBorders read FBorders;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      ABorders: TdxTableCellBorders); reintroduce;
  end;

  { TdxNativeStyleTableCellProperties }

  TdxNativeStyleTableCellProperties = class(TdxNativeTableCellPropertiesBase)
  strict private
    FProperties: TdxTableCellProperties;
  protected
    function CreateTableCellBorders: IdxRichEditTableCellBorders; override;
    function GetProperties: TdxTableCellProperties; override;
  public
    constructor Create(ADocument: TdxNativeDocument; AProperties: TdxTableCellProperties); reintroduce;
  end;

  { TdxNativeTableBordersBase }

  TdxNativeTableBordersBase = class abstract(TInterfacedObject, IdxRichEditTableBorders)
  strict private
    FDocument: TdxNativeSubDocument;

    FBottom: IdxRichEditTableBorder;
    FInsideHorizontalBorder: IdxRichEditTableBorder;
    FInsideVerticalBorder: IdxRichEditTableBorder;
    FLeft: IdxRichEditTableBorder;
    FRight: IdxRichEditTableBorder;
    FTop: IdxRichEditTableBorder;
  protected
    function CreateBottom: IdxRichEditTableBorder; virtual; abstract;
    function CreateInsideHorizontalBorder: IdxRichEditTableBorder; virtual; abstract;
    function CreateInsideVerticalBorder: IdxRichEditTableBorder; virtual; abstract;
    function CreateLeft: IdxRichEditTableBorder; virtual; abstract;
    function CreateRight: IdxRichEditTableBorder; virtual; abstract;
    function CreateTop: IdxRichEditTableBorder; virtual; abstract;

    function GetBottom: IdxRichEditTableBorder;
    function GetInsideHorizontalBorder: IdxRichEditTableBorder;
    function GetInsideVerticalBorder: IdxRichEditTableBorder;
    function GetLeft: IdxRichEditTableBorder;
    function GetRight: IdxRichEditTableBorder;
    function GetTop: IdxRichEditTableBorder;

    property Document: TdxNativeSubDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeSubDocument);
    property Bottom: IdxRichEditTableBorder read GetBottom;
    property InsideHorizontalBorder: IdxRichEditTableBorder read GetInsideHorizontalBorder;
    property InsideVerticalBorder: IdxRichEditTableBorder read GetInsideVerticalBorder;
    property Left: IdxRichEditTableBorder read GetLeft;
    property Right: IdxRichEditTableBorder read GetRight;
    property Top: IdxRichEditTableBorder read GetTop;
  end;

  { TdxNativeStyleTableBorders }

  TdxNativeStyleTableBorders = class(TdxNativeTableBordersBase)
  strict private
    FBorders: TdxTableBorders;
  protected
    function CreateBottom: IdxRichEditTableBorder; override;
    function CreateInsideHorizontalBorder: IdxRichEditTableBorder; override;
    function CreateInsideVerticalBorder: IdxRichEditTableBorder; override;
    function CreateLeft: IdxRichEditTableBorder; override;
    function CreateRight: IdxRichEditTableBorder; override;
    function CreateTop: IdxRichEditTableBorder; override;

    property Borders: TdxTableBorders read FBorders;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      ABorders: TdxTableBorders); reintroduce;
  end;

  { TdxNativeTablePropertiesBase }

  TdxNativeTablePropertiesBase = class abstract(TInterfacedObject, IdxRichEditTablePropertiesBase)
  strict private
    FDocument: TdxNativeDocument;
    FTableBorders: IdxRichEditTableBorders;
    function GetBottomPadding: TdxNullableSingle;
    function GetLeftPadding: TdxNullableSingle;
    function GetRightPadding: TdxNullableSingle;
    function GetTableAlignment: TdxNullableValue<TdxRichEditTableRowAlignment>;
    function GetTableBackgroundColor: TdxNullableValue<TdxAlphaColor>;
    function GetTableBorders: IdxRichEditTableBorders;
    function GetTableCellSpacing: TdxNullableSingle;
    function GetTableLayout: TdxNullableValue<TdxRichEditTableLayoutType>;
    function GetTopPadding: TdxNullableSingle;
    procedure SetBottomPadding(const Value: TdxNullableSingle);
    procedure SetLeftPadding(const Value: TdxNullableSingle);
    procedure SetRightPadding(const Value: TdxNullableSingle);
    procedure SetTableAlignment(const Value: TdxNullableValue<TdxRichEditTableRowAlignment>);
    procedure SetTableBackgroundColor(const Value: TdxNullableValue<TdxAlphaColor>);
    procedure SetTableCellSpacing(const Value: TdxNullableSingle);
    procedure SetTableLayout(const Value: TdxNullableValue<TdxRichEditTableLayoutType>);
    procedure SetTopPadding(const Value: TdxNullableSingle);
  protected
    function CreateNativeStyleTableBorders: IdxRichEditTableBorders; virtual; abstract;
    function GetProperties: TdxTableProperties; virtual; abstract;

    property Document: TdxNativeDocument read FDocument;
    property Properties: TdxTableProperties read GetProperties;
  public
    constructor Create(ADocument: TdxNativeDocument);

    procedure Reset; overload; virtual;
    procedure Reset(const AMask: TdxRichEditTablePropertiesMask); overload; virtual;

    class function ApiMaskToModelMask(const AMask: TdxRichEditTablePropertiesMask): Integer; static;

    property BottomPadding: TdxNullableSingle read GetBottomPadding write SetBottomPadding;
    property LeftPadding: TdxNullableSingle read GetLeftPadding write SetLeftPadding;
    property RightPadding: TdxNullableSingle read GetRightPadding write SetRightPadding;
    property TableAlignment: TdxNullableValue<TdxRichEditTableRowAlignment> read GetTableAlignment write SetTableAlignment;
    property TableBackgroundColor: TdxNullableValue<TdxAlphaColor> read GetTableBackgroundColor write SetTableBackgroundColor;
    property TableBorders: IdxRichEditTableBorders read GetTableBorders;
    property TableCellSpacing: TdxNullableSingle read GetTableCellSpacing write SetTableCellSpacing;
    property TableLayout: TdxNullableValue<TdxRichEditTableLayoutType> read GetTableLayout write SetTableLayout;
    property TopPadding: TdxNullableSingle read GetTopPadding write SetTopPadding;
  end;

  { TdxNativeStyleTableProperties }

  TdxNativeStyleTableProperties = class(TdxNativeTablePropertiesBase)
  strict private
    FProperties: TdxTableProperties;
  protected
    function CreateNativeStyleTableBorders: IdxRichEditTableBorders; override;
    function GetProperties: TdxTableProperties; override;
  public
    constructor Create(ADocument: TdxNativeDocument; AProperties: TdxTableProperties); reintroduce;
  end;

  { TdxNativeTableStyle }

  TdxNativeTableStyle = class(TInterfacedObject, IdxRichEditTableStyle)
  strict private
    FDocument: TdxNativeDocument;
    FInnerStyle: TdxTableStyle;

    FCharacterProperties: IdxRichEditCharacterPropertiesBase;
    FConditionalStyleProperties: IdxRichEditTableConditionalStyleProperties;
    FParagraphProperties: IdxRichEditParagraphPropertiesBase;
    FTableCellProperties: IdxRichEditTableCellPropertiesBase;
    FTableProperties: IdxRichEditTablePropertiesBase;

    function GetDocumentModel: TdxDocumentModel;

    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetConditionalStyleProperties: IdxRichEditTableConditionalStyleProperties;
    function GetIsDeleted: Boolean;
    function GetName: string;
    function GetParent: IdxRichEditTableStyle;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
    function GetTableProperties: IdxRichEditTablePropertiesBase;
    procedure SetName(const Value: string);
    procedure SetParent(const Value: IdxRichEditTableStyle);
  protected
    property Document: TdxNativeDocument read FDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ADocument: TdxNativeDocument; AInnerStyle: TdxTableStyle);

    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ConditionalStyleProperties: IdxRichEditTableConditionalStyleProperties read GetConditionalStyleProperties;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property TableCellProperties: IdxRichEditTableCellPropertiesBase read GetTableCellProperties;
    property TableProperties: IdxRichEditTablePropertiesBase read GetTableProperties;

    property InnerStyle: TdxTableStyle read FInnerStyle;
    property IsDeleted: Boolean read GetIsDeleted;
    property Name: string read GetName write SetName;
    property Parent: IdxRichEditTableStyle read GetParent write SetParent;
  end;

  { TdxNativeTableConditionalStyle }

  TdxNativeTableConditionalStyle = class(TInterfacedObject, IdxRichEditTableConditionalStyle)
  strict private
    FDocument: TdxNativeDocument;
    FInnerStyle: TdxTableConditionalStyle;

    FCharacterProperties: IdxRichEditCharacterPropertiesBase;
    FParagraphProperties: IdxRichEditParagraphPropertiesBase;
    FTableCellProperties: IdxRichEditTableCellPropertiesBase;
    FTableProperties: IdxRichEditTablePropertiesBase;
  protected
    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
    function GetTableProperties: IdxRichEditTablePropertiesBase;

    property Document: TdxNativeDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeDocument; AInnerStyle: TdxTableConditionalStyle);

    property InnerStyle: TdxTableConditionalStyle read FInnerStyle;
  end;

  { TdxNativeTableConditionalStyleProperties }

  TdxNativeTableConditionalStyleProperties = class(TInterfacedObject,
    IdxRichEditTableConditionalStyleProperties)
  strict private
    FItems: TDictionary<TdxConditionalTableStyleFormattingType, IdxRichEditTableConditionalStyle>;
    FOwner: TdxNativeTableStyle;

    function CreateConditionalStyle(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
    function GetItem(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
    function GetOwner: IdxRichEditTableStyle;
    procedure InitializeItems;
  public
    constructor Create(AOwner: TdxNativeTableStyle);
    destructor Destroy; override;

    property Items: TDictionary<TdxConditionalTableStyleFormattingType, IdxRichEditTableConditionalStyle> read FItems;
    property Owner: TdxNativeTableStyle read FOwner;
  end;

  { TdxNativeReadOnlyCollectionBase }

  TdxNativeReadOnlyCollectionBase<TItem: IInterface; TModelItem: TdxStyleBase> = class abstract(TInterfacedObject, IdxReadOnlyList<TItem>)
  strict private
    FDocument: TdxNativeDocument;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItem(Index: Integer): TItem; overload; virtual; abstract;
    function GetItem(AItem: TModelItem): TItem; overload;
    function CreateNew(AItem: TModelItem): TItem; overload; virtual; abstract;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Document: TdxNativeDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeDocument);
    destructor Destroy; override;

    procedure Invalidate;


    property Count: Integer read GetCount;
  end;

  { TdxNativeReadOnlyStyleCollectionBase }

  TdxNativeReadOnlyStyleCollectionBase<TStyle: IInterface; TModelStyle: TdxStyleBase> = class abstract(TdxNativeReadOnlyCollectionBase<TStyle, TModelStyle>)
  public
    function GetStyle(AStyle: TModelStyle): TStyle;
  end;

  { TdxNativeStyleCollectionBase }

  TdxNativeStyleCollectionBase<TStyle: IInterface; TModelStyle: TdxStyleBase> = class abstract(TdxNativeReadOnlyStyleCollectionBase<TStyle, TModelStyle>)
  protected
    function GetItem(Index: Integer): TStyle; overload; override;
    function GetItem(const AName: string): TStyle; overload;
    function GetInnerStyles: TdxStyleCollectionBase; virtual; abstract;
    function GetCount: Integer; override;
    function CreateModelStyle: TModelStyle; virtual; abstract;
    function GetModelStyle(const AStyle: TStyle): TModelStyle; virtual; abstract;

    property InnerStyles: TdxStyleCollectionBase read GetInnerStyles;
  public
    function CreateNew: TStyle; overload;
    procedure Add(const AStyle: TStyle);
    procedure ValidateStyleProperties(const AStyle: TStyle); virtual;
    procedure Delete(const AStyle: TStyle);
  end;

  { TdxNativeParagraphStyleCollection }

  TdxNativeParagraphStyleCollection = class(TdxNativeStyleCollectionBase<IdxRichEditParagraphStyle, TdxParagraphStyle>,
    IdxRichEditParagraphStyleCollection)
  protected
    function CreateModelStyle: TdxParagraphStyle; override;
    function CreateNew(AItem: TdxParagraphStyle): IdxRichEditParagraphStyle; override;
    function GetInnerStyles: TdxStyleCollectionBase; override;
    function GetModelStyle(const AStyle: IdxRichEditParagraphStyle): TdxParagraphStyle; override;
  public
    procedure ValidateStyleProperties(const AStyle: IdxRichEditParagraphStyle); override;
  end;

  { TdxNativeCharacterStyleCollection }

  TdxNativeCharacterStyleCollection = class(TdxNativeStyleCollectionBase<IdxRichEditCharacterStyle, TdxCharacterStyle>,
    IdxRichEditCharacterStyleCollection)
  protected
    function CreateModelStyle: TdxCharacterStyle; override;
    function CreateNew(AItem: TdxCharacterStyle): IdxRichEditCharacterStyle; override;
    function GetInnerStyles: TdxStyleCollectionBase; override;
    function GetModelStyle(const AStyle: IdxRichEditCharacterStyle): TdxCharacterStyle; override;
  end;

  { TdxNativeTableStyleCollection }

  TdxNativeTableStyleCollection = class(TdxNativeStyleCollectionBase<IdxRichEditTableStyle, TdxTableStyle>, IdxRichEditTableStyleCollection)
  protected
    function CreateModelStyle: TdxTableStyle; override;
    function CreateNew(AItem: TdxTableStyle): IdxRichEditTableStyle; override;
    function GetInnerStyles: TdxStyleCollectionBase; override;
    function GetModelStyle(const AStyle: IdxRichEditTableStyle): TdxTableStyle; override;
  end;

  { TdxNativeDefaultParagraphProperties }

  TdxNativeDefaultParagraphProperties = class(TdxNativeSimpleParagraphProperties)
  public
    procedure Reset; overload; override;
    procedure Reset(const AMask: TdxRichEditParagraphPropertiesMask); overload; override;
  end;

  { TdxNativeDefaultCharacterProperties }

  TdxNativeDefaultCharacterProperties = class(TdxNativeSimpleCharacterProperties)
  public
    procedure Reset; overload; override;
    procedure Reset(const AMask: TdxRichEditCharacterPropertiesMask); overload; override;
  end;

  { TdxNativeDefaultTableProperties }

  TdxNativeDefaultTableProperties = class(TdxNativeStyleTableProperties)
  public
    procedure Reset; overload; override;
    procedure Reset(const AMask: TdxRichEditTablePropertiesMask); overload; override;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.PieceTableModifiers;

type
  { TdxNativeApiFontPropertiesModifier }

  TdxNativeApiFontPropertiesModifier = class(TdxFontPropertiesModifier)
  strict private
    FResetOptions: TdxCharacterFormattingOptions;
  public
    constructor Create(const ANewValue: TdxMergedCharacterProperties; const AResetOptions: TdxCharacterFormattingOptions); reintroduce;

    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxMergedCharacterProperties; override;
    function Merge(const ALeftValue, ARightValue: TdxMergedCharacterProperties): TdxMergedCharacterProperties; override;
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
  end;

  { TdxNativeApiParagraphPropertiesModifier }

  TdxNativeApiParagraphPropertiesModifier = class(TdxParagraphPropertiesModifier)
  strict private
    FResetOptions: TdxParagraphFormattingOptions;
  public
    constructor Create(const ANewValue: TdxMergedParagraphProperties; const AResetOptions: TdxParagraphFormattingOptions); reintroduce;

    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxMergedParagraphProperties; override;
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxMergedParagraphProperties): TdxMergedParagraphProperties; override;
  end;

{ TdxNativeApiFontPropertiesModifier }

constructor TdxNativeApiFontPropertiesModifier.Create(
  const ANewValue: TdxMergedCharacterProperties; const AResetOptions: TdxCharacterFormattingOptions);
begin
  inherited Create(ANewValue);
  FResetOptions := AResetOptions;
end;

function TdxNativeApiFontPropertiesModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxMergedCharacterProperties;
var
  AFormattingInfo: TdxCharacterFormattingInfo;
  AOptions: TdxCharacterFormattingOptions;
begin
  AFormattingInfo := ARun.MergedCharacterFormatting;
  AOptions := ARun.CharacterProperties.Info.Options;
  Result := TdxMergedCharacterProperties.Create(AFormattingInfo, AOptions);
end;

function TdxNativeApiFontPropertiesModifier.Merge(const ALeftValue, ARightValue: TdxMergedCharacterProperties): TdxMergedCharacterProperties;
var
  ATargetOptions: TdxCharacterFormattingOptions;
  ATargetInfo, ARunInfo: TdxCharacterFormattingInfo;
begin
  ATargetOptions := ALeftValue.Options;
  ATargetInfo := ALeftValue.Info;
  ARunInfo := ARightValue.Info;
  ATargetOptions.UseAllCaps := ATargetOptions.UseAllCaps and (ATargetInfo.AllCaps = ARunInfo.AllCaps) and (ATargetOptions.UseAllCaps = ARightValue.Options.UseAllCaps);
  ATargetOptions.UseBackColor := ATargetOptions.UseBackColor and (ATargetInfo.BackColor = ARunInfo.BackColor) and (ATargetOptions.UseBackColor = ARightValue.Options.UseBackColor);
  ATargetOptions.UseFontBold := ATargetOptions.UseFontBold and (ATargetInfo.FontBold = ARunInfo.FontBold) and (ATargetOptions.UseFontBold = ARightValue.Options.UseFontBold);
  ATargetOptions.UseFontItalic := ATargetOptions.UseFontItalic and (ATargetInfo.FontItalic = ARunInfo.FontItalic) and (ATargetOptions.UseFontItalic = ARightValue.Options.UseFontItalic);
  ATargetOptions.UseFontName := ATargetOptions.UseFontName and (ATargetInfo.FontName = ARunInfo.FontName) and (ATargetOptions.UseFontName = ARightValue.Options.UseFontName);
  ATargetOptions.UseDoubleFontSize := ATargetOptions.UseDoubleFontSize and (ATargetInfo.DoubleFontSize = ARunInfo.DoubleFontSize) and (ATargetOptions.UseDoubleFontSize = ARightValue.Options.UseDoubleFontSize);
  ATargetOptions.UseFontStrikeoutType := ATargetOptions.UseFontStrikeoutType and (ATargetInfo.FontStrikeoutType = ARunInfo.FontStrikeoutType) and (ATargetOptions.UseFontStrikeoutType = ARightValue.Options.UseFontStrikeoutType);
  ATargetOptions.UseFontUnderlineType := ATargetOptions.UseFontUnderlineType and (ATargetInfo.FontUnderlineType = ARunInfo.FontUnderlineType) and (ATargetOptions.UseFontUnderlineType = ARightValue.Options.UseFontUnderlineType);
  ATargetOptions.UseForeColor := ATargetOptions.UseForeColor and (ATargetInfo.ForeColor = ARunInfo.ForeColor) and (ATargetOptions.UseForeColor = ARightValue.Options.UseForeColor);
  ATargetOptions.UseScript := ATargetOptions.UseScript and (ATargetInfo.Script = ARunInfo.Script) and (ATargetOptions.UseScript = ARightValue.Options.UseScript);
  ATargetOptions.UseStrikeoutColor := ATargetOptions.UseStrikeoutColor and (ATargetInfo.StrikeoutColor = ARunInfo.StrikeoutColor) and (ATargetOptions.UseStrikeoutColor = ARightValue.Options.UseStrikeoutColor);
  ATargetOptions.UseStrikeoutWordsOnly := ATargetOptions.UseStrikeoutWordsOnly and (ATargetInfo.StrikeoutWordsOnly = ARunInfo.StrikeoutWordsOnly) and (ATargetOptions.UseStrikeoutWordsOnly = ARightValue.Options.UseStrikeoutWordsOnly);
  ATargetOptions.UseUnderlineColor := ATargetOptions.UseUnderlineColor and (ATargetInfo.UnderlineColor = ARunInfo.UnderlineColor) and (ATargetOptions.UseUnderlineColor = ARightValue.Options.UseUnderlineColor);
  ATargetOptions.UseUnderlineWordsOnly := ATargetOptions.UseUnderlineWordsOnly and (ATargetInfo.UnderlineWordsOnly = ARunInfo.UnderlineWordsOnly) and (ATargetOptions.UseUnderlineWordsOnly = ARightValue.Options.UseUnderlineWordsOnly);
  ATargetOptions.UseHidden := ATargetOptions.UseHidden and (ATargetInfo.Hidden = ARunInfo.Hidden) and (ATargetOptions.UseHidden = ARightValue.Options.UseHidden);
  Result := TdxMergedCharacterProperties.Create(ATargetInfo, ATargetOptions);
end;

procedure TdxNativeApiFontPropertiesModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  AProperties: IdxCharacterProperties;
  AInfo: TdxCharacterFormattingInfo;
  AOptions: TdxCharacterFormattingOptions;
begin
  inherited ModifyTextRun(ARun, ARunIndex);
  AProperties := ARun as IdxCharacterProperties;
  AInfo := NewValue.Info;
  AOptions := NewValue.Options;
  if AOptions.UseBackColor then
    AProperties.BackColor := AInfo.BackColor;
  if not FResetOptions.UseAllCaps then
    ARun.CharacterProperties.Info.Options.UseAllCaps := False;
  if not FResetOptions.UseBackColor then
    ARun.CharacterProperties.Info.Options.UseBackColor := False;
  if not FResetOptions.UseFontBold then
    ARun.CharacterProperties.Info.Options.UseFontBold := False;
  if not FResetOptions.UseFontItalic then
    ARun.CharacterProperties.Info.Options.UseFontItalic := False;
  if not FResetOptions.UseFontName then
    ARun.CharacterProperties.Info.Options.UseFontName := False;
  if not FResetOptions.UseDoubleFontSize then
    ARun.CharacterProperties.Info.Options.UseDoubleFontSize := False;
  if not FResetOptions.UseFontStrikeoutType then
    ARun.CharacterProperties.Info.Options.UseFontStrikeoutType := False;
  if not FResetOptions.UseFontUnderlineType then
    ARun.CharacterProperties.Info.Options.UseFontUnderlineType := False;
  if not FResetOptions.UseForeColor then
    ARun.CharacterProperties.Info.Options.UseForeColor := False;
  if not FResetOptions.UseScript then
    ARun.CharacterProperties.Info.Options.UseScript := False;
  if not FResetOptions.UseStrikeoutColor then
    ARun.CharacterProperties.Info.Options.UseStrikeoutColor := False;
  if not FResetOptions.UseStrikeoutWordsOnly then
    ARun.CharacterProperties.Info.Options.UseStrikeoutWordsOnly := False;
  if not FResetOptions.UseUnderlineColor then
    ARun.CharacterProperties.Info.Options.UseUnderlineColor := False;
  if not FResetOptions.UseUnderlineWordsOnly then
    ARun.CharacterProperties.Info.Options.UseUnderlineWordsOnly := False;
  if not FResetOptions.UseHidden then
    ARun.CharacterProperties.Info.Options.UseHidden := False;
end;

{ TdxNativeApiParagraphPropertiesModifier }

constructor TdxNativeApiParagraphPropertiesModifier.Create(
  const ANewValue: TdxMergedParagraphProperties; const AResetOptions: TdxParagraphFormattingOptions);
begin
  inherited Create(ANewValue);
  FResetOptions := AResetOptions;
end;

function TdxNativeApiParagraphPropertiesModifier.Merge(const ALeftValue, ARightValue: TdxMergedParagraphProperties): TdxMergedParagraphProperties;
var
  ATargetOptions: TdxParagraphFormattingOptions;
  ATargetInfo: TdxParagraphFormattingInfo;
  ARunInfo: TdxParagraphFormattingInfo;
begin
  ATargetOptions := ALeftValue.Options;
  ATargetInfo := ALeftValue.Info;
  ARunInfo := ARightValue.Info;
  ATargetOptions.UseLeftIndent := ATargetOptions.UseLeftIndent and (ATargetInfo.LeftIndent = ARunInfo.LeftIndent) and (ATargetOptions.UseLeftIndent = ARightValue.Options.UseLeftIndent);
  ATargetOptions.UseRightIndent := ATargetOptions.UseRightIndent and (ATargetInfo.RightIndent = ARunInfo.RightIndent) and (ATargetOptions.UseRightIndent = ARightValue.Options.UseRightIndent);
  ATargetOptions.UseFirstLineIndent := ATargetOptions.UseFirstLineIndent and (ATargetInfo.FirstLineIndent = ARunInfo.FirstLineIndent) and
    (ATargetInfo.FirstLineIndentType = ARunInfo.FirstLineIndentType) and (ATargetOptions.UseFirstLineIndent = ARightValue.Options.UseFirstLineIndent);
  ATargetOptions.UseAlignment := ATargetOptions.UseAlignment and (ATargetInfo.Alignment = ARunInfo.Alignment) and (ATargetOptions.UseAlignment = ARightValue.Options.UseAlignment);
  ATargetOptions.UseSpacingBefore := ATargetOptions.UseSpacingBefore and (ATargetInfo.SpacingBefore = ARunInfo.SpacingBefore) and (ATargetOptions.UseSpacingBefore = ARightValue.Options.UseSpacingBefore);
  ATargetOptions.UseSpacingAfter := ATargetOptions.UseSpacingAfter and (ATargetInfo.SpacingAfter = ARunInfo.SpacingAfter) and (ATargetOptions.UseSpacingAfter = ARightValue.Options.UseSpacingAfter);
  ATargetOptions.UseLineSpacing := ATargetOptions.UseLineSpacing and (ATargetInfo.LineSpacing = ARunInfo.LineSpacing) and
    (ATargetInfo.LineSpacingType = ARunInfo.LineSpacingType) and (ATargetOptions.UseLineSpacing = ARightValue.Options.UseLineSpacing);
  ATargetOptions.UseSuppressHyphenation := ATargetOptions.UseSuppressHyphenation and (ATargetInfo.SuppressHyphenation = ARunInfo.SuppressHyphenation) and (ATargetOptions.UseSuppressHyphenation = ARightValue.Options.UseSuppressHyphenation);
  ATargetOptions.UseSuppressLineNumbers := ATargetOptions.UseSuppressLineNumbers and (ATargetInfo.SuppressLineNumbers = ARunInfo.SuppressLineNumbers) and (ATargetOptions.UseSuppressLineNumbers = ARightValue.Options.UseSuppressLineNumbers);
  ATargetOptions.UseContextualSpacing := ATargetOptions.UseContextualSpacing and (ATargetInfo.ContextualSpacing = ARunInfo.ContextualSpacing) and (ATargetOptions.UseContextualSpacing = ARightValue.Options.UseContextualSpacing);
  ATargetOptions.UsePageBreakBefore := ATargetOptions.UsePageBreakBefore and (ATargetInfo.PageBreakBefore = ARunInfo.PageBreakBefore) and (ATargetOptions.UsePageBreakBefore = ARightValue.Options.UsePageBreakBefore);
  ATargetOptions.UseBeforeAutoSpacing := ATargetOptions.UseBeforeAutoSpacing and (ATargetInfo.BeforeAutoSpacing = ARunInfo.BeforeAutoSpacing) and (ATargetOptions.UseBeforeAutoSpacing = ARightValue.Options.UseBeforeAutoSpacing);
  ATargetOptions.UseAfterAutoSpacing := ATargetOptions.UseAfterAutoSpacing and (ATargetInfo.AfterAutoSpacing = ARunInfo.AfterAutoSpacing) and (ATargetOptions.UseAfterAutoSpacing = ARightValue.Options.UseAfterAutoSpacing);
  ATargetOptions.UseKeepWithNext := ATargetOptions.UseKeepWithNext and (ATargetInfo.KeepWithNext = ARunInfo.KeepWithNext) and (ATargetOptions.UseKeepWithNext = ARightValue.Options.UseKeepWithNext);
  ATargetOptions.UseKeepLinesTogether := ATargetOptions.UseKeepLinesTogether and (ATargetInfo.KeepLinesTogether = ARunInfo.KeepLinesTogether) and (ATargetOptions.UseKeepLinesTogether = ARightValue.Options.UseKeepLinesTogether);
  ATargetOptions.UseWidowOrphanControl := ATargetOptions.UseWidowOrphanControl and (ATargetInfo.WidowOrphanControl = ARunInfo.WidowOrphanControl) and (ATargetOptions.UseWidowOrphanControl = ARightValue.Options.UseWidowOrphanControl);
  ATargetOptions.UseOutlineLevel := ATargetOptions.UseOutlineLevel and (ATargetInfo.OutlineLevel = ARunInfo.OutlineLevel) and (ATargetOptions.UseOutlineLevel = ARightValue.Options.UseOutlineLevel);
  ATargetOptions.UseBackColor := ATargetOptions.UseBackColor and (ATargetInfo.BackColor = ARunInfo.BackColor) and (ATargetOptions.UseBackColor = ARightValue.Options.UseBackColor);
  Result := TdxMergedParagraphProperties.Create(ATargetInfo, ATargetOptions);
end;

function TdxNativeApiParagraphPropertiesModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxMergedParagraphProperties;
begin
  Result := TdxMergedParagraphProperties.Create(AParagraph.ParagraphProperties.Info.Info, AParagraph.ParagraphProperties.Info.Options);
end;

procedure TdxNativeApiParagraphPropertiesModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
begin
  inherited ModifyParagraph(AParagraph, AParagraphIndex);
  if not FResetOptions.UseLeftIndent then
    AParagraph.ParagraphProperties.Info.Options.UseLeftIndent := False;
  if not FResetOptions.UseRightIndent then
    AParagraph.ParagraphProperties.Info.Options.UseRightIndent := False;
  if not FResetOptions.UseFirstLineIndent then
    AParagraph.ParagraphProperties.Info.Options.UseFirstLineIndent := False;
  if not FResetOptions.UseAlignment then
    AParagraph.ParagraphProperties.Info.Options.UseAlignment := False;
  if not FResetOptions.UseSpacingBefore then
    AParagraph.ParagraphProperties.Info.Options.UseSpacingBefore := False;
  if not FResetOptions.UseSpacingAfter then
    AParagraph.ParagraphProperties.Info.Options.UseSpacingAfter := False;
  if not FResetOptions.UseLineSpacing then
    AParagraph.ParagraphProperties.Info.Options.UseLineSpacing := False;
  if not FResetOptions.UseSuppressHyphenation then
    AParagraph.ParagraphProperties.Info.Options.UseSuppressHyphenation := False;
  if not FResetOptions.UseSuppressLineNumbers then
    AParagraph.ParagraphProperties.Info.Options.UseSuppressLineNumbers := False;
  if not FResetOptions.UseContextualSpacing then
    AParagraph.ParagraphProperties.Info.Options.UseContextualSpacing := False;
  if not FResetOptions.UsePageBreakBefore then
    AParagraph.ParagraphProperties.Info.Options.UsePageBreakBefore := False;
  if not FResetOptions.UseBeforeAutoSpacing then
    AParagraph.ParagraphProperties.Info.Options.UseBeforeAutoSpacing := False;
  if not FResetOptions.UseAfterAutoSpacing then
    AParagraph.ParagraphProperties.Info.Options.UseAfterAutoSpacing := False;
  if not FResetOptions.UseKeepWithNext then
    AParagraph.ParagraphProperties.Info.Options.UseKeepWithNext := False;
  if not FResetOptions.UseKeepLinesTogether then
    AParagraph.ParagraphProperties.Info.Options.UseKeepLinesTogether := False;
  if not FResetOptions.UseWidowOrphanControl then
    AParagraph.ParagraphProperties.Info.Options.UseWidowOrphanControl := False;
  if not FResetOptions.UseOutlineLevel then
    AParagraph.ParagraphProperties.Info.Options.UseOutlineLevel := False;
  if not FResetOptions.UseBackColor then
    AParagraph.ParagraphProperties.Info.Options.UseBackColor := False;
end;

{ TdxNativeCharacterPropertiesBase }

function TdxNativeCharacterPropertiesBase.GetAllCaps: TdxNullableBoolean;
begin
  if not Options.UseAllCaps then
    Exit(TdxNullableBoolean.Null);
  Result := Info.AllCaps;
end;

function TdxNativeCharacterPropertiesBase.GetBackColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Options.UseBackColor then
    Exit(TdxNullableValue<TdxAlphaColor>.Null);
  Result := Info.BackColor;
end;

function TdxNativeCharacterPropertiesBase.GetFontSize: TdxNullableSingle;
begin
  if not Options.UseDoubleFontSize then
    Exit(TdxNullableSingle.Null);
  Result := Info.DoubleFontSize / 2;
end;

function TdxNativeCharacterPropertiesBase.GetBold: TdxNullableBoolean;
begin
  if not Options.UseFontBold then
    Exit(TdxNullableBoolean.Null);
  Result := Info.FontBold;
end;

function TdxNativeCharacterPropertiesBase.GetForeColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Options.UseForeColor then
    Exit(TdxNullableValue<TdxAlphaColor>.Null);
  Result := Info.ForeColor;
end;

function TdxNativeCharacterPropertiesBase.GetItalic: TdxNullableBoolean;
begin
  if not Options.UseFontItalic then
    Exit(TdxNullableBoolean.Null);
  Result := Info.FontItalic;
end;

function TdxNativeCharacterPropertiesBase.GetFontName: TdxNullableString;
begin
  if not Options.UseFontName then
    Exit(TdxNullableString.Null);
  Result := Info.FontName;
end;

function TdxNativeCharacterPropertiesBase.GetUnderline: TdxNullableValue<TdxRichEditUnderlineType>;
begin
  if not Options.UseFontUnderlineType then
    Exit(TdxNullableValue<TdxRichEditUnderlineType>.Null);
  Result := Info.FontUnderlineType;
end;

function TdxNativeCharacterPropertiesBase.GetStrikeout: TdxNullableValue<TdxRichEditStrikeoutType>;
begin
  if not Options.UseFontStrikeoutType then
    Exit(TdxNullableValue<TdxRichEditStrikeoutType>.Null);
  Result := Info.FontStrikeoutType;
end;

function TdxNativeCharacterPropertiesBase.GetHidden: TdxNullableBoolean;
begin
  if not Options.UseHidden then
    Exit(TdxNullableBoolean.Null);
  Result := Info.Hidden;
end;

function TdxNativeCharacterPropertiesBase.GetSubscript: TdxNullableBoolean;
begin
  if not Options.UseScript then
    Exit(TdxNullableBoolean.Null);
  Result := Info.Script = TdxCharacterFormattingScript.Subscript;
end;

function TdxNativeCharacterPropertiesBase.GetSuperscript: TdxNullableBoolean;
begin
  if not Options.UseScript then
    Exit(TdxNullableBoolean.Null);
  Result := Info.Script = TdxCharacterFormattingScript.Superscript;
end;

function TdxNativeCharacterPropertiesBase.GetStrikeoutColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Options.UseStrikeoutColor then
    Exit(TdxNullableValue<TdxAlphaColor>.Null);
  Result := Info.StrikeoutColor;
end;

function TdxNativeCharacterPropertiesBase.GetUnderlineColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Options.UseUnderlineColor then
    Exit(TdxNullableValue<TdxAlphaColor>.Null);
  Result := Info.UnderlineColor;
end;

procedure TdxNativeCharacterPropertiesBase.SetAllCaps(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseAllCaps])
  else
    SetAllCapsCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseBackColor])
  else
    SetBackColorCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetFontSize(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseDoubleFontSize])
  else
    SetDoubleFontSizeCore(Trunc(Value.Value * 2));
end;

procedure TdxNativeCharacterPropertiesBase.SetBold(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseFontBold])
  else
    SetBoldCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetForeColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseForeColor])
  else
    SetForeColorCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetItalic(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseFontItalic])
  else
    SetItalicCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetFontName(const Value: TdxNullableString);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseFontName])
  else
    SetFontNameCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetUnderline(const Value: TdxNullableValue<TdxRichEditUnderlineType>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseFontUnderlineType])
  else
    SetUnderlineCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetStrikeout(const Value: TdxNullableValue<TdxRichEditStrikeoutType>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseFontStrikeoutType])
  else
    SetStrikeoutCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetHidden(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseHidden])
  else
    SetHiddenCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetSubscript(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseScript])
  else
  begin
    if Value.Value then
      SetScriptCore(TdxCharacterFormattingScript.Subscript)
    else
      if GetSubscript <> Value then
        SetScriptCore(TdxCharacterFormattingScript.Normal);
  end;
end;

procedure TdxNativeCharacterPropertiesBase.SetSuperscript(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseScript])
  else
  begin
    if Value.Value then
      SetScriptCore(TdxCharacterFormattingScript.Superscript)
    else
      if GetSuperscript <> Value then
        SetScriptCore(TdxCharacterFormattingScript.Normal);
  end;
end;

procedure TdxNativeCharacterPropertiesBase.SetStrikeoutColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseStrikeoutColor])
  else
    SetStrikeoutColorCore(Value);
end;

procedure TdxNativeCharacterPropertiesBase.SetUnderlineColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxUsedCharacterFormattingOption.UseUnderlineColor])
  else
    SetUnderlineColorCore(Value);
end;

function TdxNativeCharacterPropertiesBase.GetResetOptions: PdxCharacterFormattingOptions;
begin
  Result := Options;
end;

procedure TdxNativeCharacterPropertiesBase.SetAllCapsCore(const Value: Boolean);
begin
  Info.AllCaps := Value;
  Options.UseAllCaps := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetBackColorCore(const Value: TdxAlphaColor);
begin
  Info.BackColor := Value;
  Options.UseBackColor := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetDoubleFontSizeCore(const Value: Integer);
begin
  Info.DoubleFontSize := Value;
  Options.UseDoubleFontSize := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetBoldCore(const Value: Boolean);
begin
  Info.FontBold := Value;
  Options.UseFontBold := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetForeColorCore(const Value: TdxAlphaColor);
begin
  Info.ForeColor := Value;
  Options.UseForeColor := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetItalicCore(const Value: Boolean);
begin
  Info.FontItalic := Value;
  Options.UseFontItalic := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetFontNameCore(const Value: string);
begin
  Info.FontName := Value;
  Options.UseFontName := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetStrikeoutCore(const Value: TdxRichEditStrikeoutType);
begin
  Info.FontStrikeoutType := Value;
  Options.UseFontStrikeoutType := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetHiddenCore(const Value: Boolean);
begin
  Info.Hidden := Value;
  Options.UseHidden := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetScriptCore(const Value: TdxCharacterFormattingScript);
begin
  Info.Script := Value;
  Options.UseScript := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetStrikeoutColorCore(const Value: TdxAlphaColor);
begin
  Info.StrikeoutColor := Value;
  Options.UseStrikeoutColor := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetUnderlineCore(const Value: TdxRichEditUnderlineType);
begin
  Info.FontUnderlineType := Value;
  Options.UseFontUnderlineType := True;
end;

procedure TdxNativeCharacterPropertiesBase.SetUnderlineColorCore(const Value: TdxAlphaColor);
begin
  Info.UnderlineColor := Value;
  Options.UseUnderlineColor := True;
end;

procedure TdxNativeCharacterPropertiesBase.Reset;
begin
  ResetOptions.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  Options.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
end;

procedure TdxNativeCharacterPropertiesBase.Reset(const AMask: TdxRichEditCharacterPropertiesMask);
begin
  ResetOptions.ResetUse(AMask);
  Options.ResetUse(AMask);
end;

{ TdxNativeSimpleCharacterProperties }

constructor TdxNativeSimpleCharacterProperties.Create(AProperties: TdxCharacterProperties);
begin
  inherited Create;
  FProperties := AProperties;
end;

procedure TdxNativeSimpleCharacterProperties.SetAllCapsCore(const Value: Boolean);
begin
  FProperties.AllCaps := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetBackColorCore(const Value: TdxAlphaColor);
begin
  FProperties.BackColor := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetDoubleFontSizeCore(const Value: Integer);
begin
  FProperties.DoubleFontSize := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetBoldCore(const Value: Boolean);
begin
  FProperties.FontBold := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetForeColorCore(const Value: TdxAlphaColor);
begin
  FProperties.ForeColor := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetItalicCore(const Value: Boolean);
begin
  FProperties.FontItalic := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetFontNameCore(const Value: string);
begin
  FProperties.FontName := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetStrikeoutCore(const Value: TdxRichEditStrikeoutType);
begin
  FProperties.FontStrikeoutType := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetHiddenCore(const Value: Boolean);
begin
  FProperties.Hidden := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetScriptCore(const Value: TdxCharacterFormattingScript);
begin
  FProperties.Script := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetStrikeoutColorCore(const Value: TdxAlphaColor);
begin
  FProperties.StrikeoutColor := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetUnderlineCore(const Value: TdxRichEditUnderlineType);
begin
  FProperties.FontUnderlineType := Value;
end;

procedure TdxNativeSimpleCharacterProperties.SetUnderlineColorCore(const Value: TdxAlphaColor);
begin
  FProperties.UnderlineColor := Value;
end;

function TdxNativeSimpleCharacterProperties.GetInfo: TdxCharacterFormattingInfo;
begin
  Result := FProperties.Info.Info;
end;

function TdxNativeSimpleCharacterProperties.GetOptions: PdxCharacterFormattingOptions;
begin
  Result := @FProperties.Info.Options;
end;

{ TdxNativeCharacterProperties }

constructor TdxNativeCharacterProperties.Create(ADocument: TdxNativeSubDocument;
  AFrom: TdxDocumentLogPosition; ALength: Integer);
begin
  inherited Create;
  FDocument := ADocument;
  FFrom := AFrom;
  FLength := ALength;
  FResetOptions := TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseAll);
  FProperties := CreateProperties;
end;

destructor TdxNativeCharacterProperties.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TdxNativeCharacterProperties.Apply;
var
  AModifier: TdxFontPropertiesModifier;
begin
  AModifier := TdxNativeApiFontPropertiesModifier.Create(FProperties, FResetOptions);
  try
    FDocument.PieceTable.ApplyCharacterFormatting(FFrom, FLength, AModifier);
  finally
    AModifier.Free;
  end;
end;

function TdxNativeCharacterProperties.GetInfo: TdxCharacterFormattingInfo;
begin
  Result := FProperties.Info;
end;

function TdxNativeCharacterProperties.GetOptions: PdxCharacterFormattingOptions;
begin
  Result := @FProperties.Options;
end;

function TdxNativeCharacterProperties.GetResetOptions: PdxCharacterFormattingOptions;
begin
  Result := @FResetOptions;
end;

function TdxNativeCharacterProperties.CreateProperties: TdxMergedCharacterProperties;
var
  AModifier: TdxFontPropertiesModifier;
begin
  AModifier := TdxNativeApiFontPropertiesModifier.Create(nil, FResetOptions);
  try
    Result := AModifier.ObtainMergedRunsPropertyValue(FDocument.PieceTable, FFrom, FLength);
  finally
    AModifier.Free;
  end;
end;

function TdxNativeCharacterProperties.GetStyle: IdxRichEditCharacterStyle;
var
  AModifier: TdxRunCharacterStyleModifier;
  AIndex: Integer;
begin
  AModifier := TdxRunCharacterStyleModifier.Create(-1);
  try
    if not AModifier.ObtainRunsPropertyValue(FDocument.PieceTable, FFrom, FLength, AIndex) then
      Result := nil
    else
      Result := TdxNativeCharacterStyleCollection(TdxNativeDocument(FDocument).CharacterStyles).GetStyle(DocumentModel.CharacterStyles[AIndex]);
  finally
    AModifier.Free;
  end;
end;

procedure TdxNativeCharacterProperties.SetStyle(const Value: IdxRichEditCharacterStyle);
var
  AModifier: TdxRunCharacterStyleModifier;
  AIndex: Integer;
begin
  AIndex := DocumentModel.CharacterStyles.IndexOf(TdxNativeCharacterStyle(Value).InnerStyle);
  AModifier := TdxRunCharacterStyleModifier.Create(AIndex);
  try
    FDocument.PieceTable.ApplyCharacterFormatting(FFrom, FLength, AModifier);
  finally
    AModifier.Free;
  end;
end;

function TdxNativeCharacterProperties.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocument.DocumentModel;
end;

{ TdxNativeCharacterStyle }

constructor TdxNativeCharacterStyle.Create(ADocument: TdxNativeDocument;
  AInnerStyle: TdxCharacterStyle);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerStyle := AInnerStyle;
  FCharacterProperties := TdxNativeSimpleCharacterProperties.Create(AInnerStyle.CharacterProperties);
end;

function TdxNativeCharacterStyle.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FInnerStyle.DocumentModel);
end;

function TdxNativeCharacterStyle.GetName: string;
begin
  Result := FInnerStyle.StyleName;
end;

procedure TdxNativeCharacterStyle.SetName(const Value: string);
begin
  FInnerStyle.StyleName := Value;
end;

function TdxNativeCharacterStyle.GetIsDeleted: Boolean;
begin
  Result := FInnerStyle.Deleted;
end;

function TdxNativeCharacterStyle.GetParent: IdxRichEditCharacterStyle;
var
  AStyles: TdxNativeCharacterStyleCollection;
begin
  AStyles := TdxNativeCharacterStyleCollection(FDocument.CharacterStyles);
  Result := AStyles.GetStyle(FInnerStyle.Parent);
end;

function TdxNativeCharacterStyle.GetPrimary: Boolean;
begin
  Result := InnerStyle.Primary;
end;

procedure TdxNativeCharacterStyle.SetParent(const Value: IdxRichEditCharacterStyle);
var
  AStyle: TdxCharacterStyle;
begin
  if Value <> nil then
    AStyle := TdxNativeCharacterStyle(Value).InnerStyle
  else
    AStyle := nil;
  FInnerStyle.Parent := AStyle;
end;

procedure TdxNativeCharacterStyle.SetPrimary(const Value: Boolean);
begin
  InnerStyle.Primary := Value;
end;

function TdxNativeCharacterStyle.GetLinkedStyle: IdxRichEditParagraphStyle;
var
  AStyles: TdxNativeParagraphStyleCollection;
begin
  AStyles := TdxNativeParagraphStyleCollection(FDocument.ParagraphStyles);
  Result := AStyles.GetStyle(FInnerStyle.LinkedStyle);
end;

procedure TdxNativeCharacterStyle.SetLinkedStyle(const Value: IdxRichEditParagraphStyle);
var
  AStyle: TdxParagraphStyle;
begin
  if Value <> nil then
    AStyle := TdxNativeParagraphStyle(Value).InnerStyle
  else
    AStyle := nil;
  if AStyle = FInnerStyle.LinkedStyle then
    Exit;
  if FInnerStyle.HasLinkedStyle then
    DocumentModel.StyleLinkManager.DeleteLinkCore(FInnerStyle.LinkedStyle, FInnerStyle);

  if AStyle <> nil then
    DocumentModel.StyleLinkManager.CreateLink(AStyle, FInnerStyle);
end;

function TdxNativeCharacterStyle.GetFontName: TdxNullableString;
begin
  Result := FCharacterProperties.FontName;
end;

procedure TdxNativeCharacterStyle.SetFontName(const Value: TdxNullableString);
begin
  FCharacterProperties.FontName := Value;
end;

function TdxNativeCharacterStyle.GetFontSize: TdxNullableSingle;
begin
  Result := FCharacterProperties.FontSize;
end;

procedure TdxNativeCharacterStyle.SetFontSize(const Value: TdxNullableSingle);
begin
  FCharacterProperties.FontSize := Value;
end;

function TdxNativeCharacterStyle.GetBold: TdxNullableBoolean;
begin
  Result := FCharacterProperties.Bold;
end;

procedure TdxNativeCharacterStyle.SetBold(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.Bold := Value;
end;

function TdxNativeCharacterStyle.GetItalic: TdxNullableBoolean;
begin
  Result := FCharacterProperties.Italic;
end;

procedure TdxNativeCharacterStyle.SetItalic(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.Italic := Value;
end;

function TdxNativeCharacterStyle.GetSuperscript: TdxNullableBoolean;
begin
  Result := FCharacterProperties.Superscript;
end;

function TdxNativeCharacterStyle.GetStrikeoutColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FCharacterProperties.StrikeoutColor;
end;

procedure TdxNativeCharacterStyle.SetSuperscript(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.Superscript := Value;
end;

function TdxNativeCharacterStyle.GetSubscript: TdxNullableBoolean;
begin
  Result := FCharacterProperties.Subscript;
end;

procedure TdxNativeCharacterStyle.SetSubscript(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.Subscript := Value;
end;

function TdxNativeCharacterStyle.GetAllCaps: TdxNullableBoolean;
begin
  Result := FCharacterProperties.AllCaps;
end;

procedure TdxNativeCharacterStyle.SetAllCaps(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.AllCaps := Value;
end;

function TdxNativeCharacterStyle.GetUnderline: TdxNullableValue<TdxRichEditUnderlineType>;
begin
  Result := FCharacterProperties.Underline;
end;

procedure TdxNativeCharacterStyle.SetUnderline(const Value: TdxNullableValue<TdxRichEditUnderlineType>);
begin
  FCharacterProperties.Underline := Value;
end;

function TdxNativeCharacterStyle.GetStrikeout: TdxNullableValue<TdxRichEditStrikeoutType>;
begin
  Result := FCharacterProperties.Strikeout;
end;

procedure TdxNativeCharacterStyle.SetStrikeout(const Value: TdxNullableValue<TdxRichEditStrikeoutType>);
begin
  FCharacterProperties.Strikeout := Value;
end;

procedure TdxNativeCharacterStyle.SetStrikeoutColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  FCharacterProperties.StrikeoutColor := Value;
end;

function TdxNativeCharacterStyle.GetUnderlineColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FCharacterProperties.UnderlineColor;
end;

procedure TdxNativeCharacterStyle.SetUnderlineColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  FCharacterProperties.UnderlineColor := Value;
end;

function TdxNativeCharacterStyle.GetForeColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FCharacterProperties.ForeColor;
end;

procedure TdxNativeCharacterStyle.SetForeColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  FCharacterProperties.ForeColor := Value;
end;

function TdxNativeCharacterStyle.GetBackColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FCharacterProperties.BackColor;
end;

procedure TdxNativeCharacterStyle.SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  FCharacterProperties.BackColor := Value;
end;

function TdxNativeCharacterStyle.GetHidden: TdxNullableBoolean;
begin
  Result := FCharacterProperties.Hidden;
end;

procedure TdxNativeCharacterStyle.SetHidden(const Value: TdxNullableBoolean);
begin
  FCharacterProperties.Hidden := Value;
end;

procedure TdxNativeCharacterStyle.Reset;
begin
  FCharacterProperties.Reset;
end;

procedure TdxNativeCharacterStyle.Reset(const AMask: TdxRichEditCharacterPropertiesMask);
begin
  FCharacterProperties.Reset(AMask);
end;

{ TdxNativeParagraphPropertiesBase }

constructor TdxNativeParagraphPropertiesBase.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeParagraphPropertiesBase.GetResetOptions: PdxParagraphFormattingOptions;
begin
  Result := Options;
end;

procedure TdxNativeParagraphPropertiesBase.SetAlignmentCore(const Value: TdxRichEditParagraphAlignment);
begin
  Info.Alignment := Value;
  Options.UseAlignment := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetBackColorCore(const Value: TdxAlphaColor);
begin
  Info.BackColor := Value;
  Options.UseBackColor := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetContextualSpacingCore(const Value: Boolean);
begin
  Info.ContextualSpacing := Value;
  Options.UseContextualSpacing := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetFirstLineIndentCore(const Value: Integer);
begin
  Info.FirstLineIndent := Value;
  Options.UseFirstLineIndent := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetFirstLineIndentTypeCore(const Value: TdxRichEditParagraphFirstLineIndent);
begin
  Info.FirstLineIndentType := Value;
  Options.UseFirstLineIndent := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetKeepLinesTogetherCore(const Value: Boolean);
begin
  Info.KeepLinesTogether := Value;
  Options.UseKeepLinesTogether := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetLeftIndentCore(const Value: Integer);
begin
  Info.LeftIndent := Value;
  Options.UseLeftIndent := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetLineSpacingCore(const Value: Single);
begin
  Info.LineSpacing := Value;
  Options.UseLineSpacing := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetLineSpacingRuleCore(const Value: TdxRichEditParagraphLineSpacing);
begin
  Info.LineSpacingType := Value;
  Options.UseLineSpacing := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetOutlineLevelCore(const Value: Integer);
begin
  Info.OutlineLevel := Value;
  Options.UseOutlineLevel := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetPageBreakBeforeCore(const Value: Boolean);
begin
  Info.PageBreakBefore := Value;
  Options.UsePageBreakBefore := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetRightIndentCore(const Value: Integer);
begin
  Info.RightIndent := Value;
  Options.UseRightIndent := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetSpacingAfterCore(const Value: Integer);
begin
  Info.SpacingAfter := Value;
  Options.UseSpacingAfter := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetSpacingBeforeCore(const Value: Integer);
begin
  Info.SpacingBefore := Value;
  Options.UseSpacingBefore := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetSuppressHyphenationCore(const Value: Boolean);
begin
  Info.SuppressHyphenation := Value;
  Options.UseSuppressHyphenation := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetSuppressLineNumbersCore(const Value: Boolean);
begin
  Info.SuppressLineNumbers := Value;
  Options.UseSuppressLineNumbers := True;
end;

procedure TdxNativeParagraphPropertiesBase.SetAlignment(const Value: TdxNullableValue<TdxRichEditParagraphAlignment>);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseAlignment])
  else
    SetAlignmentCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetBackColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseBackColor])
  else
    SetBackColorCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetContextualSpacing(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseContextualSpacing])
  else
    SetContextualSpacingCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetFirstLineIndent(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseFirstLineIndent])
  else
    SetFirstLineIndentCore(Document.UnitsToModelUnits(Value.Value));
end;

procedure TdxNativeParagraphPropertiesBase.SetFirstLineIndentType(const Value: TdxNullableValue<TdxRichEditParagraphFirstLineIndent>);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseFirstLineIndent])
  else
    SetFirstLineIndentTypeCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetKeepLinesTogether(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseKeepLinesTogether])
  else
    SetKeepLinesTogetherCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetLeftIndent(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseLeftIndent])
  else
    SetLeftIndentCore(Document.UnitsToModelUnits(Value.Value));
end;

procedure TdxNativeParagraphPropertiesBase.SetLineSpacing(const Value: TdxNullableSingle);
var
  AValue: Single;
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseLineSpacing])
  else
  begin
    AValue := Document.UnitsToModelUnitsF(Value.Value);
    if not GetLineSpacingRule.IsNull and (GetLineSpacingRule.Value = TdxRichEditParagraphLineSpacing.Multiple) then
      AValue := Document.ModelUnitsToPointsF(AValue) / 12;
    SetLineSpacingCore(AValue);
  end;
end;

procedure TdxNativeParagraphPropertiesBase.SetLineSpacingMultiplier(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseLineSpacing])
  else
  begin
    if GetLineSpacingRule <> TdxParagraphLineSpacing.Multiple then
      SetLineSpacingRule(TdxParagraphLineSpacing.Multiple);
    SetLineSpacingCore(Value);
  end;
end;

procedure TdxNativeParagraphPropertiesBase.SetLineSpacingRule(const Value: TdxNullableValue<TdxRichEditParagraphLineSpacing>);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseLineSpacing])
  else
    SetLineSpacingRuleCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetOutlineLevel(const Value: TdxNullableInteger);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseOutlineLevel])
  else
    SetOutlineLevelCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetPageBreakBefore(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UsePageBreakBefore])
  else
    SetPageBreakBeforeCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetRightIndent(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseRightIndent])
  else
    SetRightIndentCore(Document.UnitsToModelUnits(Value));
end;

procedure TdxNativeParagraphPropertiesBase.SetSpacingAfter(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseSpacingAfter])
  else
    SetSpacingAfterCore(Document.UnitsToModelUnits(Value.Value));
end;

procedure TdxNativeParagraphPropertiesBase.SetSpacingBefore(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseSpacingBefore])
  else
    SetSpacingBeforeCore(Document.UnitsToModelUnits(Value.Value));
end;

procedure TdxNativeParagraphPropertiesBase.SetSuppressHyphenation(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseSuppressHyphenation])
  else
    SetSuppressHyphenationCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.SetSuppressLineNumbers(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxUsedParagraphFormattingOption.UseSuppressLineNumbers])
  else
    SetSuppressLineNumbersCore(Value);
end;

procedure TdxNativeParagraphPropertiesBase.Reset;
begin
  Options.ResetUse(TdxParagraphFormattingOptions.MaskUseAll);
  ResetOptions.ResetUse(TdxParagraphFormattingOptions.MaskUseAll);
end;

procedure TdxNativeParagraphPropertiesBase.Reset(const AMask: TdxRichEditParagraphPropertiesMask);
begin
  Options.ResetUse(AMask);
  ResetOptions.ResetUse(AMask);
end;

function TdxNativeParagraphPropertiesBase.GetAlignment: TdxNullableValue<TdxRichEditParagraphAlignment>;
begin
  if not Options.UseAlignment then
    Result := TdxNullableValue<TdxRichEditParagraphAlignment>.Null
  else
    Result := Info.Alignment;
end;

function TdxNativeParagraphPropertiesBase.GetBackColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Options.UseBackColor then
    Result := TdxNullableValue<TdxAlphaColor>.Null
  else
    Result := Info.BackColor;
end;

function TdxNativeParagraphPropertiesBase.GetContextualSpacing: TdxNullableBoolean;
begin
  if not Options.UseContextualSpacing then
    Result := TdxNullableBoolean.Null
  else
    Result := Info.ContextualSpacing;
end;

function TdxNativeParagraphPropertiesBase.GetFirstLineIndent: TdxNullableSingle;
begin
  if not Options.UseFirstLineIndent then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Info.FirstLineIndent);
end;

function TdxNativeParagraphPropertiesBase.GetFirstLineIndentType: TdxNullableValue<TdxRichEditParagraphFirstLineIndent>;
begin
  if not Options.UseFirstLineIndent then
    Result := TdxNullableValue<TdxRichEditParagraphFirstLineIndent>.Null
  else
    Result := Info.FirstLineIndentType;
end;

function TdxNativeParagraphPropertiesBase.GetKeepLinesTogether: TdxNullableBoolean;
begin
  if not Options.UseKeepLinesTogether then
    Result := TdxNullableBoolean.Null
  else
    Result := Info.KeepLinesTogether;
end;

function TdxNativeParagraphPropertiesBase.GetLeftIndent: TdxNullableSingle;
begin
  if not Options.UseLeftIndent then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Info.LeftIndent);
end;

function TdxNativeParagraphPropertiesBase.GetLineSpacing: TdxNullableSingle;
begin
  if not Options.UseLineSpacing then
    Result := TdxNullableSingle.Null
  else
  begin
    Result := Info.LineSpacing;
    if not GetLineSpacingRule.IsNull and (GetLineSpacingRule.Value = TdxRichEditParagraphLineSpacing.Multiple) then
      Result := Document.PointsToModelUnitsF(Result.Value * 12);
    Result := FDocument.ModelUnitsToUnitsF(Result);
  end;
end;

function TdxNativeParagraphPropertiesBase.GetLineSpacingMultiplier: TdxNullableSingle;
begin
  if not Options.UseLineSpacing then
    Result := TdxNullableSingle.Null
  else
    Result := GetLineSpacing;
end;

function TdxNativeParagraphPropertiesBase.GetLineSpacingRule: TdxNullableValue<TdxRichEditParagraphLineSpacing>;
begin
  if not Options.UseLineSpacing then
    Result := TdxNullableValue<TdxRichEditParagraphLineSpacing>.Null
  else
    Result := Info.LineSpacingType;
end;

function TdxNativeParagraphPropertiesBase.GetOutlineLevel: TdxNullableInteger;
begin
  if not Options.UseOutlineLevel then
    Result := TdxNullableInteger.Null
  else
    Result := Info.OutlineLevel;
end;

function TdxNativeParagraphPropertiesBase.GetPageBreakBefore: TdxNullableBoolean;
begin
  if not Options.UsePageBreakBefore then
    Result := TdxNullableBoolean.Null
  else
    Result := Info.PageBreakBefore;
end;

function TdxNativeParagraphPropertiesBase.GetRightIndent: TdxNullableSingle;
begin
  if not Options.UseRightIndent then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Info.RightIndent);
end;

function TdxNativeParagraphPropertiesBase.GetSpacingAfter: TdxNullableSingle;
begin
  if not Options.UseSpacingAfter then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Info.SpacingAfter);
end;

function TdxNativeParagraphPropertiesBase.GetSpacingBefore: TdxNullableSingle;
begin
  if not Options.UseSpacingBefore then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Info.SpacingBefore);
end;

function TdxNativeParagraphPropertiesBase.GetSuppressHyphenation: TdxNullableBoolean;
begin
  if not Options.UseSuppressHyphenation then
    Result := TdxNullableBoolean.Null
  else
    Result := Info.SuppressHyphenation;
end;

function TdxNativeParagraphPropertiesBase.GetSuppressLineNumbers: TdxNullableBoolean;
begin
  if not Options.UseSuppressLineNumbers then
    Result := TdxNullableBoolean.Null
  else
    Result := Info.SuppressLineNumbers;
end;

{ TdxNativeSimpleParagraphProperties }

constructor TdxNativeSimpleParagraphProperties.Create(ADocument: TdxNativeDocument;
  AParagraphProperties: TdxParagraphProperties);
begin
  inherited Create(ADocument);
  FModelParagraphProperties := AParagraphProperties;
end;

function TdxNativeSimpleParagraphProperties.GetInfo: TdxParagraphFormattingInfo;
begin
  Result := FModelParagraphProperties.Info.Info;
end;

function TdxNativeSimpleParagraphProperties.GetOptions: PdxParagraphFormattingOptions;
begin
  Result := @FModelParagraphProperties.Info.Options;
end;

procedure TdxNativeSimpleParagraphProperties.SetAlignmentCore(const Value: TdxRichEditParagraphAlignment);
begin
  FModelParagraphProperties.Alignment := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetBackColorCore(const Value: TdxAlphaColor);
begin
  FModelParagraphProperties.BackColor := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetContextualSpacingCore(const Value: Boolean);
begin
  FModelParagraphProperties.ContextualSpacing := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetFirstLineIndentCore(const Value: Integer);
begin
  FModelParagraphProperties.FirstLineIndent := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetFirstLineIndentTypeCore(const Value: TdxRichEditParagraphFirstLineIndent);
begin
  FModelParagraphProperties.FirstLineIndentType := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetKeepLinesTogetherCore(const Value: Boolean);
begin
  FModelParagraphProperties.KeepLinesTogether := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetLeftIndentCore(const Value: Integer);
begin
  FModelParagraphProperties.LeftIndent := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetLineSpacingCore(const Value: Single);
begin
  FModelParagraphProperties.LineSpacing := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetLineSpacingRuleCore(const Value: TdxRichEditParagraphLineSpacing);
begin
  FModelParagraphProperties.LineSpacingType := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetOutlineLevelCore(const Value: Integer);
begin
  FModelParagraphProperties.OutlineLevel := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetPageBreakBeforeCore(const Value: Boolean);
begin
  FModelParagraphProperties.PageBreakBefore := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetRightIndentCore(const Value: Integer);
begin
  FModelParagraphProperties.RightIndent := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetSpacingAfterCore(const Value: Integer);
begin
  FModelParagraphProperties.SpacingAfter := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetSpacingBeforeCore(const Value: Integer);
begin
  FModelParagraphProperties.SpacingBefore := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetSuppressHyphenationCore(const Value: Boolean);
begin
  FModelParagraphProperties.SuppressHyphenation := Value;
end;

procedure TdxNativeSimpleParagraphProperties.SetSuppressLineNumbersCore(const Value: Boolean);
begin
  FModelParagraphProperties.SuppressLineNumbers := Value;
end;

{ TdxNativeStyleParagraphProperties }

constructor TdxNativeStyleParagraphProperties.Create(ADocument: TdxNativeDocument;
  ATabs: TdxTabProperties; AParagraphProperties: TdxParagraphProperties);
begin
  inherited Create(ADocument, AParagraphProperties);
  FTabs := ATabs;
end;

function TdxNativeStyleParagraphProperties.BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
var
  ATabInfo: TdxTabFormattingInfo;
begin
  ATabInfo := FTabs.GetTabs;
  if ATabInfo = nil then
    Exit(nil);
  Result := TdxNativeParagraphProperties.CreateTabInfoCollection(Document, ATabInfo);
end;

procedure TdxNativeStyleParagraphProperties.EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);
var
  AInfo: TdxTabFormattingInfo;
begin
  if ATabs = nil then
    Exit;

  AInfo := TdxNativeParagraphProperties.CreateModelTabInfoCollection(Document, ATabs);
  try
    FTabs.SetTabs(AInfo);
  finally
    AInfo.Free;
  end;
end;

{ TdxNativeParagraphStyle }

constructor TdxNativeParagraphStyle.Create(ADocument: TdxNativeDocument; AInnerStyle: TdxParagraphStyle);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerStyle := AInnerStyle;
  FParagraphProperties := TdxNativeStyleParagraphProperties.Create(ADocument, AInnerStyle.Tabs, AInnerStyle.ParagraphProperties);
  FCharacterProperties := TdxNativeSimpleCharacterProperties.Create(AInnerStyle.CharacterProperties);
end;

function TdxNativeParagraphStyle.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FInnerStyle.DocumentModel);
end;

function TdxNativeParagraphStyle.GetName: string;
begin
  Result := FInnerStyle.StyleName;
end;

procedure TdxNativeParagraphStyle.SetName(const Value: string);
begin
  FInnerStyle.StyleName := Value;
end;

function TdxNativeParagraphStyle.GetIsDeleted: Boolean;
begin
  Result := FInnerStyle.Deleted;
end;

function TdxNativeParagraphStyle.GetParent: IdxRichEditParagraphStyle;
var
  AStyles: TdxNativeParagraphStyleCollection;
begin
  AStyles := TdxNativeParagraphStyleCollection(FDocument.ParagraphStyles);
  Result := AStyles.GetStyle(FInnerStyle.Parent);
end;

function TdxNativeParagraphStyle.GetPrimary: Boolean;
begin
  Result := InnerStyle.Primary;
end;

procedure TdxNativeParagraphStyle.SetParent(const Value: IdxRichEditParagraphStyle);
var
  AStyle: TdxParagraphStyle;
begin
  if Value <> nil then
    AStyle := TdxNativeParagraphStyle(Value).InnerStyle
  else
    AStyle := nil;
  FInnerStyle.Parent := AStyle;
end;

procedure TdxNativeParagraphStyle.SetPrimary(const Value: Boolean);
begin
  InnerStyle.Primary := Value;
end;

function TdxNativeParagraphStyle.BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
begin
  Result := FParagraphProperties.BeginUpdateTabs(AOnlyOwnTabs);
end;

procedure TdxNativeParagraphStyle.EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);
begin
  FParagraphProperties.EndUpdateTabs(ATabs);
end;

function TdxNativeParagraphStyle.GetNextStyle: IdxRichEditParagraphStyle;
var
  AStyles: TdxNativeParagraphStyleCollection;
begin
  AStyles := TdxNativeParagraphStyleCollection(FDocument.ParagraphStyles);
  Result := AStyles.GetStyle(FInnerStyle.NextParagraphStyle);
end;

procedure TdxNativeParagraphStyle.SetNextStyle(const Value: IdxRichEditParagraphStyle);
var
  AStyle: TdxParagraphStyle;
begin
  if Value <> nil then
    AStyle := TdxNativeParagraphStyle(Value).InnerStyle
  else
    AStyle := nil;
  FInnerStyle.NextParagraphStyle := AStyle;
end;

function TdxNativeParagraphStyle.GetLinkedStyle: IdxRichEditCharacterStyle;
var
  AStyles: TdxNativeCharacterStyleCollection;
begin
  AStyles := TdxNativeCharacterStyleCollection(FDocument.CharacterStyles);
  Result := AStyles.GetStyle(FInnerStyle.LinkedStyle);
end;

procedure TdxNativeParagraphStyle.SetLinkedStyle(const Value: IdxRichEditCharacterStyle);
var
  AStyle: TdxCharacterStyle;
begin
  if Value <> nil then
    AStyle := TdxNativeCharacterStyle(Value).InnerStyle
  else
    AStyle := nil;
  if AStyle = FInnerStyle.LinkedStyle then
    Exit;
  if FInnerStyle.HasLinkedStyle then
    DocumentModel.StyleLinkManager.DeleteLink(FInnerStyle);

  if AStyle <> nil then
    DocumentModel.StyleLinkManager.CreateLink(FInnerStyle, AStyle);
end;

function TdxNativeParagraphStyle.GetNumberingListIndex: Integer;
begin
  Result := InnerStyle.GetNumberingListIndex;
end;

procedure TdxNativeParagraphStyle.SetNumberingListIndex(const Value: Integer);
var
  AStyle: TdxParagraphStyle;
begin
  AStyle := FInnerStyle;
  if not FInnerStyle.Deleted and (DocumentModel.ParagraphStyles.IndexOf(FInnerStyle) >= 0) then
    ValidateNumberingListIndex(Value);
  FInnerStyle.DocumentModel.BeginUpdate;
  try
    AStyle.SetNumberingListIndex(Value);
  finally
    FInnerStyle.DocumentModel.EndUpdate;
  end;
end;

function TdxNativeParagraphStyle.GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
begin
  Result := FCharacterProperties;
end;

function TdxNativeParagraphStyle.GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
begin
  Result := FParagraphProperties;
end;

function TdxNativeParagraphStyle.GetListLevelIndex: Integer;
begin
  Result := InnerStyle.GetOwnListLevelIndex;
end;

procedure TdxNativeParagraphStyle.SetListLevelIndex(const Value: Integer);
var
  AStyle: TdxParagraphStyle;
begin
  AStyle := FInnerStyle;
  FInnerStyle.DocumentModel.BeginUpdate;
  try
    AStyle.SetNumberingListLevelIndex(Value);
  finally
    FInnerStyle.DocumentModel.EndUpdate;
  end;
end;

//function TdxNativeParagraphStyle.BeginUpdateTabs(AOnlyOwnTabs: Boolean): TdxTabInfoCollection;
//begin
//  Result := FParagraphProperties.BeginUpdateTabs(AOnlyOwnTabs);
//end;
//
//procedure TdxNativeParagraphStyle.EndUpdateTabs(ATabs: TdxTabInfoCollection);
//begin
//  FParagraphProperties.EndUpdateTabs(ATabs);
//end;

procedure TdxNativeParagraphStyle.ValidateStyleProperties;
begin
  ValidateNumberingListIndex(NumberingListIndex);
end;

procedure TdxNativeParagraphStyle.ValidateNumberingListIndex(AValue: Integer);
var
  ANumberingListCount: Integer;
begin
  ANumberingListCount := DocumentModel.NumberingLists.Count;
  if AValue >= ANumberingListCount then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionInvalidNumberingListIndex));
end;

{ TdxNativeParagraphProperties }

constructor TdxNativeParagraphProperties.Create(ADocument: TdxNativeSubDocument;
  AFrom, ALength: Integer);
begin
  inherited Create(ADocument);
  FFrom := AFrom;
  FLength := ALength;
  FResetOptions := TdxParagraphFormattingOptions.Create(TdxParagraphFormattingOptions.MaskUseAll);
  FProperties := CreateProperties;
end;

destructor TdxNativeParagraphProperties.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TdxNativeParagraphProperties.Apply;
var
  AModifier: TdxParagraphPropertiesModifier;
begin
  AModifier := TdxNativeApiParagraphPropertiesModifier.Create(FProperties, FResetOptions);
  try
    Document.PieceTable.ApplyParagraphFormatting(From, Length, AModifier);
  finally
    AModifier.Free;
  end;
end;

function TdxNativeParagraphProperties.BeginUpdateTabs(
  AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
var
  AModifier: TdxTabFormattingInfoModifier;
  AInfo: TdxTabFormattingInfo;
begin
  if AOnlyOwnTabs then
    AModifier := TdxOwnTabFormattingInfoModifier.Create(nil, Document.DocumentModel.DocumentProperties.DefaultTabWidth)
  else
    AModifier := TdxTabFormattingInfoModifier.Create(nil, Document.DocumentModel.DocumentProperties.DefaultTabWidth);
  try
    AInfo := Document.PieceTable.ObtainMergedParagraphsTabFormattingInfo(From, Length, AModifier);
  finally
    AModifier.Free;
  end;
  if AInfo = nil then
  begin
    Exit(nil);
  end;
  Result := CreateTabInfoCollection(Document, AInfo);
end;

procedure TdxNativeParagraphProperties.EndUpdateTabs(
  const ATabs: IdxRichEditTabInfoCollection);
var
  AInfo: TdxTabFormattingInfo;
  AModifier: TdxTabFormattingInfoModifier;
begin
  if ATabs = nil then
    Exit;
  AInfo := CreateModelTabInfoCollection(Document, ATabs);
  try
    AModifier := TdxTabFormattingInfoModifier.Create(AInfo, Document.DocumentModel.DocumentProperties.DefaultTabWidth);
    try
      Document.PieceTable.ApplyParagraphFormatting(From, Length, AModifier);
    finally
      AModifier.Free;
    end;
  finally
    AInfo.Free;
  end;
end;

function TdxNativeParagraphProperties.CreateProperties: TdxMergedParagraphProperties;
var
  AModifier: TdxParagraphPropertiesModifier;
begin
  AModifier := TdxNativeApiParagraphPropertiesModifier.Create(nil, FResetOptions);
  try
    Result := Document.PieceTable.ObtainMergedParagraphsProperties(From, Length, AModifier);
  finally
    AModifier.Free;
  end;
end;

class function TdxNativeParagraphProperties.CreateModelTabInfo(
  ADocument: TdxNativeSubDocument;
  const ATabInfo: IdxRichEditTabInfo): TdxTabInfo;
var
  APosition: Integer;
begin
  APosition := ADocument.UnitsToModelUnits(ATabInfo.Position);
  Result := TdxTabInfo.Create(APosition, ATabInfo.Alignment,
    ATabInfo.Leader, ATabInfo.Deleted, False);
end;

class function TdxNativeParagraphProperties.CreateModelTabInfoCollection(
  ADocument: TdxNativeSubDocument;
  const ATabs: IdxRichEditTabInfoCollection): TdxTabFormattingInfo;
var
  ACount, I: Integer;
begin
  Result := TdxTabFormattingInfo.Create;
  ACount := ATabs.Count;
  for I := 0 to ACount - 1 do
    Result.Add(CreateModelTabInfo(ADocument, ATabs[I]));
end;

class function TdxNativeParagraphProperties.CreateTabInfo(
  ADocument: TdxNativeSubDocument; const ATabInfo: TdxTabInfo): IdxRichEditTabInfo;
begin
  Result := TdxNativeTabInfo.Create;
  Result.Alignment := ATabInfo.Alignment;
  Result.Leader := ATabInfo.Leader;
  Result.Position := ADocument.ModelUnitsToUnits(ATabInfo.Position);
  Result.Deleted := ATabInfo.Deleted;
end;

class function TdxNativeParagraphProperties.CreateTabInfoCollection(
  ADocument: TdxNativeSubDocument;
  ATabs: TdxTabFormattingInfo): IdxRichEditTabInfoCollection;
var
  ACount, I: Integer;
  AResult: TdxNativeTabInfoCollection;
begin
  AResult := TdxNativeTabInfoCollection.Create;
  ACount := ATabs.Count;
  for I := 0 to ACount - 1 do
    AResult.Add(CreateTabInfo(ADocument, ATabs[I]));
  Result := AResult;
end;

function TdxNativeParagraphProperties.GetInfo: TdxParagraphFormattingInfo;
begin
  Result := FProperties.Info;
end;

function TdxNativeParagraphProperties.GetOptions: PdxParagraphFormattingOptions;
begin
  Result := @FProperties.Options;
end;

function TdxNativeParagraphProperties.GetResetOptions: PdxParagraphFormattingOptions;
begin
  Result := @FResetOptions;
end;

{ TdxNativeTabInfoCollection }

function TdxNativeTabInfoCollection.CreateNew: IdxRichEditTabInfo;
begin
  Result := TdxNativeTabInfo.Create;
end;

function TdxNativeTabInfoCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

{ TdxNativeTabInfo }

function TdxNativeTabInfo.GetAlignment: TdxRichEditTabAlignmentType;
begin
  Result := FAlignment;
end;

function TdxNativeTabInfo.GetDeleted: Boolean;
begin
  Result := FDeleted;
end;

function TdxNativeTabInfo.GetLeader: TdxRichEditTabLeaderType;
begin
  Result := FLeader;
end;

function TdxNativeTabInfo.GetPosition: Single;
begin
  Result := FPosition;
end;

procedure TdxNativeTabInfo.SetAlignment(
  const Value: TdxRichEditTabAlignmentType);
begin
  FAlignment := Value;
end;

procedure TdxNativeTabInfo.SetDeleted(const Value: Boolean);
begin
  FDeleted := Value;
end;

procedure TdxNativeTabInfo.SetLeader(const Value: TdxRichEditTabLeaderType);
begin
  FLeader := Value;
end;

procedure TdxNativeTabInfo.SetPosition(const Value: Single);
begin
  FPosition := Value;
end;

{ TdxNativeTableCellPropertiesBase }

constructor TdxNativeTableCellPropertiesBase.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

procedure TdxNativeTableCellPropertiesBase.Reset;
begin
  Properties.Info.Value := TdxTableCellPropertiesOptions.MaskUseNone;
end;

procedure TdxNativeTableCellPropertiesBase.Reset(const AMask: TdxRichEditTableCellPropertiesMask);
var
  AValue: Integer;
begin
  AValue := 0;
  if TdxRichEditUsedTableCellFormattingOption.UsePreferredWidth in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUsePreferredWidth;
  if TdxRichEditUsedTableCellFormattingOption.UseNoWrap in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseNoWrap;
  if TdxRichEditUsedTableCellFormattingOption.UseLeftPadding in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseLeftMargin;
  if TdxRichEditUsedTableCellFormattingOption.UseRightPadding in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseRightMargin;
  if TdxRichEditUsedTableCellFormattingOption.UseTopPadding in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseTopMargin;
  if TdxRichEditUsedTableCellFormattingOption.UseBottomPadding in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseBottomMargin;
  if TdxRichEditUsedTableCellFormattingOption.UseVerticalAlignment in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseVerticalAlignment;
  if TdxRichEditUsedTableCellFormattingOption.UseLeftBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseLeftBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseRightBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseRightBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseTopBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseTopBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseBottomBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseBottomBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseInsideHorizontalBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseInsideHorizontalBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseInsideVerticalBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseInsideVerticalBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseTopLeftDiagonalBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseTopLeftDiagonalBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseTopRightDiagonalBorder in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseTopRightDiagonalBorder;
  if TdxRichEditUsedTableCellFormattingOption.UseBackgroundColor in AMask then
    AValue := AValue or TdxTableCellPropertiesOptions.MaskUseBackgroundColor;

  Properties.ResetUse(AValue);
end;

function TdxNativeTableCellPropertiesBase.GetCellBackgroundColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Properties.UseBackgroundColor then
    Result := TdxNullableValue<TdxAlphaColor>.Null
  else
    Result := Properties.BackgroundColor;
end;

function TdxNativeTableCellPropertiesBase.GetCellBottomPadding: TdxNullableSingle;
begin
  if not Properties.UseBottomMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Bottom.Value);
end;

function TdxNativeTableCellPropertiesBase.GetCellLeftPadding: TdxNullableSingle;
begin
  if not Properties.UseLeftMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Left.Value);
end;

function TdxNativeTableCellPropertiesBase.GetCellRightPadding: TdxNullableSingle;
begin
  if not Properties.UseRightMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Right.Value);
end;

function TdxNativeTableCellPropertiesBase.GetCellTopPadding: TdxNullableSingle;
begin
  if not Properties.UseTopMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Top.Value);
end;

function TdxNativeTableCellPropertiesBase.GetNoWrap: TdxNullableBoolean;
begin
  if not Properties.UseNoWrap then
    Result := TdxNullableBoolean.Null
  else
    Result := Properties.NoWrap;
end;

function TdxNativeTableCellPropertiesBase.GetTableCellBorders: IdxRichEditTableCellBorders;
begin
  if FTableCellBorders = nil then
    FTableCellBorders := CreateTableCellBorders;
  Result := FTableCellBorders;
end;

function TdxNativeTableCellPropertiesBase.GetVerticalAlignment: TdxNullableValue<TdxRichEditTableCellVerticalAlignment>;
begin
  if not Properties.UseVerticalAlignment then
    Result := TdxNullableValue<TdxRichEditTableCellVerticalAlignment>.Null
  else
    Result := TdxNativeTableCellPropertiesBase.VerticalAlignmentMap[Properties.VerticalAlignment];
end;

procedure TdxNativeTableCellPropertiesBase.SetCellBackgroundColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseBackgroundColor])
  else
    Properties.BackgroundColor := Value.Value;
end;

procedure TdxNativeTableCellPropertiesBase.SetCellBottomPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseBottomPadding])
  else
  begin
    Properties.CellMargins.Bottom.Value := Document.UnitsToModelUnits(Value.Value);
    Properties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTableCellPropertiesBase.SetCellLeftPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseLeftPadding])
  else
  begin
    Properties.CellMargins.Left.Value := Document.UnitsToModelUnits(Value.Value);
    Properties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTableCellPropertiesBase.SetCellRightPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseRightPadding])
  else
  begin
    Properties.CellMargins.Right.Value := Document.UnitsToModelUnits(Value.Value);
    Properties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTableCellPropertiesBase.SetCellTopPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseTopPadding])
  else
  begin
    Properties.CellMargins.Top.Value := Document.UnitsToModelUnits(Value.Value);
    Properties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTableCellPropertiesBase.SetNoWrap(const Value: TdxNullableBoolean);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseNoWrap])
  else
    Properties.NoWrap := Value.Value;
end;

procedure TdxNativeTableCellPropertiesBase.SetVerticalAlignment(const Value: TdxNullableValue<TdxRichEditTableCellVerticalAlignment>);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableCellFormattingOption.UseVerticalAlignment])
  else
    Properties.VerticalAlignment := TdxNativeTableCellPropertiesBase.ModelVerticalAlignmentMap[Value.Value];
end;

{ TdxNativeBorderBase }

constructor TdxNativeBorderBase.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeBorderBase.GetLineColor: TdxAlphaColor;
begin
  Result := GetLineColorCore;
end;

function TdxNativeBorderBase.GetLineStyle: TdxRichEditTableBorderLineStyle;
begin
  Result := TdxRichEditTableBorderLineStyle(GetLineStyleCore);
end;

function TdxNativeBorderBase.GetLineThickness: Single;
begin
  Result := FDocument.UnitConverter.Converter.ModelUnitsToPointsF(GetLineThicknessCore);
end;

procedure TdxNativeBorderBase.SetLineColor(const Value: TdxAlphaColor);
begin
  SetLineColorCore(Value);
end;

procedure TdxNativeBorderBase.SetLineStyle(const Value: TdxRichEditTableBorderLineStyle);
begin
  SetLineStyleCore(TdxBorderLineStyle(Value));
end;

procedure TdxNativeBorderBase.SetLineThickness(const Value: Single);
begin
  SetLineThicknessCore(Round(FDocument.UnitConverter.Converter.PointsToModelUnitsF(Value)));
end;

{ TdxNativeStyleBorderBase }

constructor TdxNativeStyleBorderBase.Create(ADocument: TdxNativeSubDocument; ABorder: TdxBorderBase);
begin
  inherited Create(ADocument);
  FBorder := ABorder;
end;

function TdxNativeStyleBorderBase.GetLineColorCore: TdxAlphaColor;
begin
  Result := FBorder.Color;
end;

function TdxNativeStyleBorderBase.GetLineStyleCore: TdxBorderLineStyle;
begin
  Result := FBorder.Style;
end;

function TdxNativeStyleBorderBase.GetLineThicknessCore: Integer;
begin
  Result := FBorder.Width;
end;

procedure TdxNativeStyleBorderBase.SetLineColorCore(const Value: TdxAlphaColor);
begin
  FBorder.Color := Value;
end;

procedure TdxNativeStyleBorderBase.SetLineStyleCore(const Value: TdxBorderLineStyle);
begin
  FBorder.Style := Value;
end;

procedure TdxNativeStyleBorderBase.SetLineThicknessCore(const Value: Integer);
begin
  FBorder.Width := Value;
end;

{ TdxNativeTableCellBordersBase }

constructor TdxNativeTableCellBordersBase.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeTableCellBordersBase.GetBottom: IdxRichEditTableCellBorder;
begin
  if FBottom = nil then
    FBottom := CreateBottom;
  Result := FBottom;
end;

function TdxNativeTableCellBordersBase.GetLeft: IdxRichEditTableCellBorder;
begin
  if FLeft = nil then
    FLeft := CreateLeft;
  Result := FLeft;
end;

function TdxNativeTableCellBordersBase.GetRight: IdxRichEditTableCellBorder;
begin
  if FRight = nil then
    FRight := CreateRight;
  Result := FRight;
end;

function TdxNativeTableCellBordersBase.GetTop: IdxRichEditTableCellBorder;
begin
  if FTop = nil then
    FTop := CreateTop;
  Result := FTop;
end;

{ TdxNativeStyleTableCellBorders }

constructor TdxNativeStyleTableCellBorders.Create(ADocument: TdxNativeSubDocument;
  ABorders: TdxTableCellBorders);
begin
  inherited Create(ADocument);
  FBorders := ABorders;
end;

function TdxNativeStyleTableCellBorders.CreateBottom: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeStyleTableCellBorder.Create(Document, Borders.BottomBorder);
end;

function TdxNativeStyleTableCellBorders.CreateLeft: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeStyleTableCellBorder.Create(Document, Borders.LeftBorder);
end;

function TdxNativeStyleTableCellBorders.CreateRight: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeStyleTableCellBorder.Create(Document, Borders.RightBorder);
end;

function TdxNativeStyleTableCellBorders.CreateTop: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeStyleTableCellBorder.Create(Document, Borders.TopBorder);
end;

{ TdxNativeStyleTableCellProperties }

constructor TdxNativeStyleTableCellProperties.Create(
  ADocument: TdxNativeDocument; AProperties: TdxTableCellProperties);
begin
  inherited Create(ADocument);
  FProperties := AProperties;
end;

function TdxNativeStyleTableCellProperties.CreateTableCellBorders: IdxRichEditTableCellBorders;
begin
  Result := TdxNativeStyleTableCellBorders.Create(Document, Properties.Borders);
end;

function TdxNativeStyleTableCellProperties.GetProperties: TdxTableCellProperties;
begin
  Result := FProperties;
end;

{ TdxNativeTableBordersBase }

constructor TdxNativeTableBordersBase.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeTableBordersBase.GetBottom: IdxRichEditTableBorder;
begin
  if FBottom = nil then
    FBottom := CreateBottom;
  Result := FBottom;
end;

function TdxNativeTableBordersBase.GetInsideHorizontalBorder: IdxRichEditTableBorder;
begin
  if FInsideHorizontalBorder = nil then
    FInsideHorizontalBorder := CreateInsideHorizontalBorder;
  Result := FInsideHorizontalBorder;
end;

function TdxNativeTableBordersBase.GetInsideVerticalBorder: IdxRichEditTableBorder;
begin
  if FInsideVerticalBorder = nil then
    FInsideVerticalBorder := CreateInsideVerticalBorder;
  Result := FInsideVerticalBorder;
end;

function TdxNativeTableBordersBase.GetLeft: IdxRichEditTableBorder;
begin
  if FLeft = nil then
    FLeft := CreateLeft;
  Result := FLeft;
end;

function TdxNativeTableBordersBase.GetRight: IdxRichEditTableBorder;
begin
  if FRight = nil then
    FRight := CreateRight;
  Result := FRight;
end;

function TdxNativeTableBordersBase.GetTop: IdxRichEditTableBorder;
begin
  if FTop = nil then
    FTop := CreateTop;
  Result := FTop;
end;

{ TdxNativeStyleTableBorders }

constructor TdxNativeStyleTableBorders.Create(ADocument: TdxNativeSubDocument;
  ABorders: TdxTableBorders);
begin
  inherited Create(ADocument);
  FBorders := ABorders;
end;

function TdxNativeStyleTableBorders.CreateBottom: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.BottomBorder)
end;

function TdxNativeStyleTableBorders.CreateInsideHorizontalBorder: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.InsideHorizontalBorder)
end;

function TdxNativeStyleTableBorders.CreateInsideVerticalBorder: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.InsideVerticalBorder)
end;

function TdxNativeStyleTableBorders.CreateLeft: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.LeftBorder)
end;

function TdxNativeStyleTableBorders.CreateRight: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.RightBorder)
end;

function TdxNativeStyleTableBorders.CreateTop: IdxRichEditTableBorder;
begin
  Result := TdxNativeStyleTableBorder.Create(Document, Borders.TopBorder)
end;

{ TdxNativeTablePropertiesBase }

constructor TdxNativeTablePropertiesBase.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

procedure TdxNativeTablePropertiesBase.Reset;
begin
  Properties.ResetAllUse;
end;

procedure TdxNativeTablePropertiesBase.Reset(const AMask: TdxRichEditTablePropertiesMask);
begin
  Properties.ResetUse(ApiMaskToModelMask(AMask));
end;

class function TdxNativeTablePropertiesBase.ApiMaskToModelMask(const AMask: TdxRichEditTablePropertiesMask): Integer;
begin
  Result := 0;
  if TdxRichEditUsedTableFormattingOption.UseLeftPadding in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseLeftMargin;
  if TdxRichEditUsedTableFormattingOption.UseRightPadding in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseRightMargin;
  if TdxRichEditUsedTableFormattingOption.UseTopPadding in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTopMargin;
  if TdxRichEditUsedTableFormattingOption.UseBottomPadding in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseBottomMargin;
  if TdxRichEditUsedTableFormattingOption.UseCellSpacing in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseCellSpacing;
  if TdxRichEditUsedTableFormattingOption.UseTableIndent in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableIndent;
  if TdxRichEditUsedTableFormattingOption.UseTableLayout in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableLayout;
  if TdxRichEditUsedTableFormattingOption.UseTableLook in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableLook;
  if TdxRichEditUsedTableFormattingOption.UsePreferredWidth in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUsePreferredWidth;
  if TdxRichEditUsedTableFormattingOption.UseTableStyleColBandSize in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableStyleColBandSize;
  if TdxRichEditUsedTableFormattingOption.UseTableStyleRowBandSize in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableStyleRowBandSize;
  if TdxRichEditUsedTableFormattingOption.UseLeftBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseLeftBorder;
  if TdxRichEditUsedTableFormattingOption.UseRightBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseRightBorder;
  if TdxRichEditUsedTableFormattingOption.UseTopBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTopBorder;
  if TdxRichEditUsedTableFormattingOption.UseBottomBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseBottomBorder;
  if TdxRichEditUsedTableFormattingOption.UseInsideHorizontalBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder;
  if TdxRichEditUsedTableFormattingOption.UseInsideVerticalBorder in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseInsideVerticalBorder;
  if TdxRichEditUsedTableFormattingOption.UseBackgroundColor in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseBackgroundColor;
  if TdxRichEditUsedTableFormattingOption.UseTableAlignment in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseTableAlignment;
  if TdxRichEditUsedTableFormattingOption.UseBorders in AMask then
    Result := Result or TdxTablePropertiesOptions.MaskUseBorders;
end;

function TdxNativeTablePropertiesBase.GetBottomPadding: TdxNullableSingle;
begin
  if not Properties.UseBottomMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Bottom.Value);
end;

function TdxNativeTablePropertiesBase.GetLeftPadding: TdxNullableSingle;
begin
  if not Properties.UseLeftMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Left.Value);
end;

function TdxNativeTablePropertiesBase.GetRightPadding: TdxNullableSingle;
begin
  if not Properties.UseRightMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Right.Value);
end;

function TdxNativeTablePropertiesBase.GetTableAlignment: TdxNullableValue<TdxRichEditTableRowAlignment>;
begin
  if not Properties.UseTableAlignment then
    Result := TdxNullableValue<TdxRichEditTableRowAlignment>.Null
  else
    Result := Properties.TableAlignment;
end;

function TdxNativeTablePropertiesBase.GetTableBackgroundColor: TdxNullableValue<TdxAlphaColor>;
begin
  if not Properties.UseBackgroundColor then
    Result := TdxNullableValue<TdxAlphaColor>.Null
  else
    Result := Properties.BackgroundColor;
end;

function TdxNativeTablePropertiesBase.GetTableBorders: IdxRichEditTableBorders;
begin
  if FTableBorders = nil then
    FTableBorders := CreateNativeStyleTableBorders;
  Result := FTableBorders;
end;

function TdxNativeTablePropertiesBase.GetTableCellSpacing: TdxNullableSingle;
begin
  if not Properties.UseCellSpacing then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellSpacing.Value);
end;

function TdxNativeTablePropertiesBase.GetTableLayout: TdxNullableValue<TdxRichEditTableLayoutType>;
begin
  if not Properties.UseTableLayout then
    Result := TdxNullableValue<TdxRichEditTableLayoutType>.Null
  else
    Result := Properties.TableLayout;
end;

function TdxNativeTablePropertiesBase.GetTopPadding: TdxNullableSingle;
begin
  if not Properties.UseTopMargin then
    Result := TdxNullableSingle.Null
  else
    Result := Document.ModelUnitsToUnits(Properties.CellMargins.Top.Value);
end;

procedure TdxNativeTablePropertiesBase.SetBottomPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseBottomPadding])
  else
  begin
    Properties.CellMargins.Bottom.Value := Document.UnitsToModelUnits(Value);
    Properties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTablePropertiesBase.SetLeftPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseLeftPadding])
  else
  begin
    Properties.CellMargins.Left.Value := Document.UnitsToModelUnits(Value);
    Properties.CellMargins.Left.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTablePropertiesBase.SetRightPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseRightPadding])
  else
  begin
    Properties.CellMargins.Right.Value := Document.UnitsToModelUnits(Value);
    Properties.CellMargins.Right.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

procedure TdxNativeTablePropertiesBase.SetTableAlignment(const Value: TdxNullableValue<TdxRichEditTableRowAlignment>);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseTableAlignment])
  else
    Properties.TableAlignment := Value;
end;

procedure TdxNativeTablePropertiesBase.SetTableBackgroundColor(const Value: TdxNullableValue<TdxAlphaColor>);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseBackgroundColor])
  else
    Properties.BackgroundColor := Value;
end;

procedure TdxNativeTablePropertiesBase.SetTableCellSpacing(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseCellSpacing])
  else
    Properties.CellSpacing.Value := Document.UnitsToModelUnits(Value.Value);
end;

procedure TdxNativeTablePropertiesBase.SetTableLayout(const Value: TdxNullableValue<TdxRichEditTableLayoutType>);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseTableLayout])
  else
    Properties.TableLayout := Value.Value;
end;

procedure TdxNativeTablePropertiesBase.SetTopPadding(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    Reset([TdxRichEditUsedTableFormattingOption.UseTopPadding])
  else
  begin
    Properties.CellMargins.Top.Value := Document.UnitsToModelUnits(Value);
    Properties.CellMargins.Top.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

{ TdxNativeStyleTableProperties }

constructor TdxNativeStyleTableProperties.Create(ADocument: TdxNativeDocument;
  AProperties: TdxTableProperties);
begin
  inherited Create(ADocument);
  FProperties := AProperties;
end;

function TdxNativeStyleTableProperties.CreateNativeStyleTableBorders: IdxRichEditTableBorders;
begin
  Result := TdxNativeStyleTableBorders.Create(Document, Properties.Borders);
end;

function TdxNativeStyleTableProperties.GetProperties: TdxTableProperties;
begin
  Result := FProperties;
end;

{ TdxNativeTableConditionalStyleProperties }

constructor TdxNativeTableConditionalStyleProperties.Create(AOwner: TdxNativeTableStyle);
begin
  inherited Create;
  FOwner := AOwner;
  InitializeItems;
end;

destructor TdxNativeTableConditionalStyleProperties.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxNativeTableConditionalStyleProperties.CreateConditionalStyle(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
begin
  if FItems[ACondition] = nil then
  begin
    if FOwner.InnerStyle.ConditionalStyleProperties[ACondition] = nil then
      FOwner.InnerStyle.ConditionalStyleProperties.AddStyle(TdxTableConditionalStyle.Create(FOwner.InnerStyle, ACondition));
    FItems[ACondition] := TdxNativeTableConditionalStyle.Create(FOwner.Document, FOwner.InnerStyle.ConditionalStyleProperties[ACondition]);
  end;
  Result := Items[ACondition];
end;

function TdxNativeTableConditionalStyleProperties.GetItem(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
begin
  Result := FItems[ACondition];
end;

function TdxNativeTableConditionalStyleProperties.GetOwner: IdxRichEditTableStyle;
begin
  Result := FOwner;
end;

procedure TdxNativeTableConditionalStyleProperties.InitializeItems;
var
  I: TdxConditionalTableStyleFormattingType;
begin
  FItems := TDictionary<TdxConditionalTableStyleFormattingType, IdxRichEditTableConditionalStyle>.Create;
  for I := Low(TdxConditionalTableStyleFormattingType) to High(TdxConditionalTableStyleFormattingType) do
    FItems.Add(I, nil);
end;

{ TdxNativeTableStyle }

constructor TdxNativeTableStyle.Create(ADocument: TdxNativeDocument; AInnerStyle: TdxTableStyle);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerStyle := AInnerStyle;
  FParagraphProperties := TdxNativeStyleParagraphProperties.Create(ADocument, AInnerStyle.Tabs, AInnerStyle.ParagraphProperties);
  FCharacterProperties := TdxNativeSimpleCharacterProperties.Create(AInnerStyle.CharacterProperties);
  FTableCellProperties := TdxNativeStyleTableCellProperties.Create(ADocument, AInnerStyle.TableCellProperties);
  FTableProperties := TdxNativeStyleTableProperties.Create(ADocument, AInnerStyle.TableProperties);
  FConditionalStyleProperties := TdxNativeTableConditionalStyleProperties.Create(Self);
end;

function TdxNativeTableStyle.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FInnerStyle.DocumentModel);
end;

function TdxNativeTableStyle.GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
begin
  Result := FCharacterProperties;
end;

function TdxNativeTableStyle.GetConditionalStyleProperties: IdxRichEditTableConditionalStyleProperties;
begin
  Result := FConditionalStyleProperties;
end;

function TdxNativeTableStyle.GetIsDeleted: Boolean;
begin
  Result := InnerStyle.Deleted;
end;

function TdxNativeTableStyle.GetName: string;
begin
  Result := InnerStyle.StyleName;
end;

function TdxNativeTableStyle.GetParent: IdxRichEditTableStyle;
var
  AStyles: TdxNativeTableStyleCollection;
begin
  AStyles := TdxNativeTableStyleCollection(Document.TableStyles);
  Result := AStyles.GetStyle(InnerStyle.Parent);
end;

function TdxNativeTableStyle.GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
begin
  Result := FParagraphProperties;
end;

function TdxNativeTableStyle.GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
begin
  Result := FTableCellProperties;
end;

function TdxNativeTableStyle.GetTableProperties: IdxRichEditTablePropertiesBase;
begin
  Result := FTableProperties;
end;

procedure TdxNativeTableStyle.SetName(const Value: string);
begin
  InnerStyle.StyleName := Value;
end;

procedure TdxNativeTableStyle.SetParent(const Value: IdxRichEditTableStyle);
var
  AStyle: TdxTableStyle;
begin
  if Value = nil then
    AStyle := nil
  else
    AStyle := TdxNativeTableStyle(Value).InnerStyle;
  InnerStyle.Parent := AStyle;
end;

{ TdxNativeTableConditionalStyle }

constructor TdxNativeTableConditionalStyle.Create(ADocument: TdxNativeDocument; AInnerStyle: TdxTableConditionalStyle);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerStyle := AInnerStyle;
end;

function TdxNativeTableConditionalStyle.GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
begin
  if FCharacterProperties = nil then
    FCharacterProperties := TdxNativeSimpleCharacterProperties.Create(FInnerStyle.CharacterProperties);
  Result := FCharacterProperties;
end;

function TdxNativeTableConditionalStyle.GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
begin
  if FParagraphProperties = nil then
    FParagraphProperties := TdxNativeStyleParagraphProperties.Create(Document, InnerStyle.Tabs, InnerStyle.ParagraphProperties);
  Result := FParagraphProperties;
end;

function TdxNativeTableConditionalStyle.GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
begin
  if FTableCellProperties = nil then
    FTableCellProperties := TdxNativeStyleTableCellProperties.Create(Document, InnerStyle.TableCellProperties);
  Result := FTableCellProperties;
end;

function TdxNativeTableConditionalStyle.GetTableProperties: IdxRichEditTablePropertiesBase;
begin
  if FTableProperties = nil then
    FTableProperties := TdxNativeStyleTableProperties.Create(Document, InnerStyle.TableProperties);
  Result := FTableProperties;
end;

{ TdxNativeReadOnlyCollectionBase }

constructor TdxNativeReadOnlyCollectionBase<TItem, TModelItem>.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

destructor TdxNativeReadOnlyCollectionBase<TItem, TModelItem>.Destroy;
begin
  inherited Destroy;
end;

function TdxNativeReadOnlyCollectionBase<TItem, TModelItem>.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocument.DocumentModel;
end;

function TdxNativeReadOnlyCollectionBase<TItem, TModelItem>.GetItem(AItem: TModelItem): TItem;
begin
  if AItem = nil then
    Exit(nil);
  Result := CreateNew(AItem);
end;

procedure TdxNativeReadOnlyCollectionBase<TItem, TModelItem>.Invalidate;
begin
end;


{ TdxNativeReadOnlyStyleCollectionBase }

function TdxNativeReadOnlyStyleCollectionBase<TStyle, TModelStyle>.GetStyle(AStyle: TModelStyle): TStyle;
begin
  Result := GetItem(AStyle);
end;

{ TdxNativeStyleCollectionBase }

function TdxNativeStyleCollectionBase<TStyle, TModelStyle>.GetItem(Index: Integer): TStyle;
begin
  Result := GetStyle(InnerStyles[index]);
end;

function TdxNativeStyleCollectionBase<TStyle, TModelStyle>.GetItem(const AName: string): TStyle;
begin
  Result := GetStyle(InnerStyles.GetStyleByName(AName));
end;

function TdxNativeStyleCollectionBase<TStyle, TModelStyle>.GetCount: Integer;
begin
  Result := InnerStyles.Count;
end;

function TdxNativeStyleCollectionBase<TStyle, TModelStyle>.CreateNew: TStyle;
var
  AInnerStyle: TModelStyle;
begin
  AInnerStyle := CreateModelStyle;
  Result := GetStyle(AInnerStyle);
end;

procedure TdxNativeStyleCollectionBase<TStyle, TModelStyle>.Add(const AStyle: TStyle);
var
  AInnerStyle: TModelStyle;
begin
  AInnerStyle := GetModelStyle(AStyle);
  if InnerStyles.IndexOf(AInnerStyle) = -1 then
  begin
    ValidateStyleProperties(AStyle);
    InnerStyles.Add(AInnerStyle);
  end;
end;

procedure TdxNativeStyleCollectionBase<TStyle, TModelStyle>.ValidateStyleProperties(const AStyle: TStyle);
begin
end;

procedure TdxNativeStyleCollectionBase<TStyle, TModelStyle>.Delete(const AStyle: TStyle);
var
  AInnerStyle: TModelStyle;
begin
  AInnerStyle := GetModelStyle(AStyle);
  InnerStyles.Delete(AInnerStyle);
end;

{ TdxNativeParagraphStyleCollection }

function TdxNativeParagraphStyleCollection.CreateModelStyle: TdxParagraphStyle;
begin
  Result := TdxParagraphStyle.Create(DocumentModel)
end;

function TdxNativeParagraphStyleCollection.CreateNew(AItem: TdxParagraphStyle): IdxRichEditParagraphStyle;
begin
  Result := TdxNativeParagraphStyle.Create(Document, AItem);
end;

function TdxNativeParagraphStyleCollection.GetInnerStyles: TdxStyleCollectionBase;
begin
  Result := DocumentModel.ParagraphStyles;
end;

function TdxNativeParagraphStyleCollection.GetModelStyle(const AStyle: IdxRichEditParagraphStyle): TdxParagraphStyle;
begin
  Result := TdxNativeParagraphStyle(AStyle).InnerStyle;
end;

procedure TdxNativeParagraphStyleCollection.ValidateStyleProperties(const AStyle: IdxRichEditParagraphStyle);
var
  ANativeStyle: TdxNativeParagraphStyle;
begin
  inherited ValidateStyleProperties(AStyle);
  ANativeStyle := TdxNativeParagraphStyle(AStyle);
  ANativeStyle.ValidateStyleProperties;
end;

{ TdxNativeCharacterStyleCollection }

function TdxNativeCharacterStyleCollection.CreateModelStyle: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle.Create(DocumentModel)
end;

function TdxNativeCharacterStyleCollection.CreateNew(AItem: TdxCharacterStyle): IdxRichEditCharacterStyle;
begin
  Result := TdxNativeCharacterStyle.Create(Document, AItem);
end;

function TdxNativeCharacterStyleCollection.GetInnerStyles: TdxStyleCollectionBase;
begin
  Result := DocumentModel.CharacterStyles;
end;

function TdxNativeCharacterStyleCollection.GetModelStyle(const AStyle: IdxRichEditCharacterStyle): TdxCharacterStyle;
begin
  Result := TdxNativeCharacterStyle(AStyle).InnerStyle;
end;

{ TdxNativeTableStyleCollection }

function TdxNativeTableStyleCollection.GetInnerStyles: TdxStyleCollectionBase;
begin
  Result := DocumentModel.TableStyles;
end;

function TdxNativeTableStyleCollection.CreateNew(AItem: TdxTableStyle): IdxRichEditTableStyle;
begin
  Result := TdxNativeTableStyle.Create(Document, AItem);
end;

function TdxNativeTableStyleCollection.CreateModelStyle: TdxTableStyle;
begin
  Result := TdxTableStyle.Create(DocumentModel);
end;

function TdxNativeTableStyleCollection.GetModelStyle(const AStyle: IdxRichEditTableStyle): TdxTableStyle;
begin
  Result := TdxNativeTableStyle(AStyle).InnerStyle;
end;

{ TdxNativeDefaultParagraphProperties }

procedure TdxNativeDefaultParagraphProperties.Reset;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

procedure TdxNativeDefaultParagraphProperties.Reset(const AMask: TdxRichEditParagraphPropertiesMask);
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

{ TdxNativeDefaultCharacterProperties }

procedure TdxNativeDefaultCharacterProperties.Reset;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

procedure TdxNativeDefaultCharacterProperties.Reset(
  const AMask: TdxRichEditCharacterPropertiesMask);
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

{ TdxNativeDefaultTableProperties }

procedure TdxNativeDefaultTableProperties.Reset;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

procedure TdxNativeDefaultTableProperties.Reset(
  const AMask: TdxRichEditTablePropertiesMask);
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCantResetDefaultProperties));
end;

end.

