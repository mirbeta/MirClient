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

unit dxRichEdit.NativeApi;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, DB, Classes, Generics.Defaults, Generics.Collections,
  Graphics, Rtti, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.OfficeImage;

type
  IdxRichEditParagraph = interface;
  IdxRichEditTable = interface;
  IdxRichEditTableRow = interface;
  IdxRichEditCharacterStyle = interface;
  IdxRichEditParagraphStyle = interface;
  IdxRichEditTableStyle = interface;
  IdxRichEditNumberingList = interface;
  IdxRichEditSubDocument = interface;
  IdxRichEditDocument = interface;

  TdxRichEditDocumentFormat = (
    Undefined,
    PlainText,
    Rtf,
    Html,
    OpenXml,
    Doc
  );

  TdxRichEditInsertOptions = (KeepSourceFormatting, MatchDestinationFormatting);

  TdxRichEditDocumentUnit = (
    Document,
    Inch,
    Millimeter,
    Centimeter,
    Point);

  { IdxReadOnlyList<T> }

  IdxReadOnlyList<T> = interface
    function GetItem(Index: Integer): T;
    function GetCount: Integer;

    property Self[Index: Integer]: T read GetItem; default;
    property Count: Integer read GetCount;
  end;

  { IdxRichEditList<T> }

  IdxRichEditList<T: IInterface> = interface(IdxReadOnlyList<T>)
    function Add(const Value: T): Integer;
    procedure Insert(Index: Integer; const Value: T);

    function Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    function Extract(const Value: T): T;
  end;

  { IdxRichEditDocumentContainer }

  IdxRichEditDocumentContainer = interface(IdxBatchUpdateable)
  ['{7F27E5B3-961C-4863-84BC-C6D8CFCFB66D}']
    function GetDocument: IdxRichEditDocument;

    property Document: IdxRichEditDocument read GetDocument;
  end;

  IdxInnerRichEditDocumentContainerOwner = interface
  ['{5147D9F0-1517-4E21-96EA-D98D6A2E4083}']
    function CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
  end;

  { IdxRichEditDocumentPosition }

  IdxRichEditDocumentPosition = interface
  ['{BAAA5FBF-304C-4294-9AAA-CA68DEC84CF9}']
    function Equals(AObj: TObject): Boolean;
    function CompareTo(const AOther: IdxRichEditDocumentPosition): Integer;
    function GetHashCode: Integer;
    function GetLogPosition: Integer;
    function ToString: string;
    function ToInt: Integer;

    property LogPosition: Integer read GetLogPosition;
  end;

  { IdxRichEditDocumentRange }

  IdxRichEditDocumentRange = interface
    function GetEnd: IdxRichEditDocumentPosition;
    function GetLength: Integer;
    function GetStart: IdxRichEditDocumentPosition;

    function Contains(const APos: IdxRichEditDocumentPosition): Boolean;

    property &End: IdxRichEditDocumentPosition read GetEnd;
    property Length: Integer read GetLength;
    property Start: IdxRichEditDocumentPosition read GetStart;
  end;


  TdxRichEditSearchOption = (
    CaseSensitive,
    WholeWord);
  TdxRichEditSearchOptions = set of TdxRichEditSearchOption;

  TdxRichEditSearchDirection = (
    Forward,
    Backward);

  { IdxRichEditSearchResult }

  IdxRichEditSearchResult = interface
    function GetCurrentResult: IdxRichEditDocumentRange;

    procedure Reset;
    function FindNext: Boolean;
    procedure Replace(const AReplaceWith: string);

    property CurrentResult: IdxRichEditDocumentRange read GetCurrentResult;
  end;

  { IdxRichEditRegexSearchGroup }

  IdxRichEditRegexSearchGroup = interface
    function GetLength: Integer;
    function GetPosition: IdxRichEditDocumentPosition;
    function GetValue: string;

    function GetRange: IdxRichEditDocumentRange;

    property Length: Integer read GetLength;
    property Position: IdxRichEditDocumentPosition read GetPosition;
    property Value: string read GetValue;
  end;

  IdxRichEditRegexSearchGroupCollection = interface(IdxReadOnlyList<IdxRichEditRegexSearchGroup>)
  end;

  { IdxRichEditRegexSearchMatch }

  IdxRichEditRegexSearchMatch = interface(IdxRichEditRegexSearchGroup)
    function GetGroups: IdxRichEditRegexSearchGroupCollection;
    property Groups: IdxRichEditRegexSearchGroupCollection read GetGroups;
  end;

  { IdxRichEditRegexSearchResult }

  IdxRichEditRegexSearchResult = interface(IdxRichEditSearchResult)
    function GetMatch: IdxRichEditRegexSearchMatch;
    property Match: IdxRichEditRegexSearchMatch read GetMatch;
  end;


  { IdxRichEditCharacterPropertiesBase }

  TdxRichEditUsedCharacterFormattingOption = (
    UseFontName,
    UseDoubleFontSize,
    UseFontBold,
    UseFontItalic,
    UseFontStrikeoutType,
    UseFontUnderlineType,
    UseAllCaps,
    UseForeColor,
    UseBackColor,
    UseUnderlineColor,
    UseStrikeoutColor,
    UseUnderlineWordsOnly,
    UseStrikeoutWordsOnly,
    UseScript,
    UseHidden,
    UseNoProof);

  TdxRichEditCharacterPropertiesMask = set of TdxRichEditUsedCharacterFormattingOption;

  TdxRichEditStrikeoutType = (
    None = 0,
    Single = 1,
    Double = 2);

  TdxRichEditUnderlineType = (
    None = 0,
    Single = 1,
    Dotted = 2,
    Dashed = 3,
    DashDotted = 4,
    DashDotDotted = 5,
    Double = 6,
    HeavyWave = 7,
    LongDashed = 8,
    ThickSingle = 9,
    ThickDotted = 10,
    ThickDashed = 11,
    ThickDashDotted = 12,
    ThickDashDotDotted = 13,
    ThickLongDashed = 14,
    DoubleWave = 15,
    Wave = 16,
    DashSmallGap = 17);

  IdxRichEditCharacterPropertiesBase = interface
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

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditCharacterPropertiesMask); overload;

    property AllCaps: TdxNullableBoolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxNullableValue<TdxAlphaColor> read GetBackColor write SetBackColor;
    property Bold: TdxNullableBoolean read GetBold write SetBold;
    property FontName: TdxNullableString read GetFontName write SetFontName;
    property FontSize: TdxNullableSingle  read GetFontSize write SetFontSize;
    property ForeColor: TdxNullableValue<TdxAlphaColor> read GetForeColor write SetForeColor;
    property Hidden: TdxNullableBoolean read GetHidden write SetHidden;
    property Italic: TdxNullableBoolean read GetItalic write SetItalic;
    property Strikeout: TdxNullableValue<TdxRichEditStrikeoutType> read GetStrikeout write SetStrikeout;
    property StrikeoutColor: TdxNullableValue<TdxAlphaColor> read GetStrikeoutColor write SetStrikeoutColor;
    property Subscript: TdxNullableBoolean read GetSubscript write SetSubscript;
    property Superscript: TdxNullableBoolean read GetSuperscript write SetSuperscript;
    property Underline: TdxNullableValue<TdxRichEditUnderlineType> read GetUnderline write SetUnderline;
    property UnderlineColor: TdxNullableValue<TdxAlphaColor> read GetUnderlineColor write SetUnderlineColor;
  end;

  { IdxRichEditCharacterStyle }

  IdxRichEditCharacterStyle = interface(IdxRichEditCharacterPropertiesBase)
    function GetIsDeleted: Boolean;
    function GetLinkedStyle: IdxRichEditParagraphStyle;
    function GetName: string;
    function GetParent: IdxRichEditCharacterStyle;
    function GetPrimary: Boolean;

    procedure SetLinkedStyle(const Value: IdxRichEditParagraphStyle);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: IdxRichEditCharacterStyle);
    procedure SetPrimary(const Value: Boolean);

    property IsDeleted: Boolean read GetIsDeleted;
    property LinkedStyle: IdxRichEditParagraphStyle read GetLinkedStyle write SetLinkedStyle;
    property Name: string read GetName write SetName;
    property Parent: IdxRichEditCharacterStyle read GetParent write SetParent;
    property Primary: Boolean read GetPrimary write SetPrimary;
  end;

  { IdxRichEditCharacterProperties }

  IdxRichEditCharacterProperties = interface(IdxRichEditCharacterPropertiesBase)
    function GetStyle: IdxRichEditCharacterStyle;
    procedure SetStyle(const Value: IdxRichEditCharacterStyle);

    property Style: IdxRichEditCharacterStyle read GetStyle write SetStyle;
  end;


  TdxRichEditTabAlignmentType = (
    Left,
    Center,
    Right,
    Decimal);

  TdxRichEditTabLeaderType = (
    None,
    Dots,
    MiddleDots,
    Hyphens,
    Underline,
    ThickLine,
    EqualSign);

  { IdxRichEditTabInfo }

  IdxRichEditTabInfo = interface
    function GetAlignment: TdxRichEditTabAlignmentType;
    function GetDeleted: Boolean;
    function GetLeader: TdxRichEditTabLeaderType;
    function GetPosition: Single;
    procedure SetAlignment(const Value: TdxRichEditTabAlignmentType);
    procedure SetDeleted(const Value: Boolean);
    procedure SetLeader(const Value: TdxRichEditTabLeaderType);
    procedure SetPosition(const Value: Single);

    property Alignment: TdxRichEditTabAlignmentType read GetAlignment write SetAlignment;
    property Deleted: Boolean read GetDeleted write SetDeleted;
    property Leader: TdxRichEditTabLeaderType read GetLeader write SetLeader;
    property Position: Single read GetPosition write SetPosition;
  end;

  IdxRichEditTabInfoCollection = interface(IdxRichEditList<IdxRichEditTabInfo>)
    function CreateNew: IdxRichEditTabInfo;
  end;


  { IdxRichEditParagraphPropertiesBase }

  TdxRichEditUsedParagraphFormattingOption = (
    UseAlignment,
    UseLeftIndent,
    UseRightIndent,
    UseSpacingBefore,
    UseSpacingAfter,
    UseLineSpacing,
    UseFirstLineIndent,
    UseSuppressHyphenation,
    UseSuppressLineNumbers,
    UseContextualSpacing,
    UsePageBreakBefore,
    UseBeforeAutoSpacing,
    UseAfterAutoSpacing,
    UseKeepWithNext,
    UseKeepLinesTogether,
    UseWidowOrphanControl,
    UseOutlineLevel,
    UseBackColor,
    UseLeftBorder,
    UseRightBorder,
    UseTopBorder,
    UseBottomBorder);

  TdxRichEditParagraphPropertiesMask = set of TdxRichEditUsedParagraphFormattingOption;

  TdxRichEditParagraphAlignment = (
    Left,
    Right,
    Center,
    Justify);

  TdxRichEditParagraphLineSpacing = (
    Single,
    Sesquialteral,
    Double,
    Multiple,
    Exactly,
    AtLeast);

  TdxRichEditParagraphFirstLineIndent = (
    None,
    Indented,
    Hanging);

  IdxRichEditParagraphPropertiesBase = interface
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

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditParagraphPropertiesMask); overload;

    property Alignment: TdxNullableValue<TdxRichEditParagraphAlignment> read GetAlignment write SetAlignment;
    property BackColor: TdxNullableValue<TdxAlphaColor> read GetBackColor write SetBackColor;
    property ContextualSpacing: TdxNullableBoolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: TdxNullableSingle read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxNullableValue<TdxRichEditParagraphFirstLineIndent> read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: TdxNullableBoolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property LeftIndent: TdxNullableSingle read GetLeftIndent write SetLeftIndent;
    property LineSpacing: TdxNullableSingle read GetLineSpacing write SetLineSpacing;
    property LineSpacingMultiplier: TdxNullableSingle read GetLineSpacingMultiplier write SetLineSpacingMultiplier;
    property LineSpacingRule: TdxNullableValue<TdxRichEditParagraphLineSpacing> read GetLineSpacingRule write SetLineSpacingRule;
    property OutlineLevel: TdxNullableInteger read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: TdxNullableBoolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: TdxNullableSingle read GetRightIndent write SetRightIndent;
    property SpacingAfter: TdxNullableSingle read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: TdxNullableSingle read GetSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: TdxNullableBoolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: TdxNullableBoolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
  end;

  { IdxRichEditParagraphPropertiesWithTabs }

  IdxRichEditParagraphPropertiesWithTabs = interface
    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);

    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
  end;

  { IdxRichEditParagraphProperties }

  IdxRichEditParagraphProperties = interface(IdxRichEditParagraphPropertiesBase)
    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);
  end;

  { IdxRichEditParagraphStyle }

  IdxRichEditParagraphStyle = interface(IdxRichEditParagraphPropertiesWithTabs)

    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;

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

    property IsDeleted: Boolean read GetIsDeleted;
    property LinkedStyle: IdxRichEditCharacterStyle read GetLinkedStyle write SetLinkedStyle;
    property ListLevelIndex: Integer read GetListLevelIndex write SetListLevelIndex;
    property Name: string read GetName write SetName;
    property NextStyle: IdxRichEditParagraphStyle read GetNextStyle write SetNextStyle;
    property NumberingListIndex: Integer read GetNumberingListIndex write SetNumberingListIndex;
    property Parent: IdxRichEditParagraphStyle read GetParent write SetParent;
    property Primary: Boolean read GetPrimary write SetPrimary;
  end;


  TdxRichEditTableBorderLineStyle = (
    &Nil = TdxBorderLineStyleValues.&Nil,
    None = TdxBorderLineStyleValues.None,
    Single = TdxBorderLineStyleValues.Single,
    Thick = TdxBorderLineStyleValues.Thick,
    Double = TdxBorderLineStyleValues.Double,
    Dotted = TdxBorderLineStyleValues.Dotted,
    Dashed = TdxBorderLineStyleValues.Dashed,
    DotDash = TdxBorderLineStyleValues.DotDash,
    DotDotDash = TdxBorderLineStyleValues.DotDotDash,
    Triple = TdxBorderLineStyleValues.Triple,
    ThinThickSmallGap = TdxBorderLineStyleValues.ThinThickSmallGap,
    ThickThinSmallGap = TdxBorderLineStyleValues.ThickThinSmallGap,
    ThinThickThinSmallGap = TdxBorderLineStyleValues.ThinThickThinSmallGap,
    ThinThickMediumGap = TdxBorderLineStyleValues.ThinThickMediumGap,
    ThickThinMediumGap = TdxBorderLineStyleValues.ThickThinMediumGap,
    ThinThickThinMediumGap = TdxBorderLineStyleValues.ThinThickThinMediumGap,
    ThinThickLargeGap = TdxBorderLineStyleValues.ThinThickLargeGap,
    ThickThinLargeGap = TdxBorderLineStyleValues.ThickThinLargeGap,
    ThinThickThinLargeGap = TdxBorderLineStyleValues.ThinThickThinLargeGap,
    Wave = TdxBorderLineStyleValues.Wave,
    DoubleWave = TdxBorderLineStyleValues.DoubleWave,
    DashSmallGap = TdxBorderLineStyleValues.DashSmallGap,
    DashDotStroked = TdxBorderLineStyleValues.DashDotStroked,
    ThreeDEmboss = TdxBorderLineStyleValues.ThreeDEmboss,
    ThreeDEngrave = TdxBorderLineStyleValues.ThreeDEngrave,
    Outset = TdxBorderLineStyleValues.Outset,
    Inset = TdxBorderLineStyleValues.Inset);

  TdxRichEditTableCellVerticalAlignment = (
    Top,
    Center,
    Bottom);

  TdxRichEditTableRowAlignment = (
    Both,
    Center,
    Distribute,
    Left,
    NumTab,
    Right);

  TdxRichEditTableLayoutType = (
    Fixed,
    Autofit);

  TdxRichEditConditionalTableStyleFormattingType = (
    BottomLeftCell,
    BottomRightCell,
    TopLeftCell,
    TopRightCell,
    EvenRowBanding,
    OddRowBanding,
    EvenColumnBanding,
    OddColumnBanding,
    LastColumn,
    FirstColumn,
    LastRow,
    FirstRow,
    WholeTable);
  TdxRichEditConditionalTableStyleFormattingTypes = set of TdxRichEditConditionalTableStyleFormattingType;

  { IdxRichEditTableCellBorder }

  IdxRichEditTableCellBorder = interface
    function GetLineColor: TdxAlphaColor;
    function GetLineStyle: TdxRichEditTableBorderLineStyle;
    function GetLineThickness: Single;
    procedure SetLineColor(const Value: TdxAlphaColor);
    procedure SetLineStyle(const Value: TdxRichEditTableBorderLineStyle);
    procedure SetLineThickness(const Value: Single);

    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property LineStyle: TdxRichEditTableBorderLineStyle read GetLineStyle write SetLineStyle;
    property LineThickness: Single read GetLineThickness write SetLineThickness;
  end;

  { IdxRichEditTableCellBorders }

  IdxRichEditTableCellBorders = interface
    function GetBottom: IdxRichEditTableCellBorder;
    function GetLeft: IdxRichEditTableCellBorder;
    function GetRight: IdxRichEditTableCellBorder;
    function GetTop: IdxRichEditTableCellBorder;

    property Bottom: IdxRichEditTableCellBorder read GetBottom;
    property Left: IdxRichEditTableCellBorder read GetLeft;
    property Right: IdxRichEditTableCellBorder read GetRight;
    property Top: IdxRichEditTableCellBorder read GetTop;
  end;

  { IdxRichEditTableBorder }

  IdxRichEditTableBorder = interface
    function GetLineColor: TdxAlphaColor;
    function GetLineStyle: TdxRichEditTableBorderLineStyle;
    function GetLineThickness: Single;
    procedure SetLineColor(const Value: TdxAlphaColor);
    procedure SetLineStyle(const Value: TdxRichEditTableBorderLineStyle);
    procedure SetLineThickness(const Value: Single);

    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property LineStyle: TdxRichEditTableBorderLineStyle read GetLineStyle write SetLineStyle;
    property LineThickness: Single read GetLineThickness write SetLineThickness;
  end;

  { IdxRichEditTableBorders }

  IdxRichEditTableBorders = interface
    function GetBottom: IdxRichEditTableBorder;
    function GetInsideHorizontalBorder: IdxRichEditTableBorder;
    function GetInsideVerticalBorder: IdxRichEditTableBorder;
    function GetLeft: IdxRichEditTableBorder;
    function GetRight: IdxRichEditTableBorder;
    function GetTop: IdxRichEditTableBorder;

    property Bottom: IdxRichEditTableBorder read GetBottom;
    property InsideHorizontalBorder: IdxRichEditTableBorder read GetInsideHorizontalBorder;
    property InsideVerticalBorder: IdxRichEditTableBorder read GetInsideVerticalBorder;
    property Left: IdxRichEditTableBorder read GetLeft;
    property Right: IdxRichEditTableBorder read GetRight;
    property Top: IdxRichEditTableBorder read GetTop;
  end;

  { IdxRichEditTableCellPropertiesBase }

  TdxRichEditUsedTableCellFormattingOption = (
    UsePreferredWidth,
    UseNoWrap,
    UseLeftPadding,
    UseRightPadding,
    UseTopPadding,
    UseBottomPadding,
    UseVerticalAlignment,
    UseLeftBorder,
    UseRightBorder,
    UseTopBorder,
    UseBottomBorder,
    UseInsideHorizontalBorder,
    UseInsideVerticalBorder,
    UseTopLeftDiagonalBorder,
    UseTopRightDiagonalBorder,
    UseBackgroundColor);
  TdxRichEditTableCellPropertiesMask = set of TdxRichEditUsedTableCellFormattingOption;

  IdxRichEditTableCellPropertiesBase = interface
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

  { IdxRichEditTablePropertiesBase }

  TdxRichEditUsedTableFormattingOption = (
    UseLeftPadding,
    UseRightPadding,
    UseTopPadding,
    UseBottomPadding,
    UseCellSpacing,
    UseTableIndent,
    UseTableLayout,
    UseTableLook,
    UsePreferredWidth,
    UseTableStyleColBandSize,
    UseTableStyleRowBandSize,
    UseLeftBorder,
    UseRightBorder,
    UseTopBorder,
    UseBottomBorder,
    UseInsideHorizontalBorder,
    UseInsideVerticalBorder,
    UseBackgroundColor,
    UseTableAlignment,
    UseBorders);
  TdxRichEditTablePropertiesMask = set of TdxRichEditUsedTableFormattingOption;

  IdxRichEditTablePropertiesBase = interface
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

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditTablePropertiesMask); overload;

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

  { IdxRichEditTableCellStyle }

  IdxRichEditTableCellStyle = interface
    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetIsDeleted: Boolean;
    function GetName: string;
    function GetParent: IdxRichEditTableCellStyle;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
    procedure SetName(const Value: string);
    procedure SetParent(const Value: IdxRichEditTableCellStyle);

    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property TableCellProperties: IdxRichEditTableCellPropertiesBase read GetTableCellProperties;

    property IsDeleted: Boolean read GetIsDeleted;
    property Name: string read GetName write SetName;
    property Parent: IdxRichEditTableCellStyle read GetParent write SetParent;
  end;

  { IdxRichEditTableConditionalStyle }

  IdxRichEditTableConditionalStyle = interface
    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetTableCellProperties: IdxRichEditTableCellPropertiesBase;
    function GetTableProperties: IdxRichEditTablePropertiesBase;

    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property TableCellProperties: IdxRichEditTableCellPropertiesBase read GetTableCellProperties;
    property TableProperties: IdxRichEditTablePropertiesBase read GetTableProperties;
  end;

  { IdxRichEditTableConditionalStyleProperties }

  IdxRichEditTableConditionalStyleProperties = interface
    function GetItem(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
    function GetOwner: IdxRichEditTableStyle;

    function CreateConditionalStyle(const ACondition: TdxRichEditConditionalTableStyleFormattingType): IdxRichEditTableConditionalStyle;
    property Items[const ACondition: TdxRichEditConditionalTableStyleFormattingType]: IdxRichEditTableConditionalStyle read GetItem;
    property Owner: IdxRichEditTableStyle read GetOwner;
  end;

  { IdxRichEditTableStyle }

  IdxRichEditTableStyle = interface
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

    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ConditionalStyleProperties: IdxRichEditTableConditionalStyleProperties read GetConditionalStyleProperties;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property TableCellProperties: IdxRichEditTableCellPropertiesBase read GetTableCellProperties;
    property TableProperties: IdxRichEditTablePropertiesBase read GetTableProperties;

    property IsDeleted: Boolean read GetIsDeleted;
    property Name: string read GetName write SetName;
    property Parent: IdxRichEditTableStyle read GetParent write SetParent;
  end;


  { IdxRichEditStyleCollection<T> }

  IdxRichEditStyleCollection<T: IInterface> = interface(IdxReadOnlyList<T>)
    function GetItem(const AName: string): T;

    procedure Add(const AStyle: T);
    function CreateNew: T;
    procedure Delete(const AStyle: T);

    property Self[const AName: string]: T read GetItem; default;
  end;

  IdxRichEditCharacterStyleCollection = interface(IdxRichEditStyleCollection<IdxRichEditCharacterStyle>)
  end;
  IdxRichEditParagraphStyleCollection = interface(IdxRichEditStyleCollection<IdxRichEditParagraphStyle>)
  end;
  IdxRichEditTableStyleCollection = interface(IdxRichEditStyleCollection<IdxRichEditTableStyle>)
  end;


  TdxRichEditMergeMode = (
    NewParagraph,
    NewSection,
    JoinTables);

  TdxRichEditMergeRecords = (
    All,
    Selected);

  { IdxRichEditMailMergeOptions }

  IdxRichEditMailMergeOptions = interface
    function GetCopyTemplateStyles: Boolean;
    function GetDataSource: TDataSource;
    function GetHeaderFooterLinkToPrevious: Boolean;
    function GetMergeMode: TdxRichEditMergeMode;
    function GetMergeRecords: TdxRichEditMergeRecords;
    procedure SetCopyTemplateStyles(const Value: Boolean);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetHeaderFooterLinkToPrevious(const Value: Boolean);
    procedure SetMergeMode(const Value: TdxRichEditMergeMode);
    procedure SetMergeRecords(const Value: TdxRichEditMergeRecords);

    property CopyTemplateStyles: Boolean read GetCopyTemplateStyles write SetCopyTemplateStyles;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property HeaderFooterLinkToPrevious: Boolean read GetHeaderFooterLinkToPrevious write SetHeaderFooterLinkToPrevious;
    property MergeMode: TdxRichEditMergeMode read GetMergeMode write SetMergeMode;
    property MergeRecords: TdxRichEditMergeRecords read GetMergeRecords write SetMergeRecords;
  end;

  { IdxRichEditField }

  IdxRichEditField = interface
    function GetCodeRange: IdxRichEditDocumentRange;
    function GetLocked: Boolean;
    function GetParent: IdxRichEditField;
    function GetRange: IdxRichEditDocumentRange;
    function GetResultRange: IdxRichEditDocumentRange;
    function GetShowCodes: Boolean;
    procedure SetLocked(const Value: Boolean);
    procedure SetShowCodes(const Value: Boolean);

    procedure Update;

    property CodeRange: IdxRichEditDocumentRange read GetCodeRange;
    property Locked: Boolean read GetLocked write SetLocked;
    property Parent: IdxRichEditField read GetParent;
    property Range: IdxRichEditDocumentRange read GetRange;
    property ResultRange: IdxRichEditDocumentRange read GetResultRange;
    property ShowCodes: Boolean read GetShowCodes write SetShowCodes;
  end;

  { IdxRichEditReadOnlyFieldCollection }

  IdxRichEditReadOnlyFieldCollection = interface(IdxReadOnlyList<IdxRichEditField>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyFieldCollection;
  end;

  { IdxRichEditFieldCollection }

  IdxRichEditFieldCollection = interface(IdxRichEditReadOnlyFieldCollection)
    function CreateField(const ARange: IdxRichEditDocumentRange): IdxRichEditField; overload;
    function CreateField(const AStart: IdxRichEditDocumentPosition; const ACode: string): IdxRichEditField; overload;
    procedure Update;
  end;

  { IdxRichEditDocumentVariableCollection }

  IdxRichEditDocumentVariableCollection = interface
    function GetCount: Integer;
    function GetItem(const AName: string): TValue;
    procedure SetItem(const AName: string; const Value: TValue);

    procedure Add(const AName: string; const AValue: TValue);
    procedure Clear;
    function GetVariableNames: TArray<string>;
    function GetVariableValue(const AVariableName: string): TValue;
    procedure Remove(const AName: string);

    property Count: Integer read GetCount;
    property Items[const AName: string]: TValue read GetItem write SetItem; default;
  end;


  { TdxRichEditParagraph }

  IdxRichEditParagraph = interface
    function GetAlignment: TdxRichEditParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Single;
    function GetFirstLineIndentType: TdxRichEditParagraphFirstLineIndent;
    function GetIndex: Integer;
    function GetIsInList: Boolean;
    function GetKeepLinesTogether: Boolean;
    function GetLeftIndent: Single;
    function GetLineSpacing: Single;
    function GetLineSpacingMultiplier: Single;
    function GetLineSpacingRule: TdxRichEditParagraphLineSpacing;
    function GetListIndex: Integer;
    function GetListLevel: Integer;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetParagraphIndex: Integer;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightIndent: Single;
    function GetSpacingAfter: Single;
    function GetSpacingBefore: Single;
    function GetStyle: IdxRichEditParagraphStyle;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;

    procedure SetAlignment(const Value: TdxRichEditParagraphAlignment);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Single);
    procedure SetFirstLineIndentType(const Value: TdxRichEditParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetLeftIndent(const Value: Single);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingMultiplier(const Value: Single);
    procedure SetLineSpacingRule(const Value: TdxRichEditParagraphLineSpacing);
    procedure SetListIndex(const Value: Integer);
    procedure SetListLevel(const Value: Integer);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetRightIndent(const Value: Single);
    procedure SetSpacingAfter(const Value: Single);
    procedure SetSpacingBefore(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditParagraphStyle);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);

    property ParagraphIndex: Integer read GetParagraphIndex;

    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);

    procedure Reset;

    property Alignment: TdxRichEditParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Single read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxRichEditParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property Index: Integer read GetIndex;
    property IsInList: Boolean read GetIsInList;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property LeftIndent: Single read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingMultiplier: Single read GetLineSpacingMultiplier write SetLineSpacingMultiplier;
    property LineSpacingRule: TdxRichEditParagraphLineSpacing read GetLineSpacingRule write SetLineSpacingRule;
    property ListIndex: Integer read GetListIndex write SetListIndex;
    property ListLevel: Integer read GetListLevel write SetListLevel;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightIndent: Single read GetRightIndent write SetRightIndent;
    property SpacingAfter: Single read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Single read GetSpacingBefore write SetSpacingBefore;
    property Style: IdxRichEditParagraphStyle read GetStyle write SetStyle;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
  end;

  { IdxRichEditReadOnlyParagraphCollection }

  IdxRichEditReadOnlyParagraphCollection = interface(IdxReadOnlyList<IdxRichEditParagraph>)
    function Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection; overload;
  end;

  { IdxRichEditParagraphCollection }

  IdxRichEditParagraphCollection = interface(IdxRichEditReadOnlyParagraphCollection)
    procedure AddParagraphsToList(const ARange: IdxRichEditDocumentRange; const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      const AList: IdxRichEditNumberingList; ALevelIndex: Integer); overload;
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      ANumberingListIndex, ALevelIndex: Integer); overload;
    function Append: IdxRichEditParagraph;
    function Insert(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function Insert(const APos: IdxRichEditDocumentPosition; const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph; overload;
    procedure RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
    procedure RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);
  end;


  TdxRichEditPaperKind = (
    Custom = 0,
    Letter = 1,
    LetterSmall = 2,
    Tabloid = 3,
    Ledger = 4,
    Legal = 5,
    Statement = 6,
    Executive = 7,
    A3 = 8,
    A4 = 9,
    A4Small = 10,
    A5 = 11,
    B4 = 12,
    B5 = 13,
    Folio = 14,
    Quarto = 15,
    Standard10x14 = 16,
    Standard11x17 = 17,
    Note = 18,
    Number9Envelope = 19,
    Number10Envelope = 20,
    Number11Envelope = 21,
    Number12Envelope = 22,
    Number14Envelope = 23,
    CSheet = 24,
    DSheet = 25,
    ESheet = 26,
    DLEnvelope = 27,
    C5Envelope = 28,
    C3Envelope = 29,
    C4Envelope = 30,
    C6Envelope = 31,
    C65Envelope = 32,
    B4Envelope = 33,
    B5Envelope = 34,
    B6Envelope = 35,
    ItalyEnvelope = 36,
    MonarchEnvelope = 37,
    PersonalEnvelope = 38,
    USStandardFanfold = 39,
    GermanStandardFanfold = 40,
    GermanLegalFanfold = 41,
    IsoB4 = 42,
    JapanesePostcard = 43,
    Standard9x11 = 44,
    Standard10x11 = 45,
    Standard15x11 = 46,
    InviteEnvelope = 47,
    //Reserved -- do not use
    Reserved_48 = 48,
    //Reserved -- do not use
    Reserved_49 = 49,
    LetterExtra = 50,
    LegalExtra = 51,
    TabloidExtra = 52,
    A4Extra = 53,
    LetterTransverse = 54,
    A4Transverse = 55,
    LetterExtraTransverse = 56,
    APlus = 57,
    BPlus = 58,
    LetterPlus = 59,
    A4Plus = 60,
    A5Transverse = 61,
    B5Transverse = 62,
    A3Extra = 63,
    A5Extra = 64,
    B5Extra = 65,
    A2 = 66,
    A3Transverse = 67,
    A3ExtraTransverse = 68,
    JapaneseDoublePostcard = 69,
    A6 = 70,
    JapaneseEnvelopeKakuNumber2 = 71,
    JapaneseEnvelopeKakuNumber3 = 72,
    JapaneseEnvelopeChouNumber3 = 73,
    JapaneseEnvelopeChouNumber4 = 74,
    LetterRotated = 75,
    A3Rotated = 76,
    A4Rotated = 77,
    A5Rotated = 78,
    B4JisRotated = 79,
    B5JisRotated = 80,
    JapanesePostcardRotated = 81,
    JapaneseDoublePostcardRotated = 82,
    A6Rotated = 83,
    JapaneseEnvelopeKakuNumber2Rotated = 84,
    JapaneseEnvelopeKakuNumber3Rotated = 85,
    JapaneseEnvelopeChouNumber3Rotated = 86,
    JapaneseEnvelopeChouNumber4Rotated = 87,
    B6Jis = 88,
    B6JisRotated = 89,
    Standard12x11 = 90,
    JapaneseEnvelopeYouNumber4 = 91,
    JapaneseEnvelopeYouNumber4Rotated = 92,
    Prc16K = 93,
    Prc32K = 94,
    Prc32KBig = 95,
    PrcEnvelopeNumber1 = 96,
    PrcEnvelopeNumber2 = 97,
    PrcEnvelopeNumber3 = 98,
    PrcEnvelopeNumber4 = 99,
    PrcEnvelopeNumber5 = 100,
    PrcEnvelopeNumber6 = 101,
    PrcEnvelopeNumber7 = 102,
    PrcEnvelopeNumber8 = 103,
    PrcEnvelopeNumber9 = 104,
    PrcEnvelopeNumber10 = 105,
    Prc16KRotated = 106,
    Prc32KRotated = 107,
    Prc32KBigRotated = 108,
    PrcEnvelopeNumber1Rotated = 109,
    PrcEnvelopeNumber2Rotated = 110,
    PrcEnvelopeNumber3Rotated = 111,
    PrcEnvelopeNumber4Rotated = 112,
    PrcEnvelopeNumber5Rotated = 113,
    PrcEnvelopeNumber6Rotated = 114,
    PrcEnvelopeNumber7Rotated = 115,
    PrcEnvelopeNumber8Rotated = 116,
    PrcEnvelopeNumber9Rotated = 117,
    PrcEnvelopeNumber10Rotated = 118);

  { IdxRichEditSectionPage }

  IdxRichEditSectionPage = interface
    function GetHeight: Single;
    function GetLandscape: Boolean;
    function GetPaperKind: TdxRichEditPaperKind;
    function GetWidth: Single;
    procedure SetHeight(const AValue: Single);
    procedure SetLandscape(const AValue: Boolean);
    procedure SetPaperKind(const AValue: TdxRichEditPaperKind);
    procedure SetWidth(const AValue: Single);

    property Height: Single read GetHeight write SetHeight;
    property Landscape: Boolean read GetLandscape write SetLandscape;
    property PaperKind: TdxRichEditPaperKind read GetPaperKind write SetPaperKind;
    property Width: Single read GetWidth write SetWidth;
  end;

  { IdxRichEditSectionColumn }

  IdxRichEditSectionColumn = interface
    function GetSpacing: Single;
    function GetWidth: Single;
    procedure SetSpacing(const Value: Single);
    procedure SetWidth(const Value: Single);

    property Spacing: Single read GetSpacing write SetSpacing;
    property Width: Single read GetWidth write SetWidth;
  end;

  { IdxRichEditSectionColumnCollection }

  IdxRichEditSectionColumnCollection = interface(IdxRichEditList<IdxRichEditSectionColumn>)
  end;

  { IdxRichEditSectionColumns }

  IdxRichEditSectionColumns = interface
    function GetCount: Integer;

    function GetColumns: IdxRichEditSectionColumnCollection;
    procedure SetColumns(const Value: IdxRichEditSectionColumnCollection);

    function CreateUniformColumns(AColumnWidth, AColumnSpacing: Single; AColumnCount: Integer): IdxRichEditSectionColumnCollection; overload;
    function CreateUniformColumns(const APage: IdxRichEditSectionPage; AColumnSpacing: Single; AColumnCount: Integer): IdxRichEditSectionColumnCollection; overload;

    property Count: Integer read GetCount;
  end;

  { IdxRichEditSectionLineNumbering }

  TdxRichEditLineNumberingRestart = (
    NewPage,
    NewSection,
    Continuous);

  IdxRichEditSectionLineNumbering = interface
    function GetCountBy: Integer;
    function GetDistance: Single;
    function GetRestartType: TdxRichEditLineNumberingRestart;
    function GetStart: Integer;
    procedure SetCountBy(const Value: Integer);
    procedure SetDistance(const Value: Single);
    procedure SetRestartType(const Value: TdxRichEditLineNumberingRestart);
    procedure SetStart(const Value: Integer);

    property CountBy: Integer read GetCountBy write SetCountBy;
    property Distance: Single read GetDistance write SetDistance;
    property RestartType: TdxRichEditLineNumberingRestart read GetRestartType write SetRestartType;
    property Start: Integer read GetStart write SetStart;
  end;

  { IdxRichEditSectionPageNumbering }

  TdxRichEditNumberingFormat = (
    Decimal,
    AIUEOHiragana,
    AIUEOFullWidthHiragana,
    ArabicAbjad,
    ArabicAlpha,
    Bullet,
    CardinalText,
    Chicago,
    ChineseCounting,
    ChineseCountingThousand,
    ChineseLegalSimplified,
    Chosung,
    DecimalEnclosedCircle,
    DecimalEnclosedCircleChinese,
    DecimalEnclosedFullstop,
    DecimalEnclosedParentheses,
    DecimalFullWidth,
    DecimalFullWidth2,
    DecimalHalfWidth,
    DecimalZero,
    Ganada,
    Hebrew1,
    Hebrew2,
    Hex,
    HindiConsonants,
    HindiDescriptive,
    HindiNumbers,
    HindiVowels,
    IdeographDigital,
    IdeographEnclosedCircle,
    IdeographLegalTraditional,
    IdeographTraditional,
    IdeographZodiac,
    IdeographZodiacTraditional,
    Iroha,
    IrohaFullWidth,
    JapaneseCounting,
    JapaneseDigitalTenThousand,
    JapaneseLegal,
    KoreanCounting,
    KoreanDigital,
    KoreanDigital2,
    KoreanLegal,
    LowerLetter,
    LowerRoman,
    None,
    NumberInDash,
    Ordinal,
    OrdinalText,
    RussianLower,
    RussianUpper,
    TaiwaneseCounting,
    TaiwaneseCountingThousand,
    TaiwaneseDigital,
    ThaiDescriptive,
    ThaiLetters,
    ThaiNumbers,
    UpperLetter,
    UpperRoman,
    VietnameseDescriptive);

  IdxRichEditSectionPageNumbering = interface
    function GetContinueNumbering: Boolean;
    function GetFirstPageNumber: Integer;
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    procedure SetContinueNumbering(const Value: Boolean);
    procedure SetFirstPageNumber(const Value: Integer);
    procedure SetNumberingFormat(const Value: TdxRichEditNumberingFormat);

    property ContinueNumbering: Boolean read GetContinueNumbering write SetContinueNumbering;
    property FirstPageNumber: Integer read GetFirstPageNumber write SetFirstPageNumber;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
  end;

  { IdxRichEditSectionMargins }

  IdxRichEditSectionMargins = interface
    function GetBottom: Single;
    function GetFooterOffset: Single;
    function GetHeaderOffset: Single;
    function GetLeft: Single;
    function GetRight: Single;
    function GetTop: Single;
    procedure SetBottom(const Value: Single);
    procedure SetFooterOffset(const Value: Single);
    procedure SetHeaderOffset(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);

    property Bottom: Single read GetBottom write SetBottom;
    property FooterOffset: Single read GetFooterOffset write SetFooterOffset;
    property HeaderOffset: Single read GetHeaderOffset write SetHeaderOffset;
    property Left: Single read GetLeft write SetLeft;
    property Right: Single read GetRight write SetRight;
    property Top: Single read GetTop write SetTop;
  end;

  { IdxRichEditSection }

  TdxRichEditSectionStartType = (
    NextPage,
    OddPage,
    EvenPage,
    Continuous,
    Column);

  TdxRichEditHeaderFooterType = (
    First,
    Odd,
    Even);

  IdxRichEditSection = interface
    function GetColumns: IdxRichEditSectionColumns;
    function GetDifferentFirstPage: Boolean;
    function GetFirstPageTray: Integer;
    function GetLineNumbering: IdxRichEditSectionLineNumbering;
    function GetMargins: IdxRichEditSectionMargins;
    function GetOtherPagesTray: Integer;
    function GetPage: IdxRichEditSectionPage;
    function GetPageNumbering: IdxRichEditSectionPageNumbering;
    function GetParagraphs: IdxRichEditReadOnlyParagraphCollection;
    function GetStartType: TdxRichEditSectionStartType;
    procedure SetDifferentFirstPage(const Value: Boolean);
    procedure SetFirstPageTray(const Value: Integer);
    procedure SetOtherPagesTray(const Value: Integer);
    procedure SetStartType(const Value: TdxRichEditSectionStartType);

    function BeginUpdateHeader: IdxRichEditSubDocument; overload;
    function BeginUpdateHeader(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument; overload;
    procedure EndUpdateHeader(const ADocument: IdxRichEditSubDocument);
    function HasHeader(AType: TdxRichEditHeaderFooterType): Boolean;

    function BeginUpdateFooter: IdxRichEditSubDocument; overload;
    function BeginUpdateFooter(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument; overload;
    procedure EndUpdateFooter(const ADocument: IdxRichEditSubDocument);
    function HasFooter(AType: TdxRichEditHeaderFooterType): Boolean;

    function IsHeaderLinkedToPrevious: Boolean; overload;
    procedure LinkHeaderToPrevious; overload;
    procedure UnlinkHeaderFromPrevious; overload;

    function IsFooterLinkedToPrevious: Boolean; overload;
    procedure LinkFooterToPrevious; overload;
    procedure UnlinkFooterFromPrevious; overload;

    function IsHeaderLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkHeaderToPrevious(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkHeaderFromPrevious(AType: TdxRichEditHeaderFooterType); overload;

    function IsFooterLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkFooterToPrevious(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkFooterFromPrevious(AType: TdxRichEditHeaderFooterType); overload;

    function IsHeaderLinkedToNext: Boolean; overload;
    procedure LinkHeaderToNext; overload;
    procedure UnlinkHeaderFromNext; overload;

    function IsFooterLinkedToNext: Boolean; overload;
    procedure LinkFooterToNext; overload;
    procedure UnlinkFooterFromNext; overload;

    function IsHeaderLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkHeaderToNext(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkHeaderFromNext(AType: TdxRichEditHeaderFooterType); overload;

    function IsFooterLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkFooterToNext(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkFooterFromNext(AType: TdxRichEditHeaderFooterType); overload;

    property Columns: IdxRichEditSectionColumns read GetColumns;
    property DifferentFirstPage: Boolean read GetDifferentFirstPage write SetDifferentFirstPage;
    property FirstPageTray: Integer read GetFirstPageTray write SetFirstPageTray;
    property LineNumbering: IdxRichEditSectionLineNumbering read GetLineNumbering;
    property Margins: IdxRichEditSectionMargins read GetMargins;
    property OtherPagesTray: Integer read GetOtherPagesTray write SetOtherPagesTray;
    property Page: IdxRichEditSectionPage read GetPage;
    property PageNumbering: IdxRichEditSectionPageNumbering read GetPageNumbering;
    property Paragraphs: IdxRichEditReadOnlyParagraphCollection read GetParagraphs;
    property StartType: TdxRichEditSectionStartType read GetStartType write SetStartType;
  end;

  IdxRichEditSectionCollection = interface(IdxReadOnlyList<IdxRichEditSection>)
  end;


  { IdxRichEditSelectionCollection }

  IdxRichEditSelectionCollection = interface(IdxReadOnlyList<IdxRichEditDocumentRange>)
    function Add(const ARange: IdxRichEditDocumentRange): Integer; overload;
    procedure Add(const ARanges: TList<IdxRichEditDocumentRange>); overload;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Unselect(const ARange: IdxRichEditDocumentRange);
  end;


  { IdxRichEditDocumentImage }

  IdxRichEditDocumentImage = interface
    function GetImage: TdxOfficeImage;
    function GetLockAspectRatio: Boolean;
    function GetOriginalSize: TdxSizeF;
    function GetRange: IdxRichEditDocumentRange;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetSize: TdxSizeF;
    function GetUri: string;
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetSize(const Value: TdxSizeF);
    procedure SetUri(const Value: string);

    property Image: TdxOfficeImage read GetImage;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property OriginalSize: TdxSizeF read GetOriginalSize;
    property Range: IdxRichEditDocumentRange read GetRange;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Size: TdxSizeF read GetSize write SetSize;
    property Uri: string read GetUri write SetUri;
  end;

  { IdxRichEditReadOnlyDocumentImageCollection }

  IdxRichEditReadOnlyDocumentImageCollection = interface(IdxReadOnlyList<IdxRichEditDocumentImage>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection;
  end;

  { IdxRichEditDocumentImageCollection }

  IdxRichEditDocumentImageCollection = interface(IdxRichEditReadOnlyDocumentImageCollection)
    function Append(AImage: TGraphic): IdxRichEditDocumentImage;
    function Insert(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditDocumentImage;
  end;


  { IdxRichEditHyperlink }

  IdxRichEditHyperlink = interface
    function GetAnchor: string;
    function GetNavigateUri: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetTarget: string;
    function GetToolTip: string;
    function GetVisited: Boolean;
    procedure SetAnchor(const Value: string);
    procedure SetNavigateUri(const Value: string);
    procedure SetTarget(const Value: string);
    procedure SetToolTip(const Value: string);
    procedure SetVisited(const Value: Boolean);

    property Anchor: string read GetAnchor write SetAnchor;
    property NavigateUri: string read GetNavigateUri write SetNavigateUri;
    property Range: IdxRichEditDocumentRange read GetRange;
    property Target: string read GetTarget write SetTarget;
    property ToolTip: string read GetToolTip write SetToolTip;
    property Visited: Boolean read GetVisited write SetVisited;
  end;

  { IdxRichEditReadOnlyHyperlinkCollection }

  IdxRichEditReadOnlyHyperlinkCollection = interface(IdxReadOnlyList<IdxRichEditHyperlink>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyHyperlinkCollection;
  end;

  { IdxRichEditHyperlinkCollection }

  IdxRichEditHyperlinkCollection = interface(IdxRichEditReadOnlyHyperlinkCollection)
    function CreateHyperlink(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditHyperlink; overload;
    function CreateHyperlink(const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink; overload;
    function FindHyperlink(AFieldIndex: Integer; out AHyperlink: IdxRichEditHyperlink): Boolean;
    procedure RemoveHyperlink(const AHyperlink: IdxRichEditHyperlink);
  end;

  { IdxRichEditBookmark }

  IdxRichEditBookmark = interface
    function GetName: string;
    function GetRange: IdxRichEditDocumentRange;

    property Name: string read GetName;
    property Range: IdxRichEditDocumentRange read GetRange;
  end;

  { IdxRichEditReadOnlyBookmarkCollection }

  IdxRichEditReadOnlyBookmarkCollection = interface(IdxReadOnlyList<IdxRichEditBookmark>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyBookmarkCollection;
  end;

  { IdxRichEditBookmarkCollection }

  IdxRichEditBookmarkCollection = interface(IdxRichEditReadOnlyBookmarkCollection)
    function GetItem(const Name: string): IdxRichEditBookmark;

    function CreateBookmark(const ARange: IdxRichEditDocumentRange; const AName: string): IdxRichEditBookmark; overload;
    function CreateBookmark(const AStart: IdxRichEditDocumentPosition; ALength: Integer; const AName: string): IdxRichEditBookmark; overload;
    procedure SelectBookmark(const ABookmark: IdxRichEditBookmark);
    procedure RemoveBookmark(const ABookmark: IdxRichEditBookmark);

    property Items[const Name: string]: IdxRichEditBookmark read GetItem; default;
  end;


  TdxRichEditShapeHorizontalAlignment = (
    None,
    Left,
    Center,
    Right,
    Inside,
    Outside);

  TdxRichEditShapeVerticalAlignment = (
    None,
    Top,
    Center,
    Bottom,
    Inside,
    Outside);

  TdxRichEditShapeRelativeHorizontalPosition = (
    Page,
    Character,
    Column,
    Margin,
    LeftMargin,
    RightMargin,
    InsideMargin,
    OutsideMargin);

  TdxRichEditShapeRelativeVerticalPosition = (
    Page,
    Line,
    Paragraph,
    Margin,
    TopMargin,
    BottomMargin,
    InsideMargin,
    OutsideMargin);

  TdxRichEditTextWrappingType = (
    Square,
    Tight,
    Through,
    TopAndBottom,
    BehindText,
    InFrontOfText);

  TdxRichEditTextBoxSizeRule = (
    Auto,
    Exact);

  { IdxRichEditShapeFill }

  IdxRichEditShapeFill = interface
    function GetColor: TdxAlphaColor;
    procedure SetColor(const Value: TdxAlphaColor);

    property Color: TdxAlphaColor read GetColor write SetColor;
  end;

  { IdxRichEditShapeLine }

  IdxRichEditShapeLine = interface
    function GetColor: TdxAlphaColor;
    function GetThickness: Single;
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetThickness(const Value: Single);

    property Color: TdxAlphaColor read GetColor write SetColor;
    property Thickness: Single read GetThickness write SetThickness;
  end;

  { IdxRichEditTextBox }

  IdxRichEditTextBox = interface
    function GetDocument: IdxRichEditSubDocument;
    function GetHeightRule: TdxRichEditTextBoxSizeRule;
    function GetMarginBottom: Single;
    function GetMarginLeft: Single;
    function GetMarginRight: Single;
    function GetMarginTop: Single;
    procedure SetHeightRule(const Value: TdxRichEditTextBoxSizeRule);
    procedure SetMarginBottom(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginTop(const Value: Single);

    property Document: IdxRichEditSubDocument read GetDocument;
    property HeightRule: TdxRichEditTextBoxSizeRule read GetHeightRule write SetHeightRule;
    property MarginBottom: Single read GetMarginBottom write SetMarginBottom;
    property MarginLeft: Single read GetMarginLeft write SetMarginLeft;
    property MarginRight: Single read GetMarginRight write SetMarginRight;
    property MarginTop: Single read GetMarginTop write SetMarginTop;
  end;

  { IdxRichEditShape }

  IdxRichEditShape = interface
    function GetFill: IdxRichEditShapeFill;
    function GetHorizontalAlignment: TdxRichEditShapeHorizontalAlignment;
    function GetLine: IdxRichEditShapeLine;
    function GetLockAspectRatio: Boolean;
    function GetMarginBottom: Single;
    function GetMarginLeft: Single;
    function GetMarginRight: Single;
    function GetMarginTop: Single;
    function GetName: string;
    function GetOffset: TdxPointF;
    function GetOriginalSize: TdxSizeF;
    function GetPicture: TdxOfficeImage;
    function GetPictureUri: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetRelativeHorizontalPosition: TdxRichEditShapeRelativeHorizontalPosition;
    function GetRelativeVerticalPosition: TdxRichEditShapeRelativeVerticalPosition;
    function GetRotationAngle: Single;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetSize: TdxSizeF;
    function GetTextBox: IdxRichEditTextBox;
    function GetTextWrapping: TdxRichEditTextWrappingType;
    function GetVerticalAlignment: TdxRichEditShapeVerticalAlignment;
    function GetZOrder: Integer;
    procedure SetHorizontalAlignment(const Value: TdxRichEditShapeHorizontalAlignment);
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetMarginBottom(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginTop(const Value: Single);
    procedure SetName(const Value: string);
    procedure SetOffset(const Value: TdxPointF);
    procedure SetPictureUri(const Value: string);
    procedure SetRelativeHorizontalPosition(const Value: TdxRichEditShapeRelativeHorizontalPosition);
    procedure SetRelativeVerticalPosition(const Value: TdxRichEditShapeRelativeVerticalPosition);
    procedure SetRotationAngle(const Value: Single);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetSize(const Value: TdxSizeF);
    procedure SetTextWrapping(const Value: TdxRichEditTextWrappingType);
    procedure SetVerticalAlignment(const Value: TdxRichEditShapeVerticalAlignment);
    procedure SetZOrder(const Value: Integer);

    property Fill: IdxRichEditShapeFill read GetFill;
    property HorizontalAlignment: TdxRichEditShapeHorizontalAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
    property Line: IdxRichEditShapeLine read GetLine;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property MarginBottom: Single read GetMarginBottom write SetMarginBottom;
    property MarginLeft: Single read GetMarginLeft write SetMarginLeft;
    property MarginRight: Single read GetMarginRight write SetMarginRight;
    property MarginTop: Single read GetMarginTop write SetMarginTop;
    property Name: string read GetName write SetName;
    property Offset: TdxPointF read GetOffset write SetOffset;
    property OriginalSize: TdxSizeF read GetOriginalSize;
    property Picture: TdxOfficeImage read GetPicture;
    property PictureUri: string read GetPictureUri write SetPictureUri;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RelativeHorizontalPosition: TdxRichEditShapeRelativeHorizontalPosition read GetRelativeHorizontalPosition write SetRelativeHorizontalPosition;
    property RelativeVerticalPosition: TdxRichEditShapeRelativeVerticalPosition read GetRelativeVerticalPosition write SetRelativeVerticalPosition;
    property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Size: TdxSizeF read GetSize write SetSize;
    property TextBox: IdxRichEditTextBox read GetTextBox;
    property TextWrapping: TdxRichEditTextWrappingType read GetTextWrapping write SetTextWrapping;
    property VerticalAlignment: TdxRichEditShapeVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property ZOrder: Integer read GetZOrder write SetZOrder;
  end;

  { IdxRichEditReadOnlyShapeCollection }

  IdxRichEditReadOnlyShapeCollection = interface(IdxReadOnlyList<IdxRichEditShape>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection;
  end;

  { IdxRichEditShapeCollection }

  IdxRichEditShapeCollection = interface(IdxRichEditReadOnlyShapeCollection)
    function GetItem(const Name: string): IdxRichEditShape;

    function InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape;
    function InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;

    property Items[const Name: string]: IdxRichEditShape read GetItem; default;
  end;


  { IdxRichEditRangePermission }

  IdxRichEditRangePermission = interface
    function GetGroup: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetUserName: string;
    procedure SetGroup(const Value: string);
    procedure SetUserName(const Value: string);

    property Group: string read GetGroup write SetGroup;
    property Range: IdxRichEditDocumentRange read GetRange;
    property UserName: string read GetUserName write SetUserName;
  end;

  { IdxRichEditRangePermissionCollection }

  IdxRichEditRangePermissionCollection = interface(IdxRichEditList<IdxRichEditRangePermission>)
    function CreateRangePermission(const ARange: IdxRichEditDocumentRange): IdxRichEditRangePermission;
  end;


  TdxRichEditWidthType = (
    None = TdxWidthUnitTypeValues.&Nil,
    Auto = TdxWidthUnitTypeValues.Auto,
    FiftiethsOfPercent = TdxWidthUnitTypeValues.FiftiethsOfPercent,
    Fixed = TdxWidthUnitTypeValues.ModelUnits);

  TdxRichEditHeightType = (
    AtLeast,
    Auto,
    Exact);

  TdxRichEditTableLookType = (
    ApplyFirstRow,
    ApplyLastRow,
    ApplyFirstColumn,
    ApplyLastColumn,
    DoNotApplyRowBanding,
    DoNotApplyColumnBanding);
  TdxRichEditTableLookTypes = set of TdxRichEditTableLookType;

  TdxRichEditAutoFitBehaviorType = (
    FixedColumnWidth = TdxTableAutoFitBehaviorTypeValues.FixedColumnWidth,
    AutoFitToContents = TdxTableAutoFitBehaviorTypeValues.AutoFitToContents,
    AutoFitToWindow = TdxTableAutoFitBehaviorTypeValues.AutoFitToWindow);

  { IdxRichEditTableCell }

  IdxRichEditTableCell = interface
    function GetBackgroundColor: TdxAlphaColor;
    function GetBorders: IdxRichEditTableCellBorders;
    function GetBottomPadding: Single;
    function GetColumnSpan: Integer;
    function GetContentRange: IdxRichEditDocumentRange;
    function GetHeight: Single;
    function GetHeightType: TdxRichEditHeightType;
    function GetIndex: Integer;
    function GetLeftPadding: Single;
    function GetNestingLevel: Integer;
    function GetNext: IdxRichEditTableCell;
    function GetPreferredWidth: Single;
    function GetPreferredWidthType: TdxRichEditWidthType;
    function GetPrevious: IdxRichEditTableCell;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightPadding: Single;
    function GetRow: IdxRichEditTableRow;
    function GetStyle: IdxRichEditTableCellStyle;
    function GetTable: IdxRichEditTable;
    function GetTopPadding: Single;
    function GetVerticalAlignment: TdxRichEditTableCellVerticalAlignment;
    function GetWordWrap: Boolean;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetBottomPadding(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetHeightType(const Value: TdxRichEditHeightType);
    procedure SetLeftPadding(const Value: Single);
    procedure SetPreferredWidth(const Value: Single);
    procedure SetPreferredWidthType(const Value: TdxRichEditWidthType);
    procedure SetRightPadding(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditTableCellStyle);
    procedure SetTopPadding(const Value: Single);
    procedure SetVerticalAlignment(const Value: TdxRichEditTableCellVerticalAlignment);
    procedure SetWordWrap(const Value: Boolean);

    procedure Split(ARowCount, AColumnCount: Integer);
    procedure Delete;

    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property Borders: IdxRichEditTableCellBorders read GetBorders;
    property BottomPadding: Single read GetBottomPadding write SetBottomPadding;
    property ColumnSpan: Integer read GetColumnSpan;
    property ContentRange: IdxRichEditDocumentRange read GetContentRange;
    property Height: Single read GetHeight write SetHeight;
    property HeightType: TdxRichEditHeightType read GetHeightType write SetHeightType;
    property Index: Integer read GetIndex;
    property LeftPadding: Single read GetLeftPadding write SetLeftPadding;
    property NestingLevel: Integer read GetNestingLevel;
    property Next: IdxRichEditTableCell read GetNext;
    property PreferredWidth: Single read GetPreferredWidth write SetPreferredWidth;
    property PreferredWidthType: TdxRichEditWidthType read GetPreferredWidthType write SetPreferredWidthType;
    property Previous: IdxRichEditTableCell read GetPrevious;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightPadding: Single read GetRightPadding write SetRightPadding;
    property Row: IdxRichEditTableRow read GetRow;
    property Style: IdxRichEditTableCellStyle read GetStyle write SetStyle;
    property Table: IdxRichEditTable read GetTable;
    property TopPadding: Single read GetTopPadding write SetTopPadding;
    property VerticalAlignment: TdxRichEditTableCellVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

  { IdxRichEditTableCellCollection }

  IdxRichEditTableCellCollection = interface(IdxReadOnlyList<IdxRichEditTableCell>)
    function GetFirst: IdxRichEditTableCell;
    function GetLast: IdxRichEditTableCell;

    function Append: IdxRichEditTableCell;
    procedure Delete(AColumnIndex: Integer);
    function InsertBefore(AColumnIndex: Integer): IdxRichEditTableCell;
    function InsertAfter(AColumnIndex: Integer): IdxRichEditTableCell;

    property First: IdxRichEditTableCell read GetFirst;
    property Last: IdxRichEditTableCell read GetLast;
  end;

  { IdxRichEditTableRow }

  IdxRichEditTableRow = interface
    function GetCells: IdxRichEditTableCellCollection;
    function GetFirstCell: IdxRichEditTableCell;
    function GetGridAfter: Integer;
    function GetGridBefore: Integer;
    function GetHeight: Single;
    function GetHeightType: TdxRichEditHeightType;
    function GetIndex: Integer;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
    function GetItem(AColumn: Integer): IdxRichEditTableCell;
    function GetLastCell: IdxRichEditTableCell;
    function GetNestingLevel: Integer;
    function GetNext: IdxRichEditTableRow;
    function GetPrevious: IdxRichEditTableRow;
    function GetRange: IdxRichEditDocumentRange;
    function GetTable: IdxRichEditTable;
    function GetTableRowAlignment: TdxRichEditTableRowAlignment;
    procedure SetHeight(const Value: Single);
    procedure SetHeightType(const Value: TdxRichEditHeightType);
    procedure SetTableRowAlignment(const Value: TdxRichEditTableRowAlignment);

    procedure Delete;

    property Cells: IdxRichEditTableCellCollection read GetCells;
    property FirstCell: IdxRichEditTableCell read GetFirstCell;
    property GridAfter: Integer read GetGridAfter;
    property GridBefore: Integer read GetGridBefore;
    property Height: Single read GetHeight write SetHeight;
    property HeightType: TdxRichEditHeightType read GetHeightType write SetHeightType;
    property Index: Integer read GetIndex;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property Items[AColumn: Integer]: IdxRichEditTableCell read GetItem; default;
    property LastCell: IdxRichEditTableCell read GetLastCell;
    property NestingLevel: Integer read GetNestingLevel;
    property Next: IdxRichEditTableRow read GetNext;
    property Previous: IdxRichEditTableRow read GetPrevious;
    property Range: IdxRichEditDocumentRange read GetRange;
    property Table: IdxRichEditTable read GetTable;
    property TableRowAlignment: TdxRichEditTableRowAlignment read GetTableRowAlignment write SetTableRowAlignment;
  end;

  { IdxRichEditTableRowCollection }

  IdxRichEditTableRowCollection = interface(IdxReadOnlyList<IdxRichEditTableRow>)
    function GetFirst: IdxRichEditTableRow;
    function GetLast: IdxRichEditTableRow;

    function Append: IdxRichEditTableRow;
    procedure Delete(ARowIndex: Integer);
    function InsertBefore(ARowIndex: Integer): IdxRichEditTableRow;
    function InsertAfter(ARowIndex: Integer): IdxRichEditTableRow;

    property First: IdxRichEditTableRow read GetFirst;
    property Last: IdxRichEditTableRow read GetLast;
  end;

  { IdxRichEditTable }

  TdxRichEditTableCellProcessorDelegate = reference to procedure(const ACell: IdxRichEditTableCell;
    ARowIndex, ACellIndex: Integer);
  TdxRichEditTableRowProcessorDelegate = reference to procedure(const ARow: IdxRichEditTableRow; ARowIndex: Integer);

  IdxRichEditTable = interface
    function GetBorders: IdxRichEditTableBorders;
    function GetBottomPadding: Single;
    function GetFirstRow: IdxRichEditTableRow;
    function GetIndent: Single;
    function GetItem(ARow, AColumn: Integer): IdxRichEditTableCell;
    function GetLastRow: IdxRichEditTableRow;
    function GetLeftPadding: Single;
    function GetNestingLevel: Integer;
    function GetParentCell: IdxRichEditTableCell;
    function GetPreferredWidth: Single;
    function GetPreferredWidthType: TdxRichEditWidthType;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightPadding: Single;
    function GetRows: IdxRichEditTableRowCollection;
    function GetStyle: IdxRichEditTableStyle;
    function GetTableAlignment: TdxRichEditTableRowAlignment;
    function GetTableBackgroundColor: TdxAlphaColor;
    function GetTableCellSpacing: Single;
    function GetTableLayout: TdxRichEditTableLayoutType;
    function GetTableLook: TdxRichEditTableLookTypes;
    function GetTopPadding: Single;
    procedure SetBottomPadding(const Value: Single);
    procedure SetIndent(const Value: Single);
    procedure SetLeftPadding(const Value: Single);
    procedure SetPreferredWidth(const Value: Single); overload;
    procedure SetPreferredWidthType(const Value: TdxRichEditWidthType);
    procedure SetRightPadding(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditTableStyle);
    procedure SetTableAlignment(const Value: TdxRichEditTableRowAlignment);
    procedure SetTableBackgroundColor(const Value: TdxAlphaColor);
    procedure SetTableCellSpacing(const Value: Single);
    procedure SetTableLayout(const Value: TdxRichEditTableLayoutType);
    procedure SetTableLook(const Value: TdxRichEditTableLookTypes);
    procedure SetTopPadding(const Value: Single);

    procedure BeginUpdate;
    procedure EndUpdate;

    function Cell(ARow, AColumn: Integer): IdxRichEditTableCell;
    procedure Validate;
    procedure MergeCells(const AMergeFrom, AMergeTo: IdxRichEditTableCell);
    procedure SetPreferredWidth(AWidth: Single; AWidthType: TdxRichEditWidthType); overload;
    procedure ForEachCell(const ACellProcessor: TdxRichEditTableCellProcessorDelegate);
    procedure ForEachRow(const ARowProcessor: TdxRichEditTableRowProcessorDelegate);

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditTablePropertiesMask); overload;

    property Borders: IdxRichEditTableBorders read GetBorders;
    property BottomPadding: Single read GetBottomPadding write SetBottomPadding;
    property FirstRow: IdxRichEditTableRow read GetFirstRow;
    property Indent: Single read GetIndent write SetIndent;
    property LastRow: IdxRichEditTableRow read GetLastRow;
    property LeftPadding: Single read GetLeftPadding write SetLeftPadding;
    property NestingLevel: Integer read GetNestingLevel;
    property ParentCell: IdxRichEditTableCell read GetParentCell;
    property PreferredWidth: Single read GetPreferredWidth write SetPreferredWidth;
    property PreferredWidthType: TdxRichEditWidthType read GetPreferredWidthType write SetPreferredWidthType;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightPadding: Single read GetRightPadding write SetRightPadding;
    property Rows: IdxRichEditTableRowCollection read GetRows;
    property Self[ARow, AColumn: Integer]: IdxRichEditTableCell read GetItem; default;
    property Style: IdxRichEditTableStyle read GetStyle write SetStyle;
    property TableAlignment: TdxRichEditTableRowAlignment read GetTableAlignment write SetTableAlignment;
    property TableBackgroundColor: TdxAlphaColor read GetTableBackgroundColor write SetTableBackgroundColor;
    property TableCellSpacing: Single read GetTableCellSpacing write SetTableCellSpacing;
    property TableLayout: TdxRichEditTableLayoutType read GetTableLayout write SetTableLayout;
    property TableLook: TdxRichEditTableLookTypes read GetTableLook write SetTableLook;
    property TopPadding: Single read GetTopPadding write SetTopPadding;
  end;

  { IdxRichEditReadOnlyTableCollection }

  IdxRichEditReadOnlyTableCollection = interface(IdxReadOnlyList<IdxRichEditTable>)
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyTableCollection;
    function GetTableCell(const APos: IdxRichEditDocumentPosition): IdxRichEditTableCell;
  end;

  { IdxRichEditTableCollection }

  IdxRichEditTableCollection = interface(IdxRichEditReadOnlyTableCollection)
    function GetFirst: IdxRichEditTable;
    function GetLast: IdxRichEditTable;

    function Add(const APos: IdxRichEditDocumentPosition; ARowCount, AColumnCount: Integer;
      AAutoFitBehavior: TdxRichEditAutoFitBehaviorType = TdxRichEditAutoFitBehaviorType.AutoFitToContents;
      AFixedColumnWidths: Integer = MinInt): IdxRichEditTable;
    procedure Delete(ATableIndex: Integer);
    procedure Remove(const ATable: IdxRichEditTable);
    function IndexOf(const ATable: IdxRichEditTable): Integer;

    property First: IdxRichEditTable read GetFirst;
    property Last: IdxRichEditTable read GetLast;
  end;


  TdxRichEditNumberingType = (
    MultiLevel,
    Simple,
    Bullet);

  { IdxRichEditListLevelProperties }

  IdxRichEditListLevelProperties = interface
    function GetConvertPreviousLevelNumberingToDecimal: Boolean;
    function GetDisplayFormatString: string;
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    function GetRelativeRestartLevel: Integer;
    function GetSeparator: Char;
    function GetStart: Integer;
    function GetSuppressBulletResize: Boolean;
    function GetSuppressRestart: Boolean;
    procedure SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
    procedure SetDisplayFormatString(const Value: string);
    procedure SetNumberingFormat(const Value: TdxRichEditNumberingFormat);
    procedure SetRelativeRestartLevel(const Value: Integer);
    procedure SetSeparator(const Value: Char);
    procedure SetStart(const Value: Integer);
    procedure SetSuppressBulletResize(const Value: Boolean);
    procedure SetSuppressRestart(const Value: Boolean);

    property ConvertPreviousLevelNumberingToDecimal: Boolean read GetConvertPreviousLevelNumberingToDecimal write SetConvertPreviousLevelNumberingToDecimal;
    property DisplayFormatString: string read GetDisplayFormatString write SetDisplayFormatString;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
    property RelativeRestartLevel: Integer read GetRelativeRestartLevel write SetRelativeRestartLevel;
    property Separator: Char read GetSeparator write SetSeparator;
    property Start: Integer read GetStart write SetStart;
    property SuppressBulletResize: Boolean read GetSuppressBulletResize write SetSuppressBulletResize;
    property SuppressRestart: Boolean read GetSuppressRestart write SetSuppressRestart;
  end;

  { IdxRichEditListLevel }

  IdxRichEditListLevel = interface(IdxRichEditListLevelProperties)
    function GetBulletLevel: Boolean;
    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetParagraphStyle: IdxRichEditParagraphStyle;
    procedure SetParagraphStyle(const Value: IdxRichEditParagraphStyle);

    property BulletLevel: Boolean read GetBulletLevel;
    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property ParagraphStyle: IdxRichEditParagraphStyle read GetParagraphStyle write SetParagraphStyle;
  end;

  { IdxReadOnlyListLevelCollection<T> }

  IdxReadOnlyListLevelCollection<T: IdxRichEditListLevel> = interface(IdxReadOnlyList<T>)
  end;

  { IdxRichEditOverrideListLevel }

  IdxRichEditOverrideListLevel = interface(IdxRichEditListLevel)
    function GetNewStart: Integer;
    function GetOverrideStart: Boolean;
    procedure SetNewStart(const Value: Integer);

    procedure SetOverrideStart(Value: Boolean);

    property NewStart: Integer read GetNewStart write SetNewStart;
    property OverrideStart: Boolean read GetOverrideStart;
  end;

  { IdxRichEditNumberingListBase }

  IdxRichEditNumberingListBase = interface
    function GetId: Integer;
    function GetLevels: IdxReadOnlyListLevelCollection<IdxRichEditListLevel>;
    function GetNumberingType: TdxRichEditNumberingType;
    procedure SetId(const Value: Integer);
    procedure SetNumberingType(const Value: TdxRichEditNumberingType);

    property Id: Integer read GetID write SetId;
    property Levels: IdxReadOnlyListLevelCollection<IdxRichEditListLevel> read GetLevels;
    property NumberingType: TdxRichEditNumberingType read GetNumberingType write SetNumberingType;
  end;

  { IdxRichEditAbstractNumberingList }

  IdxRichEditAbstractNumberingList = interface(IdxRichEditNumberingListBase)
    function GetIndex: Integer;
    property Index: Integer read GetIndex;
  end;

  { IdxRichEditListLevelCollection<T> }

  IdxRichEditListLevelCollection<T: IdxRichEditListLevel> = interface(IdxReadOnlyListLevelCollection<T>)
    function Add: IdxRichEditOverrideListLevel; overload;
    procedure Add(const ALevel: IdxRichEditOverrideListLevel); overload;
    function CreateNew: IdxRichEditOverrideListLevel;
  end;

  { IdxRichEditNumberingList }

  IdxRichEditNumberingList = interface(IdxRichEditNumberingListBase)
    function GetAbstractNumberingList: IdxRichEditAbstractNumberingList;
    function GetAbstractNumberingListIndex: Integer;
    function GetIndex: Integer;
    function GetLevels: IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel>;

    property AbstractNumberingList: IdxRichEditAbstractNumberingList read GetAbstractNumberingList;
    property AbstractNumberingListIndex: Integer read GetAbstractNumberingListIndex;
    property Index: Integer read GetIndex;
    property Levels: IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel> read GetLevels;
  end;

  { IdxRichEditTemplateAbstractNumberingList }

  IdxRichEditTemplateAbstractNumberingList = interface(IdxRichEditNumberingListBase)
    function CreateNew: IdxRichEditAbstractNumberingList;
  end;

  { IdxRichEditAbstractNumberingListCollection }

  IdxRichEditAbstractNumberingListCollection = interface(IdxReadOnlyList<IdxRichEditAbstractNumberingList>)
    function GetBulletedListTemplate: IdxRichEditTemplateAbstractNumberingList;
    function GetMultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList;
    function GetNumberedListTemplate: IdxRichEditTemplateAbstractNumberingList;

    function Add: IdxRichEditAbstractNumberingList; overload;
    procedure Add(const AList: IdxRichEditAbstractNumberingList); overload;
    function CreateNew: IdxRichEditAbstractNumberingList;

    property BulletedListTemplate: IdxRichEditTemplateAbstractNumberingList read GetBulletedListTemplate;
    property MultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList read GetMultiLevelListTemplate;
    property NumberedListTemplate: IdxRichEditTemplateAbstractNumberingList read GetNumberedListTemplate;
  end;

  { IdxRichEditNumberingListCollection }

  IdxRichEditNumberingListCollection = interface(IdxReadOnlyList<IdxRichEditNumberingList>)
    function Add(AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList; overload;
    procedure Add(const AList: IdxRichEditNumberingList); overload;
    function CreateNew(AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList;
  end;


  TdxRichEditSubDocumentType = (
    Main,
    Header,
    Footer,
    TextBox);

  { TdxRichEditTextFragmentOptions }

  TdxRichEditTextFragmentOptions = record
  public
    AllowExtendingDocumentRange: Boolean;
    PreserveOriginalNumbering: Boolean;
    class function Create: TdxRichEditTextFragmentOptions; static;
  end;

  { IdxRichEditSubDocument }

  IdxRichEditSubDocument = interface
    function GetBookmarks: IdxRichEditBookmarkCollection;
    function GetFields: IdxRichEditFieldCollection;
    function GetHyperlinks: IdxRichEditHyperlinkCollection;
    function GetImages: IdxRichEditDocumentImageCollection; overload;
    function GetRange: IdxRichEditDocumentRange;
    function GetParagraphs: IdxRichEditParagraphCollection; overload;
    function GetLength: Integer;
    function GetPageBackColor: TdxAlphaColor;
    function GetShapes: IdxRichEditShapeCollection; overload;
    function GetShowPageBackground: Boolean;
    function GetTables: IdxRichEditTableCollection;

    procedure SetPageBackColor(const Value: TdxAlphaColor);
    procedure SetShowPageBackground(const Value: Boolean);

    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const ARange: IdxRichEditDocumentRange;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const ARange: IdxRichEditDocumentRange; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;

    function AppendParagraph: IdxRichEditParagraph;
    function InsertParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function InsertParagraph(const APos: IdxRichEditDocumentPosition; const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph; overload;

    function AppendText(const AText: string): IdxRichEditDocumentRange;
    function InsertText(const APos: IdxRichEditDocumentPosition; const AText: string): IdxRichEditDocumentRange;

    function AppendSingleLineText(const AText: string): IdxRichEditDocumentRange;
    function InsertSingleLineText(const APos: IdxRichEditDocumentPosition; const AText: string): IdxRichEditDocumentRange;

    function AppendRtfText(const ARtfText: string;
      AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;
    function InsertRtfText(const APos: IdxRichEditDocumentPosition;
      const ARtfText: string; AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;


    function AppendImage(AImage: TGraphic): IdxRichEditDocumentImage;
    function InsertImage(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditDocumentImage;

    function InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;
    function InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape;

    function InsertTable(const APos: IdxRichEditDocumentPosition; ARowCount, AColumnCount: Integer;
      AAutoFitBehavior: TdxRichEditAutoFitBehaviorType = TdxRichEditAutoFitBehaviorType.AutoFitToContents;
      AFixedColumnWidths: Integer = MinInt): IdxRichEditTable;

    function CreatePosition(AStart: Integer): IdxRichEditDocumentPosition;
    function CreateRange(AStart, ALength: Integer): IdxRichEditDocumentRange; overload;
    function CreateRange(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditDocumentRange; overload;

    procedure Delete(const ARange: IdxRichEditDocumentRange);
    procedure SelectAll;

    function GetParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
    function GetParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphCollection; overload;
    function GetImages(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection; overload;
    function GetShapes(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection; overload;

    function BeginUpdateParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphProperties;
    procedure EndUpdateParagraphs(const AProperties: IdxRichEditParagraphProperties);
    function BeginUpdateCharacters(AStart, ALength: Integer): IdxRichEditCharacterProperties; overload;
    function BeginUpdateCharacters(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditCharacterProperties; overload;
    function BeginUpdateCharacters(const ARange: IdxRichEditDocumentRange): IdxRichEditCharacterProperties; overload;
    procedure EndUpdateCharacters(const AProperties: IdxRichEditCharacterProperties);

    function BeginUpdateRangePermissions: IdxRichEditRangePermissionCollection;
    procedure EndUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
    procedure CancelUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);

    function GetText(const ARange: IdxRichEditDocumentRange): string; overload;
    function GetText(const ARange: IdxRichEditDocumentRange; const AFragmentOptions: TdxRichEditTextFragmentOptions): string; overload;
    function GetRtfText(const ARange: IdxRichEditDocumentRange): string; overload;
    function GetOpenXmlBytes(const ARange: IdxRichEditDocumentRange): TBytes; overload;

    function CreateBookmark(const AStart: IdxRichEditDocumentPosition; ALength: Integer; const AName: string): IdxRichEditBookmark; overload;
    function CreateBookmark(const ARange: IdxRichEditDocumentRange; const AName: string): IdxRichEditBookmark; overload;
    procedure SelectBookmark(const ABookmark: IdxRichEditBookmark); overload;
    procedure RemoveBookmark(const ABookmark: IdxRichEditBookmark); overload;


    function CreateHyperlink(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditHyperlink; overload;
    function CreateHyperlink(const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink; overload;
    procedure RemoveHyperlink(const AHyperlink: IdxRichEditHyperlink);

    procedure BeginUpdate;
    procedure EndUpdate;

    function Replace(const ARange: IdxRichEditDocumentRange; const AText: string): Integer;
    function FindAll(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
      const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>; overload;
    function FindAll(const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>; overload;
    function ReplaceAll(const ATextToFind, AReplaceWith: string;
      AOptions: TdxRichEditSearchOptions = []; const ARange: IdxRichEditDocumentRange = nil): Integer; overload;
    function ReplaceAll(const ARegex: TRegEx; const AReplaceWith: string; const ARange: IdxRichEditDocumentRange = nil): Integer; overload;
    function StartSearch(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
      ADirection: TdxRichEditSearchDirection = TdxRichEditSearchDirection.Forward;
      const ARange: IdxRichEditDocumentRange = nil): IdxRichEditSearchResult; overload;
    function StartSearch(const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange = nil): IdxRichEditRegexSearchResult; overload;

    procedure AddParagraphsToList(const ARange: IdxRichEditDocumentRange; const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      const AList: IdxRichEditNumberingList; ALevelIndex: Integer); overload;
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      ANumberingListIndex, ALevelIndex: Integer); overload;
    procedure RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
    procedure RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);

    function GetSubDocumentType: TdxRichEditSubDocumentType;

    property Range: IdxRichEditDocumentRange read GetRange;
    property Length: Integer read GetLength;
    property PageBackColor: TdxAlphaColor read GetPageBackColor write SetPageBackColor;
    property Paragraphs: IdxRichEditParagraphCollection read GetParagraphs;
    property ShowPageBackground: Boolean read GetShowPageBackground write SetShowPageBackground;
    property Fields: IdxRichEditFieldCollection read GetFields;
    property Bookmarks: IdxRichEditBookmarkCollection read GetBookmarks;
    property Hyperlinks: IdxRichEditHyperlinkCollection read GetHyperlinks;
    property Tables: IdxRichEditTableCollection read GetTables;
    property Shapes: IdxRichEditShapeCollection read GetShapes;
    property Images: IdxRichEditDocumentImageCollection read GetImages;
  end;

  { IdxRichEditDocument }

  IdxRichEditDocument = interface(IdxRichEditSubDocument)
    function GetOpenXmlBytes: TBytes; overload;
    function GetRtfText: string; overload;
    function GetText: string; overload;
    procedure SetOpenXmlBytes(const Value: TBytes);
    procedure SetRtfText(const Value: string);
    procedure SetText(const Value: string);

    function GetAbstractNumberingLists: IdxRichEditAbstractNumberingListCollection;
    function GetActiveSubDocument: IdxRichEditSubDocument;
    function GetCaretPosition: IdxRichEditDocumentPosition;
    function GetCharacterStyles: IdxRichEditCharacterStyleCollection;
    function GetDefaultCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetDefaultParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetDefaultTableProperties: IdxRichEditTablePropertiesBase;
    function GetIsEmpty: Boolean;
    function GetNumberingLists: IdxRichEditNumberingListCollection;
    function GetParagraphStyles: IdxRichEditParagraphStyleCollection;
    function GetSections: IdxRichEditSectionCollection;
    function GetSelections: IdxRichEditSelectionCollection;
    function GetSelection: IdxRichEditDocumentRange;
    function GetTableStyles: IdxRichEditTableStyleCollection;
    function GetUnit: TdxRichEditDocumentUnit;
    function GetVariables: IdxRichEditDocumentVariableCollection;
    procedure SetCaretPosition(const Value: IdxRichEditDocumentPosition);
    procedure SetSelection(const Value: IdxRichEditDocumentRange);
    procedure SetUnit(const Value: TdxRichEditDocumentUnit);


    procedure CreateNewDocument;
    procedure LoadDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string); overload;
    procedure LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string); overload;
    procedure SaveDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure SaveDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;

    function InsertSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;
    function AppendSection: IdxRichEditSection;
    function GetSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;

    procedure Copy; overload;
    procedure Copy(const ARange: IdxRichEditDocumentRange); overload;
    procedure Cut; overload;
    procedure Cut(const ARange: IdxRichEditDocumentRange); overload;
    procedure Paste; overload;
    procedure Paste(const AFormat: TdxRichEditDocumentFormat); overload;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions;
    procedure MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument); overload;

    function GetIsDocumentProtected: Boolean;
    procedure Protect(const APassword: string);
    procedure Unprotect;
    procedure SetEncryptionPassword(const APassword: string);
    function HasEncryptionPassword: Boolean;

    procedure ChangeActiveDocument(const ADocument: IdxRichEditSubDocument);


    property DefaultCharacterProperties: IdxRichEditCharacterPropertiesBase read GetDefaultCharacterProperties;
    property DefaultParagraphProperties: IdxRichEditParagraphPropertiesBase read GetDefaultParagraphProperties;
    property DefaultTableProperties: IdxRichEditTablePropertiesBase read GetDefaultTableProperties;
    property CharacterStyles: IdxRichEditCharacterStyleCollection read GetCharacterStyles;
    property ParagraphStyles: IdxRichEditParagraphStyleCollection read GetParagraphStyles;
    property TableStyles: IdxRichEditTableStyleCollection read GetTableStyles;

    property AbstractNumberingLists: IdxRichEditAbstractNumberingListCollection read GetAbstractNumberingLists;
    property ActiveSubDocument: IdxRichEditSubDocument read GetActiveSubDocument;
    property CaretPosition: IdxRichEditDocumentPosition read GetCaretPosition write SetCaretPosition;
    property IsDocumentProtected: Boolean read GetIsDocumentProtected;
    property IsEmpty: Boolean read GetIsEmpty;
    property NumberingLists: IdxRichEditNumberingListCollection read GetNumberingLists;
    property Sections: IdxRichEditSectionCollection read GetSections;
    property Selection: IdxRichEditDocumentRange read GetSelection write SetSelection;
    property Selections: IdxRichEditSelectionCollection read GetSelections;
    property Variables: IdxRichEditDocumentVariableCollection read GetVariables;
    property &Unit: TdxRichEditDocumentUnit read GetUnit write SetUnit;


    property OpenXmlBytes: TBytes read GetOpenXmlBytes write SetOpenXmlBytes;
    property RtfText: string read GetRtfText write SetRtfText;
    property Text: string read GetText write SetText;
  end;

implementation

{ TdxRichEditTextFragmentOptions }

class function TdxRichEditTextFragmentOptions.Create: TdxRichEditTextFragmentOptions;
begin
  Result.AllowExtendingDocumentRange := True;
  Result.PreserveOriginalNumbering := False;
end;

end.
