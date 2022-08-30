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

unit dxRichEdit.DocumentModel.Numbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics, dxCultureInfo,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.UnitConverter;

const
  AbstractNumberingListIndexInvalidValue = -1;
  AbstractNumberingListIndexMinValue     = 0;
  AbstractNumberingListIndexMaxValue     = MaxInt;

type
  TdxAbstractListLevel = class;
  TdxNumberingListStyle = class;
  TdxAbstractNumberingListCollection = class;

  { IdxListLevel }

  IdxListLevel = interface
  ['{98A61643-3F8C-40BF-BA7C-8A1C4337A7CE}']
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetListLevelProperties: TdxListLevelProperties;
    function GetParagraphProperties: TdxParagraphProperties;
    function GetParagraphStyle: TdxParagraphStyle;
    function GetCharacterProperties: TdxCharacterProperties;
    function GetBulletLevel: Boolean;
    function GetTabs: TdxTabProperties;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property ListLevelProperties: TdxListLevelProperties read GetListLevelProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property BulletLevel: Boolean read GetBulletLevel;

    property ParagraphStyle: TdxParagraphStyle read GetParagraphStyle;
    property Tabs: TdxTabProperties read GetTabs;
  end;

  { IdxInternalListLevel }

  IdxInternalListLevel = interface(IdxListLevel)
  ['{6B785813-3EA3-4D68-B1D9-0FA066CF6563}']
    function GetFontCacheIndex: Integer;

    property FontCacheIndex: Integer read GetFontCacheIndex;
  end;

  { IdxOverrideListLevel }

  IdxOverrideListLevel = interface(IdxListLevel)
  ['{C7B75CBF-6493-44E9-8693-2BC325C6E085}']
    function GetNewStart: Integer;
    function GetOverrideStart: Boolean;
    procedure SetNewStart(const Value: Integer);
    procedure SetOverrideStart(AValue: Boolean);

    property NewStart: Integer read GetNewStart write SetNewStart;
    property OverrideStart: Boolean read GetOverrideStart;
  end;

  { TdxListIdProviderBase }

  TdxListIdProviderBase = class abstract
  private
    FDocumentModel: TdxCustomDocumentModel;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel);

    function GetNextId: Integer; virtual; abstract;
    procedure Reset; virtual; abstract;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  end;

  { TdxAbstractNumberingListIdProvider }

  TdxAbstractNumberingListIdProvider = class(TdxListIdProviderBase)
  strict private
    class var
      FLock: TObject;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function IsNewId(AId: Integer): Boolean;
  public
    procedure Reset; override;
    function GetNextId: Integer; override;
  end;

  { TdxNumberingListIdProvider }

  TdxNumberingListIdProvider = class(TdxListIdProviderBase)
  private
    FLastId: Integer;
  protected
    function IsNewId(AId: Integer): Boolean;
  public
    procedure Reset; override;
    function GetNextId: Integer; override;
  end;

  { TdxListLevelCollection }

  TdxListLevelCollection = class
  private
    FList: TdxFastObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxAbstractListLevel;
    procedure SetItem(AIndex: Integer; AObject: TdxAbstractListLevel);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ALevel: TdxAbstractListLevel);

    property List: TdxFastObjectList read FList;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxAbstractListLevel read GetItem write SetItem; default;
  end;

  { TdxAbstractListLevel }

  TdxAbstractListLevel = class abstract(TcxIUnknownObject,
    IdxListLevel)
  protected
    function GetListLevelProperties: TdxListLevelProperties; virtual; abstract;
    function GetParagraphProperties: TdxParagraphProperties; virtual; abstract;
    function GetParagraphStyle: TdxParagraphStyle; virtual; abstract;
    function GetCharacterProperties: TdxCharacterProperties; virtual; abstract;
    function GetBulletLevel: Boolean; virtual; abstract;
    function GetTabs: TdxTabProperties; virtual; abstract;
    function GetDocumentModel: TdxCustomDocumentModel; virtual; abstract;
    function GetOverrideStart: Boolean; virtual; abstract;
    function GetNewStart: Integer; virtual; abstract;
    procedure SetNewStart(const Value: Integer); virtual; abstract;
  public
    property ListLevelProperties: TdxListLevelProperties read GetListLevelProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property BulletLevel: Boolean read GetBulletLevel;
    property ParagraphStyle: TdxParagraphStyle read GetParagraphStyle;
    property Tabs: TdxTabProperties read GetTabs;
    property OverrideStart: Boolean read GetOverrideStart;
    property NewStart: Integer read GetNewStart write SetNewStart;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
  end;

  { TdxListLevel }

  TdxListLevel = class(TdxAbstractListLevel,
    IdxInternalListLevel,
    IdxParagraphPropertiesContainer,
    IdxCharacterPropertiesContainer,
    IdxListLevel,
    IdxCharacterProperties,
    IdxParagraphProperties)
  public const
    BulletLevelDisplayFormatStringLength = 1;
  private
    FParagraphProperties: TdxParagraphProperties;
    FCharacterProperties: TdxCharacterProperties;
    FListLevelProperties: TdxListLevelProperties;
    FParagraphStyleIndex: Integer;
    FDocumentModel: TdxCustomDocumentModel;
    FMergedCharacterFormattingCacheIndex: Integer;
    FMergedParagraphFormattingCacheIndex: Integer;
    FFontCacheIndex: Integer;
    FTabs: TdxTabProperties;
    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetDoubleFontSize: Integer;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetFontBold: Boolean;
    function GetFontCacheIndex: Integer;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetForeColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetMainPieceTable: TdxCustomPieceTable;
    function GetMergedCharacterFormatting: TdxCharacterFormattingInfo;
    function GetMergedCharacterFormattingCacheIndex: Integer;
    function GetMergedParagraphFormatting: TdxParagraphFormattingInfo;
    function GetMergedParagraphFormattingCacheIndex: Integer;
    function GetOutlineLevel: Integer;
    function GetNoProof: Boolean;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetScript: TdxCharacterFormattingScript;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;
    function GetWidowOrphanControl: Boolean;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetAllCaps(const Value: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetFontBold(const Value: Boolean);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetHidden(const Value: Boolean);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetParagraphStyleIndex(const Value: Integer);
    procedure SetRightIndent(const Value: Integer);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    procedure SetNoProof(const Value: Boolean);
    function GetLeftBorder: TdxBorderInfo;
    function GetTopBorder: TdxBorderInfo;
    function GetRightBorder: TdxBorderInfo;
    function GetBottomBorder: TdxBorderInfo;
    procedure SetLeftBorder(const AValue: TdxBorderInfo);
    procedure SetRightBorder(const AValue: TdxBorderInfo);
    procedure SetTopBorder(const AValue: TdxBorderInfo);
    procedure SetBottomBorder(const AValue: TdxBorderInfo);
  protected
    function GetPieceTable: TdxCustomPieceTable;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;
    procedure OnParagraphPropertiesChanged;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetOverrideStart: Boolean; override;
    function GetNewStart: Integer; override;
    procedure SetNewStart(const Value: Integer); override;

    function GetDocumentModel: TdxCustomDocumentModel; override;

    function GetBulletLevel: Boolean; override;
    function GetCharacterProperties: TdxCharacterProperties; override;
    function GetListLevelProperties: TdxListLevelProperties; override;
    function GetParagraphProperties: TdxParagraphProperties; override;
    function GetParagraphStyle: TdxParagraphStyle; override;
    function GetTabs: TdxTabProperties; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); virtual;
    destructor Destroy; override;
    procedure OnParagraphStyleChanged;
    procedure CopyFrom(AListLevel: TdxListLevel); virtual;
    function Equals(AObj: TObject): Boolean; override;
    function Clone: TdxListLevel; virtual;
    function CreateLevel: TdxListLevel; virtual;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; virtual;
    procedure OnPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);
    property FontCacheIndex: Integer read GetFontCacheIndex;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write SetParagraphStyleIndex;
    property ParagraphStyle: TdxParagraphStyle read GetParagraphStyle;
    property Tabs: TdxTabProperties read GetTabs;
    property PieceTable: TdxCustomPieceTable read GetMainPieceTable;
    property MergedCharacterFormattingCacheIndex: Integer read GetMergedCharacterFormattingCacheIndex;
    property MergedParagraphFormattingCacheIndex: Integer read GetMergedParagraphFormattingCacheIndex;
    property MergedCharacterFormatting: TdxCharacterFormattingInfo read GetMergedCharacterFormatting;
    property MergedParagraphFormatting: TdxParagraphFormattingInfo read GetMergedParagraphFormatting;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontName: string read GetFontName write SetFontName;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;

    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
  end;

  { TdxNumberingListBaseImpl }

  TdxNumberingListBaseImpl = class(TcxIUnknownObject)
  strict private
    FId: Integer;
    FDocumentModel: TdxCustomDocumentModel;
    FLevels: TdxListLevelCollection;
    function GetAbstractNumberingLists: TdxAbstractNumberingListCollection;
  protected
    function GetLevels: TdxListLevelCollection; virtual;
    function GetDocumentModel: TdxCustomDocumentModel;
    property AbstractNumberingLists: TdxAbstractNumberingListCollection read GetAbstractNumberingLists;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; ALevelCount: Integer);
    destructor Destroy; override;

    function CreateLevel(ALevelIndex: Integer): TdxAbstractListLevel; virtual; abstract;
    function EqualsLevels(AListLevel: TdxListLevelCollection): Boolean;
    function GenerateNewId: Integer; virtual; abstract;
    function GetId: Integer;
    function Id: Integer;
    function IsEqual(AList: TdxNumberingListBaseImpl): Boolean;
    procedure CopyFrom(AList: TdxNumberingListBaseImpl); virtual;
    procedure CopyLevelsFrom(ASourceLevels: TdxListLevelCollection); virtual; abstract;
    procedure InitLevels(ALevelCount: Integer); virtual;
    procedure SetId(AId: Integer); virtual;
    procedure SetLevel(ALevelIndex: Integer; AValue: TdxListLevel); virtual;

    property Levels: TdxListLevelCollection read GetLevels;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
  end;

  { TdxOverrideListLevel }

  TdxOverrideListLevel = class(TdxListLevel, IdxOverrideListLevel)
  private
    FOverrideStart: Boolean;
  protected
    function GetOverrideStart: Boolean; override;
    function GetNewStart: Integer; override;
    procedure SetNewStart(const Value: Integer); override;
  public
    function CreateLevel: TdxListLevel; override;

    procedure SetOverrideStart(AValue: Boolean); virtual;
  end;

  { TdxAbstractNumberingList }

  TdxAbstractNumberingList = class(TdxNumberingListBaseImpl)
  private
    FDeleted: Boolean;
    FNumberingStyleReferenceIndex: Integer;
    FStyleLinkIndex: Integer;
    function GetNumberingStyleReference: TdxNumberingListStyle;
    function GetStyleLink: TdxNumberingListStyle;
    function GetTemplateCode: Integer;
    function GetUseStyleLevels: Boolean;
  protected
    function GetLevels: TdxListLevelCollection; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    function CreateLevel(ALevelIndex: Integer): TdxAbstractListLevel; override;
    procedure CopyLevelsFrom(ASourceLevels: TdxListLevelCollection); override;
    function Clone: TdxAbstractNumberingList;
    function CreateNumberingList: TdxAbstractNumberingList; virtual;
    function GenerateNewId: Integer; override;

    // for internal use
    procedure SetNumberingStyleReferenceIndex(const Value: Integer);
    procedure SetStyleLinkIndex(const Value: Integer);

    property Deleted: Boolean read FDeleted write FDeleted;
    property NumberingStyleReferenceIndex: Integer read FNumberingStyleReferenceIndex;
    property StyleLinkIndex: Integer read FStyleLinkIndex;
    property StyleLink: TdxNumberingListStyle read GetStyleLink;
    property NumberingStyleReference: TdxNumberingListStyle read GetNumberingStyleReference;
    property TemplateCode: Integer read GetTemplateCode;
    property UseStyleLevels: Boolean read GetUseStyleLevels;
  end;

  { TdxAbstractNumberingListCollection }

  TdxAbstractNumberingListCollection = class(TdxObjectList<TdxAbstractNumberingList>)
  public
    function HasListOfType(AListType: TdxNumberingType): Boolean;
  end;

  { TdxNumberingListHelper }

  TdxNumberingListHelper = class
  public type
    TdxListLevelPropertiesDelegate = reference to function(AProperties: TdxListLevelProperties): Boolean;
  public const
    TemplateCodeStart = $100;
    TemplateCodeEnd = $7FFFFFFF;
  public
    class function GetLevelType(const ANumberingList: TdxNumberingListBaseImpl; ALevelIndex: Integer): TdxNumberingType; static;
    class function GetListType(const ANumberingList: TdxNumberingListBaseImpl): TdxNumberingType; static;
    class function IsHybridList(const ANumberingList: TdxNumberingListBaseImpl): Boolean; static;
    class function IsBulletLevel(const ALevel: IdxListLevel): Boolean; static;
    class function GenerateNewTemplateCode(ADocumentModel: TdxCustomDocumentModel): Integer; static;
    class function IsNewTemplateCode(ADocumentModel: TdxCustomDocumentModel; ATemplateCode: Integer): Boolean; static;
    class procedure SetHybridListType(const AList: TdxNumberingListBaseImpl); static;
    class procedure SetSimpleListType(const AList: TdxNumberingListBaseImpl); static;
    class procedure SetListLevelsProperty(const AList: TdxNumberingListBaseImpl; const ACondition: TdxListLevelPropertiesDelegate;
      const AAction: TdxAction<TdxListLevelProperties>); static;
    class procedure SetListType(const AList: TdxNumberingListBaseImpl; AValue: TdxNumberingType); static;
    class procedure SetListLevelsFormat(const AList: TdxNumberingListBaseImpl; AFormat: TdxRichEditNumberingFormat); static;
    class function GetAbstractListIndexByType(AListCollection: TdxAbstractNumberingListCollection; AType: TdxNumberingType): TdxAbstractNumberingListIndex; static;
    class function GetAbstractListByType(AListCollection: TdxAbstractNumberingListCollection; AType: TdxNumberingType): TdxAbstractNumberingList; static;
  end;

  { TdxNumberingList }

  TdxNumberingList = class(TdxNumberingListBaseImpl)
  strict private
    FReferringParagraphsCount: Integer;
    FAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
    function GetAbstractNumberingList: TdxAbstractNumberingList;
    function GetNumberingListIdProvider: TdxNumberingListIdProvider;
  protected
    property NumberingListIdProvider: TdxNumberingListIdProvider read GetNumberingListIdProvider;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AAbstractNumberingListIndex: TdxAbstractNumberingListIndex; ALevelCount: Integer = 9);
    function Clone: TdxNumberingList;

    function CanRemove: Boolean;
    function CreateLevel(ALevelIndex: Integer): TdxAbstractListLevel; override;
    function GenerateNewId: Integer; override;
    procedure CopyLevelsFrom(ASourceLevels: TdxListLevelCollection); override;

    property AbstractNumberingListIndex: TdxAbstractNumberingListIndex read FAbstractNumberingListIndex;
    property AbstractNumberingList: TdxAbstractNumberingList read GetAbstractNumberingList;
    procedure OnParagraphAdded;
    procedure OnParagraphRemoved;
  end;

  { TdxNumberingListReferenceLevel }

  TdxNumberingListReferenceLevel = class(TdxAbstractListLevel,
      IdxInternalListLevel,
      IdxOverrideListLevel,
      IdxListLevel,
      IdxCharacterProperties, IdxParagraphProperties
    )
  private
    FLevel: Integer;
    FOwner: TdxNumberingList;
    FOverrideStart: Boolean;
    FNewStart: Integer;
    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetDoubleFontSize: Integer;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetFontBold: Boolean;
    function GetFontCacheIndex: Integer;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetForeColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetNoProof: Boolean;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetScript: TdxCharacterFormattingScript;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;
    function GetWidowOrphanControl: Boolean;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetAllCaps(const Value: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetFontBold(const Value: Boolean);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetHidden(const Value: Boolean);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetNoProof(const Value: Boolean);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetRightIndent(const Value: Integer);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    function GetLeftBorder: TdxBorderInfo;
    function GetTopBorder: TdxBorderInfo;
    function GetRightBorder: TdxBorderInfo;
    function GetBottomBorder: TdxBorderInfo;
    procedure SetLeftBorder(const AValue: TdxBorderInfo);
    procedure SetTopBorder(const AValue: TdxBorderInfo);
    procedure SetRightBorder(const AValue: TdxBorderInfo);
    procedure SetBottomBorder(const AValue: TdxBorderInfo);
  protected
    // IdxListLevel implements
    function GetBulletLevel: Boolean; override;
    function GetCharacterProperties: TdxCharacterProperties; override;
    function GetListLevelProperties: TdxListLevelProperties; override;
    function GetParagraphProperties: TdxParagraphProperties; override;
    function GetParagraphStyle: TdxParagraphStyle; override;
    function GetTabs: TdxTabProperties; override;
    function GetDocumentModel: TdxCustomDocumentModel; override;
    function GetOverrideStart: Boolean; override;
    function GetNewStart: Integer; override;
    procedure SetNewStart(const Value: Integer); override;

    procedure SetLevelCore(ALevel: Integer);
    function GetOwnerLevel: TdxListLevel; virtual;
  public
    constructor Create(AOwner: TdxNumberingList; ALevel: Integer);
    procedure SetOverrideStart(AValue: Boolean); virtual;

    property Owner: TdxNumberingList read FOwner;
    property Level: Integer read FLevel;
    property FontCacheIndex: Integer read GetFontCacheIndex;
    property OwnerLevel: TdxListLevel read GetOwnerLevel;

    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ListLevelProperties: TdxListLevelProperties read GetListLevelProperties;
    property BulletLevel: Boolean read GetBulletLevel;
    property FontName: string read GetFontName write SetFontName;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property ParagraphStyle: TdxParagraphStyle read GetParagraphStyle;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
  end;

  { TdxNumberingListCollection }

  TdxNumberingListCollection = class(TdxObjectList<TdxNumberingList>) 
  strict private
    function GetItem(Index: Integer): TdxNumberingList;
  public
    function First: TdxNumberingList;
    function Last: TdxNumberingList;

    property Items[Index: Integer]: TdxNumberingList read GetItem; default;
  end;

  { TdxNumberingListStyle }

  TdxNumberingListStyle = class(TdxStyleBase)
  strict private
    FNumberingListIndex: TdxNumberingListIndex;
    function GetParent: TdxNumberingListStyle;
    procedure SetParent(const Value: TdxNumberingListStyle);
    function GetNumberingList: TdxNumberingList;
  strict protected
    function CalculateChangeActions: TdxDocumentModelChangeActions; override;
    function CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxNumberingListStyle; virtual;
    function GetType: TdxStyleType; override;
    procedure MergePropertiesWithParent; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; ANumberingListIndex: TdxNumberingListIndex; const AStyleName: string = ''); reintroduce; overload;
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AStyleName: string); reintroduce; overload;

    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; override;
    procedure CopyProperties(ASource: TdxStyleBase); override;
    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType); override;

    // for internal use
    procedure SetNumberingListIndex(const Value: TdxNumberingListIndex);

    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex;
    property NumberingList: TdxNumberingList read GetNumberingList;
    property Parent: TdxNumberingListStyle read GetParent write SetParent;
  end;

  { TdxNumberingListStyleCollection }

  TdxNumberingListStyleCollection = class(TdxStyleCollectionBase)
  public const
    DefaultListStyleName = 'List';
  strict private
    function GetItem(Index: Integer): TdxNumberingListStyle;
  strict protected
    function CanDeleteStyle(AItem: TdxStyleBase): Boolean; override;
    function CreateDefaultItem: TdxStyleBase; override;
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase); override;
  public
    property Items[Index: Integer]: TdxNumberingListStyle read GetItem; default;
  end;

  { TdxNumberingListCountersCalculator }

  TdxNumberingListCountersCalculator = class
  strict private
    FCounters: TIntegerDynArray;
    FList: TdxAbstractNumberingList;
    FUsedListIndex: TdxSortedList<TdxNumberingListIndex>;
    procedure AdvanceSkippedLevelCounters(AListLevelIndex: Integer);
    function GetCounterCount: Integer;
    procedure RestartNextLevelCounters(AListLevelIndex: Integer);
  strict protected
    procedure CreateListLevelCounters;
  public
    constructor Create(AList: TdxAbstractNumberingList);
    destructor Destroy; override;
    procedure AdvanceListLevelCounters(AParagraph: TdxSimpleParagraph; AListLevelIndex: Integer);
    procedure BeginCalculateCounters;
    function CalculateCounters(AParagraph: TdxSimpleParagraph): TIntegerDynArray;
    function CalculateNextCounters(ACurrentParagraph: TdxSimpleParagraph): TIntegerDynArray;
    procedure EndCalculateCounters;
    function GetActualRangeCounters(AListLevelIndex: Integer): TIntegerDynArray;
    function ShouldAdvanceListLevelCounters(AParagraph: TdxSimpleParagraph;
      AbstractNumberingList: TdxAbstractNumberingList): Boolean;

    property List: TdxAbstractNumberingList read FList;
    property Counters: TIntegerDynArray read FCounters write FCounters;
    property CounterCount: Integer read GetCounterCount;
  end;

  { TdxNumberingListIndexCalculator }

  TdxNumberingListIndexCalculator = class
  strict private
    FModel: TdxSimpleDocumentModel;
    FNumberingListType: TdxNumberingType;
    FContinueList: Boolean;
    FNestingLevel: Integer;
  protected
    function GetListIndexCore(AParagraphIndex: TdxParagraphIndex): TdxNumberingListIndex;

    property DocumentModel: TdxSimpleDocumentModel read FModel;
  public
    constructor Create(AModel: TdxSimpleDocumentModel; ANumberingListType: TdxNumberingType);
    function GetListIndex(AStart, AEnd: TdxParagraphIndex): TdxNumberingListIndex; virtual;
    function CreateNewAbstractList(ASource: TdxAbstractNumberingList): TdxAbstractNumberingListIndex;
    function CreateNewList(ASource: TdxAbstractNumberingList): TdxNumberingListIndex; virtual;

    property NumberingListType: TdxNumberingType read FNumberingListType;
    property ContinueList: Boolean read FContinueList write FContinueList;
    property NestingLevel: Integer read FNestingLevel write FNestingLevel;
  end;

  { TdxDefaultNumberingListHelper }

  TdxDefaultNumberingListHelper = class
  public type
    TCreateDefaultAbstractList = function (DocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList;
  protected
    class function CreateDefaultBulletAbstractNumberingList(DocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList; static;
    class function CreateDefaultSimpleAbstractNumberingList(DocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList; static;
    class function CreateDefaultMultilevelAbstractNumberingList(DocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList; static;

    class procedure SetTemplateCode(const ALevel: IdxListLevel; ATemplateCode: Integer);
    class procedure SetDisplayFormatString(const ALevel: IdxListLevel; const ADisplayFormatString: string);
    class procedure SetFirstLineIndent(const ALevel: IdxListLevel; ALineIndent, AFirstLineIndent: Integer);
    class procedure InsertDefaultNumberingList(DocumentModel: TdxCustomDocumentModel; AUnitConverter: TdxDocumentModelUnitConverter;
      ADefaultTabWidth: Integer; ACreateFunction: TCreateDefaultAbstractList); static;
  public
    class procedure InsertNumberingLists(ADocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer);
    class procedure InsertDefaultSimpleNumberingList(ADocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer); static;
    class procedure InsertDefaultBulletNumberingList(ADocumentModel: TdxCustomDocumentModel;
      AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer); static;
  end;

implementation

uses
  RTLConsts, StrUtils, Math, dxCore,

  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxCharacters;


{ TdxListIdProviderBase }

constructor TdxListIdProviderBase.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
  Reset;
end;

{ TdxAbstractNumberingListIdProvider }

class constructor TdxAbstractNumberingListIdProvider.Initialize;
begin
  FLock := TObject.Create;
  Randomize;
end;

class destructor TdxAbstractNumberingListIdProvider.Finalize;
begin
  FLock.Free;
end;

procedure TdxAbstractNumberingListIdProvider.Reset;
begin
end;

function TdxAbstractNumberingListIdProvider.GetNextId: Integer;
begin
  repeat                              
    System.TMonitor.Enter(FLock);     
    Result := RandomRange(0, MaxInt); 
    System.TMonitor.Exit(FLock);      
  until IsNewId(Result);              
end;

function TdxAbstractNumberingListIdProvider.IsNewId(AId: Integer): Boolean;
var
  I: Integer;
  ALists: TdxAbstractNumberingListCollection;
  AMaxIndex: TdxAbstractNumberingListIndex;
begin
  ALists := TdxDocumentModel(DocumentModel).AbstractNumberingLists;
  AMaxIndex := ALists.Count - 1;
  for I := 0 to AMaxIndex do
    if AId = ALists[I].GetId then
      Exit(False);
  Result := True;
end;

{ TdxNumberingListIdProvider }

function TdxNumberingListIdProvider.GetNextId: Integer;
begin
  repeat
    Inc(FLastId);
  until IsNewId(FLastId);
  Result := FLastId;
end;

procedure TdxNumberingListIdProvider.Reset;
begin
  FLastId := 0;
end;

function TdxNumberingListIdProvider.IsNewId(AId: Integer): Boolean;
begin
  Result := AId > TdxDocumentModel(DocumentModel).NumberingLists.Max(function(const AItem: TdxNumberingList): Integer
    begin
      Result := AItem.GetId;
    end);
end;

{ TdxNumberingListReferenceLevel }

constructor TdxNumberingListReferenceLevel.Create(AOwner: TdxNumberingList; ALevel: Integer);
begin
  inherited Create;
  Assert(AOwner <> nil);
  Assert(ALevel >= 0);
  FOwner := AOwner;
  SetLevelCore(ALevel);
end;

procedure TdxNumberingListReferenceLevel.SetOutlineLevel(const Value: Integer);
begin
  OwnerLevel.OutlineLevel := Value;
end;

procedure TdxNumberingListReferenceLevel.SetOverrideStart(AValue: Boolean);
begin
  FOverrideStart := AValue;
end;

procedure TdxNumberingListReferenceLevel.SetPageBreakBefore(const Value: Boolean);
begin
  OwnerLevel.PageBreakBefore := Value;
end;

procedure TdxNumberingListReferenceLevel.SetRightIndent(const Value: Integer);
begin
  OwnerLevel.RightIndent := Value;
end;

procedure TdxNumberingListReferenceLevel.SetScript(const Value: TdxCharacterFormattingScript);
begin
  OwnerLevel.Script := Value;
end;

procedure TdxNumberingListReferenceLevel.SetSpacingAfter(const Value: Integer);
begin
  OwnerLevel.SpacingAfter := Value;
end;

procedure TdxNumberingListReferenceLevel.SetSpacingBefore(const Value: Integer);
begin
  OwnerLevel.SpacingBefore := Value;
end;

procedure TdxNumberingListReferenceLevel.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  OwnerLevel.StrikeoutColor := Value;
end;

procedure TdxNumberingListReferenceLevel.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  OwnerLevel.StrikeoutWordsOnly := Value;
end;

procedure TdxNumberingListReferenceLevel.SetSuppressHyphenation(const Value: Boolean);
begin
  OwnerLevel.SuppressHyphenation := Value;
end;

procedure TdxNumberingListReferenceLevel.SetSuppressLineNumbers(const Value: Boolean);
begin
  OwnerLevel.SuppressLineNumbers := Value;
end;

procedure TdxNumberingListReferenceLevel.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  OwnerLevel.UnderlineColor := Value;
end;

procedure TdxNumberingListReferenceLevel.SetUnderlineWordsOnly(const Value: Boolean);
begin
  OwnerLevel.UnderlineWordsOnly := Value;
end;

procedure TdxNumberingListReferenceLevel.SetWidowOrphanControl(const Value: Boolean);
begin
  OwnerLevel.WidowOrphanControl := Value;
end;

procedure TdxNumberingListReferenceLevel.SetAfterAutoSpacing(const Value: Boolean);
begin
  OwnerLevel.AfterAutoSpacing := Value;
end;

procedure TdxNumberingListReferenceLevel.SetAlignment(const Value: TdxParagraphAlignment);
begin
  OwnerLevel.Alignment := Value;
end;

procedure TdxNumberingListReferenceLevel.SetAllCaps(const Value: Boolean);
begin
  OwnerLevel.AllCaps := Value;
end;

procedure TdxNumberingListReferenceLevel.SetBackColor(const Value: TdxAlphaColor);
begin
  OwnerLevel.BackColor := Value;
end;

function TdxNumberingListReferenceLevel.GetLeftBorder: TdxBorderInfo;
begin
  Result := OwnerLevel.LeftBorder;
end;

procedure TdxNumberingListReferenceLevel.SetLeftBorder(const AValue: TdxBorderInfo);
begin
  OwnerLevel.LeftBorder := AValue;
end;

function TdxNumberingListReferenceLevel.GetRightBorder: TdxBorderInfo;
begin
  Result := OwnerLevel.RightBorder;
end;

procedure TdxNumberingListReferenceLevel.SetRightBorder(const AValue: TdxBorderInfo);
begin
  OwnerLevel.RightBorder := AValue;
end;

function TdxNumberingListReferenceLevel.GetTopBorder: TdxBorderInfo;
begin
  Result := OwnerLevel.TopBorder;
end;

procedure TdxNumberingListReferenceLevel.SetTopBorder(const AValue: TdxBorderInfo);
begin
  OwnerLevel.TopBorder := AValue;
end;

function TdxNumberingListReferenceLevel.GetBottomBorder: TdxBorderInfo;
begin
  Result := OwnerLevel.BottomBorder;
end;

procedure TdxNumberingListReferenceLevel.SetBottomBorder(const AValue: TdxBorderInfo);
begin
  OwnerLevel.BottomBorder := AValue;
end;

procedure TdxNumberingListReferenceLevel.SetBeforeAutoSpacing(const Value: Boolean);
begin
  OwnerLevel.BeforeAutoSpacing := Value;
end;

procedure TdxNumberingListReferenceLevel.SetContextualSpacing(const Value: Boolean);
begin
  OwnerLevel.ContextualSpacing := Value;
end;

procedure TdxNumberingListReferenceLevel.SetDoubleFontSize(const Value: Integer);
begin
  OwnerLevel.DoubleFontSize := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFirstLineIndent(const Value: Integer);
begin
  OwnerLevel.FirstLineIndent := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  OwnerLevel.FirstLineIndentType := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFontBold(const Value: Boolean);
begin
  OwnerLevel.FontBold := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFontItalic(const Value: Boolean);
begin
  OwnerLevel.FontItalic := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFontName(const Value: string);
begin
  OwnerLevel.FontName := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  OwnerLevel.FontStrikeoutType := Value;
end;

procedure TdxNumberingListReferenceLevel.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  OwnerLevel.FontUnderlineType := Value;
end;

procedure TdxNumberingListReferenceLevel.SetForeColor(const Value: TdxAlphaColor);
begin
  OwnerLevel.ForeColor := Value;
end;

procedure TdxNumberingListReferenceLevel.SetHidden(const Value: Boolean);
begin
  OwnerLevel.Hidden := Value;
end;

procedure TdxNumberingListReferenceLevel.SetKeepLinesTogether(const Value: Boolean);
begin
  OwnerLevel.KeepLinesTogether := Value;
end;

procedure TdxNumberingListReferenceLevel.SetKeepWithNext(const Value: Boolean);
begin
  OwnerLevel.KeepWithNext := Value;
end;

procedure TdxNumberingListReferenceLevel.SetLeftIndent(const Value: Integer);
begin
  OwnerLevel.LeftIndent := Value;
end;

procedure TdxNumberingListReferenceLevel.SetLevelCore(ALevel: Integer);
begin
  FLevel := ALevel;
end;

procedure TdxNumberingListReferenceLevel.SetLineSpacing(const Value: Single);
begin
  OwnerLevel.LineSpacing := Value;
end;

procedure TdxNumberingListReferenceLevel.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  OwnerLevel.LineSpacingType := Value;
end;


function TdxNumberingListReferenceLevel.GetOwnerLevel: TdxListLevel;
begin
  Result := Owner.AbstractNumberingList.Levels.Items[Level] as TdxListLevel;
end;

function TdxNumberingListReferenceLevel.GetFirstLineIndent: Integer;
begin
  Result := OwnerLevel.FirstLineIndent;
end;

function TdxNumberingListReferenceLevel.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := OwnerLevel.FirstLineIndentType;
end;

function TdxNumberingListReferenceLevel.GetFontBold: Boolean;
begin
  Result := OwnerLevel.FontBold;
end;

function TdxNumberingListReferenceLevel.GetFontCacheIndex: Integer;
begin
  Result := (OwnerLevel as IdxInternalListLevel).FontCacheIndex;
end;

function TdxNumberingListReferenceLevel.GetFontItalic: Boolean;
begin
  Result := OwnerLevel.FontItalic;
end;

function TdxNumberingListReferenceLevel.GetFontName: string;
begin
  Result := OwnerLevel.FontName;
end;

function TdxNumberingListReferenceLevel.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := OwnerLevel.FontStrikeoutType;
end;

function TdxNumberingListReferenceLevel.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := OwnerLevel.FontUnderlineType;
end;

function TdxNumberingListReferenceLevel.GetForeColor: TdxAlphaColor;
begin
  Result := OwnerLevel.ForeColor;
end;

function TdxNumberingListReferenceLevel.GetHidden: Boolean;
begin
  Result := OwnerLevel.Hidden;
end;

function TdxNumberingListReferenceLevel.GetKeepLinesTogether: Boolean;
begin
  Result := OwnerLevel.KeepLinesTogether;
end;

function TdxNumberingListReferenceLevel.GetKeepWithNext: Boolean;
begin
  Result := OwnerLevel.KeepWithNext;
end;

function TdxNumberingListReferenceLevel.GetLeftIndent: Integer;
begin
  Result := OwnerLevel.LeftIndent;
end;

function TdxNumberingListReferenceLevel.GetLineSpacing: Single;
begin
  Result := OwnerLevel.LineSpacing;
end;

function TdxNumberingListReferenceLevel.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := OwnerLevel.LineSpacingType;
end;

function TdxNumberingListReferenceLevel.GetListLevelProperties: TdxListLevelProperties;
begin
  Result := OwnerLevel.ListLevelProperties;
end;

function TdxNumberingListReferenceLevel.GetPageBreakBefore: Boolean;
begin
  Result := OwnerLevel.PageBreakBefore;
end;

function TdxNumberingListReferenceLevel.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := OwnerLevel.ParagraphProperties;
end;

function TdxNumberingListReferenceLevel.GetParagraphStyle: TdxParagraphStyle;
begin
  Result := OwnerLevel.ParagraphStyle;
end;

function TdxNumberingListReferenceLevel.GetTabs: TdxTabProperties;
begin
  Result := OwnerLevel.Tabs;
end;

function TdxNumberingListReferenceLevel.GetRightIndent: Integer;
begin
  Result := OwnerLevel.RightIndent;
end;

function TdxNumberingListReferenceLevel.GetScript: TdxCharacterFormattingScript;
begin
  Result := OwnerLevel.Script;
end;

function TdxNumberingListReferenceLevel.GetSpacingAfter: Integer;
begin
  Result := OwnerLevel.SpacingAfter;
end;

function TdxNumberingListReferenceLevel.GetSpacingBefore: Integer;
begin
  Result := OwnerLevel.SpacingBefore;
end;

function TdxNumberingListReferenceLevel.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := OwnerLevel.StrikeoutColor;
end;

function TdxNumberingListReferenceLevel.GetStrikeoutWordsOnly: Boolean;
begin
  Result := OwnerLevel.StrikeoutWordsOnly;
end;

function TdxNumberingListReferenceLevel.GetSuppressHyphenation: Boolean;
begin
  Result := OwnerLevel.SuppressHyphenation;
end;

function TdxNumberingListReferenceLevel.GetSuppressLineNumbers: Boolean;
begin
  Result := OwnerLevel.SuppressLineNumbers;
end;

function TdxNumberingListReferenceLevel.GetUnderlineColor: TdxAlphaColor;
begin
  Result := OwnerLevel.UnderlineColor;
end;

function TdxNumberingListReferenceLevel.GetUnderlineWordsOnly: Boolean;
begin
  Result := OwnerLevel.UnderlineWordsOnly;
end;

function TdxNumberingListReferenceLevel.GetWidowOrphanControl: Boolean;
begin
  Result := OwnerLevel.WidowOrphanControl;
end;

function TdxNumberingListReferenceLevel.GetAfterAutoSpacing: Boolean;
begin
  Result := OwnerLevel.AfterAutoSpacing;
end;

function TdxNumberingListReferenceLevel.GetAlignment: TdxParagraphAlignment;
begin
  Result := OwnerLevel.Alignment;
end;

function TdxNumberingListReferenceLevel.GetAllCaps: Boolean;
begin
  Result := OwnerLevel.AllCaps;
end;

function TdxNumberingListReferenceLevel.GetBackColor: TdxAlphaColor;
begin
  Result := OwnerLevel.BackColor;
end;

function TdxNumberingListReferenceLevel.GetBeforeAutoSpacing: Boolean;
begin
  Result := OwnerLevel.BeforeAutoSpacing;
end;

function TdxNumberingListReferenceLevel.GetBulletLevel: Boolean;
begin
  Result := OwnerLevel.BulletLevel;
end;

function TdxNumberingListReferenceLevel.GetOutlineLevel: Integer;
begin
  Result := OwnerLevel.OutlineLevel;
end;

function TdxNumberingListReferenceLevel.GetOverrideStart: Boolean;
begin
  Result := FOverrideStart;
end;

function TdxNumberingListReferenceLevel.GetNewStart: Integer;
begin
  Result := FNewStart;
end;


function TdxNumberingListReferenceLevel.GetNoProof: Boolean;
begin
  Result := OwnerLevel.NoProof;
end;

procedure TdxNumberingListReferenceLevel.SetNewStart(const Value: Integer);
begin
  FNewStart := Value;
end;

procedure TdxNumberingListReferenceLevel.SetNoProof(const Value: Boolean);
begin
  OwnerLevel.NoProof := Value;
end;

function TdxNumberingListReferenceLevel.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := OwnerLevel.CharacterProperties;
end;

function TdxNumberingListReferenceLevel.GetContextualSpacing: Boolean;
begin
  Result := OwnerLevel.ContextualSpacing;
end;

function TdxNumberingListReferenceLevel.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := OwnerLevel.DocumentModel;
end;

function TdxNumberingListReferenceLevel.GetDoubleFontSize: Integer;
begin
  Result := OwnerLevel.DoubleFontSize;
end;

{ TdxNumberingList }

function TdxNumberingList.CanRemove: Boolean;
begin
  Result := FReferringParagraphsCount <= 0;
end;

function TdxNumberingList.Clone: TdxNumberingList;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxNumberingList.Create(DocumentModel, FAbstractNumberingListIndex);
  Result.CopyFrom(Self);
end;

function TdxNumberingList.CreateLevel(ALevelIndex: Integer): TdxAbstractListLevel;
begin
  Result := TdxNumberingListReferenceLevel.Create(Self, ALevelIndex);
end;

procedure TdxNumberingList.CopyLevelsFrom(ASourceLevels: TdxListLevelCollection);
var
  I: Integer;
  ALevel: TdxAbstractListLevel;
  ANewLevel, ASourceLevel: TdxOverrideListLevel;
  ANewILevel, ASourceListLevel: IdxOverrideListLevel;
begin
  for I := 0 to ASourceLevels.Count - 1 do
  begin
    if ASourceLevels[I] is TdxOverrideListLevel then
    begin
      ASourceLevel := TdxOverrideListLevel(ASourceLevels[I]);
      ANewLevel := TdxOverrideListLevel.Create(DocumentModel);
      ANewLevel.CopyFrom(ASourceLevel);
      Levels[I] := ANewLevel;
    end
    else
    begin
      ASourceListLevel := ASourceLevels[I] as IdxOverrideListLevel;

      ALevel := CreateLevel(I);
      ANewILevel := ALevel as IdxOverrideListLevel;
      ANewILevel.SetOverrideStart(ASourceListLevel.OverrideStart);
      ANewILevel.NewStart := ASourceListLevel.NewStart;
      Levels[I] := ALevel;
    end;
  end;
end;

constructor TdxNumberingList.Create(ADocumentModel: TdxCustomDocumentModel;
  AAbstractNumberingListIndex: TdxAbstractNumberingListIndex; ALevelCount: Integer = 9);
begin
  inherited Create(ADocumentModel, ALevelCount);
  if (AAbstractNumberingListIndex < 0) or
    (AAbstractNumberingListIndex > AbstractNumberingLists.Count - 1) then
    TdxRichEditExceptions.ThrowArgumentException('abstractNumberingListIndex', AAbstractNumberingListIndex);
  FAbstractNumberingListIndex := AAbstractNumberingListIndex;
end;

function TdxNumberingList.GenerateNewId: Integer;
begin
  Result := NumberingListIdProvider.GetNextId;
end;

function TdxNumberingList.GetAbstractNumberingList: TdxAbstractNumberingList;
begin
  Result := AbstractNumberingLists[AbstractNumberingListIndex];
end;

function TdxNumberingList.GetNumberingListIdProvider: TdxNumberingListIdProvider;
begin
  Result := TdxDocumentModel(DocumentModel).NumberingListIdProvider;
end;

procedure TdxNumberingList.OnParagraphAdded;
begin
  Inc(FReferringParagraphsCount);
end;

procedure TdxNumberingList.OnParagraphRemoved;
begin
  Dec(FReferringParagraphsCount);
end;

{ TdxNumberingListCollection }

function TdxNumberingListCollection.First: TdxNumberingList;
begin
  Result := TdxNumberingList(inherited First);
end;

function TdxNumberingListCollection.GetItem(Index: Integer): TdxNumberingList;
begin
  Result := TdxNumberingList(inherited Items[Index]);
end;

function TdxNumberingListCollection.Last: TdxNumberingList;
begin
  Result := TdxNumberingList(inherited Last);
end;

{ TdxNumberingListStyle }

constructor TdxNumberingListStyle.Create(ADocumentModel: TdxCustomDocumentModel; ANumberingListIndex: TdxNumberingListIndex;
  const AStyleName: string);
begin
  inherited Create(ADocumentModel, nil, AStyleName);
  if (ANumberingListIndex < NumberingListIndexMinValue) and
      (ANumberingListIndex <> NumberingListIndexNoNumberingList) then
    TdxRichEditExceptions.ThrowArgumentException('ANumberingListIndex', ANumberingListIndex);
  FNumberingListIndex := ANumberingListIndex;
end;

function TdxNumberingListStyle.CalculateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.ParagraphStyle);
end;

function TdxNumberingListStyle.Copy(ATargetModel: TdxCustomDocumentModel): Integer;
var
  I: Integer;
  ATarget: TdxDocumentModel absolute ATargetModel;
begin
  for I := 0 to ATarget.NumberingListStyles.Count - 1 do
    if StyleName = ATarget.NumberingListStyles[I].StyleName then
      Exit(I);
  Result := ATarget.NumberingListStyles.AddNewStyle(CopyFrom(ATarget));
end;

function TdxNumberingListStyle.CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxNumberingListStyle;
var
  ACopiedNumberingListIndex: TdxNumberingListIndex;
begin
  if FNumberingListIndex <> NumberingListIndexNoNumberingList then
    ACopiedNumberingListIndex := TdxDocumentModel(DocumentModel).GetNumberingListIndex(ATargetModel, FNumberingListIndex, NumberingListIndexMaxValue)
  else
    ACopiedNumberingListIndex := NumberingListIndexNoNumberingList;
  Result := TdxNumberingListStyle.Create(ATargetModel, ACopiedNumberingListIndex, StyleName);
  Assert(Parent = nil);
end;

procedure TdxNumberingListStyle.CopyProperties(ASource: TdxStyleBase);
begin
  FNumberingListIndex := TdxNumberingListStyle(ASource).NumberingListIndex;
end;

constructor TdxNumberingListStyle.Create(ADocumentModel: TdxCustomDocumentModel;
  const AStyleName: string);
begin
  Create(ADocumentModel, NumberingListIndexNoNumberingList, AStyleName);
end;

function TdxNumberingListStyle.GetNumberingList: TdxNumberingList;
begin
  Result := TdxDocumentModel(DocumentModel).NumberingLists[FNumberingListIndex];
end;

function TdxNumberingListStyle.GetParent: TdxNumberingListStyle;
begin
  Result := TdxNumberingListStyle(inherited Parent);
end;

function TdxNumberingListStyle.GetType: TdxStyleType;
begin
  Result := TdxStyleType.NumberingListStyle;
end;

procedure TdxNumberingListStyle.MergePropertiesWithParent;
begin
  Assert(Parent = nil);
end;

procedure TdxNumberingListStyle.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
end;

procedure TdxNumberingListStyle.SetNumberingListIndex(
  const Value: TdxNumberingListIndex);
begin
  FNumberingListIndex := Value;
end;

procedure TdxNumberingListStyle.SetParent(const Value: TdxNumberingListStyle);
begin
  inherited Parent := Value;
end;

{ TdxNumberingListStyleCollection }

function TdxNumberingListStyleCollection.CanDeleteStyle(AItem: TdxStyleBase): Boolean;
begin
  Result := False; 
end;

function TdxNumberingListStyleCollection.CreateDefaultItem: TdxStyleBase;
begin
  Result := nil;
end;

function TdxNumberingListStyleCollection.GetItem(Index: Integer): TdxNumberingListStyle;
begin
  Result := TdxNumberingListStyle(inherited Items[Index]);
end;

procedure TdxNumberingListStyleCollection.NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable;
  AStyle: TdxStyleBase);
begin
end;

{ TdxNumberingListCountersCalculator }

constructor TdxNumberingListCountersCalculator.Create(AList: TdxAbstractNumberingList);
begin
  inherited Create;
  FList := AList;
end;

destructor TdxNumberingListCountersCalculator.Destroy;
begin
  FUsedListIndex.Free;
  inherited Destroy;
end;

procedure TdxNumberingListCountersCalculator.BeginCalculateCounters;
begin
  CreateListLevelCounters;
  FreeAndNil(FUsedListIndex);
  FUsedListIndex := TdxSortedList<TdxNumberingListIndex>.Create(TComparer<TdxNumberingListIndex>.Default);
end;

procedure TdxNumberingListCountersCalculator.EndCalculateCounters;
begin
// do nothing
end;

function TdxNumberingListCountersCalculator.CalculateCounters(AParagraph: TdxSimpleParagraph): TIntegerDynArray;
var
  AbstractNumberingList: TdxAbstractNumberingList;
  I: Integer;
  ACurrentParagraph: TdxParagraph;
  AInnerParagraph: TdxParagraph absolute AParagraph;
begin
  BeginCalculateCounters;
  try
    AbstractNumberingList := AInnerParagraph.GetAbstractNumberingList;
    Assert(List <> nil); 
    for I := 0 to AInnerParagraph.Index do
    begin
      ACurrentParagraph := AInnerParagraph.PieceTable.Paragraphs[I];
      if ShouldAdvanceListLevelCounters(ACurrentParagraph, AbstractNumberingList) then
        AdvanceListLevelCounters(ACurrentParagraph, ACurrentParagraph.GetListLevelIndex);
    end;
    Result := GetActualRangeCounters(AInnerParagraph.GetListLevelIndex);
  finally
    EndCalculateCounters;
  end;
end;

function TdxNumberingListCountersCalculator.CalculateNextCounters(ACurrentParagraph: TdxSimpleParagraph): TIntegerDynArray;
var
  AbstractNumberingList: TdxAbstractNumberingList;
  AParagraph: TdxParagraph absolute ACurrentParagraph;
begin
  AbstractNumberingList := AParagraph.GetAbstractNumberingList;
  if ShouldAdvanceListLevelCounters(AParagraph, AbstractNumberingList) then
    AdvanceListLevelCounters(AParagraph, AParagraph.GetListLevelIndex);
  Result := GetActualRangeCounters(AParagraph.GetListLevelIndex);
end;

procedure TdxNumberingListCountersCalculator.CreateListLevelCounters;
var
  ALevels: TdxListLevelCollection;
  I: Integer;
  AListLevel: IdxListLevel; 
begin
  ALevels := List.Levels;
  SetLength(FCounters, CounterCount);
  for I := 0 to CounterCount - 1 do
  begin
    Supports(ALevels[I], IdxListLevel, AListLevel);
    FCounters[I] := AListLevel.ListLevelProperties.Start - 1;
  end;
end;

function TdxNumberingListCountersCalculator.ShouldAdvanceListLevelCounters(AParagraph: TdxSimpleParagraph;
  AbstractNumberingList: TdxAbstractNumberingList): Boolean;
begin
  Result := TdxParagraph(AParagraph).GetAbstractNumberingList = AbstractNumberingList;
end;

procedure TdxNumberingListCountersCalculator.AdvanceListLevelCounters(AParagraph: TdxSimpleParagraph; AListLevelIndex: Integer);
var
  ANumberingListIndex: TdxNumberingListIndex;
  ANumberingList: TdxNumberingList;
  ALevel: IdxOverrideListLevel;
begin
  ANumberingListIndex := AParagraph.GetNumberingListIndex;
  ANumberingList := TdxParagraph(AParagraph).DocumentModel.NumberingLists[ANumberingListIndex];
  if not Supports(ANumberingList.Levels[AListLevelIndex], IdxOverrideListLevel, ALevel) then
    Assert(False);
  if ALevel.OverrideStart and not FUsedListIndex.Contains(ANumberingListIndex) then
  begin
    FUsedListIndex.Add(ANumberingListIndex);
    Counters[AListLevelIndex] := ALevel.NewStart;
  end
  else
    Counters[AListLevelIndex] := Counters[AListLevelIndex] + 1;
  AdvanceSkippedLevelCounters(AListLevelIndex);
  RestartNextLevelCounters(AListLevelIndex);
end;

procedure TdxNumberingListCountersCalculator.AdvanceSkippedLevelCounters(AListLevelIndex: Integer);
var
  ALevels: TdxListLevelCollection;
  I: Integer;
  AListLevel: IdxListLevel; 
begin
  ALevels := List.Levels;
  for I := 0 to AListLevelIndex - 1 do
  begin
    if not Supports(ALevels[I], IdxListLevel, AListLevel) then
      Assert(False);
    if Counters[I] = AListLevel.ListLevelProperties.Start - 1 then
      Counters[I] := Counters[I] + 1;
  end;
end;

function TdxNumberingListCountersCalculator.GetActualRangeCounters(AListLevelIndex: Integer): TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, AListLevelIndex + 1);
  for I := 0 to AListLevelIndex do
    Result[I] := Counters[I];
end;

function TdxNumberingListCountersCalculator.GetCounterCount: Integer;
begin
  Result := List.Levels.Count;
end;

procedure TdxNumberingListCountersCalculator.RestartNextLevelCounters(AListLevelIndex: Integer);
var
  ALevels: TdxListLevelCollection;
  I: Integer;
  ARestartedLevels: array of Boolean;
  AProperties: TdxListLevelProperties;
  ARestartLevel: Integer;
  AListLevel: IdxListLevel; 
begin
  ALevels := List.Levels;
  SetLength(ARestartedLevels, CounterCount);
  try
    ARestartedLevels[AListLevelIndex] := True;
    for I := AListLevelIndex + 1 to CounterCount - 1 do
    begin
      if not Supports(ALevels[I], IdxListLevel, AListLevel) then
        Assert(False);
      AProperties := AListLevel.ListLevelProperties;
      if not AProperties.SuppressRestart then
      begin
        ARestartLevel := I - AProperties.RelativeRestartLevel - 1;
        if (ARestartLevel >= 0) and (ARestartLevel < CounterCount) and ARestartedLevels[ARestartLevel] then
        begin
          Counters[I] := AProperties.Start - 1;
          ARestartedLevels[I] := True;
        end;
      end;
    end;
  finally
    SetLength(ARestartedLevels, 0);
  end;
end;

{ TdxListLevelCollection }

constructor TdxListLevelCollection.Create;
begin
  inherited Create;
  FList := TdxFastObjectList.Create(True, 16);
end;

destructor TdxListLevelCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxListLevelCollection.Add(ALevel: TdxAbstractListLevel);
begin
  List.Add(ALevel);
end;

function TdxListLevelCollection.GetCount: Integer;
begin
  Result := List.Count;
end;

function TdxListLevelCollection.GetItem(AIndex: Integer): TdxAbstractListLevel;
begin
  Result := TdxAbstractListLevel(List[AIndex]);
end;

procedure TdxListLevelCollection.SetItem(AIndex: Integer; AObject: TdxAbstractListLevel);
begin
  List[AIndex] := AObject;
end;

{ TdxNumberingListBaseImpl }

procedure TdxNumberingListBaseImpl.CopyFrom(AList: TdxNumberingListBaseImpl);
begin
  FId := AList.Id;
  CopyLevelsFrom(AList.FLevels);
end;

constructor TdxNumberingListBaseImpl.Create(ADocumentModel: TdxCustomDocumentModel; ALevelCount: Integer);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
  FLevels := TdxListLevelCollection.Create; 
  FId := -1;
  InitLevels(ALevelCount);
end;

destructor TdxNumberingListBaseImpl.Destroy;
begin
  FreeAndNil(FLevels);
  inherited Destroy;
end;

function TdxNumberingListBaseImpl.EqualsLevels(AListLevel: TdxListLevelCollection): Boolean;
var
  I: Integer;
  ALevel: TdxListLevel;
begin
  for I := 0 to AListLevel.Count - 1 do
  begin
    if Levels[I] is TdxListLevel then
      ALevel := TdxListLevel(Levels[I])
    else
      ALevel := nil;
    if (ALevel <> nil) and not ALevel.Equals(AListLevel[I]) then
      Exit(False);
  end;
  Result := True;
end;

function TdxNumberingListBaseImpl.GetId: Integer;
begin
  Result := FId;
end;

function TdxNumberingListBaseImpl.GetLevels: TdxListLevelCollection;
begin
  Result := FLevels;
end;

function TdxNumberingListBaseImpl.Id: Integer;
begin
  if FId = -1 then
    FId := GenerateNewId;
  Result := FId;
end;

procedure TdxNumberingListBaseImpl.InitLevels(ALevelCount: Integer);
var
  I: Integer;
begin
  for I := 0 to ALevelCount - 1 do
    FLevels.Add(CreateLevel(I));
end;

function TdxNumberingListBaseImpl.IsEqual(AList: TdxNumberingListBaseImpl): Boolean;
begin
  Result := EqualsLevels(AList.FLevels);
end;

procedure TdxNumberingListBaseImpl.SetId(AId: Integer);
begin
  FId := AId;
end;

procedure TdxNumberingListBaseImpl.SetLevel(ALevelIndex: Integer; AValue: TdxListLevel);
begin
  Levels.SetItem(ALevelIndex, AValue);
end;

function TdxNumberingListBaseImpl.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FDocumentModel;
end;

function TdxNumberingListBaseImpl.GetAbstractNumberingLists: TdxAbstractNumberingListCollection;
begin
  Result := TdxDocumentModel(FDocumentModel).AbstractNumberingLists;
end;

{ TdxListLevel }

constructor TdxListLevel.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FParagraphStyleIndex := -1;
  FMergedCharacterFormattingCacheIndex := -1;
  FMergedParagraphFormattingCacheIndex := -1;
  FFontCacheIndex := -1;

  FDocumentModel := ADocumentModel;
  FParagraphProperties := TdxParagraphProperties.Create(Self);
  FCharacterProperties := TdxCharacterProperties.Create(Self);
  FListLevelProperties := TdxListLevelProperties.Create(ADocumentModel);
  FTabs := TdxTabProperties.Create(Self);

  FParagraphProperties.OnObtainAffectedRange.Add(OnPropertiesObtainAffectedRange); 
  FCharacterProperties.OnObtainAffectedRange.Add(OnPropertiesObtainAffectedRange); 
  FListLevelProperties.OnObtainAffectedRange.Add(OnPropertiesObtainAffectedRange); 
end;

destructor TdxListLevel.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FListLevelProperties);
  FreeAndNil(FCharacterProperties);
  FreeAndNil(FParagraphProperties);
  inherited Destroy;
end;

function TdxListLevel.Clone: TdxListLevel;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateLevel;
  Result.CopyFrom(Self);
end;

procedure TdxListLevel.CopyFrom(AListLevel: TdxListLevel);
begin
  ListLevelProperties.CopyFrom(AListLevel.ListLevelProperties.Info);
  ParagraphProperties.CopyFrom(AListLevel.ParagraphProperties.Info);
  CharacterProperties.CopyFrom(AListLevel.CharacterProperties.Info);
  ParagraphStyleIndex := AListLevel.ParagraphStyleIndex;
  Tabs.CopyFrom(AListLevel.Tabs.Info);
end;

function TdxListLevel.Equals(AObj: TObject): Boolean;
var
  AOther: TdxListLevel;
  AThisParagraphStyle, AOtherParagraphStyle: TdxParagraphStyle;
begin
  if AObj = Self then
    Exit(True);
  if not (AObj is TdxListLevel) then
    Exit(False);
  AOther := TdxListLevel(AObj);
  if not ListLevelProperties.Info.Equals(AOther.ListLevelProperties.Info) or
    not CharacterProperties.Info.Equals(AOther.CharacterProperties.Info) or
    not ParagraphProperties.Equals(AOther.ParagraphProperties) or
    not Tabs.Info.Equals(AOther.Tabs.Info) then
    Exit(False);
  if DocumentModel = AOther.DocumentModel then
    Result := ParagraphStyleIndex = AOther.ParagraphStyleIndex
  else
  begin
    AThisParagraphStyle := ParagraphStyle;
    AOtherParagraphStyle := AOther.ParagraphStyle;
    if (AThisParagraphStyle = nil) and (AOtherParagraphStyle = nil) then
      Exit(True);
    if (AThisParagraphStyle = nil) or (AOtherParagraphStyle = nil) then
      Exit(False);
    Result := AThisParagraphStyle.StyleName = AOtherParagraphStyle.StyleName;
  end;
end;

function TdxListLevel.CreateLevel: TdxListLevel;
begin
  Result := TdxListLevel.Create(DocumentModel);
end;

function TdxListLevel.GetAfterAutoSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.AfterAutoSpacing;
end;

function TdxListLevel.GetAlignment: TdxParagraphAlignment;
begin
  Result := MergedParagraphFormatting.Alignment;
end;

function TdxListLevel.GetAllCaps: Boolean;
begin
  Result := MergedCharacterFormatting.AllCaps;
end;

function TdxListLevel.GetBackColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.BackColor;
end;

function TdxListLevel.GetBeforeAutoSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.BeforeAutoSpacing;
end;

function TdxListLevel.GetBulletLevel: Boolean;
var
  S: string;
begin
  S := Format(ListLevelProperties.DisplayFormatString, ['0', '0', '0', '0', '0', '0', '0', '0', '0', '0']);
  Result := (ListLevelProperties.Format = TdxRichEditNumberingFormat.Bullet) or
    ((S = ListLevelProperties.DisplayFormatString) and
      (Length(S) = BulletLevelDisplayFormatStringLength));
end;

function TdxListLevel.GetPieceTable: TdxCustomPieceTable;
begin
  Result := GetMainPieceTable;
end;

function TdxListLevel.GetRightIndent: Integer;
begin
  Result := MergedParagraphFormatting.RightIndent;
end;

function TdxListLevel.CreateCharacterPropertiesChangedHistoryItem:
  TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart,
    CharacterProperties);
end;

function TdxListLevel.CreateParagraphPropertiesChangedHistoryItem:
  TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart,
    ParagraphProperties);
end;

procedure TdxListLevel.OnCharacterPropertiesChanged;
begin
  FMergedCharacterFormattingCacheIndex := -1;
  FFontCacheIndex := -1;
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Character);
end;

procedure TdxListLevel.OnParagraphPropertiesChanged;
begin
  FMergedParagraphFormattingCacheIndex := -1;
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Paragraph);
end;

procedure TdxListLevel.OnParagraphStyleChanged;
begin
  FMergedParagraphFormattingCacheIndex := -1;
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.All);
end;

function TdxListLevel.GetMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := TdxMergedParagraphProperties.Create(ParagraphProperties);
  Result.Merge(TdxDocumentModel(DocumentModel).DefaultParagraphProperties);
end;

function TdxListLevel.GetOverrideStart: Boolean;
begin
  dxAbstractError;
  Result := False;
end;

function TdxListLevel.GetNewStart: Integer;
begin
  dxAbstractError;
  Result := -1;
end;

function TdxListLevel.GetNoProof: Boolean;
begin
  Result := MergedCharacterFormatting.NoProof;
end;

procedure TdxListLevel.SetNewStart(const Value: Integer);
begin
  dxAbstractError;
end;

function TdxListLevel.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FDocumentModel;
end;


procedure TdxListLevel.SetNoProof(const Value: Boolean);
begin
  CharacterProperties.NoProof := Value;
end;

function TdxListLevel.GetLeftBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.LeftBorder;
end;

procedure TdxListLevel.SetLeftBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.LeftBorder := AValue;
end;

function TdxListLevel.GetRightBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.RightBorder;
end;

procedure TdxListLevel.SetRightBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.RightBorder := AValue;
end;

function TdxListLevel.GetTopBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.TopBorder;
end;

procedure TdxListLevel.SetTopBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.TopBorder := AValue;
end;

function TdxListLevel.GetBottomBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.BottomBorder;
end;

procedure TdxListLevel.SetBottomBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.BottomBorder := AValue;
end;

function TdxListLevel.GetOutlineLevel: Integer;
begin
  Result := MergedParagraphFormatting.SpacingBefore;
end;


function TdxListLevel.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := FCharacterProperties;
end;

function TdxListLevel.GetContextualSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.ContextualSpacing;
end;

function TdxListLevel.GetDoubleFontSize: Integer;
begin
  Result := MergedCharacterFormatting.DoubleFontSize;
end;

function TdxListLevel.GetFirstLineIndent: Integer;
begin
  Result := MergedParagraphFormatting.FirstLineIndent;
end;

function TdxListLevel.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := MergedParagraphFormatting.FirstLineIndentType;
end;

function TdxListLevel.GetFontBold: Boolean;
begin
  Result := MergedCharacterFormatting.FontBold;
end;

function TdxListLevel.GetFontCacheIndex: Integer;
var
  AFontStyle: TFontStyles;
begin
  if FFontCacheIndex < 0 then
  begin
    AFontStyle := [];
    if FontBold then
      Include(AFontStyle, fsBold);
    if FontItalic then
      Include(AFontStyle, fsItalic);
    FFontCacheIndex := DocumentModel.FontCache.CalcFontIndex(FontName, DoubleFontSize, AFontStyle, Script);
  end;
  Result := FFontCacheIndex;
end;

function TdxListLevel.GetFontItalic: Boolean;
begin
  Result := MergedCharacterFormatting.FontItalic;
end;

function TdxListLevel.GetFontName: string;
begin
  Result := MergedCharacterFormatting.FontName;
end;

function TdxListLevel.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := MergedCharacterFormatting.FontStrikeoutType;
end;

function TdxListLevel.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := MergedCharacterFormatting.FontUnderlineType;
end;

function TdxListLevel.GetForeColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.ForeColor;
end;

function TdxListLevel.GetHidden: Boolean;
begin
  Result := MergedCharacterFormatting.Hidden;
end;

function TdxListLevel.GetKeepLinesTogether: Boolean;
begin
  Result := MergedParagraphFormatting.KeepLinesTogether;
end;

function TdxListLevel.GetKeepWithNext: Boolean;
begin
  Result := MergedParagraphFormatting.KeepWithNext;
end;

function TdxListLevel.GetPageBreakBefore: Boolean;
begin
  Result := MergedParagraphFormatting.PageBreakBefore;
end;

function TdxListLevel.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := FParagraphProperties;
end;

function TdxListLevel.GetScript: TdxCharacterFormattingScript;
begin
  Result := MergedCharacterFormatting.Script;
end;

function TdxListLevel.GetSpacingAfter: Integer;
begin
  Result := MergedParagraphFormatting.SpacingBefore;
end;

function TdxListLevel.GetSpacingBefore: Integer;
begin
  Result := MergedParagraphFormatting.SpacingBefore;
end;

function TdxListLevel.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.StrikeoutColor;
end;

function TdxListLevel.GetStrikeoutWordsOnly: Boolean;
begin
  Result := MergedCharacterFormatting.StrikeoutWordsOnly;
end;

function TdxListLevel.GetSuppressHyphenation: Boolean;
begin
  Result := MergedParagraphFormatting.SuppressHyphenation;
end;

function TdxListLevel.GetSuppressLineNumbers: Boolean;
begin
  Result := MergedParagraphFormatting.SuppressLineNumbers;
end;

function TdxListLevel.GetUnderlineColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.UnderlineColor;
end;

function TdxListLevel.GetUnderlineWordsOnly: Boolean;
begin
  Result := MergedCharacterFormatting.UnderlineWordsOnly;
end;

function TdxListLevel.GetWidowOrphanControl: Boolean;
begin
  Result := MergedParagraphFormatting.WidowOrphanControl;
end;

function TdxListLevel.GetParagraphStyle: TdxParagraphStyle;
begin
  if FParagraphStyleIndex >= 0 then
    Result := TdxDocumentModel(DocumentModel).ParagraphStyles[ParagraphStyleIndex]
  else
    Result := nil;
end;

function TdxListLevel.GetTabs: TdxTabProperties;
begin
  Result := FTabs;
end;

function TdxListLevel.GetMainPieceTable: TdxCustomPieceTable;
begin
  Result := DocumentModel.MainPart;
end;

procedure TdxListLevel.SetAfterAutoSpacing(const Value: Boolean);
begin
  ParagraphProperties.AfterAutoSpacing := Value;
end;

procedure TdxListLevel.SetAlignment(const Value: TdxParagraphAlignment);
begin
  ParagraphProperties.Alignment := Value;
end;

procedure TdxListLevel.SetAllCaps(const Value: Boolean);
begin
  CharacterProperties.AllCaps := Value;
end;

procedure TdxListLevel.SetBackColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.BackColor := Value;
end;

procedure TdxListLevel.SetBeforeAutoSpacing(const Value: Boolean);
begin
  ParagraphProperties.BeforeAutoSpacing := Value;
end;

procedure TdxListLevel.SetContextualSpacing(const Value: Boolean);
begin
  ParagraphProperties.ContextualSpacing := Value;
end;

procedure TdxListLevel.SetDoubleFontSize(const Value: Integer);
begin
  CharacterProperties.DoubleFontSize := Value;
end;

procedure TdxListLevel.SetFirstLineIndent(const Value: Integer);
begin
  ParagraphProperties.FirstLineIndent := Value;
end;

procedure TdxListLevel.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  ParagraphProperties.FirstLineIndentType := Value;
end;

procedure TdxListLevel.SetFontBold(const Value: Boolean);
begin
  CharacterProperties.FontBold := Value;
end;

procedure TdxListLevel.SetFontItalic(const Value: Boolean);
begin
  CharacterProperties.FontItalic := Value;
end;

procedure TdxListLevel.SetFontName(const Value: string);
begin
  CharacterProperties.FontName := Value;
end;

procedure TdxListLevel.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  CharacterProperties.FontStrikeoutType := Value;
end;

procedure TdxListLevel.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  CharacterProperties.FontUnderlineType := Value;
end;

procedure TdxListLevel.SetForeColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.ForeColor := Value;
end;

procedure TdxListLevel.SetHidden(const Value: Boolean);
begin
  CharacterProperties.Hidden := Value;
end;

procedure TdxListLevel.SetKeepLinesTogether(const Value: Boolean);
begin
  ParagraphProperties.KeepLinesTogether := Value;
end;

procedure TdxListLevel.SetKeepWithNext(const Value: Boolean);
begin
  ParagraphProperties.KeepWithNext := Value;
end;

procedure TdxListLevel.SetLeftIndent(const Value: Integer);
begin
  ParagraphProperties.LeftIndent := Value;
end;

procedure TdxListLevel.SetLineSpacing(const Value: Single);
begin
  ParagraphProperties.LineSpacing := Value;
end;

procedure TdxListLevel.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  ParagraphProperties.LineSpacingType := Value;
end;

procedure TdxListLevel.SetOutlineLevel(const Value: Integer);
begin
  ParagraphProperties.OutlineLevel := Value;
end;

procedure TdxListLevel.SetPageBreakBefore(const Value: Boolean);
begin
  ParagraphProperties.PageBreakBefore := Value;
end;

procedure TdxListLevel.SetParagraphStyleIndex(const Value: Integer);
begin
  FParagraphStyleIndex := Value;
  OnParagraphStyleChanged;
end;

procedure TdxListLevel.SetRightIndent(const Value: Integer);
begin
  ParagraphProperties.RightIndent := Value;
end;

procedure TdxListLevel.SetScript(const Value: TdxCharacterFormattingScript);
begin
  CharacterProperties.Script := value;
end;

procedure TdxListLevel.SetSpacingAfter(const Value: Integer);
begin
  ParagraphProperties.SpacingAfter := Value;
end;

procedure TdxListLevel.SetSpacingBefore(const Value: Integer);
begin
  ParagraphProperties.SpacingBefore := Value;
end;

procedure TdxListLevel.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.StrikeoutColor := Value;
end;

procedure TdxListLevel.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  CharacterProperties.StrikeoutWordsOnly := Value;
end;

procedure TdxListLevel.SetSuppressHyphenation(const Value: Boolean);
begin
  ParagraphProperties.SuppressHyphenation := Value;
end;

procedure TdxListLevel.SetSuppressLineNumbers(const Value: Boolean);
begin
  ParagraphProperties.SuppressLineNumbers := Value;
end;

procedure TdxListLevel.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.UnderlineColor := Value;
end;

procedure TdxListLevel.SetUnderlineWordsOnly(const Value: Boolean);
begin
  CharacterProperties.UnderlineWordsOnly := Value;
end;

procedure TdxListLevel.SetWidowOrphanControl(const Value: Boolean);
begin
  ParagraphProperties.WidowOrphanControl := Value;
end;

function TdxListLevel.GetLeftIndent: Integer;
begin
  Result := MergedParagraphFormatting.LeftIndent;
end;

function TdxListLevel.GetLineSpacing: Single;
begin
  Result := MergedParagraphFormatting.LineSpacing;
end;

function TdxListLevel.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := MergedParagraphFormatting.LineSpacingType;
end;

function TdxListLevel.GetListLevelProperties: TdxListLevelProperties;
begin
  Result := FListLevelProperties;
end;

function TdxListLevel.GetMergedCharacterFormatting: TdxCharacterFormattingInfo;
begin
  Result := TdxSimpleDocumentCache(DocumentModel.Cache).MergedCharacterFormattingInfoCache[MergedCharacterFormattingCacheIndex];
end;

function TdxListLevel.GetMergedCharacterFormattingCacheIndex: Integer;
var
  AProperties: TdxMergedCharacterProperties;
begin
  if FMergedCharacterFormattingCacheIndex < 0 then
  begin
    AProperties := GetMergedCharacterProperties;
    try
      FMergedCharacterFormattingCacheIndex :=
        TdxSimpleDocumentCache(DocumentModel.Cache).MergedCharacterFormattingInfoCache.GetItemIndex(AProperties.Info);
    finally
      AProperties.Free;
    end;
  end;
  Result := FMergedCharacterFormattingCacheIndex;
end;

function TdxListLevel.GetMergedParagraphFormatting: TdxParagraphFormattingInfo;
begin
  Result := TdxSimpleDocumentCache(DocumentModel.Cache).MergedParagraphFormattingInfoCache[MergedParagraphFormattingCacheIndex];
end;

function TdxListLevel.GetMergedParagraphFormattingCacheIndex: Integer;
var
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  if FMergedParagraphFormattingCacheIndex < 0 then
  begin
    AMergedParagraphProperties := GetMergedParagraphProperties;
    try
      FMergedParagraphFormattingCacheIndex :=
        TdxSimpleDocumentCache(DocumentModel.Cache).MergedParagraphFormattingInfoCache.GetItemIndex(AMergedParagraphProperties.Info);
    finally
      AMergedParagraphProperties.Free;
    end;
  end;
  Result := FMergedParagraphFormattingCacheIndex;
end;

function TdxListLevel.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create(CharacterProperties);
  Result.Merge(TdxSimpleDocumentModel(DocumentModel).DefaultCharacterProperties);
end;

procedure TdxListLevel.OnPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;     
  E.&End := MaxInt; 
end;

{ TdxOverrideListLevel }

function TdxOverrideListLevel.CreateLevel: TdxListLevel;
begin
  Result := TdxOverrideListLevel.Create(DocumentModel);
end;

function TdxOverrideListLevel.GetNewStart: Integer;
begin
  Result := ListLevelProperties.Start;
end;

function TdxOverrideListLevel.GetOverrideStart: Boolean;
begin
  Result := FOverrideStart;
end;

procedure TdxOverrideListLevel.SetNewStart(const Value: Integer);
begin
  ListLevelProperties.Start := Value;
end;

procedure TdxOverrideListLevel.SetOverrideStart(AValue: Boolean);
begin
  FOverrideStart := AValue;
end;

{ TdxAbstractNumberingList }

constructor TdxAbstractNumberingList.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel, 9);
  FNumberingStyleReferenceIndex := -1;
  FStyleLinkIndex := -1;
end;

function TdxAbstractNumberingList.GenerateNewId: Integer;
begin
  Result := TdxDocumentModel(DocumentModel).AbstractNumberingListIdProvider.GetNextId;
end;

function TdxAbstractNumberingList.CreateLevel(ALevelIndex: Integer): TdxAbstractListLevel;
begin
  Result := TdxListLevel.Create(DocumentModel);
end;

procedure TdxAbstractNumberingList.CopyLevelsFrom(ASourceLevels: TdxListLevelCollection);
var
  I: Integer;
  ANewLevel: TdxListLevel;
  ASourceLevel: TdxListLevel;
begin
  for I := 0 to ASourceLevels.Count - 1 do
  begin
    ASourceLevel := ASourceLevels[I] as TdxListLevel;
    ANewLevel := TdxListLevel.Create(DocumentModel);
    ANewLevel.CopyFrom(ASourceLevel);
    Levels[I] := ANewLevel;
  end;
end;

function TdxAbstractNumberingList.Clone: TdxAbstractNumberingList;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateNumberingList;
  Result.CopyFrom(Self);
end;

function TdxAbstractNumberingList.CreateNumberingList: TdxAbstractNumberingList;
begin
  Result := TdxAbstractNumberingList.Create(DocumentModel);
end;

procedure TdxAbstractNumberingList.SetNumberingStyleReferenceIndex(
  const Value: Integer);
begin
  FNumberingStyleReferenceIndex := Value;
end;

procedure TdxAbstractNumberingList.SetStyleLinkIndex(const Value: Integer);
begin
  FStyleLinkIndex := Value;
end;

function TdxAbstractNumberingList.GetLevels: TdxListLevelCollection;
begin
  if UseStyleLevels then
    Result := TdxDocumentModel(DocumentModel).NumberingListStyles[FNumberingStyleReferenceIndex].NumberingList.AbstractNumberingList.Levels
  else
    Result := inherited GetLevels;
end;

function TdxAbstractNumberingList.GetNumberingStyleReference: TdxNumberingListStyle;
begin
  if FNumberingStyleReferenceIndex >= 0 then
    Result := TdxDocumentModel(DocumentModel).NumberingListStyles[FNumberingStyleReferenceIndex]
  else
    Result := nil;
end;

function TdxAbstractNumberingList.GetStyleLink: TdxNumberingListStyle;
begin
  if FStyleLinkIndex >= 0 then
    Result := TdxDocumentModel(DocumentModel).NumberingListStyles[FStyleLinkIndex]
  else
    Result := nil;
end;

function TdxAbstractNumberingList.GetTemplateCode: Integer;
begin
  Result := $0409001D;
end;

function TdxAbstractNumberingList.GetUseStyleLevels: Boolean;
begin
  Result := (FStyleLinkIndex < 0) and (FNumberingStyleReferenceIndex >= 0);
end;

{ TdxAbstractNumberingListCollection }

function TdxAbstractNumberingListCollection.HasListOfType(AListType: TdxNumberingType): Boolean;
var
  I: TdxAbstractNumberingListIndex;
begin
  for I := Count - 1 downto 0 do
    if TdxNumberingListHelper.GetListType(Self[I]) = AListType then
      Exit(True);
  Result := False;
end;

{ TdxNumberingListHelper }

class function TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel: TdxCustomDocumentModel): Integer;
begin
  repeat
    Result := TemplateCodeStart + Random(TemplateCodeEnd - TemplateCodeStart); 
    if IsNewTemplateCode(ADocumentModel, Result) then
      Break;
  until False;
end;

class function TdxNumberingListHelper.GetLevelType(const ANumberingList: TdxNumberingListBaseImpl;
  ALevelIndex: Integer): TdxNumberingType;
var
  ALevel: IdxListLevel;
begin
  ALevel := ANumberingList.Levels[ALevelIndex];
  if IsBulletLevel(ALevel) then
    Result := TdxNumberingType.Bullet
  else
    Result := TdxNumberingType.Simple;
end;

class function TdxNumberingListHelper.GetListType(const ANumberingList: TdxNumberingListBaseImpl): TdxNumberingType;
begin
  if ANumberingList.Levels[0].BulletLevel then
    Result := TdxNumberingType.Bullet
  else
    if not IsHybridList(ANumberingList) then
      Result := TdxNumberingType.MultiLevel
    else
      Result := TdxNumberingType.Simple;
end;

class function TdxNumberingListHelper.IsHybridList(const ANumberingList: TdxNumberingListBaseImpl): Boolean;
var
  ALevels: TdxListLevelCollection;
  I: Integer;
begin
  Result := False;
  ALevels := ANumberingList.Levels;
  for I := 0 to ALevels.Count - 1 do
    if ALevels[I].ListLevelProperties.TemplateCode <> 0 then
    begin
      Result := True;
      Break;
    end;
end;

class function TdxNumberingListHelper.IsBulletLevel(const ALevel: IdxListLevel): Boolean;
var
  S: string;
begin
  S := Format(ALevel.ListLevelProperties.DisplayFormatString, ['0', '0', '0', '0', '0', '0', '0', '0', '0', '0']);
  Result := ALevel.BulletLevel or (S = ALevel.ListLevelProperties.DisplayFormatString);
end;

class function TdxNumberingListHelper.IsNewTemplateCode(ADocumentModel: TdxCustomDocumentModel;
  ATemplateCode: Integer): Boolean;
var
  I: Integer;
  ALevelInfoCache: TdxListLevelInfoCache;
begin
  Result := True;
  ALevelInfoCache := TdxDocumentModel(ADocumentModel).Cache.ListLevelInfoCache;
  for I := 0 to ALevelInfoCache.Count - 1 do
    if ALevelInfoCache[I].TemplateCode = ATemplateCode then
    begin
      Result := False;
      Break;
    end;
end;

class procedure TdxNumberingListHelper.SetHybridListType(const AList: TdxNumberingListBaseImpl);
var
  AAnchorAction: TdxAction<TdxListLevelProperties>;
begin
  AAnchorAction := procedure(const AProperties: TdxListLevelProperties)
    begin
      AProperties.TemplateCode := GenerateNewTemplateCode(AList.DocumentModel);
    end;
  SetListLevelsProperty(AList, function(AProperties: TdxListLevelProperties): Boolean
  begin
    Result := AProperties.TemplateCode = 0;
  end,
  AAnchorAction);
end;

class procedure TdxNumberingListHelper.SetListLevelsFormat(const AList: TdxNumberingListBaseImpl;
  AFormat: TdxRichEditNumberingFormat);
var
  AAnchorAction: TdxAction<TdxListLevelProperties>;
begin
  AAnchorAction := procedure(const AProperties: TdxListLevelProperties)
    begin
      AProperties.Format := AFormat;
    end;
  SetListLevelsProperty(AList, function(AProperties: TdxListLevelProperties): Boolean
    begin
      Result := True;
    end,
    AAnchorAction);
end;

class function TdxNumberingListHelper.GetAbstractListByType(
  AListCollection: TdxAbstractNumberingListCollection;
  AType: TdxNumberingType): TdxAbstractNumberingList;
var
  AIndex: TdxAbstractNumberingListIndex;
begin
  AIndex := GetAbstractListIndexByType(AListCollection, AType);
  Result := AListCollection[AIndex];
end;

class function TdxNumberingListHelper.GetAbstractListIndexByType(AListCollection: TdxAbstractNumberingListCollection;
  AType: TdxNumberingType): TdxAbstractNumberingListIndex;
var
  I: TdxAbstractNumberingListIndex;
begin
  for I := 0 to AListCollection.Count - 1 do
    if TdxNumberingListHelper.GetListType(AListCollection[I]) = AType then
      Exit(I);
  TdxRichEditExceptions.ThrowArgumentException('type', Ord(AType));
  Result := -1;
end;

class procedure TdxNumberingListHelper.SetListLevelsProperty(const AList: TdxNumberingListBaseImpl;
  const ACondition: TdxListLevelPropertiesDelegate; const AAction: TdxAction<TdxListLevelProperties>);
var
  I: Integer;
  AListLevel: IdxListLevel;
begin
  for I := 0 to AList.Levels.Count - 1 do
  begin
    AListLevel := AList.Levels[I];
    AListLevel.ListLevelProperties.BeginInit;
    try
      if ACondition(AListLevel.ListLevelProperties) then
        AAction(AListLevel.ListLevelProperties);
    finally
      AListLevel.ListLevelProperties.EndInit;
    end;
  end;
end;

class procedure TdxNumberingListHelper.SetListType(const AList: TdxNumberingListBaseImpl; AValue: TdxNumberingType);
var
  ACurrentType: TdxNumberingType;
  AWasSimple, AWasBullet, ANewValueIsMultiLevel, ANewValueIsSimple, ANewValueIsBullet: Boolean;
begin
  ACurrentType := GetListType(AList);
  AWasSimple := ACurrentType <> TdxNumberingType.MultiLevel;
  AWasBullet := ACurrentType = TdxNumberingType.Bullet;
  ANewValueIsMultiLevel := AValue = TdxNumberingType.MultiLevel;
  ANewValueIsSimple := AValue = TdxNumberingType.Simple;
  ANewValueIsBullet := AValue = TdxNumberingType.Bullet;
  if AWasSimple and ANewValueIsMultiLevel then
    TdxNumberingListHelper.SetSimpleListType(AList)
  else
    if not ANewValueIsMultiLevel then
      TdxNumberingListHelper.SetHybridListType(AList);
  if ANewValueIsBullet then
    SetListLevelsFormat(AList, TdxRichEditNumberingFormat.Bullet);
  if AWasBullet and ANewValueIsSimple then
    SetListLevelsFormat(AList, TdxRichEditNumberingFormat.Decimal);
end;

class procedure TdxNumberingListHelper.SetSimpleListType(const AList: TdxNumberingListBaseImpl);
var
  AAnchorAction: TdxAction<TdxListLevelProperties>;
begin
  AAnchorAction := procedure(const AProperties: TdxListLevelProperties)
    begin
      AProperties.TemplateCode := 0;
    end;
  SetListLevelsProperty(AList, function(AProperties: TdxListLevelProperties): Boolean
    begin
      Result := AProperties.TemplateCode <> 0;
    end,
    AAnchorAction);
end;

{ TdxNumberingListIndexCalculator }

constructor TdxNumberingListIndexCalculator.Create(AModel: TdxSimpleDocumentModel; ANumberingListType: TdxNumberingType);
begin
  inherited Create;
  Assert(AModel <> nil); 
  FModel := AModel;
  FNumberingListType := ANumberingListType;
end;

function TdxNumberingListIndexCalculator.CreateNewAbstractList(
  ASource: TdxAbstractNumberingList): TdxAbstractNumberingListIndex;
var
  ANewAbstractNumberingList: TdxAbstractNumberingList;
begin
  ANewAbstractNumberingList := TdxAbstractNumberingList.Create(DocumentModel);
  ANewAbstractNumberingList.CopyFrom(ASource); 
  TdxDocumentModel(DocumentModel).AddAbstractNumberingListUsingHistory(ANewAbstractNumberingList);
  ANewAbstractNumberingList.SetId(TdxDocumentModel(DocumentModel).AbstractNumberingListIdProvider.GetNextId); 
  Result := TdxDocumentModel(DocumentModel).AbstractNumberingLists.Count - 1;
end;

function TdxNumberingListIndexCalculator.CreateNewList(ASource: TdxAbstractNumberingList): TdxNumberingListIndex;
var
  ANewList: TdxNumberingList;
  AAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
begin
  AAbstractNumberingListIndex := CreateNewAbstractList(ASource);
  ANewList := TdxNumberingList.Create(DocumentModel, AAbstractNumberingListIndex);
  TdxDocumentModel(DocumentModel).AddNumberingListUsingHistory(ANewList);
  Result := TdxDocumentModel(DocumentModel).NumberingLists.Count - 1;
end;

function TdxNumberingListIndexCalculator.GetListIndex(AStart, AEnd: TdxParagraphIndex): TdxNumberingListIndex;
begin
  if (AStart > 0) and DocumentModel.GetActivePieceTableCore.Paragraphs[AStart - 1].IsInList then
  begin
    Result := GetListIndexCore(AStart - 1);
    if Result >= 0 then 
      Exit;
  end;
  if (AEnd < DocumentModel.GetActivePieceTableCore.Paragraphs.Count - 1) and DocumentModel.GetActivePieceTableCore.Paragraphs[AEnd + 1].IsInList then
  begin
    Result := GetListIndexCore(AEnd + 1);
    if Result >= 0 then 
      Exit;
  end;
  Result := NumberingListIndexListIndexNotSetted; 
end;

function TdxNumberingListIndexCalculator.GetListIndexCore(AParagraphIndex: TdxParagraphIndex): TdxNumberingListIndex;
var
  AParagraphListType: TdxNumberingType;
  ANearParagraphListIndex: TdxNumberingListIndex;
begin
  ANearParagraphListIndex := TdxSimplePieceTable(DocumentModel.GetActivePieceTableCore).Paragraphs[AParagraphIndex].GetNumberingListIndex;
  AParagraphListType := TdxNumberingListHelper.GetListType(TdxDocumentModel(DocumentModel).NumberingLists[ANearParagraphListIndex].AbstractNumberingList);
  if AParagraphListType = NumberingListType then
  begin
    ContinueList := True;
    Result := ANearParagraphListIndex;
  end
  else
    Result := NumberingListIndexListIndexNotSetted;
end;

{ TdxDefaultNumberingListHelper }

class function TdxDefaultNumberingListHelper.CreateDefaultBulletAbstractNumberingList(DocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList;
const
  ASymbolDisplayFormat: array[0..1] of string = (TdxCharacters.MiddleDot, #$006F); 
var
  ADocumentModel: TdxDocumentModel absolute DocumentModel;
  ALevelOffset: Integer;
  I, AFirstLineIndent, AStringFormatIndex: Integer;
  ALevel: TdxListLevel;
begin
  ALevelOffset := AUnitConverter.DocumentsToModelUnits(150);
  Result := TdxAbstractNumberingList.Create(ADocumentModel);
  for I := 0 to Result.Levels.Count - 1 do
  begin
    ALevel := TdxListLevel.Create(ADocumentModel);
    Result.Levels[I] := ALevel;
    ALevel.CharacterProperties.BeginInit;
    ALevel.CharacterProperties.FontName := 'Symbol';
    ALevel.CharacterProperties.EndInit;
    AFirstLineIndent := ALevelOffset * (I + 1);
    SetFirstLineIndent(Result.Levels[I], AFirstLineIndent, ADefaultTabWidth div 2);
    AStringFormatIndex := I mod Length(ASymbolDisplayFormat);
    SetDisplayFormatString(Result.Levels[I], ASymbolDisplayFormat[AStringFormatIndex]);
    SetTemplateCode(Result.Levels[I], TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel));
    ALevel.ListLevelProperties.Format := TdxRichEditNumberingFormat.Bullet;
  end;
end;

class function TdxDefaultNumberingListHelper.CreateDefaultMultilevelAbstractNumberingList(
  DocumentModel: TdxCustomDocumentModel; AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList;
const
  AlignPositionsInDocuments: array[0..8] of Integer = (75, 165, 255, 360, 465, 570, 675, 780, 900);
var
  ADocumentModel: TdxDocumentModel absolute DocumentModel;
  I, ALevelOffset, AFirstLinePosition, ALeftIndent: Integer;
  ALevel: TdxListLevel;
begin
  Result := TdxAbstractNumberingList.Create(ADocumentModel);
  ALevelOffset := AUnitConverter.DocumentsToModelUnits(75);
  for I := 0 to Result.Levels.Count - 1 do
  begin
    ALevel := TdxListLevel.Create(ADocumentModel);
    Result.Levels[I] := ALevel;
    AFirstLinePosition := ALevelOffset * I;
    ALeftIndent := AUnitConverter.DocumentsToModelUnits(
      AlignPositionsInDocuments[Math.Min(I, Length(AlignPositionsInDocuments) - 1)]);
    SetFirstLineIndent(Result.Levels[I], ALeftIndent, ALeftIndent - AFirstLinePosition);
  end;
  SetDisplayFormatString(Result.Levels[0], '%0:s.');
  SetDisplayFormatString(Result.Levels[1], '%0:s.%1:s.');
  SetDisplayFormatString(Result.Levels[2], '%0:s.%1:s.%2:s.');
  SetDisplayFormatString(Result.Levels[3], '%0:s.%1:s.%2:s.%3:s.');
  SetDisplayFormatString(Result.Levels[4], '%0:s.%1:s.%2:s.%3:s.%4:s.');
  SetDisplayFormatString(Result.Levels[5], '%0:s.%1:s.%2:s.%3:s.%4:s.%5:s.');
  SetDisplayFormatString(Result.Levels[6], '%0:s.%1:s.%2:s.%3:s.%4:s.%5:s.%6:s.');
  SetDisplayFormatString(Result.Levels[7], '%0:s.%1:s.%2:s.%3:s.%4:s.%5:s.%6:s.%7:s.');
  SetDisplayFormatString(Result.Levels[8], '%0:s.%1:s.%2:s.%3:s.%4:s.%5:s.%6:s.%7:s.%8:s.');
end;

class function TdxDefaultNumberingListHelper.CreateDefaultSimpleAbstractNumberingList(DocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer): TdxAbstractNumberingList;
var
  ADocumentModel: TdxDocumentModel absolute DocumentModel;
  ALevel: TdxListLevel;
  I, ALevelOffset, AFirstLineIndent: Integer;
begin
  Result := TdxAbstractNumberingList.Create(ADocumentModel);
  ALevelOffset := AUnitConverter.DocumentsToModelUnits(150);
  for I := 0 to Result.Levels.Count - 1 do
  begin
    ALevel := TdxListLevel.Create(ADocumentModel);
    Result.Levels[I] := ALevel;
    AFirstLineIndent := ALevelOffset * (I + 1);
    SetFirstLineIndent(Result.Levels[I], AFirstLineIndent, ADefaultTabWidth div 2);
    SetDisplayFormatString(Result.Levels[I], '%' + IntToStr(I) + ':s.'); 
    SetTemplateCode(Result.Levels[I], TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel));
  end;
end;

class procedure TdxDefaultNumberingListHelper.InsertNumberingLists(ADocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer);
begin
  InsertDefaultNumberingList(ADocumentModel, AUnitConverter, ADefaultTabWidth, CreateDefaultSimpleAbstractNumberingList);
  InsertDefaultNumberingList(ADocumentModel, AUnitConverter, ADefaultTabWidth, CreateDefaultBulletAbstractNumberingList);
  InsertDefaultNumberingList(ADocumentModel, AUnitConverter, ADefaultTabWidth, CreateDefaultMultilevelAbstractNumberingList);
end;

class procedure TdxDefaultNumberingListHelper.InsertDefaultSimpleNumberingList(ADocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer);
begin
  InsertDefaultNumberingList(ADocumentModel, AUnitConverter, ADefaultTabWidth, CreateDefaultSimpleAbstractNumberingList);
end;

class procedure TdxDefaultNumberingListHelper.InsertDefaultBulletNumberingList(ADocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer);
begin
  InsertDefaultNumberingList(ADocumentModel, AUnitConverter, ADefaultTabWidth, CreateDefaultBulletAbstractNumberingList);
end;

class procedure TdxDefaultNumberingListHelper.InsertDefaultNumberingList(DocumentModel: TdxCustomDocumentModel;
  AUnitConverter: TdxDocumentModelUnitConverter; ADefaultTabWidth: Integer; ACreateFunction: TCreateDefaultAbstractList);
var
  ADocumentModel: TdxDocumentModel absolute DocumentModel;
  AAbstractNumberingList: TdxAbstractNumberingList;
  AAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
begin
  AAbstractNumberingList := ACreateFunction(ADocumentModel, AUnitConverter, ADefaultTabWidth);
  ADocumentModel.AddAbstractNumberingListUsingHistory(AAbstractNumberingList);
  AAbstractNumberingListIndex := ADocumentModel.AbstractNumberingLists.Count - 1;
  ADocumentModel.AddNumberingListUsingHistory(TdxNumberingList.Create(ADocumentModel, AAbstractNumberingListIndex));
end;

class procedure TdxDefaultNumberingListHelper.SetDisplayFormatString(const ALevel: IdxListLevel;
  const ADisplayFormatString: string);
begin
  ALevel.ListLevelProperties.BeginInit;
  try
    ALevel.ListLevelProperties.DisplayFormatString := ADisplayFormatString;
  finally
    ALevel.ListLevelProperties.EndInit;
  end;
end;

class procedure TdxDefaultNumberingListHelper.SetFirstLineIndent(const ALevel: IdxListLevel; ALineIndent,
  AFirstLineIndent: Integer);
begin
  ALevel.ParagraphProperties.BeginInit;
  try
    ALevel.ParagraphProperties.LeftIndent := ALineIndent;
    ALevel.ParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
    ALevel.ParagraphProperties.FirstLineIndent := AFirstLineIndent;
  finally
    ALevel.ParagraphProperties.EndInit;
  end;
end;

class procedure TdxDefaultNumberingListHelper.SetTemplateCode(const ALevel: IdxListLevel; ATemplateCode: Integer);
begin
  ALevel.ListLevelProperties.BeginInit;
  try
    ALevel.ListLevelProperties.TemplateCode := ATemplateCode;
  finally
    ALevel.ListLevelProperties.EndInit;
  end;
end;

end.
